#!/bin/bash

TIME=/usr/bin/time
THREADS=4
CLOC=cloc

##################################################

[ -z "`which cloc`" ] && echo "Please install the 'cloc' program." && exit 1

DIR=`dirname "$0"`
declare -g -i TOTAL COUNT
TOTAL=`"$DIR"/files.sh | wc -l`
COUNT=0
declare -g FMT
FMT="%0${#TOTAL}d"

declare -g -i STIME ETIME
STIME=`date +%s`

DATE=`date --rfc-3339=seconds | tr ' ' '_'`

mkdir -p "$DIR"/logs
LOGFILE="$DIR"/logs/infer_"$DATE".log

TMPOUT=`mktemp -d`

function finish() {
    ETIME=`date +%s`
    SECS=$(( (ETIME - STIME) ))
    echo "%%% end run $DATE elapsed=`date -d@$SECS -u +%H:%M:%S`" >> "$LOGFILE"
}

trap "{ wait; finish; rm -f $TMPOUT/*; rmdir $TMPOUT; exit 0; }" EXIT

declare -A PIPES
declare -a PIDS

echo "%%% begin run $DATE" > "$LOGFILE"

function start_thread() {
    m="$1"
    f="$2"
    c=$3
    r=`printf "$FMT" $((TOTAL - c))`
    p=`printf "%05d" $BASHPID`
    [ -z "${MODULES[$m]}" ] && MODULES+=([$m]=yes)
    echo "[PID=$p REM=$r] Starting stencils-infer MOD=$m FILE=\"$f\"..." >&2
    echo "%%% begin stencils-infer MOD=$m FILE=\"$f\""
    lines=`$CLOC --progress-rate=0 --quiet --csv "$f" | tail -1 | cut -f 5 -d,`
    echo "LineCount: $lines"
    echo "StartTime: `date --rfc-3339=seconds`"
    echo "Progress: $c / $TOTAL"
    "$TIME" stack exec camfort -- stencils-infer "$f" "$TMPOUT" -m Eval 2>&1
    echo "EndTime: `date --rfc-3339=seconds`"
    echo "%%% end stencils-infer MOD=$m FILE=\"$f\""
    echo "[PID=$p REM=$r] Ending stencils-infer MOD=$m FILE=\"$f\"" >&2
}

function check_threads() {
    declare -a newpids
    newpids=()
    for p in ${PIDS[@]}; do
        ps -p $p &> /dev/null
        if [ "$?" -ne "0" ]; then
            cat ${PIPES[$p]} >> "$LOGFILE"
            rm -f ${PIPES[$p]}
            unset PIPES[$p]
            wait $p &> /dev/null
        else
            newpids+=($p)
        fi
    done
    PIDS=(${newpids[@]})
}

while IFS=',' read m f; do
    started=0
    while [ $started == 0 ]; do
        if [ ${#PIDS[@]} -lt "$THREADS" ]; then
            pipe=`mktemp -p $TMPOUT pipeXXX`
            COUNT=$((COUNT + 1))
            start_thread $m $f $COUNT > $pipe &
            pid=$!
            PIDS+=($pid)
            PIPES+=([$pid]=$pipe)
            started=1
        else
            sleep 1
        fi
        check_threads
    done
done < <("$DIR"/files.sh)

wait
check_threads

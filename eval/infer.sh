#!/bin/bash

TIME=/usr/bin/time
THREADS=8

##################################################

DIR=`dirname "$0"`

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
    [ -z "${MODULES[$m]}" ] && MODULES+=([$m]=yes)
    echo "[PID=$BASHPID] Starting stencils-infer MOD=$m FILE=\"$f\"..." >&2
    echo "%%% begin stencils-infer MOD=$m FILE=\"$f\""
    lines=`wc -l "$f" | cut -f 1 -d ' '`
    echo "LineCount: $lines"
    echo "StartTime: `date --rfc-3339=seconds`"
    "$TIME" stack exec camfort -- stencils-infer "$f" "$TMPOUT" -m Eval 2>&1
    echo "EndTime: `date --rfc-3339=seconds`"
    echo "%%% end stencils-infer MOD=$m FILE=\"$f\""
    echo "[PID=$BASHPID] Ending stencils-infer MOD=$m FILE=\"$f\"" >&2
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
    if [ ${#PIDS[@]} -lt "$THREADS" ]; then
        pipe=`mktemp -p $TMPOUT pipeXXX`
        start_thread $m $f > $pipe &
        pid=$!
        PIDS+=($pid)
        PIPES+=([$pid]=$pipe)
    else
        sleep 1
    fi
    check_threads
done < <("$DIR"/files.sh)

wait
check_threads

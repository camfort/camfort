#!/bin/bash

TIME=/usr/bin/time

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

trap "{ finish; rm -f $TMPOUT/* ; rmdir $TMPOUT ; exit 255; }" EXIT

echo "%%% begin run $DATE" > "$LOGFILE"

# loop for each line containing the name of a module, the name of a file
while IFS=',' read m f; do
    [ -z "${MODULES[$m]}" ] && MODULES+=([$m]=yes)
    echo -n "stencils-infer MOD=$m FILE=\"$f\"..."
    echo "%%% begin stencils-infer MOD=$m FILE=\"$f\"" >> "$LOGFILE"
    "$TIME" stack exec camfort stencils-infer "$f" "$TMPOUT" -m Eval &>> "$LOGFILE"
    echo "%%% end stencils-infer MOD=$m FILE=\"$f\"" >> "$LOGFILE"
    echo "done."
done < <("$DIR"/files.sh)

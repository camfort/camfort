#!/bin/bash

DIR=`dirname "$0"`
CLOC=cloc

declare -A COUNTS

function dump() {
    declare -i sum
    sum=0
    for k in ${!COUNTS[@]}; do
        n=${COUNTS[$k]}
        echo "$k: $n"
        sum=$((sum + n))
    done
    echo "Overall: $sum"
}

while IFS=',' read m f; do
    n=`$CLOC --progress-rate=0 --quiet --csv "$f" | tail -1 | cut -f 5 -d,`
    ((COUNTS[$m]+=$n))
    echo $f: $n
done < <("$DIR"/files.sh)

dump

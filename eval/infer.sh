#!/bin/bash

CAMFORT=dist/build/camfort/camfort

##################################################

DIR=`dirname "$0"`

mkdir -p "$DIR"/logs
LOGFILE="$DIR"/logs/infer_`date --rfc-3339=seconds | tr ' ' '_'`.log

TMPOUT=`mktemp -d`

STENCILWORDS=(readOnce reflexive forward backward atMost atLeast boundedBoth)

parsefailed=0
matches=0

stencilre=")[[:space:]]*stencil"

declare -A MODULES

function stats() {
    echo | tee -a "$LOGFILE"
    echo "--------------------------------------------------" | tee -a "$LOGFILE"
    overall_matches=0
    overall_parsefailed=0
    for w in ${STENCILWORDS[@]}; do
        eval "overall_$w=0"
    done

    for m in ${!MODULES[@]}; do
        eval "echo ${m}_matches,\$${m}_matches | tee -a \"$LOGFILE\""
        eval "overall_matches=\$((overall_matches + ${m}_matches))"
        eval "echo ${m}_parsefailed,\$${m}_parsefailed | tee -a \"$LOGFILE\""
        eval "overall_parsefailed=\$((overall_parsefailed + ${m}_parsefailed))"
        for w in ${STENCILWORDS[@]}; do
            var="${m}_$w"
            eval "echo $var,\$$var | tee -a \"$LOGFILE\""
            eval "overall_$w=\$((overall_$w+$var))"
        done
    done

    echo overall_matches,$overall_matches | tee -a "$LOGFILE"
    echo overall_parsefailed,$overall_parsefailed | tee -a "$LOGFILE"

    for w in ${STENCILWORDS[@]}; do
        eval "echo overall_$w,\$overall_$w | tee -a \"$LOGFILE\""
    done
}

trap "{ stats; rm -f $TMPOUT/* ; rmdir $TMPOUT ; exit 255; }" EXIT

# loop for each line containing the name of a module, the name of a file
while IFS=',' read m f; do
    [ -z "${MODULES[$m]}" ] && MODULES+=([$m]=yes)
    echo -n "$f... "
    output=`"$CAMFORT" stencils-infer "$f" "$TMPOUT" 2>&1 | tee -a "$LOGFILE"`
    echo -n "$f... " >> "$LOGFILE"

    if [[ "$output" =~ failed ]]; then
        echo "parse failed." | tee -a "$LOGFILE"
        eval "${m}_parsefailed=\$((${m}_parsefailed+1))"
    elif [[ "$output" =~ $stencilre ]]; then
        # clear stencil keyword counters
        cur_matches=0
        for w in ${STENCILWORDS[@]}; do
            eval "cur_$w=0"
        done

        prevSrcSpan=""
        prevVars=""
        prevSpec=""

        # count stencil keywords for each stencil line
        while IFS=: read spec blank vars; do
            # each variable is also counted separately
            vcount=`echo "$vars" | fgrep -o ',' | wc -l`
            vcount=$((vcount+1))

            cur_matches=$((cur_matches+vcount))
            for w in ${STENCILWORDS[@]}; do
                re="[^A-Za-z]$w[^A-Za-z]"
                [[ "$spec" =~ $re ]] && eval "cur_$w=\$((cur_$w+$vcount))"
            done

            # look for variables bounded on both sides by atMost and atLeast.
            srcSpan=`echo "$spec" | cut -f1 -d' '`
            if [ "$srcSpan" == "$prevSrcSpan" -a "$vars" == "$prevVars" ]; then
                atMostRE="[^A-Za-z]atMost[^A-Za-z]"
                atLeastRE="[^A-Za-z]atLeast[^A-Za-z]"
                [[ ("$spec" =~ $atMostRE && "$prevSpec" =~ $atLeastRE) || ("$spec" =~ $atLeastRE && "$prevSpec" =~ $atMostRE) ]] \
                    && cur_boundedBoth=$((cur_boundedBoth+1))
            fi

            prevVars="$vars"
            prevSrcSpan="$srcSpan"
            prevSpec="$spec"
        done < <(echo "$output" | grep "$stencilre")

        # output and add onto module 'successful matches' count.
        echo -n "$cur_matches matches..." | tee -a "$LOGFILE"
        eval "${m}_matches=\$((${m}_matches+cur_matches))"
        # output stencil keyword counters
        for w in ${STENCILWORDS[@]}; do
            eval "n=\$cur_$w"
            [ $n -ne 0 ] && echo -n "$w=$n..." | tee -a "$LOGFILE"

            # add onto current module counter for current stencil keyword
            statvar="${m}_$w"
            eval "$statvar=\$(($statvar+$n))"
        done
        echo "ok." | tee -a "$LOGFILE"
    else
        echo "nothing found." | tee -a "$LOGFILE"
    fi
done < <("$DIR"/files.sh)

#!/bin/bash

DIR=`dirname "$0"`

TMPOUT=`mktemp -d`

trap "{ echo cleaning up; rm -f $TMPOUT/* ; rmdir $TMPOUT ; exit 255; }" EXIT

for line in `$DIR/files.sh`; do
    f=`echo "$line" | cut -f2 -d,`
    echo "$f... "
    stack exec camfort -- stencils-infer "$f" "$TMPOUT" -m Eval
done

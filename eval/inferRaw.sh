#!/bin/bash

CAMFORT=dist/build/camfort/camfort

DIR=`dirname "$0"`

TMPOUT=`mktemp -d`

trap "{ echo cleaning up; rm -f $TMPOUT/* ; rmdir $TMPOUT ; exit 255; }" EXIT

for line in `$DIR/files.sh`; do
    f=`echo "$line" | cut -f2 -d,`
    echo "$f... "
    "$CAMFORT" stencils-infer "$f" "$TMPOUT" 2>&1
done

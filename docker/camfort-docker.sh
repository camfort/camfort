#!/bin/bash

ARGS=""
for a; do
    case $a in
        # Prefix absolute pathnames with /mnt.
        # Not perfect but widely usable. Symlinks may break.
        /*) ARGS="$ARGS /mnt$a" ;;
        *) ARGS="$ARGS $a" ;;
    esac
done

# Mount the computer's root directory under /mnt in the Docker
# container.
docker run -it --rm -v /:/mnt -w "/mnt$PWD" camfort/camfort:latest $ARGS

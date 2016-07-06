#!/bin/bash

for f in jacobi/f/jacobi*.ppm; do
    echo -n "$f: "
    cmp -l "$f" jacobi/jacobi.ppm | wc -l
done

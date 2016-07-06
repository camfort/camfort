#!/bin/bash


for f in jacobi/f/jacobi*.f90; do
    stack exec -- camfort stencils-check "$f" out
done | tee jacobi/output.log


#!/bin/bash

file=$1
shift

cat "$file"|cpp $@ - - |grep -v '^#'

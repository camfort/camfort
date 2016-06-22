#!/bin/bash

# View most recent log

DIR=`dirname "$0"`

ls -1t "$DIR"/logs/*.log | head -1 | xargs less

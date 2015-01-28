#!/bin/sh
# Pass the output of SysTest and this will scan the result to see if it was correct

set -u
set -e

grep "main_inst: main.c:34: void" "$1" || exit 1
grep "scheduler: Found assertion violation" "$1" || exit 1

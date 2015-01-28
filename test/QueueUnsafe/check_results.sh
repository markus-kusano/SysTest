#!/bin/sh

set -e
set -u

grep 'main_inst: main.c:135: void' "$1" || exit 1
grep 'scheduler: Found assertion violation' "$1" || exit 1

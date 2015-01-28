#!/bin/sh

grep "====== Run 642 ======" "$1" || exit 1
grep "main_inst: main.c:52: void" "$1" || exit 1
grep "scheduler: Found assertion violation" "$1" || exit 1

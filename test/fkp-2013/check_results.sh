#!/bin/sh

set -e
set -u

grep "scheduler: Found assertion violation" "$1" || exit 1

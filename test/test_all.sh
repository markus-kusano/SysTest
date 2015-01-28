#!/bin/bash

set -u
set -e

TEST_DIRS="AccountBad \
  QueueUnsafe \
  LogProcSweep \
  StackUnsafe \
  Stateful01_Unsafe \
  32pthread5 \
  fkp-2013 \
  bigshot \
  BluetoothDriverBad \
  TwoStage3_Unsafe"

for d in $TEST_DIRS
do
  echo "==== Testing: $d ===="
  time ./test.sh $d
done


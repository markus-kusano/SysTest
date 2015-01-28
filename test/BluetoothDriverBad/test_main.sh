#!/bin/sh

# run the uninstrumented program 2336 times (same as the number of runs DPOR takes to find the bug)
for i in `seq 1 2336`
do
  ./main
done 

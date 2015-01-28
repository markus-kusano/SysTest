#!/bin/bash

die() {
  echo ${1:-}
  exit 1
}

source ./exports.sh  || exit 1

if [ -z ${1:-} ]
then
  echo "Error: Pass directory to test"
fi

cd $1 
make main_inst >&make.log  || die "make failed"
$SYSTEST ./main_inst &>out.log
./check_results.sh out.log &>/dev/null || die "Test results dont match"
make clean >&make.log
rm -f out.log
rm -f make.log
echo "==== $1 Passed ===="
cd - >&/dev/null

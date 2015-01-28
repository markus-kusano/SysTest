## README
Author: Markus Kusano

Haskell implementation of a systematic concurrency tester.

### Directory Structure

`src` contains two directories:

1. `llvm`: contains the LLVM instrumentation pass
1. `runtime`: contains dynamic analysis runtime library files

Additionally, it contains the scheduler implementation

### Building

SysTest has three components: a runtime library for the program-under-test, a
scheduler, and a LLVM `opt` pass.

#### Building LLVM Library Files

Use the `cmake` file in `src/llvm/` to build the SysTest LLVM pass. This will
create a `.so` file which can be loaded by `opt:

To build using cmake:
    
    cd src/llvm
    mkdir build
    cd build
    cmake ../
    make

This will create a `.so` file: `build/systest/libSysTestInst.so

    opt -load <library file location>/libSysTestInst.so -systest_inst <bc file> ><output file>

The resulting `<output file>` will have instrumentation functions inserted. It
will still need to be linked with a runtime dynamic analysis library (i.e., the
actual instrumentation function implementations).

By default, `cmake` will search some standard paths to find LLVM. If you would
like to specify your own LLVM installation, you need to point `cmake` toward
the installed LLVM `cmake` files using the variable `LLVM_DIR`:

    cmake -DLLVM_DIR=/home/markus/src/install-3.2/share/llvm/cmake ../


#### Scheduler

The scheduler requires `ghc` (the Haskell compiler) and cabal (a Haskell build
system). It was develope dwith `cabal-install` version 1.18.0.3, cabal library
version 1.18.1.2 and GHC version 7.6.3.

To build:

    cd src/scheduler
    make

If all goes well, there should be a binary named `scheduler` in the directory.


#### Runtime Library

To build the runtime library:

    cd src/runtime
    make

The result is a `.a` file, `libsystest_runtime.a`. In order to test an
application, it needs to be first instrumented by the LLVM pass and then linked
with the runtime library.

### Test Usage

the `test/` directory contains some scripts to make using SysTest easier. 

    cd test
    source exports.sh

This will create a enviroment variable SYSTEST which is the location of the
SysTest scheduler.

Then, to run a test example:

    cd AccountBad
    make main_inst
    $SYSTEST ./main_inst

The result should be 3 runs ending in failure

The command `make main_inst` creates the instrumented program-under-test. It is
then passed to SysTest and explored.

### Usage in depth

The `test` directory contains some test files and scripts to run them.

Here are some step by step instructions on how to use the tool.

Assuming there is a file `main.c` which we would like to test.

First, compile it into LLVM's intermediate representation:

    clang -emit-llvm -S main.c -o main.bc

This will allow it to be modified by LLVM passes.

Next, instrument the code to include calls to our runtime functions:

    
	  opt -load <path to .so>/libSysTestInst.so -systest_inst main.bc > main_inst.bc

Compile the instrumented file to a `.s` file:

	  llc main_inst.bc

If you have multiple files, they can each be instrumented separately and then
all linked together.

Then, link the final executable:

    
	clang++ main_inst.s -o main_inst -L$<path to runtime .a> -l$systest_runtime -pthread 

The runtime library is written in C++ so it requires the C++ stdlibs to be
linked in or just use `clang++`/`g++`.


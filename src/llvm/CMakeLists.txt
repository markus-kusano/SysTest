cmake_minimum_required(VERSION 2.8.8)

find_package(LLVM REQUIRED CONFIG)

add_definitions(${LLVM_DEFINITIONS} -std=c++11 -g -Wall -fno-rtti)
include_directories(${LLVM_INCLUDE_DIRS})

add_subdirectory(systest)

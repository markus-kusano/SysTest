cmake_minimum_required(VERSION 2.6)

find_package(LLVM REQUIRED CONFIG)

add_definitions(${LLVM_DEFINITIONS} -std=c++11)
include_directories(${LLVM_INCLUDE_DIRS})

add_subdirectory(systest)

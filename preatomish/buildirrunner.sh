#!/bin/bash
PATH="`pwd`/vendor/llvm-built/bin/:$PATH"
g++ -g llvmirrunner.cpp `llvm-config --cxxflags --ldflags --libs all` -O3 -o llvmirrunner

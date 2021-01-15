#!/bin/bash
dir=`pwd`
target=${dir##*/}

echo Cleaning
make -j4 VPATH="../../Source" -f ../makefile_scarc clean

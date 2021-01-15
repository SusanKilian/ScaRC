#!/bin/bash

dir=`pwd`
target=${dir##*/}

echo Cleaning
`ifort -v`

make -j4 VPATH="../../Source" -f ../makefile_scarc clean

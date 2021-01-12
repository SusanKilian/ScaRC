#!/bin/bash
# --------------------------------------------------------------------------
# run a single case
#
# Syntax:     ./run_scarc.sh #nproc dirname fdsgeom
# Example:    ./run_scarc.sh 4 Pressure_Solver poisson2d_4mesh_scarc.fds
#
# --------------------------------------------------------------------------
CURDIR=`pwd`
FDSDIR=`dirname $CURDIR`              # links to main FDS-repository

MPIPROC=$1
CASEDIR=$2
FDSNAME=$3

EXEDBG=$FDSDIR/Build/mpi_intel_osx_64_db
EXEOPT=$FDSDIR/Build/mpi_intel_osx_64

cd $CURDIR/$CASEDIR
echo mpirun -np $MPIPROC $EXEOPT/fds_mpi_intel_osx_64 $FDSNAME
mpirun -np $MPIPROC $EXEOPT/fds_mpi_intel_osx_64 $FDSNAME
cd $CURDIR

#!/bin/sh
#
#SBATCH -N 1
#SBATCH --tasks-per-node=5
#SBATCH --cpus-per-task=4
#SBATCH --exclusive
#
#SBATCH -t 00:59:00
# 
#SBATCH -o output/stdout_%j.out
#SBATCH -e output/stderr_%j.out
#
#SBATCH  -A  snic2020-5-79
#
#SBATCH --mail-user=susanne.kilian@me.com
#SBATCH --mail-type=ALL
##SBATCH --qos=test

if [ $# -ne 1 ]; then
    echo --------------------------------------------
    echo Error when executing $0 
    echo misssing name of fds input file
    echo usage: sbatch -J filename.fds run_fds6_mpi.sh  filename.fds
    echo example: sbatch -J roomfire.fds run_fds6_mpi.sh  roomfire.fds
    echo --------------------------------------------
    exit 1
fi
FDS_FILE=$1
dir_name=$PWD
echo $dir_name

module purge
module load intel/2018a


export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK


export FDS_AVX="/lunarc/nobackup/users/skil/GIT/ScaRC/Build/impi_intel_linux_64_avx/fds_impi_intel_linux_64_avx"


srun $FDS_AVX $FDS_FILE >avx1.out 2>avx2.out



#!/bin/bash
#SBATCH -N 1
#SBATCH -n 16
#SBATCH -t 01:00:00
#SBATCH -J letkf
#SBATCH -p debug
#SBATCH -A r00296
#SBATCH --ntasks-per-node=16
#SBATCH --mem=128GB
module unload xalt
module load openmpi
ulimit -s unlimited
cd /N/slate/ckieu/tc_wrf_letkf/letkf/
mpirun -n 16 ./letkf_mpi.exe

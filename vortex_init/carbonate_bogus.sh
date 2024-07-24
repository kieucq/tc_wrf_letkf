#!/bin/bash 
#
# NOTE: This script is for producing a bogus sythetic vortex
#       from the TC vital information, which is used by 
#       LETKF to generate ensemble system
#
# HIST: - Mar 28, 2013: created by CK
#
# AUTHOR: Chanh Kieu (chanh.kieu@noaa.gov)
#===========================================================
#SBATCH -N 1
#SBATCH -n 16
#SBATCH -t 02:00:00
#SBATCH -J wrf
#SBATCH --ntasks-per-node=16
#SBATCH --mem=128GB
#module unload intel-mpi/19.0.5
#module load openmpi/intel/4.0.1
module unload xalt
ulimit -s unlimited
set -x
if [ $# == 1 ]; then
  time_input=$1
  data_year_vinit=`echo ${time_input} | cut -c1-4`
  data_month_vinit=`echo ${time_input} | cut -c5-6`
  data_day_vinit=`echo ${time_input} | cut -c7-8`
  data_hour_vinit=`echo ${time_input} | cut -c9-10`
  data_minute_vinit=`echo ${time_input} | cut -c11-12`
else
  echo " BOGUS: Please use the format "
  echo " ./runvinit.sh YYYYMMDDHH"
  echo " VINIT is exiting 1"
  exit 1
fi
echo " BOGUS: Running year is: $data_year_vinit"
echo " BOGUS: Running month is: $data_month_vinit"
echo " BOGUS: Running day is: $data_day_vinit"
echo " BOGUS: Running hour is: $data_hour_vinit"
echo " BOGUS: Number of ne is: $ne"
#
# linking namelist and background GFS file here
#
rm -f namelist.letkf wrfinput_d01 tmpvit log*
ln -sf ../namelist.letkf ./
cp ../ana/$time_input/wrfinput_d01 ./
cp ../tcvital/vital_record.txt ./tmpvit
#
# running the vortex creation program now to generate 
# sythetic radiosonde vortex obs
#
rm -rf bvortex.txt vox_${data_year_vinit}-${data_month_vinit}-${data_day_vinit}_${data_hour_vinit}*
./bvortex.exe
./prep_obs.exe
#
# rename observation file for later used by LETKF 
#
check_file=`ls -la ./obs.dat | awk '{print $5}'`
echo "BOGUS: check_file = $check_file"
if (( check_file < 1000 )); then
   echo " BOGUS: obs.dat file size for bogus vortex is too small...exit 1"
   exit 1
else
   obs_file=obs_${data_year_vinit}-${data_month_vinit}-${data_day_vinit}_${data_hour_vinit}:${data_minute_vinit}:00
   vox_file=vox_${data_year_vinit}-${data_month_vinit}-${data_day_vinit}_${data_hour_vinit}:${data_minute_vinit}:00
   mv obs.dat ./$vox_file
   cp $vox_file ../obs/$obs_file
fi
rm -f namelist.letkf wrfinput_d01 tmpvit log* in.txt input.sh
echo " BOGUS: bogus vortex intialization finishes safely"

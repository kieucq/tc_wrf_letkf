#!/bin/sh
#
# [NOTE]:   
#           This shell script is for running letkf  
#
# [HISTORY]:
#         - 27 Mar, 2010: Created
#         - 09 Apr, 2010: update the runmode to read from environment for better
#                         communication with the runmain.sh
#
# [AUTHOR]:
#           Chanh Kieu, Vietnam National University (kieucq@atmos.umd.edu)
#
# [COPYRIGHT]: GNU open source
#             
#===========================================================================================
#
# option for real-time mode or experiment mode
#
set -x
. ./input.sh
if [ "$runmode" == "" ]; then
 echo " LEKTF: runmode is empty, check runmain.sh ...exit 1"
 exit 1
fi
if [ "$DIR_RUN" == "" ]; then
 echo " LEKTF: letkf running dir is empty, check runmain.sh ...exit 1"
 exit 1
fi
if [ $# == 2 ]; then
  time_input=$1
  data_year_let=`echo ${time_input} | cut -c1-4`
  data_month_let=`echo ${time_input} | cut -c5-6`
  data_day_let=`echo ${time_input} | cut -c7-8`
  data_hour_let=`echo ${time_input} | cut -c9-10`
  data_minute_let=`echo ${time_input} | cut -c11-12`
  ne=$2          
else
  echo " VORT_DA: Please use the format "
  echo " ./runvortda.sh YYYYMMDDHH ne"
  echo " VORTDA is exiting 0"
  exit 0
fi		 
echo " VORT_DA: Running year is: $data_year_let"
echo " VORT_DA: Running month is: $data_month_let"
echo " VORT_DA: Running day is: $data_day_let"
echo " VORT_DA: Running hour is: $data_hour_let"
echo " VORT_DA: Number of ne is: $ne"
#
# checking for the input GFS data to see if the data available
# 
echo " VORT_DA: Start looping now ..."
nfile=$ne
ifile=1
obs_file=obs_${data_year_let}-${data_month_let}-${data_day_let}_${data_hour_let}:${data_minute_let}:00
bgd_file=${data_year_let}-${data_month_let}-${data_day_let}_${data_hour_let}:${data_minute_let}:00
time_string=${data_year_let}${data_month_let}${data_day_let}${data_hour_let}${data_minute_let}
rm -f bgd_*.dat *~ obs.dat ana_*.dat
if [ -f $DIR_OBS/$obs_file ]; then
 echo " VORT_DA: $DIR_OBS/$obs_file exists... linking"
 ln -sf $DIR_OBS/$obs_file ./obs.dat
else
 echo " VORT_DA: $DIR_OBS/$obs_file does not exist... exit"
 exit 1
fi
while [ "$ifile" -le "$nfile" ]; do								  
 if [ "$ifile" -lt 10 ]; then
  prefix="00${ifile}"
 elif [ "$ifile" -lt 100 ]; then
  prefix="0${ifile}"
 elif [ "$ifile" -lt 1000 ]; then
  prefix="${ifile}"
 else
  echo " TO MANY MEMBER...exit 10"
  exit 10
 fi
 check_file="$DIR_BGD/${time_string}/bgd_d01_${prefix}_${bgd_file}"      	
 if [ -f $check_file ]; then
  echo " VORT_DA: input file: $check_file exists"
  ln -sf $check_file ./bgd_${prefix}.dat
  if [ "$runmode" == "EXP_IDEAL" ] || [ "$runmode" == "EXP_REAN" ]; then
   cp -f bgd_${prefix}.dat ana_${prefix}.dat
   if [ "$ifile" -eq "$nfile" ]; then
    ./letkf_restart.exe >> $DIR_RUN/log.vortda
   fi
  else
   cp -f wrfinput_d01 ana_${prefix}.dat
  fi  
 else
  echo "   VORT_DA: input file: $check_file does not exists... exit 3"
  exit 3
 fi
 ifile=$(($ifile+1))   
done
rm -f *~ error.status
#qsub job_card_vortda_karst.sh
#mpirun -np 1 ./letkf_mpi.exe
srun -n 4 ./letkf_mpi.exe
iloop=1
until [ -f ./error.status ]; do
 sleep 30
 iloop=$(($iloop+1))
 if (( iloop > 100)); then  
  echo " VORT_DA: letkf_mpi.exe has abrupt die..."
  exit 3
 fi
done
rm -f error.status
#
# backup the GFS init background to gfs dir and move all of 
# new init with bogused vortex to the bgd dir
#
mkdir -p $DIR_BGD/${time_string}/bogus
#mv $DIR_BGD/${time_string}/bgd_d01* $DIR_BGD/${time_string}/gfs/
ifile=1
while [ "$ifile" -le "$nfile" ]; do								  
 if [ "$ifile" -lt 10 ]; then
  prefix="00${ifile}"
 elif [ "$ifile" -lt 100 ]; then
  prefix="0${ifile}"
 elif [ "$ifile" -lt 1000 ]; then
  prefix="${ifile}"
 else
  echo " TO MANY MEMBER...exit 10"
  exit 10
 fi
 cp -f ana_${prefix}.dat $DIR_BGD/${time_string}/bogus/bgd_d01_${prefix}_${bgd_file}
 ifile=$(($ifile+1))   
done
echo " VORT_DA script finished normally     "
echo " VORT_DA script finished normally     " > $DIR_RUN/error.status
rm -f bgd_*.dat wrfinput_d0* wrfbdy_d0*

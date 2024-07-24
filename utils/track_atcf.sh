#!/bin/bash
#
# NOTE: This script is for pulling the track data from an ATCF file and
#       plot either tracks for an ensemble of models (ensemble mode) or
#       tracks for a composite of a single storm (composite mode).
#
# INPUT:
#
# DEPENDENCE:
#
# HISTORY: - Jan 06, 2012: created 
#       
# AUTHOR: Chanh Kieu (chanh.kieu@noaa.gov, NOAA/NWS/NCEP/EMC)
#
#======================================================================
#
# set up enviroment
#
home_path="/state/partition1/home/kieuc/bio/ncep/programs"
adeck_path="${home_path}/Statistics/adeck"
bdeck_path="${home_path}/Statistics/bdeck"
out_interval=3
#
# check for the script input
#
if [ "$1" == "composite" ] && [ $# == 6 ];  then
 date_start=$2
 date_end=$3
 model=$4
 storm=$5
 interval=$6
 adeck="${adeck_path}/a${storm}.dat"
 bdeck="${bdeck_path}/b${storm}.dat"
 echo "Starting date for composite plot is: $date_start"
 echo "Ending date for composite plot is: $date_end"
 echo "Adeck file is: $adeck"
 echo "Bdeck file is: $bdeck"
 check_model=`grep $model $adeck | head -n 1`
 check_time1=`grep $date_start $adeck | head -n 1` 
 check_time2=`grep $date_end $adeck | head -n 1`
 if [ "$check_model" == "" ] || [ "$check_time1" == "" ] \
 || [ "$check_time2" == "" ]; then
  echo "Starting/ending date for composite plot donot exist in adeck..."
  exit 2
 else
  echo "Starting/ending date are validated. Continue ..."
 fi
#
# create a set of track text file for Grads plots. Each cycle correspnonds
# to a track file named track_mem_x.txt
#
 . ./func_cal_time2.sh
 fcount=1
 time_to_process=${date_start}
 while [ "$time_to_process" -le "${date_end}" ];
 do
  echo "Extracting the track forecast of $model at $time_to_process"
  grep $time_to_process $adeck | grep $model | awk '{print $6}' | sed 's/,//g' > fort.9
  grep $time_to_process $adeck | grep $model | awk '{print $7}' | sed 's/,//g' > fort.10
  grep $time_to_process $adeck | grep $model | awk '{print $8}' | sed 's/,//g' > fort.11
  lat_sign=`cat fort.10 | grep N | head -n 1`
  lon_sign=`cat fort.11 | grep W | head -n 1`
  if [ "$lat_sign" != "" ]; then
   cat fort.10 | sed 's/N//' | awk '{print $1/10}' > fort.100
  else
   cat fort.10 | sed 's/N//' | awk '{print -1*$1/10}' > fort.100
  fi
  if [ "$lon_sign" != "" ]; then
   cat fort.11 | sed 's/N//' | awk '{print -1*$1/10}' > fort.111
  else
   cat fort.11 | sed 's/N//' | awk '{print $1/10}' > fort.111
  fi 
  track_file="track_mem_${fcount}.txt"
  echo "Track forecast of $model for cycle $time_to_process" > ${track_file}
  echo "1 0.2" >> ${track_file}
  echo "22 1 9" >> ${track_file}
  echo "0 ${out_interval}" >> ${track_file}
  i=1
  while read ilon
  do
#  ftime=$((($i-1)*$out_interval))
   ftime=`head -n ${i} fort.9 | tail -1`
   ilat=`head -n ${i} fort.100 | tail -1`
   if [ "${ftime}" != "${ftime_old}" ]; then
    echo "$ftime $ilon $ilat" >> ${track_file}
   fi
   ftime_old=$ftime
   i=$(($i+1)) 
  done < fort.111
  addh ${time_to_process} ${interval} E_YEAR E_MONTH E_DAY E_HOUR
  time_to_process="${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}" 
  fcount=$(($fcount+1))
 done
 echo "$(($fcount-1))" > fort.12
elif [ "$1" == "ensemble" ] && [ $# == 4 ];  then
 date_start=$2
 storm=$3
 adeck="${adeck_path}/a${storm}.dat"
 bdeck="${bdeck_path}/b${storm}.dat" 
 input_file=$4
 echo "Starting date for ensemble plot is: $date_start"
 echo "Adeck file is: $adeck"
 echo "Bdeck file is: $bdeck"
 echo "Input namelist is $input_file"
else
 echo "Script syntax is not correct. Please enter either:"
 echo "./track_atcf.sh composite YYYYMMDDHH YYYYMMDDHH model storm HH"
 echo "or"
 echo "./track_atcf.sh ensemble YYYYMMDDHH storm track_namelist.txt"
 exit 1
fi

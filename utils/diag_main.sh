#!/bin/sh
#
# NOTE: This script is to analyze track and intensity error
#       for a set of model forecasts with the basin-wide
#       input that contains many models and cycles (a-deck)
#
# INPUT: 1. An input file "infile.txt" contains the full
#           path to the a-deck files
#        2. A best track data analysis file (observation)
#        These 2 inputs have to be linked to this home dir
#
# OUTPUT: multiple output files in grads format and track file 
#         in the text format for plotting.
#
# HIST: Nov 03, 2011: created
#
# AUTHOR: Chanh Kieu, EMC (chanh.kieu@noaa.gov)
#
#=================================================================
home="/state/partition1/home/kieuc/bio/ncep/programs"
models="HWRF GFDL"                               # forecast models
ahome="${home}/Tracks/2011/"
bhome="${home}/Tracks/2011/"
#
# create first a forecast lead time from the actf hist file 
#
. ./func_cal_time2.sh
rm -rf fort.* ./output/tmp.*
icycle=1
while read adeck
do
  storm_id=`echo "${adeck}" | cut -c2-9`
  bdeck="./b${storm_bs}${storm_id}${storm_yy}.dat"
  for model in `echo ${models}`
  do
   echo "Working with model = $model; adeck = $adeck; bdeck = $bdeck"
#
# first pick out the forecast of each model from the adeck
#
   tmp_file="./output/tmp.${model}.${storm_id}.dat"
   rm -rf ./output/tmp.${model}
   cat ${adeck} | grep ${model} > ${tmp_file}   
#
# next pick out the cycles of each model forecast, one cycle
# per file
#
   prefix="./output/tmp.${model}.${storm_id}.c"
   rm -rf ${prefix}*.dat
   icount=1
   itime=1
   while read inline
   do
    time_input="$inline"
    in_string=`echo ${time_input} | cut -c9-18`
    if [ "${icount}" -eq "1" ]; then
     echo "${inline}" > ${prefix}${itime}.dat
    else
     if [ "${in_string}" -eq "${save_string}" ]; then
      echo "${inline}" >> ${prefix}${itime}.dat
     else
      itime=$(($itime + 1))
      echo "${inline}" >> ${prefix}${itime}.dat
     fi
    fi
    save_string=${in_string}
    icount=$(($icount+1))
   done < ${tmp_file}
#
# advance time step in each cycle to match with observation
#
   for ifile in `ls ${prefix}*.dat`
   do
    echo "Doing statistics at cycle $ifile"
    rm -rf fort.10 
    while read line
    do
     time_input="$line"
     temp=`echo "${time_input}" | cut -c31-31`
     in_string=`echo ${time_input} | cut -c9-18`
     if [ "$temp" -eq "0"  ]; then
      fsct_time=`echo "${time_input}" | cut -c32-33`
     else
      fsct_time=`echo "${time_input}" | cut -c31-33`
     fi
     fsct_time=`echo ${fsct_time} | tr -s " "`
     addh ${in_string} ${fsct_time} E_YEAR E_MONTH E_DAY E_HOUR
     fsc_time="${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}" 
     new_line=`echo "$line" | sed "s/${in_string}/${fsc_time}/"`
     echo "$new_line" ${in_string} >> fort.10
    done < ${ifile}
    cp ./fort.10 ${ifile}
#
# call an external fortran code to do statistics for each cycle
# per each mode
# 
    ln -sf ${bdeck} ./fort.11
    ls fort.10 > in.txt
    ls fort.11 >> in.txt
    ./diag_core.exe < in.txt > log.diag_core
    cycle_id=`echo ${ifile} | sed "s/\.\/output\/tmp//"`
    mv fsct_error.dat ./output/diag_fsct_error${cycle_id}
    mv grads.dat ./output/diag_grads${cycle_id}
    mv track_fst.dat ./output/diag_track_fst${cycle_id}
    mv track_obs.dat ./output/diag_track_obs${cycle_id}
   done 
#
# now combine the statistics for all cycles for each case
#
#   icount=1
#   track_file="./output/track_error_${model}_${storm_id}"
#   vmax_file="./output/vmax_error_${model}_${storm_id}"
#   rm -rf $track_file $vmax_file
#   for ifile in `ls ./output/diag_fsct*${model}*${storm_id}*.dat`
#   do
#    if [ "$icount" -eq "1" ]; then
#     cat ${ifile} | grep "Lead" > ${track_file}
#     cat ${ifile} | grep "Lead" > ${vmax_file}
#    fi
#    cat ${ifile} | grep "Track" >> ${track_file}
#    cat ${ifile} | grep "Vmax" >> ${vmax_file}
#    cat ${ifile} | grep "Pmin" >> ${vmax_file}
#    icount=$(($icount+1))
#   done 
  done
done < infile.txt
#
# Finally combine all kind of errors into a single file, one
# for each model
#
for model in `echo ${models}`
do
 diag_err="./statistics_${model}.dat"
 rm -rf $diag_err
 for ifile in `ls ./output/diag_fsct*${model}*.dat`
 do
  cat ${ifile} >> ${diag_err}
 done
done  

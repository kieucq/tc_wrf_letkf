#!/bin/bash
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
ahome="${home}/Tracks/2011/"
bhome="${home}/Tracks/2011/"
cycle="2011082300"
ncycle=21
dt=6
bdeck="./bal092011.dat"
adeck="./aal092011.dat"
prefix="irene09l.2011082300.hwrfprs_n.grbf"
#
# create first a forecast lead time from the actf hist file 
#
. ./func_cal_time2.sh
icycle=2
ncycle=21
dt=6
while [ "$icycle" -le "$ncycle" ]; do
 time=$((($icycle-1)*$dt)) 
 if [ "$time" -le 9 ]; then
  time_idex="0$time"
 else
  time_idex="$time"
 fi
 grib_file="${prefix}${time_idex}"
 ctl_file="${prefix}${time_idex}.ctl"
 /misc/whome/Chanh.Kieu/opt/bin/grib2ctl.pl -verf ${grib_file} > ${ctl_file}
 gribmap -i ${ctl_file}
 ln -sf  ./${ctl_file}  ./fort.ctl
 addh ${cycle} ${time} E_YEAR E_MONTH E_DAY E_HOUR
 fsc_time="${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}"
 echo "grib file: $grib_file"
 echo "ctl file: $ctl_file"
 echo "validate time: $fsc_time"
#
# filter the radius from the bdeck file
#
 ne34=`grep $fsc_time ${bdeck} | awk '{if ($12 == "34,") print $14}' | sed 's/,//g'`
 se34=`grep $fsc_time ${bdeck} | awk '{if ($12 == "34,") print $15}' | sed 's/,//g'`
 sw34=`grep $fsc_time ${bdeck} | awk '{if ($12 == "34,") print $16}' | sed 's/,//g'`
 nw34=`grep $fsc_time ${bdeck} | awk '{if ($12 == "34,") print $17}' | sed 's/,//g'`
 ne50=`grep $fsc_time ${bdeck} | awk '{if ($12 == "50,") print $14}' | sed 's/,//g'`
 se50=`grep $fsc_time ${bdeck} | awk '{if ($12 == "50,") print $15}' | sed 's/,//g'`
 sw50=`grep $fsc_time ${bdeck} | awk '{if ($12 == "50,") print $16}' | sed 's/,//g'`
 nw50=`grep $fsc_time ${bdeck} | awk '{if ($12 == "50,") print $17}' | sed 's/,//g'`
 ne64=`grep $fsc_time ${bdeck} | awk '{if ($12 == "64,") print $14}' | sed 's/,//g'`
 se64=`grep $fsc_time ${bdeck} | awk '{if ($12 == "64,") print $15}' | sed 's/,//g'`
 sw64=`grep $fsc_time ${bdeck} | awk '{if ($12 == "64,") print $16}' | sed 's/,//g'`
 nw64=`grep $fsc_time ${bdeck} | awk '{if ($12 == "64,") print $17}' | sed 's/,//g'`
 echo "ne34 = $ne34"
 echo "se34 = $se34"
 echo "sw34 = $sw34"
 echo "nw34 = $nw34"
 echo "ne50 = $ne50"
 echo "se50 = $se50"
 echo "sw50 = $sw50"
 echo "nw50 = $nw50"
 echo "ne64 = $ne64"
 echo "se64 = $se64"
 echo "sw64 = $sw64"
 echo "nw64 = $nw64"
#
# find the forecast lat/lon
#
 if [ "$time" -le 9 ]; then
  index="00${time},"
 elif [ "$time" -le 99 ]; then
  index="0${time},"
 else
  index="${time},"
 fi
 arg1="${cycle},"
 arg2="HWRF,"
# flat=`awk '{if ($3 == "'$arg1'" && $5 == "'$arg2'" && $6 == "'$index'") print $0}' $adeck | awk '{print $7}' | head -n 1 | sed 's/,//g'`
 flon=`awk '{if ($6 == "'$index'") print $0}' $adeck | awk '{print $7}' | head -n 1 | sed 's/,//g'`
 flon=`awk '{if ($6 == "'$index'") print $0}' $adeck | awk '{print $8}' | head -n 1 | sed 's/,//g'`
 echo "forecast lat = $flat"
 echo "forecast lon = $flon"
 lati=`echo $flat | cut -c1-2` 
 latr=`echo $flat | cut -c3`
 latf="${lati}.${latr}"
 loni=`echo $flon | cut -c1-2`
 lonr=`echo $flon | cut -c3`
 sign=`echo $flon | cut -c4`
 if [ "${sign}" == "W" ]; then
  lonf="-${loni}.${lonr}"
 else
  lonf="${loni}.${lonr}"
 fi
 echo "forecast lat = $latf"
 echo "forecast lon = $lonf"
#
# convert the radii to lat/lon and put in a file by multiping by sin(pi/4)
# 1 nm = 1.852 km
# sin(pi/4) = 0.707
# 1 degree = 111 km
#
 radii_file="radii_${cycle}_${time_idex}.txt"
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$ne34" != "" ]; then
  lat_ne34=`echo "($latf + 0.707*$ne34*1.852/111)" | bc -l`
  lon_ne34=`echo "($lonf + 0.707*$ne34*1.852/111)" | bc -l`
  lat_ne34=`printf "%.1f\n" $lat_ne34`
  lon_ne34=`printf "%.1f\n" $lon_ne34`
 else
  lat_ne34=-999
  lon_ne34=-999
 fi
 echo "lat_ne34 = ${lat_ne34}" > $radii_file
 echo "lon_ne34 = ${lon_ne34}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$se34" != "" ]; then
  lat_se34=`echo "($latf - 0.707*$se34*1.852/111)" | bc -l`
  lon_se34=`echo "($lonf + 0.707*$se34*1.852/111)" | bc -l`
  lat_se34=`printf "%.1f\n" $lat_se34`
  lon_se34=`printf "%.1f\n" $lon_se34`
 else
  lat_se34=-999
  lon_se34=-999
 fi
 echo "lat_se34 = ${lat_se34}" >> $radii_file
 echo "lon_se34 = ${lon_se34}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$sw34" != "" ]; then
  lat_sw34=`echo "($latf - 0.707*$sw34*1.852/111)" | bc -l`
  lon_sw34=`echo "($lonf - 0.707*$sw34*1.852/111)" | bc -l`
  lat_sw34=`printf "%.1f\n" $lat_sw34`
  lon_sw34=`printf "%.1f\n" $lon_sw34`
 else
  lat_sw34=-999
  lon_sw34=-999
 fi
 echo "lat_sw34 = ${lat_sw34}" >> $radii_file
 echo "lon_sw34 = ${lon_sw34}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$nw34" != "" ]; then
  lat_nw34=`echo "($latf + 0.707*$nw34*1.852/111)" | bc -l`
  lon_nw34=`echo "($lonf - 0.707*$nw34*1.852/111)" | bc -l`
  lat_nw34=`printf "%.1f\n" $lat_nw34`
  lon_nw34=`printf "%.1f\n" $lon_nw34`
 else
  lat_nw34=-999
  lon_nw34=-999
 fi
 echo "lat_nw34 = ${lat_nw34}" >> $radii_file
 echo "lon_nw34 = ${lon_nw34}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$ne50" != "" ]; then
  lat_ne50=`echo "($latf + 0.707*$ne50*1.852/111)" | bc -l`
  lon_ne50=`echo "($lonf + 0.707*$ne50*1.852/111)" | bc -l`
  lat_ne50=`printf "%.1f\n" $lat_ne50`
  lon_ne50=`printf "%.1f\n" $lon_ne50`
 else
  lat_ne50=-999
  lon_ne50=-999
 fi
 echo "lat_ne50 = ${lat_ne50}" >> $radii_file
 echo "lon_ne50 = ${lon_ne50}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$se50" != "" ]; then
  lat_se50=`echo "($latf - 0.707*$se50*1.852/111)" | bc -l`
  lon_se50=`echo "($lonf + 0.707*$se50*1.852/111)" | bc -l`
  lat_se50=`printf "%.1f\n" $lat_se50`
  lon_se50=`printf "%.1f\n" $lon_se50`
 else
  lat_se50=-999
  lon_se50=-999
 fi
 echo "lat_se50 = ${lat_se50}" >> $radii_file
 echo "lon_se50 = ${lon_se50}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$sw50" != "" ]; then
  lat_sw50=`echo "($latf - 0.707*$sw50*1.852/111)" | bc -l`
  lon_sw50=`echo "($lonf - 0.707*$sw50*1.852/111)" | bc -l`
  lat_sw50=`printf "%.1f\n" $lat_sw50`
  lon_sw50=`printf "%.1f\n" $lon_sw50`
 else
  lat_sw50=-999
  lon_sw50=-999
 fi
 echo "lat_sw50 = ${lat_sw50}" >> $radii_file
 echo "lon_sw50 = ${lon_sw50}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$nw50" != "" ]; then
  lat_nw50=`echo "($latf + 0.707*$nw50*1.852/111)" | bc -l`
  lon_nw50=`echo "($lonf - 0.707*$nw50*1.852/111)" | bc -l`
  lat_nw50=`printf "%.1f\n" $lat_nw50`
  lon_nw50=`printf "%.1f\n" $lon_nw50`
 else
  lat_nw50=-999
  lon_nw50=-999
 fi
 echo "lat_nw50 = ${lat_nw50}" >> $radii_file
 echo "lon_nw50 = ${lon_nw50}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$ne64" != "" ]; then
  lat_ne64=`echo "($latf + 0.707*$ne64*1.852/111)" | bc -l`
  lon_ne64=`echo "($lonf + 0.707*$ne64*1.852/111)" | bc -l`
  lat_ne64=`printf "%.1f\n" $lat_ne64`
  lon_ne64=`printf "%.1f\n" $lon_ne64`
 else
  lat_ne64=-999
  lon_ne64=-999
 fi
 echo "lat_ne64 = ${lat_ne64}" >> $radii_file
 echo "lon_ne64 = ${lon_ne64}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$se64" != "" ]; then
  lat_se64=`echo "($latf - 0.707*$se64*1.852/111)" | bc -l`
  lon_se64=`echo "($lonf + 0.707*$se64*1.852/111)" | bc -l`
  lat_se64=`printf "%.1f\n" $lat_se64`
  lon_se64=`printf "%.1f\n" $lon_se64`
 else
  lat_se64=-999
  lon_se64=-999
 fi
 echo "lat_se64 = ${lat_se64}" >> $radii_file
 echo "lon_se64 = ${lon_se64}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$sw64" != "" ]; then
  lat_sw64=`echo "($latf - 0.707*$sw64*1.852/111)" | bc -l`
  lon_sw64=`echo "($lonf - 0.707*$sw64*1.852/111)" | bc -l`
  lat_sw64=`printf "%.1f\n" $lat_sw64`
  lon_sw64=`printf "%.1f\n" $lon_sw64`
 else
  lat_sw64=-999
  lon_sw64=-999
 fi
 echo "lat_sw64 = ${lat_sw64}" >> $radii_file
 echo "lon_sw64 = ${lon_sw64}" >> $radii_file
#
 if [ "$latf" != "." ] && [ "$lonf" != "."  ] && [ "$nw64" != "" ]; then
  lat_nw64=`echo "($latf + 0.707*$nw64*1.852/111)" | bc -l`
  lon_nw64=`echo "($lonf - 0.707*$nw64*1.852/111)" | bc -l`
  lat_nw64=`printf "%.1f\n" $lat_nw64`
  lon_nw64=`printf "%.1f\n" $lon_nw64`
 else
  lat_nw64=-999
  lon_nw64=-999
 fi
 echo "lat_nw64 = ${lat_nw64}" >> $radii_file
 echo "lon_nw64 = ${lon_nw64}" >> $radii_file
#
# plot grads
#
 ln -sf $radii_file ./fort.10
 grads -xlbc plot_radii.gs
 mv out.png ./radii_${cycle}_${time_idex}.png
 icycle=$(($icycle+1))
done 
#rm -f out* fort*

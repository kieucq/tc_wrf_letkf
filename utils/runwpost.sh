#!/bin/sh
#
# [NOTE]:   
#           This shell script is for creating a set of figs for real-time 
#           application. 
#
# [HISTORY]:
#         - 07 Oct, 2010: Created
#
# [AUTHOR]:
#           Chanh Kieu, Vietnam National University (kieucq@atmos.umd.edu)
#
# [COPYRIGHT]: GNU open source
#             
#===========================================================================================
# 
# setting up path environment
#
rm -f *.dat wpost.dat wpost.ctl wrfout*
DIR_FSC="../fsc"
DIR_TRU="../truth"
DIR_CTL="../ctl"
#
# option for real-time mode or experiment mode
#
if [ $# == 2 ]; then
  time_input=$1
  data_year_rms=`echo ${time_input} | cut -c1-4`
  data_month_rms=`echo ${time_input} | cut -c5-6`
  data_day_rms=`echo ${time_input} | cut -c7-8`
  data_hour_rms=`echo ${time_input} | cut -c9-10`
  data_minute_rms=`echo ${time_input} | cut -c11-12`
  ne=$2
else
  echo " WPOST: Please use the format "
  echo " ./runrms.sh YYYYMMDDHH ne"
  echo " WPOST is exiting 0"
  exit 0
fi         
#
# force the analysis to be at the restart cycle rather than
# wrf output interval
#
echo " WPOST: Running year is: $data_year_rms"
echo " WPOST: Running month is: $data_month_rms"
echo " WPOST: Running day is: $data_day_rms"
echo " WPOST: Running hour is: $data_hour_rms"
echo " WPOST: Forecast minute is: $data_minute_rms"
echo " WPOST: Number of members is $ne"
wtime=${data_year_rms}${data_month_rms}${data_day_rms}${data_hour_rms}${data_minute_rms}
rm -rf $wtime
mkdir -p $wtime
mkdir -p $wtime/Figures
mkdir -p $wtime/data
echo ${data_year_rms}${data_month_rms}${data_day_rms}${data_hour_rms} > wpost.txt
#
# loop thru all members at each fsc time
#
ie=1
while [ "$ie" -le "$ne" ]; do                                                                
#
# link wrfout data 
#
 if [ "$ie" -lt 10 ]; then
  prefix="00${ie}"
 elif [ "$ie" -lt 100 ]; then
  prefix="0${ie}"
 elif [ "$ie" -lt 1000 ]; then
  prefix="${ie}"
 else
  echo " MAIN: Too many members...exit 10"
  exit 10
 fi
 ln -sf $DIR_FSC/${wtime}/mem_${prefix}/wrfout_d01* ./
#
# create a namelist for wrf_to_grads
#
 ls wrfout* > log
 if [ "$ie" == 1 ]; then
 cat > control_file_wpost << EOF
-1                  ! negative means ignore the times
2007-07-16_00:00:00
end_of_time_list
U                   ! U Compoment of wind
V                   ! V Component of wind
 THETA               ! Theta
 TC                  ! Temperature in C
Z                   ! Height (m)
RH                  ! Relative Humidity (diagnostic)
QCLOUD              ! cloud
end_of_3dvar_list
slvl                ! sea level pressure
RAINC               ! ACCUMULATED TOTAL CUMULUS PRECIPITATION
RAINCV              ! TIME-STEP CUMULUS PRECIPITATION
RAINNC              ! ACCUMULATED TOTAL GRID SCALE PRECIPITATION
T2                  ! TEMP at 2 M
end_of_2dvar_list
EOF
 n=1
 while read line
 do
  echo "$line" >> control_file_wpost
  n=$(($n+1))
 done < log
 cat >> control_file_wpost << EOF1
end_of_file_list
                    ! required blank
real                ! real (input/output)/ideal/static
1                   ! 0=no map bgd, 1=map bgd 
1                   ! specify grads vertical grid  
                    !
                    !
                    !
1000
990
950
900
800
700
600
500
400
300
200
100
50
EOF1
 fi
#
# run the wrfpost now and create figures
#
 ./wrf_to_grads control_file_wpost wpost
 grads -lbxc wpost.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}
 mkdir -p ${wtime}/data/mem_${prefix}
 mv *.gif ./${wtime}/Figures/mem_${prefix}/
#
 grads -xlbc liberia_rain.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/liberia_rain
 mv prep_* ./${wtime}/Figures/mem_${prefix}/liberia_rain/
 grads -xlbc ghana_rain.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/ghana_rain
 mv prep_* ./${wtime}/Figures/mem_${prefix}/ghana_rain/
#
 grads -xlbc liberia_temp.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/liberia_temp
 mv temp_* ./${wtime}/Figures/mem_${prefix}/liberia_temp/
 grads -xlbc ghana_temp.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/ghana_temp
 mv temp_* ./${wtime}/Figures/mem_${prefix}/ghana_temp/
#
 grads -xlbc liberia_wind.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/liberia_wind
 mv wind_* ./${wtime}/Figures/mem_${prefix}/liberia_wind/
 grads -xlbc ghana_wind.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/ghana_wind
 mv wind_* ./${wtime}/Figures/mem_${prefix}/ghana_wind/
#
 grads -xlbc liberia_cloud.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/liberia_cloud
 mv cloud_* ./${wtime}/Figures/mem_${prefix}/liberia_cloud/
 grads -xlbc ghana_cloud.gs
 mkdir -p ${wtime}/Figures/mem_${prefix}/ghana_cloud
 mv cloud_* ./${wtime}/Figures/mem_${prefix}/ghana_cloud/
#
 mv wpost.dat ./${wtime}/data/mem_${prefix}/
 mv wpost.ctl ./${wtime}/data/mem_${prefix}/
#
# loop thru other members
#
 ie=$(($ie+1))
done
#
# copy the product to UMD server
#
#scp -r ./${wtime}/Figures kieucq@halo.atmos.umd.edu:/homes/metogra/kieucq/www/wrf_letkf/
cp -rf ${wtime}/Figures /mnt/clusterweb/letkf/
cp -rf ${wtime}/Figures/mem_001/liberia* /mnt/clusterweb/countries/Liberia/
cp -rf ${wtime}/Figures/mem_001/ghana* /mnt/clusterweb/countries/Ghana/
echo "                                     "
echo " WPOST script finished normally      "


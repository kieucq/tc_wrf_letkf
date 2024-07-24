#!/bin/sh
#
# [NOTE]:   
#           This shell script is for creating a set of figs for real-time 
#           application. 
#
# [HISTORY]:
#         - 07 Oct, 2010: Created by CK @ VNU
#         - 02 May, 2022: revised for IU system
#
# [AUTHOR]:
#           Chanh Kieu, Indiana University (ckieu@iu.edu)
#
# [COPYRIGHT]: GNU open source
#             
#===========================================================================================
# 
# setting up path environment
#
rm -f *.dat wpost.dat wpost.ctl wrfout*
DIR_FSC="../fsc"
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
cat > wrf2grads_mean_in.txt << EOF2
k_shear1   = 2  ! height to search for lower shear
k_shear2   = 10 ! height to search for upper shear
k_w        = 10 ! height to search for max w
k_rh       = 10 ! height to search for mid level rh
k_area     = 10 ! area to search for rmw
k_rmw      = 2  ! height to search for maximum surface wind
storm_move = 0  ! 0- no move; 1-following storm (need fort.10)
tc_mv_time = 1  ! step to update TC search center
EOF2
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
   ln -sf $DIR_FSC/${wtime}/mem_${prefix}/wrfout* ./
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
    mv *.png ./${wtime}/Figures/mem_${prefix}/
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
rm wrfout* log
echo "                                     "
echo " WPOST script finished normally      "


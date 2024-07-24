#!/bin/bash -l
#SBATCH -N 1
#SBATCH -n 16
#SBATCH -t 01:00:00
#SBATCH -J wrfreal
#SBATCH -p debug
#SBATCH -A r00296
#SBATCH --ntasks-per-node=16
#SBATCH --mem=128GB
#module unload xalt
ulimit -s unlimited
set -x
#
# [NOTE]:   
#           This shell script is for running the real.exe of WRF, which is called by a master 
#           script for an ensemble breeding run.
#
# [RUN]:
#	    To run the script, need to compile WRF code first, then modify some env setup
#           and boundary interval
#
# [HISTORY]:
#         - Mar 27, 2010: updated from the breeding project
#         - Apr 20, 2010: update with idealized run and move all namelist to an external script
#         - May 01, 2010: change runmode to input para to ease the call from main script   
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
DIR_RUN=`pwd`
DIR_WPS="${DIR_MAN}/model/WPS"
DIR_WRF_REAL="${DIR_MAN}/model/WRFV3/test/em_real"
DIR_WRF_IDEAL="${DIR_MAN}/model/WRFV3/test/em_quarter_ss"
DIR_ANA="${DIR_MAN}/ana"
interval_bnd=`grep BND_INTERVAL input.sh | cut -d '=' -f 2`
d01_nx=`grep D01_NX input.sh | cut -d '=' -f 2`
d01_ny=`grep D01_NY input.sh | cut -d '=' -f 2`
d01_nz=`grep D01_NZ input.sh | cut -d '=' -f 2`
d01_dx=`grep D01_DX input.sh | cut -d '=' -f 2`
d01_dy=`grep D01_DY input.sh | cut -d '=' -f 2`
d01_dt=`grep D01_DT input.sh | cut -d '=' -f 2`
max_dom=`grep MAXIMUM_DOM input.sh | cut -d '=' -f 2`
runmode_m=`grep RUNMODE input.sh | cut -d '=' -f 2`
if [ "$interval_bnd" == "" ]; then
    echo " WRF-REAL: interval_bnd is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$runmode_m" == "" ]; then
    echo " WRF-REAL: runmode is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$d01_nz" == "" ]; then
    echo " WRF-REAL: d01_nz is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$max_dom" == "" ]; then
    echo " WRF-REAL: max_dom is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$d01_nx" == "" ]; then
    echo "WRF-REAL:  d01_nx is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$d01_ny" == "" ]; then
    echo " WRF-REAL: d01_ny is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$d01_dx" == "" ]; then 
    echo " WRF-REAL: d01_dx is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$d01_dy" == "" ]; then
    echo " WRF-REAL: d01_dy is empty, check runmain.sh ...exit 1"
    exit 1
elif [ "$d01_dt" == "" ]; then
    echo " WRF-REAL: d01_dt is empty, check runmain.sh ...exit 1"
    exit 1	
fi
#
# checking script argument input
#
echo " WRF-REAL: We will run history case... "
time_input=`grep start_time_main input.sh | cut -d '=' -f 2`
data_year_real=`echo ${time_input} | cut -c1-4`
data_month_real=`echo ${time_input} | cut -c5-6`
data_day_real=`echo ${time_input} | cut -c7-8`
data_hour_real=`echo ${time_input} | cut -c9-10`
data_minute_real=`echo ${time_input} | cut -c11-12`
fsct_length_real=`grep fcst_length input.sh | cut -d '=' -f 2`
runmode=`grep runmode input.sh | cut -d '=' -f 2`
echo $time_input $fsct_length_real $runmode
if [ "$runmode" == "EXP_IDEAL" ]; then
    ln -sf $DIR_WRF_IDEAL/* $DIR_RUN
    rm -f namelist.input*
else
    ln -sf $DIR_WRF_REAL/* $DIR_RUN
    rm -f namelist.input*
fi
echo " WRF-REAL: Checking for the WRF-REAL path now ..." > log.real
echo "   WRF-REAL: Current WRF-REAL dir is: $DIR_RUN" >> log.real
echo "   WRF-REAL: RUNMODE is: $RUNMODE" >> log.real
echo "   WRF-REAL: boundary interval is $interval_bnd" >> log.real
echo "   WRF-REAL: Running year is: $data_year_real" >> log.real
echo "   WRF-REAL: Running month is: $data_month_real" >> log.real
echo "   WRF-REAL: Running day is: $data_day_real" >> log.real
echo "   WRF-REAL: Running hour is: $data_hour_real" >> log.real
echo "   WRF-REAL: Forecast hour is: $fsct_length_real" >> log.real
#
# construct the strings of starting time and end time for integration
#
. ./quartz_cal_time.sh
cal_time ${data_year_real} ${data_month_real} ${data_day_real} ${data_hour_real} ${data_minute_real} 0 ${fsct_length_real} 0
end_time=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00   
start_time=${data_year_real}-${data_month_real}-${data_day_real}_${data_hour_real}:${data_minute_real}:00
echo " WRF-REAL: Starting time is: $start_time"
echo " WRF-REAL: Ending time is: $end_time"
mcphysics=1
lwradiation=1
cu_opt=1
./quartz_nlist_wrf.sh $time_input $fsct_length_real $mcphysics $lwradiation $cu_opt ${runmode_m}
if [ -f ./namelist.input ]; then
    echo " WRF-REAL: namelist.input is properly generated"
else
    echo " WRF-REAL: namelist.input could not be properly generated. Exti 3"
    exit 3
fi
if [ "$runmode" == "EXP_IDEAL" ]; then
    echo " WRF-REAL: do not support EXP_IDEAL in runreal.sh script...exit 0"
    exit 0
    rm -f wrfinput_d01*
    ln -sf input_sounding_bgd ./input_sounding 
    ./ideal.exe > log.ideal
    if [ -d $DIR_ANA/${time_input} ]; then
        rm -rf $DIR_ANA/${time_input}
    fi
    mkdir $DIR_ANA/${time_input} 
    mv wrfinput_* $DIR_ANA/${time_input}/
    cp namelist.input $DIR_ANA/${time_input}/
else
    #
    # checking for the input GFS data to see if the data available
    # Note that this only apply for AVN data, for other data, need 
    # to change the format of the file name: met_em.d01.2005-07-17_00:00:00.nc
    # 
    echo " WRF-REAL: check for the WRF-REAL input data for WRF-REAL code now"
    nfile=$(($fsct_length_real*3600/$interval_bnd + 1))
    ifile=1
    check_file=met_em.d01.${data_year_real}-${data_month_real}-${data_day_real}_${data_hour_real}:${data_minute_real}:00.nc
    while [ "$ifile" -le "$nfile" ]; do								   
        if [ -f $DIR_ANA/${time_input}/$check_file ]; then
            echo "   WRF-REAL: input file: $check_file exists" >> log.real
            ln -sf  $DIR_ANA/${time_input}/$check_file ./
        else
            echo "   WRF-REAL: input file: $check_file does not exists... exit 3"
            exit 3
        fi
        next_time=$(($ifile*$interval_bnd/3600))
        cal_time ${data_year_real} ${data_month_real} ${data_day_real} ${data_hour_real} ${data_minute_real} 0 ${next_time} 0
        check_file=met_em.d01.${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00.nc
        ifile=$(($ifile+1))   
    done
    #
    # runing the WRF-REAL model now
    #
    rm -f wrfinput_d0* wrfbdy_d0*
    ./real.exe >> log.real
    #
    # create a backup dir and clean the dir
    #
    rm met_em* 
    mv wrfinput_* $DIR_ANA/${time_input}/
    mv wrfbdy_*   $DIR_ANA/${time_input}/
    cp namelist.input $DIR_ANA/${time_input}/
    cp namelist.wps $DIR_ANA/${time_input}/
fi
echo " WRF-REAL script finished normally       "
echo " WRF-REAL script finished normally       " > $DIR_RUN/error.status
echo "" 


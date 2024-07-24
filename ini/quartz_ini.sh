#!/bin/bash -l
#SBATCH -N 1
#SBATCH -n 16
#SBATCH -t 01:00:00
#SBATCH -p debug
#SBATCH -J ini_ensemble
#SBATCH -A r00296
#SBATCH --ntasks-per-node=16
#SBATCH --mem=128GB
#module unload xalt
ulimit -s unlimited
#
# [NOTE]:   
#           This shell script is for creating a set of obs data for letkf test  
#
# [HISTORY]:
#         - 26 Mar, 2010: Created by Chanh Kieu
#         - 28 Mar, 2015: Added an improvement that run the wrf model for 1-h
#                         for smoothing of the cold-start option. 
#
# [AUTHOR]:
#           Chanh Kieu, Indiana University (ckieu@indiana.edu)
#
# [COPYRIGHT]: (C) 2015 GNU open source
#             
#===========================================================================================
#
# option for real-time mode or experiment mode
#
set -x
. ./input.sh
time_input=${start_time_main}
data_year_bgd=`echo ${time_input} | cut -c1-4`
data_month_bgd=`echo ${time_input} | cut -c5-6`
data_day_bgd=`echo ${time_input} | cut -c7-8`
data_hour_bgd=`echo ${time_input} | cut -c9-10`
data_minute_bgd=`echo ${time_input} | cut -c11-12`
echo " INI: Running year is: $data_year_bgd"
echo " INI: Running month is: $data_month_bgd"
echo " INI: Running day is: $data_day_bgd"
echo " INI: Running hour is: $data_hour_bgd"
echo " INI: Number of ne is: $ne"
#
# first link all of the wrf input files needed to run the model
#
rm -f ini*.dat bgd_*.dat *~
ln -sf ${DIR_MAN}/model/WRFV3/run/wrf.exe ./
ln -sf ${DIR_MAN}/model/WRFV3/run/*.TBL ./
ln -sf ${DIR_MAN}/model/WRFV3/run/RRTMG* ./
ln -sf ${DIR_MAN}/model/WRFV3/run/*_DATA ./
ln -sf ${DIR_MAN}/model/WRFV3/run/ETA* ./
ln -sf ${DIR_MAN}/model/WRFV3/run/*.tbl ./
ln -sf ${DIR_MAN}/model/WRFV3/run/ozone* ./
#
# create an 1-hour namelist for this special run
#
if [ -f ./wrfinput_d01 ]; then
    mv ./wrfinput_d01 ./wrfinput_d01_orig
else
    echo " INI: ./wrfinput_d01 does not exist...exit 2"
    exit 2
fi
if [ -f ./wrfbdy_d01 ]; then
    echo " INI: ./wrfbdy_d01 exists for running wrf now"
else
    echo " INI: ./wrfbdy_d01 does not exist...exit 3"
    exit 3
fi
#
# checking for the input GFS data to see if the data available
# 
echo " INI: Start looping now ..."
nfile=$ne
ifile=1
check_file="./wrfinput_d01_orig"
bgd_file=${data_year_bgd}-${data_month_bgd}-${data_day_bgd}_${data_hour_bgd}:${data_minute_bgd}:00
while [ "$ifile" -le "$nfile" ]; do								   
   if [ -f $check_file ]; then
      echo " INI: input file: $check_file exists"
      if [ "$ifile" -lt 10 ]; then
         prefix="bgd_d01_00${ifile}"
      elif [ "$ifile" -lt 100 ]; then
         prefix="bgd_d01_0${ifile}"
      elif [ "$ifile" -lt 1000 ]; then
         prefix="bgd_d01_${ifile}"
      else
         echo " TO MANY MEMBER...exit 10"
         exit 10
      fi
      cp -f $check_file ./bgd.dat
      rm -f error.status
      ./ini.exe
      if [ "$SPINUP" == "YES" ] && [ "$OBSMODE" != "CONTROL" ]; then
          mv ./bgd.dat ./wrfinput_d01
          mpirun -n 16 ./wrf.exe
          #srun -n 1 ./wrf.exe
          wrf_file=`ls ./wrfout_d01* | tail -n 1`
          mv ${wrf_file} ${prefix}_${bgd_file}
      else
          mv ./bgd.dat ${prefix}_${bgd_file}
      fi
      mv ini.dat ini${ifile}.dat
   else
      echo "   INI: input file: $check_file does not exists... exit 3"
      exit 3
   fi
   ifile=$(($ifile+1))   
done
rm -f *~ error.status wrfbdy_d0* wrfinput_d0* rsl* namelist.output
rm -f *.TBL ./wrf.exe RRTMG* *_DATA ETA* *.tbl ozone* wrfout_d*
ln -sf ini1.dat ini.dat
check_ini=`ls ./bgd_*`
if [ "${check_ini}" == "" ]; then
    echo "   INI: quartz_ini.sh failed... exit 6"
    exit 6
else
    echo "   INI: quartz_ini.sh done" > ${DIR_RUN}/status_ini_ok
fi
echo "                                     "
echo "  INI script finished normally       "

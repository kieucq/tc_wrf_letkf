#!/bin/bash -l
#SBATCH -N 1
#SBATCH -n 16
#SBATCH -t 01:00:00
#SBATCH -J vinit
#SBATCH -A r00296
#SBATCH -p debug
#SBATCH --ntasks-per-node=16
#SBATCH --mem=128GB
#module unload xalt
ulimit -s unlimited
set -x
. ./input.sh
./quartz_bogus.sh ${start_time_main} > $DIR_RUN/log.bogusv
check_bogus=`grep safely $DIR_RUN/log.bogusv`
if [ "${check_bogus}" == "" ]; then
   echo " VINIT: quartz_bogus.sh failed ... exit 1"
   exit 1
fi
cd $DIR_LETKF
rm -f wrfinput_d0* wrfbdy_d0* ./namelist*
cp  $DIR_MAN/run/input.sh ./
cp  $DIR_MAN/script/quartz_vortda.sh ./
ln -sf $DIR_RUN/wrfinput_d0* ./
ln -sf $DIR_RUN/wrfbdy_d0* ./
if [ "$blend_obs" == "YES" ]; then
    echo " VINIT: option for blending bogus vortex and cimss is on... skip vortex DA"
    echo " VINIT: quartz_bogus.sh finishes " > $DIR_RUN/status_vinit_ok
    exit 0
fi
check_da_flag=`grep da_flag ../namelist.letkf | grep 345`
if [ "$check_da_flag" != ""  ]; then
    cp $DIR_RUN/namelist.letkf ./namelist.temp
    #sed 's/obs_flag     = 1/obs_flag     = 0/' ./namelist.temp > ./namelist.letkf
    #sed -i 's/= 345/= 45/' ./namelist.temp 
    sed 's/nzl          = 3/nzl          = 1/' ./namelist.temp > ./namelist.letkf
    cp ./namelist.letkf ./namelist.letkf.bogus
    check_obs_flag=`grep obs_flag ./namelist.letkf | awk '{if($3==0) print $3}`
    if [ "${check_obs_flag}" == "" ]; then
        echo " VINIT: obs_flag in namelist.letkf must be 0 for bogus vortex without blending... QUIT 1"
        #exit 1
    fi 
else
    cp $DIR_RUN/namelist.letkf ./namelist.letkf
fi
rm -rf $DIR_LETKF/letkf.dat
./quartz_vortda.sh ${start_time_main} ${ne} >> $DIR_RUN/log.vortda
if [ -f $DIR_LETKF/letkf.dat ]; then
    mv letkf.dat letkf_bogusv.dat
    echo " VINIT: quartz_vortda.sh finishes safely" > $DIR_RUN/status_vinit_ok
else
    echo " VINIT: quartz_vortda.sh step failed... exit 1"
    exit 1
fi

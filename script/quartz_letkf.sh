#!/bin/bash
#SBATCH -N 1
#SBATCH -n 16
#SBATCH -t 01:00:00
#SBATCH -J letkf
#SBATCH -p debug
#SBATCH -A r00296
#SBATCH --ntasks-per-node=16
#SBATCH --mem=128GB
#module unload xalt
ulimit -s unlimited
#
# [NOTE]:   
#           This shell script is for running letkf either directly or via 
#           scheduler system. 
#
# [HISTORY]:
#         - 27 Mar, 2010: Created
#         - 09 Apr, 2010: update the runmode to read from environment for better
#                         communication with the runmain.sh
#         - 03 Jan, 2014: added the vort_init option to correct the bug in linking
#                         the background field when bogus vortex OSSE profiles are
#                         used during the bogus vortex assimilation process
#         - 20 Mar, 2015: modified for revised job flows
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
if [ "$runmode" == "" ]; then
 echo " LEKTF: runmode is empty, check runmain.sh ...exit 1"
 exit 1
fi
if [ "$DIR_RUN" == "" ]; then
 echo " LEKTF: letkf running dir is empty, check runmain.sh ...exit 1"
 exit 1
fi
if [ "$vort_init" == "" ]; then
 echo " LEKTF: vort_init option is empty, check runmain.sh ...exit 1"
 exit 1
fi
time_input=$start_time_main
data_year_let=`echo ${time_input} | cut -c1-4`
data_month_let=`echo ${time_input} | cut -c5-6`
data_day_let=`echo ${time_input} | cut -c7-8`
data_hour_let=`echo ${time_input} | cut -c9-10`
data_minute_let=`echo ${time_input} | cut -c11-12`
echo " LETKF: Running year is: $data_year_let"
echo " LETKF: Running month is: $data_month_let"
echo " LETKF: Running day is: $data_day_let"
echo " LETKF: Running hour is: $data_hour_let"
echo " LETKF: Number of ne is: $ne"
#
# checking for the observational input data to see if the data available
# 
echo " LETKF: Start looping now ..."
nfile=$ne
ifile=1
obs_file=obs_${data_year_let}-${data_month_let}-${data_day_let}_${data_hour_let}:${data_minute_let}:00
bgd_file=${data_year_let}-${data_month_let}-${data_day_let}_${data_hour_let}:${data_minute_let}:00
vox_file=vox_${data_year_let}-${data_month_let}-${data_day_let}_${data_hour_let}:${data_minute_let}:00
time_string=${data_year_let}${data_month_let}${data_day_let}${data_hour_let}${data_minute_let}
rm -f bgd_*.dat *~ obs.dat 
if [ -f $DIR_OBS/$obs_file ] && [ "$OBSMODE" != "CONTROL" ]; then
 echo " LETKF: $DIR_OBS/$obs_file exists... linking"
 no1=`head -n 1 $DIR_OBS/$obs_file | awk '{print $1}'`
 if [ ${no1} == "0" ]; then
     echo " LETKF: $DIR_OBS/$obs_file exists but there is obs data to assimilate. Switch to control now"
     blend_obs="NO"
     OBSMODE="CONTROL"
     echo "CONROL" > $DIR_MAN/run/no_da_flag.txt
 else
     rm -f $DIR_MAN/run/no_da_flag.txt
 fi
 if [ "$blend_obs" == "NO" ]; then
     ln -sf $DIR_OBS/$obs_file ./obs.dat
 elif [ "$blend_obs" == "YES" ]; then
     no1=`head -n 1 $DIR_OBS/$obs_file | awk '{print $1}'`
     no2=`head -n 1 $DIR_MAN/vortex_init/$vox_file | awk '{print $1}'`
     if [ ${no1} == "" ] || [ ${no2} == "" ]; then
         echo "LETKF: Something wrong with obs or bogus vortex step... exit 1"
         echo "LETKF: no1 = $no1"
         echo "LETKF: no2 = $no2" 
         exit 1
     fi
     no_blend=`echo $no1 + $no2 | bc -l`
     echo "${no_blend} lon | lat | lev | u qcu | v qcv | t qct | qv qcqv | ph qcph |" > ./obs.dat
     sed '/lat/d' $DIR_OBS/$obs_file >> ./obs.dat
     sed '/lat/d' $DIR_MAN/vortex_init/$vox_file >> ./obs.dat 
 else
     echo " LETKF: blend_obs has to be YES or NO... exit 1"
     exit 1
 fi
else
 if [ "$OBSMODE" != "CONTROL" ]; then
     echo " LETKF: $DIR_OBS/$obs_file does not exist... exit"
     exit 1
 fi
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
 if [ "$vort_init" == "YES" ] && [ "$blend_obs" == "NO" ]; then
  check_file="$DIR_BGD/${time_string}/bogus/bgd_d01_${prefix}_${bgd_file}"      	
 else
  check_file="$DIR_BGD/${time_string}/bgd_d01_${prefix}_${bgd_file}"
 fi
 if [ -f $check_file ]; then
  echo " LETKF: input file: $check_file exists"
  ln -sf $check_file ./bgd_${prefix}.dat
  if [ "$runmode" == "EXP_IDEAL" ] || [ "$runmode" == "EXP_REAN" ]; then
      cp -f bgd_${prefix}.dat ana_${prefix}.dat
      if [ "$ifile" -eq "$nfile" ]; then
          ./letkf_restart.exe >> $DIR_RUN/log.letkf
      fi
  elif [ "$OBSMODE" == "CONTROL" ]; then
      cp -f bgd_${prefix}.dat ana_${prefix}.dat 
      echo " LETKF: just copy bgd to ana in CONTROL mode" >> ./error.status
  else
      if [ -f ./ana_${prefix}.dat ]; then
          echo " LETKF: ./ana_${prefix}.dat exits... move on next member"
      else
          echo " LETKF: ./ana_${prefix}.dat do not exit... cp from wrfinput_d01"
          cp -f wrfinput_d01 ana_${prefix}.dat
      fi
  fi  
 else
  echo "   LETKF: input file: $check_file does not exists... exit 3"
  exit 3
 fi
 ifile=$(($ifile+1))   
done
if [ "$OBSMODE" != "CONTROL" ]; then
    rm -f *~ error.status
    mpirun -n 4 ./letkf_mpi.exe >> $DIR_RUN/log.letkf
    #srun -n 1 ./letkf_mpi.exe >> $DIR_RUN/log.letkf
    if [ -f error.status ]; then
        echo " LETKF: letkf_mpi.exe ran smoothly..."
        mv letkf.dat letkf_${time_string}.dat
    else
        echo " LETKF: letkf.exe has abrupt die..."
        exit 3
    fi
fi
#
# move all of new analysis to a new dir
#
ifile=1
while [ "$ifile" -le "$nfile" ]; do								  
    if [ "$ifile" -lt 10 ]; then
       prefix="00${ifile}"
    elif [ "$ifile" -lt 100 ]; then
       prefix="0${ifile}"
    elif [ "$ifile" -lt 1000 ]; then
       prefix="${ifile}"
    else
       echo " TO MANY MEMBER...exit 2"
       exit 2
    fi
    if [ -d $DIR_ANA/${time_string}/mem_${prefix} ]; then
       rm -rf $DIR_ANA/${time_string}/mem_${prefix}
    fi
    mkdir $DIR_ANA/${time_string}/mem_${prefix}
    if [ "$runmode" == "EXP_IDEAL" ]; then
       mv ana_${prefix}.dat $DIR_ANA/${time_string}/mem_${prefix}/wrfinput_d01
    else	 
       mv ana_${prefix}.dat $DIR_ANA/${time_string}/mem_${prefix}/wrfinput_d01
       cp wrfbdy_d01 $DIR_ANA/${time_string}/mem_${prefix}/wrfbdy_d01
    fi 
    ifile=$(($ifile+1))   
done
if [ -f ./error.status ]; then
    mv ./error.status ${DIR_RUN}/status_letkf_ok
    echo " LETKF script finished safely " >> ${DIR_RUN}/status_letkf_ok
else
    echo " MAIN: karst_letkf.sh has abrupt die...exit 3"
    exit 3
fi
rm -f bgd_*.dat wrfinput_d0* wrfbdy_d0* 
echo " LETKF script finished safely     "

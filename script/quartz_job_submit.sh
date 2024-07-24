#!/bin/bash
#
# NOTE: This is to submit a SGE job. It requires an input of 
#       maximum number of job cards an user can submit at one
#       time, user name, and running dir
#
# AUTHOR: Chanh Kieu
#
njob_max=$1
DIR_RUN=$2
if  [ "$njob_max" -lt 1 ] || [ "$DIR_RUN" == "" ]; then
    echo "quartz_job_submit_pbs.sh script cannot be run with njob_max = $njob_max"
    exit 1
fi
#njob=`squeue -u ckieu | grep wrflet| awk '{if($5!="C") print $0}' | wc -l`
njob=`squeue -u $USER | grep wrflet| wc -l`
echo " MAIN: number of job pending/running is: $njob"
if [ "$njob" == "" ] || [ "$njob"  -lt "$njob_max" ]; then
     sbatch  wrf_qsub.sh >> $DIR_RUN/log.qsub
else
     echo " MAIN: $njob_max jobs running. Wait one to end before submitting new one"
     until [ "$njob" -lt "$njob_max" ]; do
         njob=`qstat | grep $USER | awk '{if($5!="C") print $0}' | wc -l`
         sleep 5m
     done
     sbatch  wrf_qsub.sh >> $DIR_RUN/log.qsub
fi


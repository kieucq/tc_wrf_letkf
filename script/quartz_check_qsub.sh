#!/bin/sh
#
# [NOTE]:   
#           This shell script is for monitoring the qsub jobs
#
# [RUN]:
#  	    To run the script, use the following syntax
#           sh ./quartz_check_qsub.sh file_to_be_checked
#
# [HISTORY]:
#         - Mar 20, 2015: created by CK
#
# [AUTHOR]:
#           Chanh Kieu, Indiana University (ckieu@indiana.edu)
#
# [COPYRIGHT]: (C) 2015 GNU open source
#             
#===========================================================================================
# 
# setting up path environment
#
set -x
if [ $# == 1 ]; then
    check_file=$1
else
    echo " QSUB: Please use the following syntax "
    echo " sh ./quartz_check_qsub.sh file_to_be_checked "
fi		 
echo "   QSUB: file to be check is: $check_file"
check_flag="NO"
icount=1
until [ "${check_flag}" == "YES"  ];
do
    echo "QSUB: $check_file is not existing...sleep 5"
    if [ -f $check_file ]; then
         echo "QSUB: $check_file exists"
         check_flag="YES"
    else
         icount=$(($icount+1))
         sleep 5
    fi 
    if [ $icount -gt 21600  ]; then
         echo "QSUB: job is on queue for > 30h...exit"
         exit 1
    fi
done

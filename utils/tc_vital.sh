#
# NOTE: this script is for checking and filtering the TC vital record, which
#       will be used for setting up a nested domain when a storm warning is
#       issued. The program will use some obs information to find the lat/lon
#       for the lower left corner of the nested domain  
#
# HIST: - Jun 18, 2011: created by CK
#
# AUTHOR: Chanh Kieu (chanhkq@vnu.edu.vn), Vietnam National University
#============================================================================
#!/bin/sh
#
# read input time string first for cross check
#
if [ $# == 1 ]; then
  tc_time_input=$1
else
  echo " TC: have to input a time when TC vital is ready...exiting"
  echo " TC: format is ./tcvital.sh YYYYMMDDHHMM" 
  echo " TC: exiting 0"
  exit 0
fi 
echo " TC: time to start TC domain is $tc_time_input"
#
# check if the vital record file is existing and read the vital record
#
if [ -f ./vital_record.txt ]; then
 size=`ls -l vital_record.txt | awk '{print $5}'`
 if [ "$size" == 0 ]; then
  echo " TC: the vital_record.txt file is empty...exit 1"
  exit 1
 else
  echo " TC: check for vital_record passed"
 fi
else
 echo " TC: the vital_record.txt file does not exit...exit 2"
 exit 2
fi
#
# extract information of the TC center from the vital 
#
iline=0
while read line
do
 epath="$line"
 iline=$(($iline+1))
 if [ $iline == "2" ]; then
  echo "   TC: the vital record at line $iline: $line"
#  vital_time_s=`expr index "$line" \"`
#  echo "   TC: start of vital time is: $vital_time_s"
#  string1=${line#\"}
#  echo "   TC: string1 is: $string1"
#  vital_time=${string1%%\"}
   vital_time=`echo $line| cut -d '"' -f 2`
  echo "   TC: vital time is: $vital_time"
  if [ "$vital_time" -ne "$tc_time_input" ]; then
   echo "   TC: input parameter does not fit vital_time...exit 3"
   exit 3
  else
   echo "   TC: tc input parameter passed the check with vital time" 
  fi
 elif [ $iline == "3" ]; then
  echo "   TC: the vital record at line $iline: $line"
  tc_lat_input=`echo $line| cut -d '"' -f 2`
  echo "   TC: lat of domain center is: $tc_lat_input"
 elif [ $iline == "4" ]; then
  echo "   TC: the vital record at line $iline: $line"
  tc_lon_input=`echo $line| cut -d '"' -f 2`
  echo "   TC: lon of domain center is: $tc_lon_input"
 fi
done < vital_record.txt
#
# set the domain confguration for outer and inner domains
#
maximum_dom=2
MOM_CEN_LAT=16
MOM_CEN_LON=115
DOM1_NX=155
DOM1_NY=155
DOM1_DX=36000
DOM1_DY=36000
DOM2_DX=12000
DOM2_DY=12000
DOM2_NX=121
DOM2_NY=121
DOM3_DX=4000
DOM3_DY=4000
DOM3_NX=100
DOM3_NY=100
LLI1=$(($DOM1_NX/2-$DOM2_NX/6))
LLJ1=$(($DOM1_NY/2-$DOM2_NY/2))
LLI2=$(($DOM2_NX/2-$DOM3_NX/6))
LLJ2=$(($DOM2_NY/2-$DOM3_NY/2))
tem=`echo "($tc_lon_input - $MOM_CEN_LON)" | bc -l`
TX1=`echo "(($tem * 111000) / $DOM1_DX)" | bc -l`
tem=`echo "($tc_lat_input - $MOM_CEN_LAT)" | bc -l`
TY1=`echo "(($tem * 111000) / $DOM1_DY)" | bc -l`
RX1=`printf "%.0f\n" $TX1`
LLI=`expr $LLI1 + $RX1`
RY1=`printf "%.0f\n" $TY1`
LLJ=`expr $LLJ1 + $RY1`
echo " TC: LLI1 = $LLI1"
echo " TC: LLJ1 = $LLJ1"
echo " TC: TX1  = $TX1"
echo " TC: TY1  = $TY1"
echo " TC: RX1  = $RX1"
echo " TC: RY1  = $RY1"
echo " TC: LLI  = $LLI"
echo " TC: LLJ = $LLJ"
if [ ${LLI} -lt "5" ] || [ ${LLI} -gt "153" ]; then
 echo " TC: the nested domain is not within the mother domain"
 echo " TC: reset the outer domain inside the TY dir...exit 4"
 exit 4
fi
if [ ${LLJ} -lt "5" ] || [ ${LLJ} -gt "153" ]; then 
 echo " TC: the nested domain is not within the mother domain" 
 echo " TC: reset the outer domain inside the TY dir...exit 4"
 exit 4
fi
#
# broad cast the domain configuration for a new run
#
source env.txt
export MOM_CEN_LAT=${MOM_CEN_LAT}
export MOM_CEN_LAT=${MOM_CEN_LON}
export DOM1_NX=${DOM1_NX}
export DOM1_NY=${DOM1_NY}
export DOM1_DX=${DOM1_DX}
export DOM1_DY=${DOM1_DY}
export DOM2_NX=${DOM2_NX}
export DOM2_NY=${DOM2_NY}
export DOM2_DX=${DOM2_DX}
export DOM2_DY=${DOM2_DY}
export DOM2_LLI=${LLI}
export DOM2_LLJ=${LLJ}
export DOM3_NX=${DOM3_NX}
export DOM3_NY=${DOM3_NY}
export DOM3_DX=${DOM3_DX}
export DOM3_DY=${DOM3_DY}
export DOM3_LLI=${LLI2}
export DOM3_LLJ=${LLJ2}
echo " TC: done setting up environment"






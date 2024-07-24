#!/bin/bash 
# 
# [NOTE]
#       This script is for creating a Grads output of a time series 
#       and track. The work is under the IMSG contract. So copyright
#       is implemented for any use.
#
# [INPUT]
#       A file that indicate the path to ensemble data named 
#       ensemble_dir.txt and an input file wrf2grads_mean_in.txt
#       contains several pieces of information for global mean (for
#       option -dgmean in Makefile).
#
# [OUPUT]
#       Ensemble of text files that contain time,lon,lat and a Grads
#       data and ctl file to plot time series
#
# [AUTHOR]
#       Chanh Q. Kieu, Vietnam National University (chanhkq@vnu.edu.vn)
#
# [HISTORY]
#     - Aug 14, 2009: Created
#     - Feb 08, 2011: Add an internal namelist for wrf2grads 
#
# [COPYRIGHT] (C) 2011 copyrighted by the I.M. System Group, Inc, USA     
#
#===================================================================
#
# set up first some prefix information and a control file for wrf2grads
#
#$ -V                     # Inherit the submission environment
#$ -cwd                   # Start job in  submission directory
#$ -N log_at
#$ -j y
#$ -A TG-ATM090042
#$ -pe 1way 16  
#$ -q serial              # Queue name
#$ -l h_rt=12:00:00       # Run time (hh:mm:ss) - 1.5 hours
export MY_NSLOTS=1
prename="dolly"                # prename for the case we're workin on
ninterval=3                    # output interval of wrfout
working_dom="d03"              # working domain
rm -f wrfout_*
#
# create a namelist for wrf_to_grads
#
if [ -f ensemble_dir.txt  ]; then
 while read line
 do
  epath="$line"
  ln -sf $epath/wrfout_${working_dom}* ./
 done < ensemble_dir.txt
else
 echo "Input file: ensemble_dir.txt does not exit"
 exit 0
fi
ls wrfout_${working_dom}* > log
cat > wrf2grads_mean_in.txt << EOF2
k_shear1 = 2  ! height to search for lower shear
k_shear2 = 10 ! height to search for upper shear
k_w      = 10 ! height to search for max w
k_rh     = 10 ! height to search for mid level rh
k_area   = 10 ! area to search for rmw
k_rmw    = 2  ! height to search for maximum surface wind
EOF2
cat > control_file_${prename} << EOF
-1                  ! negative means ignore the times
2007-07-16_00:00:00
end_of_time_list
U                   ! U Compoment of wind
V                   ! V Component of wind
 THETA               ! Theta
 Z                   ! Height (m)
RH                  ! Relative Humidity (diagnostic)
end_of_3dvar_list
slvl                ! sea level pressure
RAINC               ! ACCUMULATED TOTAL CUMULUS PRECIPITATION
RAINCV              ! TIME-STEP CUMULUS PRECIPITATION
RAINNC              ! ACCUMULATED TOTAL GRID SCALE PRECIPITATION
T2                  ! TEMP at 2 M
U10                 ! U at 10 M
V10                 ! V at 10 M
end_of_2dvar_list
EOF
n=1
while read line
do
 echo "$line" >> control_file_${prename}
 n=$(($n+1))
done < log
cat >> control_file_${prename} << EOF1
end_of_file_list
                    ! required blank
real                ! real (input/output)/ideal/static
1                   ! 0=no map bgd, 1=map bgd 
1                   ! specify grads vertical grid  
                    !
                    !
                    !
1000
800
600
400
200
100
EOF1
#
# first count the number of ensemble member
#
ne=1
while read line
do
 epath="$line"
 echo "Working with member number $ne as: $epath"
 ln -sf $epath/wrfout_${working_dom}* ./
 ./wrf_to_grads control_file_$prename $prename$ne
 if [ "$ne" -lt 10 ]; then
   imember="_00$ne"
 elif [ "$ne" -lt 100 ]; then
   imember="_0$ne"
 elif [ "$ne" -lt 1000 ]; then
   imember="_$ne"
 else
   echo " TO MANY MEMBER...exit 1"
   exit 1
 fi
 bfile="ensemble_tseries$imember.txt"
 mv wrf2grads_evolution.txt $bfile
 mv wrf2grads_track.txt track_$ne.txt
 ne=$(($ne+1))
done < ensemble_dir.txt
ne=$(($ne-1))
rm -f wrfout_*
echo "Number of ensemble is $ne"
#
# creating a master ctl file for ensemble 3d data
#
n=1
nlevel=1000
while read line
do
 if [ "$n" -eq 1 ]; then
  echo "dset ^$prename%e.dat" > ensemble_$prename.ctl
  echo "options template" >> ensemble_$prename.ctl
 elif [ "$n" -eq 2 ]; then
  echo "$line" >> ensemble_$prename.ctl 
 elif [ "$n" -gt 2 ] && [ "$n" -lt 5 ]; then
  echo "$line" >> ensemble_$prename.ctl 
 elif [ "$n" -eq 5 ]; then
  echo "$line" >> ensemble_$prename.ctl
  nlevel=`echo $line | tail -n 1 | awk '{print $2}'`
  nlevel1=$(($nlevel+5))
  nlevel2=$(($nlevel1+1))
 elif [ "$n" -gt 5 ] && [ "$n" -le "$nlevel1" ]; then
  echo "$line" >> ensemble_$prename.ctl
 elif [ "$n" -eq "$nlevel2" ]; then
  echo "$line" >> ensemble_$prename.ctl
  nt=`echo $line | tail -n 1 | awk '{print $2}'`
  echo "EDEF  $ne names" >> ensemble_$prename.ctl
  k=1
  while [ "$k" -le "$ne" ]
  do
   echo "$k" >> ensemble_$prename.ctl
   k=$(($k+1))
  done
 else
  echo "$line" >> ensemble_$prename.ctl
 fi 
 n=$(($n+1))
done < "$prename"1.ctl
echo "nt=$nt; nlevel=$nlevel; ne=$ne"
#
# run post process to output time series
#
echo $ne > temp.txt
echo $nt >> temp.txt
./ensemble_tseries_at.exe < temp.txt
rm temp.txt
echo "DSET ^ensemble_tseries.dat" > ensemble_tseries.ctl
echo "TITLE $prename Simulation" >> ensemble_tseries.ctl 
echo "UNDEF -99999" >> ensemble_tseries.ctl
echo "XDEF $nt LINEAR 0 $ninterval" >> ensemble_tseries.ctl
echo "YDEF $(($ne+1))  LINEAR 1 1" >> ensemble_tseries.ctl
echo "ZDEF 1  LINEAR 0 1" >> ensemble_tseries.ctl
echo "TDEF 216 LINEAR 00Z17JUL2005 1hr" >> ensemble_tseries.ctl
echo "VARS    3" >> ensemble_tseries.ctl
echo "pmin   0    99 3D field" >> ensemble_tseries.ctl
echo "vmax   0    99 3D field" >> ensemble_tseries.ctl
echo "rmw    0    99 3D field" >> ensemble_tseries.ctl
echo "ENDVARS" >> ensemble_tseries.ctl
echo "Done time series Grads output"
#
# create an ensemble of track input for track scripting
#
ie=1
marktype="0"
size="0.1"
color=1
style="1"
cthick="9"
start_hour="0"
while [ "$ie" -le "$ne" ]
do
 ifile="track_$ie.txt"
 n=1 
 while read line
 do
  if [ "$n" -eq 1 ]; then
   echo "ensemble $ie" > track_mem_$ie.txt
   echo "$marktype $size" >> track_mem_$ie.txt
   echo "$color $style $cthick" >> track_mem_$ie.txt
   echo "$start_hour $ninterval" >> track_mem_$ie.txt
   echo "$line" >> track_mem_$ie.txt
  else
   echo "$line" >> track_mem_$ie.txt
  fi
  n=$(($n+1))
 done < $ifile
 rm $ifile
 ie=$(($ie+1))
 color=$(($color+1))
done
#
# mv all the output to a separate folder
#
mkdir -p ${working_dom}
mv ensemble_tseries_*.txt ./${working_dom}/
mv ensemble_tseries.ctl ./${working_dom}/
mv ensemble_tseries.dat ./${working_dom}/
mv ${prename}*.* ./${working_dom}/
mv track_mem*.txt ./${working_dom}/
mv ensemble_${prename}.ctl ./${working_dom}/ 
cp *.gs ./${working_dom}/
cp tc_message.txt ./${working_dom}/tc_message_${prename}.txt

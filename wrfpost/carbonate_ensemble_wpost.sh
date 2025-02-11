#!/bin/bash 
# 
# [NOTE]
#       This script is for creating Grads outputs of a time series 
#       and track, given an ensemble of forecast from an ensemble
#       system. 
#
# [INPUT]
#       1. tc_input_dir.txt: file containing paths to ensemble data; 
#       2. bdeck input pointed by $bdeck_path 
#
# [OUPUT]
#       Ensemble of text files that contain time,lon,lat and a Grads
#       data and ctl file to plot time series
#
# [HOW TO RUN]
#       Simply edit the namelist below and run:
#       ./tc_ensemble_wpost.sh storm domain basin(WP/AL) runmode
#       All outputs will be in a dir name ${working dom}
#
# [AUTHOR]
#       Chanh Q. Kieu, Vietnam National University (chanhkq@vnu.edu.vn)
#
# [HISTORY]
#     - Aug 14, 2009: Created
#     - Feb 08, 2011: Add an internal namelist for wrf2grads 
#     - Jan 26, 2012: updated adeck/bdeck option and automate all the 
#                     plotting routines
#     - Mar 25, 2012: revised to support further ensemble mean adeck
#                     for later verification.
#     - Apr 02, 2012: update to generalize the post to use of the bdeck 
#                     input in the ATCF format instead of hand-fixed.
#
# [COPYRIGHT] (C) 2011 copyrighted by the I.M. System Group, Inc, USA     
#
#===================================================================
#
# Namelist to setup some info and control file for wrf2grads
#
set -x
bdeck_path="/N/u/ckieu/Carbonate/tc_wrf_letkf/wrfpost/bdeck"
if [ -d ${bdeck_path} ]; then
    echo " POST: $bdeck_path exits"
else
    echo " POST: $bdeck_path does not exist...exit 1"
    exit 1
fi
if [ $# == 5 ];  then
    prename=$1                       # name of storm we're workin on
    working_dom=$2                   # working domain
    basin=$3                         # working basin (WP/AL)
    runmode=$4                       # rean/cimss/rads/rada ....
    cycle=$5                         # cycle to post process
else
    echo "Incorrect input parameters. Please run as:"
    echo "./tc_ensemble_wpost.sh storm domain basin(WP/AL) runmode $cycle"
    echo "e.g."
    echo "./tc_ensemble_wpost.sh Conson d01 WP rean 2010101418"
    exit 1
fi
if [ "${#cycle}" -ne "10" ]; then
    echo "wrong cycle format. 10 digits YYYYMMDDHH  only" 
    exit 1
fi
storm_name=`echo $prename | tr '[a-z]' '[A-Z]'`
storm_year=`echo $cycle | cut -c 1-4` 
storm_hour=`echo $cycle | cut -c 9-10`
bdeck=`grep $storm_name ${bdeck_path}/b*.dat | grep $basin | grep  \
       $storm_year | cut -d ':' -f 1 | head -n 1`
ninterval=6                       # output interval of wrfout
infile="tc_input_dir.txt"         # input list of data to process
storm_following="1"               # opt for TC center following (1/0)
marktype="0"                      # default mark type for Grads plots
size="0.1"                        # default mark size for Grads plots
color=2                           # default color for Grads plots
style="1"                         # default style for Grads plots
cthick="7"                        # default thickness for Grads plots 
start_hour="0"                    # default starting time for track plot
bw_option="Y"                     # option for plotting in black/white
if [ "$bdeck" == "" ]; then
    echo "bdeck file is not existent... exit 1"
    exit 1
else
    echo "Found 1 bdeck file: $bdeck"
fi
#
# create an observed track file from track_mem_(ne+1).txt. Should
# implement further for bdeck file here. fort.10 is what we need
# for better tracking algorithm in the analysis mode.
#
if [ "$ninterval" != 6 ]; then
    echo "ERROR: The post-process is tailored to 6-h interval by bdeck!"
    echo "ERROR: by passing of this option leads to wrong time series... exit"
    exit 1
fi
if [ "$working_dom" == "d03" ]; then
    echo "Searching for storm center with moving opt is disabled with d03"
    storm_following="0"
fi
if [ -f ${bdeck} ]; then
#
# echo "{if($3=="2010101500,") print NR})" > in.awk
# awk -f in.awk bwp152010.txt | head -n 1
# x=2010101500
# awk '{if($3=="'$x',") print NR}' bwp152010.txt | head -n 1
#
    ista=`cat -n ${bdeck} | grep ${cycle} | awk '{print $1}' | head -n 1`
    ieof=`cat -n ${bdeck} | awk '{print $1}' | tail -n 1`
    iend=`echo "$ista + 62" | bc -l`
    if [[ $ieof -lt $iend  ]]; then
        iend=$ieof
    fi
    sed -n ${ista},${iend}p ${bdeck} | awk '{if($12!="50," && $12!="64,")         \
      print $0}' | awk '{print $8 " " $7}' | sed 's/,//g' | sed 's/N/ N/g' |   \
      sed 's/S/ S/g' | sed 's/W/ W/g' | sed 's/E/ E/g' > fort.21              
    awk '{if($2=="E" && $4=="N") print $1/10" "$3/10;                             \
      else if($2=="W" && $4=="N") print -$1/10" "$3/10;                        \
      else if($2=="W" && $4=="S") print -$1/10" "-$3/10;                       \
      else if($2=="E" && $4=="S") print $1/10" "-$3/10}' fort.21 > fort.10
    sed -n ${ista},${iend}p ${bdeck} | awk '{if($12!="50," && $12!="64,")         \
      print $0}' | awk '{print $9 " " $10}' | sed 's/,//g'  > fort.12 
    storm_basin=`awk '{print $1}' ${bdeck} | head -n 1 | sed 's/,//g'`
    storm_id=`awk '{print $2}' ${bdeck} | head -n 1 | sed 's/,//g'`
else
    echo "Input observed track file $bdeck does not exit"
    exit 1
fi
rm -f wrfout_*
mkdir -p ${working_dom}
#
# create a namelist for wrf_to_grads and trim out all other 3-h interval
# if existing since the post only work with 6-h output to fit with ATCF format
#
if [ -f ${infile} ]; then
    while read line
    do
        epath="$line"
        ln -sf $epath/wrfout_${working_dom}* ./
    done < ${infile}
else
    echo "Input file: ./${infile} does not exit"
    exit 1
fi
if   [ "$storm_hour" == "12" ] || [ "$storm_hour" == "06" ] || \
     [ "$storm_hour" == "00" ] || [ "$storm_hour" == "18" ]; then
    rm -f wrfout*03:00:00 wrfout*09:00:00 wrfout*15:00:00 wrfout*21:00:00
elif [ "$storm_hour" == "03" ] || [ "$storm_hour" == "09" ] || \
     [ "$storm_hour" == "15" ] || [ "$storm_hour" == "21" ]; then
    rm -f wrfout*00:00:00 wrfout*06:00:00 wrfout*12:00:00 wrfout*18:00:00
fi
check_wrfout=`ls wrfout_${working_dom}* | wc -l`
if [ "$check_wrfout" -le 1 ]; then
    echo "There is no wrfout_${working_dom}. Check the input data dir...exit 1"
    exit 1
elif [ "$check_wrfout" -gt 22 ]; then
    echo "There is so many wrfout_${working_dom}. Work with 6-h output only...exit 1"
    exit 1
fi
ls wrfout_${working_dom}* > fort.13
cat > wrf2grads_mean_in.txt << EOF2
k_shear1   = 2  ! height to search for lower shear
k_shear2   = 10 ! height to search for upper shear
k_w        = 10 ! height to search for max w
k_rh       = 10 ! height to search for mid level rh
k_area     = 10 ! area to search for rmw
k_rmw      = 2  ! height to search for maximum surface wind
storm_move = ${storm_following}  ! 0- no move; 1-following storm (need fort.10)
tc_mv_time = 1  ! step to update TC search center
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
done < fort.13
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
#
# first count the number of ensemble member
#
ne=1
while read line
do
 epath="$line"
 echo "Working with member number $ne as: $epath"
 ln -sf $epath/wrfout_${working_dom}* ./
 if   [ "$storm_hour" == "12" ] || [ "$storm_hour" == "06" ] || \
      [ "$storm_hour" == "00" ] || [ "$storm_hour" == "18" ]; then
  rm -f wrfout*03:00:00 wrfout*09:00:00 wrfout*15:00:00 wrfout*21:00:00
 elif [ "$storm_hour" == "03" ] || [ "$storm_hour" == "09" ] || \
      [ "$storm_hour" == "15" ] || [ "$storm_hour" == "21" ]; then
  rm -f wrfout*00:00:00 wrfout*06:00:00 wrfout*12:00:00 wrfout*18:00:00
 fi
 ./wrf_to_grads control_file_$prename $prename$ne
 if [ "$ne" -lt 10 ]; then
   imember="00$ne"
 elif [ "$ne" -lt 100 ]; then
   imember="0$ne"
 elif [ "$ne" -lt 1000 ]; then
   imember="$ne"
 else
   echo " TO MANY MEMBER...exit 1"
   exit 1
 fi
 bfile="ensemble_tseries_$imember.txt"
 mv wrf2grads_evolution.txt $bfile
 mv wrf2grads_track.txt track_$ne.txt
 sed -i s/"BB,"/${storm_basin},/ wrf2grads_atcf.txt
 sed -i s/"NN,"/${storm_id},/ wrf2grads_atcf.txt
 sed -i s/VWRF/V${imember}/ wrf2grads_atcf.txt
 mv wrf2grads_atcf.txt ./${working_dom}/atcf_mem_$ne.dat
 ne=$(($ne+1))
done < ${infile}
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
  echo "dset ^$prename%e.dat" > ensemble_all.ctl
  echo "options template" >> ensemble_all.ctl
 elif [ "$n" -eq 2 ]; then
  echo "$line" >> ensemble_all.ctl 
 elif [ "$n" -gt 2 ] && [ "$n" -lt 5 ]; then
  echo "$line" >> ensemble_all.ctl 
 elif [ "$n" -eq 5 ]; then
  echo "$line" >> ensemble_all.ctl
  nlevel=`echo $line | tail -n 1 | awk '{print $2}'`
  nlevel1=$(($nlevel+5))
  nlevel2=$(($nlevel1+1))
 elif [ "$n" -gt 5 ] && [ "$n" -le "$nlevel1" ]; then
  echo "$line" >> ensemble_all.ctl
 elif [ "$n" -eq "$nlevel2" ]; then
  echo "$line" >> ensemble_all.ctl
  nt=`echo $line | tail -n 1 | awk '{print $2}'`
  echo "EDEF  $ne names" >> ensemble_all.ctl
  k=1
  while [ "$k" -le "$ne" ]
  do
   echo "$k" >> ensemble_all.ctl
   k=$(($k+1))
  done
 else
  echo "$line" >> ensemble_all.ctl
 fi 
 n=$(($n+1))
done < "$prename"1.ctl
echo "nt=$nt; nlevel=$nlevel; ne=$ne"
#
# create an ensemble of track input for track scripting
#
ie=1
while [ "$ie" -le "$ne" ]
do
 ifile="track_$ie.txt"
 n=1 
 while read line
 do
  if [ "$bw_option" == "Y" ]; then
   color=9
  fi
  if [ "$n" -eq 1 ]; then
   echo "ensemble $ie" > track_mem_$ie.txt
   echo "$marktype $size" >> track_mem_$ie.txt
   echo "$color $style $cthick" >> track_mem_$ie.txt
   echo "$start_hour $ninterval" >> track_mem_$ie.txt
   echo "$line" | awk '{print $1" "$2" "$3-0.0}' >> track_mem_$ie.txt
  else
   echo "$line" | awk '{print $1" "$2" "$3-0.0}' >> track_mem_$ie.txt
  fi
  n=$(($n+1))
 done < $ifile
 rm $ifile
 ie=$(($ie+1))
 color=$(($color+1))
 if [ "$color" -gt "16" ]; then
  color=2
 fi
done
#
# create an ensemble mean track file
#
ie=$(($ne+1))
out_file=track_mem_${ie}.txt
iline=1
while read tline
do
 echo "working at line $iline"
 if [ "$iline" -le "4" ]; then
  echo "Ensemble mean of $prename" > ${out_file}
  echo "1 $size" >> ${out_file}
  echo "1 1 18" >> ${out_file}
  echo "$start_hour $ninterval" >> ${out_file}
 else
  nsum=0
  lat_out=0
  lon_out=0
  ifile=1
  while [ "$ifile" -le "$ne" ]; do
   input_file="track_mem_${ifile}.txt"
   itime=`awk 'NR == '${iline}' {print $1}' ${input_file}`
   lon_i=`awk 'NR == '${iline}' {print $2}' ${input_file}`
   lat_i=`awk 'NR == '${iline}' {print $3}' ${input_file}`
   if [ "$lon_i" != "" ] && [ "$lat_i" != ""  ]; then
    lon_out=`echo "$lon_out + $lon_i" | bc -l`
    lat_out=`echo "$lat_out + $lat_i" | bc -l`
    nsum=$(($nsum+1))
   fi
   ifile=$(($ifile+1))
  done
  if [ "$nsum" -ge "1" ]; then
   lat_mean=`echo "$lat_out/$nsum" | bc -l`
   lat_mean=`printf "%.2f\n" $lat_mean`
   lon_mean=`echo "$lon_out/$nsum" | bc -l`
   lon_mean=`printf "%.2f\n" $lon_mean`
   echo "lat_out=$lat_out, lat_mean=$lat_mean"
   echo "$itime  $lon_mean  $lat_mean"   >> ${out_file}
  fi
 fi
 iline=$(($iline+1))
done < track_mem_1.txt
#
# create an observed track_file from fort.10
#
ie=$(($ne+2))
echo "BEST Track of $prename" > track_mem_$ie.txt
echo "1 $size" >> track_mem_$ie.txt
echo "2 $style 18" >> track_mem_$ie.txt
echo "$start_hour $ninterval" >> track_mem_$ie.txt 
hh=0
while read line
do
 if [[ $hh -le 120 ]]; then
  echo "$hh  $line" >> track_mem_$ie.txt
 fi
 hh=$(($hh+$ninterval))
done < fort.10
#
# run post process to output time series
#
ie=$(($ne+1))
echo $ne > temp.txt
echo $nt >> temp.txt
ln -sf track_mem_${ie}.txt ./fort.16
cp ./${working_dom}/atcf_mem_1.dat ./fort.17 
./tc_ensemble_wpost_tseries.exe < temp.txt
rm temp.txt
sed -i s/"BB,"/${storm_basin},/ fort.18
sed -i s/"NN,"/${storm_id},/ fort.18
sed -i s/V001/VWRF/ fort.18
mv fort.18 ./${working_dom}/atcf_mem_$ie.dat
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
# create a header file for plotting
#
header="wpost.txt"
echo $runmode > ${header}
echo $ne >> ${header}
echo ${prename} >> ${header}
ptime=`cat ./d01/atcf_mem_1.dat | awk '{print $3}'  | sed 's/,//g' | head -n 1`
echo $ptime >> ${header}
pmin=`awk '{if(min=="")(min=$3); if($3<min)(min=$3)} END {print min}' ./ensemble_tseries_*.txt`
pmax=`awk '{if(max=="")(max=$3); if($3>max)(max=$3)} END {print max}' ./ensemble_tseries_*.txt`
v1=`echo "$pmin - 5" | bc`
v2=`echo "$pmax + 5" | bc`
echo "$v1 $v2" >> $header
vmin=`awk '{if(min=="")(min=$6); if($6<min)(min=$6)} END {print min}' ./ensemble_tseries_*.txt`
vmax=`awk '{if(max=="")(max=$6); if($6>max)(max=$6)} END {print max}' ./ensemble_tseries_*.txt`
v1=`echo "$vmin - 5" | bc`
v2=`echo "$vmax + 5" | bc`
echo "0 $v2" >> $header
latmin=`awk '{if(min=="")(min=$2); if($2<min)(min=$2)} END {print min}' ./fort.10`
latmax=`awk '{if(max=="")(max=$2); if($2>max)(max=$2)} END {print max}' ./fort.10`
v1=`echo "$latmin - 5" | bc`
v2=`echo "$latmax + 5" | bc`
echo "$v1 $v2" >> $header
lonmin=`awk '{if(min=="")(min=$1); if($1<min)(min=$1)} END {print min}' ./fort.10`
lonmax=`awk '{if(max=="")(max=$1); if($1>max)(max=$1)} END {print max}' ./fort.10`
v1=`echo "$lonmin - 5" | bc`
v2=`echo "$lonmax + 5" | bc`
echo "$v1 $v2" >> $header
echo "$nt $ninterval" >> $header
#
# mv all the output to a separate folder
#
mv ensemble_tseries_*.txt ./${working_dom}/
mv ensemble_*.ctl ./${working_dom}/
mv ensemble_tseries.dat ./${working_dom}/
mv ${prename}*.* ./${working_dom}/
mv track_mem*.txt ./${working_dom}/
mv wpost.txt ./${working_dom}/
cp tc_ensemble*.gs ./${working_dom}/
cd ./${working_dom}/
grads -lxbc tc_ensemble_wpost_tseries.gs
grads -xlbc tc_ensemble_wpost_track_driver.gs
mv track.gif ${prename}_${working_dom}_${runmode}_track.gif
mv pmin.gif ${prename}_${working_dom}_${runmode}_pmin.gif
mv vmax.gif ${prename}_${working_dom}_${runmode}_vmax.gif
mv error_record.txt ${prename}_${working_dom}_${runmode}_error.txt
cd ../
rm -f fort.* control_file_${prename} wrf2grads_*.txt
echo "DONE WPOST ENSEMBLE PEACEFULLY"

#!/bin/sh
#
# [NOTE]:   
#           This shell script is for running a WPS in real-time, which is called by a master 
#           script for an ensemble breeding run.
#
# [RUN]:
#  	    To run the script, need to compile WPS code first, then modify some env setup
#           and boundary interval, and the gfs file name format (search for check_name
#           var). 
#
# [HISTORY]:
#         - Mar 27, 2010: created based on that from the breeding project 
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
set -x
DIR_RUN=`pwd`
DIR_WPS="$DIR_MAN/model/WPS"   
DIR_GEOG="$DIR_MAN/model/geog"
DIR_ANA="$DIR_MAN//ana"
interval_bnd=`echo $BND_INTERVAL`
DIR_GRIBDATA=`echo $DIR_GRIB_DATA`
d01_nx=`echo $D01_NX`
d01_ny=`echo $D01_NY`
d01_nz=`echo $D01_NZ`
d01_dx=`echo $D01_DX`
d01_dy=`echo $D01_DY`
d02_nx=`echo $D02_NX`
d02_ny=`echo $D02_NY`
d02_nz=`echo $D02_NZ`
d02_dx=`echo $D02_DX`
d02_dy=`echo $D02_DY`
d02_dt=`echo $D02_DT`
d02_ILL=`echo $D02_ISTART`
d02_JLL=`echo $D02_JSTART`
d02_ratio12=`echo $RATIO_12`
d03_nx=`echo $D03_NX`
d03_ny=`echo $D03_NY`
d03_nz=`echo $D03_NZ`
d03_dx=`echo $D03_DX`
d03_dy=`echo $D03_DY`
d03_dt=`echo $D03_DT`
d03_ILL=`echo $D03_ISTART`
d03_JLL=`echo $D03_JSTART`
REF_LAT=`echo $REF_LAT_MAIN`
REF_LON=`echo $REF_LON_MAIN`
max_dom=`echo $MAXIMUM_DOM`
if [ "$interval_bnd" == "" ]; then 
   echo " WPS: interval_bnd is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$DIR_GRIBDATA" == "" ]; then
   echo " WPS: DIR_GRIBDATA is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$max_dom" == "" ]; then
   echo " WPS: max_dom is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$d01_nx" == "" ]; then
   echo " WPS: d01_nx is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$d01_ny" == "" ]; then
   echo " WPS: d01_ny is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$d01_dx" == "" ]; then 
   echo " WPS: d01_dx is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$d01_dy" == "" ]; then 
   echo " WPS: d01_dy is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$REF_LAT" == "" ]; then
   echo " WPS: REF_LAT is empty, check runmain.sh ...exit 1"
   exit 1
elif [ "$REF_LON" == "" ]; then
   echo " WPS: REF_LON is empty, check runmain.sh ...exit 1"
   exit 1
else
   echo " WPS: all environment vats are ok"
fi
ln -sf $DIR_WPS/* $DIR_RUN
rm -f met_em* FILE:* GRIB* 
echo "   WPS: Current WPS dir is: $DIR_WPS"
echo "   WPS: Gribdata dir is: $DIR_GRIBDATA"
echo "   WPS: Geo data dir for WPS run is: $DIR_GEOG"
echo "   WPS: Running dir is: $DIR_RUN"
echo "   WPS: Initial condition dir backup is: $DIR_ANA"
echo "   WPS: boundary interval updated at $interval_bnd"
#
# option for real-time mode or experiment mode
#
if [ $# == 2 ]; then
  echo " WPS: We will run the case... "
  time_input=$1
  data_year_wps=`echo ${time_input} | cut -c1-4`
  data_month_wps=`echo ${time_input} | cut -c5-6`
  data_day_wps=`echo ${time_input} | cut -c7-8`
  data_hour_wps=`echo ${time_input} | cut -c9-10`
  data_minute_wps=`echo ${time_input} | cut -c11-12`
  fsct_length_wps=$2
else
  echo " WPS: Please use the option: "
  echo "  runwps.sh YYYYMMDDHH HH (for experiment) "
fi		 
echo "   WPS: Running year is: $data_year_wps"
echo "   WPS: Running month is: $data_month_wps"
echo "   WPS: Running day is: $data_day_wps"
echo "   WPS: Running hour is: $data_hour_wps"
echo "   WPS: Forecast hour is: $fsct_length_wps"
#
# construct the strings of starting time and end time for integration
#
. ./quartz_cal_time.sh
cal_time ${data_year_wps} ${data_month_wps} ${data_day_wps} ${data_hour_wps} ${data_minute_wps} 0 ${fsct_length_wps} 0
end_time=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00   
start_time=${data_year_wps}-${data_month_wps}-${data_day_wps}_${data_hour_wps}:${data_minute_wps}:00
echo " WPS: Starting time is: $start_time"
echo " WPS: Ending time is: $end_time"
#
# checking for the input GFS data to see if the data available
# Note that this only apply for AVN data, for other data, need 
# to change the format of the file name: fnl_050711_00_00
#                                        fnl_20100515_12_00_c
# 
#echo " WPS: check for the GFS input data for WPS code now"
#nfile=$(($fsct_length_wps*3600/$interval_bnd + 1))
#ifile=1
##check_file=fnl_${data_year_wps#20}${data_month_wps}${data_day_wps}_${data_hour_wps}_00
#check_file=fnl_${data_year_wps}${data_month_wps}${data_day_wps}_${data_hour_wps}_00_c
#while [ "$ifile" -le "$nfile" ]; do								   
# if [ -f $DIR_GRIBDATA/$check_file ]; then
#  echo "   WPS: input file: $check_file exists"
# else
#  echo "   WPS: input file: $check_file does not exists... exit 3"
#  exit 3
# fi
# next_time=$(($ifile*$interval_bnd/3600))
# cal_time ${data_year_wps} ${data_month_wps} ${data_day_wps} ${data_hour_wps} ${data_minute_wps} 0 ${next_time} 0
## check_file=fnl_${E_YEAR#20}${E_MONTH}${E_DAY}_${E_HOUR}_${E_MINUTE}
# check_file=fnl_${E_YEAR}${E_MONTH}${E_DAY}_${E_HOUR}_${E_MINUTE}_c
# ifile=$(($ifile+1))   
#done
#
# create a namelist for wps
#
mv namelist.wps namelist.wps.backup
cat > ${DIR_RUN}/namelist.wps << EOF
&share
 wrf_core = 'ARW',
 max_dom = ${max_dom},
 start_date = '${start_time}','${start_time}','${start_time}'              
 end_date   = '${end_time}','${end_time}','${end_time}'
 interval_seconds = ${interval_bnd},
 io_form_geogrid = 2,
/

&geogrid
 parent_id         =   1,   1,   2,   3,
 parent_grid_ratio =   1,   ${d02_ratio12},   ${d02_ratio12},   ${d02_ratio12},
 i_parent_start    =   1,   ${d02_ILL},   ${d03_ILL},   3,
 j_parent_start    =   1,   ${d02_JLL},   ${d03_ILL},   9,
 s_we              =   1,   1,   1,   1,
 e_we              =  ${d01_nx},  ${d02_nx},  ${d03_nx},  55,
 s_sn              =   1,   1,   1,   1,
 e_sn              =  ${d01_ny},  ${d02_ny},  ${d03_ny},  55,
 geog_data_res     = '10m','5m','2m','30s',
 dx        = ${d01_dx},
 dy        = ${d01_dy},
 map_proj  = 'mercator',
 ref_lat   =  ${REF_LAT},
 ref_lon   =  ${REF_LON},
 truelat1  =  15.93,
 truelat2  =  60.0,
 stand_lon =  100.0,
 geog_data_path = '${DIR_GEOG}'
/

&ungrib
 out_format = 'WPS',
 prefix     = 'FILE',
/

&metgrid
 fg_name         = 'FILE'
 io_form_metgrid = 2,
/

EOF
#
# runing the WPS model now
#
./link_grib.csh $DIR_GRIBDATA/
echo " WPS: running geogrid.exe ..."
./geogrid.exe >& log.geogrid
echo " WPS: running ungrib.exe ..."
./ungrib.exe >& log.ungrib
echo " WPS: running metgrid.exe ..."
./metgrid.exe >& log.metgrid
#
# create a backup dir and clean the dir
#
metdir=${data_year_wps}${data_month_wps}${data_day_wps}${data_hour_wps}${data_minute_wps}
if [ -d $DIR_ANA/${metdir} ]; then
    rm -rf $DIR_ANA/${metdir}
fi 
mkdir $DIR_ANA/${metdir}
check_met_em=`ls met_em*`
if [ "${check_met_em}" == "" ]; then
    echo "WPS: quartz_wps.sh failed... Exit"
    exit 1
else
    mv met_em* $DIR_ANA/${metdir}/
fi
rm -f FILE:* GRIBFILE* 
echo " WPS script finished normally              "
echo "                                           "
echo " WPS: quartz_wps.sh finished normally       " > $DIR_RUN/error.status


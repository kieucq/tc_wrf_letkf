#!/bin/sh
#
# [NOTE]:   
#           This shell script is for creating a set of obs data for letkf test  
#
# [HISTORY]:
#         - 26 Mar, 2010: Created
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
. ./input.sh
interval_restart_second=`echo $RESTART_INTERVAL`           # frequency of truth data in seconds
interval_restart=$(($interval_restart_second/60))          # frequency of truth data in minutes
obsmode=`echo $OBSMODE`
runmode=`echo $RUNMODE`
d01_nx=`echo $D01_NX`
d01_ny=`echo $D01_NY`
d01_nz=`echo $D01_NZ`
d01_dx=`echo $D01_DX`
ref_lat=`echo $REF_LAT_MAIN`
ref_lon=`echo $REF_LON_MAIN`
if [ "$interval_restart" == "" ]; then
 echo " OBS: interval boundary is empty, check runmain.sh ...exit 0"
 exit 0
elif [ "$obsmode" == "" ]; then
 echo " OBS: OBSMODE is empty, check runmain.sh ...exit 0"
 exit 0
elif [ "$runmode" == "" ]; then
 echo " OBS: RUNMODE is empty, check runmain.sh ...exit 0"
 exit 0
fi
rm -f tru_d01* tru_*.dat
if [ "$obsmode" == "BVOX" ] || [ "$obsmode" == "REAN" ]; then
 ln -sf ../truth/tru_d01*:* ./
fi
#
# option for real-time mode or experiment mode. The input is taken from the
# shared input.sh
#
data_year_obs=`echo ${time_input} | cut -c1-4`
data_month_obs=`echo ${time_input} | cut -c5-6`
data_day_obs=`echo ${time_input} | cut -c7-8`
data_hour_obs=`echo ${time_input} | cut -c9-10`
data_minute_obs=`echo ${time_input} | cut -c11-12`
fsct_length_obs=$(($fsct_length_hour*60))                # forecast length in minutes
echo " OBS: Running year is: $data_year_obs"
echo " OBS: Running month is: $data_month_obs"
echo " OBS: Running day is: $data_day_obs"
echo " OBS: Running hour is: $data_hour_obs"
echo " OBS: Forecast minute is: $fsct_length_obs"
#
# construct the strings of starting time and end time for integration
#
. ./carbonate_cal_time.sh
cal_time ${data_year_obs} ${data_month_obs} ${data_day_obs} ${data_hour_obs} ${data_minute_obs} 0 0 ${fsct_length_obs} 
end_time=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00   
start_time=${data_year_obs}-${data_month_obs}-${data_day_obs}_${data_hour_obs}:${data_minute_obs}:00
echo " OBS: Starting time is: $start_time"
echo " OBS: Ending time is: $end_time"
#
# creating obs.dat file
# 
echo " OBS: Start looping now ..."
if [ "$fsct_length_obs" == "0" ]; then
 nfile=1
else 
 nfile=$(($fsct_length_obs/$interval_restart + 1))
fi
ifile=1
if [ "$obsmode" == "BVOX" ] || [ "$obsmode" == "REAN" ]; then
 if [ "$runmode" == "REAL_TIME" ]; then
  rm -f wrfinput_d01	 
  ln -s ../run/wrfinput_d01 ./
  check_file="wrfinput_d01"  
 else	 
  check_file=tru_d01_${data_year_obs}-${data_month_obs}-${data_day_obs}_${data_hour_obs}:${data_minute_obs}:00
 fi
 obs_file=obs_${data_year_obs}-${data_month_obs}-${data_day_obs}_${data_hour_obs}:${data_minute_obs}:00
 rm -f obs*.dat obs_*
 while [ "$ifile" -le "$nfile" ]; do								   
  if [ -f $check_file ]; then
   echo " OBS: input file: $check_file exists"
   rm -f $obs_file
   ln -sf $check_file ./truth.dat
   rm -f error.status
   ./obs.exe
   mv obsout.dat $obs_file
   mv obs.dat obs${ifile}.dat
  else
   echo " OBS: input file: $check_file does not exists... exit 3"
   exit 3
  fi
  next_time=$(($ifile*$interval_restart))
  cal_time ${data_year_obs} ${data_month_obs} ${data_day_obs} ${data_hour_obs} ${data_minute_obs} 0 0 ${next_time}
  check_file=tru_d01_${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
  obs_file=obs_${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
  ifile=$(($ifile+1))   
 done
 rm -f truth.dat *~ error.status
 ln -sf obs1.dat obs.dat
elif [ "$obsmode" == "RADS" ]; then
 sound_input="${data_year_obs#20}${data_month_obs}${data_day_obs}${data_hour_obs}"
 echo $sound_input > sin.dat 
 obs_time=${data_year_obs}-${data_month_obs}-${data_day_obs}_${data_hour_obs}:${data_minute_obs}:00 
 cal_time ${data_year_obs} ${data_month_obs} ${data_day_obs} ${data_hour_obs} ${data_minute_obs} 0 0 0
 obs_timem=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
 cal_time ${data_year_obs} ${data_month_obs} ${data_day_obs} ${data_hour_obs} ${data_minute_obs} 0 0 120
 obs_timep=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
 obs_file=obs_${obs_time}
 obs_dir="${data_year_obs}${data_month_obs}${data_day_obs}${data_hour_obs}${data_minute_obs}"
 ln -sf $DIR_ANA/${obs_dir}/wrfinput_d01 ./
#
# run a program to convert from radionsonde to litte_r format first
#
 if [ -f ${DIR_OBS_DATA}/${obsmode}/${obs_dir}/stationid.txt ]; then
  echo " OBS: radiosonde dir: $obs_dir exists"
  ln -sf ${DIR_OBS_DATA}/${obsmode}/$obs_dir/* ./
  ln -sf ${DIR_OBS_DATA}/${obsmode}/$obs_dir/stationid.txt ./
  ./sounding_to_littler.exe < sin.dat >> $DIR_RUN/log.runobs
 else
  echo " OBS: station id for radiosonde data does not exist... exit 3"
  echo " OBS: ${DIR_OBS_DATA}/${obsmode}/$obs_dir"
  exit 3
 fi
#
# linking and creating a namelist for WRF-OBSPROC
#
 ln -sf $DIR_3DV/obsproc.exe ./
 ln -sf $DIR_3DV/*.txt ./
 cat > namelist.obsproc << EOF
&record1
 obs_gts_filename = 'obs_littler.dat',
 obs_err_filename = 'obserr.txt',
/

&record2
 time_window_min  = '${obs_timem}',
 time_analysis    = '${obs_time}',
 time_window_max  = '${obs_timep}',
/

&record3
 max_number_of_obs        = 400000,
 fatal_if_exceed_max_obs  = .TRUE.,
/

&record4
 qc_test_vert_consistency = .TRUE.,
 qc_test_convective_adj   = .TRUE.,
 qc_test_above_lid        = .TRUE.,
 remove_above_lid         = .TRUE.,
 domain_check_h           = .true.,
 Thining_SATOB            = false,
 Thining_SSMI             = false,
 Thining_QSCAT            = false,
/

&record5
 print_gts_read           = .TRUE.,
 print_gpspw_read         = .TRUE.,
 print_recoverp           = .TRUE.,
 print_duplicate_loc      = .TRUE.,
 print_duplicate_time     = .TRUE.,
 print_recoverh           = .TRUE.,
 print_qc_vert            = .TRUE.,
 print_qc_conv            = .TRUE.,
 print_qc_lid             = .TRUE.,
 print_uncomplete         = .TRUE.,
/

&record6
 ptop =  5000.0,
 base_pres       = 100000.0,
 base_temp       = 300.0,
 base_lapse      = 50.0,
 base_strat_temp = 215.0,
 base_tropo_pres = 20000.0
/

&record7
 IPROJ = 3,
 PHIC  = ${ref_lat},
 XLONC = ${ref_lon},
 TRUELAT1= 30.93,
 TRUELAT2= 60.0,
 MOAD_CEN_LAT = 25,
 STANDARD_LON = 110.,
/

&record8
 IDD    =   1,
 MAXNES =   1,
 NESTIX =  ${d01_nx},  200,  136,  181,  211,
 NESTJX =  ${d01_ny},  200,  181,  196,  211,
 DIS    =  $(($d01_dx/1000)),  10.,  3.3,  1.1,  1.1,
 NUMC   =    1,    1,   2,     3,    4,
 NESTI  =    1,   40,  28,    35,   45,
 NESTJ  =    1,   60,  25,    65,   55,
 /

&record9
 PREPBUFR_OUTPUT_FILENAME = 'prepbufr_output_filename',
 PREPBUFR_TABLE_FILENAME = 'prepbufr_table_filename',
 OUTPUT_OB_FORMAT = 2
 use_for          = '3DVAR',
 num_slots_past   = 3,
 num_slots_ahead  = 3,
 write_synop = .true.,
 write_ship  = .true.,
 write_metar = .true.,
 write_buoy  = .true.,
 write_pilot = .true.,
 write_sound = .true.,
 write_amdar = .true.,
 write_satem = .true.,
 write_satob = .true.,
 write_airep = .true.,
 write_gpspw = .true.,
 write_gpsztd= .true.,
 write_gpsref= .true.,
 write_gpseph= .true.,
 write_ssmt1 = .true.,
 write_ssmt2 = .true.,
 write_ssmi  = .true.,
 write_tovs  = .true.,
 write_qscat = .true.,
 write_profl = .true.,
 write_bogus = .true.,
 write_airs  = .true.,
 /
 
EOF
 echo " OBS: running obsproc.exe..."
 ./obsproc.exe >& $DIR_RUN/log.obsproc
#
# final convert from output of WRF-3DVAR format to LETKF format
#
 rm -f ./${obs_file}
 ln -sf obs_gts_${obs_time}.3DVAR ./obsproc.dat
 echo " OBS: running wrf3dvar_to_letkf.exe..."
 ./wrf3dvar_to_letkf.exe >> $DIR_RUN/log.runobs
 mv obsout.dat ./${obs_file} 
elif [ "$obsmode" == "CIMSS" ]; then
 obs_time=${data_year_obs}-${data_month_obs}-${data_day_obs}_${data_hour_obs}:${data_minute_obs}:00
 obs_file=obs_${obs_time}
 obs_dir="${data_year_obs}${data_month_obs}${data_day_obs}${data_hour_obs}${data_minute_obs}"
 cimss_file="${obs_dir}.txt"
 ln -sf $DIR_ANA/${obs_dir}/wrfinput_d01 ./
#
# run a program to convert from CIMSS AMVs with QC to letkf format 
#
 if [ -f ${DIR_OBS_DATA}/${obsmode}/${cimss_file} ]; then
  echo " OBS: CIMSS AMV data: ${cimss_file} exists"
  ln -sf ${DIR_OBS_DATA}/${obsmode}/${cimss_file} ./cimss_text.dat  
  ./cimss_to_letkf.exe >> $DIR_RUN/log.cimss
 else
  echo " OBS: CIMSS AMV wind data does not exist... exit 3"
  echo " OBS: ${DIR_OBS_DATA}/${obsmode}/$cimss_file"
  exit 3
 fi
 rm -f ./${obs_file}
 mv obsout.dat ./${obs_file}
else
 echo " OBS: dont understand OBSMODE=$obsmode... exit 4"
 exit 4
fi
#rm -f obs_littler.dat obsproc.dat sin.dat error.status fort*
echo "                                     "
echo " OBS script finished safely        "
echo " OBS script finished safely        " > error.status








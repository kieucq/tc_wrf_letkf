#!/bin/sh
#
# [NOTE]:   
#           This shell script is calling a master loop to perform the LETKF cycles 
#           for WRF V3.1 system.  
#
# [RUN]:
#	    To run the script, need to compile WRF code first, then modify some env setup
#           and boundary interval, and the gfs file name format (search for check_name
#           var). 
#
# [HISTORY]:
#         - Mar 02, 2010: Created from runmain.sh of the breeding projects
#         - Mar 10, 2010: update the namelist for better communidcation with subscripts
#         - Apr 19, 2010: add an option for EXP_IDEAL 
#         - Apr 22, 2010: update the time_string to include minute signal 
#         - Oct 05, 2010: update real time experiment
#         - Feb 06, 2011: support qsub parallel
#         - Mar 07, 2011: add a third grid option
#         - Mar 17, 2011: add the CIMSS satelitte wind option
#         - Apr 15, 2011: add multiple cumulus and radiation from bk10 
#         - May 04, 2011: add the QSUb option for the EXP_IDEAL
#
# [AUTHOR]:
#           Chanh Kieu, Vietnam National University (chanhkq@vnu.edu.vn)
#
# [DEPENDENCIES]:
#          1. runwps.sh		                  ! running WPS
#          2. runreal.sh			  ! running WRF-REAL
#          3. runcolds.sh			  ! running cold start
#          4. ../truth/runtru.sh		  ! creating a bogus truth
#          5. ../ini/runini.sh			  ! creating a set of cold start members
#          6. ../ctl/runctl.sh			  ! running a ctl for the cold start
#          7. ../letkf/runletkf.sh		  ! letkf main code
#          8. ../dig/runrms.sh    		  ! compute post analysis and evaluation
#   	   9. func_time_cal.sh			  ! external func for computing time strings
#          10 runwpost.sh                         ! create figures for real_time mode
#          11 run3dvar.sh                         ! support for run3dvar radiosonde q.c.
#          12 func_cal_ensemble.sh                ! compute ensemble information
#
# [COPYRIGHT]: (C) 2011 Commercial version trademarked for the I.M System Group, Inc, USA
#             
#===========================================================================================
# 
# setting up path environment
#
DIR_MAN="/state/partition1/home/kieuc/da_wrf_letkf"
DIR_RUN=`pwd`
DIR_WPS="${DIR_MAN}/model/WPS"
DIR_WRF="${DIR_MAN}/model/WRFV3"
DIR_FSC="${DIR_MAN}/fsc"
DIR_INI="${DIR_MAN}/ini"
DIR_ANA="${DIR_MAN}/ana"
DIR_BGD="${DIR_MAN}/bgd"
DIR_OBS="${DIR_MAN}/obs"
DIR_CTL="${DIR_MAN}/ctl"
DIR_DIG="${DIR_MAN}/dig"
DIR_LETKF="${DIR_MAN}/letkf"
DIR_GRIB_DATA="${DIR_MAN}/data/avn/20080814"
DIR_POST="${DIR_MAN}/wrfpost"
MPIRUN="yes"
NCORE=8
WRF_QSUB="YES"
#runwrfmpi="mpirun -np 2 -machinefile mywrf ./wrf.exe"
runwrfmpi="mpirun -n 3 -host cluster.IMSG.COM ./wrf.exe"
#
# set up a date namelist for the job. No need for REAL_TIME mode
#
ratio_12=3                                           # ratio of domain 1 over 2  
d01_year="2008"                                      # start year for domain 1
d01_month="08"				   	     # start month for domain 1
d01_day="28"                                         # start day for domain 1
d01_hour="00"                                        # start hour for domain 1
d01_minute="00"                                      # start minute for domain 1
d02_year=$d01_year                                   # start year for domain 2
d02_month="08"                                       # start month for domain 2
d02_day="28"                                         # start day for domain 2
d02_hour="00"                                        # start hour for domain 2
d02_minute="00"                                      # start minute for domain 2
d03_year=$d01_year                                   # start year for domain 3
d03_month="08"                                       # start month for domain 3
d03_day="28"                                         # start day for domain 3
d03_hour="00"                                        # start hour for domain 3
d03_minute="00"                                      # start minute for domain 3
#
# set up domain for the job
#
d01_nx=151                                           # number of grid point in x-dim for WRF namelist
d01_ny=151                                           # number of grid point in y-dim for WRF namelist
d01_nz=31                                            # number of grid point in z-dim for WRF namelist         
d01_dx=36000                                         # dx for domain 1 in m
d01_dy=36000                                         # dy for domain 1 in m
d01_dt=180                                           # dt for domain 1 in second
d02_nx=100                                           # number of grid point in x-dim for domain 2
d02_ny=100                                           # number of grid point in y-dim for domain 2
d02_nz=$d01_nz                                       # number of grid point in z-dim for domain 2
d02_dx=$(($d01_dx/$ratio_12))                        # dx for domain 2 in m
d02_dy=$(($d01_dy/$ratio_12))                        # dy for domain 2 in m
d02_dt=$(($d01_dt/$ratio_12))                        # dt for domain 2 in second
d02_istart=58                                        # start i lower left of domain 2
d02_jstart=58                                        # start j lower left of domain 2
d03_nx=100                                           # number of grid point in x-dim for domain 3
d03_ny=100                                           # number of grid point in y-dim for domain 3
d03_nz=$d01_nz                                       # number of grid point in z-dim for domain 3
d03_dx=$(($d02_dx/$ratio_12))                        # dx for domain 3 in m
d03_dy=$(($d02_dy/$ratio_12))                        # dy for domain 3 in m
d03_dt=$(($d02_dt/$ratio_12))                        # dt for domain 3 in second
d03_istart=32                                        # start i lower left of domain 3
d03_jstart=32                                        # start j lower left of domain 3
ref_lat="15.2"                                       # reference lat for the domain center (mercator proj)
ref_lon="122.0"                                      # reference lon for the domain center (mercator proj)
corr_dist=5                                          # distance to the bnd to control movement   
#
# set up boundary update and ensemble information
#
interval_bnd=21600                                   # interval updating the boundary for WPS in second
interval_out=180                                     # output interval of the WRF model in minute
nloop=7                                              # number of master script loop
restart=720                                          # restart interval for new da cycle in minute
ne=21                                                # number of ensemble members
nme=1                                                # number of model error ensemble members
model_flag=1                                         # option for model asign: 0-perfect; 1-imperfect
model_opt="YES"                                       # option for different model physics for model err
mcphysics=1                                          # default physics option                      
lwradiation=1                                        # default long wave radiation option
cu_opt=1                                             # default cumulus option
infactor="1.0"                                       # inflation factor
runmode="REAL_TIME"                                  # mode of running EXP_REAL, EXP_IDEAL, REAL_TIME, EXP_REAN
cold_start="COLD"   	         		     # cold start option
obsmode="CIMSS"                                       # BVOX, radioson(RADS), radar(RADA), reanaly(REAN), CIMSS
maximum_dom=3                                       # maximum number of WRF domain
damode="LETKF"                                       # option for LETKF or 3DVAR (3DVAR script is runmain3dvar.sh)
tloop=$(($nloop-1))
fsct_length_main=$(($tloop*$restart/60))             # total length of bogussed observation for exp mode in hrs
da_var=5                                             # 0 (all); 1(no u); 2(no v); 3(no T)... see readme
h_local=5                                            # dimension of horizontal local patch
v_local=3                                            # dimension of vertical local patch
if [ "$obsmode" == "CIMSS" ]; then
 da_var=345
 v_local=3
fi
#
# set up time namelist for REAL_TIME mode
#
if [ "${runmode}" == "REAL_TIME" ]; then
 if [ $# == 4 ]; then
  time_input=$1
  d01_year=`echo ${time_input} | cut -c1-4`
  d01_month=`echo ${time_input} | cut -c5-6`
  d01_day=`echo ${time_input} | cut -c7-8`
  d01_hour=`echo ${time_input} | cut -c9-10`
  d01_minute=`echo ${time_input} | cut -c11-12`
  d02_year=$d01_year                          
  d02_month=$d01_month                          
  d02_day=$d01_day                                
  d02_hour=$d01_hour                               
  d02_minute=$d01_minute 
  d03_year=$d02_year
  d03_month=$d02_month
  d03_day=$d02_day
  d03_hour=$d02_hour
  d03_minute=$d02_minute                               
  fsct_length_main=$2                                # forecast length in hours
  interval_bnd=$(($3*3600))                          # interval updating the boundary for WPS in second
  cold_start=$4
  DIR_GRIB_DATA="${DIR_MAN}/data/gfs/${d01_year}${d01_month}${d01_day}${d01_hour}${d01_minute}"
 else
  echo " MAIN: runmain.sh needs input from control script runmaster.sh"
  echo " ./runmaster.sh YYYYMMDDHH HH HH cold_start"
  echo " MAIN: exiting 0"
  exit 0
 fi     
fi
#
# cross check of supporting options  
#
if [ "$runmode" == "EXP_REAL" ] && [ "$obsmode" == "BVOX" ]; then
 echo " MAIN: does not make sense to run bogussed vortex with real-time cycle...exit 0"
fi
if [ "$runmode" == "EXP_IDEAL" ] && [ "$obsmode" == "RADS" ]; then
 echo " MAIN: does not make sense to run ideal case with radiosondes assimilation...exit 0"
 exit 0
fi
if [ "$damode" != "LETKF" ]; then
 echo " MAIN: damode=LETKF is the only opt for runmain.sh ...exit 0"
 exit 0
fi
if [ "$runmode" == "REAL_TIME" ] && [ "$obsmode" != "REAN" ]; then
 echo " MAIN: WARNING: REAL_TIME exp is supported with REAN mode only"
fi
#
# set up a namelist for the LETKF code
#
mv -f ../namelist.letkf ./namelist.letkf.backup
cat > ../namelist.letkf << EOF
debug        = 0               ! debuging level
restart      = $interval_out            ! reatart inteval for assimilation [mn]
dy           = ${d01_dy}           ! grid distance in the y-direction [m] 
zamp         = 0              ! amplitude of v-anomaly           [m]
zscale       = 1e+5            ! scale of v-anomaly               [m]
icen         = 72              ! i-center of the bogus obs patch  []
jcen         = 72              ! j-center of the bogus obs patch  []
obs_err_u    = 1.2             ! obs error for u                  [m/s]
obs_err_v    = 1.2             ! obs error for v                  [m/s]
obs_err_t    = 1.2             ! obs error magnitude              [m]
obs_err_q    = 1e-3            ! obs error for q                  [kg/kg]
obs_err_p    = 1.2             ! obs error for geopoten perp      [m^2 s^-2]
bgd_err_u    = 1.4             ! background err for u coldstart   [m]
bgd_err_v    = 1.4             ! background err for v coldstart   [m/s]
bgd_err_t    = 1.4             ! background err for T coldstart   [K]
bgd_err_q    = 2e-3            ! background err for q coldstart   [kg/kg]
bgd_err_p    = 2.              ! background err for p coldstart   [m^2 s^-2]
model_flag   = ${model_flag}               ! flag for model: 0-perfect, 1-imperfect
ini_flag     = 1               ! flag for initial condition: 0-perfect, 1-imperfect
rscale       = 1.0             ! scale of localization
ifactor      = ${infactor}             ! inflation factor to increase model error
nme          = ${nme}              ! number of ensemble member for model error calculation
timeout      = 100             ! output interval for the model (steps)
tlm_flag     = 1               ! option for the TLM model: 1-first order, 2-second order
no           = 1               ! number of observation locations = nz*(2*r_obs+1)^2
ne           = ${ne}              ! number of ensemble members for LETKF
nxl          = ${h_local}               ! size of the local patch in x-y direction
nzl          = ${v_local}               ! size of the local patch in z direction
slat         = 10.             ! start latitude
nx           = $(($d01_nx-1))              ! grid point in x-direction for mass point
ny           = $(($d01_ny-1))              ! grid point in y-direction for mass point
nz           = $(($d01_nz-1))              ! grid point in z-direction for mass point
tfcst        = ${fsct_length_main}                ! length of forecast               [h]
dt           = 300.            ! model timestep
obs_flag     = 1               ! option for obs output: 0:full, 1:vortex
oscale       = 700e+03         ! radius of obs influence
da_flag      = ${da_var}               ! 0 (all); 1(no u); 2(no v);... see readme
r_obs        = 1              ! radius around(icen,jcen) where bogused obs is created 
para3        = 0               ! extra slot for later new para
para4        = 0               ! extra slot for later new para
para5        = 0               ! extra slot for later new para
para6        = 0               ! extra slot for later new para
para7        = 0               ! extra slot for later new para
para8        = 0               ! extra slot for later new para
para9        = 0               ! extra slot for later new para
para10       = 0               ! extra slot for later new para
para11       = 0               ! extra slot for later new para
para12       = 0               ! extra slot for later new para
para13       = 0               ! extra slot for later new para
para14       = 0               ! extra slot for later new para
para15       = 0               ! extra slot for later new para
para16       = 0               ! extra slot for later new para
para17       = 0               ! extra slot for later new para
para18       = 0               ! extra slot for later new para
para19       = 0               ! extra slot for later new para
EOF
ln -sf ../namelist.letkf ./
#
#====================================================================================
#
# STOP HERE! END OF USER CHANGES  
#
#====================================================================================
#
# set up common communication stored as environment
#
data_year_main=${d01_year}
data_month_main=${d01_month}
data_day_main=${d01_day}
data_hour_main=${d01_hour}
data_minute_main=${d01_minute}
sho=`echo $SHELL`
export BND_INTERVAL=${interval_bnd}
export D01_NX=${d01_nx}
export D01_NY=${d01_ny}
export D01_NZ=${d01_nz}
export D01_DX=${d01_dx}
export D01_DY=${d01_dy}
export D01_DT=${d01_dt}
export D02_NX=${d02_nx}
export D02_NY=${d02_ny}
export D02_NZ=${d02_nz}
export D02_DX=${d02_dx}
export D02_DY=${d02_dy}
export D02_DT=${d02_dt}
export D02_ISTART=${d02_istart}
export D02_JSTART=${d02_jstart}
export D03_NX=${d03_nx}
export D03_NY=${d03_ny}
export D03_NZ=${d03_nz}
export D03_DX=${d03_dx}
export D03_DY=${d03_dy}
export D03_DT=${d03_dt}
export D03_ISTART=${d03_istart}
export D03_JSTART=${d03_jstart}
export RATIO_12=${ratio_12}
export REF_LAT_MAIN=${ref_lat}
export REF_LON_MAIN=${ref_lon}
export RUNMODE=${runmode}
export OBSMODE=${obsmode}			   
export DIR_GRIB_DATA=${DIR_GRIB_DATA} 
export MAXIMUM_DOM=${maximum_dom}			   
export RESTART_INTERVAL=$(($restart*60))
export MPIRUN=${MPIRUN}
export OUT_INTERVAL=${interval_out}
export DAMODE=${damode}
export CORR_DIST=${corr_dist}
export LETKF_RUN_DIR=${DIR_RUN}
#
# option for real-time mode or experiment mode
#
if [ "$interval_out" -gt "$restart" ]; then
 echo " MAIN: WRF output interval cannot be larger than restart interval...exit 1"
 exit 1 
fi
if [ "$cold_start" == "COLD" ] && [ "$runmode" != "REAL_TIME" ]; then
  echo " MAIN: We will run a clean cold start... "
  rm -rf $DIR_FSC/*
  rm -rf $DIR_ANA/*
  rm -rf $DIR_BGD/*
  rm -f log.*
else
  echo " MAIN: Run either REAL_TIME or cold_start is not set"
  rm -f log.*  
fi		 
echo " MAIN: Checking for the MAIN path now ..." 	   
echo " MAIN: Current WPS dir is: $DIR_WPS" 
echo " MAIN: Current WRF dir is: $DIR_WRF" 
echo " MAIN: Initializing dir is: $DIR_INI" 
echo " MAIN: Running dir is: $DIR_RUN" 
echo " MAIN: Analysis dir backup is: $DIR_ANA"
echo " MAIN: Background dir backup is: $DIR_BGD"
echo " MAIN: Observational dir backup is: $DIR_OBS"
echo " MAIN: Control dir backup is: $DIR_CTL"
echo " MAIN: Diagnostic dir is: $DIR_DIG"
echo " MAIN: Running year is: $data_year_main" 
echo " MAIN: Running month is: $data_month_main" 
echo " MAIN: Running day is: $data_day_main" 
echo " MAIN: Running hour is: $data_hour_main" 
echo " MAIN: Forecast hour for bogussed obs is: $fsct_length_main"
echo " MAIN: number of ensemble is $ne" 
echo " MAIN: number of experimental loop is $nloop " 
echo " MAIN: restart inverval in hours is $restart (mn)" 
echo " MAIN: Model domain d01 $d01_nx, $d01_ny, $d01_nz"
echo " MAIN: Model grid d01 $d01_dx, $d01_dy"
start_time_main=${data_year_main}${data_month_main}${data_day_main}${data_hour_main}${data_minute_main}
start_time_orig=${data_year_main}${data_month_main}${data_day_main}${data_hour_main}${data_minute_main}
#
# loop da cycle now
#
if [ $runmode == "EXP_REAL" ]; then
 iloop=1
 while [ "$iloop" -le "$nloop" ]; do
  echo " " 
  echo " "
  echo " ============================================================================================= "
#
# calling first the WPS system. Note that the time_interval var is set in each script
# separately. Can change it to the environment so that we can retrieve it for all 
# later run. Oh, ok, if I use . ./xxx.sh then the values will be stored inside that
# subroutine.... dont need to set bnd_interval any longer.
#
  echo " MAIN: Starting time for EXP_REAL is: $start_time_main with looping at $iloop" 
  echo " MAIN: cold_start = $cold_start"
  rm -f error.status
  ./runwps.sh ${start_time_main} ${fsct_length_main} 
  if [ -f error.status ]; then
   rm -f error.status
  else
   echo " MAIN: runwps.sh has abrupt die...exit 3"
   exit 3
  fi
#
# calling next the WRF real to create a control boundary and initial condition
#
  rm -f wrfinput_d0* wrfbdy_d0* rsl*
  ./runreal.sh ${start_time_main} ${fsct_length_main} ${runmode}
  if [ -f error.status ]; then
   rm -f error.status
  else
   echo " MAIN: runreal.sh has abrupt die...exit 4"
   exit 4
  fi
  ln -sf $DIR_ANA/${start_time_main}/wrfinput_d0* ./
  ln -sf $DIR_ANA/${start_time_main}/wrfbdy_d0* ./
#
# create a cold start for an ensemble of wrf input 
#
  if [ $cold_start == "COLD" ]; then
   rm -f rsl* error*
   ./runcolds.sh ${start_time_main} ${fsct_length_main} ${ne} ${runmode}
   if [ -f error.status ]; then
    rm -f error.status
   else
    echo " MAIN: runcolds.sh has abrupt die...exit 5"
    exit 5
   fi
   cold_start="WARM"
  fi 
#
# prepare observation next by calling WRF-3DVAR to create obs files
#
  if [ "$obsmode" != "REAN" ]; then
   cd $DIR_OBS
   ./runobs.sh ${start_time_main} ${fsct_length_main} > $DIR_RUN/log.runobs 
   cd $DIR_RUN
  fi 
#
# calling the letkf core now
#
  cd $DIR_LETKF
  ln -sf $DIR_RUN/namelist.letkf ./
  rm -f wrfinput_d0* wrfbdy_d0*
  ln -sf $DIR_RUN/wrfinput_d0* ./
  ln -sf $DIR_RUN/wrfbdy_d0* ./
  ./runletkf.sh ${start_time_main} ${ne}
  if [ -f $DIR_RUN/error.status ]; then
   rm -f $DIR_RUN/error.status
  else
   echo " MAIN: runletkf.sh has abrupt die...exit 5"
   exit 6
  fi
#
# update boundary and create namelist for each member 
#
  cd $DIR_RUN
  . ./func_cal_time.sh
  . ./func_cal_ensemble.sh
  cal_time ${data_year_main} ${data_month_main} ${data_day_main} ${data_hour_main} ${data_minute_main} 0 0 ${restart}
  restart_file=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
  restart_dir=${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}${E_MINUTE}
  ie=1
  while [ "$ie" -le "$ne" ]; do
   cal_ensemble_prefix ${ie}
   prefix=${out_prefix}
#
# link the analysis
#
   rm -f wrfinput_d0* wrfbdy_d0*
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfbdy_d0* ./
#
# update boundary condition now for each member. Note that link to wrfbdy_d01
# will be overwritten and each member's wrfbdy_d01 will be modified with
# this step.
#
   ln -sf $DIR_ANA/${start_time_main}/wrfinput_d01 ./wrfinput_d01_orig
   ln -sf ../model/WRFDA/var/test/update_bc/parame.in ./
   ln -sf ../model/WRFDA/var/build/da_update_bc.exe ./
   ./da_update_bc.exe > log.update_bc
   if [ -f fort.11 ]; then
     rm -f fort*
   else
     echo " MAIN: da_update_bc.exe has abrupt die...exit 7"
     exit 7
   fi
#
# option for multiple physics options
#
   if [ "$model_opt" == "YES" ]; then
    lwradiation=$(($lwradiation+1))
    if [ "$lwradiation" -gt "2" ]; then
     lwradiation=1
     cu_opt=$(($cu_opt+1))
     if [ "$cu_opt" -gt "2" ]; then
      cu_opt=1
      mcphysics=$(($mcphysics+1))
      if [ "$mcphysics" -gt "7" ]; then
       mcphysics=1
      fi
     fi
    fi
   fi
   ./nlist_wrf.sh ${start_time_main} $fsct_length_main $mcphysics $lwradiation $cu_opt $runmode > log.namelist
   mv namelist.input_${mcphysics} $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input
#
# create qsub for each member
#
cat > wrf_qsub.sh << EOF1
#!/bin/sh
#$ -N rsl_qsub
#$ -cwd
#$ -j y
#$ -pe mpich 8
#$ -S /bin/bash
echo "Got \$NSLOTS processors."
echo "Machines:"
echo "\$TMPDIR/machines"
echo "node file is \$HOSTNAME"
cat \$TMPDIR/machines
/state/partition1/home/kieuc/opt/mpich1-pgi/bin/mpirun -np \$NSLOTS ./wrf.exe
rm -f *.exe *.TBL *DATA* ozone* *.tbl grib* co2_trans*
EOF1
   mv wrf_qsub.sh $DIR_ANA/${start_time_main}/mem_${prefix}/
   ie=$(($ie+1))
  done
#
# option for submit qsub job for all members simultaneously or serially
#
  ie=1
  while [ "$ie" -le "$ne" ]; do
   cal_ensemble_prefix ${ie}
   prefix=${out_prefix}
   if [ "$WRF_QSUB" == "NO"  ]; then
    rm -f wrfinput_d0* wrfbdy_d0*
    ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
    ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfbdy_d0* ./
    ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input ./namelist.input
#
# running WRF core serially
#
    echo " MAIN: running wrf.exe for member $ie ..."
    rm -f rsl*
    if [ "$MPIRUN" == "yes" ]; then
     $runwrfmpi >& log.wrf
    else
     ./wrf.exe >& log.wrf
    fi
    if [ "$ie" -eq "1" ]; then
     if [ -d $DIR_FSC/${start_time_main} ]; then
      rm -rf $DIR_FSC/${start_time_main}
     fi
     mkdir -p $DIR_FSC/${start_time_main}
     mkdir -p $DIR_BGD/${restart_dir}
    fi
    mkdir -p $DIR_FSC/${start_time_main}/mem_${prefix}
    mv wrfout_* $DIR_FSC/${start_time_main}/mem_${prefix}
    cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
   else
    cd $DIR_ANA/${start_time_main}/mem_${prefix}/
    ln -sf $DIR_WRF/run/*.exe ./
    ln -sf $DIR_WRF/run/*.TBL ./
    ln -sf $DIR_WRF/run/*DATA* ./
    ln -sf $DIR_WRF/run/ozone* ./
    ln -sf $DIR_WRF/run/*.tbl ./
    ln -sf $DIR_WRF/run/grib* ./
    ln -sf $DIR_WRF/run/co2_trans* ./
    qsub wrf_qsub.sh >> $DIR_RUN/log.qsub 
    cd $DIR_RUN
   fi
   ie=$(($ie+1))
  done
#
# for qsub mode, check if all run finish normally
#
  if [ "$WRF_QSUB" == "YES" ]; then
   qsub_done=0
   qsub_count=0
   while [ "$qsub_done" -lt "$ne" ]; do
    ie=1
    while [ "$ie" -le "$ne" ]; do
     cal_ensemble_prefix ${ie}
     prefix=${out_prefix}    
     cd $DIR_ANA/${start_time_main}/mem_${prefix}/
     if [ -f rsl.error.0000  ]; then
      check_qsub=`tail rsl.error.0000 | tail -n 1 | awk '{print $5}'`
      if [ "${check_qsub}" == "COMPLETE" ]; then
       qsub_done=$(($qsub_done+1))
      else
       qsub_done=0
      fi
     else
      check_qsub="NO RSL.ERROR.0000"
      qsub_done=0
     fi
     echo " MAIN: check member $ie returns: ($check_qsub) and ($qsub_done)" >> $DIR_RUN/log.qsub
     ie=$(($ie+1))
    done
    sleep 2m
    qsub_count=$(($qsub_count+1))
    if [ "$qsub_count" -eq 1000 ]; then
     echo " MAIN: qsub is running for too long...something wrong, exit"
     exit 0
    fi
   done
   ie=1
   while [ "$ie" -le "$ne" ]; do
    cal_ensemble_prefix ${ie}
    prefix=${out_prefix}
    cd $DIR_ANA/${start_time_main}/mem_${prefix}/
    if [ "$ie" -eq "1" ]; then
     if [ -d $DIR_FSC/${start_time_main} ]; then
      rm -rf $DIR_FSC/${start_time_main}
     fi
     mkdir -p $DIR_FSC/${start_time_main}
     mkdir -p $DIR_BGD/${restart_dir}
    fi
    mkdir -p $DIR_FSC/${start_time_main}/mem_${prefix}
    mv wrfout_* $DIR_FSC/${start_time_main}/mem_${prefix}
    cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
    ie=$(($ie+1))
   done 
   cd $DIR_RUN
  fi
#
# update new time for the next cycle, based on restart value and loop option
#  
  data_year_main="$E_YEAR"
  data_month_main="$E_MONTH"
  data_day_main="$E_DAY"
  data_hour_main="$E_HOUR"
  data_minute_main="$E_MINUTE"
  start_time_main=${data_year_main}${data_month_main}${data_day_main}${data_hour_main}${data_minute_main}
  iloop=$(($iloop + 1))
  rm -rf wrfbdy* wrfinput* wrfout* 
 done
elif [ $runmode == "EXP_IDEAL" ]; then
#
# call first WPS to create a control initial condition
#
 rm -f wrfinput_d0* wrfbdy_d0* rsl* error.status
 ./runwps.sh ${start_time_main} ${fsct_length_main} 
 if [ -f error.status ]; then
  rm -f error.status
 else
  echo " MAIN: runwps.sh has abrupt die...exit 3"
  exit 3
 fi
#
# call real program now to create wrfinput_d01 (dont need  
# wrfbdy_d01 for EXP_IDEAL mode
#
 ./runreal.sh ${start_time_main} ${fsct_length_main} EXP_REAL 
 if [ -f error.status ]; then
  rm -f error.status
 else
  echo " MAIN: runreal.sh for idealizing has abrupt die...exit 4"
  exit 4
 fi
 ln -sf $DIR_ANA/${start_time_main}/wrfinput_d0* ./
 ln -sf $DIR_ANA/${start_time_main}/wrfbdy_d0* ./
#
# create a cold start for an ensemble of wrf input 
#
 if [ $cold_start == "COLD" ]; then
  if [ -f error.status ]; then
   rm -f error.status
  fi
  rm -f rsl*
  ./runcolds.sh ${start_time_main} ${fsct_length_main} ${ne} ${runmode}
  if [ -f error.status ]; then
   rm -f error.status
  else
   echo " MAIN: runcolds.sh has abrupt die...exit 5"
   exit 5
  fi  
  cold_start="WARM"
 fi 
 iloop=1
 while [ "$iloop" -le "$nloop" ]; do
  echo " " 
  echo " "
  echo " ============================================================================================= "
  echo " MAIN: running EXP_IDEAL for cycle ${start_time_main} at loop ${iloop}"
  rm -f $DIR_RUN/error.status
#
# calling the letkf core now
#
  cd $DIR_LETKF
  ln -sf $DIR_RUN/namelist.letkf ./
  if [ $cold_start == "COLD" ]; then
   cold_start="WARM"
  else
   if [ -d $DIR_ANA/${start_time_main} ]; then
    ./runletkf.sh ${start_time_main} ${ne}
   else 
    mkdir -p $DIR_ANA/${start_time_main}
    ./runletkf.sh ${start_time_main} ${ne}
   fi 
   if [ -f $DIR_RUN/error.status ]; then
    rm -f $DIR_RUN/error.status
   else
    echo " MAIN: runletkf.sh has abrupt die...exit 0"
    exit 0
   fi
  fi 
#
# running wrf model now with all members
#
  cd $DIR_RUN
  . ./func_cal_time.sh
  . ./func_cal_ensemble.sh
  cal_time ${data_year_main} ${data_month_main} ${data_day_main} ${data_hour_main} ${data_minute_main} 0 0 ${restart} 
  restart_file=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
  restart_dir=${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}${E_MINUTE}
  echo " MAIN: restart dir is $restart_dir"
  echo " MAIN: restart file is $restart_file"
  ie=1
  while [ "$ie" -le "$ne" ]; do
#
# linking analysis input now
#
   if [ "$ie" -lt 10 ]; then
    prefix="00${ie}"
   elif [ "$ie" -lt 100 ]; then
    prefix="0${ie}"
   elif [ "$ie" -lt 1000 ]; then
    prefix="${ie}"
   else
    echo " MAIN: Too many members...exit 10"
    exit 10
   fi
   rm -f wrfinput_d0* 
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
   if [ "$model_opt" == "YES" ]; then
    lwradiation=$(($lwradiation+1))
    if [ "$lwradiation" -gt "2" ]; then
     lwradiation=1
     cu_opt=$(($cu_opt+1))
     if [ "$cu_opt" -gt "2" ]; then
      cu_opt=1
      mcphysics=$(($mcphysics+1))
      if [ "$mcphysics" -gt "7" ]; then
       mcphysics=1
      fi
     fi
    fi
   fi
   ./nlist_wrf.sh ${start_time_main} $fsct_length_main $mcphysics $lwradiation $cu_opt $runmode > log.namelist
   mv namelist.input_${mcphysics} $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input
#
# create qsub for each member
#
cat > wrf_qsub.sh << EOF1
#!/bin/sh
#$ -N rsl_qsub
#$ -cwd
#$ -j y
#$ -pe mpich 8
#$ -S /bin/bash
echo "Got \$NSLOTS processors."
echo "Machines:"
echo "\$TMPDIR/machines"
echo "node file is \$HOSTNAME"
cat \$TMPDIR/machines
/state/partition1/home/kieuc/opt/mpich1-pgi/bin/mpirun -np \$NSLOTS ./wrf.exe
rm -f *.exe *.TBL *DATA* ozone* *.tbl grib* co2_trans*
EOF1
   mv wrf_qsub.sh $DIR_ANA/${start_time_main}/mem_${prefix}/
   ie=$(($ie+1))
  done
#
# option for submit qsub job for all members simultaneously or serially
#
  ie=1
  while [ "$ie" -le "$ne" ]; do
   cal_ensemble_prefix ${ie}
   prefix=${out_prefix}
   if [ "$WRF_QSUB" == "NO"  ]; then
    rm -f wrfinput_d0* wrfbdy_d0*
    ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
    ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfbdy_d0* ./
    ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input ./namelist.input
#
# running WRF core serially
#
    echo " MAIN: running wrf.exe for member $ie ..."
    rm -f rsl*
    if [ "$MPIRUN" == "yes" ]; then
     $runwrfmpi >& log.wrf
    else
     ./wrf.exe >& log.wrf
    fi
    if [ "$ie" -eq "1" ]; then
     if [ -d $DIR_FSC/${start_time_main} ]; then
      rm -rf $DIR_FSC/${start_time_main}
     fi
     mkdir -p $DIR_FSC/${start_time_main}
     mkdir -p $DIR_BGD/${restart_dir}
    fi
    mkdir -p $DIR_FSC/${start_time_main}/mem_${prefix}
    mv wrfout_* $DIR_FSC/${start_time_main}/mem_${prefix}
    cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
   else
    cd $DIR_ANA/${start_time_main}/mem_${prefix}/
    ln -sf $DIR_WRF/run/*.exe ./
    ln -sf $DIR_WRF/run/*.TBL ./
    ln -sf $DIR_WRF/run/*DATA* ./
    ln -sf $DIR_WRF/run/ozone* ./
    ln -sf $DIR_WRF/run/*.tbl ./
    ln -sf $DIR_WRF/run/grib* ./
    ln -sf $DIR_WRF/run/co2_trans* ./
    qsub wrf_qsub.sh >> $DIR_RUN/log.qsub
    cd $DIR_RUN
   fi
   ie=$(($ie+1))
  done
#
# for qsub mode, check if all run finish normally
#
  if [ "$WRF_QSUB" == "YES" ]; then
   qsub_done=0
   qsub_count=0
   while [ "$qsub_done" -lt "$ne" ]; do
    ie=1
    while [ "$ie" -le "$ne" ]; do
     cal_ensemble_prefix ${ie}
     prefix=${out_prefix}
     cd $DIR_ANA/${start_time_main}/mem_${prefix}/
     if [ -f rsl.error.0000  ]; then
      check_qsub=`tail rsl.error.0000 | tail -n 1 | awk '{print $5}'`
      if [ "${check_qsub}" == "COMPLETE" ]; then
       qsub_done=$(($qsub_done+1))
      else
       qsub_done=0
      fi
     else
      check_qsub="NO RSL.ERROR.0000"
      qsub_done=0
     fi
     echo " MAIN: check member $ie returns: ($check_qsub) and ($qsub_done)" >> $DIR_RUN/log.qsub
     ie=$(($ie+1))
    done
    sleep 2m
    qsub_count=$(($qsub_count+1))
    if [ "$qsub_count" -eq 1000 ]; then
     echo " MAIN: qsub is running for too long...something wrong, exit"
     exit 0
    fi
   done
   ie=1
   while [ "$ie" -le "$ne" ]; do
    cal_ensemble_prefix ${ie}
    prefix=${out_prefix}
    cd $DIR_ANA/${start_time_main}/mem_${prefix}/
    if [ "$ie" -eq "1" ]; then
     if [ -d $DIR_FSC/${start_time_main} ]; then
      rm -rf $DIR_FSC/${start_time_main}
     fi
     mkdir -p $DIR_FSC/${start_time_main}
     mkdir -p $DIR_BGD/${restart_dir}
    fi
    mkdir -p $DIR_FSC/${start_time_main}/mem_${prefix}
    mv wrfout_* $DIR_FSC/${start_time_main}/mem_${prefix}
    cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
    ie=$(($ie+1))
   done
   cd $DIR_RUN
  fi
#
# update new time for the next cycle, based on restart value and loop option
#  
  data_year_main="$E_YEAR"
  data_month_main="$E_MONTH"
  data_day_main="$E_DAY"
  data_hour_main="$E_HOUR"
  data_minute_main="$E_MINUTE"
  start_time_main=${data_year_main}${data_month_main}${data_day_main}${data_hour_main}${data_minute_main}
  iloop=$(($iloop + 1))
  rm -rf wrfbdy* wrfinput* wrfout* 
 done
elif [ $runmode == "EXP_REAN" ]; then
 iloop=1
 while [ "$iloop" -le "$nloop" ]; do
  echo " " 
  echo " "
  echo " ============================================================================================= "
  echo " MAIN: running EXP_REAN at cycle ${start_time_main} at loop ${iloop}"
  rm -f $DIR_RUN/error.status
  fsct_length_rean=$(($restart/60))
#
# call first WPS to create a control initial condition
#
  rm -f wrfinput_d0* wrfbdy_d0* rsl* error.status
  ./runwps.sh ${start_time_main} ${fsct_length_main} 
  if [ -f error.status ]; then
   rm -f error.status
  else
   echo " MAIN: runwps.sh has abrupt die...exit 3"
   exit 3
  fi
#
# call real program now to create wrfinput_d01 (dont need  
#
  ./runreal.sh ${start_time_main} ${fsct_length_main} ${runmode} 
  if [ -f error.status ]; then
   rm -f error.status
  else
   echo " MAIN: runreal.sh for idealizing has abrupt die...exit 4"
   exit 4
  fi
  ln -sf $DIR_ANA/${start_time_main}/wrfinput_d0* ./
  ln -sf $DIR_ANA/${start_time_orig}/wrfbdy_d0* ./
#
# create a cold start for an ensemble of wrf input 
#
  if [ $cold_start == "COLD" ]; then
   rm -f rsl* error.status
   ./runcolds.sh ${start_time_main} ${fsct_length_main} ${ne} ${runmode}
   if [ -f error.status ]; then
    rm -f error.status
   else
    echo " MAIN: runcolds.sh has abrupt die...exit 5"
    exit 5
   fi  
   cold_start="WARM"
  fi 
#
# calling the letkf core now
#
  cd $DIR_LETKF
  ln -sf $DIR_RUN/namelist.letkf ./
  rm -f wrfinput_d0* wrfbdy_d0*
  ln -sf $DIR_RUN/wrfinput_d0* ./
  ln -sf $DIR_RUN/wrfbdy_d0* ./  
  if [ $cold_start == "COLD" ]; then
   cold_start="WARM"
  else
   if [ -d $DIR_ANA/${start_time_main} ]; then
    ./runletkf.sh ${start_time_main} ${ne}
   else 
    mkdir -p $DIR_ANA/${start_time_main}
    ./runletkf.sh ${start_time_main} ${ne}
   fi 
   if [ -f $DIR_RUN/error.status ]; then
    rm -f $DIR_RUN/error.status
   else
    echo " MAIN: runletkf.sh has abrupt die...exit 0"
    exit 0
   fi
  fi 
#
# running wrf model now with all members
#
  cd $DIR_RUN
  . ./func_cal_time.sh
  cal_time ${data_year_main} ${data_month_main} ${data_day_main} ${data_hour_main} ${data_minute_main} 0 0 ${restart} 
  restart_file=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
  restart_dir=${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}${E_MINUTE}
  echo " MAIN: restart dir is $restart_dir"
  echo " MAIN: restart file is $restart_file"
  ie=1
  while [ "$ie" -le "$ne" ]; do
#
# linking analysis input now
#
   if [ "$ie" -lt 10 ]; then
    prefix="00${ie}"
   elif [ "$ie" -lt 100 ]; then
    prefix="0${ie}"
   elif [ "$ie" -lt 1000 ]; then
    prefix="${ie}"
   else
    echo " MAIN: Too many members...exit 10"
    exit 10
   fi
   rm -f wrfinput_d0*  wrfbdy_d0*
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfbdy_d0* ./
   if [ "$model_opt" == "YES" ]; then
    lwradiation=$(($lwradiation+1))
    if [ "$lwradiation" -gt "2" ]; then
     lwradiation=1
     cu_opt=$(($cu_opt+1))
     if [ "$cu_opt" -gt "2" ]; then
      cu_opt=1
      mcphysics=$(($mcphysics+1))
      if [ "$mcphysics" -gt "7" ]; then
       mcphysics=1
      fi
     fi
    fi
   fi
   ./nlist_wrf.sh ${start_time_main} $fsct_length_main $mcphysics $lwradiation $cu_opt $runmode > log.namelist
   ln -sf namelist.input_${mcphysics} ./namelist.input
   echo " MAIN: running wrf.exe for member $ie ..."
   rm -f rsl*
   if [ "$MPIRUN" == "yes" ]; then
    $runwrfmpi >& log.wrf
   else
    ./wrf.exe >& log.wrf
   fi
   if [ "$ie" -eq "1" ]; then
    if [ -d $DIR_FSC/${start_time_main} ]; then
     rm -rf $DIR_FSC/${start_time_main}
    fi
    mkdir -p $DIR_FSC/${start_time_main}
    mkdir -p $DIR_BGD/${restart_dir}
   fi
   mkdir -p $DIR_FSC/${start_time_main}/mem_${prefix}
   mv wrfout_* $DIR_FSC/${start_time_main}/mem_${prefix}
   cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file} 
   ie=$(($ie+1))
  done
#
# update new time for the next cycle, based on restart value and loop option
#  
  data_year_main="$E_YEAR"
  data_month_main="$E_MONTH"
  data_day_main="$E_DAY"
  data_hour_main="$E_HOUR"
  data_minute_main="$E_MINUTE"
  start_time_main=${data_year_main}${data_month_main}${data_day_main}${data_hour_main}${data_minute_main}
  iloop=$(($iloop + 1))
  rm -rf wrfbdy* wrfinput* wrfout* 
 done
elif [ $runmode == "REAL_TIME" ]; then
 echo " " 
 echo "#################################################################"
 echo "#                                                               #"
 echo "#            THE REAL TIME WRF-LETKF SYSTEM V1.9.2              #"
 echo "#          I. M. SYSTEM GROUP INC COMMERCIAL VERSION            #"
 echo "#          3206 OAK TOWER, ROCKVILLE, MARYLAND - USA            #"
 echo "#                   HANOI - Copyright 2011                      #"
 echo "#                                                               #"
 echo "#################################################################"
 echo " "
 echo " MAIN: Starting time for REAL_TIME is: $start_time_main" 
 echo " MAIN: cold_start = $cold_start"
#
# run first WPS code
#
 rm -f error.status
 ./runwps.sh ${start_time_main} ${fsct_length_main} 
 if [ -f error.status ]; then
  rm -f error.status
 else
  echo " MAIN: runwps.sh has abrupt die...exit 3"
  exit 3
 fi
#
# calling next the WRF real to create a control boundary and initial condition
#
 rm -f wrfinput_d0* wrfbdy_d0* rsl*
 ./runreal.sh ${start_time_main} ${fsct_length_main} ${runmode}
 if [ -f error.status ]; then
  rm -f error.status
 else
  echo " MAIN: runreal.sh has abrupt die...exit 4"
  exit 4
 fi
 ln -sf $DIR_ANA/${start_time_main}/wrfinput_d0* ./
 ln -sf $DIR_ANA/${start_time_main}/wrfbdy_d0* ./
#
# create a cold start for an ensemble of wrf input 
#
 if [ $cold_start == "COLD" ]; then
  cd $DIR_INI
  ln -sf $DIR_RUN/wrfinput_d0* ./
  ./runini.sh ${start_time_main} ${ne} > $DIR_RUN/log.ini 
  if [ -f error.status ]; then
   rm -f error.status
  else
   echo " MAIN: runini.sh has abrupt die...exit 5"
   exit 5
  fi
  mkdir -p $DIR_BGD/${start_time_main}
  mv bgd_* $DIR_BGD/${start_time_main}/
  cp $DIR_RUN/wrfbdy_d01 $DIR_BGD/${start_time_main}/
  cd $DIR_RUN
 fi 
#
# prepare observation next by calling WRF-3DVAR to create obs files
#
 cd $DIR_OBS
 ./runobs.sh ${start_time_main} 0 > $DIR_RUN/log.runobs 
 cd $DIR_RUN
#
# calling the letkf core now
#
 cd $DIR_LETKF
 ln -sf $DIR_RUN/namelist.letkf ./
 rm -f wrfinput_d0* wrfbdy_d0*
 ln -sf $DIR_RUN/wrfinput_d0* ./
 ln -sf $DIR_RUN/wrfbdy_d0* ./
 ./runletkf.sh ${start_time_main} ${ne}
 if [ -f $DIR_RUN/error.status ]; then
  rm -f $DIR_RUN/error.status
 else
  echo " MAIN: runletkf.sh has abrupt die...exit 5"
  exit 6
 fi
#
# update boundary and create namelist for each member
#
 cd $DIR_RUN
 . ./func_cal_time.sh
 . ./func_cal_ensemble.sh
 cal_time ${data_year_main} ${data_month_main} ${data_day_main} ${data_hour_main} ${data_minute_main} 0 0 ${restart}
 restart_file=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
 restart_dir=${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}${E_MINUTE}
 ie=1
 while [ "$ie" -le "$ne" ]; do
  cal_ensemble_prefix ${ie}
  prefix=${out_prefix}
#
# link the analysis
#
  rm -f wrfinput_d0* wrfbdy_d0*
  ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
  ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfbdy_d0* ./
#
# update boundary condition now for each member. Note that link to wrfbdy_d01
# will be overwritten and each member's wrfbdy_d01 will be modified with
# this step.
#
  ln -sf $DIR_ANA/${start_time_main}/wrfinput_d01 ./wrfinput_d01_orig
  ln -sf ../model/WRFDA/var/test/update_bc/parame.in ./
  ln -sf ../model/WRFDA/var/build/da_update_bc.exe ./
  ./da_update_bc.exe > log.update_bc
  if [ -f fort.11 ]; then
    rm -f fort*
  else
    echo " MAIN: da_update_bc.exe has abrupt die...exit 7"
    exit 7
  fi
#
# option for multiple physics options
#
   if [ "$model_opt" == "YES" ]; then
    lwradiation=$(($lwradiation+1))
    if [ "$lwradiation" -gt "2" ]; then
     lwradiation=1
     cu_opt=$(($cu_opt+1))
     if [ "$cu_opt" -gt "2" ]; then
      cu_opt=1
      mcphysics=$(($mcphysics+1))
      if [ "$mcphysics" -gt "7" ]; then
       mcphysics=1
      fi
     fi
    fi
   fi
   ./nlist_wrf.sh ${start_time_main} $fsct_length_main $mcphysics $lwradiation $cu_opt $runmode > log.namelist
  mv namelist.input_${mcphysics} $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input
#
# create qsub for each member
#
cat > wrf_qsub.sh << EOF1
#!/bin/sh
#$ -N rsl_qsub 
#$ -cwd
#$ -j y
#$ -pe mpich 8
#$ -S /bin/bash
echo "Got \$NSLOTS processors."
echo "Machines:"
echo "\$TMPDIR/machines"
echo "node file is \$HOSTNAME"
cat \$TMPDIR/machines
/state/partition1/home/kieuc/opt/mpich1-pgi/bin/mpirun -np \$NSLOTS ./wrf.exe
rm -f *.exe *.TBL *DATA* ozone* *.tbl grib* co2_trans*
EOF1
  mv wrf_qsub.sh $DIR_ANA/${start_time_main}/mem_${prefix}/
  ie=$(($ie+1))
 done
#
# option for submit qsub job for all members simultaneously or serially
#
 ie=1
 while [ "$ie" -le "$ne" ]; do
  cal_ensemble_prefix ${ie}
  prefix=${out_prefix}
  if [ "$WRF_QSUB" == "NO"  ]; then
   rm -f wrfinput_d0* wrfbdy_d0*
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfbdy_d0* ./
   ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input ./namelist.input
#
# running WRF core serially
#
   echo " MAIN: running wrf.exe for member $ie ..."
   rm -f rsl*
   if [ "$MPIRUN" == "yes" ]; then
    $runwrfmpi >& log.wrf
   else
    ./wrf.exe >& log.wrf
   fi
   if [ "$ie" -eq "1" ]; then
    if [ -d $DIR_FSC/${start_time_main} ]; then
     rm -rf $DIR_FSC/${start_time_main}
    fi
    mkdir -p $DIR_FSC/${start_time_main}
    mkdir -p $DIR_BGD/${restart_dir}
   fi
   mkdir -p $DIR_FSC/${start_time_main}/mem_${prefix}
   mv wrfout_* $DIR_FSC/${start_time_main}/mem_${prefix}
   cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
  else
   cd $DIR_ANA/${start_time_main}/mem_${prefix}/
   ln -sf $DIR_WRF/run/*.exe ./
   ln -sf $DIR_WRF/run/*.TBL ./
   ln -sf $DIR_WRF/run/*DATA* ./
   ln -sf $DIR_WRF/run/ozone* ./
   ln -sf $DIR_WRF/run/*.tbl ./
   ln -sf $DIR_WRF/run/grib* ./
   ln -sf $DIR_WRF/run/co2_trans* ./
   qsub wrf_qsub.sh >> $DIR_RUN/log.qsub
   cd $DIR_RUN
  fi
  ie=$(($ie+1))
 done
#
# for qsub mode, check if all run finish normally
#
 if [ "$WRF_QSUB" == "YES" ]; then
  echo " MAIN: check for qsub output"
  qsub_done=0
  qsub_count=0
  while [ "$qsub_done" -lt "$ne" ]; do
   ie=1
   while [ "$ie" -le "$ne" ]; do
    cal_ensemble_prefix ${ie}
    prefix=${out_prefix}
    cd $DIR_ANA/${start_time_main}/mem_${prefix}/
    if [ -f rsl.error.0000  ]; then 
     check_qsub=`tail rsl.error.0000 | tail -n 1 | awk '{print $5}'`
     if [ "${check_qsub}" == "COMPLETE" ]; then
      qsub_done=$(($qsub_done+1))
     else
      qsub_done=0
     fi
    else
     check_qsub="NO RSL.ERROR.0000"
     qsub_done=0
    fi
    echo " MAIN: check member $ie returns: ($check_qsub) and ($qsub_done)" >> $DIR_RUN/log.qsub
    ie=$(($ie+1))
   done
   sleep 2m
   qsub_count=$(($qsub_count+1))
   if [ "$qsub_count" -eq 1000 ]; then
    echo " MAIN: qsub is running for too long...something wrong, exit"
    exit 0
   fi
  done
  ie=1
  while [ "$ie" -le "$ne" ]; do
   cal_ensemble_prefix ${ie}
   prefix=${out_prefix}
   cd $DIR_ANA/${start_time_main}/mem_${prefix}/
   if [ "$ie" -eq "1" ]; then
    if [ -d $DIR_FSC/${start_time_main} ]; then
     rm -rf $DIR_FSC/${start_time_main}
    fi
    mkdir -p $DIR_FSC/${start_time_main}
    mkdir -p $DIR_BGD/${restart_dir}
   fi
   mkdir -p $DIR_FSC/${start_time_main}/mem_${prefix}
   mv wrfout_* $DIR_FSC/${start_time_main}/mem_${prefix}
   cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
   ie=$(($ie+1))
  done
  cd $DIR_RUN
 fi
 rm -f wrfbdy* wrfinput* wrfout*
else
 echo " MAIN: RUNMODE = $runmode is not supported...exit 2"
 exit 2
fi
#
# doing some diagnostic calculations
#
#if [ "$runmode" == "REAL_TIME" ]; then
# cd $DIR_POST
# ./runwpost.sh ${start_time_main} ${ne} > log.wpost
# cd $DIR_RUN 
#else	
# cd $DIR_DIG 
# ./runrms.sh ${start_time_orig} ${fsct_length_main} $(($restart/60)) > log.rms
# cd $DIR_RUN
#fi
#
# clean all links finally
#
rm -rf wrfbdy* wrfinput* wrfout* namelist* CAM* *.TBL RRT* ETAMP* clean Vtable ungrib* ndown*
rm -rf tr4* geo* fsc_* README* *.tbl *.txt config* tr6* metgrid* ozone* nup* compile arch error*
ln -sf ../namelist.letkf ./
echo ""
echo " MAIN script has finished normally     "
echo " MAIN script has finished normally     " > error.status
echo ""

mail -s "wrf-letkf successful report at $( date )" hoangmaik52dubao@gmail.com < main.log

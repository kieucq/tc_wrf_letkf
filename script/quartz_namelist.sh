#!/bin/bash
#
# NOTE: this included namelist script is for
#       1) set up namelist for TC forecast; and 
#       2) check and filtering the TC vital record, which
#          will be used for setting up a nested domain when a storm warning is
#          issued. The program will use some obs information to find the lat/lon
#          for the lower left corner of the nested domain  
#
# HIST: - Jun 18, 2011: created by CK
#       - Apr 17, 2022: added option for 1 dom with best track bdeck 
#       - Apr 18, 2022: added more notes for WRF/LETKF namelist clarification.
#
# AUTHOR: Chanh Kieu (chanhkq@vnu.edu.vn), Vietnam National University
#============================================================================
#
# 0. Setup the WRF-LETKF configuration and MPI options. Note that the runmode 
#    must take one of the following options:
#    - EXP_REAL: real GFS input data with TC vital and vortex initialization
#    - EXP_IDEAL: idealized bogus vortex implant (bogus vortex /= vortex ini)
#    - REAL_TIME: general weather prediction. Need to be called from a master script
#    - EXP_REAN: general weather prediction with reanalysis DA in terms of data columns
#
#set -x
flag_tc=1                                            # flag for TC forecast (1) or normal weather forecast (0)
WRF_QSUB="YES"                                       # option for scheduler or not
runmode="EXP_REAL"                                   # mode of running EXP_REAL, EXP_IDEAL, REAL_TIME, EXP_REAN
vort_init="YES"                                      # option for the running bogus vortex assimilation
SPINUP="NO"                                         # option for spinning cold start members with 1-hr run with DFI opt
blend_obs="NO"                                      # option for blending bogus vortex and CIMSS data
dacycle=720                                          # DA cycle in minute
wrfout_interval=360                                  # WRF output interval in minute (must < dacycle)
nesemble=21                                          # number of ensemble members
obsmode="CIMSS"                                      # default obs mode for weather forecast. Overided by flag_tc opt
                                                     # BVOX, radioson(RADS), radar(RADA), reanaly(REAN), CIMSS, or CONTROL
cold_start="COLD"                                    # default cold start option. will be overided by flag_tc opt
damode="LETKF"                                       # option for LETKF or 3DVAR (3DVAR script is runmain3dvar.sh)
npass=1                                              # option for DFI smoothing with SPINUP = YES 
NCORE=8                                              # number of core for the parallel run
njob_max=100                                         # max number of job submission at one time
MPIRUN="yes"                                         # option for parallel run or not
runwrfmpi="srun -n 16 ./wrf.exe"                     # template for mpirun
#runwrfmpi="mpirun -np 2 -machinefile mywrf ./wrf.exe"
#
# 1. Setup default WRF namelist for the weather mode. The actual datetime for 
# REAL_TIME/EXP_REAL mode will be passed from the arguments.
#
ratio_12=3                                           # ratio of domain 1 over 2  
d01_year="2016"                                      # start year for domain 1
d01_month="10"                                       # start month for domain 1
d01_day="10"                                         # start day for domain 1
d01_hour="00"                                        # start hour for domain 1
d01_minute="00"                                      # start minute for domain 1
d02_year=$d01_year                                   # start year for domain 2
d02_month="10"                                       # start month for domain 2
d02_day="10"                                         # start day for domain 2
d02_hour="00"                                        # start hour for domain 2
d02_minute="00"                                      # start minute for domain 2
d03_year=$d01_year                                   # start year for domain 3
d03_month="10"                                       # start month for domain 3
d03_day="10"                                         # start day for domain 3
d03_hour="00"                                        # start hour for domain 3
d03_minute="00"                                      # start minute for domain 3
fsct_length_main=120                                 # forecast length in hours
ncycle=1                                             # default number of DA cycles
#
# 2. Setup domain for the WRF job. Note that these default opts are for the normal weather forecast.
# For the TC confguration, please go down for more information on the TC forecast settings
#
d01_nx=151                                           # number of grid point in x-dim for WRF namelist
d01_ny=151                                           # number of grid point in y-dim for WRF namelist
d01_nz=31                                            # number of grid point in z-dim for WRF namelist         
d01_dx=36000                                         # dx for domain 1 in m
d01_dy=36000                                         # dy for domain 1 in m
d01_dt=180                                           # dt for domain 1 in second
d02_nx=199                                           # number of grid point in x-dim for domain 2
d02_ny=199                                           # number of grid point in y-dim for domain 2
d02_nz=$d01_nz                                       # number of grid point in z-dim for domain 2
d02_dx=$(($d01_dx/$ratio_12))                        # dx for domain 2 in m
d02_dy=$(($d01_dy/$ratio_12))                        # dy for domain 2 in m
d02_dt=$(($d01_dt/$ratio_12))                        # dt for domain 2 in second
d02_istart=66                                        # start i lower left of domain 2
d02_jstart=66                                        # start j lower left of domain 2
d03_nx=199                                           # number of grid point in x-dim for domain 3
d03_ny=199                                           # number of grid point in y-dim for domain 3
d03_nz=$d01_nz                                       # number of grid point in z-dim for domain 3
d03_dx=$(($d02_dx/$ratio_12))                        # dx for domain 3 in m
d03_dy=$(($d02_dy/$ratio_12))                        # dy for domain 3 in m
d03_dt=$(($d02_dt/$ratio_12))                        # dt for domain 3 in second
d03_istart=25                                        # start i lower left of domain 3
d03_jstart=100                                       # start j lower left of domain 3
ref_lat="20"                                         # reference lat for the domain center (mercator proj)
ref_lon="95"                                         # reference lon for the domain center (mercator proj)
corr_dist=5                                          # distance to the bnd to control movement   
maximum_dom=3                                        # default max dom for normal weather forecast
#
# 3. Reset WRF date/time namelist for REAL_TIME mode for TC forecast
#
time_input=${d01_year}${d01_month}${d01_day}${d01_hour}${d01_minute}
if [ "${flag_tc}" == "1" ]; then
    echo " NAMELIST: RUN A TC FORECAST CONFIGURATION NOW"
    echo " NAMELIST: =========================================="
    if [ $# == 7 ]; then
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
        cold_start=$4                                      # cold start option (Y/N)
        obsmode=$5                                         # obs mode CIMSS/REAN/RADS
        storm_id=$6                                        # storm ID for TC forecast
        ncycle=$7                                          # number of DA cycles
    else
        echo " NAMELIST: quartz_tc_runmain.sh needs arguments for TC forecast mode flag_tc = 1"
        echo " ./tc_runmain.sh YYYYMMDDHHMM forecast_time bdy_interval cold_start obs_mode stormid ncycle"
        echo "   E.g. running storm 18W:   ./tc_runmain.sh 201207101200 120 6 COLD CIMSS 18W 1"
        echo "   or running warm start:    ./tc_runmain.sh 201207101200 120 6 WARM CIMSS 18W 1"
        echo "   or running with sonde:    ./tc_runmain.sh 202108280000 120 6 WARM RADS 09L 1"
        echo "   or running ensemble only: ./tc_runmain.sh 201207101200 120 6 COLD CONTROL 99W 1"
        echo " NAMELIST: exit 1"
        exit 1
    fi
elif [ "${flag_tc}" == "0" ] && [ $# == 5 ]; then
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
    fsct_length_main=$2                                    # forecast length in hours
    interval_bnd=$(($3*3600))                              # interval updating the boundary for WPS in second
    cold_start=$4                                          # cold start option (Y/N)
    obsmode=$5                                             # obs mode CIMSS/REAN/RADS
    echo ""
    echo " NAMELIST: RUN A WEATHER FORECAST CONFIGURATION NOW"
    echo " NAMELIST: ============================================="
    echo " NAMELIST: double check the weather settings in the namelist script"
    echo " NAMELIST: flag_tc = 0"
    echo " NAMELIST: obsmode = $obsmode"
    echo " NAMELIST: fsct_length_main = $fsct_length_main"
    echo " NAMELIST: time_input = $time_input"
    echo " NAMELIST: interval_bnd = $interval_bnd"
    echo " NAMELIST: maximum_dom = $maximum_dom" 
    echo " NAMELIST: DFI option SPINUP = $SPINUP"
    echo " NAMELIST: DA blending option is blend_obs = $blend_obs"
    echo " NAMELIST: vort_init option must be NO vs. $vort_init"
else
    echo " NAMELIST: Incorrect argument for quartz_tc_main.sh... E.g."
    echo " NAMELIST: flag_tc=1: ./tc_runmain.sh 201207101200 120 6 COLD CIMSS 18W 1"
    echo " NAMELIST: flag_tc=0: ./tc_runmain.sh 201207101200 120 6 COLD CIMSS"
    echo " NAMELIST: Exit now ..."
    exit 1
fi
if [ "$runmode" = "REAL_TIME" ]; then
    DIR_GRIB_DATA="${DIR_MAN}/data/gfs/${d01_year}${d01_month}${d01_day}${d01_hour}${d01_minute}"
else
    DIR_GRIB_DATA="${DIR_MAN}/data/avn/${d01_year}${d01_month}${d01_day}${d01_hour}${d01_minute}"
fi
echo " NAMELIST: running main time input is: $time_input"
#
# Search for the storm at the corresponding time to see if it has any record in tcvital_syndrat.dat
# for TC forecast mode
#
if [ "${flag_tc}" == "1" ]; then
    if [ -f ../tcvital/vital_record.txt ]; then
        echo " NAMELIST: storm $storm_id has the vital_record already... move on"
        size=`ls -l ../tcvital/vital_record.txt | awk '{print $5}'`
        if [ "$size" -lt "10" ]; then
            echo " NAMELIST: the ../tcvital/vital_record.txt file is empty...quit 1"
            exit 1
        fi
    else
        if [ "$storm_id" != "99W" ]; then
            echo " NAMELIST: storm $storm_id does not have vital record... searching"
            cat ../tcvital/syndat_tcvitals.${d01_year} | awk -v sid=$storm_id '{if($2==sid) print $0}' | \
            grep ${d01_year}${d01_month}${d01_day} | grep "${d01_hour}00" | head -n 1 > ../tcvital/vital_record.txt
        fi
        size=`ls -l ../tcvital/vital_record.txt | awk '{print $5}'`
        if [ "$size" -lt "10" ]; then
            echo " NAMELIST: the ../tcvital/vital_record.txt file is still empty or run 99W...exit 2"
            exit 2
        fi
    fi
fi
#
# check if the vital record file is existing and read the vital record to create a wrf namelist for this
# mode.
#
if [ "$flag_tc" == "1" ]; then
    size=`ls -l ../tcvital/vital_record.txt | awk '{print $5}'`
    if [ "$size" -lt "10" ]; then
        echo " NAMELIST: the ../tcvital/vital_record.txt file must be empty...exit 3"
        exit 3
    else
        echo " NAMELIST: ../tcvital/vital_record pre-exists. Replace this pre-existing now"
        #
        # extract information of the TC center from the vital
        #
        vital_time=`cat ../tcvital/vital_record.txt  | awk '{print $4""$5}'`
        tc_lat_input=`cat ../tcvital/vital_record.txt  | awk '{print $6}' | sed 's/N/ N/' | sed 's/S/ S/' | \
                awk '{if($2=="N") print $1/10; else print -$1/10}'`
        tc_lon_input=`cat ../tcvital/vital_record.txt  | awk '{print $7}' | sed 's/E/ E/' | sed 's/W/ W/' | \
                awk '{if($2=="E") print $1/10; else print -$1/10}'`
        tc_vmax=`cat ../tcvital/vital_record.txt  | awk '{print $13}'`
        if [ "$tc_vmax" -lt "18" ]; then
            npass=3
        elif [ "$tc_vmax" -lt "50" ] && [ "$tc_vmax" -ge "18" ]; then
            npass=8
        else
            npass=2
        fi
        if [ "$vital_time" -ne "$time_input" ]; then
            echo " NAMELIST: input parameter does not fit vital_time...exit 3"
            echo " NAMELIST:  $vital_time - $time_input "
            exit 3
        else
            echo " NAMELIST: lon of domain center is: $tc_lon_input"
            echo " NAMELIST: lat of domain center is: $tc_lat_input"
            echo " NAMELIST: tc_vmax from tcvital is: $tc_vmax"
        fi
        #
        # set the domain confguration for outer and inner domains for TC simulation
        #
        maximum_dom=3
        d01_nx=151                                           # number of grid point in x-dim for WRF namelist
        d01_ny=151                                           # number of grid point in y-dim for WRF namelist
        d01_nz=31                                            # number of grid point in z-dim for WRF namelist
        d01_dx=36000                                         # dx for domain 1 in m
        d01_dy=36000                                         # dy for domain 1 in m
        d01_dt=180                                           # dt for domain 1 in second
        d02_nx=199                                           # number of grid point in x-dim for domain 2
        d02_ny=199                                           # number of grid point in y-dim for domain 2
        d02_nz=$d01_nz                                       # number of grid point in z-dim for domain 2
        d02_dx=$(($d01_dx/$ratio_12))                        # dx for domain 2 in m
        d02_dy=$(($d01_dy/$ratio_12))                        # dy for domain 2 in m
        d02_dt=$(($d01_dt/$ratio_12))                        # dt for domain 2 in second
        d03_nx=199                                           # number of grid point in x-dim for domain 3
        d03_ny=199                                           # number of grid point in y-dim for domain 3
        d03_nz=$d01_nz                                       # number of grid point in z-dim for domain 3
        d03_dx=$(($d02_dx/$ratio_12))                        # dx for domain 3 in m
        d03_dy=$(($d02_dy/$ratio_12))                        # dy for domain 3 in m
        d03_dt=$(($d02_dt/$ratio_12))                        # dt for domain 3 in second
        ref_lat=$tc_lat_input                                # reference lat for domain center (mercator proj)
        ref_lon=$tc_lon_input                                # reference lon for domain center (mercator proj)
        corr_dist=5
        LLI1=$(($d01_nx/2-$d02_nx/6))
        LLJ1=$(($d01_ny/2-$d02_ny/6))
        d03_istart=$(($d02_nx/2-$d03_nx/6))
        d03_jstart=$(($d02_ny/2-$d03_ny/6))
        tem=`echo "($tc_lon_input - $ref_lon)" | bc -l`
        TX1=`echo "(($tem * 111000) / $d01_dx)" | bc -l`
        tem=`echo "($tc_lat_input - $ref_lat)" | bc -l`
        TY1=`echo "(($tem * 111000) / $d01_dy)" | bc -l`
        RX1=`printf "%.0f\n" $TX1`
        d02_istart=`expr $LLI1 + $RX1`
        RY1=`printf "%.0f\n" $TY1`
        d02_jstart=`expr $LLJ1 + $RY1`
        echo " NAMELIST: LLI1 = $LLI1"
        echo " NAMELIST: LLJ1 = $LLJ1"
        echo " NAMELIST: d02_istart  = $d02_istart"
        echo " NAMELIST: d02_jstart  = $d02_jstart"
        echo " NAMELIST: fsct_length_main = $fsct_length_main"
        echo " NAMELIST: time_input = $time_input"
        echo " NAMELIST: maximum_dom = $maximum_dom"
        echo " NAMELIST: DFI option SPINUP = $SPINUP"
        echo " NAMELIST: DA blending option is blend_obs = $blend_obs"
        echo " NAMELIST: vort_init option must be NO vs. $vort_init"
        echo " NAMELIST: interval_bnd = $interval_bnd"
        echo " NAMELIST: obsmode = $obsmode"
        if [ ${d02_istart} -lt "5" ] || [ ${d02_istart} -gt "153" ]; then
            echo " NAMELIST: the nested domain is not within the mother domain"
            echo " NAMELIST: reset the outer domain inside the TY dir...exit 4"
            exit 4
        fi
        if [ ${d02_jstart} -lt "5" ] || [ ${d02_jstart} -gt "153" ]; then
            echo " NAMELIST: the nested domain is not within the mother domain"
            echo " NAMELIST: reset the outer domain inside the TY dir...exit 4"
            exit 4
        fi
    fi
fi
#
# 5. Setup LETKF namelist and ensemble information
#
interval_out=${wrfout_interval}                      # output interval of the WRF model in minute
nloop=${ncycle}                                      # number of DA cycles
restart=${dacycle}                                   # restart interval for new da cycle in minute
ne=${nesemble}                                       # number of ensemble members
nme=1                                                # number of model error ensemble members
model_flag=1                                         # option for model asign: 0-perfect; 1-imperfect
model_opt="YES"                                      # option for different model physics for model err
mcphysics=1                                          # default physics option
lwradiation=1                                        # default long wave radiation option
cu_opt=1                                             # default cumulus option
infactor="1.0"                                       # inflation factor
tloop=$(($nloop-1))
da_var=5                                             # 0 (all); 1(no u); 2(no v); 3(no T)... see readme
h_local=5                                            # dimension of LETKF horizontal local patch
v_local=3                                            # dimension of LETKF vertical local patch
if [ "$obsmode" == "CIMSS" ]; then
    da_var=345
    v_local=3
fi
#if [ "$runmode" != "REAL_TIME" ]; then
#    fsct_length_main=$(($tloop*$restart/60))        # forecast time in hrs for non real-time experiments
#fi
if [ "$vort_init" == "YES" ] && [ "$blend_obs" == "NO" ] && [ "$SPINUP" == "YES" ]; then
    npass=0                                          # smoothing option for LETKF
fi
#
# set up a namelist for the LETKF
#
if [ -f namelist.letkf ]; then
    mv -f namelist.letkf ./namelist.letkf.backup
fi
cat > ../namelist.letkf << EOF
debug        = 0               ! debuging level
restart      = $dacycle            ! window only for idealized DA exp [mn]
dy           = ${d01_dy}           ! grid distance in the y-direction [m] 
zamp         = 0              ! amplitude of v-anomaly           [m]
zscale       = 1e+5            ! scale of v-anomaly               [m]
icen         = 72              ! i-center of the bogus obs patch  []
jcen         = 72              ! j-center of the bogus obs patch  []
obs_err_u    = 1.2             ! obs error for u                  [m/s]
obs_err_v    = 1.2             ! obs error for v                  [m/s]
obs_err_t    = 1.2             ! obs error magnitude              [m]
obs_err_q    = 1e-3            ! obs error for q                  [kg/kg]
obs_err_p    = 1.2             ! obs error for pressure perp      [hPa]
bgd_err_u    = 1.4             ! background err for u coldstart   [m]
bgd_err_v    = 1.4             ! background err for v coldstart   [m/s]
bgd_err_t    = 1.4             ! background err for T coldstart   [K]
bgd_err_q    = 2e-3            ! background err for q coldstart   [kg/kg]
bgd_err_p    = 2.              ! background err for pp coldstart  [hPa]
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
obs_flag     = ${npass}               ! option for obs output: 0:no smooth, 1:smooth
oscale       = 700e+03         ! radius of obs influence
da_flag      = ${da_var}               ! 0 (all); 1(no u); 2(no v);... see readme
r_obs        = 1              ! radius around(icen,jcen) where bogused obs is created 
mean_flag    = 0               ! (0) short-range fcsts as bgd, (1)GFS update as bgd 
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
#
# cross check of supporting options
#
if [ "$runmode" == "EXP_REAL" ] && [ "$obsmode" == "BVOX" ]; then
    echo " NAMELIST: does not support running bogus vortex with real-time cycle...exit 0"
fi
if [ "$runmode" == "EXP_IDEAL" ] && [ "$obsmode" == "RADS" ]; then
    echo " NAMELIST: does not support running ideal case with radiosondes assimilation...exit 0"
    exit 0
fi
if [ "$damode" != "LETKF" ]; then
    echo " NAMELIST: damode=LETKF is the only opt for runmain.sh ...exit 0"
    exit 0
fi
if [ "$runmode" == "REAL_TIME" ] && [ "$obsmode" != "REAN" ]; then
    echo " NAMELIST: WARNING: REAL_TIME exp is supported with REAN mode only"
fi
ln -sf ../namelist.letkf ./
echo " NAMELIST: done setting up environment"

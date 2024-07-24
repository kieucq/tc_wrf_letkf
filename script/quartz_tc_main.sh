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
#         - Jun 20, 2011: re-designed the code throughly for TC forecast 
#         - Mar 31, 2012: modified to allow for limitted submission of PBS jobs
#                         per run
#         - Mar 10, 2013: added new vortex intialization component to the system
#         - Jan 03, 2014: added the environment export for the runletkf.sh script
#         - Mar 20, 2015: added the new job submitting flows
#         - Apr 17, 2022: Upgraded for the new Carbonate system
#         - Aug 20, 2023: Upgraded for the new system "Quartz"
#
# [AUTHOR]:
#           Chanh Kieu, Indiana University (ckieu@indiana.edu)
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
# [SCHEDULER]: The system needs to be modified for a new scheduler with the following scripts
#          - carbonate_bogus.sh"
#          - carbonate_vinit.sh"
#          - carbonate_init.sh"
#          - carbonate_letkf.sh"
#          - carbonate_job_card_dfi.sh"
#          - carbonate_job_card_ctl.sh"
#
# [COPYRIGHT]: (C) 2015 trademarked for Dept of Geological Sciences, Indiana University
#             
#===========================================================================================
# 
# setting up path environment
#
#set -u -x -e
USER_EMAIL="ckieu@indiana.edu"
DIR_MAN="/N/slate/ckieu/tc_wrf_letkf"
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
DIR_GRIB_DATA="${DIR_MAN}/data/avn/20131010"
DIR_POST="${DIR_MAN}/wrfpost"
source ./quartz_namelist.sh
if [ -f $DIR_MAN/namelist.letkf ]; then
    echo " MAIN: $DIR_MAN/namelist.letkf exists... start running now"
else
    echo " MAIN: $DIR_MAN/namelist.letkf does not exist...exit 0"
    exit 0
fi
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
export VORT_INIT=${vort_init}
export DIR_MAN=${DIR_MAN}
#
# create a shared input for all scripts. This is much better than exporting 
# thru the main script.
#
echo "start_time_main=${d01_year}${d01_month}${d01_day}${d01_hour}${d01_minute}" > ./input.sh
echo "BND_INTERVAL=${interval_bnd}"       >> ./input.sh
echo "D01_NX=${d01_nx}"                   >> ./input.sh
echo "D01_NY=${d01_ny}"                   >> ./input.sh
echo "D01_NZ=${d01_nz}"                   >> ./input.sh
echo "D01_DX=${d01_dx}"                   >> ./input.sh
echo "D01_DY=${d01_dy}"                   >> ./input.sh
echo "D01_DT=${d01_dt}"                   >> ./input.sh
echo "MAXIMUM_DOM=${maximum_dom}"         >> ./input.sh
echo "DIR_LETKF=$DIR_LETKF"               >> ./input.sh
echo "DIR_RUN=$DIR_RUN"                   >> ./input.sh
echo "DIR_MAN=$DIR_MAN"                   >> ./input.sh
echo "DIR_ANA=$DIR_ANA"                   >> ./input.sh
echo "DIR_BGD=$DIR_BGD"                   >> ./input.sh
echo "DIR_FSC=$DIR_FSC"                   >> ./input.sh
echo "DIR_INI=$DIR_INI"                   >> ./input.sh
echo "DIR_OBS=$DIR_OBS"                   >> ./input.sh
echo "DIR_OBS_DATA=$DIR_MAN/data/obs/"    >> ./input.sh
echo "DIR_3DV=../model/WRFDA/var/obsproc" >> ./input.sh
echo "runmode=${RUNMODE}"                 >> ./input.sh
echo "RUNMODE=${RUNMODE}"                 >> ./input.sh
echo "RESTART_INTERVAL=$RESTART_INTERVAL" >> ./input.sh    
echo "OBSMODE=$OBSMODE"                   >> ./input.sh
echo "REF_LAT_MAIN=$REF_LAT_MAIN"         >> ./input.sh
echo "REF_LON_MAIN=$REF_LON_MAIN"         >> ./input.sh
echo "ne=${ne}"                           >> ./input.sh
echo "vort_init=${vort_init}"             >> ./input.sh
echo "SPINUP=${SPINUP}"                   >> ./input.sh
echo "blend_obs=${blend_obs}"             >> ./input.sh
echo "fcst_length=$fsct_length_main"      >> ./input.sh
chmod uog+x ./input.sh
#
# safty check for real-time mode or experiment mode
#
if [ "$interval_out" -gt "$restart" ]; then
    echo " MAIN: WRF output interval cannot be larger than restart interval... exit"
    exit 1 
fi
if [ "$cold_start" == "COLD" ] && [ "$runmode" != "REAL_TIME" ]; then
    echo " MAIN: We will run a clean cold start... "
    #rm -rf $DIR_FSC/*
    #rm -rf $DIR_ANA/*
    #rm -rf $DIR_BGD/*
    #rm -f log.*
else
    echo " MAIN: Run either REAL_TIME or cold_start is not set"
    #rm -f log.*  
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
# Carry out the full DA step for one cycle now. For REAL_TIME mode, run only 1 cycle valid at the real 
# time (i.e., nloop = 1). For other modes, loop thru all DA cycles (nloop > 1). See namelist.sh script
# for the setting of nloop/tloop/fsct_length.
#
if [ $runmode == "REAL_TIME" ]; then
    echo " " 
    echo " #################################################################"
    echo " #                                                               #"
    echo " #         THE TYPHOON REAL TIME WRF-LETKF SYSTEM V4.0           #"
    echo " #            INDIANA UNIVERSITY - Copyright 2015                #"
    echo " #                                                               #"
    echo " #################################################################"
    echo " MAIN: The number of DA cycles for real-time must be 1 vs ${nloop}"
else
    echo " "
    echo " #################################################################"
    echo " #                                                               #"
    echo " #               THE WRF-LETKF SYSTEM  V4.0                      #"
    echo " #           INDIANA UNIVERSITY - Copyright 2015                 #"
    echo " #                                                               #"
    echo " #################################################################"
    echo " MAIN: the number of DA cycles is ${nloop} "
fi
iloop=1
while [ "$iloop" -le "$nloop" ]; do
    echo " MAIN: Current cycle: $start_time_main" 
    echo " MAIN: cold_start = $cold_start"
    #
    # run first WPS code
    #
    if [ -f status_wps_ok ]; then
        echo " MAIN: quartz_wps.sh successfully ran. Skip this wps step ..." 
    else
        echo " MAIN: quartz_wps.sh is running now ..."
        ./quartz_wps.sh ${start_time_main} ${fsct_length_main} >& ${DIR_RUN}/log.wps
        if [ -f error.status ]; then
            mv error.status status_wps_ok
        else
            echo " MAIN: quartz_wps.sh has abrupt die...exit 3"
            exit 3
        fi
    fi
    #
    # calling next the WRF real to create a control boundary and initial condition
    #
    rm -f wrfinput_d0* wrfbdy_d0* rsl*
    if [ -f status_real_ok ]; then
        echo " MAIN: quartz_real.sh successfully ran. Skip this real step ..."
    else
        echo " MAIN: quartz_real.sh is running now ..."
        if [ "$WRF_QSUB" == "YES" ]; then
            rm -f slurm-* rsl*
            sbatch ./quartz_real.sh >& /dev/null
            check_real=""
            while [ "$check_real" == "" ]; do
                echo "wrf real step is still running. Sleep 10s..." >& $DIR_RUN/log.real
                if [ -f rsl.out.0000 ]; then
                    check_real=`grep SUCCESS rsl.out.0000` >& /dev/null
                else
                    check_real=""
                fi
                sleep 10
            done 
            #$DIR_RUN/carbonate_check_qsub.sh $DIR_RUN/status_real_ok >& $DIR_RUN/log.real
            cat slurm-* >> $DIR_RUN/log.real
        else
            #./quartz_real.sh ${start_time_main} ${fsct_length_main} ${runmode} >& ${DIR_RUN}/log.real
            ./quartz_real.sh  >& ${DIR_RUN}/log.real
        fi
        if [ -f $DIR_ANA/${start_time_main}/wrfbdy_d01 ]; then
            echo "quartz_real.sh successfully finished" > ${DIR_RUN}/status_real_ok
        else
            echo " MAIN: quartz_real.sh has abrupt die...exit 4"
            exit 4
        fi
    fi
    ln -sf $DIR_ANA/${start_time_main}/wrfinput_d0* ./
    ln -sf $DIR_ANA/${start_time_main}/wrfbdy_d0* ./
    #
    # create a cold start for an ensemble of wrf input 
    #
    if [ $cold_start == "COLD" ]; then
        cd $DIR_INI
        ln -sf $DIR_RUN/wrfinput_d0* ./
        ln -sf $DIR_RUN/wrfbdy_d0* ./
        cp ${DIR_RUN}/input.sh ./
        cp ${DIR_RUN}/quartz_cal_time.sh ./
        cp ${DIR_RUN}/quartz_cal_ensemble.sh ./
        cp ${DIR_RUN}/quartz_nlist_wrf.sh ./
        cp ${DIR_RUN}/quartz_nlist_dfi.sh ./
        ./quartz_nlist_wrf.sh ${start_time_main} 1 1 1 1 1HR > ${DIR_RUN}/log.nlist
        if [ -f $DIR_RUN/status_ini_ok ]; then
            echo " MAIN: quartz_ini.sh successfully ran. Skip this ini step ..."
        else
            echo " MAIN: quartz_ini.sh creating initial ensemble for cold start is running now ..."
            echo "s,OUTPUTDIR,${DIR_RUN},g" > in.txt
            sed -f in.txt ${DIR_MAN}/run/quartz_ini.sh > ./quartz_ini.sh
            if [ "$WRF_QSUB" == "YES" ] && [ "$SPINUP" == "YES" ]; then
                rm -f slurm-*
                sbatch ./quartz_ini.sh >& /dev/null
                $DIR_RUN/quartz_check_qsub.sh $DIR_RUN/status_ini_ok >& $DIR_RUN/log.ini
                cat slurm-* >> $DIR_RUN/log.ini
            else
                sh ./quartz_ini.sh >& $DIR_RUN/log.ini 
            fi
            mkdir -p $DIR_BGD/${start_time_main}
            mv bgd_* $DIR_BGD/${start_time_main}/
            cp $DIR_RUN/wrfbdy_d01 $DIR_BGD/${start_time_main}/
        fi
        cd $DIR_RUN
    fi 
    #
    # calling the script to create bogus obs and insert sythetic vortex obs 
    # into the initial ensemble before assimilating other sources of obs. This is a
    # 2-step process. 1) creating a bogus vortex via carbonate_bogus.sh, and 2)
    # do DA of the bogus vortex via carbonate_vortda.sh. This 2-step process is
    # contained in carbonate_vinit.sh. 
    #
    if [ "$vort_init" == "YES" ] && [ "$obsmode" != "CONTROL" ]; then
        if [ -f $DIR_RUN/status_vinit_ok  ]; then
            echo " MAIN: quartz_vinit.sh successfully ran. Skip this bogus vortex step ..."
        else
            echo " MAIN: quartz_vinit.sh will be run now ..."
            cd ${DIR_MAN}/vortex_init
            cp ${DIR_MAN}/run/quartz_cal_time.sh ./
            cp ${DIR_MAN}/run/quartz_cal_ensemble.sh ./
            echo "s,OUTPUTDIR,${DIR_RUN},g" > in.txt
            sed -f in.txt ${DIR_MAN}/run/quartz_vinit.sh > ./quartz_vinit.sh
            cp ${DIR_MAN}/run/quartz_bogus.sh ./
            cp ${DIR_RUN}/input.sh ./
            if [ "${WRF_QSUB}" == "YES" ]; then
                rm -f slurm-*
                sbatch ./quartz_vinit.sh >& $DIR_RUN/log.vinit
                $DIR_RUN/quartz_check_qsub.sh $DIR_RUN/status_vinit_ok >& $DIR_RUN/log.check_qsub_vinit
                cat slurm-* >> $DIR_RUN/log.vinit
            else
                sh ./quartz_vinit.sh >& $DIR_RUN/log.vinit
            fi 
        fi
    fi
    #
    # prepare traditional observation by calling WRF-3DVAR to create obs files
    # in an intermediate format for the LETKF system.
    #
    cd $DIR_OBS
    if [ -f $DIR_RUN/status_obs_ok  ] || [ "$obsmode" == "CONTROL" ]; then
        echo " MAIN: quartz_obs.sh safely finished or obsmode = CONTROL. Skip this obs step ..."
    else
        echo " MAIN: quartz_obs.sh is running now ..."
        cp ${DIR_RUN}/input.sh ./
        cp ${DIR_MAN}/run/quartz_obs.sh ./
        cp ${DIR_MAN}/run/quartz_cal_time.sh ./
        cp ${DIR_MAN}/run/quartz_cal_ensemble.sh ./
        echo "time_input=${start_time_main}" >> ./input.sh
        echo "fsct_length_hour=0" >> ./input.sh
        sh ./quartz_obs.sh >& $DIR_RUN/log.obs 
        check_obs=`ls obs_*`
        if [ "${check_obs}" == "" ]; then
            echo "MAIN: quartz_obs.sh failed... exit 7"
            exit 7
        else
            echo "quartz_obs.sh successfully finished" > ${DIR_RUN}/status_obs_ok
        fi
    fi
    #
    # calling the letkf to assimilate the additional source of observation
    # on top of (potential) bogus vortex. For obsmode=CONTROL, this letkf step
    # will simply cp bgd to ana inside the carbonate_letkf.sh script.
    #
    cd $DIR_LETKF
    if [ -f $DIR_RUN/status_letkf_ok  ]; then
        echo " MAIN: quartz_letkf.sh successfully ran. Skip this LETKF step ..."
    else
        echo " MAIN: quartz_letkf.sh is running now ..."
        ln -sf $DIR_RUN/namelist.letkf ./
        rm -f wrfinput_d0* wrfbdy_d0*
        ln -sf $DIR_RUN/wrfinput_d0* ./
        ln -sf $DIR_RUN/wrfbdy_d0* ./
        cp $DIR_RUN/input.sh ./
        cp ${DIR_MAN}/run/quartz_cal_time.sh ./
        cp ${DIR_MAN}/run/quartz_cal_ensemble.sh ./
        echo "s,OUTPUTDIR,${DIR_RUN},g" > in.txt 
        sed -f in.txt $DIR_MAN/run/quartz_letkf.sh > ./quartz_letkf.sh
        if [ "$WRF_QSUB" == "YES" ]; then
            rm -f slurm-*
            sbatch ./quartz_letkf.sh  >& /dev/null
            $DIR_RUN/quartz_check_qsub.sh $DIR_RUN/status_letkf_ok >& $DIR_RUN/log.letkf
            cat slurm-* >> $DIR_RUN/log.letkf
        else
            sh ./quartz_letkf.sh >& $DIR_RUN/log.letkf
        fi
    fi
    if [ -f $DIR_RUN/no_da_flag.txt ]; then
        echo " MAIN: No data for assimilation. Switching to CONTROL mode"
        obsmode="CONTROL"
        echo " Switching to CONTROL mode " >> $DIR_RUN/status_letkf_ok
    fi
    #
    # update boundary for each member when obsmode is different from CONTROL
    #
    cd $DIR_RUN
    . ./quartz_cal_time.sh
    . ./quartz_cal_ensemble.sh
    cal_time ${data_year_main} ${data_month_main} ${data_day_main} \
             ${data_hour_main} ${data_minute_main} 0 0 ${restart}
    restart_file=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00
    restart_dir=${E_YEAR}${E_MONTH}${E_DAY}${E_HOUR}${E_MINUTE}
    echo " MAIN: restart_file = $restart_file"
    echo " MAIN: restart_dir  = $restart_dir"
    echo " MAIN: generating boundary condition from up_date_bc now"
    if [ -f $DIR_RUN/status_update_bc_ok  ] || [ "$obsmode" == "CONTROL" ]; then
        echo " MAIN: Boundary update step is ok. Skip this boundary update step ..."
    else
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
            cp ./wrfinput_d01 ./wrfinput_d01_orig
            cp ${DIR_MAN}/script/quartz_wrfda_parame.in ./parame.in
            ln -sf ${DIR_MAN}/model/WRFDA/var/build/da_update_bc.exe ./
            ./da_update_bc.exe > $DIR_RUN/log.update_bc
            if [ -f fort.11 ]; then
                 echo " MAIN: da_update_bc.exe finished member $ie"
                 mv fort.11 $DIR_RUN/status_bcupdate_ok
            else
                 echo " MAIN: da_update_bc.exe has abrupt die...exit 8"
                 exit 8
            fi
            ie=$(($ie+1))
        done
        echo " MAIN: MAIN: Boundary update step finished safely" >& $DIR_RUN/status_update_bc_ok
    fi
    #
    # Now create separate namelists for multiple physics options and corresponding job submission
    # scripts.
    #
    echo " MAIN: creating mutiple/single physics wrf namelist now"
    ie=1
    while [ "$ie" -le "$ne" ]; do
        cal_ensemble_prefix ${ie}
        prefix=${out_prefix}
        if [ "$model_opt" == "YES" ] && [ "$obsmode" != "CONTROL" ]; then
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
        ./quartz_nlist_wrf.sh ${start_time_main} $fsct_length_main $mcphysics $lwradiation \
                       $cu_opt $runmode > $DIR_RUN/log.namelist
        mv namelist.input_${mcphysics} $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input
        ./quartz_nlist_dfi.sh ${start_time_main} $fsct_length_main $mcphysics $lwradiation \
                       $cu_opt $runmode > $DIR_RUN/log.namelist_dfi
        mv namelist.input_dfi_${mcphysics} $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input_dfi
        #
        # create scheduler submission job script for each member
        #
        if [ "$SPINUP" == "YES" ] && [ "$obsmode" != "CONTROL" ]; then
            ./quartz_job_card_dfi.sh 8 "$DIR_ANA/${start_time_main}/mem_${prefix}/"
        else
            ./quartz_job_card_ctl.sh 8 "$DIR_ANA/${start_time_main}/mem_${prefix}/"
        fi
        mv wrf_qsub.sh $DIR_ANA/${start_time_main}/mem_${prefix}/
        ie=$(($ie+1))
    done
    #
    # option for submit qsub job for all members simultaneously or serially
    #
    if [ -f $DIR_RUN/status_wrf_job_submit_ok ]; then 
        echo " MAIN: All WRF jobs successfully ran. Skip this WRF submission ..."
    else
        echo " MAIN: Submit WRF jobs now"
        rm -f rsl* log.qsub 
        ie=1
        while [ "$ie" -le "$ne" ]; do
            cal_ensemble_prefix ${ie}
            prefix=${out_prefix}
            if [ "$WRF_QSUB" == "NO"  ]; then
                rm -f wrfinput_d0* wrfbdy_d0*
                ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfinput_d0* ./
                ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/wrfbdy_d0* ./
                if [ "$SPINUP" == "YES"  ]; then
                    ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input_dfi ./namelist.input
                    echo " MAIN: running DFI wrf.exe for member $ie ..."
                    rm -f rsl*
                    if [ "$MPIRUN" == "yes" ]; then
                        $runwrfmpi >& log.wrf
                    else
                        ./wrf.exe >& log.wrf
                    fi
                    if [ -f wrfinput_initialized_d01 ]; then
                        mv wrfinput_initialized_d01 ./wrfinput_d01
                    else
                        echo " MAIN: DFI step failed ... exit 3"
                        exit 3
                    fi 
                fi
                #
                # running WRF core serially in the DIR_RUN location
                #
                ln -sf $DIR_ANA/${start_time_main}/mem_${prefix}/namelist.input ./namelist.input
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
                cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} \
                   $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
            else
                cd $DIR_ANA/${start_time_main}/mem_${prefix}/
                ln -sf $DIR_WRF/run/*.exe ./
                ln -sf $DIR_WRF/run/*.TBL ./
                ln -sf $DIR_WRF/run/*DATA* ./
                ln -sf $DIR_WRF/run/ozone* ./
                ln -sf $DIR_WRF/run/*.tbl ./
                ln -sf $DIR_WRF/run/grib* ./
                ln -sf $DIR_WRF/run/co2_trans* ./
                rm -f slurm*.out
                $DIR_RUN/quartz_job_submit.sh $njob_max $DIR_RUN
                cd $DIR_RUN
            fi
            if [ "$ie" == "1" ]; then
                echo $DIR_FSC/${start_time_main}/mem_${prefix} > ${DIR_MAN}/wrfpost/tc_input_dir.txt
            else
                echo $DIR_FSC/${start_time_main}/mem_${prefix} >> ${DIR_MAN}/wrfpost/tc_input_dir.txt
            fi
            ie=$(($ie+1))
        done
    fi 
    echo "MAIN: submit all wrf jobs run safely" >&  $DIR_RUN/status_wrf_job_submit_ok 
    #
    # Check if all WRF runs finished safely ... 
    #
    wrf_job_done="NO"
    if [ "$WRF_QSUB" == "YES" ]; then
        echo " MAIN: check for WRF jobs in the qsub mode ..."
        qsub_done=0
        qsub_count=0
        while [ "$qsub_done" -lt "$ne" ] && [ "${wrf_job_done}" == "NO" ]; do
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
                elif [ -f slurm*.out ]; then
                    check_qsub=`tail -n 10 slurm*.out | grep SUCCESS`
                    if [ "${check_qsub}" != "" ]; then
                        qsub_done=$(($qsub_done+1))
                    else
                        qsub_done=0
                    fi
                else
                    check_qsub="NO rsl.error.0000 or slurm output yet ... sleeping"
                    qsub_done=0
                fi
                echo " MAIN: check member $ie returns: ($check_qsub) and ($qsub_done)" >> $DIR_RUN/log.qsub
                ie=$(($ie+1))
            done
            if [ "${qsub_done}" -ge "$ne" ]; then
                wrf_job_done="YES"
            else
                sleep 2m
            fi
            qsub_count=$(($qsub_count+1))
            if [ "$qsub_count" -eq 10000 ]; then
                echo " MAIN: slurm is on queue for too long...something wrong, exit"
                mail -s "wrf-letkf crashed at $( date )" ${USER_EMAIL} < main.log
                exit 1
            fi
        done
    else
        echo " MAIN: check for WRF jobs in interactive mode ..."
        qsub_done=0
        ie=1
        while [ "$ie" -le "$ne" ]; do
            cal_ensemble_prefix ${ie}
            prefix=${out_prefix}
            cd $DIR_ANA/${start_time_main}/mem_${prefix}/
            if [ -f log.wrf ]; then
                check_wrf_log=`grep SUCCESS ./log.wrf`
                if [ "${check_wrf_log}" != "" ]; then
                    qsub_done=$(($qsub_done+1))
                else
                    qsub_done=0
                fi
            else
                echo "MAIN: this member must fail... exit 2"
                exit 2
            fi
            echo " MAIN: check member $ie returns: ($check_wrf_log) and ($qsub_done)" >> $DIR_RUN/log.qsub
            ie=$(($ie+1))
        done
        if [ "${qsub_done}" -lt "$ne" ]; then
            echo " MAIN: one of interactive WRF job submissions failed... exit 1"
            exit 1    
        fi
    fi  
    echo " MAIN: all slurm wrf jobs finished safely " > $DIR_RUN/status_wrf_ok
    echo " MAIN: all slurm wrf jobs completed safely "
    #
    # move all forecast data from ana dir to fcs dir
    # 
    echo " MAIN: Moving output from ana to fsc location..."
    if [ -f $DIR_RUN/status_transfer_data_ok ]; then
        echo " MAIN: All WRF jobs data has been transfer. Skip this step ..."
    else
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
            cp $DIR_FSC/${start_time_main}/mem_${prefix}/wrfout_d01_${restart_file} \
            $DIR_BGD/${restart_dir}/bgd_d01_${prefix}_${restart_file}
            ie=$(($ie+1))
        done
        echo " MAIN: Done trasnfering" >& $DIR_RUN/status_transfer_data_ok
    fi
    cd $DIR_RUN
    #
    # doing some diagnostic calculations for REAL_TIME/EXP_REAL mode only. All other modes 
    # have to do it separately
    #
    echo " MAIN: running post processing now"
    if [ "$runmode" == "REAL_TIME" ] || [ "$runmode" == "EXP_REAL" ]; then
        cd $DIR_POST
        cp $DIR_MAN/run/quartz_ensemble_wpost.sh ./
        cp $DIR_MAN/run/input.sh ./
        if [ -f ${DIR_MAN}/tcvital/vital_record.txt ]; then
            mkdir -p ./$start_time_main
            rm -rf ./$start_time_main/d0*
            storm=`cat ${DIR_MAN}/tcvital/vital_record.txt | awk '{print $3}'`
            stormid=`cat ${DIR_MAN}/tcvital/vital_record.txt | awk '{print $2}'`
            basin=`cat ${DIR_MAN}/tcvital/vital_record.txt | awk '{print $2}' | sed 's/W/ W/' | sed 's/L/ A/' | \
                    awk '{if($2=="W") print "WP"; if($2=="A") print "AL"}'`
            mv ${DIR_MAN}/tcvital/vital_record.txt ${DIR_MAN}/tcvital/vital_record_${storm}_${start_time_main}.txt
            cycle="${data_year_main}${data_month_main}${data_day_main}${data_hour_main}"
            if [ -f $DIR_RUN/status_transfer_data_ok ]; then
                echo " MAIN: runing ensemble post: $storm d0X ${basin} $obsmode $cycle"
                ./quartz_ensemble_wpost.sh $storm d01 ${basin} $obsmode $cycle >& ${DIR_RUN}/log.wpostd01
                ./quartz_ensemble_wpost.sh $storm d02 ${basin} $obsmode $cycle >& ${DIR_RUN}/log.wpostd02
                ./quartz_ensemble_wpost.sh $storm d03 ${basin} $obsmode $cycle >& ${DIR_RUN}/log.wpostd03
                mv d0* ./$start_time_main/
            else
                echo " MAIN: some jobs failed. Cannot produce diagnostic now. Skip this cycle"
            fi
            cd $DIR_RUN
        else
            cd $DIR_POST
            cp $DIR_RUN/quartz_runwpost.sh ./
            ./quartz_runwpost.sh ${start_time_main} ${ne} > ${DIR_RUN}/log.wpost
            cd $DIR_RUN
        fi
    else
        cd $DIR_DIG
        ./runrms.sh ${start_time_orig} ${fsct_length_main} $(($restart/60)) > $DIR_RUN/log.rms
        cd $DIR_RUN
    fi
    rm -f wrfbdy* wrfinput* wrfout*
    iloop=$(($iloop+1))
done
#
# clean all links finally
#
#mail -s "wrf-letkf successful report at $( date )" ckieu@indiana.edu < main.log
rm -rf wrfbdy* wrfinput* wrfout* namelist* CAM* *.TBL util example* kernel* CCN*
rm -rf RRT* ETAMP* clean Vtable ungrib* ndown* namelist.output link_grib.sh
rm -rf tr4* geo* fsc_* README* *.tbl *.txt config* bulk* *.exe landFilenames
rm -rf tr6* metgrid* ozone* nup* compile arch error* *.log *.asc CLM* aerosol*
rm -rf in.txt parame.in fort.* error.status slurm*
mv ../namelist.letkf ./
echo " MAIN script has finished normally     "

# [NOTE]:
#           This shell script is for creating a namelist.input for wrf with different
#           microphysic options
#
# [HISTORY]:
#         - Apr 20, 2010: created
#         - Apr 15, 2011: upgraded multiple physics, cumulus, and radiation from bk10
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
DIR_RUN=`pwd`
interval_bnd=`echo $BND_INTERVAL`                          # frequency of updating WPS boundary in s
interval_out=`echo $OUT_INTERVAL`
interval_restart_second=`echo $RESTART_INTERVAL`           # frequency of truth data in seconds
interval_restart=$(($interval_restart_second/60))          # frequency of truth data in minutes
d01_nx=`echo $D01_NX`
d01_ny=`echo $D01_NY`
d01_nz=`echo $D01_NZ`
d01_dx=`echo $D01_DX`
d01_dy=`echo $D01_DY`
d01_dt=`echo $D01_DT`
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
BND_DIST=`echo $CORR_DIST`
max_dom=`echo $MAXIMUM_DOM`
if [ "$interval_bnd" == "" ]; then
 echo " NAMELIST: interval_bnd is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$interval_out" == "" ]; then
 echo " NAMELIST: interval_bnd is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$interval_restart_second" == "" ]; then
 echo " NAMELIST: d01_nz is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$d01_nz" == "" ]; then
 echo " NAMELIST: d01_nz is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$max_dom" == "" ]; then
 echo " NAMELIST: max_dom is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$d01_nx" == "" ]; then
 echo "NAMELIST:  d01_nx is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$d01_ny" == "" ]; then
 echo " NAMELIST: d01_ny is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$d01_dx" == "" ]; then 
 echo " NAMELIST: d01_dx is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$d01_dy" == "" ]; then
 echo " NAMELIST: d01_dy is empty, check runmain.sh ...exit 1"
 exit 1
elif [ "$d01_dt" == "" ]; then
 echo " NAMELIST: d01_dt is empty, check runmain.sh ...exit 1"
 exit 1
fi
#
# option for real-time mode or experiment mode
#
if [ $# == 6 ]; then
  echo " NAMELIST: We will run history case... "
  time_input=$1
  data_year_nlist=`echo ${time_input} | cut -c1-4`
  data_month_nlist=`echo ${time_input} | cut -c5-6`
  data_day_nlist=`echo ${time_input} | cut -c7-8`
  data_hour_nlist=`echo ${time_input} | cut -c9-10`
  data_minute_nlist=`echo ${time_input} | cut -c11-12`
  fsct_length_nlist=$2
  mcphysics=$3
  lwradiation=$4
  cu_opt=$5
  runmode=$6
else
  echo " NAMELIST: Please use one of these options: "
  echo " ./namelistwrf.sh YYYYMMDDHH HH mcphysics lwradiation cu_opt runmode"
  exit 0
fi		 
#
# construct the strings of starting time and end time for integration
#
. ./carbonate_cal_time.sh
cal_time ${data_year_nlist} ${data_month_nlist} ${data_day_nlist} ${data_hour_nlist} ${data_minute_nlist} 0 ${fsct_length_nlist} 0
end_time=${E_YEAR}-${E_MONTH}-${E_DAY}_${E_HOUR}:${E_MINUTE}:00   
start_time=${data_year_nlist}-${data_month_nlist}-${data_day_nlist}_${data_hour_nlist}:${data_minute_nlist}:00
echo " NAMELIST: Starting time is: $start_time"
echo " NAMELIST: Ending time is: $end_time"
echo " NAMELIST: interval_bnd= $interval_bnd"
echo " NAMELIST: d01_nx= $d01_nx"
echo " NAMELIST: d01_ny= $d01_ny"
echo " NAMELIST: d01_nz= $d01_nz"
echo " NAMELIST: d01_dx= $d01_dx"
echo " NAMELIST: d01_dy= $d01_dy"
echo " NAMELIST: max_dom= $max_dom"
echo " NAMELIST: runmode= $runmode"
echo " NAMELIST: mcphysics option = $mcphysics"
echo " NAMELIST: lwradiation = $lwradiation"
echo " NAMELIST: cu_opt = $cu_opt"
#
# create a namelist for wps
#
cal_time ${data_year_nlist} ${data_month_nlist} ${data_day_nlist} ${data_hour_nlist} ${data_minute_nlist} 0 ${fsct_length_nlist} 0
if [ "$runmode" == "EXP_IDEAL" ]; then
 echo " NAMELIST: creating namelist for idealized run"
 cat > ${DIR_RUN}/namelist.input_${mcphysics} << EOF1
 &time_control
 run_days                            = 0,
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = ${data_year_nlist}, ${data_year_nlist}, ${data_year_nlist},
 start_month                         = ${data_month_nlist},   ${data_month_nlist},   ${data_month_nlist},
 start_day                           = ${data_day_nlist},   ${data_day_nlist},   ${data_day_nlist},
 start_hour                          = ${data_hour_nlist},   ${data_hour_nlist},   ${data_hour_nlist},
 start_minute                        = ${data_minute_nlist},   ${data_minute_nlist},   ${data_minute_nlist},
 start_second                        = 00,   00,   00,
 end_year                            = ${E_YEAR}, ${E_YEAR}, ${E_YEAR},
 end_month                           = ${E_MONTH},   ${E_MONTH},   ${E_MONTH},
 end_day                             = ${E_DAY},   ${E_DAY},   ${E_DAY},
 end_hour                            = ${E_HOUR},   ${E_HOUR},   ${E_HOUR},
 end_minute                          = ${E_MINUTE},   ${E_MINUTE},   ${E_MINUTE},
 end_second                          = 00,   00,   00,
 interval_seconds                    = ${interval_bnd}
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = ${interval_out},  ${interval_out},  ${interval_out},
 frames_per_outfile                  = 1,  1,  1,
 restart                             = .false.,
 restart_interval                    = 54000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = ${d01_dt},
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = ${max_dom},
 e_we                                = ${d01_nx},    ${d02_nx},    ${d03_nx},
 e_sn                                = ${d01_ny},    ${d02_ny},    ${d03_ny},
 e_vert                              = ${d01_nz},    ${d02_nz},    ${d03_nz},
 p_top_requested                     = 5000,
 num_metgrid_levels                  = 42,
 num_metgrid_soil_levels             = 0,
 dx                                  = ${d01_dx}, ${d02_dx},  ${d03_dx},
 dy                                  = ${d01_dy}, ${d02_dy},  ${d03_dy},
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 1,     ${d02_ILL},    ${d03_ILL},
 j_parent_start                      = 1,     ${d02_JLL},    ${d03_JLL},
 parent_grid_ratio                   = 1,     ${d02_ratio12},     ${d02_ratio12},
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
 /

 &physics
 mp_physics                          = ${mcphysics},     ${mcphysics},    ${mcphysics},
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = ${lwradiation},     ${lwradiation},     ${lwradiation},
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = ${cu_opt},     ${cu_opt},     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 0,
 ifsnow                              = 0,
 icloud                              = 0,
 surface_input_source                = 0,
 num_soil_layers                     = 1,
 sf_urban_physics                    = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &fdda
 /

 &dynamics
 w_damping                           = 0,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,      0,      0,
 diff_6th_factor                     = 0.12,   0.12,   0.12,
 base_temp                           = 290.
 damp_opt                            = 0,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.2,    0.2,    0.2
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 non_hydrostatic                     = .true., .true., .true.,
 moist_adv_opt                       = 1,      1,      1,     
 scalar_adv_opt                      = 1,      1,      1,     
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .false.,.false.,.false.,
 periodic_x                          = .false.,.false.,.false.,
 symmetric_xs                        = .false.,.false.,.false.,
 symmetric_xe                        = .false.,.false.,.false.,
 open_xs                             = .true., .false.,.false.,
 open_xe                             = .true., .false.,.false.,
 periodic_y                          = .false.,.false.,.false.,
 symmetric_ys                        = .false.,.false.,.false.,
 symmetric_ye                        = .false.,.false.,.false.,
 open_ys                             = .true., .false.,.false.,
 open_ye                             = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF1
elif [ "$runmode" == "REAL_TIME" ] || [ "$runmode" == "EXP_REAL" ] || [ "$runmode" == "EXP_REAN" ]; then
#
# create a namelist for WRF
#
 echo " NAMELIST: creating namelist for real run"
 rm -f namelist.input* 
 cat > ${DIR_RUN}/namelist.input_${mcphysics} << EOF2
 &time_control
 run_days                            = 0,
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = ${data_year_nlist}, ${data_year_nlist}, ${data_year_nlist},
 start_month                         = ${data_month_nlist},   ${data_month_nlist},   ${data_month_nlist},
 start_day                           = ${data_day_nlist},   ${data_day_nlist},   ${data_day_nlist},
 start_hour                          = ${data_hour_nlist},   ${data_hour_nlist},   ${data_hour_nlist},
 start_minute                        = ${data_minute_nlist},   ${data_minute_nlist},   ${data_minute_nlist},
 start_second                        = 00,   00,   00,
 end_year                            = ${E_YEAR}, ${E_YEAR}, ${E_YEAR},
 end_month                           = ${E_MONTH},   ${E_MONTH},   ${E_MONTH},
 end_day                             = ${E_DAY},   ${E_DAY},   ${E_DAY},
 end_hour                            = ${E_HOUR},   ${E_HOUR},   ${E_HOUR},
 end_minute                          = ${E_MINUTE},   ${E_MINUTE},   ${E_MINUTE},
 end_second                          = 00,   00,   00,
 interval_seconds                    = ${interval_bnd}
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = ${interval_out},  ${interval_out},  ${interval_out},
 frames_per_outfile                  = 1,  1,  1,
 restart                             = .false.,
 restart_interval                    = 54000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = ${d01_dt},
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = ${max_dom},
 e_we                                = ${d01_nx},    ${d02_nx},    ${d03_nx},
 e_sn                                = ${d01_ny},    ${d02_ny},    ${d03_ny},
 e_vert                              = ${d01_nz},    ${d02_nz},    ${d03_nz},
 p_top_requested                     = 10000,
 num_metgrid_levels                  = 42,
 num_metgrid_soil_levels             = 0,
 dx                                  = ${d01_dx}, ${d02_dx},  ${d03_dx},
 dy                                  = ${d01_dy}, ${d02_dy},  ${d03_dy},
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 1,     ${d02_ILL},    ${d03_ILL},
 j_parent_start                      = 1,     ${d02_JLL},    ${d03_JLL},
 parent_grid_ratio                   = 1,     ${d02_ratio12},     ${d02_ratio12},
 parent_time_step_ratio              = 1,     ${d02_ratio12},     ${d02_ratio12},
 feedback                            = 1,
 smooth_option                       = 0
 vortex_interval                     = 18,    18,    18,       
 max_vortex_speed                    = 40,    40,    40,       
 corral_dist                         = ${BND_DIST},    ${BND_DIST},     ${BND_DIST},        
 track_level                         = 70000   
 time_to_move                        = 0,     0,     0, 
 /

 &physics
 mp_physics                          = ${mcphysics},     ${mcphysics},    ${mcphysics},
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = ${lwradiation},     ${lwradiation},     ${lwradiation},
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = ${cu_opt},     ${cu_opt},     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 1,
 sf_urban_physics                    = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &fdda
 /

 &dynamics
 w_damping                           = 0,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,      0,      0,
 diff_6th_factor                     = 0.12,   0.12,   0.12,
 base_temp                           = 290.
 damp_opt                            = 0,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.2,    0.2,    0.2
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 non_hydrostatic                     = .true., .true., .true.,
 moist_adv_opt                       = 1,      1,      1,     
 scalar_adv_opt                      = 1,      1,      1,     
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF2
elif [ "$runmode" == "1HR" ]; then
#
# create a namelist for WRF for 1-hour intergration only, which is needed for initializing
# a cold start.
#
 echo " NAMELIST: creating namelist for real run in 1hour"
 rm -f namelist.input* 
 cat > ${DIR_RUN}/namelist.input_${mcphysics} << EOF3
 &time_control
 run_days                            = 0,
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = ${data_year_nlist}, ${data_year_nlist}, ${data_year_nlist},
 start_month                         = ${data_month_nlist},   ${data_month_nlist},   ${data_month_nlist},
 start_day                           = ${data_day_nlist},   ${data_day_nlist},   ${data_day_nlist},
 start_hour                          = ${data_hour_nlist},   ${data_hour_nlist},   ${data_hour_nlist},
 start_minute                        = ${data_minute_nlist},   ${data_minute_nlist},   ${data_minute_nlist},
 start_second                        = 00,   00,   00,
 end_year                            = ${E_YEAR}, ${E_YEAR}, ${E_YEAR},
 end_month                           = ${E_MONTH},   ${E_MONTH},   ${E_MONTH},
 end_day                             = ${E_DAY},   ${E_DAY},   ${E_DAY},
 end_hour                            = ${E_HOUR},   ${E_HOUR},   ${E_HOUR},
 end_minute                          = ${E_MINUTE},   ${E_MINUTE},   ${E_MINUTE},
 end_second                          = 00,   00,   00,
 interval_seconds                    = ${interval_bnd}
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = 60,  60,  60,
 frames_per_outfile                  = 1,  1,  1,
 restart                             = .false.,
 restart_interval                    = 54000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 /

 &domains
 time_step                           = ${d01_dt},
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 e_we                                = ${d01_nx},    ${d02_nx},    ${d03_nx},
 e_sn                                = ${d01_ny},    ${d02_ny},    ${d03_ny},
 e_vert                              = ${d01_nz},    ${d02_nz},    ${d03_nz},
 p_top_requested                     = 10000,
 num_metgrid_levels                  = 42,
 num_metgrid_soil_levels             = 0,
 dx                                  = ${d01_dx}, ${d02_dx},  ${d03_dx},
 dy                                  = ${d01_dy}, ${d02_dy},  ${d03_dy},
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 1,     ${d02_ILL},    ${d03_ILL},
 j_parent_start                      = 1,     ${d02_JLL},    ${d03_JLL},
 parent_grid_ratio                   = 1,     ${d02_ratio12},     ${d02_ratio12},
 parent_time_step_ratio              = 1,     ${d02_ratio12},     ${d02_ratio12},
 feedback                            = 1,
 smooth_option                       = 0
 vortex_interval                     = 18,    18,    18,       
 max_vortex_speed                    = 40,    40,    40,       
 corral_dist                         = ${BND_DIST},    ${BND_DIST},     ${BND_DIST},        
 track_level                         = 70000   
 time_to_move                        = 0,     0,     0, 
 /

 &physics
 mp_physics                          = ${mcphysics},     ${mcphysics},    ${mcphysics},
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = ${lwradiation},     ${lwradiation},     ${lwradiation},
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = ${cu_opt},     ${cu_opt},     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 1,
 sf_urban_physics                    = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &fdda
 /

 &dynamics
 w_damping                           = 0,
 diff_opt                            = 1,
 km_opt                              = 4,
 diff_6th_opt                        = 0,      0,      0,
 diff_6th_factor                     = 0.12,   0.12,   0.12,
 base_temp                           = 290.
 damp_opt                            = 0,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.2,    0.2,    0.2
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 non_hydrostatic                     = .true., .true., .true.,
 moist_adv_opt                       = 1,      1,      1,     
 scalar_adv_opt                      = 1,      1,      1,     
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF3

else
 echo " NAMELIST: option for $runmode has no namelist option...exit 3"
 exit 3
fi
if [ -f $DIR_RUN/namelist.input_${mcphysics} ]; then
    ln -sf $DIR_RUN/namelist.input_${mcphysics} ./namelist.input
    echo " NAMELIST script for WRF finished normally       "
else
    echo " NAMELIST namelist could not be generated...exit 4"
    exit 4
fi

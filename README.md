VERSION INFORMATION (v3.0, 11 Mar 2012)

- A new vortex intialization component has been added 
  based on the TC vital informaiton in a starndard format
- tc_runmain.sh has been added new capability with different
  scheduler sourced from an external file for better 
  control for different platform
- new scripts have been added to support different scheduler
  platform
- letkf has been modified to assimilation the geopotential
  HEIGHT perturbation as well as in-homegeneous data sources
  that may have undefined values (-99999 flag).

VERSION INFORMATION (v2.2, 31 Mar 2012)

- The tc_runmain.sh has been revised to allow for limitted
  computational resource
- tc_ensemble post process has been added more automated
  steps
-

  REMAINING ISSUES:
  + localzation scales
  + adaptive inflation
  + d03 topo
  + inclusion of more moisture vars
  + surface obs
  + support more obs types
  + adding an arch support platfform
  + 

VERSION INFORMATION (v2.1.1, 30 Jan 2012)

This versions has several updated as follows:

- the wrfpost has been upgraded such as it can produce all 
  TC tracking plots automatically as well as ensemble mean
  as long as a directory poiting to output forecast and a
  bogus bdeck file are present.  
- MPI letkf version has been put in the main operational  
  mode
- an email option has been added to notify users when the
  main program is done.


VERSION INFORMATION (v2.1, 11 August 2011)

This is the version that works well with opretational normal
wethear, TC forecast, and research

- fixed a bug in idealized experiment (geop perp is not a 
  diagnostic var), but pressure perp is. Note that the 
  pressure pertrubation from the idealized cases has to
  decrease with height as it is a function of density...
  ... more imporvement is needed. 
- fixed a problem with the idealized bogused observation
- parallelize the letkf code. Work much faster now with the
  same amount of memory.
- the typhoon scripts has been revised such as tc_namelist
  has the obsmode as an input for more flexible runs
- the rms.f90 has been revised to handle even missing cases.
  A bug related to reading data has been fixed as well
- more on cross correlation needs to be done...!!!


VERSION INFORMATION (v1.9.8, 19 June 2011)

This updated version includes following improvements:

- added a TC configuration that allows for operationally forecasting
  TC track when a tc vital record is entered
- added a post-process routine to view 
- added qsub for queing run with SGE/TORQUE bash queing
- add multple physics options
- revised the wpost.gs


NOTE

- need to edit parame.in for update_wrfda_bc
- need to link Vtable to WPS
- need to edit the set up in the runmain.sh carefully
- need to edit the qsub for wrf (for now)


VERSION INFORMATION (v1.9, 20 August 2010)

This updated version includes following improvements:

- got few compiler bugs fixed with the ifort compiler
- allows for ensemble boundaries using WRFDA up_date_bc ultility.
- include a new script for running WRF-3dvar and several other
  enhancements of other shell scripts for dealing with 3dvar mode
- obs points can now be arbitrary in the vertical direction, allowing
  for more versatile obs data such as MADIS or GOES winds. 
- option for REAL-TIME run has been added together with a master
  script for obtaining GFS data and an automatical script for
  creating figure in wrfpost. System is ready for operational
  purposes.


VERSION INFORMATION (v1.6, 5 August 2010)

In this updated version, several very significant changes are made including:

- allow for the quality control for each obs var at each location
- the iterative input wrfinput_d01 for idealized run is an ensemble mean
  instead of taking directly from the background. This is help the 
  ensemble to not diverse too much. 
- allow for observation to be arbitrary (previously, obs are pulled 
  back to grid mesh).
- the obs data for the real experiment has a big improvement. Instead
  of using a prescribed obs error structure for each var, the WRFDA
  OBSPROC has been used to perform quality control before assimlating.
  So from this version, WRFDA has to be compiled also for the system
  to run in the real mode. 

DIR INFORMATION

- ini:  dir that contains a cold start initialization for an ensemble of
        initital inputs

- truth: dir that contains an truth created from a base run of the base model.
        This program will use an initial input and integrate the model with 
        time, output at some regular interval as the truth benchmark.

- obs:  dir contains the obs data that are created by running obs.exe. This 
        program will read the truth and add some perturbed white noise to
        the truth to mimic the real obs.

- model: contains the systems of the WRF model including WRF-DA, WPS, WRF

- letkf: contains the assimilation code that performs a data assimilation
        process.

- ana:  contains the analysis output

- fsc:  contains the forecast for each cycle.

- ctl:  contains the base run with no assimilation. This is needed to compare
        to the runs with assimilation.

- dig:  contains some diagnostic calculations

- bgd:  contains the background backup after each assimilating cycle. 

- utils: contains some ultility programs that do intermediate steps

- wrfpost: this dir contains the wrf post processor (V2.1) that could be used
        to have a quick plot of wrf output 

- data: this is the place where WRF input and observation data is stored.

- dig:  this is where the diagnostic calculations of assimilating system are
        stored.


HOW TO RUN:

1. compile each directory by running install.sh (rememer to edit the configure.letkf
   first before installing the program). Note that the whole WRF model including
   WRFV3, WPS, and WRFDA have to be compiled as well. These model components are
   stored in directory "model". 
2. edit the namelist in ./run/runmain.sh (need to link all scripts from the script
   dir to run dir for running the code)
3. run the main script by typing ./runmain.sh.
4. check the log files in ./run to see if there is any error
5. plotting the output by going directly to the dig dir, where some diganostic
   calculations are performed, or linking the output to wrfpost to have more
   detailed plots. 


DEBUG STEPS

1. go to truth dir and run truth first (runtruth.sh) and plot to check
2. go to obs dir and run obs.exe and plot to check
3. go to ini dir and run ini.exe and plot to check
4. go to ctl dir and run a control forecast
5. go to letkf and run letkf.exe to check for a single assimilation
6. if all run well, go to the main dir and run run.sh
7. go to dig dir and check for diagnostic output and plots
8. good luck


NAMELIST GUIDE

In the namelist guide below, pay attention particularly to the vars the have
starisk on it. The numbers of star denotes the importance of the vars. The
vars without star is either set in the main script (runmain.sh) or of no
relevance to the users' use.

Note that this namelist.letkf is generated automatically in the main script 
(runmain.sh). So, all changes should be directed to that main script. Any
change to the namelist.letkf in this dir (./) will be lost.

debug          = 0               ! Debuging level
restart        = 180             ! Output frequency for WRF run     [mn]
                                 ! This output internval is defined
				 ! in the main run script. Note that 
				 ! there is a difference between WRF
				 ! output interval and restart interval
				 ! that is used for assimilating update.
				   
dy             = 81000           ! Grid distance in the y-direction [m]
                                 ! This is defined in the main script
				 
zamp*          = 36              ! Amplitude of v-anomaly           [m]
                                 ! This is a non-dimensiona para
				 ! for an idealized vortex that is used
				 ! for the cold start initialization
				 ! of the idealized experiment. Users
				 ! need to check truth/bvortex.ctl
				 ! for the actual magnitude of this
				 ! bogussed vortex. 
				 
zscale*        = 5e+4            ! Scale of v-anomaly               [m]
                                 ! Same design as zamp but for the 
				 ! horizontal scale of the vortex for
				 ! the idealized experiment with bogused
				 ! vortex.
				 
icen*          = 30              ! i-center of the anomaly          []
                                 ! Same functions as zamp but for the 
				 ! center of the idealized vortex. Note
				 ! that the location is defined on WRF
				 ! input mesh, So icen must < nx.
				 
jcen*          = 18              ! j-center of the anomaly          []
                                 ! Same as i-center for the latitude
				 ! direction.
 				 
obs_err_u**    = 1.2             ! obs error for u                  [m/s]
                                 ! Statistical information for the obs
				 ! of the zonal wind at the surface.
				 ! This information is only used in creating
				 ! bogussed obs for the ideal runs. My tests 
				 ! showed that this obs is very important.
				 ! So it is needed to be tuned carefully
				 ! for the idealized experiment. For the
				 ! real-time radionsonde assimilation,
				 ! the obs statistcal information is taken
				 ! directly from output of obsproc.exe (check
				 ! /model/WRFDA/var/obsproc/obsproc.exe
				 ! for more details)
				 
obs_err_v**    = 1.2             ! obs error for v                  [m/s]
                                 ! The same as obs_err_v, but for the 
				 ! meridional wind.
				 
obs_err_t**    = 1.2             ! obs error magnitude              [m]
                                 ! The same as obs_err_t but for temperature
				 
obs_err_q**    = 1e-3            ! obs error for q                  [kg/kg]
                                 ! The same as obs_err_t but for vapor
				 
obs_err_p**    = 1               ! obs error for geopoten perp      [m^2 s^-2]
                                 ! The same as obs_err_t but for geopotential
				 ! height perturbation. 
				 
bgd_err_u**    = 2.5             ! background err for u coldstart   [m]
                                 ! Statistical information for the background
				 ! of the zonal wind at the surface.
				 ! This information is only used in creating
				 ! cold start initialization. Various tests 
				 ! showed that this bgd is very important.
				 ! So it is needed to be tuned carefully
				 ! for the idealized experiment. 
				 
bgd_err_v**    = 2.5             ! background err for v coldstart   [m/s]
                                 ! The same as bgd_err_u but for y-wind
				 
bgd_err_t**    = 2.5             ! background err for T coldstart   [K]
                                 ! The same as bgd_err_u but for temp
				 
bgd_err_q**    = 3e-3            ! background err for q coldstart   [kg/kg]
                                 ! The same as bgd_err_u but for vapor
				 
bgd_err_p**    = 2.              ! background err for p coldstart   [m^2 s^-2]
                                 ! The same as bgd_err_u but for geop height
				 ! perturbation.
				 
model_flag*    = 0               ! flag for model: 0-perfect, 1-imperfect
                                 ! This is for controling the assumption of model
                                 ! error. The implementation for imperfect model
                                 ! can be handle through inflation factor or by
                                 ! by my own scheme. Set 0 if you are not sure
                                 ! how to use it.
				 
ini_flag*      = 1               ! flag for initial condition: 0-perfect, 1-imperfect
                                 ! This option is paired with the model_flag opt
                                 ! above. Set it 1 if you are not sure how to use
                                 ! it.
				 
rscale**       = 1.0             ! scale of localization
                                 ! This is Gaussian scale of the localization for the
                                 ! LEKTF technique. The number denotes the distance 
                                 ! measured by grid points that localization is performed. 
				 
ifactor**      = 1.4             ! inflation factor to increase model error
                                 ! This is to inflate the analysis covariance matrix
                                 ! to take into account the model error.
				 
nme            = 0               ! number of ensemble member for model error calculation
                                 ! This is an option for number of ensemble members that 
                                 ! is used to evaluate the model error by my approach.
                                 ! Dont change if you dont know to use it.
				 
timeout        = 100             ! output interval for the model (steps)
                                 ! This is redundant option. Dont care about it.
				 
tlm_flag       = 1               ! option for the TLM model: 1-first order, 2-second order
                                 ! This is a reduent option. Dont care about it.
				 
no             = 3630            ! number of observation locations = nz*(2*r_obs+1)^2
                                 ! Ignore this option.
				 
ne             = 5               ! number of ensemble members for LETKF
                                 ! This is the number of ensemble members for the LETKF. 
                                 ! Its value is set in the main run script.
				 
nxl**          = 5               ! size of the local patch in x-y direction
                                 ! This is the size of local patch measured in grid points.
                                 ! in horizontal direction
				 
nzl**          = 1               ! size of the local patch in z direction
                                 ! Similar to nxl but for the vertical size of the patch.
                                 ! So far only tested with nzl=1.
				 
slat           = 10.             ! start latitude
                                 ! Ignore this option
				 
nx             = 54              ! grid point in x-direction for mass point
                                 ! Number of grid points in x direction for the model domain
                                 ! that is set in the main run script.
				 
ny             = 54              ! grid point in y-direction for mass point
                                 ! Similar to ny but for longitude direction.
				 
nz             = 30              ! grid point in z-direction for mass point
                                 ! Similiar to nx but for the vertical direction
				 
tfcst          = 48              ! length of forecast               [h]
                                 ! Ignore this value
				 
dt             = 300.            ! model timestep
                                 ! Model time step that is set in the main script.
				 
obs_flag*      = 1               ! option for obs output: 0:full, 1:vortex
                                 ! This is an option for the idealized exp with bogused
                                 ! obs data. For obs_flag=0, the bogussed obs will be
                                 ! generated over the whole domain with step decided
                                 ! by r_obs (see below, e.g., for r_obs=2, bogused obs
                                 ! will be generate at every 2 grid points). For 
                                 ! obs_flag=1, bogused obs will be generated around
                                 ! a bogused vortex center that is centered at (icen
                                 ! jcen) as specified above. In this case, the bogused
                                 ! obs will be generated at all grid points within a
                                 ! domain of the size r_obs around the vortex center.
                                 ! E.g., with r_obs=5, a square domain of size 11x11
                                 ! around vortex center will be used to create bogused
                                 ! obs. 
				 
oscale         = 700e+03         ! radius of obs influence
                                 ! ignore this var.
				 
da_flag**      = 5               ! 0 (all); 1(no u); 2(no v); 3(no t); 4(no q); 5(no p); 
                                 ! 12(no u,v); 123(no u,v,t) ...
				 ! This option is for specifying which variables will be
                                 ! neglected in the LETKF assimilation. E.g. if da_flag
                                 ! = 1, then no U-component will be assimilated. For
                                 ! da_flag=1234, thne U,V,T,Q will not be used.
				 
r_obs**        = 5               ! radius around(icen,jcen) where faked obs is created
                                 ! This controls the size or step of the bogussed 
                                 ! observation. See obs_flag for more details.
				 
para3          = 0               ! extra slot for later new para
para4          = 0               ! extra slot for later new para
para5          = 0               ! extra slot for later new para
para6          = 0               ! extra slot for later new para
para7          = 0               ! extra slot for later new para
para8          = 0               ! extra slot for later new para
para9          = 0               ! extra slot for later new para
para10         = 0               ! extra slot for later new para
para11         = 0               ! extra slot for later new para
para12         = 0               ! extra slot for later new para
para13         = 0               ! extra slot for later new para
para14         = 0               ! extra slot for later new para
para15         = 0               ! extra slot for later new para
para16         = 0               ! extra slot for later new para
para17         = 0               ! extra slot for later new para
para18         = 0               ! extra slot for later new para
para19         = 0               ! extra slot for later new para


CONTACT INFORMATION

Further question or inquiries, please contact: Chanh Kieu, Dept of Meteorology, 
Hanoi College of Science, Vietnam National University, 334 Nguyen Trai, Thanh Xuan 
Hanoi, Vietnam 10000. Email: kieucq@atmos.umd.edu.


EXAMPLES of HOW TO RUN
1. run 1 TC forecast cycle with a single domain and CIMSS DA + Bogus vortex
   with a cold start and an option of blending CIMSS/bogus vortex before doing
   DA.
   - save FNL data under ./data/avn, CIMSS data under obs/CIMSS, and tcvital
     under ./tcvital
   - go to run and cp ./script/namelist_example1.sh ./run/carbonate_namelist.sh
   - go to run and type: ./carbonate_tc_main.sh 201310100000 120 6 COLD CIMSS 24W 1

2. run 1 TC forecast cycle with a single domain and CIMSS DA + Bogus vortex
   with a cold start and an option of blending CIMSS/bogus vortex before doing
   DA and DFI to filter initial noise option.
   - save FNL data under ./data/avn, CIMSS data under obs/CIMSS, and tcvital
     under ./tcvital
   - go to run and cp ./script/namelist_example2.sh ./run/carbonate_namelist.sh
   - go to run and type: ./carbonate_tc_main.sh 201310100000 120 6 COLD CIMSS 24W 1

3. run 1 TC forecast cycle with a single domain without using any DA, but start from
   the same cold start option. Note that this CONTROL run has a set up with a single
   physical option only. Note that the namelist for this exp is the same as exp 2
   because the CONTROL option is given from the input arguments
   - save FNL data under ./data/avn, and tcvital under ./tcvital
   - go to run and cp ./script/namelist_example3.sh ./run/carbonate_namelist.sh
   - go to run and type: ./carbonate_tc_main.sh 201310100000 120 6 COLD CONTROL 24W 1

4. run 1 TC forecast cycle with a single domain and CIMSS DA  with a cold start
   without an option of bogus vortex and DFI.
   - save FNL data under ./data/avn, CIMSS data under obs/CIMSS, and tcvital
     under ./tcvital
   - go to run and cp ./script/namelist_example4.sh ./run/carbonate_namelist.sh
   - go to run and type: ./carbonate_tc_main.sh 201310100000 120 6 COLD CIMSS 24W 1

5. run normal weather forecast with a single domaint and CIMSS DA
   - save FNL data under ./data/avn, CIMSS data under obs/CIMSS
   - go to run and cp ./script/namelist_example5.sh ./run/carbonate_namelist.sh
   - go to run and type: ./carbonate_tc_main.sh

6. run TC forecast with 3 doms and CIMSS DA + Bogus vortex with a cold start and
   an option of blending CIMSS/bogus vortex before doing DA.
   - save FNL data under ./data/avn, CIMSS data under obs/CIMSS, and tcvital
     under ./tcvital
   - go to run and cp ./script/namelist_example6.sh ./run/carbonate_namelist.sh
   - go to run and type: ./carbonate_tc_main.sh 201310100000 120 6 COLD CIMSS 24W 1

7. run TC forecast with 3 doms and blending vortex initialization and radionsondes.
   - save FNL data under ./data/avn, radionsode data under obs/RADS and creating a file
     named stationid.txt under the obs/RADS/yyyymmddhhmm, and tcvital
     under ./tcvital
   - go to run and cp ./script/namelist_example7.sh ./run/carbonate_namelist.sh
   - go to run and type: ./carbonate_tc_main.sh 202108261200 120 6 COLD CIMSS 09L 1



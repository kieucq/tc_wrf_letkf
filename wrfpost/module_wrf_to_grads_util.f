! [HISTORY]
! + August 14, 09: Chanh updated further from previous version with an revised
! tracking for the TC center and rmw. Note that the search
! for rmw is confined to levels 5,6,7. May need to change.
! + August 20, 09: Chanh added a paragraph to find the mean characterisics.
! + August 23, 09: Modified the w_mean by searching for the maximum value
! vertically. Also the area-averaged domain is imposed.
! + August 25, 09: Output couples of more mean measures.
! + August 27, 09: re-strucutre the output name u_r -> tem2... to have more
! options for other mean measusres.
! + Sep 12, 09: + add reflectivity into the codes
! + fix a bug in pressure calculation of THETAE1
! + Oct 26, 09: + add a paragraph to re-find the radius of 34 kt
! and rmw from the axisymmetrical vortex
! + Jan 26, 12: added option to follow the storm center using the obs
! track information. Also output a ATCF file
!
!
!---------------------------------------------------------------------------------
MODULE module_wrf_to_grads_util
USE module_wrf_to_grads_netcdf
CONTAINS
!---------------------------------------------------------------------------------
  subroutine create_grads( grads_file, file_for_time, file_time_index, times, &
                           output_times, variables3d, number_of_3dvar, &
                           variables2d, number_of_2dvar, &
                           desc3d, desc2d, &
                           output_input_grid, use_lowest_heights, grads_levels, &
                           num_grads_levels, case_type, vertical_type, map, &
                           debug1, debug2 )
  implicit none
  include "netcdf.inc"
  integer :: output_times
  character (len=80), intent(in) :: grads_file
  character (len=80), intent(in), dimension(output_times) :: file_for_time, times
  integer, intent(in), dimension(output_times) :: file_time_index
  integer :: number_of_3dvar, number_of_2dvar
  character (len=20), dimension(number_of_3dvar) :: variables3d
  character (len=20), dimension(number_of_2dvar) :: variables2d
  character (len=50), dimension(number_of_3dvar) :: desc3d
  character (len=50), dimension(number_of_2dvar) :: desc2d
  logical, intent(in) :: output_input_grid, use_lowest_heights
  integer :: num_grads_levels
  real, dimension( num_grads_levels ), intent(in) :: grads_levels
  character (len=6) :: case_type
  character (len=1) :: vertical_type
  integer :: map
  logical, intent(in) :: debug1, debug2
  real, allocatable, dimension(:,:,:) :: z, ph, phb
  real, allocatable, dimension(:,:,:) :: p, pb
  real, allocatable, dimension(:,:,:) :: data_out, data_out_z,my_data_out_z
  character (len=30) :: var_to_get, var_to_plot
  integer :: length_var, length_plot
  integer :: it
  integer :: ivar
  integer :: number_of_levels
  integer :: num_vert
  integer, dimension(2) :: loc_of_min_z
  real :: vert_args(100)
  logical :: soil
  real :: MISSING
  integer :: mynx,myny,jrec
  real :: mydx,mydy
  real :: mycen_lat,mycen_lon
  real :: mytruelat1,mytruelat2
  real, allocatable, dimension(:,:) :: myxlat,myxlon
  character (len=40) :: myctlfile, mydatfile
  integer :: ncid, dimid, nf_status
  integer :: rcode, trcode
  real :: value_real
  integer :: nx, ny, nz
  integer :: nlgen
  integer :: ndims, dims(4)
  integer :: i, j, k, hourint
  integer :: icen1,jcen1,icen2,jcen2,imax,jmax
  integer :: ii, jj, kk, hours,day,hour1,day1
  integer :: minutes,mins1
  character (len=40) :: ctlfile, datfile
  character (len=35) :: tdef
  integer, dimension(output_times) :: timestamp, datestamp
  character (len=19) :: wrf_time,wrf_time1
  integer :: irec,rcodep,rcodee
  real, allocatable, dimension(:,:) :: xlat, xlon
  real :: xlat_a(4), xlon_a(4)
  real :: xlat_n_max, xlat_s_max
  real :: xlon_w, xlon_e
  real :: abslatmin, abslatmax
  real :: abslonmin, abslonmax
  real :: truelat1, truelat2, temp
  real :: cen_lat, cen_lon
  real :: centeri, centerj
  integer :: ipoints, jpoints
  integer :: ncref, nrref
  real :: dx, dy
  real :: dxll
  integer :: map_proj
!
! Chanh arrays
!
  real :: minslvl1,minslvl2,vmax0,vmax1,vmax2
  real :: vmin1,vmin2,rmw0,rmw1,rmw2
  real :: slat,elat,slon,elon,lon_obs,lat_obs
  real, allocatable, dimension(:,:) :: slvl_r,sst_r,tem1,tem2,tem3
  real, allocatable, dimension(:,:,:) :: u_r,v_r,t_r,rh_r,w_r,p_r,h_r
  real, allocatable, dimension(:,:,:) :: u_c,u_c1,v_c,v_c1
  integer :: k_shear1,k_shear2,k_w,k_rh
  integer :: k_area,k_rmw,nr,nth,storm_follow
  real :: xc,yc,pi,sta,t_max,z_tmax,a,b
  real :: u_up,u_low,v_up,v_low,z_vmax,r_34kt
  real :: mean_w,mean_rh,mean_shear,mean_sst
  real :: t1,t2,t3,t4,sta1
  integer :: dt_int,debug,tc_move_time
  character*40 :: buf1,buf2
  character*200 :: atcf_string
  parameter (MISSING=1.0E-35)
  soil = .false.
  irec = 1
  jrec = 1
  debug = 0
!==================================================================================
! need to pull out some data to set up dimensions, etc.
!
      nf_status = nf_open (file_for_time(1), nf_nowrite, ncid)
      call handle_err('Error opening file',nf_status)
!
        nf_status = nf_inq_dimid (ncid, 'west_east_stag', dimid)
        call handle_err('west_east_stag',nf_status)
        nf_status = nf_inq_dimlen (ncid, dimid, nx)
        call handle_err('Get NX',nf_status)
        nx = nx-1
!
        nf_status = nf_inq_dimid (ncid, 'south_north_stag', dimid)
        call handle_err('south_north_stag',nf_status)
        nf_status = nf_inq_dimlen (ncid, dimid, ny)
        call handle_err('Get NY',nf_status)
        ny = ny-1
!
      IF ( case_type /= 'static' ) THEN
        nf_status = nf_inq_dimid (ncid, 'bottom_top', dimid)
        call handle_err('bottom_top',nf_status)
        nf_status = nf_inq_dimlen (ncid, dimid, nz)
        call handle_err('Get NZ',nf_status)
      ELSE
        nz = 100
      ENDIF
        nlgen = nz
!
      nf_status = nf_close (ncid)
      call handle_err('Error closing file',nf_status)
!
! Chanh here
!
      print*,'*****************',nx,ny,nz
!
!==================================================================================
! open output files
   ctlfile = trim(grads_file)//".ctl"
   datfile = trim(grads_file)//".dat"
   open (13, file=ctlfile)
   write (13, '("dset ^",a40)') datfile
   write (13, '("undef 1.e35")')
     open (15, file=datfile, form="unformatted",access="direct", &
           recl=(nx*ny))
!==================================================================================
! How will the vertical coordinate look like
  IF ( (.not. output_input_grid) .and. (.not. use_lowest_heights)) THEN
    ! we have user supplied vertical levels - CAN WE DO IT?
       nf_status = nf_open (file_for_time(1), nf_nowrite, ncid)
       call handle_err('Error opening file',nf_status)
       if ( vertical_type == 'p' ) then
          rcode = nf_inq_varid ( ncid, "P", dimid )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PB", dimid )
          if ( nf_status == 0 ) rcode = trcode
          rcodep = rcode
          rcodee = nf_inq_varid ( ncid, "ZNW", dimid )
          if (rcodee.eq.0) rcodee = nf_inq_varid ( ncid, "MU0", dimid )
       else if ( vertical_type == 'z' ) then
          rcode = nf_inq_varid ( ncid, "PH", dimid )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PHB", dimid )
          if ( nf_status == 0 ) rcode = trcode
       endif
       nf_status = nf_close (ncid)
       call handle_err('Error closing file',nf_status)
       if ( rcode.eq.0.or.rcodee.eq.0 ) then
           ! we can do it
           write(6,*) ' '
           write(6,*) " Asking to interpolate to ",vertical_type," levels - we can do that"
           write(6,*) ' '
           number_of_levels = num_grads_levels
           vert_args(1:number_of_levels) = grads_levels(1:number_of_levels)
       else
           ! no interp, just put out computational grid
           write(6,*) ' '
           write(6,*) ' FIELDS MISSING TO INTERPOLATE TO USER SPECIFIED VERTICAL LEVELS'
           write(6,*) ' WILL OUTPUT ON MODEL GRID'
           write(6,*) ' '
           number_of_levels = nz
           vertical_type = 'n'
       endif
  END IF
  IF ( (output_input_grid) .and. (use_lowest_heights)) THEN
    ! use lowest column for z heights of grids - CAN WE DO IT?
       nf_status = nf_open (file_for_time(1), nf_nowrite, ncid)
       call handle_err('Error opening file',nf_status)
       rcode = nf_inq_varid ( ncid, "P", dimid )
       trcode = rcode
       rcode = nf_inq_varid ( ncid, "PB", dimid )
       if ( nf_status == 0 ) rcode = trcode
       nf_status = nf_close (ncid)
       call handle_err('Error closing file',nf_status)
       if ( rcode == 0 ) then
           ! we can do it
           write(6,*) ' '
           write(6,*) " Asking to interpolate to lowerst h level -  we can do that"
           write(6,*) ' '
           allocate( z(nx,ny,nz) )
           allocate( ph(nx,ny,nz+1) )
           allocate( phb(nx,ny,nz+1) )
           ! get base and perturbation height at full eta levels:
           call get_var_3d_real_cdf( file_for_time(1),'PH',ph, &
                                     nx,ny,nz+1,1,debug2 )
           call get_var_3d_real_cdf( file_for_time(1),'PHB',phb, &
                                     nx,ny,nz+1,1,debug2 )
           ! compute Z at half levels:
           ph = (ph+phb)/9.81
           z = 0.5*(ph(:,:,1:nz)+ph(:,:,2:nz+1))
           z = z/1000. ! convert to kilometers
           number_of_levels = nz
           vertical_type = 'z'
           loc_of_min_z = minloc(z(:,:,1))
           vert_args(1:number_of_levels) = &
                     z(loc_of_min_z(1),loc_of_min_z(2),1:number_of_levels)
           vert_args(1) = vert_args(1) + 0.002
           vert_args(nz) = vert_args(nz) - 0.002
           deallocate( z )
           deallocate( ph )
           deallocate( phb )
       else
           ! no interp, just put out computational grid
           write(6,*) ' '
           write(6,*) ' FIELDS MISSING TO INTERPOLATE TO HEIGHT LEVELS'
           write(6,*) ' WILL OUTPUT ON MODEL GRID'
           write(6,*) ' '
           number_of_levels = nz
           vertical_type = 'n'
       endif
  END IF
  IF ( output_input_grid .and. (.not. use_lowest_heights)) THEN
    ! no interp, just put out computational grid
    write(6,*) " Will use model levels for output"
    number_of_levels = nz
  ENDIF
!==================================================================================
  if(debug1) then
    write(6,*) ' number of levels = ',number_of_levels
    do k=1, number_of_levels
      write(6,*) ' k, vert_args(k) ',k,vert_args(k)
    enddo
  end if
!==================================================================================
! work out times and time differences
   tdef = '        11 linear 00z01jan2000  1hr'
   IF ( case_type /= 'static' ) THEN
       hourint = 0
       do it = 1, output_times
           day1 = day
           call time_calc(times(it), timestamp(it), datestamp(it), debug2 , &
                          tdef, it, case_type, hours,hour1,minutes,mins1,day )
           if (it.eq.2.and.hourint.eq.0) then
            if (hours.lt.hour1.and.hours.eq.0) then
             hourint = 24 - hour1
            else
             hourint = hours - hour1
            endif
           endif
       enddo
       write (tdef(9:10),'(i2)') output_times
       print*,'hour interval is',hourint
       if (hourint.eq.0) then
        print*,' WARNING of hour interval... it is reset to 1'
        hourint = 1
       endif
   ENDIF
!==================================================================================
! try to get map information
  IF (map == 1) THEN ! display the real data
      call get_gl_att_int_cdf( file_for_time(1), 'MAP_PROJ', map_proj, debug2 )
      if ( map_proj /= 0 ) then
      ! get more map parameters first
         call get_gl_att_real_cdf( file_for_time(1), 'DX', dx, debug2 )
         call get_gl_att_real_cdf( file_for_time(1), 'DY', dy, debug2 )
         call get_gl_att_real_cdf( file_for_time(1), 'CEN_LAT', cen_lat, debug2 )
         call get_gl_att_real_cdf( file_for_time(1), 'TRUELAT1', truelat1, debug2 )
         call get_gl_att_real_cdf( file_for_time(1), 'TRUELAT2', truelat2, debug2 )
         nf_status = nf_open (file_for_time(1), nf_nowrite, ncid)
         call handle_err('Error opening file',nf_status)
         rcode = NF_GET_ATT_REAL(ncid, nf_global, 'STAND_LON', value_real )
         nf_status = nf_close (ncid)
         call handle_err('Error closing file',nf_status)
         if ( rcode == 0) then
           call get_gl_att_real_cdf( file_for_time(1), 'STAND_LON', cen_lon, debug2 )
         else
           write(6,*) ' #####                                           #####'
           write(6,*) ' ##### NOTE probably dealing with version 1 data #####'
           write(6,*) ' ##### Using CEN_LON in calculations             #####'
           write(6,*) ' ##### Please check project of GrADS data        #####'
           write(6,*) ' #####                                           #####'
           write(6,*) ' '
           call get_gl_att_real_cdf( file_for_time(1), 'CEN_LON', cen_lon, debug2 )
         endif
         allocate( xlat(nx,ny) )
         allocate( xlon(nx,ny) )
         call get_var_2d_real_cdf( file_for_time(1), 'XLAT', xlat, nx,ny, 1, debug2 )
         call get_var_2d_real_cdf( file_for_time(1), 'XLONG',xlon, nx,ny, 1, debug2 )
      end if
      if (map_proj == 0 .OR. map_proj == 3) then
      ! NO or MERCATOR
           write(13,'("xdef ",i4," linear ",f9.4," ",f8.4)') &
                       nx,xlon(1,1),(abs(xlon(1,1)-xlon(nx,ny))/(nx-1))
           write(13,'("ydef ",i4," linear ",f9.4," ",f8.4)') &
!! ny,xlat(1,1),(abs(xlat(1,1)-xlat(nx,ny))/(ny-1))
                       ny,xlat(1,1),(abs(xlon(1,1)-xlon(nx,ny))/(nx-1))
! write(13,'("xdef ",i4," levels ",f9.4," ",f8.4)')nx
! do i = 1,nx
! write(13,'(" ",f10.5)')xlon(i,1)
! enddo
! write(13,'("ydef ",i4," levels ",f9.4," ",f8.4)')ny
! do j = 1,ny
! write(13,'(" ",f10.5)')xlat(1,j)
! enddo
           if (vertical_type == 'n' ) then
             write (13, '("zdef  ",i3, " linear 1 1")') number_of_levels
           else
             write(13,'("zdef  ",i3, " levels  ")') number_of_levels
             do k = 1,number_of_levels
               write(13,'(" ",f10.5)') vert_args(k)
             enddo
           endif
      else if (map_proj == 1) then
    ! LAMBERT-CONFORMAL
    ! make sure truelat1 is the larger number
          if (truelat1 < truelat2) then
             if (debug2) write (6,*) ' switching true lat values'
             temp = truelat1
             truelat1 = truelat2
             truelat2 = temp
          endif
          xlat_a(1) = xlat(1,1)
          xlat_a(2) = xlat(1,ny)
          xlat_a(3) = xlat(nx,1)
          xlat_a(4) = xlat(nx,ny)
          xlon_a(1) = xlon(1,1)
          xlon_a(2) = xlon(1,ny)
          xlon_a(3) = xlon(nx,1)
          xlon_a(4) = xlon(nx,ny)
          abslatmin = 99999.
          abslatmax = -99999.
          abslonmin = 99999.
          abslonmax = -99999.
          do i=1,4
            abslatmin=min(abslatmin,xlat_a(i))
            abslonmin=min(abslonmin,xlon_a(i))
            abslatmax=max(abslatmax,xlat_a(i))
            abslonmax=max(abslonmax,xlon_a(i))
          enddo
          xlat_s_max = -90.
          xlat_n_max = -90.
         do i = 1, nx
           xlat_s_max = max (xlat_s_max,xlat(i,1))
           xlat_n_max = max (xlat_n_max,xlat(i,ny))
         enddo
         xlon_w = xlon(1, ny)
         xlon_e = xlon(nx, ny)
         centeri = int((cen_lon-xlon_w)*((nx-1)/(xlon_e-xlon_w))+1)
         centerj = ((cen_lat-xlat_s_max)*((ny)/(xlat_n_max-xlat_s_max)))
         dxll = (dx/1000.)/111./2.
         ipoints = int((abslatmax-abslatmin+2)/dxll)
         jpoints = int((abslonmax-abslonmin+2)/dxll)
           write(13,'("pdef ",i3," ",i3," lcc ",f7.3," ",f8.3," ",&
& f8.3," ",f8.3," ",f4.0," ",f4.0," ",f8.3," ",&
& f6.0," ",f7.0)')&
                       nx,ny,cen_lat,cen_lon,centeri,centerj,&
                       truelat1,truelat2,cen_lon,dx,dy
           write(13,'("xdef ",i4," linear ",f6.1," ",f12.8)') jpoints, &
                       (abslonmin-1.),dxll
           write(13,'("ydef ",i4," linear ",f6.1," ",f12.8)') ipoints, &
                       (abslatmin-1.),dxll
           if (vertical_type == 'n' ) then
             write (13, '("zdef  ",i3, " linear 1 1")') number_of_levels
           else
             write(13,'("zdef  ",i3, " levels  ")') number_of_levels
             do k = 1,number_of_levels
               write(13,'(" ",f10.5)') vert_args(k)
             enddo
           endif
      elseif (map_proj == 2) then
        ! POLAR STEREO
         xlat_a(1) = xlat(1,1)
         xlat_a(2) = xlat(1,ny)
         xlat_a(3) = xlat(nx,1)
         xlat_a(4) = xlat(nx,ny)
         xlon_a(1) = xlon(1,1)
         xlon_a(2) = xlon(1,ny)
         xlon_a(3) = xlon(nx,1)
         xlon_a(4) = xlon(nx,ny)
         abslatmin = 99999.
         abslatmax = -99999.
         abslonmin = 99999.
         abslonmax = -99999.
         do i=1,4
           abslatmin=min(abslatmin,xlat_a(i))
           abslonmin=min(abslonmin,xlon_a(i))
           abslatmax=max(abslatmax,xlat_a(i))
           abslonmax=max(abslonmax,xlon_a(i))
         enddo
         dxll = (dx/1000.)/111./2.
         ipoints = int((abslatmax-abslatmin+2)/dxll) + 20
         jpoints = int((abslonmax-abslonmin+2)/dxll) + 20
         ncref = nx/2
         nrref = ny/2
           write(13,'("pdef ",i3," ",i3," ops ",f8.3," ",f8.3," ",f12.4," ", &
& f12.4," ",i4.1," ",i4.1," ",f12.2," ",f12.2)')        &
                  nx,ny,xlat(ncref,nrref), xlon(ncref,nrref),dx*0.1,dy*0.1, &
                  ncref,nrref,dx,dy
           write(13,'("xdef ",i4," linear ",f6.1," ",f12.8)') jpoints, &
                        (abslonmin-1.),dxll
           write(13,'("ydef ",i4," linear ",f6.1," ",f12.8)') ipoints, &
                        (abslatmin-1.),dxll
           if (vertical_type == 'n' ) then
             write (13, '("zdef  ",i3, " linear 1 1")') number_of_levels
           else
             write(13,'("zdef  ",i3, " levels  ")') number_of_levels
             do k = 1,number_of_levels
               write(13,'(" ",f10.5)') vert_args(k)
             enddo
           endif
       endif ! END of map projections
  ELSE ! display the ideal data
     if ( case_type == 'ideal' .and. nx == 2) then
      write (13, '("xdef     1 linear 0 0.0001")')
     else
      write (13, '("xdef  ",i3, " linear 0 0.0001")') nx
     endif
     if ( case_type == 'ideal' .and. ny == 2) then
      write (13, '("ydef     1 linear 0 0.0001")')
     else
      write (13, '("ydef  ",i3, " linear 0 0.0001")') ny
     endif
     if (vertical_type == 'n' ) then
        write (13, '("zdef  ",i3, " linear 1 1")') number_of_levels
     else
        write(13,'("zdef  ",i3, " levels  ")') number_of_levels
        do k = 1,number_of_levels
          write(13,'(" ",f10.5)') vert_args(k)
        enddo
     endif
  ENDIF
   write (13, '("tdef",a35)') tdef
!==================================================================================
! create the grads file
  if(debug1) then
    write(6,*) ' number of 3d variables to process = ',number_of_3dvar
    write(6,*) ' number of 2d variables to process = ',number_of_2dvar
    do k=1,number_of_3dvar
      write(6,*) k, variables3d(k)
    enddo
    do k=1,number_of_2dvar
      write(6,*) k+number_of_3dvar, variables2d(k)
    enddo
    write(6,*) ' output times '
    do k=1,output_times
      write(6,*) k, timestamp(k)
    enddo
    write(6,*) ' horizontal dims ',nx,ny
    write(6,*) ' vertical dims nl '
    write(6,*) 'vert coord: ',vert_args(1:number_of_levels)
  endif
!==================================================================================
! First check to see if we have all the variables
     write(6,*) ' CHECK to make sure we have all the variables in the input file'
     call check_all_variables ( file_for_time(1), &
                                variables3d, desc3d, number_of_3dvar, &
                                variables2d, desc2d, number_of_2dvar, &
                                debug1 )
   write (13, '("vars  ",i3)') number_of_3dvar+number_of_2dvar
!==================================================================================
! first, get some base-state-stuff
  if ( vertical_type == 'p' ) then
    allocate( p (nx, ny, nz) )
    allocate( pb(nx, ny, nz) )
    if (rcodep.eq.0) call get_var_3d_real_cdf( file_for_time(1),'PB',pb,nx,ny,nz,1,debug2 )
  endif
  if ( vertical_type == 'z' ) then
    allocate( z (nx, ny, nz) )
    allocate( ph (nx, ny, nz+1) )
    allocate( phb(nx, ny, nz+1) )
    call get_var_3d_real_cdf( file_for_time(1),'PHB',phb,nx,ny,nz+1,1,debug2 )
  endif
!=============================TIME LOOP STARTS HERE================================
  print *,' '
  print *,'    ###############  Begin time loop  ###############  '
  print *,'               Number of output time is',output_times
  print *,' '
!
! Chanh here...finding the center of hurricanes
!
  open(26,file='wrf2grads_atcf.txt')
  open(27,file='wrf2grads_evolution.txt')
  open(28,file='wrf2grads_track.txt')
  k_shear1=2
  k_shear2=10
  k_w=10
  k_rh=10
  k_area=10
  k_rmw=2
  open(29,file='wrf2grads_mean_in.txt')
  open(30,file='wrf2grads_mean_out.txt')
  read(29,*)buf1,buf2,k_shear1
  read(29,*)buf1,buf2,k_shear2
  read(29,*)buf1,buf2,k_w
  read(29,*)buf1,buf2,k_rh
  read(29,*)buf1,buf2,k_area
  read(29,*)buf1,buf2,k_rmw
  read(29,*)buf1,buf2,storm_follow
  read(29,*)buf1,buf2,tc_move_time
  if (debug.eq.1) then
   print*,' Shear low level              =',k_shear1
   print*,' Shear up level               =',k_shear2
   print*,' Level for computing mean w   =',k_w
   print*,' Level for computing mean RH  =',k_rh
   print*,' Radius of area averaged      =',k_area
   print*,' level of searching RMW       =',k_rmw
   print*,' storm following option       =',storm_follow
   print*,' time to update tc search     =',tc_move_time
  endif
  if (storm_follow.eq.1) then
   open(31,file='fort.10',status='old')
  endif
  do it = 1, output_times
! first, get p/z if needed
    if ( vertical_type.eq.'p'.and.rcodep.eq.0) then
      call get_var_3d_real_cdf( file_for_time(it),'P',p, nx, ny, nz, &
                                file_time_index(it),debug2 )
      p = p+pb
    else if (vertical_type.eq.'p'.and.rcodee.eq.0) then
      var_to_plot = "PRES_INPUT"
      length_plot = len_trim(var_to_plot)
      call g_output_3d (file_for_time(it), file_time_index(it), it, &
                          var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                          p, debug2, missing)
    endif
    if ( vertical_type == 'z' ) then
      call get_var_3d_real_cdf( file_for_time(it),'PH',ph, nx, ny, nz+1, &
                              file_time_index(it),debug2 )
      ph = (ph+phb)/9.81
      z = 0.5*(ph(:,:,1:nz)+ph(:,:,2:nz+1))
      ! need to convert to kilometers for coordinate
      z = z/1000.
    endif
!=============================DEALING WITH 3D VARIABLES============================
    do ivar = 1, number_of_3dvar
      var_to_get = variables3d(ivar)
      var_to_plot = var_to_get
      call check_special_variable( var_to_get )
      length_var = len_trim(var_to_get)
      length_plot = len_trim(var_to_plot)
      if(debug2) write(6,*) ' getting dims for ',var_to_get(1:length_var)
      call get_dims_cdf( file_for_time(it), var_to_get(1:length_var), &
                         dims, ndims, debug2 )
      if ( dims(3) < nlgen ) then
        num_vert = dims(3)
        soil = .true.
      endif
      if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
      if (soil) then
        allocate ( data_out_z (nx, ny, num_vert) )
        call get_var_3d_real_cdf( file_for_time(it),var_to_get(1:length_var), &
                                  data_out_z, nx, ny, num_vert, &
                                  file_time_index(it),debug2 )
      else
        allocate ( data_out (nx, ny, nz) )
        allocate ( data_out_z (nx, ny, number_of_levels) )
        call g_output_3d (file_for_time(it), file_time_index(it), it, &
                          var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                          data_out, debug2, missing)
        if(debug2) write(6,*) 'number_of_levels = ', number_of_levels
        if ( vertical_type == 'p' ) then
            call interp_to_z( data_out , nx, ny, nz, &
                              data_out_z, nx, ny, number_of_levels, &
                              p/100., vert_args, missing, &
                              vertical_type, debug1 )
        else if ( vertical_type == 'z' ) then
            call interp_to_z( data_out , nx, ny, nz, &
                              data_out_z, nx, ny, number_of_levels, &
                              z, vert_args, missing, &
                              vertical_type, debug1 )
        else
            data_out_z = data_out
        endif
        num_vert = number_of_levels
      deallocate ( data_out )
      endif
        if(debug1) write(6,*) ' writing out variable, time ',ivar,it
        wrf_time = times(it)
        write(6,*) ' time ',wrf_time,', output variable ',var_to_plot(1:length_plot)
        if (it == 1) write(13,'(a15,i3," 0 ",a50)') var_to_plot, num_vert, &
                           desc3d(ivar)
       if ( case_type == 'ideal' .and. ny == 2 .and. nx == 2 )then
        do kk=1,num_vert
          write(15,rec=irec) ((data_out_z(ii,jj,kk),ii=1,1),jj=1,1)
          irec=irec+1
        enddo
       else if ( case_type == 'ideal' .and. ny == 2 .and. nx .ne. 2 )then
        do kk=1,num_vert
          write(15,rec=irec) ((data_out_z(ii,jj,kk),ii=1,nx),jj=1,1)
          irec=irec+1
        enddo
       else if ( case_type == 'ideal' .and. nx == 2 .and. ny .ne. 2 )then
        do kk=1,num_vert
          write(15,rec=irec) ((data_out_z(ii,jj,kk),ii=1,1),jj=1,ny)
          irec=irec+1
        enddo
       else
        do kk=1,num_vert
          write(15,rec=irec) ((data_out_z(ii,jj,kk),ii=1,nx),jj=1,ny)
          irec=irec+1
        enddo
       endif
      deallocate ( data_out_z )
      soil = .false.
    enddo ! END of 3D LOOP
!=============================DEALING WITH 2D VARIABLES============================
    do ivar = 1, number_of_2dvar
      var_to_get = variables2d(ivar)
      var_to_plot = var_to_get
      length_var = len_trim(var_to_get)
      length_plot = len_trim(var_to_plot)
      allocate( data_out(nx, ny, 1) )
      if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
      call g_output_2d (file_for_time(it), file_time_index(it), it, &
                        var_to_plot, length_plot, nx,ny,nz, &
                        data_out, debug2)
      if(debug1) write(6,*) ' writing out variable, time ',ivar+number_of_3dvar,it
      wrf_time = times(it)
      write(6,*) ' time ',wrf_time,', output variable ',var_to_plot(1:length_plot)
      if (it == 1) write(13,'(a15," 0  0 ",a50)') var_to_plot, desc2d(ivar)
       if ( case_type == 'ideal' .and. ny == 2 .and. nx == 2 )then
          write(15,rec=irec) ((data_out(ii,jj,1),ii=1,1),jj=1,1)
          irec=irec+1
       elseif ( case_type == 'ideal' .and. ny == 2 .and. nx .ne. 2 )then
          write(15,rec=irec) ((data_out(ii,jj,1),ii=1,nx),jj=1,1)
          irec=irec+1
       else if ( case_type == 'ideal' .and. nx == 2 .and. ny .ne. 2 )then
          write(15,rec=irec) ((data_out(ii,jj,1),ii=1,1),jj=1,ny)
          irec=irec+1
       else
          write(15,rec=irec) ((data_out(ii,jj,1),ii=1,nx),jj=1,ny)
          irec=irec+1
       endif
      deallocate(data_out)
    enddo ! END OF 2D VARIABLES
!=============================DEALING WITH SCALAR VARIABLES============================
     print*,' Finding the center of hurricane.....'
     xlat = 0.
     xlon = 0.
     call get_var_2d_real_cdf( file_for_time(it), 'XLAT' ,xlat, nx,ny,file_time_index(it), debug2 )
     call get_var_2d_real_cdf( file_for_time(it), 'XLONG',xlon, nx,ny,file_time_index(it), debug2 )
     slat = xlat(1,1)
     elat = xlat(1,ny)
     slon = xlon(1,1)
     elon = xlon(nx,1)
!
     var_to_get = 'slvl'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( data_out(nx, ny, 1) )
     allocate( slvl_r(nx,ny))
     call g_output_2d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, &
                       data_out, debug2)
     do j = 1,ny
      do i = 1,nx
       slvl_r(i,j) = data_out(i,j,1)
      enddo
     enddo
     deallocate(data_out)
!
     var_to_get = 'U'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( u_r(nx, ny, nz) )
     if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
     call g_output_3d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                       u_r, debug2,missing)
     u_low = 0.
     u_up = 0.
     do j = 1,ny
      do i = 1,nx
       u_low = u_low &
                 + (u_r(i,j,k_shear1-1)+u_r(i,j,k_shear1)+u_r(i,j,k_shear1+1))/3.
       u_up = u_up &
                 + (u_r(i,j,k_shear2-1)+u_r(i,j,k_shear2)+u_r(i,j,k_shear2+1))/3.
      enddo
     enddo
     u_low = u_low/(nx*ny)
     u_up = u_up/(nx*ny)
!
     var_to_get = 'V'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( v_r(nx, ny, nz) )
     if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
     call g_output_3d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                       v_r, debug2,missing)
!
     var_to_get = 'Z'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( h_r(nx, ny, nz) )
     if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
     call g_output_3d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                       h_r, debug2,missing)
!
     var_to_get = 'THETA'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( t_r(nx, ny, nz) )
     if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
     call g_output_3d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                       t_r, debug2,missing)
     v_low = 0.
     v_up = 0.
     do j = 1,ny
      do i = 1,nx
       v_low = v_low &
                 + (v_r(i,j,k_shear1-1)+v_r(i,j,k_shear1)+v_r(i,j,k_shear1+1))/3.
       v_up = v_up &
                 + (v_r(i,j,k_shear2-1)+v_r(i,j,k_shear2)+v_r(i,j,k_shear2+1))/3.
      enddo
     enddo
     v_low = v_low/(nx*ny)
     v_up = v_up/(nx*ny)
!
     if (storm_follow.eq.1.and.(mod(it,tc_move_time).eq.1.or.tc_move_time.eq.1)) then
      read(31,*,end=103)lon_obs,lat_obs
      slat = lat_obs - 5
      elat = lat_obs + 5
      slon = lon_obs - 5
      elon = lon_obs + 5
      print*,' best track lon of center is: ',lon_obs
      print*,' best track lat of center is: ',lat_obs
     endif
103 print*,' slon =',slon,'  elon =',elon
     print*,' slat =',slat,'  elat =',elat
!
! finding minimum slvl first
!
     call p_min(slvl_r,nx,ny,minslvl1,dx,dy,slat,elat,slon,elon,icen1,jcen1,xlat,xlon)
     print*,' pmin   =',minslvl1
     print*,' i pmin =',icen1
     print*,' j pmin =',jcen1
!
! refine with minimum of speed
!
     if (minslvl1.gt.0) then
      call v_min(u_r,v_r,k_rmw,nx,ny,nz,dx,dy,icen1,jcen1,icen2,jcen2,vmin1)
!
! search for the RMW
!
      call v_max(u_r,v_r,h_r,k_rmw,nx,ny,nz,dx,dy,imax,jmax,vmax1,z_vmax,r_34kt,icen2,jcen2)
!
! compute RMW
!
      rmw1 = sqrt(((imax-icen1)*dx)**2+((jmax-jcen1)*dy)**2)
      rmw2 = sqrt(((imax-icen2)*dx)**2+((jmax-jcen2)*dy)**2)
      if (debug.eq.1) then
       print*,' Vmin   =',vmin1
       print*,' i Vmin =',icen2
       print*,' j Vmin =',jcen2
       print*,' Vmax   =',vmax1
       print*,' i Vmax =',imax
       print*,' j Vmax =',jmax
       print*,' z_vmax =',z_vmax
       print*,' r_34kt =',r_34kt
       print*,' rmw1   =',rmw1
       print*,' rmw2   =',rmw2
      endif
     else
      print*,'Can not find the TC center...setting everything to missing'
      minslvl1 = -99
      vmin1 = -99
      vmax1 = -99
      r_34kt = -99
      rmw1 = -99
      rmw2 = -99
      icen1 = 1
      jcen1 = 1
      icen2 = 1
      jcen2 = 1
      imax = 1
      jmax = 1
      z_vmax = 1
     endif
!
! create a atcf string
!
     dt_int = (it-1)*hourint
     wrf_time1 = times(1)
     call atcf_format(atcf_string,xlat,xlon,vmax1,minslvl1,r_34kt,icen1,jcen1,wrf_time1,nx,ny,dt_int)
     if (icen1.ne.1) then
      write(26,'(1A108)')atcf_string(1:108)
      write(27,'(2I5,F13.3,2I5,4F13.3,A20)') &
           icen1,jcen1,minslvl1,icen2,jcen2,vmax1,rmw0/1000,rmw1/1000,rmw2/1000,wrf_time
      write(28,'(I4,2F13.3)')(it-1)*hourint,xlon(icen1,jcen1),xlat(icen1,jcen1)
      print*,it,hourint
     endif
!
! compute the warm core magnitude and height
!
     t_max = 0.
     z_tmax = 0.
     do k = 1,nz
      t1 = 0.
      do i = 1,nx
       do j = 1,ny
        t1 = t1 + t_r(i,j,k)
       enddo
      enddo
      t1 = t1/(nx*ny)
      do i = icen1-int(1.8e5/dx),icen1+int(1.8e5/dx)
       do j = jcen1-int(1.8e5/dy),jcen1+int(1.8e5/dy)
        t2 = t_r(i,j,k)-t1
        if (t_max.lt.t2.and.h_r(i,j,k).lt.1.4e4) then
         t_max = t2
         z_tmax = h_r(i,j,k)
        endif
       enddo
      enddo
     enddo
!
! find the mean SST...welll this is TSK, not SST.. .weird
!
     allocate(sst_r(nx,ny))
     sst_r = 0.
     call get_var_2d_real_cdf( file_for_time(it), 'SST' ,sst_r, nx,ny,file_time_index(it), debug2 )
     mean_sst = 0.
     do i = max(1,icen1-k_area),max(icen1+k_area,nx)
      do j = max(1,jcen1-k_area),max(jcen1+k_area,ny)
       mean_sst = mean_sst + sst_r(i,j)
      enddo
     enddo
     mean_sst = mean_sst/(nx*ny)
!
     var_to_get = 'RH'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( rh_r(nx, ny, nz) )
     if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
     call g_output_3d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                       rh_r, debug2,missing)
     mean_rh = 0.
     do i = max(1,icen1-k_area),max(icen1+k_area,nx)
      do j = max(1,jcen1-k_area),max(jcen1+k_area,ny)
       mean_rh = mean_rh + rh_r(i,j,k_rh)
      enddo
     enddo
     mean_rh = mean_rh/(nx*ny)
!
     var_to_get = 'W'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( w_r(nx, ny, nz) )
     if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
     call g_output_3d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                       w_r, debug2,missing)
     mean_w = 0.
     do k = 1,nz
      do i = max(1,icen1-k_area),max(icen1+k_area,nx)
       do j = max(1,jcen1-k_area),max(jcen1+k_area,ny)
        t1 = t1 + w_r(i,j,k)
       enddo
      enddo
      t1 = t1/(nx*ny)
      if (mean_w.lt.t1) mean_w = t1
     enddo
!
     var_to_get = 'P'
     var_to_plot = var_to_get
     length_var = len_trim(var_to_get)
     length_plot = len_trim(var_to_plot)
     allocate( p_r(nx, ny, nz) )
     if(debug1) write(6,*) ' getting data for ',var_to_get(1:length_var)
     call g_output_3d (file_for_time(it), file_time_index(it), it, &
                       var_to_plot, length_plot, nx,ny,nz, dx,dy, &
                       p_r, debug2,missing)
     if (debug.eq.1) then
      print*,' t_max  =',t_max
      print*,' z_tmax =',z_tmax
      print*,' mean_sst =',mean_sst
      print*,' mean_rh  =',mean_rh
      print*,' mean_w   =',mean_w
      print*,' k_shear1  =',p_r(1,1,k_shear1),' (mb)'
      print*,' k_shear2  =',p_r(1,1,k_shear2),' (mb)'
      print*,' k_w       =',p_r(1,1,k_w),' (mb)'
      print*,' k_rh      =',p_r(1,1,k_rh),' (mb)'
      print*,' k_rmw     =',p_r(1,1,k_rmw),' (mb)'
     endif
     mean_shear = sqrt((u_up - u_low)**2+(v_up - v_low)**2)
     write(30,'(13F13.3,A20)')mean_w,mean_rh,mean_shear,minslvl1,vmax0,rmw0/1000, &
                              mean_sst,z_vmax/1000,r_34kt/1000,t_max,z_tmax/1000, &
                              xlon(icen1,jcen1),xlat(icen1,jcen1),wrf_time
     deallocate(w_r,rh_r,sst_r,p_r,h_r,t_r)
     deallocate(slvl_r,u_r,v_r)
    print*," "
  enddo ! END of TIME LOOP
!==================================================================================
! we're finished - clean up
  if ( vertical_type == 'p' ) then
    deallocate( p )
    deallocate( pb )
  endif
  if ( vertical_type == 'z' ) then
    deallocate( z )
    deallocate( ph )
    deallocate( phb )
  endif
   write(13, '("endvars")' )
   close (15)
  end subroutine create_grads
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
     subroutine atcf_format(atcf_string,xlat,xlon,vmax,pmin,r34,ic,jc,wrf_time,nx,ny,dt)
     implicit none
     integer nx,ny,ic,jc
     character*200 atcf_string
     character*19 wrf_time
     character*4 tmp_year
     character*2 tmp_month
     character*2 tmp_day
     character*2 tmp_hour
     real xlon(nx,ny),xlat(nx,ny),vmax,pmin,r34
     integer dt,atcf_tmp
     atcf_string='BB, NN, YYYYMMDDHH,   , VWRF,   0, LATN,  LONW,   0,    0, TC,  34, NEQ,    0,    0,    0,    0,'
!
! write lon
!
     atcf_tmp = int(abs(xlon(ic,jc))*10)
     if (atcf_tmp.lt.10) then
      write(atcf_string(45:45),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(44:45),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(43:45),'(1I3)')atcf_tmp
     elseif (atcf_tmp.lt.10000) then
      write(atcf_string(42:45),'(1I4)')atcf_tmp
     else
      print*,'atcf format for lon is wrong...stop'
      stop
     endif
     if (xlon(ic,jc).gt.0) then
      write(atcf_string(46:46),'(1A1)')'E'
     else
      write(atcf_string(46:46),'(1A1)')'W'
     endif
!
! write lat
!
     atcf_tmp = int(abs(xlat(ic,jc))*10)
     if (atcf_tmp.lt.10) then
      write(atcf_string(38:38),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(37:38),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(36:38),'(1I3)')atcf_tmp
     else
      print*,'atcf format for lat is wrong...stop'
      stop
     endif
     if (xlat(ic,jc).gt.0) then
      write(atcf_string(39:39),'(1A1)')'N'
     else
      write(atcf_string(39:39),'(1A1)')'S'
     endif
!
! write forecast hour
!
     if (dt.lt.10) then
      write(atcf_string(33:33),'(1I1)')dt
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(32:33),'(1I2)')dt
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(31:33),'(1I3)')dt
     else
      print*,'atcf format for dt is wrong...stop'
      stop
     endif
!
! write datestamp
!
     read(wrf_time(1:4),'(1A4)')tmp_year
     read(wrf_time(6:7),'(1A2)')tmp_month
     read(wrf_time(9:10),'(1A2)')tmp_day
     read(wrf_time(12:13),'(1A2)')tmp_hour
     write(atcf_string(9:12),'(1A4)')tmp_year
     write(atcf_string(13:14),'(1A2)')tmp_month
     write(atcf_string(15:16),'(1A2)')tmp_day
     write(atcf_string(17:18),'(1A2)')tmp_hour
!
! write vmax in kt
!
     atcf_tmp = nint(vmax*2)
     if (atcf_tmp.lt.10) then
      write(atcf_string(51:51),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(50:51),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(49:51),'(1I3)')atcf_tmp
     else
      print*,'atcf format for vmax is wrong...stop'
      stop
     endif
!
! write pmin
!
     atcf_tmp = int(pmin)
     if (atcf_tmp.lt.10) then
      write(atcf_string(57:57),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(56:57),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(55:57),'(1I3)')atcf_tmp
     elseif (atcf_tmp.lt.10000) then
      write(atcf_string(54:57),'(1I4)')atcf_tmp
     else
      print*,'atcf format for pmin is wrong...stop'
      stop
     endif
!
! r34 kt radius in nml
!
     atcf_tmp = int(r34*1.6/1000)
     if (atcf_tmp.lt.10) then
      write(atcf_string(77:77),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(76:77),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(75:77),'(1I3)')atcf_tmp
     elseif (atcf_tmp.lt.10000) then
      write(atcf_string(74:77),'(1I4)')atcf_tmp
     else
      print*,'atcf format for r34 is wrong...stop'
      stop
     endif
     return
     end subroutine atcf_format
  subroutine read_time_list( io_unit, times, max_times, output_times, &
                              all_times, debug1, debug2 )
  implicit none
  integer, intent(in) :: max_times, io_unit
  logical, intent(in ) :: debug1, debug2
  character (len=80), intent(out) :: times(max_times)
  integer, intent(out) :: output_times
  logical, intent(out) :: all_times
  character (len=80) :: tmp_time
  integer :: itimes, length
  read(io_unit, *) output_times
  if(output_times > 0) then
    all_times = .false.
    itimes = 0
    do
      itimes = itimes + 1
      read(io_unit,fmt='(a80)') tmp_time
      length = max(1,index(tmp_time,' ')-1)
      if ( tmp_time(1:16) == 'end_of_time_list') exit
      if ( itimes .le. output_times ) then
        times(itimes) = tmp_time(1:length)
        if(debug1) write(6,*) ' output time ',itimes,' is ',trim(times(itimes))
      endif
    enddo
  else
    all_times = .true.
    if(debug1) write(6,*) ' All times in file will be processed'
    do
      read(io_unit,fmt='(a80)') tmp_time
      if ( tmp_time(1:16) == 'end_of_time_list') exit
    enddo
    output_times = 0
  end if
  end subroutine read_time_list
!---------------------------------------------------------------------------------
  subroutine read_variable_list( io_unit, &
                                 variables3d, desc3d, number_of_3dvar, &
                                 variables2d, desc2d, number_of_2dvar, &
                                 max_variables, debug1, debug2 )
  implicit none
  integer, intent(in) :: max_variables, io_unit
  integer, intent(out) :: number_of_3dvar, number_of_2dvar
  character (len=20), intent(out) :: variables3d(max_variables)
  character (len=20), intent(out) :: variables2d(max_variables)
  character (len=50), intent(out) :: desc3d(max_variables)
  character (len=50), intent(out) :: desc2d(max_variables)
  character (len=20) :: tmp_var
  character (len=50) :: tmp_desc
  integer :: length
  logical, intent(in) :: debug1, debug2
  logical read_complete_3d
  logical read_complete_2d
  read_complete_3d = .false.
  read_complete_2d = .false.
  number_of_3dvar = 0
  number_of_2dvar = 0
  do while( .not. read_complete_3d )
    read(io_unit, fmt='(a20,2x,a50)') tmp_var, tmp_desc
    if(tmp_var(1:1) /= ' ' .and. tmp_var(1:17) /= 'end_of_3dvar_list') then
      number_of_3dvar = number_of_3dvar + 1
      length = max(1,index(tmp_var,' ')-1)
      variables3d(number_of_3dvar) = tmp_var(1:length)
      desc3d(number_of_3dvar) = tmp_desc
      if(debug2) write(6,*) ' 3D variable ', number_of_3dvar,' ', &
                             tmp_var(1:15), ': ', &
                             trim(desc3d(number_of_3dvar))
    else if(tmp_var(1:17) == 'end_of_3dvar_list') then
      read_complete_3d = .true.
    end if
  enddo
  do while( .not. read_complete_2d )
    read(io_unit, fmt='(a20,2x,a50)') tmp_var, tmp_desc
    if(tmp_var(1:1) /= ' ' .and. tmp_var(1:17) /= 'end_of_2dvar_list') then
      number_of_2dvar = number_of_2dvar + 1
      length = max(1,index(tmp_var,' ')-1)
      variables2d(number_of_2dvar) = tmp_var(1:length)
      desc2d(number_of_2dvar) = tmp_desc
      if(debug2) write(6,*) ' 2D variable ', number_of_2dvar,' ', &
                             tmp_var(1:15), ': ', &
                             trim(desc2d(number_of_2dvar))
    else if(tmp_var(1:17) == 'end_of_2dvar_list') then
      read_complete_2d = .true.
    end if
  enddo
  end subroutine read_variable_list
!---------------------------------------------------------------------------------
  subroutine read_file_list( io_unit, files, max_files, &
                              number_of_files, debug1, debug2 )
  implicit none
  integer, intent(in) :: max_files, io_unit
  logical, intent(in) :: debug1, debug2
  integer, intent(out) :: number_of_files
  character (len=80), intent(out) :: files(max_files)
  character (len=80) :: file_in
  integer :: length
  number_of_files = 0
  do
    read(io_unit, fmt='(a80)') file_in
    if(file_in(1:16) == 'end_of_file_list') exit
    if(file_in(1:1) /= ' ') then
      number_of_files = number_of_files + 1
      length = max(1,index(file_in,' ')-1)
      files(number_of_files) = file_in(1:length)
      if(debug1) write(6,*) ' file ', number_of_files,' ', &
                                      file_in(1:length)
    end if
  enddo
  end subroutine read_file_list
!---------------------------------------------------------------------------------
  subroutine create_time_file_list( data_files, &
                                    file_for_time, &
                                    file_time_index, &
                                    times, &
                                    max_times, &
                                    all_times, &
                                    output_times, &
                                    number_of_files, &
                                    debug )
  implicit none
  integer, intent(in) :: max_times, number_of_files
  integer, intent(out) :: output_times
  character (len=80) :: data_files(max_times),times(max_times), &
                                                file_for_time(max_times)
  character (len=80) :: times_in_file(max_times)
  integer, intent(out), dimension(max_times) :: file_time_index
  logical, intent(in) :: debug, all_times
  character (len=80) :: input_time
  character (len=80) :: time1, time2, file_check
  character (len=19) :: wrf_time
  integer :: file_index, i, j, n_times, length,length1
  logical :: already_on_list
! put together the list of "times" and "file_for_time".
! for each file, find the times in that file and either
! (1) see if it corresponds to a required time - then set the
! file for that time, or
! (2) if all times are desired, add if to the list if it does
! not duplicate a time already in the list
  if( .not. all_times ) then
    do i=1,output_times
      file_for_time(i) = 'no_time_found'
    enddo
  end if
  loop_over_files : do file_index = 1, number_of_files
    if(debug) write(6,*) ' getting times for file ',data_files(file_index)
    call get_times_cdf( data_files(file_index), times_in_file, &
                        n_times, max_times, debug )
    if( n_times <= 0 ) then
      write(6,*) ' no output times found in ',data_files(file_index)
      write(6,*) ' error stop '
      stop
    end if
    if(debug) then
      write(6,*) ' times from netcdf file '
      do i=1,n_times
        wrf_time = times_in_file(i)
        write(6,*) i,wrf_time
      enddo
    endif
    if(all_times) then
      if(debug) write(6,*) ' sorting all times '
      do i=1, n_times
! length = max(1,index(times_in_file(i),' ')-1)
        length = 19
        already_on_list = .false.
        time1 = times_in_file(i)
        do j=1, output_times
          time2 = times(j)
          if( time1(1:length) == time2(1:length)) already_on_list = .true.
        enddo
        if(.not.already_on_list) then
          output_times = output_times+1
          times(output_times) = time1
          file_for_time(output_times) = data_files(file_index)
          file_time_index(output_times) = i
        endif
      enddo
    else
      do i=1, n_times
! length = max(1,index(times_in_file(i),' ')-1)
        length = 19
        do j=1, output_times
          time1 = times_in_file(i)
          time2 = times(j)
          if( time1(1:length) == time2(1:length)) then
             file_for_time(j) = data_files(file_index)
             file_time_index(j) = i
          end if
        enddo
      enddo
    end if
  enddo loop_over_files
! check here to see if we have data for all times if
! times were specified as input
  if( .not. all_times) then
    do i=1,output_times
      file_check = file_for_time(i)
      if( file_check(1:13) == 'no_time_found') then
        write(6,*) ' no data found for time ',times(i)
        write(6,*) ' error stop '
        stop
      end if
    enddo
  end if
  if(debug) then
    write(6,*) ' time and file list '
    do i=1,output_times
      time1 = times(i)
      file_check = file_for_time(i)
      length = max(1,index(file_check,' ')-1)
      length1 = 19
      write(6,*) i,time1(1:length1),' ',file_check(1:length),file_time_index(i)
    enddo
  end if
  end subroutine create_time_file_list
!-------------------------------------------------------------------
  subroutine time_calc(time,timestamp,datestamp,debug,tdef,it,case_type,hours,hour1,minutes,mins1,day)
  implicit none
  character (len=80), intent(in) :: time
  character (len=35) :: tdef
  character (len=5) :: case_type
  integer, intent(out) :: timestamp, datestamp
  logical, intent(in) :: debug
  integer :: hours, minutes, seconds, year, month, day,it,hour1,hourint
  integer :: mins1,minsint
  read(time(18:19),'(I2)') seconds
  read(time(15:16),'(I2)') minutes
  read(time(12:13),'(I2)') hours
  read(time(1:4),'(I4)') year
  read(time(6:7),'(I2)') month
  read(time(9:10),'(I2)') day
  if(debug) write(6,*) ' day, month, year, hours, minutes, seconds '
  if(debug) write(6,*) day, month, year, hours, minutes, seconds
  if ( it == 1) then
   if (case_type(1:4) == 'real')then
    write (tdef(19:20),'(i2)') hours
    if ( day < 10 ) then
      write (tdef(23:23),'(i1)') day
    else
      write (tdef(22:23),'(i2)') day
    endif
    write (tdef(27:30),'(i4)') year
    if (month == 1) write (tdef(24:26),'(a3)') 'jan'
    if (month == 2) write (tdef(24:26),'(a3)') 'feb'
    if (month == 3) write (tdef(24:26),'(a3)') 'mar'
    if (month == 4) write (tdef(24:26),'(a3)') 'apr'
    if (month == 5) write (tdef(24:26),'(a3)') 'may'
    if (month == 6) write (tdef(24:26),'(a3)') 'jun'
    if (month == 7) write (tdef(24:26),'(a3)') 'jul'
    if (month == 8) write (tdef(24:26),'(a3)') 'aug'
    if (month == 9) write (tdef(24:26),'(a3)') 'sep'
    if (month ==10) write (tdef(24:26),'(a3)') 'oct'
    if (month ==11) write (tdef(24:26),'(a3)') 'nov'
    if (month ==12) write (tdef(24:26),'(a3)') 'dec'
   endif
    hour1=hours
    mins1=minutes
  elseif ( it == 2) then
    hourint = abs(hours-hour1)
    minsint = abs(minutes-mins1)
    if (hourint == 0 ) then
      if (minsint == 0 ) minsint = 1
      if(debug) write(6,*) "interval is",minsint
      write (tdef(34:35),'(a2)') "mn"
      write (tdef(32:33),'(i2)') minsint
      write(6,*) "TDEF is",tdef,minsint,minutes,mins1
    else
      if(debug) write(6,*) "Interval is",hourint
      write (tdef(32:33),'(i2)') hourint
      if(debug) write(6,*) "TDEF is",tdef
      write(6,*) "TDEF is",tdef,hourint,hours,hour1
    endif
  endif
  timestamp = seconds+100*minutes+10000*hours
  if((year > 1800) .and. (year < 2000)) year = year-1900
  if((year >= 2000)) year = year-2000
  if(month >= 2) day = day+31 ! add january
  if(month >= 3) day = day+28 ! add february
  if(month >= 4) day = day+31 ! add march
  if(month >= 5) day = day+30 ! add april
  if(month >= 6) day = day+31 ! add may
  if(month >= 7) day = day+30 ! add june
  if(month >= 8) day = day+31 ! add july
  if(month >= 9) day = day+31 ! add august
  if(month >= 10) day = day+30 ! add september
  if(month >= 11) day = day+31 ! add october
  if(month >= 12) day = day+30 ! add november
  if((month > 2) .and. (mod(year,4) == 0)) day = day+1 ! get leap year day
  datestamp = (year)*1000 + day
! datestamp = (year+2100)*1000 + day
  if(debug) then
    write(6,*) ' time, timestamp, datestamp ',time(1:19),timestamp,datestamp
  endif
  end subroutine time_calc
!-------------------------------------------------------------------------
  subroutine g_output_3d (file, file_time_index, it, var, length_var, &
                          nx, ny, nz, dx,dy, data_out, debug, missing)
  implicit none
  character (len=80) :: file
  integer :: it, file_time_index
  character (len=30) :: var,var1
  integer :: length_var
  integer :: nx, ny, nz
  real, dimension(nx,ny,nz) :: data_out
  logical :: debug
  real, allocatable, dimension(:,:,:) :: data_tmp, data_tmp2
  real, allocatable, dimension(:,:,:) :: u, v, u1, v1
  real, allocatable, dimension(:,:) :: xlat, xlon, cor
  real, allocatable, dimension(:,:,:) :: z,ph, phb
  real, allocatable, dimension(:,:,:) :: p,pb,vort,theta,pres
  real, allocatable, dimension(:,:,:) :: t,qv,qi,qs,qg,qr,qc
  integer :: map_proj
  real :: cen_lon, truelat1, truelat2
  integer :: id_file, id_var, status, istart(2),iend(2)
  real, allocatable :: muy0(:,:), sigma(:), tam(:)
  REAL , PARAMETER :: g = 9.81 ! acceleration due to gravity (m {s}^-2)
  REAL , PARAMETER :: r_d = 287.
  REAL , PARAMETER :: r_v = 461.6
  REAL , PARAMETER :: cp = 7.*r_d/2.
  REAL , PARAMETER :: cv = cp-r_d
  REAL , PARAMETER :: cliq = 4190.
  REAL , PARAMETER :: cice = 2106.
  REAL , PARAMETER :: psat = 610.78
  REAL , PARAMETER :: rcv = r_d/cv
  REAL , PARAMETER :: rcp = r_d/cp
  REAL , PARAMETER :: c2 = cp * rcv
  REAL , PARAMETER :: T0 = 273.16
  REAL , PARAMETER :: p1000mb = 100000.
  REAL , PARAMETER :: cpovcv = cp/(cp-r_d)
  REAL , PARAMETER :: cvovcp = 1./cpovcv
  REAL :: pp,dx,dy,t1,t2,t3,missing
  INTEGER :: i,j,k,wrf_real
  include 'netcdf.inc'
  if(debug) then
    write(6,*) ' calculations for variable ',var
  end if
  if(var == 'U' ) then
          allocate ( data_tmp(nx+1,ny,nz) )
          call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(1:nx,:,:)+data_tmp(2:nx+1,:,:))
          deallocate ( data_tmp )
  else if(var == 'V' ) then
          allocate ( data_tmp(nx,ny+1,nz) )
          call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
          deallocate ( data_tmp )
!
! Chanh here
!
  else if(var == 'RU_TENDF' ) then
          allocate ( data_tmp(nx+1,ny,nz) )
          call get_var_3d_real_cdf( file,"RU_TENDF", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(1:nx,:,:)+data_tmp(2:nx+1,:,:))
          deallocate ( data_tmp )
  else if(var == 'RV_TENDF' ) then
          allocate ( data_tmp(nx,ny+1,nz) )
          call get_var_3d_real_cdf( file,"RV_TENDF", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
          deallocate ( data_tmp )
  else if(var == 'RV_TEND' ) then
          allocate ( data_tmp(nx,ny+1,nz) )
          call get_var_3d_real_cdf( file,"RV_TEND", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
          deallocate ( data_tmp )
  else if(var == 'RW_TENDF' ) then
          allocate ( data_tmp(nx,ny,nz+1) )
          call get_var_3d_real_cdf( file,"RW_TENDF", data_tmp, nx, ny, nz+1, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(:,:,1:nz)+data_tmp(:,:,2:nz+1))
          deallocate ( data_tmp )
  else if(var == 'T_TEND' ) then
          allocate ( data_tmp(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"T_TEND", data_tmp, nx, ny, nz, &
                                file_time_index, debug )
          data_out=data_tmp
          deallocate ( data_tmp )
  else if(var == 'H_DIABATIC' ) then
          allocate ( data_tmp(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"H_DIABATIC", data_tmp, nx, ny, nz, &
                                file_time_index, debug )
          data_out=data_tmp
          deallocate ( data_tmp )
  else if(var == 'GRADPU' ) then
          allocate ( data_tmp(nx+1,ny,nz) )
          call get_var_3d_real_cdf( file,"GRADPU", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(1:nx,:,:)+data_tmp(2:nx+1,:,:))
          deallocate ( data_tmp )
  else if(var == 'GRADPV' ) then
          allocate ( data_tmp(nx,ny+1,nz) )
          call get_var_3d_real_cdf( file,"GRADPV", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
          deallocate ( data_tmp )
  else if(var == 'PHB' ) then
          allocate ( data_tmp(nx,ny,nz+1) )
          call get_var_3d_real_cdf( file,"PHB", data_tmp, nx, ny, nz+1, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(:,:,1:nz)+data_tmp(:,:,2:nz+1))
          deallocate ( data_tmp )
  else if(var == 'VORT' ) then
! print*,' ---> Calculate VORT by Chanh'
          allocate ( data_tmp(nx+1,ny,nz) )
          allocate ( u(nx,ny,nz))
          call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
   do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             u(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i+1,j,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          allocate ( data_tmp(nx,ny+1,nz) )
          allocate ( v(nx,ny,nz))
          call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
   do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             v(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i,j+1,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          data_out = missing
   do k = 1,nz
           do j = 2,ny-1
            do i = 2,nx-1
             data_out(i,j,k) = (v(i+1,j,k)-v(i-1,j,k))/(2*dx) &
                             - (u(i,j+1,k)-u(i,j-1,k))/(2*dy)
            enddo
           enddo
          enddo
  else if(var == 'PV' ) then
          allocate ( cor(nx,ny))
          call get_var_2d_real_cdf( file,"F",cor,nx,ny,1,debug )
          allocate ( data_tmp(nx+1,ny,nz) )
          allocate ( u(nx,ny,nz))
          call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             u(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i+1,j,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          allocate ( data_tmp(nx,ny+1,nz) )
          allocate ( v(nx,ny,nz))
          call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             v(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i,j+1,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          allocate (vort(nx,ny,nz))
          vort = missing
          do k = 1,nz
           do j = 2,ny-1
            do i = 2,nx-1
             vort(i,j,k) = (v(i+1,j,k)-v(i-1,j,k))/(2*dx) &
                             - (u(i,j+1,k)-u(i,j-1,k))/(2*dy)
            enddo
           enddo
          enddo
          wrf_real = 0
          allocate(theta(nx,ny,nz))
          call get_var_3d_real_cdf( file,"T", theta, nx, ny, nz, &
                                file_time_index, debug )
          if (wrf_real.eq.0) theta = theta + 300
          if (wrf_real.eq.1) then
           print*,'WARNING: compute PV for wrf_real input, pressure is NOT accurate'
           allocate ( pres(nx,ny,nz),muy0(nx,ny))
           call get_var_2d_real_cdf( file, 'MU0', muy0 ,nx, ny, file_time_index, debug )
           allocate ( sigma(nz),tam(nz+1) )
           status = NF_OPEN(file,NF_NOWRITE,id_file)
           status = NF_INQ_VARID(id_file,'ZNW',id_var)
           istart(1)= 1
           iend(1) = nz+1
           istart(2)= file_time_index
           iend(2) = 1
           status = NF_GET_VARA_REAL(id_file,id_var,istart,iend,tam)
           do k = 1,nz
            sigma(k)= (tam(nz+1-k)+tam(nz-k))/2.
           enddo
           status = NF_CLOSE(id_file)
           do k = 1,nz
            do j = 1,ny
             do i = 1,nx
              pres(i,j,k) = sigma(k)*muy0(i,j) + 3000. ! ptop is 30mb. Have to change if use different PTOP in wrfsi.nl
             enddo
            enddo
           enddo
          else
           allocate ( p(nx,ny,nz) )
           allocate ( pb(nx,ny,nz) )
           allocate ( pres(nx,ny,nz))
           call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, &
                                 file_time_index, debug )
           call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, &
                                 file_time_index, debug )
           pres = p+pb
           deallocate ( p )
           deallocate ( pb )
          endif
          allocate (data_tmp2(nx,ny,nz))
          data_tmp2 = theta*(pres/p1000mb)**rcp ! temperature in K
          allocate ( data_tmp(nx,ny,nz) )
          data_tmp = missing
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
              data_tmp(i,j,k)=pres(i,j,k)/286.5/data_tmp2(i,j,k) ! density in kg/m^3
            enddo
           enddo
          enddo
          data_out = missing
          DO 588 k=2,nz-1
          DO 588 j=2,ny-1
          DO 588 i=2,nx-1
            if (data_tmp(i,j,k).ne.missing) then
             data_out(i,j,k)=1.e+6*(cor(i,j) + vort(i,j,k)) &
                                 *(theta(i,j,k+1)-theta(i,j,k-1))/(pres(i,j,k+1)-pres(i,j,k-1))
! /data_tmp(i,j,k) ! Do we need this?
            endif
            if (abs(data_out(i,j,k)).gt.1) then
             data_out(i,j,k) = missing
            endif
588 CONTINUE
          data_out(1,:,:)=data_out(2,:,:)
          data_out(nz,:,:)=data_out(nz-1,:,:)
  else if(var == 'PV_INS' ) then
          allocate ( cor(nx,ny) )
          call get_var_2d_real_cdf( file,"F",cor,nx,ny,1,debug )
          allocate ( data_tmp(nx+1,ny,nz) )
          allocate ( u(nx,ny,nz))
          call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             u(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i+1,j,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          allocate ( data_tmp(nx,ny+1,nz) )
          allocate ( v(nx,ny,nz))
          call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             v(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i,j+1,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          allocate (vort(nx,ny,nz))
          vort = missing
          do k = 1,nz
           do j = 2,ny-1
            do i = 2,nx-1
             vort(i,j,k) = (v(i+1,j,k)-v(i-1,j,k))/(2*dx) &
                             - (u(i,j+1,k)-u(i,j-1,k))/(2*dy)
            enddo
           enddo
          enddo
          wrf_real = 0
          allocate(theta(nx,ny,nz))
          call get_var_3d_real_cdf( file,"T", theta, nx, ny, nz, &
                                file_time_index, debug )
          if (wrf_real.eq.0) theta = theta + 300
          if (wrf_real.eq.1) then
           allocate ( pres(nx,ny,nz),muy0(nx,ny))
           call get_var_2d_real_cdf( file, 'MU0', muy0 ,nx, ny, file_time_index, debug )
           allocate ( sigma(nz),tam(nz+1) )
           status = NF_OPEN(file,NF_NOWRITE,id_file)
           status = NF_INQ_VARID(id_file,'ZNW',id_var)
           istart(1)= 1
           iend(1) = nz+1
           istart(2)= file_time_index
           iend(2) = 1
           status = NF_GET_VARA_REAL(id_file,id_var,istart,iend,tam)
           do k = 1,nz
            sigma(k)= (tam(nz+1-k)+tam(nz-k))/2.
           enddo
           status = NF_CLOSE(id_file)
           do k = 1,nz
            do j = 1,ny
             do i = 1,nx
              pres(i,j,k) = sigma(k)*muy0(i,j) + 3000. ! ptop is 30mb. Have to change if use different PTOP in wrfsi.nl
             enddo
            enddo
           enddo
          else
           allocate ( p(nx,ny,nz) )
           allocate ( pb(nx,ny,nz) )
           allocate ( pres(nx,ny,nz))
           call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, &
                                 file_time_index, debug )
           call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, &
                                 file_time_index, debug )
           pres = p+pb
           deallocate ( p )
           deallocate ( pb )
          endif
          allocate (data_tmp2(nx,ny,nz))
          data_tmp2 = theta*(pres/p1000mb)**rcp ! temperature in K
          allocate ( data_tmp(nx,ny,nz) )
          data_tmp = missing
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
              data_tmp(i,j,k)=pres(i,j,k)/286.5/data_tmp2(i,j,k) ! density in kg/m^3
            enddo
           enddo
          enddo
          data_tmp2 = missing ! re-use data_tmp2. Dangerous
          DO 589 k=2,nz-1
          DO 589 j=2,ny-1
          DO 589 i=2,nx-1
            if (data_tmp(i,j,k).ne.missing) then
             data_tmp2(i,j,k)=1.e+6*(cor(i,j) + vort(i,j,k)) &
                                 *(theta(i,j,k+1)-theta(i,j,k-1))/(pres(i,j,k+1)-pres(i,j,k-1))
! /data_tmp(i,j,k) ! Do we need to divide by density in p-coordinate?
            endif
            if (abs(data_tmp2(i,j,k)).gt.1) then
             data_tmp2(i,j,k) = missing
            endif
589 CONTINUE
          data_tmp2(1,:,:)=data_tmp2(2,:,:)
          data_tmp2(nz,:,:)=data_tmp2(nz-1,:,:)
          data_out = missing
          do k = 1,nz
           do j = 3,ny-2
            do i = 3,nx-2
             data_out(i,j,k) = (data_tmp2(i,j+1,k)-data_tmp2(i,j-1,k))/(2*dy)
            enddo
           enddo
          enddo
  else if(var == 'DIREC' ) then
          call get_gl_att_int_cdf ( file, 'MAP_PROJ', map_proj, debug )
          allocate (u1(nx,ny,nz))
          allocate (v1(nx,ny,nz))
          IF ( map_proj == 1 .OR. map_proj == 2 ) THEN
              allocate ( u(nx,ny,nz) )
              allocate ( v(nx,ny,nz) )
              allocate ( xlat(nx,ny) )
              allocate ( xlon(nx,ny) )
              allocate ( data_tmp(nx+1,ny,nz) )
              call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                    file_time_index, debug )
              u = 0.5*(data_tmp(1:nx,:,:)+data_tmp(2:nx+1,:,:))
              deallocate ( data_tmp )
              allocate ( data_tmp(nx,ny+1,nz) )
              call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                    file_time_index, debug )
              v = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
              deallocate ( data_tmp )
              call get_gl_att_real_cdf( file, 'STAND_LON', cen_lon, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT1', truelat1, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT2', truelat2, debug )
              call get_var_2d_real_cdf( file, 'XLAT', xlat,nx,ny, 1,debug )
              call get_var_2d_real_cdf( file, 'XLONG',xlon,nx,ny, 1,debug )
              var1(1:4)="UMET"
              call rotate_wind (u,v,nx,ny,nz,var1, &
                                map_proj,cen_lon,xlat,xlon, &
                                truelat1,truelat2,data_out)
              do k = 1,nz
               do j = 1,ny
                do i = 1,nx
                 u1(i,j,k) = data_out(i,j,k)
                enddo
               enddo
              enddo
              var1(1:4)="VMET"
              call rotate_wind (u,v,nx,ny,nz,var1, &
                                map_proj,cen_lon,xlat,xlon, &
                                truelat1,truelat2,data_out)
              do k = 1,nz
               do j = 1,ny
                do i = 1,nx
                 v1(i,j,k) = data_out(i,j,k)
                enddo
               enddo
              enddo
              deallocate ( xlat )
              deallocate ( xlon )
              deallocate ( u )
              deallocate ( v )
          ELSE
              allocate ( data_tmp(nx+1,ny,nz) )
              call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
              do k = 1,nz
               do j = 1,ny
                do i = 1,nx
                 u1(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i+1,j,k))
                enddo
               enddo
              enddo
              deallocate ( data_tmp )
              allocate ( data_tmp(nx,ny+1,nz) )
              call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
              do k = 1,nz
               do j = 1,ny
                do i = 1,nx
                 v1(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i,j+1,k))
                enddo
               enddo
              enddo
              deallocate ( data_tmp )
          ENDIF
          data_out = missing
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             data_out(i,j,k) = atan(abs(v1(i,j,k))/abs(u1(i,j,k)))*180./4./atan(1.)
             if (u1(i,j,k).ge.0.and.v1(i,j,k).ge.0) data_out(i,j,k) = 270 - data_out(i,j,k)
             if (u1(i,j,k).ge.0.and.v1(i,j,k).le.0) data_out(i,j,k) = 270 + data_out(i,j,k)
             if (u1(i,j,k).le.0.and.v1(i,j,k).ge.0) data_out(i,j,k) = 90. + data_out(i,j,k)
             if (u1(i,j,k).le.0.and.v1(i,j,k).le.0) data_out(i,j,k) = 90. - data_out(i,j,k)
            enddo
           enddo
          enddo
  else if(var == 'BINS' ) then
! print*,' ---> Calculate BINS by Chanh'
          allocate ( data_tmp(nx+1,ny,nz) )
          allocate ( u(nx,ny,nz), cor(nx,ny))
          call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
          call get_var_2d_real_cdf( file,"F",cor,nx,ny,1,debug )
   do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             u(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i+1,j,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          data_out = missing
   do k = 1,nz
           do j = 2,ny-1
            do i = 2,nx-1
             data_out(i,j,k) = (cor(i,j+1)-cor(i,j-1))/(2*dy) &
                             - (u(i,j+1,k) - 2*u(i,j,k) + u(i,j-1,k))/(dy**2)
            enddo
           enddo
          enddo
  else if(var == 'TEMGRAD' ) then
          print*,' ---> Calculate TEMGRAD by Chanh'
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          allocate ( data_tmp(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"T", data_tmp, nx, ny, nz, &
                                file_time_index, debug )
          data_out = missing
          do k = 1,nz
           do i = 2,nx-1
            do j = 2,ny-1
             data_out(i,j,k) = (data_tmp(i,j+1,k)-data_tmp(i,j-1,k))/(2*dy)
            enddo
           enddo
          enddo
          deallocate ( p )
          deallocate ( pb )
          deallocate ( data_tmp )
  else if(var == 'DIV' ) then
! print*,' ---> Calculate DIV by Chanh'
          allocate ( data_tmp(nx+1,ny,nz) )
          allocate ( u(nx,ny,nz))
          call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                file_time_index, debug )
   do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             u(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i+1,j,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          allocate ( data_tmp(nx,ny+1,nz) )
          allocate ( v(nx,ny,nz))
          call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                file_time_index, debug )
   do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             v(i,j,k) = 0.5*(data_tmp(i,j,k)+data_tmp(i,j+1,k))
            enddo
           enddo
          enddo
          deallocate ( data_tmp )
          data_out = missing
   do k = 1,nz
           do j = 2,ny-1
            do i = 2,nx-1
             data_out(i,j,k) = (u(i+1,j,k)-u(i-1,j,k))/(2*dx) &
                             + (v(i,j+1,k)-v(i,j-1,k))/(2*dy)
            enddo
           enddo
          enddo
  else if(var == 'RH_INPUT' ) then
! print*,' ---> Calculate RH_INPUT by Chanh'
          allocate ( data_tmp(nx,ny,nz) )
          allocate ( data_tmp2(nx,ny,nz) )
          allocate ( qv(nx,ny,nz), T(nx,ny,nz), p(nx,ny,nz))
          call get_var_3d_real_cdf( file, "QVAPOR", qv, nx, ny, nz, file_time_index, debug )
          call get_var_3d_real_cdf( file, "T" , T, nx, ny, nz, file_time_index, debug )
          allocate ( muy0(nx,ny))
          call get_var_2d_real_cdf( file, 'MU0', muy0 ,nx, ny, file_time_index, debug )
          allocate ( sigma(nz),tam(nz+1) )
          status = NF_OPEN(file,NF_NOWRITE,id_file)
          status = NF_INQ_VARID(id_file,'ZNW',id_var)
          istart(1)= 1
          iend(1) = nz+1
          istart(2)= file_time_index
          iend(2) = 1
          status = NF_GET_VARA_REAL(id_file,id_var,istart,iend,tam)
          do k = 1,nz
           sigma(k)= (tam(nz+1-k)+tam(nz-k))/2.
          enddo
          status = NF_CLOSE(id_file)
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             p(i,j,k) = sigma(k)*muy0(i,j) + 3000. ! ptop is 30mb. Have to change if use different PTOP in wrfsi.nl
            enddo
           enddo
          enddo
          t = t*(p/p1000mb)**rcp
          data_tmp2 = 10.*0.6112*exp(17.67*(t-T0)/(t-29.65))
          data_tmp = 0.622*data_tmp2/(0.01 * p - (1.-0.622)*data_tmp2)
          data_out = 100.*AMAX1(AMIN1(qv/data_tmp,1.0),0.0)
          deallocate(qv,T,p,sigma,muy0,data_tmp,data_tmp2)
  else if(var == 'THETA_E' ) then
          allocate ( data_tmp(nx,ny,nz) )
          allocate ( data_tmp2(nx,ny,nz) )
          allocate ( qv(nx,ny,nz), T(nx,ny,nz), p(nx,ny,nz))
          call get_var_3d_real_cdf( file, "QVAPOR", qv, nx, ny, nz, file_time_index, debug )
          call get_var_3d_real_cdf( file, "T" , T, nx, ny, nz, file_time_index, debug )
          allocate ( muy0(nx,ny))
          call get_var_2d_real_cdf( file, 'MU0', muy0 ,nx, ny, file_time_index, debug )
          allocate ( sigma(nz),tam(nz+1) )
          status = NF_OPEN(file,NF_NOWRITE,id_file)
          status = NF_INQ_VARID(id_file,'ZNW',id_var)
          istart(1)= 1
          iend(1) = nz+1
          istart(2)= file_time_index
          iend(2) = 1
          status = NF_GET_VARA_REAL(id_file,id_var,istart,iend,tam)
          do k = 1,nz
           sigma(k)= (tam(nz+1-k)+tam(nz-k))/2.
          enddo
! write(*,'(7F7.3)')(tam(i),i=1,nz+1)
! write(*,'(7F7.3)')(sigma(i),i=1,nz)
          status = NF_CLOSE(id_file)
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             p(i,j,k) = sigma(k)*muy0(i,j) + 3000. ! ptop is 30mb. Have to change if use different PTOP in wrfsi.nl
            enddo
           enddo
          enddo
          t = t*(p/p1000mb)**rcp
          call thetae(data_out,t,p,qv,nx,ny,nz)
          deallocate(qv,T,p,sigma,muy0,data_tmp,data_tmp2)
  else if(var == 'THETAE1' ) then
           allocate ( p(nx,ny,nz) )
           allocate ( pb(nx,ny,nz) )
           allocate ( pres(nx,ny,nz) )
           allocate ( qv(nx,ny,nz) )
           allocate ( T(nx,ny,nz) )
           call get_var_3d_real_cdf( file,"QVAPOR", qv, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"T", T, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, file_time_index, debug )
           pres = p+pb
           t = (t+300)*(pres/p1000mb)**rcp
           call thetae(data_out,t,pres,qv,nx,ny,nz)
           deallocate(p,pb,qv,T,pres)
  else if(var == 'RADAR' ) then
           allocate ( p(nx,ny,nz) )
           allocate ( pb(nx,ny,nz) )
           allocate ( pres(nx,ny,nz) )
           allocate ( qr(nx,ny,nz) )
           allocate ( qi(nx,ny,nz) )
           allocate ( qs(nx,ny,nz) )
           allocate ( qg(nx,ny,nz) )
           allocate ( T(nx,ny,nz) )
           call get_var_3d_real_cdf( file,"QRAIN", qr, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"QICE", qi, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"QSNOW", qs, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"QGRAUP", qg, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"T", T, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, file_time_index, debug )
           call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, file_time_index, debug )
           pres = p+pb
           t = (t+300)*(pres/p1000mb)**rcp
           call radfld(data_out,t,qr,qi,qs,qg,pres,nx,ny,nz)
           deallocate(p,pb,qr,qi,qs,qg,T,pres)
  else if(var == 'PRES_INPUT' ) then
          allocate ( muy0(nx,ny))
          call get_var_2d_real_cdf( file, 'MU0', muy0 ,nx, ny, file_time_index, debug )
          allocate ( sigma(nz),tam(nz+1) )
          status = NF_OPEN(file,NF_NOWRITE,id_file)
          status = NF_INQ_VARID(id_file,'ZNW',id_var)
          istart(1)= 1
          iend(1) = nz+1
          istart(2)= file_time_index
          iend(2) = 1
          status = NF_GET_VARA_REAL(id_file,id_var,istart,iend,tam)
          do k = 1,nz
           sigma(k)= (tam(nz+1-k)+tam(nz-k))/2.
          enddo
          status = NF_CLOSE(id_file)
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             data_out(i,j,k) = sigma(k)*muy0(i,j) + 3000. ! ptop is 30mb. Have to change if use different PTOP in wrfsi.nl
            enddo
           enddo
          enddo
  else if(var == 'UMET' ) then
          call get_gl_att_int_cdf ( file, 'MAP_PROJ', map_proj, debug )
          IF ( map_proj == 1 .OR. map_proj == 2 ) THEN
              allocate ( u(nx,ny,nz) )
              allocate ( v(nx,ny,nz) )
              allocate ( xlat(nx,ny) )
              allocate ( xlon(nx,ny) )
              allocate ( data_tmp(nx+1,ny,nz) )
              call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                    file_time_index, debug )
              u = 0.5*(data_tmp(1:nx,:,:)+data_tmp(2:nx+1,:,:))
              deallocate ( data_tmp )
              allocate ( data_tmp(nx,ny+1,nz) )
              call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                    file_time_index, debug )
              v = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
              deallocate ( data_tmp )
              call get_gl_att_real_cdf( file, 'STAND_LON', cen_lon, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT1', truelat1, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT2', truelat2, debug )
              call get_var_2d_real_cdf( file, 'XLAT', xlat,nx,ny, 1,debug )
              call get_var_2d_real_cdf( file, 'XLONG',xlon,nx,ny, 1,debug )
              call rotate_wind (u,v,nx,ny,nz,var, &
                                map_proj,cen_lon,xlat,xlon, &
                                truelat1,truelat2,data_out)
              deallocate ( xlat )
              deallocate ( xlon )
              deallocate ( u )
              deallocate ( v )
          ELSE
              allocate ( data_tmp(nx+1,ny,nz) )
              call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                    file_time_index, debug )
              data_out = 0.5*(data_tmp(1:nx,:,:)+data_tmp(2:nx+1,:,:))
              deallocate ( data_tmp )
          ENDIF
  else if(var == 'VMET' ) then
          call get_gl_att_int_cdf ( file, 'MAP_PROJ', map_proj, debug )
          IF ( map_proj == 1 .OR. map_proj == 2 ) THEN
              allocate ( u(nx,ny,nz) )
              allocate ( v(nx,ny,nz) )
              allocate ( xlat(nx,ny) )
              allocate ( xlon(nx,ny) )
              allocate ( data_tmp(nx+1,ny,nz) )
              call get_var_3d_real_cdf( file,"U", data_tmp, nx+1, ny, nz, &
                                    file_time_index, debug )
              u = 0.5*(data_tmp(1:nx,:,:)+data_tmp(2:nx+1,:,:))
              deallocate ( data_tmp )
              allocate ( data_tmp(nx,ny+1,nz) )
              call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                    file_time_index, debug )
              v = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
              deallocate ( data_tmp )
              call get_gl_att_real_cdf( file, 'STAND_LON', cen_lon, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT1', truelat1, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT2', truelat2, debug )
              call get_var_2d_real_cdf( file, 'XLAT', xlat,nx,ny, 1,debug )
              call get_var_2d_real_cdf( file, 'XLONG',xlon,nx,ny, 1,debug )
              call rotate_wind (u,v,nx,ny,nz,var, &
                                map_proj,cen_lon,xlat,xlon, &
                                truelat1,truelat2,data_out)
              deallocate ( xlat )
              deallocate ( xlon )
              deallocate ( u )
              deallocate ( v )
          ELSE
              allocate ( data_tmp(nx,ny+1,nz) )
              call get_var_3d_real_cdf( file,"V", data_tmp, nx, ny+1, nz, &
                                    file_time_index, debug )
              data_out = 0.5*(data_tmp(:,1:ny,:)+data_tmp(:,2:ny+1,:))
              deallocate ( data_tmp )
          ENDIF
  else if(var == 'W' ) then
          allocate ( data_tmp(nx,ny,nz+1) )
          call get_var_3d_real_cdf( file,"W", data_tmp, nx, ny, nz+1, &
                                file_time_index, debug )
          data_out = 0.5*(data_tmp(:,:,1:nz)+data_tmp(:,:,2:nz+1))
          deallocate ( data_tmp )
  else if(var == 'P' ) then
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, &
                                file_time_index, debug )
          data_out = (p+pb)*.01
          deallocate ( p )
          deallocate ( pb )
  else if(var == 'Z' ) then
          allocate ( ph(nx,ny,nz+1) )
          allocate ( phb(nx,ny,nz+1) )
          call get_var_3d_real_cdf( file,"PH", ph, nx, ny, nz+1, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PHB", phb, nx, ny, nz+1, &
                                file_time_index, debug )
          ph = (ph+phb)/9.81
          data_out = 0.5*(ph(:,:,1:nz)+ph(:,:,2:nz+1))
          deallocate ( ph )
          deallocate ( phb )
  else if(var == 'THETA' ) then
          call get_var_3d_real_cdf( file,"T", data_out, nx, ny, nz, &
                                file_time_index, debug )
          data_out = data_out + 300.
  else if(var == 'TK' ) then
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          allocate ( data_tmp(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, &
                                file_time_index, debug )
          p = p+pb
          call get_var_3d_real_cdf( file,"T", data_tmp, nx, ny, nz, &
                                file_time_index, debug )
          data_out = (data_tmp+300.)*(p/p1000mb)**rcp
          deallocate ( p )
          deallocate ( pb )
          deallocate ( data_tmp )
  else if(var == 'TC' ) then
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          allocate ( data_tmp(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, &
                                file_time_index, debug )
          p = p+pb
          call get_var_3d_real_cdf( file,"T", data_tmp, nx, ny, nz, &
                                file_time_index, debug )
          data_out = (data_tmp+300.)*(p/p1000mb)**rcp -T0
          deallocate ( p )
          deallocate ( pb )
          deallocate ( data_tmp )
  else if(var == 'TD' ) then
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          allocate ( qv(nx,ny,nz) )
          allocate ( data_tmp(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, &
                                file_time_index, debug )
          p = p+pb
          call get_var_3d_real_cdf( file,"QVAPOR", qv, nx, ny, nz, &
                                file_time_index, debug )
          data_tmp = qv*(p/100.)/(0.622+qv)
          data_tmp = AMAX1(data_tmp,0.001)
          data_out = (243.5*log(data_tmp)-440.8)/(19.48-log(data_tmp))
          deallocate ( p )
          deallocate ( pb )
          deallocate ( qv )
          deallocate ( data_tmp )
  else if(var == 'TD_IN' ) then
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          allocate ( qv(nx,ny,nz) )
          allocate ( data_tmp(nx,ny,nz) )
          allocate ( muy0(nx,ny))
          allocate ( sigma(nz),tam(nz+1) )
          call get_var_2d_real_cdf( file, 'MU0', muy0 ,nx, ny, file_time_index, debug )
          status = NF_OPEN(file,NF_NOWRITE,id_file)
          status = NF_INQ_VARID(id_file,'ZNW',id_var)
          istart(1)= 1
          iend(1) = nz+1
          istart(2)= file_time_index
          iend(2) = 1
          status = NF_GET_VARA_REAL(id_file,id_var,istart,iend,tam)
          do k = 1,nz
           sigma(k)= (tam(nz+1-k)+tam(nz-k))/2.
          enddo
          status = NF_CLOSE(id_file)
          do k = 1,nz
           do j = 1,ny
            do i = 1,nx
             p(i,j,k) = sigma(k)*muy0(i,j) + 3000. ! ptop is 30mb. Have to change if use different PTOP in wrfsi.nl
            enddo
           enddo
          enddo
          call get_var_3d_real_cdf( file,"QVAPOR", qv, nx, ny, nz, &
                                file_time_index, debug )
          data_tmp = qv*(p/100.)/(0.622+qv)
          data_tmp = AMAX1(data_tmp,0.001)
          data_out = (243.5*log(data_tmp)-440.8)/(19.48-log(data_tmp))
          deallocate ( p )
          deallocate ( pb )
          deallocate ( qv )
          deallocate ( data_tmp )
  else if(var == 'RH' ) then
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          allocate ( qv(nx,ny,nz) )
          allocate ( t(nx,ny,nz) )
          allocate ( data_tmp(nx,ny,nz) )
          allocate ( data_tmp2(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"P", p, nx, ny, nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PB", pb, nx, ny, nz, &
                                file_time_index, debug )
          p = p+pb
          call get_var_3d_real_cdf( file,"T", t, nx, ny, nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"QVAPOR", qv, nx, ny, nz, &
                                file_time_index, debug )
          t = (t+300.)*(p/p1000mb)**rcp
          data_tmp2 = 10.*0.6112*exp(17.67*(t-T0)/(t-29.65))
          data_tmp = 0.622*data_tmp2/(0.01 * p - (1.-0.622)*data_tmp2)
          data_out = 100.*AMAX1(AMIN1(qv/data_tmp,1.0),0.0)
          deallocate ( p )
          deallocate ( pb )
          deallocate ( qv )
          deallocate ( t )
          deallocate ( data_tmp )
          deallocate ( data_tmp2 )
  else
          call get_var_3d_real_cdf( file,var(1:length_var), &
                                    data_out, nx,ny,nz, &
                                    file_time_index, debug )
  endif
  end subroutine g_output_3d
!-------------------------------------------------------------------------
  subroutine g_output_2d (file, file_time_index, it, var, length_var, &
                          nx, ny, nz, data_out, debug)
  implicit none
  character (len=80) :: file
  integer :: it, file_time_index
  character (len=30) :: var
  integer :: length_var
  integer :: nx, ny, nz
  real, dimension(nx,ny,1) :: data_out
  logical :: debug
  integer, allocatable, dimension(:,:,:) :: data_int
  real, allocatable, dimension(:,:,:) :: u10, v10
  real, allocatable, dimension(:,:) :: xlat, xlon, lama, mu0
  real, allocatable, dimension(:,:,:) :: z,ph,phb
  real, allocatable, dimension(:,:,:) :: p,pb
  real, allocatable, dimension(:,:,:) :: ts,qv
  integer :: map_proj,i,j,k
  real :: cen_lon, truelat1, truelat2
  if(debug) then
     write(6,*) ' calculations for variable ',var
  end if
       if(var == 'slvl') then
          allocate ( z(nx,ny,nz) )
          allocate ( ph(nx,ny,nz+1) )
          allocate ( phb(nx,ny,nz+1) )
          allocate ( p(nx,ny,nz) )
          allocate ( pb(nx,ny,nz) )
          allocate ( ts(nx,ny,nz) )
          allocate ( qv(nx,ny,nz) )
          call get_var_3d_real_cdf( file,"PH", ph, nx, ny,nz+1, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PHB", phb, nx, ny,nz+1, &
                                file_time_index, debug )
          ph = (ph+phb)/9.81
          z = 0.5*(ph(:,:,1:nz)+ph(:,:,2:nz+1))
          call get_var_3d_real_cdf( file,"P", p, nx, ny,nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"PB", pb, nx, ny,nz, &
                                file_time_index, debug )
          p = p+pb
          call get_var_3d_real_cdf( file,"T", ts, nx, ny,nz, &
                                file_time_index, debug )
          call get_var_3d_real_cdf( file,"QVAPOR", qv, nx, ny,nz, &
                                file_time_index, debug )
          call compute_seaprs (nx, ny, nz, z, ts, p, qv, data_out, debug)
          deallocate ( z )
          deallocate ( ph )
          deallocate ( phb )
          deallocate ( p )
          deallocate ( pb )
          deallocate ( ts )
          deallocate ( qv )
  else if(var == 'U10M' ) then
          call get_gl_att_int_cdf ( file, 'MAP_PROJ', map_proj, debug )
          IF ( map_proj == 1 .OR. map_proj == 2 ) THEN
              allocate ( u10(nx,ny,1) )
              allocate ( v10(nx,ny,1) )
              allocate ( xlat(nx, ny) )
              allocate ( xlon(nx, ny) )
              call get_var_2d_real_cdf( file,"U10", u10, nx, ny, &
                                    file_time_index, debug )
              call get_var_2d_real_cdf( file,"V10", v10, nx, ny, &
                                    file_time_index, debug )
              call get_gl_att_real_cdf( file, 'STAND_LON', cen_lon, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT1', truelat1, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT2', truelat2, debug )
              call get_var_2d_real_cdf( file, 'XLAT', xlat,nx,ny, 1,debug )
              call get_var_2d_real_cdf( file, 'XLONG',xlon,nx,ny, 1,debug )
              call rotate_wind (u10,v10,nx,ny,1,var, &
                                map_proj,cen_lon,xlat,xlon, &
                                truelat1,truelat2,data_out)
              deallocate ( xlat )
              deallocate ( xlon )
              deallocate ( u10 )
              deallocate ( v10 )
          ELSE
              call get_var_2d_real_cdf( file,"U10", data_out, nx, ny, &
                                    file_time_index, debug )
          ENDIF
  else if(var == 'V10M' ) then
          call get_gl_att_int_cdf ( file, 'MAP_PROJ', map_proj, debug )
          IF ( map_proj == 1 .OR. map_proj == 2 ) THEN
              allocate ( u10(nx,ny,1) )
              allocate ( v10(nx,ny,1) )
              allocate ( xlat(nx, ny) )
              allocate ( xlon(nx, ny) )
              call get_var_2d_real_cdf( file,"U10", u10, nx, ny, &
                                    file_time_index, debug )
              call get_var_2d_real_cdf( file,"V10", v10, nx, ny, &
                                    file_time_index, debug )
              call get_gl_att_real_cdf( file, 'STAND_LON', cen_lon, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT1', truelat1, debug )
              call get_gl_att_real_cdf( file, 'TRUELAT2', truelat2, debug )
              call get_var_2d_real_cdf( file, 'XLAT', xlat,nx,ny, 1,debug )
              call get_var_2d_real_cdf( file, 'XLONG',xlon,nx,ny, 1,debug )
              call rotate_wind (u10,v10,nx,ny,1,var, &
                                map_proj,cen_lon,xlat,xlon, &
                                truelat1,truelat2,data_out)
              deallocate ( xlat )
              deallocate ( xlon )
              deallocate ( u10 )
              deallocate ( v10 )
          ELSE
              call get_var_2d_real_cdf( file,"V10", data_out, nx, ny, &
                                    file_time_index, debug )
          ENDIF
  else if(var == 'IVGTYP' .or. var == 'ISLTYP') then
          allocate (data_int(nx,ny,1))
          call get_var_2d_int_cdf( file,var(1:length_var), &
                                    data_int, nx,ny, &
                                    file_time_index, debug )
          data_out = data_int
          deallocate (data_int)
!
! Chanh here
!
  else if(var == 'pmsl') then
          call get_var_2d_real_cdf( file,"PMSL",data_out,nx, ny, &
                                file_time_index, debug )
  else if(var == 'PSFC_REAL') then
          allocate (lama(nx,ny))
          allocate (mu0(nx,ny))
          call get_var_2d_real_cdf( file,"PMSL",data_out,nx, ny, &
                                file_time_index, debug )
          call get_var_2d_real_cdf( file,"MU0",mu0,nx, ny, &
                                file_time_index, debug )
          call get_var_2d_real_cdf( file,"LANDMASK",lama,nx, ny, &
                                file_time_index, debug )
          do i = 1,nx
           do j = 1,ny
            if (lama(i,j).eq.1) data_out(i,j,1) = mu0(i,j) + 30*100 ! ptop=30mb
           enddo
          enddo
          deallocate(lama,mu0)
  else if(var == 'SST') then
          call get_var_2d_real_cdf( file,"SST",data_out,nx, ny, &
                                file_time_index, debug )
          print*,'read SST from wrf_real_input'
  else
          call get_var_2d_real_cdf( file,var(1:length_var), &
                                    data_out, nx,ny, &
                                    file_time_index, debug )
  endif
  end subroutine g_output_2d
!------------------------------------------------------------------
  subroutine check_special_variable( var_to_get )
  implicit none
  character (len=20), intent(inout) :: var_to_get
  if(var_to_get(1:6) == 'THETA ' .or. var_to_get(1:6) == 'TC    ' &
     .or. var_to_get(1:6) == 'TK    ') then
    var_to_get(1:6) = 'T     '
  else if(var_to_get(1:6) == 'TD    ' .or. var_to_get(1:6) == 'RH    ' ) then
    var_to_get(1:6) = 'QVAPOR'
  else if(var_to_get(1:5) == 'TD_IN') then
    var_to_get(1:6) = 'QVAPOR'
  else if(var_to_get(1:2) == 'Z ') then
    var_to_get(1:6) = 'PH    '
  else if(var_to_get(1:4) == 'UMET') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:4) == 'VMET') then
    var_to_get(1:6) = 'V     '
  else if(var_to_get(1:8) == 'RU_TENDF') then
    var_to_get(1:8) = 'U     '
  else if(var_to_get(1:8) == 'RV_TENDF') then
    var_to_get(1:8) = 'V     '
  else if(var_to_get(1:7) == 'RV_TEND') then
    var_to_get(1:7) = 'V     '
  else if(var_to_get(1:8) == 'RW_TENDF') then
    var_to_get(1:8) = 'W     '
  else if(var_to_get(1:6) == 'T_TEND') then
    var_to_get(1:6) = 'T     '
  else if(var_to_get(1:10) == 'H_DIABATIC') then
    var_to_get(1:10) ='T     '
  else if(var_to_get(1:4) == 'VORT') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:6) == 'GRADPU') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:6) == 'GRADPV') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:3) == 'PHB') then
    var_to_get(1:6) = 'PH    '
  else if(var_to_get(1:2) == 'PV') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:2) == 'PV_INS') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:5) == 'DIREC') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:4) == 'BINS') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:7) == 'TEMGRAD') then
    var_to_get(1:7) = 'T      '
  else if(var_to_get(1:7) == 'THETA_E') then
    var_to_get(1:7) = 'T      '
  else if(var_to_get(1:7) == 'THETAE1') then
    var_to_get(1:7) = 'T      '
  else if(var_to_get(1:5) == 'RADAR') then
    var_to_get(1:7) = 'T      '
  else if(var_to_get(1:3) == 'DIV') then
    var_to_get(1:6) = 'U     '
  else if(var_to_get(1:8) == 'RH_INPUT') then
    var_to_get(1:8) = 'T       '
  end if
  end subroutine check_special_variable
!--------------------------------------------------------
  subroutine interp_to_z( data_in , nx_in , ny_in , nz_in , &
                              data_out, nx_out, ny_out, nz_out, &
                              z_in, z_out, missing_value, &
                              vertical_type, debug )
  implicit none
  integer, intent(in) :: nx_in , ny_in , nz_in
  integer, intent(in) :: nx_out, ny_out, nz_out
  real, intent(in) :: missing_value
  real, dimension(nx_in , ny_in , nz_in ), intent(in ) :: data_in, z_in
  real, dimension(nx_out, ny_out, nz_out), intent(out) :: data_out
  real, dimension(nz_out), intent(in) :: z_out
  logical, intent(in) :: debug
  character (len=1) :: vertical_type
  real, dimension(nz_in) :: data_in_z, zz_in
  real, dimension(nz_out) :: data_out_z
  integer :: i,j,k
    do i=1,nx_in
    do j=1,ny_in
      do k=1,nz_in
        data_in_z(k) = data_in(i,j,k)
        zz_in(k) = z_in(i,j,k)
      enddo
      do k=1,nz_out
        data_out_z(k) = data_out(i,j,k)
      enddo
      call interp_1d( data_in_z, zz_in, nz_in, &
                      data_out_z, z_out, nz_out, &
                      vertical_type, missing_value )
      do k=1,nz_out
        data_out(i,j,k) = data_out_z(k)
      enddo
    enddo
    enddo
  end subroutine interp_to_z
!----------------------------------------------
  subroutine interp_1d( a, xa, na, &
                        b, xb, nb, vertical_type, missing_value )
  implicit none
  integer, intent(in) :: na, nb
  real, intent(in), dimension(na) :: a, xa
  real, intent(in), dimension(nb) :: xb
  real, intent(out), dimension(nb) :: b
  real, intent(in) :: missing_value
  integer :: n_in, n_out
  logical :: interp
  real :: w1, w2
  character (len=1) :: vertical_type
  if ( vertical_type == 'p' ) then
  do n_out = 1, nb
    b(n_out) = missing_value
    interp = .false.
    n_in = 1
    do while ( (.not.interp) .and. (n_in < na) )
      if( (xa(n_in) >= xb(n_out)) .and. &
          (xa(n_in+1) <= xb(n_out)) ) then
        interp = .true.
        w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
        w2 = 1. - w1
        b(n_out) = w1*a(n_in) + w2*a(n_in+1)
      end if
      n_in = n_in +1
    enddo
  enddo
  else
  do n_out = 1, nb
    b(n_out) = missing_value
    interp = .false.
    n_in = 1
    do while ( (.not.interp) .and. (n_in < na) )
      if( (xa(n_in) <= xb(n_out)) .and. &
          (xa(n_in+1) >= xb(n_out)) ) then
        interp = .true.
        w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
        w2 = 1. - w1
        b(n_out) = w1*a(n_in) + w2*a(n_in+1)
      end if
      n_in = n_in +1
    enddo
  enddo
  endif
  end subroutine interp_1d
!-------------------------------------------------------------------------
!
! This routines has been taken "as is" from wrf_user_fortran_util_0.f
!
! This routine assumes
! index order is (i,j,k)
! wrf staggering
! units: pressure (Pa), temperature(K), height (m), mixing ratio (kg kg{-1})
! availability of 3d p, t, and qv; 2d terrain; 1d half-level zeta string
! output units of SLP are Pa, but you should divide that by 100 for the
! weather weenies.
! virtual effects are included
!
! Dave
      subroutine compute_seaprs ( nx , ny , nz , &
                                  z, t , p , q , &
                                  sea_level_pressure,debug)
! & t_sea_level, t_surf, level )
      IMPLICIT NONE
! Estimate sea level pressure.
      INTEGER nx , ny , nz
      REAL z(nx,ny,nz)
      REAL t(nx,ny,nz) , p(nx,ny,nz) , q(nx,ny,nz)
! The output is the 2d sea level pressure.
      REAL sea_level_pressure(nx,ny)
      INTEGER level(nx,ny)
      REAL t_surf(nx,ny) , t_sea_level(nx,ny)
      LOGICAL debug
! Some required physical constants:
      REAL R, G, GAMMA
      PARAMETER (R=287.04, G=9.81, GAMMA=0.0065)
! Specific constants for assumptions made in this routine:
      REAL TC, PCONST
      PARAMETER (TC=273.16+17.5, PCONST = 10000)
      LOGICAL ridiculous_mm5_test
      PARAMETER (ridiculous_mm5_test = .TRUE.)
! PARAMETER (ridiculous_mm5_test = .false.)
! Local variables:
      INTEGER i , j , k
      INTEGER klo , khi
      REAL plo , phi , tlo, thi , zlo , zhi
      REAL p_at_pconst , t_at_pconst , z_at_pconst
      REAL z_half_lowest
      REAL , PARAMETER :: cp = 7.*R/2.
      REAL , PARAMETER :: rcp = R/cp
      REAL , PARAMETER :: p1000mb = 100000.
      LOGICAL l1 , l2 , l3, found
! Find least zeta level that is PCONST Pa above the surface. We later use this
! level to extrapolate a surface pressure and temperature, which is supposed
! to reduce the effect of the diurnal heating cycle in the pressure field.
      t(:,:,:) = (t(:,:,:)+300.)*(p(:,:,:)/p1000mb)**rcp
      DO j = 1 , ny
         DO i = 1 , nx
            level(i,j) = -1
            k = 1
            found = .false.
            do while( (.not. found) .and. (k.le.nz))
               IF ( p(i,j,k) .LT. p(i,j,1)-PCONST ) THEN
                  level(i,j) = k
                  found = .true.
               END IF
               k = k+1
            END DO
            IF ( level(i,j) .EQ. -1 ) THEN
            PRINT '(A,I4,A)','Troubles finding level ', &
                        NINT(PCONST)/100,' above ground.'
            PRINT '(A,I4,A,I4,A)', &
                  'Problems first occur at (',i,',',j,')'
            PRINT '(A,F6.1,A)', &
                  'Surface pressure = ',p(i,j,1)/100,' hPa.'
            STOP 'Error_in_finding_100_hPa_up'
         END IF
         END DO
      END DO
! Get temperature PCONST Pa above surface. Use this to extrapolate
! the temperature at the surface and down to sea level.
      DO j = 1 , ny
         DO i = 1 , nx
            klo = MAX ( level(i,j) - 1 , 1 )
            khi = MIN ( klo + 1 , nz - 1 )
            IF ( klo .EQ. khi ) THEN
               PRINT '(A)','Trapping levels are weird.'
               PRINT '(A,I3,A,I3,A)','klo = ',klo,', khi = ',khi, &
                            ': and they should not be equal.'
               STOP 'Error_trapping_levels'
            END IF
         plo = p(i,j,klo)
         phi = p(i,j,khi)
         tlo = t(i,j,klo)*(1. + 0.608 * q(i,j,klo) )
         thi = t(i,j,khi)*(1. + 0.608 * q(i,j,khi) )
! zlo = zetahalf(klo)/ztop*(ztop-terrain(i,j))+terrain(i,j)
! zhi = zetahalf(khi)/ztop*(ztop-terrain(i,j))+terrain(i,j)
         zlo = z(i,j,klo)
         zhi = z(i,j,khi)
         p_at_pconst = p(i,j,1) - pconst
         t_at_pconst = thi-(thi-tlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)
         z_at_pconst = zhi-(zhi-zlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)
         t_surf(i,j) = t_at_pconst*(p(i,j,1)/p_at_pconst)**(gamma*R/g)
         t_sea_level(i,j) = t_at_pconst+gamma*z_at_pconst
         END DO
      END DO
! If we follow a traditional computation, there is a correction to the sea level
! temperature if both the surface and sea level temnperatures are *too* hot.
      IF ( ridiculous_mm5_test ) THEN
         DO j = 1 , ny
            DO i = 1 , nx
               l1 = t_sea_level(i,j) .LT. TC
               l2 = t_surf (i,j) .LE. TC
               l3 = .NOT. l1
               IF ( l2 .AND. l3 ) THEN
                  t_sea_level(i,j) = TC
               ELSE
                  t_sea_level(i,j) = TC - 0.005*(t_surf(i,j)-TC)**2
               END IF
            END DO
         END DO
      END IF
! The grand finale: ta da!
      DO j = 1 , ny
      DO i = 1 , nx
! z_half_lowest=zetahalf(1)/ztop*(ztop-terrain(i,j))+terrain(i,j)
         z_half_lowest=z(i,j,1)
         sea_level_pressure(i,j) = p(i,j,1) * &
                               EXP((2.*g*z_half_lowest)/ &
                                   (R*(t_sea_level(i,j)+t_surf(i,j))))
         sea_level_pressure(i,j) = sea_level_pressure(i,j)*0.01
      END DO
      END DO
    if (debug) then
      print *,'sea pres input at weird location i=20,j=1,k=1'
      print *,'t=',t(20,1,1),t(20,2,1),t(20,3,1)
      print *,'z=',z(20,1,1),z(20,2,1),z(20,3,1)
      print *,'p=',p(20,1,1),p(20,2,1),p(20,3,1)
      print *,'slp=',sea_level_pressure(20,1), &
               sea_level_pressure(20,2),sea_level_pressure(20,3)
    endif
! print *,'t=',t(10:15,10:15,1),t(10:15,2,1),t(10:15,3,1)
! print *,'z=',z(10:15,1,1),z(10:15,2,1),z(10:15,3,1)
! print *,'p=',p(10:15,1,1),p(10:15,2,1),p(10:15,3,1)
! print *,'slp=',sea_level_pressure(10:15,10:15), &
! sea_level_pressure(10:15,10:15),sea_level_pressure(20,10:15)
      end subroutine compute_seaprs
!------------------------------------------------------------------
  subroutine rotate_wind (u,v,d1,d2,d3,var, &
                          map_proj,cen_lon,xlat,xlon, &
                          truelat1,truelat2,data_out)
  implicit none
  integer, intent(in) :: d1, d2, d3
  real, dimension(d1,d2,d3) :: data_out
  integer :: map_proj,i,j,k
  real :: cen_lon, truelat1, truelat2, cone
  real, dimension(d1,d2,d3) :: u,v
  real, dimension(d1,d2) :: xlat, xlon, diff, alpha
  character (len=10), intent(in) :: var
   REAL , PARAMETER :: pii = 3.14159265
   REAL , PARAMETER :: radians_per_degree = pii/180.
       cone = 1. ! PS
       if( map_proj .eq. 1) then ! Lambert Conformal mapping
         IF (ABS(truelat1-truelat2) .GT. 0.1) THEN
            cone=(ALOG(COS(truelat1*radians_per_degree))- &
                  ALOG(COS(truelat2*radians_per_degree))) / &
            (ALOG(TAN((90.-ABS(truelat1))*radians_per_degree*0.5 ))- &
             ALOG(TAN((90.-ABS(truelat2))*radians_per_degree*0.5 )) )
         ELSE
            cone = SIN(ABS(truelat1)*radians_per_degree )
         ENDIF
       end if
       diff = xlon - cen_lon
       do i = 1, d1
       do j = 1, d2
         if(diff(i,j) .gt. 180.) then
           diff(i,j) = diff(i,j) - 360.
         end if
         if(diff(i,j) .lt. -180.) then
           diff(i,j) = diff(i,j) + 360.
         end if
       end do
       end do
       do i = 1, d1
       do j = 1, d2
          if(xlat(i,j) .lt. 0.) then
            alpha(i,j) = - diff(i,j) * cone * radians_per_degree
          else
            alpha(i,j) = diff(i,j) * cone * radians_per_degree
          end if
       end do
       end do
       if(var(1:1) .eq. "U") then
         do k=1,d3
           data_out(:,:,k) = v(:,:,k)*sin(alpha) + u(:,:,k)*cos(alpha)
         end do
       else if(var(1:1) .eq. "V") then
         do k=1,d3
           data_out(:,:,k) = v(:,:,k)*cos(alpha) - u(:,:,k)*sin(alpha)
         end do
       end if
  end subroutine rotate_wind
!------------------------------------------------------------------
      subroutine handle_err(rmarker,nf_status)
      include "netcdf.inc"
      integer nf_status
      character*(*) :: rmarker
      if (nf_status .ne. nf_noerr) then
         write(*,*) 'NetCDF error : ',rmarker
         write(*,*) '  ',nf_strerror(nf_status)
         stop
      endif
      end subroutine handle_err
!------------------------------------------------------------------
     subroutine check_all_variables ( file, &
                                      variables3d, desc3d, number_of_3dvar, &
                                      variables2d, desc2d, number_of_2dvar, &
                                      debug )
      include "netcdf.inc"
      character (len=80) :: file
      integer :: number_of_3dvar ,number_of_2dvar
      character (len=20), dimension(number_of_3dvar) :: variables3d
      character (len=20), dimension(number_of_2dvar) :: variables2d
      character (len=50), dimension(number_of_3dvar) :: desc3d
      character (len=50), dimension(number_of_2dvar) :: desc2d
      logical :: debug
      integer :: nf_status, ncid, rcode, id_var, trcode
      integer :: missing3d, missing2d
      integer :: newi
      nf_status = nf_open (file, nf_nowrite, ncid)
      call handle_err('Error opening file',nf_status)
      missing3d = 0
      do i = 1,number_of_3dvar
             if ( variables3d(i) == 'UMET' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "V", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'VMET' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "V", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'Z' ) then
          rcode = nf_inq_varid ( ncid, "PH", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PHB", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'P' ) then
          rcode = nf_inq_varid ( ncid, "P", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'THETA' ) then
          rcode = nf_inq_varid ( ncid, "T", id_var )
        else if ( variables3d(i) == 'TK' ) then
          rcode = nf_inq_varid ( ncid, "P", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "T", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'TC' ) then
          rcode = nf_inq_varid ( ncid, "P", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "T", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'TD' ) then
          rcode = nf_inq_varid ( ncid, "P", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "QVAPOR", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'RH' ) then
          rcode = nf_inq_varid ( ncid, "P", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "T", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "QVAPOR", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables3d(i) == 'VORT' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "V", id_var )
   if ( rcode == 0 ) print*,' #### Vortcity calculation is ready####'
        else if ( variables3d(i) == 'PV' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "V", id_var )
          if ( rcode == 0 ) print*,' #### Vortcity calculation is ready####'
        else if ( variables3d(i) == 'PV_INS' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "V", id_var )
          if ( rcode == 0 ) print*,' #### Vortcity calculation is ready####'
        else if ( variables3d(i) == 'DIREC' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "V", id_var )
          if ( rcode == 0 ) print*,' ####  Direction calculation is ready####'
        else if ( variables3d(i) == 'BINS' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "F", id_var )
   if ( rcode == 0 ) print*,' #### Vortcity calculation is ready####'
        else if ( variables3d(i) == 'DIV' ) then
          rcode = nf_inq_varid ( ncid, "U", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "V", id_var )
   if ( rcode == 0 ) print*,' #### Divergent calculation is ready####'
        else if ( variables3d(i) == 'TEMGRAD' ) then
          rcode = nf_inq_varid ( ncid, "T", id_var )
        else if ( variables3d(i) == 'RH_INPUT' ) then
          rcode = nf_inq_varid ( ncid, "QVAPOR", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "ZNW", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "MU0", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "T", id_var )
   if ( rcode == 0 ) print*,' #### RH_INPUT calculation is ready####'
        else if ( variables3d(i) == 'TD_IN' ) then
          rcode = nf_inq_varid ( ncid, "QVAPOR", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "ZNW", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "MU0", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "T", id_var )
   if ( rcode == 0 ) print*,' #### TD_IN calculation is ready####'
        else if ( variables3d(i) == 'THETA_E' ) then
          rcode = nf_inq_varid ( ncid, "QVAPOR", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "ZNW", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "MU0", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "T", id_var )
          if ( rcode == 0 ) print*,' ####  THETAE calculation from wrf_real is ready####'
 else if ( variables3d(i) == 'THETAE1' ) then
          rcode = nf_inq_varid ( ncid, "QVAPOR", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "P", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "T", id_var )
          if ( rcode == 0 ) print*,' ####  THETAE calculation from wrfout is ready####'
        else if ( variables3d(i) == 'RADAR' ) then
          rcode = nf_inq_varid ( ncid, "QRAIN", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "QICE", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "QSNOW", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "QGRAUP", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "P", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = nf_inq_varid ( ncid, "T", id_var )
          if ( rcode == 0 ) print*,' ####  RADAR calculation from wrfout is ready####'
        else
          rcode = nf_inq_varid ( ncid, variables3d(i), id_var )
        endif
        if (rcode .ne. 0) then
          write(6,*) ' Not in file, remove from list: ',trim(variables3d(i))
          variables3d(i) = ' '
          desc3d(i) = ' '
          missing3d = missing3d+1
        endif
      enddo
      missing2d = 0
      do i = 1,number_of_2dvar
         if ( variables2d(i) == 'U10M' ) then
          rcode = nf_inq_varid ( ncid, "U10", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "V10", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables2d(i) == 'V10M' ) then
          rcode = nf_inq_varid ( ncid, "U10", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "V10", id_var )
          if ( rcode == 0 ) rcode = trcode
        else if ( variables2d(i) == 'slvl' ) then
          rcode = nf_inq_varid ( ncid, "P", id_var )
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PB", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PH", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "PHB", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "T", id_var )
          if ( rcode == 0 ) rcode = trcode
          trcode = rcode
          rcode = nf_inq_varid ( ncid, "QVAPOR", id_var )
          if ( rcode == 0 ) rcode = trcode
!
! Chanh here
!
        else if ( variables2d(i) == 'pmsl' ) then
          rcode = nf_inq_varid ( ncid, "PMSL", id_var )
          print*,'rcode for PMSL from NCEP input is',rcode
        else if ( variables2d(i) == 'PSFC_REAL' ) then
          rcode = nf_inq_varid ( ncid, "PMSL", id_var )
          if (rcode == 0) rcode = nf_inq_varid ( ncid, "MU0", id_var )
          print*,'rcode for PMSL from NCEP input is',rcode
        else if ( variables2d(i) == 'SST' ) then
          rcode = nf_inq_varid ( ncid, "SST", id_var )
          print*,'rcode for SST from NCEP input is',rcode
        else
          rcode = nf_inq_varid ( ncid, variables2d(i), id_var )
        endif
        if (rcode .ne. 0) then
          write(6,*) ' Not in file, remove from list: ',trim(variables2d(i))
          variables2d(i) = ' '
          desc2d(i) = ' '
          missing2d = missing2d+1
        endif
      enddo
      newi = 0
      do i = 1,number_of_3dvar
        if ( variables3d(i) /= ' ' ) then
          newi = newi+1
          variables3d(newi) = variables3d(i)
          desc3d(newi) = desc3d(i)
        endif
      enddo
      number_of_3dvar = number_of_3dvar - missing3d
      newi = 0
      do i = 1,number_of_2dvar
        if ( variables2d(i) /= ' ' ) then
          newi = newi+1
          variables2d(newi) = variables2d(i)
          desc2d(newi) = desc2d(i)
        endif
      enddo
      number_of_2dvar = number_of_2dvar - missing2d
      nf_status = nf_close (ncid)
      call handle_err('Error closing file',nf_status)
     end subroutine check_all_variables
!
! Chanh
!
     SUBROUTINE smooth9p(arr,nx,ny,nz,ibgn,iend,jbgn,jend,kbgn,kend,tem1,npass)
!c
!c#######################################################################
!c
!c PURPOSE:
!c
!c 1 2 1
!c Smooth a 3-D array horizontally by the filter of { 2 4 2 }
!c 1 2 1
!c
!c#######################################################################
!c
!c INPUT:
!c
!c nx Number of grid points in the x-direction
!c ny Number of grid points in the y-direction
!c ibgn First index in x-direction in the soomthing region.
!c iend Last index in x-direction in the soomthing region.
!c jbgn First index in j-direction in the soomthing region.
!c jend Last index in j-direction in the soomthing region.
!c
!c arr 3-D array
!c
!c OUTPUT:
!c
!c arr 3-D array
!c
!c TEMPORARY:
!c
!c tem1 Temporary 3-D array
!c
!c#######################################################################
!c
!c Variable Declarations.
!c
!c
!c#######################################################################
!c
      implicit none
      integer nx ! Number of grid points in the x-direction
      integer ny ! Number of grid points in the y-direction
      integer nz ! Number of grid points in the z-direction
      integer ibgn
      integer iend
      integer jbgn
      integer jend
      integer kbgn
      integer kend
      integer n,npass
      real arr (nx,ny,nz) ! 3-D array
      real tem1(nx,ny,nz) ! Temporary array
!c
!c#######################################################################
!c
!c Misc. local variables:
!c
!c#######################################################################
!c
      integer i,j,k
      real wtf,wtfb,wtfc
!c
!c#######################################################################
!c
!c Include files:
!c
!c#######################################################################
!c
!c
!C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!C
!C Beginning of executable code...
!C
!C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!c
      wtf = 1.0/16.0
      wtfb = 1.0/12.0
      wtfc = 1.0/9.0
      DO k = kbgn, kend
      DO n = 1,npass
      DO j = jbgn+1, jend-1
       DO i = ibgn+1, iend-1
        tem1(i,j,k) = wtf &
      * ( arr(i-1,j-1,k) + 2.*arr(i,j-1,k) + arr(i+1,j-1,k) &
        + 2.*arr(i-1,j,k) + 4.*arr(i,j,k) + 2.*arr(i+1,j,k) &
        + arr(i-1,j+1,k) + 2.*arr(i,j+1,k) + arr(i+1,j+1,k) )
       ENDDO
      ENDDO
      DO j = jbgn+1, jend-1
        tem1(ibgn,j,k) = wtfb &
      * ( 2.*arr(ibgn,j-1,k) + arr(ibgn+1,j-1,k) &
        + 4.*arr(ibgn,j, k) + 2.*arr(ibgn+1,j,k) &
        + 2.*arr(ibgn,j+1,k) + arr(ibgn+1,j+1,k) )
        tem1(iend,j,k) = wtfb &
      * ( arr(iend-1,j-1,k) + 2.*arr(iend,j-1,k) &
        + 2.*arr(iend-1,j,k) + 4.*arr(iend,j,k) &
        + arr(iend-1,j+1,k) + 2.*arr(iend,j+1,k) )
      ENDDO
      DO i = ibgn+1, iend-1
        tem1(i,jbgn,k) = wtfb &
      * ( 2.*arr(i-1,jbgn,k) + 4.*arr(i,jbgn,k) + 2.*arr(i+1,jbgn,k) &
        + arr(i-1,jbgn+1,k) + 2.*arr(i,jbgn+1,k) + arr(i+1,jbgn+1,k) )
        tem1(i,jend,k) = wtfb &
      * ( arr(i-1,jend-1,k) + 2.*arr(i,jend-1,k) + arr(i+1,jend-1,k) &
        + 2.*arr(i-1,jend,k) + 4.*arr(i,jend,k) + 2.*arr(i+1,jend,k) )
      ENDDO
      tem1(ibgn,jbgn,k) = wtfc &
       * ( 2.*arr(ibgn,jbgn+1,k) + arr(ibgn+1,jbgn+1,k) &
         + 4.*arr(ibgn,jbgn,k) + 2.*arr(ibgn+1,jbgn,k) )
      tem1(ibgn,jend,k) = wtfc &
       * ( 4.*arr(ibgn,jend,k) + 2.*arr(ibgn+1,jend,k) &
         + 2.*arr(ibgn,jend-1,k) + arr(ibgn+1,jend-1,k) )
      tem1(iend,jbgn,k) = wtfc &
       * ( arr(iend-1,jbgn+1,k) + 2.*arr(iend,jbgn+1,k) &
         + 2.*arr(iend-1,jbgn,k) + 4.*arr(iend,jbgn,k) )
      tem1(iend,jend,k) = wtfc &
        * ( 2.*arr(iend-1,jend,k) + 4.*arr(iend,jend,k) &
          + arr(iend-1,jend-1,k) + 2.*arr(iend-1,jend,k) )
      ENDDO
      DO j = 1, ny
       DO i = 1, nx
        arr(i,j,k) = tem1(i,j,k)
       ENDDO
      ENDDO
      ENDDO
      RETURN
      END SUBROUTINE SMOOTH9P
!
! Chanh here
!
      SUBROUTINE center(slvl,UU,VV,nx,ny,nz,icen1,jcen1,minslvl1, &
                        icen2,jcen2,minslvl2,vmax1,vmax2,dx,dy,slat &
                        ,elat,slon,elon,xlat,xlon)
      integer :: nx,ny,nz,icen1,jcen1,icen2,jcen2
      real, dimension(nx,ny) :: slvl,UU,VV
      real :: minslvl1,minslvl2,t1,t2,t3,t4,vmax1,vmax2,dx,dy
      integer :: bndr,si,ei,sj,ej
      real :: slat,elat,slon,elon,xlat(nx,ny),xlon(nx,ny)
!
! Find the index for the area in which the center will be found
!
      si = 999
      ei = 999
      sj = 999
      ej = 999
      DO i = 1,nx-1
       IF (xlon(i,1).le.slon.and.slon.lt.xlon(i+1,1)) si = i
       IF (xlon(i,1).le.elon.and.elon.lt.xlon(i+1,1)) ei = i
      ENDDO
      DO j = 1,ny-1
       IF (xlat(1,j).le.slat.and.slat.lt.xlat(1,j+1)) sj = j
       IF (xlat(1,j).le.elat.and.elat.lt.xlat(1,j+1)) ej = j
      ENDDO
      PRINT*,'si,ei,sj,ej',si,ei,sj,ej
      IF (si.eq.999.or.ei.eq.999.or.sj.eq.999.or.ej.eq.999) THEN
       PRINT*,'Domain finding center is out of range'
       PRINT*,'Adjust domain now...'
       if (si.eq.999) si = 1
       if (ei.eq.999) ei = nx
       if (sj.eq.999) sj = 1
       if (ej.eq.999) ej = ny
       PRINT*,'New domain is: si,ei,sj,ej',si,ei,sj,ej
! STOP
      ENDIF
!
! Find first center
!
      bndr = int(180.e03/dx)
      t3 = 0. ! criteria for maximum wind of a TS
      minslvl1 = 1008.
      vmax1 = t3
      icen1 = 9999
      jcen1 = 9999
      t4 = 0.
      DO j = sj,ej ! bndr+1,ny-bndr
       DO i = si,ei ! !bndr+1,nx-bndr
        t1 = (slvl(i-1,j-1)+slvl(i-1,j)+slvl(i-1,j+1) &
                      +slvl(i,j-1) +slvl(i,j+1) &
                      +slvl(i+1,j-1)+slvl(i+1,j)+slvl(i+1,j+1))/8.
        IF (slvl(i,j).lt.minslvl1.and.(t1-slvl(i,j).lt.5)) THEN
         icen1 = i
         jcen1 = j
         minslvl1 = slvl(i,j)
        ENDIF
        IF (vmax1.lt.sqrt(UU(i,j)**2 + VV(i,j)**2)) vmax1 = sqrt(UU(i,j)**2 + VV(i,j)**2)
       ENDDO
      ENDDO
      PRINT*,'Chanh',icen1,jcen1,vmax1,minslvl1
      READ*
      IF (icen1.eq.9999) THEN
       print*,' ### can not find the first center'
       icen1 = 1
       jcen1 = 1
       icen2 = 1
       jcen2 = 1
       RETURN
      ENDIF
!
! Find second center
!
      minslvl2 = 1008.
      vmax2 = t3
      icen2 = 9999
      jcen2 = 9999
      t4 = 0.
      DO j = bndr,ny-bndr
       DO i = bndr,nx-bndr
        IF (abs(i-icen1).ge.10.and.abs(j-jcen1).ge.10) THEN
         t1 = (slvl(i-1,j-1)+slvl(i-1,j)+slvl(i-1,j+1) &
                      +slvl(i,j-1) +slvl(i,j+1) &
                      +slvl(i+1,j-1)+slvl(i+1,j)+slvl(i+1,j+1))/8.
         DO j1 = j-bndr,j+bndr
          DO i1 = i-bndr,i+bndr
           IF (vmax2.lt.sqrt(UU(i,j)**2 + VV(i,j)**2)) &
               vmax2 = sqrt(UU(i,j)**2 + VV(i,j)**2)
          ENDDO
         ENDDO
         IF (slvl(i,j).lt.minslvl2.and.(t1-slvl(i,j).lt.5).and.vmax2.gt.t3) THEN
          icen2 = i
          jcen2 = j
          minslvl2 = slvl(i,j)
         ENDIF
         IF (t4.lt.sqrt(UU(i,j)**2 + VV(i,j)**2)) t4 = sqrt(UU(i,j)**2 + VV(i,j)**2)
        ENDIF
       ENDDO
      ENDDO
      IF (icen2.eq.9999) THEN
       print*,' ### can not find the second center'
       icen2 = 1
       jcen2 = 1
      ENDIF
      END SUBROUTINE center
      SUBROUTINE p_min(slvl,nx,ny,minslvl,dx,dy,slat,elat,slon,elon,icen1,jcen1,xlat,xlon)
      implicit none
      integer :: nx,ny,icen1,jcen1
      real, dimension(nx,ny) :: slvl
      real :: minslvl,dx,dy,t1
      integer :: i,j,si,ei,sj,ej
      real :: slat,elat,slon,elon,xlat(nx,ny),xlon(nx,ny)
!
! Find the index for the area in which the center will be found
!
      si = 999
      ei = 999
      sj = 999
      ej = 999
      DO i = 1,nx-1
       IF (xlon(i,1).le.slon.and.slon.lt.xlon(i+1,1)) si = i
       IF (xlon(i,1).le.elon.and.elon.lt.xlon(i+1,1)) ei = i
      ENDDO
      DO j = 1,ny-1
       IF (xlat(1,j).le.slat.and.slat.lt.xlat(1,j+1)) sj = j
       IF (xlat(1,j).le.elat.and.elat.lt.xlat(1,j+1)) ej = j
      ENDDO
      PRINT*,' TC center searching domain:',si,ei,sj,ej
      IF (si.eq.999.or.ei.eq.999.or.sj.eq.999.or.ej.eq.999) THEN
       PRINT*,' -> Domain finding center is out of range'
       PRINT*,' -> Adjust domain now...'
       if (si.eq.999) si = 1
       if (ei.eq.999) ei = nx
       if (sj.eq.999) sj = 1
       if (ej.eq.999) ej = ny
       PRINT*,' -> New domain:',si,ei,sj,ej
      ENDIF
!
! Find the center of pmin now
!
      minslvl = 1015.
      icen1 = 9999
      jcen1 = 9999
      DO j = sj,ej
       DO i = si,ei
        t1 = (slvl(i-1,j-1)+slvl(i-1,j)+slvl(i-1,j+1) &
                      +slvl(i,j-1) +slvl(i,j+1) &
                      +slvl(i+1,j-1)+slvl(i+1,j)+slvl(i+1,j+1))/8.
        IF (slvl(i,j).lt.minslvl) THEN
         icen1 = i
         jcen1 = j
         minslvl = slvl(i,j)
        ENDIF
       ENDDO
      ENDDO
      IF (icen1.eq.9999) THEN
       print*,' ### can not find the first center'
       icen1 = -99
       jcen1 = -99
       minslvl = -99
       RETURN
      ENDIF
      END SUBROUTINE p_min
      SUBROUTINE v_min(u3d,v3d,k_rmw,nx,ny,nz,dx,dy,icen1,jcen1,icen2,jcen2,vmin)
      implicit none
      integer :: nx,ny,nz,k_rmw,icen1,jcen1,icen2,jcen2
      real :: u3d(nx,ny,nz),v3d(nx,ny,nz)
      real :: u(nx,ny),v(nx,ny),dx,dy,vmin
      real :: t1,t2,t3,t4
      integer :: i,j,radi
      do j = 1,ny
       do i = 1,nx
        u(i,j) = (u3d(i,j,k_rmw-1)+u3d(i,j,k_rmw)+u3d(i,j,k_rmw+1))/3.
        v(i,j) = (v3d(i,j,k_rmw-1)+v3d(i,j,k_rmw)+v3d(i,j,k_rmw+1))/3.
       enddo
      enddo
      radi = int(30.e+03/dx)
      vmin = 1000.
      icen2 = 9999
      jcen2 = 9999
      do i = icen1-radi,icen1+radi
       do j = jcen1-radi,jcen1+radi
         t1 = sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
         if (vmin.gt.t1) then
          vmin = t1
          icen2 = i
          jcen2 = j
         endif
       enddo
      enddo
      IF (icen2.eq.9999.or.jcen2.eq.9999) THEN
       print*,' ### can not find the second center'
       icen2 = 1
       jcen2 = 1
       RETURN
      ENDIF
      END SUBROUTINE v_min
      SUBROUTINE v_max(u3d,v3d,h3d,k_rmw,nx,ny,nz,dx,dy,imax,jmax,vmax,z_vmax,r_34kt,icen2,jcen2)
      implicit none
      integer :: nx,ny,nz,k_rmw,imax,jmax,icen2,jcen2
      real :: u3d(nx,ny,nz),v3d(nx,ny,nz),h3d(nx,ny,nz)
      real :: u(nx,ny),v(nx,ny),dx,dy,vmax
      real :: t1,t2,t3,t4
      real :: z_vmax,r_34kt ! 34 kt = 17.5 m/s
      integer :: i,j,k,radi
!
! define 2d arrays
!
      do j = 1,ny
       do i = 1,nx
        u(i,j) = (u3d(i,j,k_rmw-1)+u3d(i,j,k_rmw)+u3d(i,j,k_rmw+1))/3.
        v(i,j) = (v3d(i,j,k_rmw-1)+v3d(i,j,k_rmw)+v3d(i,j,k_rmw+1))/3.
       enddo
      enddo
      radi = int(180.e+03/dx)
      vmax = 0.
      imax = 9999
      jmax = 9999
      do i = max(1,icen2-radi),min(nx,icen2+radi)
       do j = max(1,jcen2-radi),min(ny,jcen2+radi)
         t1 = sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))
         if (vmax.lt.t1) then
          vmax = t1
          imax = i
          jmax = j
         endif
       enddo
      enddo
      IF (imax.eq.9999.or.jmax.eq.9999) THEN
       print*,' ### can not find the RMW'
       imax = 1
       jmax = 1
       vmax = -99
       RETURN
      ENDIF
!
! search for the height of VMAX at the thru-center cross section
!
      z_vmax = 0.
      t1 = 0.
      do k = 1,nz
       if (h3d(icen2,jcen2,k).le.1e4) then
        do i = 1,nx
         t2 = (v3d(i,jcen2-1,k)+v3d(i,jcen2,k)+v3d(i,jcen2+1,k))/3.
         if (t1.lt.t2) then
          t1 = t2
          z_vmax = h3d(i,jcen2,k)
         endif
        enddo
       endif
      enddo
!
! search for the radius of 34-kt wind (17.5 m/s) at 500 mb
!
      t1 = 0
      do i = icen2+int(5.e4/dx),nx
       t4 = (v(i-1,jcen2)+v(i,jcen2)+v(i+1,jcen2))/3.
       if (t4.lt.17.5) then
        t1 = (i-icen2)*dx
        goto 101
       endif
      enddo
101 continue
      t2 = 0.
      do i = nx-1,icen2,-1
       t3 = (v(i-1,jcen2)+v(i,jcen2)+v(i+1,jcen2))/3.
       if (t3.ge.17.5) then
        t2 = (i-icen2)*dx
        goto 102
       endif
      enddo
102 continue
! r_34kt = (t1+t2)/2.
      r_34kt = t2
      print*,' Radius of 34 kt wind is:',t1,t2
      END SUBROUTINE v_max
!------------------------------------------------------------------
      subroutine thetae(wk,t,p,qv,lq,lp,km)
!---------------------------------------------------------
! to calculate equivlent potential temperature all vertical levels
!---------------------------------------------------------
      parameter(grv=9.805,rgas=287.05)
      parameter(cp=1004.6,rcp=rgas/cp,al=2.5e6)
      integer lq,lp,km
      real t(lq,lp,km),p(lq,lp,km)
      real qv(lq,lp,km),wk(lq,lp,km)
      real TL
      do 10 i=1,lq
       do 10 j=1,lp
        do 10 k=2,km-1
         TL=2840.0/(3.5*alog(t(i,j,k))-alog(p(i,j,k)*0.01*qv(i,j,k) &
            /(0.622+qv(i,j,k)))-4.805)+55.0
         sita=t(i,j,k)*(1.e5/p(i,j,k))**rcp
         tqs=1000.0*qv(i,j,k)*(1.+0.61*qv(i,j,k))*(3.376/TL-.00254)
         wk(i,j,k)=sita*exp(tqs)
10 continue
      do i=1,lq
       do j=1,lp
        wk(i,j,1)=0.3*wk(i,j,2)
        wk(i,j,km)=wk(i,j,km-1)
       enddo
      enddo
! print*,k,'tl=',tl,'t(k)=',t(i,j,k),'qv(k)=',qv(i,j,k)
!--------------------------------------------------------------
      return
      end subroutine thetae
      SUBROUTINE car_cyl(nx,ny,nz,nr,nt,icen,jcen,dx,dy,u,ucy)
!
! Input: u : an 3D array in Cartesian cordinate need to be converted (nx,ny,nz)
! nx : number of grid poitns in x direction of u
! ny : number of grid poitns in y direction of u
! nz : number of grid poitns in z direction of u
! nr : number of grid poitns in radial direction of ucy
! nt : number of grid poitns in azimuthal direction of ucy
! icen : i index of center position of cylidrical coordinate in (x,y) grid
! jcen : j index of center position of cylidrical coordinate in (x,y) grid
! dx : grid distance in x direction
! dy : grid distance in y direction
! Output:
! ucy : an 3D array in cylindrical coordinate (nr,nt,nz)
!
! Note: the maximum radius of a circle within a retangular is equal to half of the shortest side
! So, the cycle can not sweep all points within the retangular --> max(nr) = min(nx,ny)/2...
!
! Author: Chanh Q. Kieu
!
      INTEGER Nx,Ny,Nz,Nr,Nt
      REAL u(Nx,Ny,Nz),ucy(Nr,Nt,Nz)
      REAL r1,r2,r3,r4,xtime,r,theta
      REAL dx,dy,pi,dthe,dr,x,y
      INTEGER sx,sy,icen,jcen,ix,iy,thres
      thres = Ny-jcen-1
      IF (thres.gt.Nx-icen-1) thres=Nx-icen-1
! print*,'Threshold of coordinate transformation is',thres
! print*,icen,jcen,nx,ny
      if (thres.ge.nr) thres=nr-2
      dr = dx
      pi = 4.*atan(1.)
      dthe = (2*pi)/Nt
      DO k = 1,Nz
       DO i = 2,thres
        DO j = 1,Nt
         r = (i-1)*dr
         theta = (j-1)*dthe
         x = r*cos(theta)
         y = r*sin(theta)
         IF (x.ge.0) THEN
          sx = 1
         ELSE
          sx = -1
         ENDIF
         IF (y.ge.0) THEN
          sy = 1
         ELSE
          sy = -1
         ENDIF
         ix = int((x+sx*0.1)/dx)
         iy = int((y+sy*0.1)/dy)
         r1 = sqrt((x- ix*dx)**2 + (y- iy*dy)**2)
         r2 = sqrt((x-(ix+sx)*dx)**2 + (y- iy*dy)**2)
         r3 = sqrt((x-(ix+sx)*dx)**2 + (y-(iy+sy)*dy)**2)
         r4 = sqrt((x- ix*dx)**2 + (y-(iy+sy)*dy)**2)
         ix = icen + ix
         iy = jcen + iy
         if (ix.lt.1) ix = 1
         if (ix.gt.nx) ix = nx
         if (iy.lt.1) iy = 1
         if (iy.gt.ny) iy = ny
         ucy(i,j,k) = (u(ix,iy,k)*r2*r3*r4 &
                    + u(ix+sx,iy,k)*r1*r3*r4 &
                    + u(ix+sx,iy+sy,k)*r1*r2*r4 &
                    + u(ix,iy+sy,k)*r1*r2*r3) &
                    / (r2*r3*r4 + r1*r3*r4 + r1*r2*r4 + r1*r2*r3)
        ENDDO
       ENDDO
       DO j = 1,Nt
        ucy(1,j,k) = ucy(2,j,k)
       ENDDO
       DO i = thres+1,Nr
        DO j = 1,Nt
         ucy(i,j,k) = ucy(i-1,j,k)
        ENDDO
       ENDDO
      ENDDO
      CALL fill3(ucy,nr,nt,nz)
      RETURN
      END SUBROUTINE car_cyl
      subroutine vmax_height(v,h,nx,ny,nr,nt,nz,z_vmax,vmax0,rmw0,r_34kt,k_rmw,dr)
      implicit none
      integer nr,nt,nz,nx,ny
      real v(nr,nt,nz),h(nx,ny,nz),mv(nr,nz)
      real t1,t2,t3,t4,z_vmax,rmw0,r_34kt,dr,vmax0
      integer i,j,k,k_rmw
      t2 = 0.
      do k = 1,nz
       if (h(1,1,k).lt.1e4) then
        do i = 1,nr
         t1 = 0.
         do j = 1,nt
          t1 = t1 + v(i,j,k)
         enddo
         t1 = t1/nt
         if (t2.lt.t1) then
          t2 = t1
          z_vmax = h(i,j,k)
         endif
        enddo
       endif
      enddo
!
! find azimuthal mean
!
      do k = 1,nz
       mv(1,k) = 0.
       do i = 2,nr
        mv(i,k) = 0.
        do j = 1,nt
         mv(i,k) = mv(i,k) + v(i,j,k)
        enddo
        mv(i,k) = mv(i,k)/nt
       enddo
      enddo
!
! searching for the RMW and r_34kt
!
      rmw0 = 0.
      vmax0 = 0.
      r_34kt = 54.e3
      do i = 2,nr
       t2 = (mv(i,k_rmw-1)+mv(i,k_rmw)+mv(i,k_rmw+1))/3.
       if (t2.gt.vmax0) then
        rmw0 = (i-1)*dr
        vmax0 = t2
       endif
      enddo
      do i = nr,2,-1
       t2 = (mv(i,k_rmw-1)+mv(i,k_rmw)+mv(i,k_rmw+1))/3.
       if (t2.gt.17.) then
        r_34kt = (i-1)*dr
        goto 301
       endif
! if (i.le.3) then
! print*,' Could not find r_34kt...stop',i,t2
! do j = nr,2,-1
! t3 = (mv(j,k_rmw-1)+mv(j,k_rmw)+mv(j,k_rmw+1))/3.
! print*,i,j,t3
! read*
! enddo
! endif
      enddo
301 continue
      end subroutine vmax_height
      subroutine fill3(a,nx,ny,nz)
      integer nx,ny,nz
      real a(nx,ny,nz)
      do i = 1,nx
       do k = 1,nz
        a(i,1,k) = a(i,3,k) + 2*(a(i,2,k)-a(i,3,k))
        a(i,ny,k) = a(i,ny-2,k) + 2*(a(i,ny-1,k)-a(i,ny-2,k))
       enddo
      enddo
      do j = 1,ny
       do k = 1,nz
        a(1,j,k) = a(3,j,k) + 2*(a(2,j,k)-a(3,j,k))
        a(nx,j,k) = a(nx-2,j,k) + 2*(a(nx-1,j,k)-a(nx-2,j,k))
       enddo
      enddo
      do i = 2,nx-1
       do j = 2,ny-1
        a(i,j,1) = a(i,j,3) + 2*(a(i,j,2)-a(i,j,3))
        a(i,j,nz) = a(i,j,nz-2) + 2*(a(i,j,nz-1)-a(i,j,nz-2))
       enddo
      enddo
      return
      end subroutine fill3
      subroutine radfld(rdr,t,qr,qi,qs,qg,p,lq,lp,km)
      real qr(lq,lp,km),p(lq,lp,km),rdr(lq,lp,km)
      real t(lq,lp,km),qs(lq,lp,km),qi(lq,lp,km),qg(lq,lp,km)
      rdr= 0.
      rgas=287.0
      pi=4.0*atan(1.0)
      drain=1.0e3
      dsnow=1.0e2
      dgraupel=4.0e2
      ron=8.0e6
      son=2.0e7
      gon=5.0e7
      arain=842.0
      brain=0.8
      asnow=11.72
      bsnow=0.41
      agraupel=19.3
      bgraupel=0.37
      ppir=1.0/(pi*ron*drain)
      ppis=1.0/(pi*son*dsnow)
      ppig=1.0/(pi*gon*dgraupel)
      brain4=4.0+brain
      vtcr=arain*gama(brain4)/6.0
      bsnow4=4.0+bsnow
      vtcs=asnow*gama(bsnow4)/6.0
      bgraupel4=4.0+bgraupel
      vtcg=asnow*gama(bgraupel4)/6.0
! Constants for variable son
      const1a=2.5e6*exp(.94*alog((1.0/1000.0*1.0/3600.0)))
      const1b=asnow*gama(bsnow4)/6.0
      cnsns=4.0/(4.0-0.94*bsnow)
!--------------------------------------------------
      do 10 k=1,km
      do 10 i=1,lq
      do 10 j=1,lp
      qr(i,j,k)=amax1(1.0e-20,qr(i,j,k))
      qs(i,j,k)=amax1(1.0e-20,qs(i,j,k))
      qg(i,j,k)=amax1(1.0e-20,qg(i,j,k))
      qi(i,j,k)=amax1(1.0e-20,qi(i,j,k))
      dens=p(i,j,k)/(rgas*t(i,j,k))
!-------------------------------------------------
! To calculate the slope factors for rain and snow
!-------------------------------------------------
      if(qs(i,j,k).le.1.0e-8) then
      sonv=son
      else
      densqi=dens*qs(i,j,k)
      fm=alog(pi*dsnow/densqi)
      dum1=exp(.25*bsnow*fm)
      dum1=const1a*exp(.94*alog(drain/(densqi*const1b)*dum1))
      sonv=exp(cnsns*alog(dum1))
      endif
      if(sonv.gt.son) sonv=son
      sonv=sonv/son
      if(qg(i,j,k).le.1.e-12) then
      gonv=gon
      else
      gonv=2.38*exp(.92*alog(pi*dgraupel/(dens*qg(i,j,k))))
      gonv=amax1(1.0e4,amin1(gonv,gon))
      endif
      gonv=gonv/gon
!------------------------------------------------
      dum1=vtcr*exp(.25*brain*alog((dens*qr(i,j,k)*ppir)))
      dum2=vtcs*exp(.25*bsnow*alog((dens*qs(i,j,k)*ppis/sonv)))
      dum3=vtcg*exp(.25*bgraupel*alog((dens*qg(i,j,k)*ppig/gonv)))
      dum4=1.0*3.29*exp(0.16*alog((dens*qi(i,j,k))))
      frn=amax1(1.0e-20,dens*dum1*qr(i,j,k)*3.6e3)
      fic=amax1(1.0e-20,dens*(dum2*qs(i,j,k)+dum3*qg(i,j,k)+ &
                dum4*qi(i,j,k))*3.6e3)
!-------------------------------------
      zzr=300.0*frn**1.35
      zzi=427.0*fic**1.09
      zz=zzr+zzi
      rdr(i,j,k)=amax1(0.,10.0*alog10(zz))
  10 continue
!
      return
      end subroutine radfld
      real function gama(x)
!
! This routine calculates the gamma function for a real argument x.
!
      real c(7),p(8),q(8)
      logical parity
      data one,half,twelve,two,zero/1.0e0,0.5e0,12.0e0,2.0e0,0.0e0/, &
           sqrtpi/0.9189385332046727417803297e0/, &
           pi/3.1415926535897932384626434e0/
      data xbig,xminin,eps/35.040e0,1.18e-38,1.19e-7/,xinf/3.4e38/
!
      data p/-1.71618513886549492533811e+0,2.47656508055759199108314e+1, &
             -3.79804256470945635097577e+2,6.29331155312818442661052e+2, &
             8.66966202790413211295064e+2,-3.14512729688483675254357e+4, &
             -3.61444134186911729807069e+4,6.64561438202405440627855e+4/
      data q/-3.08402300119738975254353e+1,3.15350626979604161529144e+2, &
            -1.01515636749021914166146e+3,-3.10777167157231109440444e+3, &
             2.25381184209801510330112e+4,4.75584627752788110767815e+3, &
            -1.34659959864969306392456e+5,-1.15132259675553483497211e+5/
      data c/-1.910444077728e-03,8.4171387781295e-04, &
          -5.952379913043012e-04,7.93650793500350248e-04, &
          -2.777777777777681622553e-03,8.333333333333333331554247e-02, &
           5.7083835261e-03/
!
      conv(i)=real(i)
      parity=.false.
      fact=one
      n=0
      y=x
      if(y.le.zero) then
      y=-x
      y1=aint(y)
      res=y-y1
      if(res.ne.zero) then
      if(y1.ne.aint(y1*half)*two) parity=.true.
      fact=-pi/sin(pi*res)
      y=y+one
      else
      res=xinf
      goto 900
      endif
      endif
!
      if(y.lt.eps) then
      if(y.ge.xminin) then
      res=one/y
      else
      res=xinf
      goto 900
      endif
      else if(y.lt.twelve) then
      y1=y
      if(y.lt.one) then
      z=y
      y=y+one
      else
      n=int(y)-1
      y=y-conv(n)
      z=y-one
      endif
!
      xnum=zero
      xden=one
!vdir novector
      do 260 i=1,8
      xnum=(xnum+p(i))*z
      xden=xden*z+q(i)
 260 continue
!
      res=xnum/xden+one
      if(y1.lt.y) then
      res=res/y1
      else if(y1.gt.y) then
!vdir novector
      do 290 i=1,n
      res=res*y
      y=y+one
 290 continue
      endif
      else
      if(y.le.xbig) then
      ysq=y*y
      sum=c(7)
!vdir novector
      do 350 i=1,6
      sum=sum/ysq+c(i)
 350 continue
      sum=sum/y-y+sqrtpi
      sum=sum+(y-half)*log(y)
      res=exp(sum)
      else
      res=xinf
      goto 900
      endif
      endif
!
      if(parity) res=-res
      if(fact.ne.one) res=fact/res
 900 gama=res
!
      return
      end function gama
END MODULE module_wrf_to_grads_util

!
! [NOTE]:      This program is to generate a bogus vortex, based on some 
!              idealized profile and several inputs from TC vital
! 
! [INPUT]:     A TC vital in the standard format is needed to runthis 
!              program (tmpvit)
!
! [OUTPUT]:    A text file (bvortex.txt) that contains the data arranged
!              in the format (u,v,T,z,lon,lat,level).
!
! [HISTORY]: - 01 Nov 2012: created from the bogus vortex program
!            - 18 Apr 2013: added capability for choosing GFS storm
!                           center to remove the discrepancy between
!                           best track center and GFS center
!
! [AUTHOR]:    Chanh Q. Kieu, NOAA scientist
!              Email: chanh.kieu@noaa.gov
!
! [REFERENCE]: Kieu, C. Q. (2005): arxiv.org
!
! [COPYRIGHT]: (C) 2012
!
!===========================================================================
      program bogus_vortex_profiles
      implicit none
      INCLUDE 'netcdf.inc'
! 
! netcdf attribution io
!     
      integer, parameter :: n=4
      integer            :: ncid,status
      integer            :: ndims,nvars,ngatts,unlimdimid
      integer            :: uid,vid,wid,tid,pid
      integer            :: undim,vndim,wndim,tndim
      integer            :: udim(n),vdim(n),wdim(n),tdim(n)
! 
! C-grid info from the wrfinput_D01
!     
      integer            :: wrf_nx,wrf_ny,wrf_nz,wrf_nt             ! wrf grid dim
      integer            :: nx1,ny1,nz1,nt1                         ! wrf grid dim extended
      real               :: wrf_dx,wrf_dy,wrf_dz                    ! wrf grid size
      real,allocatable   :: wrf_u(:,:,:,:),wrf_v(:,:,:,:)           ! wrf level z on grid
      real,allocatable   :: wrf_lat(:,:,:),wrf_lon(:,:,:)           ! wrf lat/lon grid 
      real,allocatable   :: tem4d(:,:,:,:)                          ! wrf poten temp perturb
      character*100      :: name,tem
      integer            :: tem_len
!
! bogus vortex vars
!
      real               :: pmin,penv,vmax,clat,clon,r34,roci,rmw
      integer            :: nx,ny,nz
      real,allocatable   :: u(:,:,:),v(:,:,:),h(:,:,:),t(:,:,:),z(:)
      real               :: dx,dy,dz,dlat,dlon,mis
      character*1        :: vert_type
      integer            :: irec,debug,ic,jc
      integer            :: i,j,k,vortex_opt
      integer            :: ic_o,jc_o,ic_g,jc_g
      real               :: sp_min,sp_tem
!
! obtain basic TC information from TCvital
!
      call tcvital(pmin,penv,vmax,clat,clon,r34,roci,rmw)
      print*,'clat = ',clat,' degree'
      print*,'clon = ',clon,' degree'
      print*,'pmin = ',pmin,' mb'
      print*,'penv = ',penv,' mb'
      print*,'roci = ',roci,' km'
      print*,'vmax = ',vmax,' m/s'
      print*,'rmw  = ',rmw,' km'
!
! initialize vortex domain 
!
      nz         = 30                        ! num of vert levels
      nx         = 201                       ! num of x dim
      ny         = 201                       ! num of y dim
      dx         = 9000                      ! grid mesh in x dim [m]
      dy         = dx                        ! dy  = dx [m]
      mis        = -99999.                   ! missing valie
      irec       = 1                         ! record length for Grads
      vortex_opt = 2                         ! option for vortex profile
      vert_type  = 's'                       ! type of output vertical coordinate
      print*,'vortex option is: ',vortex_opt
      allocate(u(nx,ny,nz),v(nx,ny,nz))
      allocate(h(nx,ny,nz),t(nx,ny,nz),z(nz))
!
! create a bogus vortex of type I (Rakine profile at surface, and decrease
! linearly upward (Rounno and Emanuel 1987 vertical profile)
!
      if (vortex_opt.eq.1) then
       call vortex1(nx,ny,nz,dx,dy,u,v,h,t,vert_type,vmax,rmw,roci,z)
!
! create a bogus vortex of type II (Rakine profile at surface, and decrease
! upward as given by Kieu and Zhang 2009.
!
      elseif (vortex_opt.eq.2) then
       call vortex2(nx,ny,nz,dx,dy,u,v,h,t,vert_type,vmax,rmw,roci,z)
!
! create a bogus vortex of type III with radial profile at surface as given
! by RE87 and the vertical profile given by KZ09.
!
      elseif (vortex_opt.eq.3) then
       call vortex3(nx,ny,nz,dx,dy,u,v,h,t,vert_type,vmax,rmw,roci,z)
      else
       print*,'vortex_opt = ',vortex_opt,' is not supported. Stop!'
       stop
      endif
!
! now find the GFS storm center, based on the observed storm center
!
      status = NF_OPEN('wrfinput_d01',nf_write,ncid)
      print*,''
      print*,'bvortex.exe: 1. NF_OPEN and return value: '
      print*,'bvortex.exe: status     =',status
      print*,'bvortex.exe: nf_noerr   =',nf_noerr
      print*,'bvortex.exe: ncid       =',ncid
!
! probing some dimension and structure of data fields
!
      status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
      print*,''
      print*,'bvortex.exe: 2. NF_INQ returns values: '
      print*,'bvortex.exe: ncid       =',ncid            ! file id
      print*,'bvortex.exe: ndims      =',ndims           ! number of dims
      print*,'bvortex.exe: nvars      =',nvars           ! number of varibales
      print*,'bvortex.exe: ngatts     =',ngatts          ! number of globale attributes
      print*,'bvortex.exe: unlimdimid =',unlimdimid      ! number of umlimitted dim
!
! pull our varid and dim information for a temporary variable
! which will be used later.
!
      tem                = 'T'
      tem_len            = len_trim(tem)
      print*,'bvortex.exe: Checking var is: ',tem(1:tem_len)
      status = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status = NF_INQ_VARNDIMS(ncid,uid,undim)
      status = NF_INQ_VARDIMID(ncid,uid,udim)
      IF (undim.ne.n) THEN
       print*,'bvortex.exe: Number of dim is /= 4...re-allocation now'
       status            = NF_CLOSE(ncid)
       STOP
      ENDIF
      print*,''
      print*,'bvortex.exe: 3. Pull out information about: ',tem(1:tem_len)
      print*,'bvortex.exe: uid        =',uid
      print*,'bvortex.exe: undim      =',undim
      DO i               = 1,undim
       j                 = udim(i)
       status            = NF_INQ_DIMNAME(ncid,j,name)
       status            = NF_INQ_DIMLEN(ncid,j,k)
       IF (i.eq.1) wrf_nx    = k
       IF (i.eq.2) wrf_ny    = k
       IF (i.eq.3) wrf_nz    = k
       IF (i.eq.4) wrf_nt    = k
       print*,'bvortex.exe: Dimension name is:',i,udim(i),name(1:20)
      ENDDO
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DX',wrf_dx)
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DY',wrf_dy)
      print*,'bvortex.exe: wrf_nx =',wrf_nx
      print*,'bvortex.exe: wrf_ny =',wrf_ny
      print*,'bvortex.exe: wrf_nz =',wrf_nz
      print*,'bvortex.exe: wrf_nt =',wrf_nt
      print*,'bvortex.exe: wrf_dx =',wrf_dx
      print*,'bvortex.exe: wrf_dy =',wrf_dy
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variablies and read all the map
! information. Remember to convert geopotential to geopotential
! height for coordiante mapping at model level
!
      nx1                = wrf_nx + 1
      ny1                = wrf_ny + 1
      nz1                = wrf_nz + 1
      allocate(wrf_lat(wrf_nx,wrf_ny,wrf_nt),wrf_lon(wrf_nx,wrf_ny,wrf_nt))
      allocate(wrf_u(wrf_nx,wrf_ny,wrf_nz,wrf_nt),wrf_v(wrf_nx,wrf_ny,wrf_nz,wrf_nt))
! ... mass lat
      tem                = 'XLAT'
      status             = NF_INQ_VARID(ncid,tem(1:4),pid)
      status             = NF_GET_VAR_REAL(ncid,pid,wrf_lat)
      if (debug.ge.1) print*,'bvortex.exe: reading xlat returns:',status
! ... mass long
      tem                = 'XLONG'
      status             = NF_INQ_VARID(ncid,tem(1:5),pid)
      status             = NF_GET_VAR_REAL(ncid,pid,wrf_lon)
      if (debug.ge.1) print*,'bvortex.exe: reading xlong returns:',status     
! ... U wind
      allocate(tem4d(nx1,wrf_ny,wrf_nz,wrf_nt))
      tem                = 'U'
      status             = NF_INQ_VARID(ncid,tem(1:1),pid)
      status             = NF_GET_VAR_REAL(ncid,pid,tem4d)
      wrf_u(1:wrf_nx,:,:,:)  = 0.5*(tem4d(1:wrf_nx,:,:,:)+tem4d(2:nx1,:,:,:))
      if (debug.ge.1) print*,'bvortex.exe: reading wrf_u returns:',status
      deallocate(tem4d)
! ... V wind
      allocate(tem4d(wrf_nx,ny1,wrf_nz,wrf_nt))
      tem                = 'V'
      status             = NF_INQ_VARID(ncid,tem(1:1),pid)
      status             = NF_GET_VAR_REAL(ncid,pid,tem4d)
      wrf_v(:,1:wrf_ny,:,:)  = 0.5*(tem4d(:,1:wrf_ny,:,:)+tem4d(:,2:ny1,:,:))
      if (debug.ge.1) print*,'bvortex.exe: reading wrf_v returns:',status
      deallocate(tem4d)
!
! search for the minimum wind speed around the observed storm center
!
      do j      = 1,ny
       do i     = 1,nx
        if (wrf_lon(i,j,1).le.clon.and.clon.lt.wrf_lon(i+1,j,1).and. &
            wrf_lat(i,j,1).le.clat.and.clat.lt.wrf_lat(i,j+1,1))    then
         ic_o   = i
         jc_o   = j
        endif
       enddo
      enddo
      !ic_o      = int((clon-wrf_lon(1,1,1))*111.e+03/wrf_dx)
      !jc_o      = int((clat-wrf_lat(1,1,1))*111.e+03/wrf_dy)
      print*,'bvortex.exe: observed center: ',wrf_lon(ic_o,jc_o,1),wrf_lat(ic_o,jc_o,1)
      print*,'bvortex.exe: observed center w.r.t wrf grid: ',ic_o,jc_o
      if (ic_o.lt.1.or.ic_o.gt.nx.or.jc_o.lt.1.or.jc_o.gt.ny) then
       print*,'bvortex.exe: has serious error....stop'
       stop
      endif
      sp_min    = 9999
      do j      = max(1,jc_o-5),min(jc_o+5,wrf_ny)
       do i     = max(1,ic_o-5),min(ic_o+5,wrf_nx)
        !sp_tem  = sqrt (                                                                      & 
        !        + (0.25*(wrf_u(i,j,8,1)+wrf_u(i,j,2,1)+wrf_u(i,j,9,1)+wrf_u(i,j,10,1)))**2.   &
        !        + (0.25*(wrf_v(i,j,8,1)+wrf_v(i,j,2,1)+wrf_v(i,j,9,1)+wrf_v(i,j,10,1)))**2. )
        sp_tem  = sqrt (wrf_u(i,j,1,1)**2.+wrf_v(i,j,1,1)**2.)
        print*,i,j,sp_tem
        if (sp_min.gt.sp_tem) then
          sp_min = sp_tem
          ic_g   = i
          jc_g   = j
        endif 
       enddo   
      enddo
!
! re-assigne storm center from obs to GFS center
!     
      print*,'bvortex.exe: observed center lat/lon:',clat,clon
      print*,'bvortex.exe: GFS center wind: ',wrf_u(ic_g,jc_g,1,1),wrf_v(ic_g,jc_g,1,1),sp_min
      print*,'bvortex.exe: GFS center lat lon: ',wrf_lat(ic_g,jc_g,1),wrf_lon(ic_g,jc_g,1)
      if (abs(clat-wrf_lat(ic_g,jc_g,1)).lt.0.5.and.abs(clon-wrf_lon(ic_g,jc_g,1)).lt.0.5) then
       clat       = wrf_lat(ic_g,jc_g,1)
       clon       = wrf_lon(ic_g,jc_g,1)
      endif
!
! finally printout the data for checking/assimilating
!
      open(80,file='bvortex.txt')
      write(80,'(1A7,3I7)')vert_type(1:1),nx,ny,nz
      write(80,'(7A13)')'u |','v |','T |','gh |','lat |','lon |','level'
      ic         = nx/2+1
      jc         = ny/2+1
      do i       = max(1,ic-5*int(rmw/dx)),min(ic+5*int(rmw/dx),nx),2
       do j      = max(1,jc-5*int(rmw/dx)),min(jc+5*int(rmw/dx),ny),2
        do k     = 1,nz
         dlat    = clat + (j-jc)*dy/111e3
         dlon    = clon + (i-ic)*dx/111e3
         !write(80,'(4E13.3,3F13.3)')u(i,j,k),v(i,j,k),t(i,j,k),h(i,j,k),dlat,dlon,z(k) 
         if (k.eq.1.and.vert_type(1:1).eq.'z') then
          write(80,'(4F13.3,3F13.5)')u(i,j,k),v(i,j,k),t(i,j,k),h(i,j,k),dlat,dlon,50.0
         else
          write(80,'(4F13.3,3F13.5)')u(i,j,k),v(i,j,k),t(i,j,k),h(i,j,k),dlat,dlon,z(k)
         endif
        enddo
       enddo
      enddo
      open(90,file='bvortex.dat',form='UNFORMATTED',access='DIRECT',recl=nx*ny)
      do k               = 1,nz
       write(90,rec=irec)((u(i,j,k),i=1,nx),j=1,ny)
       irec              = irec + 1
      enddo
      do k               = 1,nz
       write(90,rec=irec)((v(i,j,k),i=1,nx),j=1,ny)
       irec              = irec + 1
      enddo
      do k               = 1,nz
       write(90,rec=irec)((t(i,j,k),i=1,nx),j=1,ny)
       irec              = irec + 1
      enddo
      do k               = 1,nz
       write(90,rec=irec)((h(i,j,k),i=1,nx),j=1,ny)
       irec              = irec + 1
      enddo
      status  = NF_CLOSE(ncid)
      end
!##############################################################
! SUBROUTINE SECTIONS
!##############################################################
      subroutine vortex3(nx,ny,nz,dx,dy,u,v,h,t,vert_type,vmax,rmw,roci,z)
      implicit none
!
! I/O arguments
!
      integer,intent(in)       :: nx,ny,nz
      real,intent(inout)       :: vmax,roci,rmw
      real,intent(in)          :: dx,dy
      real,intent(out)         :: u(nx,ny,nz),v(nx,ny,nz)
      real,intent(out)         :: h(nx,ny,nz),t(nx,ny,nz)
      real,intent(out)         :: z(nz)
      character*1, intent(in)  :: vert_type
!
! local vars
!
      real             :: Cof(nx,ny),beta(nx,ny),cx(nx,ny),cy(nx,ny)
      real             :: a,b,c,dz
      real             :: zscale,radi,Vmag
      real             :: psi(nx,ny,nz),rhs(nx,ny,nz)
      real             :: rhs2d(nx,ny),phi2d(nx,ny)
      real             :: epsi,tref,pref,mis,g
      real             :: pi,roref,cp,rd
      integer          :: nmax,irec,debug
      integer          :: i,j,k,ic,jc
!
! initailize some parameter
!
      pi         = 4.*atan(1.)
      dz         = 500                       ! grid mesh in z dim [m]
      ic         = nx/2+1                    ! center of storm in grid dom 
      jc         = nx/2+1                    ! center of storm in grid dom
      zscale     = 17000                     ! vertical scale of vortex [m]
      roci       = roci*1e3
      rmw        = 1.8*rmw*1e3               ! convert to [m]
!
! start to construct wind profile first
!
      a          = vmax/rmw                  ! Rankine coefficient
      c          = log(10/vmax)/log(rmw/roci)! Rankine coefficient
      b          = a*(rmw**(c+1))            ! Rankine coefficient
      do k       = 1,nz
       do i      = 1,nx
        do j     = 1,ny
         radi    = sqrt((i-ic)**2.+(j-jc)**2.)*dx
         if (radi.gt.0) then
          Vmag   = ((((vmax*radi/rmw)**2.)*((2*rmw/(radi+rmw))**3. - (2*rmw/(roci+rmw))**3.) & 
                    + (5e-4*radi*0.5)**2.)**0.5 - 5e-4*radi*0.5)                             &
                 * cos(pi*k/(nz*2))*exp(-(k*dz/zscale)**4)
         else
          Vmag   = 0
         endif
         Vmag    = max(Vmag,0.)
         if (radi.gt.0) then
          u(i,j,k) = -Vmag*(j-jc)*dx/radi
          v(i,j,k) =  Vmag*(i-ic)*dx/radi
         else
          u(i,j,k) = 0.
          v(i,j,k) = 0.
         endif
        enddo
       enddo
       if (vert_type(1:1).eq.'z') then
        z(k)      = (k-1)*dz
       elseif (vert_type(1:1).eq.'s') then
        z(k)      = float(k)
       else
        print*,'Vortex type II does not support vertical coordinate: ',vert_type(1:1)
       endif
      enddo
!
! use the gradient wind balance to compute geopotential
! pertrubation now by computing streamfunction first,
! then invert for phi. Note that this is geopotential
! (gz), not geopotential height
!
      irec         = 1
      pi           = 4.*atan(1.)
      g            = 9.81
      roref        = 1.
      pref         = 1.01e+05
      tref         = 300.
      cp           = 1004.
      rd           = 287.
      mis          = -99999
      Cof          = 1.e-5        ! Coriolis forcing
      beta         = 0.           ! beta factor
      cx           = 1            ! x-coefficient of Laplacian
      cy           = 1            ! y-coefficient of Laplacian
      nmax         = 5E4          ! iteration maximum for relaxation
      epsi         = 0.01         ! threshold for relaxation
      call xsor(psi,u,v,nx,ny,nz,dx,dy)
      call forcing_psi(rhs,psi,nx,ny,nz,dx,dy,dz,Cof,beta)
      do k         = 1,nz
       print*,'iteration at lev ',k
       rhs2d(:,:)  = rhs(:,:,k) 
       phi2d       = 0
       call p2d_dd_lipman(phi2d,nx,ny,dx,dy,cx,cy,rhs2d,nmax,epsi,debug)
       h(:,:,k)    = phi2d(:,:)
      enddo
!
! compute pertrubation potential temperature now
!
      call cal_pt1(h,t,tref,nx,ny,nz,dz)
      return
      end subroutine vortex3

      subroutine vortex2(nx,ny,nz,dx,dy,u,v,h,t,vert_type,vmax,rmw,roci,z)
      implicit none
!
! I/O arguments
!
      integer,intent(in)       :: nx,ny,nz
      real,intent(inout)       :: vmax,roci,rmw
      real,intent(in)          :: dx,dy
      real,intent(out)         :: u(nx,ny,nz),v(nx,ny,nz)
      real,intent(out)         :: h(nx,ny,nz),t(nx,ny,nz)
      real,intent(out)         :: z(nz)
      character*1, intent(in)  :: vert_type
!
! local vars
!
      real             :: Cof(nx,ny),beta(nx,ny),cx(nx,ny),cy(nx,ny)
      real             :: a,b,c,dz
      real             :: zscale,radi,Vmag
      real             :: psi(nx,ny,nz),rhs(nx,ny,nz)
      real             :: rhs2d(nx,ny),phi2d(nx,ny)
      real             :: epsi,tref,pref,mis,g
      real             :: pi,roref,cp,rd
      integer          :: nmax,irec,debug
      integer          :: i,j,k,ic,jc
!
! initailize some parameter
!
      pi         = 4.*atan(1.)
      dz         = 500                       ! grid mesh in z dim [m]
      ic         = nx/2+1                    ! center of storm in grid dom 
      jc         = nx/2+1                    ! center of storm in grid dom
      zscale     = 17000                     ! vertical scale of vortex [m]
      roci       = roci*1e3                  ! convert to [m]
      rmw        = 1.8*rmw*1e3               ! convert [nm] to [m]
!
! start to construct wind profile first
!
      a          = vmax/rmw              ! Rankine coefficient
      c          = log(10/vmax)/log(rmw/roci)! Rankine coefficient
      b          = a*(rmw**(c+1))            ! Rankine coefficient
      do k       = 1,nz
       do i      = 1,nx
        do j     = 1,ny
         radi    = sqrt((i-ic)**2.+(j-jc)**2.)*dx
         if (radi.gt.0.and.radi.le.rmw) then
          Vmag   = (a*radi)*cos(pi*k/(nz*2))*exp(-(k*dz/zscale)**4)
         elseif (radi.gt.rmw) then
          Vmag   = (b/(radi**c))*cos(pi*k/(nz*2))*exp(-(k*dz/zscale)**4)
         else
          Vmag   = 0
         endif
         Vmag    = max(Vmag,0.)*exp(-(radi/5/rmw)**2.)
         if (radi.gt.0) then
          u(i,j,k) = -Vmag*(j-jc)*dx/radi
          v(i,j,k) =  Vmag*(i-ic)*dx/radi
         else
          u(i,j,k) = 0.
          v(i,j,k) = 0.
         endif
        enddo
       enddo
       if (vert_type(1:1).eq.'z') then
        z(k)      = (k-1)*dz
       elseif (vert_type(1:1).eq.'s') then
        z(k)      = float(k)
       else
        print*,'Vortex type II does not support vertical coordinate: ',vert_type(1:1)
       endif
      enddo
!
! use the gradient wind balance to compute geopotential
! pertrubation now by computing streamfunction first,
! then invert for phi. Note that this is geopotential
! (gz), not geopotential height
!
      irec         = 1
      pi           = 4.*atan(1.)
      g            = 9.81
      roref        = 1.
      pref         = 1.01e+05
      tref         = 300.
      cp           = 1004.
      rd           = 287.
      mis          = -99999
      Cof          = 1.e-5        ! Coriolis forcing
      beta         = 0.           ! beta factor
      cx           = 1            ! x-coefficient of Laplacian
      cy           = 1            ! y-coefficient of Laplacian
      nmax         = 5E4          ! iteration maximum for relaxation
      epsi         = 0.01         ! threshold for relaxation
      call xsor(psi,u,v,nx,ny,nz,dx,dy)
      call forcing_psi(rhs,psi,nx,ny,nz,dx,dy,dz,Cof,beta)
      do k         = 1,nz
       print*,'iteration at lev ',k
       rhs2d(:,:)  = rhs(:,:,k) 
       phi2d       = 0
       call p2d_dd_lipman(phi2d,nx,ny,dx,dy,cx,cy,rhs2d,nmax,epsi,debug)
       h(:,:,k)    = phi2d(:,:)
      enddo
!
! compute pertrubation potential temperature now
!
      call cal_pt1(h,t,tref,nx,ny,nz,dz)
      return
      end subroutine vortex2

      subroutine vortex1(nx,ny,nz,dx,dy,u,v,h,t,vert_type,vmax,rmw,roci,z)
      implicit none
!
! I/O arguments
!
      integer,intent(in)       :: nx,ny,nz
      real,intent(inout)       :: vmax,roci,rmw
      real,intent(in)          :: dx,dy
      real,intent(out)         :: u(nx,ny,nz),v(nx,ny,nz)
      real,intent(out)         :: h(nx,ny,nz),t(nx,ny,nz)
      real,intent(out)         :: z(nz)
      character*1, intent(in)  :: vert_type
!
! local vars
!
      real             :: Cof(nx,ny),beta(nx,ny),cx(nx,ny),cy(nx,ny)
      real             :: a,b,c,dz
      real             :: zscale,radi,Vmag
      real             :: psi(nx,ny,nz),rhs(nx,ny,nz)
      real             :: rhs2d(nx,ny),phi2d(nx,ny)
      real             :: epsi,tref,pref,mis,g
      real             :: pi,roref,cp,rd
      integer          :: nmax,irec,debug
      integer          :: i,j,k,ic,jc
!
! initailize some parameter
!
      dz         = 500                       ! grid mesh in z dim [m]
      ic         = nx/2+1                    ! center of storm in grid dom 
      jc         = nx/2+1                    ! center of storm in grid dom
      zscale     = 17000                     ! vertical scale of vortex [m]
      roci       = roci*1e3
      rmw        = 1.8*rmw*1e3               ! convert to [m]
!
! start to construct wind profile first
!
      a          = vmax/rmw                  ! Rankine coefficient
      c          = log(10/vmax)/log(rmw/roci)! Rankine coefficient
      b          = a*(rmw**(c+1))            ! Rankine coefficient
      do k       = 1,nz
       do i      = 1,nx
        do j     = 1,ny
         radi    = sqrt((i-ic)**2.+(j-jc)**2.)*dx
         if (radi.gt.0.and.radi.le.rmw) then
          Vmag   = a*radi*(nz-k)*dz/zscale
         elseif (radi.gt.rmw) then
          Vmag   = b*(nz-k)*dz/zscale/(radi**c)
         else
          Vmag   = 0
         endif
         Vmag    = max(Vmag,0.)
         if (radi.gt.0) then
          u(i,j,k) = -Vmag*(j-jc)*dx/radi
          v(i,j,k) =  Vmag*(i-ic)*dx/radi
         else
          u(i,j,k) = 0.
          v(i,j,k) = 0.
         endif
        enddo
       enddo
       if (vert_type(1:1).eq.'z') then
        z(k)      = (k-1)*dz
       elseif (vert_type(1:1).eq.'s') then
        z(k)      = float(k)
       else
        print*,'Vortex type II does not support vertical coordinate: ',vert_type(1:1)
       endif
      enddo
!
! use the gradient wind balance to compute geopotential
! pertrubation now by computing streamfunction first,
! then invert for phi. Note that this is geopotential
! (gz), not geopotential height
!
      irec         = 1
      pi           = 4.*atan(1.)
      g            = 9.81
      roref        = 1.
      pref         = 1.01e+05
      tref         = 300.
      cp           = 1004.
      rd           = 287.
      mis          = -99999.
      Cof          = 1.e-5        ! Coriolis forcing
      beta         = 0.           ! beta factor
      cx           = 1            ! x-coefficient of Laplacian
      cy           = 1            ! y-coefficient of Laplacian
      nmax         = 5E4          ! iteration maximum for relaxation
      epsi         = 0.01         ! threshold for relaxation
      call xsor(psi,u,v,nx,ny,nz,dx,dy)
      call forcing_psi(rhs,psi,nx,ny,nz,dx,dy,dz,Cof,beta)
      do k         = 1,nz
       print*,'iteration at lev ',k
       rhs2d(:,:)  = rhs(:,:,k) 
       phi2d       = 0
       call p2d_dd_lipman(phi2d,nx,ny,dx,dy,cx,cy,rhs2d,nmax,epsi,debug)
       h(:,:,k)    = phi2d(:,:)
      enddo
!
! compute pertrubation potential temperature now
!
      call cal_pt1(h,t,tref,nx,ny,nz,dz)
      return
      end subroutine vortex1
!
      subroutine tcvital(pmin,penv,vmax,clat,clon,r34,roci,rmw)
      implicit none
      real          :: pmin,penv,vmax,clat,clon,r34,roci,rmw
      character*20  :: value(30),clat_flag,clon_flag
      integer       :: i,j,k
!
! open for reading data
!
      open(10,file='tmpvit',status='old')
      read(10,*)(value(i),i=1,19)
      do i       = 1,19
       print*,i,value(i)(1:10)
      enddo 
!
! read center lat
!
      i          = len_trim(value(6))
      if (i.eq.4) then
       read(value(6)(1:3),'(1F3.1)')clat
       read(value(6)(4:4),'(1A1)')clat_flag
      else
       read(value(6)(1:4),'(1F4.1)')clat
       read(value(6)(5:5),'(1A1)')clat_flag
      endif
      if (clat_flag.eq.'S') clat= -1*clat
!
! read center lon
!
      i          = len_trim(value(7))
      if (i.eq.4) then
       read(value(7)(1:3),'(1F3.1)')clon
       read(value(7)(4:4),'(1A1)')clon_flag
      else
       read(value(7)(1:4),'(1F4.1)')clon
       read(value(7)(5:5),'(1A1)')clon_flag
      endif
      if (clon_flag.eq.'W') clon= -1*clon
!
! read pmin/penv/rmw/vmax/roci
!
      i          = len_trim(value(10))
      read(value(10)(1:i),'(1F4.0)')pmin
      i          = len_trim(value(11))
      read(value(11)(1:i),'(1F4.0)')penv
      i          = len_trim(value(12))
      read(value(12)(1:i),'(1F4.0)')roci
      i          = len_trim(value(13))
      read(value(13)(1:i),'(1F2.0)')vmax
      i          = len_trim(value(14))
      read(value(14)(1:i),'(1F3.0)')rmw
!
! set r34 = 0 
!
      r34        = 0
      close(10)
      return
      end subroutine tcvital


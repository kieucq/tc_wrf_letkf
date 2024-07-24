!
! [NOTE]:      This program is to convert the WRF 3DVAR output from 
!              obsproc program to the fomat that the LETKF code needs
!
! [HISTORY]: - Jul 07, 2010: created
!            - Nov 06, 2010: fix allocation array of p1(nlevel) for
!              ifort running
!            - Mar 18, 2010: add vertical truncate level for too high
!              sounding, which could cause program to die due to 
!              very large cfl at the upper level.
!
! [AUTHOR]:    Chanh Q. Kieu 
!              Dept of atmospheric and oceanic science
!              Vietnam Nationaal University
!              Email: kieucq@atmos.umd.edu
!
! [REFERENCE]: Kieu, C. Q. (2005): arxiv.org
!
! [COPYRIGHT]: (C) 2010
!
!===========================================================================
!
      PROGRAM wrf3dvar_conversion_to_letkf
      IMPLICIT none
      INCLUDE 'netcdf.inc' 
!
! netcdf header information
!
      INTEGER, PARAMETER  :: n=4,nmax=10000,nlevel=1000
      INTEGER             :: ncid,status,nrs
      INTEGER             :: ndims,nvars,ngatts,unlimdimid
      INTEGER             :: uid,vid,tid,rid,pid
      INTEGER             :: tndim,tdim(n)
      REAL                :: dx,dy
!
! station information
!
      REAL                :: latrs(nmax),lonrs(nmax)
      INTEGER             :: idrs(nmax),ilat,ilon
      REAL                :: tlat,tlon,tlev
      REAL,ALLOCATABLE    :: latrs1(:),lonrs1(:)
      INTEGER,ALLOCATABLE :: idrs1(:)
!
! local vars
!
      INTEGER             :: nx,ny,nz,nt,nx1,ny1,nz1,nt1
      INTEGER             :: iday,ihour,imin,fid,nl,nobs 
      REAL                :: umax,vmax,tmax,rmax  
      REAL                :: rtime,tfcst,restart,ptop,pi
      REAL                :: w1,w2,uu,vv,qq,tt,zz,tem1
      REAL                :: uuc,vvc,ttc,qqc,zzc
      REAL,ALLOCATABLE    :: latin(:,:,:),lonin(:,:,:),eta(:,:,:)
      REAL,ALLOCATABLE    :: p(:,:,:,:),pb(:,:,:,:),psfc(:,:,:)
      REAL,ALLOCATABLE    :: p1(:),q1(:),sp(:),di(:),th(:),t1(:)
      REAL,ALLOCATABLE    :: u1(:),v1(:),td(:),rh(:),z1(:),t1c(:)
      REAL,ALLOCATABLE    :: p1c(:),q1c(:),spc(:),dic(:),thc(:)
      REAL,ALLOCATABLE    :: u1c(:),v1c(:),tdc(:),rhc(:),z1c(:)
      REAL,ALLOCATABLE    :: tem3d(:,:,:,:),tem1d(:,:)
      CHARACTER*100       :: name,ofile,wfile,tem
      INTEGER             :: i,j,k,tem_len,debug,ne,nfile,ifile,irec
      INTEGER             :: ntruncate
      LOGICAL             :: check_data1,check_data2
      irec               = 1 
      ntruncate          = 7
!
! reading background map information for later interpolation
!
      ofile              = 'wrfinput_d01'
      if (debug.eq.1) print*,' stationid.exe: opened file is: ',ofile(1:30)
      status = NF_OPEN(ofile(1:len_trim(ofile)),nf_write,ncid)
      if (debug.ge.1) then
       print*,''
       print*,'1. NF_OPEN and return value: '
       print*,'status     =',status
       print*,'nf_noerr   =',nf_noerr 
       print*,'ncid       =',ncid
      endif
!
! probing some dimension and structure of data fields
!
      status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
      print*,''
      print*,'2. NF_INQ returns values: '
      print*,'ncid       =',ncid                             ! file id
      print*,'ndims      =',ndims                            ! number of dims
      print*,'nvars      =',nvars                            ! number of varibales
      print*,'ngatts     =',ngatts                           ! number of globale attributes
      print*,'unlimdimid =',unlimdimid                       ! number of umlimitted dim
!
! pull our varid and dim information for a temporary variable
! which will be used later.  
!
      tem                = 'T'
      tem_len            = len_trim(tem)
      print*,'Checking var is: ',tem(1:tem_len)
      status = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
      status = NF_INQ_VARNDIMS(ncid,tid,tndim)
      status = NF_INQ_VARDIMID(ncid,tid,tdim)
      if (tndim.ne.n) THEN
       print*,'Number of dim is /= n...allocation stop'
       STOP
      endif
      print*,''
      print*,'3. Pull out information about: ',tem(1:tem_len)
      print*,'tid        =',tid
      print*,'tndim      =',tndim
      do i               = 1,tndim
       j                 = tdim(i)
       status            = NF_INQ_DIMNAME(ncid,j,name)
       status            = NF_INQ_DIMLEN(ncid,j,k)
       if (i.eq.1) nx    = k
       if (i.eq.2) ny    = k
       if (i.eq.3) nz    = k
       if (i.eq.4) nt    = k
       print*,'Dimension name is:',i,tdim(i),name(1:20)
      enddo
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DX',dx)
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DY',dy)
      print*,'nx         =',nx
      print*,'ny         =',ny
      print*,'nz         =',nz
      print*,'nt         =',nt
      print*,'dx         =',dx
      print*,'dy         =',dy
      if (debug.eq.1) READ*
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
      nx1          = nx + 1
      ny1          = ny + 1
      nz1          = nz + 1
      allocate(latin(nx,ny,nt),lonin(nx,ny,nt),eta(nx,ny,nz))
      allocate(p(nx,ny,nz,nt),pb(nx,ny,nz,nt),psfc(nx,ny,nt))   
!
! reading data from the background wrfinput to define the local mesh
!
      tem          = 'XLAT'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(ncid,uid,latin)
      tem          = 'XLONG'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(ncid,vid,lonin)
      tem          = 'P'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,p)
      tem          = 'PB'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_GET_VAR_REAL(ncid,rid,pb)
      p            = p + pb
      tem          = 'PSFC'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,psfc)
      tem          = 'P_TOP'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,ptop)
      do k         = 1,nz
       do i        = 1,nx
        do j       = 1,ny
         eta(i,j,k)= (p(i,j,k,1)-ptop)/(psfc(i,j,1)-ptop) 
        enddo
       enddo
      enddo
!
! do a first loop of all data to see how many obs we can aggregate
!
      open(90,file='obsproc.dat',status='old')
      nobs         = 0
      i            = 1
10    read(90,*,end=11)name(1:10)
      if (name(1:1).eq.'#') goto 12
      i            = i + 1
      goto 10
11    print*,'wrf3dvar_to_letkf.exe: reading obsproc.dat errors...stop'
      stop
12    print*,'wrf3dvar_to_letkf.exe: header information ends at',i
14    continue
      i            = 1
      read(90,'(1A74,I6,F12.3,F23.3)',end=15)name,nl,latrs(i),lonrs(i)
      read(90,*)
      allocate(di(nl),dic(nl),z1(nl),z1c(nl),th(nl),thc(nl))
      allocate(p1(nl),p1c(nl),q1(nl),q1c(nl),sp(nl),spc(nl))
      allocate(u1(nl),u1c(nl),v1(nl),v1c(nl),td(nl),tdc(nl))
      allocate(rh(nl),rhc(nl),t1(nl),t1c(nl))
      do k         = 1,nl
       read(90,*)p1(k),tem1,p1c(k),sp(k),tem1,spc(k),di(k),tem1,dic(k),     &
                 z1(k),tem1,z1c(k),t1(k),tem1,t1c(k),td(k),tem1,tdc(k),     &
                 rh(k),tem1,rhc(k)
      enddo
      tlat         = (latrs(i)-latin(1,1,1))*111.e3/dx
      tlon         = (lonrs(i)-lonin(1,1,1))*111.e3/dy
      ilat         = nint(tlat)
      ilon         = nint(tlon)
      if (ilat.lt.1.or.ilat.gt.ny.or.ilon.lt.1.or.ilon.gt.nx) goto 16  
      do j         = 1,nl
       do k        = 1,nz-ntruncate
        check_data1 = p1(j).ne.-888888.000.and.p1c(j).ne.-888888.000.and.     &
                      sp(j).ne.-888888.000.and.spc(j).ne.-888888.000.and.     &
                      di(j).ne.-888888.000.and.dic(j).ne.-888888.000.and.     &
                      z1(j).ne.-888888.000.and.z1c(j).ne.-888888.000.and.     &
                      t1(j).ne.-888888.000.and.t1c(j).ne.-888888.000.and.     &
                      td(j).ne.-888888.000.and.tdc(j).ne.-888888.000.and.     &
                      rh(j).ne.-888888.000.and.rhc(j).ne.-888888.000.and.     &
                      p1(j).le.p(ilon,ilat,k,1).and.                          &
                      p1(j).gt.p(ilon,ilat,k+1,1)
        check_data2 = p1(j+1).ne.-888888.000.and.p1c(j+1).ne.-888888.000.and. &
                      sp(j+1).ne.-888888.000.and.spc(j+1).ne.-888888.000.and. &
                      di(j+1).ne.-888888.000.and.dic(j+1).ne.-888888.000.and. &
                      z1(j+1).ne.-888888.000.and.z1c(j+1).ne.-888888.000.and. &
                      t1(j+1).ne.-888888.000.and.t1c(j+1).ne.-888888.000.and. &
                      td(j+1).ne.-888888.000.and.tdc(j+1).ne.-888888.000.and. &
                      rh(j+1).ne.-888888.000.and.rhc(j+1).ne.-888888.000   
        if (check_data1) THEN
         nobs      = nobs + 1           
         goto 18
        endif
       enddo
18     continue
      enddo
16    continue      
      deallocate(u1,u1c,v1,v1c,p1,p1c,di,dic,sp,spc,td,tdc,q1,q1c,z1,z1c)
      deallocate(th,thc,t1,t1c,rh,rhc)
      goto 14
15    print*,'wrf3dvar_to_letkf.exe: total number of observation is',nobs
      close(90)
!
! reading the header information from the obsproc output
!
      open(90,file='obsproc.dat',status='old')
      open(11,file='obsout.dat')
      write(11,*)nobs,' lon | lat | lev | u qcu | v qcv | t qct | &
                        qv qcqv | ph qcph |'
30    read(90,*,end=31)name(1:10)
      if (name(1:1).eq.'#') goto 32
      goto 30
31    stop
32    continue
      pi           = 4.*atan(1.)
      i            = 1
34    continue
      read(90,'(1A74,I6,F12.3,F23.3)',end=35)name,nl,latrs(i),lonrs(i)
      read(90,*)
      allocate(di(nl),dic(nl),z1(nl),z1c(nl),th(nl),thc(nl))
      allocate(p1(nl),p1c(nl),q1(nl),q1c(nl),sp(nl),spc(nl))
      allocate(u1(nl),u1c(nl),v1(nl),v1c(nl),td(nl),tdc(nl))
      allocate(rh(nl),rhc(nl),t1(nl),t1c(nl))
      do k         = 1,nl
       read(90,*)p1(k),tem1,p1c(k),sp(k),tem1,spc(k),di(k),tem1,dic(k),        &
                 z1(k),tem1,z1c(k),t1(k),tem1,t1c(k),td(k),tem1,tdc(k),        &
                 rh(k),tem1,rhc(k)
       u1(k)       =  sp(k)*cos(pi/2.+di(k)*pi/180.)/2.
       v1(k)       = -sp(k)*sin(pi/2.+di(k)*pi/180.)/2.
       call td_to_qv(p1(k),t1(k),td(k),q1(k))
       call tk_to_theta(p1(k),t1(k),th(k))
       u1c(k)      = spc(k)
       v1c(k)      = spc(k)
       thc(k)      = t1c(k)*th(k)/t1(k)
       q1c(k)      = q1(k)*rhc(k)/rh(k)
       print*,k,sp(k),di(k),u1(k)
      enddo
      tlat         = (latrs(i)-latin(1,1,1))*111.e3/dx
      tlon         = (lonrs(i)-lonin(1,1,1))*111.e3/dy
      ilat         = nint(tlat)
      ilon         = nint(tlon)
      if (ilat.lt.1.or.ilat.gt.ny.or.ilon.lt.1.or.ilon.gt.nx) goto 36
      do j         = 1,nl
       do k        = 1,nz-ntruncate
        check_data1 = p1(j).ne.-888888.000.and.p1c(j).ne.-888888.000.and.     &
                      sp(j).ne.-888888.000.and.spc(j).ne.-888888.000.and.     &
                      di(j).ne.-888888.000.and.dic(j).ne.-888888.000.and.     &
                      z1(j).ne.-888888.000.and.z1c(j).ne.-888888.000.and.     &
                      t1(j).ne.-888888.000.and.t1c(j).ne.-888888.000.and.     &
                      td(j).ne.-888888.000.and.tdc(j).ne.-888888.000.and.     &
                      rh(j).ne.-888888.000.and.rhc(j).ne.-888888.000.and.     &
                      p1(j).le.p(ilon,ilat,k,1).and.                          &
                      p1(j).gt.p(ilon,ilat,k+1,1)
        check_data2 = p1(j+1).ne.-888888.000.and.p1c(j+1).ne.-888888.000.and. &
                      sp(j+1).ne.-888888.000.and.spc(j+1).ne.-888888.000.and. &
                      di(j+1).ne.-888888.000.and.dic(j+1).ne.-888888.000.and. &
                      z1(j+1).ne.-888888.000.and.z1c(j+1).ne.-888888.000.and. &
                      t1(j+1).ne.-888888.000.and.t1c(j+1).ne.-888888.000.and. &
                      td(j+1).ne.-888888.000.and.tdc(j+1).ne.-888888.000.and. &
                      rh(j+1).ne.-888888.000.and.rhc(j+1).ne.-888888.000 
        if (check_data1) then
         w1        = p(ilon,ilat,k,1)-p1(j)
         w2        = p(ilon,ilat,k,1)-p(ilon,ilat,k+1,1)
         uu        = u1(j)
         vv        = v1(j)
         qq        = q1(j)
         tt        = th(j)
         zz        = 10.
         uuc       = u1c(j)
         vvc       = v1c(j)
         qqc       = q1c(j)
         ttc       = thc(j)
         zzc       = 1.         
         tlev      = float(k) + w1/w2
         write(11,'(3F12.4,10E12.4)')tlon,tlat,tlev,uu,uuc,vv,vvc,  &
                                         tt,ttc,qq,qqc,zz,zzc
         print*,i,latrs(i),lonrs(i),w1,w2,p1(j),p(ilon,ilat,k,1),p1(j+1)
         print*,tlon,tlat,tlev,uu,vv,tt,qq,p1(j),p(ilon,ilat,k,1),p1(j+1)
         goto 38
        endif
       enddo
38     continue
      enddo
      goto 37
36    print*,'wrf3dvar_to_letkf.exe: station out of domain..skip'      
37    i             = i + 1
      deallocate(u1,u1c,v1,v1c,p1,p1c,di,dic,sp,spc,td,tdc,q1,q1c,z1,z1c)
      deallocate(th,thc,t1,t1c,rh,rhc)
      goto 34
35    print*,'wrf3dvar_to_letkf.exe: done conversion'
!
! close the data file and loop all files
!
      status       = NF_CLOSE(ncid)
      if (status.ne.0) print*,'Can not close the new file'
      END
      
      subroutine td_to_qv(p,t,td,q)
      real p,t,td,q,w,e,tc
!
! compute vapor pressure (hPa or mb)
!     
      tc           = td-273.15
      e            = 6.11*(10**((7.5*tc)/(237.7+tc)))
!
! compute q (kg/kg)
!      
      q            = 621.97*e*1e-3/(p/100.-e)
      return
      end

      subroutine tk_to_theta(p,t,th)
      real p,t,th,R,Cp,pref,tref
      R            = 286.5517
      Cp           = 1003.5
      pref         = 1000.
      tref         = 300.
      th           = t*(100000./p)**(R/Cp)
      return
      end
    

      subroutine interp_1d( a, xa, na, b, xb, nb, vertical_type, missing_value)
      implicit none
      integer, intent(in)              :: na, nb
      real, intent(in), dimension(na)  :: a, xa
      real, intent(in), dimension(nb)  :: xb
      real, intent(out), dimension(nb) :: b
      real, intent(in)                 :: missing_value

      integer                          :: n_in, n_out
      logical                          :: interp
      real                             :: w1, w2
      character (len=1)                :: vertical_type


      if ( vertical_type == 'p' ) then

      do n_out = 1, nb

        b(n_out) = missing_value
        interp = .false.
        n_in = 1

        do while ( (.not.interp) .and. (n_in < na) )

          if( (xa(n_in)   >= xb(n_out)) .and. &
              (xa(n_in+1) <= xb(n_out))        ) then
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

          if( (xa(n_in)   <= xb(n_out)) .and. &
              (xa(n_in+1) >= xb(n_out))        ) then
            interp = .true.
            w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
            w2 = 1. - w1
            b(n_out) = w1*a(n_in) + w2*a(n_in+1)
          end if
          n_in = n_in +1

        enddo

        if (n_out.eq.1.and.(.not.interp)) then
!         b(1)  = a(1)  !+ (xb(1)-xa(1))*(a(2)-a(1))/(xa(2)-xa(1))
        endif 

        if (n_out.eq.nb.and.(.not.interp)) then
!         b(nb) = a(na) !+ (xb(nb)-xa(na))*(a(na)-a(na-1))/(xa(na)-xa(na-1))
        endif 

      enddo
  
      endif

      end subroutine interp_1d

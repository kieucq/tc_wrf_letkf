!
! [NOTE]:      This program is to read all radiosonde station data and 
!              convert it into he fomat that the LETKF code needs
!
! [HISTORY]: - Created on May 25, 2010
!            - Updated on Jun 26, 2010: add obs quality control into the 
!                                       obsout.dat to allow for the variation
!                                       of obs_error with height as in WRFDA  
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
      PROGRAM radionsode_conversion
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
      REAL,ALLOCATABLE    :: latrs1(:),lonrs1(:)
      INTEGER,ALLOCATABLE :: idrs1(:)
      REAL                :: serr_u,serr_v,serr_t
      REAL                :: serr_q,serr_p
      REAL,ALLOCATABLE    :: err_u(:),err_v(:),err_t(:)
      REAL,ALLOCATABLE    :: err_q(:),err_p(:)   
!
! local vars
!
      INTEGER             :: nx,ny,nz,nt,nx1,ny1,nz1,nt1
      INTEGER             :: iday,ihour,imin,fid,nl,nobs 
      REAL                :: umax,vmax,tmax,rmax  
      REAL                :: rtime,tfcst,restart,ptop,t1,pi
      REAL                :: w1,w2,uu,vv,qq,tt
      REAL,ALLOCATABLE    :: latin(:,:,:),lonin(:,:,:),eta(:,:,:)
      REAL,ALLOCATABLE    :: p(:,:,:,:),pb(:,:,:,:),psfc(:,:,:)
      REAL,ALLOCATABLE    :: p1(:),q1(:),sp(:),di(:),theta(:)
      REAL,ALLOCATABLE    :: u1(:),v1(:)
      REAL,ALLOCATABLE    :: tem3d(:,:,:,:),tem1d(:,:)
      CHARACTER*100       :: name,ofile,tem,wfile
      INTEGER             :: i,j,k,tem_len,debug,ne,nfile,ifile,irec
      irec               = 1 
!
! reading the surface obs error first from the shared namelist 
!
      CALL input_namelist(debug,serr_u,serr_v,serr_t,serr_q,serr_p)
      PRINT*,'obs.exe: obs_error_u is: ',serr_u
      PRINT*,'obs.exe: obs_error_v is: ',serr_v
      PRINT*,'obs.exe: obs_error_t is: ',serr_t
      PRINT*,'obs.exe: obs_error_q is: ',serr_q
      PRINT*,'obs.exe: obs_error_q is: ',serr_p      
      IF (debug.eq.1) READ*
!
! reading station id now
!
      OPEN(11,file='stationid.txt',status='old')
      READ(11,*)
      i                  = 1
12    READ(11,*,end=13)idrs(i),latrs(i),lonrs(i)
      PRINT*,i,idrs(i),latrs(i),lonrs(i)
      i                  = i + 1
      GOTO 12
13    CONTINUE
      CLOSE(11)
      nrs                = i - 1
      PRINT*,'Number of stations from the namelist is',nrs  
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
      DO i               = 1,tndim
       j                 = tdim(i)
       status            = NF_INQ_DIMNAME(ncid,j,name)
       status            = NF_INQ_DIMLEN(ncid,j,k)
       if (i.eq.1) nx    = k
       if (i.eq.2) ny    = k
       if (i.eq.3) nz    = k
       if (i.eq.4) nt    = k
       print*,'Dimension name is:',i,tdim(i),name(1:20)
      ENDDO
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DX',dx)
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DY',dy)
      print*,'nx         =',nx
      print*,'ny         =',ny
      print*,'nz         =',nz
      print*,'nt         =',nt
      print*,'dx         =',dx
      print*,'dy         =',dy
      IF (debug.eq.1) READ*
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
      nx1          = nx + 1
      ny1          = ny + 1
      nz1          = nz + 1
      ALLOCATE(latin(nx,ny,nt),lonin(nx,ny,nt),eta(nx,ny,nz))
      ALLOCATE(p(nx,ny,nz,nt),pb(nx,ny,nz,nt),psfc(nx,ny,nt))   
      ALLOCATE(err_u(nz),err_v(nz),err_t(nz),err_q(nz),err_p(nz))      
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
      DO k         = 1,nz
       DO i        = 1,nx
        DO j       = 1,ny
         eta(i,j,k)= (p(i,j,k,1)-ptop)/(psfc(i,j,1)-ptop) 
        ENDDO
       ENDDO
      ENDDO
      DO k         = 1,nz  
       PRINT*,k,eta(1,1,k),eta(10,10,k),eta(20,20,k),eta(30,30,k)
      ENDDO
      p            = p/100.
!
! do a first loop of all data to see how many obs we can aggregate
!
      OPEN(11,file='obsout.dat')
      ALLOCATE(p1(nlevel),q1(nlevel),sp(nlevel),di(nlevel))
      ALLOCATE(u1(nlevel),v1(nlevel),theta(nlevel))
      wfile        ='00000.dat'
      i            = 1
      nobs         = 0
14    fid          = idrs(i)
      write(wfile(1:5),'(I5)')fid
      PRINT*,'Input station data is: ',i,wfile(1:len_trim(wfile))
      OPEN(12,file=wfile(1:len_trim(wfile)),status='old')
      READ(12,*)
      READ(12,*)
      READ(12,*)
      READ(12,*)
      READ(12,*)
      j            = 1
15    READ(12,*,end=16)p1(j)
      j            = j + 1
      GOTO 15
16    CLOSE(12)
      nl           = j - 1
      ilat         = nint((latrs(i)-latin(1,1,1))*111.e3/dx)
      ilon         = nint((lonrs(i)-lonin(1,1,1))*111.e3/dy)
      IF (ilat.lt.1.or.ilat.gt.ny.or.ilon.lt.1.or.ilon.gt.nx) GOTO 23
      DO k         = 1,nz
       DO j        = 1,nl-1
        IF (p1(j).gt.p(ilon,ilat,k,1).and.p(ilon,ilat,k,1).gt.p1(j+1)) THEN
         nobs      = nobs + 1
         GOTO 22
        ENDIF
       ENDDO
22     CONTINUE
      ENDDO
23    i            = i + 1
      PRINT*,'Number of level is ',k,nobs
      IF (i.le.nrs) GOTO 14
      WRITE(11,*)nobs,'   lon    lat    lev    u    v    t    qv    ph'
!
! looping over all stations finally and stag data into a single file
! obsout.dat
!
      pi           = 4.*atan(1.)
      i            = 1
17    fid          = idrs(i)
      write(wfile(1:5),'(I5)')fid
      PRINT*,'Input station data is: ',i,wfile(1:len_trim(wfile))
      OPEN(12,file=wfile(1:len_trim(wfile)),status='old')     
      READ(12,*)
      READ(12,*)
      READ(12,*)
      READ(12,*)
      READ(12,*)
      j            = 1
18    READ(12,*,end=19)p1(j),t1,t1,t1,t1,q1(j),di(j),sp(j),theta(j)
      u1(j)        = sp(j)*cos(pi/2.+di(j)*pi/180.)/2.
      v1(j)        = -sp(j)*sin(pi/2.+di(j)*pi/180.)/2.
      PRINT*,j,p1(j),q1(j),sp(j),di(j),theta(j),u1(j),v1(j)
      j            = j + 1
      GOTO 18
19    CONTINUE
      CLOSE(12)
      nl           = j - 1
      ilat         = nint((latrs(i)-latin(1,1,1))*111.e3/dx)
      ilon         = nint((lonrs(i)-lonin(1,1,1))*111.e3/dy)
      IF (ilat.lt.1.or.ilat.gt.ny.or.ilon.lt.1.or.ilon.gt.nx) GOTO 20
!     ilat         = 20
!     ilon         = 30
      DO k         = 1,nz
       DO j        = 1,nl-1
        IF (p1(j).gt.p(ilon,ilat,k,1).and.p(ilon,ilat,k,1).gt.p1(j+1)) THEN
         w1        = p1(j)-p(ilon,ilat,k,1)
         w2        = p(ilon,ilat,k,1)-p1(j+1)
         uu        = (w2*u1(j)+w1*u1(j+1))/(w1+w2)
         vv        = (w2*v1(j)+w1*v1(j+1))/(w1+w2)
         qq        = (w2*q1(j)+w1*q1(j+1))*1e-3/(w1+w2)
         tt        = (w2*theta(j)+w1*theta(j+1))/(w1+w2)
!         err_u(k)  = serr_u*(1.+float(k-1.)/float(nz))
!         err_v(k)  = err_u(k)
!         err_t(k)  = serr_t
!         err_q(k)  = serr_q*exp(-2.*(k-1.)/float(nz))
!         err_p(k)  = serr_p*exp(2.*(k-1.)/float(nz)) 
         call compute_err_z(serr_u,serr_v,serr_t,serr_q,serr_p,k,nz,   &
                            err_u(k),err_v(k),err_t(k),err_q(k),err_p(k))
         WRITE(11,'(3I5,10E12.4)')ilon,ilat,k,uu,err_u(k),vv,err_v(k), & 
                                  tt,err_t(k),qq,err_q(k),p1(j)/10,err_p(k)
         PRINT*,ilon,ilat,k,uu,vv,tt,qq,p1(j),p(ilon,ilat,k,1),p1(j+1)
         GOTO 21
        ENDIF
       ENDDO
21     CONTINUE
      ENDDO 
20    i            = i + 1
      IF (i.le.nrs) GOTO 17
!
! close the data file and loop all files
!
      status       = NF_CLOSE(ncid)
      if (status.ne.0) print*,'Can not close the new file'
      
      END
      
      SUBROUTINE input_namelist(debug,obs_err_u,obs_err_v,obs_err_t,obs_err_q,obs_err_p)
      INCLUDE "../Registry/letkf.inc"
      RETURN
      END      

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

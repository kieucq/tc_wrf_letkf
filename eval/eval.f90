!
! [NOTE]:      This program is to read all truth forecast and prinout
!              in the grads format for viewing 
!
! [HISTORY]: - Created on Apr 4, 2010
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
      PROGRAM rms_error
      USE common_utils
      IMPLICIT none
      INCLUDE 'netcdf.inc' 
      INTEGER, PARAMETER :: n=4
      INTEGER            :: gncid,wncid,tncid,status
      INTEGER            :: ndims,nvars,ngatts,unlimdimid
      INTEGER            :: uid,vid,tid,qid,pid
      INTEGER            :: tndim,tdim(n)
      INTEGER            :: nx,ny,nz,nt,nx1,ny1,nz1,nt1,ne,no
      INTEGER            :: iday,ihour,imin 
      REAL               :: w1,w2 
      REAL               :: dt,tfcst,restart,mis,pref
      INTEGER            :: id,jd
!
      REAL,ALLOCATABLE   :: ug(:,:,:,:),vg(:,:,:,:),tg(:,:,:,:),hg(:,:,:,:),pg(:,:,:,:)
      REAL,ALLOCATABLE   :: ut(:,:,:,:),vt(:,:,:,:),tt(:,:,:,:),ht(:,:,:,:),pt(:,:,:,:)
      REAL,ALLOCATABLE   :: uw(:,:,:,:),vw(:,:,:,:),tw(:,:,:,:),hw(:,:,:,:),pw(:,:,:,:)
      REAL,ALLOCATABLE   :: ug_2d(:,:),vg_2d(:,:),tg_2d(:,:),hg_2d(:,:)
      REAL,ALLOCATABLE   :: ut_2d(:,:),vt_2d(:,:),tt_2d(:,:),ht_2d(:,:)
      REAL,ALLOCATABLE   :: uw_2d(:,:,:),vw_2d(:,:,:),tw_2d(:,:,:),hw_2d(:,:,:)
      REAL,ALLOCATABLE   :: tem1(:,:,:,:),tem2(:,:,:,:)
!
! tem vars
!
      CHARACTER*100      :: name,rfile,tem
      CHARACTER*100      :: gfile,tfile,wfile
      INTEGER            :: i,j,k,tem_len,debug,nfile,ifile,irec      
      debug              = 1
      irec               = 1 
      pref               = 50000.
      mis                = -99999
      print*,'How many ensemble member?'
      read(*,*)nfile
!
! open gfs forecast data file first 
!
      gfile              = 'gfs.dat'
      IF (debug.eq.1) PRINT*,'eval.exe: Open truth file is:  ',gfile(1:30) 
      status = NF_OPEN(gfile(1:len_trim(gfile)),nf_nowrite,gncid)
!
! pull our varid and dim information for a temporary variable
! which will be used later.
!
      tem                = 'PRES'
      tem_len            = len_trim(tem)
      PRINT*,'Checking var is: ',tem(1:tem_len)
      status = NF_INQ_VARID(gncid,tem(1:tem_len),tid)
      status = NF_INQ_VARNDIMS(gncid,tid,tndim)
      status = NF_INQ_VARDIMID(gncid,tid,tdim)
      IF (tndim.ne.n) THEN
       PRINT*,'Number of dim is /= n...allocation stop'
       status            = NF_CLOSE(gncid)
       STOP
      ENDIF
      IF (debug.eq.1) PRINT*,'eval.exe: Pull out information of ',tem(1:tem_len)
      IF (debug.eq.1) PRINT*,'tid        =',tid
      IF (debug.eq.1) PRINT*,'tndim      =',tndim
      DO i               = 1,tndim
       j                 = tdim(i)
       status            = NF_INQ_DIMNAME(gncid,j,name)
       status            = NF_INQ_DIMLEN(gncid,j,k)
       IF (i.eq.1) nx    = k
       IF (i.eq.2) ny    = k
       IF (i.eq.3) nz    = k
       IF (i.eq.4) nt    = k
       PRINT*,'Dimension name is:',i,tdim(i),name(1:20)
      ENDDO
      IF (debug.eq.1) PRINT*,'nx         =',nx
      IF (debug.eq.1) PRINT*,'ny         =',ny
      IF (debug.eq.1) PRINT*,'nz         =',nz
      IF (debug.eq.1) PRINT*,'nt         =',nt
      nx1          = nx + 1
      ny1          = ny + 1
      nz1          = nz + 1
      ALLOCATE(ug(nx,ny,nz,nt),vg(nx,ny,nz,nt),tg(nx,ny,nz,nt),hg(nx,ny,nz,nt),pg(nx,ny,nz,nt))
!
! read the GFS forecast data and interpolate the 500 mb level
!
      tem          = 'UU'  
      allocate(tem1(nx1,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(gncid,tid,tem1)
      ug(:,:,:,1)  = 0.5*(tem1(1:nx,:,:,1)+tem1(2:nx1,:,:,1))
      deallocate(tem1)
! 
      tem          = 'VV'
      allocate(tem1(nx,ny1,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(gncid,tid,tem1)
      vg(:,:,:,1)  = 0.5*(tem1(:,1:ny,:,1)+tem1(:,2:ny1,:,1))
      deallocate(tem1)
!
      tem          = 'TT'
      allocate(tem1(nx,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(gncid,tid,tem1)
      tg(:,:,:,1)  = tem1(:,:,:,1)
      deallocate(tem1)
!
      tem          = 'GHT'
      allocate(tem1(nx,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(gncid,tid,tem1)
      hg(:,:,:,1)  = tem1(:,:,:,1)
      deallocate(tem1)
!
      tem          = 'PRES'
      allocate(tem1(nx,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(gncid,tid,tem1)
      pg(:,:,:,1)  = tem1(:,:,:,1)
      deallocate(tem1)
!
! search for 500 mb level for checking
!       
      allocate(ug_2d(nx,ny),vg_2d(nx,ny),tg_2d(nx,ny),hg_2d(nx,ny))
      do k         = 1,nz
       if (abs(pg(nx/2,ny/2,k,1)-pref).lt.0.001) then
        ug_2d(:,:) = ug(:,:,k,1)
        vg_2d(:,:) = vg(:,:,k,1)
        tg_2d(:,:) = tg(:,:,k,1)
        hg_2d(:,:) = hg(:,:,k,1)
        goto 100
       endif 
      enddo
      print*,'eval.exe: could not find the 500 mb level in the gfs...stop'
      read*
      stop
100   continue
      deallocate(ug,vg,tg,hg,pg)
      status       = NF_CLOSE(gncid)
!
! Now processing the truth. We will allocate the level for interpolating
! to the 500 mb level.
!
      tfile              = 'truth.dat'
      status = NF_OPEN(tfile(1:len_trim(tfile)),nf_nowrite,tncid)
      tem                = 'T'
      tem_len            = len_trim(tem)
      PRINT*,'Checking var is: ',tem(1:tem_len)
      status = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status = NF_INQ_VARNDIMS(tncid,tid,tndim)
      status = NF_INQ_VARDIMID(tncid,tid,tdim)
      IF (tndim.ne.n) THEN
       PRINT*,'Number of dim is /= n...allocation stop'
       status            = NF_CLOSE(tncid)
       STOP
      ENDIF
      IF (debug.eq.1) PRINT*,'eval.exe: Pull out information of ',tem(1:tem_len)
      IF (debug.eq.1) PRINT*,'tid        =',tid
      IF (debug.eq.1) PRINT*,'tndim      =',tndim
      DO i               = 1,tndim
       j                 = tdim(i)
       status            = NF_INQ_DIMNAME(tncid,j,name)
       status            = NF_INQ_DIMLEN(tncid,j,k)
       IF (i.eq.1) nx    = k
       IF (i.eq.2) ny    = k
       IF (i.eq.3) nz    = k
       IF (i.eq.4) nt    = k
       PRINT*,'Dimension name is:',i,tdim(i),name(1:20)
      ENDDO
      IF (debug.eq.1) PRINT*,'nx         =',nx
      IF (debug.eq.1) PRINT*,'ny         =',ny
      IF (debug.eq.1) PRINT*,'nz         =',nz
      IF (debug.eq.1) PRINT*,'nt         =',nt
      nx1          = nx + 1
      ny1          = ny + 1
      nz1          = nz + 1
      ALLOCATE(ut(nx,ny,nz,nt),vt(nx,ny,nz,nt),tt(nx,ny,nz,nt),ht(nx,ny,nz,nt),pt(nx,ny,nz,nt))
!
! read the truth forecast data, which is the GFS at t = 0 
!
      tem          = 'U'
      allocate(tem1(nx1,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,tem1)
      ut(:,:,:,1)  = 0.5*(tem1(1:nx,:,:,1)+tem1(2:nx1,:,:,1))
      deallocate(tem1)
!
      tem          = 'V'
      allocate(tem1(nx,ny1,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,tem1)
      vt(:,:,:,1)  = 0.5*(tem1(:,1:ny,:,1)+tem1(:,2:ny1,:,1))
      deallocate(tem1)
!
      tem          = 'T'
      allocate(tem1(nx,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,tem1)
      tt(:,:,:,1)  = tem1(:,:,:,1)
      deallocate(tem1)
!
      tem          = 'PH'
      allocate(tem1(nx,ny,nz1,nt),tem2(nx,ny,nz1,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,tem1)
      tem          = 'PHB'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,tem2)
      tem1         = tem1+tem2
      ht(:,:,:,1)  = 0.5*(tem1(:,:,1:nz,1)+tem1(:,:,2:nz1,1)) 
      deallocate(tem1,tem2)
!
      tem          = 'P'
      allocate(tem1(nx,ny,nz,nt),tem2(nx,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,tem1)
      tem          = 'PB'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,tem2)
      pt(:,:,:,1)  = tem1(:,:,:,1)+tem2(:,:,:,1)
      deallocate(tem1,tem2)
!
! search for 500 mb level for checking
!
      allocate(ut_2d(nx,ny),vt_2d(nx,ny),tt_2d(nx,ny),ht_2d(nx,ny))
      do i         = 1,nx
       do j        = 1,ny
        do k       = 1,nz
         if (pt(i,j,k,1).gt.pref.and.pref.gt.pt(i,j,k+1,1)) then
          w1       = pt(i,j,k,1) - pref
          w2       = pref - pt(i,j,k+1,1) 
          ut_2d(i,j) = (w2*ut(i,j,k,1)+w1*ut(i,j,k+1,1))/(w1+w2)
          vt_2d(i,j) = (w2*vt(i,j,k,1)+w1*vt(i,j,k+1,1))/(w1+w2)
          tt_2d(i,j) = (w2*tt(i,j,k,1)+w1*tt(i,j,k+1,1))/(w1+w2)
          ht_2d(i,j) = (w2*ht(i,j,k,1)+w1*ht(i,j,k+1,1))/(w1+w2)
          goto 101
         endif
        enddo
        print*,'eval.exe: could not find the 500 mb level in the truth...stop'
        read*
        stop
101     continue
       enddo
      enddo
      deallocate(ut,vt,tt,ht,pt)
      status       = NF_CLOSE(tncid)
!
! process the WRF forecast from all members and truth
!      
      ALLOCATE(pw(nx,ny,nz,nfile))
      ALLOCATE(uw(nx,ny,nz,nfile),vw(nx,ny,nz,nfile),tw(nx,ny,nz,nfile),hw(nx,ny,nz,nfile))
      ALLOCATE(uw_2d(nx,ny,nfile),vw_2d(nx,ny,nfile),tw_2d(nx,ny,nfile),hw_2d(nx,ny,nfile))
      ifile              = 1
      wfile              = 'wrf_000.dat'
102   continue      
      call name_string(ifile,7,wfile)
      IF (debug.eq.1) PRINT*,'eval.exe: open forecast file is:  ',wfile(1:30)
      status = NF_OPEN(wfile(1:len_trim(wfile)),nf_nowrite,wncid)
!
! reading data now
!
      allocate(tem1(nx1,ny,nz,nt))
      tem          = 'U'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(wncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(wncid,tid,tem1)
      uw(:,:,:,ifile) = 0.5*(tem1(1:nx,:,:,1)+tem1(2:nx1,:,:,1))
      deallocate(tem1)
!
      allocate(tem1(nx,ny1,nz,nt))
      tem          = 'V'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(wncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(wncid,tid,tem1)
      vw(:,:,:,ifile) = 0.5*(tem1(:,1:ny,:,1)+tem1(:,2:ny1,:,1))
      deallocate(tem1)
!
      allocate(tem1(nx,ny,nz,nt))
      tem          = 'T'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(wncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(wncid,tid,tem1)
      tw(:,:,:,ifile) = tem1(:,:,:,1)
      deallocate(tem1)            
!
      tem          = 'PH'
      allocate(tem1(nx,ny,nz1,nt),tem2(nx,ny,nz1,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(wncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(wncid,tid,tem1)
      tem          = 'PHB'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(wncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(wncid,tid,tem2)
      tem1         = tem1+tem2
      hw(:,:,:,ifile)  = 0.5*(tem1(:,:,1:nz,1)+tem1(:,:,2:nz1,1))
      deallocate(tem1,tem2)
!      
      tem          = 'P'
      allocate(tem1(nx,ny,nz,nt),tem2(nx,ny,nz,nt))
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(wncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(wncid,tid,tem1)
      tem          = 'PB'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(wncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(wncid,tid,tem2)
      pw(:,:,:,ifile)  = tem1(:,:,:,1)+tem2(:,:,:,1)
      deallocate(tem1,tem2)
      if (debug.eq.1) print*,'eval.exe: finish reading data at',ifile
!     
! search for 500 mb level for checking
!
      do i         = 1,nx
       do j        = 1,ny
        do k       = 1,nz
         if (pw(i,j,k,1).ge.pref.and.pref.gt.pw(i,j,k+1,1)) then
          w1       = pw(i,j,k,1) - pref
          w2       = pref - pw(i,j,k+1,1)
          uw_2d(i,j,ifile) = (w2*uw(i,j,k,ifile)+w1*uw(i,j,k+1,ifile))/(w1+w2)
          vw_2d(i,j,ifile) = (w2*vw(i,j,k,ifile)+w1*vw(i,j,k+1,ifile))/(w1+w2)
          tw_2d(i,j,ifile) = (w2*tw(i,j,k,ifile)+w1*tw(i,j,k+1,ifile))/(w1+w2)
          hw_2d(i,j,ifile) = (w2*hw(i,j,k,ifile)+w1*hw(i,j,k+1,ifile))/(w1+w2)
          goto 103
         endif
        enddo
        print*,'eval.exe: could not find the 500 mb level in the forecast...stop'
        do k       = 1,nz
         print*,i,j,k,pw(i,j,k,1),pw(i,j,k+1,1),pref
        enddo
        read*
        stop
103     continue
       enddo
      enddo
!
! close the data file and loop all files
!
      status       = NF_CLOSE(wncid)
      IF (status.ne.0) PRINT*,'Can not close the new file'
      ifile        = ifile + 1
      IF (ifile.le.nfile) GOTO 102
!
! PRINT out now
!
      open(99,file='eval.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny)
      WRITE(99,rec=irec)((ug_2d(i,j),i=1,nx),j=1,ny)
      irec        = irec + 1
      WRITE(99,rec=irec)((vg_2d(i,j),i=1,nx),j=1,ny)
      irec        = irec + 1
      WRITE(99,rec=irec)((tg_2d(i,j),i=1,nx),j=1,ny)
      irec        = irec + 1
      WRITE(99,rec=irec)((hg_2d(i,j),i=1,nx),j=1,ny)
      irec        = irec + 1
      WRITE(99,rec=irec)((ut_2d(i,j),i=1,nx),j=1,ny)
      irec        = irec + 1
      WRITE(99,rec=irec)((vt_2d(i,j),i=1,nx),j=1,ny)
      irec        = irec + 1
      WRITE(99,rec=irec)((tt_2d(i,j),i=1,nx),j=1,ny)
      irec        = irec + 1
      WRITE(99,rec=irec)((ht_2d(i,j)/10.,i=1,nx),j=1,ny)
      irec        = irec + 1
      DO k         = 1,nfile
       WRITE(99,rec=irec)((uw_2d(i,j,k),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nfile
       WRITE(99,rec=irec)((vw_2d(i,j,k),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nfile
       WRITE(99,rec=irec)((tw_2d(i,j,k),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nfile
       WRITE(99,rec=irec)((hw_2d(i,j,k)/10.,i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
!
! close the data file now
!
      CALL error_handle(99999)
      END


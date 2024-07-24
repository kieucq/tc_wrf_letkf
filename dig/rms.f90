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
      INTEGER            :: ancid,bncid,cncid,oncid,tncid,gncid,status
      INTEGER            :: status_a,status_c,status_t,status_i
      INTEGER            :: ndims,nvars,ngatts,unlimdimid
      INTEGER            :: uid,vid,tid,qid,pid
      INTEGER            :: tndim,tdim(n)
      INTEGER            :: nx,ny,nz,nt,nx1,ny1,nz1,nt1,ne,no
      INTEGER            :: iday,ihour,imin 
      REAL               :: umax,vmax,tmax,rmax,r_obs  
      REAL               :: dt,tfcst,restart,mis
      INTEGER            :: id,jd
      REAL,ALLOCATABLE   :: ua(:,:,:,:),va(:,:,:,:),ta(:,:,:,:),qa(:,:,:,:)
      REAL,ALLOCATABLE   :: uc(:,:,:,:),vc(:,:,:,:),tc(:,:,:,:),qc(:,:,:,:)
      REAL,ALLOCATABLE   :: ut(:,:,:,:),vt(:,:,:,:),tt(:,:,:,:),qt(:,:,:,:)                  
      REAL,ALLOCATABLE   :: ui(:,:,:,:),vi(:,:,:,:),ti(:,:,:,:),qi(:,:,:,:)
      REAL,ALLOCATABLE   :: u1(:,:,:,:),v1(:,:,:,:),t1(:,:,:,:),q1(:,:,:,:)    
      REAL, ALLOCATABLE, DIMENSION(:)       :: rmsa,rmsb,rmst,rmsc,tu,tv,tz
      REAL, ALLOCATABLE, DIMENSION(:)       :: lono,lato,rmsi
!
! tem vars
!
      CHARACTER*100      :: name,rfile,tem,wfile
      CHARACTER*100      :: bfile,cfile,tfile,ofile,afile,gfile
      INTEGER            :: i,j,k,tem_len,debug,nfile,ifile,irec      
      irec               = 1 
      mis                = -99999
!
! reading namelist
!
!     CALL input_namelist(debug,ne,restart,tfcst,no)
!     PRINT*,'rms.exe: restart        =',restart
!     PRINT*,'rms.exe: tfcst          =',tfcst
!     PRINT*,'rms.exe: number of ensemble is: ',ne
!     PRINT*,'rms.exe: number of obs is: ',no
!     restart            = restart/60.
!     IF (debug.eq.1) read*
!
! open data file now
!
      print*,'How many files?' 
      read(*,*)nfile
      ifile              = 1
!     nfile              = nint(tfcst/restart) + 1
      tfile              = 'tru_000.dat'
      ofile              = 'obs_000.dat'
      afile              = 'ana_000.dat'
      cfile              = 'ctl_000.dat'
      gfile              = 'ini_000.dat'
10    continue      
      call name_string(ifile,7,tfile)
      call name_string(ifile,7,afile)
      call name_string(ifile,7,cfile)  
      call name_string(ifile,7,gfile)                
      IF (debug.eq.1) PRINT*,'ana.exe: Open truth file is:  ',tfile(1:30)
      IF (debug.eq.1) PRINT*,'ana.exe: Open analysis file is: ',afile(1:30)
      IF (debug.eq.1) PRINT*,'ana.exe: Open observation file is:  ',ofile(1:30)
      IF (debug.eq.1) PRINT*,'ana.exe: Open control file is:  ',cfile(1:30)
      IF (debug.eq.1) PRINT*,'ana.exe: Analysis initial file is:  ',gfile(1:30)
      status_a = NF_OPEN(afile(1:len_trim(afile)),nf_nowrite,ancid)
      status_t = NF_OPEN(tfile(1:len_trim(tfile)),nf_nowrite,tncid)
      status_c = NF_OPEN(cfile(1:len_trim(cfile)),nf_nowrite,cncid)   
      status_i = NF_OPEN(gfile(1:len_trim(gfile)),nf_nowrite,gncid)   
      IF (debug.ge.1) THEN
       PRINT*,''
       PRINT*,'1. NF_OPEN and return value: '
       PRINT*,'status    =',status
       PRINT*,'nf_noerr  =',nf_noerr 
       PRINT*,'ancid     =',ancid
       PRINT*,'cncid     =',cncid
       PRINT*,'tncid     =',tncid                     
       PRINT*,'gncid     =',gncid
      ENDIF
!
! probing some dimension and structure of data fields
!
      IF (ifile.eq.1) THEN
       status = NF_INQ(cncid, ndims, nvars, ngatts, unlimdimid)
       PRINT*,''
       PRINT*,'2. NF_INQ returns values: '
       PRINT*,'cncid      =',cncid                                               ! file id
       PRINT*,'ndims      =',ndims                                               ! number of dims
       PRINT*,'nvars      =',nvars                                               ! number of varibales
       PRINT*,'ngatts     =',ngatts                                              ! number of globale attributes
       PRINT*,'unlimdimid =',unlimdimid                                          ! number of umlimitted dim
!
! pull our varid and dim information for a temporary variable
! which will be used later.  
!
       tem                = 'T'
       tem_len            = len_trim(tem)
       PRINT*,'Checking var is: ',tem(1:tem_len)
       status = NF_INQ_VARID(cncid,tem(1:tem_len),tid)
       status = NF_INQ_VARNDIMS(cncid,tid,tndim)
       status = NF_INQ_VARDIMID(cncid,tid,tdim)
       IF (tndim.ne.n) THEN
        PRINT*,'Number of dim is /= n...allocation stop'
        status            = NF_CLOSE(cncid)
        STOP
       ENDIF
       PRINT*,''
       PRINT*,'3. Pull out information about: ',tem(1:tem_len)
       PRINT*,'tid        =',tid
       PRINT*,'tndim      =',tndim
       DO i               = 1,tndim
        j                 = tdim(i)
        status            = NF_INQ_DIMNAME(cncid,j,name)
        status            = NF_INQ_DIMLEN(cncid,j,k)
        IF (i.eq.1) nx    = k
        IF (i.eq.2) ny    = k
        IF (i.eq.3) nz    = k
        IF (i.eq.4) nt    = k
        PRINT*,'Dimension name is:',i,tdim(i),name(1:20)
       ENDDO
       PRINT*,'nx         =',nx
       PRINT*,'ny         =',ny
       PRINT*,'nz         =',nz
       PRINT*,'nt         =',nt
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
       nx1          = nx + 1
       ny1          = ny + 1
       nz1          = nz + 1
       ALLOCATE(ua(nx,ny,nz,nfile),va(nx,ny,nz,nfile),ta(nx,ny,nz,nfile),qa(nx,ny,nz,nfile))
       ALLOCATE(uc(nx,ny,nz,nfile),vc(nx,ny,nz,nfile),tc(nx,ny,nz,nfile),qc(nx,ny,nz,nfile))
       ALLOCATE(ut(nx,ny,nz,nfile),vt(nx,ny,nz,nfile),tt(nx,ny,nz,nfile),qt(nx,ny,nz,nfile))
       ALLOCATE(ui(nx,ny,nz,nfile),vi(nx,ny,nz,nfile),ti(nx,ny,nz,nfile),qi(nx,ny,nz,nfile))
       ALLOCATE(u1(nx1,ny,nz,nt),v1(nx,ny1,nz,nt),t1(nx,ny,nz,nt),q1(nx,ny,nz,nt))     
       ALLOCATE(rmsa(nfile),rmsc(nfile),rmsi(nfile),rmst(nfile))
      ENDIF 
!
! reading data now
!
      tem          = 'U'
      tem_len      = len_trim(tem)
      u1           = 0
      status       = NF_INQ_VARID(ancid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(ancid,uid,u1)
      if (status.eq.0) then
       ua(:,:,:,ifile) = 0.5*(u1(1:nx,:,:,1)+u1(2:nx1,:,:,1))
      else
       ua(:,:,:,ifile) = 0.
      endif
      u1           = 0
      status       = NF_INQ_VARID(cncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(cncid,uid,u1)            
      if (status.eq.0) then
       uc(:,:,:,ifile) = 0.5*(u1(1:nx,:,:,1)+u1(2:nx1,:,:,1))
      else
       uc(:,:,:,ifile) = 0.
      endif
      u1           = 0
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(tncid,uid,u1)
      if (status.eq.0) then
       ut(:,:,:,ifile) = 0.5*(u1(1:nx,:,:,1)+u1(2:nx1,:,:,1))
      else
       ut(:,:,:,ifile) = 0.
      endif
      u1           = 0
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(gncid,uid,u1)
      if (status.eq.0) then
       ui(:,:,:,ifile) = 0.5*(u1(1:nx,:,:,1)+u1(2:nx1,:,:,1))
      else
       ui(:,:,:,ifile) = 0.
      endif
!
      tem          = 'V'
      tem_len      = len_trim(tem)
      v1           = 0
      status       = NF_INQ_VARID(ancid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(ancid,vid,v1)
      if (status.eq.0) then
       va(:,:,:,ifile) = 0.5*(v1(:,1:ny,:,1)+v1(:,2:ny1,:,1))
      else
       va(:,:,:,ifile) = 0.
      endif
      v1           = 0
      status       = NF_INQ_VARID(cncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(cncid,vid,v1)
      if (status.eq.0) then
       vc(:,:,:,ifile) = 0.5*(v1(:,1:ny,:,1)+v1(:,2:ny1,:,1))
      else
       vc(:,:,:,ifile) = 0.
      endif
      v1           = 0
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(tncid,vid,v1)                  
      if (status.eq.0) then
       vt(:,:,:,ifile) = 0.5*(v1(:,1:ny,:,1)+v1(:,2:ny1,:,1))
      else
       vt(:,:,:,ifile) = 0.
      endif
      v1           = 0
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(gncid,vid,v1)
      if (status.eq.0) then
       vi(:,:,:,ifile) = 0.5*(v1(:,1:ny,:,1)+v1(:,2:ny1,:,1))
      else
       vi(:,:,:,ifile) = 0.
      endif
!
      tem          = 'T'
      tem_len      = len_trim(tem)
      t1           = 0.
      status       = NF_INQ_VARID(ancid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(ancid,tid,t1)
      if (status.eq.0) then
       ta(:,:,:,ifile) = t1(:,:,:,1)            
      else
       ta(:,:,:,ifile) = 0.
      endif
      t1           = 0.
      status       = NF_INQ_VARID(cncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(cncid,tid,t1)
      if (status.eq.0) then
       tc(:,:,:,ifile) = t1(:,:,:,1)                  
      else
       tc(:,:,:,ifile) = 0.
      endif
      t1           = 0.
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(tncid,tid,t1)            
      if (status.eq.0) then
       tt(:,:,:,ifile) = t1(:,:,:,1)                  
      else
       tt(:,:,:,ifile) = 0.
      endif
      t1           = 0.
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(gncid,tid,t1)
      if (status.eq.0) then
       ti(:,:,:,ifile) = t1(:,:,:,1)
      else
       ti(:,:,:,ifile) = 0.
      endif
!
      tem          = 'QVAPOR'
      tem_len      = len_trim(tem)
      q1           = 0.
      status       = NF_INQ_VARID(ancid,tem(1:tem_len),qid)
      status       = NF_GET_VAR_REAL(ancid,qid,q1)
      if (status.eq.0) then
       qa(:,:,:,ifile) = q1(:,:,:,1)                  
      else
       qa(:,:,:,ifile) = 0.
      endif
      q1           = 0.
      status       = NF_INQ_VARID(cncid,tem(1:tem_len),qid)
      status       = NF_GET_VAR_REAL(cncid,qid,q1)
      if (status.eq.0) then
       qc(:,:,:,ifile) = q1(:,:,:,1)                        
      else
       qc(:,:,:,ifile) = 0.
      endif
      q1           = 0.
      status       = NF_INQ_VARID(tncid,tem(1:tem_len),qid)
      status       = NF_GET_VAR_REAL(tncid,qid,q1)                  
      if (status.eq.0) then
       qt(:,:,:,ifile) = q1(:,:,:,1)
      else
       qt(:,:,:,ifile) = 0.
      endif
      q1           = 0.
      status       = NF_INQ_VARID(gncid,tem(1:tem_len),qid)
      status       = NF_GET_VAR_REAL(gncid,qid,q1)
      if (status.eq.0) then
       qi(:,:,:,ifile) = q1(:,:,:,1)
      else
       qi(:,:,:,ifile) = 0.
      endif
!
! compute the total rms error. Following Zhang (2004)
! Energy RMS = 0.5(U'U' + V'V' + CpT'T'/Tr)
!
      rmsa(ifile)  = 0.
      rmsc(ifile)  = 0.
      rmsi(ifile)  = 0.
      rmst(ifile)  = 0.
      DO k         = 1,nz
       DO i        = 1,nx
        DO j       = 1,ny
         rmsa(ifile) = rmsa(ifile) + 0.5*(ua(i,j,k,ifile)-ut(i,j,k,ifile))**2      &
                                   + 0.5*(va(i,j,k,ifile)-vt(i,j,k,ifile))**2      
         rmsc(ifile) = rmsc(ifile) + 0.5*(uc(i,j,k,ifile)-ut(i,j,k,ifile))**2      &
                                   + 0.5*(vc(i,j,k,ifile)-vt(i,j,k,ifile))**2 
         rmsi(ifile) = rmsi(ifile) + 0.5*(ui(i,j,k,ifile)-ut(i,j,k,ifile))**2      &
                                   + 0.5*(vi(i,j,k,ifile)-vt(i,j,k,ifile))**2     
         rmst(ifile) = rmst(ifile) + 0.5*ut(i,j,k,ifile)**2                        &
                                   + 0.5*vt(i,j,k,ifile)**2
	ENDDO			 
       ENDDO
      ENDDO
      rmsa(ifile)  = sqrt(rmsa(ifile)/nx/ny/nz)
      rmsc(ifile)  = sqrt(rmsc(ifile)/nx/ny/nz)
      rmsi(ifile)  = sqrt(rmsi(ifile)/nx/ny/nz)          
      rmst(ifile)  = sqrt(rmst(ifile)/nx/ny/nz)
!
! PRINT out now
!
      IF (status_a.ne.0) THEN 
       rmsa(ifile)     = mis
       ua(:,:,:,ifile) = mis
       va(:,:,:,ifile) = mis
       ta(:,:,:,ifile) = mis
       qa(:,:,:,ifile) = mis
      ENDIF
      IF (status_c.ne.0) THEN 
       rmsc(ifile)     = mis
       uc(:,:,:,ifile) = mis
       vc(:,:,:,ifile) = mis
       tc(:,:,:,ifile) = mis
       qc(:,:,:,ifile) = mis
      ENDIF
      IF (status_i.ne.0) THEN 
       rmsi(ifile)     = mis
       ui(:,:,:,ifile) = mis
       vi(:,:,:,ifile) = mis
       ti(:,:,:,ifile) = mis
       qi(:,:,:,ifile) = mis
      ENDIF
      IF (status_t.ne.0) THEN 
       rmst(ifile)     = mis
       rmsi(ifile)     = mis
       rmsa(ifile)     = mis
       rmsc(ifile)     = mis
       ut(:,:,:,ifile) = mis
       vt(:,:,:,ifile) = mis
       tt(:,:,:,ifile) = mis
       qt(:,:,:,ifile) = mis
      ENDIF
      IF (ifile.eq.1) THEN
       open(99,file='rms.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*4)
      ENDIF 
      DO k         = 1,nz
       WRITE(99,rec=irec)((ua(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((va(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((ta(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((qa(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO       
      DO k         = 1,nz
       WRITE(99,rec=irec)((uc(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((vc(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((tc(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((qc(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO  
      DO k         = 1,nz
       WRITE(99,rec=irec)((ut(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((vt(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((tt(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((qt(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((ui(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((vi(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((ti(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((qi(i,j,k,ifile),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO       
!
! close the data file and loop all files
!
      status       = NF_CLOSE(ancid)
      status       = NF_CLOSE(cncid)
      status       = NF_CLOSE(tncid)
      status       = NF_CLOSE(gncid)            
      IF (status.ne.0) PRINT*,'Can not close the new file'
      ifile        = ifile + 1
      IF (ifile.le.nfile) GOTO 10
!
! print out error file 
!
      OPEN(11,file='rms.txt')
      WRITE(11,*)' cycle    rmsi        rmsa        rmsc         rmst  '
      DO i         = 1,nfile
       WRITE(11,'(I5,10F12.4)')i,rmsi(i),rmsa(i),rmsc(i),rmst(i)
      ENDDO 
!
! close the data file now
!
      CALL error_handle(99999)
      END


      SUBROUTINE input_namelist(debug,ne,restart,tfcst,no)
      INCLUDE "../Registry/letkf.inc"
      RETURN
      END

!
! [NOTE]:      This program is to read all truth forecast and prinout
!              in the grads format for viewing 
!
! [HISTORY]: - Created on Mar 18, 2010
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
      PROGRAM plot_truth
      USE common_utils
      IMPLICIT none
      INCLUDE 'netcdf.inc' 
      INTEGER, PARAMETER :: n=4
      INTEGER            :: ncid,status
      INTEGER            :: ndims,nvars,ngatts,unlimdimid
      INTEGER            :: uid,vid,tid,rid,pid
      INTEGER            :: tndim,tdim(n)
      INTEGER            :: nx,ny,nz,nt,nx1,ny1,nz1,nt1
      INTEGER            :: iday,ihour,imin 
      REAL               :: umax,vmax,tmax,rmax  
      REAL               :: rtime,tfcst,restart
      REAL,ALLOCATABLE   :: u(:,:,:,:),v(:,:,:,:),t(:,:,:,:),r(:,:,:,:)
      REAL,ALLOCATABLE   :: u1(:,:,:,:),v1(:,:,:,:),t1(:,:,:,:),r1(:,:,:,:)
      REAL,ALLOCATABLE   :: tem3d(:,:,:),tem1d(:)
      CHARACTER*100      :: name,ofile,tem
      INTEGER            :: i,j,k,tem_len,debug,ne,nfile,ifile,irec
      irec               = 1 
!
! reading namelist now
!
      call input_namelist(debug,tfcst,restart)
      nfile              = nint(tfcst/restart) + 1
      if (debug.eq.1) then
       print*,' truth.exe: forecast length tfcst = ',tfcst
       print*,' truth.exe: resrtat interval restart =',restart
       print*,' truth.exe: number of forecast output nfile =',nfile
       read*
      endif        
!
! open data file now
!      
      ifile = 1
8     continue      
      rtime              = (ifile-1)*restart*3600
      iday               = int (rtime/86400.)
      ihour              = int ((rtime-iday*86400.)/3600.)
      imin               = int ((rtime-iday*86400.-ihour*3600.)/60.)
      ofile              = 'tru_000.dat'
      call name_string(ifile,7,ofile)
      if (debug.eq.1) print*,' truth.exe: opened file is: ',ofile(1:30)
      status = NF_OPEN(ofile(1:len_trim(ofile)),nf_nowrite,ncid)
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
      if (ifile.eq.1) THEN
       status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
       print*,''
       print*,'2. NF_INQ returns values: '
       print*,'ncid       =',ncid                                                ! file id
       print*,'ndims      =',ndims                                               ! number of dims
       print*,'nvars      =',nvars                                               ! number of varibales
       print*,'ngatts     =',ngatts                                              ! number of globale attributes
       print*,'unlimdimid =',unlimdimid                                          ! number of umlimitted dim
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
       print*,'nx         =',nx
       print*,'ny         =',ny
       print*,'nz         =',nz
       print*,'nt         =',nt
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
       nx1          = nx + 1
       ny1          = ny + 1
       nz1          = nz + 1
       ALLOCATE(u(nx1,ny,nz,nt),v(nx,ny1,nz,nt),t(nx,ny,nz,nt),r(nx,ny,nz,nt))
       ALLOCATE(u1(nx,ny,nz,nfile),v1(nx,ny,nz,nfile),t1(nx,ny,nz,nfile),r1(nx,ny,nz,nfile))   
      endif
!
! reading data now
!
      tem          = 'U'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(ncid,uid,u)
      u1(1:nx,:,:,ifile) = 0.5*(u(1:nx,:,:,1) + u(2:nx1,:,:,1))
      tem          = 'V'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(ncid,vid,v)
      v1(1:nx,:,:,ifile) = 0.5*(v(:,1:ny,:,1) + v(:,2:ny1,:,1))
      tem          = 'T'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,t)
      t1(:,:,:,ifile) = t(:,:,:,1)
      tem          = 'QVAPOR'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_GET_VAR_REAL(ncid,rid,r)
      r1(:,:,:,ifile) = r(:,:,:,1)
!
! close the data file and loop all files
!
      status       = NF_CLOSE(ncid)
      if (status.ne.0) print*,'Can not close the new file'
      ifile        = ifile + 1
      if (ifile.le.nfile) GOTO 8
!
! printout now
!
      open(99,file='tru_check.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*4)
      do ifile     = 1,nfile  
       do k        = 1,nz
        write(99,rec=irec)((u1(i,j,k,ifile),i=1,nx),j=1,ny)
	irec       = irec + 1
       enddo
       do k        = 1,nz
        write(99,rec=irec)((v1(i,j,k,ifile),i=1,nx),j=1,ny)
	irec       = irec + 1
       enddo
       do k        = 1,nz
        write(99,rec=irec)((t1(i,j,k,ifile),i=1,nx),j=1,ny)
	irec       = irec + 1
       enddo
       do k        = 1,nz
        write(99,rec=irec)((r1(i,j,k,ifile),i=1,nx),j=1,ny)
	irec       = irec + 1
       enddo         
      enddo 
!
! close the data file now
!
      CALL error_handle(99999) 
      END

      SUBROUTINE input_namelist(debug,tfcst,restart)
      INCLUDE "../Registry/letkf.inc"
      RETURN
      END

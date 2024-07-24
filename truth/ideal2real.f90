!
! [NOTE]:      This program is to put all of the idealization data 
!              into the wrfinput_d01 created by real.exe
!
! [HISTORY]: - Created on Apr 23, 2010
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
      PROGRAM ideal2real
      IMPLICIT none
      INCLUDE 'netcdf.inc' 
      INTEGER, PARAMETER :: n=4
      INTEGER            :: ncid,wcid,status
      INTEGER            :: ndims,nvars,ngatts,unlimdimid
      INTEGER            :: uid,vid,tid,rid,pid
      INTEGER            :: tndim,tdim(n)
      INTEGER            :: nx,ny,nz,nt,nx1,ny1,nz1,nt1,nx2,ny2,nz2,nt2
      INTEGER            :: iday,ihour,imin 
      REAL               :: umax,vmax,tmax,rmax  
      REAL               :: rtime,tfcst,restart
      REAL,ALLOCATABLE   :: u(:,:,:,:),v(:,:,:,:),t(:,:,:,:),r(:,:,:,:)
      REAL,ALLOCATABLE   :: u1(:,:,:,:),v1(:,:,:,:),t1(:,:,:,:),r1(:,:,:,:)
      REAL,ALLOCATABLE   :: tem3d(:,:,:,:),tem1d(:,:)
      CHARACTER*100      :: name,ofile,tem,wfile
      INTEGER            :: i,j,k,tem_len,debug,ne,nfile,ifile,irec
      irec               = 1 
      nfile              = 1
!
! open data file now
!      
      ifile = 1
8     continue      
      ofile              = 'input_wrfinput_d01_ideal_exe'
      wfile              = 'wrfinput_d01'
      if (debug.eq.1) print*,' ideal2real: opened file is: ',ofile(1:30)
      if (debug.eq.1) print*,' ideal2real: write file is: ',wfile(1:30)
      status = NF_OPEN(ofile(1:len_trim(ofile)),nf_nowrite,ncid)
      status = NF_OPEN(wfile(1:len_trim(wfile)),nf_write,wcid)
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
! pull our varid and dim information for the ideal wrf input
! which will be used later.  
!
       tem                = 'T'
       tem_len            = len_trim(tem)
       print*,'ideal2real: checking var is: ',tem(1:tem_len)
       status = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
       status = NF_INQ_VARNDIMS(ncid,tid,tndim)
       status = NF_INQ_VARDIMID(ncid,tid,tdim)
       if (tndim.ne.n) THEN
        print*,'ideal2real: number of dim is /= n...allocation stop'
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
! pull our varid and dim information for the real wrf input
! which will be used to cross check for the ideal input.
!
       tem                = 'T'
       tem_len            = len_trim(tem)
       print*,'ideal2real: checking var is: ',tem(1:tem_len)
       status = NF_INQ_VARID(wcid,tem(1:tem_len),tid)
       status = NF_INQ_VARNDIMS(wcid,tid,tndim)
       status = NF_INQ_VARDIMID(wcid,tid,tdim)
       if (tndim.ne.n) THEN
        print*,'ideal2real: number of dim is /= n...allocation stop'
        STOP
       endif
       print*,''
       print*,'4. Pull out information about: ',tem(1:tem_len)
       print*,'tid        =',tid
       print*,'tndim      =',tndim
       DO i               = 1,tndim
        j                 = tdim(i)
        status            = NF_INQ_DIMNAME(wcid,j,name)
        status            = NF_INQ_DIMLEN(wcid,j,k)
        if (i.eq.1) nx2   = k
        if (i.eq.2) ny2   = k
        if (i.eq.3) nz2   = k
        if (i.eq.4) nt2   = k
        print*,'Dimension name is:',i,tdim(i),name(1:20)
       ENDDO
       print*,'nx2        =',nx2
       print*,'ny2        =',ny2
       print*,'nz2        =',nz2
       print*,'nt2        =',nt2
       if (nx.ne.nx2.or.ny.ne.ny2.or.nz.ne.nz2.or.nt.ne.nt2) then
        print*,'Input for real and ideal are not consistent...stop'
        stop
       endif
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
       nx1          = nx + 1
       ny1          = ny + 1
       nz1          = nz + 1
       ALLOCATE(u1(nx,ny,nz,nfile),v1(nx,ny,nz,nfile),t1(nx,ny,nz,nfile),r1(nx,ny,nz,nfile))   
      endif
!
! reading data now
!
      allocate(t(nx1,ny,nz,nt))
      tem          = 'U'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(ncid,uid,t)
      status       = NF_INQ_VARID(wcid,tem(1:tem_len),uid)
      status       = NF_PUT_VAR_REAL(wcid,uid,t)
      u1(1:nx,:,:,ifile) = 0.5*(t(1:nx,:,:,1) + t(2:nx1,:,:,1))
      deallocate(t)
      allocate(t(nx,ny1,nz,nt))
      tem          = 'V'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(ncid,vid,t)
      status       = NF_INQ_VARID(wcid,tem(1:tem_len),vid)
      status       = NF_PUT_VAR_REAL(wcid,vid,t)
      v1(1:nx,:,:,ifile) = 0.5*(t(:,1:ny,:,1) + t(:,2:ny1,:,1))
      deallocate(t)
      allocate(t(nx,ny,nz,nt))
      tem          = 'T'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,t)
      status       = NF_INQ_VARID(wcid,tem(1:tem_len),pid)
      status       = NF_PUT_VAR_REAL(wcid,pid,t)
      t1(:,:,:,ifile) = t(:,:,:,1)
      deallocate(t)
      allocate(t(nx,ny,nz,nt))
      tem          = 'QVAPOR'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_GET_VAR_REAL(ncid,rid,t)
      status       = NF_INQ_VARID(wcid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(wcid,rid,t)
      r1(:,:,:,ifile) = t(:,:,:,1)
      deallocate(t)
      allocate(t(nx,ny,nz1,nt))
      tem          = 'PH'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_GET_VAR_REAL(ncid,rid,t)
      status       = NF_INQ_VARID(wcid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(wcid,rid,t)
!     r1(:,:,:,ifile) = t(:,:,:,1)
      deallocate(t)
!
! insert bogussed data (dont need for the ideal2real conversion)
!
      goto 9
      allocate(tem3d(nx,ny,nz,1),tem1d(nz,1))
      tem3d        = 0.
      tem1d        = 0.
      tem          = 'T_INIT'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem3d)
      print*,'put var returns',status
      tem          = 'P'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem3d)
      print*,'put var returns',status
      tem          = 'PB'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem3d)
      print*,'put var returns',status
      goto 9
      tem          = 'T_BASE'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem1d)
      print*,'put var returns',status
      tem          = 'U_BASE'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem1d)
      print*,'put var returns',status
      tem          = 'V_BASE'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem1d)
      print*,'put var returns',status
      tem          = 'Z_BASE'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem1d)
      print*,'put var returns',status
      tem          = 'QV_BASE'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_PUT_VAR_REAL(ncid,rid,tem1d)
      print*,'put var returns',status
9     continue
!
! close the data file and loop all files
!
      status       = NF_CLOSE(ncid)
      if (status.ne.0) print*,'Can not close the ideal file'
      status       = NF_CLOSE(wcid)
      if (status.ne.0) print*,'Can not close the real file'
      ifile        = ifile + 1
      if (ifile.lt.nfile) GOTO 8
      END

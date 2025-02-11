MODULE common_io
!
! [PURPOSE]:   Contains the core of the LETKF algorithm
!
! [AUTHOR]:    Chanh Q. Kieu, Research Associate
!              Department of Atmospheric and Oceanic Science
!              Vietnam National University, Hanoi, Vietnam
!              Email: kieucq@atmos.umd.edu
!
! [HISTORY]: - 25 Mar 2010: Created by CK
!            - 23 Jul 2010: add an upper bound check for obs data 
!
! [COPYRIGHT] (C) 2010
!
  USE letkf_interface
  IMPLICIT NONE
  SAVE
  CONTAINS

  SUBROUTINE  input_namelist(debug,model_flag,ini_flag,ifactor,rscale, & 
                             nme,ne,nx,ny,nz,nxl,nzl,da_flag,obs_flag,mean_flag)

  INCLUDE "../Registry/letkf.inc"
  PRINT*,'letkf.exe: input namelist is'
  PRINT*,'letkf.exe: debug         =',debug
  PRINT*,'letkf.exe: model_flag    =',model_flag
  PRINT*,'letkf.exe: ini_flag      =',ini_flag
  PRINT*,'letkf.exe: ifactor       =',ifactor
  PRINT*,'letkf.exe: rscale        =',rscale
  PRINT*,'letkf.exe: da_flag       =',da_flag
  PRINT*,'letkf.exe: obs_flag      =',obs_flag
  PRINT*,'letkf.exe: nme           =',nme
  PRINT*,'letkf.exe: ne            =',ne
  PRINT*,'letkf.exe: nxl           =',nxl
  PRINT*,'letkf.exe: nzl           =',nzl
  PRINT*,'letkf.exe: nx            =',nx
  PRINT*,'letkf.exe: ny            =',ny
  PRINT*,'letkf.exe: nz            =',nz
  PRINT*,'letkf.exe: mean_flag     =',mean_flag
  IF (debug.ge.1) read*
  IF (ini_flag.eq.0) THEN
   PRINT*,'WARNING: INPUT IS PERFECT? WHY NEED ASSIMILATION...STOP'
   stop
  ENDIF
  IF (nme.lt.1.or.ne.lt.1) THEN
   PRINT*,'WARNING: DOES NOT MAKE SENSE VALUE OF ne OR nme...STOP'
   stop
  ENDIF  
!
! nondimensionalize the obs_err
!
  IF (nodim.eq.1) THEN
   obs_err_u     = obs_err_u/u_order
   obs_err_v     = obs_err_v/v_order
   obs_err_t     = obs_err_t/t_order
   obs_err_q     = obs_err_q/q_order
   obs_err_p     = obs_err_p/p_order
  ENDIF
  RETURN
  END SUBROUTINE  input_namelist

  SUBROUTINE get_obs(irec,debug,no,nx,ny,nz,ne)
  IMPLICIT NONE
  INTEGER            :: debug,no,irec,nx,ny,nz
  REAL, ALLOCATABLE  :: tem2d1(:,:,:)
  INTEGER            :: i,j,k,ie,ne
  i              = 1
2 continue
  read(ounit,*,end=3)olon(i),olat(i),olev(i),uo(i),err_u(i),vo(i),err_v(i), & 
                     to(i),err_t(i),qo(i),err_q(i),po(i),err_p(i)
  IF (debug.ge.2) THEN
   IF (i.eq.1) PRINT*,'debug: Checking the obs'
   PRINT*,i,olon(i),olat(i),olev(i),uo(i),vo(i),to(i),qo(i),po(i)
   IF (abs(uo(i)).gt.180.or.abs(vo(i)).gt.180.or.abs(to(i)).gt.1000.or.     &
       abs(qo(i)).gt.10.or.abs(po(i)).gt.1e7) THEN
    PRINT*,'letkf.exe: observational data is out of range... check obs 3dvar'
    STOP
   ENDIF    
  ENDIF
  i             = i + 1
  IF (i.le.no) goto 2
  goto 4
3 PRINT*,'letkf.exe: There is not enough obs data as needed by no...stop'
  stop
4 continue
!
! quick check of obs data. Note a trick here that a $ne
! copies of obs will be plot so that obs,bgd,ana can be
! put in the same file. The time index of the ctl file
! will run from 1->ne denoting which member we are plotting
!
  allocate(tem2d1(nx,ny,nz))
  tem2d1        = mis
  DO i          = 1,no
   tem2d1(int(olon(i)),int(olat(i)),int(olev(i))) = uo(i)
  ENDDO
  DO k          = 1,nz
   write(wunit,rec=irec)((tem2d1(i,j,k),i=1,nx),j=1,ny)
   irec         = irec + 1
  ENDDO
  tem2d1        = mis
  DO i          = 1,no
   tem2d1(int(olon(i)),int(olat(i)),int(olev(i))) = vo(i)
  ENDDO
  DO k          = 1,nz
   write(wunit,rec=irec)((tem2d1(i,j,k),i=1,nx),j=1,ny)
   irec         = irec + 1
  ENDDO
  tem2d1        = mis
  DO i          = 1,no
   tem2d1(int(olon(i)),int(olat(i)),int(olev(i))) = to(i)
  ENDDO
  DO k          = 1,nz
   write(wunit,rec=irec)((tem2d1(i,j,k),i=1,nx),j=1,ny)
   irec         = irec + 1
  ENDDO
  tem2d1        = mis
  DO i          = 1,no
   tem2d1(int(olon(i)),int(olat(i)),int(olev(i))) = qo(i)
  ENDDO
  DO k          = 1,nz
   write(wunit,rec=irec)((tem2d1(i,j,k),i=1,nx),j=1,ny)
   irec         = irec + 1
  ENDDO
  tem2d1        = mis
  DO i          = 1,no
   tem2d1(int(olon(i)),int(olat(i)),int(olev(i))) = po(i)
  ENDDO
  DO k          = 1,nz
   write(wunit,rec=irec)((tem2d1(i,j,k),i=1,nx),j=1,ny)
   irec         = irec + 1
  ENDDO   
  deallocate(tem2d1)
!
! non-dimensionalized all vars
!
  IF (nodim.eq.1) THEN
   uo           = uo/u_order
   vo           = vo/v_order
   to           = to/t_order
   qo           = qo/q_order
   po           = po/p_order
   err_u        = err_u/u_order
   err_v        = err_v/v_order
   err_t        = err_t/t_order
   err_q        = err_q/q_order
   err_p        = err_p/p_order
  ENDIF
  RETURN
  END SUBROUTINE get_obs

  SUBROUTINE get_mde(irec,debug,nx,ny,nz,nme)
  USE common_utils
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, PARAMETER :: n=4
  INTEGER            :: ncid,ocid,status
  INTEGER            :: ndims,nvars,ngatts,unlimdimid
  INTEGER            :: uid,vid,tid,qid,pid
  INTEGER            :: tndim,tdim(n)
  INTEGER            :: nx,ny,nz,nme,debug
  INTEGER            :: nx1,ny1,nz1,nx2,ny2,nz2,nt
  REAL, ALLOCATABLE  :: dump(:,:,:,:)
  INTEGER            :: i,j,k,ie,tem_len,irec
  REAL               :: rtime
  CHARACTER*100      :: ofile,tem,name
  ie                 = 1
8 continue
  rtime              = 0.
  ofile              = 'mde_000.dat'
  call name_string(ie,7,ofile)
  IF (debug.ge.1) THEN
   PRINT*,''
   PRINT*,'debug: opened background file is: ',ofile(1:30)
  ENDIF
  status = NF_OPEN(ofile(1:len_trim(ofile)),nf_nowrite,ncid)
  IF (debug.ge.1) THEN
   PRINT*,'1. NF_OPEN and return value: '
   PRINT*,'status     =',status
   PRINT*,'nf_noerr   =',nf_noerr
   PRINT*,'ncid       =',ncid
  ENDIF
!
! probing some dimension and structure of data fields
!
  IF (ie.eq.1) THEN
   status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
   PRINT*,''
   PRINT*,'2. NF_INQ returns values: '
   PRINT*,'ncid       =',ncid                                                ! file id
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
   status = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
   status = NF_INQ_VARNDIMS(ncid,tid,tndim)
   status = NF_INQ_VARDIMID(ncid,tid,tdim)
   IF (tndim.ne.n) THEN
    PRINT*,'Number of dim is /= n...allocation stop'
    status            = NF_CLOSE(ncid)
    STOP
   ENDIF
   PRINT*,''
   PRINT*,'3. Pull out information about: ',tem(1:tem_len)
   PRINT*,'tid        =',tid
   PRINT*,'tndim      =',tndim
   DO i               = 1,tndim
    j                 = tdim(i)
    status            = NF_INQ_DIMNAME(ncid,j,name)
    status            = NF_INQ_DIMLEN(ncid,j,k)
    IF (i.eq.1) nx2   = k
    IF (i.eq.2) ny2   = k
    IF (i.eq.3) nz2   = k
    IF (i.eq.4) nt    = k
     PRINT*,'Dimension name is:',i,tdim(i),name(1:20)
   ENDDO
   PRINT*,'nx2        =',nx2
   PRINT*,'ny2        =',ny2
   PRINT*,'nz2        =',nz2
   PRINT*,'nt         =',nt
   IF (nx.ne.nx2.or.ny.ne.ny2.or.nz.ne.nz2) THEN
    PRINT*,' letkf.exe: array dimension (nx,ny,nz) /= bgd files'
    PRINT*,' letkf.exe: check',nx,nx2,ny,ny2,nz,nz2
    STOP
   ENDIF
!
! define a mother domain with A-grid based on the dims of
! PB from C-grid, and locate all variable
!
   nx1                = nx2 + 1
   ny1                = ny2 + 1
   nz1                = nz2 + 1
  ENDIF
!
! reading data now
!
  ALLOCATE(dump(nx1,ny,nz,nt))
  tem          = 'U'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
  status       = NF_GET_VAR_REAL(ncid,uid,dump)
  ue(1:nx,:,:,ie) = 0.5*(dump(1:nx,:,:,1) + dump(2:nx1,:,:,1))
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny1,nz,nt))
  tem          = 'V'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
  status       = NF_GET_VAR_REAL(ncid,vid,dump)
  ve(:,1:ny,:,ie) = 0.5*(dump(:,1:ny,:,1) + dump(:,2:ny1,:,1))
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz,nt))
  tem          = 'T'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
  status       = NF_GET_VAR_REAL(ncid,tid,dump)
  te(:,:,:,ie) = dump(:,:,:,1) + tref
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz,nt))
  tem          = 'QVAPOR'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),qid)
  status       = NF_GET_VAR_REAL(ncid,qid,dump)
  qe(:,:,:,ie) = dump(:,:,:,1)
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz1,nt))
  tem          = 'PH'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
  status       = NF_GET_VAR_REAL(ncid,pid,dump)
  pe(:,:,1:nz,ie) = 0.5*(dump(:,:,1:nz,1)+dump(:,:,2:nz1,1))
  !pe(:,:,1:nz,ie) = dump(:,:,1:nz,1)
  DEALLOCATE(dump)  
!
! close the data file and loop all files
!
  status       = NF_CLOSE(ncid)
  IF (status.ne.0) PRINT*,'Can not close the new file'
  ie           = ie + 1
  IF (ie.le.nme) GOTO 8  
!
! nondimensionalized all vars
!
  IF (nodim.eq.1) THEN
   ue          = ue/u_order
   ve          = ve/v_order
   te          = te/t_order
   qe          = qe/q_order
   pe          = pe/p_order 
  ENDIF  
  RETURN
  END SUBROUTINE get_mde

  SUBROUTINE get_bgd(irec,debug,nx,ny,nz,ne)
  USE common_utils
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, PARAMETER :: n=4
  INTEGER            :: ncid,ocid,status
  INTEGER            :: ndims,nvars,ngatts,unlimdimid
  INTEGER            :: uid,vid,tid,qid,pid
  INTEGER            :: tndim,tdim(n)
  INTEGER            :: nx,ny,nz,ne,debug
  INTEGER            :: nx1,ny1,nz1,nx2,ny2,nz2,nt
  REAL, ALLOCATABLE  :: dump(:,:,:,:)
  INTEGER            :: i,j,k,ie,tem_len,irec
  REAL               :: rtime
  CHARACTER*100      :: ofile,tem,name
  ie                 = 1
8 continue
  rtime              = 0.
  ofile              = 'bgd_000.dat'
  call name_string(ie,7,ofile)
  IF (debug.ge.1) THEN
   PRINT*,''
   PRINT*,'debug: opened background file is: ',ofile(1:30)
  ENDIF
  status = NF_OPEN(ofile(1:len_trim(ofile)),nf_nowrite,ncid)
  IF (debug.ge.1) THEN
   PRINT*,'1. NF_OPEN and return value: '
   PRINT*,'status     =',status
   PRINT*,'nf_noerr   =',nf_noerr
   PRINT*,'ncid       =',ncid
  ENDIF
!
! probing some dimension and structure of data fields
!
  IF (ie.eq.1) THEN
   status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
   PRINT*,''
   PRINT*,'2. NF_INQ returns values: '
   PRINT*,'ncid       =',ncid                                                ! file id
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
   status = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
   status = NF_INQ_VARNDIMS(ncid,tid,tndim)
   status = NF_INQ_VARDIMID(ncid,tid,tdim)
   IF (tndim.ne.n) THEN
    PRINT*,'Number of dim is /= n...allocation stop'
    status            = NF_CLOSE(ncid)
    STOP
   ENDIF
   PRINT*,''
   PRINT*,'3. Pull out information about: ',tem(1:tem_len)
   PRINT*,'tid        =',tid
   PRINT*,'tndim      =',tndim
   DO i               = 1,tndim
    j                 = tdim(i)
    status            = NF_INQ_DIMNAME(ncid,j,name)
    status            = NF_INQ_DIMLEN(ncid,j,k)
    IF (i.eq.1) nx2   = k
    IF (i.eq.2) ny2   = k
    IF (i.eq.3) nz2   = k
    IF (i.eq.4) nt    = k
     PRINT*,'Dimension name is:',i,tdim(i),name(1:20)
   ENDDO
   PRINT*,'nx2        =',nx2
   PRINT*,'ny2        =',ny2
   PRINT*,'nz2        =',nz2
   PRINT*,'nt         =',nt
   IF (nx.ne.nx2.or.ny.ne.ny2.or.nz.ne.nz2) THEN
    PRINT*,' letkf.exe: array dimension (nx,ny,nz) /= bgd files'
    PRINT*,' letkf.exe: check',nx,nx2,ny,ny2,nz,nz2
    STOP
   ENDIF
!
! define a mother domain with A-grid based on the dims of
! PB from C-grid, and locate all variable
!
   nx1                = nx2 + 1
   ny1                = ny2 + 1
   nz1                = nz2 + 1
  ENDIF
!
! reading data now
!
  ALLOCATE(dump(nx1,ny,nz,nt))
  tem          = 'U'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
  status       = NF_GET_VAR_REAL(ncid,uid,dump)
  ub(1:nx,:,:,ie) = 0.5*(dump(1:nx,:,:,1) + dump(2:nx1,:,:,1))
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny1,nz,nt))
  tem          = 'V'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
  status       = NF_GET_VAR_REAL(ncid,vid,dump)
  vb(:,1:ny,:,ie) = 0.5*(dump(:,1:ny,:,1) + dump(:,2:ny1,:,1))
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz,nt))
  tem          = 'T'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
  status       = NF_GET_VAR_REAL(ncid,tid,dump)
  tb(1:nx,:,:,ie) = dump(:,:,:,1) + tref
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz,nt))
  tem          = 'QVAPOR'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),qid)
  status       = NF_GET_VAR_REAL(ncid,qid,dump)
  qb(1:nx,:,:,ie) = dump(:,:,:,1)
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz1,nt))
  tem          = 'PH'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
  status       = NF_GET_VAR_REAL(ncid,pid,dump)
  pb(:,:,:,ie) = 0.5*(dump(:,:,1:nz,1)+dump(:,:,2:nz1,1))
  !pb(:,:,1:nz,ie) = dump(:,:,1:nz,1)
  DEALLOCATE(dump)  
!
! close the data file and loop all files
!
  status       = NF_CLOSE(ncid)
  IF (status.ne.0) PRINT*,'Can not close the new file'
  ie           = ie + 1
  IF (ie.le.ne) GOTO 8
!
! PRINT* out for checking of ensemble mean of all background member 
!
  ALLOCATE(dump(nx,ny,nz,1))
  dump(:,:,:,1)= 0.
  do ie        = 1,ne
   dump(:,:,:,1) = dump(:,:,:,1) + ub(:,:,:,ie)
  enddo
  dump(:,:,:,1)= dump(:,:,:,1)/ne
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((dump(i,j,k,1),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO

  dump(:,:,:,1)= 0.
  do ie        = 1,ne
   dump(:,:,:,1) = dump(:,:,:,1) + vb(:,:,:,ie)
  enddo
  dump(:,:,:,1)= dump(:,:,:,1)/ne
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((dump(i,j,k,1),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO

  dump(:,:,:,1)= 0.
  do ie        = 1,ne
   dump(:,:,:,1) = dump(:,:,:,1) + tb(:,:,:,ie)
  enddo
  dump(:,:,:,1)= dump(:,:,:,1)/ne
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((dump(i,j,k,1),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO

  dump(:,:,:,1)= 0.
  do ie        = 1,ne
   dump(:,:,:,1) = dump(:,:,:,1) + qb(:,:,:,ie)
  enddo
  dump(:,:,:,1)= dump(:,:,:,1)/ne
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((dump(i,j,k,1),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO

  dump(:,:,:,1)= 0.
  do ie        = 1,ne
   dump(:,:,:,1) = dump(:,:,:,1) + pb(:,:,:,ie)
  enddo
  dump(:,:,:,1)= dump(:,:,:,1)/ne
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((dump(i,j,k,1),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO  
  DEALLOCATE(dump)
!
! nondimensionalized all vars
!
  IF (nodim.eq.1) THEN
   ub          = ub/u_order
   vb          = vb/v_order
   tb          = tb/t_order
   qb          = qb/q_order
   pb          = pb/p_order 
  ENDIF 
  RETURN
  END SUBROUTINE get_bgd
  
  SUBROUTINE put_mde(irec,debug,nx,ny,nz,nme)
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER            :: nx,ny,nz,nme,no,debug
  INTEGER            :: i,j,k,irec
!
! go back to dimensional vars
!
  IF (nodim.eq.1) THEN
   ume               = sqrt(ume)*u_order
   vme               = sqrt(vme)*v_order
   tme               = sqrt(tme)*t_order
   qme               = sqrt(qme)*q_order
   pme               = sqrt(pme)*p_order
  ENDIF
!
! print
!  
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((ume(i,j,k),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((vme(i,j,k),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((tme(i,j,k),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((qme(i,j,k),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((pme(i,j,k),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO  
  RETURN
  END SUBROUTINE put_mde

  SUBROUTINE put_ana(irec,debug,nx,ny,nz,ne,no,nxl,obs_flag,mean_flag,da_flag)
  USE common_utils
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, PARAMETER :: n=4
  INTEGER            :: ncid,ocid,status,obs_flag,mean_flag
  INTEGER            :: ndims,nvars,ngatts,unlimdimid
  INTEGER            :: uid,vid,tid,qid,pid
  INTEGER            :: tndim,tdim(n),im,jm,km,nxl
  INTEGER            :: nx,ny,nz,ne,no,debug,da_flag
  INTEGER            :: nx1,ny1,nz1,nx2,ny2,nz2,nt
  REAL, ALLOCATABLE  :: dump(:,:,:,:),dump1(:,:,:)
  INTEGER            :: i,j,k,ie,tem_len,irec,npass
  INTEGER            :: i1,j1,i_s,i_e,j_s,j_e
  REAL               :: rtime
  CHARACTER*100      :: ofile,tem,name
  npass              = obs_flag
!
! go back to dimensional vars
!
  IF (nodim.eq.1) THEN
   ua                = ua*u_order
   va                = va*v_order
   ta                = ta*t_order
   qa                = qa*q_order
   pa                = pa*p_order
  ENDIF
  if (mean_flag.eq.1) then
    print*,'letkf_main: not yet support mean_flag=1'
    stop
  endif
!
! get header information next
! 
  ie                 = 1
14 CONTINUE
  rtime              = 0.
  ofile              = 'ana_000.dat'
  call name_string(ie,7,ofile)
  IF (debug.ge.1) THEN
   PRINT*,''
   PRINT*,'debug: opened background file is: ',ofile(1:30)
  ENDIF
  status = NF_OPEN(ofile(1:len_trim(ofile)),nf_write,ncid)
  IF (debug.ge.1) THEN
   PRINT*,'1. NF_OPEN and return value: '
   PRINT*,'status     =',status
   PRINT*,'nf_noerr   =',nf_noerr
   PRINT*,'ncid       =',ncid
  ENDIF
!
! probing some dimension and structure of data fields
!
  IF (ie.eq.1) THEN
   status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
   PRINT*,''
   PRINT*,'2. NF_INQ returns values: '
   PRINT*,'ncid       =',ncid                                                ! file id
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
   status = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
   status = NF_INQ_VARNDIMS(ncid,tid,tndim)
   status = NF_INQ_VARDIMID(ncid,tid,tdim)
   IF (tndim.ne.n) THEN
    PRINT*,'Number of dim is /= n...allocation stop'
    status            = NF_CLOSE(ncid)
    STOP
   ENDIF
   PRINT*,''
   PRINT*,'3. Pull out information about: ',tem(1:tem_len)
   PRINT*,'tid        =',tid
   PRINT*,'tndim      =',tndim
   DO i               = 1,tndim
    j                 = tdim(i)
    status            = NF_INQ_DIMNAME(ncid,j,name)
    status            = NF_INQ_DIMLEN(ncid,j,k)
    IF (i.eq.1) nx2   = k
    IF (i.eq.2) ny2   = k
    IF (i.eq.3) nz2   = k
    IF (i.eq.4) nt    = k
    PRINT*,'Dimension name is:',i,tdim(i),name(1:20)
   ENDDO
   PRINT*,'nx2        =',nx2
   PRINT*,'ny2        =',ny2
   PRINT*,'nz2        =',nz2
   PRINT*,'nt         =',nt
   IF (nx.ne.nx2.or.ny.ne.ny2.or.nz.ne.nz2) THEN
    PRINT*,' letkf.exe: array dimension (nx,ny,nz) /= bgd files'
    STOP
   ENDIF
!
! define a mother domain with A-grid based on the dims of
! P from C-grid, and locate all variable
!
   nx1                = nx2 + 1
   ny1                = ny2 + 1
   nz1                = nz2 + 1
  ENDIF
!
! reading data then
!
  ALLOCATE(dump(nx1,ny,nz,nt),dump1(nx1,ny,nz))
  tem          = 'U'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
  status       = NF_GET_VAR_REAL(ncid,uid,dump)
  dump1(:,:,:) = dump(:,:,:,1)
  DO i         = 1,no
   im          = nint(olon(i))
   jm          = nint(olat(i))
   km          = nint(olev(i))
   i_s         = max(im - nxl,2)
   i_e         = min(im + nxl,nx-1)
   j_s         = max(jm - nxl,2)
   j_e         = min(jm + nxl,ny-1)
   DO i1       = i_s,i_e
    DO j1      = j_s,j_e 
     dump1(i1,j1,km)   = 0.5*(ua(i1-1,j1,km,ie) + ua(i1,j1,km,ie))
    ENDDO
   ENDDO
  ENDDO
  !i_s         = 1
  !i_e         = 2*nxl
  !call smooth9p(dump1,nx1,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !i_s         = nx1-2*nxl
  !i_e         = nx1
  !call smooth9p(dump1,nx1,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !j_s         = 1
  !j_e         = 2*nxl
  !call smooth9p(dump1,nx1,ny,nz,1,nx1,j_s,j_e,1,nz,npass)
  !j_s         = ny-2*nxl
  !j_e         = ny
  !call smooth9p(dump1,nx1,ny,nz,1,nx1,j_s,j_e,1,nz,npass)
  if (npass.gt.1) then
   print*,' LETKF: smoothing U with npass = ',obs_flag
   call smooth9p(dump1,nx1,ny,nz,1,nx1,1,ny,1,nz,npass)
  endif
  dump(:,:,:,1)= dump1(:,:,:)
  ua(:,:,:,ie) = dump1(:,:,:)
  status       = NF_PUT_VAR_REAL(ncid,uid,dump)
  DEALLOCATE(dump,dump1)
  IF (debug.ge.1) PRINT*,'debug: put var status',status
!  
  ALLOCATE(dump(nx,ny1,nz,nt),dump1(nx,ny1,nz))
  tem          = 'V'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
  status       = NF_GET_VAR_REAL(ncid,vid,dump)
  dump1(:,:,:) = dump(:,:,:,1)
  DO i         = 1,no
   im          = nint(olon(i))
   jm          = nint(olat(i))
   km          = nint(olev(i))
   i_s         = max(im - nxl,2)
   i_e         = min(im + nxl,nx-1)
   j_s         = max(jm - nxl,2)
   j_e         = min(jm + nxl,ny-1)
   DO i1       = i_s,i_e
    DO j1      = j_s,j_e
     dump1(i1,j1,km)   = 0.5*(va(i1,j1-1,km,ie) + va(i1,j1,km,ie))
    ENDDO
   ENDDO
  ENDDO
  !i_s         = 1
  !i_e         = 2*nxl
  !call smooth9p(dump1,nx,ny1,nz,i_s,i_e,1,ny1,1,nz,npass)
  !i_s         = nx-2*nxl
  !i_e         = nx
  !call smooth9p(dump1,nx,ny1,nz,i_s,i_e,1,ny1,1,nz,npass)
  !j_s         = 1
  !j_e         = 2*nxl
  !call smooth9p(dump1,nx,ny1,nz,1,nx,j_s,j_e,1,nz,npass)
  !j_s         = ny1-2*nxl
  !j_e         = ny1
  !call smooth9p(dump1,nx,ny1,nz,1,nx,j_s,j_e,1,nz,npass)
  if (npass.gt.1) then
   print*,' LETKF: smooth V with npass =',npass
   call smooth9p(dump1,nx,ny1,nz,1,nx,1,ny1,1,nz,npass)
  endif
  dump(:,:,:,1)= dump1(:,:,:)  
  va(:,:,:,ie) = dump1(:,:,:)
  status       = NF_PUT_VAR_REAL(ncid,vid,dump)
  DEALLOCATE(dump,dump1)  
  IF (debug.ge.1) PRINT*,'debug: put var status',status
!  
  if (da_flag.ne.345.or.da_flag.ne.45) then
  ALLOCATE(dump(nx,ny,nz,nt),dump1(nx,ny,nz))
  tem          = 'T'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
  status       = NF_GET_VAR_REAL(ncid,tid,dump)
  dump1(:,:,:) = dump(:,:,:,1)
  DO i         = 1,no
   im          = nint(olon(i))
   jm          = nint(olat(i))
   km          = nint(olev(i))
   i_s         = max(im - nxl,2)
   i_e         = min(im + nxl,nx-1)
   j_s         = max(jm - nxl,2)
   j_e         = min(jm + nxl,ny-1)
    DO i1      = i_s,i_e
     DO j1     = j_s,j_e
      dump1(i1,j1,km)   = ta(i1,j1,km,ie) - tref
     ENDDO
    ENDDO
  ENDDO
  !i_s         = 1
  !i_e         = 2*nxl
  !call smooth9p(dump1,nx,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !i_s         = nx-2*nxl
  !i_e         = nx
  !call smooth9p(dump1,nx,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !j_s         = 1
  !j_e         = 2*nxl
  !call smooth9p(dump1,nx,ny,nz,1,nx,j_s,j_e,1,nz,npass)
  !j_s         = ny-2*nxl
  !j_e         = ny
  !call smooth9p(dump1,nx,ny,nz,1,nx,j_s,j_e,1,nz,npass)
  if (npass.gt.1) then
   print*,' LETKF: smoothing T with nass = ',obs_flag
   call smooth9p(dump1,nx,ny,nz,1,nx,1,ny,1,nz,10)
  endif
  dump(:,:,:,1)= dump1(:,:,:)
  ta(:,:,:,ie) = dump1(:,:,:) + tref
  status       = NF_PUT_VAR_REAL(ncid,tid,dump)
  DEALLOCATE(dump,dump1)
  IF (debug.ge.1) PRINT*,'debug: put var status',status
  endif
!
  if (da_flag.ne.345.or.da_flag.ne.45) then
  ALLOCATE(dump(nx,ny,nz,nt),dump1(nx,ny,nz))
  tem          = 'QVAPOR'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),qid)
  status       = NF_GET_VAR_REAL(ncid,qid,dump)
  dump1(:,:,:) = dump(:,:,:,1)
  DO i         = 1,no
   im          = nint(olon(i))
   jm          = nint(olat(i))
   km          = nint(olev(i))
   i_s         = max(im - nxl,2)
   i_e         = min(im + nxl,nx-1)
   j_s         = max(jm - nxl,2)
   j_e         = min(jm + nxl,ny-1)
   DO i1       = i_s,i_e
    DO j1      = j_s,j_e
     dump1(i1,j1,km)   = qa(i1,j1,km,ie)
    ENDDO
   ENDDO
  ENDDO
  !i_s         = 1
  !i_e         = 2*nxl
  !call smooth9p(dump1,nx,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !i_s         = nx-2*nxl
  !i_e         = nx
  !call smooth9p(dump1,nx,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !j_s         = 1
  !j_e         = 2*nxl
  !call smooth9p(dump1,nx,ny,nz,1,nx,j_s,j_e,1,nz,npass)
  !j_s         = ny-2*nxl
  !j_e         = ny
  !call smooth9p(dump1,nx,ny,nz,1,nx,j_s,j_e,1,nz,npass)
  if (npass.gt.1) then
    print*,' LETKF: smoothing Q with nass = ',obs_flag
    call smooth9p(dump1,nx,ny,nz,1,nx,1,ny,1,nz,10)
  endif
  dump(:,:,:,1)= dump1(:,:,:) 
  qa(:,:,:,ie) = dump1(:,:,:) 
  status       = NF_PUT_VAR_REAL(ncid,qid,dump)
  DEALLOCATE(dump,dump1)
  IF (debug.ge.1) PRINT*,'debug: put var status',status
  endif
!
  if (da_flag.ne.345.or.da_flag.ne.45) then
  ALLOCATE(dump(nx,ny,nz1,nt),dump1(nx,ny,nz1))
  tem          = 'PH'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
  status       = NF_GET_VAR_REAL(ncid,pid,dump)
  dump1(:,:,:) = dump(:,:,:,1)
  DO i         = 1,no
   im          = nint(olon(i))
   jm          = nint(olat(i))
   km          = nint(olev(i))
   i_s         = max(im - nxl,2)
   i_e         = min(im + nxl,nx-1)
   j_s         = max(jm - nxl,2)
   j_e         = min(jm + nxl,ny-1)   
   IF (km.gt.1.and.km.lt.nz1) THEN
    DO i1       = i_s,i_e
     DO j1      = j_s,j_e
      dump1(im,jm,km) = 0.5*(pa(im,jm,km-1,ie)+pa(im,jm,km+1,ie)) 
      !dump1(im,jm,km) = pa(im,jm,km,ie)
     ENDDO
    ENDDO
   ENDIF 
  ENDDO
  !i_s         = 1
  !i_e         = 2*nxl
  !call smooth9p(dump1,nx,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !i_s         = nx-2*nxl
  !i_e         = nx
  !call smooth9p(dump1,nx,ny,nz,i_s,i_e,1,ny,1,nz,npass)
  !j_s         = 1
  !j_e         = 2*nxl
  !call smooth9p(dump1,nx,ny,nz,1,nx,j_s,j_e,1,nz,npass)
  !j_s         = ny-2*nxl
  !j_e         = ny
  !call smooth9p(dump1,nx,ny,nz,1,nx,j_s,j_e,1,nz,npass)
  if (npass.gt.1) then
    print*,' LETKF: smoothing PH with nass = ',obs_flag
    call smooth9p(dump1,nx,ny,nz1,1,nx,1,ny,1,nz,10)
  endif
  dump(:,:,:,1)= dump1(:,:,:)  
  pa(:,:,:,ie) = dump1(:,:,:) ! Note that pa has to be unstagged as pb
  status       = NF_PUT_VAR_REAL(ncid,pid,dump)
  DEALLOCATE(dump,dump1)  
  IF (debug.ge.1) PRINT*,'debug: put var status',status
  endif
!
! close the data file and loop all files
!
  status       = NF_CLOSE(ncid)
  IF (status.ne.0) PRINT*,'Can not close the new file'
  ie           = ie + 1
  IF (ie.le.ne) GOTO 14
!
! print out now for checking the center member
!
  ie           = max(1,ne/2)
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((ua(i,j,k,ie),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((va(i,j,k,ie),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((ta(i,j,k,ie),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((qa(i,j,k,ie),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO
  DO k         = 1,nz
   WRITE(wunit,rec=irec)((pa(i,j,k,ie),i=1,nx),j=1,ny)
   irec        = irec + 1
  ENDDO  
  RETURN
  END SUBROUTINE put_ana

END MODULE common_io


!
! [PURPOSE]:   Create a restart wrfinput_d01 for the idealize
!              run. This will help the background to not spread
!              too much every assimilating cycles.
!
! [AUTHOR]:    Chanh Q. Kieu, Lecturer
!              Department of Atmospheric and Oceanic Science
!              Vietnam National University, Hanoi, Vietnam
!              Email: kieucq@atmos.umd.edu
!
! [HISTORY]: - 30 June 2010: Created
!
  PROGRAM letkf_restart
  USE common_utils
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER, PARAMETER :: n=4
  INTEGER            :: ncid,ocid,status
  INTEGER            :: ndims,nvars,ngatts,unlimdimid
  INTEGER            :: uid,vid,tid,qid,pid
  INTEGER            :: tndim,tdim(n)
  INTEGER            :: nx,ny,nz,ne,debug
  INTEGER            :: nx1,ny1,nz1,nt
  REAL, ALLOCATABLE  :: dump(:,:,:,:),ub(:,:,:,:),vb(:,:,:,:)
  REAL, ALLOCATABLE  :: tb(:,:,:,:),qb(:,:,:,:),pb(:,:,:,:)
  REAL, ALLOCATABLE  :: dump1(:,:,:)
  INTEGER            :: i,j,k,ie,tem_len,irec
  CHARACTER*100      :: ofile,tem,name
  call input_namelist(debug,ne) 
  ie                 = 1
8 continue
  ofile              = 'ana_000.dat'
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
   PRINT*,'ne         =',ne
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
   nx1                = nx + 1
   ny1                = ny + 1
   nz1                = nz + 1
   ALLOCATE(ub(nx1,ny,nz,ne),vb(nx,ny1,nz,ne),tb(nx,ny,nz,ne))
   ALLOCATE(qb(nx,ny,nz,ne),pb(nx,ny,nz1,ne))
  ENDIF
!
! reading data now
!
  ALLOCATE(dump(nx1,ny,nz,nt))
  tem          = 'U'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
  status       = NF_GET_VAR_REAL(ncid,uid,dump)
  ub(:,:,:,ie) = dump(:,:,:,1)
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny1,nz,nt))
  tem          = 'V'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
  status       = NF_GET_VAR_REAL(ncid,vid,dump)
  vb(:,:,:,ie) = dump(:,:,:,1)
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz,nt))
  tem          = 'T'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
  status       = NF_GET_VAR_REAL(ncid,tid,dump)
  tb(:,:,:,ie) = dump(:,:,:,1)
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz,nt))
  tem          = 'QVAPOR'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),qid)
  status       = NF_GET_VAR_REAL(ncid,qid,dump)
  qb(:,:,:,ie) = dump(:,:,:,1)
  DEALLOCATE(dump)
  ALLOCATE(dump(nx,ny,nz1,nt))
  tem          = 'PH'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
  status       = NF_GET_VAR_REAL(ncid,pid,dump)
  pb(:,:,:,ie) = dump(:,:,:,1)
  DEALLOCATE(dump)  
!
! close the data file and loop all files
!
  status       = NF_CLOSE(ncid)
  IF (status.ne.0) PRINT*,'Can not close the new file'
  ie           = ie + 1
  IF (ie.le.ne) GOTO 8
!
! replace the background for each member by ensemble mean of
! all members. For the real time run, such ensemble mean is
! trivial because the wrfinput_d01 is copied multiply for each
! member. For the idealized run, the background mean will help
! reduce the spread of the members.
!
  ALLOCATE(dump1(nx1,ny,nz))
  dump1              = 0.
  DO ie              = 1,ne
   dump1(:,:,:)      = dump1(:,:,:) + ub(:,:,:,ie)
  ENDDO
  dump1              = dump1/ne
  DO ie              = 1,ne
   ub(:,:,:,ie)      = dump1(:,:,:)
  ENDDO
  DEALLOCATE(dump1)
!
  ALLOCATE(dump1(nx,ny1,nz))
  dump1              = 0.
  DO ie              = 1,ne
   dump1(:,:,:)      = dump1(:,:,:) + vb(:,:,:,ie)
  ENDDO
  dump1              = dump1/ne
  DO ie              = 1,ne
   vb(:,:,:,ie)      = dump1(:,:,:)
  ENDDO
  DEALLOCATE(dump1)
!
  ALLOCATE(dump1(nx,ny,nz))
  dump1              = 0.
  DO ie              = 1,ne
   dump1(:,:,:)      = dump1(:,:,:) + tb(:,:,:,ie)
  ENDDO
  dump1              = dump1/ne
  DO ie              = 1,ne
   tb(:,:,:,ie)      = dump1(:,:,:)
  ENDDO
  DEALLOCATE(dump1)
!
  ALLOCATE(dump1(nx,ny,nz))
  dump1              = 0.
  DO ie              = 1,ne
   dump1(:,:,:)      = dump1(:,:,:) + qb(:,:,:,ie)
  ENDDO
  dump1              = dump1/ne
  DO ie              = 1,ne
   qb(:,:,:,ie)      = dump1(:,:,:)
  ENDDO
  DEALLOCATE(dump1)
!
  ALLOCATE(dump1(nx,ny,nz1))
  dump1              = 0.
  DO ie              = 1,ne
   dump1(:,:,:)      = dump1(:,:,:) + pb(:,:,:,ie)
  ENDDO
  dump1              = dump1/ne
  DO ie              = 1,ne
   pb(:,:,:,ie)      = dump1(:,:,:)
  ENDDO
  DEALLOCATE(dump1)
!
! put the data back now
!
  ie                 = 1
14 CONTINUE
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
  ALLOCATE(dump(nx1,ny,nz,nt))
  tem          = 'U'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
  dump(:,:,:,1)= ub(:,:,:,ie)
  status       = NF_PUT_VAR_REAL(ncid,uid,dump)
  DEALLOCATE(dump)
  IF (debug.ge.1) PRINT*,'debug: put var status',status
!
  ALLOCATE(dump(nx,ny1,nz,nt))
  tem          = 'V'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
  dump(:,:,:,1)= vb(:,:,:,ie)
  status       = NF_PUT_VAR_REAL(ncid,vid,dump)
  DEALLOCATE(dump)
  IF (debug.ge.1) PRINT*,'debug: put var status',status  
!
  ALLOCATE(dump(nx,ny,nz,nt))
  tem          = 'T'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
  dump(:,:,:,1)= tb(:,:,:,ie)
  status       = NF_PUT_VAR_REAL(ncid,tid,dump)
  DEALLOCATE(dump)
  IF (debug.ge.1) PRINT*,'debug: put var status',status
!
  ALLOCATE(dump(nx1,ny,nz,nt))
  tem          = 'QVAPOR'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),qid)
  dump(:,:,:,1)= qb(:,:,:,ie)
  status       = NF_PUT_VAR_REAL(ncid,qid,dump)
  DEALLOCATE(dump)
  IF (debug.ge.1) PRINT*,'debug: put var status',status  
!
  ALLOCATE(dump(nx,ny,nz1,nt))
  tem          = 'PH'
  tem_len      = len_trim(tem)
  status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
  dump(:,:,:,1)= pb(:,:,:,ie)
  status       = NF_PUT_VAR_REAL(ncid,pid,dump)
  DEALLOCATE(dump)
  IF (debug.ge.1) PRINT*,'debug: put var status',status  
!
! close the data file and loop all files
!
  status       = NF_CLOSE(ncid)
  IF (status.ne.0) PRINT*,'Can not close the new file'
  ie           = ie + 1
  IF (ie.le.ne) GOTO 14  
  END

  SUBROUTINE  input_namelist(debug,ne)
  INCLUDE "../Registry/letkf.inc"
  PRINT*,'letkf_restart.exe: input namelist is'
  PRINT*,'letkf_restart.exe: ne =',ne
  RETURN
  END



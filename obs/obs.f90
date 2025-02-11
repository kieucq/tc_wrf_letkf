!
! [NOTE]:      This program is to read all truth forecast and prinout
!              in the grads format for viewing 
!
! [HISTORY]: - Mar 18, 2010: Created by CK
!            - Apl 18, 2010: add more var and rearrange the output to
!                            fit with souding style
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
      PROGRAM creating_obs_from_truth
      USE common_utils
      IMPLICIT none
      INCLUDE 'netcdf.inc' 
      INTEGER, PARAMETER :: n=4
      INTEGER            :: ncid,ocid,status
      INTEGER            :: ndims,nvars,ngatts,unlimdimid
      INTEGER            :: uid,vid,tid,qid,pid
      INTEGER            :: tndim,tdim(n)
      INTEGER            :: nx,ny,nz,nt,nx1,ny1,nz1,nt1
      INTEGER            :: iday,ihour,imin 
      REAL               :: umax,vmax,tmax,rmax,r_obs  
      REAL               :: obs_err_u,obs_err_v,obs_err_t,obs_err_q,obs_err_p
      REAL               :: err_u,err_v,err_t,err_p,err_q
      REAL               :: rtime,dt,tfcst,restart,mis,tref,coef
      INTEGER            :: obs_flag,ne,itime,ntime
      INTEGER            :: id,jd,no,icen,jcen
      REAL,ALLOCATABLE   :: u(:,:,:,:),v(:,:,:,:),t(:,:,:,:),q(:,:,:,:),p(:,:,:,:)
      REAL,ALLOCATABLE   :: u1(:,:,:,:),v1(:,:,:,:),t1(:,:,:,:),q1(:,:,:,:),p1(:,:,:,:)
      REAL,ALLOCATABLE   :: ur(:,:,:),vr(:,:,:),tr(:,:,:),qr(:,:,:),pr(:,:,:)
      REAL(8),ALLOCATABLE :: rnd(:)
      CHARACTER*100      :: name,ofile,rfile,tem,wfile
      INTEGER            :: i,j,k,tem_len,debug,nfile,ifile,irec
      INTEGER            :: ntruncate
      irec               = 1 
      mis                = -99999
      tref               = 300.
      ntruncate          = 9
      coef               = 0
!
! reading namelist
!
      CALL input_namelist(debug,tfcst,restart,obs_err_u,obs_err_v,obs_err_t, &
                          obs_err_q,obs_err_p,no,ne,obs_flag,r_obs,icen,jcen)
      PRINT*,'obs.exe: restart        =',restart
      PRINT*,'obs.exe: tfcst          =',tfcst
      PRINT*,'obs.exe: obs_error_u is: ',obs_err_u
      PRINT*,'obs.exe: obs_error_v is: ',obs_err_v
      PRINT*,'obs.exe: obs_error_t is: ',obs_err_t
      PRINT*,'obs.exe: obs_error_q is: ',obs_err_q
      PRINT*,'obs.exe: obs_error_q is: ',obs_err_p
      PRINT*,'obs.exe: number of ensemble is: ',ne
      PRINT*,'obs.exe: number of obs is: ',no
      PRINT*,'obs.exe: obs_flag: ',obs_flag
      PRINT*,'obs.exe: r_obs is: ',r_obs
      PRINT*,'obs.exe: icen is: ',icen
      PRINT*,'obs.exe: jcen is: ',jcen
      restart=restart/60.
      IF (debug.eq.1) read*
!
! open data file now
!
      ifile              = 1
      ofile              = 'truth.dat'
      rfile              = 'obsout.dat'
      IF (debug.eq.1) then
       PRINT*,'obs.exe: opened truth file is: ',ofile(1:30)
       PRINT*,'obs.exe: opened obs file is: ',rfile(1:30)
      ENDIF
      status = NF_OPEN(ofile(1:len_trim(ofile)),nf_nowrite,ncid)
      IF (debug.ge.1) then
       PRINT*,''
       PRINT*,'1. NF_OPEN and return value: '
       PRINT*,'status    =',status
       PRINT*,'nf_noerr  =',nf_noerr 
       PRINT*,'ncid      =',ncid
      ENDIF
!
! probing some dimension and structure of data fields
!
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
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
      nx1          = nx + 1
      ny1          = ny + 1
      nz1          = nz + 1
      ALLOCATE(u(nx1,ny,nz,nt),v(nx,ny1,nz,nt),t(nx,ny,nz,nt),q(nx,ny,nz,nt),p(nx,ny,nz1,nt))
      ALLOCATE(u1(nx,ny,nz,nt),v1(nx,ny,nz,nt),t1(nx,ny,nz,nt),q1(nx,ny,nz,nt),p1(nx,ny,nz,nt)) 
      ALLOCATE(ur(nx1,ny,nz),vr(nx,ny1,nz),tr(nx,ny,nz),qr(nx,ny,nz),pr(nx,ny,nz1))
!
! reading data now
!
      tem          = 'U'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(ncid,uid,u)
      tem          = 'V'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(ncid,vid,v)
      tem          = 'T'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
      status       = NF_GET_VAR_REAL(ncid,tid,t)
      tem          = 'QVAPOR'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),qid)
      status       = NF_GET_VAR_REAL(ncid,qid,q)
      tem          = 'P'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,p)
!
! generate random perturbation now
!
      IF (debug.eq.1) PRINT*,'obs.exe: calling random function'
      ALLOCATE(rnd(nx1*ny*nz))
      CALL com_randn(nx1*ny*nz,rnd)
      ur           = coef*RESHAPE(rnd,(/nx1,ny,nz/))*obs_err_u
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny1*nz))
      CALL com_randn(nx*ny1*nz,rnd)
      vr           = coef*RESHAPE(rnd,(/nx,ny1,nz/))*obs_err_v
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny*nz))
      CALL com_randn(nx*ny*nz,rnd)
      tr           = coef*RESHAPE(rnd,(/nx,ny,nz/))*obs_err_t
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny*nz))
      CALL com_randn(nx*ny*nz,rnd)
      qr           = coef*RESHAPE(rnd,(/nx,ny,nz/))*obs_err_q
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny*nz1))
      CALL com_randn(nx*ny*nz1,rnd)
      pr           = coef*RESHAPE(rnd,(/nx,ny,nz1/))*obs_err_p
      DEALLOCATE(rnd)
!
! write out all the data now
!
      u1(:,:,:,1)  = mis
      v1(:,:,:,1)  = mis
      t1(:,:,:,1)  = mis
      q1(:,:,:,1)  = mis
      p1(:,:,:,1)  = mis
      OPEN(72,file=rfile,status='unknown')
      IF (obs_flag.eq.0) THEN
!
! obs are distributed every r_obs values in (x,y) direction
!              
       WRITE(72,*)((nx-1)/nint(r_obs)+1)*((ny-1)/nint(r_obs)+1)*(nz-ntruncate),  &
                  "lon    lat    lev    u    v    t    qv     pp"
       id           = 0
       DO i         = 1,nx,nint(r_obs)
        DO j        = 1,ny,nint(r_obs)
         DO k       = 1,nz-ntruncate
          id        = id + 1
          u1(i,j,k,1)  = 0.5*(u(i,j,k,1) + ur(i,j,k) + u(i+1,j,k,1) + ur(i+1,j,k))
          v1(i,j,k,1)  = 0.5*(v(i,j,k,1) + vr(i,j,k) + v(i,j+1,k,1) + vr(i,j+1,k))
          t1(i,j,k,1)  = t(i,j,k,1) + tr(i,j,k) + tref
          q1(i,j,k,1)  = q(i,j,k,1) + qr(i,j,k)
          p1(i,j,k,1)  = 0.5*(p(i,j,k,1) + pr(i,j,k) + p(i,j,k+1,1) + pr(i,j,k+1))
          call compute_err_z(obs_err_u,obs_err_v,obs_err_t,obs_err_q,obs_err_p,                        &
                             k,nz,err_u,err_v,err_t,err_q,err_p)
          WRITE(72,'(2F12.4,1I5,10E12.4)')float(i),float(j),k,u1(i,j,k,1),err_u,v1(i,j,k,1),err_v,     &
                             t1(i,j,k,1),err_t,abs(q1(i,j,k,1)),err_q,p1(i,j,k,1),err_p  
         ENDDO
        ENDDO
       ENDDO
       IF (debug.eq.1) PRINT*,' OBS: number of obs is',id/40,((nx-1)/nint(r_obs)+1)*((ny-1)/nint(r_obs)+1)
      ELSE        
!
! obs are distributed around (icen,jcen) with a radius r_obs
!              
       WRITE(72,*)nint((2*r_obs+1)*(2*r_obs+1)*(nz-ntruncate)), &
                  "lon    lat    lev    u    v    t    qv     pp"    
       id           = 0
       DO i         = icen-int(r_obs),icen+nint(r_obs)
        DO j        = jcen-int(r_obs),jcen+nint(r_obs)      
         DO k       = 1,nz-ntruncate
          id        = id + 1
          u1(i,j,k,1)  = 0.5*(u(i,j,k,1) + ur(i,j,k) + u(i+1,j,k,1) + ur(i+1,j,k)) 
          v1(i,j,k,1)  = 0.5*(v(i,j,k,1) + vr(i,j,k) + v(i,j+1,k,1) + vr(i,j+1,k))
          t1(i,j,k,1)  = t(i,j,k,1) + tr(i,j,k) + tref
          q1(i,j,k,1)  = q(i,j,k,1) + qr(i,j,k)
          p1(i,j,k,1)  = 0.5*(p(i,j,k,1) + pr(i,j,k) + p(i,j,k+1,1) + pr(i,j,k+1)) 
          call compute_err_z(obs_err_u,obs_err_v,obs_err_t,obs_err_q,obs_err_p,                         &
                             k,nz,err_u,err_v,err_t,err_q,err_p)          
          WRITE(72,'(2F12.4,1I5,10E12.4)')float(i),float(j),k,u1(i,j,k,1),err_u,v1(i,j,k,1),err_v,      &
                             t1(i,j,k,1),err_t,abs(q1(i,j,k,1)),err_q,p1(i,j,k,1),err_p
         ENDDO
        ENDDO
       ENDDO  
!       IF (id.ne.no) THEN
!        PRINT*,'obs.exe: cross check of no and r_obs doesnot fit...stop',id,no
!        status      = NF_CLOSE(ncid)
!        STOP
!       ENDIF
      ENDIF 
      CLOSE(72)
!
! PRINT out now
!
      open(99,file='obs.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*4)
      DO k         = 1,nz
       WRITE(99,rec=irec)((0.5*(u(i,j,k,1)+u(i+1,j,k,1)),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((0.5*(v(i,j,k,1)+v(i,j+1,k,1)),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((t(i,j,k,1),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((q(i,j,k,1),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO 
      DO k         = 1,nz
       WRITE(99,rec=irec)((0.5*(p(i,j,k,1)+p(i,j,k+1,1)),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO       
      DO k         = 1,nz
       WRITE(99,rec=irec)((u1(i,j,k,1),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((v1(i,j,k,1),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((t1(i,j,k,1),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO
      DO k         = 1,nz
       WRITE(99,rec=irec)((q1(i,j,k,1),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO 
      DO k         = 1,nz
       WRITE(99,rec=irec)((p1(i,j,k,1),i=1,nx),j=1,ny)
       irec        = irec + 1
      ENDDO 
      close(99)
!
! close the data file and loop all files
!
      status       = NF_CLOSE(ncid)
      IF (status.ne.0) PRINT*,'Can not close the new file'
!
! close the data file now
!
      CALL error_handle(99999)
      END

      SUBROUTINE com_randn(ndim,var)
      USE mt19937
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ndim
      REAL(8),INTENT(OUT) :: var(1:ndim)
      REAL(8) :: rnd(2),pi
      INTEGER :: idate(8)
      INTEGER :: i,iseed
      LOGICAL,SAVE :: first=.true.
      pi = 4.*atan(1.)
      IF (first) THEN
        CALL DATE_AND_TIME(VALUES=idate)
        iseed = idate(8) + idate(7)*1000
        CALL init_genrand(iseed)
        first=.false.
      ENDIF

      IF (MOD(ndim,2)==0 ) THEN
        DO i=1,ndim/2
          rnd(1) = genrand_res53()
          rnd(2) = genrand_res53()
          var(i*2-1) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
          var(i*2) = sqrt( -2.0d0 * log( rnd(1) ) ) * cos( 2.0d0*pi*rnd(2) )
        ENDDO
      ELSE
        DO i=1,(ndim-1)/2
          rnd(1) = genrand_res53()
          rnd(2) = genrand_res53()
          var(i*2-1) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
          var(i*2) = sqrt( -2.0d0 * log( rnd(1) ) ) * cos( 2.0d0*pi*rnd(2) )
        ENDDO
        rnd(1) = genrand_res53()
        rnd(2) = genrand_res53()
        var(ndim) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
      ENDIF
      RETURN
      END SUBROUTINE com_randn

      SUBROUTINE input_namelist(debug,tfcst,restart,obs_err_u,obs_err_v,obs_err_t, &
                                obs_err_q,obs_err_p,no,ne,obs_flag,r_obs,icen,jcen)
      INCLUDE "../Registry/letkf.inc"
      RETURN
      END

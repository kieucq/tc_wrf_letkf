!
! [NOTE]:      This program is to read all truth forecast and prinout
!              in the grads format for viewing 
!
! [HISTORY]: - Mar 18, 2010: created by Chanh Kieu
!            - Apr 18, 2010: fix a bug in obs_err_u -> bgd_err_u
!                            and allow for more vars      
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
      PROGRAM add_random_noise_ini
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
      REAL               :: bgd_err_u,bgd_err_v,bgd_err_t,bgd_err_q,bgd_err_p
      REAL               :: rtime,dt,tfcst,restart,mis
      INTEGER            :: obs_flag,ne,itime,ntime
      INTEGER            :: id,jd,no,icen,jcen
      REAL,ALLOCATABLE   :: u(:,:,:,:),v(:,:,:,:),t(:,:,:,:),q(:,:,:,:),p(:,:,:,:)
      REAL,ALLOCATABLE   :: ur(:,:,:),vr(:,:,:),tr(:,:,:),qr(:,:,:),pr(:,:,:)
      REAL,ALLOCATABLE   :: err_u(:),err_v(:),err_t(:),err_q(:),err_p(:)
      REAL(8),ALLOCATABLE :: rnd(:)
      CHARACTER*100      :: name,ofile,rfile,tem
      INTEGER            :: i,j,k,tem_len,debug,ie,irec
      irec               = 1 
      mis                = -99999
!
! reading namelist
!
      CALL input_namelist(debug,tfcst,restart,bgd_err_u,bgd_err_v,bgd_err_t, &
                          bgd_err_q,bgd_err_p,no,ne,obs_flag,r_obs,icen,jcen)
      PRINT*,'ini.exe: restart        =',restart
      PRINT*,'ini.exe: tfcst          =',tfcst
      PRINT*,'ini.exe: bgd_error_u is: ',bgd_err_u
      PRINT*,'ini.exe: bgd_error_v is: ',bgd_err_v
      PRINT*,'ini.exe: bgd_error_t is: ',bgd_err_t
      PRINT*,'ini.exe: bgd_error_q is: ',bgd_err_q
      PRINT*,'ini.exe: bgd_error_p is: ',bgd_err_p
      PRINT*,'ini.exe: number of ensemble is: ',ne
      PRINT*,'ini.exe: number of obs is: ',no
      PRINT*,'ini.exe: obs_flag: ',obs_flag
      PRINT*,'ini.exe: r_obs is: ',r_obs
      PRINT*,'ini.exe: icen is: ',icen
      PRINT*,'ini.exe: jcen is: ',jcen
      restart            = restart/60.
      IF (debug.eq.1) read*
!
! open data file now
!
      ie                 = 1
8     continue      
      rtime              = 0.
      iday               = int (rtime/86400.)
      ihour              = int ((rtime-iday*86400.)/3600.)
      imin               = int ((rtime-iday*86400.-ihour*3600.)/60.)
      ofile              = 'bgd.dat'
      IF (debug.eq.1) then
       PRINT*,''
       PRINT*,'ini.exe: opened truth file is: ',ofile(1:30)
      ENDIF
      status = NF_OPEN(ofile(1:len_trim(ofile)),nf_write,ncid)
      IF (debug.ge.1) then
       PRINT*,'1. NF_OPEN and return value: '
       PRINT*,'status     =',status
       PRINT*,'nf_noerr   =',nf_noerr 
       PRINT*,'ncid       =',ncid
      ENDIF
!
! probing some dimension and structure of data fields
!
      status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
      PRINT*,''
      PRINT*,'2. NF_INQ returns values: '
      PRINT*,'ncid       =',ncid              ! file id
      PRINT*,'ndims      =',ndims             ! number of dims
      PRINT*,'nvars      =',nvars             ! number of varibales
      PRINT*,'ngatts     =',ngatts            ! number of globale attributes
      PRINT*,'unlimdimid =',unlimdimid        ! number of umlimitted dim
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
      tem          = 'PH'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,p)
!
! generate random perturbation now
!
      IF (debug.eq.1) PRINT*,'ini.exe: calling random function'
      ALLOCATE(rnd(nx1*ny*nz))
      CALL com_randn(nx1*ny*nz,rnd)
      ur           = RESHAPE(rnd,(/nx1,ny,nz/))
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny1*nz))
      CALL com_randn(nx*ny1*nz,rnd)
      vr           = RESHAPE(rnd,(/nx,ny1,nz/))
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny*nz))
      CALL com_randn(nx*ny*nz,rnd)
      tr           = RESHAPE(rnd,(/nx,ny,nz/))
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny*nz))
      CALL com_randn(nx*ny*nz,rnd)
      qr           = RESHAPE(rnd,(/nx,ny,nz/))
      DEALLOCATE(rnd)
      ALLOCATE(rnd(nx*ny*nz1))
      CALL com_randn(nx*ny*nz1,rnd)
      pr           = RESHAPE(rnd,(/nx,ny,nz1/))
      DEALLOCATE(rnd)
!
! allow for the variation of background error with height
!
      ALLOCATE(err_u(nz),err_v(nz),err_t(nz),err_q(nz),err_p(nz))
      DO k         = 1,nz
       call compute_err_z(bgd_err_u,bgd_err_v,bgd_err_t,bgd_err_q,bgd_err_p,   &
                          k,nz,err_u(k),err_v(k),err_t(k),err_q(k),err_p(k))
       print*,k,err_u(k),err_v(k),err_t(k),err_q(k),err_p(k)
       ur(:,:,k)   = ur(:,:,k)*err_u(k)
       vr(:,:,k)   = vr(:,:,k)*err_v(k)
       tr(:,:,k)   = tr(:,:,k)*err_t(k)
       qr(:,:,k)   = qr(:,:,k)*err_q(k)
       pr(:,:,k)   = pr(:,:,k)*err_p(k)
      ENDDO
      pr(:,:,nz1)  = pr(:,:,nz1)*err_p(nz)
!
! smooth to the boundaries
!
      call smoothbdy(ur,nx1,ny,nz)
      call smoothbdy(vr,nx,ny1,nz)
      call smoothbdy(tr,nx,ny,nz)
      call smoothbdy(qr,nx,ny,nz)
      call smoothbdy(pr,nx,ny,nz1)
!
! write out all the data now
!
      u(:,:,:,1)   = u(:,:,:,1) + ur(:,:,:)
      v(:,:,:,1)   = v(:,:,:,1) + vr(:,:,:)
      t(:,:,:,1)   = t(:,:,:,1) + tr(:,:,:)
      q(:,:,:,1)   = q(:,:,:,1) + abs(qr(:,:,:))
      p(:,:,:,1)   = p(:,:,:,1) + pr(:,:,:)
      tem          = 'U'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status       = NF_PUT_VAR_REAL(ncid,uid,u)
      IF (debug.eq.1) PRINT*,'ini.exe: put var u status',status
      tem          = 'V'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
      status       = NF_PUT_VAR_REAL(ncid,vid,v)
      IF (debug.eq.1) PRINT*,'ini.exe: put var v status',status
      tem          = 'T'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
      status       = NF_PUT_VAR_REAL(ncid,tid,t)
      IF (debug.eq.1) PRINT*,'ini.exe: put var t status',status      
      tem          = 'QVAPOR'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),qid)
      status       = NF_PUT_VAR_REAL(ncid,qid,q)
      IF (debug.eq.1) PRINT*,'ini.exe: put var q status',status
      tem          = 'PH'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_PUT_VAR_REAL(ncid,pid,p)
      IF (debug.eq.1) PRINT*,'ini.exe: put var p status',status      
!
! PRINT out now
!
      open(99,file='ini.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*4)
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

      SUBROUTINE input_namelist(debug,tfcst,restart,bgd_err_u,bgd_err_v,bgd_err_t, &
                                bgd_err_q,bgd_err_p,no,ne,obs_flag,r_obs,icen,jcen)
      INCLUDE "../Registry/letkf.inc"
      RETURN
      END


      SUBROUTINE smoothbdy(a,nx,ny,nz)
      IMPLICIT NONE
      INTEGER nx,ny,nz,i,j,k
      REAL a(nx,ny,nz),rscale,fxl,fxr,fyl,fyr
      rscale        = 5.
      DO k          = 1,nz
       DO i         = 1,nx
        fxl         = 1. - exp(-((i-1.)/rscale)**4.)
        fxr         = 1. - exp(-((nx-i)/rscale)**4.)  
        DO j        = 1,ny
         fyl        = 1. - exp(-((j-1.)/rscale)**4.)
         fyr        = 1. - exp(-((ny-j)/rscale)**4.)
         a(i,j,k)   = a(i,j,k)*fxl*fxr*fyl*fyr 
        ENDDO
       ENDDO
      ENDDO
      RETURN
      END

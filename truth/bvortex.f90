!
! [NOTE]:      This program is to modify the intial condition for the WRF-ARW
!              with C-grid by removing the idealized b_wave defaulted by WRF
!             (V2.1.2) and inserting my own idealized vortex. Note that the
!              vertical grid is on pseudo-height...be careful
!
! [HISTORY]:  - Created on Jan 7, 2009
!             - The data fields are all put nicely into the original
!               wrfinput. Just need a further check of wrfinput to make
!               sure that it works right. Also, several other important
!               vars need to be replaced including RDNW, RDN, MU, MUB,
!               DNU, DNW, U_BASE, SST, TSK, U10M, V10M... check to see
!               more (Jan 14, 2009).
!             - OK, so the coordinate will be the same, and theta, phi, mu
!               at (1,1,:) will be used as the reference states (barred vars)
!               the real phi will be computed from the total theta, and mu'
!               will be computed from the difference between total mu and 
!               $\overline{\mu}$ (March 5, 2009)
!             - Update to have 2 vortices on the domain to study their
!               interaction (March 23, 2009)
!             - NEED TO REOOMPUTE DENSITY FOR PP INGEST (line 487)
!
! [AUTHOR]:    Chanh Q. Kieu, Research Associate 
!              Dept of atmospheric and oceanic science
!              University of Maryland, College Park
!              Email: kieucq@atmos.umd.edu
!
! [REFERENCE]: Kieu, C. Q. (2005): arxiv.org
!
! [COPYRIGHT]: (C) 2009
!
!===========================================================================
!
      PROGRAM idealize_vortex
      IMPLICIT none
      INCLUDE 'netcdf.inc' 
      INTEGER, PARAMETER :: n=4
      INTEGER            :: ncid,status
      INTEGER            :: ndims,nvars,ngatts,unlimdimid
      INTEGER            :: uid,vid,wid,tid,pid
      INTEGER            :: undim,vndim,wndim,tndim
      INTEGER            :: udim(n),vdim(n),wdim(n),tdim(n)
      INTEGER            :: nx,ny,nz,nt,nx1,ny1,nz1,nt1
      REAL               :: dx,dy,dz
      REAL               :: zscale,rscale,Vamp,rad,epsi
      REAL               :: Vmag,pi,G,cp,rd,roref,pref,tref
      REAL               :: zbar
      INTEGER            :: ic,jc,nmax,debug
      INTEGER            :: numv
      REAL,ALLOCATABLE   :: u(:,:,:,:),u1(:,:,:,:),u2(:,:,:)
      REAL,ALLOCATABLE   :: v(:,:,:,:),v1(:,:,:,:),v2(:,:,:)
      REAL,ALLOCATABLE   :: w(:,:,:,:),w1(:,:,:,:)
      REAL,ALLOCATABLE   :: t(:,:,:,:),t1(:,:,:,:),t2(:,:,:)
      REAL,ALLOCATABLE   :: mu1(:,:),mu(:,:,:),tsk(:,:,:),tmn(:,:,:)
      REAL,ALLOCATABLE   :: mub1(:,:),mub(:,:,:),mu0(:,:,:)
      REAL,ALLOCATABLE   :: ph(:,:,:,:),ph1(:,:,:,:),ph2(:,:,:)
      REAL,ALLOCATABLE   :: phb(:,:,:,:),phb1(:,:,:,:)
      REAL,ALLOCATABLE   :: psi(:,:,:),rhs(:,:,:),tbase(:),ubase(:)
      REAL,ALLOCATABLE   :: rhs2d(:,:),phi2d(:,:)
      REAL,ALLOCATABLE   :: Cof(:,:),beta(:,:),cx(:,:),cy(:,:)
      REAL,ALLOCATABLE   :: pbar(:,:,:),ro(:,:,:)
      REAL,ALLOCATABLE   :: znu(:),znw(:)
      REAL               :: obs_err_u,obs_err_v,obs_err_t,obs_err_q
      REAL               :: rtime,dt,tfcst,restart,mis,r_obs
      INTEGER            :: obs_flag,ne,itime,ntime
      INTEGER            :: id,jd,no,icen,jcen
      CHARACTER*100      :: name,tem
      INTEGER            :: i,j,k,irec,tem_len
      irec               = 1
      pi                 = 4.*atan(1.)
      G                  = 9.81
      roref              = 1.
      pref               = 1.01e+05
      tref               = 300.
      cp                 = 1004.
      rd                 = 287.
      numv               = 1 
      mis                = -99999
!
! reading namelist
!
      CALL input_namelist(debug,tfcst,restart,no,ne,obs_flag,r_obs,icen,jcen,Vamp,rscale)
      PRINT*,'bvortex.exe: restart                :',restart
      PRINT*,'bvortex.exe: tfcst                  :',tfcst
      PRINT*,'bvortex.exe: number of ensemble is  :',ne
      PRINT*,'bvortex.exe: number of obs is       :',no
      PRINT*,'bvortex.exe: obs_flag               :',obs_flag
      PRINT*,'bvortex.exe: r_obs is               :',r_obs
      PRINT*,'bvortex.exe: icen is                :',icen
      PRINT*,'bvortex.exe: jcen is                :',jcen
      PRINT*,'bvortex.exe: number of vortex is    :',numv
      PRINT*,'bvortex.exe: amplitude of b-wind    :',Vamp
      PRINT*,'bvortex.exe: radial scale of b-wind :',rscale
      restart            = restart/60.
      IF (debug.eq.1) read*            
!
! open a file now
!
      status = NF_OPEN('wrfinput_d01_truth',nf_write,ncid)
      PRINT*,''
      PRINT*,'bvortex.exe: 1. NF_OPEN and return value: '
      PRINT*,'bvortex.exe: status     =',status
      PRINT*,'bvortex.exe: nf_noerr   =',nf_noerr 
      PRINT*,'bvortex.exe: ncid       =', ncid
!
! probing some dimension and structure of data fields
!
      status = NF_INQ(ncid, ndims, nvars, ngatts, unlimdimid)
      PRINT*,''
      PRINT*,'bvortex.exe: 2. NF_INQ returns values: '
      PRINT*,'bvortex.exe: ncid       =',ncid            ! file id
      PRINT*,'bvortex.exe: ndims      =',ndims           ! number of dims
      PRINT*,'bvortex.exe: nvars      =',nvars           ! number of varibales
      PRINT*,'bvortex.exe: ngatts     =',ngatts          ! number of globale attributes
      PRINT*,'bvortex.exe: unlimdimid =',unlimdimid      ! number of umlimitted dim
!
! pull our varid and dim information for a temporary variable
! which will be used later.  
!
      tem                = 'T'
      tem_len            = len_trim(tem)
      PRINT*,'bvortex.exe: Checking var is: ',tem(1:tem_len)
      status = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status = NF_INQ_VARNDIMS(ncid,uid,undim)
      status = NF_INQ_VARDIMID(ncid,uid,udim)
      IF (undim.ne.n) THEN
       PRINT*,'bvortex.exe: Number of dim is /= 4...re-allocation now'
       status            = NF_CLOSE(ncid)
       STOP
      ENDIF
      PRINT*,''
      PRINT*,'bvortex.exe: 3. Pull out information about: ',tem(1:tem_len)
      PRINT*,'bvortex.exe: uid        =',uid
      PRINT*,'bvortex.exe: undim      =',undim
      DO i               = 1,undim
       j                 = udim(i)
       status            = NF_INQ_DIMNAME(ncid,j,name)
       status            = NF_INQ_DIMLEN(ncid,j,k)
       IF (i.eq.1) nx    = k
       IF (i.eq.2) ny    = k
       IF (i.eq.3) nz    = k
       IF (i.eq.4) nt    = k
       PRINT*,'bvortex.exe: Dimension name is:',i,udim(i),name(1:20)
      ENDDO
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DX',dx)
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DY',dy)      
      PRINT*,'bvortex.exe: nx =',nx
      PRINT*,'bvortex.exe: ny =',ny
      PRINT*,'bvortex.exe: nz =',nz
      PRINT*,'bvortex.exe: nt =',nt
      PRINT*,'bvortex.exe: dx =',dx
      PRINT*,'bvortex.exe: dy =',dy            
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
      nx1                = nx + 1
      ny1                = ny + 1
      nz1                = nz + 1
      ALLOCATE(u1(nx1,ny1,nz1,nt),u(nx1,ny,nz,nt),u2(nx1,ny1,nz1))
      ALLOCATE(v1(nx1,ny1,nz1,nt),v(nx,ny1,nz,nt),v2(nx1,ny1,nz1))
      ALLOCATE(w1(nx1,ny1,nz1,nt),w(nx,ny,nz1,nt))
      ALLOCATE(t1(nx1,ny1,nz1,nt),t(nx,ny,nz,nt),t2(nx1,ny1,nz1))
      ALLOCATE(ph1(nx1,ny1,nz1,nt),ph(nx,ny,nz1,nt),ph2(nx1,ny1,nz1))
      ALLOCATE(phb1(nx1,ny1,nz1,nt),phb(nx,ny,nz1,nt))
      ALLOCATE(Cof(nx1,ny1),beta(nx1,ny1),cx(nx1,ny1),cy(nx1,ny1))
      ALLOCATE(rhs(nx1,ny1,nz1),rhs2d(nx1,ny1))
      ALLOCATE(psi(nx1,ny1,nz1),phi2d(nx1,ny1))
      ALLOCATE(pbar(nx1,ny1,nz1),ro(nx1,ny1,nz1))
      ALLOCATE(tbase(nz),ubase(nz),tmn(nx,ny,nt))
      ALLOCATE(mu1(nx1,ny1),mu(nx,ny,nt),tsk(nx,ny,nt))
      ALLOCATE(mub1(nx1,ny1),mub(nx,ny,nt),mu0(nx,ny,nt))
!
! define some parameter for the idealized vortex now
!
      dz           = 16.E3/(nz-1) ! from namelist.input
      ic           = icen         ! i-center of vortex
      jc           = jcen         ! j-center of vortex
      zscale       = 2.*nz1/3.    ! scale in z-direction of idealized vortex
      rscale       = rscale/dx    ! scale in radius (number of grid point)
      nmax         = 5E4          ! iteration maximum for relaxation
      epsi         = 0.01         ! threshold for relaxation
      Cof          = 1.e-5        ! Coriolis forcing
      beta         = 0.       
!
! create an idealize rotational flows now on A-grid
!
      IF (numv.eq.1) THEN
       DO k        = 1,nz1
        DO i       = 1,nx1
         DO j      = 1,ny1
          rad      = sqrt((i-ic)**2.+(j-jc)**2.)
          IF (rad.gt.0) THEN
           Vmag    = Vamp*(1-exp(-rad**2/rscale**2))   &
                   / rad*cos(pi*k/(nz*2))              &
                   * exp(-(float(k)/zscale)**4)
          ELSE
           Vmag    = 0
          ENDIF
          Vmag     = max(Vmag,0.) 
          IF (rad.gt.0) THEN
           u1(i,j,k,1) = -Vmag*(j-jc)/rad
           v1(i,j,k,1) =  Vmag*(i-ic)/rad
          ELSE
           u1(i,j,k,1) = 0.
           v1(i,j,k,1) = 0.
          ENDIF
         ENDDO
        ENDDO
       ENDDO 
      ELSEIF (numv.eq.2) THEN
       ic          = 0.4*nx1
       jc          = ny1/2+1
       u1          = 0.
       v1          = 0.
       u2          = 0.
       v2          = 0.
       DO k        = 1,nz1
        DO i       = 1,nx1
         DO j      = 1,ny1
          rad      = sqrt((i-ic)**2.+(j-jc)**2.)
          IF (rad.gt.0) THEN
           Vmag    = Vamp*(1-exp(-rad**2/rscale**2))   &
                   / rad*cos(pi*k/(nz*2))              &
                   * exp(-(float(k)/zscale)**4)
          ELSE
           Vmag    = 0
          ENDIF
          Vmag     = max(Vmag,0.)
          IF (rad.gt.0) THEN
           u2(i,j,k) = -Vmag*(j-jc)/rad
           v2(i,j,k) =  Vmag*(i-ic)/rad
          ELSE
           u2(i,j,k) = 0.
           v2(i,j,k) = 0.
          ENDIF
         ENDDO
        ENDDO
       ENDDO
       u1(:,:,:,1) = u1(:,:,:,1) + u2(:,:,:)
       v1(:,:,:,1) = v1(:,:,:,1) + v2(:,:,:)
       ic          = 0.6*nx1
       jc          = ny1/2+1
       u2          = 0.
       v2          = 0.
       DO k        = 1,nz1
        DO i       = 1,nx1
         DO j      = 1,ny1
          rad      = sqrt((i-ic)**2.+(j-jc)**2.)
          IF (rad.gt.0) THEN
           Vmag    = Vamp*(1-exp(-rad**2/rscale**2))   &
                   / rad*cos(pi*k/(nz*2))              &
                   * exp(-(float(k)/zscale)**4)
          ELSE
           Vmag    = 0
          ENDIF
          Vmag     = max(Vmag,0.)
          IF (rad.gt.0) THEN
           u2(i,j,k) = -Vmag*(j-jc)/rad  
           v2(i,j,k) =  Vmag*(i-ic)/rad  
          ELSE
           u2(i,j,k) = 0.  
           v2(i,j,k) = 0.  
          ENDIF
         ENDDO
        ENDDO
       ENDDO
       u1(:,:,:,1) = u1(:,:,:,1) + u2(:,:,:)
       v1(:,:,:,1) = v1(:,:,:,1) + v2(:,:,:)  
      ELSE
       PRINT*,'bvortex.exe: numv > 2, which is not supported...stop'
       STOP
      ENDIF
!
! use the gradient wind balance to compute geopotential
! pertrubation now by computing streamfunction first,
! then invert for phi
!
      u2(:,:,:)       = u1(:,:,:,1)
      v2(:,:,:)       = v1(:,:,:,1)
      cx              = 1
      cy              = 1
      CALL xsor(psi,u2,v2,nx1,ny1,nz1,dx,dy)
      CALL forcing_psi(rhs,psi,nx1,ny1,nz1,dx,dy,dz,Cof,beta)
      DO k            = 1,nz1
       rhs2d(:,:)=rhs(:,:,k) 
       phi2d=0
       CALL p2d_dd_lipman(phi2d,nx1,ny1,dx,dy,cx,cy,rhs2d,nmax,epsi,debug)
       ph1(:,:,k,1)=phi2d(:,:)
      ENDDO
!
! compute pertrubation potential temperature now
!
      ph2(:,:,:)      = ph1(:,:,:,1)
      CALL cal_pt1(ph2,t2,tref,nx1,ny1,nz1,dz)
      t1(:,:,:,1)     = t2(:,:,:)
!
! pull out the T_BASE information, which is needed to compute PHB
! for the idealized atmosphere from the hydrostatic eqn. Note that
! the var T in wrfinput is (T - tref), not the pertubartion
! temp. So t1 needs to be re-calculated. Also, the mean temp is now
! stored in t2
!
      status          = NF_INQ_VARID(ncid,'T_BASE',tid)
      status          = NF_GET_VAR_REAL(ncid,tid,tbase)
      PRINT*,'bvortex.exe: Checking for the base temperature'
      DO k            = 1,nz
       PRINT*,k,tbase(k),tbase(k)+ tref
       t2(:,:,k)      = tbase(k) + tref
       t1(:,:,k,1)    = (tbase(k) + tref + t1(:,:,k,1)) - tref
      ENDDO
!
! compute the base geopotential now. Note that the coordinate system
! used in em_b_wave is very similar to the pseudo-height coordinate
! with vertical stretching of pressure such as dz = constant. So,
! the based geopotential can be computed from the hydrostatic eqn, which
! in the pseudo-z coordinate taken the form $\frac{\partial
! \phi}{\partial z} = G*\frac{\theta}{\theta_0} 
!
      phb1(:,:,1,1)   = 0
      DO k            = 2,nz1
       phb1(:,:,k,1)  = phb1(:,:,k-1,1) + G*(t2(:,:,k)+t2(:,:,k-1))*dz/tref/2.
      ENDDO
!
! compute the reference pressure and density at each level based on
! T_BASE. Note that density is 
! calculated from the pbar, and mean T, i.e., t2, at each level. 
! Given pbar, we will now also compute mub
!
      PRINT*,'bvortex.exe: Checking for the zbar and pbar'
      DO k            = 1,nz1
       zbar           = (k-1)*dz
       pbar(:,:,k)    = pref*(1-G*zbar/(cp*tref))**(cp/rd)
       print*,k,zbar,pbar(1,1,k)
      ENDDO
      DO i            = 1,nx1
       DO j           = 1,ny1
        mub1(i,j)     = pbar(i,j,1) - pbar(i,j,nz1)
       ENDDO
      ENDDO
!
! compute the actual mu from the total temperature t1, and subtract
! the reference mu from the total mu to obtain mu'. As we only need 
! mu' at z = 0, then the simplest way to do this is to use the 
! hydrostatic opt to estimate mu' with ro ~ 1.1 kg m{^-3}. Note that
! if we have a flat surface, T_BASE = t1, then mu' = 0. mu' indicates
! how the real p is different from the Pref (=1000mb) at the surface
! due to the use of p coordinate. Be careful with Pref, Psurface.
! mub = Pref - Ptop; mu = Psurface - Ptop; mu' = mu - mub 
!
      DO i            = 1,nx1
       DO j           = 1,ny1
        mu1(i,j)      = ph1(i,j,1,1)*1.1
       ENDDO
      ENDDO
!
! re-setting U_BASE even though I dont know why we need this var
! in WRF dyn_em core
!
      ubase            = 0.
!
! checking the idealized vortex now
!
      OPEN(90,file='bvortex.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx1*ny1*4)
      DO k               = 1,nz1
       WRITE(90,rec=irec)((u1(i,j,k,1),i=1,nx1),j=1,ny1)
       irec              = irec + 1
      ENDDO
      DO k               = 1,nz1
       WRITE(90,rec=irec)((v1(i,j,k,1),i=1,nx1),j=1,ny1)
       irec              = irec + 1
      ENDDO
      DO k               = 1,nz1
       WRITE(90,rec=irec)((w1(i,j,k,1),i=1,nx1),j=1,ny1)
       irec              = irec + 1
      ENDDO
      DO k               = 1,nz1
       WRITE(90,rec=irec)((t1(i,j,k,1),i=1,nx1),j=1,ny1)
       irec              = irec + 1
      ENDDO
      DO k               = 1,nz1
       WRITE(90,rec=irec)((ph1(i,j,k,1),i=1,nx1),j=1,ny1)
       irec              = irec + 1
      ENDDO
      DO k               = 1,nz1
       WRITE(90,rec=irec)((rhs(i,j,k),i=1,nx1),j=1,ny1)
       irec              = irec + 1
      ENDDO
      DO k               = 1,nz1
       WRITE(90,rec=irec)((psi(i,j,k),i=1,nx1),j=1,ny1)
       irec              = irec + 1
      ENDDO
      WRITE(90,rec=irec)((mub1(i,j),i=1,nx1),j=1,ny1)
      irec               = irec + 1
      WRITE(90,rec=irec)((mu1(i,j),i=1,nx1),j=1,ny1)
      irec               = irec + 1
      CLOSE(90)
!
! convert from A to C-grid now
!
      DO k               = 1,nz
       DO i              = 1,nx1
        DO j             = 1,ny
         u(i,j,k,1)      = ( (u1(i,j,k,1)   + u1(i,j+1,k,1)  )/2.      &
                            +(u1(i,j,k+1,1) + u1(i,j+1,k+1,1))/2.)/2.
        ENDDO
       ENDDO
       DO i              = 1,nx
        DO j             = 1,ny1
         v(i,j,k,1)      = ( (v1(i,j,k,1)   + v1(i+1,j,k,1)  )/2.      &
                            +(v1(i,j,k+1,1) + v1(i+1,j,k+1,1))/2.)/2.
        ENDDO
       ENDDO
       DO i              = 1,nx
        DO j             = 1,ny
         t(i,j,k,1)      = ( (t1(i,j,k,1)     + t1(i+1,j,k,1)          & 
                             +t1(i,j+1,k,1)   + t1(i+1,j+1,k,1))/4.    &
                            +(t1(i,j,k+1,1)   + t1(i+1,j,k+1,1)        &
                             +t1(i,j+1,k+1,1) + t1(i+1,j+1,k+1,1))/4. )/2.
        ENDDO
       ENDDO
      ENDDO
      DO k               = 1,nz1
       DO i              = 1,nx
        DO j             = 1,ny
         w(i,j,k,1)      = (  w1(i+1,j+1,k,1) + w1(i+1,j,k,1)          &
                            + w1(i,j+1,k,1)   + w1(i,j,k,1)     )/4.
         ph(i,j,k,1)     = ( ph1(i+1,j+1,k,1) + ph1(i+1,j,k,1)         &
                            +ph1(i,j+1,k,1)   + ph1(i,j,k,1)    )/4.
         phb(i,j,k,1)    = (phb1(i+1,j+1,k,1) + phb1(i+1,j,k,1)        &
                           +phb1(i,j+1,k,1)   + phb1(i,j,k,1)   )/4.
        ENDDO
       ENDDO
      ENDDO
      DO i               = 1,nx
       DO j              = 1,ny
        mub(i,j,1)       = ( mub1(i+1,j+1) + mub1(i+1,j)         &
                           +mub1(i,j+1)   + mub1(i,j)    )/4.
        mu(i,j,1)        = ( mu1(i+1,j+1)  + mu1(i+1,j)         &
                           +mu1(i,j+1)    + mu1(i,j)    )/4.
       ENDDO
      ENDDO
      mu0                = mu+mub
      tsk                = 300.
      tmn                = 300.
      DEALLOCATE(u1,u2,v1,v2,w)
!
! put the data back to input file now
!
      ALLOCATE(w(nx1,ny,nz,nt))
      tem                = 'U'
      status             = NF_INQ_VARID(ncid,tem(1:1),uid)
      status             = NF_GET_VAR_REAL(ncid,uid,w)
      w                  = w + u                  
      status             = NF_PUT_VAR_REAL(ncid,uid,w)
      DEALLOCATE(w)
      PRINT*,'bvortex.exe: 5. NF_PUT_VAR_REAL U returns:',status
!      
      ALLOCATE(w(nx,ny1,nz,nt))
      tem                = 'V'
      status             = NF_INQ_VARID(ncid,tem(1:1),vid)
      status             = NF_GET_VAR_REAL(ncid,vid,w)
      w                  = w + v
      status             = NF_PUT_VAR_REAL(ncid,vid,w)
      DEALLOCATE(w)
      PRINT*,'bvortex.exe: 6. NF_PUT_VAR_REAL V returns:',status      
!      
      ALLOCATE(w(nx,ny,nz,nt))
      tem                = 'T'
      status             = NF_INQ_VARID(ncid,tem(1:1),tid)
      status             = NF_GET_VAR_REAL(ncid,tid,w)
      w                  = w + t
      status             = NF_PUT_VAR_REAL(ncid,tid,w)
      DEALLOCATE(w)
      PRINT*,'bvortex.exe: 7. NF_PUT_VAR_REAL T returns:',status
!
      ALLOCATE(w(nx,ny,nz1,nt))
      tem                = 'P'
      status             = NF_INQ_VARID(ncid,tem(1:2),pid)
      status             = NF_GET_VAR_REAL(ncid,pid,w)
      w                  = w + ph*1.1
      status             = NF_PUT_VAR_REAL(ncid,pid,w)
      PRINT*,'bvortex.exe: 8. NF_PUT_VAR_REAL PH returns:',status
!
! close the data file now
!
      status  = NF_CLOSE(ncid)
      IF (status.ne.0) PRINT*,'bvortex.exe: Can not close the new file' 
      END

      SUBROUTINE input_namelist(debug,tfcst,restart,no,ne,obs_flag,r_obs,icen,jcen,zamp,zscale)
      INCLUDE "../Registry/letkf.inc"
      RETURN
      END


!
! NOTE:     
!      This program performs a local ensemble transform Kalman filter
!      for the shallow water equation. Note particular that this program
!      is designed only for a model with 3 varirables only. 
!
! HISTORY:  
!    - 22 Mar 2010: Updated from CK LETKF for swe model.
!    - 25 Mar 2010: Run good. update now the option for different 
!                   assimilating vars, and reorganize module
!    - 18 Apr 2010: update new vars, also change zo->to,za-> ta...
!                   for consistency with obs data
!    - 27 Jun 2010: allow for quality control for each obs
!    - 18 Jul 2010: upgrade to more subtle Q matrix/inflation
!
! REFERENCE:
!    - Hunt et al., 2006: arxiv:physics/0511236
!
! AUTHOR: 
!      Chanh Q. Kieu, Research Associate
!      Dept. of Atmospheric and Oceanic science
!      Univ. of Maryland, College Park, MD
!      email: kieucq@atmos.umd.edu
!
! COPYRIGHT: (C) 2010
!
!=================================================================
!
  program LETKF_wrf
  use letkf_interface
  use common_mtx
  use letkf_core
  use common_random
  use common_io
  use common_utils
  implicit none
  integer                          :: nx                  ! model domain size in x direction
  integer                          :: ny                  ! model domain size in y direction
  integer                          :: nz                  ! model domain size in z direction
  integer                          :: ng                  ! model model global vector length     
  integer                          :: ne                  ! number of ensemble members
  integer                          :: nxl                 ! local patch dimension in x-dir (need to be odd)
  integer                          :: nzl                 ! local patch dimension in z-dir (need to be odd)
  integer                          :: nh                  ! total number of grid point in x-y dims
  integer                          :: no                  ! number of observations
  integer                          :: nol                 ! local patch of obs
!
! vector state arrays
!      
  real, allocatable                :: Xa(:,:)             ! local analysis
  real, allocatable                :: Xa_m(:)             ! local ensemble mean of analysis
  real, allocatable                :: Xb(:,:)             ! local background (forecast) 
  real, allocatable                :: Xb_m(:)             ! local ensemble mean of background
  real, allocatable                :: Xe(:,:)             ! local model error state
  real, allocatable                :: Xe_m(:)             ! local ensemble mean of model error state
  real, allocatable                :: Yb(:,:)             ! local background obs ensemble
  real, allocatable                :: Yb_m(:)             ! local ensemble mean of ensemble obs
  real, allocatable                :: Yo(:)               ! local observation data
  real, allocatable                :: Ro(:,:)             ! local obs error cov mtx
  real, allocatable                :: rho(:)              ! localization matrix
  real, allocatable                :: qc(:)               ! quality control
  real, allocatable                :: Q(:,:),Qinv(:,:)    ! local model error matrix
!
! system options
!
  integer                          :: model_flag          ! flag for model: 0-perfect,1-imperfect
  integer                          :: ini_flag            ! flag for initial condition: 0-perfect, 1-imperfect
  integer                          :: np                  ! total local patch dimension = nxl*nxl*num_var
  integer                          :: nme                 ! number of model ensemble
  real                             :: ifactor             ! inflation factor
  character*100                    :: ifile,ofile,temc    ! I/O files
  integer                          :: i,j,k,m,n,L,ie      ! indexing
  integer                          :: debug               ! debuging
  real                             :: obs_err_u           ! observation variance for u
  real                             :: obs_err_v           ! observation variance for v
  real                             :: obs_err_t           ! observation variance for z
  real                             :: obs_err_q           ! observation variance for q
  real                             :: obs_err_p           ! observation variance for p
  real                             :: rscale              ! scale of background variance matrix
  integer                          :: da_flag             ! flag for choosing assimilation variables
  integer                          :: irec                ! record of output
  irec          = 1
  print*,'letkf.exe: number of vars is',nvar
  print*,'letkf.exe: boundary flag assimilation is',bdy_flag
  print*,'letkf.exe: reference temperature is',tref
!
! reading namelist now
!
  call input_namelist(debug,model_flag,ini_flag,ifactor,rscale,nme, &
                      ne,nx,ny,nz,nxl,nzl,da_flag)
!
! allocate arrays now
!
  open(wunit,file='letkf.dat',access='direct',form='unformatted',recl=nx*ny*4)
  ng            = nx*ny*nz  
  np            = nxl*nxl*nzl*nvar
  allocate(ub(nx,ny,nz,ne),vb(nx,ny,nz,ne),tb(nx,ny,nz,ne))
  allocate(qb(nx,ny,nz,ne),pb(nx,ny,nz,ne))
  allocate(ua(nx,ny,nz,ne),va(nx,ny,nz,ne),ta(nx,ny,nz,ne))
  allocate(qa(nx,ny,nz,ne),pa(nx,ny,nz,ne))
  allocate(Xb(np,ne),Xb_m(np),Xa(np,ne),Xa_m(np))
  allocate(Xe(np,nme),Xe_m(np),Q(np,np),Qinv(np,np))
!
! reading observation data
!
  open(ounit,file='obs.dat')
  read(ounit,*)no
  allocate(uo(no),vo(no),to(no),qo(no),po(no),olat(no),olon(no),olev(no))
  allocate(err_u(no),err_v(no),err_t(no),err_q(no),err_p(no))
  call get_obs(irec,debug,no,nx,ny,nz,ne)
  close(ounit)
!
! reading background data next
!
  call get_bgd(irec,debug,nx,ny,nz,ne)
!
! reading the model error members
!
  if (model_flag.eq.0) then
   ifactor      = 1.0
  else if (model_flag.eq.1) then
   if (nme.gt.1) then
    allocate(ue(nx,ny,nz,nme),ve(nx,ny,nz,nme),te(nx,ny,nz,nme))
    allocate(qe(nx,ny,nz,nme),pe(nx,ny,nz,nme))
    allocate(ume(nx,ny,nz),vme(nx,ny,nz),tme(nx,ny,nz))
    allocate(qme(nx,ny,nz),pme(nx,ny,nz))
    call get_mde(irec,debug,nx,ny,nz,nme)       
   endif        
  else
   print*,'letkf.exe: stop for invalid model_flag option...stop'
   stop 
  endif        
!
! create the background first for the analysis
!
  ua             = ub
  va             = vb
  ta             = tb
  qa             = qb
  pa             = pb
  Q              = 0.0
  Qinv           = 0.0
!
! Now loop over all grid point with the corresponding
! local patch.
!
  nh             = nx*ny
  grid_loop: do i = 1,ng
!
! compute first the grid location and var.
!
   L             = i/nh + 1
   IF (i-(L-1)*nh.ne.0) THEN   
    n            = (i-(L-1)*nh)/nx + 1
   ELSE
    L            = L - 1
    n            = ny     
   ENDIF 
   m             = mod(i-(L-1)*nh,nx)
   if (m.eq.0) then
    n            = n - 1
    m            = nx
   endif
   if (debug.ge.1) then
    print*,''
    print*,'debug: working at the local patch around',i,m,n,L
   endif
!
! finding the number of local observation within each patch. Note that
! the size of the local patch in 2D array will be (m-nxl/2,m+nxl/2) x
! (n-nxl/2,n+nxl/2). This step is merely for defining the local size
!
   call local_obs(m,n,L,nx,ny,nz,nxl,nzl,no,nol,debug)
   if (nol.lt.1) goto 10
!
! now we will project from global to local patch at each grid point (m,n,L)
! The cross correlation can be handled separately to create a new set of
! e.g., un from z. (u,un) are then treated as the total obs of u around
! (m,n).
!
   allocate(Yb(nol,ne),Yb_m(nol),Yo(nol),Ro(nol,nol),rho(nol),qc(nol))
   call global2local(Xb,Xb_m,Yb,Yb_m,Yo,Xe,Xe_m,Q,qc,np,nol,rho,m,n,L, & 
                     nx,ny,nz,ng,ne,no,nxl,nzl,rscale,debug,model_flag,nme) 
!
! define the local observational error covariance matrix R
!
   call observational_err_cov_mtx(Ro,nol,qc)
!
! calling the LETKF core now.
!
   call letkf_main(Xa,Ro,Xb,Xb_m,Yb,Yb_m,Yo,Xe,Xe_m,Q,np,nol,ne,ifactor,rho,   & 
                   da_flag,debug,model_flag,nme)
   if (debug.ge.1) then
    print*,'Local Yb(:1) is'
    write(*,'(9F12.5)')(Yb(j,1),j=1,nol)
    print*,'Local Yo is'
    write(*,'(9F12.5)')(Yo(j),j=1,nol)
    print*,'Local Xb(:1) is'
    write(*,'(9F12.5)')(Xb(j,1),j=1,np)
    print*,'Local Xa(:1) is'
    write(*,'(9F12.5)')(Xa(j,1),j=1,np)
    print*,'Model error matrix Q is'
    write(*,'(9F12.5)')(Q(j,1),j=1,np)
   endif 
!
! project from local patch to global patch at the point $i$, which is corresponding
! to point (m,n) on the 2D grid
!
   call local2global(Xa,Q,np,ne,nx,ny,nz,ng,m,n,L,nxl,nzl,i,model_flag,nme)
   if (debug.ge.1) then
    print*,'letkf.exe: grid point ',m,n,l,i
    write(*,'(9F12.5)')ua(m,n,l,1),va(m,n,l,1),qa(m,n,l,1),ta(m,n,l,1),pa(m,n,l,1)
    write(*,'(9F12.5)')ub(m,n,l,1),vb(m,n,l,1),qb(m,n,l,1),tb(m,n,l,1),pb(m,n,l,1)
    read*
   endif
!
! deallocate the arrays
!
   deallocate(Yb,Yb_m,Yo,Ro,rho,qc)
10 continue

  enddo grid_loop
!
! output now
!  
  call put_ana(irec,debug,nx,ny,nz,ne,no,nxl)
  if (model_flag.eq.1.and.nme.gt.1) call put_mde(irec,debug,nx,ny,nz,nme)
!
  print*,'letkf.exe: LETKF finishes safely...!'
  close(wunit)
  CALL error_handle(99999)  
  end


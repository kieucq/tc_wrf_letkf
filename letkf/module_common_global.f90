MODULE common_global
!
! [PURPOSE]:   Contains the global share varibales
!
! [AUTHOR]:    Chanh Q. Kieu, Research Associate
!              Department of Atmospheric and Oceanic Science
!              Vietnam National University, Hanoi, Vietnam
!              Email: kieucq@atmos.umd.edu
!
! [HISTORY]: - 25 Sep 2011: Created
!
  USE letkf_interface
  USE common_io
  USE common_utils
  IMPLICIT NONE
  SAVE
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
  real, allocatable                :: Xe_m(:)             ! local ensemble mean of model err state
  real, allocatable                :: Yb(:,:)             ! local background obs ensemble
  real, allocatable                :: Yb_m(:)             ! local ensemble mean of ensemble obs
  real, allocatable                :: Yo(:)               ! local observation data
  real, allocatable                :: Ro(:,:)             ! local obs error cov mtx
  real, allocatable                :: rho(:)              ! localization matrix
  real, allocatable                :: qc(:)               ! quality control
  real, allocatable                :: Q(:,:),Qinv(:,:)    ! local model error matrix
  real, allocatable,dimension(:,:) :: u1d,v1d,t1d,q1d,p1d ! temp arrays
  integer, allocatable             :: checko(:)           ! indicator of obs validity (0 means good) 
!
! system options
!
  integer                          :: model_flag          ! flag for model: 0-perfect,1-imperfect
  integer                          :: ini_flag            ! flag for initial condition: 0-perfect, 1-imperfect
  integer                          :: np                  ! total local patch dimension = nxl*nxl*num_var
  integer                          :: nme                 ! number of model ensemble
  real                             :: ifactor             ! inflation factor
  character*100                    :: ifile,ofile,temc    ! I/O files
  real                             :: obs_err_u           ! observation variance for u
  real                             :: obs_err_v           ! observation variance for v
  real                             :: obs_err_t           ! observation variance for z
  real                             :: obs_err_q           ! observation variance for q
  real                             :: obs_err_p           ! observation variance for p
  real                             :: rscale              ! scale of background variance matrix
  integer                          :: da_flag             ! flag for choosing assimilation variables
  integer                          :: obs_flag            ! flag for bogus vortex option smoother
  integer                          :: mean_flag           ! short-range fcsts (0) or GFS update (1)a for bgd

CONTAINS

  SUBROUTINE ini_array(wunit)
  implicit none
  integer wunit
  open(wunit,file='letkf.dat',access='direct',form='unformatted',recl=nx*ny)
  ng            = nx*ny*nz
  np            = nxl*nxl*nzl*nvar
  allocate(ub(nx,ny,nz,ne),vb(nx,ny,nz,ne),tb(nx,ny,nz,ne))
  allocate(qb(nx,ny,nz,ne),pb(nx,ny,nz,ne))
  allocate(Xb(np,ne),Xb_m(np),Xa(np,ne),Xa_m(np))
  allocate(Xe(np,nme),Xe_m(np),Q(np,np),Qinv(np,np))
  allocate(u1d(ng,ne),v1d(ng,ne),t1d(ng,ne),q1d(ng,ne),p1d(ng,ne))
  END SUBROUTINE ini_array

  SUBROUTINE get_model_error(irec,debug)
  IMPLICIT NONE
  INTEGER irec,debug
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
  END SUBROUTINE get_model_error

  SUBROUTINE get_grid(i,m,n,L,debug)
  IMPLICIT NONE
  INTEGER :: i,m,n,L,debug
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
  END SUBROUTINE get_grid

  SUBROUTINE set_analysis_background
  IMPLICIT NONE
  INTEGER i,j,k,ig
  Q              = 0.0
  Qinv           = 0.0
  nh             = nx*ny
  DO k           = 1,nz
   DO j          = 1,ny
    DO i         = 1,nx
     ig          = i + (j-1)*nx + (k-1)*nh
     u1d(ig,1:ne)= ub(i,j,k,1:ne)
     v1d(ig,1:ne)= vb(i,j,k,1:ne)
     t1d(ig,1:ne)= tb(i,j,k,1:ne)
     q1d(ig,1:ne)= qb(i,j,k,1:ne)
     p1d(ig,1:ne)= pb(i,j,k,1:ne)
    ENDDO
   ENDDO
  ENDDO
  END SUBROUTINE set_analysis_background

  SUBROUTINE check_letkf(m,n,l,i,debug)
  IMPLICIT NONE
  INTEGER m,n,l,i,debug,j
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
   print*,'letkf.exe: grid point ',m,n,l,i
   write(*,'(9F12.5)')u1d(i,1),v1d(i,1),q1d(i,1),t1d(i,1),p1d(i,1)
   write(*,'(9F12.5)')ub(m,n,l,1),vb(m,n,l,1),qb(m,n,l,1),tb(m,n,l,1),pb(m,n,l,1)
   read*
  endif
  END SUBROUTINE check_letkf

  subroutine local2global_1d(m,n,L,ig,debug)
  implicit none
  integer m,n,L,ig
  integer istart,iend,jstart,jend,kstart,kend,ic,debug
!
! defining the start and end position of the local patch around ig
!
  istart         = m - nxl/2
  iend           = m + nxl/2
  jstart         = n - nxl/2
  jend           = n + nxl/2
  kstart         = L - nzl/2
  kend           = L + nzl/2
  if (istart.lt.1) then
   istart        = 1
   iend          = nxl
   if (bdy_flag.eq.0) return
  endif
  if (iend.gt.nx-1)  then
   istart        = nx - nxl + 1
   iend          = nx
   if (bdy_flag.eq.0) return
  endif
  if (jstart.lt.1) then
   jstart        = 1
   jend          = nxl
   if (bdy_flag.eq.0) return
  endif
  if (jend.gt.ny-1)  then
   jstart        = ny - nxl + 1
   jend          = ny
   if (bdy_flag.eq.0) return
  endif
  if (kstart.lt.1) then
   kstart        = 1
   kend          = nzl
   if (bdy_flag.eq.0) return
  endif
  if (kend.gt.nz-1)  then
   kstart        = nz - nzl + 1
   kend          = nz
   if (bdy_flag.eq.0) return
  endif
  if (debug.ge.1) then
   print*,'debug: fixed istart,iend,jstart,jend,kstart,kend and nvar'
   write(*,'(10I5)')istart,iend,jstart,jend,kstart,kend,nvar
  endif
!
! note that np = nxl*nxl*nzl*nvar
!
  ic             = nxl*nxl*nzl/2 + 1
  u1d(ig,1:ne)   = Xa(ic,1:ne)
  v1d(ig,1:ne)   = Xa(np/nvar+ic,1:ne)
  t1d(ig,1:ne)   = Xa(2*np/nvar+ic,1:ne)
  q1d(ig,1:ne)   = Xa(3*np/nvar+ic,1:ne)
  p1d(ig,1:ne)   = Xa(4*np/nvar+ic,1:ne)
!
! plot model error variance at each point
!
  if (model_flag.eq.1.and.nme.gt.1) then
   ume(m,n,L)    = Q(ic,ic)
   vme(m,n,L)    = Q(np/nvar+ic,np/nvar+ic)
   tme(m,n,L)    = Q(2*np/nvar+ic,2*np/nvar+ic)
   qme(m,n,L)    = Q(3*np/nvar+ic,3*np/nvar+ic)
   pme(m,n,L)    = Q(4*np/nvar+ic,4*np/nvar+ic)
  endif
  return
  end subroutine local2global_1d

  subroutine get_ana(debug)
  implicit none
  integer i,j,k,npass,is,ie,js,je,debug,L
  allocate(ua(nx,ny,nz,ne),va(nx,ny,nz,ne),ta(nx,ny,nz,ne))
  allocate(qa(nx,ny,nz,ne),pa(nx,ny,nz,ne))
  do k        = 1,nz
   do j       = 1,ny
    do i      = 1,nx
     L        = i + (j-1)*nx + (k-1)*nh
     ua(i,j,k,1:ne) = u1d(L,1:ne)
     va(i,j,k,1:ne) = v1d(L,1:ne)
     ta(i,j,k,1:ne) = t1d(L,1:ne)
     qa(i,j,k,1:ne) = q1d(L,1:ne)
     pa(i,j,k,1:ne) = p1d(L,1:ne)
    enddo
   enddo
  enddo
  end subroutine get_ana

END MODULE common_global


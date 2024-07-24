MODULE letkf_interface
!
! [PURPOSE]:   Contains the constant of the LETKF algorithm
!
! [AUTHOR]:    Chanh Q. Kieu, Research Associate
!              Department of Atmospheric and Oceanic Science
!              Vietnam National University, Hanoi, Vietnam
!              Email: kieucq@atmos.umd.edu
!
! [HISTORY]: - 25 Mar 2010: Created
!
  IMPLICIT NONE
  SAVE
!
! state arrays
!
  REAL(4), ALLOCATABLE                :: ub(:,:,:,:)                 ! background u
  REAL(4), ALLOCATABLE                :: vb(:,:,:,:)                 ! background v
  REAL(4), ALLOCATABLE                :: tb(:,:,:,:)                 ! background z
  REAL(4), ALLOCATABLE                :: qb(:,:,:,:)                 ! background q  
  REAL(4), ALLOCATABLE                :: pb(:,:,:,:)                 ! background p
  REAL(4), ALLOCATABLE                :: ua(:,:,:,:)                 ! analysis u
  REAL(4), ALLOCATABLE                :: va(:,:,:,:)                 ! analysis v
  REAL(4), ALLOCATABLE                :: ta(:,:,:,:)                 ! analysis z
  REAL(4), ALLOCATABLE                :: qa(:,:,:,:)                 ! analysis q  
  REAL(4), ALLOCATABLE                :: pa(:,:,:,:)                 ! analysis p
  REAL(4), ALLOCATABLE                :: ue(:,:,:,:)                 ! model err members u
  REAL(4), ALLOCATABLE                :: ve(:,:,:,:)                 ! model err members v
  REAL(4), ALLOCATABLE                :: te(:,:,:,:)                 ! model err members z
  REAL(4), ALLOCATABLE                :: qe(:,:,:,:)                 ! model err members q
  REAL(4), ALLOCATABLE                :: pe(:,:,:,:)                 ! model err members p
  REAL(4), ALLOCATABLE                :: ume(:,:,:)                  ! model err u
  REAL(4), ALLOCATABLE                :: vme(:,:,:)                  ! model err v
  REAL(4), ALLOCATABLE                :: tme(:,:,:)                  ! model err z
  REAL(4), ALLOCATABLE                :: qme(:,:,:)                  ! model err q
  REAL(4), ALLOCATABLE                :: pme(:,:,:)                  ! model err p
  REAL(4), ALLOCATABLE                :: uo(:),vo(:)                 ! obs u,v
  REAL(4), ALLOCATABLE                :: to(:),qo(:),po(:)           ! obs z,q,p
  REAL(4), ALLOCATABLE                :: err_u(:),err_v(:),err_t(:)  ! obs err from quality control
  REAL(4), ALLOCATABLE                :: err_q(:),err_p(:)
!
! observed locations
!
  REAL(4), ALLOCATABLE                :: olon(:)                     ! obs location in x-direction
  REAL(4), ALLOCATABLE                :: olat(:)                     ! obs location in y-direction
  REAL(4), ALLOCATABLE                :: olev(:)                     ! obs location in z-direction
!
! constants
!
  REAL(4), PARAMETER                  :: tref=300,pref=1.e5          ! reference temperature
  INTEGER, PARAMETER                  :: bdy_flag=0                  ! flag for boundary assimilation
  REAL(4), PARAMETER                  :: mis=-99999.                 ! missing value for ploting
  INTEGER, PARAMETER                  :: ounit=90,wunit=92           ! OI units
  INTEGER, PARAMETER                  :: nvar=5                      ! maximum number of assimilated vars
  REAL(4), PARAMETER                  :: qcoef=0.01                  ! coefficient for handling model err 
!
! scales of all vars for non-dimensionalization
!
  REAL(4), PARAMETER                  :: u_order=1.0                 ! u-order
  REAL(4), PARAMETER                  :: v_order=1.0                 ! v-order
  REAL(4), PARAMETER                  :: t_order=300.                ! t-order
  REAL(4), PARAMETER                  :: q_order=1.e-3               ! q-order
  REAL(4), PARAMETER                  :: p_order=10000.0             ! p-order (geopotential gz, not height)
  INTEGER, PARAMETER                  :: nodim=1                     ! non-dimensionalized option

END MODULE letkf_interface


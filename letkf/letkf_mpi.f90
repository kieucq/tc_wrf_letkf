!
! NOTE:     
!      This program performs a local ensemble transform Kalman 
!      filter for the WRF-ARW model system.
!
! HISTORY:  
!    - 22 Mar 2010: Updated from CK LETKF for swe model.
!    - 25 Mar 2010: Run good. update now the option for different 
!                   assimilating vars, and reorganize module
!    - 18 Apr 2010: update new vars, also change zo->to,za-> ta...
!                   for consistency with obs data
!    - 27 Jun 2010: allow for quality control for each obs
!    - 18 Jul 2010: upgrade to more subtle Q matrix/inflation
!    - 25 Sep 2011: modulized for parallelization
!    - 22 Mar 2015: added a flag_qc to remove the data points from
!                   CIMSS-AMV that have zero obs error  
!
! REFERENCE:
!    - Hunt et al., 2006: arxiv:physics/0511236
!
! AUTHOR: 
!      Chanh Q. Kieu, Professor
!      Atmospheric Science Program
!      Department of Geological Sciences
!      Indiana University, Bloomington, IN 47405
!      email: ckieu@indiana.edu
!
! COPYRIGHT: (C) 2015
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
  use common_global
  use common_mpi
  implicit none
  include 'mpif.h'
  integer              :: ierr,nprocs,myrank,irank,newtype1        
  integer              :: ista,iend,iprev,inext    
  integer              :: ureq1,vreq1,treq1,qreq1,preq1
  integer              :: istatus(MPI_STATUS_SIZE)
  integer              :: i,j,k,m,n,L,ie      ! local indexing
  integer              :: debug,irec          ! record and debug
  integer              :: flag_qc             ! flag for qc checking
  integer, allocatable :: itype(:),ureq(:),vreq(:)
  integer, allocatable :: treq(:),qreq(:),preq(:)
  irec          = 1
!
! initialize MPI communications
!
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
  if (myrank.eq.0) then
   print*,'letkf.exe: number of vars is',nvar
   print*,'letkf.exe: boundary flag assimilation is',bdy_flag
   print*,'letkf.exe: reference temperature is',tref
  endif
!
! reading namelist now
!
  call input_namelist(debug,model_flag,ini_flag,ifactor,    &
                      rscale,nme,ne,nx,ny,nz,nxl,nzl,       &
                      da_flag,obs_flag,mean_flag)
!
! allocate arrays now
!
  call ini_array(wunit)
!
! reading observation data
!
  open(ounit,file='obs.dat')
  read(ounit,*)no
  allocate(uo(no),vo(no),to(no),qo(no),po(no))
  allocate(olat(no),olon(no),olev(no))
  allocate(err_u(no),err_v(no),err_t(no),err_q(no),err_p(no))
  call get_obs(irec,debug,no,nx,ny,nz,ne)
  close(ounit)
!
! reading background data next
!
  call get_bgd(irec,debug,nx,ny,nz,ne)
!
! reading the model error members for external Q approach
!
  call get_model_error(irec,debug)
!
! create the background first for the analysis
!
  call set_analysis_background
!
! indexing the MPI jobs
!
  call para_range(1,ng,nprocs,myrank,ista,iend)
  iprev         = myrank-1
  inext         = myrank+1
  if (myrank.eq.0)        iprev = MPI_PROC_NULL
  if (myrank.eq.nprocs-1) inext = MPI_PROC_NULL
  print*,'myrank = ',myrank,ista,iend
!
! Now loop over all grid point with the corresponding
! local patch.
!
  grid_loop: do i = ista,iend
!
! compute first the grid location and var.
!
   call get_grid(i,m,n,L,debug)
!
! finding the number of local observation within each patch. 
! Note that the size of the local patch in 2D array will be 
! (m-nxl/2,m+nxl/2)x(n-nxl/2,n+nxl/2). 
!
   call local_obs(m,n,L,nx,ny,nz,nxl,nzl,no,nol,debug)
   if (nol.lt.1) goto 10
!
! now we will project from global to local patch at each 
! grid point (m,n,L)
!
   allocate(Yb(nol,ne),Yb_m(nol),Yo(nol),Ro(nol,nol))
   allocate(rho(nol),qc(nol),checko(nol))
   call global2local(Xb,Xb_m,Yb,Yb_m,Yo,Xe,Xe_m,Q,qc,np,     &
                     nol,rho,m,n,L,nx,ny,nz,ng,ne,no,nxl,    & 
                     nzl,rscale,debug,model_flag,nme,checko) 
!
! define the local observational error covariance matrix R
!
   call observational_err_cov_mtx(Ro,nol,qc,flag_qc)
   if (flag_qc.eq.1) goto 11
!
! calling the LETKF core now.
!
   call letkf_main(Xa,Ro,Xb,Xb_m,Yb,Yb_m,Yo,Xe,Xe_m,Q,np,  &
                   nol,ne,ifactor,rho,da_flag,debug,       &
                   model_flag,nme,checko,mean_flag)
!
! project from local patch to global patch at the point $i$, 
! which is corresponding to point (m,n,L) on the 3D grid
!
   call local2global_1d(m,n,L,i,debug)
!
! deallocate the temporay local arrays
!
   call check_letkf(m,n,l,i,debug)
11 continue
   deallocate(Yb,Yb_m,Yo,Ro,rho,qc,checko)
10 continue
  enddo grid_loop
!
! initaate rank communication for MPI
!
  deallocate(ub,vb,tb,qb,pb)
  allocate(itype(0:nprocs-1))
  allocate(ureq(0:nprocs-1))  
  allocate(vreq(0:nprocs-1))
  allocate(treq(0:nprocs-1))
  allocate(qreq(0:nprocs-1))
  allocate(preq(0:nprocs-1))
  do irank       = 0,nprocs - 1
   call para_range(1,ng,nprocs,irank,ista,iend)
   call para_type_block2(1,ng,1,ista,iend,1,ne,MPI_REAL,itype(irank))
  enddo
  if (myrank.eq.0) then
!
! gather the updates from all cores (non-contiguous memory)
!
   do irank      = 1,nprocs-1
    call MPI_IRECV(u1d,1,itype(irank),irank,1,MPI_COMM_WORLD,ureq(irank),ierr)
    call MPI_IRECV(v1d,1,itype(irank),irank,1,MPI_COMM_WORLD,vreq(irank),ierr)
    call MPI_IRECV(t1d,1,itype(irank),irank,1,MPI_COMM_WORLD,treq(irank),ierr)
    call MPI_IRECV(q1d,1,itype(irank),irank,1,MPI_COMM_WORLD,qreq(irank),ierr)
    call MPI_IRECV(p1d,1,itype(irank),irank,1,MPI_COMM_WORLD,preq(irank),ierr)
   enddo
   do irank      = 1,nprocs-1
    call MPI_WAIT(ureq(irank),istatus,ierr)
    call MPI_WAIT(vreq(irank),istatus,ierr)
    call MPI_WAIT(treq(irank),istatus,ierr)
    call MPI_WAIT(qreq(irank),istatus,ierr)
    call MPI_WAIT(preq(irank),istatus,ierr)
   enddo
!
! output now
! 
   call get_ana(debug)
   call put_ana(irec,debug,nx,ny,nz,ne,no,nxl,obs_flag,mean_flag,da_flag)
   if (model_flag.eq.1.and.nme.gt.1) then 
    call put_mde(irec,debug,nx,ny,nz,nme)
   endif
  else
   call MPI_ISEND(u1d,1,itype(myrank),0,1,MPI_COMM_WORLD,ureq1,ierr)
   call MPI_ISEND(v1d,1,itype(myrank),0,1,MPI_COMM_WORLD,vreq1,ierr)
   call MPI_ISEND(t1d,1,itype(myrank),0,1,MPI_COMM_WORLD,treq1,ierr)
   call MPI_ISEND(q1d,1,itype(myrank),0,1,MPI_COMM_WORLD,qreq1,ierr)
   call MPI_ISEND(p1d,1,itype(myrank),0,1,MPI_COMM_WORLD,preq1,ierr)
   call MPI_WAIT(ureq1,istatus,ierr)
   call MPI_WAIT(vreq1,istatus,ierr)
   call MPI_WAIT(treq1,istatus,ierr)
   call MPI_WAIT(qreq1,istatus,ierr)
   call MPI_WAIT(preq1,istatus,ierr)
  endif  
!
  print*,'letkf.exe: LETKF finishes safely...!'
  close(wunit)
  if (myrank.eq.0) CALL error_handle(99999)  
  CALL MPI_FINALIZE(ierr)
  end


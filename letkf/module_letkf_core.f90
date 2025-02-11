MODULE letkf_core
!
! [PURPOSE]:   Contains the core of the LETKF algorithm
!
! [AUTHOR]:    Chanh Q. Kieu, Research Associate
!              Department of Atmospheric and Oceanic Science
!              Vietnam National University, Hanoi, Vietnam
!              Email: kieucq@atmos.umd.edu
!
! [HISTORY]: - 25 Mar 2010: Created
!            - 12 May 2010: change the ifactor so that 
!                           it is consistent with H03's paper
!
  USE letkf_interface
  IMPLICIT NONE
  SAVE
  CONTAINS

  subroutine local_obs(m,n,L,nx,ny,nz,nxl,nzl,no,nol,debug)
  implicit none
  integer m,n,L,no,nxl,nzl,nol,nx,ny,nz
  real xlon
  integer i,j,k,istart,iend,jstart,jend,kstart,kend,debug
!
! defining the start and end position of the local patch around ig
!
  nol            = 0
  istart         = m - nxl/2
  iend           = m + nxl/2
  jstart         = n - nxl/2
  jend           = n + nxl/2
  kstart         = L - nzl/2
  kend           = L + nzl/2
  if (debug.ge.1) then
   print*,'debug: istart,iend,jstart,jend,kstart,kend'
   write(*,'(10I5)')istart,iend,jstart,jend,kstart,kend
  endif
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
   print*,'debug: fixed istart,iend,jstart,jend,kstart,kend'
   write(*,'(10I5)')istart,iend,jstart,jend,kstart,kend
  endif
!
! find the local obs within the local obs patch.
! Note that the cyclinc boundary is applied only for +/- 5 points
! at each end of the domain.
!
  nol            = 0
  do i           = 1,no
   if (olon(i).ge.istart.and.olon(i).le.iend.and. &
       olat(i).ge.jstart.and.olat(i).le.jend.and. &
       olev(i).ge.kstart.and.olev(i).le.kend) then
    nol          = nol + 1
   endif
  enddo
!
! multiply by nvar as we will work with nvar vars.
!
  nol            = nol*nvar
  return
  end subroutine local_obs

                         
  subroutine global2local(Xb,Xb_m,Yb,Yb_m,Yo,Xe,Xe_m,Q,qc,np,nol,rho,m,n,L,nx,ny,nz, &
                          ng,ne,no,nxl,nzl,rscale,debug,model_flag,nme,checko)
  implicit none
  integer              :: nx,ny,nz,no,np,nol,ng,ne,m,n,L,nxl,nzl,nme,model_flag
  real, intent(in)     :: rscale
  real, intent(out)    :: Xb(np,ne),Xb_m(np),Yb(nol,ne),Yb_m(nol),Yo(nol)
  real, intent(out)    :: Xe(np,nme),Xe_m(np),rho(nol),qc(nol),Q(np,np)
  integer, intent(out) :: checko(nol)
  integer              :: i,j,k,debug,nlocal,id
  integer              :: istart,iend,jstart,jend,kstart,kend
  integer              :: i_lon,j_lat,k_lev
  real                 :: r1,r2,r3,r4,r5,r6,r7,r8
  real                 :: w1,w2,w3,w4,w5,w6,w7,w8,dem
  real                 :: u_ck,v_ck,t_ck,q_ck,p_ck
  logical              :: check_nme
!
! defining the start and end position of the local patch around ig
!
  check_nme      = (model_flag.eq.1.and.nme.gt.1)
  istart         = m - nxl/2
  iend           = m + nxl/2
  jstart         = n - nxl/2
  jend           = n + nxl/2
  kstart         = L - nzl/2
  kend           = L + nzl/2
!  if (debug.ge.1) then
!   print*,'debug: istart,iend,jstart,jend,kstart,kend'
!   write(*,'(10I5)')istart,iend,jstart,jend,kstart,kend
!  endif
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
!  if (debug.ge.1) then
!   print*,'debug: fixed istart,iend,jstart,jend,kstart,kend'
!   write(*,'(10I5)')istart,iend,jstart,jend,kstart,kend
!  endif
!
! asign the global array to local patch
!
  id             = 0
  do k           = kstart,kend
   do j          = jstart,jend
    do i         = istart,iend
     id          = id + 1
!
! background state
!     
     Xb(id,1:ne)            = ub(i,j,k,1:ne)
     Xb(np/nvar+id,1:ne)    = vb(i,j,k,1:ne)
     Xb(2*np/nvar+id,1:ne)  = tb(i,j,k,1:ne)
     Xb(3*np/nvar+id,1:ne)  = qb(i,j,k,1:ne)
     Xb(4*np/nvar+id,1:ne)  = pb(i,j,k,1:ne)
!
! model error state
!     
     if (check_nme) then
      Xe(id,1:nme)           = ue(i,j,k,1:nme)
      Xe(np/nvar+id,1:nme)   = ve(i,j,k,1:nme)
      Xe(2*np/nvar+id,1:nme) = te(i,j,k,1:nme)
      Xe(3*np/nvar+id,1:nme) = qe(i,j,k,1:nme)
      Xe(4*np/nvar+id,1:nme) = pe(i,j,k,1:nme)     
     endif
    enddo
   enddo
  enddo
!
! ensemble mean of the background state
!
  Xb_m(:)        = 0
  do i           = 1,ne
   Xb_m(:)       = Xb_m(:) + Xb(:,i)
  enddo
  Xb_m(:)        = Xb_m(:)/ne
  do i           = 1,ne
   Xb(:,i)       = Xb(:,i) - Xb_m(:)
  enddo
!
! ensemble mean of the model error state
!  
  if (check_nme) then
   Xe_m(:)       = 0
   do i          = 1,nme
    Xe_m(:)      = Xe_m(:) + Xe(:,i)
   enddo
   Xe_m(:)       = Xe_m(:)/nme
   do i          = 1,nme
    Xe(:,i)      = Xe(:,i) - Xe_m(:)
   enddo
   do i          = 1,np
    do j         = 1,np
     Q(i,j)      = 0.
     do k        = 1,nme
      Q(i,j)     = Q(i,j) + Xe(i,k)*Xe(j,k)
     enddo
     Q(i,j)      = Q(i,j)/float(nme)
    enddo
   enddo
  endif   
!
! find and assign the global obs ensemble to the local obs patch.
! Note that the cyclinc boundary is applied only for +/- 5 points
! at each end of the domain.
!
  k              = 0
  do i           = 1,no
   if (olon(i).ge.istart.and.olon(i).le.iend.and. &
       olat(i).ge.jstart.and.olat(i).le.jend.and. &
       olev(i).ge.kstart.and.olev(i).le.kend) then
    k            = k + 1
!
! background observation first. This needs obs operator $H$.
!
    i_lon                 = int(olon(i))
    j_lat                 = int(olat(i))
    k_lev                 = int(olev(i))
    r1                    = sqrt((olon(i)-i_lon)**2.+(olat(i)-j_lat)**2.                           &
                                +(olev(i)-k_lev)**2.)
    r2                    = sqrt((olon(i)-i_lon-1)**2.+(olat(i)-j_lat)**2.                         &
                                +(olev(i)-k_lev)**2.)
    r3                    = sqrt((olon(i)-i_lon)**2.+(olat(i)-j_lat-1)**2.                         & 
                                +(olev(i)-k_lev)**2.)
    r4                    = sqrt((olon(i)-i_lon-1)**2.+(olat(i)-j_lat-1)**2.                       &
                                +(olev(i)-k_lev)**2.)
    r5                    = sqrt((olon(i)-i_lon)**2.+(olat(i)-j_lat)**2.                           &
                                +(olev(i)-k_lev-1)**2.)
    r6                    = sqrt((olon(i)-i_lon-1)**2.+(olat(i)-j_lat)**2.                         &
                                +(olev(i)-k_lev-1)**2.)
    r7                    = sqrt((olon(i)-i_lon)**2.+(olat(i)-j_lat-1)**2.                         & 
                                +(olev(i)-k_lev-1)**2.)
    r8                    = sqrt((olon(i)-i_lon-1)**2.+(olat(i)-j_lat-1)**2.                       &
                                +(olev(i)-k_lev-1)**2.)    
    dem                   = r2*r3*r4*r5*r6*r7*r8+r1*r3*r4*r5*r6*r7*r8                              &
                           +r1*r2*r4*r5*r6*r7*r8+r1*r2*r3*r5*r6*r7*r8                              &
                           +r1*r2*r3*r4*r6*r7*r8+r1*r2*r3*r4*r5*r7*r8                              &
                           +r1*r2*r3*r4*r5*r6*r8+r1*r2*r3*r4*r5*r6*r7                              
    w1                    = r2*r3*r4*r5*r6*r7*r8/dem
    w2                    = r1*r3*r4*r5*r6*r7*r8/dem 
    w3                    = r1*r2*r4*r5*r6*r7*r8/dem
    w4                    = r1*r2*r3*r5*r6*r7*r8/dem
    w5                    = r1*r2*r3*r4*r6*r7*r8/dem
    w6                    = r1*r2*r3*r4*r5*r7*r8/dem                 
    w7                    = r1*r2*r3*r4*r5*r6*r8/dem
    w8                    = r1*r2*r3*r4*r5*r6*r7/dem 
    Yb(k,1:ne)            = w1*ub(i_lon,j_lat,k_lev,1:ne)   + w2*ub(i_lon+1,j_lat,k_lev,1:ne) +    &
                            w3*ub(i_lon,j_lat+1,k_lev,1:ne) + w4*ub(i_lon+1,j_lat+1,k_lev,1:ne) +  &
                            w5*ub(i_lon,j_lat,k_lev+1,1:ne) + w6*ub(i_lon+1,j_lat,k_lev+1,1:ne) +  &
                            w7*ub(i_lon,j_lat+1,k_lev+1,1:ne) + w8*ub(i_lon+1,j_lat+1,k_lev+1,1:ne)  
    Yb(nol/nvar+k,1:ne)   = w1*vb(i_lon,j_lat,k_lev,1:ne)   + w2*vb(i_lon+1,j_lat,k_lev,1:ne) +    &
                            w3*vb(i_lon,j_lat+1,k_lev,1:ne) + w4*vb(i_lon+1,j_lat+1,k_lev,1:ne) +  &
                            w5*vb(i_lon,j_lat,k_lev+1,1:ne) + w6*vb(i_lon+1,j_lat,k_lev+1,1:ne) +  &  
                            w7*vb(i_lon,j_lat+1,k_lev+1,1:ne) + w8*vb(i_lon+1,j_lat+1,k_lev+1,1:ne)
    Yb(2*nol/nvar+k,1:ne) = w1*tb(i_lon,j_lat,k_lev,1:ne)   + w2*tb(i_lon+1,j_lat,k_lev,1:ne) +    &
                            w3*tb(i_lon,j_lat+1,k_lev,1:ne) + w4*tb(i_lon+1,j_lat+1,k_lev,1:ne) +  &
                            w5*tb(i_lon,j_lat,k_lev+1,1:ne) + w6*tb(i_lon+1,j_lat,k_lev+1,1:ne) +  &
                            w7*tb(i_lon,j_lat+1,k_lev+1,1:ne) + w8*tb(i_lon+1,j_lat+1,k_lev+1,1:ne)   
    Yb(3*nol/nvar+k,1:ne) = w1*qb(i_lon,j_lat,k_lev,1:ne)   + w2*qb(i_lon+1,j_lat,k_lev,1:ne) +    &
                            w3*qb(i_lon,j_lat+1,k_lev,1:ne) + w4*qb(i_lon+1,j_lat+1,k_lev,1:ne) +  &
                            w5*qb(i_lon,j_lat,k_lev+1,1:ne) + w6*qb(i_lon+1,j_lat,k_lev+1,1:ne) +  &
                            w7*qb(i_lon,j_lat+1,k_lev+1,1:ne) + w8*qb(i_lon+1,j_lat+1,k_lev+1,1:ne)
    Yb(4*nol/nvar+k,1:ne) = w1*pb(i_lon,j_lat,k_lev,1:ne)   + w2*pb(i_lon+1,j_lat,k_lev,1:ne) +    &
                            w3*pb(i_lon,j_lat+1,k_lev,1:ne) + w4*pb(i_lon+1,j_lat+1,k_lev,1:ne) +  &
                            w5*pb(i_lon,j_lat,k_lev+1,1:ne) + w6*pb(i_lon+1,j_lat,k_lev+1,1:ne) +  &
                            w7*pb(i_lon,j_lat+1,k_lev+1,1:ne) + w8*pb(i_lon+1,j_lat+1,k_lev+1,1:ne) 
!
! next, true observation projection here
!
    Yo(k)                 = uo(i)
    Yo(nol/nvar+k)        = vo(i)
    Yo(2*nol/nvar+k)      = to(i)
    Yo(3*nol/nvar+k)      = qo(i)
    Yo(4*nol/nvar+k)      = po(i)
!
! filter the bad data point here. Need to go back to dimensional
! unit first
!
    IF (nodim.eq.1) THEN
     u_ck                 = uo(i)*u_order
     v_ck                 = vo(i)*v_order
     t_ck                 = to(i)*t_order
     q_ck                 = qo(i)*q_order
     p_ck                 = po(i)*p_order
    ENDIF
    if (abs(u_ck-mis).lt.0.1) then
     checko(k)            = 1
    else
     checko(k)            = 0
    endif
    if (abs(v_ck-mis).lt.0.1) then
     checko(nol/nvar+k)   = 1
    else
     checko(nol/nvar+k)   = 0
    endif
    if (abs(t_ck-mis).lt.0.1) then
     checko(2*nol/nvar+k) = 1
    else
     checko(2*nol/nvar+k) = 0
    endif
    if (abs(q_ck-mis).lt.0.1) then
     checko(3*nol/nvar+k) = 1
    else
     checko(3*nol/nvar+k) = 0
    endif
    if (abs(p_ck-mis).lt.0.1) then
     checko(4*nol/nvar+k) = 1
    else
     checko(4*nol/nvar+k) = 0
    endif
    !print*,'chanhkq: i,k: ',i,k
    !print*,uo(i),u_ck,checko(k)
    !print*,vo(i),v_ck,checko(nol/nvar+k)
    !print*,to(i),t_ck,checko(2*nol/nvar+k)
    !print*,qo(i),q_ck,checko(3*nol/nvar+k)
    !print*,po(i),p_ck,checko(4*nol/nvar+k)
    !read*
!
! finally compute the localization factor
!
    rho(k)                = exp(-((olon(i)-m)**2.+(olat(i)-n)**2.+(olev(i)-L)**2.)/rscale**2)
    rho(nol/nvar+k)       = rho(k)
    rho(2*nol/nvar+k)     = rho(k)
    rho(3*nol/nvar+k)     = rho(k)
    rho(4*nol/nvar+k)     = rho(k)
!
! assign quality control for observations
!
    qc(k)                 = err_u(i)
    qc(nol/nvar+k)        = err_v(i)
    qc(2*nol/nvar+k)      = err_t(i)
    qc(3*nol/nvar+k)      = err_q(i)
    qc(4*nol/nvar+k)      = err_p(i)
   endif
  enddo
  Yb_m(:)        = 0
  do i           = 1,ne
   Yb_m(:)       = Yb_m(:) + Yb(:,i)
  enddo
  Yb_m(:)        = Yb_m(:)/ne
  do i           = 1,ne
   Yb(:,i)       = Yb(:,i) - Yb_m(:)
  enddo
  if (k.ne.nol/nvar) then
   print*,'letkf.exe: The number of obs in local patch does not match the array shape'
   stop
  endif
  return
  end subroutine global2local

  subroutine local2global(Xa,Q,np,ne,nx,ny,nz,ng,m,n,L,nxl,nzl,ig,model_flag,nme)
  implicit none
  integer nx,ny,nz,ne,np,ng,m,n,L,ig,nxl,nzl,model_flag,nme
  real Xa(np,ne),Q(np,np)
  integer istart,iend,jstart,jend,kstart,kend,ic,debug
  debug          = 0
!
! defining the start and end position of the local patch around ig
!
  istart         = m - nxl/2
  iend           = m + nxl/2
  jstart         = n - nxl/2
  jend           = n + nxl/2
  kstart         = L - nzl/2
  kend           = L + nzl/2
!  if (debug.ge.1) then
!   print*,'debug: istart,iend,jstart,jend,kstart,kend'
!   write(*,'(10I5)')istart,iend,jstart,jend,kstart,kend
!  endif
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
!  if (debug.ge.1) then
!   print*,'debug: fixed istart,iend,jstart,jend,kstart,kend and nvar'
!   write(*,'(10I5)')istart,iend,jstart,jend,kstart,kend,nvar
!  endif
!
! note that np = nxl*nxl*nzl*nvar
!
  ic             = nxl*nxl*nzl/2 + 1
  ua(m,n,L,1:ne) = Xa(ic,1:ne)
  va(m,n,L,1:ne) = Xa(np/nvar+ic,1:ne)
  ta(m,n,L,1:ne) = Xa(2*np/nvar+ic,1:ne)
  qa(m,n,L,1:ne) = Xa(3*np/nvar+ic,1:ne)
  pa(m,n,L,1:ne) = Xa(4*np/nvar+ic,1:ne)
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
  end subroutine local2global

  subroutine letkf_main(Xa,R,Xb,Xb_m,Yb,Yb_m,Yo,Xe,Xe_m,Q,nxl,nol,ne,     & 
                        ifactor,rho,da_flag,debug,model_flag,nme,         &
                        checko,mean_flag)
  use common_random
  use common_mtx
  implicit none
  integer           :: nxl,nol,ne,debug,model_flag,nme
  real, intent(in)  :: R(nol,nol),Xb(nxl,ne),Xb_m(nxl),rho(nol)
  real, intent(in)  :: Xe(nxl,nme),Xe_m(nxl)
  real, intent(in)  :: Yb(nol,ne),Yb_m(nol),Yo(nol),ifactor
  real, intent(out) :: Xa(nxl,ne)
  integer,intent(in):: checko(nol)
  real              :: Q(nxl,nxl),C(ne,nol),dy(nol),Qinv(nxl,nxl)
  integer           :: da_flag,mean_flag
  real              :: tem1(ne,ne),tem2(ne,ne),tem3(nxl,nxl)
  real              :: Wa(ne,ne),Pat(ne,ne),wa_m(ne),Rinv(nol,nol)
  integer           :: i1,j1,k1,i,j,k
  logical           :: check_nme
  check_nme     = model_flag.eq.1.and.nme.gt.1    
!
! compute the R inverted first, and couple with the localization
! factor
!
  call compute_Rinv(R,Rinv,nol,da_flag,checko)
  do i1         = 1,nol
   Rinv(i1,i1)  = Rinv(i1,i1)*rho(i1)
  enddo
!
! compute matrix C (step 4 in H06)
!
  C             = matmul(transpose(Yb),Rinv)
!
! compute matrix Pa tilde (step 5 in H06). Note that the model
! error matrix here is computed with assumption of two different
! ensembles; one for the initial condition, and one for the model
! error. In case these ensembles are combined into one, then Q
! will be included already in ${\tilde P}^a$, and we dont need
! Q separately (but do we need inflation factor in this case?).
!
  tem1          = matmul(C,Yb)
  if (check_nme) then   
   do j         = 1,ne
    tem1(j,j)   = tem1(j,j) + (ne-1)
   enddo   
   call mtx_inv(ne,tem1,Pat)
   tem2         = matmul(transpose(Xb),matmul(Q,Xb))
   Pat          = Pat + qcoef*tem2
  else
   do j         = 1,ne
    tem1(j,j)   = tem1(j,j) + (ne-1)/ifactor
   enddo
   call mtx_inv(ne,tem1,Pat) 
!
! tem2(i,j)  = tem1(i,j) + (ne-1)
! call mtx_inv(ne,tem1,tem2)
! Q          = matmul(Xb,matmul(tem2-tem1,transpose(Xb)))
! compute Q matrix from here to plot...to much time 
!
  endif 
!
! compute matrix Wa (step 6 in H06)
!
  tem1          = (ne-1)*Pat
  call mtx_sqrt(ne,tem1,Wa)
!
! compute wa_m and add it to Wa matrix (step 7 in H06)
!
  dy            = Yo - Yb_m
  if (check_nme) then
   call mtx_inv(nxl,Q,Qinv)
   wa_m         = qcoef*matmul(matmul(transpose(Xb),Qinv),Xb_m-Xe_m)
   wa_m         = matmul(Pat,matmul(C,dy)-wa_m) 
  else
   wa_m         = matmul(matmul(Pat,C),dy)
  endif 
  do j          = 1,ne
   Wa(:,j)      = Wa(:,j) + wa_m(:)
  enddo
!
! Computing the analysis ensemble members (step 8 in H06)
!
  Xa            = matmul(Xb,Wa)
  if (mean_flag.eq.0) then
    do j        = 1,ne
      Xa(:,j)   = Xa(:,j) + Xb_m(:)
    enddo
  else if (mean_flag.eq.1) then
    print*,' letkf_main: option mean_flag = 1. Will use GFS as bgd'
  else
    print*,' letkf_main: option mean_flag has to be 1 or 0'
    stop
  endif
  return
  end subroutine letkf_main

  subroutine observation_operator(nx,no,olon,H)
  implicit none
  integer nx,no,i,j,m,n,k,io,jo
  real H(no,nx),olon(no)
  H         = 0.
  do i      = 1,no
   j        = int(olon(i)+0.001)
   H(i,j)   = 1.
  enddo
  return
  end subroutine observation_operator

  subroutine localiztion_operator(nx,ny,no,nv,olon,olat,lopt)
  implicit none
  integer nx,ny,no,nv,i,j,m,n
  real lopt(nv,no),olat(no),olon(no),rscale,radi
  rscale    = 10.
  do i      = 1,nv
   do j     = 1,no
    m       = mod(i,nx)
    n       = i/nx + 1
    if (m.eq.0) then
     m      = nx
     n      = n - 1
    endif
    radi    = sqrt((olon(j)-m)**2. + (olat(j)-n)**2.)
    lopt(i,j) = exp(-(radi/rscale)**2)
   enddo
  enddo
  print*,'Checking Pat matrix'
  do i       = 1,20
   write(*,'(20F6.2)')(lopt(i,j),j=1,20)
  enddo
  return
  end subroutine localiztion_operator

  subroutine background_err_cov_mtx(B,nx,bvar,rscale)
  implicit none
  integer i,j,m,n,nx
  real B(nx,nx),rscale,radi,bvar
  do i      = 1,nx
   do j     = 1,nx
    radi    = sqrt((j-i)**2. + (j-i)**2.)
    B(i,j)  = bvar*bvar*exp(-(radi/rscale)**2)
   enddo
  enddo
  return
  end subroutine background_err_cov_mtx

  subroutine compute_Htilde(H,no,nv,Xf,ne,Ht)
  implicit none
  integer ne,nv,no
  real H(no,nv),Xf(nv,ne),Ht(no,ne)
  integer i,j,k,m,n
  Ht       = matmul(H,Xf)
  return
  end subroutine compute_Htilde

  subroutine observational_err_cov_mtx(R,no,qc,flag_qc)
  implicit none
  integer no,i,flag_qc
  real R(no,no),qc(no)
  R         = 0
  flag_qc   = 0
  do i      = 1,no
   if (abs(qc(i)).lt.1e-7) then
    print*,'Something wrong with quality control error...remove this data point'
    flag_qc = 1
   else
    R(i,i)  = abs(qc(i))**2
   endif
  enddo
  return
  end subroutine observational_err_cov_mtx

  subroutine compute_Rinv(R,Rinv,no,da_flag,checko)
  implicit none
  integer no,i,da_flag,checko(no)
  real R(no,no),Rinv(no,no)
  Rinv     = 0.
  do i     = 1,no
   if (checko(i).eq.1) then
    Rinv(i,i)  = 0
   else
    Rinv(i,i)  = 1/R(i,i)
   endif
   if (da_flag.eq.1.or.da_flag.eq.12.or.da_flag.eq.13.or.da_flag.eq.14.or.da_flag.eq.15.or.   &
       da_flag.eq.123.or.da_flag.eq.1234.or.da_flag.eq.12345)  then  ! no u assimilation
    if (i.le.no/nvar)                      Rinv(i,i) = 0.
   endif
   if (da_flag.eq.2.or.da_flag.eq.12.or.da_flag.eq.23.or.da_flag.eq.24.or.da_flag.eq.25.or.   &
       da_flag.eq.123.or.da_flag.eq.1234.or.da_flag.eq.12345) then   ! no v assimilation
    if (i.gt.no/nvar.and.i.le.2*no/nvar)   Rinv(i,i) = 0.
   endif
   if (da_flag.eq.3.or.da_flag.eq.13.or.da_flag.eq.23.or.da_flag.eq.34.or.da_flag.eq.35.or.   &
       da_flag.eq.345.or.da_flag.eq.1234.or.da_flag.eq.12345) then   ! no t assimilation
    if (i.gt.2*no/nvar.and.i.le.3*no/nvar) Rinv(i,i) = 0.
   endif
   if (da_flag.eq.4.or.da_flag.eq.14.or.da_flag.eq.24.or.da_flag.eq.34.or.da_flag.eq.45.or.   &
       da_flag.eq.345.or.da_flag.eq.1234.or.da_flag.eq.12345) then   ! no q assimilation
    if (i.gt.3*no/nvar.and.i.le.4*no/nvar) Rinv(i,i) = 0.
   endif
   if (da_flag.eq.5.or.da_flag.eq.15.or.da_flag.eq.25.or.da_flag.eq.35.or.da_flag.eq.45.or.   &
       da_flag.eq.345.or.da_flag.eq.2345.or.da_flag.eq.12345) then   ! no p assimilation
    if (i.gt.4*no/nvar)                    Rinv(i,i) = 0.
   endif
  enddo
  return
  end subroutine compute_Rinv

  subroutine obs_increment(H,no,nv,xfm,po,ne,obs_inc)
  implicit none
  integer no,ne,nv
  real H(no,nv),xfm(nv),po(no),obs_inc(nv)
  real tem
  integer i,j
  do i     = 1,no
   tem     = 0.
   do j    = 1,nv
    tem    = tem + H(i,j)*xfm(j)
   enddo
   obs_inc(i) = po(i) - tem
  enddo
  return
  end subroutine obs_increment

  subroutine analysis_mean(K,lopt,nv,no,xfm,obs_inc,xam)
  implicit none
  integer no,nv
  real K(nv,no),xfm(nv),obs_inc(no),xam(nv),lopt(nv,no)
  integer i,j
  do i     = 1,nv
   xam(i)  = xfm(i)
   do j    = 1,no
    xam(i) = xam(i) + lopt(i,j)*K(i,j)*obs_inc(j)
   enddo
  enddo
  return
  end subroutine analysis_mean

  subroutine convert_vector_array(Xa,nx,ny,nv,pa)
  implicit none
  integer nv,nx,ny
  real Xa(nv),pa(nx,ny)
  integer i,j,k,m,n
  do i   = 1,nx
   do j  = 1,ny
    m    = (j-1)*nx + i
    pa(i,j)  = Xa(m)
   enddo
  enddo
  return
  end subroutine convert_vector_array

  subroutine convert_vector_array1(Xa,nv,pa,nx,ny)
  implicit none
  integer nv,nx,ny
  real Xa(nv),pa(nx,ny)
  integer i,j,k,m,n
  do i       = 1,nx
   do j      = 1,ny
    m        = (j-1)*nx + i
    pa(i,j)  = Xa(m)
   enddo
  enddo
  return
  end subroutine convert_vector_array1

  subroutine convert_array_vector(ub,vb,zb,qb,nx,ny,nz,ne,ub_g,vb_g,zb_g,qb_g,ng)
  implicit none
  integer nx,ny,nz,ne,ng
  real ub(nx,ny,nz,ne),vb(nx,ny,nz,ne),zb(nx,ny,nz,ne),qb(nx,ny,nz,ne)
  real ub_g(ng,ne),vb_g(ng,ne),zb_g(ng,ne),qb_g(ng,ne)
  integer i,j,k,m,n,L,nv
  do i           = 1,ng
   n             = i/nx + 1
   m             = mod(i,nx)
   if (m.eq.0) then
    m            = nx
    n            = n - 1
   endif
   ub_g(i,:)     = ub(m,n,L,:)
   vb_g(i,:)     = vb(m,n,L,:)
   zb_g(i,:)     = zb(m,n,L,:)
  enddo
  return
  end subroutine convert_array_vector

  SUBROUTINE global_observation(uo_g,vo_g,zo_g,uo_gm,vo_gm,zo_gm,  &
                                ub,vb,zb,olat,olon,ne,nx,ny,ng,no)
  IMPLICIT NONE
  INTEGER nx,ny,ng,no,ne
  REAL ub(nx,ny,ne),vb(nx,ny,ne),zb(nx,ny,ne)
  REAL uo_g(no,ne),vo_g(no,ne),zo_g(no,ne)
  REAL uo_gm(no),vo_gm(no),zo_gm(no)
  REAL olat(no),olon(no)
  INTEGER i,j,k,m,n
!
! this is the first step in Hunt et al, for which H is simple = I because
! observations are given at the grid point.
!
  DO k           = 1,ne
   DO i          = 1,no
    m            = nint(olon(i))
    n            = nint(olat(i))
    uo_g(i,k)    = ub(m,n,k)
    vo_g(i,k)    = vb(m,n,k)
    zo_g(i,k)    = zb(m,n,k)
   ENDDO
  ENDDO
!
! compute the $\ba Y$ according to H05
!
  uo_gm(:)       = 0.
  vo_gm(:)       = 0.
  zo_gm(:)       = 0.
  do k           = 1,ne
   uo_gm(:)      = uo_gm(:) + uo_g(:,k)
   vo_gm(:)      = vo_gm(:) + vo_g(:,k)
   zo_gm(:)      = zo_gm(:) + zo_g(:,k)
  enddo
  uo_gm(:)       = uo_gm(:)/ne
  vo_gm(:)       = vo_gm(:)/ne
  zo_gm(:)       = zo_gm(:)/ne
!
! substract the mean of the total global obs from the total global obs
! to create pertrubation global obs
!
  do k           = 1,ne
   uo_g(:,k)     = uo_g(:,k) - uo_gm(:)
   vo_g(:,k)     = vo_g(:,k) - vo_gm(:)
   zo_g(:,k)     = zo_g(:,k) - zo_gm(:)
  enddo
  RETURN
  END SUBROUTINE global_observation

  SUBROUTINE vector2array(ua_g,va_g,za_g,ua,va,za,ng,ne,nx,ny)
  IMPLICIT NONE
  INTEGER nx,ny,ne,ng
  REAL ua_g(ng,ne),va_g(ng,ne),za_g(ng,ne)
  REAL ua(nx,ny,ne),va(nx,ny,ne),za(nx,ny,ne)
  INTEGER i,j,k,m,n
  DO i           = 1,nx
   DO j          = 1,ny
    m            = (j-1)*nx + i
    ua(i,j,:)    = ua_g(m,:)
    va(i,j,:)    = va_g(m,:)
    za(i,j,:)    = za_g(m,:)
   ENDDO
  ENDDO
  RETURN
  END SUBROUTINE vector2array

END MODULE letkf_core


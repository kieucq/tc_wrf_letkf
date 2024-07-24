      SUBROUTINE xsor(si,U,V,IMAX,JMAX,KMAX,dx,dy)
!     driver for routine sor
      INTEGER IMAX,JMAX,KMAX,NSTEP
      INTEGER i,j,k,midl,ii,jj
      real U(IMAX,JMAX,KMAX),V(IMAX,JMAX,KMAX)
      real PI,cf,g,dx,dy
      real si(IMAX,JMAX,KMAX),vot(imax,jmax)
      PARAMETER(NSTEP=8,PI=3.1415926,cf=5.e-5,g=9.81)
      double precision rjac,a(IMAX,JMAX),b(IMAX,JMAX),c(IMAX,JMAX) &
      ,d(IMAX,JMAX),e(IMAX,JMAX),f1(IMAX,JMAX),f2(IMAX,JMAX) &
      ,psi(imax,jmax)
      double precision dx2,dy2,vn,psiout

      dx2=2.*dx
      dy2=2.*dy

      do 12 i=1,IMAX
      do 12 j=1,JMAX
          a(i,j)=1.d0
          b(i,j)=1.d0
          c(i,j)=1.d0
          d(i,j)=1.d0
          e(i,j)=-4.d0
12    continue

      midl=IMAX/2+1
      rjac=cos(PI/IMAX)

      do 58 k=1,KMAX

      do i=1,imax
      do j=1,jmax
          vot(i,j)=0.0
          psi(i,j)=0.0
      end do
      end do

      do i=2,imax-1
      do j=2,jmax-1
        vot(i,j)=(v(i+1,j,k)-v(i-1,j,k))/dx2 &
                -(u(i,j+1,k)-u(i,j-1,k))/dy2
!       if(vot(i,j).lt.-1.e-5) then
!        vot(i,j)=-1.e-5
       if(vot(i,j).lt.-4.5e-5) then
!        print*,' i=',i,' j=',j,' k=',k,' vot=',vot(i,j)
        vot(i,j)=-4.5e-5
       end if
      end do
      end do

!      call fill0(vot,imax,jmax)

      do i=1,imax
      do j=1,jmax
          f1(i,j)=vot(i,j)*dx*dx
      end do
      end do
!      go to 2
!
!   compute the circle mean outflow
!
      vn=0.0
      do i=2,imax-1
          vn=vn-v(i,1,k)
      end do
      do j=2,jmax-1
          vn=vn+u(imax,j,k)
      end do
      do i=imax-1,2,-1
          vn=vn+v(i,jmax,k)
      end do
      do j=jmax-1,2,-1
          vn=vn-u(1,j,k)
      end do
          vn=vn-0.5*(v(1,1,k)+v(imax,1,k))  &
          +0.5*(u(imax,1,k)+u(imax,jmax,k)) &
          +0.5*(v(imax,jmax,k)+v(1,jmax,k)) &
          -0.5*(u(1,1,k)+u(1,jmax,k))
      psiout=vn/float(2*imax+2*jmax-4)*dx
!
!   For the lateral boundary condition of stream function
!   here through iteration to reduce the psi(1,1) so that the
!   zero flux lateral boundary condition is satisfied.
!
      num=0
 1    psi(1,1)=0.0
      num=num+1
      do i=2,imax
        psi(i,1)=psi(i-1,1)+0.5*(v(i-1,1,k)+v(i,1,k))*dx+psiout
      end do
      do j=2,jmax
        psi(imax,j)=psi(imax,j-1)  &
       -0.5*(u(imax,j-1,k)+u(imax,j,k))*dy+psiout
      end do
      do i=imax-1,1,-1
        psi(i,jmax)=psi(i+1,jmax)  &
       -0.5*(v(i+1,jmax,k)+v(i,jmax,k))*dx+psiout
      end do
      do j=jmax-1,1,-1
        psi(1,j)=psi(1,j+1)+0.5*(u(1,j+1,k)+u(1,j,k))*dy+psiout
      end do
      if(dabs(psi(1,1)).gt.0.1.and.num.lt.10) then
        psiout=psiout-0.9*psi(1,1)/float(2*imax+2*jmax-4)
!        print*,' k=',k,'psi(1,1)=',psi(1,1),' psi(1,2)=',psi(1,2)
        go to 1
      end if
      call sor(a,b,c,d,e,f1,psi,IMAX,JMAX,rjac,1.d-6)

      do 28 i=1,IMAX
      do 28 j=1,JMAX
       si(i,j,k)=psi(i,j)
28    continue


58    continue

       print*,' ok sor!'

      RETURN
      END SUBROUTINE xsor


      SUBROUTINE sor(a,b,c,d,e,f,u,imax,jmax,rjac,eps)
      INTEGER imax,jmax,MAXITS
      double precision rjac,a(imax,jmax),b(imax,jmax),c(imax,jmax), &
      d(imax,jmax),e(imax,jmax),f(imax,jmax),u(imax,jmax),EPS
      PARAMETER (MAXITS=1000)
      INTEGER ipass,i,j,jsw,l,lsw,n
      double precision anorm,anormf,omega,resid
      anormf=0.d0
      do 12 i=2,imax-1
        do 11 j=2,jmax-1
          anormf=anormf+abs(f(i,j))
11      continue
12    continue
      omega=1.d0
      do 16 n=1,MAXITS
        anorm=0.d0
        jsw=1
        do 15 ipass=1,2

          lsw=jsw
          do 14 j=2,imax-1
            do 13 l=lsw+1,jmax-1,2
              resid=a(j,l)*u(j+1,l)+b(j,l)*u(j-1,l)+c(j,l)*u(j,l+1)+d(j,  &
     l)*u(j,l-1)+e(j,l)*u(j,l)-f(j,l)
              anorm=anorm+abs(resid)
              u(j,l)=u(j,l)-omega*resid/e(j,l)
13          continue
            lsw=3-lsw
14        continue
          jsw=3-jsw
          if(n.eq.1.and.ipass.eq.1) then
            omega=1.d0/(1.d0-.5d0*rjac**2)
          else
            omega=1.d0/(1.d0-.25d0*rjac**2*omega)

          endif
15      continue
        if(anorm.lt.EPS*anormf)return
16    continue
      print*, 'MAXITS exceeded in sor'
      END
       

      SUBROUTINE fill0(a,nx,ny)
        integer i,j,nx,ny
        real a(nx,ny)
        do i=2,nx
         a(i,1)=2*a(i,2)-a(i,3)
         a(i,ny)=2*a(i,ny-1)-a(i,ny-2)
        end do
        do j=1,ny
         a(1,j)=2*a(2,j)-a(3,j)
         a(nx,j)=2*a(nx-1,j)-a(nx-2,j)
        end do
        return
        end

      SUBROUTINE fill1(a,nx,ny)
        integer i,j,nx,ny
        double precision a(nx,ny)
        do i=2,nx
         a(i,1)=2*a(i,2)-a(i,3)
         a(i,ny)=2*a(i,ny-1)-a(i,ny-2)
        end do
        do j=1,ny
         a(1,j)=2*a(2,j)-a(3,j)
         a(nx,j)=2*a(nx-1,j)-a(nx-2,j)
        end do
        return
        end

      SUBROUTINE fillz(a,nx,ny,nz)
        integer i,j,k,nx,ny,nz
        real a(nx,ny,nz)
        k=1
        do i=1,nx
         do j=1,ny
         a(i,j,k)=2*a(i,j,k+1)-a(i,j,k+2)
         enddo
        enddo
        k=nz
        do i=1,nx
         do j=1,ny
         a(i,j,k)=2*a(i,j,k-1)-a(i,j,k-2)
         enddo
        enddo
        return
        end

      SUBROUTINE fill2(a,nx,ny,nz)
      integer i,j,nx,ny,nz
      real a(nx,ny,nz)
      do K=1,nz
        do i=2,nx-1
         a(i,1,k)=2*a(i,2,k)-a(i,3,k)
         a(i,ny,k)=2*a(i,ny-1,k)-a(i,ny-2,k)
        end do
        do j=1,ny
         a(1,j,k)=2*a(2,j,k)-a(3,j,k)
         a(nx,j,k)=2*a(nx-1,j,k)-a(nx-2,j,k)
        end do
      end do
      return
      end

      SUBROUTINE fill3(a,nx,ny,nz)
      integer i,j,k,nx,ny,nz
      real a(nx,ny,nz)
      do K=2,nz-1
        do i=2,nx-1
         a(i,1,k)=2*a(i,2,k)-a(i,3,k)
         a(i,ny,k)=2*a(i,ny-1,k)-a(i,ny-2,k)
        end do
        do j=1,ny
         a(1,j,k)=2*a(2,j,k)-a(3,j,k)
         a(nx,j,k)=2*a(nx-1,j,k)-a(nx-2,j,k)
        end do
      end do
      DO J=1,NY
      DO I=1,NX
         a(i,j,1)=2*a(i,j,2)-a(i,j,3)
         a(i,j,nz)=2*a(i,j,nz-1)-a(i,j,nz-2)
      END DO
      END DO
      return
      end

      SUBROUTINE forcing_psi(rhs,psi,nx,ny,nz,dx,dy,dz,f,beta)
      IMPLICIT NONE
INTEGER :: nx,ny,nz                                          ! input: grid dims
real    :: dx,dy,dz                                          ! input: grid resolution
real    :: beta(nx,ny)                                       ! input: beta array                                     
real    :: psi(nx,ny,nz)                                     ! input: mean streamfunction
real    :: pv(nx,ny,nz)                                      ! input: mean pv
real    :: f(nx,ny)                                          ! input: corilois
real    :: rhs(nx,ny,nz)                                     ! output: forcing for psi 
real    :: psixx,psiyy,psixy,psiy                            ! local: the finite form of stream function
real    :: phixz,phiyz,phixx,phiyy,phizz                     ! local: the finite form of geopotential height
real    :: dx2,dy2,dxs,dys                                   ! local: temp vars  
INTEGER :: i,j,k                                             ! local: indexing

dx2    = 2*dx
dy2    = 2*dy
dxs    = dx*dx
dys    = dy*dy

DO k=1,nz
 DO i=2,nx-1
   DO j=2,ny-1
            psiy  = ( psi(i,j+1,k) - psi(i,j-1,k) ) / (2*dy) 
            psixx = ( psi(i+1,j,k) + psi(i-1,j,k) - 2.*psi(i,j,k) ) / (dx*dx) 
            psiyy = ( psi(i,j+1,k) + psi(i,j-1,k) - 2.*psi(i,j,k) ) / (dy*dy) 
            psixy = ( psi(i+1,j+1,k)+psi(i-1,j-1,k)-psi(i-1,j+1,k)-psi(i+1,j-1,k)) / (4*dx*dy) 
            rhs(i,j,k)=f(i,j)*(psixx+psiyy) + &
                       2*(psixx*psiyy-psixy*psixy) + &
!                       2*(psixx*psiyy-psixy*psixy) 
                       psiy*beta(i,j)
    ENDDO
  ENDDO
ENDDO

  call fill2(rhs,nx,ny,nz)
    RETURN
   END SUBROUTINE forcing_psi

  SUBROUTINE p3d_ddn_relax(phi,nx,ny,nz,dx,dy,dz,cx,cy,cz,rhs,phit,phib,nmax,epsi,debug)

  implicit none
  integer nx,ny,nz
  real phi(nx,ny,nz),phio(nx,ny,nz),denom(nx,ny,nz)
  real cx(nx,ny,nz),cy(nx,ny,nz),cz(nx,ny,nz)
  real cxodx2(nx,ny,nz),cyody2(nx,ny,nz),czodz2(nx,ny,nz)
  real rhs(nx,ny,nz),dx,dy,dz,phit(nx,ny),phib(nx,ny)
  real epsi,dx2,dy2,dz2,err,err0,numer,res,relaxfac
  integer i,j,k,loop,nmax,debug
  relaxfac=0.9
  dx2        = dx*dx
  dy2        = dy*dy
  dz2        = dz*dz
  do k       = 1,nz
   do i      = 1,nx
    do j     = 1,ny
     denom(i,j,k)=2*(cx(i,j,k)/dx2+cy(i,j,k)/dy2+cz(i,j,k)/dz2)
     cxodx2(i,j,k) = cx(i,j,k)/(dx2*denom(i,j,k))
     cyody2(i,j,k) = cy(i,j,k)/(dy2*denom(i,j,k))
     czodz2(i,j,k) = cz(i,j,k)/(dz2*denom(i,j,k))
    enddo
   enddo
  enddo
  iteration_loop : do loop = 1,nmax
   phio=phi
  DO k=2,nz-1
   DO j=2,ny-1
    DO i=2,nx-1

      if (k.eq.2) then
           phi(i,j,1) = phi(i,j,2) - phib(i,j)*dz
      elseif (k.eq.nz-1) then
           phi(i,j,nz) = phi(i,j,nz-1) + phit(i,j)*dz
      endif
       
      Res=-phi(i,j,k) -                                                  & 
        rhs(i,j,k)/denom(i,j,k) +                                     &
        cxodx2(i,j,k)*(phi(i+1,j,k)+phi(i-1,j,k)) +    &
        cyody2(i,j,k)*(phi(i,j+1,k)+phi(i,j-1,k)) +    &
        czodz2(i,j,k)*(phi(i,j,k+1)+phi(i,j,k-1))     
     phi(i,j,k)=phi(i,j,k)+relaxfac*res 
     ENDDO
    ENDDO
   ENDDO
   err       = 0.
   do i      = 2,nx-1
    do j     = 2,ny-1
     err     = err + abs(phi(i,j,nz/2)-phio(i,j,nz/2))
    enddo
   enddo
   if (loop.eq.1) err0 = err
   print *, loop,err0,err
!   if (debug.eq.1) write(*,'(I10,5F18.9)')loop,epsi,err0,err
   if (err0.ne.0.and.err/err0.lt.epsi) goto 56

   enddo iteration_loop 
56 continue
  RETURN
  END SUBROUTINE p3d_ddn_relax

  SUBROUTINE p3d_ddn_lipman(phi,nx,ny,nz,dx,dy,dz,cx,cy,cz,rhs,phit,phib,nmax,epsi,debug)
!
! Solver for 3D poisson with DDN bnd type, using Lipman iteration
!
  implicit none
  integer nx,ny,nz
  real phi(nx,ny,nz),phio(nx,ny,nz),denom(nx,ny,nz)
  real cx(nx,ny,nz),cy(nx,ny,nz),cz(nx,ny,nz)
  real cxodx2(nx,ny,nz),cyody2(nx,ny,nz),czodz2(nx,ny,nz)
  real rhs(nx,ny,nz),dx,dy,dz,phit(nx,ny),phib(nx,ny)
  real epsi,dx2,dy2,dz2,err,err0,numer
  integer i,j,k,loop,nmax,debug
  dx2        = dx*dx
  dy2        = dy*dy
  dz2        = dz*dz

  do k       = 1,nz
   do i      = 1,nx
    do j     = 1,ny
     denom(i,j,k)=2*(cx(i,j,k)/dx2+cy(i,j,k)/dy2+cz(i,j,k)/dz2)
     cxodx2(i,j,k) = cx(i,j,k)/dx2
     cyody2(i,j,k) = cy(i,j,k)/dy2
     czodz2(i,j,k) = cz(i,j,k)/dz2
    enddo
   enddo
  enddo
  iteration_loop : do loop = 1,nmax
   phio      = phi
   do k      = 2,nz-1
    do i     = 2,nx-1
     do j    = 2,ny-1
      if (k.eq.2) then
       phi(i,j,1) = phi(i,j,2) - phib(i,j)*dz
       numer = cxodx2(i,j,k)*(phi(i+1,j,k) + phi(i-1,j,k))              &
             + cyody2(i,j,k)*(phi(i,j+1,k) + phi(i,j-1,k))              &
             + czodz2(i,j,k)*(phi(i,j,k+1) + phi(i,j,k-1))              &
             - rhs(i,j,k) 
       phi(i,j,k) = numer/denom(i,j,k)
      elseif (k.eq.nz-1) then
       phi(i,j,nz) = phi(i,j,nz-1) + phit(i,j)*dz
       numer = cxodx2(i,j,k)*(phi(i+1,j,k) + phi(i-1,j,k))              &
             + cyody2(i,j,k)*(phi(i,j+1,k) + phi(i,j-1,k))              &
             + czodz2(i,j,k)*(phi(i,j,k+1) + phi(i,j,k-1))              &
             - rhs(i,j,k)
       phi(i,j,k) = numer/denom(i,j,k)
      else
       numer = cxodx2(i,j,k)*(phi(i+1,j,k) + phi(i-1,j,k))              &
             + cyody2(i,j,k)*(phi(i,j+1,k) + phi(i,j-1,k))              &
             + czodz2(i,j,k)*(phi(i,j,k+1) + phi(i,j,k-1))              &
             - rhs(i,j,k)
       phi(i,j,k) = numer/denom(i,j,k)
      endif
     enddo
    enddo
   enddo
   err       = 0.
   do i      = 2,nx-1
    do j     = 2,ny-1
     err     = err + abs(phi(i,j,nz/2)-phio(i,j,nz/2))
    enddo
   enddo
   if (loop.eq.1) err0 = err
   print *, loop,err0,err
!   if (debug.eq.1) write(*,'(I10,5F18.9)')loop,epsi,err0,err
   if (err0.ne.0.and.err/err0.lt.epsi) goto 54
  enddo iteration_loop
54 continue     
  return
  END SUBROUTINE p3d_ddn_lipman

  SUBROUTINE p2d_dd_lipman(phi,nx,ny,dx,dy,cx,cy,rhs,nmax,epsi,debug)
!
! Solver for 2D poisson with DD bnd type, using Lipman iteration
!
  IMPLICIT NONE
  INTEGER nx,ny
  real phi(nx,ny),rhs(nx,ny),cx(nx,ny),cy(nx,ny)
  real phio(nx,ny),dx,dy,dx2,dy2,denom(nx,ny),numer
  real err,err0,epsi
  INTEGER i,j,k,loop,nmax,debug
  dx2         = dx*dx
  dy2         = dy*dx
  DO i        = 1,nx
   DO j       = 1,ny
    denom(i,j)= 2*(cx(i,j)/dx2+cy(i,j)/dy2)
   ENDDO
  ENDDO
  iteration_loop : DO loop = 1,nmax
   phio      = phi
   err       = 0
   DO i      = 2,nx-1
    DO j     = 2,ny-1
     numer   = cx(i,j)*(phi(i+1,j) + phi(i-1,j))/dx2      &
             + cy(i,j)*(phi(i,j+1) + phi(i,j-1))/dy2      &
             - rhs(i,j)
     phi(i,j)= numer/denom(i,j)
     err     = err + abs(phi(i,j)-phio(i,j))
    ENDDO
   ENDDO
   IF (debug.eq.1) PRINT*,loop,err0,err
   IF (loop.eq.1) err0 = err
   IF (err0.ne.0.and.err/err0.le.epsi) goto 55 
   IF (loop.gt.1.and.err0.eq.0) goto 55
  ENDDO iteration_loop
55 CONTINUE
  RETURN
  END SUBROUTINE p2d_dd_lipman

  SUBROUTINE phimean(phi0,pt,pt0,nx,ny,nz,dz)
  IMPLICIT NONE
  real phi0(nx,ny,nz),pt(nx,ny,nz)
  real pt0,dz,G
  integer nx,ny,nz,i,j,k
  G=9.8
  phi0(:,:,1)=10*G
  do k=2,nz
  phi0(:,:,k)=phi0(:,:,k-1)+dz*G*pt(:,:,k)/pt0
  enddo
  RETURN
  END SUBROUTINE phimean 

  SUBROUTINE cal_pt1(phi,pt1,pt0,nx,ny,nz,dz)
  IMPLICIT NONE
  real phi(nx,ny,nz),pt1(nx,ny,nz)
  real pt0,dz,G,a0,dz2
  integer nx,ny,nz,i,j,k
  G=9.8
  a0=pt0/G 
  k=1
  dz2=dz*2
  do i=1,nx
    do j=1,ny
     pt1(i,j,k)=a0*(phi(i,j,k+1)-phi(i,j,k))/dz
    enddo
  enddo
  k=nz
  do i=1,nx
    do j=1,ny
    pt1(i,j,k)=a0*(phi(i,j,k)-phi(i,j,k-1))/dz
    enddo
  enddo
  do k=2,nz-1
    do i=1,nx
      do j=1,ny
      pt1(i,j,k)=a0*(phi(i,j,k+1)-phi(i,j,k-1))/dz2
      enddo
    enddo
  enddo
  RETURN
  END SUBROUTINE cal_pt1 


  SUBROUTINE p2d_dd_relax(phi,nx,ny,dx,dy,cx,cy,rhs,nmax,epsi,debug)
!
! Solver for 2D poisson with DD bnd type, using Lipman iteration
!
  IMPLICIT NONE
  INTEGER nx,ny
  real phi(nx,ny),rhs(nx,ny),cx(nx,ny),cy(nx,ny)
  real cxodx2(nx,ny),cyody2(nx,ny)
  real phio(nx,ny),dx,dy,dx2,dy2,denom(nx,ny),numer
  real err,err0,epsi,res,relaxfac
  INTEGER i,j,k,loop,nmax,debug
  relaxfac=1.5
  dx2         = dx*dx
  dy2         = dy*dy
  DO i        = 1,nx
   DO j       = 1,ny
    denom(i,j)= 2*(cx(i,j)/dx2+cy(i,j)/dy2)
    cxodx2(i,j) = cx(i,j)/(dx2*denom(i,j))
    cyody2(i,j) = cy(i,j)/(dy2*denom(i,j))
   ENDDO
  ENDDO
  iteration_loop : DO loop = 1,nmax
   phio      = phi
   err       = 0
   DO i      = 2,nx-1
    DO j     = 2,ny-1
      res     = cxodx2(i,j)*(phi(i+1,j) + phi(i-1,j))     &
              + cyody2(i,j)*(phi(i,j+1) + phi(i,j-1))     &
              - rhs(i,j)/denom(i,j) - phi(i,j)

      phi(i,j)=phi(i,j) + relaxfac*res 
     err     = err + abs(phi(i,j)-phio(i,j))
    ENDDO
   ENDDO
   IF (debug.eq.1) PRINT*,loop,err0,err
   IF (abs(err0).lt.1e-10) goto 55
   IF (loop.eq.1) err0 = err
   IF (err0.ne.0.and.err/err0.le.epsi) goto 55 
  ENDDO iteration_loop
55 CONTINUE
  RETURN
  END SUBROUTINE p2d_dd_relax


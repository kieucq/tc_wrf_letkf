MODULE common_utils
!
! [PURPOSE]:   Contains some frequent used routines
!
! [AUTHOR]:    Chanh Q. Kieu, Research Associate
!              Department of Atmospheric and Oceanic Science
!              University of Maryland, College Park
!              MD 20740, USA.  Email: kieucq@atmos.umd.edu
!
! [HISTORY]: - Created on Oct 8, 2008
!
  IMPLICIT NONE
  SAVE
  CONTAINS

  SUBROUTINE name_string(id,i_end,ofile)
  IMPLICIT NONE
  INTEGER id,i_start,i_end
  CHARACTER*100 ofile
  IF (id.le.9) THEN
   i_start     = i_end
   WRITE(ofile(i_start:i_end),'(1I1)')id
  ELSEIF (id.le.99)  THEN
   i_start     = i_end-1
   WRITE(ofile(i_start:i_end),'(1I2)')id
  ELSEIF (id.le.999) THEN
   i_start     = i_end-2
   WRITE(ofile(i_start:i_end),'(1I3)')id
  ELSEIF (id.le.9999) THEN
   i_start     = i_end-3
   WRITE(ofile(i_start:i_end),'(1I4)')id
  ELSE
   PRINT*,'Too many ensemble members...stop'
   STOP
  ENDIF
  RETURN
  END SUBROUTINE name_string

  SUBROUTINE error_handle(iopen)
  IMPLICIT NONE
  INTEGER iopen
  OPEN(iopen,file='error.status',status='new')
  WRITE(iopen,*)'There is no error...remove this file for the next run'
  CLOSE(iopen)
  RETURN
  END SUBROUTINE error_handle

  SUBROUTINE fill1(a,nx,ny)
  integer i,j,k,nx,ny
  REAL a(nx,ny)
   DO i=2,nx-1
    a(i,1)=2*a(i,2)-a(i,3)
    a(i,ny)=2*a(i,ny-1)-a(i,ny-2)
   ENDDO
   DO j=1,ny
    a(1,j)=2*a(2,j)-a(3,j)
    a(nx,j)=2*a(nx-1,j)-a(nx-2,j)
   ENDDO
  RETURN
  END SUBROUTINE fill1

  SUBROUTINE fill2(a,nx,ny,nz)
  integer i,j,k,nx,ny,nz
  REAL a(nx,ny,nz)
  DO K=1,nz
   DO i=2,nx-1
    a(i,1,k)=2*a(i,2,k)-a(i,3,k)
    a(i,ny,k)=2*a(i,ny-1,k)-a(i,ny-2,k)
   ENDDO
   DO j=1,ny
    a(1,j,k)=2*a(2,j,k)-a(3,j,k)
    a(nx,j,k)=2*a(nx-1,j,k)-a(nx-2,j,k)
   ENDDO
  ENDDO
  RETURN
  END SUBROUTINE fill2

  SUBROUTINE fill3(a,nx,ny,nz)
  integer i,j,k,nx,ny,nz
  REAL a(nx,ny,nz)
  DO K=2,nz-1
    DO i=2,nx-1
     a(i,1,k)=2*a(i,2,k)-a(i,3,k)
     a(i,ny,k)=2*a(i,ny-1,k)-a(i,ny-2,k)
    ENDDO
    DO j=1,ny
     a(1,j,k)=2*a(2,j,k)-a(3,j,k)
     a(nx,j,k)=2*a(nx-1,j,k)-a(nx-2,j,k)
    ENDDO
  ENDDO
  DO J=1,NY
  DO I=1,NX
     a(i,j,1)=2*a(i,j,2)-a(i,j,3)
     a(i,j,nz)=2*a(i,j,nz-1)-a(i,j,nz-2)
  END DO
  END DO
  RETURN
  END SUBROUTINE fill3
  
       SUBROUTINE smooth9p(arr,nx,ny,nz,ibgn,iend,jbgn,jend,kbgn,kend,npass)
!
!#######################################################################
!
!     PURPOSE:
!
!                                                        1 2 1
!     Smooth a 3-D array horizontally by the filter of { 2 4 2 }
!                                                        1 2 1
!
!#######################################################################
!
!     INPUT:
!
!     nx       Number of grid points in the x-direction
!     ny       Number of grid points in the y-direction
!     ibgn     First index in x-direction in the soomthing region.
!     iend     Last  index in x-direction in the soomthing region.
!     jbgn     First index in j-direction in the soomthing region.
!     jend     Last  index in j-direction in the soomthing region.
!
!     arr    3-D array
!
!     OUTPUT:
!
!     arr    3-D array
!
!     TEMPORARY:
!
!     tem1     Temporary 3-D array
!
!#######################################################################
!
!     Variable Declarations.
!
!
!#######################################################################
!
      implicit none
      integer nx            ! Number of grid points in the x-direction
      integer ny            ! Number of grid points in the y-direction
      integer nz            ! Number of grid points in the z-direction
      integer ibgn
      integer iend
      integer jbgn
      integer jend
      integer kbgn
      integer kend
      integer n,npass
      real arr (nx,ny,nz)   ! 3-D array
      real tem1(nx,ny,nz)   ! Temporary array
!
!#######################################################################
!
!     Misc. local variables:
!
!#######################################################################
!
      integer   i,j,k
      real wtf,wtfb,wtfc
!
!#######################################################################
!
!     Include files:
!
!#######################################################################
!

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!     Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      wtf  = 1.0/16.0
      wtfb = 1.0/12.0
      wtfc = 1.0/9.0
      tem1 = arr

      DO k = kbgn, kend

      DO n = 1,npass

      DO j = jbgn+1, jend-1
       DO i = ibgn+1, iend-1
        tem1(i,j,k) = wtf                                               &
      * (    arr(i-1,j-1,k) + 2.*arr(i,j-1,k) + arr(i+1,j-1,k)          &
        + 2.*arr(i-1,j,k) + 4.*arr(i,j,k) + 2.*arr(i+1,j,k)             &
        +    arr(i-1,j+1,k) + 2.*arr(i,j+1,k) + arr(i+1,j+1,k) )
       ENDDO
      ENDDO

      DO  j = jbgn+1, jend-1
        tem1(ibgn,j,k) = wtfb                                           &
      * ( 2.*arr(ibgn,j-1,k) +    arr(ibgn+1,j-1,k)                     &
        + 4.*arr(ibgn,j, k) + 2.*arr(ibgn+1,j,k)                        &
        + 2.*arr(ibgn,j+1,k) +    arr(ibgn+1,j+1,k) )

        tem1(iend,j,k) = wtfb                                           &
      * (    arr(iend-1,j-1,k)  + 2.*arr(iend,j-1,k)                    &
        + 2.*arr(iend-1,j,k)  + 4.*arr(iend,j,k)                        &
        +    arr(iend-1,j+1,k)  + 2.*arr(iend,j+1,k) )
      ENDDO

      DO i = ibgn+1, iend-1
        tem1(i,jbgn,k) = wtfb                                           &
      * ( 2.*arr(i-1,jbgn,k) + 4.*arr(i,jbgn,k) + 2.*arr(i+1,jbgn,k)    &
        + arr(i-1,jbgn+1,k) + 2.*arr(i,jbgn+1,k) + arr(i+1,jbgn+1,k) )

        tem1(i,jend,k) = wtfb                                           &
      * (  arr(i-1,jend-1,k) + 2.*arr(i,jend-1,k) + arr(i+1,jend-1,k)   &
        + 2.*arr(i-1,jend,k) + 4.*arr(i,jend,k) + 2.*arr(i+1,jend,k) )
      ENDDO

      tem1(ibgn,jbgn,k) = wtfc                                          &
       * ( 2.*arr(ibgn,jbgn+1,k) +  arr(ibgn+1,jbgn+1,k)                &
         + 4.*arr(ibgn,jbgn,k) + 2.*arr(ibgn+1,jbgn,k) )

      tem1(ibgn,jend,k) = wtfc                                          &
       * ( 4.*arr(ibgn,jend,k) + 2.*arr(ibgn+1,jend,k)                  &
         + 2.*arr(ibgn,jend-1,k) + arr(ibgn+1,jend-1,k) )

      tem1(iend,jbgn,k) = wtfc                                          &
       * (    arr(iend-1,jbgn+1,k) + 2.*arr(iend,jbgn+1,k)              &
         + 2.*arr(iend-1,jbgn,k) + 4.*arr(iend,jbgn,k) )

      tem1(iend,jend,k) = wtfc                                          &
        * ( 2.*arr(iend-1,jend,k) + 4.*arr(iend,jend,k)                 &
          +    arr(iend-1,jend-1,k) + 2.*arr(iend-1,jend,k) )

      ENDDO

      DO j = 1, ny
       DO i = 1, nx
        arr(i,j,k) = tem1(i,j,k)
       ENDDO
      ENDDO

      ENDDO
      RETURN
      END SUBROUTINE SMOOTH9P


END MODULE common_utils


MODULE common_interface
!=======================================================================
!
! [PURPOSE:] General constants and procedures
!
! [ATTENTION:] This module calls 'mt19937'
!
!=======================================================================
  IMPLICIT NONE
  PUBLIC
!-----------------------------------------------------------------------
! Variable size definitions
!-----------------------------------------------------------------------
  INTEGER,PARAMETER :: r_size=4 !kind(0.0d0)
  INTEGER,PARAMETER :: r_dble=8 !kind(0.0d0)
  INTEGER,PARAMETER :: r_sngl=kind(0.0e0)
!-----------------------------------------------------------------------
! Constants
!-----------------------------------------------------------------------
  REAL(r_size),PARAMETER :: pi=3.1415926535d0
  REAL(r_size),PARAMETER :: gg=9.81d0
  REAL(r_size),PARAMETER :: rd=287.0d0
  REAL(r_size),PARAMETER :: cp=1005.7d0
  REAL(r_size),PARAMETER :: re=6371.3d3
  REAL(r_size),PARAMETER :: r_omega=7.292d-5

CONTAINS
!-----------------------------------------------------------------------
! Mean
!-----------------------------------------------------------------------
SUBROUTINE com_mean(ndim,var,amean)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: var(ndim)
  REAL(r_size),INTENT(OUT) :: amean

  INTEGER :: i

  amean = 0.0d0
  DO i=1,ndim
    amean = amean + var(i)
  END DO
  amean = amean / REAL(ndim,r_size)

  RETURN
END SUBROUTINE com_mean
!-----------------------------------------------------------------------
! Standard deviation
!-----------------------------------------------------------------------
SUBROUTINE com_stdev(ndim,var,aout)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: var(ndim)
  REAL(r_size),INTENT(OUT) :: aout

  REAL(r_size) :: amean
  REAL(r_size) :: dev(ndim)
  INTEGER :: i

  CALL com_mean(ndim,var,amean)

  dev(:) = var(:) - amean

  aout = SQRT( SUM(dev*dev) / REAL(ndim,r_size) )

  RETURN
END SUBROUTINE com_stdev
!-----------------------------------------------------------------------
! Covariance
!-----------------------------------------------------------------------
SUBROUTINE com_covar(ndim,var1,var2,cov)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: var1(ndim)
  REAL(r_size),INTENT(IN) :: var2(ndim)
  REAL(r_size),INTENT(OUT) :: cov

  REAL(r_size) :: amean1,amean2
  REAL(r_size) :: dev1(ndim),dev2(ndim)

  CALL com_mean(ndim,var1,amean1)
  CALL com_mean(ndim,var2,amean2)

  dev1(:) = var1(:) - amean1
  dev2(:) = var2(:) - amean2

  cov = SUM( dev1*dev2 ) / REAL(ndim,r_size)

  RETURN
END SUBROUTINE com_covar
!-----------------------------------------------------------------------
! Correlation
!-----------------------------------------------------------------------
SUBROUTINE com_correl(ndim,var1,var2,cor)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: var1(ndim)
  REAL(r_size),INTENT(IN) :: var2(ndim)
  REAL(r_size),INTENT(OUT) :: cor

  REAL(r_size) :: cov,stdev1,stdev2

  CALL com_stdev(ndim,var1,stdev1)
  CALL com_stdev(ndim,var2,stdev2)
  CALL com_covar(ndim,var1,var2,cov)

  cor = cov/stdev1/stdev2

  RETURN
END SUBROUTINE com_correl
!-----------------------------------------------------------------------
! Anomaly Correlation
!-----------------------------------------------------------------------
SUBROUTINE com_anomcorrel(ndim,var1,var2,varmean,cor)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: var1(ndim)
  REAL(r_size),INTENT(IN) :: var2(ndim)
  REAL(r_size),INTENT(IN) :: varmean(ndim)
  REAL(r_size),INTENT(OUT) :: cor

  REAL(r_size) :: dev1(ndim),dev2(ndim)

  dev1 = var1 - varmean
  dev2 = var2 - varmean

  cor = SUM( dev1*dev2 ) / SQRT( SUM(dev1*dev1) * SUM(dev2*dev2) )

  RETURN
END SUBROUTINE com_anomcorrel
!-----------------------------------------------------------------------
! L2 Norm
!-----------------------------------------------------------------------
SUBROUTINE com_l2norm(ndim,var,anorm)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: var(ndim)
  REAL(r_size),INTENT(OUT) :: anorm

  anorm = SQRT( SUM(var*var) )

  RETURN
END SUBROUTINE com_l2norm
!-----------------------------------------------------------------------
! RMS (root mean square)
!-----------------------------------------------------------------------
SUBROUTINE com_rms(ndim,var,rmsv)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: var(ndim)
  REAL(r_size),INTENT(OUT) :: rmsv

  rmsv = SQRT( SUM(var*var) / REAL(ndim,r_size) )

  RETURN
END SUBROUTINE com_rms
!-----------------------------------------------------------------------
! Lanczos Filter (Low-pass) with cyclic boundary
!-----------------------------------------------------------------------
SUBROUTINE com_filter_lanczos(ndim,fc,var)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: fc    ! critical frequency in [0,pi]
  REAL(r_size),INTENT(INOUT) :: var(ndim)

  INTEGER,PARAMETER :: lresol=24

  REAL(r_size) :: weight(-lresol:lresol)
  REAL(r_size) :: varwk(1-lresol:ndim+lresol)
  INTEGER :: i,l
!
! Weight
!
  DO l=-lresol,-1
    weight(l) = sin(fc*real(l,8)) * sin(pi*real(l,8)/real(lresol,8)) &
      & * real(lresol,8) / pi / real(l,8) / pi / real(l,8)
  END DO
  DO l=1,lresol
    weight(l) = sin(fc*real(l,8)) * sin(pi*real(l,8)/real(lresol,8)) &
      & * real(lresol,8) / pi / real(l,8) / pi / real(l,8)
  END DO
  weight(0) = fc / pi
!
! Cyclic boundary
!
  DO i=1-lresol,0
    varwk(i) = var(ndim-i)
  END DO
  DO i=ndim+1,ndim+lresol
    varwk(i) = var(i-ndim)
  END DO
  varwk(1:ndim) = var(1:ndim)
!
! Filter
!
  var = 0.0d0
  DO i=1,ndim
    DO l=-lresol,lresol
      var(i) = var(i) + weight(l) * varwk(i+l)
    END DO
  END DO

  RETURN
END SUBROUTINE com_filter_lanczos
!-----------------------------------------------------------------------
! RAND (random number with uniform distribution)
!-----------------------------------------------------------------------
SUBROUTINE com_rand(ndim,var)
  USE mt19937
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(OUT) :: var(1:ndim)
  INTEGER :: idate(8)
  INTEGER :: i,iseed
  LOGICAL,SAVE :: first=.true.

  IF (first) THEN
    CALL DATE_AND_TIME(VALUES=idate)
    iseed = idate(8) + idate(7)*1000
    CALL init_genrand(iseed)
    first=.false.
  END IF

  DO i=1,ndim
    var(i) = genrand_res53()
  END DO

  RETURN
END SUBROUTINE com_rand
!-----------------------------------------------------------------------
! RANDN (random number with normal distribution)
!-----------------------------------------------------------------------
SUBROUTINE com_randn(ndim,var)
  USE mt19937
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(OUT) :: var(1:ndim)
  REAL(r_size) :: rnd(2)
  INTEGER :: idate(8)
  INTEGER :: i,iseed
  LOGICAL,SAVE :: first=.true.

  IF (first) THEN
    CALL DATE_AND_TIME(VALUES=idate)
    iseed = idate(8) + idate(7)*1000
    CALL init_genrand(iseed)
    first=.false.
  END IF

  IF( MOD(ndim,2)==0 ) THEN
    DO i=1,ndim/2
      rnd(1) = genrand_res53()
      rnd(2) = genrand_res53()
      var(i*2-1) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
      var(i*2) = sqrt( -2.0d0 * log( rnd(1) ) ) * cos( 2.0d0*pi*rnd(2) )
    END DO
  ELSE
    DO i=1,(ndim-1)/2
      rnd(1) = genrand_res53()
      rnd(2) = genrand_res53()
      var(i*2-1) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
      var(i*2) = sqrt( -2.0d0 * log( rnd(1) ) ) * cos( 2.0d0*pi*rnd(2) )
    END DO
    rnd(1) = genrand_res53()
    rnd(2) = genrand_res53()
    var(ndim) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
  END IF

  RETURN
END SUBROUTINE com_randn
!-----------------------------------------------------------------------
! TIMEINC
!-----------------------------------------------------------------------
SUBROUTINE com_timeinc_hr(iy,im,id,ih,incr)
  IMPLICIT NONE

  INTEGER,INTENT(INOUT) :: iy
  INTEGER,INTENT(INOUT) :: im
  INTEGER,INTENT(INOUT) :: id
  INTEGER,INTENT(INOUT) :: ih
  INTEGER,INTENT(IN) :: incr

  ih = ih + incr
  IF(ih>23) THEN
    ih = ih - 24
    id = id + 1
    IF(id==29.AND.im==2.AND.mod(iy,4)/=0) THEN
      id = 1
      im = 3
    ELSE IF(id==30.AND.im==2.AND.mod(iy,4)==0) THEN
      id = 1
      im = 3
    ELSE IF(id==31.AND.(im==4.OR.im==6.OR.im==9.OR.im==11)) THEN
      id = 1
      im = im + 1
    ELSE IF(id==32.AND.(im==1.OR.im==3.OR.im==5.OR.im==7.OR.im==8.OR.im==10)) THEN
      id = 1
      im = im + 1
    ELSE IF(id==32.AND.im==12) THEN
      id = 1
      im = 1
      iy = iy + 1
    END IF
  END IF

  RETURN
END SUBROUTINE com_timeinc_hr
!-----------------------------------------------------------------------
! TIMECONVERSION
!-----------------------------------------------------------------------
SUBROUTINE com_time2ymdh(itime,iy,im,id,ih)
  IMPLICIT NONE
  INTEGER(8),INTENT(IN) :: itime
  INTEGER,INTENT(OUT) :: iy
  INTEGER,INTENT(OUT) :: im
  INTEGER,INTENT(OUT) :: id
  INTEGER,INTENT(OUT) :: ih

  iy = INT(  itime / 1000000 )
  im = INT( (itime-iy*1000000) / 10000 )
  id = INT( (itime-iy*1000000-im*10000) / 100 )
  ih = INT(  itime-iy*1000000-im*10000-id*100 )

  RETURN
END SUBROUTINE com_time2ymdh

SUBROUTINE com_ymdh2time(iy,im,id,ih,itime)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iy
  INTEGER,INTENT(IN) :: im
  INTEGER,INTENT(IN) :: id
  INTEGER,INTENT(IN) :: ih
  INTEGER(8),INTENT(OUT) :: itime

  itime=iy*1000000+im*10000+id*100+ih

  RETURN
END SUBROUTINE com_ymdh2time

END MODULE common_interface

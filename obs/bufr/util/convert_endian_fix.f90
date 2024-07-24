program convert_endian_fix
!
!
  use kinds, only: i_kind,r_single
  implicit none

  integer(i_kind):: msig,mlat,inerr
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn,corp_avn,hwllp_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn,corqq_avn
  real(r_single),dimension(:,:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn,agv_avn

  integer :: k
!
!
!
  inerr=22
  open(inerr,file='berror_stats',form='unformatted',convert='BIG_ENDIAN')
  read(inerr)msig,mlat
  write(*,*) '1'

  allocate ( clat_avn(mlat),sigma_avn(1:msig) )
  allocate ( corz_avn(1:mlat,1:msig,1:4),corp_avn(mlat))
  allocate ( corqq_avn(1:mlat,1:msig) )
  allocate ( hwll_avn(0:mlat+1,1:msig,1:4),hwllp_avn(0:mlat+1) )
  allocate ( vztdq_avn(1:msig,0:mlat+1,1:4) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

  rewind inerr
  read(inerr)
  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  write(*,*) '2'
  read(inerr)corz_avn,corp_avn,corqq_avn
  write(*,*) '3'
  read(inerr)hwll_avn,hwllp_avn
  write(*,*) '4'
  read(inerr)vztdq_avn
  write(*,*) '5'
  read(inerr)agv_avn,bv_avn,wgv_avn
  write(*,*) '6'
  close(inerr)

  inerr=33
  open(inerr,file='berror_stats_little_endian',form='unformatted')
  rewind inerr
  write(inerr)msig,mlat
  write(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  write(inerr)corz_avn,corp_avn,corqq_avn
  write(inerr)hwll_avn,hwllp_avn
  write(inerr)vztdq_avn
  write(inerr)agv_avn,bv_avn,wgv_avn
  close(inerr)

end

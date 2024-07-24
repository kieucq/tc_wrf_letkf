 program bufr_decode_temperature
 implicit none
 integer      :: nv=1         ! number of data values at 1 point
 integer      :: nz=9         ! number of vertical levels 
 real(8)      :: obs(nv,nz)   ! observations at 1 point
 character(8) :: obstr        ! string describing obs
 integer      :: iret,idate   ! date to of observation
 character(8) :: msgtype      ! type of message
 integer      :: i,j,k
 obstr        = 'TOB'
!
! encode
!
 open(10,file='test1_data.bufr',action='read',form='unformatted')
 call datelen(10)
 call openbf(10,'IN',10)
  call readmg(10,msgtype,idate,iret)
  call ireadsb(10,iret)
  call ufbint(10,obs,nv,nz,iret,obstr)
  print*,'obs is ',obs(1,1)
 call closbf(10)
 end 

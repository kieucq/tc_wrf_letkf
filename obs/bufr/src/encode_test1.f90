 program bufr_encode_temperature
 implicit none
 integer      :: nv=1         ! number of data values at 1 point
 integer      :: nz=1         ! number of vertical levels 
 real(8)      :: obs(nv,nz)   ! observations at 1 point
 character(80):: obstr        ! string describing obs
 integer      :: iret,idate   ! date to of observation
 character(8) :: msgtype      ! type of message
 integer      :: i,j,k
!
! set values
!
 do k         = 1,nz
  obs(1,k)    = 10.15 + k
 enddo
 idate        = 2008120100
 msgtype      = 'ADPUPA'
 obstr        = 'TOB'
!
! encode
!
 open(20,file='encode_test1_table.txt')
 open(10,file='test1_data.bufr',action='write',form='unformatted')
 call datelen(10)
 call openbf(10,'OUT',20)
  call openmb(10,msgtype,idate)
   call ufbint(10,obs,nv,nz,iret,obstr)
   call writsb(10)
  call closmg(10)
 call closbf(10)
 end 

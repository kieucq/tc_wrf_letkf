program bufr_decode_CIMSS
!
! example of reading observations from bufr
!
 implicit none

 character(80):: hdstr='XOB YOB DHR'
 character(80):: obstr
 real(8)      :: hdr(3),obs_t(1,10),obs_z(1,10),obs_p(1,10)
 integer      :: ireadmg,ireadsb
 character(8) :: subset
 integer      :: unit_in=10
 integer      :: idate,iret,num_message,num_subset
 integer      :: i,j,k
 obstr        = 'TOB'
!
! decode
!
 open(unit_in,file='test_data_cimss.bufr',action='read',form='unformatted')
 call openbf(unit_in,'IN',unit_in)
 call datelen(10)
 num_message  = 0
 msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
  num_message=num_message+1
  num_subset = 0
  write(*,'(I10,I4,a10)') idate,num_message,subset
  sb_report: do while (ireadsb(unit_in) == 0)
    num_subset = num_subset+1
!    call ufbint(unit_in,hdr,3,1 ,iret,hdstr)
!    call ufbint(unit_in,obs_t,1,10,iret,'TOB')
!    call ufbint(unit_in,obs_z,1,10,iret,'ZOB')
!    call ufbint(unit_in,obs_p,1,10,iret,'POB') 
!    write(*,*)'num_subset = ',num_subset 
!    write(*,*)'num of level = ',iret
!    write(*,*)'obs lat (-90 to 90) =',hdr(1)
!    write(*,*)'obs lon (0 to 360)  =',hdr(2)
!    write(*,*)'obs time (wrt idate) =',hdr(3)
!    do i       = 1,10
!     write(*,'(1I5,3F18.2)')i,obs_t(1,i),obs_z(1,i),obs_p(1,i)
!    enddo
!    read*
  enddo sb_report
 enddo msg_report
 call closbf(unit_in)

end program

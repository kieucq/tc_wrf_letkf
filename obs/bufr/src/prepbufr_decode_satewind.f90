!
! NOTE: this program is designed to decode the prepbufr GDAS
!       observation, using the mnemonic acronym provided by NCEP.
!       The program requires the gdas data in litte endian on
!       a linux platform (conversion needed)
!
! HIST: - Jan 16, 2012: created based on sample bufr guide
!
! INPUT: read all observations out from prepbufr. 
!
! OUPUT: 1. a bufr table from prepbufr file
!        2. a decode output file containing satellite-derived wind
! 
!#################################################################
 program prepbufr_decode_satellite_wind
 implicit none
 integer, parameter :: mxmn=35, mxlv=250
 character(80)      :: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80)      :: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80)      :: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
 character(80)      :: oestr='POE QOE TOE NUL WOE NUL PWE     '
 character(8)       :: msgtype               ! message name
 character(8)       :: c_sid                 ! id station in char
 real(8)            :: hdr(mxmn)             ! header information
 real(8)            :: obs(mxmn,mxlv)        ! observed subset data  
 real(8)            :: qcf(mxmn,mxlv)        ! quality control factor
 real(8)            :: oer(mxmn,mxlv)        ! observed errors
 real(8)            :: rstation_id           ! id station in real
 integer            :: ireadmg,ireadsb       ! bufr return functions
 integer            :: unit_in=10,idate      ! unit/idate
 integer            :: nmsg,nsub             ! subset/mesg counts
 integer            :: i,k,iret              ! temporary vars 
 equivalence(rstation_id,c_sid)
!
! open file and looping data 
!
 open(20,file='prepbufr_decode_table_satwnd.txt')
 open(21,file='prepbufr_decode_data_satwnd.dat')
 open(unit_in,file='gdas.prepbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,20)
 call datelen(10)
 nmsg=0
 msg_report: do while (ireadmg(unit_in,msgtype,idate) == 0)
  if (msgtype.eq.'SATWND') then
   nmsg          = nmsg+1
   write(*,*)
   write(*,*)'reading message at nmsg = ',nmsg 
   write(*,'(3a,i10)')' message = ',msgtype,' time = ',idate
   nsub          = 0
   sb_report: do while (ireadsb(unit_in) == 0)
    nsub         = nsub+1
    call ufbint(unit_in,hdr,mxmn,1   ,iret,hdstr)
    call ufbint(unit_in,obs,mxmn,mxlv,iret,obstr)
    call ufbint(unit_in,oer,mxmn,mxlv,iret,oestr)
    call ufbint(unit_in,qcf,mxmn,mxlv,iret,qcstr)
    rstation_id  = hdr(1)
    write(*,'(i3,a5,9f14.1)')nsub,'qcf:',(qcf(i,1),i=1,7)
    write(21,'(a7,i10,a14,8f15.1)')'info:',iret,c_sid,(hdr(i),i=2,8)
    do k=1,iret
     write(21,'(i3,16f15.1)')k,(obs(i,k),oer(i,k),i=1,7),obs(i,8),obs(i,9)
    enddo
   enddo sb_report
  endif
 enddo msg_report
 call closbf(unit_in)
 end program

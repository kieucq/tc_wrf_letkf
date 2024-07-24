!
! this program is to decode a sample of bufr data file
!
    PROGRAM reading_bufr
    INTEGER, PARAMETER :: maxin=1600
    INTEGER lubfr,idate,iret
    CHARACTER cbfmsg*1600(1600),csubset*8
    INTEGER ibfmsg (maxin/4)
    EQUIVALENCE (cbfmsg (1), ibfmsg (1) )
    character*8  cmgtag,mtyp
    character*1  answer
    data mtyp /' '/
    INTEGER ntot
!
! start below
!
    lubfr        = 18
    OPEN(18,file='testdata.dat',status='old')
!
! Open a BUFR file for reading
!
    CALL openbf(18,'SEC3',18)
!
! specify that we want the date output is 10-digit format
!
    CALL DATELEN(10)
!
! count total reports
!
    READ*
    ntot = 0
    do while ( IREADNS(18,cmgtag,msgdt,ierr) .eq. 0)
      ntot = ntot + 1
    enddo
    rewind(18)
    print *,'ntot=',ntot
    READ*
!
! read message
!
!    CALL READMG(18,cbfmsg,idate,iret)
    iret = IREADMG(18,cbfmsg,idate)
    PRINT*,'iret  = ',iret
    PRINT*,'idate = ',idate
    CALL READERME (ibfmsg,18,csubset,idate,iret)
    PRINT*,'iret  = ',iret
    PRINT*,'idate = ',idate
!
! Starts the read of BUFR messages
!
!     do while(ireadmg.eq.0)  ! Starts the read of BUFR messages
!      do while(ireadsb.eq.0) ! Starts the read of BUFR subsets within the current message
!       ufbint
!       ufbrep
!       ufbseq
!      enddo ! ireadsb
!     enddo ! ireadmg
!
! Close the BUFR file
!
    CALL closbf(lubfr)
    end


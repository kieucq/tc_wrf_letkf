!
! [NOTE]: This program is to read a set of output from 
!         wrf_to_grads and compute ensemble mean, print
!         the ensemble of outputs in a single GRads file
!         and compose an atcf-formatted output for the
!         ensemble mean
!
!         Note that this program is tuned particularly 
!         for 6-h output to fit with the current bdeck
!         standard. The obs information will be displayed
!         incorrectly if different output intervals are
!         used.
!
! [HIST]: Mar, 1,  2010: created by Chanh Kieu
!         Mar, 25, 2012: updated further with ensemble 
!                        mean output
!
!=======================================================
!
    program timeseries_ensemble_plot
    implicit none
    integer, parameter     :: pt=500,pe=500
    real, dimension(pt,pe) :: pmin,vmax,rmw
    real                   :: po(pt),vo(pt)
    integer                :: nt,ne,ie,it,ne1,nt1
    character*50           :: ofile
    character              :: temp
    real                   :: mlon(pt),mlat(pt)
    real                   :: mpmin(pt),mvmax(pt) 
    character*108          :: aline(pt),atcf_string
    real p1(pt),v1(pt),p2(pt),v2(pt),p3(pt),v3(pt)
    real p4(pt),v4(pt),p5(pt),v5(pt),mis
    integer i,j,k,loop,irec,atcf_tmp,nsum
    irec         = 1
    ofile        = "ensemble_tseries_000.txt"
    print*,'Enter the number of ensemble members'
    read*,ne
    print*,'Enter the number of output times'
    read*,nt1
    print*,'The number of ensemble member: ',ne
    print*,'The number of output time    : ',nt1
!
! reading data
! 
    mis          = -99999
    pmin         = mis
    vmax         = mis
    rmw          = mis
    ie           = 1
8   continue    
    IF (ie.le.9) THEN
     WRITE(ofile(20:20),'(1I1)')ie
    ELSEIF (ie.le.99)  THEN
     WRITE(ofile(19:20),'(1I2)')ie
    ELSEIF (ie.le.999) THEN
     WRITE(ofile(18:20),'(1I3)')ie
    ELSE
     PRINT*,'Too many ensemble members...stop'
     STOP
    ENDIF
    open(13,file=ofile,status='unknown')
    print*,'Reading the member: ',ofile(1:30)
    it           = 1
9   read(13,'(2I5,F13.3,2I5,2F13.3)',end=10)loop,loop,pmin(it,ie),loop,loop,vmax(it,ie),rmw(it,ie)
    print*,'Get value ',pmin(it,ie),vmax(it,ie),rmw(it,ie)       
    it           = it + 1
    goto 9
10  continue    
    nt           = it - 1
    close(13)
    ie           = ie + 1
    if (ie.le.ne) goto 8
    print*,'Number of time is: ',nt
    print*,'Number of ensemble is: ',ne
!
! reading observational data. Note that we have assumed that we have exactly 21 obs
! from best track, each 6-h separated according to JTWC data set for 120-h. Note that
! this is tailored to 6-h bdeck as well as 6-h wrfout interval only.
!
    open(14,file='fort.12',status='old')
    vo           = mis
    po           = mis
    do i         = 1,nt1
      read(14,*,end=101)vo(i),po(i)
      print*,i,vo(i),po(i)
      vmax(i,ne+1)=0.5*vo(i)
      pmin(i,ne+1)=po(i)
      rmw(i,ne+1)= 100.
    enddo
101 continue
!    do i         = 1,nt1
!     if (mod(i,2).eq.1) then
!      read(14,*)vo(i),po(i)
!      print*,i,vo(i),po(i)
!     endif
!    enddo
!    do i         = 1,nt1
!     rmw(i,ne+1) = 50
!     if (mod(i,2).eq.0) then
!      vmax(i,ne+1)      = 0.5*(vo(i-1)+vo(i+1))/2.
!      pmin(i,ne+1)      = 0.5*(po(i-1)+po(i+1))
!     else
!      vmax(i,ne+1)      = vo(i)/2.
!      pmin(i,ne+1)      = po(i)
!     endif
!    enddo
!
! output the data in Grads format
!
    ne1          = ne+1
    open(8,file='ensemble_tseries.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt1*ne1)
    write(8,rec=irec)((pmin(i,j),i=1,nt1),j=1,ne1)  
    irec         = irec + 1
    write(8,rec=irec)((vmax(i,j),i=1,nt1),j=1,ne1)
    irec         = irec + 1
    write(8,rec=irec)((rmw(i,j),i=1,nt1),j=1,ne1)
    irec         = irec + 1            
!
! open the mean track ensemble to create an adeck for ensemble mean
!
    open(20,file='fort.16',status='old')
    open(21,file='fort.17',status='old')
    open(22,file='fort.18')
    read(20,*)
    read(20,*)
    read(20,*)
    read(20,*)
    it           = 1
11  read(20,*,end=12)loop,mlon(it),mlat(it)
    print*,'Get mean value: ',loop,mlon(it),mlat(it)
    it           = it + 1
    goto 11 
12  continue
    nt           = it - 1
    it           = 1
16  read(21,'(1A108)',end=17)aline(it)(1:108)
    print*,'Get atcf background: ',aline(it)(1:30)
    it           = it + 1
    goto 16
17  continue
    nt           = min(nt,it-1)
!
! compute the mean values
!
    do i         = 1,nt
     mpmin(i)    = 0.
     mvmax(i)    = 0.
     nsum        = 0
     do j        = 1,ne
      if (pmin(i,j).ne.mis.and.vmax(i,j).ne.mis) then
       mpmin(i)  = mpmin(i) + pmin(i,j)
       mvmax(i)  = mvmax(i) + vmax(i,j)
       nsum      = nsum + 1
      endif
     enddo 
     mpmin(i)    = mpmin(i)/nsum
     mvmax(i)    = mvmax(i)*1.944/nsum
    enddo
!
! convert mean lat/lon/pmin/vmax from number to string
!
    do i         = 1,nt
     atcf_string(1:108) = aline(i)(1:108)
!
! write lon
!
     atcf_tmp  = int(abs(mlon(i))*10)
     if (atcf_tmp.lt.10) then
      write(atcf_string(45:45),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(44:45),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(43:45),'(1I3)')atcf_tmp
     elseif (atcf_tmp.lt.10000) then
      write(atcf_string(42:45),'(1I4)')atcf_tmp
     else
      print*,'atcf format for lon is wrong...stop'
      stop
     endif
     if (mlon(i).gt.0) then
      write(atcf_string(46:46),'(1A1)')'E'
     else
      write(atcf_string(46:46),'(1A1)')'W'
     endif
!
! write lat
!
     atcf_tmp  = int(abs(mlat(i))*10)
     if (atcf_tmp.lt.10) then
      write(atcf_string(38:38),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(37:38),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(36:38),'(1I3)')atcf_tmp
     else
      print*,'atcf format for lat is wrong...stop'
      stop
     endif
     if (mlat(i).gt.0) then
      write(atcf_string(39:39),'(1A1)')'N'
     else
      write(atcf_string(39:39),'(1A1)')'S'
     endif
!
! write vmax
!
     atcf_tmp  = nint(mvmax(i))
     if (atcf_tmp.lt.10) then
      write(atcf_string(51:51),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(50:51),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(49:51),'(1I3)')atcf_tmp
     else
      print*,'atcf format for vmax is wrong...stop'
      stop
     endif
!
! write pmin
!
     atcf_tmp  = nint(mpmin(i))
     if (atcf_tmp.lt.10) then
      write(atcf_string(57:57),'(1I1)')atcf_tmp
     elseif (atcf_tmp.lt.100) then
      write(atcf_string(56:57),'(1I2)')atcf_tmp
     elseif (atcf_tmp.lt.1000) then
      write(atcf_string(55:57),'(1I3)')atcf_tmp
     elseif (atcf_tmp.lt.10000) then
      write(atcf_string(54:57),'(1I4)')atcf_tmp
     else
      print*,'atcf format for pmin is wrong...stop'
      stop
     endif
!
     write(22,'(1A108)')atcf_string(1:108)
     print*,atcf_string(1:108)
     print*,aline(i)(1:108)
    enddo
    end

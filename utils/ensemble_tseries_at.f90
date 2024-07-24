    program timeseries_ensemble_plot
    integer, parameter :: pt=500,pe=500
    real, dimension(pt,pe) :: pmin,vmax,rmw
    real                   :: po(pt),vo(pt)
    integer                :: nt,ne,ie,it,ne1,nt1
    character*50           :: ofile
    character              :: temp
    real p1(pt),v1(pt),p2(pt),v2(pt),p3(pt),v3(pt)
    real p4(pt),v4(pt),p5(pt),v5(pt)
    integer i,j,k,loop,irec
    irec         = 1
    ofile        = "ensemble_tseries_000.txt"
    print*,'Enter the number of ensemble time series'
    read*,ne
    print*,'Enter the number of output time'
    read*,nt1
    print*,'The number of ensemble member: ',ne
    print*,'The number of output time    : ',nt1
!
! reading data
! 
    pmin         = -99999
    vmax         = -99999
    rmw          = -99999
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
! reading observational data. Note that we have assumed that we have exactly 13 obs
! from best track, each 6-h separated according to JTWC data set for 72-h
!
    open(14,file='tc_message.txt',status='old')
    do i         = 1,nt1
     if (mod(i,2).eq.1) then
      read(14,*)temp,temp,temp,temp,vo(i),po(i)
      print*,i,temp,vo(i),po(i)
     endif
    enddo
    do i         = 1,nt1
     rmw(i,ne+1) = 50
     if (mod(i,2).eq.0) then
      vmax(i,ne+1)      = 0.5*(vo(i-1)+vo(i+1))/2.
      pmin(i,ne+1)      = 0.5*(po(i-1)+po(i+1))
     else
      vmax(i,ne+1)      = vo(i)/2.
      pmin(i,ne+1)      = po(i)
     endif
    enddo
!
! output the data in Grads format
!
    ne1          = ne+1
    open(8,file='ensemble_tseries.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt1*ne1*4)
    write(8,rec=irec)((pmin(i,j),i=1,nt1),j=1,ne1)  
    irec         = irec + 1
    write(8,rec=irec)((vmax(i,j),i=1,nt1),j=1,ne1)
    irec         = irec + 1
    write(8,rec=irec)((rmw(i,j),i=1,nt1),j=1,ne1)
    irec         = irec + 1            
    end

    program timeseries_ensemble_plot
    integer, parameter :: pt=500,pe=500
    real, dimension(pt,pe) :: pmin,vmax,rmw
    integer                :: nt,ne,ie,it
    character*50           :: ofile
    real p1(nt),v1(nt),p2(nt),v2(nt),p3(nt),v3(nt)
    real p4(nt),v4(nt),p5(nt),v5(nt)
    integer i,j,k,loop,irec
    irec         = 1
    ofile        = "ensemble_tseries_000.txt"
    print*,'Enter the number of ensemble time series'
    read*,ne
    print*,'The number of ensemble time series is: ',ne
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
! output the data in Grads format
!
    open(8,file='ensemble_tseries.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt*ne*4)
    write(8,rec=irec)((pmin(i,j),i=1,nt),j=1,ne)  
    irec         = irec + 1
    write(8,rec=irec)((vmax(i,j),i=1,nt),j=1,ne)
    irec         = irec + 1
    write(8,rec=irec)((rmw(i,j),i=1,nt),j=1,ne)
    irec         = irec + 1            

    end

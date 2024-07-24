!
! [NOTE]:      This program is to read all radiosonde station data and 
!              convert it into he fomat that the LETKF code needs
!
! [HISTORY]: - Created on May 25, 2010
!
! [AUTHOR]:    Chanh Q. Kieu 
!              Dept of atmospheric and oceanic science
!              Vietnam Nationaal University
!              Email: kieucq@atmos.umd.edu
!
! [REFERENCE]: Kieu, C. Q. (2005): arxiv.org
!
! [COPYRIGHT]: (C) 2010
!
!===========================================================================
!
      PROGRAM radionsode_conversion
      IMPLICIT none
!
! netcdf header information
!
      INTEGER, PARAMETER  :: n=4,nmax=10000,nlevel=1000
      INTEGER             :: ncid,status,nrs
      INTEGER             :: ndims,nvars,ngatts,unlimdimid
      INTEGER             :: uid,vid,tid,rid,pid
      INTEGER             :: tndim,tdim(n)
      REAL                :: dx,dy
!
! station information
!
      REAL                :: latrs(nmax),lonrs(nmax),ters(nmax)
      INTEGER             :: idrs(nmax),ilat,ilon
      REAL,ALLOCATABLE    :: latrs1(:),lonrs1(:),ters1(:)
      INTEGER,ALLOCATABLE :: idrs1(:)
!
! local vars
!
      INTEGER             :: nx,ny,nz,nt,mdate,iseq_num
      INTEGER             :: iday,ihour,imin,fid,nl,nobs 
      REAL                :: umax,vmax,tmax,rmax  
      REAL                :: ter,slp,xlat,xlon
      REAL                :: rtime,tfcst,restart,ptop,tem1,pi
      REAL,ALLOCATABLE    :: p1(:),t1(:),sp(:),di(:),td1(:),z1(:)
      CHARACTER*100       :: name,ofile,tem,wfile
      INTEGER             :: i,j,k,tem_len,debug,ne,nfile,ifile,irec
      LOGICAL             :: bogus
      irec               = 1 
!
! reading station id now
!
      OPEN(11,file='stationid.txt',status='old')
      READ(11,*)
      i                  = 1
12    READ(11,*,end=13)idrs(i),latrs(i),lonrs(i),ters(i)
      PRINT*,i,idrs(i),latrs(i),lonrs(i),ters(i)
      i                  = i + 1
      GOTO 12
13    CONTINUE
      CLOSE(11)
      nrs                = i - 1
      PRINT*,'Number of stations from the namelist is',nrs  
!
! looping over all stations finally and stag data into a single file
! obs.dat
!      
      bogus        = .false.
      slp          = 101325 
      mdate        = 05071700
      iseq_num     = 0
      wfile        = '00000.dat'
      OPEN(11,file='obs_littler.dat')
      ALLOCATE(p1(nlevel),t1(nlevel),sp(nlevel),di(nlevel))
      ALLOCATE(z1(nlevel),td1(nlevel))
      pi           = 4.*atan(1.)
      i            = 1
17    fid          = idrs(i)
      write(wfile(1:5),'(I5)')fid
      PRINT*,'Input station data is: ',i,wfile(1:len_trim(wfile))
      OPEN(12,file=wfile(1:len_trim(wfile)),status='old')     
      READ(12,*)
      READ(12,*)
      READ(12,*)
      READ(12,*)
      READ(12,*)
      j            = 1
18    READ(12,*,end=19)p1(j),z1(j),t1(j),td1(j),tem1,tem1,di(j),sp(j)
      PRINT*,j,p1(j),z1(j),t1(j),td1(j),di(j),sp(j)
      j            = j + 1
      GOTO 18
19    CONTINUE
      CLOSE(12)
      nl           = j - 1
      ter          = ters(i)
      xlat         = latrs(i)
      xlon         = lonrs(i)
      do k         = 1,nl
       p1(k)       = p1(k)*100.
       t1(k)       = t1(k)+273.15
       td1(k)      = td1(k)+273.15
      enddo

      if ( nl .eq. 1 ) then
         call write_obs (p1,z1,t1,td1,sp,di,                  &
                          slp, ter, xlat, xlon, mdate, nl,    & 
               '99001  Maybe more site info             ',    &
               'SURFACE DATA FROM ??????????? SOURCE    ',    &
               'FM-12 SYNOP                             ',    &
               'RADIOSONDE MONSOON EXP                  ',    &
               bogus,iseq_num,11,nlevel)
      else
         call write_obs (p1,z1,t1,td1,sp,di,                  &
                          slp, ter, xlat, xlon, mdate, nl,    &
               '99001  Maybe more site info             ',    &
               'SOUNDINGS FROM ????????? SOURCE         ',    &
               'FM-35 TEMP                              ',    &
               'WRF RADIOSONDE MONSOON EXP              ',    &
               bogus,iseq_num,11,nlevel)
      endif
      read*
20    i            = i + 1
      IF (i.le.nrs) GOTO 17
!
! close the data file and loop all files
!
      if (status.ne.0) print*,'Can not close the new file'
      
      END


      SUBROUTINE write_obs ( p , z , t , td , spd , dir ,            &
                            slp , ter , xlat , xlon , mdate , kx ,   &
       string1 , string2 , string3 , string4 , bogus , iseq_num ,    &
       iunit, nlevel )

      dimension p(nlevel),z(nlevel),t(nlevel)
      dimension td(nlevel),spd(nlevel),dir(nlevel)

      character *20 date_char
      character *40 string1, string2 , string3 , string4
      CHARACTER *84  rpt_format
      CHARACTER *22  meas_format
      CHARACTER *14  end_format
      logical bogus


      rpt_format =  ' ( 2f20.5 , 2a40 , '                            & 
                   // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '              &
                   // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
      meas_format =  ' ( 10( f13.5 , i7 ) ) '
      end_format = ' ( 3 ( i7 ) ) '

      write (date_char(9:16),fmt='(i8.8)') mdate
      if (mdate/1000000 .GT. 70 ) then
         date_char(7:8)='19'
      else
         date_char(7:8)='20'
      endif
      date_char(17:20)='0000'
      date_char(1:6)='      '

      WRITE ( UNIT = iunit , ERR = 19 , FMT = rpt_format )           &
              xlat,xlon, string1 , string2 ,                         & 
              string3 , string4 , ter, kx*6, 0,0,iseq_num,0,         &
              .true.,bogus,.false.,                                  &
               -888888, -888888, date_char ,                         &
               slp,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0, &
                     -888888.,0,                                     &
                     -888888.,0, -888888.,0, -888888.,0, -888888.,0, &
                     -888888.,0,                                     & 
                     -888888.,0, -888888.,0

      do 100 k = 1 , kx
         WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format )       &
                p(k), 0, z(k),0, t(k),0, td(k),0,                    &
                spd(k),0, dir(k),0,                                  & 
                -888888.,0, -888888.,0,-888888.,0, -888888.,0
100   continue
      WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format )          & 
       -777777.,0, -777777.,0,float(kx),0,                           & 
       -888888.,0, -888888.,0, -888888.,0,                           &
       -888888.,0, -888888.,0, -888888.,0,                           &
       -888888.,0
      WRITE ( UNIT = iunit , ERR = 19 , FMT = end_format )kx, 0, 0

      return
19    continue
      print *,'troubles writing a sounding'
      stop 19
      END


      subroutine interp_1d( a, xa, na, b, xb, nb, vertical_type, missing_value)
      implicit none
      integer, intent(in)              :: na, nb
      real, intent(in), dimension(na)  :: a, xa
      real, intent(in), dimension(nb)  :: xb
      real, intent(out), dimension(nb) :: b
      real, intent(in)                 :: missing_value

      integer                          :: n_in, n_out
      logical                          :: interp
      real                             :: w1, w2
      character (len=1)                :: vertical_type


      if ( vertical_type == 'p' ) then

      do n_out = 1, nb

        b(n_out) = missing_value
        interp = .false.
        n_in = 1

        do while ( (.not.interp) .and. (n_in < na) )

          if( (xa(n_in)   >= xb(n_out)) .and. &
              (xa(n_in+1) <= xb(n_out))        ) then
            interp = .true.
            w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
            w2 = 1. - w1
            b(n_out) = w1*a(n_in) + w2*a(n_in+1)
          end if
          n_in = n_in +1

        enddo

      enddo
  
      else

      do n_out = 1, nb

        b(n_out) = missing_value
        interp = .false.
        n_in = 1

        do while ( (.not.interp) .and. (n_in < na) )

          if( (xa(n_in)   <= xb(n_out)) .and. &
              (xa(n_in+1) >= xb(n_out))        ) then
            interp = .true.
            w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
            w2 = 1. - w1
            b(n_out) = w1*a(n_in) + w2*a(n_in+1)
          end if
          n_in = n_in +1

        enddo

        if (n_out.eq.1.and.(.not.interp)) then
!         b(1)  = a(1)  !+ (xb(1)-xa(1))*(a(2)-a(1))/(xa(2)-xa(1))
        endif 

        if (n_out.eq.nb.and.(.not.interp)) then
!         b(nb) = a(na) !+ (xb(nb)-xa(na))*(a(na)-a(na-1))/(xa(na)-xa(na-1))
        endif 

      enddo
  
      endif

      end subroutine interp_1d

      program  gwin2bufr
C 15 Oct 1998	! GMC	Geary Callan
C 01 Nov 1998   ! JMD   Jaime Daniels (ORA/FPDT)   Version 1.2
C 09 Aug 2002   ! YS    Yi Song       (RSIS/IPB)   Version 1.4
C  VERSION- 1.2  DATE: 11/01/98   PROGRAMMER: JAIME DANIELS (ORA/FPDT)
C  VERSION- 1.3  DATE: 08/09/99   PROGRAMMER: JAIME DANIELS (ORA/FPDT)
C  VERSION- 1.4  DATE: 08/09/02   PROGRAMMER: Yi Song (RSIS/IPB)
C
C  VERSION- 1.2  Add GOES-10,11,12 ID's
C  VERSION- 1.3  Change originating center to 160 (NOAA/NESDIS); was 98 (EUMETSAT). Modify
C                segment size from 2000m to 60m via use of PP_GEN_RES environmental variable
C  VERSION- 1.4  Modify to produce the BUFR messages less than 12KB in the 
C                AWIPS 10 sub-regions 
C              10/16/2002 Add the filter to throw away the bad quality data
C Author     : M.Dragosavac/M.Rohn ECMWF, July 1997.
C Purpose    : reads AMV in schema GWIN (Wisconsin) into amv_array 
C              to be saved as BUFR
C 
C Name       : GWIN2BUFR
C Parameters : mdin            : input  MD   file name
C              bufrout         : output BUFR file name
C
C Format     : gwin2bufr md_in bufr_out sat subcent
C              where <sat> is "a", "A", "t", "T", (Aqua/Terra), "OP" (MetOp), or 7-18 (NOAA)'
C Remarks    : 
C
C Examples   : GWIN2BUFR MDXX5001 bufr5101 07 2
C
C     input  : MDfile   'MDXX5001' 
C               with AMVectors from GOES MD   schema GWIN
C     output : BUFRfile 'bufr5101' 
C               with AMVectors from GOES BUFR subtype 87 
C      
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C 1     Declarations
C-----------------------------------------------------------------------
      implicit none

C-----------------------------------------------------------------------
C 1.1   Common declaration
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C 1.2   Constant declaration
C-----------------------------------------------------------------------
      integer num_noaas
      parameter (num_noaas = 19)      ! Through NOAA-19

C WCS3 - Generating Center parameter. Uncomment which one you want to use.

      integer       gen_centnum
      parameter (gen_centnum = 176)   ! CIMSS WMO number
C      parameter (gen_centnum = 160)  ! NESDIS WMO number
C      parameter (gen_centnum = 98)   ! EUMETSAT WMO number


      logical is_noaa_id   
      logical test 
      data    test      /.false./
C      data    test      /.true./		! DEBUG

C amv bad data value
      real    amv_bad
      data    amv_bad   /1.7e38/

C Missing data
      integer  miss
      data     miss /Z'80808080'/

C quality indicator
      integer       qi_RFI, qi_RFF, qi_FLAG, qi_EUM

C amv input array dimensions
      integer    max_nobs, nrow_in, ndata_in, nheader
      integer    scale_start, row_start

      parameter( max_nobs    = 20000,
     1           nrow_in     =     7,
     2           ndata_in    =   113,
     3           nheader     =    64,
     4           scale_start =   864,
     5           row_start   =  4096 )

C define maximum number of wind subsets per bulletin
      integer    max_winds
      parameter( max_winds    =310)
C define AWIPS maximum number of wind subsets per bulletin
      integer    max_awips
      parameter( max_awips    =280)
C
C Computational Method
      character*4  md_product
      character*12 fname
      integer      code_002152, code_002023
      integer      cloud_mask
C
C Computational Method
      character*4  md_type   

      real         code_002153
C
C Fortran
      integer ios
C
C-----------------------------------------------------------------------
C 1.3 Mcidas variable
C-----------------------------------------------------------------------
C MD file identifiers
      character*256  mdin

C MD file header
      integer       mdhd
      dimension     mdhd(nheader)

C MD Repeat groups
      integer       data_in_nreps
      integer       data_in_repsiz
      integer       data_in_reppos

C MD file column headers are not used

C Key information for input MD amv file
      integer       scales_in
      dimension     scales_in   (nrow_in+ndata_in)
      integer       nkeys_in, nrow_keys_in,ncol_keys_in,
     1              ndata_keys_in
      integer       number_of_rows, number_of_cols

C amv input array for MD record
      integer       rowh_in_temp
      dimension     rowh_in_temp ( nrow_in     )
      integer       rowh_in
      dimension     rowh_in (10, nrow_in     )
      integer*4     data_in 
      dimension     data_in (        ndata_in)
      real          md_in
      dimension     md_in   (nrow_in+ndata_in)

C Actual number of processed observations
      integer       nout

C Number of observations  in row
      integer       nobs

C-----------------------------------------------------------------------
C 1.4 amv channel information (different production channels)
C-----------------------------------------------------------------------
      integer       comp_method
      data          comp_method      /4/
C
      integer       gen_res
c      data          gen_res          /60/
c For MODIS winds, the resolution is 26*1000m
      data          gen_res          /26/
C
      integer       sat_id
C 
C
C-----------------------------------------------------------------------
C 1.5 BUFR variables
C-----------------------------------------------------------------------
      character*256  bufrout
C
      integer jsup,jsec0,jsec1,jsec2,jsec3,jsec4,jbufl
      integer jwork,jkey 
c
      parameter(jsup =   9,jsec0=   3,jsec1= 40,jsec2= 64 ,jsec3=    4,
     1          jsec4=   2,jbufl=150000,jwork=160000,jkey=46)
C
      integer kelem,kvals,iy,itl,iunit1,ierr,iz,iziw,iw
      integer kdlen,ktdlen,kbufl,sub_centre
      integer ns,ist,iend,izz,insoff,ins,nbout
C
      parameter (kelem=248)
      parameter (kvals=160000)
C
      integer    kbufr(jbufl)
      integer    ksup(jsup)  ,ksec0(jsec0),ksec1(jsec1)
      integer    ksec2(jsec2),ksec3(jsec3),ksec4(jsec4)
      integer    key  (jkey)
C
      real      values(kvals)
      integer   ktdlst(kelem)
C
      integer   kdata(20)
C
      real      rvind
C
      character*80 cvals(kvals)
      character*2  cppres
      character*3  cppsubcentre
C
      data rvind /1.7E38/
C
C hardcoded values    b_CODEno_MDindex (MDindex = 000 : all)
C-----------------------------------------------------------------------
      integer    b_miss, b_miss_02152,
     &           bufr_length, sub_type, bufr_repeat_vector,
     1           b_002020_000,
     2           b_008021_026,b_008021_031, b_008021_034, b_008021_0i3,
     2           b_008021_044, b_008021_0i2(2),
     3           b_002163_074,b_002163_077,b_002163_080,b_002163_083,
     4           b_002163_086,
     5           b_002152_000 

      parameter( bufr_length        = 248 ) 
      parameter( bufr_repeat_vector =  10 ) 
c
      integer    goes8_id, goes9_id, goes10_id, goes11_id, goes12_id
      integer    gms5_id
      integer    terra_id, aqua_id, metop_id
      integer    noaa_ids(num_noaas)
c
c
      real b_goes_002153_WV_HIGH, b_goes_002153_WV_MID,
     1     b_goes_002153_WV_LOW,
     2     b_goes_002153_VIS, b_goes_002153_IR
      real b_gms_002153_WV_HIGH,
     1     b_gms_002153_VIS, b_gms_002153_IR
c
      data    b_miss         /Z'80808080'/
      data    b_miss_02152   /Z'FFFFFFFF'/
      data    sub_type       / 87/
c
C satellite ID's
C-----------------------------------------------------------------------
      data    goes8_id        /252/
      data    goes9_id        /253/
      data    goes10_id       /254/
      data    goes11_id       /255/
      data    goes12_id       /256/
      data    gms5_id         /152/
      data    terra_id        /783/
      data    aqua_id         /784/
      data    metop_id        /4/
      data    noaa_ids        /0,0,0,0,0,0,707,0,201,202,203,
     &                         204,0,205,206,207,208,209,223/
C
C-----------------------------------------------------------------------
C frequency bands
C-----------------------------------------------------------------------
C assume KHz for now
C GOES
C IR
      parameter( b_goes_002153_IR        = 2.803738E13 )
C VIS
      parameter( b_goes_002153_VIS       = 4.615384E14 )
C WV 6.8 mu (imager)
      parameter( b_goes_002153_WV_HIGH       = 4.411764E13 )
C WV 7.0 mu (sounder band 10)
      parameter( b_goes_002153_WV_MID        = 4.285714E13 )
C WV 7.4 mu (sounder band 11)
      parameter( b_goes_002153_WV_LOW        = 4.054054E13 )

C GMS
C IR
      parameter( b_gms_002153_IR        = 2.727272E13 )
C VIS
      parameter( b_gms_002153_VIS       = 4.761904E14 )
C WV 6.8 mu (imager)
      parameter( b_gms_002153_WV_HIGH       = 4.411764E13 )
C
C time significance
C-----------------------------------------------------------------------
      data    b_008021_026   / 27/
      data    b_008021_031   /  4/
      data    b_008021_034   / 27/
      data    b_008021_044   /  2/
      data    b_008021_0i2   /  28, 29/
      data    b_008021_0i3   /  2/

C-----------------------------------------------------------------------
C flag 033254 handling
C
      integer*4     error2flag
C
C-----------------------------------------------------------------------
C height assignment methods
C-----------------------------------------------------------------------
      data    b_002163_074   /  1/
      data    b_002163_077   /  2/
      data    b_002163_080   /  3/
      data    b_002163_083   /  4/
      data    b_002163_086   /  7/

      data    b_002152_000   /  4194304/

C date
C-----------------------------------------------------------------------
      integer      year, month, day, hour, minute, second, iiyear
      integer      yearday, monthday, idate
      character*8  ydate
C descriptors
C-----------------------------------------------------------------------
      integer      code

C Array containing all observations
C-----------------------------------------------------------------------
      real         amv_array, bbbb
      dimension  amv_array(max_nobs,bufr_length)
      dimension  bbbb(max_nobs,bufr_length)

C Array containing all observations
C-----------------------------------------------------------------------

C character key
      character*4   key_char4

      integer*4    i4array
      EQUIVALENCE (i4array,key_char4)

C-----------------------------------------------------------------------
C Counter variables
C-----------------------------------------------------------------------
      integer      iobs, iobs_extracted, irow, ii, ikey, ibuf, imiss
      integer      irepeat, i, j
      integer      iwarning

C-----------------------------------------------------------------------
C argument variables
C-----------------------------------------------------------------------
      integer       iarg, narg, iargc
ccjd    
      integer       num1, num2, len_sat, isat
ccjd
      character*256 carg(4)
      character*2 sat
      character*2 subcent
      external      getarg,getenv

C-----------------------------------------------------------------------
C files
C-----------------------------------------------------------------------
      integer      iunit, istat
      integer      kount
c----------------file name number----------------
      character*2 filenumber(99)
      data filenumber /'01', '02', '03', '04', '05', '06', '07',
     & '08', '09', '10', '11', '12', '13', '14', '15', '16', 
     & '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', 
     & '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
     & '37', '38', '39', '40', '41', '42', '43', '44', '45', '46',
     & '47', '48', '49', '50', '51', '52', '53', '54', '55', '56',
     & '57', '58', '59', '60', '61', '62', '63', '64', '65', '66',
     & '67', '68', '69', '70', '71', '72', '73', '74', '75', '76',
     & '77', '78', '79', '80', '81', '82', '83', '84', '85', '86',
     & '87', '88', '89', '90', '91', '92', '93', '94', '95', '96',
     & '97', '98', '99' /
      character*10 fprefix
C-----------------------------------------------------------------------
C               Start of main program
C-----------------------------------------------------------------------
C               Get enviromental variable for resolution
C
      cppres=' '
      call getenv('PP_GEN_RES',cppres)
      WRITE (6,'(''PP_SUBCENTRE '',a2,i6)') cppres, i		! DEBUG 
      cppres = '60'						! GMC
c      cppres = '1'						! GMC
      i=index(cppres,' ')
      WRITE (6,'(''PP_SUBCENTRE '',a2,i6)') cppres, i		! DEBUG 
      if(i.eq.1) then
         print*,'PP_SUBCENTRE variable not set.'
         call exit(2)
      end if

c      read(cppres,'(i2)',iostat=ios) gen_res
c      if(ios.ne.0) then
c         print*,'Internal read error on cppres'
c         call exit(2)
c      end if
c      gen_res=gen_res*1000
C
C     sub_centre 1 = NOAA
C     sub_centre 2 = CIMSS
C

C initalize everything

      cppsubcentre=' '
      call getenv('PP_SUBCENTRE',cppsubcentre)
C      cppsubcentre='160'				! GMC

C WCS3 - gets generating center from parameter defined at beginning of program
C	 and converts it to a string for input into BUFR file

      write(cppsubcentre,'(I3)') gen_centnum

      i=index(cppsubcentre,' ')
      if(i.eq.1) then
         print*,'PP_SUBCENTRE variable not set.'
         call exit(2)
      end if
      read(cppsubcentre,'(i3)',iostat=ios) sub_centre

      if(ios.ne.0) then
         print*,'Internal read error on cppsubcentre.'
         call exit(2)
      end if
C-----------------------------------------------------------------------
C 1.1           Assign values from input parameters
C-----------------------------------------------------------------------

      narg=IARGC()
      if (narg.NE.4) then
         print*,'Usage: gwin2bufr md_in bufr_out sat subcent'
         print *, 'where <sat> is "a", "A", "t", "T", "OP", or 7-19'
         stop
      endif
      do 110 iarg=1,narg
         call getarg(iarg,carg(iarg))
 110  continue

      mdin    = carg(1)
      bufrout = carg(2)
      sat = carg(3)
      subcent= carg(4)
     
      len_sat = len_trim(sat)
      i=index(mdin,'MDXX')
      num1 = index(mdin, ' ')
c      num1 = len(mdin)
      j=index(bufrout,'I')
      num2 = index(bufrout, ' ')
      fprefix=bufrout(j:num2)
c      num2 = len(bufrout)
      print *, 'Subcenter as input: ', subcent
      print *, 'i and num1 are ',i, num1
      print *, 'j and num2 are ',j, num2
      print *, 'MD   input  file:    ', mdin(i:num1)
      print *, 'BUFR output file:    ', bufrout(j:num2)
      print *, 'Sat id is ', sat(1:len_sat)
ccjd      i=index(mdin,' ')
ccjd      i=i-1
ccjd      j=index(bufrout,' ')
ccjd      j=j-1
ccjd      print *, 'MD   input  file:    ', mdin(1:i)
ccjd      print *, 'BUFR output file:    ', bufrout(1:j)

cc          if ((fprefix(2:2) .eq. 'Q') .or. (fprefix(2:2) .eq. 'Q')) then
cc               cppres = '75'
cc      elseif ((fprefix(2:2) .eq. 'H') .or. (fprefix(2:2) .eq. 'J')) then
cc               cppres = '30'
cc      elseif ((fprefix(2:2) .eq. 'K') .or. (fprefix(2:2) .eq. 'M')) then
cc               cppres = '70'
cc      elseif ((fprefix(2:2) .eq. 'N') .or. (fprefix(2:2) .eq. 'P')) then
cc               cppres = '70'
cc           endif
      read(cppres,'(i2)',iostat=ios) gen_res
      if(ios.ne.0) then
         print*,'Internal read error on cppres'
         call exit(2)
      end if
      gen_res=gen_res*1000
      print *, 'gen_res=', gen_res
      print *, 'subcenter argument=', subcent

C-----------------------------------------------------------------------
C 1.2           Open input MD file and read keys and file headers
C-----------------------------------------------------------------------

      call pbopen(iunit, mdin(i:num1), "r", istat )
ccjd      call pbopen(iunit, mdin(1:i), 'r', istat )
      if ( istat .NE. 0) then
      print *,'Error pbopen : could not open file MDXX: ', mdin(i:num1)
      print *,'Error pbopen : could not open file MDXX: ', istat
ccjd        print *,'Error pbopen : could not open file MDXX: ', mdin(1:i)
        stop
      endif
c
c      call pbopen(iunit1,bufrout(j:num2),'w',ierr)
ccjd      call pbopen(iunit1,bufrout(1:j),'w',ierr)
c      if(ierr.ne.0) then
c         print*,'Open error on ',bufrout(j:num2)
ccjd         print*,'Open error on ',bufrout(1:j)
c         call exit(2)
c      end if

C read header
C-----------------------------------------------------------------------
      kount = nheader * 4
      call pbread2(iunit, mdhd, kount, istat) 
      if ( istat .LT. 0) then
      print *,'Error pbread2 : could not read MD header: ', mdin(i:num1)
ccjd        print *,'Error pbread2 : could not read MD header: ', mdin(1:i)
        stop
      endif

      if(test) then
         write(*, '(a)'   ) ' mdhd ------------------------------------'
         write(*, '(5i15)' ) ( mdhd(ikey), ikey=1,64 )
      endif

      number_of_rows = mdhd( 4)
      number_of_cols = mdhd( 5)
      nkeys_in       = mdhd( 6)
      nrow_keys_in   = mdhd( 7)
      ncol_keys_in   = mdhd( 8)
      ndata_keys_in  = mdhd( 9)
      data_in_nreps  = mdhd(12)
      data_in_repsiz = mdhd(13)
      data_in_reppos = mdhd(14)

      if (test) then
          print *,'MD Header     :'
          print *,'number_of_rows:', number_of_rows
          print *,'number_of_cols:', number_of_cols
          print *,'nkeys_in      :', nkeys_in
          print *,'nrow_keys_in  :', nrow_keys_in
          print *,'ncol_keys_in  :', ncol_keys_in
          print *,'ndata_keys_in :', ndata_keys_in
          print *,'data_in_nreps :', data_in_nreps
          print *,'data_in_repsiz:', data_in_repsiz
          print *,'data_in_reppos:', data_in_reppos
      endif

C read scales
C-----------------------------------------------------------------------
      call pbseek(iunit, scale_start*4, 0, istat)
      if ( istat .LT. 0) then
        print *,'Error pbseek : could not position MD scales: ',
     1           mdin(i:num1)
ccjd        print *,'Error pbseek : could not position MD scales: ',
ccjd     1           mdin(1:i)
        stop
      endif

      kount = nkeys_in * 4
      call pbread2(iunit, scales_in, kount, istat) 
      if ( istat .LT. 0) then
        print *,'Error pbread2 : could not read MD header: ',
     1            mdin(i:num1)
ccjd        print *,'Error pbread2 : could not read MD header: ',
ccjd     1            mdin(1:i)
        stop
      endif

      if(test) then
         write(*, '(a)'   ) ' scales ---------------------------------'
         write(*, '(5i15)' ) ( scales_in(ikey), ikey=1,nkeys_in)
      endif

C-----------------------------------------------------------------------
C 2.0           Loop over all observations
C-----------------------------------------------------------------------


      do 201 irow=1, number_of_rows
        do 202 ikey=1,nrow_in
         rowh_in(irow,ikey)   = miss
  202   continue     
  201 continue     
      do 204 ikey=1,ndata_in
         data_in(ikey)   = miss
  204 continue
      do 206 ikey=1,nrow_in+ndata_in
         md_in(ikey)     = miss
  206 continue
      do 208 iobs=1,max_nobs
        do 209 ibuf=1,bufr_length
          amv_array(iobs,ibuf) = b_miss
  209   continue
  208 continue

      call pbseek(iunit, row_start*4, 0, istat)
      if ( istat .LT. 0) then
        print *,'Error pbseek : could not position MD row: ',
     1            mdin(i:num1)
ccjd        print *,'Error pbseek : could not position MD row: ',
ccjd     1            mdin(1:i)
        stop
      endif

C-----------------------------------------------------------------------
C 2.1           loop over MD rows and read row headers
C-----------------------------------------------------------------------
      do 210 irow=1, number_of_rows

C       call pbread2(iunit, rowh_in(irow, 1:nrow_keys_in), 
        call pbread2(iunit, rowh_in_temp, 
     1               nrow_keys_in * 4, istat               )
        do iz=1, nrow_keys_in
          rowh_in(irow, iz) = rowh_in_temp(iz)
        enddo
        
        if ( istat .LT. 0) then
           print *,'Error pbread2 : could not read MD row : ', 
     1              mdin(i:num1)
ccjd           print *,'Error pbread2 : could not read MD row : ', 
ccjd     1              mdin(1:i)
        stop
        endif

        if(test) then
           write(*, '(a)'   ) ' row ---------------------------------'
           write(*, '(5i15)' ) (rowh_in(irow,ikey),ikey=1,nrow_keys_in)
        endif

        do 211 ikey=1,nrow_keys_in
           md_in(ikey) = rowh_in(irow,ikey)
  211   continue
c
C SAT ID's
C-------------------------------------------------------------
C
        if (rowh_in(irow,6) .EQ. 8) then
c          GOES 8
           b_002020_000    = 241
           rowh_in(irow,6) = goes8_id
           sat_id     = goes8_id
        endif
        if (rowh_in(irow,6) .EQ. 9) then
           b_002020_000    = 241
c          GOES 9
           rowh_in(irow,6) = goes9_id
           sat_id     = goes9_id
        endif
        if (rowh_in(irow,6) .EQ. 10) then
           b_002020_000    = 241
c          GOES 10
           rowh_in(irow,6) = goes10_id
           sat_id     = goes10_id
        endif
        if (rowh_in(irow,6) .EQ. 11) then
           b_002020_000    = 241
c          GOES 11
           rowh_in(irow,6) = goes11_id
           sat_id     = goes11_id
        endif
        if (rowh_in(irow,6) .EQ. 12) then
           b_002020_000    = 241
c          GOES 12
           rowh_in(irow,6) = goes12_id
           sat_id     = goes12_id
        endif
        if (rowh_in(irow,6) .EQ. 2) then
           b_002020_000    = 271
c          GMS 5
           rowh_in(irow,6) = gms5_id
           sat_id     = gms5_id
        endif
c In MD files MODIS' sat_id=7, so look for a 7
c JK: This also seems to be the case for AVHRR (including Metop)
c Until we get the new value from WMO for the 002020, just use 511
        if (rowh_in(irow,6) .EQ. 7) then
          if (sat(1:len_sat) .eq. 't' .or. 
     &        sat(1:len_sat) .eq. 'T') then
            b_002020_000    = 511
            rowh_in(irow,6) = terra_id
            sat_id          = terra_id
          elseif (sat(1:len_sat) .eq. 'a' .or. 
     &            sat(1:len_sat) .eq. 'A') then
            b_002020_000    = 511
            rowh_in(irow,6) = aqua_id
            sat_id          = aqua_id
          elseif (sat(1:len_sat) .eq. 'op' .or. 
     &            sat(1:len_sat) .eq. 'OP') then
            b_002020_000    = 511
            rowh_in(irow,6) = metop_id
            sat_id          = metop_id
	  else
            read(unit=sat(1:len_sat),fmt='(i2)') isat
	    if (isat .lt. 7 .or. isat .gt. num_noaas) then 
	      print *, 'Satellite not recognized.  Terminating.'
	      stop
	    endif
            b_002020_000 = 511
            rowh_in(irow,6) = noaa_ids(isat)
            sat_id = noaa_ids(isat)
          endif
          print *, 'sat_id is ', sat_id
        endif
C
C Close Row loop
C---------------
  210 continue  
C
C-------------------------------------------------------------
C 2.2           read data
C-------------------------------------------------------------
      iwarning = 0

      nout  = 0 
      imiss = 0 

C               loop over MD rows
C-----------------------------------------------------------------------
      do 221 irow=1, number_of_rows

        iobs_extracted = 0 
        nobs           = rowh_in(irow, 3)

        kount =   row_start
     1          + nrow_keys_in  * number_of_rows
     2          + ndata_keys_in * number_of_cols * (irow-1)

        call pbseek(iunit, kount*4, 0, istat)
        if ( istat .LT. 0) then
          print *,'Error pbseek : could not position MD row: ', 
     1             mdin(i:num1)
ccjd          print *,'Error pbseek : could not position MD row: ', 
ccjd 1             mdin(1:i)
          stop
        endif

C               Loop over observations in rows
C-------------------------------------------------------------

        do 223 iobs = 1, nobs

C               read data
C-----------------------------------------------------------------------
            istat=0
            call pbread(iunit, data_in, ndata_keys_in * 4, istat) 
            if ( istat .EQ. -2) then
              print*,'istat=',istat
              print *,'Error pbread : could not read MD Data : ', 
     1                 mdin(i:num1)
ccjd              print *,'Error pbread : could not read MD Data : ', 
ccjd     1                 mdin(1:i)
              call exit(2)
            else if ( istat .EQ. -1) then
              print *,'Error pbread   - ',
     1                'EOF encontered - row           :', irow
              print *,'               - iobs_extracted:', iobs_extracted
              print *,'               - bytes read    :', istat
C             stop
            endif

C 2.2.1       skip uncomplete observations
C-----------------------------------------
            if( (data_in( 6) .EQ. miss) .OR.
     1          (data_in( 7) .EQ. miss) .OR.
     2           istat       .EQ. -1     ) then
              imiss = imiss+1

            else
  
              iobs_extracted = iobs_extracted + 1

              if(  test .AND.
     1           ( (irow.EQ.1 .AND. iobs_extracted.EQ. 1).OR.
     2             (irow.EQ.2 .AND. iobs_extracted.EQ. 1))  ) then
                write(*, *  ) ' ----------------irow, iobs ', irow, iobs
                write(*,'(5i15)') (data_in(ikey),ikey=1,ndata_keys_in)
              endif

              do 224 ikey=1, ndata_keys_in
                if (data_in(ikey) .NE. miss) then
                   md_in(nrow_keys_in + ikey)
     1             = real(data_in(ikey))
     2               / (10.0**(scales_in(nrow_keys_in+ikey)))
                endif 
  224         continue 

C Assign amv_array
C-----------------
              ii = nout + iobs_extracted
c
              amv_array(ii,   1) = sat_id
              amv_array(ii,   2) = sub_centre      !98.
              amv_array(ii,   3) = b_002020_000
              amv_array(ii,   4) = gen_res
              amv_array(ii,   5) = gen_res

c
              year    = md_in(69) / 1000
              yearday = md_in(69) - (year*1000)

c              CALL YD2DAT(yearday,  year,  month,  day)
              call juldate(yearday,  year,  month,  day)

	      if (year .lt. 1000) then 
	              year = year + 1900
	      endif

              amv_array(ii,   6) = year
c              amv_array(ii,   6) = year
              amv_array(ii,   7) = month
              amv_array(ii,   8) = day
c
              hour    = md_in(70) / 10000
              minute  = md_in(70) /   100 - hour *   100
              second  = md_in(70)         - hour * 10000 
     1                                       - minute * 100
c
              if(hour.eq.24) then
                 hour=0
c                idate=(year-iiyear)*10000+month*100+day
                 idate=year*10000+month*100+day
                 write(ydate(1:8),'(i8)',iostat=ios) idate
                 if(ios.ne.0) then
                    print*,'Internal write error on idate.'
                    call exit(2)
                 end if
                 call daypn(ydate,1)
                 read(ydate(1:8),'(i8)',iostat=ios) idate
                 if(ios.ne.0) then
                    print*,'Internal read error on idate.'
                    call exit(2)
                 end if
c                year=idate/10000+iiyear
                 year=idate/10000
                 monthday=idate-(idate/10000)*10000
                 month=monthday/100
                 day=monthday-month*100
                 amv_array(ii,   6) = year
                 amv_array(ii,   7) = month
                 amv_array(ii,   8) = day
              end if
c
              amv_array(ii,   9) = hour
              amv_array(ii,  10) = minute
              amv_array(ii,  11) = second
C -- latitude :
              amv_array(ii,  12) = md_in( 11)
C -- longitude: Flip sign from MD convention to normal ("+ == west")
              amv_array(ii,  13) = md_in( 12) * (-1.0)
      WRITE(10,'(i4,2f10.4)') ii, amv_array(ii, 12), amv_array(ii, 13)	! DEBUG
C
C -- Check MD product
              write(key_char4,'(a4)',iostat=ios) rowh_in(irow,  5)
              if(ios.ne.0) then
                 print*,'Internal write error on rowh_in(irow,  5).'
                 call exit(2)
              end if
              md_product = key_char4

              cloud_mask = data_in( 52)
C
C -- code 002023 and code 002152
              write(key_char4,'(a4)',iostat=ios) data_in(  3)
              if(ios.ne.0) then
                 print*,'Internal write error on data_in(  3).'
                 call exit(2)
              end if

              md_type = key_char4
C
C -- intialize
              code_002023 = b_miss
              code_002152 = b_miss_02152
              code_002153 = b_miss
C
C -- Imager winds

              if ( sat_id .EQ. goes8_id .or.
     1             sat_id .EQ. goes9_id .or.
     2             sat_id .EQ. goes10_id .or.
     3             sat_id .EQ. goes11_id .or.
     4             sat_id .EQ. goes12_id .or.
     5             sat_id .EQ. terra_id .or.
     6             sat_id .EQ. aqua_id .or.
     7             sat_id .EQ. metop_id .or.
     8             is_noaa_id(sat_id,noaa_ids,num_noaas)) then
                if      (md_type .EQ. 'IR' ) then
                 code_002153 = b_goes_002153_IR
                 code_002023 = 1
                 code_002152 = 4194304
                 if (sat_id .EQ. terra_id .or.
     &               sat_id .EQ. aqua_id .or.
     &               sat_id .EQ. metop_id .or.
     &               is_noaa_id(sat_id,noaa_ids,num_noaas))
     &            code_002152 = 262144
                else if (md_type .EQ. 'VIS') then
                 code_002153 = b_goes_002153_VIS
                 code_002023 = 2
                 code_002152 = 4194304
                 if (sat_id .EQ. terra_id .or.
     &               sat_id .EQ. aqua_id .or.
     &               sat_id .EQ. metop_id .or.
     &               is_noaa_id(sat_id,noaa_ids,num_noaas))
     &            code_002152 = 262144
                else if (md_type .EQ. 'WV' ) then
                 code_002152 = 4194304
                 if (sat_id .EQ. terra_id .or.
     &               sat_id .EQ. aqua_id .or.
     &               sat_id .EQ. metop_id .or.
     &               is_noaa_id(sat_id,noaa_ids,num_noaas))
     &            code_002152 = 262144
                 code_002153 = b_goes_002153_WV_HIGH
                 if     (cloud_mask .EQ. 2 ) then
                    code_002023 = 3
                 else if(cloud_mask .EQ. 1 ) then
                    code_002023 = 5
                 else
                    code_002023 = 7
                 endif
                endif
              else if ( sat_id .EQ. gms5_id ) then
                if      (md_type .EQ. 'IR' ) then
                 code_002153 = b_gms_002153_IR
                 code_002023 = 1
                 code_002152 = 4194304
                else if (md_type .EQ. 'VIS') then
                 code_002153 = b_gms_002153_VIS
                 code_002023 = 2
                 code_002152 = 4194304
                else if (md_type .EQ. 'WV' ) then
                 code_002152 = 4194304
                 code_002153 = b_gms_002153_WV_HIGH
                 if     (cloud_mask .EQ. 2 ) then
C
                    code_002023 = 3
                 else if(cloud_mask .EQ. 1 ) then
                    code_002023 = 5
                 else
                    code_002023 = 7
                 endif
                endif
              end if
C
C -- Sounder winds
              if ( code_002023 .EQ. b_miss .OR.
     1             code_002152 .EQ. b_miss_02152     ) then

                if ( md_type .EQ. 'WV10' ) then
                 code_002152 = 2097152
                 code_002153 = b_goes_002153_WV_MID
                 if     (cloud_mask .EQ. 2 ) then
C -- Sounder band 10 CLOUD DRIFT
                    code_002023 = 3
                 else if(cloud_mask .EQ. 1 ) then
C -- Sounder band 10 WV CLEAR AIR
                    code_002023 = 5
                 else
                    code_002023 = 7
                 endif
                else if (md_type .EQ. 'WV11' ) then
                 code_002152 = 2097152
                 code_002153 = b_goes_002153_WV_LOW
                 if     (cloud_mask .EQ. 2 ) then
C -- Sounder band 11 CLOUD DRIFT
                    code_002023 = 3
                 else if(cloud_mask .EQ. 1 ) then
C -- Sounder band 11 WV CLEAR AIR
                    code_002023 = 5
                 else
                    code_002023 = 7
                 endif
                else
                 code_002152 = b_miss_02152
                 code_002023 = 15
                end if
              end if
C
              amv_array(ii,  14) = code_002152
              amv_array(ii,  15) = code_002023
C
              if(test) then
                print*, ' md_product: ', md_product
                print*, ' key_char4 ', key_char4
                print*, ' --> code_002023: ', code_002023
                print*, ' --> code_002152: ', code_002152
              end if
c
              amv_array(ii,  16) = md_in( 15)*100.
              amv_array(ii,  17) = md_in( 13)
              amv_array(ii,  18) = md_in( 14)
              amv_array(ii,  19) = code_002153
              amv_array(ii,  20) = b_miss
              amv_array(ii,  21) = md_in( 16)
C -- code 002163	height assignment from CH
              write(key_char4,'(a4)',iostat=ios) data_in( 11)
c      WRITE (6,'(''code 002163 '',a4,z9)') key_char4, data_in(11)	! DEBUG
              if(ios.ne.0) then
                 print*,'Internal write error on data_in( 11).'
                 call exit(2)
              end if
C 
c      WRITE (6,'(4hCODE,2x,A4)') key_char4		! DEBUG
              if      (key_char4 .EQ. 'WIN' ) then
                 code = 1 
              else if (key_char4 .EQ. 'HIST' ) then
                 code = 2 
              else if (key_char4 .EQ. 'H2O') then
                 code = 3 
              else if (key_char4 .EQ. 'CO2') then
                 code = 4
              else
                 code = 15
              end if
              amv_array(ii,  22) = code
c      WRITE (6,'(i4,i8,f8.2)') ii, code, amv_array(ii, 22)		! DEBUG
C -- code 002164	tracer corellation method from WM
              write(key_char4,'(a4)',iostat=ios) data_in( 29)
              if(ios.ne.0) then
                 print*,'Internal write error on data_in( 29).'
                 call exit(2)
              end if

              if      (key_char4 .EQ. 'LP') then
                 code = 0 
              else if (key_char4 .EQ. 'EN') then
                 code = 1 
              else if (key_char4 .EQ. 'CC') then
                 code = 2 
              else
                 code = 7 
              end if
              amv_array(ii,  23) = code
C -- code 008012 	land/sea flag from LAND
              if      (data_in( 51) .EQ. 1) then
                 code = 0			! land
              else if (data_in( 51) .EQ. 2) then
                 code = 1			! sea
              else
                 code = 3 
              end if
              amv_array(ii,  24) = code

ccjd      write (6,'(''zen '',f10.3)') md_in(60)	! DEBUG
              amv_array(ii,  25) = md_in( 60)	! satellite zenith angle
              amv_array(ii,  26) = b_miss
              amv_array(ii,  27) = b_008021_026
              amv_array(ii,  28) = b_miss
              amv_array(ii,  29) = b_miss
              amv_array(ii,  30) = b_miss
              amv_array(ii,  31) = b_miss
              amv_array(ii,  32) = b_008021_031
              amv_array(ii,  33) = b_miss
C -- FG wind vector
              irepeat=0
              ibuf = irepeat * bufr_repeat_vector
C
              amv_array(ii,  34+ibuf) = b_008021_034
C
              amv_array(ii,  35+ibuf) = b_miss
              amv_array(ii,  36+ibuf) = b_miss
              amv_array(ii,  37+ibuf) = b_miss
              amv_array(ii,  38+ibuf) = b_miss
              amv_array(ii,  39+ibuf) = b_miss
              amv_array(ii,  40+ibuf) = b_miss
              amv_array(ii,  41+ibuf) = b_miss
              amv_array(ii,  42+ibuf) = md_in( 37)
              amv_array(ii,  43+ibuf) = md_in( 38)
C
C -- Original wind vector
              irepeat=1
              ibuf = irepeat * bufr_repeat_vector
C
              amv_array(ii,  34+ibuf) = b_008021_044
C
              amv_array(ii,  35+ibuf) = b_miss
              amv_array(ii,  36+ibuf) = b_miss
              amv_array(ii,  37+ibuf) = b_miss
              amv_array(ii,  38+ibuf) = b_miss
              amv_array(ii,  39+ibuf) = b_miss
              amv_array(ii,  40+ibuf) = b_miss
              amv_array(ii,  41+ibuf) = b_miss
              amv_array(ii,  42+ibuf) = md_in( 45)
              amv_array(ii,  43+ibuf) = md_in( 46)
C
C -- i. vector
C -- only i=1,2 in BUFR position 3,4
              do 133 irepeat=0, 1
                ikey =  irepeat    * data_in_repsiz
                ibuf = (irepeat+2) * bufr_repeat_vector
C
                amv_array(ii,  34+ibuf) = b_008021_0i2(irepeat+1)
C
                if( md_in( data_in_reppos+1+ikey) .NE. miss ) then
                  hour    = md_in( data_in_reppos+1+ikey) / 10000
                  minute  = md_in( data_in_reppos+1+ikey) /   100
     1                      - hour *   100
                  second  = md_in( data_in_reppos+1+ikey)
     1                      - hour * 10000
     2                      - minute * 100
                  if( hour   .GE. 0 .AND. hour   .LE. 24 .AND.
     1                minute .GE. 0 .AND. minute .LE. 60 .AND.
     2                second .GE. 0 .AND. second .LE. 60      ) then
                    amv_array(ii, 35+ibuf) = hour
                    amv_array(ii, 36+ibuf) = minute
                    amv_array(ii, 37+ibuf) = second
                  else
                   iwarning = iwarning + 1
                   if( test ) then
                    print *,'TIME wrong in repeat group:', irepeat+1
                    print *,'     hour     ', hour
                    print *,'     minute   ', minute
                    print *,'     second   ', second
                   endif
                  endif
                endif
                  amv_array(ii, 38+ibuf) = b_miss
                  amv_array(ii, 39+ibuf) = b_miss
                  amv_array(ii, 40+ibuf) = b_miss
                  amv_array(ii, 41+ibuf) = b_miss
                if( md_in( data_in_reppos+4+ikey) .NE. miss .AND.
     1              md_in( data_in_reppos+5+ikey) .NE. miss      ) then
                  if( md_in( data_in_reppos+4+ikey) .GE.   0 .AND.
     1                md_in( data_in_reppos+4+ikey) .LE. 360 .AND.
     2                md_in( data_in_reppos+5+ikey) .GE.   0 .AND.
     3                md_in( data_in_reppos+5+ikey) .LT. 500     ) then
                    amv_array(ii, 42+ibuf)=md_in(data_in_reppos+4+ikey)
                    amv_array(ii, 43+ibuf)=md_in(data_in_reppos+5+ikey)
                  else
                   iwarning = iwarning + 1
                   if( test ) then
                    print *,'DIR/SPD wrong in repeat group:', irepeat+1
                    print *,'     DIR   ', md_in(data_in_reppos+4+ikey)
                    print *,'     SPD   ', md_in(data_in_reppos+5+ikey)
                   endif
                  endif
                endif
C-----------------------------------------------------------------------
  133         continue
C
C -- height assignments
C IRW HEIGHT ASSIGNMENT		PWWI Window channel height assignment (IRW)
              amv_array(ii,  74) = b_002163_074
              if(md_in( 31).ne.miss)  then
                 amv_array(ii,  75) = md_in( 31)*100
              else
                 amv_array(ii,  75) = b_miss
              end if
              amv_array(ii,  76) = md_in( 32)
C WV HEIGHT ASSIGNMENT		PWHI Histogram height assignment (WV)
              amv_array(ii,  77) = b_002163_077
              if(md_in( 27).ne.miss)  then
                 amv_array(ii,  78) = md_in( 27)*100
              else
                 amv_array(ii,  78) = b_miss
              end if
              amv_array(ii,  79) = md_in( 28)
C H2O INTERCEPT HEIGHT ASSIGNMENT	PW8A H2O intercept height assignment
              amv_array(ii,  80) = b_002163_080
              if(md_in( 23).ne.miss)  then
                 amv_array(ii,  81) = md_in( 23)*100
              else
                 amv_array(ii,  81) = b_miss
              end if
              amv_array(ii,  82) = md_in( 24)
C CO2 SLICING HEIGHT ASSIGNMENT		PW58 CO2 slicing height assignment
              amv_array(ii,  83) = b_002163_083
              if(md_in( 19).ne.miss)  then
                 amv_array(ii,  84) = md_in( 19)*100
              else
                 amv_array(ii,  84) = b_miss
              end if
              amv_array(ii,  85) = md_in( 20)
C PRIMARY HEIGHT ASSIGNMENT	OPW Original pressure of average vector
              amv_array(ii,  86) = b_002163_086
              if(md_in( 47).ne.miss)  then
                 amv_array(ii,  87) = md_in( 47)*100
              else
                 amv_array(ii,  87) = b_miss
              end if
              amv_array(ii,  88) = b_miss
              amv_array(ii,  89) = b_miss
              amv_array(ii,  90) = b_miss
              amv_array(ii,  91) = b_miss
              amv_array(ii,  92) = b_miss
              amv_array(ii,  93) = b_miss
              amv_array(ii,  94) = b_miss
              amv_array(ii,  95) = b_miss
              amv_array(ii,  96) = b_miss
              amv_array(ii,  97) = b_miss
              amv_array(ii,  98) = b_miss
              amv_array(ii,  99) = b_miss
              amv_array(ii, 100) = b_miss
              amv_array(ii, 101) = b_miss
              amv_array(ii, 102) = b_miss
              amv_array(ii, 103) = b_miss
C -------------------------------------------
C -- end   template
C -------------------------------------------
C -- start quality block
C -------------------------------------------
C --- intialize
              do iy=104,bufr_length
                amv_array(ii,iy)=0.
              end do

Cgmc          amv_array(ii, 104) = 222000
Cgmc          amv_array(ii, 105) = 236000
              amv_array(ii, 104) = 222000
              amv_array(ii, 105) = 236000
C -- data present
              do iy=106,208
              amv_array(ii,iy)=1.
              if(iy.eq.122) amv_array(ii,iy)=0.
              if(iy.eq.123) amv_array(ii,iy)=0.
              if(iy.eq.147) amv_array(ii,iy)=0.
              if(iy.eq.148) amv_array(ii,iy)=0.
              if(iy.eq.157) amv_array(ii,iy)=0.
              if(iy.eq.158) amv_array(ii,iy)=0.
              if(iy.eq.167) amv_array(ii,iy)=0.
              if(iy.eq.168) amv_array(ii,iy)=0.
              if(iy.eq.177) amv_array(ii,iy)=0.
              if(iy.eq.178) amv_array(ii,iy)=0.
              end do
c
c             Generating centre/application
c
C              amv_array(ii,209)=160.

C GENERATING CENTER FOR CIMSS
              amv_array(ii,209)=gen_centnum
C -- Use generating application '1' for RFI
C -- Simon Elliott 30-10-3001
              amv_array(ii,210)=1.

C -- pick the quality estimate
c -- Now qi_RFI is the new quality index
              qi_RFI=md_in( 68 )
              qi_RFF=md_in( 51 )
	      
              qi_EUM=md_in( 65 )

	      if (qi_EUM .lt. 0) then
		qi_EUM=md_in( 68 )
	      endif

	      if(qi_RFI .lt. 0) qi_RFI=qi_EUM

              if ( qi_RFI .LT. 0    .AND.
     1             qi_RFI .GT. 100      ) then
                qi_RFI = 0
              end if
              if ( qi_RFF .LT. 0   .AND.
     1             qi_RFF .GT. 100      ) then
                qi_RFF = 0
              endif
              if ( qi_EUM .LT. 0   .AND.
     1             qi_EUM .GT. 100      ) then
                qi_EUM = 0
              endif
              if (md_in( 68 ) .LT. 50 ) then
              write(11, *) md_in(68)
              endif
c
C -- code 033007 : confidence
              amv_array(ii,211)=qi_RFI
              amv_array(ii,212)=qi_RFI

c -- Generating centre/application
C -- code 033007 : confidence
              amv_array(ii,221)=222000
              amv_array(ii,222)=237000
C WCS3 - original code
C              amv_array(ii,223)=160.

C WCS3 - New code. Stores generating center parameter into array variable
              amv_array(ii,223)=gen_centnum

C -- Use generating application '2' for RFF
C -- Simon Elliott 30-10-3001
              amv_array(ii,224)=2.

              amv_array(ii,225)=qi_RFF
              amv_array(ii,226)=qi_RFF

c -- Generating centre/application
C -- code 033007 : confidence
              amv_array(ii,235)=222000
              amv_array(ii,236)=237000
C WCS3 - Old code
C              amv_array(ii,237)=160.
C WCS3 - Stores generating center parameter into array variable
              amv_array(ii,237)=gen_centnum

C -- Use generating application '3' for EUMETSAT type QI
C -- Simon Elliott 31-10-3001
              amv_array(ii,238)=3.

              amv_array(ii,239)=qi_EUM
              amv_array(ii,240)=qi_EUM

C skip bad quality  observation, but not for PT
      if (md_type .NE. 'PT') then
       if ((qi_RFI .LT. 50) .OR. (qi_RFF .LT. 50) 
     & .OR. (qi_EUM .LT. 50)
     & .OR. (((sat_id .EQ. terra_id) .or. (sat_id .EQ. aqua_id) 
     & .or. is_noaa_id(sat_id,noaa_ids,num_noaas)) 
     & .and. (code_002023 .eq. 7))) then
               iobs_extracted = iobs_extracted - 1
       endif
      endif
c
C skip uncomplete observation
C----------------------------
            endif
C
C Close observation loop
C-----------------------
  223     continue     

          nout = nout + iobs_extracted

C Close Row loop
C---------------
       write(10,*)'# of records extracted nout  :  ', nout
       write(10,*)'# of records extracted nobs  :  ', nobs
       write(10,*)'# of records irow            :  ', irow 
       write(10,*)'# of records number_of_rows  :  ', number_of_rows
       write(10,*)'# of iobs_extracted          :  ', iobs_extracted
  221  continue  

C-----------------------------------------------------------------------
C 3.0           Encode amv_array into BUFR
C It is for the entire domain. Now this process has been abondanded
C-----------------------------------------------------------------------
  300  continue  
      DO I=1,10				! DEBUG
      WRITE(6,6300) I, amv_array(I,9), amv_array(I,10), amv_array(I,11),
     1 amv_array(I,12), amv_array(I,13), amv_array(I,22)		! DEBUG
 6300 FORMAT (I4,6F10.2)			! DEBUG
      ENDDO				! DEBUG
C
C      Move amv_array into values array
C
       ns=nout
       ins=ns/max_winds
       insoff=ns-ins*max_winds
       if(insoff.ne.0) ins=ins+1
       ist=1
       iend=max_winds
       if(ns.lt.max_winds) iend=ns
 
       do 320  i=1,ins
       izz=0
       do 310  iz=ist,iend
       izz=izz+1
       do 301  iw=1,kelem
       iziw=iw+(izz-1)*kelem
       if(amv_array(iz,iw).eq.b_miss) then
          values(iziw)=rvind
       else
          values(iziw)=amv_array(iz,iw)
       end if
 301   continue
 310   continue
c
c      initialize all the sections
       do ii=1, jsec0
       ksec0(ii)=0
       enddo

       do ii=1, jsec1
       ksec1(ii)=0
       enddo

       do ii=1, jsec2
       ksec2(ii)=0
       enddo

       do ii=1, jsec3
       ksec3(ii)=0
       enddo

       do ii=1, jsec4
       ksec4(ii)=0
       enddo

C      Set section 0 
       ksec0(3)=3
C
C      Set section 1
C
       ksec1(1)=18
       ksec1(2)=3
C WCS3 - old code
C       ksec1(3)=160
C WCS3 - Stores generating center parameter into array variable
       ksec1(3)=gen_centnum
       ksec1(4)=0
       ksec1(5)=0
       ksec1(6)=5
       ksec1(7)=87
       ksec1(8)=0
ccjd       ksec1(8)=1
       if(nint(values(6)).le.2000) then
          ksec1(9)=nint(values(6))-1900
       else
          ksec1(9)=nint(values(6))-2000
       end if
       ksec1(10)=nint(values(7))
       ksec1(11)=nint(values(8))
       ksec1(12)=nint(values(9))
       ksec1(13)=nint(values(10))
       ksec1(15)=10
       ksec1(16)=0       !sub_centre
C WCS: Subcenter for TROMSO
       if(subcent.eq.'10') then
         ksec1(16)=10       !sub_centre
       endif
C WCS: Subcenter for McMurdo
       if(subcent.eq.'11') then
         ksec1(16)=11       !sub_centre
       endif
C JK: Subcenter for Sodankyla
       if(subcent.eq.'12') then
         ksec1(16)=12       !sub_centre
       endif
C JK: Subcenter for Fairbanks
       if(subcent.eq.'13') then
         ksec1(16)=13       !sub_centre
       endif
C JK: Subcenter for Barrow (HRPT)
       if(subcent.eq.'14') then
         ksec1(16)=14       !sub_centre
       endif
C JK: Subcenter for Rothera, Antarctica (HRPT)
       if(subcent.eq.'15') then
         ksec1(16)=15       !sub_centre
       endif
C  
C      Set section 3
C
       ksec3(3)=iend-ist+1
       ist=iend+1
       iend=iend+max_winds
       if(iend.gt.ns) iend=ns
c
       ksec3(4)=192
c
       ktdlst( 1)=310014
       ktdlst( 2)=222000
       ktdlst( 3)=236000
       ktdlst( 4)=101103
       ktdlst( 5)=031031
       ktdlst( 6)=001031
       ktdlst( 7)=001032
       ktdlst( 8)=101010
       ktdlst( 9)=033007
       ktdlst(10)=222000
       ktdlst(11)=237000
       ktdlst(12)=001031
       ktdlst(13)=001032
       ktdlst(14)=101010
       ktdlst(15)=033007
       ktdlst(16)=222000
       ktdlst(17)=237000
       ktdlst(18)=001031
       ktdlst(19)=001032
       ktdlst(20)=101010
       ktdlst(21)=033007
c
       ktdlen=21
c    
c       call bufren(ksec0 ,ksec1,ksec2,ksec3,ksec4,
c     1               ktdlen,ktdlst,kdlen,kdata,
c     2               kelem,kvals,values,cvals,kbufl,kbufr,ierr)
c       if(ierr.gt.0) call exit(2)
c
c      Write bufr message to output file
c
c      Use actual length of message, not number of 4 byte words*4,
c      since length may be any even number of bytes.
c      7777 as int*4 = 926365495, so if the last word is this
c      then the messga elength is 4 * kbufl.  If it is not this,
c      then the length is (4 * kbufl) - 2.

C HIB Commented out check
C       if (kbufr(kbufl) .eq. 926365495) then
C          itl = kbufl * 4
C       else
C          itl = (kbufl * 4) - 2
C       end if


c      fname=fprefix(1:4)//filenumber(i)(1:2)
c      print *, 'fname', bufrout(j:num2)+i 
c      call pbopen(iunit1,fname,'w',ierr)
c      call pbopen(iunit1,bufrout(j:num2),'w',ierr)
c      if(ierr.ne.0) then
c         print*,'Open error on ',bufrout(j:num2)
c         call exit(2)
c      end if

c       call pbwrite(iunit1,kbufr,itl,ierr)
c       if(ierr.lt.0) then
c          print*,'Error writing into target file.'
c          call exit(2)
c       end if
C
c      call pbclose(iunit1, istat)
c      if ( istat .NE. 0) then
c        print *,'Error pbclose : could not close file ',bufrout(j:num2)
ccjd        print *,'Error pbclose : could not close file ',bufrout(1:j)
c        call exit(2)
c      endif


320    continue
C
       print*,'# of strange records warnings    :  ', iwarning
       print*,'# of records with missing DIR/SPD:  ', imiss
       print*,'# of records extracted           :  ', nout
C
C      print*,'BUFR record from row 1: iobs=   1 '
C      print*,'----------------------------------------------------'
C      write(*, '(5f14.6)' ) (amv_array(   1,ikey),ikey=1,bufr_length)
C      print*,'----------------------------------------------------'
C
C      print*,'BUFR record from row 2: iobs=   32 '
C      print*,'----------------------------------------------------'
C      write(*, '(5f14.6)' ) (amv_array(1514,ikey),ikey=1,bufr_length)
C      print*,'----------------------------------------------------'
       nbout=0
c--------------end of the entire domain process----

C-----------------------------------------------------------------------
C 3.0.0           test the points out of the superbox 
C-----------------------------------------------------------------------
       do i=1, nout
       if( ( (amv_array(i,12).ge. (-35.)).and.
     &       (amv_array(i,12).le. ( 75.)) ).and. 
     &     ( (amv_array(i,13).gt. (-35.)).and.
     &       (amv_array(i,13).le. (0.)) ) ) then
       nbout=nbout+1
       endif
       enddo
       write(10,*)'out points', nbout, nout
       do i=1, nout
       if( amv_array(i,12).lt. (-35.)) then
       nbout=nbout+1
       endif
       enddo
       write(10,*)'out points', nbout, nout
C
C-----------------------------------------------------------------------
C 3.1           output bufr message into subbox 
C-----------------------------------------------------------------------
       do 420 ii=11,11 
       nbout=0
C-----------------------------------------------------------------------
C 3.1.1           output bufr into Area1 
C-----------------------------------------------------------------------
       if (ii.eq.1) then 
       fprefix='JUTX1KNES' 
       do 450 i=1, nout
       if( ( (amv_array(i,12).ge. (-35.)).and.
     &       (amv_array(i,12).lt. ( 37.)) ).and. 
     &     ( (amv_array(i,13).ge. (-90.)).and.
     &       (amv_array(i,13).le. (-35.)) ) ) then
       nbout=nbout+1
       do 460 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
460    continue
       endif
450    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.2           output bufr into Area2 
C-----------------------------------------------------------------------
       if (ii.eq.2) then 
       fprefix='JUTX2KNES' 
       do 470 i=1, nout
       if( ( (amv_array(i,12).ge. ( 37.)).and.
     &       (amv_array(i,12).le. ( 75.)) ).and. 
     &     ( (amv_array(i,13).ge. (-90.)).and.
     &       (amv_array(i,13).le. (-35.)) ) ) then
       nbout=nbout+1
       do 480 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
480    continue
       endif
470    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.1           output bufr into Area3 
C-----------------------------------------------------------------------
       if (ii.eq.3) then 
       fprefix='JUTX3KNES' 
       do 490 i=1, nout
       if( ( (amv_array(i,12).ge. (-35.)).and.
     &       (amv_array(i,12).lt. ( 37.)) ).and. 
     &     ( (amv_array(i,13).ge. (-109.)).and.
     &       (amv_array(i,13).lt. (-90.)) ) ) then
       nbout=nbout+1
       do 500 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
500    continue
       endif
490    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.1           output bufr into Area4 
C-----------------------------------------------------------------------
       if (ii.eq.4) then 
       fprefix='JUTX4KNES' 
       do 510 i=1, nout
       if( ( (amv_array(i,12).ge. ( 37.)).and.
     &       (amv_array(i,12).le. ( 75.)) ).and. 
     &     ( (amv_array(i,13).ge. (-109.)).and.
     &       (amv_array(i,13).lt. (-90.)) ) ) then
       nbout=nbout+1
       do 520 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
520    continue
       endif
510    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.1           output bufr into Area5 
C-----------------------------------------------------------------------
       if (ii.eq.5) then 
       fprefix='JUTX5KNES' 
       do 530 i=1, nout
       if( ( (amv_array(i,12).ge. (-35.)).and.
     &       (amv_array(i,12).lt. ( 42.)) ).and. 
     &     ( (amv_array(i,13).ge. (-140.)).and.
     &       (amv_array(i,13).lt. (-109.)) ) ) then
       nbout=nbout+1
       do 540 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
540    continue
       endif
530    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.1           output bufr into Area6 
C-----------------------------------------------------------------------
       if (ii.eq.6) then 
       fprefix='JUTX6KNES' 
       do 550 i=1, nout
       if( ( ( (amv_array(i,12).ge. ( 42.)).and.
     &         (amv_array(i,12).le. ( 75.)) ).and. 
     &       ( (amv_array(i,13).ge. (-128.)).and.
     &         (amv_array(i,13).lt. (-109.)) ) ) .or.
     &     ( ( (amv_array(i,12).ge. ( 42.)).and.
     &         (amv_array(i,12).lt. ( 52.)) ).and. 
     &       ( (amv_array(i,13).ge. (-140.)).and.
     &         (amv_array(i,13).lt. (-128.)) ) ) ) then
       nbout=nbout+1
       do 560 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
560    continue
       endif
550    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.1           output bufr into Area7 
C-----------------------------------------------------------------------
       if (ii.eq.7) then 
       fprefix='JUTX7KNES' 
       do 570 i=1, nout
       if( ( ( (amv_array(i,12).ge. (-35.)).and.
     &         (amv_array(i,12).le. ( 50.)) ).and. 
     &     ( ( (amv_array(i,13).ge. (-180.)).and.
     &         (amv_array(i,13).lt. (-140.)) ) .or.
     &       ( (amv_array(i,13).eq. ( 180.)) ) ) ) ) then
       nbout=nbout+1
       do 580 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
580    continue
       endif
570    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.1           output bufr into Area8 
C-----------------------------------------------------------------------
       if (ii.eq.8) then 
       fprefix='JUTX8KNES' 
       do 590 i=1, nout
       if( ( (amv_array(i,12).ge. (-35.)).and.
     &       (amv_array(i,12).lt. ( 50.)) ).and. 
     &     ( (amv_array(i,13).ge. (130.)).and.
     &       (amv_array(i,13).lt. (180.)) ) ) then
       nbout=nbout+1
       do 600 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
600    continue
       endif
590    continue
       if (nbout.eq.0) goto 420
       endif
C-----------------------------------------------------------------------
C 3.1.9           output bufr into Area9 
C-----------------------------------------------------------------------
       if (ii.eq.9) then 
       fprefix='JUTX9KNES' 
       do 610 i=1, nout
       if( ( ( (amv_array(i,12).ge. ( 52.)).and.
     &         (amv_array(i,12).le. ( 75.)) ).and. 
     &       ( (amv_array(i,13).ge. (-140.)).and.
     &         (amv_array(i,13).lt. (-128.)) ) ) .or.
     &     ( ( (amv_array(i,12).ge. ( 50.)).and.
     &         (amv_array(i,12).le. ( 75.)) ).and. 
     &     ( ( (amv_array(i,13).gt. (-180.)).and.
     &         (amv_array(i,13).lt. (-140.)) ).or.
     &       ( (amv_array(i,13).gt. ( 130.)).and. 
     &         (amv_array(i,13).lt. ( 180.)) ) ) ) ) then
       nbout=nbout+1
       do 620 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j) 
620    continue
       endif
610    continue
       if (nbout.eq.0) goto 420
       endif

C-----------------------------------------------------------------------
C 3.1.9           output bufr into Area10 the remain data
C-----------------------------------------------------------------------
       if (ii.eq.10) then
       fprefix='JUTX0KNES'
       do 630 i=1, nout
       if(  (amv_array(i,12).gt. ( 75.)).or.
     &      (amv_array(i,12).lt. (-35.)).or.
     &      (amv_array(i,13).gt. (-35.)).or.
     & ((amv_array(i,13).gt. 0.).and.(amv_array(i,13).lt.130.)) ) then
       nbout=nbout+1
       do 640 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j)
640    continue
       endif
630    continue
       if (nbout.eq.0) goto 420
       print *, 'nbout=', nbout
       endif

C-----------------------------------------------------------------------
C 3.1.10           output bufr for MODIS 
C-----------------------------------------------------------------------
       if (ii.eq.11) then
       fprefix='JUTX0KNES'
       do 650 i=1, nout
       nbout=nbout+1
       do 660 j=1,kelem
       bbbb(nbout,j)=amv_array(i,j)
660    continue
650    continue
       if (nbout.eq.0) goto 420
       print *, 'nbout=', nbout
       endif

  400  continue 
C
C      Move amv_array into values array
C
       ns=nbout
       ins=ns/max_awips
       insoff=ns-ins*max_awips
       if(insoff.ne.0) ins=ins+1
       ist=1
       iend=max_awips
       if(ns.lt.max_awips) iend=ns
       write (10,*)'OUTPUT FOR THE SUB AREA  ', ii 
       write (10,*)'nbout', ns, ins, insoff, iend 
      fname=fprefix(1:9)
c      print *, 'fname', bufrout(j:num2)+i 
c      call pbopen(iunit1,fname,'w',ierr)
c      call pbopen(iunit1,bufrout(j:num2),'w',ierr)
c      if(ierr.ne.0) then
c         print*,'Open error on ',bufrout(j:num2)
c         call exit(2)
c      end if
       do 421  i=1,ins
       izz=0
       do 410  iz=ist,iend
       izz=izz+1
       do 401  iw=1,kelem
       iziw=iw+(izz-1)*kelem
       if(bbbb(iz,iw).eq.b_miss) then
          values(iziw)=rvind
       else
          values(iziw)=bbbb(iz,iw)
       end if
 401   continue
 410   continue
c
C      Set section 0 
C
C      Set section 1
C
       if(nint(values(6)).le.2000) then
          ksec1(9)=nint(values(6))-1900
       else
          ksec1(9)=nint(values(6))-2000
       end if
       ksec1(10)=nint(values(7))
       ksec1(11)=nint(values(8))
       ksec1(12)=nint(values(9))
       ksec1(13)=nint(values(10))
C  
C      Set section 3
C
       ksec3(3)=iend-ist+1
       ist=iend+1
       iend=iend+max_awips
       if(iend.gt.ns) iend=ns
c
c    
       call bufren(ksec0 ,ksec1,ksec2,ksec3,ksec4,
     1               ktdlen,ktdlst,kdlen,kdata,
     2               kelem,kvals,values,cvals,kbufl,kbufr,ierr)
       if(ierr.gt.0) call exit(2)
c
c      Write bufr message to output file
c
c      Use actual length of message, not number of 4 byte words*4,
c      since length may be any even number of bytes.
c      7777 as int*4 = 926365495, so if the last word is this
c      then the messga elength is 4 * kbufl.  If it is not this,
c      then the length is (4 * kbufl) - 2.
       if (kbufr(kbufl) .eq. 926365495) then
          itl = kbufl * 4
       else
          itl = (kbufl * 4) - 2
       end if

      fname=fprefix(1:9)//filenumber(i)(1:2)
c      print *, 'fname', bufrout(j:num2)+i
      call pbopen(iunit1,fname,'w',ierr)
c      call pbopen(iunit1,bufrout(j:num2),'w',ierr)
      if(ierr.ne.0) then
         print*,'Open error on ',bufrout(j:num2)
         call exit(2)
      end if
c
c      Write bufr message to output file
c
c      If it's Linux, swap the bytes... (11/22/05)
       call swbyt4( kbufr, kbufl)

       call pbwrite(iunit1,kbufr,itl,ierr)
       if(ierr.lt.0) then
          print*,'Error writing into target file.'
          call exit(2)
       end if
      
       call pbclose(iunit1, istat)
C
 421   continue
c      call pbclose(iunit1, istat)
      if ( istat .NE. 0) then
        print *,'Error pbclose : could not close file ',bufrout(j:num2)
ccjd        print *,'Error pbclose : could not close file ',bufrout(1:j)
        call exit(2)
      endif


420    continue
C-----------------------------------------------------------------------
C 4.0           clean up and close
C-----------------------------------------------------------------------

      call pbclose(iunit, istat)
      if ( istat .NE. 0) then
        print *,'Error pbclose : could not close file MDXX: ', 
     1           mdin(i:num1)
ccjd        print *,'Error pbclose : could not close file MDXX: ', 
ccjd     1           mdin(1:i)
        call exit(2)
      endif

c      call pbclose(iunit1, istat)
c      if ( istat .NE. 0) then
c        print *,'Error pbclose : could not close file ',bufrout(j:num2)
ccjd        print *,'Error pbclose : could not close file ',bufrout(1:j)
c        call exit(2)
c      endif

C-----------------------------------------------------------------------
C end gwin2bufr
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      print*, ' end GWIN2BUFR '
C-----------------------------------------------------------------------

      end


      function  error2flag(data_in,miss)
C --- function  error2flag
C ---
C --- converts GWIN "tracking algorithm error code" into
C ---          BUFR flag table (20 bit)
C-----------------------------------------------------------------------
C     function  error2flag(data_in,miss)
      integer*4 error2flag
C-----------------------------------------------------------------------
      integer      flag,data_temp, data_in
      integer      digit, digit_off, idigit
      dimension    digit    (4)
      dimension    digit_off(4)
      data         digit_off      / 0, 3,10,15/
C-----------------------------------------------------------------------

      flag = 0
      if ( (data_in .EQ.    0) .OR.
     1     (data_in .GT. 5000)  .OR.
     2     (data_in .EQ. miss)     ) then
         flag = 20
         goto 1000
      endif

      data_temp = data_in
      do idigit=4,1,-1
         digit(idigit) = data_temp / ( 10**(idigit-1) )
         data_temp     = data_temp
     1                         - digit(idigit)*( 10**(idigit-1) )
      enddo

      do idigit=1,4
         if( digit(idigit) .NE. 0 ) then
            flag = flag +
     1             2**(digit_off(idigit) + digit(idigit) - 1)
         endif
      enddo
 1000 continue

      error2flag = flag
C-----------------------------------------------------------------------
      return
      end
      subroutine YD2DAT(iyd,iy,im,id)
c
      integer iyd,iy,im,id
c
      iy=iyd/10000
      imd=iyd-iy*10000
      im=imd/100
      id=imd-im*100
      return
      end
      
C-----------------------------------------------------------------------
      logical function is_noaa_id(sat_id,noaa_ids,num_noaas)
      integer sat_id, noaa_ids(*), i, num_noaas
      logical result
      
      result = .FALSE.
      do i=1,num_noaas
        if (sat_id .eq. noaa_ids(i)) then
          result = .TRUE.
	endif
      enddo
      is_noaa_id = result
      
      end
    
C----------------converts length len string to integer - HIB/WCS3

      subroutine STR2NUM(CBUF,len,numreturn)
      integer numreturn,I,len,digit
      character*10 CBUF
      DO I=1,len
          digit=ICHAR(CBUF(I:I)) - ICHAR('0')
          numreturn = numreturn + digit*10**(len-I)
      END DO 
      return
      end

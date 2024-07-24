!
! [NOTE]:      This program is to convert the CIMSS text wind satellite 
!              format to the fomat that the LETKF code needs
!
! [HISTORY]: - Mar 11, 2011: created
!            - Mar 30, 2011: just found that CIMSS longitude is opposite
!                            world longitude.
!
! [AUTHOR]:    Chanh Q. Kieu 
!              I. M. System Group, Inc 
!              Email: kieuc@imsg.com
!
! [COPYRIGHT]: (C) 2011 trademarked by IMSG, Inc, USA.
!
!===========================================================================
!
      PROGRAM cimss_text_to_letkf_format
      IMPLICIT none
      INCLUDE 'netcdf.inc' 
!
! netcdf header information
!
      INTEGER, PARAMETER  :: n=4,nmax=10000
      INTEGER             :: ncid,status,nrs
      INTEGER             :: ndims,nvars,ngatts,unlimdimid
      INTEGER             :: uid,vid,tid,rid,pid
      INTEGER             :: tndim,tdim(n)
      REAL                :: dx,dy
!
! station information
!
      INTEGER             :: ilat,ilon
      REAL                :: tlat,tlon,tlev
      REAL                :: latrs,lonrs,p1,sp,di,spc,u1,v1
!
! local vars
!
      INTEGER             :: nx,ny,nz,nt,nx1,ny1,nz1,nt1
      INTEGER             :: iday,ihour,imin,fid,nl,nobs 
      REAL                :: umax,vmax,tmax,rmax  
      REAL                :: rtime,tfcst,restart,ptop,pi
      REAL                :: w1,w2,uu,vv,qq,tt,zz
      REAL                :: uuc,vvc,ttc,qqc,zzc
      REAL,ALLOCATABLE    :: latin(:,:,:),lonin(:,:,:)
      REAL,ALLOCATABLE    :: p(:,:,:,:),pb(:,:,:,:),psfc(:,:,:)
      REAL,ALLOCATABLE    :: q1(:),th(:),t1(:)
      CHARACTER*100       :: name,ofile,wfile,tem
      CHARACTER           :: tem1
      INTEGER             :: i,j,k,tem_len,debug,ne,nfile,ifile,irec
      LOGICAL             :: check_data1,check_data2,ntruncate
      irec               = 1 
      debug              = 0
      ntruncate          = 5
!
! reading background map information for later interpolation
!
      ofile              = 'wrfinput_d01'
      if (debug.eq.1) print*,' cimss_to_letkf.exe: opened file is: ',ofile(1:30)
      status = NF_OPEN(ofile(1:len_trim(ofile)),nf_write,ncid)
      if (debug.ge.1) then
       print*,''
       print*,'1. NF_OPEN and return value: '
       print*,'status     =',status
       print*,'nf_noerr   =',nf_noerr 
       print*,'ncid       =',ncid
      endif
!
! pull our varid and dim information for a temporary variable
! which will be used later.  
!
      tem                = 'T'
      tem_len            = len_trim(tem)
      print*,'Checking var is: ',tem(1:tem_len)
      status = NF_INQ_VARID(ncid,tem(1:tem_len),tid)
      status = NF_INQ_VARNDIMS(ncid,tid,tndim)
      status = NF_INQ_VARDIMID(ncid,tid,tdim)
      if (tndim.ne.n) THEN
       print*,'Number of dim is /= n...allocation stop'
       STOP
      endif
      print*,''
      print*,'2. Pull out information about: ',tem(1:tem_len)
      print*,'tid        =',tid
      print*,'tndim      =',tndim
      do i               = 1,tndim
       j                 = tdim(i)
       status            = NF_INQ_DIMNAME(ncid,j,name)
       status            = NF_INQ_DIMLEN(ncid,j,k)
       if (i.eq.1) nx    = k
       if (i.eq.2) ny    = k
       if (i.eq.3) nz    = k
       if (i.eq.4) nt    = k
       print*,'Dimension name is:',i,tdim(i),name(1:20)
      enddo
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DX',dx)
      status             = NF_GET_ATT_REAL(ncid,nf_global,'DY',dy)
      print*,'nx         =',nx
      print*,'ny         =',ny
      print*,'nz         =',nz
      print*,'nt         =',nt
      print*,'dx         =',dx
      print*,'dy         =',dy
      if (debug.eq.1) READ*
!
! define a mother domain with A-grid based on the dims of
! PH from C-grid, and locate all variable
!
      nx1          = nx + 1
      ny1          = ny + 1
      nz1          = nz + 1
      allocate(latin(nx,ny,nt),lonin(nx,ny,nt))
      allocate(p(nx,ny,nz,nt),pb(nx,ny,nz,nt),psfc(nx,ny,nt))   
!
! reading data from the background wrfinput to define the local mesh
!
      tem          = 'XLAT'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),uid)
      status       = NF_GET_VAR_REAL(ncid,uid,latin)
      tem          = 'XLONG'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),vid)
      status       = NF_GET_VAR_REAL(ncid,vid,lonin)
      tem          = 'P'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,p)
      tem          = 'PB'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),rid)
      status       = NF_GET_VAR_REAL(ncid,rid,pb)
      p            = p + pb
      tem          = 'PSFC'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,psfc)
      tem          = 'P_TOP'
      tem_len      = len_trim(tem)
      status       = NF_INQ_VARID(ncid,tem(1:tem_len),pid)
      status       = NF_GET_VAR_REAL(ncid,pid,ptop)
!
! do a first loop of all data to see how many obs we can aggregate
!
      nl           = 90000 
      open(90,file='cimss_text.dat',status='old')
      nobs         = 0
      i            = 1
      read(90,*)
10    continue
      read(90,*,end=16)tem1,tem1,tem1,tem1,latrs,lonrs,p1
      lonrs        = -lonrs
      p1           = p1*100.
      tlat         = (latrs-latin(1,1,1))*111.e3/dx
      tlon         = (lonrs-lonin(1,1,1))*111.e3/dy
      ilat         = nint(tlat)
      ilon         = nint(tlon)
      if (ilat.lt.2.or.ilat.gt.ny-1.or.ilon.lt.2.or.ilon.gt.nx-1) goto 18  
      do k         = 1,nz-ntruncate  !1,19  !1,nz-1
       check_data1 = p1.le.p(ilon,ilat,k,1).and.                          &
                     p1.gt.p(ilon,ilat,k+1,1)
       if (check_data1) then
        nobs       = nobs + 1           
        goto 18
       endif
      enddo
18    i            = i + 1
      goto 10
16    print*,'cimss_to_letkf.exe: total number of observation is',nobs
      if (nobs.eq.0) goto 35
      close(90)
!
! reading the cmiss text data
!
      open(90,file='cimss_text.dat',status='old')
      open(11,file='obsout.dat')
      write(11,*)nobs,' lon | lat | lev | u qcu | v qcv | t qct | &
                        qv qcqv | ph qcph |'
      read(90,*)
      pi           = 4.*atan(1.)
      i            = 1
34    continue
      read(90,*,end=35)tem1,tem1,tem1,tem1,latrs,lonrs,p1,sp,di,tem1,spc
      lonrs        = -lonrs
      p1           = p1*100.
      u1           =  sp*cos(pi/2.+di*pi/180.)/2.
      v1           = -sp*sin(pi/2.+di*pi/180.)/2.
      tlat         = (latrs-latin(1,1,1))*111.e3/dx
      tlon         = (lonrs-lonin(1,1,1))*111.e3/dy
      ilat         = nint(tlat)
      ilon         = nint(tlon)
      if (ilat.lt.2.or.ilat.gt.ny-1.or.ilon.lt.2.or.ilon.gt.nx-1) goto 37
      do k         = 1,nz-ntruncate  !1,19  !1,nz-1
       check_data1 = p1.le.p(ilon,ilat,k,1).and.                          &
                     p1.gt.p(ilon,ilat,k+1,1)
       if (check_data1) then
        w1        = p(ilon,ilat,k,1)-p1
        w2        = p(ilon,ilat,k,1)-p(ilon,ilat,k+1,1)
        uu        = u1
        vv        = v1
        qq        = 1.e-3
        tt        = 300
        zz        = 10.
        uuc       = spc
        vvc       = spc
        qqc       = 1.e-3
        ttc       = 1
        zzc       = 1.         
        tlev      = float(k) + w1/w2
        write(11,'(3F12.4,10E12.4)')tlon,tlat,tlev,uu,uuc,vv,vvc,  &
                                         tt,ttc,qq,qqc,zz,zzc
        goto 37
       endif
      enddo
36    print*,'cimss_to_letkf.exe: wind point out of domain..skip'      
37    i             = i + 1
      goto 34
35    continue
      if (nobs.eq.0) then
         open(11,file='obsout.dat')
         write(11,*)nobs,' lon | lat | lev | u qcu | v qcv | t qct | &
                        qv qcqv | ph qcph |'
      endif
      print*,'cimss_to_letkf.exe: done conversion'
!
! close the data file and loop all files
!
      status       = NF_CLOSE(ncid)
      if (status.ne.0) print*,'Can not close the new file'
      END
      
      subroutine td_to_qv(p,t,td,q)
      real p,t,td,q,w,e,tc
!
! compute vapor pressure (hPa or mb)
!     
      tc           = td-273.15
      e            = 6.11*(10**((7.5*tc)/(237.7+tc)))
!
! compute q (kg/kg)
!      
      q            = 621.97*e*1e-3/(p/100.-e)
      return
      end

      subroutine tk_to_theta(p,t,th)
      real p,t,th,R,Cp,pref,tref
      R            = 286.5517
      Cp           = 1003.5
      pref         = 1000.
      tref         = 300.
      th           = t*(100000./p)**(R/Cp)
      return
      end
    

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

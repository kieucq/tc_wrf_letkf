      program dmpbfr

c dmpbfr.f - 17 Nov 2004
c  Reads bufr file & dumps info to unit 25
c
c 30 Apr 2003 - added message filter option
c  9 Dec 2003 - added jump to report # option
c 17 Nov 2004 - debug of msgdt output format
c
c====.7===============================================================72
      character*8  cmgtag,mtyp
      character*1  answer

      data mtyp /' '/

      luprt  = 0  ! =0: pause in dumps (UFBDMP); else luprt==output lun
      lunout = 6  ! =6: standard out

c- OPEN BUFR FILE FOR READING
      lunbf = 11  ! input bufr file (w/embedded DX table)
      lundx = 12  ! external bufrtable file (if needed)
      lundx = lunbf  ! assumes dictionary table embedded in source file
      OPEN(lunbf,file='testdata.dat',status='old')
       
      call OPENBF (lunbf, 'IN', lundx)
      READ*      

c- Set dates to 10-digit
      call DATELEN(10)

c- count total reports
      ntot = 0
      do while ( IREADNS(lunbf,cmgtag,msgdt,ierr) .eq. 0)
        ntot = ntot + 1
      enddo
      rewind(lunbf)
      print *,'ntot=',ntot


c- READ MESSAGES/SUBSETS
      numr = 0
      nrpt = 0
      do while ( IREADNS(lunbf,cmgtag,msgdt,ierr) .eq. 0)
        nrpt = nrpt + 1
c       if (nrpt.lt.mnum .and. mnum.ne.0) cycle
        if (nrpt.lt.numr ) cycle

        if(mtyp.eq.' ' .or. mtyp.eq.cmgtag) then 
          print *, 'dmpbfr: ' // cmgtag, msgdt, 'rpt#',nrpt,' of',ntot
          call UFBDMP (lunbf,luprt)

 99       print '(a,i5,a,$)', 
     &      '(q)uit (d)ump (t)yp (g)oto (of ',ntot,') [cr]ontinue: '
          read(5,'(a1)') answer

c         if(answer.eq.'q' .or. answer.eq.'') return
c         write(lunout,*) 'answer=>>' // answer // '<<'
          if(answer.eq.'q') then 
            print *, 'quitting'
            stop
          endif
          if(answer.eq.'d') then 
            print *, 'calling UFDUMP'
            call UFDUMP(lunbf,lunout)
            goto 99
          endif
          if(answer.eq.'t') then 
            print '(a,$)', '(8char-)msg type: '
            read(5,'(a8)') mtyp
            print *, 'msgtype now=' // mtyp
          endif
          if(answer.eq.'g') then 
            print '(a,i5,a,$)', 
     &        '(int)rpt number (current rpt=',nrpt,'): '
            read(5,*) numr
            print *,'going to rpt# ',numr
          endif
        endif ! msg='' or msg=cmgtag

      enddo ! while IREADNS works

      stop
      end

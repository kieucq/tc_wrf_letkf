'reinit'
'open wpost.ctl'
'set mpdset hires'
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set clopts 1 7 0.14'
'set lat -5 40'
'set lon 85 135'
'set clskip 2'
'rgbset.gs'
interval=3
infile='wpost.txt'
tem1=read(infile)
tem2=sublin(tem1,2)
stime=subwrd(tem2,1)
*
* 500 mb RH and stream field
*
tim=1
fname='rh500'
'set clskip 2'
'set lev 500'
while (tim <=25)
 hh=(tim-1)*interval
 'set t 'tim
 'set grads off'
 'set xlint 5'
 'set ylint 5'
 'set gxout shaded'
 'set clevs  65 70 75 80 85 90 95 96 97 98 99'
 'set ccols  0 11 5 13 3 10 22 23 24 25 26 27 28 29'
 'd rh'
 'cbar.gs'
 'set gxout contour'
 'set gxout stream'
 'set strmden 1'
 'set cthick 5'
 'set ccolor 1'
 'set arrscl 0.5 25'
 'd u;v'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' + (07) local time'
 if (hh < 10)
  'printim 'fname'_'stime'_0'hh'.gif white'
 else
  'printim 'fname'_'stime'_'hh'.gif white'
 endif
 tim=tim+1
 'c'
endwhile
*
* wind speed and barb
*
fname='2m_wind'
tim=1
while (tim <=25)
 hh=(tim-1)*interval
 'set t 'tim
 'set xlint 10'
 'set ylint 10'
 'set grads off'
 'set gxout shaded'
 'set clevs 2 3 4 5 6 7 8 9 10 12 14 16 18 20 22 26 30 34 38 40'
 'set ccols 0 4 11 5 13 3 10 7 22 23 24 25 26 27 28 69 68 67 66 65'
 'd mag(u,v)'
 'cbarn.gs'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' + (07) local time'
 'set cthick 7'
 'set gxout barb'
 'set arrowhead 0.11'
 'set arrscl 0.5 10'
 'd skip(u*2,13,13);v*2'
 if (hh < 10)
  'printim 'fname'_'stime'_0'hh'.gif white'
 else
  'printim 'fname'_'stime'_'hh'.gif white'
 endif
 'c'
 tim=tim+1
endwhile
*
* plot precipation
*
tim=1
fname='prep'
while (tim <=25)
 hh=(tim-1)*interval
 'set t 'tim
 'set grads off'
 'set xlint 10'
 'set ylint 10'
 'set gxout shaded'
 'set clevs 5 10 20 30 40 50 60 70 80 90 100 120 140 160 180 200 250 300 350 400 450'
 'set ccols 0 32 33 34 35 36 3 37 38 39 74 75 76 77 78 79 69 68 67 66 65'
 'd rainc+rainnc'
 'cbar.gs'
 'set gxout contour'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' + (07) local time'
 if (hh < 10)
  'printim 'fname'_'stime'_0'hh'.gif white'
 else
  'printim 'fname'_'stime'_'hh'.gif white'
 endif
 tim=tim+1
 'c'
endwhile
*
* plot temperature
*
fname='temp'
tim=1
while (tim <=25)
 hh=(tim-1)*interval
 'set t 'tim
 'set xlint 10'
 'set ylint 10'
 'set grads off'
 'set gxout shaded'
 'set clevs 2 4 6 8 10 12 14 16 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 34 36'
 'set ccols 41 42 43 44 45 46 47 48 49 39 38 37 36 35 34 22 23 24 25 26 27 28 29 69 68 67 66 65'
 'd t2-273'
 'cbarn.gs'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' (Local time)'
 if (hh < 10)
  'printim 'fname'_'stime'_0'hh'.gif white'
 else
  'printim 'fname'_'stime'_'hh'.gif white'
 endif
 'c'
 tim=tim+1
endwhile
*
* plot slvl pressure
*
tim=1
fname='slvl'
while (tim <=25)
 hh=(tim-1)*interval
 'set t 'tim
 'set grads off'
 'set xlint 5'
 'set ylint 5'
 'set gxout shaded'
 'set clevs 988 992 996 1000 1002 1004 1006 1007 1008 1009 1010 1011 1012 1014 1016 1018 1022 1026'
 'set ccols 68 69 29 28 27 26 25 24 23 22 21 42 43 44 45 46 47 48 49'
 'd slvl'
 'cbar.gs'
 'set gxout contour'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' + (07) local time'
 if (hh < 10)
  'printim 'fname'_'stime'_0'hh'.gif white'
 else
  'printim 'fname'_'stime'_'hh'.gif white' 
 endif
 tim=tim+1 
 'c'
endwhile



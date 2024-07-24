'reinit'
'open wpost.ctl'
'set mpdset hires'
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set clopts 1 7 0.14'
'set lat 4 9'
'set lon -12 -7'
'set clskip 2'
'rgbset.gs'
interval=3
infile='wpost.txt'
tem1=read(infile)
tem2=sublin(tem1,2)
stime=subwrd(tem2,1)
*
* plot precipation
*
tim=1
fname='prep'
while (tim <=25)
 tim_b=tim-1
 hh=(tim-1)*interval
 'set t 'tim
 'set grads off'
 'set xlint 2'
 'set ylint 2'
 'set gxout shaded'
 'set clevs 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 40 42 45'
 'set ccols 0 32 33 34 35 36 3 37 38 39 74 75 76 77 78 79 69 68 67 66 65'
 'd smth9(rainc+rainnc-ave(rainnc+rainc,t='tim_b',t='tim_b'))'
 'cbar.gs'
 'set gxout contour'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' local time'
 'printim 'fname'_'hh'.gif white'
 tim=tim+1
 'c'
endwhile



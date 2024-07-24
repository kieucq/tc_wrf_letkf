'reinit'
'open mergi_composite.ctl'
'set xlopts 1 7 0.16'
'set ylopts 1 7 0.16'
'set clopts 1 7 0.14'
'set lat 5 35'
'set lon 100 140'
'set mpdset hires'
'set map 15 1 7'
'set clskip 3'
'Crainbow-3.gs'
*
* plot 999 mb circles 
*
tim=1
fname='mergi_comp'
while (tim <=41)
 'set t 'tim
 'set grads off'
 'set xlint 10'
 'set ylint 10'
 'set gxout shaded'
 'set clevs 14 18 22 26 30 34 38 42 46 50'
 'set ccols 0 11 5 13 3 10 7 12 8 2 6'
 'd mag(u,v)'
 'ensemble_track.gs'
 'cbarn.gs'
 'set gxout contour'
 'set cthick 7'
 'set cint 5'
* 'set clevs 1000 990 980 870 960 950 940 930 920 910 900'
 'set ccolor 1'
* 'd slvl'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' UTC time'
 'set gxout barb'
 'set cthick 5'
 'set ccolor 1'
 'set arrowhead 0.12'
 'd skip(u,4,4);v'
 tim=tim+1
 'enable print 'fname'_'tim'.gmf'
 'print'
 'disable print'
 '!gxps -c -i 'fname'_'tim'.gmf -o 'fname'_'tim'.ps'
 '!convert -rotate 90 -density 300 -geometry 2000x2000 'fname'_'tim'.ps 'fname'_'tim'.jpeg'
* pull a
 'c'
endwhile
'!rm *.ps *.gmf'


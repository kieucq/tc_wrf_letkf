'reinit'
'open ensemble_mergi.ctl'
'set xlopts 1 7 0.16'
'set ylopts 1 7 0.16'
'set parea 2 8.2 1.2 7.4'
'set grid off'
'set mproj off'
'Crainbow-3.gs'
'set clab off'
tim=1
fname='mergi00z17'
*
* plot 999 mb circles 
*
tim=1
fname='mergi00z19'
while (tim <=25)
 'set t 'tim
 'set grads off'
 'set xaxis -600 600 200'
 'set yaxis -600 600 200'
 'set gxout shaded'
 'set clevs 14 18 22 26 28 30 35 40 45 50 55 58'
 'set ccols 0 90 85 11 5 13 3 10 7 12 8 2 6'
 'd mag(u,v)'
 'cbarn.gs'
 'set gxout contour'
 'set cthick 5'
 'set ccolor 1'
 'set clevs 14 18 22 26 28 30 35 40 45 50 55 58'
 'd mag(u,v)'
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' UTC time'
 'set gxout barb'
 'set cthick 5'
 'set ccolor 16'
 'set arrowhead 0.12'
 'd skip(u,5,5);v'
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


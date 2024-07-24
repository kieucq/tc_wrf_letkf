'reinit'
'open letkf_ida.ctl'
'set xlopts 1 7 0.16'
'set ylopts 1 7 0.16'
*'set lat 5 50'
*'set lon 105 165'
'set mpdset hires'
'set map 15 1 5'
'set z 18'
'set arrscl 0.5 10'
'd uo-ub;vo-vb'
'set arrscl 0.5 2'
'set ccolor 2'
'd skip(maskout(ua-ub,mag(uo,vo)),2,2);va-vb'
pull a
*
* obs at each level
*
lev1=10
fname='obs_12z18'
while (lev1<=29)
 say 'Working with level: 'lev1 
 'set z 'lev1
 'set grads off'
 'set xlint 5'
 'set ylint 5'
 'set cthick 5'
 'set ccolor 5'
 'set gxout vector'
 'set arrowhead 0.11'
 'set arrscl 0.5 10'
 'define sp=sqrt((ua-ub)*(ua-ub)+(va-vb)*(va-vb))'
 'define ua1=maskout(ua-ub,sp-2.0)'
 'define va1=maskout(va-vb,sp-2.0)'
 if (lev1 < 20)
  'd skip(ua1,1,1);(va1)'
 else
  'd skip(ua1,2,2);(va1)'
 endif
 'set cthick 9'
 'set arrscl 0.5 20'
 'set ccolor 1'
 'set gxout barb'
 'set arrowhead 0.11'
 'd (uo-ub)*2;(vo-vb)*2'
* 'enable print 'fname'_'lev1'.gmf'
* 'print'
* 'disable print'
* '!gxps -c -i 'fname'_'lev1'.gmf -o 'fname'_'lev1'.ps'
* '!convert -trim -rotate 90 -density 180 -geometry 2000x2000 'fname'_'lev1'.ps 'fname'_'lev1'.pdf'
 pull a
 'c'
 lev1=lev1+1
endwhile
*
* obs for lower levels
*
lev1=1
fname='obs_12z18_LL'
while (lev1<=22)
 'set z 'lev1
 'set grads off'
 'set xlint 5'
 'set ylint 5'
 'set cthick 7'
 'set arrscl 0.5 20'
 'set ccolor 1'
 'set gxout barb'
 'set arrowhead 0.11'
 'd (uo)*2;(vo)*2'
 lev1=lev1+1
endwhile
'set z 16'
'set cthick 5'
'set ccolor 3'
'set arrowhead 0.11'
'set arrscl 0.5 10'
'define sp=sqrt((ua-ub)*(ua-ub)+(va-vb)*(va-vb))'
'define ua1=maskout(ua-ub,sp-2)'
'define va1=maskout(va-vb,sp-2)'
'd skip(ub,3,3);(vb)'
*'enable print 'fname'.gmf'
*'print'
*'disable print'
*'!gxps -c -i 'fname'.gmf -o 'fname'.ps'
*'!convert -trim -rotate 90 -density 180 -geometry 2000x2000 'fname'.ps 'fname'.pdf'
pull a
'c'
*
* upper level
*
lev1=23
fname='obs_12z18_UL'
while (lev1<=29)
 'set z 'lev1
 'set grads off'
 'set xlint 5'
 'set ylint 5'
 'set cthick 7'
 'set arrscl 0.5 20'
 'set ccolor 1'
 'set gxout barb'
 'set arrowhead 0.11'
 'd (uo)*2;(vo)*2'
 lev1=lev1+1
endwhile
'set z 25'
'set cthick 5'
'set ccolor 3'
'set arrowhead 0.11'
'set arrscl 0.5 10'
'define sp=sqrt((ua-ub)*(ua-ub)+(va-vb)*(va-vb))'
'define ua1=maskout(ua-ub,sp-2)'
'define va1=maskout(va-vb,sp-2)'
'd skip(ub,3,3);(vb)'
*'enable print 'fname'.gmf'
*'print'
*'disable print'
*'!gxps -c -i 'fname'.gmf -o 'fname'.ps'
*'!convert -trim -rotate 90 -density 180 -geometry 2000x2000 'fname'.ps 'fname'.pdf'
pull a
'c'
'!rm *.gmf *.ps'


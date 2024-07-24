'reinit'
'open fort.ctl'
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set clopts 1 7 0.14'
'set xlint 2'
'set ylint 2'
*
* 34 kt
*
'set grads off'
'set clevs 17'
'set cthick 9'
'set ccolor 5'
'd mag(ugrd10m,vgrd10m)'
*
* 50 kt
*
'set clevs 25'
'set cthick 9'
'set ccolor 2'
'd mag(ugrd10m,vgrd10m)'
*
* 64 kt
*
'set clevs 32'
'set cthick 9'
'set ccolor 4'
'd mag(ugrd10m,vgrd10m)'
*
* draw vector
*
'set ccolor 1'
'set cthick 5'
'set arrscl 0.5 35'
'd skip(ugrd10m,7,7);vgrd10m'
*
* plot obs radii
*
i=1
while (i <= 12)
 rc=read('fort.10')
 iline=sublin(rc,2)
 ilat=subwrd(iline,3)
 rc=read('fort.10')
 iline=sublin(rc,2)
 ilon=subwrd(iline,3)
 say 'lon is 'ilon
 say 'lat is 'ilat
 if (ilat != -999) & (ilon != -999)
  'q w2xy 'ilon'  'ilat
  rc=sublin(result,1)
  ix=subwrd(rc,3)
  iy=subwrd(rc,6)
  say ix' 'iy
  if (i<=4)
   'set line 5'
  endif
  if (i<=8) & (i>4)
   'set line 2'
  endif
  if (i > 8)
   'set line 4'
  endif
  'draw mark 3 'ix'  'iy' 0.2'
 endif
 i=i+1
endwhile
'enable print out.gmf'
'print'
'disable print'
'!gxps -c -i out.gmf -o out.ps'
'!convert -rotate 90 -geometry 900x900 out.ps out.png'

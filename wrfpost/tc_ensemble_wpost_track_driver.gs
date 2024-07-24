'reinit'
'open ensemble_all.ctl'
'set xlopts 1 9 0.16'
'set ylopts 1 9 0.16'
'set map 15 1 10 '
*
* get header information to set up basemap
* 
rc=read('wpost.txt')
runmode=sublin(rc,2)
rc=read('wpost.txt')
ne=sublin(rc,2)
rc=read('wpost.txt')
storm=sublin(rc,2)
rc=read('wpost.txt')
ptime=sublin(rc,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
lat1=subwrd(iline,1)
lat2=subwrd(iline,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
lon1=subwrd(iline,1)
lon2=subwrd(iline,2)
'set lon 'lon1' 'lon2
'set lat 'lat1' 'lat2
'set mpdset hires'
'set clab off'
*
* plot 999 mb circles 
*
tim=1
cscale=1
fname=storm'_track_'ptime'_'runmode
while (tim <=5)
 'set t 'tim
 'set grads off'
 'set xlint 10'
 'set ylint 10'
 'set clevs 1998'
 if (cscale > 14) ; cscale = 1 ; endif
 'set gxout contour'
 'set cthick 7'
 'set cint 3'
 'set cmin 100'
 'set ccolor 9'
 'd ave(slvl,e=1,e=21)'
 if (tim=1)
  'tc_ensemble_wpost_track_core.gs'
  'q time'
  result1 = sublin(result,1)
  wtime = subwrd(result1,3)
  'draw title Track forecast of 'storm': cycle 'wtime' UTC'
  'set gxout barb'
  'set cthick 5'
  'set ccolor 3'
  'set arrowhead 0.12'
  'd skip(u,4,4);v'
 endif 
 tim=tim+3
 cscale=cscale+1
endwhile
'printim track.gif white'
*'printim 'fname'.gif white'
*'enable print 'fname'.gmf'
*'print'
*'disable print'
*'!gxps -c -i 'fname'.gmf -o 'fname'.ps'
*'!convert -rotate 90 -density 180 -geometry 2000x2000 'fname'.ps 'fname'.jpeg'
*'!rm *.ps *.gmf'

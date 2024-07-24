'reinit'
'open ensemble_tseries.ctl'
'set xlopts 1 7 0.16'
'set ylopts 1 7 0.16'
'set parea 1.1 10.1 1.2 7.9'
'set grid off'
'set mproj off'
'set clab off'
tim=1
*
* plot pmin 
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
p1=subwrd(iline,1)
p2=subwrd(iline,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
v1=subwrd(iline,1)
v2=subwrd(iline,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
rc=read('wpost.txt')
iline=sublin(rc,2)
ntime=subwrd(iline,1)
dtime=subwrd(iline,2)
ftime=(ntime-1)*dtime
ilat=1
fname=storm'_pmin_'ptime'_'runmode
while (ilat <=ne)
 'set ylpos 0 r'
 'set lat 'ilat
 'set grads off'
 'set xaxis  0 'ftime' 12'
 'set vrange 'p1' 'p2
 'set cthick 2'
 'set cstyle 1'
 'set ccolor 9'
 'set cmark 0'
 'd pmin'
 ilat=ilat+1
endwhile
'set vrange 'p1' 'p2
'set ylpos 0 l'
'set cthick 18'
'set ccolor 1'
'set cstyle 1'
'set cmark 0'
'd ave(pmin,lat=1,lat='ne')'
'set cthick 18'
'set ccolor 4'
'set cstyle 1'
'set cmark 0'
'set lat 'ne+1
'd pmin'
'draw title Forecast of 'storm' initialized at 'ptime
'draw xlab Forecast time (hours)'
'draw ylab Minimum sea level pressure (hPa)'
'printim pmin.gif white'
*'printim 'fname'.gif white'
*'set strsiz 0.2'
*'set string 1 c 9 0'
*'draw string 1.6 0.7 25/00'
*'draw string 4.4 0.7 26/00'
*'draw string 7.2 0.7 27/00'
*'draw string 10.0 0.7 28/00'
*'draw string 12.8 0.7 29/00'
*'draw ylab minimum sea level pressure (mb)'
*'enable print 'fname'.gmf'
*'print'
*'disable print'
*'!gxps -c -i 'fname'.gmf -o 'fname'.ps'
*'!convert -rotate 90 -density 300 -geometry 2000x2000 'fname'.ps 'fname'.jpeg'
'c'
*
* plot vmax          
*
ilat=1
fname=storm'_vmax_'ptime'_'runmode
while (ilat <=ne)
 'set ylpos 0 r'
 'set lat 'ilat
 'set grads off'
 'set xaxis  0 'ftime' 12'
 'set vrange 0 'v2
 'set ylint 5'
 'set cthick 2'
 'set cstyle 1'
 'set ccolor 9'
 'set cmark 0'
 'd vmax'
 ilat=ilat+1
endwhile
'set ylpos 0 l'
'set cthick 18'
'set ccolor 1'
'set cstyle 1'
'set cmark 0'
'set vrange 0 'v2
'set ylint 5'
'd ave(vmax,lat=1,lat='ne')'
'set cthick 18'
'set ccolor 4'
'set cstyle 1'
'set cmark 0'
'set lat 'ne+1
'd vmax'
'draw title Forecast of 'storm' initialized at 'ptime
'draw xlab Forecast time (hours)'
'draw ylab Maximum 10-m wind (hPa)'
'printim vmax.gif white'
*'printim 'fname'.gif white'
*'set strsiz 0.2'
*'set string 1 c 9 0'
*'draw string 1.6 0.7 25/00'
*'draw string 4.4 0.7 26/00'
*'draw string 7.2 0.7 27/00'
*'draw string 10.0 0.7 28/00'
*'draw string 12.8 0.7 29/00'
*'draw ylab maximum surface wind (m/s)'
*'enable print 'fname'.gmf'
*'print'
*'disable print'
*'!gxps -c -i 'fname'.gmf -o 'fname'.ps'
*'!convert -rotate 90 -density 300 -geometry 2000x2000 'fname'.ps 'fname'.jpeg'
*
* print out error
*
rc=write('error_record.txt','error_vmax')
'define vem=ave(vmax,lat=1,lat='ne')'
'define pem=ave(pmin,lat=1,lat='ne')'
'set lon 0'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v0=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p0=subwrd(result1,4)
'set lon 12'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v1=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p1=subwrd(result1,4)
'set lon 24'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v2=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p2=subwrd(result1,4)
'set lon 36'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v3=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p3=subwrd(result1,4)
'set lon 48'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v4=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p4=subwrd(result1,4)
'set lon 60'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v5=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p5=subwrd(result1,4)
'set lon 72'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v6=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p6=subwrd(result1,4)
'set lon 84'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v7=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p7=subwrd(result1,4)
'set lon 96'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v8=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p8=subwrd(result1,4)
'set lon 108'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v9=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p9=subwrd(result1,4)
'set lon 120'
'set lat 'ne+1
'd vmax-vem'
result1=sublin(result,1)
v10=subwrd(result1,4)
'd pmin-pem'
result1=sublin(result,1)
p10=subwrd(result1,4)
*time_series=wtime' 'rc1'   'rc2'   'rc3'   'rc4'   'rc5'   'rc6'   'rc7'   'rc8
rc=write('error_record.txt',v0,append)
rc=write('error_record.txt',v1,append)
rc=write('error_record.txt',v2,append)
rc=write('error_record.txt',v3,append)
rc=write('error_record.txt',v4,append)
rc=write('error_record.txt',v5,append)
rc=write('error_record.txt',v6,append)
rc=write('error_record.txt',v7,append)
rc=write('error_record.txt',v8,append)
rc=write('error_record.txt',v9,append)
rc=write('error_record.txt',v10,append)
rc=write('error_record.txt','error pmin',append)
rc=write('error_record.txt',p0,append)
rc=write('error_record.txt',p1,append)
rc=write('error_record.txt',p2,append)
rc=write('error_record.txt',p3,append)
rc=write('error_record.txt',p4,append)
rc=write('error_record.txt',p5,append)
rc=write('error_record.txt',p6,append)
rc=write('error_record.txt',p7,append)
rc=write('error_record.txt',p8,append)
rc=write('error_record.txt',p9,append)
rc=write('error_record.txt',p10,append)
'!rm -f *.ps *.gmf'


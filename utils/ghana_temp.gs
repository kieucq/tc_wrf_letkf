'reinit'
'open wpost.ctl'
'set mpdset hires'
'set lat 4 11.5'
'set lon -4 2'
'set parea 1.6 10 1.6 7.5'
'set xlopts 1 7 0.16'
'set ylopts 1 7 0.16'
'rgbset.gs'
'set map 1 1 7'
*
* max temperature
*
lat1=10
lon1=-2
lat2=10
lon2=0
lat3=8
lon3=-2
lat4=8
lon4=0
lat5=7
lon5=-3
lat6=7
lon6=-1.6
lat7=7
lon7=0
lat8=6
lon8=-2
lat9=9
lon9=-1
lat10=5
lon10=-2
interval=3
fname='temp'
tim=1
time_series="time         S1   S2   S3   S4   S5   S6   S7   S8   S9   S10"
rc=write('temp_record.txt',time_series)
while (tim <=25)
 hh=(tim-1)*interval
 'set t 'tim
 'set xlint 2'
 'set ylint 2'
 'set grads off'
 'set gxout shaded'
 'set clevs 2 4 6 8 10 12 14 16 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 34 36'
 'set ccols 41 42 43 44 45 46 47 48 49 39 38 37 36 35 34 22 23 24 25 26 27 28 29 69 68 67 66 65'
 'd smth9(t2-273)'
 'cbarn.gs'
*
* draw temp for S1
*
 'q w2xy 'lon1' 'lat1
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat1',lat='lat1'),lon='lon1',lon='lon1')'
 'd temp'
 result1=sublin(result,1)
 temp1=subwrd(result1,4) 
 rc=math_nint(temp1)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S2
*
 'q w2xy 'lon2' 'lat2
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat2',lat='lat2'),lon='lon2',lon='lon2')'
 'd temp'
 result1=sublin(result,1)
 temp2=subwrd(result1,4)
 rc=math_nint(temp2)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S3
*
 'q w2xy 'lon3' 'lat3
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat3',lat='lat3'),lon='lon3',lon='lon3')'
 'd temp'
 result1=sublin(result,1)
 temp3=subwrd(result1,4)
 rc=math_nint(temp3)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S4
*
 'q w2xy 'lon4' 'lat4
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat4',lat='lat4'),lon='lon4',lon='lon4')'
 'd temp'
 result1=sublin(result,1)
 temp4=subwrd(result1,4)
 rc=math_nint(temp4)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S5
*
 'q w2xy 'lon5' 'lat5
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat5',lat='lat5'),lon='lon5',lon='lon5')'
 'd temp'
 result1=sublin(result,1)
 temp5=subwrd(result1,4)
 rc=math_nint(temp5)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S6
*
 'q w2xy 'lon6' 'lat6
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat6',lat='lat6'),lon='lon6',lon='lon6')'
 'd temp'
 result1=sublin(result,1)
 temp6=subwrd(result1,4)
 rc=math_nint(temp6)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S7
*
 'q w2xy 'lon7' 'lat7
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat7',lat='lat7'),lon='lon7',lon='lon7')'
 'd temp'
 result1=sublin(result,1)
 temp7=subwrd(result1,4)
 rc=math_nint(temp7)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S8
*
 'q w2xy 'lon8' 'lat8
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat8',lat='lat8'),lon='lon8',lon='lon8')'
 'd temp'
 result1=sublin(result,1)
 temp8=subwrd(result1,4)
 rc=math_nint(temp8)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S9
*
 'q w2xy 'lon9' 'lat9
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat9',lat='lat9'),lon='lon9',lon='lon9')'
 'd temp'
 result1=sublin(result,1)
 temp9=subwrd(result1,4)
 rc=math_nint(temp9)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* draw temp for S10
*
 'q w2xy 'lon10' 'lat10
 result1 = sublin(result,1)
 x1=subwrd(result1,3)
 y1=subwrd(result1,6)
 x1o=x1-0.3
 y1o=y1-0.2
 x1u=x1+0.3
 y1u=y1+0.2
 'set line 1 1 9'
 'draw rec 'x1o' 'y1o' 'x1u' 'y1u
 'set strsiz 0.15'
 'set string 1 c 9 0'
 'define temp=ave(ave(t2-273,lat='lat10',lat='lat10'),lon='lon10',lon='lon10')'
 'd temp'
 result1=sublin(result,1)
 temp10=subwrd(result1,4)
 rc=math_nint(temp10)
 'draw string 'x1' 'y1' 'rc'`3.`0C'
*
* print title
*
 'q time'
 result1 = sublin(result,1)
 wtime = subwrd(result1,3)
 'draw title 'wtime' +2 (local time)'
 'set gxout vector'
 'set arrowhead 0.11'
 'set arrscl 0.5 10'
 'printim 'fname'_'hh'.gif white'
* '!gxps -c -i 'fname'_'tim'.gmf -o 'fname'_'tim'.ps'
* '!convert -rotate 90 -density 300 -geometry 2000x2000 'fname'_'tim'.ps 'fname'_'tim'.pdf'
 rc1=math_nint(temp1)
 rc2=math_nint(temp2)
 rc3=math_nint(temp3)
 rc4=math_nint(temp4)
 rc5=math_nint(temp5)
 rc6=math_nint(temp6)
 rc7=math_nint(temp7)
 rc8=math_nint(temp8)
 rc9=math_nint(temp9)
 rc10=math_nint(temp10)
 time_series=wtime' 'rc1'   'rc2'   'rc3'   'rc4'   'rc5'   'rc6'   'rc7'   'rc8'   'rc9'   'rc10
 rc=write('temp_record.txt',time_series,append)
 tim=tim+1
 'c'
endwhile

'!rm -f *.gmf *.ps'


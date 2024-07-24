'reinit'
'open wpost.ctl'
'set mpdset hires'
'set lat 3 13'
'set lon -6 4'
'set parea 1.6 10 1.6 7.5'
'set xlopts 1 7 0.16'
'set ylopts 1 7 0.16'
'set map 4 1 7'
'set rgb  71  255   255   255'
'set rgb  72  235   235   235'
'set rgb  73  215   215   215'
'set rgb  74  200   200   200'
'set rgb  75  190   190   190'
'set rgb  76  180   180   180'
'set rgb  77  170   170   170'
'set rgb  78  160   160   160'
'set rgb  79  150   150   150'
'set rgb  80  140   140   140'
'set rgb  81  130   130   130'
'set rgb  82  120   120   120'
'set rgb  83  100   100   100'
'set rgb  84  90    90     90'
'set rgb  85  80    80     80'
'set rgb  86  70    70     70'
'set rgb  87  60    60     60'
'set rgb  88  50    50     50'
'set rgb  89  40    40     40'
'set rgb  90  30    30     30'
'set rgb  91  20    20     20'
'set rgb  92  10    10     10'
'set rgb  93  0     0      0'
*
* max temperature
*
lat1=21
lon1=106
lat2=22
lon2=105
lat3=22
lon3=103
fname='cloud'
tim=1
time_series="time         HN   TB   VB"
rc=write('cloud_record.txt',time_series)
interval=3
while (tim <=25)
 hh=(tim-1)*interval
 'set t 'tim
 'set xlint 2'
 'set ylint 2'
 'set grads off'
 'set gxout shaded'
 'set clevs 50 70 90 110 130 150 170 200 230 270 300 350 400 500 700 900 1100 1300 1600'
 'set ccols 92 91 90 89 88 87 86 85 84 83 82 81 80 79 77 75 74 73 72 71'
 'd smth9(ave(qcloud,lev=1000,lev=50))*1e8'
 'cbarn.gs'
*
* draw temp for Hanoi
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
* draw temp for TayBac
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
* draw temp for VietBac
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
 time_series=wtime' 'rc1'   'rc2'   'rc3
 rc=write('cloud_record.txt',time_series,append)
 'c'
 tim=tim+1
endwhile
'!rm -f *.gmf *.ps'


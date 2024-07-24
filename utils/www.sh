#!/bin/sh
#
# set html product
#
#fname="s"
#string0="slvl"
#string2="Sea level pressure"
#fname="r"
#string0="rh500"
#string2="Relative humidity at 500 mb"
#fname="t"
#string0="temp"
#string2="Surface temperature"
#fname="w"
#string0="wind"
#string2="Surface wind speed"
fname="p"
string0="prep"
string2="Accumulated rainfall"
#
# set time input
#
fst_length=72
out_interval=3
ntime=$(($fst_length/$out_interval + 1))
ie=1
itime=0
while [ "$ie" -le "$ntime" ]; do                                                                
#
# link wrfout data 
#
 string1="${string0}_${itime}.gif" 
 cat > ${fname}${itime}.html << EOF
<html>
<head>
<title>WRF-LETKF real time</title>
<CENTER> 
Hi There! This is the real time forecast of the WRF-LETKF developed by 
Lab of Climate and Weather Research, Vietnam National University <BR>
</CENTER>
<P>
</P>
<A HREF="index1.html">Main</A>
<TABLE CELLSPACING=1>
<TR> <TD>Sea level pressure:</TD> 
EOF
 je=1
 jtime=0
 while [ "$je" -le "$ntime" ]; do  
  cat >> ${fname}${itime}.html << EOF1
     <TD> <A HREF="s${jtime}.html">${jtime}z</A> </TD>     
EOF1
  je=$(($je+1))
  jtime=$(($je*out_interval))
  jh=$(($je*out_interval))
 done
  cat >> ${fname}${itime}.html << EOF2 
</TR><TR> <TD>Surface temperature: </TD> 
EOF2
 je=1
 jtime=0
 while [ "$je" -le "$ntime" ]; do  
  cat >> ${fname}${itime}.html << EOF3
     <TD> <A HREF="t${jtime}.html">${jtime}z</A> </TD>     
EOF3
  je=$(($je+1))
  jtime=$(($je*out_interval))
  jh=$(($je*out_interval))
 done
 cat >> ${fname}${itime}.html << EOF4     
</TR><TR> <TD>Relative humidity at 500 mb:</TD> 
EOF4
 je=1
 jtime=0
 while [ "$je" -le "$ntime" ]; do  
  cat >> ${fname}${itime}.html << EOF5
     <TD> <A HREF="r${jtime}.html">${jtime}z</A> </TD>     
EOF5
  je=$(($je+1))
  jtime=$(($je*out_interval))
  jh=$(($je*out_interval))
 done
 cat >> ${fname}${itime}.html << EOF6     
</TR><TR> <TD>Surface wind speed:</TD> 
EOF6
 je=1
 jtime=0
 while [ "$je" -le "$ntime" ]; do  
  cat >> ${fname}${itime}.html << EOF7
     <TD> <A HREF="w${jtime}.html">${jtime}z</A> </TD>     
EOF7
  je=$(($je+1))
  jtime=$(($je*out_interval))
  jh=$(($je*out_interval))
 done
 cat >> ${fname}${itime}.html << EOF9
</TR><TR> <TD>Accumulated rainfall:</TD>
EOF9
 je=1
 jtime=0 
 while [ "$je" -le "$ntime" ]; do  
  cat >> ${fname}${itime}.html << EOF10
     <TD> <A HREF="p${jtime}.html">${jtime}z</A> </TD>
EOF10
  je=$(($je+1))
  jtime=$(($je*out_interval))
  jh=$(($je*out_interval))
 done

 cat >> ${fname}${itime}.html << EOF8
</TABLE>

<TABLE BORDER=1 CELLSPACING=10>
<TR> <TD><CENTER> ${string2} - <A HREF="./Figures/mem_001/${string1}"> member 1</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_001/${string1}" ALT="Member_1" WIDTH=430 HEIGHT=270> </TD>     
     <TD><CENTER> ${string2} - <A HREF="./Figures/mem_002/${string1}"> member 2</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_002/${string1}" ALT="Member_2" WIDTH=430 HEIGHT=270> </TD> 
     <TD><CENTER> ${string2} - <A HREF="./Figures/mem_003/${string1}"> member 3</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_003/${string1}" ALT="Member_3" WIDTH=430 HEIGHT=270> </TD> </TR> 
<TR> <TD><CENTER> ${string2} - <A HREF="./Figures/mem_004/${string1}"> member 4</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_004/${string1}" ALT="Member_4" WIDTH=430 HEIGHT=270> </TD>     
     <TD><CENTER> ${string2} - <A HREF="./Figures/mem_005/${string1}"> member 5</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_005/${string1}" ALT="Member_5" WIDTH=430 HEIGHT=270> </TD>
     <TD><CENTER> ${string2} - <A HREF="./Figures/mem_006/${string1}"> member 6</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_006/${string1}" ALT="Member_6" WIDTH=430 HEIGHT=270> </TD>
<TR> <TD><CENTER> ${string2} - <A HREF="./Figures/mem_007/${string1}"> member 7</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_007/${string1}" ALT="Member_7" WIDTH=430 HEIGHT=270> </TD>
     <TD><CENTER> ${string2} - <A HREF="./Figures/mem_008/${string1}"> member 8</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_008/${string1}" ALT="Member_8" WIDTH=430 HEIGHT=270> </TD>
     <TD><CENTER> ${string2} - <A HREF="./Figures/mem_009/${string1}"> member 9</A> </CENTER> <BR> 
            <IMG SRC="./Figures/mem_009/${string1}" ALT="Member_9" WIDTH=430 HEIGHT=270> </TD> </TR>
</TABLE>
Contact address: Chanh Kieu, Lab of Climate and Weather Research, College of Science, 334 Nguyen Trai, 
Hanoi, Vietnam. Email: kieucq.at.atmos.umd.edu <BR> <A HREF="index1.html">Main</A>
</html>
EOF8
ie=$(($ie+1))
itime=$(($itime+$out_interval))
done


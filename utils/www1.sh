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
fname="w"
string0="wind"
string2="Surface wind speed"
#fname="p"
#string0="prep"
#string2="Accumulated rainfall"
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
This is a demo real time forecast of the WRF-LETKF developed by 
I. M. System Group, USA <BR>
</CENTER>
<P>
</P>
<A HREF="index.html">Main</A>
<P>
</P>
<TABLE CELLSPACING=1>
<TR> <TD>VARIABLE:</TD> 
     <TD> <A HREF="s0.html">Sea level pressure |</A></TD>
     <TD> <A HREF="t0.html">Surface temperature |</A></TD>
     <TD> <A HREF="w0.html">Wind |</A></TD>
     <TD> <A HREF="r0.html">Relative humidity |</A></TD>
     <TD> <A HREF="p0.html">Accumulated precipitation</A></TD>
</TABLE>
<TABLE CELLSPACING=1>
<TR> <TD>TIME:</TD> 
EOF
 je=1
 jtime=0
 while [ "$je" -le "$ntime" ]; do  
  cat >> ${fname}${itime}.html << EOF1
     <TD> <A HREF="${fname}${jtime}.html">${jtime}h</A> </TD>     
EOF1
  jtime=$(($je*out_interval))
  je=$(($je+1))
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
Contact address: Chanh Kieu, I. M. System Group, USA. email: kieuc@imsg.com <BR> <A HREF="index.html">Main</A>
</html>
EOF8
ie=$(($ie+1))
itime=$(($itime+$out_interval))
done


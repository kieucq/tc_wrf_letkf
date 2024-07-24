#!/bin/sh
#
#C ? runbufr  - Script which drives winds BUFR encoder program,
#C ?            gwin2bufr
#C ?
#C ? usage:   runbufr <sat> <product> <mdfile> <stationid> <outfile>
#C ?
#C ?          <sat>      - Satellite to process (E,W,C)
#C ?                          E --> GOES-E
#C ?                          W --> GOES-W
#C ?                          C --> GOES-C
#C ?                          T --> MODIS Terra
#C ?                          A --> MODIS Aqua
#C ?                          OP --> MetOp
#C ?                          7-19 --> NOAA-7 through NOAA-19
#C ?          <product>  - Wind product to process 
#C ?                          cdft --> Cloud-drift winds
#C ?                          wvap --> Imager vwater vapor winds
#C ?                          visb --> Visible winds
#C ?                          wv10 --> Sounder water vapor winds (7.3um)  
#C ?                          wv11 --> Sounder water vapor winds (7.0um)  
#C ?                          swir --> Short wave winds (3.9um)  
#C ?          <mdfile>   -  McIDAS MD dataset name (MDXXnnnn); input MD file
#C ?                        to BUFR encoder program.  It is assumed that the
#C ?                        file has both water vapor and cloud drift winds.
#C ?          <stationid> - A station identifier, used in the output file name
#C ?                        and to determine the subcenter value.  Tromso=ENTC,
#C ?                        McMurdo=MCMR, Sodankyla=EFSO, Fairbanks=PAFA,
#                           Barrow=PBRW, Rothera=EGAR, otherwise 0. See note below.
#C ?          <outfil>    - Output BUFR file name
#
#  ASSUMPTIONS/REQUIREMENTS: 
#    This must be run while in the directory where the MD file is, and that
#    directory is in MCPATH. MCPATH is not set here, so it must be set in
#    the calling program.
#
#  NOTES:
#    - The station identifiers are mostly from the International Civil Aviation Organization (ICAO)
#      identifier list. McMurdo is incorrect (should be NZCM), and Barrow's PBRW code is the
#      raob station code (ICAO code is PABR).
#
#   Version- 1.0   Date: 01/12/99  Programmer:  Jaime Daniels (ORA/FPDT)
#   Modified by W. Straka and J. Key, 05/06 and beyond.
#------------------------------------------------------------

echo 'Starting BUFR encoding...'

# We need the 32-bit gcc library on 64-bit computers until we work out the 64-bit issue.
# On 32-bit computers it we don't need these lines, but they don't seem to hurt.

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/backup/libs32
export LD_LIBRARY_PATH

# Define Enviromental Variables

export PP_GEN_RES=1
export PP_SUBCENTRE=2
export BUFR_TABLES=$HOME/windco/bufr/  #JK location
#export BUFR_TABLES=$HOME/BUFF/bufr/	#WCS location
# NOTE: MCPATH is exported by the calling program, step3_rtMODIS_db.sh.
# That script also put us in the directory with the MD file.  For testing
# outside of the wind processing system, work from the $HOME/mcidas/data
# directory (uncomment the next two lines).
#cd $HOME/mdicas/data
export PATH=$PATH:/home/mcidas/bin:$HOME/mcidas/bin:$HOME/windco/bin
export PATH=$PATH:$HOME/windco/fortran/base:$HOME/windco/fortran/local
export PATH=$PATH:/usr/local/bin
export MCPATH=$HOME/mcidas/data:/home/mcidas/data

bufrdir=$BUFR_TABLES

# Check input for correct number of values.

if [ $# -ne 5 ]; then
echo
echo '>>>' PLEASE ENTER CORRECT NUMBER OF ARGUMENTS
echo '>>>'
echo '>>>'  usage : runbufr '<'sat'>' '<'product'>' '<'mdfile'>' '<'stationid'>' '<'outfil'>' 
echo '>>>        <sat>      - Satellite to process (E,W,C)'
echo '>>>                        E --> GOES-E'
echo '>>>                        W --> GOES-W'
echo '>>>                        C --> GOES-C'
echo '>>>                        T --> MODIS Terra'
echo '>>>                        A --> MODIS Aqua'
echo '>>>                        M --> GMS5  '
echo '>>>                        T --> MODIS Terra'
echo '>>>                        A --> MODIS Aqua'
echo '>>>                        OP --> MetOp'
echo '>>>                        7-19 --> NOAA-7 through NOAA-19'
echo '>>>'
echo '>>>        <product>  - Wind product to process'
echo '>>>                        cdft --> Cloud-drift winds'
echo '>>>                        wvap --> Imager vwater vapor winds'
echo '>>>                        visb --> Visible winds'
echo '>>>                        wv10 --> Sounder water vapor winds (7.3um)'
echo '>>>                        wv11 --> Sounder water vapor winds (7.0um)'
echo '>>>                        swir --> Short wave winds (3.9um)'
echo '>>>'
echo '>>>        <mdfile>   -  McIDAS MD dataset name (MDXXnnnn); input MD file'
echo '>>>                      to BUFR encoder program, assumed to have both'
echo '>>>                      WV and cloud drift winds.'
echo '>>>'
echo '>>>        <stationid> - The station identifier (e.g., MCMR)'
echo '>>>'
echo '>>>        <outfil>    - Output BUFR file name'
exit
fi

# Input arguments

SAT_ID=$1
type=$2
MD_FILE=$3
stati=$4
outfil=$5

echo OUTFIL: $outfil

# Remove old files

rm -f KNES_si.SSD*
rm -f JUTX*
rm -f bufrpost*
rm -f sy1
rm -f sy3
rm -f sy5
rm -f fort.10
rm -f fort.11

filep=CX
filef=I

if [ "$SAT_ID" = E ]; then
  sat=1
  if [ "$type" = cdft ]; then IACX1=A 
  elif [ "$type" = wvap ]; then IACX1=E 
  elif [ "$type" = visb ]; then IACX1=H
  elif [ "$type" = wv10 ]; then IACX1=K 
  elif [ "$type" = wv11 ]; then IACX1=N 
  elif [ "$type" = pt ]; then IACX1=S 
  elif [ "$type" = swir ]; then IACX1=Q 
  fi      
fi
#
if [ "$SAT_ID" = W ]; then
  sat=5
  if [ "$type" = cdft ]; then IACX1=C 
  elif [ "$type" = wvap ]; then IACX1=G 
  elif [ "$type" = visb ]; then IACX1=J 
  elif [ "$type" = wv10 ]; then IACX1=M 
  elif [ "$type" = wv11 ]; then IACX1=P 
  elif [ "$type" = pt ]; then IACX1=T 
  elif [ "$type" = swir ]; then IACX1=R 
  fi      
fi
#
if [ "$SAT_ID" = T ]; then
  sat=terra
  if [ "$type" = cdft ]; then IACX1=B 
  elif [ "$type" = wvap ]; then IACX1=F 
  fi      
fi      
#
if [ "$SAT_ID" = A ]; then
  sat=aqua
  if [ "$type" = cdft ]; then IACX1=I 
  elif [ "$type" = wvap ]; then IACX1=L 
  fi      
fi
# JK: IACX1 below isn't correct, cuz I copied the values from Aqua!
# Check if the input argument is an integer.
#
if [ "$SAT_ID" = "OP" ]; then
  sat=metop
  if [ "$type" = cdft ]; then IACX1=I 
  fi      
fi
echo $SAT_ID | grep [^0-9] > /dev/null 2>&1
if [ "$?" -ne "0" ]; then
  if [ "$SAT_ID" -ge 7 -a "$SAT_ID" -le 19 ]; then
    sat=noaa
    if [ "$type" = cdft ]; then IACX1=I 
    fi
  fi
fi

# For MODIS, split the IR and WVAP winds out of the MD file and create 
# separate, temporary files.  Then use the temporary MD file in bufr 
# conversion.  For AVHRR, the splitting isn't necessary (but doesn't hurt,
# so we leave it), and we use the original MD file.

# Add in GWIN schema

sche.k DAGWIN

#Add DSSERVE for this run

dsserve.k ADD M/M MD 1 9999

# Separate cloud drift winds out.  NOTE: It is assumed that we're in the
# directory where the MD file exists, and that that directory is in MCPATH.

#if [ "$stati" = "ENTC" -o "$stati" = "MCMR" -o  "$stati" = "EFSO" ]; then
#  cp $MD_FILE MDXX0001
#else
  cp $MD_FILE $HOME/mcidas/data/MDXX0005
  ptcopy.k M/M.5 M/M.1
  rm $HOME/mcidas/data/MDXX0005
#fi

if [ "$type" = cdft ]; then
  mceval.k MDU DEL 2 2
  mceval.k MDU MAKE 2 GWIN X 1 30000 X
  scopy.k 1 2 1 1 CK1=TYPE CV1=IR ADD=YES
fi

# Separate out the water vapor winds

if [ "$type" = wvap ]; then
  mceval.k MDU DEL 2 2
  mceval.k MDU MAKE 2 GWIN X 1 30000 X
  scopy.k 1 2 1 1 CK1=TYPE CV1=WV ADD=YES
fi

if [ "$sat" = "terra" -o "$sat" = "aqua" -o "$sat" = "metop" -o "$sat" = "noaa" ]; then
  MD_FILE=MDXX0002
fi

#move MDXX0002 to proper directory

mv $HOME/mcidas/data/MDXX0002 .

#delete MDXX0001
rm $HOME/mcidas/data/MDXX0001


bufrfile=$filef$IACX1$filep
echo $bufrfile goober
subcent=0
if [ "$stati" = "ENTC" ]; then      # Tromso
  subcent=10
elif [ "$stati" = "MCMR" ]; then    # McMurdo (not a WMO id)
  subcent=11
elif [ "$stati" = "EFSO" ]; then    # Sodankyla
  subcent=12
elif [ "$stati" = "PAFA" ]; then    # Fairbanks
  subcent=13
elif [ "$stati" = "PBRW" ]; then    # Barrow HRPT
  subcent=14
elif [ "$stati" = "EGAR" ]; then    # Rothera HRPT
  subcent=15
fi

rm -f *$bufrfile*

# Run BUFR Encoder Program for Winds
# MDXX0002 is a temporary MD file created when the two wind types are split
# out of the MD file.  The original code (commented out) assumed the MD
# file only had one or the other.

#date +%y%j%T | read bufrpost5
bufrpost5=`date +%Y%%j%T`
echo $bufrpost5 >> sy1
echo $bufrdir/gwin2bufr2 $MD_FILE $bufrfile $SAT_ID $subcent >> sy1
$bufrdir/gwin2bufr2 $MD_FILE $bufrfile $SAT_ID $subcent

# Build output dataset name based on input to this script 

day=`date +%j`
hour=`date +%H`
minute=`date +%M`
second=`date +%S`

#echo $day
#echo $time
space=' '
for i in 1 2 3 4 5 6 7 8 9 0
do 
rm -f filelist
command_line=$i$space$IACX1
ls JUTX$i*KNES* >>filelist
finp="filelist"
IFS="
    , ;!?"
while read line
do
    if [[ "$line" != "" ]]
then 
    set $line
    tmpfil=$*
#    print $tmpfil
    echo $tmpfil
    command_line=$command_line$space$tmpfil
fi
done < $finp
echo "$bufrdir/head_add $command_line" >> bufrpost2
#$bufrdir/head_add $command_line
done

# Check and delete the 0 length file
echo 'ls KNES_si.SSD* >bufr_list' >> bufrpost2
echo 'finp="bufr_list"'>> bufrpost2
echo 'IFS=" '>> bufrpost2
echo ' , ;!?" '>> bufrpost2
echo ' while read line '>> bufrpost2
echo ' do '>> bufrpost2
echo '     if [[ "$line" != "" ]] '>> bufrpost2
echo ' then '>> bufrpost2
echo '     set $line '>> bufrpost2
echo '     bufrmessage=$* '>> bufrpost2
echo '     if [ ! -s $bufrmessage ] '>> bufrpost2
echo ' then '>> bufrpost2
echo '     rm $bufrmessage '>> bufrpost2
#    cp $MD_FILE bad_md/MDXX4000$bufrpost5
#    echo "0 length data at $bufrpost5 for $MD_FILE"|mail yi.song@noaa.gov
echo ' fi '>> bufrpost2
echo ' fi '>> bufrpost2
echo ' done < $finp '>> bufrpost2

# create the file name for ftp server
#strtfl=satwnd.bufr$type.$sat 
#day=`date -u +%y%j`
#time=`date -u +%H%M`
#echo $day
#echo $time
#outfil=$strtfl.D$day.T$time"Z".$stati
echo "outfil=$outfil" >>bufrpost2 
echo "SAT_ID=$SAT_ID" >>bufrpost2 
#echo 'if [ "$SAT_ID" = T ] || [ "$SAT_ID" = A ]' >>bufrpost2 
#echo 'then ' >>bufrpost2
echo 'cp $bufrmessage $outfil' >> bufrpost2
#echo 'fi' >> bufrpost2
chmod +x bufrpost2
#echo "bufrpost2 >> sy3" >> sy5
./bufrpost2 >>sy3 

echo "Done with BUFR conversion."

#Deletes tempoary files

rm -f sy1
rm -f sy3
rm -f MDXX0002
rm -f MDXX0001


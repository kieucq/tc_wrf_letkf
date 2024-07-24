#!/bin/sh
iline=5
out_file=track_mem_27.txt
rm -rf ${out_file}
while [ "$iline" -le "29" ]; do
 echo "working at line $iline"
 ifile=1
 lat_out=0
 lon_out=0
 nsum=21
 while [ "$ifile" -le "21" ]; do
  input_file="track_mem_${ifile}.txt"
  itime=`awk 'NR == '${iline}' {print $1}' ${input_file}`
  lon_i=`awk 'NR == '${iline}' {print $2}' ${input_file}`
  lon_out=`echo "$lon_out + $lon_i" | bc -l`
  lat_i=`awk 'NR == '${iline}' {print $3}' ${input_file}`
  lat_out=`echo "$lat_out + $lat_i" | bc -l`
  ifile=$(($ifile+1))
 done
 lat_mean=`echo "$lat_out/21" | bc -l`
 lat_mean=`printf "%.2f\n" $lat_mean`
 lon_mean=`echo "$lon_out/21" | bc -l`
 lon_mean=`printf "%.2f\n" $lon_mean`
 echo "lat_out=$lat_out, lat_mean=$lat_mean"
 echo "$itime  $lon_mean  $lat_mean"   >> ${out_file}
 iline=$(($iline+1))
done

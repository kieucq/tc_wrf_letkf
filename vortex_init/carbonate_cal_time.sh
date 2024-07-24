     function cal_time()
{
     S_YEAR=$1
     S_MONTH=$2
     S_DAY=$3
     S_HOUR=$4
     S_MINUTE=$5
     RUN_DAYS=$6
     RUN_HOURS=$7
     RUN_MINUTES=$8
     E_YEAR="$S_YEAR"
     E_MONTH="$S_MONTH"
     E_DAY=`expr $S_DAY + $RUN_DAYS` 
     E_HOUR=`expr $S_HOUR + $RUN_HOURS`
     E_MINUTE=`expr $S_MINUTE + $RUN_MINUTES`
     until [ ${E_MINUTE} -lt 60 -a ${E_MINUTE} -ge 0 ]; do
        if [ ${E_MINUTE} -lt 0 ]; then
             let "E_MINUTE += 60"
             let "E_HOUR -= 1"
        elif [ ${E_MINUTE} -ge 60 ]; then
             let "E_MINUTE -= 60"
             let "E_HOUR += 1"
        fi
     done
     until [ ${E_HOUR} -lt 24 -a ${E_HOUR} -ge 0 ]; do
        if [ ${E_HOUR} -lt 0 ]; then
             let "E_HOUR += 24"
             let "E_DAY -= 1"
        elif [ ${E_HOUR} -ge 24 ]; then
             let "E_HOUR -= 24"
             let "E_DAY += 1"
        fi
     done 
     DAY_TAIL=`echo \`cal ${E_MONTH} ${E_YEAR}\` |tail -n1 |awk '{print $NF}'`
     until [ ${E_DAY} -le $DAY_TAIL -a ${E_MONTH} -le 12 ]; do
        let "E_DAY -= ${DAY_TAIL}"
        E_MONTH=`expr ${E_MONTH} + 1`
        if [ "${E_MONTH}" -gt "12" ]; then
           let "E_MONTH -= 12"
           let "E_YEAR += 1"
        fi
        DAY_TAIL=`echo \`cal ${E_MONTH} ${E_YEAR}\` |tail -n1 |awk '{print $NF}'`
     done
     E_YEAR=`echo "$E_YEAR"|awk '{if (length($1)==1) $1=200$1; printf "%s",$1}'`
     E_YEAR=`echo "$E_YEAR"|awk '{if (length($1)==2) $1=20$1; printf "%s",$1}'`
     E_MONTH=`echo "${E_MONTH}"|awk '{if (length($1)==1) $1=0$1; printf "%s",$1}'`
     E_DAY=`echo "${E_DAY}"|awk '{if (length($1)==1) $1=0$1; printf "%s",$1}'`
     E_HOUR=`echo "${E_HOUR}"|awk '{if (length($1)==1) $1=0$1; printf "%s",$1}'`
     E_MINUTE=`echo "${E_MINUTE}"|awk '{if (length($1)==1) $1=0$1; printf "%s",$1}'`
}


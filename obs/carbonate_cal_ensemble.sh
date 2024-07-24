     function cal_ensemble_prefix()
{     
     imember=$1
     if [ "$imember" -lt 10 ]; then
      out_prefix="00${imember}"
     elif [ "$imember" -lt 100 ]; then
      out_prefix="0${imember}"
     elif [ "$imember" -lt 1000 ]; then
      out_prefix="${imember}"
     else
      echo " MAIN: Too many members...exit 10"
      exit 10
     fi
}


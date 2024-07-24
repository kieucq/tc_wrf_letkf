# functions
#	This file includes all the functions to be used in scripts
# Author: Kien T. Nguyen
# Contact: ngtrungkien311@gmail.com
# Created on: Oct 21 2010
# Last updated: Oct 25 2010

ctime() {
	# Check whether the input has the form of: YYYYMMDDHH
	# Example: ctime 2010102018

	local _DD=${1:0:8}
	local _HH=${1:8:2}	

	[ ${#1} == 10 ] && date -d "${_DD} ${_HH}" > /dev/null 2>&1 \
					|| ( echo "${0##*/}(1): invalid time \`$1'"; return 1 )
}

subaddh() {
	# Add/Sub an amount of hours to/from a given date
	# and return YYYY, MM, DD, HH
	# Example: subaddh 2010102018 13 YYYY MM DD HH +

	#[ $# == 6 ] || ( echo "usage: addh <input_date(YYYYMMDDHH)> <range(h)> <YYYY> <MM> <DD> <HH>"; return 1 )

	ctime $1 || return 1
	test $2 -ge 0 2> /dev/null || ( echo "${0##*/}(1): range must be an integer"; return 1 )

	local _DATE=${1:0:8}
	local _YY=${1:0:4}
	local _MM=${1:4:2}
	local _DD=${1:6:2}
	local _HH=${1:8:2}	

	range=$(($2*3600))

	local _ED_YY=$3
	local _ED_MM=$4
	local _ED_DD=$5
	local _ED_HH=$6
	local _OP=$7

	local _DATE_STR="1970-01-01 $(($(date -ud "${_DATE} ${_HH} UTC" "+%s") ${_OP} ${range})) sec"
	eval ${_ED_YY}=`date -ud "${_DATE_STR}" "+%Y"`
	eval ${_ED_MM}=`date -ud "${_DATE_STR}" "+%m"`
	eval ${_ED_DD}=`date -ud "${_DATE_STR}" "+%d"`
	eval ${_ED_HH}=`date -ud "${_DATE_STR}" "+%H"`
}

addh() {
	[ $# == 6 ] || ( echo "usage: addh <input_date(YYYYMMDDHH)> <range(h)> <YYYY> <MM> <DD> <HH>"; return 1 )
	subaddh $1 $2 $3 $4 $5 $6 +
}

subh() {
	[ $# == 6 ] || ( echo "usage: subh <input_date(YYYYMMDDHH)> <range(h)> <YYYY> <MM> <DD> <HH>"; return 1 )
	subaddh $1 $2 $3 $4 $5 $6 -
}

ktqsub() {
	# Submit a job and wait until it finishes
	local _JOBID=`qsub $1`
	echo "JOBID: ${_JOBID}"
	[ -z ${_JOBID} ] && return 1
	local _J_FIN=1
	while [[ ${_J_FIN} -eq 1 ]]; do 
		sleep 5
		jobchk ${_JOBID}
		_J_FIN=$?
	done
}

jobchk() {
	# Check if a job is running
	# return 0 if no, 2 if any error occured, 1 otherwise (R, Q, E, ...)
	local _J_STAT=`qstat $1 | tail -n 1 | awk '{print $5}'` 
	[ -z ${_J_STAT} ] && return 2
	( [ ${_J_STAT} == "C" ] && return 0 ) || return 1
}

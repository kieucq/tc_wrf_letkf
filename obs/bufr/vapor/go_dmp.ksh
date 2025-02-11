#!/bin/ksh
# go_dmp.ksh
# trigger for dmpbfr.x (routine to read bufr files)
#
# 27 May 2004 - orig+
#  7 Mar 2005 - enabled gzipped source files
#
usage='go_dmp <BUFR file to read>'
lab='go_dmp'

bfile=${1:?"$usage"}

TDIR=/stmp/$USER/go_dmp
[ -d $TDIR ] || mkdir $TDIR || { echo "error mkdir $TDIR - exiting" ; exit ; }

if [ ! -s $bfile ] ; then 
  echo "$lab: $bfile not found - exiting"
else

  tfile=''
  if [ .`echo $bfile | egrep 'gz$'` != . ] ; then
    zfile=$TDIR/`basename $bfile`
    cp -p $bfile $zfile
    gzip -d $zfile
    tfile=`echo $zfile | sed 's/.gz//'`
    bfile=$tfile
  fi # file zipped

  dmplst='/nfsuser/g01/wx22dk/fix.jifs/bufr_dumplist'
  dmplst='/nwprod/fix/bufr_dumplist'

  tank=`basename $bfile`
  case "`echo $tank | cut -c1-2" in
    xx) #mtst=`echo $bfile | cut -d/ -f5- | sed 's=/xx==;s/b//'`
        mtst=`echo $bfile | awk '{print substr($0,index($0,"/b0")+1,10)}' \
              | sed 's=/xx==;s/b//'`
        mtst=`egrep "$mtst  #" $dmplst | cut -c2-9` ;;
     *) mtst=$tank ;;
  esac

  echo "$lab:  $bfile  $mtst"

  export XLFRTEOPTS="unit_vars=yes"
  export XLFUNITS=0
  export XLFUNIT_11="$bfile"

  /global/save/wx22gk/useful_misc/dmp/dmpbfr.x
  dmprc=$?

  echo "$lab: dmpbfr rc=$dmprc"

  if [ -n "$tfile" ] ; then rm -f $tfile ; fi

fi # bfile found

exit

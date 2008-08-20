#!/bin/bash

# Don't stop on errors
#set -e
#set -x

TESTSUITEDIR=$HOME/testsuite
INCOMINGDIR=$TESTSUITEDIR/incoming
FAILEDDIR=$TESTSUITEDIR/failed
WORKDIR=$TESTSUITEDIR/work
TESTSOURCEDIR=$HOME/src/lazarus
TESTSUITEBINDIR=$TESTSUITEDIR/bin
DATENAME=`date +%Y-%m-%d-%0k-%M-%S`
LOGFILE=$HOME/logs/updatetestsuitedb.$DATENAME.log
DBCFG=$TESTSUITEBINDIR/dbdigest.cfg.dbdata

# processfile <file>
processfile() {
  echo
  echo "`date`: Start processing file $1"
  echo

  basefn=`basename $1`
  
  rm -rf $WORKDIR
  mkdir -p $WORKDIR

  cd $WORKDIR
  cp -pr $1 $WORKDIR
  
#    better avoid usernames and passwords in the script (available in SVN among others)
#     echo "DatabaseName=TESTSUITE" >> dbdigest.cfg
#     echo "UserName=yyy" >> dbdigest.cfg
#     echo "PassWord=xxx" >> dbdigest.cfg
  if [ -f $DBCFG ]; then
    cat $DBCFG >> dbdigest.cfg
  if [ -f dbdigest.cfg ]; then
    # remove unit paths from testsuite parameters
    sed -e 's/-Fu[^ ]*//' < dbdigest.cfg > dbdigest.cfg.new
    mv dbdigest.cfg.new dbdigest.cfg

#    doesn't work if hostname contains number
#    date=`echo $1 | sed 's/[^0-9]*\([0-9]*\)[^0-9]*/\1/'`
#   next won't work if hostname contains 12 consecutive numbers, better (jonas)
     date=`echo $1 | sed 's/.*\([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\).*/\1/'`
#     echo "Date=$date" >> dbdigest.cfg
     echo "TestSrcDir=$TESTSOURCEDIR" >> dbdigest.cfg
     echo "logfile=$1" >> dbdigest.cfg


     $TESTSUITEBINDIR/importtestresults
     err=$?
    else
     echo "ERROR: No dbdigest.cfg found!"
     err=1
    fi
  else
    echo "ERROR: No $DBCFG found!"
    err=1
  fi

  if [ $err -eq 0 ]; then
    rm -f $1
  else
    echo "ERROR: dbdigest failed, moving file $basefn to $FAILEDDIR"
    mv $1 $FAILEDDIR/$basefn
  fi
  
  echo "Done"
}


#
# First check if there are new files to process
#
FILES=`ls $INCOMINGDIR/results-*.xml 2> /dev/null`
if [ "$FILES" = "" ]; then
  exit 0
fi

PATH=".:/bin:/usr/bin"
(
date

# Update sources
cd $TESTSOURCEDIR 
svn up

for f in $FILES; do
    processfile $f
done
echo "`date`: Finished."
) > $LOGFILE 2>&1 </dev/null

gzip $LOGFILE

#!/bin/bash

#set -x
set -e

# get date of day
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`

# get installed fpc version
FPCRPM=`rpm -qa | egrep '^fpc-'`
if [ "x$FPCRPM" = "x" ]; then
  echo ERROR: fpc rpm not installed
  exit
fi

Date=$Year$Month$Day
LazVersion=0.9.0.10
LazRelease=`echo $FPCRPM | sed -e 's/-/_/g'`
SrcTGZ=lazarus-$Date.tgz
TmpDir=/tmp/lazarus$LazVersion
SpecFile=lazarus-$LazVersion-$LazRelease.spec

# download lazarus cvs if necessary
if [ ! -f $SrcTGZ ]; then
  ./create_lazarus_export_tgz.sh $SrcTGZ
fi

# put src tgz into rpm build directory
cp $SrcTGZ /usr/src/redhat/SOURCES/

# create spec file
cat lazarus.spec | \
  sed -e "s/LAZVERSION/$LazVersion/g" \
      -e "s/LAZRELEASE/$LazRelease/" \
      -e "s/LAZSOURCE/$SrcTGZ/" \
  > $SpecFile

# build rpm
rpm -ba $SpecFile || rpmbuild -ba $SpecFile

# end.


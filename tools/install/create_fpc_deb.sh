#!/bin/bash

set -x
set -e

Usage="Usage: $0 devel or stable"

FPCVersion=$1

FPCVersionOk=no
for ver in devel stable; do
  if [ "x$FPCVersion" = "x$ver" ]; then
    FPCVersionOk=yes
  fi
done
if [ "x$FPCVersionOk" = "xno" ]; then
  echo $Usage
  exit -1
fi


# set here the fpc cvs dates for the various versions
if [ "x$FPCVersion" = "xdevel" ]; then
  Year=02
  Month=12
  Day=25
  LazVersion=1.1
fi
if [ "x$FPCVersion" = "xstable" ]; then
  Year=03
  Month=03
  Day=01
  LazVersion=1.0.7
fi


Date=20$Year$Month$Day
LazRelease=laz.$Date
SrcTGZ=fpcsrc-$LazVersion-$LazRelease.tgz
TmpDir=/tmp/fpc$LazVersion
DebianRulezDir=$TmpDir/fpc/install/debian/

# download fpc cvs if necessary
if [ ! -f $SrcTGZ ]; then
  ./create_fpc_export_tgz.sh $SrcTGZ $FPCVersion $Month/$Day/$Year
fi

# unpack source into temporary directory
rm -rf $TmpDir
mkdir -p $TmpDir
cp $SrcTGZ $TmpDir/
cd $TmpDir
tar xzf $SrcTGZ
cd -

# change debian files

# prepend changelog information
cd $DebianRulezDir
File=changelog
OldFile=changelog.old.fpc
cp $File $OldFile
echo "fpc ($LazVersion-$LazRelease) unstable; urgency=low" > $File
echo '  * Unoffical snapshot build for lazarus' >> $File
echo ' -- Mattias Gaertner <mattias@freepascal.org>  Mon,  31 Mar 2003 11:01:50 +0100' >> $File
echo "" >> $File
cat $OldFile >> $File 
rm $OldFile
cd -


# compile
cd $TmpDir/fpc
make rtl
make compiler
make deb
cd -


echo
#echo building fpcsrc deb ...


# end.


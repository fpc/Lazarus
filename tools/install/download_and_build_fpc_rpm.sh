#!/bin/bash
#
# Author: Mattias Gaertner
#
# Script to download fpc and create the rpms 'fpc' and 'fpcsrc'.

set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 devel|stable [nodocs]"

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

shift

WithDOCS=yes
if [ "x$1" = "xnodocs" ]; then
  WithDOCS=no
  shift
fi

if [ "x$1" != "x" ]; then
  echo $Usage
  exit -1
fi


# set here the fpc cvs dates for the various versions
if [ "x$FPCVersion" = "xdevel" ]; then
  Year=04
  Month=06
  Day=07
  LazVersion=1.9.5
fi
if [ "x$FPCVersion" = "xstable" ]; then
  Year=04
  Month=06
  Day=07
  LazVersion=1.0.11
fi

Date=20$Year$Month$Day
LazRelease=laz.$Date
SrcTGZ=fpcsrc-$LazVersion-$LazRelease.tgz
SrcPatch=fpcsrc-patch
TmpDir=/tmp/fpc$LazVersion
SpecFile=$TmpDir/fpc/install/fpc.spec

# download fpc cvs if necessary
if [ ! -f $SrcTGZ ]; then
  ./create_fpc_export_tgz.sh $SrcTGZ $FPCVersion $Month/$Day/$Year
fi

# unpack source into temporary directory
rm -rf $TmpDir
mkdir -p $TmpDir
cp $SrcTGZ $SrcPatch $TmpDir/
cd $TmpDir
tar xzf $SrcTGZ
cd -

# compile
Params="notemp $TmpDir/fpc $LazRelease"
if [ "$WithDOCS" = "no" ]; then
  Params="nodocs $Params"
fi
./build_fpc_rpm.sh $Params



echo
echo building fpcsrc rpm ...
set -x

# copy src tgz into building directory
cp $SrcTGZ /usr/src/redhat/SOURCES/

# create spec file
SpecFile=fpcsrc-$LazVersion-$LazRelease.spec
cat fpcsrc.spec | \
  sed -e "s/LAZVERSION/$LazVersion/g" -e "s/LAZRELEASE/$LazRelease/" \
  > $SpecFile

# build rpm
rpmbuild -ba $SpecFile || rpm -ba $SpecFile

# end.


#!/bin/bash

set -x
set -e


#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 <FPCSrcDir> <release>"

FPCSourceDir=$1
shift
if [ "x$FPCSourceDir" = "x" ]; then
  echo $Usage
  exit -1
fi

LazRelease=$1
shift
if [ "x$LazRelease" = "x" ]; then
  echo $Usage
  exit -1
fi


#------------------------------------------------------------------------------
# patching
#------------------------------------------------------------------------------

# retrieve the version information
VersionFile="$FPCSourceDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
LazVersion="$CompilerVersion.$CompilerRelease"
if [ "$CompilerPatch" != "0" ]; then
  LazVersion="$LazVersion.$CompilerPatch"
fi

FPCTGZ=fpcsrc-$LazVersion-$LazRelease.tgz

# pack the directory
./create_fpc_tgz_from_local_dir.sh $FPCSourceDir $FPCTGZ

# copy src tgz into rpm building directory
cp $FPCTGZ /usr/src/redhat/SOURCES/

# create spec file
SpecFile=fpcsrc-$LazVersion-$LazRelease.spec
cat fpcsrc.spec | \
  sed -e "s/LAZVERSION/$LazVersion/g" -e "s/LAZRELEASE/$LazRelease/" \
  > $SpecFile

# build rpm
rpmbuild -ba $SpecFile || rpm -ba $SpecFile

# end.


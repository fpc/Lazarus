#!/usr/bin/env bash

set -x
set -e

TmpDir=~/tmp

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 [nodocs] [deb] <FPCSrcDir> [Starting Compiler]"

WithDOCS=yes
if [ "x$1" = "xnodocs" ]; then
  WithDOCS=no
  shift
fi

PkgType=rpm
if [ "x$1" = "xdeb" ]; then
  PkgType=deb
  shift
fi

FPCSrcDir=$1
shift
if [ "x$FPCSrcDir" = "x" ]; then
  echo $Usage
  exit -1
fi

STARTPP=$1
if [ -n "$STARTPP" ]; then
  export STARTPP
fi

LazRelease=`date +%Y%m%d`

if [ ! -d $FPCSrcDir/compiler ]; then
  echo "The directory $FPCSrcDir does not look like a fpc source directory (fpc)"
  exit -1
fi

if [ ! -d $FPCSrcDir/.svn ]; then
  echo "The directory $FPCSrcDir does not look like a svn working directory"
  exit -1
fi

RPMDIR=$(rpm/get_rpm_source_dir.sh)
ARCH=`rpm --eval "%{_arch}"`

#------------------------------------------------------------------------------
# patching
#------------------------------------------------------------------------------

# create a temporary copy of the fpc sources to patch it
TmpFPCDir=$TmpDir/fpc
rm -rf $TmpFPCDir
mkdir -p $TmpDir

echo "extracting FPC from local svn ..."
svn export $FPCSrcDir $TmpFPCDir

# retrieve the version information
VersionFile="$TmpFPCDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerMinorPatch=`cat $VersionFile | grep ' *minorpatch *=.*;' | sed -e 's/.*minorpatch.*= *//g' -e "s/'//g" -e 's/;//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
LazVersion="$CompilerVersion.$CompilerRelease.$CompilerPatch$CompilerMinorPatch"

# set version numbers in all Makefiles
perl replace_in_files.pl -sR -f '=\d.\d.\d' -r =$LazVersion -m 'Makefile(.fpc)?' $TmpFPCDir/*

# create a source tar.gz
cd $TmpDir
tar -czf $RPMDIR/SOURCES/fpc-$LazVersion-$LazRelease.source.tar.gz fpc

# remove the tempdir
cd -
rm -rf $TmpFPCDir

SpecFileTemplate=rpm/fpc.spec.template
SpecFile=$RPMDIR/SPECS/fpc.spec

# change spec file
cat $SpecFileTemplate | \
  sed -e 's/^Version: .*/Version: '"$LazVersion/" \
      -e 's/^Release: .*/Release: '"$LazRelease/" \
      -e 's/^%define fpcversion .*/%define fpcversion '"$CompilerVersionStr/" \
  > $SpecFile
#      -e 's/\(%define builddocdir.*\)/%define __strip smart_strip.sh\n\n\1/' \
#      -e 's/^\%{fpcdir}\/samplecfg .*/%{fpcdir}\/samplecfg %{_libdir}\/fpc\/\\\$version/' \

rpmbuild --target $ARCH -ba $SpecFile --nodeps

export FpcFullVersion=$LazVersion

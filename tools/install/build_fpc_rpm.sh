#!/bin/bash

set -x
set -e


#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 [nodocs] [notemp] <FPCSrcDir> <release>"

WithDOCS=yes
if [ "x$1" = "xnodocs" ]; then
  WithDOCS=no
  shift
fi

WithTempDir=yes
if [ "x$1" = "xnotemp" ]; then
  WithTempDir=no
  shift
fi

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

# create a temporary copy of the fpc sources to patch it
TmpDir=/tmp/fpc_patchdir
if [ "$WithTempDir" = "yes" ]; then
  rm -rf $TmpDir
  cp -a $FPCSourceDir $TmpDir
else
  TmpDir=$FPCSourceDir
fi

# retrieve the version information
VersionFile="$TmpDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
LazVersion="$CompilerVersion.$CompilerRelease"
if [ "$CompilerPatch" != "0" ]; then
  LazVersion="$LazVersion.$CompilerPatch"
fi


SpecFile=$TmpDir/install/fpc.spec
SrcPatch=fpcsrc-patch

# patch sources
patch -p2 -d $TmpDir/ < $SrcPatch

# change spec file
cat $SpecFile | \
  sed -e 's/^Version: .*/Version: '"$LazVersion/" \
      -e 's/^Release: .*/Release: '"$LazRelease/" \
  > $SpecFile.New
#      -e 's/^\%{fpcdir}\/samplecfg .*/%{fpcdir}\/samplecfg %{_libdir}\/fpc\/\\\$version/' \
mv $SpecFile.New $SpecFile
if [ "$WithDOCS" = "no" ]; then
  cat $SpecFile | \
    sed -e 's/^\(.*\bmake\b.*\bdocs\b\)/#\1/g' \
        -e 's/^\(%doc.*\*\.pdf\)/#\1/g' \
  > $SpecFile.New
  mv $SpecFile.New $SpecFile
fi

# change Makefile for new rpmbuild, if not already done
cd $TmpDir
if [ -n `grep rpmbuild Makefile` ]; then
  cat Makefile | \
    sed -e 's/rpm\( --nodeps -ba .*\)$/rpm\1 || rpmbuild\1/g' \
    > New.Makefile
  mv New.Makefile Makefile
fi
cd -

#------------------------------------------------------------------------------
# compile
#------------------------------------------------------------------------------
cd $TmpDir
#make rtl
#make compiler
if [ "$WithDOCS" = "no" ]; then
  make rpm NODOCS=1
else
  make rpm
fi
cd -

# end.


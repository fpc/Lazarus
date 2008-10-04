#!/bin/bash

#set -x
set -e


#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 <FPCSrcDir> [release]"

FPCSourceDir=$1
if [ "x$FPCSourceDir" = "x" ]; then
  echo $Usage
  exit -1
fi
shift

LazRelease=$1
if [ "x$LazRelease" = "x" ]; then
  LazRelease=$(date +%y%m%d)
else
  shift
fi

Arch=$(rpm --eval "%{_arch}")

# retrieve the version information
echo -n "getting FPC version from local svn ..."
VersionFile="$FPCSourceDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
LazVersion=$CompilerVersionStr
echo " $CompilerVersionStr-$LazRelease"

FPCTGZ=$(rpm/get_rpm_source_dir.sh)/SOURCES/fpc-src-$CompilerVersionStr-$LazRelease.source.tar.gz
CurDir=`pwd`

# pack the directory
sh create_fpc_tgz_from_local_dir.sh $FPCSourceDir $FPCTGZ

# build fpc-src rpm

echo "building fpc-src rpm ..."

# copy custom rpm scripts
TmpDir=$HOME/tmp
mkdir -p $TmpDir
cp smart_strip.sh $TmpDir/smart_strip.sh
chmod a+x $TmpDir/smart_strip.sh
cp do_nothing.sh $TmpDir/do_nothing.sh
chmod a+x $TmpDir/do_nothing.sh

# create spec file
SpecFile=rpm/fpc-src-$LazVersion-$LazRelease.spec
cat rpm/fpc-src.spec | \
  sed -e "s/LAZVERSION/$LazVersion/g" -e "s/LAZRELEASE/$LazRelease/g" -e "s#LAZSCRIPTDIR#$TmpDir#g" \
  > $SpecFile
  
# build rpm
rpmbuild -ba $SpecFile || rpm -ba $SpecFile

echo "The new rpm can be found in $(./rpm/get_rpm_source_dir.sh)/RPMS/$Arch/fpc-src-$LazVersion-$LazRelease.$Arch.rpm"

# end.


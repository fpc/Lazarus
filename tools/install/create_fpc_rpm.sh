#!/bin/bash

#set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 [nodocs] [notemp] [hasbuildid] <FPCSrcDir> [release]"

PkgName=fpc-laz

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

HasBuildID=no
if [ "x$1" = "xhasbuildid" ]; then
  HasBuildID=yes
  shift
fi


PkgType=rpm

FPCSrcDir=$1
if [ "x$FPCSrcDir" = "x" ]; then
  echo $Usage
  exit -1
fi
FPCSrcDir=$(echo $FPCSrcDir)
shift

FPCRelease=$1
if [ "x$FPCRelease" = "x" ]; then
  FPCRelease=$(date +%y%m%d)
else
  shift
fi

if [ ! -d $FPCSrcDir/compiler ]; then
  echo "The directory $FPCSrcDir does not look like a fpc source directory (missing subdirectory compiler)"
  exit -1
fi

# checking for needed tools
rpmbuild --version

#------------------------------------------------------------------------------
# patching
#------------------------------------------------------------------------------

# create a temporary copy of the fpc sources to patch it
TmpDir=~/tmp/fpc_patchdir
if [ "$WithTempDir" = "yes" ]; then
  if [ -d $TmpDir ]; then
    rm -rf $TmpDir
  fi
  echo "extracting FPC from local git ..."
  mkdir -p $TmpDir/$PkgName
  cp -a $FPCSrcDir/.git $TmpDir/$PkgName/
  git -C $TmpDir/$PkgName checkout $TmpDir/$PkgName
  rm -rf $TmpDir/$PkgName/.git*
else
  TmpDir=$FPCSrcDir
fi

# retrieve the version information
echo -n "getting FPC version from local git ..."
VersionFile="$TmpDir/$PkgName/compiler/version.pas"
CompilerVersion=$(cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerRelease=$(cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerPatch=$(cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
FPCVersion="$CompilerVersion.$CompilerRelease.$CompilerPatch"
echo " $CompilerVersionStr-$FPCRelease"

Arch=$(rpm --eval "%{_target_cpu}")

#------------------------------------------------------------------------------
# patch sources

SmartStripScript=smart_strip.sh
ReplaceScript=replace_in_files.pl


# set version numbers in all Makefiles
echo "set version numbers in all Makefiles ..."
perl replace_in_files.pl -sR -f 'version=\d.\d.\d' -r version=$CompilerVersionStr -m 'Makefile(.fpc)?' $TmpDir/$PkgName/*

# update smart_strip.sh
#ATM: not needed: cp $SmartStripScript $TmpDir/$PkgName/install/

# build fpc-laz rpm

echo "creating spec file ..."
SpecFileTemplate=rpm/$PkgName.spec.template
SpecFile=rpm/$PkgName.spec

# change spec file
cat $SpecFileTemplate | \
    sed -e 's/^Version: .*/Version: '"$FPCVersion/" \
        -e 's/^Release: .*/Release: '"$FPCRelease/" \
        -e 's/^%define fpcversion .*/%define fpcversion '"$FPCVersion/" \
        -e 's/^\%{fpcdir}\/samplecfg .*/%{fpcdir}\/samplecfg %{_libdir}\/fpc\/\\\$version/' \
    > $SpecFile
#      -e 's/\(%define builddocdir.*\)/%define __strip smart_strip.sh\n\n\1/' \
if [ "$HasBuildID" = "yes" ]; then
  sed -e 's/%undefine _missing_build-ids_terminate_build//' -i $SpecFile
fi


SrcTGZ=$(rpm/get_rpm_source_dir.sh)/SOURCES/fpc-$CompilerVersionStr-$FPCRelease.source.tar.gz
echo "creating $SrcTGZ ..."
tar czf $SrcTGZ -C $TmpDir $PkgName

#----------------------------------------------------------------------------
# compile
#----------------------------------------------------------------------------
if [ "$WithDOCS" = "no" ]; then
  export NODOCS=1
fi
rpmbuild --nodeps -ba $SpecFile

echo "The new rpm can be found in $(./rpm/get_rpm_source_dir.sh)/RPMS/$Arch/$PkgName-$FPCVersion-$FPCRelease.$Arch.rpm"

# end.


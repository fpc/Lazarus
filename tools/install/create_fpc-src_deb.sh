#!/bin/bash

set -x
set -e

# ToDo: check libgpm
# ToDo: check if ppcxxx is installed

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 [nodocs] [notemp] <FPCSrcDir> [release]"

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
  echo "The directory $FPCSrcDir does not look like a fpc source directory (fpc/)"
  exit -1
fi


#------------------------------------------------------------------------------
# quick tests
./check_fpc_dependencies.sh

CurDir=`pwd`

#------------------------------------------------------------------------------
# patching
#------------------------------------------------------------------------------

# retrieve the version information
echo -n "getting FPC version from local svn ..."
VersionFile="$FPCSrcDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
FPCVersion="$CompilerVersion.$CompilerRelease"
if [ "$CompilerPatch" != "0" ]; then
  FPCVersion="$FPCVersion.$CompilerPatch"
fi
echo " $CompilerVersionStr-$FPCRelease"

SrcTGZ=$(pwd)/fpc-$FPCVersion-$FPCRelease.tar.gz

# download/export fpc svn if needed
if [ ! -f $SrcTGZ ]; then
  ./create_fpc_export_tgz.sh $FPCSrcDir $SrcTGZ
fi

# create a temporary copy of the fpc sources to patch it
TmpDir=/tmp/fpc_patchdir
if [ "$WithTempDir" = "yes" ]; then
  if [ -d $TmpDir ]; then
    rm -rf $TmpDir/*
    rm -r $TmpDir
  fi
  mkdir -p $TmpDir

  cd $TmpDir
  echo "unpacking $SrcTGZ to "$(pwd)" ..."
  tar xzf $SrcTGZ
  cd -
  FPCSrcDir=$TmpDir/fpc
else
  TmpDir=$FPCSrcDir
fi

#------------------------------------------------------------------------------
# patch sources

ReplaceScript=replace_in_files.pl

# set version numbers in all Makefiles
echo "set version numbers in all Makefiles ..."
perl replace_in_files.pl -sR -f '=\d.\d.\d' -r =$CompilerVersionStr -m 'Makefile(.fpc)?' $FPCSrcDir/*

#------------------------------------------------------------------------------
#

PackageName=fpc-src
FPCBuildDir=$TmpDir/fpc_build
FPCDeb=$CurDir/$PackageName-$FPCVersion-$FPCRelease.deb
ResourceDir=$CurDir/debian_$PackageName
DebianLibDir=$FPCBuildDir/usr/lib/fpc 
DebianRulezDir=$FPCBuildDir/DEBIAN/
DebianDocDir=$FPCBuildDir/usr/share/doc/$PackageName
DebianSourceDir=$FPCBuildDir/usr/share/fpcsrc
Date=`date --rfc-822`


#------------------------------------------------------------------------------
# copy fpc sources
mkdir -p $DebianSourceDir
cp -a $FPCSrcDir/* $DebianSourceDir/

#------------------------------------------------------------------------------
# build fpc
cd $TmpDir
# TODO make all
cd -

#------------------------------------------------------------------------------
# build fpc-src deb

# change debian files
mkdir -p $DebianDocDir
mkdir -p $DebianRulezDir

# create debian control file, which contains the package description
echo "creating DEBIAN/control file"
cat $ResourceDir/control | sed -e "s/FPCVERSION/$FPCVersion/g" > $DebianRulezDir/control

# create debian changelog file, needed for version
echo "creating usr/share/doc/fpc/changelog file ..."
File=$DebianDocDir/changelog
echo "fpc ($FPCVersion-$FPCRelease) unstable; urgency=low" > $File
echo '  * Unofficial snapshot build for lazarus' >> $File
echo " -- Mattias Gaertner <mattias@freepascal.org>  $Date" >> $File
echo "" >> $File
cat $ResourceDir/changelog >> $File
gzip --best $File

# create changelog.Debian file
echo "creating changelog.Debian file ..."
File=$DebianDocDir/changelog.Debian
cp $ResourceDir/changelog.Debian $File
gzip --best $File

# create debian copyright file
echo "creating copyright file ..."
cp $ResourceDir/copyright $DebianDocDir/

# creating deb
cd $TmpDir
fakeroot dpkg-deb --build $FPCBuildDir
mv $FPCBuildDir.deb $FPCDeb

# end.


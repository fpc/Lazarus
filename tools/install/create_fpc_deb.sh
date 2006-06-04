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
  TmpDir=$TmpDir/fpc
else
  TmpDir=$FPCSrcDir
fi

#------------------------------------------------------------------------------
# patch sources

ReplaceScript=replace_in_files.pl


# set version numbers in all Makefiles
echo "set version numbers in all Makefiles ..."
perl replace_in_files.pl -sR -f '=\d.\d.\d' -r =$CompilerVersionStr -m 'Makefile(.fpc)?' $TmpDir/fpc/*

#------------------------------------------------------------------------------
#

FPCBuildDir=$TmpDir/fpc_build
FPCDeb=$CurDir/fpc-$FPCVersion-$FPCRelease.deb
DebianSrcDir=$CurDir/debian_fpc
FPCDestDir=$FPCBuildDir/usr/lib/fpc 

#------------------------------------------------------------------------------
# build fpc
cd $TmpDir
# TODO make all
cd -

#------------------------------------------------------------------------------
# build fpc debs

# change debian files
DebianRulezDir=$FPCBuildDir/DEBIAN/
$DebianDocDir=$FPCBuildDir/usr/share/doc/fpc
mkdir -p $DebianRulezDir
Date=`date --rfc-822`

# create debian control file, which contains the package description
echo "creating DEBIAN/control file"
cat $DebianSrcDir/control | sed -e 's/FPCVERSION/$FPCVersion/g' > $DebianRulezDir/control

# create debian changelog file, needed for version
echo "creating usr/share/doc/fpc/changelog file ..."
File=$DebianDocDir/changelog
echo "fpc ($FPCVersion-$FPCRelease) unstable; urgency=low" > $File
echo '  * Unofficial snapshot build for lazarus' >> $File
echo " -- Mattias Gaertner <mattias@freepascal.org>  $Date" >> $File
echo "" >> $File
cat $DebianSrcDir/changelog >> $File
gzip --best $File

# create debian copyright file
echo "creating copyright file ..."
cp $DebianSrcDir/copyright $DebianDocDir/

# create debian/rules file
# - copy the complete examples directory
# - do not install non existing files Changes.fcl Changes.utils
cat $DebianSrcDir/rules | \
  sed -e 's/^\(.*mv .*\)uncgi\( .*examples.*\)$/\1???*\2/' \
      -e 's/^.*logs\/Changes\.fcl.*$//' \
      -e 's/^.*logs\/Changes\.utils.*$//' \
  > rules.laz
cp rules.laz rules # use cp to preserve file attribs
rm rules.laz
cd -


# compile
cd $TmpDir/fpc
make debcopy
cd -
cd /usr/src/fpc-$FPCVersion
./debian/rules binary-arch
cd -


exit

OLD STUFF


# get date of day
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`

Date=20$Year$Month$Day
CurDir=`pwd`
FPCRelease=laz.$Date
SrcTGZ=fpc-src-$FPCVersion-$FPCRelease.tgz
TmpDir=/tmp/fpc$FPCVersion
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

# prepend changelog information, needed for version
cd $DebianRulezDir
File=changelog
OldFile=changelog.old.fpc
cp $File $OldFile
echo "fpc ($FPCVersion-$FPCRelease) unstable; urgency=low" > $File
echo '  * Unofficial snapshot build for lazarus' >> $File
echo ' -- Mattias Gaertner <mattias@freepascal.org>  Mon,  31 Mar 2003 11:01:50 +0100' >> $File
echo "" >> $File
cat $OldFile >> $File 
rm $OldFile
cd -

# fix debian/rules
# - copy the complete examples directory
# - do not install non existing files Changes.fcl Changes.utils
cd $DebianRulezDir
cat rules | \
  sed -e's/^\(.*mv .*\)uncgi\( .*examples.*\)$/\1???*\2/' \
      -e 's/^.*logs\/Changes\.fcl.*$//' \
      -e 's/^.*logs\/Changes\.utils.*$//' \
  > rules.tmp
cp rules.tmp rules
rm rules.tmp 
cd - 


# compile
cd $TmpDir/fpc
make debcopy
cd -
cd /usr/src/fpc-$FPCVersion
./debian/rules binary-arch
cd -

echo ===================================================
echo
echo building fpc-src deb ...

FPCSrcTmpDir=/tmp/fpc-src$FPCVersion
FPCSrcBuildDir=$FPCSrcTmpDir/fpc-src_build
FPCSrcDeb=fpc-src-$FPCVersion-$FPCRelease.deb
DebianSrcDir=$CurDir/debian_fpc-src

echo "Build directory is $FPCSrcBuildDir"
if [ x$FPCSrcBuildDir = x/ ]; then
  echo "ERROR: invalid build directory"
  exit
fi
rm -rf $FPCSrcBuildDir

# Unpack fpc source
echo "unpacking $SrcTGZ ..."
mkdir -p $FPCSrcBuildDir/usr/share/
cd $FPCSrcBuildDir/usr/share/
tar xzf $CurDir/$SrcTGZ
find . -name '.cvsignore' -exec rm {} \;
mv fpc fpcsrc
cd -

# create control file
echo "copying control file"
mkdir -p $FPCSrcBuildDir/DEBIAN
cat $DebianSrcDir/control | \
  sed -e "s/FPCVERSION/$FPCVersion-$FPCRelease/g" \
  > $FPCSrcBuildDir/DEBIAN/control

# copyright and changelog files
echo "copying copyright and changelog files"
mkdir -p $FPCSrcBuildDir/usr/share/doc/fpc-src
cat $DebianSrcDir/changelog | \
  sed -e "s/FPCVERSION/$FPCVersion-$FPCRelease/g" \
      -e "s/FPCDATE/$Year-$Month-$Day/g" \
  > $FPCSrcBuildDir/usr/share/doc/fpc-src/changelog
cp $DebianSrcDir/{copyright,changelog.Debian} $FPCSrcBuildDir/usr/share/doc/fpc-src/
gzip --best $FPCSrcBuildDir/usr/share/doc/fpc-src/changelog
gzip --best $FPCSrcBuildDir/usr/share/doc/fpc-src/changelog.Debian

# fixing permissions
echo "fixing permissions ..."
find $FPCSrcBuildDir -type d | xargs chmod 755  # this is necessary on Debian Woody, don't ask me why

# creating deb
echo "creating deb ..."
cd $TempDir
fakeroot dpkg-deb --build $FPCSrcBuildDir
mv $FPCSrcBuildDir.deb $FPCSrcDeb
echo "`pwd`/$FPCSrcDeb created."
cd -


# end.


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

CurDir=`pwd`
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

# prepend changelog information, needed for version
cd $DebianRulezDir
File=changelog
OldFile=changelog.old.fpc
cp $File $OldFile
echo "fpc ($LazVersion-$LazRelease) unstable; urgency=low" > $File
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
  > rules.laz
cp rules.laz rules
rm rules.laz 
cd - 


# compile
cd $TmpDir/fpc
make debcopy
cd -
cd /usr/src/fpc-$LazVersion
./debian/rules binary-arch
cd -

echo ===================================================
echo
echo building fpcsrc deb ...

FPCSrcTmpDir=/tmp/fpcsrc$LazVersion
FPCSrcBuildDir=$FPCSrcTmpDir/fpcsrc_build
FPCSrcDeb=fpcsrc-$LazVersion-$LazRelease.deb
DebianSrcDir=$CurDir/debian_fpcsrc

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
  sed -e "s/FPCVERSION/$LazVersion-$LazRelease/g" \
  > $FPCSrcBuildDir/DEBIAN/control

# copyright and changelog files
echo "copying copyright and changelog files"
mkdir -p $FPCSrcBuildDir/usr/share/doc/fpcsrc
cat $DebianSrcDir/changelog | \
  sed -e "s/FPCVERSION/$LazVersion-$LazRelease/g" \
      -e "s/FPCDATE/$Year-$Month-$Day/g" \
  > $FPCSrcBuildDir/usr/share/doc/fpcsrc/changelog
cp $DebianSrcDir/{copyright,changelog.Debian} $FPCSrcBuildDir/usr/share/doc/fpcsrc/
gzip --best $FPCSrcBuildDir/usr/share/doc/fpcsrc/changelog
gzip --best $FPCSrcBuildDir/usr/share/doc/fpcsrc/changelog.Debian

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


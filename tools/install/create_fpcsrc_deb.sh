#!/bin/bash
#set -x
set -e

CurDir=`pwd`
TempDir=/tmp/fpcsrc_temp
FPCSrcBuildDir=$TempDir/fpcsrc_build
FPCSrcTGZ=$CurDir/fpcsrc-1.0.7.tgz
FPCSrcDeb=fpcsrc-1.0.7-1.deb
DebianSrcDir=$CurDir/debian_fpcsrc

echo "Build directory is $FPCSrcBuildDir"
if [ x$FPCSrcBuildDir = x/ ]; then
  echo "ERROR: invalid build directory"
  exit
fi
rm -rf $FPCSrcBuildDir

# Unpack fpc source
echo "unpacking $FPCSrcTGZ ..."
mkdir -p $FPCSrcBuildDir/usr/share/
cd $FPCSrcBuildDir/usr/share/
tar xzf $FPCSrcTGZ
find . -name '.cvsignore' -exec rm {} \;
mv fpc fpcsrc
cd -

# create control file
echo "copying control file"
mkdir -p $FPCSrcBuildDir/DEBIAN
cp $DebianSrcDir/control $FPCSrcBuildDir/DEBIAN/

# copyright and changelog files
echo "copying copyright and changelog files"
mkdir -p $FPCSrcBuildDir/usr/share/doc/fpcsrc
cp $DebianSrcDir/{copyright,changelog,changelog.Debian} $FPCSrcBuildDir/usr/share/doc/fpcsrc/
gzip --best $FPCSrcBuildDir/usr/share/doc/fpcsrc/changelog
gzip --best $FPCSrcBuildDir/usr/share/doc/fpcsrc/changelog.Debian

# fixing permissions
echo "fixing permissions ..."
find $FPCSrcBuildDir -type d | xargs chmod 755   # this is necessary on Debian Woody, don't ask me why

# creating deb
echo "creating deb ..."
cd $TempDir
fakeroot dpkg-deb --build $FPCSrcBuildDir
mv $FPCSrcBuildDir.deb $FPCSrcDeb
echo "`pwd`/$FPCSrcDeb created."
cd -

# end.

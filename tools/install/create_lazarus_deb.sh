#!/bin/bash

set -x
set -e

# get installed fpc version
FPCDeb=`dpkg -l | grep fp-compiler`
if [ "x$FPCDeb" = "x" ]; then
  echo ERROR: fp-compiler deb not installed
  exit
fi

# get date of day
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`

Date=20$Year$Month$Day
LazVersion=0.9.8
LazRelease=`dpkg -s fp-compiler | grep '^Version' | sed -e 's/Version: //'`
LazRelease=`echo $LazRelease | sed -e 's/-/_/g'`
SrcTGZ=lazarus-$Date.tgz
CurDir=`pwd`
TmpDir=/tmp/lazarus$LazVersion
LazBuildDir=$TmpDir/lazarus_build
LazDeb=$CurDir/lazarus-$LazVersion-fpc_$LazRelease.deb
DebianSrcDir=$CurDir/debian_lazarus
LazDestDir=$LazBuildDir/usr/share/lazarus
FPCVersion=`echo $LazRelease | sed -e 's/_.*//'`
ChangeLogDate=`date --rfc-822`

# download lazarus cvs if necessary
if [ ! -f $SrcTGZ ]; then
  ./create_lazarus_export_tgz.sh $SrcTGZ
fi

echo "Build directory is $LazBuildDir"
if [ x$LazBuildDir = x/ ]; then
  echo "ERROR: invalid build directory"
  exit
fi
rm -rf $LazBuildDir

# Unpack lazarus source
echo "unpacking $SrcTGZ ..."
mkdir -p $LazBuildDir/usr/share/
cd $LazBuildDir/usr/share/
tar xzf $CurDir/$SrcTGZ
find . -name '.cvsignore' -exec rm {} \;
cd -

# compile
cd $LazDestDir
make
strip lazarus
cd -

# create control file
echo "copying control file"
mkdir -p $LazBuildDir/DEBIAN
cat $DebianSrcDir/control | \
  sed -e "s/FPCVERSION/$FPCVersion/g" \
      -e "s/LAZVERSION/$LazVersion/g" \
  > $LazBuildDir/DEBIAN/control

# copyright and changelog files
echo "copying copyright and changelog files"
mkdir -p $LazBuildDir/usr/share/doc/lazarus
cp $DebianSrcDir/{copyright,changelog,changelog.Debian} $LazBuildDir/usr/share/doc/lazarus/
gzip --best $LazBuildDir/usr/share/doc/lazarus/changelog
gzip --best $LazBuildDir/usr/share/doc/lazarus/changelog.Debian

# icons, links
mkdir -p $LazBuildDir/usr/share/pixmaps/
mkdir -p $LazBuildDir/usr/share/gnome/apps/Development/
mkdir -p $LazBuildDir/usr/bin/
install -m 644 $LazDestDir/images/ide_icon48x48.png $LazBuildDir/usr/share/pixmaps/lazarus.png
install -m 644 $LazDestDir/gnome.ide.desktop $LazBuildDir/usr/share/gnome/apps/Development/lazarus.desktop
ln -s $LazDestDir/lazarus $LazBuildDir/usr/bin/lazarus

# fixing permissions
echo "fixing permissions ..."
find $LazBuildDir -type d | xargs chmod 755  # this is necessary on Debian Woody, don't ask me why
find $LazBuildDir -name '*.sh' -exec chmod a+x {} \;

# creating deb
echo "creating deb ..."
cd $TmpDir
fakeroot dpkg-deb --build $LazBuildDir
mv $LazBuildDir.deb $LazDeb
echo "$LazDeb created."
cd -

# end.


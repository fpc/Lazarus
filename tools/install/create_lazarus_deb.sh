#!/bin/bash
#set -x
set -e

CurDir=`pwd`
TempDir=/tmp/lazarus_temp
LazBuildDir=$TempDir/lazarus_build
LazTGZ=$CurDir/lazarus-0.8.5.tgz
LazDeb=lazarus-0.8.5-1.deb
DebianSrcDir=$CurDir/debian_lazarus
LazDestDir=$LazBuildDir/usr/share/lazarus

echo "Build directory is $LazBuildDir"
if [ x$LazBuildDir = x/ ]; then
  echo "ERROR: invalid build directory"
  exit
fi
rm -rf $LazBuildDir

# Unpack lazarus source
echo "unpacking $LazTGZ ..."
mkdir -p $LazBuildDir/usr/share/
cd $LazBuildDir/usr/share/
tar xzf $LazTGZ
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
cp $DebianSrcDir/control $LazBuildDir/DEBIAN/

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
find $LazBuildDir -type d | xargs chmod 755   # this is necessary on Debian Woody, don't ask me why
find $LazBuildDir -name '*.sh' -exec chmod a+x {} \;

# creating deb
echo "creating deb ..."
cd $TempDir
fakeroot dpkg-deb --build $LazBuildDir
mv $LazBuildDir.deb $LazDeb
echo "`pwd`/$LazDeb created."
cd -

# end.

#!/bin/bash

set -x
set -e

# get FPC version
FPCVersion=$(fpc -v | grep 'Compiler version' | sed 's/.*\([0-9]\.[0-9]\.[0-9]\).*/\1/')
Arch=$(fpc -v | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')

# get date of day
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`

Date=20$Year$Month$Day
LazVersion=$(./get_lazarus_version.sh)
LazRelease='0'
SrcTGZ=lazarus-$LazVersion-$LazRelease.tar.gz
CurDir=`pwd`
TmpDir=/tmp/lazarus$LazVersion
LazBuildDir=$TmpDir/lazarus_build
LazDeb=$CurDir/lazarus_${LazVersion}-${LazRelease}_$Arch.deb
DebianSrcDir=$CurDir/debian_lazarus
LazDestDir=$LazBuildDir/usr/share/lazarus
LazDestDirInstalled=/usr/share/lazarus
FPCVersion=$(ppc386 -v | grep version| sed 's/.*\([0-9]\+\.[0-9]\+\.[0-9]\+\).*/\1/')
ChangeLogDate=`date --rfc-822`

# download/export lazarus svn if needed
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
cd -

# compile
echo "Compiling may take a while ... =========================================="
cd $LazDestDir
MAKEOPTS="-Fl/opt/gnome/lib"
if [ -n "$FPCCfg" ]; then
  MAKEOPTS="$MAKEOPTS -n @$FPCCfg"
fi
make bigide OPT="$MAKEOPTS" USESVN2REVISIONINC=0
make lazbuilder OPT="$MAKEOPTS"
make tools OPT="$MAKEOPTS"
# build gtk2 .ppu
export LCL_PLATFORM=gtk2
make lcl ideintf packager/registration bigidecomponents OPT="$MAKEOPTS"
export LCL_PLATFORM=
strip lazarus
strip startlazarus
strip lazbuild
cd -

# create control file
echo "========================================================================="
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
mkdir -p $LazBuildDir/usr/share/applications
mkdir -p $LazBuildDir/usr/bin/
install -m 644 $LazDestDir/images/ide_icon48x48.png $LazBuildDir/usr/share/pixmaps/lazarus.png
install -m 644 $LazDestDir/install/lazarus.desktop $LazBuildDir/usr/share/applications/lazarus.desktop
ln -s $LazDestDirInstalled/lazarus $LazBuildDir/usr/bin/lazarus
ln -s $LazDestDirInstalled/startlazarus $LazBuildDir/usr/bin/startlazarus
ln -s $LazDestDirInstalled/lazbuild $LazBuildDir/usr/bin/lazbuild

# docs
mkdir -p $LazBuildDir/usr/share/man/man1
cat $LazDestDir/docs/lazbuild.1 | gzip > $LazBuildDir/usr/share/man/man1/lazbuild.1.gz

# fixing permissions
echo "fixing permissions ..."
find $LazBuildDir -type d | xargs chmod 755  # this is needed on Debian Woody, don't ask me why

# creating deb
echo "creating deb ..."
cd $TmpDir
fakeroot dpkg-deb --build $LazBuildDir
mv $LazBuildDir.deb $LazDeb
echo "the new deb can be found at $LazDeb"
cd -

# removing temporary files
rm -r $TmpDir

# end.


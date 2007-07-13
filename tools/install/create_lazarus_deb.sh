#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Usage: ./create_lazarus_deb.sh [gtk2] [release=svn]
#
#   Options:
#     gtk2              compile IDE and programs for gtk2. gtk1 ppu are built too.
#     append-revision   append the svn revision to the .deb version

set -x
set -e

LCLWidgetset=
if [ "$1" = "gtk2" ]; then
  LCLWidgetset=gtk2
  shift
fi

LazVersionPostfix=
if [ "$1" = "append-revision" ]; then
  LazVersionPostfix=.$(./get_lazarus_revision.sh | sed -e 's/[^0-9]//')
  shift
fi

if [ -n "$1" ]; then
  echo "Usage: ./create_lazarus_deb.sh [gtk2] [release=svn]"
  exit
fi

# get FPC version
FPCVersion=$(fpc -v | grep 'Compiler version' | sed 's/.*\([0-9]\.[0-9]\.[0-9]\).*/\1/')
Arch=$(fpc -v | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')

Date=`date +%Y%m%d`
LazVersion=$(./get_lazarus_version.sh)$LazVersionPostfix
LazRelease='0'
SrcTGZ=lazarus-$LazVersion-$LazRelease.tar.gz
CurDir=`pwd`
TmpDir=/tmp/lazarus$LazVersion
LazBuildDir=$TmpDir/lazarus_build
LazDeb=$CurDir/lazarus_${LazVersion}-${LazRelease}_$Arch.deb
DebianSrcDir=$CurDir/debian_lazarus
LazDestDir=$LazBuildDir/usr/share/lazarus
LazDestDirInstalled=/usr/share/lazarus
 
Arch=`dpkg --print-architecture` 
if [  "$Arch" = i386 ]; then 
  ppcbin=ppc386 
else 
  if [ "$Arch" = amd64 ]; then 
    ppcbin=ppcx64 
  else   
    if [  "$Arch" = powerpc ]; then 
      ppcbin=ppcppc 
    else 
      if [  "$Arch" = sparc ]; then 
        ppcbin=ppcsparc 
      else 
        echo "$Arch is not supported." 
        exit -1 
      fi 
    fi 
  fi 
fi 
 
FPCVersion=$($ppcbin -v | grep version| sed 's/.*\([0-9]\+\.[0-9]\+\.[0-9]\+\).*/\1/') 
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
# build for default platform
make lcl packager/registration ideintf bigidecomponents OPT="$MAKEOPTS"
# build gtk2 .ppu
export LCL_PLATFORM=gtk2
make lcl packager/registration ideintf bigidecomponents OPT="$MAKEOPTS"
export LCL_PLATFORM=
# build IDE
export LCL_PLATFORM=$LCLWidgetset
make bigide OPT="$MAKEOPTS" USESVN2REVISIONINC=0
make lazbuilder OPT="$MAKEOPTS"
make tools OPT="$MAKEOPTS"
export LCL_PLATFORM=

strip lazarus
strip startlazarus
strip lazbuild
strip tools/apiwizz/apiwizz
strip tools/lazres
strip tools/updatepofiles
cd -

# create control file
echo "========================================================================="
echo "copying control file"
mkdir -p $LazBuildDir/DEBIAN
cat $DebianSrcDir/control$LCLWidgetset | \
  sed -e "s/FPCVERSION/$FPCVersion/g" \
      -e "s/LAZVERSION/$LazVersion/g" \
      -e "s/ARCH/$Arch/g" \
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


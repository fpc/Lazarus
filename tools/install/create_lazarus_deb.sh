#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Usage: ./create_lazarus_deb.sh [gtk1] [append-revision] [chmhelp]
#
#   Options:
#     gtk1              compile IDE and programs for gtk1.
#     qt                compile IDE and programs for qt.
#     append-revision   append the svn revision to the .deb version
#     chmhelp           add package chmhelp and add chm,kwd files in docs/chm
#     pas2jszip <pas2js-linux-version.zip>
#                       unzip pas2js release zip to "pas2js/version"

set -e

LCLWidgetset=
LazVersionPostfix=
UseCHMHelp=
Pas2jsZip=
LazRelease='0'

while [ $# -gt 0 ]; do
  echo "param=$1"
  case "$1" in
  gtk1)
    echo "Compile for gtk1"
    if [ -n "$LCLWidgetset" ]; then
      echo "widgetset already set to $LCLWidgetset"
      exit -1
    fi
    LCLWidgetset=gtk
    LazVersionPostfix=${LazVersionPostfix}-gtk
    ;;

  qt)
    echo "Compile for qt"
    if [ -n "$LCLWidgetset" ]; then
      echo "widgetset already set to $LCLWidgetset"
      exit -1
    fi
    LCLWidgetset=qt
    LazVersionPostfix=${LazVersionPostfix}-qt
    ;;

  append-revision)
    LazRevision=$(./get_svn_revision_number.sh .)
    if [ -n "$LazRevision" ]; then
      LazVersionPostfix=$LazVersionPostfix.$LazRevision
    fi
    echo "Appending svn revision $LazVersionPostfix to deb name"
    ;;

  chmhelp)
    echo "using package chmhelp"
    UseCHMHelp=1
    ;;

  pas2jszip)
    shift
    echo "param=$1"
    Pas2jsZip=$1
    Pattern="*pas2js*.zip"
    if [[ $Pas2jsZip == $Pattern ]]; then
      echo "using pas2js zip file $Pas2jsZip"
    else
      echo "invalid pas2js zip file $Pas2jsZip"
      exit -1
    fi
    if [ ! -f $Pas2jsZip ]; then
      echo "missing pas2js zip file $Pas2jsZip"
      exit -1
    fi
    ;;

  *)
    echo "invalid parameter $1"
    echo "Usage: ./create_lazarus_deb.sh [chmhelp] [gtk1] [qt] [qtlib=<dir>] [release=svn] "
    exit 1
    ;;
  esac
  shift
done

# get date
Date=`date +%Y%m%d`
ChangeLogDate=`date --rfc-822`

# get FPC version
Arch=`dpkg --print-architecture`
echo "debian architecture=$Arch"
targetos=$Arch
if [  "$Arch" = i386 ]; then
  ppcbin=ppc386
else
  if [ "$Arch" = amd64 ]; then 
    ppcbin=ppcx64 
    targetos=x86_64
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

# get Lazarus version
LazVersion=$(./get_lazarus_version.sh)
# get consistent major.minor.release version, to avoid dpkg install an older version
if [ $(echo $LazVersion | egrep '^[^.]*\.[^.]*$') ]; then
  LazVersion=${LazVersion}.0
fi
LazVersion=$LazVersion$LazVersionPostfix

PkgName=lazarus-project
SrcTGZ=$PkgName-$LazVersion-$LazRelease.tar.gz
CurDir=`pwd`
TmpDir=~/tmp/$PkgName$LazVersion
LazBuildDir=$TmpDir/${PkgName}_build
LazDeb=$CurDir/${PkgName}_${LazVersion}-${LazRelease}_$Arch.deb
DebianSrcDir=$CurDir/debian_lazarus
EtcSrcDir=$CurDir/linux
LazSrcDir=../..
LazDestDir=$LazBuildDir/usr/share/lazarus/${LazVersion}
LazDestDirInstalled=/usr/share/lazarus/${LazVersion}
Pas2jsVer=

echo "ppcbin=$ppcbin"
echo "LazVersion=$LazVersion"
echo "FPCVersion=$FPCVersion"
echo "ChangeLogDate=$ChangeLogDate"

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
ls -l $SrcTGZ
mkdir -p $LazDestDir
cd $LazDestDir
tar xzf $CurDir/$SrcTGZ --strip 1
cd -
if [ "$UseCHMHelp" = "1" ]; then
  echo
  echo "Copying chm files"
  cd $LazSrcDir/docs/chm
  cp -v *.kwd *.chm $LazDestDir/docs/chm/
  cd -
fi
if [ ! "x$Pas2jsZip" = "x" ]; then
  # unzip pas2jszip to pas2js/version
  mkdir $LazDestDir/pas2js   # fails if already there -> good
  unzip $Pas2jsZip -d $LazDestDir/pas2js
  Pas2jsBin="$LazDestDir/pas2js/*pas2js*/bin/pas2js"
  if [ ! -f $Pas2jsBin ]; then
    echo "missing $Pas2jsZip/*pas2js*/bin/pas2js"
    exit 1
  fi
  Pas2jsVer=$($Pas2jsBin -iV | tr -d '\n')
  mv $LazDestDir/pas2js/*pas2js* $LazDestDir/pas2js/$Pas2jsVer
fi
chmod a-x $LazDestDir/debian/rules

# compile
echo
echo "Compiling may take a while ... =========================================="
cd $LazDestDir
MAKEOPTS="-Fl/opt/gnome/lib"
if [ -n "$FPCCfg" ]; then
  MAKEOPTS="$MAKEOPTS -n @$FPCCfg"
fi
echo "MAKEOPTS=$MAKEOPTS"
# build
export LCL_PLATFORM=$LCLWidgetset
make bigide PP=$ppcbin OPT="$MAKEOPTS"
# create directories for building alternative widgetsets
mkdir -p units/${targetos}-linux/qt

export LCL_PLATFORM=

find . -name '*.or' -delete
strip lazarus
strip startlazarus
strip lazbuild
strip tools/lazres
strip tools/updatepofiles
strip tools/lrstolfm
strip tools/svn2revisioninc
if [ -f components/chmhelp/lhelp/lhelp ]; then
  strip components/chmhelp/lhelp/lhelp
fi
cd -

# get installed size in kb
LazSize=$(du -s $LazDestDir | cut -f1)

# create control file
echo "========================================================================="
echo "copying control file"
mkdir -p $LazBuildDir/DEBIAN
cat $DebianSrcDir/control$LCLWidgetset | \
  sed -e "s/FPCVERSION/$FPCVersion/g" \
      -e "s/LAZVERSION/$LazVersion/g" \
      -e "s/ARCH/$Arch/g" \
      -e "s/LAZSIZE/$LazSize/g" \
      -e "s/PKGNAME/$PkgName/g" \
  > $LazBuildDir/DEBIAN/control
cp $DebianSrcDir/conffiles $LazBuildDir/DEBIAN/

# copyright and changelog files
echo "copying copyright and changelog files"
LazBuildDocDir=$LazBuildDir/usr/share/doc/$PkgName
mkdir -p $LazBuildDocDir
cp $DebianSrcDir/copyright $LazBuildDocDir/
cat $LazSrcDir/debian/changelog | sed -e "s/^lazarus (/$PkgName (/" > $LazBuildDocDir/changelog
cp $LazBuildDocDir/changelog $LazBuildDocDir/changelog.Debian
gzip -n --best $LazBuildDocDir/changelog
gzip -n --best $LazBuildDocDir/changelog.Debian
mkdir -p $LazBuildDir/usr/share/lintian/overrides
cat $DebianSrcDir/lintian.overrides | sed -e "s/^lazarus:/$PkgName:/" > $LazBuildDir/usr/share/lintian/overrides/$PkgName

# icons, links
mkdir -p $LazBuildDir/usr/share/pixmaps
mkdir -p $LazBuildDir/usr/share/applications
mkdir -p $LazBuildDir/usr/share/mime/packages
mkdir -p $LazBuildDir/usr/bin/
install -m 644 $LazDestDir/images/icons/lazarus128x128.png $LazBuildDir/usr/share/pixmaps/lazarus.png
install -m 644 $LazDestDir/install/lazarus.desktop $LazBuildDir/usr/share/applications/lazarus.desktop
install -m 644 $LazDestDir/install/lazarus-mime.xml $LazBuildDir/usr/share/mime/packages/lazarus.xml
ln -s $LazDestDirInstalled/lazarus $LazBuildDir/usr/bin/lazarus-ide
ln -s $LazDestDirInstalled/startlazarus $LazBuildDir/usr/bin/startlazarus
ln -s $LazDestDirInstalled/lazbuild $LazBuildDir/usr/bin/lazbuild

# docs
mkdir -p $LazBuildDir/usr/share/man/man1
for exe in lazbuild lazarus-ide startlazarus lazres svn2revisioninc updatepofiles; do
  cat $LazDestDir/install/man/man1/$exe.1 | gzip -n --best > $LazBuildDir/usr/share/man/man1/$exe.1.gz
done

# default configs
mkdir -p $LazBuildDir/etc/lazarus
cat $EtcSrcDir/environmentoptions.xml | \
  sed -e "s#__LAZARUSDIR__#$LazDestDirInstalled/#" \
      -e "s#__FPCSRCDIR__#/usr/share/fpcsrc/\$(FPCVER)/#" \
  > $LazBuildDir/etc/lazarus/environmentoptions.xml
if [ -n $Pas2jsZip ]; then
  cat $EtcSrcDir/pas2jsdsgnoptions.xml | \
    sed -e "s#__PAS2JSVERSION__#$Pas2jsVer#" \
    > $LazBuildDir/etc/lazarus/pas2jsdsgnoptions.xml
  cat $LazBuildDir/etc/lazarus/pas2jsdsgnoptions.xml
fi
chmod 644 $LazBuildDir/etc/lazarus/*.xml

# fixing permissions
echo "fixing permissions ..."
find $LazBuildDir -type d -exec chmod 755 {} \;
find $LazBuildDir -perm 775 -exec chmod 755 {} \;
find $LazBuildDir -perm 664 -exec chmod 644 {} \;

# postinst + postrm:
#  don't know

# creating deb
echo "creating deb ..."
cd $TmpDir
fakeroot dpkg-deb --build $LazBuildDir
mv $LazBuildDir.deb $LazDeb
echo "you can now test it with lintian $LazDeb"
cd -

# removing temporary files
rm -r $TmpDir

# end.


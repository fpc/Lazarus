#!/bin/bash

#set -x
set -e


#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 [deb] <FPCSrcDir> [release]"

PkgType=rpm
if [ "x$1" = "xdeb" ]; then
  PkgType=deb
  shift
fi

FPCSourceDir=$1
if [ "x$FPCSourceDir" = "x" ]; then
  echo $Usage
  exit -1
fi
shift

LazRelease=$1
if [ "x$LazRelease" = "x" ]; then
  LazRelease=$(date +%y%m%d)
else
  shift
fi

#------------------------------------------------------------------------------
# patching
#------------------------------------------------------------------------------

# retrieve the version information
echo -n "getting FPC version from local svn ..."
VersionFile="$FPCSourceDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
LazVersion=$CompilerVersionStr
echo " $CompilerVersionStr-$LazRelease"

FPCTGZ=$(rpm/get_rpm_source_dir.sh)/SOURCES/fpc-src-$CompilerVersionStr-$LazRelease.source.tar.gz
CurDir=`pwd`

# pack the directory
sh create_fpc_tgz_from_local_dir.sh $FPCSourceDir $FPCTGZ

if [ "$PkgType" = "deb" ]; then
  # build fpc-src deb

  echo building fpc-src deb ...

  FPCSrcTmpDir=/tmp/fpc-src$LazVersion
  FPCSrcBuildDir=$FPCSrcTmpDir/fpc-src_build
  FPCSrcDeb=fpc-src-$LazVersion-$LazRelease.deb
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
  tar xzf $CurDir/$FPCTGZ
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
  mkdir -p $FPCSrcBuildDir/usr/share/doc/fpc-src
  cat $DebianSrcDir/changelog | \
    sed -e "s/FPCVERSION/$LazVersion-$LazRelease/g" \
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

else
  # build fpc-src rpm

  echo "building fpc-src rpm ..."

  # create spec file
  SpecFile=rpm/fpc-src-$LazVersion-$LazRelease.spec
  cat fpc-src.spec | \
    sed -e "s/LAZVERSION/$LazVersion/g" -e "s/LAZRELEASE/$LazRelease/" \
    > $SpecFile
    
  # copy custom rpm scripts
  cp smart_strip.sh /tmp/smart_strip.sh
  chmod a+x /tmp/smart_strip.sh
  cp do_nothing.sh /tmp/do_nothing.sh
  chmod a+x /tmp/do_nothing.sh

  # build rpm
  rpmbuild -ba $SpecFile || rpm -ba $SpecFile

  echo "The new rpm can be found in $(./rpm/get_rpm_source_dir.sh)/RPMS/i386/fpc-src-$LazVersion-$LazRelease.i386.rpm"
fi

# end.


#!/bin/bash

set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 fpc|fpc-src [notemp] <FPCSrcDir> [release]"

# what package should be built ...
PackageName=""
if [ "$1" = fpc ]; then
    PackageName=$1
fi
if [ "$1" = fpc-src ]; then
    PackageName=$1
fi
if [ "x$PackageName" = "x" ]; then
  echo $Usage
  exit -1
fi
shift

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

fakeroot -v

getBINUTILSPREFIX() {
  _IFS="$IFS"
  IFS=":"
  set $PATH
  IFS="$_IFS"
  for p in "$@"
  do
    as=`echo $p/${TARGET_PREFIX}*as`
    if test -x "$as"
    then
      TARGET_PREFIX="${as%%as}"
      break
    fi
  done
  if test -x "${TARGET_PREFIX}as"
  then echo "${TARGET_PREFIX}"
  fi
}

#------------------------------------------------------------------------------
# retrieve the version information

echo -n "getting FPC version from local svn ..."
VersionFile="$FPCSrcDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^-1-9]//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
FPCVersion="$CompilerVersion.$CompilerRelease"
FPCVersion="$FPCVersion.$CompilerPatch"
echo " $CompilerVersionStr-$FPCRelease"

#------------------------------------------------------------------------------ 
# architecture dependent stuff 

Arch=`dpkg --print-architecture` 

CPU_TARGET="${CPU_TARGET:-$Arch}"
case "$CPU_TARGET" in
  i386)  ppcbin=ppc386;;
  amd64) ppcbin=ppcx64;;
  powerpc) ppcbin=ppcppc;;
  sparc) ppcbin=ppcsparc;;
  arm) ppcbin=ppcarm;;
  *)    echo "$CPU_TARGET is not supported."
        exit -1;;
esac

if [ "$CPU_TARGET" != "$Arch" ]
then TARGET_SUFFIX="-${CPU_TARGET}"
     TARGET_PREFIX="${CPU_TARGET}-"
     CROSSINSTALL=1
fi 

if test -n "$OS_TARGET"
then
	TARGET_SUFFIX="${TARGET_SUFFIX}-${OS_TARGET}"
	TARGET_PREFIX="${TARGET_PREFIX}${OS_TARGET}-"
fi

if test -z "$FPC"
then
	FPC="`fpc -P$Arch -PB`"
fi

BINUTILS=binutils
# detect any finalprefix elements
if test -n "$TARGET_PREFIX"
then
  BINUTILSPREFIX="`getBINUTILSPREFIX $TARGET_PREFIX`"
  if test -n "$BINUTILSPREFIX"
  then echo "BINUTILSPREFIX=$BINUTILSPREFIX"
       BINUTILS=`dpkg -S "${BINUTILSPREFIX}as" | sed "s/:.*//"`
  else echo "Can't find cross binutils"
       exit 1
  fi
fi


#------------------------------------------------------------------------------
# download/export fpc svn if needed

SrcTGZ=$(pwd)/fpc-$FPCVersion-$FPCRelease.tar.gz

if [ ! -f $SrcTGZ ]; then
  ./create_fpc_export_tgz.sh $FPCSrcDir $SrcTGZ
fi


#------------------------------------------------------------------------------
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
# setup variables

CurDir=`pwd`
FPCBuildDir=$TmpDir/fpc_build
FPCDeb=$CurDir/${PackageName}${TARGET_SUFFIX}_$FPCVersion-${FPCRelease}_$Arch.deb
ResourceDir=$CurDir/debian_$PackageName
DebianInstallDir=$FPCBuildDir/usr
DebianRulezDir=$FPCBuildDir/DEBIAN/
DebianDocDir=$FPCBuildDir/usr/share/doc/$PackageName${TARGET_SUFFIX}
DebianSourceDir=$FPCBuildDir/usr/share/fpcsrc
Date=`date --rfc-822`


#------------------------------------------------------------------------------
# patch sources

ReplaceScript=replace_in_files.pl

# set version numbers in all Makefiles
echo "set version numbers in all Makefiles ..."
perl replace_in_files.pl -sR -f '=\d.\d.\d' -r =$CompilerVersionStr -m 'Makefile(.fpc)?' $FPCSrcDir/*


#------------------------------------------------------------------------------
# create rulez and files

# change debian files
mkdir -p $DebianDocDir
chmod 755 $DebianDocDir
mkdir -p $DebianRulezDir
chmod 755 $DebianRulezDir

# create debian control file, which contains the package description
echo "creating DEBIAN/control file"
cat $ResourceDir/control \
  | sed -e "s/FPCVERSION/$FPCVersion/g" -e "s/ARCH/$Arch/g" \
        -e "s/^Package: .*/Package: $PackageName$TARGET_SUFFIX/" \
        -e "s/Depends: binutils/Depends: $BINUTILS/" \
  > $DebianRulezDir/control
# create debian changelog file, needed for version
echo "creating usr/share/doc/fpc/changelog file ..."
File=$DebianDocDir/changelog
echo "fpc ($FPCVersion-$FPCRelease) unstable; urgency=low" > $File
echo '  * Unofficial snapshot build for lazarus' >> $File
echo " -- Mattias Gaertner <mattias@freepascal.org>  $Date" >> $File
echo "" >> $File
cat $ResourceDir/changelog >> $File
rm -f $File.gz
gzip --best $File

# create postinst if needed
if [ -f "$ResourceDir/postinst" ]; then
    echo "creating DEBIAN/postinst file"
    cat $ResourceDir/postinst \
      | sed -e "s/FPCVERSION/$FPCVersion/g" -e "s/PPCBIN/$ppcbin/g" \
      > $DebianRulezDir/postinst
    chmod a+rx $DebianRulezDir/postinst
fi

# create changelog.Debian file
echo "creating changelog.Debian file ..."
File=$DebianDocDir/changelog.Debian
cp $ResourceDir/changelog.Debian $File
rm -f $File.gz
gzip --best $File

# create debian copyright file
echo "creating copyright file ..."
cp $ResourceDir/copyright $DebianDocDir/

#------------------------------------------------------------------------------

if [ "$PackageName" = "fpc-src" ]; then
    # copy fpc sources
    mkdir -p $DebianSourceDir
    cp -a $FPCSrcDir/* $DebianSourceDir/
fi

if [ "$PackageName" = "fpc" ]; then
    # build fpc
    mkdir -p $FPCBuildDir/etc
    cd $FPCSrcDir
    make clean all ${CPU_TARGET:+CPU_TARGET=$CPU_TARGET} ${OS_TARGET:+OS_TARGET=$OS_TARGET} ${FPC:+FPC=$FPC} ${BINUTILSPREFIX:+BINUTILSPREFIX=$BINUTILSPREFIX} ${CROSSINSTALL:+CROSSINSTALL=$CROSSINSTALL}
    mkdir -p $DebianInstallDir
    make install INSTALL_PREFIX=$DebianInstallDir ${CPU_TARGET:+CPU_TARGET=$CPU_TARGET} ${OS_TARGET:+OS_TARGET=$OS_TARGET} ${FPC:+FPC=$FPC} ${BINUTILSPREFIX:+BINUTILSPREFIX=$BINUTILSPREFIX} ${CROSSINSTALL:+CROSSINSTALL=$CROSSINSTALL}

    # if building cross-package
    # 1. get rid of ./doc
    # 2. rename bin's with prefix
    #
    if test -n "${TARGET_PREFIX}"
    then
	find $DebianInstallDir
    fi
    cd -
fi

# fixing permissions
echo "fixing permissions ..."
find $FPCBuildDir -type d | xargs chmod 755  # this is needed, don't ask me why
find $FPCBuildDir -type f | xargs chmod a+r  # this is needed, don't ask me why

#------------------------------------------------------------------------------
# creating deb

cd $TmpDir
fakeroot dpkg-deb --build $FPCBuildDir
mv $FPCBuildDir.deb $FPCDeb

echo "The new deb can be found at $FPCDeb"
echo "You can test it with lintian."

# end.


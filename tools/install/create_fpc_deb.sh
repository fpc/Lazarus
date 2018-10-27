#!/bin/bash

set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: [OS_TARGET=xxx] [CPU_TARGET=xxx] [BINUTILSPREFIX=xxx] $0 fpc|fpc-src [notemp] <FPCSrcDir> [release]"

TmpDir=$TEMP
if [ -z "$TmpDir" ]; then
  TmpDir=~/tmp
fi
TmpDir=$TmpDir/fpc_patchdir

# what package should be built ...
PackageName=""
if [ "$1" = fpc ]; then
    PackageName=fpc-laz
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
    set `echo $p/${TARGET_PREFIX}*as`
    as="$1"
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
CompilerVersion=$(cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerRelease=$(cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerPatch=$(cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
FPCVersion="$CompilerVersion.$CompilerRelease.$CompilerPatch"
echo " $CompilerVersionStr-$FPCRelease"

#------------------------------------------------------------------------------
# architecture dependent stuff

Arch=`dpkg --print-architecture`

CPU_TARGET="${CPU_TARGET:-$Arch}"

case "$CPU_TARGET" in
  i386)    ppcbin=386;   FPCArch=i386;;
  amd64)   ppcbin=x64;   FPCArch=x86_64;;
  powerpc) ppcbin=ppc;   FPCArch=powerpc;;
  sparc)   ppcbin=sparc; FPCArch=sparc;;
  arm)     ppcbin=arm;   FPCArch=arm;;
  *)    echo "$CPU_TARGET is not supported."
        exit -1;;
esac

if [ "$CPU_TARGET" != "$Arch" ]
then TARGET_SUFFIX="-${CPU_TARGET}"
     TARGET_PREFIX="${CPU_TARGET}-"
     CROSSINSTALL=1
     PPPRE=ppcross
else
     PPPRE=ppc
fi

if test -n "$OS_TARGET"
then
    TARGET_SUFFIX="${TARGET_SUFFIX}-${OS_TARGET}"
    TARGET_RPPEFIX="${TARGET_PREFIX}${OS_TARGET}-"
    TARGET="${CPU_TARGET}-${OS_TARGET}"
    CROSSINSTALL=1
fi

if test -z "$FPC"
then
    FPC="`fpc -P$FPCArch -PB`"
fi

BINUTILS=binutils
# detect any finalprefix elements
if test -n "$TARGET_PREFIX" -a -z "$BINUTILSPREFIX"
then
  BINUTILSPREFIX="`getBINUTILSPREFIX $BINUTILSPREFIX`"

  if test -n "$BINUTILSPREFIX"
  then echo "BINUTILSPREFIX=$BINUTILSPREFIX"
     BINUTILS=`dpkg -S "${BINUTILSPREFIX}as" | sed "s/:.*//"`
  else echo "Can't find cross binutils automatically, consider setting BINUTILSPREFIX"
     exit 1
  fi
fi

#------------------------------------------------------------------------------
# download/export fpc svn if needed

SrcTGZ=$(pwd)/fpc-$FPCVersion-$FPCRelease.tar.gz

if [ ! -f $SrcTGZ ]; then
  ./create_fpc_export_tgz.sh $FPCSrcDir $SrcTGZ
fi

# optional: svn/fpcbuild/trunk/install under ../install
FPCManDir=$FPCSrcDir/../install/man

#------------------------------------------------------------------------------
# create a temporary copy of the fpc sources to patch it

if [ "$WithTempDir" = "yes" ]; then
  if [ -d $TmpDir ]; then
    rm -rf $TmpDir
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
DebianLintianDir=$FPCBuildDir/usr/share/lintian
DebianSourceDir=$FPCBuildDir/usr/share/fpcsrc/$FPCVersion
Date=`date --rfc-822`

#------------------------------------------------------------------------------
# patch sources

ReplaceScript=replace_in_files.pl

# set version numbers in all Makefiles
echo "set version numbers in all Makefiles ..."
perl replace_in_files.pl -sR -f 'version=\d.\d.\d' -r version=$CompilerVersionStr -m 'Makefile(.fpc)?' $FPCSrcDir/*


#------------------------------------------------------------------------------

mkdir -p $DebianDocDir
chmod 755 $DebianDocDir
mkdir -p $DebianRulezDir
chmod 755 $DebianRulezDir
mkdir -p $DebianLintianDir
chmod 755 $DebianLintianDir

if [ "$PackageName" = "fpc-src" ]; then
    # copy fpc sources
    mkdir -p $DebianSourceDir
    cp -a $FPCSrcDir/* $DebianSourceDir/
fi

if [ "$PackageName" = "fpc-laz" ]; then
  # build fpc
  mkdir -p $FPCBuildDir/etc
  cd $FPCSrcDir
  make clean all ${FPCArch:+FPCArch=$FPCArch} ${OS_TARGET:+OS_TARGET=$OS_TARGET} ${FPC:+FPC=$FPC} ${BINUTILSPREFIX:+BINUTILSPREFIX=$BINUTILSPREFIX} ${CROSSINSTALL:+CROSSINSTALL=$CROSSINSTALL}
  mkdir -p $DebianInstallDir
  make install INSTALL_PREFIX=$DebianInstallDir ${FPCArch:+FPCArch=$FPCArch} ${OS_TARGET:+OS_TARGET=$OS_TARGET} ${FPC:+FPC=$FPC} ${BINUTILSPREFIX:+BINUTILSPREFIX=$BINUTILSPREFIX} ${CROSSINSTALL:+CROSSINSTALL=$CROSSINSTALL}
  if test -z "$BINUTILSPREFIX"
  then
    # need up to date samplecfg that chains cross compiler additions
    SampleCfg=$DebianInstallDir/lib/fpc/$FPCVersion/samplecfg  
    grep 'fpc-cross.cfg' "$SampleCfg" &>/dev/null || \
      sed -i -e "/^FPCPATH=/aFPCPARENT=\"\`dirname \"\$1\"\`\"
;/^#ENDIF NEEDCROSSBINUTILS/i#include \$FPCPARENT/fpc-cross.cfg"  "$SampleCfg"
  else cat > $DebianInstallDir/lib/fpc/$FPCVersion/fpc${TARGET_SUFFIX}.cfg <<CROSS
# Detect $TARGET compiles
#IF \$fpc-target = $TARGET
 -XP$BINUTILSPREFIX
#WRITE Target $TARGET with binutils prefix $BINUTILSPREFIX
#END
CROSS
  fi
  cd -

  # remove non binaries in /usr/bin
  for f in $DebianInstallDir/bin/*; do
    if [ ! -x "$f" ]; then
      rm $f
    fi
  done

  # docs
  if [ -d "$FPCManDir" ]; then
    #
    mkdir -p $FPCBuildDir/usr/share/man/man1
    for man in $FPCManDir/man1/*.1; do
      echo "copying man page $man"
      shortman=$(basename $man)
      cat $man | gzip -n --best > $FPCBuildDir/usr/share/man/man1/$shortman.gz
    done
  else
    echo "WARNING: man directory not found: $FPCManDir"
  fi
fi

#------------------------------------------------------------------------------
# create rulez and files

# change debian files
DEPENDS="$BINUTILS"

if test -n "$CROSSINSTALL"
then
  DEPENDS="$DEPENDS, fpc (= $FPCVersion)"
fi

# get installed size in kb
DebSize=$(du -s $FPCBuildDir | cut -f1)

# create debian control file, which contains the package description
echo "creating DEBIAN/control file"
cat $ResourceDir/control \
  | sed -e "s/FPCVERSION/$FPCVersion/g" -e "s/ARCH/$Arch/g" \
        -e "s/^Package: .*/Package: $PackageName$TARGET_SUFFIX/" \
        -e "s/Depends: binutils/Depends: $DEPENDS/" \
        -e "s/DEBSIZE/$DebSize/" \
  > $DebianRulezDir/control
mkdir -p $DebianLintianDir/overrides
cp $ResourceDir/lintian.overrides $DebianLintianDir/overrides/$PackageName$TARGET_SUFFIX

# identify conf files
if test -n "$TARGET_SUFFIX"
then
echo "/usr/lib/fpc/$FPCVersion/fpc${TARGET_SUFFIX:--cross}.cfg" >> $DebianRulezDir/conffiles
fi

# create debian changelog file, needed for version
echo "creating usr/share/doc/fpc/changelog file ..."
File=$DebianDocDir/changelog
echo "fpc ($FPCVersion-$FPCRelease) unstable; urgency=low" > $File
echo '  * Unofficial snapshot build for lazarus' >> $File
echo " -- Mattias Gaertner <mattias@freepascal.org>  $Date" >> $File
echo "" >> $File
cat $ResourceDir/changelog >> $File
rm -f $File.gz
gzip -n --best $File
cp $File.gz $File.Debian.gz

# create postinst if needed
if [ -f "$ResourceDir/postinst" ]
then
  if [ -z "$CROSSINSTALL" ]
  then
    echo "creating DEBIAN/postinst file"
    cat $ResourceDir/postinst \
      | sed -e "s/FPCVERSION/$FPCVersion/g" -e "s/PPCBIN/$PPPRE$ppcbin/g" \
      > $DebianRulezDir/postinst
    cat >> $DebianRulezDir/postinst <<CFG
#! /bin/sh
set -e
touch /usr/lib/fpc/$FPCVersion/fpc-cross.cfg
sed -i -e "/^#if 2.3.1 /{:eat;s/.*//;N;/#end/d;beat}" /usr/lib/fpc/$FPCVersion/fpc-cross.cfg
cat >> /usr/lib/fpc/$FPCVersion/fpc-cross.cfg << FPCCFG
#if $FPCVersion = \\\$fpcversion
#include /usr/lib/fpc/$FPCVersion/fpc-cross.cfg
#end
FPCCFG
CFG
    chmod a+rx $DebianRulezDir/postinst
    # un-install
    cat > $DebianRulezDir/prerm <<CROSS
#! /bin/sh
set -e
rm -f /usr/lib/fpc/$FPCVersion/ppc$ppcbin
# remove fpc-cross include lines
if [ -f /usr/lib/fpc/$FPCVersion/fpc-cross.cfg ]; then
  sed -i -e "/^#if 2.3.1 /{:eat;s/.*//;N;/#end/d;beat}" /usr/lib/fpc/$FPCVersion/fpc-cross.cfg
fi
CROSS
    chmod a+rx $DebianRulezDir/prerm
  else
    # cross-compilerpostinst
    cat > $DebianRulezDir/postinst <<CROSS
#! /bin/sh
set -e
ln -sf /usr/lib/fpc/$FPCVersion/$PPPRE$ppcbin /usr/bin/ppc$ppcbin
grep 2>/dev/null '#include /usr/lib/fpc/$FPCVersion/fpc${TARGET_SUFFIX}.cfg' /usr/lib/fpc/$FPCVersion/fpc-cross.cfg || echo '#include /usr/lib/fpc/$FPCVersion/fpc${TARGET_SUFFIX}.cfg' >> /usr/lib/fpc/$FPCVersion/fpc-cross.cfg
CROSS
    chmod a+rx $DebianRulezDir/postinst
    # un-install
    cat > $DebianRulezDir/prerm <<CROSS
#! /bin/sh
set -e
rm -f /usr/lib/fpc/$FPCVersion/$PPPRE$ppcbin
if [ -f /usr/lib/fpc/$FPCVersion/fpc-cross.cfg ]; then
  sed -i -e "/#include \/usr\/lib\/fpc\/$FPCVersion\/fpc${TARGET_SUFFIX}.cfg/d" /usr/lib/fpc/$FPCVersion/fpc-cross.cfg
fi
CROSS
    chmod a+rx $DebianRulezDir/prerm
  fi
fi

# create changelog.Debian file
echo "creating changelog.Debian file ..."
File=$DebianDocDir/changelog.Debian
cp $ResourceDir/changelog.Debian $File
rm -f $File.gz
gzip -n --best $File

# create debian copyright file
echo "creating copyright file ..."
cp $ResourceDir/copyright $DebianDocDir/


#------------------------------------------------------------------------------
# fixing permissions
echo "fixing permissions ..."
find $FPCBuildDir -type d -print0 | xargs -0 chmod 755  # this is needed, don't ask me why
find $FPCBuildDir -type f -print0 | xargs -0 chmod a+r  # this is needed, don't ask me why
find $FPCBuildDir -perm 775 | xargs -d '\n' chmod 755 || true
find $FPCBuildDir -perm 664 | xargs -d '\n' chmod 644 || true

#------------------------------------------------------------------------------
# creating deb

cd $TmpDir
fakeroot dpkg-deb --build $FPCBuildDir
mv $FPCBuildDir.deb $FPCDeb

echo "The new deb can be found at $FPCDeb"
echo "You can test it with lintian."

# end.


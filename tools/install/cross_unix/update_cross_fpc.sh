#!/usr/bin/env bash
# Author: Mattias Gaertner
# License: LGPL
# Abstract: Download, compile binutils and FPC

set -e
set -x

# This is the root for all download and building directories
BuildRoot=~/freepascal

# the binutils version to download
BinutilsVersion=2.16
BinutilsDownloadPath=http://ftp.gnu.org/gnu/binutils/

# the FPC targets
Targets="i386-win32";

# steps
DownloadBinutils=no
DownloadFPC=no
BuildBinutils=no
BuildCrossFPC=no
BuildNormalFPC=no
CreateFPCCfg=yes

#===============================================================================
# calculate targets

TARGETS_WIN=""        # "cygwin mingw32 msdosdjgpp"
TARGETS_I386=""       # "freebsd netbsd openbsd linux solaris darwin"
TARGETS_POWERPC=""    # "freebsd netbsd openbsd linux darwin"
TARGETS_SPARC=""      # "freebsd netbsd openbsd linux solaris"
TARGETS_M68k=

for Target in $Targets; do
  if [ $Target = 'i386-win32' ]; then
    TARGETS_WIN="$TARGETS_WIN mingw32"
  else
    TargetCPU=$(echo $Target | sed -e 's#^\(.*\)-.*$#\1#')
    TargetOS=$(echo $Target | sed -e 's#^.*-\(.*\)$#\1#')
    if [ $TargetCPU = 'i386' ]; then
      TARGETS_I386="$TARGETS_I386 $TargetOS"
    fi
    if [ $TargetCPU = 'powerpc' ]; then
      TARGETS_I386="$TARGETS_POWERPC $TargetOS"
    fi
    if [ $TargetCPU = 'sparc' ]; then
      TARGETS_I386="$TARGETS_SPARC $TargetOS"
    fi
    if [ $TargetCPU = 'm86k' ]; then
      TARGETS_I386="$TARGETS_M68k $TargetOS"
    fi
  fi
done
TARGETS_WIN=$(echo $TARGETS_WIN | sed -e 's#^ +##g' -e 's# +$##g')
TARGETS_I386=$(echo $TARGETS_I386 | sed -e 's#^ +##g' -e 's# +$##g')
TARGETS_POWERPC=$(echo $TARGETS_POWERPC | sed -e 's#^ +##g' -e 's# +$##g')
TARGETS_SPARC=$(echo $TARGETS_SPARC | sed -e 's#^ +##g' -e 's# +$##g')
TARGETS_M68k=$(echo $TARGETS_M68k | sed -e 's#^ +##g' -e 's# +$##g')

#===============================================================================
# Download binutils

DownloadDir=$BuildRoot/download
BinutilsFilename=binutils-$BinutilsVersion.tar.gz
BinutilsTGZ=$DownloadDir/$BinutilsFilename
if [ $DownloadBinutils = "yes" ]; then
  if [ ! -f $BinutilsTGZ ]; then
    mkdir -p $DownloadDir
    cd $DownloadDir
    wget $BinutilsDownloadPath/$BinutilsFilename
  fi
fi

#===============================================================================
# Download FPC - update existing or download stable tree

if [ $DownloadFPC = "yes" ]; then
  cd $BuildRoot
  if [ -f fpc ]; then
    cd fpc
    svn cleanup
    svn up
  else
    svn co http://svn.freepascal.org/svn/fpc/branches/fixes_2_0 fpc
  fi
  if [ -f install ]; then
    cd install
    svn cleanup
    svn up
  else
    svn co http://svn.freepascal.org/svn/fpcbuild/branches/fixes_2_0/install install
  fi
fi

#===============================================================================
# setup some variables
BinDir=$BuildRoot/bin/

# retrieve the version information
VersionFile="$BuildRoot/fpc/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"

#===============================================================================
# build binutils
if [ $BuildBinutils = "yes" ]; then
  # create custom version of buildcrossbinutils
  echo "building cross binutils ..."
  cd $BuildRoot/install/cross/
  cat buildcrossbinutils | \
    sed -e 's#^BASE=.*$#BASE='"$BuildRoot"'/binutils#' \
        -e 's#^BINUTILSPATH=.*$#BINUTILSPATH='"$DownloadDir"'/#' \
        -e 's#^BINUTILSBASE=.*$#BINUTILSBASE=binutils#' \
        -e 's#^BINUTILSVERSION=.*$#BINUTILSVERSION='"$BinutilsVersion"'#' \
        -e 's#^BINUTILS_GZIP=.*$#BINUTILS_GZIP=yes#' \
        -e 's#^\(TARGETS_.*=\).*$#\1#' \
        -e 's#^TARGETS_WIN=.*$#TARGETS_WIN='"$TARGETS_WIN"'#' \
        -e 's#^TARGETS_I386=.*$#TARGETS_I386='"$TARGETS_I386"'#' \
        -e 's#^TARGETS_POWERPC=.*$#TARGETS_POWERPC='"$TARGETS_POWERPC"'#' \
        -e 's#^TARGETS_SPARC=.*$#TARGETS_SPARC='"$TARGETS_SPARC"'#' \
        -e 's#^TARGETS_M68k=.*$#TARGETS_M68k='"$TARGETS_M68k"'#' \
    > buildcrossbinutils.sh
  #debugging: head -n 50 buildcrossbinutils.sh

  # build the cross binutils
  echo "HINT: when something goes wrong see the log files in $BuildRoot/bintuils/logs/"
  sh buildcrossbinutils.sh
fi

#===============================================================================
# build cross FPC
if [ $BuildCrossFPC = "yes" ]; then
  echo "building cross FPC ..."
  for Target in $Targets; do
    TargetCPU=$(echo $Target | sed -e 's#^\(.*\)-.*$#\1#')
    TargetOS=$(echo $Target | sed -e 's#^.*-\(.*\)$#\1#')

    # create custom version of buildcrosssnapshot
    cd $BuildRoot/install/cross/
    cat buildcrosssnaphot | \
      sed -e 's#^CROSSTOOLSROOT=.*$#CROSSTOOLSROOT='"$BuildRoot"'/binutils/cross/#' \
          -e 's#^FPCCVS=.*$#FPCCVS='"$BuildRoot"'/fpc#' \
          -e 's#^TARGETS_OS=.*$#TARGETS_OS='"$TargetOS"'#' \
          -e 's#^TARGETS_CPU=.*$#TARGETS_CPU='"$TargetCPU"'#' \
          -e 's#^MOS=cygwin$#MOS=mingw32#' \
      > buildcrosssnapshot.sh
    #debugging: head -n 40 buildcrosssnapshot.sh

    # build the cross fpc
    echo "building cross fpc for target $Target in $BuildRoot/bintuils/cross/destination/"
    echo "see logs in $BuildRoot/bintuils/cross/logs/"
    sh buildcrosssnapshot.sh
  done
fi

#===============================================================================
# build normal FPC
# compile the non cross FPC into the smae structure, so that the fpc.cfg
# can be very simple.
if [ $BuildNormalFPC = "yes" ]; then
  echo "building normal FPC ..."
  CPU=i386  # `uname -p | tr "[:upper:]" "[:lower:]"`
  OS=`uname -s | tr "[:upper:]" "[:lower:]"`
  echo HOST platform is ${CPU}-${OS}

  MAKE=make

  case "$OS" in
   *bsd*) MAKE=gmake
    ;;
  esac

  CROSSTOOLSROOT=$BuildRoot/binutils/cross/
  DESTDIR=${CROSSTOOLSROOT}destination/
  LOGDIR=${CROSSTOOLSROOT}logs/
  BASELIBDIR=${CROSSTOOLSROOT}crosslibs

  mkdir -p ${DESTDIR}
  mkdir -p ${LOGDIR}

  cd $BuildRoot/fpc
  echo "building non cross fpc for target $CPU-$OS in $BuildRoot/bintuils/cross/destination/"
  echo "see logs in $BuildRoot/bintuils/cross/logs/"
  ${MAKE} clean all > ${LOGDIR}snapbuild-${CPU}-${OS} 2>&1
  ${MAKE} install INSTALL_PREFIX=${DESTDIR} LIBDIR=${BASELIBDIR}/${CPU}-${OS} OPT="-Xd -Xt -gl" > ${LOGDIR}snapinstalllog-${CPU}-${OS} 2>&1
  
  # save samplecfg
  mkdir -p $BinDir
  cp ${DESTDIR}lib/fpc/$CompilerVersionStr/samplecfg $BinDir
fi

#===============================================================================
# build ~/.fpc.cfg
if [ $CreateFPCCfg = "yes" ]; then
  echo "building ~/fpc.cfg ..."

  $BuildRoot/bin/samplecfg $BuildRoot/binutils/cross/destination/lib/fpc/$CompilerVersionStr/ ~/freepascal
fi

# end.


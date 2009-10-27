#!/usr/bin/env bash
# Author: Mattias Gaertner
# License: LGPL
# Abstract: Download, compile binutils and FPC

set -e
set -x

# This is the root for all download and building directories
if [ ! -d "$BuildRoot" ]; then
  BuildRoot=~/freepascal
fi

# the binutils version to download
BinutilsVersion=2.20
BinutilsDownloadPath=http://ftp.gnu.org/gnu/binutils/

# the FPC targets
Targets="i386-win32";

# where to install the fpc+binutils executables
# Only used if called with 'install'. This directory must be in $PATH.
InstallBin=~/bin/

# what intel platform do you want/need to build for?
MyIntel=i686

#===============================================================================
# parse command line parameters
DownloadBinutils=no
DownloadFPC=no
BuildBinutils=no
BuildCrossFPC=no
BuildNormalFPC=no
CreateFPCCfg=no
InstallAsDefault=no   # install ~/fpc.cfg, binaries to ~/bin

Params=$@
for p in $Params; do
  case "$p" in
  all)
    DownloadBinutils=yes
    DownloadFPC=yes
    BuildBinutils=yes
    BuildCrossFPC=yes
    BuildNormalFPC=yes
    CreateFPCCfg=yes
    ;;
  downloadbinutils)
    DownloadBinutils=yes
    ;;
  downloadfpc)
    DownloadFPC=yes
    ;;
  buildbinutils)
    BuildBinutils=yes
    ;;
  buildnormalfpc)
    BuildNormalFPC=yes
    ;;
  buildcrossfpc)
    BuildCrossFPC=yes
    ;;
  createfpccfg)
    CreateFPCCfg=yes
    ;;
  install)
    InstallAsDefault=yes
    ;;
  installbin=*)
    InstallBin=$(echo $p | sed -e 's#^installbin=##')
    ;;
  buildroot=*)
    BuildRoot=$(echo $p | sed -e 's#^buildroot=##')
    ;;
  targets=*)
    Targets=$(echo $p | sed -e 's#^targets=##')
    ;;
  *)
    echo "Unknown option: $p"
    echo
    echo "Usage:"
    echo "  $0 [all] [downloadbinutils] [downloadfpc] [buildbinutils] [buildnormalfpc] [buildcrossfpc] [createfpccfg] [install] [installbin=<dir>] [targets=<i386-win32>] [buildroot=<dir default=~/freepascal/>]"
    exit -1
    ;;
  esac
done

#===============================================================================

# expand paths
BuildRoot=$(echo $BuildRoot | sed -e 's#//#/#g' -e 's#/$##')
InstallBin=$(echo $InstallBin | sed -e 's#//#/#g' -e 's#/$##')

# the 'bin' directory - where to put the fpc+binutils executables
# On InstallAsDefault=yes they will be copied to $HomeBin
BinDir=$BuildRoot/bin/

CompilerName="ppc386"

# check install path
if [ $InstallAsDefault = "yes" ]; then
  # check if the $InstallBin is in $PATH
  TrimmedPath=$(echo $InstallBin | sed -e 's#/$##g' -e 's#//#/#')
  if [ -z $(echo $PATH | egrep "(^|:)$TrimmedPath(:|$)") ]; then
    echo "ERROR: $InstallBin not in \$PATH."
    echo "This script installs FPC and binutils executables to a directory"
    echo "that is in your search PATH before your installed FPC."
    echo "Either prepend it to \$PATH or define another with installbin=<path>"
    exit -1
  fi
  # check if $InstallBin is in PATH before the system compiler
  mkdir -p $InstallBin
  NewCompilerName=$InstallBin/$CompilerName
  if [ ! -f $NewCompilerName ]; then
    touch $NewCompilerName
    chmod a+x $NewCompilerName
  fi
  hash -r
  FoundFPC=$(which $CompilerName)
  if [ ! $FoundFPC = $NewCompilerName ]; then
    echo "ERROR: $InstallBin not early enough in \$PATH."
    echo "This script installs FPC and binutils executables to a directory"
    echo "that is in your search PATH before your installed FPC."
    echo "Either prepend it to \$PATH or define another with installbin=<path>"
    exit -1
  fi

  BinDir=$InstallBin
fi

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
    echo "Downloading $BinutilsDownloadPath/$BinutilsFilename ..."
    wget $BinutilsDownloadPath/$BinutilsFilename
  fi
fi

#===============================================================================
# Download FPC - update existing or download stable tree

if [ $DownloadFPC = "yes" ]; then
  cd $BuildRoot
  if [ -d fpc ]; then
    cd fpc
    echo "SVN update for FPC ..."
    svn cleanup
    svn up
    cd -
  else
    echo "SVN checkout for FPC 2.2 ..."
    svn co http://svn.freepascal.org/svn/fpc/branches/fixes_2_2 fpc
  fi
  if [ -d install ]; then
    cd install
    echo "SVN update for FPC install ..."
    svn cleanup
    svn up
    cd -
  else
    echo "SVN checkout for FPC 2.2 install ..."
    svn co http://svn.freepascal.org/svn/fpcbuild/branches/fixes_2_2/install install
  fi
fi

#===============================================================================
# setup some variables
# retrieve the version information
VersionFile="$BuildRoot/fpc/compiler/version.pas"
if [ ! -f $VersionFile ]; then
  echo No FPC sources found. To download them call ./update_cross_fpc.sh DownloadFPC
  exit
fi
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"

#===============================================================================
# build binutils
if [ $BuildBinutils = "yes" ]; then
  # check if cc is there
  cc -v
  # check if bison is there
  bison -V
  
  # create custom version of buildcrossbinutils
  echo "building cross binutils for targets:"
  echo "  WIN=$TARGETS_WIN"
  echo "  I386=$TARGETS_I386"
  echo "  POWERPC=$TARGETS_POWERPC"
  echo "  SPARC=$TARGETS_SPARC"
  echo "  M68K=$TARGETS_M68K"
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
        -e 's#^rm ./config.cache$##' \
    > buildcrossbinutils.sh
  #debugging: head -n 50 buildcrossbinutils.sh

  # build the cross binutils
  echo "HINT: when something goes wrong see the log files in $BuildRoot/binutils/logs/"
  rm -rf $BuildRoot/binutils
  Command=$BuildRoot/install/cross/buildcrossbinutils.sh
  echo "calling $Command ..."
  Result=$(sh buildcrossbinutils.sh)
  FailResult=$(echo "$Result" | grep -i failes || true)
  if [ -n "$FailResult" ]; then
    echo $Result
    echo "HINT: see the log files in $BuildRoot/binutils/logs/"
    exit -1
  fi

  # link binaries into the bin directory, so that FPC can find them
  mkdir -p $BinDir
  for Target in $Targets; do
    TargetCPU=$(echo $Target | sed -e 's#^\(.*\)-.*$#\1#')
    TargetOS=$(echo $Target | sed -e 's#^.*-\(.*\)$#\1#')
    BinUtilsCPU=$TargetCPU
    BinUtilsOS=$TargetOS

    if [ $TargetOS = "win32" ]; then
      BinUtilsOS="mingw32"
      BinUtilsCPU=$MyIntel
    fi
    
    BinUtilsDir=$BuildRoot/binutils/cross/bin/
    BinUtilsPrefix="$BinUtilsCPU-$BinUtilsOS-"

    cd ${BinUtilsDir}
    for binutility in $(ls -B ${BinUtilsPrefix}*); do
      link=$(echo $binutility | sed -e "s#$BinUtilsPrefix#${TargetCPU}-${TargetOS}-#")
      ln -sf ${BinUtilsDir}${binutility} ${BinDir}${link}
    done
  done
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
          -e 's#^FPCSVN=.*$#FPCSVN='"$BuildRoot"'/fpc#' \
          -e 's#^FPCCVS=.*$#FPCCVS='"$BuildRoot"'/fpc#' \
          -e 's#^TARGETS_OS=.*$#TARGETS_OS='"$TargetOS"'#' \
          -e 's#^TARGETS_CPU=.*$#TARGETS_CPU='"$TargetCPU"'#' \
          -e 's#^MOS=cygwin$#MOS=mingw32#' \
      > buildcrosssnapshot.sh
    #debugging: head -n 40 buildcrosssnapshot.sh

    # build the cross fpc
    echo "building cross fpc for target $Target in $BuildRoot/binutils/cross/destination/"
    echo "see logs in $BuildRoot/binutils/cross/logs/"
    sh buildcrosssnapshot.sh
  done
fi

#===============================================================================
# build normal FPC
# compile the non cross FPC into the same structure, so that the fpc.cfg
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
  echo "building non cross fpc for target $CPU-$OS in $DESTDIR"
  echo "see logs in $LOGDIR"
  ${MAKE} clean all > ${LOGDIR}snapbuild-${CPU}-${OS} 2>&1
  ${MAKE} install INSTALL_PREFIX=${DESTDIR} LIBDIR=${BASELIBDIR}/${CPU}-${OS} OPT="-Xd -Xt -gl" > ${LOGDIR}snapinstalllog-${CPU}-${OS} 2>&1
  
  # save samplecfg, ppc386
  mkdir -p $BinDir
  cp ${DESTDIR}lib/fpc/$CompilerVersionStr/samplecfg $BinDir
  cp ${DESTDIR}lib/fpc/$CompilerVersionStr/ppc* $BinDir
fi

#===============================================================================
# build ~/.fpc.cfg
if [ $CreateFPCCfg = "yes" ]; then
  echo "building ~/fpc.cfg ..."

  DestDir=~/freepascal
  $BuildRoot/bin/samplecfg $BuildRoot/binutils/cross/destination/lib/fpc/$CompilerVersionStr/ $DestDir
  FPCCfg=$DestDir/fpc.cfg
  
  # add -FD and -XP entry for cross compiling
  echo "# set binutils paths for crosscompiling" >> $FPCCfg
  echo "#IFDEF FPC_CROSSCOMPILING" >> $FPCCfg
  echo "  -FD${BinDir}" >> $FPCCfg
  echo '  -XP$fpctarget-' >> $FPCCfg
  echo "#ENDIF" >> $FPCCfg
  
  # test the fpc.cfg
  echo "Testing compiling test.pas ..."
  TestPas="$BuildRoot/test.pas"
  echo "program Test;" > $TestPas
  echo "{$mode objfpc}{$H+}" >> $TestPas
  echo "uses Classes, SysUtils;" >> $TestPas
  echo "begin" >> $TestPas
  echo "  writeln('Test');" >> $TestPas
  echo "end." >> $TestPas
  cd $BuildRoot
  $BinDir/$CompilerName test.pas

  if [ $InstallAsDefault = "yes" ]; then
    echo "Installing ~/fpc.cfg ..."
    cp $BuildRoot/fpc.cfg ~/.fpc.cfg
  fi
fi

# end.


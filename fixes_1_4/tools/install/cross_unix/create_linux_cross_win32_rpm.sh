#!/usr/bin/env bash
# Author: Mattias Gaertner
# License: LGPL
# Abstract: Download, compile binutils and FPC

set -e
#set -x

# This is the root for all download and building directories
if [ ! -d "$BuildRoot" ]; then
  BuildRoot=~/freepascal
fi

#===============================================================================
# parse command line parameters
echo "parsing parameters ..."
DownloadBinutils=no
DownloadFPC=no
BuildBinutils=no
BuildCrossFPC=no
BuildNormalFPC=no
BuildCrossWin32RPM=no

Params=$@
for p in $Params; do
  case "$p" in
  all)
    DownloadBinutils=yes
    DownloadFPC=yes
    BuildBinutils=yes
    BuildLinWin32RPM=no
    BuildNormalRPM=no
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
  buildcrosswin32rpm)
    BuildCrossWin32RPM=yes
    ;;
  *)
    echo "Unknown option: $p"
    echo
    echo "Usage:"
    echo "  $0 [all] [downloadbinutils] [downloadfpc] [buildbinutils] [buildcrossfpc] [buildcrosswin32rpm]"
    exit -1
    ;;
  esac
done

# expand paths
BuildRoot=$(echo $BuildRoot | sed -e 's#//#/#g' -e 's#/$##')
Targets='i386-win32'

#===============================================================================
# download and build binutils and fpc
set -x
Params=""
if [ $DownloadBinutils = "yes" ]; then
  Params="$Params downloadbinutils"
fi
if [ $DownloadFPC = "yes" ]; then
  Params="$Params downloadfpc"
fi
if [ $BuildBinutils = "yes" ]; then
  Params="$Params buildbinutils"
fi
if [ $BuildNormalFPC = "yes" ]; then
  Params="$Params buildnormalfpc"
fi
if [ $BuildCrossFPC = "yes" ]; then
  Params="$Params buildcrossfpc"
fi

if [ "x$Params" != "x" ]; then
  Params="$Params targets=i386-win32"
  echo "calling update_cross_fpc.sh $Params ..."
  ./update_cross_fpc.sh $Params || exit
fi


#===============================================================================
# build fpc_crosswin32 rpm
if [ $BuildCrossWin32RPM = "yes" ]; then

  #----------------------------------------------------------------------------
  # retrieve the version information
  #----------------------------------------------------------------------------
  echo -n "retrieving the FPC version ..."
  VersionFile="$BuildRoot/fpc/compiler/version.pas"
  CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"

  Release=$(date +%y%m%d)
  echo " $CompilerVersionStr-$Release"

  #----------------------------------------------------------------------------
  # create temporary directory
  #----------------------------------------------------------------------------
  TmpSrcDir=$HOME/tmp/fpc_crosswin32
  echo "create temporary directory $TmpSrcDir ..."
  rm -rf $TmpSrcDir
  mkdir -p $TmpSrcDir
  
  #----------------------------------------------------------------------------
  # collect binutils
  #----------------------------------------------------------------------------
  echo "collecting binutils from $BuildRoot/binutils/cross/bin/ ..."
  BinDir=$TmpSrcDir/usr/bin/
  mkdir -p $BinDir
  MyIntel=i686
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
      NewName=$(echo $binutility | sed -e "s#^$BinUtilsPrefix##")
      NewName="fpc-${TargetCPU}-${TargetOS}-$NewName"
      cp ${BinUtilsDir}${binutility} ${BinDir}${NewName}
    done
    cd -
  done

  #----------------------------------------------------------------------------
  # collect fpc libs (e.g. .ppu/.o)
  #----------------------------------------------------------------------------
  echo "collecting fpc libs (e.g. .ppu/.o) from $BuildRoot/binutils/cross/destination/ ..."
  for Target in $Targets; do
    FPCLibDir=lib/fpc/$CompilerVersionStr/units # !!! no / at end
    
    mkdir -p $TmpSrcDir/$FPCLibDir
    cp -a $BuildRoot/binutils/cross/destination/$FPCLibDir/$Target $TmpSrcDir/$FPCLibDir/
  done

  #----------------------------------------------------------------------------
  # create default /usr/lib/fpc/fpc-cross.cfg
  #----------------------------------------------------------------------------
  touch $TmpSrcDir/lib/fpc/fpc-cross.cfg

  #----------------------------------------------------------------------------
  # copy tools (windres)
  #----------------------------------------------------------------------------
  FPCLibDir=lib/fpc/$CompilerVersionStr # !!! no / at end
  DestDir=$TmpSrcDir/$FPCLibDir
  cp -p $BuildRoot/bin/* $DestDir/

  #----------------------------------------------------------------------------
  # create tgz
  #----------------------------------------------------------------------------
  echo "creating tgz ..."
  SrcTGZ=$(../rpm/get_rpm_source_dir.sh)/SOURCES/fpc_crosswin32-$CompilerVersionStr-$Release.tar.gz

  cd $TmpSrcDir
  tar czf $SrcTGZ .
  cd -

  #----------------------------------------------------------------------------
  # change spec file
  #----------------------------------------------------------------------------
  echo "creating spec file ..."
  SpecFileTemplate=../rpm/fpc_crosswin32.spec.template
  SpecFile=fpc_crosswin32.spec
  cat $SpecFileTemplate | \
    sed -e 's/FPCVERSION/'"$CompilerVersionStr/" \
        -e 's/FPCRELEASE/'"$Release/" \
    > $SpecFile
    

  rpmbuild --nodeps -ba $SpecFile

  echo "The new rpm can be found at $(../rpm/get_rpm_source_dir.sh)/RPMS/i386/fpc_crosswin32-$CompilerVersionStr-$Release.i386.rpm"
else
  echo "To build the rpm call this script with parameter all or buildcrosswin32rpm"
fi

# end.


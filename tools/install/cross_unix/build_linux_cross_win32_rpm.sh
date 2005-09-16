#!/usr/bin/env bash
# Author: Mattias Gaertner
# License: LGPL
# Abstract: Download, compile binutils and FPC

set -e
set -x

# This is the root for all download and building directories
BuildRoot=~/freepascal

#===============================================================================
# parse command line parameters
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

#===============================================================================
# download and build binutils and fpc
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
if [ ! -z $Params ]; then
  Params="$Params targets=i386-win32"
  ./update_cross_fpc.sh $Params
fi

#===============================================================================
# build fpc_crosswin32 rpm
if [ $BuildCrossWin32RPM = "yes" ]; then
  #----------------------------------------------------------------------------
  # retrieve the version information
  #----------------------------------------------------------------------------
  VersionFile="$BuildRoot/fpc/compiler/version.pas"
  CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"

  Release=$(date +%y%m%d)

  #----------------------------------------------------------------------------
  # create source directory
  #----------------------------------------------------------------------------
  TmpDir=/tmp/fpc_patchdir
  rm -rf $TmpDir
  mkdir -p $TmpDir
  
  

  #----------------------------------------------------------------------------
  # change spec file
  #----------------------------------------------------------------------------
  SpecFileTemplate=../rpm/fpc_crosswin32.spec.template
  SpecFile=../rpm/fpc_crosswin32.spec
  cat $SpecFileTemplate | \
    sed -e 's/FPCVERSION/'"$CompilerVersionStr/" \
        -e 's/FPCRELEASE/'"$Release/" \
    > $SpecFile
    
  #----------------------------------------------------------------------------
  # compile
  #----------------------------------------------------------------------------
  rpmbuild --nodeps -ba $SpecFile

  echo "The new rpm can be found in /usr/src/redhat/RPMS/i386/"
fi

# end.


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
BuildCrossWin32DEB=no

Params=$@
for p in $Params; do
  case "$p" in
  all)
    DownloadBinutils=yes
    DownloadFPC=yes
    BuildBinutils=yes
    BuildLinWin32DEB=no
    BuildNormalDEB=no
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
  buildcrosswin32deb)
    BuildCrossWin32DEB=yes
    ;;
  *)
    echo "Unknown option: $p"
    echo
    echo "Usage:"
    echo "  $0 [all] [downloadbinutils] [downloadfpc] [buildbinutils] [buildcrossfpc] [buildcrosswin32deb]"
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
# build fpc_crosswin32 deb
if [ $BuildCrossWin32DEB = "yes" ]; then

  # check if bison is there
  fakeroot -v

  #----------------------------------------------------------------------------
  # retrieve the version information
  #----------------------------------------------------------------------------
  echo -n "retrieving the FPC version ..."
  VersionFile="$BuildRoot/fpc/compiler/version.pas"
  CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
  CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
  FPCVersion="$CompilerVersion.$CompilerRelease"
  if [ "$CompilerPatch" != "0" ]; then
    FPCVersion="$FPCVersion.$CompilerPatch"
  fi
  echo " $CompilerVersionStr-$FPCRelease"

  FPCRelease=$(date +%y%m%d)
  echo " $CompilerVersionStr-$FPCRelease"

  #------------------------------------------------------------------------------
  # setup variables

  CurDir=`pwd`
  Arch=i386
  PackageName=fpc_crosswin32
  ResourceDir=$CurDir/debian_crosswin32
  FPCBuildDir=/tmp/fpc_build
  FPCDeb=$CurDir/${PackageName}_${FPCVersion}-${FPCRelease}_$Arch.deb
  DebianInstallDir=$FPCBuildDir/usr
  DebianRulezDir=$FPCBuildDir/DEBIAN/
  DebianDocDir=$FPCBuildDir/usr/share/doc/$PackageName
  Date=`date --rfc-822`

  #----------------------------------------------------------------------------
  # create temporary directory
  #----------------------------------------------------------------------------
  echo "create temporary directory $TmpSrcDir ..."
  rm -rf $FPCBuildDir
  mkdir -p $FPCBuildDir
  
  #----------------------------------------------------------------------------
  # collect binutils
  #----------------------------------------------------------------------------
  echo "collecting binutils from $BuildRoot/binutils/cross/bin/ ..."
  BinDir=$FPCBuildDir/usr/bin/
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
      NewFileName="${TargetCPU}-${TargetOS}-$NewName"
      if [ $NewName = "windres" ]; then
        # windres needs two names:
        # - the Makefiles expects without prefix fpc-
        # - the compiler expects 'windres' with prefix fpc-
        ln -s /usr/bin/${NewFileName} ${BinDir}windres
      fi
      cp ${BinUtilsDir}${binutility} ${BinDir}${NewFileName}
    done
    cd -
  done

  #----------------------------------------------------------------------------
  # collect fpc libs (e.g. .ppu/.o)
  #----------------------------------------------------------------------------
  echo "collecting fpc libs (e.g. .ppu/.o) from $BuildRoot/binutils/cross/destination/ ..."
  for Target in $Targets; do
    FPCLibDir=lib/fpc/$CompilerVersionStr/units # !!! no / at end
    
    DestDir=$FPCBuildDir/usr/$FPCLibDir
    mkdir -p $DestDir
    cp -av $BuildRoot/binutils/cross/destination/$FPCLibDir/$Target $DestDir
  done
  
  #----------------------------------------------------------------------------
  # create default /usr/lib/fpc/fpc-cross.cfg
  #----------------------------------------------------------------------------
  touch $FPCBuildDir/usr/lib/fpc/fpc-cross.cfg
  
  #------------------------------------------------------------------------------
  # create rulez and files

  # change debian files
  mkdir -p $DebianDocDir
  mkdir -p $DebianRulezDir

  # create debian control file, which contains the package description
  echo "creating DEBIAN/control file"
  cat $ResourceDir/control | sed -e "s/FPCVERSION/$FPCVersion/g" > $DebianRulezDir/control

  # create debian changelog file, needed for version
  echo "creating usr/share/doc/fpc/changelog file ..."
  File=$DebianDocDir/changelog
  echo "$PackageName ($FPCVersion-$FPCRelease) unstable; urgency=low" > $File
  echo " -- Mattias Gaertner <mattias@freepascal.org>  $Date" >> $File
  echo "" >> $File
  cat $ResourceDir/changelog >> $File
  gzip --best $File

  # create postinst if needed
  if [ -f "$ResourceDir/postinst" ]; then
    echo "creating DEBIAN/postinst file"
    cat $ResourceDir/postinst | sed -e "s/FPCVERSION/$FPCVersion/g" > $DebianRulezDir/postinst
    chmod a+x $DebianRulezDir/postinst
  fi

  # create postrm if needed
  if [ -f "$ResourceDir/postrm" ]; then
    echo "creating DEBIAN/postrm file"
    cat $ResourceDir/postrm | sed -e "s/FPCVERSION/$FPCVersion/g" > $DebianRulezDir/postrm
    chmod a+x $DebianRulezDir/postrm
  fi

  # create changelog.Debian file
  echo "creating changelog.Debian file ..."
  File=$DebianDocDir/changelog.Debian
  cp $ResourceDir/changelog.Debian $File
  gzip --best $File

  # create debian copyright file
  echo "creating copyright file ..."
  cp $ResourceDir/copyright $DebianDocDir/

  #------------------------------------------------------------------------------
  # creating deb

  cd $FPCBuildDir
  fakeroot dpkg-deb --build $FPCBuildDir
  mv $FPCBuildDir.deb $FPCDeb

  echo "The new deb can be found at $FPCDeb"
  echo "You can test it with lintian."
else
  echo "To build the deb call this script with parameter all or buildcrosswin32deb"
fi

# end.


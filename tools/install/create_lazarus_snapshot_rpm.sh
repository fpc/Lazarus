#!/usr/bin/env bash

set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 <LazarusSrcDir> <FPCFullVersion>  [append-revision]"

LazSrcDir=$1
shift
if [ "x$LazSrcDir" = "x" ]; then
  echo $Usage
  exit -1
fi

if [ ! -d $LazSrcDir/lcl ]; then
  echo "The directory $LazSrcDir does not look like a lazarus source directory"
  exit -1
fi

if [ ! -d $LazSrcDir/.svn ]; then
  echo "The directory $LazSrcDir does not look like a svn working directory"
  exit -1
fi

FpcFullVersion=$1
shift
if [ "x$FpcFullVersion" = "x" ]; then
  echo $Usage
  exit -1
fi

cd $(dirname $0)
if [ "$1" = "append-revision" ]; then
  LazVersionPostfix=$(./get_svn_revision_number.sh $LazSrcDir)
  if [ -n "$LazVersionPostfix" ]; then
    LazVersionPostfix=.$LazVersionPostfix
  fi
  shift
fi

Date=`date +%Y%m%d`
# get fpc snapshot rpm
RPMDIR=$(rpm/get_rpm_source_dir.sh)
ARCH=`rpm --eval "%{_arch}"`
LIB=`rpm --eval "%{_lib}"`
FPCRPM=$RPMDIR/RPMS/$ARCH/fpc-$FpcFullVersion-$Date.$ARCH.rpm
if [ ! -f $FPCRPM ]; then
  echo ERROR: fpc rpm $FPCRPM not available
  exit
fi

# extract the fpc snapshot, so we don't need to install it
TmpFPCDir=~/tmp/fpc
if [ -e $TmpFPCDir ]; then
  rm -rf $TmpFPCDir
fi 
mkdir -p $TmpFPCDir
cd $TmpFPCDir
rpm2cpio $FPCRPM | cpio -id 
FPCVersion=`usr/bin/fpc -iV`
FPCFullVersion=`usr/bin/fpc -iW`
usr/$LIB/fpc/$FPCVersion/samplecfg $TmpFPCDir/usr/$LIB/fpc/$FPCVersion .
FPCCfg=$TmpFPCDir/fpc.cfg
export FPCCfg
FPC=$TmpFPCDir/usr/bin/fpc
export FPC
FPCDIR=$TmpFPCDir/usr/$LIB/fpc/$FPCVersion
export FPCDIR
cd -

# create a temporary copy of the lazarus sources for packaging
LazVersion=$(./get_lazarus_version.sh)$LazVersionPostfix
LazRelease=`echo $FPCRPM | sed -e 's/-/_/g'`
TmpLazDir=~/tmp/lazarus

rm -rf $TmpLazDir
echo "extracting Lazarus source from local svn ..."
svn export $LazSrcDir $TmpLazDir
if [ ! -e ../svn2revisioninc ]; then
  make -C ../.. tools OPT="-n @$FPCCfg"
fi
../svn2revisioninc $LazSrcDir $TmpLazDir/ide/revision.inc

# create a source tar.gz
cd $TmpLazDir/..
tar -czf $RPMDIR/SOURCES/lazarus-$LazVersion-$Date.tar.gz lazarus

# remove the tempdir
cd -
rm -rf $TmpLazDir

# create spec file
SpecFile=$RPMDIR/SPECS/lazarus-$LazVersion-$Date.spec
cat rpm/lazarus.spec.template | \
  sed -e "s/LAZVERSION/$LazVersion/g" \
      -e "s/LAZRELEASE/$Date/g" \
      -e "s/FPCVERSION/$FPCFullVersion/g" \
      -e "s/FPCSRCVERSION/$FPCFullVersion/g" \
  > $SpecFile

# build rpm
rpmbuild --target $ARCH -ba $SpecFile --nodeps

# remove the temp fpc dir
rm -rf $TmpFPCDir


# end.


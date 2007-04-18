#!/usr/bin/env bash

set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 <LazarusSrcDir>"

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

Date=`date +%Y%m%d`
# get fpc snapshot rpm
RPMDIR=$(rpm/get_rpm_source_dir.sh)
ARCH=`rpm --eval "%{_arch}"`
LIB=`rpm --eval "%{_lib}"`
FPCRPM=$RPMDIR/RPMS/$ARCH/fpc-2.0.4-$Date.$ARCH.rpm
if [ ! -f $FPCRPM ]; then
  echo ERROR: fpc rpm $FPCRPM not available
  exit
fi

User=`whoami`
TmpFPCDir=/tmp/$User/fpc
if [ -e $TmpFPCDir ]; then
  rm -rf $TmpFPCDir
fi 
mkdir -p $TmpFPCDir
cd $TmpFPCDir
rpm2cpio $FPCRPM | cpio -id 
FPCVersion=`usr/bin/fpc -iV`
usr/$LIB/fpc/$FPCVersion/samplecfg $TmpFPCDir/usr/$LIB/fpc/$FPCVersion .
FPCCfg=$TmpFPCDir/fpc.cfg
export FPCCfg
FPC=$TmpFPCDir/usr/bin/fpc
export FPC
FPCDIR=$TmpFPCDir/usr/$LIB/fpc/$FPCVersion
export FPCDIR
cd -

# create a temporary copy of the lazarus sources for packaging
LazVersion=$(./get_lazarus_version.sh)
LazRelease=`echo $FPCRPM | sed -e 's/-/_/g'`
TmpDir=/tmp/`whoami`/lazarus

rm -rf $TmpDir
echo "extracting Lazarus source from local svn ..."
svn export $LazSrcDir $TmpDir
if [ ! -e ../svn2revisioninc ]; then
  make -C ../.. tools OPT="-n @$FPCCfg"
fi
../svn2revisioninc $LazSrcDir $TmpDir/ide/revision.inc

# create a source tar.gz
cd $TmpDir/..
tar -czf $RPMDIR/SOURCES/lazarus-$LazVersion-$Date.tar.gz lazarus

# remove the tempdir
cd -
rm -rf $TmpDir

# create spec file
SpecFile=$RPMDIR/SPECS/lazarus-$LazVersion-$Date.spec
cat rpm/lazarus.spec.template | \
  sed -e "s/LAZVERSION/$LazVersion/g" \
      -e "s/LAZRELEASE/$Date/g" \
      -e "s/FPCVERSION/$FPCVersion/g" \
      -e "s/FPCSRCVERSION/$FPCVersion/g" \
  > $SpecFile

# build rpm
rpmbuild --target $ARCH -ba $SpecFile --nodeps

rm -rf $TmpFpcDir

# end.


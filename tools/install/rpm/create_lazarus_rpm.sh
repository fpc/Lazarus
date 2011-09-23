#!/usr/bin/env bash
# note: this script uses Fedora mock to build the lazarus rpm
# in a chroot environment

set -x
set -e
set -u

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 <LazarusSrcDir> <FPCFullVersion>  [append-revision] [snapshot]"

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

FPCFullVersion=$1
shift
if [ "x$FPCFullVersion" = "x" ]; then
  echo $Usage
  exit -1
fi

cd $(dirname $0)
if [ "$1" = "append-revision" ]; then
  LazVersionPostfix=$(../get_svn_revision_number.sh $LazSrcDir)
  if [ -n "$LazVersionPostfix" ]; then
    LazVersionPostfix=.$LazVersionPostfix
  fi
  shift
else
LazVersionPostfix=
fi

LazVersion=$(../get_lazarus_version.sh)$LazVersionPostfix
if [ "$1" = "snapshot" ]; then
  LazRelease=`date +%Y%m%d`
else
  LazRelease=0
fi

RPMDIR=$(./get_rpm_source_dir.sh)
RPMARCH=$(rpm --eval "%{_arch}")
MOCKCONFIG=fedora-15-$RPMARCH

FPCRPM=$RPMDIR/RPMS/$RPMARCH/fpc-$FPCFullVersion-0.$RPMARCH.rpm
if [ ! -f $FPCRPM ]; then
  echo ERROR: fpc rpm $FPCRPM not available
  exit
fi

LAZSPEC=$RPMDIR/SPECS/lazarus-$LazVersion-$LazRelease.spec
Src=lazarus-$LazVersion-$LazRelease.tar.gz
SrcTGZ=$RPMDIR/SOURCES/$Src

#create lazarus spec file
cat lazarus.spec.template | \
  sed -e "s/LAZVERSION/$LazVersion/g" \
      -e "s/LAZRELEASE/$LazRelease/g" \
      -e "s/FPCVERSION/$FPCFullVersion/g" \
      -e "s/FPCSRCVERSION/$FPCFullVersion/g" \
  > $LAZSPEC

#create lazarus source tgz
../create_lazarus_export_tgz.sh $SrcTGZ

#init mock environment
mock -r $MOCKCONFIG --init

#install fpc in mock
mock -r $MOCKCONFIG --install $FPCRPM  

#build lazarus source rpm
rpmbuild --verbose -bs --nodeps $LAZSPEC

#build lazarus rpm
mock -r $MOCKCONFIG --no-clean --installdeps --rebuild $RPMDIR/SRPMS/lazarus-$LazVersion-$LazRelease.src.rpm

#copy result into non-root rpm repository
cp /var/lib/mock/$MOCKCONFIG/result/*.src.rpm $RPMDIR/SRPMS/
cp /var/lib/mock/$MOCKCONFIG/result/*.$RPMARCH.rpm $RPMDIR/RPMS/$RPMARCH/

echo "The new rpm can be found at $RPMDIR/RPMS/$RPMARCH/lazarus-$LazVersion-$LazRelease.$RPMARCH.rpm"



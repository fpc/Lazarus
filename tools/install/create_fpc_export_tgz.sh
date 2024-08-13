#!/bin/bash
#
#

#set -x
set -e

Usage="$0 download|<fpc-source-directory> <outputfilename>"

SourceDir=$1;
Download=no
if [ "x$SourceDir" = "xdownload" ]; then
  Download=yes
fi
shift

OutputFile=$1
if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit
fi

TmpDir=$TEMP
if [ -z "$TmpDir" ]; then
  TmpDir=~/tmp
fi
TmpFPCDir=$TmpDir/fpc
FPCTGZ=$TmpDir/fpc.tgz
if [ "x$Download" = "xyes" ]; then
  echo "downloading fpc svn ..."
  rm -rf $TmpFPCDir
  mkdir -p $TmpDir
  cd $TmpDir
  git clone https://gitlab.com/freepascal.org/fpc/build.git $TmpFPCDir
  cd -
else
  echo "extracting fpc from local git ..."
  rm -rf $TmpFPCDir
  mkdir -p $TmpFPCDir
  cp -a $SourceDir/.git $TmpFPCDir/
  git -C $TmpFPCDir restore .
  rm -rf $TmpFPCDir/.git*
fi

cd $TmpDir
echo "packing ..."
tar cvzf $FPCTGZ fpc
cd -
mv $FPCTGZ $OutputFile
rm -rf $TmpFPCDir

# end.


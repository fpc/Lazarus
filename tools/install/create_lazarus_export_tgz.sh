#!/bin/bash

#set -x
set -e

OutputFile=$1
Usage="$0 [download] outputfilename"
TmpDir=~/tmp

Download=no
if [ "x$1" = "xdownload" ]; then
  Download=yes
  shift
fi

if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit
fi

TmpLazDir=$TmpDir/lazarus
mkdir -p $TmpDir
rm -rf $TmpLazDir
if [ "x$Download" = "xyes" ]; then
  echo "downloading lazarus svn ..."
  mkdir -p $TmpLazDir
  Revision=Exported
  cd $TmpDir
  svn export http://svn.freepascal.org/svn/lazarus/trunk $TmpLazDir
  cd -
else
  echo "extracting lazarus from local svn ..."
  SourceDir=$(pwd | sed -e 's#/tools/install.*$##')
  Revision=$(svnversion $SourceDir)
  cd $TmpDir
  svn export $SourceDir $TmpLazDir
  cd -
fi

# add ide/revision.inc
echo "const RevisionStr = '$Revision';" > $TmpLazDir/ide/revision.inc

cd $TmpDir
echo "packing ..."
tar cvzf lazarus.tgz lazarus
cd -
mv $TmpDir/lazarus.tgz $OutputFile
rm -rf $TmpLazDir

# end.


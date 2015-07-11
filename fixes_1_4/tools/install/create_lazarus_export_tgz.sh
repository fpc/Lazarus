#!/bin/bash
#
# Author: Mattias Gaertner
#
# Usage: ./create_lazarus_export_tgz.sh [chmhelp] [download] outputfilename
#
#   Options:
#     chmhelp    add chm,kwd files in docs/chm
#     download   download instead of using the current files
#

Download=
UseCHMHelp=
OutputFile=
TmpDir=~/tmp

LastParam=
while [ $# -gt 0 ]; do
  echo "param=$1"
  case "$1" in
  chmhelp)
    echo "using files in docs/chm"
    UseCHMHelp=1
    ;;
  download)
    Download=yes
    ;;

  *)
    if [ -n "$OutputFile" ]; then
        echo "invalid parameter $LastParam"
	exit 1
    fi
    OutputFile=$1
    ;;
  esac
  LastParam=$1
  shift
done

set -e

if [ "x$OutputFile" = "x" ]; then
  echo "Usage: ./create_lazarus_export_tgz.sh [chmhelp] [download] outputfilename"
  exit 1
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
  LazSrcDir=$(pwd | sed -e 's#/tools/install.*$##')
  Revision=$(svnversion $LazSrcDir)
  cd $TmpDir
  svn export $LazSrcDir $TmpLazDir
  cd -
  if [ "$UseCHMHelp" = "1" ]; then
    echo
    echo "Copying chm files"
    cd $LazSrcDir/docs/chm
    cp -v *.kwd *.chm $TmpLazDir/docs/chm/
    cd -
  fi
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


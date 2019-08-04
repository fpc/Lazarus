#!/bin/bash
#
# Author: Mattias Gaertner
#
# Usage: ./create_lazarus_export_tgz.sh [chmhelp] [download] outputfilename.tgz
#
#   Options:
#     download   download instead of using the current files
#     chmhelp    add chm,kwd files in docs/chm
#     pas2jszip <pas2js-linux-version.zip>
#                       unzip pas2js release zip to "pas2js/version"
#

Download=
UseCHMHelp=
OutputFile=
TmpDir=~/tmp
Pas2jsZip=

LastParam=
while [ $# -gt 0 ]; do
  echo "param=$1"
  case "$1" in
  download)
    Download=yes
    ;;
  chmhelp)
    echo "using files in docs/chm"
    UseCHMHelp=1
    ;;
  pas2jszip)
    shift
    echo "param=$1"
    Pas2jsZip=$1
    Pattern="*pas2js*.zip"
    if [[ $Pas2jsZip == $Pattern ]]; then
      echo "using pas2js zip file $Pas2jsZip"
    else
      echo "invalid pas2js zip file $Pas2jsZip"
      exit -1
    fi
    if [ ! -f $Pas2jsZip ]; then
      echo "missing pas2js zip file $Pas2jsZip"
      exit -1
    fi
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
  echo "Usage: ./create_lazarus_export_tgz.sh [chmhelp] [download] outputfilename.tgz"
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

if [ ! "x$Pas2jsZip" = "x" ]; then
  # unzip pas2jszip to pas2js/version
  mkdir $TmpLazDir/pas2js   # fails if already there -> good
  unzip $Pas2jsZip -d $TmpLazDir/pas2js
  Pas2jsBin="$TmpLazDir/pas2js/*pas2js*/bin/pas2js"
  if [ ! -f $Pas2jsBin ]; then
    echo "missing $Pas2jsZip/*pas2js*/bin/pas2js"
    exit 1
  fi
  Pas2jsVer=$($Pas2jsBin -iV | tr -d '\n')
  mv $TmpLazDir/pas2js/*pas2js* $TmpLazDir/pas2js/$Pas2jsVer
fi

# add ide/revision.inc
echo "const RevisionStr = '$Revision';" > $TmpLazDir/ide/revision.inc

cd $TmpDir
echo "packing ..."
tar cvzf lazarus.tgz lazarus
cd -
mv $TmpDir/lazarus.tgz $OutputFile
echo "Created $OutputFile"
rm -rf $TmpLazDir

# end.


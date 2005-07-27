#!/usr/bin/env bash

set -x
set -e

LazSrcDir=$1
OutputFile=$2

if [ "x$LazSrcDir" = "x" ]; then
  echo "Usage: $0 <lazarus_source_directory> <output-file>"
  exit
fi
if [ ! -d $LazSrcDir/designer ]; then
  echo "The directory does not look like a lazarus source directory (lazarus/)"
  exit
fi

if [ "x$OutputFile" = "x" ]; then
  echo "Usage: $0 <lazarus_source_directory> <output-file>"
  exit -1
fi


TmpBaseDir=/tmp
TmpDir=$TmpBaseDir/lazarus

ppc386 -Fu../../lcl/units/i386/linux cvsexportlocal.pas
echo "extracting Lazarus from local cvs ..."
rm -rf $TmpDir
./cvsexportlocal $LazSrcDir $TmpDir

# pack
echo "packing ..."
cd /tmp
tar cvzf lazarus.tgz lazarus
cd -
mv /tmp/lazarus.tgz $OutputFile
rm -rf /tmp/lazarus

echo ""
echo "NOTE: DON'T FORGET TO PUT THE $OutFile INTO /usr/src/redhat/SOURCES/"

# end.


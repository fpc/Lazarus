#!/usr/bin/env bash

set -x
set -e

LazSrcDir=$1
OutFile=lazarus-0.9.1-5.tgz

if [ "x$LazSrcDir" = "x" ]; then
  echo "Usage: $0 <lazarus_source_directory>"
  exit
fi
if [ ! -d $LazSrcDir/designer ]; then
  echo "The directory does not look like a lazarus source directory (lazarus/)"
  exit
fi

echo "copy $LazSrcDir to /tmp/lazarus ..."
cd /tmp
rm -rf /tmp/lazarus
cd -
cp -a $LazSrcDir /tmp/lazarus

sh clean_lazaru_directory.sh $LazSrcDir

# pack
echo "packing ..."
cd /tmp/
tar cvzf lazarus.tgz lazarus
cd -
mv /tmp/lazarus.tgz $OutFile
rm -rf /tmp/lazarus

echo ""
echo "NOTE: DON'T FORGET TO PUT THE $OutFile INTO /usr/src/redhat/SOURCES/"

# end.


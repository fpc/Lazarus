#!/bin/bash

set -x
set -e

LazSrcDir=$1
OutFile=lazarus-0.8.5-1.tgz

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

echo "cleaning up (CVS, ppu, o) ..."
cd /tmp/lazarus
make cleanall
find . -name '*.ppu' -exec rm -rf {} \;
find . -name '*.o' -exec rm -rf {} \;
find . -name '*.rst' -exec rm -rf {} \;
rm -rf tools/install/*.tgz
cd -

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


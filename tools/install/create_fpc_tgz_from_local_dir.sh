#!/bin/bash

set -e
set -x

FPCSrcDir=$1
OutputFile=fpcsrc-1.0.7-1.tgz

if [ "x$FPCSrcDir" = "x" ]; then
  echo "Usage: $0 <fpc_source_directory>"
  exit
fi
if [ ! -d $FPCSrcDir/compiler ]; then
  echo "The directory does not look like a fpc source directory (fpc/)"
  exit
fi

echo "copy dir to /tmp/fpc ..."
cd /tmp
rm -rf /tmp/fpc
cp -a $FPCSrcDir fpc
cd -

echo "cleaning up (CVS, ppu, o) ..."
cd /tmp/fpc
make distclean
find . -name '*.ppu' -exec rm -rf {} \;
find . -name '*.o' -exec rm -rf {} \;
if [ -d CVS ]; then
  find . -name 'CVS' | xargs rm -r
fi
cd -

# pack
echo "creating tgz ..."
cd /tmp
tar czf fpc_src.tgz fpc
cd -
mv /tmp/fpc_src.tgz $OutputFile
rm -rf /tmp/fpc

echo ""
echo "NOTE: DON'T FORGET TO PUT THE $OutputFile INTO /usr/src/redhat/SOURCES/"


# end.


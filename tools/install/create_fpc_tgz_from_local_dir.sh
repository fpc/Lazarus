#!/bin/bash

set -e
#set -x

FPCSrcDir=$1
OutputFile=$2

fpcsrc-1.0.7-1.tgz

Usage="Usage: $0 <fpc_source_directory> <outputfile>"

if [ "x$FPCSrcDir" = "x" ]; then
  echo $Usage
  exit
fi

if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit
fi

if [ ! -d $FPCSrcDir/compiler ]; then
  echo "The directory $FPCSrcDir does not look like a fpc source directory (fpc/)"
  exit
fi

echo "copy $FPCSrcDir to /tmp/fpc ..."
cd /tmp
rm -rf /tmp/fpc
cd -
cp -a $FPCSrcDir /tmp/fpc

echo "cleaning up (CVS, ppu, o) ..."
cd /tmp/fpc
make distclean
find . -name '*.ppu' -exec rm -rf {} \;
find . -name '*.rst' -exec rm -rf {} \;
rm -f *.tar.gz
if [ -d CVS ]; then
  find . -name 'CVS' | xargs rm -r
fi
cd -
cd /tmp/fpc/docs
make clean
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


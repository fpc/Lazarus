#!/bin/bash

set -e
set -x

FPCSrcDir=$1
OutputFile=$2

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

TmpBaseDir=/tmp
TmpDir=$TmpBaseDir/fpc

echo "copy $FPCSrcDir to /tmp/fpc ..."
cd $TmpBaseDir
rm -rf $TmpDir
cd -
cp -a $FPCSrcDir $TmpDir

echo "cleaning up (CVS, ppu, o) ..."
cd $TmpDir
make distclean

for Ext in ppu ppw ppl o ow rst cvsignore bak; do
  find . -name "*.$Ext" -exec rm -f {} \;
done
find . -name "*.~*" -exec rm -f {} \;
rm -f *.tar.gz
if [ -d CVS ]; then
  # use xargs to remove directories (otherwise find returns strange things)
  find . -name 'CVS' | xargs rm -r
fi
cd -
# clean up docs
cd $TmpDir/docs
make clean
cd -

# pack
echo "creating tgz ..."
cd $TmpBaseDir
tar czf fpc_src.tgz fpc
cd -
mv $TmpBaseDir/fpc_src.tgz $OutputFile
rm -rf $TmpDir


# end.


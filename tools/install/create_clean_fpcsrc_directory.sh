#!/usr/bin/env bash

set -x
set -e

SrcDir=$1
DestDir=$2

SrcDir=$(echo $SrcDir | sed -e 's#//#/#' -e 's#/$##')/
DestDir=$(echo $DestDir | sed -e 's#//#/#' -e 's#/$##')/

if [ "x$SrcDir" = "x" ]; then
  echo "Usage: $0 <fpc_source_directory> <fpc_destination_directory>"
  exit
fi
if [ "x$DestDir" = "x" ]; then
  echo "Usage: $0 <fpc_source_directory> <fpc_destination_directory>"
  exit
fi
if [ "x$DestDir" = "x/" ]; then
  echo "Usage: $0 <fpc_source_directory> <fpc_destination_directory>"
  exit
fi
if [ ! -d $SrcDir/compiler ]; then
  echo "The directory does not look like a fpc source directory (lazarus/)"
  exit
fi

echo "copy $SrcDir to $DestDir ..."
if [ `which rsync` ]; then
  rsync -va --exclude="CVS" --exclude=".cvsignore" \
        --exclude="*.ppu" --exclude="*.ppw" --exclude="*.ppl" \
        --exclude="*.o" --exclude="*.ow" --exclude="*.rst" \
        --exclude=".#*" --exclude="*.~*" --exclude="*.bak" \
        --exclude="*.orig" --exclude="*.rej" --exclude="*.bak" \
        --exclude=".xvpics" --exclude="*.compiled" --exclude="killme*" \
        --exclude=".gdb_hist*" \
        $SrcDir $DestDir
else
  cp -a $SrcDir $DestDir
fi

echo "cleaning up (svn, ppu, o) ..."
cd $DestDir
make distclean
for Ext in ppu ppw ppl o ow rst cvsignore bak orig rej xvpics; do
  find . -name "*.$Ext" -exec rm -f {} \;
done
find . -name "*.~*" -exec rm -f {} \;
find . -name "*.#*" -exec rm -f {} \;
# delete all CVS directories
find . -name "CVS" -exec rm -rf {} \;
# delete all .svn directories
find . -name ".svn" -exec rm -rf {} \;
# delete all executables
find . -perm +a+x -type f -exec rm -f {} \;
cd -

# end.


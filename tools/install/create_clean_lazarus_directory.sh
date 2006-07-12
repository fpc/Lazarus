#!/usr/bin/env bash

set -x
set -e

LazSrcDir=$1
LazDestDir=$2

LazSrcDir=$(echo $LazSrcDir | sed -e 's#//#/#' -e 's#/$##')/
LazDestDir=$(echo $LazDestDir | sed -e 's#//#/#' -e 's#/$##')/

if [ "x$LazSrcDir" = "x" ]; then
  echo "Usage: $0 <lazarus_source_directory> <lazarus_destination_directory>"
  exit
fi
if [ "x$LazDestDir" = "x" ]; then
  echo "Usage: $0 <lazarus_source_directory> <lazarus_destination_directory>"
  exit
fi
if [ "x$LazDestDir" = "x/" ]; then
  echo "Usage: $0 <lazarus_source_directory> <lazarus_destination_directory>"
  exit
fi
if [ ! -d $LazSrcDir/designer ]; then
  echo "The directory does not look like a lazarus source directory (lazarus/)"
  exit
fi

echo "copy $LazSrcDir to $LazDestDir ..."
if [ `which rsync` ]; then
  rsync -va --exclude="CVS" --exclude=".cvsignore" \
        --exclude="*.ppu" --exclude="*.ppw" --exclude="*.ppl" \
        --exclude="*.o" --exclude="*.ow" --exclude="*.a" --exclude="*.rst" \
        --exclude=".#*" --exclude="*.~*" --exclude="*.bak" \
        --exclude="*.orig" --exclude="*.rej" --exclude="*.bak" \
        --exclude=".xvpics" --exclude="*.compiled" --exclude="killme*" \
        --exclude=".gdb_hist*" \
        $LazSrcDir $LazDestDir
else
  cp -a $LazSrcDir $LazDestDir
fi

echo "cleaning up (svn, ppu, o) ..."
cd $LazDestDir
make clean
make -C examples clean
make -C tools clean
for Ext in ppu ppw ppl o ow rst cvsignore bak orig rej xvpics; do
  find . -name "*.$Ext" -exec rm -f {} \;
done
find . -name "*.~*" -exec rm -f {} \;
find . -name "*.#*" -exec rm -f {} \;
# delete all CVS directories
find . -name "CVS" -exec rm -rf {} \;
# delete all SVN directories
find . -name ".svn" -exec rm -rf {} \;
# delete all executables
find . -perm +a+x -type f -exec rm -f {} \;
rm -rf tools/install/*.tgz
rm -rf tools/install/*.*.spec
cd -


# end.


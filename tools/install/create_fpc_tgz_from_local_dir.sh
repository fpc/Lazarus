#!/bin/bash

set -e
set -x

FPCSrcDir=$1
OutputFile=$2

FPCSrcDir=$(echo $FPCSrcDir | sed -e 's#//#/#' -e 's#/$##')/
OutputFile=$(echo $OutputFile | sed -e 's#//#/#' -e 's#/$##')

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
sh create_clean_fpcsrc_directory.sh $FPCSrcDir $TmpDir

# pack
echo "creating tgz ..."
cd $TmpBaseDir
tar czf fpc_src.tgz fpc
cd -
mv $TmpBaseDir/fpc_src.tgz $OutputFile
rm -rf $TmpDir


# end.


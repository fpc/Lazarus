#!/bin/bash

set -e
set -x

RenameSmart=no
if [ "x$1" = "xrenamesmart" ]; then
  RenameSmart=yes
  shift
fi

FPCSrcDir=$1
OutputFile=$2

FPCSrcDir=$(echo $FPCSrcDir | sed -e 's#//#/#' -e 's#/$##')/
OutputFile=$(echo $OutputFile | sed -e 's#//#/#' -e 's#/$##')

Usage="Usage: $0 [renamesmart] <fpc_source_directory> <outputfile>"

if [ "x$FPCSrcDir" = "x" ]; then
  echo $Usage
  exit -1
fi

if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit -1
fi

if [ ! -d $FPCSrcDir/compiler ]; then
  echo "The directory $FPCSrcDir does not look like a fpc source directory (fpc/)"
  exit -1
fi

TmpBaseDir=/tmp
TmpDir=$TmpBaseDir/fpc

ppc386 -Fu../../lcl/units/i386/linux cvsexportlocal.pas
echo "extracting FPC from local cvs ..."
rm -rf $TmpDir
./cvsexportlocal $FPCSrcDir $TmpDir

if [ $RenameSmart = "yes" ]; then
  for Ext in pm pl; do
    find $TmpDir -name "*.$Ext" -exec mv {} {}.renamed \;
  done
fi

# pack
echo "creating tgz ..."
cd $TmpBaseDir
tar czf fpc_src.tgz fpc
cd -
mv $TmpBaseDir/fpc_src.tgz $OutputFile
rm -rf $TmpDir


# end.


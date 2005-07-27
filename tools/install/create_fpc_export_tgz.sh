#!/bin/bash

#set -x
set -e

Usage="$0 "'outputfilename devel or stable [cvs date mm/dd/yy]'

OutputFile=$1
FPCVersion=$2
FPCDate=$3
TempDir=/tmp/fpc_cvs_export/

FPCVersionOk=no
for ver in devel stable; do
  if [ "x$FPCVersion" = "x$ver" ]; then
    FPCVersionOk=yes
  fi
done
if [ "x$FPCVersionOk" = "xno" ]; then
  echo $Usage
  exit -1
fi

if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit -1
fi
if [ "x$FPCDate" = "x" ]; then
  FPCDate='NOW'
fi


echo downloading cvs $CVSParams ...
mkdir -p $TempDir
cd $TempDir
rm -rf fpc
export CVS_RSH=
export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
echo 'The password is: cvs'
cvs login
cvs -z3 export -D $FPCDate fpc
if [ "x$FPCVersion" = "xstable" ]; then
  Dirs='compiler rtl logs install'
  for dir in $Dirs; do
    rm -rf fpc/$dir
    cvs -z3 export -r FIXES_1_0_0 -D $FPCDate fpc/$dir
    echo
  done
fi
cd -

# pack
cd $TempDir
echo 'creating tgz ...'
tar czf fpc_src.tgz fpc
cd -
mv $TempDir/fpc_src.tgz $OutputFile
rm -rf $TempDir

# end.


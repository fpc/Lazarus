#!/bin/bash

#set -x
set -e

OutputFile=$1
Usage="$0 [download] outputfilename"

Download=no
if [ "x$1" = "xdownload" ]; then
  Download=yes
  shift
fi

if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit
fi

if [ "x$Download" = "xyes" ]; then
  echo "downloading lazarus cvs ..."
  cd /tmp
  rm -rf /tmp/lazarus
  export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
  cvs login
  cvs -z3 export -r HEAD lazarus
  cd -
else
  echo "building cvsexportlocal ..."
  ppc386 -Fu../../lcl/units/i386/linux cvsexportlocal.pas
  echo "extracting lazarus from local cvs ..."
  cd ../..
  SourceDir=`pwd`
  cd -
  rm -rf /tmp/lazarus
  ./cvsexportlocal $SourceDir /tmp/lazarus
fi

cd /tmp
echo "packing ..."
tar cvzf lazarus.tgz lazarus
cd -
mv /tmp/lazarus.tgz $OutputFile
rm -rf /tmp/lazarus

# end.


#!/bin/bash

#set -x
set -e

OutputFile=$1
shift
CVSParams=$@

Usage="$0 outputfilename [cvs params]"

if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit
fi
if [ "x$CVSParams" = "x" ]; then
  CVSParams="-r FIXES_1_0_0"
fi

echo downloading cvs $CVSParams ...
cd /tmp
rm -rf /tmp/fpc
export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
echo "The password is: cvs"
cvs login
cvs -z3 export $CVSParams fpc

# pack
echo "creating tgz ..."
tar czf fpc_src.tgz fpc
cd -
mv /tmp/fpc_src.tgz $OutputFile
rm -rf /tmp/fpc

# end.


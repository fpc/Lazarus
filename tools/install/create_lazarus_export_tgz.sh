#!/bin/bash

#set -x
set -e

OutputFile=$1
Usage="$0 outputfilename"

if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit
fi

echo "downloading lazarus cvs ..."
cd /tmp
rm -rf /tmp/lazarus
export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
cvs login
cvs -z3 export -r HEAD lazarus

echo "packing ..."
tar cvzf lazarus.tgz lazarus
cd -
mv /tmp/lazarus.tgz $OutputFile
rm -rf /tmp/lazarus

# end.


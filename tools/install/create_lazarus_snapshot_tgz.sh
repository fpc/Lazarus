#!/bin/bash

#set -x
set -e

OutFile=lazarus-0.8.5-1.tgz

echo "downloading lazarus cvs ..."
cd /tmp
rm -rf /tmp/lazarus
export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
cvs login
cvs -z3 export -r HEAD lazarus

echo "packing ..."
tar cvzf lazarus.tgz lazarus
cd -
mv /tmp/lazarus.tgz $OutFile
rm -rf /tmp/lazarus

echo ""
echo "NOTE: DON'T FORGET TO PUT THE $OutFile INTO /usr/src/redhat/SOURCES/"

# end.


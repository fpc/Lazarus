#!/bin/bash

#set -x
set -e

cd /tmp
rm -rf /tmp/lazarus
export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
cvs login
cvs -z3 export -r HEAD lazarus
tar cvzf lazarus.tgz lazarus
cd -
mv /tmp/lazarus.tgz lazarus-0.8.5.tgz
rm -rf /tmp/lazarus

echo ""
echo "NOTE: DON'T FORGET TO PUT THE .tgz INTO /usr/src/redhat/SOURCES/"

# end.


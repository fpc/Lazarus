#!/bin/bash

#set -x
set -e

cd /tmp
rm -rf /tmp/gtk2
export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
cvs login
cvs -z3 export -r FIXES_1_0_0 fpc
tar cvzf fpc_src.tgz fpc
cd -
mv /tmp/fpc_src.tgz fpcsrc-1.0.7.tgz
rm -rf /tmp/fpc

echo ""
echo "NOTE: DON'T FORGET TO PUT THE .tgz INTO /usr/src/redhat/SOURCES/"

# end.


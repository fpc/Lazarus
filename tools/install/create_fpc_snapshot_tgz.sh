#!/bin/bash

#set -x
set -e

OutputFile=fpcsrc-1.0.7-1.tgz

echo "downloading fpc branch cvs FIXES_1_0_0 ..."
cd /tmp
rm -rf /tmp/fpc
export CVSROOT=:pserver:cvs@cvs.freepascal.org:/FPC/CVS
cvs login
cvs -z3 export -r FIXES_1_0_0 fpc

# pack
echo "creating tgz ..."
tar czf fpc_src.tgz fpc
cd -
mv /tmp/fpc_src.tgz $OutputFile
rm -rf /tmp/fpc

echo ""
echo "NOTE: DON'T FORGET TO PUT THE $OutputFile INTO /usr/src/redhat/SOURCES/"

# end.


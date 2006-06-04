#!/bin/bash
#
#

#set -x
set -e

Usage="$0 download|<fpc-source-directory> <outputfilename>"

SourceDir=$1;
Download=no
if [ "x$SourceDir" = "xdownload" ]; then
  Download=yes
fi
shift

OutputFile=$1
if [ "x$OutputFile" = "x" ]; then
  echo $Usage
  exit
fi

if [ "x$Download" = "xyes" ]; then
  echo "downloading fpc svn ..."
  cd /tmp
  rm -rf /tmp/fpc
  svn export http://svn.freepascal.org/svn/fpc/trunk fpc
  cd -
else
  echo "extracting fpc from local svn ..."
  rm -rf /tmp/fpc
  svn export $SourceDir /tmp/fpc
fi

cd /tmp
echo "packing ..."
tar cvzf /tmp/fpc.tgz fpc
cd -
mv /tmp/fpc.tgz $OutputFile
rm -rf /tmp/fpc

# end.


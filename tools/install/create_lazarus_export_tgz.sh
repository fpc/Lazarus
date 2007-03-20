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
  echo "downloading lazarus svn ..."
  cd /tmp
  rm -rf /tmp/lazarus
  svn export http://svn.freepascal.org/svn/lazarus/trunk lazarus
  cd -
else
  echo "extracting lazarus from local svn ..."
  SourceDir=$(pwd | sed -e 's#lazarus[_0-9]*/tools.*$#lazarus#')
  rm -rf /tmp/lazarus
  svn export $SourceDir /tmp/lazarus
fi

# add ide/revision.inc
Revision=$(cat /tmp/lazarus/.svn/entries | grep committed-rev= | head -n 1 | cut -d\" -f2)
echo "const RevisionStr = '$Revision';" > /tmp/lazarus/ide/revision.inc

cd /tmp
echo "packing ..."
tar cvzf lazarus.tgz lazarus
cd -
mv /tmp/lazarus.tgz $OutputFile
rm -rf /tmp/lazarus

# end.


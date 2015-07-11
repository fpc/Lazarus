#!/usr/bin/env bash
#
# Check if fpc can be compiled
# This checks a few common mistakes.

set -x 
set -e

# try to compile a program
echo Testing fpc ...
TmpDir=$TEMP
if [ -z "$TmpDir" ]; then
  TmpDir=~/tmp
fi
TmpDir=$TmpDir/fpc
rm -rf $TmpDir
mkdir -p $TmpDir
TestPas=$TmpDir/test.pas
echo "program test;" >> $TestPas
echo "{\$linklib gpm}" >> $TestPas
echo "{\$linklib ncurses}" >> $TestPas
echo "begin end." >> $TestPas

if [ "x$PP" = "x" ]; then
  fpc $TestPas
else
  $PP $TestPas
fi

# end.


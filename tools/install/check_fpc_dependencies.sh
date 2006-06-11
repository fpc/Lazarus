#!/usr/bin/env bash
#
# Check if fpc can be compiled
# This checks a few common mistakes.

set -x 
set -e

# try to compile a program
echo Testing fpc ...
TmpDir=/tmp/fpc
rm -rf $TmpDir
mkdir -p $TmpDir
TestPas=$TmpDir/test.pas
echo "program test;" >> $TestPas
echo "{\$linklib gpm}" >> $TestPas
echo "{\$linklib ncurses}" >> $TestPas
echo "begin end." >> $TestPas

fpc $TestPas

# end.


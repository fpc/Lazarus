#!/bin/sh
#
# Author: Mattias Gaertner
#
# This script calls makeskel on every unit in the current directory and creates
# an fpdoc xml file for every unit.

set -x
set -e

MakeSkel=$1
if [ -z $MakeSkel ]; then
  MakeSkel=makeskel
fi

# create unit list
UnitList=`echo *.pp *.pas`

for unit in $UnitList; do
  OutFile=`echo $unit | sed -e 's/\.pp\b/\.xml/g' -e 's/\.pas\b/\.xml/g'`
  $MakeSkel --input=$unit' -Fiinclude' --package=lcl --output=$OutFile
done

# end.


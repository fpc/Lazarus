#!/usr/bin/env bash
#
# *****************************************************************************
# *                                                                           *
# *  See the file COPYING.modifiedLGPL, included in this distribution,        *
# *  for details about the copyright.                                         *
# *                                                                           *
# *  This program is distributed in the hope that it will be useful,          *
# *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
# *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
# *                                                                           *
# *****************************************************************************
#
# Author: Mattias Gaertner
#
# This script calls makeskel on every unit in the current directory and creates
# an fpdoc xml file for every unit.

set -x
set -e

MakeSkel=$MAKESKEL
if [ -z $MakeSkel ]; then
  MakeSkel=makeskel
fi

if [ -z $PackageName ]; then

fi

# create unit list
UnitList=`echo *.pp *.pas`

for unit in $UnitList; do
  OutFile=`echo $unit | sed -e 's/\.pp\b/\.xml/g' -e 's/\.pas\b/\.xml/g'`
  $MakeSkel --input=$unit' -Fiinclude' --package=lcl --output=$OutFile
done

# end.


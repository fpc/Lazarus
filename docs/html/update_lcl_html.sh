#!/bin/bash
#
# Author: Mattias Gaertner
#

set -x
set -e

FPDoc=$1
if [ -z $FPDoc ]; then
  FPDoc=fpdoc
fi

XMLSrcDir=../xml/lcl/
PasSrcDir=../../lcl/

# create unit list
cd $XMLSrcDir
UnitList='buttons.pp menus.pp'
#UnitList=`echo *.pp *.pas`
cd -

DescrFiles=''
InputFiles=''
for unit in $UnitList; do
  ShortFile=`echo $unit | sed -e 's/\.pp\b//g' -e 's/\.pas\b//g'`
  DescrFiles="$DescrFiles --descr=$XMLSrcDir$ShortFile.xml"
  InputFiles="$InputFiles --input=$PasSrcDir$unit -Fiinclude"
done

fpdoc --content $DescrFiles $InputFiles --package=lcl --format=html

# end.


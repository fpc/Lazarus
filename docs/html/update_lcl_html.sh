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
InputFileList=inputfile.txt

# create unit list
cd $PasSrcDir
#UnitList='buttons.pp menus.pp'
UnitList=`echo *.pp *.pas`
cd -

# create description file list
DescrFiles=''
for unit in $UnitList; do
  ShortFile=`echo $unit | sed -e 's/\.pp\b//g' -e 's/\.pas\b//g'`
  DescrFiles="$DescrFiles --descr=$XMLSrcDir$ShortFile.xml"
done

# create input file list
rm -f $InputFileList
for unit in $UnitList; do
  echo $PasSrcDir$unit -Fi${PasSrcDir}include >> $InputFileList
done

$FPDoc $DescrFiles --input=@$InputFileList --content --package=lcl --format=html

# end.


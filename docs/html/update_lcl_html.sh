#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the fpdoc HTML output for the LCL

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

$FPDoc $DescrFiles --input=@$InputFileList --content=lcl.cnt --package=lcl \
   --format=html
   
# --output=lcl

# end.


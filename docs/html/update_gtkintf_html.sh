#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the fpdoc HTML output for the gtk interface

set -x
set -e

FPDoc=$1
if [ -z $FPDoc ]; then
  FPDoc=fpdoc
fi

XMLSrcDir=../xml/lcl/interfaces/gtk/
PasSrcDir=../../lcl/interfaces/gtk/
InputFileList=inputfile.txt

# create unit list
cd $PasSrcDir
UnitList=`echo *.pp *.pas | sed -e 's/\*.*\b//g'`
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
  echo $PasSrcDir$unit -dGTK1 >> $InputFileList
done

$FPDoc $DescrFiles --input=@$InputFileList --content=gtkinterface.cnt \
  --import=lcl.cnt,../lcl/ --package=gtkinterface \
  --format=html
  
# --output=lcl/interfaces/gtk

# end.


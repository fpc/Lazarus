#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the fpdoc HTML output for the gtk interface

set -e

FPDoc=$1
if [ -z $FPDoc ]; then
  FPDoc=fpdoc
fi

PackageName=gtkinterface
XMLSrcDir=../xml/lcl/interfaces/gtk/
PasSrcDir=../../lcl/interfaces/gtk/
InputFileList=inputfile.txt

# create output directory
mkdir -p $PackageName

# create unit list
cd $PasSrcDir
UnitList=`echo *.pp *.pas | sed -e 's/\*.*\b//g'`
cd -

# create description file list
DescrFiles=''
for unit in $UnitList; do
  ShortFile=${unit%.pp}
  ShortFile=${ShortFile%.pas}
  DescrFiles="$DescrFiles --descr=../$XMLSrcDir$ShortFile.xml"
done

# create input file list
CurInputFileList=$PackageName/$InputFileList
rm -f $CurInputFileList
for unit in $UnitList; do
  echo ../$PasSrcDir$unit -Fi../$PasSrcDir -dGTK1 >> $CurInputFileList
done

cd $PackageName
$FPDoc $DescrFiles --input=@$InputFileList --content=$PackageName.cnt \
  --import=../lcl/lcl.cnt,../lcl/ --package=$PackageName \
  --format=html
cd -
  
# --output=lcl/interfaces/gtk

# end.


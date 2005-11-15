#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the fpdoc HTML output for the LCL

# set -x
set -e

FPDoc=$1
if [ -z $FPDoc ]; then
  FPDoc=fpdoc
fi
FPDocFooter=$2
RtlContent=$3

PackageName=lcl
XMLSrcDir=../xml/lcl/
PasSrcDir=../../lcl/
InputFileList=inputfile.txt

# create output directory
mkdir -p $PackageName

# create unit list
cd $PasSrcDir
UnitList=`echo *.pp *.pas`
cd -

# create description file list
DescrFiles=''
for unit in $UnitList; do
  ShortFile=`echo $unit | sed -e 's/\.pp\b//g' -e 's/\.pas\b//g'`
  # no need to document lazcwstring, it will be obsolete after 2.0.2
  if [ "$ShortFile" != "lazcwstring" ]; then
    DescrFiles="$DescrFiles --descr=../$XMLSrcDir$ShortFile.xml"
  fi
done

# create input file list
CurInputFileList=$PackageName/$InputFileList
rm -f $CurInputFileList
for unit in $UnitList; do
  echo ../${PasSrcDir}$unit -Fi../${PasSrcDir}include >> $CurInputFileList
done

FPDocParams="--content=lcl.cnt --package=lcl --descr=../${XMLSrcDir}lcl.xml --format=html"
if [ -n "$FPDocFooter" ]; then
  FPDocParams="$FPDocParams --footer=$FPDocFooter"
fi
if [ -n "$RtlContent" ]; then
  FPDocParams="$FPDocParams --import=$RtlContent,../rtl/"
fi


cd $PackageName
$FPDoc $DescrFiles --input=@$InputFileList $FPDocParams
cd -
   
# --output=lcl

# end.


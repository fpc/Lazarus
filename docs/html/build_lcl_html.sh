#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the fpdoc HTML output for the LCL
# Creates an chm file, if HTMLFMT is set to chm,
# otherwise it create html docs


#set -x
set -e

FPDoc=$1
if [ -z $FPDoc ]; then
  FPDoc=fpdoc
fi
FPDocFooter=$2
FPCDocDir=$3

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
  ShortFile=${unit%.pp}
  ShortFile=${ShortFile%.pas}
  DescrFiles="$DescrFiles --descr=../$XMLSrcDir$ShortFile.xml"
done

# create input file list
CurInputFileList=$PackageName/$InputFileList
rm -f $CurInputFileList
for unit in $UnitList; do
  echo ../${PasSrcDir}$unit -Fi../${PasSrcDir}include >> $CurInputFileList
done

if [ -z "$HTMLFMT" ]; then
  HTMLFMT=html
fi

FPDocParams="--content=lcl.xct --package=lcl --descr=../${XMLSrcDir}lcl.xml --format=$HTMLFMT"
if [ "$HTMLFMT" == "chm" ]; then
  FPDocParams="$FPDocParams --css-file=../fpdoc.css --auto-toc --auto-index --make-searchable --output=lcl.chm"
fi
if [ -n "$FOOTERDATE" ]; then
  FPDocParams="$FPDocParams --footer-date=$FOOTERDATE"
fi
if [ -n "$FPDocFooter" ]; then
  FPDocParams="$FPDocParams --footer=$FPDocFooter"
fi
if [ -n "$FPCDocDir" ]; then
  if [ "$HTMLFMT" == "chm" ]; then
    FPDocParams="$FPDocParams --import=$FPCDocDir/rtl.xct,ms-its:rtl.chm::/ --import=$FPCDocDir/fcl.xct,ms-its:fcl.chm::/"
  else
    FPDocParams="$FPDocParams --import=$FPCDocDir/rtl.xct,../rtl/ --import=$FPCDocDir/fcl.xct,../fcl/"
  fi
fi

cd $PackageName
$FPDoc $DescrFiles --input=@$InputFileList $FPDocParams
cd -
   
# end.


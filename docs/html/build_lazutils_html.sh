#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the fpdoc HTML output for the Lazutils package
# Creates an chm file, if HTMLFMT is set to chm,
# otherwise it create html docs
#
#

#set -x
set -e

FPDoc=$1
if [ -z $FPDoc ]; then
  FPDoc=fpdoc
fi
FPDocFooter=$2
FPCDocDir=$3

PackageName=lazutils
XMLSrcDir=../xml/lazutils/
PasSrcDir=../../components/lazutils/
InputFileList=inputfile.txt

# list with units in a preseeded order.
# missing units will be dropped from this list, other units added.
# units not in import order will mutilate links.

PreorderUnitList=( lazutilstrconsts luresstrings lazutf8sysutils lazmethodlist avglvltree )
PreorderUnitList+=( lazutf8 lazutf16 masks fileutil lazutf8classes lconvencoding paswstring )
PreorderUnitList+=( lazdbglog lazfileutils lazfilecache  lazutils ) 
PreorderUnitList+=( laz2_xmlutils laz2_dom laz2_xmlread laz2_xmlwrite )
PreorderUnitList+=( laz_dom lazxmlread laz_xmlwrite )
PreorderUnitList+=( laz_xmlcfg laz2_xmlcfg  laz_xmlstreaming )
#------------------

inarray() 
{ local tofind=$1  element; 
 shift; for element; do [[ $element = "$tofind" ]] && return; done; return 1; 
} 
# Usage: inarray "$value" "${array[@]}"


UnitListArr=()

# create output directory
mkdir -p $PackageName

# create unit list
cd $PasSrcDir
FindUnitList=(*.pas)
cd -

# test for preorder unit existance, add them to unitlist.
for preorder in ${PreorderUnitList[@]}; do
  if [ -f $PasSrcDir/$preorder.pp ]
  then
    UnitListArr+=($preorder.pp)
  fi  
  if [ -f $PasSrcDir/$preorder.pas ]
  then
    UnitListArr+=($preorder.pas)
  fi  
done

#echo 1 ${UnitListArr[@]}

for foundunit in ${FindUnitList[@]}; do
  if ! inarray "$foundunit" "${UnitListArr[@]}" 
  then
    UnitListArr+=($foundunit)  
  fi   
done

UnitList=
for foundunit in ${UnitListArr[@]}; do
  UnitList+="$foundunit "
done

# echo 2 $UnitList

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

FPDocParams="--content=lazutils.xct --package=lazutils --descr=../${XMLSrcDir}lazutils.xml --format=$HTMLFMT"
if [ "$HTMLFMT" == "chm" ]; then
  FPDocParams="$FPDocParams --css-file=../fpdoc.css --auto-toc --auto-index --make-searchable --output=lazutils.chm"
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
pwd
$FPDoc $DescrFiles --input=@$InputFileList $FPDocParams
cd -
   
# end.


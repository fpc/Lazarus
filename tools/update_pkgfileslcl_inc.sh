#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Abstract:
#   Updates the list of files of the LCL package:
#     lazarus/packager/pkgfileslcl.inc

#set -x
set -e

# find the lazarus directory
PrgPath=$0
if [ "${PrgPath:0:1}" != "/" ]; then
  # path does not start with / => relative
  PrgPath=$(pwd)/$PrgPath
fi
LazarusDir=${PrgPath%/tools/*}/

OutputFile=$LazarusDir"packager/pkgfileslcl.inc"
echo "{%MainUnit packagesystem.pas}" > $OutputFile
echo "// automatically created by tools/update_pkgfileslcl_inc.sh" >> $OutputFile

# cd into the lcl directory
cd $LazarusDir/lcl

# scan every LCL .pas/.pp file
for file in $(ls -1 *.pp *.pas widgetset/*.pp nonwin32/*.pp); do
  echo $file
  # get unit name
  Unitname=$(egrep -i '^unit .*;' $file | sed -e 's/unit \+//I' -e 's/;.*//')
  if [ -n "$Unitname" ]; then
    # check if 'procedure Register;' exists
    Flags=""
    if [ -n "$(egrep -i 'procedure register;' $file)" ]; then
      Flags="pffHasRegisterProc"
    fi
    Line="AddFile('"$file"','"$Unitname"',pftUnit,["$Flags"],cpBase);"
    echo $Line >> $OutputFile
  fi
done

echo "" >> $OutputFile

# end.



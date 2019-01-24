#!/usr/bin/env bash
#
# Usage: sh localize.sh
#
# This script should be executed after adding new resource strings and after
# udating the translated .po files.
#
# This script
# - converts all compiled .rst files to .pot files,
# - updates all translated xx.po files
#

# enable for debugging
#set -x
set -e

if [ ! -x tools/updatepofiles ]; then
  cd tools
  make updatepofiles
  cd -
fi

if [ "@"$FPCTARGET == "@" ]; then
  FPCTARGET=`fpc -iTP`-`fpc -iTO`
  if [ $FPCTARGET == "-" ]; then
    FPCTARGET=""
  fi
fi

RSEXT="rst"
FPCVER=`fpc -iV`
if [ "$FPCVER" \> "2.7.0" ]; then
  RSEXT="rsj"
fi

RSTFILES=(
  ". lazarusidestrconsts lazaruside"
  ". debuggerstrconst"
)

set -x

for idx in ${!RSTFILES[@]}; do
  LINE=(${RSTFILES[idx]})
  RSTDIR=${LINE[0]}
  RSTFILE=${LINE[1]}
  POFILE=${LINE[2]:-$RSTFILE}
   
  RST=$(find $RSTDIR -name $RSTFILE.$RSEXT)
  if [ -n "$RST" ]; then
    RST=`find $RSTDIR -name $RSTFILE.$RSEXT | xargs ls -1t | head -1`;
  
    if [ -n "$RST" ]; then
      POFileFull=$RSTDIR/languages/$POFILE.pot
      
      ./tools/updatepofiles $RST $POFileFull
      
    fi
  fi
done

exit 0

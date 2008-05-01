#!/usr/bin/env bash
#
# Usage: sh localize.sh
#
# This script should be executed after adding new resource strings and after
# udating the translated .po files.
#
# This script
# - converts all compiled .rst files to .po files,
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

RSTFILES=(
  ". lazarusidestrconsts lazaruside"
  "ideintf objinspstrconsts"
  "components/codetools codetoolsstrconsts"
  "components/synedit syneditstrconst synedit"
  "components/synedit synmacrorecorder"
  "components/synedit synhighlighterunixshellscript"
  "components/tdbf registerdbf"
  "components/turbopower_ipro ipconst"
  "components/turbopower_ipro iputils"
  "components/cgi cgimodules"
  "components/memds frmselectdataset"
  "components/messagecomposer messagecomposer"  
  "components/printers/design ideprinting"
  "components/projecttemplates projecttemplates"
  "components/projecttemplates frmtemplatevariables"
  "components/projecttemplates idetemplateproject"
  "lcl lclstrconsts"
)  

set -x

for idx in ${!RSTFILES[@]}; do
  LINE=(${RSTFILES[idx]})
  RSTDIR=${LINE[0]}  
  RSTFILE=${LINE[1]}  
  POFILE=${LINE[2]:-$RSTFILE}
   
  RST=$(find $RSTDIR -name $RSTFILE.rst)
  if [ -n "$RST" ]; then
    RST=`find $RSTDIR -name $RSTFILE.rst | xargs ls -1t | head -1`;
  
    if [ -n "$RST" ]; then
      POFileFull=$RSTDIR/languages/$POFILE.po
      echo $POFileFull

      echo 'msgid ""
msgstr ""
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"
' > $POFileFull

      rstconv -i $RST -o $POFileFull.tmp
      cat $POFileFull.tmp >> $POFileFull
      rm $POFileFull.tmp
      ./tools/updatepofiles $POFileFull
    fi
  fi
done

exit 0


#!/usr/bin/env bash
# basic script to do an lcl.chm build. Marco van de Voort dec 2011

# assumes a fpcdocs checkout where "fixdocs.sh" has been successfully run.
# on 1.7GHz Core2 laptop single thread 6:26 minutes. Don't be impatient :-)

set -e

# set to path to FPC docs dir. Default assume it is on the same level as the lazarus checkout
if [ -z "$FPCDocDir" ]; then
  FPCDocDir=../../../fpcdocs
fi
FPCDocDirEXP=$(cd "$FPCDocDir" ; pwd)

if [ -d "$FPCDocDirEXP" ]
then
echo FPCDOCS dir is $FPCDocDirEXP
else
echo FPCDOCS dir $FPCDocDir expanded to $FPCDocDirEXP and was not found.
exit 1
fi

export HTMLFMT=chm
bash build_lazutils_html.sh fpdoc `pwd`/locallclfooter.xml $FPCDocDirEXP 1>lazutilsoutput.log 2>lazutilserror.log
bash build_lcl_html.sh fpdoc `pwd`/locallclfooter.xml $FPCDocDirEXP 1>lcloutput.log 2>lclerror.log

# end.


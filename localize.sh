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

set -x
set -e

if [ ! -x tools/updatepofiles ]; then
  cd tools
  make updatepofiles
  cd -
fi

# IDE
IDE_RST=`find units -name lazarusidestrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $IDE_RST -o languages/lazaruside.po
./tools/updatepofiles languages/lazaruside.po

# IDEIntf
ObjInsp_RST=`find ideintf/units -name objinspstrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $ObjInsp_RST -o ideintf/languages/objinspstrconsts.po
tools/updatepofiles ideintf/languages/objinspstrconsts.po

# CodeTools
CodeTools_RST=`find components/codetools/units -name codetoolsstrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $CodeTools_RST \
  -o components/codetools/languages/codetools.po
./tools/updatepofiles components/codetools/languages/codetools.po

# SynEdit
SynEdit_RST=`find components/synedit/units -name syneditstrconst.rst | xargs ls -1t | head -1`;
rstconv -i $SynEdit_RST \
  -o components/synedit/languages/synedit.po
./tools/updatepofiles components/synedit/languages/synedit.po

# SynMacroRecorder
SynMacroRec_RST=`find components/synedit/units -name synmacrorecorder.rst | xargs ls -1t | head -1`;
rstconv -i $SynMacroRec_RST \
  -o components/synedit/languages/synmacrorecorder.po
./tools/updatepofiles components/synedit/languages/synmacrorecorder.po

# LCL
LCL_RST=`find lcl/units -name lclstrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $LCL_RST -o lcl/languages/lclstrconsts.po
./tools/updatepofiles lcl/languages/lclstrconsts.po

# end.


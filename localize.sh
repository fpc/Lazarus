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
# - converts all translated .po files into .mo files
#

set -x
set -e

if [ ! -x tools/updatepofiles ]; then
  cd tools
  make
  cd -
fi

# IDE
IDE_RST=`find . -name lazarusidestrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $IDE_RST -o languages/lazaruside.po
./tools/updatepofiles languages/lazaruside.po
for lang in de ru ruwin es fr pl it ca fi; do
  msgfmt languages/lazaruside.$lang.po -o languages/lazaruside.$lang.mo
done

# IDEIntf
ObjInsp_RST=`find . -name objinspstrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $ObjInsp_RST -o languages/objinspstrconsts.po
tools/updatepofiles languages/objinspstrconsts.po
for lang in de es fr ru ruwin pl it ca fi; do
  msgfmt languages/objinspstrconsts.$lang.po \
    -o languages/objinspstrconsts.$lang.mo
done

# CodeTools
CodeTools_RST=`find components/units -name codetoolsstrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $CodeTools_RST \
  -o components/codetools/languages/codetools.po
./tools/updatepofiles components/codetools/languages/codetools.po
for lang in de fr pl it ca ru ruwin fi; do
  msgfmt components/codetools/languages/codetools.$lang.po \
    -o components/codetools/languages/codetools.$lang.mo
done

# SynEdit
SynEdit_RST=`find components/units -name syneditstrconst.rst | xargs ls -1t | head -1`;
rstconv -i $SynEdit_RST \
  -o components/synedit/languages/synedit.po
./tools/updatepofiles components/synedit/languages/synedit.po
for lang in de fr pl ca ru ruwin; do
  msgfmt components/synedit/languages/synedit.$lang.po \
    -o components/synedit/languages/synedit.$lang.mo
done

# SynMacroRecorder
SynMacroRec_RST=`find components/units -name synmacrorecorder.rst | xargs ls -1t | head -1`;
rstconv -i $SynMacroRec_RST \
  -o components/synedit/languages/synmacrorecorder.po
./tools/updatepofiles components/synedit/languages/synmacrorecorder.po
for lang in de fr ru ruwin pl ca; do
  msgfmt components/synedit/languages/synmacrorecorder.$lang.po \
    -o components/synedit/languages/synmacrorecorder.$lang.mo || true
done

# LCL
LCL_RST=`find lcl/units -name lclstrconsts.rst | xargs ls -1t | head -1`;
rstconv -i $LCL_RST -o lcl/languages/lcl.po
./tools/updatepofiles lcl/languages/lcl.po
for lang in de es fr ru ruwin pl it ca fi; do
  msgfmt lcl/languages/lcl.$lang.po -o lcl/languages/lcl.$lang.mo || true
done

# end.


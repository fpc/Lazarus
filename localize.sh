#!/bin/bash
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
set +e

# IDE without objectinspector
rstconv -i lazarusidestrconsts.rst -o languages/lazaruside.po
./tools/updatepofiles languages/lazaruside.po
for lang in de ru es fr; do
  msgfmt languages/lazaruside.$lang.po -o languages/lazaruside.$lang.mo
done

# Object Inspector
rstconv -i objinspstrconsts.rst -o languages/objinspstrconsts.po
tools/updatepofiles languages/objinspstrconsts.po
for lang in de es fr ru; do
  msgfmt languages/objinspstrconsts.$lang.po \
    -o languages/objinspstrconsts.$lang.mo
done

# CodeTools
rstconv -i components/units/codetoolsstrconsts.rst \
  -o components/codetools/languages/codetools.po
./tools/updatepofiles components/codetools/languages/codetools.po
for lang in de fr; do
  msgfmt components/codetools/languages/codetools.$lang.po \
    -o components/codetools/languages/codetools.$lang.mo
done

# SynEdit
rstconv -i components/units/syneditstrconst.rst \
  -o components/synedit/languages/synedit.po
./tools/updatepofiles components/synedit/languages/synedit.po
for lang in de fr; do
  msgfmt components/synedit/languages/synedit.$lang.po \
    -o components/synedit/languages/synedit.$lang.mo
done

# SynMacroRecorder
rstconv -i components/units/synmacrorecorder.rst \
  -o components/synedit/languages/synmacrorecorder.po
./tools/updatepofiles components/synedit/languages/synmacrorecorder.po
for lang in de fr ru; do
  msgfmt components/synedit/languages/synmacrorecorder.$lang.po \
    -o components/synedit/languages/synmacrorecorder.$lang.mo
done

# LCL
rstconv -i lcl/units/lclstrconsts.rst -o lcl/languages/lcl.po
./tools/updatepofiles lcl/languages/lcl.po
for lang in de es fr ru; do
  msgfmt lcl/languages/lcl.$lang.po -o lcl/languages/lcl.$lang.mo
done

# end.


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
#set -e

# IDE without objectinspector
rstconv -i lazarusidestrconsts.rst -o languages/lazaruside.po
./tools/updatepofiles languages/lazaruside.po
for lang in de ru es fr; do
  msgfmt languages/lazaruside.$lang.po -o languages/lazaruside.$lang.mo
done

# objectinspector
rstconv -i objinspstrconsts.rst -o languages/objinspstrconsts.po
tools/updatepofiles languages/objinspstrconsts.po
for lang in de es fr; do
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

# LCL
rstconv -i lcl/units/lclstrconsts.rst -o lcl/languages/lcl.po
./tools/updatepofiles lcl/languages/lcl.po
for lang in de es fr; do
  msgfmt lcl/languages/lcl.$lang.po -o lcl/languages/lcl.$lang.mo
done

# end.


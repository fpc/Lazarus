#!/bin/bash
# This script may be used to compile all language.

set -x

# IDE without objectinspector
for lang in de ru es; do
  msgfmt languages/lazaruside.$lang.po -o languages/lazaruside.$lang.mo
done

# objectinspector
for lang in de; do
  msgfmt languages/objinspstrconsts.$lang.po \
    -o languages/objinspstrconsts.$lang.mo
done

# CodeTools
for lang in de; do
  msgfmt components/codetools/languages/codetools.$lang.po \
    -o components/codetools/languages/codetools.$lang.mo
done

# LCL
for lang in de; do
  msgfmt lcl/languages/lcl.$lang.po -o lcl/languages/lcl.$lang.mo
done

# end.


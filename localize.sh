#!/bin/sh
# This script may be used to compile all language.
# This script is too small and simple to be licensed.
# Let's decide, that it is public property.
# If it can't be public property anywhere (or anywhy), it also can be of GPL 2.

msgfmt languages/lazaruside.de.po -o languages/lazaruside.de.mo
msgfmt languages/lazaruside.ru.po -o languages/lazaruside.ru.mo
msgfmt components/codetools/languages/codetools.ru.po -o components/codetools/languages/codetools.ru.mo
msgfmt lcl/languages/lcl.de.po -o lcl/languages/lcl.de.mo
msgfmt lcl/languages/lcl.ru.po -o lcl/languages/lcl.ru.mo

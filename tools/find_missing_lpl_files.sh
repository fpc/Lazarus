#!/usr/bin/env bash
#
# run in tools
# prints out all lpk files without lpl

set -e

if [ ! -e tools ]; then
  echo "please cd to lazarus source directory before running this script"
  exit
fi

LPKFiles=$(find . -name '*.lpk' | xargs)
for LPK in $LPKFiles; do
  LPKName=$(echo $LPK | sed -e 's/.*\///' -e 's/\.lpk//')
  Missing=
  ls packager/globallinks/${LPKName}-*.lpl >/dev/null 2>/dev/null || Missing=1
  if [ -n "$Missing" ]; then
    echo missing lpl for $LPK
    LPK=$(echo $LPK | sed -e 's/^\.\///')
    LPLFilename=packager/globallinks/$LPKName-0.lpl
    echo '$(LazarusDir)/'$LPK > $LPLFilename
  fi
done

#end.


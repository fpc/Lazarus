#!/usr/bin/env bash
#
# run in tools
# prints out all lpk files without lpl

set -e

LazDir=..

LPKFiles=$(find $LazDir -name '*.lpk' | xargs)
for LPK in $LPKFiles; do
  LPKName=$(echo $LPK | sed -e 's/.*\///' -e 's/.lpk//')
  ls $LazDir/packager/globallinks/${LPKName}-*.lpl 2>/dev/null > /dev/null || echo missing lpl for $LPK
done

#end.


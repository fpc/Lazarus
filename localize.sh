#!/usr/bin/env bash
#
# Usage: sh localize.sh
#
# This script should be executed after adding new resource strings and after
# udating the translated .po files.
#
# This script
# - converts all compiled .rsj files to .pot files,
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

RSEXT="rsj"

set -x

./tools/updatepofiles --searchdir=./units lazarusidestrconsts.$RSEXT ./languages/lazaruside.pot
./tools/updatepofiles --searchdir=./units debuggerstrconst.$RSEXT ./languages/debuggerstrconst.pot

exit 0

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

# IDE
# Lazarus languages : ca de es esutf fi fiwin fr he it itiso nl pl pliso plwin ru ruwin ruutf 
IDE_RST=`find . -name lr_const.rst | xargs ls -1t | head -1`;
rstconv -i $IDE_RST -o ./../languages/lazreport.po
updatepofiles ./../languages/lazreport.po
for lang in fr; do
  msgfmt ./../languages/lazreport.$lang.po -o ./../languages/lazreport/lazaruside.$lang.mo
done

# end.


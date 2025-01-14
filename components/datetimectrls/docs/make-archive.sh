#!/bin/bash

echo ""
echo "Create archive for DateTimeCtrls html documentation"
echo ""

# convert to unix line endings expected by git

# create an archive for the generated html content
dt=`date +"%Y-%m-%d"`
7z a -t7z -r -mx9 docs-html-datetimectrls-$dt.7z ./datetimectrls/

cp -v docs-html-datetimectrls-$dt.7z ../../../docs/build/html/
# the .7z file is removed in clean-files.sh

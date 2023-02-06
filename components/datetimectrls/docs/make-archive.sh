#!/bin/bash

echo ""
echo "Create archive for DateTimeCtrls html documentation"
echo ""

# convert to unix line endings expected by git
cd datetimectrls
dos2unix *.{html,css}
dos2unix **/*.html
cd ..

# create an archive for the generated html content
dt=`date +"%Y-%m-%d"`
7z a -t7z -r -mx9 docs-html-datetimectrls-$dt.7z ./datetimectrls/

# move .7z file to another directory for permanent storage
# the .7z file is removed in clean-files.sh

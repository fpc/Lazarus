#!/bin/bash

# run build_chm.sh first... it generates the footer files with version and revision info

# change any path information used in this script for your system
fpdocpath="c:/lazarus/fpc/3.2.0/bin/x86_64-win64"

# build html output format using locallclfooter.xml, locallazutilsfooter.xml for footer content
./build_lcl_docs.exe --outfmt=html --css-file=fpdoc.css --fpcdocs=../chm --footer locallclfooter.xml --fpdoc=$fpdocpath/fpdoc.exe --warnings --verbose 2>&1 | tee ./build_html.log

# use custom css instead of the built in one
cp lazutils/fpdoc.css lazutils/lazutils/
cp lcl/fpdoc.css lcl/lcl/

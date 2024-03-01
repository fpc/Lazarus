#!/bin/bash

# Builds Lazarus documentaiton for LCL and LazUtils in HTML format.
# Requires a $(LazarusDir)/docs/chm directory which contains rtl.xct and fcl.xct.
# Process fails if either the directory or the files are missing.
# Process creates ./lcl and ./lazutils subdirectories for HTML output.
# Update the content in locallclfooter.xml if needed before starting the script.

# Path to the fpdoc program
# The trunk (main) version of fpdoc is recommended.
# CHANGE THIS TO A PATH VALID FOR YOUR SYSTEM AND FPDOC VERSION.
#fpdocpath="../../fpc/3.2.2/bin/x86_64-win64"
fpdocpath="../../../fpc331/fpc/bin/x86_64-win64"

# rebuild if needed
#../../lazbuild build_lcl_docs.lpi

# build html output without footers
#./build_lcl_docs --outfmt=html --fpcdocs=../chm --fpdoc=$fpdocpath/fpdoc 2>&1 | tee -a ./build_html.log

# build chm output with date in footer
# DOES NOT WORK IN FPDOC 3.3.1 AS OF 2024-03-01 (AV)
#./build_lcl_docs --outfmt=html --fpcdocs=../chm --fpdoc=$fpdocpath/fpdoc --arg="--footer-date" 2>&1 | tee -a ./build_html.log

# For FPDoc 3.2.X: build html output with footers in locallclfooter.xml
# update the content in locallclfooter.xml if needed before starting the script.
#./build_lcl_docs --outfmt=html --fpcdocs=../chm --fpdoc=$fpdocpath/fpdoc --footer=locallclfooter.xml 2>&1 | tee -a ./build_html.log

# For FPDoc 3.3.X: build html output with footers in locallclfooter.xml
# update the content in locallclfooter.xml if needed before starting the script.
./build_lcl_docs --outfmt=html --fpcdocs=../chm --fpdoc=$fpdocpath/fpdoc --footer=@locallclfooter.xml 2>&1 | tee -a ./build_html.log

# For FPDoc 3.3.X ONLY: build html output with footer text
# update the text in ftr before starting the script
ftr="Version 3.4 (YYYY-MM-DD)"
#./build_lcl_docs --outfmt=html --fpcdocs=../chm --fpdoc=$fpdocpath/fpdoc --footer="$ftr" 2>&1 | tee -a ./build_html.log

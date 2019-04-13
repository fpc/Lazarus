HTML documentation of Lazarus
=============================

This directory contains the tools to create the HTML and CHM documentation for
Lazarus. The documentation is stored in fpdoc format in the docs/xml/ directory.

For UNIXes:

The script build_html.sh will automatically create the whole HTML
documentation.
Build build_lcl_docs.lpi
../../lazbuild build_lcl_docs.lpi

./build_lcl_docs --fpdoc /path/to/fpc/trunk/utils/fpdoc/fpdoc --fpcdocs=../chm --outfmt chm --footer locallclfooter.xml

For Windows:

Build the project build_lcl_docs.lpi, fix the PATH in the build_html.bat batch
script to your local installation and then run the script, or run
build_lcl_docs.exe directly.


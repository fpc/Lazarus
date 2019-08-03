#!/bin/bash
# Builds all Lazarus docs and pack them in one .chm file
#
../../lazbuild build_lcl_docs.lpi
./build_lcl_docs --outfmt chm --fpcdocs=../chm --footer locallclfooter.xml

# Preferably: use trunk fpdoc: --fpdoc /path/to/fpc/trunk/utils/fpdoc/fpdoc

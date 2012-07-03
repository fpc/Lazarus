#!/bin/bash
# Builds all Lazarus docs and pack them in one .chm file
#
../../lazbuild build_lcl_docs.lpi
./build_lcl_docs --outfmt chm

# Build with links to RTL+FCL
#./build_lcl_docs --fpcdocs ../chm/ --footer locallclfooter.xml --outfmt chm

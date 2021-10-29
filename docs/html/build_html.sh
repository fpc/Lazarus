#!/bin/bash
# Builds all Lazarus docs and pack them in one .chm file
#
# Notes:
#
# Before running this file, first compile the project build_lcl_docs.lpi
#
# In order to link to RTL and FCL, place rtl.xct and fcl.xct in ../chm/
#
./build_lcl_docs --fpcdocs=../chm --outfmt html



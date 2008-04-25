#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the whole HTML output for Lazarus

set -x
set -e

sh ./build_lcl_html.sh $@
sh ./build_gtkintf_html.sh $@

# end.


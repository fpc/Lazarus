#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the whole HTML output for Lazarus

set -x
set -e

./update_lcl_html.sh $@
./update_gtkintf_html.sh $@

# end.


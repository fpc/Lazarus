#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Creates the whole HTML output for Lazarus

set -x
set -e

sh ./update_lcl_html.sh $@
sh ./update_gtkintf_html.sh $@

# end.


#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Usage: file_filter.sh <executable> [options]
#
# Excludes .pl/.pm files from stdin and calls <executable>.

set -e
set -x

Executable=$1
shift
Params=$@
FileList=`sed -e 's/[^ ]\+\.\(pm\|pl\)//g'`
echo $FileList | $Executable $Params

# end.


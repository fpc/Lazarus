#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Normally just calls strip with the same parameters. Special files, which can
# not be stripped are skipped

Params=$@
for p in $Params; do
  File=$p
done
echo $File | grep -q '\bpalmos\b' || strip $PARAMS

# end.


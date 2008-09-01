#!/usr/bin/env bash
set -e
LazBuild=$1
if [ ! -x "$LazBuild" ]; then
  echo Usage: $0 ./lazbuild
  exit -1
fi
set -x

find . -name '*.lpk' -exec $LazBuild {} \+

find . -name '*.lpi' -exec $LazBuild {} \+

#end.

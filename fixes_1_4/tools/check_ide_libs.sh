#!/bin/bash

set -e
set -x

GlibCount=`ldd lazarus | grep '\blibglib-[12]' | wc -l | sed 's/ //g'`
if [ "x$GlibCount" = "x2" ]; then
  echo "lazarus.pp(1) fatal: lib glib is used twice"
  exit -1
fi

# end.

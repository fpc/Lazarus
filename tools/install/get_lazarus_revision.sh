#!/usr/bin/env bash

if [ ! -f ../svn2revisioninc ]; then
  make -C ../../lcl
  make -C ..
fi

../svn2revisioninc -s

# end.


#!/usr/bin/env bash

grep 'lisLazarusVersionString = ' ../../lazarusidestrconsts.pas \
  | sed -e 's/.*[^0-9\.]\([0-9\.]\+\)[^0-9\.].*/\1/'

# end.


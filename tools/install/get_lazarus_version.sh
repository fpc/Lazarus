#!/usr/bin/env bash

grep 'lisLazarusVersionString = ' ../../ide/lazarusidestrconsts.pas \
  | sed -e 's/.*[^0-9\.]\([0-9\.]\+\)[^0-9\.].*/\1/'

# end.


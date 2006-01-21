#!/usr/bin/env bash

grep 'LazarusVersionStr= ' ../../ide/aboutfrm.pas \
  | sed -e 's/.*[^0-9\.]\([0-9\.]\+\)[^0-9\.].*/\1/'

# end.


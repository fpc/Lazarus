#!/usr/bin/env bash

cat ../../ide/version.inc \
  | sed -e 's/^.*[^0-9 \.]\([0-9\.]\+\)[^0-9\.].*$/\1/'

# end.


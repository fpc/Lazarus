#!/usr/bin/env bash

cat "$(dirname $0)/../../ide/version.inc" \
  | tr -d \' | tr -d ' ' | tr -d '\t'

# end.


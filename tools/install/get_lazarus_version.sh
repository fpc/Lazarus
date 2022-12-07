#!/usr/bin/env bash

cat "$(dirname $0)/../../ide/packages/ideconfig/version.inc" \
  | tr -d \' | tr -d ' ' | tr -d '\t'

# end.


#!/usr/bin/env bash

cat ../../ide/version.inc \
  | tr -d \' | tr -d ' ' | tr -d '\t'

# end.


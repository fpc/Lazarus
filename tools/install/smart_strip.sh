#!/usr/bin/env bash

Params=$@
for p in $Params; do
  File=$p
done
echo $File | grep -q '\bpalmos\b' || strip $PARAMS

# end.


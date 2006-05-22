#!/usr/bin/env bash

LFM=$1
LRS=$(echo $LFM | sed -e 's/\.lfm$/\.lrs/')

./lazres $LRS $LFM

# end.


#!/usr/bin/env bash

echo $(svnversion $1 | sed -e 's/\([0-9]*\).*/\1/')

# end.


#!/usr/bin/env bash
#
# Author: Mattias Gaertner

find . -name '*.po' -exec echo -n $(basename {}) " " \;


# end.


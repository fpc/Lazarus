#!/usr/bin/env bash
#
# Author: Mattias Gaertner

find . -name '*.mo' -exec echo -n $(basename {}) " " \;


# end.


#!/bin/bash

CommandLine=$@
#set -x

echo $CommandLine
$CommandLine
echo "--------------------------------------------------"
echo "Press return"
read

# end.

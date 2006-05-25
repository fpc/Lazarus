#!/bin/sh

CommandLine=$@
#set -x

echo $CommandLine
ext=${CommandLine#*.}
if [ "$ext" = "exe" ]; then
  echo "Windows Executable detected. Attempting to use WINE..."
  if [ -x "`which wine`" ]; then
    wine $CommandLine
  else
    echo "WINE not found in path"
  fi
else
  $CommandLine
fi

echo "--------------------------------------------------"
echo "Press enter"
read trash crash

# end.

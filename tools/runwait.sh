#!/bin/sh

CommandLine="$@"
ExeName="$1"
#set -x

echo $CommandLine
ext=${ExeName#*.}
echo "ext=$ext"
if [ "$ext" = "exe" ]; then
  echo "Windows Executable detected. Attempting to use WINE..."
  if [ -x "`which wine`" ]; then
    wine $CommandLine
  else
    echo "WINE not found in path"
  fi
else
  shift
  "$ExeName" $@
fi

echo "--------------------------------------------------"
echo "Press enter"
read trash crash

# end.

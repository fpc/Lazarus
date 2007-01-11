#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# converts a .po files to UTF-8

set -e
#set -x

POFile=$1
if [ -f "$POFile" ]; then
  OldCharSet=$(cat $POFile | egrep '^".* charset=' | sed -e 's/.*charset=\(.*\)"/\1/' -e 's/\\n//')
  echo $OldCharSet
  if [ -n "$OldCharSet"="UTF-8" ]; then
    iconv --from-code=$OldCharSet --to-code=UTF-8 $POFile > $POFile.tmp
    cat $POFile.tmp | sed -e 's/\(^".* charset\)='$OldCharSet'/\1=UTF-8/' > $POFile
    rm $POFile.tmp
  fi
else
  echo Usage: $0 po-file
  exit -1
fi


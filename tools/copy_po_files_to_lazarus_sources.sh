#!/usr/bin/env bash
#
# Author: Mattias Gaertner
#
# Usage:
#   copy_po_files_to_lazarus_sources.sh LazarusSrcDir file1.po [file2.po] ...
#
# Copies a set of localized .po files to the right location in the lazarus
# source directory.

#set -x
set -e

Usage="$0 LazarusSrcDir file1.po [file2.po] ..."

# get and check lazarus source dir
LazarusSrcDir=$1
if [ ! -d "$LazarusSrcDir" ]; then
  echo $Usage
  exit
fi
echo LazarusSrcDir=$LazarusSrcDir
if [ ! -d "$LazarusSrcDir/lcl/languages" ]; then
  echo Error: $LazarusSrcDir does not look like the Lazarus Source Directory
  exit
fi
shift

# check and copy all .po files to the right directories
POFiles=$@
for POFile in $POFiles; do
  echo $POFile

  # check if .po file exists
  if [ ! -f "$POFile" ]; then
    echo Error: $POFile not found
    exit
  fi

  # check if .po file format file.lang.po
  FileOk=`echo $POFile | sed -e 's/.*\..*\.po$/ok/'`
  if [ "$FileOk" != "ok" ]; then
    echo "Error: $POFile invalid  (expected format: file.lang.po)"
    exit
  fi

  # check if .po file has corresponding base file
  BasePOFile=`echo $POFile | sed -e 's/\(.*\)\..*\(\.po\)$/\1\2/'`
  BasePOFile=`basename $BasePOFile`
  FoundPOFile=`find $LazarusSrcDir -name $BasePOFile`
  if [ ! -f "$FoundPOFile" ]; then
    echo "Error: base .po file not found: $BasePOFile"
    exit
  fi

  # copy the .po file to the same directory as the base .po file
  DestDir=`dirname $FoundPOFile`
  echo "  -> $DestDir/"
  cp $POFile $DestDir/
done

# end.


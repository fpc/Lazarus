#!/usr/bin/env bash

#set -x
set -e

# make sure, we are in the right directory
cd ../scripts

CHeaderDir=../c_src/x11
PascalIncDir=../include
HTmpFile=temp.h
PasTmpFile=temp.inc

HFiles=$(ls $CHeaderDir/*.h)
for HFile in $HFiles; do
  echo $HFile
  ShortHFile=$(echo $HFile | sed -e 's#.*/##')
  ShortPascalFile=$(echo $ShortHFile | sed -e 's#\.h$#.inc#')
  echo $ShortPascalFile
  PascalFile=$PascalIncDir/$ShortPascalFile
  cat $HFile | sed \
    -e 's#G_BEGIN_DECLS##g' \
    -e 's#G_END_DECLS##g' \
    -e 's#G_CONST_RETURN#const#g' \
    > $HTmpFile

  h2pas -d -e -i -p -t -o $PasTmpFile $HTmpFile
  cat $PasTmpFile | sed \
    -e 's#\bT\(gint\)\b#\1#g' \
    -e 's#\bT\(guint\)\b#\1#g' \
    -e 's#\bT\(gboolean\)\b#\1#g' \
    -e 's#\bT\(glong\)\b#\1#g' \
    -e 's#\bT\(gulong\)\b#\1#g' \
    -e 's#\bT\(gchar\)\b#\1#g' \
    -e 's#\bT\(guchar\)\b#\1#g' \
    -e 's#\bT\(gshort\)\b#\1#g' \
    -e 's#\bT\(gushort\)\b#\1#g' \
    -e 's#\bT\(gfloat\)\b#\1#g' \
    -e 's#\bT\(gdouble\)\b#\1#g' \
    -e 's#\bT\(gpointer\)\b#\1#g' \
    -e 's#\bT\(gconstpointer\)\b#\1#g' \
    -e 's#\bT\(guint32\)\b#\1#g' \
    > $PascalFile
done


# end.


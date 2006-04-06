#!/usr/bin/env bash

set -e

PPCARCH=ppcppc
SVN=/usr/local/bin/svn
FREEZE=/usr/bin/freeze
HDIUTIL=/usr/bin/hdiutil
UPDATELIST=~/tmp/updatelist

TEMPLATEDIR=~/etc/templates

FPCSVNDIR=~/src/fpc/build
FPCSOURCEDIR=$FPCSVNDIR/fpcsrc
COMPILER=~/fpc/bin/$PPCARCH
INSTALLDIR=~/tmp/fpcsrc

DATESTAMP=`date +%Y%m%d`
PACKPROJ=fpcsrc.packproj


# clean installdir: since I am not root and the install dir can contain files owned by root 
# created by a previous freeze, I just move it out of the way
TRASHDIR=/tmp/`whoami`/trash
if [ ! -d $TRASHDIR ] ; then
  mkdir -p $TRASHDIR
fi
if [ -d $INSTALLDIR ] ; then
  mv $INSTALLDIR $TRASHDIR/fpcsrc-`date +%Y%m%d%H%M%S`
fi

# copy sources
mkdir -p $INSTALLDIR/fpcsrc
$SVN export $FPCSOURCEDIR/rtl $INSTALLDIR/fpcsrc/rtl
$SVN export $FPCSOURCEDIR/fcl $INSTALLDIR/fpcsrc/fcl
$SVN export $FPCSOURCEDIR/packages $INSTALLDIR/fpcsrc/packages

# fill in packproj template.
FPCVERSION=`$COMPILER -iV`
FPCARCH=`$COMPILER -iSP`
OLDIFS=$IFS
IFS=.
FPCMAJORVERSION=`set $FPCVERSION;  echo $1`
FPCMINORVERSION=`set $FPCVERSION;  echo $2$3`
IFS=$OLDIFS
sed -e "s|_FPCSRCDIR_|$FPCSVNDIR|g" -e "s|_FPCVERSION_|$FPCVERSION|g" \
  -e "s|_DATESTAMP_|$DATESTAMP|g" -e s/_FPCMAJORVERSION_/$FPCMAJORVERSION/g -e s/_FPCMINORVERSION_/$FPCMINORVERSION/g \
  $TEMPLATEDIR/$PACKPROJ  > $INSTALLDIR/$PACKPROJ

# build package
$FREEZE -v $INSTALLDIR/$PACKPROJ

DMGFILE=~/pkg/fpcsrc-$FPCVERSION-$DATESTAMP-$FPCARCH-macosx.dmg
rm -rf $DMGFILE

$HDIUTIL create -anyowners -volname fpcsrc-$FPCVERSION -imagekey zlib-level=9 -format UDZO -srcfolder $INSTALLDIR/build $DMGFILE

if [ -e $DMGFILE ]; then
#update lazarus snapshot web page
  echo "$DMGFILE fpcsrc-*-*-$FPCARCH-macosx.dmg" >> $UPDATELIST
fi

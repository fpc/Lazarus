#!/usr/bin/env bash

set -e
set -x

HDIUTIL=/usr/bin/hdiutil
UPDATELIST=~/tmp/updatelist

FPCSVNDIR=$1
if [ ! -d "$FPCSVNDIR" ]; then
  FPCSVNDIR=~/src/fpcbuild/2.0.4
  echo "Using default FPCSVNDIR: $FPCSVNDIR"
fi

PPCARCH=ppcppc
ARCH=`uname -p`
if [ "$ARCH" = "i386" ]; then
  PPCARCH=ppc386
fi

SVN=`which svn`
if [ ! -e "$SVN" ]; then
  SVN=/usr/local/bin/svn
fi

if [ ! -e "$SVN" ]; then
  SVN=/sw/bin/svn
fi

if [ ! -e "$SVN" ]; then
  echo "Cannot find a svn executable"
fi

FREEZE=/usr/local/bin/freeze
if [ ! -e "$FREEZE" ]; then
  FREEZE=/usr/bin/freeze
fi
if [ ! -e "$FREEZE" ]; then
  echo "Cannot find freeze"
fi

TEMPLATEDIR=`dirname $0`

FPCSOURCEDIR=$FPCSVNDIR/fpcsrc
COMPILER=~/fpc/bin/$PPCARCH
INSTALLDIR=~/tmp/fpcsrc

DATESTAMP=`date +%Y%m%d`
PACKPROJ=fpcsrc.packproj.template


# clean installdir: since I am not root and the install dir can contain files owned by root 
# created by a previous freeze, I just move it out of the way
TRASHDIR=~/tmp/trash
mkdir -p $TRASHDIR
if [ -d $INSTALLDIR ] ; then
  mv $INSTALLDIR $TRASHDIR/fpcsrc-`date +%Y%m%d%H%M%S`
fi

# copy sources
mkdir -p $INSTALLDIR/fpcsrc
$SVN export $FPCSOURCEDIR/rtl $INSTALLDIR/fpcsrc/rtl
if [ -d $FPCSOURCEDIR/fcl ] ; then
  $SVN export $FPCSOURCEDIR/fcl $INSTALLDIR/fpcsrc/fcl
fi
$SVN export $FPCSOURCEDIR/packages $INSTALLDIR/fpcsrc/packages

# fill in packproj template.
FPCVERSION=`$COMPILER -iV`
FPCFULLVERSION=`$COMPILER -iW`
FPCARCH=`$COMPILER -iSP`
OLDIFS=$IFS
IFS=.
FPCMAJORVERSION=`set $FPCVERSION;  echo $1`
FPCMINORVERSION=`set $FPCVERSION;  echo $2$3`
IFS=$OLDIFS
sed -e "s|_FPCSRCDIR_|$FPCSVNDIR|g" -e "s|_DATESTAMP_|$DATESTAMP|g" \
  -e "s|_FPCVERSION_|$FPCVERSION|g" -e "s|_FPCFULLVERSION_|$FPCFULLVERSION|g" \
  -e s/_FPCMAJORVERSION_/$FPCMAJORVERSION/g -e s/_FPCMINORVERSION_/$FPCMINORVERSION/g \
  $TEMPLATEDIR/$PACKPROJ  > $INSTALLDIR/$PACKPROJ

# build package
$FREEZE -v $INSTALLDIR/$PACKPROJ

DMGFILE=~/pkg/fpcsrc-$FPCFULLVERSION-$DATESTAMP-$FPCARCH-macosx.dmg
rm -rf $DMGFILE

$HDIUTIL create -anyowners -volname fpcsrc-$FPCVERSION -imagekey zlib-level=9 -format UDZO -srcfolder $INSTALLDIR/build $DMGFILE

if [ -e $DMGFILE ]; then
#update lazarus snapshot web page
  echo "$DMGFILE fpcsrc-$FPCFULLVERSION-*-$FPCARCH-macosx.dmg" >> $UPDATELIST
fi

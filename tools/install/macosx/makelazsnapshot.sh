#!/usr/bin/env bash

set -e
set -x

PPCARCH=ppcppc
SVN=/usr/local/bin/svn
FREEZE=/usr/bin/freeze
HDIUTIL=/usr/bin/hdiutil
UPDATELIST=~/tmp/updatelist

LAZSOURCEDIR=~/src/lazsource

COMPILER=~/fpc/bin/$PPCARCH
BUILDDIR=~/tmp/buildlaz
LAZBUILDDIR=$BUILDDIR/lazarus
DATESTAMP=`date +%Y%m%d`
PACKPROJ=lazarus.packproj
TEMPLATEDIR=$LAZSOURCEDIR/tools/install/macosx
rm -rf $BUILDDIR

# copy sources
cd $LAZSOURCEDIR
$SVN update

cd $LAZSOURCEDIR/tools/install
LAZVERSION=`./get_lazarus_version.sh`
cd -

if [ -d $BUILDDIR ]; then
  rm -rf $BUILDDIR
fi
mkdir -p $BUILDDIR
$SVN export $LAZSOURCEDIR $LAZBUILDDIR
if [ ! -e tools/svn2revisioninc ]; then
  make tools PP=$COMPILER
fi
tools/svn2revisioninc $LAZSOURCEDIR $LAZBUILDDIR/ide/revision.inc

cd $LAZBUILDDIR

make bigide PP=$COMPILER
make lcl LCL_PLATFORM=carbon PP=$COMPILER
strip lazarus
strip startlazarus

# copy license file, it must be a txt file.
cp $LAZBUILDDIR/COPYING.GPL $BUILDDIR/License.txt

# fill in packproj template.
OLDIFS=$IFS
IFS=.
LAZMAJORVERSION=`set $LAZVERSION;  echo $1`
LAZMINORVERSION=`set $LAZVERSION;  echo $2$3`
FPCARCH=`$COMPILER -iSP`
IFS=$OLDIFS
sed -e "s|_LAZARUSDIR_|$LAZBUILDDIR|g" -e "s|_LAZVERSION_|$LAZVERSION|g" \
  -e "s|_DATESTAMP_|$DATESTAMP|g" -e s/_LAZMAJORVERSION_/$LAZMAJORVERSION/g \
  -e s/_LAZMINORVERSION_/$LAZMINORVERSION/g \
  $TEMPLATEDIR/$PACKPROJ.template  > $BUILDDIR/$PACKPROJ

# build package
$FREEZE -v $BUILDDIR/$PACKPROJ

DMGFILE=~/pkg/lazarus-$LAZVERSION-$DATESTAMP-$FPCARCH-macosx.dmg
rm -rf $DMGFILE

$HDIUTIL create -anyowners -volname lazarus-$LAZVERSION -imagekey zlib-level=9 \
  -format UDZO -srcfolder $BUILDDIR/build $DMGFILE

if [ -e $DMGFILE ]; then
#update lazarus snapshot web page
  echo "$DMGFILE lazarus-*-*-$FPCARCH-macosx.dmg " >> $UPDATELIST
fi

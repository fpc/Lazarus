#!/usr/bin/env bash

set -e
set -x

HDIUTIL=/usr/bin/hdiutil
UPDATELIST=~/tmp/updatelist

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

LAZSOURCEDIR=~/src/lazsource

COMPILER=~/fpc/bin/$PPCARCH
CROSSCOMPILER=~/fpc/bin/ppcppc
FPCVERSION=`$COMPILER -iV`
BUILDDIR=~/tmp/buildlaz
LAZBUILDDIR=$BUILDDIR/lazarus
DATESTAMP=`date +%Y%m%d`
PACKPROJ=lazarus.packproj
TEMPLATEDIR=$LAZSOURCEDIR/tools/install/macosx


# copy sources
cd $LAZSOURCEDIR
$SVN update

cd $LAZSOURCEDIR/tools/install
LAZVERSION=`./get_lazarus_version.sh`
cd -

# clean builddir: since I am not root and the install dir can contain files owned by root 
# created by a previous freeze, I just move it out of the way
TRASHDIR=/tmp/`whoami`/trash
if [ ! -d $TRASHDIR ] ; then
  mkdir -p $TRASHDIR
fi
if [ -d $BUILDDIR ] ; then
  mv $BUILDDIR $TRASHDIR/lazbuild-`date +%Y%m%d%H%M%S`
fi


mkdir -p $BUILDDIR
$SVN export $LAZSOURCEDIR $LAZBUILDDIR
if [ ! -e tools/svn2revisioninc ]; then
  make tools PP=$COMPILER
fi
tools/svn2revisioninc $LAZSOURCEDIR $LAZBUILDDIR/ide/revision.inc

cd $LAZBUILDDIR

export FPCDIR=~/fpc/lib/fpc/$FPCVERSION

make bigide PP=$COMPILER USESVN2REVISIONINC=0
make lazbuilder PP=$COMPILER

# make non-default LCL platforms
make lcl LCL_PLATFORM=carbon PP=$COMPILER
make lcl LCL_PLATFORM=gtk2 OPT="-dUseX" PP=$COMPILER

# cross compilation units?
if [ "$ARCH" == "i386NotYet" ]; then
  make lcl CPU_TARGET=powerpc PP=$CROSSCOMPILER
  make lcl CPU_TARGET=powerpc LCL_PLATFORM=carbon PP=$CROSSCOMPILER
  make lcl CPU_TARGET=powerpc LCL_PLATFORM=gtk2 OPT="-dUseX" PP=$CROSSCOMPILER
  make -C components/synedit CPU_TARGET=powerpc PP=$CROSSCOMPILER
  make -C packager/registration CPU_TARGET=powerpc PP=$CROSSCOMPILER
fi

strip lazarus
strip startlazarus
strip lazbuild

# create symlinks
mkdir -p $BUILDDIR/bin
cd $BUILDDIR/bin
ln -s ../share/lazarus/lazarus lazarus
ln -s ../share/lazarus/startlazarus startlazarus
ln -s ../share/lazarus/lazbuild lazbuild

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

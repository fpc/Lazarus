#!/usr/bin/env bash

set -e
set -x

HDIUTIL=/usr/bin/hdiutil
UPDATELIST=~/tmp/updatelist

LazVersionPostfix=
if [ "$1" = "append-revision" ]; then
  LazVersionPostfix=$(../get_svn_revision_number.sh .)
  if [ -n "$LazVersionPostfix" ]; then
    LazVersionPostfix=.$LazVersionPostfix
  fi
  shift
fi

EditMode=
if [ "$1" = "edit" ]; then
  EditMode=$1
  shift
fi

#PPCARCH=ppcppc
ARCH=$(uname -p)
if [ "$ARCH" = "i386" ]; then
  PPCARCH=ppc386
fi

SVN=$(which svn)
if [ ! -e "$SVN" ]; then
  SVN=/usr/local/bin/svn
fi
if [ ! -e "$SVN" ]; then
  SVN=/sw/bin/svn
fi
if [ ! -e "$SVN" ]; then
  echo "Cannot find a svn executable"
  exit 1
fi

FREEZE=/usr/local/bin/freeze
if [ ! -e "$FREEZE" ]; then
  FREEZE=/usr/bin/freeze
fi
if [ ! -e "$FREEZE" ]; then
  echo "Cannot find freeze"
  exit 1
fi

cd ../../..
LAZSOURCEDIR=$(pwd)
cd -

cd ..
LazVersion=$(./get_lazarus_version.sh)
cd -
LazRelease='0'
# Iceberg only supports a major and a minor version number
LAZMAJORVERSION=$(echo $LazVersion | sed -e 's/\..*//')
LAZMINORVERSION=$(echo $LazVersion | sed -e 's/[^.]*\.//' -e 's/\..*//')

COMPILER=$PP
if [ -z "$COMPILER" ]; then
  COMPILER=$(which fpc)
fi
#~/fpc/bin/$PPCARCH
#CROSSCOMPILER=~/fpc/bin/fpc
FPCVERSION=$($COMPILER -iV)
BUILDDIR=~/tmp/buildlaz
ROOTDIR=$BUILDDIR/Root
LAZBUILDDIR=$ROOTDIR/Developer/lazarus
DATESTAMP=$(date +%Y%m%d)
TEMPLATEDIR=$LAZSOURCEDIR/tools/install/macosx
PACKPROJTEMPLATE=$TEMPLATEDIR/lazarus_release.packproj.template
PACKPROJ=$BUILDDIR/lazarus.packproj

# copy sources
rm -rf $BUILDDIR
mkdir -p $ROOTDIR/Developer
$SVN export $LAZSOURCEDIR $LAZBUILDDIR

#cp $LAZSOURCEDIR/lazarus $LAZBUILDDIR/
#cp -R $LAZSOURCEDIR/lazarus.app $LAZBUILDDIR/
#mkdir -p $LAZBUILDDIR/tools/install/
#cp -R $LAZSOURCEDIR/tools/install/macosx $LAZBUILDDIR/tools/install/
#cp -R $LAZSOURCEDIR/images $LAZBUILDDIR/

cd $LAZBUILDDIR
if [ ! -e tools/svn2revisioninc ]; then
  make tools PP=$COMPILER
fi
./tools/svn2revisioninc $LAZSOURCEDIR ide/revision.inc


#export FPCDIR=~/fpc/lib/fpc/$FPCVERSION

make bigide PP=$COMPILER USESVN2REVISIONINC=0
make lazbuilder PP=$COMPILER

# make non-default LCL platforms
make LCL_PLATFORM=gtk PP=$COMPILER lcl
make LCL_PLATFORM=gtk2 OPT="-dUseX" PP=$COMPILER lcl

# cross compilation units?
if [ -n "$CROSSCOMPILER" ]; then
  make lcl CPU_TARGET=powerpc PP=$CROSSCOMPILER
  make lcl CPU_TARGET=powerpc LCL_PLATFORM=gtk PP=$CROSSCOMPILER
  make lcl CPU_TARGET=powerpc LCL_PLATFORM=gtk2 OPT="-dUseX" PP=$CROSSCOMPILER
  make -C components/synedit CPU_TARGET=powerpc PP=$CROSSCOMPILER
  make -C packager/registration CPU_TARGET=powerpc PP=$CROSSCOMPILER
fi

strip lazarus
strip startlazarus
strip lazbuild

# create symlinks
mkdir -p $ROOTDIR/usr/bin
cd $ROOTDIR/usr/bin
ln -s /Developer/lazarus/lazarus.app/Contents/MacOS/lazarus lazarus
ln -s /Developer/lazarus/lazarus.app/Contents/MacOS/startlazarus startlazarus
ln -s /Developer/lazarus/lazbuild lazbuild
find $BUILDDIR -name '.svn' -exec rm -rf {} \; || true
find $BUILDDIR -name '.DS_Store' -exec rm -rf {} \; || true
mkdir -p $ROOTDIR/Applications
ln -s /Developer/lazarus/lazarus.app $ROOTDIR/Applications/Lazarus.app

# create etc
mkdir -p $ROOTDIR/etc/lazarus
cp $TEMPLATEDIR/environmentoptions.xml $ROOTDIR/etc/lazarus/

# fill in packproj template.
if [ -n "$EditMode" ]; then
  # edit mode => do not touch the template
  # and allow user to edit the lazarus.packproj file via Iceberg
  cp $PACKPROJTEMPLATE $PACKPROJ
  set +x
  echo ""
  echo "EDIT MODE:"
  echo "  You can now open $PACKPROJ with Iceberg"
  echo "  When you did your changes, close the project in Iceberg"
  echo "  copy the file back with"
  echo
  echo cp $PACKPROJ $PACKPROJTEMPLATE
  echo
  echo "  Then run the script again."
  exit 0
fi

cat $PACKPROJTEMPLATE | sed \
  -e "s|_LAZVERSION_|$LazVersion|g" \
  -e "s|_DATESTAMP_|$DATESTAMP|g" \
  -e "s/18273645/$LAZMAJORVERSION/g" \
  -e "s/45362718/$LAZMINORVERSION/g" \
  > $PACKPROJ

# build package
cd $BUILDDIR
#$FREEZE -v $PACKPROJ

exit 0

DMGFILE=~/tmp/lazarus-$LazVersion$LazVersionPostfix-$DATESTAMP-$FPCARCH-macosx.dmg
rm -rf $DMGFILE

$HDIUTIL create -anyowners -volname lazarus-$LAZVERSION$LazVersionPostfix \
  -imagekey zlib-level=9 -format UDZO -srcfolder $BUILDDIR/build $DMGFILE

echo The new dmg file is $DMGFile

#end.


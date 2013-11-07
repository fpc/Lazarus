#!/usr/bin/env bash
#
# This script requires an installed 'Iceberg'
#
# Usage:
#   ./create_lazarus_dmg.sh [chmhelp] [edit]
#
# This will setup a lazarus package in ~/tmp/buildlaz
# run 'freeze' of Iceberg and create the dmg.
#
# Options:
#   chmhelp          add package chmhelp and add chm,kwd files in docs/chm
#   append-revision  append svn revision to dmg
#
# To edit the Iceberg configuration 'lazarus.packproj', run
#   ./create_lazarus_dmg.sh edit
# This will create ~/tmp/buildlaz/lazarus.packproj. Open the file with Iceberg.
# When changed, save the file. Copy it back to the lazarus sources:
# cp ~/tmp/buildlaz/lazarus.packproj <lazarus>/tools/install/macosx/lazarus_release.packproj.template
#
# The script will replace the following in the lazarus.packproj:
#   _LAZVERSION_ with $LazVersion
#   _DATESTAMP_ with $DATESTAMP
#   18273645 with $LAZMAJORVERSION
#   45362718 with $LAZMINORVERSION

set -e
set -x

HDIUTIL=/usr/bin/hdiutil

LazVersionPostfix=
UseCHMHelp=
EditMode=

while [ $# -gt 0 ]; do
  echo "param=$1"
  case "$1" in
  chmhelp)
    echo "using package chmhelp"
    UseCHMHelp=1
    ;;

  append-revision)
    LazRevision=$(../get_svn_revision_number.sh .)
    if [ -n "$LazRevision" ]; then
      LazVersionPostfix=$LazVersionPostfix.$LazRevision
    fi
    echo "Appending svn revision $LazVersionPostfix to dmg name"
    ;;

  edit)
    EditMode=1
    ;;

  *)
    echo "invalid parameter $1"
    echo "Usage: ./create_lazarus_dmg.sh [chmhelp] [append-revision] [edit]"
    exit 1
    ;;
  esac
  shift
done

PPCARCH=ppcppc
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
LazVersion=$(./get_lazarus_version.sh | sed -e 's/[a-zA-Z\-]//g')
cd -
LazRelease='0'
# Iceberg only supports a major and a minor version number
# convert 1.2.3 => 1.23 and 0.9.30.2RC1 => 0.93021
LAZMAJORVERSION=$(echo $LazVersion | sed -e 's/\..*//')
LAZMINORVERSION=$(echo $LazVersion | sed -e 's/[^.]*\.//' -e 's/\.//g'  | tr -d [A-Za-z])

COMPILER=$PP
if [ -z "$COMPILER" ]; then
  COMPILER=$(which fpc)
fi
FPCARCH=$($COMPILER -iSP)
FPCVERSION=$($COMPILER -iV)
BUILDDIR=~/tmp/buildlaz
ROOTDIR=$BUILDDIR/Root
LAZBUILDDIR=$ROOTDIR/Developer/lazarus
DATESTAMP=$(date +%Y%m%d)
TEMPLATEDIR=$LAZSOURCEDIR/tools/install/macosx
PACKPROJTEMPLATE=$TEMPLATEDIR/lazarus.packproj.template
PACKPROJ=$BUILDDIR/lazarus.packproj
#MACOSX104LINKEROPTS="-k-macosx_version_min -k10.4 -XR/Developer/SDKs/MacOSX10.4u.sdk/"
MACOSX105LINKEROPTS="-WM10.5"

# copy sources
rm -rf $BUILDDIR
mkdir -p $ROOTDIR/Developer
$SVN export $LAZSOURCEDIR $LAZBUILDDIR

if [ "$UseCHMHelp" = "1" ]; then
  echo
  echo "Copying chm files"
  cd $LAZSOURCEDIR/docs/chm
  cp -v *.kwd *.chm $LAZBUILDDIR/docs/chm/
  cd -
fi

#cp $LAZSOURCEDIR/lazarus $LAZBUILDDIR/
#cp -R $LAZSOURCEDIR/lazarus.app $LAZBUILDDIR/
#mkdir -p $LAZBUILDDIR/tools/install/
#cp -R $LAZSOURCEDIR/tools/install/macosx $LAZBUILDDIR/tools/install/
#cp -R $LAZSOURCEDIR/images $LAZBUILDDIR/

cd $LAZBUILDDIR
make bigide PP=$COMPILER USESVN2REVISIONINC=1 OPT="$MACOSX105LINKEROPTS"

# clean up
strip lazarus
strip startlazarus
strip lazbuild
strip tools/lazres
strip tools/updatepofiles
strip tools/lrstolfm
strip tools/svn2revisioninc
if [ -f components/chmhelp/lhelp/lhelp ]; then
  strip components/chmhelp/lhelp/lhelp
fi
find $BUILDDIR -name '.svn' -exec rm -rf {} \; || true
find $BUILDDIR -name '.DS_Store' -exec rm -rf {} \; || true

# create symlinks
mkdir -p $ROOTDIR/usr/local/bin
cd $ROOTDIR/usr/local/bin
ln -s /Developer/lazarus/lazbuild lazbuild
cp $TEMPLATEDIR/uninstall.sh $ROOTDIR/Developer/lazarus/

# create /Applications/Lazarus.app alias
mkdir -p $ROOTDIR/Applications
ln -s /Developer/lazarus/lazarus.app $ROOTDIR/Applications/Lazarus.app

# fix permissions
# everyone can read, group can write
find $BUILDDIR -exec chmod a+r,g+w {} \;
# what is executable should be executable by everyone
find $BUILDDIR -perm +o+x -exec chmod a+x {} \;
# everyone can access directories
find $BUILDDIR -type d -exec chmod a+x {} \;

# create etc
mkdir -p $ROOTDIR/etc/lazarus
cat $TEMPLATEDIR/environmentoptions.xml | sed \
  -e "s|_PPCARCH_|$PPCARCH|g" \
  > $ROOTDIR/etc/lazarus/environmentoptions.xml

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
$FREEZE -v $PACKPROJ

DMGFILE=~/tmp/lazarus-$LazVersion$LazVersionPostfix-$DATESTAMP-$FPCARCH-macosx.dmg
rm -rf $DMGFILE

$HDIUTIL create -anyowners -volname lazarus-$LazVersion$LazVersionPostfix \
  -imagekey zlib-level=9 -format UDZO -srcfolder $BUILDDIR/build $DMGFILE

set +x
echo The new dmg file is $DMGFILE

#end.


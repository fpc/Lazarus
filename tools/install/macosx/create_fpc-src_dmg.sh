#!/usr/bin/env bash
#
# This script requires an installed 'Iceberg'
#
# Usage:
#   ./create_fpc-src_dmg.sh [edit] <fpc-src-directory>
#
# This will setup a fpc-src package in ~/tmp/buildfpcsrc
# run 'freeze' of Iceberg and create the dmg.
#
# To edit the Iceberg configuration 'lazarus.packproj', run
#   ./create_fpc-src_dmg.sh edit
# This will create ~/tmp/buildfpcsrc/fpcsrc.packproj. Open the file with Iceberg.
# When changed, save the file. Copy it back to the lazarus sources:
# cp ~/tmp/buildfpcsrc/fpcsrc.packproj <lazarus>/tools/install/macosx/fpcsrc_release.packproj.template
#
# The script will replace the following in the fpcsrc.packproj:
#   _FPCVERSION_ with $FPCVersion
#   _DATESTAMP_ with $DATESTAMP
#   18273645 with $LAZMAJORVERSION
#   45362718 with $LAZMINORVERSION

set -e
set -x

HDIUTIL=/usr/bin/hdiutil

FPCVersionPostfix=
EditMode=
FPCSrcDir=

while [ $# -gt 0 ]; do
  echo "param=$1"
  case "$1" in
  edit)
    EditMode=1
    ;;

  *)
    if [ -n "$FPCSrcDir" ]; then
        echo "unknown parameter $1"
        exit 1
    fi

    FPCSrcDir=$1
    ;;
  esac
  shift
done

if [ -z "$FPCSrcDir" ]; then
    echo "Usage: ./create_fpc-src_dmg.sh [edit] fpc-src-dir"
    exit 1
fi
if [ -d "$FPCSrcDir" ]; then
    echo "FPCSrcDir=$FPCSrcDir";
else
    echo "invalid fpcdir $FPCSrcDir"
    exit 1
fi
if [ ! -d "$FPCSrcDir/.svn" ]; then
    echo "missing fpcdir svn $FPCSrcDir/.svn"
    exit 1
fi
if [ ! -d "$FPCSrcDir/compiler" ]; then
    echo "missing fpcdir svn $FPCSrcDir/compiler"
    exit 1
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

# get FPC source version
echo -n "getting FPC version from local svn ..."
VersionFile="$FPCSrcDir/compiler/version.pas"
CompilerVersion=$(cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerRelease=$(cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerPatch=$(cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g')
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
FPCVersion="$CompilerVersion.$CompilerRelease.$CompilerPatch"
echo " $CompilerVersionStr"

# Iceberg only supports a major and a minor version number
# convert 1.2.3 => 1.23 and 0.9.30.2RC1 => 0.93021
FPCMAJORVERSION=$(echo $FPCVersion | sed -e 's/\..*//')
FPCMINORVERSION=$(echo $FPCVersion | sed -e 's/[^.]*\.//' -e 's/\.//g'  | tr -d [A-Za-z])

BUILDDIR=~/tmp/buildfpcsrc
ROOTDIR=$BUILDDIR/Root
FPCBUILDDIR=$ROOTDIR/Developer/fpcsrc
DATESTAMP=$(date +%Y%m%d)
TEMPLATEDIR=$(pwd)
PACKPROJTEMPLATE=$TEMPLATEDIR/fpcsrc.packproj.template
PACKPROJ=$BUILDDIR/fpcsrc.packproj

# copy sources
rm -rf $BUILDDIR
mkdir -p $ROOTDIR/Developer
$SVN export $FPCSrcDir $FPCBUILDDIR

find $BUILDDIR -name '.svn' -exec rm -rf {} \; || true
find $BUILDDIR -name '.DS_Store' -exec rm -rf {} \; || true

#cp $TEMPLATEDIR/uninstall.sh $ROOTDIR/Developer/lazarus/

# fix permissions
# everyone can read, group can write
find $BUILDDIR -exec chmod a+r,g+w {} \;
# what is executable should be executable by everyone
# Note: OS X does not understand /o+x
find $BUILDDIR -perm +0001 -exec chmod a+x {} \;
# everyone can access directories
find $BUILDDIR -type d -exec chmod a+x {} \;

# fill in packproj template.
if [ -n "$EditMode" ]; then
  # edit mode => do not touch the template
  # and allow user to edit the fpcsrc.packproj file via Iceberg
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
  -e "s|_FPCVERSION_|$FPCVersion|g" \
  -e "s|_DATESTAMP_|$DATESTAMP|g" \
  -e "s/_FPCMAJORVERSION_/$FPCMAJORVERSION/g" \
  -e "s/_FPCMINORVERSION_/$FPCMINORVERSION/g" \
  > $PACKPROJ

# build package
cd $BUILDDIR
$FREEZE -v $PACKPROJ

DMGFILE=~/tmp/fpc-src-$FPCVersion-$DATESTAMP-$FPCARCH-macosx.dmg
rm -rf $DMGFILE

$HDIUTIL create -anyowners -volname lazarus-$FPCVersion \
  -imagekey zlib-level=9 -format UDZO -srcfolder $BUILDDIR/build $DMGFILE

set +x
echo The new dmg file is $DMGFILE

#end.


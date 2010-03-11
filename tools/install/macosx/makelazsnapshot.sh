#!/usr/bin/env bash

set -e
set -x

LAZSOURCEDIR=$1
if [ x$LAZSOURCEDIR == x ]; then
  LAZSOURCEDIR=~/src/lazsource
fi

HDIUTIL=/usr/bin/hdiutil
UPDATELIST=~/tmp/updatelist

PPCARCH=ppcppc
ARCH=`uname -p`
if [ "$ARCH" = "i386" ]; then
  PPCARCH=ppc386
  export CROSSCOMPILER=~/fpc/bin/ppcppc
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

export FPC=fpc

$SVN up $LAZSOURCEDIR
cd $LAZSOURCEDIR/tools/install/macosx
./create_lazarus_dmg.sh append-revision

cd ..
LazVersion=$(./get_lazarus_version.sh)
LazFullVersion=$LazVersion.$(./get_svn_revision_number.sh .)
cd -
DATESTAMP=$(date +%Y%m%d)
FPCARCH=$($FPC -iSP)
DMGFILE=~/tmp/lazarus-$LazFullVersion-$DATESTAMP-$FPCARCH-macosx.dmg

if [ -e $DMGFILE ]; then
#update lazarus snapshot web page
  echo "$DMGFILE 'lazarus-$LazVersion.?????-*-$FPCARCH-macosx.dmg' " >> $UPDATELIST
fi

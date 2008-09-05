#!/bin/bash

set -x
set -e

# get date of day
Year=$(date +%y)
Month=$(date +%m)
Day=$(date +%d)

# get installed fpc version
echo "getting installed fpc version ..."
if ! which ppc386  ; then
  echo ERROR: fpc not installed
  exit
fi
FPCVER=`ppc386 -l | head -n 1 | awk '{print $5}'`
echo "installed fpc version: $FPCVER"

Date=$Year$Month$Day
LazVersion=$(./get_lazarus_version.sh)
LazRelease='0' # $(echo $FPCRPM | sed -e 's/-/_/g')
TmpDir=~/tmp/lazarus$LazVersion
Src=~/tmp/lazarus-$LazVersion-$LazRelease.tar.gz
SlackTGZ=~/tmp/lazarus-$LazVersion-i486-$LazRelease.tgz
DescFile=slacktgz/slack-desc
DepFile=slacktgz/slack-required
BuildRoot=~/tmp/lazaruspackage/
SrcDir="$TmpDir/lazarus"
Where=`pwd`

# download lazarus svn if needed
echo "creating lazarus tgz ..."
  ./create_lazarus_export_tgz.sh $Src
  rm -rf $TmpDir
  mkdir -p $TmpDir
  cd $TmpDir
  tar zxvf $Src
  cd $Where

# create a slack-desc  file
echo "creating lazarus slack metadata files ..."
echo "fpc >= $FPCVER" > $DepFile
echo "fpcsrc >= $FPCVER" >> $DepFile

# build slacktgz
echo "building slackware tgz package ..."
if [ -d $BuildRoot ] ; then
  rm -fr $BuildRoot
fi
mkdir $BuildRoot
mkdir -p $BuildRoot/install
cp $DepFile $BuildRoot/install/
cp $DescFile $BuildRoot/install/

./slacktgz/build.sh $SrcDir


cd $BuildRoot
mkdir -p $BuildRoot/usr/lib/lazarus
mkdir -p $BuildRoot/usr/bin
mkdir -p $BuildRoot/usr/share/pixmaps
mkdir -p $BuildRoot/usr/share/applications
mkdir -p $BuildRoot/usr/man/man1
cp -arf $SrcDir/* $BuildRoot/usr/lib/lazarus/

  cp  $SrcDir/images/ide_icon48x48.png $BuildRoot/usr/share/pixmaps/lazarus.png
  cp  $SrcDir/install/lazarus.desktop $BuildRoot/usr/share/applications/lazarus.desktop
    ln -sf /usr/lib/lazarus/lazarus usr/bin/lazarus-ide
    ln -sf /usr/lib/lazarus/startlazarus usr/bin/startlazarus
    ln -sf /usr/lib/lazarus/lazbuild usr/bin/lazbuild
    cat $SrcDir/install/man/man1/lazbuild.1 | gzip > $BuildRoot/usr/man/man1/lazbuild.1.gz

/sbin/makepkg -l y -c y $SlackTGZ
cd

#Clean up
rm -fr $BuildRoot  $TmpDir
    
echo "The new slackware tgz can be found at: $SlackTGZ"
echo "A source package is has been created at: $Src"
# end.


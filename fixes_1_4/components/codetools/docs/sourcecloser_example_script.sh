#!/bin/bash

set -e
set -x

# safety: switch to the package directory
path=$0
path=${path%/*}
cd "$path"

PkgName=pascon2013
ReleaseDir=~/tmp/release_of_$PkgName
SrcDir=.
Zip=~/tmp/$PkgName.zip

# clone directory
rm -rf $ReleaseDir
cp -a $SrcDir $ReleaseDir

# delete development files
rm -rf $ReleaseDir/lib $ReleaseDir/*.sh
find $ReleaseDir -type d -name '.svn' -d -exec rm -rf {} \;
find $ReleaseDir -name '*.cache' -d -exec rm -rf {} \;

# convert lpk, add -Ur, set "Build Manually"
sourcecloser $ReleaseDir/$PkgName.lpk

# build package
lazbuild -B $ReleaseDir/$PkgName.lpk

# clean up: remove .compiled
find $ReleaseDir -name '*.compiled' -delete

# remove compile commands
sourcecloser --disablecompile $ReleaseDir/$PkgName.lpk

# remove implementation from all units
sourcecloser --define=NeededFlag $ReleaseDir/*.pas

# zip
zip -r $Zip $ReleaseDir

echo "Successfully created $Zip"


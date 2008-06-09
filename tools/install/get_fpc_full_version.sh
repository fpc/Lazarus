#!/usr/bin/env bash

# retrieve the version information
VersionFile="$1/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerMinorPatch=`cat $VersionFile | grep ' *minorpatch *=.*;' | sed -e 's/.*minorpatch.*= *//g' -e "s/'//g" -e 's/;//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
LazVersion="$CompilerVersion.$CompilerRelease.$CompilerPatch$CompilerMinorPatch"

echo $LazVersion
# end.


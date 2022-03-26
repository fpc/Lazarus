#!/bin/bash

# SET THE CORRECT PATHS FOR YOUR SYSTEM
# path to Lazarus docs directory
docdir="../../../docs"

# path to fpdoc executable
#fpcdir="../../../fpc/3.2.2/bin/x86_64-win64"
fpcdir="../../../../fpc331/fpc/bin/x86_64-win64"

echo "RTTIControls package"

# generate chm format without footers
# imports done manually to set the correct prefix for the output format
# output to current directory
$fpcdir/fpdoc.exe --project=rtticontrols-project.xml --format=chm --import="$docdir/chm/rtl.xct,ms-its:rtl.chm::/" --import="$docdir/chm/fcl.xct,ms-its:fcl.chm::/" --import="$docdir/chm/lcl.xct,ms-its:lcl.chm::/" --import="$docdir/chm/lazutils.xct,ms-its:lazutils.chm::/"

# copy generated chm help to lazarus docs directory
cp -v runtimetypeinfocontrols.{chm,xct} $docdir/chm/

# generate html format without footers
# imports done manually to set the correct prefix for the output format
# html written to rtticontrols sub-directory
$fpcdir/fpdoc.exe --project=rtticontrols-project.xml --format=html --import="$docdir/chm/rtl.xct,../rtl/" --import="$docdir/chm/fcl.xct,../fcl/" --import="$docdir/chm/lcl.xct,../lcl/" --import="$docdir/chm/lazutils.xct,../lazutils/" --output=rtticontrols

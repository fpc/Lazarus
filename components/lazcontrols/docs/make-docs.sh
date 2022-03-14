#!/bin/bash
#======================================================================
# Builds CHM and HTML help files for the LazControls package using fpdoc.
# Requires chm and xct files for rtl, fcl, lcl, and lazutils in the
# Lazarus docs/chm directory.
# fpdoc will fail if they are not present.
#======================================================================

#=======================================
# SET THE CORRECT PATHS FOR YOUR SYSTEM
#=======================================
# lazarus documentation directory
docdir="../../../docs"

# fpdoc executable directory
#fpcdir="../../../fpc/3.2.2/bin/x86_64-win64"
fpcdir="../../../../fpc331/fpc/bin/x86_64-win64"

echo -e "\n\e[7m LazControls package \e[0m";

# generate chm format without footers
# imports done manually to set the correct prefix for the output format
# output to current directory
echo "Generating CHM help..."
$fpcdir/fpdoc --project=lazcontrols-project.xml --format=chm --import="$docdir/chm/rtl.xct,ms-its:rtl.chm::/" --import="$docdir/chm/fcl.xct,ms-its:fcl.chm::/" --import="$docdir/chm/lcl.xct,ms-its:lcl.chm::/" --import="$docdir/chm/lazutils.xct,ms-its:lazutils.chm::/" 2>&1 | tee ./build_chm.log

# generate html format without footers
# imports done manually to set the correct prefix for the output format
# html written to lazcontrols sub-directory
echo "Generating HTML help..."
$fpcdir/fpdoc --project=lazcontrols-project.xml --format=html --import="$docdir/chm/rtl.xct,../rtl/" --import="$docdir/chm/fcl.xct,../fcl/" --import="$docdir/chm/lcl.xct,../lcl/" --import="$docdir/chm/lazutils.xct,../lazutils/" --output=lazcontrols  2>&1 | tee ./build_html.log

# copy generated chm, xct to lazarus docs directory
cp -v lazcontrols.{chm,xct} $docdir/chm/

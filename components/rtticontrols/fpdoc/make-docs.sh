#!/bin/bash

# SET THE CORRECT PATHS FOR YOUR SYSTEM
# path to Lazarus docs directory
docdir="../../../docs"

# path to fpdoc executable
fpcdir="../../../fpc/3.2.2/bin/x86_64-win64"
#fpcdir="../../../../fpc331/fpc/bin/x86_64-win64"

# info for footers... not used atm
#verno="2.3.0"
#commit=`git describe --long | cut -d "-" -f 4 | cut -b "2-"`
dt=`date +"%Y-%m-%d"`

echo "(RttiControls) RTTI Controls Package"

# generate chm format with date footer
# imports done manually to set the correct prefix for the output format
# output to current directory
$fpcdir/fpdoc --project=rtticontrols-project.xml --format=chm --footer-date="yyyy-mm-dd" --import="$docdir/chm/rtl.xct,ms-its:rtl.chm::/" --import="$docdir/chm/fcl.xct,ms-its:fcl.chm::/" --import="$docdir/chm/lcl.xct,ms-its:lcl.chm::/" --import="$docdir/chm/lazutils.xct,ms-its:lazutils.chm::/" 2>&1 | tee ./build_chm.log

# generate html format with date footer
# imports done manually to set the correct prefix for the output format
# html written to rtticontrols directory
$fpcdir/fpdoc --project=rtticontrols-project.xml --format=html --footer-date="yyyy-mm-dd" --import="$docdir/chm/rtl.xct,../rtl/" --import="$docdir/chm/fcl.xct,../fcl/" --import="$docdir/chm/lcl.xct,../lcl/" --import="$docdir/chm/lazutils.xct,../lazutils/" --output=rtticontrols 2>&1 | tee ./build_html.log

# copy generated chm help to lazarus docs directory
#cp -v runtimetypeinfocontrols.{chm,xct} $docdir/chm/

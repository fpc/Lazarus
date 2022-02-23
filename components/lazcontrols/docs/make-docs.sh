#!/bin/bash
#======================================================================
# Builds CHM and HTML help files for the LazControls package using the fpdoc utility.
# Requires chm and xct files for rtl, fcl, lcl, and lazutils in the Lazarus docs/chm directory.
# fpdoc will fail if they are not present.
# Requires git command line client to get the commit hash for the repository.
#======================================================================

#=======================================
# SET THE CORRECT PATHS FOR YOUR SYSTEM
#=======================================
# lazarus documentation directory
docdir="../../../docs"
# fpdoc executable directory
#fpcdir="../../../../lazarus-2.2.0-RC2/fpc/3.2.2/bin/x86_64-win64"
fpcdir="../../../../fpc331/fpc/bin/x86_64-win64"
# directory with lazarus git repository
gitdir="../../../../usr/work/git-lazarus"
# values used in the footer
# package title for CHM
pkgtitle="Lazarus Controls Package (LazControls)"
# CHANGE THIS BEFORE BUILDING
version="2.3.0"
# commit SHA
commit=`git -C "$gitdir" describe --all --long | cut -d "-" -f 3 | cut -b "2-"`
# date stamp in YYYY-MM-DD format
dtstamp=`date +"%Y-%m-%d"`

echo -e "\n\e[7m $pkgtitle $version—$commit $dtstamp \e[0m";

# generate footer with title, version, commit SHA, date stamp
# footer is included using the fpdoc project file
cat << EOT > lazcontrols-footer.xml
<hr class="footer-sep"/>
<table class="footer">
   <tr>
    <td class="footer-doc">$pkgtitle</td>
    <td class="footer-ver">Version $version-$commit</td>
    <td class="footer-date">Generated $dtstamp</td>
  </tr>
</table>
EOT

# generate chm format
# imports done manually to set the correct prefix for the output format
# output to current directory
echo "Generating CHM help..."
$fpcdir/fpdoc --project=lazcontrols-project.xml --format=chm --verbose --import="$docdir/chm/rtl.xct,ms-its:rtl.chm::/" --import="$docdir/chm/fcl.xct,ms-its:fcl.chm::/" --import="$docdir/chm/lcl.xct,ms-its:lcl.chm::/" --import="$docdir/chm/lazutils.xct,ms-its:lazutils.chm::/" 2>&1 | tee ./build_chm.log

# generate html format
# imports done manually to set the correct prefix for the output format
# html written to lazcontrols sub-directory
echo "Generating HTML help..."
$fpcdir/fpdoc --project=lazcontrols-project.xml --format=html --verbose --import="$docdir/chm/rtl.xct,../rtl/" --import="$docdir/chm/fcl.xct,../fcl/" --import="$docdir/chm/lcl.xct,../lcl/" --import="$docdir/chm/lazutils.xct,../lazutils/" --output=lazcontrols  2>&1 | tee ./build_html.log

# convert CRLF to LF so git doesn't complain
#echo "Converting CRLF to LF in HTML files..."
#cd lazcontrols
#dos2unix *.{css,html}
#dos2unix **/*.{css,html}
#cd ..

# create an archive file with html content
#echo "Creating archive: docs-html-lazcontrols-$dtstamp.7z"
#7z a -t7z -mx9 -r docs-html-lazcontrols-$dtstamp.7z ./lazcontrols/ > /dev/null

# copy or move generated html to lazarus docs/html directory
#rm -vrf $docdir/html/lazcontrols/*
#cp -vrf ./lazcontrols/* $docdir/html/lazcontrols/

# copy or move generated chm, xct to lazarus docs directory
#cp -v lazcontrols.{chm,xct} $docdir/chm/

# discard generated footer file
rm lazcontrols-footer.xml

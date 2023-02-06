#!/bin/bash

#==========================================================================
# Builds CHM and HTML help files for the DateTimeCtrls package using fpdoc.
# Requires chm and xct files for rtl, fcl, lcl, and lazutils in the
# Lazarus docs/chm directory.
# fpdoc will fail if they are not present.
#==========================================================================

#==========================================================================
# SET THE CORRECT PATHS FOR YOUR SYSTEM
# docdir = path to lazarus documentation directory
# fpcdir = path to fpdoc executable
# gitpath = path to git repository for lazarus
#==========================================================================

# lazarus documentation directory
docdir="../../../docs"

# fpdoc executable directory
fpcdir="../../../fpc/3.2.2/bin/x86_64-win64"
#fpcdir="../../../../fpc331/fpc/bin/x86_64-win64"

# lazarus repository directory
#gitpath="../../../../usr/work/git-lazarus"
gitpath=""

# version number is hard-coded because main-2_3 tag is not the format needed
verno="2.3.0"
dt=`date +"%Y-%m-%d"`
# get only the commit hash
#commit=`git -C "$gitpath" describe --all --long | cut -d "-" -f 3 | cut -b "2-"`

# chm and html footer files are the same... for now
# chm footer should have a link to the online HTML
cat <<EOT > ./datetimectrls-chm-footer.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Date/Time Controls Package (DateTimeCtrls)</td>
    <td class="footer-ver">Version $verno</td>
    <td class="footer-date">Generated $dt</td>
  </tr>
</table>
EOT

cat <<EOT > ./datetimectrls-html-footer.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Date/Time Controls Package (DateTimeCtrls)</td>
    <td class="footer-ver">Version $verno</td>
    <td class="footer-date">Generated $dt</td>
  </tr>
</table>
EOT

echo -e "\n\e[7m DateTimeCtrls package \e[0m";

# generate chm format with footers and chm title
# imports done manually to set the correct prefixs for the chm format
# output to current directory
echo "Generating CHM help..."
chmTitle="(DateTimeCtrls) Date/Time Controls"
$fpcdir/fpdoc --project=datetimectrls-project.xml --format=chm --chm-title="$chmTitle" --footer="datetimectrls-chm-footer.xml" --import="$docdir/chm/rtl.xct,ms-its:rtl.chm::/" --import="$docdir/chm/fcl.xct,ms-its:fcl.chm::/" --import="$docdir/chm/lcl.xct,ms-its:lcl.chm::/" --import="$docdir/chm/lazutils.xct,ms-its:lazutils.chm::/" 2>&1 | tee ./build_chm.log

# generate html format with footers
# imports done manually to set the correct prefixs for the html format
# html written to the datetimectrls sub-directory
echo "Generating HTML help..."
$fpcdir/fpdoc --project=datetimectrls-project.xml --format=html --footer="datetimectrls-html-footer.xml" --import="$docdir/chm/rtl.xct,../rtl/" --import="$docdir/chm/fcl.xct,../fcl/" --import="$docdir/chm/lcl.xct,../lcl/" --import="$docdir/chm/lazutils.xct,../lazutils/" --output=datetimectrls  2>&1 | tee ./build_html.log

# copy generated chm, xct to lazarus docs directory
cp -v datetimectrls.{chm,xct} $docdir/chm/
# copy css file to output directory for generated html content
cp -v datetimectrls.css datetimectrls/

# run make-archive.sh if needed
# run clean-files.sh if needed

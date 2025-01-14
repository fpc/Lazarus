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
docdir=$(realpath ../../../docs)
chmdir=../../../docs/chm

# path to fpdoc executable and file name
#fpcdir=$(realpath ../../../../fpc/bin/x86_64-linux)
fpcdir=$(realpath ~/fpc331/fpc/bin/x86_64-linux)
fpdocpgm=$fpcdir/fpdoc

# lazarus repository directory
gitpath=$(realpath ../../..)

# version number, commit hash, generated date, link info for footer files
#verno="4.99"
#verno="4.0.0-RC2"

vermaj=`git -C $gitpath describe --long --always | cut -d "-" -f 1 | cut -d "_" -f 2`
vermin=`git -C $gitpath describe --long --always | cut -d "-" -f 1 | cut -d "_" -f 3`
verno=$vermaj.$vermin
commit=`git -C $gitpath describe --all --long | cut -d "-" -f 3 | cut -b "2-"`
dt=`date +"%Y-%m-%d"`
linkurl="https://dsiders.gitlab.io/lazdocsnext/"
#linkurl="https://lazarus-ccr.sourceforge.io/docs/"
linktext="Home"

# chm and html footer files are the same... for now
# chm footer should have a link to the online HTML
cat <<EOT > ./datetimectrls-chm-footer.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Date/Time Controls Package (DateTimeCtrls)</td>
    <td class="footer-ver">Version $verno-$commit</td>
    <td class="footer-date">Generated $dt</td>
  </tr>
</table>
EOT

cat <<EOT > ./datetimectrls-html-footer.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Date/Time Controls Package (DateTimeCtrls)</td>
    <td class="footer-ver">Version $verno-$commit ($dt)</td>
    <td class="footer-date">
      <a href="$linkurl">$linktext</a>
    </td>
  </tr>
</table>
EOT

echo -e "\n\e[7m DateTimeCtrls package \e[0m";

# generate chm format with footers and chm title
# imports done manually to set the correct prefixs for the chm format
# output to current directory
echo "Generating CHM help..."
chmTitle="(DateTimeCtrls) Date/Time Controls"
$fpdocpgm --project=datetimectrls-project.xml --format=chm --chm-title="$chmTitle" --footer=@datetimectrls-chm-footer.xml \
  --import="$chmdir/rtl.xct,ms-its:rtl.chm::/" --import="$chmdir/fcl.xct,ms-its:fcl.chm::/" \
  --import="$chmdir/lcl.xct,ms-its:lcl.chm::/" --import="$chmdir/lazutils.xct,ms-its:lazutils.chm::/" \
  2>&1 | tee ./build_chm.log

# generate html format with footers
# imports done manually to set the correct prefixs for the html format
# html written to the datetimectrls sub-directory
echo "Generating HTML help..."
$fpdocpgm --project=datetimectrls-project.xml --format=html --footer=@datetimectrls-html-footer.xml \
  --import="$chmdir/rtl.xct,../rtl/" --import="$chmdir/fcl.xct,../fcl/" \
  --import="$chmdir/lcl.xct,../lcl/" --import="$chmdir/lazutils.xct,../lazutils/" --output=datetimectrls  \
  2>&1 | tee ./build_html.log

# copy generated chm, xct to lazarus docs directory
cp -v datetimectrls.{chm,xct} $chmdir/
# copy css file to output directory for generated html content
cp -v datetimectrls.css datetimectrls/

# ./make-archive.sh
# ./clean-files.sh

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

# lazarus documentation and chm directories
docdir=$(realpath ../../../docs)
chmdir=$docdir/chm

# path to fpdoc executable and file name
fpcdir=$(realpath ../../../../fpc/bin/x86_64-linux)
#fpcdir=$(realpath ~/fpc331/fpc/bin/x86_64-linux)
fpdocpgm=$fpcdir/fpdoc

# path to lazarus git repo
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

#chm format footer file
cat <<EOT > ./lazcontrols-chm-footer.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Lazarus Controls Package (LazControls)</td>
    <td class="footer-ver">Version $verno-$commit</td>
    <td class="footer-date">Generated $dt</td>
  </tr>
</table>
EOT

# html format footer file
cat <<EOT > ./lazcontrols-html-footer.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Lazarus Controls Package (LazControls)</td>
    <td class="footer-ver">Version $verno-$commit ($dt)</td>
    <td class="footer-date">
      <a href="$linkurl">$linktext</a>
    </td>
  </tr>
</table>
EOT

echo -e "\n\e[7m LazControls package \e[0m";

# generate chm format
# imports done manually to set the correct prefix for the output format
# output to current directory
echo "Generating CHM help..."
$fpdocpgm --project=lazcontrols-project.xml --format=chm --footer="lazcontrols-chm-footer.xml" --import="$chmdir/rtl.xct,ms-its:rtl.chm::/" --import="$chmdir/fcl.xct,ms-its:fcl.chm::/" --import="$chmdir/lcl.xct,ms-its:lcl.chm::/" --import="$chmdir/lazutils.xct,ms-its:lazutils.chm::/" 2>&1 | tee ./build_chm.log

# generate html format
# imports done manually to set the correct prefix for the output format
# html written to lazcontrols sub-directory
echo "Generating HTML help..."
$fpdocpgm --project=lazcontrols-project.xml --format=html --footer="lazcontrols-html-footer.xml" --import="$chmdir/rtl.xct,../rtl/" --import="$chmdir/fcl.xct,../fcl/" --import="$chmdir/lcl.xct,../lcl/" --import="$chmdir/lazutils.xct,../lazutils/" --output=lazcontrols  2>&1 | tee ./build_html.log

# copy generated chm, xct to chm directory
mv -v lazcontrols.{chm,xct} $chmdir/

# generate an archive for html content
7z a -t7z -mx9 -r docs-html-lazcontrols-$dt.7z ./lazcontrols/
#mv -v docs-html-lazcontrols-$dt.7z $docdir/build/html/

# clean up
rm -rf ./lazcontrols
rm -v lazcontrols-*-footer.xml
#rm -v *.log
#rm -v *.7z

#!/bin/bash

#==============================================================
# builds Lazarus and LazUtils CHM documentation
# preferably: use trunk fpdoc: --fpdoc /path/to/fpc/trunk/utils/fpdoc/fpdoc
# run ../../lazbuild ./build_lcl_docs.lpi to build .exe when needed
# change any path information used in this script for your system
#==============================================================

#==============================================================
# uses version.inc, commit.inc, date.inc for values used in footers
# git describe doesn't return a usable branch or tag name like 2.3.0
# main-2_3 and fixes_2_2 are not what is needed for the doc files
# usable branches or tags like 2.3.0 or 2.2.0-RC2 do not exist
# just get or set the text used for version.inc
#==============================================================
verno="2.3.0"
echo $verno > version.inc
#verno=`cat version.inc | dos2unix --`

#==============================================================
#svn info https://svn.freepascal.org/svn/lazarus/trunk --show-item revision > ./revision.inc
# change gitpath to the local repo on your system
# get hash for the commit used in the build
#==============================================================
gitpath="c:/usr/work/git-lazarus"
commit=`git -C $gitpath describe --all --long | cut -d "-" -f 3 | cut -b "2-"`
echo $commit > ./commit.inc

#==============================================================
# get datestamp for the footer
#==============================================================
dt=`date +"%Y-%m-%d"`
echo $dt > ./date.inc

#==============================================================
# rewrite footer files for LCL
# locallclfooter.xml is the version without a link to the online HTML
# chmlclfooter.xml includes a link to the online HTML
#==============================================================
cat <<EOT > ./locallclfooter.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Lazarus Component Library (LCL)</td>
    <td class="footer-ver">Version $verno-$commit</td>
    <td class="footer-date">Generated $dt</td>
  </tr>
</table>
EOT

# chm footer for releases (no commit info - assumes the tag is enough)
cat <<EOT > ./chmlclfooter.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Lazarus Component Library (LCL)</td>
    <td class="footer-ver">Version $verno ($dt)</td>
    <td class="footer-date">
      <a href="https://lazarus-ccr.sourceforge.io/docs/">HTML version (Online)</a>
    </td>
  </tr>
</table>
EOT

#==============================================================
# rewrite footer files for LazUtils
# locallazutilsfooter.xml is the version without a link to the online HTML
# chmlazutilsfooter.xml includes a link to the online HTML
#==============================================================
cat <<EOT > ./locallazutilsfooter.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Lazarus Utilities (LazUtils)</td>
    <td class="footer-ver">Version $verno-$commit</td>
    <td class="footer-date">Generated $dt</td>
  </tr>
</table>
EOT

# chm footer for releases (no commit info - assumes the tag is enough)
cat <<EOT > ./chmlazutilsfooter.xml
<hr class="footer-sep"/>
<table class="footer">
  <tr>
    <td class="footer-doc">Lazarus Utilities (LazUtils)</td>
    <td class="footer-ver">Version $verno ($dt)</td>
    <td class="footer-date">
      <a href="https://lazarus-ccr.sourceforge.io/docs/">HTML version (Online)</a>
    </td>
  </tr>
</table>
EOT

# path to the fpdoc executable
fpdocpath="c:/lazarus/fpc/3.2.0/bin/x86_64-win64"

#==============================================================
# build the CHM output format with the desired footers
#==============================================================
./build_lcl_docs.exe --outfmt=chm --fpcdocs=../chm --footer chmlclfooter.xml --fpdoc=$fpdocpath/fpdoc.exe --warnings --verbose 2>&1 | tee ./build_chm.log

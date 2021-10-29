echo off
cls

REM ========================================================================
REM Builds LCL and LazUtils documentation in CHM format
REM
REM use the trunk version of fpdoc if possible:
REM     --fpdoc \path\to\fpc\trunk\bin\x86_64-win64\fpdoc.exe
REM
REM Adjust any paths used in the batch file for use on your system.
REM If needed,  compile the build_lcl_docs.lpi project before running this script.
REM ========================================================================

REM ========================================================================
REM creates and uses version.inc, commit.inc, date.inc for values used in footers
REM git describe doesn't return a usable branch or tag name like 2.3.0
REM main-2_3 and fixes_2_2 are not what is needed for the doc files
REM  usable branches or tags like 2.3.0 or 2.2.0-RC2 do not exist
REM  just get or set the text used for version.inc
REM ========================================================================
set verno=2.3.0
echo|(set /p=%verno%)> version.inc
REM set verno=<version.inc

REM ========================================================================
REM svn info https://svn.freepascal.org/svn/lazarus/trunk --show-item revision > ./revision.inc
REM  not using svn or svn2revision
REM  change gitpath to the local repo on your system
REM  get hash for the commit used in the build
REM ========================================================================
set gitpath="c:\usr\work\git-lazarus"
git -C %gitpath% describe --all --long > describe.txt
for /f "delims=-g tokens=1-3" %%a in (describe.txt) do set commit=%%c
echo|(set /p=%commit%) > commit.inc
del describe.txt

REM ========================================================================
REM get datestamp for the footer
REM ========================================================================
set dt=%date:~10,4%-%date:~4,2%-%date:~7,2%
echo|(set /p=%dt%) > date.inc

REM ========================================================================
REM rewrite footer files for LCL
REM locallclfooter.xml is the version without a link to the online HTML
REM chmlclfooter.xml includes a link to the online HTML
REM ========================================================================
echo|(set /p="<hr class='footer-sep'/>" & echo.) > locallclfooter.xml
echo|(set /p="<table class='footer'>" & echo.)  >> locallclfooter.xml
echo|(set /p="<tr>" & echo.)  >> locallclfooter.xml
echo|(set /p="<td class='footer-doc'>Lazarus Component Library (LCL)</td>" & echo.)  >> locallclfooter.xml
echo|(set /p="<td class='footer-ver'>Version %verno%-%commit%</td>" & echo.)  >> locallclfooter.xml
echo|(set /p="<td class='footer-date'>Generated %dt%</td>" & echo.)  >> locallclfooter.xml
echo|(set /p="</tr>" & echo.)  >> locallclfooter.xml
echo|(set /p="</table>" & echo.)  >> locallclfooter.xml

REM chm footer for releases (no commit info - assumes the tag is enough)
echo|(set /p="<hr class='footer-sep'/>" & echo.) > chmlclfooter.xml
echo|(set /p="<table class='footer'>" & echo.)  >> chmlclfooter.xml
echo|(set /p="<tr>" & echo.)  >> chmlclfooter.xml
echo|(set /p="<td class='footer-doc'>Lazarus Component Library (LCL)</td>" & echo.)  >> chmlclfooter.xml
echo|(set /p="<td class='footer-ver'>Version %verno% (%dt%)</td>" & echo.)  >> chmlclfooter.xml
echo|(set /p="<td class='footer-date'>" & echo.)  >> chmlclfooter.xml
echo|(set /p="<a href="https://lazarus-ccr.sourceforge.io/docs/">HTML version (Online)</a>" & echo.)  >> chmlclfooter.xml
echo|(set /p="</td>" & echo.)  >> chmlclfooter.xml
echo|(set /p="</tr>" & echo.)  >> chmlclfooter.xml
echo|(set /p="</table>" & echo.)  >> chmlclfooter.xml

REM ========================================================================
REM rewrite footer files for LazUtils
REM locallazutilsfooter.xml is the version without a link to the online HTML
REM chmlazutilsfooter.xml includes a link to the online HTML
REM ========================================================================
echo|(set /p="<hr class='footer-sep'/>" & echo.) > locallazutilsfooter.xml
echo|(set /p="<table class='footer'>" & echo.)  >> locallazutilsfooter.xml
echo|(set /p="<tr>" & echo.)  >> locallazutilsfooter.xml
echo|(set /p="<td class='footer-doc'>Lazarus Utilities (LazUtils)</td>" & echo.)  >> locallazutilsfooter.xml
echo|(set /p="<td class='footer-ver'>Version %verno%-%commit%</td>" & echo.)  >> locallazutilsfooter.xml
echo|(set /p="<td class='footer-date'>Generated %dt%</td>" & echo.)  >> locallazutilsfooter.xml
echo|(set /p="</tr>" & echo.)  >> locallazutilsfooter.xml
echo|(set /p="</table>" & echo.)  >> locallazutilsfooter.xml

REM chm footer (no commit info - assumes the tag is enough)
echo|(set /p="<hr class='footer-sep'/>" & echo.) > chmlazutilsfooter.xml
echo|(set /p="<table class='footer'>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="<tr>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="<td class='footer-doc'>Lazarus Utilties (LazUtils)</td>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="<td class='footer-ver'>Version %verno% (%dt%)</td>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="<td class='footer-date'>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="<a href="https://lazarus-ccr.sourceforge.io/docs/">HTML version (Online)</a>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="</td>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="</tr>" & echo.)  >> chmlazutilsfooter.xml
echo|(set /p="</table>" & echo.)  >> chmlazutilsfooter.xml

REM path to the fpdoc executable
set fpdocpath="c:\lazarus\fpc\3.2.0\bin\x86_64-win64"

REM ========================================================================
REM build the CHM output format with the desired footers
REM ========================================================================
echo.
echo Building LCL, LazUtils documentation in CHM format... this can take some time.
echo.

REM build_lcl_docs.exe --outfmt=chm --footer=chmlclfooter.xml --fpcdocs=../chm --fpdoc=%fpdocpath%/fpdoc.exe --warnings --verbose 1>build_chm.log 2>&1

build_lcl_docs.exe --outfmt=chm --fpcdocs=../chm --footer=chmlclfooter.xml --fpdoc=%fpdocpath%/fpdoc.exe --warnings --verbose 2>&1

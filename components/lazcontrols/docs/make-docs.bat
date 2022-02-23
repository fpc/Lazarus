@echo off

REM =======================================================================
REM Builds CHM and HTML help files for the LazControls package using the fpdoc utility.
REM Requires chm and xct files for rtl, fcl, lcl, and lazutils in the Lazarus docs\chm directory.
REM fpdoc will fail if they are not present.
REM Requires git command line client to get the commit hash for the repository.
REM =======================================================================

REM ======================================
REM SET THE CORRECT PATHS FOR YOUR SYSTEM
REM ======================================
REM lazarus documentation directory
set docdir=..\..\..\docs
REM fpdoc executable directory
REM set fpcdir=..\..\..\..\lazarus-2.2.0-RC2\fpc\3.2.2\bin\x86_64-win64
set fpcdir=..\..\..\..\fpc331\fpc\bin\x86_64-win64
REM directory with lazarus git repository
set gitdir=..\..\..\..\usr\work\git-lazarus

REM values used in the footer
REM package title for CHM
set pkgtitle=Lazarus Controls Package (LazControls)

REM CHANGE THIS BEFORE BUILDING
set version=2.3.0

REM commit SHA
git -C %gitdir% describe --all --long > describe.txt
for /f "delims=-g tokens=1-3" %%a in (describe.txt) do set commit=%%c
del describe.txt

REM date stamp in YYYY-MM-DD format
for /f "usebackq tokens=2 delims== " %%i in (`wmic os get LocalDateTime /value`) do set token10=%%i
set dtstamp=%token10:~0,4%-%token10:~4,2%-%token10:~6,2%
set token10=
REM TODO: wmic is deprecated in Windows 11... check for powershell equivalent

echo %pkgtitle% %version%-%commit% %dtstamp%

REM generate footer with title, version, commit SHA, date stamp
REM footer is included using the fpdoc project file

echo|(set /p="<hr class='footer-sep'/>" & echo.) > lazcontrols-footer.xml
echo|(set /p="<table class='footer'>" & echo.) >> lazcontrols-footer.xml
echo|(set /p=" <tr>" & echo.) >> lazcontrols-footer.xml
echo|(set /p="  <td class='footer-doc'>%pkgtitle%</td>" & echo.) >> lazcontrols-footer.xml
echo|(set /p="  <td class='footer-ver'>Version %version%-%commit%</td>" & echo.) >> lazcontrols-footer.xml
echo|(set /p="  <td class='footer-date'>Generated %dtstamp%</td>" & echo.) >> lazcontrols-footer.xml
echo|(set /p=" </tr>" & echo.) >> lazcontrols-footer.xml
echo|(set /p="</table>" & echo.) >> lazcontrols-footer.xml

echo Generating CHM help...

REM generate chm format
REM imports done manually to set the correct prefix for the output format
REM output to current directory

%fpcdir%\fpdoc --project=lazcontrols-project.xml --format=chm --verbose --import="%docdir%\chm\rtl.xct,ms-its:rtl.chm::/" --import="%docdir%\chm\fcl.xct,ms-its:fcl.chm::/" --import="%docdir%\chm\lcl.xct,ms-its:lcl.chm::/" --import="%docdir%\chm\lazutils.xct,ms-its:lazutils.chm::/" > .\build_chm.log

echo Generating HTML help...

REM generate html format
REM imports done manually to set the correct prefix for the output format
REM html written to lazcontrols sub-directory

%fpcdir%\fpdoc --project=lazcontrols-project.xml --format=html --verbose --import="%docdir%\chm\rtl.xct,..\rtl\" --import="%docdir%\chm\fcl.xct,..\fcl\" --import="%docdir%\chm\lcl.xct,..\lcl\" --import="%docdir%\chm\lazutils.xct,..\lazutils\" --output=lazcontrols > .\build_html.log

REM create an archive file with html content
REM echo Creating archive: docs-html-lazcontrols-%dtstamp%.7z
REM 7z a -t7z -mx9 -r docs-html-lazcontrols-%dtstamp%.7z .\lazcontrols\ > NUL

REM move generated html to lazarus docs\html directory
REM move /Y .\lazcontrols %docdir%\html\

REM move generated chm, xct to lazarus docs directory
REM move /Y lazcontrols.chm %docdir%\chm\
REM move /Y lazcontrols.xct %docdir%\chm\

REM discard generated footer file
del lazcontrols-footer.xml

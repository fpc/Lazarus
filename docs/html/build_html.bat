echo off
cls

REM ========================================================================
REM Builds LCL and LazUtils documentation in HTML format
REM ========================================================================
REM
REM use the trunk version of fpdoc if possible:
REM     --fpdoc \path\to\fpc\trunk\bin\x86_64-win64\fpdoc.exe
REM
REM Adjust any paths used in the batch file for use on your system.
REM If needed,  compile the build_lcl_docs.lpi project before running this script.
REM
REM Run build_chm.bat first. It (re-)generates the footer files with version and revision info.
REM ========================================================================

REM path to the fpdoc executable
set fpdocpath="c:\lazarus\fpc\3.2.0\bin\x86_64-win64"

echo.
echo Building LCL, LazUtils documentation in HTML format... this can take some time.
echo.

REM build_lcl_docs.exe --outfmt html --css-file fpdoc.css --fpcdocs ..\chm --footer locallclfooter.xml --fpdoc %fpdocpath%\fpdoc.exe --warnings --verbose 1>build_html.log 2>&1

build_lcl_docs.exe --outfmt html --css-file fpdoc.css --fpcdocs ..\chm --footer locallclfooter.xml --fpdoc %fpdocpath%\fpdoc.exe --warnings --verbose 2>&1

REM use custom css instead of the built in one
copy lazutils\fpdoc.css lazutils\lazutils\
copy lcl\fpdoc.css lcl\lcl\

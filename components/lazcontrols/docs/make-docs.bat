@echo off

REM =======================================================================
REM Requires chm and xct files for rtl, fcl, lcl, and lazutils in the
REM Lazarus docs\chm directory.
REM fpdoc will fail if they are not present.
REM =======================================================================

REM ======================================
REM SET THE CORRECT PATHS FOR YOUR SYSTEM
REM ======================================
REM lazarus documentation directory
set docdir=..\..\..\docs

REM fpdoc executable directory
REM set fpcdir=..\..\..\..\lazarus-2.2.0-RC2\fpc\3.2.2\bin\x86_64-win64
set fpcdir=..\..\..\..\fpc331\fpc\bin\x86_64-win64

echo LazControls package

echo Generating CHM help...

REM generate chm format without footers
REM imports done manually to set the correct prefix for the output format
REM output to current directory

%fpcdir%\fpdoc --project=lazcontrols-project.xml --format=chm --import="%docdir%\chm\rtl.xct,ms-its:rtl.chm::/" --import="%docdir%\chm\fcl.xct,ms-its:fcl.chm::/" --import="%docdir%\chm\lcl.xct,ms-its:lcl.chm::/" --import="%docdir%\chm\lazutils.xct,ms-its:lazutils.chm::/" > .\build_chm.log

echo Generating HTML help...

REM generate html format without footers
REM imports done manually to set the correct prefix for the output format
REM html written to lazcontrols sub-directory

%fpcdir%\fpdoc --project=lazcontrols-project.xml --format=html --import="%docdir%\chm\rtl.xct,..\rtl\" --import="%docdir%\chm\fcl.xct,..\fcl\" --import="%docdir%\chm\lcl.xct,..\lcl\" --import="%docdir%\chm\lazutils.xct,..\lazutils\" --output=lazcontrols > .\build_html.log

REM copy generated chm, xct to lazarus docs directory
copy /Y lazcontrols.chm %docdir%\chm\
copy /Y lazcontrols.xct %docdir%\chm\

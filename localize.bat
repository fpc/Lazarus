@echo off
REM
REM Make sure you have your setings correctly
REM
REM Usage: localize
REM
REM This script should be executed after adding new resource strings and after
REM udating the translated .po files.
REM
REM This script
REM - converts all compiled .rst files to .po files,
REM - updates all translated xx.po files
REM

REM set LazInstall=C:\lazarus\
set ArchOS=i386-win32
set ArchOsWS=%ArchOS%\win32

echo.

REM Compile tools if updatepofiles is missing

if exist tools\updatepofiles.exe goto SkipTools
echo The updatepofiles tool was not found, compiling tools ...
echo.
cd tools
make updatepofiles.exe
cd..
if not exist tools\updatepofiles.exe goto Exit_Error

:SkipTools

echo Updating language files ...

echo on

@REM IDE
@set IDE_RST=units\%ArchOsWS%\LazarusIDEStrConsts.rst
tools\updatepofiles %IDE_RST% languages\lazaruside.po
@if exist %IDE_RST% echo RST found

@REM Debugger dialogs
@set DBGD_RST=units\%ArchOsWS%\DebuggerStrConst.rst
tools\updatepofiles %DBGD_RST% languages\debuggerstrconst.po
@if exist %DBGD_RST% echo RST found

@REM LazDataDesktop
@set LazDataDesktop_RST=tools\lazdatadesktop\lib\%ArchOS%\lazdatadeskstr.rst
tools\updatepofiles %LazDataDesktop_RST% tools\lazdatadesktop\languages\lazdatadesktop.po
@if exist %LazDataDesktop_RST% echo RST found

@REM LazDoc
@set LazDoc_RST=doceditor\units\%ArchOS%\lazdemsg.rst
tools\updatepofiles %LazDoc_RST% doceditor\languages\lazde.po
@if exist %LazDoc_RST% echo RST found

@REM LazExplorer
@set LazExplorer_RST=examples\lazresexplorer\lib\%ArchOS%\reconstsunit.rst
tools\updatepofiles %LazExplorer_RST% examples\lazresexplorer\languages\resexplorer.po
@if exist %LazExplorer_RST% echo RST found

@REM LazReport editor sample
@set LREditor_RST=components\lazreport\samples\editor\maincalleditor.rst
if not exist %LREditor_RST% goto SkipLREditor
tools\updatepofiles %LREditor_RST% components\lazreport\samples\editor\languages\calleditorwithpkg.po
:SkipLREditor

@goto Exit

:Exit_Error
echo Unable to compile updatepofiles tool

:Exit


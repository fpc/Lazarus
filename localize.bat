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
REM - converts all compiled .rsj (.rst if .rsj is not found) files to .pot files,
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
echo.

echo on

@echo Updating IDE
@set IDE_RSJ=units\%ArchOsWS%\LazarusIDEStrConsts.rsj
@if exist %IDE_RSJ% goto IDE_update
@echo RSJ file NOT found. Searching for RST.
@set IDE_RSJ=units\%ArchOsWS%\LazarusIDEStrConsts.rst
@if not exist %IDE_RSJ% goto SkipIDE
:IDE_update
@tools\updatepofiles %IDE_RSJ% languages\lazaruside.pot
@echo Translation file %IDE_RSJ% found.
:SkipIDE
@echo.

@echo Updating Debugger dialogs
@set DBGD_RSJ=units\%ArchOsWS%\DebuggerStrConst.rsj
@if exist %DBGD_RSJ% goto DBGD_update
@echo RSJ file NOT found. Searching for RST.
@set DBGD_RSJ=units\%ArchOsWS%\DebuggerStrConst.rst
@if not exist %DBGD_RSJ% goto SkipDBGD
:DBGD_update
@tools\updatepofiles %DBGD_RSJ% languages\debuggerstrconst.pot
@echo Translation file %DBGD_RSJ% found.
:SkipDBGD
@echo.

@goto Exit

:Exit_Error
echo Unable to compile updatepofiles tool

:Exit


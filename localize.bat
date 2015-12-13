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
REM - converts all compiled .rsj (.rst if .rsj is not found) files to .po files,
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
@tools\updatepofiles %IDE_RSJ% languages\lazaruside.po
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
@tools\updatepofiles %DBGD_RSJ% languages\debuggerstrconst.po
@echo Translation file %DBGD_RSJ% found.
:SkipDBGD
@echo.

@echo Updating LazDataDesktop
@set LazDataDesktop_RSJ=tools\lazdatadesktop\lib\%ArchOS%\lazdatadeskstr.rsj
@if exist %LazDataDesktop_RSJ% goto LazDataDesktop_update
@echo RSJ file NOT found. Searching for RST.
@set LazDataDesktop_RSJ=tools\lazdatadesktop\lib\%ArchOS%\lazdatadeskstr.rst
@if not exist %LazDataDesktop_RSJ% goto SkipLazDataDesktop
:LazDataDesktop_update
@tools\updatepofiles %LazDataDesktop_RSJ% tools\lazdatadesktop\languages\lazdatadesktop.po
@echo Translation file %LazDataDesktop_RSJ% found.
:SkipLazDataDesktop
@echo.

@echo Updating LazDoc
@set LazDoc_RSJ=doceditor\units\%ArchOS%\lazdemsg.rsj
@if exist %LazDoc_RSJ% goto LazDoc_update
@echo RSJ file NOT found. Searching for RST.
@set LazDoc_RSJ=doceditor\units\%ArchOS%\lazdemsg.rst
@if not exist %LazDoc_RSJ% goto SkipLazDoc
:LazDoc_update
@tools\updatepofiles %LazDoc_RSJ% doceditor\languages\lazde.po
@echo Translation file %LazDoc_RSJ% found.
:SkipLazDoc
@echo.

@echo Updating LazExplorer
@set LazExplorer_RSJ=examples\lazresexplorer\lib\%ArchOS%\reconstsunit.rsj
@if exist %LazExplorer_RSJ% goto LazExplorer_update
@echo RSJ file NOT found. Searching for RST.
@set LazExplorer_RSJ=examples\lazresexplorer\lib\%ArchOS%\reconstsunit.rst
@if not exist %LazExplorer_RSJ% goto SkipLazExplorer
:LazExplorer_update
@tools\updatepofiles %LazExplorer_RSJ% examples\lazresexplorer\languages\resexplorer.po
@echo Translation file %LazExplorer_RSJ% found.
:SkipLazExplorer
@echo.

@echo Updating LazReport editor sample
@set LREditor_RSJ=components\lazreport\samples\editor\maincalleditor.rsj
@if exist %LREditor_RSJ% goto LREditor_update
@echo RSJ file NOT found. Searching for RST.
@set LREditor_RSJ=components\lazreport\samples\editor\maincalleditor.rst
@if not exist %LREditor_RSJ% goto SkipLREditor
:LREditor_update
@tools\updatepofiles %LREditor_RSJ% components\lazreport\samples\editor\languages\calleditorwithpkg.po
@echo Translation file %LREditor_RSJ% found.
:SkipLREditor
@echo.

@goto Exit

:Exit_Error
echo Unable to compile updatepofiles tool

:Exit


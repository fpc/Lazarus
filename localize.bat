@echo off
REM
REM Usage: localize
REM
REM This script should be executed after adding new resource strings and after
REM udating the translated .po files.
REM
REM This script
REM - converts all compiled .rsj files to .pot files,
REM - updates all translated xx.po files
REM

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
@tools\updatepofiles --searchdir=units lazarusidestrconsts.rsj languages\lazaruside.pot
@echo.

@echo Updating Debugger dialogs
@tools\updatepofiles --searchdir=units debuggerstrconst.rsj languages\debuggerstrconst.pot
@echo.

@goto Exit

:Exit_Error
echo Unable to compile updatepofiles tool

:Exit


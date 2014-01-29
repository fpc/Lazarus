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

@REM Debugger MI interface
@set DBG_RST=units\%ArchOsWS%\gdbmidebugger.rst
tools\updatepofiles %DBG_RST% languages\gdbmidebugger.po
@if exist %DBG_RST% echo RST found

@REM Debugger MI server interface
@set DBGS_RST=units\%ArchOsWS%\gdbmiserverdebugger.rst
tools\updatepofiles %DBGS_RST% languages\gdbmiserverdebugger.po
@if exist %DBGS_RST% echo RST found

@REM CodeTools
@set CodeTools_RST=components\codetools\units\%ArchOS%\CodeToolsStrConsts.rst
tools\updatepofiles %CodeTools_RST% components\codetools\languages\codetoolsstrconsts.po
@if exist %CodeTools_RST% echo RST found

@REM LCL
@set LCL_RST=lcl\units\%ArchOS%\lclstrconsts.rst
tools\updatepofiles %LCL_RST% lcl\languages\lclstrconsts.po
@if exist %LCL_RST% echo RST found

@REM LazReport
@set LazReport_RST=components\lazreport\source\lib\%ArchOS%\lr_const.rst
tools\updatepofiles %LazReport_RST% components\lazreport\source\languages\lr_const.po
@if exist %LazReport_RST% echo RST found

@REM MemDS
@set MemDS_RST=components\memds\lib\%ArchOsWS%\frmselectdataset.rst
tools\updatepofiles %MemDS_RST% components\memds\languages\frmselectdataset.po
@if exist %MemDS_RST% echo RST found

@REM Printers
@set Printers_RST=components\printers\design\lib\%ArchOsWS%\ideprinting.rst
tools\updatepofiles %Printers_RST% components\printers\design\languages\ideprinting.po
@if exist %Printers_RST% echo RST found

@REM ProjectTemplates
@set ProjectTemplates_RST=components\projecttemplates\lib\%ArchOsWS%\frmtemplatevariables.rst
tools\updatepofiles %ProjectTemplates_RST% components\projecttemplates\languages\frmtemplatevariables.po
@if exist %ProjectTemplates_RST% echo RST found

@set ProjectTemplates_RST=components\projecttemplates\lib\%ArchOsWS%\idetemplateproject.rst
tools\updatepofiles %ProjectTemplates_RST% components\projecttemplates\languages\idetemplateproject.po
@if exist %ProjectTemplates_RST% echo RST found

@set ProjectTemplates_RST=components\projecttemplates\lib\%ArchOsWS%\projecttemplates.rst
tools\updatepofiles %ProjectTemplates_RST% components\projecttemplates\languages\projecttemplates.po
@if exist %ProjectTemplates_RST% echo RST found

@REM TDBF
@set TDBF_RST=components\tdbf\lib\%ArchOsWS%\registerdbf.rst
tools\updatepofiles %TDBF_RST% components\tdbf\languages\registerdbf.po
@if exist %TDBF_RST% echo RST found

@REM TP_IPro
@set TP_IPro_RST=components\turbopower_ipro\units\%ArchOsWS%\ipconst.rst
tools\updatepofiles %TP_IPro_RST% components\turbopower_ipro\languages\ipconst.po
@if exist %TP_IPro_RST% echo RST found

@set TP_IPro_RST=components\turbopower_ipro\units\%ArchOsWS%\iputils.rst
tools\updatepofiles %TP_IPro_RST% components\turbopower_ipro\languages\iputils.po
@if exist %TP_IPro_RST% echo RST found

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


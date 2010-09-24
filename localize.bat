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
@set IDE_RST=units\%ArchOS%\LazarusIDEStrConsts.rst
tools\updatepofiles %IDE_RST% languages\lazaruside.po

@REM IDEIntf
@set ObjInsp_RST=ideintf\units\%ArchOS%\ObjInspStrConsts.rst
tools\updatepofiles %ObjInsp_RST% ideintf\languages\objinspstrconsts.po

@REM INSTALLER
@set Installer_RST=tools\install\win\installerstrconsts.rst
if not exist %Installer_RST% goto SkipInstaller
tools\updatepofiles %Installer_RST% languages\installerstrconsts.po
:SkipInstaller

@REM CodeTools
@set CodeTools_RST=components\codetools\units\%ArchOS%\CodeToolsStrConsts.rst
tools\updatepofiles %CodeTools_RST% components\codetools\languages\codetoolsstrconsts.po

@REM SynEdit
@set SynEdit_RST=components\synedit\units\%ArchOS%\SynEditStrConst.rst
tools\updatepofiles %SynEdit_RST% components\synedit\languages\synedit.po

@REM SynDesign
@set SynDesign_RST=components\synedit\units\%ArchOS%\syndesignstringconstants.rst
tools\updatepofiles %SynDesign_RST% components\synedit\languages\syndesign.po

@REM SynMacroRecorder
@set SynMacroRec_RST=components\synedit\units\%ArchOS%\synmacrorecorder.rst
tools\updatepofiles %SynMacroRec_RST% components\synedit\languages\synmacrorecorder.po

@REM SynUniHighLighterShellScript
@set SynUniHighLighterShellScript_RST=components\synedit\units\%ArchOS%\synhighlighterunixshellscript.rst
tools\updatepofiles %SynUniHighLighterShellScript_RST% components\synedit\languages\synhighlighterunixshellscript.po

@REM LCL
@set LCL_RST=lcl\units\%ArchOS%\lclstrconsts.rst
tools\updatepofiles %LCL_RST% lcl\languages\lclstrconsts.po

@REM LazReport
@set LazReport_RST=components\lazreport\source\lib\%ArchOS%\lr_const.rst
tools\updatepofiles %LazReport_RST% components\lazreport\source\languages\lr_const.po

@REM MemDS
@set MemDS_RST=components\memds\lib\%ArchOSWS%\frmselectdataset.rst
tools\updatepofiles %MemDS_RST% components\memds\languages\frmselectdataset.po

@REM Printers
@set Printers_RST=components\printers\design\lib\%ArchOS%\ideprinting.rst
tools\updatepofiles %Printers_RST% components\printers\design\languages\ideprinting.po

@REM ProjectTemplates
@set ProjectTemplates_RST=components\projecttemplates\lib\%ArchOsWS%\frmtemplatevariables.rst
tools\updatepofiles %ProjectTemplates_RST% components\projecttemplates\languages\frmtemplatevariables.po

@set ProjectTemplates_RST=components\projecttemplates\lib\%ArchOsWS%\idetemplateproject.rst
tools\updatepofiles %ProjectTemplates_RST% components\projecttemplates\languages\idetemplateproject.po

@set ProjectTemplates_RST=components\projecttemplates\lib\%ArchOsWS%\projecttemplates.rst
tools\updatepofiles %ProjectTemplates_RST% components\projecttemplates\languages\projecttemplates.po

@REM TDBF
@set TDBF_RST=components\tdbf\lib\%ArchOSWS%\registerdbf.rst
tools\updatepofiles %TDBF_RST% components\tdbf\languages\registerdbf.po

@REM TP_IPro
@set TP_IPro_RST=components\turbopower_ipro\units\%ArchOS%\ipconst.rst
tools\updatepofiles %TP_IPro_RST% components\turbopower_ipro\languages\ipconst.po

@set TP_IPro_RST=components\turbopower_ipro\units\%ArchOS%\iputils.rst
tools\updatepofiles %TP_IPro_RST% components\turbopower_ipro\languages\iputils.po

@REM LazDataDesktop
@set LazDataDesktop_RST=tools\lazdatadesktop\lib\%ArchOS%\lazdatadeskstr.rst
tools\updatepofiles %LazDataDesktop_RST% tools\lazdatadesktop\languages\lazdatadesktop.po

@REM LazDoc
@set LazDoc_RST=doceditor\units\%ArchOS%\lazdemsg.rst
tools\updatepofiles %LazDoc_RST% doceditor\languages\lazde.po

@REM LazExplorer
@set LazExplorer_RST=examples\lazresexplorer\lib\%ArchOS%\reconstsunit.rst
tools\updatepofiles %LazExplorer_RST% examples\lazresexplorer\languages\resexplorer.po


@REM LazReport editor sample
@set LREditor_RST=components\lazreport\samples\editor\maincalleditor.rst
if not exist %LREditor_RST% goto SkipLREditor
tools\updatepofiles %LREditor_RST% components\lazreport\samples\editor\languages\calleditorwithpkg.po
:SkipLREditor

@goto Exit

:Exit_Error
echo Unable to compile updatepofiles tool

:Exit


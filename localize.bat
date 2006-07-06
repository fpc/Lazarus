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

set LazInstall=C:\lazarus\
set ArchOS=i386-win32

echo.

REM Compile tools if updatepofiles if missing

if exist tools\updatepofiles.exe goto SkipTools
echo The updatepofiles tool was not found, compiling tools ...
echo.
cd tools
make updatepofiles.exe
cd..
:SkipTools

echo Updating language files ...

echo on

@REM IDE
@set IDE_RST=units\%ArchOS%\LazarusIDEStrConsts.rst
rstconv -i %IDE_RST% -o languages\lazaruside.po
tools\updatepofiles languages\lazaruside.po

@REM IDEIntf
@set ObjInsp_RST=ideintf\units\%ArchOS%\ObjInspStrConsts.rst
rstconv -i %ObjInsp_RST% -o languages\objinspstrconsts.po
tools\updatepofiles languages\objinspstrconsts.po

@REM INSTALLER
@set Installer_RST=tools\install\win32\installerstrconsts.rst
if not exist %Installer_RST% goto SkipInstaller
rstconv -i %Installer_RST% -o languages\installerstrconsts.po
tools\updatepofiles languages\installerstrconsts.po
:SkipInstaller

@REM CodeTools
@set CodeTools_RST=components\codetools\units\%ArchOS%\CodeToolsStrConsts.rst
rstconv -i %CodeTools_RST% -o components\codetools\languages\codetools.po
tools\updatepofiles components\codetools\languages\codetools.po

@REM SynEdit
@set SynEdit_RST=components\synedit\units\%ArchOS%\SynEditStrConst.rst
rstconv -i %SynEdit_RST% -o components\synedit\languages\synedit.po
tools\updatepofiles components\synedit\languages\synedit.po

@REM SynMacroRecorder
@set SynMacroRec_RST=components\synedit\units\%ArchOS%\synmacrorecorder.rst
rstconv -i %SynMacroRec_RST% -o components\synedit\languages\synmacrorecorder.po
tools\updatepofiles components\synedit\languages\synmacrorecorder.po

@REM LCL
@set LCL_RST=lcl\units\%ArchOS%\lclstrconsts.rst
rstconv -i %LCL_RST% -o lcl\languages\lcl.po
tools\updatepofiles lcl\languages\lcl.po


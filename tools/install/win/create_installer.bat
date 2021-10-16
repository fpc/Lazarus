:: Note: This script sets various ENV variables as output.

:: check all the necessary parameters are given and not empty
if [%1]==[] goto USAGE
if [%2]==[] goto USAGE
if [%3]==[] goto USAGE
if [%4]==[] goto USAGE
if [%1]==[""] goto USAGE
if [%2]==[""] goto USAGE
if [%3]==[""] goto USAGE
if [%4]==[""] goto USAGE

:: Path to the fpc sources checked out of fpcbuild git repository
SET FPCGITDIR=%1
:: Path to the lazarus sources checked out of subversion
SET LAZGITDIR=%2
:: Path to the lazarus third party binaries checked out of subversion
SET LAZGITBINDIR=%3
:: Path to latest release compiler
SET RELEASE_PPC=%4

::---------------------------------------------------------------------
:: Optional parameter to indicate the LCL Widgetset used by the IDE
IF [%5]==[] GOTO EMPTY5
IF [%5]==[""] GOTO EMPTY5
SET IDE_WIDGETSET=%5
:EMPTY5

:: Name of fpc patch file
IF [%6]==[] GOTO EMPTY6
IF [%6]==[""] GOTO EMPTY6
SET PATCHFILE=%6
:EMPTY6

SET CHMHELPFILES=%7

::=====================================================================
:: Find required programs
:: These settings are dependent on the configuration of the build machine

:: set program files 32bits directory in windows 64bits and windows 32bits
set ProgramFiles32bits=%ProgramFiles:"=%
if not "%ProgramFiles(x86)%" == "" set ProgramFiles32bits=%ProgramFiles(x86):"=%

::---------------------------------------------------------------------
:: Path to the Inno Setup Compiler
:: It may or may not have quotes, make sure it has.
SET ISCC="%ISCC%"
SET ISCC=%ISCC:"=%
SET ISCC="%ISCC%"
if not [%ISCC%]==[""] goto ISCC_DONE
SET ISCC="%ProgramFiles32bits%\Inno Setup 5\iscc.exe"
if not exist %ISCC% SET SET ISCC="%ProgramFiles%\Inno Setup 5\iscc.exe"
:: NO fallback to PATH
:ISCC_DONE

::---------------------------------------------------------------------
:: Path to the git executable; make sure it has quotes
SET GIT="%GIT%"
SET GIT=%GIT:"=%
SET GIT="%GIT%"
if not [%GIT%]==[""] GOTO GIT_DONE
:: set Subversion; if it doesn't exist try TortoiseGIT 32bits and 64bits else error info
SET GIT="%ProgramFiles%\Git\bin\git.exe"
if not exist %GIT% SET GIT="%ProgramFiles%\Git\mingw64\bin\git.exe"
if not exist %GIT% SET GIT="%ProgramFiles32bits%\Git\bin\git.exe"
:: Use GIT in Path
if not exist %GIT% SET GIT=git.exe
:GIT_DONE

::---------------------------------------------------------------------
:: Path to build directory.
:: In this directory an image of the installation will be built.
:: If the user specified a LAZTEMPBUILDDIR environment variable, use that instead.
SET BUILDDIR=c:\temp\lazbuild
if NOT [%LAZTEMPBUILDDIR%]==[] SET BUILDDIR=%LAZTEMPBUILDDIR%

::=====================================================================
:: no change needed after this.

:: Some internal variables
FOR /F %%L IN ('%RELEASE_PPC% -iTO') DO SET FPCTARGETOS=%%L
FOR /F %%L IN ('%RELEASE_PPC% -iTP') DO SET FPCTARGETCPU=%%L
SET FPCFULLTARGET=%FPCTARGETCPU%-%FPCTARGETOS%

SET FPCBINDIR=%FPCGITDIR%\install\binw%FPCTARGETOS:~-2%
SET FPCBINTREE=install/binw%FPCTARGETOS:~-2%
SET MAKEEXE=%FPCBINDIR%\make.exe
SET PATCHEXE=%FPCGITDIR%\install\binw32\patch.exe
SET LOGFILE=%CD%\installer.log
SET PATCHDIR=%CD%\..\patches

:: Path to the directory containing the mingw gdb debugger installation
:: it should have the debugger with the name gdb.exe in its bin subdirectory
SET GDBDIR=%LAZGITBINDIR%\%FPCFULLTARGET%\gdb

:: OPENSSL
SET OPENSSLDIR=%LAZGITBINDIR%\%FPCFULLTARGET%\openssl
for /F %%i in ('dir /b "%OPENSSLDIR%\*.*"') do (
   SET HASOPENSSL=1
)

:: Path to the directory containing the qtinf dll matching the qt4.pas from 
:: http://users.pandora.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html
SET QTINFDIR=%LAZGITBINDIR%\%FPCFULLTARGET%\qt

SET QT5INFDIR=%LAZGITBINDIR%\%FPCFULLTARGET%\qt5
for /F %%i in ('dir /b "%QT5INFDIR%\*.*"') do (
   SET HASQT5=1
)

::---------------------------------------------------------------------
FOR /F %%L IN ('%FPCBINDIR%\gdate.exe +%%Y%%m%%d') DO SET DATESTAMP=%%L
SET BUILDDRIVE=%BUILDDIR:~,2%
SET CP=%FPCBINDIR%\cp.exe
FOR /F %%F IN ('%LAZGITDIR%\tools\install\get_lazarus_version.bat') DO set LAZVERSION=%%F
FOR /F %%F IN ('"%GIT% -C %LAZGITDIR% log -1 --pretty=format:%%h" ') DO set LAZREVISION=%%F
IF [%LAZREVISION%] == [] GOTO SVNVERERR
:: Remove : from revision if present
SET LAZREVISION=%LAZREVISION::=_%

ECHO Starting at: > %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%

:: set path to make sure the right tools are used
SET OLDPATH=%PATH%
SET PATH=%FPCBINDIR%

::---------------------------------------------------------------------
:: copy lazarus dir
rmdir /s /q %BUILDDIR%
mkdir %BUILDDIR%
%GIT% -C %LAZGITDIR% --work-tree=%BUILDDIR% restore . >> %LOGFILE%

rem cp -pr %LAZGITDIR%\* %BUILDDIR% >> %LOGFILE%
IF %ERRORLEVEL% NEQ 0 GOTO GITERR
call svn2revisioninc.bat %LAZGITDIR% %BUILDDIR%\ide\revision.inc

::---------------------------------------------------------------------
call build-fpc.bat

:: INSTALL_BINDIR is set by build-fpc.bat
mkdir %BUILDDIR%\fpcbins
%GIT% -C %FPCGITDIR% --work-tree=%BUILDDIR%\fpcbins restore --source=HEAD:%FPCBINTREE% . >> %LOGFILE%
IF %ERRORLEVEL% NEQ 0 GOTO GITERR
mv -f %BUILDDIR%\fpcbins\*.* %INSTALL_BINDIR%
%FPCBINDIR%\rm -rf %BUILDDIR%\fpcbins
del %INSTALL_BINDIR%\gdb.exe
:: copy from 32 bit, missing in 64bit
for %%T in ( cpp.exe gcc.exe windres.exe windres.h ) DO if not exist %INSTALL_BINDIR%\%%T copy %FPCGITDIR%\install\binw32\%%T %INSTALL_BINDIR%\

:: exit if no compiler has been made
if not exist %INSTALL_BINDIR%\fpc.exe goto WARNING_NO_COMPILER_MADE
if not exist %INSTALL_BINDIR%\fpcmkcfg.exe goto WARNING_NO_COMPILER_MADE

%INSTALL_BINDIR%\fpcmkcfg.exe -d "basepath=%INSTALL_BASE%" -o %INSTALL_BINDIR%\fpc.cfg

::---------------------------------------------------------------------
call build-lazarus.bat

:: remove fpc.cfg, the installer will create a new one
del %INSTALL_BINDIR%\fpc.cfg

:: do not create installer, if the required executables are not there
if not exist %BUILDDIR%\lazarus.exe goto WARNING_NO_LAZARUS
if not exist %BUILDDIR%\startlazarus.exe goto WARNING_NO_LAZARUS

::---------------------------------------------------------------------
:: copy gdb into build dir
if NOT exist %GDBDIR% goto NOGDB
gmkdir -p %BUILDDIR%\mingw\%FPCFULLTARGET%
cp -pr %GDBDIR%\* %BUILDDIR%\mingw\%FPCFULLTARGET%
:NOGDB

::---------------------------------------------------------------------
:: create the installer
IF [%BUILDLAZRELEASE%]==[] GOTO SNAPSHOT
SET OutputFileName=lazarus-%LAZVERSION%-fpc-%FPCFULLVERSION%-%FPCTARGETOS%
if not [%IDE_WIDGETSET%]==[win32] SET OutputFileName=lazarus-%IDE_WIDGETSET%-%LAZVERSION%-fpc-%FPCFULLVERSION%-%FPCTARGETOS%
GOTO GO_ON

:SNAPSHOT
SET OutputFileName=lazarus-%LAZVERSION%-%LAZREVISION%-fpc-%FPCFULLVERSION%-%DATESTAMP%-%FPCTARGETOS%
if not [%IDE_WIDGETSET%]==[win32] SET OutputFileName=lazarus-%IDE_WIDGETSET%-%LAZVERSION%-%LAZREVISION%-fpc-%FPCFULLVERSION%-%DATESTAMP%-%FPCTARGETOS%

:GO_ON
SET OutputFileName=%OutputFileName::=_%

:: %LAZBUILD_HOOK_DIR% is a directory, that can contain scripts to be hooked into the build process. Further hooks can be defined the same way
if not [%LAZBUILD_HOOK_DIR%]==[] if exist %LAZBUILD_HOOK_DIR%\lazhook_before_iscc.bat call %LAZBUILD_HOOK_DIR%\lazhook_before_iscc.bat

if not [%LAZBUILD_MAKE282_SRC%]==[] if exist %LAZBUILD_MAKE282_SRC% copy %LAZBUILD_MAKE282_SRC% %INSTALL_BINDIR%\make.exe


%ISCC% lazarus.iss >> installer.log

:: do not delete build dir, if installer failed.
if not exist "output\%OutputFileName%.exe" goto WARNING_INSTALLER_FAILED

:: delete build dir
rd /s /q %BUILDDIR% > NUL

GOTO END

:SVNVERERR
echo GITVersion failed
echo
echo This script requires GIT.
echo If not installed, then please download at:
echo http://subversion.apache.org/packages.html or http://tortoisesvn.net/downloads.html
GOTO END
:GITERR
echo GIT failed
echo
echo This script requires GIT.
echo If not installed, then please download at:
echo http://subversion.apache.org/packages.html or http://tortoisesvn.net/downloads.html
:END

SET PATH=%OLDPATH%

ECHO Finished at: >> %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%

goto STOP

:WARNING_INSTALLER_FAILED
echo Installer was not created in output\%OutputFileName%.exe. Ending now.
echo 
echo This script requires inno setup. 
echo If not installed, then please download at:
echo http://www.jrsoftware.org
GOTO END

:WARNING_NO_COMPILER_MADE
echo Could not find compiler at %INSTALL_BINDIR%\fpc.exe. Ending now.
GOTO END

:WARNING_NO_LAZARUS
echo Could not find %BUILDDIR%\lazarus.exe or %BUILDDIR%\startlazarus.exe. Ending now.
GOTO END

:USAGE
@echo off
echo Usage:
echo create_installer.bat FPCGITDIR LAZGITDIR LAZGITBINDIR RELEASECOMPILER  [IDEWIDGETSET] [PATCHFILE] [CHMHELPFILES]
echo FPCGITDIR: directory that contains a git version of the fpcbuild repository
echo LAZGITDIR: directory that contains a git version of the lazarus repository
echo LAZGITBINDIR: directory that contains a git version of the lazarus binaries repository
echo RELEASECOMPILER: bootstrapping compiler for building fpc
echo IDEWIDGETSET: optional, LCL platform used for compiling the IDE
echo PATCHFILE: optional patch file for the fpc sources
echo CHMHELPFILES: optional directory with chm helpfiles

:STOP

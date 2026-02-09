:: Note: This script sets various ENV variables as output.

if [%3]==[] goto USAGE
if [%3]==[""] goto USAGE

call defaults.bat %1 %2 %3 %4 %5 %6 %7

if [%USAGE%] ==[1] goto USAGE

::=====================================================================
::=====================================================================

ECHO Starting at: > %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%

:: set path to make sure the right tools are used
SET OLDPATH=%PATH%
SET PATH=%FPCBINDIR%

rmdir /s /q %BUILDDIR%
mkdir %BUILDDIR%

::=====================================================================
::=====================================================================
:: BUILD FPC
::=====================================================================
::=====================================================================

::---------------------------------------------------------------------
call build-fpc.bat

:: INSTALL_BINDIR is set by build-fpc.bat
:: copy gnu binaries (as.exe, ld.exe, ...)
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

if [%FPC_ONLY%]==[1] GOTO NOLAZ

:: temporary fpc.cfg to build lazarus
%INSTALL_BINDIR%\fpcmkcfg.exe -d "basepath=%INSTALL_BASE%" -o %INSTALL_BINDIR%\fpc.cfg


::=====================================================================
::=====================================================================
:: BUILD Lazarus
::=====================================================================
::=====================================================================

::---------------------------------------------------------------------
:: copy lazarus dir
%GIT% -C %LAZGITDIR% --work-tree=%BUILDDIR% restore . >> %LOGFILE%
IF %ERRORLEVEL% NEQ 0 GOTO GITERR
call svn2revisioninc.bat %LAZGITDIR% %BUILDDIR%\ide\revision.inc

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

::=====================================================================
::=====================================================================
:: BUILD installer
::=====================================================================
::=====================================================================

SET ISSFILE=lazarus.iss

IF [%BUILDLAZRELEASE%]==[] GOTO SNAPSHOT
SET OutputFileName=lazarus-%LAZVERSION%-fpc-%FPCFULLVERSION%-%FPCTARGETOS%
if not [%IDE_WIDGETSET%]==[win32] SET OutputFileName=lazarus-%IDE_WIDGETSET%-%LAZVERSION%-fpc-%FPCFULLVERSION%-%FPCTARGETOS%
GOTO GO_ON

:SNAPSHOT
SET OutputFileName=lazarus-%LAZVERSION%-%LAZREVISION%-fpc-%FPCFULLVERSION%-%DATESTAMP%-%FPCTARGETOS%
if not [%IDE_WIDGETSET%]==[win32] SET OutputFileName=lazarus-%IDE_WIDGETSET%-%LAZVERSION%-%LAZREVISION%-fpc-%FPCFULLVERSION%-%DATESTAMP%-%FPCTARGETOS%

GOTO GO_ON

::=====================================================================
:: FPC ONLY
:NOLAZ

SET ISSFILE=lazarus-cross.iss
mkdir %BUILDDIR%\image

if not [%FPC_ONLY_DIR%]==[] SET %FPC_ONLY_DIR% = fpc
if not [%FPC_ONLY_DIR%]==[] mv %BUILDDIR%\fpc  %BUILDDIR%\image\%FPC_ONLY_DIR%

if  [%IDE_WIDGETSET%]==[] SET IDE_WIDGETSET=%FPCTARGETOS%
if  [%IDE_WIDGETSET%]==[] SET IDE_WIDGETSET=

SET OutputFileName=fpc-%FPCFULLVERSION%-%FPCTARGETOS%

::=====================================================================
:: INSTALLER (Laz or fpc only)
:GO_ON
SET OutputFileName=%OutputFileName::=_%


:: %LAZBUILD_HOOK_DIR% is a directory, that can contain scripts to be hooked into the build process. Further hooks can be defined the same way
if not [%LAZBUILD_HOOK_DIR%]==[] if exist %LAZBUILD_HOOK_DIR%\lazhook_before_iscc.bat call %LAZBUILD_HOOK_DIR%\lazhook_before_iscc.bat

if not [%LAZBUILD_MAKE282_SRC%]==[] if exist %LAZBUILD_MAKE282_SRC% copy %LAZBUILD_MAKE282_SRC% %INSTALL_BINDIR%\make.exe


%ISCC% %ISSFILE% >> installer.log

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


:: check all the necessary parameters are given
if [%1]==[] goto USAGE
if [%2]==[] goto USAGE
if [%3]==[] goto USAGE
if [%4]==[] goto USAGE

:: set program files 32bits directory in windows 64bits and windows 32bits
set ProgramFiles32bits=%ProgramFiles%
if not "%ProgramFiles(x86)%" == "" set ProgramFiles32bits=%ProgramFiles(x86)%

:: These settings are dependent on the configuration of the build machine
:: Path to the Inno Setup Compiler
if [%ISCC%]==[] SET ISCC="%ProgramFiles32bits%\Inno Setup 5\iscc.exe"

:: check is Inno Setup 5 exist
if not exist %ISCC% GOTO ERROR_INNO

:: Path to build directory.
:: In this directory an image of the installation will be built.
:: If the user specified a LAZTEMPBUILDDIR environment variable, use that instead.
SET BUILDDIR=c:\temp\lazbuild
if NOT [%LAZTEMPBUILDDIR%]==[] SET BUILDDIR=%LAZTEMPBUILDDIR%
 
:: Path to the svn executable
if not [%SVN%]==[] GOTO SVN_BY_USER
:: set Subversion if no exist try TortoiseSVN 32bits and 64bits else error info
SET SVN="%ProgramFiles32bits%\subversion\bin\svn.exe"
if not exist %SVN% SET SVN="%ProgramFiles32bits%\TortoiseSVN\bin\svn.exe"
if not exist %SVN% SET SVN="%ProgramFiles%\TortoiseSVN\bin\svn.exe"
:SVN_BY_USER
if not exist %SVN% GOTO ERROR_SVN

:: Path to the fpc sources checked out of fpcbuild svn repository
SET FPCSVNDIR=%1

:: Path to the lazarus sources checked out of subversion
SET LAZSVNDIR=%2

:: Path to the lazarus third party binaries checked out of subversion
SET LAZSVNBINDIR=%3

:: Path to latest release compiler
SET RELEASE_PPC=%4

:: Optional parameter to indicate the LCL Widget set used by the IDE
IF [%5]==[] GOTO EMPTY5
IF [%5]==[""] GOTO EMPTY5
SET IDE_WIDGETSET=%5
:EMPTY5

:: Name of fpc patch file
IF [%6]==[] GOTO EMPTY6
IF [%6]==[""] GOTO EMPTY6
IF NOT [%6]==[] SET PATCHFILE=%6
:EMPTY6

SET CHMHELPFILES=%7

::=====================================================================
:: no change needed after this.

:: Some internal variables
FOR /F %%L IN ('%RELEASE_PPC% -iTO') DO SET FPCTARGETOS=%%L
FOR /F %%L IN ('%RELEASE_PPC% -iTP') DO SET FPCTARGETCPU=%%L
SET FPCFULLTARGET=%FPCTARGETCPU%-%FPCTARGETOS%

SET FPCBINDIR=%FPCSVNDIR%\install\binw%FPCTARGETOS:~-2%
SET MAKEEXE=%FPCBINDIR%\make.exe
SET PATCHEXE=%FPCSVNDIR%\install\binw32\patch.exe
SET LOGFILE=%CD%\installer.log
SET PATCHDIR=%CD%\..\patches

:: Path to the directory containing the mingw gdb debugger installation
:: it should have the debugger with the name gdb.exe in its bin subdirectory
SET GDBDIR=%LAZSVNBINDIR%\%FPCFULLTARGET%\gdb

:: Path to the directory containing the qtinf dll matching the qt4.pas from 
:: http://users.pandora.be/Jan.Van.hijfte/qtforfpc/fpcqt4.html
SET QTINFDIR=%LAZSVNBINDIR%\%FPCFULLTARGET%\qt

FOR /F %%L IN ('%FPCBINDIR%\gdate.exe +%%Y%%m%%d') DO SET DATESTAMP=%%L
SET BUILDDRIVE=%BUILDDIR:~,2%
SET CP=%FPCBINDIR%\cp.exe
FOR /F %%F IN ('%LAZSVNDIR%\tools\install\get_lazarus_version.bat') DO set LAZVERSION=%%F
FOR /F %%F IN ('svnversion.exe %LAZSVNDIR%') DO set LAZREVISION=%%F
IF [%LAZREVISION%] == [] GOTO SVNVERERR


ECHO Starting at: > %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%

:: set path to make sure the right tools are used
SET OLDPATH=%PATH%
SET PATH=%FPCBINDIR%

:: copy lazarus dir
rmdir /s /q %BUILDDIR%
%SVN% export -q %LAZSVNDIR% %BUILDDIR% >> %LOGFILE%
IF %ERRORLEVEL% NEQ 0 GOTO SVNERR
call svn2revisioninc.bat %LAZSVNDIR% %BUILDDIR%\ide\revision.inc

call build-fpc.bat

:: INSTALL_BINDIR is set by build-fpc.bat
%SVN% export -q %FPCBINDIR% %BUILDDIR%\fpcbins >> %LOGFILE%
IF %ERRORLEVEL% NEQ 0 GOTO SVNERR
mv %BUILDDIR%\fpcbins\*.* %INSTALL_BINDIR%
%FPCBINDIR%\rm -rf %BUILDDIR%\fpcbins
del %INSTALL_BINDIR%\gdb.exe

:: exit if no compiler has been made
if not exist %INSTALL_BINDIR%\fpc.exe goto END

%INSTALL_BINDIR%\fpcmkcfg.exe -d "basepath=%INSTALL_BASE%" -o %INSTALL_BINDIR%\fpc.cfg

call build-lazarus.bat

:: remove fpc.cfg, the installer will create a new one
del %INSTALL_BINDIR%\fpc.cfg

:: do not create installer, if the required executables are not there
if not exist %BUILDDIR%\lazarus.exe goto END
if not exist %BUILDDIR%\startlazarus.exe goto END

:: copy gdb into build dir
if NOT exist %GDBDIR% goto NOGDB
gmkdir -p %BUILDDIR%\mingw
%SVN% export -q %GDBDIR% %BUILDDIR%\mingw\%FPCFULLTARGET%
:NOGDB

:: create the installer
IF [%BUILDLAZRELEASE%]==[] GOTO SNAPSHOT
SET OutputFileName=lazarus-%LAZVERSION%-fpc-%FPCFULLVERSION%-%FPCTARGETOS%
if not [%IDE_WIDGETSET%]==[win32] SET OutputFileName=lazarus-%IDE_WIDGETSET%-%LAZVERSION%-fpc-%FPCFULLVERSION%-%FPCTARGETOS%
GOTO GO_ON

:SNAPSHOT
SET OutputFileName=lazarus-%LAZVERSION%-%LAZREVISION%-fpc-%FPCFULLVERSION%-%DATESTAMP%-%FPCTARGETOS%
if not [%IDE_WIDGETSET%]==[win32] SET OutputFileName=lazarus-%IDE_WIDGETSET%-%LAZVERSION%-%LAZREVISION%-fpc-%FPCFULLVERSION%-%DATESTAMP%-%FPCTARGETOS%

:GO_ON

%ISCC% lazarus.iss >> installer.log

:: do not delete build dir, if installer failed.
if not exist "output\%OutputFileName%.exe" goto END

:: delete build dir
rd /s /q %BUILDDIR% > NUL

GOTO END

:SVNVERERR
echo SVNVersion failed
GOTO END
:SVNERR
echo SVN failed
:END

SET PATH=%OLDPATH%

ECHO Finished at: >> %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%

goto STOP

:ERROR_INNO
echo Inno setup instalation %ISCC% no exist.
echo Please download and install this program is required to create installer.
echo http://www.jrsoftware.org
GOTO STOP

:ERROR_SVN
echo Subversion or TortoiseSVN instalation %SVN% no exist.
echo Please download and install this program is required to create installer.
echo http://subversion.apache.org/packages.html or http://tortoisesvn.net/downloads.html
GOTO STOP

:USAGE
@echo off
echo Usage:
echo create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASECOMPILER  [IDEWIDGETSET] [PATCHFILE] [CHMHELPFILES]
echo FPCSVNDIR: directory that contains a svn version of the fpcbuild repository
echo LAZSVNDIR: directory that contains a svn version of the lazarus repository
echo LAZSVNBINDIR: directory that contains a svn version of the lazarus binaries repository
echo RELEASECOMPILER: bootstrapping compiler for building fpc
echo IDEWIDGETSET: optional, LCL platform used for compiling the IDE
echo PATCHFILE: optional patch file for the fpc sources
echo CHMHELPFILES: optional directory with chm helpfiles

:STOP

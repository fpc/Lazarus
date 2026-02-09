:: Note: This script sets various ENV variables as output.

:: check all the necessary parameters are given and not empty
if [%1]==[] goto USAGE
if [%2]==[] goto USAGE
if [%4]==[] goto USAGE
if [%1]==[""] goto USAGE
if [%2]==[""] goto USAGE
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
::=====================================================================
:: DONE SETTING DEFAULTS
::=====================================================================
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

rem :: OPENSSL
rem SET OPENSSLDIR=%LAZGITBINDIR%\%FPCFULLTARGET%\openssl
rem for /F %%i in ('dir /b "%OPENSSLDIR%\*.*"') do (
rem    SET HASOPENSSL=1
rem )

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

GOTO END

:USAGE
SET USAGE=1

:END

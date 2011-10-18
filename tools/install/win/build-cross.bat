:: check all the necessary parameters are given
if [%1]==[] goto USAGE
if [%2]==[] goto USAGE
if [%3]==[] goto USAGE
if [%4]==[] goto USAGE
if [%5]==[] goto USAGE


:: These settings are dependent on the configuration of the build machine
:: Path to the Inno Setup Compiler
if [%ISCC%]==[] SET ISCC="C:\Program Files\Inno Setup 5\iscc.exe"

:: Path to the directory containing the binutils for each target in a 
:: separate directory, for example arm-wince for the arm-wince target
if [%BINUTILSDIR%]==[] SET BINUTILSDIR=c:\lazarus\source\binutils

:: Path to build directory. 
:: In this directory an image of the installation will be built.
SET BUILDDIR=c:\temp\lazbuild

:: Path to the svn executable
if [%SVN%]==[] SET SVN="c:\program files\subversion\bin\svn.exe"

:: Set some environment variables from the command line
:: Path to the fpc sources checked out of fpcbuild svn repository
SET FPCSVNDIR=%1

:: Path to the lazarus sources checked out of subversion
SET LAZSVNDIR=%2

:: Path to latest release compiler
SET RELEASE_PPC=%3

SET TARGETCPU=%4
SET TARGETOS=%5

:: Some internal variables
SET OLDCURDIR=%CD%
SET OLDCURDRIVE=%CD:~,2%
SET BUILDDRIVE=%BUILDDIR:~,2%


SET FPCBINDIR=%FPCSVNDIR%\install\binw32
FOR /F %%L IN ('%FPCBINDIR%\gdate.exe +%%Y%%m%%d') DO SET DATESTAMP=%%L
SET FPCFULLTARGET=%TARGETCPU%-%TARGETOS%
FOR /F %%F IN ('svnversion.exe %LAZSVNDIR%') DO set LAZREVISION=%%F

SET TIMESTAMP=%date:~9,4%%date:~6,2%%date:~3,2%-%time:~,2%%time:~3,2%%time:~6,2%
SET MAKEEXE=%FPCBINDIR%\make.exe

:: set path to make sure the right tools are used
SET OLDPATH=%PATH%
PATH=%FPCBINDIR%;
cd %FPCSVNDIR%\fpcsrc

%MAKEEXE% distclean FPC=%RELEASE_PPC% 
rm -rf %FPCSVNDIR%\fpcsrc\compiler\*.exe
:: create a native compiler + utils
%MAKEEXE% compiler_cycle FPC=%RELEASE_PPC%

FOR /F %%L IN ('%FPCSVNDIR%\fpcsrc\compiler\utils\fpc.exe -PB') DO SET COMPILER=%FPCSVNDIR%\fpcsrc\compiler\%%L
FOR /F %%L IN ('%COMPILER% -iSO') DO SET FPCSourceOS=%%L
FOR /F %%L IN ('%FPCSVNDIR%\fpcsrc\compiler\utils\fpc.exe -P%TARGETCPU% -PB') DO SET PPCNAME=%%L
SET FPCFPMAKE=%COMPILER%

:: rebuild the rtl without WPO information
%MAKEEXE% rtl_clean rtl PP=%COMPILER%
%MAKEEXE% -C packages fpmkunit_all fcl-base_all FPC=%COMPILER% 
%MAKEEXE% -C utils fpcm_all FPC=%COMPILER%
:: Add -gtttt so that ShowBuiltinCommand is initialized to false, bug in fpc 2.4.4 
%MAKEEXE% -C utils fpcmkcfg_all OPT="-gtttt" FPC=%COMPILER% 

%MAKEEXE% compiler FPC=%COMPILER% PPC_TARGET=%TARGETCPU% EXENAME=%PPCNAME%
IF ERRORLEVEL 1 GOTO CLEANUP
SET COMPILER=%FPCSVNDIR%\fpcsrc\compiler\%PPCNAME%
SET CPU_TARGET=%TARGETCPU%
SET OS_TARGET=%TARGETOS%
SET CROSSBINDIR=%BINUTILSDIR%\%FPCFULLTARGET%
SET BINUTILSPREFIX=%FPCFULLTARGET%-

%MAKEEXE% -C rtl clean FPC=%COMPILER%
%MAKEEXE% -C packages clean FPC=%COMPILER%
%MAKEEXE% rtl packages FPC=%COMPILER% 
IF ERRORLEVEL 1 GOTO CLEANUP

FOR /F %%L IN ('%COMPILER% -iV') DO SET FPCVERSION=%%L
FOR /F %%L IN ('%COMPILER% -iW') DO SET FPCFULLVERSION=%%L

SET INSTALL_BASE=%BUILDDIR%\image\fpc\%FPCVERSION%
SET INSTALL_BINDIR=%INSTALL_BASE%\bin\i386-win32

:: copy the binutils
rmdir /s /q %BUILDDIR%
gmkdir -p %INSTALL_BINDIR%
cp %CROSSBINDIR%\* %INSTALL_BINDIR%

%MAKEEXE% rtl_install packages_install FPCMAKE=%FPCSVNDIR%\fpcsrc\utils\fpcm\fpcmake.exe INSTALL_PREFIX=%INSTALL_BASE% FPC=%COMPILER%

copy %COMPILER% %INSTALL_BINDIR%
%FPCSVNDIR%\fpcsrc\utils\fpcmkcfg\fpcmkcfg.exe -d "basepath=%INSTALL_BASE%" -o %INSTALL_BINDIR%\fpc.cfg
SET COMPILER=%INSTALL_BINDIR%\%PPCNAME%

gmkdir -p %BUILDDIR%\packager
%SVN% export %LAZSVNDIR%\packager\registration %BUILDDIR%\packager\registration
cd %BUILDDIR%\packager\registration
%MAKEEXE% FPC=%compiler%
IF ERRORLEVEL 1 GOTO CLEANUP
gmkdir -p %BUILDDIR%\image\packager\units
cp -pr %BUILDDIR%\packager\units\%FPCFULLTARGET% %BUILDDIR%\image\packager\units\%FPCFULLTARGET%

gmkdir -p %BUILDDIR%\components
%SVN% export %LAZSVNDIR%\components\lazutils %BUILDDIR%\components\lazutils
cd %BUILDDIR%\components\lazutils
%MAKEEXE% FPC=%compiler%
IF ERRORLEVEL 1 GOTO CLEANUP
gmkdir -p %BUILDDIR%\image\components\lazutils
cp -pr %BUILDDIR%\components\lazutils\lib %BUILDDIR%\image\components\lazutils\lib

%SVN% export %LAZSVNDIR%\lcl %BUILDDIR%\lcl
%BUILDDRIVE%
cd %BUILDDIR%\lcl
%MAKEEXE% FPC=%compiler%
IF ERRORLEVEL 1 GOTO CLEANUP
gmkdir -p %BUILDDIR%\image\lcl\units
cp -pr %BUILDDIR%\lcl\units\%FPCFULLTARGET% %BUILDDIR%\image\lcl\units\%FPCFULLTARGET%

%SVN% export %LAZSVNDIR%\ideintf %BUILDDIR%\ideintf
:: export images dir, the ideintf includes them
%SVN% export %LAZSVNDIR%\images %BUILDDIR%\images
cd %BUILDDIR%\ideintf
IF ERRORLEVEL 1 GOTO CLEANUP
%MAKEEXE% FPC=%compiler%
gmkdir -p %BUILDDIR%\image\ideintf\units
cp -pr %BUILDDIR%\ideintf\units\%FPCFULLTARGET% %BUILDDIR%\image\ideintf\units\%FPCFULLTARGET%

gmkdir -p %BUILDDIR%\components
%SVN% export %LAZSVNDIR%\components\synedit %BUILDDIR%\components\synedit
cd %BUILDDIR%\components\synedit
IF ERRORLEVEL 1 GOTO CLEANUP
%MAKEEXE% FPC=%compiler%
gmkdir -p %BUILDDIR%\image\components\synedit\units
cp -pr %BUILDDIR%\components\synedit\units\%FPCFULLTARGET% %BUILDDIR%\image\components\synedit\units\%FPCFULLTARGET%

del %INSTALL_BINDIR%\fpc.cfg

%OLDCURDRIVE%
cd %OLDCURDIR%
FOR /F "delims='" %%F IN (%LAZSVNDIR%\ide\version.inc) DO set LAZVERSION=%%F

SET OutputFileName=lazarus-%LAZVERSION%-fpc-%FPCFULLVERSION%-cross-%FPCFULLTARGET%-%FPCSourceOS%
if [%BUILDLAZRELEASE%]==[] SET OutputFileName=lazarus-%LAZVERSION%-%LAZREVISION%-fpc-%FPCFULLVERSION%-%DATESTAMP%-cross-%FPCFULLTARGET%-%FPCSourceOS%

%ISCC% lazarus-cross.iss 

:CLEANUP
SET FPCFPMAKE=
SET CPU_TARGET=
SET OS_TARGET=
SET CROSSBINDIR=
SET BINUTILSPREFIX=
SET PATH=%OLDPATH%

goto STOP

:USAGE
@echo off
echo Usage:
echo build-cross.bat FPCSVNDIR LAZSVNDIR RELEASECOMPILER TARGETCPU TARGETOS
echo FPCSVNDIR: directory that contains a svn version of the fpcbuild repository
echo LAZSVNDIR: directory that contains a svn version of the lazarus repository
echo RELEASECOMPILER: bootstrapping compiler for building fpc
echo TARGETCPU: target CPU
echo TARGETOS: target operating system

:STOP

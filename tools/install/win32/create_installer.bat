:: These settings need to change for every build
SET LAZVERSION=0.9.2.2

:: These setting are dependent on the configuration of the build machine
:: Path to the Inno Setup Compiler
SET ISCC="C:\Program Files\Inno Setup 4\iscc.exe"

:: Path to the fpc sources checked out of cvs
SET FPCCVSDIR=c:\lazarus\source\fpc-1.9

:: Path to the lazarus sources checked out of cvs
SET LAZCVSDIR=c:\lazarus\source\lazarus

:: Path to fpc 1.0.10 compiler
SET RELEASE_PPC=c:\fpc\bin\ppc386-release.exe

:: Path to the directory containing some third party utilities used by fpc
:: it will be copied completely to the pp\bin\win32 directory
:: fpc supplies them in asldw32.zip, makew32.zip
SET FPCBINDIR=c:\lazarus\source\fpcbindir

:: Path to build directory. 
:: In this directory an image of the installation will be built.
SET BUILDDIR=c:\temp\lazbuild

:: Path to the tool to create an export using a local cvs directory
SET EXPORTCVS=%LAZCVSDIR%\tools\install\cvsexportlocal.exe


::=====================================================================
:: no change needed after this.

:: Some internal variables
SET MAKEEXE=%FPCBINDIR%\make.exe
SET LOGFILE=%CD%\installer.log
SET DATESTAMP=%date:~-4,4%%date:~-7,2%%date:~-10,2%
SET BUILDDRIVE=%BUILDDIR:~,2%

ECHO Starting at: > %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%

:: set path to make sure the right tools are used
SET OLDPATH=%PATH%
SET PATH=%FPCBINDIR%

:: copy lazarus dir
%EXPORTCVS% %LAZCVSDIR% %BUILDDIR% >> %LOGFILE%

:: copy fpc source
%EXPORTCVS% %FPCCVSDIR%\rtl %BUILDDIR%\fpcsrc\rtl >> %LOGFILE%
%EXPORTCVS% %FPCCVSDIR%\fcl %BUILDDIR%\fpcsrc\fcl >> %LOGFILE%
%EXPORTCVS% %FPCCVSDIR%\packages %BUILDDIR%\fpcsrc\packages >> %LOGFILE%

call build-fpc.bat
copy %FPCBINDIR%\*.* %BUILDDIR%\pp\bin\i386-win32 >> %LOGFILE% 
samplecfg.vbs

call build-lazarus.bat

:: do not create installer, if the required executables are not there
if not exist %BUILDDIR%\lazarus.exe goto END
if not exist %BUILDDIR%\startlazarus.exe goto END

:: create the installer
%ISCC% lazarus.iss >> installer.log

:: delete build dir
rd /s /q %BUILDDIR% > NUL

:END

SET PATH=%OLDPATH%

ECHO Finished at: >> %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%


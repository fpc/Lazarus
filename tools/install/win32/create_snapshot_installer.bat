
:: These settings are dependent on the configuration of the build machine
:: Path to the Inno Setup Compiler
SET ISCC="C:\Program Files\Inno Setup 5\iscc.exe"

:: Path to the fpc sources checked out of svn
::SET FPCSVNDIR=c:\lazarus\source\fpc\2.1
SET FPCSVNDIR=c:\lazarus\source\fpcbuild\2.0.4\fpcsrc
::SET FPCSVNDIR=c:\lazarus\source\fpc\2.0

:: Path to the lazarus sources checked out of subversion
SET LAZSVNDIR=c:\lazarus\source\lazsource

:: Path to fpc 2.0.2 compiler
SET RELEASE_PPC=c:\fpc\2.0.2\bin\i386-win32\ppc386.exe

:: Path to the directory containing some third party utilities used by fpc
:: it will be copied completely to the pp\bin\win32 directory
:: fpc supplies them in asldw32.zip, makew32.zip
SET FPCBINDIR=c:\lazarus\source\fpcbindir

:: Path to the directory containing the mingw gdb debugger installation
:: it should have the debugger with the name gdb.exe in its bin subdirectory
SET GDBDIR=c:\lazarus\source\mingw

:: Path to build directory. 
:: In this directory an image of the installation will be built.
SET BUILDDIR=c:\temp\lazbuild

:: Path to the svn executable
SET SVN="c:\program files\subversion\bin\svn.exe"

:: Path to the directory containing translated version of the GPL license
SET LICENSEDIR=c:\lazarus\source\license


::=====================================================================
:: no change needed after this.

:: Some internal variables
SET MAKEEXE=%FPCBINDIR%\make.exe
SET LOGFILE=%CD%\installer.log
SET DATESTAMP=%date:~-4,4%%date:~-7,2%%date:~-10,2%
SET BUILDDRIVE=%BUILDDIR:~,2%
SET CP=%FPCBINDIR%\cp.exe
FOR /F "delims='" %%F IN (%LAZSVNDIR%\ide\version.inc) DO (set LAZVERSION=%%F)

ECHO Starting at: > %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%

:: set path to make sure the right tools are used
SET OLDPATH=%PATH%
SET PATH=%FPCBINDIR%

:: copy lazarus dir
rmdir /s /q %BUILDDIR%
%SVN% export %LAZSVNDIR% %BUILDDIR% >> %LOGFILE%
..\..\svn2revisioninc %LAZSVNDIR% %BUILDDIR%\ide\revision.inc

call build-fpc.bat

:: copy fpc source
gmkdir -p %INSTALL_BASE%\source
%SVN% export %FPCSVNDIR%\rtl %INSTALL_BASE%\source\rtl >> %LOGFILE%
%SVN% export %FPCSVNDIR%\fcl %INSTALL_BASE%\source\fcl >> %LOGFILE%
%SVN% export %FPCSVNDIR%\packages %INSTALL_BASE%\source\packages >> %LOGFILE%

:: exit if no compiler has been made
if not exist %INSTALL_BINDIR%\ppc386.exe goto END

%CP% %FPCBINDIR%\*.* %INSTALL_BINDIR% >> %LOGFILE% 
%INSTALL_BINDIR%\fpcmkcfg.exe -d "basepath=%INSTALL_BASE%" -o %INSTALL_BINDIR%\fpc.cfg

call build-lazarus.bat

:: do not create installer, if the required executables are not there
if not exist %BUILDDIR%\lazarus.exe goto END
if not exist %BUILDDIR%\startlazarus.exe goto END

:: copy gdb into build dir
%CP% -pr %GDBDIR% %BUILDDIR%

:: create the installer
%ISCC% lazarus.iss >> installer.log

:: do not delete build dir, if installer failed.
if not exist output\lazarus-%LAZVERSION%-%DATESTAMP%-win32.exe goto END

:: delete build dir
::rd /s /q %BUILDDIR% > NUL

:END

SET PATH=%OLDPATH%

ECHO Finished at: >> %LOGFILE%
%FPCBINDIR%\gdate >> %LOGFILE%


SET OLDCURDIR=%CD%
SET OLDCURDRIVE=%CD:~,2%

SET SOURCE_DIR=%FPCSVNDIR%

%SOURCE_DIR:~,2%
cd %SOURCE_DIR%
%MAKEEXE% clean PP=%RELEASE_PPC% >> %LOGFILE% 2>&1
%MAKEEXE% compiler_cycle PP=%RELEASE_PPC% >> %LOGFILE% 2>&1

set COMPILER=%SOURCE_DIR%\compiler\ppc386.exe

%MAKEEXE% -C rtl clean PP=%COMPILER% >> %LOGFILE% 
%MAKEEXE% -C packages clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% -C fcl clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% rtl packages_base_all fcl packages_extra_all PP=%COMPILER% OPT="-g -gl -Ur -CX" >> %LOGFILE%
%MAKEEXE% utils PP=%COMPILER% OPT="-CX -XX -Xs" DATA2INC=%SOURCE_DIR%\utils\data2inc >> %LOGFILE%

FOR /F %%L IN ('%COMPILER% -iV') DO SET FPCVERSION=%%L
SET INSTALL_BASE=%BUILDDIR%\fpc\%FPCVERSION%
SET INSTALL_BINDIR=%INSTALL_BASE%\bin\i386-win32
%MAKEEXE% compiler_install rtl_install fcl_install packages_install utils_install INSTALL_PREFIX=%INSTALL_BASE% PP=%COMPILER% FPCMAKE=%FPCSVNDIR%\utils\fpcm\fpcmake.exe >> %LOGFILE%

SET COMPILER=%INSTALL_BINDIR%\ppc386.exe
%MAKEEXE% clean PP=%COMPILER% >> %LOGFILE%

%OLDCURDRIVE%
cd %OLDCURDIR%

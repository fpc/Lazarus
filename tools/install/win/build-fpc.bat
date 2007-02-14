SET OLDCURDIR=%CD%
SET OLDCURDRIVE=%CD:~,2%

SET SOURCE_DIR=%FPCSVNDIR%\fpcsrc

%SOURCE_DIR:~,2%
cd %SOURCE_DIR%
%MAKEEXE% clean PP=%RELEASE_PPC% >> %LOGFILE% 2>&1
%MAKEEXE% compiler_cycle PP=%RELEASE_PPC% >> %LOGFILE% 2>&1

FOR /F %%L IN ('%SOURCE_DIR%\compiler\utils\fpc.exe -PB') DO SET COMPILER=%SOURCE_DIR%\compiler\%%L

%MAKEEXE% -C rtl clean PP=%COMPILER% >> %LOGFILE% 
%MAKEEXE% -C packages clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% -C fcl clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% rtl packages_base_all fcl packages_extra_all PP=%COMPILER% OPT="-g -gl -Ur -CX" >> %LOGFILE%
%MAKEEXE% utils PP=%COMPILER% OPT="-CX -XX -Xs" DATA2INC=%SOURCE_DIR%\utils\data2inc >> %LOGFILE%

FOR /F %%L IN ('%COMPILER% -iV') DO SET FPCVERSION=%%L
SET INSTALL_BASE=%BUILDDIR%\fpc\%FPCVERSION%
SET INSTALL_BINDIR=%INSTALL_BASE%\bin\%FPCFULLTARGET%
%MAKEEXE% compiler_install rtl_install fcl_install packages_install utils_install INSTALL_PREFIX=%INSTALL_BASE% PP=%COMPILER% FPCMAKE=%SOURCE_DIR%\utils\fpcm\fpcmake.exe >> %LOGFILE%

::SET COMPILER=%INSTALL_BINDIR%\ppc386.exe
FOR /F %%L IN ('%INSTALL_BINDIR%\fpc.exe -PB') DO SET COMPILER=%%L
%MAKEEXE% clean PP=%COMPILER% >> %LOGFILE%

%OLDCURDRIVE%
cd %OLDCURDIR%

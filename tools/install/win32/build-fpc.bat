SET OLDCURDIR=%CD%
SET OLDCURDRIVE=%CD:~,2%

SET SOURCE_DIR=%FPCCVSDIR%
SET INSTALL_BASE=%BUILDDIR%\pp
SET INSTALL_BINDIR=%INSTALL_BASE%\bin\win32

copy %FPCBINDIR%\as.exe %FPCBINDIR%\asw.exe 
copy %FPCBINDIR%\ld.exe %FPCBINDIR%\ldw.exe 

%SOURCE_DIR:~,2%
cd %SOURCE_DIR%
%MAKEEXE% clean PP=%RELEASE_PPC% >> %LOGFILE%
%MAKEEXE% compiler_cycle PP=%RELEASE_PPC% >> %LOGFILE%

del %FPCBINDIR%\asw.exe 
del %FPCBINDIR%\ldw.exe 

set COMPILER=%SOURCE_DIR%/compiler/ppc386

%MAKEEXE% -C rtl clean PP=%COMPILER% >> %LOGFILE% 
%MAKEEXE% -C packages clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% -C fcl clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% rtl packages_base_all fcl packages_extra_all PP=%COMPILER% OPT="-g -gl" >> %LOGFILE%
%MAKEEXE% utils PP=%COMPILER% OPT="-CX -XX -Xs" >> %LOGFILE%

%MAKEEXE% compiler_install rtl_install fcl_install packages_install utils_install INSTALL_PREFIX=%INSTALL_BASE% PP=%COMPILER% FPCMAKE=%FPCCVSDIR%\utils\fpcm\fpcmake.exe >> %LOGFILE%

SET COMPILER=%INSTALL_BASE%\bin\win32\ppc386.exe
%MAKEEXE% clean PP=%COMPILER% >> %LOGFILE%

%OLDCURDRIVE%
cd %OLDCURDIR%

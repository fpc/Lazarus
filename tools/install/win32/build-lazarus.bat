SET OLDCURDIR=%CD%

SET COMPILER=%BUILDDIR%\pp\bin\win32\ppc386.exe

cd %BUILDDIR%
%MAKEEXE% clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% all OPT="-gl -Xs" PP=%COMPILER% >> %LOGFILE%
%FPCBINDIR%\strip.exe lazarus.exe

%FPCBINDIR%\strip.exe startlazarus.exe

cd %OLDCURDIR%
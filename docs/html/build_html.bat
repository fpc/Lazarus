REM
REM Builds all Lazarus docs
REM
REM Notes:
REM
REM 1 - If necessary, please fix the path so it finds fpdoc.exe (normally in your fpc compiler dir)
REM 2 - Before running this file, first compile the project build_lcl_docs.lpi
REM
PATH=C:\Programas\lazarus22\fpc\2.2.0\bin\i386-win32;%PATH%
build_lcl_docs.exe
pause
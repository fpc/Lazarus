REM
REM Builds all Lazarus docs and pack them in one .chm file
REM
REM Notes:
REM
REM 1 - Please fix the path in this file for your installation
REM 2 - Before running this file, first compile the project build_lcl_docs.lpi
REM
PATH=D:\programming\fpc\bin\i386-win32\
build_lcl_docs.exe --outfmt chm
pause

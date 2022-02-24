REM
REM Builds Lazarus documentation for LCL and LazUtils in HTML format.
REM
REM Requires a $(LazarusDir)\docs\chm directory which contains rtl.xct and fcl.xct.
REM Process fails if either the directory or the files are missing.
REM Process creates .\lcl and .\lazutils subdirectories for HTML output.
REM Update the content in locallclfooter.xml before starting the script.
REM
REM Notes:
REM
REM 1 - If necessary, please fix the path so it finds fpdoc.exe (normally in your fpc compiler dir)
REM 2 - Before running this file, first compile the project build_lcl_docs.lpi
REM ..\..\lazbuild.exe build_lcl_docs.lpi
REM

REM PATH=C:\lazarus\fpc\3.2.2\bin\x86_64-win64;%PATH%
PATH=C:\fpc331\fpc\bin\x86_64-win64;%PATH%

REM build html output without footers
REM .\build_lcl_docs.exe --outfmt=html --fpcdocs=..\chm

REM For FPDoc 3.2.X: build html output with footers in locallclfooter.xml
REM .\build_lcl_docs.exe --outfmt=html --fpcdocs=..\chm --footer=locallclfooter.xml

REM For FPDoc 3.3.X: build html output with footers in locallclfooter.xml
.\build_lcl_docs.exe --outfmt=html --fpcdocs=..\chm --footer=@locallclfooter.xml

REM For FPDoc 3.3.X: build html output with footer text
REM .\build_lcl_docs.exe --outfmt=html --fpcdocs=..\chm --footer="(c) 2022. All rights reserved."

pause

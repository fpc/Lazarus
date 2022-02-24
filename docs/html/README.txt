CHM and HTML Documentation for Lazarus
======================================

This directory contains the tools to create the HTML and CHM documentation for
Lazarus. The documentation is stored in fpdoc format in the docs/xml/ directory.

The build_lcl_docs program requires a docs/chm directory which contains rtl.xct
and fcl.xct. The process fails if either the directory or the files are missing.
The process creates lcl and lazutils subdirectories for output.
Update the content in locallclfooter.xml before starting the script.

Scripts contain a path to the FPDoc program which may need to be updated
to match your local installation. This allows either FPDoc version 3.2.X or
3.3.X to be used to generate documentation formats.

Note: It is recommended to use the FPDoc version from FPC trunk (main).
If you are unlucky and got a buggy revision, you can try with the released
FPDoc program.


For UNIXes:

build_html.sh and build_chm.sh will automatically create the documentation for the
output formats.

Build build_lcl_docs.lpi:
  ../../lazbuild build_lcl_docs.lpi

Update the PATH environment variable in build_chm.sh and build_html.sh for
your local installation. Run the script(s), or run build_lcl_docs directly.

Run a script:
  ./build_chm.sh
  ./build_html.sh

Run the program:
  ./build_lcl_docs --fpdoc /path/to/trunk/fpdoc/fpdoc --fpcdocs=../chm --outfmt chm --footer locallclfooter.xml


For Windows:

build_html.bat and build_chm.bat will automatically create the documentation for the
output formats.

Build build_lcl_docs.lpi:
  ..\..\lazbuild.exe build_lcl_docs.lpi

Update the PATH environment variable in build_chm.bat and build_html.bat for
  your local installation. Run the script(s), or run build_lcl_docs.exe directly.

Run a script:
  .\build_chm.bat
  .\build_html.bat

Run the program:
  .\build_lcl_docs.exe --fpdoc \path\to\trunk\fpdoc --fpcdocs=..\chm --outfmt chm --footer locallclfooter.xml
  .\build_lcl_docs.exe --fpdoc \path\to\trunk\fpdoc --fpcdocs=..\chm --outfmt html --footer locallclfooter.xml


FPDoc Version Differences

When using FPDoc 3.3.1 or higher, the syntax for the --footer argument was changed.

For FPDoc version 3.2.X, the syntax is:

  --footer=path/to/filename.ext

For  FPDoc versions 3.3.X, the syntax requires an @ symbol as a prefix for the
file name. For example:

  --footer=@path/to/filename.ext

FPDoc versions 3.3.X allows a string of text to be used as the footer.
For example:

  --footer="(c) 2022. All rights reserved."


All of the scripts have a commented version of the various commands using
the FPDoc 3.3.X syntax. Use the one needed for your system and FPDoc version.

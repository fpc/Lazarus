Compile Pascal Qt6 Interface from source:
========================================
 * Download and unpack the sources 
 * cd into the resulting unpacked directory
 * This directory contains amongs other things a Qt Project File Qt6Pas.pro
 * run qmake -query to inspect your Qt installation (could be named qmake6 on some distros)
 * qmake (creates Makefile based upon Qt6Pas.pro)
 * make (compiles)
 * make install (obtain super user rights with sudo or su)
 * make clean (cleans directory, do this when switching Qt versions)

Linux Note:
-----------
To use a different Qt then the system wide Qt,
use /PathToOtherQt/bin/qmake and learn about LD_LIBRARY_PATH
(export LD_LIBRARY_PATH=/PathToOtherQt/lib)


Mac OsX Note: 
-------------
qmake may create a xcode project 
instead of a g++ Makefile 
use xcodebuild to compile

to create a g++ Makefile:
QMAKESPEC=macx-g++ qmake

The qmake project is configured to create a 
framework instead of a plain library 

Windows Note:
-------------
Verify the QTDIR,QMAKESPEC environment variables.
e.g. QMAKESPEC=win32-g++ and that make -v
and gcc -v produce the expected result.
Be sure that mingw32 shipped with Qt6 is in PATH,
so bindings are built with correct mingw32 version.
eg. for Qt-6.2 you can set all paths in cmd and
build bindings.

set PATH=C:\Qt6Directory\Tools\mingwXXXX\bin;C:\Qt6Directory\6.2\mingwXXXX\bin;%PATH%
qmake -query
qmake
mingw32-make



Installation
============
When compiling from source,
you can use make install.

When using the binary packages,
see below. 

If preferred, first strip
the libraries before installation.

Linux Binary Installation
-------------------------
The libraries should be copied
to the distribution specific 
system library directory
e.g. /usr/lib. 
Ensure the symlinks are correct.
The .so.OneDigit link is used when running a program.(google soname)
The .so link is used when linking during development

Windows Binary Installation
---------------------------
The dll can be copied to e.g. 
the Qt bin directory, as this 
directory is already in the 
PATH environment variable.

Mac OsX Binary Installation
---------------------------
The provided package installs
to /Library/Frameworks 

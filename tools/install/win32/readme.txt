This readme describes how to create a lazarus installation package for win32.
The creation of an installation packages consists of three steps:
A. Setup the build machine.
B. Adjust create_installer.bat to your configuration.
C. Run create_installer.bat.

A Setup the build machine.

A.1 Inno Setup
I used Inno Setup, a free installer for Windows programs. You can download it from http://www.jrsoftware.org/. 

A.2 FPC sources
The build script assumes you have a cvs version of the fpc sources. For information about getting the fpc sources from cvs see: http://www.freepascal.org/ 

A.3 Lazarus sources
A Lazarus cvs tree, containing the lazarus sources.

A.4. fpc 1.0.10 compiler
You need a fpc 1.0.10 compiler  to bootstrap compilation of the current fpc version.

A.5 Debugger
Download http://prdownloads.sourceforge.net/mingw/gdb-6.0-20031011.exe?download and install it in some directory. Set GDBDIR to this directory.

A.6 Other binary utilities
You need some third party binary utilities, for example make, ld, strip. Download a recent binary fpc installer, and extract asldw32.zip and makew32.zip to a new directory.

Note: I wasn't able to build fpc using the make utility suplied by fpc, but used mingw32-make.exe. You can download that from mingw.sourceforge.net

B Adjust create_installer.bat to your configuration

Open the create_installer.bat in a text editor and set the LAZVERSION variable.
Check the other variables, to see if they match your configuration:
ISCC: Path to the Inno Setup Compiler
FPCCVSDIR: Path to the fpc sources checked out of cvs
LAZCVSDIR: Path to the lazarus sources checked out of cvs
RELEASE_PPC: Path to fpc 1.0.10 compiler
FPCBINDIR: Path to the directory containing some utilities used by fpc
GDBDIR: Path to the directory containing the mingw gdb debugger installation
BUILDDIR: Path to build directory. 
EXPORTCVS: Path to the tool to create an export using a local cvs directory

C Run create_installer.bat

Just do it. Wait 20 - 30 minutes. A log file named installer.log will be written to the current directory.

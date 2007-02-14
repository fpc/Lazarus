This readme describes how to create a lazarus installation package for win32.
The creation of an installation packages consists of three steps:
A. Setup the build machine.
B. Adjust create_installer.bat to your configuration.
C. Run create_installer.bat.

A Setup the build machine.

A.1 Inno Setup
I used Inno Setup, a free installer for Windows programs. You can download it from http://www.jrsoftware.org/. 

A.2 FPC sources
The build script assumes you have a svn version of the fpc sources. For information about getting the fpc sources from svn see: http://www.freepascal.org/ 

A.3 Lazarus sources
A Lazarus svn tree, containing the lazarus sources.

A.4. The latest release of the fpc compiler
You need a fpc 2.0.2 compiler (just the ppc386.exe) to bootstrap compilation of the current fpc version.

A.5 Debugger
Download http://prdownloads.sourceforge.net/mingw/gdb-6.0-20031011.exe?download and install it in some directory. Set GDBDIR to this directory.

A.6 Translations of the GPL license
The installer is able to show its messages in multiple languages. The lazarus svn tree doesn't contain the translations of the GPL languages. Download those translations from http://www.gnu.org/licenses/translations.html and put them in a directory. At the moment the following files are needed:
http://users.skynet.be/xterm/gpld.txt
http://www.magnux.org/doc/GPL-pt_BR.txt
Not for all languages the installer can handle at the moment, there are text files on http://www.gnu.org/licenses/translations.html. For these languages the official GPL is shown.

A.7 Other binary utilities
You need some third party binary utilities, for example make, ld, strip. Download a recent binary fpc installer, and extract asldw32.zip and makew32.zip to a new directory.


B Adjust create_installer.bat to your configuration

Open the create_installer.bat in a text editor and set the LAZVERSION variable.
Check the other variables, to see if they match your configuration:
ISCC: Path to the Inno Setup Compiler
FPCSVNDIR: Path to the fpc sources checked out of svn
LAZSVNDIR: Path to the lazarus sources checked out of svn
RELEASE_PPC: Path to fpc 2.0.2 compiler
FPCBINDIR: Path to the directory containing some utilities used by fpc
GDBDIR: Path to the directory containing the mingw gdb debugger installation
BUILDDIR: Path to build directory. 
LICENSEDIR: Path to the directory containing translations of the GPL license

C Run create_installer.bat

Just do it. Wait about 40 minutes (on 2.6 GHz P-IV). A log file named installer.log will be written to the current directory.

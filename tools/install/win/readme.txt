This readme describes how to create a lazarus installation package for win32.
The creation of an installation packages consists of three steps:
A. Setup the build machine.
B. Adjust create_installer.bat to your configuration.
C. Run create_installer.bat.

A Setup the build machine.

A.1 Inno Setup
I used Inno Setup, a free installer for Windows programs. You can download it from http://www.jrsoftware.org/, and you need the QuickStart Pack and during installation choose Inno Setup Preprocessor too.

A.2 Subversion
The build script assumes you have the Subversion installed. You can download it from http://subversion.tigris.org/, you can use the package with setup in name.

A.3 FPC sources
The build script assumes you have a svn version of the fpcbuild sources. For information about getting the fpc sources from svn see: http://www.freepascal.org/

A.4 Lazarus sources
A Lazarus svn tree, containing the lazarus sources.

A.5 Lazarus binaries
Some binaries from a lazarus/binaries svn tree. These binaries (like gdb and the qt interface dll) are distributed with Lazarus, but are not built from source.

A.6. The latest release of the fpc compiler
Currently the latest release is fpc 2.2.0. You need just the ppcXXX.exe (ppc386.exe for win32 and ppcx64.exe for win64) to bootstrap compilation of the current fpc version.

B Adjust create_installer.bat to your configuration

Open the create_installer.bat in a text editor and check the variables, to see if they match your configuration:
ISCC: Path to the Inno Setup Compiler .exe file.
BUILDDIR: Path to build directory.
SVN: Path to the Subversion .exe file.

C Run create_installer.bat

Run the script:

create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC

where:
FPCSVNDIR: Path to the fpc sources checked out of svn
LAZSVNDIR: Path to the lazarus sources checked out of svn
LAZSVNBINDIR: Path to the svn lazarus binaries
RELEASE_PPC: Path to fpc 2.0.2 compiler

Just do it. Wait about 40 minutes (on 2.6 GHz P-IV). A log file named installer.log will be written to the current directory and setup file will be in the Output subdirectory of the current directory.

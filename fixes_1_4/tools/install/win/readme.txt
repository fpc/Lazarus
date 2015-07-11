This readme describes how to create a lazarus installation package for win32.
The creation of an installation packages consists of three steps:
A. Setup the build machine.
B. Adjust create_installer.bat to your configuration.
C. Run create_installer.bat.

A Setup the build machine.

A.1 Inno Setup
I used Inno Setup, a free installer for Windows programs. You can download it from http://www.jrsoftware.org/, and you need the QuickStart Pack and during installation choose Inno Setup Preprocessor too.

A.2 Subversion
The build script assumes you have the Subversion client installed. You can download it from http://subversion.tigris.org/, you can use the package with setup in name.

A.3 FPC sources
The build script assumes you have a svn version of the fpcbuild sources. For information about getting the fpc sources from svn see: http://www.freepascal.org/

A.4 Lazarus sources
A Lazarus svn tree, containing the lazarus sources.

A.5 Lazarus binaries
Some binaries from a lazarus/binaries svn tree. These binaries (like gdb and the qt interface dll) are distributed with Lazarus, but are not built from source.

A.6. The latest release of the fpc compiler
Currently the latest release is fpc 2.6.0. You need just the ppcXXX.exe (ppc386.exe for win32 and ppcx64.exe for win64) to bootstrap compilation of the current fpc version.

A.7. The CHM help files for Lazarus
If you want to include CHM help files in the installer, you'll need a directory with the FCL, RTL, LCL etc .CHM files (and associated index etc files, optionally including subdirectories) .

B Adjust create_installer.bat to your configuration

Open the create_installer.bat in a text editor and check the variables, to see if they match your configuration:
ISCC: Path to the Inno Setup Compiler .exe file.
BUILDDIR: Path to build directory.
SVN: Path to the Subversion .exe file.

Alternatively, you can set these as environment variables before you run the batch script, e.g.:
SET ISCC="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
SET LAZTEMPBUILDDIR="c:\temp\lazarusbuild"
SET SVN="C:\Program Files\Subversion\bin\svn.exe"

C Run create_installer.bat

Run the script:

create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC
or
create_installer.bat FPCSVNDIR LAZSVNDIR LAZSVNBINDIR RELEASE_PPC IDE_WIDGETSET PATCHFILE CHMHELPFILES

where:
FPCSVNDIR: Path to the fpc sources checked out of svn (see A.3)
LAZSVNDIR: Path to the lazarus sources checked out of svn (see A.4)
LAZSVNBINDIR: Path to the svn lazarus binaries (see A.5)
RELEASE_PPC: Path to the FPC compiler required to start the build of fpc it FPCSVNDIR (see A.6)
IDE_WIDGETSET: Optional: IDE widgetset to be created. If not needed: don't enter it or use ""
PATCHFILE: Optional: name of FPC patch file for the FPC sources. If not needed: don't enter it or use ""
CHMHELPFILES: Optional: directory containing CHM help files to be included in the installer (see A.7). If not needed: don't enter it or use ""

Just do it. Wait about 40 minutes (on 2.6 GHz P-IV). A log file named installer.log will be written to the current directory and setup file will be in the Output subdirectory of the current directory.

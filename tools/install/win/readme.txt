This readme describes how to create a lazarus installation package for win32.
The creation of an installation packages consists of three steps:
A. Setup the build machine.
B. Run create_installer.bat.
C. Build cross installer add-on
D. Other info
E. UpdateInnoRemovedFilesFromGitDiff

### A Setup the build machine. ###

A.1 Inno Setup
I used Inno Setup, a free installer for Windows programs. You can download it from http://www.jrsoftware.org/, and you need the QuickStart Pack and during installation choose Inno Setup Preprocessor too.

A.2 GIT
The build script assumes you have git installed.

A.3 FPC sources https://gitlab.com/freepascal.org/fpc/build
The build script assumes you have a git repository of the **fpcbuild** sources. For information about getting the fpc sources from git see: http://www.freepascal.org/
Among others, the repository has the folders
  install with subdirs per target
  fpcsrc  with a git submodule of the fpc source git

A.4 Lazarus sources https://gitlab.com/freepascal.org/lazarus/lazarus
A Lazarus git repository tree, containing the lazarus sources.

A.5 Lazarus binaries https://gitlab.com/freepascal.org/lazarus/binaries
Some binaries from the lazarus/binaries git repository. These binaries (like gdb and the qt interface dll) are distributed with Lazarus, but are not built from source.

A.6. The latest release of the fpc compiler
You need just the ppcXXX.exe (ppc386.exe for win32 and ppcx64.exe for win64) to bootstrap compilation of the current fpc version.

A.7. The CHM help files for Lazarus
If you want to include CHM help files in the installer, you'll need a directory with the FCL, RTL, LCL etc .CHM files (and associated index etc files, optionally including subdirectories) .


### B. Run create_installer.bat. ###

B.1 Environment

You should set the following environment before calling the script
SET GIT=C:\____\git.exe
SET ISCC="C:\Program Files (x86)\Inno Setup _____\iscc.exe"
SET LAZTEMPBUILDDIR=C:\temp\

Alternatively you can open the create_installer.bat in a text editor and check the variables, to see if the defaults match your system:
ISCC: Path to the Inno Setup Compiler .exe file.
BUILDDIR: Path to build directory.
GIT: Path to the Subversion .exe file.


B.2 Build

Call the script:

  create_installer.bat  %FPC_GIT_BUILD_DIR%   %LAZ_GIT_BUILD_DIR%   %LAZBIN_GIT_BUILD_DIR%  %PPC_RELEASE_EXE%  ""  ""  %LAZCHM_DIR%

where
%FPC_GIT_BUILD_DIR%  is the folder with the fpcbuild repository
%LAZ_GIT_BUILD_DIR%  is the folder with the lazarus git
%LAZBIN_GIT_BUILD_DIR% is the folder with the lazarus binary git
%PPC_RELEASE_EXE%   is the full-filename of the ppc___.exe
""  Optional: IDE widgetset to be created. If not needed: don't enter it or use ""
""  Optional: name of FPC patch file for the FPC sources. If not needed: don't enter it or use ""
%LAZCHM_DIR%  The chm help files (usually in docs\chm in the binary lazarus git repository)


### C. Build cross installer add-on ###

Set the environment as in B.1

  SET BINUTILSDIR=c:\lazarus\source\binutils
     to a directory containing a folder   %BINUTILSDIR%\%WANTCPU%-%WANTOS%
     with the cross binutils (as, ld, ...) 
     This can be found in the FPC-build gin "install\crossbinwce" "install\crossbinw64"
  
  
  SET LAZBUILD_REPLACE_TEXT=C:\temp\ReplaceText.exe
     the replace text utility / lpi in the Lazarus repository tools\install\win\ReplaceText\


  SET INST_TARGET_NAME=win64
     when building the cross-win-32bit installer, that is to be used by the 64bit IDE
     This changes the last bit of the istaller filename lazarus-x.y-fpc-a.b.c-cross-i386-win32-WIN32 to WIN64 
     (the last segment should indicate the target on which the install should run)


Call the script:

  build-cross.bat   %FPC_GIT_BUILD_DIR%     %LAZ_GIT_BUILD_DIR%   %PPC_RELEASE_EXE%  %WANTCPU% %WANTOS% %LAZBIN_SVN_BUILD_DIR% %SKIPCROSS% %INSTPREFIX%

where
%FPC_GIT_BUILD_DIR%  is the folder with the fpcbuild repository
%LAZ_GIT_BUILD_DIR%  is the folder with the lazarus git
%PPC_RELEASE_EXE%   is the full-filename of the ppc___.exe
%WANTCPU%  is e.g. i386 x86_64 arm
%WANTOS%   is e.g. win32 win64 wince
%SKIPCROSS%    Set to "1" and it builds a native instead of a cross compiler
%INSTPREFIX%  e.g. i386-win32 or x86_64-win64
              The prefix where the non-cross compiler is.
              E.g if compiling ON a i386-win32 computer for any cross target then it is i386-win32


SKIPCROSS is for the case of building a compiler for TARGET i386-win32 that can be run on a 64bit Windows system.
A real cross compiler would not handle some float types correctly. So in this case a native i386-win32 compiler is build,
but name as if it was a cross compiler.



### D. Other info ###

Environment %LAZBUILD_HOOK_DIR% can be set to a folder containing the script
  lazhook_before_iscc.bat
This will be run immediately before running the inno builder.
=> THis is NOT used to build the official installer


### E. UpdateInnoRemovedFilesFromGitDiff ###

Update the files
  RemovedFiles32.iss
  RemovedFiles64.iss

  UpdateInnoRemovedFilesFromGitDiff.exe C:\lazarus_git_root_dir  lazarus_X_Y lazarus_X_Z > RemovedFiles32.iss

#include "compiler:Default.isl"

[CustomMessages]


DelUserConf=Delete all user configuration files from previous installs
CleanUp=Clean up:

InstallQt=Globally install Qt interface DLL
InstallChm=Install CHM help files
InstallOpenSSL=Install openssl libraries (required by OnlinePackageManager)
InstallOpenSSLGlobal=Globally Install openssl libraries
AssociateGroup=Associate file extensions

CheckSecondClick=Create a new secondary installation
CheckSecondInfo=A secondary installation allows two or more versions of Lazarus to be installed. Each version will have its own configuration. Please read the FAQ on multiple installations before using this option.

FolderHasSpaces=Selected folder contains spaces, please select a folder without spaces in it.
FolderNotEmpty=The target folder is not empty. Continue with installation?
FolderNotEmpty2=The target folder is not empty.

FolderForSecondNoFile=The target folder is not empty and does not contain an upgradable secondary Lazarus installation.%0:sPlease choose an empty folder, or a folder with an existing secondary Lazarus installation for updating.
FolderForSecondBadFile=The target folder is not empty. The installer could not detect if it contains an upgradable secondary Lazarus installation.%0:sPlease choose an empty folder, or a folder with an existing secondary Lazarus installation for updating.
FolderForSecondUpgrading=The target folder is not empty.%0:sIt contains a secondary Lazarus installation using the following folder for configuration:%0:s%1:s%0:s%0:sContinue with installation?
FolderForSecondUpgradingPrimary=The target folder is not empty.%0:sIt contains a default (non-secondary) Lazarus installation.%0:sIf you continue, it will be changed into a secondary installation.%0:s%0:s%0:sContinue with installation?

FolderForSecondBadUninstall=The target folder is not empty. The installer could not verify if it is safe to use.%0:sPlease choose an empty folder, or a folder with an existing secondary Lazarus installation for updating.

SecondConfCapt=Select configuration folder
SecondConfCapt2=Where do you want this Lazarus installation to store its configuration?
SecondConfBody=Select a new empty folder for this installation of Lazarus to store its configuration, then continue with 'Next'.

FolderForConfig=Folder for config

FolderForConfNotEmpty=The selected folder is not empty.

AskUninstallTitle1=Previous Installation
AskUninstallTitle2=Do you want to run the uninstaller?
BtnUninstall=Uninstall
ChkContinue=Continue without uninstall

OldInDestFolder1=Another installation of %1:s exists in the destination folder. If you wish to uninstall first, please use the button below.
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=Another installation of %1:s was found at %2:s. Please use the button below to uninstall it now. If you wish to keep it, please tick the checkbox to continue.
OldInOtherFolder2=Warning: There may be conflicts between the different installations and they may not function properly.
OldInOtherFolder3=Note: You have not chosen a dedicated configuration folder for this installation.
OldInOtherFolder4=If you wish to use more than one installation, please go back and check: "Create a new secondary installation".

OldInBadFolder1=Warning: Another installation of %1:s was found at %2:s, but the uninstaller was found at %3:s. Please make sure the uninstaller is correct.
OldInBadFolder2=Warning: There may be conflicts between the different installations and they may not function properly.
OldInBadFolder3=Note: If you wish to use more than one installation, please go back and check: "Create a new secondary installation".
OldInBadFolder4=Please use the button below to uninstall it now. If you wish to keep it, please tick the checkbox to continue.

OldSecondInDestFolder1=Another installation of %1:s exists in the destination folder. If you wish to uninstall first, please use the button below.
OldSecondInDestFolder2=
OldSecondInDestFolder3=This is a secondary installation and the folder for configuration is (and will be kept):
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=Updating secondary installation with configuration in folder:%0:s%1:s%2:s
SecondTaskCreate=Creating secondary installation with configuration in folder:%0:s%1:s%2:s

DuringInstall=Some Info from our FAQ: http://wiki.lazarus.freepascal.org/Lazarus_Faq%0:s%0:s    What is Lazarus?%0:sLazarus is a cross-platform IDE for Pascal. Its aim is write once, compile anywhere.%0:s%0:s    How to reduce the exe file size?%0:sThe default binaries are very big because they include debug information. For release builds you can switch this off in the Project settings.%0:s%0:s    Licensing:%0:s- The LCL is licensed LGPL with linking exception. This allows you to create apps with any license you want, including proprietary.%0:s- The IDE is licensed GPL. If you distribute a modified IDE you must follow the GPL.%0:s- Other packages and components have various licenses. See the readme of each package.

UninstVerbose=About to uninstall %1:s from folder %0:s. Continue?

[_ISTool]
EnableISX=true
[Defines]
#define AppVersion GetEnv('LazVersion')
// LazVersion may be 0.9.30.2RC1
// A valid file version contains only digits, so drop the RC part
#if pos('RC',AppVersion)>0
  #define FileVersion = copy(AppVersion, 1, pos('RC', AppVersion)-1)
#else
  #define FileVersion = AppVersion
#endif
#define FPCVersion GetEnv('FPCVersion')
#define FPCTargetOS GetEnv('FPCTargetOS')
#define FPCFullTarget GetEnv('FPCFullTarget')
#define AppName "Lazarus"
#define SetupDate GetEnv('DateStamp')
#define BuildDir GetEnv('BuildDir')
#define QtInfDir GetEnv('QTINFDIR')
#define IDEWidgetSet GetEnv('IDE_WidgetSet')
#define OutputFileName GetEnv('OutputFileName')
#define CHMHELPFILES GetEnv('CHMHELPFILES')
[Setup]
AllowNoIcons=yes
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisherURL=http://www.lazarus.freepascal.org/
AppSupportURL=http://www.lazarus.freepascal.org/
AppUpdatesURL=http://www.lazarus.freepascal.org/
ArchitecturesInstallIn64BitMode=x64
DefaultDirName={code:GetDefDir|{sd}\lazarus}
DefaultGroupName={#AppName}
DirExistsWarning=no
OutputBaseFilename={#OutputFileName}
InternalCompressLevel=ultra
;InternalCompressLevel=ultra64
;Compression=lzma2/ultra64
SolidCompression=yes
VersionInfoVersion={#FileVersion}
VersionInfoTextVersion={#AppVersion}-{#SetupDate}
ShowLanguageDialog=yes
WizardImageFile=lazarus_install_cheetah.bmp
WizardSmallImageFile=lazarus_install_cheetah_small.bmp
WizardImageStretch=false
ShowTasksTreeLines=yes
TimeStampRounding=0
PrivilegesRequired=none
ChangesAssociations=yes
; prevent chekbox pre-set (for delete user conf). Latest inno supports unchecked checkedonce
UsePreviousTasks=no

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: delusersettings; Description: Delete all user configuration files from previous installs; GroupDescription: Clean up;  Flags: unchecked 
;unchecked checkedonce

[Components]
#if FPCTargetOS=="win32"
#if IDEWidgetSet!="qt"
Name: installqtintfdll; Description: Install QT interface dll; Types: custom full compact
#endif
#endif
#ifdef CHMHELPFILES
#if CHMHELPFILES!=""
Name: installhelp; Description: Install chm help files; Types: custom full
#endif
#endif
Name: association; Description: Associate file extensions; Types: custom full
Name: association/associatelfm; Description: {code:GetAssociateDesc|.lfm}; Types: custom full
Name: association/associatelpi; Description: {code:GetAssociateDesc|.lpi}; Types: custom full
Name: association/associatelpk; Description: {code:GetAssociateDesc|.lpk}; Types: custom full
Name: association/associatelpr; Description: {code:GetAssociateDesc|.lpr}; Types: custom full
Name: association/associateinc; Description: {code:GetAssociateDesc|.inc}; Types: custom full
Name: association/associatepas; Description: {code:GetAssociateDesc|.pas}; Types: custom full
Name: association/associatepp; Description: {code:GetAssociateDesc|.pp}; Types: custom full

[InstallDelete]
Name: {localappdata}\lazarus\*.xml; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\*.cfg; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\lazarus.dci; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\compilertest.pas; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\easydocklayout.lyt; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\laz_indentation.pas; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\staticpackages.inc; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\unitdictionarycodyunitdictionary*.tmp; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\projectsessions\*.lps; Type: files; Tasks: delusersettings
Name: {localappdata}\lazarus\userschemes\*.xml; Type: files; Tasks: delusersettings
#if FPCTargetOS=="win32"
#include "RemovedFiles32.iss"
#endif
#if FPCTargetOS=="win64"
#include "RemovedFiles64.iss"
#endif

[Files]
Source: {#BuildDir}\*.*; DestDir: {app}; Flags: recursesubdirs
Source: environmentoptions.xml; DestDir: {app}; Flags: confirmoverwrite; AfterInstall: UpdateEnvironmentOptions; DestName: environmentoptions.xml
#if FPCTargetOS=="win32"
#if IDEWidgetSet=="qt"
Source: {#QtInfDir}\*.dll; DestDir: {sys}; Flags: sharedfile replacesameversion
#else
Source: {#QtInfDir}\*.dll; DestDir: {sys}; Flags: sharedfile replacesameversion; Components: installqtintfdll; Tasks: 
#endif
#if FPCVersion=="2.2.0"
Source: {#BuildDir}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\cpp.exe; DestDir: {app}\ide; MinVersion: 1,0
#endif
#endif

#ifdef CHMHELPFILES
#if CHMHELPFILES!=""
Source: {#CHMHELPFILES}\*.*; DestDir: {app}\docs\chm; Components: installhelp; Flags: recursesubdirs
#endif
#endif

[INI]
Filename: {app}\Lazarus Home Page.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/
Filename: {app}\Lazarus Forums.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/index.php?action=forum
Filename: {app}\Lazarus Wiki Help.url; Section: InternetShortcut; Key: URL; String: http://wiki.lazarus.freepascal.org/

[Icons]
Name: {group}\{#AppName}; Filename: {app}\lazarus.exe; IconFilename: {app}\images\mainicon.ico; Comment: Open Source IDE for Free Pascal
Name: {group}\{cm:ProgramOnTheWeb,Lazarus}; Filename: {app}\Lazarus Home Page.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Forums; Filename: {app}\Lazarus Forums.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Wiki Help; Filename: {app}\Lazarus Wiki Help.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\{cm:UninstallProgram,Lazarus}; Filename: {uninstallexe}
Name: {userdesktop}\Lazarus; Filename: {app}\lazarus.exe; Tasks: desktopicon; IconFilename: {app}\images\mainicon.ico; Comment: Open Source IDE for Free Pascal
Name: {group}\{#AppName} (debug); Filename: {app}\startlazarus.exe; Parameters: --debug; WorkingDir: {app}; IconFilename: {app}\images\mainicon.ico; Comment: Lazarus --debug

[Run]
Filename: {app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\fpcmkcfg.exe; Parameters: "-d ""basepath={app}\fpc\$FPCVERSION"" -o ""{app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\fpc.cfg"""; Flags: runhidden; Languages: ; WorkingDir: {app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}; Components: 

[UninstallDelete]
Name: {app}\compilertest.pas; Type: files
Name: {app}\Lazarus Wiki Help.url; Type: files
Name: {app}\Lazarus Home Page.url; Type: files
Name: {app}\Lazarus Forums.url; Type: files
Name: {app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\fpc.cfg; Type: files
Name: {app}\lazarus.old.exe; Type: files
Name: {app}\lazarus.old2.exe; Type: files

[Registry]
; HKLM
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lfm; ValueType: String; ValueData: Lazarus Form; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lfm\DefaultIcon; ValueType: String; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lfm\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lfm\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpi; ValueType: String; ValueData: Lazarus Project Information; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpi\DefaultIcon; ValueType: String; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpi\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpi\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpk; ValueType: String; ValueData: Lazarus Package File; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpk\DefaultIcon; ValueType: String; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpk\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpk\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpr; ValueType: String; ValueData: Lazarus Project Main Source; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpr\DefaultIcon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpr\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.lpr\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.inc; ValueType: String; ValueData: Object Pascal Include File; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.inc\DefaultIcon; ValueType: String; ValueData: {app}\images\includefile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.inc\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\includefile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.inc\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pas; ValueType: String; ValueData: Pascal Source Code; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pas\DefaultIcon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pas\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pas\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pp; ValueType: String; ValueData: Pascal Source Code; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pp\DefaultIcon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pp\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Classes\Lazarus.AssocFile.pp\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\Classes\.lfm; ValueType: String; ValueData: Lazarus.AssocFile.lfm; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: association/associatelfm
Root: HKLM; Subkey: Software\Classes\.lpi; ValueType: String; ValueData: Lazarus.AssocFile.lpi; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: association/associatelpi
Root: HKLM; Subkey: Software\Classes\.lpk; ValueType: String; ValueData: Lazarus.AssocFile.lpk; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: association/associatelpk
Root: HKLM; Subkey: Software\Classes\.lpr; ValueType: String; ValueData: Lazarus.AssocFile.lpr; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: association/associatelpr
Root: HKLM; Subkey: Software\Classes\.inc; ValueType: String; ValueData: Lazarus.AssocFile.inc; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: association/associateinc
Root: HKLM; Subkey: Software\Classes\.pas; ValueType: String; ValueData: Lazarus.AssocFile.pas; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: association/associatepas
Root: HKLM; Subkey: Software\Classes\.pp; ValueType: String; ValueData: Lazarus.AssocFile.pp; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: association/associatepp

Root: HKLM; Subkey: Software\Lazarus\Capabilities; ValueType: String; ValueName: ApplicationName; ValueData: Lazarus IDE; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities; ValueType: String; ValueName: ApplicationDescription; ValueData: Open Source IDE for Free Pascal.; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities\FileAssociations; ValueName: .lfm; ValueType: String; ValueData: Lazarus.AssocFile.lfm; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities\FileAssociations; ValueName: .lpi; ValueType: String; ValueData: Lazarus.AssocFile.lpi; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities\FileAssociations; ValueName: .lpk; ValueType: String; ValueData: Lazarus.AssocFile.lpk; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities\FileAssociations; ValueName: .lpr; ValueType: String; ValueData: Lazarus.AssocFile.lpr; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities\FileAssociations; ValueName: .inc; ValueType: String; ValueData: Lazarus.AssocFile.inc; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities\FileAssociations; ValueName: .pas; ValueType: String; ValueData: Lazarus.AssocFile.pas; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: Software\Lazarus\Capabilities\FileAssociations; ValueName: .pp; ValueType: String; ValueData: Lazarus.AssocFile.pp; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: Software\RegisteredApplications; ValueType: String; ValueName: Lazarus; ValueData: Software\Lazarus\Capabilities; Flags: uninsdeletevalue; Check: IsHKLMWriteable

; HKCU
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lfm; ValueType: String; ValueData: Lazarus Form; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lfm\DefaultIcon; ValueType: String; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lfm\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lfm\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpi; ValueType: String; ValueData: Lazarus Project Information; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpi\DefaultIcon; ValueType: String; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpi\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpi\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpk; ValueType: String; ValueData: Lazarus Package File; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpk\DefaultIcon; ValueType: String; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpk\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpk\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpr; ValueType: String; ValueData: Lazarus Project Main Source; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpr\DefaultIcon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpr\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.lpr\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.inc; ValueType: String; ValueData: Object Pascal Include File; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.inc\DefaultIcon; ValueType: String; ValueData: {app}\images\includefile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.inc\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\includefile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.inc\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pas; ValueType: String; ValueData: Pascal Source Code; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pas\DefaultIcon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pas\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pas\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pp; ValueType: String; ValueData: Pascal Source Code; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pp\DefaultIcon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pp\Shell\Open; ValueName: Icon; ValueType: String; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: Software\Classes\Lazarus.AssocFile.pp\Shell\Open\Command; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: Software\Classes\.lfm; ValueType: String; ValueData: Lazarus.AssocFile.lfm; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: association/associatelfm
Root: HKCU; Subkey: Software\Classes\.lpi; ValueType: String; ValueData: Lazarus.AssocFile.lpi; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: association/associatelpi
Root: HKCU; Subkey: Software\Classes\.lpk; ValueType: String; ValueData: Lazarus.AssocFile.lpk; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: association/associatelpk
Root: HKCU; Subkey: Software\Classes\.lpr; ValueType: String; ValueData: Lazarus.AssocFile.lpr; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: association/associatelpr
Root: HKCU; Subkey: Software\Classes\.inc; ValueType: String; ValueData: Lazarus.AssocFile.inc; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: association/associateinc
Root: HKCU; Subkey: Software\Classes\.pas; ValueType: String; ValueData: Lazarus.AssocFile.pas; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: association/associatepas
Root: HKCU; Subkey: Software\Classes\.pp; ValueType: String; ValueData: Lazarus.AssocFile.pp; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: association/associatepp

[Languages]
Name: default; MessagesFile: compiler:Default.isl
Name: ca; MessagesFile: compiler:Languages\Catalan.isl
Name: cs; MessagesFile: compiler:Languages\Czech.isl
Name: de; MessagesFile: compiler:Languages\German.isl
Name: es; MessagesFile: compiler:Languages\Spanish.isl
Name: fi; MessagesFile: compiler:Languages\Finnish.isl
Name: fr; MessagesFile: compiler:Languages\French.isl
Name: hu; MessagesFile: compiler:Languages\Hungarian.isl
Name: it; MessagesFile: compiler:Languages\Italian.isl
Name: nl; MessagesFile: compiler:Languages\Dutch.isl
Name: no; MessagesFile: compiler:Languages\Norwegian.isl
Name: pl; MessagesFile: compiler:Languages\Polish.isl
Name: pt; MessagesFile: compiler:Languages\Portuguese.isl
Name: pt_BR; MessagesFile: compiler:Languages\BrazilianPortuguese.isl
Name: ru; MessagesFile: compiler:Languages\Russian.isl
;Slovak.isl not avail with latest inno setup
;Name: sk; MessagesFile: compiler:Languages\Slovak.isl
Name: sl; MessagesFile: compiler:Languages\Slovenian.isl

[Code]
type
  TUninstallState = (uiUnknown, UIDone, UIOtherNeeded, uiDestNeeded);
var 
  wpAskUnistall: TWizardPage;
  wpLabel1, wpLabel2, wpLabel3, wpLabel4: TNewStaticText;
  wpCheckBox: TNewCheckBox;
  wpButton: TNewButton;
  
  UninstallState, UninstallDoneState: TUninstallState;
  OldPath, OldName, UnInstaller: String;
  PathEqual: Boolean;


function GetUninstallData(s: String): String; // 'UninstallString'
var
  Path: String;
begin
  Path := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\lazarus_is1');
  Result := '';
  if not RegQueryStringValue(HKLM, Path, s, Result) then
    RegQueryStringValue(HKCU, Path, s, Result);
end;

function IsDirEmpty(s: String): Boolean;
var
	FindRec: TFindRec;
begin
  Result := not DirExists(s);
  if Result then exit;
  SetCurrentDir(s);
  Result := not FindFirst('*', FindRec);
  if Result then exit;
  if (FindRec.Name = '.') or (FindRec.Name = '..') then Result := not FindNext(FindRec);
  if (not Result) and ((FindRec.Name = '.') or (FindRec.Name = '..')) then Result := not FindNext(FindRec);
  FindClose(FindRec);
end;

procedure UpdateUninstallInfo;
begin 
  OldPath := '';
  OldName := '';
  UnInstaller := '';
  PathEqual := False;
  if UninstallState = uiDone then exit;

  UnInstaller := RemoveQuotes(GetUninstallData('UninstallString'));
  if (UnInstaller <> '') and FileExists(UnInstaller) then 
  begin
    OldPath := RemoveQuotes((GetUninstallData('Inno Setup: App Path')));
	OldName := GetUninstallData('DisplayName');
    PathEqual := (OldPath <> '') and (CompareText(RemoveBackslashUnlessRoot(OldPath), RemoveBackslashUnlessRoot(WizardDirValue)) = 0);
	if PathEqual then
      UninstallState := uiDestNeeded
	else
      UninstallState := uiOtherNeeded;

  end
  else
  begin
    UninstallState := uiDone;
  end;
end;

function NextButtonClick(CurPage: Integer): Boolean;
var
    folder: String;
    FolderEmpty: Boolean;
begin
  // by default go to next page
  Result := true;

  // if curpage is wpSelectDir check is filesystem
  if CurPage = wpSelectDir then
  begin
    folder := WizardDirValue;

    if Pos( ' ', folder ) > 0 then
    begin
      MsgBox( 'Selected folder contains spaces, please select a folder without spaces in it.', mbInformation, MB_OK );
      Result := false;
      exit;
    end

	UpdateUninstallInfo;
    UnInstaller := RemoveQuotes(GetUninstallData('UninstallString'));
    FolderEmpty := IsDirEmpty(folder);
    
    if ((UninstallState = uiDone) or (UninstallState = UIOtherNeeded)) and not(FolderEmpty) then begin
      // Dir NOT empty
        Result := MsgBox('The target folder is not empty. Continue with installation?', mbConfirmation, MB_YESNO) = IDYES;
    end;
	if not Result then exit;
  
  end;
    
end;

function GetDefDir( def: String ) : String;
begin
  if Pos( ' ', def ) > 0 then
  begin
    def := Copy( def, 1, Pos( ' ', def ) - 1 ) + '\NoFolderSpace';
  end;
  Result := def;
end;

procedure UpdateEnvironmentOptions();
var
  FileName: string;
  Content: string;
begin
  FileName := ExpandConstant(CurrentFileName);
  LoadStringFromFile(FileName, Content);
  StringChange(Content, '%Temp%', GetTempDir);
  StringChange(Content, '%LazDir%', ExpandConstant('{app}'));
  StringChange(Content, '%FpcBinDir%', ExpandConstant('{app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\'));
  SaveStringToFile(FileName, Content, False);
end;

function IsHKLMWriteable(): boolean;
begin
  Result := IsAdminLoggedOn or IsPowerUserLoggedOn;
end;

function IsHKLMNotWriteable: boolean;
begin
  Result := not IsHKLMWriteable();
end;

var
  PoFileStrings: TArrayOfString;

procedure LoadPoFile;
var
  PoFilename: string;
begin
  if (GetArrayLength(PoFileStrings)=0) then begin
    PoFilename := ExpandConstant('{app}\languages\installerstrconsts.{language}.po');
    if not FileExists(PoFileName) then
      PoFilename := ExpandConstant('{app}\languages\installerstrconsts.po');
    LoadStringsFromFile(PoFileName, PoFileStrings);
  end;
end;

function MultiByteToWideChar(CodePage, dwFlags: cardinal; lpMultiByteStr: PChar;
   cbMultiByte: integer; lpWideCharStr: PChar; cchWideChar: integer) : integer;
   EXTERNAL 'MultiByteToWideChar@kernel32.dll stdcall';

function WideCharToMultiByte(CodePage, dwFlags: cardinal; lpWideCharStr: PChar;
  cchWideChar: integer; lpMultiByteStr: PChar; cbMultiByte: integer;
  lpDefaultChar: integer; lpUsedDefaultChar: integer): integer;
   EXTERNAL 'WideCharToMultiByte@kernel32.dll stdcall';

function GetLastError: DWord;
   EXTERNAL 'GetLastError@kernel32.dll stdcall';

const
  CP_ACP = 0;
  CP_UTF8 = 65001;

function ConvertUTF8ToSystemCharSet(const UTF8String: string): string;
var
  UTF8Length: integer;
  UCS2String : string;
  SystemCharSetString: string;
  ResultLength: integer;
begin
  UTF8Length := length(UTF8String);
  // this is certainly long enough
  SetLength(UCS2String, length(UTF8String)*2+1);
  MultiByteToWideChar(CP_UTF8, 0,
    PChar(UTF8String), -1, PChar(UCS2String), UTF8Length + 1);
  SetLength(SystemCharSetString, Length(UTF8String));
  ResultLength := WideCharToMultiByte(CP_ACP, 0, PChar(UCS2String), -1,
    PChar(SystemCharSetString), Length(SystemCharSetString)+1, 0, 0) -1;
  Result := copy(SystemCharSetString, 1, ResultLength);
end;

function GetPoString(const msgid: string): string;
var
  Signature: string;
  i: integer;
  Count: integer;
begin
  LoadPoFile;
  //MsgBox(msgid, mbInformation, MB_OK);
  Result := msgid;
  Signature := '#: '+ msgid;
  Count := GetArrayLength(PoFileStrings);
  i := 0;
  while (i<Count) and (PoFileStrings[i]<>Signature) do begin
    i := i+1;
  end;
  if i+2<Count then begin
    Result := copy(PoFileStrings[i+2],9, Length(PoFileStrings[i+2])-9);
    //MsgBox(Result, mbInformation, MB_OK);
    if Result='' then
      Result := copy(PoFileStrings[i+1],8, Length(PoFileStrings[i+1])-8);
  end;
  Result := ConvertUTF8ToSystemCharSet(Result);
end;

function GetAssociateDesc(const ext: string): string;
var
  AmpersandPos: integer;
begin
  Result := FmtMessage(CustomMessage('AssocFileExtension'), ['Lazarus',ext]);
  AmpersandPos := pos('&', Result);
  if AmpersandPos>0 then
    Delete(Result, AmpersandPos, 1);
end;

procedure InitAskUninstall(s1, s2, s3, s4: String);
begin
  wpLabel1.Caption := s1;
  wpLabel2.Caption := s2;
  wpLabel3.Caption := s3;
  wpLabel4.Caption := s4;
  
  wpLabel1.AdjustHeight;
  wpLabel2.AdjustHeight;
  wpLabel3.AdjustHeight;
  wpLabel4.AdjustHeight;
  
  wpLabel2.Top := wpLabel1.Top + wpLabel1.Height + ScaleY(5);
  wpLabel3.Top := wpLabel2.Top + wpLabel2.Height + ScaleY(5);
  wpLabel4.Top := wpLabel3.Top + wpLabel3.Height + ScaleY(5);
  wpButton.Top := wpLabel4.Top + wpLabel4.Height + ScaleY(20);
end;
  
procedure UnInstUpdateGUI;
begin
  UpdateUninstallInfo;
  
  WizardForm.NextButton.Enabled := (UninstallState = uiDone) or (UninstallState = uiDestNeeded) or wpCheckBox.Checked;
  wpCheckBox.Enabled := not(UninstallState = uiDone);
  wpButton.Enabled := not(UninstallState = uiDone);  
end;

procedure ActivateAskUninst(Sender: TWizardPage);
begin
  UnInstUpdateGUI;
end;

function SkipAskUninst(Sender: TWizardPage): Boolean;
begin
  Result := UninstallState = uiDone;
  if Result Then exit;
  
  UnInstUpdateGUI;
  //UpdateUninstallInfo;
  
  //FolderEmpty := IsDirEmpty(WizardDirValue);

  if UninstallState = uiDestNeeded then begin
	wpLabel2.Font.Color := clDefault;
    wpCheckBox.Visible := False;
	InitAskUninstall(
	  'Another installation of "'+OldName+'" exists in the destination folder. If you wish to uninstall first, please use the button below.',
	  '',
	  '',
	  ''
	);
  end
  else
  begin	
	wpLabel2.Font.Color := clRed;
    wpCheckBox.Visible := True;
	InitAskUninstall(
	  'Another installation of "'+OldName+'" was found at "'+OldPath+'". Please use the button below to uninstall it now. If you wish to keep it, please tick the checkbox to continue.',
	  'Note: Using multiple copies of Lazarus is not supported by this installer.',
	  'Using several installations of Lazarus can lead to conflicts in files shared by all of the installations, such as the IDE configuration.',
	  'If you wish to use more than one installation, then you must do additional setup after this installation finished. Please see the Lazarus web page for this, and how to use --primary-config-path'
	);
  end;

end;
  
procedure UnInstBtnClick(Sender: TObject);
var
  UnInstaller: String;
  b, FolderEmpty : Boolean;
  i: integer;
begin
  UninstallDoneState := UninstallState;
  UninstallState := uiDone;
  
  UnInstaller := RemoveQuotes(GetUninstallData('UninstallString'));
  
  b := (UnInstaller <> '') and FileExists(UnInstaller);
  if b then b := Exec(UnInstaller, '/SILENT /NORESTART','', SW_SHOW, ewWaitUntilTerminated, i);
  if not b then  
    MsgBox('Uninstall failed.', mbConfirmation, MB_OK)
  else begin
    if (UninstallDoneState = uiDestNeeded) then
    begin
      FolderEmpty := IsDirEmpty(WizardDirValue);
	  if not FolderEmpty then begin Sleep(500); FolderEmpty := IsDirEmpty(WizardDirValue); end;
	  if not FolderEmpty then begin Sleep(500); FolderEmpty := IsDirEmpty(WizardDirValue); end;
	  if not FolderEmpty then begin Sleep(500); FolderEmpty := IsDirEmpty(WizardDirValue); end;
      if not(FolderEmpty) then begin
        // Dir NOT empty, after uninstall
        MsgBox('The target folder is not empty.', mbConfirmation, MB_OK);
      end;
    end;
  end;

  UnInstUpdateGUI;
end;

procedure UnInstCheckboxClick(Sender: TObject);
begin
  UnInstUpdateGUI;
end;
  
procedure InitializeWizard();
begin
  wpAskUnistall := CreateCustomPage(wpSelectDir, 'Previous Installation', 'Do you want to run the uninstaller?');
  wpAskUnistall.OnShouldSkipPage := @SkipAskUninst;
  wpAskUnistall.OnActivate := @ActivateAskUninst;
  
  wpLabel1 := TNewStaticText.Create(wpAskUnistall);
  wpLabel1.Parent := wpAskUnistall.Surface;
  wpLabel1.Top := 0;  
  wpLabel1.Left := 0;  
  wpLabel1.Width := wpAskUnistall.SurfaceWidth;  
  wpLabel1.Autosize:= False;
  wpLabel1.WordWrap := True;
  wpLabel1.Caption := '';
  
  wpLabel2 := TNewStaticText.Create(wpAskUnistall);
  wpLabel2.Parent := wpAskUnistall.Surface;
  wpLabel2.Left := 0;  
  wpLabel2.Width := wpAskUnistall.SurfaceWidth;  
  wpLabel2.Autosize:= False;
  wpLabel2.WordWrap := True;
  wpLabel2.Caption := '';
  
  wpLabel3 := TNewStaticText.Create(wpAskUnistall);
  wpLabel3.Parent := wpAskUnistall.Surface;
  wpLabel3.Left := 0;  
  wpLabel3.Width := wpAskUnistall.SurfaceWidth;  
  wpLabel3.Autosize:= False;
  wpLabel3.WordWrap := True;
  wpLabel3.Caption := '';
  
  wpLabel4 := TNewStaticText.Create(wpAskUnistall);
  wpLabel4.Parent := wpAskUnistall.Surface;
  wpLabel4.Left := 0;  
  wpLabel4.Width := wpAskUnistall.SurfaceWidth;  
  wpLabel4.Autosize:= False;
  wpLabel4.WordWrap := True;
  wpLabel4.Caption := '';
  
  wpButton := TNewButton.Create(wpAskUnistall);
  wpButton.Parent := wpAskUnistall.Surface;
  wpButton.Width := ScaleX(80);
  wpButton.Left := (wpAskUnistall.SurfaceWidth div 2) - ScaleX(40);
  wpButton.Caption := 'Uninstall';
  wpButton.OnClick := @UnInstBtnClick;

  wpCheckBox := TNewCheckBox.Create(wpAskUnistall);
  wpCheckBox.Parent := wpAskUnistall.Surface;
  wpCheckBox.Top := wpAskUnistall.SurfaceHeight - wpCheckBox.Height - 1;
  wpCheckBox.Width := wpAskUnistall.SurfaceWidth;  
  wpCheckBox.Caption := 'Continue without uninstall';
  wpCheckBox.OnClick := @UnInstCheckboxClick;
  
  UninstallState := uiUnknown;
  UninstallDoneState := uiUnknown;
 
end;



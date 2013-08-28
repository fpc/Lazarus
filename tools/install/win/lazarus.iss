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
; AddId: registry/uninstall info: Max 127 char
AppId={code:GetAppId}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisher=Lazarus Team
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
ShowTasksTreeLines=true
TimeStampRounding=0
PrivilegesRequired=none
ChangesAssociations=yes
; prevent checkbox pre-set (for delete user conf). Latest inno supports unchecked checkedonce
UsePreviousTasks=no
; since appid can change, UsePreviousLanguage must be off
UsePreviousLanguage=no
UninstallDisplayIcon={app}\lazarus.exe

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: delusersettings; Description: {cm:DelUserConf}; GroupDescription: {cm:CleanUp};  Flags: unchecked 
;unchecked checkedonce

[Components]
#if FPCTargetOS=="win32"
#if IDEWidgetSet!="qt"
Name: installqtintfdll; Description: {cm:InstallQt}; Types: custom full compact
#endif
#endif
#ifdef CHMHELPFILES
#if CHMHELPFILES!=""
Name: installhelp; Description: {cm:InstallChm}; Types: custom full
#endif
#endif
Name: association; Description: {cm:AssociateGroup}; Types: custom full
Name: association/associatelfm; Description: {code:GetAssociateDesc|.lfm}; Types: custom full
Name: association/associatelpi; Description: {code:GetAssociateDesc|.lpi}; Types: custom full
Name: association/associatelpk; Description: {code:GetAssociateDesc|.lpk}; Types: custom full
Name: association/associatelpr; Description: {code:GetAssociateDesc|.lpr}; Types: custom full
Name: association/associateinc; Description: {code:GetAssociateDesc|.inc}; Types: custom full
Name: association/associatepas; Description: {code:GetAssociateDesc|.pas}; Types: custom full
Name: association/associatepp; Description: {code:GetAssociateDesc|.pp}; Types: custom full

[InstallDelete]
Name: {code:GetPCPForDelete}*.xml; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}*.cfg; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}lazarus.dci; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}compilertest.pas; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}easydocklayout.lyt; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}laz_indentation.pas; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}staticpackages.inc; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}unitdictionarycodyunitdictionary*.tmp; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}projectsessions\*.lps; Type: files; Tasks: delusersettings
Name: {code:GetPCPForDelete}userschemes\*.xml; Type: files; Tasks: delusersettings
#if FPCTargetOS=="win32"
#include "RemovedFiles32.iss"
#endif
#if FPCTargetOS=="win64"
#include "RemovedFiles64.iss"
#endif

[Files]
Source: {#BuildDir}\*.*; DestDir: {app}; Flags: recursesubdirs
Source: environmentoptions.xml; DestDir: {app}; AfterInstall: UpdateEnvironmentOptions; DestName: environmentoptions.xml
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
Name: default; MessagesFile: lazarus.def.isl
Name: ca; MessagesFile: compiler:Languages\Catalan.isl
Name: cs; MessagesFile: compiler:Languages\Czech.isl
Name: de; MessagesFile: lazarus.de.isl
Name: es; MessagesFile: lazarus.es.isl
Name: fi; MessagesFile: compiler:Languages\Finnish.isl
Name: fr; MessagesFile: lazarus.fr.isl
Name: hu; MessagesFile: compiler:Languages\Hungarian.isl
Name: it; MessagesFile: compiler:Languages\Italian.isl
Name: nl; MessagesFile: compiler:Languages\Dutch.isl
Name: no; MessagesFile: compiler:Languages\Norwegian.isl
Name: pl; MessagesFile: compiler:Languages\Polish.isl
Name: pt; MessagesFile: compiler:Languages\Portuguese.isl
Name: pt_BR; MessagesFile: lazarus.pt_BR.isl
Name: ru; MessagesFile: lazarus.ru.isl
;Slovak.isl not avail with latest inno setup
;Name: sk; MessagesFile: compiler:Languages\Slovak.isl
Name: sl; MessagesFile: compiler:Languages\Slovenian.isl

[Code]
type
  TUninstallState = (
    uiUnknown,
    UIDone,         // There IS no uninstaller, OR it was already executed during this install
    UIOtherNeeded,  // The uninstaller ('Inno Setup: App Path') points to a different Path than "WizardFolder"
                    // Uninstall for OTHER folder NEEDED
    uiDestNeeded,   // Uninstaller for "WizardFolder" found
                    // Uninstall for DESTINATION folder NEEDED
    uiInconsistent  // Path of uninstaller and lazarus to be removed, do not match
  );
  TCfgFileState = (csNoFile, csUnreadable, csParsedOk);

var
  wpAskUnistall: TWizardPage;
  wpLabel1, wpLabel2, wpLabel3, wpLabel4: TNewStaticText;
  wpCheckBox: TNewCheckBox;
  wpButton: TNewButton;
  
  wpAskConfDir: TInputDirWizardPage;
  CheckSecondInstall: TCheckBox;
  CheckSecondLabel: TLabel;
  
  UninstallState, UninstallDoneState: TUninstallState;
  UnInstallerInAppPath: Boolean; // The uninstaller is in the directory that it will remove

  OldPath,              // Registry 'Inno Setup: App Path'
  OldName,              // Registry 'DisplayName'
  UnInstaller: String;  // Registry 'UninstallString'
  PathEqual: Boolean;

  IsSecondaryUpdate: Boolean;
  SecondPCP: String;
  NewCFGFile: TStringList;

  UninstDir: String;
  CFGFileForUninstDir: TStringList;

function GetAppId(param:string): String;
var
  s: String;
begin
  if ( (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked) ) or IsSecondaryUpdate then
  begin
	// Secondary
    s := RemoveBackslashUnlessRoot(Lowercase(WizardDirValue));
    Result := 'lazarus_sec_'+GetSHA1OfString(s) + '_' + IntToStr(length(s));
  end
  else
    Result := 'lazarus';
end;

function GetPCPForDelete(param:string): String;
begin
  if ( (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked) ) then
  begin
    if SecondPCP = '' then
      Result := AddBackslash(WizardDirValue) // some fallback
    else
      Result := AddBackslash(SecondPCP);
  end
  else
    Result := ExpandConstant('{localappdata}\lazarus\');
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

function SaveCustomMessage(AMsgId, ADefaulText: String): String;
begin
  try
    Result := CustomMessage(AMsgId);
  except
    Result := ADefaulText;
  end;
end;

function GetUninstallData(ARegName: String): String; // Get one entry from registry e.g. 'UninstallString'
var
  Path: String;
begin
  Path := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\'+GetAppId('')+'_is1');
  Result := '';
  if not RegQueryStringValue(HKLM, Path, ARegName, Result) then
    RegQueryStringValue(HKCU, Path, ARegName, Result);
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

    PathEqual := (OldPath <> '') and
                 (CompareText(RemoveBackslashUnlessRoot(OldPath), RemoveBackslashUnlessRoot(WizardDirValue)) = 0);
	if PathEqual then
      UninstallState := uiDestNeeded
	else
      UninstallState := uiOtherNeeded;

    UnInstallerInAppPath := (CompareText(RemoveBackslashUnlessRoot(OldPath), RemoveBackslashUnlessRoot(ExtractFilePath(UnInstaller))) = 0);
    if (not UnInstallerInAppPath) and
       ( (CompareText(RemoveBackslashUnlessRoot(OldPath), RemoveBackslashUnlessRoot(WizardDirValue)) = 0) or
         (CompareText(RemoveBackslashUnlessRoot(ExtractFilePath(UnInstaller)), RemoveBackslashUnlessRoot(WizardDirValue)) = 0)
       )
    then
      UninstallState := uiInconsistent;

  end
  else
  begin
    UninstallState := uiDone;
  end;
end;
  

function LoadCFGFile(AFolder: String; var AList: TStringList): Boolean;
var
  cfgfile: String;
begin
  if AList = nil then
    AList := TStringList.Create
  else
    AList.Clear;

  cfgfile := AddBackslash(AFolder) + 'lazarus.cfg';
  Result := FileExists(cfgfile);
  if not Result then
    exit;
  AList.LoadFromFile(cfgfile);
end;

procedure CreateCFGFile(APCP: String; var AList: TStringList);
var
  cfgfile: String;
begin
  if AList = nil then
    AList := TStringList.Create
  else
    AList.Clear;
  AList.add('--primary-config-path=' + APCP);
end;

function ParseCFGFile(AFolder: String; var APrimConfDir: String): TCfgFileState;
var
  s, cfgfile: String;
  i: Integer;
  l: TStringList;
begin
  cfgfile := AddBackslash(AFolder) + 'lazarus.cfg';

  Result := csNoFile;
  if not FileExists(cfgfile) then
    exit;

  Result := csUnreadable;
  l := TStringList.Create;
  l.LoadFromFile(cfgfile);
  for i := 0 to l.Count - 1 do
    if copy(l[i], 1, 6) = '--pcp=' then
      s := copy(l[i], 7, length(l[i]))
    else
    if copy(l[i], 1, 22) = '--primary-config-path=' then
      s := copy(l[i], 23, length(l[i]));
  l.Free;

  if s = '' then
    exit;

  if (s[1] = '"') and (s[length(s)] = '"') then
    s := copy(s, 2, length(s)-2)
  else
  if (s[1] = '''') and (s[length(s)] = '''') then
    s := copy(s, 2, length(s)-2)

  if s = '' then
    exit;

  if (not FileExists(AddBackslash(s) + 'environmentoptions.xml')) and
     (not IsDirEmpty(s))
  then
    exit;

  Result := csParsedOk;
  APrimConfDir := s;
end;

procedure CurPageChanged(CurPageID: Integer);
var
  m: TMemo;
begin
  if CurPageID = wpInstalling then
  begin
    WizardForm.ProgressGauge.Parent.Handle;
    m:= TMemo.Create(WizardForm);
    m.Parent:=WizardForm.ProgressGauge.Parent;
    m.Top := WizardForm.ProgressGauge.Top + WizardForm.ProgressGauge.Height + 10;
    m.Left := WizardForm.ProgressGauge.Left;
    m.Width := WizardForm.ProgressGauge.Width ;
    m.Height := WizardForm.ProgressGauge.Parent.Height - WizardForm.ProgressGauge.Height - WizardForm.ProgressGauge.Top - 15;
	m.ReadOnly  := True;
	m.WordWrap  := True;
	m.ScrollBars := ssVertical;
	
	m.Text := Format(CustomMessage('DuringInstall'), [#13#10]);
  end;
  
  if (CurPageID = wpSelectDir) and (CheckSecondInstall = nil)  then
  begin
    WizardForm.DirEdit.Parent.Handle;
	
	CheckSecondInstall := TCheckBox.Create(WizardForm);
    CheckSecondInstall.Parent:=WizardForm.DirEdit.Parent;
    CheckSecondInstall.Top := WizardForm.DirEdit.Top + WizardForm.DirEdit.Height + 10;
    CheckSecondInstall.Left := WizardForm.DirEdit.Left;
    CheckSecondInstall.Width := WizardForm.DirEdit.Parent.Width - WizardForm.DirEdit.Left;
	CheckSecondInstall.Caption := CustomMessage('CheckSecondClick');

	CheckSecondLabel := TLabel.Create(WizardForm);
    CheckSecondLabel.Parent:=WizardForm.DirEdit.Parent;
	CheckSecondLabel.AutoSize := False;
	CheckSecondLabel.WordWrap := True;
    CheckSecondLabel.Top := CheckSecondInstall.Top + CheckSecondInstall.Height + 10;
    CheckSecondLabel.Left := WizardForm.DirEdit.Left;
    CheckSecondLabel.Width := WizardForm.DirEdit.Parent.Width - WizardForm.DirEdit.Left;
	CheckSecondLabel.Height := WizardForm.DirEdit.Parent.Height - CheckSecondLabel.Top - 15;
	CheckSecondLabel.Caption := CustomMessage('CheckSecondInfo');
  end;
end;

function NextButtonClick(CurPage: Integer): Boolean;
var
    s, folder: String;
    FolderEmpty: Boolean;
begin
	// by default go to next page
	Result := true;

	// if curpage is wpSelectDir check is filesystem
	if (CurPage = wpSelectDir) then 
	begin
        IsSecondaryUpdate := False;
		folder := WizardDirValue;

		if Pos( ' ', folder ) > 0 then
		begin
			MsgBox(SaveCustomMessage('FolderHasSpaces', 'Selected folder contains spaces, please select a folder without spaces in it.'),
                   mbInformation, MB_OK );
			Result := false;
			exit;
		end;

		FolderEmpty := IsDirEmpty(folder);
		UpdateUninstallInfo;

        if FolderEmpty then
          exit;

		if ( (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked) ) then
		begin
		    // Secondary
            // ALways set "SecondPCP", if avail
            case ParseCFGFile(folder, SecondPCP) of
              csNoFile: begin
				  Result := False;
                  MsgBox(Format(CustomMessage('FolderForSecondNoFile'), [#13#10]), mbConfirmation, MB_OK);
                  exit;
                end;
              csUnreadable: begin
				  Result := False;
                  MsgBox(Format(CustomMessage('FolderForSecondBadFile'), [#13#10]), mbConfirmation, MB_OK);
                  exit;
                end;
              csParsedOk: begin
// TODO ask, depending on uninstaller availability
			      if (UninstallState = UIOtherNeeded) or (UninstallState = uiInconsistent)
                  then begin
                    MsgBox(Format(CustomMessage('FolderForSecondBadUninstall'), [#13#10]), mbConfirmation, MB_OK);
				    Result := False;
                    exit;
                  end
                  else
			      if ((UninstallState = uiDone) or (UninstallState = UIOtherNeeded)) or
                     (UninstallState = uiInconsistent)
                  then begin
                    Result := MsgBox(Format(CustomMessage('FolderForSecondUpgrading'), [#13#10, SecondPCP]), mbConfirmation, MB_YESNO) = IDYES;
                    IsSecondaryUpdate := True;
                  end;
                end;
            end;

          // MUST always be loaded, if leaving this page
          LoadCFGFile(folder, NewCFGFile);
		end

        else
		begin
			if ((UninstallState = uiDone) or (UninstallState = UIOtherNeeded)) or
               (UninstallState = uiInconsistent)
            then
			begin
				// Dir NOT empty
				Result := MsgBox(SaveCustomMessage('FolderNotEmpty', 'The target folder is not empty. Continue with installation?'),
                                 mbConfirmation, MB_YESNO) = IDYES;
			end;
            if Result and
               (ParseCFGFile(folder, SecondPCP) = csParsedOk)
            then begin
              IsSecondaryUpdate := True;
              LoadCFGFile(folder, NewCFGFile);
            end;
		end;
	end;

    if CurPage = wpAskConfDir.ID then begin
      s := wpAskConfDir.Values[0];
      if (not IsDirEmpty(s)) then begin
        MsgBox(Format(CustomMessage('FolderForConfNotEmpty'), [#13#10]), mbConfirmation, MB_OK);
		Result := False;
        exit;
      end;

      SecondPCP := s;
      CreateCFGFile(SecondPCP, NewCFGFile);
    end;

    if CurPage = wpInfoAfter then begin
      if (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked) then begin
        if (NewCFGFile = nil) or (not FileExists(AddBackslash(WizardDirValue) + 'lazarus.cfg')) then
          MsgBox('Something went wrong. The secondary config folder was not setup. Repeat the installation.', mbConfirmation, MB_OK);
   end;
    end;
end;

function ShouldSkipPage(PageId: Integer): Boolean;
begin
  Result := False
  if PageId = wpAskConfDir.ID then
    Result := (IsSecondaryUpdate) or
              ( (CheckSecondInstall = nil) or (not CheckSecondInstall.Checked) );

  // UnInst uses: SkipAskUninst()
end;

function UpdateReadyMemo(Space, NewLine,
  MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo,
  MemoTasksInfo: String): String;
begin
  Result := '';
  if MemoUserInfoInfo <> '' then
    Result := Result + MemoUserInfoInfo + NewLine;
  if MemoDirInfo <> '' then
    Result := Result + MemoDirInfo + NewLine;
  if MemoTypeInfo <> '' then
    Result := Result + MemoTypeInfo + NewLine;
  if MemoComponentsInfo <> '' then
    Result := Result + MemoComponentsInfo + NewLine;
  if MemoGroupInfo <> '' then
    Result := Result + MemoGroupInfo + NewLine;
  if MemoTasksInfo <> '' then
    Result := Result + MemoTasksInfo + NewLine;
  if (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked) then begin
    if IsSecondaryUpdate then
      Result := Result + Format(SaveCustomMessage('SecondTaskUpdate', ''), [NewLine, Space, SecondPCP])
    else
      Result := Result + Format(SaveCustomMessage('SecondTaskCreate', ''), [NewLine, Space, SecondPCP]);
  end
  else
  if IsSecondaryUpdate then
    Result := Result + Format(SaveCustomMessage('SecondTaskUpdate', ''), [NewLine, Space, SecondPCP]);
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  Result := '';
  if (CheckSecondInstall <> nil) and (CheckSecondInstall.Checked) then begin
    if (NewCFGFile <> nil) then
      try
        NewCFGFile.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
      except
        Result := 'Internal Error (1): Could not save CFG'
      end
    else begin
      Result := 'Internal Error (2)'
    end;
  end
  else
  if (UninstallDoneState <> uiUnknown) and (IsSecondaryUpdate) and
     (not FileExists(AddBackslash(WizardDirValue) + 'lazarus.cfg'))
  then begin
    // cfg was uninstalled / restore
    if (NewCFGFile <> nil) then
      try
        NewCFGFile.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
      except
        Result := 'Internal Error (3): Could not restore CFG for secondary install'
      end
    else
    if (UninstDir = WizardDirValue) and (CFGFileForUninstDir <> nil) and
       (CFGFileForUninstDir.count > 0)
    then begin
      try
        CFGFileForUninstDir.SaveToFile(AddBackslash(WizardDirValue) + 'lazarus.cfg')
      except
        Result := 'Internal Error (4): Could not restore CFG for secondary install'
      end
    end
    else begin
      Result := 'Internal Error (5): Could not restore CFG for secondary install'
    end;
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
  FileName, Content: string;
  s: Ansistring;
begin
  FileName := ExpandConstant(CurrentFileName);
  LoadStringFromFile(FileName, s);
  Content := s;
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
var
  y: integer;
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
  y := wpLabel4.Top + wpLabel4.Height + ScaleY(20);
  if y > wpAskUnistall.SurfaceHeight - wpCheckBox.Height - wpButton.Height then
    y := wpAskUnistall.SurfaceHeight - wpCheckBox.Height - wpButton.Height;
  wpButton.Top := y;
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
  
  // OldName, Registry 'DisplayName'
  // OldPath,  Registry 'Inno Setup: App Path'
  // UnInstaller

  case UninstallState of
    uiDestNeeded: begin
	    wpLabel2.Font.Color := clDefault;
        wpCheckBox.Visible := False;
        if IsSecondaryUpdate then
          InitAskUninstall(Format(SaveCustomMessage('OldSecondInDestFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldSecondInDestFolder2', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldSecondInDestFolder3', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldSecondInDestFolder4', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]))
        else
          InitAskUninstall(Format(SaveCustomMessage('OldInDestFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldInDestFolder2', ''), {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInDestFolder3', ''), {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInDestFolder4', ''), {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
      end;
    UIOtherNeeded: begin
	    wpLabel2.Font.Color := clRed;
	    wpLabel2.Font.Color := clRed;
        wpCheckBox.Visible := True;
        //if IsSecondaryUpdate then
        //  InitAskUninstall(Format(SaveCustomMessage('InOtherFolder1', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	       //                Format(SaveCustomMessage('OldSecondInOtherFolder2', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInOtherFolder3', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInOtherFolder4', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //else
          InitAskUninstall(Format(SaveCustomMessage('OldInOtherFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldInOtherFolder2', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInOtherFolder3', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInOtherFolder4', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //end;
      end;
    uiInconsistent: begin
	    wpLabel1.Font.Color := clRed;
	    wpLabel2.Font.Color := clRed;
        wpCheckBox.Visible := True;
        //if IsSecondaryUpdate then
        //  InitAskUninstall(Format(SaveCustomMessage('OldSecondInBadFolder1', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	       //                Format(SaveCustomMessage('OldSecondInBadFolder2', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInBadFolder3', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
        //                   Format(SaveCustomMessage('OldSecondInBadFolder4', ''),
        //                                        {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //else
          InitAskUninstall(Format(SaveCustomMessage('OldInBadFolder1', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
	                       Format(SaveCustomMessage('OldInBadFolder2', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInBadFolder3', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]),
                           Format(SaveCustomMessage('OldInBadFolder4', ''),
                                                {}[#13#10, OldName, OldPath, UnInstaller, SecondPCP]));
        //end;
      end;
  end;

end;
  
procedure UnInstBtnClick(Sender: TObject);
var
  s, UnInstaller: String;
  b, FolderEmpty : Boolean;
  i: integer;
begin
  UninstallDoneState := UninstallState;
  UninstallState := uiDone;
  
  UnInstaller := RemoveQuotes(GetUninstallData('UninstallString'));
  
  b := (UnInstaller <> '') and FileExists(UnInstaller);
  if b then begin
    LoadCFGFile(WizardDirValue, CFGFileForUninstDir);
    UninstDir := WizardDirValue;

    if UninstallState = uiInconsistent then
      b := Exec(UnInstaller, '/VERBOSE /NORESTART','', SW_SHOW, ewWaitUntilTerminated, i)
    else
      b := Exec(UnInstaller, '/SILENT /NORESTART','', SW_SHOW, ewWaitUntilTerminated, i);
  end;
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
	    try 
		  s := CustomMessage('FolderNotEmpty2');
	    except
		  s := 'The target folder is not empty.';
        end;
        MsgBox(s, mbConfirmation, MB_OK);
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
var
  s, s2 : String;
begin

  try 
    s := CustomMessage('AskUninstallTitle1');
    s2 := CustomMessage('AskUninstallTitle2');
  except
    s := 'Previous Installation';
	s2 := 'Do you want to run the uninstaller?';
  end;
  wpAskUnistall := CreateCustomPage(wpSelectDir, s, s2);
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
  
  try 
    s := CustomMessage('BtnUninstall');
  except
    s := 'Uninstall';
  end;
  wpButton := TNewButton.Create(wpAskUnistall);
  wpButton.Parent := wpAskUnistall.Surface;
  wpButton.Width := ScaleX(80);
  wpButton.Left := (wpAskUnistall.SurfaceWidth div 2) - ScaleX(40);
  wpButton.Caption := s;
  wpButton.OnClick := @UnInstBtnClick;

  try 
    s := CustomMessage('ChkContinue');
  except
    s := 'Continue without uninstall';
  end;
  wpCheckBox := TNewCheckBox.Create(wpAskUnistall);
  wpCheckBox.Parent := wpAskUnistall.Surface;
  wpCheckBox.Top := wpAskUnistall.SurfaceHeight - wpCheckBox.Height - 1;
  wpCheckBox.Width := wpAskUnistall.SurfaceWidth;  
  wpCheckBox.Caption := s;
  wpCheckBox.OnClick := @UnInstCheckboxClick;
  
  UninstallState := uiUnknown;
  UninstallDoneState := uiUnknown;
 
  /////////////////////////
  wpAskConfDir := CreateInputDirPage(wpSelectDir,
  CustomMessage('SecondConfCapt'), CustomMessage('SecondConfCapt2'),
  CustomMessage('SecondConfBody'),
  False, 'laz_conf');
  wpAskConfDir.Add('Folder for config');
  
end;

//function InitializeUninstall(): Boolean;
//var i: integer;
//begin
//  Result := True;
//  for i := 0 to ParamCount - 1 do
//    if ParamStr(i) = '/VERBOSE' then
//      Result :=
//        MsgBox(Format(CustomMessage('UninstVerbose'),
//                      {}[RemoveQuotes(GetUninstallData('Inno Setup: App Path')),
//                       GetUninstallData('DisplayName')]),
//               mbConfirmation, MB_YESNO) = IDYES;
//
//end;



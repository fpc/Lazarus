[_ISTool]
EnableISX=true
[Defines]
#define AppVersion GetEnv('LazVersion')
#define FPCVersion GetEnv('FPCVersion')
#define FPCTargetOS GetEnv('FPCTargetOS')
#define FPCFullTarget GetEnv('FPCFullTarget')
#define AppName "Lazarus"
#define SetupDate GetEnv('DateStamp')
#define BuildDir GetEnv('BuildDir')
#define QtInfDir GetEnv('QTINFDIR')
#define IDEWidgetSet GetEnv('IDE_WidgetSet')
#define OutputFileName GetEnv('OutputFileName')
[Setup]
AllowNoIcons=yes
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisherURL=http://www.lazarus.freepascal.org/
AppSupportURL=http://www.lazarus.freepascal.org/
AppUpdatesURL=http://www.lazarus.freepascal.org/
ArchitecturesInstallIn64BitMode=x64
DefaultDirName={code:GetDefDir|c:\lazarus}
DefaultGroupName={#AppName}
OutputBaseFilename={#OutputFileName}
InternalCompressLevel=ultra
;InternalCompressLevel=ultra64
;Compression=lzma2/ultra64
SolidCompression=true
VersionInfoVersion={#AppVersion}
VersionInfoTextVersion={#AppVersion}-{#SetupDate}
ShowLanguageDialog=yes
WizardImageFile=lazarus_install_cheetah.bmp
WizardSmallImageFile=lazarus_install_cheetah_small.bmp
WizardImageStretch=false
ShowTasksTreeLines=true
TimeStampRounding=0
PrivilegesRequired=none
ChangesAssociations=true

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Components]
#if FPCTargetOS=="win32"
#if IDEWidgetSet!="qt"
Name: installqtintfdll; Description: Install QT interface dll; Types: custom full compact
#endif
#endif
Name: associatelfm; Description: {code:GetAssociateDesc|.lfm}; Types: custom full
Name: associatelpi; Description: {code:GetAssociateDesc|.lpi}; Types: custom full
Name: associatelpk; Description: {code:GetAssociateDesc|.lpk}; Types: custom full
Name: associatelpr; Description: {code:GetAssociateDesc|.lpr}; Types: custom full
Name: associateinc; Description: {code:GetAssociateDesc|.inc}; Types: custom full
Name: associatepas; Description: {code:GetAssociateDesc|.pas}; Types: custom full
Name: associatepp; Description: {code:GetAssociateDesc|.pp}; Types: custom full

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

[INI]
Filename: {app}\Lazarus Home Page.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/
Filename: {app}\Lazarus Forums.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/index.php?action=forum
Filename: {app}\Lazarus Wiki Help.url; Section: InternetShortcut; Key: URL; String: http://wiki.lazarus.freepascal.org/

[Icons]
Name: {group}\{#AppName}; Filename: {app}\lazarus.exe; IconFilename: {app}\images\mainicon.ico; Comment: "Open Source IDE for Free Pascal"
Name: {group}\{cm:ProgramOnTheWeb,Lazarus}; Filename: {app}\Lazarus Home Page.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Forums; Filename: {app}\Lazarus Forums.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Wiki Help; Filename: {app}\Lazarus Wiki Help.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\{cm:UninstallProgram,Lazarus}; Filename: {uninstallexe}
Name: {userdesktop}\Lazarus; Filename: {app}\lazarus.exe; Tasks: desktopicon; IconFilename: {app}\images\mainicon.ico; Comment: "Open Source IDE for Free Pascal"
Name: {group}\{#AppName} (debug); Filename: {app}\startlazarus.exe; Parameters: --debug; WorkingDir: {app}; IconFilename: {app}\images\mainicon.ico; Comment: "Lazarus --debug"

[Run]
Filename: {app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\fpcmkcfg.exe; Parameters: "-d ""basepath={app}\fpc\{#FPCVersion}"" -o fpc.cfg"; Flags: runhidden; Tasks: ; Languages: 

[UninstallDelete]
Name: {app}\compilertest.pas; Type: files
Name: {app}\Lazarus Wiki Help.url; Type: files
Name: {app}\Lazarus Home Page.url; Type: files
Name: {app}\Lazarus Forums.url; Type: files
Name: {app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\fpc.cfg; Type: files

[Registry]
; HKLM
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lfm"; ValueType: String; ValueData: "Lazarus Form"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lfm\DefaultIcon"; ValueType: String; ValueData: "{app}\images\LazarusForm.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lfm\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\LazarusForm.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lfm\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpi"; ValueType: String; ValueData: "Lazarus Project Information"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpi\DefaultIcon"; ValueType: String; ValueData: "{app}\images\LazarusProject.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpi\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\LazarusProject.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpi\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpk"; ValueType: String; ValueData: "Lazarus Package File"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpk\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lazaruspackage.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpk\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lazaruspackage.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpk\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpr"; ValueType: String; ValueData: "Lazarus Project Main Source"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpr\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpr\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.lpr\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.inc"; ValueType: String; ValueData: "Object Pascal Include File"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.inc\DefaultIcon"; ValueType: String; ValueData: "{app}\images\includefile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.inc\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\includefile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.inc\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pas"; ValueType: String; ValueData: "Pascal Source Code"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pas\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pas\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pas\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pp"; ValueType: String; ValueData: "Pascal Source Code"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pp\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pp\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Classes\Lazarus.AssocFile.pp\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\Classes\.lfm"; ValueType: String; ValueData: "Lazarus.AssocFile.lfm"; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelfm
Root: HKLM; Subkey: "Software\Classes\.lpi"; ValueType: String; ValueData: "Lazarus.AssocFile.lpi"; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpi
Root: HKLM; Subkey: "Software\Classes\.lpk"; ValueType: String; ValueData: "Lazarus.AssocFile.lpk"; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpk
Root: HKLM; Subkey: "Software\Classes\.lpr"; ValueType: String; ValueData: "Lazarus.AssocFile.lpr"; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpr
Root: HKLM; Subkey: "Software\Classes\.inc"; ValueType: String; ValueData: "Lazarus.AssocFile.inc"; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: associateinc
Root: HKLM; Subkey: "Software\Classes\.pas"; ValueType: String; ValueData: "Lazarus.AssocFile.pas"; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: associatepas
Root: HKLM; Subkey: "Software\Classes\.pp"; ValueType: String; ValueData: "Lazarus.AssocFile.pp"; Flags: uninsdeletevalue; Check: IsHKLMWriteable; Components: associatepp

Root: HKLM; Subkey: "Software\Lazarus\Capabilities"; ValueType: String; ValueName: "ApplicationName"; ValueData: "Lazarus IDE"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities"; ValueType: String; ValueName: "ApplicationDescription"; ValueData: "Open Source IDE for Free Pascal."; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities\FileAssociations"; ValueName: ".lfm"; ValueType: String; ValueData: "Lazarus.AssocFile.lfm"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities\FileAssociations"; ValueName: ".lpi"; ValueType: String; ValueData: "Lazarus.AssocFile.lpi"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities\FileAssociations"; ValueName: ".lpk"; ValueType: String; ValueData: "Lazarus.AssocFile.lpk"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities\FileAssociations"; ValueName: ".lpr"; ValueType: String; ValueData: "Lazarus.AssocFile.lpr"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities\FileAssociations"; ValueName: ".inc"; ValueType: String; ValueData: "Lazarus.AssocFile.inc"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities\FileAssociations"; ValueName: ".pas"; ValueType: String; ValueData: "Lazarus.AssocFile.pas"; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; Subkey: "Software\Lazarus\Capabilities\FileAssociations"; ValueName: ".pp"; ValueType: String; ValueData: "Lazarus.AssocFile.pp"; Flags: uninsdeletekey; Check: IsHKLMWriteable

Root: HKLM; Subkey: "Software\RegisteredApplications"; ValueType: String; ValueName: "Lazarus"; ValueData: "Software\Lazarus\Capabilities"; Flags: uninsdeletevalue; Check: IsHKLMWriteable

; HKCU
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lfm"; ValueType: String; ValueData: "Lazarus Form"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lfm\DefaultIcon"; ValueType: String; ValueData: "{app}\images\LazarusForm.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lfm\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\LazarusForm.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lfm\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpi"; ValueType: String; ValueData: "Lazarus Project Information"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpi\DefaultIcon"; ValueType: String; ValueData: "{app}\images\LazarusProject.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpi\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\LazarusProject.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpi\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpk"; ValueType: String; ValueData: "Lazarus Package File"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpk\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lazaruspackage.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpk\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lazaruspackage.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpk\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpr"; ValueType: String; ValueData: "Lazarus Project Main Source"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpr\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpr\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.lpr\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.inc"; ValueType: String; ValueData: "Object Pascal Include File"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.inc\DefaultIcon"; ValueType: String; ValueData: "{app}\images\includefile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.inc\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\includefile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.inc\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pas"; ValueType: String; ValueData: "Pascal Source Code"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pas\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pas\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pas\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pp"; ValueType: String; ValueData: "Pascal Source Code"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pp\DefaultIcon"; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pp\Shell\Open"; ValueName: Icon; ValueType: String; ValueData: "{app}\images\lprfile.ico"; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; Subkey: "Software\Classes\Lazarus.AssocFile.pp\Shell\Open\Command"; ValueType: String; ValueData: """{app}\lazarus.exe"" ""%1"""; Flags: uninsdeletekey; Check: IsHKLMNotWriteable

Root: HKCU; Subkey: "Software\Classes\.lfm"; ValueType: String; ValueData: "Lazarus.AssocFile.lfm"; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelfm
Root: HKCU; Subkey: "Software\Classes\.lpi"; ValueType: String; ValueData: "Lazarus.AssocFile.lpi"; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpi
Root: HKCU; Subkey: "Software\Classes\.lpk"; ValueType: String; ValueData: "Lazarus.AssocFile.lpk"; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpk
Root: HKCU; Subkey: "Software\Classes\.lpr"; ValueType: String; ValueData: "Lazarus.AssocFile.lpr"; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpr
Root: HKCU; Subkey: "Software\Classes\.inc"; ValueType: String; ValueData: "Lazarus.AssocFile.inc"; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associateinc
Root: HKCU; Subkey: "Software\Classes\.pas"; ValueType: String; ValueData: "Lazarus.AssocFile.pas"; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatepas
Root: HKCU; Subkey: "Software\Classes\.pp"; ValueType: String; ValueData: "Lazarus.AssocFile.pp"; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatepp

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
Name: sk; MessagesFile: compiler:Languages\Slovak.isl
Name: sl; MessagesFile: compiler:Languages\Slovenian.isl

[Code]
function NextButtonClick(CurPage: Integer): Boolean;
var
	folder: String;
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
    end

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

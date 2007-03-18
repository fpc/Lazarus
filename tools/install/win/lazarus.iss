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
[Setup]
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisherURL=http://www.lazarus.freepascal.org/
AppSupportURL=http://www.lazarus.freepascal.org/
AppUpdatesURL=http://www.lazarus.freepascal.org/
ArchitecturesInstallIn64BitMode=x64
DefaultDirName={code:GetDefDir|c:\lazarus}
DefaultGroupName={#AppName}
OutputBaseFilename={#AppName}-{#AppVersion}-fpc-{#FPCVersion}-{#SetupDate}-{#FPCTargetOS}
InternalCompressLevel=ultra
SolidCompression=true
VersionInfoVersion={#AppVersion}
VersionInfoTextVersion={#AppVersion}-{#SetupDate}
ShowLanguageDialog=yes
WizardImageFile=lazarus_install_cheetah.bmp
WizardSmallImageFile=lazarus_install_cheetah_small.bmp
WizardImageStretch=false
ShowTasksTreeLines=true

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Components]
Name: associatelfm; Description: {code:GetAssociateDesc|.lfm}; Types: custom full
Name: associatelpi; Description: {code:GetAssociateDesc|.lpi}; Types: custom full
Name: associatelpk; Description: {code:GetAssociateDesc|.lpk}; Types: custom full
Name: associatelpr; Description: {code:GetAssociateDesc|.lpr}; Types: custom full
Name: associateinc; Description: {code:GetAssociateDesc|.inc}; Types: custom full
Name: associatepas; Description: {code:GetAssociateDesc|.pas}; Types: custom full
Name: associatepp; Description: {code:GetAssociateDesc|.pp}; Types: custom full

[Files]
Source: {#BuildDir}\*.*; DestDir: {app}; Flags: recursesubdirs
Source: environmentoptions-{#FPCTargetOS}.xml; DestDir: {app}; Flags: onlyifdoesntexist; AfterInstall: UpdateEnvironmentOptions; DestName: environmentoptions.xml
Source: editoroptions.xml; DestDir: {app}; Flags: onlyifdoesntexist

[INI]
Filename: {app}\Lazarus Home Page.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/
Filename: {app}\Lazarus Forums.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/modules.php?op=modload&name=PNphpBB2&file=index
Filename: {app}\Lazarus Wiki Help.url; Section: InternetShortcut; Key: URL; String: http://wiki.lazarus.freepascal.org/index.php/Main_Page

[Icons]
Name: {group}\{#AppName}; Filename: {app}\lazarus.exe
Name: {group}\{cm:ProgramOnTheWeb,Lazarus}; Filename: {app}\Lazarus Home Page.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Forums; Filename: {app}\Lazarus Forums.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Wiki Help; Filename: {app}\Lazarus Wiki Help.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\{cm:UninstallProgram,Lazarus}; Filename: {uninstallexe}
Name: {userdesktop}\Lazarus; Filename: {app}\lazarus.exe; Tasks: desktopicon
Name: {group}\{#AppName} (debug); Filename: {app}\startlazarus.exe; Parameters: --debug; WorkingDir: {app}

[Run]
Filename: {app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\fpcmkcfg.exe; Parameters: "-d ""basepath={app}\fpc\{#FPCVersion}"" -o fpc.cfg"; Flags: runhidden; Tasks: ; Languages: 

[UninstallDelete]
Name: {app}\compilertest.pas; Type: files
Name: {app}\Lazarus Wiki Help.url; Type: files
Name: {app}\Lazarus Home Page.url; Type: files
Name: {app}\Lazarus Forums.url; Type: files
Name: {app}\fpc\{#FPCVersion}\bin\{#FPCFullTarget}\fpc.cfg; Type: files

[Registry]
Root: HKLM; SubKey: SOFTWARE\Classes\.lpi; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpi
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject; ValueType: string; ValueName: ; ValueData: Lazarus Project; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject\DefaultIcon; ValueType: string; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.lpk; ValueType: string; ValueData: LazarusPackage; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpk
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage; ValueType: string; ValueName: ; ValueData: Lazarus Package; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage\DefaultIcon; ValueType: string; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenpackage}; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.lpr; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpr
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource; ValueType: string; ValueName: ; ValueData: Lazarus Project; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource\DefaultIcon; ValueType: string; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.lfm; ValueType: string; ValueData: LazarusForm; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelfm
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusForm; ValueType: string; ValueName: ; ValueData: Lazarus Form; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusForm\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusForm\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wiseditform}; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusForm\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.pas; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatepas
Root: HKLM; SubKey: SOFTWARE\Classes\.pp; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatepp
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusUnit; ValueType: string; ValueName: ; ValueData: Object Pascal Unit; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusUnit\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusSource.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusUnit\shell\open; ValueType: string; ValueData: {code:GetPoString|installerstrconsts:wiseditsource}; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusUnit\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.inc; ValueType: string; ValueData: LazarusInclude; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associateinc
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude; ValueType: string; ValueName: ; ValueData: Object Pascal Include; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude\DefaultIcon; ValueType: string; ValueData: {app}\images\includefile.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wiseditsource}; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable

Root: HKCU; SubKey: SOFTWARE\Classes\.lpi; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpi
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject; ValueType: string; ValueName: ; ValueData: Lazarus Project; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject\DefaultIcon; ValueType: string; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.lpk; ValueType: string; ValueData: LazarusPackage; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpk
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage; ValueType: string; ValueName: ; ValueData: Lazarus Package; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage\DefaultIcon; ValueType: string; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenpackage}; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.lpr; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpr
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource; ValueType: string; ValueName: ; ValueData: Lazarus Project; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource\DefaultIcon; ValueType: string; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.lfm; ValueType: string; ValueData: LazarusForm; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelfm
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm; ValueType: string; ValueName: ; ValueData: Lazarus Form; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.pas; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatepas
Root: HKCU; SubKey: SOFTWARE\Classes\.pp; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatepp
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit; ValueType: string; ValueName: ; ValueData: Object Pascal Unit; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusSource.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wiseditsource}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.inc; ValueType: string; ValueData: LazarusInclude; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associateinc
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude; ValueType: string; ValueName: ; ValueData: Object Pascal Include; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude\DefaultIcon; ValueType: string; ValueData: {app}\images\includefile.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wiseditsource}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable

[Languages]
Name: default; MessagesFile: compiler:Default.isl
Name: ca; MessagesFile: compiler:Languages\Catalan.isl
Name: cs; MessagesFile: compiler:Languages\Czech.isl
Name: de; MessagesFile: compiler:Languages\German.isl
Name: fi; MessagesFile: Finnish-5.1.0.isl
Name: fr; MessagesFile: compiler:Languages\French.isl
Name: nl; MessagesFile: compiler:Languages\Dutch.isl
Name: no; MessagesFile: compiler:Languages\Norwegian.isl
Name: pl; MessagesFile: compiler:Languages\Polish.isl
Name: pt; MessagesFile: compiler:Languages\Portuguese.isl
Name: pt_BR; MessagesFile: compiler:Languages\BrazilianPortuguese.isl
Name: ru; MessagesFile: compiler:Languages\Russian.isl
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
  StringChange(Content, '%FpcSrcDir%', ExpandConstant('{app}\fpc\{#FPCVersion}\source'));
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

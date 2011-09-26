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
OutputBaseFilename={#OutputFileName}
InternalCompressLevel=ultra
SolidCompression=true
VersionInfoVersion={#FileVersion}
VersionInfoTextVersion={#AppVersion}-{#SetupDate}
ShowLanguageDialog=yes
WizardImageFile=lazarus_install_cheetah.bmp
WizardSmallImageFile=lazarus_install_cheetah_small.bmp
WizardImageStretch=false
ShowTasksTreeLines=true
TimeStampRounding=0
PrivilegesRequired=none

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
Filename: {app}\Lazarus Forums.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/modules.php?op=modload&name=PNphpBB2&file=index
Filename: {app}\Lazarus Wiki Help.url; Section: InternetShortcut; Key: URL; String: http://wiki.lazarus.freepascal.org/index.php/Main_Page

[Icons]
Name: {group}\{#AppName}; Filename: {app}\lazarus.exe; IconFilename: {app}\images\mainicon.ico
Name: {group}\{cm:ProgramOnTheWeb,Lazarus}; Filename: {app}\Lazarus Home Page.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Forums; Filename: {app}\Lazarus Forums.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\Lazarus Wiki Help; Filename: {app}\Lazarus Wiki Help.url; IconFilename: {app}\images\LazarusProject.ico
Name: {group}\{cm:UninstallProgram,Lazarus}; Filename: {uninstallexe}
Name: {userdesktop}\Lazarus; Filename: {app}\lazarus.exe; Tasks: desktopicon; IconFilename: {app}\images\mainicon.ico
Name: {group}\{#AppName} (debug); Filename: {app}\startlazarus.exe; Parameters: --debug; WorkingDir: {app}; IconFilename: {app}\images\mainicon.ico

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
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject; ValueType: string; ValueName: ; ValueData: Lazarus project information; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject\DefaultIcon; ValueType: string; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProject\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.lpk; ValueType: string; ValueData: LazarusPackage; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpk
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage; ValueType: string; ValueName: ; ValueData: Lazarus package file; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage\DefaultIcon; ValueType: string; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenpackage}; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.lpr; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelpr
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource; ValueType: string; ValueName: ; ValueData: Lazarus project main source; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource\DefaultIcon; ValueType: string; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\.lfm; ValueType: string; ValueData: LazarusForm; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMWriteable; Components: associatelfm
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusForm; ValueType: string; ValueName: ; ValueData: Lazarus form; Flags: uninsdeletekey; Check: IsHKLMWriteable
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
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude; ValueType: string; ValueName: ; ValueData: Object Pascal include file; Flags: uninsdeletekey; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude\DefaultIcon; ValueType: string; ValueData: {app}\images\includefile.ico; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wiseditsource}; Flags: uninsdeletevalue; Check: IsHKLMWriteable
Root: HKLM; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMWriteable

Root: HKCU; SubKey: SOFTWARE\Classes\.lpi; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpi
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject; ValueType: string; ValueName: ; ValueData: Lazarus project information; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject\DefaultIcon; ValueType: string; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProject\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.lpk; ValueType: string; ValueData: LazarusPackage; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpk
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage; ValueType: string; ValueName: ; ValueData: Lazarus package; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage\DefaultIcon; ValueType: string; ValueData: {app}\images\lazaruspackage.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenpackage}; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusPackage\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.lpr; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelpr
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource; ValueType: string; ValueName: ; ValueData: Lazarus project main source; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource\DefaultIcon; ValueType: string; ValueData: {app}\images\lprfile.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusProjectSource\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.lfm; ValueType: string; ValueData: LazarusForm; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatelfm
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm; ValueType: string; ValueName: ; ValueData: Lazarus form; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wisopenproject}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusForm\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.pas; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatepas
Root: HKCU; SubKey: SOFTWARE\Classes\.pp; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associatepp
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit; ValueType: string; ValueName: ; ValueData: Object Pascal unit; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusSource.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wiseditsource}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusUnit\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\.inc; ValueType: string; ValueData: LazarusInclude; Flags: uninsdeletekeyifempty uninsdeletevalue; Check: IsHKLMNotWriteable; Components: associateinc
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude; ValueType: string; ValueName: ; ValueData: Object Pascal include file; Flags: uninsdeletekey; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude\DefaultIcon; ValueType: string; ValueData: {app}\images\includefile.ico; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open; ValueType: string; ValueName: ; ValueData: {code:GetPoString|installerstrconsts:wiseditsource}; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable
Root: HKCU; SubKey: SOFTWARE\Classes\LazarusInclude\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue; Check: IsHKLMNotWriteable

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

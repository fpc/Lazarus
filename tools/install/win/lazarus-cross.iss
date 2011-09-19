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
#define LazRevision GetEnv('LazRevision')
#define FPCVersion GetEnv('FPCVersion')
#define FPCFullVersion GetEnv('FPCFullVersion')
#define FPCSourceOS GetEnv('FPCSourceOS')
#define FPCFullSource GetEnv('FPCFullSource')
#define FPCFullTarget GetEnv('FPCFullTarget')
#define AppName "Lazarus"
#define SetupDate GetEnv('DateStamp')
#define BuildDir GetEnv('BuildDir')
#define OutputFileName GetEnv('OutputFileName')
[Setup]
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisherURL=http://www.lazarus.freepascal.org/
AppSupportURL=http://www.lazarus.freepascal.org/
AppUpdatesURL=http://www.lazarus.freepascal.org/
ArchitecturesInstallIn64BitMode=x64
DefaultDirName={code:GetDefDir|{sd}\lazarus}
DefaultGroupName={#AppName}
InternalCompressLevel=ultra
OutputBaseFilename={#OutputFileName}
SolidCompression=true
VersionInfoVersion={#FileVersion}
VersionInfoTextVersion={#AppVersion}-{#SetupDate}
ShowLanguageDialog=yes
WizardImageFile=lazarus_install_cheetah.bmp
WizardSmallImageFile=lazarus_install_cheetah_small.bmp
WizardImageStretch=false
ShowTasksTreeLines=true
WindowVisible=true
PrivilegesRequired=none

[Files]
Source: {#BuildDir}\image\*.*; DestDir: {app}; Flags: recursesubdirs

[INI]
Filename: {app}\Lazarus Home Page.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/
Filename: {app}\Lazarus Forums.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/modules.php?op=modload&name=PNphpBB2&file=index
Filename: {app}\Lazarus Wiki Help.url; Section: InternetShortcut; Key: URL; String: http://wiki.lazarus.freepascal.org/index.php/Main_Page

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

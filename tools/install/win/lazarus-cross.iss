[_ISTool]
EnableISX=true
[Defines]
#define AppVersion GetEnv('LazVersion')
// LazVersion may be 0.9.30.2RC1
// A valid file version contains only digits, so drop the RC part
#if pos('RC',AppVersion)>0
  #define FileVersion = copy(AppVersion, 1, pos('RC', AppVersion)-1)
#else 
  #if pos('pre',AppVersion)>0
    #define FileVersion = copy(AppVersion, 1, pos('pre', AppVersion)-1)
  #else
    #define FileVersion = AppVersion
  #endif
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
#define CrossTargetCPU GetEnv('TARGETCPU')
#define CrossTagetOs GetEnv('TARGETOS')
[Setup]
AppName={#AppName} - Addon for target {#CrossTagetOs}-{#CrossTargetCPU}
UpdateUninstallLogAppName=no
;UninstallDisplayName={#AppName} {#AppVersion}
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
AppendDefaultDirName=no
DirExistsWarning=no
EnableDirDoesntExistWarning=yes
OutputBaseFilename={#OutputFileName}
;InternalCompressLevel=ultra64
Compression=lzma2/ultra64
LZMADictionarySize=131072
LZMAUseSeparateProcess=yes
LZMANumFastBytes=270
SolidCompression=yes
VersionInfoVersion={#FileVersion}
VersionInfoTextVersion={#AppVersion}-{#SetupDate}
ShowLanguageDialog=yes
WizardImageFile=lazarus_install_cheetah.bmp
WizardSmallImageFile=lazgear.bmp
WizardImageStretch=false
ShowTasksTreeLines=true
PrivilegesRequired=none
; since appid can change, UsePreviousLanguage must be off
UsePreviousLanguage=no
DisableDirPage=no

[Files]
Source: {#BuildDir}\image\*.*; DestDir: {app}; Flags: recursesubdirs

[INI]
Filename: {app}\Lazarus Home Page.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/
Filename: {app}\Lazarus Forums.url; Section: InternetShortcut; Key: URL; String: http://www.lazarus.freepascal.org/modules.php?op=modload&name=PNphpBB2&file=index
Filename: {app}\Lazarus Wiki Help.url; Section: InternetShortcut; Key: URL; String: http://wiki.lazarus.freepascal.org/index.php/Main_Page

[Code]

var
  InitDone: String;
  
procedure InitializeWizard();
begin
  InitDone := '1';
end;

function GetAppId(param:string): String;
var
  s: String;
begin
  Result := 'lazarus'; 
  if InitDone = '' then
    exit;
  s := AddBackslash(WizardDirValue) + 'lazarus.cfg';
  if FileExists(s) then
  begin
	// Secondary
    s := RemoveBackslashUnlessRoot(Lowercase(WizardDirValue));
    Result := 'lazarus_sec_'+GetSHA1OfString(s) + '_' + IntToStr(length(s));
  end
  else
    Result := 'lazarus';
end;

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

[_ISTool]
EnableISX=true
[Defines]
#define AppVersion GetEnv('LazVersion')
#define AppName "Lazarus"
#define SetupDate GetEnv('DateStamp')
#define BuildDir GetEnv('BuildDir')
#define LicenseDir GetEnv('LicenseDir')
[Setup]
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisherURL=http://www.lazarus.freepascal.org/
AppSupportURL=http://www.lazarus.freepascal.org/
AppUpdatesURL=http://www.lazarus.freepascal.org/
LicenseFile={#BuildDir}\COPYING.GPL
DefaultDirName={code:GetDefDir|c:\lazarus}
DefaultGroupName={#AppName}
OutputBaseFilename={#AppName}-{#AppVersion}-{#SetupDate}-win32
InternalCompressLevel=ultra
SolidCompression=true
VersionInfoVersion={#AppVersion}
VersionInfoTextVersion={#AppVersion}-{#SetupDate}
ShowLanguageDialog=yes

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: {#BuildDir}\*.*; DestDir: {app}; Flags: recursesubdirs
Source: environmentoptions.xml; DestDir: {app}; Flags: onlyifdoesntexist; AfterInstall: UpdateEnvironmentOptions
Source: editoroptions.xml; DestDir: {app}; Flags: onlyifdoesntexist
Source: samplefpc.cfg; DestDir: {app}\pp\bin\i386-win32; AfterInstall: UpdateFpcCfg; DestName: fpc.cfg

[Icons]
Name: {group}\{#AppName}; Filename: {app}\startlazarus.exe
Name: {group}\{cm:UninstallProgram,Lazarus}; Filename: {uninstallexe}
Name: {userdesktop}\Lazarus; Filename: {app}\startlazarus.exe; Tasks: desktopicon

[UninstallDelete]
Name: {app}\compilertest.pas; Type: files
[Registry]
Root: HKCR; SubKey: .lpi; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue
Root: HKCR; SubKey: LazarusProject; ValueType: string; ValueName: ; ValueData: Lazarus Project; Flags: uninsdeletekey
Root: HKCR; SubKey: LazarusProject\DefaultIcon; ValueType: string; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusProject\shell\open; ValueType: string; ValueName: ; ValueData: &Open Project; Flags: uninsdeletekey
Root: HKCR; SubKey: LazarusProject\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue
Root: HKCR; SubKey: .lpr; ValueType: string; ValueData: LazarusProject; Flags: uninsdeletekeyifempty uninsdeletevalue
Root: HKCR; SubKey: LazarusProjectSource; ValueType: string; ValueName: ; ValueData: Lazarus Project; Flags: uninsdeletekey
Root: HKCR; SubKey: LazarusProjectSource\DefaultIcon; ValueType: string; ValueData: {app}\images\LazarusProject.ico; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusProjectSource\shell\open; ValueType: string; ValueName: ; ValueData: &Open Project; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusProjectSource\shell\open\command; ValueType: string; ValueData: "{app}\lazarus.exe  ""%1"""; Flags: uninsdeletevalue
Root: HKCR; SubKey: .lfm; ValueType: string; ValueData: LazarusForm; Flags: uninsdeletekeyifempty uninsdeletevalue
Root: HKCR; SubKey: LazarusForm; ValueType: string; ValueName: ; ValueData: Lazarus Form; Flags: uninsdeletekey
Root: HKCR; SubKey: LazarusForm\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusForm.ico; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusForm\shell\open; ValueType: string; ValueName: ; ValueData: &Edit Form; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusForm\shell\open\command; ValueType: string; ValueData: "Notepad.exe ""%1"""; Flags: uninsdeletevalue
Root: HKCR; SubKey: .pas; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue
Root: HKCR; SubKey: .pp; ValueType: string; ValueData: LazarusUnit; Flags: uninsdeletekeyifempty uninsdeletevalue
Root: HKCR; SubKey: LazarusUnit; ValueType: string; ValueName: ; ValueData: Object Pascal Unit; Flags: uninsdeletekey
Root: HKCR; SubKey: LazarusUnit\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusSource.ico; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusUnit\shell\open; ValueType: string; ValueName: ; ValueData: &Edit Source; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusUnit\shell\open\command; ValueType: string; ValueData: "Notepad.exe ""%1"""; Flags: uninsdeletevalue
Root: HKCR; SubKey: .inc; ValueType: string; ValueData: LazarusInclude; Flags: uninsdeletekeyifempty uninsdeletevalue
Root: HKCR; SubKey: LazarusInclude; ValueType: string; ValueName: ; ValueData: Object Pascal Include; Flags: uninsdeletekey
Root: HKCR; SubKey: LazarusInclude\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\images\LazarusSource.ico; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusInclude\shell\open; ValueType: string; ValueName: ; ValueData: &Edit Source; Flags: uninsdeletevalue
Root: HKCR; SubKey: LazarusInclude\shell\open\command; ValueType: string; ValueData: "Notepad.exe ""%1"""; Flags: uninsdeletevalue
[Languages]
Name: default; MessagesFile: compiler:Default.isl
Name: ca; MessagesFile: compiler:Languages\Catalan.isl
Name: cs; MessagesFile: compiler:Languages\Czech.isl
Name: de; MessagesFile: compiler:Languages\German.isl
Name: fr; MessagesFile: compiler:Languages\French.isl
Name: nl; MessagesFile: compiler:Languages\Dutch.isl; LicenseFile: {#LicenseDir}\GPL-nl.txt
Name: no; MessagesFile: compiler:Languages\Norwegian.isl
Name: pl; MessagesFile: compiler:Languages\Polish.isl
Name: pt; MessagesFile: compiler:Languages\PortugueseStd.isl; LicenseFile: {#LicenseDir}\GPL-pt.txt
Name: ru; MessagesFile: compiler:Languages\Russian.isl
Name: sl; MessagesFile: compiler:Languages\Slovenian.isl
Name: pt_BR; MessagesFile: C:\lazarus\source\lazarus\tools\install\win32\Portuguese_BR.isl; LicenseFile: {#LicenseDir}\GPL-pt_BR.txt
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
  // if default folder contains spaces, suggest user to install in folderwithnospaces\Myappdir
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
  StringChange(Content, '%FpcSrcDir%', ExpandConstant('{app}\fpcsrc'));
  StringChange(Content, '%FpcBinDir%', ExpandConstant('{app}\pp\bin\i386-win32'));
  SaveStringToFile(FileName, Content, False);
end;

procedure UpdateFpcCfg();
var
  FileName: string;
  Content: string;
begin
  FileName := ExpandConstant(CurrentFileName);
  LoadStringFromFile(FileName, Content);
  StringChange(Content, '$1', ExpandConstant('{app}\pp'));
  SaveStringToFile(FileName, Content, False);
end;

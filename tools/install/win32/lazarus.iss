[_ISTool]
EnableISX=true
[Defines]
#define AppVersion GetEnv('LazVersion')
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
LicenseFile={#BuildDir}\COPYING
DefaultDirName={code:GetDefDir|c:\lazarus}
DefaultGroupName={#AppName}
OutputBaseFilename={#AppName}-{#AppVersion}-{#SetupDate}
InternalCompressLevel=ultra
SolidCompression=true

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: {#BuildDir}\*.*; DestDir: {app}; Flags: recursesubdirs
Source: environmentoptions.xml; DestDir: {app}; Flags: onlyifdoesntexist; AfterInstall: UpdateEnvironmentOptions
Source: editoroptions.xml; DestDir: {app}; Flags: onlyifdoesntexist
Source: samplefpc.cfg; DestDir: {app}\pp\bin\win32; Flags: onlyifdoesntexist; AfterInstall: UpdateFpcCfg; DestName: fpc.cfg

[Icons]
Name: {group}\{#AppName}; Filename: {app}\startlazarus.exe
Name: {group}\{cm:UninstallProgram,Lazarus}; Filename: {uninstallexe}
Name: {userdesktop}\Lazarus; Filename: {app}\startlazarus.exe; Tasks: desktopicon

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
  StringChange(Content, '%FpcBinDir%', ExpandConstant('{app}\pp\bin\win32'));
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

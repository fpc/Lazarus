unit fppkg_initializeoptionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn,
  ButtonPanel,
  fpTemplate,
  // IDEIntf
  IDEOptionsIntf,
  // fppkg
  pkgglobals, pkgFppkg, pkgoptions, pkgUninstalledSrcsRepo;

type

  { TInitializeOptionsForm }

  TInitializeOptionsForm = class(TForm)
    AdvancedCheckbox: TCheckBox;
    LazarusDirValidationLabel: TLabel;
    LazarusButtonPanel: TButtonPanel;
    FPCButtonPanel: TButtonPanel;
    CompilerEdit: TEdit;
    LazarusDirectoryEdit: TDirectoryEdit;
    InitializeLazarusLabel: TLabel;
    Label2: TLabel;
    LazarusConfigPanel: TPanel;
    PrefixLabel: TLabel;
    PathEdit: TEdit;
    FPCDirectoryEdit: TDirectoryEdit;
    FppkgConfigPanel: TPanel;
    InitializeFppkgLabel: TLabel;
    Label1: TLabel;
    FPCDirValidationLabel: TLabel;
    PathLabel: TLabel;
    PrefixEdit: TEdit;
    CompilerLabel: TLabel;
    procedure AdvancedCheckboxChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FPCDirectoryEditChange(Sender: TObject);
    procedure LazarusDirectoryEditChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure OnLazarusButtonClick(Sender: TObject);
  private
    function IsVersionStr(AString: string): Boolean;
    class function IsValidFPCLocation(APath: string; out Message: string): Boolean;
    class function GuessFPCLocationFromCompilerExecutable(AnExecutablePath: string): string;
  public
    class function CheckInitialConfiguration: Boolean;
    class function RecreateFppkgConfiguration: Boolean;
    class function CheckLazarusConfiguration: Boolean;
    class function RecreateLazarusConfiguration: Boolean;
  end;

var
  InitializeOptionsForm: TInitializeOptionsForm;

implementation

{$R *.lfm}

{ TInitializeOptionsForm }

procedure TInitializeOptionsForm.FPCDirectoryEditChange(Sender: TObject);
var
  Prefix, s: RawByteString;
  Message: string;
  Dir: string;
begin
  if IsValidFPCLocation(FPCDirectoryEdit.Text, Message) then
    begin
    Dir := IncludeTrailingPathDelimiter(FPCDirectoryEdit.Text);
    FPCDirValidationLabel.Caption := '';

    s := ExtractFileName(ExcludeTrailingPathDelimiter(Dir));
    Prefix := ExtractFilePath(ExcludeTrailingPathDelimiter(Dir));

    if IsVersionStr(s) then
      PathEdit.Text := Prefix + '{CompilerVersion}' + PathDelim
    else
      PathEdit.Text := Dir;

    Prefix := ExtractFilePath(ExcludeTrailingPathDelimiter(Prefix));
    Prefix := ExtractFilePath(ExcludeTrailingPathDelimiter(Prefix));
    PrefixEdit.Text := Prefix;

    s := ConcatPaths([Prefix, 'bin', 'fpc'+ExeExt]);
    if FileExists(s) then
      CompilerEdit.Text := s
    else
      CompilerEdit.Text := ExeSearch('fpc'+ExeExt,GetEnvironmentVariable('PATH'));
    end
  else
    begin
    FPCDirValidationLabel.Caption := Message;
    end;
  FPCButtonPanel.OKButton.Enabled := FPCDirValidationLabel.Caption = '';
end;

procedure TInitializeOptionsForm.LazarusDirectoryEditChange(Sender: TObject);
var
  Dir: string;
begin
  Dir := IncludeTrailingPathDelimiter(LazarusDirectoryEdit.Text);
  if not DirectoryExists(Dir) then
    LazarusDirValidationLabel.Caption := 'Directory does not exist'
  else if DirectoryExists(Dir+'components') and DirectoryExists(Dir+'lcl') and DirectoryExists(Dir+'packager') then
    begin
    LazarusDirValidationLabel.Caption := '';
    end
  else
    LazarusDirValidationLabel.Caption := 'This location does not seems to contain a valid lazarus-installation';
  LazarusButtonPanel.OKButton.Enabled := LazarusDirValidationLabel.Caption = '';
end;

procedure TInitializeOptionsForm.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TInitializeOptionsForm.AdvancedCheckboxChange(Sender: TObject);
begin
  PathLabel.Enabled := AdvancedCheckbox.Checked;
  PrefixLabel.Enabled := AdvancedCheckbox.Checked;
  PathEdit.Enabled := AdvancedCheckbox.Checked;
  PrefixEdit.Enabled := AdvancedCheckbox.Checked;
end;

procedure TInitializeOptionsForm.OKButtonClick(Sender: TObject);
var
  FPpkg: TpkgFPpkg;
  FileName: string;
  CurrentSection: TFppkgRepositoryOptionSection;
begin
  FPpkg := TpkgFPpkg.Create(Self);
  try
    CurrentSection := FPpkg.Options.AddRepositoryOptionSection(TFppkgRepositoryOptionSection);
    CurrentSection.RepositoryName := 'fpc';
    CurrentSection.Description := 'Packages which are installed along with the Free Pascal Compiler';
    CurrentSection.Path := PathEdit.Text;
    CurrentSection.Prefix := PrefixEdit.Text;

    FPpkg.Options.AddIncludeFilesOptionSection('{LocalRepository}config/conf.d/*.conf');

    CurrentSection := FPpkg.Options.AddRepositoryOptionSection(TFppkgRepositoryOptionSection);
    CurrentSection.RepositoryName := 'user';
    CurrentSection.Description := 'User-installed packages';
    CurrentSection.Path := '{LocalRepository}lib/fpc/{CompilerVersion}/';
    CurrentSection.Prefix := '{LocalRepository}';

    FileName := GetFppkgConfigFile(False, False);
    ForceDirectories(ExtractFilePath(FileName));
    FPpkg.Options.SaveToFile(FileName);

    // Load the just created configuration-file.
    FPpkg.InitializeGlobalOptions(FileName);
    FPpkg.CompilerOptions.Compiler := CompilerEdit.Text;
    // Remove the default configuration-file, so a new one will be generated
    FileName:=FPpkg.Options.GlobalSection.CompilerConfigDir+FPpkg.Options.CommandLineSection.CompilerConfig;
    if FileExists(FileName) then
      RenameFile(FileName, ChangeFileExt(FileName, '.bak'));
    // This will create the compiler-configuration file
    FPpkg.InitializeCompilerOptions;
  finally
    FPpkg.Free;
  end;
end;

procedure TInitializeOptionsForm.OnLazarusButtonClick(Sender: TObject);
var
  Dir: string;
  LazarusConfFilename: RawByteString;
  OptionParser: TTemplateParser;
  LazarusConfFile: TStrings;
  GlobalOpt: TFppkgGlobalOptionSection;

  procedure AddLazarusPackageLocation(ARepoName, ARepoDescription, ALocation: string);
  var
    SourceOptSection: TFppkgUninstalledSourceRepositoryOptionSection;
    OptSection: TFppkgUninstalledRepositoryOptionSection;
  begin
    OptSection := TFppkgUninstalledRepositoryOptionSection.Create(OptionParser);
    SourceOptSection := TFppkgUninstalledSourceRepositoryOptionSection.Create(OptionParser);
    try
      SourceOptSection.RepositoryName := ARepoName + '-src';
      SourceOptSection.Description := ARepoDescription + ' sources';
      SourceOptSection.Path := Dir + ALocation;
      SourceOptSection.InstallRepositoryName := ARepoName;
      SourceOptSection.SaveToStrings(LazarusConfFile);
      LazarusConfFile.Add('');

      OptSection.RepositoryName := ARepoName;
      OptSection.Description := ARepoDescription;
      OptSection.Path := Dir + ALocation;
      OptSection.SourceRepositoryName := ARepoName + '-src';
      OptSection.SaveToStrings(LazarusConfFile);
      LazarusConfFile.Add('');
    finally
      SourceOptSection.Free;
      OptSection.Free;
    end;
  end;

begin
  Dir := IncludeTrailingPathDelimiter(LazarusDirectoryEdit.Text);
  LazarusConfFilename := ConcatPaths([GetUserDir, '.fppkg', 'config', 'conf.d', 'lazarus.conf']);
  LazarusConfFile := TStringList.Create;
  try

    OptionParser := TTemplateParser.Create;
    try
      AddLazarusPackageLocation('laz-comp', 'Lazarus components', 'components');
      AddLazarusPackageLocation('laz-packages', 'Lazarus packager', 'packager');
      AddLazarusPackageLocation('laz-lclbase', 'Lazarus LCL-base', '');
      AddLazarusPackageLocation('laz-lcl', 'Lazarus LCL', 'lcl');

      LazarusConfFile.Add('[Global]');
      LazarusConfFile.Add('FPMakeOptions=--lazarusdir=' + Dir);
    finally
      OptionParser.Free;
    end;

    ForceDirectories(ExtractFileDir(LazarusConfFilename));
    LazarusConfFile.SaveToFile(LazarusConfFilename);
  finally
    LazarusConfFile.Free;
  end;
end;

function TInitializeOptionsForm.IsVersionStr(AString: string): Boolean;
var
  i: Integer;
begin
  Result := length(AString) > 0;
  for i := 1 to length(AString) do
    begin
    // only allow digits or a dot.
    if AString[i] in ['0'..'9'] then
      Continue
    // allow dots, but not as first or last character
    else if (AString[i] = '.') and (i > 1) and (i < length(AString)) then
      begin
      // do not allow two consecutive dots
      if (i > 1) and (AString[i-1]='.') then
        Result := False;
      end
    else
      begin
      Result := False;
      Exit;
      end;
    end;
end;

class function TInitializeOptionsForm.CheckInitialConfiguration: Boolean;
begin
  Result := true;
  if not FileExists(GetFppkgConfigFile(False, False))  and
    not FileExists(GetFppkgConfigFile(True, False)) then
    begin
      Result := RecreateFppkgConfiguration;
    end;
end;

class function TInitializeOptionsForm.RecreateFppkgConfiguration: Boolean;
var
  Frm: TInitializeOptionsForm;
begin
  Frm := TInitializeOptionsForm.Create(nil);
  try
    Frm.FppkgConfigPanel.Visible := True;
    Frm.FPCDirectoryEdit.Text := GuessFPCLocationFromCompilerExecutable(IDEEnvironmentOptions.GetParsedCompilerFilename);
    if Frm.ShowModal in [mrClose, mrCancel] then
      Result := False
    else
      Result := True;
  finally
    Frm.Free;
  end;
end;

class function TInitializeOptionsForm.CheckLazarusConfiguration: Boolean;
begin
  Result := true;
  if not FileExists(ConcatPaths([GetUserDir, '.fppkg', 'config', 'conf.d', 'lazarus.conf'])) then
    begin
      Result := RecreateLazarusConfiguration;
    end;
end;

class function TInitializeOptionsForm.RecreateLazarusConfiguration: Boolean;
var
  Frm: TInitializeOptionsForm;
begin
  Frm := TInitializeOptionsForm.Create(nil);
  try
    Frm.LazarusConfigPanel.Visible := True;
    if Frm.ShowModal in [mrClose, mrCancel] then
      Result := False
    else
      Result := True;
  finally
    Frm.Free;
  end;
end;

class function TInitializeOptionsForm.IsValidFPCLocation(APath: string; out Message: string): Boolean;
var
  Dir: string;
  SR: TRawByteSearchrec;
begin
  Result := False;
  Message := '';
  Dir := IncludeTrailingPathDelimiter(APath);
  if not DirectoryExists(Dir) then
    Message := 'Directory does not exist'
  else
    begin
    if FindFirst(Dir+'ppc*'+ExeExt, faAnyFile-faDirectory, SR) = 0 then
      begin
      FindClose(SR);
      if FileExists(Dir+'units') and FileExists(Dir+'fpmkinst') then
        Result := True
      else
        Message := 'This location does not seems to contain a valid fpc-installation'
      end
    else
      Message := 'Compiler not found at given location'
    end;
end;

class function TInitializeOptionsForm.GuessFPCLocationFromCompilerExecutable(AnExecutablePath: string): string;
var
  Dir: string;
  SR: TRawByteSearchRec;
  Message: string;
begin
  Result := '';
  if IsValidFPCLocation(ExtractFileDir(AnExecutablePath), Message) then
    begin
    Result := ExtractFileDir(AnExecutablePath);
    Exit;
    end;
  if ExtractFileName(ExtractFileNameWithoutExt(AnExecutablePath)) = 'fpc' then
    begin
    Dir := ExtractFileDir(ExtractFileDir(AnExecutablePath));
    Dir := ConcatPaths([Dir, 'lib', 'fpc']);
    if not DirectoryExists(Dir) then
      Dir := ConcatPaths([Dir, 'lib64', 'fpc']);
    if DirectoryExists(Dir) then
      begin
      if FindFirst(IncludeTrailingPathDelimiter(Dir)+AllFiles, faDirectory, SR) = 0 then
        begin
        repeat
        if (SR.Name<>'.') and (SR.Name<>'..') and IsValidFPCLocation(IncludeTrailingPathDelimiter(Dir)+SR.Name, Message) then
          Result := IncludeTrailingPathDelimiter(Dir)+SR.Name;
        until FindNext(Sr) <> 0;
        FindClose(SR);
        end;
      end;
    end;
end;

end.


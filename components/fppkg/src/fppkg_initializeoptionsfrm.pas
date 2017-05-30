unit fppkg_initializeoptionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn,
  ButtonPanel,
  // fppkg
  pkgglobals, pkgFppkg, pkgoptions;

type

  { TInitializeOptionsForm }

  TInitializeOptionsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    AdvancedCheckbox: TCheckBox;
    CompilerEdit: TEdit;
    PrefixLabel: TLabel;
    PathEdit: TEdit;
    FPCDirectoryEdit: TDirectoryEdit;
    Edit1: TEdit;
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
    procedure OKButtonClick(Sender: TObject);
  private
    function IsVersionStr(AString: string): Boolean;
  public
    class function CheckInitialConfiguration: Boolean;
    class function RecreateFppkgConfiguration: Boolean;
  end;

var
  InitializeOptionsForm: TInitializeOptionsForm;

implementation

{$R *.lfm}

{ TInitializeOptionsForm }

procedure TInitializeOptionsForm.FPCDirectoryEditChange(Sender: TObject);
var
  SR: TSearchRec;
  Dir: string;
  Prefix, s: RawByteString;
begin
  Dir := IncludeTrailingPathDelimiter(FPCDirectoryEdit.Text);
  if not DirectoryExists(Dir) then
    FPCDirValidationLabel.Caption := 'Directory does not exist'
  else
    begin
    if FindFirst(Dir+'ppc*'+ExeExt, faAnyFile-faDirectory, SR) = 0 then
      begin
      FindClose(SR);
      if FileExists(Dir+'units') and FileExists(Dir+'fpmkinst') then
        begin
        FPCDirValidationLabel.Caption := '';
        ButtonPanel.OKButton.Enabled := True;

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
        FPCDirValidationLabel.Caption := 'This location does not seems to contain a valid fpc-installation'
      end
    else
      begin
      FPCDirValidationLabel.Caption := 'Compiler not found at given location'
      end;
    end
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
  Dir: string;
begin
  Dir := IncludeTrailingPathDelimiter(FPCDirectoryEdit.Text);
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
      DeleteFile(FileName);
    // This will create the compiler-configuration file
    FPpkg.InitializeCompilerOptions;
  finally
    FPpkg.Free;
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
    if Frm.ShowModal in [mrClose, mrCancel] then
      Result := False
    else
      Result := True;
  finally
    Frm.Free;
  end;
end;

end.


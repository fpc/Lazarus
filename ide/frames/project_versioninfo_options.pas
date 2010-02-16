unit project_versioninfo_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, Grids, Project, IDEOptionsIntf, LazarusIDEStrConsts,
  W32VersionInfo;

type

  { TProjectVersionInfoOptionsFrame }

  TProjectVersionInfoOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutomaticallyIncreaseBuildCheckBox: TCheckBox;
    BuildLabel: TLabel;
    BuildSpinEdit: TSpinEdit;
    CharacterSetComboBox: TComboBox;
    CharacterSetLabel: TLabel;
    LanguageSelectionComboBox: TComboBox;
    LanguageSelectionLabel: TLabel;
    LanguageSettingsGroupBox: TGroupBox;
    MinorVersionLabel: TLabel;
    MinorVersionSpinEdit: TSpinEdit;
    RevisionLabel: TLabel;
    RevisionSpinEdit: TSpinEdit;
    OtherInfoGroupBox: TGroupBox;
    StringInfo: TStringGrid;
    UseVersionInfoCheckBox: TCheckBox;
    VersionInfoGroupBox: TGroupBox;
    MajorVersionLabel: TLabel;
    MajorVersionSpinEdit: TSpinEdit;
    procedure UseVersionInfoCheckBoxChange(Sender: TObject);
  private
    FVersionInfo: TProjectVersionInfo;
    procedure EnableVersionInfo(UseVersionInfo: boolean);
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjectVersionInfoOptionsFrame }

procedure TProjectVersionInfoOptionsFrame.UseVersionInfoCheckBoxChange(Sender: TObject);
begin
  EnableVersionInfo(UseVersionInfoCheckBox.Checked);
end;

procedure TProjectVersionInfoOptionsFrame.EnableVersionInfo(UseVersionInfo: boolean);
begin
  VersionInfoGroupBox.Enabled := UseVersionInfo;
  LanguageSettingsGroupBox.Enabled := UseVersionInfo;
  OtherInfoGroupBox.Enabled := UseVersionInfo;
end;

function TProjectVersionInfoOptionsFrame.GetTitle: string;
begin
  Result := VersionInfoTitle;
end;

procedure TProjectVersionInfoOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  UseVersionInfoCheckBox.Caption := rsIncludeVersionInfoInExecutable;
  VersionInfoGroupBox.Caption := rsVersionNumbering;
  MajorVersionLabel.Caption := rsMajorVersion;
  MinorVersionLabel.Caption := rsMinorVersion;
  RevisionLabel.Caption := rsRevision;
  BuildLabel.Caption := rsBuild;
  AutomaticallyIncreaseBuildCheckBox.Caption := rsAutomaticallyIncreaseBuildNumber;
  LanguageSettingsGroupBox.Caption := rsLanguageOptions;
  LanguageSelectionLabel.Caption := rsLanguageSelection;
  CharacterSetLabel.Caption := rsCharacterSet;
  OtherInfoGroupBox.Caption := rsOtherInfo;
end;

procedure TProjectVersionInfoOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
begin
  FVersionInfo := TProjectVersionInfo((AOptions as TProject).Resources[TProjectVersionInfo]);

  UseVersionInfoCheckBox.Checked := FVersionInfo.UseVersionInfo;
  MajorVersionSpinEdit.Value := FVersionInfo.MajorVersionNr;
  MinorVersionSpinEdit.Value := FVersionInfo.MinorVersionNr;
  RevisionSpinEdit.Value := FVersionInfo.RevisionNr;
  BuildSpinEdit.Value := FVersionInfo.BuildNr;

  EnableVersionInfo(FVersionInfo.UseVersionInfo);

  if FVersionInfo.AutoIncrementBuild then
    AutomaticallyIncreaseBuildCheckBox.Checked := True;
  // ComboBox.sort does not change ItemIndex.[Windows]
  LanguageSelectionComboBox.Items.Assign(MSLanguages);
  LanguageSelectionComboBox.Sorted := True;
  i := MSHexLanguages.IndexOf(FVersionInfo.HexLang);
  if i >= 0 then
   i := LanguageSelectionComboBox.Items.IndexOf(MSLanguages[i]);
  LanguageSelectionComboBox.ItemIndex := i;
  CharacterSetComboBox.Items.Assign(MSCharacterSets);
  CharacterSetComboBox.Sorted := True;
  i := MSHexCharacterSets.IndexOf(FVersionInfo.HexCharSet);
  if i >= 0 then
    i := CharacterSetComboBox.Items.IndexOf(MSCharacterSets[i]);
  CharacterSetComboBox.ItemIndex := i;

  // read string info
  StringInfo.RowCount := FVersionInfo.StringTable.Count + 1;
  for i := 0 to FVersionInfo.StringTable.Count - 1 do
  begin
    StringInfo.Cells[0, i + 1] := FVersionInfo.StringTable.Keys[i];
    StringInfo.Cells[1, i + 1] := FVersionInfo.StringTable.ValuesByIndex[i];
  end;
end;

procedure TProjectVersionInfoOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  VersionInfo: TProjectVersionInfo;
  i: integer;
begin
  VersionInfo := TProjectVersionInfo((AOptions as TProject).Resources[TProjectVersionInfo]);
  VersionInfo.UseVersionInfo := UseVersionInfoCheckBox.Checked;
  VersionInfo.AutoIncrementBuild := AutomaticallyIncreaseBuildCheckBox.Checked;
  VersionInfo.MajorVersionNr := MajorVersionSpinEdit.Value;
  VersionInfo.MinorVersionNr := MinorVersionSpinEdit.Value;
  VersionInfo.RevisionNr := RevisionSpinEdit.Value;
  VersionInfo.BuildNr := BuildSpinEdit.Value;
  VersionInfo.HexLang := MSLanguageToHex(LanguageSelectionComboBox.Text);
  VersionInfo.HexCharSet := MSCharacterSetToHex(CharacterSetComboBox.Text);
  // write string info
  VersionInfo.StringTable.Clear;
  for i := 1 to StringInfo.RowCount - 1 do
    VersionInfo.StringTable[StringInfo.Cells[0, i]] := StringInfo.Cells[1, i];
end;

class function TProjectVersionInfoOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectVersionInfoOptionsFrame,
    ProjectOptionsVersionInfo);

end.


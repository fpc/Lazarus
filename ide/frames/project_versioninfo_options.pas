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

function LanguageCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: String;
begin
  S1 := List[Index1];
  S2 := List[Index2];
  Result := CompareStr(S1, S2);
end;

function CharsetCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: String;
begin
  S1 := List[Index1];
  S2 := List[Index2];
  if S1 = 'Unicode' then
    Result := -1
  else
  if S2 = 'Unicode' then
    Result := 1
  else
    Result := CompareStr(S1, S2);
end;

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
var
  Items: TStringList;
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
  StringInfo.Cells[0, 0] := rsKey;
  StringInfo.Cells[0, 1] := rsValue;
  // fill comboboxes
  Items := TStringList.Create;
  try
    Items.Assign(MSLanguages);
    Items.CustomSort(@LanguageCompare);
    LanguageSelectionComboBox.Items.Assign(Items);

    Items.Assign(MSCharacterSets);
    Items.CustomSort(@CharsetCompare);
    CharacterSetComboBox.Items.Assign(Items);
  finally
    Items.Free;
  end;
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

  i := MSHexLanguages.IndexOf(FVersionInfo.HexLang);
  if i >= 0 then
   i := LanguageSelectionComboBox.Items.IndexOf(MSLanguages[i]);
  LanguageSelectionComboBox.ItemIndex := i;
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
  t: TProjectVersionStringTable;
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
  t:=TProjectVersionStringTable.Create('01234567');
  try
    for i := 1 to StringInfo.RowCount - 1 do
      t[StringInfo.Cells[0, i]] := StringInfo.Cells[1, i];
    VersionInfo.StringTable.Assign(t); // use Assign to check for changes
  finally
    t.Free;
  end;
end;

class function TProjectVersionInfoOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectVersionInfoOptionsFrame,
    ProjectOptionsVersionInfo);

end.


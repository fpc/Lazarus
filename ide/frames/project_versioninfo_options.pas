unit project_versioninfo_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Dialogs, StdCtrls, Spin, Grids, CheckLst,
  // LazUtils
  LazUTF8,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  Project, LazarusIDEStrConsts, W32VersionInfo;

type

  { TProjectVersionInfoOptionsFrame }

  TProjectVersionInfoOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutomaticallyIncreaseBuildCheckBox: TCheckBox;
    BuildLabel: TLabel;
    BuildSpinEdit: TSpinEdit;
    CharacterSetComboBox: TComboBox;
    CharacterSetLabel: TLabel;
    clbAttributes: TCheckListBox;
    AttributesGroupBox: TGroupBox;
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
    procedure clbAttributesClickCheck(Sender: TObject);
    procedure UseVersionInfoCheckBoxChange(Sender: TObject);
  private
    FVersionInfo: TProjectVersionInfo;
    procedure EnableVersionInfo(UseVersionInfo: boolean);
    procedure AddKey(AKey: String);
    procedure DeleteKey(AKey: String);
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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

procedure TProjectVersionInfoOptionsFrame.clbAttributesClickCheck(Sender: TObject);
begin
  if clbAttributes.Checked[Ord(pvaPrivateBuild)] then
    DeleteKey('PrivateBuild')
  else
    AddKey('PrivateBuild');
  if clbAttributes.Checked[Ord(pvaSpecialBuild)] then
    DeleteKey('SpecialBuild')
  else
    AddKey('SpecialBuild');
end;

procedure TProjectVersionInfoOptionsFrame.EnableVersionInfo(UseVersionInfo: boolean);
begin
  VersionInfoGroupBox.Enabled := UseVersionInfo;
  LanguageSettingsGroupBox.Enabled := UseVersionInfo;
  OtherInfoGroupBox.Enabled := UseVersionInfo;
  AttributesGroupBox.Enabled := UseVersionInfo;
end;

procedure TProjectVersionInfoOptionsFrame.AddKey(AKey: String);
var
  I: Integer;
begin
  for I := StringInfo.RowCount - 1 downto 1 do
    if UTF8LowerCase(StringInfo.Cells[0, I]) = UTF8LowerCase(AKey) then
      StringInfo.DeleteRow(I);
end;

procedure TProjectVersionInfoOptionsFrame.DeleteKey(AKey: String);
var
  I: Integer;
begin
  for I := 0 to StringInfo.RowCount - 1 do
    if UTF8LowerCase(StringInfo.Cells[0, I]) = UTF8LowerCase(AKey) then
      Exit;
  StringInfo.RowCount := StringInfo.RowCount + 1;
  StringInfo.Cells[0, StringInfo.RowCount - 1] := AKey;
end;

function TProjectVersionInfoOptionsFrame.GetTitle: string;
begin
  Result := VersionInfoTitle;
end;

procedure TProjectVersionInfoOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Items: TStringList;
  Attr: TProjectVersionAttribute;
begin
  UseVersionInfoCheckBox.Caption := rsIncludeVersionInfoInExecutable;
  UseVersionInfoCheckBox.Hint := rsIncludeVersionInfoHint;
  VersionInfoGroupBox.Caption := rsVersionNumbering;
  MajorVersionLabel.Caption := rsMajorVersion;
  MinorVersionLabel.Caption := rsMinorVersion;
  RevisionLabel.Caption := rsRevision;
  BuildLabel.Caption := rsBuild;
  AutomaticallyIncreaseBuildCheckBox.Caption := rsAutomaticallyIncreaseBuildNumber;
  AutomaticallyIncreaseBuildCheckBox.Hint := rsAutomaticallyIncreaseBuildNumberHint;
  LanguageSettingsGroupBox.Caption := rsLanguageOptions;
  LanguageSelectionLabel.Caption := rsLanguageSelection;
  CharacterSetLabel.Caption := rsCharacterSet;
  OtherInfoGroupBox.Caption := rsOtherInfo;
  StringInfo.Columns[0].Title.Caption := lisKey;
  StringInfo.Columns[1].Title.Caption := lisValue;
  AttributesGroupBox.Caption := rsAttributes;
  for Attr := Low(TProjectVersionAttribute) to High(TProjectVersionAttribute) do
    clbAttributes.AddItem(ProjectVersionAttributeToStr[Attr], nil);
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
  Attr: TProjectVersionAttribute;
begin
  FVersionInfo := (AOptions as TProjectIDEOptions).Project.ProjResources.VersionInfo;

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

  // read attributes
  for Attr in FVersionInfo.Attributes do
    clbAttributes.Checked[Ord(Attr)] := True;

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
  attrs: TProjectVersionAttributes;
begin
  VersionInfo := (AOptions as TProjectIDEOptions).Project.ProjResources.VersionInfo;
  VersionInfo.UseVersionInfo := UseVersionInfoCheckBox.Checked;
  VersionInfo.AutoIncrementBuild := AutomaticallyIncreaseBuildCheckBox.Checked;
  VersionInfo.MajorVersionNr := MajorVersionSpinEdit.Value;
  VersionInfo.MinorVersionNr := MinorVersionSpinEdit.Value;
  VersionInfo.RevisionNr := RevisionSpinEdit.Value;
  VersionInfo.BuildNr := BuildSpinEdit.Value;
  VersionInfo.HexLang := MSLanguageToHex(LanguageSelectionComboBox.Text);
  VersionInfo.HexCharSet := MSCharacterSetToHex(CharacterSetComboBox.Text);
  // write attributes
  attrs := [];
  for i := 0 to clbAttributes.Count - 1 do
    if clbAttributes.Checked[i] then
      include(attrs, TProjectVersionAttribute(i));
  VersionInfo.Attributes := attrs;
  // write string info
  t := TProjectVersionStringTable.Create('01234567');
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
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectVersionInfoOptionsFrame,
    ProjectOptionsVersionInfo);

end.


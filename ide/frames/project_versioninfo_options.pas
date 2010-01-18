unit project_versioninfo_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, Project, IDEOptionsIntf, LazarusIDEStrConsts,
  W32VersionInfo, VersionInfoAdditionalInfo;

type

  { TProjectVersionInfoOptionsFrame }

  TProjectVersionInfoOptionsFrame = class(TAbstractIDEOptionsEditor)
    AdditionalInfoButton: TBitBtn;
    AutomaticallyIncreaseBuildCheckBox: TCheckBox;
    BuildLabel: TLabel;
    BuildSpinEdit: TSpinEdit;
    CharacterSetComboBox: TComboBox;
    CharacterSetLabel: TLabel;
    CopyrightEdit: TEdit;
    CopyrightLabel: TLabel;
    DescriptionEdit: TEdit;
    DescriptionLabel: TLabel;
    LanguageSelectionComboBox: TComboBox;
    LanguageSelectionLabel: TLabel;
    LanguageSettingsGroupBox: TGroupBox;
    MajorRevisionLabel: TLabel;
    MajorRevisionSpinEdit: TSpinEdit;
    MinorRevisionLabel: TLabel;
    MinorRevisionSpinEdit: TSpinEdit;
    OtherInfoGroupBox: TGroupBox;
    UseVersionInfoCheckBox: TCheckBox;
    VersionInfoGroupBox: TGroupBox;
    VersionLabel: TLabel;
    VersionSpinEdit: TSpinEdit;
    procedure AdditionalInfoButtonClick(Sender: TObject);
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

procedure TProjectVersionInfoOptionsFrame.AdditionalInfoButtonClick(Sender: TObject);
begin
  ShowVersionInfoAdditionailInfoForm(FVersionInfo);
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
  VersionLabel.Caption := rsVersion;
  MajorRevisionLabel.Caption := rsMajorRevision;
  MinorRevisionLabel.Caption := rsMinorRevision;
  BuildLabel.Caption := rsBuild;
  AutomaticallyIncreaseBuildCheckBox.Caption := rsAutomaticallyIncreaseBuildNumber;
  LanguageSettingsGroupBox.Caption := rsLanguageOptions;
  LanguageSelectionLabel.Caption := rsLanguageSelection;
  CharacterSetLabel.Caption := rsCharacterSet;
  OtherInfoGroupBox.Caption := rsOtherInfo;
  DescriptionLabel.Caption := lisCodeToolsDefsDescription;
  CopyrightLabel.Caption := rsCopyright;
  AdditionalInfoButton.Caption := rsAdditionalInfo;
  AdditionalInfoButton.LoadGlyphFromLazarusResource('laz_add');
end;

procedure TProjectVersionInfoOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
  var
    i : integer;
begin
  FVersionInfo := TProjectVersionInfo((AOptions as TProject).Resources[TProjectVersionInfo]);

  UseVersionInfoCheckBox.Checked := FVersionInfo.UseVersionInfo;
  VersionSpinEdit.Value := FVersionInfo.VersionNr;
  MajorRevisionSpinEdit.Value := FVersionInfo.MajorRevNr;
  MinorRevisionSpinEdit.Value := FVersionInfo.MinorRevNr;
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
  DescriptionEdit.Text := FVersionInfo.DescriptionString;
  CopyrightEdit.Text := FVersionInfo.CopyrightString;
end;

procedure TProjectVersionInfoOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  VersionInfo: TProjectVersionInfo;
begin
  VersionInfo := TProjectVersionInfo((AOptions as TProject).Resources[TProjectVersionInfo]);
  VersionInfo.UseVersionInfo := UseVersionInfoCheckBox.Checked;
  VersionInfo.AutoIncrementBuild := AutomaticallyIncreaseBuildCheckBox.Checked;
  VersionInfo.VersionNr := VersionSpinEdit.Value;
  VersionInfo.MajorRevNr := MajorRevisionSpinEdit.Value;
  VersionInfo.MinorRevNr := MinorRevisionSpinEdit.Value;
  VersionInfo.BuildNr := BuildSpinEdit.Value;
  VersionInfo.DescriptionString := DescriptionEdit.Text;
  VersionInfo.CopyrightString := CopyrightEdit.Text;
  VersionInfo.HexLang := MSLanguageToHex(LanguageSelectionComboBox.Text);
  VersionInfo.HexCharSet := MSCharacterSetToHex(CharacterSetComboBox.Text);
end;

class function TProjectVersionInfoOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectVersionInfoOptionsFrame,
    ProjectOptionsVersionInfo);

end.


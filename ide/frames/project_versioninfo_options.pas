unit project_versioninfo_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
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
    FProject: TProject;
    procedure EnableVersionInfo(UseVersionInfo: boolean);
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TProjectVersionInfoOptionsFrame }

procedure TProjectVersionInfoOptionsFrame.UseVersionInfoCheckBoxChange(Sender: TObject);
begin
  EnableVersionInfo(UseVersionInfoCheckBox.Checked);
end;

procedure TProjectVersionInfoOptionsFrame.AdditionalInfoButtonClick(Sender: TObject);
begin
  ShowVersionInfoAdditionailInfoForm(FProject.Resources.VersionInfo);
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
begin
  FProject := AOptions as TProject;
  with FProject do
  begin
    UseVersionInfoCheckBox.Checked := Resources.VersionInfo.UseVersionInfo;
    VersionSpinEdit.Value := Resources.VersionInfo.VersionNr;
    MajorRevisionSpinEdit.Value := Resources.VersionInfo.MajorRevNr;
    MinorRevisionSpinEdit.Value := Resources.VersionInfo.MinorRevNr;
    BuildSpinEdit.Value := Resources.VersionInfo.BuildNr;

    EnableVersionInfo(Resources.VersionInfo.UseVersionInfo);

    if Resources.VersionInfo.AutoIncrementBuild then
      AutomaticallyIncreaseBuildCheckBox.Checked := True;
    LanguageSelectionComboBox.Items.Assign(MSLanguages);
    LanguageSelectionComboBox.ItemIndex :=
      MSHexLanguages.IndexOf(Resources.VersionInfo.HexLang);
    LanguageSelectionComboBox.Sorted := True;
    CharacterSetComboBox.Items.Assign(MSCharacterSets);
    CharacterSetComboBox.ItemIndex :=
      MSHexCharacterSets.IndexOf(Resources.VersionInfo.HexCharSet);
    CharacterSetComboBox.Sorted := True;
    DescriptionEdit.Text := Resources.VersionInfo.DescriptionString;
    CopyrightEdit.Text := Resources.VersionInfo.CopyrightString;
  end;
end;

procedure TProjectVersionInfoOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProject do
  begin
    Resources.VersionInfo.UseVersionInfo := UseVersionInfoCheckBox.Checked;
    Resources.VersionInfo.AutoIncrementBuild := AutomaticallyIncreaseBuildCheckBox.Checked;
    Resources.VersionInfo.VersionNr := VersionSpinEdit.Value;
    Resources.VersionInfo.MajorRevNr := MajorRevisionSpinEdit.Value;
    Resources.VersionInfo.MinorRevNr := MinorRevisionSpinEdit.Value;
    Resources.VersionInfo.BuildNr := BuildSpinEdit.Value;
    Resources.VersionInfo.DescriptionString := DescriptionEdit.Text;
    Resources.VersionInfo.CopyrightString := CopyrightEdit.Text;
    Resources.VersionInfo.HexLang := MSLanguageToHex(LanguageSelectionComboBox.Text);
    Resources.VersionInfo.HexCharSet := MSCharacterSetToHex(CharacterSetComboBox.Text);
  end;
end;

class function TProjectVersionInfoOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  {$I project_versioninfo_options.lrs}
  RegisterIDEOptionsEditor(GroupProject, TProjectVersionInfoOptionsFrame,
    ProjectOptionsVersionInfo);

end.


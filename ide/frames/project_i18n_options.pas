unit project_i18n_options;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  // LCL
  StdCtrls, EditBtn, ExtCtrls, Graphics,
  // LazUtils
  LazFileUtils,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  Project, LazarusIDEStrConsts;

type

  { TProjectI18NOptionsFrame }

  TProjectI18NOptionsFrame = class(TAbstractIDEOptionsEditor)
    ForceUpdatePoFilesCheckBox: TCheckBox;
    EnableI18NCheckBox: TCheckBox;
    ExcludedGroupBox: TGroupBox;
    I18NGroupBox: TGroupBox;
    ExcludedIdentifiersMemo: TMemo;
    ExcludedOriginalsMemo: TMemo;
    ExcludedIdentifiersLabel: TLabel;
    ExcludedIdentifiersPanel: TPanel;
    ExcludedOriginalsPanel: TPanel;
    ExcludedOriginalsLabel: TLabel;
    PoForFormsCheckBox: TCheckBox;
    POOutDirEdit: TEditButton;
    PoOutDirLabel: TLabel;
    procedure EnableI18NCheckBoxChange(Sender: TObject);
    procedure ExcludedIdentifiersMemoChange(Sender: TObject);
    procedure ExcludedOriginalsMemoChange(Sender: TObject);
    procedure POOutDirButtonClick(Sender: TObject);
  private
    FProject: TProject;
    FExcludedStringsChanged: Boolean;
    procedure Enablei18nInfo(Usei18n: boolean);
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjectI18NOptionsFrame }

procedure TProjectI18NOptionsFrame.EnableI18NCheckBoxChange(Sender: TObject);
begin
  Enablei18nInfo(EnableI18NCheckBox.Checked);
end;

procedure TProjectI18NOptionsFrame.POOutDirButtonClick(Sender: TObject);
var
  NewDirectory: string;
begin
  NewDirectory := LazSelectDirectory(lisPOChoosePoFileDirectory,
                                     FProject.Directory);
  if NewDirectory = '' then Exit;
  if not FProject.IsVirtual then
    NewDirectory:=CreateRelativePath(NewDirectory,FProject.Directory);
  POOutDirEdit.Text := NewDirectory;
end;

procedure TProjectI18NOptionsFrame.ExcludedIdentifiersMemoChange(Sender: TObject);
begin
  FExcludedStringsChanged := True;
  ExcludedIdentifiersLabel.Font.Style := [fsBold];
  ForceUpdatePoFilesCheckBox.Font.Style := [fsBold];
end;

procedure TProjectI18NOptionsFrame.ExcludedOriginalsMemoChange(Sender: TObject);
begin
  FExcludedStringsChanged := True;
  ExcludedOriginalsLabel.Font.Style := [fsBold];
  ForceUpdatePoFilesCheckBox.Font.Style := [fsBold];
end;

procedure TProjectI18NOptionsFrame.Enablei18nInfo(Usei18n: boolean);
begin
  I18NGroupBox.Enabled := Usei18n;
  ExcludedGroupBox.Enabled := Usei18n;
  ForceUpdatePoFilesCheckBox.Enabled := Usei18n;
end;

function TProjectI18NOptionsFrame.GetTitle: string;
begin
  Result := dlgPOI18n;
end;

procedure TProjectI18NOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  EnableI18NCheckBox.Caption := rsEnableI18n;
  EnableI18NCheckBox.Hint:=lisEnableInternationalizationAndTranslationSupport;
  I18NGroupBox.Caption := rsI18nOptions;
  PoOutDirLabel.Caption := rsPOOutputDirectory;
  POOutDirEdit.Hint:=lisDirectoryWhereTheIDEPutsThePoFiles;
  PoForFormsCheckBox.Caption:=lisCreateUpdatePoFileWhenSavingALfmFile;
  PoForFormsCheckBox.Hint:=
    lisYouCanDisableThisForIndividualFormsViaThePopupMenu;
  ExcludedGroupBox.Caption := rsI18nExcluded;
  ExcludedIdentifiersLabel.Caption := rsI18nIdentifiers;
  ExcludedOriginalsLabel.Caption := rsI18nOriginals;
  ForceUpdatePoFilesCheckBox.Caption := rsI18nForceUpdatePoFilesOnNextBuild;
end;

procedure TProjectI18NOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  FProject := (AOptions as TProjectIDEOptions).Project;
  with FProject do
  begin
    POOutDirEdit.Text := POOutputDirectory;
    EnableI18NCheckBox.Checked := Enablei18n;
    PoForFormsCheckBox.Checked:=EnableI18NForLFM;
    ExcludedIdentifiersMemo.Lines.Clear;
    ExcludedIdentifiersMemo.Lines.AddStrings(I18NExcludedIdentifiers);
    ExcludedOriginalsMemo.Lines.Clear;
    ExcludedOriginalsMemo.Lines.AddStrings(I18NExcludedOriginals);
    ForceUpdatePoFilesCheckBox.Checked := ForceUpdatePoFiles;
    Enablei18nInfo(Enablei18n);
  end;
  FExcludedStringsChanged := False;
  ExcludedIdentifiersLabel.ParentFont := True;
  ExcludedOriginalsLabel.ParentFont := True;
  ForceUpdatePoFilesCheckBox.ParentFont := True;
  IDEImages.AssignImage(POOutDirEdit.Button, ResBtnSelDir); //DirectoryEdit
end;

procedure TProjectI18NOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with (AOptions as TProjectIDEOptions).Project do
  begin
    POOutputDirectory := POOutDirEdit.Text;
    EnableI18N := EnableI18NCheckBox.Checked;
    EnableI18NForLFM := PoForFormsCheckBox.Checked;
    I18NExcludedIdentifiers.Clear;
    I18NExcludedIdentifiers.AddStrings(ExcludedIdentifiersMemo.Lines);
    I18NExcludedOriginals.Clear;
    I18NExcludedOriginals.AddStrings(ExcludedOriginalsMemo.Lines);
    ForceUpdatePoFiles := ForceUpdatePoFilesCheckBox.Checked;
    if FExcludedStringsChanged then
      Modified := True;
  end;
end;

class function TProjectI18NOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectI18NOptionsFrame, ProjectOptionsI18N);

end.


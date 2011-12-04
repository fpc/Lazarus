unit project_i18n_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Project, IDEProcs, IDEOptionsIntf, LazarusIDEStrConsts, IDEDialogs;

type

  { TProjectI18NOptionsFrame }

  TProjectI18NOptionsFrame = class(TAbstractIDEOptionsEditor)
    EnableI18NCheckBox: TCheckBox;
    I18NGroupBox: TGroupBox;
    PoForFormsCheckBox: TCheckBox;
    POOutDirButton: TButton;
    POOutDirEdit: TEdit;
    PoOutDirLabel: TLabel;
    procedure EnableI18NCheckBoxChange(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure POOutDirButtonClick(Sender: TObject);
  private
    FProject: TProject;
    procedure Enablei18nInfo(Usei18n: boolean);
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
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

procedure TProjectI18NOptionsFrame.FrameClick(Sender: TObject);
begin

end;

procedure TProjectI18NOptionsFrame.POOutDirButtonClick(Sender: TObject);
var
  NewDirectory: string;
begin
  NewDirectory := LazSelectDirectory(lisPOChoosePoFileDirectory,
                                     FProject.ProjectDirectory);
  if NewDirectory = '' then Exit;
  if not FProject.IsVirtual then
    NewDirectory:=CreateRelativePath(NewDirectory,FProject.ProjectDirectory);
  POOutDirEdit.Text := NewDirectory;
end;

procedure TProjectI18NOptionsFrame.Enablei18nInfo(Usei18n: boolean);
begin
  I18NGroupBox.Enabled := Usei18n;
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
end;

procedure TProjectI18NOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  AFilename: String;
begin
  FProject := AOptions as TProject;
  with FProject do
  begin
    AFilename := POOutputDirectory;
    if not IsVirtual then
      AFilename:=CreateRelativePath(AFilename,FProject.ProjectDirectory);
    POOutDirEdit.Text := AFilename;
    EnableI18NCheckBox.Checked := Enablei18n;
    PoForFormsCheckBox.Checked:=EnableI18NForLFM;
    Enablei18nInfo(Enablei18n);
  end;
end;

procedure TProjectI18NOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  AFilename: String;
begin
  with AOptions as TProject do
  begin
    AFilename := ChompPathDelim(TrimFilename(POOutDirEdit.Text));
    if EnableI18NCheckBox.Checked
    and (AFilename<>'')
    and (not FilenameIsAbsolute(AFilename)) and (not IsVirtual) then
      AFilename:=AppendPathDelim(ProjectDirectory)+AFilename;
    POOutputDirectory := AFilename;
    EnableI18N := EnableI18NCheckBox.Checked;
    EnableI18NForLFM := PoForFormsCheckBox.Checked;
  end;
end;

class function TProjectI18NOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectI18NOptionsFrame, ProjectOptionsI18N);

end.


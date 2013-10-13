unit project_application_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, ExtDlgs, Math, LCLType, IDEOptionsIntf,
  LazIDEIntf, IDEDialogs, Project, LazarusIDEStrConsts, EnvironmentOpts,
  ApplicationBundle, ProjectIcon, W32Manifest, CompilerOptions;

type

  { TProjectApplicationOptionsFrame }

  TProjectApplicationOptionsFrame = class(TAbstractIDEOptionsEditor)
    AppSettingsGroupBox: TGroupBox;
    DefaultIconButton: TButton;
    UIAccessCheckBox: TCheckBox;
    ExecutionLevelComboBox: TComboBox;
    DpiAwareCheckBox: TCheckBox;
    ClearIconButton: TBitBtn;
    CreateAppBundleButton: TBitBtn;
    IconImage: TImage;
    IconLabel: TLabel;
    IconPanel: TPanel;
    IconTrack: TTrackBar;
    IconTrackLabel: TLabel;
    ExecutionLevelLabel: TLabel;
    LoadIconButton: TBitBtn;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveIconButton: TBitBtn;
    SavePictureDialog1: TSavePictureDialog;
    TitleEdit: TEdit;
    TitleLabel: TLabel;
    UseAppBundleCheckBox: TCheckBox;
    UseXPManifestCheckBox: TCheckBox;
    procedure ClearIconButtonClick(Sender: TObject);
    procedure CreateAppBundleButtonClick(Sender: TObject);
    procedure DefaultIconButtonClick(Sender: TObject);
    procedure IconImagePictureChanged(Sender: TObject);
    procedure IconTrackChange(Sender: TObject);
    procedure LoadIconButtonClick(Sender: TObject);
    procedure SaveIconButtonClick(Sender: TObject);
    procedure UseXPManifestCheckBoxChange(Sender: TObject);
  private
    FProject: TProject;
    procedure SetIconFromStream(Value: TStream);
    function GetIconAsStream: TStream;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

const
  ExecutionLevelToCaption: array[TXPManifestExecutionLevel] of PString =  (
  { xmelAsInvoker            } @dlgPOAsInvoker,
  { xmelHighestAvailable     } @dlgPOHighestAvailable,
  { xmelRequireAdministrator } @dlgPORequireAdministrator
  );

function CreateProjectApplicationBundle(AProject: TProject): string;
// returns target file name
var
  TargetExeName: string;
begin
  Result := '';
  if AProject.MainUnitInfo = nil then
  begin
    IDEMessageDialog(lisCCOErrorCaption, lisThisProjectHasNoMainSourceFile,
      mtError, [mbCancel]);
    Exit;
  end;
  if AProject.IsVirtual then
    TargetExeName := LazarusIDE.GetTestBuildDirectory +
      ExtractFilename(AProject.MainUnitInfo.Filename)
  else
    TargetExeName := AProject.CompilerOptions.CreateTargetFilename(
      AProject.MainFilename);

  if not (CreateApplicationBundle(TargetExeName, AProject.GetTitle, True) in
    [mrOk, mrIgnore]) then
  begin
    IDEMessageDialog(lisCCOErrorCaption, Format(
      lisFailedToCreateApplicationBundleFor, [TargetExeName]), mtError, [
      mbCancel]);
    Exit;
  end;
  if not (CreateAppBundleSymbolicLink(TargetExeName, True) in [mrOk, mrIgnore]) then
  begin
    // no error message needed
    Exit;
  end;
  IDEMessageDialog(lisSuccess, Format(lisTheApplicationBundleWasCreatedFor, [
    TargetExeName]), mtInformation, [mbOk]);
  Result := TargetExeName;
end;

{ TProjectApplicationOptionsFrame }

procedure TProjectApplicationOptionsFrame.IconImagePictureChanged(Sender: TObject);
var
  HasIcon: boolean;
  cx, cy: integer;
begin
  HasIcon := (IconImage.Picture.Graphic <> nil) and
    (not IconImage.Picture.Graphic.Empty);
  IconTrack.Enabled := HasIcon;
  if HasIcon then
  begin
    IconTrack.Min := 0;
    IconTrack.Max := IconImage.Picture.Icon.Count - 1;
    IconTrack.Position := IconImage.Picture.Icon.Current;
    IconImage.Picture.Icon.GetSize(cx, cy);
    IconTrackLabel.Caption :=
      Format(dlgPOIconDesc, [cx, cy, PIXELFORMAT_BPP[IconImage.Picture.Icon.PixelFormat]]);
  end
  else
    IconTrackLabel.Caption := dlgPOIconDescNone;
end;

procedure TProjectApplicationOptionsFrame.IconTrackChange(Sender: TObject);
begin
  IconImage.Picture.Icon.Current :=
    Max(0, Min(IconImage.Picture.Icon.Count - 1, IconTrack.Position));
end;

procedure TProjectApplicationOptionsFrame.ClearIconButtonClick(Sender: TObject);
begin
  IconImage.Picture.Clear;
end;

procedure TProjectApplicationOptionsFrame.CreateAppBundleButtonClick(Sender: TObject);
begin
  CreateProjectApplicationBundle(FProject);
end;

procedure TProjectApplicationOptionsFrame.DefaultIconButtonClick(Sender: TObject);
begin
  IconImage.Picture.Icon.LoadFromResourceName(HInstance, 'MAINICONPROJECT');
end;

procedure TProjectApplicationOptionsFrame.LoadIconButtonClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    try
      IconImage.Picture.LoadFromFile(OpenPictureDialog1.FileName)
    except
      on E: Exception do
        IDEMessageDialog(lisCCOErrorCaption, E.Message, mtError, [mbOK]);
    end;
end;

procedure TProjectApplicationOptionsFrame.SaveIconButtonClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
    IconImage.Picture.SaveToFile(SavePictureDialog1.FileName);
end;

procedure TProjectApplicationOptionsFrame.UseXPManifestCheckBoxChange(Sender: TObject);
begin
  DpiAwareCheckBox.Enabled := UseXPManifestCheckBox.Checked;
  ExecutionLevelLabel.Enabled := UseXPManifestCheckBox.Checked;
  ExecutionLevelComboBox.Enabled := UseXPManifestCheckBox.Checked;
  UIAccessCheckBox.Enabled := UseXPManifestCheckBox.Checked;
end;

procedure TProjectApplicationOptionsFrame.SetIconFromStream(Value: TStream);
begin
  IconImage.Picture.Clear;
  if Value <> nil then
    try
      IconImage.Picture.Icon.LoadFromStream(Value);
    except
      on E: Exception do
        IDEMessageDialog(lisCodeToolsDefsReadError, E.Message, mtError, [mbOK]);
    end;
end;

function TProjectApplicationOptionsFrame.GetIconAsStream: TStream;
begin
  Result := nil;
  if not ((IconImage.Picture.Graphic = nil) or IconImage.Picture.Graphic.Empty) then
  begin
    Result := TMemoryStream.Create;
    IconImage.Picture.Icon.SaveToStream(Result);
    Result.Position := 0;
  end;
end;

function TProjectApplicationOptionsFrame.GetTitle: string;
begin
  Result := dlgPOApplication;
end;

procedure TProjectApplicationOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  ExecutionLevel: TXPManifestExecutionLevel;
begin
  AppSettingsGroupBox.Caption := dlgApplicationSettings;
  TitleLabel.Caption := dlgPOTitle;
  TitleEdit.Text := '';
  UseAppBundleCheckBox.Caption := dlgPOUseAppBundle;
  UseAppBundleCheckBox.Checked := False;

  // manifest
  UseXPManifestCheckBox.Caption := dlgPOUseManifest;
  DpiAwareCheckBox.Caption := dlgPODpiAware;
  ExecutionLevelLabel.Caption := dlgPOExecutionLevel;
  for ExecutionLevel := Low(TXPManifestExecutionLevel) to High(TXPManifestExecutionLevel) do
    ExecutionLevelComboBox.Items.Add(ExecutionLevelToCaption[ExecutionLevel]^);
  UIAccessCheckBox.Caption := dlgPOUIAccess;

  CreateAppBundleButton.Caption := dlgPOCreateAppBundle;
  CreateAppBundleButton.LoadGlyphFromResourceName(HInstance, 'pkg_compile');

  // icon
  IconLabel.Caption := dlgPOIcon;
  LoadIconButton.Caption := dlgPOLoadIcon;
  DefaultIconButton.Caption := dlgPODefaultIcon;
  SaveIconButton.Caption := dlgPOSaveIcon;
  ClearIconButton.Caption := dlgPOClearIcon;
  LoadIconButton.LoadGlyphFromStock(idButtonOpen);
  if LoadIconButton.Glyph.Empty then
    LoadIconButton.LoadGlyphFromResourceName(HInstance, 'laz_open');
  SaveIconButton.LoadGlyphFromStock(idButtonSave);
  if SaveIconButton.Glyph.Empty then
    SaveIconButton.LoadGlyphFromResourceName(HInstance, 'laz_save');
  ClearIconButton.LoadGlyphFromResourceName(HInstance, 'menu_clean');
  IconImagePictureChanged(nil);
end;

procedure TProjectApplicationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  AStream: TStream;
begin
  FProject := AOptions as TProject;
  with FProject do
  begin
    TitleEdit.Text := Title;
    UseAppBundleCheckBox.Checked := UseAppBundle;
    UseXPManifestCheckBox.Checked := TProjectXPManifest(ProjResources[TProjectXPManifest]).UseManifest;
    DpiAwareCheckBox.Checked := TProjectXPManifest(ProjResources[TProjectXPManifest]).DpiAware;
    ExecutionLevelComboBox.ItemIndex := Ord(TProjectXPManifest(ProjResources[TProjectXPManifest]).ExecutionLevel);
    UIAccessCheckBox.Checked := TProjectXPManifest(ProjResources[TProjectXPManifest]).UIAccess;
    DpiAwareCheckBox.Enabled := UseXPManifestCheckBox.Checked;
    ExecutionLevelLabel.Enabled := UseXPManifestCheckBox.Checked;
    ExecutionLevelComboBox.Enabled := UseXPManifestCheckBox.Checked;
    UIAccessCheckBox.Enabled := UseXPManifestCheckBox.Checked;
    AStream := TProjectIcon(ProjResources[TProjectIcon]).GetStream;
    try
      SetIconFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TProjectApplicationOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  AStream: TStream;
begin
  with AOptions as TProject do
  begin
    Title := TitleEdit.Text;
    AStream := GetIconAsStream;
    try
      ProjResources.ProjectIcon.SetStream(AStream);
    finally
      AStream.Free;
    end;
    UseAppBundle := UseAppBundleCheckBox.Checked;
    with ProjResources.XPManifest do
    begin
      UseManifest := UseXPManifestCheckBox.Checked;
      DpiAware := DpiAwareCheckBox.Checked;
      ExecutionLevel := TXPManifestExecutionLevel(ExecutionLevelComboBox.ItemIndex);
      UIAccess := UIAccessCheckBox.Checked;
    end;
  end;
end;

class function TProjectApplicationOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectApplicationOptionsFrame, ProjectOptionsApplication);

end.


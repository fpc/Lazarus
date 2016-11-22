unit project_application_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, ExtDlgs, Math, LCLType, IDEOptionsIntf,
  LazIDEIntf, IDEDialogs, DividerBevel, Project, LazarusIDEStrConsts,
  EnvironmentOpts, ApplicationBundle, ProjectIcon, W32Manifest, CompilerOptions,
  ManifestDialog;

type

  { TProjectApplicationOptionsFrame }

  TProjectApplicationOptionsFrame = class(TAbstractIDEOptionsEditor)
    AppSettingsGroupBox: TGroupBox;
    ManifestButton: TButton;
    CreateAppBundleButton: TBitBtn;
    DefaultIconButton: TButton;
    WindowsDividerBevel: TDividerBevel;
    DarwinDividerBevel: TDividerBevel;
    ClearIconButton: TBitBtn;
    IconImage: TImage;
    IconLabel: TLabel;
    IconPanel: TPanel;
    IconTrack: TTrackBar;
    IconTrackLabel: TLabel;
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
    procedure ManifestButtonClick(Sender: TObject);
    procedure SaveIconButtonClick(Sender: TObject);
    procedure UseXPManifestCheckBoxChange(Sender: TObject);
  private
    FProject: TProject;
    fIconChanged: boolean;
    procedure SetIconFromStream(Value: TStream);
    function GetIconAsStream: TStream;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

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
    TargetExeName := AProject.CompilerOptions.CreateTargetFilename;

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
  fIconChanged:=true;
  IconImage.Picture.Clear;
end;

procedure TProjectApplicationOptionsFrame.CreateAppBundleButtonClick(Sender: TObject);
begin
  CreateProjectApplicationBundle(FProject);
end;

procedure TProjectApplicationOptionsFrame.DefaultIconButtonClick(Sender: TObject);
begin
  IconImage.Picture.Icon.LoadFromResourceName(HInstance, 'MAINICONPROJECT');
  fIconChanged:=true;
end;

procedure TProjectApplicationOptionsFrame.LoadIconButtonClick(Sender: TObject);
begin
  if not OpenPictureDialog1.Execute then exit;
  try
    IconImage.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    fIconChanged:=true;
  except
    on E: Exception do
      IDEMessageDialog(lisCCOErrorCaption, E.Message, mtError, [mbOK]);
  end;
end;

procedure TProjectApplicationOptionsFrame.ManifestButtonClick(Sender: TObject);
var
  Man: TProjectXPManifest;
  Form: TManifestForm;
begin
  Man:= FProject.ProjResources.XPManifest;
  Form:= TManifestForm.Create(Self);
  try
    Form.DpiAwareComboBox.ItemIndex := Ord(Man.DpiAware);
    Form.ExecutionLevelComboBox.ItemIndex := Ord(Man.ExecutionLevel);
    Form.UIAccessCheckBox.Checked := Man.UIAccess;
    Form.NameEdit.Text := Man.TextName;
    Form.DescEdit.Text := Man.TextDesc;
    if Form.ShowModal=mrOk then
    begin
      Man.DpiAware := TXPManifestDpiAware(Form.DpiAwareComboBox.ItemIndex);
      Man.ExecutionLevel := TXPManifestExecutionLevel(Form.ExecutionLevelComboBox.ItemIndex);
      Man.UIAccess := Form.UIAccessCheckBox.Checked;
      Man.TextName := Form.NameEdit.Text;
      Man.TextDesc := Form.DescEdit.Text;
    end;
  finally
    Form.Free;
  end;
end;

procedure TProjectApplicationOptionsFrame.SaveIconButtonClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
    IconImage.Picture.SaveToFile(SavePictureDialog1.FileName);
end;

procedure TProjectApplicationOptionsFrame.UseXPManifestCheckBoxChange(Sender: TObject);
begin
  ManifestButton.Enabled := UseXPManifestCheckBox.Checked;
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
begin
  AppSettingsGroupBox.Caption := dlgApplicationSettings;
  TitleLabel.Caption := dlgPOTitle;
  TitleEdit.Text := '';
  UseAppBundleCheckBox.Caption := dlgPOUseAppBundle;
  UseAppBundleCheckBox.Checked := False;

  // Windows specific, Manifest
  WindowsDividerBevel.Caption := lisForWindows;
  UseXPManifestCheckBox.Caption := dlgPOUseManifest;
  ManifestButton.Caption := dlgPOManifestOptions;
  UseXPManifestCheckBoxChange(nil);

  // Darwin specific, Application Bundle
  DarwinDividerBevel.Caption := lisForDarwin;
  CreateAppBundleButton.Caption := dlgPOCreateAppBundle;
  CreateAppBundleButton.LoadGlyphFromResourceName(HInstance, 'pkg_compile');

  // Icon
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
  IconImage.KeepOriginXWhenClipped := True;
  IconImage.KeepOriginYWhenClipped := True;
  IconImagePictureChanged(nil);
end;

procedure TProjectApplicationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  AStream: TStream;
begin
  FProject := (AOptions as TProjectIDEOptions).Project;
  with FProject do
  begin
    TitleEdit.Text := Title;
    UseAppBundleCheckBox.Checked := UseAppBundle;
    with ProjResources.XPManifest do
    begin
      UseXPManifestCheckBox.Checked := UseManifest;
    end;
    AStream := TProjectIcon(ProjResources[TProjectIcon]).GetStream;
    try
      SetIconFromStream(AStream);
    finally
      AStream.Free;
    end;
    fIconChanged:=false;
  end;
end;

procedure TProjectApplicationOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  AStream: TStream;
begin
  with (AOptions as TProjectIDEOptions).Project {AOptions as TProject} do
  begin
    Title := TitleEdit.Text;
    if fIconChanged then
    begin
      AStream := GetIconAsStream;
      try
        ProjResources.ProjectIcon.SetStream(AStream);
      finally
        AStream.Free;
      end;
    end;
    UseAppBundle := UseAppBundleCheckBox.Checked;
    with ProjResources.XPManifest do
    begin
      UseManifest := UseXPManifestCheckBox.Checked;
    end;
  end;
end;

class function TProjectApplicationOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectApplicationOptionsFrame, ProjectOptionsApplication);

end.


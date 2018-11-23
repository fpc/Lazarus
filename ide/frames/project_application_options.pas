unit project_application_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LazUtils
  FileUtil,
  // LCL
  LCLProc, LCLType, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  ComCtrls, ExtDlgs, LCLStrConsts,
  // LazControls
  DividerBevel,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, LazIDEIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, Project, ProjectIcon, CompilerOptions,
  ApplicationBundle, W32Manifest;

type

  { TProjectApplicationOptionsFrame }

  TProjectApplicationOptionsFrame = class(TAbstractIDEOptionsEditor)
    AppSettingsGroupBox: TGroupBox;
    LongPathCheckBox: TCheckBox;
    DarwinDividerBevel: TDividerBevel;
    NameEdit: TEdit;
    DescriptionEdit: TEdit;
    NameLabel: TLabel;
    DescriptionLabel: TLabel;
    UseLCLScalingCheckBox: TCheckBox;
    CreateAppBundleButton: TBitBtn;
    DefaultIconButton: TButton;
    DpiAwareLabel: TLabel;
    DpiAwareComboBox: TComboBox;
    WindowsDividerBevel: TDividerBevel;
    UIAccessCheckBox: TCheckBox;
    ExecutionLevelComboBox: TComboBox;
    ClearIconButton: TBitBtn;
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
    fIconChanged: boolean;
    procedure EnableManifest(aEnable: Boolean);
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
  IconImage.Picture.Clear;
  fIconChanged:=true;
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
  if OpenPictureDialog1.InitialDir='' then
    OpenPictureDialog1.InitialDir:=FProject.Directory;
  if not OpenPictureDialog1.Execute then exit;
  try
    IconImage.Picture.Icon.LoadFromFile(OpenPictureDialog1.FileName);
    fIconChanged:=true;
  except
    on E: Exception do
      IDEMessageDialog(lisCCOErrorCaption, E.Message, mtError, [mbOK]);
  end;
end;

procedure TProjectApplicationOptionsFrame.SaveIconButtonClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
    IconImage.Picture.Icon.SaveToFile(SavePictureDialog1.FileName);
end;

procedure TProjectApplicationOptionsFrame.EnableManifest(aEnable: Boolean);
begin
  DpiAwareLabel.Enabled := aEnable;
  DpiAwareComboBox.Enabled := aEnable;
  ExecutionLevelLabel.Enabled := aEnable;
  ExecutionLevelComboBox.Enabled := aEnable;
  UIAccessCheckBox.Enabled := aEnable;
  LongPathCheckBox.Enabled := aEnable;
  NameEdit.Enabled := aEnable;
  DescriptionEdit.Enabled := aEnable;
end;

procedure TProjectApplicationOptionsFrame.UseXPManifestCheckBoxChange(Sender: TObject);
begin
  EnableManifest(UseXPManifestCheckBox.Checked);
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
  DpiLevel: TXPManifestDpiAware;
  DpiLevelNames: array[TXPManifestDpiAware] of string;
begin
  AppSettingsGroupBox.Caption := dlgApplicationSettings;
  TitleLabel.Caption := dlgPOTitle;
  TitleEdit.Text := '';
  UseLCLScalingCheckBox.Caption := dlgPOUseLCLScaling;
  UseLCLScalingCheckBox.Checked := False;
  UseAppBundleCheckBox.Caption := dlgPOUseAppBundle;
  UseAppBundleCheckBox.Checked := False;

  // Windows specific, Manifest
  WindowsDividerBevel.Caption := lisForWindows;
  UseXPManifestCheckBox.Caption := dlgPOUseManifest;

  DpiAwareLabel.Caption := dlgPODpiAwareness;
  DpiLevelNames[xmdaFalse] := dlgPODpiAwarenessOff;
  DpiLevelNames[xmdaTrue] := dlgPODpiAwarenessOn;
  DpiLevelNames[xmdaPerMonitor] := dlgPODpiAwarenessOldOffNewPerMonitor;
  DpiLevelNames[xmdaTruePM] := dlgPODpiAwarenessOldOnNewPerMonitor;
  DpiLevelNames[xmdaPerMonitorV2] := dlgPODpiAwarenessOldOnNewPerMonitorV2;

  ExecutionLevelLabel.Caption := dlgPOExecutionLevel;
  for ExecutionLevel in TXPManifestExecutionLevel do
    ExecutionLevelComboBox.Items.Add(ExecutionLevelToCaption[ExecutionLevel]^);
  for DpiLevel in TXPManifestDpiAware do
    DpiAwareComboBox.Items.Add(DpiLevelNames[DpiLevel] + ' (' + ManifestDpiAwareValues[DpiLevel] + ')');
  UIAccessCheckBox.Caption := dlgPOUIAccess;
  LongPathCheckBox.Caption := dlgPOLongPathAware;
  NameLabel.Caption := lisName;
  DescriptionLabel.Caption := lisCodeHelpDescrTag;

  // Darwin specific, Application Bundle
  DarwinDividerBevel.Caption := lisForMacOSDarwin;
  CreateAppBundleButton.Caption := dlgPOCreateAppBundle;
  IDEImages.AssignImage(CreateAppBundleButton, 'pkg_compile');

  // Icon
  IconLabel.Caption := dlgPOIcon;
  LoadIconButton.Caption := dlgPOLoadIcon;
  DefaultIconButton.Caption := dlgPODefaultIcon;
  SaveIconButton.Caption := dlgPOSaveIcon;
  ClearIconButton.Caption := dlgPOClearIcon;
  LoadIconButton.LoadGlyphFromStock(idButtonOpen);
  if LoadIconButton.Glyph.Empty then
    IDEImages.AssignImage(LoadIconButton, 'laz_open');
  SaveIconButton.LoadGlyphFromStock(idButtonSave);
  if SaveIconButton.Glyph.Empty then
    IDEImages.AssignImage(SaveIconButton, 'laz_save');
  IDEImages.AssignImage(ClearIconButton, 'menu_clean');
  IconImage.KeepOriginXWhenClipped := True;
  IconImage.KeepOriginYWhenClipped := True;
  IconImagePictureChanged(nil);
  OpenPictureDialog1.Filter := GraphicFilter(TIcon)+'|'+
                         Format(rsAllFiles,[GetAllFilesMask,GetAllFilesMask,'']);
end;

procedure TProjectApplicationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  AStream: TStream;
begin
  FProject := (AOptions as TProjectIDEOptions).Project;
  with FProject do
  begin
    TitleEdit.Text := Title;
    UseLCLScalingCheckBox.Checked := Scaled;
    UseAppBundleCheckBox.Checked := UseAppBundle;
    // Manifest
    with ProjResources.XPManifest do
    begin
      UseXPManifestCheckBox.Checked := UseManifest;
      DpiAwareComboBox.ItemIndex := Ord(DpiAware);
      ExecutionLevelComboBox.ItemIndex := Ord(ExecutionLevel);
      UIAccessCheckBox.Checked := UIAccess;
      LongPathCheckBox.Checked := LongPathAware;
      NameEdit.Text := TextName;
      DescriptionEdit.Text := TextDesc;
    end;
    EnableManifest(UseXPManifestCheckBox.Checked);
    // Icon
    AStream := TProjectIcon(ProjResources[TProjectIcon]).GetStream;
    try
      SetIconFromStream(AStream);
    finally
      AStream.Free;
    end;
    fIconChanged := False;
  end;
end;

procedure TProjectApplicationOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  AStream: TStream;
begin
  with (AOptions as TProjectIDEOptions).Project do
  begin
    Title := TitleEdit.Text;
    Scaled := UseLCLScalingCheckBox.Checked;
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
      DpiAware := TXPManifestDpiAware(DpiAwareComboBox.ItemIndex);
      ExecutionLevel := TXPManifestExecutionLevel(ExecutionLevelComboBox.ItemIndex);
      UIAccess := UIAccessCheckBox.Checked;
      LongPathAware := LongPathCheckBox.Checked;
      TextName := NameEdit.Text;
      TextDesc := DescriptionEdit.Text;
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


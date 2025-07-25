unit project_application_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, System.UITypes,
  // LazUtils
  FileUtil,
  // LCL
  LCLType, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, ExtDlgs,
  LCLStrConsts,
  // LazControls
  DividerBevel,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, LazIDEIntf, IDEImagesIntf, IDEDialogs,
  // IdeUtils
  IdeUtilsPkgStrConsts,
  // IdeConfig
  CompilerOptions,
  // IdeProject
  IdeProjectStrConsts,
  // IDE
  LazarusIDEStrConsts, Project, ProjectIcon, ApplicationBundle, W32Manifest;

type

  { TProjectApplicationOptionsFrame }

  TProjectApplicationOptionsFrame = class(TAbstractIDEOptionsEditor)
    AppSettingsGroupBox: TGroupBox;
    DefaultIconButton: TBitBtn;
    LblNSPrincipalClass: TLabel;
    EdNSPrincipalClass: TEdit;
    LongPathCheckBox: TCheckBox;
    DarwinDividerBevel: TDividerBevel;
    AnsiUTF8CheckBox: TCheckBox;
    NameEdit: TEdit;
    DescriptionEdit: TEdit;
    NameLabel: TLabel;
    DescriptionLabel: TLabel;
    IconBtnsPanel: TPanel;
    UseLCLScalingCheckBox: TCheckBox;
    CreateAppBundleButton: TBitBtn;
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
    procedure UseAppBundleCheckBoxChange(Sender: TObject);
    procedure UseXPManifestCheckBoxChange(Sender: TObject);
  private
    FProject: TProject;
    fIconChanged: boolean;
    procedure EnableManifest(aEnable: Boolean);
    procedure SetIconFromStream(Value: TStream);
    function GetIconAsStream: TStream;
    function HasIcon: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
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

  if not (CreateApplicationBundle(TargetExeName, AProject.GetTitle, True, AProject) in
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

constructor TProjectApplicationOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DARWIN}
  SavePictureDialog1.DefaultExt := '.icns';
  SavePictureDialog1.FilterIndex := 7;
  {$ELSE}
  SavePictureDialog1.DefaultExt := '.ico';
  SavePictureDialog1.FilterIndex := 6;
  {$ENDIF}
  OpenPictureDialog1.DefaultExt := SavePictureDialog1.DefaultExt;
end;

function TProjectApplicationOptionsFrame.HasIcon: Boolean;
begin
  Result := (IconImage.Picture.Graphic <> nil) and
    (not IconImage.Picture.Graphic.Empty);
end;

procedure TProjectApplicationOptionsFrame.IconImagePictureChanged(Sender: TObject);
var
  cx, cy: integer;
begin
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
    IconImage.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    fIconChanged:=true;
  except
    on E: Exception do
      IDEMessageDialog(lisCCOErrorCaption, E.Message, mtError, [mbOK]);
  end;
end;

procedure TProjectApplicationOptionsFrame.SaveIconButtonClick(Sender: TObject);
var
  ext: String;

  procedure SaveAllImages;
  var
    savedCurrent: Integer;
    fn: String;
    pf: TPixelFormat;
    w, h: Word;
    i: Integer;
  begin
    savedCurrent := IconImage.Picture.Icon.Current;
    try
      fn := ChangeFileExt(SavePictureDialog1.FileName, '');
      for i := 0 to IconImage.Picture.Icon.Count-1 do
      begin
        IconImage.Picture.Icon.Current := i;
        IconImage.Picture.Icon.GetDescription(i, pf, h, w);  // order h-w is correct
        IconImage.Picture.SaveToFile(Format('%s_%dx%d.%s', [fn, w, h, ext]));
      end;
    finally
      IconImage.Picture.Icon.Current := savedCurrent;
    end;
  end;

var
  res: TModalResult;
  selIdx: Integer;
  info: String;
  options: array[0..1] of string;
begin
  if not HasIcon then
    exit;

  if SavePictureDialog1.Execute then
  begin
    ext := ExtractFileExt(SavePictureDialog1.FileName);
    Delete(ext, 1, 1);
    // Save as icon
    try
      if SameText(ext, 'ico') or SameText(ext, 'icns') or SameText(ext, 'cur') then
        IconImage.Picture.SaveToFile(SavePictureDialog1.FileName)
      else
      begin
        // Save as another format allowing only a single image per file
        info := lisMultipleImagesInfo + LineEnding + lisHowToProceed;
        options[0] := lisSaveOnlyCurrentImageSize;
        options[1] := lisSaveAllImageSizesToIndividualFiles;
        res := TaskDlg(rsMtInformation, lisSaveIconToFile, info, tdiQuestion, [mbOK, mbCancel], options, selIdx);
        if res = mrOK then
          case selIdx of
            0: IconImage.Picture.SaveToFile(SavePictureDialog1.FileName);
            1: SaveAllImages;
          end;
      end;
    except
      on E: Exception do
        IDEMessageDialog(rsMtError, E.Message, mtError, [mbCancel]);
    end;
  end;
end;

procedure TProjectApplicationOptionsFrame.UseAppBundleCheckBoxChange(
  Sender: TObject);
begin
  EdNSPrincipalClass.Enabled := UseAppBundleCheckBox.Checked;
end;

procedure TProjectApplicationOptionsFrame.EnableManifest(aEnable: Boolean);
begin
  DpiAwareLabel.Enabled := aEnable;
  DpiAwareComboBox.Enabled := aEnable;
  ExecutionLevelLabel.Enabled := aEnable;
  ExecutionLevelComboBox.Enabled := aEnable;
  UIAccessCheckBox.Enabled := aEnable;
  LongPathCheckBox.Enabled := aEnable;
  AnsiUTF8CheckBox.Enabled := aEnable;
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

// NOTE: This creates an instance of TMemoryStream which must be destroyed by
// the calling routine.
function TProjectApplicationOptionsFrame.GetIconAsStream: TStream;
begin
  Result := nil;
  if HasIcon then
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
  LblNSPrincipalClass.Caption := dlgNSPrincipalClass;

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
  AnsiUTF8CheckBox.Caption := dlgPOAnsiUTF8;
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
  IDEImages.AssignImage(LoadIconButton, 'laz_open');
  SaveIconButton.LoadGlyphFromStock(idButtonSave);
  IDEImages.AssignImage(DefaultIconButton, 'restore_default');
  IDEImages.AssignImage(SaveIconButton, 'laz_save');
  IDEImages.AssignImage(ClearIconButton, 'menu_clean');
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
    if TProjectIDEOptions(AOptions).LclApp then
      UseLCLScalingCheckBox.Checked := Scaled
    else
      UseLCLScalingCheckBox.Enabled := False; // Disable for a console program.
    UseAppBundleCheckBox.Checked := UseAppBundle;
    EdNSPrincipalClass.Text := NSPrincipalClass;
    // Manifest
    with ProjResources.XPManifest do
    begin
      UseXPManifestCheckBox.Checked := UseManifest;
      DpiAwareComboBox.ItemIndex := Ord(DpiAware);
      ExecutionLevelComboBox.ItemIndex := Ord(ExecutionLevel);
      UIAccessCheckBox.Checked := UIAccess;
      LongPathCheckBox.Checked := LongPathAware;
      AnsiUTF8CheckBox.Checked := AnsiUTF8;
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
    NSPrincipalClass := EdNSPrincipalClass.Text;
    with ProjResources.XPManifest do
    begin
      UseManifest := UseXPManifestCheckBox.Checked;
      DpiAware := TXPManifestDpiAware(DpiAwareComboBox.ItemIndex);
      ExecutionLevel := TXPManifestExecutionLevel(ExecutionLevelComboBox.ItemIndex);
      UIAccess := UIAccessCheckBox.Checked;
      LongPathAware := LongPathCheckBox.Checked;
      AnsiUTF8 := AnsiUTF8CheckBox.Checked;
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


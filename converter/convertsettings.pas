{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Juha Manninen

  Abstract:
    Settings form for Delphi conversion. Used for unit, project and package conversion.
}
unit ConvertSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, Buttons, ButtonPanel, ComCtrls,
  // LazUtils
  FileUtil, LazFileUtils, DividerBevel,
  // CodeTools
  CodeToolManager,
  // IdeIntf
  IdeIntfStrConsts, IDEImagesIntf, IDEMsgIntf, LazIDEIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts,
  // Converter
  ConvertBase, MissingUnits, ReplaceNamesUnit, ReplaceFuncsUnit;

type

  { TConvertSettingsGui }

  TConvertSettingsGui = class(TConvertSettings)
  private
  public
    constructor Create(const ATitle: string);
    destructor Destroy; override;
    function RunSettingsForm(ACacheUnitsThread: TCacheUnitsThread): TModalResult; override;
  end;

  { TConvertSettingsForm }

  TConvertSettingsForm = class(TForm)
    BackupCheckBox: TCheckBox;
    DelphiDefineCheckBox: TCheckBox;
    FuncReplaceCommentCB: TCheckBox;
    ScanParentDirCheckBox: TCheckBox;
    OtherOptGroupBox: TGroupBox;
    InputPathLabel: TLabel;
    InputPathListBox: TListBox;
    KeepFileOpenCheckBox: TCheckBox;
    StopScanButton: TBitBtn;
    CoordOffsComboBox: TComboBox;
    ScanLabel: TLabel;
    ScanProgressBar: TProgressBar;
    UnitReplaceComboBox: TComboBox;
    CrossPlatformCheckBox: TCheckBox;
    SameDfmCheckBox: TCheckBox;
    SupportDelphiCheckBox: TCheckBox;
    TargetGroupBox: TGroupBox;
    FuncReplaceComboBox: TComboBox;
    TypeReplaceComboBox: TComboBox;
    UnknownPropsComboBox: TComboBox;
    UnknownPropsDivider: TDividerBevel;
    UnitReplaceDivider: TDividerBevel;
    TypeReplaceDivider: TDividerBevel;
    FuncReplaceDivider: TDividerBevel;
    CoordOffsDivider: TDividerBevel;
    FuncReplaceButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    TypeReplaceButton: TBitBtn;
    UnitReplaceButton: TBitBtn;
    CoordOffsButton: TBitBtn;
    procedure SameDfmCheckBoxChange(Sender: TObject);
    procedure ScanParentDirCheckBoxClick(Sender: TObject);
    procedure StopScanButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SupportDelphiCheckBoxChange(Sender: TObject);
    procedure TypeReplaceButtonClick(Sender: TObject);
    procedure FuncReplaceButtonClick(Sender: TObject);
    procedure CoordOffsButtonClick(Sender: TObject);
    procedure UnitReplaceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fSettings: TConvertSettings;
    fCacheUnitsThread: TCacheUnitsThread;
    fThreadStarted: Boolean;
    procedure StartThreadIfValid;
    procedure ThreadGuiShow(aRunning: Boolean);
    procedure ThreadTerminated(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ASettings: TConvertSettings); reintroduce;
    destructor Destroy; override;
  end;


implementation

{$R *.lfm}

{ TConvertSettingsGui }

constructor TConvertSettingsGui.Create(const ATitle: string);
begin
  inherited Create(ATitle);
end;

destructor TConvertSettingsGui.Destroy;
begin
  inherited Destroy;
end;

function TConvertSettingsGui.RunSettingsForm(ACacheUnitsThread: TCacheUnitsThread
  ): TModalResult;
var
  SettingsForm: TConvertSettingsForm;
begin
  SettingsForm:=TConvertSettingsForm.Create(nil, Self);
  try
    Load;   // Load settings from ConfigStorage.
    with SettingsForm do
    begin
      Caption:=Title + ' - ' + ExtractFileName(MainFilename);
      InputPathListBox.Items.Assign(MainFilenames);
      // Settings --> UI. Loaded from ConfigSettings earlier.
      CrossPlatformCheckBox.Checked  :=CrossPlatform;
      SupportDelphiCheckBox.Checked  :=SupportDelphi;
      SameDfmCheckBox.Checked        :=SameDfmFile;
      DelphiDefineCheckBox.Checked   :=DelphiDefine;
      BackupCheckBox.Checked         :=BackupFiles;
      KeepFileOpenCheckBox.Checked   :=KeepFileOpen;
      ScanParentDirCheckBox.Checked  :=ScanParentDir;
      FuncReplaceCommentCB.Checked   :=FuncReplaceComment;
      UnitReplaceComboBox.ItemIndex  :=integer(UnitsReplaceMode);
      UnknownPropsComboBox.ItemIndex :=integer(PropReplaceMode);
      TypeReplaceComboBox.ItemIndex  :=integer(TypeReplaceMode);
      FuncReplaceComboBox.ItemIndex  :=integer(FuncReplaceMode);
      CoordOffsComboBox.ItemIndex    :=integer(CoordOffsMode);
      SupportDelphiCheckBoxChange(SupportDelphiCheckBox);
      SameDfmCheckBoxChange(SameDfmCheckBox);

      fCacheUnitsThread := ACacheUnitsThread;
      StartThreadIfValid;
      Result:=ShowModal;   // Let the user change the settings.
      if Result=mrOK then  // The thread will be finished before the form closes.
      begin
        // UI --> Settings. Will be saved to ConfigSettings later.
        CrossPlatform    :=CrossPlatformCheckBox.Checked;
        SupportDelphi    :=SupportDelphiCheckBox.Checked;
        SameDfmFile      :=SameDfmCheckBox.Checked;
        DelphiDefine     :=DelphiDefineCheckBox.Checked;
        BackupFiles      :=BackupCheckBox.Checked;
        KeepFileOpen     :=KeepFileOpenCheckBox.Checked;
        ScanParentDir    :=ScanParentDirCheckBox.Checked;
        FuncReplaceComment:=FuncReplaceCommentCB.Checked;
        UnitsReplaceMode :=TReplaceModeLong(UnitReplaceComboBox.ItemIndex);
        PropReplaceMode  :=TReplaceModeLong(UnknownPropsComboBox.ItemIndex);
        TypeReplaceMode  :=TReplaceModeAllow(TypeReplaceComboBox.ItemIndex);
        FuncReplaceMode  :=TReplaceModeShort(FuncReplaceComboBox.ItemIndex);
        CoordOffsMode    :=TReplaceModeShort(CoordOffsComboBox.ItemIndex);
        if BackupFiles then
          DeleteDirectory(BackupPath, True); // Delete old backup if there is any.
        Save;
      end;
    end;
  finally
    FreeAndNil(SettingsForm);
  end;
end;


{ TConvertSettingsForm }

constructor TConvertSettingsForm.Create(AOwner: TComponent; ASettings: TConvertSettings);
begin
  inherited Create(AOwner);
  fSettings:=ASettings;
end;

destructor TConvertSettingsForm.Destroy;
begin
  inherited Destroy;
end;

procedure TConvertSettingsForm.FormCreate(Sender: TObject);
begin
  InputPathLabel.Caption:=lisToFPCPath; // Reuse a string form options page.
  InputPathListBox.Clear;
  InputPathListBox.Hint:=lisProjectPathHint;
  // Target
  TargetGroupBox.Caption:=lisConvertTarget;
  TargetGroupBox.Hint:=lisConvertTargetHint;
  CrossPlatformCheckBox.Caption:=lisConvertTargetCrossPlatform;
  CrossPlatformCheckBox.Hint:=lisConvertTargetCrossPlatformHint;
  SupportDelphiCheckBox.Caption:=lisConvertTargetSupportDelphi;
  SupportDelphiCheckBox.Hint:=lisConvertTargetSupportDelphiHint;
  SameDfmCheckBox.Caption:=lisConvertTargetSameDfmFile;
  SameDfmCheckBox.Hint:=lisConvertTargetSameDfmFileHint;
  // Other
  OtherOptGroupBox.Caption:=lisCEOtherGroup;
  OtherOptGroupBox.Hint:=lisConvertOtherHint;
  DelphiDefineCheckBox.Caption:=lisAddDelphiDefine;
  DelphiDefineCheckBox.Hint:=lisAddDelphiDefineHint;
  BackupCheckBox.Caption:=lisBackupChangedFiles;
  BackupCheckBox.Hint:=lisBackupHint;
  KeepFileOpenCheckBox.Caption:=lisKeepFileOpen;
  KeepFileOpenCheckBox.Hint:=lisKeepFileOpenHint;
  ScanParentDirCheckBox.Caption:=lisScanFilesInParentDir;
  ScanParentDirCheckBox.Hint:=lisScanFilesInParentDirHint;
  // File system scanning
  ScanLabel.Caption := lisScanParentDir;
  StopScanButton.Caption:=lisStop;
  IDEImages.AssignImage(StopScanButton, 'menu_stop');
  // Unit Replacements
  UnitReplaceDivider.Caption:=lisConvUnitReplacements;
  UnitReplaceButton.Caption:=lisEdit;    // Recycled string.
  IDEImages.AssignImage(UnitReplaceButton, 'laz_edit');
  UnitReplaceDivider.Hint:=lisConvUnitReplHint;
  UnitReplaceButton.Hint:=lisConvUnitReplHint;
  UnitReplaceComboBox.Items.Add(lisDisabled);    // 'Disabled'
  UnitReplaceComboBox.Items.Add(lisInteractive); // 'Interactive'
  UnitReplaceComboBox.Items.Add(lisAutomatic);   // 'Automatic'
  // Unknown Properties
  UnknownPropsDivider.Caption:=lisConvUnknownProps;
  UnknownPropsComboBox.Items.Add(lisDisabled);
  UnknownPropsComboBox.Items.Add(lisInteractive);
  UnknownPropsComboBox.Items.Add(lisAutomatic);
  // Type Replacements
  TypeReplaceDivider.Caption:=lisConvTypeReplacements;
  TypeReplaceButton.Caption:=lisEdit;
  IDEImages.AssignImage(TypeReplaceButton, 'laz_edit');
  TypeReplaceDivider.Hint:=lisConvTypeReplHint;
  TypeReplaceButton.Hint:=lisConvTypeReplHint;
  TypeReplaceComboBox.Items.Add(lisInteractive);
  TypeReplaceComboBox.Items.Add(lisAutomatic);
  // Func Replacements
  FuncReplaceDivider.Caption:=lisConvFuncReplacements;
  FuncReplaceButton.Caption:=lisEdit;
  IDEImages.AssignImage(FuncReplaceButton, 'laz_edit');
  FuncReplaceDivider.Hint:=lisConvFuncReplHint;
  FuncReplaceButton.Hint:=lisConvFuncReplHint;
  FuncReplaceComboBox.Items.Add(lisDisabled);
  FuncReplaceComboBox.Items.Add(lisEnabled);
  FuncReplaceCommentCB.Caption:=lisConvAddCommentAfterReplacement;
  // Coordinate Offsets
  CoordOffsDivider.Caption:=lisConvCoordOffs;
  CoordOffsButton.Caption:=lisEdit;
  IDEImages.AssignImage(CoordOffsButton, 'laz_edit');
  CoordOffsDivider.Hint:=lisConvCoordHint;
  CoordOffsButton.Hint:=lisConvCoordHint;
  CoordOffsComboBox.Items.Add(lisDisabled);
  CoordOffsComboBox.Items.Add(lisEnabled);

  ButtonPanel1.OKButton.Caption:=lisStartConversion;
end;

procedure TConvertSettingsForm.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TConvertSettingsForm.SupportDelphiCheckBoxChange(Sender: TObject);
var
  Chk: Boolean;
begin
  Chk:=(Sender as TCheckBox).Checked;
  SameDfmCheckBox.Enabled:=Chk;
  if not Chk then
    SameDfmCheckBox.Checked:=Chk;
end;

procedure TConvertSettingsForm.SameDfmCheckBoxChange(Sender: TObject);
var
  Chk: Boolean;
begin
  Chk:=(Sender as TCheckBox).Checked;
  if Chk then
    CoordOffsComboBox.ItemIndex:=integer(rsDisabled);
  CoordOffsComboBox.Enabled:=not Chk;
end;

procedure TConvertSettingsForm.ScanParentDirCheckBoxClick(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
    StartThreadIfValid;
end;

procedure TConvertSettingsForm.StartThreadIfValid;
begin
  if ScanParentDirCheckBox.Checked and Assigned(fCacheUnitsThread) then
  begin
    ThreadGuiShow(True);
    fCacheUnitsThread.OnTerminate:=@ThreadTerminated;
    fCacheUnitsThread.Start;
    fThreadStarted := True;
  end
  else
    ThreadGuiShow(False);          // Hide controls dealing with scanning
end;

procedure TConvertSettingsForm.StopScanButtonClick(Sender: TObject);
begin
  fCacheUnitsThread.Searcher.Stop; // Terminate;
end;

procedure TConvertSettingsForm.CancelButtonClick(Sender: TObject);
begin
  if Assigned(fCacheUnitsThread) and fThreadStarted then
  begin
    fCacheUnitsThread.Searcher.Stop;
    fCacheUnitsThread.WaitFor;
  end;
end;

procedure TConvertSettingsForm.ThreadGuiShow(aRunning: Boolean);
begin
  ScanLabel.Visible := aRunning;
  ScanProgressBar.Visible := aRunning;
  StopScanButton.Visible := aRunning;
  // These are disabled while thread is running
  ButtonPanel1.OKButton.Enabled := not aRunning;
  ScanParentDirCheckBox.Enabled := not aRunning;
end;

procedure TConvertSettingsForm.ThreadTerminated(Sender: TObject);
begin
  ThreadGuiShow(False);
end;

// Edit replacements in grids

procedure TConvertSettingsForm.UnitReplaceButtonClick(Sender: TObject);
begin
  EditMap(fSettings.ReplaceUnits, lisConvUnitsToReplace);
end;

procedure TConvertSettingsForm.TypeReplaceButtonClick(Sender: TObject);
begin
  EditMap(fSettings.ReplaceTypes, lisConvTypesToReplace);
end;

procedure TConvertSettingsForm.FuncReplaceButtonClick(Sender: TObject);
begin
  EditFuncReplacements(fSettings.ReplaceFuncs, lisConvFuncsToReplace);
end;

procedure TConvertSettingsForm.CoordOffsButtonClick(Sender: TObject);
begin
  EditCoordOffsets(fSettings.CoordOffsets, lisConvCoordOffs);
end;

end.


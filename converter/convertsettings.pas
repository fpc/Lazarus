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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Juha Manninen

  Abstract:
    Settings for ConvertDelphi unit. Used for unit, project and package conversion.
}
unit ConvertSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, Buttons, ExtCtrls, DialogProcs, CodeToolsStructs,
  ReplaceNamesUnit, LazarusIDEStrConsts;

type

  TConvertTarget = (ctLazarus, ctLazarusWin, ctLazarusAndDelphi);

  { TConvertSettings }

  TConvertSettings = class
  private
    fTitle: String;       // Used for form caption.
    // Unit, Project or Package top file and path.
    fMainFilename: String;
    fMainPath: String;
    // Actual user settings.
    fBackupFiles: boolean;
    fTarget: TConvertTarget;
    fSameDFMFile: boolean;
    fAutoRemoveProperties: boolean;
    fAutoConvertTypes: boolean;
    // Delphi units mapped to Lazarus units, will be replaced or removed.
    fReplaceUnits: TStringToStringTree;
    // Delphi types mapped to Lazarus types, will be replaced.
    fReplaceTypes: TStringToStringTree;

    function GetBackupPath: String;
    procedure SetMainFilename(const AValue: String);
  public
    constructor Create(const ATitle: string);
    destructor Destroy; override;
    function RunForm: TModalResult;

    // Lazarus file name based on Delphi file name, keep suffix.
    function DelphiToLazFilename(const DelphiFilename: string;
      LowercaseFilename: boolean): string; overload;
    // Lazarus file name based on Delphi file name with new suffix.
    function DelphiToLazFilename(const DelphiFilename, LazExt: string;
      LowercaseFilename: boolean): string; overload;

    // Create Lazarus file name and copy/rename from Delphi file, keep suffix.
    function RenameDelphiToLazFile(const DelphiFilename: string;
      out LazFilename: string; LowercaseFilename: boolean): TModalResult; overload;
    // Create Lazarus file name and copy/rename from Delphi file with new suffix.
    function RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
      out LazFilename: string; LowercaseFilename: boolean): TModalResult; overload;

    function RenameFile(const SrcFilename, DestFilename: string): TModalResult;
    function BackupFile(const AFilename: string): TModalResult;
  public
    property MainFilename: String read fMainFilename write SetMainFilename;
    property MainPath: String read fMainPath;
    property BackupPath: String read GetBackupPath;

    property BackupFiles: boolean read fBackupFiles;
    property Target: TConvertTarget read fTarget;
    property SameDFMFile: boolean read fSameDFMFile;
    property AutoRemoveProperties: boolean read fAutoRemoveProperties;
    property AutoConvertTypes: boolean read fAutoConvertTypes;
    property ReplaceUnits: TStringToStringTree read fReplaceUnits;
    property ReplaceTypes: TStringToStringTree read fReplaceTypes;
  end;


  { TConvertSettingsForm }

  TConvertSettingsForm = class(TForm)
    BackupCheckBox: TCheckBox;
    TypeReplacementsButton: TBitBtn;
    SameDFMCheckBox: TCheckBox;
    ProjectPathEdit: TLabeledEdit;
    TargetRadioGroup: TRadioGroup;
    UnitReplacementsButton: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    BtnPanel: TPanel;
    HelpButton: TBitBtn;
    SettingsGroupBox: TGroupBox;
    MissingStuffGroupBox: TGroupBox;
    MissingStuffLabel: TLabel;
    AutoConvertTypesCheckBox: TCheckBox;
    AutoRemovePropertiesCheckBox: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure TypeReplacementsButtonClick(Sender: TObject);
    procedure UnitReplacementsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TargetRadioGroupClick(Sender: TObject);
  private
    fSettings: TConvertSettings;
  public
    constructor Create(AOwner: TComponent; ASettings: TConvertSettings); reintroduce;
    destructor Destroy; override;
  end; 

var
  ConvertSettingsForm: TConvertSettingsForm;

implementation


{$R *.lfm}

{ TConvertSettings }

constructor TConvertSettings.Create(const ATitle: string);
begin
  fTitle:=ATitle;
  fMainFilename:='';
  fMainPath:='';
  // Now hard-code some values. Later move them to a config file.
  // Map Delphi units to Lazarus units.
  fReplaceUnits:=TStringToStringTree.Create(false);
  fReplaceUnits['Windows']:='LCLIntf, LCLType, LMessages';
  fReplaceUnits['Variants']:='';
  fReplaceUnits['TntActnList']:='ActnList';
  fReplaceUnits['TntMenus']:='Menus';
  fReplaceUnits['TntClasses']:='Classes';
  fReplaceUnits['TntForms']:='Forms';
  fReplaceUnits['TntComCtrls']:='ComCtrls';
  fReplaceUnits['TntStdCtrls']:='StdCtrls';
  fReplaceUnits['TntExtCtrls']:='ExtCtrls';
  fReplaceUnits['TntSysUtils']:='SysUtils';
  fReplaceUnits['pngImage']:='';
  fReplaceUnits['Jpeg']:='';
  fReplaceUnits['gifimage']:='';

  // Map Delphi types to LCL types.
  fReplaceTypes:=TStringToStringTree.Create(false);
  fReplaceTypes['TFlowPanel']:='TPanel';
  fReplaceTypes['TGridPanel']:='TPanel';
  fReplaceTypes['TComboBoxEx']:='TComboBox';
  fReplaceTypes['TCoolBar']:='TPanel';
  fReplaceTypes['TControlBar']:='TPanel';
  fReplaceTypes['TValueListEditor']:='TStringGrid';
  fReplaceTypes['TRichEdit']:='TMemo';
  fReplaceTypes['TDBRichEdit']:='TDBMemo';
  fReplaceTypes['TApplicationEvents']:='TApplicationProperties';
  fReplaceTypes['TPNGObject']:='TPortableNetworkGraphic';
  fReplaceTypes['TTnt(.+)']:='T$1';

end;

destructor TConvertSettings.Destroy;
begin
  fReplaceTypes.Free;
  fReplaceUnits.Free;
  inherited Destroy;
end;

function TConvertSettings.RunForm: TModalResult;
var
  SettingsForm: TConvertSettingsForm;
begin
  SettingsForm:=TConvertSettingsForm.Create(nil, Self);
  with SettingsForm do
  try
    Caption:=fTitle;
    ProjectPathEdit.Text:=fMainPath;
{
    // ToDo: Load from XML.
    // Settings --> UI.
    BackupCheckBox.Checked          :=fBackupFiles;
    TargetRadioGroup.ItemIndex      :=integer(fTarget);
    SameDFMCheckBox.Checked         :=fSameDFMFile;
    AutoRemovePropertiesCheckBox.Checked :=fAutoRemoveProperties;
    AutoConvertTypesCheckBox.Checked:=fAutoConvertTypes;
}
    Result:=ShowModal;
    if Result=mrOK then begin
      // UI --> Settings.
      fBackupFiles         :=BackupCheckBox.Checked;
      fTarget              :=TConvertTarget(TargetRadioGroup.ItemIndex);
      fSameDFMFile         :=SameDFMCheckBox.Checked;
      fAutoRemoveProperties:=AutoRemovePropertiesCheckBox.Checked;
      fAutoConvertTypes    :=AutoConvertTypesCheckBox.Checked;
      // ToDo: Save to XML.
    end;
  finally
    Free;
  end;
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename: string;
                                              LowercaseFilename: boolean): string;
begin
  Result:=DelphiToLazFilename(DelphiFilename,'',LowercaseFilename);
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename, LazExt: string;
                                              LowercaseFilename: boolean): string;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, fMainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  Result:=fMainPath+SubPath+fn+LazExt;
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename: string;
  out LazFilename: string; LowercaseFilename: boolean): TModalResult;
begin
  Result:=RenameDelphiToLazFile(DelphiFilename,'',LazFilename,LowercaseFilename);
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
  out LazFilename: string; LowercaseFilename: boolean): TModalResult;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, fMainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  // Rename in the same directory.
  if fBackupFiles then begin
    Result:=BackupFile(DelphiFilename); // Save before rename.
    if Result<>mrOK then exit;
  end;
  LazFilename:=fMainPath+SubPath+fn+LazExt;
  Result:=RenameFileWithErrorDialogs(DelphiFilename,LazFilename,[mbAbort]);
end;

function TConvertSettings.RenameFile(const SrcFilename, DestFilename: string): TModalResult;
begin
//  Result:=mrOK;
  if fBackupFiles then
    BackupFile(SrcFilename); // Save before rename.
  Result:=RenameFileWithErrorDialogs(SrcFilename,DestFilename,[mbAbort]);
end;

function TConvertSettings.BackupFile(const AFilename: string): TModalResult;
var
  bp, fn: String;
begin
  bp:=BackupPath;
  fn:=ExtractFileName(AFilename);
  Result:=CopyFileWithErrorDialogs(AFilename,bp+fn,[mbAbort]);
end;

procedure TConvertSettings.SetMainFilename(const AValue: String);
begin
  fMainFilename:=AValue;
  fMainPath:=ExtractFilePath(AValue);
end;

function TConvertSettings.GetBackupPath: String;
const
  BackupPathName='ConverterBackup';
begin
  Result:='';
  if fBackupFiles then begin
    Result:=fMainPath+BackupPathName+PathDelim;
    // Create backup path if needed.
    if not DirectoryExistsUTF8(Result) then
      CreateDirUTF8(Result);
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
const                // Move later to resourcestrings
  lisUseSameDFMFile = 'Use the same DFM file for Lazarus (ToDo...)';
begin
  ProjectPathEdit.Text:='';
  ProjectPathEdit.EditLabel.Caption:=lisProjectPath;
  BackupCheckBox.Caption:=lisBackupChangedFiles;
  btnOK.Caption:=lisStartConversion;
  TargetRadioGroup.Items.Clear;
  TargetRadioGroup.Items.Append(lisConvertTarget1);
  TargetRadioGroup.Items.Append(lisConvertTarget2);
  TargetRadioGroup.Items.Append(lisConvertTarget3);
  TargetRadioGroup.ItemIndex:=0;
  SameDFMCheckBox.Caption:=lisUseSameDFMFile;
  MissingStuffGroupBox.Caption:= lisConvUnitsTypesProperties;
  AutoRemovePropertiesCheckBox.Caption:=lisConvAutoRemoveProperties;
  UnitReplacementsButton.Caption:=lisConvUnitReplacements;
  TypeReplacementsButton.Caption:=lisConvTypeReplacements;
  TargetRadioGroupClick(TargetRadioGroup);
end;

procedure TConvertSettingsForm.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TConvertSettingsForm.TargetRadioGroupClick(Sender: TObject);
// Delphi compatibility doesn't allow renaming the form file.
var
  Trg: TConvertTarget;
begin
  Trg:=TConvertTarget((Sender as TRadioGroup).ItemIndex);
  if Trg<>ctLazarusAndDelphi then
    SameDFMCheckBox.Checked:=false;
  SameDFMCheckBox.Enabled:=false; //Trg=ctLazarusAndDelphi;
end;

procedure TConvertSettingsForm.UnitReplacementsButtonClick(Sender: TObject);
var
  ReplaceNamesForm: TReplaceNamesForm;
begin
  ReplaceNamesForm:=TReplaceNamesForm.Create(nil);
  try
    ReplaceNamesForm.Caption:=lisConvUnitsToReplace;
    CopyFromMapToGrid(ReplaceNamesForm.NamePairGrid, fSettings.ReplaceUnits);
    if ReplaceNamesForm.ShowModal=mrOK then
      CopyFromGridToMap(ReplaceNamesForm.NamePairGrid, fSettings.ReplaceUnits);
  finally
    ReplaceNamesForm.Free;
  end;
end;

procedure TConvertSettingsForm.TypeReplacementsButtonClick(Sender: TObject);
var
  ReplaceNamesForm: TReplaceNamesForm;
begin
  ReplaceNamesForm:=TReplaceNamesForm.Create(nil);
  try
    ReplaceNamesForm.Caption:=lisConvTypesToReplace;
    CopyFromMapToGrid(ReplaceNamesForm.NamePairGrid, fSettings.ReplaceTypes);
    if ReplaceNamesForm.ShowModal=mrOK then
      CopyFromGridToMap(ReplaceNamesForm.NamePairGrid, fSettings.ReplaceTypes);
  finally
    ReplaceNamesForm.Free;
  end;
end;

procedure TConvertSettingsForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;


end.


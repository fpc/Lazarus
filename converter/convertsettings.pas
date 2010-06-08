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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, IDEProcs,
  StdCtrls, EditBtn, Buttons, ExtCtrls, DialogProcs, LazarusIDEStrConsts,
  CodeToolsStructs, AVL_Tree, BaseIDEIntf, LazConfigStorage,
  ButtonPanel, ReplaceNamesUnit;

type

  TConvertTarget = (ctLazarus, ctLazarusWin, ctLazarusAndDelphi);

  { TConvertSettings }

  TConvertSettings = class
  private
    fTitle: String;       // Used for form caption.
    // Unit, Project or Package top file and path.
    fMainFilename: String;
    fMainPath: String;
    // Persistent storage in XML or some other format.
    fConfigStorage: TConfigStorage;
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
    // Getter / setter:
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
    ButtonPanel: TButtonPanel;
    TypeReplacementsButton: TBitBtn;
    SameDFMCheckBox: TCheckBox;
    ProjectPathEdit: TLabeledEdit;
    TargetRadioGroup: TRadioGroup;
    UnitReplacementsButton: TBitBtn;
    SettingsGroupBox: TGroupBox;
    MissingStuffGroupBox: TGroupBox;
    MissingStuffLabel: TLabel;
    AutoConvertTypesCheckBox: TCheckBox;
    AutoRemovePropertiesCheckBox: TCheckBox;
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

procedure LoadStringToStringTree(Config: TConfigStorage; const Path: string;
  Tree: TStringToStringTree);
var
  Cnt: LongInt;
  SubPath: String;
  CurName: String;
  CurValue: String;
  i: Integer;
begin
  Tree.Clear;
  Cnt:=Config.GetValue(Path+'Count',0);
  for i:=0 to Cnt-1 do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    CurName:=Config.GetValue(SubPath+'Name','');
    CurValue:=Config.GetValue(SubPath+'Value','');
    Tree[CurName]:=CurValue;
  end;
end;

procedure SaveStringToStringTree(Config: TConfigStorage; const Path: string;
  Tree: TStringToStringTree);
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
  i, j: Integer;
  SubPath: String;
begin
  Config.SetDeleteValue(Path+'Count',Tree.Tree.Count,0);
  Node:=Tree.Tree.FindLowest;
  i:=0;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    Config.SetDeleteValue(SubPath+'Name',Item^.Name,'');
    Config.SetDeleteValue(SubPath+'Value',Item^.Value,'');
    Node:=Tree.Tree.FindSuccessor(Node);
    inc(i);
  end;
  for j:=i to i+10 do begin
    SubPath:=Path+'Item'+IntToStr(j)+'/';
    Config.DeletePath(SubPath);
  end;
end;

constructor TConvertSettings.Create(const ATitle: string);
var
  TheMap: TStringToStringTree;

  procedure MapReplacement(ADelphi, ALCL: string);
  begin
    if not TheMap.Contains(ADelphi) then
      TheMap[ADelphi]:=ALCL;
  end;

begin
  fTitle:=ATitle;
  fMainFilename:='';
  fMainPath:='';
  fReplaceUnits:=TStringToStringTree.Create(false);
  fReplaceTypes:=TStringToStringTree.Create(false);
  // Load settings from ConfigStorage.
  fConfigStorage:=GetIDEConfigStorage('delphiconverter.xml', true);
  fBackupFiles          :=fConfigStorage.GetValue('BackupFiles', true);
  fTarget:=TConvertTarget(fConfigStorage.GetValue('ConvertTarget', 0));
  fSameDFMFile          :=fConfigStorage.GetValue('SameDFMFile', false);
  fAutoRemoveProperties :=fConfigStorage.GetValue('AutoRemoveProperties', false);
  fAutoConvertTypes     :=fConfigStorage.GetValue('AutoConvertTypes', false);
  LoadStringToStringTree(fConfigStorage, 'ReplaceUnits', fReplaceUnits);
  LoadStringToStringTree(fConfigStorage, 'ReplaceTypes', fReplaceTypes);

  // Add default values for string maps if ConfigStorage doesn't have them.
  // Map Delphi units to Lazarus units.
  TheMap:=fReplaceUnits;
  MapReplacement('Windows',  'LCLIntf, LCLType, LMessages');
  MapReplacement('Mask',     'MaskEdit');
  MapReplacement('Variants', '');
  MapReplacement('ShellApi', '');
  MapReplacement('pngImage', '');
  MapReplacement('Jpeg',     '');
  MapReplacement('gifimage', '');
  MapReplacement('^Q(.+)',   '$1');               // Kylix unit names.
  MapReplacement('^Tnt(.+)', '$1');               // Tnt* third party components.

  // Map Delphi types to LCL types.
  TheMap:=fReplaceTypes;
  MapReplacement('TFlowPanel',        'TPanel');
  MapReplacement('TGridPanel',        'TPanel');
  MapReplacement('TControlBar',       'TToolBar');
  MapReplacement('TCoolBar',          'TToolBar');
  MapReplacement('TComboBoxEx',       'TComboBox');
  MapReplacement('TValueListEditor',  'TStringGrid');
  MapReplacement('TRichEdit',         'TMemo');
  MapReplacement('TDBRichEdit',       'TDBMemo');
  MapReplacement('TApplicationEvents','TApplicationProperties');
  MapReplacement('TPNGObject',        'TPortableNetworkGraphic');
  MapReplacement('TCxEdit',           'TEdit');
  MapReplacement('^TTnt(.+)',         'T$1');
end;

destructor TConvertSettings.Destroy;
begin
  // Save possibly modified settings to ConfigStorage.
  fConfigStorage.SetDeleteValue('BackupFiles',          fBackupFiles, true);
  fConfigStorage.SetDeleteValue('ConvertTarget',        integer(fTarget), 0);
  fConfigStorage.SetDeleteValue('SameDFMFile',          fSameDFMFile, false);
  fConfigStorage.SetDeleteValue('AutoRemoveProperties', fAutoRemoveProperties, false);
  fConfigStorage.SetDeleteValue('AutoConvertTypes',     fAutoConvertTypes, false);
  SaveStringToStringTree(fConfigStorage, 'ReplaceUnits', fReplaceUnits);
  SaveStringToStringTree(fConfigStorage, 'ReplaceTypes', fReplaceTypes);
  // Free stuff
  fConfigStorage.Free;
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
    // Settings --> UI. Loaded from ConfigSettings earlier.
    BackupCheckBox.Checked               :=fBackupFiles;
    TargetRadioGroup.ItemIndex           :=integer(fTarget);
    SameDFMCheckBox.Checked              :=fSameDFMFile;
    AutoRemovePropertiesCheckBox.Checked :=fAutoRemoveProperties;
    AutoConvertTypesCheckBox.Checked     :=fAutoConvertTypes;
    Result:=ShowModal;         // Let the user change settings in a form.
    if Result=mrOK then begin
      // UI --> Settings. Will be saved to ConfigSettings later.
      fBackupFiles         :=BackupCheckBox.Checked;
      fTarget              :=TConvertTarget(TargetRadioGroup.ItemIndex);
      fSameDFMFile         :=SameDFMCheckBox.Checked;
      fAutoRemoveProperties:=AutoRemovePropertiesCheckBox.Checked;
      fAutoConvertTypes    :=AutoConvertTypesCheckBox.Checked;
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
  ButtonPanel.OKButton.Caption:=lisStartConversion;
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
  RNForm: TReplaceNamesForm;
  GridUpdater: TGridUpdater;
begin
  RNForm:=TReplaceNamesForm.Create(nil);
  GridUpdater:=TGridUpdater.Create(fSettings.ReplaceUnits, RNForm.Grid);
  try
    RNForm.Caption:=lisConvUnitsToReplace;
    GridUpdater.MapToGrid;
    if RNForm.ShowModal=mrOK then
      GridUpdater.GridToMap;
  finally
    GridUpdater.Free;
    RNForm.Free;
  end;
end;

procedure TConvertSettingsForm.TypeReplacementsButtonClick(Sender: TObject);
var
  RNForm: TReplaceNamesForm;
  GridUpdater: TGridUpdater;
begin
  RNForm:=TReplaceNamesForm.Create(nil);
  GridUpdater:=TGridUpdater.Create(fSettings.ReplaceTypes, RNForm.Grid);
  try
    RNForm.Caption:=lisConvTypesToReplace;
    GridUpdater.MapToGrid;
    if RNForm.ShowModal=mrOK then
      GridUpdater.GridToMap;
  finally
    GridUpdater.Free;
    RNForm.Free;
  end;
end;


end.


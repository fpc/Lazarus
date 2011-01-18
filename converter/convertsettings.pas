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
  StdCtrls, EditBtn, Buttons, ExtCtrls, DialogProcs, ButtonPanel,
  LazarusIDEStrConsts, CodeToolsStructs, DividerBevel, AVL_Tree, BaseIDEIntf,
  LazConfigStorage, ConverterTypes, ReplaceNamesUnit, ReplaceFuncsUnit;

type

  TConvertTarget = (ctLazarus, ctLazarusDelphi, ctLazarusWin, ctLazarusDelphiSameDfm);
  TReplaceModeLong = (rlDisabled, rlInteractive, rlAutomatic);
  TReplaceModeShort = (rsDisabled, rsEnabled);

  TConvertSettingsForm = class;

  { TConvertSettings }

  TConvertSettings = class
  private
    fEnabled: Boolean;
    fTitle: String;       // Used for form caption.
    // Unit, Project or Package top file and path.
    fMainFilename: String;
    fMainPath: String;
    // Persistent storage in XML or some other format.
    fConfigStorage: TConfigStorage;
    fSettingsForm: TConvertSettingsForm;
    // Actual user settings.
    fBackupFiles: boolean;
    fKeepFileOpen: boolean;
    fTarget: TConvertTarget;
    // Modes for replacements:
    fUnitsReplaceMode: TReplaceModeLong;
    fUnknownPropsMode: TReplaceModeLong;
    fFuncReplaceMode: TReplaceModeShort;
    fCoordOffsMode: TReplaceModeShort;
    // Delphi units mapped to Lazarus units, will be replaced or removed.
    fReplaceUnits: TStringToStringTree;
    // Delphi types mapped to Lazarus types, will be replaced.
    fReplaceTypes: TStringToStringTree;
    // Delphi global function names mapped to FCL/LCL functions.
    fReplaceFuncs: TFuncsAndCategories;
    // Coordinate offsets of components in a visual container.
    fCoordOffsets: TVisualOffsets;
    // Getter / setter:
    function GetBackupPath: String;
    procedure SetEnabled(const AValue: Boolean);
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
    property Enabled: Boolean read fEnabled write SetEnabled;

    property BackupFiles: boolean read fBackupFiles;
    property KeepFileOpen: boolean read fKeepFileOpen;
    property Target: TConvertTarget read fTarget;
    property UnitsReplaceMode: TReplaceModeLong read fUnitsReplaceMode;
    property UnknownPropsMode: TReplaceModeLong read fUnknownPropsMode;
    property FuncReplaceMode: TReplaceModeShort read fFuncReplaceMode;
    property CoordOffsMode:   TReplaceModeShort read fCoordOffsMode;
    property ReplaceUnits: TStringToStringTree read fReplaceUnits;
    property ReplaceTypes: TStringToStringTree read fReplaceTypes;
    property ReplaceFuncs: TFuncsAndCategories read fReplaceFuncs;
    property CoordOffsets: TVisualOffsets read fCoordOffsets;
  end;


  { TConvertSettingsForm }

  TConvertSettingsForm = class(TForm)
    CoordOffsRadioGroup: TRadioGroup;
    UnitReplaceRadioGroup: TRadioGroup;
    UnknownPropsRadioGroup: TRadioGroup;
    UnknownPropsDivider: TDividerBevel;
    UnitReplaceDivider: TDividerBevel;
    TypeReplaceDivider: TDividerBevel;
    FuncReplaceDivider: TDividerBevel;
    CoordOffsDivider: TDividerBevel;
    FuncReplaceButton: TBitBtn;
    KeepFileOpenCheckBox: TCheckBox;
    TargetRadioGroup: TRadioGroup;
    BackupCheckBox: TCheckBox;
    ButtonPanel1: TButtonPanel;
    TypeReplaceButton: TBitBtn;
    TypeReplaceInfoLabel: TLabel;
    UnitReplaceButton: TBitBtn;
    ProjectPathEdit: TLabeledEdit;
    CoordOffsButton: TBitBtn;
    FuncReplaceRadioGroup: TRadioGroup;
    procedure TypeReplaceButtonClick(Sender: TObject);
    procedure FuncReplaceButtonClick(Sender: TObject);
    procedure CoordOffsButtonClick(Sender: TObject);
    procedure UnitReplaceButtonClick(Sender: TObject);
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

// Load and store configuration in StringToStringTree :

procedure LoadStringToStringTree(Config: TConfigStorage; const Path: string;
  Tree: TStringToStringTree);
var
  SubPath: String;
  CurName, CurValue: String;
  Cnt, i: Integer;
begin
  Tree.Clear;
  Cnt:=Config.GetValue(Path+'Count', 0);
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
  SubPath: String;
  i, j: Integer;
begin
  Config.SetDeleteValue(Path+'Count', Tree.Tree.Count, 0);
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
  // Remove leftover items in case the list has become shorter.
  for j:=i to i+10 do begin
    SubPath:=Path+'Item'+IntToStr(j)+'/';
    Config.DeletePath(SubPath);
  end;
end;

// Load and store configuration in TFuncsAndCategories :

procedure LoadFuncReplacements(Config: TConfigStorage;
  const FuncPath, CategPath: string; aFuncsAndCateg: TFuncsAndCategories);
var
  SubPath: String;
  xCategory, xDelphiFunc, xReplacement, xPackage, xUnitName: String;
  CategUsed: Boolean;
  Cnt, i: Integer;
begin
  aFuncsAndCateg.Clear;
  // Replacement functions
  Cnt:=Config.GetValue(FuncPath+'Count', 0);
  for i:=0 to Cnt-1 do begin
    SubPath:=FuncPath+'Item'+IntToStr(i)+'/';
    xCategory   :=Config.GetValue(SubPath+'Category','');
    xDelphiFunc :=Config.GetValue(SubPath+'DelphiFunction','');
    xReplacement:=Config.GetValue(SubPath+'Replacement','');
    xPackage    :=Config.GetValue(SubPath+'Package','');
    xUnitName   :=Config.GetValue(SubPath+'UnitName','');
    aFuncsAndCateg.AddFunc(xCategory, xDelphiFunc, xReplacement, xPackage, xUnitName);
  end;
  // Categories
  Cnt:=Config.GetValue(CategPath+'Count', 0);
  for i:=0 to Cnt-1 do begin
    SubPath:=CategPath+'Item'+IntToStr(i)+'/';
    xCategory:=Config.GetValue(SubPath+'Name','');
    CategUsed:=Config.GetValue(SubPath+'InUse',True);
    aFuncsAndCateg.AddCategory(xCategory, CategUsed);
  end;
end;

procedure SaveFuncReplacements(Config: TConfigStorage;
  const FuncPath, CategPath: string; aFuncsAndCateg: TFuncsAndCategories);
var
  FuncRepl: TFuncReplacement;
  SubPath, s: String;
  i: Integer;
begin
  // Replacement functions
  Config.SetDeleteValue(FuncPath+'Count', aFuncsAndCateg.Funcs.Count, 0);
  for i:=0 to aFuncsAndCateg.Funcs.Count-1 do begin
    FuncRepl:=aFuncsAndCateg.FuncAtInd(i);
    if FuncRepl<>nil then begin
      SubPath:=FuncPath+'Item'+IntToStr(i)+'/';
      Config.SetDeleteValue(SubPath+'Category'      ,FuncRepl.Category,'');
      Config.SetDeleteValue(SubPath+'DelphiFunction',aFuncsAndCateg.Funcs[i],'');
      Config.SetDeleteValue(SubPath+'Replacement'   ,FuncRepl.ReplClause,'');
      Config.SetDeleteValue(SubPath+'Package'       ,FuncRepl.PackageName,'');
      Config.SetDeleteValue(SubPath+'UnitName'      ,FuncRepl.UnitName,'');
    end;
  end;
  // Remove leftover items in case the list has become shorter.
  for i:=aFuncsAndCateg.Funcs.Count to aFuncsAndCateg.Funcs.Count+10 do begin
    SubPath:=FuncPath+'Item'+IntToStr(i)+'/';
    Config.DeletePath(SubPath);
  end;
  // Categories
  Config.SetDeleteValue(CategPath+'Count', aFuncsAndCateg.Categories.Count, 0);
  for i:=0 to aFuncsAndCateg.Categories.Count-1 do begin
    s:=aFuncsAndCateg.Categories[i];
    if s<>'' then begin
      SubPath:=CategPath+'Item'+IntToStr(i)+'/';
      Config.SetDeleteValue(SubPath+'Name',s,'');
      Config.SetDeleteValue(SubPath+'InUse',aFuncsAndCateg.CategoryIsUsed(i),True);
    end;
  end;
  for i:=aFuncsAndCateg.Categories.Count to aFuncsAndCateg.Categories.Count+10 do begin
    SubPath:=CategPath+'Item'+IntToStr(i)+'/';
    Config.DeletePath(SubPath);
  end;
end;

// Load and store configuration in VisualOffsets :

procedure LoadVisualOffsets(Config: TConfigStorage; const Path: string;
  aVisualOffsets: TVisualOffsets);
var
  ParentType, SubPath: String;
  xTop, xLeft: Integer;
  Cnt, i: Integer;
begin
  aVisualOffsets.Clear;
  Cnt:=Config.GetValue(Path+'Count', 0);
  for i:=0 to Cnt-1 do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    ParentType:=Config.GetValue(SubPath+'ParentType','');
    xTop :=Config.GetValue(SubPath+'Top',0);
    xLeft:=Config.GetValue(SubPath+'Left',0);
    aVisualOffsets.Add(TVisualOffset.Create(ParentType, xTop, xLeft));
  end;
end;

procedure SaveVisualOffsets(Config: TConfigStorage; const Path: string;
  aVisualOffsets: TVisualOffsets);
var
  offs: TVisualOffset;
  SubPath: String;
  i: Integer;
begin
  Config.SetDeleteValue(Path+'Count', aVisualOffsets.Count, 0);
  for i:=0 to aVisualOffsets.Count-1 do begin
    offs:=aVisualOffsets[i];
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    Config.SetDeleteValue(SubPath+'ParentType',offs.ParentType,'');
    Config.SetDeleteValue(SubPath+'Top'       ,offs.Top,0);
    Config.SetDeleteValue(SubPath+'Left'      ,offs.Left,0);
  end;
  // Remove leftover items in case the list has become shorter.
  for i:=aVisualOffsets.Count to aVisualOffsets.Count+10 do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    Config.DeletePath(SubPath);
  end;
end;

{ TConvertSettings }

constructor TConvertSettings.Create(const ATitle: string);
var
  TheMap: TStringToStringTree;
  Categ: string;

  procedure MapReplacement(aDelphi, aLCL: string);
  begin
    if not TheMap.Contains(aDelphi) then
      TheMap[aDelphi]:=aLCL;
  end;

  procedure AddDefaultCategory(aCategory: string);
  var
    x: integer;
  begin
    with fReplaceFuncs do
      if not Categories.Find(aCategory, x) then
        AddCategory(aCategory, True);
  end;

begin
  fTitle:=ATitle;
  fMainFilename:='';
  fMainPath:='';
  fEnabled:=True;
  fSettingsForm:=Nil;
  fReplaceUnits:=TStringToStringTree.Create(false);
  fReplaceTypes:=TStringToStringTree.Create(false);
  fReplaceFuncs:=TFuncsAndCategories.Create;
  fCoordOffsets:=TVisualOffsets.Create;
  // Load settings from ConfigStorage.
  fConfigStorage:=GetIDEConfigStorage('delphiconverter.xml', true);
  fBackupFiles                      :=fConfigStorage.GetValue('BackupFiles', true);
  fKeepFileOpen                     :=fConfigStorage.GetValue('KeepFileOpen', false);
  fTarget            :=TConvertTarget(fConfigStorage.GetValue('ConvertTarget', 0));
  fUnitsReplaceMode:=TReplaceModeLong(fConfigStorage.GetValue('UnitsReplaceMode', 2));
  fUnknownPropsMode:=TReplaceModeLong(fConfigStorage.GetValue('UnknownPropsMode', 2));
  fFuncReplaceMode:=TReplaceModeShort(fConfigStorage.GetValue('FuncReplaceMode', 1));
  fCoordOffsMode  :=TReplaceModeShort(fConfigStorage.GetValue('CoordOffsMode', 1));
  LoadStringToStringTree(fConfigStorage, 'UnitReplacements/', fReplaceUnits);
  LoadStringToStringTree(fConfigStorage, 'TypeReplacements/', fReplaceTypes);
  LoadFuncReplacements(fConfigStorage, 'FuncReplacements/', 'Categories/', fReplaceFuncs);
  LoadVisualOffsets(fConfigStorage, 'VisualOffsets/', fCoordOffsets);

  // Add default values for configuration if ConfigStorage doesn't have them.

  // Map Delphi units to Lazarus units.
  TheMap:=fReplaceUnits;
  MapReplacement('Windows',             'LCLIntf, LCLType, LMessages');
  MapReplacement('Mask',                'MaskEdit');
  MapReplacement('OpenGL',              'GL, GLu');
  MapReplacement('ShellApi',            '');
  MapReplacement('pngImage',            '');
  MapReplacement('Jpeg',                '');
  MapReplacement('gifimage',            '');
  MapReplacement('WinTypes',            '');
  MapReplacement('WinProcs',            '');
  MapReplacement('^FastMM.*',           '');           // External memory manager.
  MapReplacement('MMSystem',            '');
  MapReplacement('.*dll.*',             '');
  MapReplacement('^Q(.+)',              '$1');         // Kylix unit names.
  // Tnt* third party components.
  MapReplacement('TntLXStringGrids',    'Grids');
  MapReplacement('TntLXCombos',         '');
  MapReplacement('TntLXDataSet',        '');
  MapReplacement('TntLXVarArrayDataSet','');
  MapReplacement('TntLXLookupCtrls',    '');
  MapReplacement('^TntLX(.+)',          '$1');
  MapReplacement('^Tnt([^L][^X].+)',    '$1');
  // from Mattias: ^Tnt(([^L]|.[^X]).+)  or  ^Tnt(([^L]|L[^X]).*|L$)
  // from Alexander Klenin: ^Tnt(?!LX)(.+)$

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
  // DevExpress components.
  MapReplacement('TCxEdit',           'TEdit');
  // Tnt* third party components.
  MapReplacement('^TTnt(.+)LX$',      'T$1');
  MapReplacement('^TTnt(.+[^L][^X])$','T$1');

  // Coordinate offsets for some visual containers.
  with fCoordOffsets do begin
    AddVisualOffset('TGroupBox' , 14,2);
    AddVisualOffset('TPanel',      2,2);
    AddVisualOffset('RadioGroup', 14,2);
    AddVisualOffset('CheckGroup', 14,2);
  end;

  // Map Delphi function names to FCL/LCL functions.
  with fReplaceFuncs do begin
    // File name encoding.
    Categ:='UTF8Names';
    AddDefaultCategory(Categ);
    AddFunc(Categ,'FileExists',          'FileExistsUTF8($1)',          'LCL','FileUtil');
    AddFunc(Categ,'FileAge',             'FileAgeUTF8($1)',             'LCL','FileUtil');
    AddFunc(Categ,'DirectoryExists',     'DirectoryExistsUTF8($1)',     'LCL','FileUtil');
    AddFunc(Categ,'ExpandFileName',      'ExpandFileNameUTF8($1)',      'LCL','FileUtil');
    AddFunc(Categ,'ExpandUNCFileName',   'ExpandUNCFileNameUTF8($1)',   'LCL','FileUtil');
    AddFunc(Categ,'ExtractShortPathName','ExtractShortPathNameUTF8($1)','LCL','FileUtil');
    AddFunc(Categ,'FindFirst',           'FindFirstUTF8($1,$2,$3)',     'LCL','FileUtil');
    AddFunc(Categ,'FindNext',            'FindNextUTF8($1)',            'LCL','FileUtil');
    AddFunc(Categ,'FindClose',           'FindCloseUTF8($1)',           'LCL','FileUtil');
    AddFunc(Categ,'FileSetDate',         'FileSetDateUTF8($1,$2)',      'LCL','FileUtil');
    AddFunc(Categ,'FileGetAttr',         'FileGetAttrUTF8($1)',         'LCL','FileUtil');
    AddFunc(Categ,'FileSetAttr',         'FileSetAttrUTF8($1)',         'LCL','FileUtil');
    AddFunc(Categ,'DeleteFile',          'DeleteFileUTF8($1)',          'LCL','FileUtil');
    AddFunc(Categ,'RenameFile',          'RenameFileUTF8($1,$2)',       'LCL','FileUtil');
    AddFunc(Categ,'FileSearch',          'FileSearchUTF8($1,$2)',       'LCL','FileUtil');
    AddFunc(Categ,'FileIsReadOnly',      'FileIsReadOnlyUTF8($1)',      'LCL','FileUtil');
    AddFunc(Categ,'GetCurrentDir',       'GetCurrentDirUTF8',           'LCL','FileUtil');
    AddFunc(Categ,'SetCurrentDir',       'SetCurrentDirUTF8($1)',       'LCL','FileUtil');
    AddFunc(Categ,'CreateDir',           'CreateDirUTF8($1)',           'LCL','FileUtil');
    AddFunc(Categ,'RemoveDir',           'RemoveDirUTF8($1)',           'LCL','FileUtil');
    AddFunc(Categ,'ForceDirectories',    'ForceDirectoriesUTF8($1)',    'LCL','FileUtil');
    // File functions using a handle.
    Categ:='FileHandle';
    AddDefaultCategory(Categ);
    AddFunc(Categ, 'CreateFile', 'FileCreate($1)','','SysUtils');
    AddFunc(Categ, 'GetFileSize','FileSize($1)'  ,'','SysUtils');
    AddFunc(Categ, 'ReadFile',   'FileRead($1)'  ,'','SysUtils');
    AddFunc(Categ, 'CloseHandle','FileClose($1)' ,'','SysUtils');
    // WindowsAPI
    Categ:='WindowsAPI';
    AddFunc(Categ, 'ShellExecute',
                   'if $3 match ":/" then OpenURL($3); OpenDocument($3)', 'LCL', '');
    AddFunc(Categ, 'TimeGetTime', 'GetTickCount','LCL',''); // In Windows MMSystems unit.
    // OpenGL
    Categ:='OpenGL';
    AddFunc(Categ, 'glIsEnabled', 'Boolean(glIsEnabled($1))', '', 'GL');
    AddFunc(Categ, 'glIsList',    'Boolean(glIsList($1))',    '', 'GL');
    AddFunc(Categ, 'glIsTexture', 'Boolean(glIsTexture($1))', '', 'GL');
    // In glColorMask, glDepthMask and gluQuadricTexture :
    //  the parameter types should be typecasted, too. Not supported yet.
    // Others
    Categ:='Other';
    AddDefaultCategory(Categ);
    AddFunc(Categ, 'Ptr','Pointer($1)' ,'','');
  end;
end;

destructor TConvertSettings.Destroy;
begin
  // Save possibly modified settings to ConfigStorage.
  fConfigStorage.SetDeleteValue('BackupFiles',      fBackupFiles, true);
  fConfigStorage.SetDeleteValue('KeepFileOpen',     fKeepFileOpen, false);
  fConfigStorage.SetDeleteValue('ConvertTarget',    integer(fTarget), 0);
  fConfigStorage.SetDeleteValue('UnitsReplaceMode', integer(fUnitsReplaceMode), 2);
  fConfigStorage.SetDeleteValue('UnknownPropsMode', integer(fUnknownPropsMode), 2);
  fConfigStorage.SetDeleteValue('FuncReplaceMode',  integer(fFuncReplaceMode), 1);
  fConfigStorage.SetDeleteValue('CoordOffsMode',    integer(fCoordOffsMode), 1);
  SaveStringToStringTree(fConfigStorage, 'UnitReplacements/', fReplaceUnits);
  SaveStringToStringTree(fConfigStorage, 'TypeReplacements/', fReplaceTypes);
  SaveFuncReplacements(fConfigStorage, 'FuncReplacements/', 'Categories/', fReplaceFuncs);
  SaveVisualOffsets(fConfigStorage, 'VisualOffsets/', fCoordOffsets);
  // Free stuff
  fConfigStorage.Free;
  fReplaceFuncs.Clear;
  fReplaceFuncs.Free;
  fReplaceTypes.Free;
  fReplaceUnits.Free;
  fCoordOffsets.Free;
  inherited Destroy;
end;

function TConvertSettings.RunForm: TModalResult;
begin
  fSettingsForm:=TConvertSettingsForm.Create(nil, Self);
  try
    with fSettingsForm do begin
      Caption:=fTitle + ' - ' + ExtractFileName(fMainFilename);
      ProjectPathEdit.Text:=fMainPath;
      // Settings --> UI. Loaded from ConfigSettings earlier.
      BackupCheckBox.Checked          :=fBackupFiles;
      KeepFileOpenCheckBox.Checked    :=fKeepFileOpen;
      TargetRadioGroup.ItemIndex      :=integer(fTarget);
      UnitReplaceRadioGroup.ItemIndex :=integer(fUnitsReplaceMode);
      UnknownPropsRadioGroup.ItemIndex:=integer(fUnknownPropsMode);
      FuncReplaceRadioGroup.ItemIndex :=integer(fFuncReplaceMode);
      CoordOffsRadioGroup.ItemIndex   :=integer(fCoordOffsMode);
      TargetRadioGroupClick(TargetRadioGroup);
      Result:=ShowModal;         // Let the user change settings in a form.
      if Result=mrOK then begin
        // UI --> Settings. Will be saved to ConfigSettings later.
        fBackupFiles     :=BackupCheckBox.Checked;
        fKeepFileOpen    :=KeepFileOpenCheckBox.Checked;
        fTarget          :=TConvertTarget(TargetRadioGroup.ItemIndex);
        fUnitsReplaceMode:=TReplaceModeLong(UnitReplaceRadioGroup.ItemIndex);
        fUnknownPropsMode:=TReplaceModeLong(UnknownPropsRadioGroup.ItemIndex);
        fFuncReplaceMode :=TReplaceModeShort(FuncReplaceRadioGroup.ItemIndex);
        fCoordOffsMode   :=TReplaceModeShort(CoordOffsRadioGroup.ItemIndex);
      end;
    end;
  finally
    FreeAndNil(fSettingsForm);
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

procedure TConvertSettings.SetEnabled(const AValue: Boolean);
begin
  if fEnabled=AValue then exit;
  fEnabled:=AValue;
  if Assigned(fSettingsForm) then
    fSettingsForm.ButtonPanel1.Enabled:=fEnabled; // OKButton
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
  ProjectPathEdit.Text:='';
  ProjectPathEdit.EditLabel.Caption:=lisProjectPath;
  ProjectPathEdit.Hint:=lisProjectPathHint;

  BackupCheckBox.Caption:=lisBackupChangedFiles;
  BackupCheckBox.Hint:=lisBackupHint;

  KeepFileOpenCheckBox.Caption:=lisKeepFileOpen;
  KeepFileOpenCheckBox.Hint:=lisKeepFileOpenHint;

  TargetRadioGroup.Items.Clear;
  TargetRadioGroup.Items.Add(lisConvertTargetLaz);
  TargetRadioGroup.Items.Add(lisConvertTargetLazAndDelphi);
  TargetRadioGroup.Items.Add(lisConvertTargetLazWinOnly);
  TargetRadioGroup.Items.Add(lisConvertTargetLazAndDelphiSameDfm);
  TargetRadioGroup.ItemIndex:=0;
  TargetRadioGroup.Hint:=lisConvertTargetHint;

  UnitReplaceDivider.Caption:=lisConvUnitReplacements;
  UnitReplaceButton.Caption:=lisCodeToolsDefsEdit;    // Recycled string.
  UnitReplaceDivider.Hint:=lisConvUnitReplHint;
  UnitReplaceButton.Hint:=lisConvUnitReplHint;
  UnitReplaceRadioGroup.Items.Clear;
  UnitReplaceRadioGroup.Items.Add(lisDisabled);    // 'Disabled'
  UnitReplaceRadioGroup.Items.Add(lisInteractive); // 'Interactive'
  UnitReplaceRadioGroup.Items.Add(lisAutomatic);   // 'Automatic'

  UnknownPropsDivider.Caption:=lisConvUnknownProps;
  UnknownPropsRadioGroup.Items.Clear;
  UnknownPropsRadioGroup.Items.Add(lisDisabled);
  UnknownPropsRadioGroup.Items.Add(lisInteractive);
  UnknownPropsRadioGroup.Items.Add(lisAutomatic);

  TypeReplaceDivider.Caption:=lisConvTypeReplacements;
  TypeReplaceButton.Caption:=lisCodeToolsDefsEdit;
  TypeReplaceDivider.Hint:=lisConvTypeReplHint;
  TypeReplaceButton.Hint:=lisConvTypeReplHint;
  TypeReplaceInfoLabel.Caption:=lisInteractive;

  FuncReplaceDivider.Caption:=lisConvFuncReplacements;
  FuncReplaceButton.Caption:=lisCodeToolsDefsEdit;
  FuncReplaceDivider.Hint:=lisConvFuncReplHint;
  FuncReplaceButton.Hint:=lisConvFuncReplHint;
  FuncReplaceRadioGroup.Items.Clear;
  FuncReplaceRadioGroup.Items.Add(lisDisabled);    // 'Disabled'
  FuncReplaceRadioGroup.Items.Add(lisEnabled);     // 'Enabled'

  CoordOffsDivider.Caption:=lisConvCoordOffs;
  CoordOffsButton.Caption:=lisCodeToolsDefsEdit;
  CoordOffsDivider.Hint:=lisConvCoordHint;
  CoordOffsButton.Hint:=lisConvCoordHint;
  CoordOffsRadioGroup.Items.Clear;
  CoordOffsRadioGroup.Items.Add(lisDisabled);
  CoordOffsRadioGroup.Items.Add(lisEnabled);

  ButtonPanel1.OKButton.Caption:=lisStartConversion;
  ButtonPanel1.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel1.CancelButton.Caption:=dlgCancel;
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
  // Function names are not replaced for Windows only target.
  if Trg=ctLazarusWin then
    FuncReplaceRadioGroup.ItemIndex:=integer(rsDisabled);
  FuncReplaceRadioGroup.Enabled:=Trg<>ctLazarusWin;
  // Coordinates are not adjusted when the same DFM form file is used.
  if Trg=ctLazarusDelphiSameDfm then
    CoordOffsRadioGroup.ItemIndex:=integer(rsDisabled);
  CoordOffsRadioGroup.Enabled:=Trg<>ctLazarusDelphiSameDfm;
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


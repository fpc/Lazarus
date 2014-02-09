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
  StdCtrls, EditBtn, Buttons, ExtCtrls, DialogProcs, ButtonPanel, ComCtrls,
  LazarusIDEStrConsts, CodeToolsStructs, CodeToolManager, CodeCache,
  DividerBevel, BaseIDEIntf, IDEMsgIntf, IDEExternToolIntf, AVL_Tree,
  LazConfigStorage, ConverterTypes, ReplaceNamesUnit, ReplaceFuncsUnit;

type

  TReplaceModeLong = (rlDisabled, rlInteractive, rlAutomatic);
  TReplaceModeShort = (rsDisabled, rsEnabled);
  TReplaceModeAllow = (raEnabled, raAutomatic);

  TConvertSettingsForm = class;

  { TConvertSettings }

  TConvertSettings = class
  private
    fEnabled: Boolean;
    fTitle: String;       // Used for form caption.
    fLog: TStringList;
    // Unit, Project or Package top file and path.
    fMainFilenames: TStringList;
    // Persistent storage in XML or some other format.
    fConfigStorage: TConfigStorage;
    fSettingsForm: TConvertSettingsForm;
    // Actual user settings.
    fDelphiDefine: Boolean;
    fBackupFiles: Boolean;
    fKeepFileOpen: Boolean;
    fCrossPlatform: Boolean;
    fSupportDelphi: Boolean;
    fSameDfmFile: Boolean;
    fFuncReplaceComment: Boolean;
    // Modes for replacements:
    fUnitsReplaceMode: TReplaceModeLong;
    fPropReplaceMode: TReplaceModeLong;
    fTypeReplaceMode: TReplaceModeAllow;
    fFuncReplaceMode: TReplaceModeShort;
    fCoordOffsMode: TReplaceModeShort;
    // Unit names to leave out of a project. Currently not user editable.
    fOmitProjUnits: TStringToStringTree;
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
    function GetMainFilename: String;
    function GetMainPath: String;
    procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create(const ATitle: string);
    destructor Destroy; override;
    function RunForm(ACacheUnitsThread: TThread): TModalResult;

    // Lazarus file name based on Delphi file name, keep suffix.
    function DelphiToLazFilename(const DelphiFilename: string;
      LowercaseFilename: Boolean): string; overload;
    // Lazarus file name based on Delphi file name with new suffix.
    function DelphiToLazFilename(const DelphiFilename, LazExt: string;
      LowercaseFilename: Boolean): string; overload;
    // Create Lazarus file name and copy/rename from Delphi file, keep suffix.
    function RenameDelphiToLazFile(const DelphiFilename: string;
      out LazFilename: string; LowercaseFilename: Boolean): TModalResult; overload;
    // Create Lazarus file name and copy/rename from Delphi file with new suffix.
    function RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
      out LazFilename: string; LowercaseFilename: Boolean): TModalResult; overload;
    function RenameFile(const SrcFilename, DestFilename: string): TModalResult;
    function MaybeBackupFile(const AFilename: string): TModalResult;
    procedure ClearLog;
    function AddLogLine(const ALine: string {$IFDEF EnableNewExtTools}; Urgency: TMessageLineUrgency = mluHint{$ENDIF}): integer;
    function SaveLog: Boolean;
  public
    property MainFilenames: TStringlist read fMainFilenames;
    property MainFilename: String read GetMainFilename;
    property MainPath: String read GetMainPath;
    property BackupPath: String read GetBackupPath;
    property Enabled: Boolean read fEnabled write SetEnabled;
    property DelphiDefine: Boolean read fDelphiDefine;
    property BackupFiles: Boolean read fBackupFiles;
    property KeepFileOpen: Boolean read fKeepFileOpen;
    property CrossPlatform: Boolean read fCrossPlatform;
    property SupportDelphi: Boolean read fSupportDelphi;
    property SameDfmFile: Boolean read fSameDfmFile;
    property FuncReplaceComment: Boolean read fFuncReplaceComment;
    property UnitsReplaceMode: TReplaceModeLong read fUnitsReplaceMode;
    property PropReplaceMode: TReplaceModeLong read fPropReplaceMode;
    property TypeReplaceMode: TReplaceModeAllow read fTypeReplaceMode;
    property FuncReplaceMode: TReplaceModeShort read fFuncReplaceMode;
    property CoordOffsMode:   TReplaceModeShort read fCoordOffsMode;
    property OmitProjUnits: TStringToStringTree read fOmitProjUnits;
    property ReplaceUnits: TStringToStringTree read fReplaceUnits;
    property ReplaceTypes: TStringToStringTree read fReplaceTypes;
    property ReplaceFuncs: TFuncsAndCategories read fReplaceFuncs;
    property CoordOffsets: TVisualOffsets read fCoordOffsets;
  end;


  { TConvertSettingsForm }

  TConvertSettingsForm = class(TForm)
    FuncReplaceCommentCB: TCheckBox;
    DelphiDefineCheckBox: TCheckBox;
    InputPathLabel: TLabel;
    InputPathListBox: TListBox;
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
    KeepFileOpenCheckBox: TCheckBox;
    BackupCheckBox: TCheckBox;
    ButtonPanel1: TButtonPanel;
    TypeReplaceButton: TBitBtn;
    UnitReplaceButton: TBitBtn;
    CoordOffsButton: TBitBtn;
    procedure SameDfmCheckBoxChange(Sender: TObject);
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
    fCacheUnitsThread: TThread;
    procedure ThreadTerminated(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ASettings: TConvertSettings); reintroduce;
    destructor Destroy; override;
    property CacheUnitsThread: TThread read fCacheUnitsThread;
  end;

var
  ConvertSettingsForm: TConvertSettingsForm;

function IsWinSpecificUnit(const ALowercaseUnitName: string): Boolean;


implementation

uses ConvertDelphi;

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

function IsWinSpecificUnit(const ALowercaseUnitName: string): Boolean;
// These units exist in Windows only.
// They must be treated as missing units when converting for multi-platform.
begin
  Result := (ALowercaseUnitName = 'windows')
         or (ALowercaseUnitName = 'shellapi')
         or (ALowercaseUnitName = 'wintypes')
         or (ALowercaseUnitName = 'winproc') ;
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
  fLog:=TStringList.Create;
  fMainFilenames:=TStringList.Create;
  fEnabled:=True;
  fSettingsForm:=Nil;
  fOmitProjUnits:=TStringToStringTree.Create(false);
  fReplaceUnits:=TStringToStringTree.Create(false);
  fReplaceTypes:=TStringToStringTree.Create(false);
  fReplaceFuncs:=TFuncsAndCategories.Create;
  fCoordOffsets:=TVisualOffsets.Create;
  // Load settings from ConfigStorage.
  fConfigStorage:=GetIDEConfigStorage('delphiconverter.xml', true);
  fDelphiDefine                     :=fConfigStorage.GetValue('DelphiDefine', true);
  fBackupFiles                      :=fConfigStorage.GetValue('BackupFiles', true);
  fKeepFileOpen                     :=fConfigStorage.GetValue('KeepFileOpen', false);
  fCrossPlatform                    :=fConfigStorage.GetValue('CrossPlatform', true);
  fSupportDelphi                    :=fConfigStorage.GetValue('SupportDelphi', false);
  fSameDfmFile                      :=fConfigStorage.GetValue('SameDfmFile', false);
  fFuncReplaceComment               :=fConfigStorage.GetValue('FuncReplaceComment', true);
  fUnitsReplaceMode:=TReplaceModeLong(fConfigStorage.GetValue('UnitsReplaceMode', 2));
  fPropReplaceMode :=TReplaceModeLong(fConfigStorage.GetValue('UnknownPropsMode', 2));
  fTypeReplaceMode:=TReplaceModeAllow(fConfigStorage.GetValue('TypeReplaceMode', 1));
  fFuncReplaceMode:=TReplaceModeShort(fConfigStorage.GetValue('FuncReplaceMode', 1));
  fCoordOffsMode  :=TReplaceModeShort(fConfigStorage.GetValue('CoordOffsMode', 1));
  LoadStringToStringTree(fConfigStorage, 'UnitReplacements/', fReplaceUnits);
  LoadStringToStringTree(fConfigStorage, 'TypeReplacements/', fReplaceTypes);
  LoadFuncReplacements(fConfigStorage, 'FuncReplacements/', 'Categories/', fReplaceFuncs);
  LoadVisualOffsets(fConfigStorage, 'VisualOffsets/', fCoordOffsets);

  // Units left out of project. Some projects include them although there are
  //  Lazarus packages for them. This setting is not saved in configuration.
  //  Key = Unit name, Value = Lazarus Package to be added to project as dependency
  fOmitProjUnits['FastMM4']            :='';  // FastMM4 is not needed as FPC's
  fOmitProjUnits['FastMM4Messages']    :='';  // memory manager does its job well.
  fOmitProjUnits['GR32']               :='GR32_Lazarus';
  fOmitProjUnits['GR32_Blend']         :='GR32_Lazarus';
  fOmitProjUnits['GR32_Containers']    :='GR32_Lazarus';
  fOmitProjUnits['GR32_DrawingEx']     :='GR32_Lazarus';
  fOmitProjUnits['GR32_Filters']       :='GR32_Lazarus';
  fOmitProjUnits['GR32_Image']         :='GR32_Lazarus';
  fOmitProjUnits['GR32_Layers']        :='GR32_Lazarus';
  fOmitProjUnits['GR32_LowLevel']      :='GR32_Lazarus';
  fOmitProjUnits['GR32_Math']          :='GR32_Lazarus';
  fOmitProjUnits['GR32_MicroTiles']    :='GR32_Lazarus';
  fOmitProjUnits['GR32_OrdinalMaps']   :='GR32_Lazarus';
  fOmitProjUnits['GR32_RangeBars']     :='GR32_Lazarus';
  fOmitProjUnits['GR32_Rasterizers']   :='GR32_Lazarus';
  fOmitProjUnits['GR32_RepaintOpt']    :='GR32_Lazarus';
  fOmitProjUnits['GR32_Resamplers']    :='GR32_Lazarus';
  fOmitProjUnits['GR32_System']        :='GR32_Lazarus';
  fOmitProjUnits['GR32_Transforms']    :='GR32_Lazarus';
  fOmitProjUnits['GR32_VectorMaps']    :='GR32_Lazarus';
  // OpenGL will be replaced with dglOpenGL in uses section. Download dglOpenGL :
  //  http://wiki.delphigl.com/index.php/dglOpenGL.pas/en#Download
  fOmitProjUnits['OpenGL']             :='';
  fOmitProjUnits['uPSCompiler']        :='pascalscript';
  fOmitProjUnits['uPSUtils']           :='pascalscript';
  fOmitProjUnits['uPSComponent']       :='pascalscript';
  fOmitProjUnits['uPSRuntime']         :='pascalscript';
  fOmitProjUnits['uPSDebugger']        :='pascalscript';
  fOmitProjUnits['uPSPreProcessor']    :='pascalscript';
  fOmitProjUnits['uPSR_dll']           :='pascalscript';
  fOmitProjUnits['uPSC_dll']           :='pascalscript';
  fOmitProjUnits['SynEdit']            :='SynEdit';
  fOmitProjUnits['SynEditMiscProcs']   :='SynEdit';
  fOmitProjUnits['SynEditTextBuffer']  :='SynEdit';
  fOmitProjUnits['SynEditTypes']       :='SynEdit';
  fOmitProjUnits['SynEditHighlighter'] :='SynEdit';
  fOmitProjUnits['SynEditKbdHandler']  :='SynEdit';
  fOmitProjUnits['SynEditKeyCmds']     :='SynEdit';
  fOmitProjUnits['SynEditKeyConst']    :='SynEdit';
  fOmitProjUnits['SynEditMiscClasses'] :='SynEdit';
  fOmitProjUnits['SynEditStrConst']    :='SynEdit';
  fOmitProjUnits['SynEditWordWrap']    :='SynEdit';
  fOmitProjUnits['SynHighlighterMulti']:='SynEdit';
  fOmitProjUnits['SynHighlighterPas']  :='SynEdit';
  fOmitProjUnits['SynTextDrawer']      :='SynEdit';
  fOmitProjUnits['SynRegExpr']         :='SynEdit';

  // * Add default values for configuration if ConfigStorage doesn't have them *

  // Map Delphi units to Lazarus units.
  TheMap:=fReplaceUnits;
  MapReplacement('Windows',             'LCLIntf, LCLType, LMessages');
  MapReplacement('WinTypes',            'LCLIntf, LCLType, LMessages');
  MapReplacement('WinProcs',            'LCLIntf, LCLType, LMessages');
  MapReplacement('Mask',                'MaskEdit');
  MapReplacement('TabNotBk',            'ComCtrls');
  MapReplacement('OpenGL',              'dglOpenGL');
//  MapReplacement('dglOpenGL',           'GL, GLu, GLut');  // ?
  // Database components
  MapReplacement('SqlExpr',             'sqldb');
  MapReplacement('DBLocalS',            'sqldb');
  MapReplacement('DBLocalB',            'sqldb');
  MapReplacement('DBTables',            'sqldb');
  MapReplacement('ADODB',               'sqldb');
  MapReplacement('IBTable',             'sqldb');
  MapReplacement('IBQuery',             'sqldb');
  MapReplacement('IBStoredProc',        'sqldb');
  MapReplacement('IBDatabase',          'sqldb');
  MapReplacement('IBUpdateSQL',         'sqldb');
  MapReplacement('IBCustomDataSet',     'sqldb');
  MapReplacement('IBSQL',               'sqldb');
  MapReplacement('DBLocalI',            'IBConnection');
  // Remove these
  MapReplacement('ShellApi',            '');
  MapReplacement('pngImage',            '');
  MapReplacement('Jpeg',                '');
  MapReplacement('gifimage',            '');
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
  MapReplacement('^Tnt(([^L]|L[^X]).*)','$1');

  // Map Delphi types to LCL types.
  TheMap:=fReplaceTypes;
  MapReplacement('TFlowPanel',        'TPanel');
  MapReplacement('TGridPanel',        'TPanel');
  MapReplacement('TComboBoxEx',       'TComboBox');
  MapReplacement('TRichEdit',         'TMemo'); // or TRichMemo from CRC.
  MapReplacement('TDBRichEdit',       'TDBMemo');
  MapReplacement('TApplicationEvents','TApplicationProperties');
  MapReplacement('TPNGObject',        'TPortableNetworkGraphic');
  MapReplacement('TTabbedNotebook',   'TPageControl');
  MapReplacement('TTabPage',          'ts$autoinc: TTabSheet');
  MapReplacement('THeader',           'THeaderControl');
  MapReplacement('TMonthCalendar',    'TCalendar');
  MapReplacement('TOleContainer',     'TActiveXContainer'); // from LazActiveX
  // Database components
  MapReplacement('TSQLConnection',    'TSQLConnector');
  MapReplacement('TSQLClientDataSet', 'TSQLConnector');
  MapReplacement('TSQLDataset',       'TSQLQuery');
  MapReplacement('TSQLTable',         'TSQLQuery');
  MapReplacement('TSQLStoredProc',    'TSQLQuery');
  // BDE
  MapReplacement('TDatabase',         'TSQLConnector');
  MapReplacement('TBDEClientDataSet', 'TSQLConnector');
  MapReplacement('TTable',            'TSQLQuery');
  MapReplacement('TQuery',            'TSQLQuery');
  MapReplacement('TUpdateSQL',        'TSQLQuery');
  MapReplacement('TStoredProc',       'TSQLQuery');
  // ADO
  MapReplacement('TADOConnection',    'TSQLConnector');
  MapReplacement('TADODataSet',       'TSQLQuery');
  MapReplacement('TADOTable',         'TSQLQuery');
  MapReplacement('TADOQuery',         'TSQLQuery');
  MapReplacement('TADOCommand',       'TSQLQuery');
  MapReplacement('TADOStoredProc',    'TSQLQuery');
  // Interbase
  MapReplacement('TIBDatabase',       'TIBConnection');
  MapReplacement('TIBClientDataSet',  'TIBConnection');
  MapReplacement('TIBDataSet',        'TSQLQuery');
  MapReplacement('TIBTable',          'TSQLQuery');
  MapReplacement('TIBQuery',          'TSQLQuery');
  MapReplacement('TIBUpdateSQL',      'TSQLQuery');
  MapReplacement('TIBSQL',            'TSQLQuery');
  MapReplacement('TIBStoredProc',     'TSQLQuery');
  MapReplacement('TIBTransaction',    'TSQLTransaction');
  // DevExpress components
  MapReplacement('TCxEdit',           'TEdit');
  // Tnt* third party components
  MapReplacement('^TTnt(.+)LX$',      'T$1');
  MapReplacement('^TTnt(.+[^L][^X])$','T$1');

  // Coordinate offsets for some visual containers
  with fCoordOffsets do begin
    AddVisualOffset('TGroupBox' , 14,2);
    AddVisualOffset('TPanel',      2,2);
    AddVisualOffset('RadioGroup', 14,2);
    AddVisualOffset('CheckGroup', 14,2);
  end;

  // Map Delphi function names to FCL/LCL functions
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
    AddFunc(Categ,'FileSetAttr',         'FileSetAttrUTF8($1,$2)',      'LCL','FileUtil');
    AddFunc(Categ,'DeleteFile',          'DeleteFileUTF8($1)',          'LCL','FileUtil');
    AddFunc(Categ,'RenameFile',          'RenameFileUTF8($1,$2)',       'LCL','FileUtil');
    AddFunc(Categ,'FileSearch',          'FileSearchUTF8($1,$2)',       'LCL','FileUtil');
    AddFunc(Categ,'FileIsReadOnly',      'FileIsReadOnlyUTF8($1)',      'LCL','FileUtil');
    AddFunc(Categ,'GetCurrentDir',       'GetCurrentDirUTF8',           'LCL','FileUtil');
    AddFunc(Categ,'SetCurrentDir',       'SetCurrentDirUTF8($1)',       'LCL','FileUtil');
    AddFunc(Categ,'CreateDir',           'CreateDirUTF8($1)',           'LCL','FileUtil');
    AddFunc(Categ,'RemoveDir',           'RemoveDirUTF8($1)',           'LCL','FileUtil');
    AddFunc(Categ,'ForceDirectories',    'ForceDirectoriesUTF8($1)',    'LCL','FileUtil');
    // File functions using a handle
    Categ:='FileHandle';
    AddDefaultCategory(Categ);
    AddFunc(Categ, 'CreateFile', 'FileCreate($1)','','SysUtils');
    AddFunc(Categ, 'GetFileSize','FileSize($1)'  ,'','SysUtils');
    AddFunc(Categ, 'ReadFile',   'FileRead($1)'  ,'','SysUtils');
    AddFunc(Categ, 'CloseHandle','FileClose($1)' ,'','SysUtils');
    // WindowsAPI
    Categ:='WindowsAPI';
    AddDefaultCategory(Categ);
    AddFunc(Categ, 'ShellExecute',
                   'if $3 match ":/" then OpenURL($3); OpenDocument($3)', 'LCL', 'LCLIntf');
    AddFunc(Categ, 'TimeGetTime', 'GetTickCount','LCL','LCLIntf'); // In Windows MMSystems unit.
    // OpenGL
    Categ:='OpenGL';
    AddDefaultCategory(Categ);
    AddFunc(Categ, 'glIsEnabled', 'Boolean(glIsEnabled($1))', '', 'GL');
    AddFunc(Categ, 'glIsList',    'Boolean(glIsList($1))',    '', 'GL');
    AddFunc(Categ, 'glIsTexture', 'Boolean(glIsTexture($1))', '', 'GL');
    AddFunc(Categ, 'glColorMask', 'glColorMask(GLboolean($1),GLboolean($2),GLboolean($3),GLboolean($4))', '', 'GL');
    AddFunc(Categ, 'glDepthMask', 'glDepthMask(GLboolean($1))', '', 'GL');
    AddFunc(Categ, 'gluQuadricTexture', 'gluQuadricTexture($1,GLboolean($2))', '', 'GLu');
    // Others
    Categ:='Other';
    AddDefaultCategory(Categ);
    AddFunc(Categ, 'Ptr','Pointer($1)' ,'','');
    // SysUtils has AnsiSameStr and SameText but no SameStr.
    AddFunc(Categ, 'SameStr','(CompareStr($1,$2) = 0)' ,'', 'SysUtils');
  end;
end;

destructor TConvertSettings.Destroy;
begin
  // Save possibly modified settings to ConfigStorage.
  fConfigStorage.SetDeleteValue('DelphiDefine',      fDelphiDefine, true);
  fConfigStorage.SetDeleteValue('BackupFiles',       fBackupFiles, true);
  fConfigStorage.SetDeleteValue('KeepFileOpen',      fKeepFileOpen, false);
  fConfigStorage.SetDeleteValue('CrossPlatform',     fCrossPlatform, true);
  fConfigStorage.SetDeleteValue('SupportDelphi',     fSupportDelphi, false);
  fConfigStorage.SetDeleteValue('SameDfmFile',       fSameDfmFile, false);
  fConfigStorage.SetDeleteValue('FuncReplaceComment',fFuncReplaceComment, true);
  fConfigStorage.SetDeleteValue('UnitsReplaceMode', integer(fUnitsReplaceMode), 2);
  fConfigStorage.SetDeleteValue('UnknownPropsMode', integer(fPropReplaceMode), 2);
  fConfigStorage.SetDeleteValue('TypeReplaceMode',  integer(fTypeReplaceMode), 1);
  fConfigStorage.SetDeleteValue('FuncReplaceMode',  integer(fFuncReplaceMode), 1);
  fConfigStorage.SetDeleteValue('CoordOffsMode',    integer(fCoordOffsMode), 1);
  SaveStringToStringTree(fConfigStorage, 'UnitReplacements/', fReplaceUnits);
  SaveStringToStringTree(fConfigStorage, 'TypeReplacements/', fReplaceTypes);
  SaveFuncReplacements(fConfigStorage, 'FuncReplacements/', 'Categories/', fReplaceFuncs);
  SaveVisualOffsets(fConfigStorage, 'VisualOffsets/', fCoordOffsets);
  // Free stuff
  fConfigStorage.Free;
  fCoordOffsets.Free;
  fReplaceFuncs.Clear;
  fReplaceFuncs.Free;
  fReplaceTypes.Free;
  fReplaceUnits.Free;
  fOmitProjUnits.Free;
  fMainFilenames.Free;
  fLog.Free;
  inherited Destroy;
end;

function TConvertSettings.RunForm(ACacheUnitsThread: TThread): TModalResult;
begin
  fSettingsForm:=TConvertSettingsForm.Create(nil, Self);
  try
    with fSettingsForm do begin
      fCacheUnitsThread := ACacheUnitsThread;
      if Assigned(fCacheUnitsThread) then begin
        fCacheUnitsThread.OnTerminate:=@ThreadTerminated;
        ButtonPanel1.OKButton.Enabled := False; // Disabled while thread is running
        fCacheUnitsThread.Start;
      end
      else
        ThreadTerminated(nil);        // Hide controls dealing with scanning
      Caption:=fTitle + ' - ' + ExtractFileName(MainFilename);
      InputPathListBox.Items.Assign(fMainFilenames);
      // Settings --> UI. Loaded from ConfigSettings earlier.
      DelphiDefineCheckBox.Checked   :=fDelphiDefine;
      BackupCheckBox.Checked         :=fBackupFiles;
      KeepFileOpenCheckBox.Checked   :=fKeepFileOpen;
      CrossPlatformCheckBox.Checked  :=fCrossPlatform;
      SupportDelphiCheckBox.Checked  :=fSupportDelphi;
      SameDfmCheckBox.Checked        :=fSameDfmFile;
      FuncReplaceCommentCB.Checked   :=fFuncReplaceComment;
      UnitReplaceComboBox.ItemIndex  :=integer(fUnitsReplaceMode);
      UnknownPropsComboBox.ItemIndex :=integer(fPropReplaceMode);
      TypeReplaceComboBox.ItemIndex  :=integer(fTypeReplaceMode);
      FuncReplaceComboBox.ItemIndex  :=integer(fFuncReplaceMode);
      CoordOffsComboBox.ItemIndex    :=integer(fCoordOffsMode);
      SupportDelphiCheckBoxChange(SupportDelphiCheckBox);
      SameDfmCheckBoxChange(SameDfmCheckBox);
      Result:=ShowModal;        // Let the user change settings in a form.
      if Result=mrOK then begin // The thread will finished before the form closes.
        // UI --> Settings. Will be saved to ConfigSettings later.
        fDelphiDefine      :=DelphiDefineCheckBox.Checked;
        fBackupFiles       :=BackupCheckBox.Checked;
        fKeepFileOpen      :=KeepFileOpenCheckBox.Checked;
        fCrossPlatform     :=CrossPlatformCheckBox.Checked;
        fSupportDelphi     :=SupportDelphiCheckBox.Checked;
        fSameDfmFile       :=SameDfmCheckBox.Checked;
        fFuncReplaceComment:=FuncReplaceCommentCB.Checked;
        fUnitsReplaceMode:=TReplaceModeLong(UnitReplaceComboBox.ItemIndex);
        fPropReplaceMode :=TReplaceModeLong(UnknownPropsComboBox.ItemIndex);
        fTypeReplaceMode :=TReplaceModeAllow(TypeReplaceComboBox.ItemIndex);
        fFuncReplaceMode :=TReplaceModeShort(FuncReplaceComboBox.ItemIndex);
        fCoordOffsMode   :=TReplaceModeShort(CoordOffsComboBox.ItemIndex);
        if fBackupFiles then
          DeleteDirectory(BackupPath, True); // Delete old backup if there is any.
      end;
    end;
  finally
    FreeAndNil(fSettingsForm);
  end;
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename: string;
                                              LowercaseFilename: Boolean): string;
begin
  Result:=DelphiToLazFilename(DelphiFilename,'',LowercaseFilename);
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename, LazExt: string;
                                              LowercaseFilename: Boolean): string;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, MainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  Result:=MainPath+SubPath+fn+LazExt;
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename: string;
  out LazFilename: string; LowercaseFilename: Boolean): TModalResult;
begin
  Result:=RenameDelphiToLazFile(DelphiFilename,'',LazFilename,LowercaseFilename);
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
  out LazFilename: string; LowercaseFilename: Boolean): TModalResult;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, MainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  // Rename in the same directory.
  Result:=MaybeBackupFile(DelphiFilename); // Save before rename.
  if Result<>mrOK then exit;
  LazFilename:=MainPath+SubPath+fn+LazExt;
  Result:=RenameFileWithErrorDialogs(DelphiFilename,LazFilename,[mbAbort]);
end;

function TConvertSettings.RenameFile(const SrcFilename, DestFilename: string): TModalResult;
begin
  Result:=MaybeBackupFile(SrcFilename); // Save before rename.
  if Result<>mrOK then exit;
  Result:=RenameFileWithErrorDialogs(SrcFilename,DestFilename,[mbAbort]);
end;

function TConvertSettings.MaybeBackupFile(const AFilename: string): TModalResult;
var
  BackupFN: String;
begin
  Result:=mrOK;
  if fBackupFiles then begin
    BackupFN:=BackupPath+ExtractFileName(AFilename);
    if not FileExistsUTF8(BackupFN) then
      Result:=CopyFileWithErrorDialogs(AFilename,BackupFN,[mbAbort]);
  end;
end;

procedure TConvertSettings.ClearLog;
begin
  IDEMessagesWindow.Clear;
  fLog.Clear;
end;

function TConvertSettings.AddLogLine(
  const ALine: string
  {$IFDEF EnableNewExtTools}; Urgency: TMessageLineUrgency{$ENDIF}
  ): integer;
begin
  {$IFDEF EnableNewExtTools}
  IDEMessagesWindow.AddCustomMessage(Urgency,aLine); // Show in message window
  Result:=fLog.Add(MessageLineUrgencyNames[Urgency]+': '+ALine);// and store for log.
  {$ELSE}
  IDEMessagesWindow.AddMsg(ALine, '', -1);   // Show in message window
  Result:=fLog.Add(ALine);                   // and store for log.
  {$ENDIF}
end;

function TConvertSettings.SaveLog: Boolean;
var
  aFilename: String;
  Code: TCodeBuffer;
begin
  aFilename:=MainPath+'AutomaticConversion.log';
  Code:=CodeToolBoss.CreateFile(aFilename);
  Code.Assign(fLog);
  Result:=SaveCodeBuffer(Code)=mrOk;
  if Result then
    {$IFDEF EnableNewExtTools}
    IDEMessagesWindow.AddCustomMessage(mluHint,'This log was saved to '+aFilename); // Show in message window
    {$ELSE}
    IDEMessagesWindow.AddMsg('This log was saved to '+aFilename, '', -1);
    {$ENDIF}
end;

function TConvertSettings.GetBackupPath: String;
const
  BackupPathName='ConverterBackup';
begin
  Result:='';
  if fBackupFiles then begin
    Result:=MainPath+BackupPathName+PathDelim;
    // Create backup path if needed.
    if not DirectoryExistsUTF8(Result) then
      CreateDirUTF8(Result);
  end;
end;

function TConvertSettings.GetMainFilename: String;
begin
  Result:=fMainFilenames[0];
end;

function TConvertSettings.GetMainPath: String;
begin
  Result:=ExtractFilePath(fMainFilenames[0]);
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
  InputPathLabel.Caption:=lisToFPCPath; // Reuse a string form options page.
  InputPathListBox.Clear;
  InputPathListBox.Hint:=lisProjectPathHint;
  DelphiDefineCheckBox.Caption:=lisAddDelphiDefine;
  DelphiDefineCheckBox.Hint:=lisAddDelphiDefineHint;
  BackupCheckBox.Caption:=lisBackupChangedFiles;
  BackupCheckBox.Hint:=lisBackupHint;
  KeepFileOpenCheckBox.Caption:=lisKeepFileOpen;
  KeepFileOpenCheckBox.Hint:=lisKeepFileOpenHint;
  // Target
  CrossPlatformCheckBox.Caption:=lisConvertTargetCrossPlatform;
  CrossPlatformCheckBox.Hint:=lisConvertTargetCrossPlatformHint;
  SupportDelphiCheckBox.Caption:=lisConvertTargetSupportDelphi;
  SupportDelphiCheckBox.Hint:=lisConvertTargetSupportDelphiHint;
  SameDfmCheckBox.Caption:=lisConvertTargetSameDfmFile;
  SameDfmCheckBox.Hint:=lisConvertTargetSameDfmFileHint;
  TargetGroupBox.Caption:=lisConvertTarget;
  TargetGroupBox.Hint:=lisConvertTargetHint;
  // Unit Replacements
  UnitReplaceDivider.Caption:=lisConvUnitReplacements;
  UnitReplaceButton.Caption:=lisEdit;    // Recycled string.
  UnitReplaceButton.LoadGlyphFromLazarusResource('laz_edit');
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
  TypeReplaceButton.LoadGlyphFromLazarusResource('laz_edit');
  TypeReplaceDivider.Hint:=lisConvTypeReplHint;
  TypeReplaceButton.Hint:=lisConvTypeReplHint;
  TypeReplaceComboBox.Items.Add(lisInteractive);
  TypeReplaceComboBox.Items.Add(lisAutomatic);
  // Func Replacements
  FuncReplaceDivider.Caption:=lisConvFuncReplacements;
  FuncReplaceButton.Caption:=lisEdit;
  FuncReplaceButton.LoadGlyphFromLazarusResource('laz_edit');
  FuncReplaceDivider.Hint:=lisConvFuncReplHint;
  FuncReplaceButton.Hint:=lisConvFuncReplHint;
  FuncReplaceComboBox.Items.Add(lisDisabled);
  FuncReplaceComboBox.Items.Add(lisEnabled);
  FuncReplaceCommentCB.Caption:=lisConvAddCommentAfterReplacement;
  // Coordinate Offsets
  CoordOffsDivider.Caption:=lisConvCoordOffs;
  CoordOffsButton.Caption:=lisEdit;
  CoordOffsButton.LoadGlyphFromLazarusResource('laz_edit');
  CoordOffsDivider.Hint:=lisConvCoordHint;
  CoordOffsButton.Hint:=lisConvCoordHint;
  CoordOffsComboBox.Items.Add(lisDisabled);
  CoordOffsComboBox.Items.Add(lisEnabled);
  // File system scanning
  ScanLabel.Caption := lisScanParentDir;
  StopScanButton.Caption:=lisStop;
  StopScanButton.LoadGlyphFromLazarusResource('menu_stop');

  ButtonPanel1.OKButton.Caption:=lisStartConversion;
  ButtonPanel1.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel1.CancelButton.Caption:=lisCancel;
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

procedure TConvertSettingsForm.StopScanButtonClick(Sender: TObject);
begin
  (fCacheUnitsThread as TCacheUnitsThread).Searcher.Stop; // Terminate;
end;

procedure TConvertSettingsForm.CancelButtonClick(Sender: TObject);
begin
  if Assigned(fCacheUnitsThread) then begin
    (fCacheUnitsThread as TCacheUnitsThread).Searcher.Stop;
    fCacheUnitsThread.WaitFor;
  end;
end;

procedure TConvertSettingsForm.ThreadTerminated(Sender: TObject);
begin
  ScanLabel.Visible := False;
  ScanProgressBar.Visible := False;
  StopScanButton.Visible := False;
  ButtonPanel1.OKButton.Enabled := True;
  fCacheUnitsThread := nil;  // Thread frees itself. Make the variable nil, too.
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


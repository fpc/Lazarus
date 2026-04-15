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
    Base classes for Delphi conversion.
    Used by IDE plugin / embedded code and hopefully later in a standlone converter.
    No LCL or IdeIntf dependencies.
}
unit ConvertBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes, AVL_Tree,
  // CodeTools
  CodeToolManager, CodeCache, BasicCodeTools, LinkScanner, KeywordFuncLists, SourceChanger,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, LazConfigStorage, AvgLvlTree, LazLoggerBase,
  // BuildIntf
  ProjectIntf, IDEExternToolIntf, BaseIDEIntf,
  // IdeConfig
  DialogProcs, ProjPackCommon,
  // IdeProject
  Project,
  // IdePackager
  PackageDefs,
  // Converter
  ConverterTypes, ReplaceFuncsUnit,  {!} LazarusIDEStrConsts;

type
  TConvertUnitFlag = (
    cdtlufRenameLowercase, // rename the unit lowercase
    cdtlufCanAbort   // show 'Cancel all' button in error messages using mrAbort
  );
  TConvertUnitFlags = set of TConvertUnitFlag;

  TConvertDelphiPBase = class;

  { TCacheUnitsThread }

  TCacheUnitsThread = class(TThread)
  private
    fPath: string;
    fSearcher: TFileSearcher;
  protected
    procedure Execute; override;
  public
    constructor Create(aConverter: TConvertDelphiPBase; aPath: string);
    destructor Destroy; override;
  public
    property Searcher: TFileSearcher read fSearcher;
  end;

  TConvertSettings = class;

  { TCodeToolLink }

  TCodeToolLink = class
  private
  protected
    fCodeTool: TCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fAskAboutError: boolean;
    fSettings: TConvertSettings;          // Conversion settings.
    procedure InitCodeTool;
  public
    constructor Create(ACode: TCodeBuffer);
    destructor Destroy; override;
    procedure ResetMainScanner;
    function DummyReplacements: Boolean;
  public
    property CodeTool: TCodeTool read fCodeTool;
    property Code: TCodeBuffer read fCode;
    property SrcCache: TSourceChangeCache read fSrcCache;
    property AskAboutError: boolean read fAskAboutError write fAskAboutError;
    property Settings: TConvertSettings read fSettings write fSettings;
  end;

  { TUsedUnitsTool }

  TUsedUnitsToolBase = class
  private
    fCTLink: TCodeToolLink;
    fFilename: string;
  public
    constructor Create(ACTLink: TCodeToolLink; AFilename: string);
    destructor Destroy; override;
    property CTLink: TCodeToolLink read fCTLink;
    property Filename: string read fFilename;
  end;

  { TConvertedDelphiProjectDescriptor }

  TConvertedDelphiProjectDescriptor = class(TProjectDescriptor)
  private
  public
    function InitProject(AProject: TLazProject): TModalResult; override;
  end;

  { TConvertedDelphiPackageDescriptor }
{
  TConvertedDelphiPackageDescriptor = class(TPackageDescriptor)
  private
  public
    procedure InitPackage(APackage: TLazPackage); override;    // ToDo
  end;
}

  { TConvertSettings }

  TReplaceModeLong = (rlDisabled, rlInteractive, rlAutomatic);
  TReplaceModeShort = (rsDisabled, rsEnabled);
  TReplaceModeAllow = (raEnabled, raAutomatic);
{
  TAskUnitPathEvent = function(aConv: TConvertDelphiPBase; ATool: TUsedUnitsToolBase;
    AUnitName: string; ATargetDelphi: boolean): TModalResult; // No "of object" here.

  TCheckFailedEvent = function(PrevResult: TModalResult; aFilename: string): TModalResult;

  // Event signature is from TIDEMessagesWindowInterface.AddCustomMessage.
  TAddMessageEvent = procedure(TheUrgency: TMessageLineUrgency; Msg: string;
    aSrcFilename: string; LineNumber: integer; Column: integer //; const ViewCaption: string
    ) of object;
  TClearMessagesEvent = procedure of object;
}
  TConvertSettings = class
  private
    fVersion: Integer;
    fEnabled: Boolean;
    fTitle: String;       // Used for form caption.
    fLog: TStringList;
    // Unit, Project or Package top file and path.
    fMainFilenames: TStringList;
    // Persistent storage in XML or some other format.
    fConfigStorage: TConfigStorage;
    // Actual user settings.
    fCrossPlatform: Boolean;
    fSupportDelphi: Boolean;
    fSameDfmFile: Boolean;
    fDelphiDefine: Boolean;
    fBackupFiles: Boolean;
    fKeepFileOpen: Boolean;
    fScanParentDir: Boolean;
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
    //procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create(const ATitle: string); virtual;
    destructor Destroy; override;
    procedure Load;
    procedure LoadFuncReplacements;
    procedure LoadStringToStringTree(Path: string; Tree: TStringToStringTree);
    procedure LoadVisualOffsets;
    procedure Save;
    procedure SaveFuncReplacements;
    procedure SaveStringToStringTree(Path: string; Tree: TStringToStringTree);
    procedure SaveVisualOffsets;
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
    function MaybeBackupFile(const AFilename: string): TModalResult;
    procedure ClearLog;
    procedure AddLogLine(Urgency: TMessageLineUrgency; const Msg: string;
      const Filename: string=''; LineNumber: integer=0; Column: integer=0);
    function SaveLog: Boolean;

    // To be overridden in GUI / console class.
    function BeginCodeTools: boolean; virtual; abstract;
    // Files
    function CloseEditorFile(Filename: string): TModalResult; virtual;
    function CloseEditorFile(AUnitInfo: TUnitInfo; Quiet: Boolean): TModalResult; virtual;
    function OpenEditorFile(AFileName: string): TModalResult; virtual;
    function SaveEditorFile(Filename: string): TModalResult; virtual;
    function SaveEditorFile(AUnitInfo: TUnitInfo; QuietUnitCheck: Boolean
      ): TModalResult; virtual;
    // Project
    function NewProject(AInfoFilename: string): iProjPack; virtual; abstract;
    function SaveProject(QuietUnitCheck: Boolean): TModalResult; virtual;

    function JumpToMessage: boolean; virtual;
    procedure JumpToCodeToolBossError; virtual;

    function CheckFailed(PrevResult: TModalResult; aFilename: string): TModalResult; virtual;
    function AskUnitPath(aConv: TConvertDelphiPBase; ATool: TUsedUnitsToolBase;
      AUnitName: string; ATargetDelphi: boolean): TModalResult; virtual;
    function MsgDialog(const aCaption, aMsg: string;
                DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer; virtual;
    procedure AddMessage(Urgency: TMessageLineUrgency; Msg: string;
      SrcFilename: string = ''; LineNumber: integer = 0; Column: integer = 0); virtual;
    procedure ClearMessagesWin; virtual;
    procedure ProcessMessages; virtual;
    procedure BeginWaitCursor; virtual;
    procedure EndWaitCursor; virtual;
    function RunSettingsForm(ACacheUnitsThread: TCacheUnitsThread): TModalResult; virtual;
  public
    property Title: String read fTitle write fTitle;
    property MainFilenames: TStringlist read fMainFilenames;
    property MainFilename: String read GetMainFilename;
    property MainPath: String read GetMainPath;
    property BackupPath: String read GetBackupPath;
    //property Enabled: Boolean read fEnabled write SetEnabled;
    property CrossPlatform: Boolean read fCrossPlatform write fCrossPlatform;
    property SupportDelphi: Boolean read fSupportDelphi write fSupportDelphi;
    property SameDfmFile: Boolean read fSameDfmFile write fSameDfmFile;
    property DelphiDefine: Boolean read fDelphiDefine write fDelphiDefine;
    property BackupFiles: Boolean read fBackupFiles write fBackupFiles;
    property KeepFileOpen: Boolean read fKeepFileOpen write fKeepFileOpen;
    property ScanParentDir: Boolean read fScanParentDir write fScanParentDir;
    property FuncReplaceComment: Boolean read fFuncReplaceComment write fFuncReplaceComment;
    property UnitsReplaceMode: TReplaceModeLong read fUnitsReplaceMode write fUnitsReplaceMode;
    property PropReplaceMode: TReplaceModeLong read fPropReplaceMode write fPropReplaceMode;
    property TypeReplaceMode: TReplaceModeAllow read fTypeReplaceMode write fTypeReplaceMode;
    property FuncReplaceMode: TReplaceModeShort read fFuncReplaceMode write fFuncReplaceMode;
    property CoordOffsMode:   TReplaceModeShort read fCoordOffsMode write fCoordOffsMode;

    property OmitProjUnits: TStringToStringTree read fOmitProjUnits;
    property ReplaceUnits: TStringToStringTree read fReplaceUnits;
    property ReplaceTypes: TStringToStringTree read fReplaceTypes;
    property ReplaceFuncs: TFuncsAndCategories read fReplaceFuncs;
    property CoordOffsets: TVisualOffsets read fCoordOffsets;
  end;

  TConvertSettingsClass = class of TConvertSettings;

  { TConvertDelphiPBase }

  // Base class for all converters. Takes care of error handling etc.
  // TConvertDelphiUnit and TConvertDelphiProjPack inherit from this,
  //  wrapping either one unit or whole project / package conversion.
  TConvertDelphiPBase = class
  private
    fSettings: TConvertSettings;
    fErrorMsg: string;
    // IsConsoleApp is only updated for TConvertDelphiProjPack.
    fIsConsoleApp: Boolean;
    // The user selected path when searching missing units.
    fPrevSelectedPath: string;
    // Missing units that are commented automatically in all units.
    fAllCommentedUnits: TStringListUTF8Fast;
    // Units found in user defined paths.
    fCachedUnitNames: TStringToStringTree;
    // Map of case incorrect unit name -> real unit name.
    fCachedRealFileNames: TStringToStringTree;
  protected
    function EndConvert(AStatus: TModalResult): Boolean;
  public
    constructor Create(const ADescription: string);
    constructor Create(const AFilename, ADescription: string);
    destructor Destroy; override;
    function DoMissingUnits({%H-}AUsedUnitsTool: TUsedUnitsToolBase): integer; virtual;
    function GetCachedUnitPath(const AUnitName: string): string;
  public
    property Settings: TConvertSettings read fSettings;
    property ErrorMsg: string read fErrorMsg write fErrorMsg;
    property IsConsoleApp: Boolean read fIsConsoleApp write fIsConsoleApp;
    property PrevSelectedPath: string read fPrevSelectedPath write fPrevSelectedPath;
    property AllCommentedUnits: TStringListUTF8Fast read fAllCommentedUnits write fAllCommentedUnits;
    property CachedUnitNames: TStringToStringTree read fCachedUnitNames write fCachedUnitNames;
    property CachedRealFileNames: TStringToStringTree read fCachedRealFileNames write fCachedRealFileNames;
  end;

function IsWinSpecificUnit(const ALowercaseUnitName: string): Boolean;

const
  mbYesNo = [mbYes, mbNo];   // Same as in Dialogs
  ConverterVersion: integer = 2;
  // Version 2 removed conversion of string functions into their UTF8 versions.

var
  TheSettingsClass: TConvertSettingsClass;


implementation

function IsWinSpecificUnit(const ALowercaseUnitName: string): Boolean;
// These units exist in Windows only.
// They must be treated as missing units when converting for multi-platform.
begin
  Result := (ALowercaseUnitName = 'windows')
         or (ALowercaseUnitName = 'shellapi')
         or (ALowercaseUnitName = 'wintypes')
         or (ALowercaseUnitName = 'winproc') ;
end;

type
  { TUnitsSearcher }

  TUnitsSearcher = class(TFileSearcher)
  private
    fConverter: TConvertDelphiPBase;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(aConverter: TConvertDelphiPBase);
  end;

{ TUnitsSearcher }

constructor TUnitsSearcher.Create(aConverter: TConvertDelphiPBase);
begin
  inherited Create;
  fConverter := aConverter;
end;

procedure TUnitsSearcher.DoFileFound;
var
  RelPath, SubPath, sUnitName, fn: String;
begin
  RelPath:=CreateRelativePath(FileName, fConverter.fSettings.MainPath);
  SubPath:=ExtractFilePath(RelPath);
  fn:=ExtractFileName(RelPath);
  sUnitName:=ExtractFileNameOnly(fn);
  if (SubPath<>'') and (sUnitName<>'') then begin
    //DebugLn(['RelPath=',RelPath,'SubPath=',SubPath,'fn=',fn,'sUnitName=',sUnitName]);
    // Map path by unit name.
    fConverter.fCachedUnitNames[sUnitName]:=SubPath;
    // Map real unit name by uppercase unit name.
    fConverter.fCachedRealFileNames[UpperCase(sUnitName)]:=fn;
  end;
end;

{ TCacheUnitsThread }

constructor TCacheUnitsThread.Create(aConverter: TConvertDelphiPBase; aPath: string);
begin
  inherited Create(True);
  FreeOnTerminate:=False; // Will be set to True before starting;
  // Create searcher already now. Its Stop method can be called anytime.
  fSearcher:=TUnitsSearcher.Create(aConverter);
  // The parent directory to be scanned
  fPath:=aPath;
end;

destructor TCacheUnitsThread.Destroy;
begin
  fSearcher.Free;
  inherited Destroy;
end;

procedure TCacheUnitsThread.Execute;
// Scan for unit files. This assumes that cache is not used while updating it.
// The main GUI thread must wait for this thread before starting conversion.

  function IsRootPath(APath: String): Boolean;
  // Crude function, it maybe needs support for UNC drives
  var
    D: String;
    Len: Integer;
  begin
    D := ExtractFileDrive(APath);
    Len := Length(D);
    System.Delete(APath, 1, Len);
    Result := (Length(APath) = 1) and (APath[1] in AllowDirectorySeparators);
  end;

begin
  //If a project is in a subfolder of the root, fPath will be root path.
  //Do not search the entire drive, or we may find Borland VCL files and convert them too
  if IsRootPath(fPath) then
    Sleep(1)     // Let the main thread execute, avoid possible synchr. problems.
  else
    // Scan for files and store to fCachedUnitNames, path relative to fSettings.MainPath.
    fSearcher.Search(fPath, '*.pas');
end;

{ TCodeToolLink }

constructor TCodeToolLink.Create(ACode: TCodeBuffer);
begin
  inherited Create;
  fCode:=ACode;
  fAskAboutError:=True;
  InitCodeTool;
end;

destructor TCodeToolLink.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeToolLink.InitCodeTool;
begin
  // Initialize codetools. (Copied from TCodeToolManager.)
  fCodeTool:=nil;
  fSrcCache:=nil;
  if not CodeToolBoss.InitCurCodeTool(fCode) then exit;
  fCodeTool:=CodeToolBoss.CurCodeTool;
  fSrcCache:=CodeToolBoss.SourceChangeCache;
  ResetMainScanner;
  fCodeTool.Scanner.IgnoreMissingIncludeFiles:=True;
end;

procedure TCodeToolLink.ResetMainScanner;
begin
  fSrcCache.MainScanner:=fCodeTool.Scanner;
end;

function TCodeToolLink.DummyReplacements: Boolean;
// If Codetools cannot parse the code, do dummy replacement for all reserved words:
//  '.'+ReservedWord -> '.&'+ReservedWord, needed for OleVariant.
// Most Codetools functions cannot be used because the code is invalid,
//  but TSourceChangeCache.ReplaceEx works.
var
  p, AStart: Integer;
  Src: string;
  LastWasPoint: Boolean;
begin
  p:=1;
  LastWasPoint:=false;
  Src:=fCode.Source;
  repeat
    ReadRawNextPascalAtom(Src,p,AStart,false);
    if p>length(Src) then break;
    // Reserved words are in WordIsKeyWord list in CodeTools.
    if LastWasPoint and WordIsKeyWord.DoIdentifier(@Src[AStart]) then
    begin
      // '.'+ReservedWord was found
      if not fSrcCache.ReplaceEx(gtNone,gtNone,1,1,fCode,AStart,AStart,'&') then
        Exit(False);
    end;
    LastWasPoint:=Src[AStart]='.';
  until false;
  // Apply the changes in buffer
  if not fSrcCache.Apply then
    Exit(False);
  Result:=True;
end;

{ TUsedUnitsTool }

constructor TUsedUnitsToolbase.Create(ACTLink: TCodeToolLink; AFilename: string);
begin
  inherited Create;
  fCTLink:=ACTLink;
  fFilename:=AFilename;
end;

destructor TUsedUnitsToolBase.Destroy;
begin
  inherited Destroy;
end;

{ TConvertedDelphiProjectDescriptor }

function TConvertedDelphiProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
begin
  Result:=inherited InitProject(AProject);
  AProject.LazCompilerOptions.SyntaxMode:='delphi';
end;

{ TConvertSettings }

constructor TConvertSettings.Create(const ATitle: string);
begin
  fTitle:=ATitle;
  fLog:=TStringList.Create;
  fMainFilenames:=TStringList.Create;
  fEnabled:=True;
  fOmitProjUnits:=TStringToStringTree.Create(false);
  fReplaceUnits:=TStringToStringTree.Create(false);
  fReplaceTypes:=TStringToStringTree.Create(false);
  fReplaceFuncs:=TFuncsAndCategories.Create;
  fCoordOffsets:=TVisualOffsets.Create;
  fConfigStorage:=GetIDEConfigStorage('delphiconverter.xml', true);

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
end;

destructor TConvertSettings.Destroy;
begin
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

// Load and store configuration in StringToStringTree :

procedure TConvertSettings.LoadStringToStringTree(Path: string; Tree: TStringToStringTree);
var
  SubPath: String;
  CurName, CurValue: String;
  Cnt, i: Integer;
begin
  Tree.Clear;
  Cnt:=fConfigStorage.GetValue(Path+'Count', 0);
  for i:=0 to Cnt-1 do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    CurName:=fConfigStorage.GetValue(SubPath+'Name','');
    CurValue:=fConfigStorage.GetValue(SubPath+'Value','');
    Tree[CurName]:=CurValue;
  end;
end;

procedure TConvertSettings.SaveStringToStringTree(Path: string; Tree: TStringToStringTree);
var
  Node: TAVLTreeNode;
  Item: PStringToStringItem;
  SubPath: String;
  i: Integer;
begin
  fConfigStorage.DeletePath(Path);      // Make sure there are no old leftover items.
  fConfigStorage.SetDeleteValue(Path+'Count', Tree.Tree.Count, 0);
  Node:=Tree.Tree.FindLowest;
  i:=0;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    fConfigStorage.SetDeleteValue(SubPath+'Name',Item^.Name,'');
    fConfigStorage.SetDeleteValue(SubPath+'Value',Item^.Value,'');
    Node:=Tree.Tree.FindSuccessor(Node);
    inc(i);
  end;
end;

// Load and store configuration in TFuncsAndCategories :

procedure TConvertSettings.LoadFuncReplacements;
var
  SubPath: String;
  xCategory, xDelphiFunc, xReplacement, xPackage, xUnitName: String;
  CategUsed: Boolean;
  Cnt, i: Integer;
begin
  fReplaceFuncs.Clear;
  // Replacement functions
  Cnt:=fConfigStorage.GetValue('FuncReplacements/Count', 0);
  for i:=0 to Cnt-1 do begin
    SubPath:='FuncReplacements/Item'+IntToStr(i)+'/';
    xCategory   :=fConfigStorage.GetValue(SubPath+'Category','');
    // Delete UTF8 func conversion from old configuration.
    if (fVersion < 2) and (xCategory = 'UTF8Names') then Continue;
    xDelphiFunc :=fConfigStorage.GetValue(SubPath+'DelphiFunction','');
    xReplacement:=fConfigStorage.GetValue(SubPath+'Replacement','');
    xPackage    :=fConfigStorage.GetValue(SubPath+'Package','');
    xUnitName   :=fConfigStorage.GetValue(SubPath+'UnitName','');
    fReplaceFuncs.AddFunc(xCategory, xDelphiFunc, xReplacement, xPackage, xUnitName);
  end;
  // Categories
  Cnt:=fConfigStorage.GetValue('Categories/Count', 0);
  for i:=0 to Cnt-1 do begin
    SubPath:='Categories/Item'+IntToStr(i)+'/';
    xCategory:=fConfigStorage.GetValue(SubPath+'Name','');
    // Delete UTF8 category from old configuration.
    if (fVersion < 2) and (xCategory = 'UTF8Names') then Continue;
    CategUsed:=fConfigStorage.GetValue(SubPath+'InUse',True);
    fReplaceFuncs.AddCategory(xCategory, CategUsed);
  end;
end;

procedure TConvertSettings.SaveFuncReplacements;
var
  FuncRepl: TFuncReplacement;
  SubPath, s: String;
  i: Integer;
begin
  // Replacement functions
  fConfigStorage.DeletePath('FuncReplacements/');
  fConfigStorage.SetDeleteValue('FuncReplacements/Count', fReplaceFuncs.Funcs.Count, 0);
  for i:=0 to fReplaceFuncs.Funcs.Count-1 do begin
    FuncRepl:=fReplaceFuncs.FuncAtInd(i);
    if FuncRepl<>nil then begin
      SubPath:='FuncReplacements/Item'+IntToStr(i)+'/';
      fConfigStorage.SetDeleteValue(SubPath+'Category'      ,FuncRepl.Category,'');
      fConfigStorage.SetDeleteValue(SubPath+'DelphiFunction',fReplaceFuncs.Funcs[i],'');
      fConfigStorage.SetDeleteValue(SubPath+'Replacement'   ,FuncRepl.ReplClause,'');
      fConfigStorage.SetDeleteValue(SubPath+'Package'       ,FuncRepl.PackageName,'');
      fConfigStorage.SetDeleteValue(SubPath+'UnitName'      ,FuncRepl.UnitName,'');
    end;
  end;
  // Categories
  fConfigStorage.DeletePath('Categories/');
  fConfigStorage.SetDeleteValue('Categories/Count', fReplaceFuncs.Categories.Count, 0);
  for i:=0 to fReplaceFuncs.Categories.Count-1 do begin
    s:=fReplaceFuncs.Categories[i];
    if s<>'' then begin
      SubPath:='Categories/Item'+IntToStr(i)+'/';
      fConfigStorage.SetDeleteValue(SubPath+'Name',s,'');
      fConfigStorage.SetDeleteValue(SubPath+'InUse',fReplaceFuncs.CategoryIsUsed(i),True);
    end;
  end;
end;

// Load and store configuration in VisualOffsets :

procedure TConvertSettings.LoadVisualOffsets;
var
  ParentType, SubPath: String;
  xTop, xLeft: Integer;
  Cnt, i: Integer;
begin
  fCoordOffsets.Clear;
  Cnt:=fConfigStorage.GetValue('VisualOffsets/Count', 0);
  for i:=0 to Cnt-1 do begin
    SubPath:='VisualOffsets/Item'+IntToStr(i)+'/';
    ParentType:=fConfigStorage.GetValue(SubPath+'ParentType','');
    xTop :=fConfigStorage.GetValue(SubPath+'Top',0);
    xLeft:=fConfigStorage.GetValue(SubPath+'Left',0);
    fCoordOffsets.Add(TVisualOffset.Create(ParentType, xTop, xLeft));
  end;
end;

procedure TConvertSettings.SaveVisualOffsets;
var
  offs: TVisualOffset;
  SubPath: String;
  i: Integer;
begin
  fConfigStorage.DeletePath('VisualOffsets/');
  fConfigStorage.SetDeleteValue('VisualOffsets/Count', fCoordOffsets.Count, 0);
  for i:=0 to fCoordOffsets.Count-1 do begin
    offs:=fCoordOffsets[i];
    SubPath:='VisualOffsets/Item'+IntToStr(i)+'/';
    fConfigStorage.SetDeleteValue(SubPath+'ParentType',offs.ParentType,'');
    fConfigStorage.SetDeleteValue(SubPath+'Top'       ,offs.Top,0);
    fConfigStorage.SetDeleteValue(SubPath+'Left'      ,offs.Left,0);
  end;
end;

procedure TConvertSettings.Load;
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
  fVersion                          :=fConfigStorage.GetValue('Version', 0);
  fCrossPlatform                    :=fConfigStorage.GetValue('CrossPlatform', true);
  fSupportDelphi                    :=fConfigStorage.GetValue('SupportDelphi', false);
  fSameDfmFile                      :=fConfigStorage.GetValue('SameDfmFile', false);
  fDelphiDefine                     :=fConfigStorage.GetValue('DelphiDefine', true);
  fBackupFiles                      :=fConfigStorage.GetValue('BackupFiles', true);
  fKeepFileOpen                     :=fConfigStorage.GetValue('KeepFileOpen', false);
  fScanParentDir                    :=fConfigStorage.GetValue('ScanParentDir', true);
  fFuncReplaceComment               :=fConfigStorage.GetValue('FuncReplaceComment', true);
  fUnitsReplaceMode:=TReplaceModeLong(fConfigStorage.GetValue('UnitsReplaceMode', 2));
  fPropReplaceMode :=TReplaceModeLong(fConfigStorage.GetValue('UnknownPropsMode', 2));
  fTypeReplaceMode:=TReplaceModeAllow(fConfigStorage.GetValue('TypeReplaceMode', 1));
  fFuncReplaceMode:=TReplaceModeShort(fConfigStorage.GetValue('FuncReplaceMode', 1));
  fCoordOffsMode  :=TReplaceModeShort(fConfigStorage.GetValue('CoordOffsMode', 1));

  // * Map Delphi units to Lazarus units *
  TheMap:=fReplaceUnits;
  LoadStringToStringTree('UnitReplacements/', TheMap);
  // Add default values for configuration if ConfigStorage doesn't have them
  // Remove the first part of dotted unit names.
  MapReplacement('^Data\.(.+)',         '$1');
  MapReplacement('^System\.(.+)',       '$1');
  MapReplacement('^Vcl\.(.+)',          '$1');
  MapReplacement('^Vcl\.Imaging\.(.+)', '$1');
  MapReplacement('^Vcl\.Samples\.(.+)', '$1');
  MapReplacement('^Winapi\.(.+)',       '$1');         // Windows units.
  // Windows specifig types need special treatment.
  MapReplacement('Windows',             'LCLIntf, LCLType, LMessages');
  MapReplacement('WinTypes',            'LCLIntf, LCLType, LMessages');
  MapReplacement('WinProcs',            'LCLIntf, LCLType, LMessages');
  MapReplacement('Mask',                'Masks');
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
  // SynEdit has an identical RegExpr as FCL has, but names differ.
  MapReplacement('SynRegExpr',          'RegExpr');
  // Tnt* third party components.
  MapReplacement('TntLXStringGrids',    'Grids');
  MapReplacement('TntLXCombos',         '');
  MapReplacement('TntLXDataSet',        '');
  MapReplacement('TntLXVarArrayDataSet','');
  MapReplacement('TntLXLookupCtrls',    '');
  MapReplacement('^TntLX(.+)',          '$1');
  MapReplacement('^Tnt(([^L]|L[^X]).*)','$1');

  // * Map Delphi types to LCL types *
  TheMap:=fReplaceTypes;
  LoadStringToStringTree('TypeReplacements/', TheMap);
  // Add default values for configuration if ConfigStorage doesn't have them
  MapReplacement('TFlowPanel',        'TPanel');
  MapReplacement('TGridPanel',        'TPanel');
  MapReplacement('TRichEdit',         'TRichMemo');
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

  // * Map Delphi function names to FCL/LCL functions *
  LoadFuncReplacements;
  // Add default values for configuration if ConfigStorage doesn't have them
  with fReplaceFuncs do begin
    // File functions using a handle
    Categ:='FileHandle';
    AddDefaultCategory(Categ);
    //AddFunc(Categ, 'CreateFile', 'FileCreate($1)','','SysUtils');
    //AddFunc(Categ, 'ReadFile',   'FileRead($1)'  ,'','SysUtils');
    AddFunc(Categ, 'CloseHandle','FileClose($1)' ,'','SysUtils');
    AddFunc(Categ, 'GetFileSize','FileSize($1)'  ,'LazUtils','FileUtil');
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
    AddFunc(Categ, 'RecreateWnd','RecreateWnd(Self)' ,'LCL', 'Controls');
    // SysUtils has AnsiSameStr and SameText but no SameStr.
    AddFunc(Categ, 'SameStr','(CompareStr($1,$2) = 0)' ,'', 'SysUtils');
  end;

  // * Coordinate offsets for some visual containers *
  LoadVisualOffsets;
  // Add default values for configuration if ConfigStorage doesn't have them
  with fCoordOffsets do begin
    AddVisualOffset('TGroupBox' , 14,2);
    AddVisualOffset('TPanel',      2,2);
    AddVisualOffset('RadioGroup', 14,2);
    AddVisualOffset('CheckGroup', 14,2);
  end;
end;

procedure TConvertSettings.Save;
begin
  // Save possibly modified settings to ConfigStorage.
  fConfigStorage.SetDeleteValue('Version',           ConverterVersion, 0);
  fConfigStorage.SetDeleteValue('CrossPlatform',     fCrossPlatform, true);
  fConfigStorage.SetDeleteValue('SupportDelphi',     fSupportDelphi, false);
  fConfigStorage.SetDeleteValue('SameDfmFile',       fSameDfmFile, false);
  fConfigStorage.SetDeleteValue('DelphiDefine',      fDelphiDefine, true);
  fConfigStorage.SetDeleteValue('BackupFiles',       fBackupFiles, true);
  fConfigStorage.SetDeleteValue('KeepFileOpen',      fKeepFileOpen, false);
  fConfigStorage.SetDeleteValue('ScanParentDir',     fScanParentDir, true);
  fConfigStorage.SetDeleteValue('FuncReplaceComment', fFuncReplaceComment, true);
  fConfigStorage.SetDeleteValue('UnitsReplaceMode', integer(fUnitsReplaceMode), 2);
  fConfigStorage.SetDeleteValue('UnknownPropsMode', integer(fPropReplaceMode), 2);
  fConfigStorage.SetDeleteValue('TypeReplaceMode',  integer(fTypeReplaceMode), 1);
  fConfigStorage.SetDeleteValue('FuncReplaceMode',  integer(fFuncReplaceMode), 1);
  fConfigStorage.SetDeleteValue('CoordOffsMode',    integer(fCoordOffsMode), 1);
  SaveStringToStringTree('UnitReplacements/', fReplaceUnits);
  SaveStringToStringTree('TypeReplacements/', fReplaceTypes);
  SaveFuncReplacements;
  SaveVisualOffsets;
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
  RelPath:=CreateRelativePath(DelphiFilename, MainPath);
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
  RelPath:=CreateRelativePath(DelphiFilename, MainPath);
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
  ClearMessagesWin;
  fLog.Clear;
end;

procedure TConvertSettings.AddLogLine(Urgency: TMessageLineUrgency;
  const Msg: string; const Filename: string; LineNumber: integer; Column: integer);
var
  FN, Coords, Urg: String;
begin
  // Show in message window
  AddMessage(Urgency, Msg, Filename, LineNumber, Column);
  // and store for log.
  FN := ExtractFileName(Filename);
  if (LineNumber<>0) or (Column<>0) then
    Coords := Format('(%d,%d)', [LineNumber, Column]);
  if Urgency <> mluImportant then
    Urg := MessageLineUrgencyNames[Urgency];
  fLog.Add(FN + Coords + ' ' + Urg + ': ' + Msg);
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
  if Result then                                     // Show in message window
    AddMessage(mluHint, Format(lisConvThisLogWasSaved,[aFilename]));
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

function TConvertSettings.CloseEditorFile(Filename: string): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.CloseEditorFile(AUnitInfo: TUnitInfo; Quiet: Boolean
  ): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.OpenEditorFile(AFileName: string): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.SaveEditorFile(Filename: string): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.SaveEditorFile(AUnitInfo: TUnitInfo;
  QuietUnitCheck: Boolean): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.SaveProject(QuietUnitCheck: Boolean): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.JumpToMessage: boolean;
begin
  Result:=True;
end;

procedure TConvertSettings.JumpToCodeToolBossError;
begin
  ;
end;

function TConvertSettings.CheckFailed(PrevResult: TModalResult;
  aFilename: string): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.AskUnitPath(aConv: TConvertDelphiPBase;
  ATool: TUsedUnitsToolBase; AUnitName: string; ATargetDelphi: boolean
  ): TModalResult;
begin
  Result:=mrOK;
end;

function TConvertSettings.MsgDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
begin
  Result:=mrYes;
end;

procedure TConvertSettings.AddMessage(Urgency: TMessageLineUrgency;
  Msg: string; SrcFilename: string; LineNumber: integer; Column: integer);
begin
  ;
end;

procedure TConvertSettings.ClearMessagesWin;
begin
  ;
end;

procedure TConvertSettings.ProcessMessages;
begin
  ;
end;

procedure TConvertSettings.BeginWaitCursor;
begin
  ;
end;

procedure TConvertSettings.EndWaitCursor;
begin
  ;
end;

function TConvertSettings.RunSettingsForm(ACacheUnitsThread: TCacheUnitsThread
  ): TModalResult;
begin
  Result:=mrOK;
end;

{
procedure TConvertSettings.SetEnabled(const AValue: Boolean);
begin
  if fEnabled=AValue then exit;
  fEnabled:=AValue;
  if Assigned(fSettingsForm) then
    fSettingsForm.ButtonPanel1.Enabled:=fEnabled; // OKButton
end;
}

{ TConvertDelphiPBase }

constructor TConvertDelphiPBase.Create(const ADescription: string);
begin
  inherited Create;
  fSettings:=TheSettingsClass.Create(ADescription);
  fIsConsoleApp:=False;                   // Default = GUI app.
end;

constructor TConvertDelphiPBase.Create(const AFilename, ADescription: string);
begin
  Create(ADescription);
  fSettings.MainFilenames.Add(AFilename);
  fPrevSelectedPath:=fSettings.MainPath;
end;

destructor TConvertDelphiPBase.Destroy;
begin
  fSettings.Free;
  inherited Destroy;
end;

function TConvertDelphiPBase.DoMissingUnits(AUsedUnitsTool: TUsedUnitsToolBase): integer;
begin
  Result := 0;
end;

function TConvertDelphiPBase.GetCachedUnitPath(const AUnitName: string): string;
begin
  Result:=fCachedUnitNames[AUnitName];
end;

function TConvertDelphiPBase.EndConvert(AStatus: TModalResult): Boolean;
begin
  // Show ending message
  if AStatus=mrOK then
    fSettings.AddLogLine(mluImportant, lisConvDelphiConversionReady)
  else begin
    if fErrorMsg<>'' then
      fSettings.AddLogLine(mluError, fErrorMsg);
    fSettings.AddLogLine(mluFatal, lisConvDelphiConversionAborted);
  end;
  // Save log messages to file.
  Result:=fSettings.SaveLog;
end;

end.


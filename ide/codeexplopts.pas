{
/***************************************************************************
                              CodeExplOpts.pas
                             -------------------

 ***************************************************************************/

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

  Abstract:
    Dialog for the options of the code explorer.
}
unit CodeExplOpts;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, Forms, Controls, Graphics, Dialogs, Buttons,
  // CodeTools
  BasicCodeTools, FileProcs,
  // LazUtils
  AvgLvlTree, Laz2_XMLCfg, LazFileUtils, LazFileCache,
  // IDEIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazConf, IDEProcs, LazarusIDEStrConsts;

type
  { TCodeExplorerOptions }
  
  TCodeExplorerPage = (
    cepNone,
    cepCode,
    cepDirectives
    );

  TCodeExplorerRefresh = (
    cerManual,  // only via refresh button
    cerSwitchEditorPage,// everytime the source editor switches to another page
    cerOnIdle // on idle
    );
    
  TCodeExplorerMode = (
    cemCategory, // Category - Delphi like
    cemSource    // Follows Source Code
  );
  
  TCodeExplorerCategory = (
    cecNone,
    cecUses,
    cecTypes,
    cecVariables,
    cecConstants,
    cecProperties,
    cecProcedures,
    cecCodeObserver,
    cecSurrounding
    );
  TCodeExplorerCategories = set of TCodeExplorerCategory;

  TCEObserverCategory = (
    cefcLongProcs,    // procedures with many lines of code
    cefcLongParamLists, // procedures with many parameters
    cefcEmptyProcs,  // procs without code (can contain comments)
    cefcNestedProcs, // procs with a lot of nested sub procs
    cefcUnnamedConsts, // numbers, strings in statements
    cefcEmptyBlocks,   // empty begin..end block (not even a comment)
    cefcWrongIndentation, // possible missing lines or begin blocks
    cefcPublishedPropWithoutDefault, // published properties without default specifier
    cefcUnsortedClassVisibility, // public,private,protected,published keywords are not sorted
    cefcEmptyClassSections, // empty public,private,protected,published section
    cefcUnsortedClassMembers, // member of a public,private,protected,published section is not sorted alphabetically
    cefcToDos  // todo comment
    );
  TCEObserverCategories = set of TCEObserverCategory;

  TCEObserverCategoryGroup = (ocgComplexity, ocgEmpty, ocgStyle, ocgOther);

const
  FirstCodeExplorerCategory = cecUses;
  DefaultCodeExplorerCategories = [cecUses,
               cecTypes,cecVariables,cecConstants,cecProcedures];
  cefcAll = [low(TCEObserverCategory)..high(TCEObserverCategory)];
  DefaultCodeExplorerPage = cepCode;
  DefaultCodeObserverCategories = [
    cefcLongProcs,
    cefcEmptyProcs,
    cefcUnnamedConsts,
    cefcEmptyBlocks,
    cefcWrongIndentation,
    cefcPublishedPropWithoutDefault,
    cefcToDos
    ];
  DefaultCOLongProcLineCount = 50;
  DefaultCOLongParamListCount = 6;
  DefaultCONestedProcCount = 3;
  DefaultCOureCharConst = false;
  DefaultCOIgnoreConstants: array[1..2] of ansistring // Note: keep this asciiz
    = (
    '0',
    '1'
    );
  DefaultCOIgnoreConstInFuncs: array[1..15] of ansistring // Note: keep this asciiz
    = (
    'Assert',
    'Debug',
    'DebugLn',
    'DbgOut',
    'write',
    'writeln',
    'Format',
    'FormatBuf',
    'StrFmt',
    'StrLFmt',
    'FmtStr',
    'FloatToStrF',
    'FloatToStr',
    'CurrToStrF',
    'FormatDateTime'
    );

type

  TCodeExplorerOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FCategories: TCodeExplorerCategories;
    FChangeStep: integer;
    FObserveCharConst: boolean;
    FCOIgnoreConstInFuncs: TStringToStringTree;
    FLongParamListCount: integer;
    FLongProcLineCount: integer;
    FNestedProcCount: integer;
    FObserverCategories: TCEObserverCategories;
    FFollowCursor: boolean;
    FMode : TCodeExplorerMode;
    FObserverIgnoreConstants: TAvlTree;// tree of AnsiString
    FOptionsFilename: string;
    FPage: TCodeExplorerPage;
    FRefresh: TCodeExplorerRefresh;
    FSavedChangeStep: integer;
    function GetModified: boolean;
    procedure SetCategories(const AValue: TCodeExplorerCategories);
    procedure SetFollowCursor(const AValue: boolean);
    procedure SetLongParamListCount(const AValue: integer);
    procedure SetLongProcLineCount(const AValue: integer);
    procedure SetMode(const AValue: TCodeExplorerMode);
    procedure SetModified(AValue: boolean);
    procedure SetNestedProcCount(const AValue: integer);
    procedure SetObserveCharConst(const AValue: boolean);
    procedure SetObserverCategories(const AValue: TCEObserverCategories);
    procedure SetPage(AValue: TCodeExplorerPage);
    procedure SetRefresh(const AValue: TCodeExplorerRefresh);
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite(Restore: boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure Load;
    procedure Save;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure IncreaseChangeStep;
    property Modified: boolean read GetModified write SetModified;
  public
    // observer: ignore constants
    function CreateListOfCOIgnoreConstants: TStrings;
    procedure SetListOf_COIgnoreConstants(List: TStrings; Add: boolean);
    function COIgnoreConstant(p: PChar): boolean;// test if atom is in COIgnoreConstants
    function COIgnoreConstants_AreDefault(Exactly: boolean): boolean; // true if COIgnoreConstants contain/are default values
    function COIgnoreConstant_IsDefault(const Atom: string): boolean; // true if Atom is in default values
    procedure Add_COIgnoreConstant(const Atom: string);
    procedure Clear_COIgnoreConstants;
    procedure LoadDefaults_COIgnoreConstants;
  public
    // observer: ignore constants in functions
    function CreateListOfCOIgnoreConstInFuncs: TStrings;
    procedure SetListOf_COIgnoreConstInFuncs(List: TStrings; Add: boolean);
    function COIgnoreConstInFunc(const Func: string): boolean;// test if function is in COIgnoreConstInFuncs
    function COIgnoreConstInFuncs_AreDefault(Exactly: boolean): boolean; // true if COIgnoreConstInFuncs contain/are default values
    function COIgnoreConstInFuncs_IsDefault(const Func: string): boolean; // true if Atom is in default values
    procedure Add_COIgnoreConstInFuncs(const Func: string);
    procedure Clear_COIgnoreConstInFuncs;
    procedure LoadDefaults_COIgnoreConstInFuncs;
  public
    property Refresh: TCodeExplorerRefresh read FRefresh write SetRefresh default cerSwitchEditorPage;
    property Mode: TCodeExplorerMode read FMode write SetMode default cemCategory;
    property OptionsFilename: string read FOptionsFilename write FOptionsFilename;
    property FollowCursor: boolean read FFollowCursor write SetFollowCursor default true;
    property Categories: TCodeExplorerCategories read FCategories write SetCategories default DefaultCodeExplorerCategories;
    property Page: TCodeExplorerPage read FPage write SetPage default DefaultCodeExplorerPage;
    property ChangeStep: integer read FChangeStep write FChangeStep;
  public
    // Observer
    property ObserveCharConst: boolean read FObserveCharConst write SetObserveCharConst default DefaultCOureCharConst;
    property ObserverCategories: TCEObserverCategories read FObserverCategories write SetObserverCategories default DefaultCodeObserverCategories;
    property ObserverIgnoreConstants: TAvlTree read FObserverIgnoreConstants;
    property COIgnoreConstInFuncs: TStringToStringTree read FCOIgnoreConstInFuncs;
    property LongParamListCount: integer read FLongParamListCount write SetLongParamListCount default DefaultCOLongParamListCount;
    property LongProcLineCount: integer read FLongProcLineCount write SetLongProcLineCount default DefaultCOLongProcLineCount;
    property NestedProcCount: integer read FNestedProcCount write SetNestedProcCount default DefaultCONestedProcCount;
  end;

const
  CodeExplorerVersion = 1;

  cerDefault = cerSwitchEditorPage;

  CodeExplorerPageNames: array[TCodeExplorerPage] of string = (
    '?',
    'Code',
    'Directives'
    );
  CodeExplorerRefreshNames: array[TCodeExplorerRefresh] of string = (
    'Manual',
    'SwitchEditorPage',
    'OnIdle'
    );
  CodeExplorerModeNames: array[TCodeExplorerMode] of string = (
    'Category',
    'Source'
    );
  CodeExplorerCategoryNames: array[TCodeExplorerCategory] of string = (
    '?',
    'Uses',
    'Types',
    'Variables',
    'Constants',
    'Properties',
    'Procedures',
    'CodeObserver',
    'Surrounding'
    );
  CodeObserverCategoryNames: array[TCEObserverCategory] of string = (
    'LongProcs',
    'LongParamLists',
    'EmptyProcs',
    'NestedProcs',
    'UnnamedConsts',
    'EmptyBlocks',
    'WrongIndentation',
    'PublishedPropWithoutDefault',
    'UnsortedClassVisibility',
    'EmptyClassSections',
    'UnsortedClassMembers',
    'ToDos'
    );

var
  CodeExplorerOptions: TCodeExplorerOptions = nil; // set by the IDE

function CodeExplorerPageNameToEnum(const s: string): TCodeExplorerPage;
function CodeExplorerRefreshNameToEnum(const s: string): TCodeExplorerRefresh;
function CodeExplorerModeNameToEnum(const s: string): TCodeExplorerMode;
function CodeExplorerCategoryNameToEnum(const s: string): TCodeExplorerCategory;
function CodeExplorerLocalizedString(const c: TCodeExplorerCategory): string;
function CodeObserverCatNameToEnum(const s: string): TCEObserverCategory;
function CodeExplorerLocalizedString(const c: TCEObserverCategory): string;
function dbgs(c: TCodeExplorerCategory): string; overload;

implementation

function CodeExplorerPageNameToEnum(const s: string): TCodeExplorerPage;
begin
  for Result:=Low(TCodeExplorerPage) to High(TCodeExplorerPage) do
    if SysUtils.CompareText(CodeExplorerPageNames[Result],s)=0 then exit;
  Result:=DefaultCodeExplorerPage;
end;

function CodeExplorerRefreshNameToEnum(const s: string): TCodeExplorerRefresh;
begin
  for Result:=Low(TCodeExplorerRefresh) to High(TCodeExplorerRefresh) do
    if SysUtils.CompareText(CodeExplorerRefreshNames[Result],s)=0 then exit;
  Result:=cerDefault;
end;

function CodeExplorerModeNameToEnum(const s: string): TCodeExplorerMode;
begin
  for Result:=Low(TCodeExplorerMode) to High(TCodeExplorerMode) do
    if SysUtils.CompareText(CodeExplorerModeNames[Result],s)=0 then exit;
  Result:=cemCategory;
end;

function CodeExplorerCategoryNameToEnum(const s: string): TCodeExplorerCategory;
begin
  for Result:=FirstCodeExplorerCategory to High(TCodeExplorerCategory) do
    if SysUtils.CompareText(CodeExplorerCategoryNames[Result],s)=0 then exit;
  Result:=cecTypes;
end;

function CodeExplorerLocalizedString(const c: TCodeExplorerCategory): string;
begin
  case c of
  cecUses: Result:=lisCEUses;
  cecTypes: Result:=lisCETypes;
  cecVariables: Result:=lisCEVariables;
  cecConstants: Result:=lisCEConstants;
  cecProcedures: Result:=lisCEProcedures;
  cecProperties: Result:=lisCEProperties;
  cecCodeObserver: Result:=lisCodeObserver;
  cecSurrounding: Result:=lisCESurrounding;
  else Result:='?';
  end;
end;

function CodeObserverCatNameToEnum(const s: string): TCEObserverCategory;
begin
  for Result:=low(TCEObserverCategory) to High(TCEObserverCategory) do
    if SysUtils.CompareText(CodeObserverCategoryNames[Result],s)=0 then exit;
  Result:=cefcLongProcs;
end;

function CodeExplorerLocalizedString(const c: TCEObserverCategory): string;
begin
  case c of
  cefcLongProcs: Result:=lisCELongProcedures;
  cefcLongParamLists: Result:=lisCEManyParameters;
  cefcEmptyProcs: Result:=lisCEEmptyProcedures;
  cefcNestedProcs: Result:=lisCEManyNestedProcedures;
  cefcUnnamedConsts: Result:=lisCEUnnamedConstants;
  cefcEmptyBlocks: Result:=lisCEEmptyBlocks;
  cefcWrongIndentation: Result:=lisCEWrongIndentation;
  cefcPublishedPropWithoutDefault: Result:=lisCEPublishedPropertyWithoutDefault;
  cefcUnsortedClassVisibility: Result:=lisCEUnsortedVisibility;
  cefcEmptyClassSections: Result:=lisCEEmptyClassSections;
  cefcUnsortedClassMembers: Result:=lisCEUnsortedMembers;
  cefcToDos: Result:=lisCEToDos;
  else Result:='?';
  end;
end;

function dbgs(c: TCodeExplorerCategory): string;
begin
  Result:=CodeExplorerCategoryNames[c];
end;

{ TCodeExplorerOptions }

procedure TCodeExplorerOptions.SetRefresh(const AValue: TCodeExplorerRefresh);
begin
  if FRefresh=AValue then exit;
  FRefresh:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetMode(const AValue: TCodeExplorerMode);
begin
  if FMode=AValue then exit;
  FMode:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStep
  else
    FSavedChangeStep:=FChangeStep;
end;

procedure TCodeExplorerOptions.SetNestedProcCount(const AValue: integer);
begin
  if FNestedProcCount=AValue then exit;
  FNestedProcCount:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetObserveCharConst(const AValue: boolean);
begin
  if FObserveCharConst=AValue then exit;
  FObserveCharConst:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetObserverCategories(
  const AValue: TCEObserverCategories);
begin
  if FObserverCategories=AValue then exit;
  FObserverCategories:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetPage(AValue: TCodeExplorerPage);
begin
  if FPage=AValue then Exit;
  FPage:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetFollowCursor(const AValue: boolean);
begin
  if FFollowCursor=AValue then exit;
  FFollowCursor:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetLongParamListCount(const AValue: integer);
begin
  if FLongParamListCount=AValue then exit;
  FLongParamListCount:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetLongProcLineCount(const AValue: integer);
begin
  if FLongProcLineCount=AValue then exit;
  FLongProcLineCount:=AValue;
  IncreaseChangeStep;
end;

procedure TCodeExplorerOptions.SetCategories(
  const AValue: TCodeExplorerCategories);
begin
  if FCategories=AValue then exit;
  FCategories:=AValue;
  IncreaseChangeStep;
end;

function TCodeExplorerOptions.GetModified: boolean;
begin
  Result:=FSavedChangeStep<>FChangeStep;
end;

constructor TCodeExplorerOptions.Create;
begin
  FOptionsFilename:=AppendPathDelim(GetPrimaryConfigPath)+'codeexploreroptions.xml';
  FObserverIgnoreConstants:=TAvlTree.Create(TListSortCompare(@CompareAtom));
  FCOIgnoreConstInFuncs:=TStringToStringTree.Create(false);
  Clear;
  LoadDefaults_COIgnoreConstants;
  LoadDefaults_COIgnoreConstInFuncs;
end;

destructor TCodeExplorerOptions.Destroy;
begin
  Clear_COIgnoreConstants;
  FreeAndNil(FObserverIgnoreConstants);
  FreeAndNil(FCOIgnoreConstInFuncs);
  inherited Destroy;
end;

class function TCodeExplorerOptions.GetGroupCaption: string;
begin
  Result := dlgGroupCodeExplorer;
end;

class function TCodeExplorerOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := CodeExplorerOptions;
end;

procedure TCodeExplorerOptions.DoAfterWrite(Restore: boolean);
begin
  if not Restore then
    Save;
end;

procedure TCodeExplorerOptions.Clear;
begin
  IncreaseChangeStep;
  FMode:=cemCategory;
  FRefresh:=cerDefault;
  FFollowCursor:=true;
  FPage:=DefaultCodeExplorerPage;
  FCategories:=DefaultCodeExplorerCategories;
  FObserverCategories:=DefaultCodeObserverCategories;
  FLongProcLineCount:=DefaultCOLongProcLineCount;
  FLongParamListCount:=DefaultCOLongParamListCount;
  FNestedProcCount:=DefaultCONestedProcCount;
  FObserveCharConst:=DefaultCOureCharConst;
  Clear_COIgnoreConstants;
  Clear_COIgnoreConstInFuncs;
end;

procedure TCodeExplorerOptions.Assign(Source: TPersistent);
var
  Src: TCodeExplorerOptions;
  List: TStrings;
begin
  if Source is TCodeExplorerOptions then begin
    IncreaseChangeStep;
    Src:=TCodeExplorerOptions(Source);
    FRefresh:=Src.Refresh;
    FMode:=Src.Mode;
    FFollowCursor:=Src.FollowCursor;
    FPage:=Src.Page;
    FCategories:=Src.Categories;
    FObserverCategories:=Src.ObserverCategories;
    FLongProcLineCount:=Src.LongProcLineCount;
    FLongParamListCount:=Src.LongParamListCount;
    FNestedProcCount:=Src.NestedProcCount;
    FObserveCharConst:=Src.ObserveCharConst;
    List:=Src.CreateListOfCOIgnoreConstants;
    try
      SetListOf_COIgnoreConstants(List,false);
    finally
      List.Free;
    end;
    List:=Src.CreateListOfCOIgnoreConstInFuncs;
    try
      SetListOf_COIgnoreConstInFuncs(List,false);
    finally
      List.Free;
    end;
  end else
    inherited Assign(Source);
end;

procedure TCodeExplorerOptions.Load;
var
  XMLConfig: TXMLConfig;
  //FileVersion: integer;
begin
  if not FileExistsUTF8(FOptionsFilename) then
  begin
    Clear;
    LoadDefaults_COIgnoreConstants;
    LoadDefaults_COIgnoreConstInFuncs;
    Exit;
  end;
  try
    XMLConfig:=TXMLConfig.Create(FOptionsFilename);
    //FileVersion:=XMLConfig.GetValue('CodeExplorer/Version/Value',0);
    LoadFromXMLConfig(XMLConfig,'CodeExplorer/');
    XMLConfig.Free;
  except
    on E: Exception do begin
      DebugLn('[TCodeExplorerOptions.Load]  error reading "',FOptionsFilename,'" ',E.Message);
    end;
  end;
end;

procedure TCodeExplorerOptions.Save;
var
  XMLConfig: TXMLConfig;
begin
  if FileExistsCached(FOptionsFilename) and not Modified then exit;
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.CreateClean(FOptionsFilename);
    XMLConfig.SetDeleteValue('CodeExplorer/Version/Value',
      CodeExplorerVersion,0);
    SaveToXMLConfig(XMLConfig,'CodeExplorer/');
    XMLConfig.Flush;
    XMLConfig.Free;
    Modified:=false;
  except
    on E: Exception do begin
      DebugLn('[TCodeExplorerOptions.Save]  error writing "',FOptionsFilename,'" ',E.Message);
    end;
  end;
end;

procedure TCodeExplorerOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  c: TCodeExplorerCategory;
  f: TCEObserverCategory;
  CurPath: String;
  List: TStringList;
begin
  IncreaseChangeStep;
  Clear;
  FRefresh:=CodeExplorerRefreshNameToEnum(
                                   XMLConfig.GetValue(Path+'Refresh/Value',''));
  FMode:=CodeExplorerModeNameToEnum(
                                   XMLConfig.GetValue(Path+'Mode/Value',''));
  FFollowCursor:=XMLConfig.GetValue(Path+'FollowCursor',true);
  FPage:=CodeExplorerPageNameToEnum(XMLConfig.GetValue(Path+'Page/Value',''));

  FCategories:=[];
  for c:=FirstCodeExplorerCategory to high(TCodeExplorerCategory) do
    if XMLConfig.GetValue(Path+'Categories/'+CodeExplorerCategoryNames[c],
      c in DefaultCodeExplorerCategories) then
        Include(FCategories,c);
  FObserverCategories:=[];
  for f:=low(TCEObserverCategory) to high(TCEObserverCategory) do
  begin
    CurPath:=Path+'CodeObserver/'+CodeObserverCategoryNames[f]+'/';
    if XMLConfig.GetValue(CurPath+'Show',f in DefaultCodeObserverCategories)
    then
      Include(FObserverCategories,f);
    case f of
    cefcLongProcs:
      FLongProcLineCount:=XMLConfig.GetValue(CurPath+'LineCount/Value',
                                                 DefaultCOLongProcLineCount);
    cefcLongParamLists:
      FLongParamListCount:=XMLConfig.GetValue(CurPath+'Count/Value',
                                                 DefaultCOLongParamListCount);
    cefcNestedProcs:
      FNestedProcCount:=XMLConfig.GetValue(CurPath+'Count/Value',
                                                 DefaultCONestedProcCount);
    cefcUnnamedConsts:
      begin
        FObserveCharConst:=XMLConfig.GetValue(CurPath+'CharConsts/Value',
                                                 DefaultCOureCharConst);
        // load standard ObserverIgnoreConstants
        if XMLConfig.GetValue(CurPath+'Ignore/ContainsDefaults',true) then
          LoadDefaults_COIgnoreConstants;
        // load custom ObserverIgnoreConstants
        List:=TStringList.Create;
        try
          LoadStringList(XMLConfig,List,CurPath+'Ignore/');
          SetListOf_COIgnoreConstants(List,true);
        finally
          List.Free;
        end;
        // load standard COIgnoreConstInFuncs
        if XMLConfig.GetValue(CurPath+'IgnoreInFuncs/ContainsDefaults',true) then
          LoadDefaults_COIgnoreConstInFuncs;
        // load custom COIgnoreConstInFuncs
        List:=TStringList.Create;
        try
          LoadStringList(XMLConfig,List,CurPath+'IgnoreFuncs/');
          SetListOf_COIgnoreConstInFuncs(List,true);
        finally
          List.Free;
        end;
      end;
    end;
  end;
end;

procedure TCodeExplorerOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  c: TCodeExplorerCategory;
  f: TCEObserverCategory;
  CurPath: String;
  List: TStrings;
  ContainsDefaults: Boolean;
  i: Integer;
begin
  XMLConfig.SetDeleteValue(Path+'Refresh/Value',
                           CodeExplorerRefreshNames[FRefresh],
                           CodeExplorerRefreshNames[cerDefault]);
  XMLConfig.SetDeleteValue(Path+'Mode/Value',
                           CodeExplorerModeNames[FMode],
                           CodeExplorerModeNames[cemCategory]);
  XMLConfig.SetDeleteValue(Path+'FollowCursor',FFollowCursor,true);
  XMLConfig.SetDeleteValue(Path+'Page/Value',CodeExplorerPageNames[FPage],
                           CodeExplorerPageNames[DefaultCodeExplorerPage]);

  for c:=FirstCodeExplorerCategory to high(TCodeExplorerCategory) do
    XMLConfig.SetDeleteValue(Path+'Categories/'+CodeExplorerCategoryNames[c],
      c in FCategories,c in DefaultCodeExplorerCategories);
  for f:=low(TCEObserverCategory) to high(TCEObserverCategory) do
  begin
    CurPath:=Path+'CodeObserver/'+CodeObserverCategoryNames[f]+'/';
    XMLConfig.SetDeleteValue(CurPath+'Show',
      f in FObserverCategories,f in DefaultCodeObserverCategories);
    case f of
    cefcLongProcs:
      XMLConfig.SetDeleteValue(CurPath+'LineCount/Value',
                           FLongProcLineCount,DefaultCOLongProcLineCount);
    cefcLongParamLists:
      XMLConfig.SetDeleteValue(CurPath+'Count/Value',
                           FLongParamListCount,DefaultCOLongParamListCount);
    cefcNestedProcs:
      XMLConfig.SetDeleteValue(CurPath+'Count/Value',
                           FNestedProcCount,DefaultCONestedProcCount);
    cefcUnnamedConsts:
      begin
        XMLConfig.SetDeleteValue(CurPath+'CharConsts/Value',
                           FObserveCharConst,DefaultCOureCharConst);
        // save standard ObserverIgnoreConstants
        ContainsDefaults:=COIgnoreConstants_AreDefault(false);
        XMLConfig.SetDeleteValue(CurPath+'Ignore/ContainsDefaults',
           ContainsDefaults,true);
        // save ObserverIgnoreConstants
        List:=CreateListOfCOIgnoreConstants;
        try
          for i:=List.Count-1 downto 0 do
            if COIgnoreConstant_IsDefault(List[i]) then
              List.Delete(i);
          SaveStringList(XMLConfig,List,CurPath+'Ignore/');
        finally
          List.Free;
        end;
        // save standard COIgnoreConstInFuncs
        ContainsDefaults:=COIgnoreConstInFuncs_AreDefault(false);
        XMLConfig.SetDeleteValue(CurPath+'IgnoreInFuncs/ContainsDefaults',
           ContainsDefaults,true);
        // save COIgnoreConstInFuncs
        List:=CreateListOfCOIgnoreConstInFuncs;
        try
          for i:=List.Count-1 downto 0 do
            if COIgnoreConstInFuncs_IsDefault(List[i]) then
              List.Delete(i);
          SaveStringList(XMLConfig,List,CurPath+'IgnoreInFuncs/');
        finally
          List.Free;
        end;
      end;
    end;
  end;
end;

procedure TCodeExplorerOptions.IncreaseChangeStep;
begin
  if FChangeStep=high(integer) then
    FChangeStep:=low(integer)
  else
    inc(FChangeStep);
end;

function TCodeExplorerOptions.CreateListOfCOIgnoreConstants: TStrings;
var
  AVLNode: TAvlTreeNode;
  s: String;
begin
  Result:=TStringList.Create;
  AVLNode:=ObserverIgnoreConstants.FindLowest;
  while AVLNode<>nil do begin
    s:=GetAtomString(PChar(AVLNode.Data),false);
    if s<>'' then
      Result.Add(s);
    AVLNode:=ObserverIgnoreConstants.FindSuccessor(AVLNode);
  end;
end;

procedure TCodeExplorerOptions.Clear_COIgnoreConstants;
var
  AVLNode: TAvlTreeNode;
  s: String;
begin
  if FObserverIgnoreConstants.Count=0 then exit;
  IncreaseChangeStep;
  s:='';
  AVLNode:=FObserverIgnoreConstants.FindLowest;
  while AVLNode<>nil do begin
    // decrease reference counter
    Pointer(s):=AVLNode.Data;
    s:='';
    AVLNode:=FObserverIgnoreConstants.FindSuccessor(AVLNode);
  end;
  if s='' then ; // omit fpc note
  FObserverIgnoreConstants.Clear;
end;

procedure TCodeExplorerOptions.SetListOf_COIgnoreConstants(List: TStrings;
  Add: boolean);
var
  i: Integer;
begin
  IncreaseChangeStep;
  if not Add then
    Clear_COIgnoreConstants;
  for i:=0 to List.Count-1 do
    Add_COIgnoreConstant(List[i]);
end;

procedure TCodeExplorerOptions.LoadDefaults_COIgnoreConstants;
var
  i: Integer;
begin
  if COIgnoreConstants_AreDefault(true) then exit;
  IncreaseChangeStep;
  Clear_COIgnoreConstants;
  for i:=low(DefaultCOIgnoreConstants) to high(DefaultCOIgnoreConstants) do
    Add_COIgnoreConstant(DefaultCOIgnoreConstants[i]);
end;

function TCodeExplorerOptions.CreateListOfCOIgnoreConstInFuncs: TStrings;
var
  AVLNode: TAvlTreeNode;
  s: String;
begin
  Result:=TStringList.Create;
  AVLNode:=COIgnoreConstInFuncs.Tree.FindLowest;
  while AVLNode<>nil do begin
    s:=PStringToStringItem(AVLNode.Data)^.Name;
    if s<>'' then
      Result.Add(s);
    AVLNode:=COIgnoreConstInFuncs.Tree.FindSuccessor(AVLNode);
  end;
end;

procedure TCodeExplorerOptions.SetListOf_COIgnoreConstInFuncs(List: TStrings;
  Add: boolean);
var
  i: Integer;
begin
  IncreaseChangeStep;
  if not Add then
    Clear_COIgnoreConstInFuncs;
  for i:=0 to List.Count-1 do
    Add_COIgnoreConstInFuncs(List[i]);
end;

function TCodeExplorerOptions.COIgnoreConstInFunc(const Func: string): boolean;
begin
  Result:=FCOIgnoreConstInFuncs.Contains(Func);
end;

function TCodeExplorerOptions.COIgnoreConstInFuncs_AreDefault(Exactly: boolean
  ): boolean;
const
  DefCount = high(DefaultCOIgnoreConstInFuncs)-Low(DefaultCOIgnoreConstInFuncs)+1;
var
  i: Integer;
begin
  if Exactly and (FCOIgnoreConstInFuncs.Count=DefCount) then
    exit(false);
  if FCOIgnoreConstInFuncs.Count<DefCount then exit(false);
  for i:=low(DefaultCOIgnoreConstInFuncs) to high(DefaultCOIgnoreConstInFuncs) do
    if not COIgnoreConstInFunc(DefaultCOIgnoreConstInFuncs[i]) then
      exit(false);
  Result:=true;
end;

function TCodeExplorerOptions.COIgnoreConstInFuncs_IsDefault(const Func: string
  ): boolean;
var
  i: Integer;
begin
  for i:=low(DefaultCOIgnoreConstants) to high(DefaultCOIgnoreConstants) do
    if SysUtils.CompareText(Func,DefaultCOIgnoreConstants[i])=0 then
      exit(true);
  Result:=false;
end;

procedure TCodeExplorerOptions.Add_COIgnoreConstInFuncs(const Func: string);
begin
  if Func='' then exit;
  if COIgnoreConstInFunc(Func) then exit;
  IncreaseChangeStep;
  FCOIgnoreConstInFuncs.Values[Func]:='';
end;

procedure TCodeExplorerOptions.Clear_COIgnoreConstInFuncs;
begin
  if FCOIgnoreConstInFuncs.Count=0 then exit;
  IncreaseChangeStep;
  FCOIgnoreConstInFuncs.Clear;
end;

procedure TCodeExplorerOptions.LoadDefaults_COIgnoreConstInFuncs;
var
  i: Integer;
begin
  if COIgnoreConstInFuncs_AreDefault(true) then exit;
  IncreaseChangeStep;
  Clear_COIgnoreConstInFuncs;
  for i:=low(DefaultCOIgnoreConstInFuncs) to high(DefaultCOIgnoreConstInFuncs) do
    Add_COIgnoreConstInFuncs(DefaultCOIgnoreConstInFuncs[i]);
end;

function TCodeExplorerOptions.COIgnoreConstant(p: PChar): boolean;
begin
  Result:=FObserverIgnoreConstants.Find(p)<>nil;
end;

procedure TCodeExplorerOptions.Add_COIgnoreConstant(const Atom: string);
var
  s: String;
begin
  if Atom='' then exit;
  if COIgnoreConstant(PChar(Atom)) then exit;
  IncreaseChangeStep;
  s:=Atom;
  FObserverIgnoreConstants.Add(Pointer(s));
  Pointer(s):=nil;
end;

function TCodeExplorerOptions.COIgnoreConstants_AreDefault(Exactly: boolean
  ): boolean;
const
  DefCount = high(DefaultCOIgnoreConstants)-Low(DefaultCOIgnoreConstants)+1;
var
  i: Integer;
begin
  if Exactly and (FObserverIgnoreConstants.Count=DefCount) then
    exit(false);
  if FObserverIgnoreConstants.Count<DefCount then exit(false);
  for i:=low(DefaultCOIgnoreConstants) to high(DefaultCOIgnoreConstants) do
    if not COIgnoreConstant(PChar(DefaultCOIgnoreConstants[i])) then
      exit(false);
  Result:=true;
end;

function TCodeExplorerOptions.COIgnoreConstant_IsDefault(const Atom: string
  ): boolean;
var
  i: Integer;
begin
  for i:=low(DefaultCOIgnoreConstants) to high(DefaultCOIgnoreConstants) do
    if CompareAtom(PChar(Atom),PChar(DefaultCOIgnoreConstants[i]),false)=0 then
      exit(true);
  Result:=false;
end;

initialization
  RegisterIDEOptionsGroup(GroupCodeExplorer, TCodeExplorerOptions);
end.


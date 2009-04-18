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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Dialog for the options of the code explorer.
}
unit CodeExplOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, FileUtil, StdCtrls, ButtonPanel, AvgLvlTree,
  CodeToolManager, Laz_XMLCfg, BasicCodeTools,
  IDEContextHelpEdit,
  LazConf, IDEProcs, IDEOptionsIntf, LazarusIDEStrConsts;

type
  { TCodeExplorerOptions }
  
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
    cecFigures
    );
  TCodeExplorerCategories = set of TCodeExplorerCategory;

  TCEFigureCategory = (
    cefcLongProcs,
    cefcLongParamLists,
    cefcEmptyProcs,
    cefcNestedProcs,
    cefcUnnamedConsts,
    cefcPublishedPropWithoutDefault,
    cefcUnsortedClassVisibility,
    cefcEmptyClassSections,
    cefcUnsortedClassMembers,
    cefcToDos
    );
  TCEFigureCategories = set of TCEFigureCategory;

const
  FirstCodeExplorerCategory = cecUses;
  DefaultCodeExplorerCategories = [cecUses,
                              cecTypes,cecVariables,cecConstants,cecProcedures];
  cefcAll = [low(TCEFigureCategory)..high(TCEFigureCategory)];
  DefaultCodeExplorerFigureCategories = cefcAll;
  DefaultFigLongProcLineCount = 50;
  DefaultFigLongParamListCount = 6;
  DefaultFigNestedProcCount = 3;
  DefaultFigureCharConst = false;
  DefaultNotFigureConstants: array[1..2] of ansistring // Note: keep this asciiz
    = (
    '0',
    '1'
    );

type

  TCodeExplorerOptions = class(TAbstractIDEOptions)
  private
    FCategories: TCodeExplorerCategories;
    FFigureCharConst: boolean;
    FLongParamListCount: integer;
    FLongProcLineCount: integer;
    FNestedProcCount: integer;
    FFigures: TCEFigureCategories;
    FFollowCursor: boolean;
    FMode : TCodeExplorerMode;
    FNotFigureConstants: TAvgLvlTree;// tree of AnsiString
    FOptionsFilename: string;
    FRefresh: TCodeExplorerRefresh;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure Load;
    procedure Save;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function CreateListOfNotFigureConstants: TStrings;
    procedure ClearNotFigureConstants;
    procedure SetListOfNotFigureConstants(List: TStrings; Add: boolean);
    procedure LoadNotFigureConstantsToDefaults;
    function NotFigureConstant(p: PChar): boolean;// test if atom is in NotFigureConstants
    procedure AddNotFigureConstant(const Atom: string);
    function IsNotFigureConstantsDefault(Exactly: boolean): boolean;
    function IsNotFigureConstantDefault(const Atom: string): boolean;
  public
    property Refresh: TCodeExplorerRefresh read FRefresh write FRefresh default cerSwitchEditorPage;
    property Mode: TCodeExplorerMode read FMode write FMode default cemCategory;
    property OptionsFilename: string read FOptionsFilename write FOptionsFilename;
    property FollowCursor: boolean read FFollowCursor write FFollowCursor default true;
    property Categories: TCodeExplorerCategories read FCategories write FCategories default DefaultCodeExplorerCategories;
    // Figures
    property Figures: TCEFigureCategories read FFigures write FFigures default DefaultCodeExplorerFigureCategories;
    property LongProcLineCount: integer read FLongProcLineCount write FLongProcLineCount default DefaultFigLongProcLineCount;
    property LongParamListCount: integer read FLongParamListCount write FLongParamListCount default DefaultFigLongParamListCount;
    property NestedProcCount: integer read FNestedProcCount write FNestedProcCount default DefaultFigNestedProcCount;
    property FigureCharConst: boolean read FFigureCharConst write FFigureCharConst default DefaultFigureCharConst;
    property NotFigureConstants: TAvgLvlTree read FNotFigureConstants;
  end;

const
  CodeExplorerVersion = 1;

  cerDefault = cerSwitchEditorPage;

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
    'Figures'
    );
  CEFigureCategoryNames: array[TCEFigureCategory] of string = (
    'LongProcs',
    'LongParamLists',
    'EmptyProcs',
    'NestedProcs',
    'UnnamedConsts',
    'PublishedPropWithoutDefault',
    'UnsortedClassVisibility',
    'EmptyClassSections',
    'UnsortedClassMembers',
    'ToDos'
    );

var
  CodeExplorerOptions: TCodeExplorerOptions = nil; // set by the IDE

function CodeExplorerRefreshNameToEnum(const s: string): TCodeExplorerRefresh;
function CodeExplorerModeNameToEnum(const s: string): TCodeExplorerMode;
function CodeExplorerCategoryNameToEnum(const s: string): TCodeExplorerCategory;
function CodeExplorerLocalizedString(const c: TCodeExplorerCategory): string;
function CodeExplorerFigureNameToEnum(const s: string): TCEFigureCategory;
function CodeExplorerLocalizedString(const c: TCEFigureCategory): string;


implementation


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
  cecFigures: Result:=lisCEFigures;
  else Result:='?';
  end;
end;

function CodeExplorerFigureNameToEnum(const s: string): TCEFigureCategory;
begin
  for Result:=low(TCEFigureCategory) to High(TCEFigureCategory) do
    if SysUtils.CompareText(CEFigureCategoryNames[Result],s)=0 then exit;
  Result:=cefcLongProcs;
end;

function CodeExplorerLocalizedString(const c: TCEFigureCategory): string;
begin
  case c of
  cefcLongProcs: Result:=lisCELongProcedures;
  cefcLongParamLists: Result:=lisCEManyParameters;
  cefcEmptyProcs: Result:=lisCEEmptyProcedures;
  cefcNestedProcs: Result:=lisCEManyNestedProcedures;
  cefcUnnamedConsts: Result:=lisCEUnnamedConstants;
  cefcPublishedPropWithoutDefault: Result:=lisCEPublishedPropertyWithoutDefault;
  cefcUnsortedClassVisibility: Result:=lisCEUnsortedVisibility;
  cefcEmptyClassSections: Result:=lisCEEmptyClassSections;
  cefcUnsortedClassMembers: Result:=lisCEUnsortedMembers;
  cefcToDos: Result:=lisCEToDos;
  else Result:='?';
  end;
end;

{ TCodeExplorerOptions }

constructor TCodeExplorerOptions.Create;
begin
  FOptionsFilename:=
                AppendPathDelim(GetPrimaryConfigPath)+'codeexploreroptions.xml';
  FNotFigureConstants:=TAvgLvlTree.Create(TListSortCompare(@CompareAtom));
  Clear;
  AddNotFigureConstant('0');
  AddNotFigureConstant('1');
end;

destructor TCodeExplorerOptions.Destroy;
begin
  ClearNotFigureConstants;
  FreeAndNil(FNotFigureConstants);
  inherited Destroy;
end;

procedure TCodeExplorerOptions.Clear;
begin
  FMode:=cemCategory;
  FRefresh:=cerDefault;
  FFollowCursor:=true;
  FCategories:=DefaultCodeExplorerCategories;
  FFigures:=DefaultCodeExplorerFigureCategories;
  FLongProcLineCount:=DefaultFigLongProcLineCount;
  FLongParamListCount:=DefaultFigLongParamListCount;
  FNestedProcCount:=DefaultFigNestedProcCount;
  FFigureCharConst:=DefaultFigureCharConst;
  ClearNotFigureConstants;
end;

procedure TCodeExplorerOptions.Assign(Source: TPersistent);
var
  Src: TCodeExplorerOptions;
  List: TStrings;
begin
  if Source is TCodeExplorerOptions then begin
    Src:=TCodeExplorerOptions(Source);
    FRefresh:=Src.Refresh;
    FMode:=Src.Mode;
    FFollowCursor:=Src.FollowCursor;
    FCategories:=Src.Categories;
    FFigures:=Src.Figures;
    FLongProcLineCount:=Src.LongProcLineCount;
    FLongParamListCount:=Src.LongParamListCount;
    FNestedProcCount:=Src.NestedProcCount;
    FFigureCharConst:=Src.FigureCharConst;
    List:=Src.CreateListOfNotFigureConstants;
    try
      SetListOfNotFigureConstants(List,false);
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
  if not FileExistsUTF8(FOptionsFilename) then begin
    Clear;
    exit;
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
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.CreateClean(FOptionsFilename);
    XMLConfig.SetDeleteValue('CodeExplorer/Version/Value',
      CodeExplorerVersion,0);
    SaveToXMLConfig(XMLConfig,'CodeExplorer/');
    XMLConfig.Flush;
    XMLConfig.Free;
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
  f: TCEFigureCategory;
  CurPath: String;
  List: TStringList;
begin
  Clear;
  FRefresh:=CodeExplorerRefreshNameToEnum(
                                   XMLConfig.GetValue(Path+'Refresh/Value',''));
  FMode:=CodeExplorerModeNameToEnum(
                                   XMLConfig.GetValue(Path+'Mode/Value',''));
  FFollowCursor:=XMLConfig.GetValue(Path+'FollowCursor',true);
  
  FCategories:=[];
  for c:=FirstCodeExplorerCategory to high(TCodeExplorerCategory) do
    if XMLConfig.GetValue(Path+'Categories/'+CodeExplorerCategoryNames[c],
      c in DefaultCodeExplorerCategories) then
        Include(FCategories,c);
  FFigures:=[];
  for f:=low(TCEFigureCategory) to high(TCEFigureCategory) do
  begin
    CurPath:=Path+'Figures/'+CEFigureCategoryNames[f]+'/';
    if XMLConfig.GetValue(CurPath+'Show',f in DefaultCodeExplorerFigureCategories)
    then
      Include(FFigures,f);
    case f of
    cefcLongProcs:
      FLongProcLineCount:=XMLConfig.GetValue(CurPath+'LineCount/Value',
                                                 DefaultFigLongProcLineCount);
    cefcLongParamLists:
      FLongParamListCount:=XMLConfig.GetValue(CurPath+'Count/Value',
                                                 DefaultFigLongParamListCount);
    cefcNestedProcs:
      FNestedProcCount:=XMLConfig.GetValue(CurPath+'Count/Value',
                                                 DefaultFigNestedProcCount);
    cefcUnnamedConsts:
      begin
        FFigureCharConst:=XMLConfig.GetValue(CurPath+'CharConsts/Value',
                                                 DefaultFigureCharConst);
        // load standard NotFigureConstants
        if XMLConfig.GetValue(CurPath+'Ignore/ContainsDefaults',true) then
          LoadNotFigureConstantsToDefaults;
        // load custom NotFigureConstants
        List:=TStringList.Create;
        try
          LoadStringList(XMLConfig,List,CurPath+'Ignore/');
          SetListOfNotFigureConstants(List,true);
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
  f: TCEFigureCategory;
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
  
  for c:=FirstCodeExplorerCategory to high(TCodeExplorerCategory) do
    XMLConfig.SetDeleteValue(Path+'Categories/'+CodeExplorerCategoryNames[c],
      c in FCategories,c in DefaultCodeExplorerCategories);
  for f:=low(TCEFigureCategory) to high(TCEFigureCategory) do
  begin
    CurPath:=Path+'Figures/'+CEFigureCategoryNames[f]+'/';
    XMLConfig.SetDeleteValue(CurPath+'Show',
      f in FFigures,f in DefaultCodeExplorerFigureCategories);
    case f of
    cefcLongProcs:
      XMLConfig.SetDeleteValue(CurPath+'LineCount/Value',
                           FLongProcLineCount,DefaultFigLongProcLineCount);
    cefcLongParamLists:
      XMLConfig.SetDeleteValue(CurPath+'Count/Value',
                           FLongParamListCount,DefaultFigLongParamListCount);
    cefcNestedProcs:
      XMLConfig.SetDeleteValue(CurPath+'Count/Value',
                           FNestedProcCount,DefaultFigNestedProcCount);
    cefcUnnamedConsts:
      begin
        XMLConfig.SetDeleteValue(CurPath+'CharConsts/Value',
                           FFigureCharConst,DefaultFigureCharConst);
        // save standard NotFigureConstants
        ContainsDefaults:=IsNotFigureConstantsDefault(false);
        XMLConfig.SetDeleteValue(CurPath+'Ignore/ContainsDefaults',
           ContainsDefaults,true);
        // save NotFigureConstants
        List:=CreateListOfNotFigureConstants;
        try
          for i:=List.Count-1 downto 0 do
            if IsNotFigureConstantDefault(List[i]) then
              List.Delete(i);
          SaveStringList(XMLConfig,List,CurPath+'Ignore/');
        finally
          List.Free;
        end;
      end;
    end;
  end;

end;

function TCodeExplorerOptions.CreateListOfNotFigureConstants: TStrings;
var
  AVLNode: TAvgLvlTreeNode;
  i: Integer;
  s: String;
begin
  Result:=TStringList.Create;
  AVLNode:=NotFigureConstants.FindLowest;
  i:=0;
  while AVLNode<>nil do begin
    s:=GetAtomString(PChar(AVLNode.Data),false);
    if s<>'' then begin
      inc(i);
      Result.Add(s);
    end;
    AVLNode:=NotFigureConstants.FindSuccessor(AVLNode);
  end;
end;

procedure TCodeExplorerOptions.ClearNotFigureConstants;
var
  AVLNode: TAvgLvlTreeNode;
  s: String;
begin
  s:='';
  AVLNode:=FNotFigureConstants.FindLowest;
  while AVLNode<>nil do begin
    // decrease reference counter
    Pointer(s):=AVLNode.Data;
    s:='';
    AVLNode:=FNotFigureConstants.FindSuccessor(AVLNode);
  end;
  if s='' then ; // omit fpc note
  FNotFigureConstants.Clear;
end;

procedure TCodeExplorerOptions.SetListOfNotFigureConstants(List: TStrings;
  Add: boolean);
var
  i: Integer;
begin
  if not Add then
    ClearNotFigureConstants;
  for i:=0 to List.Count-1 do
    AddNotFigureConstant(List[i]);
end;

procedure TCodeExplorerOptions.LoadNotFigureConstantsToDefaults;
var
  i: Integer;
begin
  ClearNotFigureConstants;
  for i:=low(DefaultNotFigureConstants) to high(DefaultNotFigureConstants) do
    AddNotFigureConstant(DefaultNotFigureConstants[i]);
end;

function TCodeExplorerOptions.NotFigureConstant(p: PChar): boolean;
begin
  Result:=FNotFigureConstants.Find(p)<>nil;
end;

procedure TCodeExplorerOptions.AddNotFigureConstant(const Atom: string);
var
  s: String;
begin
  if Atom='' then exit;
  if NotFigureConstant(PChar(Atom)) then exit;
  s:=Atom;
  FNotFigureConstants.Add(Pointer(s));
  Pointer(s):=nil;
end;

function TCodeExplorerOptions.IsNotFigureConstantsDefault(Exactly: boolean
  ): boolean;
const
  DefCount = high(DefaultNotFigureConstants)-Low(DefaultNotFigureConstants)+1;
var
  i: Integer;
begin
  if Exactly and (FNotFigureConstants.Count=DefCount) then
    exit(false);
  if FNotFigureConstants.Count<DefCount then exit(false);
  for i:=low(DefaultNotFigureConstants) to high(DefaultNotFigureConstants) do
    if not NotFigureConstant(PChar(DefaultNotFigureConstants[i])) then exit(false);
  Result:=true;
end;

function TCodeExplorerOptions.IsNotFigureConstantDefault(const Atom: string
  ): boolean;
var
  i: Integer;
begin
  for i:=low(DefaultNotFigureConstants) to high(DefaultNotFigureConstants) do
    if CompareAtom(PChar(Atom),PChar(DefaultNotFigureConstants[i]),false)=0 then
      exit(true);
  Result:=false;
end;

initialization
  RegisterIDEOptionsGroup(GroupCodeExplorer, dlgGroupCodeExplorer);
end.


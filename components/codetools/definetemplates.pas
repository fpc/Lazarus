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

  Author: Mattias Gaertner

  Abstract:
    This unit is a support unit for the code tools. It manages compilation
    information, which is not stored in the source, like Makefile information
    and compiler command line options. This information is needed to
    successfully find the right units, include files, predefined variables,
    etc..
    
    The information is stored in a TDefineTree, which contains nodes of type
    TDefineTemplate. Each TDefineTemplate is a tree of defines, undefines,
    definerecurses, ifdefs, ifndefs, elses, elseifs, directories ... .
    
    Simply give a TDefineTree a directory and it will return all predefined
    variables for that directory. These values can be used to parse a unit in
    the directory.
    
    TDefineTree can be saved to and loaded from a XML file.
    
    The TDefinePool contains a list of TDefineTemplate trees, and can generate
    some default templates for Lazarus and FPC sources.
    
  ToDo:
    Better Error handling of DefinePool
}
unit DefineTemplates;

{$mode objfpc}{$H+}

{ $Define VerboseDefineCache}
{ $Define VerboseFPCSrcScan}
{ $Define ShowTriedFiles}

interface

uses
  Classes, SysUtils, CodeToolsStrConsts, ExprEval, DirectoryCacher,
  Laz_XMLCfg, AVL_Tree,
  Process, KeywordFuncLists, FileProcs;

const
  ExternalMacroStart = ExprEval.ExternalMacroStart;

  // Standard Template Names (do not translate them)
  StdDefTemplFPC            = 'Free Pascal Compiler';
  StdDefTemplFPCSrc         = 'Free Pascal sources';
  StdDefTemplLazarusSources = 'Lazarus sources';
  StdDefTemplLazarusSrcDir  = 'Lazarus source directory';
  StdDefTemplLazarusBuildOpts = 'Lazarus build options';
  StdDefTemplLCLProject     = 'LCL project';

  // Standard macros
  DefinePathMacroName      = ExternalMacroStart+'DefinePath';
  UnitPathMacroName        = ExternalMacroStart+'UnitPath';
  IncludePathMacroName     = ExternalMacroStart+'IncPath';
  SrcPathMacroName         = ExternalMacroStart+'SrcPath';
  PPUSrcPathMacroName      = ExternalMacroStart+'PPUSrcPath';
  PPWSrcPathMacroName      = ExternalMacroStart+'PPWSrcPath';
  DCUSrcPathMacroName      = ExternalMacroStart+'DCUSrcPath';
  CompiledSrcPathMacroName = ExternalMacroStart+'CompiledSrcPath';
  UnitLinksMacroName       = ExternalMacroStart+'UnitLinks';
  FPCUnitPathMacroName     = ExternalMacroStart+'FPCUnitPath';

  // virtual directories
  VirtualDirectory='VIRTUALDIRECTORY';
  VirtualTempDir='TEMPORARYDIRECTORY';
  
  // FPC operating systems and processor types
  FPCOperatingSystemNames: array[1..20] of shortstring =(
      'linux', 'freebsd', 'openbsd', 'netbsd', 'win32', 'wince',
      'go32v1', 'go32v2',
      'beos', 'os2', 'amiga', 'atari', 'sunos', 'palmos', 'qnx', 'watcom',
      'emx', 'darwin', 'wdosx', 'netware'
    );
  FPCOperatingSystemAlternativeNames: array[1..2] of shortstring =(
      'unix', 'win' // see GetDefaultSrcOSForTargetOS
    );
  FPCOperatingSystemAlternative2Names: array[1..1] of shortstring =(
      'bsd' // see GetDefaultSrcOS2ForTargetOS
    );
  FPCProcessorNames: array[1..6] of shortstring =(
      'i386', 'powerpc', 'm68k', 'x86_64', 'sparc', 'arm'
    );

  Lazarus_CPU_OS_Widget_Combinations: array[1..31] of shortstring = (
    'i386-linux-gtk',
    'i386-linux-gnome',
    'i386-linux-gtk2',
    'i386-linux-qt',
    'i386-freebsd-gtk',
    'i386-freebsd-gnome',
    'i386-freebsd-gtk2',
    'i386-freebsd-qt',
    'i386-openbsd-gtk',
    'i386-openbsd-gnome',
    'i386-openbsd-gtk2',
    'i386-openbsd-qt',
    'i386-netbsd-gtk',
    'i386-netbsd-gnome',
    'i386-netbsd-gtk2',
    'i386-netbsd-qt',
    'i386-win32-win32',
    'i386-win32-wince',
    'i386-win32-gtk',
    'i386-darwin-gtk',
    'i386-darwin-gtk2',
    'i386-darwin-carbon',
    'i386-darwin-qt',
    'powerpc-darwin-gtk',
    'powerpc-darwin-gtk2',
    'powerpc-darwin-carbon',
    'powerpc-linux-gtk',
    'powerpc-linux-gtk2',
    'sparc-linux-gtk',
    'sparc-linux-gtk2',
    'arm-wince-wince'
    );

type
  //---------------------------------------------------------------------------
  // TDefineTemplate stores a define action, the variablename and the value
  TDefineAction = (
     da_None,
     da_Block,
     da_Define,
     da_DefineRecurse,
     da_Undefine,
     da_UndefineRecurse,
     da_UndefineAll,
     da_If,
     da_IfDef,
     da_IfNDef,
     da_ElseIf,
     da_Else,
     da_Directory
     );

const
  DefineActionBlocks = [da_Block, da_Directory, da_If, da_IfDef, da_IfNDef,
                        da_ElseIf, da_Else];
  DefineActionDefines = [da_Define,da_DefineRecurse,da_Undefine,
                         da_UndefineRecurse,da_UndefineAll];
  DefineActionNames: array[TDefineAction] of string = (
      'None', 'Block', 'Define', 'DefineRecurse', 'Undefine', 'UndefineRecurse',
      'UndefineAll', 'If', 'IfDef', 'IfNDef', 'ElseIf', 'Else', 'Directory'
    );

type
  TDefineTree = class;
  TDefineTemplateFlag = (
    dtfAutoGenerated
    );
  TDefineTemplateFlags = set of TDefineTemplateFlag;
  
  TDefineTemplate = class
  private
    FChildCount: integer;
    FFirstChild: TDefineTemplate;
    FLastChild: TDefineTemplate;
    FMarked: boolean;
    FMergeNameBehind: string;
    FMergeNameInFront: string;
    FNext: TDefineTemplate;
    FParent: TDefineTemplate;
    FPrior: TDefineTemplate;
  public
    Name: string;
    Description: string;
    Variable: string;
    Value: string;
    Action: TDefineAction;
    Flags: TDefineTemplateFlags;
    Owner: TObject;
    class procedure MergeTemplates(ParentDefTempl: TDefineTemplate;
                  var FirstSibling, LastSibling:TDefineTemplate;
                  SourceTemplate: TDefineTemplate; WithSiblings: boolean;
                  const NewNamePrefix: string);
    class procedure MergeXMLConfig(ParentDefTempl: TDefineTemplate;
                  var FirstSibling, LastSibling:TDefineTemplate;
                  XMLConfig: TXMLConfig; const Path, NewNamePrefix: string);
    constructor Create(const AName, ADescription, AVariable, AValue: string;
                       AnAction: TDefineAction);
    constructor Create;
    destructor Destroy; override;
    function  ConsistencyCheck: integer; // 0 = ok
    function  CreateCopy(OnlyMarked: boolean = false;
                         WithSiblings: boolean = true;
                         WithChilds: boolean = true): TDefineTemplate;
    function  CreateMergeCopy: TDefineTemplate;
    function  FindByName(const AName: string;
                     WithSubChilds, WithNextSiblings: boolean): TDefineTemplate;
    function  FindChildByName(const AName: string): TDefineTemplate;
    function  FindRoot: TDefineTemplate;
    function  FindUniqueName(const Prefix: string): string;
    function  GetFirstSibling: TDefineTemplate;
    function  HasDefines(OnlyMarked, WithSiblings: boolean): boolean;
    function  IsAutoGenerated: boolean;
    function  IsEqual(ADefineTemplate: TDefineTemplate;
                      CheckSubNodes, CheckNextSiblings: boolean): boolean;
    function  Level: integer;
    function  LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                ClearOldSiblings, WithMergeInfo: boolean): boolean;
    function  SelfOrParentContainsFlag(AFlag: TDefineTemplateFlag): boolean;
    procedure AddChild(ADefineTemplate: TDefineTemplate);
    procedure ReplaceChild(ADefineTemplate: TDefineTemplate);
    function DeleteChild(const AName: string): boolean;
    procedure Assign(ADefineTemplate: TDefineTemplate; WithSubNodes,
                     WithNextSiblings, ClearOldSiblings: boolean); virtual;
    procedure AssignValues(ADefineTemplate: TDefineTemplate);
    procedure Clear(WithSiblings: boolean);
    procedure CreateMergeInfo(WithSiblings, OnlyMarked: boolean);
    procedure InheritMarks(WithSiblings, WithChilds, Down, Up: boolean);
    procedure InsertBehind(APrior: TDefineTemplate);
    procedure InsertInFront(ANext: TDefineTemplate);
    procedure LoadValuesFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                      WithMergeInfo: boolean);
    procedure MarkFlags(const MustFlags, NotFlags: TDefineTemplateFlags;
                        WithSiblings, WithChilds: boolean);
    procedure MarkNodes(WithSiblings, WithChilds: boolean);
    procedure MarkOwnedBy(TheOwner: TObject;
                          const MustFlags, NotFlags: TDefineTemplateFlags;
                          WithSiblings, WithChilds: boolean);
    procedure RemoveFlags(TheFlags: TDefineTemplateFlags);
    procedure RemoveLeaves(TheOwner: TObject; const MustFlags,
                           NotFlags: TDefineTemplateFlags;
                           WithSiblings: boolean;
                           var FirstDefTemplate: TDefineTemplate);
    procedure RemoveMarked(WithSiblings: boolean;
                           var FirstDefTemplate: TDefineTemplate);
    procedure RemoveOwner(TheOwner: TObject; WithSiblings: boolean);
    procedure ReverseMarks(WithSiblings, WithChilds: boolean);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              WithSiblings, OnlyMarked,
                              WithMergeInfo, UpdateMergeInfo: boolean);
    procedure SaveValuesToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                    WithMergeInfo: boolean);
    procedure SetDefineOwner(NewOwner: TObject; WithSiblings: boolean);
    procedure SetFlags(AddFlags, SubFlags: TDefineTemplateFlags;
                       WithSiblings: boolean);
    procedure Unbind;
    procedure UnmarkNodes(WithSiblings, WithChilds: boolean);
    procedure WriteDebugReport(OnlyMarked: boolean);
  public
    property ChildCount: integer read FChildCount;
    property FirstChild: TDefineTemplate read FFirstChild;
    property LastChild: TDefineTemplate read FLastChild;
    property Marked: boolean read FMarked write FMarked;
    property Next: TDefineTemplate read FNext;
    property Parent: TDefineTemplate read FParent;
    property Prior: TDefineTemplate read FPrior;
    property MergeNameInFront: string read FMergeNameInFront write FMergeNameInFront;
    property MergeNameBehind: string read FMergeNameBehind write FMergeNameBehind;
  end;

  //---------------------------------------------------------------------------
  //
  TDirectoryDefines = class
  public
    Path: string;
    Values: TExpressionEvaluator;
    constructor Create;
    destructor Destroy; override;
  end;
  
  TOnGetVirtualDirectoryDefines = procedure(Sender: TDefineTree;
    Defines: TDirectoryDefines) of object;

  //---------------------------------------------------------------------------
  // TDefineTree caches the define values for directories
  TOnReadValue = procedure(Sender: TObject; const VariableName: string;
                          var Value: string; var Handled: boolean) of object;

  TOnGetVirtualDirectoryAlias = procedure(Sender: TObject;
    var RealDir: string) of object;
    
  TReadFunctionData = record
    Param: string;
    Result: string;
  end;
  PReadFunctionData = ^TReadFunctionData;
  
  TDefTreeCalculate = procedure(Tree: TDefineTree; Node: TDefineTemplate;
    ValueParsed: boolean; const ParsedValue: string;
    ExpressionCalculated: boolean; const ExpressionResult: string;
    Execute: boolean) of object;

  TDefineTree = class
  private
    FDirectoryCachePool: TCTDirectoryCachePool;
    FFirstDefineTemplate: TDefineTemplate;
    FCache: TAVLTree; // tree of TDirectoryDefines
    FChangeStep: integer;
    FErrorDescription: string;
    FErrorTemplate: TDefineTemplate;
    FMacroFunctions: TKeyWordFunctionList;
    FMacroVariables: TKeyWordFunctionList;
    FOnCalculate: TDefTreeCalculate;
    FOnGetVirtualDirectoryAlias: TOnGetVirtualDirectoryAlias;
    FOnGetVirtualDirectoryDefines: TOnGetVirtualDirectoryDefines;
    FOnPrepareTree: TNotifyEvent;
    FOnReadValue: TOnReadValue;
    FVirtualDirCache: TDirectoryDefines;
    function Calculate(DirDef: TDirectoryDefines): boolean;
    procedure IncreaseChangeStep;
    procedure SetDirectoryCachePool(const AValue: TCTDirectoryCachePool);
  protected
    function FindDirectoryInCache(const Path: string): TDirectoryDefines;
    function GetDirDefinesForDirectory(const Path: string;
                                    WithVirtualDir: boolean): TDirectoryDefines;
    function GetDirDefinesForVirtualDirectory: TDirectoryDefines;
    function MacroFuncExtractFileExt(Data: Pointer): boolean;
    function MacroFuncExtractFilePath(Data: Pointer): boolean;
    function MacroFuncExtractFileName(Data: Pointer): boolean;
    function MacroFuncExtractFileNameOnly(Data: Pointer): boolean;
    procedure DoClearCache;
    procedure DoPrepareTree;
  public
    property RootTemplate: TDefineTemplate
                           read FFirstDefineTemplate write FFirstDefineTemplate;
    property ChangeStep: integer read FChangeStep;
    property ErrorTemplate: TDefineTemplate read FErrorTemplate;
    property ErrorDescription: string read FErrorDescription;
    property OnGetVirtualDirectoryAlias: TOnGetVirtualDirectoryAlias
             read FOnGetVirtualDirectoryAlias write FOnGetVirtualDirectoryAlias;
    property OnGetVirtualDirectoryDefines: TOnGetVirtualDirectoryDefines
         read FOnGetVirtualDirectoryDefines write FOnGetVirtualDirectoryDefines;
    property OnReadValue: TOnReadValue read FOnReadValue write FOnReadValue;
    property OnPrepareTree: TNotifyEvent read FOnPrepareTree write FOnPrepareTree;
    property OnCalculate: TDefTreeCalculate read FOnCalculate write FOnCalculate;
    property MacroFunctions: TKeyWordFunctionList read FMacroFunctions;
    property MacroVariables: TKeyWordFunctionList read FMacroVariables;
  public
    constructor Create;
    destructor Destroy; override;
    function  ConsistencyCheck: integer; // 0 = ok
    function  ExtractNonAutoCreated: TDefineTemplate;
    function  ExtractTemplatesOwnedBy(TheOwner: TObject; const MustFlags,
                               NotFlags: TDefineTemplateFlags): TDefineTemplate;
    function  FindDefineTemplateByName(const AName: string;
                                       OnlyRoots: boolean): TDefineTemplate;
    function  GetCompiledSrcPathForDirectory(const Directory: string): string;
    function  GetDCUSrcPathForDirectory(const Directory: string): string;
    function  GetDefinesForDirectory(const Path: string;
                                 WithVirtualDir: boolean): TExpressionEvaluator;
    function  GetDefinesForVirtualDirectory: TExpressionEvaluator;
    function  GetIncludePathForDirectory(const Directory: string): string;
    function  GetLastRootTemplate: TDefineTemplate;
    function  GetPPUSrcPathForDirectory(const Directory: string): string;
    function  GetPPWSrcPathForDirectory(const Directory: string): string;
    function  GetSrcPathForDirectory(const Directory: string): string;
    function  GetUnitPathForDirectory(const Directory: string): string;
    function  IsEqual(SrcDefineTree: TDefineTree): boolean;
    procedure Add(ADefineTemplate: TDefineTemplate);
    procedure AddChild(ParentTemplate, NewDefineTemplate: TDefineTemplate);
    procedure AddFirst(ADefineTemplate: TDefineTemplate);
    procedure Assign(SrcDefineTree: TDefineTree);
    procedure AssignNonAutoCreated(SrcDefineTree: TDefineTree);
    procedure Clear;
    procedure ClearCache;
    procedure MarkNonAutoCreated;
    procedure MarkTemplatesOwnedBy(TheOwner: TObject;
                               const MustFlags, NotFlags: TDefineTemplateFlags);
    procedure MergeDefineTemplates(SourceTemplate: TDefineTemplate;
                                   const NewNamePrefix: string);
    procedure MergeTemplates(SourceTemplate: TDefineTemplate;
                             const NewNamePrefix: string);
    procedure ReadValue(const DirDef: TDirectoryDefines;
                   const PreValue, CurDefinePath: string; out NewValue: string);
    procedure RemoveDefineTemplate(ADefTempl: TDefineTemplate);
    procedure RemoveMarked;
    procedure RemoveRootDefineTemplateByName(const AName: string);
    procedure RemoveTemplatesOwnedBy(TheOwner: TObject;
                               const MustFlags, NotFlags: TDefineTemplateFlags);
    procedure ReplaceChild(ParentTemplate, NewDefineTemplate: TDefineTemplate;
                           const ChildName: string);
    procedure ReplaceRootSameName(ADefineTemplate: TDefineTemplate);
    procedure ReplaceRootSameName(const Name: string;
                                  ADefineTemplate: TDefineTemplate);
    procedure ReplaceRootSameNameAddFirst(ADefineTemplate: TDefineTemplate);
    procedure WriteDebugReport;
    property DirectoryCachePool: TCTDirectoryCachePool read FDirectoryCachePool write SetDirectoryCachePool;
  end;

  //---------------------------------------------------------------------------
  TDefinePool = class
  private
    FEnglishErrorMsgFilename: string;
    FItems: TFPList; // list of TDefineTemplate;
    function GetItems(Index: integer): TDefineTemplate;
    procedure SetEnglishErrorMsgFilename(const AValue: string);
  public
    property Items[Index: integer]: TDefineTemplate read GetItems; default;
    function Count: integer;
    procedure Add(ADefineTemplate: TDefineTemplate);
    procedure Insert(Index: integer; ADefineTemplate: TDefineTemplate);
    procedure Delete(Index: integer);
    procedure Move(SrcIndex, DestIndex: integer);
    property EnglishErrorMsgFilename: string
        read FEnglishErrorMsgFilename write SetEnglishErrorMsgFilename;
    // FPC templates
    function CreateFPCTemplate(const CompilerPath, CompilerOptions,
                               TestPascalFile: string;
                               out UnitSearchPath, TargetOS,
                               TargetProcessor: string;
                               Owner: TObject): TDefineTemplate;
    function CreateFPCSrcTemplate(const FPCSrcDir, UnitSearchPath, PPUExt,
                          DefaultTargetOS, DefaultProcessorName: string;
                          UnitLinkListValid: boolean; var UnitLinkList: string;
                          Owner: TObject): TDefineTemplate;
    function CreateFPCCommandLineDefines(const Name, CmdLine: string;
                                         RecursiveDefines: boolean;
                                         Owner: TObject): TDefineTemplate;
    // Lazarus templates
    function CreateLazarusSrcTemplate(
                          const LazarusSrcDir, WidgetType, ExtraOptions: string;
                          Owner: TObject): TDefineTemplate;
    function CreateLCLProjectTemplate(const LazarusSrcDir, WidgetType,
                          ProjectDir: string; Owner: TObject): TDefineTemplate;
    // Delphi templates
    function CreateDelphiSrcPath(DelphiVersion: integer;
                                 const PathPrefix: string): string;
    function CreateDelphiCompilerDefinesTemplate(DelphiVersion: integer;
                                               Owner: TObject): TDefineTemplate;
    function CreateDelphiDirectoryTemplate(const DelphiDirectory: string;
                       DelphiVersion: integer; Owner: TObject): TDefineTemplate;
    function CreateDelphiProjectTemplate(const ProjectDir,
                                 DelphiDirectory: string; DelphiVersion: integer;
                                 Owner: TObject): TDefineTemplate;
    // Kylix templates
    function CreateKylixCompilerDefinesTemplate(KylixVersion: integer;
                                               Owner: TObject): TDefineTemplate;
    function CreateKylixSrcPath(KylixVersion: integer;
                                const PathPrefix: string): string;
    function CreateKylixDirectoryTemplate(const KylixDirectory: string;
                        KylixVersion: integer; Owner: TObject): TDefineTemplate;
    function CreateKylixProjectTemplate(const ProjectDir,
                                 KylixDirectory: string; KylixVersion: integer;
                                 Owner: TObject): TDefineTemplate;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;
  
const
  DefineTemplateFlagNames: array[TDefineTemplateFlag] of shortstring = (
      'AutoGenerated'
    );
  
function DefineActionNameToAction(const s: string): TDefineAction;
function DefineTemplateFlagsToString(Flags: TDefineTemplateFlags): string;
function GetDefaultSrcOSForTargetOS(const TargetOS: string): string;
function GetDefaultSrcOS2ForTargetOS(const TargetOS: string): string;
procedure SplitLazarusCPUOSWidgetCombo(const Combination: string;
  var CPU, OS, WidgetSet: string);

// functions to quickly setup some defines
function CreateDefinesInDirectories(const SourcePaths, FlagName: string
                                    ): TDefineTemplate;


implementation


type
  TDefTemplUnitNameLink = class
  public
    UnitName: string;
    Filename: string;
    MacroCount: integer;
    UsedMacroCount: integer;
    Priority: integer;
  end;

// some useful functions

function DefineActionNameToAction(const s: string): TDefineAction;
begin
  for Result:=Low(TDefineAction) to High(TDefineAction) do
    if CompareText(s,DefineActionNames[Result])=0 then exit;
  Result:=da_None;
end;

function DefineTemplateFlagsToString(Flags: TDefineTemplateFlags): string;
var f: TDefineTemplateFlag;
begin
  Result:='';
  for f:=Low(TDefineTemplateFlag) to High(TDefineTemplateFlag) do begin
    if f in Flags then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+DefineTemplateFlagNames[f];
    end;
  end;
end;

function CompareUnitLinkNodes(NodeData1, NodeData2: pointer): integer;
var Link1, Link2: TDefTemplUnitNameLink;
begin
  Link1:=TDefTemplUnitNameLink(NodeData1);
  Link2:=TDefTemplUnitNameLink(NodeData2);
  Result:=CompareText(Link1.UnitName,Link2.UnitName);
end;

function CompareUnitNameWithUnitLinkNode(UnitName: Pointer;
  NodeData: pointer): integer;
begin
  Result:=CompareText(String(UnitName),TDefTemplUnitNameLink(NodeData).UnitName);
end;

function CompareDirectoryDefines(NodeData1, NodeData2: pointer): integer;
var DirDef1, DirDef2: TDirectoryDefines;
begin
  DirDef1:=TDirectoryDefines(NodeData1);
  DirDef2:=TDirectoryDefines(NodeData2);
  Result:=CompareFilenames(DirDef1.Path,DirDef2.Path);
end;

function GetDefaultSrcOSForTargetOS(const TargetOS: string): string;
begin
  Result:='';
  if (CompareText(TargetOS,'linux')=0)
  or (CompareText(TargetOS,'freebsd')=0)
  or (CompareText(TargetOS,'netbsd')=0)
  or (CompareText(TargetOS,'openbsd')=0)
  or (CompareText(TargetOS,'darwin')=0)
  then
    Result:='unix'
  else
  if (CompareText(TargetOS,'win32')=0)
  or (CompareText(TargetOS,'win64')=0)
  or (CompareText(TargetOS,'wince')=0)
  then
    Result:='win';
end;

function GetDefaultSrcOS2ForTargetOS(const TargetOS: string): string;
begin
  Result:='';
  if (CompareText(TargetOS,'freebsd')=0)
  or (CompareText(TargetOS,'netbsd')=0)
  or (CompareText(TargetOS,'openbsd')=0)
  or (CompareText(TargetOS,'darwin')=0)
  then
    Result:='bsd';
end;

procedure SplitLazarusCPUOSWidgetCombo(const Combination: string;
  var CPU, OS, WidgetSet: string);
var
  StartPos, EndPos: integer;
begin
  StartPos:=1;
  EndPos:=StartPos;
  while (EndPos<=length(Combination)) and (Combination[EndPos]<>'-') do
    inc(EndPos);
  CPU:=copy(Combination,StartPos,EndPos-StartPos);
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Combination)) and (Combination[EndPos]<>'-') do
    inc(EndPos);
  OS:=copy(Combination,StartPos,EndPos-StartPos);
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Combination)) and (Combination[EndPos]<>'-') do
    inc(EndPos);
  WidgetSet:=copy(Combination,StartPos,EndPos-StartPos);
end;

function CreateDefinesInDirectories(const SourcePaths, FlagName: string
  ): TDefineTemplate;
var
  StartPos: Integer;
  EndPos: LongInt;
  CurDirectory: String;
  DirsTempl: TDefineTemplate;
  DirTempl: TDefineTemplate;
  SetFlagTempl: TDefineTemplate;
begin
  // create a block template for the directories
  DirsTempl:=TDefineTemplate.Create(FlagName,
    'Block of directories to set '+FlagName,
    '','',da_Block);

  // create a define flag for every directory
  StartPos:=1;
  while StartPos<=length(SourcePaths) do begin
    EndPos:=StartPos;
    while (EndPos<=length(SourcePaths)) and (SourcePaths[EndPos]<>';') do
      inc(EndPos);
    if EndPos>StartPos then begin
      CurDirectory:=copy(SourcePaths,StartPos,EndPos-StartPos);
      DirTempl:=TDefineTemplate.Create('FlagDirectory','FlagDirectory',
        '',CurDirectory,da_Directory);
      SetFlagTempl:=TDefineTemplate.Create(FlagName,FlagName,
        FlagName,'1',da_Define);
      DirTempl.AddChild(SetFlagTempl);
      DirsTempl.AddChild(DirTempl);
    end;
    StartPos:=EndPos+1;
  end;
  
  Result:=DirsTempl;
end;

{ TDefineTemplate }

procedure TDefineTemplate.MarkFlags(
  const MustFlags, NotFlags: TDefineTemplateFlags;
  WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=ANode.FMarked
                   or (((ANode.Flags*MustFlags)=MustFlags)
                   and (ANode.Flags*NotFlags=[]));
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkFlags(MustFlags,NotFlags,true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.MarkOwnedBy(TheOwner: TObject;
  const MustFlags, NotFlags: TDefineTemplateFlags;
  WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=ANode.FMarked
                   or ((ANode.Owner=TheOwner)
                       and ((ANode.Flags*MustFlags)=MustFlags)
                       and (ANode.Flags*NotFlags=[]));
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkOwnedBy(TheOwner,MustFlags,NotFlags,true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.MarkNodes(WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=true;
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkNodes(true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.ReverseMarks(WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=not ANode.FMarked;
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkNodes(true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.InheritMarks(WithSiblings, WithChilds, Down,
  Up: boolean);
var
  ANode: TDefineTemplate;
  ChildNode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    if WithChilds then begin
      ChildNode:=ANode.FirstChild;
      while ChildNode<>nil do begin
        if Down and ANode.FMarked then
          ChildNode.FMarked:=true;
        ChildNode.InheritMarks(false,true,Down,Up);
        if Up and ChildNode.FMarked then
          ANode.FMarked:=true;
        ChildNode:=ChildNode.Next;
      end;
    end;
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.UnmarkNodes(WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=false;
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.UnmarkNodes(true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.RemoveMarked(WithSiblings: boolean;
  var FirstDefTemplate: TDefineTemplate);
var ANode, NextNode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    NextNode:=ANode.Next;
    if ANode.FirstChild<>nil then begin
      ANode.FirstChild.RemoveMarked(true,FirstDefTemplate);
    end;
    if ANode.FMarked and (ANode.FirstChild=nil) then begin
      if ANode=FirstDefTemplate then FirstDefTemplate:=ANode.Next;
      ANode.Unbind;
      ANode.Free;
    end;
    if not WithSiblings then break;
    ANode:=NextNode;
  end;
end;

procedure TDefineTemplate.RemoveOwner(TheOwner: TObject; WithSiblings: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    if ANode.FFirstChild<>nil then
      ANode.FFirstChild.RemoveOwner(TheOwner,true);
    if ANode.Owner=TheOwner then ANode.Owner:=nil;
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.RemoveLeaves(TheOwner: TObject; const MustFlags,
  NotFlags: TDefineTemplateFlags; WithSiblings: boolean;
  var FirstDefTemplate: TDefineTemplate);
var ANode, NextNode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    NextNode:=ANode.Next;
    if ANode.FirstChild<>nil then
      ANode.FirstChild.RemoveLeaves(TheOwner,MustFlags,NotFlags,true,
                                    FirstDefTemplate);
    if ANode.FirstChild=nil then begin
      // this is a leaf
      if ((ANode.Owner=TheOwner)
      and ((ANode.Flags*MustFlags)=MustFlags)
      and (ANode.Flags*NotFlags=[]))
      then begin
        if ANode=FirstDefTemplate then
          FirstDefTemplate:=ANode.Next;
        ANode.Unbind;
        ANode.Free;
      end;
    end;
    if not WithSiblings then break;
    ANode:=NextNode;
  end;
end;

procedure TDefineTemplate.AddChild(ADefineTemplate: TDefineTemplate);
// add as last child
begin
  if ADefineTemplate=nil then exit;
  if LastChild=nil then begin
    while ADefineTemplate<>nil do begin
      ADefineTemplate.fParent:=Self;
      if ADefineTemplate.Prior=nil then FFirstChild:=ADefineTemplate;
      if ADefineTemplate.Next=nil then FLastChild:=ADefineTemplate;
      inc(FChildCount);
      ADefineTemplate:=ADefineTemplate.Next;
    end;
  end else begin
    ADefineTemplate.InsertBehind(LastChild);
  end;
end;

procedure TDefineTemplate.ReplaceChild(ADefineTemplate: TDefineTemplate);
var
  OldTempl: TDefineTemplate;
begin
  OldTempl:=FindChildByName(ADefineTemplate.Name);
  if OldTempl<>nil then begin
    ADefineTemplate.InsertInFront(OldTempl);
    OldTempl.UnBind;
    OldTempl.Free;
  end else
    AddChild(ADefineTemplate);
end;

function TDefineTemplate.DeleteChild(const AName: string): boolean;
var
  OldTempl: TDefineTemplate;
begin
  OldTempl:=FindChildByName(AName);
  if OldTempl<>nil then begin
    Result:=true;
    OldTempl.Unbind;
    OldTempl.Free;
  end else
    Result:=false;
end;

procedure TDefineTemplate.InsertBehind(APrior: TDefineTemplate);
// insert this and all next siblings behind APrior
var ANode, LastSibling, NewParent: TDefineTemplate;
begin
  if APrior=nil then exit;
  NewParent:=APrior.Parent;
  if Parent<>nil then begin
    ANode:=Self;
    while ANode<>nil do begin
      if ANode=APrior then
        raise Exception.Create('internal error: '
          +'TDefineTemplate.InsertBehind: APrior=ANode');
      dec(Parent.FChildCount);
      ANode.FParent:=nil;
      ANode:=ANode.Next;
    end;
  end;
  LastSibling:=Self;
  while LastSibling.Next<>nil do LastSibling:=LastSibling.Next;
  FParent:=NewParent;
  if Parent<>nil then begin
    ANode:=Self;
    while (ANode<>nil) do begin
      ANode.FParent:=Parent;
      inc(Parent.FChildCount);
      ANode:=ANode.Next;
    end;
    if Parent.LastChild=APrior then Parent.FLastChild:=LastSibling;
  end;
  FPrior:=APrior;
  LastSibling.FNext:=APrior.Next;
  APrior.FNext:=Self;
  if LastSibling.Next<>nil then LastSibling.Next.FPrior:=LastSibling;
end;

procedure TDefineTemplate.InsertInFront(ANext: TDefineTemplate);
// insert this and all next siblings in front of ANext
var ANode, LastSibling: TDefineTemplate;
begin
  if ANext=nil then exit;
  if FParent<>nil then begin
    ANode:=Self;
    while ANode<>nil do begin
      if ANode=ANext then
        raise Exception.Create('internal error: '
          +'TDefineTemplate.InsertInFront: ANext=ANode');
      dec(FParent.FChildCount);
      ANode.FParent:=nil;
      ANode:=ANode.Next;
    end;
  end;
  LastSibling:=Self;
  while LastSibling.Next<>nil do LastSibling:=LastSibling.Next;
  FParent:=ANext.Parent;
  if Parent<>nil then begin
    ANode:=Self;
    while ANode<>nil do begin
      ANode.FParent:=Parent;
      inc(Parent.FChildCount);
      ANode:=ANode.Next;
    end;
    if Parent.FirstChild=ANext then Parent.FFirstChild:=Self;
  end;
  FPrior:=ANext.Prior;
  if Prior<>nil then Prior.FNext:=Self;
  LastSibling.FNext:=ANext;
  ANext.FPrior:=LastSibling;
end;

procedure TDefineTemplate.Assign(ADefineTemplate: TDefineTemplate;
  WithSubNodes, WithNextSiblings, ClearOldSiblings: boolean);
var ChildTemplate, CopyTemplate, NextTemplate: TDefineTemplate;
begin
  Clear(ClearOldSiblings);
  if ADefineTemplate=nil then exit;
  AssignValues(ADefineTemplate);
  if WithSubNodes then begin
    ChildTemplate:=ADefineTemplate.FirstChild;
    if ChildTemplate<>nil then begin
      CopyTemplate:=TDefineTemplate.Create;
      AddChild(CopyTemplate);
      CopyTemplate.Assign(ChildTemplate,true,true,false);
    end;
  end;
  if WithNextSiblings then begin
    NextTemplate:=ADefineTemplate.Next;
    if NextTemplate<>nil then begin
      CopyTemplate:=TDefineTemplate.Create;
      CopyTemplate.InsertBehind(Self);
      CopyTemplate.Assign(NextTemplate,WithSubNodes,true,false);
    end;
  end;
end;

procedure TDefineTemplate.AssignValues(ADefineTemplate: TDefineTemplate);
begin
  Name:=ADefineTemplate.Name;
  Description:=ADefineTemplate.Description;
  Variable:=ADefineTemplate.Variable;
  Value:=ADefineTemplate.Value;
  Action:=ADefineTemplate.Action;
  Flags:=ADefineTemplate.Flags;
  MergeNameInFront:=ADefineTemplate.MergeNameInFront;
  MergeNameBehind:=ADefineTemplate.MergeNameBehind;
  Owner:=ADefineTemplate.Owner;
end;

procedure TDefineTemplate.Unbind;
begin
  if FPrior<>nil then FPrior.FNext:=FNext;
  if FNext<>nil then FNext.FPrior:=FPrior;
  if FParent<>nil then begin
    if FParent.FFirstChild=Self then FParent.FFirstChild:=FNext;
    if FParent.FLastChild=Self then FParent.FLastChild:=FPrior;
    dec(FParent.FChildCount);
  end;
  FNext:=nil;
  FPrior:=nil;
  FParent:=nil;
end;

procedure TDefineTemplate.Clear(WithSiblings: boolean);
begin
  while FFirstChild<>nil do FFirstChild.Free;
  if WithSiblings then
    while FNext<>nil do FNext.Free;
  Name:='';
  Description:='';
  Value:='';
  Variable:='';
  Flags:=[];
end;

constructor TDefineTemplate.Create;
begin
  inherited Create;
end;

constructor TDefineTemplate.Create(const AName, ADescription, AVariable,
  AValue: string; AnAction: TDefineAction);
begin
  inherited Create;
  Name:=AName;
  Description:=ADescription;
  Variable:=AVariable;
  Value:=AValue;
  Action:=AnAction;
end;

function TDefineTemplate.CreateCopy(OnlyMarked: boolean;
  WithSiblings: boolean; WithChilds: boolean): TDefineTemplate;
var LastNewNode, NewNode, ANode: TDefineTemplate;
begin
  Result:=nil;
  LastNewNode:=nil;
  ANode:=Self;
  while ANode<>nil do begin
    if (not OnlyMarked) or (ANode.FMarked) then begin
      // copy node
      NewNode:=TDefineTemplate.Create;
      NewNode.Assign(ANode,false,false,false);
      if LastNewNode<>nil then
        NewNode.InsertBehind(LastNewNode)
      else
        Result:=NewNode;
      LastNewNode:=NewNode;
      // copy childs
      if WithChilds and (ANode.FirstChild<>nil) then begin
        NewNode:=ANode.FirstChild.CreateCopy(OnlyMarked,true,true);
        if NewNode<>nil then
          LastNewNode.AddChild(NewNode);
      end;
    end;
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

function TDefineTemplate.CreateMergeCopy: TDefineTemplate;
begin
  CreateMergeInfo(false,false);
  Result:=TDefineTemplate.Create;
  Result.Assign(Self,true,false,false);
end;

function TDefineTemplate.FindRoot: TDefineTemplate;
begin
  Result:=Self;
  repeat
    if Result.Parent<>nil then
      Result:=Result.Parent
    else if Result.Prior<>nil then
      Result:=Result.Prior
    else
      break;
  until false;
end;

procedure RaiseCatchableException(const Msg: string);
begin
  { Raises an exception.
    gdb does not catch fpc Exception objects, therefore this procedure raises
    a standard AV which is catched by gdb. }
  DebugLn('ERROR in CodeTools: ',Msg);
  // creates an exception, that gdb catches:
  DebugLn('Creating gdb catchable error:');
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

destructor TDefineTemplate.Destroy;
begin
  Clear(false);
  Unbind;
  inherited Destroy;
end;

function TDefineTemplate.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; ClearOldSiblings, WithMergeInfo: boolean): boolean;
var IndexedPath: string;
  i, LvlCount: integer;
  DefTempl, LastDefTempl: TDefineTemplate;
  NewChild: TDefineTemplate;
begin
  Clear(ClearOldSiblings);
  LvlCount:=XMLConfig.GetValue(Path+'Count/Value',0);
  DefTempl:=nil;
  for i:=1 to LvlCount do begin
    if i=1 then begin
      DefTempl:=Self;
      LastDefTempl:=Prior;
    end else begin
      LastDefTempl:=DefTempl;
      DefTempl:=TDefineTemplate.Create;
      DefTempl.InsertBehind(LastDefTempl);
    end;
    IndexedPath:=Path+'Node'+IntToStr(i)+'/';
    DefTempl.LoadValuesFromXMLConfig(XMLConfig,IndexedPath,WithMergeInfo);
    // load childs
    if XMLConfig.GetValue(IndexedPath+'Count/Value',0)>0 then begin
      NewChild:=TDefineTemplate.Create;
      DefTempl.AddChild(NewChild);
      if not NewChild.LoadFromXMLConfig(XMLConfig,IndexedPath,
                                        false,WithMergeInfo) then
      begin
        Result:=false;  exit;
      end;
    end;
  end;
  Result:=true;
end;

procedure TDefineTemplate.LoadValuesFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; WithMergeInfo: boolean);
var f: TDefineTemplateFlag;
begin
  Name:=XMLConfig.GetValue(Path+'Name/Value','no name');
  Description:=XMLConfig.GetValue(Path+'Description/Value','');
  Value:=XMLConfig.GetValue(Path+'Value/Value','');
  Variable:=XMLConfig.GetValue(Path+'Variable/Value','');
  Action:=DefineActionNameToAction(
                         XMLConfig.GetValue(Path+'Action/Value',''));
  Flags:=[];
  for f:=Low(TDefineTemplateFlag) to High(TDefineTemplateFlag) do begin
    if (f<>dtfAutoGenerated)
    and (XMLConfig.GetValue(Path+'Flags/'+DefineTemplateFlagNames[f],false))
    then
      Include(Flags,f);
  end;
  if WithMergeInfo then begin
    MergeNameInFront:=XMLConfig.GetValue(Path+'MergeNameInFront/Value','');
    MergeNameBehind:=XMLConfig.GetValue(Path+'MergeNameInFront/Value','');
  end else begin
    MergeNameInFront:='';
    MergeNameBehind:='';
  end;
end;

procedure TDefineTemplate.SaveValuesToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; WithMergeInfo: boolean);
var
  f: TDefineTemplateFlag;
begin
  XMLConfig.SetDeleteValue(Path+'Name/Value',Name,'');
  XMLConfig.SetDeleteValue(Path+'Description/Value',Description,'');
  XMLConfig.SetDeleteValue(Path+'Value/Value',Value,'');
  XMLConfig.SetDeleteValue(Path+'Variable/Value',Variable,'');
  XMLConfig.SetDeleteValue(Path+'Action/Value',
                           DefineActionNames[Action],
                           DefineActionNames[da_None]);
  for f:=Low(TDefineTemplateFlag) to High(TDefineTemplateFlag) do begin
    if (f<>dtfAutoGenerated) then
      XMLConfig.SetDeleteValue(
         Path+'Flags/'+DefineTemplateFlagNames[f]
         ,f in Flags,false);
  end;
  if WithMergeInfo then begin
    XMLConfig.SetDeleteValue(Path+'MergeNameInFront/Value',
                             MergeNameInFront,'');
    XMLConfig.SetDeleteValue(Path+'MergeNameBehind/Value',
                             MergeNameBehind,'');
  end else begin
    XMLConfig.SetDeleteValue(Path+'MergeNameInFront/Value','','');
    XMLConfig.SetDeleteValue(Path+'MergeNameBehind/Value','','');
  end;
end;

procedure TDefineTemplate.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string;
  WithSiblings, OnlyMarked, WithMergeInfo, UpdateMergeInfo: boolean);
var IndexedPath: string;
  Index, LvlCount: integer;
  DefTempl: TDefineTemplate;
begin
  if UpdateMergeInfo then CreateMergeInfo(WithSiblings,OnlyMarked);
  DefTempl:=Self;
  LvlCount:=0;
  while DefTempl<>nil do begin
    inc(LvlCount);
    DefTempl:=DefTempl.Next;
  end;
  DefTempl:=Self;
  Index:=0;
  repeat
    if (DefTempl.FMarked) or (not OnlyMarked) then begin
      // save node
      inc(Index);
      IndexedPath:=Path+'Node'+IntToStr(Index)+'/';
      DefTempl.SaveValuesToXMLConfig(XMLConfig,IndexedPath,WithMergeInfo);
      // save childs
      if DefTempl.FFirstChild<>nil then
        DefTempl.FirstChild.SaveToXMLConfig(XMLConfig,IndexedPath,
                                   true,OnlyMarked,
                                   WithMergeInfo,false)
      else
        XMLConfig.SetDeleteValue(IndexedPath+'Count/Value',0,0);
    end;
    if not WithSiblings then break;
    DefTempl:=DefTempl.Next;
  until DefTempl=nil;
  XMLConfig.SetDeleteValue(Path+'Count/Value',Index,0);
end;

procedure TDefineTemplate.CreateMergeInfo(WithSiblings, OnlyMarked: boolean);
var
  DefTempl: TDefineTemplate;
begin
  DefTempl:=Self;
  repeat
    if (DefTempl.FMarked) or (not OnlyMarked) then begin
      if DefTempl.Prior<>nil then
        DefTempl.MergeNameInFront:=DefTempl.Prior.Name
      else
        DefTempl.MergeNameInFront:='';
      if DefTempl.Next<>nil then
        DefTempl.MergeNameBehind:=DefTempl.Next.Name
      else
        DefTempl.MergeNameBehind:='';
      // update childs
      if DefTempl.FFirstChild<>nil then
        DefTempl.FirstChild.CreateMergeInfo(true,OnlyMarked);
    end;
    if not WithSiblings then break;
    DefTempl:=DefTempl.Next;
  until DefTempl=nil;
end;

class procedure TDefineTemplate.MergeXMLConfig(ParentDefTempl: TDefineTemplate;
  var FirstSibling, LastSibling: TDefineTemplate;
  XMLConfig: TXMLConfig; const Path, NewNamePrefix: string);
var
  SrcNode: TDefineTemplate;
begin
  SrcNode:=TDefineTemplate.Create;
  SrcNode.LoadFromXMLConfig(XMLConfig,Path,false,true);
  MergeTemplates(ParentDefTempl,FirstSibling,LastSibling,SrcNode,true,
                 NewNamePrefix);
  SrcNode.Clear(true);
  SrcNode.Free;
end;

class procedure TDefineTemplate.MergeTemplates(ParentDefTempl: TDefineTemplate;
  var FirstSibling, LastSibling: TDefineTemplate;
  SourceTemplate: TDefineTemplate; WithSiblings: boolean;
  const NewNamePrefix: string);
// merge SourceTemplate. This will keep SourceTemplate untouched
var
  NewNode, PosNode: TDefineTemplate;
  Inserted: boolean;
  SrcNode: TDefineTemplate;
begin
  SrcNode:=SourceTemplate;
  while SrcNode<>nil do begin
    // merge all source nodes
    NewNode:=SrcNode.CreateCopy(false,false,false);
    Inserted:=false;
    if NewNode.Name<>'' then begin
      // node has a name -> test if already exists
      PosNode:=FirstSibling;
      while (PosNode<>nil)
      and (CompareText(PosNode.Name,NewNode.Name)<>0) do
        PosNode:=PosNode.Next;
      if PosNode<>nil then begin
        // node with same name already exists -> check if it is a copy
        if NewNode.IsEqual(PosNode,false,false) then begin
          // node already exists
          NewNode.Free;
          NewNode:=PosNode;
        end else begin
          // node has same name, but different values
          // -> rename node
          NewNode.Name:=NewNode.FindUniqueName(NewNamePrefix+NewNode.Name);
          // insert behind PosNode
          NewNode.InsertBehind(PosNode);
        end;
        Inserted:=true;
      end;
    end;
    if not Inserted then begin
      // node name is unique or empty -> insert node
      if NewNode.MergeNameInFront<>'' then begin
        // last time, node was inserted behind MergeNameInFront
        // -> search MergeNameInFront
        PosNode:=LastSibling;
        while (PosNode<>nil)
        and (CompareText(PosNode.Name,NewNode.MergeNameInFront)<>0) do
          PosNode:=PosNode.Prior;
        if PosNode<>nil then begin
          // MergeNameInFront found -> insert behind
          NewNode.InsertBehind(PosNode);
          Inserted:=true;
        end;
      end;
      if not Inserted then begin
        if NewNode.MergeNameBehind<>'' then begin
          // last time, node was inserted in front of MergeNameBehind
          // -> search MergeNameBehind
          PosNode:=FirstSibling;
          while (PosNode<>nil)
          and (CompareText(PosNode.Name,NewNode.MergeNameBehind)<>0) do
            PosNode:=PosNode.Next;
          if PosNode<>nil then begin
            // MergeNameBehind found -> insert in front
            NewNode.InsertInFront(PosNode);
            Inserted:=true;
          end;
        end;
      end;
      if not Inserted then begin
        // no merge position found -> add as last
        if LastSibling<>nil then begin
          NewNode.InsertBehind(LastSibling);
        end else if ParentDefTempl<>nil then begin
          ParentDefTempl.AddChild(NewNode);
        end;
      end;
    end;
    // NewNode is now inserted -> update FirstSibling and LastSibling
    if FirstSibling=nil then begin
      FirstSibling:=NewNode;
      LastSibling:=NewNode;
    end;
    while FirstSibling.Prior<>nil do
      FirstSibling:=FirstSibling.Prior;
    while LastSibling.Next<>nil do
      LastSibling:=LastSibling.Next;
    // merge childs
    MergeTemplates(NewNode,NewNode.FFirstChild,NewNode.FLastChild,
                   SrcNode.FirstChild,true,NewNamePrefix);
    if not WithSiblings then break;
    SrcNode:=SrcNode.Next;
  end;
end;

function TDefineTemplate.ConsistencyCheck: integer;
var RealChildCount: integer;
  DefTempl: TDefineTemplate;
begin
  RealChildCount:=0;
  DefTempl:=FFirstChild;
  if DefTempl<>nil then begin
    if DefTempl.Prior<>nil then begin
      // not first child
      Result:=-2;  exit;
    end;
    while DefTempl<>nil do begin
      if DefTempl.Parent<>Self then begin
        DebugLn('  C: DefTempl.Parent<>Self: ',Name,',',DefTempl.Name);
        Result:=-3;  exit;
      end;
      if (DefTempl.Next<>nil) and (DefTempl.Next.Prior<>DefTempl) then begin
        Result:=-4;  exit;
      end;
      if (DefTempl.Prior<>nil) and (DefTempl.Prior.Next<>DefTempl) then begin
        Result:=-5;  exit;
      end;
      Result:=DefTempl.ConsistencyCheck;
      if Result<>0 then begin
        dec(Result,100);  exit;
      end;
      DefTempl:=DefTempl.Next;
      inc(RealChildCount);
    end;
  end;
  if (Parent<>nil) then begin
    if (Prior=nil) and (Parent.FirstChild<>Self) then begin
      Result:=-6;  exit;
    end;
    if (Next=nil) and (Parent.LastChild<>Self) then begin
      Result:=-7;  exit;
    end;
  end;
  if RealChildCount<>FChildCount then begin
    Result:=-1;  exit;
  end;
  Result:=0;
end;

procedure TDefineTemplate.SetDefineOwner(NewOwner: TObject;
  WithSiblings: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.Owner:=NewOwner;
    if ANode.FFirstChild<>nil then
      ANode.FFirstChild.SetDefineOwner(NewOwner,true);
    if not WithSiblings then exit;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.SetFlags(AddFlags, SubFlags: TDefineTemplateFlags;
  WithSiblings: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.Flags:=ANode.Flags+AddFlags-SubFlags;
    if ANode.FFirstChild<>nil then
      ANode.FFirstChild.SetFlags(AddFlags,SubFlags,true);
    if not WithSiblings then exit;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.WriteDebugReport(OnlyMarked: boolean);

  procedure WriteNode(ANode: TDefineTemplate; const Prefix: string);
  var ActionStr: string;
  begin
    if ANode=nil then exit;
    if (not OnlyMarked) or (ANode.Marked) then begin
      ActionStr:=DefineActionNames[ANode.Action];
      DebugLn(Prefix+'Self='+DbgS(ANode),
        ' Name="'+ANode.Name,'"',
        ' Consistency='+dbgs(ANode.ConsistencyCheck),
        ' Next='+DbgS(ANode.Next),
        ' Prior='+DbgS(ANode.Prior),
        ' Action='+ActionStr,
        ' Flags=['+DefineTemplateFlagsToString(ANode.Flags),']',
        ' Marked='+dbgs(ANode.Marked)
        );
      DebugLn(Prefix+'   + Description="',ANode.Description,'"');
      DebugLn(Prefix+'   + Variable="',ANode.Variable,'"');
      DebugLn(Prefix+'   + Value="',ANode.Value,'"');
    end;
    WriteNode(ANode.FirstChild,Prefix+'  ');
    WriteNode(ANode.Next,Prefix);
  end;

begin
  WriteNode(Self,'  ');
end;

function TDefineTemplate.HasDefines(OnlyMarked, WithSiblings: boolean): boolean;
var
  CurTempl: TDefineTemplate;
begin
  Result:=true;
  CurTempl:=Self;
  while CurTempl<>nil do begin
    if ((not OnlyMarked) or (CurTempl.FMarked))
    and (CurTempl.Action in DefineActionDefines) then exit;
    // go to next
    if CurTempl.FFirstChild<>nil then
      CurTempl:=CurTempl.FFirstChild
    else if (CurTempl.FNext<>nil)
    and (WithSiblings or (CurTempl.Parent<>Parent)) then
      CurTempl:=CurTempl.FNext
    else begin
      // search uncle
      repeat
        CurTempl:=CurTempl.Parent;
        if (CurTempl=Parent)
        or ((CurTempl.Parent=Parent) and not WithSiblings) then begin
          Result:=false;
          exit;
        end;
      until (CurTempl.FNext<>nil);
      CurTempl:=CurTempl.FNext;
    end;
  end;
  Result:=false;
end;

function TDefineTemplate.IsEqual(ADefineTemplate: TDefineTemplate;
  CheckSubNodes, CheckNextSiblings: boolean): boolean;
var SrcNode, DestNode: TDefineTemplate;
begin
  Result:=(ADefineTemplate<>nil)
      and (Name=ADefineTemplate.Name)
      and (Description=ADefineTemplate.Description)
      and (Variable=ADefineTemplate.Variable)
      and (Value=ADefineTemplate.Value)
      and (Action=ADefineTemplate.Action)
      and (Flags=ADefineTemplate.Flags)
      and (Owner=ADefineTemplate.Owner);
  if not Result then begin
    exit;
  end;
  if CheckSubNodes then begin
    if (ChildCount<>ADefineTemplate.ChildCount) then begin
      Result:=false;
      exit;
    end;
    SrcNode:=FirstChild;
    DestNode:=ADefineTemplate.FirstChild;
    if SrcNode<>nil then begin
      Result:=SrcNode.IsEqual(DestNode,CheckSubNodes,true);
      if not Result then exit;
    end;
  end;
  if CheckNextSiblings then begin
    SrcNode:=Next;
    DestNode:=ADefineTemplate.Next;
    while (SrcNode<>nil) and (DestNode<>nil) do begin
      Result:=SrcNode.IsEqual(DestNode,CheckSubNodes,false);
      if not Result then exit;
      SrcNode:=SrcNode.Next;
      DestNode:=DestNode.Next;
    end;
    Result:=(SrcNode=nil) and (DestNode=nil);
    if not Result then begin
      DebugLn('TDefineTemplate.IsEqual DIFF 3 ',Name,' ',
        ADefineTemplate.Name,' ',dbgs(ChildCount),' ',dbgs(ADefineTemplate.ChildCount));
    end;
  end;
end;

function TDefineTemplate.IsAutoGenerated: boolean;
begin
  Result:=SelfOrParentContainsFlag(dtfAutoGenerated);
end;

procedure TDefineTemplate.RemoveFlags(TheFlags: TDefineTemplateFlags);
var ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    Flags:=Flags-TheFlags;
    if FirstChild<>nil then FirstChild.RemoveFlags(TheFlags);
    ANode:=ANode.Next;
  end;
end;

function TDefineTemplate.Level: integer;
var ANode: TDefineTemplate;
begin
  Result:=-1;
  ANode:=Self;
  while ANode<>nil do begin
    inc(Result);
    ANode:=ANode.Parent;
  end;
end;

function TDefineTemplate.GetFirstSibling: TDefineTemplate;
begin
  Result:=Self;
  while Result.Prior<>nil do Result:=Result.Prior;
end;

function TDefineTemplate.SelfOrParentContainsFlag(
  AFlag: TDefineTemplateFlag): boolean;
var Node: TDefineTemplate;
begin
  Node:=Self;
  while (Node<>nil) do begin
    if AFlag in Node.Flags then begin
      Result:=true;
      exit;
    end;
    Node:=Node.Parent;
  end;
  Result:=false;
end;

function TDefineTemplate.FindChildByName(const AName: string): TDefineTemplate;
begin
  if FirstChild<>nil then begin
    Result:=FirstChild.FindByName(AName,false,true)
  end else
    Result:=nil;
end;

function TDefineTemplate.FindByName(const AName: string; WithSubChilds,
  WithNextSiblings: boolean): TDefineTemplate;
var ANode: TDefineTemplate;
begin
  if CompareText(AName,Name)=0 then begin
    Result:=Self;
  end else begin
    if WithSubChilds and (FirstChild<>nil) then
      Result:=FirstChild.FindByName(AName,true,true)
    else
      Result:=nil;
    if (Result=nil) and WithNextSiblings then begin
      ANode:=Next;
      while (ANode<>nil) do begin
        Result:=ANode.FindByName(AName,WithSubChilds,false);
        if Result<>nil then break;
        ANode:=ANode.Next;
      end;
    end;
  end;
end;

function TDefineTemplate.FindUniqueName(const Prefix: string): string;
var Root: TDefineTemplate;
  i: integer;
begin
  Root:=FindRoot;
  i:=0;
  repeat
    inc(i);
    Result:=Prefix+IntToStr(i);
  until Root.FindByName(Result,true,true)=nil;
end;


{ TDirectoryDefines }

constructor TDirectoryDefines.Create;
begin
  inherited Create;
  Values:=TExpressionEvaluator.Create;
  Path:='';
end;

destructor TDirectoryDefines.Destroy;
begin
  Values.Free;
  inherited Destroy;
end;


{ TDefineTree }

procedure TDefineTree.Clear;
begin
  if FFirstDefineTemplate<>nil then begin
    FFirstDefineTemplate.Clear(true);
    FFirstDefineTemplate.Free;
    FFirstDefineTemplate:=nil;
  end;
  ClearCache;
end;

function TDefineTree.IsEqual(SrcDefineTree: TDefineTree): boolean;
begin
  Result:=false;
  if SrcDefineTree=nil then exit;
  if (FFirstDefineTemplate=nil) xor (SrcDefineTree.FFirstDefineTemplate=nil)
  then exit;
  if (FFirstDefineTemplate<>nil)
  and (not FFirstDefineTemplate.IsEqual(
                                  SrcDefineTree.FFirstDefineTemplate,true,true))
  then exit;
  Result:=true;
end;

procedure TDefineTree.Assign(SrcDefineTree: TDefineTree);
begin
  if IsEqual(SrcDefineTree) then exit;
  Clear;
  if SrcDefineTree.FFirstDefineTemplate<>nil then begin
    FFirstDefineTemplate:=TDefineTemplate.Create;
    FFirstDefineTemplate.Assign(SrcDefineTree.FFirstDefineTemplate,
                                true,true,true);
  end;
end;

procedure TDefineTree.AssignNonAutoCreated(SrcDefineTree: TDefineTree);
var
  SrcNonAutoCreated: TDefineTemplate;
begin
  MarkNonAutoCreated;
  RemoveMarked;
  SrcNonAutoCreated:=SrcDefineTree.ExtractNonAutoCreated;
  if SrcNonAutoCreated=nil then exit;
  //DebugLn('TDefineTree.AssignNonAutoCreated A Front=',SrcNonAutoCreated.MergeNameInFront,' Behind=',SrcNonAutoCreated.MergeNameBehind);
  MergeTemplates(SrcNonAutoCreated,'');
  SrcNonAutoCreated.Clear(true);
  SrcNonAutoCreated.Free;
  FFirstDefineTemplate.CreateMergeInfo(true,false);
  //DebugLn('TDefineTree.AssignNonAutoCreated B Front=',FFirstDefineTemplate.MergeNameInFront,' Behind=',FFirstDefineTemplate.MergeNameBehind);
end;

procedure TDefineTree.ClearCache;
begin
  if (FCache.Count=0) and (FVirtualDirCache=nil) then exit;
  DoClearCache;
end;

constructor TDefineTree.Create;
begin
  inherited Create;
  FFirstDefineTemplate:=nil;
  FCache:=TAVLTree.Create(@CompareDirectoryDefines);
  
  FMacroFunctions:=TKeyWordFunctionList.Create;
  FMacroFunctions.AddExtended('Ext',nil,@MacroFuncExtractFileExt);
  FMacroFunctions.AddExtended('PATH',nil,@MacroFuncExtractFilePath);
  FMacroFunctions.AddExtended('NAME',nil,@MacroFuncExtractFileName);
  FMacroFunctions.AddExtended('NAMEONLY',nil,@MacroFuncExtractFileNameOnly);
  
  FMacroVariables:=TKeyWordFunctionList.Create;
end;

destructor TDefineTree.Destroy;
begin
  Clear;
  FMacroVariables.Free;
  FMacroFunctions.Free;
  FCache.Free;
  inherited Destroy;
end;

function TDefineTree.GetLastRootTemplate: TDefineTemplate;
begin
  Result:=FFirstDefineTemplate;
  if Result=nil then exit;
  while Result.Next<>nil do Result:=Result.Next;
end;

function TDefineTree.FindDirectoryInCache(
  const Path: string): TDirectoryDefines;
var cmp: integer;
  ANode: TAVLTreeNode;
begin
  ANode:=FCache.Root;
  while (ANode<>nil) do begin
    cmp:=CompareFilenames(Path,TDirectoryDefines(ANode.Data).Path);
    if cmp<0 then
      ANode:=ANode.Left
    else if cmp>0 then
      ANode:=ANode.Right
    else
      break;
  end;
  if ANode<>nil then
    Result:=TDirectoryDefines(ANode.Data)
  else
    Result:=nil;
end;

function TDefineTree.GetDirDefinesForDirectory(const Path: string;
  WithVirtualDir: boolean): TDirectoryDefines;
var
  ExpPath: String;
begin
  //DebugLn('[TDefineTree.GetDirDefinesForDirectory] "',Path,'"');
  if (Path<>'') or (not WithVirtualDir) then begin
    DoPrepareTree;
    ExpPath:=TrimFilename(Path);
    if (ExpPath<>'') and (ExpPath[length(ExpPath)]<>PathDelim) then
      ExpPath:=ExpPath+PathDelim;
    Result:=FindDirectoryInCache(ExpPath);
    if Result=nil then begin
      Result:=TDirectoryDefines.Create;
      Result.Path:=ExpPath;
      //DebugLn('[TDefineTree.GetDirDefinesForDirectory] B ',ExpPath,' ');
      if Calculate(Result) then begin
        //DebugLn('[TDefineTree.GetDirDefinesForDirectory] C success');
        FCache.Add(Result);
      end else begin
        //DebugLn('[TDefineTree.GetDirDefinesForDirectory] D failed');
        Result.Free;
        Result:=nil;
      end;
    end;
  end else begin
    Result:=GetDirDefinesForVirtualDirectory;
  end;
end;

function TDefineTree.GetDirDefinesForVirtualDirectory: TDirectoryDefines;
begin
  DoPrepareTree;
  if FVirtualDirCache=nil then begin
    //DebugLn('################ TDefineTree.GetDirDefinesForVirtualDirectory');
    FVirtualDirCache:=TDirectoryDefines.Create;
    FVirtualDirCache.Path:=VirtualDirectory;
    if Calculate(FVirtualDirCache) then begin
      //DebugLn('TDefineTree.GetDirDefinesForVirtualDirectory ');
    end else begin
      FVirtualDirCache.Free;
      FVirtualDirCache:=nil;
    end;
  end;
  Result:=FVirtualDirCache;
end;

function TDefineTree.MacroFuncExtractFileExt(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFileExt(FuncData^.Param);
  Result:=true;
end;

function TDefineTree.MacroFuncExtractFilePath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFilePath(FuncData^.Param);
  Result:=true;
end;

function TDefineTree.MacroFuncExtractFileName(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFileName(FuncData^.Param);
  Result:=true;
end;

function TDefineTree.MacroFuncExtractFileNameOnly(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFileNameOnly(FuncData^.Param);
  Result:=true;
end;

procedure TDefineTree.DoClearCache;
begin
  {$IFDEF VerboseDefineCache}
  DebugLn('TDefineTree.DoClearCache A +++++++++');
  {$ENDIF}
  if FCache<>nil then FCache.FreeAndClear;
  if FVirtualDirCache<>nil then begin
    FVirtualDirCache.Free;
    FVirtualDirCache:=nil;
  end;
  IncreaseChangeStep;
end;

procedure TDefineTree.DoPrepareTree;
begin
  if Assigned(OnPrepareTree) then OnPrepareTree(Self);
end;

procedure TDefineTree.RemoveMarked;
begin
  if FFirstDefineTemplate=nil then exit;
  FFirstDefineTemplate.RemoveMarked(true,FFirstDefineTemplate);
  ClearCache;
end;

procedure TDefineTree.MarkNonAutoCreated;
begin
  if FFirstDefineTemplate=nil then exit;
  with FFirstDefineTemplate do begin
    // clear marks
    UnmarkNodes(true,true);
    // mark each non autocreated node
    MarkFlags([],[dtfAutoGenerated],true,true);
    // mark every parent with a marked child
    InheritMarks(true,true,false,true);
  end;
end;

function TDefineTree.GetUnitPathForDirectory(const Directory: string): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[UnitPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetIncludePathForDirectory(const Directory: string
  ): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[IncludePathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetSrcPathForDirectory(const Directory: string): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[SrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetPPUSrcPathForDirectory(const Directory: string
  ): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[PPUSrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetPPWSrcPathForDirectory(const Directory: string
  ): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[PPWSrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetDCUSrcPathForDirectory(const Directory: string
  ): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[DCUSrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetCompiledSrcPathForDirectory(const Directory: string
  ): string;
var
  Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[CompiledSrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetDefinesForDirectory(
  const Path: string; WithVirtualDir: boolean): TExpressionEvaluator;
var
  DirDef: TDirectoryDefines;
begin
  DirDef:=GetDirDefinesForDirectory(Path,WithVirtualDir);
  if DirDef<>nil then
    Result:=DirDef.Values
  else
    Result:=nil;
end;

function TDefineTree.GetDefinesForVirtualDirectory: TExpressionEvaluator;
var
  DirDef: TDirectoryDefines;
begin
  DirDef:=GetDirDefinesForVirtualDirectory;
  if DirDef<>nil then
    Result:=DirDef.Values
  else
    Result:=nil;
end;

procedure TDefineTree.ReadValue(const DirDef: TDirectoryDefines;
  const PreValue, CurDefinePath: string; out NewValue: string);
var
  Buffer: PChar;
  BufferPos: integer;
  BufferSize: integer;
  ValuePos: integer;

  function SearchBracketClose(const s: string; Position:integer): integer;
  var BracketClose:char;
    sLen: Integer;
  begin
    if s[Position]='(' then
      BracketClose:=')'
    else
      BracketClose:='{';
    inc(Position);
    sLen:=length(s);
    while (Position<=sLen) and (s[Position]<>BracketClose) do begin
      if s[Position]=SpecialChar then
        inc(Position)
      else if (s[Position] in ['(','{']) then
        Position:=SearchBracketClose(s,Position);
      inc(Position);
    end;
    Result:=Position;
  end;

  function ExecuteMacroFunction(const FuncName, Params: string): string;
  var
    FuncData: TReadFunctionData;
  begin
    FuncData.Param:=Params;
    FuncData.Result:='';
    FMacroFunctions.DoDataFunction(PChar(Pointer(FuncName)),length(FuncName),
                                   @FuncData);
    Result:=FuncData.Result;
  end;
  
  function ExecuteMacroVariable(var MacroVariable: string): boolean;
  var
    FuncData: TReadFunctionData;
  begin
    FuncData.Param:=MacroVariable;
    FuncData.Result:='';
    Result:=FMacroVariables.DoDataFunction(
                 PChar(Pointer(MacroVariable)),length(MacroVariable),@FuncData);
    if Result then
      MacroVariable:=FuncData.Result;
  end;

  procedure GrowBuffer(MinSize: integer);
  var
    NewSize: Integer;
  begin
    if MinSize<=BufferSize then exit;
    NewSize:=MinSize*2+100;
    ReAllocMem(Buffer,NewSize);
    BufferSize:=NewSize;
  end;

  procedure CopyStringToBuffer(const Src: string);
  begin
    if Src='' then exit;
    Move(Src[1],Buffer[BufferPos],length(Src));
    inc(BufferPos,length(Src));
  end;

  procedure CopyFromValueToBuffer(Len: integer);
  begin
    if Len=0 then exit;
    Move(NewValue[ValuePos],Buffer[BufferPos],Len);
    inc(BufferPos,Len);
    inc(ValuePos,Len);
  end;

  function Substitute(const CurValue: string; ValueLen: integer;
    MacroStart: integer; var MacroEnd: integer): boolean;
  var
    MacroFuncNameEnd: Integer;
    MacroFuncNameLen: Integer;
    MacroStr: String;
    MacroFuncName: String;
    NewMacroLen: Integer;
    MacroParam: string;
    OldMacroLen: Integer;
    Handled: Boolean;
    MacroVarName: String;
  begin
    Result:=false;
    MacroFuncNameEnd:=MacroEnd;
    MacroFuncNameLen:=MacroFuncNameEnd-MacroStart-1;
    MacroEnd:=SearchBracketClose(CurValue,MacroFuncNameEnd)+1;
    if MacroEnd>ValueLen+1 then exit;
    OldMacroLen:=MacroEnd-MacroStart;
    // Macro found
    if MacroFuncNameLen>0 then begin
      MacroFuncName:=copy(CurValue,MacroStart+1,MacroFuncNameLen);
      // Macro function -> substitute macro parameter first
      ReadValue(DirDef,copy(CurValue,MacroFuncNameEnd+1
          ,MacroEnd-MacroFuncNameEnd-2),CurDefinePath,MacroParam);
      // execute the macro function
      //debugln('Substitute MacroFuncName="',MacroFuncName,'" MacroParam="',MacroParam,'"');
      MacroStr:=ExecuteMacroFunction(MacroFuncName,MacroParam);
    end else begin
      // Macro variable
      MacroVarName:=copy(CurValue,MacroStart+2,MacroEnd-MacroStart-3);
      MacroStr:=MacroVarName;
      //DebugLn('**** MacroVarName=',MacroVarName,' ',DirDef.Values.Variables[MacroVarName]);
      //DebugLn('DirDef.Values=',DirDef.Values.AsString);
      if MacroVarName=DefinePathMacroName then begin
        MacroStr:=CurDefinePath;
      end else if DirDef.Values.IsDefined(MacroVarName) then begin
        MacroStr:=DirDef.Values.Variables[MacroVarName];
      end else begin
        Handled:=false;
        if Assigned(FOnReadValue) then begin
          MacroParam:=MacroVarName;
          MacroStr:='';
          FOnReadValue(Self,MacroParam,MacroStr,Handled);
        end;
        if not Handled then begin
          MacroStr:=MacroVarName;
          Handled:=ExecuteMacroVariable(MacroStr);
        end;
        if not Handled then begin
          MacroStr:='';
        end;
      end;
    end;
    NewMacroLen:=length(MacroStr);
    GrowBuffer(BufferPos+NewMacroLen-OldMacroLen+ValueLen-ValuePos+1);
    // copy text between this macro and last macro
    CopyFromValueToBuffer(MacroStart-ValuePos);
    // copy macro value to buffer
    CopyStringToBuffer(MacroStr);
    ValuePos:=MacroEnd;
    Result:=true;
  end;

  procedure SetNewValue;
  var
    RestLen: Integer;
  begin
    if Buffer=nil then exit;
    // write rest to buffer
    RestLen:=length(NewValue)-ValuePos+1;
    if RestLen>0 then begin
      GrowBuffer(BufferPos+RestLen);
      Move(NewValue[ValuePos],Buffer[BufferPos],RestLen);
      inc(BufferPos,RestLen);
    end;
    // copy the buffer into NewValue
    //DebugLn('    [ReadValue] Old="',copy(NewValue,1,100),'"');
    SetLength(NewValue,BufferPos);
    if BufferPos>0 then
      Move(Buffer^,NewValue[1],BufferPos);
    //DebugLn('    [ReadValue] New="',copy(NewValue,1,100),'"');
    // clean up
    FreeMem(Buffer);
    Buffer:=nil;
  end;

var MacroStart,MacroEnd: integer;
  ValueLen: Integer;
begin
  //  DebugLn('    [ReadValue] A   "',copy(PreValue,1,100),'"');
  NewValue:=PreValue;
  if NewValue='' then exit;
  MacroStart:=1;
  ValueLen:=length(NewValue);
  Buffer:=nil;
  BufferSize:=0;
  BufferPos:=0; // position in buffer
  ValuePos:=1;  // same position in value
  while MacroStart<=ValueLen do begin
    // search for macro
    while (MacroStart<=ValueLen) and (NewValue[MacroStart]<>'$') do begin
      if (NewValue[MacroStart]=SpecialChar) then inc(MacroStart);
      inc(MacroStart);
    end;
    if MacroStart>ValueLen then break;
    // read macro function name
    MacroEnd:=MacroStart+1;
    while (MacroEnd<=ValueLen)
    and (NewValue[MacroEnd] in ['0'..'9','A'..'Z','a'..'z','_']) do
      inc(MacroEnd);
    // read macro name / parameters
    if (MacroEnd<ValueLen) and (NewValue[MacroEnd] in ['(','{']) then
    begin
      if not Substitute(NewValue,ValueLen,MacroStart,MacroEnd) then break;
    end;
    MacroStart:=MacroEnd;
  end;
  if Buffer<>nil then SetNewValue;
end;

procedure TDefineTree.MarkTemplatesOwnedBy(TheOwner: TObject; const MustFlags,
  NotFlags: TDefineTemplateFlags);
begin
  if FFirstDefineTemplate=nil then exit;
  with FFirstDefineTemplate do begin
    // unmark all nodes
    UnmarkNodes(true,true);
    // mark each node in filter
    MarkOwnedBy(TheOwner,MustFlags,NotFlags,true,true);
    // mark every parent, that has a marked child
    InheritMarks(true,true,false,true);
  end;
end;

procedure TDefineTree.RemoveTemplatesOwnedBy(TheOwner: TObject;
  const MustFlags, NotFlags: TDefineTemplateFlags);
begin
  if FFirstDefineTemplate=nil then exit;
  FFirstDefineTemplate.RemoveLeaves(TheOwner,MustFlags,NotFlags,true,
                                    FFirstDefineTemplate);
  FFirstDefineTemplate.RemoveOwner(TheOwner,true);
  ClearCache;
end;

function TDefineTree.ExtractTemplatesOwnedBy(TheOwner: TObject;
  const MustFlags, NotFlags: TDefineTemplateFlags): TDefineTemplate;
begin
  Result:=nil;
  if FFirstDefineTemplate=nil then exit;
  MarkTemplatesOwnedBy(TheOwner,MustFlags,NotFlags);
  with FFirstDefineTemplate do begin
    // store some information, so that merging the nodes will result in old order
    CreateMergeInfo(true,false);
    // extract marked nodes
    Result:=CreateCopy(true,true,true);
  end;
end;

function TDefineTree.ExtractNonAutoCreated: TDefineTemplate;
begin
  Result:=nil;
  if FFirstDefineTemplate=nil then exit;
  MarkNonAutoCreated;
  with FFirstDefineTemplate do begin
    // store some information, so that merging the nodes will result in old order
    CreateMergeInfo(true,false);
    // extract marked nodes
    Result:=CreateCopy(true,true,true);
  end;
end;

procedure TDefineTree.MergeTemplates(SourceTemplate: TDefineTemplate;
  const NewNamePrefix: string);
var
  LastDefTempl: TDefineTemplate;
begin
  LastDefTempl:=GetLastRootTemplate;
  TDefineTemplate.MergeTemplates(nil,FFirstDefineTemplate,LastDefTempl,
                  SourceTemplate,true,NewNamePrefix);
  ClearCache;
end;

function TDefineTree.Calculate(DirDef: TDirectoryDefines): boolean;
// calculates the values for a single directory
// returns false on error
var
  ExpandedDirectory, EvalResult, TempValue: string;

  procedure CalculateTemplate(DefTempl: TDefineTemplate; const CurPath: string);
  
    procedure CalculateIfChilds;
    begin
      // execute childs
      CalculateTemplate(DefTempl.FirstChild,CurPath);
      // jump to end of else templates
      while (DefTempl.Next<>nil)
      and (DefTempl.Next.Action in [da_Else,da_ElseIf])
      do begin
        if Assigned(OnCalculate) then
          OnCalculate(Self,DefTempl,false,'',false,'',false);
        DefTempl:=DefTempl.Next;
      end;
    end;

  // procedure CalculateTemplate(DefTempl: TDefineTemplate; const CurPath: string);
  var SubPath, TempValue: string;
  begin
    while DefTempl<>nil do begin
      //DebugLn('  [CalculateTemplate] CurPath="',CurPath,'" DefTempl.Name="',DefTempl.Name,'"');
      case DefTempl.Action of
      da_Block:
        // calculate children
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          CalculateTemplate(DefTempl.FirstChild,CurPath);
        end;

      da_Define:
        // Define for a single Directory (not SubDirs)
        begin
          if FilenameIsMatching(CurPath,ExpandedDirectory,true) then begin
            ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,true,TempValue,false,'',true);
            DirDef.Values.Variables[DefTempl.Variable]:=TempValue;
          end else begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,false,'',false,'',false);
          end;
        end;

      da_DefineRecurse:
        // Define for current and sub directories
        begin
          ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,true,TempValue,false,'',true);
          DirDef.Values.Variables[DefTempl.Variable]:=TempValue;
        end;

      da_Undefine:
        // Undefine for a single Directory (not SubDirs)
        if FilenameIsMatching(CurPath,ExpandedDirectory,true) then begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          DirDef.Values.Undefine(DefTempl.Variable);
        end else begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',false);
        end;

      da_UndefineRecurse:
        // Undefine for current and sub directories
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          DirDef.Values.Undefine(DefTempl.Variable);
        end;

      da_UndefineAll:
        // Undefine every value for current and sub directories
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          DirDef.Values.Clear;
        end;

      da_If, da_ElseIf:
        begin
          // test expression in value
          ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
          EvalResult:=DirDef.Values.Eval(TempValue);
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,true,TempValue,true,EvalResult,EvalResult='1');
          //debugln('da_If,da_ElseIf: DefTempl.Value="',DbgStr(DefTempl.Value),'" CurPath="',CurPath,'" TempValue="',TempValue,'" EvalResult=',EvalResult);
          if DirDef.Values.ErrorPosition>=0 then begin
            FErrorDescription:=Format(ctsSyntaxErrorInExpr,[TempValue]);
            FErrorTemplate:=DefTempl;
          end else if EvalResult='1' then
            CalculateIfChilds;
        end;
      da_IfDef:
        // test if variable is defined
        begin
          //DebugLn('da_IfDef A Name=',DefTempl.Name,
          //  ' Variable=',DefTempl.Variable,
          //  ' Is=',dbgs(DirDef.Values.IsDefined(DefTempl.Variable)),
          //  ' CurPath="',CurPath,'"',
          //  ' Values.Count=',dbgs(DirDef.Values.Count));
          if DirDef.Values.IsDefined(DefTempl.Variable) then begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,false,'',false,'',true);
            CalculateIfChilds;
          end else begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,false,'',false,'',false);
          end;
        end;

      da_IfNDef:
        // test if variable is not defined
        if not DirDef.Values.IsDefined(DefTempl.Variable) then begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          CalculateIfChilds;
        end else begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',false);
        end;

      da_Else:
        // execute childs
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          CalculateTemplate(DefTempl.FirstChild,CurPath);
        end;

      da_Directory:
        begin
          // template for a sub directory
          ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
          // CurPath can be ''
          SubPath:=AppendPathDelim(CurPath)+TempValue;
          // test if ExpandedDirectory is part of SubPath
          if FilenameIsMatching(SubPath,ExpandedDirectory,false) then begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,true,SubPath,false,'',true);
            CalculateTemplate(DefTempl.FirstChild,SubPath);
          end else begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,true,SubPath,false,'',false);
          end;
        end;
      end;
      if ErrorTemplate<>nil then exit;
      if DefTempl<>nil then
        DefTempl:=DefTempl.Next;
    end;
  end;

// function TDefineTree.Calculate(DirDef: TDirectoryDefines): boolean;
begin
  {$IFDEF VerboseDefineCache}
  DebugLn('[TDefineTree.Calculate] ++++++ "',DirDef.Path,'"');
  {$ENDIF}
  Result:=true;
  FErrorTemplate:=nil;
  ExpandedDirectory:=DirDef.Path;
  if (ExpandedDirectory=VirtualDirectory)
  and Assigned(OnGetVirtualDirectoryAlias) then
    OnGetVirtualDirectoryAlias(Self,ExpandedDirectory);
  if (ExpandedDirectory<>VirtualDirectory) then begin
    ReadValue(DirDef,ExpandedDirectory,'',TempValue);
    ExpandedDirectory:=TempValue;
  end;
  DirDef.Values.Clear;
  // compute the result of all matching DefineTemplates
  CalculateTemplate(FFirstDefineTemplate,'');
  if (ExpandedDirectory=VirtualDirectory)
  and (Assigned(OnGetVirtualDirectoryDefines)) then
    OnGetVirtualDirectoryDefines(Self,DirDef);
  Result:=(ErrorTemplate=nil);
end;

procedure TDefineTree.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
  if DirectoryCachePool<>nil then DirectoryCachePool.IncreaseTimeStamp;
end;

procedure TDefineTree.SetDirectoryCachePool(const AValue: TCTDirectoryCachePool
  );
begin
  if FDirectoryCachePool=AValue then exit;
  FDirectoryCachePool:=AValue;
end;

procedure TDefineTree.Add(ADefineTemplate: TDefineTemplate);
// add as last
var LastDefTempl: TDefineTemplate;
begin
  if ADefineTemplate=nil then exit;
  if RootTemplate=nil then
    RootTemplate:=ADefineTemplate
  else begin
    // add as last
    LastDefTempl:=RootTemplate;
    while LastDefTempl.Next<>nil do
      LastDefTempl:=LastDefTempl.Next;
    ADefineTemplate.InsertBehind(LastDefTempl);
  end;
  ClearCache;
end;

procedure TDefineTree.AddFirst(ADefineTemplate: TDefineTemplate);
// add as first
begin
  if ADefineTemplate=nil then exit;
  if RootTemplate=nil then
    RootTemplate:=ADefineTemplate
  else begin
    RootTemplate.InsertBehind(ADefineTemplate);
    RootTemplate:=ADefineTemplate;
  end;
  ClearCache;
end;

function TDefineTree.FindDefineTemplateByName(
  const AName: string; OnlyRoots: boolean): TDefineTemplate;
begin
  Result:=RootTemplate;
  if RootTemplate<>nil then
    Result:=RootTemplate.FindByName(AName,not OnlyRoots,true)
  else
    Result:=nil;
end;

procedure TDefineTree.ReplaceRootSameName(const Name: string;
  ADefineTemplate: TDefineTemplate);
// if there is a DefineTemplate with the same name then replace it
// else add as last
var OldDefineTemplate: TDefineTemplate;
begin
  if (Name='') then exit;
  OldDefineTemplate:=FindDefineTemplateByName(Name,true);
  if OldDefineTemplate<>nil then begin
    if not OldDefineTemplate.IsEqual(ADefineTemplate,true,false) then begin
      ClearCache;
    end;
    if ADefineTemplate<>nil then
      ADefineTemplate.InsertBehind(OldDefineTemplate);
    if OldDefineTemplate=FFirstDefineTemplate then
      FFirstDefineTemplate:=FFirstDefineTemplate.Next;
    OldDefineTemplate.Unbind;
    OldDefineTemplate.Free;
  end else
    Add(ADefineTemplate);
end;

procedure TDefineTree.RemoveRootDefineTemplateByName(const AName: string);
var ADefTempl: TDefineTemplate;
begin
  ADefTempl:=FindDefineTemplateByName(AName,true);
  if ADefTempl<>nil then RemoveDefineTemplate(ADefTempl);
end;

procedure TDefineTree.RemoveDefineTemplate(ADefTempl: TDefineTemplate);
var
  HadDefines: Boolean;
begin
  if ADefTempl=FFirstDefineTemplate then
    FFirstDefineTemplate:=FFirstDefineTemplate.Next;
  HadDefines:=ADefTempl.HasDefines(false,false);
  ADefTempl.Unbind;
  ADefTempl.Free;
  if HadDefines then ClearCache;
end;

procedure TDefineTree.ReplaceChild(ParentTemplate,
  NewDefineTemplate: TDefineTemplate; const ChildName: string);
// if there is a DefineTemplate with the same name then replace it
// else add as last
var OldDefineTemplate: TDefineTemplate;
begin
  if (ChildName='') or (ParentTemplate=nil) then exit;
  OldDefineTemplate:=ParentTemplate.FindChildByName(ChildName);
  if OldDefineTemplate<>nil then begin
    if not OldDefineTemplate.IsEqual(NewDefineTemplate,true,false) then begin
      ClearCache;
    end;
    if NewDefineTemplate<>nil then
      NewDefineTemplate.InsertBehind(OldDefineTemplate);
    if OldDefineTemplate=FFirstDefineTemplate then
      FFirstDefineTemplate:=FFirstDefineTemplate.Next;
    OldDefineTemplate.Unbind;
    OldDefineTemplate.Free;
  end else begin
    ClearCache;
    ParentTemplate.AddChild(NewDefineTemplate);
  end;
end;

procedure TDefineTree.AddChild(ParentTemplate,
  NewDefineTemplate: TDefineTemplate);
begin
  ClearCache;
  ParentTemplate.AddChild(NewDefineTemplate);
end;

procedure TDefineTree.ReplaceRootSameName(ADefineTemplate: TDefineTemplate);
begin
  if (ADefineTemplate=nil) then exit;
  ReplaceRootSameName(ADefineTemplate.Name,ADefineTemplate);
end;

procedure TDefineTree.ReplaceRootSameNameAddFirst(
  ADefineTemplate: TDefineTemplate);
var OldDefineTemplate: TDefineTemplate;
begin
  if ADefineTemplate=nil then exit;
  OldDefineTemplate:=FindDefineTemplateByName(ADefineTemplate.Name,true);
  if OldDefineTemplate<>nil then begin
    if not OldDefineTemplate.IsEqual(ADefineTemplate,true,false) then begin
      ClearCache;
    end;
    ADefineTemplate.InsertBehind(OldDefineTemplate);
    if OldDefineTemplate=FFirstDefineTemplate then
      FFirstDefineTemplate:=FFirstDefineTemplate.Next;
    OldDefineTemplate.Unbind;
    OldDefineTemplate.Free;
  end else
    AddFirst(ADefineTemplate);
end;

procedure TDefineTree.MergeDefineTemplates(SourceTemplate: TDefineTemplate;
  const NewNamePrefix: string);
var
  LastDefTempl: TDefineTemplate;
begin
  if SourceTemplate=nil then exit;
  // import new defines
  LastDefTempl:=GetLastRootTemplate;
  TDefineTemplate.MergeTemplates(nil,FFirstDefineTemplate,LastDefTempl,
                                 SourceTemplate,true,NewNamePrefix);
  ClearCache;
end;

function TDefineTree.ConsistencyCheck: integer;
begin
  if FFirstDefineTemplate<>nil then begin
    Result:=FFirstDefineTemplate.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,1000);  exit;
    end;
  end;
  Result:=FCache.ConsistencyCheck;
  if Result<>0 then begin
    dec(Result,2000);  exit;
  end;
  Result:=0;
end;

procedure TDefineTree.WriteDebugReport;
begin
  DebugLn('TDefineTree.WriteDebugReport  Consistency=',dbgs(ConsistencyCheck));
  if FFirstDefineTemplate<>nil then
    FFirstDefineTemplate.WriteDebugReport(false)
  else
    DebugLn('  No templates defined');
  DebugLn(FCache.ReportAsString);
  DebugLn('');
end;

    
{ TDefinePool }

constructor TDefinePool.Create;
begin
  inherited Create;
  FItems:=TFPList.Create;
end;

destructor TDefinePool.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TDefinePool.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do begin
    Items[i].Clear(true);
    Items[i].Free;
  end;
  FItems.Clear;
end;

function TDefinePool.GetItems(Index: integer): TDefineTemplate;
begin
  Result:=TDefineTemplate(FItems[Index]);
end;

procedure TDefinePool.SetEnglishErrorMsgFilename(const AValue: string);
begin
  if FEnglishErrorMsgFilename=AValue then exit;
  FEnglishErrorMsgFilename:=AValue;
end;

procedure TDefinePool.Add(ADefineTemplate: TDefineTemplate);
begin
  if ADefineTemplate<>nil then
    FItems.Add(ADefineTemplate);
end;

procedure TDefinePool.Insert(Index: integer; ADefineTemplate: TDefineTemplate);
begin
  FItems.Insert(Index,ADefineTemplate);
end;

procedure TDefinePool.Delete(Index: integer);
begin
  Items[Index].Clear(true);
  Items[Index].Free;
  FItems.Delete(Index);
end;

procedure TDefinePool.Move(SrcIndex, DestIndex: integer);
begin
  FItems.Move(SrcIndex,DestIndex);
end;

function TDefinePool.Count: integer;
begin
  Result:=FItems.Count;
end;

function TDefinePool.CreateFPCTemplate(
  const CompilerPath, CompilerOptions, TestPascalFile: string;
  out UnitSearchPath, TargetOS, TargetProcessor: string;
  Owner: TObject): TDefineTemplate;
// create symbol definitions for the freepascal compiler
// To get reliable values the compiler itself is asked for
var
  LastDefTempl: TDefineTemplate;
  ShortTestFile: string;
  
  procedure AddTemplate(NewDefTempl: TDefineTemplate);
  begin
    if NewDefTempl=nil then exit;
    if LastDefTempl<>nil then
      NewDefTempl.InsertBehind(LastDefTempl);
    LastDefTempl:=NewDefTempl;
  end;
  
  function FindSymbol(const SymbolName: string): TDefineTemplate;
  begin
    Result:=LastDefTempl;
    while (Result<>nil)
    and (Comparetext(Result.Variable,SymbolName)<>0) do
      Result:=Result.Prior;
  end;

  procedure DefineSymbol(const SymbolName, SymbolValue: string;
    const Description: string = '');
  var NewDefTempl: TDefineTemplate;
    Desc: String;
  begin
    NewDefTempl:=FindSymbol(SymbolName);
    if NewDefTempl=nil then begin
      if Description<>'' then
        Desc:=Description
      else
        Desc:=ctsDefaultppc386Symbol;
      NewDefTempl:=TDefineTemplate.Create('Define '+SymbolName,
           Desc,SymbolName,SymbolValue,da_DefineRecurse);
      AddTemplate(NewDefTempl);
    end else begin
      NewDefTempl.Value:=SymbolValue;
    end;
  end;

  procedure UndefineSymbol(const SymbolName: string);
  var
    ADefTempl: TDefineTemplate;
  begin
    ADefTempl:=FindSymbol(SymbolName);
    if ADefTempl=nil then exit;
    if LastDefTempl=ADefTempl then LastDefTempl:=ADefTempl.Prior;
    ADefTempl.Unbind;
    ADefTempl.Free;
  end;

  procedure ProcessOutputLine(var Line: string);
  var
    SymbolName, SymbolValue, UpLine, NewPath: string;
    i: integer;
  begin
    UpLine:=UpperCaseStr(Line);
    i:=length(ShortTestFile);
    if (length(Line)>i)
    and (CompareText(LeftStr(Line,i),ShortTestFile)=0)
    and (Line[i+1]='(') then begin
      inc(i);
      while (i<length(Line)) and (Line[i]<>')') do inc(i);
      inc(i);
      while (i<length(Line)) and (Line[i]=' ') do inc(i);
      if (i<=length(Line)) then begin
        System.Delete(Line,1,i-1);
        System.Delete(UpLine,1,i-1);
      end;
    end;
    if copy(UpLine,1,15)='MACRO DEFINED: ' then begin
      SymbolName:=copy(UpLine,16,length(Line)-15);
      DefineSymbol(SymbolName,'');
    end else if copy(UpLine,1,17)='MACRO UNDEFINED: ' then begin
      SymbolName:=copy(UpLine,18,length(Line)-17);
      UndefineSymbol(SymbolName);
    end else if copy(UpLine,1,6)='MACRO ' then begin
      System.Delete(Line,1,6);
      System.Delete(UpLine,1,6);
      i:=1;
      while (i<=length(Line)) and (Line[i]<>' ') do inc(i);
      SymbolName:=copy(UpLine,1,i-1);
      inc(i); // skip '='
      System.Delete(Line,1,i-1);
      System.Delete(UpLine,1,i-1);
      if copy(UpLine,1,7)='SET TO ' then begin
        SymbolValue:=copy(Line,8,length(Line)-7);
        DefineSymbol(SymbolName,SymbolValue);
      end;
    end else if copy(UpLine,1,17)='USING UNIT PATH: ' then begin
      NewPath:=copy(Line,18,length(Line)-17);
      if not FilenameIsAbsolute(NewPath) then
        NewPath:=ExpandFileName(NewPath);
      {$IFDEF VerboseFPCSrcScan}
      DebugLn('Using unit path: "',NewPath,'"');
      {$ENDIF}
      UnitSearchPath:=UnitSearchPath+NewPath+';';
    end;
  end;
  
var CmdLine: string;
  i, OutLen, LineStart: integer;
  TheProcess: TProcess;
  OutputLine, Buf: String;
  NewDefTempl: TDefineTemplate;
  SrcOS: string;
  SrcOS2: String;
  Step: String;
begin
  Result:=nil;
  //DebugLn('TDefinePool.CreateFPCTemplate PPC386Path="',CompilerPath,'" PPCOptions="',CompilerOptions,'"');
  if TestPascalFile='' then begin
    DebugLn(['WARNING: TDefinePool.CreateFPCTemplate TestPascalFile empty']);
  end;
  UnitSearchPath:='';
  TargetOS:='';
  SrcOS:='';
  TargetProcessor:='';
  if (CompilerPath='') or (not FileIsExecutable(CompilerPath)) then exit;
  LastDefTempl:=nil;
  // find all initial compiler macros and all unit paths
  // -> ask compiler with the -vm -vt switch
  SetLength(Buf,1024);
  Step:='Init';
  try
    CmdLine:=CompilerPath+' -va ';
    if FileExistsCached(EnglishErrorMsgFilename) then
      CmdLine:=CmdLine+'-Fr'+EnglishErrorMsgFilename+' ';
    if CompilerOptions<>'' then
      CmdLine:=CmdLine+CompilerOptions+' ';
    CmdLine:=CmdLine+TestPascalFile;
    //DebugLn('TDefinePool.CreateFPCTemplate CmdLine="',CmdLine,'"');
    ShortTestFile:=ExtractFileName(TestPascalFile);

    TheProcess := TProcess.Create(nil);
    TheProcess.CommandLine := CmdLine;
    TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
    TheProcess.ShowWindow := swoHide;
    Step:='Running '+CmdLine;
    try
      TheProcess.Execute;
      OutputLine:='';
      repeat
        if (TheProcess.Output<>nil) then begin
          OutLen:=TheProcess.Output.Read(Buf[1],length(Buf));
        end else
          OutLen:=0;
        LineStart:=1;
        i:=1;
        while i<=OutLen do begin
          if Buf[i] in [#10,#13] then begin
            OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
            ProcessOutputLine(OutputLine);
            OutputLine:='';
            if (i<OutLen) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
            then
              inc(i);
            LineStart:=i+1;
          end;
          inc(i);
        end;
        OutputLine:=copy(Buf,LineStart,OutLen-LineStart+1);
      until OutLen=0;
      TheProcess.WaitOnExit;
    finally
      //DebugLn('TDefinePool.CreateFPCTemplate Run with -va: OutputLine="',OutputLine,'"');
      TheProcess.Free;
    end;
    DefineSymbol(FPCUnitPathMacroName,UnitSearchPath,'FPC default unit search path');

    //DebugLn('TDefinePool.CreateFPCTemplate First done UnitSearchPath="',UnitSearchPath,'"');

    // ask for target operating system -> ask compiler with switch -iTO
    CmdLine:=CompilerPath;
    if CompilerOptions<>'' then
      CmdLine:=CmdLine+' '+CompilerOptions;
    CmdLine:=CmdLine+' -iTO';

    TheProcess := TProcess.Create(nil);
    TheProcess.CommandLine := CmdLine;
    TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
    TheProcess.ShowWindow := swoHide;
    Step:='Running '+CmdLine;
    try
      TheProcess.Execute;
      if (TheProcess.Output<>nil) then
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf))
      else
        OutLen:=0;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          // define #TargetOS
          TargetOS:=copy(Buf,1,i-1);
          NewDefTempl:=TDefineTemplate.Create('Define TargetOS',
            ctsDefaultppc386TargetOperatingSystem,
            ExternalMacroStart+'TargetOS',TargetOS,da_DefineRecurse);
          AddTemplate(NewDefTempl);
          // define #SrcOS
          SrcOS:=GetDefaultSrcOSForTargetOS(TargetOS);
          if SrcOS='' then SrcOS:=TargetOS;
          NewDefTempl:=TDefineTemplate.Create('Define SrcOS',
            ctsDefaultppc386SourceOperatingSystem,
            ExternalMacroStart+'SrcOS',SrcOS,da_DefineRecurse);
          AddTemplate(NewDefTempl);
          // define #SrcOS2
          SrcOS2:=GetDefaultSrcOS2ForTargetOS(TargetOS);
          if SrcOS2='' then SrcOS2:=TargetOS;
          NewDefTempl:=TDefineTemplate.Create('Define SrcOS2',
            ctsDefaultppc386Source2OperatingSystem,
            ExternalMacroStart+'SrcOS2',SrcOS2,da_DefineRecurse);
          AddTemplate(NewDefTempl);
          break;
        end;
        inc(i);
      end;
      TheProcess.WaitOnExit;
      //DebugLn('TDefinePool.CreateFPCTemplate target OS done');
    finally
      //DebugLn('TDefinePool.CreateFPCTemplate Run with -iTO: OutputLine="',OutputLine,'"');
      TheProcess.Free;
    end;
    
    // ask for target processor -> ask compiler with switch -iTP
    TheProcess := TProcess.Create(nil);
    CmdLine:=CompilerPath;
    if CompilerOptions<>'' then
      CmdLine:=CmdLine+' '+CompilerOptions;
    CmdLine:=CmdLine+' -iTP';
    TheProcess.CommandLine := CmdLine;
    TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
    TheProcess.ShowWindow := swoHide;
    Step:='Running '+CmdLine;
    try
      TheProcess.Execute;
      if TheProcess.Output<>nil then
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf))
      else
        OutLen:=0;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          TargetProcessor:=copy(Buf,1,i-1);
          NewDefTempl:=TDefineTemplate.Create('Define TargetProcessor',
            ctsDefaultppc386TargetProcessor,
            ExternalMacroStart+'TargetProcessor',TargetProcessor,
            da_DefineRecurse);
          AddTemplate(NewDefTempl);
          break;
        end;
        inc(i);
      end;
      TheProcess.WaitOnExit;
      //DebugLn('TDefinePool.CreateFPCTemplate target CPU done');
    finally
      //DebugLn('TDefinePool.CreateFPCTemplate Run with -iTP: OutputLine="',OutputLine,'"');
      TheProcess.Free;
    end;

    // add
    if (LastDefTempl<>nil) then begin
      Result:=TDefineTemplate.Create('Free Pascal Compiler',
        ctsFreePascalCompilerInitialMacros,'','',da_Block);
      Result.AddChild(LastDefTempl.GetFirstSibling);
      Result.SetFlags([dtfAutoGenerated],[],false);
      //DebugLn('TDefinePool.CreateFPCTemplate FPC defines done');
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: TDefinePool.CreateFPCTemplate (',Step,'): ',E.Message);
    end;
  end;
  if Result<>nil then
    Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateFPCSrcTemplate(
  const FPCSrcDir, UnitSearchPath, PPUExt, DefaultTargetOS,
  DefaultProcessorName: string;
  UnitLinkListValid: boolean; var UnitLinkList: string;
  Owner: TObject): TDefineTemplate;
var
  Dir, TargetOS, SrcOS, SrcOS2, TargetProcessor, UnitLinks,
  IncPathMacro, SrcPathMacro: string;
  DS: char; // dir separator
  UnitTree: TAVLTree; // tree of TDefTemplUnitNameLink
  DefaultSrcOS, DefaultSrcOS2: string;

  procedure GatherUnits; forward;

  function FindUnitLink(const AnUnitName: string): TDefTemplUnitNameLink;
  var ANode: TAVLTreeNode;
    cmp: integer;
  begin
    if UnitTree=nil then GatherUnits;
    ANode:=UnitTree.Root;
    while ANode<>nil do begin
      Result:=TDefTemplUnitNameLink(ANode.Data);
      cmp:=CompareText(AnUnitName,Result.UnitName);
      if cmp<0 then
        ANode:=ANode.Left
      else if cmp>0 then
        ANode:=ANode.Right
      else
        exit;
    end;
    Result:=nil;
  end;

  procedure GatherUnits;
  
    function FileNameMacroCount(const AFilename: string): integer;
    // count number of macros in filename
    // a macro looks like this '$(name)' without a SpecialChar in front
    // macronames can contain macros themselves
    var i: integer;
    begin
      Result:=0;
      i:=1;
      while (i<=length(AFilename)) do begin
        if (AFilename[i]=SpecialChar) then
          inc(i,2)
        else if (AFilename[i]='$') then begin
          inc(i);
          if (i<=length(AFilename)) and (AFilename[i]='(') then
            inc(Result);
        end else
          inc(i);
      end;
    end;
    
    function BuildMacroFilename(const AFilename: string;
      var MacroCount, UsedMacroCount: integer): string;
    // replace Operating System and Processor Type with macros
    // MacroCount = number of macros are in the filename
    // UsedMacroCount = number of macros fitting to the current settings
    var DirStart, DirEnd, i: integer;
      DirName: string;
      
      function ReplaceDir(const MacroValue, DefaultMacroValue,
        MacroName: string): boolean;
      begin
        Result:=false;
        if CompareText(MacroValue,DirName)=0 then begin
          // this is a macro
          if CompareText(DirName,DefaultMacroValue)=0 then begin
            // the current settings would replace the macro to fit this filename
            inc(UsedMacroCount);
          end;
          BuildMacroFilename:=copy(BuildMacroFilename,1,DirStart-1)+MacroName+
            copy(BuildMacroFilename,DirEnd,length(BuildMacroFilename)-DirEnd+1);
          inc(DirEnd,length(MacroName)-length(DirName));
          DirName:=MacroName;
          Result:=true;
        end;
      end;
      
    begin
      MacroCount:=0;
      Result:=copy(AFilename,length(FPCSrcDir)+1,
                   length(AFilename)-length(FPCSrcDir));
      DirStart:=1;
      while (DirStart<=length(Result)) do begin
        while (DirStart<=length(Result)) and (Result[DirStart]=PathDelim)
        do
          inc(DirStart);
        DirEnd:=DirStart;
        while (DirEnd<=length(Result)) and (Result[DirEnd]<>PathDelim) do
          inc(DirEnd);
        if DirEnd>length(Result) then break;
        if DirEnd>DirStart then begin
          DirName:=copy(Result,DirStart,DirEnd-DirStart);
          // replace operating system
          for i:=Low(FPCOperatingSystemNames) to High(FPCOperatingSystemNames)
          do
            if ReplaceDir(FPCOperatingSystemNames[i],DefaultTargetOS,TargetOS)
            then
              break;
          // replace operating system class
          for i:=Low(FPCOperatingSystemAlternativeNames)
              to High(FPCOperatingSystemAlternativeNames)
          do
            if ReplaceDir(FPCOperatingSystemAlternativeNames[i],DefaultSrcOS,
              SrcOS)
            then
              break;
          // replace operating system secondary class
          for i:=Low(FPCOperatingSystemAlternative2Names)
              to High(FPCOperatingSystemAlternative2Names)
          do
            if ReplaceDir(FPCOperatingSystemAlternative2Names[i],DefaultSrcOS2,
              SrcOS2)
            then
              break;
          // replace processor type
          for i:=Low(FPCProcessorNames) to High(FPCProcessorNames) do
            if ReplaceDir(FPCProcessorNames[i],DefaultProcessorName,
              TargetProcessor)
            then
              break;
        end;
        DirStart:=DirEnd;
      end;
      Result:=FPCSrcDir+Result;
    end;
    
    procedure BrowseDirectory(ADirPath: string);
    const
      IgnoreDirs: array[1..16] of shortstring =(
          '.', '..', 'CVS', '.svn', 'examples', 'example', 'tests', 'fake',
          'ide', 'demo', 'docs', 'template', 'fakertl', 'install', 'installer',
          'compiler'
        );
    var
      AFilename, Ext, UnitName, MacroFileName: string;
      FileInfo: TSearchRec;
      NewUnitLink, OldUnitLink: TDefTemplUnitNameLink;
      i: integer;
      MacroCount, UsedMacroCount: integer;
      Priority: Integer;
    begin
      {$IFDEF VerboseFPCSrcScan}
      DebugLn('Browse ',ADirPath);
      {$ENDIF}
      if ADirPath='' then exit;
      if not (ADirPath[length(ADirPath)]=PathDelim) then
        ADirPath:=ADirPath+PathDelim;
      // set directory priority
      Priority:=0;
      if System.Pos(AppendPathDelim(FPCSrcDir)+'rtl'+PathDelim,ADirPath)>0 then
        Priority:=1;
      // search sources .pp,.pas
      if FindFirst(ADirPath+FileMask,faAnyFile,FileInfo)=0 then begin
        repeat
          AFilename:=FileInfo.Name;
          if (AFilename='') or (AFilename='.') or (AFilename='..') then
            continue;
          //debugln('Browse Filename=',AFilename,' IsDir=',(FileInfo.Attr and faDirectory)>0);
          i:=High(IgnoreDirs);
          while (i>=Low(IgnoreDirs)) and (AFilename<>IgnoreDirs[i]) do dec(i);
          //if CompareText(AFilename,'fcl')=0 then
          //  debugln('Browse ',AFilename,' IsDir=',(FileInfo.Attr and faDirectory)>0,' Ignore=',i>=Low(IgnoreDirs));
          if i>=Low(IgnoreDirs) then continue;
          AFilename:=ADirPath+AFilename;
          if (FileInfo.Attr and faDirectory)>0 then begin
            // ToDo: prevent cycling in links
            BrowseDirectory(AFilename);
          end else begin
            Ext:=UpperCaseStr(ExtractFileExt(AFilename));
            if (Ext='.PP') or (Ext='.PAS') or (Ext='.P') then begin
              // pascal unit found
              UnitName:=FileInfo.Name;
              UnitName:=copy(UnitName,1,length(UnitName)-length(Ext));
              if UnitName<>'' then begin
                OldUnitLink:=FindUnitLink(UnitName);
                MacroCount:=0;
                UsedMacroCount:=0;
                MacroFileName:=
                        BuildMacroFileName(AFilename,MacroCount,UsedMacroCount);
                if OldUnitLink=nil then begin
                  // first unit with this name
                  NewUnitLink:=TDefTemplUnitNameLink.Create;
                  NewUnitLink.UnitName:=UnitName;
                  NewUnitLink.FileName:=MacroFileName;
                  NewUnitLink.MacroCount:=MacroCount;
                  NewUnitLink.UsedMacroCount:=UsedMacroCount;
                  NewUnitLink.Priority:=Priority;
                  UnitTree.Add(NewUnitLink);
                end else begin
                  { there is another unit with this name

                    the decision which filename is the right one is based on a
                    simple heuristic:
                    - a filename with macros is preferred above one without
                      This skips the templates.
                    - A macro fitting better with the current settings
                      is preferred. For example:
                      If the current OS is linux then on fpc 1.0.x:
                        $(#FPCSrcDir)/fcl/classes/$(#TargetOS)/classes.pp
                    - A unit in the rtl is preferred above one in the fcl

                     FPC stores a unit many times, if there is different version
                     for each Operating System or Processor Type. And sometimes
                     units are stored in a combined OS (e.g. 'unix').
                     Therefore every occurence of such values is replaced by a
                     macro. And filenames without macros are always deleted if
                     there is a filename with a macro. (The filename without
                     macro is only used by the FPC team as a template source
                     for the OS specific).
                     If there are several macro filenames for the same unit, the
                     filename with the highest number of default values is used.
                     
                     For example:
                       classes.pp can be found in several places
                       In fpc 1.0.x:

                        <FPCSrcDir>/rtl/amiga/classes.pp
                        <FPCSrcDir>/fcl/amiga/classes.pp
                        <FPCSrcDir>/fcl/beos/classes.pp
                        <FPCSrcDir>/fcl/qnx/classes.pp
                        <FPCSrcDir>/fcl/sunos/classes.pp
                        <FPCSrcDir>/fcl/template/classes.pp
                        <FPCSrcDir>/fcl/classes/freebsd/classes.pp
                        <FPCSrcDir>/fcl/classes/go32v2/classes.pp
                        <FPCSrcDir>/fcl/classes/linux/classes.pp
                        <FPCSrcDir>/fcl/classes/netbsd/classes.pp
                        <FPCSrcDir>/fcl/classes/openbsd/classes.pp
                        <FPCSrcDir>/fcl/classes/os2/classes.pp
                        <FPCSrcDir>/fcl/classes/win32/classes.pp

                       In fpc 1.9.x/2.0.x:
                        <FPCSrcDir>/rtl/win32/classes.pp
                        <FPCSrcDir>/rtl/watcom/classes.pp
                        <FPCSrcDir>/rtl/go32v2/classes.pp
                        <FPCSrcDir>/rtl/netwlibc/classes.pp
                        <FPCSrcDir>/rtl/netbsd/classes.pp
                        <FPCSrcDir>/rtl/linux/classes.pp
                        <FPCSrcDir>/rtl/os2/classes.pp
                        <FPCSrcDir>/rtl/freebsd/classes.pp
                        <FPCSrcDir>/rtl/openbsd/classes.pp
                        <FPCSrcDir>/rtl/netware/classes.pp
                        <FPCSrcDir>/rtl/darwin/classes.pp
                        <FPCSrcDir>/rtl/morphos/classes.pp
                        <FPCSrcDir>/fcl/sunos/classes.pp
                        <FPCSrcDir>/fcl/beos/classes.pp
                        <FPCSrcDir>/fcl/qnx/classes.pp
                        <FPCSrcDir>/fcl/classes/win32/classes.pp
                        <FPCSrcDir>/fcl/classes/go32v2/classes.pp
                        <FPCSrcDir>/fcl/classes/netbsd/classes.pp
                        <FPCSrcDir>/fcl/classes/linux/classes.pp
                        <FPCSrcDir>/fcl/classes/os2/classes.pp
                        <FPCSrcDir>/fcl/classes/freebsd/classes.pp
                        <FPCSrcDir>/fcl/classes/openbsd/classes.pp
                        <FPCSrcDir>/fcl/template/classes.pp
                        <FPCSrcDir>/fcl/amiga/classes.pp

                       This means, there are several possible macro filenames:
                        $(#FPCSrcDir)/rtl/$(#TargetOS)/classes.pp
                        $(#FPCSrcDir)/fcl/$(#TargetOS)/classes.pp
                        $(#FPCSrcDir)/fcl/classes/$(#TargetOS)/classes.pp
                        
                   Example: libc.pp
                     <FPCSrcDir>/rtl/netwlibc/libc.pp
                     <FPCSrcDir>/packages/base/libc/libc.pp
                     There are no macros and no templates. This is a special case.
                     
                  }
                  if (UnitName='libc')
                  and (System.Pos(AppendPathDelim(FPCSrcDir)+'packages'+PathDelim,ADirPath)>0)
                  then begin
                    // <FPCSrcDir>/rtl/netwlibc/libc.pp
                    // <FPCSrcDir>/packages/base/libc/libc.pp
                    Priority:=2;
                  end;
                  if (UsedMacroCount>OldUnitLink.UsedMacroCount)
                  or ((UsedMacroCount=OldUnitLink.UsedMacroCount)
                    and ((Priority>OldUnitLink.Priority)
                      or ((Priority=OldUnitLink.Priority)
                          and (OldUnitLink.MacroCount<MacroCount))))
                  then begin
                    // take the new macro filename
                    OldUnitLink.Filename:=MacroFileName;
                    OldUnitLink.MacroCount:=MacroCount;
                    OldUnitLink.Priority:=Priority;
                  end;
                end;
              end;
            end;
          end;
        until FindNext(FileInfo)<>0;
      end;
      FindClose(FileInfo);
    end;
  
  begin
    if UnitTree=nil then
      UnitTree:=TAVLTree.Create(@CompareUnitLinkNodes)
    else
      UnitTree.FreeAndClear;
    BrowseDirectory(FPCSrcDir);
  end;
  

  procedure AddFPCSourceLinkForUnit(const AnUnitName: string);
  var UnitLink: TDefTemplUnitNameLink;
    s: string;
  begin
    // search
    if AnUnitName='' then exit;
    UnitLink:=FindUnitLink(AnUnitName);
    {$IFDEF VerboseFPCSrcScan}
    DbgOut('AddFPCSourceLinkForUnit ',AnUnitName,' ');
    if UnitLink<>nil then
      DebugLn(' -> ',UnitLink.Filename)
    else
      DebugLn('MISSING');
    {$ELSE}
    if UnitLink=nil then
      DebugLn(['WARNING: unable to find source of fpc unit ',AnUnitName]);
    {$ENDIF}
    if UnitLink=nil then exit;
    s:=AnUnitName+' '+UnitLink.Filename+LineEnding;
    UnitLinkList:=UnitLinkList+s;
  end;

  procedure FindStandardPPUSources;
  var PathStart, PathEnd: integer;
    ADirPath, UnitName: string;
    FileInfo: TSearchRec;
    CurMask: String;
  begin
    {$IFDEF VerboseFPCSrcScan}
    DebugLn('FindStandardPPUSources ..');
    {$ENDIF}
    // try every ppu file in every reachable directory (CompUnitPath)
    if UnitLinkListValid then exit;
    UnitLinkList:='';
    PathStart:=1;
    CurMask:=PPUExt;
    if CurMask='' then CurMask:='.ppu';
    if CurMask[1]<>'.' then
      CurMask:='.'+CurMask;
    CurMask:='*'+CurMask;
    //DebugLn('FindStandardPPUSources UnitSearchPath="',UnitSearchPath,'"');
    while PathStart<=length(UnitSearchPath) do begin
      while (PathStart<=length(UnitSearchPath))
      and (UnitSearchPath[PathStart]=';') do
        inc(PathStart);
      PathEnd:=PathStart;
      // extract single path from unit search path
      while (PathEnd<=length(UnitSearchPath))
      and (UnitSearchPath[PathEnd]<>';') do
        inc(PathEnd);
      if PathEnd>PathStart then begin
        ADirPath:=copy(UnitSearchPath,PathStart,PathEnd-PathStart);
        {$IFDEF VerboseFPCSrcScan}
        DebugLn('FindStandardPPUSources Searching ',CurMask,' in ',ADirPath);
        {$ENDIF}
        // search all ppu files in this directory
        if FindFirst(ADirPath+CurMask,faAnyFile,FileInfo)=0 then begin
          repeat
            UnitName:=lowercase(ExtractFileNameOnly(FileInfo.Name));
            {$IFDEF VerboseFPCSrcScan}
            DebugLn('FindStandardPPUSources Found: ',UnitName);
            {$ENDIF}
            AddFPCSourceLinkForUnit(UnitName);
            if (UnitTree=nil) or (UnitTree.Count=0) then exit;
          until FindNext(FileInfo)<>0;
        end;
        FindClose(FileInfo);
      end;
      PathStart:=PathEnd;
    end;
    UnitLinkListValid:=true;
  end;
  
  procedure AddProcessorTypeDefine(ParentDefTempl: TDefineTemplate);
  // some FPC source files expects defines 'i386' instead of 'CPUi386'
  // define them automatically with IF..THEN constructs
  var
    i: Integer;
    CPUName: String;
    IfTemplate: TDefineTemplate;
  begin
    // FPC defines CPUxxx defines (e.g. CPUI386, CPUPOWERPC).
    // These defines are created by the compiler depending
    // on xxx defines (i386, powerpc).
    // Create:
    //   IF CPUi386 then define i386
    //   IF CPUpowerpc then define powerpc
    //   ...
    for i:=Low(FPCProcessorNames) to high(FPCProcessorNames) do begin
      CPUName:=FPCProcessorNames[i];
      IfTemplate:=TDefineTemplate.Create('IFDEF CPU'+CPUName,
        'IFDEF CPU'+CPUName,'CPU'+CPUName,'',da_IfDef);
      IfTemplate.AddChild(TDefineTemplate.Create('DEFINE '+CPUName,
        'DEFINE '+CPUName,CPUName,'',da_DefineRecurse));
      ParentDefTempl.AddChild(IfTemplate);
    end;
  end;
  
  procedure AddSrcOSDefines(ParentDefTempl: TDefineTemplate);
  var
    IfTargetOSIsNotSrcOS: TDefineTemplate;
    RTLSrcOSDir: TDefineTemplate;
    IfTargetOSIsNotSrcOS2: TDefineTemplate;
    RTLSrcOS2Dir: TDefineTemplate;
  begin
    // if TargetOS<>SrcOS
    IfTargetOSIsNotSrcOS:=TDefineTemplate.Create(
      'IF TargetOS<>SrcOS',
      ctsIfTargetOSIsNotSrcOS,'',''''+TargetOS+'''<>'''+SrcOS+'''',da_If);
    // rtl/$(#SrcOS)
    RTLSrcOSDir:=TDefineTemplate.Create('SrcOS',SrcOS,'',
      SrcOS,da_Directory);
    IfTargetOSIsNotSrcOS.AddChild(RTLSrcOSDir);
    RTLSrcOSDir.AddChild(TDefineTemplate.Create('Include Path',
      'include path',
      ExternalMacroStart+'IncPath',IncPathMacro+';inc',
      da_Define));
    RTLSrcOSDir.AddChild(TDefineTemplate.Create('Include Path',
      'include path to TargetProcessor directories',
      ExternalMacroStart+'IncPath',IncPathMacro+';'+TargetProcessor,
      da_Define));
    ParentDefTempl.AddChild(IfTargetOSIsNotSrcOS);

    // if TargetOS<>SrcOS2
    IfTargetOSIsNotSrcOS2:=TDefineTemplate.Create(
      'IF TargetOS is not SrcOS2',
      ctsIfTargetOSIsNotSrcOS,'',''''+TargetOS+'''<>'''+SrcOS2+'''',da_If);
    // rtl/$(#SrcOS2)
    RTLSrcOS2Dir:=TDefineTemplate.Create('SrcOS2',SrcOS2,'',
      SrcOS2,da_Directory);
    IfTargetOSIsNotSrcOS2.AddChild(RTLSrcOS2Dir);
    RTLSrcOS2Dir.AddChild(TDefineTemplate.Create('Include Path',
      'include path to TargetProcessor directories',
      ExternalMacroStart+'IncPath',IncPathMacro+';'+TargetProcessor,
      da_DefineRecurse));
    ParentDefTempl.AddChild(IfTargetOSIsNotSrcOS2);
  end;

var
  DefTempl, MainDir, FCLDir, RTLDir, RTLOSDir, PackagesDir, CompilerDir,
  UtilsDir, DebugSvrDir: TDefineTemplate;
  s: string;
  FCLDBDir: TDefineTemplate;
  FCLDBInterbaseDir: TDefineTemplate;
  InstallerDir: TDefineTemplate;
  IFTempl: TDefineTemplate;
begin
  {$IFDEF VerboseFPCSrcScan}
  DebugLn('CreateFPCSrcTemplate ',FPCSrcDir,': length(UnitSearchPath)=',DbgS(length(UnitSearchPath)),' Valid=',DbgS(UnitLinkListValid),' PPUExt=',PPUExt);
  {$ENDIF}
  if UnitSearchPath='' then begin
    DebugLn(['Note: TDefinePool.CreateFPCSrcTemplate UnitSearchPath empty']);
  end;
  Result:=nil;
  if (FPCSrcDir='') or (not DirPathExists(FPCSrcDir)) then begin
    DebugLn(['TDefinePool.CreateFPCSrcTemplate FPCSrcDir does not exist: FPCSrcDir="',FPCSrcDir,'"']);
    exit;
  end;
  DS:=PathDelim;
  Dir:=AppendPathDelim(FPCSrcDir);
  TargetOS:='$('+ExternalMacroStart+'TargetOS)';
  SrcOS:='$('+ExternalMacroStart+'SrcOS)';
  SrcOS2:='$('+ExternalMacroStart+'SrcOS2)';
  TargetProcessor:='$('+ExternalMacroStart+'TargetProcessor)';
  IncPathMacro:='$('+ExternalMacroStart+'IncPath)';
  SrcPathMacro:='$('+ExternalMacroStart+'SrcPath)';
  UnitLinks:=UnitLinksMacroName;
  UnitTree:=nil;
  DefaultSrcOS:=GetDefaultSrcOSForTargetOS(DefaultTargetOS);
  DefaultSrcOS2:=GetDefaultSrcOS2ForTargetOS(DefaultTargetOS);


  Result:=TDefineTemplate.Create(StdDefTemplFPCSrc,
     Format(ctsFreePascalSourcesPlusDesc,['RTL, FCL, Packages, Compiler']),
     '','',da_Block);

  // try to find for every reachable ppu file the unit file in the FPC sources
  FindStandardPPUSources;
  DefTempl:=TDefineTemplate.Create('FPC Unit Links',
    ctsSourceFilenamesForStandardFPCUnits,
    UnitLinks,UnitLinkList,da_DefineRecurse);
  Result.AddChild(DefTempl);

  // The free pascal sources build a world of their own,
  // reset search paths
  MainDir:=TDefineTemplate.Create('Free Pascal Source Directory',
    ctsFreePascalSourceDir,'',FPCSrcDir,da_Directory);
  Result.AddChild(MainDir);
  DefTempl:=TDefineTemplate.Create('Reset SrcPath',
    ctsSrcPathInitialization,ExternalMacroStart+'SrcPath','',da_DefineRecurse);
  MainDir.AddChild(DefTempl);
  DefTempl:=TDefineTemplate.Create('Reset UnitPath',
    ctsUnitPathInitialization,ExternalMacroStart+'UnitPath','',da_DefineRecurse);
  MainDir.AddChild(DefTempl);
  // turn Nested comments on
  DefTempl:=TDefineTemplate.Create('Nested Comments',
    ctsNestedCommentsOn,ExternalMacroStart+'NestedComments','',da_DefineRecurse);
  MainDir.AddChild(DefTempl);
  // enable FPDocSystem to find compiler functions like writeln and readln
  {DefTempl:=TDefineTemplate.Create('FPDocSystem',
    ctsFPDocSystemOn,'FPDocSystem','',da_DefineRecurse);
  MainDir.AddChild(DefTempl);}

  // rtl
  RTLDir:=TDefineTemplate.Create('RTL',ctsRuntimeLibrary,'','rtl',da_Directory);
  MainDir.AddChild(RTLDir);

  // rtl include paths
  s:=IncPathMacro
    +';'+Dir+'rtl'+DS+'objpas'+DS
    +';'+Dir+'rtl'+DS+'objpas'+DS+'sysutils'
    +';'+Dir+'rtl'+DS+'objpas'+DS+'classes'
    +';'+Dir+'rtl'+DS+'inc'+DS
    +';'+Dir+'rtl'+DS+'inc'+DS+'graph'+DS
    +';'+Dir+'rtl'+DS+SrcOS+DS;
  if (TargetOS<>SrcOS) then
    s:=s+';'+Dir+'rtl'+DS+TargetOS+DS;
  if (SrcOS2<>'') and (SrcOS2<>SrcOS) then begin
    s:=s+';'+Dir+'rtl'+DS+SrcOS2+DS
        +';'+Dir+'rtl'+DS+SrcOS2+DS+TargetProcessor;
  end;
  s:=s
    +';'+Dir+'rtl'+DS+TargetProcessor+DS
    +';'+Dir+'rtl'+DS+TargetOS+DS+TargetProcessor+DS;
  RTLDir.AddChild(TDefineTemplate.Create('Include Path',
    Format(ctsIncludeDirectoriesPlusDirs,
    ['objpas, inc,'+TargetProcessor+','+SrcOS]),
    ExternalMacroStart+'IncPath',s,da_DefineRecurse));

  // rtl/$(#TargetOS)
  RTLOSDir:=TDefineTemplate.Create('TargetOS','Target OS','',
                                   TargetOS,da_Directory);
  s:=IncPathMacro
    +';'+Dir+'rtl'+DS+TargetOS+DS+SrcOS+'inc' // e.g. rtl/win32/wininc/
    +';'+Dir+'rtl'+DS+TargetOS+DS+TargetProcessor+DS
    ;
  RTLOSDir.AddChild(TDefineTemplate.Create('Include Path',
    Format(ctsIncludeDirectoriesPlusDirs,[TargetProcessor]),
    ExternalMacroStart+'IncPath',
    s,da_DefineRecurse));
  s:=SrcPathMacro
    +';'+Dir+'rtl'+DS+'objpas'+DS;
  RTLOSDir.AddChild(TDefineTemplate.Create('Src Path',
    Format(ctsAddsDirToSourcePath,[TargetProcessor]),
    ExternalMacroStart+'SrcPath',s,da_DefineRecurse));
  RTLDir.AddChild(RTLOSDir);

  // rtl: IF SrcOS=win then add include path rtl/win/wininc
  IFTempl:=TDefineTemplate.Create('If SrcOS=win','If SrcOS=win',
    '',SrcOS+'=win',da_If);
  IFTempl.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['wininc']),
      ExternalMacroStart+'IncPath',
      IncPathMacro
      +';'+FPCSrcDir+'rtl'+DS+'win'+DS+'wininc'
      +';'+FPCSrcDir+'rtl'+DS+'win',
      da_DefineRecurse));
  RTLDir.AddChild(IFTempl);

  // rtl: IF TargetOS=darwin then add include path rtl/freebsd
  IFTempl:=TDefineTemplate.Create('If TargetOS=darwin','If TargetOS=darwin',
    '',TargetOS+'=darwin',da_If);
  IFTempl.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['rtl'+DS+'freebsd']),
      ExternalMacroStart+'IncPath',
      IncPathMacro
      +';'+FPCSrcDir+'rtl'+DS+'freebsd',
      da_DefineRecurse));
  RTLDir.AddChild(IFTempl);

  // add processor and SrcOS alias defines for the RTL
  AddProcessorTypeDefine(RTLDir);
  AddSrcOSDefines(RTLDir);


  // fcl
  FCLDir:=TDefineTemplate.Create('FCL',ctsFreePascalComponentLibrary,'','fcl',
      da_Directory);
  MainDir.AddChild(FCLDir);
  FCLDir.AddChild(TDefineTemplate.Create('Include Path',
    Format(ctsIncludeDirectoriesPlusDirs,['inc,'+SrcOS]),
    ExternalMacroStart+'IncPath',
    IncPathMacro
    +';'+Dir+'fcl'+DS+'inc'+DS
    +';'+Dir+'fcl'+DS+'classes'+DS
    +';'+Dir+'fcl'+DS+TargetOS+DS // TargetOS before SrcOS !
    +';'+Dir+'fcl'+DS+SrcOS+DS
    ,da_DefineRecurse));

  // fcl/db
  FCLDBDir:=TDefineTemplate.Create('DB','DB','','db',da_Directory);
  FCLDir.AddChild(FCLDBDir);
  FCLDBInterbaseDir:=TDefineTemplate.Create('interbase','interbase','',
    'interbase',da_Directory);
  FCLDBDir.AddChild(FCLDBInterbaseDir);
  FCLDBInterbaseDir.AddChild(TDefineTemplate.Create('SrcPath',
    'SrcPath addition',
    ExternalMacroStart+'SrcPath',
    SrcPathMacro
    +';'+Dir+'packages'+DS+'base'+DS+'ibase'
    ,da_Define));

  // packages
  PackagesDir:=TDefineTemplate.Create('Packages',ctsPackageDirectories,'',
     'packages',da_Directory);
  MainDir.AddChild(PackagesDir);

  // utils
  UtilsDir:=TDefineTemplate.Create('Utils',ctsUtilsDirectories,'',
     'utils',da_Directory);
  MainDir.AddChild(UtilsDir);

  // utils/debugsvr
  DebugSvrDir:=TDefineTemplate.Create('DebugSvr','Debug Server','',
     'debugsvr',da_Directory);
  UtilsDir.AddChild(DebugSvrDir);
  DebugSvrDir.AddChild(TDefineTemplate.Create('Interface Path',
    Format(ctsAddsDirToSourcePath,['..']),ExternalMacroStart+'SrcPath',
    '..;'+ExternalMacroStart+'SrcPath',da_DefineRecurse));

  // packages
  InstallerDir:=TDefineTemplate.Create('Installer',ctsInstallerDirectories,'',
     'installer',da_Directory);
  InstallerDir.AddChild(TDefineTemplate.Create('SrcPath','SrcPath addition',
    ExternalMacroStart+'SrcPath',
    SrcPathMacro+';'+Dir+'ide;'+Dir+'fv',da_Define));
  MainDir.AddChild(InstallerDir);

  // compiler
  CompilerDir:=TDefineTemplate.Create('Compiler',ctsCompiler,'','compiler',
     da_Directory);
  AddProcessorTypeDefine(CompilerDir);
  CompilerDir.AddChild(TDefineTemplate.Create('SrcPath','SrcPath addition',
    ExternalMacroStart+'SrcPath',
    SrcPathMacro+';'+Dir+TargetProcessor,da_Define));
  CompilerDir.AddChild(TDefineTemplate.Create('IncPath','IncPath addition',
    ExternalMacroStart+'IncPath',
    IncPathMacro+';'+Dir+'compiler',da_DefineRecurse));
  MainDir.AddChild(CompilerDir);

  // clean up
  if UnitTree<>nil then begin
    UnitTree.FreeAndClear;
    UnitTree.Free;
  end;

  Result.SetDefineOwner(Owner,true);
  Result.SetFlags([dtfAutoGenerated],[],false);
end;

function TDefinePool.CreateDelphiSrcPath(DelphiVersion: integer;
  const PathPrefix: string): string;
begin
  case DelphiVersion of
  1..5:
    Result:=PathPrefix+'Source/Rtl/Win;'
      +PathPrefix+'Source/Rtl/Sys;'
      +PathPrefix+'Source/Rtl/Corba;'
      +PathPrefix+'Source/Vcl;';
  else
    // 6 and above
    Result:=PathPrefix+'Source/Rtl/Win;'
      +PathPrefix+'Source/Rtl/Sys;'
      +PathPrefix+'Source/Rtl/Common;'
      +PathPrefix+'Source/Rtl/Corba40;'
      +PathPrefix+'Source/Vcl;';
  end;
end;

function TDefinePool.CreateLazarusSrcTemplate(
  const LazarusSrcDir, WidgetType, ExtraOptions: string;
  Owner: TObject): TDefineTemplate;
type
  TLazWidgetSet = (wsGtk, wsGtk2, wsGnome, wsWin32, wsWinCE, wsCarbon, wsQT);
const
  ds: char = PathDelim;
  LazWidgetSets: array[TLazWidgetSet] of string = (
    'gtk','gtk2','gnome','win32','wince','carbon','qt');

  function D(const Filename: string): string;
  begin
    Result:=SetDirSeparators(Filename);
  end;
    
var
  MainDir, DirTempl, SubDirTempl, IntfDirTemplate, IfTemplate,
  LCLUnitsDir, LCLUnitsCPUOSDir, LCLUnitsCPUOSWidgetSetDir,
  SubTempl: TDefineTemplate;
  TargetOS, SrcOS, SrcPath: string;
  i: Integer;
  CurCPU, CurOS, CurWidgetSet, ExtraSrcPath: string;
  ElseTemplate: TDefineTemplate;
  LCLWidgetSetDir: TDefineTemplate;
  IDEIntfDir: TDefineTemplate;
  ToolsInstallDirTempl: TDefineTemplate;
  CurCPUOS: String;
  SynEditDirTempl: TDefineTemplate;
  SynEditUnitsDirTempl: TDefineTemplate;
  CodeToolsDirTempl: TDefineTemplate;
  CodeToolsUnitsDirTempl: TDefineTemplate;
begin
  Result:=nil;
  if (LazarusSrcDir='') or (WidgetType='') then exit;
  //TargetCPU:='$('+ExternalMacroStart+'TargetCPU)';
  TargetOS:='$('+ExternalMacroStart+'TargetOS)';
  SrcOS:='$('+ExternalMacroStart+'SrcOS)';
  SrcPath:='$('+ExternalMacroStart+'SrcPath)';

  // <LazarusSrcDir>
  MainDir:=TDefineTemplate.Create(
    StdDefTemplLazarusSrcDir, ctsDefsForLazarusSources,'',LazarusSrcDir,
    da_Directory);
  // clear src path
  MainDir.AddChild(TDefineTemplate.Create('Clear SrcPath','Clear SrcPath',
    ExternalMacroStart+'SrcPath','',da_DefineRecurse));
  // if SrcOS<>win
  IfTemplate:=TDefineTemplate.Create('IF '''+SrcOS+'''<>''win''',
    ctsIfTargetOSIsNotWin32,'',''''+SrcOS+'''<>''win''',da_If);
    // then define #SrcPath := #SrcPath;lcl/nonwin32
    IfTemplate.AddChild(TDefineTemplate.Create('win32api for non win',
      Format(ctsAddsDirToSourcePath,[d(LazarusSrcDir+'/lcl/nonwin32')]),
      ExternalMacroStart+'SrcPath',
      d(LazarusSrcDir+'/lcl/nonwin32;')+SrcPath,da_DefineRecurse));
  MainDir.AddChild(IfTemplate);
  MainDir.AddChild(TDefineTemplate.Create(
    'LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),ExternalMacroStart+'SrcPath',
    d('lcl;lcl/interfaces/')+WidgetType+';'+SrcPath
    ,da_Define));
  // set SrcPath for IDE
  MainDir.AddChild(TDefineTemplate.Create(
    'Component path addition',
    Format(ctsAddsDirToSourcePath,['designer, debugger, components, ..']),
    ExternalMacroStart+'SrcPath',
      d('designer;'
       +'designer/jitform;'
       +'debugger;'
       +'converter;'
       +'packager;'
       +'packager/registration;'
       +'ideintf;'
       +'ide;'
       +'components/synedit;'
       +'components/codetools;'
       +'components/custom;'
       +'components/mpaslex;')
       +SrcPath
    ,da_Define));
  // include path addition
  MainDir.AddChild(TDefineTemplate.Create('includepath addition',
    Format(ctsSetsIncPathTo,['ide/include, ide/include/TargetOS, ide/include/SrcOS']),
    ExternalMacroStart+'IncPath',
    d('ide/include;ide/include/'+TargetOS+';ide/include/'+SrcOS),
    da_Define));
  // turn Nested comments on
  MainDir.AddChild(TDefineTemplate.Create('Nested Comments',
    ctsNestedCommentsOn,ExternalMacroStart+'NestedComments','',da_DefineRecurse));


  // <LazarusSrcDir>/include
  // (does not need special setup)

  // <LazarusSrcDir>/ide
  DirTempl:=TDefineTemplate.Create('ide',ctsIDEDirectory,
    '','ide',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl, components']),
    ExternalMacroStart+'SrcPath',
      d('..;'
       +'../designer;'
       +'../designer/jitform;'
       +'../debugger;'
       +'../converter;'
       +'../packager;'
       +'../packager/registration;'
       +'../ideintf;'
       +'../lcl;'
       +'../lcl/interfaces/'+WidgetType+';'
       +'../components/synedit;'
       +'../components/codetools;'
       +'../components/custom;'
       +'../components/mpaslex;')
    ,da_DefineRecurse));
  // include path addition
  DirTempl.AddChild(TDefineTemplate.Create('includepath addition',
    Format(ctsSetsIncPathTo,['include, include/TargetOS, include/SrcOS']),
    ExternalMacroStart+'IncPath',
    d('include;include/'+TargetOS+';include/'+SrcOS),
    da_Define));
  MainDir.AddChild(DirTempl);

  // <LazarusSrcDir>/designer
  DirTempl:=TDefineTemplate.Create('Designer',ctsDesignerDirectory,
    '','designer',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    SrcPathMacroName,
      d('../lcl'
       +';../lcl/interfaces/'+WidgetType)
       +';'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('main path addition',
    Format(ctsAddsDirToSourcePath,[ctsLazarusMainDirectory]),
    SrcPathMacroName,
    d('../ide;../packager;')+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('components path addition',
    Format(ctsAddsDirToSourcePath,['synedit']),
    ExternalMacroStart+'SrcPath',
      d('../ideintf;'
       +'../components/synedit;'
       +'../components/codetools;'
       +'../components/custom;'
       +'jitform;')
       +SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('includepath addition',
    Format(ctsIncludeDirectoriesPlusDirs,['include']),
    ExternalMacroStart+'IncPath',
    d('../ide/include;../ide/include/'+TargetOS),
    da_Define));
  // <LazarusSrcDir>/designer/jitform
  SubDirTempl:=TDefineTemplate.Create('JITForm',ctsJITFormDirectory,
    '','jitform',da_Directory);
  SubDirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    SrcPathMacroName,
      d('../../lcl'
      +';../../lcl/interfaces/'+WidgetType)
      +';'+SrcPath
    ,da_Define));
  DirTempl.AddChild(SubDirTempl);
  // <LazarusSrcDir>/designer/units
  SubDirTempl:=TDefineTemplate.Create('Designer Units',
    ctsDesignerUnitsDirectory,'','units',da_Directory);
  SubDirTempl.AddChild(TDefineTemplate.Create('CompiledSrcPath',
    ctsCompiledSrcPath,CompiledSrcPathMacroName,d('../jitform/'),
    da_Define));
  DirTempl.AddChild(SubDirTempl);
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/images


  // <LazarusSrcDir>/debugger
  DirTempl:=TDefineTemplate.Create('Debugger',ctsDebuggerDirectory,
    '','debugger',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl, components']),
    ExternalMacroStart+'SrcPath',
      d('../ide'
       +';../ideintf'
       +';../components/codetools'
       +';../lcl'
       +';../lcl/interfaces/'+WidgetType)
       +';'+SrcPath
    ,da_DefineRecurse));
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/converter
  DirTempl:=TDefineTemplate.Create('Converter',ctsDebuggerDirectory,
    '','converter',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl, components']),
    ExternalMacroStart+'SrcPath',
      d('../ide'
       +';../ideintf'
       +';../components/codetools'
       +';../components/synedit'
       +';../packager'
       +';../debugger'
       +';../designer'
       +';../lcl'
       +';../lcl/interfaces/'+WidgetType)
       +';'+SrcPath
    ,da_DefineRecurse));
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/doceditor
  DirTempl:=TDefineTemplate.Create('Doc Editor',ctsDocEditorDirectory,
    '','doceditor',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('Unit path addition',
    Format(ctsAddsDirToSourcePath,['lcl, components']),
    SrcPathMacroName,
      d('../ideintf'
       +';../components/codetools'
       +';../lcl'
       +';../lcl/interfaces/'+WidgetType)
       +';'+SrcPath
    ,da_DefineRecurse));
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/packager
  DirTempl:=TDefineTemplate.Create('Packager',ctsDesignerDirectory,
    '','packager',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    SrcPathMacroName,
      d('../lcl'
      +';../lcl/interfaces/'+WidgetType)
      +';'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('main path addition',
    Format(ctsAddsDirToSourcePath,[ctsLazarusMainDirectory]),
    SrcPathMacroName,
    '../ide;'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('components path addition',
    Format(ctsAddsDirToSourcePath,['synedit']),
    SrcPathMacroName,
      d('registration;'
      +'../ideintf;'
      +'../components/synedit;'
      +'../components/codetools;'
      +'../components/custom;')
      +SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('includepath addition',
    Format(ctsIncludeDirectoriesPlusDirs,['include']),
    ExternalMacroStart+'IncPath',
    d('../ide/include;../ide/include/'+TargetOS),
    da_Define));
  // <LazarusSrcDir>/packager/registration
  SubDirTempl:=TDefineTemplate.Create('Registration',
    ctsPackagerRegistrationDirectory,'','registration',da_Directory);
  DirTempl.AddChild(SubDirTempl);
  // <LazarusSrcDir>/packager/units
  SubDirTempl:=TDefineTemplate.Create('Packager Units',
    ctsPackagerUnitsDirectory,'','units',da_Directory);
  SubDirTempl.AddChild(TDefineTemplate.Create('CompiledSrcPath',
    ctsCompiledSrcPath,CompiledSrcPathMacroName,
    LazarusSrcDir+d('/packager/registration'),
    da_DefineRecurse));
  DirTempl.AddChild(SubDirTempl);
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/ideintf
  IDEIntfDir:=TDefineTemplate.Create('IDEIntf',ctsIDEIntfDirectory,
    '','ideintf',da_Directory);
  IDEIntfDir.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    SrcPathMacroName,
      d('../components/codetools'
       +';../packager/registration'
       +';../lcl'
       +';../lcl/interfaces/'+WidgetType)
       +';'+SrcPath
    ,da_Define));
  IDEIntfDir.AddChild(TDefineTemplate.Create('CompiledSrcPath',
    ctsCompiledSrcPath,CompiledSrcPathMacroName,
    LazarusSrcDir+d('/ideintf'),
    da_DefineRecurse));
  MainDir.AddChild(IDEIntfDir);

  // <LazarusSrcDir>/examples
  DirTempl:=TDefineTemplate.Create('Examples',
    Format(ctsNamedDirectory,['Examples']),
    '','examples',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    ExternalMacroStart+'SrcPath',
      d('../lcl'
      +';../lcl/interfaces/'+WidgetType+';'+SrcPath)
    ,da_Define));
  MainDir.AddChild(DirTempl);
  
  // <LazarusSrcDir>/lcl
  DirTempl:=TDefineTemplate.Create('LCL',Format(ctsNamedDirectory,['LCL']),
    '','lcl',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('IncludePath',
     Format(ctsIncludeDirectoriesPlusDirs,['include']),
     ExternalMacroStart+'IncPath',
     'include',da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['forms']),
    ExternalMacroStart+'SrcPath','forms;'+SrcPath,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['widgetset']),
    ExternalMacroStart+'SrcPath','widgetset;'+SrcPath,da_Define));
  MainDir.AddChild(DirTempl);
  
  // <LazarusSrcDir>/lcl/forms
  LCLWidgetSetDir:=TDefineTemplate.Create('Forms',Format(ctsNamedDirectory,['WidgetSet']),
    '','forms',da_Directory);
  LCLWidgetSetDir.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['..']),
    ExternalMacroStart+'SrcPath','..;'+SrcPath,da_Define));
  DirTempl.AddChild(LCLWidgetSetDir);

  // <LazarusSrcDir>/lcl/widgetset
  LCLWidgetSetDir:=TDefineTemplate.Create('WidgetSet',Format(ctsNamedDirectory,['WidgetSet']),
    '','widgetset',da_Directory);
  LCLWidgetSetDir.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['..']),
    ExternalMacroStart+'SrcPath','..;'+SrcPath,da_Define));
  DirTempl.AddChild(LCLWidgetSetDir);

  // <LazarusSrcDir>/lcl/units
  LCLUnitsDir:=TDefineTemplate.Create('Units',Format(ctsNamedDirectory,['Units']),
    '','units',da_Directory);
  DirTempl.AddChild(LCLUnitsDir);
  for i:=Low(Lazarus_CPU_OS_Widget_Combinations)
      to High(Lazarus_CPU_OS_Widget_Combinations) do
  begin
    SplitLazarusCPUOSWidgetCombo(Lazarus_CPU_OS_Widget_Combinations[i],
                                 CurCPU,CurOS,CurWidgetSet);
    // <LazarusSrcDir>/lcl/units/<TargetCPU>-<TargetOS>
    // these directories contain the output of the LCL (excluding the interfaces)
    CurCPUOS:=CurCPU+'-'+CurOS;
    LCLUnitsCPUOSDir:=LCLUnitsDir.FindChildByName(CurCPUOS);
    if LCLUnitsCPUOSDir=nil then begin
      LCLUnitsCPUOSDir:=TDefineTemplate.Create(CurCPUOS,
        Format(ctsNamedDirectory,[CurCPUOS]),
        '',CurCPUOS,da_Directory);
      LCLUnitsDir.AddChild(LCLUnitsCPUOSDir);

      ExtraSrcPath:='../..;../../widgetset';
      if CurOS<>'win32' then
        ExtraSrcPath:=ExtraSrcPath+';../../nonwin32';
      LCLUnitsCPUOSDir.AddChild(TDefineTemplate.Create('CompiledSrcPath',
         ctsSrcPathForCompiledUnits,CompiledSrcPathMacroName,
         d(ExtraSrcPath),da_Define));
    end;
    // <LazarusSrcDir>/lcl/units/<TargetCPU>-<TargetOS>/<WidgetSet>
    // these directories contain the output of the LCL interfaces
    LCLUnitsCPUOSWidgetSetDir:=LCLUnitsCPUOSDir.FindChildByName(CurWidgetSet);
    if LCLUnitsCPUOSWidgetSetDir=nil then begin
      LCLUnitsCPUOSWidgetSetDir:=TDefineTemplate.Create(CurWidgetSet,
        Format(ctsNamedDirectory,[CurWidgetSet]),
        '',CurWidgetSet,da_Directory);
      LCLUnitsCPUOSDir.AddChild(LCLUnitsCPUOSWidgetSetDir);
      ExtraSrcPath:='../../../interfaces/'+CurWidgetSet;
      if (CurWidgetSet='gnome') or (CurWidgetSet='gtk2') then
        ExtraSrcPath:=ExtraSrcPath+';../../../interfaces/gtk';
      LCLUnitsCPUOSWidgetSetDir.AddChild(
        TDefineTemplate.Create('CompiledSrcPath',
          ctsSrcPathForCompiledUnits,CompiledSrcPathMacroName,
          d(ExtraSrcPath),da_Define));
    end;
  end;

  // <LazarusSrcDir>/lcl/interfaces
  SubDirTempl:=TDefineTemplate.Create('WidgetDirectory',
    ctsWidgetDirectory,'','interfaces',da_Directory);
  // add lcl to the source path of all widget set directories
  SubDirTempl.AddChild(TDefineTemplate.Create('LCL Path',
    Format(ctsAddsDirToSourcePath,['lcl']),ExternalMacroStart+'SrcPath',
    LazarusSrcDir+d('/lcl;')+LazarusSrcDir+d('/lcl/widgetset;')+SrcPath,
    da_DefineRecurse));
  DirTempl.AddChild(SubDirTempl);
  
  // <LazarusSrcDir>/lcl/interfaces/gtk
  IntfDirTemplate:=TDefineTemplate.Create('gtkIntfDirectory',
    ctsIntfDirectory,'','gtk',da_Directory);
    // if LCLWidgetType=gtk2
    IfTemplate:=TDefineTemplate.Create('IF '''+WidgetType+'''=''gtk2''',
      ctsIfLCLWidgetTypeEqualsGtk2,'',''''+WidgetType+'''=''gtk2''',da_If);
      // then define gtk2
      IfTemplate.AddChild(TDefineTemplate.Create('Define gtk2',
        ctsDefineMacroGTK2,'gtk2','',da_Define));
    IntfDirTemplate.AddChild(IfTemplate);
    // else LCLWidgetType=gtk2
    ElseTemplate:=TDefineTemplate.Create('ELSE',
      ctsElse,'','',da_Else);
      // then define gtk1
      ElseTemplate.AddChild(TDefineTemplate.Create('Define gtk1',
        ctsDefineMacroGTK1,'gtk1','',da_Define));
    IntfDirTemplate.AddChild(ElseTemplate);
  SubDirTempl.AddChild(IntfDirTemplate);

  // <LazarusSrcDir>/lcl/interfaces/gtk2
  IntfDirTemplate:=TDefineTemplate.Create('gtk2IntfDirectory',
    ctsGtk2IntfDirectory,'','gtk2',da_Directory);
  // add '../gtk' to the SrcPath
  IntfDirTemplate.AddChild(TDefineTemplate.Create('SrcPath',
    Format(ctsAddsDirToSourcePath,['gtk']),ExternalMacroStart+'SrcPath',
    d('../gtk;')+SrcPath,da_Define));
  SubDirTempl.AddChild(IntfDirTemplate);
  
  // <LazarusSrcDir>/lcl/interfaces/gnome
  IntfDirTemplate:=TDefineTemplate.Create('gnomeIntfDirectory',
    ctsGnomeIntfDirectory,'','gnome',da_Directory);
  // add '../gtk' to the SrcPath
  IntfDirTemplate.AddChild(TDefineTemplate.Create('SrcPath',
    Format(ctsAddsDirToSourcePath,['gtk']),ExternalMacroStart+'SrcPath',
    d('../gtk;')+SrcPath,da_Define));
    // if LCLWidgetType=gnome2
    IfTemplate:=TDefineTemplate.Create('IF '''+WidgetType+'''=''gnome2''',
      ctsIfLCLWidgetTypeEqualsGnome2,'',''''+WidgetType+'''=''gnome2''',da_If);
      // then define gnome2
      IfTemplate.AddChild(TDefineTemplate.Create('Define gnome2',
        ctsDefineMacroGTK2,'gnome2','',da_Define));
    IntfDirTemplate.AddChild(IfTemplate);
  SubDirTempl.AddChild(IntfDirTemplate);

  // <LazarusSrcDir>/lcl/interfaces/win32
  // no special

  // <LazarusSrcDir>/lcl/interfaces/wince
  IntfDirTemplate:=TDefineTemplate.Create('winceIntfDirectory',
    ctsIntfDirectory,'','wince',da_Directory);
    // then define wince1
    IntfDirTemplate.AddChild(TDefineTemplate.Create('Define wince1',
      ctsDefineMacroWinCE1,'wince1','',da_Define));
  SubDirTempl.AddChild(IntfDirTemplate);

  // <LazarusSrcDir>/lcl/interfaces/carbon
  IntfDirTemplate:=TDefineTemplate.Create('carbonIntfDirectory',
    ctsIntfDirectory,'','carbon',da_Directory);
    // then define carbon1
    IntfDirTemplate.AddChild(TDefineTemplate.Create('Define carbon1',
      ctsDefineMacroCarbon1,'carbon1','',da_Define));
  SubDirTempl.AddChild(IntfDirTemplate);

  // <LazarusSrcDir>/lcl/interfaces/qt
  IntfDirTemplate:=TDefineTemplate.Create('qtIntfDirectory',
    ctsIntfDirectory,'','qt',da_Directory);
    // then define qt1
    IntfDirTemplate.AddChild(TDefineTemplate.Create('Define qt1',
      ctsDefineMacroQT1,'qt1','',da_Define));
  SubDirTempl.AddChild(IntfDirTemplate);

  // <LazarusSrcDir>/components
  DirTempl:=TDefineTemplate.Create('Components',ctsComponentsDirectory,
    '','components',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL Path',
    Format(ctsAddsDirToSourcePath,['lcl']),
    ExternalMacroStart+'SrcPath',
    d(LazarusSrcDir+'/lcl'
    +';'+LazarusSrcDir+'/lcl/interfaces/'+WidgetType)
    +';'+SrcPath
    ,da_DefineRecurse));
  MainDir.AddChild(DirTempl);
  
  // <LazarusSrcDir>/components/synedit/units
  SynEditDirTempl:=TDefineTemplate.Create('synedit',
    'SynEdit','','synedit',da_Directory);
  SynEditUnitsDirTempl:=TDefineTemplate.Create('synedit output directory',
    'units','','units',da_Directory);
  SynEditDirTempl.AddChild(SynEditUnitsDirTempl);
  SynEditUnitsDirTempl.AddChild(TDefineTemplate.Create('CompiledSrcPath',
     ctsSrcPathForCompiledUnits,
     ExternalMacroStart+'CompiledSrcPath',
     d(LazarusSrcDir+'components/synedit')
     ,da_DefineRecurse));
  DirTempl.AddChild(SynEditDirTempl);

  // <LazarusSrcDir>/components/codetools/units
  CodeToolsDirTempl:=TDefineTemplate.Create('codetools',
    'CodeTools','','codetools',da_Directory);
  CodeToolsUnitsDirTempl:=TDefineTemplate.Create('codetools output directory',
    'units','','units',da_Directory);
  CodeToolsDirTempl.AddChild(CodeToolsUnitsDirTempl);
  CodeToolsUnitsDirTempl.AddChild(TDefineTemplate.Create('CompiledSrcPath',
     ctsSrcPathForCompiledUnits,
     ExternalMacroStart+'CompiledSrcPath',
     d(LazarusSrcDir+'components/codetools')
     ,da_DefineRecurse));
  DirTempl.AddChild(CodeToolsDirTempl);

  // <LazarusSrcDir>/components/custom
  SubDirTempl:=TDefineTemplate.Create('Custom Components',
    ctsCustomComponentsDirectory,
    '','custom',da_Directory);
  SubDirTempl.AddChild(TDefineTemplate.Create('lazarus standard components',
    Format(ctsAddsDirToSourcePath,['synedit']),
    ExternalMacroStart+'SrcPath',
    d('../synedit;')
    +SrcPath
    ,da_DefineRecurse));
  DirTempl.AddChild(SubDirTempl);

  // <LazarusSrcDir>/tools
  DirTempl:=TDefineTemplate.Create('Tools',
    ctsToolsDirectory,'','tools',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    ExternalMacroStart+'SrcPath',
    d('../lcl;../lcl/interfaces/'+WidgetType
    +';../components/codetools')
    +';'+SrcPath
    ,da_Define));
    // <LazarusSrcDir>/tools/install
    ToolsInstallDirTempl:=TDefineTemplate.Create('Install',
      ctsInstallDirectory,'','install',da_Directory);
    DirTempl.AddChild(ToolsInstallDirTempl);
    ToolsInstallDirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
      Format(ctsAddsDirToSourcePath,['lcl']),
      ExternalMacroStart+'SrcPath',
      d('../../lcl;../../lcl/interfaces/'+WidgetType
      +';../../components/codetools')
      +';'+SrcPath
      ,da_Define));
  MainDir.AddChild(DirTempl);

  // extra options
  SubTempl:=CreateFPCCommandLineDefines(StdDefTemplLazarusBuildOpts,
                                        ExtraOptions,true,Owner);
  MainDir.AddChild(SubTempl);

  // put it all into a block
  if MainDir<>nil then begin
    Result:=TDefineTemplate.Create(StdDefTemplLazarusSources,
       ctsLazarusSources,'','',da_Block);
    Result.AddChild(MainDir);
  end;
  
  Result.SetDefineOwner(Owner,true);
  Result.SetFlags([dtfAutoGenerated],[],false);
end;

function TDefinePool.CreateLCLProjectTemplate(
  const LazarusSrcDir, WidgetType, ProjectDir: string;
  Owner: TObject): TDefineTemplate;
var DirTempl: TDefineTemplate;
begin
  Result:=nil;
  if (LazarusSrcDir='') or (WidgetType='') or (ProjectDir='') then exit;
  DirTempl:=TDefineTemplate.Create('ProjectDir',ctsAnLCLProject,
    '',ProjectDir,da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL',
    Format(ctsAddsDirToSourcePath,['lcl']),
    ExternalMacroStart+'SrcPath',
    LazarusSrcDir+PathDelim+'lcl;'
     +LazarusSrcDir+PathDelim+'lcl'+PathDelim+'interfaces'
     +PathDelim+WidgetType
     +';$('+ExternalMacroStart+'SrcPath)'
    ,da_DefineRecurse));
  Result:=TDefineTemplate.Create(StdDefTemplLCLProject,
       'LCL Project','','',da_Block);
  Result.AddChild(DirTempl);
  Result.SetDefineOwner(Owner,true);
  Result.SetFlags([dtfAutoGenerated],[],false);
end;

function TDefinePool.CreateDelphiCompilerDefinesTemplate(
  DelphiVersion: integer; Owner: TObject): TDefineTemplate;
var
  DefTempl: TDefineTemplate;
  VerMacro: String;
begin
  DefTempl:=TDefineTemplate.Create('Delphi'+IntToStr(DelphiVersion)
      +' Compiler Defines',
      Format(ctsOtherCompilerDefines,['Delphi'+IntToStr(DelphiVersion)]),
      '','',da_Block);
  DefTempl.AddChild(TDefineTemplate.Create('Reset',
      ctsResetAllDefines,
      '','',da_UndefineAll));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro DELPHI',
      Format(ctsDefineMacroName,['DELPHI']),
      'DELPHI','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro FPC_DELPHI',
      Format(ctsDefineMacroName,['FPC_DELPHI']),
      'FPC_DELPHI','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro MSWINDOWS',
      Format(ctsDefineMacroName,['MSWINDOWS']),
      'MSWINDOWS','',da_DefineRecurse));

  // version
  case DelphiVersion of
  3: VerMacro:='VER_110';
  4: VerMacro:='VER_125';
  5: VerMacro:='VER_130';
  6: VerMacro:='VER_140';
  else
    // else define Delphi 7
    VerMacro:='VER_150';
  end;
  DefTempl.AddChild(TDefineTemplate.Create('Define macro '+VerMacro,
      Format(ctsDefineMacroName,[VerMacro]),
      VerMacro,'',da_DefineRecurse));

  DefTempl.AddChild(TDefineTemplate.Create(
     Format(ctsDefineMacroName,[ExternalMacroStart+'Compiler']),
     'Define '+ExternalMacroStart+'Compiler variable',
     ExternalMacroStart+'Compiler','DELPHI',da_DefineRecurse));

  Result:=DefTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateDelphiDirectoryTemplate(
  const DelphiDirectory: string; DelphiVersion: integer;
  Owner: TObject): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Delphi'+IntToStr(DelphiVersion)
     +' Directory',
     Format(ctsNamedDirectory,['Delphi'+IntToStr(DelphiVersion)]),
     '',DelphiDirectory,da_Directory);
  MainDirTempl.AddChild(CreateDelphiCompilerDefinesTemplate(DelphiVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsSetsSrcPathTo,['RTL, VCL']),
      ExternalMacroStart+'SrcPath',
      SetDirSeparators(CreateDelphiSrcPath(DelphiVersion,'$(#DefinePath)/')
                       +'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateDelphiProjectTemplate(
  const ProjectDir, DelphiDirectory: string;
  DelphiVersion: integer; Owner: TObject): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Delphi'+IntToStr(DelphiVersion)+' Project',
     Format(ctsNamedProject,['Delphi'+IntToStr(DelphiVersion)]),
     '',ProjectDir,da_Directory);
  MainDirTempl.AddChild(
    CreateDelphiCompilerDefinesTemplate(DelphiVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create(
     'Define '+ExternalMacroStart+'DelphiDir',
     Format(ctsDefineMacroName,[ExternalMacroStart+'DelphiDir']),
     ExternalMacroStart+'DelphiDir',DelphiDirectory,da_DefineRecurse));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsAddsDirToSourcePath,['Delphi RTL+VCL']),
      ExternalMacroStart+'SrcPath',
      SetDirSeparators(CreateDelphiSrcPath(DelphiVersion,'$(#DelphiDir)/')
                       +'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateKylixCompilerDefinesTemplate(KylixVersion: integer;
  Owner: TObject): TDefineTemplate;
var
  DefTempl: TDefineTemplate;
begin
  DefTempl:=TDefineTemplate.Create('Kylix'+IntToStr(KylixVersion)
      +' Compiler Defines',
      Format(ctsOtherCompilerDefines,['Kylix'+IntToStr(KylixVersion)]),
      '','',da_Block);
  DefTempl.AddChild(TDefineTemplate.Create('Reset',
      ctsResetAllDefines,
      '','',da_UndefineAll));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro KYLIX',
      Format(ctsDefineMacroName,['KYLIX']),
      'KYLIX','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro FPC_DELPHI',
      Format(ctsDefineMacroName,['FPC_DELPHI']),
      'FPC_DELPHI','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro LINUX',
      Format(ctsDefineMacroName,['LINUX']),
      'LINUX','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro CPU386',
      Format(ctsDefineMacroName,['CPU386']),
      'CPU386','',da_DefineRecurse));

  // version
  case KylixVersion of
  1:
    DefTempl.AddChild(TDefineTemplate.Create('Define macro VER_125',
        Format(ctsDefineMacroName,['VER_125']),
        'VER_125','',da_DefineRecurse));
  2:
    DefTempl.AddChild(TDefineTemplate.Create('Define macro VER_130',
        Format(ctsDefineMacroName,['VER_130']),
        'VER_130','',da_DefineRecurse));
  else
    // else define Kylix 3
    DefTempl.AddChild(TDefineTemplate.Create('Define macro VER_140',
        Format(ctsDefineMacroName,['VER_140']),
        'VER_140','',da_DefineRecurse));
  end;

  DefTempl.AddChild(TDefineTemplate.Create(
     Format(ctsDefineMacroName,[ExternalMacroStart+'Compiler']),
     'Define '+ExternalMacroStart+'Compiler variable',
     ExternalMacroStart+'Compiler','DELPHI',da_DefineRecurse));

  Result:=DefTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateKylixSrcPath(KylixVersion: integer;
  const PathPrefix: string): string;
begin
  Result:=PathPrefix+'source/rtl/linux;'
    +PathPrefix+'source/rtl/sys;'
    +PathPrefix+'source/rtl/common;'
    +PathPrefix+'source/rtl/corba40;'
    +PathPrefix+'source/rtle;'
    +PathPrefix+'source/rtl/clx';
end;

function TDefinePool.CreateKylixDirectoryTemplate(const KylixDirectory: string;
  KylixVersion: integer; Owner: TObject): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Kylix'+IntToStr(KylixVersion)
     +' Directory',
     Format(ctsNamedDirectory,['Kylix'+IntToStr(KylixVersion)]),
     '',KylixDirectory,da_Directory);
  MainDirTempl.AddChild(CreateKylixCompilerDefinesTemplate(KylixVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsSetsSrcPathTo,['RTL, CLX']),
      ExternalMacroStart+'SrcPath',
      SetDirSeparators(CreateKylixSrcPath(KylixVersion,'$(#DefinePath)/')
                       +'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateKylixProjectTemplate(const ProjectDir,
  KylixDirectory: string; KylixVersion: integer; Owner: TObject
  ): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Kylix'+IntToStr(KylixVersion)+' Project',
     Format(ctsNamedProject,['Kylix'+IntToStr(KylixVersion)]),
     '',ProjectDir,da_Directory);
  MainDirTempl.AddChild(
    CreateDelphiCompilerDefinesTemplate(KylixVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create(
     'Define '+ExternalMacroStart+'KylixDir',
     Format(ctsDefineMacroName,[ExternalMacroStart+'KylixDir']),
     ExternalMacroStart+'KylixDir',KylixDirectory,da_DefineRecurse));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsAddsDirToSourcePath,['Kylix RTL+VCL']),
      ExternalMacroStart+'SrcPath',
      SetDirSeparators(CreateKylixSrcPath(KylixVersion,'$(#KylixDir)/')
                       +'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateFPCCommandLineDefines(const Name, CmdLine: string;
  RecursiveDefines: boolean; Owner: TObject): TDefineTemplate;
  
  function ReadNextParam(LastEndPos: integer;
    var StartPos, EndPos: integer): boolean;
  begin
    StartPos:=LastEndPos;
    while (StartPos<=length(CmdLine)) and (CmdLine[StartPos] in [' ',#9]) do
      inc(StartPos);
    EndPos:=StartPos;
    while (EndPos<=length(CmdLine)) and (not (CmdLine[EndPos] in [' ',#9])) do
      inc(EndPos);
    Result:=StartPos<=length(CmdLine);
  end;
  
  procedure AddDefine(const AName, ADescription, AVariable, AValue: string;
    AnAction: TDefineAction);
  var
    NewTempl: TDefineTemplate;
  begin
    if AName='' then exit;
    NewTempl:=TDefineTemplate.Create(AName, ADescription, AVariable, AValue,
                                     AnAction);
    if Result=nil then
      Result:=TDefineTemplate.Create(Name,ctsCommandLineParameters,'','',
                                     da_Block);
    Result.AddChild(NewTempl);
  end;
  
var
  StartPos, EndPos: Integer;
  s: string;
  NewAction: TDefineAction;
begin
  Result:=nil;
  EndPos:=1;
  while ReadNextParam(EndPos,StartPos,EndPos) do begin
    if (StartPos<length(CmdLine)) and (CmdLine[StartPos]='-') then begin
      // a parameter
      case CmdLine[StartPos+1] of

      'd':
        begin
          // define
          s:=copy(CmdLine,StartPos+2,EndPos-StartPos-2);
          if RecursiveDefines then
            NewAction:=da_DefineRecurse
          else
            NewAction:=da_Define;
          AddDefine('Define '+s,ctsDefine+s,s,'',NewAction);
        end;

      end;
    end;
  end;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.ConsistencyCheck: integer;
var i: integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i].ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,100);  exit;
    end;
  end;
  Result:=0;
end;

procedure TDefinePool.WriteDebugReport;
var i: integer;
begin
  DebugLn('TDefinePool.WriteDebugReport Consistency=',dbgs(ConsistencyCheck));
  for i:=0 to Count-1 do begin
    Items[i].WriteDebugReport(false);
  end;
end;


end.


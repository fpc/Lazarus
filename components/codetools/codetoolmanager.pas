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
    TCodeToolManager gathers all tools in one single Object
    to easily use the code tools in a program.

}
unit CodeToolManager;

{$ifdef fpc}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{.$DEFINE CTDEBUG}
{ $DEFINE DoNotHandleFindDeclException}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, BasicCodeTools, CodeToolsStrConsts,
  EventCodeTool, CodeTree, CodeAtom, SourceChanger, DefineTemplates, CodeCache,
  ExprEval, LinkScanner, KeywordFuncLists, TypInfo,
  DirectoryCacher, AVL_Tree, LFMTrees, DirectivesTree, PascalParserTool,
  CodeToolsConfig, CustomCodeTool, FindDeclarationTool, IdentCompletionTool,
  StdCodeTools, ResourceCodeTool, CodeToolsStructs, CodeTemplatesTool,
  ExtractProcTool;

type
  TCodeToolManager = class;
  TCodeTool = TEventsCodeTool;
  TDirectivesTool = TCompilerDirectivesTree;

  TGetStringProc = procedure(const s: string) of object;
  TOnBeforeApplyChanges = procedure(Manager: TCodeToolManager;
                                    var Abort: boolean) of object;
  TOnAfterApplyChanges = procedure(Manager: TCodeToolManager) of object;
  TOnGatherExternalChanges = procedure(Manager: TCodeToolManager;
                                       var Abort: boolean) of object;
  TOnSearchUsedUnit = function(const SrcFilename: string;
                               const TheUnitName, TheUnitInFilename: string
                               ): TCodeBuffer of object;
  TOnCodeToolCheckAbort = function: boolean of object;
  TOnFindDefineProperty = procedure(Sender: TObject;
    const PersistentClassName, AncestorClassName, Identifier: string;
    var IsDefined: boolean) of object;
    
  ECodeToolManagerError = class(Exception);

  { TCodeToolManager }

  TCodeToolManager = class
  private
    FAbortable: boolean;
    FAddInheritedCodeToOverrideMethod: boolean;
    FAdjustTopLineDueToComment: boolean;
    FCatchExceptions: boolean;
    FChangeStep: integer;
    FCheckFilesOnDisk: boolean;
    FCompleteProperties: boolean;
    FCurCodeTool: TCodeTool; // current codetool
    FCurDirectivesTool: TDirectivesTool;
    FCursorBeyondEOL: boolean;
    FDirectivesTools: TAVLTree; // tree of TDirectivesTool sorted for Code (TCodeBuffer)
    FErrorCode: TCodeBuffer;
    FErrorColumn: integer;
    FErrorLine: integer;
    FErrorMsg: string;
    FErrorTopLine: integer;
    FIndentSize: integer;
    FJumpCentered: boolean;
    FOnAfterApplyChanges: TOnAfterApplyChanges;
    FOnBeforeApplyChanges: TOnBeforeApplyChanges;
    FOnCheckAbort: TOnCodeToolCheckAbort;
    FOnGatherExternalChanges: TOnGatherExternalChanges;
    FOnFindDefinePropertyForContext: TOnFindDefinePropertyForContext;
    FOnFindDefineProperty: TOnFindDefineProperty;
    FOnGetMethodName: TOnGetMethodname;
    FOnSearchUsedUnit: TOnSearchUsedUnit;
    FResourceTool: TResourceCodeTool;
    FSetPropertyVariablename: string;
    FSourceExtensions: string; // default is '.pp;.pas;.lpr;.dpr;.dpk'
    FSourceTools: TAVLTree; // tree of TCustomCodeTool sorted TCustomCodeTool(Data).Scanner.MainCode
    FTabWidth: integer;
    FVisibleEditorLines: integer;
    FWriteExceptions: boolean;
    FWriteLockCount: integer;// Set/Unset counter
    FWriteLockStep: integer; // current write lock ID
    function OnScannerGetInitValues(Code: Pointer;
      out AChangeStep: integer): TExpressionEvaluator;
    procedure OnDefineTreeReadValue(Sender: TObject; const VariableName: string;
                                    var Value: string; var Handled: boolean);
    procedure OnGlobalValuesChanged;
    function DoOnFindUsedUnit(SrcTool: TFindDeclarationTool; const TheUnitName,
          TheUnitInFilename: string): TCodeBuffer;
    function DoOnGetSrcPathForCompiledUnit(Sender: TObject;
          const AFilename: string): string;
    function OnInternalGetMethodName(const AMethod: TMethod;
                                     CheckOwner: TObject): shortstring;
    function FindCodeOfMainUnitHint(Code: TCodeBuffer): TCodeBuffer;
    procedure CreateScanner(Code: TCodeBuffer);
    procedure SetAbortable(const AValue: boolean);
    procedure SetAddInheritedCodeToOverrideMethod(const AValue: boolean);
    procedure SetCheckFilesOnDisk(NewValue: boolean);
    procedure SetCompleteProperties(const AValue: boolean);
    procedure SetIndentSize(NewValue: integer);
    procedure SetTabWidth(const AValue: integer);
    procedure SetVisibleEditorLines(NewValue: integer);
    procedure SetJumpCentered(NewValue: boolean);
    procedure SetCursorBeyondEOL(NewValue: boolean);
    procedure BeforeApplyingChanges(var Abort: boolean);
    procedure AfterApplyingChanges;
    procedure AdjustErrorTopLine;
    procedure WriteError;
    function OnGetCodeToolForBuffer(Sender: TObject;
      Code: TCodeBuffer; GoToMainCode: boolean): TFindDeclarationTool;
    function OnGetDirectoryCache(const ADirectory: string): TCTDirectoryCache;
    procedure OnToolSetWriteLock(Lock: boolean);
    procedure OnToolGetWriteLockInfo(out WriteLockIsSet: boolean;
      out WriteLockStep: integer);
    function OnParserProgress(Tool: TCustomCodeTool): boolean;
    function OnScannerProgress(Sender: TLinkScanner): boolean;
    function GetResourceTool: TResourceCodeTool;
    function GetOwnerForCodeTreeNode(ANode: TCodeTreeNode): TObject;
    function DirectoryCachePoolGetString(const ADirectory: string;
                                  const AStringType: TCTDirCacheString): string;
    function DirectoryCachePoolFindVirtualFile(const Filename: string): string;
  public
    DefinePool: TDefinePool; // definition templates (rules)
    DefineTree: TDefineTree; // cache for defines (e.g. initial compiler values)
    SourceCache: TCodeCache; // cache for source (units, include files, ...)
    SourceChangeCache: TSourceChangeCache; // cache for write accesses
    GlobalValues: TExpressionEvaluator;
    DirectoryCachePool: TCTDirectoryCachePool;
    IdentifierList: TIdentifierList;
    IdentifierHistory: TIdentifierHistoryList;
    Positions: TCodeXYPositions;

    constructor Create;
    destructor Destroy; override;
    
    procedure Init(Config: TCodeToolsOptions);
    
    procedure ActivateWriteLock;
    procedure DeactivateWriteLock;
    property ChangeStep: integer read FChangeStep;
    procedure IncreaseChangeStep;

    // file handling
    property SourceExtensions: string
                                 read FSourceExtensions write FSourceExtensions;
    function FindFile(const ExpandedFilename: string): TCodeBuffer;
    function LoadFile(const ExpandedFilename: string;
                      UpdateFromDisk, Revert: boolean): TCodeBuffer;
    function CreateFile(const AFilename: string): TCodeBuffer;
    function CreateTempFile(const AFilename: string): TCodeBuffer;
    procedure ReleaseTempFile(Buffer: TCodeBuffer);
    function SaveBufferAs(OldBuffer: TCodeBuffer;const ExpandedFilename: string;
                          out NewBuffer: TCodeBuffer): boolean;
    function FilenameHasSourceExt(const AFilename: string): boolean;
    function GetMainCode(Code: TCodeBuffer): TCodeBuffer;
    function GetIncludeCodeChain(Code: TCodeBuffer;
                                 RemoveFirstCodesWithoutTool: boolean;
                                 out ListOfCodeBuffer: TFPList): boolean;
    property OnSearchUsedUnit: TOnSearchUsedUnit
                                 read FOnSearchUsedUnit write FOnSearchUsedUnit;

    // initializing single codetools
    function FindCodeToolForSource(Code: TCodeBuffer): TCustomCodeTool;
    property CurCodeTool: TCodeTool read FCurCodeTool;
    procedure ClearCurCodeTool;
    function InitCurCodeTool(Code: TCodeBuffer): boolean;
    function InitResourceTool: boolean;
    procedure ClearPositions;
    function GetCodeToolForSource(Code: TCodeBuffer;
                      GoToMainCode, ExceptionOnError: boolean): TCustomCodeTool;
      
    // initializing single compilerdirectivestrees
    function FindDirectivesToolForSource(Code: TCodeBuffer
                                        ): TDirectivesTool;
    property CurDirectivesTool: TDirectivesTool read FCurDirectivesTool;
    procedure ClearCurDirectivesTool;
    function InitCurDirectivesTool(Code: TCodeBuffer): boolean;
    function GetDirectivesToolForSource(Code: TCodeBuffer;
                            ExceptionOnError: boolean): TCompilerDirectivesTree;

    // exception handling
    procedure ClearError;
    function HandleException(AnException: Exception): boolean;
    procedure SetError(Code: TCodeBuffer; Line, Column: integer;
                       const TheMessage: string);
    property CatchExceptions: boolean
                                   read FCatchExceptions write FCatchExceptions;
    property WriteExceptions: boolean
                                   read FWriteExceptions write FWriteExceptions;
    property ErrorCode: TCodeBuffer read fErrorCode;
    property ErrorColumn: integer read fErrorColumn;
    property ErrorLine: integer read fErrorLine;
    property ErrorMessage: string read fErrorMsg;
    property ErrorTopLine: integer read fErrorTopLine;
    property Abortable: boolean read FAbortable write SetAbortable;
    property OnCheckAbort: TOnCodeToolCheckAbort
                                         read FOnCheckAbort write FOnCheckAbort;

    // tool settings
    property AdjustTopLineDueToComment: boolean read FAdjustTopLineDueToComment
                                               write FAdjustTopLineDueToComment;
    property CheckFilesOnDisk: boolean read FCheckFilesOnDisk
                                       write SetCheckFilesOnDisk;
    property CursorBeyondEOL: boolean read FCursorBeyondEOL
                                      write SetCursorBeyondEOL;
    property IndentSize: integer read FIndentSize write SetIndentSize;
    property JumpCentered: boolean read FJumpCentered write SetJumpCentered;
    property SetPropertyVariablename: string
                   read FSetPropertyVariablename write FSetPropertyVariablename;
    property VisibleEditorLines: integer
                           read FVisibleEditorLines write SetVisibleEditorLines;
    property TabWidth: integer read FTabWidth write SetTabWidth;
    property CompleteProperties: boolean
                           read FCompleteProperties write SetCompleteProperties;
    property AddInheritedCodeToOverrideMethod: boolean
                                      read FAddInheritedCodeToOverrideMethod
                                      write SetAddInheritedCodeToOverrideMethod;

    // source changing
    procedure BeginUpdate;
    procedure EndUpdate;
    function GatherExternalChanges: boolean;
    property OnGatherExternalChanges: TOnGatherExternalChanges
                   read FOnGatherExternalChanges write FOnGatherExternalChanges;
    function ApplyChanges: boolean;
    property OnBeforeApplyChanges: TOnBeforeApplyChanges
                         read FOnBeforeApplyChanges write FOnBeforeApplyChanges;
    property OnAfterApplyChanges: TOnAfterApplyChanges
                           read FOnAfterApplyChanges write FOnAfterApplyChanges;
          
    // defines
    function SetGlobalValue(const VariableName, VariableValue: string): boolean;
    function GetUnitPathForDirectory(const Directory: string;
                                     UseCache: boolean = true): string;
    function GetIncludePathForDirectory(const Directory: string;
                                        UseCache: boolean = true): string;
    function GetSrcPathForDirectory(const Directory: string;
                                    UseCache: boolean = true): string;
    function GetCompleteSrcPathForDirectory(const Directory: string;
                                            UseCache: boolean = true): string;
    function GetPPUSrcPathForDirectory(const Directory: string): string;
    function GetPPWSrcPathForDirectory(const Directory: string): string;
    function GetDCUSrcPathForDirectory(const Directory: string): string;
    function GetCompiledSrcPathForDirectory(const Directory: string;
                                            UseCache: boolean = true): string;
    function GetNestedCommentsFlagForFile(const Filename: string): boolean;
    function GetPascalCompilerForDirectory(const Directory: string): TPascalCompiler;
    function GetCompilerModeForDirectory(const Directory: string): TCompilerMode;
    function GetCompiledSrcExtForDirectory(const Directory: string): string;
    function FindUnitInUnitLinks(const Directory, UnitName: string): string;
    function GetUnitLinksForDirectory(const Directory: string;
                                      UseCache: boolean = false): string;
    function GetFPCUnitPathForDirectory(const Directory: string;
                                        UseCache: boolean = false): string;
    procedure GetFPCVersionForDirectory(const Directory: string;
                                 out FPCVersion, FPCRelease, FPCPatch: integer);

    // miscellaneous
    property OnGetMethodName: TOnGetMethodname read FOnGetMethodName
                                               write FOnGetMethodName;

    // data function
    procedure FreeListOfPCodeXYPosition(var List: TFPList);
    procedure FreeTreeOfPCodeXYPosition(var Tree: TAVLTree);
    function CreateTreeOfPCodeXYPosition: TAVLTree;
    procedure AddListToTreeOfPCodeXYPosition(SrcList: TFPList;
          DestTree: TAVLTree; ClearList, CreateCopies: boolean);

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    // code exploring
    function Explore(Code: TCodeBuffer; out ACodeTool: TCodeTool;
          WithStatements: boolean; OnlyInterface: boolean = false): boolean;
    function CheckSyntax(Code: TCodeBuffer; out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer; out ErrorMsg: string): boolean;
    function ExploreDirectives(Code: TCodeBuffer;
          out ADirectivesTool: TDirectivesTool): boolean;

    // compiler directives
    function GuessMisplacedIfdefEndif(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    // find include directive of include file at position X,Y
    function FindEnclosingIncludeDirective(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function FindResourceDirective(Code: TCodeBuffer; StartX, StartY: integer;
          out NewCode: TCodeBuffer; out NewX, NewY, NewTopLine: integer;
          const Filename: string = ''; SearchInCleanSrc: boolean = true): boolean;
    function AddResourceDirective(Code: TCodeBuffer; const Filename: string;
          SearchInCleanSrc: boolean = true; const NewSrc: string = ''): boolean;
    function RemoveDirective(Code: TCodeBuffer; NewX, NewY: integer;
          RemoveEmptyIFs: boolean): boolean;
    function FixIncludeFilenames(Code: TCodeBuffer; Recursive: boolean;
          out MissingIncludeFilesCodeXYPos: TFPList): boolean;
    function FixMissingH2PasDirectives(Code: TCodeBuffer;
          var Changed: boolean): boolean;
    function ReduceCompilerDirectives(Code: TCodeBuffer;
          Undefines, Defines: TStrings; var Changed: boolean): boolean;

    // keywords and comments
    function IsKeyword(Code: TCodeBuffer; const KeyWord: string): boolean;
    function ExtractCodeWithoutComments(Code: TCodeBuffer): string;

    // blocks (e.g. begin..end, case..end, try..finally..end, repeat..until)
    function FindBlockCounterPart(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function FindBlockStart(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function GuessUnclosedBlock(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
          
    // method jumping
    function JumpToMethod(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer;
          var RevertableJump: boolean): boolean;

    // find declaration
    function FindDeclaration(Code: TCodeBuffer; X,Y: integer;
          out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer): boolean;
    function FindSmartHint(Code: TCodeBuffer; X,Y: integer): string;
    function FindDeclarationInInterface(Code: TCodeBuffer;
          const Identifier: string; out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer): boolean;
    function FindDeclarationWithMainUsesSection(Code: TCodeBuffer;
          const Identifier: string;
          out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer): Boolean;
    function FindDeclarationAndOverload(Code: TCodeBuffer; X,Y: integer;
          var ListOfPCodeXYPosition: TFPList;
          Flags: TFindDeclarationListFlags): boolean;
    function FindMainDeclaration(Code: TCodeBuffer; X,Y: integer;
          out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer): boolean;
    function FindDeclarationOfPropertyPath(Code: TCodeBuffer;
          const PropertyPath: string; out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer): Boolean;

    // get code context
    function FindCodeContext(Code: TCodeBuffer; X,Y: integer;
          out CodeContexts: TCodeContextInfo): boolean;
    function ExtractProcedureHeader(Code: TCodeBuffer; X,Y: integer;
          Attributes: TProcHeadAttributes; var ProcHead: string): boolean;

    // gather identifiers (i.e. all visible)
    function GatherIdentifiers(Code: TCodeBuffer; X,Y: integer): boolean;
    function GetIdentifierAt(Code: TCodeBuffer; X,Y: integer;
          var Identifier: string): boolean;
    function IdentItemCheckHasChilds(IdentItem: TIdentifierListItem): boolean;
    
    // rename identifier
    function FindReferences(IdentifierCode: TCodeBuffer;
          X, Y: integer; TargetCode: TCodeBuffer; SkipComments: boolean;
          var ListOfPCodeXYPosition: TFPList): boolean;
    function RenameIdentifier(TreeOfPCodeXYPosition: TAVLTree;
          const OldIdentifier, NewIdentifier: string): boolean;
    function ReplaceWord(Code: TCodeBuffer; const OldWord, NewWord: string;
          ChangeStrings: boolean): boolean;

    // resourcestring sections
    function GatherResourceStringSections(
          Code: TCodeBuffer; X,Y: integer;
          CodePositions: TCodeXYPositions): boolean;
    function IdentifierExistsInResourceStringSection(Code: TCodeBuffer;
          X,Y: integer; const ResStrIdentifier: string): boolean;
    function CreateIdentifierFromStringConst(
          StartCode: TCodeBuffer; StartX, StartY: integer;
          EndCode: TCodeBuffer;   EndX, EndY: integer;
          out Identifier: string; MaxLen: integer): boolean;
    function StringConstToFormatString(
          StartCode: TCodeBuffer; StartX, StartY: integer;
          EndCode: TCodeBuffer;   EndX, EndY: integer;
          out FormatStringConstant, FormatParameters: string;
          out StartInStringConst, EndInStringConst: boolean): boolean;
    function GatherResourceStringsWithValue(SectionCode: TCodeBuffer;
          SectionX, SectionY: integer; const StringValue: string;
          CodePositions: TCodeXYPositions): boolean;
    function AddResourcestring(CursorCode: TCodeBuffer; X,Y: integer;
          SectionCode: TCodeBuffer; SectionX, SectionY: integer;
          const NewIdentifier, NewValue: string;
          InsertPolicy: TResourcestringInsertPolicy): boolean;
    procedure ImproveStringConstantStart(const ACode: string;
                                         var StartPos: integer);
    procedure ImproveStringConstantEnd(const ACode: string;
                                       var EndPos: integer);

    // expressions
    function GetStringConstBounds(Code: TCodeBuffer; X,Y: integer;
          var StartCode: TCodeBuffer; var StartX, StartY: integer;
          var EndCode: TCodeBuffer; var EndX, EndY: integer;
          ResolveComments: boolean): boolean;
    function ReplaceCode(Code: TCodeBuffer; StartX, StartY: integer;
          EndX, EndY: integer; const NewCode: string): boolean;

    // code completion = auto class completion, auto forward proc completion,
    //             local var assignment completion, event assignment completion
    function CompleteCode(Code: TCodeBuffer; X,Y,TopLine: integer;
          out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer): boolean;
    function FindRedefinitions(Code: TCodeBuffer;
          out TreeOfCodeTreeNodeExt: TAVLTree; WithEnums: boolean): boolean;
    function RemoveRedefinitions(Code: TCodeBuffer;
          TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function RemoveAllRedefinitions(Code: TCodeBuffer): boolean;
    function FindAliasDefinitions(Code: TCodeBuffer;
          out TreeOfCodeTreeNodeExt: TAVLTree; OnlyWrongType: boolean): boolean;
    function FixAliasDefinitions(Code: TCodeBuffer;
          TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function FixAllAliasDefinitions(Code: TCodeBuffer): boolean;
    function FindConstFunctions(Code: TCodeBuffer;
                                out TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function ReplaceConstFunctions(Code: TCodeBuffer;
                                   TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function ReplaceAllConstFunctions(Code: TCodeBuffer): boolean;
    function FindTypeCastFunctions(Code: TCodeBuffer;
                                  out TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function ReplaceTypeCastFunctions(Code: TCodeBuffer;
                                      TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function ReplaceAllTypeCastFunctions(Code: TCodeBuffer): boolean;
    function FixForwardDefinitions(Code: TCodeBuffer): boolean;

    // custom class completion
    function InitClassCompletion(Code: TCodeBuffer;
                const UpperClassName: string; out CodeTool: TCodeTool): boolean;

    // extract proc (creates a new procedure from code in selection)
    function CheckExtractProc(Code: TCodeBuffer;
          const StartPoint, EndPoint: TPoint;
          var MethodPossible, SubProcSameLvlPossible: boolean): boolean;
    function ExtractProc(Code: TCodeBuffer; const StartPoint, EndPoint: TPoint;
          ProcType: TExtractProcType; const ProcName: string;
          var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer
          ): boolean;

    // code templates
    function InsertCodeTemplate(Code: TCodeBuffer;
          SelectionStart, SelectionEnd: TPoint;
          TopLine: integer;
          CodeTemplate: TCodeToolTemplate;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;

    // source name  e.g. 'unit UnitName;'
    function GetSourceName(Code: TCodeBuffer; SearchMainCode: boolean): string;
    function GetCachedSourceName(Code: TCodeBuffer): string;
    function RenameSource(Code: TCodeBuffer; const NewName: string): boolean;
    function GetSourceType(Code: TCodeBuffer; SearchMainCode: boolean): string;

    // uses sections
    function FindUnitInAllUsesSections(Code: TCodeBuffer;
          const AnUnitName: string;
          out NamePos, InPos: integer): boolean;
    function RenameUsedUnit(Code: TCodeBuffer;
          const OldUnitName, NewUnitName, NewUnitInFile: string): boolean;
    function ReplaceUsedUnits(Code: TCodeBuffer;
          UnitNamePairs: TStringToStringTree): boolean;
    function AddUnitToMainUsesSection(Code: TCodeBuffer;
          const NewUnitName, NewUnitInFile: string): boolean;
    function RemoveUnitFromAllUsesSections(Code: TCodeBuffer;
          const AnUnitName: string): boolean;
    function FindUsedUnitFiles(Code: TCodeBuffer; var MainUsesSection: TStrings
          ): boolean;
    function FindUsedUnitFiles(Code: TCodeBuffer; var MainUsesSection,
          ImplementationUsesSection: TStrings): boolean;
    function FindUsedUnitNames(Code: TCodeBuffer; var MainUsesSection,
          ImplementationUsesSection: TStrings): boolean;
    function FindMissingUnits(Code: TCodeBuffer; var MissingUnits: TStrings;
          FixCase: boolean = false; SearchImplementation: boolean = true): boolean;
    function FindDelphiProjectUnits(Code: TCodeBuffer;
          var FoundInUnits, MissingInUnits, NormalUnits: TStrings): boolean;
    function FindDelphiPackageUnits(Code: TCodeBuffer;
          var FoundInUnits, MissingInUnits, NormalUnits: TStrings): boolean;
    function CommentUnitsInUsesSections(Code: TCodeBuffer;
          MissingUnits: TStrings): boolean;
    function FindUnitCaseInsensitive(Code: TCodeBuffer;
                              var AnUnitName, AnUnitInFilename: string): string;
    function FindUnitSource(Code: TCodeBuffer;
                       const AnUnitName, AnUnitInFilename: string): TCodeBuffer;

    // resources
    property OnFindDefinePropertyForContext: TOnFindDefinePropertyForContext
                                          read FOnFindDefinePropertyForContext
                                          write FOnFindDefinePropertyForContext;
    property OnFindDefineProperty: TOnFindDefineProperty
                                           read FOnFindDefineProperty
                                           write FOnFindDefineProperty;
    function FindLFMFileName(Code: TCodeBuffer): string;
    function CheckLFM(UnitCode, LFMBuf: TCodeBuffer;
          var LFMTree: TLFMTree;
          RootMustBeClassInIntf, ObjectsMustExists: boolean): boolean;
    function FindNextResourceFile(Code: TCodeBuffer;
          var LinkIndex: integer): TCodeBuffer;
    function AddLazarusResourceHeaderComment(Code: TCodeBuffer;
          const CommentText: string): boolean;
    function FindLazarusResource(Code: TCodeBuffer;
          const ResourceName: string): TAtomPosition;
    function AddLazarusResource(Code: TCodeBuffer;
          const ResourceName, ResourceData: string): boolean;
    function RemoveLazarusResource(Code: TCodeBuffer;
          const ResourceName: string): boolean;
    function RenameMainInclude(Code: TCodeBuffer; const NewFilename: string;
          KeepPath: boolean): boolean;
    function RenameIncludeDirective(Code: TCodeBuffer; LinkIndex: integer;
          const NewFilename: string; KeepPath: boolean): boolean;
    procedure DefaultFindDefinePropertyForContext(Sender: TObject;
                       const ClassContext, AncestorClassContext: TFindContext;
                       LFMNode: TLFMTreeNode;
                       const IdentName: string; var IsDefined: boolean);

    // register proc
    function HasInterfaceRegisterProc(Code: TCodeBuffer;
          var HasRegisterProc: boolean): boolean;
          
    // Delphi to Lazarus conversion
    function ConvertDelphiToLazarusSource(Code: TCodeBuffer;
          AddLRSCode: boolean): boolean;
          
    // Application.Createform(ClassName,VarName) statements in program source
    function FindCreateFormStatement(Code: TCodeBuffer; StartPos: integer;
          const AClassName, AVarName: string;
          out Position: integer): integer; // 0=found, -1=not found, 1=found, but wrong classname
    function AddCreateFormStatement(Code: TCodeBuffer;
          const AClassName, AVarName: string): boolean;
    function RemoveCreateFormStatement(Code: TCodeBuffer;
          const AVarName: string): boolean;
    function ChangeCreateFormStatement(Code: TCodeBuffer;
          const OldClassName, OldVarName: string;
          const NewClassName, NewVarName: string;
          OnlyIfExists: boolean): boolean;
    function ListAllCreateFormStatements(Code: TCodeBuffer): TStrings;
    function SetAllCreateFromStatements(Code: TCodeBuffer; 
          List: TStrings): boolean;
          
    // Application.Title:= statements in program source
    function GetApplicationTitleStatement(Code: TCodeBuffer;
          var Title: string): boolean;
    function SetApplicationTitleStatement(Code: TCodeBuffer;
          const NewTitle: string): boolean;
    function RemoveApplicationTitleStatement(Code: TCodeBuffer): boolean;

    // forms
    function RenameForm(Code: TCodeBuffer;
      const OldFormName, OldFormClassName: string;
      const NewFormName, NewFormClassName: string): boolean;
    function FindFormAncestor(Code: TCodeBuffer; const FormClassName: string;
      var AncestorClassName: string; DirtySearch: boolean): boolean;

    // form components
    function CompleteComponent(Code: TCodeBuffer;
          AComponent, AncestorComponent: TComponent): boolean;
    function PublishedVariableExists(Code: TCodeBuffer;
          const AClassName, AVarName: string;
          ErrorOnClassNotFound: boolean): boolean;
    function AddPublishedVariable(Code: TCodeBuffer;
          const AClassName,VarName, VarType: string): boolean;
    function RemovePublishedVariable(Code: TCodeBuffer;
          const AClassName, AVarName: string;
          ErrorOnClassNotFound: boolean): boolean;
    function RenamePublishedVariable(Code: TCodeBuffer;
          const AClassName, OldVariableName, NewVarName,
          VarType: shortstring; ErrorOnClassNotFound: boolean): boolean;
    function FindDanglingComponentEvents(Code: TCodeBuffer;
          const AClassName: string;
          RootComponent: TComponent; ExceptionOnClassNotFound,
          SearchInAncestors: boolean;
          out ListOfPInstancePropInfo: TFPList): boolean;

    // functions for events in the object inspector
    function GetCompatiblePublishedMethods(Code: TCodeBuffer;
          const AClassName: string; TypeData: PTypeData;
          Proc: TGetStringProc): boolean;
    function PublishedMethodExists(Code:TCodeBuffer; const AClassName,
          AMethodName: string; TypeData: PTypeData;
          out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
          ): boolean;
    function JumpToPublishedMethodBody(Code: TCodeBuffer;
          const AClassName, AMethodName: string;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function RenamePublishedMethod(Code: TCodeBuffer;
          const AClassName, OldMethodName,
          NewMethodName: string): boolean;
    function CreatePublishedMethod(Code: TCodeBuffer; const AClassName,
          NewMethodName: string; ATypeInfo: PTypeInfo;
          UseTypeInfoForParameters: boolean = false;
          const APropertyUnitName: string = ''; const APropertyPath: string = ''
          ): boolean;

    // private class parts
    function CreatePrivateMethod(Code: TCodeBuffer; const AClassName,
          NewMethodName: string; ATypeInfo: PTypeInfo;
          UseTypeInfoForParameters: boolean = false;
          const APropertyUnitName: string = '';
          const APropertyPath: string = ''): boolean;

    // IDE % directives
    function GetIDEDirectives(Code: TCodeBuffer;
          DirectiveList: TStrings): boolean;
    function SetIDEDirectives(Code: TCodeBuffer;
          DirectiveList: TStrings): boolean;
          
    // linker jumping
    function JumpToLinkerIdentifier(Code: TCodeBuffer;
          const SourceFilename: string; SourceLine: integer;
          const MangledFunction, Identifier: string;
          out NewCode: TCodeBuffer;
          out NewX, NewY, NewTopLine: integer): boolean;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport(WriteTool,
          WriteDefPool, WriteDefTree, WriteCache, WriteGlobalValues: boolean);
  end;


var CodeToolBoss: TCodeToolManager;


function CreateDefinesForFPCMode(const Name: string;
                                 CompilerMode: TCompilerMode): TDefineTemplate;


implementation


function CompareCodeToolMainSources(Data1, Data2: Pointer): integer;
var
  Src1, Src2: Pointer;
begin
  Src1:=TCustomCodeTool(Data1).Scanner.MainCode;
  Src2:=TCustomCodeTool(Data2).Scanner.MainCode;
  if Src1<Src2 then
    Result:=-1
  else if Src1>Src2 then
    Result:=+1
  else
    Result:=0;
end;

function CompareDirectivesTreeSources(Data1, Data2: Pointer): integer;
var
  Src1, Src2: Pointer;
begin
  Src1:=TCompilerDirectivesTree(Data1).Code;
  Src2:=TCompilerDirectivesTree(Data2).Code;
  if Src1<Src2 then
    Result:=-1
  else if Src1>Src2 then
    Result:=+1
  else
    Result:=0;
end;

function GetOwnerForCodeTreeNode(ANode: TCodeTreeNode): TObject;
begin
  Result:=CodeToolBoss.GetOwnerForCodeTreeNode(ANode);
end;

procedure DumpExceptionBackTrace;
var
  FrameCount: integer;
  Frames: PPointer;
  FrameNumber:Integer;
begin
  DebugLn('Codetools  Stack trace:');
  DebugLn(BackTraceStrFunc(ExceptAddr));
  FrameCount:=ExceptFrameCount;
  Frames:=ExceptFrames;
  for FrameNumber := 0 to FrameCount-1 do
    DebugLn(BackTraceStrFunc(Frames[FrameNumber]));
end;

function CreateDefinesForFPCMode(const Name: string; CompilerMode: TCompilerMode
  ): TDefineTemplate;
var
  cm: TCompilerMode;
  NewMode: String;
begin
  Result:=TDefineTemplate.Create(Name,'set FPC compiler mode',
                                 '','',da_Block);
  for cm:=Low(TCompilerMode) to High(TCompilerMode) do begin
    Result.AddChild(TDefineTemplate.Create(CompilerModeVars[cm],
      CompilerModeVars[cm],CompilerModeVars[cm],'',da_Undefine));
  end;
  NewMode:=CompilerModeVars[CompilerMode];
  Result.AddChild(TDefineTemplate.Create(NewMode,
    NewMode,NewMode,'1',da_Define));
end;


{ TCodeToolManager }

constructor TCodeToolManager.Create;
begin
  inherited Create;
  FCheckFilesOnDisk:=true;
  FOnFindDefinePropertyForContext:=@DefaultFindDefinePropertyForContext;
  DefineTree:=TDefineTree.Create;
  DefineTree.OnReadValue:=@OnDefineTreeReadValue;
  DefinePool:=TDefinePool.Create;
  SourceCache:=TCodeCache.Create;
  SourceChangeCache:=TSourceChangeCache.Create;
  SourceChangeCache.OnBeforeApplyChanges:=@BeforeApplyingChanges;
  SourceChangeCache.OnAfterApplyChanges:=@AfterApplyingChanges;
  GlobalValues:=TExpressionEvaluator.Create;
  DirectoryCachePool:=TCTDirectoryCachePool.Create;
  DirectoryCachePool.OnGetString:=@DirectoryCachePoolGetString;
  DirectoryCachePool.OnFindVirtualFile:=@DirectoryCachePoolFindVirtualFile;
  FAddInheritedCodeToOverrideMethod:=true;
  FAdjustTopLineDueToComment:=true;
  FCatchExceptions:=true;
  FCompleteProperties:=true;
  FCursorBeyondEOL:=true;
  FIndentSize:=2;
  FJumpCentered:=true;
  FSourceExtensions:='.pp;.pas;.p;.lpr;.lpk;.dpr;.dpk';
  FVisibleEditorLines:=20;
  FWriteExceptions:=true;
  FSourceTools:=TAVLTree.Create(@CompareCodeToolMainSources);
  FDirectivesTools:=TAVLTree.Create(@CompareDirectivesTreeSources);
  IdentifierList:=TIdentifierList.Create;
  IdentifierHistory:=TIdentifierHistoryList.Create;
  IdentifierList.History:=IdentifierHistory;
end;

destructor TCodeToolManager.Destroy;
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.Destroy] A');
  {$ENDIF}
  FreeAndNil(GlobalValues);
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.Destroy] B');
  {$ENDIF}
  FreeAndNil(Positions);
  FreeAndNil(IdentifierHistory);
  FreeAndNil(IdentifierList);
  FSourceTools.FreeAndClear;
  FreeAndNil(FSourceTools);
  FDirectivesTools.FreeAndClear;
  FreeAndNil(FDirectivesTools);
  FreeAndNil(FResourceTool);
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.Destroy] C');
  {$ENDIF}
  FreeAndNil(DefineTree);
  FreeAndNil(DefinePool);
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.Destroy] D');
  {$ENDIF}
  FreeAndNil(SourceChangeCache);
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.Destroy] E');
  {$ENDIF}
  FreeAndNil(SourceCache);
  FreeAndNil(DirectoryCachePool);
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.Destroy] F');
  {$ENDIF}
  inherited Destroy;
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.Destroy] END');
  {$ENDIF}
  {$IFDEF MEM_CHECK}
  CheckHeap('TCodeToolManager.Destroy END');
  {$ENDIF}
end;

procedure TCodeToolManager.Init(Config: TCodeToolsOptions);
var
  FPCUnitPath, TargetOS, TargetProcessor: string;
  UnitLinkList: String;
  FPCDefines: TDefineTemplate;
  FPCSrcDefines: TDefineTemplate;
  LazarusSrcDefines: TDefineTemplate;
  ATestPascalFile: String;
begin
  // set global values
  with GlobalValues do begin
    Variables[ExternalMacroStart+'LazarusSrcDir']:=Config.LazarusSrcDir;
    Variables[ExternalMacroStart+'FPCSrcDir']:=Config.FPCSrcDir;
    Variables[ExternalMacroStart+'LCLWidgetType']:=Config.LCLWidgetType;
    Variables[ExternalMacroStart+'ProjectDir']:=Config.ProjectDir;
  end;

  // build DefinePool
  with DefinePool do begin
    FPCUnitPath:=Config.FPCUnitPath;
    TargetOS:=Config.TargetOS;
    TargetProcessor:=Config.TargetProcessor;
    ATestPascalFile:=Config.TestPascalFile;
    if ATestPascalFile='' then
      ATestPascalFile:=GetTempFilename('fpctest.pas','');
    FPCDefines:=CreateFPCTemplate(Config.FPCPath, Config.FPCOptions,
                          ATestPascalFile,
                          FPCUnitPath, TargetOS, TargetProcessor,
                          nil);
    if FPCDefines=nil then begin
      raise Exception.Create('TCodeToolManager.Init: Unable to execute '+Config.FPCPath+' to get compiler values');
    end;
    Add(FPCDefines);
    Config.FPCUnitPath:=FPCUnitPath;
    Config.TargetOS:=TargetOS;
    Config.TargetProcessor:=TargetProcessor;
    UnitLinkList:=Config.UnitLinkList;
    FPCSrcDefines:=CreateFPCSrcTemplate(Config.FPCSrcDir,Config.FPCUnitPath,
                          Config.PPUExt,
                          Config.TargetOS, Config.TargetProcessor,
                          Config.UnitLinkListValid,UnitLinkList,
                          nil);
    Add(FPCSrcDefines);
    Config.UnitLinkListValid:=UnitLinkList<>'';
    Config.UnitLinkList:=UnitLinkList;
    LazarusSrcDefines:=CreateLazarusSrcTemplate('$(#LazarusSrcDir)',
                                 '$(#LCLWidgetType)',
                                 Config.LazarusSrcOptions,nil);
    Add(LazarusSrcDefines);
  end;

  // build define tree
  DefineTree.Add(FPCDefines.CreateCopy);
  DefineTree.Add(FPCSrcDefines.CreateCopy);
  DefineTree.Add(LazarusSrcDefines.CreateCopy);
  DefineTree.Add(DefinePool.CreateLCLProjectTemplate(
                 '$(#LazarusSrcDir)','$(#LCLWidgetType)','$(#ProjectDir)',nil));
end;

procedure TCodeToolManager.BeginUpdate;
begin
  SourceChangeCache.BeginUpdate;
end;

procedure TCodeToolManager.EndUpdate;
begin
  SourceChangeCache.EndUpdate;
end;

function TCodeToolManager.GatherExternalChanges: boolean;
var
  Abort: Boolean;
begin
  Result:=true;
  if Assigned(OnGatherExternalChanges) then begin
    Abort:=false;
    OnGatherExternalChanges(Self,Abort);
    Result:=not Abort;
  end;
end;

function TCodeToolManager.FindFile(const ExpandedFilename: string): TCodeBuffer;
begin
  Result:=SourceCache.FindFile(ExpandedFilename);
end;

function TCodeToolManager.LoadFile(const ExpandedFilename: string;
  UpdateFromDisk, Revert: boolean): TCodeBuffer;
begin
  {$IFDEF CTDEBUG}
  DebugLn('>>>>>> [TCodeToolManager.LoadFile] ',ExpandedFilename,' Update=',dbgs(UpdateFromDisk),' Revert=',dbgs(Revert));
  {$ENDIF}
  Result:=SourceCache.LoadFile(ExpandedFilename);
  if Result<>nil then begin
    if Revert then
      Result.Revert
    else if UpdateFromDisk then
      Result.Reload;
  end;
end;

function TCodeToolManager.CreateFile(const AFilename: string): TCodeBuffer;
begin
  Result:=SourceCache.CreateFile(AFilename);
  {$IFDEF CTDEBUG}
  DebugLn('****** TCodeToolManager.CreateFile "',AFilename,'" ',dbgs(Result<>nil));
  {$ENDIF}
end;

function TCodeToolManager.CreateTempFile(const AFilename: string): TCodeBuffer;
var
  i: Integer;
  TempFilename: string;
begin
  i:=1;
  repeat
    TempFilename:=VirtualTempDir+PathDelim+IntToStr(i)+PathDelim+AFilename;
    Result:=FindFile(TempFilename);
    if (Result<>nil) and (Result.ReferenceCount=0) then exit;
    inc(i);
  until Result=nil;
  Result:=SourceCache.CreateFile(TempFilename);
  Result.IncrementRefCount;
end;

procedure TCodeToolManager.ReleaseTempFile(Buffer: TCodeBuffer);
begin
  Buffer.ReleaseRefCount;
end;

function TCodeToolManager.SaveBufferAs(OldBuffer: TCodeBuffer;
  const ExpandedFilename: string; out NewBuffer: TCodeBuffer): boolean;
begin
  Result:=SourceCache.SaveBufferAs(OldBuffer,ExpandedFilename,NewBuffer);
end;

function TCodeToolManager.FilenameHasSourceExt(
  const AFilename: string): boolean;
var i, CurExtStart, CurExtEnd, ExtStart, ExtLen: integer;
begin
  ExtStart:=length(AFilename);
  while (ExtStart>0) and (AFilename[ExtStart]<>'.')
  and (AFilename[ExtStart]<>PathDelim) do
    dec(ExtStart);
  if (ExtStart<1) or (AFilename[ExtStart]<>'.') then begin
    Result:=false;
    exit;
  end;
  ExtLen:=length(AFilename)-ExtStart+1;
  CurExtStart:=1;
  CurExtEnd:=CurExtStart;
  while CurExtEnd<=length(FSourceExtensions)+1 do begin
    if (CurExtEnd>length(FSourceExtensions))
    or (FSourceExtensions[CurExtEnd] in [':',';']) then begin
      // compare current extension with filename-extension
      if ExtLen=CurExtEnd-CurExtStart then begin
        i:=0;
        while (i<ExtLen) 
        and (UpChars[AFilename[i+ExtStart]]
            =UpChars[FSourceExtensions[CurExtStart+i]]) do
          inc(i);
        if i=ExtLen then begin
          Result:=true;
          exit;
        end;
      end;
      inc(CurExtEnd);
      CurExtStart:=CurExtEnd;
    end else
      inc(CurExtEnd);
  end;
  Result:=false;
end;

function TCodeToolManager.GetMainCode(Code: TCodeBuffer): TCodeBuffer;
var
  NewFile: TCodeBuffer;
begin
  // find MainCode (= the start source, e.g. a unit/program/package source)
  Result:=Code;
  if Result=nil then exit;
  // if this is an include file, find the top level source
  while (Result.LastIncludedByFile<>'') do begin
    NewFile:=SourceCache.LoadFile(Result.LastIncludedByFile);
    if (NewFile=nil) then begin
      Result.LastIncludedByFile:='';
      break;
    end;
    Result:=NewFile;
  end;
  if (not FilenameHasSourceExt(Result.Filename)) then begin
    Result:=FindCodeOfMainUnitHint(Result);
  end;
  if Result=nil then exit;
  CreateScanner(Result);
end;

function TCodeToolManager.GetIncludeCodeChain(Code: TCodeBuffer;
  RemoveFirstCodesWithoutTool: boolean; out ListOfCodeBuffer: TFPList): boolean;
var
  OldCode: TCodeBuffer;
  NewCode: TCodeBuffer;
begin
  // find MainCode (= the start source, e.g. a unit/program/package source)
  Result:=false;
  ListOfCodeBuffer:=nil;
  if Code=nil then exit;
  
  Result:=true;
  ListOfCodeBuffer:=TFPList.Create;
  ListOfCodeBuffer.Add(Code);
  
  // if this is an include file, find the top level source
  while (Code.LastIncludedByFile<>'') do begin
    NewCode:=SourceCache.LoadFile(Code.LastIncludedByFile);
    if NewCode=nil then begin
      NewCode.LastIncludedByFile:='';
      break;
    end;
    Code:=NewCode;
    ListOfCodeBuffer.Insert(0,Code);
  end;

  if (not FilenameHasSourceExt(Code.Filename)) then begin
    OldCode:=Code;
    Code:=FindCodeOfMainUnitHint(OldCode);
    if Code<>OldCode then
      ListOfCodeBuffer.Insert(0,Code);
  end;

  if RemoveFirstCodesWithoutTool then begin
    while ListOfCodeBuffer.Count>0 do begin
      Code:=TCodeBuffer(ListOfCodeBuffer[0]);
      if FindCodeToolForSource(Code)<>nil then break;
      ListOfCodeBuffer.Delete(0);
    end;
    if ListOfCodeBuffer.Count=0 then begin
      ListOfCodeBuffer.Free;
      ListOfCodeBuffer:=nil;
      Result:=false;
      exit;
    end;
  end;
end;

function TCodeToolManager.FindCodeOfMainUnitHint(Code: TCodeBuffer
  ): TCodeBuffer;
var
  MainUnitFilename: string;
begin
  Result:=nil;
  if Code=nil then exit;
  //DebugLn('TCodeToolManager.FindCodeOfMainUnitHint ',Code.Filename);
  if not FindMainUnitHint(Code.Source,MainUnitFilename) then exit;
  MainUnitFilename:=TrimFilename(MainUnitFilename);
  if (not FilenameIsAbsolute(MainUnitFilename))
  and (not Code.IsVirtual) then
    MainUnitFilename:=TrimFilename(ExtractFilePath(Code.Filename)+PathDelim
                                   +MainUnitFilename);
  //DebugLn('TCodeToolManager.FindCodeOfMainUnitHint B ');
  Result:=SourceCache.LoadFile(MainUnitFilename);
end;

procedure TCodeToolManager.CreateScanner(Code: TCodeBuffer);
begin
  if FilenameHasSourceExt(Code.Filename) and (Code.Scanner=nil) then begin
    // create a scanner for the unit/program
    Code.Scanner:=TLinkScanner.Create;
    Code.Scanner.OnGetInitValues:=@OnScannerGetInitValues;
    Code.Scanner.OnSetGlobalWriteLock:=@OnToolSetWriteLock;
    Code.Scanner.OnGetGlobalWriteLockInfo:=@OnToolGetWriteLockInfo;
    Code.Scanner.OnProgress:=@OnScannerProgress;
  end;
end;

procedure TCodeToolManager.ClearError;
begin
  fErrorMsg:='';
  fErrorCode:=nil;
  fErrorLine:=-1;
end;

procedure TCodeToolManager.ClearCurCodeTool;
begin
  ClearError;
  if IdentifierList<>nil then IdentifierList.Clear;
  FCurCodeTool:=nil;
end;

function TCodeToolManager.ApplyChanges: boolean;
begin
  Result:=SourceChangeCache.Apply;
end;

function TCodeToolManager.SetGlobalValue(const VariableName,
  VariableValue: string): boolean;
var
  OldValue: string;
begin
  OldValue:=GlobalValues[VariableName];
  Result:=(OldValue<>VariableValue);
  if not Result then exit;
  GlobalValues[VariableName]:=VariableValue;
  DefineTree.ClearCache;
end;

function TCodeToolManager.GetUnitPathForDirectory(const Directory: string;
  UseCache: boolean): string;
begin
  if UseCache then
    Result:=DirectoryCachePool.GetString(Directory,ctdcsUnitPath,true)
  else
    Result:=DefineTree.GetUnitPathForDirectory(Directory);
end;

function TCodeToolManager.GetIncludePathForDirectory(const Directory: string;
  UseCache: boolean): string;
begin
  if UseCache then
    Result:=DirectoryCachePool.GetString(Directory,ctdcsIncludePath,true)
  else
    Result:=DefineTree.GetIncludePathForDirectory(Directory);
end;

function TCodeToolManager.GetSrcPathForDirectory(const Directory: string;
  UseCache: boolean): string;
begin
  if UseCache then
    Result:=DirectoryCachePool.GetString(Directory,ctdcsSrcPath,true)
  else
    Result:=DefineTree.GetSrcPathForDirectory(Directory);
end;

function TCodeToolManager.GetCompleteSrcPathForDirectory(
  const Directory: string; UseCache: boolean): string;
// returns the SrcPath + UnitPath + any CompiledSrcPath
var
  CurUnitPath: String;
  StartPos: Integer;
  EndPos: LongInt;
  CurSrcPath: String;
  CurUnitDir: String;
  CurCompiledSrcPath: String;
begin
  if UseCache then
    Result:=DirectoryCachePool.GetString(Directory,ctdcsCompleteSrcPath,true)
  else begin
    CurUnitPath:='.;'+GetUnitPathForDirectory(Directory);
    CurSrcPath:=GetSrcPathForDirectory(Directory);
    // for every unit path, get the CompiledSrcPath
    StartPos:=1;
    while StartPos<=length(CurUnitPath) do begin
      EndPos:=StartPos;
      while (EndPos<=length(CurUnitPath)) and (CurUnitPath[EndPos]<>';') do
        inc(EndPos);
      if EndPos>StartPos then begin
        CurUnitDir:=TrimFilename(copy(CurUnitPath,StartPos,EndPos-StartPos));
        if not FilenameIsAbsolute(CurUnitDir) then
          CurUnitDir:=TrimFilename(AppendPathDelim(Directory)+CurUnitDir);
        CurCompiledSrcPath:=CreateAbsoluteSearchPath(
                         GetCompiledSrcPathForDirectory(CurUnitDir),CurUnitDir);
        if CurCompiledSrcPath<>'' then
          CurSrcPath:=CurSrcPath+';'+CurCompiledSrcPath;
      end;
      StartPos:=EndPos+1;
    end;
    // combine unit, src and compiledsrc search path
    Result:=CurUnitPath+';'+CurSrcPath;
    // make it absolute, so the user need less string concatenations
    if FilenameIsAbsolute(Directory) then
      Result:=CreateAbsoluteSearchPath(Result,Directory);
    // trim the paths, remove doubles and empty paths
    Result:=MinimizeSearchPath(Result);
  end;
end;

function TCodeToolManager.GetPPUSrcPathForDirectory(const Directory: string
  ): string;
begin
  Result:=DefineTree.GetPPUSrcPathForDirectory(Directory);
end;

function TCodeToolManager.GetPPWSrcPathForDirectory(const Directory: string
  ): string;
begin
  Result:=DefineTree.GetPPWSrcPathForDirectory(Directory);
end;

function TCodeToolManager.GetDCUSrcPathForDirectory(const Directory: string
  ): string;
begin
  Result:=DefineTree.GetDCUSrcPathForDirectory(Directory);
end;

function TCodeToolManager.GetCompiledSrcPathForDirectory(
  const Directory: string; UseCache: boolean): string;
begin
  Result:=DefineTree.GetCompiledSrcPathForDirectory(Directory);
end;

function TCodeToolManager.GetNestedCommentsFlagForFile(
  const Filename: string): boolean;
var
  Evaluator: TExpressionEvaluator;
  Directory: String;
begin
  Result:=false;
  Directory:=ExtractFilePath(Filename);
  // check pascal compiler is FPC and mode is FPC or OBJFPC
  if GetPascalCompilerForDirectory(Directory)<>pcFPC then exit;
  if not (GetCompilerModeForDirectory(Directory) in [cmFPC,cmOBJFPC]) then exit;
  // check Nested Compiler define is on
  Evaluator:=DefineTree.GetDefinesForDirectory(Directory,true);
  if Evaluator=nil then exit;
  if ((Evaluator.IsDefined(NestedCompilerDefine))
    or (CompareFileExt(Filename,'pp',false)=0))
  then
    Result:=true;
end;

function TCodeToolManager.GetPascalCompilerForDirectory(const Directory: string
  ): TPascalCompiler;
var
  Evaluator: TExpressionEvaluator;
  PascalCompiler: string;
  pc: TPascalCompiler;
begin
  Result:=pcFPC;
  Evaluator:=DefineTree.GetDefinesForDirectory(Directory,true);
  if Evaluator=nil then exit;
  PascalCompiler:=Evaluator.Variables[PascalCompilerDefine];
  for pc:=Low(TPascalCompiler) to High(TPascalCompiler) do
    if (PascalCompiler=PascalCompilerNames[pc]) then
      Result:=pc;
end;

function TCodeToolManager.GetCompilerModeForDirectory(const Directory: string
  ): TCompilerMode;
var
  Evaluator: TExpressionEvaluator;
  cm: TCompilerMode;
begin
  Result:=cmFPC;
  Evaluator:=DefineTree.GetDefinesForDirectory(Directory,true);
  if Evaluator=nil then exit;
  for cm:=Low(TCompilerMode) to High(TCompilerMode) do
    if Evaluator.IsDefined(CompilerModeVars[cm]) then
      Result:=cm;
end;

function TCodeToolManager.GetCompiledSrcExtForDirectory(const Directory: string
  ): string;
var
  Evaluator: TExpressionEvaluator;
begin
  Result:='.ppu';
  Evaluator:=DefineTree.GetDefinesForDirectory(Directory,true);
  if Evaluator=nil then exit;
  if Evaluator.IsDefined('WIN32') and Evaluator.IsDefined('VER1_0') then
    Result:='.ppw';
end;

function TCodeToolManager.FindUnitInUnitLinks(const Directory, UnitName: string
  ): string;
begin
  Result:=DirectoryCachePool.FindUnitInUnitLinks(Directory,UnitName);
end;

function TCodeToolManager.GetUnitLinksForDirectory(const Directory: string;
  UseCache: boolean): string;
var
  Evaluator: TExpressionEvaluator;
begin
  if UseCache then begin
    Result:=DirectoryCachePool.GetString(Directory,ctdcsUnitLinks,true)
  end else begin
    Result:='';
    Evaluator:=DefineTree.GetDefinesForDirectory(Directory,true);
    if Evaluator=nil then exit;
    Result:=Evaluator[UnitLinksMacroName];
  end;
end;

function TCodeToolManager.GetFPCUnitPathForDirectory(const Directory: string;
  UseCache: boolean): string;
var
  Evaluator: TExpressionEvaluator;
begin
  if UseCache then begin
    Result:=DirectoryCachePool.GetString(Directory,ctdcsFPCUnitPath,true)
  end else begin
    Result:='';
    Evaluator:=DefineTree.GetDefinesForDirectory(Directory,true);
    if Evaluator=nil then exit;
    Result:=Evaluator[FPCUnitPathMacroName];
  end;
end;

procedure TCodeToolManager.GetFPCVersionForDirectory(const Directory: string;
  out FPCVersion, FPCRelease, FPCPatch: integer);
var
  Evaluator: TExpressionEvaluator;
  i: Integer;
  VarName: String;
  p: Integer;

  function ReadInt(var AnInteger: integer): boolean;
  var
    StartPos: Integer;
  begin
    StartPos:=p;
    AnInteger:=0;
    while (p<=length(VarName)) and (VarName[p] in ['0'..'9']) do begin
      AnInteger:=AnInteger*10+(ord(VarName[p])-ord('0'));
      if AnInteger>=100 then begin
        Result:=false;
        exit;
      end;
      inc(p);
    end;
    Result:=StartPos<p;
  end;
  
begin
  FPCVersion:=0;
  FPCRelease:=0;
  FPCPatch:=0;
  Evaluator:=DefineTree.GetDefinesForDirectory(Directory,true);
  if Evaluator=nil then exit;
  for i:=0 to Evaluator.Count-1 do begin
    VarName:=Evaluator.Names(i);
    if (length(VarName)>3) and (VarName[1] in ['V','v'])
    and (VarName[2] in ['E','e']) and (VarName[3] in ['R','r'])
    and (VarName[4] in ['0'..'9']) then begin
      p:=4;
      if not ReadInt(FPCVersion) then continue;
      if (p>=length(VarName)) or (VarName[p]<>'_') then continue;
      inc(p);
      if not ReadInt(FPCRelease) then continue;
      if (p>=length(VarName)) or (VarName[p]<>'_') then continue;
      inc(p);
      if not ReadInt(FPCPatch) then continue;
      exit;
    end;
  end;
end;

procedure TCodeToolManager.FreeListOfPCodeXYPosition(var List: TFPList);
var
  i: Integer;
  CursorPos: PCodeXYPosition;
begin
  if List<>nil then begin
    for i:=0 to List.Count-1 do begin
      CursorPos := PCodeXYPosition(List[i]);
      Dispose(CursorPos);
    end;
    List.Free;
    List:=nil;
  end;
end;

procedure TCodeToolManager.FreeTreeOfPCodeXYPosition(var Tree: TAVLTree);
var
  ANode: TAVLTreeNode;
  CursorPos: PCodeXYPosition;
begin
  if Tree=nil then exit;
  ANode:=Tree.FindLowest;
  while ANode<>nil do begin
    CursorPos:=PCodeXYPosition(ANode.Data);
    if CursorPos<>nil then
      Dispose(CursorPos);
    ANode:=Tree.FindSuccessor(ANode);
  end;
  Tree.Free;
end;


function TCodeToolManager.CreateTreeOfPCodeXYPosition: TAVLTree;
begin
  Result:=TAVLTree.Create(TListSortCompare(@CompareCodeXYPositions));
end;

procedure TCodeToolManager.AddListToTreeOfPCodeXYPosition(SrcList: TFPList;
  DestTree: TAVLTree; ClearList, CreateCopies: boolean);
var
  i: Integer;
  CodePos: PCodeXYPosition;
  NewCodePos: PCodeXYPosition;
begin
  if SrcList=nil then exit;
  for i:=SrcList.Count-1 downto 0 do begin
    CodePos:=PCodeXYPosition(SrcList[i]);
    if DestTree.Find(CodePos)=nil then begin
      // new position -> add
      if CreateCopies and (not ClearList) then begin
        // list items should be kept and copies should be added to the tree
        New(NewCodePos);
        NewCodePos^:=CodePos^;
      end else
        NewCodePos:=CodePos;
      DestTree.Add(NewCodePos);
    end else if ClearList then begin
      // position alread exists and items should be deleted
      Dispose(NewCodePos);
    end;
  end;
  if ClearList then
    SrcList.Clear;
end;

function TCodeToolManager.Explore(Code: TCodeBuffer;
  out ACodeTool: TCodeTool; WithStatements: boolean; OnlyInterface: boolean
  ): boolean;
begin
  Result:=false;
  ACodeTool:=nil;
  try
    if InitCurCodeTool(Code) then begin
      ACodeTool:=FCurCodeTool;
      FCurCodeTool.Explore(WithStatements,OnlyInterface);
      Result:=true;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.InitCurCodeTool(Code: TCodeBuffer): boolean;
var MainCode: TCodeBuffer;
begin
  Result:=false;
  ClearCurCodeTool;
  MainCode:=GetMainCode(Code);
  if MainCode=nil then begin
    fErrorMsg:='TCodeToolManager.InitCurCodeTool MainCode=nil';
    exit;
  end;
  if MainCode.Scanner=nil then begin
    FErrorMsg:=Format(ctsNoScannerFound,[MainCode.Filename]);
    exit;
  end;
  FCurCodeTool:=TCodeTool(GetCodeToolForSource(MainCode,false,true));
  FCurCodeTool.ErrorPosition.Code:=nil;
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.InitCurCodeTool] ',Code.Filename,' ',dbgs(Code.SourceLength));
  {$ENDIF}
  Result:=(FCurCodeTool.Scanner<>nil);
  if not Result then begin
    fErrorCode:=MainCode;
    fErrorMsg:=ctsNoScannerAvailable;
  end;
end;

function TCodeToolManager.InitResourceTool: boolean;
begin
  fErrorMsg:='';
  fErrorCode:=nil;
  fErrorLine:=-1;
  Result:=true;
end;

procedure TCodeToolManager.ClearPositions;
begin
  if Positions=nil then
    Positions:=TCodeXYPositions.Create
  else
    Positions.Clear;
end;

function TCodeToolManager.HandleException(AnException: Exception): boolean;
var ErrorSrcTool: TCustomCodeTool;
  DirtyPos: Integer;
  ErrorDirTool: TCompilerDirectivesTree;
begin
  fErrorMsg:=AnException.Message;
  fErrorTopLine:=0;
  if (AnException is ELinkScannerError) then begin
    // link scanner error
    DirtyPos:=0;
    if AnException is ELinkScannerEditError then begin
      fErrorCode:=TCodeBuffer(ELinkScannerEditError(AnException).Buffer);
      if fErrorCode<>nil then
        DirtyPos:=ELinkScannerEditError(AnException).BufferPos;
    end else begin
      fErrorCode:=TCodeBuffer(ELinkScannerError(AnException).Sender.Code);
      DirtyPos:=ELinkScannerError(AnException).Sender.SrcPos;
    end;
    if (fErrorCode<>nil) and (DirtyPos>0) then begin
      fErrorCode.AbsoluteToLineCol(DirtyPos,fErrorLine,fErrorColumn);
    end;
  end else if (AnException is ECodeToolError) then begin
    // codetool error
    ErrorSrcTool:=ECodeToolError(AnException).Sender;
    fErrorCode:=ErrorSrcTool.ErrorPosition.Code;
    fErrorColumn:=ErrorSrcTool.ErrorPosition.X;
    fErrorLine:=ErrorSrcTool.ErrorPosition.Y;
  end else if (AnException is ECDirectiveParserException) then begin
    // Compiler directive parser error
    ErrorDirTool:=ECDirectiveParserException(AnException).Sender;
    fErrorCode:=ErrorDirTool.Code;
    fErrorColumn:=-1;
    fErrorLine:=-1;
  end else if (AnException is ESourceChangeCacheError) then begin
    // SourceChangeCache error
    fErrorCode:=nil;
  end else if (AnException is ECodeToolManagerError) then begin
    // CodeToolManager error
    fErrorCode:=nil;
  end else begin
    // unknown exception
    DumpExceptionBackTrace;
    FErrorMsg:=AnException.ClassName+': '+FErrorMsg;
    if FCurCodeTool<>nil then begin
      fErrorCode:=FCurCodeTool.ErrorPosition.Code;
      fErrorColumn:=FCurCodeTool.ErrorPosition.X;
      fErrorLine:=FCurCodeTool.ErrorPosition.Y;
    end;
  end;

  // adjust error topline
  AdjustErrorTopLine;
  // write error
  WriteError;
  // raise or catch
  if not FCatchExceptions then raise AnException;
  Result:=false;
end;

procedure TCodeToolManager.AdjustErrorTopLine;
begin
  // adjust error topline
  if (fErrorCode<>nil) and (fErrorTopLine<1) then begin
    fErrorTopLine:=fErrorLine;
    if (fErrorTopLine>0) and JumpCentered then begin
      dec(fErrorTopLine,VisibleEditorLines div 2);
      if fErrorTopLine<1 then fErrorTopLine:=1;
    end;
  end;
end;

procedure TCodeToolManager.WriteError;
begin
  if FWriteExceptions then begin
    {$IFDEF CTDEBUG}
    WriteDebugReport(true,false,false,false,false);
    {$ENDIF}
    DbgOut('### TCodeToolManager.HandleException: "'+ErrorMessage+'"');
    if ErrorLine>0 then DbgOut(' at Line=',DbgS(ErrorLine));
    if ErrorColumn>0 then DbgOut(' Col=',DbgS(ErrorColumn));
    if ErrorCode<>nil then DbgOut(' in "',ErrorCode.Filename,'"');
    DebugLn('');
  end;
end;

function TCodeToolManager.CheckSyntax(Code: TCodeBuffer;
  out NewCode: TCodeBuffer; out NewX, NewY, NewTopLine: integer;
  out ErrorMsg: string): boolean;
// returns true on syntax correct
var
  ACodeTool: TCodeTool;
begin
  Result:=Explore(Code,ACodeTool,true);
  if ACodeTool=nil then ;
  NewCode:=ErrorCode;
  NewX:=ErrorColumn;
  NewY:=ErrorLine;
  NewTopLine:=ErrorTopLine;
  ErrorMsg:=ErrorMessage;
end;

function TCodeToolManager.ExploreDirectives(Code: TCodeBuffer; out
  ADirectivesTool: TDirectivesTool): boolean;
begin
  Result:=false;
  ADirectivesTool:=nil;
  try
    if InitCurDirectivesTool(Code) then begin
      ADirectivesTool:=FCurDirectivesTool;
      FCurDirectivesTool.Parse;
      Result:=true;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.JumpToMethod(Code: TCodeBuffer; X,Y: integer;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer;
  var RevertableJump: boolean): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.JumpToMethod A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.JumpToMethod B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindJumpPoint(CursorPos,NewPos,NewTopLine,
                                       RevertableJump);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.JumpToMethod END ');
  {$ENDIF}
end;

function TCodeToolManager.FindDeclaration(Code: TCodeBuffer; X,Y: integer;
  out NewCode: TCodeBuffer;
  out NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclaration A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclaration B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    {$IFDEF DoNotHandleFindDeclException}
    DebugLn('TCodeToolManager.FindDeclaration NOT HANDLING EXCEPTIONS');
    RaiseUnhandableExceptions:=true;
    {$ENDIF}
    Result:=FCurCodeTool.FindDeclaration(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  {$IFDEF DoNotHandleFindDeclException}
  finally
    RaiseUnhandableExceptions:=false;
  end;
  {$ELSE}
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$ENDIF}
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclaration END ');
  {$ENDIF}
end;

function TCodeToolManager.FindSmartHint(Code: TCodeBuffer; X, Y: integer
  ): string;
var
  CursorPos: TCodeXYPosition;
begin
  Result:='';
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindSmartHint A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindSmartHint B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindSmartHint(CursorPos);
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindSmartHint END ');
  {$ENDIF}
end;

function TCodeToolManager.FindDeclarationInInterface(Code: TCodeBuffer;
  const Identifier: string; out NewCode: TCodeBuffer; out NewX, NewY,
  NewTopLine: integer): boolean;
var
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationInInterface A ',Code.Filename,' Identifier=',Identifier);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationInInterface B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindDeclarationInInterface(Identifier,NewPos,
                                                    NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationInInterface END ');
  {$ENDIF}
end;

function TCodeToolManager.FindDeclarationWithMainUsesSection(Code: TCodeBuffer;
  const Identifier: string; out NewCode: TCodeBuffer;
  out NewX, NewY, NewTopLine: integer): Boolean;
var
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationWithMainUsesSection A ',Code.Filename,' Identifier=',Identifier);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindDeclarationWithMainUsesSection(Identifier,NewPos,
                                                            NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationInInterface END ');
  {$ENDIF}
end;

function TCodeToolManager.FindDeclarationAndOverload(Code: TCodeBuffer; X,
  Y: integer; var ListOfPCodeXYPosition: TFPList;
  Flags: TFindDeclarationListFlags): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationAndOverload A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationAndOverload B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindDeclarationAndOverload(CursorPos,
                                                   ListOfPCodeXYPosition,Flags);
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationAndOverload END ');
  {$ENDIF}
end;

function TCodeToolManager.FindMainDeclaration(Code: TCodeBuffer; X, Y: integer;
  out NewCode: TCodeBuffer; out NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindMainDeclaration A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.FindMainDeclaration(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindMainDeclaration END ');
  {$ENDIF}
end;

function TCodeToolManager.FindDeclarationOfPropertyPath(Code: TCodeBuffer;
  const PropertyPath: string; out NewCode: TCodeBuffer; out NewX, NewY,
  NewTopLine: integer): Boolean;
var
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationOfPropertyPath A ',Code.Filename,' Path="',PropertyPath,'"');
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindDeclarationOfPropertyPath(PropertyPath,
                                                       NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDeclarationOfPropertyPath END ');
  {$ENDIF}
end;

function TCodeToolManager.FindCodeContext(Code: TCodeBuffer; X, Y: integer; out
  CodeContexts: TCodeContextInfo): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindCodeContext A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.FindCodeContext(CursorPos,CodeContexts);
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindCodeContext END ');
  {$ENDIF}
end;

function TCodeToolManager.ExtractProcedureHeader(Code: TCodeBuffer; X,
  Y: integer; Attributes: TProcHeadAttributes; var ProcHead: string): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ExtractProcedureHeader A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.ExtractProcedureHeader(CursorPos,Attributes,ProcHead);
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ExtractProcedureHeader END ');
  {$ENDIF}
end;

function TCodeToolManager.GatherIdentifiers(Code: TCodeBuffer; X, Y: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GatherIdentifiers A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GatherIdentifiers B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.GatherIdentifiers(CursorPos,IdentifierList,
                      SourceChangeCache.BeautifyCodeOptions);
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GatherIdentifiers END ');
  {$ENDIF}
end;

function TCodeToolManager.GetIdentifierAt(Code: TCodeBuffer; X, Y: integer;
  var Identifier: string): boolean;
var
  CleanPos: integer;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetIdentifierAt A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  Code.LineColToPosition(Y,X,CleanPos);
  if (CleanPos>0) and (CleanPos<=Code.SourceLength) then begin
    Identifier:=GetIdentifier(@Code.Source[CleanPos]);
    Result:=true;
  end else begin
    Identifier:='';
    Result:=false;
  end;
end;

function TCodeToolManager.IdentItemCheckHasChilds(IdentItem: TIdentifierListItem
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.IdentItemCheckHasChilds A ');
  {$ENDIF}
  try
    IdentItem.CheckHasChilds;
    Result:=true;
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindReferences(IdentifierCode: TCodeBuffer;
  X, Y: integer; TargetCode: TCodeBuffer; SkipComments: boolean;
  var ListOfPCodeXYPosition: TFPList): boolean;
var
  CursorPos: TCodeXYPosition;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindReferences A ',IdentifierCode.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  ListOfPCodeXYPosition:=nil;
  if not FindMainDeclaration(IdentifierCode,X,Y,NewCode,NewX,NewY,NewTopLine)
  then begin
    DebugLn('TCodeToolManager.FindReferences unable to FindMainDeclaration ',IdentifierCode.Filename,' x=',dbgs(x),' y=',dbgs(y));
    exit;
  end;
  if NewTopLine=0 then ;
  if not InitCurCodeTool(TargetCode) then exit;
  CursorPos.X:=NewX;
  CursorPos.Y:=NewY;
  CursorPos.Code:=NewCode;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindReferences B ',dbgs(FCurCodeTool.Scanner<>nil),' x=',dbgs(CursorPos.X),' y=',dbgs(CursorPos.Y),' ',CursorPos.Code.Filename);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindReferences(CursorPos,SkipComments,
                                        ListOfPCodeXYPosition);
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindReferences END ');
  {$ENDIF}
end;

function TCodeToolManager.RenameIdentifier(TreeOfPCodeXYPosition: TAVLTree;
  const OldIdentifier, NewIdentifier: string): boolean;
var
  ANode: TAVLTreeNode;
  CurCodePos: PCodeXYPosition;
  IdentStartPos: integer;
  IdentLen: Integer;
  Code: TCodeBuffer;
begin
  Result:=false;
  { $IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenameIdentifier A Old=',OldIdentifier,' New=',NewIdentifier,' ',dbgs(TreeOfPCodeXYPosition<>nil));
  { $ENDIF}
  if TreeOfPCodeXYPosition=nil then begin
    Result:=true;
    exit;
  end;
  if (NewIdentifier='') or (not IsValidIdent(NewIdentifier)) then exit;

  ClearCurCodeTool;
  SourceChangeCache.Clear;
  IdentLen:=length(OldIdentifier);

  // the tree is sorted for descending line code positions
  // -> go from end of source to start of source, so that replacing does not
  // change any CodeXYPosition not yet processed
  ANode:=TreeOfPCodeXYPosition.FindLowest;
  while ANode<>nil do begin
    // next position
    CurCodePos:=PCodeXYPosition(ANode.Data);
    Code:=CurCodePos^.Code;
    Code.LineColToPosition(CurCodePos^.Y,CurCodePos^.X,IdentStartPos);
    DebugLn('TCodeToolManager.RenameIdentifier File ',Code.Filename,' Line=',dbgs(CurCodePos^.Y),' Col=',dbgs(CurCodePos^.X));
    // search absolute position in source
    if IdentStartPos<1 then begin
      SetError(Code, CurCodePos^.Y, CurCodePos^.X, ctsPositionNotInSource);
      exit;
    end;
    // check if old identifier is there
    if CompareIdentifiers(@Code.Source[IdentStartPos],PChar(Pointer(OldIdentifier)))<>0
    then begin
      SetError(CurCodePos^.Code,CurCodePos^.Y,CurCodePos^.X,
        Format(ctsStrExpectedButAtomFound,[OldIdentifier,
                                   GetIdentifier(@Code.Source[IdentStartPos])])
        );
      exit;
    end;
    // change if needed
    if CompareIdentifiersCaseSensitive(@Code.Source[IdentStartPos],
       PChar(Pointer(NewIdentifier)))<>0
    then begin
      DebugLn('TCodeToolManager.RenameIdentifier Change ');
      SourceChangeCache.ReplaceEx(gtNone,gtNone,1,1,Code,
         IdentStartPos,IdentStartPos+IdentLen,NewIdentifier);
    end else begin
      DebugLn('TCodeToolManager.RenameIdentifier KEPT ',GetIdentifier(@Code.Source[IdentStartPos]));
    end;
    ANode:=TreeOfPCodeXYPosition.FindSuccessor(ANode);
  end;
  // apply
  DebugLn('TCodeToolManager.RenameIdentifier Apply');
  if not SourceChangeCache.Apply then exit;

  DebugLn('TCodeToolManager.RenameIdentifier Success');
  Result:=true;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenameIdentifier END ');
  {$ENDIF}
end;

function TCodeToolManager.ReplaceWord(Code: TCodeBuffer; const OldWord,
  NewWord: string; ChangeStrings: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ReplaceWord A ',Code.Filename,' OldWord="',OldWord,'" NewWord="',NewWord,'"');
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ReplaceWord(OldWord, NewWord, ChangeStrings,
                       SourceChangeCache);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.GatherResourceStringSections(Code: TCodeBuffer;
  X, Y: integer; CodePositions: TCodeXYPositions): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GatherResourceStringSections A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  if CodePositions=nil then begin
    ClearPositions;
    CodePositions:=Positions;
  end;
  try
    Result:=FCurCodeTool.GatherResourceStringSections(CursorPos,CodePositions);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.IdentifierExistsInResourceStringSection(
  Code: TCodeBuffer; X, Y: integer; const ResStrIdentifier: string): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.IdentifierExistsInResourceStringSection A ',Code.Filename,' x=',dbgs(x),' y=',dbgs(y));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.IdentifierExistsInResourceStringSection(CursorPos,
                                                              ResStrIdentifier);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.CreateIdentifierFromStringConst(
  StartCode: TCodeBuffer; StartX, StartY: integer;
  EndCode: TCodeBuffer; EndX, EndY: integer;
  out Identifier: string; MaxLen: integer): boolean;
var
  StartCursorPos, EndCursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CreateIdentifierFromStringConst A ',StartCode.Filename,' x=',dbgs(StartX),' y=',dbgs(StartY));
  {$ENDIF}
  if not InitCurCodeTool(StartCode) then exit;
  StartCursorPos.X:=StartX;
  StartCursorPos.Y:=StartY;
  StartCursorPos.Code:=StartCode;
  EndCursorPos.X:=EndX;
  EndCursorPos.Y:=EndY;
  EndCursorPos.Code:=EndCode;
  Identifier:='';
  try
    Result:=FCurCodeTool.CreateIdentifierFromStringConst(
                                 StartCursorPos,EndCursorPos,Identifier,MaxLen);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.StringConstToFormatString(
  StartCode: TCodeBuffer; StartX, StartY: integer;
  EndCode: TCodeBuffer; EndX, EndY: integer;
  out FormatStringConstant, FormatParameters: string;
  out StartInStringConst, EndInStringConst: boolean): boolean;
var
  StartCursorPos, EndCursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.StringConstToFormatString A ',StartCode.Filename,' x=',dbgs(StartX),' y=',dbgs(StartY));
  {$ENDIF}
  if not InitCurCodeTool(StartCode) then exit;
  StartCursorPos.X:=StartX;
  StartCursorPos.Y:=StartY;
  StartCursorPos.Code:=StartCode;
  EndCursorPos.X:=EndX;
  EndCursorPos.Y:=EndY;
  EndCursorPos.Code:=EndCode;
  try
    Result:=FCurCodeTool.StringConstToFormatString(
             StartCursorPos,EndCursorPos,FormatStringConstant,FormatParameters,
             StartInStringConst,EndInStringConst);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.GatherResourceStringsWithValue(
  SectionCode: TCodeBuffer; SectionX, SectionY: integer;
  const StringValue: string; CodePositions: TCodeXYPositions): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GatherResourceStringsWithValue A ',SectionCode.Filename,' x=',dbgs(SectionX),' y=',dbgs(SectionY));
  {$ENDIF}
  if not InitCurCodeTool(SectionCode) then exit;
  CursorPos.X:=SectionX;
  CursorPos.Y:=SectionY;
  CursorPos.Code:=SectionCode;
  if CodePositions=nil then begin
    ClearPositions;
    CodePositions:=Positions;
  end;
  try
    Result:=FCurCodeTool.GatherResourceStringsWithValue(CursorPos,StringValue,
                                                        CodePositions);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddResourcestring(
  CursorCode: TCodeBuffer; X,Y: integer;
  SectionCode: TCodeBuffer; SectionX, SectionY: integer;
  const NewIdentifier, NewValue: string;
  InsertPolicy: TResourcestringInsertPolicy): boolean;
var
  CursorPos, SectionPos, NearestPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddResourcestring A ',SectionCode.Filename,' x=',dbgs(SectionX),' y=',dbgs(SectionY));
  {$ENDIF}
  if not InitCurCodeTool(SectionCode) then exit;
  SectionPos.X:=SectionX;
  SectionPos.Y:=SectionY;
  SectionPos.Code:=SectionCode;
  try
    NearestPos.Code:=nil;
    if InsertPolicy=rsipContext then begin
      CursorPos.X:=X;
      CursorPos.Y:=Y;
      CursorPos.Code:=CursorCode;
      Result:=FCurCodeTool.FindNearestResourceString(CursorPos, SectionPos,
                                                     NearestPos);
      if not Result then exit;
    end;
    Result:=FCurCodeTool.AddResourcestring(SectionPos, NewIdentifier, NewValue,
                                     InsertPolicy,NearestPos,SourceChangeCache);
  except
    on e: Exception do HandleException(e);
  end;
end;

procedure TCodeToolManager.ImproveStringConstantStart(const ACode: string;
  var StartPos: integer);
begin
  BasicCodeTools.ImproveStringConstantStart(ACode,StartPos);
end;

procedure TCodeToolManager.ImproveStringConstantEnd(const ACode: string;
  var EndPos: integer);
begin
  BasicCodeTools.ImproveStringConstantEnd(ACode,EndPos);
end;

function TCodeToolManager.GetStringConstBounds(Code: TCodeBuffer; X, Y: integer;
  var StartCode: TCodeBuffer; var StartX, StartY: integer;
  var EndCode: TCodeBuffer; var EndX, EndY: integer;
  ResolveComments: boolean): boolean;
var
  CursorPos, StartPos, EndPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetStringConstBounds A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.GetStringConstBounds(CursorPos,StartPos,EndPos,
                                              ResolveComments);
    if Result then begin
      StartCode:=StartPos.Code;
      StartX:=StartPos.X;
      StartY:=StartPos.Y;
      EndCode:=EndPos.Code;
      EndX:=EndPos.X;
      EndY:=EndPos.Y;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ReplaceCode(Code: TCodeBuffer; StartX,
  StartY: integer; EndX, EndY: integer; const NewCode: string): boolean;
var
  StartCursorPos, EndCursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ReplaceCode A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  StartCursorPos.X:=StartX;
  StartCursorPos.Y:=StartY;
  StartCursorPos.Code:=Code;
  EndCursorPos.X:=EndX;
  EndCursorPos.Y:=EndY;
  EndCursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.ReplaceCode(StartCursorPos,EndCursorPos,NewCode,
                                     SourceChangeCache);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.GuessMisplacedIfdefEndif(Code: TCodeBuffer; X,Y: integer;
  var NewCode: TCodeBuffer;
  var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GuessMisplacedIfdefEndif A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.GuessMisplacedIfdefEndif(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindEnclosingIncludeDirective(Code: TCodeBuffer; X,
  Y: integer; var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindEnclosingIncludeDirective A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.FindEnclosingIncludeDirective(CursorPos,
                                                       NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindResourceDirective(Code: TCodeBuffer; StartX,
  StartY: integer;
  out NewCode: TCodeBuffer; out NewX, NewY, NewTopLine: integer;
  const Filename: string; SearchInCleanSrc: boolean): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
  Tree: TCompilerDirectivesTree;
  p: integer;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindResourceDirective A ',Code.Filename);
  {$ENDIF}
  NewCode:=nil;
  NewX:=0;
  NewY:=0;
  NewTopLine:=0;
  if SearchInCleanSrc then begin
    if not InitCurCodeTool(Code) then exit;
    CursorPos.X:=StartX;
    CursorPos.Y:=StartY;
    CursorPos.Code:=Code;
    try
      Result:=FCurCodeTool.FindResourceDirective(CursorPos,NewPos,NewTopLine,
                                                 Filename);
      if Result then begin
        NewX:=NewPos.X;
        NewY:=NewPos.Y;
        NewCode:=NewPos.Code;
      end;
    except
      on e: Exception do Result:=HandleException(e);
    end;
  end else begin
    try
      Tree:=TCompilerDirectivesTree.Create;
      try
        Tree.Parse(Code,GetNestedCommentsFlagForFile(Code.Filename));
        Code.LineColToPosition(StartY,StartX,p);
        Result:=Tree.NodeStartToCodePos(Tree.FindResourceDirective(Filename,p),
                                        CursorPos);
        NewCode:=CursorPos.Code;
        NewX:=CursorPos.X;
        NewY:=CursorPos.Y;
        NewTopLine:=NewY;
      finally
        Tree.Free;
      end;
    except
      on e: Exception do Result:=HandleException(e);
    end;
  end;
end;

function TCodeToolManager.AddResourceDirective(Code: TCodeBuffer;
  const Filename: string; SearchInCleanSrc: boolean; const NewSrc: string
  ): boolean;
var
  Tree: TCompilerDirectivesTree;
  Node: TCodeTreeNode;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddResourceDirective A ',Code.Filename,' Filename=',Filename);
  {$ENDIF}
  if SearchInCleanSrc then begin
    if not InitCurCodeTool(Code) then exit;
    try
      Result:=FCurCodeTool.AddResourceDirective(Filename,SourceChangeCache,NewSrc);
    except
      on e: Exception do Result:=HandleException(e);
    end;
  end else begin
    try
      Tree:=TCompilerDirectivesTree.Create;
      try
        Tree.Parse(Code,GetNestedCommentsFlagForFile(Code.Filename));
        Node:=Tree.FindResourceDirective(Filename);
        if Node=nil then
          Result:=AddResourceDirective(Code,Filename,true,NewSrc)
        else
          Result:=true;
      finally
        Tree.Free;
      end;
    except
      on e: Exception do Result:=HandleException(e);
    end;
  end;
end;

function TCodeToolManager.RemoveDirective(Code: TCodeBuffer; NewX,
  NewY: integer; RemoveEmptyIFs: boolean): boolean;
var
  Tree: TCompilerDirectivesTree;
  p: integer;
  Node: TCodeTreeNode;
  Changed: boolean;
  ParentNode: TCodeTreeNode;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemoveDirective A ',Code.Filename);
  {$ENDIF}
  try
    Code.LineColToPosition(NewY,NewX,p);
    if (p<1) or (p>Code.SourceLength) then exit;
    Tree:=TCompilerDirectivesTree.Create;
    try
      Tree.Parse(Code,GetNestedCommentsFlagForFile(Code.Filename));
      Node:=Tree.FindNodeAtPos(p);
      if Node=nil then exit;
      ParentNode:=Node.Parent;
      Changed:=false;
      Tree.DisableNode(Node,Changed,true);
      if RemoveEmptyIFs and (ParentNode<>nil) and Tree.NodeIsEmpty(ParentNode) then
        Tree.DisableNode(ParentNode,Changed,true);
      Result:=Changed;
    finally
      Tree.Free;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FixIncludeFilenames(Code: TCodeBuffer;
  Recursive: boolean; out MissingIncludeFilesCodeXYPos: TFPList): boolean;
  
  procedure CreateErrorForMissingIncludeFile;
  var
    CodePos: PCodeXYPosition;
  begin
    ClearError;
    CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[0]);
    fErrorCode:=CodePos^.Code;
    fErrorLine:=CodePos^.Y;
    fErrorColumn:=CodePos^.X;
    FErrorMsg:='missing include file';
  end;
  
var
  FoundIncludeFiles: TStrings;
  i: Integer;
  AFilename: string;
  ToFixIncludeFiles: TStringList;
  FixedIncludeFiles: TStringList;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FixIncludeFilenames A ',Code.Filename,' Recursive=', DbgS(Recursive));
  {$ENDIF}
  MissingIncludeFilesCodeXYPos:=nil;
  if not InitCurCodeTool(Code) then exit;
  try
    FixedIncludeFiles:=nil;
    ToFixIncludeFiles:=TStringList.Create;
    try
      ToFixIncludeFiles.Add(Code.Filename);
      while ToFixIncludeFiles.Count>0 do begin
        // get next include file
        AFilename:=ToFixIncludeFiles[ToFixIncludeFiles.Count-1];
        ToFixIncludeFiles.Delete(ToFixIncludeFiles.Count-1);
        Code:=LoadFile(AFilename,false,false);
        if Code=nil then begin
          raise ECodeToolError.Create(FCurCodeTool,
                                      'unable to read file "'+AFilename+'"');
        end;
        // fix file
        FoundIncludeFiles:=nil;
        try
          Result:=FCurCodeTool.FixIncludeFilenames(Code,SourceChangeCache,
                                FoundIncludeFiles,MissingIncludeFilesCodeXYPos);
          if (MissingIncludeFilesCodeXYPos<>nil)
          and (MissingIncludeFilesCodeXYPos.Count>0) then begin
            DebugLn('TCodeToolManager.FixIncludeFilenames Missing: ',dbgs(MissingIncludeFilesCodeXYPos.Count));
            Result:=false;
            CreateErrorForMissingIncludeFile;
            exit;
          end;
          if not Recursive then begin
            // check only main file -> stop
            exit;
          end;
          // remember, that the file has been fixed to avoid cycles
          if FixedIncludeFiles=nil then
            FixedIncludeFiles:=TStringList.Create;
          FixedIncludeFiles.Add(Code.Filename);
          // add new include files to stack
          if FoundIncludeFiles<>nil then begin
            for i:=0 to FoundIncludeFiles.Count-1 do begin
              AFilename:=FoundIncludeFiles[i];
              if ((FixedIncludeFiles=nil)
              or (FixedIncludeFiles.IndexOf(AFilename)<0))
              and (ToFixIncludeFiles.IndexOf(AFilename)<0) then begin
                ToFixIncludeFiles.Add(AFilename);
              end;
            end;
          end;
          //DebugLn('TCodeToolManager.FixIncludeFilenames FixedIncludeFiles=',FixedIncludeFiles.Text,' ToFixIncludeFiles=',ToFixIncludeFiles.Text);
        finally
          FoundIncludeFiles.Free;
        end;
      end;
    finally
      FixedIncludeFiles.Free;
      ToFixIncludeFiles.Free;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FixMissingH2PasDirectives(Code: TCodeBuffer;
  var Changed: boolean): boolean;
begin
  Result:=false;
  try
    if InitCurDirectivesTool(Code) then begin
      FCurDirectivesTool.Parse;
      FCurDirectivesTool.FixMissingH2PasDirectives(Changed);
      Result:=true;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ReduceCompilerDirectives(Code: TCodeBuffer;
  Undefines, Defines: TStrings; var Changed: boolean): boolean;
begin
  Result:=false;
  try
    if InitCurDirectivesTool(Code) then begin
      FCurDirectivesTool.Parse;
      FCurDirectivesTool.ReduceCompilerDirectives(Undefines,Defines,Changed);
      Result:=true;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.IsKeyword(Code: TCodeBuffer; const KeyWord: string
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.IsKeyword A ',Code.Filename,' Keyword=',KeyWord);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.StringIsKeyWord(KeyWord);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ExtractCodeWithoutComments(Code: TCodeBuffer): string;
begin
  Result:=CleanCodeFromComments(Code.Source,
                                GetNestedCommentsFlagForFile(Code.Filename));
end;

function TCodeToolManager.FindBlockCounterPart(Code: TCodeBuffer;
  X, Y: integer; var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindBlockCounterPart A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindBlockCounterPart B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindBlockCounterPart(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindBlockCounterPart END ');
  {$ENDIF}
end;

function TCodeToolManager.FindBlockStart(Code: TCodeBuffer;
  X, Y: integer; var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindBlockStart A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindBlockStart B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindBlockStart(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindBlockStart END ');
  {$ENDIF}
end;

function TCodeToolManager.GuessUnclosedBlock(Code: TCodeBuffer; X, Y: integer;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GuessUnclosedBlock A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GuessUnclosedBlock B ',dbgs(FCurCodeTool.Scanner<>nil));
  {$ENDIF}
  try
    Result:=FCurCodeTool.GuessUnclosedBlock(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GuessUnclosedBlock END ');
  {$ENDIF}
end;

function TCodeToolManager.GetCompatiblePublishedMethods(Code: TCodeBuffer;
  const AClassName: string; TypeData: PTypeData; Proc: TGetStringProc): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetCompatiblePublishedMethods A ',Code.Filename,' Classname=',AClassname);
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.GetCompatiblePublishedMethods(UpperCaseStr(AClassName),
       TypeData,Proc);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.PublishedMethodExists(Code:TCodeBuffer;
  const AClassName, AMethodName: string; TypeData: PTypeData;
  out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.PublishedMethodExists A ',Code.Filename,' ',AClassName,':',AMethodName);
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    Result:=FCurCodeTool.PublishedMethodExists(UpperCaseStr(AClassName),
              UpperCaseStr(AMethodName),TypeData,
              MethodIsCompatible,MethodIsPublished,IdentIsMethod);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.JumpToPublishedMethodBody(Code: TCodeBuffer;
  const AClassName, AMethodName: string;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var NewPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.JumpToPublishedMethodBody A ',Code.Filename,' ',AClassName,':',AMethodName);
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    Result:=FCurCodeTool.JumpToPublishedMethodBody(UpperCaseStr(AClassName),
              UpperCaseStr(AMethodName),NewPos,NewTopLine);
    if Result then begin
      NewCode:=NewPos.Code;
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenamePublishedMethod(Code: TCodeBuffer;
  const AClassName, OldMethodName, NewMethodName: string): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenamePublishedMethod A');
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    SourceChangeCache.Clear;
    Result:=FCurCodeTool.RenamePublishedMethod(UpperCaseStr(AClassName),
              UpperCaseStr(OldMethodName),NewMethodName,
              SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CreatePublishedMethod(Code: TCodeBuffer;
  const AClassName, NewMethodName: string; ATypeInfo: PTypeInfo;
  UseTypeInfoForParameters: boolean;
  const APropertyUnitName: string; const APropertyPath: string): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CreatePublishedMethod A');
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    SourceChangeCache.Clear;
    Result:=FCurCodeTool.CreateMethod(UpperCaseStr(AClassName),
            NewMethodName,ATypeInfo,APropertyUnitName,APropertyPath,
            SourceChangeCache,UseTypeInfoForParameters,pcsPublished);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CreatePrivateMethod(Code: TCodeBuffer;
  const AClassName, NewMethodName: string; ATypeInfo: PTypeInfo;
  UseTypeInfoForParameters: boolean;
  const APropertyUnitName, APropertyPath: string): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CreatePrivateMethod A');
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    SourceChangeCache.Clear;
    Result:=FCurCodeTool.CreateMethod(UpperCaseStr(AClassName),
            NewMethodName,ATypeInfo,APropertyUnitName,APropertyPath,
            SourceChangeCache,UseTypeInfoForParameters,pcsPrivate);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.GetIDEDirectives(Code: TCodeBuffer;
  DirectiveList: TStrings): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetIDEDirectives A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.GetIDEDirectives(DirectiveList);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.SetIDEDirectives(Code: TCodeBuffer;
  DirectiveList: TStrings): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetIDEDirectives A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.SetIDEDirectives(DirectiveList,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.JumpToLinkerIdentifier(Code: TCodeBuffer;
  const SourceFilename: string; SourceLine: integer;
  const MangledFunction, Identifier: string; out NewCode: TCodeBuffer; out
  NewX, NewY, NewTopLine: integer): boolean;
var
  NewPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.JumpToLinkerIdentifier A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindJumpPointForLinkerPos(
                        SourceFilename, SourceLine, MangledFunction, Identifier,
                        NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CompleteCode(Code: TCodeBuffer; X,Y,TopLine: integer;
  out NewCode: TCodeBuffer; out NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CompleteCode A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.CompleteCode(CursorPos,TopLine,
                                           NewPos,NewTopLine,SourceChangeCache);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindRedefinitions(Code: TCodeBuffer; out
  TreeOfCodeTreeNodeExt: TAVLTree; WithEnums: boolean): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindRedefinitions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindRedefinitions(TreeOfCodeTreeNodeExt,WithEnums);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveRedefinitions(Code: TCodeBuffer;
  TreeOfCodeTreeNodeExt: TAVLTree): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemoveRedefinitions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemoveRedefinitions(TreeOfCodeTreeNodeExt,
                                             SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveAllRedefinitions(Code: TCodeBuffer): boolean;
var
  TreeOfCodeTreeNodeExt: TAVLTree;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemoveAllRedefinitions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  try
    TreeOfCodeTreeNodeExt:=nil;
    if not InitCurCodeTool(Code) then exit;
    try
      Result:=FCurCodeTool.FindRedefinitions(TreeOfCodeTreeNodeExt,false);
      if not Result then exit;
      Result:=FCurCodeTool.RemoveRedefinitions(TreeOfCodeTreeNodeExt,
                                               SourceChangeCache);
    except
      on e: Exception do Result:=HandleException(e);
    end;
  finally
    if TreeOfCodeTreeNodeExt<>nil then begin
      TreeOfCodeTreeNodeExt.FreeAndClear;
      TreeOfCodeTreeNodeExt.Free;
    end;
  end;
end;

function TCodeToolManager.FindAliasDefinitions(Code: TCodeBuffer; out
  TreeOfCodeTreeNodeExt: TAVLTree; OnlyWrongType: boolean): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindAliasDefinitions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindAliasDefinitions(TreeOfCodeTreeNodeExt,
                                              OnlyWrongType);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FixAliasDefinitions(Code: TCodeBuffer;
  TreeOfCodeTreeNodeExt: TAVLTree): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FixAliasDefinitions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FixAliasDefinitions(TreeOfCodeTreeNodeExt,
                                             SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FixAllAliasDefinitions(Code: TCodeBuffer): boolean;
var
  TreeOfCodeTreeNodeExt: TAVLTree;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FixAllAliasDefinitions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  try
    TreeOfCodeTreeNodeExt:=nil;
    if not InitCurCodeTool(Code) then exit;
    try
      Result:=FCurCodeTool.FindAliasDefinitions(TreeOfCodeTreeNodeExt,true);
      if not Result then exit;
      Result:=FCurCodeTool.FixAliasDefinitions(TreeOfCodeTreeNodeExt,
                                               SourceChangeCache);
    except
      on e: Exception do Result:=HandleException(e);
    end;
  finally
    if TreeOfCodeTreeNodeExt<>nil then begin
      TreeOfCodeTreeNodeExt.FreeAndClear;
      TreeOfCodeTreeNodeExt.Free;
    end;
  end;
end;

function TCodeToolManager.FindConstFunctions(Code: TCodeBuffer; out
  TreeOfCodeTreeNodeExt: TAVLTree): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindConstFunctions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindConstFunctions(TreeOfCodeTreeNodeExt);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ReplaceConstFunctions(Code: TCodeBuffer;
  TreeOfCodeTreeNodeExt: TAVLTree): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ReplaceConstFunctions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ReplaceConstFunctions(TreeOfCodeTreeNodeExt,
                                               SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ReplaceAllConstFunctions(Code: TCodeBuffer): boolean;
var
  TreeOfCodeTreeNodeExt: TAVLTree;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ReplaceAllConstFunctions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    repeat
      TreeOfCodeTreeNodeExt:=nil;
      try
        Result:=FCurCodeTool.FindConstFunctions(TreeOfCodeTreeNodeExt);
        if (not Result) or (TreeOfCodeTreeNodeExt=nil)
        or (TreeOfCodeTreeNodeExt.Count=0) then
          break;
        Result:=FCurCodeTool.ReplaceConstFunctions(TreeOfCodeTreeNodeExt,
                                                   SourceChangeCache);
      finally
        if TreeOfCodeTreeNodeExt<>nil then begin
          TreeOfCodeTreeNodeExt.FreeAndClear;
          TreeOfCodeTreeNodeExt.Free;
        end;
      end;
    until not Result;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindTypeCastFunctions(Code: TCodeBuffer; out
  TreeOfCodeTreeNodeExt: TAVLTree): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindTypeCastFunctions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindTypeCastFunctions(TreeOfCodeTreeNodeExt);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ReplaceTypeCastFunctions(Code: TCodeBuffer;
  TreeOfCodeTreeNodeExt: TAVLTree): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ReplaceTypeCastFunctions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ReplaceTypeCastFunctions(TreeOfCodeTreeNodeExt,
                                                  SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ReplaceAllTypeCastFunctions(Code: TCodeBuffer
  ): boolean;
var
  TreeOfCodeTreeNodeExt: TAVLTree;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ReplaceAllTypeCastFunctions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    repeat
      TreeOfCodeTreeNodeExt:=nil;
      try
        Result:=FCurCodeTool.FindTypeCastFunctions(TreeOfCodeTreeNodeExt);
        if (not Result) or (TreeOfCodeTreeNodeExt=nil)
        or (TreeOfCodeTreeNodeExt.Count=0) then
          break;
        Result:=FCurCodeTool.ReplaceTypeCastFunctions(TreeOfCodeTreeNodeExt,
                                                      SourceChangeCache);
      finally
        if TreeOfCodeTreeNodeExt<>nil then begin
          TreeOfCodeTreeNodeExt.FreeAndClear;
          TreeOfCodeTreeNodeExt.Free;
        end;
      end;
    until not Result;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FixForwardDefinitions(Code: TCodeBuffer): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FixForwardDefinitions A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FixForwardDefinitions(SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.InitClassCompletion(Code: TCodeBuffer;
  const UpperClassName: string; out CodeTool: TCodeTool): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.InitClassCompletion A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  CodeTool:=nil;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.InitClassCompletion(UpperClassName,SourceChangeCache);
    CodeTool:=FCurCodeTool;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CheckExtractProc(Code: TCodeBuffer; const StartPoint,
  EndPoint: TPoint; var MethodPossible, SubProcSameLvlPossible: boolean): boolean;
var
  StartPos, EndPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CheckExtractProc A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  StartPos.X:=StartPoint.X;
  StartPos.Y:=StartPoint.Y;
  StartPos.Code:=Code;
  EndPos.X:=EndPoint.X;
  EndPos.Y:=EndPoint.Y;
  EndPos.Code:=Code;
  try
    Result:=FCurCodeTool.CheckExtractProc(StartPos,EndPos,MethodPossible,
                                          SubProcSameLvlPossible);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ExtractProc(Code: TCodeBuffer; const StartPoint,
  EndPoint: TPoint; ProcType: TExtractProcType; const ProcName: string;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var
  StartPos, EndPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ExtractProc A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  StartPos.X:=StartPoint.X;
  StartPos.Y:=StartPoint.Y;
  StartPos.Code:=Code;
  EndPos.X:=EndPoint.X;
  EndPos.Y:=EndPoint.Y;
  EndPos.Code:=Code;
  try
    Result:=FCurCodeTool.ExtractProc(StartPos,EndPos,ProcType,ProcName,
                                     NewPos,NewTopLine,SourceChangeCache);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.InsertCodeTemplate(Code: TCodeBuffer;
  SelectionStart, SelectionEnd: TPoint; TopLine: integer;
  CodeTemplate: TCodeToolTemplate; var NewCode: TCodeBuffer; var NewX, NewY,
  NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  EndPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.InsertCodeTemplate A ',Code.Filename);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=SelectionStart.X;
  CursorPos.Y:=SelectionStart.Y;
  CursorPos.Code:=Code;
  EndPos.X:=SelectionStart.X;
  EndPos.Y:=SelectionStart.Y;
  EndPos.Code:=Code;
  try
    Result:=FCurCodeTool.InsertCodeTemplate(CursorPos,EndPos,TopLine,
                              CodeTemplate,NewPos,NewTopLine,SourceChangeCache);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.GetSourceName(Code: TCodeBuffer;
  SearchMainCode: boolean): string;
begin
  Result:='';
  if (Code=nil)
  or ((not SearchMainCode) and (Code.LastIncludedByFile<>'')) then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetSourceName A ',Code.Filename,' ',dbgs(Code.SourceLength));
  {$ENDIF}
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(MemCheck_GetMem_Cnt));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.GetSourceName;
  except
    on e: Exception do begin
      Result:=FCurCodeTool.ExtractSourceName;
      HandleException(e);
    end;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetSourceName B ',Code.Filename,' ',dbgs(Code.SourceLength));
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(MemCheck_GetMem_Cnt));
  {$ENDIF}
  DebugLn('SourceName=',Result);
  {$ENDIF}
end;

function TCodeToolManager.GetCachedSourceName(Code: TCodeBuffer): string;
begin
  Result:='';
  if (Code=nil)
  or (Code.LastIncludedByFile<>'') then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetCachedSourceName A ',Code.Filename,' ',dbgs(Code.SourceLength));
  {$ENDIF}
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(MemCheck_GetMem_Cnt));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.GetCachedSourceName;
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetCachedSourceName B ',Code.Filename,' ',dbgs(Code.SourceLength));
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(MemCheck_GetMem_Cnt));
  {$ENDIF}
  DebugLn('SourceName=',Result);
  {$ENDIF}
end;

function TCodeToolManager.GetSourceType(Code: TCodeBuffer;
  SearchMainCode: boolean): string;
begin
  Result:='';
  if (Code=nil)
  or ((not SearchMainCode) and (Code.LastIncludedByFile<>'')) then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetSourceType A ',Code.Filename,' ',dbgs(Code.SourceLength));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    // GetSourceType does not parse the code -> parse it with GetSourceName
    FCurCodeTool.GetSourceName;
    case FCurCodeTool.GetSourceType of
      ctnProgram: Result:='PROGRAM';
      ctnPackage: Result:='PACKAGE';
      ctnLibrary: Result:='LIBRARY';
      ctnUnit: Result:='UNIT';
    else
      Result:='';
    end;
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetSourceType END ',Code.Filename,',',dbgs(Code.SourceLength));
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(MemCheck_GetMem_Cnt));
  {$ENDIF}
  DebugLn('SourceType=',Result);
  {$ENDIF}
end;

function TCodeToolManager.RenameSource(Code: TCodeBuffer;
  const NewName: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenameSource A ',Code.Filename,' NewName=',NewName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameSource(NewName,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindUnitInAllUsesSections(Code: TCodeBuffer;
  const AnUnitName: string;
  out NamePos, InPos: integer): boolean;
var NameAtomPos, InAtomPos: TAtomPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindUnitInAllUsesSections A ',Code.Filename,' UnitName=',AnUnitName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindUnitInAllUsesSections B ',Code.Filename,' UnitName=',AnUnitName);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindUnitInAllUsesSections(UpperCaseStr(AnUnitName),
                NameAtomPos, InAtomPos);
    if Result then begin
      NamePos:=NameAtomPos.StartPos;
      InPos:=InAtomPos.StartPos;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameUsedUnit(Code: TCodeBuffer;
  const OldUnitName, NewUnitName, NewUnitInFile: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenameUsedUnit A ',Code.Filename,' Old=',OldUnitName,' New=',NewUnitName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameUsedUnit(UpperCaseStr(OldUnitName),NewUnitName,
                  NewUnitInFile,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ReplaceUsedUnits(Code: TCodeBuffer;
  UnitNamePairs: TStringToStringTree): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ReplaceUsedUnits A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ReplaceUsedUnits(UnitNamePairs,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.AddUnitToMainUsesSection(Code: TCodeBuffer;
  const NewUnitName, NewUnitInFile: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddUnitToMainUsesSection A ',Code.Filename,' NewUnitName=',NewUnitName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.AddUnitToMainUsesSection(NewUnitName, NewUnitInFile,
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveUnitFromAllUsesSections(Code: TCodeBuffer;
  const AnUnitName: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemoveUnitFromAllUsesSections A ',Code.Filename,' UnitName=',AnUnitName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemoveUnitFromAllUsesSections(UpperCaseStr(AnUnitName),
                SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindUsedUnitFiles(Code: TCodeBuffer;
  var MainUsesSection: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindUsedUnitFiles A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindUsedUnitFiles(MainUsesSection);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindUsedUnitFiles(Code: TCodeBuffer;
  var MainUsesSection, ImplementationUsesSection: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindUsedUnitFiles A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindUsedUnitFiles(MainUsesSection,
                                           ImplementationUsesSection);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindUsedUnitNames(Code: TCodeBuffer;
  var MainUsesSection, ImplementationUsesSection: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindUsedUnitNames A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindUsedUnitNames(MainUsesSection,
                                           ImplementationUsesSection);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindMissingUnits(Code: TCodeBuffer;
  var MissingUnits: TStrings; FixCase: boolean;
  SearchImplementation: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindMissingUnits A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindMissingUnits(MissingUnits,FixCase,
                                        SearchImplementation,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindDelphiProjectUnits(Code: TCodeBuffer;
  var FoundInUnits, MissingInUnits, NormalUnits: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDelphiProjectUnits A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindDelphiProjectUnits(FoundInUnits,
                                                MissingInUnits, NormalUnits);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindDelphiPackageUnits(Code: TCodeBuffer;
  var FoundInUnits, MissingInUnits, NormalUnits: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDelphiPackageUnits A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindDelphiProjectUnits(FoundInUnits,
                                              MissingInUnits, NormalUnits,true);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CommentUnitsInUsesSections(Code: TCodeBuffer;
  MissingUnits: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CommentUnitsInUsesSections A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.CommentUnitsInUsesSections(MissingUnits,
                                                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindUnitCaseInsensitive(Code: TCodeBuffer;
  var AnUnitName, AnUnitInFilename: string): string;
begin
  Result:='';
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindUnitCaseInsensitive A ',Code.Filename,' AnUnitName="',AnUnitName,'"',' AnUnitInFilename="',AnUnitInFilename,'"');
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindUnitCaseInsensitive(AnUnitName,AnUnitInFilename);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindUnitSource(Code: TCodeBuffer; const AnUnitName,
  AnUnitInFilename: string): TCodeBuffer;
begin
  Result:=nil;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindUnit A ',Code.Filename,' AnUnitName="',AnUnitName,'" AnUnitInFilename="',AnUnitInFilename,'"');
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindUnitSource(AnUnitName,AnUnitInFilename,false);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindLFMFileName(Code: TCodeBuffer): string;
var LinkIndex: integer;
  CurCode: TCodeBuffer;
  Ext: string;
begin
  Result:='';
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindLFMFileName A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    LinkIndex:=-1;
    CurCode:=FCurCodeTool.FindNextIncludeInInitialization(LinkIndex);
    while (CurCode<>nil) do begin
      if UpperCaseStr(ExtractFileExt(CurCode.Filename))='.LRS' then begin
        Result:=CurCode.Filename;
        Ext:=ExtractFileExt(Result);
        Result:=copy(Result,1,length(Result)-length(Ext))+'.lfm';
        exit;
      end;
      CurCode:=FCurCodeTool.FindNextIncludeInInitialization(LinkIndex);
    end;
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.CheckLFM(UnitCode, LFMBuf: TCodeBuffer;
  var LFMTree: TLFMTree; RootMustBeClassInIntf, ObjectsMustExists: boolean
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CheckLFM A ',UnitCode.Filename,' ',LFMBuf.Filename);
  {$ENDIF}
  if not InitCurCodeTool(UnitCode) then exit;
  try
    Result:=FCurCodeTool.CheckLFM(LFMBuf,LFMTree,OnFindDefinePropertyForContext,
                                  RootMustBeClassInIntf,ObjectsMustExists);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindNextResourceFile(Code: TCodeBuffer;
  var LinkIndex: integer): TCodeBuffer;
begin
  Result:=nil;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindNextResourceFile A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindNextIncludeInInitialization(LinkIndex);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddLazarusResourceHeaderComment(Code: TCodeBuffer;
  const CommentText: string): boolean;
begin
  Result:=false;
  if not InitResourceTool then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddLazarusResourceHeaderComment A ',Code.Filename,' CommentText=',CommentText);
  {$ENDIF}
  try
    Result:=GetResourceTool.AddLazarusResourceHeaderComment(Code,
      '{ '+CommentText+' }'+SourceChangeCache.BeautifyCodeOptions.LineEnd
      +SourceChangeCache.BeautifyCodeOptions.LineEnd);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindLazarusResource(Code: TCodeBuffer;
  const ResourceName: string): TAtomPosition;
begin
  Result.StartPos:=-1;
  if not InitResourceTool then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindLazarusResource A ',Code.Filename,' ResourceName=',ResourceName);
  {$ENDIF}
  try
    Result:=GetResourceTool.FindLazarusResource(Code,ResourceName,-1);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddLazarusResource(Code: TCodeBuffer;
  const ResourceName, ResourceData: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddLazarusResource A ',Code.Filename,' ResourceName=',ResourceName,' ',dbgs(length(ResourceData)));
  {$ENDIF}
  if not InitResourceTool then exit;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddLazarusResource B ');
  {$ENDIF}
  try
    Result:=GetResourceTool.AddLazarusResource(Code,ResourceName,ResourceData);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveLazarusResource(Code: TCodeBuffer;
  const ResourceName: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemoveLazarusResource A ',Code.Filename,' ResourceName=',ResourceName);
  {$ENDIF}
  if not InitResourceTool then exit;
  try
    Result:=GetResourceTool.RemoveLazarusResource(Code,ResourceName);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameMainInclude(Code: TCodeBuffer;
  const NewFilename: string; KeepPath: boolean): boolean;
var
  LinkIndex: integer;
  OldIgnoreMissingIncludeFiles: boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenameMainInclude A ',Code.Filename,' NewFilename=',NewFilename,' KeepPath=',dbgs(KeepPath));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    OldIgnoreMissingIncludeFiles:=
      FCurCodeTool.Scanner.IgnoreMissingIncludeFiles;
    FCurCodeTool.Scanner.IgnoreMissingIncludeFiles:=true;
    LinkIndex:=-1;
    if FCurCodeTool.FindNextIncludeInInitialization(LinkIndex)=nil then exit;
    Result:=FCurCodeTool.RenameInclude(LinkIndex,NewFilename,KeepPath,
                       SourceChangeCache);
    FCurCodeTool.Scanner.IgnoreMissingIncludeFiles:=
      OldIgnoreMissingIncludeFiles;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameIncludeDirective(Code: TCodeBuffer;
  LinkIndex: integer; const NewFilename: string; KeepPath: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenameIncludeDirective A ',Code.Filename,' NewFilename=',NewFilename,' KeepPath=',dbgs(KeepPath));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameInclude(LinkIndex,NewFilename,KeepPath,
                                       SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

procedure TCodeToolManager.DefaultFindDefinePropertyForContext(Sender: TObject;
  const ClassContext, AncestorClassContext: TFindContext; LFMNode: TLFMTreeNode;
  const IdentName: string; var IsDefined: boolean);
var
  PersistentClassName: String;
  AncestorClassName: String;
begin
  if Assigned(OnFindDefineProperty) then begin
    PersistentClassName:=ClassContext.Tool.ExtractClassName(
                                                       ClassContext.Node,false);
    AncestorClassName:='';
    if AncestorClassContext.Tool<>nil then
     AncestorClassName:=AncestorClassContext.Tool.ExtractClassName(
                                               AncestorClassContext.Node,false);
    OnFindDefineProperty(ClassContext.Tool,
                         PersistentClassName,AncestorClassName,IdentName,
                         IsDefined);
  end;
end;

function TCodeToolManager.FindCreateFormStatement(Code: TCodeBuffer;
  StartPos: integer;
  const AClassName, AVarName: string;
  out Position: integer): integer;
// 0=found, -1=not found, 1=found, but wrong classname
var PosAtom: TAtomPosition;
begin
  Result:=-1;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindCreateFormStatement A ',Code.Filename,' StartPos=',dbgs(StartPos),' ',AClassName,':',AVarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindCreateFormStatement(StartPos,UpperCaseStr(AClassName),
                 UpperCaseStr(AVarName),PosAtom);
    if Result<>-1 then
      Position:=PosAtom.StartPos;
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddCreateFormStatement(Code: TCodeBuffer;
  const AClassName, AVarName: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddCreateFormStatement A ',Code.Filename,' ',AClassName,':',AVarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.AddCreateFormStatement(AClassName,AVarName,
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveCreateFormStatement(Code: TCodeBuffer;
  const AVarName: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemoveCreateFormStatement A ',Code.Filename,' ',AVarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemoveCreateFormStatement(UpperCaseStr(AVarName),
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ChangeCreateFormStatement(Code: TCodeBuffer;
  const OldClassName, OldVarName: string; const NewClassName,
  NewVarName: string; OnlyIfExists: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ChangeCreateFormStatement A ',Code.Filename,
    ' ',OldVarName+':',OldClassName,' -> ',NewVarName+':',NewClassName,
    ' OnlyIfExists=',dbgs(OnlyIfExists));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ChangeCreateFormStatement(-1,OldClassName,OldVarName,
                    NewClassName,NewVarName,OnlyIfExists,
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ListAllCreateFormStatements(
  Code: TCodeBuffer): TStrings;
begin
  Result:=nil;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ListAllCreateFormStatements A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ListAllCreateFormStatements;
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.SetAllCreateFromStatements(Code: TCodeBuffer; 
  List: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.SetAllCreateFromStatements A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.SetAllCreateFromStatements(List,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.GetApplicationTitleStatement(Code: TCodeBuffer;
  var Title: string): boolean;
var
  StartPos, StringConstStartPos, EndPos: integer;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.GetApplicationTitleStatement A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindApplicationTitleStatement(StartPos,
                                                    StringConstStartPos,EndPos);
    if StartPos=0 then ;
    Result:=FCurCodeTool.GetApplicationTitleStatement(StringConstStartPos,
                                                      EndPos,Title);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.SetApplicationTitleStatement(Code: TCodeBuffer;
  const NewTitle: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.SetApplicationTitleStatement A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.SetApplicationTitleStatement(NewTitle,
                                                      SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveApplicationTitleStatement(Code: TCodeBuffer
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemoveApplicationTitleStatement A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemoveApplicationTitleStatement(SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameForm(Code: TCodeBuffer; const OldFormName,
  OldFormClassName: string; const NewFormName, NewFormClassName: string
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenameForm A ',Code.Filename,
    ' OldFormName=',OldFormName,' OldFormClassName=',OldFormClassName,
    ' NewFormName=',NewFormName,' NewFormClassName=',NewFormClassName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameForm(OldFormName,OldFormClassName,
                                NewFormName,NewFormClassName,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindFormAncestor(Code: TCodeBuffer;
  const FormClassName: string; var AncestorClassName: string;
  DirtySearch: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindFormAncestor A ',Code.Filename,' ',FormClassName);
  {$ENDIF}
  AncestorClassName:='';
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindFormAncestor(UpperCaseStr(FormClassName),
                                          AncestorClassName);
  except
    on e: Exception do Result:=HandleException(e);
  end;
  if (not Result) and DirtySearch then begin
    AncestorClassName:=FindClassAncestorName(Code.Source,FormClassName);
    Result:=AncestorClassName<>'';
  end;
end;

function TCodeToolManager.CompleteComponent(Code: TCodeBuffer;
  AComponent, AncestorComponent: TComponent): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.CompleteComponent A ',Code.Filename,' ',AComponent.Name,':',AComponent.ClassName,' ',dbgsName(AncestorComponent));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.CompleteComponent(AComponent,AncestorComponent,
                                           SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.PublishedVariableExists(Code: TCodeBuffer;
  const AClassName, AVarName: string; ErrorOnClassNotFound: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.PublishedVariableExists A ',Code.Filename,' ',AClassName,':',AVarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindPublishedVariable(UpperCaseStr(AClassName),
                 UpperCaseStr(AVarName),ErrorOnClassNotFound)<>nil;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.AddPublishedVariable(Code: TCodeBuffer;
  const AClassName, VarName, VarType: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.AddPublishedVariable A ',Code.Filename,' ',AClassName,':',VarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.AddPublishedVariable(UpperCaseStr(AClassName),
                      VarName,VarType,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemovePublishedVariable(Code: TCodeBuffer;
  const AClassName, AVarName: string; ErrorOnClassNotFound: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RemovePublishedVariable A ',Code.Filename,' ',AClassName,':',AVarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemovePublishedVariable(UpperCaseStr(AClassName),
               UpperCaseStr(AVarName),ErrorOnClassNotFound,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenamePublishedVariable(Code: TCodeBuffer;
  const AClassName, OldVariableName, NewVarName, VarType: shortstring;
  ErrorOnClassNotFound: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.RenamePublishedVariable A ',Code.Filename,' ',AClassName,' OldVar=',OldVariableName,' NewVar=',NewVarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenamePublishedVariable(UpperCaseStr(AClassName),
               UpperCaseStr(OldVariableName),NewVarName,VarType,
               ErrorOnClassNotFound,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindDanglingComponentEvents(Code: TCodeBuffer;
  const AClassName: string; RootComponent: TComponent;
  ExceptionOnClassNotFound, SearchInAncestors: boolean;
  out ListOfPInstancePropInfo: TFPList
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.FindDanglingComponentEvents A ',Code.Filename,' ',AClassName);
  {$ENDIF}
  ListOfPInstancePropInfo:=nil;
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindDanglingComponentEvents(AClassName,RootComponent,
                              ExceptionOnClassNotFound,SearchInAncestors,
                              ListOfPInstancePropInfo);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.HasInterfaceRegisterProc(Code: TCodeBuffer;
  var HasRegisterProc: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.HasInterfaceRegisterProc A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.HasInterfaceRegisterProc(HasRegisterProc);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ConvertDelphiToLazarusSource(Code: TCodeBuffer;
  AddLRSCode: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeToolManager.ConvertDelphiToLazarusSource A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ConvertDelphiToLazarusSource(AddLRSCode,
                                                      SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.DoOnFindUsedUnit(SrcTool: TFindDeclarationTool;
  const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
begin
  if Assigned(OnSearchUsedUnit) then
    Result:=OnSearchUsedUnit(SrcTool.MainFilename,
                             TheUnitName,TheUnitInFilename)
  else
    Result:=nil;
end;

function TCodeToolManager.DoOnGetSrcPathForCompiledUnit(Sender: TObject;
  const AFilename: string): string;
begin
  if CompareFileExt(AFilename,'.ppu',false)=0 then
    Result:=GetPPUSrcPathForDirectory(ExtractFilePath(AFilename))
  else if CompareFileExt(AFilename,'.ppw',false)=0 then
    Result:=GetPPWSrcPathForDirectory(ExtractFilePath(AFilename))
  else if CompareFileExt(AFilename,'.dcu',false)=0 then
    Result:=GetDCUSrcPathForDirectory(ExtractFilePath(AFilename));
  if Result='' then
    Result:=GetCompiledSrcPathForDirectory(ExtractFilePath(AFilename));
end;

function TCodeToolManager.OnInternalGetMethodName(const AMethod: TMethod;
  CheckOwner: TObject): shortstring;
begin
  if Assigned(OnGetMethodName) then
    Result:=OnGetMethodName(AMethod,CheckOwner)
  else if (AMethod.Data=nil) or (AMethod.Code=nil) then
    Result:=''
  else if (CheckOwner<>nil) and (TObject(AMethod.Data)<>CheckOwner) then
    Result:=''
  else
    Result:=TObject(AMethod.Data).MethodName(AMethod.Code);
end;

function TCodeToolManager.OnParserProgress(Tool: TCustomCodeTool): boolean;
begin
  Result:=true;
  if not FAbortable then exit;
  if not Assigned(OnCheckAbort) then exit;
  Result:=not OnCheckAbort();
end;

function TCodeToolManager.OnScannerProgress(Sender: TLinkScanner): boolean;
begin
  Result:=true;
  if not FAbortable then exit;
  if not Assigned(OnCheckAbort) then exit;
  Result:=not OnCheckAbort();
end;

function TCodeToolManager.OnScannerGetInitValues(Code: Pointer;
  out AChangeStep: integer): TExpressionEvaluator;
begin
  Result:=nil;
  AChangeStep:=DefineTree.ChangeStep;
  if Code=nil then exit;
  //DefineTree.WriteDebugReport;
  if not TCodeBuffer(Code).IsVirtual then
    Result:=DefineTree.GetDefinesForDirectory(
      ExtractFilePath(TCodeBuffer(Code).Filename),false)
  else
    Result:=DefineTree.GetDefinesForVirtualDirectory;
end;

procedure TCodeToolManager.OnDefineTreeReadValue(Sender: TObject;
  const VariableName: string; var Value: string; var Handled: boolean);
begin
  Handled:=GlobalValues.IsDefined(VariableName);
  if Handled then
    Value:=GlobalValues[VariableName];
  //DebugLn('[TCodeToolManager.OnDefineTreeReadValue] Name="',VariableName,'" = "',Value,'"');
end;

procedure TCodeToolManager.OnGlobalValuesChanged;
begin
  DefineTree.ClearCache;
end;

procedure TCodeToolManager.SetCheckFilesOnDisk(NewValue: boolean);
begin
  if NewValue=FCheckFilesOnDisk then exit;
  FCheckFilesOnDisk:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.CheckFilesOnDisk:=NewValue;
end;

procedure TCodeToolManager.SetCompleteProperties(const AValue: boolean);
begin
  if CompleteProperties=AValue then exit;
  FCompleteProperties:=AValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.CompleteProperties:=AValue;
end;

procedure TCodeToolManager.SetIndentSize(NewValue: integer);
begin
  if NewValue=FIndentSize then exit;
  FIndentSize:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.IndentSize:=NewValue;
  SourceChangeCache.BeautifyCodeOptions.Indent:=NewValue;
end;

procedure TCodeToolManager.SetTabWidth(const AValue: integer);
begin
  if FTabWidth=AValue then exit;
  FTabWidth:=AValue;
  SourceChangeCache.BeautifyCodeOptions.TabWidth:=AValue;
end;

procedure TCodeToolManager.SetVisibleEditorLines(NewValue: integer);
begin
  if NewValue=FVisibleEditorLines then exit;
  FVisibleEditorLines:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.VisibleEditorLines:=NewValue;
end;

procedure TCodeToolManager.SetJumpCentered(NewValue: boolean);
begin
  if NewValue=FJumpCentered then exit;
  FJumpCentered:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.JumpCentered:=NewValue;
end;

procedure TCodeToolManager.SetCursorBeyondEOL(NewValue: boolean);
begin
  if NewValue=FCursorBeyondEOL then exit;
  FCursorBeyondEOL:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.CursorBeyondEOL:=NewValue;
end;

procedure TCodeToolManager.BeforeApplyingChanges(var Abort: boolean);
begin
  IncreaseChangeStep;
  if Assigned(FOnBeforeApplyChanges) then
    FOnBeforeApplyChanges(Self,Abort);
end;

procedure TCodeToolManager.AfterApplyingChanges;
begin
  // clear all codetrees of changed buffers
  if FCurCodeTool<>nil then
    FCurCodeTool.Clear;
    
  // user callback
  if Assigned(FOnAfterApplyChanges) then
    FOnAfterApplyChanges(Self);
end;

function TCodeToolManager.FindCodeToolForSource(Code: TCodeBuffer
  ): TCustomCodeTool;
var
  ANode: TAVLTreeNode;
  CurSrc, SearchedSrc: Pointer;
begin
  ANode:=FSourceTools.Root;
  SearchedSrc:=Pointer(Code);
  while (ANode<>nil) do begin
    CurSrc:=Pointer(TCustomCodeTool(ANode.Data).Scanner.MainCode);
    if CurSrc>SearchedSrc then
      ANode:=ANode.Left
    else if CurSrc<SearchedSrc then
      ANode:=ANode.Right
    else begin
      Result:=TCustomCodeTool(ANode.Data);
      exit;
    end;
  end;
  Result:=nil;
end;

procedure TCodeToolManager.SetError(Code: TCodeBuffer; Line, Column: integer;
  const TheMessage: string);
begin
  FErrorMsg:=TheMessage;
  FErrorCode:=Code;
  FErrorLine:=Line;
  FErrorColumn:=Column;
  FErrorTopLine:=FErrorLine;
  AdjustErrorTopLine;
  WriteError;
end;

function TCodeToolManager.GetCodeToolForSource(Code: TCodeBuffer;
  GoToMainCode, ExceptionOnError: boolean): TCustomCodeTool;
// return a codetool for the source
begin
  Result:=nil;
  if Code=nil then begin
    if ExceptionOnError then
      raise Exception.Create('TCodeToolManager.GetCodeToolForSource '
        +'internal error: Code=nil');
    exit;
  end;
  if GoToMainCode then
    Code:=GetMainCode(Code);
  Result:=FindCodeToolForSource(Code);
  if Result=nil then begin
    CreateScanner(Code);
    if Code.Scanner=nil then begin
      if ExceptionOnError then
        raise ECodeToolManagerError.CreateFmt(ctsNoScannerFound,[Code.Filename]);
      exit;
    end;
    Result:=TCodeTool.Create;
    Result.Scanner:=Code.Scanner;
    FSourceTools.Add(Result);
    TCodeTool(Result).OnGetCodeToolForBuffer:=@OnGetCodeToolForBuffer;
    TCodeTool(Result).OnGetDirectoryCache:=@OnGetDirectoryCache;
    TCodeTool(Result).OnFindUsedUnit:=@DoOnFindUsedUnit;
    TCodeTool(Result).OnGetSrcPathForCompiledUnit:=@DoOnGetSrcPathForCompiledUnit;
    TCodeTool(Result).OnGetMethodName:=@OnInternalGetMethodName;
    Result.OnSetGlobalWriteLock:=@OnToolSetWriteLock;
    Result.OnGetGlobalWriteLockInfo:=@OnToolGetWriteLockInfo;
    TCodeTool(Result).OnParserProgress:=@OnParserProgress;
  end;
  with TCodeTool(Result) do begin
    AdjustTopLineDueToComment:=Self.AdjustTopLineDueToComment;
    AddInheritedCodeToOverrideMethod:=Self.AddInheritedCodeToOverrideMethod;
    CompleteProperties:=Self.CompleteProperties;
  end;
  Result.CheckFilesOnDisk:=FCheckFilesOnDisk;
  Result.IndentSize:=FIndentSize;
  Result.VisibleEditorLines:=FVisibleEditorLines;
  Result.JumpCentered:=FJumpCentered;
  Result.CursorBeyondEOL:=FCursorBeyondEOL;
end;

function TCodeToolManager.FindDirectivesToolForSource(Code: TCodeBuffer
  ): TDirectivesTool;
var
  ANode: TAVLTreeNode;
  CurSrc, SearchedSrc: Pointer;
begin
  ANode:=FDirectivesTools.Root;
  SearchedSrc:=Pointer(Code);
  while (ANode<>nil) do begin
    CurSrc:=Pointer(TDirectivesTool(ANode.Data).Code);
    if CurSrc>SearchedSrc then
      ANode:=ANode.Left
    else if CurSrc<SearchedSrc then
      ANode:=ANode.Right
    else begin
      Result:=TDirectivesTool(ANode.Data);
      exit;
    end;
  end;
  Result:=nil;
end;

procedure TCodeToolManager.ClearCurDirectivesTool;
begin
  ClearError;
  FCurDirectivesTool:=nil;
end;

function TCodeToolManager.InitCurDirectivesTool(Code: TCodeBuffer): boolean;
begin
  Result:=false;
  ClearCurDirectivesTool;
  FCurDirectivesTool:=TDirectivesTool(GetDirectivesToolForSource(Code,true));
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.InitCurDirectivesTool] ',Code.Filename,' ',dbgs(Code.SourceLength));
  {$ENDIF}
  Result:=true;
end;

function TCodeToolManager.GetDirectivesToolForSource(Code: TCodeBuffer;
  ExceptionOnError: boolean): TCompilerDirectivesTree;
begin
  Result:=FindDirectivesToolForSource(Code);
  if Result=nil then begin
    Result:=TDirectivesTool.Create;
    Result.Code:=Code;
    FDirectivesTools.Add(Result);
  end;
  Result.NestedComments:=GetNestedCommentsFlagForFile(Code.Filename);
end;

procedure TCodeToolManager.SetAbortable(const AValue: boolean);
begin
  if FAbortable=AValue then exit;
  FAbortable:=AValue;
end;

procedure TCodeToolManager.SetAddInheritedCodeToOverrideMethod(
  const AValue: boolean);
begin
  if FAddInheritedCodeToOverrideMethod=AValue then exit;
  FAddInheritedCodeToOverrideMethod:=AValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.AddInheritedCodeToOverrideMethod:=AValue;
end;

function TCodeToolManager.OnGetCodeToolForBuffer(Sender: TObject;
  Code: TCodeBuffer; GoToMainCode: boolean): TFindDeclarationTool;
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.OnGetCodeToolForBuffer]'
    ,' Sender=',TCustomCodeTool(Sender).MainFilename
    ,' Code=',Code.Filename);
  {$ENDIF}
  Result:=TFindDeclarationTool(GetCodeToolForSource(Code,GoToMainCode,true));
end;

function TCodeToolManager.OnGetDirectoryCache(const ADirectory: string
  ): TCTDirectoryCache;
begin
  Result:=DirectoryCachePool.GetCache(ADirectory,true,true);
end;

procedure TCodeToolManager.ActivateWriteLock;
begin
  if FWriteLockCount=0 then begin
    // start a new write lock
    if FWriteLockStep<>$7fffffff then
      inc(FWriteLockStep)
    else
      FWriteLockStep:=-$7fffffff;
    SourceCache.GlobalWriteLockIsSet:=true;
    SourceCache.GlobalWriteLockStep:=FWriteLockStep;
  end;
  inc(FWriteLockCount);
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.ActivateWriteLock] FWriteLockCount=',dbgs(FWriteLockCount),' FWriteLockStep=',dbgs(FWriteLockStep));
  {$ENDIF}
end;

procedure TCodeToolManager.DeactivateWriteLock;
begin
  if FWriteLockCount>0 then begin
    dec(FWriteLockCount);
    if FWriteLockCount=0 then begin
      // end the write lock
      SourceCache.GlobalWriteLockIsSet:=false;
    end;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeToolManager.DeactivateWriteLock] FWriteLockCount=',dbgs(FWriteLockCount),' FWriteLockStep=',dbgs(FWriteLockStep));
  {$ENDIF}
end;

procedure TCodeToolManager.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
end;

procedure TCodeToolManager.OnToolGetWriteLockInfo(out WriteLockIsSet: boolean;
  out WriteLockStep: integer);
begin
  WriteLockIsSet:=FWriteLockCount>0;
  WriteLockStep:=FWriteLockStep;
//DebugLn(' FWriteLockCount=',FWriteLockCount,' FWriteLockStep=',FWriteLockStep);
end;

function TCodeToolManager.GetResourceTool: TResourceCodeTool;
begin
  if FResourceTool=nil then FResourceTool:=TResourceCodeTool.Create;
  Result:=FResourceTool;
end;

function TCodeToolManager.GetOwnerForCodeTreeNode(ANode: TCodeTreeNode
  ): TObject;
var
  AToolNode: TAVLTreeNode;
  CurTool: TCustomCodeTool;
  RootCodeTreeNode: TCodeTreeNode;
  CurDirTool: TCompilerDirectivesTree;
begin
  Result:=nil;
  if ANode=nil then exit;
  RootCodeTreeNode:=ANode.GetRoot;
  
  // search in codetools
  AToolNode:=FSourceTools.FindLowest;
  while (AToolNode<>nil) do begin
    CurTool:=TCustomCodeTool(AToolNode.Data);
    if (CurTool.Tree<>nil) and (CurTool.Tree.Root=RootCodeTreeNode) then begin
      Result:=CurTool;
      exit;
    end;
    AToolNode:=FSourceTools.FindSuccessor(AToolNode);
  end;
  
  // search in directivestools
  AToolNode:=FDirectivesTools.FindLowest;
  while (AToolNode<>nil) do begin
    CurDirTool:=TCompilerDirectivesTree(AToolNode.Data);
    if (CurDirTool.Tree<>nil) and (CurDirTool.Tree.Root=RootCodeTreeNode) then
    begin
      Result:=CurDirTool;
      exit;
    end;
    AToolNode:=FDirectivesTools.FindSuccessor(AToolNode);
  end;
end;

function TCodeToolManager.DirectoryCachePoolGetString(const ADirectory: string;
  const AStringType: TCTDirCacheString): string;
begin
  case AStringType of
  ctdcsUnitPath: Result:=GetUnitPathForDirectory(ADirectory,false);
  ctdcsSrcPath: Result:=GetSrcPathForDirectory(ADirectory,false);
  ctdcsIncludePath: Result:=GetIncludePathForDirectory(ADirectory,false);
  ctdcsCompleteSrcPath: Result:=GetCompleteSrcPathForDirectory(ADirectory,false);
  ctdcsUnitLinks: Result:=GetUnitLinksForDirectory(ADirectory,false);
  ctdcsFPCUnitPath: Result:=GetFPCUnitPathForDirectory(ADirectory,false);
  else RaiseCatchableException('');
  end;
end;

function TCodeToolManager.DirectoryCachePoolFindVirtualFile(
  const Filename: string): string;
var
  Code: TCodeBuffer;
begin
  Result:='';
  if (Filename='') or (System.Pos(PathDelim,Filename)>0) then
    exit;
  Code:=FindFile(Filename);
  if Code<>nil then
    Result:=Code.Filename;
end;

procedure TCodeToolManager.OnToolSetWriteLock(Lock: boolean);
begin
  if Lock then ActivateWriteLock else DeactivateWriteLock;
end;

function TCodeToolManager.ConsistencyCheck: integer;
// 0 = ok
begin
  try
    Result:=0;
    if FCurCodeTool<>nil then begin
      Result:=FCurCodeTool.ConsistencyCheck;
      if Result<>0 then begin
        dec(Result,10000);  exit;
      end;
    end;
    Result:=DefinePool.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,20000);  exit;
    end;
    Result:=DefineTree.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,30000);  exit;
    end;
    Result:=SourceCache.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,40000);  exit;
    end;
    Result:=GlobalValues.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,50000);  exit;
    end;
    Result:=SourceChangeCache.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,60000);  exit;
    end;
    Result:=FSourceTools.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,70000);  exit;
    end;
    Result:=FDirectivesTools.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,80000);  exit;
    end;
  finally
    if (Result<>0) and (FCatchExceptions=false) then
      raise Exception.CreateFmt(ctsTCodeToolManagerConsistencyCheck, [Result]);
  end;
  Result:=0;
end;

procedure TCodeToolManager.WriteDebugReport(WriteTool,
  WriteDefPool, WriteDefTree, WriteCache, WriteGlobalValues: boolean);
begin
  DebugLn('[TCodeToolManager.WriteDebugReport] Consistency=',dbgs(ConsistencyCheck));
  if FCurCodeTool<>nil then begin
    if WriteTool then
      FCurCodeTool.WriteDebugTreeReport
    else
      DebugLn('  FCurCodeTool.ConsistencyCheck=',dbgs(FCurCodeTool.ConsistencyCheck));
  end;
  if WriteDefPool then
    DefinePool.WriteDebugReport
  else
    DebugLn('  DefinePool.ConsistencyCheck=',dbgs(DefinePool.ConsistencyCheck));
  if WriteDefTree then
    DefineTree.WriteDebugReport
  else
    DebugLn('  DefineTree.ConsistencyCheck=',dbgs(DefineTree.ConsistencyCheck));
  if WriteCache then
    SourceCache.WriteDebugReport
  else
    DebugLn('  SourceCache.ConsistencyCheck=',dbgs(SourceCache.ConsistencyCheck));
  if WriteGlobalValues then
    GlobalValues.WriteDebugReport
  else
    DebugLn('  GlobalValues.ConsistencyCheck=',dbgs(GlobalValues.ConsistencyCheck));
end;

//-----------------------------------------------------------------------------

initialization
  CodeToolBoss:=TCodeToolManager.Create;
  OnFindOwnerOfCodeTreeNode:=@GetOwnerForCodeTreeNode;


finalization
  {$IFDEF CTDEBUG}
  DebugLn('codetoolmanager.pas - finalization');
  {$ENDIF}
  OnFindOwnerOfCodeTreeNode:=nil;
  CodeToolBoss.Free;
  CodeToolBoss:=nil;
  {$IFDEF CTDEBUG}
  DebugLn('codetoolmanager.pas - finalization finished');
  {$ENDIF}

end.


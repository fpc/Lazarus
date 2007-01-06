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
    TFindDeclarationTool enhances the TPascalReaderTool with the ability
    to find the source position or code tree node of a declaration.


  ToDo:
    - ignore errors behind cursor (implemented, not tested)
    - find declaration in dead code (started)
    - high type expression evaluation
      (i.e. at the moment: integer+integer=longint
                   wanted: integer+integer=integer)
    - caching for procs
    - multi pass find declaration (i.e. searching with timeout)
    - Get and Set property access parameter lists
    - make @Proc context sensitive (started, but not complete)
    - operator overloading
    - ppu, ppw, dcu files
    - many things, search for 'ToDo'
}
unit FindDeclarationTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

// activate for debugging:

// mem check
{ $DEFINE MEM_CHECK}

// verbosity
{ $DEFINE CTDEBUG}
{ $DEFINE ShowTriedFiles}
{ $DEFINE ShowTriedContexts}
{ $DEFINE ShowTriedBaseContexts}
{ $DEFINE ShowTriedParentContexts}
{ $DEFINE ShowTriedIdentifiers}
{ $DEFINE ShowTriedUnits}
{ $DEFINE ShowExprEval}
{ $DEFINE ShowFoundIdentifier}
{ $DEFINE ShowInterfaceCache}
{ $DEFINE ShowNodeCache}
{ $DEFINE ShowBaseTypeCache}
{ $DEFINE ShowCacheDependencies}
{ $DEFINE ShowCollect}
{ $DEFINE ShowProcSearch}

{$IFDEF CTDEBUG}{$DEFINE DebugPrefix}{$ENDIF}
{$IFDEF ShowTriedIdentifiers}{$DEFINE DebugPrefix}{$ENDIF}
{$IFDEF ShowTriedContexts}{$DEFINE DebugPrefix}{$ENDIF}

// new features
{ $DEFINE DisableIgnoreErrorAfter}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, CustomCodeTool,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, DirectoryCacher,
  AVL_Tree, PascalParserTool,
  PascalReaderTool, FileProcs, DefineTemplates, FindDeclarationCache;

type
  TFindDeclarationTool = class;
  
  //----------------------------------------------------------------------------
  // variable atoms

  TVariableAtomType = (
    vatNone,             // undefined
    vatSpace,            // empty or space
    vatIdentifier,       // an identifier
    vatPreDefIdentifier, // an identifier with special meaning to the compiler
    vatPoint,            // .
    vatAS,               // AS keyword
    vatINHERITED,        // INHERITED keyword
    vatUp,               // ^
    vatRoundBracketOpen, // (
    vatRoundBracketClose,// )
    vatEdgedBracketOpen, // [
    vatEdgedBracketClose,// ]
    vatAddrOp,           // @
    vatKeyword           // other keywords
    );
    
const
  // for nicer debugging output
  VariableAtomTypeNames: array[TVariableAtomType] of string =
    ('<None>',
     'Space',
     'Ident',
     'PreDefIdent',
     'Point',
     'AS',
     'INHERITED',
     'Up^ ',
     'Bracket(',
     'Bracket)',
     'Bracket[',
     'Bracket]',
     'AddrOperator@ ',
     'Keyword'
     );
     
type
  //----------------------------------------------------------------------------
  // searchpath delimiter is semicolon
  TOnGetSearchPath = function(Sender: TObject): string of object;
  TOnGetSrcPathForCompiledUnit =
    function(Sender: TObject; const Filename: string): string of object;

  //----------------------------------------------------------------------------
  // flags/states for searching
  TFindDeclarationFlag = (
    fdfSearchInAncestors,   // if context is a class, search also in
                            //    ancestors/interfaces
    fdfSearchInParentNodes, // if identifier not found in current context,
                            //    proceed in prior nodes on same lvl and parents
    fdfIgnoreCurContextNode,// skip context and proceed in prior/parent context
    fdfIgnoreUsedUnits,     // stay in current source
    fdfSearchForward,       // instead of searching in prior nodes, search in
                            //    next nodes (successors)
                            
    fdfExceptionOnNotFound, // raise exception if identifier not found
                            //    predefined identifiers will not raise
    fdfExceptionOnPredefinedIdent,// raise an exception even if the identifier
                            // is an predefined identifier
                            
    fdfIgnoreClassVisibility,//find inaccessible private+protected fields

    fdfIgnoreMissingParams, // found proc fits, even if parameters are missing
    fdfOnlyCompatibleProc,  // incompatible procs are ignored
    fdfIgnoreOverloadedProcs,// ignore param lists and take the first proc found
    
    fdfFindVariable,        // do not search for the base type of a variable,
                            //   instead return the variable declaration
    fdfFunctionResult,      // if function is found, return result type
    
    fdfCollect,             // return every reachable identifier
    fdfTopLvlResolving,     // set, when searching for an identifier of the
                            //   top lvl variable
    fdfDoNotCache           // result will not be cached
    );
  TFindDeclarationFlags = set of TFindDeclarationFlag;
  
const
  fdfGlobals = [fdfExceptionOnNotFound, fdfTopLvlResolving];
  fdfGlobalsSameIdent = fdfGlobals+[fdfExceptionOnPredefinedIdent,
                fdfIgnoreMissingParams, fdfIgnoreUsedUnits, fdfDoNotCache,
                fdfOnlyCompatibleProc, fdfSearchInAncestors, fdfCollect];
  fdfDefaultForExpressions = [fdfSearchInParentNodes, fdfSearchInAncestors,
                              fdfExceptionOnNotFound];

  // for nicer output
  FindDeclarationFlagNames: array[TFindDeclarationFlag] of string = (
    'fdfSearchInAncestors',
    'fdfSearchInParentNodes',
    'fdfIgnoreCurContextNode',
    'fdfIgnoreUsedUnits',
    'fdfSearchForward',
    'fdfExceptionOnNotFound',
    'fdfExceptionOnPredefinedIdent',
    'fdfIgnoreClassVisibility',
    'fdfIgnoreMissingParams',
    'fdfOnlyCompatibleProc',
    'fdfIgnoreOverloadedProcs',
    'fdfFindVariable',
    'fdfFunctionResult',
    'fdfCollect',
    'fdfTopLvlResolving',
    'fdfDoNotCache'
  );

type
  // flags/states for result
  TFoundDeclarationFlag = (
    fodDoNotCache
    );
  TFoundDeclarationFlags = set of TFoundDeclarationFlag;
  
const
  FoundDeclarationFlagNames: array[TFoundDeclarationFlag] of string = (
      'fodDoNotCache'
    );

  //----------------------------------------------------------------------------
type
  TFindDeclarationParams = class;
  
  TFindContext = record
    Node: TCodeTreeNode;
    Tool: TFindDeclarationTool;
  end;
  PFindContext = ^TFindContext;
  
const
  CleanFindContext: TFindContext = (Node:nil; Tool:nil);
  
type
  //----------------------------------------------------------------------------
  { TExpressionTypeDesc describes predefined types
    The Freepascal compiler can automatically convert them
  }
  TExpressionTypeDesc = (
    xtNone,        // undefined
    xtContext,     // a node
    xtChar,        // char
    xtWideChar,    // widechar
    xtReal,        // real
    xtSingle,      // single
    xtDouble,      // double
    xtExtended,    // extended
    xtCurrency,    // currency
    xtComp,        // comp
    xtInt64,       // int64
    xtCardinal,    // cardinal
    xtQWord,       // qword
    xtBoolean,     // boolean
    xtByteBool,    // bytebool
    xtLongBool,    // longbool
    xtString,      // string
    xtAnsiString,  // ansistring
    xtShortString, // shortstring
    xtWideString,  // widestring
    xtPChar,       // pchar
    xtPointer,     // pointer
    xtFile,        // file
    xtText,        // text
    xtConstOrdInteger,// enum, number, integer
    xtConstString, // string, string constant, char constant
    xtConstReal,   // real number
    xtConstSet,    // [] set
    xtConstBoolean,// true, false
    xtLongint,     // longint
    xtLongWord,    // longword
    xtWord,        // word
    xtSmallInt,    // smallint
    xtShortInt,    // shortint
    xtByte,        // byte
    xtCompilerFunc,// SUCC, PREC, LOW, HIGH, ORD, LENGTH, COPY (1.1)
    xtVariant,     // variant
    xtNil          // nil  = pointer, class, procedure, method, ...
    );
  // Do not use this: TExpressionTypeDescs = set of TExpressionTypeDesc;
  // There are too many enums, so the set would be big and slow
  
const
  ExpressionTypeDescNames: array[TExpressionTypeDesc] of string = (
    'None',
    'Context',
    'Char',
    'WideChar',
    'Real',
    'Single',
    'Double',
    'Extended',
    'Currency',
    'Comp',
    'Int64',
    'Cardinal',
    'QWord',
    'Boolean',
    'ByteBool',
    'LongBool',
    'String',
    'AnsiString',
    'ShortString',
    'WideString',
    'PChar',
    'Pointer',
    'File',
    'TextFile',
    'ConstOrdInt',
    'ConstString',
    'ConstReal',
    'ConstSet',
    'ConstBoolean',
    'LongInt',
    'LongWord',
    'Word',
    'SmallInt',
    'ShortInt',
    'Byte',
    'CompilerFunc',
    'Variant',
    'Nil'
  );

  xtAllTypes = [Low(TExpressionTypeDesc)..High(TExpressionTypeDesc)]-[xtNone];
  xtAllPredefinedTypes = xtAllTypes-[xtContext];
  xtAllIntegerTypes = [xtInt64, xtQWord, xtConstOrdInteger, xtLongint,
                       xtLongWord, xtWord, xtCardinal, xtSmallInt, xtShortInt,
                       xtByte];
  xtAllBooleanTypes = [xtBoolean, xtByteBool, xtLongBool];
  xtAllRealTypes = [xtReal, xtConstReal, xtSingle, xtDouble, xtExtended,
                    xtCurrency, xtComp];
  xtAllStringTypes = [xtConstString, xtShortString, xtString, xtAnsiString];
  xtAllWideStringTypes = [xtConstString, xtWideString];
  xtAllPointerTypes = [xtPointer, xtNil];

  xtAllStringCompatibleTypes = xtAllStringTypes+[xtChar];
  xtAllWideStringCompatibleTypes = xtAllWideStringTypes+[xtWideChar,xtChar];

  xtAllIntegerConvertibles = xtAllIntegerTypes;
  xtAllRealConvertibles = xtAllRealTypes+xtAllIntegerTypes;
  xtAllStringConvertibles = xtAllStringCompatibleTypes+[xtPChar];
  xtAllWideStringConvertibles = xtAllWideStringCompatibleTypes+[xtPChar];
  xtAllBooleanConvertibles = xtAllBooleanTypes+[xtConstBoolean];
  xtAllPointerConvertibles = xtAllPointerTypes+[xtPChar];

type
  { TExpressionType is used for compatibility check
    A compatibility check is done by comparing two TExpressionType

    if Desc = xtConstSet, SubDesc contains the type of the set
    if Context.Node<>nil, it contains the corresponding codetree node
  }
  TExpressionType = record
    Desc: TExpressionTypeDesc;
    SubDesc: TExpressionTypeDesc;
    Context: TFindContext;
  end;
  PExpressionType = ^TExpressionType;
  
const
  CleanExpressionType : TExpressionType =
    (Desc:xtNone; SubDesc:xtNone; Context:(Node:nil; Tool:nil));

type
  //----------------------------------------------------------------------------
  // TTypeCompatibility is the result of a compatibility check
  TTypeCompatibility = (
    tcExact,        // exactly same type
    tcCompatible,   // type can be auto converted
    tcIncompatible  // type is incompatible
    );
  TTypeCompatibilityList = ^TTypeCompatibility;
    
const
  TypeCompatibilityNames: array[TTypeCompatibility] of string = (
       'Exact',
       'Compatible', // convertable, but not allowed for var params
       'Incompatible'
     );

type
  //----------------------------------------------------------------------------
  // TExprTypeList is used for compatibility checks of whole parameter lists
  TExprTypeList = class
  private
    FCapacity: integer;
    procedure SetCapacity(const AValue: integer);
  protected
    procedure Grow;
  public
    Count: integer;
    Items: ^TExpressionType;
    procedure Add(const ExprType: TExpressionType);
    procedure AddFirst(const ExprType: TExpressionType);
    property Capacity: integer read FCapacity write SetCapacity;
    destructor Destroy; override;
    function AsString: string;
  end;
  
  //----------------------------------------------------------------------------
  // TFoundProc is used for comparing overloaded procs
  TFoundProc = record
    // the expression input list, which should fit into the searched proc
    ExprInputList: TExprTypeList;
    // the best proc found till now
    Context: TFindContext;
    // if the proc was already compared (CacheValid=true), then some of the
    // compatibility check results are cached.
    CacheValid: boolean;
    ProcCompatibility: TTypeCompatibility;
    ParamCompatibilityList: TTypeCompatibilityList;
  end;
  PFoundProc = ^TFoundProc;

  //---------------------------------------------------------------------------
type
  TIdentifierFoundResult = (ifrProceedSearch, ifrAbortSearch, ifrSuccess);

const
  IdentifierFoundResultNames: array[TIdentifierFoundResult] of shortstring =
    ('ProceedSearch', 'AbortSearch', 'Success');

type
  TOnIdentifierFound = function(Params: TFindDeclarationParams;
    const FoundContext: TFindContext): TIdentifierFoundResult of object;
  TOnFindUsedUnit = function(SrcTool: TFindDeclarationTool;
    const TheUnitName, TheUnitInFilename: string): TCodeBuffer of object;
  TOnGetCodeToolForBuffer = function(Sender: TObject;
    Code: TCodeBuffer; GoToMainCode: boolean): TFindDeclarationTool of object;
  TOnGetDirectoryCache = function(const ADirectory: string
                                  ): TCTDirectoryCache of object;

  TFindDeclarationInput = record
    Flags: TFindDeclarationFlags;
    Identifier: PChar;
    ContextNode: TCodeTreeNode;
    OnIdentifierFound: TOnIdentifierFound;
    IdentifierTool: TFindDeclarationTool;
    FoundProc: PFoundProc;
  end;

  { TFindDeclarationParams }

  TFindDeclarationParams = class(TObject)
  public
    // input parameters:
    Flags: TFindDeclarationFlags;
    Identifier: PChar;
    ContextNode: TCodeTreeNode;
    OnIdentifierFound: TOnIdentifierFound;
    IdentifierTool: TFindDeclarationTool;
    FoundProc: PFoundProc;
    // global params
    OnTopLvlIdentifierFound: TOnIdentifierFound;
    // results:
    NewNode: TCodeTreeNode;
    NewCleanPos: integer;
    NewCodeTool: TFindDeclarationTool;
    NewPos: TCodeXYPosition;
    NewTopLine: integer;
    NewFlags: TFoundDeclarationFlags;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Save(var Input: TFindDeclarationInput);
    procedure Load(var Input: TFindDeclarationInput);
    procedure SetResult(const AFindContext: TFindContext);
    procedure SetResult(ANewCodeTool: TFindDeclarationTool;
                        ANewNode: TCodeTreeNode);
    procedure SetResult(ANewCodeTool: TFindDeclarationTool;
                        ANewNode: TCodeTreeNode;  ANewCleanPos: integer);
    procedure SetResult(NodeCacheEntry: PCodeTreeNodeCacheEntry);
    procedure SetIdentifier(NewIdentifierTool: TFindDeclarationTool;
                NewIdentifier: PChar; NewOnIdentifierFound: TOnIdentifierFound);
    procedure SetFirstFoundProc(const ProcContext: TFindContext);
    procedure ChangeFoundProc(const ProcContext: TFindContext;
                              ProcCompatibility: TTypeCompatibility;
                              ParamCompatibilityList: TTypeCompatibilityList);
    procedure ConvertResultCleanPosToCaretPos;
    procedure ClearResult(CopyCacheFlags: boolean);
    procedure ClearInput;
    procedure ClearFoundProc;
    procedure WriteDebugReport;
  end;
  
  
  //----------------------------------------------------------------------------
  // TFindDeclarationTool is source based and can therefore search for more
  // than declarations:
  TFindSmartFlag = (
    fsfIncludeDirective, // search for include file
    fsfFindMainDeclaration, // stop if already on a declaration
    fsfSearchSourceName // if searching for a unit name, return the source name node
    );
  TFindSmartFlags = set of TFindSmartFlag;
  
  TFindSrcStartType = (
    fsstIdentifier
    );

  TFindDeclarationListFlag = (
    fdlfWithoutEmptyProperties, // omit properties without type and attributes
    fdlfWithoutForwards         // omit foward classes and procedures
    );
  TFindDeclarationListFlags = set of TFindDeclarationListFlag;
  
const
  AllFindSmartFlags = [fsfIncludeDirective];

type
  //----------------------------------------------------------------------------
  ECodeToolUnitNotFound = class(ECodeToolFileNotFound)
  end;

  //----------------------------------------------------------------------------

  { TFindDeclarationTool }

  TFindDeclarationTool = class(TPascalReaderTool)
  private
    FAdjustTopLineDueToComment: boolean;
    FDirectoryCache: TCTDirectoryCache;
    FInterfaceIdentifierCache: TInterfaceIdentifierCache;
    FOnFindUsedUnit: TOnFindUsedUnit;
    FOnGetCodeToolForBuffer: TOnGetCodeToolForBuffer;
    FOnGetDirectoryCache: TOnGetDirectoryCache;
    FOnGetSrcPathForCompiledUnit: TOnGetSrcPathForCompiledUnit;
    FOnGetUnitSourceSearchPath: TOnGetSearchPath;
    FFirstNodeCache: TCodeTreeNodeCache;
    FLastNodeCachesGlobalWriteLockStep: integer;
    FRootNodeCache: TCodeTreeNodeCache;
    FFirstBaseTypeCache: TBaseTypeCache;
    FDependentCodeTools: TAVLTree;// the codetools, that depend on this codetool
    FDependsOnCodeTools: TAVLTree;// the codetools, that this codetool depends on
    FClearingDependentNodeCaches: boolean;
    FCheckingNodeCacheDependencies: boolean;
    {$IFDEF DebugPrefix}
    DebugPrefix: string;
    procedure IncPrefix;
    procedure DecPrefix;
    {$ENDIF}
    function FindDeclarationInUsesSection(UsesNode: TCodeTreeNode;
      CleanPos: integer;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function IsIncludeDirectiveAtPos(CleanPos, CleanCodePosInFront: integer;
      var IncludeCode: TCodeBuffer): boolean;
    function FindEnumInContext(Params: TFindDeclarationParams): boolean;
    // sub methods for FindIdentifierInContext
    function DoOnIdentifierFound(Params: TFindDeclarationParams;
      FoundNode: TCodeTreeNode): TIdentifierFoundResult;
    function FindIdentifierInProcContext(ProcContextNode: TCodeTreeNode;
      Params: TFindDeclarationParams): TIdentifierFoundResult;
    function FindIdentifierInClassOfMethod(ProcContextNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInWithVarContext(WithVarNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInAncestors(ClassNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInUsesSection(UsesNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInHiddenUsedUnits(
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInUsedUnit(const AnUnitName: string;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInRecordCase(RecordCaseNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
  protected
    WordIsPredefinedIdentifier: TKeyWordFunctionList;
    procedure BeginParsing(DeleteNodes, OnlyInterfaceNeeded: boolean); override;
  protected
    // node caches
    procedure DoDeleteNodes; override;
    function NodeCacheGlobalWriteLockStepDidNotChange: boolean;
    function CheckDependsOnNodeCaches: boolean;
    procedure ClearNodeCaches(Force: boolean);
    procedure ClearDependentNodeCaches;
    procedure ClearDependsOnToolRelationships;
    procedure AddToolDependency(DependOnTool: TFindDeclarationTool);
    function CreateNewNodeCache(Node: TCodeTreeNode): TCodeTreeNodeCache;
    function CreateNewBaseTypeCache(Node: TCodeTreeNode): TBaseTypeCache;
    procedure CreateBaseTypeCaches(NodeStack: PCodeTreeNodeStack;
      const Result: TFindContext);
    function GetNodeCache(Node: TCodeTreeNode;
      CreateIfNotExists: boolean): TCodeTreeNodeCache;
    procedure AddResultToNodeCaches(
      StartNode, EndNode: TCodeTreeNode; SearchedForward: boolean;
      Params: TFindDeclarationParams; SearchRangeFlags: TNodeCacheEntryFlags);
  protected
    // expressions, operands, variables
    function GetCurrentAtomType: TVariableAtomType;
    function FindEndOfVariable(StartPos: integer;
      ExceptionIfNoVariableStart: boolean): integer;
    function FindStartOfVariable(EndPos: integer): integer;
    function FindExpressionTypeOfVariable(StartPos, EndPos: integer;
      Params: TFindDeclarationParams): TExpressionType;
    function FindEndOfExpression(StartPos: integer): integer;
    function ConvertNodeToExpressionType(Node: TCodeTreeNode;
      Params: TFindDeclarationParams): TExpressionType;
    function ReadOperandTypeAtCursor(
      Params: TFindDeclarationParams): TExpressionType;
    function FindExpressionTypeOfPredefinedIdentifier(StartPos: integer;
      Params: TFindDeclarationParams): TExpressionType;
    function CalculateBinaryOperator(LeftOperand, RightOperand: TExpressionType;
      BinaryOperator: TAtomPosition;
      Params: TFindDeclarationParams): TExpressionType;
    function GetParameterNode(Node: TCodeTreeNode): TCodeTreeNode;
    function GetExpressionTypeOfTypeIdentifier(
      Params: TFindDeclarationParams): TExpressionType;
    function FindTermTypeAsString(TermAtom: TAtomPosition;
      CursorNode: TCodeTreeNode; Params: TFindDeclarationParams): string;
  protected
    function CheckSrcIdentifier(Params: TFindDeclarationParams;
      const FoundContext: TFindContext): TIdentifierFoundResult;
    function FindDeclarationOfIdentAtParam(
      Params: TFindDeclarationParams): boolean;
    function IdentifierIsDefined(IdentAtom: TAtomPosition;
      ContextNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
    function FindContextNodeAtCursor(
      Params: TFindDeclarationParams): TFindContext;
    function FindClassOfMethod(ProcNode: TCodeTreeNode;
      Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
    function FindForwardIdentifier(Params: TFindDeclarationParams;
      var IsForward: boolean): boolean;
    function FindExpressionResultType(Params: TFindDeclarationParams;
      StartPos, EndPos: integer): TExpressionType;
    function FindCodeToolForUsedUnit(UnitNameAtom,
      UnitInFileAtom: TAtomPosition;
      ExceptionOnNotFound: boolean): TFindDeclarationTool;
    function FindCodeToolForUsedUnit(const AnUnitName, AnUnitInFilename: string;
      ExceptionOnNotFound: boolean): TFindDeclarationTool;
    function FindIdentifierInInterface(AskingTool: TFindDeclarationTool;
      Params: TFindDeclarationParams): boolean;
    function CompareNodeIdentifier(Node: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function GetInterfaceNode: TCodeTreeNode;
    function CompatibilityList1IsBetter(List1, List2: TTypeCompatibilityList;
      ListCount: integer): boolean;
    function IsParamExprListCompatibleToNodeList(
      FirstTargetParameterNode: TCodeTreeNode;
      SourceExprParamList: TExprTypeList; IgnoreMissingParameters: boolean;
      Params: TFindDeclarationParams;
      CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
    function IsParamNodeListCompatibleToParamNodeList(FirstTargetParameterNode,
      FirstSourceParameterNode: TCodeTreeNode;
      Params: TFindDeclarationParams;
      CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
    function CreateParamExprListFromStatement(StartPos: integer;
      Params: TFindDeclarationParams): TExprTypeList;
    function CreateParamExprListFromProcNode(ProcNode: TCodeTreeNode;
      Params: TFindDeclarationParams): TExprTypeList;
    function ContextIsDescendOf(
      const DescendContext, AncestorContext: TFindContext;
      Params: TFindDeclarationParams): boolean;
    function IsCompatible(TargetNode: TCodeTreeNode;
      const ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
    function IsCompatible(TargetType, ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
    function IsBaseCompatible(const TargetType, ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
    function CheckParameterSyntax(CursorNode: TCodeTreeNode;
      CleanCursorPos: integer; out ParameterAtom, ProcNameAtom: TAtomPosition;
      out ParameterIndex: integer): boolean;
  protected
    function OpenCodeToolForUnit(UnitNameAtom, UnitInFileAtom: TAtomPosition;
      ExceptionOnNotFound: boolean): TFindDeclarationTool;
    function CheckDirectoryCache: boolean;
  public
    procedure BuildTree(OnlyInterfaceNeeded: boolean); override;
    destructor Destroy; override;
    function ConsistencyCheck: integer; override;

    function FindDeclaration(const CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindMainDeclaration(const CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindDeclaration(const CursorPos: TCodeXYPosition;
      SearchSmartFlags: TFindSmartFlags;
      var NewTool: TFindDeclarationTool; var NewNode: TCodeTreeNode;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindDeclarationInInterface(const Identifier: string;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindDeclarationWithMainUsesSection(const Identifier: string;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindDeclarationOfPropertyPath(const PropertyPath: string;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindDeclarationNodeInInterface(const Identifier: string;
      BuildTheTree: Boolean): TCodeTreeNode;

    function FindMainUsesSection(UseContainsSection: boolean = false): TCodeTreeNode;
    function FindImplementationUsesSection: TCodeTreeNode;

    function FindUnitSource(const AnUnitName,
      AnUnitInFilename: string; ExceptionOnNotFound: boolean): TCodeBuffer;
    function FindUnitCaseInsensitive(var AnUnitName,
                                     AnUnitInFilename: string): string;
    procedure GatherUnitAndSrcPath(var UnitPath, CompleteSrcPath: string);
    function SearchUnitInUnitLinks(const TheUnitName: string): string;
    
    function FindSmartHint(const CursorPos: TCodeXYPosition): string;
    
    function BaseTypeOfNodeHasSubIdents(ANode: TCodeTreeNode): boolean;
    function FindBaseTypeOfNode(Params: TFindDeclarationParams;
      Node: TCodeTreeNode): TFindContext;
      
    function FindDeclarationAndOverload(const CursorPos: TCodeXYPosition;
      out ListOfPCodeXYPosition: TFPList;
      Flags: TFindDeclarationListFlags): boolean;
    function FindClassAndAncestors(ClassNode: TCodeTreeNode;
      out ListOfPFindContext: TFPList): boolean;
    function FindContextClassAndAncestors(const CursorPos: TCodeXYPosition;
      var ListOfPFindContext: TFPList): boolean;
    function FindReferences(const CursorPos: TCodeXYPosition;
      SkipComments: boolean; out ListOfPCodeXYPosition: TFPList): boolean;
      
    function CleanPosIsDeclarationIdentifier(CleanPos: integer;
                                 Node: TCodeTreeNode): boolean;

    function FindIdentifierInContext(Params: TFindDeclarationParams): boolean;
    function FindAncestorOfClass(ClassNode: TCodeTreeNode;
      Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
    function FindNthParameterNode(Node: TCodeTreeNode;
                                  ParameterIndex: integer): TCodeTreeNode;
    function GetFirstParameterNode(Node: TCodeTreeNode): TCodeTreeNode;
    function IsParamNodeListCompatibleToExprList(
      TargetExprParamList: TExprTypeList;
      FirstSourceParameterNode: TCodeTreeNode;
      Params: TFindDeclarationParams;
      CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;

    function JumpToNode(ANode: TCodeTreeNode;
        var NewPos: TCodeXYPosition; var NewTopLine: integer;
        IgnoreJumpCentered: boolean): boolean;
    function JumpToCleanPos(NewCleanPos, NewTopLineCleanPos,
        NewBottomLineCleanPos: integer;
        var NewPos: TCodeXYPosition; var NewTopLine: integer;
        IgnoreJumpCentered: boolean): boolean;
    function NodeIsForwardDeclaration(Node: TCodeTreeNode): boolean;

    property InterfaceIdentifierCache: TInterfaceIdentifierCache
                                                 read FInterfaceIdentifierCache;
    property OnGetUnitSourceSearchPath: TOnGetSearchPath
               read FOnGetUnitSourceSearchPath write FOnGetUnitSourceSearchPath;
    property OnFindUsedUnit: TOnFindUsedUnit
                                     read FOnFindUsedUnit write FOnFindUsedUnit;
    property OnGetCodeToolForBuffer: TOnGetCodeToolForBuffer
                     read FOnGetCodeToolForBuffer write FOnGetCodeToolForBuffer;
    property OnGetDirectoryCache: TOnGetDirectoryCache read FOnGetDirectoryCache
                                                     write FOnGetDirectoryCache;
    property OnGetSrcPathForCompiledUnit: TOnGetSrcPathForCompiledUnit
           read FOnGetSrcPathForCompiledUnit write fOnGetSrcPathForCompiledUnit;
    property AdjustTopLineDueToComment: boolean
               read FAdjustTopLineDueToComment write FAdjustTopLineDueToComment;
    property DirectoryCache: TCTDirectoryCache read FDirectoryCache;
  end;

function ExprTypeToString(const ExprType: TExpressionType): string;
function CreateExpressionType(const Desc, SubDesc: TExpressionTypeDesc;
  const Context: TFindContext): TExpressionType;

function CreateFindContext(NewTool: TFindDeclarationTool;
  NewNode: TCodeTreeNode): TFindContext;
function CreateFindContext(Params: TFindDeclarationParams): TFindContext;
function CreateFindContext(BaseTypeCache: TBaseTypeCache): TFindContext;
function FindContextAreEqual(const Context1, Context2: TFindContext): boolean;
function CompareFindContexts(const Context1, Context2: PFindContext): integer;
procedure AddFindContext(var ListOfPFindContext: TFPList;
  const NewContext: TFindContext);
function IndexOfFindContext(var ListOfPFindContext: TFPList;
  const AContext: PFindContext): integer;
procedure FreeListOfPFindContext(var ListOfPFindContext: TFPList);

function ListOfPFindContextToStr(const ListOfPFindContext: TFPList): string;
function DbgsFC(const Context: TFindContext): string;

function PredefinedIdentToExprTypeDesc(Identifier: PChar): TExpressionTypeDesc;
function FindDeclarationFlagsAsString(
  const Flags: TFindDeclarationFlags): string;
function FoundDeclarationFlagsAsString(
  const Flags: TFoundDeclarationFlags): string;


implementation


function FindDeclarationFlagsAsString(
  const Flags: TFindDeclarationFlags): string;
var Flag: TFindDeclarationFlag;
begin
  Result:='';
  for Flag:=Low(TFindDeclarationFlag) to High(TFindDeclarationFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+', ';
      Result:=Result+FindDeclarationFlagNames[Flag];
    end;
  end;
end;

function FoundDeclarationFlagsAsString(
  const Flags: TFoundDeclarationFlags): string;
var Flag: TFoundDeclarationFlag;
begin
  Result:='';
  for Flag:=Low(TFoundDeclarationFlag) to High(TFoundDeclarationFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+', ';
      Result:=Result+FoundDeclarationFlagNames[Flag];
    end;
  end;
end;

function ListOfPFindContextToStr(const ListOfPFindContext: TFPList): string;
var
  Context: TFindContext;
  i: Integer;
begin
  if ListOfPFindContext=nil then
    Result:='nil'
  else begin
    Result:='';
    for i:=0 to ListOfPFindContext.Count-1 do begin
      Context:=PFindContext(ListOfPFindContext[i])^;
      Result:=Result+'  '+DbgsFC(Context)+LineEnding;
    end;
  end;
end;

function DbgsFC(const Context: TFindContext): string;
var
  CursorPos: TCodeXYPosition;
begin
  if Context.Tool=nil then
    Result:='nil'
  else begin
    Result:=Context.Tool.MainFilename;
    if Context.Node=nil then
      Result:=Result+'()'
    else begin
      Context.Tool.CleanPosToCaret(Context.Node.StartPos,CursorPos);
      Result:=Result+'(y='+dbgs(CursorPos.Y)+',x='+dbgs(CursorPos.X)+')';
    end;
  end;
end;

function PredefinedIdentToExprTypeDesc(Identifier: PChar): TExpressionTypeDesc;
begin
  // predefined identifiers
  if CompareIdentifiers(Identifier,'NIL')=0 then
    Result:=xtNil
  else if CompareIdentifiers(Identifier,'POINTER')=0 then
    Result:=xtPointer
  else if (CompareIdentifiers(Identifier,'TRUE')=0)
  or (CompareIdentifiers(Identifier,'FALSE')=0) then
    Result:=xtConstBoolean
  else if CompareIdentifiers(Identifier,'STRING')=0 then
    Result:=xtString
  else if CompareIdentifiers(Identifier,'SHORTSTRING')=0 then
    Result:=xtShortString
  else if CompareIdentifiers(Identifier,'ANSISTRING')=0 then
    Result:=xtAnsiString
  else if CompareIdentifiers(Identifier,'WIDESTRING')=0 then
    Result:=xtWideString
  else if CompareIdentifiers(Identifier,'INT64')=0 then
    Result:=xtInt64
  else if CompareIdentifiers(Identifier,'CARDINAL')=0 then
    Result:=xtCardinal
  else if CompareIdentifiers(Identifier,'QWORD')=0 then
    Result:=xtQWord
  else if CompareIdentifiers(Identifier,'BOOLEAN')=0 then
    Result:=xtBoolean
  else if CompareIdentifiers(Identifier,'BYTEBOOL')=0 then
    Result:=xtByteBool
  else if CompareIdentifiers(Identifier,'LONGBOOL')=0 then
    Result:=xtLongBool
  else if CompareIdentifiers(Identifier,'CHAR')=0 then
    Result:=xtChar
  else if CompareIdentifiers(Identifier,'WIDECHAR')=0 then
    Result:=xtWideChar
  else if CompareIdentifiers(Identifier,'REAL')=0 then
    Result:=xtReal
  else if CompareIdentifiers(Identifier,'SINGLE')=0 then
    Result:=xtSingle
  else if CompareIdentifiers(Identifier,'DOUBLE')=0 then
    Result:=xtDouble
  else if CompareIdentifiers(Identifier,'EXTENDED')=0 then
    Result:=xtExtended
  else if CompareIdentifiers(Identifier,'COMP')=0 then
    Result:=xtComp
  else if CompareIdentifiers(Identifier,'FILE')=0 then
    Result:=xtFile
  else if CompareIdentifiers(Identifier,'TEXT')=0 then
    Result:=xtText
  else if CompareIdentifiers(Identifier,'SIZEOF')=0 then
    Result:=xtConstOrdInteger
  else if CompareIdentifiers(Identifier,'ORD')=0 then
    Result:=xtConstOrdInteger
  else if CompareIdentifiers(Identifier,'VARIANT')=0 then
    Result:=xtVariant
  else if IsWordBuiltInFunc.DoIt(Identifier) then
    Result:=xtCompilerFunc

  // the delphi compiler special types
  else if CompareIdentifiers(Identifier,'CURRENCY')=0 then
    Result:=xtCurrency
  else if CompareIdentifiers(Identifier,'LONGINT')=0 then
    Result:=xtLongInt
  else if CompareIdentifiers(Identifier,'LONGWORD')=0 then
    Result:=xtLongWord
  else if CompareIdentifiers(Identifier,'WORD')=0 then
    Result:=xtWord
  else if CompareIdentifiers(Identifier,'LONGWORD')=0 then
    Result:=xtCardinal
  else if CompareIdentifiers(Identifier,'SMALLINT')=0 then
    Result:=xtSmallInt
  else if CompareIdentifiers(Identifier,'SHORTINT')=0 then
    Result:=xtShortInt
  else if CompareIdentifiers(Identifier,'BYTE')=0 then
    Result:=xtByte
  else if CompareIdentifiers(Identifier,'PCHAR')=0 then
    Result:=xtPChar
  else
    Result:=xtNone;
end;

function ExprTypeToString(const ExprType: TExpressionType): string;
var
  IdentNode: TCodeTreeNode;
begin
  Result:='Desc='+ExpressionTypeDescNames[ExprType.Desc]
         +' SubDesc='+ExpressionTypeDescNames[ExprType.SubDesc];
  if ExprType.Context.Node<>nil then begin
    Result:=Result+' Node='+ExprType.Context.Node.DescAsString;
    IdentNode:=ExprType.Context.Node;
    while (IdentNode<>nil) do begin
      if IdentNode.Desc in AllIdentifierDefinitions then begin
        Result:=Result+' Ident="'+
          ExprType.Context.Tool.ExtractIdentifier(IdentNode.StartPos)+'"';
        break;
      end;
      IdentNode:=IdentNode.Parent;
    end;
    Result:=Result+' File="'+ExprType.Context.Tool.MainFilename+'"';
  end;
end;

function CreateExpressionType(const Desc, SubDesc: TExpressionTypeDesc;
  const Context: TFindContext): TExpressionType;
begin
  Result.Desc:=Desc;
  Result.SubDesc:=SubDesc;
  Result.Context:=Context;
end;

{ TFindContext }

function CreateFindContext(NewTool: TFindDeclarationTool;
  NewNode: TCodeTreeNode): TFindContext;
begin
  Result.Node:=NewNode;
  Result.Tool:=NewTool;
end;

function CreateFindContext(Params: TFindDeclarationParams): TFindContext;
begin
  Result.Node:=Params.NewNode;
  Result.Tool:=TFindDeclarationTool(Params.NewCodeTool);
end;

function CreateFindContext(BaseTypeCache: TBaseTypeCache): TFindContext;
begin
  Result.Node:=BaseTypeCache.NewNode;
  Result.Tool:=TFindDeclarationTool(BaseTypeCache.NewTool);
end;

function FindContextAreEqual(const Context1, Context2: TFindContext): boolean;
begin
  Result:=(Context1.Tool=Context2.Tool) and (Context1.Node=Context2.Node);
end;

function CompareFindContexts(const Context1, Context2: PFindContext): integer;
begin
  if Pointer(Context1^.Tool)>Pointer(Context2^.Tool) then
    Result:=1
  else if Pointer(Context1^.Tool)<Pointer(Context2^.Tool) then
    Result:=-1
  else if Pointer(Context1^.Node)>Pointer(Context2^.Node) then
    Result:=1
  else if Pointer(Context1^.Node)<Pointer(Context2^.Node) then
    Result:=-1
  else
    Result:=0;
end;

procedure AddFindContext(var ListOfPFindContext: TFPList;
  const NewContext: TFindContext);
var
  AddContext: PFindContext;
begin
  if ListOfPFindContext=nil then ListOfPFindContext:=TFPList.Create;
  New(AddContext);
  AddContext^:=NewContext;
  ListOfPFindContext.Add(AddContext);
end;

function IndexOfFindContext(var ListOfPFindContext: TFPList;
  const AContext: PFindContext): integer;
begin
  if ListOfPFindContext=nil then
    Result:=-1
  else begin
    Result:=ListOfPFindContext.Count-1;
    while (Result>=0)
    and (CompareFindContexts(AContext,
                             PFindContext(ListOfPFindContext[Result]))<>0)
    do
      dec(Result);
  end;
end;

procedure FreeListOfPFindContext(var ListOfPFindContext: TFPList);
var
  CurContext: PFindContext;
  i: Integer;
begin
  if ListOfPFindContext=nil then exit;
  for i:=0 to ListOfPFindContext.Count-1 do begin
    CurContext:=PFindContext(ListOfPFindContext[i]);
    Dispose(CurContext);
  end;
  ListOfPFindContext.Free;
  ListOfPFindContext:=nil;
end;


{ TFindDeclarationTool }

function TFindDeclarationTool.FindDeclaration(const CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var
  NewTool: TFindDeclarationTool;
  NewNode: TCodeTreeNode;
begin
  Result:=FindDeclaration(CursorPos,AllFindSmartFlags,NewTool,NewNode,
                          NewPos,NewTopLine);
end;

function TFindDeclarationTool.FindMainDeclaration(
  const CursorPos: TCodeXYPosition; var NewPos: TCodeXYPosition;
  var NewTopLine: integer): boolean;
var
  NewTool: TFindDeclarationTool;
  NewNode: TCodeTreeNode;
begin
  Result:=FindDeclaration(CursorPos,[fsfFindMainDeclaration],NewTool,NewNode,
                          NewPos,NewTopLine);
end;

function TFindDeclarationTool.FindDeclaration(const CursorPos: TCodeXYPosition;
  SearchSmartFlags: TFindSmartFlags;
  var NewTool: TFindDeclarationTool; var NewNode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var CleanCursorPos: integer;
  CursorNode, ClassNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  DirectSearch, SkipChecks, SearchForward: boolean;

  procedure CheckIfCursorOnAForwardDefinedClass;
  begin
    if SkipChecks then exit;
    if CursorNode.Desc=ctnTypeDefinition then begin
      if (CursorNode.FirstChild<>nil)
      and (CursorNode.FirstChild.Desc in [ctnClass,ctnClassInterface])
      and ((CursorNode.FirstChild.SubDesc and ctnsForwardDeclaration)>0) then
      begin
        DirectSearch:=true;
        SearchForward:=true;
        SkipChecks:=true;
      end;
    end;
  end;

  procedure CheckIfCursorInClassNode;
  begin
    if SkipChecks then exit;
    ClassNode:=CursorNode;
    while (ClassNode<>nil)
    and (not (ClassNode.Desc in [ctnClass,ctnClassInterface]))
    do
      ClassNode:=ClassNode.Parent;
    if ClassNode<>nil then begin
      // cursor is in class/object/class interface definition
      if (ClassNode.SubDesc and ctnsForwardDeclaration)=0 then begin
        // parse class and build CodeTreeNodes for all properties/methods
        BuildSubTreeForClass(ClassNode);
        CursorNode:=FindDeepestNodeAtPos(ClassNode,CleanCursorPos,true);
        if (CursorNode.Desc in [ctnClass,ctnClassInterface])
        and (CleanCursorPos<ClassNode.FirstChild.StartPos) then begin
          // identifier is an ancestor/interface identifier
          CursorNode:=CursorNode.Parent;
          DirectSearch:=true;
          SkipChecks:=true;
        end;
      end;
    end;
  end;

  procedure CheckIfCursorInBeginNode;
  begin
    if SkipChecks then exit;
    if CursorNode.Desc=ctnBeginBlock then begin
      BuildSubTreeForBeginBlock(CursorNode);
      CursorNode:=FindDeepestNodeAtPos(CursorNode,CleanCursorPos,true);
    end;
  end;

  procedure CheckIfCursorInProcNode;
  var IsMethod: boolean;
  begin
    if SkipChecks then exit;
    if CursorNode.Desc=ctnProcedureHead then
      CursorNode:=CursorNode.Parent;
    if CursorNode.Desc=ctnProcedure then begin
      BuildSubTreeForProcHead(CursorNode);
      CursorNode:=FindDeepestNodeAtPos(CursorNode,CleanCursorPos,true);
      // check if cursor on proc name
      if (CursorNode.Desc=ctnProcedureHead)
      and (CleanCursorPos>=CursorNode.StartPos) then begin
        MoveCursorToNodeStart(CursorNode);
        ReadNextAtom;
        IsMethod:=false;
        if AtomIsIdentifier(false) then begin
          ReadNextAtom;
          if AtomIsChar('.') then begin
            ReadNextAtom;
            ReadNextAtom;
            IsMethod:=true;
          end;
        end;
        if (CurPos.StartPos>CleanCursorPos) and (not IsMethod) then begin
          // cursor on proc name
          // -> ignore proc name and search overloaded identifier
          DirectSearch:=true;
          SkipChecks:=true;
        end;
      end;
      if CursorNode.Desc=ctnProcedureHead then
        CursorNode:=CursorNode.Parent;
    end;
  end;

  procedure CheckIfCursorInPropertyNode;
  begin
    if SkipChecks then exit;
    if (CursorNode.Desc=ctnProperty) or (CursorNode.Desc=ctnGlobalProperty) then
    begin
      MoveCursorToNodeStart(CursorNode);
      if (CursorNode.Desc=ctnProperty) then
        ReadNextAtom; // read 'property'
      ReadNextAtom; // read property name
      if CleanCursorPos<CurPos.EndPos then begin
        DirectSearch:=true;
        SkipChecks:=true;
      end;
    end;
  end;
  
  function FindSourceName(ACode: TCodeBuffer): boolean;
  var
    NamePos: TAtomPosition;
  begin
    Result:=false;
    NewTool:=nil;
    NewNode:=nil;
    if Assigned(FOnGetCodeToolForBuffer) then
      NewTool:=FOnGetCodeToolForBuffer(Self,ACode,false);
    if NewTool=nil then exit;
    NewTool.BuildTree(true);
    if not NewTool.GetSourceNamePos(NamePos) then exit;
    NewNode:=NewTool.Tree.Root;
    if not NewTool.JumpToCleanPos(NamePos.StartPos,NamePos.StartPos,
                                  NamePos.StartPos,NewPos,NewTopLine,false)
    then exit;
    Result:=true;
  end;

var
  CleanPosInFront: integer;
  CursorAtIdentifier: boolean;
  IdentifierStart: PChar;
begin
  Result:=false;
  NewTool:=nil;
  NewNode:=nil;
  SkipChecks:=false;
  ActivateGlobalWriteLock;
  try
    // build code tree
    {$IFDEF CTDEBUG}
    DebugLn('TFindDeclarationTool.FindDeclaration A CursorPos=X',dbgs(CursorPos.X),',Y',dbgs(CursorPos.Y));
    {$ENDIF}
    if DirtySrc<>nil then DirtySrc.Clear;
    BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,
                  [{$IFNDEF DisableIgnoreErrorAfter}btSetIgnoreErrorPos,{$ENDIF}
                   btLoadDirtySource,btCursorPosOutAllowed]);
    {$IFDEF CTDEBUG}
    DebugLn('TFindDeclarationTool.FindDeclaration C CleanCursorPos=',dbgs(CleanCursorPos));
    {$ENDIF}
    // find CodeTreeNode at cursor
    if (Tree.Root<>nil) and (Tree.Root.StartPos<=CleanCursorPos) then begin
      CursorNode:=BuildSubTreeAndFindDeepestNodeAtPos(Tree.Root,CleanCursorPos,
                                                      true);
      if (fsfFindMainDeclaration in SearchSmartFlags)
      and CleanPosIsDeclarationIdentifier(CleanCursorPos,CursorNode)
      then begin
        NewTool:=Self;
        NewNode:=CursorNode;
        CleanCursorPos:=GetIdentStartPosition(Src,CleanCursorPos);
        Result:=JumpToCleanPos(CleanCursorPos,CleanCursorPos,CleanCursorPos,
                               NewPos,NewTopLine,false);
        exit;
      end;
      CleanPosInFront:=CursorNode.StartPos;
    end else begin
      CleanPosInFront:=1;
      CursorNode:=nil;
    end;
    if (not IsDirtySrcValid)
    and IsIncludeDirectiveAtPos(CleanCursorPos,CleanPosInFront,NewPos.Code)
    then begin
      // include directive
      NewPos.X:=1;
      NewPos.Y:=1;
      NewTopLine:=1;
      NewNode:=nil;
      NewTool:=Self;
      Result:=(fsfIncludeDirective in SearchSmartFlags);
      exit;
    end;
    if CursorNode=nil then
      // raise exception
      FindDeepestNodeAtPos(CleanCursorPos,true);
    {$IFDEF CTDEBUG}
    DebugLn('TFindDeclarationTool.FindDeclaration D CursorNode=',NodeDescriptionAsString(CursorNode.Desc),' HasChilds=',dbgs(CursorNode.FirstChild<>nil));
    {$ENDIF}
    if (not IsDirtySrcValid)
    and (CursorNode.Desc=ctnUsesSection) then begin
      // in uses section
      Result:=FindDeclarationInUsesSection(CursorNode,CleanCursorPos,
                                           NewPos,NewTopLine);
      NewNode:=nil;
      NewTool:=nil;
      if Result and (fsfSearchSourceName in SearchSmartFlags) then
        Result:=FindSourceName(NewPos.Code);
      exit;
    end;
    DirectSearch:=false;
    SearchForward:=false;
    CheckIfCursorOnAForwardDefinedClass;
    CheckIfCursorInClassNode;
    CheckIfCursorInBeginNode;
    CheckIfCursorInProcNode;
    CheckIfCursorInPropertyNode;
    // set cursor on identifier
    MoveCursorToCleanPos(CleanCursorPos);
    if IsDirtySrcValid then begin
      DirtySrc.SetCursorToIdentStartEndAtPosition;
      CursorAtIdentifier:=DirtySrc.CurPos.StartPos<DirtySrc.CurPos.EndPos;
      IdentifierStart:=DirtySrc.GetCursorSrcPos;
    end else begin
      GetIdentStartEndAtPosition(Src,CleanCursorPos,
                                 CurPos.StartPos,CurPos.EndPos);
      CursorAtIdentifier:=CurPos.StartPos<CurPos.EndPos;
      IdentifierStart:=@Src[CurPos.StartPos];
    end;
    if CursorAtIdentifier then begin
      // find declaration of identifier
      Params:=TFindDeclarationParams.Create;
      try
        Params.ContextNode:=CursorNode;
        Params.SetIdentifier(Self,IdentifierStart,@CheckSrcIdentifier);
        Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound,
                       fdfExceptionOnPredefinedIdent,
                       fdfTopLvlResolving,fdfSearchInAncestors];
        if not DirectSearch then begin
          // ToDo: DirtySrc
          Result:=FindDeclarationOfIdentAtParam(Params);
        end else begin
          Include(Params.Flags,fdfIgnoreCurContextNode);
          if SearchForward then
            Include(Params.Flags,fdfSearchForward);
          Result:=FindIdentifierInContext(Params);
        end;
        if Result then begin
        
          // adjust result for nicer position
          NewNode:=Params.NewNode;
          if (NewNode<>nil) and (NewNode.Desc=ctnProcedure)
          and (NewNode.FirstChild<>nil)
          and (NewNode.FirstChild.Desc=ctnProcedureHead) then begin
            // Instead of jumping to the procedure keyword,
            // jump to the procedure name
            Params.NewNode:=NewNode.FirstChild;
            Params.NewCleanPos:=Params.NewNode.StartPos;
          end;
            
          Params.ConvertResultCleanPosToCaretPos;
          NewNode:=Params.NewNode;
          NewTool:=Params.NewCodeTool;
          NewPos:=Params.NewPos;
          NewTopLine:=Params.NewTopLine;
          if NewPos.Code=nil then begin
            if Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then
              Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier)
            else
              MoveCursorToCleanPos(CleanCursorPos);
            Params.IdentifierTool.RaiseExceptionFmt(ctsIdentifierNotFound,
                                          [GetIdentifier(Params.Identifier)]);
          end;
        end;
      finally
        Params.Free;
      end;
    end else begin
      // find declaration of non identifier, e.g. numeric label

    end;
  finally
    ClearIgnoreErrorAfter;
    DeactivateGlobalWriteLock;
  end;
end;

function TFindDeclarationTool.FindDeclarationInInterface(
  const Identifier: string; var NewPos: TCodeXYPosition; var NewTopLine: integer
  ): boolean;
var
  Node: TCodeTreeNode;
begin
  Result:=false;
  if Identifier='' then exit;
  Node:=FindDeclarationNodeInInterface(Identifier,true);
  if Node<>nil then
    Result:=JumpToNode(Node,NewPos,NewTopLine,false);
end;

function TFindDeclarationTool.FindDeclarationWithMainUsesSection(
  const Identifier: string; var NewPos: TCodeXYPosition; var NewTopLine: integer
  ): boolean;
var
  UsesNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
begin
  Result:=false;
  if Identifier='' then exit;
  BuildTree(false);
  UsesNode:=FindMainUsesSection;
  if UsesNode=nil then exit;

  Params:=TFindDeclarationParams.Create;
  ActivateGlobalWriteLock;
  try
    Params.Flags:=[fdfExceptionOnNotFound];
    Params.SetIdentifier(Self,PChar(Pointer(Identifier)),nil);
    if FindIdentifierInUsesSection(UsesNode,Params) then begin
      if Params.NewNode=nil then exit;
      Result:=Params.NewCodeTool.JumpToNode(Params.NewNode,NewPos,
                                            NewTopLine,false);
    end;
  finally
    Params.Free;
    DeactivateGlobalWriteLock;
  end;
end;

function TFindDeclarationTool.FindDeclarationOfPropertyPath(
  const PropertyPath: string; var NewPos: TCodeXYPosition;
  var NewTopLine: integer): boolean;
// example: PropertyPath='TForm1.Font.Color'
var
  StartPos: Integer;

  function GetNextIdentifier: string;
  var
    EndPos: LongInt;
  begin
    EndPos:=StartPos;
    while (EndPos<=length(PropertyPath)) and (IsIdentChar[PropertyPath[EndPos]])
    do inc(EndPos);
    if (EndPos<=length(PropertyPath)) and (PropertyPath[EndPos]<>'.') then
      Result:=''
    else begin
      Result:=copy(PropertyPath,StartPos,EndPos-StartPos);
      StartPos:=EndPos+1;
    end;
  end;
  
var
  Params: TFindDeclarationParams;
  Identifier: String;
  IsLastProperty: Boolean;
  Context: TFindContext;
begin
  Result:=false;
  //DebugLn('TFindDeclarationTool.FindDeclarationOfPropertyPath PropertyPath="',PropertyPath,'"');
  if PropertyPath='' then exit;
  BuildTree(false);

  // first search the class/variable in the interface
  StartPos:=1;
  Identifier:=GetNextIdentifier;
  if Identifier='' then exit;
  Context.Tool:=Self;
  Context.Node:=FindDeclarationNodeInInterface(Identifier,true);
  if Context.Node=nil then begin
    //DebugLn(['TFindDeclarationTool.FindDeclarationOfPropertyPath Identifier not found in interface ',Identifier]);
    exit;
  end;
  Params:=TFindDeclarationParams.Create;
  ActivateGlobalWriteLock;
  try
    Context:=FindBaseTypeOfNode(Params,Context.Node);
    if Context.Node=nil then begin
      //DebugLn(['TFindDeclarationTool.FindDeclarationOfPropertyPath context not found']);
      exit;
    end;
    // then search the properties
    repeat
      //DebugLn('TFindDeclarationTool.FindDeclarationOfPropertyPath ',Context.Node.DescAsString);
      if (not (Context.Node.Desc in [ctnClass,ctnClassInterface,ctnRecordType]))
      then
        exit;
      Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInAncestors];
      Identifier:=GetNextIdentifier;
      //DebugLn('TFindDeclarationTool.FindDeclarationOfPropertyPath Identifier="',identifier,'"');
      if Identifier='' then exit;
      Params.SetIdentifier(Self,PChar(Pointer(Identifier)),nil);
      Params.ContextNode:=Context.Node;
      IsLastProperty:=StartPos>length(PropertyPath);
      if IsLastProperty then
        Params.Flags:=Params.Flags+[fdfFindVariable]
      else
        Params.Flags:=Params.Flags-[fdfFindVariable]+[fdfFunctionResult];
      if not Context.Tool.FindIdentifierInContext(Params) then exit;
      Context.Tool:=Params.NewCodeTool;
      Context.Node:=Params.NewNode;
      if Context.Node=nil then exit;
      if IsLastProperty then begin
        Result:=Context.Tool.JumpToNode(Context.Node,NewPos,NewTopLine,false);
        break;
      end else begin
        Context:=Context.Tool.FindBaseTypeOfNode(Params,Context.Node);
        if Context.Node=nil then exit;
      end;
     until false;
  finally
    Params.Free;
    DeactivateGlobalWriteLock;
  end;
end;

function TFindDeclarationTool.FindDeclarationNodeInInterface(
  const Identifier: string; BuildTheTree: Boolean): TCodeTreeNode;
var
  StartNode: TCodeTreeNode;
  SectionNode: TCodeTreeNode;
  Node: TCodeTreeNode;
  BestNodeIsForwardDecaration: Boolean;
  CurNodeIsForwardDeclaration: Boolean;
  BestNode: TCodeTreeNode;
begin
  Result:=nil;
  if Identifier='' then exit;
  if BuildTheTree then BuildTree(true);
  if Tree.Root=nil then exit;
  if Tree.Root.Desc=ctnUnit then
    StartNode:=FindInterfaceNode
  else
    StartNode:=Tree.Root;
  if StartNode=nil then exit;
  SectionNode:=StartNode.FirstChild;
  if SectionNode=nil then exit;
  BestNode:=nil;
  BestNodeIsForwardDecaration:=false;
  while SectionNode<>nil do begin
    if SectionNode.Desc in AllDefinitionSections then begin
      Node:=SectionNode.FirstChild;
      while Node<>nil do begin
        if Node.Desc in AllIdentifierDefinitions then begin
          if CompareSrcIdentifiers(Node.StartPos,PChar(Pointer(Identifier)))
          then begin
            CurNodeIsForwardDeclaration:=NodeIsForwardDeclaration(Node);
            if (BestNode=nil) or BestNodeIsForwardDecaration then begin
              BestNode:=Node;
              BestNodeIsForwardDecaration:=CurNodeIsForwardDeclaration;
            end;
          end;
        end;
        Node:=Node.NextBrother;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
  Result:=BestNode;
end;

function TFindDeclarationTool.FindMainUsesSection(UseContainsSection: boolean
  ): TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  if UseContainsSection then begin
    if Result.Desc<>ctnPackage then exit(nil);
    Result:=Result.FirstChild;
    while (Result<>nil) and (Result.Desc<>ctnContainsSection) do
      Result:=Result.NextBrother;
  end else begin
    if Result.Desc=ctnUnit then begin
      Result:=Result.NextBrother;
      if Result=nil then exit;
    end;
    Result:=Result.FirstChild;
    if (Result=nil) then exit;
    if (Result.Desc<>ctnUsesSection) then Result:=nil;
  end;
end;

function TFindDeclarationTool.FindImplementationUsesSection: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  while (Result<>nil) and (Result.Desc<>ctnImplementation) do
    Result:=Result.NextBrother;
  if Result=nil then exit;
  Result:=Result.FirstChild;
  if (Result=nil) then exit;
  if (Result.Desc<>ctnUsesSection) then Result:=nil;
end;

function TFindDeclarationTool.FindDeclarationInUsesSection(
  UsesNode: TCodeTreeNode; CleanPos: integer;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
  
  procedure RaiseUsesExpected;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,['"uses"',GetAtom]);
  end;
  
  procedure RaiseStrConstExpected;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsStringConstant,GetAtom]);
  end;
  
var UnitName, UnitInFilename: string;
  UnitNamePos, UnitInFilePos: TAtomPosition;
begin
  Result:=false;
  {$IFDEF ShowTriedContexts}
  DebugLn('TFindDeclarationTool.FindDeclarationInUsesSection A');
  {$ENDIF}
  // reparse uses section
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom;
  if not UpAtomIs('USES') then
    RaiseUsesExpected;
  repeat
    ReadNextAtom;  // read name
    if CurPos.StartPos>CleanPos then break;
    if AtomIsChar(';') then break;
    AtomIsIdentifier(true);
    UnitNamePos:=CurPos;
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then RaiseStrConstExpected;
      UnitInFilePos:=CurPos;
      ReadNextAtom;
    end else
      UnitInFilePos.StartPos:=-1;
    if CleanPos<UnitNamePos.EndPos then begin
      // cursor is on a unitname -> try to locate it
      UnitName:=copy(Src,UnitNamePos.StartPos,
                     UnitNamePos.EndPos-UnitNamePos.StartPos);
      if UnitInFilePos.StartPos>=1 then begin
        UnitInFilename:=copy(Src,UnitInFilePos.StartPos+1,
                     UnitInFilePos.EndPos-UnitInFilePos.StartPos-2)
      end else
        UnitInFilename:='';
      NewPos.Code:=FindUnitSource(UnitName,UnitInFilename,true);
      if NewPos.Code=nil then
        RaiseExceptionInstance(
          ECodeToolUnitNotFound.Create(Self,Format(ctsUnitNotFound,[UnitName]),
            UnitName));
      NewPos.X:=1;
      NewPos.Y:=1;
      NewTopLine:=1;
      Result:=true;
      exit;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[';',GetAtom])
  until (CurPos.StartPos>SrcLen);
  {$IFDEF ShowTriedContexts}
  DebugLn('TFindDeclarationTool.FindDeclarationInUsesSection END cursor not on unitname');
  {$ENDIF}
end;

function TFindDeclarationTool.FindUnitSource(const AnUnitName,
  AnUnitInFilename: string; ExceptionOnNotFound: boolean): TCodeBuffer;
var
  CompiledFilename: string;
  AFilename: String;
  NewUnitName: String;
  NewInFilename: String;
  NewCompiledUnitname: String;
begin
  {$IFDEF ShowTriedFiles}
  DebugLn('TFindDeclarationTool.FindUnitSource A AnUnitName="',AnUnitName,'" AnUnitInFilename="',AnUnitInFilename,'" Self="',MainFilename,'"');
  {$ENDIF}
  Result:=nil;
  if (AnUnitName='') or (Scanner=nil) or (Scanner.MainCode=nil)
  or (not (TObject(Scanner.MainCode) is TCodeBuffer))
  or (Scanner.OnLoadSource=nil)
  or (not CheckDirectoryCache) then
  begin
    RaiseException('TFindDeclarationTool.FindUnitSource Invalid Data');
  end;
  
  NewUnitName:=AnUnitName;
  NewInFilename:=AnUnitInFilename;
  AFilename:=DirectoryCache.FindUnitSourceInCompletePath(
                                               NewUnitName,NewInFilename,false);
  Result:=TCodeBuffer(Scanner.OnLoadSource(Self,AFilename,true));

  if (Result=nil) and Assigned(OnFindUsedUnit) then begin
    // no unit found
    Result:=OnFindUsedUnit(Self,AnUnitName,AnUnitInFilename);
  end;
  
  if Result=nil then begin
    // search .ppu
    NewCompiledUnitname:=AnUnitName+'.ppu';
    CompiledFilename:=DirectoryCache.FindCompiledUnitInCompletePath(
                                                     NewCompiledUnitname,false);
  end else begin
    CompiledFilename:='';
  end;

  if (Result=nil) and ExceptionOnNotFound then begin
    if CompiledFilename<>'' then begin
      // there is a compiled unit, only the source was not found
      RaiseExceptionInstance(
        ECodeToolUnitNotFound.Create(Self,
          Format(ctsSourceNotFoundUnit, [CompiledFilename]),AnUnitName));
    end else begin
      // nothing found
      RaiseExceptionInstance(
        ECodeToolUnitNotFound.Create(Self,Format(ctsUnitNotFound,[AnUnitName]),
          AnUnitName));
    end;
  end;
end;

function TFindDeclarationTool.FindUnitCaseInsensitive(var AnUnitName,
  AnUnitInFilename: string): string;
begin
  if not CheckDirectoryCache then exit;
  Result:=DirectoryCache.FindUnitSourceInCompletePath(
                                              AnUnitName,AnUnitInFilename,true);
end;

procedure TFindDeclarationTool.GatherUnitAndSrcPath(var UnitPath,
  CompleteSrcPath: string);
begin
  UnitPath:='';
  CompleteSrcPath:='';
  if not CheckDirectoryCache then exit;
  UnitPath:=DirectoryCache.Strings[ctdcsUnitPath];
  CompleteSrcPath:=DirectoryCache.Strings[ctdcsCompleteSrcPath];
  //DebugLn('TFindDeclarationTool.GatherUnitAndSrcPath UnitPath="',UnitPath,'" CompleteSrcPath="',CompleteSrcPath,'"');
end;

function TFindDeclarationTool.SearchUnitInUnitLinks(const TheUnitName: string
  ): string;
begin
  Result:='';
  if not CheckDirectoryCache then exit;
  Result:=DirectoryCache.FindUnitLink(TheUnitName);
end;

function TFindDeclarationTool.FindSmartHint(const CursorPos: TCodeXYPosition
  ): string;
var
  NewTool: TFindDeclarationTool;
  NewNode, IdentNode, TypeNode: TCodeTreeNode;
  NewPos: TCodeXYPosition;
  NewTopLine: integer;
  AbsCursorPos: integer;
  IdentStartPos, IdentEndPos: integer;
  IdentAdded: boolean;
  ClassStr: String;
  NodeStr: String;
begin
  Result:='';
  if FindDeclaration(CursorPos,AllFindSmartFlags,
    NewTool,NewNode,NewPos,NewTopLine) then
  begin
    { Examples:
        var i: integer
        /home/.../codetools/finddeclarationtools.pas(1224,7)
    }
    IdentAdded:=false;
    // identifier category and identifier
    if NewNode<>nil then begin
      case NewNode.Desc of
      ctnVarDefinition, ctnTypeDefinition, ctnConstDefinition,
      ctnEnumIdentifier:
        begin
          case NewNode.Desc of
          ctnVarDefinition: Result:=Result+'var ';
          ctnTypeDefinition: Result:=Result+'type ';
          ctnConstDefinition: Result:=Result+'const ';
          ctnEnumIdentifier: Result:=Result+'enum ';
          end;
          
          // add class name
          ClassStr := NewTool.ExtractClassName(NewNode, False);
          if ClassStr <> '' then Result := Result + ClassStr + '.';
          
          NewTool.MoveCursorToNodeStart(NewNode);
          NewTool.ReadNextAtom;
          Result:=Result+NewTool.GetAtom;
          IdentAdded:=true;
          TypeNode:=NewTool.FindTypeNodeOfDefinition(NewNode);
          if TypeNode<>nil then begin
            case TypeNode.Desc of
            ctnIdentifier, ctnClass, ctnClassInterface:
              begin
                NewTool.MoveCursorToNodeStart(TypeNode);
                NewTool.ReadNextAtom;
                Result:=Result+': '+NewTool.GetAtom;
              end;
            ctnConstant:
              begin
                NodeStr:=' = '+NewTool.ExtractNode(TypeNode,[]);
                Result:=Result+copy(NodeStr,1,50);
              end;
            end;
          end else begin
            case NewNode.Desc of
            ctnConstDefinition:
              begin
                DebugLn('TFindDeclarationTool.FindSmartHint const without subnode "',NewTool.ExtractNode(NewNode,[]),'"');
                NodeStr:=NewTool.ExtractCode(NewNode.StartPos
                                   +GetIdentLen(@NewTool.Src[NewNode.StartPos]),
                                   NewNode.EndPos,[]);
                Result:=Result+copy(NodeStr,1,50);
              end;
            end;
          end;
        end;
        
      ctnProcedure,ctnProcedureHead:
        begin

          // ToDo: ppu, ppw, dcu files

          Result:=Result+NewTool.ExtractProcHead(NewNode,
            [phpAddClassName,phpWithStart,phpWithVarModifiers,phpWithParameterNames,
             phpWithDefaultValues,phpWithResultType,phpWithOfObject]);
          IdentAdded:=true;
        end;
        
      ctnProperty,
      ctnProgram,ctnUnit,ctnPackage,ctnLibrary:
        begin
          IdentNode:=NewNode;

          // ToDo: ppu, ppw, dcu files
        
          NewTool.MoveCursorToNodeStart(IdentNode);
          NewTool.ReadNextAtom;
          Result:=Result+NewTool.GetAtom+' ';
          
          if NewNode.Desc = ctnProperty then
          begin // add class name
            ClassStr := NewTool.ExtractClassName(NewNode, False);
            if ClassStr <> '' then Result := Result + ClassStr + '.';
          end;
          
          NewTool.ReadNextAtom;
          Result:=Result+NewTool.GetAtom+' ';
          IdentAdded:=true;
        end;
        
      ctnGlobalProperty:
        begin
          IdentNode:=NewNode;

          // ToDo: ppu, ppw, dcu files

          NewTool.MoveCursorToNodeStart(IdentNode);
          Result:=Result+'property ';
          NewTool.ReadNextAtom;
          Result:=Result+NewTool.GetAtom+' ';
          IdentAdded:=true;
        end;


      else
        DebugLn('ToDo: TFindDeclarationTool.FindSmartHint ',NewNode.DescAsString);
      end;
    end;
    // read the identifier if not already done
    if not IdentAdded then begin
      CursorPos.Code.LineColToPosition(CursorPos.Y,CursorPos.X,AbsCursorPos);
      GetIdentStartEndAtPosition(CursorPos.Code.Source,
        AbsCursorPos,IdentStartPos,IdentEndPos);
      if IdentStartPos<IdentEndPos then begin
        Result:=Result+copy(CursorPos.Code.Source,IdentStartPos,IdentEndPos-IdentStartPos);
        // type

        // ToDo

        Result:=Result+' ';
      end;
    end;
    // filename
    if Result<>'' then Result:=Result+LineEnding;
    Result:=Result+NewPos.Code.Filename;
    // file position
    if NewPos.Y>=1 then begin
      Result:=Result+'('+IntToStr(NewPos.Y);
      if NewPos.X>=1 then begin
        Result:=Result+','+IntToStr(NewPos.X);
      end;
      Result:=Result+')';
    end;
  end;
end;

function TFindDeclarationTool.BaseTypeOfNodeHasSubIdents(ANode: TCodeTreeNode
  ): boolean;
var
  FindContext: TFindContext;
  Params: TFindDeclarationParams;
begin
  Result:=false;
  if (ANode=nil) then exit;
  ActivateGlobalWriteLock;
  Params:=TFindDeclarationParams.Create;
  try
    FindContext:=FindBaseTypeOfNode(Params,ANode);
    if (FindContext.Node<>nil)
    and (FindContext.Node.Desc in [ctnRecordType,ctnClass,ctnClassInterface])
    and (FindContext.Node.FirstChild<>nil)
    then
      Result:=true;
  finally
    Params.Free;
    DeactivateGlobalWriteLock;
  end;
end;

function TFindDeclarationTool.IsIncludeDirectiveAtPos(CleanPos,
  CleanCodePosInFront: integer; var IncludeCode: TCodeBuffer): boolean;
var LinkIndex, CommentStart, CommentEnd: integer;
  SrcLink: TSourceLink;
begin
  Result:=false;
  if (Scanner=nil) then exit;
  LinkIndex:=Scanner.LinkIndexAtCleanPos(CleanPos);
  if (LinkIndex<0) or (LinkIndex>=Scanner.LinkCount-1) then exit;
  SrcLink:=Scanner.Links[LinkIndex+1];
  if (SrcLink.Code=nil) or (SrcLink.Code=Scanner.Links[LinkIndex].Code) then
    exit;
  if CleanPosIsInComment(CleanPos,CleanCodePosInFront,CommentStart,CommentEnd)
  and (CommentEnd=SrcLink.CleanedPos) then begin
    IncludeCode:=TCodeBuffer(SrcLink.Code);
    Result:=true;
    exit;
  end;
end;

function TFindDeclarationTool.FindDeclarationOfIdentAtParam(
  Params: TFindDeclarationParams): boolean;
{ searches an identifier in clean code, parses code in front and after the
  identifier

  Params:
    Identifier in clean source
    ContextNode  // = DeepestNode at Cursor
    
  Result:
    true, if found

  Examples:
    A^.B().C[].Identifier
    inherited Identifier(p1,p2)
}
var
  StartPos, EndPos: integer;
  ExprType: TExpressionType;
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TFindDeclarationTool.FindDeclarationOfIdentAtParam] Identifier=',
    '"',GetIdentifier(Params.Identifier),'"',
    ' ContextNode=',NodeDescriptionAsString(Params.ContextNode.Desc),
    ' "',copy(Src,Params.ContextNode.StartPos,20),'"');
  {$ENDIF}
  Result:=false;
  // search in cleaned source
  MoveCursorToCleanPos(Params.Identifier);
  if Params.ContextNode.Desc<>ctnIdentifier then
    StartPos:=-1
  else
    StartPos:=GetHybridCursorStart;
  ReadNextAtom;
  EndPos:=CurPos.EndPos;
  ReadNextAtom;
  if CurPos.Flag=cafRoundBracketOpen then begin
    ReadTilBracketClose(true);
    EndPos:=CurPos.EndPos;
  end;
  Include(Params.Flags,fdfFindVariable);
  ExprType:=FindExpressionTypeOfVariable(StartPos,EndPos,Params);
  if (ExprType.Desc<>xtContext) then begin
    Params.SetResult(CleanFindContext);
  end;
  {$IFDEF CTDEBUG}
  DbgOut('[TFindDeclarationTool.FindDeclarationOfIdentAtParam] Ident=',
    '"',GetIdentifier(Params.Identifier),'" ');
  if Params.NewNode<>nil then
    DebugLn('Node=',Params.NewNode.DescAsString,' ',Params.NewCodeTool.MainFilename)
  else
    DebugLn('NOT FOUND');
  {$ENDIF}
  Result:=true;
end;

function TFindDeclarationTool.IdentifierIsDefined(IdentAtom: TAtomPosition;
  ContextNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
begin
  // find declaration of identifier
  Params.ContextNode:=ContextNode;
  Params.SetIdentifier(Self,@Src[IdentAtom.StartPos],nil);
  Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors,
                 fdfTopLvlResolving,fdfFindVariable,fdfIgnoreCurContextNode];
  Result:=FindIdentifierInContext(Params);
end;

function TFindDeclarationTool.FindIdentifierInContext(
  Params: TFindDeclarationParams): boolean;
{ searches an identifier in context node
  It does not care about code in front of the identifier like 'a.Identifer'.
  
  Params:
    Identifier
    ContextNode  // = DeepestNode at Cursor

  Result:
    true, if NewPos+NewTopLine valid
}
var
  LastContextNode, StartContextNode, FirstSearchedNode, LastSearchedNode,
  ContextNode: TCodeTreeNode;
  IsForward: boolean;
  OldParamFlags: TFindDeclarationFlags;
  IdentifierFoundResult: TIdentifierFoundResult;
  LastNodeCache: TCodeTreeNodeCache;
  LastCacheEntry: PCodeTreeNodeCacheEntry;
  SearchRangeFlags: TNodeCacheEntryFlags;
  NodeCacheEntryFlags: TNodeCacheEntryFlags;

  procedure InitNodesAndCacheAccess;
  
    procedure RaiseInternalError;
    begin
      RaiseException('[TFindDeclarationTool.FindIdentifierInContext] '
        +' internal error: Params.ContextNode=nil');
    end;
  
  begin
    ContextNode:=Params.ContextNode;
    if ContextNode=nil then RaiseInternalError;
    StartContextNode:=ContextNode;
    FirstSearchedNode:=nil;
    LastSearchedNode:=nil;
    SearchRangeFlags:=[];
    if fdfSearchInParentNodes in Params.Flags then
      Include(SearchRangeFlags,ncefSearchedInParents);
    if fdfSearchInAncestors in Params.Flags then
    Include(SearchRangeFlags,ncefSearchedInAncestors);
    LastNodeCache:=nil;
    LastCacheEntry:=nil;
    NodeCacheEntryFlags:=[];
    if fdfSearchInParentNodes in Params.Flags then
      Include(NodeCacheEntryFlags,ncefSearchedInParents);
    if fdfSearchInAncestors in Params.Flags then
      Include(NodeCacheEntryFlags,ncefSearchedInAncestors);
  end;

  function FindInNodeCache: boolean;
  var
    NodeCache: TCodeTreeNodeCache;
  begin
    Result:=false;
    // the node cache is identifier based
    if (fdfCollect in Params.Flags) then exit;
    
    NodeCache:=GetNodeCache(ContextNode,false);
    if (NodeCache<>LastNodeCache) then begin
      // NodeCache changed -> search nearest cache entry for the identifier
      LastNodeCache:=NodeCache;
      if NodeCache<>nil then begin
        LastCacheEntry:=NodeCache.FindNearest(Params.Identifier,
                    ContextNode.StartPos,ContextNode.EndPos,
                    not (fdfSearchForward in Params.Flags));
      end else
        LastCacheEntry:=nil;
    end;
    if (LastCacheEntry<>nil)
    and (LastCacheEntry^.CleanStartPos<=ContextNode.StartPos)
    and (LastCacheEntry^.CleanEndPos>=ContextNode.EndPos)
    and ((NodeCacheEntryFlags-LastCacheEntry^.Flags)=[])
    then begin
      // cached result found
      Params.SetResult(LastCacheEntry);
      {$IFDEF ShowNodeCache}
      DbgOut(':::: TFindDeclarationTool.FindIdentifierInContext.FindInNodeCache');
      DebugLn(' Ident=',GetIdentifier(Params.Identifier),
               ' Wanted=[',NodeCacheEntryFlagsAsString(NodeCacheEntryFlags),']',
               ' Cache=[',NodeCacheEntryFlagsAsString(LastCacheEntry^.Flags),']'
             );
      DebugLn('    ContextNode=',ContextNode.DescAsString,
              ' StartPos=',DbgS(ContextNode.StartPos),
              ' EndPos=',DbgS(ContextNode.EndPos),
              ' Self=',MainFilename);
      DebugLn('  LastCacheEntry(Pos=',DbgS(LastCacheEntry^.CleanStartPos),
              '-',DbgS(LastCacheEntry^.CleanEndPos),')');
      if (Params.NewNode<>nil) then
        DebugLn('   NewTool=',Params.NewCodeTool.MainFilename,
                ' NewNode=',Params.NewNode.DescAsString)
      else
        DebugLn('   cache says: identifier does NOT exist');
      if CompareSrcIdentifiers(Params.Identifier,'TDefineAction') then begin
        NodeCache.WriteDebugReport('NANUNANA: ');
      end;
      {$ENDIF}
      Result:=true;
    end;
  end;
  
  procedure CacheResult(Found: boolean; EndNode: TCodeTreeNode);
  begin
    if not Found then exit;
    FindIdentifierInContext:=true;
    if (FirstSearchedNode=nil) then exit;
    if ([fdfDoNotCache,fdfCollect]*Params.Flags<>[]) then exit;
    if ([fodDoNotCache]*Params.NewFlags<>[]) then exit;
    if (Params.OnIdentifierFound<>@CheckSrcIdentifier) then exit;
    if (Params.FoundProc<>nil) then exit; // do not cache proc searches
    // cache result
    if (Params.NewNode<>nil) and (Params.NewNode.Desc=ctnProcedure) then begin
      DebugLn('NOTE: TFindDeclarationTool.FindIdentifierInContext.CacheResult Node is proc');
      // ToDo:
      // The search range is from start to end of search.
      // This does not work for overloaded procs.
      // -> do not cache
      exit;
    end;
    AddResultToNodeCaches(FirstSearchedNode,EndNode,
                      fdfSearchForward in Params.Flags,Params,SearchRangeFlags);
  end;

  function CheckResult(NewResult, CallOnIdentifierFound: boolean): boolean;
  // returns: true to stop search
  //          false if search should continue
  
    procedure RaiseNotFound;
    var
      Identifier: string;
    begin
      Identifier:=GetIdentifier(Params.Identifier);
      if (Identifier='') and (Params.Identifier<>nil)
      and (Params.Identifier[0]<>#0) then begin
        Identifier:=Params.Identifier[0];
        if Identifier='[' then begin
          Params.IdentifierTool.RaiseException(ctsDefaultPropertyNotFound);
        end;
      end;
      Params.IdentifierTool.RaiseExceptionFmt(ctsIdentifierNotFound,
                                              [Identifier]);
    end;
  
  var IdentFoundResult: TIdentifierFoundResult;
  begin
    Result:=true;
    FindIdentifierInContext:=NewResult;
    {$IFDEF ShowCollect}
    if fdfCollect in Params.Flags then begin
      DebugLn('[TFindDeclarationTool.FindIdentifierInContext] COLLECT CheckResult Ident=',
      '"',GetIdentifier(Params.Identifier),'"',
      ' File="',ExtractFilename(MainFilename)+'"',
      ' Flags=[',FindDeclarationFlagsAsString(Params.Flags)+']',
      ' NewResult=',DbgS(NewResult),
      ' CallOnIdentifierFound=',DbgS(CallOnIdentifierFound));
    end;
    {$ENDIF}
    if NewResult then begin
      // identifier found
      if CallOnIdentifierFound then begin
        {
        debugln('[TFindDeclarationTool.FindIdentifierInContext] CallOnIdentifierFound Ident=',
        '"',GetIdentifier(Params.Identifier),'"',
        ' StartContext="',StartContextNode.DescAsString,'" "',copy(Src,StartContextNode.StartPos,20),'"',
        ' File="',ExtractFilename(MainFilename)+'"',
        ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
        );
        }
        IdentFoundResult:=Params.NewCodeTool.DoOnIdentifierFound(Params,
                                                                Params.NewNode);
        if (IdentFoundResult=ifrSuccess) then
          CacheResult(true,ContextNode);
        Result:=IdentFoundResult<>ifrProceedSearch;
        if IdentFoundResult<>ifrAbortSearch then exit;
      end else begin
        if fdfCollect in Params.Flags then
          Result:=false;
        CacheResult(true,ContextNode);
        exit;
      end;
    end;
    if Params.FoundProc<>nil then begin
      // there was a proc, only the search for the overloaded proc was
      // unsuccessful
      // -> return the found proc
      Params.SetResult(Params.FoundProc^.Context.Tool,
                       Params.FoundProc^.Context.Node);
      FindIdentifierInContext:=true;
      {$IFDEF ShowProcSearch}
      DebugLn('[TFindDeclarationTool.FindIdentifierInContext] PROC Search ended with only one proc:');
      Params.WriteDebugReport;
      {$ENDIF}
      exit;
    end;
    // identifier was not found
    if not (fdfExceptionOnNotFound in Params.Flags) then exit;
    if (Params.Identifier<>nil)
    and not (fdfExceptionOnPredefinedIdent in Params.Flags)
    and WordIsPredefinedIdentifier.DoIt(Params.Identifier) then begin
      Params.SetResult(nil,nil);
      exit;
    end;
    // identifier was not found and exception is wanted
    // -> raise exception
    if Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then
      Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
    RaiseNotFound;
  end;
  
  procedure MoveContextNodeToChilds;
  begin
    if ContextNode.Desc=ctnClass then begin
      // just-in-time parsing for class node
      BuildSubTreeForClass(ContextNode);
    end;
    if (ContextNode.LastChild<>nil) then begin
      if not (fdfSearchForward in Params.Flags) then begin
        RaiseLastErrorIfInFrontOfCleanedPos(ContextNode.EndPos);
        ContextNode:=ContextNode.LastChild;
      end else
        ContextNode:=ContextNode.FirstChild;
    end;
  end;
  
  function SearchInTypeVarConstPropDefinition: boolean;
  // returns: true if ok to exit
  //          false if search should continue
  begin
    Result:=false;
    //DebugLn('  Definition Identifier "',GetIdentifier(Params.Identifier),'" ',GetIdentifier(@UpperSrc[ContextNode.StartPos]));
    if (fdfCollect in Params.Flags)
    or CompareSrcIdentifiers(ContextNode.StartPos,Params.Identifier)
    then begin
      {$IFDEF ShowTriedIdentifiers}
      DebugLn('  Definition Identifier found="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      // identifier found
      Params.SetResult(Self,ContextNode);
      Result:=CheckResult(true,true);
      if not (fdfCollect in Params.Flags) then
        exit;
    end;
    // search for enums
    Params.ContextNode:=ContextNode;
    if FindEnumInContext(Params) then begin
      Result:=CheckResult(true,false);
    end;
  end;
  
  function SearchInOnBlockDefinition: boolean;
  begin
    Result:=false;
    if ContextNode.FirstChild=nil then exit;
    //debugln('SearchInOnBlockDefinition B ',GetIdentifier(@UpperSrc[ContextNode.StartPos]));
    if (fdfCollect in Params.Flags)
    or CompareSrcIdentifiers(ContextNode.FirstChild.StartPos,Params.Identifier)
    then begin
      {$IFDEF ShowTriedIdentifiers}
      DebugLn('  ON Identifier found="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      // identifier found
      Params.SetResult(Self,ContextNode.FirstChild);
      Result:=CheckResult(true,true);
      if not (fdfCollect in Params.Flags) then
        exit;
    end;
  end;

  function SearchInSourceName: boolean;
  // returns: true if ok to exit
  //          false if search should continue
  begin
    Result:=false;
    MoveCursorToNodeStart(ContextNode);
    ReadNextAtom; // read keyword
    ReadNextAtom; // read name
    if (fdfCollect in Params.Flags)
    or CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then
    begin
      // identifier found
      {$IFDEF ShowTriedIdentifiers}
      DebugLn('  Source Name Identifier found="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      Params.SetResult(Self,ContextNode,CurPos.StartPos);
      Result:=CheckResult(true,true);
      if not (fdfCollect in Params.Flags) then
        exit;
    end;
    if (not (fdfIgnoreUsedUnits in Params.Flags))
    and FindIdentifierInHiddenUsedUnits(Params) then begin
      Result:=CheckResult(true,false);
    end;
  end;
  
  function SearchInProperty: boolean;
  // returns: true if ok to exit
  //          false if search should continue
  begin
    Result:=false;
    if (fdfCollect in Params.Flags)
    or (Params.Identifier[0]<>'[') then begin
      MoveCursorToNodeStart(ContextNode);
      if (ContextNode.Desc=ctnProperty) then
        ReadNextAtom; // read keyword 'property'
      ReadNextAtom; // read name
      if (fdfCollect in Params.Flags)
      or CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then begin
        // identifier found
        {$IFDEF ShowTriedIdentifiers}
        DebugLn('  Property Identifier found="',GetIdentifier(Params.Identifier),'"');
        {$ENDIF}
        Params.SetResult(Self,ContextNode,CurPos.StartPos);
        Result:=CheckResult(true,true);
      end;
    end else begin
      // the default property is searched
      if PropertyIsDefault(ContextNode) then begin
        Params.SetResult(Self,ContextNode);
        Result:=CheckResult(true,true);
      end;
    end;
  end;
  
  function LeavingContextIsPermitted: boolean;
  begin
    Result:=true;
    if (not ContextNode.HasAsParent(StartContextNode)) then begin
      // searching in a prior node, will leave the start context
      if (not (fdfSearchInParentNodes in Params.Flags)) then begin
        // searching in any parent context is not permitted
        if not ((fdfSearchInAncestors in Params.Flags)
        and (ContextNode.Desc in [ctnClass,ctnClassInterface])) then begin
          // even searching in ancestors contexts is not permitted
          // -> there is no prior context accessible any more
          // -> identifier not found
          {$IFDEF ShowTriedContexts}
          DebugLn('[TFindDeclarationTool.FindIdentifierInContext] no prior node accessible ',
          ' ContextNode=',ContextNode.DescAsString,
          ' "',StringToPascalConst(copy(Src,ContextNode.StartPos,15)),'"'
          );
          {$ENDIF}
          ContextNode:=nil;
          Result:=false;
        end;
      end;
    end;
  end;
  
  function SearchNextNode: boolean;
  begin
    repeat
      // search for prior node
      {$IFDEF ShowTriedIdentifiers}
      //DebugLn('[TFindDeclarationTool.FindIdentifierInContext] Searching prior node of ',ContextNode.DescAsString);
      {$ENDIF}
      LastSearchedNode:=ContextNode;

      if (ContextNode.Desc in [ctnClass,ctnClassInterface])
      and (fdfSearchInAncestors in Params.Flags) then begin
        // after searching in a class definiton, search in its ancestors

        // ToDo: check for circles in ancestors
        
        OldParamFlags:=Params.Flags;
        Exclude(Params.Flags,fdfExceptionOnNotFound);
        Result:=FindIdentifierInAncestors(ContextNode,Params);
        Params.Flags:=OldParamFlags;
        if Result then begin
          FindIdentifierInContext:=true;
          Result:=false;
          exit;
        end;
      end;

      if ((not (fdfSearchForward in Params.Flags))
           and (ContextNode.PriorBrother<>nil))
      or ((fdfSearchForward in Params.Flags)
          and (ContextNode.NextBrother<>nil)
          and (ContextNode.NextBrother.Desc<>ctnImplementation)) then
      begin
        // search next in prior/next brother
        if not (fdfSearchForward in Params.Flags) then
          ContextNode:=ContextNode.PriorBrother
        else begin
          RaiseLastErrorIfInFrontOfCleanedPos(ContextNode.NextBrother.EndPos);
          ContextNode:=ContextNode.NextBrother;
        end;
        {$IFDEF ShowTriedIdentifiers}
        DebugLn('[TFindDeclarationTool.FindIdentifierInContext] Searching in Brother  ContextNode=',ContextNode.DescAsString);
        {$ENDIF}
        // it is not always allowed to search in every node on the same lvl:

        // -> test if class visibility valid
        case ContextNode.Desc of
        ctnClassPublished: break;
        ctnClassPublic:    break;
        ctnClassProtected: break;
        ctnClassPrivate:   break;
        ctnWithVariable:
          begin
            // check if StartContextNode is covered by the ContextNode
            // a WithVariable ranges from the start of its expression
            // to the end of the with statement
            {$IFDEF ShowExprEval}
            DebugLn('SearchNextNode WithVar StartContextNode.StartPos=',dbgs(StartContextNode.StartPos),
              ' ContextNode=',dbgs(ContextNode.StartPos),'-',dbgs(ContextNode.EndPos),
              ' WithStart="',StringToPascalConst(copy(Src,ContextNode.StartPos,15)),'"');
            {$ENDIF}
            if (StartContextNode.StartPos>=ContextNode.StartPos)
            and (StartContextNode.StartPos<ContextNode.EndPos) then break;
            { ELSE: this with statement does not cover the startcontext
             -> skip it
             for example:
               will be skipped:
                 with ContextNode do ;
                 with B do StartContextNode;

               will be searched:
                 with ContextNode, StartContextNode do ;
            }
          end;
        else
          break;
        end;
      end else if (ContextNode.Parent<>nil)
      and ((fdfSearchInParentNodes in Params.Flags)
        or (ContextNode.HasAsParent(StartContextNode))) then
      begin
        // search next in parent
        {$IFDEF ShowTriedParentContexts}
        DebugLn('[TFindDeclarationTool.FindIdentifierInContext] Searching in Parent ',
          ' old ContextNode=',ContextNode.DescAsString,
          ' new ContextNode=',ContextNode.Parent.DescAsString
          );
        {$ENDIF}
        ContextNode:=ContextNode.Parent;

        case ContextNode.Desc of

        ctnTypeSection, ctnVarSection, ctnConstSection, ctnResStrSection,
        ctnLabelSection, ctnPropertySection,
        ctnInterface, ctnImplementation,
        ctnClassPublished,ctnClassPublic,ctnClassProtected,ctnClassPrivate,
        ctnRecordVariant,
        ctnProcedureHead, ctnParameterList:
          // these codetreenodes build a parent-child-relationship, but
          // for pascal it is only a range, hence after searching in the
          // childs of the last node, search must continue in the childs
          // of the prior node
          ;

        ctnClass, ctnClassInterface, ctnRecordType, ctnRecordCase:
          // do not search again in this node, go on ...
          ;
          
        ctnVarDefinition, ctnConstDefinition:
          if (ContextNode.Parent<>nil)
          and (ContextNode.Parent.Desc=ctnParameterList) then begin
            // pascal allows declarations like: 'var a: a;' in parameters
            // -> skip variable and search in next context node
            ;
          end else begin
            break;
          end;

        ctnProcedure:
          begin
            Result:=FindIdentifierInClassOfMethod(ContextNode,Params);
            if Result then begin
              FindIdentifierInContext:=true;
              Result:=false;
              exit;
            end;
          end;

        else
          break;
        end;
      end else begin
        ContextNode:=nil;
        break;
      end;
    until false;
    Result:=true;
  end;
  
begin
  Result:=false;
  InitNodesAndCacheAccess;

  {$IFDEF ShowTriedContexts}
  DebugLn('[TFindDeclarationTool.FindIdentifierInContext] Start Ident=',
  '"'+GetIdentifier(Params.Identifier)+'"',
  ' Context="'+ContextNode.DescAsString+'" "'+StringToPascalConst(copy(Src,ContextNode.StartPos,20)),'"',
  ' File="'+ExtractFilename(MainFilename)+'"',
  ' Flags=['+FindDeclarationFlagsAsString(Params.Flags)+']'
  );
  {$ELSE}
    {$IFDEF ShowCollect}
    if fdfCollect in Params.Flags then begin
      DebugLn('[TFindDeclarationTool.FindIdentifierInContext] COLLECT Start Ident=',
      '"',GetIdentifier(Params.Identifier),'"',
      ' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
      ' File="',ExtractFilename(MainFilename)+'"',
      ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
      );
    end;
    {$ENDIF}
  {$ENDIF}

  //try
    // search in the Tree of this tool
    repeat
      {$IFDEF ShowTriedIdentifiers}
      DebugLn('[TFindDeclarationTool.FindIdentifierInContext] Loop Ident=',
      '"',GetIdentifier(Params.Identifier),'"',
      ' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
      ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
      );
      {$ELSE}
        {$IFDEF ShowCollect}
        if fdfCollect in Params.Flags then begin
          DebugLn('[TFindDeclarationTool.FindIdentifierInContext] COLLECT Loop Ident=',
          '"',GetIdentifier(Params.Identifier),'"',
          ' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
          ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
          );
        end;
        {$ENDIF}
      {$ENDIF}
      // search identifier in current context
      LastContextNode:=ContextNode;
      if not (fdfIgnoreCurContextNode in Params.Flags) then begin
        // search in cache
        if FindInNodeCache then begin
          if CheckResult(Params.NewNode<>nil,Params.NewNode<>nil) then
            exit;
        end;
        if FirstSearchedNode=nil then FirstSearchedNode:=ContextNode;
        LastSearchedNode:=ContextNode;
        
        case ContextNode.Desc of

        ctnTypeSection, ctnVarSection, ctnConstSection, ctnResStrSection,
        ctnLabelSection, ctnPropertySection,
        ctnInterface, ctnImplementation,
        ctnClassPublic, ctnClassPrivate, ctnClassProtected, ctnClassPublished,
        ctnClass, ctnClassInterface,
        ctnRecordType, ctnRecordVariant,
        ctnParameterList:
          // these nodes build a parent-child relationship. But in pascal
          // they just define a range and not a context.
          // -> search in all childs
          MoveContextNodeToChilds;
          
        ctnTypeDefinition, ctnVarDefinition, ctnConstDefinition,
        ctnGlobalProperty:
          if SearchInTypeVarConstPropDefinition then exit;

        ctnProcedure:
          begin
            IdentifierFoundResult:=
              FindIdentifierInProcContext(ContextNode,Params);
            if IdentifierFoundResult in [ifrAbortSearch,ifrSuccess] then begin
              if CheckResult(IdentifierFoundResult=ifrSuccess,true) then
                exit;
            end;
          end;

        ctnProcedureHead:
          begin
            BuildSubTreeForProcHead(ContextNode);
            if ContextNode.FirstChild<>nil then
              ContextNode:=ContextNode.FirstChild;
          end;

        ctnProgram, ctnPackage, ctnLibrary, ctnUnit:
          if SearchInSourceName then exit;

        ctnProperty:
          if SearchInProperty then exit;
          
        ctnUsesSection:
          begin
            if FindIdentifierInUsesSection(ContextNode,Params)
            and CheckResult(true,false) then
              exit;
          end;

        ctnWithVariable:
          begin
            if FindIdentifierInWithVarContext(ContextNode,Params)
            and CheckResult(true,false) then
              exit;
          end;
          
        ctnOnBlock:
          if SearchInOnBlockDefinition then exit;

        ctnPointerType:
          begin
            // pointer types can be forward definitions
            // -> search in both directions
            Params.ContextNode:=ContextNode.Parent;
            if CheckResult(FindForwardIdentifier(Params,IsForward),false) then
              exit;
          end;

        ctnRecordCase:
          begin
            if FindIdentifierInRecordCase(ContextNode,Params)
            and CheckResult(true,true) then
              exit;
            // search in variants
            MoveContextNodeToChilds;
          end;
          
        end;
      end else begin
        Exclude(Params.Flags,fdfIgnoreCurContextNode);
        {$IFDEF ShowTriedContexts}
        DebugLn('[TFindDeclarationTool.FindIdentifierInContext] IgnoreCurContext');
        {$ENDIF}
      end;
      if LastContextNode=ContextNode then begin
        // same context -> search in prior context
        if not LeavingContextIsPermitted then break;
        if not SearchNextNode then exit;
      end;
    until ContextNode=nil;
    
  {except
    // unexpected exception
    on E: Exception do begin
      DebugLn('*** Unexpected Exception during find declaration: ',
        E.ClassName,': ',E.Message);
      DebugLn('  MainFilename=',MainFilename);
      raise;
    end;
  end;}
  // if we are here, the identifier was not found
  if (FirstSearchedNode<>nil) and (Params.FoundProc=nil)
  and (not (fdfCollect in Params.Flags)) then begin
    // add result to cache
    Params.NewNode:=nil;
    Params.NewCodeTool:=nil;
    AddResultToNodeCaches(FirstSearchedNode,LastSearchedNode,
                    fdfSearchForward in Params.Flags,Params,SearchRangeFlags);
  end;
  CheckResult(false,false);
end;

function TFindDeclarationTool.FindEnumInContext(
  Params: TFindDeclarationParams): boolean;
{ search all subnodes for ctnEnumIdentifier

  Params:
    Identifier
    ContextNode  // = DeepestNode at Cursor

  Result:
    true, if enum found
 }
var OldContextNode, CurContextNode: TCodeTreeNode;
  CollectResult: TIdentifierFoundResult;
begin
  Result:=false;
  if Params.ContextNode=nil then exit;
  CurContextNode:=Params.ContextNode;
  if CurContextNode.Desc=ctnClass then
    BuildSubTreeForClass(CurContextNode);
  CurContextNode:=CurContextNode.FirstChild;
  while CurContextNode<>nil do begin
    if (CurContextNode.Desc=ctnEnumIdentifier) then begin
      if (fdfCollect in Params.Flags) then begin
        //debugln('TFindDeclarationTool.FindEnumInContext ',GetIdentifier(@Src[CurContextNode.StartPos]));
        CollectResult:=DoOnIdentifierFound(Params,CurContextNode);
        if CollectResult=ifrAbortSearch then begin
          Result:=false;
          exit;
        end else if CollectResult=ifrSuccess then begin
          Result:=true;
          Params.SetResult(Self,CurContextNode);
          exit;
        end;
      end else if CompareSrcIdentifiers(CurContextNode.StartPos,Params.Identifier)
      then begin
        // identifier found
        Result:=true;
        Params.SetResult(Self,CurContextNode);
        exit;
      end;
    end;
    OldContextNode:=Params.ContextNode;
    if OldContextNode.FirstChild<>nil then begin
      try
        Params.ContextNode:=CurContextNode;
        Result:=FindEnumInContext(Params);
      finally
        Params.ContextNode:=OldContextNode;
      end;
      if Result then exit;
    end;
    CurContextNode:=CurContextNode.NextBrother;
  end;
end;

function TFindDeclarationTool.FindContextNodeAtCursor(
  Params: TFindDeclarationParams): TFindContext;
{ searches for the context node at a specific cursor pos
  Params.Context should contain the deepest node at cursor
  if there is no special context, then result is equal to Params.Context }
var
  EndPos: integer;
  ExprType: TExpressionType;
  OldFlags: TFindDeclarationFlags;
begin
  EndPos:=CurPos.StartPos;
  OldFlags:=Params.Flags;
  Params.Flags:=Params.Flags-[fdfFindVariable];
  ExprType:=FindExpressionTypeOfVariable(-1,EndPos,Params);
  Params.Flags:=OldFlags;
  if (ExprType.Desc=xtContext) then
    Result:=ExprType.Context
  else begin
    if fdfExceptionOnNotFound in Params.Flags then begin
      MoveCursorToCleanPos(EndPos);
      RaiseException(ctsNoContextNodeFoundAtCursor);
    end else begin
      Result:=CleanFindContext;
    end;
  end;
end;

function TFindDeclarationTool.FindBaseTypeOfNode(Params: TFindDeclarationParams;
  Node: TCodeTreeNode): TFindContext;
  
  procedure RaiseForwardClassNameLess;
  begin
    RaiseException('[TFindDeclarationTool.FindBaseTypeOfNode] '
                  +'forward class node without name');
  end;
  
  procedure RaiseCircleDefs;
  begin
    Params.NewCodeTool.RaiseException(ctsCircleInDefinitions
      +' ('+ctsIdentifier+'='+GetIdentifier(Params.Identifier)+')');
  end;
  
  procedure RaiseInternalError;
  begin
    Params.IdentifierTool.RaiseException(
       '[TFindDeclarationTool.FindBaseTypeOfNode]'
      +' internal error: not IsPCharInSrc(Params.Identifier) '
      +' Params.IdentifierTool.='
              +TCodeBuffer(Params.IdentifierTool.Scanner.MainCode).Filename
      +' Ident="'+GetIdentifier(Params.Identifier)+'"');
  end;
  
  procedure RaiseBaseTypeOfNotFound;
  begin
    RaiseExceptionFmt(ctsBaseTypeOfNotFound,[GetIdentifier(Params.Identifier)]);
  end;
  
  procedure RaiseClassOfWithoutIdentifier;
  begin
    RaiseExceptionFmt(ctsBaseTypeOfNotFound+' ("class of")',
                      [GetIdentifier(Params.Identifier)]);
  end;
  
var
  OldInput: TFindDeclarationInput;
  ClassIdentNode, DummyNode: TCodeTreeNode;
  NodeStack: TCodeTreeNodeStack;
  OldPos: integer;
  TypeFound: boolean;
  
  procedure RaiseForwardNotResolved;
  begin
    RaiseExceptionFmt(ctsForwardClassDefinitionNotResolved,
        [copy(Src,ClassIdentNode.StartPos,
            ClassIdentNode.EndPos-ClassIdentNode.StartPos)]);
  end;
  
  procedure RaiseClassOfNotResolved;
  begin
    MoveCursorToNodeStart(ClassIdentNode);
    RaiseExceptionFmt(ctsClassOfDefinitionNotResolved,
        [copy(Src,ClassIdentNode.StartPos,
            ClassIdentNode.EndPos-ClassIdentNode.StartPos)]);
  end;

begin
  Result.Node:=Node;
  Result.Tool:=Self;
  Exclude(Params.Flags,fdfTopLvlResolving);
  InitializeNodeStack(@NodeStack);
  try
    while (Result.Node<>nil) do begin
      if (Result.Node.Cache<>nil) and (Result.Node.Cache is TBaseTypeCache) then
      begin
        // base type already cached
        Result:=CreateFindContext(TBaseTypeCache(Result.Node.Cache));
        exit;
      end;
      if NodeExistsInStack(@NodeStack,Result.Node) then begin
        // circle detected
        Result.Tool.MoveCursorToNodeStart(Result.Node);
        Result.Tool.RaiseException(ctsCircleInDefinitions);
      end;
      AddNodeToStack(@NodeStack,Result.Node);

      {$IFDEF ShowTriedBaseContexts}
      DebugLn('[TFindDeclarationTool.FindBaseTypeOfNode] LOOP Result=',Result.Node.DescAsString,' ',DbgS(Result.Node));
      DebugLn('  Flags=[',FindDeclarationFlagsAsString(Params.Flags),']');
      {$ENDIF}
      if (Result.Node.Desc in AllIdentifierDefinitions) then begin
        // instead of variable/const/type definition, return the type
        DummyNode:=Result.Tool.FindTypeNodeOfDefinition(Result.Node);
        if DummyNode=nil then
          // some constants and variants do not have a type
          break;
        Result.Node:=DummyNode;
      end else
      if (Result.Node.Desc in [ctnClass,ctnClassInterface])
      and ((Result.Node.SubDesc and ctnsForwardDeclaration)>0) then
      begin
        // this is a forward defined class
        // -> search the real class
        {$IFDEF ShowTriedBaseContexts}
        DebugLn('[TFindDeclarationTool.FindBaseTypeOfNode] Class is forward');
        {$ENDIF}

        // ToDo: check for circles in ancestor chain
        
        ClassIdentNode:=Result.Node.Parent;
        if (ClassIdentNode=nil) or (not (ClassIdentNode.Desc=ctnTypeDefinition))
        then begin
          MoveCursorToCleanPos(Result.Node.StartPos);
          RaiseForwardClassNameLess;
        end;
        Params.Save(OldInput);
        Params.SetIdentifier(Self,@Src[ClassIdentNode.StartPos],
                             @CheckSrcIdentifier);
        Params.Flags:=[fdfSearchInParentNodes,fdfSearchForward,
                       fdfIgnoreUsedUnits,fdfExceptionOnNotFound,
                       fdfIgnoreCurContextNode]
                      +(fdfGlobals*Params.Flags);
        Params.ContextNode:=ClassIdentNode;
        FindIdentifierInContext(Params);
        if (Params.NewNode.Desc<>ctnTypeDefinition)
        or (Params.NewCodeTool<>Self) then begin
          MoveCursorToCleanPos(Result.Node.StartPos);
          RaiseForwardNotResolved;
        end;
        Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
        Params.Load(OldInput);
        exit;
      end else
      if (Result.Node.Desc=ctnClassOfType) then
      begin
        // this is a 'class of' type
        // -> search the real class
        {$IFDEF ShowTriedBaseContexts}
        DebugLn('[TFindDeclarationTool.FindBaseTypeOfNode] "Class Of"');
        {$ENDIF}

        // ToDo: check for circles in ancestor chain

        ClassIdentNode:=Result.Node.FirstChild;
        if (ClassIdentNode=nil) or (not (ClassIdentNode.Desc=ctnIdentifier))
        then begin
          MoveCursorToCleanPos(Result.Node.StartPos);
          RaiseClassOfWithoutIdentifier;
        end;
        Params.Save(OldInput);
        // first search backwards
        Params.SetIdentifier(Self,@Src[ClassIdentNode.StartPos],
                             @CheckSrcIdentifier);
        Params.Flags:=[fdfSearchInParentNodes,
                       fdfIgnoreCurContextNode]
                      +(fdfGlobals*Params.Flags)-[fdfExceptionOnNotFound];
        Params.ContextNode:=Result.Node.Parent;
        if not FindIdentifierInContext(Params) then begin
          // then search forwards
          Params.Load(OldInput);
          Params.SetIdentifier(Self,@Src[ClassIdentNode.StartPos],
                               @CheckSrcIdentifier);
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound,
                         fdfIgnoreCurContextNode,fdfSearchForward]
                        +(fdfGlobals*Params.Flags);
          Params.ContextNode:=Result.Node.Parent;
          FindIdentifierInContext(Params);
        end;
        if (Params.NewNode.Desc<>ctnTypeDefinition) then begin
          MoveCursorToCleanPos(Result.Node.StartPos);
          RaiseClassOfNotResolved;
        end;
        Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
        Params.Load(OldInput);
        exit;
      end else
      if (Result.Node.Desc=ctnOnIdentifier) and (Result.Node.PriorBrother=nil)
      then begin
        // this is the ON variable node, the type comes right behind
        Result.Node:=Result.Node.NextBrother;
      end else
      if (Result.Node.Desc in [ctnIdentifier,ctnOnIdentifier]) then begin
        // this type is just an alias for another type
        // -> search the basic type
        if Result.Node.Parent=nil then
          break;
        Params.Save(OldInput);
        DummyNode:=Result.Node;
        Params.SetIdentifier(Self,@Src[Result.Node.StartPos],
                             @CheckSrcIdentifier);
        Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                      +(fdfGlobals*Params.Flags);
        Params.ContextNode:=Result.Node.Parent;
        if (Params.ContextNode.Desc in [ctnVarDefinition,ctnConstDefinition])
        then begin
          // pascal allows things like 'var a: a;' -> skip var definition
          Include(Params.Flags,fdfIgnoreCurContextNode);
        end;
        if Params.ContextNode.Desc=ctnParameterList then
          // skip search in parameter list
          Params.ContextNode:=Params.ContextNode.Parent;
        if Params.ContextNode.Desc=ctnProcedureHead then
          // skip search in proc parameters
          Params.ContextNode:=Params.ContextNode.Parent;
        TypeFound:=FindIdentifierInContext(Params);
        if TypeFound and (Params.NewNode.Desc in [ctnUnit,ctnLibrary,ctnPackage])
        then begin
          // unitname.typename
          MoveCursorToNodeStart(Result.Node);
          ReadNextAtom; // read unitname
          ReadNextAtomIsChar('.');
          ReadNextAtom; // read type identifier
          Params.Load(OldInput);
          Params.SetIdentifier(Self,@Src[CurPos.StartPos],
                               @CheckSrcIdentifier);
          Params.Flags:=[fdfExceptionOnNotFound]
                        +(fdfGlobals*OldInput.Flags);
          Params.ContextNode:=Params.NewCodeTool.FindInterfaceNode;
          TypeFound:=Params.NewCodeTool.FindIdentifierInContext(Params);
        end;
        if TypeFound then begin
          if Params.NewNode.Desc in [ctnTypeDefinition] then begin
            if NodeExistsInStack(@NodeStack,Params.NewNode) then begin
              // circle detected
              Params.NewCodeTool.MoveCursorToNodeStart(Params.NewNode);
              RaiseCircleDefs;
            end;
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                          Params.NewNode)
          end else begin
            // not a type
            MoveCursorToNodeStart(DummyNode);
            ReadNextAtom;
            RaiseExceptionFmt(ctsStrExpectedButAtomFound,
                              [ctsTypeIdentifier,GetAtom]);
          end;
        end else
          // predefined identifier
          Result:=CreateFindContext(Self,Result.Node);
        Params.Load(OldInput);
        exit;
      end else
      if (Result.Node.Desc=ctnProperty)
      or (Result.Node.Desc=ctnGlobalProperty) then begin
        // this is a property -> search the type definition of the property
        MoveCursorToNodeStart(Result.Node);
        if (Result.Node.Desc=ctnProperty) then
          ReadNextAtom; // read 'property'
        ReadNextAtom; // read name
        ReadNextAtom;
        if CurPos.Flag=cafEdgedBracketOpen then begin
          // this is an indexed property
          exit;
        end;
        if CurPos.Flag=cafColon then begin
          ReadNextAtom;
          AtomIsIdentifier(true);
          OldPos:=CurPos.StartPos;
          // property has type
          Params.Save(OldInput);
          Params.SetIdentifier(Self,@Src[CurPos.StartPos],nil);
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                        +(fdfGlobals*Params.Flags);
          Params.ContextNode:=Result.Node.Parent;
          if FindIdentifierInContext(Params) then begin
            if Params.NewNode.Desc in [ctnTypeDefinition] then begin
              if NodeExistsInStack(@NodeStack,Params.NewNode) then begin
                // circle detected
                Params.NewCodeTool.MoveCursorToNodeStart(Params.NewNode);
                RaiseCircleDefs;
              end;
              Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                            Params.NewNode)
            end else begin
              // not a type
              MoveCursorToCleanPos(OldPos);
              ReadNextAtom;
              RaiseExceptionFmt(ctsStrExpectedButAtomFound,
                                [ctsTypeIdentifier,GetAtom]);
            end;
          end else
            // predefined identifier
            Result:=CreateFindContext(Self,Result.Node);
          Params.Load(OldInput);
          exit;
        end else if (Result.Node.Desc=ctnProperty) then begin
          // property has no type
          // -> search ancestor property
          Params.Save(OldInput);
          MoveCursorToNodeStart(Result.Node);
          ReadNextAtom; // read 'property'
          ReadNextAtom; // read name
          OldPos:=CurPos.StartPos;
          Params.SetIdentifier(Self,@Src[CurPos.StartPos],nil);
          Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInAncestors]
                       +(fdfGlobalsSameIdent*Params.Flags);
          FindIdentifierInAncestors(Result.Node.Parent.Parent,Params);
          if Params.NewNode.Desc=ctnProperty then begin
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                          Params.NewNode);
          end else begin
            // ancestor is not a property
            MoveCursorToCleanPos(OldPos);
            RaiseException(ctsAncestorIsNotProperty);
          end;
          Params.Load(OldInput);
          exit;
        end;
      end else
      if (Result.Node.Desc in [ctnProcedure,ctnProcedureHead])
      and (fdfFunctionResult in Params.Flags) then begin
        // a proc -> if this is a function then return the result type
        if Result.Node.Desc=ctnProcedure then
          Result.Node:=Result.Node.FirstChild;
        BuildSubTreeForProcHead(Result.Node,DummyNode);
        if (DummyNode<>nil) then begin
          // a function or an overloaded operator
          Result.Node:=DummyNode;
          Exclude(Params.Flags,fdfFunctionResult);
        end else begin
          // this is a procedure or destructor
          break;
        end;
      end else
      if (Result.Node.Desc=ctnTypeType) then begin
        // a TypeType is for example 'MyInt = type integer;'
        // the context is not the 'type' keyword, but the identifier after it.
        Result.Node:=Result.Node.FirstChild;
      end else
      if (Result.Node.Desc=ctnEnumIdentifier) then begin
        // an enum identifier, the base type is the enumeration
        Result.Node:=Result.Node.Parent;
      end else
        break;
    end;
    if (Result.Node=nil) and (fdfExceptionOnNotFound in Params.Flags) then begin
      if (Result.Tool<>nil) and (Params.Identifier<>nil) then begin

        // ToDo ppu, ppw, dcu

        if (not Params.IdentifierTool.IsPCharInSrc(Params.Identifier)) then
          RaiseInternalError;
        Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
      end;
      RaiseBaseTypeOfNotFound;
    end;
  finally
    // cache the result in all nodes
    CreateBaseTypeCaches(@NodeStack,Result);
    // free node stack
    FinalizeNodeStack(@NodeStack);
  end;
  {$IFDEF ShowFoundIdentifier}
  DbgOut('[TFindDeclarationTool.FindBaseTypeOfNode] END Node=');
  if Node<>nil then DbgOut(Node.DescAsString) else DbgOut('NIL');
  DbgOut(' Result=');
  if Result.Node<>nil then DbgOut(Result.Node.DescAsString) else DbgOut('NIL');
  DebugLn('');
  {$ENDIF}
end;

function TFindDeclarationTool.FindDeclarationAndOverload(
  const CursorPos: TCodeXYPosition; out ListOfPCodeXYPosition: TFPList;
  Flags: TFindDeclarationListFlags): boolean;
var
  CurCursorPos: TCodeXYPosition;
  NewTool: TFindDeclarationTool;
  NewNode: TCodeTreeNode;
  NewPos: TCodeXYPosition;
  NewTopLine: integer;
  CurTool: TFindDeclarationTool;
  OldPositions: TFPList;
  NodeList: TFPList;
  CleanPos: integer;

  procedure AddPos;
  begin
    AddCodePosition(OldPositions,NewPos);
    if (NodeList.IndexOf(NewNode)>=0) then
      exit;
    NodeList.Add(NewNode);

    if (fdlfWithoutEmptyProperties in Flags)
    and (NewNode.Desc=ctnProperty)
    and (NewTool.PropNodeIsTypeLess(NewNode)) then
      exit;
    if (fdlfWithoutForwards in Flags) then begin
      if (NewNode.Desc=ctnTypeDefinition)
      and NewTool.NodeIsForwardDeclaration(NewNode)
      then
        exit;
      if (NewNode.Desc=ctnProcedure)
      and ((NewNode.SubDesc and ctnsForwardDeclaration)>0) then
        exit;
    end;
    AddCodePosition(ListOfPCodeXYPosition,NewPos);
  end;

begin
  Result:=true;
  ListOfPCodeXYPosition:=nil;
  NewTool:=nil;
  NewNode:=nil;
  OldPositions:=nil;
  NodeList:=nil;

  ActivateGlobalWriteLock;
  try
    BuildTreeAndGetCleanPos(trAll,CursorPos,CleanPos,[]);

    NodeList:=TFPList.Create;
    NewTool:=Self;
    NewNode:=FindDeepestExpandedNodeAtPos(CleanPos,true);
    NewPos:=CursorPos;
    AddPos;

    CurCursorPos:=CursorPos;
    CurTool:=Self;
    try
      while CurTool.FindDeclaration(CurCursorPos,AllFindSmartFlags
        +[fsfSearchSourceName],
        NewTool,NewNode,NewPos,NewTopLine) do
      begin
        if IndexOfCodePosition(OldPositions,@NewPos)>=0 then break;
        AddPos;
        CurCursorPos:=NewPos;
        CurTool:=NewTool;
        debugln('TFindDeclarationTool.FindDeclarationAndOverload ',
          ' Self="',MainFilename,'" ');
        if CurCursorPos.Code<>nil then
          debugln('  CurCursorPos=',CurCursorPos.Code.Filename,' ',dbgs(CurCursorPos.X),',',dbgs(CurCursorPos.Y));
        if CurTool<>nil then
          debugln('  CurTool=',CurTool.MainFilename);
        if (CurTool=nil) then exit;
      end;
    except
      // ignore normal errors
      on E: ECodeToolError do ;
      on E: ELinkScannerError do ;
    end;
  finally
    DeactivateGlobalWriteLock;
    FreeListOfPCodeXYPosition(OldPositions);
    NodeList.Free;
  end;
end;

function TFindDeclarationTool.FindClassAndAncestors(ClassNode: TCodeTreeNode;
  out ListOfPFindContext: TFPList): boolean;
var
  FoundContext: TFindContext;
  CurTool: TFindDeclarationTool;
  Params: TFindDeclarationParams;
begin
  Result:=true;
  ListOfPFindContext:=nil;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (ClassNode.Parent=nil)
  or (ClassNode.Parent.Desc<>ctnTypeDefinition) then exit;

  AddFindContext(ListOfPFindContext,CreateFindContext(Self,ClassNode));

  Params:=TFindDeclarationParams.Create;
  ActivateGlobalWriteLock;
  try
    try
      CurTool:=Self;
      while CurTool.FindAncestorOfClass(ClassNode,Params,true) do begin
        if (Params.NewCodeTool=nil) then break;
        FoundContext.Tool:=Params.NewCodeTool;
        FoundContext.Node:=Params.NewNode;
        if IndexOfFindContext(ListOfPFindContext,@FoundContext)>=0 then break;
        AddFindContext(ListOfPFindContext,FoundContext);
        //debugln('TFindDeclarationTool.FindClassAndAncestors FoundContext=',DbgsFC(FoundContext));
        CurTool:=Params.NewCodeTool;
        ClassNode:=Params.NewNode;
        if (ClassNode=nil)
        or (not (ClassNode.Desc in [ctnClass,ctnClassInterface])) then
          break;
      end;
    except
      // just stop on errors
      on E: ECodeToolError do ;
      on E: ELinkScannerError do ;
    end;
  finally
    DeactivateGlobalWriteLock;
    Params.Free;
  end;
end;

function TFindDeclarationTool.FindContextClassAndAncestors(
  const CursorPos: TCodeXYPosition; var ListOfPFindContext: TFPList
  ): boolean;
// returns a list of nodes of ctnClass
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
  ClassNode: TCodeTreeNode;
begin
  Result:=false;
  ListOfPFindContext:=nil;

  ActivateGlobalWriteLock;
  try
    BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,
                [{$IFNDEF DisableIgnoreErrorAfter}btSetIgnoreErrorPos{$ENDIF}]);

    // find class node
    ANode:=FindDeepestNodeAtPos(CleanCursorPos,true);
    ClassNode:=FindClassNode(ANode);
    if (ClassNode=nil) or (ClassNode.Parent=nil)
    or (ClassNode.Parent.Desc<>ctnTypeDefinition) then exit;

    //debugln('TFindDeclarationTool.FindContextClassAndAncestors A ClassName=',ExtractClassName(ClassNode,false));
    // add class and ancestors type definition to ListOfPCodeXYPosition
    if not FindClassAndAncestors(ClassNode,ListOfPFindContext)
    then exit;
    
    //debugln('TFindDeclarationTool.FindContextClassAndAncestors List: ',ListOfPFindContextToStr(ListOfPFindContext));
    
  finally
    DeactivateGlobalWriteLock;
  end;
  Result:=true;
end;

{-------------------------------------------------------------------------------
  function TFindDeclarationTool.FindReferences(const CursorPos: TCodeXYPosition;
    SkipComments: boolean; var ListOfPCodeXYPosition: TFPList): boolean;

  Search for all identifiers in current unit, referring to the declaration
  at CursorPos.
-------------------------------------------------------------------------------}
function TFindDeclarationTool.FindReferences(const CursorPos: TCodeXYPosition;
  SkipComments: boolean; out ListOfPCodeXYPosition: TFPList): boolean;
var
  Identifier: string;
  DeclarationTool: TFindDeclarationTool;
  DeclarationNode: TCodeTreeNode;
  CleanDeclCursorPos: integer;
  AliasDeclarationNode: TCodeTreeNode;
  StartPos: Integer;
  Params: TFindDeclarationParams;
  PosTree: TAVLTree; // tree of PChar positions in Src
  AVLNode: TAVLTreeNode;
  ReferencePos: TCodeXYPosition;
  MaxPos: Integer;
  CursorNode: TCodeTreeNode;
  UnitStartFound, Found: Boolean;

  procedure AddReference;
  var
    p: PChar;
  begin
    if PosTree=nil then
      PosTree:=TAVLTree.Create;
    p:=@Src[StartPos];
    //debugln('TFindDeclarationTool.FindReferences.AddReference ',DbgS(p),' ',dbgs(PosTree.Find(p)=nil));
    if PosTree.Find(p)=nil then
      PosTree.Add(p);
  end;
  
  procedure AddCodePosition(const NewCodePos: TCodeXYPosition);
  var
    AddCodePos: PCodeXYPosition;
  begin
    if ListOfPCodeXYPosition=nil then ListOfPCodeXYPosition:=TFPList.Create;
    New(AddCodePos);
    AddCodePos^:=NewCodePos;
    ListOfPCodeXYPosition.Add(AddCodePos);
    //debugln('TFindDeclarationTool.FindReferences.AddCodePosition line=',dbgs(NewCodePos.Y),' col=',dbgs(NewCodePos.X));
  end;
  
  procedure ReadIdentifier(IsComment: boolean);
  var
    IdentEndPos: LongInt;
  begin
    if (not IsComment) then begin
      UnitStartFound:=true;
    end;
    IdentEndPos:=StartPos;
    while (IdentEndPos<=MaxPos) and (IsIdentChar[Src[IdentEndPos]]) do
      inc(IdentEndPos);
    //debugln('ReadIdentifier ',copy(Src,StartPos,IdentEndPos-StartPos));
    if (IdentEndPos-StartPos=length(Identifier))
    and (CompareIdentifiers(PChar(Pointer(Identifier)),@Src[StartPos])=0)
    and ((not IsComment)
         or ((not SkipComments) and UnitStartFound))
    then begin
      debugln('Identifier with same name found at: ',
        dbgs(StartPos),' ',GetIdentifier(@Src[StartPos]),
        ' CleanDeclCursorPos=',dbgs(CleanDeclCursorPos),
        ' MaxPos='+dbgs(MaxPos),
        ' IsComment='+dbgs(IsComment),
        ' SkipComments='+dbgs(SkipComments),
        ' UnitStartFound='+dbgs(UnitStartFound));
      if CleanPosToCaret(StartPos,ReferencePos) then
        debugln('  x=',dbgs(ReferencePos.X),' y=',dbgs(ReferencePos.Y),' ',ReferencePos.Code.Filename);

      CursorNode:=BuildSubTreeAndFindDeepestNodeAtPos(Tree.Root,StartPos,true);
      debugln('  CursorNode=',CursorNode.DescAsString,' Forward=',dbgs(CursorNode.SubDesc and ctnsForwardDeclaration));

      if (DeclarationTool=Self)
      and ((StartPos=CleanDeclCursorPos) or (CursorNode=AliasDeclarationNode))
      then
        // declaration itself found
        AddReference
      else if CleanPosIsDeclarationIdentifier(StartPos,CursorNode) then
        // this identifier is another declaration with the same name
      else begin
        // find declaration
        if Params=nil then
          Params:=TFindDeclarationParams.Create
        else
          Params.Clear;
        Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors,
                       fdfExceptionOnNotFound,fdfIgnoreCurContextNode];
        if NodeIsForwardDeclaration(CursorNode) then begin
          debugln('Node is forward declaration');
          Params.Flags:=Params.Flags+[fdfSearchForward];
        end;
        Params.ContextNode:=CursorNode;
        //debugln(copy(Src,Params.ContextNode.StartPos,200));
        Params.SetIdentifier(Self,@Src[StartPos],@CheckSrcIdentifier);

        // search identifier in comment -> if not found, this is no bug
        // => silently ignore
        try
          if fdfSearchForward in Params.Flags then
            Found:=FindIdentifierInContext(Params)
          else
            Found:=FindDeclarationOfIdentAtParam(Params);
        except
          on E: ECodeToolError do
            if not IsComment then raise;
          on E: Exception do
            raise;
        end;

        debugln(' Found=',dbgs(Found));
        if Found and (Params.NewNode<>nil) then begin
          if (Params.NewNode.Desc=ctnProcedure)
          and (Params.NewNode.FirstChild<>nil)
          and (Params.NewNode.FirstChild.Desc=ctnProcedureHead) then begin
            // Instead of jumping to the procedure keyword,
            // jump to the procedure name
            Params.NewNode:=Params.NewNode.FirstChild;
            Params.NewCodeTool.MoveCursorToProcName(Params.NewNode,true);
            Params.NewCleanPos:=Params.NewCodeTool.CurPos.StartPos;
          end;
          debugln('Context=',Params.NewNode.DescAsString,' ',dbgs(Params.NewNode.StartPos),' ',dbgs(DeclarationNode.StartPos));
          if (Params.NewNode=DeclarationNode)
          or (Params.NewNode=AliasDeclarationNode) then
            AddReference;
        end;
      end;
    end;
    StartPos:=IdentEndPos;
  end;
  
  procedure SearchIdentifiers;
  var
    CommentLvl: Integer;
    InStrConst: Boolean;
    //CommentStart: LongInt;
  begin
    StartPos:=1;
    UnitStartFound:=false;
    while StartPos<=MaxPos do begin
      case Src[StartPos] of
      
      '{': // pascal comment
        begin
          //CommentStart:=StartPos;
          inc(StartPos);
          CommentLvl:=1;
          InStrConst:=false;
          while StartPos<=MaxPos do begin
            case Src[StartPos] of
            '{': if Scanner.NestedComments then inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then break;
              end;
            'a'..'z','A'..'Z','_':
              if not InStrConst then begin
                ReadIdentifier(true);
                dec(StartPos);
              end;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(StartPos);
          end;
          inc(StartPos);
          //debugln(StartPos,' ',copy(Src,CommentStart,StartPos-CommentStart));
        end;
        
      '/':  // Delphi comment
        if (Src[StartPos+1]<>'/') then begin
          inc(StartPos);
        end else begin
          inc(StartPos,2);
          InStrConst:=false;
          while (StartPos<=MaxPos) do begin
            case Src[StartPos] of
            #10,#13:
              break;
            'a'..'z','A'..'Z','_':
              if not InStrConst then begin
                ReadIdentifier(true);
                dec(StartPos);
              end;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(StartPos);
          end;
          inc(StartPos);
          if (StartPos<=MaxPos) and (Src[StartPos] in [#10,#13])
          and (Src[StartPos-1]<>Src[StartPos]) then
            inc(StartPos);
        end;
        
      '(': // turbo pascal comment
        if (Src[StartPos+1]<>'*') then begin
          inc(StartPos);
        end else begin
          inc(StartPos,3);
          InStrConst:=false;
          while (StartPos<=MaxPos) do begin
            case Src[StartPos] of
            ')':
              if Src[StartPos-1]='*' then break;
            'a'..'z','A'..'Z','_':
              if not InStrConst then begin
                ReadIdentifier(true);
                dec(StartPos);
              end;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(StartPos);
          end;
          inc(StartPos);
        end;
        
      'a'..'z','A'..'Z','_':
        ReadIdentifier(false);
        
      '''':
        begin
          // skip string constant
          inc(StartPos);
          while (StartPos<=MaxPos) do begin
            if (not (Src[StartPos] in ['''',#10,#13])) then
              inc(StartPos)
            else begin
              inc(StartPos);
              break;
            end;
          end;
        end;
        
      else
        inc(StartPos);
      end;
    end;
  end;
  
  function FindDeclarationNode: boolean;
  const
    JumpToProcAttr = [phpInUpperCase,phpWithoutClassName,phpWithVarModifiers];
  var
    ProcNode: TCodeTreeNode;
  begin
    Result:=false;

    // find the main declaration node and identifier
    DeclarationTool:=nil;
    if Assigned(FOnGetCodeToolForBuffer) then
      DeclarationTool:=FOnGetCodeToolForBuffer(Self,CursorPos.Code,true)
    else if CursorPos.Code=TObject(Scanner.MainCode) then
      DeclarationTool:=Self;
    if DeclarationTool=nil then begin
      debugln('WARNING: TFindDeclarationTool.FindReferences DeclarationTool=nil');
      exit;
    end;
    DeclarationTool.BuildTreeAndGetCleanPos(trAll,CursorPos,CleanDeclCursorPos,
                                            []);
    DeclarationNode:=DeclarationTool.BuildSubTreeAndFindDeepestNodeAtPos(
                             DeclarationTool.Tree.Root,CleanDeclCursorPos,true);
    Identifier:=DeclarationTool.ExtractIdentifier(CleanDeclCursorPos);
    if Identifier='' then begin
      debugln('FindDeclarationNode Identifier="',Identifier,'"');
      exit;
    end;

    // find alias declaration node
    debugln('FindDeclarationNode DeclarationNode=',DeclarationNode.DescAsString);
    AliasDeclarationNode:=nil;
    case DeclarationNode.Desc of

    ctnProcedure:
      AliasDeclarationNode:=FindCorrespondingProcNode(DeclarationNode,
                                                      JumpToProcAttr);
    ctnProcedureHead:
      AliasDeclarationNode:=FindCorrespondingProcNode(DeclarationNode.Parent,
                                                      JumpToProcAttr);
    ctnVarDefinition:
      if DeclarationNode.HasParentOfType(ctnProcedureHead) then begin
        // this is a parameter name
        ProcNode:=DeclarationNode.GetNodeOfType(ctnProcedure);
        // search alias for parameter
        ProcNode:=FindCorrespondingProcNode(ProcNode,JumpToProcAttr);
        if ProcNode<>nil then begin
          BuildSubTreeForProcHead(ProcNode);
          AliasDeclarationNode:=ProcNode;
          while (AliasDeclarationNode<>nil) do begin
            if AliasDeclarationNode.Desc
              in [ctnProcedure,ctnProcedureHead,ctnParameterList]
            then
              AliasDeclarationNode:=AliasDeclarationNode.FirstChild
            else begin
              if CompareIdentifiers(PChar(Pointer(Identifier)),
                @Src[AliasDeclarationNode.StartPos])=0 then break;
              AliasDeclarationNode:=AliasDeclarationNode.NextBrother;
            end;
          end;
        end;
      end;

    end;
    
    if (AliasDeclarationNode<>nil) and (AliasDeclarationNode.Desc=ctnProcedure)
    and (AliasDeclarationNode.FirstChild<>nil)
    and (AliasDeclarationNode.FirstChild.Desc=ctnProcedureHead) then
      AliasDeclarationNode:=AliasDeclarationNode.FirstChild;
    if AliasDeclarationNode<>nil then begin
      debugln('FindDeclarationNode AliasDeclarationNode=',AliasDeclarationNode.DescAsString);
    end;

    Result:=true;
  end;
  
begin
  Result:=false;
  debugln('FindReferences CursorPos=',CursorPos.Code.Filename,' x=',dbgs(CursorPos.X),' y=',dbgs(CursorPos.Y),' SkipComments=',dbgs(SkipComments));
  
  ListOfPCodeXYPosition:=nil;
  Params:=nil;
  PosTree:=nil;

  ActivateGlobalWriteLock;
  try
    BuildTree(false);

    // find declaration nodes and identifier
    if not FindDeclarationNode then exit;

    // search identifiers
    MaxPos:=Tree.FindLastPosition;
    if MaxPos>SrcLen then MaxPos:=SrcLen;
    //debugln('FindReferences StartPos=',dbgs(StartPos),' MaxPos=',dbgs(MaxPos));
    SearchIdentifiers;

    // create the reference list
    if PosTree<>nil then begin
      AVLNode:=PosTree.FindHighest;
      while AVLNode<>nil do begin
        StartPos:=PChar(AVLNode.Data)-PChar(Pointer(Src))+1;
        if CleanPosToCaret(StartPos,ReferencePos) then
          AddCodePosition(ReferencePos);
        AVLNode:=PosTree.FindPrecessor(AVLNode);
      end;
    end;

  finally
    Params.Free;
    PosTree.Free;
    DeactivateGlobalWriteLock;
  end;
  Result:=true;
end;

{-------------------------------------------------------------------------------
  function TFindDeclarationTool.CleanPosIsDeclarationIdentifier(CleanPos: integer;
    Node: TCodeTreeNode): boolean;

  Node should be the deepest node at CleanPos, and all sub trees built.
  See BuildSubTree
-------------------------------------------------------------------------------}
function TFindDeclarationTool.CleanPosIsDeclarationIdentifier(CleanPos: integer;
  Node: TCodeTreeNode): boolean;

  function InNodeIdentifier(NodeIdentStartPos: Integer): boolean;
  var
    IdentStartPos, IdentEndPos: integer;
  begin
    GetIdentStartEndAtPosition(Src,CleanPos,IdentStartPos,IdentEndPos);
    Result:=(IdentEndPos>IdentStartPos) and (IdentStartPos=NodeIdentStartPos);
  end;

begin
  Result:=false;
  if Node=nil then exit;
  case Node.Desc of

  ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition,ctnEnumIdentifier:
    begin
      if NodeIsForwardDeclaration(Node) then exit;
      Result:=InNodeIdentifier(Node.StartPos);
    end;
    
  ctnProcedure:
    begin
      if (Node.SubDesc and ctnsForwardDeclaration)>0 then
        RaiseException('TFindDeclarationTool.CleanPosIsDeclarationIdentifier Node not expanded');
      MoveCursorToProcName(Node,true);
      Result:=InNodeIdentifier(CurPos.StartPos);
    end;
    
  ctnProcedureHead:
    begin
      MoveCursorToProcName(Node,true);
      Result:=InNodeIdentifier(CurPos.StartPos);
    end;
    
  ctnProperty, ctnGlobalProperty:
    begin
      if not MoveCursorToPropName(Node) then exit;
      Result:=InNodeIdentifier(CurPos.StartPos);
    end;

  ctnBeginBlock,ctnClass:
    if (Node.SubDesc and ctnsForwardDeclaration)>0 then
      RaiseException('TFindDeclarationTool.CleanPosIsDeclarationIdentifier Node not expanded');
    
  end;
end;

function TFindDeclarationTool.JumpToNode(ANode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  IgnoreJumpCentered: boolean): boolean;
var
  JumpPos: LongInt;
begin
  Result:=false;
  if (ANode=nil) or (ANode.StartPos<1) then exit;
  JumpPos:=ANode.StartPos;
  if ANode.Desc=ctnProperty then begin
    MoveCursorToPropName(ANode);
    JumpPos:=CurPos.StartPos;
  end;
  Result:=JumpToCleanPos(JumpPos,JumpPos,ANode.EndPos,
                         NewPos,NewTopLine,IgnoreJumpCentered);
end;

function TFindDeclarationTool.JumpToCleanPos(NewCleanPos, NewTopLineCleanPos,
  NewBottomLineCleanPos: integer; var NewPos: TCodeXYPosition;
  var NewTopLine: integer; IgnoreJumpCentered: boolean): boolean;
var
  CenteredTopLine: integer;
  NewTopLinePos: TCodeXYPosition;
  NewBottomLinePos: TCodeXYPosition;
begin
  Result:=false;
  // convert clean position to line, column and code
  if not CleanPosToCaret(NewCleanPos,NewPos) then exit;
  NewTopLine:=NewPos.Y;
  if AdjustTopLineDueToComment then begin
    // if there is a comment in front of the top position, it probably belongs
    // to the destination code
    // -> adjust the topline position, so that the comment is visible
    NewTopLineCleanPos:=FindLineEndOrCodeInFrontOfPosition(NewTopLineCleanPos,
                                                           false);
    if (NewTopLineCleanPos>=1) and (Src[NewTopLineCleanPos] in [#13,#10])
    then begin
      inc(NewTopLineCleanPos);
      if (Src[NewTopLineCleanPos] in [#10,#13])
      and (Src[NewTopLineCleanPos]<>Src[NewTopLineCleanPos-1]) then
        inc(NewTopLineCleanPos);
    end;
  end;
  // convert clean top line position to line, column and code
  if not CleanPosToCaret(NewTopLineCleanPos,NewTopLinePos) then exit;
  // convert clean bottom line position to line, column and code
  NewBottomLinePos:=NewPos;
  if (NewBottomLineCleanPos>NewCleanPos)
  and (not CleanPosToCaret(NewBottomLineCleanPos,NewBottomLinePos)) then exit;

  if NewTopLinePos.Code=NewPos.Code then begin
    // top line position is in the same code as the destination position
    NewTopLine:=NewTopLinePos.Y;
    CenteredTopLine:=NewPos.Y-VisibleEditorLines div 2;
    if JumpCentered and (not IgnoreJumpCentered) then begin
      // center the destination position in the source editor
      if CenteredTopLine<NewTopLine then
        NewTopLine:=CenteredTopLine;
    end;
    // NewTopLine not above first line of code
    if NewTopLine<1 then NewTopLine:=1;
    // make NewTopLine visible
    if NewTopLine<=NewPos.Y-VisibleEditorLines then begin
      // NewTopLine is not visible
      // center or align to bottom
      if (NewBottomLineCleanPos>NewCleanPos)
      and (NewBottomLinePos.Y<NewPos.Y+(VisibleEditorLines div 2))
      then begin
        // align to bottom
        NewTopLine:=NewBottomLinePos.Y-VisibleEditorLines+1;
      end else begin
        // center
        NewTopLine:=CenteredTopLine;
      end;
      if NewTopLine<1 then NewTopLine:=1;
    end;
  end else
    NewTopLine:=1;
  Result:=true;
end;

function TFindDeclarationTool.NodeIsForwardDeclaration(Node: TCodeTreeNode
  ): boolean;
var
  TypeNode: TCodeTreeNode;
begin
  Result:=false;
  if (Node=nil) or (Node.Desc<>ctnTypeDefinition) then exit;
  TypeNode:=FindTypeNodeOfDefinition(Node);
  if TypeNode=nil then exit;
  if TypeNode.Desc=ctnClass then begin
    if (TypeNode.SubDesc and ctnsForwardDeclaration)>0 then begin
      Result:=true;
      exit;
    end;
  end;
end;

function TFindDeclarationTool.FindIdentifierInProcContext(
  ProcContextNode: TCodeTreeNode;
  Params: TFindDeclarationParams): TIdentifierFoundResult;
{ this function is internally used by FindIdentifierInContext
}
var
  NameAtom: TAtomPosition;
begin
  Result:=ifrProceedSearch;
  // if proc is a method body, search in class
  // -> find class name
  MoveCursorToNodeStart(ProcContextNode.FirstChild);
  ReadNextAtom; // read name
  if not AtomIsIdentifier(false) then exit; // ignore operator procs
  NameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method body (not a declaration).
    // -> proceed the search normally ...
  end else begin
    // proc is a proc declaration
    if ((fdfCollect in Params.Flags)
    or CompareSrcIdentifiers(NameAtom.StartPos,Params.Identifier)) then begin
      // proc identifier found
      {$IFDEF ShowTriedContexts}
      DebugLn('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc-Identifier found="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      Params.SetResult(Self,ProcContextNode,NameAtom.StartPos);
      Result:=ifrSuccess;
    end else begin
      // proceed the search normally ...
    end;
  end;
end;

function TFindDeclarationTool.FindIdentifierInClassOfMethod(
  ProcContextNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var
  ClassNameAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  ClassContext: TFindContext;
begin
  Result:=false;
  // if proc is a method, search in class
  // -> find class name
  MoveCursorToNodeStart(ProcContextNode);
  ReadNextAtom; // read keyword
  if UpAtomIs('CLASS') then
    ReadNextAtom;
  ReadNextAtom; // read classname
  ClassNameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method
    if CompareSrcIdentifiers(ClassNameAtom.StartPos,Params.Identifier) then
    begin
      // the class itself is searched
      // -> proceed the search normally ...
    end else begin
      // search the identifier in the class first
      // 1. search the class in the same unit
      Params.Save(OldInput);
      try
        Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes]
                      +(fdfGlobals*Params.Flags)
                      +[fdfExceptionOnNotFound,fdfIgnoreUsedUnits]
                      -[fdfTopLvlResolving];
        Params.ContextNode:=ProcContextNode;
        Params.SetIdentifier(Self,@Src[ClassNameAtom.StartPos],nil);
        {$IFDEF ShowTriedContexts}
        DebugLn('[TFindDeclarationTool.FindIdentifierInClassOfMethod]  Proc="',copy(src,ProcContextNode.StartPos,30),'" searching class of method   class="',ExtractIdentifier(ClassNameAtom.StartPos),'"');
        {$ENDIF}
        FindIdentifierInContext(Params);
        ClassContext:=Params.NewCodeTool.FindBaseTypeOfNode(
                                                         Params,Params.NewNode);
        if (ClassContext.Node=nil)
        or (ClassContext.Node.Desc<>ctnClass) then begin
          MoveCursorToCleanPos(ClassNameAtom.StartPos);
          RaiseException(ctsClassIdentifierExpected);
        end;
        // class context found
        // 2. -> search identifier in class
        Params.Load(OldInput);
        Params.Flags:=[fdfSearchInAncestors]
                      +(fdfGlobalsSameIdent*Params.Flags)
                      -[fdfExceptionOnNotFound];
        Params.ContextNode:=ClassContext.Node;
        {$IFDEF ShowTriedContexts}
        DebugLn('[TFindDeclarationTool.FindIdentifierInClassOfMethod]  searching identifier in class of method');
        {$ENDIF}
        Result:=ClassContext.Tool.FindIdentifierInContext(Params);
        if Result then
          exit;
      finally
        Params.Load(OldInput);
      end;
    end;
  end else begin
    // proc is not a method
    if CompareSrcIdentifiers(ClassNameAtom.StartPos,Params.Identifier) then
    begin
      // proc identifier found
      {$IFDEF ShowTriedContexts}
      DebugLn('[TFindDeclarationTool.FindIdentifierInClassOfMethod]  Proc Identifier found="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      Result:=true;
      Params.SetResult(Self,ProcContextNode,ClassNameAtom.StartPos);
      exit;
    end else begin
      // proceed the search normally ...
    end;
  end;
end;

function TFindDeclarationTool.FindClassOfMethod(ProcNode: TCodeTreeNode;
  Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
var
  ClassNameAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  ClassContext: TFindContext;
begin
  {$IFDEF ShowTriedContexts}
  DebugLn('[TFindDeclarationTool.FindClassOfMethod] A ');
  {$ENDIF}
  Result:=false;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Parent.Desc
  in [ctnClassPublished,ctnClassPublic,ctnClassProtected,ctnClassPrivate]
  then begin
    if FindClassContext then begin
      // return the class node
      Params.SetResult(Self,ProcNode.GetNodeOfType(ctnClass));
    end else begin
      // return the type identifier node
      Params.SetResult(Self,ProcNode.GetNodeOfType(ctnClass).Parent);
    end;
    Result:=true;
    exit;
  end;
  
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom; // read keyword
  if UpAtomIs('CLASS') then ReadNextAtom;
  ReadNextAtom; // read classname
  ClassNameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method
    // -> search the class
    Params.Save(OldInput);
    Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes,
                   fdfExceptionOnNotFound,fdfIgnoreUsedUnits]
                  +(fdfGlobals*Params.Flags)
                  -[fdfTopLvlResolving];
    Params.ContextNode:=ProcNode;
    Params.SetIdentifier(Self,@Src[ClassNameAtom.StartPos],nil);
    {$IFDEF ShowTriedContexts}
    DebugLn('[TFindDeclarationTool.FindClassOfMethod]  searching class of method   class="',ExtractIdentifier(ClassNameAtom.StartPos),'"');
    {$ENDIF}
    FindIdentifierInContext(Params);
    if FindClassContext then begin
      // parse class and return class node
      ClassContext:=FindBaseTypeOfNode(Params,Params.NewNode);
      if (ClassContext.Node=nil)
      or (ClassContext.Node.Desc<>ctnClass) then begin
        MoveCursorToCleanPos(ClassNameAtom.StartPos);
        RaiseException(ctsClassIdentifierExpected);
      end;
      // class of method found
      Params.SetResult(ClassContext);
      // parse class and return class node

      // ToDo: do no JIT parsing for PPU, PPW, DCU files

      ClassContext.Tool.BuildSubTreeForClass(ClassContext.Node);
    end;
    Result:=true;
    Params.Load(OldInput);
  end else begin
    // proc is not a method
  end;
end;

function TFindDeclarationTool.FindAncestorOfClass(ClassNode: TCodeTreeNode;
  Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
var AncestorAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  AncestorNode, ClassIdentNode: TCodeTreeNode;
  SearchBaseClass: boolean;
  AncestorContext: TFindContext;
begin
  if (ClassNode=nil) or (not (ClassNode.Desc in [ctnClass,ctnClassInterface]))
  then
    RaiseException('[TFindDeclarationTool.FindAncestorOfClass] '
      +' invalid classnode');
  Result:=false;
  
  // ToDo: ppu, ppw, dcu
  
  // search the ancestor name
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // read keyword 'class', 'object', 'interface', 'dispinterface'
  if UpAtomIs('PACKED') then ReadNextAtom;
  ReadNextAtom;
  ClassIdentNode:=ClassNode.GetNodeOfType(ctnTypeDefinition);
  if AtomIsChar('(') then begin
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    // ancestor name found
    AncestorAtom:=CurPos;
    SearchBaseClass:=false;
    if (ClassIdentNode<>nil)
    and (CompareIdentifiers(@Src[CurPos.StartPos],
      @Src[ClassIdentNode.StartPos])=0)
    then begin
      MoveCursorToCleanPos(CurPos.StartPos);
      RaiseException('ancestor has same name as class');
    end;
  end else begin
    // no ancestor class specified
    // check class name
    if (ClassIdentNode=nil) or (ClassIdentNode.Desc<>ctnTypeDefinition) then
    begin
      MoveCursorToNodeStart(ClassNode);
      RaiseException('class without name');
    end;
    if ClassNode.Desc=ctnClass then begin
      // if this class is not TObject, TObject is class ancestor
      SearchBaseClass:=
                    not CompareSrcIdentifier(ClassIdentNode.StartPos,'TObject');
    end else begin
      // Delphi has as default interface IInterface
      // FPC has as interface IUnknown
      SearchBaseClass:=
                (not CompareSrcIdentifier(ClassIdentNode.StartPos,'IInterface'))
            and (not CompareSrcIdentifier(ClassIdentNode.StartPos,'IUnknown'));
    end;
    if not SearchBaseClass then exit;
  end;
  {$IFDEF ShowTriedContexts}
  DebugLn('[TFindDeclarationTool.FindAncestorOfClass] ',
  ' search ancestor class = ',GetAtom);
  {$ENDIF}

  // search ancestor class context
  CurPos.StartPos:=CurPos.EndPos;
  Params.Save(OldInput);
  try
    Params.Flags:=[fdfSearchInParentNodes,fdfIgnoreCurContextNode,
                   fdfExceptionOnNotFound]
                  +(fdfGlobals*Params.Flags)
                  -[fdfTopLvlResolving];
    if not SearchBaseClass then
      Params.SetIdentifier(Self,@Src[AncestorAtom.StartPos],nil)
    else begin
      if ClassNode.Desc=ctnClass then
        Params.SetIdentifier(Self,'TObject',nil)
      else
        Params.SetIdentifier(Self,'IInterface',nil);
      Exclude(Params.Flags,fdfExceptionOnNotFound);
    end;
    Params.ContextNode:=ClassNode;
    if not FindIdentifierInContext(Params) then begin
      MoveCursorToNodeStart(ClassNode);
      if ClassNode.Desc=ctnClass then
        RaiseException(ctsDefaultClassAncestorTObjectNotFound)
      else
        RaiseException(ctsDefaultInterfaceAncestorIInterfaceNotFound);
    end;
    if FindClassContext then begin
      AncestorNode:=Params.NewNode;
      AncestorContext:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                             AncestorNode);
      Params.SetResult(AncestorContext);
    end;
    Result:=true;
  finally
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindForwardIdentifier(
  Params: TFindDeclarationParams; var IsForward: boolean): boolean;
{ first search the identifier in the normal way via FindIdentifierInContext
  then search the other direction }
var
  OldInput: TFindDeclarationInput;
begin
  Params.Save(OldInput);
  try
    Exclude(Params.Flags,fdfExceptionOnNotFound);
    Result:=FindIdentifierInContext(Params);
    if not Result then begin
      Params.Load(OldInput);
      Params.Flags:=Params.Flags+[fdfSearchForward,fdfIgnoreCurContextNode];
      Result:=FindIdentifierInContext(Params);
      IsForward:=true;
    end else begin
      IsForward:=false;
    end;
  finally
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindIdentifierInWithVarContext(
  WithVarNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext }
var
  WithVarExpr: TExpressionType;
  OldInput: TFindDeclarationInput;
begin
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.FindIdentifierInWithVarContext] Ident=',
  '"',GetIdentifier(Params.Identifier),'"',
  ' WithStart=',StringToPascalConst(copy(Src,WithVarNode.StartPos,15))
  );
  {$ENDIF}
  Result:=false;
  // find the base type of the with variable
  // move cursor to start of with-variable
  Params.Save(OldInput);
  Params.ContextNode:=WithVarNode;
  Params.Flags:=Params.Flags*fdfGlobals
                +[fdfExceptionOnNotFound,fdfFunctionResult];
  WithVarExpr:=FindExpressionTypeOfVariable(WithVarNode.StartPos,-1,Params);
  if (WithVarExpr.Desc<>xtContext)
  or (WithVarExpr.Context.Node=nil)
  or (WithVarExpr.Context.Node=OldInput.ContextNode)
  or (not (WithVarExpr.Context.Node.Desc
           in [ctnClass,ctnClassInterface,ctnRecordType]))
  then begin
    MoveCursorToCleanPos(WithVarNode.StartPos);
    RaiseException(ctsExprTypeMustBeClassOrRecord);
  end;
  // search identifier in with context
  Params.Load(OldInput);
  Exclude(Params.Flags,fdfExceptionOnNotFound);
  Params.ContextNode:=WithVarExpr.Context.Node;
  Result:=WithVarExpr.Context.Tool.FindIdentifierInContext(Params);
  Params.Load(OldInput);
end;

function TFindDeclarationTool.FindIdentifierInAncestors(
  ClassNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
  and FindBaseTypeOfNode
}
var
  OldInput: TFindDeclarationInput;
begin
  Result:=false;

  if not (fdfSearchInAncestors in Params.Flags) then exit;
  Result:=FindAncestorOfClass(ClassNode,Params,true);
  if not Result then exit;

  Params.Save(OldInput);
  Params.ContextNode:=Params.NewNode;
  Params.Flags:=Params.Flags-[fdfIgnoreCurContextNode,fdfSearchInParentNodes];
  Result:=Params.NewCodeTool.FindIdentifierInContext(Params);
  Params.Load(OldInput);
end;

{$IFDEF DebugPrefix}
procedure TFindDeclarationTool.DecPrefix;
begin
  DebugPrefix:=copy(DebugPrefix,1,length(DebugPrefix)-2);
end;

procedure TFindDeclarationTool.IncPrefix;
begin
  DebugPrefix:=DebugPrefix+'  ';
end;
{$ENDIF}

function TFindDeclarationTool.FindExpressionResultType(
  Params: TFindDeclarationParams; StartPos, EndPos: integer): TExpressionType;
{
- operators
    - mixing ansistring and shortstring gives ansistring
    - Pointer +,- Pointer gives Pointer
    - Sets:
        [enum1] gives  set of enumeration type
        set *,-,+ set   gives set of same type
        set <>,=,<,> set  gives boolean
    - precedence rules table:
        1. brackets
        2. not @ sign
        3. * / div mod and shl shr as
        4. + - or xor
        5. < <> > <= >= in is
    - nil is compatible to pointers and classes
    

- operator overloading?
- internal types. e.g. string[], ansistring[], shortstring[], pchar[] to char
- the type of a subrange is the type of the first constant/enum/number/char
- predefined types:
    ordinal:
      int64, cardinal, QWord, boolean, bytebool, longbool, char
      
    real:
      real, single, double, extended, comp, currency
      
- predefined functions:
    function pred(ordinal type): ordinal constant of same type;
    function succ(ordinal type): ordinal constant of same type;
    function ord(ordinal type): ordinal type;
    val?
    function low(array): type of leftmost index type in the array;
    function high(array): type of leftmost index type in the array;
    procedure dec(ordinal var);
    procedure dec(ordinal var; ordinal type);
    procedure dec(pointer var);
    procedure dec(pointer var; ordinal type);
    procedure inc(ordinal var);
    procedure inc(ordinal var; ordinal type);
    procedure inc(pointer var);
    procedure inc(pointer var; ordinal type);
    procedure write(...);
    procedure writeln(...);
    function SizeOf(type): ordinal constant;
    typeinfo?
    uniquestring?
    procedure include(set type,enum identifier);
    procedure exclude(set type,enum identifier);
}
type
  TOperandAndOperator = record
    Operand: TExpressionType;
    theOperator: TAtomPosition;
    OperatorLvl: integer;
  end;
  TExprStack = array[0..4] of TOperandAndOperator;
var
  CurExprType: TExpressionType;
  ExprStack: TExprStack;
  StackPtr: integer;
  
  procedure ExecuteStack(Complete: boolean);
  { Executes the oerand+operator stack
    Examples:
      Position Operand Operator
         0      AWord     *
         1      AByte     +
      Because * has higher predence than + the stack is executed:
      AWord*AByte gives an integer. New stack
      Position Operand Operator
         0      Integer   +
  }
  var
    NewOperand: TExpressionType;
    LastPos: TAtomPosition;
  begin
    if StackPtr<=0 then begin
      // only one element -> nothing to do
      exit;
    end;
    LastPos:=CurPos;
    {$IFDEF ShowExprEval}
    DebugLn('[TFindDeclarationTool.FindExpressionResultType.ExecuteStack] ',
      ' StackPtr=',dbgs(StackPtr),
      ' Lvl=',dbgs(ExprStack[StackPtr].OperatorLvl),
      ' Complete=',dbgs(Complete));
    {$ENDIF}
    while (StackPtr>0)
    and (Complete
     or (ExprStack[StackPtr-1].OperatorLvl>=ExprStack[StackPtr].OperatorLvl)) do
    begin
      // next operand has a lower or equal priority/precedence
      // -> calculate last two operands
      NewOperand:=CalculateBinaryOperator(ExprStack[StackPtr-1].Operand,
        ExprStack[StackPtr].Operand,ExprStack[StackPtr-1].theOperator,
        Params);
      // put result on stack
      ExprStack[StackPtr-1]:=ExprStack[StackPtr];
      dec(StackPtr);
      ExprStack[StackPtr].Operand:=NewOperand;
    end;
    MoveCursorToAtomPos(LastPos);
  end;
  
  procedure RaiseBinaryOperatorNotFound;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsBinaryOperator,GetAtom]);
  end;
  
  procedure RaiseInternalError;
  begin
    RaiseException('[TFindDeclarationTool.FindExpressionResultType]'
      +' internal error: unknown precedence lvl for operator '+GetAtom);
  end;

  procedure RaiseInternalErrorStack;
  begin
    RaiseException('[TFindDeclarationTool.FindExpressionResultType]'
      +' internal error: stackptr too big ');
  end;

var
  OldFlags: TFindDeclarationFlags;
begin
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.FindExpressionResultType] Start',
  ' Pos=',dbgs(StartPos),'-',dbgs(EndPos),
  '="',copy(Src,StartPos,EndPos-StartPos),'" Context=',Params.ContextNode.DescAsString);
  {$ENDIF}
  Result:=CleanExpressionType;
  OldFlags:=Params.Flags;
  Exclude(Params.Flags,fdfFindVariable);
  // read the expression from left to right and calculate the type
  StackPtr:=-1;
  MoveCursorToCleanPos(StartPos);
  repeat
    // read operand
    CurExprType:=ReadOperandTypeAtCursor(Params);
    {$IFDEF ShowExprEval}
    DebugLn('[TFindDeclarationTool.FindExpressionResultType] Operand: ',
      ExprTypeToString(CurExprType));
    {$ENDIF}
    // put operand on stack
    inc(StackPtr);
    if StackPtr>High(ExprStack) then
      RaiseInternalErrorStack;
    ExprStack[StackPtr].Operand:=CurExprType;
    ExprStack[StackPtr].theOperator.StartPos:=-1;
    ExprStack[StackPtr].OperatorLvl:=5;
    // read operator
    ReadNextAtom;
    {$IFDEF ShowExprEval}
    DebugLn('[TFindDeclarationTool.FindExpressionResultType] Operator: ',
      GetAtom,' CurPos.EndPos=',dbgs(CurPos.EndPos),' EndPos=',dbgs(EndPos));
    {$ENDIF}
    // check if expression is completely parsed
    if (CurPos.EndPos>EndPos) or (CurExprType.Desc=xtNone) then begin
      // -> execute complete stack
      ExecuteStack(true);
      Result:=ExprStack[StackPtr].Operand;
      Params.Flags:=OldFlags;
      exit;
    end;
    if not WordIsBinaryOperator.DoItUpperCase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
    then
      RaiseBinaryOperatorNotFound;
    // put operator on stack
    ExprStack[StackPtr].theOperator:=CurPos;
    // find operator precendence level
    if WordIsLvl1Operator.DoItUpperCase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
    then
      ExprStack[StackPtr].OperatorLvl:=1
    else if WordIsLvl2Operator.DoItUpperCase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
    then
      ExprStack[StackPtr].OperatorLvl:=2
    else if WordIsLvl3Operator.DoItUpperCase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
    then
      ExprStack[StackPtr].OperatorLvl:=3
    else if WordIsLvl4Operator.DoItUpperCase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
    then
      ExprStack[StackPtr].OperatorLvl:=4
    else
      RaiseInternalError;
    // execute stack if possible
    ExecuteStack(false);
    // move cursor to next atom (= next operand start)
    ReadNextAtom;
  until false;
end;

function TFindDeclarationTool.FindIdentifierInUsesSection(
  UsesNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext

   search backwards through the uses section
   compare first the unit name, then load the unit and search there
}
var
  InAtom, UnitNameAtom: TAtomPosition;
  NewCodeTool: TFindDeclarationTool;
  OldFlags: TFindDeclarationFlags;
begin
  Result:=false;
  MoveCursorToUsesEnd(UsesNode);
  repeat
    ReadPriorUsedUnit(UnitNameAtom, InAtom);
    if (fdfIgnoreUsedUnits in Params.Flags) then begin
      if (Params.IdentifierTool=Self)
      and CompareSrcIdentifiers(UnitNameAtom.StartPos,Params.Identifier) then
      begin
        // the searched identifier was a uses unitname, but because the unit
        // should not be opened, point to identifier in the uses section
        Result:=true;
        Params.SetResult(Self,UsesNode,UnitNameAtom.StartPos);
        exit;
      end else begin
        // identifier not found
      end;
    end else begin
      // open the unit
      {$IFDEF ShowTriedUnits}
      DebugLn('TFindDeclarationTool.FindIdentifierInUsesSection Self=',MainFilename,
        ' UnitName=',GetAtom(UnitNameAtom));
      Params.WriteDebugReport;
      {$ENDIF}
      NewCodeTool:=OpenCodeToolForUnit(UnitNameAtom,InAtom,true);
      // search the identifier in the interface of the used unit
      OldFlags:=Params.Flags;
      Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobalsSameIdent*Params.Flags)
                   -[fdfExceptionOnNotFound];
      Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
      Params.Flags:=OldFlags;
      if Result then exit;
      // restore the cursor
      MoveCursorToCleanPos(UnitNameAtom.StartPos);
    end;
    ReadPriorAtom; // read keyword 'uses' or comma
  until not AtomIsChar(',');
end;

function TFindDeclarationTool.FindCodeToolForUsedUnit(UnitNameAtom,
  UnitInFileAtom: TAtomPosition;
  ExceptionOnNotFound: boolean): TFindDeclarationTool;
var AnUnitName, AnUnitInFilename: string;
begin
  Result:=nil;
  if (UnitNameAtom.StartPos<1) or (UnitNameAtom.EndPos<=UnitNameAtom.StartPos)
  or (UnitNameAtom.EndPos>SrcLen+1) then
    RaiseException('[TFindDeclarationTool.FindCodeToolForUsedUnit] '
      +'internal error: invalid UnitNameAtom');
  AnUnitName:=copy(Src,UnitNameAtom.StartPos,
                   UnitNameAtom.EndPos-UnitNameAtom.StartPos);
  if UnitInFileAtom.StartPos>=1 then begin
    if (UnitInFileAtom.StartPos<1)
    or (UnitInFileAtom.EndPos<=UnitInFileAtom.StartPos)
    or (UnitInFileAtom.EndPos>SrcLen+1) then
      RaiseException('[TFindDeclarationTool.FindCodeToolForUsedUnit] '
        +'internal error: invalid UnitInFileAtom');
    AnUnitInFilename:=copy(Src,UnitInFileAtom.StartPos+1,
                   UnitInFileAtom.EndPos-UnitInFileAtom.StartPos-2);
  end else
    AnUnitInFilename:='';
  Result:=FindCodeToolForUsedUnit(AnUnitName,AnUnitInFilename,ExceptionOnNotFound);
end;

function TFindDeclarationTool.FindCodeToolForUsedUnit(const AnUnitName,
  AnUnitInFilename: string; ExceptionOnNotFound: boolean): TFindDeclarationTool;
var
  NewCode: TCodeBuffer;
begin
  Result:=nil;
  NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename,ExceptionOnNotFound);
  if (NewCode=nil) then begin
    // no source found
    if ExceptionOnNotFound then
      RaiseException('unit '+AnUnitName+' not found');
  end else begin
    // source found -> get codetool for it
    {$IFDEF ShowTriedFiles}
    DebugLn('[TFindDeclarationTool.FindCodeToolForUsedUnit] ',
    ' This source is=',TCodeBuffer(Scanner.MainCode).Filename,
    ' NewCode=',NewCode.Filename);
    {$ENDIF}
    if Assigned(FOnGetCodeToolForBuffer) then
      Result:=FOnGetCodeToolForBuffer(Self,NewCode,false)
    else if NewCode=TCodeBuffer(Scanner.MainCode) then
      Result:=Self;
  end;
end;

function TFindDeclarationTool.FindIdentifierInInterface(
  AskingTool: TFindDeclarationTool; Params: TFindDeclarationParams): boolean;
var InterfaceNode: TCodeTreeNode;
  SrcIsUsable: boolean;
  OldInput: TFindDeclarationInput;
  CacheEntry: PInterfaceIdentCacheEntry;
  
  procedure RaiseWrongContext;
  begin
    DebugLn('TFindDeclarationTool.FindIdentifierInInterface.RaiseWrongContext');
    Params.WriteDebugReport;
    SaveRaiseException('TFindDeclarationTool.FindIdentifierInInterface '
                      +'Internal Error: Wrong CodeTool');
  end;
  
begin
  Result:=false;
  // build code tree
  {$IFDEF ShowTriedContexts}
  DebugLn({$IFDEF DebugPrefix}DebugPrefix,{$ENDIF}
  'TFindDeclarationTool.FindIdentifierInInterface',
  ' Ident="',GetIdentifier(Params.Identifier),'"',
  ' IgnoreUsedUnits=',dbgs(fdfIgnoreUsedUnits in Params.Flags),
  ' Self=',TCodeBuffer(Scanner.MainCode).Filename
  );
  {$ENDIF}

  // ToDo: build codetree for ppu, ppw, dcu files
  
  // build tree for pascal source
  BuildTree(true);
  if (AskingTool<>Self) and (AskingTool<>nil) then
    AskingTool.AddToolDependency(Self);

  // search identifier in cache
  if (FInterfaceIdentifierCache<>nil)
  and (not (fdfCollect in Params.Flags)) then begin
    CacheEntry:=FInterfaceIdentifierCache.FindIdentifier(Params.Identifier);
    if CacheEntry<>nil then begin
      // identifier in cache found
      {$IFDEF ShowInterfaceCache}
      DebugLn('[TFindDeclarationTool.FindIdentifierInInterface] Ident already in cache:',
      ' Exists=',DbgS(CacheEntry^.Node<>nil));
      {$ENDIF}
      if CacheEntry^.Node=nil then begin
        // identifier not in this interface
      end else begin
        // identifier in this interface found
        Params.SetResult(Self,CacheEntry^.Node,CacheEntry^.CleanPos);
        Result:=true;
      end;
      exit;
    end;
  end;
  
  // check source name
  MoveCursorToNodeStart(Tree.Root);
  ReadNextAtom; // read keyword for source type, e.g. 'unit'
  SrcIsUsable:=UpAtomIs('UNIT');
  if not SrcIsUsable then
    RaiseException(ctsSourceIsNotUnit);
  ReadNextAtom; // read source name
  if CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then begin
    // identifier is source name
    Params.SetResult(Self,Tree.Root,CurPos.StartPos);
    Result:=true;
    exit;
  end;
  
  // search identifier in interface
  InterfaceNode:=FindInterfaceNode;
  if InterfaceNode=nil then
    RaiseException(ctsInterfaceSectionNotFound);
  Params.Save(OldInput);
  Params.Flags:=(fdfGlobalsSameIdent*Params.Flags)
                -[fdfExceptionOnNotFound,fdfSearchInParentNodes]
                +[fdfIgnoreUsedUnits];
  Params.ContextNode:=InterfaceNode;
  Result:=FindIdentifierInContext(Params);
  Params.Load(OldInput);

  if (Params.NewCodeTool<>Self) then Result:=false;

  // save result in cache
  if Params.Flags*[fdfCollect,fdfDoNotCache]=[] then begin
    if FInterfaceIdentifierCache=nil then
      FInterfaceIdentifierCache:=TInterfaceIdentifierCache.Create(Self);
    if Result then begin
      // identifier exists in interface
      if (Params.NewNode<>nil) and (Params.NewNode.Desc=ctnProcedure) then begin
        //DebugLn('NOTE: TFindDeclarationTool.FindIdentifierInInterface Node is proc');
        // ToDo: add param list to cache
        // -> do not cache
      end else begin
        FInterfaceIdentifierCache.Add(OldInput.Identifier,Params.NewNode,
          Params.NewCleanPos);
      end;
    end else begin
      // identifier does not exist in this interface
      FInterfaceIdentifierCache.Add(OldInput.Identifier,nil,-1);
    end;
  end;
end;

function TFindDeclarationTool.CompareNodeIdentifier(Node: TCodeTreeNode;
  Params: TFindDeclarationParams): boolean;
begin
  Result:=false;
  if Node=nil then exit;
  if Node.Desc in AllSourceTypes then begin
    MoveCursorToNodeStart(Node);
    ReadNextAtom;
    ReadNextAtom;
    Result:=CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier);
  end else if (Node.Desc in AllIdentifierDefinitions)
  or (Node.Desc=ctnIdentifier) then begin
    Result:=CompareSrcIdentifiers(Node.StartPos,Params.Identifier);
  end;
end;

function TFindDeclarationTool.GetInterfaceNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then begin
    CurPos.StartPos:=-1;
    RaiseException('[TFindDeclarationTool.GetInterfaceNode] no code tree found');
  end;
  if not (Tree.Root.Desc in AllUsableSourceTypes) then begin
    CurPos.StartPos:=-1;
    RaiseException(ctsUsedUnitIsNotAPascalUnit);
  end;
  Result:=FindInterfaceNode;
  if Result=nil then begin
    CurPos.StartPos:=-1;
    RaiseException(ctsInterfaceSectionNotFound);
  end;
end;

function TFindDeclarationTool.FindIdentifierInUsedUnit(
  const AnUnitName: string; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInHiddenUsedUnits
  for hidden used units, like the system unit or the objpas unit
}
var
  NewCode: TCodeBuffer;
  NewCodeTool: TFindDeclarationTool;
  OldInput: TFindDeclarationInput;
begin
  Result:=false;
  // open the unit and search the identifier in the interface
  NewCode:=FindUnitSource(AnUnitName,'',true);
  if (NewCode=nil) then begin
    // no source found
    CurPos.StartPos:=-1;
    RaiseExceptionInstance(
      ECodeToolUnitNotFound.Create(Self,Format(ctsUnitNotFound,[AnUnitName]),AnUnitName));
  end else if NewCode=TCodeBuffer(Scanner.MainCode) then begin
    // Searching again in hidden unit
    DebugLn('WARNING: Searching again in hidden unit: "',NewCode.Filename,'"');
  end else begin
    // source found -> get codetool for it
    {$IFDEF ShowTriedContexts}
    DebugLn('[TFindDeclarationTool.FindIdentifierInUsedUnit] ',
    ' This source is=',TCodeBuffer(Scanner.MainCode).Filename,
    ' NewCode=',NewCode.Filename,' IgnoreUsedUnits=',dbgs(fdfIgnoreUsedUnits in Params.Flags));
    {$ENDIF}
    if Assigned(FOnGetCodeToolForBuffer) then begin
      NewCodeTool:=FOnGetCodeToolForBuffer(Self,NewCode,false);
      if NewCodeTool=nil then begin
        CurPos.StartPos:=-1;
        RaiseExceptionInstance(
          ECodeToolUnitNotFound.Create(Self,Format(ctsUnitNotFound,[AnUnitName]),
            AnUnitName));
      end;
    end else if NewCode=TCodeBuffer(Scanner.MainCode) then begin
      NewCodeTool:=Self;
      CurPos.StartPos:=-1;
      RaiseExceptionFmt(ctsIllegalCircleInUsedUnits,[AnUnitName]);
    end;
    // search the identifier in the interface of the used unit
    Params.Save(OldInput);
    Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobalsSameIdent*Params.Flags)
                 -[fdfExceptionOnNotFound];
    Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindIdentifierInRecordCase(
  RecordCaseNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
var
  IdentPos: LongInt;
begin
  Result:=false;
  MoveCursorToNodeStart(RecordCaseNode);
  ReadNextAtom;// case
  ReadNextAtom;// identifier
  IdentPos:=CurPos.StartPos;
  ReadNextAtom;
  if AtomIsChar(':')
  and ((fdfCollect in Params.Flags)
       or CompareSrcIdentifiers(IdentPos,Params.Identifier))
  then begin
    // identifier found
    {$IFDEF ShowTriedContexts}
    DebugLn('[TFindDeclarationTool.FindIdentifierInRecordCase]  found="',GetIdentifier(Params.Identifier),'" Src=',GetIdentifier(@Src[IdentPos]));
    {$ENDIF}
    Params.SetResult(Self,RecordCaseNode,IdentPos);
    Result:=true;
  end else begin
    // proceed the search normally ...
  end;
end;

procedure TFindDeclarationTool.BeginParsing(DeleteNodes,
  OnlyInterfaceNeeded: boolean);
begin
  // scan code and init parser
  inherited BeginParsing(DeleteNodes,OnlyInterfaceNeeded);

  // now the scanner knows, which compiler mode is needed
  // -> setup compiler dependent tables
  case Scanner.PascalCompiler of
  pcDelphi: WordIsPredefinedIdentifier:=WordIsPredefinedDelphiIdentifier;
  else
    WordIsPredefinedIdentifier:=WordIsPredefinedFPCIdentifier;
  end;
end;

procedure TFindDeclarationTool.BuildTree(OnlyInterfaceNeeded: boolean);
begin
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TFindDeclarationTool.BuildTree Checking depends ...');{$ENDIF}
  CheckDependsOnNodeCaches;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TFindDeclarationTool.BuildTree building tree');{$ENDIF}
  inherited BuildTree(OnlyInterfaceNeeded);
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TFindDeclarationTool.BuildTree  tree built');{$ENDIF}
end;

function TFindDeclarationTool.FindIdentifierInHiddenUsedUnits(
  Params: TFindDeclarationParams): boolean;
type
  SystemUnitType = (
    sutSystem,
    sutObjPas,
    sutMacPas,
    sutLineInfo,
    sutHeapTrc,
    sutSysThrds,
    sutNone);
var
  OldInput: TFindDeclarationInput;
  SystemAlias: string;
  CurUnitType: SystemUnitType;
begin
  Result:=false;
  {$IFDEF ShowTriedContexts}
  DebugLn('[TFindDeclarationTool.FindIdentifierInHiddenUsedUnits] ',
  '"',GetIdentifier(Params.Identifier),'" IgnoreUsedUnits=',dbgs(fdfIgnoreUsedUnits in Params.Flags));
  {$ENDIF}
  if (Tree.Root<>nil) and (not (fdfIgnoreUsedUnits in Params.Flags)) then begin
    // check, if this is a special unit
    MoveCursorToNodeStart(Tree.Root);
    ReadNextAtom;
    ReadNextAtom;
    SystemAlias:='SYSTEM';
    if (Scanner.PascalCompiler=pcDelphi) then begin
      SystemAlias:='System';
    end else begin
      // FPC
      if Scanner.InitialValues.IsDefined('VER1_0')
      then begin
        if Scanner.InitialValues.IsDefined('LINUX') then
          SystemAlias:='SYSLINUX'
        else if Scanner.InitialValues.IsDefined('BSD') then
          SystemAlias:='SYSBSD'
        else if Scanner.InitialValues.IsDefined('WIN32') then
          SystemAlias:='SYSWIN32';
      end;
    end;
    if UpAtomIs(SystemAlias) or UpAtomIs('SYSTEM') then
      CurUnitType:=sutSystem
    else if UpAtomIs('OBJPAS') then
      CurUnitType:=sutObjPas
    else if UpAtomIs('MACPAS') then
      CurUnitType:=sutMacPas
    else if UpAtomIs('LINEINFO') then
      CurUnitType:=sutLineInfo
    else if UpAtomIs('HEAPTRC') then
      CurUnitType:=sutHeapTrc
    else if UpAtomIs('SYSTHRDS') then
      CurUnitType:=sutSysThrds
    else
      CurUnitType:=sutNone;
    // try hidden units
    if (CurUnitType>sutSysThrds)
    and Scanner.InitialValues.IsDefined(ExternalMacroStart+'UseSysThrds')
    then begin
      // try hidden used unit 'systhrds'
      Result:=FindIdentifierInUsedUnit('SysThrds',Params);
      if Result then exit;
    end;
    if (CurUnitType>sutHeapTrc)
    and Scanner.InitialValues.IsDefined(ExternalMacroStart+'UseHeapTrcUnit')
    then begin
      // try hidden used unit 'heaptrc'
      Result:=FindIdentifierInUsedUnit('HeapTrc',Params);
      if Result then exit;
    end;
    if (CurUnitType>sutLineInfo)
    and Scanner.InitialValues.IsDefined(ExternalMacroStart+'UseLineInfo')
    then begin
      // try hidden used unit 'lineinfo'
      Result:=FindIdentifierInUsedUnit('LineInfo',Params);
      if Result then exit;
    end;
    if (CurUnitType>sutObjPas)
    and (Scanner.CompilerMode in [cmDELPHI,cmOBJFPC])
    and (Scanner.PascalCompiler=pcFPC) then begin
      // try hidden used fpc unit 'objpas'
      Result:=FindIdentifierInUsedUnit('ObjPas',Params);
      if Result then exit;
    end;
    if (CurUnitType>sutMacPas)
    and (Scanner.CompilerMode=cmMacPas)
    and (Scanner.PascalCompiler=pcFPC) then begin
      // try hidden used fpc unit 'macpas'
      Result:=FindIdentifierInUsedUnit('MacPas',Params);
      if Result then exit;
    end;
    if (CurUnitType>sutSystem) then begin
      // try hidden used unit 'system'
      if not CompareSrcIdentifiers(Params.Identifier,'system')
      then begin
        Result:=FindIdentifierInUsedUnit(SystemAlias,Params);
      end else begin
        // the system unit name itself is searched -> rename searched identifier
        Params.Save(OldInput);
        try
          Params.SetIdentifier(Self,PChar(Pointer(SystemAlias)),nil);
          Result:=FindIdentifierInUsedUnit(SystemAlias,Params);
        finally
          // ! always reset input, because the string SystemAlias is freed !
          Params.Load(OldInput);
        end;
      end;
    end;
    if Result then exit;
  end;
end;

function TFindDeclarationTool.FindEndOfVariable(
  StartPos: integer; ExceptionIfNoVariableStart: boolean): integer;
{ a variable can have the form:
    A
    A.B()^.C()[]^^.D
    (A).B
    inherited A
}
  procedure RaiseIdentNotFound;
  begin
    RaiseExceptionFmt(ctsIdentExpectedButAtomFound,[GetAtom]);
  end;

var
  FirstIdentifier: boolean;
begin
  MoveCursorToCleanPos(StartPos);
  ReadNextAtom;
  if UpAtomIs('INHERITED') then
    ReadNextAtom;
  FirstIdentifier:=true;
  if (CurPos.Flag in AllCommonAtomWords) and AtomIsIdentifier(true) then begin
    FirstIdentifier:=false;
    ReadNextAtom;
  end;
  repeat
    case CurPos.Flag of
    cafRoundBracketOpen:
      begin
        ReadTilBracketClose(true);
        FirstIdentifier:=false;
      end;
      
    cafPoint:
      begin
        if FirstIdentifier and ExceptionIfNoVariableStart then
          RaiseIdentNotFound;
        ReadNextAtom;
        AtomIsIdentifier(true);
      end;

    cafEdgedBracketOpen:
      begin
        if FirstIdentifier and ExceptionIfNoVariableStart then
          RaiseIdentNotFound;
        ReadTilBracketClose(true);
      end;
      
    else
      if AtomIsChar('^') then begin
        if FirstIdentifier and ExceptionIfNoVariableStart then
          RaiseIdentNotFound;
      end else
        break;
    end;
    ReadNextAtom;
  until false;
  if LastAtoms.Count>0 then
    UndoReadNextAtom
  else
    MoveCursorToCleanPos(StartPos);
  Result:=CurPos.EndPos;
end;

function TFindDeclarationTool.FindStartOfVariable(EndPos: integer): integer;
{ a variable can be combinations of
  1. A.B
  2. A().B
  3. inherited A
  4. A[].
  5. A[].B
  6. A^.
  7. (A).
  8. (A as B)
  9. (@A)
}
  procedure RaiseIdentNotFound;
  begin
    RaiseExceptionFmt(ctsIdentExpectedButAtomFound,[GetAtom]);
  end;

var CurAtom, NextAtom: TAtomPosition;
  NextAtomType, CurAtomType: TVariableAtomType;
begin
  MoveCursorToCleanPos(EndPos);
  NextAtom:=CurPos;
  NextAtomType:=vatSpace;
  repeat
    ReadPriorAtom;
    CurAtom:=CurPos;
    CurAtomType:=GetCurrentAtomType;
    if CurAtomType in [vatRoundBracketClose,vatEdgedBracketClose] then begin
      ReadBackTilBracketOpen(true);
      CurAtom.StartPos:=CurPos.StartPos;
    end;
    // check if CurAtom belongs to variable
    if CurAtomType=vatINHERITED then begin
      Result:=CurAtom.StartPos;
      exit;
    end;
    if CurAtomType in [vatAS,vatKeyword] then begin
      Result:=NextAtom.StartPos;
      exit;
    end;
    if (not (CurAtomType in [vatIdentifier,vatPreDefIdentifier,vatPoint,vatUp,
      vatEdgedBracketClose,vatRoundBracketClose]))
    or ((CurAtomType in [vatIdentifier,vatPreDefIdentifier,vatNone])
        and (NextAtomType in [vatIdentifier,vatPreDefIdentifier]))
    or ((CurAtomType in [vatNone])
        and (NextAtomType in [vatIdentifier,vatPreDefIdentifier,
                              vatRoundBracketClose]))
    then begin
      // the next atom is the start of the variable
      if (not (NextAtomType in [vatSpace,vatIdentifier,vatPreDefIdentifier,
        vatRoundBracketClose,vatEdgedBracketClose,vatAddrOp])) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseIdentNotFound;
      end;
      Result:=NextAtom.StartPos;
      exit;
    end;
    NextAtom:=CurAtom;
    NextAtomType:=CurAtomType;
  until false;
end;

function TFindDeclarationTool.FindExpressionTypeOfVariable(
  StartPos, EndPos: integer;  Params: TFindDeclarationParams): TExpressionType;
{ examples
  1. A.B
  2. A().B
  3. inherited A
  4. A[]
  5. A[].B
  6. A^.
  7. (A).
  8. (A as B)
  9. (@A)
}
type
  TIsIdentEndOfVar = (iieovYes, iieovNo, iieovUnknown);
var
  CurAtomType, NextAtomType, LastAtomType: TVariableAtomType;
  CurAtom, NextAtom: TAtomPosition;
  CurAtomBracketEndPos: integer;
  StartContext: TFindContext;
  OldInput: TFindDeclarationInput;
  StartFlags: TFindDeclarationFlags;
  IsIdentEndOfVar: TIsIdentEndOfVar;
  ExprType: TExpressionType;

  procedure RaiseIdentExpected;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsIdentifier,GetAtom]);
  end;

  procedure RaiseIdentNotFound;
  begin
    RaiseExceptionFmt(ctsIdentifierNotFound,[GetAtom]);
  end;

  procedure RaiseIllegalQualifierFound;
  begin
    RaiseExceptionFmt(ctsIllegalQualifier,[GetAtom]);
  end;

  procedure RaisePointNotFound;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,['.',GetAtom]);
  end;

  procedure InitAtomQueue;
  
    procedure RaiseInternalError;
    begin
      RaiseException('internal codetool error: FindExpressionTypeOfVariable '
        +' StartPos='+IntToStr(StartPos)+' EndPos='+IntToStr(EndPos));
    end;
  
  begin
    if StartPos<1 then
      StartPos:=FindStartOfVariable(EndPos)
    else if EndPos<1 then
      EndPos:=FindEndOfVariable(StartPos,true);
    if (StartPos<1) then
      RaiseInternalError;
    {$IFDEF ShowExprEval}
    DebugLn('  InitAtomQueue Expr="',copy(Src,StartPos,EndPos-StartPos),'"');
    {$ENDIF}
    LastAtomType:=vatNone;
    MoveCursorToCleanPos(StartPos);
    ReadNextAtom;
    CurAtom:=CurPos;
    CurAtomType:=GetCurrentAtomType;
    if CurAtomType in [vatRoundBracketOpen,vatEdgedBracketOpen] then
      ReadTilBracketClose(true);
    CurAtomBracketEndPos:=CurPos.EndPos;
    ReadNextAtom;
    NextAtom:=CurPos;
    if NextAtom.EndPos<=EndPos then
      NextAtomType:=GetCurrentAtomType
    else
      NextAtomType:=vatSpace;
    MoveCursorToCleanPos(CurAtom.StartPos);
    IsIdentEndOfVar:=iieovUnknown;
  end;
  
  procedure ReadNextExpressionAtom;
  begin
    LastAtomType:=CurAtomType;
    CurAtom:=NextAtom;
    CurAtomType:=NextAtomType;
    MoveCursorToCleanPos(NextAtom.StartPos);
    ReadNextAtom;
    if CurAtomType in [vatRoundBracketOpen,vatEdgedBracketOpen] then
      ReadTilBracketClose(true);
    CurAtomBracketEndPos:=CurPos.EndPos;
    ReadNextAtom;
    NextAtom:=CurPos;
    if NextAtom.EndPos<=EndPos then
      NextAtomType:=GetCurrentAtomType
    else
      NextAtomType:=vatSpace;
    MoveCursorToCleanPos(CurAtom.StartPos);
    IsIdentEndOfVar:=iieovUnknown;
  end;
  
  function IsIdentifierEndOfVariable: boolean;
  var BehindFuncAtomType: TVariableAtomType;
  begin
    if IsIdentEndOfVar=iieovUnknown then begin
      MoveCursorToCleanPos(CurAtom.EndPos);
      ReadNextAtom;
      if AtomIsChar('(') then begin
        ReadTilBracketClose(true);
        ReadNextAtom;
      end;
      if CurPos.StartPos<EndPos then begin
        BehindFuncAtomType:=GetCurrentAtomType;
        if (BehindFuncAtomType in [vatPoint,vatUP,
          vatEdgedBracketOpen,vatRoundBracketOpen])
        then
          IsIdentEndOfVar:=iieovNo
        else
          IsIdentEndOfVar:=iieovYes;
      end else begin
        IsIdentEndOfVar:=iieovYes;
      end;
    end;
    Result:=(IsIdentEndOfVar=iieovYes);
  end;
  
  procedure ResolveBaseTypeOfIdentifier;
  { normally not the identifier is searched, but its type
    but there is one exception:
      if the identifier is a function and it is the end of the variable then
      the the decision is based on the fdfFunctionResult flag.
  }
  var
    ProcNode, FuncResultNode: TCodeTreeNode;
  begin
    if (ExprType.Context.Node<>nil) then begin
      // check if at the end of the variable
      if IsIdentifierEndOfVariable and (fdfFindVariable in StartFlags) then
        // the variable is wanted, not its type
        exit;

      // find base type
      Exclude(Params.Flags,fdfFunctionResult);
      {$IFDEF ShowExprEval}
      DebugLn('  ResolveBaseTypeOfIdentifier ExprType=',ExprTypeToString(ExprType));
      {$ENDIF}
      ExprType:=ExprType.Context.Tool.ConvertNodeToExpressionType(
                                                  ExprType.Context.Node,Params);
      if (ExprType.Desc=xtContext)
      and (ExprType.Context.Node.Desc in [ctnProcedure,ctnProcedureHead]) then
      begin
        // check if this is a function
        ProcNode:=ExprType.Context.Node;
        if ProcNode.Desc=ctnProcedureHead then
          ProcNode:=ProcNode.Parent;
        ExprType.Context.Tool.BuildSubTreeForProcHead(ProcNode.FirstChild,
                                                      FuncResultNode);
        if (FuncResultNode<>nil) or NodeIsConstructor(ProcNode) then begin
          // it is function or a constructor
          // -> use the result type instead of the function
          if IsIdentifierEndOfVariable then begin
            // this function identifier is the end of the variable
            if not (fdfFunctionResult in StartFlags) then
              exit;
          end;
          Include(Params.Flags,fdfFunctionResult);
          ExprType:=ExprType.Context.Tool.ConvertNodeToExpressionType(
                             ProcNode,Params);
        end;
      end;
    end;
  end;
  
  procedure ResolveIdentifier;
  var
    ProcNode: TCodeTreeNode;
    IdentFound: boolean;
    OldFlags: TFindDeclarationFlags;
  begin
    // for example  'AnObject[3]'
    
    // check special identifiers 'Result' and 'Self'
    IdentFound:=false;
    if (ExprType.Context.Node<>nil)
    and (ExprType.Context.Node.Desc in AllPascalStatements) then begin
      if CompareSrcIdentifier(CurAtom.StartPos,'SELF') then begin
        // SELF in a method is the object itself
        // -> check if in a method or nested proc of a method
        ProcNode:=ExprType.Context.Node;
        while (ProcNode<>nil) do begin
          if (ProcNode.Desc=ctnProcedure) and NodeIsMethodBody(ProcNode) then
            break;
          ProcNode:=ProcNode.Parent;
        end;
        if (ProcNode<>nil)
        and ExprType.Context.Tool.FindClassOfMethod(ProcNode,Params,
                                                  not IsIdentifierEndOfVariable)
        then begin
          ExprType.Desc:=xtContext;
          ExprType.Context:=CreateFindContext(Params);
          IdentFound:=true;
        end;
      end else if CompareSrcIdentifier(CurAtom.StartPos,'RESULT') then begin
        // RESULT has a special meaning in a function
        // -> check if in a function
        ProcNode:=ExprType.Context.Node.GetNodeOfType(ctnProcedure);
        if (ProcNode<>nil) then begin
          if IsIdentifierEndOfVariable
          and (fdfFindVariable in StartFlags) then begin
            ExprType.Desc:=xtContext;
            ExprType.Context.Node:=ProcNode.FirstChild;
          end else begin
            OldFlags:=Params.Flags;
            Include(Params.Flags,fdfFunctionResult);
            ExprType.Desc:=xtContext;
            ExprType.Context:=FindBaseTypeOfNode(Params,ProcNode);
            Params.Flags:=OldFlags;
          end;
          exit;
        end;
      end;
    end;

    // find sub identifier
    if not IdentFound then begin
      Params.Save(OldInput);

      // build new param flags for sub identifiers
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound]
                    +(fdfGlobals*Params.Flags);
      if ExprType.Context.Node=StartContext.Node then begin
        // there is no special context -> search in parent contexts too
        Params.Flags:=Params.Flags
                     +[fdfSearchInParentNodes,fdfIgnoreCurContextNode];
      end else begin
        // only search in special context
        Params.ContextNode:=ExprType.Context.Node;
      end;

      // check identifier for overloaded procs
      if (NextAtomType<>vatRoundBracketOpen)
      or (IsIdentifierEndOfVariable
          and (fdfIgnoreOverloadedProcs in StartFlags))
      then
        Include(Params.Flags,fdfIgnoreOverloadedProcs)
      else
        Exclude(Params.Flags,fdfIgnoreOverloadedProcs);

      // search ...
      Params.SetIdentifier(Self,@Src[CurAtom.StartPos],@CheckSrcIdentifier);
      {$IFDEF ShowExprEval}
      DebugLn('  ResolveIdentifier Ident="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      if ExprType.Context.Tool.FindIdentifierInContext(Params) then begin
        if not Params.NewCodeTool.NodeIsConstructor(Params.NewNode) then begin
          ExprType.Desc:=xtContext;
          ExprType.Context:=CreateFindContext(Params);
        end else begin
          // it's a constructor -> keep the class
        end;
        Params.Load(OldInput);
      end else begin
        // predefined identifier
        Params.Load(OldInput);
        ExprType:=FindExpressionTypeOfPredefinedIdentifier(CurAtom.StartPos,
                                                           Params);
      end;

      // ToDo: check if identifier in 'Protected' section

    end;
    ResolveBaseTypeOfIdentifier;
  end;

  procedure ResolvePoint;
  begin
    // for example 'A.B'
    if (not (NextAtomType in [vatSpace,vatIdentifier,vatPreDefIdentifier])) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIdentExpected;
    end;
    if (ExprType.Context.Node=nil) then begin
      MoveCursorToCleanPos(CurAtom.StartPos);
      RaiseIllegalQualifierFound;
    end;
    if (ExprType.Context.Node.Desc in AllUsableSourceTypes) then begin
      // identifier in front of the point is a unit name
      if ExprType.Context.Tool<>Self then begin
        ExprType.Context.Node:=ExprType.Context.Tool.GetInterfaceNode;
      end;
    end;
    // point changes the context to the base type
    // this is already done, so there is not much left to do.
    // Delphi knows . as shortcut for ^.
    // -> check for pointer type
    if (Scanner.CompilerMode=cmDELPHI) and (ExprType.Desc=xtContext)
    and (ExprType.Context.Node.Desc=ctnPointerType)
    and (ExprType.Context.Node<>StartContext.Node) then begin
      // left side of expression has defined a special context
      // => this '.' is a dereference
      ExprType.Desc:=xtContext;
      ExprType.Context:=ExprType.Context.Tool.FindBaseTypeOfNode(Params,
                                              ExprType.Context.Node.FirstChild);
    end;
  end;

  procedure ResolveAs;
  begin
    // for example 'A as B'
    if (not (NextAtomType in [vatSpace,vatIdentifier,vatPreDefIdentifier])) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIdentExpected;
    end;
    // 'as' is a type cast, so the left side is irrelevant
    // -> context is default context
    ExprType.Desc:=xtContext;
    ExprType.Context:=StartContext;
  end;
  
  procedure ResolveUp;
  begin
    // for example:
    //   1. 'PInt = ^integer'  pointer type
    //   2. a^  dereferencing
    if (not (NextAtomType in [vatSpace,vatPoint,vatUp,vatAS,vatEdgedBracketOpen]))
    or ((ExprType.Context.Node=nil) and (ExprType.Desc<>xtPointer))
    then begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIllegalQualifierFound;
    end;
    if (ExprType.Desc=xtPointer) then exit;
    if (ExprType.Context.Node<>StartContext.Node) then begin
      // left side of expression has defined a special context
      // => this '^' is a dereference
      if (not
          (NextAtomType in [vatSpace,vatPoint,vatAS,vatUP,vatEdgedBracketOpen]))
      then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaisePointNotFound;
      end;
      if ExprType.Context.Node.Desc<>ctnPointerType then begin
        MoveCursorToCleanPos(CurAtom.StartPos);
        RaiseExceptionFmt(ctsIllegalQualifier,['^']);
      end;
      ExprType.Desc:=xtContext;
      ExprType.Context:=ExprType.Context.Tool.FindBaseTypeOfNode(Params,
                                              ExprType.Context.Node.FirstChild);
    end else if NodeHasParentOfType(ExprType.Context.Node,ctnPointerType) then
    begin
      // this is a pointer type definition
      // -> the default context is ok
    end;
  end;

  procedure ResolveEdgedBracketOpen;
  { for example:  a[]
      this could be:
        1. ranged array      e.g. array[1..2] of
        2. dynamic array     e.g. array of integer
        3. variant array     e.g. array of const
        4. indexed pointer   e.g. PInteger[1]
        5. default property  e.g. Items[Index: integer]
        6. indexed property  e.g. Items[Index: integer]
        7. string character  e.g. string[3]
  }

    procedure RaiseTypeIdentNotFound;
    begin
      ExprType.Context.Tool.RaiseExceptionFmt(ctsStrExpectedButAtomFound,
                             [ctsTypeIdentifier,ExprType.Context.Tool.GetAtom]);
    end;
    
    procedure RaiseIdentInCurContextNotFound;
    begin
      ExprType.Context.Tool.RaiseExceptionFmt(ctsStrExpectedButAtomFound,
                                              [ctsIdentifier,GetAtom]);
    end;
  
  begin
    if not (NextAtomType in [vatSpace,vatPoint,vatAs,vatUp,vatRoundBracketClose,
      vatRoundBracketOpen,vatEdgedBracketClose,vatEdgedBracketOpen])
    or ((ExprType.Context.Node=nil)
        and (not (ExprType.Desc in xtAllStringTypes))) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIllegalQualifierFound;
    end;
    if ExprType.Desc in xtAllStringTypes then begin
      ExprType.Desc:=xtChar;
      ExprType.Context.Node:=nil;
      exit;
    end;
    if ExprType.Desc in xtAllWideStringTypes then begin
      ExprType.Desc:=xtWideChar;
      ExprType.Context.Node:=nil;
      exit;
    end;
    
    //debugln('ResolveEdgedBracketOpen A ',ExprType.Context.Node.DescAsString);
    case ExprType.Context.Node.Desc of

    ctnOpenArrayType,ctnRangedArrayType:
      begin
        // the array type is the last child node
        //debugln('ResolveEdgedBracketOpen Open/RangedArray LastChild=',ExprType.Context.Node.LastChild.DescAsString);
        if ExprType.Context.Node.LastChild.Desc=ctnOfConstType then begin
          // 'array of const'; the array type is 'TVarRec'

          // => search 'TVarRec'
          Params.Save(OldInput);
          Params.Flags:=[fdfSearchInParentNodes,fdfIgnoreCurContextNode,
                         fdfExceptionOnNotFound]
                        +fdfGlobals*Params.Flags
                        -[fdfTopLvlResolving];
          // special identifier for TVarRec
          Params.SetIdentifier(Self,'tvarrec',nil);
          Params.ContextNode:=ExprType.Context.Node;
          ExprType.Context.Tool.FindIdentifierInContext(Params);
          ExprType.Context:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                                Params.NewNode);
          Params.Load(OldInput);
        end else begin
          ExprType.Context:=ExprType.Context.Tool.FindBaseTypeOfNode(Params,
                                               ExprType.Context.Node.LastChild);
        end;
      end;
                                               
    ctnPointerType:
      // the pointer type is the only child node
      ExprType.Context:=ExprType.Context.Tool.FindBaseTypeOfNode(Params,
                                              ExprType.Context.Node.FirstChild);

    ctnClass, ctnProperty, ctnGlobalProperty:
      begin
        if ExprType.Context.Node.Desc=ctnClass then begin
          // search default property of the class
          Params.Save(OldInput);
          Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound]
                        +fdfGlobals*Params.Flags;
          // special identifier for default property
          Params.SetIdentifier(Self,@Src[CurAtom.StartPos],nil);
          Params.ContextNode:=ExprType.Context.Node;
          ExprType.Context.Tool.FindIdentifierInContext(Params);
          ExprType.Context:=CreateFindContext(Params);
          Params.Load(OldInput);
        end;
        // find base type of property
        if ExprType.Context.Tool.ReadTilTypeOfProperty(ExprType.Context.Node)
        then begin
          // property has type
          Params.Save(OldInput);
          with ExprType.Context.Tool do
            Params.SetIdentifier(ExprType.Context.Tool,
                                 @Src[CurPos.StartPos],nil);
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                        +(fdfGlobals*Params.Flags);
          Params.ContextNode:=ExprType.Context.Node.Parent;
          if ExprType.Context.Tool.FindIdentifierInContext(Params) then begin
            if Params.NewNode.Desc in [ctnTypeDefinition] then begin
              ExprType.Context:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                                Params.NewNode)
            end else begin
              // not a type
              ExprType.Context.Tool.ReadTilTypeOfProperty(ExprType.Context.Node);
              RaiseTypeIdentNotFound;
            end;
          end else begin
            // predefined identifier
          end;
          Params.Load(OldInput);
        end else
          RaiseIdentInCurContextNotFound;
      end;
      
    ctnIdentifier:
      begin
        MoveCursorToNodeStart(ExprType.Context.Node);
        ReadNextAtom;
        if UpAtomIs('STRING') or UpAtomIs('ANSISTRING')
        or UpAtomIs('SHORTSTRING') then begin
          ExprType.Desc:=xtChar;
          ExprType.Context.Node:=nil;
          exit;
        end else if UpAtomIs('WIDESTRING') then begin
          ExprType.Desc:=xtWideChar;
          ExprType.Context.Node:=nil;
          exit;
        end else begin
          MoveCursorToCleanPos(CurAtom.StartPos);
          ReadNextAtom;
          RaiseIllegalQualifierFound;
        end;
      end;

    else
      MoveCursorToCleanPos(CurAtom.StartPos);
      ReadNextAtom;
      RaiseIllegalQualifierFound;
    end;
  end;

  procedure ResolveRoundBracketOpen;
  begin
    { for example:
        (a+b)   expression bracket: the type is the result type of the
                                    expression.
        a()     typecast or function
    }
    if not (NextAtomType in [vatSpace,vatPoint,vatAs,vatUp,vatRoundBracketClose,
      vatRoundBracketOpen,vatEdgedBracketClose,vatEdgedBracketOpen]) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIllegalQualifierFound;
    end;
    if LastAtomType<>vatNone then begin
      // typecast or function
    end else begin
      // expression
      ExprType:=FindExpressionResultType(Params,CurAtom.StartPos+1,
                                         CurAtomBracketEndPos-1);
    end;
  end;

  procedure ResolveINHERITED;
  var
    ProcNode: TCodeTreeNode;
    ClassOfMethodContext: TFindContext;
  begin
    // for example: inherited A;
    // inherited skips the class and begins to search in the ancestor class
    if (ExprType.Context.Node<>StartContext.Node) or (ExprType.Context.Node=nil)
    then begin
      MoveCursorToCleanPos(CurAtom.StartPos);
      RaiseIllegalQualifierFound;
    end;
    if (not NodeIsInAMethod(ExprType.Context.Node)) then begin
      MoveCursorToCleanPos(CurAtom.StartPos);
      RaiseException(ctsInheritedKeywordOnlyAllowedInMethods);
    end;
    if not (NextAtomType in [vatIdentifier,vatPreDefIdentifier]) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIdentExpected;
    end;

    ReadNextExpressionAtom;
    {$IFDEF ShowExprEval}
    DebugLn('    ResolveINHERITED CurAtomType=',
      VariableAtomTypeNames[CurAtomType],
      ' CurAtom="',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"');
    {$ENDIF}

    // find ancestor of class of method
    ProcNode:=ExprType.Context.Node.GetNodeOfType(ctnProcedure);
    Params.Save(OldInput);
    Params.Flags:=[fdfExceptionOnNotFound]
                  +fdfGlobals*Params.Flags;
    ExprType.Context.Tool.FindClassOfMethod(ProcNode,Params,true);
    ClassOfMethodContext:=CreateFindContext(Params);

    // find class ancestor
    Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                  +fdfGlobals*Params.Flags;
    ClassOfMethodContext.Tool.FindAncestorOfClass(ClassOfMethodContext.Node,
                                                  Params,true);

    // search identifier only in class ancestor
    Params.Load(OldInput);
    Params.SetIdentifier(Self,@Src[CurAtom.StartPos],@CheckSrcIdentifier);
    Params.ContextNode:=Params.NewNode;
    Params.Flags:=Params.Flags-[fdfSearchInParentNodes]
                              +[fdfExceptionOnNotFound,fdfSearchInAncestors];
    Params.NewCodeTool.FindIdentifierInContext(Params);
    ExprType.Context:=CreateFindContext(Params);
    Params.Load(OldInput);

    ResolveBaseTypeOfIdentifier;
  end;
  
begin
  Result:=CleanExpressionType;
  StartFlags:=Params.Flags;
  StartContext.Node:=Params.ContextNode;
  StartContext.Tool:=Self;
  ExprType.Desc:=xtContext;
  ExprType.SubDesc:=xtNone;
  ExprType.Context:=StartContext;
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.FindExpressionTypeOfVariable]',
    ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']',
    ' StartContext=',StartContext.Node.DescAsString
  );
  {$ENDIF}
  
  InitAtomQueue;
  repeat
    {$IFDEF ShowExprEval}
    DebugLn('  FindExpressionTypeOfVariable CurAtomType=',
      VariableAtomTypeNames[CurAtomType],' CurAtom="',GetAtom(CurAtom),'"',
      ' ExprType=',ExprTypeToString(ExprType));
    {$ENDIF}
    case CurAtomType of
    vatIdentifier, vatPreDefIdentifier: ResolveIdentifier;
    vatPoint:             ResolvePoint;
    vatAS:                ResolveAs;
    vatUP:                ResolveUp;
    vatEdgedBracketOpen:  ResolveEdgedBracketOpen;
    vatRoundBracketOpen:  ResolveRoundBracketOpen;
    vatINHERITED:         ResolveINHERITED;
    end;
    ReadNextExpressionAtom;
  until CurAtom.EndPos>EndPos;
  
  Result:=ExprType;
  if (Result.Desc=xtContext) and (not (fdfFindVariable in StartFlags)) then
    Result:=Result.Context.Tool.ConvertNodeToExpressionType(
                 Result.Context.Node,Params);
  {$IFDEF ShowExprEval}
  DebugLn('  FindExpressionTypeOfVariable Result=',ExprTypeToString(Result));
  {$ENDIF}
end;

function TFindDeclarationTool.FindEndOfExpression(StartPos: integer): integer;
begin
  MoveCursorToCleanPos(StartPos);
  Result:=CurPos.StartPos;
  repeat
    ReadNextAtom;
    // read till statement end
    if (CurPos.StartPos>SrcLen)
    or (CurPos.Flag in [cafSemicolon,cafComma,cafEnd,
                        cafRoundBracketClose,cafEdgedBracketClose])
    or (AtomIsKeyWord
      and not IsKeyWordInConstAllowed.DoItUpperCase(UpperSrc,
                                 CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
    then begin
      break;
    end
    else if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then begin
      ReadTilBracketClose(true);
    end;
    Result:=CurPos.EndPos;
  until false;
end;

function TFindDeclarationTool.ConvertNodeToExpressionType(Node: TCodeTreeNode;
  Params: TFindDeclarationParams): TExpressionType;
  
  procedure ConvertIdentifierAtCursor;
  begin
    if WordIsPredefinedIdentifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then
    begin
      // predefined identifiers
      ConvertNodeToExpressionType:=FindExpressionTypeOfPredefinedIdentifier(
                                                        CurPos.StartPos,Params);
    end;
  end;
  
var
  BaseContext: TFindContext;
  OldInput: TFindDeclarationInput;
begin
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.ConvertNodeToExpressionType] A',
  ' Node=',Node.DescAsString);
  {$ENDIF}
  BaseContext:=FindBaseTypeOfNode(Params,Node);
  Node:=BaseContext.Node;
  if BaseContext.Tool<>Self then begin
    Result:=BaseContext.Tool.ConvertNodeToExpressionType(Node,Params);
    exit;
  end;
  Result:=CleanExpressionType;
  Result.Desc:=xtContext;
  Result.Context:=CreateFindContext(Self,Node);
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.ConvertNodeToExpressionType] B',
  ' Expr=',ExprTypeToString(Result));
  {$ENDIF}
  case Node.Desc of
  ctnRangeType:
    begin
      // range type -> convert to special expression type

      // ToDo: ppu, ppw, dcu files

      MoveCursorToNodeStart(Node);

      // ToDo: check for circles

      Params.Save(OldInput);
      Params.ContextNode:=Node;
      Result:=ReadOperandTypeAtCursor(Params);
      Params.Load(OldInput);
      Result.Context:=CreateFindContext(Self,Node);
    end;
    
  ctnConstDefinition:
    begin
      // const -> convert to special expression type

      // ToDo: ppu, ppw, dcu files

      MoveCursorToNodeStart(Node);

      ReadNextAtom;
      if not AtomIsIdentifier(false) then exit;
      ReadNextAtom;
      if not (CurPos.Flag in [cafEqual,cafColon]) then exit;
      ReadNextAtom;

      // ToDo: check for circles

      Params.Save(OldInput);
      Params.ContextNode:=Node;
      Result:=ReadOperandTypeAtCursor(Params);
      Params.Load(OldInput);
      Result.Context:=CreateFindContext(Self,Node);
    end;
    
  ctnIdentifier:
    begin

      // ToDo: ppu, ppw, dcu files

      MoveCursorToNodeStart(Node);
      ReadNextAtom;
      ConvertIdentifierAtCursor;
    end;
    
  ctnProperty,ctnGlobalProperty:
    begin

      // ToDo: ppu, ppw, dcu files

      ExtractPropType(Node,false,true);
      if CurPos.Flag<>cafEdgedBracketOpen then
        ConvertIdentifierAtCursor;
    end;
    
  ctnConstant:
    begin

      // ToDo: ppu, ppw, dcu files

      MoveCursorToNodeStart(Node);
      Params.Save(OldInput);
      Params.ContextNode:=Node;
      Result:=ReadOperandTypeAtCursor(Params);
      Params.Load(OldInput);
      Result.Context:=CreateFindContext(Self,Node);
    end;
  end;
end;

function TFindDeclarationTool.ReadOperandTypeAtCursor(
  Params: TFindDeclarationParams): TExpressionType;
{ internally used by FindExpressionResultType
  after reading, the cursor will be on the next atom
}
var EndPos, SubStartPos: integer;

  procedure ReadEdgedBracketOperand;
  
    procedure RaiseConstExpected;
    begin
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsConstant,GetAtom]);
    end;
  
  begin
    // 'set' constant
    SubStartPos:=CurPos.StartPos;
    ReadNextAtom;
    if not AtomIsChar(']') then begin
      Result:=ReadOperandTypeAtCursor(Params);
      {$IFDEF ShowExprEval}
      DebugLn('[TFindDeclarationTool.ReadOperandTypeAtCursor] Set of ',
      ExpressionTypeDescNames[Result.Desc]);
      if Result.Desc=xtContext then
        DebugLn('  Result.Context.Node=',Result.Context.Node.DescAsString);
      {$ENDIF}
    end else begin
      // empty set '[]'
      Result.Desc:=xtNone;
    end;
    Result.SubDesc:=Result.Desc;
    Result.Desc:=xtConstSet;
    MoveCursorToCleanPos(SubStartPos);
    ReadNextAtom;
    ReadTilBracketClose(true);
    MoveCursorToCleanPos(CurPos.EndPos);
  end;
  
  procedure RaiseIdentExpected;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsIdentifier,GetAtom]);
  end;

var
  OldFlags: TFindDeclarationFlags;
begin
  Result:=CleanExpressionType;

  if CurPos.StartPos=CurPos.EndPos then ReadNextAtom;
  // read unary operators which have no effect on the type: +, -, not
  while AtomIsChar('+') or AtomIsChar('-') or UpAtomIs('NOT') do
    ReadNextAtom;
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.ReadOperandTypeAtCursor] A Atom=',GetAtom);
  {$ENDIF}
  if (AtomIsIdentifier(false))
  or (CurPos.Flag=cafRoundBracketOpen)
  or UpAtomIs('INHERITED') then begin
    // read variable
    SubStartPos:=CurPos.StartPos;
    EndPos:=FindEndOfVariable(SubStartPos,false);
    OldFlags:=Params.Flags;
    Params.Flags:=(Params.Flags*fdfGlobals)+[fdfFunctionResult];
    Result:=FindExpressionTypeOfVariable(SubStartPos,EndPos,Params);
    Params.Flags:=OldFlags;
    MoveCursorToCleanPos(EndPos);
  end
  else if UpAtomIs('NIL') then begin
    Result.Desc:=xtNil;
    ReadNextAtom;
  end
  else if AtomIsChar('[') then begin
    ReadEdgedBracketOperand;
  end
  else if AtomIsStringConstant then begin
    // string or char constant
    if AtomIsCharConstant then
      Result.Desc:=xtChar
    else
      Result.Desc:=xtConstString;
    MoveCursorToCleanPos(CurPos.EndPos);
  end
  else if AtomIsNumber then begin
    // ordinal or real constant
    if AtomIsRealNumber then
      Result.Desc:=xtConstReal
    else
      Result.Desc:=xtConstOrdInteger;
    MoveCursorToCleanPos(CurPos.EndPos);
  end
  else if AtomIsChar('@') then begin
    // a simple pointer or an PChar or an event
    MoveCursorToCleanPos(CurPos.EndPos);
    Result:=ReadOperandTypeAtCursor(Params);
    if (Result.Desc=xtContext)
    or ((Result.Context.Node<>nil) and (Result.Context.Node.Desc=ctnProcedure))
    then begin
      Result.SubDesc:=Result.Desc;
      Result.Desc:=xtPointer;
    end else if (Result.Desc=xtChar) then begin
      Result.SubDesc:=xtNone;
      Result.Desc:=xtPChar
    end else begin
      Result.SubDesc:=xtNone;
      Result.Context:=CleanFindContext;
      Result.Desc:=xtPointer;
    end;
  end
  else
    RaiseIdentExpected;

  {$IFDEF ShowExprEval}
  DbgOut('[TFindDeclarationTool.ReadOperandTypeAtCursor] END ',
  ExpressionTypeDescNames[Result.Desc]);
  if Result.Context.Node<>nil then
    DbgOut(' Context.Node=',Result.Context.Node.DescAsString)
  else
    DbgOut(' Context.Node=nil');
  DebugLn('');
  {$ENDIF}
end;

function TFindDeclarationTool.FindExpressionTypeOfPredefinedIdentifier(
  StartPos: integer; Params: TFindDeclarationParams): TExpressionType;
var
  IdentPos: PChar;
  ParamList: TExprTypeList;
  ParamNode: TCodeTreeNode;
begin

  Result:=CleanExpressionType;
  IdentPos:=@Src[StartPos];
  Result.Desc:=PredefinedIdentToExprTypeDesc(IdentPos);

  {$IFDEF ShowExprEval}
  debugln('TFindDeclarationTool.FindExpressionTypeOfPredefinedIdentifier ',
    ExpressionTypeDescNames[Result.Desc]);
  {$ENDIF}
  ParamList:=nil;
  try
    case Result.Desc of
    xtCompilerFunc:
      begin
        if not (Params.ContextNode.Desc in AllPascalStatements) then exit;
        MoveCursorToCleanPos(StartPos);
        ReadNextAtom;
        ReadNextAtom;
        if not AtomIsChar('(') then
          exit;
        ParamList:=CreateParamExprListFromStatement(CurPos.StartPos,Params);
        if (CompareIdentifiers(IdentPos,'PREC')=0)
        or (CompareIdentifiers(IdentPos,'SUCC')=0) then begin
          // the PREC and SUCC of a expression has the same type as the expression
          if ParamList.Count<>1 then exit;
          Result:=ParamList.Items[0];
        end
        else if (CompareIdentifiers(IdentPos,'LOW')=0)
             or (CompareIdentifiers(IdentPos,'HIGH')=0) then
        begin
          {$IFDEF ShowExprEval}
          debugln('TFindDeclarationTool.FindExpressionTypeOfPredefinedIdentifier Ident=',GetIdentifier(IdentPos));
          {$ENDIF}
          { examples:
             Low(ordinal type)  is the ordinal type
             Low(array)         has type of the array items
             Low(set)           has type of the enums
          }
          if ParamList.Count<>1 then exit;
          Result:=ParamList.Items[0];
          if Result.Desc<>xtContext then exit;
          ParamNode:=Result.Context.Node;
          case ParamNode.Desc of

          ctnEnumerationType:
            // Low(enum)   has the type of the enum
            if (ParamNode.Parent<>nil)
            and (ParamNode.Parent.Desc=ctnTypeDefinition) then
              Result.Context.Node:=ParamNode.Parent;

          ctnOpenArrayType:
            // array without explicit range -> open array
            // Low(Open array) is ordinal integer
            begin
              Result.Desc:=xtConstOrdInteger;
              Result.Context:=CleanFindContext;
            end;

          ctnRangedArrayType:
            begin
              // array with explicit range
              // Low(array[SubRange])  has the type of the subrange
              MoveCursorToNodeStart(ParamNode.FirstChild);
              Result:=ReadOperandTypeAtCursor(Params);
            end;

          else
            DebugLn('NOTE: unimplemented Low(type) type=',ParamNode.DescAsString);
          end;
        end
        else if (CompareIdentifiers(IdentPos,'LENGTH')=0) then
        begin
          if ParamList.Count<>1 then exit;
          Result.Desc:=xtConstOrdInteger;
        end
        else if (CompareIdentifiers(IdentPos,'COPY')=0) then
        begin
          if (ParamList.Count<>3) or (Scanner.Values.IsDefined('VER1_0')) then
            exit;
          Result.Desc:=xtString;
        end;
      end;

    xtString:
      begin
        // ToDo: ask scanner for shortstring, ansistring, widestring
      end;
    end;
  finally
    ParamList.Free;
  end;
end;

function TFindDeclarationTool.CalculateBinaryOperator(LeftOperand,
  RightOperand: TExpressionType; BinaryOperator: TAtomPosition;
  Params: TFindDeclarationParams): TExpressionType;
begin
  Result:=CleanExpressionType;
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.CalculateBinaryOperator] A',
  ' LeftOperand=',ExpressionTypeDescNames[LeftOperand.Desc],
  ' Operator=',GetAtom(BinaryOperator),
  ' RightOperand=',ExpressionTypeDescNames[RightOperand.Desc]
  );
  {$ENDIF}
  // convert Left and RightOperand contexts to expressiontype
  if LeftOperand.Desc=xtContext then begin
    LeftOperand:=LeftOperand.Context.Tool.ConvertNodeToExpressionType(
                      LeftOperand.Context.Node,Params);
  end;
  if RightOperand.Desc=xtContext then begin
    RightOperand:=RightOperand.Context.Tool.ConvertNodeToExpressionType(
                      RightOperand.Context.Node,Params);
  end;


  // ToDo: search for an overloaded operator

  if WordIsBooleanOperator.DoItUpperCase(Src,BinaryOperator.StartPos,
    BinaryOperator.EndPos-BinaryOperator.StartPos)
  then begin
    // Boolean operators
    // < > <= >= <> in is
    Result.Desc:=xtBoolean;
  end
  else if (BinaryOperator.EndPos-BinaryOperator.StartPos=1)
  and (Src[BinaryOperator.StartPos]='/') then begin
    // real division /
    Result.Desc:=xtConstReal;
  end
  else if WordIsOrdNumberOperator.DoItUpperCase(Src,BinaryOperator.StartPos,
    BinaryOperator.EndPos-BinaryOperator.StartPos)
  then begin
    // ordinal number operator
    // or xor and mod div shl shr
    Result.Desc:=xtConstOrdInteger;
  end
  else if WordIsNumberOperator.DoItUpperCase(Src,BinaryOperator.StartPos,
    BinaryOperator.EndPos-BinaryOperator.StartPos)
  then begin
    // number operator (or string concatenating or set cut)
    // + - *
    
    
    if (Src[BinaryOperator.StartPos]='+')
    and (LeftOperand.Desc in xtAllStringCompatibleTypes)
    then begin
      // string/char '+'
      if (RightOperand.Desc in xtAllStringCompatibleTypes)
      then
        Result.Desc:=xtConstString
      else begin
        MoveCursorToCleanPos(BinaryOperator.EndPos);
        ReadNextAtom;
        RaiseExceptionFmt(ctsIncompatibleTypesGotExpected,
                          ['char',ExpressionTypeDescNames[RightOperand.Desc]]);
      end;
    end else begin
      if (LeftOperand.Desc in xtAllRealTypes)
      or (RightOperand.Desc in xtAllRealTypes) then
        Result.Desc:=xtConstReal
      else if (LeftOperand.Desc=xtPointer)
      or (RightOperand.Desc=xtPointer)
      or ((LeftOperand.Desc=xtContext)
        and (LeftOperand.Context.Node.Desc=ctnPointerType))
      or ((RightOperand.Desc=xtContext)
        and (RightOperand.Context.Node.Desc=ctnPointerType))
      then
        Result.Desc:=xtPointer
      else
        Result.Desc:=xtConstOrdInteger;
    end;
  end else
    // ???
    Result:=RightOperand;
end;

function TFindDeclarationTool.IsParamExprListCompatibleToNodeList(
  FirstTargetParameterNode: TCodeTreeNode;
  SourceExprParamList: TExprTypeList;  IgnoreMissingParameters: boolean;
  Params: TFindDeclarationParams;
  CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
// tests if SourceExprParamList fits into the TargetFirstParameterNode
var
  ParamNode: TCodeTreeNode;
  i, MinParamCnt, MaxParamCnt: integer;
  ParamCompatibility: TTypeCompatibility;
begin
  // quick check: parameter count
  ParamNode:=FirstTargetParameterNode;
  MinParamCnt:=0;
  while (ParamNode<>nil)
  and ((ParamNode.SubDesc and ctnsHasDefaultValue)=0) do begin
    ParamNode:=ParamNode.NextBrother;
    inc(MinParamCnt);
  end;
  MaxParamCnt:=MinParamCnt;
  while (ParamNode<>nil) do begin
    ParamNode:=ParamNode.NextBrother;
    inc(MaxParamCnt);
  end;

  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.IsParamExprListCompatibleToNodeList] ',
  ' ExprParamList.Count=',dbgs(SourceExprParamList.Count),
  ' MinParamCnt=',dbgs(MinParamCnt),' MaxParamCnt=',dbgs(MaxParamCnt)
  );
    try
  {$ENDIF}
  Result:=tcExact;

  if (SourceExprParamlist.Count>MaxParamCnt)
  or ((not IgnoreMissingParameters) and (SourceExprParamList.Count<MinParamCnt))
  then begin
    Result:=tcIncompatible;
    exit;
  end;

  // check each parameter for compatibility
  ParamNode:=FirstTargetParameterNode;
  i:=0;
  while (ParamNode<>nil) and (i<SourceExprParamList.Count) do begin
    ParamCompatibility:=IsCompatible(ParamNode,SourceExprParamList.Items[i],
                                     Params);
    {$IFDEF ShowExprEval}
    DebugLn('[TFindDeclarationTool.IsParamExprListCompatibleToNodeList] B ',ExprTypeToString(SourceExprParamList.Items[i]));
    {$ENDIF}
    if CompatibilityList<>nil then
      CompatibilityList[i]:=ParamCompatibility;
    if ParamCompatibility=tcIncompatible then begin
      Result:=tcIncompatible;
      exit;
    end else if ParamCompatibility=tcCompatible then begin
      Result:=tcCompatible;
    end;
    ParamNode:=ParamNode.NextBrother;
    inc(i);
  end;
  if (i<SourceExprParamList.Count) then begin
    // there are more expressions, then the param list has variables
    Result:=tcIncompatible;
  end else if (ParamNode<>nil) then begin
    // there are not enough expressions for the param list
    // -> check if missing variables have default variables
    if (ParamNode.SubDesc and ctnsHasDefaultValue)>0 then begin
      // the rest params have default values
      while ParamNode<>nil do begin
        if CompatibilityList<>nil then
          CompatibilityList[i]:=tcExact;
        ParamNode:=ParamNode.NextBrother;
        inc(i);
      end;
    end else if not IgnoreMissingParameters then begin
      // not enough expression for param list
      // -> incompatible
      Result:=tcIncompatible;
    end;
  end;
  {$IFDEF ShowExprEval}
    finally
      DebugLn('[TFindDeclarationTool.IsParamExprListCompatibleToNodeList] END ',
      ' Result=',TypeCompatibilityNames[Result],' ! ONLY VALID if no error !'
      );
    end;
  {$ENDIF}
end;

function TFindDeclarationTool.IsParamNodeListCompatibleToExprList(
  TargetExprParamList: TExprTypeList; FirstSourceParameterNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
// tests if FirstSourceParameterNode fits (i.e. can be assigned) into
// the TargetExprParamList
var
  ParamNode: TCodeTreeNode;
  i, MinParamCnt, MaxParamCnt: integer;
  ParamCompatibility: TTypeCompatibility;
  SourceExprType: TExpressionType;
begin
  // quick check: parameter count

  MinParamCnt:=0;
  ParamNode:=FirstSourceParameterNode;
  while (ParamNode<>nil) do begin
    ParamNode:=ParamNode.NextBrother;
    inc(MinParamCnt);
  end;
  MaxParamCnt:=MinParamCnt;

  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.IsParamNodeListCompatibleToExprList] ',
  ' ExprParamList.Count=',dbgs(TargetExprParamList.Count),' ',
  ' MinParamCnt=',dbgs(MinParamCnt),' MaxParamCnt=',dbgs(MaxParamCnt)
  );
    try
  {$ENDIF}
  Result:=tcExact;

  if (TargetExprParamList.Count<>MaxParamCnt) then begin
    Result:=tcIncompatible;
    exit;
  end;

  // check each parameter for compatibility
  
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.IsParamNodeListCompatibleToExprList] ',
    ' ExprParamList=[',TargetExprParamList.AsString,']');
  {$ENDIF}
  ParamNode:=FirstSourceParameterNode;
  i:=0;
  while (ParamNode<>nil) and (i<TargetExprParamList.Count) do begin
    SourceExprType:=ConvertNodeToExpressionType(ParamNode,Params);
    ParamCompatibility:=IsCompatible(TargetExprParamList.Items[i],
                                     SourceExprType,Params);
    {$IFDEF ShowExprEval}
    DebugLn('[TFindDeclarationTool.IsParamNodeListCompatibleToExprList] B ',ExprTypeToString(TargetExprParamList.Items[i]));
    {$ENDIF}
    if CompatibilityList<>nil then
      CompatibilityList[i]:=ParamCompatibility;
    if ParamCompatibility=tcIncompatible then begin
      Result:=tcIncompatible;
      exit;
    end else if ParamCompatibility=tcCompatible then begin
      Result:=tcCompatible;
    end;
    ParamNode:=ParamNode.NextBrother;
    inc(i);
  end;
  if (ParamNode<>nil) or (i<TargetExprParamList.Count) then
    RaiseException('Internal Error: one param list has changed');
    
  {$IFDEF ShowExprEval}
    finally
      DebugLn('[TFindDeclarationTool.IsParamNodeListCompatibleToExprList] END ',
      ' Result=',TypeCompatibilityNames[Result],' ! ONLY VALID if no error !'
      );
    end;
  {$ENDIF}
end;

function TFindDeclarationTool.IsParamNodeListCompatibleToParamNodeList(
  FirstTargetParameterNode, FirstSourceParameterNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
var
  CurParamNode1, CurParamNode2: TCodeTreeNode;
  ParamCompatibility: TTypeCompatibility;
  SourceExprType, TargetExprType: TExpressionType;
  OldFlags: TFindDeclarationFlags;
  i: integer;
begin
  // quick check: parameter count
  CurParamNode1:=FirstTargetParameterNode;
  CurParamNode2:=FirstSourceParameterNode;
  while (CurParamNode1<>nil) and (CurParamNode2<>nil) do begin
    CurParamNode1:=CurParamNode1.NextBrother;
    CurParamNode2:=CurParamNode2.NextBrother;
  end;
  if (CurParamNode1<>nil) or (CurParamNode2<>nil) then begin
    Result:=tcIncompatible;
    exit;
  end;
  
  // check each parameter
  OldFlags:=Params.Flags;
  Params.Flags:=Params.Flags-[fdfFindVariable]+[fdfIgnoreOverloadedProcs];
  CurParamNode1:=FirstTargetParameterNode;
  CurParamNode2:=FirstSourceParameterNode;
  Result:=tcExact;
  i:=0;
  while (CurParamNode1<>nil) and (CurParamNode2<>nil) do begin
    TargetExprType:=ConvertNodeToExpressionType(CurParamNode1,Params);
    SourceExprType:=ConvertNodeToExpressionType(CurParamNode2,Params);
    ParamCompatibility:=IsBaseCompatible(TargetExprType,SourceExprType,Params);
    if CompatibilityList<>nil then
      CompatibilityList[i]:=ParamCompatibility;
    if ParamCompatibility=tcIncompatible then begin
      Result:=tcIncompatible;
      exit;
    end else if ParamCompatibility=tcCompatible then begin
      Result:=tcCompatible;
    end;
    CurParamNode1:=CurParamNode1.NextBrother;
    CurParamNode2:=CurParamNode2.NextBrother;
    inc(i);
  end;
  Params.Flags:=OldFlags;
end;

function TFindDeclarationTool.GetParameterNode(Node: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=Node;
  if Result<>nil then begin
    if (Result.Desc=ctnProperty) then
      Result:=Result.FirstChild
    else if Result.Desc in [ctnProcedure,ctnProcedureHead] then begin
      BuildSubTreeForProcHead(Result);
      if Result.Desc=ctnProcedure then
        Result:=Result.FirstChild;
      if Result.Desc=ctnProcedureHead then
        Result:=Result.FirstChild;
    end;
  end;
end;

function TFindDeclarationTool.GetFirstParameterNode(Node: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=GetParameterNode(Node);
  if Result<>nil then Result:=Result.FirstChild;
end;

function TFindDeclarationTool.CheckSrcIdentifier(
  Params: TFindDeclarationParams;
  const FoundContext: TFindContext): TIdentifierFoundResult;
// this is a TOnIdentifierFound function
//   if identifier found is a proc then it searches for the best overloaded proc
var
  FirstParameterNode, StartContextNode: TCodeTreeNode;
  ParamCompatibility: TTypeCompatibility;
  OldInput: TFindDeclarationInput;
  CurCompatibilityList: TTypeCompatibilityList;
  CompListSize: integer;
begin
  // the search has found an identifier with the right name
  {$IFDEF ShowFoundIdentifier}
  DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
  ' Ident=',GetIdentifier(Params.Identifier),
  ' FoundContext=',FoundContext.Node.DescAsString,
  ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
  );
  {$ENDIF}
  if FoundContext.Node.Desc=ctnProcedure then begin
    // the found node is a proc
    
    // 1. the current identifier cache is blind for parameter lists
    // => proc identifiers can not be identified by the name alone
    // -> do not cache
    // 2. Even if there is only one proc. With different search flags,
    // different routes will be searched and then there can be another proc.
    // The only solution is to store the param expression list and all flags
    // in the cache. This is a ToDo
    Include(Params.Flags,fdfDoNotCache);
    Include(Params.NewFlags,fodDoNotCache);

    if (fdfIgnoreOverloadedProcs in Params.Flags) then begin
      // do not check for overloaded procs -> ident found
      Result:=ifrSuccess;
      exit;
    end;
    
    // Procs can be overloaded, that means there can be several procs with the
    // same name, but with different param lists.
    // The search must go on, and the most compatible proc is returned.
    
    if not Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then begin
      // Params.Identifier is not in the source of the start tool
      // => impossible to check param list, because the context is unknown
      // -> identifier found
      {$IFDEF ShowProcSearch}
      DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Ident=',GetIdentifier(Params.Identifier),
      ' NO SOURCE to check params'
      );
      {$ENDIF}
      Result:=ifrSuccess;
      exit;
    end;
    
    Result:=ifrProceedSearch;
    if (Params.FoundProc=nil) then begin
      // this is the first proc found
      // -> save it and proceed the search to find all overloadeded procs
      {$IFDEF ShowFoundIdentifier}
      DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Ident=',GetIdentifier(Params.Identifier),
      ' FIRST PROC'
      );
      {$ENDIF}
      Params.SetFirstFoundProc(FoundContext);
      exit;
    end;
    
    // -> check which one is more compatible
    // create the input expression list
    // (the expressions in the brackets are parsed and converted to types)
    if Params.FoundProc^.ExprInputList=nil then begin
      {$IFDEF ShowFoundIdentifier}
      DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Ident=',GetIdentifier(Params.Identifier),
      ' Creating Input Expression List ...'
      );
      {$ENDIF}
      if Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then begin
        Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
        StartContextNode:=Params.IdentifierTool.FindDeepestNodeAtPos(
          Params.IdentifierTool.CurPos.StartPos,true);
        if (StartContextNode<>nil) then begin
          if (StartContextNode.Desc in AllPascalStatements) then begin
            {$IFDEF ShowProcSearch}
            DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
            ' Ident=',GetIdentifier(Params.Identifier),
            ' Creating Input Expression List for statement ...'
            );
            {$ENDIF}
            Params.Save(OldInput);
            Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
            Params.Flags:=fdfDefaultForExpressions+Params.Flags*fdfGlobals;
            Params.ContextNode:=StartContextNode;
            Params.OnIdentifierFound:=@Params.IdentifierTool.CheckSrcIdentifier;
            Params.IdentifierTool.ReadNextAtom;
            Params.FoundProc^.ExprInputList:=
              Params.IdentifierTool.CreateParamExprListFromStatement(
                                    Params.IdentifierTool.CurPos.EndPos,Params);
            Params.Load(OldInput);
          end
          else if (StartContextNode.Desc in [ctnProcedureHead,ctnProcedure])
          then begin
            {$IFDEF ShowProcSearch}
            DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
            ' Ident=',GetIdentifier(Params.Identifier),
            ' Creating Input Expression List for proc node ...'
            );
            {$ENDIF}
            Params.FoundProc^.ExprInputList:=
              Params.IdentifierTool.CreateParamExprListFromProcNode(
                                                       StartContextNode,Params);
          end;
        end;
      end;
      if Params.FoundProc^.ExprInputList=nil then begin
        // create expression list without params
        Params.FoundProc^.ExprInputList:=TExprTypeList.Create;
      end;
    end;

    // create compatibility lists for params
    // (each parameter is checked for compatibility)
    CompListSize:=SizeOf(TTypeCompatibility)
                  *Params.FoundProc^.ExprInputList.Count;
    if (CompListSize>0)
    and (Params.FoundProc^.ParamCompatibilityList=nil) then
      GetMem(Params.FoundProc^.ParamCompatibilityList,CompListSize);

    // check the first found proc for compatibility
    // (compare the expression list with the proc param list)
    if not Params.FoundProc^.CacheValid then begin
      {$IFDEF ShowFoundIdentifier}
      DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Ident=',GetIdentifier(Params.Identifier),
      ' Check the first found proc for compatibility ...'
      );
      {$ENDIF}
      FirstParameterNode:=Params.FoundProc^.Context.Tool.GetFirstParameterNode(
                                                Params.FoundProc^.Context.Node);
      Params.Save(OldInput);
      ParamCompatibility:=
        Params.FoundProc^.Context.Tool.IsParamExprListCompatibleToNodeList(
          FirstParameterNode,
          Params.FoundProc^.ExprInputList,
          fdfIgnoreMissingParams in Params.Flags,
          Params,Params.FoundProc^.ParamCompatibilityList);
      Params.Load(OldInput);
      Params.FoundProc^.ProcCompatibility:=ParamCompatibility;
      Params.FoundProc^.CacheValid:=true;
      {$IFDEF ShowFoundIdentifier}
      DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Ident=',GetIdentifier(Params.Identifier),
      ' First Proc ParamCompatibility=',TypeCompatibilityNames[ParamCompatibility]
      );
      {$ENDIF}
      if ParamCompatibility=tcExact then begin
        // the first proc fits exactly -> stop the search
        Params.SetResult(Params.FoundProc^.Context.Tool,
                         Params.FoundProc^.Context.Node.FirstChild);
        Result:=ifrSuccess;
        exit;
      end;
    end;
      
    // check the current proc for compatibility
    // (compare the expression list with the proc param list)
    {$IFDEF ShowFoundIdentifier}
    DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
    ' Ident=',GetIdentifier(Params.Identifier),
    ' Check the current found proc for compatibility ...'
    );
    {$ENDIF}
    if CompListSize>0 then begin
      GetMem(CurCompatibilityList,CompListSize);
    end else begin
      CurCompatibilityList:=nil;
    end;
    try
      FirstParameterNode:=
        FoundContext.Tool.GetFirstParameterNode(FoundContext.Node);
      Params.Save(OldInput);
      ParamCompatibility:=
        FoundContext.Tool.IsParamExprListCompatibleToNodeList(
          FirstParameterNode,
          Params.FoundProc^.ExprInputList,
          fdfIgnoreMissingParams in Params.Flags,
          Params,CurCompatibilityList);
      Params.Load(OldInput);
      {$IFDEF ShowFoundIdentifier}
      DebugLn('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Ident=',GetIdentifier(Params.Identifier),
      ' Current Proc ParamCompatibility=',TypeCompatibilityNames[ParamCompatibility]
      );
      {$ENDIF}
      if ParamCompatibility=tcExact then begin
        // the current proc fits exactly -> stop the search
        Params.ChangeFoundProc(FoundContext,ParamCompatibility,
          CurCompatibilityList);
        CurCompatibilityList:=nil; // set to nil, so that it will not be freed
        Params.SetResult(FoundContext.Tool,FoundContext.Node.FirstChild);
        Result:=ifrSuccess;
      end else if ParamCompatibility=tcCompatible then begin
        // the proc fits not exactly, but is compatible
        if (Params.FoundProc^.ProcCompatibility=tcInCompatible)
        or CompatibilityList1IsBetter(CurCompatibilityList,
          Params.FoundProc^.ParamCompatibilityList,
          Params.FoundProc^.ExprInputList.Count) then
        begin
          // the new proc fits better
          Params.ChangeFoundProc(FoundContext,ParamCompatibility,
            CurCompatibilityList);
          CurCompatibilityList:=nil; // set to nil, so that it will not be freed
        end;
      end;
    finally
      // end overloaded proc search
      if CurCompatibilityList<>nil then
        FreeMem(CurCompatibilityList);
    end;
  end else begin
    Result:=ifrSuccess;
  end;
end;

function TFindDeclarationTool.DoOnIdentifierFound(
  Params: TFindDeclarationParams;
  FoundNode: TCodeTreeNode): TIdentifierFoundResult;
// this internal function is called, whenever an identifier is found
var IsTopLvlIdent: boolean;
begin
  IsTopLvlIdent:=(fdfTopLvlResolving in Params.Flags);
  if Assigned(Params.OnIdentifierFound) then
    Result:=Params.OnIdentifierFound(Params,CreateFindContext(Self,FoundNode))
  else
    Result:=ifrSuccess;
  if (Result=ifrSuccess) and IsTopLvlIdent
  and Assigned(Params.OnTopLvlIdentifierFound) then
    Params.OnTopLvlIdentifierFound(Params,CreateFindContext(Self,FoundNode));
end;

function TFindDeclarationTool.IsCompatible(TargetNode: TCodeTreeNode;
  const ExpressionType: TExpressionType;
  Params: TFindDeclarationParams): TTypeCompatibility;
var TargetContext: TFindContext;
  OldInput: TFindDeclarationInput;
  NodeExprType: TExpressionType;
begin
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.IsCompatible] A Node=',TargetNode.DescAsString,
  ' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc]);
  {$ENDIF}
  Result:=tcIncompatible;
  // find base type of node
  OldInput.Flags:=Params.Flags;
  Include(Params.Flags,fdfExceptionOnNotFound);
  TargetContext:=FindBaseTypeOfNode(Params,TargetNode);
  Params.Flags:=OldInput.Flags;
  
  // compare node base type and ExpressionType
  if (ExpressionType.Context.Node<>nil)
  and (ExpressionType.Context.Node=TargetContext.Node) then begin
    // same base type
    Result:=tcExact;
  end
  else if (TargetContext.Node.Desc=ctnSetType) then begin
    {$IFDEF ShowExprEval}
    DebugLn('[TFindDeclarationTool.IsCompatible] TargetContext.Node.Desc=ctnSetType',
    ' "',copy(TargetContext.Tool.Src,TargetContext.Node.Parent.StartPos,20),'"');
    {$ENDIF}
    if (ExpressionType.Desc=xtConstSet) then begin
      // both are sets, compare type of sets
      if (ExpressionType.SubDesc<>xtNone) then begin

        // ToDo: check if enums of expression fits into enums of target

        // ToDo: ppu, ppw, dcu

        Result:=tcCompatible;
      end else
        // the empty set is compatible to all kinds of sets
        Result:=tcExact;
    end else begin
    
    end;
  end else begin
    NodeExprType:=CleanExpressionType;
    NodeExprType.Desc:=xtContext;
    NodeExprType.Context:=CreateFindContext(Self,TargetNode);
    Result:=IsCompatible(NodeExprType,ExpressionType,Params);
  end;
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.IsCompatible] END',
  ' BaseNode=',TargetContext.Node.DescAsString,
  ' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc],
  ' Result=',TypeCompatibilityNames[Result]
  );
  {$ENDIF}
end;

function TFindDeclarationTool.IsCompatible(TargetType,
  ExpressionType: TExpressionType; Params: TFindDeclarationParams
  ): TTypeCompatibility;
begin
  if TargetType.Desc=xtContext then
    TargetType:=TargetType.Context.Tool.ConvertNodeToExpressionType(
                    TargetType.Context.Node,Params);
  if ExpressionType.Desc=xtContext then
    ExpressionType:=ExpressionType.Context.Tool.ConvertNodeToExpressionType(
                    ExpressionType.Context.Node,Params);
  Result:=IsBaseCompatible(TargetType,ExpressionType,Params);
end;

function TFindDeclarationTool.GetCurrentAtomType: TVariableAtomType;
begin
  if (CurPos.StartPos=CurPos.EndPos) then
    Result:=vatSpace
  else if (CurPos.StartPos>SrcLen) then
    Result:=vatNone
  else if IsIdentStartChar[UpperSrc[CurPos.StartPos]] then begin
    if WordIsPredefinedIdentifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then
      Result:=vatPreDefIdentifier
    else if UpAtomIs('INHERITED') then
      Result:=vatINHERITED
    else if UpAtomIs('AS') then
      Result:=vatAS
    else if WordIsKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos) then
      Result:=vatKeyWord
    else
      Result:=vatIdentifier;
  end
  else if (CurPos.StartPos>=1) and (CurPos.StartPos<=SrcLen)
  and (CurPos.StartPos=CurPos.EndPos-1) then begin
    case Src[CurPos.StartPos] of
    '.': Result:=vatPoint;
    '^': Result:=vatUp;
    '(': Result:=vatRoundBracketOpen;
    ')': Result:=vatRoundBracketClose;
    '[': Result:=vatEdgedBracketOpen;
    ']': Result:=vatEdgedBracketClose;
    '@': Result:=vatAddrOp;
    else Result:=vatNone;
    end;
  end
  else
    Result:=vatNone;
end;

function TFindDeclarationTool.CreateParamExprListFromStatement(
  StartPos: integer; Params: TFindDeclarationParams): TExprTypeList;
var ExprType: TExpressionType;
  BracketClose: char;
  ExprStartPos, ExprEndPos: integer;
  CurIgnoreErrorAfterPos: Integer;
  OldFlags: TFindDeclarationFlags;
  ok: Boolean;

  procedure RaiseBracketNotFound;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[BracketClose,GetAtom]);
  end;
  
begin
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.CreateParamExprListFromStatement] ',
  '"',copy(Src,StartPos,40),'" Context=',Params.ContextNode.DescAsString);
  {$ENDIF}
  Result:=TExprTypeList.Create;
  ok:=false;
  try
    MoveCursorToCleanPos(StartPos);
    ReadNextAtom; // reads first atom after proc name
    if AtomIsChar('(') then
      BracketClose:=')'
    else if AtomIsChar('[') then
      BracketClose:=']'
    else
      BracketClose:=#0;
    if IgnoreErrorAfterValid then
      CurIgnoreErrorAfterPos:=IgnoreErrorAfterCleanedPos
    else
      CurIgnoreErrorAfterPos:=-1;
    OldFlags:=Params.Flags;
    if BracketClose<>#0 then begin
      // read parameter list
      ReadNextAtom;
      if not AtomIsChar(BracketClose) then begin
        // read all expressions
        while true do begin
          ExprStartPos:=CurPos.StartPos;
          // read til comma or bracket close
          repeat
            if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then begin
              ReadTilBracketClose(true);
            end;
            ReadNextAtom;
            if (CurPos.StartPos>SrcLen)
            or (CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose,cafComma])
            then
              break;
          until false;
          ExprEndPos:=CurPos.StartPos;
          // find expression type
          if (CurIgnoreErrorAfterPos>=ExprStartPos) then
            Params.Flags:=Params.Flags-[fdfExceptionOnNotFound];
          //DebugLn('TFindDeclarationTool.CreateParamExprListFromStatement CurIgnoreErrorAfterPos=',dbgs(CurIgnoreErrorAfterPos),' ExprStartPos=',dbgs(ExprStartPos));
          ExprType:=FindExpressionResultType(Params,ExprStartPos,ExprEndPos);
          // add expression type to list
          Result.Add(ExprType);
          MoveCursorToCleanPos(ExprEndPos);
          ReadNextAtom;
          if AtomIsChar(BracketClose) then break;
          if not AtomIsChar(',') then
            RaiseBracketNotFound;
          ReadNextAtom;
        end;
      end;
    end;
    Params.Flags:=OldFlags;
    {$IFDEF ShowExprEval}
    DebugLn('[TFindDeclarationTool.CreateParamExprListFromStatement] END ',
    'ParamCount=',dbgs(Result.Count),' "',copy(Src,StartPos,40),'"');
    DebugLn('  ExprList=[',Result.AsString,']');
    {$ENDIF}
    Ok:=true;
  finally
    if not Ok then Result.Free;
  end;
end;

function TFindDeclarationTool.CreateParamExprListFromProcNode(
  ProcNode: TCodeTreeNode; Params: TFindDeclarationParams): TExprTypeList;
var
  ExprType: TExpressionType;
  ParamNode: TCodeTreeNode;
begin
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.CreateParamExprListFromProcNode] ',
  '"',copy(Src,ProcNode.StartPos,40),'" Context=',ProcNode.DescAsString);
  {$ENDIF}
  Result:=TExprTypeList.Create;
  ParamNode:=GetFirstParameterNode(ProcNode);
  while ParamNode<>nil do begin
    // find expression type
    ExprType:=ConvertNodeToExpressionType(ParamNode,Params);
    // add expression type to list
    Result.Add(ExprType);
    ParamNode:=ParamNode.NextBrother;
  end;
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.CreateParamExprListFromProcNode] END ',
  'ParamCount=',dbgs(Result.Count),' "',copy(Src,ProcNode.StartPos,40),'"');
  DebugLn('  ExprList=[',Result.AsString,']');
  {$ENDIF}
end;

function TFindDeclarationTool.CompatibilityList1IsBetter( List1,
  List2: TTypeCompatibilityList; ListCount: integer): boolean;
// List1 and List2 should only contain tcCompatible and tcExact values
var i: integer;
begin
  // search first difference, start at end
  i:=ListCount-1;
  while (i>=0) and (List1[i]=List2[i]) do dec(i);
  // List1 is better, if first difference is better for List1
  Result:=(i>=0) and (List1[i]=tcExact);
  {$IFDEF ShowFoundIdentifier}
  DebugLn('[TFindDeclarationTool.CompatibilityList1IsBetter] END i=',dbgs(i));
  {$ENDIF}
end;

function TFindDeclarationTool.ContextIsDescendOf(const DescendContext,
  AncestorContext: TFindContext; Params: TFindDeclarationParams): boolean;
  
  procedure RaiseInternalError;
  begin
    RaiseException('[TFindDeclarationTool.ContextIsDescendOf] '
      +' internal error: DescendContext.Desc<>ctnClass');
  end;
  
var CurContext: TFindContext;
  OldInput: TFindDeclarationInput;
begin
  if not (DescendContext.Node.Desc in [ctnClass,ctnClassInterface]) then
    RaiseInternalError;
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.ContextIsDescendOf] ',
  ' DescendContext="',copy(DescendContext.Tool.Src,DescendContext.Node.Parent.StartPos,15),'"');
  {$ENDIF}
  CurContext:=DescendContext;
  Params.Save(OldInput);
  repeat
    Result:=CurContext.Tool.FindAncestorOfClass(CurContext.Node,Params,true);
    if Result then begin
      CurContext:=CreateFindContext(Params);
      {$IFDEF ShowExprEval}
      DebugLn('[TFindDeclarationTool.ContextIsDescendOf] B ',
      ' CurContext="',copy(CurContext.Tool.Src,CurContext.Node.Parent.StartPos,15),'"');
      {$ENDIF}
      Result:=FindContextAreEqual(CurContext,AncestorContext);
      if Result then exit;
    end else
      break;
  until false;
  Result:=false;
end;

function TFindDeclarationTool.IsBaseCompatible(const TargetType,
  ExpressionType: TExpressionType; Params: TFindDeclarationParams
  ): TTypeCompatibility;
// test if ExpressionType can be assigned to TargetType
// both expression types must be base types
var TargetNode, ExprNode: TCodeTreeNode;
begin
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.IsBaseCompatible] B ',
  ' TargetType=',ExprTypeToString(TargetType),
  ' ExpressionType=',ExprTypeToString(ExpressionType));
  {$ENDIF}
  Result:=tcIncompatible;
  if (TargetType.Desc=ExpressionType.Desc) then begin
    case TargetType.Desc of
    
    xtNone: ;
    
    xtContext:
      begin
        TargetNode:=TargetType.Context.Node;
        ExprNode:=ExpressionType.Context.Node;
        {$IFDEF ShowExprEval}
        DebugLn('[TFindDeclarationTool.IsBaseCompatible] C ',
        ' TargetContext="',copy(TargetType.Context.Tool.Src,TargetType.Context.Node.StartPos,20),'"',
        ' ExpressionContext="',copy(ExpressionType.Context.Tool.Src,ExpressionType.Context.Node.StartPos,20),'"'
        );
        {$ENDIF}
        if TargetNode=ExprNode then
          Result:=tcExact
        else
        if ExprNode.Desc=TargetNode.Desc then begin
          // same context type
          case ExprNode.Desc of
          
          ctnClass,ctnClassInterface:
            // check, if ExpressionType.Context is descend of TargetContext
            if ContextIsDescendOf(ExpressionType.Context,
                                  TargetType.Context,Params)
            then
              Result:=tcCompatible;
              
          ctnRangedArrayType,ctnOpenArrayType:
            // ToDo: check range and type of arrayfields
            begin
              Result:=tcCompatible;
            end;

          end;
        end else begin
          // different context type
          
        end;
      end;
    else
      Result:=tcExact;
    end;
    
  end else if ((TargetType.Desc=xtPointer)
      and (ExpressionType.Desc=xtContext)
      and (ExpressionType.Context.Node.Desc in [ctnClass,ctnClassInterface]))
  then begin
    // assigning a class to a pointer
    Result:=tcExact;
    
  end else begin
    // check, if ExpressionType can be auto converted into TargetType
    if ((TargetType.Desc in xtAllRealTypes)
      and (ExpressionType.Desc in xtAllRealConvertibles))
    or ((TargetType.Desc in xtAllStringTypes)
      and (ExpressionType.Desc in xtAllStringConvertibles))
    or ((TargetType.Desc in xtAllWideStringTypes)
      and (ExpressionType.Desc in xtAllWideStringCompatibleTypes))
    or ((TargetType.Desc in xtAllIntegerTypes)
      and (ExpressionType.Desc in xtAllIntegerConvertibles))
    or ((TargetType.Desc in xtAllBooleanTypes)
      and (ExpressionType.Desc in xtAllBooleanConvertibles))
    or ((TargetType.Desc in xtAllPointerTypes)
      and (ExpressionType.Desc in xtAllPointerConvertibles))
    then
      Result:=tcCompatible
    else if (TargetType.Desc=xtContext) then begin
      TargetNode:=TargetType.Context.Node;
      if ((TargetNode.Desc
             in [ctnClass,ctnClassInterface,ctnProcedure])
        and (ExpressionType.Desc=xtNil))
      or ((TargetNode.Desc in [ctnOpenArrayType,ctnRangedArrayType])
        and (TargetNode.LastChild<>nil)
        and (TargetNode.LastChild.Desc=ctnOfConstType)
        and (ExpressionType.Desc=xtConstSet))
      then
        Result:=tcCompatible
    end
    else if (ExpressionType.Desc=xtContext) then begin
      ExprNode:=ExpressionType.Context.Node;
      if (TargetType.Desc=xtFile) and (ExprNode.Desc=ctnFileType)
      then
        Result:=tcCompatible
    end;
  end;
  {$IFDEF ShowExprEval}
  DebugLn('[TFindDeclarationTool.IsBaseCompatible] END ',
  ' TargetType=',ExpressionTypeDescNames[TargetType.Desc],
  ' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc],
  ' Result=',TypeCompatibilityNames[Result]
  );
  {$ENDIF}
end;

function TFindDeclarationTool.CheckParameterSyntax(CursorNode: TCodeTreeNode;
  CleanCursorPos: integer; out ParameterAtom, ProcNameAtom: TAtomPosition; out
  ParameterIndex: integer): boolean;
// check for Identifier(expr,expr,...,expr,VarName
//        or Identifier[expr,expr,...,expr,VarName
// ParameterIndex is 0 based
{off $DEFINE VerboseCPS}

  procedure RaiseBracketNotOpened;
  begin
    if CurPos.Flag=cafRoundBracketClose then
      SaveRaiseExceptionFmt(ctsBracketNotFound,['('])
    else
      SaveRaiseExceptionFmt(ctsBracketNotFound,['[']);
  end;

  function CheckIdentifierAndParameterList: boolean; forward;

  function CheckBrackets: boolean;
  var
    BracketAtom: TAtomPosition;
  begin
    BracketAtom:=CurPos;
    {$IFDEF VerboseCPS}DebugLn('CheckBrackets "',GetAtom,'" BracketAtom=',dbgs(BracketAtom));{$ENDIF}
    repeat
      ReadNextAtom;
      if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then begin
        if (LastAtoms.GetValueAt(0).Flag=cafWord) then begin
          {$IFDEF VerboseCPS}DebugLn('CheckBrackets check word+bracket open');{$ENDIF}
          UndoReadNextAtom;
          if CheckIdentifierAndParameterList() then exit(true);
        end else begin
          {$IFDEF VerboseCPS}DebugLn('CheckBrackets check bracket open');{$ENDIF}
          if CheckBrackets then exit(true);
        end;
      end else if CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose]
      then begin
        if (BracketAtom.Flag=cafRoundBracketOpen)
           =(CurPos.Flag=cafRoundBracketClose)
        then begin
          // closing bracket found, but the variable was not in them
          {$IFDEF VerboseCPS}DebugLn('CheckBrackets bracket closed');{$ENDIF}
          exit(false);
        end else begin
          // invalid closing bracket found
          RaiseBracketNotOpened;
        end;
      end;
    until (CurPos.EndPos>CleanCursorPos);
    Result:=false;
  end;

  function CheckIdentifierAndParameterList: boolean;
  var
    BracketAtom: TAtomPosition;
    CurProcNameAtom: TAtomPosition;
    CurParameterIndex: Integer;
    ParameterStart: integer;
  begin
    Result:=false;
    CurProcNameAtom:=CurPos;
    CurParameterIndex:=0;
    {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList START "',GetAtom,'" ',dbgs(CurProcNameAtom));{$ENDIF}
    ReadNextAtom;
    if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then begin
      BracketAtom:=CurPos;
      ParameterStart:=CurPos.EndPos;
      {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList Bracket="',GetAtom,'"');{$ENDIF}
      repeat
        ReadNextAtom;
        {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList Atom="',GetAtom,'"');{$ENDIF}
        if (CurPos.EndPos>CleanCursorPos)
        or ((CurPos.EndPos=CleanCursorPos) and (CurPos.Flag=cafWord)) then begin
          // parameter found => search parameter expression bounds e.g. ', parameter ,'
          // important: this function should work, even the code behind
          // CleanCursorPos is buggy
          {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList Parameter found, search range ...');{$ENDIF}
          ProcNameAtom:=CurProcNameAtom;
          ParameterIndex:=CurParameterIndex;
          ParameterAtom.StartPos:=ParameterStart;
          ParameterAtom.EndPos:=ParameterStart;
          MoveCursorToCleanPos(ParameterStart);
          repeat
            ReadNextAtom;
            {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList parameter atom "',GetAtom,'"');{$ENDIF}
            if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then
              ReadTilBracketClose(false)
            else
            if (CurPos.Flag in [cafNone,cafComma,cafSemicolon,cafEnd,
                cafRoundBracketClose,cafEdgedBracketClose])
            or ((CurPos.Flag=cafWord)
                and (LastAtoms.GetValueAt(0).Flag=cafWord)
                and (not LastUpAtomIs(0,'INHERITED'))) then
            begin
              // end of parameter expression found
              {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList end of parameter found');{$ENDIF}
              exit(true);
            end else begin
              // atom belongs to the parameter expression
              if ParameterAtom.StartPos=ParameterStart then
                ParameterAtom.StartPos:=CurPos.StartPos;
              ParameterAtom.EndPos:=CurPos.EndPos;
            end;
          until false;
        end;
        if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then begin
          if (LastAtoms.GetValueAt(0).Flag=cafWord) then begin
            {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList check word+bracket open');{$ENDIF}
            UndoReadNextAtom;
            if CheckIdentifierAndParameterList() then exit(true);
          end else begin
            {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList check bracket open');{$ENDIF}
            if CheckBrackets then exit(true);
          end;
        end
        else if CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose] then
        begin
          {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList check bracket close');{$ENDIF}
          if (BracketAtom.Flag=cafRoundBracketOpen)
          =(CurPos.Flag=cafRoundBracketClose)
          then begin
            // parameter list ended in front of Variable => continue search
            {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList parameter list ended in front of cursor');{$ENDIF}
            exit;
          end else begin
            // invalid closing bracket found
            RaiseBracketNotOpened;
          end;
        end;
        // finally after checking the expression: count commas
        if CurPos.Flag=cafComma then begin
          ParameterStart:=CurPos.EndPos;
          inc(CurParameterIndex);
        end;
        {$IFDEF VerboseCPS}DebugLn('CheckIdentifierAndParameterList After parsing atom. atom="',GetAtom,'"');{$ENDIF}
      until (CurPos.EndPos>CleanCursorPos);
    end;
  end;

begin
  Result:=false;
  //DebugLn('TFindDeclarationTool.CheckParameterSyntax START');

  // read code in front to find ProcName and check the syntax
  MoveCursorToNodeStart(CursorNode);
  repeat
    ReadNextAtom;
    {$IFDEF VerboseCPS}DebugLn('TCodeCompletionCodeTool.CheckParameterSyntax ',GetAtom,' ',dbgs(CurPos.EndPos),'<',dbgs(CleanCursorPos));{$ENDIF}
    if CurPos.EndPos>CleanCursorPos then exit;
    if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen])
    and (LastAtoms.GetValueAt(0).Flag=cafWord) then begin
      UndoReadNextAtom;
      if CheckIdentifierAndParameterList then exit(true);
    end;
  until false;

  Result:=true;
end;

function TFindDeclarationTool.FindNthParameterNode(Node: TCodeTreeNode;
  ParameterIndex: integer): TCodeTreeNode;
var
  ProcNode, FunctionNode: TCodeTreeNode;
  ProcHeadNode: TCodeTreeNode;
  ParameterNode: TCodeTreeNode;
  i: Integer;
begin
  Result:=nil;
  if Node=nil then exit;
  if Node.Desc in [ctnProcedure] then begin
    ProcNode:=Node;
    //DebugLn('  FindNthParameterNode ProcNode="',copy(Params.NewCodeTool.Src,ProcNode.StartPos,ProcNode.EndPos-ProcNode.StartPos),'"');
    FunctionNode:=nil;
    BuildSubTreeForProcHead(ProcNode,FunctionNode);
    // find procedure head
    ProcHeadNode:=ProcNode.FirstChild;
    if (ProcHeadNode=nil) or (ProcHeadNode.Desc<>ctnProcedureHead) then begin
      DebugLn('  FindNthParameterNode Procedure has no parameter list');
      exit;
    end;
    // find parameter list
    ParameterNode:=ProcHeadNode.FirstChild;
    if (ParameterNode=nil) or (ParameterNode.Desc<>ctnParameterList)
    then begin
      DebugLn('  FindNthParameterNode Procedure has no parameter list');
      exit;
    end;
    // find parameter
    ParameterNode:=ParameterNode.FirstChild;
    i:=0;
    while (i<ParameterIndex) and (ParameterNode<>nil) do begin
      //DebugLn('  FindNthParameterNode ',ParameterNode.DescAsString);
      ParameterNode:=ParameterNode.NextBrother;
      inc(i);
    end;
    Result:=ParameterNode;
  end;
end;

function TFindDeclarationTool.OpenCodeToolForUnit(UnitNameAtom,
  UnitInFileAtom: TAtomPosition;
  ExceptionOnNotFound: boolean): TFindDeclarationTool;
begin
  // open the unit
  Result:=FindCodeToolForUsedUnit(UnitNameAtom,UnitInFileAtom,
                                  ExceptionOnNotFound);
  if Result=nil then begin
    if ExceptionOnNotFound then begin
      MoveCursorToCleanPos(UnitNameAtom.StartPos);
      RaiseExceptionInstance(
        ECodeToolUnitNotFound.Create(Self,
                                     Format(ctsUnitNotFound,[GetAtom(UnitNameAtom)]),
                                     GetAtom(UnitNameAtom)));
    end;
  end else if Result=Self then begin
    MoveCursorToCleanPos(UnitNameAtom.StartPos);
    RaiseExceptionFmt(ctsIllegalCircleInUsedUnits,[GetAtom(UnitNameAtom)]);
  end;
end;

function TFindDeclarationTool.CheckDirectoryCache: boolean;
begin
  if FDirectoryCache<>nil then exit(true);
  if Assigned(OnGetDirectoryCache) then
    FDirectoryCache:=OnGetDirectoryCache(ExtractFilePath(MainFilename));
  Result:=FDirectoryCache<>nil;
end;

procedure TFindDeclarationTool.DoDeleteNodes;
begin
  ClearNodeCaches(true);
  if FInterfaceIdentifierCache<>nil then
    FInterfaceIdentifierCache.Clear;
  inherited DoDeleteNodes;
end;

function TFindDeclarationTool.NodeCacheGlobalWriteLockStepDidNotChange: boolean;
// checks if a node cache check is in the same GlobalWriteLockStep
// returns true if _no_ update is needed
// returns false, if further checks are needed
var
  GlobalWriteLockIsSet: boolean;
  GlobalWriteLockStep: integer;
begin
  Result:=false;
  if Assigned(OnGetGlobalWriteLockInfo) then begin
    OnGetGlobalWriteLockInfo(GlobalWriteLockIsSet,GlobalWriteLockStep);
    if GlobalWriteLockIsSet then begin
      // The global write lock is set. That means, input variables and code
      // are frozen for all codetools and scanners, and therefore also for all
      // node caches
      if (FLastNodeCachesGlobalWriteLockStep=GlobalWriteLockStep) then begin
        // source and values did not change since last NodeCache check
        Result:=true;
      end else begin
        // this is the first check in this GlobalWriteLockStep
        FLastNodeCachesGlobalWriteLockStep:=GlobalWriteLockStep;
        // proceed normally ...
      end;
    end;
  end;
  {$IFDEF ShowCacheDependencies}
  DebugLn('[TFindDeclarationTool.NodeCacheGlobalWriteLockStepDidNotChange] Result=',
          DbgS(Result),' ',MainFilename);
  {$ENDIF}
end;

function TFindDeclarationTool.CheckDependsOnNodeCaches: boolean;
var
  ANode: TAVLTreeNode;
  ATool: TFindDeclarationTool;
begin
  Result:=false;
  if (FDependsOnCodeTools=nil) or FCheckingNodeCacheDependencies
  or NodeCacheGlobalWriteLockStepDidNotChange
  then exit;

  FCheckingNodeCacheDependencies:=true;
  {$IFDEF ShowCacheDependencies}
  DebugLn('[TFindDeclarationTool.CheckDependsOnNodeCaches] START ',MainFilename);
  {$ENDIF}
  try
    ANode:=FDependsOnCodeTools.FindLowest;
    while ANode<>nil do begin
      ATool:=TFindDeclarationTool(ANode.Data);
      Result:=ATool.CheckDependsOnNodeCaches;
      if Result then exit;
      ANode:=FDependsOnCodeTools.FindSuccessor(ANode);
    end;
    Result:=UpdateNeeded(Scanner.ScanTill=lsrInterface);
  finally
    {$IFDEF ShowCacheDependencies}
    DebugLn('[TFindDeclarationTool.CheckDependsOnNodeCaches] Result=',
            DbgS(Result),' ',MainFilename);
    {$ENDIF}
    FCheckingNodeCacheDependencies:=false;
    if Result then ClearNodeCaches(true);
  end;
end;

destructor TFindDeclarationTool.Destroy;
begin
  FInterfaceIdentifierCache.Free;
  FInterfaceIdentifierCache:=nil;
  FDependsOnCodeTools.Free;
  FDependsOnCodeTools:=nil;
  FDependentCodeTools.Free;
  FDependentCodeTools:=nil;
  if FDirectoryCache<>nil then begin
    FDirectoryCache.Release;
    FDirectoryCache:=nil;
  end;
  inherited Destroy;
end;

procedure TFindDeclarationTool.ClearNodeCaches(Force: boolean);
var
  NodeCache: TCodeTreeNodeCache;
  BaseTypeCache: TBaseTypeCache;
begin
  // check if there is something in cache to delete
  if (FFirstNodeCache=nil) and (FFirstBaseTypeCache=nil)
  and (FRootNodeCache=nil)
  and ((FDependentCodeTools=nil) or (FDependentCodeTools.Count=0))
  and ((FDependsOnCodeTools=nil) or (FDependsOnCodeTools.Count=0)) then
    exit;
  {$IFDEF ShowCacheDependencies}
  DebugLn('[TFindDeclarationTool.ClearNodeCaches] Force=',
          DbgS(Force),' ',MainFilename);
  {$ENDIF}
    
  // quick check: check if in the same GlobalWriteLockStep
  if (not Force) and NodeCacheGlobalWriteLockStepDidNotChange then
    exit;

  // clear node caches
  while FFirstNodeCache<>nil do begin
    NodeCache:=FFirstNodeCache;
    FFirstNodeCache:=NodeCache.Next;
    NodeCacheMemManager.DisposeNodeCache(NodeCache);
  end;
  while FFirstBaseTypeCache<>nil do begin
    BaseTypeCache:=FFirstBaseTypeCache;
    FFirstBaseTypeCache:=BaseTypeCache.Next;
    BaseTypeCacheMemManager.DisposeBaseTypeCache(BaseTypeCache);
  end;
  if FRootNodeCache<>nil then begin
    NodeCacheMemManager.DisposeNodeCache(FRootNodeCache);
    FRootNodeCache:=nil;
  end;
  
  // clear dependent codetools
  ClearDependentNodeCaches;
  ClearDependsOnToolRelationships;
end;

procedure TFindDeclarationTool.ClearDependentNodeCaches;
var
  ANode: TAVLTreeNode;
  DependentTool: TFindDeclarationTool;
begin
  if (FDependentCodeTools=nil) or FClearingDependentNodeCaches then exit;
  FClearingDependentNodeCaches:=true;
  {$IFDEF ShowCacheDependencies}
  DebugLn('[TFindDeclarationTool.ClearDependentNodeCaches] ',MainFilename);
  {$ENDIF}
  try
    ANode:=FDependentCodeTools.FindLowest;
    while ANode<>nil do begin
      DependentTool:=TFindDeclarationTool(ANode.Data);
      DependentTool.ClearNodeCaches(true);
      ANode:=FDependentCodeTools.FindSuccessor(ANode);
    end;
    FDependentCodeTools.Clear;
  finally
    FClearingDependentNodeCaches:=false;
  end;
end;

procedure TFindDeclarationTool.ClearDependsOnToolRelationships;
var
  ANode: TAVLTreeNode;
  DependOnTool: TFindDeclarationTool;
begin
  if FDependsOnCodeTools=nil then exit;
  {$IFDEF ShowCacheDependencies}
  DebugLn('[TFindDeclarationTool.ClearDependsOnToolRelationships] ',MainFilename);
  {$ENDIF}
  ANode:=FDependsOnCodeTools.FindLowest;
  while ANode<>nil do begin
    DependOnTool:=TFindDeclarationTool(ANode.Data);
    if not DependOnTool.FClearingDependentNodeCaches then
      DependOnTool.FDependentCodeTools.Remove(Self);
    ANode:=FDependsOnCodeTools.FindSuccessor(ANode);
  end;
  FDependsOnCodeTools.Clear;
end;

procedure TFindDeclarationTool.AddToolDependency(
  DependOnTool: TFindDeclarationTool);
// build a relationship: this tool depends on DependOnTool
begin
  {$IFDEF ShowCacheDependencies}
  DebugLn('[TFindDeclarationTool.AddToolDependency] "',MainFilename,'" depends on "',DependOnTool.MainFilename,'"');
  {$ENDIF}
  if DependOnTool.FDependentCodeTools=nil then
    DependOnTool.FDependentCodeTools:=TAVLTree.Create;
  if DependOnTool.FDependentCodeTools.Find(Self)=nil then
    DependOnTool.FDependentCodeTools.Add(Self);
  if FDependsOnCodeTools=nil then
    FDependsOnCodeTools:=TAVLTree.Create;
  if FDependsOnCodeTools.Find(DependOnTool)=nil then
    FDependsOnCodeTools.Add(DependOnTool);
end;

function TFindDeclarationTool.ConsistencyCheck: integer;
var ANodeCache: TCodeTreeNodeCache;
begin
  Result:=inherited ConsistencyCheck;
  if Result<>0 then exit;
  if FInterfaceIdentifierCache<>nil then begin

  end;
  ANodeCache:=FFirstNodeCache;
  while ANodeCache<>nil do begin
    Result:=ANodeCache.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,100);
      exit;
    end;
    ANodeCache:=ANodeCache.Next;
  end;
  if FDependentCodeTools<>nil then begin
    Result:=FDependentCodeTools.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,200);
      exit;
    end;
  end;
  if FDependsOnCodeTools<>nil then begin
    Result:=FDependsOnCodeTools.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,300);
      exit;
    end;
  end;
end;

function TFindDeclarationTool.GetNodeCache(Node: TCodeTreeNode;
  CreateIfNotExists: boolean): TCodeTreeNodeCache;
begin
  while (Node<>nil) and (not (Node.Desc in AllNodeCacheDescs)) do
    Node:=Node.Parent;
  if Node<>nil then begin
    if (Node.Cache=nil) and CreateIfNotExists then
      CreateNewNodeCache(Node);
    if (Node.Cache<>nil) and (Node.Cache is TCodeTreeNodeCache) then
      Result:=TCodeTreeNodeCache(Node.Cache)
    else
      Result:=nil;
  end else begin
    if (FRootNodeCache=nil) and CreateIfNotExists then
      FRootNodeCache:=CreateNewNodeCache(nil);
    Result:=FRootNodeCache;
  end;
end;

procedure TFindDeclarationTool.AddResultToNodeCaches(
  StartNode, EndNode: TCodeTreeNode; SearchedForward: boolean;
  Params: TFindDeclarationParams; SearchRangeFlags: TNodeCacheEntryFlags);
var Node: TCodeTreeNode;
  CurNodeCache, LastNodeCache: TCodeTreeNodeCache;
  CleanStartPos, CleanEndPos: integer;
  NewNode: TCodeTreeNode;
  NewTool: TPascalParserTool;
  NewCleanPos: integer;
  {$IFDEF ShowNodeCache}
  BeVerbose: boolean;
  NodeOwner: TObject;
  
  function WriteSrcPos(t: TPascalParserTool; p: integer): string;
  begin
    Result:=StringToPascalConst(copy(t.Src,p-10,10)+'|'+copy(t.Src,p,15)+'"');
  end;
  
  function NodeOwnerAsString(ANodeOwner: TObject): string;
  begin
    if ANodeOwner=nil then
      Result:='nil'
    else if ANodeOwner is TPascalParserTool then
      Result:=ExtractFileName(TPascalParserTool(ANodeOwner).MainFilename)
    else
      Result:='?'+ANodeOwner.ClassName+'?';
  end;
  
  {$ENDIF}
begin
  if StartNode=nil then exit;
  if Params.NewNode<>nil then begin
    // identifier found
    NewNode:=Params.NewNode;
    NewTool:=Params.NewCodeTool;
    NewCleanPos:=Params.NewCleanPos;
  end else begin
    // identifier not found
    NewNode:=nil;
    NewTool:=nil;
    NewCleanPos:=-1;
  end;
  // calculate search range
  if EndNode<>nil then begin
    if SearchedForward then begin
      CleanStartPos:=StartNode.StartPos;
      CleanEndPos:=EndNode.EndPos;
    end else begin
      CleanStartPos:=EndNode.StartPos;
      CleanEndPos:=StartNode.EndPos;
    end;
  end else begin
    // searched till start or end of source
    if not SearchedForward then begin
      CleanStartPos:=1;
      CleanEndPos:=StartNode.StartPos;
    end else begin
      CleanStartPos:=StartNode.StartPos;
      CleanEndPos:=SrcLen+1;
    end;
  end;

  {$IFDEF ShowNodeCache}
  beVerbose:=CompareSrcIdentifiers(Params.Identifier,'InitDecompressor');
  if beVerbose then begin
    DebugLn('(((((((((((((((((((((((((((==================');
    
    DbgOut('TFindDeclarationTool.AddResultToNodeCaches ',
    ' Ident=',GetIdentifier(Params.Identifier));
    DbgOut(' SearchedForward=',DbgS(SearchedForward));
    DbgOut(' Flags=[');
    if ncefSearchedInParents in SearchRangeFlags then DbgOut('Parents');
    if ncefSearchedInAncestors in SearchRangeFlags then DbgOut(',Ancestors');
    DebugLn(']');
    
    DbgOut('     StartNode=',StartNode.DescAsString,
      '('+DbgS(StartNode.StartPos),'-',DbgS(StartNode.EndPos)+')=',
      WriteSrcPos(Self,StartNode.StartPos));
    NodeOwner:=FindOwnerOfCodeTreeNode(StartNode);
    if NodeOwner<>Self then DbgOut(' StartNodeOwner=',NodeOwnerAsString(NodeOwner));
    DebugLn('');
    
    if EndNode<>nil then
      DbgOut(' EndNode=',EndNode.DescAsString,
        '('+DbgS(EndNode.StartPos),'-',DbgS(EndNode.EndPos)+')=',
        WriteSrcPos(Self,EndNode.StartPos))
    else
      DbgOut(' EndNode=nil');
    NodeOwner:=FindOwnerOfCodeTreeNode(EndNode);
    if NodeOwner<>Self then DbgOut(' EndNodeOwner=',NodeOwnerAsString(NodeOwner));
    DebugLn('');

    DebugLn('     Self=',ExtractFileName(MainFilename));
    
    if NewNode<>nil then begin
      DebugLn('       NewNode=',NewNode.DescAsString,
              '(',DbgS(NewNode.StartPos),'-',DbgS(NewNode.EndPos),')=',
              WriteSrcPos(NewTool,NewNode.StartPos),
                 ' NewTool=',ExtractFileName(NewTool.MainFilename));
    end else begin
      DebugLn('       NOT FOUND');
    end;
    
    DebugLn('  CleanStartPos=',DbgS(CleanStartPos),' ',WriteSrcPos(Self,CleanStartPos));
    DebugLn('  CleanEndPos=',DbgS(CleanEndPos),' ',WriteSrcPos(Self,CleanEndPos));
  end;
  {$ENDIF}
  LastNodeCache:=nil;
  // start with parent of deepest node and end parent of highest
  Node:=StartNode;
  if (EndNode<>nil) then begin
    if (EndNode.GetLevel>StartNode.GetLevel) then begin
      Node:=EndNode;
      EndNode:=StartNode.Parent;
    end else begin
      EndNode:=EndNode.Parent;
    end;
  end else
    EndNode:=StartNode.Parent;
  Node:=Node.Parent;
  while (Node<>nil) do begin
    if (Node.Desc in AllNodeCacheDescs) then begin
      if (Node.Cache=nil) then
        CreateNewNodeCache(Node);
      if (Node.Cache is TCodeTreeNodeCache) then begin
        CurNodeCache:=TCodeTreeNodeCache(Node.Cache);
        if LastNodeCache<>CurNodeCache then begin
          {$IFDEF ShowNodeCache}
          if BeVerbose then begin
            CurNodeCache.WriteDebugReport('  BEFORE NODECACHE REPORT: ');
          end;
          {$ENDIF}
          CurNodeCache.Add(Params.Identifier,
                           CleanStartPos,CleanEndPos,
                           NewNode,NewTool,NewCleanPos,SearchRangeFlags);
          {$IFDEF ShowNodeCache}
          if BeVerbose then begin
            CurNodeCache.WriteDebugReport('  AFTER NODECACHE REPORT: ');
          end;
          {$ENDIF}
          LastNodeCache:=CurNodeCache;
        end;
      end;
    end;
    Node:=Node.Parent;
    if (EndNode=Node) then break;
  end;
  {$IFDEF ShowNodeCache}
  if BeVerbose then begin
    DebugLn('=========================))))))))))))))))))))))))))))))))');
  end;
  {$ENDIF}
end;

function TFindDeclarationTool.CreateNewNodeCache(
  Node: TCodeTreeNode): TCodeTreeNodeCache;
begin
  Result:=NodeCacheMemManager.NewNodeCache(Node);
  Result.Next:=FFirstNodeCache;
  FFirstNodeCache:=Result;
end;

function TFindDeclarationTool.CreateNewBaseTypeCache(Node: TCodeTreeNode
  ): TBaseTypeCache;
begin
  Result:=BaseTypeCacheMemManager.NewBaseTypeCache(Node);
  Result.Next:=FFirstBaseTypeCache;
  FFirstBaseTypeCache:=Result;
end;

procedure TFindDeclarationTool.CreateBaseTypeCaches(
  NodeStack: PCodeTreeNodeStack; const Result: TFindContext);
var i: integer;
  Node: TCodeTreeNodeStackEntry;
  BaseTypeCache: TBaseTypeCache;
begin
  {$IFDEF ShowBaseTypeCache}
  DbgOut('[TFindDeclarationTool.CreateBaseTypeCaches] ',
  ' StackPtr=',DbgS(NodeStack^.StackPtr));
  DebugLn(' Self=',MainFilename);
  if Result.Node<>nil then
    DbgOut(' Result='+Result.Node.DescAsString,
       ' Start='+DbgS(Result.Node.StartPos),
       ' End='+DbgS(Result.Node.EndPos),
       ' "'+copy(Src,Result.Node.StartPos,15)+'" ',Result.Tool.MainFilename)
  else
    DbgOut(' Result=nil');
  DebugLn('');
  {$ENDIF}
  for i:=0 to (NodeStack^.StackPtr-1) do begin
    Node:=GetNodeStackEntry(NodeStack,i);
    if (Node.Cache=nil)
    and ((Result.Tool<>Self) or (Result.Node<>Node)) then begin
      {$IFDEF ShowBaseTypeCache}
      DebugLn('  i=',DbgS(i),' Node=',Node.DescAsString,' "',copy(Src,Node.StartPos,15),'"');
      {$ENDIF}
      BaseTypeCache:=CreateNewBaseTypeCache(Node);
      if BaseTypeCache<>nil then begin
        BaseTypeCache.NewNode:=Result.Node;
        BaseTypeCache.NewTool:=Result.Tool;
      end;
    end;
  end;
end;

function TFindDeclarationTool.GetExpressionTypeOfTypeIdentifier(
  Params: TFindDeclarationParams): TExpressionType;
var
  OldFlags: TFindDeclarationFlags;
begin
  OldFlags:=Params.Flags;
  if FindIdentifierInContext(Params) then begin
    Params.Flags:=OldFlags;
    Result:=Params.NewCodeTool.ConvertNodeToExpressionType(Params.NewNode,Params);
  end else begin
    // predefined identifier
    Params.Flags:=OldFlags;
    Result:=CleanExpressionType;
    Result.Desc:=PredefinedIdentToExprTypeDesc(Params.Identifier);
  end;
end;

function TFindDeclarationTool.FindTermTypeAsString(TermAtom: TAtomPosition;
  CursorNode: TCodeTreeNode; Params: TFindDeclarationParams): string;

  procedure RaiseTermNotSimple;
  begin
    MoveCursorToCleanPos(TermAtom.StartPos);
    RaiseException(ctsTermNotSimple);
  end;

var
  ExprType: TExpressionType;
  FindContext: TFindContext;
  ANode: TCodeTreeNode;
begin
  Result:='';
  Params.ContextNode:=CursorNode;
  Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors,
                 fdfTopLvlResolving,fdfFunctionResult];
  ExprType:=FindExpressionResultType(Params,TermAtom.StartPos,TermAtom.EndPos);
  {$IFDEF ShowExprEval}
  DebugLn('TFindDeclarationTool.FindTermTypeAsString ExprTypeToString=',
    ExprTypeToString(ExprType));
  {$ENDIF}
  case ExprType.Desc of
    xtNone:
      RaiseTermNotSimple;

    xtContext:
      begin
        FindContext:=ExprType.Context;
        if not (FindContext.Node.Desc in AllIdentifierDefinitions) then
        begin
          if (FindContext.Node.Parent<>nil)
          and (FindContext.Node.Parent.Desc in AllIdentifierDefinitions) then
          begin
            FindContext.Node:=FindContext.Node.Parent;
          end else begin
            Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors,
                           fdfTopLvlResolving,fdfFunctionResult];
            FindContext:=ExprType.Context.Tool.FindBaseTypeOfNode(Params,
                                                         ExprType.Context.Node);
          end;
        end;
        
        // ToDo: PPU, PPW, DCU
      
        case FindContext.Node.Desc of
        
        ctnTypeDefinition:
          Result:=GetIdentifier(
                              @FindContext.Tool.Src[FindContext.Node.StartPos]);
                              
        ctnVarDefinition,ctnConstDefinition:
          begin
            ANode:=FindContext.Tool.FindTypeNodeOfDefinition(FindContext.Node);
            if (ANode=nil) or (ANode.Desc<>ctnIdentifier) then
              RaiseTermNotSimple;
            Result:=GetIdentifier(@FindContext.Tool.Src[ANode.StartPos]);
          end;
          
        ctnClass, ctnClassInterface:
          Result:=GetIdentifier(
                       @FindContext.Tool.Src[FindContext.Node.Parent.StartPos]);

        ctnEnumerationType:
          if (FindContext.Node.Parent<>nil)
          and (FindContext.Node.Parent.Desc=ctnTypeDefinition)
          then
            Result:=GetIdentifier(
                     @FindContext.Tool.Src[FindContext.Node.Parent.StartPos]);

        ctnProperty,ctnGlobalProperty:
          begin
            FindContext.Tool.MoveCursorToPropType(FindContext.Node);
            Result:=FindContext.Tool.GetAtom;
          end;
          
        end;
        
        if Result='' then begin
          DebugLn('TFindDeclarationTool.FindTermTypeAsString ContextNode=',
            FindContext.Node.DescAsString);
          RaiseTermNotSimple;
        end;
      end;

    xtChar,
    xtWideChar,
    xtReal,
    xtSingle,
    xtDouble,
    xtExtended,
    xtCurrency,
    xtComp,
    xtInt64,
    xtCardinal,
    xtQWord,
    xtPChar,
    xtPointer,
    xtFile,
    xtText,
    xtLongint,
    xtLongWord,
    xtSmallInt,
    xtShortInt,
    xtByte,
    xtWord:
      Result:=ExpressionTypeDescNames[ExprType.Desc];

    xtBoolean,
    xtByteBool,
    xtLongBool:
      Result:=ExpressionTypeDescNames[xtBoolean];

    xtString,
    xtAnsiString,
    xtShortString:
      Result:=ExpressionTypeDescNames[xtString];

    xtWideString:
      Result:=ExpressionTypeDescNames[ExprType.Desc];

    xtConstOrdInteger:
      Result:='Integer';
    xtConstString:
      Result:=ExpressionTypeDescNames[xtString];
    xtConstReal:
      Result:=ExpressionTypeDescNames[xtExtended];
    xtConstSet:
      RaiseTermNotSimple;
    xtConstBoolean:
      Result:=ExpressionTypeDescNames[xtBoolean];
    xtNil:
      RaiseTermNotSimple;
  else
    DebugLn('TCodeCompletionCodeTool.FindTermTypeAsString ExprTypeToString=',
      ExprTypeToString(ExprType));
    RaiseTermNotSimple;
  end;
end;


{ TFindDeclarationParams }

constructor TFindDeclarationParams.Create;
begin
  inherited Create;
  Clear;
end;

destructor TFindDeclarationParams.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TFindDeclarationParams.Clear;
begin
  ClearInput;
  ClearFoundProc;
  ClearResult(false);
  OnTopLvlIdentifierFound:=nil;
end;

procedure TFindDeclarationParams.Load(var Input: TFindDeclarationInput);
begin
  Flags:=Input.Flags;
  Identifier:=Input.Identifier;
  ContextNode:=Input.ContextNode;
  OnIdentifierFound:=Input.OnIdentifierFound;
  IdentifierTool:=Input.IdentifierTool;
  if FoundProc<>Input.FoundProc then begin
    ClearFoundProc;
    FoundProc:=Input.FoundProc;
  end;
end;

procedure TFindDeclarationParams.Save(var Input: TFindDeclarationInput);
begin
  Input.Flags:=Flags;
  Input.Identifier:=Identifier;
  Input.ContextNode:=ContextNode;
  Input.OnIdentifierFound:=OnIdentifierFound;
  Input.IdentifierTool:=IdentifierTool;
  Input.FoundProc:=FoundProc;
end;

procedure TFindDeclarationParams.ClearResult(CopyCacheFlags: boolean);
begin
  NewPos.Code:=nil;
  NewPos.X:=-1;
  NewPos.Y:=-1;
  NewTopLine:=-1;
  NewNode:=nil;
  NewCleanPos:=-1;
  NewCodeTool:=nil;
  NewFlags:=[];
  if CopyCacheFlags and (fdfDoNotCache in Flags) then
    Include(NewFlags,fodDoNotCache);
end;

procedure TFindDeclarationParams.SetResult(const AFindContext: TFindContext);
begin
  ClearResult(true);
  NewCodeTool:=AFindContext.Tool;
  NewNode:=AFindContext.Node;
end;

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TFindDeclarationTool;
  ANewNode: TCodeTreeNode);
begin
  ClearResult(true);
  NewCodeTool:=ANewCodeTool;
  NewNode:=ANewNode;
end;

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TFindDeclarationTool;
  ANewNode: TCodeTreeNode; ANewCleanPos: integer);
begin
  ClearResult(true);
  NewCodeTool:=ANewCodeTool;
  NewNode:=ANewNode;
  NewCleanPos:=ANewCleanPos;
end;

procedure TFindDeclarationParams.ConvertResultCleanPosToCaretPos;
begin
  NewPos.Code:=nil;
  if NewCodeTool<>nil then begin
    if (NewCleanPos>=1) then
      NewCodeTool.CleanPosToCaretAndTopLine(NewCleanPos,NewPos,NewTopLine)
    else if (NewNode<>nil) then
      NewCodeTool.CleanPosToCaretAndTopLine(NewNode.StartPos,NewPos,NewTopLine);
  end;
end;

procedure TFindDeclarationParams.ClearInput;
begin
  Flags:=[];
  Identifier:=nil;
  ContextNode:=nil;
  OnIdentifierFound:=nil;
  IdentifierTool:=nil;
end;

procedure TFindDeclarationParams.ClearFoundProc;
begin
  if FoundProc=nil then exit;
  with FoundProc^ do begin
    if ExprInputList<>nil then
      FreeAndNil(ExprInputList);
    if ParamCompatibilityList<>nil then begin
      FreeMem(ParamCompatibilityList);
      ParamCompatibilityList:=nil;
    end;
    CacheValid:=false;
  end;
  Dispose(FoundProc);
  FoundProc:=nil;
end;

procedure TFindDeclarationParams.WriteDebugReport;
begin
  DebugLn('TFindDeclarationParams.WriteDebugReport Self=',DbgS(Self));

  // input parameters:
  DebugLn(' Flags=',FindDeclarationFlagsAsString(Flags));
  DebugLn(' Identifier=',GetIdentifier(Identifier));
  if ContextNode<>nil then
    DebugLn(' ContextNode=',ContextNode.DescAsString)
  else
    DebugLn(' ContextNode=nil');
  if OnIdentifierFound<>nil then
    DebugLn(' OnIdentifierFound=',TFindDeclarationTool(TMethod(OnIdentifierFound).Data).MainFilename);
  if IdentifierTool<>nil then
    DebugLn(' IdentifierTool=',IdentifierTool.MainFilename)
  else
    DebugLn(' IdentifierTool=nil');
  if FoundProc<>nil then begin
    DebugLn(' FoundProc<>nil');
  end;

  // global params
  if OnTopLvlIdentifierFound<>nil then
    DebugLn(' OnTopLvlIdentifierFound=',TFindDeclarationTool(TMethod(OnTopLvlIdentifierFound).Code).MainFilename);

  // results:
  if NewNode<>nil then
    DebugLn(' NewNode=',NewNode.DescAsString)
  else
    DebugLn(' NewNode=nil');
  DebugLn(' NewCleanPos=',dbgs(NewCleanPos));
  if NewCodeTool<>nil then
    DebugLn(' NewCodeTool=',NewCodeTool.MainFilename)
  else
    DebugLn(' NewCodeTool=nil');
  if NewPos.Code<>nil then
    DebugLn(' NewPos=',NewPos.Code.Filename,' x=',dbgs(NewPos.X),' y=',dbgs(NewPos.Y),' topline=',dbgs(NewTopLine))
  else
    DebugLn(' NewPos=nil');
  DebugLn(' NewFlags=',FoundDeclarationFlagsAsString(NewFlags));
  DebugLn('');
end;

procedure TFindDeclarationParams.SetIdentifier(
  NewIdentifierTool: TFindDeclarationTool; NewIdentifier: PChar;
  NewOnIdentifierFound: TOnIdentifierFound);
begin
  Identifier:=NewIdentifier;
  IdentifierTool:=NewIdentifierTool;
  OnIdentifierFound:=NewOnIdentifierFound;
  FoundProc:=nil;
end;

procedure TFindDeclarationParams.SetFirstFoundProc(
  const ProcContext: TFindContext);
begin
  New(FoundProc);
  FillChar(FoundProc^,SizeOf(TFoundProc),0);
  FoundProc^.Context:=ProcContext;
end;

procedure TFindDeclarationParams.ChangeFoundProc(
  const ProcContext: TFindContext;
  ProcCompatibility: TTypeCompatibility;
  ParamCompatibilityList: TTypeCompatibilityList);
begin
  FoundProc^.Context:=ProcContext;
  FoundProc^.ProcCompatibility:=ProcCompatibility;
  if (FoundProc^.ParamCompatibilityList<>ParamCompatibilityList) then begin
    if (FoundProc^.ParamCompatibilityList<>nil) then
      FreeMem(FoundProc^.ParamCompatibilityList);
    FoundProc^.ParamCompatibilityList:=ParamCompatibilityList;
  end;
end;

procedure TFindDeclarationParams.SetResult(
  NodeCacheEntry: PCodeTreeNodeCacheEntry);
begin
  ClearResult(true);
  NewCodeTool:=TFindDeclarationTool(NodeCacheEntry^.NewTool);
  NewNode:=NodeCacheEntry^.NewNode;
  NewCleanPos:=NodeCacheEntry^.NewCleanPos;
end;


{ TExprTypeList }

destructor TExprTypeList.Destroy;
begin
  if Items<>nil then FreeMem(Items);
end;

function TExprTypeList.AsString: string;
var i: integer;
begin
  Result:='';
  for i:=0 to Count-1 do begin
    Result:=Result+'{'+ExprTypeToString(Items[i])+'}'#13#10;
  end;
end;

procedure TExprTypeList.SetCapacity(const AValue: integer);
var NewSize: integer;
begin
  if FCapacity=AValue then exit;
  FCapacity:=AValue;
  NewSize:=FCapacity*SizeOf(TExpressionType);
  if Items=nil then
    GetMem(Items,NewSize)
  else
    ReAllocMem(Items,NewSize);
  if Count>Capacity then Count:=Capacity;
end;

procedure TExprTypeList.Grow;
begin
  Capacity:=Capacity+5;
end;

procedure TExprTypeList.Add(const ExprType: TExpressionType);
begin
  inc(Count);
  if Count>Capacity then Grow;
  Items[Count-1]:=ExprType;
end;

procedure TExprTypeList.AddFirst(const ExprType: TExpressionType);
begin
  inc(Count);
  if Count>Capacity then Grow;
  if Count>1 then
    Move(Items[0],Items[1],SizeOf(TExpressionType)*(Count-1));
  Items[0]:=ExprType;
end;


end.


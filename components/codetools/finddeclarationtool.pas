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
    TFindDeclarationTool enhances the TPascalParserTool with the ability
    to find the source position or code tree node of a declaration.


  ToDo:
    - many things, search for 'ToDo'
    - ignore errors behind cursor (implemented, not tested)
    - variants
    - interfaces
    - Get and Set property access parameter lists
    - predefined funcs Pred, Succ, Val, Low, High
    - find declaration in dead code
    - make @Proc context sensitive (started but not complete)
    - operator overloading
    - ppu, ppw, dcu files
}
unit FindDeclarationTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

// activate for debug:

// mem check
{ $DEFINE MEM_CHECK}

// verbosity
{ $DEFINE CTDEBUG}
{ $DEFINE ShowSearchPaths}
{ $DEFINE ShowTriedFiles}
{ $DEFINE ShowTriedContexts}
{ $DEFINE ShowTriedParentContexts}
{ $DEFINE ShowTriedIdentifiers}
{ $DEFINE ShowExprEval}
{ $DEFINE ShowFoundIdentifier}
{ $DEFINE ShowInterfaceCache}
{ $DEFINE ShowNodeCache}
{ $DEFINE ShowBaseTypeCache}
{ $DEFINE ShowCacheDependencies}
{ $DEFINE ShowCollect}

{$IFDEF CTDEBUG}{$DEFINE DebugPrefix}{$ENDIF}
{$IFDEF ShowTriedIdentifiers}{$DEFINE DebugPrefix}{$ENDIF}

// new features
{ $DEFINE IgnoreErrorAfterCursor}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, CustomCodeTool,
  SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree,
  TypInfo, PascalParserTool, FileProcs, DefineTemplates, FindDeclarationCache;

type
  TFindDeclarationTool = class;

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
    vatAddrOp            // @
    );
    
const
  // for nicer output
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
     'AddrOperator@ '
     );
     
type
  // searchpath delimiter is semicolon
  TOnGetSearchPath = function(Sender: TObject): string of object;
  
  TOnGetCodeToolForBuffer = function(Sender: TObject;
    Code: TCodeBuffer): TFindDeclarationTool of object;

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
                            // is an predefined exception
                            
    fdfIgnoreClassVisibility,//find inaccessible private+protected fields
    fdfClassPublished,
    fdfClassPublic,
    fdfClassProtected,
    fdfClassPrivate,
    
    fdfIgnoreMissingParams, // found proc fits, even if parameters are missing
    fdfOnlyCompatibleProc,  // incompatible procs are ignored
    fdfIgnoreOverloadedProcs,// ignore param lists and take the first proc found
    
    fdfFindVariable,        // do not search for the base type of a variable,
                            //   instead return the variable declaration
    fdfFunctionResult,      // if function is found, return result type
    
    fdfCollect,             // return every reachable identifier
    fdfTopLvlResolving      // set, when searching for an identifier of the
                            //   top lvl variable
    );
  TFindDeclarationFlags = set of TFindDeclarationFlag;
  
const
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
    'fdfClassPublished',
    'fdfClassPublic',
    'fdfClassProtected',
    'fdfClassPrivate',
    'fdfIgnoreMissingParams',
    'fdfOnlyCompatibleProc',
    'fdfIgnoreOverloadedProcs',
    'fdfFindVariable',
    'fdfFunctionResult',
    'fdfCollect',
    'fdfTopLvlResolving'
  );

type
  // flags/states for result
  TFoundDeclarationFlag = (
      fdfDoNotCache
    );
  TFoundDeclarationFlags = set of TFoundDeclarationFlag;


  TFindDeclarationParams = class;
  
  TFindContext = record
    Node: TCodeTreeNode;
    Tool: TFindDeclarationTool;
  end;
  
const
  CleanFindContext: TFindContext = (Node:nil; Tool:nil);
  
type
  { TExpressionTypeDesc describes predefined types
    The Freepascal compiler can automatically convert them
  }
  TExpressionTypeDesc = (
    xtNone,      // undefined
    xtContext,   // a node
    xtChar,      // char
    xtReal,      // real
    xtSingle,    // single
    xtDouble,    // double
    xtExtended,  // extended
    xtCurrency,  // currency
    xtComp,      // comp
    xtInt64,     // int64
    xtCardinal,  // cardinal
    xtQWord,     // qword
    xtBoolean,   // boolean
    xtByteBool,  // bytebool
    xtLongBool,  // longbool
    xtString,    // string
    xtAnsiString,// ansistring
    xtShortString,// shortstring
    xtWideString,// widestring
    xtPChar,     // pchar
    xtPointer,   // pointer
    xtFile,      // file
    xtText,      // text
    xtConstOrdInteger,// enums, number, integer
    xtConstString,// string, string constant, char constant
    xtConstReal, // real number
    xtConstSet,  // [] set
    xtConstBoolean,// true, false
    xtLongint,   // longint
    xtWord,      // word
    xtNil        // nil  = pointer, class, procedure, method, ...
    );
  TExpressionTypeDescs = set of TExpressionTypeDesc;
  
const
  ExpressionTypeDescNames: array[TExpressionTypeDesc] of string = (
    'None', 'Context', 'Char', 'Real', 'Single', 'Double',
    'Extended', 'Currency', 'Comp', 'Int64', 'Cardinal', 'QWord', 'Boolean',
    'ByteBool', 'LongBool', 'String', 'AnsiString', 'ShortString', 'WideString',
    'PChar', 'Pointer', 'File', 'TextFile', 'ConstOrdInt', 'ConstString',
    'ConstReal', 'ConstSet', 'ConstBoolean', 'LongInt', 'Word', 'Nil'
  );

  xtAllTypes = [Low(TExpressionTypeDesc)..High(TExpressionTypeDesc)]-[xtNone];
  xtAllPredefinedTypes = xtAllTypes-[xtContext];
  xtAllIntegerTypes = [xtInt64, xtQWord, xtConstOrdInteger, xtLongint, xtWord];
  xtAllBooleanTypes = [xtBoolean, xtByteBool, xtLongBool];
  xtAllRealTypes = [xtReal, xtConstReal, xtSingle, xtDouble, xtExtended,
                    xtCurrency, xtComp];
  xtAllStringTypes = [xtConstString, xtShortString, xtString, xtAnsiString];
  xtAllPointerTypes = [xtPointer, xtNil];
  xtAllIntegerConvertibles = xtAllIntegerTypes;
  xtAllRealConvertibles = xtAllRealTypes+xtAllIntegerTypes;
  xtAllStringConvertibles = xtAllStringTypes+[xtChar,xtPChar];
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
  
const
  CleanExpressionType : TExpressionType =
    (Desc:xtNone; SubDesc:xtNone; Context:(Node:nil; Tool:nil));

type
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
    procedure Add(ExprType: TExpressionType);
    procedure AddFirst(ExprType: TExpressionType);
    property Capacity: integer read FCapacity write SetCapacity;
    destructor Destroy; override;
    function AsString: string;
  end;
  
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
    FoundContext: TFindContext): TIdentifierFoundResult of object;

  TFindDeclarationInput = record
    Flags: TFindDeclarationFlags;
    Identifier: PChar;
    ContextNode: TCodeTreeNode;
    OnIdentifierFound: TOnIdentifierFound;
    IdentifierTool: TFindDeclarationTool;
    FoundProc: PFoundProc;
  end;

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
    procedure Clear;
    procedure Save(var Input: TFindDeclarationInput);
    procedure Load(var Input: TFindDeclarationInput);
    procedure SetResult(AFindContext: TFindContext);
    procedure SetResult(ANewCodeTool: TFindDeclarationTool;
      ANewNode: TCodeTreeNode);
    procedure SetResult(ANewCodeTool: TFindDeclarationTool;
      ANewNode: TCodeTreeNode;  ANewCleanPos: integer);
    procedure SetResult(NodeCacheEntry: PCodeTreeNodeCacheEntry);
    procedure SetIdentifier(NewIdentifierTool: TFindDeclarationTool;
      NewIdentifier: PChar; NewOnIdentifierFound: TOnIdentifierFound);
    procedure SetFirstFoundProc(ProcContext: TFindContext);
    procedure ChangeFoundProc(ProcContext: TFindContext;
      ProcCompatibility: TTypeCompatibility;
      ParamCompatibilityList: TTypeCompatibilityList);
    procedure ConvertResultCleanPosToCaretPos;
    procedure ClearResult;
    procedure ClearInput;
    procedure ClearFoundProc;
  end;
  
  
  { TFindDeclarationTool }

  TFindDeclarationTool = class(TPascalParserTool)
  private
    FInterfaceIdentifierCache: TInterfaceIdentifierCache;
    FOnGetCodeToolForBuffer: TOnGetCodeToolForBuffer;
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
    function CheckSrcIdentifier(Params: TFindDeclarationParams;
      FoundContext: TFindContext): TIdentifierFoundResult;
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
      Result: TFindContext);
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
    function CalculateBinaryOperator(LeftOperand, RightOperand: TExpressionType;
      BinaryOperator: TAtomPosition;
      Params: TFindDeclarationParams): TExpressionType;
    function GetParameterNode(Node: TCodeTreeNode): TCodeTreeNode;
    function GetFirstParameterNode(Node: TCodeTreeNode): TCodeTreeNode;
    function GetExpressionTypeOfTypeIdentifier(
      Params: TFindDeclarationParams): TExpressionType;
    function FindTermTypeAsString(TermAtom: TAtomPosition;
      CursorNode: TCodeTreeNode; Params: TFindDeclarationParams): string;
  protected
    function FindDeclarationOfIdentAtCursor(
      Params: TFindDeclarationParams): boolean;
    function IdentifierIsDefined(IdentAtom: TAtomPosition;
      ContextNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
    function FindContextNodeAtCursor(
      Params: TFindDeclarationParams): TFindContext;
    function FindIdentifierInContext(Params: TFindDeclarationParams): boolean;
    function FindBaseTypeOfNode(Params: TFindDeclarationParams;
      Node: TCodeTreeNode): TFindContext;
    function FindClassOfMethod(ProcNode: TCodeTreeNode;
      Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
    function FindAncestorOfClass(ClassNode: TCodeTreeNode;
      Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
    function FindForwardIdentifier(Params: TFindDeclarationParams;
      var IsForward: boolean): boolean;
    function FindExpressionResultType(Params: TFindDeclarationParams;
      StartPos, EndPos: integer): TExpressionType;
    function FindCodeToolForUsedUnit(UnitNameAtom,
      UnitInFileAtom: TAtomPosition;
      ExceptionOnNotFound: boolean): TFindDeclarationTool;
    function FindIdentifierInInterface(AskingTool: TFindDeclarationTool;
      Params: TFindDeclarationParams): boolean;
    function CompareNodeIdentifier(Node: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function GetInterfaceNode: TCodeTreeNode;
    function CompatibilityList1IsBetter(List1, List2: TTypeCompatibilityList;
      ListCount: integer): boolean;
    function IsParamListCompatible(FirstParameterNode: TCodeTreeNode;
      ExprParamList: TExprTypeList; IgnoreMissingParameters: boolean;
      Params: TFindDeclarationParams;
      CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
    function IsParamListCompatible(FirstParameterNode1,
      FirstParameterNode2: TCodeTreeNode;
      Params: TFindDeclarationParams;
      CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
    function CreateParamExprList(StartPos: integer;
      Params: TFindDeclarationParams): TExprTypeList;
    function ContextIsDescendOf(DescendContext, AncestorContext: TFindContext;
      Params: TFindDeclarationParams): boolean;
    function IsCompatible(TargetNode: TCodeTreeNode;
      ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
    function IsCompatible(TargetType, ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
    function IsBaseCompatible(TargetType, ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
  public
    procedure BuildTree(OnlyInterfaceNeeded: boolean); override;
    destructor Destroy; override;
    function FindDeclaration(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindUnitSource(const AnUnitName,
      AnUnitInFilename: string): TCodeBuffer;
    property InterfaceIdentifierCache: TInterfaceIdentifierCache
      read FInterfaceIdentifierCache;
    property OnGetCodeToolForBuffer: TOnGetCodeToolForBuffer
      read FOnGetCodeToolForBuffer write FOnGetCodeToolForBuffer;
    property OnGetUnitSourceSearchPath: TOnGetSearchPath
      read FOnGetUnitSourceSearchPath write FOnGetUnitSourceSearchPath;
    function ConsistencyCheck: integer; override;
  end;

const
  fdfAllClassVisibilities = [fdfClassPublished,fdfClassPublic,fdfClassProtected,
                             fdfClassPrivate];
  fdfGlobals = [fdfExceptionOnNotFound, fdfIgnoreUsedUnits, fdfTopLvlResolving];
  fdfGlobalsSameIdent = fdfGlobals+[fdfExceptionOnPredefinedIdent,
                fdfIgnoreMissingParams,
                fdfOnlyCompatibleProc, fdfSearchInAncestors, fdfCollect]
                +fdfAllClassVisibilities;
  fdfDefaultForExpressions = [fdfSearchInParentNodes, fdfSearchInAncestors,
                              fdfExceptionOnNotFound]+fdfAllClassVisibilities;

function ExprTypeToString(ExprType: TExpressionType): string;
function CreateFindContext(NewTool: TFindDeclarationTool;
  NewNode: TCodeTreeNode): TFindContext;
function CreateFindContext(Params: TFindDeclarationParams): TFindContext;
function CreateFindContext(BaseTypeCache: TBaseTypeCache): TFindContext;
function FindContextAreEqual(Context1, Context2: TFindContext): boolean;
function PredefinedIdentToExprTypeDesc(Identifier: PChar): TExpressionTypeDesc;
function FindDeclarationFlagsAsString(Flags: TFindDeclarationFlags): string;


implementation


function FindDeclarationFlagsAsString(Flags: TFindDeclarationFlags): string;
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

function PredefinedIdentToExprTypeDesc(Identifier: PChar): TExpressionTypeDesc;
begin
  // predefined identifiers
  if CompareIdentifiers(Identifier,'NIL'#0)=0 then
    Result:=xtNil
  else if CompareIdentifiers(Identifier,'POINTER'#0)=0 then
    Result:=xtPointer
  else if (CompareIdentifiers(Identifier,'TRUE'#0)=0)
  or (CompareIdentifiers(Identifier,'FALSE'#0)=0) then
    Result:=xtConstBoolean
  else if CompareIdentifiers(Identifier,'STRING'#0)=0 then
    Result:=xtString
  else if CompareIdentifiers(Identifier,'SHORTSTRING'#0)=0 then
    Result:=xtShortString
  else if CompareIdentifiers(Identifier,'ANSISTRING'#0)=0 then
    Result:=xtAnsiString
  else if CompareIdentifiers(Identifier,'WIDESTRING'#0)=0 then
    Result:=xtWideString
  else if CompareIdentifiers(Identifier,'INT64'#0)=0 then
    Result:=xtInt64
  else if CompareIdentifiers(Identifier,'CARDINAL'#0)=0 then
    Result:=xtCardinal
  else if CompareIdentifiers(Identifier,'QWORD'#0)=0 then
    Result:=xtQWord
  else if CompareIdentifiers(Identifier,'BOOLEAN'#0)=0 then
    Result:=xtBoolean
  else if CompareIdentifiers(Identifier,'BYTEBOOL'#0)=0 then
    Result:=xtByteBool
  else if CompareIdentifiers(Identifier,'LONGBOOL'#0)=0 then
    Result:=xtLongBool
  else if CompareIdentifiers(Identifier,'CHAR'#0)=0 then
    Result:=xtChar
  else if CompareIdentifiers(Identifier,'REAL'#0)=0 then
    Result:=xtReal
  else if CompareIdentifiers(Identifier,'SINGLE'#0)=0 then
    Result:=xtSingle
  else if CompareIdentifiers(Identifier,'DOUBLE'#0)=0 then
    Result:=xtDouble
  else if CompareIdentifiers(Identifier,'EXTENDED'#0)=0 then
    Result:=xtExtended
  else if CompareIdentifiers(Identifier,'COMP'#0)=0 then
    Result:=xtComp
  else if CompareIdentifiers(Identifier,'FILE'#0)=0 then
    Result:=xtFile
  else if CompareIdentifiers(Identifier,'TEXT'#0)=0 then
    Result:=xtText
  // the delphi compiler special types
  else if CompareIdentifiers(Identifier,'CURRENCY'#0)=0 then
    Result:=xtCurrency
  else if CompareIdentifiers(Identifier,'LONGINT'#0)=0 then
    Result:=xtLongInt
  else if CompareIdentifiers(Identifier,'WORD'#0)=0 then
    Result:=xtWord
  else if CompareIdentifiers(Identifier,'LONGWORD'#0)=0 then
    Result:=xtCardinal
  else
    Result:=xtNone;
end;

function ExprTypeToString(ExprType: TExpressionType): string;
begin
  Result:='Desc='+ExpressionTypeDescNames[ExprType.Desc]
         +' SubDesc='+ExpressionTypeDescNames[ExprType.SubDesc];
  if ExprType.Context.Node<>nil then begin
    Result:=Result+' Node='+ExprType.Context.Node.DescAsString
      +' File="'+ExprType.Context.Tool.MainFilename+'"';
  end;
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

function FindContextAreEqual(Context1, Context2: TFindContext): boolean;
begin
  Result:=(Context1.Tool=Context2.Tool) and (Context1.Node=Context2.Node);
end;


{ TFindDeclarationTool }

function TFindDeclarationTool.FindDeclaration(CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var CleanCursorPos: integer;
  CursorNode, ClassNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  DirectSearch, SkipChecks, SearchForward: boolean;
  
  procedure CheckIfCursorOnAForwardDefinedClass;
  begin
    if SkipChecks then exit;
    if CursorNode.Desc=ctnTypeDefinition then begin
      if (CursorNode.FirstChild<>nil) and (CursorNode.FirstChild.Desc=ctnClass)
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
    ClassNode:=CursorNode.GetNodeOfType(ctnClass);
    if ClassNode<>nil then begin
      // cursor is in class/object definition
      if (ClassNode.SubDesc and ctnsForwardDeclaration)=0 then begin
        // parse class and build CodeTreeNodes for all properties/methods
        BuildSubTreeForClass(ClassNode);
        CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
        if (CursorNode.Desc=ctnClass)
        and (CleanCursorPos<ClassNode.FirstChild.StartPos) then begin
          // identifier is an ancestor/interface identifier
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
      CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
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
      CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
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
    if CursorNode.Desc=ctnProperty then begin
      MoveCursorToNodeStart(CursorNode);
      ReadNextAtom; // read 'property'
      ReadNextAtom; // read property name
      if CleanCursorPos<CurPos.EndPos then begin
        DirectSearch:=true;
        SkipChecks:=true;
      end;
    end;
  end;
  
begin
  Result:=false;
  SkipChecks:=false;
  ActivateGlobalWriteLock;
  try
    // build code tree
    
    {$IFDEF CTDEBUG}
    writeln(DebugPrefix,'TFindDeclarationTool.FindDeclaration A CursorPos=',CursorPos.X,',',CursorPos.Y);
    {$ENDIF}
    BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,
                  [{$IFDEF IgnoreErrorAfterCursor}btSetIgnoreErrorPos{$ENDIF}]);
    {$IFDEF CTDEBUG}
    writeln(DebugPrefix,'TFindDeclarationTool.FindDeclaration C CleanCursorPos=',CleanCursorPos);
    {$ENDIF}
    // find CodeTreeNode at cursor
    CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
    if IsIncludeDirectiveAtPos(CleanCursorPos,CursorNode.StartPos,NewPos.Code)
    then begin
      NewPos.X:=1;
      NewPos.Y:=1;
      NewTopLine:=1;
      Result:=true;
      exit;
    end;
    {$IFDEF CTDEBUG}
    writeln('TFindDeclarationTool.FindDeclaration D CursorNode=',NodeDescriptionAsString(CursorNode.Desc));
    {$ENDIF}
    if CursorNode.Desc=ctnUsesSection then begin
      // find used unit
      Result:=FindDeclarationInUsesSection(CursorNode,CleanCursorPos,
                                           NewPos,NewTopLine);
    end else begin
      DirectSearch:=false;
      SearchForward:=false;
      CheckIfCursorOnAForwardDefinedClass;
      CheckIfCursorInClassNode;
      CheckIfCursorInBeginNode;
      CheckIfCursorInProcNode;
      CheckIfCursorInPropertyNode;
      // set cursor
      MoveCursorToCleanPos(CleanCursorPos);
      while (CurPos.StartPos>1) and (IsIdentChar[Src[CurPos.StartPos-1]]) do
        dec(CurPos.StartPos);
      if (CurPos.StartPos>=1) and (IsIdentStartChar[Src[CurPos.StartPos]]) then
      begin
        // search identifier
        CurPos.EndPos:=CurPos.StartPos;
        while (CurPos.EndPos<=SrcLen) and IsIdentChar[Src[CurPos.EndPos]] do
          inc(CurPos.EndPos);
        // find declaration of identifier
        Params:=TFindDeclarationParams.Create;
        try
          Params.ContextNode:=CursorNode;
          Params.SetIdentifier(Self,@Src[CurPos.StartPos],@CheckSrcIdentifier);
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound,
                         fdfExceptionOnPredefinedIdent,
                         fdfTopLvlResolving,fdfSearchInAncestors]
                        +fdfAllClassVisibilities;
          if not DirectSearch then begin
            Result:=FindDeclarationOfIdentAtCursor(Params);
          end else begin
            Include(Params.Flags,fdfIgnoreCurContextNode);
            if SearchForward then
              Include(Params.Flags,fdfSearchForward);
            Result:=FindIdentifierInContext(Params);
          end;
          if Result then begin
            Params.ConvertResultCleanPosToCaretPos;
            NewPos:=Params.NewPos;
            NewTopLine:=Params.NewTopLine;
            if NewPos.Code=nil then begin
              if Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then
                Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
              Params.IdentifierTool.RaiseExceptionFmt(ctsIdentifierNotFound,
                                            [GetIdentifier(Params.Identifier)]);
            end;
          end;
        finally
          Params.Free;
        end;
      end else begin
        // find declaration of not identifier, e.g. numeric label

      end;
    end;
  finally
    ClearIgnoreErrorAfter;
    DeactivateGlobalWriteLock;
  end;
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
  writeln('TFindDeclarationTool.FindDeclarationInUsesSection A');
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
      NewPos.Code:=FindUnitSource(UnitName,UnitInFilename);
      if NewPos.Code=nil then
        RaiseExceptionFmt(ctsUnitNotFound,[UnitName]);
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
  writeln('TFindDeclarationTool.FindDeclarationInUsesSection END cursor not on unitname');
  {$ENDIF}
end;

function TFindDeclarationTool.FindUnitSource(const AnUnitName,
  AnUnitInFilename: string): TCodeBuffer;

  function LoadFile(const AFilename: string;
    var NewCode: TCodeBuffer): boolean;
  begin
    {$IFDEF ShowTriedFiles}
    writeln('TFindDeclarationTool.FindUnitSource.LoadFile ',AFilename);
    {$ENDIF}
    NewCode:=TCodeBuffer(Scanner.OnLoadSource(
                                          Self,ExpandFilename(AFilename),true));
    Result:=NewCode<>nil;
  end;
  
  function SearchUnitFileInDir(const ADir, AnUnitName: string): TCodeBuffer;
  var APath: string;
  begin
    APath:=ADir;
    if (APath<>'') and (APath[length(APath)]<>PathDelim) then
      APath:=APath+PathDelim;
    {$IFNDEF win32}
    if LoadFile(ADir+lowercase(AnUnitName)+'.pp',Result) then exit;
    if LoadFile(ADir+lowercase(AnUnitName)+'.pas',Result) then exit;
    {$ENDIF}
    if LoadFile(ADir+AnUnitName+'.pp',Result) then exit;
    if LoadFile(ADir+AnUnitName+'.pas',Result) then exit;
    Result:=nil;
  end;

  function SearchUnitFileInPath(const APath, TheUnitName: string): TCodeBuffer;
  var PathStart, PathEnd: integer;
    ADir: string;
  begin
    PathStart:=1;
    while PathStart<=length(APath) do begin
      PathEnd:=PathStart;
      while (PathEnd<=length(APath)) and (APath[PathEnd]<>';') do inc(PathEnd);
      if PathEnd>PathStart then begin
        ADir:=copy(APath,PathStart,PathEnd-PathStart);
        if (ADir<>'') and (ADir[length(ADir)]<>PathDelim) then
          ADir:=ADir+PathDelim;
        if not FilenameIsAbsolute(ADir) then
          ADir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename)+ADir;
        Result:=SearchUnitFileInDir(ADir,TheUnitName);
        if Result<>nil then exit;
      end;
      PathStart:=PathEnd+1;
    end;
    Result:=nil;
  end;

  function SearchFileInPath(const APath, RelativeFilename: string): TCodeBuffer;
  var PathStart, PathEnd: integer;
    ADir: string;
  begin
    PathStart:=1;
    while PathStart<=length(APath) do begin
      PathEnd:=PathStart;
      while (PathEnd<=length(APath)) and (APath[PathEnd]<>';') do inc(PathEnd);
      if PathEnd>PathStart then begin
        ADir:=copy(APath,PathStart,PathEnd-PathStart);
        if (ADir<>'') and (ADir[length(ADir)]<>PathDelim) then
          ADir:=ADir+PathDelim;
        if not FilenameIsAbsolute(ADir) then
          ADir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename)+ADir;
        if LoadFile(ADir+RelativeFilename,Result) then exit;
      end;
      PathStart:=PathEnd+1;
    end;
    Result:=nil;
  end;
  
  function SearchUnitInUnitLinks(const TheUnitName: string): TCodeBuffer;
  var UnitLinks, CurFilename: string;
    UnitLinkStart, UnitLinkEnd: integer;
  begin
    Result:=nil;
    UnitLinks:=Scanner.Values[ExternalMacroStart+'UnitLinks'];
    {$IFDEF ShowTriedFiles}
    //writeln('TFindDeclarationTool.FindUnitSource.SearchUnitInUnitLinks');
    {$ENDIF}
    UnitLinkStart:=1;
    while UnitLinkStart<=length(UnitLinks) do begin
      while (UnitLinkStart<=length(UnitLinks))
      and (UnitLinks[UnitLinkStart] in [#10,#13]) do
        inc(UnitLinkStart);
      UnitLinkEnd:=UnitLinkStart;
      while (UnitLinkEnd<=length(UnitLinks)) and (UnitLinks[UnitLinkEnd]<>' ')
      do
        inc(UnitLinkEnd);
      if UnitLinkEnd>UnitLinkStart then begin
        {$IFDEF ShowTriedFiles}
        //writeln('  unit "',copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart),'"');
        {$ENDIF}
        if CompareSubStrings(TheUnitName,UnitLinks,1,
          UnitLinkStart,UnitLinkEnd-UnitLinkStart,false)=0
        then begin
          // unit found -> parse filename
          UnitLinkStart:=UnitLinkEnd+1;
          UnitLinkEnd:=UnitLinkStart;
          while (UnitLinkEnd<=length(UnitLinks))
          and (not (UnitLinks[UnitLinkEnd] in [#10,#13])) do
            inc(UnitLinkEnd);
          if UnitLinkEnd>UnitLinkStart then begin
            CurFilename:=copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart);
            LoadFile(CurFilename,Result);
            if Result=nil then begin
              // try also different extensions
              if CompareFileExt(CurFilename,'.pp',false)=0 then
                LoadFile(ChangeFileExt(CurFilename,'.pas'),Result)
              else
                LoadFile(ChangeFileExt(CurFilename,'.pp'),Result);
            end;
            exit;
          end;
        end else begin
          UnitLinkStart:=UnitLinkEnd+1;
          while (UnitLinkStart<=length(UnitLinks))
          and (not (UnitLinks[UnitLinkStart] in [#10,#13])) do
            inc(UnitLinkStart);
        end;
      end else
        break;
    end;
  end;


var CurDir, UnitSrcSearchPath: string;
  MainCodeIsVirtual: boolean;
begin
  {$IFDEF ShowTriedFiles}
  writeln('TFindDeclarationTool.FindUnitSource A AnUnitName=',AnUnitName,' AnUnitInFilename=',AnUnitInFilename);
  {$ENDIF}
  Result:=nil;
  if (AnUnitName='') or (Scanner=nil) or (Scanner.MainCode=nil)
  or (not (TObject(Scanner.MainCode) is TCodeBuffer))
  or (Scanner.OnLoadSource=nil) then
    exit;
  if Assigned(OnGetUnitSourceSearchPath) then
    UnitSrcSearchPath:=OnGetUnitSourceSearchPath(Self)
  else
    UnitSrcSearchPath:=Scanner.Values[ExternalMacroStart+'SrcPath'];
  {$IFDEF ShowSearchPaths}
  writeln('TFindDeclarationTool.FindUnitSource ',
  ' Self="',MainFilename,'"',
  ' UnitSrcSearchPath=',UnitSrcSearchPath);
  {$ENDIF}
  //writeln('>>>>>',Scanner.Values.AsString,'<<<<<');
  MainCodeIsVirtual:=TCodeBuffer(Scanner.MainCode).IsVirtual;
  if not MainCodeIsVirtual then begin
    CurDir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename);
  end else begin
    CurDir:='';
  end;
  if AnUnitInFilename<>'' then begin
    // unitname in 'filename'
    if FilenameIsAbsolute(AnUnitInFilename) then begin
      Result:=TCodeBuffer(Scanner.OnLoadSource(Self,AnUnitInFilename,true));
    end else begin
      // search file in current directory
      CurDir:=AppendPathDelim(CurDir);
      if not LoadFile(CurDir+AnUnitInFilename,Result) then begin
        // search AnUnitInFilename in searchpath
        Result:=SearchFileInPath(UnitSrcSearchPath,AnUnitInFilename);
      end;
    end;
  end else begin
    // normal unit name -> search as the compiler would search
    // first search in current directory (= where the maincode is)
    {$IFDEF ShowTriedFiles}
    writeln('TFindDeclarationTool.FindUnitSource Search in current dir=',CurDir);
    {$ENDIF}
    Result:=SearchUnitFileInDir(CurDir,AnUnitName);
    if Result=nil then begin
      // search in search path
      {$IFDEF ShowTriedFiles}
      writeln('TFindDeclarationTool.FindUnitSource Search in search path=',UnitSrcSearchPath);
      {$ENDIF}
      Result:=SearchUnitFileInPath(UnitSrcSearchPath,AnUnitName);
      if Result=nil then begin
        // search in FPC source directory
        Result:=SearchUnitInUnitLinks(AnUnitName);
      end;
    end;
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

function TFindDeclarationTool.FindDeclarationOfIdentAtCursor(
  Params: TFindDeclarationParams): boolean;
{ searches an identifier in clean code, parses code in front and after the
  identifier

  Params:
    Identifier in clean source
    ContextNode  // = DeepestNode at Cursor
    
  Result:
    true, if NewPos+NewTopLine valid

  For example:
    A^.B().C[].Identifier
}
var
  StartPos, EndPos: integer;
  ExprType: TExpressionType;
begin
  {$IFDEF CTDEBUG}
  writeln('[TFindDeclarationTool.FindDeclarationOfIdentAtCursor] Identifier=',
    '"',GetIdentifier(Params.Identifier),'"',
    ' ContextNode=',NodeDescriptionAsString(Params.ContextNode.Desc),
    ' "',copy(Src,Params.ContextNode.StartPos,20),'"');
  {$ENDIF}
  Result:=false;
  MoveCursorToCleanPos(Params.Identifier);
  StartPos:=CurPos.StartPos;
  if Params.ContextNode.Desc<>ctnIdentifier then StartPos:=-1;
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
  write('[TFindDeclarationTool.FindDeclarationOfIdentAtCursor] Ident=',
    '"',GetIdentifier(Params.Identifier),'" ');
  if Params.NewNode<>nil then
    writeln('Node=',Params.NewNode.DescAsString,' ',Params.NewCodeTool.MainFilename)
  else
    writeln('NOT FOUND');
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
                 fdfTopLvlResolving,fdfFindVariable,fdfIgnoreCurContextNode]
                +fdfAllClassVisibilities;
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
      write(':::: TFindDeclarationTool.FindIdentifierInContext.FindInNodeCache');
      writeln(' Ident=',GetIdentifier(Params.Identifier),
               ' Wanted=[',NodeCacheEntryFlagsAsString(NodeCacheEntryFlags),']',
               ' Cache=[',NodeCacheEntryFlagsAsString(LastCacheEntry^.Flags),']'
             );
      writeln('    ContextNode=',ContextNode.DescAsString,
              ' StartPos=',ContextNode.StartPos,
              ' EndPos=',ContextNode.EndPos,
              ' Self=',MainFilename);
      writeln('  LastCacheEntry(Pos=',LastCacheEntry^.CleanStartPos,
              '-',LastCacheEntry^.CleanEndPos,')');
      if (Params.NewNode<>nil) then
        writeln('   NewTool=',Params.NewCodeTool.MainFilename,
                ' NewNode=',Params.NewNode.DescAsString)
      else
        writeln('   cache says: identifier NOT FOUND');
      if CompareSrcIdentifiers(Params.Identifier,'TDefineAction') then begin
        NodeCache.WriteDebugReport('NANUNANA: ');
      end;
      {$ENDIF}
      ContextNode:=Params.NewNode;
      Result:=true;
    end;
  end;
  
  function CheckResult(NewResult, CallOnIdentifierFound: boolean): boolean;
  // returns: true if ok to exit
  //          false if search should continue
  
    procedure RaiseNotFound;
    begin
      Params.IdentifierTool.RaiseExceptionFmt(ctsIdentifierNotFound,
                                            [GetIdentifier(Params.Identifier)]);
    end;
  
  var IdentFoundResult: TIdentifierFoundResult;
  begin
    Result:=true;
    FindIdentifierInContext:=NewResult;
    {$IFDEF ShowCollect}
    if fdfCollect in Params.Flags then begin
      writeln('[TFindDeclarationTool.FindIdentifierInContext] COLLECT CheckResult Ident=',
      '"',GetIdentifier(Params.Identifier),'"',
      ' File="',ExtractFilename(MainFilename)+'"',
      ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']',
      ' NewResult=',NewResult,
      ' CallOnIdentifierFound=',CallOnIdentifierFound
      );
    end;
    {$ENDIF}
    if NewResult then begin
      // identifier found
      if CallOnIdentifierFound then begin
        {
        write('[TFindDeclarationTool.FindIdentifierInContext] CallOnIdentifierFound Ident=',
        '"',GetIdentifier(Params.Identifier),'"',
        ' StartContext="',StartContextNode.DescAsString,'" "',copy(Src,StartContextNode.StartPos,20),'"',
        ' File="',ExtractFilename(MainFilename)+'"',
        ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
        );
        }
        IdentFoundResult:=Params.NewCodeTool.DoOnIdentifierFound(Params,
                            Params.NewNode);
        Result:=IdentFoundResult<>ifrProceedSearch;
        if IdentFoundResult<>ifrAbortSearch then exit;
      end else
        exit;
    end;
    // identifier was not found
    if Params.FoundProc<>nil then begin
      // there was a proc, only the search for the overloaded proc was
      // unsuccessful
      // -> return the found proc
      Params.SetResult(Params.FoundProc^.Context.Tool,
                       Params.FoundProc^.Context.Node.FirstChild);
      FindIdentifierInContext:=true;
      exit;
    end;
    if not (fdfExceptionOnNotFound in Params.Flags) then exit;
    if (Params.Identifier<>nil)
    and WordIsPredefinedIdentifier.DoIt(Params.Identifier)
    and not (fdfExceptionOnPredefinedIdent in Params.Flags) then begin
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
        ContextNode:=ContextNode.LastChild
      end else
        ContextNode:=ContextNode.FirstChild;
    end;
  end;
  
  function SearchInTypeVarConstDefinition: boolean;
  // returns: true if ok to exit
  //          false if search should continue
  begin
    Result:=false;
    if (fdfCollect in Params.Flags)
    or CompareSrcIdentifiers(ContextNode.StartPos,Params.Identifier)
    then begin
      {$IFDEF ShowTriedIdentifiers}
      writeln('  Definition Identifier found="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      // identifier found
      Params.SetResult(Self,ContextNode);
      Result:=CheckResult(true,true);
    end;
    // search for enums
    Params.ContextNode:=ContextNode;
    if FindEnumInContext(Params) then begin
      Result:=CheckResult(true,false);
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
      writeln('  Source Name Identifier found="',GetIdentifier(Params.Identifier),'"');
      {$ENDIF}
      Params.SetResult(Self,ContextNode,CurPos.StartPos);
      Result:=CheckResult(true,true);
    end;
    if FindIdentifierInHiddenUsedUnits(Params) then begin
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
      ReadNextAtom; // read keyword 'property'
      ReadNextAtom; // read name
      if (fdfCollect in Params.Flags)
      or CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then begin
        // identifier found
        {$IFDEF ShowTriedIdentifiers}
        writeln('  Property Identifier found="',GetIdentifier(Params.Identifier),'"');
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
        and (ContextNode.Desc=ctnClass)) then begin
          // even searching in ancestors contexts is not permitted
          // -> there is no prior context accessible any more
          // -> identifier not found
          {$IFDEF ShowTriedContexts}
          writeln('[TFindDeclarationTool.FindIdentifierInContext] no prior node accessible   ContextNode=',ContextNode.DescAsString);
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
      //writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching prior node of ',ContextNode.DescAsString);
      {$ENDIF}
      LastSearchedNode:=ContextNode;

      if (ContextNode.Desc=ctnClass)
      and (fdfSearchInAncestors in Params.Flags) then begin

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
        writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching in Brother  ContextNode=',ContextNode.DescAsString);
        {$ENDIF}
        // it is not always allowed to search in every node on the same lvl:

        // -> test if class visibility valid
        case ContextNode.Desc of
        ctnClassPublished:if (fdfClassPublished in Params.Flags) then break;
        ctnClassPublic:   if (fdfClassPublic    in Params.Flags) then break;
        ctnClassProtected:if (fdfClassProtected in Params.Flags) then break;
        ctnClassPrivate:  if (fdfClassPrivate   in Params.Flags) then break;
        ctnWithVariable:
          begin
            // check if StartContextNode is covered by the ContextNode
            // a WithVariable ranges from the start of its expression
            // to the end of the with statement
            if StartContextNode.StartPos<ContextNode.EndPos then break;
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
        ContextNode:=ContextNode.Parent;
        {$IFDEF ShowTriedParentContexts}
        writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching in Parent  ContextNode=',ContextNode.DescAsString);
        {$ENDIF}
        case ContextNode.Desc of

        ctnTypeSection, ctnVarSection, ctnConstSection, ctnResStrSection,
        ctnLabelSection,
        ctnInterface, ctnImplementation,
        ctnClassPublished,ctnClassPublic,ctnClassProtected, ctnClassPrivate,
        ctnRecordCase, ctnRecordVariant,
        ctnProcedureHead, ctnParameterList:
          // these codetreenodes build a parent-child-relationship, but
          // for pascal it is only a range, hence after searching in the
          // childs of the last node, it must be searched next in the childs
          // of the prior node
          ;

        ctnClass, ctnRecordType:
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
  writeln('[TFindDeclarationTool.FindIdentifierInContext] Start Ident=',
  '"',GetIdentifier(Params.Identifier),'"',
  ' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
  ' File="',ExtractFilename(MainFilename)+'"',
  ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
  );
  {$ELSE}
    {$IFDEF ShowCollect}
    if fdfCollect in Params.Flags then begin
      writeln('[TFindDeclarationTool.FindIdentifierInContext] COLLECT Start Ident=',
      '"',GetIdentifier(Params.Identifier),'"',
      ' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
      ' File="',ExtractFilename(MainFilename)+'"',
      ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
      );
    end;
    {$ENDIF}
  {$ENDIF}

  try
    // search in the Tree of this tool
    repeat
      {$IFDEF ShowTriedIdentifiers}
      writeln('[TFindDeclarationTool.FindIdentifierInContext] Loop Ident=',
      '"',GetIdentifier(Params.Identifier),'"',
      ' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
      ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
      );
      {$ELSE}
        {$IFDEF ShowCollect}
        if fdfCollect in Params.Flags then begin
          writeln('[TFindDeclarationTool.FindIdentifierInContext] COLLECT Loop Ident=',
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
        ctnLabelSection,
        ctnInterface, ctnImplementation,
        ctnClassPublic, ctnClassPrivate, ctnClassProtected, ctnClassPublished,
        ctnClass,
        ctnRecordType, ctnRecordCase, ctnRecordVariant,
        ctnParameterList:
          // these nodes build a parent-child relationship. But in pascal
          // they just define a range and not a context.
          // -> search in all childs
          MoveContextNodeToChilds;

        ctnTypeDefinition, ctnVarDefinition, ctnConstDefinition:
          if SearchInTypeVarConstDefinition then exit;

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

        ctnPointerType:
          begin
            // pointer types can be forward definitions
            // -> search in both directions
            Params.ContextNode:=ContextNode.Parent;
            if CheckResult(FindForwardIdentifier(Params,IsForward),false) then
              exit;
          end;

        end;
      end else begin
        Exclude(Params.Flags,fdfIgnoreCurContextNode);
        {$IFDEF ShowTriedContexts}
        writeln('[TFindDeclarationTool.FindIdentifierInContext] IgnoreCurContext');
        {$ENDIF}
      end;
      if LastContextNode=ContextNode then begin
        // same context -> search in prior context
        if not LeavingContextIsPermitted then break;
        if not SearchNextNode then exit;
      end;
    until ContextNode=nil;
    
  finally
    if Result and (not (fdfDoNotCache in Params.NewFlags))
    and (FirstSearchedNode<>nil) then begin
      // cache result
      AddResultToNodeCaches(FirstSearchedNode,ContextNode,
        fdfSearchForward in Params.Flags,Params,SearchRangeFlags);
    end;
  end;
  // if we are here, the identifier was not found
  if FirstSearchedNode<>nil then begin
    // add result to cache
    Params.NewNode:=nil;
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
begin
  Result:=false;
  if Params.ContextNode=nil then exit;
  OldContextNode:=Params.ContextNode;
  CurContextNode:=OldContextNode;
  if CurContextNode.Desc=ctnClass then
    BuildSubTreeForClass(CurContextNode);
  CurContextNode:=CurContextNode.FirstChild;
  while CurContextNode<>nil do begin
    if (CurContextNode.Desc in [ctnEnumIdentifier])
    and ((fdfCollect in Params.Flags)
      or CompareSrcIdentifiers(CurContextNode.StartPos,Params.Identifier))
    then begin
      // identifier found
      
      // ToDo: fdfCollect
      
      Result:=true;
      Params.SetResult(Self,CurContextNode);
      exit;
    end;
    OldContextNode:=Params.ContextNode;
    try
      Params.ContextNode:=CurContextNode;
      Result:=FindEnumInContext(Params);
    finally
      Params.ContextNode:=OldContextNode;
    end;
    if Result then exit;
    CurContextNode:=CurContextNode.NextBrother;
  end;
end;

function TFindDeclarationTool.FindContextNodeAtCursor(
  Params: TFindDeclarationParams): TFindContext;
{ searches for the context node for a specific cursor pos
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
  
var OldInput: TFindDeclarationInput;
  ClassIdentNode, DummyNode: TCodeTreeNode;
  NodeStack: TCodeTreeNodeStack;
  OldPos: integer;
  
  procedure RaiseForwardNotResolved;
  begin
    RaiseExceptionFmt(ctsForwardClassDefinitionNotResolved,
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

      {$IFDEF ShowTriedContexts}
      writeln('[TFindDeclarationTool.FindBaseTypeOfNode] LOOP Result=',Result.Node.DescAsString,' ',HexStr(Cardinal(Result.Node),8));
      writeln('  Flags=[',FindDeclarationFlagsAsString(Params.Flags),']');
      {$ENDIF}
      if (Result.Node.Desc in AllIdentifierDefinitions) then begin
        // instead of variable/const/type definition, return the type
        DummyNode:=FindTypeNodeOfDefinition(Result.Node);
        if DummyNode=nil then
          // some constants and variants do not have a type
          break;
        Result.Node:=DummyNode;
      end else
      if (Result.Node.Desc=ctnClass)
      and ((Result.Node.SubDesc and ctnsForwardDeclaration)>0) then
      begin
        // search the real class
        {$IFDEF ShowTriedContexts}
        writeln('[TFindDeclarationTool.FindBaseTypeOfNode] Class is forward');
        {$ENDIF}

        // ToDo: check for circles in ancestor chain
        
        ClassIdentNode:=Result.Node.Parent;
        if (ClassIdentNode=nil) or (not (ClassIdentNode.Desc=ctnTypeDefinition))
        then begin
          MoveCursorToCleanPos(Result.Node.StartPos);
          RaiseForwardClassNameLess;
        end;
        Params.Save(OldInput);
        try
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
          exit;
        finally
          Params.Load(OldInput);
        end;
      end else
      if (Result.Node.Desc=ctnIdentifier) then begin
        // this type is just an alias for another type
        // -> search the basic type
        if Result.Node.Parent=nil then
          break;
        Params.Save(OldInput);
        try
          DummyNode:=Result.Node;
          Params.SetIdentifier(Self,@Src[Result.Node.StartPos],
                               @CheckSrcIdentifier);
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                        +(fdfGlobals*Params.Flags)
                        -[fdfIgnoreUsedUnits];
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
              MoveCursorToNodeStart(DummyNode);
              ReadNextAtom;
              RaiseExceptionFmt(ctsStrExpectedButAtomFound,
                                [ctsTypeIdentifier,GetAtom]);
            end;
          end else
            // predefined identifier
            Result:=CreateFindContext(Self,Result.Node);
          exit;
        finally
          Params.Load(OldInput);
        end;
      end else
      if (Result.Node.Desc=ctnProperty) then begin
        // this is a property -> search the type definition of the property
        MoveCursorToNodeStart(Result.Node);
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
          try
            Params.SetIdentifier(Self,@Src[CurPos.StartPos],nil);
            Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                          +(fdfGlobals*Params.Flags)
                          -[fdfIgnoreUsedUnits];
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
            exit;
          finally
            Params.Load(OldInput);
          end;
        end else begin
          // property has no type
          // -> search ancestor property
          Params.Save(OldInput);
          try
            MoveCursorToNodeStart(Result.Node);
            ReadNextAtom; // read 'property'
            ReadNextAtom; // read name
            OldPos:=CurPos.StartPos;
            Params.SetIdentifier(Self,@Src[CurPos.StartPos],nil);
            Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInAncestors]
                         +(fdfGlobalsSameIdent*Params.Flags);
            FindIdentifierInAncestors(Result.Node.Parent.Parent,Params);
            if Result.Node.Desc=ctnProperty then begin
              Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                            Params.NewNode);
              exit;
            end else begin
              // ancestor is not a property
              MoveCursorToCleanPos(OldPos);
              RaiseException(ctsAncestorIsNotProperty);
            end;
          finally
            Params.Load(OldInput);
          end;
        end;
      end else
      if (Result.Node.Desc in [ctnProcedure,ctnProcedureHead])
      and (fdfFunctionResult in Params.Flags) then begin
        // a proc -> if this is a function return the result type
        BuildSubTreeForProcHead(Result.Node);
        // a proc node contains as FirstChild a proc-head node
        DummyNode:=Result.Node;
        if DummyNode.Desc=ctnProcedure then
          DummyNode:=DummyNode.FirstChild;
        // and a proc-head node has as childs the parameterlist and the result
        DummyNode:=DummyNode.FirstChild;
        if (DummyNode<>nil) and (DummyNode.Desc=ctnParameterList) then
          DummyNode:=DummyNode.NextBrother;
        if DummyNode<>nil then Result.Node:=DummyNode;
        Exclude(Params.Flags,fdfFunctionResult);
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
  write('[TFindDeclarationTool.FindBaseTypeOfNode] END Node=');
  if Node<>nil then write(Node.DescAsString) else write('NIL');
  write(' Result=');
  if Result.Node<>nil then write(Result.Node.DescAsString) else write('NIL');
  writeln('');
  {$ENDIF}
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
  NameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method body (not a declaration).
    // -> proceed the search normally ...
  end else begin
    // proc is a proc declaration
    if (fdfCollect in Params.Flags)
    or CompareSrcIdentifiers(NameAtom.StartPos,Params.Identifier) then begin
      // proc identifier found
      {$IFDEF ShowTriedContexts}
      writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc-Identifier found="',GetIdentifier(Params.Identifier),'"');
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
      // 1. search the class
      Params.Save(OldInput);
      try
        Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes]
                      +(fdfGlobals*Params.Flags)
                      +[fdfExceptionOnNotFound,fdfIgnoreUsedUnits]
                      -[fdfTopLvlResolving];
        Params.ContextNode:=ProcContextNode;
        Params.SetIdentifier(Self,@Src[ClassNameAtom.StartPos],nil);
        {$IFDEF ShowTriedContexts}
        writeln('[TFindDeclarationTool.FindIdentifierInClassOfMethod]  Proc="',copy(src,ProcContextNode.StartPos,30),'" searching class of method   class="',ExtractIdentifier(ClassNameAtom.StartPos),'"');
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
        Params.Flags:=[fdfSearchInAncestors]+fdfAllClassVisibilities
                      +(fdfGlobalsSameIdent*Params.Flags)
                      -[fdfExceptionOnNotFound];
        Params.ContextNode:=ClassContext.Node;
        {$IFDEF ShowTriedContexts}
        writeln('[TFindDeclarationTool.FindIdentifierInClassOfMethod]  searching identifier in class of method');
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
      writeln('[TFindDeclarationTool.FindIdentifierInClassOfMethod]  Proc Identifier found="',GetIdentifier(Params.Identifier),'"');
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
  writeln('[TFindDeclarationTool.FindClassOfMethod] A ');
  {$ENDIF}
  Result:=false;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom; // read keyword
  ReadNextAtom; // read classname
  ClassNameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method
    // -> search the class
    Params.Save(OldInput);
    try
      Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes,
                     fdfExceptionOnNotFound,fdfIgnoreUsedUnits]
                    +(fdfGlobals*Params.Flags)
                    -[fdfTopLvlResolving];
      Params.ContextNode:=ProcNode;
      Params.SetIdentifier(Self,@Src[ClassNameAtom.StartPos],nil);
      {$IFDEF ShowTriedContexts}
      writeln('[TFindDeclarationTool.FindClassOfMethod]  searching class of method   class="',ExtractIdentifier(ClassNameAtom.StartPos),'"');
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
    finally
      Params.Load(OldInput);
    end;
  end else begin
    // proc is not a method
  end;
end;

function TFindDeclarationTool.FindAncestorOfClass(ClassNode: TCodeTreeNode;
  Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
var AncestorAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  AncestorNode, ClassIdentNode: TCodeTreeNode;
  SearchTObject: boolean;
  AncestorContext: TFindContext;
begin
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) then
    RaiseException('[TFindDeclarationTool.FindAncestorOfClass] '
      +' invalid classnode');
  Result:=false;
  
  // ToDo: ppu, ppw, dcu
  
  // search the ancestor name
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // read keyword 'class', 'object', 'interface', 'dispinterface'
  if UpAtomIs('PACKED') then ReadNextAtom;
  ReadNextAtom;
  if not AtomIsChar('(') then begin
    // no ancestor class specified
    // check class name
    ClassIdentNode:=ClassNode.Parent;
    if (ClassIdentNode=nil) or (ClassIdentNode.Desc<>ctnTypeDefinition) then
    begin
      MoveCursorToNodeStart(ClassNode);
      RaiseException('class without name');
    end;
    // if this class is not TObject, TObject is class ancestor
    SearchTObject:=not CompareSrcIdentifier(ClassIdentNode.StartPos,'TObject');
    if not SearchTObject then exit;
  end else begin
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    // ancestor name found
    AncestorAtom:=CurPos;
    SearchTObject:=false;
  end;
  {$IFDEF ShowTriedContexts}
  writeln('[TFindDeclarationTool.FindAncestorOfClass] ',
  ' search ancestor class = ',GetAtom);
  {$ENDIF}
  // search ancestor class context
  CurPos.StartPos:=CurPos.EndPos;
  Params.Save(OldInput);
  try
    Params.Flags:=[fdfSearchInParentNodes,fdfIgnoreCurContextNode,
                   fdfExceptionOnNotFound]
                  +(fdfGlobals*Params.Flags)
                  -[fdfIgnoreUsedUnits,fdfTopLvlResolving];
    if not SearchTObject then
      Params.SetIdentifier(Self,@Src[AncestorAtom.StartPos],nil)
    else begin
      Params.SetIdentifier(Self,'TObject',nil);
      Exclude(Params.Flags,fdfExceptionOnNotFound);
    end;
    Params.ContextNode:=ClassNode;
    if not FindIdentifierInContext(Params) then begin
      MoveCursorToNodeStart(ClassNode);
      RaiseException(ctsDefaultClassAncestorTObjectNotFound);
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
{ this function is internally used by FindIdentifierInContext
}
var
  WithVarExpr: TExpressionType;
  OldInput: TFindDeclarationInput;
begin
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.FindIdentifierInWithVarContext] ',
  '"',GetIdentifier(Params.Identifier),'"'
  );
  {$ENDIF}
  Result:=false;
  // find the base type of the with variable
  // move cursor to start of with-variable
  MoveCursorToCleanPos(WithVarNode.StartPos);
  Params.Save(OldInput);
  try
    Params.ContextNode:=WithVarNode;
    Params.Flags:=Params.Flags+[fdfExceptionOnNotFound,fdfFunctionResult];
    WithVarExpr:=FindExpressionTypeOfVariable(WithVarNode.StartPos,-1,Params);
    if (WithVarExpr.Desc<>xtContext)
    or (WithVarExpr.Context.Node=nil)
    or (WithVarExpr.Context.Node=OldInput.ContextNode)
    or (not (WithVarExpr.Context.Node.Desc in [ctnClass,ctnRecordType])) then
    begin
      MoveCursorToCleanPos(WithVarNode.StartPos);
      RaiseException(ctsExprTypeMustBeClassOrRecord);
    end;
    // search identifier in with context
    Params.Load(OldInput);
    Exclude(Params.Flags,fdfExceptionOnNotFound);
    Params.ContextNode:=WithVarExpr.Context.Node;
    if WithVarExpr.Context.Tool.FindIdentifierInContext(Params) then begin
      // identifier found in with context
      Result:=true;
    end;
  finally
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindIdentifierInAncestors(
  ClassNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
  and FindBaseTypeOfNode
}
var AncestorAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  AncestorNode, ClassIdentNode: TCodeTreeNode;
  SearchTObject: boolean;
  AncestorContext: TFindContext;
begin
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) then
    RaiseException('[TFindDeclarationTool.FindIdentifierInAncestors] '
      +' invalid classnode');
  Result:=false;
  if not (fdfSearchInAncestors in Params.Flags) then exit;
  // search the ancestor name
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // read keyword 'class', 'object', 'interface', 'dispinterface'
  if UpAtomIs('PACKED') then ReadNextAtom;
  ReadNextAtom;
  if not AtomIsChar('(') then begin
    // no ancestor class specified
    // check class name
    ClassIdentNode:=ClassNode.Parent;
    if (ClassIdentNode=nil) or (ClassIdentNode.Desc<>ctnTypeDefinition) then
    begin
      MoveCursorToNodeStart(ClassNode);
      RaiseException(ctsClassWithoutName);
    end;
    // if this class is not TObject, TObject is class ancestor
    SearchTObject:=not CompareSrcIdentifier(ClassIdentNode.StartPos,'TObject');
    if not SearchTObject then begin
      // this is 'TObject', no more ancestors -> stop search
      exit;
    end;
  end else begin
    ReadNextAtom;
    AtomIsIdentifier(true);
    // ancestor name found
    AncestorAtom:=CurPos;
    SearchTObject:=false;
  end;
  {$IFDEF ShowTriedContexts}
  writeln('[TFindDeclarationTool.FindIdentifierInAncestors] ',
  ' Ident="',GetIdentifier(Params.Identifier),'"',
  ' search ancestor class = ',GetAtom);
  writeln('  Flags=[',FindDeclarationFlagsAsString(Params.Flags),']');
  {$ENDIF}
  // search ancestor class context
  CurPos.StartPos:=CurPos.EndPos;
  Params.Save(OldInput);
  try
    Params.Flags:=[fdfSearchInParentNodes,fdfIgnoreCurContextNode,
                   fdfExceptionOnNotFound]
                  +(fdfGlobals*Params.Flags)
                  -[fdfTopLvlResolving];
    if not SearchTObject then
      Params.SetIdentifier(Self,@Src[AncestorAtom.StartPos],nil)
    else begin
      Params.SetIdentifier(Self,'TObject',nil);
      Exclude(Params.Flags,fdfExceptionOnNotFound);
    end;
    Params.ContextNode:=ClassNode;
    if not FindIdentifierInContext(Params) then begin
      MoveCursorToNodeStart(ClassNode);
      //writeln('  AQ*** ',TCodeBuffer(Scanner.MainCode).Filename,' ',CurPos.StartPos);
      RaiseException(ctsDefaultClassAncestorTObjectNotFound);
    end;
    AncestorNode:=Params.NewNode;
    AncestorContext:=Params.NewCodeTool.FindBaseTypeOfNode(Params,AncestorNode);
    Params.Load(OldInput);
    Params.ContextNode:=AncestorContext.Node;
    if (AncestorContext.Tool<>Self)
    and (not (fdfIgnoreClassVisibility in Params.Flags)) then
      Exclude(Params.Flags,fdfClassPrivate);
    Exclude(Params.Flags,fdfIgnoreCurContextNode);
    Result:=AncestorContext.Tool.FindIdentifierInContext(Params);
  finally
    Params.Load(OldInput);
  end;
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
    

- operator overloading
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
  TExprStack = array[0..3] of TOperandAndOperator;
var
  CurExprType: TExpressionType;
  ExprStack: TExprStack;
  StackPtr: integer;
  
  procedure ExecuteStack;
  var NewOperand: TExpressionType;
    LastPos: integer;
  begin
    if StackPtr<=0 then begin
      // only one element -> nothing to do
      exit;
    end;
    LastPos:=CurPos.StartPos;
    while (StackPtr>0)
    and (ExprStack[StackPtr].OperatorLvl>ExprStack[StackPtr-1].OperatorLvl) do
    begin
      // next operand has a lower priority/precedence
      // -> calculate last two operands
      NewOperand:=CalculateBinaryOperator(ExprStack[StackPtr-1].Operand,
        ExprStack[StackPtr].Operand,ExprStack[StackPtr-1].theOperator,
        Params);
      // put result on stack
      dec(StackPtr);
      ExprStack[StackPtr]:=ExprStack[StackPtr+1];
      ExprStack[StackPtr].Operand:=NewOperand;
    end;
    MoveCursorToCleanPos(LastPos);
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
  
var OldFlags: TFindDeclarationFlags;
begin
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.FindExpressionResultType] ',
  '"',copy(Src,StartPos,EndPos-StartPos),'"');
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
    if CurExprType.Desc=xtNone then exit;
    // read operator
    ReadNextAtom;
    // put operand on stack
    inc(StackPtr);
    ExprStack[StackPtr].Operand:=CurExprType;
    ExprStack[StackPtr].theOperator.StartPos:=-1;
    ExprStack[StackPtr].OperatorLvl:=5;
    if CurPos.EndPos>EndPos then begin
      // expression completely parsed
      // -> execute stack
      ExecuteStack;
      Result:=ExprStack[StackPtr].Operand;
      exit;
    end;
    if not WordIsBinaryOperator.DoItUpperCase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
    then
      RaiseBinaryOperatorNotFound;
    // put operator on stack
    ExprStack[StackPtr].theOperator:=CurPos;
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
    ReadNextAtom;
    ExecuteStack;
  until false;
  ExecuteStack;
  Result:=ExprStack[0].Operand;
  Params.Flags:=OldFlags;
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
  OldInput: TFindDeclarationInput;
begin
  Result:=false;
  MoveCursorToUsesEnd(UsesNode);
  repeat
    ReadPriorUsedUnit(UnitNameAtom, InAtom);
    if (fdfIgnoreUsedUnits in Params.Flags) then begin
      if CompareSrcIdentifiers(UnitNameAtom.StartPos,Params.Identifier) then
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
      // open the unit and search the identifier in the interface
      NewCodeTool:=FindCodeToolForUsedUnit(UnitNameAtom,InAtom,false);
      if NewCodeTool=nil then begin
        MoveCursorToCleanPos(UnitNameAtom.StartPos);
        RaiseExceptionFmt(ctsUnitNotFound,[GetAtom(UnitNameAtom)]);
      end else if NewCodeTool=Self then begin
        MoveCursorToCleanPos(UnitNameAtom.StartPos);
        RaiseExceptionFmt(ctsIllegalCircleInUsedUnits,[GetAtom(UnitNameAtom)]);
      end;
      // search the identifier in the interface of the used unit
      Params.Save(OldInput);
      try
        Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobalsSameIdent*Params.Flags)
                     -[fdfExceptionOnNotFound];
        Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
        if Result then exit;
      finally
        Params.Load(OldInput);
      end;
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
  NewCode: TCodeBuffer;
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
  NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename);
  if (NewCode=nil) then begin
    // no source found
    if ExceptionOnNotFound then
      RaiseException('unit '+AnUnitName+' not found');
  end else begin
    // source found -> get codetool for it
    {$IFDEF ShowTriedFiles}
    writeln('[TFindDeclarationTool.FindCodeToolForUsedUnit] ',
    ' This source is=',TCodeBuffer(Scanner.MainCode).Filename,
    ' NewCode=',NewCode.Filename);
    {$ENDIF}
    if Assigned(FOnGetCodeToolForBuffer) then
      Result:=FOnGetCodeToolForBuffer(Self,NewCode)
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
begin
  Result:=false;
  // build code tree
  {$IFDEF ShowTriedContexts}
  writeln(DebugPrefix,'TFindDeclarationTool.FindIdentifierInInterface',
  ' Ident="',GetIdentifier(Params.Identifier),'"',
  ' IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags,
  ' Self=',TCodeBuffer(Scanner.MainCode).Filename
  );
  {$ENDIF}

  // ToDo: build codetree for ppu, ppw, dcu files
  
  // build tree for pascal source
  BuildTree(true);
  if (AskingTool<>Self) and (AskingTool<>nil) then
    AskingTool.AddToolDependency(Self);

  // search identifier in cache
  if FInterfaceIdentifierCache<>nil then begin
    CacheEntry:=FInterfaceIdentifierCache.FindIdentifier(Params.Identifier);
    if CacheEntry<>nil then begin
      // identifier in cache found
      {$IFDEF ShowInterfaceCache}
      writeln('[TFindDeclarationTool.FindIdentifierInInterface] Ident already in cache:',
      ' Exists=',CacheEntry^.Node<>nil);
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
  try
    Params.Flags:=(fdfGlobalsSameIdent*Params.Flags)
                  -[fdfExceptionOnNotFound,fdfSearchInParentNodes];
    Params.ContextNode:=InterfaceNode;
    Result:=FindIdentifierInContext(Params);
  finally
    Params.Load(OldInput)
  end;
  
  // save result in cache
  if FInterfaceIdentifierCache=nil then
    FInterfaceIdentifierCache:=TInterfaceIdentifierCache.Create(Self);
  if Result then begin
    // identifier exists in interface
    if (Params.NewNode.Desc<>ctnProcedure) then begin
      FInterfaceIdentifierCache.Add(OldInput.Identifier,Params.NewNode,
        Params.NewCleanPos);
    end else begin
      // do not save proc identifiers
    end;
  end else
    // identifier does not exist in interface
    FInterfaceIdentifierCache.Add(OldInput.Identifier,nil,-1);
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
{ this function is internally used by FindIdentifierInUsesSection
  for hidden used units, like the system unit or the objpas unit
}
var
  NewCode: TCodeBuffer;
  NewCodeTool: TFindDeclarationTool;
  OldInput: TFindDeclarationInput;
begin
  Result:=false;
  // open the unit and search the identifier in the interface
  NewCode:=FindUnitSource(AnUnitName,'');
  if (NewCode=nil) then begin
    // no source found
    CurPos.StartPos:=-1;
    RaiseExceptionFmt(ctsUnitNotFound,[AnUnitName]);
  end else if NewCode=TCodeBuffer(Scanner.MainCode) then begin
    // Searching again in hidden unit
    writeln('WARNING: Searching again in hidden unit: "',NewCode.Filename,'"');
  end else begin
    // source found -> get codetool for it
    {$IFDEF ShowTriedContexts}
    writeln('[TFindDeclarationTool.FindIdentifierInUsedUnit] ',
    ' This source is=',TCodeBuffer(Scanner.MainCode).Filename,
    ' NewCode=',NewCode.Filename,' IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags);
    {$ENDIF}
    if Assigned(FOnGetCodeToolForBuffer) then begin
      NewCodeTool:=FOnGetCodeToolForBuffer(Self,NewCode);
      if NewCodeTool=nil then begin
        CurPos.StartPos:=-1;
        RaiseExceptionFmt(ctsUnitNotFound,[AnUnitName]);
      end;
    end else if NewCode=TCodeBuffer(Scanner.MainCode) then begin
      NewCodeTool:=Self;
      CurPos.StartPos:=-1;
      RaiseExceptionFmt(ctsIllegalCircleInUsedUnits,[AnUnitName]);
    end;
    // search the identifier in the interface of the used unit
    Params.Save(OldInput);
    try
      Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobalsSameIdent*Params.Flags)
                   -[fdfExceptionOnNotFound];
      Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
    finally
      Params.Load(OldInput);
    end;
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
const
  sutSystem   = 1;
  sutObjPas   = 2;
  sutLineInfo = 3;
  sutHeapTrc  = 4;
  sutNone     = 5;
var
  OldInput: TFindDeclarationInput;
  SystemAlias: string;
  CurUnitType: integer;
begin
  Result:=false;
  {$IFDEF ShowTriedContexts}
  writeln('[TFindDeclarationTool.FindIdentifierInHiddenUsedUnits] ',
  '"',GetIdentifier(Params.Identifier),'" IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags);
  {$ENDIF}
  if (Tree.Root<>nil) and (not (fdfIgnoreUsedUnits in Params.Flags)) then begin
    // check, if this is a special unit
    MoveCursorToNodeStart(Tree.Root);
    ReadNextAtom;
    ReadNextAtom;
    if (Scanner.PascalCompiler<>pcDelphi)
    and Scanner.InitialValues.IsDefined('VER1_0')
    and Scanner.InitialValues.IsDefined('LINUX')
    then
      // ToDo: other OS than linux
      SystemAlias:='SYSLINUX'
    else
      SystemAlias:='SYSTEM';
    if UpAtomIs(SystemAlias) or UpAtomIs('SYSTEM') then
      CurUnitType:=sutSystem
    else if UpAtomIs('OBJPAS') then
      CurUnitType:=sutObjPas
    else if UpAtomIs('LINEINFO') then
      CurUnitType:=sutLineInfo
    else if UpAtomIs('HEAPTRC') then
      CurUnitType:=sutHeapTrc
    else
      CurUnitType:=sutNone;
    // try hidden units
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
      // try hidden used unit 'objpas'
      Result:=FindIdentifierInUsedUnit('ObjPas',Params);
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
          Params.SetIdentifier(Self,PChar(SystemAlias),nil);
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
  CurAtomType, NextAtomType: TVariableAtomType;
  CurAtom, NextAtom: TAtomPosition;
  CurContext, StartContext: TFindContext;
  OldInput: TFindDeclarationInput;
  StartFlags: TFindDeclarationFlags;
  CurExprDesc: TExpressionTypeDesc;
  IsIdentEndOfVar: TIsIdentEndOfVar;

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
    writeln('  InitAtomQueue Expr="',copy(Src,StartPos,EndPos-StartPos),'"');
    {$ENDIF}
    MoveCursorToCleanPos(StartPos);
    ReadNextAtom;
    CurAtom:=CurPos;
    CurAtomType:=GetCurrentAtomType;
    if CurAtomType in [vatRoundBracketOpen,vatEdgedBracketOpen] then
      ReadTilBracketClose(true);
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
    CurAtom:=NextAtom;
    CurAtomType:=NextAtomType;
    MoveCursorToCleanPos(NextAtom.StartPos);
    ReadNextAtom;
    if CurAtomType in [vatRoundBracketOpen,vatEdgedBracketOpen] then
      ReadTilBracketClose(true);
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
    FuncResultNode: TCodeTreeNode;
    ExprType: TExpressionType;
  begin
    if (CurContext.Node<>nil) then begin
      // check if at the end of the variable
      if IsIdentifierEndOfVariable and (fdfFindVariable in StartFlags) then
        // the variable is wanted, not its type
        exit;

      // find base type
      Exclude(Params.Flags,fdfFunctionResult);
      ExprType:=CurContext.Tool.ConvertNodeToExpressionType(CurContext.Node,
                                                            Params);
      CurExprDesc:=ExprType.Desc;
      CurContext:=ExprType.Context;
      if (CurExprDesc=xtContext)
      and (CurContext.Node.Desc in [ctnProcedure,ctnProcedureHead]) then begin
        // check if this is a function
        CurContext.Tool.BuildSubTreeForProcHead(CurContext.Node,
                                                FuncResultNode);
        if (FuncResultNode<>nil) then begin
          // it is function -> use the result type instead of the function
          if IsIdentifierEndOfVariable then begin
            // this function identifier is the end of the variable
            if not (fdfFunctionResult in StartFlags) then
              exit;
          end;
          Include(Params.Flags,fdfFunctionResult);
          ExprType:=CurContext.Tool.ConvertNodeToExpressionType(CurContext.Node,
                                                                Params);
          CurExprDesc:=ExprType.Desc;
          CurContext:=ExprType.Context;
        end;
      end;
    end;
  end;
  
  procedure ResolveIdentifier;
  var
    ProcNode: TCodeTreeNode;
    IdentFound: boolean;
  begin
    // for example  'AnObject[3]'
    // check special identifiers 'Result' and 'Self'
    IdentFound:=false;
    if (CurContext.Node<>nil)
    and (CurContext.Node.Desc in AllPascalStatements) then begin
      if CompareSrcIdentifier(CurAtom.StartPos,'SELF') then begin
        // SELF in a method is the object itself
        // -> check if in a proc
        ProcNode:=CurContext.Node.GetNodeOfType(ctnProcedure);
        if (ProcNode<>nil)
        and FindClassOfMethod(ProcNode,Params,not IsIdentifierEndOfVariable)
        then begin
          CurContext:=CreateFindContext(Params);
          IdentFound:=true;
        end;
      end else if CompareSrcIdentifier(CurAtom.StartPos,'RESULT') then begin
        // RESULT has a special meaning in a function
        // -> check if in a function
        ProcNode:=CurContext.Node.GetNodeOfType(ctnProcedure);
        if (ProcNode<>nil) then begin
          if IsIdentifierEndOfVariable
          and (fdfFindVariable in StartFlags) then begin
            CurContext:=CreateFindContext(CurContext.Tool,ProcNode.FirstChild);
          end else begin
            Params.Save(OldInput);
            try
              Include(Params.Flags,fdfFunctionResult);
              CurContext:=FindBaseTypeOfNode(Params,ProcNode);
            finally
              Params.Load(OldInput);
            end;
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
                    +fdfAllClassVisibilities
                    +(fdfGlobals*Params.Flags);
      if CurContext.Node=StartContext.Node then begin
        // there is no special context -> also search in parent contexts
        Params.Flags:=Params.Flags
                     +[fdfSearchInParentNodes,fdfIgnoreCurContextNode];
      end else begin
        // only search in special context
        Params.ContextNode:=CurContext.Node;
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
      if CurContext.Tool.FindIdentifierInContext(Params) then begin
        CurContext:=CreateFindContext(Params);
      end else begin
        // predefined identifier not redefined
        CurExprDesc:=PredefinedIdentToExprTypeDesc(@Src[CurAtom.StartPos]);
        CurContext:=CreateFindContext(Self,nil);
      end;

      // ToDo: check if identifier in 'Protected' section

      Params.Load(OldInput);
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
    if (CurContext.Node=nil) then begin
      MoveCursorToCleanPos(CurAtom.StartPos);
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsIllegalQualifier,'.']);
    end;
    if (CurContext.Node.Desc in AllUsableSourceTypes) then begin
      // identifier in front of the point is a unit name
      if CurContext.Tool<>Self then begin
        CurContext.Node:=CurContext.Tool.GetInterfaceNode;
      end;
    end;
    // point changes the context to the base type
    // this is already done, so there is not much left to do.
    // Delphi knows . as shortcut for ^.
    // -> check for pointer type
    if (Scanner.CompilerMode=cmDELPHI) and (CurExprDesc=xtContext)
    and (CurContext.Node.Desc=ctnPointerType)
    and (CurContext.Node<>StartContext.Node) then begin
      // left side of expression has defined a special context
      // => this '.' is a dereference
      CurContext:=CurContext.Tool.FindBaseTypeOfNode(Params,
                                                    CurContext.Node.FirstChild);
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
    CurContext:=StartContext;
  end;
  
  procedure ResolveUp;
  begin
    // for example:
    //   1. 'PInt = ^integer'  pointer type
    //   2. a^  dereferencing
    if (not (NextAtomType in [vatSpace,vatPoint,vatUp,vatAS,vatEdgedBracketOpen]))
    or ((CurContext.Node=nil) and (CurExprDesc<>xtPointer))
    then begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIllegalQualifierFound;
    end;
    if (CurExprDesc=xtPointer) then exit;
    if (CurContext.Node<>StartContext.Node) then begin
      // left side of expression has defined a special context
      // => this '^' is a dereference
      if (not
          (NextAtomType in [vatSpace,vatPoint,vatAS,vatUP,vatEdgedBracketOpen]))
      then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaisePointNotFound;
      end;
      if CurContext.Node.Desc<>ctnPointerType then begin
        MoveCursorToCleanPos(CurAtom.StartPos);
        RaiseExceptionFmt(ctsIllegalQualifier,['^']);
      end;
      CurContext:=CurContext.Tool.FindBaseTypeOfNode(Params,
                                                    CurContext.Node.FirstChild);
    end else if NodeHasParentOfType(CurContext.Node,ctnPointerType) then begin
      // this is a pointer type definition
      // -> the default context is ok
    end;
  end;

  procedure ResolveEdgedBracketOpen;
  { for example:  a[]
      this could be:
        1. ranged array
        2. dynamic array
        3. indexed pointer
        4. default property
        5. indexed property
        6. string character
  }

    procedure RaiseTypeIdentNotFound;
    begin
      CurContext.Tool.RaiseExceptionFmt(ctsStrExpectedButAtomFound,
                                   [ctsTypeIdentifier,CurContext.Tool.GetAtom]);
    end;
    
    procedure RaiseIdentInCurContextNotFound;
    begin
      CurContext.Tool.RaiseExceptionFmt(ctsStrExpectedButAtomFound,
                                        [ctsIdentifier,GetAtom]);
    end;
  
  begin
    if not (NextAtomType in [vatSpace,vatPoint,vatAs,vatUp,vatRoundBracketClose,
      vatRoundBracketOpen,vatEdgedBracketClose,vatEdgedBracketOpen])
    or ((CurContext.Node=nil) and (not (CurExprDesc in xtAllStringTypes))) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseIllegalQualifierFound;
    end;
    if CurExprDesc in xtAllStringTypes then begin
      CurExprDesc:=xtChar;
      CurContext.Node:=nil;
      exit;
    end;
    case CurContext.Node.Desc of

    ctnArrayType:
      // the array type is the last child node
      CurContext:=CurContext.Tool.FindBaseTypeOfNode(Params,
                                                     CurContext.Node.LastChild);

    ctnPointerType:
      // the pointer type is the only child node
      CurContext:=CurContext.Tool.FindBaseTypeOfNode(Params,
                                                    CurContext.Node.FirstChild);

    ctnClass, ctnProperty:
      begin
        if CurContext.Node.Desc=ctnClass then begin
          // search default property in class
          Params.Save(OldInput);
          Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound]
                        +fdfGlobals*Params.Flags
                        +fdfAllClassVisibilities*Params.Flags;
          // special identifier for default property
          Params.SetIdentifier(CurContext.Tool,'[',nil);
          Params.ContextNode:=CurContext.Node;
          CurContext.Tool.FindIdentifierInContext(Params);
          CurContext:=CreateFindContext(Params);
          Params.Load(OldInput);
        end;
        // find base type of property
        if CurContext.Tool.ReadTilTypeOfProperty(CurContext.Node) then begin
          // property has type
          Params.Save(OldInput);
          with CurContext.Tool do
            Params.SetIdentifier(CurContext.Tool,@Src[CurPos.StartPos],nil);
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                        +(fdfGlobals*Params.Flags)
                        -[fdfIgnoreUsedUnits];
          Params.ContextNode:=CurContext.Node.Parent;
          if CurContext.Tool.FindIdentifierInContext(Params) then begin
            if Params.NewNode.Desc in [ctnTypeDefinition] then begin
              CurContext:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                                Params.NewNode)
            end else begin
              // not a type
              CurContext.Tool.ReadTilTypeOfProperty(CurContext.Node);
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
        MoveCursorToNodeStart(CurContext.Node);
        ReadNextAtom;
        if UpAtomIs('STRING') or UpAtomIs('ANSISTRING')
        or UpAtomIs('SHORTSTRING') then begin
          CurExprDesc:=xtChar;
          CurContext.Node:=nil;
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
  var ExprType: TExpressionType;
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
    if CurContext.Node<>StartContext.Node then begin
      // typecast or function
    end else begin
      // expression
      ExprType:=FindExpressionResultType(Params,CurAtom.StartPos+1,
                                       CurAtom.EndPos-1);
      CurExprDesc:=ExprType.Desc;
      CurContext:=ExprType.Context;
    end;
  end;

  procedure ResolveINHERITED;
  var
    ProcNode: TCodeTreeNode;
    ClassOfMethodContext: TFindContext;
  begin
    // for example: inherited A;
    // inherited skips the class and begins to search in the ancestor class
    if (CurContext.Node<>StartContext.Node) or (CurContext.Node=nil)
    then begin
      MoveCursorToCleanPos(CurAtom.StartPos);
      RaiseIllegalQualifierFound;
    end;
    if (not NodeIsInAMethod(CurContext.Node)) then begin
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
    writeln('    ResolveINHERITED CurAtomType=',
      VariableAtomTypeNames[CurAtomType],
      ' CurAtom="',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"');
    {$ENDIF}

    // find ancestor of class of method
    ProcNode:=CurContext.Node.GetNodeOfType(ctnProcedure);
    Params.Save(OldInput);
    try
      Params.Flags:=[fdfExceptionOnNotFound]
                    +fdfGlobals*Params.Flags;
      CurContext.Tool.FindClassOfMethod(ProcNode,Params,true);
      ClassOfMethodContext:=CreateFindContext(Params);

      // find class ancestor
      Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                    +fdfGlobals*Params.Flags
                    +fdfAllClassVisibilities*Params.Flags;
      ClassOfMethodContext.Tool.FindAncestorOfClass(ClassOfMethodContext.Node,
                                                    Params,true);

      // search identifier only in class ancestor
      Params.Load(OldInput);
      Params.SetIdentifier(Self,@Src[CurAtom.StartPos],@CheckSrcIdentifier);
      Params.ContextNode:=Params.NewNode;
      Params.Flags:=Params.Flags-[fdfSearchInParentNodes]
                                +[fdfExceptionOnNotFound,fdfSearchInAncestors];
      if not Params.NewCodeTool.FindIdentifierInContext(Params) then begin
        // there is no inherited identifier
        MoveCursorToCleanPos(CurAtom.StartPos);
        ReadNextAtom;
        RaiseIdentNotFound;
      end;
      CurContext:=CreateFindContext(Params);
    finally
      Params.Load(OldInput);
    end;

    ResolveBaseTypeOfIdentifier;
  end;
  
begin
  Result:=CleanExpressionType;
  StartFlags:=Params.Flags;
  StartContext.Node:=Params.ContextNode;
  StartContext.Tool:=Self;
  CurExprDesc:=xtContext;
  CurContext:=StartContext;
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.FindExpressionTypeOfVariable]',
    ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']',
    ' StartContext=',StartContext.Node.DescAsString
  );
  {$ENDIF}
  
  InitAtomQueue;
  repeat
    {$IFDEF ShowExprEval}
    writeln('  FindExpressionTypeOfVariable CurAtomType=',
      VariableAtomTypeNames[CurAtomType],
      ' CurAtom="',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"');
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
  
  Result.Desc:=CurExprDesc;
  Result.Context:=CurContext;
  if (Result.Desc=xtContext) and (not (fdfFindVariable in StartFlags)) then
    Result:=Result.Context.Tool.ConvertNodeToExpressionType(Result.Context.Node,
                                                            Params);
  {$IFDEF ShowExprEval}
  writeln('  FindExpressionTypeOfVariable Result=',ExprTypeToString(Result));
  {$ENDIF}
end;

function TFindDeclarationTool.FindEndOfExpression(StartPos: integer): integer;
begin
  MoveCursorToCleanPos(StartPos);
  Result:=CurPos.StartPos;
  repeat
    ReadNextAtom;
    // read till statement end
    if (CurPos.Flag in [cafSemicolon,cafComma,cafEnd,cafNone,
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
      ConvertNodeToExpressionType.Desc:=
        PredefinedIdentToExprTypeDesc(@Src[CurPos.StartPos]);
      if ConvertNodeToExpressionType.Desc=xtString then begin

        // ToDo: ask scanner, if AnsiString or ShortString

      end;
    end;
  end;
  
var BaseContext: TFindContext;
begin
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.ConvertNodeToExpressionType] A',
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
  writeln('[TFindDeclarationTool.ConvertNodeToExpressionType] B',
  ' Node=',Node.DescAsString);
  {$ENDIF}
  if Node.Desc=ctnRangeType then begin
    // range type -> convert to special expression type

    // ToDo: ppu, ppw, dcu files

    MoveCursorToNodeStart(Node);

    // ToDo: check for circles

    Result:=ReadOperandTypeAtCursor(Params);
    Result.Context:=CreateFindContext(Self,Node);
  end else if Node.Desc=ctnIdentifier then begin

    // ToDo: ppu, ppw, dcu files

    MoveCursorToNodeStart(Node);
    ReadNextAtom;
    ConvertIdentifierAtCursor;
  end else if Node.Desc=ctnProperty then begin

    // ToDo: ppu, ppw, dcu files

    ExtractPropType(Node,false,true);
    if CurPos.Flag<>cafEdgedBracketOpen then
      ConvertIdentifierAtCursor;
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
      writeln('[TFindDeclarationTool.ReadOperandTypeAtCursor] Set of ',
      ExpressionTypeDescNames[Result.Desc]);
      if Result.Desc=xtContext then
        writeln('  Result.Context.Node=',Result.Context.Node.DescAsString);
      {$ENDIF}
      if not (Result.Desc in [xtConstOrdInteger,xtChar])
      and ((Result.Desc=xtContext)
        and (Result.Context.Node.Desc<>ctnEnumerationType)) then
      begin
        MoveCursorToCleanPos(SubStartPos);
        ReadNextAtom; // read '['
        ReadNextAtom;
        RaiseConstExpected;
      end;
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

begin
  Result:=CleanExpressionType;

  if CurPos.StartPos=CurPos.EndPos then ReadNextAtom;
  // read unary operators which have no effect on the type: +, -, not
  while AtomIsChar('+') or AtomIsChar('-') or UpAtomIs('NOT') do
    ReadNextAtom;
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.ReadOperandTypeAtCursor] A Atom=',GetAtom);
  {$ENDIF}
  if UpAtomIs('INHERITED') or (AtomIsIdentifier(false))
  or AtomIsChar('(') then begin
    // read variable
    SubStartPos:=CurPos.StartPos;
    EndPos:=FindEndOfVariable(SubStartPos,false);
    Params.Flags:=Params.Flags+[fdfFunctionResult]-[fdfIgnoreOverloadedProcs];
    Result:=FindExpressionTypeOfVariable(SubStartPos,EndPos,Params);
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
    // a simple pointer or an event
    Params.Flags:=Params.Flags-[fdfFunctionResult]+[fdfIgnoreOverloadedProcs];
    MoveCursorToCleanPos(CurPos.EndPos);
    Result:=ReadOperandTypeAtCursor(Params);
    if (Result.Desc=xtContext) or (Result.Context.Node.Desc=ctnProcedure)
    then
      Result.SubDesc:=Result.Desc
    else
      Result.Context:=CleanFindContext;
    Result.Desc:=xtPointer;
  end
  else
    RaiseIdentExpected;

  {$IFDEF ShowExprEval}
  write('[TFindDeclarationTool.ReadOperandTypeAtCursor] END ',
  ExpressionTypeDescNames[Result.Desc]);
  if Result.Context.Node<>nil then
    write(' Context.Node=',Result.Context.Node.DescAsString)
  else
    write(' Context.Node=nil');
  writeln('');
  {$ENDIF}
end;

function TFindDeclarationTool.CalculateBinaryOperator(LeftOperand,
  RightOperand: TExpressionType; BinaryOperator: TAtomPosition;
  Params: TFindDeclarationParams): TExpressionType;
begin
  Result:=CleanExpressionType;
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.CalculateBinaryOperator] A',
  ' LeftOperand=',ExpressionTypeDescNames[LeftOperand.Desc],
  ' Operator=',copy(Src,BinaryOperator.StartPos,
                    BinaryOperator.EndPos-BinaryOperator.StartPos),
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
    and (LeftOperand.Desc in [xtAnsiString,xtShortString,xtString,xtChar])
    then begin
      // string/char '+'
      if (RightOperand.Desc in [xtAnsiString,xtShortString,xtString,xtChar])
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

function TFindDeclarationTool.IsParamListCompatible(
  FirstParameterNode: TCodeTreeNode;
  ExprParamList: TExprTypeList;  IgnoreMissingParameters: boolean;
  Params: TFindDeclarationParams;
  CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
// tests if ExprParamList fits into the FirstParameterNode
var
  ParamNode: TCodeTreeNode;
  i, MinParamCnt, MaxParamCnt: integer;
  ParamCompatibility: TTypeCompatibility;
begin
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.IsParamListCompatible] ',
  ' ExprParamList.Count=',ExprParamList.Count,
  ' FirstParameterNode=',FirstParameterNode<>nil
  );
    try
  {$ENDIF}
  Result:=tcExact;

  // quick check: parameter count
  ParamNode:=FirstParameterNode;
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
  if (ExprParamlist.Count>MaxParamCnt)
  or ((not IgnoreMissingParameters) and (ExprParamList.Count<MinParamCnt)) then
  begin
    Result:=tcIncompatible;
    exit;
  end;

  // check each parameter for compatibility
  ParamNode:=FirstParameterNode;
  i:=0;
  while (ParamNode<>nil) and (i<ExprParamList.Count) do begin
    ParamCompatibility:=IsCompatible(ParamNode,ExprParamList.Items[i],Params);
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
  if (i<ExprParamList.Count) then begin
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
  writeln('[TFindDeclarationTool.IsParamListCompatible] END ',
  ' Result=',TypeCompatibilityNames[Result],' ! ONLY VALID if no error !'
  );
    end;
  {$ENDIF}
end;

function TFindDeclarationTool.IsParamListCompatible(FirstParameterNode1,
  FirstParameterNode2: TCodeTreeNode; Params: TFindDeclarationParams;
  CompatibilityList: TTypeCompatibilityList): TTypeCompatibility;
var
  CurParamNode1, CurParamNode2: TCodeTreeNode;
  ParamCompatibility: TTypeCompatibility;
  ExprType1, ExprType2: TExpressionType;
  OldFlags: TFindDeclarationFlags;
  i: integer;
begin
  // quick check: parameter count
  CurParamNode1:=FirstParameterNode1;
  CurParamNode2:=FirstParameterNode2;
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
  CurParamNode1:=FirstParameterNode1;
  CurParamNode2:=FirstParameterNode2;
  Result:=tcExact;
  i:=0;
  while (CurParamNode1<>nil) and (CurParamNode2<>nil) do begin
    ExprType1:=ConvertNodeToExpressionType(CurParamNode1,Params);
    ExprType2:=ConvertNodeToExpressionType(CurParamNode2,Params);
    ParamCompatibility:=IsBaseCompatible(ExprType1,ExprType2,Params);
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
  FoundContext: TFindContext): TIdentifierFoundResult;
// this is a TOnIdentifierFound function
//   if identifier found is a proc then it searches for the best overloaded proc
var FirstParameterNode: TCodeTreeNode;
  ParamCompatibility: TTypeCompatibility;
  OldInput: TFindDeclarationInput;
  CurCompatibilityList: TTypeCompatibilityList;
  CompListSize: integer;
begin
  // the search has found an identifier with the right name
  {$IFDEF ShowFoundIdentifier}
  writeln('[TFindDeclarationTool.CheckSrcIdentifier]',
  ' Indent=',GetIdentifier(Params.Identifier),
  ' FoundContext=',FoundContext.Node.DescAsString,
  ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
  );
  {$ENDIF}
  if FoundContext.Node.Desc=ctnProcedure then begin
    // the found node is a proc
    Include(Params.NewFlags,fdfDoNotCache);
    if (fdfIgnoreOverloadedProcs in Params.Flags) then begin
      // do not check for overloaded procs -> ident found
      Result:=ifrSuccess;
      exit;
    end;
    
    // Procs can be overloaded, that means there can be several procs with the
    // same name, but with different param lists.
    // The search must go on, and the most compatible proc is returned.
    Result:=ifrProceedSearch;
    if (Params.FoundProc=nil) then begin
      // this is the first proc found
      // -> save it and proceed the search to find all overloadeded procs
      Params.SetFirstFoundProc(FoundContext);
      exit;
    end;
    
    // this is another overloaded proc
    // -> check which one is more compatible
    if not Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then begin
      // Params.Identifier is not in the source of this tool
      // => impossible to check param list, because the context is unknown
      // -> identifier found
      Result:=ifrSuccess;
    end;

    // create the input expression list
    // (the expressions in the brackets are parsed and converted to types)
    if Params.FoundProc^.ExprInputList=nil then begin
      {$IFDEF ShowFoundIdentifier}
      writeln('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Indent=',GetIdentifier(Params.Identifier),
      ' Creating Input Expression List ...'
      );
      {$ENDIF}
      Params.Save(OldInput);
      try
        Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
        Params.Flags:=fdfDefaultForExpressions;
        Params.ContextNode:=Params.IdentifierTool.FindDeepestNodeAtPos(
          CurPos.StartPos,true);
        Params.OnIdentifierFound:=@Self.CheckSrcIdentifier;
        Params.IdentifierTool.ReadNextAtom;
        Params.FoundProc^.ExprInputList:=
                            Params.IdentifierTool.CreateParamExprList(
                                    Params.IdentifierTool.CurPos.EndPos,Params);
      finally
        Params.Load(OldInput);
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
      writeln('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Indent=',GetIdentifier(Params.Identifier),
      ' Check the first found proc for compatibility ...'
      );
      {$ENDIF}
      FirstParameterNode:=
        Params.FoundProc^.Context.Tool.GetFirstParameterNode(
              Params.FoundProc^.Context.Node);
      Params.Save(OldInput);
      try
        ParamCompatibility:=
          Params.FoundProc^.Context.Tool.IsParamListCompatible(
            FirstParameterNode,
            Params.FoundProc^.ExprInputList,
            fdfIgnoreMissingParams in Params.Flags,
            Params,Params.FoundProc^.ParamCompatibilityList);
      finally
        Params.Load(OldInput);
      end;
      Params.FoundProc^.ProcCompatibility:=ParamCompatibility;
      Params.FoundProc^.CacheValid:=true;
      {$IFDEF ShowFoundIdentifier}
      writeln('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Indent=',GetIdentifier(Params.Identifier),
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
    writeln('[TFindDeclarationTool.CheckSrcIdentifier]',
    ' Indent=',GetIdentifier(Params.Identifier),
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
      try
        ParamCompatibility:=
          FoundContext.Tool.IsParamListCompatible(
            FirstParameterNode,
            Params.FoundProc^.ExprInputList,
            fdfIgnoreMissingParams in Params.Flags,
            Params,CurCompatibilityList);
      finally
        Params.Load(OldInput);
      end;
      {$IFDEF ShowFoundIdentifier}
      writeln('[TFindDeclarationTool.CheckSrcIdentifier]',
      ' Indent=',GetIdentifier(Params.Identifier),
      ' Current Proc ParamCompatibility=',TypeCompatibilityNames[ParamCompatibility]
      );
      {$ENDIF}
      if ParamCompatibility=tcExact then begin
        // the current proc fits exactly -> stop the search
        Params.ChangeFoundProc(FoundContext,ParamCompatibility,
          CurCompatibilityList);
        CurCompatibilityList:=nil; // set to nil, so that it will no be freed
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
          CurCompatibilityList:=nil; // set to nil, so that it will no be freed
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

  // ToDo: check if identifier is in a forbidden class visibility section

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
  ExpressionType: TExpressionType;
  Params: TFindDeclarationParams): TTypeCompatibility;
var TargetContext: TFindContext;
  OldInput: TFindDeclarationInput;
  NodeExprType: TExpressionType;
begin
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.IsCompatible] A Node=',TargetNode.DescAsString,
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
    writeln('[TFindDeclarationTool.IsCompatible] TargetContext.Node.Desc=ctnSetType',
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
  writeln('[TFindDeclarationTool.IsCompatible] END',
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
  else if WordIsPredefinedIdentifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
    CurPos.EndPos-CurPos.StartPos)
  then
    Result:=vatPreDefIdentifier
  else if AtomIsIdentifier(false) then
    Result:=vatIdentifier
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
  else if UpAtomIs('INHERITED') then
    Result:=vatINHERITED
  else if UpAtomIs('AS') then
    Result:=vatAS
  else
    Result:=vatNone;
end;

function TFindDeclarationTool.CreateParamExprList(StartPos: integer;
  Params: TFindDeclarationParams): TExprTypeList;
var ExprType: TExpressionType;
  BracketClose: char;
  ExprStartPos, ExprEndPos: integer;
  
  procedure RaiseBracketNotFound;
  begin
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[BracketClose,GetAtom]);
  end;
  
begin
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.CreateParamExprList] ',
  '"',copy(Src,StartPos,40),'"');
  {$ENDIF}
  Result:=TExprTypeList.Create;
  MoveCursorToCleanPos(StartPos);
  ReadNextAtom; // reads first atom after proc name
  if AtomIsChar('(') then
    BracketClose:=')'
  else if AtomIsChar('[') then
    BracketClose:=']'
  else
    BracketClose:=#0;
  if BracketClose<>#0 then begin
    // read parameter list
    ReadNextAtom;
    if not AtomIsChar(BracketClose) then begin
      // read all expressions
      while true do begin
        ExprStartPos:=CurPos.StartPos;
        // read til comma or bracket close
        repeat
          if AtomIsChar('(') or AtomIsChar('[') then begin
            ReadTilBracketClose(true);
          end;
          ReadNextAtom;
          if (CurPos.StartPos>SrcLen)
          or ((CurPos.EndPos=CurPos.StartPos+1)
            and (Src[CurPos.StartPos] in [')',']',',']))
          then
            break;
        until false;
        ExprEndPos:=CurPos.StartPos;
        // find expression type
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
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.CreateParamExprList] END ',
  'ParamCount=',Result.Count,' "',copy(Src,StartPos,40),'"');
  writeln('  ExprList=[',Result.AsString,']');
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
  writeln('[TFindDeclarationTool.CompatibilityList1IsBetter] END i=',i);
  {$ENDIF}
end;

function TFindDeclarationTool.ContextIsDescendOf(DescendContext,
  AncestorContext: TFindContext; Params: TFindDeclarationParams): boolean;
  
  procedure RaiseInternalError;
  begin
    RaiseException('[TFindDeclarationTool.ContextIsDescendOf] '
      +' internal error: DescendContext.Desc<>ctnClass');
  end;
  
var CurContext: TFindContext;
  OldInput: TFindDeclarationInput;
begin
  if DescendContext.Node.Desc<>ctnClass then
    RaiseInternalError;
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.ContextIsDescendOf] ',
  ' DescendContext="',copy(DescendContext.Tool.Src,DescendContext.Node.Parent.StartPos,15),'"');
  {$ENDIF}
  CurContext:=DescendContext;
  Params.Save(OldInput);
  repeat
    Result:=CurContext.Tool.FindAncestorOfClass(CurContext.Node,Params,true);
    if Result then begin
      CurContext:=CreateFindContext(Params);
      {$IFDEF ShowExprEval}
      writeln('[TFindDeclarationTool.ContextIsDescendOf] B ',
      ' CurContext="',copy(CurContext.Tool.Src,CurContext.Node.Parent.StartPos,15),'"');
      {$ENDIF}
      Result:=FindContextAreEqual(CurContext,AncestorContext);
      if Result then exit;
    end else
      break;
  until false;
  Result:=false;
end;

function TFindDeclarationTool.IsBaseCompatible(TargetType,
  ExpressionType: TExpressionType; Params: TFindDeclarationParams
  ): TTypeCompatibility;
// can ExpressionType be assigned to TargetType
// both expression types must be base types
var TargetNode, ExprNode: TCodeTreeNode;
begin
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.IsBaseCompatible] B ',
  ' TargetType=',ExpressionTypeDescNames[TargetType.Desc],
  ' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc]);
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
        writeln('[TFindDeclarationTool.IsBaseCompatible] C ',
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
          
          ctnClass:
            // check, if ExpressionType.Context is descend of TargetContext
            if ContextIsDescendOf(ExpressionType.Context,
                                  TargetType.Context,Params)
            then
              Result:=tcCompatible;
              
          ctnArrayType:
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
  end else begin
    // check, if ExpressionType can be auto converted into TargetType
    if ((TargetType.Desc in xtAllRealTypes)
      and (ExpressionType.Desc in xtAllRealConvertibles))
    or ((TargetType.Desc in xtAllStringTypes)
      and (ExpressionType.Desc in xtAllStringConvertibles))
    or ((TargetType.Desc in xtAllIntegerTypes)
      and (ExpressionType.Desc in xtAllIntegerConvertibles))
    or ((TargetType.Desc in xtAllBooleanTypes)
      and (ExpressionType.Desc in xtAllBooleanConvertibles))
    or ((TargetType.Desc in xtAllPointerTypes)
      and (ExpressionType.Desc in xtAllPointerConvertibles))
    then
      Result:=tcCompatible
    else if (TargetType.Desc=xtContext) then begin
      if ((TargetType.Context.Node.Desc in [ctnClass,ctnProcedure])
        and (ExpressionType.Desc=xtNil))
      or ((TargetType.Context.Node.Desc=ctnArrayType)
        and (TargetType.Context.Node.FirstChild<>nil)
        and (TargetType.Context.Node.FirstChild.Desc=ctnOfConstType)
        and (ExpressionType.Desc=xtConstSet))
      then
        Result:=tcCompatible
    end;
  end;
  {$IFDEF ShowExprEval}
  writeln('[TFindDeclarationTool.IsBaseCompatible] END ',
  ' TargetType=',ExpressionTypeDescNames[TargetType.Desc],
  ' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc],
  ' Result=',TypeCompatibilityNames[Result]
  );
  {$ENDIF}
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
  writeln('[TFindDeclarationTool.NodeCacheGlobalWriteLockStepDidNotChange] Result=',Result,' ',MainFilename);
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
  writeln('[TFindDeclarationTool.CheckDependsOnNodeCaches] START ',MainFilename);
  {$ENDIF}
  try
    ANode:=FDependsOnCodeTools.FindLowest;
    while ANode<>nil do begin
      ATool:=TFindDeclarationTool(ANode.Data);
      Result:=ATool.CheckDependsOnNodeCaches;
      if Result then exit;
      ANode:=FDependsOnCodeTools.FindSuccessor(ANode);
    end;
    Result:=UpdateNeeded(Scanner.ScanTillInterfaceEnd);
  finally
    {$IFDEF ShowCacheDependencies}
    writeln('[TFindDeclarationTool.CheckDependsOnNodeCaches] Result=',Result,' ',MainFilename);
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
  writeln('[TFindDeclarationTool.ClearNodeCaches] Force=',Force,' ',MainFilename);
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
  writeln('[TFindDeclarationTool.ClearDependentNodeCaches] ',MainFilename);
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
  writeln('[TFindDeclarationTool.ClearDependsOnToolRelationships] ',MainFilename);
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
  writeln('[TFindDeclarationTool.AddToolDependency] "',MainFilename,'" depends on "',DependOnTool.MainFilename,'"');
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
  beVerbose:=CompareSrcIdentifiers(Params.Identifier,'VISIBLE');
  if beVerbose then begin
    writeln('(((((((((((((((((((((((((((==================');
    
    write('TFindDeclarationTool.AddResultToNodeCaches ',
    ' Ident=',GetIdentifier(Params.Identifier));
    write(' SearchedForward=',SearchedForward);
    write(' Flags=[');
    if ncefSearchedInParents in SearchRangeFlags then write('Parents');
    if ncefSearchedInAncestors in SearchRangeFlags then write(',Ancestors');
    writeln(']');
    
    writeln('  Tool=',MainFilename);
    
    write('     StartNode=',StartNode.DescAsString,'="',copy(Src,StartNode.StartPos-10,10),'|',copy(Src,StartNode.StartPos,15),'"');
    if EndNode<>nil then
      write(' EndNode=',EndNode.DescAsString,'="',copy(Src,EndNode.StartPos,25),'"')
    else
      write(' EndNode=nil');
    writeln('');
    
    writeln('  StartNode(',StartNode.StartPos,'-',StartNode.EndPos,')');
    if EndNode<>nil then
      writeln('  EndNode(',EndNode.StartPos,'-',EndNode.EndPos,')');

    writeln('     Self=',MainFilename);
    
    if NewNode<>nil then begin
      writeln('       NewNode=',NewNode.DescAsString,
                 ' NewTool=',NewTool.MainFilename);
    end else begin
      writeln('       NOT FOUND');
    end;
    
    writeln('  CleanStartPos=',CleanStartPos,' "',copy(Src,CleanStartPos,70),'"');
    writeln('  CleanEndPos=',CleanEndPos,' "',copy(Src,CleanEndPos-70,70),'"');
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
    writeln('=========================))))))))))))))))))))))))))))))))');
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
  NodeStack: PCodeTreeNodeStack; Result: TFindContext);
var i: integer;
  Node: TCodeTreeNodeStackEntry;
  BaseTypeCache: TBaseTypeCache;
begin
  {$IFDEF ShowBaseTypeCache}
  write('[TFindDeclarationTool.CreateBaseTypeCaches] ',
  ' StackPtr=',NodeStack^.StackPtr);
  writeln(' Self=',MainFilename);
  if Result.Node<>nil then
    write(' Result=',Result.Node.DescAsString,
       ' Start=',Result.Node.StartPos,
       ' End=',Result.Node.EndPos,
       ' "',copy(Src,Result.Node.StartPos,15),'" ',Result.Tool.MainFilename)
  else
    write(' Result=nil');
  writeln('');
  {$ENDIF}
  for i:=0 to (NodeStack^.StackPtr-1) do begin
    Node:=GetNodeStackEntry(NodeStack,i);
    if (Node.Cache=nil)
    and ((Result.Tool<>Self) or (Result.Node<>Node)) then begin
      {$IFDEF ShowBaseTypeCache}
      writeln('  i=',i,' Node=',Node.DescAsString,' "',copy(Src,Node.StartPos,15),'"');
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
                 fdfTopLvlResolving,fdfFindVariable]
                +fdfAllClassVisibilities;
  ExprType:=FindExpressionResultType(Params,TermAtom.StartPos,TermAtom.EndPos);
  {$IFDEF CTDEBUG}
  writeln('TCodeCompletionCodeTool.FindTermTypeAsString ExprTypeToString=',
    ExprTypeToString(ExprType));
  {$ENDIF}
  case ExprType.Desc of
    xtNone:
      RaiseTermNotSimple;

    xtContext:
      begin
        FindContext:=ExprType.Context;
        
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
          
        else
          RaiseTermNotSimple;
        end;
      end;

    xtChar,
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
    RaiseTermNotSimple;
  end;
end;


{ TFindDeclarationParams }

constructor TFindDeclarationParams.Create;
begin
  inherited Create;
  Clear;
end;

procedure TFindDeclarationParams.Clear;
begin
  ClearInput;
  ClearResult;
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

procedure TFindDeclarationParams.ClearResult;
begin
  NewPos.Code:=nil;
  NewPos.X:=-1;
  NewPos.Y:=-1;
  NewTopLine:=-1;
  NewNode:=nil;
  NewCleanPos:=-1;
  NewCodeTool:=nil;
  NewFlags:=[];
end;

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TFindDeclarationTool;
  ANewNode: TCodeTreeNode);
begin
  ClearResult;
  NewCodeTool:=ANewCodeTool;
  NewNode:=ANewNode;
end;

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TFindDeclarationTool;
  ANewNode: TCodeTreeNode; ANewCleanPos: integer);
begin
  ClearResult;
  NewCodeTool:=ANewCodeTool;
  NewNode:=ANewNode;
  NewCleanPos:=ANewCleanPos;
end;

procedure TFindDeclarationParams.ConvertResultCleanPosToCaretPos;
begin
  NewPos.Code:=nil;
  if NewCodeTool<>nil then begin
    if (NewCleanPos>=1) then
      NewCodeTool.CleanPosToCaretAndTopLine(NewCleanPos,
                 NewPos,NewTopLine)
    else if (NewNode<>nil) then
      NewCodeTool.CleanPosToCaretAndTopLine(NewNode.StartPos,
                 NewPos,NewTopLine);
  end;
end;

procedure TFindDeclarationParams.ClearInput;
begin
  Flags:=[];
  Identifier:=nil;
  ContextNode:=nil;
  OnIdentifierFound:=nil;
  IdentifierTool:=nil;
  ClearFoundProc;
end;

procedure TFindDeclarationParams.ClearFoundProc;
begin
  if FoundProc=nil then exit;
  with FoundProc^ do begin
    if ExprInputList<>nil then
      FreeAndNil(ExprInputList);
    if ParamCompatibilityList<>nil then
      FreeMem(ParamCompatibilityList);
    CacheValid:=false;
  end;
  Dispose(FoundProc);
  FoundProc:=nil;
end;

procedure TFindDeclarationParams.SetResult(AFindContext: TFindContext);
begin
  ClearResult;
  NewCodeTool:=AFindContext.Tool;
  NewNode:=AFindContext.Node;
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

procedure TFindDeclarationParams.SetFirstFoundProc(ProcContext: TFindContext);
begin
  New(FoundProc);
  FillChar(FoundProc^,SizeOf(TFoundProc),0);
  FoundProc^.Context:=ProcContext;
end;

procedure TFindDeclarationParams.ChangeFoundProc(ProcContext: TFindContext;
  ProcCompatibility: TTypeCompatibility;
  ParamCompatibilityList: TTypeCompatibilityList);
begin
  FoundProc^.Context:=ProcContext;
  FoundProc^.ProcCompatibility:=ProcCompatibility;
  if FoundProc^.ParamCompatibilityList<>nil then
    FreeMem(FoundProc^.ParamCompatibilityList);
  FoundProc^.ParamCompatibilityList:=ParamCompatibilityList;
end;

procedure TFindDeclarationParams.SetResult(
  NodeCacheEntry: PCodeTreeNodeCacheEntry);
begin
  ClearResult;
  NewCodeTool:=TFindDeclarationTool(NodeCacheEntry^.NewTool);
  NewNode:=NodeCacheEntry^.NewNode;
  NewCleanPos:=NodeCacheEntry^.NewCleanPos;
  NewFlags:=[fdfDoNotCache];
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

procedure TExprTypeList.Add(ExprType: TExpressionType);
begin
  inc(Count);
  if Count>Capacity then Grow;
  Items[Count-1]:=ExprType;
end;

procedure TExprTypeList.AddFirst(ExprType: TExpressionType);
begin
  inc(Count);
  if Count>Capacity then Grow;
  if Count>1 then
    Move(Items[0],Items[1],SizeOf(TExpressionType)*(Count-1));
  Items[0]:=ExprType;
end;


end.


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
    - Examples:
       - @ operator
       - 'inherited'
       - variants
       - array of const
       - interfaces
       - Get and Set property access parameter lists
       - operator overloading
       - ppu, ppw, dcu files

}
unit FindDeclarationTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

// activate for debug:

{ $DEFINE CTDEBUG}
{$DEFINE ShowSearchPaths}
{ $DEFINE ShowTriedFiles}
{ $DEFINE ShowTriedContexts}
{ $DEFINE ShowTriedIdentifiers}
{ $DEFINE ShowExprEval}
{ $DEFINE ShowFoundIdentifier}
{ $DEFINE ShowInterfaceCache}
{ $DEFINE ShowNodeCache}
{ $DEFINE ShowBaseTypeCache}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, CustomCodeTool, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  PascalParserTool, FileProcs, DefineTemplates, FindDeclarationCache;

type
  TFindDeclarationTool = class;

  // searchpath delimiter is semicolon
  TOnGetSearchPath = function(Sender: TObject): string of object;
  TOnGetCodeToolForBuffer = function(Sender: TObject;
    Code: TCodeBuffer): TFindDeclarationTool of object;

  TFindDeclarationFlag = (
    fdfSearchInParentNodes, // if identifier not found in current context,
                            //    proceed in prior nodes on same lvl and parents
    fdfSearchInAncestors,   // if context is a class, search also in
                            //    ancestors/interfaces
    fdfIgnoreCurContextNode,// skip context and proceed in prior/parent context
    fdfExceptionOnNotFound, // raise exception if identifier not found
    fdfIgnoreUsedUnits,     // stay in current source
    fdfSearchForward,       // instead of searching in prior nodes, search in
                            //    next nodes (successors)
    fdfIgnoreClassVisibility,//find inaccessible private+protected fields
    fdfClassPublished,fdfClassPublic,fdfClassProtected,fdfClassPrivate,
    fdfIgnoreMissingParams, // found proc fits, even if parameters are missing
    fdfFirstIdentFound,     // a first identifier was found, now searching for
                            // the a better one (used for proc overloading)
    fdfOnlyCompatibleProc,  // incompatible procs are ignored
    fdfNoExceptionOnStringChar,// the bracket operator after a predefined string
                            // is of type char, which is also predefined, so it
                            // can not be resolved normally
    fdfFunctionResult,      // if searching base type of function,
                            //    return result type
    fdfCollect              // return every reachable identifier
    );
  TFindDeclarationFlags = set of TFindDeclarationFlag;
  
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
    xtNone, xtContext, xtChar, xtReal, xtSingle, xtDouble,
    xtExtended, xtCurrency, xtComp, xtInt64, xtCardinal, xtQWord, xtBoolean,
    xtByteBool, xtLongBool, xtString, xtAnsiString, xtShortString, xtWideString,
    xtPChar, xtPointer, xtConstOrdInteger, xtConstString, xtConstReal,
    xtConstSet, xtConstBoolean, xtAddress, xtLongInt, xtWord, xtNil);
  TExpressionTypeDescs = set of TExpressionTypeDesc;
  
const
  ExpressionTypeDescNames : array[TExpressionTypeDesc] of string = (
    'None', 'Context', 'Char', 'Real', 'Single', 'Double',
    'Extended', 'Currency', 'Comp', 'Int64', 'Cardinal', 'QWord', 'Boolean',
    'ByteBool', 'LongBool', 'String', 'AnsiString', 'ShortString', 'WideString',
    'PChar', 'Pointer', 'ConstOrdInt', 'ConstString', 'ConstReal', 'ConstSet',
    'ConstBoolean', '@-Operator', 'LongInt', 'Word', 'Nil'
  );

  xtAllTypes = [xtContext..High(TExpressionTypeDesc)];
  xtAllPredefinedTypes = xtAllTypes-[xtContext];
  xtAllIntegerTypes = [xtInt64, xtQWord, xtConstOrdInteger, xtLongInt, xtWord];
  xtAllBooleanTypes = [xtBoolean, xtByteBool, xtLongBool];
  xtAllRealTypes = [xtReal, xtConstReal, xtSingle, xtDouble, xtExtended,
                    xtCurrency, xtComp];
  xtAllStringTypes = [xtConstString, xtShortString, xtString, xtAnsiString];
  xtAllPointerTypes = [xtPointer, xtNil];
  xtAllIntegerConvertibles = xtAllIntegerTypes;
  xtAllRealConvertibles = xtAllRealTypes+xtAllIntegerTypes;
  xtAllStringConvertibles = xtAllStringTypes+[xtChar,xtPChar];
  xtAllBooleanConvertibles = xtAllBooleanTypes+[xtConstBoolean];
  xtAllPointerConvertibles = xtAllPointerTypes+[xtPChar,xtAddress];

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
      'Exact', 'Compatible', 'Incompatible'
     );

type
  // TExprTypeList is used for compatibility checks of whole parameter lists
  TExprTypeList = class
  public
    Count: integer;
    Items: ^TExpressionType;
    procedure Add(ExprType: TExpressionType);
    destructor Destroy; override;
    function AsString: string;
  end;
  

  //---------------------------------------------------------------------------
  TIdentifierFoundResult = (ifrProceedSearch, ifrAbortSearch, ifrSuccess);

  TOnIdentifierFound = function(Params: TFindDeclarationParams;
    FoundContext: TFindContext): TIdentifierFoundResult of object;
    
  TFindDeclarationInput = record
    Flags: TFindDeclarationFlags;
    Identifier: PChar;
    ContextNode: TCodeTreeNode;
    OnIdentifierFound: TOnIdentifierFound;
    IdentifierTool: TFindDeclarationTool;
    FirstClassNode: TCodeTreeNode;
  end;

  TFindDeclarationParams = class(TObject)
  public
    // input parameters:
    Flags: TFindDeclarationFlags;
    Identifier: PChar;
    ContextNode: TCodeTreeNode;
    OnIdentifierFound: TOnIdentifierFound;
    IdentifierTool: TFindDeclarationTool;
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
    procedure ConvertResultCleanPosToCaretPos;
    procedure ClearResult;
    procedure ClearInput;
  end;

  TFindDeclarationTool = class(TPascalParserTool)
  private
    FInterfaceIdentifierCache: TInterfaceIdentifierCache;
    FOnGetCodeToolForBuffer: TOnGetCodeToolForBuffer;
    FOnGetUnitSourceSearchPath: TOnGetSearchPath;
    FFirstNodeCache: TCodeTreeNodeCache;
    FLastNodeCachesGlobalWriteLockStep: integer;
    FRootNodeCache: TCodeTreeNodeCache;
    FFirstBaseTypeCache: TBaseTypeCache;
    {$IFDEF CTDEBUG}
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
    function NodeIsInAMethod(Node: TCodeTreeNode): boolean;
  protected
    procedure DoDeleteNodes; override;
    procedure ClearNodeCaches(Force: boolean);
    function CreateNewNodeCache(Node: TCodeTreeNode): TCodeTreeNodeCache;
    function CreateNewBaseTypeCache(Node: TCodeTreeNode): TBaseTypeCache;
    procedure CreateBaseTypeCaches(NodeStack: PCodeTreeNodeStack;
      Result: TFindContext);
    function GetNodeCache(Node: TCodeTreeNode;
      CreateIfNotExists: boolean): TCodeTreeNodeCache;
    procedure AddResultToNodeCaches(Identifier: PChar;
      StartNode, EndNode: TCodeTreeNode; SearchedForward: boolean;
      Params: TFindDeclarationParams; SearchRangeFlags: TNodeCacheEntryFlags);
  protected
    function FindDeclarationOfIdentifier(
      Params: TFindDeclarationParams): boolean;
    function FindContextNodeAtCursor(
      Params: TFindDeclarationParams): TFindContext;
    function FindIdentifierInContext(Params: TFindDeclarationParams): boolean;
    function FindBaseTypeOfNode(Params: TFindDeclarationParams;
      Node: TCodeTreeNode): TFindContext;
    function GetExpressionTypeOfTypeIdentifier(
      Params: TFindDeclarationParams): TExpressionType;
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
    function CreateParamExprList(StartPos: integer;
      Params: TFindDeclarationParams): TExprTypeList;
    function ContextIsDescendOf(DescendContext, AncestorContext: TFindContext;
      Params: TFindDeclarationParams): boolean;
    function IsCompatible(TargetType, ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
    function IsCompatible(Node: TCodeTreeNode; ExpressionType: TExpressionType;
      Params: TFindDeclarationParams): TTypeCompatibility;
    // expressions, operands, variables
    function FindEndOfVariable(StartPos: integer): integer;
    function FindExpressionTypeOfVariable(StartPos: integer;
      Params: TFindDeclarationParams; var EndPos: integer): TExpressionType;
    function ConvertNodeToExpressionType(Node: TCodeTreeNode;
      Params: TFindDeclarationParams): TExpressionType;
    function ReadOperandTypeAtCursor(
      Params: TFindDeclarationParams): TExpressionType;
    function CalculateBinaryOperator(LeftOperand, RightOperand: TExpressionType;
      BinaryOperator: TAtomPosition;
      Params: TFindDeclarationParams): TExpressionType;
    function GetParameterNode(Node: TCodeTreeNode): TCodeTreeNode;
    function GetFirstParameterNode(Node: TCodeTreeNode): TCodeTreeNode;
    function PredefinedIdentToTypeDesc(Identifier: PChar): TExpressionTypeDesc;
  public
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
    procedure ActivateGlobalWriteLock; override;
    function ConsistencyCheck: integer; override;
  end;

const
  fdfAllClassVisibilities = [fdfClassPublished,fdfClassPublic,fdfClassProtected,
                             fdfClassPrivate];
  fdfGlobals = [fdfExceptionOnNotFound, fdfIgnoreUsedUnits];
  fdfGlobalsSameIdent = fdfGlobals+[fdfIgnoreMissingParams,fdfFirstIdentFound,
                fdfOnlyCompatibleProc,fdfSearchInAncestors,fdfCollect]
                +fdfAllClassVisibilities;
  fdfDefaultForExpressions = [fdfSearchInParentNodes,fdfSearchInAncestors,
                              fdfExceptionOnNotFound]+fdfAllClassVisibilities;

  FindDeclarationFlagNames: array[TFindDeclarationFlag] of string = (
    'fdfSearchInParentNodes',
    'fdfSearchInAncestors',
    'fdfIgnoreCurContextNode',
    'fdfExceptionOnNotFound',
    'fdfIgnoreUsedUnits',
    'fdfSearchForward',
    'fdfIgnoreClassVisibility',
    'fdfClassPublished','fdfClassPublic','fdfClassProtected','fdfClassPrivate',
    'fdfIgnoreMissingParams',
    'fdfFirstIdentFound',
    'fdfOnlyCompatibleProc',
    'fdfNoExceptionOnStringChar',
    'fdfFunctionResult',
    'fdfCollect'
  );

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
  SearchAlsoInCurContext: boolean;
  SearchInAncestors: boolean;
begin
  Result:=false;
  ActivateGlobalWriteLock;
  try
    // build code tree
{$IFDEF CTDEBUG}
writeln(DebugPrefix,'TFindDeclarationTool.FindDeclaration A CursorPos=',CursorPos.X,',',CursorPos.Y);
{$ENDIF}
    BuildTreeAndGetCleanPos(false,CursorPos,CleanCursorPos);
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
      SearchAlsoInCurContext:=true;
      SearchInAncestors:=true;
      // first test if in a class
      ClassNode:=CursorNode;
      while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
        ClassNode:=ClassNode.Parent;
      if ClassNode<>nil then begin
        // cursor is in class/object definition
        if (ClassNode.SubDesc and ctnsForwardDeclaration)=0 then begin
          // parse class and build CodeTreeNodes for all properties/methods
          BuildSubTreeForClass(ClassNode);
          CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
          if (CursorNode.Desc=ctnClass)
          and (CleanCursorPos<ClassNode.FirstChild.StartPos) then begin
            // identifier is an ancestor/interface identifier
            SearchAlsoInCurContext:=false;
            SearchInAncestors:=false;
          end;
        end;
      end;
      if CursorNode.Desc=ctnBeginBlock then begin
        BuildSubTreeForBeginBlock(CursorNode);
        CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
      end;
      if CursorNode.Desc=ctnProcedureHead then
        CursorNode:=CursorNode.Parent;
      if CursorNode.Desc=ctnProcedure then begin
        BuildSubTreeForProcHead(CursorNode);
        CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
      end;
      MoveCursorToCleanPos(CleanCursorPos);
      while (CurPos.StartPos>1) and (IsIdentChar[Src[CurPos.StartPos-1]]) do
        dec(CurPos.StartPos);
      if (CurPos.StartPos>=1) and (IsIdentStartChar[Src[CurPos.StartPos]]) then
      begin
        CurPos.EndPos:=CurPos.StartPos;
        while (CurPos.EndPos<=SrcLen) and IsIdentChar[Src[CurPos.EndPos]] do
          inc(CurPos.EndPos);
        // find declaration of identifier
        Params:=TFindDeclarationParams.Create;
        try
          Params.ContextNode:=CursorNode;
          Params.SetIdentifier(Self,@Src[CurPos.StartPos],@CheckSrcIdentifier);
          Params.Flags:=[fdfSearchInParentNodes,
                         fdfExceptionOnNotFound];
          if not SearchAlsoInCurContext then
            Include(Params.Flags,fdfIgnoreCurContextNode);
          if SearchInAncestors then
            Params.Flags:=Params.Flags
              +[fdfSearchInAncestors]+fdfAllClassVisibilities;
          Result:=FindDeclarationOfIdentifier(Params);
          if Result then begin
            Params.ConvertResultCleanPosToCaretPos;
            NewPos:=Params.NewPos;
            NewTopLine:=Params.NewTopLine;
          end;
        finally
          Params.Free;
        end;
      end else begin
        // find declaration of not identifier, e.g. numeric label

      end;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TFindDeclarationTool.FindDeclarationInUsesSection(
  UsesNode: TCodeTreeNode; CleanPos: integer;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var UnitName, UnitInFilename: string;
  UnitNamePos, UnitInFilePos: TAtomPosition;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclarationInUsesSection A');
{$ENDIF}
  // reparse uses section
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom;
  if not UpAtomIs('USES') then
    RaiseException('expected uses, but '+GetAtom+' found');
  repeat
    ReadNextAtom;  // read name
    if CurPos.StartPos>CleanPos then break;
    if AtomIsChar(';') then break;
    AtomIsIdentifier(true);
    UnitNamePos:=CurPos;
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseException(
          'string constant expected, but '+GetAtom+' found');
      UnitInFilePos:=CurPos;
      ReadNextAtom;
    end else
      UnitInFilePos.StartPos:=-1;
    if CleanPos<UnitNamePos.EndPos then begin
      // cursor is on a unitname -> try to locate it
      UnitName:=copy(Src,UnitNamePos.StartPos,
                     UnitNamePos.EndPos-UnitNamePos.StartPos);
      if UnitInFilePos.StartPos>=1 then
        UnitInFilename:=copy(Src,UnitInFilePos.StartPos,
                     UnitInFilePos.EndPos-UnitInFilePos.StartPos)
      else
        UnitInFilename:='';
      NewPos.Code:=FindUnitSource(UnitName,UnitInFilename);
      if NewPos.Code=nil then
        RaiseException('unit not found: '+UnitName);
      NewPos.X:=1;
      NewPos.Y:=1;
      NewTopLine:=1;
      Result:=true;
      exit;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then
      RaiseException('; expected, but '+GetAtom+' found')
  until (CurPos.StartPos>SrcLen);
{$IFDEF CTDEBUG}
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
    NewCode:=TCodeBuffer(Scanner.OnLoadSource(Self,ExpandFilename(AFilename)));
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
        if AnsiCompareText(TheUnitName,
                     copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart))=0
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
{$IFDEF CTDEBUG}
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
  if AnUnitInFilename<>'' then begin
    // unitname in 'filename'
    if FilenameIsAbsolute(AnUnitInFilename) then begin
      Result:=TCodeBuffer(Scanner.OnLoadSource(Self,AnUnitInFilename));
    end else begin
      // search AnUnitInFilename in searchpath
      Result:=SearchFileInPath(UnitSrcSearchPath,AnUnitInFilename);
    end;
  end else begin
    // normal unit name -> search as the compiler would search
    // first search in current directory (= where the maincode is)
    MainCodeIsVirtual:=TCodeBuffer(Scanner.MainCode).IsVirtual;
    if not MainCodeIsVirtual then begin
      CurDir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename);
    end else begin
      CurDir:='';
    end;
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

function TFindDeclarationTool.FindDeclarationOfIdentifier(
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
var OldContextNode: TCodeTreeNode;
  NewContext: TFindContext;
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindDeclarationOfIdentifier] Identifier=',
  '"',GetIdentifier(Params.Identifier),'"',
  ' ContextNode=',NodeDescriptionAsString(Params.ContextNode.Desc));
{$ENDIF}
  Result:=false;
  MoveCursorToCleanPos(Params.Identifier);
  OldContextNode:=Params.ContextNode;
  NewContext:=FindContextNodeAtCursor(Params);
  Params.Flags:=fdfAllClassVisibilities
                +((fdfGlobalsSameIdent+[fdfIgnoreCurContextNode])*Params.Flags);
  if NewContext.Node=OldContextNode then begin
    Params.Flags:=Params.Flags+[fdfSearchInParentNodes,fdfIgnoreCurContextNode];
  end;
  if NewContext.Tool<>Self then begin
    // search in used unit
    Exclude(Params.Flags,fdfClassPrivate);
    if NewContext.Node.Desc=ctnClass then begin
      // ToDo: if context node is not the class of the method the
      //       search started, remove fdfClassProtected from Flags
      
    end;
  end;
  if (OldContextNode.Desc=ctnTypeDefinition)
  and (OldContextNode.FirstChild<>nil)
  and (OldContextNode.FirstChild.Desc=ctnClass)
  and ((OldContextNode.FirstChild.SubDesc and ctnsForwardDeclaration)>0)
  then
    Include(Params.Flags,fdfSearchForward);

  Params.ContextNode:=NewContext.Node;

  Result:=NewContext.Tool.FindIdentifierInContext(Params);
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
  ContextNode, ANode: TCodeTreeNode;
  IsForward, OldFlag: boolean;
  IdentifierFoundResult: TIdentifierFoundResult;
  LastNodeCache: TCodeTreeNodeCache;
  LastCacheEntry: PCodeTreeNodeCacheEntry;
  SearchRangeFlags: TNodeCacheEntryFlags;
  NodeCacheEntryFlags: TNodeCacheEntryFlags;

  function FindInNodeCache: boolean;
  var
    NodeCache: TCodeTreeNodeCache;
  begin
    Result:=false;
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
    and (LastCacheEntry^.CleanStartPos<ContextNode.EndPos)
    and (LastCacheEntry^.CleanEndPos>ContextNode.StartPos)
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
              ' Self=',MainFilename);
      if (Params.NewNode<>nil) then
        writeln('   NewTool=',Params.NewCodeTool.MainFilename,
                ' NewNode=',Params.NewNode.DescAsString)
      else
        writeln('   cache says: identifier NOT FOUND');
      if CompareSrcIdentifiers(Params.Identifier,'TDefineAction') then begin
        NodeCache.WriteDebugReport('NANUNANA: ');
      end;
      {$ENDIF}
      Result:=true;
    end;
  end;
  
  procedure SetResultBeforeExit(NewResult: boolean);
  begin
    FindIdentifierInContext:=NewResult;
    if not NewResult and (fdfExceptionOnNotFound in Params.Flags) then begin
      if Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then
        Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
      Params.IdentifierTool.RaiseException('Identifier not found '
        +'"'+GetIdentifier(Params.Identifier)+'"');
    end;
  end;
  
begin
  Result:=false;
  ContextNode:=Params.ContextNode;
  if ContextNode=nil then begin
    RaiseException('[TFindDeclarationTool.FindIdentifierInContext] '
      +' internal error: Params.ContextNode=nil');
  end;
  StartContextNode:=ContextNode;
  if (fdfFirstIdentFound in Params.Flags)
  and (not (fdfSearchInParentNodes in Params.Flags)) then begin
    // this is a find next call
    // -> adjust StartContextNode, so that siblings, that are not yet searched
    //    are searched
    while (StartContextNode.Parent<>nil)
    and (StartContextNode.Parent.Desc in (AllClassSections+[ctnClass])) do
      StartContextNode:=StartContextNode.Parent;
  end;
  FirstSearchedNode:=nil;
  LastSearchedNode:=nil;
  SearchRangeFlags:=[];
  if fdfSearchInParentNodes in Params.Flags then
    Include(SearchRangeFlags,ncefSearchedInParents);
  if fdfSearchInAncestors in Params.Flags then
    Include(SearchRangeFlags,ncefSearchedInAncestors);
  try
    LastNodeCache:=nil;
    LastCacheEntry:=nil;
    NodeCacheEntryFlags:=[];
    if fdfSearchInParentNodes in Params.Flags then
      Include(NodeCacheEntryFlags,ncefSearchedInParents);
    if fdfSearchInAncestors in Params.Flags then
      Include(NodeCacheEntryFlags,ncefSearchedInAncestors);
{$IFDEF ShowTriedContexts}
writeln('[TFindDeclarationTool.FindIdentifierInContext] A Ident=',
'"',GetIdentifier(Params.Identifier),'"',
' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
);
{$ENDIF}
    repeat
{$IFDEF ShowTriedIdentifiers}
writeln('[TFindDeclarationTool.FindIdentifierInContext] A Ident=',
'"',GetIdentifier(Params.Identifier),'"',
' Context="',ContextNode.DescAsString,'" "',copy(Src,ContextNode.StartPos,20),'"',
' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
);
{$ENDIF}
      // search identifier in current context
      LastContextNode:=ContextNode;
      if not (fdfIgnoreCurContextNode in Params.Flags) then begin
        // search in cache
        if FindInNodeCache then begin
          SetResultBeforeExit(Params.NewNode<>nil);
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
          begin
            // these nodes build a parent-child relationship. But in pascal
            // they just define a range and not a context.
            // -> search in all childs
            if ContextNode.Desc=ctnClass then begin
              // just-in-time parsing for class node
              BuildSubTreeForClass(ContextNode);
            end;
            if (ContextNode.LastChild<>nil) then begin
              if not (fdfSearchForward in Params.Flags) then
                ContextNode:=ContextNode.LastChild
              else
                ContextNode:=ContextNode.FirstChild;
              if not (fdfIgnoreClassVisibility in Params.Flags) then begin
                repeat
                  case ContextNode.Desc of
                  ctnClassPublic:
                    if fdfClassPublic in Params.Flags then break;
                  ctnClassPublished:
                    if fdfClassPublished in Params.Flags then break;
                  ctnClassPrivate:
                    if fdfClassPrivate in Params.Flags then break;
                  ctnClassProtected:
                    if fdfClassProtected in Params.Flags then break;
                  else
                    break;
                  end;
                  // this context node is not visible -> search next
                  ANode:=ContextNode.Parent;
                  if not (fdfSearchForward in Params.Flags) then begin
                    ContextNode:=ContextNode.PriorBrother
                  end else begin
                    ContextNode:=ContextNode.NextBrother
                  end;
                  if ContextNode=nil then begin
                    ContextNode:=ANode;
                    break;
                  end;
                until false;
              end;
            end;
          end;
          
        ctnTypeDefinition, ctnVarDefinition, ctnConstDefinition:
          begin
            if not (fdfCollect in Params.Flags) then begin
              if CompareSrcIdentifiers(ContextNode.StartPos,Params.Identifier)
              then begin
{$IFDEF ShowTriedIdentifiers}
writeln('  Definition Identifier found="',GetIdentifier(Params.Identifier),'"');
{$ENDIF}
                // identifier found
                Result:=true;
                Params.SetResult(Self,ContextNode);
                exit;
              end;
            end else begin
              IdentifierFoundResult:=DoOnIdentifierFound(Params,ContextNode);
              if IdentifierFoundResult in [ifrAbortSearch,ifrSuccess] then begin
                SetResultBeforeExit(IdentifierFoundResult=ifrSuccess);
                exit;
              end;
            end;
            // search for enums
            Params.ContextNode:=ContextNode;
            Result:=FindEnumInContext(Params);
            if Result then exit;
          end;

        ctnProcedure:
          begin
            IdentifierFoundResult:=
              FindIdentifierInProcContext(ContextNode,Params);
            if IdentifierFoundResult in [ifrAbortSearch,ifrSuccess] then begin
              SetResultBeforeExit(IdentifierFoundResult=ifrSuccess);
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
          begin
            if not (fdfCollect in Params.Flags) then begin
              MoveCursorToNodeStart(ContextNode);
              ReadNextAtom; // read keyword
              ReadNextAtom; // read name
              if CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then
              begin
                // identifier found
{$IFDEF ShowTriedIdentifiers}
writeln('  Source Name Identifier found="',GetIdentifier(Params.Identifier),'"');
{$ENDIF}
                Result:=true;
                Params.SetResult(Self,ContextNode,CurPos.StartPos);
                exit;
              end;
            end else begin
              IdentifierFoundResult:=DoOnIdentifierFound(Params,ContextNode);
              if IdentifierFoundResult in [ifrAbortSearch,ifrSuccess] then begin
                SetResultBeforeExit(IdentifierFoundResult=ifrSuccess);
                exit;
              end;
            end;
            Result:=FindIdentifierInHiddenUsedUnits(Params);
            if Result then exit;
          end;

        ctnProperty:
          begin
            if not (fdfCollect in Params.Flags) then begin
              if (Params.Identifier[0]<>'[') then begin
                MoveCursorToNodeStart(ContextNode);
                ReadNextAtom; // read keyword 'property'
                ReadNextAtom; // read name
                if CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then
                begin
                  // identifier found

                  // ToDo: identifiers after 'read', 'write' are procs with
                  //       special parameter lists

{$IFDEF ShowTriedIdentifiers}
writeln('  Property Identifier found="',GetIdentifier(Params.Identifier),'"');
{$ENDIF}
                  Result:=true;
                  Params.SetResult(Self,ContextNode,CurPos.StartPos);
                  exit;
                end;
              end else begin
                // the default property is searched
                Result:=PropertyIsDefault(ContextNode);
                if Result then exit;
              end;
            end else begin
              IdentifierFoundResult:=DoOnIdentifierFound(Params,ContextNode);
              if IdentifierFoundResult in [ifrAbortSearch,ifrSuccess] then begin
                SetResultBeforeExit(IdentifierFoundResult=ifrSuccess);
                exit;
              end;
            end;
          end;

        ctnUsesSection:
          begin
            Result:=FindIdentifierInUsesSection(ContextNode,Params);
            if Result then exit;
          end;

        ctnWithVariable:
          begin
            Result:=FindIdentifierInWithVarContext(ContextNode,Params);
            if Result then exit;
          end;

        ctnPointerType:
          begin
            // pointer types can be forward definitions
            Params.ContextNode:=ContextNode.Parent;
            SetResultBeforeExit(FindForwardIdentifier(Params,IsForward));
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
              break;
            end;
          end;
        end;

        repeat
          // search for prior node
{$IFDEF ShowTriedIdentifiers}
//writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching prior node of ',ContextNode.DescAsString);
{$ENDIF}
          LastSearchedNode:=ContextNode;

          if (ContextNode.Desc=ctnClass) then begin
            if (fdfSearchInAncestors in Params.Flags) then begin
          
              // ToDo: check for circles in ancestors
              OldFlag:=fdfExceptionOnNotFound in Params.Flags;
              Exclude(Params.Flags,fdfExceptionOnNotFound);
              Result:=FindIdentifierInAncestors(ContextNode,Params);
              if OldFlag then Include(Params.Flags,fdfExceptionOnNotFound);
              if Result then exit;
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
            else
              ContextNode:=ContextNode.NextBrother;
{$IFDEF ShowTriedIdentifiers}
writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching in PriorBrother  ContextNode=',ContextNode.DescAsString);
{$ENDIF}
            // it is not always allowed to search in every node on the same lvl:

            // -> test if class visibility valid
            case ContextNode.Desc of
            ctnClassPublished:if (fdfClassPublished in Params.Flags) then break;
            ctnClassPublic:   if (fdfClassPublic    in Params.Flags) then break;
            ctnClassProtected:if (fdfClassProtected in Params.Flags) then break;
            ctnClassPrivate:  if (fdfClassPrivate   in Params.Flags) then break;
            else
              break;
            end;
          end else if (ContextNode.Parent<>nil)
          and ((fdfSearchInParentNodes in Params.Flags)
            or (ContextNode.HasAsParent(StartContextNode))) then
          begin
            // search next in parent
            ContextNode:=ContextNode.Parent;
{$IFDEF ShowTriedIdentifiers}
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
              
            ctnProcedure:
              begin
                Result:=FindIdentifierInClassOfMethod(ContextNode,Params);
                if Result then exit;
              end;
              
            else
              break;
            end;
          end else begin
            ContextNode:=nil;
            break;
          end;
        until false;
      end;
    until ContextNode=nil;
    
  finally
    if Result and (not (fdfDoNotCache in Params.NewFlags))
    and (FirstSearchedNode<>nil) then begin
      // cache result
      AddResultToNodeCaches(Params.Identifier,FirstSearchedNode,ContextNode,
        fdfSearchForward in Params.Flags,Params,SearchRangeFlags);
    end;
  end;
  // if we are here, the identifier was not found
  if FirstSearchedNode<>nil then begin
    // add result to cache
    AddResultToNodeCaches(Params.Identifier,FirstSearchedNode,LastSearchedNode,
      fdfSearchForward in Params.Flags,nil,SearchRangeFlags);
  end;

  SetResultBeforeExit(false);
end;

function TFindDeclarationTool.FindEnumInContext(
  Params: TFindDeclarationParams): boolean;
{ search all subnodes for ctnEnumIdentifier

  Params:
    Identifier
    ContextNode  // = DeepestNode at Cursor

  Result:
    true, if NewPos+NewTopLine valid
 }
var OldContextNode: TCodeTreeNode;
begin
  Result:=false;
  if Params.ContextNode=nil then exit;
  OldContextNode:=Params.ContextNode;
  try
    if Params.ContextNode.Desc=ctnClass then
      BuildSubTreeForClass(Params.ContextNode);
    Params.ContextNode:=Params.ContextNode.FirstChild;
    while Params.ContextNode<>nil do begin
      if (Params.ContextNode.Desc in [ctnEnumIdentifier])
      and CompareSrcIdentifiers(Params.ContextNode.StartPos,Params.Identifier)
      then begin
        // identifier found
        Result:=true;
        Params.SetResult(Self,Params.ContextNode);
        exit;
      end;
      Result:=FindEnumInContext(Params);
      if Result then exit;
      Params.ContextNode:=Params.ContextNode.NextBrother;
    end;
  finally
    Params.ContextNode:=OldContextNode;
  end;
end;

function TFindDeclarationTool.FindContextNodeAtCursor(
  Params: TFindDeclarationParams): TFindContext;
{ searches for the context node for a specific cursor pos
  Params.Context should contain the deepest node at cursor
  if there is no special context, then result is equal to Params.Context
  

  Examples:
  
  1. A.B     - CleanPos points to B: if A is a class, the context node will be
               the class node (ctnRecordType).
  2. A().B   - same as above
          
  3. inherited A - CleanPos points to A: if in a method, the context node will
                   be the class node (ctnClass) of the current method.
          
  4. A[].    - CleanPos points to '.': if A is an array, the context node will
               be the array type node (ctnArrayType).
  
  5. A[].B   - CleanPos points to B: if A is an array of record, the context
               node will be the record type node (ctnRecordType).

  6. A^.     - CleanPos points to '.': if A is a pointer of record, the context
               node will be the record type node (ctnRecordType).
               
  7. (A).    - CleanPos points to '.': if A is a class, the context node will be
               the class node (ctnClass).
               
  8. (A as B) - CleanPos points to ')': if B is a classtype, the context node
                will be the class node (ctnClass)

  9. (@A)    - CleanPos points to ')': if A is a function, not the result is
               returned, but the function itself
}
type
  TAtomType = (atNone, atSpace, atIdentifier, atPreDefIdentifier, atPoint, atAS,
               atINHERITED, atUp, atRoundBracketOpen, atRoundBracketClose,
               atEdgedBracketOpen, atEdgedBracketClose,
               atRead, atWrite);
const
  AtomTypeNames: array[TAtomType] of string =
    ('<None>','Space','Ident','PreDefIdent','Point','AS','INHERITED','Up^',
     'Bracket(','Bracket)','Bracket[','Bracket]','READ','WRITE');

  function GetCurrentAtomType: TAtomType;
  begin
    if (CurPos.StartPos=CurPos.EndPos) then
      Result:=atSpace
    else if UpAtomIs('READ') then
      Result:=atRead
    else if UpAtomIs('WRITE') then
      Result:=atWrite
    else if WordIsPredefinedIdentifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then
      Result:=atPreDefIdentifier
    else if AtomIsIdentifier(false) then
      Result:=atIdentifier
    else if (CurPos.StartPos>=1) and (CurPos.StartPos<=SrcLen)
    and (CurPos.StartPos=CurPos.EndPos-1) then begin
      case Src[CurPos.StartPos] of
      '.': Result:=atPoint;
      '^': Result:=atUp;
      '(': Result:=atRoundBracketOpen;
      ')': Result:=atRoundBracketClose;
      '[': Result:=atEdgedBracketOpen;
      ']': Result:=atEdgedBracketClose;
      else Result:=atNone;
      end;
    end
    else if UpAtomIs('INHERITED') then
      Result:=atINHERITED
    else if UpAtomIs('AS') then
      Result:=atAS
    else
      Result:=atNone;
  end;


var CurAtom, NextAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  NextAtomType, CurAtomType: TAtomType;
  ProcNode, FuncResultNode: TCodeTreeNode;
  ExprType: TExpressionType;
begin
  // start parsing the expression from right to left
  NextAtom:=CurPos;
  NextAtomType:=GetCurrentAtomType;
  ReadPriorAtom;
  CurAtom:=CurPos;
  CurAtomType:=GetCurrentAtomType;
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindContextNodeAtCursor] A ',
  ' Context=',Params.ContextNode.DescAsString,
  ' CurAtom=',AtomTypeNames[CurAtomType],
  ' "',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"',
  ' NextAtom=',AtomTypeNames[NextAtomType]
  );
writeln('');
{$ENDIF}
  if CurAtom.StartPos<Params.ContextNode.StartPos then begin
    // this is the start of the variable
    Result:=CreateFindContext(Self,Params.ContextNode);
    exit;
  end;
  if not (CurAtomType in [atIdentifier,atPreDefIdentifier,atPoint,atUp,atAs,
    atEdgedBracketClose,atRoundBracketClose,atRead,atWrite,atINHERITED])
  then begin
    // no special context found -> the context node is the deepest node at
    // cursor, and this should already be in Params.ContextNode
    if (not (NextAtomType in [atSpace,atIdentifier,atPreDefIdentifier,
      atRoundBracketOpen,atEdgedBracketOpen])) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseException('identifier expected, but '
                      +GetAtom+' found');
    end;
    Result:=CreateFindContext(Self,Params.ContextNode);
    exit;
  end;
  if (CurAtomType in [atRoundBracketClose,atEdgedBracketClose]) then begin
    ReadBackTilBracketClose(true);
    CurAtom.StartPos:=CurPos.StartPos;
  end;
  if (CurAtomType in [atAS,atRead,atWrite,atINHERITED,atNone])
  or ((CurAtomType in [atIdentifier,atPreDefIdentifier])
      and (NextAtomType in [atIdentifier,atPreDefIdentifier]))
  then begin
    // this is the start of the variable
    Result:=CreateFindContext(Self,Params.ContextNode)
  end else begin
    // find prior context
    Result:=FindContextNodeAtCursor(Params);
  end;
  if (Result.Node=nil) then exit;
  
  // the left side has been parsed and
  // now the parsing goes from left to right
  
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindContextNodeAtCursor] B ',
  ' Context=',Params.ContextNode.DescAsString,
  ' CurAtom=',AtomTypeNames[CurAtomType],
  ' "',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"',
  ' NextAtom=',AtomTypeNames[NextAtomType],
  ' Result=');
if Result.Node<>nil then write(Result.Node.DescAsString) else write('NIL');
writeln('');
{$ENDIF}

  case CurAtomType of

  atIdentifier, atPreDefIdentifier:
    begin
      // for example  'AnObject[3]'
      if not (NextAtomType in [atSpace,atPoint,atUp,atAS,atRoundBracketOpen,
        atRoundBracketClose,atEdgedBracketOpen,atEdgedBracketClose]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier "'+GetAtom+'" found');
      end;
      if (Result.Node=Params.ContextNode) then begin
        if CompareSrcIdentifier(CurAtom.StartPos,'SELF') then begin
          // SELF in a method is the object itself
          // -> check if in a proc
          ProcNode:=Params.ContextNode;
          while (ProcNode<>nil) do begin
            if (ProcNode.Desc=ctnProcedure) then begin
              // in a proc -> find the class context
              if Result.Tool.FindClassOfMethod(ProcNode,Params,true) then begin
                Result:=CreateFindContext(Params);
                exit;
              end;
            end;
            ProcNode:=ProcNode.Parent;
          end;
        end else if CompareSrcIdentifier(CurAtom.StartPos,'RESULT') then begin
          // RESULT has a special meaning in a function
          // -> check if in a function
          ProcNode:=Params.ContextNode;
          while (ProcNode<>nil) do begin
            if (ProcNode.Desc=ctnProcedure) then begin
              Params.Save(OldInput);
              Include(Params.Flags,fdfFunctionResult);
              Result:=Result.Tool.FindBaseTypeOfNode(Params,ProcNode);
              Params.Load(OldInput);
              exit;
            end;
            ProcNode:=ProcNode.Parent;
          end;
        end;
      end;
      { ToDo: check, if this is needed for Delphi:

      if (NextAtomType in [atSpace])
      and CompareSrcIdentifier(CurAtom.StartPos,'FREE')
      and ((Result.Node.Desc=ctnClass) or NodeIsInAMethod(Result.Node)) then
      begin
        // FREE calls the destructor of an object
        Params.Save(OldInput);
        Params.SetIdentifier(Self,'DESTRUCTOR',nil);
        Exclude(Params.Flags,fdfExceptionOnNotFound);
        if Result.Tool.FindIdentifierInContext(Params) then begin
          Result:=CreateFindContext(Params);
          exit;
        end;
        Params.Load(OldInput);
      end;}
      // find sub identifier
      Params.Save(OldInput);
      try
        Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound]
                      +fdfAllClassVisibilities
                      +(fdfGlobals*Params.Flags);
        if CurAtomType=atPreDefIdentifier then
          Exclude(Params.Flags,fdfExceptionOnNotFound);
        if Result.Node=Params.ContextNode then begin
          // there is no special context -> also search in parent contexts
          Params.Flags:=Params.Flags
                       +[fdfSearchInParentNodes,fdfIgnoreCurContextNode];
        end else
          // special context
          Params.ContextNode:=Result.Node;
        Params.SetIdentifier(Self,@Src[CurAtom.StartPos],@CheckSrcIdentifier);
        if Result.Tool.FindIdentifierInContext(Params) then begin
          Result:=CreateFindContext(Params);
        end else begin
          // predefined identifier not redefined
          Result:=CreateFindContext(Self,nil);
        end;
        
        // ToDo: check if identifier in 'Protected' section
        
      finally
        Params.Load(OldInput);
      end;
      if Result.Node<>nil then begin
        if (Result.Node<>nil)
        and (Result.Node.Desc in [ctnProcedure,ctnProcedureHead]) then begin
          Result.Tool.BuildSubTreeForProcHead(Result.Node,FuncResultNode);
          if FuncResultNode<>nil then begin
            // this is function
            if (NextAtomType in [atSpace,atRoundBracketClose]) then begin
              // In Delphi Mode or if there is a @ qualifier return the
              // function and not the result type
              
              // ToDo:

            end;
            // Otherwise return the result type
            Include(Params.Flags,fdfFunctionResult);
          end;
        end;
        Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node);
      end;
    end;
    
  atPoint:
    begin
      // for example 'A.B'
      if Result.Node=Params.ContextNode then begin
        MoveCursorToCleanPos(CurAtom.StartPos);
        RaiseException('identifier expected, but . found');
      end;
      if (not (NextAtomType in [atSpace,atIdentifier,atPreDefIdentifier])) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
      if (Result.Node.Desc in AllUsableSourceTypes) then begin
        // identifier in front of the point is a unit name
        if Result.Tool<>Self then begin
          Result.Node:=Result.Tool.GetInterfaceNode;
        end else begin
          Result:=CreateFindContext(Self,Params.ContextNode);
        end;
      end;
      // there is no special left to do, since Result already points to
      // the type context node.
    end;

  atAS:
    begin
      // for example 'A as B'
      if (not (NextAtomType in [atSpace,atIdentifier,atPreDefIdentifier])) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
      // 'as' is a type cast, so the left side is irrelevant and was already
      // ignored in the code at the start of this proc
      // -> context is default context
    end;

  atUP:
    begin
      // for example:
      //   1. 'PInt = ^integer'  pointer type
      //   2. a^  dereferencing
      if not (NextAtomType in [atSpace,atPoint,atUp,atAS,atEdgedBracketOpen])
      then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier "'+GetAtom+'" found');
      end;
      if Result.Node<>Params.ContextNode then begin
        // left side of expression has defined a special context
        // => this '^' is a dereference
        if (not (NextAtomType in [atSpace,atPoint,atAS,atUP,atEdgedBracketOpen]))
        then begin
          MoveCursorToCleanPos(NextAtom.StartPos);
          ReadNextAtom;
          RaiseException('. expected, but '+GetAtom+' found');
        end;
        if Result.Node.Desc<>ctnPointerType then begin
          MoveCursorToCleanPos(CurAtom.StartPos);
          RaiseException('illegal qualifier ^');
        end;
        Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node.FirstChild);
      end else if NodeHasParentOfType(Result.Node,ctnPointerType) then begin
      //end else if Result.Node.Parent.Desc=ctnPointerType then begin
        // this is a pointer type definition
        // -> the default context is ok
      end;
    end;

  atEdgedBracketClose:
    begin
      { for example:  a[]
          this could be:
            1. ranged array
            2. dynamic array
            3. indexed pointer
            4. default property
            5. string character
      }
      if not (NextAtomType in [atSpace,atPoint,atAs,atUp,atRoundBracketClose,
        atRoundBracketOpen,atEdgedBracketClose,atEdgedBracketOpen]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier');
      end;
      if Result.Node<>Params.ContextNode then begin
        case Result.Node.Desc of
        
        ctnArrayType:
          // the array type is the last child node
          Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node.LastChild);

        ctnPointerType:
          // the pointer type is the only child node
          Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node.FirstChild);

        ctnClass:
          begin
            // search default property in class
            Params.Save(OldInput);
            Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound]
                          +fdfGlobals*Params.Flags
                          +fdfAllClassVisibilities*Params.Flags;
            // special identifier for default property
            Params.SetIdentifier(Self,'[',nil);
            Params.ContextNode:=Result.Node;
            Result.Tool.FindIdentifierInContext(Params);
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(
                                                         Params,Params.NewNode);
            Params.Load(OldInput);
          end;
          
        ctnIdentifier:
          begin
            MoveCursorToNodeStart(Result.Node);
            ReadNextAtom;
            if UpAtomIs('STRING') or UpAtomIs('ANSISTRING')
            or UpAtomIs('SHORTSTRING') then begin
              if not (fdfNoExceptionOnStringChar in Params.Flags) then begin
                MoveCursorToCleanPos(CurAtom.StartPos);
                ReadNextAtom;
                RaiseException('illegal qualifier');
              end;
            end;
          end;
          
        else
          MoveCursorToCleanPos(CurAtom.StartPos);
          ReadNextAtom;
          RaiseException('illegal qualifier');
        end;
      end;
    end;

  atRoundBracketClose:
    begin
      { for example:
          (a+b)   expression bracket: the type is the result type of the
                                      expression.
          a()     typecast or function
      }
      if not (NextAtomType in [atSpace,atPoint,atAs,atUp,atRoundBracketClose,
        atRoundBracketOpen,atEdgedBracketClose,atEdgedBracketOpen]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier');
      end;
      if Result.Node<>Params.ContextNode then begin
        // typecast or function
      end else begin
        // expression
        ExprType:=FindExpressionResultType(Params,CurAtom.StartPos+1,
                                         CurAtom.EndPos-1);
        if (ExprType.Context.Node=nil) then begin
          MoveCursorToCleanPos(CurAtom.StartPos);
          ReadNextAtom;
          RaiseException('expression type is not a variable');
        end;
      end;
    end;

  atINHERITED:
    begin
      // for example: inherited A;
      if not (NextAtomType in [atSpace,atIdentifier,atPreDefIdentifier]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
      
      // ToDo: 'inherited' keyword
      
      // this is a quick hack: Just ignore the current class and start
      // searching in the ancestor
      
      // find ancestor of class of method
      ProcNode:=Result.Node;
      while (ProcNode<>nil) do begin
        if not (ProcNode.Desc in [ctnProcedure,ctnProcedureHead,ctnBeginBlock,
           ctnAsmBlock,ctnWithVariable,ctnWithStatement,ctnCaseBlock,
           ctnCaseVariable,ctnCaseStatement]) then
        begin
          break;
        end;
        if ProcNode.Desc=ctnProcedure then begin
          Result.Tool.FindClassOfMethod(ProcNode,Params,true);
          // find class ancestor
          Params.NewCodeTool.FindAncestorOfClass(Params.NewNode,Params,true);
          Result:=CreateFindContext(Params);
          exit;
        end;
        ProcNode:=ProcNode.Parent;
      end;
      MoveCursorToCleanPos(CurAtom.StartPos);
      RaiseException('inherited keyword only allowed in methods');
    end;

  else
    // expression start found
    begin
      if (not (NextAtomType in [atSpace,atIdentifier,atPreDefIdentifier,
        atRoundBracketOpen,atEdgedBracketOpen])) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
    end;
  end;
  
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindContextNodeAtCursor] END ',
  Params.ContextNode.DescAsString,' CurAtom=',AtomTypeNames[CurAtomType],
  ' NextAtom=',AtomTypeNames[NextAtomType],' Result=');
if Result.Node<>nil then write(Result.Node.DescAsString) else write('NIL');
writeln('');
{$ENDIF}
end;

function TFindDeclarationTool.FindBaseTypeOfNode(Params: TFindDeclarationParams;
  Node: TCodeTreeNode): TFindContext;
var OldInput: TFindDeclarationInput;
  ClassIdentNode, DummyNode: TCodeTreeNode;
  IsPredefinedIdentifier: boolean;
  NodeStack: TCodeTreeNodeStack;
begin
  Result.Node:=Node;
  Result.Tool:=Self;
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
        Result.Tool.RaiseException('circle in definitions');
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
          RaiseException('[TFindDeclarationTool.FindBaseTypeOfNode] '
                        +'forward class node without name');
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
            RaiseException('Forward class definition not resolved: '
                +copy(Src,ClassIdentNode.StartPos,
                    ClassIdentNode.EndPos-ClassIdentNode.StartPos));
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
          Params.SetIdentifier(Self,@Src[Result.Node.StartPos],
                               @CheckSrcIdentifier);
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                        +(fdfGlobals*Params.Flags)
                        -[fdfIgnoreUsedUnits];
          IsPredefinedIdentifier:=WordIsPredefinedIdentifier.DoIt(
                                     Params.Identifier);
          if IsPredefinedIdentifier then
            Exclude(Params.Flags,fdfExceptionOnNotFound);
          Params.ContextNode:=Result.Node.Parent;
          if Params.ContextNode.Desc=ctnParameterList then
            Params.ContextNode:=Params.ContextNode.Parent;
          if Params.ContextNode.Desc=ctnProcedureHead then
            Params.ContextNode:=Params.ContextNode.Parent;
          if FindIdentifierInContext(Params) then
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode)
          else
            // predefined identifier
            Result:=CreateFindContext(Self,Result.Node);
          exit;
        finally
          Params.Load(OldInput);
        end;
      end else
      if (Result.Node.Desc=ctnProperty) then begin
        // this is a property -> search the type definition of the property
        if ReadTilTypeOfProperty(Result.Node) then begin
          // property has type
          Params.Save(OldInput);
          try
            Params.SetIdentifier(Self,@Src[CurPos.StartPos],nil);
            Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                          +(fdfGlobals*Params.Flags)
                          -[fdfIgnoreUsedUnits];
            Params.ContextNode:=Result.Node.Parent;
            FindIdentifierInContext(Params);
            if Result.Node.HasAsParent(Params.NewNode) then
              break;
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
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
            Params.SetIdentifier(Self,@Src[CurPos.StartPos],nil);
            Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInAncestors]
                         +(fdfGlobalsSameIdent*Params.Flags);
            FindIdentifierInAncestors(Result.Node.Parent.Parent,Params);
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
            exit;
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
          Params.IdentifierTool.RaiseException(
            '[TFindDeclarationTool.FindBaseTypeOfNode]'
           +' internal error: not IsPCharInSrc(Params.Identifier) '
           +' Params.IdentifierTool.='
                   +TCodeBuffer(Params.IdentifierTool.Scanner.MainCode).Filename
           +' Ident="'+GetIdentifier(Params.Identifier)+'"');
        Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
      end;
      RaiseException('base type of "'+GetIdentifier(Params.Identifier)
        +'" not found');
    end;
  finally
    // cache the result in all nodes
    CreateBaseTypeCaches(@NodeStack,Result);
    // free node stack
    FinalizeNodeStack(@NodeStack);
  end;
{$IFDEF CTDEBUG}
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
    // proc is a method body (no declaration).
    // -> proceed the search normally ...
  end else begin
    // proc is a proc declaration
    if not (fdfCollect in Params.Flags) then begin
      if CompareSrcIdentifiers(NameAtom.StartPos,Params.Identifier) then begin
        // proc identifier found
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc-Identifier found="',GetIdentifier(Params.Identifier),'"');
{$ENDIF}
        Params.SetResult(Self,ProcContextNode,NameAtom.StartPos);
        Result:=DoOnIdentifierFound(Params,ProcContextNode);
{$IFDEF CTDEBUG}
if Result=ifrSuccess then
  writeln('[TFindDeclarationTool.FindIdentifierInProcContext] ',
    ' Params.NewCodeTool="',TCodeBuffer(Params.NewCodeTool.Scanner.MainCode).Filename,'"',
    ' Params.NewNode="',Params.NewNode.DescAsString,'"'
    );
{$ENDIF}
      end else begin
        // proceed the search normally ...
      end;
    end else begin
      Result:=DoOnIdentifierFound(Params,ProcContextNode);
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
  IdentifierFoundResult: TIdentifierFoundResult;
begin
  Result:=false;
  // if proc is a method, search in class
  // -> find class name
  MoveCursorToNodeStart(ProcContextNode);
  ReadNextAtom; // read keyword
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
      Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes]
                    +(fdfGlobals*Params.Flags)
                    +[fdfExceptionOnNotFound,fdfIgnoreUsedUnits];
      Params.ContextNode:=ProcContextNode;
      Params.SetIdentifier(Self,@Src[ClassNameAtom.StartPos],nil);
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc="',copy(src,ProcContextNode.StartPos,30),'" searching class of method   class="',ExtractIdentifier(ClassNameAtom.StartPos),'"');
{$ENDIF}
      FindIdentifierInContext(Params);
      ClassContext:=Params.NewCodeTool.FindBaseTypeOfNode(
                                                   Params,Params.NewNode);
      if (ClassContext.Node=nil)
      or (ClassContext.Node.Desc<>ctnClass) then begin
        MoveCursorToCleanPos(ClassNameAtom.StartPos);
        RaiseException('class identifier expected');
      end;
      // class context found
      // 2. -> search identifier in class
      Params.Load(OldInput);
      Params.Flags:=[fdfSearchInAncestors]+fdfAllClassVisibilities
                    +(fdfGlobalsSameIdent*Params.Flags)
                    -[fdfExceptionOnNotFound];
      Params.ContextNode:=ClassContext.Node;
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  searching identifier in class of method');
{$ENDIF}
      Result:=ClassContext.Tool.FindIdentifierInContext(Params);
      if Result then
        // dont reload the Input params, so that a find next is possible
        exit
      else
        Params.Load(OldInput);
    end;
  end else begin
    // proc is not a method
    if not (fdfCollect in Params.Flags) then begin
      if CompareSrcIdentifiers(ClassNameAtom.StartPos,Params.Identifier) then
      begin
        // proc identifier found
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc Identifier found="',GetIdentifier(Params.Identifier),'"');
{$ENDIF}
        Result:=true;
        Params.SetResult(Self,ProcContextNode,ClassNameAtom.StartPos);
        exit;
      end else begin
        // proceed the search normally ...
      end;
    end else begin
      IdentifierFoundResult:=DoOnIdentifierFound(Params,ProcContextNode);
      if IdentifierFoundResult in [ifrAbortSearch,ifrSuccess] then begin
        Result:=(IdentifierFoundResult=ifrSuccess);
        exit;
      end;
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
{$IFDEF CTDEBUG}
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
                    +(fdfGlobals*Params.Flags);
      Params.ContextNode:=ProcNode;
      Params.SetIdentifier(Self,@Src[ClassNameAtom.StartPos],nil);
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindClassOfMethod]  searching class of method   class="',ExtractIdentifier(ClassNameAtom.StartPos),'"');
{$ENDIF}
      FindIdentifierInContext(Params);
      if FindClassContext then begin
        // parse class and return class node
        ClassContext:=FindBaseTypeOfNode(Params,Params.NewNode);
        if (ClassContext.Node=nil)
        or (ClassContext.Node.Desc<>ctnClass) then begin
          MoveCursorToCleanPos(ClassNameAtom.StartPos);
          RaiseException('class identifier expected');
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
{$IFDEF CTDEBUG}
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
                  -[fdfIgnoreUsedUnits];
    if not SearchTObject then
      Params.SetIdentifier(Self,@Src[AncestorAtom.StartPos],nil)
    else begin
      Params.SetIdentifier(Self,'TObject',nil);
      Exclude(Params.Flags,fdfExceptionOnNotFound);
    end;
    Params.ContextNode:=ClassNode;
    if not FindIdentifierInContext(Params) then begin
      MoveCursorToNodeStart(ClassNode);
//writeln('  AQ2*** ',TCodeBuffer(Scanner.MainCode).Filename,' ',CurPos.StartPos);
      RaiseException('default class ancestor TObject not found');
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
  Exclude(Params.Flags,fdfExceptionOnNotFound);
  Result:=FindIdentifierInContext(Params);
  if not Result then begin
    Params.Load(OldInput);
    Include(Params.Flags,fdfSearchForward);
    Result:=FindIdentifierInContext(Params);
    IsForward:=true;
  end else begin
    IsForward:=false;
    // do not reload param input, so that find next is possible
  end;
end;

function TFindDeclarationTool.FindIdentifierInWithVarContext(
  WithVarNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var
  WithVarContext: TFindContext;
  OldInput: TFindDeclarationInput;
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInWithVarContext] ',
'"',GetIdentifier(Params.Identifier),'"'
);
{$ENDIF}
  Result:=false;
  // find the base type of the with variable
  // move cursor to end of with-expression
  if (WithVarNode.FirstChild<>nil) then begin
    // this is the last with-variable
    MoveCursorToCleanPos(WithVarNode.FirstChild.StartPos);
    ReadPriorAtom; // read 'do'
    CurPos.EndPos:=CurPos.StartPos; // make the 'do' unread,
                                    // because 'do' is not part of the expr
  end else begin
    // this is not the last with variable, so the expr end is equal to node end
    MoveCursorToCleanPos(WithVarNode.EndPos);
  end;
  Params.Save(OldInput);
  Params.ContextNode:=WithVarNode;
  Include(Params.Flags,fdfExceptionOnNotFound);
  WithVarContext:=FindContextNodeAtCursor(Params);
  if (WithVarContext.Node=nil) or (WithVarContext.Node=OldInput.ContextNode)
  or (not (WithVarContext.Node.Desc in [ctnClass,ctnRecordType])) then begin
    MoveCursorToCleanPos(WithVarNode.StartPos);
    RaiseException('expression type must be class or record type');
  end;
  // search identifier in with context
  Params.Load(OldInput);
  Exclude(Params.Flags,fdfExceptionOnNotFound);
  Params.ContextNode:=WithVarContext.Node;
  if WithVarContext.Tool.FindIdentifierInContext(Params) then begin
    // identifier found in with context
    Result:=true;
    // do not reload the param input, so that find next is possible
  end else
    Params.Load(OldInput);
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
      RaiseException('class without name');
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
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInAncestors] ',
' Ident="',GetIdentifier(Params.Identifier),'"',
' search ancestor class = ',GetAtom);
writeln('  Flags=[',FindDeclarationFlagsAsString(Params.Flags),']');
{$ENDIF}
  // search ancestor class context
  CurPos.StartPos:=CurPos.EndPos;
  Params.Save(OldInput);
  Params.Flags:=[fdfSearchInParentNodes,fdfIgnoreCurContextNode,
                 fdfExceptionOnNotFound]
                +(fdfGlobals*Params.Flags);
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
    RaiseException('default class ancestor TObject not found');
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
  if not Result then
    Params.Load(OldInput)
  else
    // do not reload param input, so that find next is possible
    ;
end;

{$IFDEF CTDEBUG}
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
  
begin
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.FindExpressionResultType] ',
'"',copy(Src,StartPos,EndPos-StartPos),'"');
{$ENDIF}
  Result:=CleanExpressionType;
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
      RaiseException('binary operator expected, but '+GetAtom+' found');
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
      RaiseException('[TFindDeclarationTool.FindExpressionResultType]'
        +' internal error: unknown precedence lvl for operator '+GetAtom);
    // execute stack if possible
    ExecuteStack;
  until false;
  ExecuteStack;
  Result:=ExprStack[0].Operand;
end;

function TFindDeclarationTool.FindIdentifierInUsesSection(
  UsesNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext

   search backwards through the uses section
   compare first the unit name, then load the unit and search there

}
var InAtom, UnitNameAtom: TAtomPosition;
  NewCodeTool: TFindDeclarationTool;
  OldInput: TFindDeclarationInput;
begin
  Result:=false;
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection) then
    RaiseException('[TFindDeclarationTool.FindIdentifierInUsesSection] '
      +'internal error: invalid UsesNode');
  // search backwards through the uses section
  MoveCursorToCleanPos(UsesNode.EndPos);
  ReadPriorAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseException('; expected, but '+GetAtom+' found');
  repeat
    ReadPriorAtom; // read unitname
    if AtomIsStringConstant then begin
      InAtom:=CurPos;
      ReadPriorAtom; // read 'in'
      if not UpAtomIs('IN') then
        RaiseException('keyword "in" expected, but '+GetAtom+' found');
      ReadPriorAtom; // read unitname
    end else
      InAtom.StartPos:=-1;
    AtomIsIdentifier(true);
    UnitNameAtom:=CurPos;
    if (fdfIgnoreUsedUnits in Params.Flags) then begin
      if CompareSrcIdentifiers(UnitNameAtom.StartPos,Params.Identifier) then
      begin
        // the searched identifier was a uses unitname, but since the unit
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
        RaiseException('unit not found: '+copy(Src,UnitNameAtom.StartPos,
           UnitNameAtom.EndPos-UnitNameAtom.StartPos));
      end else if NewCodeTool=Self then begin
        MoveCursorToCleanPos(UnitNameAtom.StartPos);
        RaiseException('illegal circle using unit: '+copy(Src,
           UnitNameAtom.StartPos,UnitNameAtom.EndPos-UnitNameAtom.StartPos));
      end;
      // search the identifier in the interface of the used unit
      Params.Save(OldInput);
      Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobalsSameIdent*Params.Flags)
                   -[fdfExceptionOnNotFound];
      Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
      if Result then
        // do not reload param input, so that find next is possible
        exit
      else
        Params.Load(OldInput);
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
    AnUnitInFilename:=copy(Src,UnitInFileAtom.StartPos,
                   UnitInFileAtom.EndPos-UnitInFileAtom.StartPos);
  end else
    AnUnitInFilename:='';
  NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename);
  if (NewCode=nil) then begin
    // no source found
    if ExceptionOnNotFound then
      RaiseException('unit '+AnUnitName+' not found');
  end else begin
    // source found -> get codetool for it
{$IFDEF CTDEBUG}
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
{$IFDEF CTDEBUG}
writeln(DebugPrefix,'TFindDeclarationTool.FindIdentifierInInterface',
' Ident="',GetIdentifier(Params.Identifier),'"',
' IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags,
' Self=',TCodeBuffer(Scanner.MainCode).Filename
);
{$ENDIF}

  // ToDo: build codetree for ppu, ppw, dcu files
  
  // build tree for pascal source
  ClearNodeCaches(false);
  BuildTree(true);
  
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
    RaiseException('source is not unit');
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
    RaiseException('interface section not found');
  Params.Save(OldInput);
  Params.Flags:=(fdfGlobalsSameIdent*Params.Flags)
                -[fdfExceptionOnNotFound,fdfSearchInParentNodes];
  Params.ContextNode:=InterfaceNode;
  Result:=FindIdentifierInContext(Params);
  if not Result then
    Params.Load(OldInput)
  else
    // do not reload param input, so that find next is possible
    ;

  // save result in cache
  if FInterfaceIdentifierCache=nil then
    FInterfaceIdentifierCache:=TInterfaceIdentifierCache.Create(Self);
  if Result then begin
    // identifier exists in interface
    if (Params.NewNode.Desc<>ctnProcedure) then begin
      FInterfaceIdentifierCache.Add(OldInput.Identifier,Params.NewNode,
        Params.NewCleanPos);
    end else begin
      // do not save proc identifiers.
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
    RaiseException('used unit is not an pascal unit');
  end;
  Result:=FindInterfaceNode;
  if Result=nil then begin
    CurPos.StartPos:=-1;
    RaiseException('no interface section found');
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
    RaiseException('unit '+AnUnitName+' not found');
  end else begin
    // source found -> get codetool for it
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInUsedUnit] ',
' This source is=',TCodeBuffer(Scanner.MainCode).Filename,
' NewCode=',NewCode.Filename,' IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags);
{$ENDIF}
    if Assigned(FOnGetCodeToolForBuffer) then begin
      NewCodeTool:=FOnGetCodeToolForBuffer(Self,NewCode);
      if NewCodeTool=nil then begin
        CurPos.StartPos:=-1;
        RaiseException('unit '+AnUnitName+' not found');
      end;
    end else if NewCode=TCodeBuffer(Scanner.MainCode) then begin
      NewCodeTool:=Self;
      CurPos.StartPos:=-1;
      RaiseException('illegal circle using unit: '+AnUnitName);
    end;
    // search the identifier in the interface of the used unit
    Params.Save(OldInput);
    Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobalsSameIdent*Params.Flags)
                 -[fdfExceptionOnNotFound];
    Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
    if Result then
      // do not reload param input, so that find next is possible
      exit
    else
      Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.NodeIsInAMethod(Node: TCodeTreeNode): boolean;
begin
  Result:=false;
  while (Node<>nil) do begin
    if (Node.Desc=ctnProcedure) then begin
    
      // ToDo: ppu, ppw, dcu
    
      MoveCursorToNodeStart(Node.FirstChild); // ctnProcedureHead
      ReadNextAtom;
      if not AtomIsIdentifier(false) then continue;
      ReadNextAtom;
      if not AtomIsChar('.') then continue;
      Result:=true;
      exit;
    end;
  end;
end;

procedure TFindDeclarationTool.BeginParsing(DeleteNodes,
  OnlyInterfaceNeeded: boolean);
begin
  inherited BeginParsing(DeleteNodes,OnlyInterfaceNeeded);
  
  case Scanner.PascalCompiler of
  pcDelphi: WordIsPredefinedIdentifier:=WordIsPredefinedDelphiIdentifier;
  else
    WordIsPredefinedIdentifier:=WordIsPredefinedFPCIdentifier;
  end;
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
  SystemUnitName: string;
  SpecialUnitType: integer;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInHiddenUsedUnits] ',
'"',GetIdentifier(Params.Identifier),'" IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags);
{$ENDIF}
  if (Tree.Root<>nil) and (not (fdfIgnoreUsedUnits in Params.Flags)) then begin
    // check, if this is a special unit
    MoveCursorToNodeStart(Tree.Root);
    ReadNextAtom;
    ReadNextAtom;
    if Scanner.InitialValues.IsDefined('LINUX')
    and (Scanner.PascalCompiler<>pcDelphi) then
      SystemUnitName:='SYSLINUX'
    else
      // ToDo: other OS than linux
      SystemUnitName:='SYSTEM';
    if UpAtomIs(SystemUnitName) then
      SpecialUnitType:=sutSystem
    else if UpAtomIs('OBJPAS') then
      SpecialUnitType:=sutObjPas
    else if UpAtomIs('LINEINFO') then
      SpecialUnitType:=sutLineInfo
    else if UpAtomIs('HEAPTRC') then
      SpecialUnitType:=sutHeapTrc
    else
      SpecialUnitType:=sutNone;
    // try hidden units
    if (SpecialUnitType>sutHeapTrc)
    and Scanner.InitialValues.IsDefined(ExternalMacroStart+'UseHeapTrcUnit')
    then begin
      // try hidden used unit 'heaptrc'
      Result:=FindIdentifierInUsedUnit('HeapTrc',Params);
      if Result then exit;
    end;
    if (SpecialUnitType>sutLineInfo)
    and Scanner.InitialValues.IsDefined(ExternalMacroStart+'UseLineInfo')
    then begin
      // try hidden used unit 'lineinfo'
      Result:=FindIdentifierInUsedUnit('LineInfo',Params);
      if Result then exit;
    end;
    if (SpecialUnitType>sutObjPas)
    and (Scanner.CompilerMode in [cmDELPHI,cmOBJFPC])
    and (Scanner.PascalCompiler=pcFPC) then begin
      // try hidden used unit 'objpas'
      Result:=FindIdentifierInUsedUnit('ObjPas',Params);
      if Result then exit;
    end;
    // try hidden used unit 'system'
    if (SpecialUnitType>sutSystem)
    and CompareSrcIdentifiers(Params.Identifier,PChar(SystemUnitName)) then begin
      // the system unit name itself is searched -> rename searched identifier
      Params.Save(OldInput);
      Params.SetIdentifier(Self,PChar(SystemUnitName),nil);
      Result:=FindIdentifierInUsedUnit(SystemUnitName,Params);
      Params.Load(OldInput);
    end else
      Result:=FindIdentifierInUsedUnit(SystemUnitName,Params);
    if Result then exit;
  end;
end;

function TFindDeclarationTool.FindEndOfVariable(
  StartPos: integer): integer;
begin
  MoveCursorToCleanPos(StartPos);
  ReadNextAtom;
  if UpAtomIs('INHERITED') then
    ReadNextAtom;
  repeat
    AtomIsIdentifier(true);
    ReadNextAtom;
    repeat
      if AtomIsChar('(') or AtomIsChar('[') then begin
        ReadTilBracketClose(true);
        ReadNextAtom;
      end else if AtomIsChar('^') then
        ReadNextAtom
      else
        break;
    until false;
    if not AtomIsChar('.') then break;
    ReadNextAtom;
  until false;
  UndoReadNextAtom;
  Result:=CurPos.EndPos;
end;

function TFindDeclarationTool.FindExpressionTypeOfVariable(StartPos: integer;
  Params: TFindDeclarationParams; var EndPos: integer): TExpressionType;
var
  OldInputFlags: TFindDeclarationFlags;
  IsPredefinedIdentifier: boolean;
  CouldBeStringChar: boolean;
begin
  OldInputFlags:=Params.Flags;
  IsPredefinedIdentifier:=WordIsPredefinedIdentifier.DoIt(@Src[StartPos]);
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.FindExpressionTypeOfVariable] ',
' IsPredefinedIdentifier=',IsPredefinedIdentifier);
{$ENDIF}
  if IsPredefinedIdentifier then
    Exclude(Params.Flags,fdfExceptionOnNotFound)
  else
    Include(Params.Flags,fdfExceptionOnNotFound);
  EndPos:=FindEndOfVariable(StartPos);
  CouldBeStringChar:=AtomIsChar(']');
  MoveCursorToCleanPos(EndPos);
  Include(Params.Flags,fdfNoExceptionOnStringChar);
  Result.Context:=FindContextNodeAtCursor(Params);
  Params.Flags:=OldInputFlags;
  if Result.Context.Node<>nil then begin
    if CouldBeStringChar then begin
      CouldBeStringChar:=(Result.Context.Node.Desc=ctnIdentifier);
      if CouldBeStringChar then begin
        MoveCursorToNodeStart(Result.Context.Node);
        ReadNextAtom;
        CouldBeStringChar:=UpAtomIs('STRING') or UpAtomIs('ANSISTRING')
                         or UpAtomIs('SHORTSTRING');
        if CouldBeStringChar then begin
          Result.Context.Node:=nil;
          Result.Desc:=xtChar;
          exit;
        end;
      end;
    end;
    Result:=Result.Context.Tool.ConvertNodeToExpressionType(Result.Context.Node,
                                                          Params);
  end else begin
    if IsPredefinedIdentifier then begin
      Result:=CleanExpressionType;
      Result.Desc:=PredefinedIdentToTypeDesc(@Src[StartPos]);
    end else
      RaiseException('identifier expected, but '+GetAtom+' found');
  end;
end;

function TFindDeclarationTool.ConvertNodeToExpressionType(Node: TCodeTreeNode;
  Params: TFindDeclarationParams): TExpressionType;
var BaseContext: TFindContext;
begin
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
    if WordIsPredefinedIdentifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then
    begin
      // predefined identifiers
      Result.Desc:=PredefinedIdentToExprTypeDesc(@Src[CurPos.StartPos]);
      if Result.Desc=xtString then begin

        // ToDo: ask scanner, if AnsiString or ShortString
        
      end;
    end;
  end;
end;

function TFindDeclarationTool.ReadOperandTypeAtCursor(
  Params: TFindDeclarationParams): TExpressionType;
{ internally used by FindExpressionResultType
  after reading, the cursor will be on the next atom
}
var EndPos, SubStartPos: integer;
begin
  Result:=CleanExpressionType;
  if CurPos.StartPos=CurPos.EndPos then ReadNextAtom;
  // read unary operators which have no effect on the type: +, -, not
  while AtomIsChar('+') or AtomIsChar('-') or UpAtomIs('NOT') do
    ReadNextAtom;
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.ReadOperandTypeAtCursor] A Atom=',GetAtom);
{$ENDIF}
  if UpAtomIs('INHERITED') or (AtomIsIdentifier(false)) then begin
    // read variable
    Result:=FindExpressionTypeOfVariable(CurPos.StartPos,Params,EndPos);
    MoveCursorToCleanPos(EndPos);
  end
  else if UpAtomIs('NIL') then begin
    Result.Desc:=xtNil;
    ReadNextAtom;
  end
  else if AtomIsChar('(') then begin
    // read til bracket end  and find the result of the inner expression
    // this algo is not very fast, but expressions are almost always small
    SubStartPos:=CurPos.EndPos;
    ReadTilBracketClose(true);
    EndPos:=CurPos.EndPos;
    Result:=FindExpressionResultType(Params,SubStartPos,CurPos.StartPos);
    if Result.Desc=xtNone then exit;
    MoveCursorToCleanPos(EndPos);
  end
  else if AtomIsChar('[') then begin
    // 'set' constant
    SubStartPos:=CurPos.StartPos;
    ReadNextAtom;
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
      RaiseException('constant expected, but '+GetAtom+' found');
    end;
    Result.SubDesc:=Result.Desc;
    Result.Desc:=xtConstSet;
    MoveCursorToCleanPos(SubStartPos);
    ReadNextAtom;
    ReadTilBracketClose(true);
    MoveCursorToCleanPos(CurPos.EndPos);
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
  else
    RaiseException('identifier expected, but '+GetAtom+' found');
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

  // ToDo: search for an overloaded operator

  if WordIsBooleanOperator.DoItUpperCase(Src,BinaryOperator.StartPos,
    BinaryOperator.EndPos-BinaryOperator.StartPos)
  then begin
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
    // or xor and mod div shl shr
    Result.Desc:=xtConstOrdInteger;
  end
  else if WordIsNumberOperator.DoItUpperCase(Src,BinaryOperator.StartPos,
    BinaryOperator.EndPos-BinaryOperator.StartPos)
  then begin
    // + - *
    if (Src[BinaryOperator.StartPos]='+')
    and (LeftOperand.Desc in [xtAnsiString,xtShortString,xtString])
    then begin
      Result.Desc:=xtConstString;
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
//   if identifier is founda proc it searches for the best overloaded proc
var FirstParameterNode: TCodeTreeNode;
  ExprInputList: TExprTypeList;
  ParamCompatibility, NewComp: TTypeCompatibility;
  OldInput: TFindDeclarationInput;
  CurFoundContext: TFindContext;
  BestCompatibilityList, CurCompatibilityList: TTypeCompatibilityList;
  CompListSize: integer;
begin
  // the search has found an identifier with the right name
{$IFDEF ShowFoundIdentifier}
writeln('[TFindDeclarationTool.CheckSrcIdentifier]',
' FoundContext=',FoundContext.Node.DescAsString
);
{$ENDIF}
  if FoundContext.Node.Desc=ctnProcedure then begin
    // the found node is a proc
    // Procs can be overloaded, that means there can be several procs with the
    // same name, but with different param lists.
    // The search must go on, and the most compatible proc is returned.
    if (fdfFirstIdentFound in Params.Flags) then begin
      // this is not the first proc found
      // -> identifier will be handled by the first call
      Result:=ifrSuccess;
    end else begin
      if Params.IdentifierTool.IsPCharInSrc(Params.Identifier) then begin
        // this is the first proc found
        Result:=ifrAbortSearch;
        // create the input expression list
        Params.Save(OldInput);
        Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
        Params.Flags:=fdfDefaultForExpressions;
        Params.ContextNode:=Params.IdentifierTool.FindDeepestNodeAtPos(
          CurPos.StartPos,true);
        Params.OnIdentifierFound:=@Self.CheckSrcIdentifier;
        Params.IdentifierTool.ReadNextAtom;
        ExprInputList:=Params.IdentifierTool.CreateParamExprList(
                           Params.IdentifierTool.CurPos.EndPos,Params);
        Params.Load(OldInput);
        // create compatibility lists
        CompListSize:=SizeOf(TTypeCompatibility)*ExprInputList.Count;
        if CompListSize>0 then begin
          GetMem(BestCompatibilityList,CompListSize);
          GetMem(CurCompatibilityList,CompListSize);
        end else begin
          BestCompatibilityList:=nil;
          CurCompatibilityList:=nil;
        end;
        try
          // check the first proc for compatibility
          CurFoundContext:=FoundContext;
          FirstParameterNode:=FoundContext.Tool.GetFirstParameterNode(
            FoundContext.Node);
          ParamCompatibility:=FoundContext.Tool.IsParamListCompatible(
            FirstParameterNode,
            ExprInputList,fdfIgnoreMissingParams in Params.Flags,
            Params,BestCompatibilityList);
          FoundContext:=CurFoundContext;
          if ParamCompatibility=tcExact then begin
            // the first proc fits exactly -> stop the search
            Result:=ifrSuccess;
            exit;
          end;
          // search the other procs
          Params.Load(OldInput);
          Include(Params.Flags,fdfFirstIdentFound);
          Params.SetResult(FoundContext);
          Params.ContextNode:=FoundContext.Node;
          repeat
{$IFDEF ShowFoundIdentifier}
writeln('[TFindDeclarationTool.CheckSrcIdentifier] Search next overloaded proc ',
' Ident="',GetIdentifier(Params.Identifier),'"',
' Params.ContextNode="',Params.ContextNode.DescAsString,'"'
);
{$ENDIF}
            Include(Params.Flags,fdfIgnoreCurContextNode);
            Exclude(Params.Flags,fdfExceptionOnNotFound);
            if Params.NewCodeTool.FindIdentifierInContext(Params) then begin
{$IFDEF ShowFoundIdentifier}
writeln('[TFindDeclarationTool.CheckSrcIdentifier] next overloaded proc ',
' Ident="',GetIdentifier(Params.Identifier),'" found '
);
{$ENDIF}
              if Params.NewNode.Desc=ctnProcedure then begin
                // another overloaded proc found
                // -> check this proc for compatibility too
                CurFoundContext:=CreateFindContext(Params);
                FirstParameterNode:=Params.NewCodeTool.GetFirstParameterNode(
                  Params.NewNode);
                NewComp:=Params.NewCodeTool.IsParamListCompatible(
                  FirstParameterNode,
                  ExprInputList,fdfIgnoreMissingParams in Params.Flags,
                  Params,CurCompatibilityList);
{$IFDEF ShowFoundIdentifier}
writeln('[TFindDeclarationTool.CheckSrcIdentifier] next overloaded proc ',
' Ident="',GetIdentifier(Params.Identifier),'" compatibility=',
TypeCompatibilityNames[NewComp],
' OldCompatibility=',TypeCompatibilityNames[ParamCompatibility],
' Proc="',copy(CurFoundContext.Tool.Src,CurFoundContext.Node.StartPos,70),'"'
);
{$ENDIF}
                if NewComp=tcExact then begin
                  // the proc fits exactly -> stop the search
                  FoundContext:=CurFoundContext;
                  Result:=ifrSuccess;
                  exit;
                end else if NewComp=tcCompatible then begin
                  // the proc fits not exactly, but is compatible
                  if (ParamCompatibility<>tcCompatible)
                  or CompatibilityList1IsBetter(CurCompatibilityList,
                    BestCompatibilityList,ExprInputList.Count) then
                  begin
                    // the new proc fits better
                    ParamCompatibility:=NewComp;
                    Move(CurCompatibilityList^,BestCompatibilityList^,
                      CompListSize);
                    FoundContext:=CurFoundContext;
                  end;
                end;
                // search next overloaded proc
                Params.NewCodeTool:=CurFoundContext.Tool;
                Params.ContextNode:=CurFoundContext.Node;
              end else begin
                // identifier found with same name, but not a proc
                // -> error: duplicate identifier
                FoundContext.Tool.MoveCursorToNodeStart(FoundContext.Node);
                FoundContext.Tool.RaiseException('duplicate identifier '
                  +'"'+GetIdentifier(Params.Identifier)+'"');
              end;
            end else begin
{$IFDEF ShowFoundIdentifier}
writeln('[TFindDeclarationTool.CheckSrcIdentifier] no next overloaded proc ',
' Ident="',GetIdentifier(Params.Identifier),'" found'
);
{$ENDIF}
              // no further proc found
              if (ParamCompatibility=tcIncompatible)
              and (fdfOnlyCompatibleProc in OldInput.Flags) then begin
                // no compatible proc found at all
                if fdfExceptionOnNotFound in OldInput.Flags then begin
                  if not Params.IdentifierTool.IsPCharInSrc(Params.Identifier)
                  then
                    Params.IdentifierTool.RaiseException(
                      '[TFindDeclarationTool.CheckSrcIdentifier]'
                     +' internal error B: not IsPCharInSrc(Params.Identifier) '
                     +' Params.IdentifierTool='
                     +TCodeBuffer(Params.IdentifierTool.Scanner.MainCode).Filename
                     +' Ident="'+GetIdentifier(Params.Identifier)+'"');
                  Params.IdentifierTool.MoveCursorToCleanPos(Params.Identifier);
                  RaiseException('identifier not found '
                    +'"'+GetIdentifier(Params.Identifier)+'"');
                end else begin
                  Result:=ifrAbortSearch;
                  exit;
                end;
              end else begin
                // proc found
                Result:=ifrSuccess;
                exit;
              end;
            end;
          until false;
        finally
          // end overloaded proc search
          Exclude(Params.Flags,fdfFirstIdentFound);
          // free memory
          ExprInputList.Free;
          if BestCompatibilityList<>nil then
            FreeMem(BestCompatibilityList);
          if CurCompatibilityList<>nil then
            FreeMem(CurCompatibilityList);
          // adjust result
          if Result=ifrSuccess then begin
            if FoundContext.Node.Desc=ctnProcedure then
              // adjust cursor to head
              FoundContext.Node:=FoundContext.Node.FirstChild;
            Params.SetResult(FoundContext);
          end;
        end;
      end else begin
        // Params.Identifier is not in the source of this tool
        Result:=ifrSuccess;
      end;
    end;
  end else begin
    Result:=ifrSuccess;
  end;
end;

function TFindDeclarationTool.DoOnIdentifierFound(
  Params: TFindDeclarationParams;
  FoundNode: TCodeTreeNode): TIdentifierFoundResult;
// this internal function is called, whenever an identifier is found
begin
  if Assigned(Params.OnIdentifierFound) then
    Result:=Params.OnIdentifierFound(Params,CreateFindContext(Self,FoundNode))
  else
    Result:=ifrSuccess;
end;

function TFindDeclarationTool.IsCompatible(Node: TCodeTreeNode;
  ExpressionType: TExpressionType;
  Params: TFindDeclarationParams): TTypeCompatibility;
var FindContext: TFindContext;
  OldInput: TFindDeclarationInput;
  NodeExprType: TExpressionType;
begin
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.IsCompatible] A Node=',Node.DescAsString,
' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc]);
{$ENDIF}
  Result:=tcIncompatible;
  // find base type of node
  OldInput.Flags:=Params.Flags;
  Include(Params.Flags,fdfExceptionOnNotFound);
  FindContext:=FindBaseTypeOfNode(Params,Node);
  Params.Flags:=OldInput.Flags;
  if (FindContext.Node.Desc=ctnSetType) then begin
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.IsCompatible] FindContext.Node.Desc=ctnSetType',
'"',copy(FindContext.Tool.Src,FindContext.Node.Parent.StartPos,20),'"');
{$ENDIF}
    if (ExpressionType.Desc<>xtConstSet) then
      exit;
    // both are sets, compare type of sets
    // -> read operand type of set type of node
    
    // ToDo: ppu, ppw, dcu
    
    FindContext.Tool.MoveCursorToNodeStart(FindContext.Node.FirstChild);
    NodeExprType:=ReadOperandTypeAtCursor(Params);
    ExpressionType.Desc:=ExpressionType.SubDesc;
    Result:=IsCompatible(NodeExprType,ExpressionType,Params);
    exit;
  end;
  // compare node base type and ExpressionType
  if (ExpressionType.Context.Node<>nil)
  and (ExpressionType.Context.Node=FindContext.Node) then begin
    // same base type
    Result:=tcExact;
  end else begin
    NodeExprType:=ConvertNodeToExpressionType(Node,Params);
    Result:=IsCompatible(NodeExprType,ExpressionType,Params);
  end;
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.IsCompatible] END',
' BaseNode=',FindContext.Node.DescAsString,
' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc],
' Result=',TypeCompatibilityNames[Result]
);
{$ENDIF}
end;

function TFindDeclarationTool.CreateParamExprList(StartPos: integer;
  Params: TFindDeclarationParams): TExprTypeList;
var ExprType: TExpressionType;
  BracketClose: char;
  ExprStartPos, ExprEndPos: integer;
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
          ReadNextAtom;
          if AtomIsChar('(') or AtomIsChar('[') then begin
            ReadTilBracketClose(true);
            ReadNextAtom;
          end;
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
          RaiseException(BracketClose+' expected, but '+GetAtom+' found');
        CurPos.StartPos:=CurPos.EndPos;
      end;
    end;
  end;
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.CreateParamExprList] END ',
'ParamCount=',Result.Count,' "',copy(Src,StartPos,40),'"');
writeln('  ExprList=[',Result.AsString,']');
{$ENDIF}
end;

function TFindDeclarationTool.PredefinedIdentToTypeDesc(Identifier: PChar
  ): TExpressionTypeDesc;
begin
  if CompareSrcIdentifiers(Identifier,'INT64') then
    Result:=xtInt64
  else if CompareSrcIdentifiers(Identifier,'CARDINAL') then
    Result:=xtCardinal
  else if CompareSrcIdentifiers(Identifier,'QWORD') then
    Result:=xtQWord
  else if CompareSrcIdentifiers(Identifier,'BOOLEAN') then
    Result:=xtBoolean
  else if CompareSrcIdentifiers(Identifier,'BYTEBOOL') then
    Result:=xtByteBool
  else if CompareSrcIdentifiers(Identifier,'LONGBOOL') then
    Result:=xtLongBool
  else if CompareSrcIdentifiers(Identifier,'CHAR') then
    Result:=xtChar
  else if CompareSrcIdentifiers(Identifier,'REAL') then
    Result:=xtReal
  else if CompareSrcIdentifiers(Identifier,'SINGLE') then
    Result:=xtSingle
  else if CompareSrcIdentifiers(Identifier,'DOUBLE') then
    Result:=xtDouble
  else if CompareSrcIdentifiers(Identifier,'EXTENDED') then
    Result:=xtExtended
  else if CompareSrcIdentifiers(Identifier,'COMP') then
    Result:=xtComp
  else if CompareSrcIdentifiers(Identifier,'CURRENCY') then
    Result:=xtCurrency
  else if CompareSrcIdentifiers(Identifier,'STRING') then

    // ToDo: ask scanner, if AnsiString or ShortString

    Result:=xtString
  else if CompareSrcIdentifiers(Identifier,'SHORTSTRING') then
    Result:=xtShortString
  else if CompareSrcIdentifiers(Identifier,'ANSISTRING') then
    Result:=xtAnsiString
  else if CompareSrcIdentifiers(Identifier,'WIDESTRING') then
    Result:=xtWideString
  else if CompareSrcIdentifiers(Identifier,'TRUE')
    or CompareSrcIdentifiers(Identifier,'FALSE') then
    Result:=xtConstBoolean
  else
    RaiseException('[TFindDeclarationTool.PredefinedIdentToTypeDesc] '
      +'internal error: not predefined identifier '+GetIdentifier(Identifier));
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
var CurContext: TFindContext;
  OldInput: TFindDeclarationInput;
begin
  if DescendContext.Node.Desc<>ctnClass then
    RaiseException('[TFindDeclarationTool.ContextIsDescendOf] '
      +' internal error: DescendContext.Desc<>ctnClass');
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

function TFindDeclarationTool.IsCompatible(TargetType,
  ExpressionType: TExpressionType; Params: TFindDeclarationParams
  ): TTypeCompatibility;
begin
{$IFDEF ShowExprEval}
writeln('[TFindDeclarationTool.IsCompatible] B ',
' TargetType=',ExpressionTypeDescNames[TargetType.Desc],
' ExpressionType=',ExpressionTypeDescNames[ExpressionType.Desc]);
{$ENDIF}
  Result:=tcIncompatible;
  if (TargetType.Desc=ExpressionType.Desc)
  and (not (TargetType.Desc in [xtNone,xtContext])) then begin
    Result:=tcExact;
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
    or ((TargetType.Desc=xtContext)
      and (TargetType.Context.Node.Desc in [ctnClass,ctnProcedure])
      and (ExpressionType.Desc=xtNil))
    then
      Result:=tcCompatible
    else if (ExpressionType.Desc=xtContext) then begin
      if ExpressionType.Context.Node.Desc=ctnClass then begin
        // check, if ExpressionType.Context is descend of FindContext
        if ContextIsDescendOf(ExpressionType.Context,TargetType.Context,Params)
        then
          Result:=tcCompatible;
      end;
    end;
  end;
end;

procedure TFindDeclarationTool.DoDeleteNodes;
begin
  ClearNodeCaches(true);
  if FInterfaceIdentifierCache<>nil then
    FInterfaceIdentifierCache.Clear;
  inherited DoDeleteNodes;
end;

destructor TFindDeclarationTool.Destroy;
begin
  FInterfaceIdentifierCache.Free;
  FInterfaceIdentifierCache:=nil;
  inherited Destroy;
end;

procedure TFindDeclarationTool.ClearNodeCaches(Force: boolean);
var
  NodeCache: TCodeTreeNodeCache;
  GlobalWriteLockIsSet: boolean;
  GlobalWriteLockStep: integer;
  BaseTypeCache: TBaseTypeCache;
begin
  if not Force then begin
    // check if node cache must be cleared
    if Assigned(OnGetGlobalWriteLockInfo) then begin
      OnGetGlobalWriteLockInfo(GlobalWriteLockIsSet,GlobalWriteLockStep);
      if GlobalWriteLockIsSet then begin
        // The global write lock is set. That means, input variables and code
        // are frozen
        if (FLastNodeCachesGlobalWriteLockStep=GlobalWriteLockStep) then begin
          // source and values did not change since last UpdateNeeded check
          exit;
        end else begin
          // this is the first check in this GlobalWriteLockStep
          FLastNodeCachesGlobalWriteLockStep:=GlobalWriteLockStep;
          // proceed normally ...
        end;
      end;
    end;
  end;
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
end;

procedure TFindDeclarationTool.ActivateGlobalWriteLock;
begin
  inherited;
  ClearNodeCaches(false);
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

procedure TFindDeclarationTool.AddResultToNodeCaches(Identifier: PChar;
  StartNode, EndNode: TCodeTreeNode; SearchedForward: boolean;
  Params: TFindDeclarationParams; SearchRangeFlags: TNodeCacheEntryFlags);
var Node: TCodeTreeNode;
  CurNodeCache, LastNodeCache: TCodeTreeNodeCache;
  CleanStartPos, CleanEndPos: integer;
  NewNode: TCodeTreeNode;
  NewTool: TPascalParserTool;
  NewCleanPos: integer;
begin
  Node:=StartNode;
  LastNodeCache:=nil;
  if Params<>nil then begin
    NewNode:=Params.NewNode;
    NewTool:=Params.NewCodeTool;
    NewCleanPos:=Params.NewCleanPos;
  end else begin
    NewNode:=nil;
    NewTool:=nil;
  end;
  CleanStartPos:=StartNode.StartPos;
  CleanEndPos:=StartNode.EndPos;
  if EndNode<>nil then begin
    if EndNode.StartPos<CleanStartPos then
      CleanStartPos:=EndNode.StartPos;
    if EndNode.EndPos>CleanEndPos then
      CleanEndPos:=EndNode.EndPos;
  end else begin
    if not SearchedForward then
      CleanStartPos:=1
    else
      CleanEndPos:=SrcLen+1;
  end;
{$IFDEF ShowNodeCache}
if CompareSrcIdentifiers(Identifier,'TDefineAction')
and (ExtractFileName(MainFilename)='codetoolsdefines.pas') then begin
  writeln('(((((((((((((((((((((((((((==================');
  write('TFindDeclarationTool.AddResultToNodeCaches ',
  ' Ident=',GetIdentifier(Identifier));
  write(' SearchedForward=',SearchedForward);
  write(' Flags=[');
  if ncefSearchedInParents in SearchRangeFlags then write('Parents');
  if ncefSearchedInAncestors in SearchRangeFlags then write(',Ancestors');
  writeln(']');
  write('     StartNode=',StartNode.DescAsString,'="',copy(Src,StartNode.StartPos-10,10),'|',copy(Src,StartNode.StartPos,15),'"');
  if EndNode<>nil then
    write(' EndNode=',EndNode.DescAsString,'="',copy(Src,EndNode.StartPos,25),'"')
  else
    write(' EndNode=nil');
  writeln('');
  writeln('     Self=',MainFilename);
  if Params<>nil then begin
    writeln('       NewNode=',Params.NewNode.DescAsString,
               ' NewTool=',Params.NewCodeTool.MainFilename);
  end else begin
    writeln('       NOT FOUND');
  end;
  writeln('  CleanStartPos=',CleanStartPos,' CleanEndPos=',CleanEndPos);
end;
{$ENDIF}
  while (Node<>nil) do begin
    if (Node.Desc in AllNodeCacheDescs) then begin
      if (Node.Cache=nil) then
        CreateNewNodeCache(Node);
      if (Node.Cache is TCodeTreeNodeCache) then begin
        CurNodeCache:=TCodeTreeNodeCache(Node.Cache);
        if LastNodeCache<>CurNodeCache then begin
{$IFDEF ShowNodeCache}
if CompareSrcIdentifiers(Identifier,'TDefineAction')
and (ExtractFileName(MainFilename)='codetoolsdefines.pas') then begin
  CurNodeCache.WriteDebugReport('  VORHER NODECACHE REPORT: ');
end;
{$ENDIF}
          CurNodeCache.Add(Identifier,CleanStartPos,CleanEndPos,
                           NewNode,NewTool,NewCleanPos,SearchRangeFlags);
{$IFDEF ShowNodeCache}
if CompareSrcIdentifiers(Identifier,'TDefineAction')
and (ExtractFileName(MainFilename)='codetoolsdefines.pas') then begin
  CurNodeCache.WriteDebugReport('  NACHHER NODECACHE REPORT: ');
end;
{$ENDIF}
          LastNodeCache:=CurNodeCache;
        end;
      end;
    end;
    Node:=Node.Parent;
    if (EndNode<>nil) and (Node=EndNode.Parent) then break;
  end;
{$IFDEF ShowNodeCache}
if CompareSrcIdentifiers(Identifier,'TDefineAction')
and (ExtractFileName(MainFilename)='codetoolsdefines.pas') then begin
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
var IsPredefinedIdentifier: boolean;
  OldFlags: TFindDeclarationFlags;
begin
  IsPredefinedIdentifier:=WordIsPredefinedIdentifier.DoIt(Params.Identifier);
  OldFlags:=Params.Flags;
  if IsPredefinedIdentifier then
    Exclude(Params.Flags,fdfExceptionOnNotFound)
  else
    Include(Params.Flags,fdfExceptionOnNotFound);
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
end;

procedure TFindDeclarationParams.Load(var Input: TFindDeclarationInput);
begin
  Flags:=Input.Flags;
  Identifier:=Input.Identifier;
  ContextNode:=Input.ContextNode;
  OnIdentifierFound:=Input.OnIdentifierFound;
  IdentifierTool:=Input.IdentifierTool;
end;

procedure TFindDeclarationParams.Save(var Input: TFindDeclarationInput);
begin
  Input.Flags:=Flags;
  Input.Identifier:=Identifier;
  Input.ContextNode:=ContextNode;
  Input.OnIdentifierFound:=OnIdentifierFound;
  Input.IdentifierTool:=IdentifierTool;
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

procedure TExprTypeList.Add(ExprType: TExpressionType);
var NewSize: integer;
begin
  inc(Count);
  NewSize:=Count*SizeOf(TExpressionType);
  if Count=1 then
    GetMem(Items,NewSize)
  else
    ReAllocMem(Items,NewSize);
  Items[Count-1]:=ExprType;
end;


end.


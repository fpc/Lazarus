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
    TPascalParserTool enhances TMultiKeyWordListCodeTool.
    This tool parses the pascal code, makes simple syntax checks and provides
    a lot of useful parsing functions. It can either parse complete sources
    or parts of it.
    
}
unit PascalParserTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE ShowIgnoreErrorAfter}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, CustomCodeTool,
  MultiKeyWordListTool, SourceLog, KeywordFuncLists, BasicCodeTools,
  LinkScanner, CodeCache, AVL_Tree, TypInfo, SourceChanger;

type
  TProcHeadAttribute = (
      // extract attributes:
      phpWithStart,          // proc keyword e.g. 'function', 'class procedure'
      phpWithoutClassKeyword,// without 'class' proc keyword
      phpAddClassName,       // extract/add 'ClassName.'
      phpWithoutClassName,   // skip classname
      phpWithoutName,        // skip function name
      phpWithoutParamList,   // skip param list
      phpWithVarModifiers,   // extract 'var', 'out', 'const'
      phpWithParameterNames, // extract parameter names
      phpWithoutParamTypes,  // skip colon, param types and default values
      phpWithDefaultValues,  // extract default values
      phpWithResultType,     // extract colon + result type
      phpWithOfObject,       // extract 'of object'
      phpWithCallingSpecs,   // extract cdecl; inline;
      phpWithComments,       // extract comments
      phpInUpperCase,        // turn to uppercase
      phpCommentsToSpace,    // replace comments with a single space
                             //   (normally unnecessary space is skipped)
      phpWithoutBrackets,    // skip start- and end-bracket of parameter list
      // search attributes:
      phpIgnoreForwards,     // skip forward procs
      phpIgnoreProcsWithBody,// skip procs with begin..end
      phpIgnoreMethods,      // skip method bodies and definitions
      phpOnlyWithClassname,  // skip procs without the right classname
      phpFindCleanPosition,  // read til ExtractSearchPos
      // parse attributes:
      phpCreateNodes         // create nodes during reading
    );
  TProcHeadAttributes = set of TProcHeadAttribute;
  
  TParseProcHeadAttribute = (pphIsMethod, pphIsFunction, pphIsType,
     pphIsOperator, pphCreateNodes);
  TParseProcHeadAttributes =  set of TParseProcHeadAttribute;
  
  TProcHeadExtractPos = (phepNone, phepStart, phepName, phepParamList,
    phepResultType, phepSpecifiers);
    
  TTreeRange = (trInterface, trAll, trTillCursor);
  
  TBuildTreeFlag = (btSetIgnoreErrorPos,btKeepIgnoreErrorPos);
  TBuildTreeFlags = set of TBuildTreeFlag;
  
  TPascalParserTool = class(TMultiKeyWordListCodeTool)
  private
  protected
    TypeKeyWordFuncList: TKeyWordFunctionList;
    InnerClassKeyWordFuncList: TKeyWordFunctionList;
    ClassInterfaceKeyWordFuncList: TKeyWordFunctionList;
    ClassVarTypeKeyWordFuncList: TKeyWordFunctionList;
    ExtractMemStream: TMemoryStream;
    ExtractSearchPos: integer;
    ExtractFoundPos: integer;
    ExtractProcHeadPos: TProcHeadExtractPos;
    procedure RaiseCharExpectedButAtomFound(c: char);
    procedure RaiseStringExpectedButAtomFound(const s: string);
    procedure RaiseUnexpectedKeyWord;
    procedure RaiseIllegalQualifier;
  protected
    procedure InitExtraction;
    function GetExtraction: string;
    function ExtractStreamEndIsIdentChar: boolean;
    procedure ExtractNextAtom(AddAtom: boolean; Attr: TProcHeadAttributes);
    // sections
    function KeyWordFuncSection: boolean;
    function KeyWordFuncEndPoint: boolean;
    // type/var/const/resourcestring
    function KeyWordFuncType: boolean;
    function KeyWordFuncVar: boolean;
    function KeyWordFuncConst: boolean;
    function KeyWordFuncResourceString: boolean;
    function KeyWordFuncLabel: boolean;
    // types
    function KeyWordFuncClass: boolean;
    function KeyWordFuncClassInterface: boolean;
    function KeyWordFuncTypePacked: boolean;
    function KeyWordFuncTypeArray: boolean;
    function KeyWordFuncTypeProc: boolean;
    function KeyWordFuncTypeSet: boolean;
    function KeyWordFuncTypeLabel: boolean;
    function KeyWordFuncTypeType: boolean;
    function KeyWordFuncTypeFile: boolean;
    function KeyWordFuncTypePointer: boolean;
    function KeyWordFuncTypeRecord: boolean;
    function KeyWordFuncTypeRecordCase: boolean;
    function KeyWordFuncTypeDefault: boolean;
    // procedures/functions/methods
    function KeyWordFuncProc: boolean;
    function KeyWordFuncBeginEnd: boolean;
    // class/object elements
    function KeyWordFuncClassSection: boolean;
    function KeyWordFuncClassMethod: boolean;
    function KeyWordFuncClassProperty: boolean;
    function KeyWordFuncClassReadTilEnd: boolean;
    function KeyWordFuncClassIdentifier: boolean;
    function KeyWordFuncClassVarTypeClass: boolean;
    function KeyWordFuncClassVarTypePacked: boolean;
    function KeyWordFuncClassVarTypeRecord: boolean;
    function KeyWordFuncClassVarTypeArray: boolean;
    function KeyWordFuncClassVarTypeSet: boolean;
    function KeyWordFuncClassVarTypeProc: boolean;
    function KeyWordFuncClassVarTypeIdent: boolean;
    // keyword lists
    procedure BuildDefaultKeyWordFunctions; override;
    procedure BuildTypeKeyWordFunctions; virtual;
    procedure BuildInnerClassKeyWordFunctions; virtual;
    procedure BuildClassVarTypeKeyWordFunctions; virtual;
    procedure BuildClassInterfaceKeyWordFunctions; virtual;
    function UnexpectedKeyWord: boolean;
    // read functions
    function ReadTilProcedureHeadEnd(ParseAttr: TParseProcHeadAttributes;
        var HasForwardModifier: boolean): boolean;
    function ReadConstant(ExceptionOnError, Extract: boolean;
        Attr: TProcHeadAttributes): boolean;
    function ReadParamType(ExceptionOnError, Extract: boolean;
        Attr: TProcHeadAttributes): boolean;
    function ReadParamList(ExceptionOnError, Extract: boolean;
        Attr: TProcHeadAttributes): boolean;
    function ReadUsesSection(ExceptionOnError: boolean): boolean;
    function ReadSubRange(ExceptionOnError: boolean): boolean;
    function ReadTilBlockEnd(StopOnBlockMiddlePart,
        CreateNodes: boolean): boolean;
    function ReadBackTilBlockEnd(StopOnBlockMiddlePart: boolean): boolean;
    function ReadTilVariableEnd(ExceptionOnError: boolean): boolean;
    function ReadTilStatementEnd(ExceptionOnError,
        CreateNodes: boolean): boolean;
    function ReadWithStatement(ExceptionOnError,
        CreateNodes: boolean): boolean;
    procedure ReadVariableType;
    function ReadTilTypeOfProperty(PropertyNode: TCodeTreeNode): boolean;
    procedure ReadGUID;
  public
    CurSection: TCodeTreeNodeDesc;

    InterfaceSectionFound: boolean;
    ImplementationSectionFound: boolean;
    EndOfSourceFound: boolean;

    function CleanPosIsInComment(CleanPos, CleanCodePosInFront: integer;
        var CommentStart, CommentEnd: integer): boolean;
        
    procedure BuildTree(OnlyInterfaceNeeded: boolean); virtual;
    procedure BuildTreeAndGetCleanPos(TreeRange: TTreeRange;
        const CursorPos: TCodeXYPosition; var CleanCursorPos: integer;
        BuildTreeFlags: TBuildTreeFlags);
    procedure BuildSubTreeForClass(ClassNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForBeginBlock(BeginNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForProcHead(ProcNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForProcHead(ProcNode: TCodeTreeNode;
        var FunctionResult: TCodeTreeNode);

    function DoAtom: boolean; override;

    function ExtractNode(ANode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;

    function ExtractPropType(PropNode: TCodeTreeNode;
        InUpperCase, EmptyIfIndexed: boolean): string;
    function ExtractProcName(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractProcHead(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractClassName(ClassNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode): string;
    function FindProcNode(StartNode: TCodeTreeNode; const AProcHead: string;
        Attr: TProcHeadAttributes): TCodeTreeNode;
    function FindProcBody(ProcNode: TCodeTreeNode): TCodeTreeNode;
    procedure MoveCursorToFirstProcSpecifier(ProcNode: TCodeTreeNode);
    function MoveCursorToProcSpecifier(ProcNode: TCodeTreeNode;
        ProcSpec: TProcedureSpecifier): boolean;
    function ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
        ProcSpec: TProcedureSpecifier): boolean;
    function GetProcNameIdentifier(ProcNode: TCodeTreeNode): PChar;

    function ExtractPropName(PropNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractProperty(PropNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function GetPropertyNameIdentifier(PropNode: TCodeTreeNode): PChar;

    function FindVarNode(StartNode: TCodeTreeNode;
        const UpperVarName: string): TCodeTreeNode;
    function FindTypeNodeOfDefinition(
        DefinitionNode: TCodeTreeNode): TCodeTreeNode;

    function FindFirstNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    function FindNextNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    
    function FindClassNode(StartNode: TCodeTreeNode;
        const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindClassNodeInInterface(const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function ClassSectionNodeStartsWithWord(ANode: TCodeTreeNode): boolean;

    function GetSourceType: TCodeTreeNodeDesc;
    function FindInterfaceNode: TCodeTreeNode;
    function FindImplementationNode: TCodeTreeNode;
    function FindInitializationNode: TCodeTreeNode;
    function FindMainBeginEndNode: TCodeTreeNode;
    
    function NodeHasParentOfType(ANode: TCodeTreeNode;
        NodeDesc: TCodeTreeNodeDesc): boolean;
    function NodeIsInAMethod(Node: TCodeTreeNode): boolean;
    function NodeIsMethodBody(ProcNode: TCodeTreeNode): boolean;
    function NodeIsFunction(ProcNode: TCodeTreeNode): boolean;
    function NodeIsConstructor(ProcNode: TCodeTreeNode): boolean;
    function NodeIsPartOfTypeDefinition(ANode: TCodeTreeNode): boolean;
    function PropertyIsDefault(PropertyNode: TCodeTreeNode): boolean;
    function PropertyNodeHasParamList(PropNode: TCodeTreeNode): boolean;
    function PropNodeIsTypeLess(PropNode: TCodeTreeNode): boolean;
    function ProcNodeHasParamList(ProcNode: TCodeTreeNode): boolean;

    procedure MoveCursorToUsesEnd(UsesNode: TCodeTreeNode);
    procedure ReadPriorUsedUnit(var UnitNameAtom, InAtom: TAtomPosition);
    
    constructor Create;
    destructor Destroy; override;
  end;
  
const
  ProcHeadAttributeNames: array[TProcHeadAttribute] of string = (
      // extract attributes:
      'phpWithStart',
      'phpWithoutClassKeyword',
      'phpAddClassName',
      'phpWithoutClassName',
      'phpWithoutName',
      'phpWithoutParamList',
      'phpWithVarModifiers',
      'phpWithParameterNames',
      'phpWithoutParamTypes',
      'phpWithDefaultValues',
      'phpWithResultType',
      'phpWithOfObject',
      'phpWithCallingSpecs',
      'phpWithComments',
      'phpInUpperCase',
      'phpCommentsToSpace',
      'phpWithoutBrackets',
      // search attributes:
      'phpIgnoreForwards',
      'phpIgnoreProcsWithBody',
      'phpIgnoreMethods',
      'phpOnlyWithClassname',
      'phpFindCleanPosition',
      // parse attributes:
      'phpCreateNodes'
    );

function ProcHeadAttributesToStr(Attr: TProcHeadAttributes): string;


implementation


type
  TEndBlockType = (ebtBegin, ebtAsm, ebtTry, ebtCase, ebtRepeat, ebtRecord,
                   ebtClass, ebtObject);
  TTryType = (ttNone, ttFinally, ttExcept);

function ProcHeadAttributesToStr(Attr: TProcHeadAttributes): string;
var a: TProcHeadAttribute;
begin
  Result:='';
  for a:=Low(TProcHeadAttribute) to High(TProcHeadAttribute) do begin
    if a in Attr then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+ProcHeadAttributeNames[a];
    end;
  end;
end;

{ TPascalParserTool }

constructor TPascalParserTool.Create;
begin
  inherited Create;
  // keywords for parsing types
  TypeKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildTypeKeyWordFunctions;
  AddKeyWordFuncList(TypeKeyWordFuncList);
  // KeyWord functions for parsing in a class
  InnerClassKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildInnerClassKeyWordFunctions;
  AddKeyWordFuncList(InnerClassKeyWordFuncList);
  ClassVarTypeKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildClassVarTypeKeyWordFunctions;
  AddKeyWordFuncList(ClassVarTypeKeyWordFuncList);
  // KeyWord functions for parsing an class interface
  ClassInterfaceKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildClassInterfaceKeyWordFunctions;
  AddKeyWordFuncList(ClassInterfaceKeyWordFuncList);
end;

destructor TPascalParserTool.Destroy;
begin
  if ExtractMemStream<>nil then
    ExtractMemStream.Free;
  inherited Destroy;
end;

procedure TPascalParserTool.BuildDefaultKeyWordFunctions;
begin
  inherited BuildDefaultKeyWordFunctions;
  with KeyWordFuncList do begin
    Add('PROGRAM',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('LIBRARY',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('PACKAGE',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('UNIT',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('INTERFACE',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}KeyWordFuncSection);

    Add('END',{$ifdef FPC}@{$endif}KeyWordFuncEndPoint);
    Add('.',{$ifdef FPC}@{$endif}KeyWordFuncEndPoint);

    Add('TYPE',{$ifdef FPC}@{$endif}KeyWordFuncType);
    Add('VAR',{$ifdef FPC}@{$endif}KeyWordFuncVar);
    Add('THREADVAR',{$ifdef FPC}@{$endif}KeyWordFuncVar);
    Add('CONST',{$ifdef FPC}@{$endif}KeyWordFuncConst);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}KeyWordFuncResourceString);
    Add('LABEL',{$ifdef FPC}@{$endif}KeyWordFuncLabel);

    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncProc);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncProc);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncProc);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncProc);
    Add('OPERATOR',{$ifdef FPC}@{$endif}KeyWordFuncProc);
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncProc);

    Add('BEGIN',{$ifdef FPC}@{$endif}KeyWordFuncBeginEnd);
    Add('ASM',{$ifdef FPC}@{$endif}KeyWordFuncBeginEnd);
    
    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}UnexpectedKeyWord;
  end;
end;

procedure TPascalParserTool.BuildTypeKeyWordFunctions;
// KeyWordFunctions for parsing types
begin
  with TypeKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('OBJECT',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('INTERFACE',{$ifdef FPC}@{$endif}KeyWordFuncClassInterface);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}KeyWordFuncClassInterface);
    Add('PACKED',{$ifdef FPC}@{$endif}KeyWordFuncTypePacked);
    Add('ARRAY',{$ifdef FPC}@{$endif}KeyWordFuncTypeArray);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncTypeProc);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncTypeProc);
    Add('SET',{$ifdef FPC}@{$endif}KeyWordFuncTypeSet);
    Add('LABEL',{$ifdef FPC}@{$endif}KeyWordFuncTypeLabel);
    Add('TYPE',{$ifdef FPC}@{$endif}KeyWordFuncTypeType);
    Add('FILE',{$ifdef FPC}@{$endif}KeyWordFuncTypeFile);
    Add('RECORD',{$ifdef FPC}@{$endif}KeyWordFuncTypeRecord);
    Add('^',{$ifdef FPC}@{$endif}KeyWordFuncTypePointer);
    
    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}KeyWordFuncTypeDefault;
  end;
end;

procedure TPascalParserTool.BuildInnerClassKeyWordFunctions;
// KeyWordFunctions for parsing in a class/object
begin
  with InnerClassKeyWordFuncList do begin
    Add('PUBLIC',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);
    Add('PRIVATE',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);
    Add('PUBLISHED',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);
    Add('PROTECTED',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);

    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('STATIC',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);

    Add('PROPERTY',{$ifdef FPC}@{$endif}KeyWordFuncClassProperty);
    
    Add('END',{$ifdef FPC}@{$endif}AllwaysFalse);

    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}KeyWordFuncClassIdentifier;
  end;
end;

procedure TPascalParserTool.BuildClassVarTypeKeyWordFunctions;
// KeywordFunctions for parsing the type of a variable in a class/object
begin
  with ClassVarTypeKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeClass);
    Add('OBJECT',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeClass);
    Add('PACKED',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypePacked);
    Add('RECORD',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeRecord);
    Add('ARRAY',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeArray);
    Add('SET',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeSet);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeProc);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeProc);

    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeIdent;
  end;
end;

procedure TPascalParserTool.BuildClassInterfaceKeyWordFunctions;
// KeyWordFunctions for parsing in a class interface, dispinterface
begin
  with ClassInterfaceKeyWordFuncList do begin
    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('PROPERTY',{$ifdef FPC}@{$endif}KeyWordFuncClassProperty);
    Add('END',{$ifdef FPC}@{$endif}AllwaysFalse);

    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}AllwaysFalse;
  end;
end;

function TPascalParserTool.UnexpectedKeyWord: boolean;
begin
  Result:=false;
  SaveRaiseExceptionFmt(ctsUnexpectedKeyword,[GetAtom]);
end;

procedure TPascalParserTool.BuildTree(OnlyInterfaceNeeded: boolean);
begin
  {$IFDEF MEM_CHECK}CheckHeap('TBasicCodeTool.BuildTree A '+IntToStr(GetMem_Cnt));{$ENDIF}
  {$IFDEF CTDEBUG}
  writeln('TPascalParserTool.BuildTree A');
  {$ENDIF}
  if not UpdateNeeded(OnlyInterfaceNeeded) then begin
    // input is the same as last time -> output is the same
    // -> if there was an error, raise it again
    if (LastErrorPhase in [CodeToolPhaseScan,CodeToolPhaseParse])
    and ((not IgnoreErrorAfterValid)
      or (not IgnoreErrAfterPositionIsInFrontOfLastErrMessage))
    then
      RaiseLastError;
    exit;
  end;
  ClearLastError;
  writeln('TPascalParserTool.BuildTree B OnlyIntf=',OnlyInterfaceNeeded,'  ',TCodeBuffer(Scanner.MainCode).Filename);
  //CheckHeap('TBasicCodeTool.BuildTree B '+IntToStr(GetMem_Cnt));
  
  // scan code
  BeginParsing(true,OnlyInterfaceNeeded);
  
  // parse code and build codetree
  CurrentPhase:=CodeToolPhaseParse;
  if Scanner.CompilerMode=cmDELPHI then
    WordIsKeyWordFuncList:=WordIsDelphiKeyWord
  else
    WordIsKeyWordFuncList:=WordIsKeyWord;
  
  InterfaceSectionFound:=false;
  ImplementationSectionFound:=false;
  EndOfSourceFound:=false;
  
  try
    ReadNextAtom;
    if UpAtomIs('UNIT') then
      CurSection:=ctnUnit
    else if UpAtomIs('PROGRAM') then
      CurSection:=ctnProgram
    else if UpAtomIs('PACKAGE') then
      CurSection:=ctnPackage
    else if UpAtomIs('LIBRARY') then
      CurSection:=ctnLibrary
    else
      SaveRaiseExceptionFmt(ctsNoPascalCodeFound,[GetAtom]);
    CreateChildNode;
    CurNode.Desc:=CurSection;
    ReadNextAtom; // read source name
    AtomIsIdentifier(true);
    ReadNextAtom; // read ';'
    if (CurPos.Flag<>cafSemicolon) then
      RaiseCharExpectedButAtomFound(';');
    if CurSection=ctnUnit then begin
      ReadNextAtom;
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode;
      if not UpAtomIs('INTERFACE') then
        RaiseStringExpectedButAtomFound('"interface"');
      CreateChildNode;
      CurSection:=ctnInterface;
      CurNode.Desc:=CurSection;
    end;
    InterfaceSectionFound:=true;
    ReadNextAtom;
    if UpAtomIs('USES') then
      ReadUsesSection(true);
    repeat
      //writeln('[TPascalParserTool.BuildTree] ALL '+GetAtom);
      if not DoAtom then break;
      if CurSection=ctnNone then begin
        EndOfSourceFound:=true;
        break;
      end;
      ReadNextAtom;
    until (CurPos.StartPos>SrcLen);
    FForceUpdateNeeded:=false;
  except
    {$IFDEF ShowIgnoreErrorAfter}
    writeln('TPascalParserTool.BuildTree ',MainFilename,' ERROR: ',LastErrorMessage);
    {$ENDIF}
    if (not IgnoreErrorAfterValid)
    or (not IgnoreErrAfterPositionIsInFrontOfLastErrMessage) then
      raise;
    {$IFDEF ShowIgnoreErrorAfter}
    writeln('TPascalParserTool.BuildTree ',MainFilename,' IGNORING ERROR: ',LastErrorMessage);
    {$ENDIF}
  end;
  {$IFDEF CTDEBUG}
  writeln('[TPascalParserTool.BuildTree] END');
  {$ENDIF}
  {$IFDEF MEM_CHECK}
  CheckHeap('TBasicCodeTool.BuildTree END '+IntToStr(GetMem_Cnt));
  {$ENDIF}
  CurrentPhase:=CodeToolPhaseTool;
end;

procedure TPascalParserTool.BuildSubTreeForClass(ClassNode: TCodeTreeNode);
// reparse a quick parsed class and build the child nodes

  procedure RaiseClassNodeNil;
  begin
    SaveRaiseException(
       'TPascalParserTool.BuildSubTreeForClass: Classnode=nil');
  end;
  
  procedure RaiseClassDescInvalid;
  begin
    SaveRaiseException('[TPascalParserTool.BuildSubTreeForClass] ClassNode.Desc='
                   +ClassNode.DescAsString);
  end;

  procedure RaiseClassKeyWordExpected;
  begin
    SaveRaiseException(
        'TPascalParserTool.BuildSubTreeForClass:'
       +' class/object keyword expected, but '+GetAtom+' found');
  end;
  
var OldPhase: integer;
begin
  if (ClassNode<>nil)
  and ((ClassNode.FirstChild<>nil)
    or ((ClassNode.SubDesc and ctnsNeedJITParsing)=0))
  then
    // class already parsed
    exit;
  OldPhase:=CurrentPhase;
  CurrentPhase:=CodeToolPhaseParse;
  try
    if ClassNode=nil then
      RaiseClassNodeNil;
    if ClassNode.Desc<>ctnClass then
      RaiseClassDescInvalid;
    // set CursorPos after class head
    MoveCursorToNodeStart(ClassNode);
    // parse
    //   - inheritage
    //   - class sections (public, published, private, protected)
    //   - methods (procedures, functions, constructors, destructors)

    // first parse the inheritage
    // read the "class"/"object" keyword
    ReadNextAtom;
    if UpAtomIs('PACKED') then ReadNextAtom;
    if (not UpAtomIs('CLASS')) and (not UpAtomIs('OBJECT')) then
      RaiseClassKeyWordExpected;
    ReadNextAtom;
    if CurPos.Flag=cafRoundBracketOpen then
      // read inheritage
      ReadTilBracketClose(true)
    else
      UndoReadNextAtom;
    // clear the last atoms
    LastAtoms.Clear;
    // start the first class section (always published)
    CreateChildNode;
    CurNode.Desc:=ctnClassPublished;
    CurNode.StartPos:=CurPos.EndPos; // behind 'class'
    ReadNextAtom;
    if CurPos.Flag=cafEdgedBracketOpen then
      ReadGUID;
    // parse till "end" of class/object
    CurKeyWordFuncList:=InnerClassKeyWordFuncList;
    try
      repeat
        if CurPos.StartPos>=ClassNode.EndPos then break;
        if not DoAtom then break;
        ReadNextAtom;
      until false;
      // end last class section (public, private, ...)
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode;
    finally
      CurKeyWordFuncList:=DefaultKeyWordFuncList;
    end;
    ClassNode.SubDesc:=ClassNode.SubDesc and (not ctnsNeedJITParsing);
  finally
    CurrentPhase:=OldPhase;
  end;
end;

procedure TPascalParserTool.BuildSubTreeForBeginBlock(BeginNode: TCodeTreeNode);
// reparse a quick parsed begin..end block and build the child nodes
//   create nodes for 'with' and 'case' statements

  procedure RaiseBeginExpected;
  begin
    SaveRaiseException(
       'TPascalParserTool.BuildSubTreeForBeginBlock: begin expected, but '
       +GetAtom+' found');
  end;

var MaxPos, OldPhase: integer;
begin
  OldPhase:=CurrentPhase;
  CurrentPhase:=CodeToolPhaseParse;
  try
    if BeginNode=nil then
      SaveRaiseException(
         'TPascalParserTool.BuildSubTreeForBeginBlock: BeginNode=nil');
    if BeginNode.Desc<>ctnBeginBlock then
      SaveRaiseException(
         'TPascalParserTool.BuildSubTreeForBeginBlock: BeginNode.Desc='
         +BeginNode.DescAsString);
    if (BeginNode.FirstChild<>nil)
    or ((BeginNode.SubDesc and ctnsNeedJITParsing)=0) then
      // block already parsed
      exit;
    // set CursorPos on 'begin'
    MoveCursorToNodeStart(BeginNode);
    ReadNextAtom;
    if CurPos.Flag<>cafBEGIN then
      RaiseBeginExpected;
    if BeginNode.EndPos<SrcLen then
      Maxpos:=BeginNode.EndPos
    else
      MaxPos:=SrcLen;
    repeat
      ReadNextAtom;
      if UpAtomIs('WITH') then
        ReadWithStatement(true,true);
      if UpAtomIs('CASE') then begin
        // ToDo
      end;
    until (CurPos.StartPos>=MaxPos);
    BeginNode.SubDesc:=ctnNone;
  finally
    CurrentPhase:=OldPhase;
  end;
end;

function TPascalParserTool.GetSourceType: TCodeTreeNodeDesc;
begin
  if Tree.Root<>nil then
    Result:=Tree.Root.Desc
  else
    Result:=ctnNone;
end;

function TPascalParserTool.KeyWordFuncClassReadTilEnd: boolean;
// read til atom after next 'end'
begin
  repeat
    ReadNextAtom;
  until (CurPos.StartPos>SrcLen) or (CurPos.Flag=cafEND);
  ReadNextAtom;
  Result:=(CurPos.StartPos<SrcLen);
end;

function TPascalParserTool.KeyWordFuncClassIdentifier: boolean;
{ parse class variable

  examples:
    Name: TypeName;
    Name: UnitName.TypeName;
    i, j: integer;
    MyArray: array of array[EnumType] of array [Range] of TypeName;
    MyRecord: record
              i: packed record
                   j: integer;
                   k: record end;
                   case integer of
                     0: (a: integer);
                     1,2,3: (b: array[char] of char; c: char);
                     3: ( d: record
                               case byte of
                                 10: (i: integer; );
                                 11: (y: byte);
                             end;
                 end;
            end;
    MyPointer: ^integer;
    MyEnum: (MyEnumm1, MyEnumm2 := 2, MyEnummy3);
    MySet: set of (MyEnummy4 := 4 , MyEnummy5);
    MyRange: 3..5;
}
begin
  // create variable definition node
  CreateChildNode;
  CurNode.Desc:=ctnVarDefinition;
  ReadNextAtom;
  while CurPos.Flag=cafComma do begin
    // end variable definition
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode;
    // read next variable name
    ReadNextAtom;
    AtomIsIdentifier(true);
    // create variable definition node
    CreateChildNode;
    CurNode.Desc:=ctnVarDefinition;
    ReadNextAtom;
  end;
  if CurPos.Flag<>cafColon then
    RaiseCharExpectedButAtomFound(':');
  // read type
  ReadVariableType;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeClass: boolean;
// class and object as type are not allowed, because they would have no name
begin
  SaveRaiseExceptionFmt(ctsAnoymDefinitionsAreNotAllowed,[GetAtom]);
  Result:=false;
end;

function TPascalParserTool.KeyWordFuncClassVarTypePacked: boolean;
// 'packed' record
begin
  ReadNextAtom;
  if CurPos.Flag=cafRECORD then
    Result:=KeyWordFuncClassVarTypeRecord
  else begin
    RaiseStringExpectedButAtomFound('"record"');
    Result:=true;
  end;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeRecord: boolean;
{ read variable type 'record'

  examples:
    record
      i: packed record
           j: integer;
           k: record end;
           case integer of
             0: (a: integer);
             1,2,3: (b: array[char] of char; c: char);
             3: ( d: record
                       case byte of
                         10: (i: integer; );
                         11: (y: byte);
                     end;
         end;
    end;
}
var Level: integer;
begin
  Level:=1;
  while (CurPos.StartPos<=SrcLen) and (Level>0) do begin
    DoProgress;
    ReadNextAtom;
    if CurPos.Flag=cafRECORD then inc(Level)
    else if (CurPos.Flag=cafEND) then dec(Level);
  end;
  if CurPos.StartPos>SrcLen then
    SaveRaiseException(ctsEndForRecordNotFound);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeArray: boolean;
{ read variable type 'array'

  examples:
    array of array[EnumType] of array [Range] of TypeName;
}
begin
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    // array[Range]
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseCharExpectedButAtomFound('[');
  ReadNextAtom;
  Result:=ClassVarTypeKeyWordFuncList.DoItUpperCase(UpperSrc,
    CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncClassVarTypeSet: boolean;
{ read variable type 'set of'

  examples:
    set of Name
    set of (MyEnummy4 := 4 , MyEnummy5);
}
begin
  CreateChildNode;
  CurNode.Desc:=ctnSetType;
  ReadNextAtom;
  if not UpAtomIs('OF') then
    RaiseStringExpectedButAtomFound('"of"');
  ReadNextAtom;
  if CurPos.StartPos>SrcLen then
    SaveRaiseException(ctsMissingEnumList);
  if IsIdentStartChar[Src[CurPos.StartPos]] then
    // set of identifier
  else if CurPos.Flag=cafRoundBracketOpen then
    // set of ()
    ReadTilBracketClose(true);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeProc: boolean;
{ read variable type 'procedure ...' or 'function ... : ...'

  examples:
    procedure
    function : integer;
    procedure (a: char) of object;
}
var IsFunction, HasForwardModifier: boolean;
  ParseAttr: TParseProcHeadAttributes;
begin
//writeln('[TPascalParserTool.KeyWordFuncClassVarTypeProc]');
  IsFunction:=UpAtomIs('FUNCTION');
  ReadNextAtom;
  HasForwardModifier:=false;
  ParseAttr:=[pphIsMethod,pphIsType];
  if IsFunction then Include(ParseAttr,pphIsFunction);
  ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeIdent: boolean;
// read variable type <identfier>
begin
  if CurPos.StartPos>SrcLen then
    SaveRaiseException(ctsMissingTypeIdentifier);
  if IsIdentStartChar[Src[CurPos.StartPos]] then
    // identifier
  else
    SaveRaiseException(ctsMissingTypeIdentifier);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassSection: boolean;
// change section in a class (public, private, protected, published)
begin
  // end last section
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  // start new section
  CreateChildNode;
  if UpAtomIs('PUBLIC') then
    CurNode.Desc:=ctnClassPublic
  else if UpAtomIs('PRIVATE') then
    CurNode.Desc:=ctnClassPrivate
  else if UpAtomIs('PROTECTED') then
    CurNode.Desc:=ctnClassProtected
  else
    CurNode.Desc:=ctnClassPublished;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassMethod: boolean;
{ parse class method

 examples:
   procedure ProcName;  virtual; abstract;
   function FuncName(Parameter1: Type1; Parameter2: Type2): ResultType;
   constructor Create;
   destructor Destroy;  override;
   class function X: integer;
   static function X: integer;
   function Intf.Method = ImplementingMethodName;

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline,
   compilerproc

 proc specifiers with parameters:
   message <id or number>
}
var IsFunction, HasForwardModifier: boolean;
  ParseAttr: TParseProcHeadAttributes;
begin
  HasForwardModifier:=false;
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProcedure;
  // read method keyword
  if UpAtomIs('CLASS') or (UpAtomIs('STATIC')) then begin
    ReadNextAtom;
    if (not UpAtomIs('PROCEDURE')) and (not UpAtomIs('FUNCTION')) then begin
      RaiseStringExpectedButAtomFound(ctsProcedureOrFunction);
    end;
  end;
  IsFunction:=UpAtomIs('FUNCTION');
  // read procedure head
  // read name
  ReadNextAtom;
  AtomIsIdentifier(true);
  // create node for procedure head
  CreateChildNode;
  CurNode.Desc:=ctnProcedureHead;
  CurNode.SubDesc:=ctnsNeedJITParsing;
  ReadNextAtom;
  if (CurPos.Flag<>cafPoint) then begin
    // read rest
    ParseAttr:=[pphIsMethod];
    if IsFunction then Include(ParseAttr,pphIsFunction);
    ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
  end else begin
    // Method resolution clause (e.g. function Intf.Method = MethodName)
    CurNode.Parent.Desc:=ctnMethodMap;
    // read Method name of interface
    ReadNextAtom;
    AtomIsIdentifier(true);
    // read '='
    ReadNextAtomIsChar('=');
    // read implementing method name
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      UndoReadNextAtom;
  end;
  // close procedure header
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  // close procedure / method map
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.ReadParamList(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
var CloseBracket: char;
  Desc: TCodeTreeNodeDesc;
  Node: TCodeTreeNode;
  
  procedure ReadPrefixModifier;
  begin
    // read parameter prefix modifier
    if (UpAtomIs('VAR')) or (UpAtomIs('CONST')) or (UpAtomIs('OUT')) then begin
      Desc:=ctnVarDefinition;
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(phpWithVarModifiers in Attr,Attr);
    end else
      Desc:=ctnVarDefinition;
  end;
  
  procedure ReadDefaultValue;
  begin
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(phpWithDefaultValues in Attr,Attr);
    ReadConstant(ExceptionOnError,
      Extract and (phpWithDefaultValues in Attr),Attr);
    if (phpCreateNodes in Attr) then begin
      Node:=CurNode;
      Node.SubDesc:=Node.SubDesc+ctnsHasDefaultValue;
      Node:=Node.PriorBrother;
      while (Node<>nil) and (Node.FirstChild=nil) do begin
        Node.SubDesc:=Node.SubDesc+ctnsHasDefaultValue;
        Node:=Node.PriorBrother;
      end;
    end;
  end;

begin
  Result:=false;
  if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then begin
    if CurPos.Flag=cafRoundBracketOpen then
      CloseBracket:=')'
    else
      CloseBracket:=']';
    if (phpCreateNodes in Attr) then begin
      CreateChildNode;
      CurNode.Desc:=ctnParameterList;
    end;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutBrackets in Attr),Attr);
  end else
    CloseBracket:=#0;
  if not (CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose]) then begin
    repeat
      ReadPrefixModifier;
      // read parameter name(s)
      repeat
        if not AtomIsIdentifier(ExceptionOnError) then exit;
        if (phpCreateNodes in Attr) then begin
          CreateChildNode;
          CurNode.Desc:=Desc;
        end;
        if not Extract then
          ReadNextAtom
        else
          ExtractNextAtom(phpWithParameterNames in Attr,Attr);
        if CurPos.Flag<>cafComma then
          break
        else begin
          if (phpCreateNodes in Attr) then begin
            CurNode.EndPos:=LastAtoms.GetValueAt(0).EndPos;
            EndChildNode;
          end;
          if not Extract then
            ReadNextAtom
          else
            ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
        end;
      until false;
      // read type
      if CurPos.Flag=cafColon then begin
        if not Extract then
          ReadNextAtom
        else
          ExtractNextAtom([phpWithoutParamList,phpWithoutParamTypes]*Attr=[],
                          Attr);
        if not ReadParamType(ExceptionOnError,Extract,Attr) then exit;
        if CurPos.Flag=cafEqual then begin
          // read default value
          ReadDefaultValue;
        end;
      end else begin
        // no type -> variant
        if (phpCreateNodes in Attr) then begin
          CreateChildNode;
          CurNode.Desc:=ctnVariantType;
          CurNode.EndPos:=CurNode.StartPos;
          EndChildNode;
        end;
      end;
      if (phpCreateNodes in Attr) then begin
        CurNode.EndPos:=CurPos.EndPos;
        EndChildNode;
      end;
      // read next parameter
      if (CurPos.StartPos>SrcLen) then
        if ExceptionOnError then
          RaiseCharExpectedButAtomFound(CloseBracket)
        else exit;
      if (CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose]) then
        break;
      if (CurPos.Flag<>cafSemicolon) then
        if ExceptionOnError then
          RaiseCharExpectedButAtomFound(CloseBracket)
        else exit;
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
    until false;
  end;
  if (CloseBracket<>#0) then begin
    if Src[CurPos.StartPos]<>CloseBracket then begin
      if ExceptionOnError then
        RaiseCharExpectedButAtomFound(CloseBracket)
      else
        exit;
    end;
    if (phpCreateNodes in Attr) then begin
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutBrackets in Attr),Attr);
  end;
  Result:=true;
end;

function TPascalParserTool.ReadParamType(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
var
  copying: boolean;
begin
  copying:=[phpWithoutParamList,phpWithoutParamTypes]*Attr=[];
  Result:=false;
  if CurPos.Flag in AllCommonAtomWords then begin
    if UpAtomIs('ARRAY') then begin
      if (phpCreateNodes in Attr) then begin
        CreateChildNode;
        CurNode.Desc:=ctnArrayType;
      end;
      if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      if not UpAtomIs('OF') then
        if ExceptionOnError then
          RaiseStringExpectedButAtomFound('"of"')
        else exit;
      if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      if UpAtomIs('CONST') then begin
        if (phpCreateNodes in Attr) then begin
          CreateChildNode;
          CurNode.Desc:=ctnOfConstType;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
        end;
        if not Extract then
          ReadNextAtom
        else
          ExtractNextAtom(copying,Attr);
        Result:=true;
        exit;
      end;
    end;
    if not AtomIsIdentifier(ExceptionOnError) then exit;
    if (phpCreateNodes in Attr) then begin
      CreateChildNode;
      CurNode.Desc:=ctnIdentifier;
      CurNode.EndPos:=CurPos.EndPos;
    end;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(copying,Attr);
    if CurPos.Flag=cafPoint then begin
      //  first identifier was unitname -> read '.' + identifier
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(copying,Attr);
      if not AtomIsIdentifier(ExceptionOnError) then exit;
      CurNode.EndPos:=CurPos.EndPos;
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(copying,Attr);
    end;
    if (phpCreateNodes in Attr) then begin
      EndChildNode;
    end;
  end else begin
    if ExceptionOnError then
      RaiseStringExpectedButAtomFound(ctsIdentifier)
    else exit;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadTilProcedureHeadEnd(
  ParseAttr: TParseProcHeadAttributes;
  var HasForwardModifier: boolean): boolean;
{ parse parameter list, result type, of object, method specifiers

  IsMethod: true if parsing in a class/object
  IsFunction: 'function'
  IsType: parsing type definition. e.g. 'Event: procedure of object'


 examples:
   procedure ProcName;  virtual; abstract;
   function FuncName(Parameter1: Type1; Parameter2: Type2): ResultType;
   constructor Create;
   destructor Destroy;  override;
   class function X: integer;
   function QWidget_mouseGrabber(): QWidgetH; cdecl;
   procedure Intf.Method = ImplementingMethodName;

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline

 proc specifiers with parameters:
   message <id or number>;
   external;
   external <id>;
   external name <id>;
   external <id or number> name <id>;
   external <id or number> index <id>;
   [alias: <string constant>]
   [external name <string constant>]
}

  procedure RaiseKeyWordExampleExpected;
  begin
    SaveRaiseExceptionFmt(
      ctsKeywordExampleExpectedButAtomFound,['alias',GetAtom]);
  end;

var IsSpecifier: boolean;
  Attr: TProcHeadAttributes;
begin
  //writeln('[TPascalParserTool.ReadTilProcedureHeadEnd] ',
  //'Method=',IsMethod,', Function=',IsFunction,', Type=',IsType);
  Result:=true;
  HasForwardModifier:=false;
  if CurPos.Flag=cafRoundBracketOpen then begin
    Attr:=[];
    if pphCreateNodes in ParseAttr then
      Include(Attr,phpCreateNodes);
    ReadParamList(true,false,Attr);
  end;
  if (pphIsOperator in ParseAttr) and (CurPos.Flag<>cafColon) then begin
    // read operator result identifier
    AtomIsIdentifier(true);
    if (pphCreateNodes in ParseAttr) then begin
      CreateChildNode;
      CurNode.Desc:=ctnVarDefinition;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    ReadNextAtom;
  end;
  if ([pphIsFunction,pphIsOperator]*ParseAttr<>[]) then begin
    // read function result type
    if CurPos.Flag=cafColon then begin
      ReadNextAtom;
      AtomIsIdentifier(true);
      if (pphCreateNodes in ParseAttr) then begin
        CreateChildNode;
        CurNode.Desc:=ctnIdentifier;
        CurNode.EndPos:=CurPos.EndPos;
        EndChildNode;
      end;
      ReadNextAtom;
    end else begin
      if (Scanner.CompilerMode<>cmDelphi) then
        RaiseCharExpectedButAtomFound(':')
      else begin
        // Delphi Mode
        if CurPos.Flag=cafEqual then begin
          // read interface alias
          ReadNextAtom;
          AtomIsIdentifier(true);
          ReadNextAtom;
        end;
      end;
    end;
  end;
  if UpAtomIs('OF') then begin
    // read 'of object'
    if not (pphIsType in ParseAttr) then
      RaiseCharExpectedButAtomFound(';');
    ReadNextAtom;
    if not UpAtomIs('OBJECT') then
      RaiseStringExpectedButAtomFound('"object"');
    ReadNextAtom;
  end;
  // read procedures/method specifiers
  if CurPos.Flag=cafEND then begin
    UndoReadNextAtom;
    exit;
  end;
  if CurPos.Flag=cafSemicolon then
    ReadNextAtom;
  if (CurPos.StartPos>SrcLen) then
    SaveRaiseException(ctsSemicolonNotFound);
  repeat
    if (pphIsMethod in ParseAttr) then
      IsSpecifier:=IsKeyWordMethodSpecifier.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    else
      IsSpecifier:=IsKeyWordProcedureSpecifier.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
    if IsSpecifier then begin
      // read specifier
      if UpAtomIs('MESSAGE') then begin
        ReadNextAtom;
        ReadConstant(true,false,[]);
      end else if UpAtomIs('EXTERNAL') then begin
        HasForwardModifier:=true;
        ReadNextAtom;
        if CurPos.Flag<>cafSemicolon then begin
          if not UpAtomIs('NAME') then
            ReadConstant(true,false,[]);
          if UpAtomIs('NAME') or UpAtomIs('INDEX') then begin
            ReadNextAtom;
            ReadConstant(true,false,[]);
          end;
        end;
      end else if CurPos.Flag=cafEdgedBracketOpen then begin
        // read assembler alias   [public,alias: 'alternative name'],
        // internproc, external
        repeat
          ReadNextAtom;
          if not (CurPos.Flag in AllCommonAtomWords) then
            RaiseStringExpectedButAtomFound(ctsKeyword);
          if not IsKeyWordProcedureBracketSpecifier.DoItUppercase(UpperSrc,
            CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
          then
            RaiseKeyWordExampleExpected;
          if UpAtomIs('INTERNPROC') then
            HasForwardModifier:=true;
          if UpAtomIs('EXTERNAL') then begin
            HasForwardModifier:=true;
            ReadNextAtom;
            if not (CurPos.Flag in [cafComma,cafEdgedBracketClose]) then begin
              if not UpAtomIs('NAME') then
                ReadConstant(true,false,[]);
              if UpAtomIs('NAME') or UpAtomIs('INDEX') then begin
                ReadNextAtom;
                ReadConstant(true,false,[]);
              end;
            end;
          end else
            ReadNextAtom;
          if CurPos.Flag in [cafColon,cafEdgedBracketClose] then
            break;
          if CurPos.Flag<>cafComma then
            RaiseCharExpectedButAtomFound(']');
        until false;
        if CurPos.Flag=cafColon then begin
          ReadNextAtom;
          if (not AtomIsStringConstant) and (not AtomIsIdentifier(false)) then
            RaiseStringExpectedButAtomFound(ctsStringConstant);
          ReadConstant(true,false,[]);
        end;
        if CurPos.Flag<>cafEdgedBracketClose then
          RaiseCharExpectedButAtomFound(']');
        ReadNextAtom;
        if CurPos.Flag=cafEND then begin
          UndoReadNextAtom;
          exit;
        end;
      end else begin
        // read specifier without parameters
        if UpAtomIs('FORWARD') then HasForwardModifier:=true;
        ReadNextAtom;
        if CurPos.Flag=cafEND then begin
          UndoReadNextAtom;
          exit;
        end;
      end;
      if CurPos.Flag<>cafSemicolon then begin
        if (Scanner.CompilerMode<>cmDelphi) then
          RaiseCharExpectedButAtomFound(';');
        // Delphi allows procs without ending semicolon
        UndoReadNextAtom; // unread unknown atom
        if CurPos.Flag=cafSemicolon then
          UndoReadNextAtom; // unread semicolon
        break;
      end;
    end else begin
      // current atom does not belong to procedure/method declaration
      UndoReadNextAtom; // unread unknown atom
      if CurPos.Flag=cafSemicolon then
        UndoReadNextAtom; // unread semicolon
      break;
    end;
    ReadNextAtom;
  until false;
end;

function TPascalParserTool.ReadConstant(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
// after reading, the CurPos will be on the atom after the constant
var
  BracketType: TCommonAtomFlag;
  c: char;
begin
  Result:=false;
  if CurPos.Flag in AllCommonAtomWords then begin
    // word (identifier or keyword)
    if AtomIsKeyWord and (not IsKeyWordInConstAllowed.DoItUppercase(UpperSrc,
            CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)) then begin
      if ExceptionOnError then
        RaiseUnexpectedKeyWord
      else exit;
    end;
    if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    if WordIsTermOperator.DoItUpperCase(UpperSrc,
         CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      // identifier + operator + ?
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      Result:=ReadConstant(ExceptionOnError,Extract,Attr);
      exit;
    end else if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
    begin
      // type cast or constant array
      BracketType:=CurPos.Flag;
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
      if (BracketType=cafRoundBracketOpen)
      and (CurPos.Flag<>cafRoundBracketClose) then
        if ExceptionOnError then
          RaiseCharExpectedButAtomFound('(')
        else exit;
      if (BracketType=cafEdgedBracketOpen)
      and (CurPos.Flag<>cafEdgedBracketClose) then
        if ExceptionOnError then
          RaiseCharExpectedButAtomFound('[')
        else exit;
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    end;
  end else if AtomIsNumber or AtomIsStringConstant then begin
    // number or '...' or #...
    if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    if WordIsTermOperator.DoItUpperCase(UpperSrc,
         CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      // number + operator + ?
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      Result:=ReadConstant(ExceptionOnError,Extract,Attr);
      exit;
    end;
  end else begin
    if CurPos.EndPos-CurPos.StartPos=1 then begin
      c:=Src[CurPos.StartPos];
      case c of
        '(':
          begin
            // open bracket + ? + close bracket
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
            if (c='(') and (CurPos.Flag<>cafRoundBracketClose) then
              if ExceptionOnError then
                RaiseCharExpectedButAtomFound(')')
              else exit;
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if WordIsTermOperator.DoItUpperCase(UpperSrc,
                 CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
            then begin
              // open bracket + ? + close bracket + operator + ?
              if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
              Result:=ReadConstant(ExceptionOnError,Extract,Attr);
              exit;
            end;
          end;
        '[':
          begin
            // open bracket + ? + close bracket
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            repeat
              if (CurPos.Flag=cafEdgedBracketClose) then break;
              // read
              if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
              if (CurPos.Flag=cafComma) or AtomIs('..') then begin
                // continue
                if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
              end else if (CurPos.Flag<>cafEdgedBracketClose) then begin
                if ExceptionOnError then
                  RaiseCharExpectedButAtomFound(']')
                else exit;
              end;
            until false;
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if WordIsTermOperator.DoItUpperCase(UpperSrc,
                 CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
            then begin
              // open bracket + ? + close bracket + operator + ?
              if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
              Result:=ReadConstant(ExceptionOnError,Extract,Attr);
              exit;
            end;
          end;
        '+','-':
          begin
            // sign
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
          end;
      else
        if ExceptionOnError then
          RaiseStringExpectedButAtomFound(ctsConstant)
        else exit;
      end;
    end else
      // syntax error
      if ExceptionOnError then
        RaiseStringExpectedButAtomFound(ctsConstant)
      else exit;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadUsesSection(
  ExceptionOnError: boolean): boolean;
{ parse uses section

  examples:
    uses name1, name2 in '', name3;

}
begin
  CreateChildNode;
  CurNode.Desc:=ctnUsesSection;
  repeat
    ReadNextAtom;  // read name
    if CurPos.Flag=cafSemicolon then break;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        if ExceptionOnError then
          RaiseStringExpectedButAtomFound(ctsStringConstant)
        else exit;
      ReadNextAtom;
    end;
    if CurPos.Flag=cafSemicolon then break;
    if CurPos.Flag<>cafComma then
      if ExceptionOnError then
        RaiseCharExpectedButAtomFound(';')
      else exit;
  until (CurPos.StartPos>SrcLen);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.ReadSubRange(ExceptionOnError: boolean): boolean;
{ parse subrange till ',' ';' ':' ']' or ')'

  examples:
    number..number
    identifier
    Low(identifier)..High(identifier)
    Pred(identifier)..Succ(identfier)
}
var RangeOpFound: boolean;
begin
  RangeOpFound:=false;
  repeat
    if CurPos.Flag in [cafSemicolon,cafColon,cafComma,cafRoundBracketClose,
                       cafEdgedBracketClose]
    then
      break;
    if AtomIs('..') then begin
      if RangeOpFound then
        RaiseCharExpectedButAtomFound(';');
      RangeOpFound:=true;
    end else if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
      ReadTilBracketClose(ExceptionOnError);
    ReadNextAtom;
  until false;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassProperty: boolean;
{ parse class/object property

 examples:
   property Visible;
   property Count: integer;
   property Color: TColor read FColor write SetColor;
   property Items[Index1, Index2: integer]: integer read GetItems; default;
   property X: integer index 1 read GetCoords write SetCoords stored IsStored;
   property Col8: ICol8 read FCol8 write FCol8 implements ICol8, IColor;

 property specifiers without parameters:
   default, nodefault

 property specifiers with parameters:
   index <id or number>, read <id>, write <id>, stored <id>, default <constant>,
   implements <id>[,<id>...]
}

  procedure RaiseSemicolonAfterPropSpecMissing(const s: string);
  begin
    SaveRaiseExceptionFmt(ctsSemicolonAfterPropSpecMissing,[s,GetAtom]);
  end;

begin
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProperty;
  // read property Name
  ReadNextAtom;
  AtomIsIdentifier(true);
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    // read parameter list
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  while (CurPos.StartPos<=SrcLen) and (CurPos.Flag<>cafSemicolon) do
    ReadNextAtom;
  ReadNextAtom;
  if UpAtomIs('DEFAULT') then begin
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      RaiseSemicolonAfterPropSpecMissing('default');
  end else if UpAtomIs('NODEFAULT') then begin
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      RaiseSemicolonAfterPropSpecMissing('nodefault');
  end else
    UndoReadNextAtom;
  // close property
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.DoAtom: boolean;
begin
//writeln('[TPascalParserTool.DoAtom] A ',HexStr(Cardinal(CurKeyWordFuncList),8));
  DoProgress;
  if (CurPos.StartPos<=SrcLen) and (CurPos.EndPos>CurPos.StartPos) then begin
    if IsIdentStartChar[Src[CurPos.StartPos]] then
      Result:=CurKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
                                      CurPos.EndPos-CurPos.StartPos)
    else begin
      if Src[CurPos.StartPos] in ['(','['] then
        ReadTilBracketClose(true);
      Result:=true;
    end;
  end else
    Result:=false;
end;

function TPascalParserTool.ExtractNode(ANode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (ANode=nil) or (ANode.StartPos<1) then exit;
  InitExtraction;
  // reparse the clean source
  MoveCursorToNodeStart(ANode);
  while (ANode.EndPos>CurPos.StartPos)
  and (CurPos.StartPos<=SrcLen) do
    ExtractNextAtom(true,Attr);
  // copy memorystream to Result string
  Result:=GetExtraction;
end;

function TPascalParserTool.KeyWordFuncSection: boolean;
// parse section keywords (program, unit, interface, implementation, ...)

  procedure RaiseUnexpectedSectionKeyWord;
  begin
    SaveRaiseExceptionFmt(ctsUnknownSectionKeyword,[GetAtom]);
  end;

begin
  case CurSection of
   ctnInterface, ctnProgram, ctnPackage, ctnLibrary, ctnUnit:
    begin
      if not ((CurSection=ctnInterface) and UpAtomIs('IMPLEMENTATION')) then
        RaiseUnexpectedKeyWord;
      // close interface section node
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode;
      ImplementationSectionFound:=true;
      // start implementation section node
      CreateChildNode;
      CurNode.Desc:=ctnImplementation;
      CurSection:=ctnImplementation;
      ReadNextAtom;
      if UpAtomIs('USES') then
        ReadUsesSection(true);
      UndoReadNextAtom;
      Result:=true;
    end;
   ctnImplementation:
    begin
      if not (UpAtomIs('INITIALIZATION') or UpAtomIs('FINALIZATION')) then
        RaiseUnexpectedKeyWord;
      // close implementation section node
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode;
      // start initialization / finalization section node
      CreateChildNode;
      if UpAtomIs('INITIALIZATION') then begin
        CurNode.Desc:=ctnInitialization;
      end else
        CurNode.Desc:=ctnFinalization;
      CurSection:=CurNode.Desc;
      repeat
        DoProgress;
        ReadNextAtom;
        if (CurSection=ctnInitialization) and UpAtomIs('FINALIZATION') then
        begin
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
          CreateChildNode;
          CurNode.Desc:=ctnFinalization;
          CurSection:=CurNode.Desc;
        end else if EndKeyWordFuncList.DoItUppercase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos) then
        begin
          ReadTilBlockEnd(false,false);
        end else if CurPos.Flag=cafEND then begin
          Result:=KeyWordFuncEndPoint;
          break;
        end;
      until (CurPos.StartPos>SrcLen);
      Result:=true;
    end;
  else
    begin
      RaiseUnexpectedSectionKeyWord;
      Result:=false;
    end;
  end;
end;

function TPascalParserTool.KeyWordFuncEndPoint: boolean;
// keyword 'end' or '.'  (source end.)
begin
  if CurPos.Flag=cafPoint then begin
    if not LastUpAtomIs(0,'END') then
      RaiseIllegalQualifier;
    UndoReadNextAtom;
    if CurNode.Desc in [ctnInterface] then
      RaiseStringExpectedButAtomFound('"implementation"');
    if not (CurNode.Desc in [ctnImplementation,ctnInitialization,
      ctnFinalization,ctnProgram])
    then begin
      ReadNextAtom;
      SaveRaiseException(ctsUnexpectedEndOfSource+' 1');
    end;
  end else if CurPos.Flag=cafEND then begin
    if LastAtomIs(0,'@') then
      RaiseStringExpectedButAtomFound(ctsIdentifier);
    if LastAtomIs(0,'@@') then begin
      // for Delphi compatibility @@end is allowed
      Result:=true;
      exit;
    end;
  end else
    SaveRaiseException('[TPascalParserTool.KeyWordFuncEndPoint] internal error');
  if CurNode.Desc in [ctnImplementation,ctnInterface] then
    CurNode.EndPos:=CurPos.StartPos
  else
    CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  if CurPos.Flag<>cafPoint then
    RaiseCharExpectedButAtomFound('.');
  CurSection:=ctnNone;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncProc: boolean;
// procedure, function, constructor, destructor, operator
var ChildCreated: boolean;
  IsFunction, HasForwardModifier, IsClassProc, IsOperator: boolean;
  ProcNode: TCodeTreeNode;
  ParseAttr: TParseProcHeadAttributes;
begin
  if UpAtomIs('CLASS') then begin
    if CurSection<>ctnImplementation then
      RaiseStringExpectedButAtomFound(ctsIdentifier);
    ReadNextAtom;
    if UpAtomIs('PROCEDURE') or UpAtomIs('FUNCTION') then
      IsClassProc:=true
    else
      RaiseStringExpectedButAtomFound('"procedure"');
  end else
    IsClassProc:=false;
  ChildCreated:=true;
  if ChildCreated then begin
    // create node for procedure
    CreateChildNode;
    if IsClassProc then
      CurNode.StartPos:=LastAtoms.GetValueAt(0).StartPos;
    ProcNode:=CurNode;
    ProcNode.Desc:=ctnProcedure;
    if CurSection=ctnInterface then
      ProcNode.SubDesc:=ctnsForwardDeclaration;
  end;
  IsFunction:=UpAtomIs('FUNCTION');
  IsOperator:=UpAtomIs('OPERATOR');
  ReadNextAtom;// read first atom of head (= name + parameterlist + resulttype;)
  if not IsOperator then AtomIsIdentifier(true);
  if ChildCreated then begin
    // create node for procedure head
    CreateChildNode;
    CurNode.Desc:=ctnProcedureHead;
    CurNode.SubDesc:=ctnsNeedJITParsing;
  end;
  ReadNextAtom;
  if (CurSection<>ctnInterface) and (CurPos.Flag=cafPoint) then begin
    // read procedure name of a class method (the name after the . )
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  // read rest of procedure head
  HasForwardModifier:=false;
  ParseAttr:=[];
  if IsFunction then Include(ParseAttr,pphIsFunction);
  if IsOperator then Include(ParseAttr,pphIsOperator);
  ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
  if ChildCreated then begin
    if HasForwardModifier then
      ProcNode.SubDesc:=ctnsForwardDeclaration;
    // close head
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if ChildCreated and ((ProcNode.SubDesc and ctnsForwardDeclaration)>0) then
  begin
    // close method
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadTilBlockEnd(
  StopOnBlockMiddlePart, CreateNodes: boolean): boolean;
// after reading cursor will be on the keyword ending the block (e.g. 'end')
var BlockType: TEndBlockType;
  TryType: TTryType;
  BlockStartPos: integer;
  
  procedure SaveRaiseExceptionWithBlockStartHint(const AMessage: string);
  var CaretXY: TCodeXYPosition;
  begin
    if (CleanPosToCaret(BlockStartPos,CaretXY))
    and (CaretXY.Code<>nil) then begin
      if CaretXY.Code=TCodeBuffer(Scanner.MainCode) then
        SaveRaiseException(AMessage+ctsPointStartAt
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')')
      else
        SaveRaiseException(AMessage+ctsPointStartAt
          +TCodeBuffer(CaretXY.Code).Filename
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')');
    end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
      SaveRaiseException(AMessage);
    end;
  end;
  
  procedure RaiseUnknownBlockType;
  begin
    SaveRaiseException('internal codetool error in '
      +'TPascalParserTool.ReadTilBlockEnd: unkown block type: '+GetAtom);
  end;
  
  procedure RaiseStrExpectedWithBlockStartHint(const Msg: string);
  begin
    SaveRaiseExceptionWithBlockStartHint(
      Format(ctsStrExpectedButAtomFound,[Msg,GetAtom]));
  end;
  
  procedure RaiseUnexpectedKeywordInAsmBlock;
  begin
    SaveRaiseExceptionFmt(ctsUnexpectedKeywordInAsmBlock,[GetAtom]);
  end;
  
begin
  Result:=true;
  TryType:=ttNone;
  if CurPos.Flag=cafBEGIN then
    BlockType:=ebtBegin
  else if UpAtomIs('REPEAT') then
    BlockType:=ebtRepeat
  else if UpAtomIs('TRY') then
    BlockType:=ebtTry
  else if UpAtomIs('CASE') then
    BlockType:=ebtCase
  else if UpAtomIs('ASM') then
    BlockType:=ebtAsm
  else if CurPos.Flag=cafRECORD then
    BlockType:=ebtRecord
  else
    RaiseUnknownBlockType;
  BlockStartPos:=CurPos.StartPos;
  repeat
    DoProgress;
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen) then
      SaveRaiseExceptionWithBlockStartHint(ctsUnexpectedEndOfSource);
      
    if not (CurPos.Flag in AllCommonAtomWords) then continue;
    
    if (CurPos.Flag=cafEND) then begin
      if BlockType=ebtRepeat then
        RaiseStrExpectedWithBlockStartHint('"until"');
      if (BlockType=ebtTry) and (TryType=ttNone) then
        RaiseStrExpectedWithBlockStartHint('"finally"');
      ReadNextAtom;
      if (CurPos.Flag=cafPoint) and (BlockType<>ebtBegin) then begin
        RaiseCharExpectedButAtomFound(';');
      end;
      UndoReadNextAtom;
      break;
    end else if EndKeyWordFuncList.DoItUppercase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos)
      or UpAtomIs('REPEAT') then
    begin
      if BlockType=ebtAsm then
        RaiseUnexpectedKeywordInAsmBlock;
      if (BlockType<>ebtRecord) or (not UpAtomIs('CASE')) then
        ReadTilBlockEnd(false,CreateNodes);
    end else if UpAtomIs('UNTIL') then begin
      if BlockType=ebtRepeat then
        break;
      RaiseStrExpectedWithBlockStartHint('"end"');
    end else if UpAtomIs('FINALLY') then begin
      if (BlockType=ebtTry) and (TryType=ttNone) then begin
        if StopOnBlockMiddlePart then break;
        TryType:=ttFinally;
      end else
        RaiseStrExpectedWithBlockStartHint('"end"');
    end else if UpAtomIs('EXCEPT') then begin
      if (BlockType=ebtTry) and (TryType=ttNone) then begin
        if StopOnBlockMiddlePart then break;
        TryType:=ttExcept;
      end else
        RaiseStrExpectedWithBlockStartHint('"end"');
    end else if CreateNodes and UpAtomIs('WITH') then begin
      ReadWithStatement(true,CreateNodes);
    end else begin
      // check for unexpected keywords
      case BlockType of
      
      ebtBegin,ebtAsm,ebtTry,ebtCase,ebtRepeat:
        if UnexpectedKeyWordInBeginBlock.DoItUppercase(UpperSrc,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
        then
          RaiseUnexpectedKeyWord;
          
      end;
    end;
  until false;
end;

function TPascalParserTool.ReadBackTilBlockEnd(
  StopOnBlockMiddlePart: boolean): boolean;
// read begin..end, try..finally, case..end, repeat..until, asm..end blocks
// backwards
var BlockType: TEndBlockType;

  procedure RaiseBlockError;
  begin
    case BlockType of
      ebtBegin:
        SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,['"begin"',GetAtom]);
      ebtTry:
        SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,['"try"',GetAtom]);
      ebtRepeat:
        SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,['"repeat"',GetAtom]);
    else
      SaveRaiseExceptionFmt(ctsUnexpectedKeywordWhileReadingBackwards,[GetAtom]);
    end;
  end;
  
  procedure RaiseUnknownBlockType;
  begin
    SaveRaiseException('internal codetool error in '
      +'TPascalParserTool.ReadBackTilBlockEnd: unkown block type: '+GetAtom);
  end;

var OldAtom: TAtomPosition;
begin
  Result:=true;
  if CurPos.Flag=cafEND then
    BlockType:=ebtBegin
  else if UpAtomIs('UNTIL') then
    BlockType:=ebtRepeat
  else if UpAtomIs('FINALLY') or UpAtomIs('EXCEPT') then
    BlockType:=ebtTry
  else
    RaiseUnknownBlockType;
  repeat
    ReadPriorAtom;
    if (CurPos.StartPos<1) then begin
      SaveRaiseExceptionFmt(ctsWordNotFound,['begin']);
    end else if WordIsBlockKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then
    begin
      if (CurPos.Flag=cafEND) or (UpAtomIs('UNTIL')) then begin
        ReadBackTilBlockEnd(false);
      end else if (CurPos.Flag in [cafBEGIN,cafRECORD]) or UpAtomIs('ASM')
      then begin
        if BlockType=ebtBegin then
          break
        else
          RaiseBlockError;
      end else if UpAtomIs('OBJECT') then begin
        if BlockType=ebtBegin then begin
          // could also be 'of object'
          ReadPriorAtom;
          if not UpAtomIs('OF') then begin
            CurPos:=NextPos;
            NextPos.StartPos:=-1;
            break;
          end;
        end else
          RaiseBlockError;
      end else if UpAtomIs('CLASS') then begin
        ReadNextAtom;
        if UpAtomIs('FUNCTION') or UpAtomIs('PROCEDURE')
        or (CurPos.Flag=cafSemicolon) or UpAtomIs('OF') then
          UndoReadNextAtom
        else begin
          UndoReadNextAtom;
          break;
        end;
      end else if UpAtomIs('CASE') then begin
        // case could also be in a record, then it should not close the block
        if BlockType=ebtBegin then begin
          // check if case in a record
          OldAtom:=CurPos;
          repeat
            ReadPriorAtom;
            if WordIsBlockKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
              CurPos.EndPos-CurPos.StartPos) then
            begin
              if UpAtomIs('CASE') then begin
                // could be another variant record, -> read further ...
              end else if CurPos.Flag=cafRECORD then begin
                // record start found -> the case is a variant record
                // block start found
                break;
              end else begin
                // this is not a variant record
                MoveCursorToCleanPos(OldAtom.StartPos);
                ReadNextAtom;
                break;
              end;
            end;
          until (CurPos.StartPos<1);
          break;
        end else
          RaiseBlockError;
      end else if UpAtomIs('REPEAT') then begin
        if BlockType=ebtRepeat then
          break
        else
          RaiseBlockError;
      end else if UpAtomIs('FINALLY') or UpAtomIs('EXCEPT') then begin
        if BlockType=ebtBegin then begin
          if StopOnBlockMiddlePart then break;
          BlockType:=ebtTry;
        end else
          RaiseBlockError;
      end else if UpAtomIs('TRY') then begin
        if BlockType=ebtTry then
          break
        else
          RaiseBlockError;
      end;
    end;
  until false;
end;

function TPascalParserTool.ReadTilVariableEnd(
  ExceptionOnError: boolean): boolean;
{ Examples:
    A
    A.B^.C[...].D(...).E
    (...).A
    @B
    inherited A
    
}
begin
  while AtomIsChar('@') do
    ReadNextAtom;
  while UpAtomIs('INHERITED') do
    ReadNextAtom;
  Result:=AtomIsIdentifier(false)
          or (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]);
  if not Result then exit;
  repeat
    if AtomIsIdentifier(false) then
      ReadNextAtom;
    repeat
      if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then begin
        Result:=ReadTilBracketClose(ExceptionOnError);
        if not Result then exit;
        ReadNextAtom;
      end else if AtomIsChar('^') then begin
        ReadNextAtom;
      end else
        break;
    until false;
    if (CurPos.Flag=cafPoint) or UpAtomIs('AS') then
      ReadNextAtom
    else
      break;
  until false;
end;

function TPascalParserTool.ReadTilStatementEnd(ExceptionOnError,
  CreateNodes: boolean): boolean;
// after reading the current atom will be on the last atom of the statement
begin
  Result:=false;
  if BlockStatementStartKeyWordFuncList.DoItUppercase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then
  begin
    if not ReadTilBlockEnd(ExceptionOnError,CreateNodes) then exit;
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then UndoReadNextAtom;
  end else if UpAtomIs('WITH') then begin
    if not ReadWithStatement(ExceptionOnError,CreateNodes) then exit;
  end else begin
    // read till semicolon or 'end'
    while (CurPos.Flag<>cafSemicolon) do begin
      DoProgress;
      ReadNextAtom;
      if CurPos.Flag=cafEND then begin
        UndoReadNextAtom;
        break;
      end;
    end;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadWithStatement(ExceptionOnError,
  CreateNodes: boolean): boolean;
var WithVarNode: TCodeTreeNode;
begin
  ReadNextAtom; // read 'with'
  if CreateNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnWithVariable;
  end;
  ReadTilVariableEnd(true);
  while CurPos.Flag=cafComma do begin
    if CreateNodes then
      EndChildNode;
    ReadNextAtom;
    if CreateNodes then begin
      CreateChildNode;
      CurNode.Desc:=ctnWithVariable
    end;
    ReadTilVariableEnd(true);
  end;
  if not UpAtomIs('DO') then begin
    if ExceptionOnError then
      RaiseStringExpectedButAtomFound('"do"')
    else begin
      Result:=false;
      exit;
    end;
  end;
  ReadNextAtom;
  if CreateNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnWithStatement;
  end;
  ReadTilStatementEnd(true,CreateNodes);
  if CreateNodes then begin
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode; // ctnWithStatement
    WithVarNode:=CurNode.PriorBrother;
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode; // ctnWithVariable
    // set all with variable ends
    while (WithVarNode<>nil) and (WithVarNode.FirstChild=nil) do begin
      WithVarNode.EndPos:=CurPos.StartPos;
      WithVarNode:=WithVarNode.PriorBrother;
    end;
  end;
  Result:=true;
end;

procedure TPascalParserTool.ReadVariableType;
// creates nodes for variable type
begin
  ReadNextAtom;
  TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
    CurPos.EndPos-CurPos.StartPos);
  if UpAtomIs('ABSOLUTE') then begin
    ReadNextAtom;
    ReadConstant(true,false,[]);
  end;
  if CurPos.Flag=cafEqual then begin
    // read constant
    repeat
      ReadNextAtom;
      if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then
        ReadTilBracketClose(true);
      if (CurPos.Flag in AllCommonAtomWords)
      and (not IsKeyWordInConstAllowed.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
      and AtomIsKeyWord
      then
        RaiseCharExpectedButAtomFound(';');
    until CurPos.Flag=cafSemicolon;
  end;
  // read ;
  if CurPos.Flag<>cafSemicolon then
    RaiseCharExpectedButAtomFound(';');
  ReadNextAtom;
  if UpAtomIs('CVAR') then begin
    // for example: 'var a: char; cvar;'
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
    ReadNextAtom;
  end;
  if UpAtomIs('PUBLIC') or UpAtomIs('EXTERNAL') then begin
    if NodeHasParentOfType(CurNode,ctnClass) then
      // class visibility keyword 'public'
      UndoReadNextAtom
    else begin
      // for example 'var a: char; public;'
      if UpAtomIs('EXTERNAL') then begin
        // read external name
        ReadNextAtom;
        if (not UpAtomIs('NAME')) and AtomIsIdentifier(false) then
          ReadConstant(true,false,[]);
      end else
        ReadNextAtom;
      if UpAtomIs('NAME') then begin
        // for example 'var a: char; public name 'b' ;'
        ReadNextAtom;
        if not AtomIsStringConstant then
          RaiseStringExpectedButAtomFound(ctsStringConstant);
        ReadConstant(true,false,[]);
      end;
      if CurPos.Flag<>cafSemicolon then
        RaiseCharExpectedButAtomFound(';');
    end;
  end else
    UndoReadNextAtom;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncBeginEnd: boolean;
// Keyword: begin, asm

  procedure SaveRaiseExceptionWithHint;
  var CaretXY: TCodeXYPosition;
    AMessage: string;
  begin
    AMessage:=Format(ctsStrExpectedButAtomFound,[';','.']);
    if (CleanPosToCaret(CurNode.StartPos,CaretXY))
    and (CaretXY.Code<>nil) then begin
      if CaretXY.Code=TCodeBuffer(Scanner.MainCode) then
        SaveRaiseException(AMessage+ctsPointHintProcStartAt
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')')
      else
        SaveRaiseException(AMessage+ctsPointHintProcStartAt
          +TCodeBuffer(CaretXY.Code).Filename
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')');
    end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
      SaveRaiseException(AMessage);
    end;
  end;

var
  ChildNodeCreated: boolean;
begin
  if (CurNode<>nil)
  and (not (CurNode.Desc in [ctnProcedure,ctnProgram,ctnImplementation])) then
    RaiseStringExpectedButAtomFound('end');
  ChildNodeCreated:=(CurPos.Flag=cafBEGIN) or UpAtomIs('ASM');
  if ChildNodeCreated then begin
    CreateChildNode;
    if CurPos.Flag=cafBEGIN then
      CurNode.Desc:=ctnBeginBlock
    else
      CurNode.Desc:=ctnAsmBlock;
    CurNode.SubDesc:=ctnsNeedJITParsing;
  end;
  // search "end"
  ReadTilBlockEnd(false,false);
  // close node
  if ChildNodeCreated then begin
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if (CurSection<>ctnInterface)
  and (CurNode<>nil) and (CurNode.Desc=ctnProcedure) then begin
    // close procedure
    CurNode.EndPos:=CurPos.EndPos;
    ReadNextAtom;
    if (CurPos.Flag=cafPoint) then
      SaveRaiseExceptionWithHint;
    UndoReadNextAtom;
    EndChildNode;
  end else if (CurNode.Desc in [ctnProgram,ctnImplementation]) then begin
    ReadNextAtom;
    if (CurPos.Flag<>cafPoint) then
      SaveRaiseException(ctsMissingPointAfterEnd);
    // close program
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
    CurSection:=ctnNone;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncType: boolean;
{ The 'type' keyword is the start of a type section.
  examples:

    interface
      type  a=b;
      
    implementation
    
    procedure c;
    type d=e;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnTypeSection;
  // read all type definitions  Name = Type;
  repeat
    DoProgress;
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnTypeDefinition;
      ReadNextAtom;
      if (CurPos.Flag<>cafEqual) then
        RaiseCharExpectedButAtomFound('=');
      // read type
      ReadNextAtom;
      TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
      // read ;
      if CurPos.Flag<>cafSemicolon then
        RaiseCharExpectedButAtomFound(';');
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncVar: boolean;
{
  examples:

    interface
      var a:b;
        a:b; cvar;
        a:b; public name 'string constant';
        a:b; external name 'string constant';
        a:b; cvar; external;

    implementation

    procedure c;
    var d:e;
      f:g=h;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnVarSection;
  // read all variable definitions  Name : Type; [cvar;] [public [name '']]
  repeat
    DoProgress;
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnVarDefinition;
      CurNode.EndPos:=CurPos.EndPos;
      ReadNextAtom;
      while (CurPos.Flag=cafComma) do begin
        EndChildNode; // close variable definition
        ReadNextAtom;
        AtomIsIdentifier(true);
        CreateChildNode;
        CurNode.Desc:=ctnVarDefinition;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
      end;
      if (CurPos.Flag<>cafColon) then begin
        RaiseCharExpectedButAtomFound(':');
      end;
      // read type
      ReadVariableType;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncConst: boolean;
{
  examples:

    interface
      const a:b=3;

    implementation

    procedure c;
    const d=2;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnConstSection;
  // read all constants  Name = <Const>; or Name : type = <Const>;
  repeat
    DoProgress;
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      ReadNextAtom;
      if (CurPos.Flag=cafColon) then begin
        // read type
        ReadNextAtom;
        TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
      end;
      if (CurPos.Flag<>cafEqual) then
        RaiseCharExpectedButAtomFound('=');
      // read constant
      repeat
        ReadNextAtom;
        if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then
          ReadTilBracketClose(true);
        if (CurPos.Flag in AllCommonAtomWords)
        and (not IsKeyWordInConstAllowed.DoItUppercase(UpperSrc,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
        and AtomIsKeyWord then
          RaiseStringExpectedButAtomFound('constant');
      until (CurPos.Flag in [cafSemicolon]);
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncResourceString: boolean;
{
  examples:

    interface
      ResourceString a='';

    implementation

    procedure c;
    ResourceString b='';
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnResStrSection;
  // read all string constants Name = 'abc';
  repeat
    DoProgress;
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      ReadNextAtom;
      if (CurPos.Flag<>cafEqual) then
        RaiseCharExpectedButAtomFound('=');
      // read string constant
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseStringExpectedButAtomFound(ctsStringConstant);
      ReadConstant(true,false,[]);
      if UpAtomIs('DEPRECATED') then
        ReadNextAtom;
      // read ;
      if CurPos.Flag<>cafSemicolon then
        RaiseCharExpectedButAtomFound(';');
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncLabel: boolean;
{
  examples:
    label a, 23, b;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnLabelSection;
  // read all constants
  repeat
    ReadNextAtom;  // identifier or number
    if (not AtomIsIdentifier(false)) and (not AtomIsNumber) then begin
      RaiseStringExpectedButAtomFound(ctsIdentifier);
    end;
    CreateChildNode;
    CurNode.Desc:=ctnLabelType;
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
    ReadNextAtom;
    if CurPos.Flag=cafSemicolon then begin
      break;
    end else if (CurPos.Flag<>cafComma) then begin
      RaiseCharExpectedButAtomFound(';');
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypePacked: boolean;
begin
  ReadNextAtom;
  if not PackedTypesKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
    CurPos.EndPos-CurPos.StartPos) then
    RaiseStringExpectedButAtomFound('"record"');
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncClass: boolean;
// class, object
//   this is a quick parser, which will only create one node for each class
//   the nodes for the methods and properties are created in a second
//   parsing phase (in KeyWordFuncClassMethod)
var
  ChildCreated: boolean;
  ClassAtomPos: TAtomPosition;
  Level: integer;
begin
  if CurNode.Desc<>ctnTypeDefinition then
    SaveRaiseExceptionFmt(ctsAnoymDefinitionsAreNotAllowed,['class']);
  if (LastUpAtomIs(0,'PACKED')) then begin
    if not LastAtomIs(1,'=') then
      SaveRaiseExceptionFmt(ctsAnoymDefinitionsAreNotAllowed,['class']);
    ClassAtomPos:=LastAtoms.GetValueAt(1);
  end else begin
    if not LastAtomIs(0,'=') then
      SaveRaiseExceptionFmt(ctsAnoymDefinitionsAreNotAllowed,['class']);
    ClassAtomPos:=CurPos;
  end;
  // class start found
  ChildCreated:=(UpAtomIs('CLASS')) or (UpAtomIs('OBJECT'));
  if ChildCreated then begin
    CreateChildNode;
    CurNode.Desc:=ctnClass;
    CurNode.StartPos:=ClassAtomPos.StartPos;
  end;
  // find end of class
  ReadNextAtom;
  if UpAtomIs('OF') then begin
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
    if ChildCreated then CurNode.Desc:=ctnClassOfType;
  end else if (CurPos.Flag=cafRoundBracketOpen) then begin
    // read inheritage brackets
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  CurNode.SubDesc:=ctnsNeedJITParsing; // will not create sub nodes now
  if CurPos.Flag=cafSemicolon then begin
    if ChildCreated and (CurNode.Desc=ctnClass) then begin
      // forward class definition found
      CurNode.SubDesc:=CurNode.SubDesc+ctnsForwardDeclaration;
    end;
  end else begin
    Level:=1;
    while (CurPos.StartPos<=SrcLen) do begin
      DoProgress;
      if CurPos.Flag=cafEND then begin
        dec(Level);
        if Level=0 then break;
      end else if CurPos.Flag=cafRECORD then inc(Level);
      ReadNextAtom;
    end;
    if (CurPos.StartPos>SrcLen) then
      SaveRaiseException(ctsEndForClassNotFound);
  end;
  if ChildCreated then begin
    // close class
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if CurPos.Flag=cafEND then begin
    ReadNextAtom;
    if UpAtomIs('DEPRECATED') or UpAtomIs('PLATFORM') then ReadNextAtom;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassInterface: boolean;
// class interface, dispinterface
var
  ChildCreated: boolean;
  IntfAtomPos: TAtomPosition;
begin
  if CurNode.Desc<>ctnTypeDefinition then
    SaveRaiseExceptionFmt(ctsAnoymDefinitionsAreNotAllowed,['interface']);
  if not LastAtomIs(0,'=') then
    SaveRaiseExceptionFmt(ctsAnoymDefinitionsAreNotAllowed,['interface']);
  IntfAtomPos:=CurPos;
  // class interface start found
  ChildCreated:=true;
  if ChildCreated then begin
    CreateChildNode;
    CurNode.Desc:=ctnClassInterface;
    CurNode.StartPos:=IntfAtomPos.StartPos;
  end;
  // find end of interface
  ReadNextAtom;
  if (CurPos.Flag<>cafSemicolon) then begin
    if (CurPos.Flag=cafRoundBracketOpen) then begin
      // read inheritage brackets
      ReadTilBracketClose(true);
      ReadNextAtom;
    end;
    if CurPos.Flag=cafEdgedBracketOpen then
      ReadGUID;
    // parse till "end" of class/object
    CurKeyWordFuncList:=ClassInterfaceKeyWordFuncList;
    try
      repeat
        if CurPos.Flag in [cafEnd,cafNone] then break;
        if not DoAtom then break;
        ReadNextAtom;
      until false;
    finally
      CurKeyWordFuncList:=DefaultKeyWordFuncList;
    end;
  end else begin
    // forward definition
    CurNode.SubDesc:=CurNode.SubDesc+ctnsForwardDeclaration;
  end;
  if ChildCreated then begin
    // close class interface
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if CurPos.Flag=cafEND then begin
    ReadNextAtom;
    if UpAtomIs('DEPRECATED') or UpAtomIs('PLATFORM') then ReadNextAtom;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeArray: boolean;
{
  examples:
    array of ...
    array[SubRange] of ...
    array[SubRange,SubRange,...] of ...
}
begin
  CreateChildNode;
  CurNode.Desc:=ctnArrayType;
  ReadNextAtom;
  if (CurPos.Flag=cafEdgedBracketOpen) then begin
    repeat
      ReadNextAtom;
      CreateChildNode;
      CurNode.Desc:=ctnRangeType;
      ReadSubRange(true);
      CurNode.EndPos:=LastAtoms.GetValueAt(0).EndPos;
      EndChildNode;
      if (CurPos.Flag=cafEdgedBracketClose) then break;
      if (CurPos.Flag<>cafComma) then
        RaiseCharExpectedButAtomFound(']');
    until false;
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseStringExpectedButAtomFound('"of"');
  ReadNextAtom;
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeProc: boolean;
{
  examples:
    procedure;
    procedure of object;
    procedure(ParmList) of object;
    function(ParmList):SimpleType of object;
    procedure; cdecl; popstack; register; pascal; stdcall;
}
var IsFunction, EqualFound: boolean;
begin
  IsFunction:=UpAtomIs('FUNCTION');
  CreateChildNode;
  CurNode.Desc:=ctnProcedureType;
  ReadNextAtom;
  if (CurPos.Flag=cafRoundBracketOpen) then begin
    // read parameter list
    ReadParamList(true,false,[]);
  end;
  if IsFunction then begin
    if (CurPos.Flag=cafColon) then begin
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
    end else begin
      RaiseCharExpectedButAtomFound(':');
    end;
  end;
  if UpAtomIs('OF') then begin
    if not ReadNextUpAtomIs('OBJECT') then
      RaiseStringExpectedButAtomFound('"object"');
    ReadNextAtom;
  end;
  if (CurPos.Flag=cafEqual)
  and (CurNode.Parent.Desc in [ctnConstDefinition,ctnVarDefinition]) then begin
    // for example  'const f: procedure = nil;'
  end else begin
    if CurPos.Flag=cafSemicolon then begin
      ReadNextAtom;
      EqualFound:=false;
    end else if (CurPos.Flag=cafEqual) then begin
      EqualFound:=true;
    end else
      EqualFound:=false;
    if not EqualFound then begin
      // read modifiers
      repeat
        if (not IsKeyWordProcedureTypeSpecifier.DoItUpperCase(UpperSrc,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)) then
        begin
          UndoReadNextAtom;
          if (CurPos.Flag<>cafSemicolon)
          and (Scanner.CompilerMode<>cmDelphi) then
            RaiseCharExpectedButAtomFound(';');
          break;
        end else begin
          ReadNextAtom;
          if CurPos.Flag<>cafSemicolon then begin
            if (CurPos.Flag=cafEqual) then begin
              break;
            end;
            if Scanner.CompilerMode<>cmDelphi then begin
              RaiseCharExpectedButAtomFound(';');
            end else begin
              // delphi allows proc modifiers without semicolons
              if not IsKeyWordProcedureTypeSpecifier.DoItUpperCase(UpperSrc,
                CurPos.StartPos,CurPos.EndPos-CurPos.StartPos) then
              begin
                RaiseCharExpectedButAtomFound(';');
              end;
              UndoReadNextAtom;
            end;
          end;
        end;
        ReadNextAtom;
      until false;
    end;
  end;
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeSet: boolean;
{
  examples:
    set of Identifier;
    set of SubRange;
}
begin
  CreateChildNode;
  CurNode.Desc:=ctnSetType;
  if not ReadNextUpAtomIs('OF') then
    RaiseStringExpectedButAtomFound('"of"');
  ReadNextAtom;
  Result:=KeyWordFuncTypeDefault;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeLabel: boolean;
// 'label;'
begin
  CreateChildNode;
  CurNode.Desc:=ctnLabelType;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeType: boolean;
// 'type identifier'
begin
  if not LastAtomIs(0,'=') then
    RaiseStringExpectedButAtomFound(ctsIdentifier);
  CreateChildNode;
  CurNode.Desc:=ctnTypeType;
  ReadNextAtom;
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncTypeFile: boolean;
// 'file' or 'file of <type>'
begin
  CreateChildNode;
  CurNode.Desc:=ctnFileType;
  if ReadNextUpAtomIs('OF') then begin
    ReadNextAtom;
    Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
    if not Result then exit;
  end;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypePointer: boolean;
// '^Identfier'
begin
  CreateChildNode;
  CurNode.Desc:=ctnPointerType;
  ReadNextAtom;
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncTypeDefault: boolean;
{ check for enumeration, subrange and identifier types

  examples:
    integer
    1..3
    (a,b:=3,c=4)
    (a)..4
    Low(integer)..High(integer)
    'a'..'z'
}
var SubRangeOperatorFound: boolean;

  procedure ReadTillTypeEnd;
  begin
    // read till ';', ':', ')', '=', 'end'
    while (CurPos.StartPos<=SrcLen) do begin
      if (CurPos.Flag in [cafSemicolon,cafColon,cafRoundBracketClose,
        cafEqual,cafEdgedBracketClose]) or AtomIsKeyWord
      then
        break;
      if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then
        ReadTilBracketClose(true)
      else if AtomIs('..') then begin
        if SubRangeOperatorFound then
          SaveRaiseException(ctsUnexpectedSubRangeOperatorFound);
        SubRangeOperatorFound:=true;
      end;
      ReadNextAtom;
    end;
  end;

// TPascalParserTool.KeyWordFuncTypeDefault: boolean
begin
  CreateChildNode;
  SubRangeOperatorFound:=false;
  if CurPos.Flag in AllCommonAtomWords then begin
    AtomIsIdentifier(true);
    ReadNextAtom;
    if (CurPos.Flag=cafPoint) then begin
      // first word was unit name
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
    end;
    while (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) do begin
      ReadTilBracketClose(true);
      ReadNextAtom;
    end;
    if not AtomIs('..') then begin
      // an identifier
      CurNode.Desc:=ctnIdentifier;
      CurNode.EndPos:=CurPos.StartPos;
    end else begin
      // a subrange
      CurNode.Desc:=ctnRangeType;
      ReadTillTypeEnd;
      if not SubRangeOperatorFound then
        SaveRaiseException(ctsInvalidSubrange);
      CurNode.EndPos:=CurPos.StartPos;
    end;
    if UpAtomIs('PLATFORM') or UpAtomIs('DEPRECATED') then ReadNextAtom;
  end else begin
    // enum or subrange
    ReadTillTypeEnd;
    if SubRangeOperatorFound then begin
      // a subrange
      CurNode.Desc:=ctnRangeType;
      CurNode.EndPos:=CurPos.StartPos;
    end else begin
      // an enum or syntax error
      MoveCursorToNodeStart(CurNode);
      ReadNextAtom;
      if (CurPos.Flag=cafRoundBracketOpen) then begin
        // an enumeration -> read all enums
        CurNode.Desc:=ctnEnumerationType;
        repeat
          ReadNextAtom; // read enum name
          if (CurPos.Flag=cafRoundBracketClose) then break;
          AtomIsIdentifier(true);
          CreateChildNode;
          CurNode.Desc:=ctnEnumIdentifier;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode; // close enum node
          ReadNextAtom;
          if AtomIs(':=') or (CurPos.Flag=cafEqual) then begin
            // read ordinal value
            ReadNextAtom;
            ReadConstant(true,false,[]);
          end;
          if (CurPos.Flag=cafRoundBracketClose) then break;
          if (CurPos.Flag<>cafComma) then
            RaiseCharExpectedButAtomFound(')');
        until false;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
      end else
        SaveRaiseException(ctsInvalidType);
    end;
  end;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeRecord: boolean;
{ read variable type 'record'

  examples:
    record
      i: packed record
           j: integer;
           k: record end;
           case y: integer of
             0: (a: integer);
             1,2,3: (b: array[char] of char; c: char);
             3: ( d: record
                       case byte of
                         10: (i: integer; );
                         11: (y: byte);
                     end; );
             4: (e: integer;
                   case z of
                   8: (f: integer)
                 );
         end;
    end;
}
// function TPascalParserTool.KeyWordFuncTypeRecord: boolean;
begin
  CreateChildNode;
  CurNode.Desc:=ctnRecordType;
  if LastUpAtomIs(0,'PACKED') then
    CurNode.StartPos:=LastAtoms.GetValueAt(0).StartPos;
  // read all variables
  repeat
    ReadNextAtom;
    if CurPos.Flag=cafEND then break;
    if UpAtomIs('CASE') then begin
      KeyWordFuncTypeRecordCase;
      break;
    end else begin
      // read variable names
      repeat
        AtomIsIdentifier(true);
        CreateChildNode;
        CurNode.Desc:=ctnVarDefinition;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
        if (CurPos.Flag=cafColon) then break;
        if (CurPos.Flag<>cafComma) then
          RaiseCharExpectedButAtomFound(':');
        EndChildNode; // close variable
        ReadNextAtom; // read next variable name
      until false;
      ReadNextAtom;
      Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
      if not Result then exit;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode; // close variable
      if CurPos.Flag=cafEND then break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode; // close record
  ReadNextAtom;
  if UpAtomIs('PLATFORM') then ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeRecordCase: boolean;
begin
  if not UpAtomIs('CASE') then
    SaveRaiseException('[TPascalParserTool.KeyWordFuncTypeRecordCase] '
      +'internal error');
  CreateChildNode;
  CurNode.Desc:=ctnRecordCase;
  ReadNextAtom; // read ordinal type
  AtomIsIdentifier(true);
  ReadNextAtom;
  if (CurPos.Flag=cafColon) then begin
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then // read 'of'
    RaiseStringExpectedButAtomFound('"of"');
  // read all variants
  repeat
    ReadNextAtom;  // read constant (variant identifier)
    if (CurPos.Flag in [cafRoundBracketClose,cafEnd]) then break;
    CreateChildNode;
    CurNode.Desc:=ctnRecordVariant;
    repeat
      ReadConstant(true,false,[]);
      if (CurPos.Flag=cafColon) then break;
      if (CurPos.Flag<>cafComma) then
        RaiseCharExpectedButAtomFound(':');
      ReadNextAtom;
    until false;
    ReadNextAtom;  // read '('
    if (CurPos.Flag<>cafRoundBracketOpen) then
      RaiseCharExpectedButAtomFound('(');
    // read all variables
    ReadNextAtom; // read first variable name
    repeat
      if (CurPos.Flag=cafRoundBracketClose) then begin
        // end of variant record
        break;
      end else if UpAtomIs('CASE') then begin
        // sub record variant
        KeyWordFuncTypeRecordCase();
        break;
      end else begin
        // sub identifier
        repeat
          AtomIsIdentifier(true);
          CreateChildNode;
          CurNode.Desc:=ctnVarDefinition;
          CurNode.EndPos:=CurPos.EndPos;
          ReadNextAtom;
          if (CurPos.Flag=cafColon) then break;
          if (CurPos.Flag<>cafComma) then
            RaiseCharExpectedButAtomFound(',');
          EndChildNode;
          ReadNextAtom; // read next variable name
        until false;
        ReadNextAtom; // read type
        Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
           CurPos.EndPos-CurPos.StartPos);
        if not Result then exit;
        CurNode.EndPos:=CurPos.EndPos;
        EndChildNode; // close variable definition
      end;
      if (CurPos.Flag=cafRoundBracketClose) then break;
      if CurPos.Flag<>cafSemicolon then
        RaiseCharExpectedButAtomFound(';');
      ReadNextAtom;
    until false;
    if (CurPos.Flag<>cafRoundBracketClose) then
      RaiseCharExpectedButAtomFound(')');
    ReadNextAtom;
    if (CurPos.Flag in [cafEnd,cafRoundBracketClose]) then begin
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode; // close variant
      break;
    end;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode; // close variant
    // read next variant
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode; // close case
  Result:=true;
end;

function TPascalParserTool.ExtractPropName(PropNode: TCodeTreeNode;
  InUpperCase: boolean): string;
begin
  Result:='';
  if (PropNode=nil) or (PropNode.Desc<>ctnProperty) then exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if not UpAtomIs('PROPERTY') then exit;
  ReadNextAtom;
  AtomIsIdentifier(true);
  if InUpperCase then
    Result:=GetUpAtom
  else
    Result:=GetAtom;
end;

function TPascalParserTool.ExtractPropType(PropNode: TCodeTreeNode;
  InUpperCase, EmptyIfIndexed: boolean): string;
begin
  Result:='';
  if (PropNode=nil) or (PropNode.Desc<>ctnProperty) then exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if not UpAtomIs('PROPERTY') then exit;
  ReadNextAtom;
  AtomIsIdentifier(true);
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    if EmptyIfIndexed then exit;
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if CurPos.Flag in [cafSemicolon,cafEND] then exit;
  if not (CurPos.Flag=cafColon) then
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[':',GetAtom]);
  ReadNextAtom;
  AtomIsIdentifier(true);
  if InUpperCase then
    Result:=GetUpAtom
  else
    Result:=GetAtom;
end;

function TPascalParserTool.ExtractProperty(PropNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (PropNode=nil) or (PropNode.StartPos<1) or (PropNode.Desc<>ctnProperty)
  then exit;
  // start extraction
  InitExtraction;
  MoveCursorToNodeStart(PropNode);
  ExtractNextAtom(false,Attr);
  // parse 'property'
  ExtractNextAtom(phpWithStart in Attr,Attr);
  ExtractProcHeadPos:=phepStart;
  // parse name
  ExtractNextAtom(not (phpWithoutName in Attr),Attr);
  ExtractProcHeadPos:=phepName;
  // read parameter list
  if (CurPos.Flag=cafEdgedBracketOpen) then
    ReadParamList(false,true,Attr);
  ExtractProcHeadPos:=phepParamList;
  // read result type
  if (CurPos.Flag=cafColon) then begin
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if not AtomIsIdentifier(false) then exit;
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    ExtractProcHeadPos:=phepResultType;
  end;

  // copy memorystream to Result string
  Result:=GetExtraction;
end;

function TPascalParserTool.ExtractProcName(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
var
  ProcHeadNode: TCodeTreeNode;
begin
  Result:='';
  if [phpWithoutClassName,phpWithoutName]*Attr=
     [phpWithoutClassName,phpWithoutName]
  then
    exit;
  while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
    ProcNode:=ProcNode.Parent;
  if ProcNode=nil then exit;
  ProcHeadNode:=ProcNode.FirstChild;
  if (ProcHeadNode=nil) or (ProcHeadNode.StartPos<1) then exit;
  MoveCursorToNodeStart(ProcHeadNode);
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  if phpInUpperCase in Attr then
    Result:=GetUpAtom
  else
    Result:=GetAtom;
  ReadNextAtom;
  if (CurPos.Flag=cafPoint) then begin
    if (phpWithoutClassName in Attr) then begin
      Result:='';
    end else begin
      if not (phpWithoutName in Attr) then
        Result:=Result+'.';
    end;
    ReadNextAtom;
    if not (phpWithoutName in Attr) then begin
      if phpInUpperCase in Attr then
        Result:=Result+GetUpAtom
      else
        Result:=Result+GetAtom;
    end;
  end else begin
    if phpWithoutName in Attr then
      Result:='';
  end;
end;

procedure TPascalParserTool.RaiseCharExpectedButAtomFound(c: char);
begin
  SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,[c,GetAtom]);
end;

procedure TPascalParserTool.RaiseStringExpectedButAtomFound(const s: string);
begin
  SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,[s,GetAtom]);
end;

procedure TPascalParserTool.RaiseUnexpectedKeyWord;
begin
  SaveRaiseExceptionFmt(ctsUnexpectedKeyword,[GetAtom]);
end;

procedure TPascalParserTool.RaiseIllegalQualifier;
begin
  SaveRaiseExceptionFmt(ctsIllegalQualifier,[GetAtom]);
end;

procedure TPascalParserTool.InitExtraction;
begin
  if ExtractMemStream=nil then
    ExtractMemStream:=TMemoryStream.Create;
  ExtractMemStream.Position:=0;
  ExtractMemStream.Size:=0;
end;

function TPascalParserTool.GetExtraction: string;
begin
  SetLength(Result,ExtractMemStream.Size);
  ExtractMemStream.Position:=0;
  ExtractMemStream.Read(Result[1],length(Result));
end;

function TPascalParserTool.ExtractStreamEndIsIdentChar: boolean;
var c: char;
begin
  if ExtractMemStream.Position=0 then begin
    Result:=false;
    exit;
  end;
  ExtractMemStream.Position:=ExtractMemStream.Position-1;
  ExtractMemStream.Read(c,1);
  Result:=IsIdentChar[c];
end;

procedure TPascalParserTool.ExtractNextAtom(AddAtom: boolean;
  Attr: TProcHeadAttributes);
// add current atom and text before, then read next atom
// if not phpWithComments in Attr then the text before will be shortened
var LastAtomEndPos, LastStreamPos: integer;
begin
  LastStreamPos:=ExtractMemStream.Position;
  if LastAtoms.Count>0 then begin
    LastAtomEndPos:=LastAtoms.GetValueAt(0).EndPos;
    if phpWithComments in Attr then begin
      // add space/comment between pascal atoms
      if phpInUpperCase in Attr then
        ExtractMemStream.Write(UpperSrc[LastAtomEndPos],
             CurPos.StartPos-LastAtomEndPos)
      else
        ExtractMemStream.Write(Src[LastAtomEndPos],
             CurPos.StartPos-LastAtomEndPos);
    end else if (CurPos.StartPos>LastAtomEndPos)
      and (ExtractMemStream.Position>0) then
    begin
      // some code was skipped
      // -> check if a space must be inserted
      if AddAtom
      and ((phpCommentsToSpace in Attr)
      or ((CurPos.StartPos<=SrcLen) and (IsIdentStartChar[Src[CurPos.StartPos]])
          and ExtractStreamEndIsIdentChar))
      then begin
        ExtractMemStream.Write(' ',1);
        LastStreamPos:=ExtractMemStream.Position;
      end;
    end;
  end;
  if AddAtom then begin
    if phpInUpperCase in Attr then
      ExtractMemStream.Write(UpperSrc[CurPos.StartPos],
          CurPos.EndPos-CurPos.StartPos)
    else
      ExtractMemStream.Write(Src[CurPos.StartPos],
          CurPos.EndPos-CurPos.StartPos);
  end;
  if (ExtractSearchPos>0)
  and (ExtractSearchPos<=ExtractMemStream.Position)
  then begin
    ExtractFoundPos:=ExtractSearchPos-1-LastStreamPos+CurPos.StartPos;
    ExtractSearchPos:=-1;
  end;
  ReadNextAtom;
end;

function TPascalParserTool.ExtractProcHead(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
var
  GrandPaNode: TCodeTreeNode;
  TheClassName, s: string;
  HasClassName, IsProcType: boolean;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (ProcNode=nil) or (ProcNode.StartPos<1) then exit;
  if ProcNode.Desc=ctnProcedureHead then begin
    ProcNode:=ProcNode.Parent;
    if ProcNode=nil then exit;
  end;
  if not ProcNode.Desc in [ctnProcedure,ctnProcedureType] then exit;
  IsProcType:=(ProcNode.Desc=ctnProcedureType);
  if (phpAddClassname in Attr) then begin
    GrandPaNode:=ProcNode.Parent;
    if GrandPaNode=nil then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if (GrandPaNode=nil) or (GrandPaNode.Desc<>ctnClass) then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if GrandPaNode.Desc<>ctnTypeDefinition then exit;
    MoveCursorToCleanPos(GrandPaNode.StartPos);
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    TheClassName:=GetAtom;
  end;
  InitExtraction;
  // reparse the clean source
  MoveCursorToNodeStart(ProcNode);
  // parse procedure head = start + name + parameterlist + result type ;
  ExtractNextAtom(false,Attr);
  // read procedure start keyword
  if (UpAtomIs('CLASS') or UpAtomIs('STATIC')) then
    ExtractNextAtom((phpWithStart in Attr)
                    and not (phpWithoutClassKeyword in Attr),Attr);
  if (UpAtomIs('PROCEDURE')) or (UpAtomIs('FUNCTION'))
  or (UpAtomIs('CONSTRUCTOR')) or (UpAtomIs('DESTRUCTOR'))
  or (UpAtomIs('OPERATOR')) then
    ExtractNextAtom(phpWithStart in Attr,Attr)
  else
    exit;
  ExtractProcHeadPos:=phepStart;
  if not IsProcType then begin
    // read name
    if not AtomIsIdentifier(false) then exit;
    ReadNextAtom;
    HasClassName:=(CurPos.Flag=cafPoint);
    UndoReadNextAtom;
    if HasClassName then begin
      // read class name
      ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
      // read '.'
      ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
      // read name
      if not AtomIsIdentifier(false) then exit;
      ExtractNextAtom(not (phpWithoutName in Attr),Attr);
    end else begin
      // read name
      if not (phpAddClassname in Attr) then begin
        ExtractNextAtom(not (phpWithoutName in Attr),Attr);
      end else begin
        // add class name
        s:=TheClassName+'.';
        if not (phpWithoutName in Attr) then
          s:=s+GetAtom;
        ExtractNextAtom(false,Attr);
        if phpInUpperCase in Attr then s:=UpperCaseStr(s);
        if ExtractStreamEndIsIdentChar then
          s:=' '+s;
        ExtractMemStream.Write(s[1],length(s));
      end;
    end;
    ExtractProcHeadPos:=phepName;
  end;
  // read parameter list
  if (CurPos.Flag=cafRoundBracketOpen) then
    ReadParamList(false,true,Attr);
  ExtractProcHeadPos:=phepParamList;
  // read result type
  if (CurPos.Flag=cafColon) then begin
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if not AtomIsIdentifier(false) then exit;
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    ExtractProcHeadPos:=phepResultType;
  end;
  // read 'of object'
  if UpAtomIs('OF') then begin
    if IsProcType then begin
      ExtractNextAtom(phpWithOfObject in Attr,Attr);
      if not UpAtomIs('OBJECT') then exit;
      ExtractNextAtom(phpWithOfObject in Attr,Attr);
    end;
  end;
  // read semicolon
  if CurPos.Flag=cafSemicolon then
    ExtractNextAtom(true,Attr);
  // read specifiers
  if phpWithCallingSpecs in Attr then begin
    while (CurPos.StartPos<=ProcNode.FirstChild.EndPos) do begin
      if CurPos.Flag=cafSemicolon then begin
        ExtractNextAtom(false,Attr);
      end else begin
        if (UpAtomIs('INLINE') or UpAtomIs('CDECL')) then begin
          ExtractNextAtom(phpWithCallingSpecs in Attr,Attr);
          ExtractMemStream.Write(';',1);
        end
        else if (CurPos.Flag=cafEdgedBracketOpen) then begin
          ReadTilBracketClose(false);
          ExtractNextAtom(false,Attr);
        end else begin
          ExtractNextAtom(false,Attr);
        end;
      end;
    end;
  end;
  
  // copy memorystream to Result string
  Result:=GetExtraction;
end;

function TPascalParserTool.ExtractClassName(ClassNode: TCodeTreeNode;
  InUpperCase: boolean): string;
var Len: integer;
begin
  if ClassNode<>nil then begin
    if ClassNode.Desc=ctnClass then begin
      ClassNode:=ClassNode.Parent;
      if ClassNode=nil then begin
        Result:='';
        exit;
      end;
    end;
    Len:=1;
    while (ClassNode.StartPos+Len<=SrcLen)
    and (IsIdentChar[Src[ClassNode.StartPos+Len]]) do
      inc(Len);
    if InUpperCase then
      Result:=copy(UpperSrc,ClassNode.StartPos,Len)
    else
      Result:=copy(Src,ClassNode.StartPos,Len);
  end else
    Result:='';
end;

function TPascalParserTool.FindProcNode(StartNode: TCodeTreeNode;
  const AProcHead: string; Attr: TProcHeadAttributes): TCodeTreeNode;
// search in all next brothers for a Procedure Node with the Name ProcName
// if there are no further brothers and the parent is a section node
// ( e.g. 'interface', 'implementation', ...) or a class visibility node
// (e.g. 'public', 'private', ...) then the search will continue in the next
// section
var CurProcHead: string;
begin
  Result:=StartNode;
  while (Result<>nil) do begin
    //writeln('TPascalParserTool.FindProcNode A "',NodeDescriptionAsString(Result.Desc),'"');
    if Result.Desc=ctnProcedure then begin
      if (not ((phpIgnoreForwards in Attr)
               and ((Result.SubDesc and ctnsForwardDeclaration)>0)))
      and (not ((phpIgnoreProcsWithBody in Attr)
            and (FindProcBody(Result)<>nil))) then
      begin
        CurProcHead:=ExtractProcHead(Result,Attr);
        //writeln('TPascalParserTool.FindProcNode B "',CurProcHead,'" =? "',AProcHead,'"');
        if (CurProcHead<>'')
        and (CompareTextIgnoringSpace(CurProcHead,AProcHead,false)=0) then
          exit;
      end;
    end;
    // next node
    Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalParserTool.FindProcBody(
  ProcNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=ProcNode;
  if Result=nil then exit;
  Result:=Result.FirstChild;
  while Result<>nil do begin
    if Result.Desc in [ctnBeginBlock,ctnAsmBlock] then
      exit;
    Result:=Result.NextBrother;
  end;
end;

function TPascalParserTool.FindVarNode(StartNode: TCodeTreeNode;
  const UpperVarName: string): TCodeTreeNode;
begin
  Result:=StartNode;
  while Result<>nil do begin
    if (Result.Desc=ctnVarDefinition)
    and (CompareNodeIdentChars(Result,UpperVarName)=0) then
      exit;
    Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalParserTool.ExtractClassNameOfProcNode(
  ProcNode: TCodeTreeNode): string;
var TheClassName: string;
begin
  Result:='';
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) then
    ProcNode:=ProcNode.FirstChild;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedureHead) then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  TheClassName:=GetAtom;
  ReadNextAtom;
  if (CurPos.Flag<>cafPoint) then exit;
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  Result:=TheClassName;
end;

function TPascalParserTool.FindFirstNodeOnSameLvl(
  StartNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=StartNode;
  if Result=nil then exit;
  Result:=Result.Parent;
  if Result=nil then exit;
  while (Result.Desc in AllCodeSections) and (Result.PriorBrother<>nil) do
    Result:=Result.PriorBrother;
  while (Result<>nil) and (Result.FirstChild=nil) do
    Result:=Result.NextBrother;
  Result:=Result.FirstChild;
end;

function TPascalParserTool.FindNextNodeOnSameLvl(
  StartNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=StartNode;
  if Result=nil then exit;
  if Result.NextBrother<>nil then
    Result:=Result.NextBrother
  else begin
    Result:=Result.Parent;
    if Result=nil then exit;
    Result:=Result.NextBrother;
    while (Result<>nil) and (Result.FirstChild=nil) do
      Result:=Result.NextBrother;
    if Result=nil then exit;
    Result:=Result.FirstChild;
  end;
end;

function TPascalParserTool.FindClassNode(StartNode: TCodeTreeNode;
  const UpperClassName: string;
  IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
// search for types on same level,
// with type class and classname = SearchedClassName
var CurClassName: string;
  ANode, CurClassNode: TCodeTreeNode;
begin
  ANode:=StartNode;
  Result:=nil;
  while (ANode<>nil) do begin
    if ANode.Desc=ctnTypeSection then begin
      Result:=FindClassNode(ANode.FirstChild,UpperClassName,IgnoreForwards,
                     IgnoreNonForwards);
      if Result<>nil then exit;
    end else if ANode.Desc=ctnTypeDefinition then begin
      CurClassNode:=ANode.FirstChild;
      if (CurClassNode<>nil) and (CurClassNode.Desc=ctnClass) then begin
        if (not (IgnoreForwards
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)>0)))
        and (not (IgnoreNonForwards
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)=0)))
        then begin
          MoveCursorToNodeStart(ANode);
          ReadNextAtom;
          CurClassName:=GetUpAtom;
          if UpperClassName=CurClassName then begin
            Result:=CurClassNode;
            exit;
          end;
        end;
      end;
    end;
    // next node
    if (ANode.NextBrother=nil) and (ANode.Parent<>nil)
    and (ANode.Parent.NextBrother<>nil)
    and (ANode.Parent.Desc in (AllCodeSections+AllClassSections)) then
      ANode:=ANode.Parent.NextBrother.FirstChild
    else
      ANode:=ANode.NextBrother;
  end;
end;

function TPascalParserTool.FindClassNodeInInterface(
  const UpperClassName: string;
  IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  if Result.Desc=ctnUnit then begin
    Result:=Result.NextBrother;
    if Result=nil then exit;
  end;
  Result:=FindClassNode(Result.FirstChild,UpperClassName,
               IgnoreForwards, IgnoreNonForwards);
end;

function TPascalParserTool.FindFirstIdentNodeInClass(
  ClassNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=nil;
  if (ClassNode=nil) then exit;
  BuildSubTreeForClass(ClassNode);
  Result:=ClassNode.FirstChild;
  while (Result<>nil) and (Result.FirstChild=nil) do
    Result:=Result.NextBrother;
  if Result=nil then exit;
  Result:=Result.FirstChild;
end;

function TPascalParserTool.FindInterfaceNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnInterface) do
    Result:=Result.NextBrother;
end;

function TPascalParserTool.FindImplementationNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnImplementation) do
    Result:=Result.NextBrother;
end;

function TPascalParserTool.FindInitializationNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnInitialization) do
    Result:=Result.NextBrother;
end;

function TPascalParserTool.FindMainBeginEndNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if (Result=nil) then exit;
  if (Result.Desc=ctnProgram) then
    Result:=Result.LastChild
  else begin
    Result:=FindImplementationNode;
    if Result<>nil then
      Result:=Result.LastChild;
  end;
  if Result=nil then exit;
  if Result.Desc<>ctnBeginBlock then Result:=nil;
end;

function TPascalParserTool.NodeHasParentOfType(ANode: TCodeTreeNode;
  NodeDesc: TCodeTreeNodeDesc): boolean;
begin
  if ANode<>nil then begin
    repeat
      ANode:=ANode.Parent;
    until (ANode=nil) or (ANode.Desc=NodeDesc);
  end;
  Result:=(ANode<>nil);
end;

function TPascalParserTool.NodeIsInAMethod(Node: TCodeTreeNode): boolean;
begin
  Result:=false;
  while (Node<>nil) do begin
    if (Node.Desc=ctnProcedure) then begin
      if NodeIsMethodBody(Node) then begin
        Result:=true;
        exit;
      end;
    end;
    Node:=Node.Parent;
  end;
end;

function TPascalParserTool.NodeIsMethodBody(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) then begin

    // ToDo: ppu, ppw, dcu

    MoveCursorToNodeStart(ProcNode.FirstChild); // ctnProcedureHead
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    ReadNextAtom;
    if (CurPos.Flag<>cafPoint) then exit;
    Result:=true;
    exit;
  end;
end;

function TPascalParserTool.NodeIsFunction(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure) then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if UpAtomIs('CLASS') then ReadNextAtom;
  Result:=UpAtomIs('FUNCTION');
end;

function TPascalParserTool.NodeIsConstructor(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode=nil) then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Desc<>ctnProcedure then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  Result:=UpAtomIs('CONSTRUCTOR');
end;

function TPascalParserTool.NodeIsPartOfTypeDefinition(ANode: TCodeTreeNode
  ): boolean;
begin
  ANode:=ANode.Parent;
  while ANode<>nil do begin
    if ANode.Desc in (AllIdentifierDefinitions+AllPascalTypes) then begin
      Result:=true;
      exit;
    end;
    ANode:=ANode.Parent;
  end;
  Result:=false;
end;

function TPascalParserTool.CleanPosIsInComment(CleanPos,
  CleanCodePosInFront: integer; var CommentStart, CommentEnd: integer): boolean;
var CommentLvl, CurCommentPos: integer;
begin
  Result:=false;
  if CleanPos>SrcLen then exit;
  if CleanCodePosInFront>CleanPos then
    SaveRaiseException(
      'TPascalParserTool.CleanPosIsInComment CleanCodePosInFront>CleanPos');
  MoveCursorToCleanPos(CleanCodePosInFront);
  repeat
    ReadNextAtom;
    if CurPos.StartPos>CleanPos then begin
      // CleanPos between two atoms -> parse space between for comments
      CommentStart:=CleanCodePosInFront;
      CommentEnd:=CurPos.StartPos;
      if CommentEnd>SrcLen then CommentEnd:=SrcLen+1;
      while CommentStart<CommentEnd do begin
        if IsCommentStartChar[Src[CommentStart]] then begin
          CurCommentPos:=CommentStart;
          case Src[CurCommentPos] of
          '{': // pascal comment
            begin
              CommentLvl:=1;
              inc(CurCommentPos);
              while (CurCommentPos<CommentEnd) and (CommentLvl>0) do begin
                case Src[CurCommentPos] of
                '{': if Scanner.NestedComments then inc(CommentLvl);
                '}': dec(CommentLvl);
                end;
                inc(CurCommentPos);
              end;
            end;
          '/':  // Delphi comment
            if (CurCommentPos<CommentEnd-1) and (Src[CurCommentPos+1]='/') then
            begin
              inc(CurCommentPos,2);
              while (CurCommentPos<CommentEnd)
              and (not (Src[CurCommentPos] in [#10,#13])) do
                inc(CurCommentPos);
              inc(CurCommentPos);
              if (CurCommentPos<CommentEnd)
              and (Src[CurCommentPos] in [#10,#13])
              and (Src[CurCommentPos-1]<>Src[CurCommentPos]) then
                inc(CurCommentPos);
            end else
              break;
          '(': // old turbo pascal comment
            if (CurCommentPos<CommentEnd-1) and (Src[CurCommentPos+1]='*') then
            begin
              inc(CurCommentPos,3);
              while (CurCommentPos<CommentEnd)
              and ((Src[CurCommentPos-1]<>'*') or (Src[CurCommentPos]<>')'))
              do
                inc(CurCommentPos);
              inc(CurCommentPos);
            end else
              break;
          end;
          if (CurCommentPos>CommentStart) and (CleanPos<CurCommentPos) then
          begin
            // CleanPos in comment
            CommentEnd:=CurCommentPos;
            Result:=true;
            exit;
          end;
          CommentStart:=CurCommentPos;
        end else if IsSpaceChar[Src[CommentStart]] then begin
          repeat
            inc(CommentStart);
          until (CommentStart>=CommentEnd)
          or (not (IsSpaceChar[Src[CommentStart]]));
        end else begin
          break;
        end;
      end;
    end else if CurPos.EndPos>CleanPos then begin
      // CleanPos not in a comment
      exit;
    end;
    CleanCodePosInFront:=CurPos.EndPos;
  until CurPos.StartPos>=SrcLen;
end;

procedure TPascalParserTool.BuildTreeAndGetCleanPos(
  TreeRange: TTreeRange; const CursorPos: TCodeXYPosition;
  var CleanCursorPos: integer; BuildTreeFlags: TBuildTreeFlags);
var
  Dummy: integer;
  IgnorePos: TCodePosition;
begin
  if (btSetIgnoreErrorPos in BuildTreeFlags) then begin
    if (CursorPos.Code<>nil) then begin
      IgnorePos.Code:=CursorPos.Code;
      IgnorePos.Code.LineColToPosition(CursorPos.Y,CursorPos.X,IgnorePos.P);
      if IgnorePos.P<1 then IgnorePos.Code:=nil;
      IgnoreErrorAfter:=IgnorePos;
    end else
      ClearIgnoreErrorAfter;
  end
  else if (btKeepIgnoreErrorPos in BuildTreeFlags) then
    ClearIgnoreErrorAfter;
    
  if (TreeRange=trTillCursor) and (not UpdateNeeded(true)) then begin
    // interface tree is valid
    // -> if there was an error, raise it again
    if (LastErrorPhase in [CodeToolPhaseScan,CodeToolPhaseParse])
    and ((not IgnoreErrorAfterValid)
      or (not IgnoreErrAfterPositionIsInFrontOfLastErrMessage))
    then
      RaiseLastError;
    // check if cursor is in interface
    Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
    if (Dummy=0) or (Dummy=-1) then
      exit;
  end;
  BuildTree(TreeRange=trInterface);
  if (not IgnoreErrorAfterValid) and (not EndOfSourceFound) then
    SaveRaiseException(ctsEndOfSourceNotFound);
  // find the CursorPos in cleaned source
  Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (Dummy<>0) and (Dummy<>-1) then
    RaiseException(ctsCursorPosOutsideOfCode);
end;

function TPascalParserTool.FindTypeNodeOfDefinition(
  DefinitionNode: TCodeTreeNode): TCodeTreeNode;
// for example: 'var a,b,c: integer;'  only c has a type child
begin
  Result:=DefinitionNode;
  while (Result<>nil)
  and (Result.Desc in AllIdentifierDefinitions) do begin
    if (Result.FirstChild<>nil) then begin
      Result:=Result.FirstChild;
      if (Result<>nil) and (not (Result.Desc in AllPascalTypes)) then
        Result:=nil;
      exit;
    end;
    Result:=Result.NextBrother;
  end;
end;

function TPascalParserTool.ReadTilTypeOfProperty(
  PropertyNode: TCodeTreeNode): boolean;
begin
  MoveCursorToNodeStart(PropertyNode);
  ReadNextAtom; // read keyword 'property'
  ReadNextAtom; // read property name
  AtomIsIdentifier(true);
  ReadNextAtom;
  if (CurPos.Flag=cafEdgedBracketOpen) then begin
    // read parameter list
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if (CurPos.Flag<>cafColon) then begin
    Result:=false;
    exit;
  end;
  ReadNextAtom; // read type
  AtomIsIdentifier(true);
  Result:=true;
end;

procedure TPascalParserTool.ReadGUID;

  procedure RaiseStringConstantExpected;
  begin
    RaiseStringExpectedButAtomFound(ctsStringConstant);
  end;

begin
  CreateChildNode;
  CurNode.Desc:=ctnClassGUID;
  // read GUID
  ReadNextAtom;
  if not AtomIsStringConstant then
    RaiseStringConstantExpected;
  ReadNextAtom;
  if CurPos.Flag<>cafEdgedBracketClose then
    RaiseCharExpectedButAtomFound(']');
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
end;

function TPascalParserTool.PropertyIsDefault(PropertyNode: TCodeTreeNode
  ): boolean;
begin
  Result:=false;
  if (PropertyNode=nil) or (PropertyNode.Desc<>ctnProperty) then exit;
  MoveCursorToCleanPos(PropertyNode.EndPos);
  ReadPriorAtom;
  if (CurPos.Flag<>cafSemicolon) then exit;
  ReadPriorAtom;
  Result:=UpAtomIs('DEFAULT');
end;

function TPascalParserTool.PropertyNodeHasParamList(PropNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, ppw, dcu

  Result:=false;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'property'
  ReadNextAtom; // read name
  ReadNextAtom;
  Result:=(CurPos.Flag=cafEdgedBracketOpen);
end;

function TPascalParserTool.PropNodeIsTypeLess(PropNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, ppw, dcu

  Result:=false;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'property'
  ReadNextAtom; // read name
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  Result:=(CurPos.Flag<>cafColon);
end;

function TPascalParserTool.ProcNodeHasParamList(ProcNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, ppw, dcu

  Result:=false;
  if ProcNode.Desc=ctnProcedure then
    ProcNode:=ProcNode.FirstChild;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom; // read name
  ReadNextAtom;
  if AtomIsChar('.') then begin
    ReadNextAtom;
    ReadNextAtom;
  end;
  Result:=AtomIsChar('(');
end;

procedure TPascalParserTool.MoveCursorToUsesEnd(UsesNode: TCodeTreeNode);
begin
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection) then
    RaiseException('[TPascalParserTool.MoveCursorToUsesEnd] '
      +'internal error: invalid UsesNode');
  // search backwards through the uses section
  MoveCursorToCleanPos(UsesNode.EndPos);
  ReadPriorAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[';',GetAtom]);
end;

procedure TPascalParserTool.ReadPriorUsedUnit(var UnitNameAtom,
  InAtom: TAtomPosition);
begin
  ReadPriorAtom; // read unitname
  if AtomIsStringConstant then begin
    InAtom:=CurPos;
    ReadPriorAtom; // read 'in'
    if not UpAtomIs('IN') then
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsKeywordIn,GetAtom]);
    ReadPriorAtom; // read unitname
  end else
    InAtom.StartPos:=-1;
  AtomIsIdentifier(true);
  UnitNameAtom:=CurPos;
end;

procedure TPascalParserTool.MoveCursorToFirstProcSpecifier(
  ProcNode: TCodeTreeNode);
// After the call,
// CurPos will stand on the first proc specifier or on a semicolon
begin
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure) then begin
    SaveRaiseException('Internal Error in'
      +' TPascalParserTool.MoveCursorFirstProcSpecifier: '
      +' (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure)');
  end;
  MoveCursorToNodeStart(ProcNode.FirstChild);
  ReadNextAtom;
  if AtomIsIdentifier(false) then begin
    // read name
    ReadNextAtom;
    if (CurPos.Flag=cafPoint) then begin
      // read method name
      ReadNextAtom;
      ReadNextAtom;
    end;
  end;
  if (CurPos.Flag=cafRoundBracketOpen) then begin
    // read paramlist
    ReadTilBracketClose(false);
    ReadNextAtom;
  end;
  if (CurPos.Flag=cafColon) then begin
    // read function result type
    ReadNextAtom;
    ReadNextAtom;
  end;
  // CurPos now stands on the first proc specifier or on a semicolon
end;

function TPascalParserTool.MoveCursorToProcSpecifier(ProcNode: TCodeTreeNode;
  ProcSpec: TProcedureSpecifier): boolean;
begin
  MoveCursorToFirstProcSpecifier(ProcNode);
  while (CurPos.StartPos<=ProcNode.FirstChild.EndPos) do begin
    if CurPos.Flag=cafSemicolon then begin
      ReadNextAtom;
    end else begin
      if UpAtomIs(ProcedureSpecifierNames[ProcSpec]) then begin
        Result:=true;
        exit;
      end;
      if (CurPos.Flag=cafEdgedBracketOpen) then begin
        ReadTilBracketClose(false);
        ReadNextAtom;
      end else if UpAtomIs('MESSAGE') then begin
        ReadNextAtom;
        ReadConstant(true,false,[]);
      end else if UpAtomIs('EXTERNAL') then begin
        ReadNextAtom;
        if CurPos.Flag<>cafSemicolon then begin
          if not UpAtomIs('NAME') then
            ReadConstant(true,false,[]);
          if UpAtomIs('NAME') or UpAtomIs('INDEX') then begin
            ReadNextAtom;
            ReadConstant(true,false,[]);
          end;
        end;
      end else begin
        ReadNextAtom;
      end;
    end;
  end;
  Result:=false;
end;

function TPascalParserTool.ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
  ProcSpec: TProcedureSpecifier): boolean;
begin

  // ToDo: ppu, ppw, dcu

  Result:=MoveCursorToProcSpecifier(ProcNode,ProcSpec);
end;

function TPascalParserTool.GetProcNameIdentifier(ProcNode: TCodeTreeNode
  ): PChar;
begin

  // ToDo: ppu, ppw, dcu

  Result:=nil;
  if ProcNode=nil then exit;
  if ProcNode.Desc=ctnProcedure then begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
  end;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  Result:=@Src[CurPos.StartPos];
  ReadNextAtom;
  if not AtomIsChar('.') then exit;
  ReadNextAtom;
  Result:=@Src[CurPos.StartPos];
end;

function TPascalParserTool.GetPropertyNameIdentifier(PropNode: TCodeTreeNode
  ): PChar;
begin

  // ToDo: ppu, ppw, dcu

  Result:=nil;
  if PropNode=nil then exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'propery'
  ReadNextAtom; // read name
  Result:=@Src[CurPos.StartPos];
end;

function TPascalParserTool.ClassSectionNodeStartsWithWord(
  ANode: TCodeTreeNode): boolean;
var p: integer;
begin
  Result:=false;
  if ANode=nil then exit;
  p:=ANode.StartPos;
  while (p<ANode.EndPos) and (IsIdentChar[Src[p]]) do inc(p);
  if (p=ANode.StartPos) then exit;
  Result:=true;
end;

procedure TPascalParserTool.BuildSubTreeForProcHead(ProcNode: TCodeTreeNode);
var HasForwardModifier, IsFunction, IsOperator, IsMethod: boolean;
  ParseAttr: TParseProcHeadAttributes;
  OldPhase: integer;
begin
  OldPhase:=CurrentPhase;
  CurrentPhase:=CodeToolPhaseParse;
  try
    if ProcNode.Desc=ctnProcedureHead then ProcNode:=ProcNode.Parent;
    if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure)
    or (ProcNode.FirstChild=nil) then
      SaveRaiseException('[TPascalParserTool.BuildSubTreeForProcHead] '
        +'internal error: invalid ProcNode');
    if (ProcNode.FirstChild.SubDesc and ctnsNeedJITParsing)=0 then exit;
    IsMethod:=ProcNode.HasParentOfType(ctnClass);
    MoveCursorToNodeStart(ProcNode);
    ReadNextAtom;
    if UpAtomIs('CLASS') then
      ReadNextAtom;
    IsFunction:=UpAtomIs('FUNCTION');
    IsOperator:=UpAtomIs('OPERATOR');
    // read procedure head (= name + parameterlist + resulttype;)
    CurNode:=ProcNode.FirstChild;
    ReadNextAtom;// read first atom of head
    if IsOperator then AtomIsIdentifier(true);
    ReadNextAtom;
    if (CurPos.Flag=cafPoint) then begin
      // read procedure name of a class method (the name after the . )
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
    end;
    // read rest of procedure head and build nodes
    HasForwardModifier:=false;
    ParseAttr:=[pphCreateNodes];
    if IsMethod then Include(ParseAttr,pphIsMethod);
    if IsFunction then Include(ParseAttr,pphIsFunction);
    if IsOperator then Include(ParseAttr,pphIsOperator);
    ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
    ProcNode.FirstChild.SubDesc:=ctnsNone;
  finally
    CurrentPhase:=OldPhase;
  end;
end;

procedure TPascalParserTool.BuildSubTreeForProcHead(ProcNode: TCodeTreeNode;
  var FunctionResult: TCodeTreeNode);
begin
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Desc<>ctnProcedure then
    RaiseException('INTERNAL ERROR: TPascalParserTool.BuildSubTreeForProcHead');
  BuildSubTreeForProcHead(ProcNode);
  FunctionResult:=ProcNode.FirstChild.FirstChild;
  if (FunctionResult<>nil) and (FunctionResult.Desc=ctnParameterList) then
    FunctionResult:=FunctionResult.NextBrother;
end;


end.



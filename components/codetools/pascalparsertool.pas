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
    TMultiKeyWordListCodeTool enhances the TCustomCodeTool with the ability
    to switch the KeyWord list and keep a list of KeyWord lists.
    
    TPascalParserTool enhances TMultiKeyWordListCodeTool.
    This tool parses the pascal code, makes simple syntax checks and provides
    a lot of useful parsing functions. It can either parse complete sources
    or parts of it.
    

  ToDo:
    - ReadBackTilBlockEnd: case could also be in a record, then it should not
        close the block
    - BuildSubTreeForBeginBlock: building case statement nodes

}
unit PascalParserTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, CustomCodeTool, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  SourceChanger;

type
  TMultiKeyWordListCodeTool = class(TCustomCodeTool)
  private
    FKeyWordLists: TList; // list of TKeyWordFunctionList
    FCurKeyWordListID: integer;
    procedure SetCurKeyWordFuncList(AKeyWordFuncList: TKeyWordFunctionList);
  protected
    procedure SetKeyWordListID(NewID: integer);
  public
    DefaultKeyWordFuncList: TKeyWordFunctionList;
    property KeyWordListID: integer read FCurKeyWordListID write SetKeyWordListID;
    property CurKeyWordFuncList: TKeyWordFunctionList
       read KeyWordFuncList write SetCurKeyWordFuncList;
    function AddKeyWordFuncList(AKeyWordFuncList: TKeyWordFunctionList): integer;
    procedure ClearKeyWordFuncLists;

    constructor Create;
    destructor Destroy; override;
  end;

  TProcHeadAttribute = (phpWithStart, phpAddClassname, phpWithoutClassName,
      phpWithoutName, phpWithVarModifiers, phpWithParameterNames,
      phpWithDefaultValues, phpWithResultType, phpWithComments, phpInUpperCase,
      phpWithoutBrackets, phpIgnoreForwards, phpIgnoreProcsWithBody,
      phpOnlyWithClassname, phpFindCleanPosition, phpWithoutParamList,
      phpCreateNodes);
  TProcHeadAttributes = set of TProcHeadAttribute;
  
  TProcHeadExtractPos = (phepNone, phepStart, phepName, phepParamList);

  TPascalParserTool = class(TMultiKeyWordListCodeTool)
  private
  protected
    EndKeyWordFuncList: TKeyWordFunctionList;
    TypeKeyWordFuncList: TKeyWordFunctionList;
    PackedTypesKeyWordFuncList: TKeyWordFunctionList;
    InnerClassKeyWordFuncList: TKeyWordFunctionList;
    ClassVarTypeKeyWordFuncList: TKeyWordFunctionList;
    BlockStatementStartKeyWordFuncList: TKeyWordFunctionList;
    ExtractMemStream: TMemoryStream;
    ExtractSearchPos: integer;
    ExtractFoundPos: integer;
    ExtractProcHeadPos: TProcHeadExtractPos;
    procedure InitExtraction;
    function GetExtraction: string;
    procedure ExtractNextAtom(AddAtom: boolean; Attr: TProcHeadAttributes);
    // sections
    function KeyWordFuncSection: boolean;
    function KeyWordFuncEndPoint: boolean;
    // type/var/const/resourcestring
    function KeyWordFuncType: boolean;
    function KeyWordFuncVar: boolean;
    function KeyWordFuncConst: boolean;
    function KeyWordFuncResourceString: boolean;
    // types
    function KeyWordFuncClass: boolean;
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
    procedure BuildEndKeyWordFunctions; virtual;
    procedure BuildTypeKeyWordFunctions; virtual;
    procedure BuildPackedTypesKeyWordFunctions; virtual;
    procedure BuildInnerClassKeyWordFunctions; virtual;
    procedure BuildClassVarTypeKeyWordFunctions; virtual;
    procedure BuildBlockStatementStartKeyWordFuncList; virtual;
    function UnexpectedKeyWord: boolean;
    // read functions
    function ReadTilProcedureHeadEnd(IsMethod, IsFunction, IsType, IsOperator,
        CreateNodes: boolean;
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
    procedure ReadTilTypeOfProperty(PropertyNode: TCodeTreeNode);
  public
    CurSection: TCodeTreeNodeDesc;

    InterfaceSectionFound: boolean;
    ImplementationSectionFound: boolean;
    EndOfSourceFound: boolean;

    function CleanPosIsInComment(CleanPos, CleanCodePosInFront: integer;
        var CommentStart, CommentEnd: integer): boolean;
    procedure BuildTree(OnlyInterfaceNeeded: boolean); virtual;
    procedure BuildTreeAndGetCleanPos(OnlyInterfaceNeeded: boolean;
        CursorPos: TCodeXYPosition; var CleanCursorPos: integer);
    procedure BuildSubTreeForClass(ClassNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForBeginBlock(BeginNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForProcHead(ProcNode: TCodeTreeNode); virtual;
    function DoAtom: boolean; override;
    function ExtractPropName(PropNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractProcName(ProcNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractProcHead(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractClassName(ClassNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode): string;
    function FindProcNode(StartNode: TCodeTreeNode; const AProcHead: string;
        Attr: TProcHeadAttributes): TCodeTreeNode;
    function FindProcBody(ProcNode: TCodeTreeNode): TCodeTreeNode;
    function FindVarNode(StartNode: TCodeTreeNode;
        const UpperVarName: string): TCodeTreeNode;
    function FindFirstNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    function FindNextNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    function FindClassNode(StartNode: TCodeTreeNode;
        const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindClassNodeInInterface(const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function FindInterfaceNode: TCodeTreeNode;
    function FindImplementationNode: TCodeTreeNode;
    function FindInitializationNode: TCodeTreeNode;
    function FindMainBeginEndNode: TCodeTreeNode;
    function FindTypeNodeOfDefinition(
        DefinitionNode: TCodeTreeNode): TCodeTreeNode;
    function GetSourceType: TCodeTreeNodeDesc;
    function NodeHasParentOfType(ANode: TCodeTreeNode;
        NodeDesc: TCodeTreeNodeDesc): boolean;
    function PropertyIsDefault(PropertyNode: TCodeTreeNode): boolean;
    
    constructor Create;
    destructor Destroy; override;
  end;

  

implementation


type
  TEndBlockType = (ebtBegin, ebtAsm, ebtTry, ebtCase, ebtRepeat, ebtRecord,
                   ebtClass, ebtObject);
  TTryType = (ttNone, ttFinally, ttExcept);
  

{ TMultiKeyWordListCodeTool }

constructor TMultiKeyWordListCodeTool.Create;
begin
  inherited Create;
  FKeyWordLists:=TList.Create; // list of TKeyWordFunctionList
  AddKeyWordFuncList(KeyWordFuncList);
  FCurKeyWordListID:=0;
  DefaultKeyWordFuncList:=KeyWordFuncList;
end;

destructor TMultiKeyWordListCodeTool.Destroy;
begin
  ClearKeyWordFuncLists;
  FKeyWordLists.Free;
  inherited Destroy;
end;

procedure TMultiKeyWordListCodeTool.SetKeyWordListID(NewID: integer);
begin
  if FCurKeyWordListID=NewID then exit;
  FCurKeyWordListID:=NewID;
  KeyWordFuncList:=TKeyWordFunctionList(FKeyWordLists[NewID]);
end;

procedure TMultiKeyWordListCodeTool.SetCurKeyWordFuncList(
  AKeyWordFuncList: TKeyWordFunctionList);
var i: integer;
begin
  i:=0;
  while i<FKeyWordLists.Count do begin
    if TKeyWordFunctionList(FKeyWordLists[i])=AKeyWordFuncList then begin
      SetKeyWordListID(i);
      exit;
    end;
    inc(i);
  end;
  RaiseException(
    '[TMultiKeyWordListCodeTool.SetCurKeyWordFuncList] unknown list');
end;

function TMultiKeyWordListCodeTool.AddKeyWordFuncList(
  AKeyWordFuncList: TKeyWordFunctionList): integer;
begin
  Result:=FKeyWordLists.Add(AKeyWordFuncList);
end;

procedure TMultiKeyWordListCodeTool.ClearKeyWordFuncLists;
var i: integer;
begin
  KeyWordListID:=0;
  for i:=FKeyWordLists.Count-1 downto 1 do begin
    TKeyWordFunctionList(FKeyWordLists[i]).Free;
    FKeyWordLists.Delete(i);
  end;
  KeyWordFuncList.Clear;
end;


{ TPascalParserTool }

constructor TPascalParserTool.Create;
begin
  inherited Create;
  // KeyWord functions for parsing blocks (e.g. begin..end)
  EndKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildEndKeyWordFunctions;
  AddKeyWordFuncList(EndKeyWordFuncList);
  // keywords for parsing types
  TypeKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildTypeKeyWordFunctions;
  AddKeyWordFuncList(TypeKeyWordFuncList);
  PackedTypesKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildPackedTypesKeyWordFunctions;
  AddKeyWordFuncList(PackedTypesKeyWordFuncList);
  // KeyWord functions for parsing in a class
  InnerClassKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildInnerClassKeyWordFunctions;
  AddKeyWordFuncList(InnerClassKeyWordFuncList);
  ClassVarTypeKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildClassVarTypeKeyWordFunctions;
  AddKeyWordFuncList(ClassVarTypeKeyWordFuncList);
  // keywords for statements
  BlockStatementStartKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildBlockStatementStartKeyWordFuncList;
  AddKeyWordFuncList(BlockStatementStartKeyWordFuncList);
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
    Add('CONST',{$ifdef FPC}@{$endif}KeyWordFuncConst);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}KeyWordFuncResourceString);
    
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

procedure TPascalParserTool.BuildEndKeyWordFunctions;
// KeyWordFunctions for parsing end - blocks
begin
  with EndKeyWordFuncList do begin
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;

procedure TPascalParserTool.BuildTypeKeyWordFunctions;
// KeyWordFunctions for parsing types
begin
  with TypeKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('OBJECT',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('INTERFACE',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}KeyWordFuncClass);
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

procedure TPascalParserTool.BuildPackedTypesKeyWordFunctions;
// KeyWordFunctions for valid packed types
begin
  with PackedTypesKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
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

procedure TPascalParserTool.BuildBlockStatementStartKeyWordFuncList;
begin
  with BlockStatementStartKeyWordFuncList do begin
    Add('BEGIN' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;

function TPascalParserTool.UnexpectedKeyWord: boolean;
begin
  Result:=false;
  RaiseException('unexpected word "'+GetAtom+'"');
end;

procedure TPascalParserTool.BuildTree(OnlyInterfaceNeeded: boolean);
begin
{$IFDEF MEM_CHECK}
CheckHeap('TBasicCodeTool.BuildTree A '+IntToStr(GetMem_Cnt));
{$ENDIF}
{$IFDEF CTDEBUG}
writeln('TPascalParserTool.BuildTree A');
{$ENDIF}
  if not UpdateNeeded(OnlyInterfaceNeeded) then exit;
writeln('TPascalParserTool.BuildTree B OnlyInterfaceNeeded=',OnlyInterfaceNeeded,
  '  ',TCodeBuffer(Scanner.MainCode).Filename);
//CheckHeap('TBasicCodeTool.BuildTree B '+IntToStr(GetMem_Cnt));
  BeginParsing(true,OnlyInterfaceNeeded);
  InterfaceSectionFound:=false;
  ImplementationSectionFound:=false;
  EndOfSourceFound:=false;
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
    RaiseException(
      'no pascal code found (first token is '+GetAtom+')');
  CreateChildNode;
  CurNode.Desc:=CurSection;
  ReadNextAtom; // read source name
  AtomIsIdentifier(true);
  ReadNextAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseException('; expected, but '+GetAtom+' found');
  if CurSection=ctnUnit then begin
    ReadNextAtom;
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode;
    if not UpAtomIs('INTERFACE') then
      RaiseException(
        '''interface'' expected, but '+GetAtom+' found');
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
{$IFDEF CTDEBUG}
writeln('[TPascalParserTool.BuildTree] END');
{$ENDIF}
{$IFDEF MEM_CHECK}
CheckHeap('TBasicCodeTool.BuildTree END '+IntToStr(GetMem_Cnt));
{$ENDIF}
end;

procedure TPascalParserTool.BuildSubTreeForClass(ClassNode: TCodeTreeNode);
// reparse a quick parsed class and build the child nodes
begin
  if ClassNode=nil then
    RaiseException(
       'TPascalParserTool.BuildSubTreeForClass: Classnode=nil');
  if (ClassNode.FirstChild<>nil)
  or ((ClassNode.SubDesc and ctnsNeedJITParsing)=0) then
    // class already parsed
    exit;
  if ClassNode.Desc<>ctnClass then
    RaiseException('[TPascalParserTool.BuildSubTreeForClass] ClassNode.Desc='
                   +ClassNode.DescAsString);
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
    RaiseException(
        'TPascalParserTool.BuildSubTreeForClass:'
       +' class/object keyword expected, but '+GetAtom+' found');
  ReadNextAtom;
  if AtomIsChar('(') then
    // read inheritage
    ReadTilBracketClose(true)
  else
    UndoReadNextAtom;
  // clear the last atoms
  LastAtoms.Clear;
  // start the first class section
  CreateChildNode;
  CurNode.StartPos:=CurPos.EndPos;
  if UpAtomIs('PUBLIC') then
    CurNode.Desc:=ctnClassPublic
  else if UpAtomIs('PRIVATE') then
    CurNode.Desc:=ctnClassPrivate
  else if UpAtomIs('PROTECTED') then
    CurNode.Desc:=ctnClassProtected
  else begin
    CurNode.Desc:=ctnClassPublished;
    if AtomIsChar('[') then begin
      // read GUID
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseException('string constant expected, but '+GetAtom+' found');
      if not ReadNextAtomIsChar(']') then
        RaiseException('] expected, but '+GetAtom+' found');
      ReadNextAtom;
      if (not (AtomIsChar(';') or UpAtomIs('END'))) then
        RaiseException('; expected, but '+GetAtom+' found');
      ReadNextAtom;
    end;
  end;
  // parse till "end" of class/object
  CurKeyWordFuncList:=InnerClassKeyWordFuncList;
  try
    repeat
      ReadNextAtom;
      if CurPos.StartPos>=ClassNode.EndPos then break;
      if not DoAtom then break;
    until false;
    // end last class section (public, private, ...)
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode;
  finally
    CurKeyWordFuncList:=DefaultKeyWordFuncList;
  end;
  ClassNode.SubDesc:=ClassNode.SubDesc and (not ctnsNeedJITParsing);
end;

procedure TPascalParserTool.BuildSubTreeForBeginBlock(BeginNode: TCodeTreeNode);
// reparse a quick parsed begin..end block and build the child nodes
//   create nodes for 'with' and 'case' statements
var MaxPos: integer;
begin
  if BeginNode=nil then
    RaiseException(
       'TPascalParserTool.BuildSubTreeForBeginBlock: BeginNode=nil');
  if BeginNode.Desc<>ctnBeginBlock then
    RaiseException(
       'TPascalParserTool.BuildSubTreeForBeginBlock: BeginNode.Desc='
       +BeginNode.DescAsString);
  if (BeginNode.FirstChild<>nil)
  or ((BeginNode.SubDesc and ctnsNeedJITParsing)=0) then
    // block already parsed
    exit;
  // set CursorPos on 'begin'
  MoveCursorToNodeStart(BeginNode);
  ReadNextAtom;
  if not UpAtomIs('BEGIN') then
    RaiseException(
       'TPascalParserTool.BuildSubTreeForBeginBlock: begin expected, but '
       +GetAtom+' found');
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
  until (CurPos.StartPos>SrcLen) or UpAtomIs('END');
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
  while AtomIsChar(',') do begin
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
  if not AtomIsChar(':') then
    RaiseException(': expected, but '+GetAtom+' found');
  // read type
  ReadVariableType;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeClass: boolean;
// class and object as type are not allowed, because they would have no name
begin
  RaiseException(
    'Anonym '+GetAtom+' definitions are not allowed');
  Result:=false;
end;

function TPascalParserTool.KeyWordFuncClassVarTypePacked: boolean;
// 'packed' record
begin
  ReadNextAtom;
  if UpAtomIs('RECORD') then
    Result:=KeyWordFuncClassVarTypeRecord
  else begin
    RaiseException(' ''record'' expected, but '+GetAtom+' found');
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
    ReadNextAtom;
    if UpAtomIs('RECORD') then inc(Level)
    else if UpAtomIs('END') then dec(Level);
  end;
  if CurPos.StartPos>SrcLen then
    RaiseException('end for record not found.');
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeArray: boolean;
{ read variable type 'array'

  examples:
    array of array[EnumType] of array [Range] of TypeName;
}
begin
  ReadNextAtom;
  if AtomIsChar('[') then begin
    // array[Range]
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseException('[ expected, but '+GetAtom+' found');
  ReadNextAtom;
//writeln('TPascalParserTool.KeyWordFuncClassVarTypeArray ',GetAtom);
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
  ReadNextAtom;
  if not UpAtomIs('OF') then
    RaiseException('''of'' expected, but '+GetAtom+' found');
  ReadNextAtom;
  if CurPos.StartPos>SrcLen then
    RaiseException('missing enum list');
  if UpperSrc[CurPos.StartPos] in ['A'..'Z','_'] then
    // set of identifier
  else if AtomIsChar('(') then
    // set of ()
    ReadTilBracketClose(true);
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
begin
//writeln('[TPascalParserTool.KeyWordFuncClassVarTypeProc]');
  IsFunction:=UpAtomIs('FUNCTION');
  ReadNextAtom;
  HasForwardModifier:=false;
  ReadTilProcedureHeadEnd(true,IsFunction,true,false,false,HasForwardModifier);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeIdent: boolean;
// read variable type <identfier>
begin
  if CurPos.StartPos>SrcLen then
    RaiseException('missing type identifier');
  if UpperSrc[CurPos.StartPos] in ['A'..'Z','_'] then
    // identifier
  else
    RaiseException('missing type identifier');
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

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline

 proc specifiers with parameters:
   message <id or number>
}
var IsFunction, HasForwardModifier: boolean;
begin
  HasForwardModifier:=false;
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProcedure;
  // read method keyword
  if UpAtomIs('CLASS') or (UpAtomIs('STATIC')) then begin
    ReadNextAtom;
    if (not UpAtomIs('PROCEDURE')) and (not UpAtomIs('FUNCTION')) then begin
      RaiseException(
        'procedure or function expected, but '+GetAtom+' found');
    end;
  end;
  IsFunction:=UpAtomIs('FUNCTION');
  // read procedure head
  // read name
  ReadNextAtom;
  if (CurPos.StartPos>SrcLen)
  or (not (UpperSrc[CurPos.StartPos] in ['A'..'Z','_']))
  then
    RaiseException('method name expected, but '+GetAtom+' found');
  // create node for procedure head
  CreateChildNode;
  CurNode.Desc:=ctnProcedureHead;
  CurNode.SubDesc:=ctnsNeedJITParsing;
  // read rest
  ReadNextAtom;
  ReadTilProcedureHeadEnd(true,IsFunction,false,false,false,HasForwardModifier);
  // close procedure header
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  // close procedure
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.ReadParamList(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
var CloseBracket: char;
  Desc: TCodeTreeNodeDesc;
  Node: TCodeTreeNode;
begin
  Result:=false;
  if AtomIsChar('(') or AtomIsChar('[') then begin
    if AtomIsChar('(') then
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
  repeat
    // read parameter prefix modifier
    if (UpAtomIs('VAR')) or (UpAtomIs('CONST')) or (UpAtomIs('OUT')) then begin
      Desc:=ctnVarDefinition;
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(phpWithVarModifiers in Attr,Attr);
    end else
      Desc:=ctnVarDefinition;
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
      if not AtomIsChar(',') then
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
    if (AtomIsChar(':')) then begin
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
      if not ReadParamType(ExceptionOnError,Extract,Attr) then exit;
      if AtomIsChar('=') then begin
        // read default value
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
    end;
    if (phpCreateNodes in Attr) then begin
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    // read next parameter
    if (CurPos.StartPos>SrcLen) then
      if ExceptionOnError then
        RaiseException(
          ''+CloseBracket+' expected, but '+GetAtom+' found')
      else exit;
    if (Src[CurPos.StartPos] in [')',']']) then break;
    if (Src[CurPos.StartPos]<>';') then
      if ExceptionOnError then
        RaiseException(
          ''+CloseBracket+' expected, but '+GetAtom+' found')
      else exit;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
  until false;
  if (CloseBracket<>#0) then begin
    if Src[CurPos.StartPos]<>CloseBracket then
      if ExceptionOnError then
        RaiseException(
          ''+CloseBracket+' expected, but '+GetAtom+' found')
      else exit;
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
begin
  Result:=false;
  if AtomIsWord then begin
    if UpAtomIs('ARRAY') then begin
      if (phpCreateNodes in Attr) then begin
        CreateChildNode;
        CurNode.Desc:=ctnArrayType;
      end;
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      if not UpAtomIs('OF') then
        if ExceptionOnError then
          RaiseException('''of'' expected, but '+GetAtom+' found')
        else exit;
      ReadNextAtom;
      if UpAtomIs('CONST') then begin
        if (phpCreateNodes in Attr) then begin
          CreateChildNode;
          CurNode.Desc:=ctnArrayType;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
        end;
        if not Extract then
          ReadNextAtom
        else
          ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
        Result:=true;
        exit;
      end;
    end;
    if not AtomIsIdentifier(ExceptionOnError) then exit;
    if (phpCreateNodes in Attr) then begin
      CreateChildNode;
      CurNode.Desc:=ctnIdentifier;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
  end else begin
    if ExceptionOnError then
      RaiseException(
        'identifier expected, but '+GetAtom+' found')
    else exit;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadTilProcedureHeadEnd(
  IsMethod, IsFunction, IsType, IsOperator, CreateNodes: boolean;
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

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline

 proc specifiers with parameters:
   message <id or number>
   external <id or number> name <id>
   external <id or number> index <id>
   [alias: <string constant>]
}
var IsSpecifier: boolean;
  Attr: TProcHeadAttributes;
begin
//writeln('[TPascalParserTool.ReadTilProcedureHeadEnd] ',
//'Method=',IsMethod,', Function=',IsFunction,', Type=',IsType);
  Result:=true;
  HasForwardModifier:=false;
  if AtomIsChar('(') then begin
    Attr:=[];
    if CreateNodes then
      Include(Attr,phpCreateNodes);
    ReadParamList(true,false,Attr);
  end;
  if IsOperator and (not AtomIsChar(':')) then begin
    // read operator result identifier
    AtomIsIdentifier(true);
    if CreateNodes then begin
      CreateChildNode;
      CurNode.Desc:=ctnVarDefinition;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    ReadNextAtom;
  end;
  if IsFunction or IsOperator then begin
    // read function result type
    if not AtomIsChar(':') then
      RaiseException(': expected, but '+GetAtom+' found');
    ReadNextAtom;
    AtomIsIdentifier(true);
    if CreateNodes then begin
      CreateChildNode;
      CurNode.Desc:=ctnIdentifier;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    ReadNextAtom;
  end;
  if UpAtomIs('OF') then begin
    // read 'of object'
    if not IsType then
      RaiseException(
        '; expected, but '+GetAtom+' found');
    ReadNextAtom;
    if not UpAtomIs('OBJECT') then
      RaiseException('"object" expected, but '+GetAtom+' found');
    ReadNextAtom;
  end;
  // read procedures/method specifiers
  if UpAtomIs('END') then begin
    UndoReadNextAtom;
    exit;
  end;
  if not AtomIsChar(';') then
    RaiseException('; expected, but '+GetAtom+' found');
  if (CurPos.StartPos>SrcLen) then
    RaiseException('semicolon not found');
  repeat
    ReadNextAtom;
    if IsMethod then
      IsSpecifier:=IsKeyWordMethodSpecifier.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    else
      IsSpecifier:=IsKeyWordProcedureSpecifier.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
    if IsSpecifier then begin
      // read specifier
      if UpAtomIs('MESSAGE') or UpAtomIs('EXTERNAL') then begin
        if UpAtomIs('EXTERNAL') then
          HasForwardModifier:=true;
        repeat
          ReadNextAtom;
          if UpAtomIs('END') then begin
            UndoReadNextAtom;
            exit;
          end;
        until (CurPos.Startpos>SrcLen) or AtomIsChar(';');
      end else if AtomIsChar('[') then begin
        // read assembler alias   [public,alias: 'alternative name']
        repeat
          ReadNextAtom;
          if not AtomIsWord then
            RaiseException('keyword expected, but '+GetAtom+' found');
          if not IsKeyWordProcedureBracketSpecifier.DoItUppercase(UpperSrc,
            CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
          then
            RaiseException('keyword (e.g. alias) expected, but '+GetAtom+' found');
          if UpAtomIs('INTERNPROC') then
            HasForwardModifier:=true;
          ReadNextAtom;
          if AtomIsChar(':') or AtomIsChar(']') then
            break;
          if not AtomIsChar(',') then
            RaiseException(': expected, but '+GetAtom+' found');
        until false;
        if AtomIsChar(':') then begin
          ReadNextAtom;
          if (not AtomIsStringConstant) and (not AtomIsIdentifier(false)) then
            RaiseException('string constant expected, but '+GetAtom+' found');
          ReadConstant(true,false,[]);
        end;
        if not AtomIsChar(']') then
          RaiseException('] expected, but '+GetAtom+' found');
        ReadNextAtom;
        if UpAtomIs('END') then begin
          UndoReadNextAtom;
          exit;
        end;
      end else begin
        // read specifier without parameters
        if UpAtomIs('FORWARD') then HasForwardModifier:=true;
        ReadNextAtom;
        if UpAtomIs('END') then begin
          UndoReadNextAtom;
          exit;
        end;
      end;
      if not AtomIsChar(';') then
        RaiseException('; expected, but '+GetAtom+' found');
    end else begin
      // current atom does not belong to procedure/method declaration
      UndoReadNextAtom;
      UndoReadNextAtom;
      break;
    end;
  until false;
end;

function TPascalParserTool.ReadConstant(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
// after reading, the CurPos will be on the atom after the constant
var c: char;
begin
  Result:=false;
  if AtomIsWord then begin
    // word (identifier or keyword)
    if AtomIsKeyWord and (not IsKeyWordInConstAllowed.DoItUppercase(UpperSrc,
            CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)) then begin
      if ExceptionOnError then
        RaiseException('unexpected keyword '+GetAtom+' found')
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
    end else if AtomIsChar('(') or AtomIsChar('[') then begin
      // type cast or constant array
      c:=Src[CurPos.StartPos];
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
      if (c='(') and (not AtomIsChar(')')) then
        if ExceptionOnError then
          RaiseException('( expected, but '+GetAtom+' found')
        else exit;
      if (c='[') and (not AtomIsChar(']')) then
        if ExceptionOnError then
          RaiseException('[ expected, but '+GetAtom+' found')
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
        '(','[':
          begin
            // open bracket + ? + close bracket
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
            if (c='(') and (not AtomIsChar(')')) then
              if ExceptionOnError then
                RaiseException(
                  '( expected, but '+GetAtom+' found')
              else exit;
            if (c='[') and (not AtomIsChar(']')) then
              if ExceptionOnError then
                RaiseException(
                  '[ expected, but '+GetAtom+' found')
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
        '+','-':
          begin
            // sign
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
          end;
      else
        if ExceptionOnError then
          RaiseException(
            'constant expected, but '+GetAtom+' found')
        else exit;
      end;
    end else
      // syntax error
      if ExceptionOnError then
        RaiseException(
          'constant expected, but '+GetAtom+' found')
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
    if AtomIsChar(';') then break;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        if ExceptionOnError then
          RaiseException(
            'string constant expected, but '+GetAtom+' found')
        else exit;
      ReadNextAtom;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then
      if ExceptionOnError then
        RaiseException(
          '; expected, but '+GetAtom+' found')
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
    if AtomIsChar(';') or AtomIsChar(')') or AtomIsChar(']') or AtomIsChar(',')
    or AtomIsChar(':') then break;
    if AtomIs('..') then begin
      if RangeOpFound then
        RaiseException('; expected, but '+GetAtom+' found');
      RangeOpFound:=true;
    end else if AtomIsChar('(') or AtomIsChar('[') then
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
   property Col8: ICol8 read FCol8 write FCol8 implements ICol8;

 property specifiers without parameters:
   default, nodefault

 property specifiers with parameters:
   index <id or number>, read <id>, write <id>, implements <id>, stored <id>
}
begin
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProperty;
  // read property Name
  ReadNextAtom;
  AtomIsIdentifier(true);
  ReadNextAtom;
  if AtomIsChar('[') then begin
    // read parameter list
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  while (CurPos.StartPos<=SrcLen) and (not AtomIsChar(';')) do
    ReadNextAtom;
  ReadNextAtom;
  if UpAtomIs('DEFAULT') then begin
    if not ReadNextAtomIsChar(';') then
      RaiseException('; expected after "default" property '
          +'specifier, but '+GetAtom+' found');
  end else if UpAtomIs('NODEFAULT') then begin
    if not ReadNextAtomIsChar(';') then
      RaiseException('; expected after "nodefault" property '
          +'specifier, but '+GetAtom+' found');
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
  if (CurPos.StartPos>SrcLen) or (CurPos.EndPos<=CurPos.StartPos) then
    Result:=false
  else if IsIdentStartChar[Src[CurPos.StartPos]] then
    Result:=CurKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
                                    CurPos.EndPos-CurPos.StartPos)
  else begin
    if Src[CurPos.StartPos] in ['(','['] then
      ReadTilBracketClose(true);
    Result:=true;
  end;
end;

function TPascalParserTool.KeyWordFuncSection: boolean;
// parse section keywords (program, unit, interface, implementation, ...)
begin
  case CurSection of
   ctnInterface, ctnProgram, ctnPackage, ctnLibrary, ctnUnit:
    begin
      if (UpAtomIs('INTERFACE')) and (LastAtomIs(1,'=')) then begin
        Result:=KeyWordFuncClass();
        exit;
      end;
      if not ((CurSection=ctnInterface) and UpAtomIs('IMPLEMENTATION')) then
        RaiseException('unexpected keyword '+GetAtom+' found');
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
        RaiseException('unexpected keyword '+GetAtom+' found');
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
        ReadNextAtom;
        if (CurSection=ctnInitialization) and UpAtomIs('FINALIZATION') then
        begin
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
          CreateChildNode;
          CurNode.Desc:=ctnFinalization;
          CurSection:=CurNode.Desc;
        end else if UpAtomIs('END') then begin
          Result:=KeyWordFuncEndPoint;
          break;
        end;
      until (CurPos.StartPos>SrcLen);
      Result:=true;
    end;
  else
    begin
      RaiseException('unexpected keyword '+GetAtom+' found');
      Result:=false;
    end;
  end;
end;

function TPascalParserTool.KeyWordFuncEndPoint: boolean;
// keyword 'end' or '.'  (source end.)
begin
  if AtomIsChar('.') then begin
    if not LastUpAtomIs(0,'END') then
      RaiseException('illegal qualifier');
    UndoReadNextAtom;
    if CurNode.Desc in [ctnInterface] then
      RaiseException('implementation expected, but '+GetAtom+' found');
    if not (CurNode.Desc in [ctnImplementation,ctnInitialization,
      ctnFinalization,ctnProgram])
    then begin
      ReadNextAtom;
      RaiseException('unexpected end of source');
    end;
  end else if UpAtomIs('END') then begin
    if LastAtomIs(0,'@') then
      RaiseException('identifier expected but keyword end found');
    if LastAtomIs(0,'@@') then begin
      // for Delphi compatibility @@end is allowed
      Result:=true;
      exit;
    end;
  end else
    RaiseException('[TPascalParserTool.KeyWordFuncEndPoint] internal error');
  if CurNode.Desc in [ctnImplementation,ctnInterface] then
    CurNode.EndPos:=CurPos.StartPos
  else
    CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  if not AtomIsChar('.') then
    RaiseException('. expected, but '+GetAtom+' found');
  CurSection:=ctnNone;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncProc: boolean;
// procedure, function, constructor, destructor, operator
var ChildCreated: boolean;
  IsFunction, HasForwardModifier, IsClassProc, IsOperator: boolean;
  ProcNode: TCodeTreeNode;
begin
  if UpAtomIs('CLASS') then begin
    if CurSection<>ctnImplementation then
      RaiseException(
        'identifier expected, but '+GetAtom+' found');
    ReadNextAtom;
    if UpAtomIs('PROCEDURE') or UpAtomIs('FUNCTION') then
      IsClassProc:=true
    else
      RaiseException(
        '"procedure" expected, but '+GetAtom+' found');
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
  if (CurSection<>ctnInterface) and (AtomIsChar('.')) then begin
    // read procedure name of a class method (the name after the . )
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  // read rest of procedure head
  HasForwardModifier:=false;
  ReadTilProcedureHeadEnd(false,IsFunction,false,IsOperator,false,
                          HasForwardModifier);
  if ChildCreated then begin
    if HasForwardModifier then
      ProcNode.SubDesc:=ctnsForwardDeclaration;
    // close head
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if ChildCreated and ((ProcNode.SubDesc and ctnsForwardDeclaration)>0) then begin
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
  
  procedure RaiseExceptionWithBlockStartHint(const AMessage: string);
  var CaretXY: TCodeXYPosition;
  begin
    if (CleanPosToCaret(BlockStartPos,CaretXY))
    and (CaretXY.Code<>nil) then begin
      if CaretXY.Code=TCodeBuffer(Scanner.MainCode) then
        RaiseException(AMessage+'. start at '
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')')
      else
        RaiseException(AMessage+'. start at '
          +TCodeBuffer(CaretXY.Code).Filename
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')');
    end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
      RaiseException(AMessage);
    end;
  end;
  
begin
  Result:=true;
  TryType:=ttNone;
  if UpAtomIs('BEGIN') then
    BlockType:=ebtBegin
  else if UpAtomIs('REPEAT') then
    BlockType:=ebtRepeat
  else if UpAtomIs('TRY') then
    BlockType:=ebtTry
  else if UpAtomIs('CASE') then
    BlockType:=ebtCase
  else if UpAtomIs('ASM') then
    BlockType:=ebtAsm
  else if UpAtomIs('RECORD') then
    BlockType:=ebtRecord
  else
    RaiseException('internal codetool error in '
      +'TPascalParserTool.ReadTilBlockEnd: unkown block type');
  BlockStartPos:=CurPos.StartPos;
  repeat
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen) then begin
      RaiseExceptionWithBlockStartHint('"end" not found')
    end else if (UpAtomIs('END')) then begin
      if BlockType=ebtRepeat then
        RaiseExceptionWithBlockStartHint(
          '"until" expected, but "'+GetAtom+'" found');
      if (BlockType=ebtTry) and (TryType=ttNone) then
        RaiseExceptionWithBlockStartHint(
          '"finally" expected, but "'+GetAtom+'" found');
      ReadNextAtom;
      if AtomIsChar('.')
      and (BlockType<>ebtBegin) then begin
        RaiseExceptionWithBlockStartHint('; expected, but . found');
      end;
      UndoReadNextAtom;
      break;
    end else if EndKeyWordFuncList.DoItUppercase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos)
      or UpAtomIs('REPEAT') then
    begin
      if BlockType=ebtAsm then
        RaiseException('unexpected keyword "'+GetAtom+'" found');
      if (BlockType<>ebtRecord) or (not UpAtomIs('CASE')) then
        ReadTilBlockEnd(false,CreateNodes);
    end else if UpAtomIs('UNTIL') then begin
      if BlockType=ebtRepeat then
        break;
      RaiseExceptionWithBlockStartHint(
        '"end" expected, but "'+GetAtom+'" found');
    end else if UpAtomIs('FINALLY') then begin
      if (BlockType=ebtTry) and (TryType=ttNone) then begin
        if StopOnBlockMiddlePart then break;
        TryType:=ttFinally;
      end else
        RaiseExceptionWithBlockStartHint(
          '"end" expected, but "'+GetAtom+'" found');
    end else if UpAtomIs('EXCEPT') then begin
      if (BlockType=ebtTry) and (TryType=ttNone) then begin
        if StopOnBlockMiddlePart then break;
        TryType:=ttExcept;
      end else
        RaiseExceptionWithBlockStartHint(
          '"end" expected, but "'+GetAtom+'" found');
    end else if CreateNodes and UpAtomIs('WITH') then begin
      ReadWithStatement(true,CreateNodes);
    end else begin
      // check for unexpected keywords
      case BlockType of
      
      ebtBegin,ebtAsm,ebtTry,ebtCase,ebtRepeat:
        if UnexpectedKeyWordInBeginBlock.DoItUppercase(UpperSrc,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
        then
          RaiseException('unexpected keyword '+GetAtom+' found');
          
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
        RaiseException('"begin" expected, but "'
                       +GetAtom+'" found');
      ebtTry:
        RaiseException('"try" expected, but "'
                       +GetAtom+'" found');
      ebtRepeat:
        RaiseException('"repeat" expected, but "'
                       +GetAtom+'" found');
    else
      RaiseException('unexpected keyword "'+GetAtom+'" found');
    end;
  end;

var OldAtom: TAtomPosition;
begin
  Result:=true;
  if UpAtomIs('END') then
    BlockType:=ebtBegin
  else if UpAtomIs('UNTIL') then
    BlockType:=ebtRepeat
  else if UpAtomIs('FINALLY') or UpAtomIs('EXCEPT') then
    BlockType:=ebtTry
  else
    RaiseException('internal codetool error in '
      +'TPascalParserTool.ReadBackTilBlockEnd: unkown block type');
  repeat
    ReadPriorAtom;
    if (CurPos.StartPos<1) then begin
      RaiseException('"begin" not found.')
    end else if WordIsBlockKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then
    begin
      if UpAtomIs('END') or (UpAtomIs('UNTIL')) then begin
        ReadBackTilBlockEnd(false);
      end else if UpAtomIs('BEGIN') or UpAtomIs('ASM') or UpAtomIs('RECORD')
      then begin
        if BlockType=ebtBegin then
          break
        else
          RaiseBlockError;
      end else if UpAtomIs('OBJECT') then begin
        if BlockType=ebtBegin then begin
          // could also be 'of object'
          OldAtom:=CurPos;
          ReadPriorAtom;
          if not UpAtomIs('OF') then begin
            CurPos:=OldAtom;
            break;
          end;
        end else
          RaiseBlockError;
      end else if UpAtomIs('CLASS') then begin
        ReadNextAtom;
        if UpAtomIs('FUNCTION') or UpAtomIs('PROCEDURE') or AtomIsChar(';')
        or UpAtomIs('OF') then
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
              end else if UpAtomIs('RECORD') then begin
                // record start found -> the case is a variant record
                // block start found
                break;
              end else begin
                // this is not a variant record
                MoveCursorToCleanPos(OldAtom.StartPos);
                CurPos.EndPos:=OldAtom.EndPos;
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
    
}
begin
  while AtomIsChar('@') do
    ReadNextAtom;
  while UpAtomIs('INHERITED') do
    ReadNextAtom;
  Result:=(AtomIsIdentifier(false) or AtomIsChar('(') or AtomIsChar('['));
  if not Result then exit;
  repeat
    if AtomIsIdentifier(false) then
      ReadNextAtom;
    if AtomIsChar('(') or AtomIsChar('[') then begin
      Result:=ReadTilBracketClose(ExceptionOnError);
      if not Result then exit;
    end;
    if AtomIsChar('.') then
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
    if not AtomIsChar(';') then UndoReadNextAtom;
  end else if UpAtomIs('WITH') then begin
    if not ReadWithStatement(ExceptionOnError,CreateNodes) then exit;
  end else begin
    // read till semicolon or 'end'
    while (not AtomIsChar(';')) do begin
      ReadNextAtom;
      if UpAtomIs('END') then begin
        UndoReadNextAtom;
        break;
      end;
    end;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadWithStatement(ExceptionOnError,
  CreateNodes: boolean): boolean;
begin
  ReadNextAtom;
  if CreateNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnWithVariable
  end;
  ReadTilVariableEnd(true);
  while AtomIsChar(',') do begin
    CurNode.EndPos:=LastAtoms.GetValueAt(0).EndPos;
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
      RaiseException('do expected, but '+GetAtom+' found')
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
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode; // ctnWithVariable
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
  if AtomIsChar('=') then begin
    // read constant
    repeat
      ReadNextAtom;
      if AtomIsChar('(') or AtomIsChar('[') then
        ReadTilBracketClose(true);
      if AtomIsWord and (not IsKeyWordInConstAllowed.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
      and (UpAtomIs('END') or AtomIsKeyWord) then
        RaiseException('; expected, but '+GetAtom+' found');
    until AtomIsChar(';');
  end;
  // read ;
  if not AtomIsChar(';') then
    RaiseException('; expected, but '+GetAtom+' found');
  ReadNextAtom;
  if UpAtomIs('CVAR') then begin
    // for example: 'var a: char; cvar;'
    if not ReadNextAtomIsChar(';') then
      RaiseException('; expected, but '+GetAtom+' found');
  end else if UpAtomIs('PUBLIC') or UpAtomIs('EXTERNAL') then begin
    if NodeHasParentOfType(CurNode,ctnClass) then
      // class visibility keyword 'public'
      UndoReadNextAtom
    else begin
      // for example 'var a: char; public;'
      ReadNextAtom;
      if UpAtomIs('NAME') then begin
        // for example 'var a: char; public name 'b' ;'
        ReadNextAtom;
        if not AtomIsStringConstant then
          RaiseException('string constant expected, but '
                         +GetAtom+' found');
        ReadConstant(true,false,[]);
        UndoReadNextAtom;
      end;
      if not ReadNextAtomIsChar(';') then
        RaiseException('; expected, but '+GetAtom+' found');
    end;
  end else
    UndoReadNextAtom;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncBeginEnd: boolean;
// Keyword: begin, asm

  procedure RaiseExceptionWithHint;
  var CaretXY: TCodeXYPosition;
    AMessage: string;
  begin
    AMessage:='; expected, but . found';
    if (CleanPosToCaret(CurNode.StartPos,CaretXY))
    and (CaretXY.Code<>nil) then begin
      if CaretXY.Code=TCodeBuffer(Scanner.MainCode) then
        RaiseException(AMessage+'.  Hint: proc start at '
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')')
      else
        RaiseException(AMessage+'.  Hint: proc start at '
          +TCodeBuffer(CaretXY.Code).Filename
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')');
    end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
      RaiseException(AMessage);
    end;
  end;

var
  ChildNodeCreated: boolean;
begin
  ChildNodeCreated:=UpAtomIs('BEGIN') or UpAtomIs('ASM');
  if ChildNodeCreated then begin
    CreateChildNode;
    if UpAtomIs('BEGIN') then
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
    if AtomIsChar('.') then
      RaiseExceptionWithHint;
    UndoReadNextAtom;
    EndChildNode;
  end else if (CurNode.Desc in [ctnProgram,ctnImplementation]) then begin
    ReadNextAtom;
    if not AtomIsChar('.') then
      RaiseException('missing . after end');
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
    RaiseException('unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnTypeSection;
  // read all type definitions  Name = Type;
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnTypeDefinition;
      if not ReadNextAtomIsChar('=') then
        RaiseException('= expected, but '+GetAtom+' found');
      // read type
      ReadNextAtom;
      TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
      // read ;
      if not AtomIsChar(';') then
        RaiseException('; expected, but '+GetAtom+' found');
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

    implementation

    procedure c;
    var d:e;
      f:g=h;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseException('unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnVarSection;
  // read all variable definitions  Name : Type; [cvar;] [public [name '']]
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnVarDefinition;
      CurNode.EndPos:=CurPos.EndPos;
      ReadNextAtom;
      while AtomIsChar(',') do begin
        EndChildNode; // close variable definition
        ReadNextAtom;
        AtomIsIdentifier(true);
        CreateChildNode;
        CurNode.Desc:=ctnVarDefinition;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
      end;
      if not AtomIsChar(':') then
        RaiseException(': expected, but '+GetAtom+' found');
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
    RaiseException('unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnConstSection;
  // read all constants  Name = <Const>; or Name : type = <Const>;
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      ReadNextAtom;
      if AtomIsChar(':') then begin
        // read type
        ReadNextAtom;
        TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
      end;
      if not AtomIsChar('=') then
        RaiseException('= expected, but '+GetAtom+' found');
      // read constant
      repeat
        ReadNextAtom;
        if AtomIsChar('(') or AtomIsChar('[') then
          ReadTilBracketClose(true);
        if AtomIsWord and (not IsKeyWordInConstAllowed.DoItUppercase(UpperSrc,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
        and (UpAtomIs('END') or AtomIsKeyWord) then
          RaiseException('; expected, but '+GetAtom+' found');
      until AtomIsChar(';');
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
    RaiseException('unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnResStrSection;
  // read all string constants Name = 'abc';
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      if not ReadNextAtomIsChar('=') then
        RaiseException('= expected, but '+GetAtom+' found');
      // read string constant
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseException(
          'string constant expected, but '+GetAtom+' found');
      // read ;
      if not ReadNextAtomIsChar(';') then
        RaiseException('; expected, but '+GetAtom+' found');
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

function TPascalParserTool.KeyWordFuncTypePacked: boolean;
begin
  ReadNextAtom;
  if not PackedTypesKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
    CurPos.EndPos-CurPos.StartPos) then
    RaiseException('''record'' expected, but '+GetAtom+' found');
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncClass: boolean;
// class, object, interface (type, not section), dispinterface
//   this is a quick parser, which will only create one node for each class
//   the nodes for the methods and properties are created in a second
//   parsing phase (in KeyWordFuncClassMethod)
var
  ChildCreated: boolean;
  ClassAtomPos: TAtomPosition;
  Level: integer;
begin
  if CurNode.Desc<>ctnTypeDefinition then
    RaiseException('anonym classes are forbidden');
  if (LastUpAtomIs(0,'PACKED')) then begin
    if not LastAtomIs(1,'=') then
      RaiseException('anonym classes are not allowed');
    ClassAtomPos:=LastAtoms.GetValueAt(1);
  end else begin
    if not LastAtomIs(0,'=') then
      RaiseException('anonym classes are not allowed');
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
    if not ReadNextAtomIsChar(';') then
      RaiseException('; expected, but '+GetAtom+' found');
    if ChildCreated then CurNode.Desc:=ctnClassOfType;
  end else if AtomIsChar('(') then begin
    // read inheritage brackets
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  CurNode.SubDesc:=ctnsNeedJITParsing; // will not create sub nodes now
  if AtomIsChar(';') then begin
    if ChildCreated and (CurNode.Desc=ctnClass) then begin
      // forward class definition found
      CurNode.SubDesc:=CurNode.SubDesc+ctnsForwardDeclaration;
    end;
  end else begin
    Level:=1;
    while (CurPos.StartPos<=SrcLen) do begin
      if UpAtomIs('END') then begin
        dec(Level);
        if Level=0 then break;
      end else if UpAtomIs('RECORD') then inc(Level);
      ReadNextAtom;
    end;
    if (CurPos.StartPos>SrcLen) then
      RaiseException('"end" for class/object not found');
  end;
  if ChildCreated then begin
    // close class
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if UpAtomIs('END') then ReadNextAtom;
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
  if ReadNextAtomIsChar('[') then begin
    repeat
      ReadNextAtom;
      CreateChildNode;
      CurNode.Desc:=ctnRangeType;
      ReadSubRange(true);
      CurNode.EndPos:=LastAtoms.GetValueAt(0).EndPos;
      EndChildNode;
      if AtomIsChar(']') then break;
      if not AtomIsChar(',') then
        RaiseException('] expected, but '+GetAtom+' found');
    until false;
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseException('''of'' expected, but '+GetAtom+' found');
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
var IsFunction: boolean;
begin
  IsFunction:=UpAtomIs('FUNCTION');
  CreateChildNode;
  CurNode.Desc:=ctnProcedureType;
  ReadNextAtom;
  if AtomIsChar('(') then begin
    // read parameter list
    ReadParamList(true,false,[]);
  end;
  if IsFunction then begin
    if not AtomIsChar(':') then
      RaiseException(': expected, but '+GetAtom+' found');
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  if UpAtomIs('OF') then begin
    if not ReadNextUpAtomIs('OBJECT') then
      RaiseException(
        '''object'' expected, but '+GetAtom+' found');
    ReadNextAtom;
  end;
  if AtomIsChar('=') and NodeHasParentOfType(CurNode,ctnConstDefinition) then
  begin
  end else begin
    if not AtomIsChar(';') then
      RaiseException('; expected, but '+GetAtom+' found');
    // read modifiers
    repeat
      ReadNextAtom;
      if not IsKeyWordProcedureTypeSpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos) then begin
        UndoReadNextAtom;
        break;
      end else begin
        if not ReadNextAtomIsChar(';') then
          RaiseException('; expected, but '+GetAtom+' found');
      end;
    until false;
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
    RaiseException('''of'' expected, but '+GetAtom+' found');
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
    RaiseException('identfier expected, but ''type'' found');
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
  if not (LastAtomIs(0,'=') or LastAtomIs(0,':')) then
    RaiseException('identifier expected, but ^ found');
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
    (a,b:=3,c)
    (a)..4
    Low(integer)..High(integer)
    'a'..'z'
}
var SubRangeOperatorFound: boolean;

  procedure ReadTillTypeEnd;
  begin
    // read till ';', ':', ')', '=', 'end'
    while (CurPos.StartPos<=SrcLen)
    and (not (Src[CurPos.StartPos] in [';',':',')',']','=']))
    and (not AtomIsKeyWord) do begin
      if AtomIsChar('(') or AtomIsChar('[') then
        ReadTilBracketClose(true)
      else if AtomIs('..') then begin
        if SubRangeOperatorFound then
          RaiseException(
            'unexpected subrange operator ''..'' found');
        SubRangeOperatorFound:=true;
      end;
      ReadNextAtom;
    end;
  end;

// TPascalParserTool.KeyWordFuncTypeDefault: boolean
begin
  CreateChildNode;
  SubRangeOperatorFound:=false;
  if AtomIsWord then begin
    AtomIsIdentifier(true);
    ReadNextAtom;
    if AtomIsChar('.') then begin
      // first word was unit name
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
    end;
    while AtomIsChar('(') or AtomIsChar('[') do begin
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
        RaiseException('invalid subrange');
      CurNode.EndPos:=CurPos.StartPos;
    end;
  end else begin
    // enum or subrange
    ReadTillTypeEnd;
    if SubRangeOperatorFound then begin
      // a subrange
      CurNode.Desc:=ctnRangeType;
      CurNode.EndPos:=CurPos.StartPos;
    end else begin
      MoveCursorToNodeStart(CurNode);
      ReadNextAtom;
      if AtomIsChar('(') then begin
        // an enumeration -> read all enums
        CurNode.Desc:=ctnEnumerationType;
        repeat
          ReadNextAtom; // read enum name
          if AtomIsChar(')') then break;
          AtomIsIdentifier(true);
          CreateChildNode;
          CurNode.Desc:=ctnEnumIdentifier;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode; // close enum node
          ReadNextAtom;
          if AtomIs(':=') then begin
            // read ordinal value
            ReadNextAtom;
            ReadConstant(true,false,[]);
          end;
          if AtomIsChar(')') then break;
          if not AtomIsChar(',') then
            RaiseException(') expected, but '+GetAtom+' found');
        until false;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
      end else
        RaiseException('invalid type');
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
    if UpAtomIs('END') then break;
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
        if AtomIsChar(':') then break;
        if not AtomIsChar(',') then
          RaiseException(': expected, but '+GetAtom+' found');
        EndChildNode; // close variable
        ReadNextAtom; // read next variable name
      until false;
      ReadNextAtom;
      Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
      if not Result then exit;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode; // close variable
      if UpAtomIs('END') then break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode; // close record
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeRecordCase: boolean;
begin
  if not UpAtomIs('CASE') then
    RaiseException('[TPascalParserTool.KeyWordFuncTypeRecordCase] '
      +'internal error');
  CreateChildNode;
  CurNode.Desc:=ctnRecordCase;
  ReadNextAtom; // read ordinal type
  AtomIsIdentifier(true);
  ReadNextAtom;
  if AtomIsChar(':') then begin
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then // read 'of'
    RaiseException('''of'' expected, but '+GetAtom+' found');
  // read all variants
  repeat
    ReadNextAtom;  // read constant (variant identifier)
    if UpAtomIs('END') then break;
    CreateChildNode;
    CurNode.Desc:=ctnRecordVariant;
    repeat
      ReadNextAtom;  // read till ':'
      if AtomIsChar(':') then break
      else if AtomIsChar('(') or AtomIsChar('[') then
        ReadTilBracketClose(true)
      else if UpAtomIs('END') or AtomIsChar(')') or AtomIsKeyWord then
        RaiseException(': expected, but '+GetAtom+' found');
    until false;
    ReadNextAtom;  // read '('
    if not AtomIsChar('(') then
      RaiseException('( expected, but '+GetAtom+' found');
    // read all variables
    ReadNextAtom; // read first variable name
    repeat
      if AtomIsChar(')') then begin
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
          if AtomIsChar(':') then break;
          if not AtomIsChar(',') then
            RaiseException(
              ''','' expected, but '+GetAtom+' found');
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
      if AtomIsChar(')') then break;
      if not AtomIsChar(';') then
        RaiseException('; expected, but '+GetAtom+' found');
      ReadNextAtom;
    until false;
    if not AtomIsChar(')') then
      RaiseException(') expected, but '+GetAtom+' found');
    ReadNextAtom;
    if UpAtomIs('END') or AtomIsChar(')') then begin
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode; // close variant
      break;
    end;
    if not AtomIsChar(';') then
      RaiseException('; expected, but '+GetAtom+' found');
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
    Result:=copy(UpperSrc,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
  else
    Result:=copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.ExtractProcName(ProcNode: TCodeTreeNode;
  InUpperCase: boolean): string;
var ProcHeadNode: TCodeTreeNode;
begin
  Result:='';
  while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
    ProcNode:=ProcNode.Parent;
  if ProcNode=nil then exit;
  ProcHeadNode:=ProcNode.FirstChild;
  while (ProcHeadNode<>nil) and (ProcHeadNode.Desc<>ctnProcedureHead) do
    ProcHeadNode:=ProcHeadNode.NextBrother;
  if (ProcHeadNode=nil) or (ProcHeadNode.StartPos<1) then exit;
  MoveCursorToNodeStart(ProcHeadNode);
  repeat
    ReadNextAtom;
    if (CurPos.StartPos<=SrcLen)
    and (UpperSrc[CurPos.StartPos] in ['.','_','A'..'Z']) then begin
      if InUpperCase then
        Result:=Result+GetUpAtom
      else
        Result:=Result+GetAtom;
    end else
      break;
  until false;
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
      if phpInUpperCase in Attr then
        ExtractMemStream.Write(UpperSrc[LastAtomEndPos],
             CurPos.StartPos-LastAtomEndPos)
      else
        ExtractMemStream.Write(Src[LastAtomEndPos],
             CurPos.StartPos-LastAtomEndPos)
    end else if (CurPos.StartPos>LastAtomEndPos) 
    and (ExtractMemStream.Position>0) then begin
      ExtractMemStream.Write(' ',1);
      LastStreamPos:=ExtractMemStream.Position;
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
  HasClassName: boolean;
// function TPascalParserTool.ExtractProcHead(ProcNode: TCodeTreeNode;
//   Attr: TProcHeadAttributes): string;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (ProcNode=nil) or (ProcNode.StartPos<1) then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode=nil then exit;
  if ProcNode.Desc<>ctnProcedure then exit;
  if phpAddClassname in Attr then begin
    GrandPaNode:=ProcNode.Parent;
    if GrandPaNode=nil then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if (GrandPaNode=nil) or (GrandPaNode.Desc<>ctnClass) then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if GrandPaNode.Desc<>ctnTypeDefinition then exit;
    CurPos.StartPos:=GrandPaNode.StartPos;
    CurPos.EndPos:=CurPos.StartPos;
    ReadNextAtom;
    if not AtomIsWord then exit;
    TheClassName:=GetAtom;
  end;
  InitExtraction;
  // reparse the clean source
  MoveCursorToNodeStart(ProcNode);
  // parse procedure head = start + name + parameterlist + result type ;
  ExtractNextAtom(false,Attr);
  // read procedure start keyword
  if (UpAtomIs('CLASS') or UpAtomIs('STATIC')) then
    ExtractNextAtom(phpWithStart in Attr,Attr);
  if (UpAtomIs('PROCEDURE')) or (UpAtomIs('FUNCTION'))
  or (UpAtomIs('CONSTRUCTOR')) or (UpAtomIs('DESTRUCTOR'))
  or (UpAtomIs('OPERATOR')) then
    ExtractNextAtom(phpWithStart in Attr,Attr)
  else
    exit;
  ExtractProcHeadPos:=phepStart;
  // read name
  if (not AtomIsWord) or AtomIsKeyWord then exit;
  ReadNextAtom;
  HasClassName:=AtomIsChar('.');
  UndoReadNextAtom;
  if HasClassName then begin
    // read class name
    ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
    // read '.'
    ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
    // read name
    if (not AtomIsWord) or AtomIsKeyWord then exit;
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
      if phpInUpperCase in Attr then s:=UpperCaseStr(s);
      ExtractNextAtom(false,Attr);
      ExtractMemStream.Write(s[1],length(s));
    end;
  end;
  ExtractProcHeadPos:=phepName;
  // read parameter list
  if AtomIsChar('(') then
    ReadParamList(false,true,Attr);
  ExtractProcHeadPos:=phepParamList;
  // read result type
  while not AtomIsChar(';') do
    ExtractNextAtom(phpWithResultType in Attr,Attr);
  if AtomIsChar(';') then
    ExtractNextAtom(true,Attr);
  
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
            and (FindProcBody(Result)<>nil))) then begin
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
    and (CompareNodeUpSrc(Result,UpperVarName)=0) then
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
  if not AtomIsWord then exit;
  TheClassName:=GetAtom;
  ReadNextAtom;
  if not AtomIsChar('.') then exit;
  ReadNextAtom;
  if not AtomIsWord then exit;
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

function TPascalParserTool.CleanPosIsInComment(CleanPos,
  CleanCodePosInFront: integer; var CommentStart, CommentEnd: integer): boolean;
var CommentLvl, CurCommentPos: integer;
begin
  Result:=false;
  if CleanPos>SrcLen then exit;
  if CleanCodePosInFront>CleanPos then
    RaiseException(
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
  OnlyInterfaceNeeded: boolean; CursorPos: TCodeXYPosition;
  var CleanCursorPos: integer);
var Dummy: integer;
begin
  BuildTree(OnlyInterfaceNeeded);
  if not EndOfSourceFound then
    RaiseException('End of Source not found');
  // find the CursorPos in cleaned source
  Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (Dummy<>0) and (Dummy<>-1) then
    RaiseException('cursor pos outside of code');
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

procedure TPascalParserTool.ReadTilTypeOfProperty(PropertyNode: TCodeTreeNode);
begin
  MoveCursorToNodeStart(PropertyNode);
  ReadNextAtom; // read keyword 'property'
  ReadNextAtom; // read property name
  AtomIsIdentifier(true);
  ReadNextAtom;
  if AtomIsChar('[') then begin
    // read parameter list
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if not AtomIsChar(':') then
    RaiseException(': expected, but '+GetAtom+' found');
  ReadNextAtom; // read type
  AtomIsIdentifier(true);
end;

function TPascalParserTool.PropertyIsDefault(PropertyNode: TCodeTreeNode
  ): boolean;
begin
  Result:=false;
  if (PropertyNode=nil) or (PropertyNode.Desc<>ctnProperty) then exit;
  MoveCursorToCleanPos(PropertyNode.EndPos);
  ReadPriorAtom;
  if (not AtomIsChar(';')) then exit;
  ReadPriorAtom;
  Result:=UpAtomIs('DEFAULT');
end;

procedure TPascalParserTool.BuildSubTreeForProcHead(ProcNode: TCodeTreeNode);
var HasForwardModifier, IsFunction, IsOperator: boolean;
begin
  if ProcNode.Desc=ctnProcedureHead then ProcNode:=ProcNode.Parent;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure)
  or (ProcNode.FirstChild=nil) then
    RaiseException('[TPascalParserTool.BuildSubTreeForProcHead] '
      +'internal error: invalid ProcNode');
  if (ProcNode.FirstChild.SubDesc and ctnsNeedJITParsing)=0 then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if UpAtomIs('CLASS') then
    ReadNextAtom;
  IsFunction:=UpAtomIs('FUNCTION');
  IsOperator:=UpAtomIs('OPERATOR');
  // read procedure head (= name + parameterlist + resulttype;)
  CurNode:=ProcNode.FirstChild;
  ReadNextAtom;// read first atom of head
  if not IsOperator then AtomIsIdentifier(true);
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // read procedure name of a class method (the name after the . )
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  // read rest of procedure head and build nodes
  HasForwardModifier:=false;
  ReadTilProcedureHeadEnd(false,IsFunction,false,IsOperator,true,
                          HasForwardModifier);
  ProcNode.FirstChild.SubDesc:=ctnsNone;
end;



end.



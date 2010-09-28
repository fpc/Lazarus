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
    TLFMTree - a tree structure for LFM files.
}
unit LFMTrees;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, FileProcs, BasicCodeTools, CodeCache, TypInfo;
  
type
  { TLFMTreeNode }

  TLFMNodeType = (
    lfmnObject,
    lfmnProperty,
    lfmnValue,
    lfmnEnum
    );

  TLFMTree = class;

  TLFMTreeNode = class
  public
    TheType: TLFMNodeType;
    StartPos: integer;
    EndPos: integer;
    Parent: TLFMTreeNode;
    FirstChild: TLFMTreeNode;
    LastChild: TLFMTreeNode;
    PrevSibling: TLFMTreeNode;
    NextSibling: TLFMTreeNode;
    Tree: TLFMTree;
    constructor CreateVirtual; virtual;
    destructor Destroy; override;
    procedure Unbind;
    procedure AddChild(ANode: TLFMTreeNode);
    function GetIdentifier: string;
    procedure FindIdentifier(var IdentStart, IdentEnd: integer);
    function GetPath: string;
    function Next(SkipChildren: Boolean = False): TLFMTreeNode;
  end;
  
  TLFMTreeNodeClass = class of TLFMTreeNode;
  
  
  { TLFMObjectNode - a LFM object }
  
  TLFMObjectNode = class(TLFMTreeNode)
  public
    IsInherited: boolean;
    IsInline: boolean;
    ChildPos: Integer;
    Name: string;
    NamePosition: integer;
    TypeName: string;
    TypeNamePosition: integer;
    AncestorTool: TObject; // TFindDeclarationTool
    AncestorNode: TObject; // TCodeTreeNode
    AncestorContextValid: boolean;
    constructor CreateVirtual; override;
  end;

  { TLFMNameParts }

  TLFMNameParts = class
  private
    FCount: integer;
    FNames: ^String;
    FNamePositions: ^integer;
    function GetNamePositions(Index: integer): integer;
    function GetNames(Index: integer): string;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Name: string; NamePosition: integer);
    property Count: integer read FCount;
    property Names[Index: integer]: string read GetNames;
    property NamePositions[Index: integer]: integer read GetNamePositions;
  end;

  { TLFMPropertyNode - a LFM property }
  
  TLFMPropertyNode = class(TLFMTreeNode)
  public
    CompleteName: string;
    NameParts: TLFMNameParts;
    constructor CreateVirtual; override;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Name: string; NamePosition: integer);
  end;


  { TLFMValueNode - a LFM value }
  
  TLFMValueType = (
    lfmvNone,
    lfmvInteger,
    lfmvFloat,
    lfmvString,
    lfmvSymbol,
    lfmvSet,
    lfmvList,
    lfmvCollection,
    lfmvBinary
    );

  TLFMValueNode = class(TLFMTreeNode)
  public
    ValueType: TLFMValueType;
    constructor CreateVirtual; override;
    function ReadString: string;
  end;


  { TLFMValueNodeSymbol - a LFM value of type symbol }
  
  TLFMSymbolType = (
    lfmsNone,
    lfmsTrue,
    lfmsFalse,
    lfmsNil,
    lfmsIdentifier
    );

  TLFMValueNodeSymbol = class(TLFMValueNode)
  public
    SymbolType: TLFMSymbolType;
    constructor CreateVirtual; override;
  end;


  { TLFMValueNodeSet - a LFM value of type set }

  TLFMValueNodeSet = class(TLFMValueNode)
  public
    constructor CreateVirtual; override;
  end;


  { TLFMValueNodeList - a list of LFM values }

  TLFMValueNodeList = class(TLFMValueNode)
  public
    constructor CreateVirtual; override;
  end;


  { TLFMValueNodeCollection - a LFM collection }

  TLFMValueNodeCollection = class(TLFMValueNode)
  public
    constructor CreateVirtual; override;
  end;


  { TLFMValueNodeBinary - LFM binary data }

  TLFMValueNodeBinary = class(TLFMValueNode)
  public
    constructor CreateVirtual; override;
  end;


  { TLFMEnumNode - an enum of a value of type set}

  TLFMEnumNode = class(TLFMTreeNode)
  public
    constructor CreateVirtual; override;
  end;


  { TLFMError }
  
  TLFMErrorType = (
    lfmeNoError,
    lfmeParseError,
    lfmeMissingRoot,
    lfmeIdentifierNotFound,
    lfmeObjectNameMissing,
    lfmeObjectIncompatible,
    lfmePropertyNameMissing,
    lfmePropertyHasNoSubProperties,
    lfmeIdentifierNotPublished,
    lfmeEndNotFound
    );
  TLFMErrorTypes = set of TLFMErrorType;

  TLFMError = class
  public
    Tree: TLFMTree;
    Node: TLFMTreeNode;
    NextError: TLFMError;
    PrevError: TLFMError;
    ErrorType: TLFMErrorType;
    ErrorMessage: string;
    Source: TCodeBuffer;
    Position: integer;
    Caret: TPoint;
    constructor Create;
    procedure Clear;
    destructor Destroy; override;
    function AsString: string;
    procedure AddToTree(ATree: TLFMTree);
    procedure Unbind;
    function FindParentError: TLFMError;
    function FindContextNode: TLFMTreeNode;
    function IsMissingObjectType: boolean;
    function GetNodePath: string;
  end;
  
  TLFMTrees = class;
  
  { TLFMTree }

  TLFMTree = class
  protected
    Parser: TParser;
    TokenStart: LongInt;
    function NextToken: Char;
    procedure ProcessValue;
    procedure ProcessProperty;
    procedure ProcessObject;
    procedure CreateChildNode(NodeClass: TLFMTreeNodeClass);
    procedure CloseChildNode;
  public
    Root: TLFMTreeNode;
    CurNode: TLFMTreeNode;
    LFMBuffer: TCodeBuffer;
    LFMBufferChangeStep: integer;
    FirstError: TLFMError;
    LastError: TLFMError;
    Trees: TLFMTrees;
    constructor Create(TheTrees: TLFMTrees; aLFMBuf: TCodeBuffer);
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearErrors;
    function Parse(LFMBuf: TCodeBuffer = nil): boolean;
    function ParseIfNeeded: boolean;
    function UpdateNeeded: boolean;
    function PositionToCaret(p: integer): TPoint;
    procedure AddError(ErrorType: TLFMErrorType; LFMNode: TLFMTreeNode;
                       const ErrorMessage: string; ErrorPosition: integer);
    function FindErrorAtLine(Line: integer): TLFMError;
    function FindErrorAtNode(Node: TLFMTreeNode): TLFMError;
    function FindError(ErrorTypes: TLFMErrorTypes): TLFMError;
    function FirstErrorAsString: string;

    function FindProperty(PropertyPath: string;
                          ContextNode: TLFMTreeNode): TLFMPropertyNode;

    procedure WriteDebugReport;
  end;
  
  { TLFMTrees }

  TLFMTrees = class
  private
    FItems: TAVLTree;// tree of TLFMTree sorted for LFMBuffer
    FClearing: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetLFMTree(LFMBuffer: TCodeBuffer;
                        CreateIfNotExists: boolean): TLFMTree;
  end;
  
  TInstancePropInfo = record
    Instance: TPersistent;
    PropInfo: PPropInfo;
  end;
  PInstancePropInfo = ^TInstancePropInfo;

const
  LFMErrorTypeNames: array[TLFMErrorType] of string = (
    'NoError',
    'ParseError',
    'MissingRoot',
    'IdentifierNotFound',
    'ObjectNameMissing',
    'ObjectIncompatible',
    'PropertyNameMissing',
    'PropertyHasNoSubProperties',
    'IdentifierNotPublished',
    'EndNotFound'
    );
    
  TLFMValueTypeNames: array[TLFMValueType] of string = (
    'None',
    'Integer',
    'Float',
    'String',
    'Symbol',
    'Set',
    'List',
    'Collection',
    'Binary'
    );
    
procedure FreeListOfPInstancePropInfo(List: TFPList);
function CompareLFMTreesByLFMBuffer(Data1, Data2: Pointer): integer;
function CompareLFMBufWithTree(Buf, Tree: Pointer): integer;

var
  DefaultLFMTrees: TLFMTrees = nil;

implementation


procedure FreeListOfPInstancePropInfo(List: TFPList);
var
  i: Integer;
  p: PInstancePropInfo;
begin
  if List=nil then exit;
  for i:=0 to List.Count-1 do begin
    p:=PInstancePropInfo(List[i]);
    Dispose(p);
  end;
  List.Free;
end;

function CompareLFMTreesByLFMBuffer(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointers(TLFMTree(Data1).LFMBuffer,TLFMTree(Data2).LFMBuffer);
end;

function CompareLFMBufWithTree(Buf, Tree: Pointer): integer;
begin
  Result:=ComparePointers(Buf,TLFMTree(Tree).LFMBuffer);
end;


{ TLFMTree }

constructor TLFMTree.Create;
begin
end;

destructor TLFMTree.Destroy;
begin
  Clear;
  if (Trees<>nil) and (not Trees.FClearing) then Trees.FItems.Remove(Self);
  inherited Destroy;
end;

procedure TLFMTree.Clear;
begin
  // do not set LFMBuffer to nil
  TokenStart:=0;
  CurNode:=nil;
  ClearErrors;
  while Root<>nil do Root.Free;
end;

procedure TLFMTree.ClearErrors;
begin
  while FirstError<>nil do FirstError.Free;
end;

function TLFMTree.Parse(LFMBuf: TCodeBuffer = nil): boolean;
var
  LFMStream: TMemoryStream;
  Src: String;
begin
  Result:=false;
  Clear;
  if LFMBuf<>LFMBuffer then begin
    DebugLn(['TLFMTree.Parse New=',LFMBuf.Filename]);
    DebugLn(['TLFMTree.Parse Old=',LFMBuffer.Filename]);
    if Trees<>nil then
      raise Exception.Create('TLFMTree.Parse: changing LFMBuffer in Tree is not allowed');
    LFMBuffer:=LFMBuf;
  end;
  LFMBufferChangeStep:=LFMBuffer.ChangeStep;
  
  LFMStream:=TMemoryStream.Create;
  Src:=LFMBuffer.Source;
  if Src<>'' then begin
    LFMStream.Write(Src[1],length(Src));
    LFMStream.Position:=0;
  end;
  Parser := TParser.Create(LFMStream);
  try
    try
      repeat
        ProcessObject;
      until (not Parser.TokenSymbolIs('OBJECT'))
        and (not Parser.TokenSymbolIs('INHERITED'))
        and (not Parser.TokenSymbolIs('INLINE'));
      Result:=true;
    except
      on E: EParserError do begin
        AddError(lfmeParseError,CurNode,E.Message,Parser.SourcePos);
      end;
    end;
  finally
    Parser.Free;
    Parser:=nil;
    LFMStream.Free;
  end;
end;

function TLFMTree.ParseIfNeeded: boolean;
begin
  if not UpdateNeeded then exit(true);
  Result:=Parse(LFMBuffer);
end;

function TLFMTree.UpdateNeeded: boolean;
begin
  Result:=(LFMBuffer=nil) or (LFMBuffer.ChangeStep<>LFMBufferChangeStep)
       or (FirstError<>nil);
end;

function TLFMTree.PositionToCaret(p: integer): TPoint;
begin
  Result:=Point(0,0);
  LFMBuffer.AbsoluteToLineCol(p,Result.Y,Result.X);
end;

procedure TLFMTree.AddError(ErrorType: TLFMErrorType;
  LFMNode: TLFMTreeNode; const ErrorMessage: string; ErrorPosition: integer);
var
  NewError: TLFMError;
begin
  NewError:=TLFMError.Create;
  NewError.Node:=LFMNode;
  NewError.ErrorType:=ErrorType;
  NewError.ErrorMessage:=ErrorMessage;
  NewError.Source:=LFMBuffer;
  NewError.Position:=ErrorPosition;
  NewError.Caret:=PositionToCaret(NewError.Position);
  DebugLn('TLFMTree.AddError ',NewError.AsString,
          ' NodePath=',NewError.GetNodePath);
  NewError.AddToTree(Self);
end;

function TLFMTree.FindErrorAtLine(Line: integer): TLFMError;
begin
  Result:=FirstError;
  while Result<>nil do begin
    if (Result.Caret.Y=Line) and (Line>=1) then exit;
    Result:=Result.NextError;
  end;
end;

function TLFMTree.FindErrorAtNode(Node: TLFMTreeNode): TLFMError;
begin
  Result:=FirstError;
  while Result<>nil do begin
    if (Result.Node=Node) and (Node<>nil) then exit;
    Result:=Result.NextError;
  end;
end;

function TLFMTree.FindError(ErrorTypes: TLFMErrorTypes): TLFMError;
begin
  Result:=FirstError;
  while (Result<>nil) and (not (Result.ErrorType in ErrorTypes)) do
    Result:=Result.NextError;
end;

function TLFMTree.FirstErrorAsString: string;
begin
  Result:='';
  if FirstError<>nil then Result:=FirstError.ErrorMessage;
end;

function TLFMTree.FindProperty(PropertyPath: string; ContextNode: TLFMTreeNode
  ): TLFMPropertyNode;
var
  Node: TLFMTreeNode;
  ObjNode: TLFMObjectNode;
  p: LongInt;
  FirstPart: String;
  RestParts: String;
begin
  if ContextNode=nil then
    Node:=Root
  else
    Node:=ContextNode.FirstChild;
  p:=System.Pos('.',PropertyPath);
  FirstPart:=copy(PropertyPath,1,p-1);
  RestParts:=copy(PropertyPath,p+1,length(PropertyPath));
  while Node<>nil do begin
    if Node is TLFMPropertyNode then begin
      Result:=TLFMPropertyNode(Node);
      if SysUtils.CompareText(Result.CompleteName,PropertyPath)=0 then
        exit;
    end else if (Node is TLFMObjectNode)
    and (RestParts<>'') then begin
      ObjNode:=TLFMObjectNode(Node);
      if CompareIdentifierPtrs(Pointer(ObjNode.Name),Pointer(FirstPart))=0 then
      begin
        Result:=FindProperty(RestParts,ObjNode);
        exit;
      end;
    end;
    Node:=Node.NextSibling;
  end;
  Result:=nil;
end;

procedure TLFMTree.WriteDebugReport;
var
  Src: string;

  procedure WriteNode(const Prefix: string; Node: TLFMTreeNode);
  var
    Child: TLFMTreeNode;
    EndPos: LongInt;
  begin
    if Node=nil then exit;
    Child:=Node.FirstChild;
    EndPos:=Node.EndPos;
    if (Child<>nil) and (EndPos>Child.StartPos) then
      EndPos:=Child.StartPos;
    DebugLn([Prefix,dbgstr(copy(Src,Node.StartPos,EndPos-Node.StartPos))]);
    while Child<>nil do begin
      WriteNode(Prefix+'  ',Child);
      Child:=Child.NextSibling;
    end;
  end;

begin
  if LFMBuffer=nil then begin
    DebugLn(['TLFMTree.WriteDebugReport LFMBuffer=nil']);
  end;
  DebugLn(['TLFMTree.WriteDebugReport ',LFMBuffer.Filename]);
  Src:=LFMBuffer.Source;
  WriteNode('',Root);
end;

function TLFMTree.NextToken: Char;
begin
  TokenStart:=Parser.SourcePos+1;
  while (TokenStart<=LFMBuffer.SourceLength)
  and (LFMBuffer.Source[TokenStart] in [' ',#9,#10,#13]) do
    inc(TokenStart);
  Result:=Parser.NextToken;
end;

procedure TLFMTree.ProcessValue;
var
  s: String;
  MemStream: TMemoryStream;
  SymbolNode: TLFMValueNodeSymbol;
begin
  case Parser.Token of
  
  toInteger:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvInteger;
      NextToken;
      CloseChildNode;
    end;
    
  toFloat:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvFloat;
      NextToken;
      CloseChildNode;
    end;
    
  Classes.toString, toWString:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvString;
      while NextToken = '+' do begin
        NextToken;   // Get next string fragment
        if not (Parser.Token in [Classes.toString,toWString]) then
          Parser.CheckToken(Classes.toString);
      end;
      CloseChildNode;
    end;
    
  toSymbol:
    begin
      CreateChildNode(TLFMValueNodeSymbol);
      SymbolNode:=TLFMValueNodeSymbol(CurNode);
      if SymbolNode=nil then ;
      s := Parser.TokenString;
      if SysUtils.CompareText(s, 'End') = 0 then
        SymbolNode.SymbolType:=lfmsNone
      else if SysUtils.CompareText(s, 'True') = 0 then
        SymbolNode.SymbolType:=lfmsTrue
      else if SysUtils.CompareText(s, 'False') = 0 then
        SymbolNode.SymbolType:=lfmsFalse
      else if SysUtils.CompareText(s, 'nil') = 0 then
        SymbolNode.SymbolType:=lfmsNil
      else
      begin
        SymbolNode.SymbolType:=lfmsIdentifier;
        Parser.TokenComponentIdent;
      end;
      if SymbolNode.SymbolType<>lfmsNone then
        NextToken;
      CloseChildNode;
    end;
    
  // Set
  '[':
    begin
      CreateChildNode(TLFMValueNodeSet);
      NextToken;
      if Parser.Token <> ']' then
        while True do
        begin
          CreateChildNode(TLFMEnumNode);
          Parser.CheckToken(toSymbol);
          CloseChildNode;
          NextToken;
          if Parser.Token = ']' then
            break;
          Parser.CheckToken(',');
          NextToken;
        end;
      NextToken;
      CloseChildNode;
    end;
    
  // List
  '(':
    begin
      CreateChildNode(TLFMValueNodeList);
      NextToken;
      while Parser.Token <> ')' do
        ProcessValue;
      NextToken;
      CloseChildNode;
    end;
    
  // Collection
  '<':
    begin
      CreateChildNode(TLFMValueNodeCollection);
      NextToken;
      while Parser.Token <> '>' do
      begin
        Parser.CheckTokenSymbol('item');
        NextToken;
        CreateChildNode(TLFMValueNodeList);
        while not Parser.TokenSymbolIs('end') do
          ProcessProperty;
        NextToken;   // Skip 'end'
        CloseChildNode;
      end;
      NextToken;
      CloseChildNode;
    end;
    
  // Binary data
  '{':
    begin
      CreateChildNode(TLFMValueNodeBinary);
      MemStream := TMemoryStream.Create;
      try
        Parser.HexToBinary(MemStream);
      finally
        MemStream.Free;
      end;
      NextToken;
      CloseChildNode;
    end;
    
  else
    Parser.Error('invalid property');
  end;
end;

procedure TLFMTree.ProcessProperty;
var
  PropertyNode: TLFMPropertyNode;
begin
  CreateChildNode(TLFMPropertyNode);
  PropertyNode:=TLFMPropertyNode(CurNode);
  if PropertyNode=nil then ;
  // Get name of property
  Parser.CheckToken(toSymbol);
  PropertyNode.Add(Parser.TokenString,TokenStart);
  while True do begin
    NextToken;
    if Parser.Token <> '.' then break;
    NextToken;
    Parser.CheckToken(toSymbol);
    PropertyNode.Add(Parser.TokenString,TokenStart);
  end;
  Parser.CheckToken('=');
  NextToken;
  ProcessValue;
  CloseChildNode;
end;

procedure TLFMTree.ProcessObject;
var
  ObjectNode: TLFMObjectNode;
  ObjectStartLine: LongInt;
begin
  CreateChildNode(TLFMObjectNode);
  ObjectNode:=TLFMObjectNode(CurNode);
  if Parser.TokenSymbolIs('OBJECT') then
    ObjectNode.IsInherited := False
  else if Parser.TokenSymbolIs('INHERITED') then
    ObjectNode.IsInherited := True
  else begin
    Parser.CheckTokenSymbol('INLINE');
    ObjectNode.IsInline := True;
  end;
  NextToken;
  Parser.CheckToken(toSymbol);
  if not Parser.TokenSymbolIs('END') then begin
    ObjectStartLine:=Parser.SourceLine;
    ObjectNode.Name := '';
    ObjectNode.TypeName := Parser.TokenString;
    ObjectNode.TypeNamePosition:=TokenStart;
    ObjectNode.ChildPos := -1;
    NextToken;
    if Parser.Token = ':' then begin
      NextToken;
      Parser.CheckToken(toSymbol);
      ObjectNode.Name := ObjectNode.TypeName;
      ObjectNode.NamePosition:=ObjectNode.TypeNamePosition;
      ObjectNode.TypeName := Parser.TokenString;
      ObjectNode.TypeNamePosition:=TokenStart;
      NextToken;
      if parser.Token = '[' then begin
        NextToken;
        ObjectNode.ChildPos := parser.TokenInt;
        NextToken;
        parser.CheckToken(']');
        NextToken;
      end;
    end;

    // read property list
    while not (Parser.TokenSymbolIs('END')
    or Parser.TokenSymbolIs('OBJECT')
    or Parser.TokenSymbolIs('INHERITED')
    or Parser.TokenSymbolIs('INLINE')) do
      ProcessProperty;

    // read child objects
    while not Parser.TokenSymbolIs('END') do begin
      if Parser.Token=toEOF then begin
        Parser.Error('END not found for'
          +' object='+ObjectNode.Name+':'+ObjectNode.TypeName
          +' starting at line '+IntToStr(ObjectStartLine));
      end;
      ProcessObject;
    end;
  end;
  NextToken; // Skip 'END' token
  
  CloseChildNode;
end;

procedure TLFMTree.CreateChildNode(NodeClass: TLFMTreeNodeClass);
var
  NewNode: TLFMTreeNode;
begin
  NewNode:=NodeClass.CreateVirtual;
  NewNode.Tree:=Self;
  NewNode.StartPos:=TokenStart;
  NewNode.EndPos:=0;
  if CurNode<>nil then begin
    CurNode.AddChild(NewNode);
  end else begin
    Root:=NewNode;
  end;
  CurNode:=NewNode;
end;

procedure TLFMTree.CloseChildNode;
begin
  if CurNode.EndPos<1 then
    CurNode.EndPos:=TokenStart;
  CurNode:=CurNode.Parent;
end;

constructor TLFMTree.Create(TheTrees: TLFMTrees; aLFMBuf: TCodeBuffer);
begin
  if (TheTrees=nil)
  or (aLFMBuf=nil) then
    raise Exception.Create('TLFMTree.Create need tree and buffer');
  Trees:=TheTrees;
  Trees.FItems.Add(Self);
  LFMBuffer:=aLFMBuf;
  LFMBufferChangeStep:=LFMBuffer.ChangeStep;
  if LFMBufferChangeStep=Low(LFMBufferChangeStep) then
    LFMBufferChangeStep:=High(LFMBufferChangeStep)
  else
    dec(LFMBufferChangeStep);
end;

{ TLFMTreeNode }

constructor TLFMTreeNode.CreateVirtual;
begin

end;

destructor TLFMTreeNode.Destroy;
begin
  while FirstChild<>nil do FirstChild.Free;
  Unbind;
  inherited Destroy;
end;

procedure TLFMTreeNode.Unbind;
begin
  if Parent<>nil then begin
    if Parent.FirstChild=Self then Parent.FirstChild:=NextSibling;
    if Parent.LastChild=Self then Parent.LastChild:=PrevSibling;
    Parent:=nil;
  end;
  if Tree<>nil then begin
    if Tree.Root=Self then Tree.Root:=NextSibling;
    Tree:=nil;
  end;
  if NextSibling<>nil then NextSibling.PrevSibling:=PrevSibling;
  if PrevSibling<>nil then PrevSibling.NextSibling:=NextSibling;
  NextSibling:=nil;
  PrevSibling:=nil;
end;

procedure TLFMTreeNode.AddChild(ANode: TLFMTreeNode);
begin
  if ANode=nil then exit;
  ANode.Unbind;
  ANode.Parent:=Self;
  ANode.Tree:=Tree;
  ANode.PrevSibling:=LastChild;
  LastChild:=ANode;
  if FirstChild=nil then FirstChild:=ANode;
  if ANode.PrevSibling<>nil then
    ANode.PrevSibling.NextSibling:=ANode;
end;

function TLFMTreeNode.GetIdentifier: string;
var
  IdentStart, IdentEnd: integer;
begin
  Result:='';
  if (Tree=nil) or (Tree.LFMBuffer=nil) or (StartPos<1) then exit;
  FindIdentifier(IdentStart,IdentEnd);
  if IdentStart<1 then exit;
  Result:=copy(Tree.LFMBuffer.Source,IdentStart,IdentEnd-IdentStart);
end;

procedure TLFMTreeNode.FindIdentifier(var IdentStart, IdentEnd: integer);
var
  Src: String;
  SrcLen: Integer;
begin
  IdentStart:=-1;
  IdentEnd:=-1;
  if (Tree=nil) or (Tree.LFMBuffer=nil) or (StartPos<1) then exit;
  Src:=Tree.LFMBuffer.Source;
  SrcLen:=length(Src);
  IdentStart:=StartPos;
  while (IdentStart<=SrcLen) and (Src[IdentStart] in [#0..#32]) do
    inc(IdentStart);
  IdentEnd:=IdentStart;
  while (IdentEnd<=SrcLen)
  and (Src[IdentEnd] in ['A'..'Z','a'..'z','0'..'9','_','.']) do
    inc(IdentEnd);

  if TheType=lfmnObject then begin
    // skip object/inherited/inline
    IdentStart:=IdentEnd;
    while (IdentStart<=SrcLen) and (Src[IdentStart] in [#0..#32]) do
      inc(IdentStart);
    IdentEnd:=IdentStart;
    while (IdentEnd<=SrcLen)
    and (Src[IdentEnd] in ['A'..'Z','a'..'z','0'..'9','_','.']) do
      inc(IdentEnd);
  end;
  //debugln('TLFMTreeNode.FindIdentifier ',copy(Src,IdentStart,IdentEnd-IdentStart),' ',DbgStr(copy(Src,StartPos,20)));
  
  if IdentEnd<=IdentStart then begin
    IdentStart:=-1;
    IdentEnd:=-1;
  end;
end;

function TLFMTreeNode.GetPath: string;
var
  ANode: TLFMTreeNode;
  PrependStr: String;
begin
  Result:='';
  ANode:=Self;
  while ANode<>nil do begin
    PrependStr:=ANode.GetIdentifier;
    {PrependStr:=PrependStr+'('+dbgs(ANode.StartPos)+','+dbgs(ANode.EndPos)+')';
    if (ANode.Tree<>nil) then begin
      if (ANode.Tree.LFMBuffer<>nil) then begin
        PrependStr:=PrependStr+'"'+DbgStr(copy(ANode.Tree.LFMBuffer.Source,ANode.StartPos,20))+'"';
      end else begin
        PrependStr:=PrependStr+'noLFMBuf';
      end;
    end else begin
      PrependStr:=PrependStr+'noTree';
    end;}
    if PrependStr<>'' then begin
      if Result<>'' then
        Result:='/'+Result;
       Result:=PrependStr+Result;
    end;
    ANode:=ANode.Parent;
  end;
end;

function TLFMTreeNode.Next(SkipChildren: Boolean = False): TLFMTreeNode;
begin
  if not SkipChildren and (FirstChild <> nil) then
    Result := FirstChild
  else
  begin
    Result := Self;
    while Result <> nil do
    begin
      if Result.NextSibling <> nil then
      begin
        Result := Result.NextSibling;
        Exit;
      end;
      Result := Result.Parent;
    end;
  end;
end;

{ TLFMObjectNode }

constructor TLFMObjectNode.CreateVirtual;
begin
  TheType:=lfmnObject;
end;

{ TLFMPropertyNode }

constructor TLFMPropertyNode.CreateVirtual;
begin
  TheType:=lfmnProperty;
end;

destructor TLFMPropertyNode.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLFMPropertyNode.Clear;
begin
  CompleteName:='';
  NameParts.Free;
  NameParts:=nil;
end;

procedure TLFMPropertyNode.Add(const Name: string; NamePosition: integer);
begin
  if NameParts=nil then NameParts:=TLFMNameParts.Create;
  NameParts.Add(Name,NamePosition);
  if CompleteName<>'' then
    CompleteName:=CompleteName+'.'+Name
  else
    CompleteName:=Name;
end;

{ TLFMValueNode }

constructor TLFMValueNode.CreateVirtual;
begin
  TheType:=lfmnValue;
  ValueType:=lfmvNone;
end;

function TLFMValueNode.ReadString: string;
var
  p: LongInt;
  Src: String;
  i: integer;
  AtomStart: LongInt;
begin
  Result:='';
  if ValueType<>lfmvString then exit;
  p:=StartPos;
  AtomStart:=p;
  Src:=Tree.LFMBuffer.Source;
  repeat
    ReadRawNextPascalAtom(Src,p,AtomStart);
    if AtomStart>length(Src) then exit;
    if Src[AtomStart]='''' then begin
      Result:=Result+copy(Src,AtomStart+1,p-AtomStart-2)
    end else if Src[AtomStart]='+' then begin
      // skip
    end else if Src[AtomStart]='#' then begin
      i:=StrToIntDef(copy(Src,AtomStart+1,p-AtomStart-1),-1);
      if (i<0) or (i>255) then exit;
      Result:=Result+chr(i);
    end else
      exit;
  until false;
end;

{ TLFMValueNodeSymbol }

constructor TLFMValueNodeSymbol.CreateVirtual;
begin
  inherited CreateVirtual;
  ValueType:=lfmvSymbol;
  SymbolType:=lfmsIdentifier;
end;

{ TLFMValueNodeSet }

constructor TLFMValueNodeSet.CreateVirtual;
begin
  inherited CreateVirtual;
  ValueType:=lfmvSet;
end;

{ TLFMEnumNode }

constructor TLFMEnumNode.CreateVirtual;
begin
  TheType:=lfmnEnum;
end;

{ TLFMValueNodeList }

constructor TLFMValueNodeList.CreateVirtual;
begin
  inherited CreateVirtual;
  ValueType:=lfmvList;
end;

{ TLFMValueNodeCollection }

constructor TLFMValueNodeCollection.CreateVirtual;
begin
  inherited CreateVirtual;
  ValueType:=lfmvCollection;
end;

{ TLFMValueNodeBinary }

constructor TLFMValueNodeBinary.CreateVirtual;
begin
  inherited CreateVirtual;
  ValueType:=lfmvBinary;
end;

{ TLFMError }

constructor TLFMError.Create;
begin
  Clear;
end;

procedure TLFMError.Clear;
begin
  ErrorType:=lfmeNoError;
  Source:=nil;
end;

destructor TLFMError.Destroy;
begin
  Unbind;
  inherited Destroy;
end;

function TLFMError.AsString: string;
begin
  Result:=LFMErrorTypeNames[ErrorType]+': '+ErrorMessage;
  if Source<>nil then begin
    Result:=Result+'. '+ExtractFileName(Source.Filename);
    Result:=Result+' ('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')';
  end;
end;

procedure TLFMError.AddToTree(ATree: TLFMTree);
begin
  if Tree=ATree then exit;
  Unbind;
  if ATree=nil then exit;
  Tree:=ATree;
  PrevError:=Tree.LastError;
  Tree.LastError:=Self;
  if PrevError<>nil then PrevError.NextError:=Self;
  if Tree.FirstError=nil then Tree.FirstError:=Self;
end;

procedure TLFMError.Unbind;
begin
  if Tree<>nil then begin
    if Tree.FirstError=Self then Tree.FirstError:=NextError;
    if Tree.LastError=Self then Tree.LastError:=PrevError;
    Tree:=nil;
  end;
  if NextError<>nil then NextError.PrevError:=PrevError;
  if PrevError<>nil then PrevError.NextError:=NextError;
  PrevError:=nil;
  NextError:=nil;
end;

function TLFMError.FindParentError: TLFMError;
var
  CurNode: TLFMTreeNode;
begin
  Result:=nil;
  if (Node=nil) or (Tree=nil) then exit;
  CurNode:=Node.Parent;
  while CurNode<>nil do begin
    Result:=Tree.FindErrorAtNode(CurNode);
    if Result<>nil then exit;
    CurNode:=CurNode.Parent;
  end;
end;

function TLFMError.FindContextNode: TLFMTreeNode;
begin
  Result:=Node;
  while (Result<>nil)
  and (not (Result.TheType in [lfmnProperty,lfmnObject])) do
    Result:=Result.Parent;
end;

function TLFMError.IsMissingObjectType: boolean;
begin
  Result:=(ErrorType in [lfmeIdentifierNotFound,lfmeMissingRoot])
      and (Node is TLFMObjectNode)
      and (TLFMObjectNode(Node).TypeName<>'')
      and (TLFMObjectNode(Node).TypeNamePosition=Position);
end;

function TLFMError.GetNodePath: string;
begin
  if Node<>nil then
    Result:=Node.GetPath
  else
    Result:='';
end;

{ TLFMNameParts }

function TLFMNameParts.GetNamePositions(Index: integer): integer;
begin
  Result:=FNamePositions[Index];
end;

function TLFMNameParts.GetNames(Index: integer): string;
begin
  Result:=FNames[Index];
end;

destructor TLFMNameParts.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLFMNameParts.Clear;
var
  i: Integer;
begin
  ReAllocMem(FNamePositions,0);
  for i:=0 to FCount-1 do FNames[i]:='';
  ReAllocMem(FNames,0);
end;

procedure TLFMNameParts.Add(const Name: string; NamePosition: integer);
var
  p: PPChar;
begin
  inc(FCount);
  ReAllocMem(FNamePositions,SizeOf(Integer)*FCount);
  FNamePositions[FCount-1]:=NamePosition;
  ReAllocMem(FNames,SizeOf(PChar)*FCount);
  p:=PPChar(FNames);
  p[FCount-1]:=nil;
  FNames[FCount-1]:=Name;
end;

{ TLFMTrees }

constructor TLFMTrees.Create;
begin
  FItems:=TAVLTree.Create(@CompareLFMTreesByLFMBuffer);
end;

destructor TLFMTrees.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TLFMTrees.Clear;
begin
  FClearing:=true;
  FItems.FreeAndClear;
  FClearing:=false;
end;

function TLFMTrees.GetLFMTree(LFMBuffer: TCodeBuffer; CreateIfNotExists: boolean
  ): TLFMTree;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FItems.FindKey(LFMBuffer,@CompareLFMBufWithTree);
  if AVLNode<>nil then
    Result:=TLFMTree(AVLNode.Data)
  else if CreateIfNotExists then
    Result:=TLFMTree.Create(Self,LFMBuffer)
  else
    Result:=nil;
end;

finalization
  FreeAndNil(DefaultLFMTrees);

end.


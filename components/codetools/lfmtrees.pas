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
  Classes, SysUtils, FileProcs, CodeCache, CodeAtom, TypInfo;
  
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
  end;
  
  TLFMTreeNodeClass = class of TLFMTreeNode;
  
  
  { TLFMObjectNode - a LFM object }
  
  TLFMObjectNode = class(TLFMTreeNode)
  public
    IsInherited: boolean;
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
  
  { TLFMTree }

  TLFMTree = class
  protected
    Parser: TParser;
    procedure ProcessValue;
    procedure ProcessProperty;
    procedure ProcessObject;
    procedure CreateChildNode(NodeClass: TLFMTreeNodeClass);
    procedure CloseChildNode;
  public
    Root: TLFMTreeNode;
    CurNode: TLFMTreeNode;
    LFMBuffer: TCodeBuffer;
    FirstError: TLFMError;
    LastError: TLFMError;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Parse(LFMBuf: TCodeBuffer): boolean;
    function PositionToCaret(p: integer): TPoint;
    procedure AddError(ErrorType: TLFMErrorType; LFMNode: TLFMTreeNode;
      const ErrorMessage: string; ErrorPosition: integer);
    function FindErrorAtLine(Line: integer): TLFMError;
    function FindErrorAtNode(Node: TLFMTreeNode): TLFMError;
    function FindError(ErrorTypes: TLFMErrorTypes): TLFMError;
    function FirstErrorAsString: string;
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
    
procedure FreeListOfPInstancePropInfo(List: TFPList);

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

{ TLFMTree }

constructor TLFMTree.Create;
begin
end;

destructor TLFMTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLFMTree.Clear;
begin
  LFMBuffer:=nil;
  CurNode:=nil;
  while FirstError<>nil do FirstError.Free;
  while Root<>nil do Root.Free;
end;

function TLFMTree.Parse(LFMBuf: TCodeBuffer): boolean;
var
  LFMStream: TMemoryStream;
  Src: String;
begin
  Result:=false;
  Clear;
  LFMBuffer:=LFMBuf;
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
        and (not Parser.TokenSymbolIs('INHERITED'));
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
      Parser.NextToken;
      CloseChildNode;
    end;
    
  toFloat:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvFloat;
      Parser.NextToken;
      CloseChildNode;
    end;
    
  toString:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvString;
      while Parser.NextToken = '+' do begin
        Parser.NextToken;   // Get next string fragment
        Parser.CheckToken(toString);
      end;
      CloseChildNode;
    end;
    
  toSymbol:
    begin
      CreateChildNode(TLFMValueNodeSymbol);
      SymbolNode:=TLFMValueNodeSymbol(CurNode);
      if SymbolNode=nil then ;
      s := Parser.TokenString;
      if CompareText(s, 'End') = 0 then
        SymbolNode.SymbolType:=lfmsNone
      else if CompareText(s, 'True') = 0 then
        SymbolNode.SymbolType:=lfmsTrue
      else if CompareText(s, 'False') = 0 then
        SymbolNode.SymbolType:=lfmsFalse
      else if CompareText(s, 'nil') = 0 then
        SymbolNode.SymbolType:=lfmsNil
      else
      begin
        SymbolNode.SymbolType:=lfmsIdentifier;
        Parser.TokenComponentIdent;
      end;
      if SymbolNode.SymbolType<>lfmsNone then
        Parser.NextToken;
      CloseChildNode;
    end;
    
  // Set
  '[':
    begin
      CreateChildNode(TLFMValueNodeSet);
      Parser.NextToken;
      if Parser.Token <> ']' then
        while True do
        begin
          CreateChildNode(TLFMEnumNode);
          Parser.CheckToken(toSymbol);
          CloseChildNode;
          Parser.NextToken;
          if Parser.Token = ']' then
            break;
          Parser.CheckToken(',');
          Parser.NextToken;
        end;
      Parser.NextToken;
      CloseChildNode;
    end;
    
  // List
  '(':
    begin
      CreateChildNode(TLFMValueNodeList);
      Parser.NextToken;
      while Parser.Token <> ')' do
        ProcessValue;
      Parser.NextToken;
      CloseChildNode;
    end;
    
  // Collection
  '<':
    begin
      CreateChildNode(TLFMValueNodeCollection);
      Parser.NextToken;
      while Parser.Token <> '>' do
      begin
        Parser.CheckTokenSymbol('item');
        Parser.NextToken;
        CreateChildNode(TLFMValueNodeList);
        while not Parser.TokenSymbolIs('end') do
          ProcessProperty;
        Parser.NextToken;   // Skip 'end'
        CloseChildNode;
      end;
      Parser.NextToken;
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
      Parser.NextToken;
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
  PropertyNode.Add(Parser.TokenString,Parser.SourcePos+1);
  while True do begin
    Parser.NextToken;
    if Parser.Token <> '.' then break;
    Parser.NextToken;
    Parser.CheckToken(toSymbol);
    PropertyNode.Add(Parser.TokenString,Parser.SourcePos+1);
  end;
  Parser.CheckToken('=');
  Parser.NextToken;
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
  else begin
    Parser.CheckTokenSymbol('INHERITED');
    ObjectNode.IsInherited := True;
  end;
  Parser.NextToken;
  Parser.CheckToken(toSymbol);
  if not Parser.TokenSymbolIs('END') then begin
    ObjectStartLine:=Parser.SourceLine;
    ObjectNode.Name := '';
    ObjectNode.TypeName := Parser.TokenString;
    ObjectNode.TypeNamePosition:=Parser.SourcePos+1;
    ObjectNode.ChildPos := -1;
    Parser.NextToken;
    if Parser.Token = ':' then begin
      Parser.NextToken;
      Parser.CheckToken(toSymbol);
      ObjectNode.Name := ObjectNode.TypeName;
      ObjectNode.NamePosition:=ObjectNode.TypeNamePosition;
      ObjectNode.TypeName := Parser.TokenString;
      ObjectNode.TypeNamePosition:=Parser.SourcePos+1;
      Parser.NextToken;
      if parser.Token = '[' then begin
        parser.NextToken;
        ObjectNode.ChildPos := parser.TokenInt;
        parser.NextToken;
        parser.CheckToken(']');
        parser.NextToken;
      end;
    end;

    // read property list
    while not (Parser.TokenSymbolIs('END')
    or Parser.TokenSymbolIs('OBJECT')
    or Parser.TokenSymbolIs('INHERITED')) do
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
  Parser.NextToken; // Skip 'END' token
  
  CloseChildNode;
end;

procedure TLFMTree.CreateChildNode(NodeClass: TLFMTreeNodeClass);
var
  NewNode: TLFMTreeNode;
begin
  NewNode:=NodeClass.CreateVirtual;
  NewNode.Tree:=Self;
  NewNode.StartPos:=Parser.SourcePos+1;
  NewNode.EndPos:=NewNode.StartPos;
  if CurNode<>nil then begin
    CurNode.AddChild(NewNode);
  end else begin
    Root:=NewNode;
  end;
  CurNode:=NewNode;
end;

procedure TLFMTree.CloseChildNode;
begin
  CurNode.EndPos:=Parser.SourcePos+1;
  CurNode:=CurNode.Parent;
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
    // skip object/inherited
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

end.


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, TypInfo, AVL_Tree,
  // LazUtils
  LazUtilities,
  // Codetools
  FileProcs, BasicCodeTools, CodeCache, KeywordFuncLists;
  
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
    function GetIdentifier: string; virtual;
    procedure FindIdentifier(out IdentStart, IdentEnd: integer);
    function GetPath: string;
    function GetSrcPos: string;
    function GetSource(Count: integer): string;
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
    TypeUnitName: string;
    TypeUnitNamePosition: integer;
    TypeName: string;
    TypeNamePosition: integer;
    AncestorTool: TObject; // TFindDeclarationTool
    AncestorNode: TObject; // TCodeTreeNode
    AncestorContextValid: boolean;
    constructor CreateVirtual; override;
    function GetFullName(UnitNameSep: char = '/'; WithName: boolean = true): string;
    function GetIdentifier: string; override;
    function GetPropertyPath: string;
  end;

  { TLFMNameParts }

  TLFMNameParts = class
  private
    FCount: integer;
    FNames: ^String;
    FNamePositions: PInteger;
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
    function GetPropertyPath: string;
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
    function ReadString: string; // for a string value
    procedure ReadLines(List: TStrings);
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
    lfmeUnitNotFound,
    lfmeIdentifierNotFound,
    lfmeIdentifierNotPublished,
    lfmeIdentifierMissingInCode,
    lfmeObjectNameMissing,
    lfmeObjectIncompatible,
    lfmePropertyNameMissing,
    lfmePropertyHasNoSubProperties,
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

  TLFMTokenKind = (
    ltkNone,
    ltkSymbol,
    ltkIdentifier,
    ltkUnicodeIdentifier,
    ltkHexNumber,
    ltkInteger,
    ltkFloat,
    ltkString
    );
  TLFMTokenKinds = set of TLFMTokenKind;

  TLFMTrees = class;

  { TLFMTree }

  TLFMTree = class
  private
    function GetColumn: integer;
    function GetSourcePos: integer;
  protected
    FLineNumber: integer;
    FLineStart: PChar;
    FTokenStart: PChar;
    FTokenEnd: PChar;
    FTokenChar: Char;
    FTokenKind: TLFMTokenKind;
    FSourceStart: PChar;
    FSourceEnd: PChar;
    procedure ParseError(const ErrorMessage: string);
    procedure ParseErrorExp(const Expected: string);
    function NextToken: Char;
    procedure HandleAlphaNum;
    procedure HandleHexNumber;
    procedure HandleNumber;
    procedure HandleString;
    procedure HandleUnknown;
    procedure ProcessValue;
    procedure ProcessProperty;
    procedure ProcessObject;
    procedure ProcessDottedIdentifier;
    procedure SkipBOM;
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
                          ContextNode: TLFMTreeNode = nil): TLFMPropertyNode;

    function TokenIsSymbol(const s: Shortstring): boolean;
    function TokenIsIdentifier(const s: Shortstring): boolean;
    function GetTokenString: string;
    function GetTokenInteger: int64;

    property SourcePos: integer read GetSourcePos; // 1-based absolute position in source
    property LineNumber: integer read FLineNumber; // 1-based
    property Column: integer read GetColumn; // 1-based
    property LineStart: PChar read FLineStart;
    property TokenStart: PChar read FTokenStart;
    property TokenEnd: PChar read FTokenEnd;
    property TokenChar: Char read FTokenChar;
    property TokenKind: TLFMTokenKind read FTokenKind;
    property SourceStart: PChar read FSourceStart;
    property SourceEnd: PChar read FSourceEnd;

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
    'UnitNotFound',
    'IdentifierNotFound',
    'IdentifierNotPublished',
    'IdentifierMissingInCode',
    'ObjectNameMissing',
    'ObjectIncompatible',
    'PropertyNameMissing',
    'PropertyHasNoSubProperties',
    'EndNotFound'
    );
    
  LFMNodeTypeNames: array[TLFMNodeType] of string = (
    'Object',
    'Property',
    'Value',
    'Enum'
    );

  LFMValueTypeNames: array[TLFMValueType] of string = (
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
  FTokenStart:=nil;
  FTokenEnd:=nil;
  FTokenKind:=ltkNone;
  FTokenChar:=#0;
  FLineNumber:=1;
  FSourceStart:=nil;
  FSourceEnd:=nil;
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

  FSourceStart:=LFMStream.Memory;
  if FSourceStart=nil then begin
    ParseError('stream is empty');
    exit;
  end;
  FSourceEnd:=FSourceStart+LFMStream.Size;
  FTokenStart:=FSourceStart;
  FTokenEnd:=FSourceStart;
  FLineStart:=FSourceStart;

  try
    try
      SkipBOM;
      NextToken;
      while TokenIsIdentifier('OBJECT')
          or TokenIsIdentifier('INHERITED')
          or TokenIsIdentifier('INLINE') do
        ProcessObject;
      Result:=true;
    except
      on E: EParserError do begin
      end;
    end;
  finally
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
  //DebugLn('TLFMTree.AddError ',NewError.AsString, ' NodePath=',NewError.GetNodePath);
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
      if CompareIdentifiers(PChar(ObjNode.Name),PChar(FirstPart))=0 then
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

function TLFMTree.GetColumn: integer;
begin
  if FTokenStart=nil then
    Result:=1
  else
    Result:=FTokenStart-FLineStart+1;
end;

function TLFMTree.GetSourcePos: integer;
begin
  if FTokenStart=nil then
    Result:=1
  else
    Result:=FTokenStart-FSourceStart+1;
end;

procedure TLFMTree.ParseError(const ErrorMessage: string);
begin
  AddError(lfmeParseError,nil,ErrorMessage,FTokenStart-FSourceStart+1);

  raise EParserError.Create(ErrorMessage);
end;

procedure TLFMTree.ParseErrorExp(const Expected: string);
var
  s: String;
  Cnt: SizeInt;
begin
  s:='';
  debugln(['TLFMTree.AddParseErrorExp ',PtrInt(FTokenStart-FSourceStart),'-',PtrInt(FTokenEnd-FSourceStart),' Char=',ord(FTokenChar),' Kind=',FTokenKind]);
  case FTokenChar of
  #0:
    s:='end of file';
  #32..#126:
    begin
      Cnt:=FTokenEnd-FTokenStart;
      if Cnt>30 then begin
        SetLength(s,30);
        System.Move(FTokenStart^,s[1],14);
        s[15]:='.';
        s[16]:='.';
        s[17]:='.';
        System.Move(FTokenEnd[-13],s[18],13);
      end else begin
        SetLength(s,cnt);
        System.Move(FTokenStart^,s[1],Cnt);
      end;
      if FTokenChar in ['a'..'z','A'..'Z'] then
        s:='"'+s+'"';
    end;
  else
    s:='symbol';
  end;
  s:='expected '+Expected+', but found '+s;
  ParseError(s);
end;

function TLFMTree.NextToken: Char;
var
  c: Char;
begin
  // skip whitespace
  FTokenStart:=FTokenEnd;
  repeat
    if FTokenStart=FSourceEnd then begin
      FTokenChar:=#0;
      FTokenKind:=ltkNone;
      exit(#0);
    end;
    case FTokenStart^ of
    #0:
      begin
        FTokenEnd:=FTokenStart;
        FTokenChar:=#0;
        FTokenKind:=ltkNone;
        exit(#0);
      end;
    ' ',#9: inc(FTokenStart);
    #10,#13:
      begin
        c:=FTokenStart^;
        inc(FTokenStart);
        if (FTokenStart<FSourceEnd) and (FTokenStart^ in [#10,#13]) and (c<>FTokenStart^) then
          inc(FTokenStart);
        FLineStart:=FTokenStart;
        inc(FLineNumber);
      end;
    else
      break;
    end;
  until false;

  c:=FTokenStart^;
  FTokenChar:=c;
  case c of
  '_','A'..'Z','a'..'z' : HandleAlphaNum;
  '$'                   : HandleHexNumber;
  '-','0'..'9'          : HandleNumber;
  '''','#'              : HandleString;
  else
    HandleUnknown;
  end;
  Result:=FTokenChar;
end;

procedure TLFMTree.HandleAlphaNum;
begin
  FTokenKind:=ltkIdentifier;
  FTokenEnd:=FTokenStart+1;
  while (FTokenEnd<FSourceEnd) and IsIdentChar[FTokenEnd^] do
    inc(FTokenEnd);
  //writeln('TLFMTree.HandleAlphaNum ',PtrInt(FTokenStart-FSourceStart),' ',PtrInt(FTokenEnd-FSourceStart),' ',PtrInt(FSourceEnd-FSourceStart));
end;

procedure TLFMTree.HandleHexNumber;
begin
  // first char is $
  FTokenKind:=ltkHexNumber;
  FTokenEnd:=FTokenStart+1;
  while (FTokenEnd<FSourceEnd) and IsHexNumberChar[FTokenEnd^] do
    inc(FTokenEnd);
  if FTokenEnd-FTokenStart=1 then
  begin
    FTokenKind:=ltkSymbol;
    AddError(lfmeParseError,nil,'expected hex number',FTokenStart-FSourceStart);
  end else begin
    FTokenKind:=ltkHexNumber;
  end;
end;

procedure TLFMTree.HandleNumber;
var
  c: Char;
  OldEnd: PChar;
begin
  // For example: 1  or  -1.2e-3
  FTokenKind:=ltkInteger;
  FTokenEnd:=FTokenStart+1;
  if FTokenChar='-' then begin
    if (FTokenEnd=FSourceEnd) or not (FTokenEnd^ in ['0'..'9']) then
    begin
      // minus without number
      FTokenKind:=ltkSymbol;
      exit;
    end;
    inc(FTokenEnd);
  end;
  while (FTokenEnd<FSourceEnd) do begin
    c:=FTokenEnd^;
    case c of
    '0'..'9':
      inc(FTokenEnd);
    '.':
      if FTokenKind=ltkInteger then begin
        inc(FTokenEnd);
        if (FTokenEnd=FSourceEnd) or not (FTokenEnd^ in ['0'..'9','e','E']) then
        begin
          dec(FTokenEnd);
          exit;
        end;
        FTokenKind:=ltkFloat;
      end else
        exit;
    'e','E':
      begin
        OldEnd:=FTokenEnd;
        inc(FTokenEnd);
        if (FTokenEnd=FSourceEnd) then begin
          // 1E<missing number>
          FTokenEnd:=OldEnd;
          exit;
        end;
        c:=FTokenEnd^;
        if c='-' then
          inc(FTokenEnd);
        if (FTokenEnd=FSourceEnd) or not (FTokenEnd^ in ['0'..'9']) then
        begin
          // 1E<missing number>
          FTokenEnd:=OldEnd;
          exit;
        end;
        FTokenKind:=ltkFloat;
        repeat
          inc(FTokenEnd);
        until (FTokenEnd=FSourceEnd) or not (FTokenEnd^ in ['0'..'9']);
        exit;
      end;
    else
      exit;
    end;
  end;
end;

procedure TLFMTree.HandleString;
var
  c: Char;
begin
  FTokenEnd:=FTokenStart;
  FTokenKind:=ltkString;
  repeat
    c:=FTokenEnd^;
    case c of
    '''':
      begin
        repeat
          inc(FTokenEnd);
          if FTokenEnd=FSourceEnd then
          begin
            ParseError('missing closing apostroph');
            exit;
          end;
          c:=FTokenEnd^;
          case c of
          #0,#10,#13:
            begin
              ParseError('missing closing apostroph');
              exit;
            end;
          '''':
            begin
              inc(FTokenEnd);
              break;
            end;
          end;
        until false;
        if FTokenEnd=FSourceEnd then exit;
      end;
    '#':
      begin
        inc(FTokenEnd);
        if (FTokenEnd=FSourceEnd) or not (FTokenEnd^ in ['0'..'9']) then
        begin
          ParseError('missing decimal after #');
          if FTokenStart+1=FTokenEnd then begin
            FTokenKind:=ltkSymbol;
            exit;
          end else begin
            dec(FTokenEnd);
            exit;
          end;
        end;
        repeat
          inc(FTokenEnd);
          if (FTokenEnd=FSourceEnd) then
            exit;
        until not (FTokenEnd^ in ['0'..'9']);
      end;
    else
      exit;
    end;
  until false;
end;

procedure TLFMTree.HandleUnknown;
begin
  FTokenKind:=ltkSymbol;
  FTokenEnd:=FTokenStart+1;
end;

procedure TLFMTree.ProcessValue;
var
  SymbolNode: TLFMValueNodeSymbol;
begin
  case FTokenKind of
  
  ltkInteger:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvInteger;
      NextToken;
      CloseChildNode;
    end;
    
  ltkFloat:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvFloat;
      NextToken;
      CloseChildNode;
    end;
    
  ltkString:
    begin
      CreateChildNode(TLFMValueNode);
      TLFMValueNode(CurNode).ValueType:=lfmvString;
      while NextToken = '+' do begin
        NextToken;   // Get next string fragment
        if FTokenKind<>ltkString then
          ParseErrorExp('string literal');
      end;
      CloseChildNode;
    end;
    
  ltkIdentifier:
    begin
      CreateChildNode(TLFMValueNodeSymbol);
      SymbolNode:=TLFMValueNodeSymbol(CurNode);
      if TokenIsIdentifier('end') then
        SymbolNode.SymbolType:=lfmsNone
      else if TokenIsIdentifier('True') then
        SymbolNode.SymbolType:=lfmsTrue
      else if TokenIsIdentifier('False') then
        SymbolNode.SymbolType:=lfmsFalse
      else if TokenIsIdentifier('nil') then
        SymbolNode.SymbolType:=lfmsNil
      else
      begin
        SymbolNode.SymbolType:=lfmsIdentifier;
        ProcessDottedIdentifier;
      end;
      if SymbolNode.SymbolType<>lfmsNone then
        NextToken;
      CloseChildNode;
    end;

  ltkSymbol:
    case fTokenChar of
    // Set
    '[':
      begin
        CreateChildNode(TLFMValueNodeSet);
        NextToken;
        if FTokenChar <> ']' then
          while True do
          begin
            CreateChildNode(TLFMEnumNode);
            if FTokenKind<>ltkIdentifier then
              ParseErrorExp('identifier');
            CloseChildNode;
            NextToken;
            if FTokenChar = ']' then
              break
            else if FTokenChar<>',' then
              ParseErrorExp(',');
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
        while FTokenChar <> ')' do begin
          if FTokenKind<>ltkString then
            ParseErrorExp('string');
          ProcessValue;
        end;
        NextToken;
        CloseChildNode;
      end;

    // Collection
    '<':
      begin
        CreateChildNode(TLFMValueNodeCollection);
        NextToken;
        while FTokenChar <> '>' do
        begin
          if not TokenIsIdentifier('item') then
            ParseErrorExp('"item"');
          NextToken;
          CreateChildNode(TLFMValueNodeList);
          while not TokenIsIdentifier('end') do
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
        // skip white space and hexnumbers until }
        inc(FTokenStart);
        repeat
          if FTokenStart=FSourceEnd then begin
            FTokenEnd:=FTokenStart;
            FTokenChar:=#0;
            ParseErrorExp('}');
          end;
          FTokenChar:=FTokenStart^;
          case FTokenChar of
          '}':
            begin
              inc(FTokenStart);
              FTokenEnd:=FTokenStart;
              break;
            end;
          ' ',#9:
            inc(FTokenStart);
          'a'..'f','A'..'F','0'..'9':
            repeat
              inc(FTokenStart);
              if (FTokenStart=FSourceEnd) or not IsHexNumberChar[FTokenStart^] then
                ParseError('binary must have even number of digits');
              inc(FTokenStart);
            until (FTokenStart=FSourceEnd) or not IsHexNumberChar[FTokenStart^];
          #10,#13:
            begin
              inc(FTokenStart);
              if (FTokenStart<FSourceEnd) and (FTokenStart^ in [#10,#13])
                  and (FTokenChar<>FTokenStart^) then
                inc(FTokenStart);
              inc(FLineNumber);
              FLineStart:=FTokenStart;
            end;
          else
            FTokenEnd:=FTokenStart+1;
            ParseErrorExp('}');
          end;
        until false;
        NextToken;
        CloseChildNode;
      end;
    end

  else
    ParseError('invalid property');
  end;
end;

procedure TLFMTree.ProcessProperty;
var
  PropertyNode: TLFMPropertyNode;
begin
  CreateChildNode(TLFMPropertyNode);
  PropertyNode:=TLFMPropertyNode(CurNode);
  // Get (dotted) name of property
  if FTokenKind<>ltkIdentifier then
    ParseErrorExp('property name');

  PropertyNode.Add(GetTokenString,SourcePos);
  while True do begin
    NextToken;
    if FTokenChar <> '.' then break;
    NextToken;
    if FTokenKind<>ltkIdentifier then
      ParseErrorExp('identifier');
    PropertyNode.Add(GetTokenString,SourcePos);
  end;
  if FTokenChar<>'=' then
    ParseErrorExp('=');
  NextToken;
  ProcessValue;
  CloseChildNode;
end;

procedure TLFMTree.ProcessObject;
var
  ObjectNode: TLFMObjectNode;
  ObjectStartLine: LongInt;
  HasDot: Boolean;
begin
  CreateChildNode(TLFMObjectNode);
  ObjectNode:=TLFMObjectNode(CurNode);
  if TokenIsIdentifier('OBJECT') then
    ObjectNode.IsInherited := False
  else if TokenIsIdentifier('INHERITED') then
    ObjectNode.IsInherited := True
  else if TokenIsIdentifier('INLINE') then
    ObjectNode.IsInline := True
  else
    ParseErrorExp('"object"');
  NextToken;
  if FTokenKind<>ltkIdentifier then
    ParseErrorExp('identifier');
  if not TokenIsIdentifier('END') then begin
    ObjectStartLine:=LineNumber;

    // read TypeName
    //   or ClassName.TypeName
    //   or Namespace.UnitName/ClassName.TypeName
    //   or Name:TypeName
    //   or Name:TypeName[decimal]
    //   or Name:UnitName/TypeName
    //   or Name:Namespace.UnitName/ClassName.TypeName
    ObjectNode.Name := '';
    ObjectNode.TypeName := GetTokenString;
    ObjectNode.TypeNamePosition := SourcePos;
    ObjectNode.ChildPos := -1;
    NextToken;
    HasDot:=false;
    while FTokenChar = '.' do begin
      HasDot:=true;
      NextToken;
      if FTokenKind<>ltkIdentifier then
        ParseErrorExp('identifier');
      ObjectNode.TypeName := ObjectNode.TypeName+'.'+GetTokenString;
      NextToken;
    end;

    if (not HasDot) and (FTokenChar = ':') then begin
      // Name:TypeName
      NextToken;
      if FTokenKind<>ltkIdentifier then
        ParseErrorExp('identifier');
      ObjectNode.Name := ObjectNode.TypeName;
      ObjectNode.NamePosition := ObjectNode.TypeNamePosition;
      ObjectNode.TypeName := GetTokenString;
      ObjectNode.TypeNamePosition := SourcePos;
      NextToken;
      while FTokenChar = '.' do begin
        NextToken;
        if FTokenKind<>ltkIdentifier then
          ParseErrorExp('identifier');
        ObjectNode.TypeName := ObjectNode.TypeName+'.'+GetTokenString;
        NextToken;
      end;
    end;

    if FTokenChar = '/' then begin
      // TypeUnitName/TypeName
      NextToken;
      if FTokenKind<>ltkIdentifier then
        ParseErrorExp('identifier');
      ObjectNode.TypeUnitName := ObjectNode.TypeName;
      ObjectNode.TypeUnitNamePosition := ObjectNode.TypeNamePosition;
      ObjectNode.TypeName := GetTokenString;
      ObjectNode.TypeNamePosition := SourcePos;
      NextToken;
      while FTokenChar = '.' do begin
        NextToken;
        if FTokenKind<>ltkIdentifier then
          ParseErrorExp('identifier');
        ObjectNode.TypeName := ObjectNode.TypeName+'.'+GetTokenString;
        NextToken;
      end;
    end;

    if FTokenChar = '[' then begin
      NextToken;
      if FTokenKind<>ltkInteger then
        ParseErrorExp('integer');
      ObjectNode.ChildPos := GetTokenInteger;
      NextToken;
      if FTokenChar<>']' then
        ParseErrorExp(']');
      NextToken;
    end;

    // read property list
    while not (TokenIsIdentifier('END')
        or TokenIsIdentifier('OBJECT')
        or TokenIsIdentifier('INHERITED')
        or TokenIsIdentifier('INLINE')) do
      ProcessProperty;

    // read child objects
    while not TokenIsIdentifier('END') do begin
      if FTokenChar=#0 then begin
        ParseError('END not found for'
          +' object='+ObjectNode.GetFullName
          +' starting at line '+IntToStr(ObjectStartLine));
      end;
      ProcessObject;
    end;
  end;
  NextToken; // Skip 'END' token
  
  CloseChildNode;
end;

procedure TLFMTree.ProcessDottedIdentifier;
begin
  while (FTokenEnd<FSourceEnd) and (FTokenEnd^='.') do begin
    inc(FTokenEnd);
    while (FTokenEnd<FSourceEnd) and (IsIdentChar[FTokenEnd^]) do inc(FTokenEnd);
  end;
end;

procedure TLFMTree.SkipBOM;
begin
  while (FTokenStart^=#$EF) and (FTokenStart[1]=#$BB) and (FTokenStart[2]=#$BF) do
    inc(FTokenStart,3);
  FTokenEnd:=FTokenStart;
  FLineStart:=FTokenStart;
end;

function TLFMTree.TokenIsSymbol(const s: Shortstring): boolean;
var
  p, q: PChar;
  l: SizeInt;
begin
  Result:=false;
  l:=length(s);
  if l=0 then exit;
  if l<>FTokenEnd-FTokenStart then exit;
  p:=FTokenStart;
  q:=@s[1];
  repeat
    if p^<>q^ then exit;
    inc(p);
    inc(q);
  until p=FTokenEnd;
  Result:=true;
end;

function TLFMTree.TokenIsIdentifier(const s: Shortstring): boolean;
var
  p, q: PChar;
  l: SizeInt;
begin
  Result:=false;
  l:=length(s);
  if l=0 then exit;
  if l<>FTokenEnd-FTokenStart then exit;
  p:=FTokenStart;
  q:=@s[1];
  repeat
    if (p^<>q^) and (UpChars[p^]<>UpChars[q^]) then exit;
    inc(p);
    inc(q);
  until p=FTokenEnd;
  Result:=true;
end;

function TLFMTree.GetTokenString: string;
var
  l: SizeInt;
begin
  if FTokenChar=#0 then exit('');
  l:=FTokenEnd-FTokenStart;
  SetLength(Result,l);
  Move(FTokenStart^,Result[1],l);
end;

function TLFMTree.GetTokenInteger: int64;
begin
  Result:=0;
  if FTokenKind in [ltkHexNumber,ltkInteger] then
    if not TryStrToInt64(GetTokenString,Result) then
      Result:=0;
end;

procedure TLFMTree.CreateChildNode(NodeClass: TLFMTreeNodeClass);
var
  NewNode: TLFMTreeNode;
begin
  NewNode:=NodeClass.CreateVirtual;
  NewNode.Tree:=Self;
  NewNode.StartPos:=fTokenStart-FSourceStart+1;
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
    CurNode.EndPos:=fTokenStart-FSourceStart+1;
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
  FindIdentifier(IdentStart,IdentEnd);
  if IdentStart<1 then exit;
  Result:=copy(Tree.LFMBuffer.Source,IdentStart,IdentEnd-IdentStart);
end;

procedure TLFMTreeNode.FindIdentifier(out IdentStart, IdentEnd: integer);
var
  Src: String;
  SrcLen: Integer;

  procedure NextIdentifier;
  begin
    while (IdentStart<=SrcLen) and (Src[IdentStart] in [#1..#32]) do
      inc(IdentStart);
    IdentEnd:=IdentStart;
    while (IdentEnd<=SrcLen)
    and (Src[IdentEnd] in ['A'..'Z','a'..'z','0'..'9','_','.']) do
      inc(IdentEnd);
  end;

begin
  IdentStart:=-1;
  IdentEnd:=-1;
  if (Tree=nil) or (Tree.LFMBuffer=nil) or (StartPos<1) then exit;
  Src:=Tree.LFMBuffer.Source;
  SrcLen:=length(Src);
  IdentStart:=StartPos;
  NextIdentifier;

  if TheType=lfmnObject then begin
    // skip object/inherited/inline
    IdentStart:=IdentEnd;
    NextIdentifier;
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
    if PrependStr<>'' then begin
      if Result<>'' then begin
        if ANode is TLFMObjectNode then
          Result:='.'+Result
        else
          Result:='/'+Result;
      end;
      Result:=PrependStr+Result;
    end else if ANode=Self then begin
      Result:=ANode.ClassName+' at '+ANode.GetSrcPos;
    end;
    ANode:=ANode.Parent;
  end;
end;

function TLFMTreeNode.GetSrcPos: string;
var
  Buf: TCodeBuffer;
begin
  if Tree=nil then
    exit('NoTree');
  Buf:=Tree.LFMBuffer;
  if Buf=nil then
    exit('NoLFMBuffer');
  Result:=Buf.AbsoluteToLineColStr(StartPos);
end;

function TLFMTreeNode.GetSource(Count: integer): string;
var
  Buf: TCodeBuffer;
  Src: String;
  Cnt: integer;
begin
  Result:='';
  if (Tree=nil) or (StartPos<1) or (EndPos<=StartPos) or (Count<=0) then
    exit;
  Buf:=Tree.LFMBuffer;
  if Buf=nil then
    exit;
  Src:=Buf.Source;
  Cnt:=length(Src)-StartPos+1;
  if Cnt>Count then Cnt:=Count;
  if Cnt>EndPos-StartPos then Cnt:=EndPos-StartPos;
  Result:=copy(Src,StartPos,Cnt);
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
  ChildPos:=-1;
end;

function TLFMObjectNode.GetFullName(UnitNameSep: char; WithName: boolean
  ): string;
begin
  Result:=TypeUnitName;
  if TypeName<>'' then begin
    if Result<>'' then
      Result:=Result+UnitNameSep+TypeName
    else
      Result:=TypeName;
  end;
  if (not WithName) or (Name='') then exit;
  if Result<>'' then
    Result:=Name+':'+Result
  else
    Result:=Name+':MissingLFMType';
end;

function TLFMObjectNode.GetIdentifier: string;
begin
  Result:=GetFullName;
end;

function TLFMObjectNode.GetPropertyPath: string;
var
  Node: TLFMTreeNode;
begin
  Result:=Name;
  Node:=Parent;
  while Node<>nil do begin
    if Node is TLFMObjectNode then
      Result:=TLFMObjectNode(Node).Name+'.'+Result;
    Node:=Node.Parent;
  end;
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

function TLFMPropertyNode.GetPropertyPath: string;
begin
  Result:=CompleteName;
  if Parent is TLFMObjectNode then
    Result:=TLFMObjectNode(Parent).GetPropertyPath+'.'+Result;
end;

{ TLFMValueNode }

constructor TLFMValueNode.CreateVirtual;
begin
  TheType:=lfmnValue;
  ValueType:=lfmvNone;
end;

function TLFMValueNode.ReadString: string;
var
  Src: String;
  p, StartP, i: integer;
begin
  Result:='';
  if ValueType<>lfmvString then exit;
  Src:=Tree.LFMBuffer.Source;
  p:=StartPos;
  while p<EndPos do begin
    case Src[p] of
    '''':
      begin
        inc(p);
        StartP:=p;
        repeat
          if p=EndPos then exit; // error
          if Src[p]='''' then break;
          inc(p);
        until false;
        Result:=Result+copy(Src,StartP,p-StartP);
        inc(p);
      end;
    '+':
      inc(p); // one string broken into several lines
    '#':
      begin
        i:=0;
        inc(p);
        while (p<EndPos) and (Src[p] in ['0'..'9']) do begin
          i:=i*10 + ord(Src[p])-ord('0');
          if i>255 then
            exit; // error
          inc(p);
        end;
        Result:=Result+chr(i);
      end;
    ' ',#9,#10,#13: inc(p);
    else
      exit; // error
    end;
  end;
end;

procedure TLFMValueNode.ReadLines(List: TStrings);
var
  Node: TLFMValueNode;
begin
  if ValueType=lfmvString then
    List.Add(ReadString)
  else if ValueType=lfmvList then begin
    Node:=FirstChild as TLFMValueNode;
    while Node<>nil do begin
      if Node.ValueType=lfmvString then
        List.Add(Node.ReadString);
      Node:=TLFMValueNode(Node.NextSibling);
    end;
  end;
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
var
  ObjNode: TLFMObjectNode;
begin
  Result:=(ErrorType in [lfmeIdentifierNotFound,lfmeMissingRoot])
      and (Node is TLFMObjectNode);
  if not Result then exit;
  ObjNode:=TLFMObjectNode(Node);
  if ObjNode.TypeName='' then
    exit(false);
  if (Position>=ObjNode.TypeNamePosition)
      and (Position<ObjNode.TypeNamePosition+length(ObjNode.TypeName)) then
    exit(true);
  if (ObjNode.TypeUnitName<>'')
      and (Position>=ObjNode.TypeUnitNamePosition)
      and (Position<ObjNode.TypeUnitNamePosition+length(ObjNode.TypeUnitName)) then
    exit(true);
  Result:=false;
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


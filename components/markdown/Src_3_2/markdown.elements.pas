{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown basic block definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit MarkDown.Elements;

{$mode ObjFPC}
{$h+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, System.Regexpr, System.CodePages.unicodedata, 
{$ELSE}  
  Classes, SysUtils, Contnrs, RegExpr, UnicodeData, 
{$ENDIF}
  MarkDown.Utils, 
  Markdown.HTMLEntities;

const
  GrowSize = 10;

Type
  EMarkDown = class(Exception);

  TWhitespaceMode = (wsLeave, wsTrim, wsStrip);

  TPosition = record
    line : integer;
    col : integer;
  end;

  TMarkDownElement = class (TObject);
  TMarkDownElementClass = class of TMarkDownElement;

  TTextNodeKind = (nkNamed,nkLineBreak,nkText,nkCode,nkURI,nkEmail,nkImg);
  TTextNodeKinds = set of TTextNodeKind;

  TNodeStyle = (nsStrong,nsEmph,nsDelete);
  TNodeStyles = Set of TNodeStyle;

  { TMarkDownTextNode }

  TMarkDownTextNode = class(TMarkDownElement)
  private
    FKind: TTextNodeKind;
    FName : Ansistring;
    FAttrs: THashTable;
    FContent : AnsiString;
    FBuild : RawByteString;
    FLength : integer;
    FPos : TPosition;
    FActive : Boolean;
    FStyles: TNodeStyles;
    function GetAttrs: THashTable;
    function GetHasAttrs: Boolean;
    function getText : AnsiString;
    function GetNodetext : ansistring;
    procedure SetName(const Value: AnsiString);
    procedure SetActive(const aValue: boolean);
  public
    constructor Create(aPos : TPosition; aKind : TTextNodeKind);
    destructor Destroy; override;
    procedure AddStyle(aStyle : TNodeStyle);
    procedure IncCol(aCount : integer);
    property Kind : TTextNodeKind Read FKind Write FKind;
    property Name : AnsiString read FName write SetName;
    property Attrs : THashTable read GetAttrs;
    property HasAttrs : Boolean Read GetHasAttrs;
    property NodeText : ansistring read GetNodetext;
    procedure AddText(ch : char); overload;
    procedure AddText(s : AnsiString); overload;
    procedure RemoveChars(count : integer);
    function IsEmpty : boolean;
    property Pos : TPosition Read FPos;
    property Active : Boolean Read FActive Write SetActive;
    property Styles : TNodeStyles Read FStyles Write FStyles;
  end;

  { TMarkDownTextNodeList }

  TMarkDownTextNodeList = class (specialize TGFPObjectList<TMarkDownTextNode>)
  private
    procedure ClearActive; inline;
  public
    // When the last block is active, add to that block. Otherwise, create a new text node
    function AddText(aPos: TPosition; aContent: AnsiString): TMarkDownTextNode;
    function AddTextNode(aPos: TPosition; aKind: TTextNodeKind; cnt: AnsiString; aDoClose: Boolean=True): TMarkDownTextNode; // always make a new node, and make it inactive
    procedure RemoveAfter(node : TMarkDownTextNode);
    function LastNode : TMarkDownTextNode;
    procedure ApplyStyleBetween(aStart,aStop : TMarkDownTextNode; aStyle : TNodeStyle);
  end;

  { TMarkdownBlock }

  TMarkdownBlock = class abstract (TMarkDownElement)
  private
    FClosed: boolean;
    FLine : integer;
    FParent : TMarkdownBlock;
  protected
    procedure SetClosed(const aValue: boolean); virtual;
    function GetChild(aIndex : Integer): TMarkDownBlock; virtual;
    function GetChildCount: Integer; virtual;
    procedure AddChild(aChild : TMarkDownBlock); virtual;
    function GetLastChild: TMarkDownBlock; virtual;
  public
    constructor Create(aParent : TMarkDownBlock; aLine : Integer);  virtual; reintroduce;
    procedure Dump(const aIndent : string = '');
    Function GetFirstText : String;
    function WhitespaceMode : TWhitespaceMode; virtual;
    property Closed : boolean read FClosed write SetClosed;
    property Line : Integer read FLine;
    property Parent : TMarkDownBlock read FParent;
    property LastChild : TMarkDownBlock Read GetLastChild;
    property ChildCount : Integer read GetChildCount;
    property Children[aIndex : Integer] : TMarkDownBlock read GetChild; default;
  end;
  TMarkdownBlockClass = class of TMarkdownBlock;

  { TMarkDownBlockList }

  TMarkDownBlockList = class(specialize TGFPObjectList<TMarkdownBlock>)
    function lastblock : TMarkDownBlock;
  end;

  { TMarkDownContainerBlock }

  TMarkDownContainerBlock = class (TMarkdownBlock)
  private
    FBlocks: TMarkDownBlockList;
  protected
    procedure AddChild(aChild : TMarkDownBlock); override;
    function GetChild(aIndex : Integer): TMarkDownBlock; override;
    function GetChildCount: Integer; override;
    function GetLastChild: TMarkDownBlock; override;
  public
    constructor Create(aParent : TMarkDownBlock; aLine : Integer); override;
    destructor Destroy; override;
    procedure DeleteChild(aIndex : Integer);
    property Blocks : TMarkDownBlockList read FBlocks;
  end;

  TMarkDownDocument = class (TMarkDownContainerBlock);

  { TMarkDownParagraphBlock }

  TMarkDownParagraphBlock = class (TMarkDownContainerBlock)
  private
    FHeader: integer;
  public
    function IsPlainPara : boolean; virtual;
    property Header : integer read FHeader write FHeader;
  end;

  TMarkDownQuoteBlock = class (TMarkDownParagraphBlock)
  public
    function IsPlainPara : boolean; override;
  end;

  TMarkDownListBlock = class (TMarkDownContainerBlock)
  private
    FOrdered: boolean;
    FStart: integer;
    FMarker: AnsiString;
    FLoose: boolean;
    FLastIndent: integer;
    FBaseIndent: integer;
    FHasSeenEmptyLine : boolean; // parser state
  public
    function Grace : integer;
    property Ordered : boolean read FOrdered write FOrdered;
    property BaseIndent : integer read FBaseIndent write FBaseIndent;
    property LastIndent : integer read FLastIndent write FLastIndent;
    property Start : integer read FStart write FStart;
    property Marker : AnsiString read FMarker write FMarker;
    property Loose : boolean read FLoose write FLoose;
    property HasSeenEmptyLine : boolean read FHasSeenEmptyLine Write FHasSeenEmptyLine; // parser state
  end;

  TMarkDownListItemBlock = class (TMarkDownParagraphBlock)
  public
    function isPlainPara : boolean; override;
  end;

  TMarkDownHeadingBlock = class (TMarkDownContainerBlock)
  private
    FLevel: integer;
  public
    constructor Create(aParent : TMarkDownBlock; aLine, alevel : Integer); reintroduce;
    property Level : integer read FLevel write FLevel;
  end;

  TMarkDownCodeBlock = class (TMarkDownContainerBlock)
  private
    FFenced: boolean;
    FLang: AnsiString;
  public
    function WhiteSpaceMode : TWhitespaceMode; override;
    property Fenced : boolean read FFenced write FFenced;
    property Lang : AnsiString read FLang write FLang;
  end;


  TMarkDownTableRowBlock = class (TMarkDownContainerBlock);

  TCellAlign = (caLeft, caCenter, caRight);
  TCellAlignArray = array of TCellAlign;

  { TMarkDownTableBlock }

  TMarkDownTableBlock = class (TMarkDownContainerBlock)
  private
    FColumns: TCellAlignArray;
  public
    property Columns : TCellAlignArray read FColumns Write FColumns;
  end;

  TMarkDownLeafBlock = class abstract (TMarkdownBlock);

  TMarkDownThematicBreakBlock = class (TMarkDownLeafBlock)
  end;

  { TMarkDownTextBlock }

  TMarkDownTextBlock = class (TMarkDownLeafBlock)
  private
    FText: AnsiString;
    FNodes : TMarkDownTextNodeList;
  protected
    procedure SetClosed(const aValue: boolean);override;
  public
    constructor Create(aParent : TMarkDownBlock; aLine : integer; aText : AnsiString); reintroduce;
    destructor Destroy; override;
    property Text : AnsiString read FText write FText;
    property Nodes : TMarkDownTextNodeList Read FNodes Write FNodes;
  end;

implementation

{ TMarkDownTextNode }

procedure TMarkDownTextNode.addText(s: AnsiString);

var
  len,AddLen,NewLen : Integer;
begin
  AddLen:=Length(S);
  if AddLen=0 then exit;
  Len:=Length(FBuild);
  NewLen:=FLength+AddLen;
  if NewLen>=Len then
    setLength(FBuild, NewLen+GrowSize);
  Move(S[1],FBuild[FLength+1],AddLen);
  inc(FLength,AddLen);
end;

constructor TMarkDownTextNode.Create(aPos : TPosition; aKind : TTextNodeKind);
begin
  inherited Create;
  FActive:=True;
  FPos:=aPos;
  FKind:=aKind;
end;

procedure TMarkDownTextNode.addText(ch: char);
var
  len : Integer;
begin
  Len:=Length(FBuild);
  if FLength>=Len then
    setLength(FBuild, Len+GrowSize);
  inc(FLength);
  FBuild[FLength]:=ch;
end;

destructor TMarkDownTextNode.Destroy;
begin
  FreeAndNil(FAttrs);
  inherited;
end;

procedure TMarkDownTextNode.AddStyle(aStyle: TNodeStyle);
begin
  include(FStyles,aStyle);
end;

procedure TMarkDownTextNode.IncCol(aCount: integer);
begin
  Inc(FPos.Col,aCount);
end;

function TMarkDownTextNode.GetAttrs: THashTable;
begin
  if FAttrs = nil then
    FAttrs:=THashTable.create;
  Result:=FAttrs;
end;

function TMarkDownTextNode.GetHasAttrs: Boolean;
begin
  Result:=Assigned(FAttrs);
end;

function TMarkDownTextNode.getText: AnsiString;
begin
  Active:=False;
  Result:=FContent;
end;

function TMarkDownTextNode.GetNodetext: ansistring;
begin
  Result:=Copy(FContent,1,Length(FContent));
end;

function TMarkDownTextNode.isEmpty: boolean;
begin
  if Active then
    Result:=FBuild = ''
  else
    Result:=FContent = '';
end;

procedure TMarkDownTextNode.removeChars(count : integer);
begin
  Active:=False;
  delete(FContent, 1, count);
end;


procedure TMarkDownTextNode.SetActive(const aValue: boolean);
begin
  if FActive and not aValue then
    FContent:=Copy(FBuild,1,FLength);
  FActive:=aValue;
end;

procedure TMarkDownTextNode.SetName(const Value: AnsiString);
begin
  FName:=Value;
  Active:=False;
end;

{ TMarkDownTextNodeList }

procedure TMarkDownTextNodeList.ClearActive;
begin
  if count > 0 then
    Self[Count-1].Active:=False;
end;

procedure TMarkDownTextNodeList.ApplyStyleBetween(aStart,aStop : TMarkDownTextNode; aStyle : TNodeStyle);

var
  Idx,lStart,lStop : Integer;

begin
  lStart:=IndexOf(aStart);
  lStop:=IndexOf(aStop)-1;
  For Idx:=lStart to lStop do
    begin
    Elements[Idx].AddStyle(aStyle)
    end;
end;

function TMarkDownTextNodeList.addText(aPos : TPosition; aContent: AnsiString): TMarkDownTextNode;
var
  lNode : TMarkDownTextNode;
begin
  lNode:=Nil;
  if (Count>0) then
    lNode:=Elements[Count-1];
  if (lNode=Nil) or not lNode.Active then
    begin
    Result:=TMarkDownTextNode.Create(aPos,nkText);
    add(Result);
    end
  else
    Result:=lNode;
  Result.addText(aContent);
end;

function TMarkDownTextNodeList.addTextNode(aPos : TPosition; aKind : TTextNodeKind; cnt: AnsiString; aDoClose : Boolean = True): TMarkDownTextNode;
begin
  ClearActive;
  Result:=TMarkDownTextNode.Create(aPos,aKind);
  add(Result);
  Result.addText(cnt);
  if aDoClose then
    Result.Active:=False;
end;

procedure TMarkDownTextNodeList.removeAfter(node: TMarkDownTextNode);
var
  i, idx : integer;

begin
  idx:=indexOf(node);
  for i:=count-1 downto idx+1 do
    Delete(i);
end;

function TMarkDownTextNodeList.lastNode: TMarkDownTextNode;
begin
  Result:=Nil;
  if Count>0 then
    Result:=Elements[Count-1];
end;

{ TMarkdownBlock }

function TMarkdownBlock.GetLastChild: TMarkDownBlock;
begin
  Result:=Nil;
end;

procedure TMarkdownBlock.SetClosed(const aValue: boolean);
begin
  if FClosed=aValue then Exit;
  FClosed:=aValue;
  if aValue and (ChildCount>0) then
    Children[ChildCount-1].Closed:=True;
end;

function TMarkdownBlock.GetChild(aIndex : Integer): TMarkDownBlock;
begin
  if aIndex<0 then ; // Silence compiler warning
  Result:=Nil;
end;

function TMarkdownBlock.GetChildCount: Integer;
begin
  Result:=0;
end;

procedure TMarkdownBlock.AddChild(aChild: TMarkDownBlock);
begin
  if (aChild<>Nil) then
  Raise Exception.Create('Cannot add child to simple block');
end;

constructor TMarkdownBlock.Create(aParent: TMarkDownBlock; aLine: Integer);
begin
  Inherited Create;
  FParent:=aParent;
  if assigned(aParent) then
    aParent.AddChild(Self);
  FLine:=aLine;
end;

procedure TMarkdownBlock.dump(const aIndent: string = '');
var
  I : Integer;
begin
  Write(aIndent);
  if not Closed then
    Write('! ')
  else
    Write('  ');
  Writeln(ClassName);
  For I:=0 to ChildCount-1 do
    Children[i].Dump(aIndent+'  ');
end;

function TMarkdownBlock.WhiteSpaceMode: TWhitespaceMode;
begin
  Result:=wsTrim;
end;

function TMarkDownBlock.GetFirstText: String;
var
  lText : TMarkDownTextBlock;
begin
  Result:='';
  if ChildCount=0 then
    exit;
  if not (Children[0] is TMarkDownTextBlock) then
    exit;
  lText:=TMarkDownTextBlock(Children[0]);
  if lText.Nodes.Count=0 then
    exit;
  Result:=lText.Nodes[0].NodeText;
end;

{ TMarkDownBlockList }

function TMarkDownBlockList.lastblock: TMarkDownBlock;
begin
  Result:=nil;
  if Count>0 then
    Result:=Self[Count-1];
end;

{ TMarkDownContainerBlock }

procedure TMarkDownContainerBlock.AddChild(aChild: TMarkDownBlock);
begin
  if aChild=Nil then
    Raise EMarkDown.CreateFmt('Cannot add nil child to block "%s"',[ClassName]);
  FBlocks.Add(aChild);
end;

function TMarkDownContainerBlock.GetChild(aIndex: Integer): TMarkDownBlock;
begin
  Result:=FBlocks[aIndex];
end;

function TMarkDownContainerBlock.GetChildCount: Integer;
begin
  Result:=FBlocks.Count;
end;

function TMarkDownContainerBlock.GetLastChild: TMarkDownBlock;
begin
  if FBlocks.Count>0 then
    Result:=FBlocks[FBlocks.Count-1]
  else
    Result:=Nil;
end;

constructor TMarkDownContainerBlock.Create(aParent : TMarkDownBlock; aLine : Integer);
begin
  inherited create(aParent,aLine);
  FBlocks:=TMarkDownBlockList.Create(true);
end;

destructor TMarkDownContainerBlock.Destroy;
begin
  FreeAndNil(FBlocks);
  inherited Destroy;
end;

procedure TMarkDownContainerBlock.DeleteChild(aIndex: Integer);
begin
  FBlocks.Delete(aIndex);
end;

{ TMarkDownParagraphBlock }

function TMarkDownParagraphBlock.IsPlainPara: boolean;
begin
  Result:=FHeader = 0;
end;


{ TMarkDownQuoteBlock }

function TMarkDownQuoteBlock.isPlainPara: boolean;
begin
  Result:=false;
end;

{ TMarkDownListBlock }

function TMarkDownListBlock.grace: integer;
begin
  if ordered then
    Result:=2
  else
    Result:=1;
end;

{ TMarkDownListItemBlock }

function TMarkDownListItemBlock.isPlainPara: boolean;
begin
  Result:=false;
end;

{ TMarkDownHeadingBlock }

constructor TMarkDownHeadingBlock.Create(aParent : TMarkDownBlock;aLine, aLevel: Integer);
begin
  Inherited Create(aParent,aLine);
  FLevel:=aLevel;
end;

{ TMarkDownCodeBlock }


function TMarkDownCodeBlock.WhiteSpaceMode: TWhitespaceMode;
begin
  Result:=wsLeave;
end;

{ TMarkDownTableBlock }

{ TMarkDownTextBlock }

procedure TMarkDownTextBlock.SetClosed(const aValue: boolean);
begin
  inherited SetClosed(aValue);
  if assigned(FNodes) then
    FNodes.ClearActive;
end;

constructor TMarkDownTextBlock.Create(aParent: TMarkDownBlock; aLine: integer; aText: AnsiString);
begin
  inherited Create(aParent,aLine);
  FText:=aText;
end;

destructor TMarkDownTextBlock.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

end.

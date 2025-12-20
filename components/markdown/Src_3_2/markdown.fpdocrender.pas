{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown FPDoc input file renderer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.FPDocRender;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.StrUtils, System.Contnrs, Xml.Dom, Xml.Writer,
{$ELSE}  
  Classes, SysUtils, strutils, contnrs, dom, XMLWrite,
{$ENDIF}
  MarkDown.Elements, MarkDown.Render, MarkDown.Utils;

type
  TElementType = (etPackage, etModule, etTopic, etElement);
  TSectionType = (stShort,stDescription,stErrors,stExamples,stSeeAlso);

const
  SectionNodeNames : Array[TSectionType] of string = ('short','descr','errors','examples','seealso');
  ElementNodeNames : Array[TElementType] of string = ('package','module','topic','element');

type
  EFPDocRender = Class(EMarkDown);
  { TMarkDownFPDocRenderer }

  TMarkDownFPDocRenderer = class(TMarkDownRenderer)
  private
    FDoc : TXMLDocument;
    FFPDoc: String;
    FPackageName: String;
    FStack: Array[0..100] of TDomElement;
    FStackCount : Integer;
    FSkipParagraph : Boolean;
    function GetParent: TDomElement;
    procedure PushElement(aElement : TDomElement);
    function PopElement : TDomElement;
  Protected
    Procedure AppendText(const aContent : String);
    function Push(const aElementName : String; const aName : string = '') : TDOMElement;
    function PushSection(aSection : TSectionType) : TDomElement;
    function Pop : TDomElement;
    function PopTill(const aElementName : string) : TDomElement;
    function PopTill(const aElementNames : array of string) : TDomElement;
    Property Doc : TXMLDocument Read FDoc;
    Property Parent : TDomElement Read GetParent;
    Property SkipParagraph : Boolean Read FSkipParagraph;
  public
    procedure RenderToXML(aDocument : TMarkDownDocument; aXML : TXMLDocument);
    procedure RenderToStream(aDocument : TMarkDownDocument; aStream : TStream);
    Procedure RenderDocument(aDocument : TMarkDownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkDownDocument; aDest : TStrings); overload;
    procedure RenderChildren(aBlock : TMarkDownContainerBlock; aAppendNewLine : Boolean); overload;
    function RenderFPDoc(aDocument : TMarkDownDocument) : string;
    Property PackageName : String read FPackageName Write FPackageName;
    Property FPDoc : String Read FFPDoc;
  end;

  { TFPDocMarkDownBlockRenderer }

  TFPDocMarkDownBlockRenderer = Class (TMarkDownBlockRenderer)
  Private
    function GetFPDocRenderer: TMarkDownFPDocRenderer;
    function GetParent: TDomElement;
  protected
    procedure CheckParent(const aParent,aChild : String);
    function CheckIsValidName(const aText : string) : boolean;
    function GetSectionType(const aText: string): TSectionType;
    function GetElementType(var aText: string): TElementType;
    procedure Append(const S : String); inline;
    procedure AppendNl(const S : String = ''); inline;
  public
    property FPDoc : TMarkDownFPDocRenderer Read GetFPDocRenderer;
    Property Parent : TDomElement Read GetParent;
  end;
  TFPDocMarkDownBlockRendererClass = class of TFPDocMarkDownBlockRenderer;
  { TFPDocMarkDownTextRenderer }

  TFPDocMarkDownTextRenderer = class(TMarkDownTextRenderer)
  Private
    FStyleStack: Array of TNodeStyle;
    FStyleStackLen : Integer;
    FLastStyles : TNodeStyles;
    FKeys : Array of String;
    FKeyCount : integer;
    FText : String;
    procedure DoKey(aItem: AnsiString; const aKey: AnsiString; var aContinue: Boolean);
    procedure EmitStyleDiff(aStyles: TNodeStyles);
    function GetFPDocRenderer: TMarkDownFPDocRenderer;
    function GetNodeTag(aElement: TMarkDownTextNode): string;
    function MustCloseNode(aElement: TMarkDownTextNode): boolean;
  protected
    procedure StartText;
    procedure EndText;
    procedure PushStyle(aStyle : TNodeStyle);
    function PopStyles(aStyle: TNodeStyles): TNodeStyle;
    procedure PopStyle(aStyle : TNodeStyle);
    procedure Append(const S : String); inline;
    procedure DoRender(aElement: TMarkDownTextNode); override;
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    property FPDoc : TMarkDownFPDocRenderer Read GetFPDocRenderer;
    function renderAttrs(aElement: TMarkDownTextNode): AnsiString;
  end;
  TFPDocMarkDownTextRendererClass = class of TFPDocMarkDownTextRenderer;

  { TFPDocParagraphBlockRenderer }

  TFPDocParagraphBlockRenderer = class (TFPDocMarkDownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownQuoteBlockRenderer }

  TFPDocMarkDownQuoteBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownTextBlockRenderer }

  TFPDocMarkDownTextBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownListBlockRenderer }

  TFPDocMarkDownListBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownListItemBlockRenderer }

  TFPDocMarkDownListItemBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownCodeBlockRenderer }

  TFPDocMarkDownCodeBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownHeadingBlockRenderer }

  TFPDocMarkDownHeadingBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownThematicBreakBlockRenderer }

  TFPDocMarkDownThematicBreakBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownTableBlockRenderer }

  TFPDocMarkDownTableBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TMarkDownTableRowBlockRenderer }

  TFPDocMarkDownTableRowBlockRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TFPDocMarkDownDocumentRenderer }

  TFPDocMarkDownDocumentRenderer = class(TFPDocMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;


implementation

type

  { TStringBuilderHelper }

  TStringBuilderHelper = class helper for TAnsiStringBuilder
    function Append(aAnsiString : Ansistring) : TAnsiStringBuilder;
  end;


function TStringBuilderHelper.Append(aAnsiString: Ansistring): TAnsiStringBuilder;
begin
  Result:=Inherited Append(aAnsiString,0,System.Length(aAnsistring))
end;

{ TMarkDownBlockRenderer }

function TFPDocMarkDownBlockRenderer.GetFPDocRenderer: TMarkDownFPDocRenderer;
begin
  if Renderer is TMarkDownFPDocRenderer then
    Result:=TMarkDownFPDocRenderer(Renderer)
  else
    Result:=Nil;
end;

function TFPDocMarkDownBlockRenderer.GetParent: TDomElement;
begin
  Result:=FPDoc.Parent;
end;

procedure TFPDocMarkDownBlockRenderer.CheckParent(const aParent, aChild: String);
begin
  if (Parent.NodeName<>aParent) then
    Raise EFPDocRender.CreateFmt('Cannot have %s below %s',[aChild,aParent]);
end;

function TFPDocMarkDownBlockRenderer.CheckIsValidName(const aText: string): boolean;
const
  StartIdentChars = ['a'..'z','A'..'Z','_'];
  AllIdentChars = StartIdentChars+['.','_','0'..'9'];
var
  I,Len : integer;
begin
  len:=Length(aText);
  Result:=(Len>0) and (aText[1] in StartIdentChars);
  I:=2;
  While Result and (I<=Len) do
    begin
    Result:=aText[I] in AllIdentChars;
    inc(i);
    end;
end;

function TFPDocMarkDownBlockRenderer.GetSectionType(const aText: string): TSectionType;
var
  lText : string;
begin
  lText:=LowerCase(Trim(aText));
  case lText of
    'short' : result:=stShort;
    'descr',
    'description' : result:=stDescription;
    'errors' : Result:=stErrors;
    'example',
    'examples' : Result:=stExamples;
    'seealso': Result:=stSeeAlso;
  else
    result:=stDescription;
  end;
end;

function TFPDocMarkDownBlockRenderer.GetElementType(var aText: string): TElementType;
var
  p : integer;
begin
  Result:=etElement;
  aText:=Trim(aText);
  p:=Pos(':',aText);
  if p=0 then
    exit;
  if SameText(Copy(aText,1,P-1),'topic') then
    begin
    Result:=etTopic;
    Delete(aText,1,P);
    aText:=Trim(aText);
    end;
end;

procedure TFPDocMarkDownBlockRenderer.Append(const S: String);
begin
  FPDoc.AppendText(S);
end;

procedure TFPDocMarkDownBlockRenderer.AppendNl(const S: String);
begin
  FPDoc.AppendText(S);
end;



{ TMarkDownFPDocRenderer }

function TMarkDownFPDocRenderer.GetParent: TDomElement;
begin
  if FStackCount>0 then
    Result:=FStack[FStackCount-1]
  else
    Result:=Nil;
end;

function TMarkDownFPDocRenderer.Push(const aElementName: String; const aName: string): TDOMElement;
begin
  Result:=FDoc.CreateElement(aElementName);
  PushElement(Result);
  if aName<>'' then
    Result['name']:=aName;
end;

procedure TMarkDownFPDocRenderer.PushElement(aElement: TDomElement);
begin
  if FStackCount=Length(FStack) then
    Raise EFPDocRender.Create('Max stack size reached');
  if FStackCount=0 then
    FDoc.AppendChild(aElement)
  else
    Parent.AppendChild(aElement);
  FStack[FStackCount]:=aElement;
  Inc(FStackCount);
end;

function TMarkDownFPDocRenderer.PopElement: TDomElement;
begin
  if FStackCount>0 then
    begin
    Result:=FStack[FStackCount-1];
    Dec(FStackCount);
    end
  else
    Result:=Nil;
end;

procedure TMarkDownFPDocRenderer.AppendText(const aContent: String);
begin
  Parent.AppendChild(FDoc.CreateTextNode(aContent))
end;

function TMarkDownFPDocRenderer.PushSection(aSection: TSectionType): TDomElement;
begin
  Result:=Push(SectionNodeNames[aSection]);
  FSkipParagraph:=aSection in [stShort,stSeeAlso];
end;

function TMarkDownFPDocRenderer.Pop: TDomElement;
begin
  Result:=PopElement;
end;

function TMarkDownFPDocRenderer.PopTill(const aElementName: string): TDomElement;
begin
  PopTill([aElementName]);
end;


function TMarkDownFPDocRenderer.PopTill(const aElementNames: array of string): TDomElement;
begin
  FSkipParagraph:=False;
  While IndexStr(UTF8Encode(Parent.NodeName),aElementNames)=-1 do
    begin
    Pop;
    if Parent=Nil then
      Raise EFPDocRender.CreateFmt('Could not pop to %s',[aElementNames[0]]);
    end;
  Result:=Parent;
end;

procedure TMarkDownFPDocRenderer.RenderToXML(aDocument: TMarkDownDocument; aXML: TXMLDocument);
begin
  FDoc:=aXML;
  try
    Push('fpdoc-descriptions');
    Push('package',FPackageName);
    RenderBlock(aDocument);
    Pop;
    Pop;
  finally
    FDoc:=Nil;
  end;
end;


procedure TMarkDownFPDocRenderer.RenderToStream(aDocument: TMarkDownDocument; aStream: TStream);
var
  lDoc : TXMLDocument;
begin
  LDoc:=TXMLDocument.Create;
  try
    RenderToXML(aDocument,LDoc);
    WriteXML(LDoc,aStream);
  finally
    FreeAndNil(LDoc);
  end;
end;

procedure TMarkDownFPDocRenderer.RenderDocument(aDocument: TMarkDownDocument);
var
  S : TStringStream;
begin
  S:=TStringStream.Create('');
  try
    RenderToStream(aDocument,S);
    FFPDoc:=S.DataString;
  finally
    S.Free;
  end;
end;

procedure TMarkDownFPDocRenderer.RenderDocument(aDocument: TMarkDownDocument; aDest: TStrings);
begin
  aDest.Text:=RenderFPDoc(aDocument);
end;

procedure TMarkDownFPDocRenderer.RenderChildren(aBlock: TMarkDownContainerBlock; aAppendNewLine: Boolean);
var
  i : integer;
begin
  for I:=0 to aBlock.Blocks.Count-1 do
    RenderBlock(aBlock.Blocks[I]);
end;

function TMarkDownFPDocRenderer.RenderFPDoc(aDocument: TMarkDownDocument): string;
begin
  RenderDocument(aDocument);
  Result:=FFPDoc;
  FFPDoc:='';
end;


procedure TFPDocMarkDownTextRenderer.Append(const S: String);
begin
  FText:=FText+S;
end;

function TFPDocMarkDownTextRenderer.MustCloseNode(aElement: TMarkDownTextNode) : boolean;

begin
  Result:=aElement.kind<>nkImg;
end;

procedure TFPDocMarkDownTextRenderer.StartText;
begin
  FText:='';
end;

procedure TFPDocMarkDownTextRenderer.EndText;
begin
  FPDoc.AppendText(FText);
  FText:='';
end;

const
  StyleNames : Array[TNodeStyle] of string = ('b','i','u');

procedure TFPDocMarkDownTextRenderer.PushStyle(aStyle: TNodeStyle);

begin
  FPDoc.Push(styleNames[aStyle]);
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

function TFPDocMarkDownTextRenderer.PopStyles(aStyle: TNodeStyles): TNodeStyle;

begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1] in aStyle) then
    begin
    Result:=FStyleStack[FStyleStackLen-1];
    FPDoc.Pop;
    Dec(FStyleStackLen);
    end;
end;

procedure TFPDocMarkDownTextRenderer.PopStyle(aStyle: TNodeStyle);
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1]=aStyle) then
    begin
    FPDoc.Pop;
    Dec(FStyleStackLen);
    end;
end;

function TFPDocMarkDownTextRenderer.GetNodeTag(aElement: TMarkDownTextNode) : string;
begin
  case aElement.Kind of
    nkCode: Result:='code';
    nkImg : Result:='img';
    nkURI,nkEmail : Result:='link'
  end;
end;

function TFPDocMarkDownTextRenderer.GetFPDocRenderer: TMarkDownFPDocRenderer;
begin
  if Renderer is TMarkDownFPDocRenderer then
    Result:=TMarkDownFPDocRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure TFPDocMarkDownTextRenderer.DoKey(aItem: AnsiString; const aKey: Ansistring; var aContinue: Boolean);
begin
  aContinue:=True;
  FKeys[FKeyCount]:=aKey;
  inc(FKeyCount);
end;

procedure TFPDocMarkDownTextRenderer.EmitStyleDiff(aStyles : TNodeStyles);

var
  lRemove : TNodeStyles;
  lAdd : TNodeStyles;
  S : TNodeStyle;

begin
  lRemove:=[];
  lAdd:=[];
  For S in TNodeStyle do
    begin
    if (S in FLastStyles) and Not (S in aStyles) then
      Include(lRemove,S);
    if (S in aStyles) and Not (S in FLastStyles) then
      Include(lAdd,S);
    end;
  While lRemove<>[] do
    begin
    S:=PopStyles(lRemove);
    Exclude(lRemove,S);
    end;
  For S in TNodeStyle do
    if S in lAdd then
      PushStyle(S);
  FLastStyles:=aStyles;
end;

procedure TFPDocMarkDownTextRenderer.DoRender(aElement: TMarkDownTextNode);

begin
  EmitStyleDiff(aElement.Styles);
  if aElement.Kind<>nkText then
    begin
    FPDoc.Push(GetNodeTag(aElement));
    RenderAttrs(aElement);
    end;
  StartText;
  if aElement.NodeText<>'' then
    Append(aElement.NodeText);
  EndText;
  if aElement.Kind<>nkText then
    FPDoc.Pop;
  aElement.Active:=False;
end;

procedure TFPDocMarkDownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  FStyleStackLen:=0;
  FLastStyles:=[];
end;

procedure TFPDocMarkDownTextRenderer.EndBlock;
begin
  While (FStyleStackLen>0) do
    Popstyle(FStyleStack[FStyleStackLen-1]);
  FLastStyles:=[];
  inherited EndBlock;
end;

function TFPDocMarkDownTextRenderer.renderAttrs(aElement: TMarkDownTextNode): AnsiString;

  function KeyAlias(const aKey : string): string;

  begin
    case aKey of
      'src' : Result:='file';
      'href' : Result:='id';
      'alt' : Result:='title';
    else
      Result:='';
    end
  end;

  procedure addKey(const aKey,aValue : String);
  var
    lKey : String;
  begin
    lKey:=KeyAlias(aKey);
    if lKey<>'' then
      FPDoc.Parent[lKey]:=aValue;
  end;

var
  lKey,lAttr : String;
  lAttrs : THashTable;
  lKeys : Array of string;
begin
  result := '';
  if not Assigned(aElement.Attrs) then
    exit;
  lAttrs:=aElement.Attrs;
  // First the known keys
  lKeys:=['src','alt','href','title'];
  for lKey in lKeys do
    if lAttrs.TryGet(lKey,lAttr) then
      AddKey(lKey,lAttr);
  // Then the other keys
  SetLength(FKeys,lAttrs.Count);
  FKeyCount:=0;
  lAttrs.Iterate(@DoKey);
  for lKey in FKeys do
    if IndexStr(lKey,['src','alt','href','title'])=-1 then
      AddKey(lKey,lAttrs[lKey]);
end;

procedure TFPDocParagraphBlockRenderer.DoRender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownParagraphBlock absolute aElement;
  et : TElementType;
  st : TSectionType;
  lText : string;
begin
  if lNode.header=0 then
    begin
    if not FPDoc.SkipParagraph then
      FPDoc.Push('p');
    end
  else
    begin
    lText:=Trim(lNode.GetFirstText);
    Case lNode.header of
    1:
      begin
      fpDoc.PopTill('package');
      CheckIsValidName(lText);
      FPDoc.Push(ElementNodeNames[etModule],lText);
      end;
    2:
      begin
      et:=GetElementType(lText);
      CheckIsValidName(lText);
      fpDoc.PopTill('module');
      FPDoc.Push(ElementNodeNames[et]);
      end;
    3:
      begin
      st:=GetSectionType(lText);
      FPDoc.PushSection(st);
      end;
    end;
    end;
  Renderer.RenderChildren(lNode);
end;

class function TFPDocParagraphBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownParagraphBlock;
end;

class function TFPDocMarkDownTextBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTextBlock;
end;

procedure TFPDocMarkDownTextBlockRenderer.DoRender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownTextBlock absolute aElement;
begin
  if assigned(lNode) and assigned(lNode.Nodes) then
    Renderer.RenderTextNodes(lNode.Nodes);
end;

procedure TFPDocMarkDownQuoteBlockRenderer.dorender(aElement: TMarkDownBlock);
var
  lNode : TMarkdownQuoteBlock absolute aElement;

begin
  CheckParent('descr','remark');
  fpDoc.Push('remark');
  Renderer.RenderChildren(lNode);
  fpDoc.Pop;
end;

class function TFPDocMarkDownQuoteBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownQuoteBlock;
end;

procedure TFPDocMarkDownListBlockRenderer.Dorender(aElement : TMarkDownBlock);

var
  lNode : TMarkDownListBlock absolute aElement;
  lNodeKind : String;
begin
  if not lNode.Ordered then
    lNodeKind:='ul'
  else
    lNodeKind:='ol';
  FPDoc.Push(lNodeKind);
  Renderer.RenderChildren(lNode);
  FPDoc.Pop;
end;

class function TFPDocMarkDownListBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListBlock;
end;


procedure TFPDocMarkDownListItemBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lItemBlock : TMarkDownListItemBlock absolute aElement;
  lBlock : TMarkDownBlock;
  lPar : TMarkDownParagraphBlock absolute lBlock;

  function IsPlainBlock(aBlock : TMarkDownBlock) : boolean;
  begin
    Result:=(aBlock is TMarkDownParagraphBlock)
             and (aBlock as TMarkDownParagraphBlock).isPlainPara
             and not (lItemblock.parent as TMarkDownListBlock).loose
  end;


begin
  fpDoc.Push('li');
  For lBlock in lItemBlock.Blocks do
    if IsPlainBlock(lBlock) then
      FPDoc.RenderChildren(lPar,True)
    else
      Renderer.RenderBlock(lBlock);
  fpDoc.Pop;
end;

class function TFPDocMarkDownListItemBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListItemBlock;
end;

procedure TFPDocMarkDownCodeBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lNode : TMarkDownCodeBlock absolute aElement;
  lBlock : TMarkDownBlock;

begin
  FPDoc.Push('code');
  for lBlock in LNode.Blocks do
    begin
    Renderer.RenderBlock(LBlock);
    AppendNl;
    end;
  FPDoc.Pop;
end;

class function TFPDocMarkDownCodeBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownCodeBlock;
end;

procedure TFPDocMarkDownThematicBreakBlockRenderer.Dorender(aElement : TMarkDownBlock);

begin
  if Not Assigned(aElement) then;
end;

class function TFPDocMarkDownThematicBreakBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownThematicBreakBlock;
end;

{ TMarkDownTableBlock }

procedure TFPDocMarkDownTableBlockRenderer.Dorender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownTableBlock absolute aElement;
  i : integer;
begin
  fpdoc.Push('table');
  Renderer.RenderBlock(lNode.blocks[0]);
  if lNode.blocks.Count > 1 then
  begin
    for i := 1 to lNode.blocks.Count -1  do
      Renderer.RenderBlock(lnode.blocks[i]);
  end;
  fpDoc.Pop;
end;

class function TFPDocMarkDownTableBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableBlock;
end;

{ TFPDocMarkDownDocumentRenderer }

procedure TFPDocMarkDownDocumentRenderer.Dorender(aElement: TMarkDownBlock);

begin
  Renderer.RenderChildren(aElement as TMarkDownDocument);
end;

class function TFPDocMarkDownDocumentRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownDocument
end;

{ TMarkDownTableRowBlock }

procedure TFPDocMarkDownTableRowBlockRenderer.Dorender(aElement : TMarkDownBlock);
const
  CellTypes : Array[Boolean] of string = ('td','th'); //
var
  lNode : TMarkDownTableRowBlock absolute aElement;
  first : boolean;
  i : integer;
  cType : String;
begin
  first:=(lNode.parent as TMarkDownContainerBlock).blocks.First = self;
  cType:=Celltypes[First];
  fpDoc.Push('tr');
  for i := 0 to length((lNode.parent as TMarkDownTableBlock).Columns) - 1 do
    begin
    fpDoc.Push(cType);
    if i < lNode.blocks.Count then
      Renderer.RenderBlock(lNode.blocks[i]);
    fpDoc.Pop;
    end;
  fpDoc.Pop;
end;

class function TFPDocMarkDownTableRowBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableRowBlock;
end;

procedure TFPDocMarkDownHeadingBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lNode : TMarkDownHeadingBlock absolute aElement;
  lText : String;
  et : TElementType;
  st : TSectionType;
begin
  lText:=Trim(lNode.GetFirstText);
  Case lNode.Level of
  1:
    begin
    fpDoc.PopTill('package');
    CheckIsValidName(lText);
    FPDoc.Push(ElementNodeNames[etModule],lText);
    end;
  2:
    begin
    et:=GetElementType(lText);
    CheckIsValidName(lText);
    fpDoc.PopTill('module');
    FPDoc.Push(ElementNodeNames[et],lText);
    end;
  3:
    begin
    fpDoc.PopTill('element');
    st:=GetSectionType(lText);
    FPDoc.PushSection(st);
    end;
  end;
  // Renderer.RenderChildren(lNode);
end;

class function TFPDocMarkDownHeadingBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownHeadingBlock;
end;

initialization
  TFPDocMarkDownHeadingBlockRenderer.RegisterRenderer(TMarkDownFPDocRenderer);
  TFPDocParagraphBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownQuoteBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownTextBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownListBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownListItemBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownCodeBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownHeadingBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownThematicBreakBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownTableBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownTableRowBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownDocumentRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkDownTextRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
end.


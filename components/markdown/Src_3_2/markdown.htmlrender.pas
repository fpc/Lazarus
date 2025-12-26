{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown HTML renderer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.HtmlRender;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.StrUtils, System.Contnrs, 
{$ELSE}
  Classes, SysUtils, strutils, contnrs, 
{$ENDIF}  
  MarkDown.Elements, 
  MarkDown.Render, 
  MarkDown.Utils;

type
  { TMarkDownHTMLRenderer }
  THTMLOption = (hoEnvelope,hoHead);
  THTMLOptions = set of THTMLOption;

  TMarkDownHTMLRenderer = class(TMarkDownRenderer)
  private
    FBuilder: TStringBuilder;
    FHead: TStrings;
    FHTML: String;
    FOptions: THTMLOptions;
    FTitle: String;
    procedure SetHead(const aValue: TStrings);
  Protected
    Procedure Append(const aContent : String);
    Procedure AppendNL(const aContent : String = '');
    Property Builder : TStringBuilder Read FBuilder;
  public
    constructor Create(aOwner : TComponent); override;
    destructor destroy; override;
    Procedure RenderDocument(aDocument : TMarkDownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkDownDocument; aDest : TStrings); overload;
    procedure RenderChildren(aBlock : TMarkDownContainerBlock; aAppendNewLine : Boolean); overload;
    function RenderHTML(aDocument : TMarkDownDocument) : string;
    procedure RenderHTMLToFile(aDocument : TMarkDownDocument; const aFileName : string);
    class function FastRender(aDocument : TMarkDownDocument; aOptions : THTMLOptions; aTitle : String = ''; aHead : TStrings = Nil) : String;
    class procedure FastRenderToFile(aDocument : TMarkDownDocument; const aFileName : string; aOptions : THTMLOptions; aTitle : String = ''; aHead : TStrings = Nil);
    Property HTML : String Read FHTML;
  published
    Property Options : THTMLOptions Read FOptions Write FOptions;
    property Title : String Read FTitle Write FTitle;
    property Head : TStrings Read FHead Write SetHead;
  end;

  { THTMLMarkDownBlockRenderer }

  THTMLMarkDownBlockRenderer = Class (TMarkDownBlockRenderer)
  Private
    function GetHTMLRenderer: TMarkDownHTMLRenderer;
  protected
    procedure Append(const S : String); inline;
    procedure AppendNl(const S : String = ''); inline;
    function HasOption(aOption : THTMLOption) : Boolean;
  public
    property HTMLRenderer : TMarkDownHTMLRenderer Read GetHTMLRenderer;
  end;
  THTMLMarkDownBlockRendererClass = class of THTMLMarkDownBlockRenderer;
  { THTMLMarkDownTextRenderer }

  THTMLMarkDownTextRenderer = class(TMarkDownTextRenderer)
  Private
    FStyleStack: Array of TNodeStyle;
    FStyleStackLen : Integer;
    FLastStyles : TNodeStyles;
    FKeys : Array of String;
    FKeyCount : integer;
    procedure DoKey(aItem: AnsiString; const aKey: AnsiString; var aContinue: Boolean);
    procedure EmitStyleDiff(aStyles: TNodeStyles);
    function GetHTMLRenderer: TMarkDownHTMLRenderer;
    function GetNodeTag(aElement: TMarkDownTextNode): string;
    function MustCloseNode(aElement: TMarkDownTextNode): boolean;
  protected
    procedure PushStyle(aStyle : TNodeStyle);
    function PopStyles(aStyle: TNodeStyles): TNodeStyle;
    procedure PopStyle(aStyle : TNodeStyle);
    procedure Append(const S : String); inline;
    procedure DoRender(aElement: TMarkDownTextNode); override;
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    property HTMLRenderer : TMarkDownHTMLRenderer Read GetHTMLRenderer;
    function renderAttrs(aElement: TMarkDownTextNode): AnsiString;
  end;
  THTMLMarkDownTextRendererClass = class of THTMLMarkDownTextRenderer;

  { THTMLParagraphBlockRenderer }

  THTMLParagraphBlockRenderer = class (THTMLMarkDownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownQuoteBlockRenderer }

  THTMLMarkDownQuoteBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownTextBlockRenderer }

  THTMLMarkDownTextBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownListBlockRenderer }

  THTMLMarkDownListBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownListItemBlockRenderer }

  THTMLMarkDownListItemBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownCodeBlockRenderer }

  THTMLMarkDownCodeBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownHeadingBlockRenderer }

  THTMLMarkDownHeadingBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownThematicBreakBlockRenderer }

  THTMLMarkDownThematicBreakBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownTableBlockRenderer }

  THTMLMarkDownTableBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TMarkDownTableRowBlockRenderer }

  THTMLMarkDownTableRowBlockRenderer = class(THTMLMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { THTMLMarkDownDocumentRenderer }

  THTMLMarkDownDocumentRenderer = class(THTMLMarkDownBlockRenderer)
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

function THTMLMarkDownBlockRenderer.GetHTMLRenderer: TMarkDownHTMLRenderer;
begin
  if Renderer is TMarkDownHTMLRenderer then
    Result:=TMarkDownHTMLRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure THTMLMarkDownBlockRenderer.Append(const S: String);
begin
  HTMLRenderer.Append(S);
end;

procedure THTMLMarkDownBlockRenderer.AppendNl(const S: String);
begin
  HTMLRenderer.AppendNL(S);
end;

function THTMLMarkDownBlockRenderer.HasOption(aOption: THTMLOption): Boolean;
begin
  Result:=(Self.Renderer is TMarkDownHTMLRenderer);
  if Result then
    Result:=aOption in TMarkDownHTMLRenderer(Renderer).Options;
end;


{ TMarkDownHTMLRenderer }

procedure TMarkDownHTMLRenderer.SetHead(const aValue: TStrings);
begin
  if FHead=aValue then Exit;
  FHead:=aValue;
end;

procedure TMarkDownHTMLRenderer.Append(const aContent: String);
begin
  FBuilder.Append(aContent);
end;

procedure TMarkDownHTMLRenderer.AppendNL(const aContent: String);
begin
  if aContent<>'' then
    FBuilder.Append(aContent);
  FBuilder.Append(sLineBreak);
end;

constructor TMarkDownHTMLRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHead:=TStringList.Create;
end;

destructor TMarkDownHTMLRenderer.destroy;
begin
  FreeAndNil(FHead);
  inherited destroy;
end;

procedure TMarkDownHTMLRenderer.RenderDocument(aDocument: TMarkDownDocument);
begin
  FBuilder:=TStringBuilder.Create;
  try
    RenderBlock(aDocument);
    FHTML:=FBuilder.ToString;
  finally
    FreeAndNil(FBuilder);
  end;
end;

procedure TMarkDownHTMLRenderer.RenderDocument(aDocument: TMarkDownDocument; aDest: TStrings);
begin
  aDest.Text:=RenderHTML(aDocument);
end;

procedure TMarkDownHTMLRenderer.RenderChildren(aBlock: TMarkDownContainerBlock; aAppendNewLine: Boolean);
var
  i,iMax : integer;
begin
  iMax:=aBlock.Blocks.Count-1;
  for I:=0 to iMax do
    begin
    if aAppendNewLine and (I>0) then
      AppendNl();
    RenderBlock(aBlock.Blocks[I]);
    end;
end;

function TMarkDownHTMLRenderer.RenderHTML(aDocument: TMarkDownDocument): string;
begin
  RenderDocument(aDocument);
  Result:=FHTML;
  FHTML:='';
end;

procedure TMarkDownHTMLRenderer.RenderHTMLToFile(aDocument: TMarkDownDocument; const aFileName: string);
var
  lHTML : String;
  lFile : THandle;
begin
  lHTML:=RenderHTML(aDocument);
  lFile:=FileCreate(aFileName);
  try
    if lHTML<>'' then
      FileWrite(lFile,lHTML[1],Length(lHTML)*SizeOf(Char));
  finally
    FileClose(lFile);
  end;
end;

class function TMarkDownHTMLRenderer.FastRender(aDocument: TMarkDownDocument; aOptions: THTMLOptions; aTitle: String;
  aHead: TStrings): String;
var
  lRender : TMarkDownHTMLRenderer;
begin
  lRender:=TMarkDownHTMLRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    if assigned(aHead) then
      lRender.Head.Assign(aHead);
    Result:=lRender.RenderHTML(aDocument);
  finally
    lRender.Free;
  end;
end;

class procedure TMarkDownHTMLRenderer.FastRenderToFile(aDocument: TMarkDownDocument; const aFileName: string;
  aOptions: THTMLOptions; aTitle: String; aHead: TStrings);
var
  lRender : TMarkDownHTMLRenderer;
begin
  lRender:=TMarkDownHTMLRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    if assigned(aHead) then
      lRender.Head.Assign(aHead);
    lRender.RenderHTMLToFile(aDocument,aFileName);
  finally
    lRender.Free;
  end;
end;


procedure THTMLMarkDownTextRenderer.Append(const S: String);
begin
  HTMLRenderer.Append(S);
end;

function THTMLMarkDownTextRenderer.MustCloseNode(aElement: TMarkDownTextNode) : boolean;

begin
  Result:=aElement.kind<>nkImg;
end;

const
  StyleNames : Array[TNodeStyle] of string = ('b','i','del');

procedure THTMLMarkDownTextRenderer.PushStyle(aStyle: TNodeStyle);

begin
  HTMLRenderer.Append('<'+styleNames[aStyle]+'>');
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

function THTMLMarkDownTextRenderer.Popstyles(aStyle: TNodeStyles) : TNodeStyle;

begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1] in aStyle) then
    begin
    Result:=FStyleStack[FStyleStackLen-1];
    HTMLRenderer.Append('</'+StyleNames[Result]+'>');
    Dec(FStyleStackLen);
    end;
end;

procedure THTMLMarkDownTextRenderer.PopStyle(aStyle: TNodeStyle);
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1]=aStyle) then
    begin
    HTMLRenderer.Append('</'+styleNames[aStyle]+'>');
    Dec(FStyleStackLen);
    end;
end;

function THTMLMarkDownTextRenderer.GetNodeTag(aElement: TMarkDownTextNode) : string;
begin
  case aElement.Kind of
    nkCode: Result:='code';
    nkImg : Result:='img';
    nkURI,nkEmail : Result:='a'
  end;
end;

function THTMLMarkDownTextRenderer.GetHTMLRenderer: TMarkDownHTMLRenderer;
begin
  if Renderer is TMarkDownHTMLRenderer then
    Result:=TMarkDownHTMLRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure THTMLMarkDownTextRenderer.DoKey(aItem: AnsiString; const aKey: Ansistring; var aContinue: Boolean);
begin
  aContinue:=True;
  FKeys[FKeyCount]:=aKey;
  inc(FKeyCount);
end;

procedure THTMLMarkDownTextRenderer.EmitStyleDiff(aStyles : TNodeStyles);

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

procedure THTMLMarkDownTextRenderer.DoRender(aElement: TMarkDownTextNode);
var
  lName : string;
begin
  EmitStyleDiff(aElement.Styles);
  if aElement.Kind<>nkText then
    begin
    lName:=GetNodeTag(aElement);
    Append('<');
    Append(lName);
    Append(renderAttrs(aElement));
    Append('>');
    end;
  if aElement.NodeText<>'' then
    Append(aElement.NodeText);
  if (lName<>'') and MustCloseNode(aElement) then
    begin
    Append('</');
    Append(lName);
    Append('>');
    end;
  aElement.Active:=False;
end;

procedure THTMLMarkDownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  FStyleStackLen:=0;
  FLastStyles:=[];
end;

procedure THTMLMarkDownTextRenderer.EndBlock;
begin
  While (FStyleStackLen>0) do
    Popstyle(FStyleStack[FStyleStackLen-1]);
  FLastStyles:=[];
  inherited EndBlock;
end;

function THTMLMarkDownTextRenderer.renderAttrs(aElement: TMarkDownTextNode): AnsiString;

  procedure addKey(aKey,aValue : String);
  begin
    Result:=Result+' '+aKey+'="'+aValue+'"';
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

procedure THTMLParagraphBlockRenderer.DoRender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownParagraphBlock absolute aElement;
  c : TMarkDownBlock;
  first : boolean;
begin
  if lNode.header=0 then
    Append('<p>')
  else
    Append('<h'+IntToStr(lNode.Header)+'>');
  first := true;
  for c in lNode.Blocks do
    begin
    if first then
      first := false
    else
      AppendNl;
   Renderer.RenderChildren(lNode);
    end;
  if lNode.header=0 then
    Append('</p>')
  else
    Append('</h'+IntToStr(lNode.Header)+'>');
  AppendNl;
end;

class function THTMLParagraphBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownParagraphBlock;
end;

class function THTMLMarkDownTextBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTextBlock;
end;

procedure THTMLMarkDownTextBlockRenderer.DoRender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownTextBlock absolute aElement;
begin
  if assigned(lNode) and assigned(lNode.Nodes) then
    Renderer.RenderTextNodes(lNode.Nodes);
end;

procedure THTMLMarkDownQuoteBlockRenderer.dorender(aElement: TMarkDownBlock);
var
  lNode : TMarkdownQuoteBlock absolute aElement;

begin
  AppendNl('<blockquote>');
  Renderer.RenderChildren(lNode);
  AppendNl('</blockquote>');
end;

class function THTMLMarkDownQuoteBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownQuoteBlock;
end;

procedure THTMLMarkDownListBlockRenderer.Dorender(aElement : TMarkDownBlock);

var
  lNode : TMarkDownListBlock absolute aElement;

begin
  if not lNode.Ordered then
    AppendNl('<ul>')
  else if lNode.Start=1 then
    AppendNL('<ol>')
  else
    AppendNl('<ol start="'+IntToStr(lNode.Start)+'">');
  Renderer.RenderChildren(lNode);
  if lNode.Ordered then
    AppendNl('</ol>')
  else
    AppendNl('</ul>');
end;

class function THTMLMarkDownListBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListBlock;
end;


procedure THTMLMarkDownListItemBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lItemBlock : TMarkDownListItemBlock absolute aElement;
  lBlock : TMarkDownBlock;
  lPar : TMarkDownParagraphBlock absolute lBlock;
  lCount : Integer;

  function IsPlainBlock(aBlock : TMarkDownBlock) : boolean;
  begin
    Result:=(aBlock is TMarkDownParagraphBlock)
             and (aBlock as TMarkDownParagraphBlock).isPlainPara
             and not (lItemblock.parent as TMarkDownListBlock).loose
  end;


begin
  Append('<li>');
  lCount:=0;
  For lBlock in lItemBlock.Blocks do
    if IsPlainBlock(lBlock) then
      HTMLRenderer.RenderChildren(lPar,True)
    else
      begin
      if lCount=0 then
        AppendNl;
      Inc(lCount);
      Renderer.RenderBlock(lBlock);
      end;
  AppendNl('</li>');
end;

class function THTMLMarkDownListItemBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListItemBlock;
end;

procedure THTMLMarkDownCodeBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lNode : TMarkDownCodeBlock absolute aElement;
  lBlock : TMarkDownBlock;
  lLang : string;
begin
  lLang:=lNode.Lang;
  if lLang<> '' then
    Append('<pre><code class="language-'+lLang+'">')
  else
    Append('<pre><code>');
  for lBlock in LNode.Blocks do
    begin
    Renderer.RenderCodeBlock(LBlock,lLang);
    AppendNl;
    end;
  Append('</code></pre>');
  AppendNl;
end;

class function THTMLMarkDownCodeBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownCodeBlock;
end;

procedure THTMLMarkDownThematicBreakBlockRenderer.Dorender(aElement : TMarkDownBlock);

begin
  if Not Assigned(aElement) then
    exit;
  Append('<hr />');
  AppendNl;
end;

class function THTMLMarkDownThematicBreakBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownThematicBreakBlock;
end;

{ TMarkDownTableBlock }

procedure THTMLMarkDownTableBlockRenderer.Dorender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownTableBlock absolute aElement;
  i : integer;
begin
  AppendNl('<table>');
  AppendNl('<thead>');
  Renderer.RenderBlock(lNode.blocks[0]);
  AppendNl('</thead>');
  if lNode.blocks.Count > 1 then
  begin
    AppendNl('<tbody>');
    for i := 1 to lNode.blocks.Count -1  do
      Renderer.RenderBlock(lnode.blocks[i]);
    AppendNl('</tbody>');
  end;
  AppendNl('</table>');
end;

class function THTMLMarkDownTableBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableBlock;
end;

{ THTMLMarkDownDocumentRenderer }

procedure THTMLMarkDownDocumentRenderer.Dorender(aElement: TMarkDownBlock);
var
  H : String;
begin
  if HasOption(hoEnvelope) then
    begin
    AppendNL('<!DOCTYPE html>');
    AppendNL('<html>');
    if HasOption(hoHead) then
      begin
      AppendNL('<head>');
      if HTMLRenderer.Title<>'' then
        begin
        Append('<title>');
        Append(HTMLRenderer.Title);
        AppendNL('</title>');
        end;
      for H in HTMLRenderer.Head do
        AppendNL(H);
      AppendNL('</head>');
      end;
    AppendNL('<body>');
    end;
  Renderer.RenderChildren(aElement as TMarkDownDocument);
  if HasOption(hoEnvelope) then
    begin
    AppendNL('</body>');
    AppendNL('</html>');
    end;
end;

class function THTMLMarkDownDocumentRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownDocument
end;

{ TMarkDownTableRowBlock }

procedure THTMLMarkDownTableRowBlockRenderer.Dorender(aElement : TMarkDownBlock);
const
  CellTypes : Array[Boolean] of string = ('td','th'); //
var
  lNode : TMarkDownTableRowBlock absolute aElement;
  lFirst : boolean;
  i,lCount : integer;
  lType,lAttr : String;
  lAlign: TCellAlign;
begin
  lFirst:=(lNode.parent as TMarkDownContainerBlock).blocks.First = self;
  lCount:=length((lNode.parent as TMarkDownTableBlock).Columns);
  lType:=CellTypes[lFirst];
  AppendNl('<tr>');
  for i:=0 to lCount-1 do
    begin
    lAlign:=(lNode.parent as TMarkDownTableBlock).Columns[i];
    case lAlign of
      caLeft   : lAttr:='';
      caCenter : lAttr:=' align="center"';
      caRight  : lAttr:=' align="right"';
    end;
    Append('<'+lType+lAttr+'>');
    if i<lNode.blocks.Count then
      Renderer.RenderBlock(lNode.blocks[i]);
    AppendNl('</'+lType+'>');
    end;
  AppendNl('</tr>');
end;

class function THTMLMarkDownTableRowBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableRowBlock;
end;

procedure THTMLMarkDownHeadingBlockRenderer.Dorender(aElement : TMarkDownBlock);

var
  lNode : TMarkDownHeadingBlock absolute aElement;
begin
  Append('<h'+inttostr(Lnode.Level)+'>');
  Renderer.RenderChildren(lNode);
  Append('</h'+inttostr(lNode.Level)+'>');
  AppendNl;
end;

class function THTMLMarkDownHeadingBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownHeadingBlock;
end;

initialization
  THTMLMarkDownHeadingBlockRenderer.RegisterRenderer(TMarkDownHTMLRenderer);
  THTMLParagraphBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownQuoteBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownTextBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownListBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownListItemBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownCodeBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownHeadingBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownThematicBreakBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownTableBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownTableRowBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownDocumentRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkDownTextRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
end.


{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown LaTeX renderer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 *********************************************************************}

unit MarkDown.LatexRender;

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
  { TMarkDownLaTeXRenderer }
  TLaTeXOption = (loEnvelope, loNumberedSections);
  TLaTeXOptions = set of TLaTeXOption;

  TMarkDownLaTeXRenderer = class(TMarkDownRenderer)
  private
    FBuilder: TStringBuilder;
    FHead: TStrings;
    FLaTeX: String;
    FOptions: TLaTeXOptions;
    FTitle: String;
    FAuthor: String;
    procedure SetHead(const aValue: TStrings);
  Protected
    Procedure Append(const aContent : String);
    Procedure AppendNL(const aContent : String = '');
    Property Builder : TStringBuilder Read FBuilder;
    function EscapeLaTeX(const S: String): String;
  public
    constructor Create(aOwner : TComponent); override;
    destructor destroy; override;
    Procedure RenderDocument(aDocument : TMarkDownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkDownDocument; aDest : TStrings); overload;
    procedure RenderChildren(aBlock : TMarkDownContainerBlock; aAppendNewLine : Boolean); overload;
    function RenderLaTeX(aDocument : TMarkDownDocument) : string;
    Procedure RenderToFile(aDocument : TMarkDownDocument; aFileName : string);
    class procedure FastRenderToFile(aDocument : TMarkDownDocument; const aFileName : string; aOptions : TLaTeXOptions = []; const aTitle : String = ''; const aAuthor : string = '');
    class function FastRender(aDocument : TMarkDownDocument; aOptions : TLaTeXOptions = []; const aTitle : String = ''; const aAuthor : string = '') : string;
  published
    Property Options : TLaTeXOptions Read FOptions Write FOptions;
    property Title : String Read FTitle Write FTitle;
    property Author : String Read FAuthor Write FAuthor;
    property Head : TStrings Read FHead Write SetHead;
  end;

  { TLaTeXMarkDownBlockRenderer }

  TLaTeXMarkDownBlockRenderer = Class (TMarkDownBlockRenderer)
  Private
    function GetLaTeXRenderer: TMarkDownLaTeXRenderer;
  protected
    procedure Append(const S : String); inline;
    procedure AppendNl(const S : String = ''); inline;
    function HasOption(aOption : TLaTeXOption) : Boolean;
    function Escape(const S: String): String;
  public
    property LaTeXRenderer : TMarkDownLaTeXRenderer Read GetLaTeXRenderer;
  end;
  TLaTeXMarkDownBlockRendererClass = class of TLaTeXMarkDownBlockRenderer;

  { TLaTeXMarkDownTextRenderer }

  TLaTeXMarkDownTextRenderer = class(TMarkDownTextRenderer)
  Private
    FStyleStack: Array of TNodeStyle;
    FStyleStackLen : Integer;
    FLastStyles : TNodeStyles;
    function GetLaTeXRenderer: TMarkDownLaTeXRenderer;
    function GetNodeTag(aElement: TMarkDownTextNode; Closing: Boolean): string;
  protected
    procedure PushStyle(aStyle : TNodeStyle);
    function PopStyles(aStyle: TNodeStyles): TNodeStyle;
    procedure PopStyle(aStyle : TNodeStyle);
    procedure Append(const S : String); inline;
    procedure DoRender(aElement: TMarkDownTextNode); override;
    function Escape(const S: String): String;
    procedure EmitStyleDiff(aStyles : TNodeStyles);
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    property LaTeXRenderer : TMarkDownLaTeXRenderer Read GetLaTeXRenderer;
  end;
  TLaTeXMarkDownTextRendererClass = class of TLaTeXMarkDownTextRenderer;

  { TLaTeXParagraphBlockRenderer }

  TLaTeXParagraphBlockRenderer = class (TLaTeXMarkDownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownQuoteBlockRenderer }

  TLaTeXMarkDownQuoteBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownTextBlockRenderer }

  TLaTeXMarkDownTextBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownListBlockRenderer }

  TLaTeXMarkDownListBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownListItemBlockRenderer }

  TLaTeXMarkDownListItemBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownCodeBlockRenderer }

  TLaTeXMarkDownCodeBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownHeadingBlockRenderer }

  TLaTeXMarkDownHeadingBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownThematicBreakBlockRenderer }

  TLaTeXMarkDownThematicBreakBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownTableBlockRenderer }

  TLaTeXMarkDownTableBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownTableRowBlockRenderer }

  TLaTeXMarkDownTableRowBlockRenderer = class(TLaTeXMarkDownBlockRenderer)
  protected
    procedure Dorender(aElement : TMarkDownBlock); override;
  public
    class function BlockClass : TMarkDownBlockClass; override;
  end;

  { TLaTeXMarkDownDocumentRenderer }

  TLaTeXMarkDownDocumentRenderer = class(TLaTeXMarkDownBlockRenderer)
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

{ TLaTeXMarkDownBlockRenderer }

function TLaTeXMarkDownBlockRenderer.GetLaTeXRenderer: TMarkDownLaTeXRenderer;
begin
  if Renderer is TMarkDownLaTeXRenderer then
    Result:=TMarkDownLaTeXRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure TLaTeXMarkDownBlockRenderer.Append(const S: String);
begin
  LaTeXRenderer.Append(S);
end;

procedure TLaTeXMarkDownBlockRenderer.AppendNl(const S: String);
begin
  LaTeXRenderer.AppendNL(S);
end;

function TLaTeXMarkDownBlockRenderer.HasOption(aOption: TLaTeXOption): Boolean;
begin
  Result:=(Self.Renderer is TMarkDownLaTeXRenderer);
  if Result then
    Result:=aOption in TMarkDownLaTeXRenderer(Renderer).Options;
end;

function TLaTeXMarkDownBlockRenderer.Escape(const S: String): String;
begin
  Result:=LaTeXRenderer.EscapeLaTeX(S);
end;

{ TMarkDownLaTeXRenderer }

procedure TMarkDownLaTeXRenderer.SetHead(const aValue: TStrings);
begin
  if FHead=aValue then Exit;
  FHead:=aValue;
end;

procedure TMarkDownLaTeXRenderer.Append(const aContent: String);
begin
  FBuilder.Append(aContent);
end;

procedure TMarkDownLaTeXRenderer.AppendNL(const aContent: String);
begin
  if aContent<>'' then
    FBuilder.Append(aContent);
  FBuilder.Append(sLineBreak);
end;

constructor TMarkDownLaTeXRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHead:=TStringList.Create;
end;

destructor TMarkDownLaTeXRenderer.destroy;
begin
  FreeAndNil(FHead);
  inherited destroy;
end;

procedure TMarkDownLaTeXRenderer.RenderDocument(aDocument: TMarkDownDocument);
begin
  FBuilder:=TStringBuilder.Create;
  try
    RenderBlock(aDocument);
    FLaTeX:=FBuilder.ToString;
  finally
    FreeAndNil(FBuilder);
  end;
end;

procedure TMarkDownLaTeXRenderer.RenderDocument(aDocument: TMarkDownDocument; aDest: TStrings);
begin
  aDest.Text:=RenderLaTeX(aDocument);
end;

procedure TMarkDownLaTeXRenderer.RenderChildren(aBlock: TMarkDownContainerBlock; aAppendNewLine: Boolean);
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

function TMarkDownLaTeXRenderer.RenderLaTeX(aDocument: TMarkDownDocument): string;
begin
  RenderDocument(aDocument);
  Result:=FLaTeX;
  FLaTeX:='';
end;

procedure TMarkDownLaTeXRenderer.RenderToFile(aDocument: TMarkDownDocument; aFileName: string);
var
  lTeX : String;
  lFile : THandle;
begin
  lTeX:=RenderLaTex(aDocument);
  lFile:=FileCreate(aFileName);
  try
    if lTex<>'' then
      FileWrite(lFile,lTex[1],Length(lTex)*SizeOf(Char));
  finally
    FileClose(lFile);
  end;
end;

class procedure TMarkDownLaTeXRenderer.FastRenderToFile(aDocument: TMarkDownDocument; const aFileName: string; aOptions: TLaTeXOptions;
  const aTitle: String; const aAuthor: string);
var
  lRender : TMarkDownLaTexRenderer;
begin
  lRender:=TMarkDownLaTexRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    lRender.Author:=aAuthor;
    lRender.RenderToFile(aDocument,aFileName);
  finally
    lRender.Free;
  end;
end;

class function TMarkDownLaTeXRenderer.FastRender(aDocument: TMarkDownDocument; aOptions: TLaTeXOptions; const aTitle: String;
  const aAuthor: string): string;
var
  lRender : TMarkDownLaTexRenderer;
begin
  lRender:=TMarkDownLaTexRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    lRender.Author:=aAuthor;
    Result:=lRender.RenderLatex(aDocument);
  finally
    lRender.Free;
  end;
end;

function TMarkDownLaTeXRenderer.EscapeLaTeX(const S: String): String;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
      '\': Result := Result + '\textbackslash{}';
      '{': Result := Result + '\{';
      '}': Result := Result + '\}';
      '$': Result := Result + '\$';
      '&': Result := Result + '\&';
      '#': Result := Result + '\#';
      '^': Result := Result + '\textasciicircum{}';
      '_': Result := Result + '\_';
      '%': Result := Result + '\%';
      '~': Result := Result + '\textasciitilde{}';
    else
      Result := Result + c;
    end;
  end;
end;

{ TLaTeXMarkDownTextRenderer }

procedure TLaTeXMarkDownTextRenderer.Append(const S: String);
begin
  LaTeXRenderer.Append(S);
end;

function TLaTeXMarkDownTextRenderer.Escape(const S: String): String;
begin
  Result:=LaTeXRenderer.EscapeLaTeX(S);
end;

function TLaTeXMarkDownTextRenderer.GetLaTeXRenderer: TMarkDownLaTeXRenderer;
begin
  if Renderer is TMarkDownLaTeXRenderer then
    Result:=TMarkDownLaTeXRenderer(Renderer)
  else
    Result:=Nil;
end;

function TLaTeXMarkDownTextRenderer.GetNodeTag(aElement: TMarkDownTextNode; Closing: Boolean): string;
var
  lUrl: String;
begin
  Result := '';
  case aElement.Kind of
    nkCode:
      if Closing then Result := '}' else Result := '\texttt{';
    nkURI, nkEmail:
      begin
        lUrl := '';
        if aElement.HasAttrs then
          aElement.Attrs.TryGet('href', lUrl);
          
        if Closing then 
          Result := '}' 
        else 
          Result := '\href{' + lUrl + '}{';
      end;
    nkImg:
      begin
        lUrl := '';
        if aElement.HasAttrs then
          aElement.Attrs.TryGet('src', lUrl);

        if Closing then 
          Result := '}' 
        else 
          Result := '\includegraphics{' + lUrl + '}{';
      end;
  end;
end;

procedure TLaTeXMarkDownTextRenderer.PushStyle(aStyle: TNodeStyle);
begin
  case aStyle of
    nsStrong: Append('\textbf{');
    nsEmph: Append('\textit{');
    nsDelete: Append('\sout{'); // Requires ulem package
  end;
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

function TLaTeXMarkDownTextRenderer.Popstyles(aStyle: TNodeStyles) : TNodeStyle;
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1] in aStyle) then
    begin
    Result:=FStyleStack[FStyleStackLen-1];
    Append('}');
    Dec(FStyleStackLen);
    end;
end;

procedure TLaTeXMarkDownTextRenderer.PopStyle(aStyle: TNodeStyle);
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1]=aStyle) then
    begin
    Append('}');
    Dec(FStyleStackLen);
    end;
end;

procedure TLaTeXMarkDownTextRenderer.EmitStyleDiff(aStyles : TNodeStyles);
var
  lRemove : TNodeStyles;
  lAdd : TNodeStyles;
  S : TNodeStyle;
begin
  lRemove:=[];
  lAdd:=[];
  For S in TNodeStyle do
    begin
    if (S in Self.FLastStyles) and Not (S in aStyles) then
      Include(lRemove,S);
    if (S in aStyles) and Not (S in Self.FLastStyles) then
      Include(lAdd,S);
    end;
  While lRemove<>[] do
    begin
    S:=Self.PopStyles(lRemove);
    Exclude(lRemove,S);
    end;
  For S in TNodeStyle do
    if S in lAdd then
      Self.PushStyle(S);
  Self.FLastStyles:=aStyles;
end;

procedure TLaTeXMarkDownTextRenderer.DoRender(aElement: TMarkDownTextNode);
var
  lTag : string;
begin
  Self.EmitStyleDiff(aElement.Styles);
  if aElement.Kind <> nkText then
  begin
    lTag := Self.GetNodeTag(aElement, False);
    Append(lTag);
  end;

  if aElement.Kind = nkImg then
  begin
     // Img handling logic here...
  end;

  if aElement.NodeText<>'' then
    Append(Self.Escape(aElement.NodeText));

  if aElement.Kind <> nkText then
  begin
    lTag := Self.GetNodeTag(aElement, True);
    Append(lTag);
  end;
  
  aElement.Active:=False;
end;

procedure TLaTeXMarkDownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  Self.FStyleStackLen:=0;
  Self.FLastStyles:=[];
end;

procedure TLaTeXMarkDownTextRenderer.EndBlock;
begin
  While (Self.FStyleStackLen>0) do
    Self.Popstyle(Self.FStyleStack[Self.FStyleStackLen-1]);
  Self.FLastStyles:=[];
  inherited EndBlock;
end;

{ TLaTeXParagraphBlockRenderer }

procedure TLaTeXParagraphBlockRenderer.DoRender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownParagraphBlock absolute aElement;
begin
  // LaTeX paragraphs are separated by blank lines.
  // No special environment needed usually, unless we want to enforce spacing.
  Renderer.RenderChildren(lNode);
  AppendNl; // Blank line after paragraph
  AppendNl;
end;

class function TLaTeXParagraphBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownParagraphBlock;
end;

{ TLaTeXMarkDownTextBlockRenderer }

class function TLaTeXMarkDownTextBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTextBlock;
end;

procedure TLaTeXMarkDownTextBlockRenderer.DoRender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownTextBlock absolute aElement;
begin
  if assigned(lNode) and assigned(lNode.Nodes) then
    Renderer.RenderTextNodes(lNode.Nodes);
end;

{ TLaTeXMarkDownQuoteBlockRenderer }

procedure TLaTeXMarkDownQuoteBlockRenderer.dorender(aElement: TMarkDownBlock);
var
  lNode : TMarkdownQuoteBlock absolute aElement;
begin
  AppendNl('\begin{quote}');
  Renderer.RenderChildren(lNode);
  AppendNl('\end{quote}');
end;

class function TLaTeXMarkDownQuoteBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownQuoteBlock;
end;

{ TLaTeXMarkDownListBlockRenderer }

procedure TLaTeXMarkDownListBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lNode : TMarkDownListBlock absolute aElement;
begin
  if not lNode.Ordered then
    AppendNl('\begin{itemize}')
  else
    AppendNl('\begin{enumerate}');
    
  Renderer.RenderChildren(lNode);
  
  if lNode.Ordered then
    AppendNl('\end{enumerate}')
  else
    AppendNl('\end{itemize}');
end;

class function TLaTeXMarkDownListBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListBlock;
end;


{ TLaTeXMarkDownListItemBlockRenderer }

procedure TLaTeXMarkDownListItemBlockRenderer.Dorender(aElement : TMarkDownBlock);
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
  Append('\item ');
  For lBlock in lItemBlock.Blocks do
    if IsPlainBlock(lBlock) then
      LaTeXRenderer.RenderChildren(lPar,True)
    else
      Renderer.RenderBlock(lBlock);
  AppendNl;
end;

class function TLaTeXMarkDownListItemBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListItemBlock;
end;

{ TLaTeXMarkDownCodeBlockRenderer }

procedure TLaTeXMarkDownCodeBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lNode : TMarkDownCodeBlock absolute aElement;
  lBlock : TMarkDownBlock;
begin
  AppendNl('\begin{verbatim}');
  for lBlock in LNode.Blocks do
    begin
    Renderer.RenderCodeBlock(LBlock,lNode.Lang);
    AppendNl;
    end;
  AppendNl('\end{verbatim}');
end;

class function TLaTeXMarkDownCodeBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownCodeBlock;
end;

{ TLaTeXMarkDownThematicBreakBlockRenderer }

procedure TLaTeXMarkDownThematicBreakBlockRenderer.Dorender(aElement : TMarkDownBlock);
begin
  if Not Assigned(aElement) then
    exit;
  AppendNl('\hrule');
end;

class function TLaTeXMarkDownThematicBreakBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownThematicBreakBlock;
end;

{ TLaTeXMarkDownTableBlockRenderer }

procedure TLaTeXMarkDownTableBlockRenderer.Dorender(aElement: TMarkDownBlock);
var
  lNode : TMarkDownTableBlock absolute aElement;
  i : integer;
  lCols: String;
  c: TCellAlign;
begin
  // Construct column definition
  lCols := '';
  for c in lNode.Columns do
  begin
    case c of
      caLeft: lCols := lCols + 'l|';
      caRight: lCols := lCols + 'r|';
      caCenter: lCols := lCols + 'c|';
    end;
  end;
  if Length(lCols) > 0 then
    lCols := '|' + lCols;

  AppendNl('\begin{tabular}{' + lCols + '}');
  AppendNl('\hline');
  
  // Header
  Renderer.RenderBlock(lNode.blocks[0]);
  AppendNl('\hline');
  
  if lNode.blocks.Count > 1 then
  begin
    for i := 1 to lNode.blocks.Count -1  do
      Renderer.RenderBlock(lnode.blocks[i]);
    AppendNl('\hline');
  end;
  AppendNl('\end{tabular}');
end;

class function TLaTeXMarkDownTableBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableBlock;
end;

{ TLaTeXMarkDownTableRowBlockRenderer }

procedure TLaTeXMarkDownTableRowBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lNode : TMarkDownTableRowBlock absolute aElement;
  i, lCount : integer;
begin
  lCount:=lNode.blocks.Count;
  for i:=0 to lCount-1 do
    begin
    if i > 0 then Append(' & ');
    Renderer.RenderBlock(lNode.blocks[i]);
    end;
  AppendNl(' \');
end;

class function TLaTeXMarkDownTableRowBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableRowBlock;
end;

{ TLaTeXMarkDownHeadingBlockRenderer }

procedure TLaTeXMarkDownHeadingBlockRenderer.Dorender(aElement : TMarkDownBlock);
var
  lNode : TMarkDownHeadingBlock absolute aElement;
  lSection: String;
  lNumbered: Boolean;
begin
  lNumbered := HasOption(loNumberedSections);
  case lNode.Level of
    1: lSection := 'section';
    2: lSection := 'subsection';
    3: lSection := 'subsubsection';
    4: lSection := 'paragraph';
    5: lSection := 'subparagraph';
    else lSection := 'textbf'; // Fallback
  end;
  
  if not lNumbered then
    lSection := lSection + '*';

  Append('\' + lSection + '{');
  Renderer.RenderChildren(lNode);
  Append('}');
  AppendNl;
end;

class function TLaTeXMarkDownHeadingBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownHeadingBlock;
end;


{ TLaTeXMarkDownDocumentRenderer }

procedure TLaTeXMarkDownDocumentRenderer.Dorender(aElement: TMarkDownBlock);
var
  H : String;
begin
  if HasOption(loEnvelope) then
    begin
    AppendNL('\documentclass{article}');
    AppendNL('\usepackage[utf8]{inputenc}');
    AppendNL('\usepackage{graphicx}');
    AppendNL('\usepackage{hyperref}');
    AppendNL('\usepackage{ulem}'); // For strikethrough
    
    if LaTeXRenderer.Title<>'' then
      AppendNL('\title{' + LaTeXRenderer.EscapeLaTeX(LaTeXRenderer.Title) + '}');
    if LaTeXRenderer.Author<>'' then
      AppendNL('\author{' + LaTeXRenderer.EscapeLaTeX(LaTeXRenderer.Author) + '}');
      
    for H in LaTeXRenderer.Head do
      AppendNL(H);
      
    AppendNL('\begin{document}');
    
    if LaTeXRenderer.Title<>'' then
      AppendNL('\maketitle');
    end;
    
  Renderer.RenderChildren(aElement as TMarkDownDocument);
  
  if HasOption(loEnvelope) then
    begin
    AppendNL('\end{document}');
    end;
end;

class function TLaTeXMarkDownDocumentRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownDocument
end;


initialization
  TLaTeXMarkDownHeadingBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXParagraphBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownQuoteBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownTextBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownListBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownListItemBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownCodeBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownThematicBreakBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownTableBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownTableRowBlockRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownDocumentRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
  TLaTeXMarkDownTextRenderer.RegisterRenderer(TMarkDownLaTeXRenderer);
end.

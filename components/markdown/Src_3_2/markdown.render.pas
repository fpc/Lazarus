{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown renderer class & render factory.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit MarkDown.Render;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs,
{$ELSE}  
  Classes, SysUtils, Contnrs,
{$ENDIF}
  MarkDown.Elements,
  MarkDown.Utils;

Type
  TMarkDownElementRenderer = Class;
  TMarkDownElementRendererClass = class of TMarkDownElementRenderer;

  TMarkDownBlockRenderer = class;
  TMarkDownBlockRendererClass = class of TMarkDownBlockRenderer;
  TMarkDownElementRendererArray = array of TMarkDownElementRenderer;

  TMarkDownTextRenderer = class;
  TMarkDownTextRendererClass = class of TMarkDownTextRenderer;


  { TMarkDownRenderer }

  TMarkDownRenderer = class(TComponent)
  private
    FSkipUnknownElements: Boolean;
    FTextRenderer : TMarkDownTextRenderer;
    FRenderStack : TFPList;
    function GetParentElementRenderer: TMarkDownElementRenderer;
  protected
    function CreateRendererInstance(aClass : TMarkDownBlockRendererClass) : TMarkDownBlockRenderer; virtual;
    function CreateRendererForBlock(aBlock : TMarkdownBlock) : TMarkDownBlockRenderer; virtual;
    function CreateTextRendererInstance(aClass : TMarkDownTextRendererClass): TMarkDownTextRenderer; virtual;
    function GetTextRenderer : TMarkDownTextRenderer;
    Property ParentElementRenderer: TMarkDownElementRenderer Read GetParentElementRenderer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    Procedure RenderText(aText : TMarkDownTextNode); virtual;
    Procedure RenderTextNodes(aTextNodes : TMarkDownTextNodeList);
    Procedure RenderBlock(aBlock : TMarkdownBlock); virtual;
    procedure RenderCodeBlock(aBlock: TMarkdownBlock; const aLang: string); virtual;
    procedure RenderChildren(aBlock : TMarkDownContainerBlock); virtual;
    Procedure RenderDocument(aDocument : TMarkDownDocument); virtual; abstract;
  published
    Property SkipUnknownElements : Boolean read FSkipUnknownElements Write FSkipUnknownElements;
  end;
  TMarkDownRendererClass = class of TMarkDownRenderer;

  { TMarkDownElementRenderer }

  TMarkDownElementRenderer = Class (TObject)
  private
    FRenderer: TMarkDownRenderer;
    function GetParentElementRenderer: TMarkDownElementRenderer;
  protected
    function GetParentRenderers : TMarkDownElementRendererArray;
    function GetFirstParentWithClass(aClass : TMarkDownElementRendererClass) : TMarkDownElementRenderer;
    Property Renderer : TMarkDownRenderer read FRenderer;
    Property ParentElementRenderer : TMarkDownElementRenderer read GetParentElementRenderer;
  public
    constructor create(aRenderer : TMarkDownRenderer);
    procedure reset; virtual;
  end;


  { TMarkDownBlockRenderer }

  TMarkDownBlockRenderer = Class (TMarkDownElementRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); virtual; abstract;
  Public
    class function BlockClass : TMarkDownBlockClass; virtual; abstract;
    class procedure RegisterRenderer(aRendererClass : TMarkDownRendererClass);
    procedure Render(aBlock : TMarkDownBlock); inline;
  end;

  { TMarkDownTextRenderer }

  TMarkDownTextRenderer = class(TMarkDownElementRenderer)
  protected
    procedure DoRender(aElement: TMarkDownTextNode); virtual; abstract;
  public
    class procedure RegisterRenderer(aRendererClass : TMarkDownRendererClass);
    procedure render(aElement : TMarkDownTextNode); inline;
    // Block state management.
    procedure BeginBlock; virtual;
    procedure EndBlock; virtual;
  end;

  { TNullRenderer }

  TNullRenderer = class(TMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  end;

  { TDocumentRenderer }

  TDocumentRenderer = class(TMarkDownBlockRenderer)
    class function BlockClass : TMarkDownBlockClass; override;
  end;


  { TMarkDownRendererFactory }

  TMarkDownRendererFactory = class(TObject)
  private
    class var _Instance : TMarkDownRendererFactory;
  type
    { TBlockRenderRegistration }

    TBlockRenderRegistration = class
      BlockClass : TMarkDownBlockClass;
      RendererClass : TMarkDownBlockRendererClass;
      constructor create(aBlockClass : TMarkDownBlockClass; aRendererClass : TMarkDownBlockRendererClass);
    end;
    TBlockRenderRegistrationList = Specialize TGFPObjectList<TBlockRenderRegistration>;

    { TRenderBlockRenderers }

    TRenderBlockRenderers = class(TObject)
      Renderer : TMarkDownRendererClass;
      BlockRenderers : TBlockRenderRegistrationList;
      Textrenderer : TMarkDownTextRendererClass;
      Constructor create(aRenderer : TMarkDownRendererClass);
      destructor destroy; override;
      function FindBlock (aClass : TMarkdownBlockClass; aAllowCreate : Boolean) : TBlockRenderRegistration;
     end;
     TRenderBlockRenderersList = specialize TGFPObjectList<TRenderBlockRenderers>;

  private
    FRegistry : TRenderBlockRenderersList;
  protected
    function FindRenderer(aClass : TMarkDownRendererClass; allowCreate : Boolean) : TRenderBlockRenderers;
  public
    class constructor init;
    class destructor done;
    constructor create;
    destructor destroy; override;
    procedure RegisterBlockRenderer(aRendererClass : TMarkdownRendererClass; aBlockClass : TMarkdownBlockClass; aBlockRendererClass : TMarkdownBlockRendererClass);
    function FindBlockRendererClass(aRendererClass : TMarkdownRendererClass; aBlockClass : TMarkdownBlockClass) : TMarkdownBlockRendererClass;
    procedure RegisterTextRenderer(aRendererClass : TMarkdownRendererClass; aTextRendererClass : TMarkdownTextRendererClass);
    function FindTextRendererClass(aRendererClass : TMarkdownRendererClass) : TMarkdownTextRendererClass;
    class property Instance : TMarkDownRendererFactory read _Instance;
  end;

implementation

class procedure TMarkDownBlockRenderer.RegisterRenderer(aRendererClass: TMarkDownRendererClass);

begin
  TMarkDownRendererFactory.Instance.RegisterBlockRenderer(aRendererClass, BlockClass, Self);
end;


procedure TMarkDownBlockRenderer.render(aBlock: TMarkDownBlock);

begin
  DoRender(aBlock);
end;


{ TMarkDownTextRenderer }

class procedure TMarkDownTextRenderer.RegisterRenderer(aRendererClass: TMarkDownRendererClass);

begin
  TMarkDownRendererFactory.Instance.RegisterTextRenderer(aRendererClass,Self);
end;


procedure TMarkDownTextRenderer.render(aElement: TMarkDownTextNode);

begin
  DoRender(aElement);
end;


procedure TMarkDownTextRenderer.BeginBlock;

begin
  // Do nothing
end;

procedure TMarkDownTextRenderer.EndBlock;

begin
  //
end;

{ TNullRenderer }

procedure TNullRenderer.DoRender(aBlock: TMarkDownBlock);

begin
  if aBlock is TMarkDownContainerBlock then
    Renderer.RenderChildren(TMarkDownContainerBlock(aBlock));
end;


{ TDocumentRenderer }

class function TDocumentRenderer.BlockClass: TMarkDownBlockClass;

begin
  Result:=TMarkDownDocument;
end;


function TMarkDownRenderer.CreateRendererInstance(aClass: TMarkDownBlockRendererClass): TMarkDownBlockRenderer;

begin
  Result:=aClass.Create(Self);
end;


function TMarkDownRenderer.CreateRendererForBlock(aBlock: TMarkdownBlock): TMarkDownBlockRenderer;

var
  lRenderClass : TMarkDownRendererClass;
  lBlockClass : TMarkDownBlockClass;
  LBlockRendererClass : TMarkDownBlockRendererClass;

begin
  Result:=Nil;
  lRenderClass:=TMarkDownRendererClass(Self.ClassType);
  lBlockClass:=TMarkDownBlockClass(aBlock.ClassType);
  LBlockRendererClass:=TMarkDownRendererFactory.Instance.FindBlockRendererClass(lRenderClass,lBlockClass);
  if assigned(LBlockRendererClass) then
    Result:=CreateRendererInstance(LBlockRendererClass);
  if assigned(Result) then
    Result.Reset;
end;


function TMarkDownRenderer.CreateTextRendererInstance(aClass : TMarkDownTextRendererClass): TMarkDownTextRenderer;

begin
  Result:=aClass.Create(Self);
end;

function TMarkDownRenderer.GetTextRenderer: TMarkDownTextRenderer;

var
  lClass : TMarkDownTextRendererClass;
  lRenderClass : TMarkDownRendererClass;

begin
  Result:=nil;
  if FTextRenderer=Nil then;
    begin
    lRenderClass:=TMarkDownRendererClass(Self.ClassType);
    lClass:=TMarkDownRendererFactory.Instance.FindTextRendererClass(lRenderClass);
    if assigned(lClass) then
      FTextRenderer:=CreateTextRendererInstance(lClass);
    end;
  Result:=FTextRenderer;
end;

function TMarkDownRenderer.GetParentElementRenderer: TMarkDownElementRenderer;
begin
  if FRenderStack.Count>1 then
    Result:=TMarkDownElementRenderer(FRenderStack[FRenderStack.Count-2])
  else
    Result:=Nil;
end;

constructor TMarkDownRenderer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderStack:=TFPList.Create;
end;

destructor TMarkDownRenderer.destroy;
begin
  FreeAndNil(FRenderStack);
  FreeAndNil(FTextRenderer);
  inherited destroy;
end;


procedure TMarkDownRenderer.RenderText(aText: TMarkDownTextNode);

var
  lRender : TMarkDownTextRenderer;

begin
  lRender:=GetTextRenderer;
  lRender.BeginBlock;
  lRender.render(aText);
  lRender.EndBlock;
end;

procedure TMarkDownRenderer.RenderTextNodes(aTextNodes: TMarkDownTextNodeList);

var
  lRender : TMarkDownTextRenderer;
  lNode : TMarkDownTextNode;

begin
  lRender:=GetTextRenderer;
  lRender.BeginBlock;
  For lNode in aTextNodes do
    lRender.render(lNode);
  lRender.EndBlock;
end;

procedure TMarkDownRenderer.RenderBlock(aBlock: TMarkdownBlock);

var
  lRender : TMarkDownBlockRenderer;

begin
  if aBlock=Nil then
    Raise EMarkdown.Create('Cannot render nil block');
  lRender:=CreateRendererForBlock(aBlock);
  try
    if Assigned(lRender) then
      begin
      FRenderStack.Add(lRender);
      lRender.render(aBlock);
      end
    else
      Raise EMarkDown.CreateFmt('No renderer for block class: %s',[aBlock.ClassName]);
  finally
    if assigned(lRender) then
      FRenderStack.Delete(FRenderStack.Count-1);
    lRender.Free;
  end;
end;

procedure TMarkDownRenderer.RenderCodeBlock(aBlock: TMarkdownBlock; const aLang: string);
begin
  if (aLang='') then ; // Silence warning
  RenderBlock(aBlock);
end;

procedure TMarkDownRenderer.RenderChildren(aBlock: TMarkDownContainerBlock);

var
  I : integer;

begin
  for I:=0 to aBlock.Blocks.Count-1 do
    RenderBlock(aBlock.Blocks[I]);
end;

{ TMarkDownElementRenderer }

function TMarkDownElementRenderer.GetParentElementRenderer: TMarkDownElementRenderer;
begin
  Result:=Renderer.ParentElementRenderer;
end;

function TMarkDownElementRenderer.GetParentRenderers: TMarkDownElementRendererArray;
var
  i : integer;
begin
  Result:=[];
  SetLength(Result,Renderer.FRenderStack.Count);
  For I:=0 to Renderer.FRenderStack.Count-1 do
    Result[i]:=TMarkDownElementRenderer(Renderer.FRenderStack.items[i]);
end;

function TMarkDownElementRenderer.GetFirstParentWithClass(aClass: TMarkDownElementRendererClass): TMarkDownElementRenderer;
var
  I : integer;
begin
  Result:=Nil;
  I:=Renderer.FRenderStack.Count-1;
  While (Result=Nil) and (I>=0) do
    begin
    Result:=TMarkDownElementRenderer(Renderer.FRenderStack.items[i]);
    if Not Result.InheritsFrom(aClass) then
      Result:=Nil;
    Dec(i);
    end;
end;

constructor TMarkDownElementRenderer.create(aRenderer: TMarkDownRenderer);

begin
  FRenderer:=aRenderer;
end;

procedure TMarkDownElementRenderer.reset;

begin
  // Do nothing
end;

{ TMarkDownRendererFactory }

constructor TMarkDownRendererFactory.create;

begin
  FRegistry:=TRenderBlockRenderersList.Create(True);
end;

destructor TMarkDownRendererFactory.destroy;

begin
  FreeAndNil(FRegistry);
  inherited destroy;
end;


procedure TMarkDownRendererFactory.RegisterBlockRenderer(aRendererClass: TMarkdownRendererClass; aBlockClass: TMarkdownBlockClass;
  aBlockRendererClass: TMarkdownBlockRendererClass);

var
  lList : TRenderBlockRenderers;
  lReg : TBlockRenderRegistration;

begin
  lList:=FindRenderer(aRendererClass,True);
  lReg:=lList.FindBlock(aBlockClass,True);
  lReg.RendererClass:=aBlockRendererClass;
end;


function TMarkDownRendererFactory.FindBlockRendererClass(aRendererClass: TMarkdownRendererClass; aBlockClass: TMarkdownBlockClass
  ): TMarkdownBlockRendererClass;

var
  lList : TRenderBlockRenderers;
  lReg : TBlockRenderRegistration;

begin
  Result:=Nil;
  lList:=FindRenderer(aRendererClass,False);
  if Assigned(lList) then
    begin
    lReg:=lList.FindBlock(aBlockClass,False);
    if assigned(lReg) then
      Result:=lReg.RendererClass;
    end;
end;


procedure TMarkDownRendererFactory.RegisterTextRenderer(aRendererClass: TMarkdownRendererClass;
  aTextRendererClass: TMarkdownTextRendererClass);

var
  lList : TRenderBlockRenderers;

begin
  lList:=FindRenderer(aRendererClass,True);
  lList.Textrenderer:=aTextRendererClass;
end;


function TMarkDownRendererFactory.FindTextRendererClass(aRendererClass: TMarkdownRendererClass): TMarkdownTextRendererClass;

var
  lList : TRenderBlockRenderers;

begin
  lList:=FindRenderer(aRendererClass,True);
  if assigned(lList) then
    Result:=lList.Textrenderer;
end;


function TMarkDownRendererFactory.FindRenderer(aClass: TMarkDownRendererClass; allowCreate: Boolean): TRenderBlockRenderers;

var
  I : Integer;

begin
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<FRegistry.Count) do
    begin
    Result:=FRegistry[I];
    if Result.Renderer<>aClass then
      Result:=Nil;
    Inc(I);
    end;
  if (Result=nil) and AllowCreate then
    begin
    Result:=TRenderBlockRenderers.Create(aClass);
    FRegistry.Add(Result);
    end;
end;


class constructor TMarkDownRendererFactory.init;

begin
  _Instance:=TMarkDownRendererFactory.Create;
end;


class destructor TMarkDownRendererFactory.done;

begin
  FreeAndNil(_Instance);
end;


{ TMarkDownRendererFactory.TBlockRenderRegistration }

constructor TMarkDownRendererFactory.TBlockRenderRegistration.create(aBlockClass: TMarkDownBlockClass;
  aRendererClass: TMarkDownBlockRendererClass);

begin
  BlockClass:=aBlockClass;
  RendererClass:=aRendererClass;
end;


{ TMarkDownRendererFactory.TRenderBlockRenderers }

constructor TMarkDownRendererFactory.TRenderBlockRenderers.create(aRenderer: TMarkDownRendererClass);

begin
  Renderer:=aRenderer;
  BlockRenderers:=TBlockRenderRegistrationList.Create(True);
end;


destructor TMarkDownRendererFactory.TRenderBlockRenderers.destroy;

begin
  FreeAndNil(BlockRenderers);
  inherited destroy;
end;


function TMarkDownRendererFactory.TRenderBlockRenderers.FindBlock(aClass: TMarkdownBlockClass; aAllowCreate: Boolean
  ): TBlockRenderRegistration;

var
  I : Integer;

begin
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<BlockRenderers.Count) do
    begin
    Result:=BlockRenderers[I];
    if Result.BlockClass<>aClass then
      Result:=Nil;
    Inc(I);
    end;
  if (Result=nil) and aAllowCreate then
    begin
    Result:=TBlockRenderRegistration.Create(aClass,Nil);
    BlockRenderers.Add(Result);
    end;
end;


end.


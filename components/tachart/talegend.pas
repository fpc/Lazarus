{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Alexander Klenin

}
unit TALegend;

{$H+}

interface

uses
  Classes, Contnrs, FPCanvas, Graphics, SysUtils,
  TAChartUtils, TADrawUtils, TATypes;

const
  DEF_LEGEND_SPACING = 4;
  DEF_LEGEND_MARGIN = 4;
  DEF_LEGEND_SYMBOL_WIDTH = 20;
  LEGEND_ITEM_ORDER_AS_ADDED = -1;
  LEGEND_ITEM_NO_GROUP = -1;

type
  { TLegendItem }

  TLegendItem = class
  strict private
    FColor: TColor;
    FFont: TFont;
    FGroupIndex: Integer;
    FOrder: Integer;
    FOwner: TIndexedComponent;
    FText: String;
  public
    constructor Create(const AText: String; AColor: TColor = clTAColor);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); virtual;
    function HasSymbol: Boolean; virtual;
    procedure UpdateFont(ADrawer: IChartDrawer; var APrevFont: TFont);
  public
    property Color: TColor read FColor write FColor;
    property Font: TFont read FFont write FFont;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property Order: Integer read FOrder write FOrder;
    property Owner: TIndexedComponent read FOwner write FOwner;
    property Text: String read FText write FText;
  end;

  { TLegendItemGroupTitle }

  TLegendItemGroupTitle = class(TLegendItem)
  public
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
    function HasSymbol: Boolean; override;
  end;

  TLegendItemDrawEvent = procedure (
    ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; AItem: TLegendItem
  ) of object;

  { TLegendItemUserDrawn }

  TLegendItemUserDrawn = class(TLegendItem)
  private
    FIndex: Integer;
    FOnDraw: TLegendItemDrawEvent;
  public
    constructor Create(
      AIndex: Integer; AOnDraw: TLegendItemDrawEvent; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
    property OnDraw: TLegendItemDrawEvent read FOnDraw;
  end;

  { TLegendItemLine }

  TLegendItemLine = class(TLegendItem)
  private
    FPen: TFPCustomPen;
  public
    constructor Create(APen: TFPCustomPen; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  { TLegendItemLinePointer }

  TLegendItemLinePointer = class(TLegendItemLine)
  protected
    FPointer: TSeriesPointer;
  public
    constructor Create(
      APen: TPen; APointer: TSeriesPointer; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  { TLegendItemBrushRect }

  TLegendItemBrushRect = class(TLegendItem)
  private
    FBrush: TFPCustomBrush;
  public
    constructor Create(ABrush: TFPCustomBrush; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  TLegendItemsEnumerator = class(TListEnumerator)
  public
    function GetCurrent: TLegendItem;
    property Current: TLegendItem read GetCurrent;
  end;

  { TChartLegendItems }

  TChartLegendItems = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TLegendItem;
    procedure SetItem(AIndex: Integer; AValue: TLegendItem);
  public
    function GetEnumerator: TLegendItemsEnumerator;
    property Items[AIndex: Integer]: TLegendItem
      read GetItem write SetItem; default;
  end;

  TChartLegendBrush = class(TBrush)
  published
    property Color default clWhite;
  end;

  TLegendAlignment = (
    laTopLeft, laCenterLeft, laBottomLeft,
    laTopCenter, laBottomCenter, // laCenterCenter makes no sense.
    laTopRight, laCenterRight, laBottomRight);

  TChartLegendDrawingData = record
    FDrawer: IChartDrawer;
    FItems: TChartLegendItems;
    FColCount: Integer;
    FRowCount: Integer;
    FItemSize: TPoint;
    FBounds: TRect;
  end;

  TLegendColumnCount = 1..MaxInt;
  TLegendItemFillOrder = (lfoColRow, lfoRowCol);

  { TChartLegend }

  TChartLegend = class(TChartElement)
  private
    FAlignment: TLegendAlignment;
    FBackgroundBrush: TChartLegendBrush;
    FColumnCount: TLegendColumnCount;
    FFont: TFont;
    FFrame: TChartPen;
    FGroupFont: TFont;
    FGroupTitles: TStrings;
    FItemFillOrder: TLegendItemFillOrder;
    FMarginX: TChartDistance;
    FMarginY: TChartDistance;
    FSpacing: TChartDistance;
    FSymbolFrame: TChartPen;
    FSymbolWidth: TChartDistance;
    FUseSidebar: Boolean;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetBackgroundBrush(AValue: TChartLegendBrush);
    procedure SetColumnCount(AValue: TLegendColumnCount);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetGroupFont(AValue: TFont);
    procedure SetGroupTitles(AValue: TStrings);
    procedure SetItemFillOrder(AValue: TLegendItemFillOrder);
    procedure SetMargin(AValue: TChartDistance);
    procedure SetMarginX(AValue: TChartDistance);
    procedure SetMarginY(AValue: TChartDistance);
    procedure SetSpacing(AValue: TChartDistance);
    procedure SetSymbolFrame(AValue: TChartPen);
    procedure SetSymbolWidth(AValue: TChartDistance);
    procedure SetUseSidebar(AValue: Boolean);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

  public
    procedure AddGroups(AItems: TChartLegendItems);
    procedure Assign(Source: TPersistent); override;
    procedure Draw(var AData: TChartLegendDrawingData);
    // Not includes the margins around item.
    function MeasureItem(
      ADrawer: IChartDrawer; AItems: TChartLegendItems): TPoint;
    procedure Prepare(var AData: TChartLegendDrawingData; var AClipRect: TRect);
    procedure SortItemsByOrder(AItems: TChartLegendItems);
  published
    property Alignment: TLegendAlignment
      read FAlignment write SetAlignment default laTopRight;
    property BackgroundBrush: TChartLegendBrush
      read FBackgroundBrush write SetBackgroundBrush;
    property ColumnCount: TLegendColumnCount
      read FColumnCount write SetColumnCount default 1;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property GroupFont: TFont read FGroupFont write SetGroupFont;
    property GroupTitles: TStrings read FGroupTitles write SetGroupTitles;
    property ItemFillOrder: TLegendItemFillOrder
      read FItemFillOrder write SetItemFillOrder default lfoColRow;
    property Margin: TChartDistance
      read FMarginX write SetMargin stored false; deprecated;
    property MarginX: TChartDistance
      read FMarginX write SetMarginX default DEF_LEGEND_MARGIN;
    property MarginY: TChartDistance
      read FMarginY write SetMarginY default DEF_LEGEND_MARGIN;
    property Spacing: TChartDistance
      read FSpacing write SetSpacing default DEF_LEGEND_SPACING;
    property SymbolFrame: TChartPen read FSymbolFrame write SetSymbolFrame;
    property SymbolWidth: TChartDistance
      read FSymbolWidth write SetSymbolWidth default DEF_LEGEND_SYMBOL_WIDTH;
    property UseSidebar: Boolean read FUseSidebar write SetUseSidebar default true;
    property Visible default false;
  end;

  TLegendMultiplicity = (lmSingle, lmPoint);

  TLegendItemCreateEvent = procedure (
    AItem: TLegendItem; AIndex: Integer) of object;

  { TChartSeriesLegend }

  TChartSeriesLegend = class(TChartElement)
  private
    FGroupIndex: Integer;
    FMultiplicity: TLegendMultiplicity;
    FOnCreate: TLegendItemCreateEvent;
    FOnDraw: TLegendItemDrawEvent;
    FOrder: Integer;
    FUserItemsCount: Integer;
    procedure SetGroupIndex(AValue: Integer);
    procedure SetMultiplicity(AValue: TLegendMultiplicity);
    procedure SetOnCreate(AValue: TLegendItemCreateEvent);
    procedure SetOnDraw(AValue: TLegendItemDrawEvent);
    procedure SetOrder(AValue: Integer);
    procedure SetUserItemsCount(AValue: Integer);
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(Source: TPersistent); override;
    procedure InitItem(
      AItem: TLegendItem; AIndex: Integer; ALegend: TChartLegend);
  published
    property GroupIndex: Integer
      read FGroupIndex write SetGroupIndex default LEGEND_ITEM_NO_GROUP;
    property Multiplicity: TLegendMultiplicity
      read FMultiplicity write SetMultiplicity default lmSingle;
    property Order: Integer
      read FOrder write SetOrder default LEGEND_ITEM_ORDER_AS_ADDED;
    property UserItemsCount: Integer
      read FUserItemsCount write SetUserItemsCount default 1;
    property Visible default true;

  published
    property OnCreate: TLegendItemCreateEvent read FOnCreate write SetOnCreate;
    property OnDraw: TLegendItemDrawEvent read FOnDraw write SetOnDraw;
  end;

implementation

uses
  Math, PropEdits, Types, TADrawerCanvas, TAGeometry;

const
  SYMBOL_TEXT_SPACING = 4;

function LegendItemCompare(AItem1, AItem2: Pointer): Integer;
var
  li1: TLegendItem absolute AItem1;
  li2: TLegendItem absolute AItem2;
begin
  Result := Sign(li1.GroupIndex - li2.GroupIndex);
  if Result = 0 then
    Result := Sign(li1.Order - li2.Order);
end;

{ TLegendItemsEnumerator }

function TLegendItemsEnumerator.GetCurrent: TLegendItem;
begin
  Result := TLegendItem(inherited GetCurrent);
end;

{ TChartLegendItems }

function TChartLegendItems.GetEnumerator: TLegendItemsEnumerator;
begin
  Result := TLegendItemsEnumerator.Create(Self);
end;

function TChartLegendItems.GetItem(AIndex: Integer): TLegendItem;
begin
  Result := TLegendItem(inherited GetItem(AIndex));
end;

procedure TChartLegendItems.SetItem(AIndex: Integer; AValue: TLegendItem);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TLegendItem }

constructor TLegendItem.Create(const AText: String; AColor: TColor);
begin
  FColor := AColor;
  FGroupIndex := LEGEND_ITEM_NO_GROUP;
  FOrder := LEGEND_ITEM_ORDER_AS_ADDED;
  FText := AText;
end;

procedure TLegendItem.Draw(ADrawer: IChartDrawer; const ARect: TRect);
begin
  ADrawer.TextOut.
    Pos(ARect.Right + SYMBOL_TEXT_SPACING, ARect.Top).Text(FText).Done;
end;

function TLegendItem.HasSymbol: Boolean;
begin
  Result := true;
end;

procedure TLegendItem.UpdateFont(ADrawer: IChartDrawer; var APrevFont: TFont);
begin
  if APrevFont = Font then exit;
  ADrawer.Font := Font;
  APrevFont := Font;
end;

{ TLegendItemGroupTitle }

procedure TLegendItemGroupTitle.Draw(ADrawer: IChartDrawer; const ARect: TRect);
begin
  ADrawer.TextOut.Pos(ARect.Left, ARect.Top).Text(Text).Done;
end;

function TLegendItemGroupTitle.HasSymbol: Boolean;
begin
  Result := false;
end;

{ TLegendItemUserDrawn }

constructor TLegendItemUserDrawn.Create(
  AIndex: Integer; AOnDraw: TLegendItemDrawEvent; const AText: String);
begin
  inherited Create(AText);
  FIndex := AIndex;
  FOnDraw := AOnDraw;
end;

procedure TLegendItemUserDrawn.Draw(ADrawer: IChartDrawer; const ARect: TRect);
var
  ic: IChartTCanvasDrawer;
begin
  if Supports(ADrawer, IChartTCanvasDrawer, ic) and Assigned(FOnDraw) then
    FOnDraw(ic.Canvas, ARect, FIndex, Self);
  inherited Draw(ADrawer, ARect);
end;

{ TLegendItemLine }

constructor TLegendItemLine.Create(APen: TFPCustomPen; const AText: String);
begin
  inherited Create(AText);
  FPen := APen;
end;

procedure TLegendItemLine.Draw(ADrawer: IChartDrawer; const ARect: TRect);
var
  y: Integer;
begin
  inherited Draw(ADrawer, ARect);
  if FPen = nil then exit;
  ADrawer.Pen := FPen;
  y := (ARect.Top + ARect.Bottom) div 2;
  ADrawer.Line(ARect.Left, y, ARect.Right, y);
end;

{ TLegendItemLinePointer }

constructor TLegendItemLinePointer.Create(
  APen: TPen; APointer: TSeriesPointer; const AText: String);
begin
  inherited Create(APen, AText);
  FPointer := APointer;
end;

procedure TLegendItemLinePointer.Draw(
  ADrawer: IChartDrawer; const ARect: TRect);
var
  c, sz: TPoint;
begin
  inherited Draw(ADrawer, ARect);
  if FPointer = nil then exit;
  c := CenterPoint(ARect);
  // Max width slightly narrower then ARect to leave place for the line.
  sz.X := Min(FPointer.HorizSize, (ARect.Right - ARect.Left) div 3);
  sz.Y := Min(FPointer.VertSize, (ARect.Bottom - ARect.Top) div 2);
  FPointer.DrawSize(ADrawer, c, sz, Color);
end;

{ TLegendItemBrushRect }

constructor TLegendItemBrushRect.Create(
  ABrush: TFPCustomBrush; const AText: String);
begin
  inherited Create(AText);
  FBrush := ABrush;
end;

procedure TLegendItemBrushRect.Draw(ADrawer: IChartDrawer; const ARect: TRect);
begin
  inherited Draw(ADrawer, ARect);
  if FBrush = nil then
    ADrawer.SetBrushParams(bsSolid, ColorDef(Color, clRed))
  else begin
    ADrawer.Brush := FBrush;
    if Color <> clTAColor then
      ADrawer.SetBrushParams(FBrush.Style, Color);
  end;
  ADrawer.Rectangle(ARect);
end;

{ TChartLegend }

procedure TChartLegend.AddGroups(AItems: TChartLegendItems);
var
  i, gi: Integer;
  g: TLegendItemGroupTitle;
begin
  for i := AItems.Count - 1 downto 0 do begin
    gi := AItems[i].GroupIndex;
    if
      InRange(gi, 0, GroupTitles.Count - 1) and
      ((i = 0) or (AItems[i - 1].GroupIndex <> gi))
    then begin
      g := TLegendItemGroupTitle.Create(GroupTitles[gi]);
      g.GroupIndex := gi;
      g.Font := GroupFont;
      AItems.Insert(i, g);
    end;
  end;
end;

procedure TChartLegend.Assign(Source: TPersistent);
begin
  if Source is TChartLegend then
    with TChartLegend(Source) do
      Self.FAlignment := FAlignment;

  inherited Assign(Source);
end;

constructor TChartLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FAlignment := laTopRight;
  FColumnCount := 1;
  FGroupTitles := TStringList.Create;
  FMarginX := DEF_LEGEND_MARGIN;
  FMarginY := DEF_LEGEND_MARGIN;
  FSpacing := DEF_LEGEND_SPACING;
  FSymbolWidth := DEF_LEGEND_SYMBOL_WIDTH;
  FUseSidebar := true;
  Visible := false;

  InitHelper(FBackgroundBrush, TChartLegendBrush);
  InitHelper(FFont, TFont);
  InitHelper(FFrame, TChartPen);
  InitHelper(FGroupFont, TFont);
  InitHelper(FSymbolFrame, TChartPen);
end;

destructor TChartLegend.Destroy;
begin
  FreeAndNil(FBackgroundBrush);
  FreeAndNil(FFont);
  FreeAndNil(FFrame);
  FreeAndNil(FGroupFont);
  FreeAndNil(FGroupTitles);
  FreeAndNil(FSymbolFrame);

  inherited;
end;

procedure TChartLegend.Draw(var AData: TChartLegendDrawingData);
var
  i, x, y: Integer;
  r: TRect;
  prevFont: TFont = nil;
  drawer: IChartDrawer;
begin
  drawer := AData.FDrawer;
  // Draw the background and the border.
  drawer.Brush := BackgroundBrush;
  if Frame.Visible then
    drawer.Pen := Frame
  else
    drawer.SetPenParams(psClear, clTAColor);
  r := AData.FBounds;
  drawer.Rectangle(r);
  if AData.FItems.Count = 0 then exit;

  r.Right -= 1;
  drawer.ClippingStart(r);

  with AData do try
    for i := 0 to FItems.Count - 1 do begin
      FItems[i].UpdateFont(drawer, prevFont);
      drawer.Brush := BackgroundBrush;
      if SymbolFrame.Visible then
        drawer.Pen := SymbolFrame
      else
        drawer.SetPenParams(psClear, clTAColor);
      x := 0;
      y := 0;
      case ItemFillOrder of
        lfoColRow: DivMod(i, FRowCount, x, y);
        lfoRowCol: DivMod(i, FColCount, y, x);
      end;
      r := Bounds(
        FBounds.Left + Spacing + x * (FItemSize.X + Spacing),
        FBounds.Top + Spacing + y * (FItemSize.Y + Spacing),
        SymbolWidth, FItemSize.Y);
      FItems[i].Draw(drawer, r);
      OffsetRect(r, 0, FItemSize.Y + Spacing);
    end;
  finally
    drawer.ClippingStop;
  end;
end;

function TChartLegend.MeasureItem(
  ADrawer: IChartDrawer; AItems: TChartLegendItems): TPoint;
var
  p: TPoint;
  prevFont: TFont = nil;
  li: TLegendItem;
begin
  Result := Point(0, 0);
  for li in AItems do begin
    li.UpdateFont(ADrawer, prevFont);
    p := ADrawer.TextExtent(li.Text);
    if li.HasSymbol then
      p.X += SYMBOL_TEXT_SPACING + SymbolWidth;
    Result := MaxPoint(p, Result);
  end;
end;

procedure TChartLegend.Prepare(
  var AData: TChartLegendDrawingData; var AClipRect: TRect);
var
  x, y: Integer;
  sidebar, legendSize: TPoint;
begin
  with AData do begin
    FColCount := Max(Min(ColumnCount, FItems.Count), 1);
    FRowCount := (FItems.Count - 1) div FColCount + 1;
    FItemSize := MeasureItem(FDrawer, FItems);
    legendSize.X := (FItemSize.X + Spacing) * FColCount + Spacing;
    legendSize.Y := (FItemSize.Y + Spacing) * FRowCount + Spacing;
  end;

  sidebar.X := 2 * MarginX;
  with AClipRect do
    legendSize.X := EnsureRange(legendSize.X, 0, Right - Left - sidebar.X);
  sidebar.X += legendSize.X;

  sidebar.Y := 2 * MarginX;
  with AClipRect do
    legendSize.Y := EnsureRange(legendSize.Y, 0, Bottom - Top - sidebar.Y);
  sidebar.Y += legendSize.Y;

  // Determine position according to the alignment.
  case Alignment of
    laTopLeft, laCenterLeft, laBottomLeft:
      x := AClipRect.Left + MarginX;
    laTopRight, laCenterRight, laBottomRight:
      x := AClipRect.Right - legendSize.X - MarginX;
    laTopCenter, laBottomCenter:
      x := (AClipRect.Right + AClipRect.Left - legendSize.X) div 2;
  end;
  case Alignment of
    laTopLeft, laTopCenter, laTopRight:
      y := AClipRect.Top + MarginY;
    laBottomLeft, laBottomCenter, laBottomRight:
      y := AClipRect.Bottom - MarginY - legendSize.Y;
    laCenterLeft, laCenterRight:
      y := (AClipRect.Top + AClipRect.Bottom - legendSize.Y) div 2;
  end;
  if UseSidebar then
    case Alignment of
      laTopLeft, laCenterLeft, laBottomLeft:
        AClipRect.Left += sidebar.X;
      laTopRight, laCenterRight, laBottomRight:
        AClipRect.Right -= sidebar.X;
      laTopCenter:
        AClipRect.Top += legendSize.Y + 2 * MarginY;
      laBottomCenter:
        AClipRect.Bottom -= legendSize.Y + 2 * MarginY;
    end;
  AData.FBounds := Bounds(x, y, legendSize.X, legendSize.Y);
end;

procedure TChartLegend.SetAlignment(AValue: TLegendAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetBackgroundBrush(AValue: TChartLegendBrush);
begin
  FBackgroundBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetColumnCount(AValue: TLegendColumnCount);
begin
  if FColumnCount = AValue then exit;
  FColumnCount := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetGroupFont(AValue: TFont);
begin
  FGroupFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetGroupTitles(AValue: TStrings);
begin
  FGroupTitles.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetItemFillOrder(AValue: TLegendItemFillOrder);
begin
  if FItemFillOrder = AValue then exit;
  FItemFillOrder := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetMargin(AValue: TChartDistance);
begin
  SetMarginX(AValue);
  SetMarginY(AValue);
end;

procedure TChartLegend.SetMarginX(AValue: TChartDistance);
begin
  if FMarginX = AValue then exit;
  FMarginX := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetMarginY(AValue: TChartDistance);
begin
  if FMarginY = AValue then exit;
  FMarginY := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetSpacing(AValue: TChartDistance);
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetSymbolFrame(AValue: TChartPen);
begin
  if FSymbolFrame = AValue then exit;
  FSymbolFrame := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetSymbolWidth(AValue: TChartDistance);
begin
  if FSymbolWidth = AValue then exit;
  FSymbolWidth := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetUseSidebar(AValue: Boolean);
begin
  if FUseSidebar = AValue then exit;
  FUseSidebar := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SortItemsByOrder(AItems: TChartLegendItems);
var
  i: Integer;
  j: Integer = MaxInt;
begin
  for i := AItems.Count - 1 downto 0 do
    if AItems[i].Order = LEGEND_ITEM_ORDER_AS_ADDED then begin
      AItems[i].Order := j;
      j -= 1;
    end;
  AItems.Sort(@LegendItemCompare);
end;

{ TChartSeriesLegend }

procedure TChartSeriesLegend.Assign(Source: TPersistent);
begin
  if Source is TChartSeriesLegend then
    with TChartSeriesLegend(Source) do begin
      Self.FMultiplicity := FMultiplicity;
      Self.FOnDraw := FOnDraw;
      Self.FUserItemsCount := FUserItemsCount;
    end;

  inherited Assign(Source);
end;

constructor TChartSeriesLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FGroupIndex := LEGEND_ITEM_NO_GROUP;
  FOrder := LEGEND_ITEM_ORDER_AS_ADDED;
  FVisible := true;
  FUserItemsCount := 1;
end;

procedure TChartSeriesLegend.InitItem(
  AItem: TLegendItem; AIndex: Integer; ALegend: TChartLegend);
begin
  if Assigned(OnCreate) then
    OnCreate(AItem, AIndex);
  if AItem.Font = nil then
    AItem.Font := ALegend.Font;
  if AItem.GroupIndex = LEGEND_ITEM_NO_GROUP then
    AItem.GroupIndex := GroupIndex;
  if AItem.Order = LEGEND_ITEM_ORDER_AS_ADDED then
    AItem.Order := Order;
end;

procedure TChartSeriesLegend.SetGroupIndex(AValue: Integer);
begin
  if FGroupIndex = AValue then exit;
  FGroupIndex := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetMultiplicity(AValue: TLegendMultiplicity);
begin
  if FMultiplicity = AValue then exit;
  FMultiplicity := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetOnDraw(AValue: TLegendItemDrawEvent);
begin
  if TMethod(FOnDraw) = TMethod(AValue) then exit;
  FOnDraw := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetOnCreate(AValue: TLegendItemCreateEvent);
begin
  if TMethod(FOnCreate) = TMethod(AValue) then exit;
  FOnCreate := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetOrder(AValue: Integer);
begin
  if FOrder = AValue then exit;
  FOrder := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetUserItemsCount(AValue: Integer);
begin
  if FUserItemsCount = AValue then exit;
  FUserItemsCount := AValue;
  StyleChanged(Self);
end;

procedure SkipObsoleteProperties;
begin
  RegisterPropertyEditor(
    TypeInfo(TChartDistance), TChartLegend, 'Margin', THiddenPropertyEditor);
end;

initialization
  SkipObsoleteProperties;

end.


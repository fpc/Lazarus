{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
    FTextFormat: TChartTextFormat;
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
    property TextFormat: TChartTextFormat read FTextFormat write FTextFormat;
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
  strict private
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
  strict protected
    FPen: TFPCustomPen;
  public
    constructor Create(APen: TFPCustomPen; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  { TLegendItemLinePointer }

  TLegendItemLinePointer = class(TLegendItemLine)
  strict protected
    FPointer: TSeriesPointer;
    FBrush: TBrush;
    FPresetBrush: Boolean;
  public
    constructor Create(
      APen: TPen; APointer: TSeriesPointer; const AText: String);
    constructor CreateWithBrush(
      APen: TPen; ABrush: TBrush; APointer: TSeriesPointer; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  { TLegendItemBrushRect }

  TLegendItemBrushRect = class(TLegendItem)
  strict private
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
  strict private
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
    FBounds: TRect;
    FColCount: Integer;
    FDrawer: IChartDrawer;
    FItems: TChartLegendItems;
    FItemSize: TPoint;
    FRowCount: Integer;
  end;

  TLegendColumnCount = 1..MaxInt;
  TLegendItemFillOrder = (lfoColRow, lfoRowCol);

  TChartLegendGridPen = class(TChartPen)
  published
    property Visible default false;
  end;

  { TChartLegend }

  TChartLegend = class(TChartElement)
  strict private
    FAlignment: TLegendAlignment;
    FBackgroundBrush: TChartLegendBrush;
    FColumnCount: TLegendColumnCount;
    FFixedItemWidth: Cardinal;
    FFixedItemHeight: Cardinal;
    FFont: TFont;
    FFrame: TChartPen;
    FGridHorizontal: TChartLegendGridPen;
    FGridVertical: TChartLegendGridPen;
    FGroupFont: TFont;
    FGroupTitles: TStrings;
    FInverted: Boolean;
    FItemFillOrder: TLegendItemFillOrder;
    FLegendRect: TRect;
    FMarginX: TChartDistance;
    FMarginY: TChartDistance;
    FSpacing: TChartDistance;
    FSymbolFrame: TChartPen;
    FSymbolWidth: TChartDistance;
    FTextFormat: TChartTextFormat;
    FTransparency: TChartTransparency;
    FUseSidebar: Boolean;

    // Not includes the margins around item.
    function MeasureItem(
      ADrawer: IChartDrawer; AItems: TChartLegendItems): TPoint;
    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetBackgroundBrush(AValue: TChartLegendBrush);
    procedure SetColumnCount(AValue: TLegendColumnCount);
    procedure SetFixedItemWidth(AValue: Cardinal);
    procedure SetFixedItemHeight(AValue: Cardinal);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetGridHorizontal(AValue: TChartLegendGridPen);
    procedure SetGridVertical(AValue: TChartLegendGridPen);
    procedure SetGroupFont(AValue: TFont);
    procedure SetGroupTitles(AValue: TStrings);
    procedure SetInverted(AValue: Boolean);
    procedure SetItemFillOrder(AValue: TLegendItemFillOrder);
    procedure SetMargin(AValue: TChartDistance);
    procedure SetMarginX(AValue: TChartDistance);
    procedure SetMarginY(AValue: TChartDistance);
    procedure SetSpacing(AValue: TChartDistance);
    procedure SetSymbolFrame(AValue: TChartPen);
    procedure SetSymbolWidth(AValue: TChartDistance);
    procedure SetTextFormat(AValue: TChartTextFormat);
    procedure SetTransparency(AValue: TChartTransparency);
    procedure SetUseSidebar(AValue: Boolean);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
  public
    procedure AddGroups(AItems: TChartLegendItems);
    procedure Assign(Source: TPersistent); override;
    procedure Draw(var AData: TChartLegendDrawingData);
    function IsPointInBounds(APoint: TPoint): Boolean;
    procedure Prepare(var AData: TChartLegendDrawingData; var AClipRect: TRect);
    procedure SortItemsByOrder(AItems: TChartLegendItems);
    procedure UpdateBidiMode;
  published
    property Alignment: TLegendAlignment
      read FAlignment write SetAlignment default laTopRight;
    property BackgroundBrush: TChartLegendBrush
      read FBackgroundBrush write SetBackgroundBrush;
    property ColumnCount: TLegendColumnCount
      read FColumnCount write SetColumnCount default 1;
    property FixedItemWidth: Cardinal
      read FFixedItemWidth write SetFixedItemWidth default 0;
    property FixedItemHeight: Cardinal
      read FFixedItemHeight write SetFixedItemHeight default 0;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property GridHorizontal: TChartLegendGridPen
      read FGridHorizontal write SetGridHorizontal;
    property GridVertical: TChartLegendGridPen
      read FGridVertical write SetGridVertical;
    property GroupFont: TFont read FGroupFont write SetGroupFont;
    property GroupTitles: TStrings read FGroupTitles write SetGroupTitles;
    property Inverted: Boolean read FInverted write SetInverted default false;
    property ItemFillOrder: TLegendItemFillOrder
      read FItemFillOrder write SetItemFillOrder default lfoColRow;
    property MarginX: TChartDistance
      read FMarginX write SetMarginX default DEF_LEGEND_MARGIN;
    property MarginY: TChartDistance
      read FMarginY write SetMarginY default DEF_LEGEND_MARGIN;
    property Spacing: TChartDistance
      read FSpacing write SetSpacing default DEF_LEGEND_SPACING;
    property SymbolFrame: TChartPen read FSymbolFrame write SetSymbolFrame;
    property SymbolWidth: TChartDistance
      read FSymbolWidth write SetSymbolWidth default DEF_LEGEND_SYMBOL_WIDTH;
    property TextFormat: TChartTextFormat
      read FTextFormat write SetTextFormat default tfNormal;
    property Transparency: TChartTransparency
      read FTransparency write SetTransparency default 0;
    property UseSidebar: Boolean read FUseSidebar write SetUseSidebar default true;
    property Visible default false;
  end;

  TLegendMultiplicity = (lmSingle, lmPoint, lmStyle);

  TLegendItemCreateEvent = procedure (
    AItem: TLegendItem; AIndex: Integer) of object;

  { TChartSeriesLegend }

  TChartSeriesLegend = class(TChartElement)
  strict private
    FFormat: String;
    FGroupIndex: Integer;
    FMultiplicity: TLegendMultiplicity;
    FOnCreate: TLegendItemCreateEvent;
    FOnDraw: TLegendItemDrawEvent;
    FOrder: Integer;
    FTextFormat: TChartTextFormat;
    FUserItemsCount: Integer;
    procedure SetFormat(AValue: String);
    procedure SetGroupIndex(AValue: Integer);
    procedure SetMultiplicity(AValue: TLegendMultiplicity);
    procedure SetOnCreate(AValue: TLegendItemCreateEvent);
    procedure SetOnDraw(AValue: TLegendItemDrawEvent);
    procedure SetOrder(AValue: Integer);
    procedure SetTextFormat(AValue: TChartTextFormat);
    procedure SetUserItemsCount(AValue: Integer);
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(Source: TPersistent); override;
    procedure InitItem(
      AItem: TLegendItem; AIndex: Integer; ALegend: TChartLegend);
  published
    property Format: String read FFormat write SetFormat;
    property GroupIndex: Integer
      read FGroupIndex write SetGroupIndex default LEGEND_ITEM_NO_GROUP;
    property Multiplicity: TLegendMultiplicity
      read FMultiplicity write SetMultiplicity default lmSingle;
    property Order: Integer
      read FOrder write SetOrder default LEGEND_ITEM_ORDER_AS_ADDED;
    property TextFormat: TChartTextFormat
      read FTextFormat write SetTextFormat default tfNormal;
    property UserItemsCount: Integer
      read FUserItemsCount write SetUserItemsCount default 1;
    property Visible default true;

  published
    property OnCreate: TLegendItemCreateEvent read FOnCreate write SetOnCreate;
    property OnDraw: TLegendItemDrawEvent read FOnDraw write SetOnDraw;
  end;

implementation

uses
  Math, PropEdits, Types, LResources,
  TADrawerCanvas, TAGeometry;

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

function LegendItemCompare_Inverted(AItem1, AItem2: Pointer): Integer;
var
  li1: TLegendItem absolute AItem1;
  li2: TLegendItem absolute AItem2;
begin
  Result := Sign(li1.GroupIndex - li2.GroupIndex);
  if Result = 0 then
    Result := Sign(li2.Order - li1.Order);
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
var
  symTextSpc: Integer;
begin
  symTextSpc := ADrawer.Scale(SYMBOL_TEXT_SPACING);
  if ADrawer.GetRightToLeft then
    ADrawer.TextOut.
      TextFormat(FTextFormat).
      Pos(ARect.Left - symTextSpc - ADrawer.TextExtent(FText, FTextFormat).X, ARect.Top).
      Text(FText).Done
  else
    ADrawer.TextOut.
      TextFormat(FTextFormat).
      Pos(ARect.Right + symTextSpc, ARect.Top).
      Text(FText).Done;
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
  if ADrawer.GetRightToLeft then
    ADrawer.TextOut.
      TextFormat(TextFormat).
      Pos(ARect.Right - ADrawer.TextExtent(Text, TextFormat).X, ARect.Top).
      Text(Text).Done
  else
    ADrawer.TextOut.
      TextFormat(TextFormat).
      Pos(ARect.Left, ARect.Top).
      Text(Text).Done;
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

constructor TLegendItemLinePointer.CreateWithBrush(
  APen: TPen; ABrush: TBrush; APointer: TSeriesPointer; const AText: String);
begin
  Create(APen, APointer, AText);
  FBrush := ABrush;
  FPresetBrush := true;
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
  sz.X := Min(ADrawer.Scale(FPointer.HorizSize), (ARect.Right - ARect.Left) div 3);
  sz.Y := Min(ADrawer.Scale(FPointer.VertSize), (ARect.Bottom - ARect.Top) div 2);
  if FPresetBrush then
    ADrawer.SetBrush(FBrush);
  FPointer.DrawSize(ADrawer, c, sz, Color, 0.0, FPresetBrush);
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
      g.TextFormat := FTextFormat;
      AItems.Insert(i, g);
    end;
  end;
end;

procedure TChartLegend.Assign(Source: TPersistent);
begin
  if Source is TChartLegend then
    with TChartLegend(Source) do begin
      Self.FAlignment := Alignment;
      Self.FBackgroundBrush.Assign(BackgroundBrush);
      Self.FColumnCount := ColumnCount;
      Self.FFixedItemWidth := FixedItemWidth;
      Self.FFixedItemHeight := FixedItemHeight;
      Self.FFont.Assign(Font);
      Self.FFrame.Assign(Frame);
      Self.FGridHorizontal.Assign(GridHorizontal);
      Self.FGridVertical.Assign(GridVertical);
      Self.FGroupFont.Assign(GroupFont);
      Self.FGroupTitles.Assign(GroupTitles);
      Self.FMarginX := MarginX;
      Self.FMarginY := MarginY;
      Self.FSpacing := Spacing;
      Self.FSymbolFrame.Assign(SymbolFrame);
      Self.FSymbolWidth := SymbolWidth;
      Self.FTextFormat := TextFormat;
      Self.FUseSidebar := UseSidebar;
    end;

  inherited Assign(Source);
end;

constructor TChartLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FAlignment := laTopRight;
  FColumnCount := 1;
  FGridHorizontal := TChartLegendGridPen.Create;
  FGridHorizontal.OnChange := @StyleChanged;
  FGridVertical := TChartLegendGridPen.Create;
  FGridVertical.OnChange := @StyleChanged;
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
  FreeAndNil(FGridHorizontal);
  FreeAndNil(FGridVertical);
  FreeAndNil(FGroupFont);
  FreeAndNil(FGroupTitles);
  FreeAndNil(FSymbolFrame);

  inherited;
end;

procedure TChartLegend.Draw(var AData: TChartLegendDrawingData);
var
  drawer: IChartDrawer;

  procedure DrawItems;
  var
    i, x, y: Integer;
    prevFont: TFont = nil;
    r: TRect;
    isRTL: Boolean;
    space, symwid: Integer;
  begin
    isRTL := drawer.GetRightToLeft;
    with AData do begin
      space := FDrawer.Scale(Spacing);
      symwid := FDrawer.Scale(SymbolWidth);
      for i := 0 to FItems.Count - 1 do begin
        FItems[i].TextFormat := FTextFormat;
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
        if isRTL then
          r := Bounds(
            FBounds.Right - space - x * (FItemSize.X + space) - symwid,
            FBounds.Top + space + y * (FItemSize.Y + space),
            symwid,
            FItemSize.Y)
        else
          r := Bounds(
            FBounds.Left + space + x * (FItemSize.X + space),
            FBounds.Top + space + y * (FItemSize.Y + space),
            symwid,
            FItemSize.Y);
        FItems[i].Draw(drawer, r);
        OffsetRect(r, 0, FItemSize.Y + space);
      end;
      if GridHorizontal.EffVisible then begin
        drawer.Pen := GridHorizontal;
        drawer.SetBrushParams(bsClear, clTAColor);
        for i := 1 to FRowCount - 1 do begin
          y := FBounds.Top + space div 2 + i * (FItemSize.Y + space);
          drawer.Line(FBounds.Left, y, FBounds.Right, y);
        end;
      end;
      if GridVertical.EffVisible then begin
        drawer.Pen := GridVertical;
        drawer.SetBrushParams(bsClear, clTAColor);
        for i := 1 to FColCount - 1 do begin
          x := FBounds.Left + space div 2 + i * (FItemSize.X + space);
          drawer.Line(x, FBounds.Top, x, FBounds.Bottom);
        end;
      end;
    end;
  end;

var
  r: TRect;
begin
  drawer := AData.FDrawer;
  drawer.SetTransparency(Transparency);
  try
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
    try
      DrawItems;
    finally
      drawer.ClippingStop;
    end;
  finally
    drawer.SetTransparency(0);
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
  if (FixedItemWidth <= 0) or (FixedItemHeight <= 0) then
    for li in AItems do begin
      li.UpdateFont(ADrawer, prevFont);
      if li.Text = '' then
        p := Point(0, ADrawer.TextExtent('I', FTextFormat).Y)
      else
        p := ADrawer.TextExtent(li.Text, FTextFormat);
      if li.HasSymbol then
        p.X += ADrawer.Scale(SYMBOL_TEXT_SPACING + SymbolWidth);
      Result := MaxPoint(p, Result);
    end;
  if FixedItemWidth > 0 then
    Result.X := ADrawer.Scale(FixedItemWidth);
  if FixedItemHeight > 0 then
    Result.Y := ADrawer.Scale(FixedItemHeight);
end;

function TChartLegend.IsPointInBounds(APoint: TPoint): Boolean;
begin
  Result := IsPointInRect(APoint, FLegendRect);
end;

procedure TChartLegend.Prepare(
  var AData: TChartLegendDrawingData; var AClipRect: TRect);
var
  x, y: Integer;
  sidebar, legendSize: TPoint;
  margX, margY, space: Integer;
begin
  with AData do begin
    margX := FDrawer.Scale(MarginX);
    margY := FDrawer.Scale(MarginY);
    space := FDrawer.Scale(Spacing);
    FColCount := Max(Min(ColumnCount, FItems.Count), 1);
    FRowCount := (FItems.Count - 1) div FColCount + 1;
    FItemSize := MeasureItem(FDrawer, FItems);
    legendSize.X := (FItemSize.X + space) * FColCount + space;
    legendSize.Y := (FItemSize.Y + space) * FRowCount + space;
  end;

  sidebar.X := 2 * margX;
  with AClipRect do
    legendSize.X := EnsureRange(legendSize.X, 0, Right - Left - sidebar.X);
  sidebar.X += legendSize.X;

  sidebar.Y := 2 * margX;
  with AClipRect do
    legendSize.Y := EnsureRange(legendSize.Y, 0, Bottom - Top - sidebar.Y);
  sidebar.Y += legendSize.Y;

  // Determine position according to the alignment.
  case Alignment of
    laTopLeft, laCenterLeft, laBottomLeft:
      x := AClipRect.Left + margX;
    laTopRight, laCenterRight, laBottomRight:
      x := AClipRect.Right - legendSize.X - margX;
    laTopCenter, laBottomCenter:
      x := (AClipRect.Right + AClipRect.Left - legendSize.X) div 2;
  end;
  case Alignment of
    laTopLeft, laTopCenter, laTopRight:
      y := AClipRect.Top + margY;
    laBottomLeft, laBottomCenter, laBottomRight:
      y := AClipRect.Bottom - margY - legendSize.Y;
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
        AClipRect.Top += legendSize.Y + 2 * margY;
      laBottomCenter:
        AClipRect.Bottom -= legendSize.Y + 2 * margY;
    end;
  AData.FBounds := Bounds(x, y, legendSize.X, legendSize.Y);
  FLegendRect := Rect(x, y, x + legendSize.X, y + legendSize.Y);
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

procedure TChartLegend.SetFixedItemWidth(AValue: Cardinal);
begin
  if FFixedItemWidth = AValue then exit;
  FFixedItemWidth := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetFixedItemHeight(AValue: Cardinal);
begin
  if FFixedItemHeight = AValue then exit;
  FFixedItemHeight := AValue;
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

procedure TChartLegend.SetGridHorizontal(AValue: TChartLegendGridPen);
begin
  if FGridHorizontal = AValue then exit;
  FGridHorizontal.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetGridVertical(AValue: TChartLegendGridPen);
begin
  if FGridVertical = AValue then exit;
  FGridVertical.Assign(AValue);
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

procedure TChartLegend.SetInverted(AValue: Boolean);
begin
  if FInverted = AValue then exit;
  FInverted := AValue;
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

procedure TChartLegend.SetTextFormat(AValue: TChartTextFormat);
begin
  if FTextFormat = AValue then exit;
  FTextFormat := AValue;
  StyleChanged(self);
end;

procedure TChartLegend.SetTransparency(AValue: TChartTransparency);
begin
  if FTransparency = AValue then exit;
  FTransparency := AValue;
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
  if FInverted then
    AItems.Sort(@LegendItemCompare_Inverted)
  else
    AItems.Sort(@LegendItemCompare);
end;

procedure TChartLegend.UpdateBidiMode;
begin
  case Alignment of
    laTopLeft     : Alignment := laTopRight;
    laCenterLeft  : Alignment := laCenterRight;
    laBottomLeft  : Alignment := laBottomRight;
    laTopRight    : Alignment := laTopLeft;
    laCenterRight : Alignment := laCenterLeft;
    laBottomRight : Alignment := laBottomLeft;
  end;
end;


{ TChartSeriesLegend }

procedure TChartSeriesLegend.Assign(Source: TPersistent);
begin
  if Source is TChartSeriesLegend then
    with TChartSeriesLegend(Source) do begin
      Self.FFormat := FFormat;
      Self.FGroupIndex := FGroupIndex;
      Self.FMultiplicity := FMultiplicity;
      Self.FOnDraw := FOnDraw;
      Self.FOrder := FOrder;
      Self.FTextFormat := FTextFormat;
      Self.FUserItemsCount := FUserItemsCount;
      Self.FVisible := FVisible;
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
  AItem.TextFormat := ALegend.TextFormat;
end;

procedure TChartSeriesLegend.SetFormat(AValue: String);
begin
  if FFormat = AValue then exit;
  FFormat := AValue;
  StyleChanged(Self);
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

procedure TChartSeriesLegend.SetTextFormat(AValue: TChartTextFormat);
begin
  if FTextFormat = AValue then exit;
  FTextFormat := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetUserItemsCount(AValue: Integer);
begin
  if FUserItemsCount = AValue then exit;
  FUserItemsCount := AValue;
  StyleChanged(Self);
end;

procedure SkipObsoleteProperties;
const
  MARGIN_NOTE = 'Obsolete, use Legend.MarginX instead';
begin
  RegisterPropertyToSkip(TChartLegend, 'Margin', MARGIN_NOTE, '');
end;

initialization
  SkipObsoleteProperties;

end.


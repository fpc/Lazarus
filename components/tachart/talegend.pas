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
  Classes, Contnrs, SysUtils, Graphics, TAChartUtils, TATypes;

const
  DEF_LEGEND_SPACING = 4;
  DEF_LEGEND_MARGIN = 4;
  DEF_LEGEND_SYMBOL_WIDTH = 20;

type
  { TLegendItem }

  TLegendItem = class
  private
    FColor: TColor;
    FText: String;
  public
    constructor Create(const AText: String; AColor: TColor = clTAColor);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); virtual;
  public
    property Color: TColor read FColor write FColor;
  end;

  TLegendItemDrawEvent = procedure (
    ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; var AText: String
  ) of object;

  { TLegendItemUserDrawn }

  TLegendItemUserDrawn = class(TLegendItem)
  private
    FIndex: Integer;
    FOnDraw: TLegendItemDrawEvent;
  public
    constructor Create(
      AIndex: Integer; AOnDraw: TLegendItemDrawEvent; const AText: String);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    property OnDraw: TLegendItemDrawEvent read FOnDraw;
  end;

  { TLegendItemLine }

  TLegendItemLine = class(TLegendItem)
  private
    FPen: TPen;
  public
    constructor Create(APen: TPen; const AText: String);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    property Pen: TPen read FPen;
  end;

  { TLegendItemLinePointer }

  TLegendItemLinePointer = class(TLegendItemLine)
  protected
    FPointer: TSeriesPointer;
  public
    constructor Create(
      APen: TPen; APointer: TSeriesPointer; const AText: String);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
  end;

  { TLegendItemBrushRect }

  TLegendItemBrushRect = class(TLegendItem)
  private
    FBrush: TBrush;
  public
    constructor Create(ABrush: TBrush; const AText: String);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
  end;

  { TLegendItemPieSlice }

  TLegendItemPieSlice = class(TLegendItem)
  public
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
  end;

  TChartLegendItems = TObjectList;

  TChartLegendBrush = class(TBrush)
  published
    property Color default clWhite;
  end;

  TLegendAlignment = (laTopLeft, laBottomLeft, laTopRight, laBottomRight);

  { TChartLegend }

  TChartLegend = class(TChartElement)
  private
    FAlignment: TLegendAlignment;
    FBackgroundBrush: TChartLegendBrush;
    FFont: TFont;
    FFrame: TChartPen;
    FMarginX: TChartDistance;
    FMarginY: TChartDistance;
    FSpacing: TChartDistance;
    FSymbolWidth: TChartDistance;
    FUseSidebar: Boolean;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetBackgroundBrush(AValue: TChartLegendBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetMargin(AValue: TChartDistance);
    procedure SetMarginX(AValue: TChartDistance);
    procedure SetMarginY(AValue: TChartDistance);
    procedure SetSpacing(AValue: TChartDistance);
    procedure SetSymbolWidth(AValue: TChartDistance);
    procedure SetUseSidebar(AValue: Boolean);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

  public
    procedure Assign(Source: TPersistent); override;
    procedure Draw(
      ACanvas: TCanvas; AItems: TObjectList; const ABounds: TRect);
    function Prepare(
      ACanvas: TCanvas; AItems: TObjectList; var AClipRect: TRect): TRect;
  published
    property Alignment: TLegendAlignment
      read FAlignment write SetAlignment default laTopRight;
    property BackgroundBrush: TChartLegendBrush
      read FBackgroundBrush write SetBackgroundBrush;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Margin: TChartDistance
      read FMarginX write SetMargin stored false; deprecated;
    property MarginX: TChartDistance
      read FMarginX write SetMarginX default DEF_LEGEND_MARGIN;
    property MarginY: TChartDistance
      read FMarginY write SetMarginY default DEF_LEGEND_MARGIN;
    property Spacing: TChartDistance
      read FSpacing write SetSpacing default DEF_LEGEND_SPACING;
    property SymbolWidth: TChartDistance
      read FSymbolWidth write SetSymbolWidth default DEF_LEGEND_SYMBOL_WIDTH;
    property UseSidebar: Boolean read FUseSidebar write SetUseSidebar default true;
    property Visible default false;
  end;

  TLegendMultiplicity = (lmSingle, lmPoint);

  { TChartSeriesLegend }

  TChartSeriesLegend = class(TChartElement)
  private
    FMultiplicity: TLegendMultiplicity;
    FOnDraw: TLegendItemDrawEvent;
    FUserItemsCount: Integer;
    procedure SetMultiplicity(AValue: TLegendMultiplicity);
    procedure SetOnDraw(AValue: TLegendItemDrawEvent);
    procedure SetUserItemsCount(AValue: Integer);
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Multiplicity: TLegendMultiplicity
      read FMultiplicity write SetMultiplicity default lmSingle;
    property OnDraw: TLegendItemDrawEvent read FOnDraw write SetOnDraw;
    property UserItemsCount: Integer
      read FUserItemsCount write SetUserItemsCount default 1;
    property Visible default true;
  end;

implementation

uses
  Math, PropEdits, Types, TADrawUtils;

const
  SYMBOL_TEXT_SPACING = 4;

{ TLegendItem }

constructor TLegendItem.Create(const AText: String; AColor: TColor);
begin
  FColor := AColor;
  FText := AText;
end;

procedure TLegendItem.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.TextOut(ARect.Right + SYMBOL_TEXT_SPACING, ARect.Top, FText);
end;

{ TLegendItemUserDrawn }

constructor TLegendItemUserDrawn.Create(
  AIndex: Integer; AOnDraw: TLegendItemDrawEvent; const AText: String);
begin
  inherited Create(AText);
  FIndex := AIndex;
  FOnDraw := AOnDraw;
end;

procedure TLegendItemUserDrawn.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if Assigned(FOnDraw) then
    FOnDraw(ACanvas, ARect, FIndex, FText);
  inherited Draw(ACanvas, ARect);
end;

{ TLegendItemLine }

constructor TLegendItemLine.Create(APen: TPen; const AText: String);
begin
  inherited Create(AText);
  FPen := APen;
end;

procedure TLegendItemLine.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  y: Integer;
begin
  inherited Draw(ACanvas, ARect);
  if FPen = nil then exit;
  ACanvas.Pen.Assign(FPen);
  y := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.Line(ARect.Left, y, ARect.Right, y);
end;

{ TLegendItemLinePointer }

constructor TLegendItemLinePointer.Create(
  APen: TPen; APointer: TSeriesPointer; const AText: String);
begin
  inherited Create(APen, AText);
  FPointer := APointer;
end;

procedure TLegendItemLinePointer.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  c, sz: TPoint;
begin
  inherited Draw(ACanvas, ARect);
  if FPointer = nil then exit;
  c := CenterPoint(ARect);
  // Max width slightly narrower then ARect to leave place for the line.
  sz.X := Min(FPointer.HorizSize, (ARect.Right - ARect.Left) div 3);
  sz.Y := Min(FPointer.VertSize, (ARect.Bottom - ARect.Top) div 2);
  FPointer.DrawSize(ACanvas, c, sz, Color);
end;

{ TLegendItemBrushRect }

constructor TLegendItemBrushRect.Create(ABrush: TBrush; const AText: String);
begin
  inherited Create(AText);
  FBrush := ABrush;
end;

procedure TLegendItemBrushRect.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  inherited Draw(ACanvas, ARect);
  if FBrush = nil then
    ACanvas.Brush.Style := bsSolid
  else
    ACanvas.Brush.Assign(FBrush);
  if Color <> clTAColor then
    ACanvas.Brush.Color := Color;
  ACanvas.Rectangle(ARect);
end;

{ TLegendItemPieSlice }

procedure TLegendItemPieSlice.Draw(ACanvas: TCanvas; const ARect: TRect);
const
  ANGLE = 30 * 16;
begin
  inherited Draw(ACanvas, ARect);
  ACanvas.Brush.Style := bsSolid;
  if Color <> clTAColor then
    ACanvas.Brush.Color := Color;
  ACanvas.RadialPie(
    2 * ARect.Left - ARect.Right, ARect.Top, ARect.Right, ARect.Bottom,
    -ANGLE, 2 * ANGLE);
end;

{ TChartLegend }

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
  FMarginX := DEF_LEGEND_MARGIN;
  FMarginY := DEF_LEGEND_MARGIN;
  FSpacing := DEF_LEGEND_SPACING;
  FSymbolWidth := DEF_LEGEND_SYMBOL_WIDTH;
  FUseSidebar := true;
  Visible := false;

  InitHelper(FBackgroundBrush, TChartLegendBrush);
  InitHelper(FFont, TFont);
  InitHelper(FFrame, TChartPen);
end;

destructor TChartLegend.Destroy;
begin
  FreeAndNil(FBackgroundBrush);
  FreeAndNil(FFont);
  FreeAndNil(FFrame);

  inherited;
end;

procedure TChartLegend.Draw(
  ACanvas: TCanvas; AItems: TObjectList; const ABounds: TRect);
var
  i, h: Integer;
  pbf: TPenBrushFontRecall;
  r: TRect;
begin
  pbf := TPenBrushFontRecall.Create(ACanvas, [pbfPen, pbfBrush, pbfFont]);
  try
    // Draw the background and the border.
    ACanvas.Font.Assign(Font);
    ACanvas.Brush.Assign(BackgroundBrush);
    if Frame.Visible then
      ACanvas.Pen.Assign(Frame)
    else
      ACanvas.Pen.Style := psClear;
    ACanvas.Rectangle(ABounds);

    r := ABounds;
    r.Right -= 1;
    ACanvas.ClipRect :=  r;
    ACanvas.Clipping := true;
    // Draw items.
    h := TypicalTextHeight(ACanvas);
    r := Bounds(ABounds.Left + Spacing, ABounds.Top + Spacing, SymbolWidth, h);
    for i := 0 to AItems.Count - 1 do begin
      ACanvas.Brush.Assign(BackgroundBrush);
      ACanvas.Pen.Assign(Frame);
      (AItems[i] as TLegendItem).Draw(ACanvas, r);
      OffsetRect(r, 0, h + Spacing);
    end;
  finally
    pbf.Free;
    ACanvas.Clipping := false;
  end;
end;

function TChartLegend.Prepare(
  ACanvas: TCanvas; AItems: TObjectList; var AClipRect: TRect): TRect;
var
  w, x, y, i, textHeight, legendWidth, legendHeight: Integer;
  f: TPenBrushFontRecall;
begin
  f := TPenBrushFontRecall.Create(ACanvas, [pbfFont]);
  try
    ACanvas.Font.Assign(Font);

    // Measure the legend.
    legendWidth := 0;
    for i := 0 to AItems.Count - 1 do
      with AItems[i] as TLegendItem do
        legendWidth := Max(ACanvas.TextWidth(FText), legendWidth);
    legendWidth += 2 * Spacing + SYMBOL_TEXT_SPACING + SymbolWidth;
    w := 2 * MarginX;
    with AClipRect do
      legendWidth := EnsureRange(legendWidth, 0, Right - Left - w);
    w += legendWidth;

    textHeight := TypicalTextHeight(ACanvas);
    legendHeight := Spacing + AItems.Count * (textHeight + Spacing);

    // Determine position according to the alignment.
    if Alignment in [laTopLeft, laBottomLeft] then begin
      x := AClipRect.Left + MarginX;
      if UseSidebar then
        AClipRect.Left += w;
    end
    else begin
      x := AClipRect.Right - legendWidth - MarginX;
      if UseSidebar then
        AClipRect.Right -= w;
    end;
    if Alignment in [laTopLeft, laTopRight] then
      y := AClipRect.Top + MarginY
    else
      y := AClipRect.Bottom - MarginY - legendHeight;

    Result := Bounds(x, y, legendWidth, legendHeight);
  finally
    f.Free;
  end;
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

{ TChartSeriesLegend }

procedure TChartSeriesLegend.Assign(Source: TPersistent);
begin
  if Source is TChartSeriesLegend then
    with TChartSeriesLegend(Source) do begin
      Self.FMultiplicity := FMultiplicity;
      Self.FOnDraw := FOnDraw;
      Self.FVisible := FVisible;
    end;

  inherited Assign(Source);
end;

constructor TChartSeriesLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FVisible := true;
  FUserItemsCount := 1;
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


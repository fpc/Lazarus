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
    FText: String;
  public
    constructor Create(const AText: String);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); virtual;
  end;

  { TLegendItemLine }

  TLegendItemLine = class(TLegendItem)
  private
    FPen: TPen;
  public
    constructor Create(APen: TPen; const AText: String);
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

  { TLegendItemColorRect }

  TLegendItemColorRect = class(TLegendItem)
  private
    FColor: TColor;
  public
    constructor Create(AColor: TColor; const AText: String);
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
    FMargin: TChartDistance;
    FSpacing: TChartDistance;
    FSymbolWidth: TChartDistance;
    FUseSidebar: Boolean;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetBackgroundBrush(AValue: TChartLegendBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetMargin(AValue: TChartDistance);
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
      read FMargin write SetMargin default DEF_LEGEND_MARGIN;
    property Spacing: TChartDistance
      read FSpacing write SetSpacing default DEF_LEGEND_SPACING;
    property SymbolWidth: TChartDistance
      read FSymbolWidth write SetSymbolWidth default DEF_LEGEND_SYMBOL_WIDTH;
    property UseSidebar: Boolean read FUseSidebar write SetUseSidebar default true;
    property Visible default false;
  end;

implementation

uses
  FPCanvas, Math, Types;

const
  SYMBOL_TEXT_SPACING = 4;

{ TLegendItem }

constructor TLegendItem.Create(const AText: String);
begin
  FText := AText;
end;

procedure TLegendItem.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.TextOut(ARect.Right + SYMBOL_TEXT_SPACING, ARect.Top, FText);
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
  ACanvas.Pen.Assign(FPen);
  y := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.Line(ARect.Left, y, ARect.Right, y);
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
  ACanvas.Brush.Assign(FBrush);
  ACanvas.Rectangle(ARect);
end;

{ TLegendItemColorRect }

constructor TLegendItemColorRect.Create(AColor: TColor; const AText: String);
begin
  inherited Create(AText);
  FColor := AColor;
end;

procedure TLegendItemColorRect.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  inherited Draw(ACanvas, ARect);
  ACanvas.Brush.Color := FColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Rectangle(ARect);
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
  FMargin := DEF_LEGEND_MARGIN;
  FSpacing := DEF_LEGEND_SPACING;
  FSymbolWidth := DEF_LEGEND_SYMBOL_WIDTH;
  FUseSidebar := true;
  Visible := false;

  InitHelper(TFPCanvasHelper(FBackgroundBrush), TChartLegendBrush);
  InitHelper(TFPCanvasHelper(FFont), TFont);
  InitHelper(TFPCanvasHelper(FFrame), TChartPen);
end;

destructor TChartLegend.Destroy;
begin
  FFont.Free;
  FFrame.Free;

  inherited;
end;

procedure TChartLegend.Draw(
  ACanvas: TCanvas; AItems: TObjectList; const ABounds: TRect);
var
  i, h: Integer;
  pbf: TPenBrushFontRecall;
  r: TRect;
begin
  if not Visible then exit;
  pbf := TPenBrushFontRecall.Create(ACanvas, [pbfPen, pbfBrush, pbfFont]);
  try
    // Draw the background and the border.
    ACanvas.Font.Assign(Font);
    ACanvas.Brush.Assign(BackgroundBrush);
    ACanvas.Pen.Assign(Frame);
    ACanvas.Rectangle(ABounds);

    // Draw items.
    h := ACanvas.TextHeight('Iy');
    r := Bounds(ABounds.Left + Spacing, ABounds.Top + Spacing, SymbolWidth, h);
    for i := 0 to AItems.Count - 1 do begin
      ACanvas.Brush.Assign(BackgroundBrush);
      ACanvas.Pen.Assign(Frame);
      (AItems[i] as TLegendItem).Draw(ACanvas, r);
      OffsetRect(r, 0, h + Spacing);
    end;
  finally
    pbf.Free;
  end;
end;

function TChartLegend.Prepare(
  ACanvas: TCanvas; AItems: TObjectList; var AClipRect: TRect): TRect;
var
  w, x, y, i, textHeight, legendWidth, legendHeight: Integer;
  f: TPenBrushFontRecall;
begin
  if not Visible then exit;

  f := TPenBrushFontRecall.Create(ACanvas, [pbfFont]);
  try
    ACanvas.Font.Assign(Font);

    // Measure the legend.
    legendWidth := 0;
    for i := 0 to AItems.Count - 1 do
      with AItems[i] as TLegendItem do
        legendWidth := Max(ACanvas.TextWidth(FText), legendWidth);
    legendWidth += 2 * Spacing + SYMBOL_TEXT_SPACING + SymbolWidth;
    textHeight := ACanvas.TextHeight('Iy');
    legendHeight := Spacing + AItems.Count * (textHeight + Spacing);
    w := legendWidth + 2 * Margin;

    // Determine position according to the alignment.
    if Alignment in [laTopLeft, laBottomLeft] then begin
      x := AClipRect.Left + Margin;
      if UseSidebar then
        AClipRect.Left += w;
    end
    else begin
      x := AClipRect.Right - legendWidth - Margin;
      if UseSidebar then
        AClipRect.Right -= w;
    end;
    if Alignment in [laTopLeft, laTopRight] then
      y := AClipRect.Top + Margin
    else
      y := AClipRect.Bottom - Margin - legendHeight;

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
  if FMargin = AValue then exit;
  FMargin := AValue;
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

end.


unit TALegend;

{$H+}

interface

uses
  Classes, Contnrs, SysUtils, Graphics, TAChartUtils, TATypes;

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

  { TChartLegend }

  TChartLegend = class(TChartElement)
  private
    FAlignment: TLegendAlignment;
    FFont: TFont;
    FFrame: TChartPen;
    FSpacing: TChartDistance;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetSpacing(const AValue: TChartDistance);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

  public
    procedure Assign(Source: TPersistent); override;
    procedure Draw(
      AItems: TObjectList; ACanvas: TCanvas; var AClipRect: TRect);
  published
    property Alignment: TLegendAlignment
      read FAlignment write SetAlignment default laRight;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Spacing: TChartDistance read FSpacing write SetSpacing default 5;
    property Visible default false;
  end;

implementation

uses
  FPCanvas, Math, Types;

const
  ICON_WIDTH = 17;
  ICON_SPACING = 3;
  MARGINS = 3;

{ TLegendItem }

constructor TLegendItem.Create(const AText: String);
begin
  FText := AText;
end;

procedure TLegendItem.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.TextOut(ARect.Right + ICON_SPACING, ARect.Top, FText);
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
  FAlignment := laRight;
  FSpacing := 5;
  Visible := false;

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
  AItems: TObjectList; ACanvas: TCanvas; var AClipRect: TRect);
var
  w, x1, y1, i, th: Integer;
  pbf: TPenBrushFontRecall;
  r: TRect;
begin
  if not Visible then exit;

  // TODO: Legend.Alignment

  pbf := TPenBrushFontRecall.Create(ACanvas, [pbfPen, pbfBrush, pbfFont]);
  try
    ACanvas.Font.Assign(Font);
    w := 0;
    for i := 0 to AItems.Count - 1 do
      with AItems[i] as TLegendItem do
        w := Max(ACanvas.TextWidth(FText), w);
    w += 2 * Spacing + ICON_WIDTH + ICON_SPACING;
    th := ACanvas.TextHeight('Iy');

    AClipRect.Right -= w + 2 * MARGINS;
    x1 := AClipRect.Right + MARGINS;
    y1 := AClipRect.Top;

    // Border
    ACanvas.Brush.Color := clWhite;
    ACanvas.Pen.Assign(Frame);
    ACanvas.Rectangle(Bounds(
      x1, y1, w, Spacing + AItems.Count * (th + Spacing)));

    r := Bounds(x1 + Spacing, y1 + Spacing, ICON_WIDTH, th);
    for i := 0 to AItems.Count - 1 do begin
      ACanvas.Brush.Color := clWhite;
      ACanvas.Pen.Assign(Frame);
      (AItems[i] as TLegendItem).Draw(ACanvas, r);
      OffsetRect(r, 0, th + Spacing);
    end;
  finally
    pbf.Free;
  end;
end;

procedure TChartLegend.SetAlignment(AValue: TLegendAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
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

procedure TChartLegend.SetSpacing(const AValue: TChartDistance);
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  StyleChanged(Self);
end;

end.


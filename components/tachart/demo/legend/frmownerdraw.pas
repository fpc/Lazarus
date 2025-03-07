unit frmOwnerDraw;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  SysUtils, Classes, Math,
  Forms, Graphics, Controls,
  TAGraph, TAChartUtils, TASeries, TALegend, TADrawUtils;

type
  TOwnerDrawFrame = class(TFrame)
    Chart: TChart;
    GradientLineSeries: TLineSeries;
    procedure ChartDrawLegend(ASender: TChart; ADrawer: IChartDrawer;
      {%H-}ALegendItems: TChartLegendItems; {%H-}ALegendItemSize: TPoint; const
      ALegendRect: TRect; {%H-}AColCount, {%H-}ARowCount: Integer);
  private
    FGradientMinValue: Double;
    FGradientMaxValue: Double;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

const
  START_COLOR = $007777FF;
  END_COLOR = $00FF7777;

{ Helper for ownerdrawn legend }
procedure DrawGradient(ADrawer: IChartDrawer; ARect: TRect;
  AStartColor, AEndColor: TColor);
var
  h: Integer;
  y: Integer;
  c: TColor;
begin
  h := ARect.Bottom - ARect.Top;
  if h <= 0 then exit;
  for y := ARect.Bottom-1 downto ARect.Top do begin
    c := InterpolateRGB(AStartColor, AEndColor, (ARect.Bottom - y) / h);
    ADrawer.SetPenParams(psSolid, c);
    ADrawer.Line(ARect.Left, y, ARect.Right, y);
  end;
end;

constructor TOwnerDrawFrame.Create(AOwner: TComponent);

  procedure PrepareData;
  const
    XMIN = -10;
    XMAX = +10;
    N = 30;
  var
    i: Integer;
    x, y: Double;
    ynorm: Double;
    c: TColor;
  begin
    // Create some data and store in series' internal listsource
    for i:=0 to N-1 do begin
      x := XMIN + (XMAX - XMIN) * i / (N-1) + (random - 0.5) * 0.5;
      y := exp(-0.2*sqr(x)) + (random-0.5) * 0.1;
      GradientLineSeries.AddXY(x, y);
    end;
    // Here we define the value range mapped to the gradient
    FGradientMinValue := GradientLineSeries.ListSource.Extent.a.y;
    FGradientMaxValue := GradientLineSeries.ListSource.Extent.b.y;
    // Colorize the data points
    for i:=0 to N-1 do begin
      y := GradientLineSeries.ListSource.Item[i]^.Y;
      ynorm := (y - FGradientMinValue) / (FGradientMaxValue - FGradientMinValue);
      c := InterpolateRGB(START_COLOR, END_COLOR, ynorm);
      GradientLineSeries.ListSource.Item[i]^.Color := c;
    end;
  end;

begin
  inherited;
  PrepareData;
end;

{ This event handler draws the legend completely on its own.
  You can draw anything here - it's your responsibility...
  Here we draw a color gradient to explain the symbol colors of the datapoints. }
procedure TOwnerDrawFrame.ChartDrawLegend(ASender: TChart; ADrawer:
  IChartDrawer; ALegendItems: TChartLegendItems; ALegendItemSize: TPoint;
  const ALegendRect: TRect; AColCount, ARowCount: Integer);
var
  xg1, xg2, yg1, yg2, y: Integer;
  s: String;
  yval: Double;
  i: Integer;
  ts: TPoint;
begin
  xg1 := ALegendRect.Left + 4;              // left edge of gradient
  xg2 := xg1 + ASender.Legend.SymbolWidth;  // right edge of gradient
  yg1 := ASender.ClipRect.Top;              // top edge of gradient
  yg2 := ASender.ClipRect.Bottom;           // bottom edge of gradient

  // Draw border around gradient bar
  ADrawer.SetPenParams(psSolid, clBlack);
  ADrawer.Rectangle(xg1-1, yg1-1, xg2+1, yg2+1);

  // Draw gradient bar
  DrawGradient(ADrawer, Rect(xg1, yg1, xg2, yg2), START_COLOR, END_COLOR);

  // Draw axis labels along gradient bar, with a short marker line
  ADrawer.SetBrushParams(bsSolid, clBlack);
  ADrawer.SetPenParams(psSolid, clBlack);
  ADrawer.SetFont(ASender.Legend.Font);
  ts := ADrawer.TextExtent('1');
  for i:=0 to ASender.LeftAxis.ValueCount-1 do begin
    // Read y axis labels
    yval := ASender.LeftAxis.Value[i].FValue;
    // make sure that label is visible
    if InRange(yval, FGradientMinValue, FGradientMaxValue) then begin
      s := Format('%.1f', [yval]);
      { or:
      s := ASender.LeftAxis.Value[i].FText; }
      y := ASender.YGraphToImage(yval);
      ADrawer.Line(xg2-2, y, xg2+4, y);
      // draw label text
      ADrawer.TextOut.Pos(xg2+12, y-ts.y div 2).Text(s).Done;
    end;
  end;
end;

end.


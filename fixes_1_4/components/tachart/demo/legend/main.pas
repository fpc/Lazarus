unit main;

{$mode objfpc}{$H+}

interface

uses
  Controls, ExtCtrls, Graphics, Spin, StdCtrls, Forms, ComCtrls,
  TAGraph, TASeries, TASources, Classes, TALegend, TAFuncSeries, TADrawUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbByRows: TCheckBox;
    cbGrid: TCheckBox;
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1FuncSeries1: TFuncSeries;
    Chart1LineSeries1: TLineSeries;
    Chart1PieSeries1: TPieSeries;
    cbUseSidebar: TCheckBox;
    cbSeries: TComboBox;
    Chart2: TChart;
    GradientLineSeries: TLineSeries;
    lblColumnCount: TLabel;
    lblSpacing: TLabel;
    lblMarginX: TLabel;
    lblSymbolWidth: TLabel;
    lblMarginY: TLabel;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    PageControl1: TPageControl;
    pnControls: TPanel;
    rgAlignment: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    seSpacing: TSpinEdit;
    seMarginX: TSpinEdit;
    seColumnCount: TSpinEdit;
    seSymbolWidth: TSpinEdit;
    seMarginY: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure cbByRowsChange(Sender: TObject);
    procedure cbGridChange(Sender: TObject);
    procedure cbSeriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cbUseSidebarChange(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1FuncSeries1DrawLegend(
      ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; AItem: TLegendItem);
    procedure Chart1FuncSeries1LegendCreate(
      AItem: TLegendItem; AIndex: Integer);
    procedure Chart2DrawLegend(ASender: TChart; ADrawer: IChartDrawer;
      ALegendItems: TChartLegendItems; ALegendItemSize: TPoint;
      const ALegendRect: TRect; AColCount, ARowCount: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
    procedure seMarginXChange(Sender: TObject);
    procedure seMarginYChange(Sender: TObject);
    procedure seSpacingChange(Sender: TObject);
    procedure seSymbolWidthChange(Sender: TObject);
    procedure seColumnCountChange(Sender: TObject);
  private
    FItems: TChartLegendItems;
    FGradientMinValue: Double;
    FGradientMaxValue: Double;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  SysUtils, Math, TAChartUtils, TADrawerCanvas;

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


{ TForm1 }

procedure TForm1.cbByRowsChange(Sender: TObject);
begin
  with Chart1.Legend do
    if cbByRows.Checked then
      ItemFillOrder := lfoRowCol
    else
      ItemFillOrder := lfoColRow;
end;

procedure TForm1.cbGridChange(Sender: TObject);
begin
  Chart1.Legend.GridHorizontal.Visible := cbGrid.Checked;
  Chart1.Legend.GridVertical.Visible := cbGrid.Checked;
end;

procedure TForm1.cbSeriesDrawItem(
  Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  id: IChartDrawer;
  r: TRect;
begin
  Unused(Control, State);
  {
  if Index = cbSeries.ItemIndex then
    cbSeries.Canvas.Brush.Color := clHighlight
  else
    cbSeries.Canvas.Brush.Color := clWindow;
    }
  cbSeries.Canvas.FillRect(ARect);
  id := TCanvasDrawer.Create(cbSeries.Canvas);
  r := Bounds(
    ARect.Left + 2, ARect.Top, Chart1.Legend.SymbolWidth, cbSeries.ItemHeight);
  id.Pen := Chart1.Legend.SymbolFrame;
  FItems[Index].Draw(id, r);
end;

procedure TForm1.cbUseSidebarChange(Sender: TObject);
begin
  Chart1.Legend.UseSidebar := cbUseSidebar.Checked;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX * 2) + 7;
end;

procedure TForm1.Chart1FuncSeries1LegendCreate(
  AItem: TLegendItem; AIndex: Integer);
begin
  AItem.Text := 'Function ' + IntToStr(AIndex);
  if AIndex = 1 then
    AItem.Order := 0;
end;

procedure TForm1.Chart1FuncSeries1DrawLegend(
  ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; AItem: TLegendItem);
var
  x, y0, w: Integer;
begin
  Unused(AIndex, AItem);
  ACanvas.Pen := Chart1FuncSeries1.Pen;
  y0 := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.MoveTo(ARect.Left, y0);
  w := ARect.Right - ARect.Left;
  for x := 0 to w do
    ACanvas.LineTo(
      ARect.Left + x,
      Round(Sin(x / w * 2 * Pi) * (ARect.Bottom - ARect.Top) / 2) + y0);
end;

{ This event handler draws the legend completely on its own.
  You can draw anything here - it's your responsibility...
  Here we draw a color gradient to explain the symbol colors of the datapoints. }
procedure TForm1.Chart2DrawLegend(ASender: TChart; ADrawer: IChartDrawer;
  ALegendItems: TChartLegendItems; ALegendItemSize: TPoint;
  const ALegendRect: TRect; AColCount, ARowCount: Integer);
var
  xg1, xg2, yg1, yg2, y: Integer;
  s: String;
  yval: Double;
  i: Integer;
  P: array[0..3] of TPoint;
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

procedure TForm1.FormCreate(Sender: TObject);

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

var
  li: TLegendItem;
begin
  // Workaround for issue #19632
  Chart1FuncSeries1.Legend.OnCreate := @Chart1FuncSeries1LegendCreate;
  FItems := Chart1.GetLegendItems;
  Chart1.Legend.SortItemsByOrder(FItems);
  for li in FItems do
    cbSeries.AddItem('', nil);

  // Prepare data for chart with owner-drawn legend
  PrepareData;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FItems);
end;

procedure TForm1.rgAlignmentClick(Sender: TObject);
begin
  with Chart1.Legend do
    case rgAlignment.ItemIndex of
      0: Alignment := laTopLeft;
      1: Alignment := laCenterLeft;
      2: Alignment := laBottomLeft;
      3: Alignment := laTopCenter;
      4: Abort;
      5: Alignment := laBottomCenter;
      6: Alignment := laTopRight;
      7: Alignment := laCenterRight;
      8: Alignment := laBottomRight;
    end;
end;

procedure TForm1.seMarginXChange(Sender: TObject);
begin
  Chart1.Legend.MarginX := seMarginX.Value;
end;

procedure TForm1.seMarginYChange(Sender: TObject);
begin
  Chart1.Legend.MarginY := seMarginY.Value;
end;

procedure TForm1.seSpacingChange(Sender: TObject);
begin
  Chart1.Legend.Spacing := seSpacing.Value;
end;

procedure TForm1.seSymbolWidthChange(Sender: TObject);
begin
  Chart1.Legend.SymbolWidth := seSymbolWidth.Value;
end;

procedure TForm1.seColumnCountChange(Sender: TObject);
begin
  Chart1.Legend.ColumnCount := seColumnCount.Value;
end;

end.


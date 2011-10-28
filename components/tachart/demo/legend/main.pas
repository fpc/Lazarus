unit main;

{$mode objfpc}{$H+}

interface

uses
  Controls, ExtCtrls, Graphics, Spin, StdCtrls, Forms,
  TAGraph, TASeries, TASources, Classes, TALegend, TAFuncSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbByRows: TCheckBox;
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1FuncSeries1: TFuncSeries;
    Chart1LineSeries1: TLineSeries;
    Chart1PieSeries1: TPieSeries;
    cbUseSidebar: TCheckBox;
    cbSeries: TComboBox;
    lblColumnCount: TLabel;
    lblSpacing: TLabel;
    lblMarginX: TLabel;
    lblSymbolWidth: TLabel;
    lblMarginY: TLabel;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    pnControls: TPanel;
    rgAlignment: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    seSpacing: TSpinEdit;
    seMarginX: TSpinEdit;
    seColumnCount: TSpinEdit;
    seSymbolWidth: TSpinEdit;
    seMarginY: TSpinEdit;
    procedure cbByRowsChange(Sender: TObject);
    procedure cbSeriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cbUseSidebarChange(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1FuncSeries1DrawLegend(
      ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; AItem: TLegendItem);
    procedure Chart1FuncSeries1LegendCreate(
      AItem: TLegendItem; AIndex: Integer);
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
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  SysUtils, TADrawerCanvas, TADrawUtils;

{ TForm1 }

procedure TForm1.cbByRowsChange(Sender: TObject);
begin
  with Chart1.Legend do
    if cbByRows.Checked then
      ItemFillOrder := lfoRowCol
    else
      ItemFillOrder := lfoColRow;
end;

procedure TForm1.cbSeriesDrawItem(
  Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  id: IChartDrawer;
  r: TRect;
begin
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
  ACanvas.Pen := Chart1FuncSeries1.Pen;
  y0 := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.MoveTo(ARect.Left, y0);
  w := ARect.Right - ARect.Left;
  for x := 0 to w do
    ACanvas.LineTo(
      ARect.Left + x,
      Round(Sin(x / w * 2 * Pi) * (ARect.Bottom - ARect.Top) / 2) + y0);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  li: TLegendItem;
begin
  // Workaround for issue #19632
  Chart1FuncSeries1.Legend.OnCreate := @Chart1FuncSeries1LegendCreate;
  FItems := Chart1.GetLegendItems;
  Chart1.Legend.SortItemsByOrder(FItems);
  for li in FItems do
    cbSeries.AddItem('', nil);
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


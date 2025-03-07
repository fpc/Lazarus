unit frmBasic;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Types,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TAChartUtils, TADrawUtils, TASources, TASeries, TAFuncSeries,
  TALegend, TADrawerCanvas, Graphics;

type
  TBasicFrame = class(TFrame)
    cbByRows: TCheckBox;
    cbGrid: TCheckBox;
    cbSeries: TComboBox;
    cbUseSidebar: TCheckBox;
    Chart: TChart;
    AreaSeries: TAreaSeries;
    FuncSeries: TFuncSeries;
    LineSeries: TLineSeries;
    PieSeries: TPieSeries;
    lblColumnCount: TLabel;
    lblMarginX: TLabel;
    lblMarginY: TLabel;
    lblSpacing: TLabel;
    lblSymbolWidth: TLabel;
    ListChartSource2: TListChartSource;
    pnControls: TPanel;
    RandomChartSource1: TRandomChartSource;
    rgAlignment: TRadioGroup;
    seColumnCount: TSpinEdit;
    seMarginX: TSpinEdit;
    seMarginY: TSpinEdit;
    seSpacing: TSpinEdit;
    seSymbolWidth: TSpinEdit;
    procedure cbByRowsChange(Sender: TObject);
    procedure cbGridChange(Sender: TObject);
    procedure cbSeriesDrawItem(Control: TWinControl; Index: Integer; ARect:
      TRect; State: TOwnerDrawState);
    procedure cbUseSidebarChange(Sender: TObject);
    procedure FuncSeriesCalculate(const AX: Double; out AY: Double);
    procedure Chart1FuncSeries1LegendCreate(AItem: TLegendItem; AIndex: Integer);
    procedure Chart1FuncSeries1ChartSeriesLegendDraw(ACanvas: TCanvas; const
      ARect: TRect; AIndex: Integer; AItem: TLegendItem);
    procedure rgAlignmentClick(Sender: TObject);
    procedure seColumnCountChange(Sender: TObject);
    procedure seMarginXChange(Sender: TObject);
    procedure seMarginYChange(Sender: TObject);
    procedure seSpacingChange(Sender: TObject);
    procedure seSymbolWidthChange(Sender: TObject);
  private
    FItems: TChartLegendItems;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

constructor TBasicFrame.Create(AOwner: TComponent);
var
  li: TLegendItem;
begin
  inherited;

  // Workaround for issue #19632
  FuncSeries.Legend.OnCreate := @Chart1FuncSeries1LegendCreate;

  FItems := Chart.GetLegendItems;
  Chart.Legend.SortItemsByOrder(FItems);
  for li in FItems do
    cbSeries.AddItem('', nil);
end;

destructor TBasicFrame.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TBasicFrame.FuncSeriesCalculate(const AX: Double; out AY:
  Double);
begin
  AY := Sin(AX * 2) + 7;
end;

procedure TBasicFrame.Chart1FuncSeries1LegendCreate(AItem: TLegendItem; AIndex: Integer);
begin
  AItem.Text := 'Function ' + IntToStr(AIndex);
  if AIndex = 1 then
    AItem.Order := 0;
end;

procedure TBasicFrame.Chart1FuncSeries1ChartSeriesLegendDraw(ACanvas: TCanvas;
  const ARect: TRect; AIndex: Integer; AItem: TLegendItem);
var
  x, y0, w: Integer;
begin
  Unused(AIndex, AItem);
  ACanvas.Pen := FuncSeries.Pen;
  y0 := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.MoveTo(ARect.Left, y0);
  w := ARect.Right - ARect.Left;
  for x := 0 to w do
    ACanvas.LineTo(
      ARect.Left + x,
      Round(Sin(x / w * 2 * Pi) * (ARect.Bottom - ARect.Top) / 2) + y0);
end;

procedure TBasicFrame.rgAlignmentClick(Sender: TObject);
begin
  with Chart.Legend do
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

procedure TBasicFrame.cbUseSidebarChange(Sender: TObject);
begin
  Chart.Legend.UseSidebar := cbUseSidebar.Checked;
end;

procedure TBasicFrame.cbByRowsChange(Sender: TObject);
begin
  with Chart.Legend do
    if cbByRows.Checked then
      ItemFillOrder := lfoRowCol
    else
      ItemFillOrder := lfoColRow;
end;

procedure TBasicFrame.cbGridChange(Sender: TObject);
begin
  Chart.Legend.GridHorizontal.Visible := cbGrid.Checked;
  Chart.Legend.GridVertical.Visible := cbGrid.Checked;
end;

procedure TBasicFrame.cbSeriesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  id: IChartDrawer;
  r: TRect;
begin
  Unused(Control, State);
  cbSeries.Canvas.FillRect(ARect);
  id := TCanvasDrawer.Create(cbSeries.Canvas);
  r := Bounds(ARect.Left + 2, ARect.Top, Chart.Legend.SymbolWidth, cbSeries.ItemHeight);
  id.Pen := Chart.Legend.SymbolFrame;
  FItems[Index].Draw(id, r);
end;

procedure TBasicFrame.seColumnCountChange(Sender: TObject);
begin
  Chart.Legend.ColumnCount := seColumnCount.Value;
end;

procedure TBasicFrame.seMarginXChange(Sender: TObject);
begin
  Chart.Legend.MarginX := seMarginX.Value;
end;

procedure TBasicFrame.seMarginYChange(Sender: TObject);
begin
  Chart.Legend.MarginY := seMarginY.Value;
end;

procedure TBasicFrame.seSpacingChange(Sender: TObject);
begin
  Chart.Legend.Spacing := seSpacing.Value;
end;

procedure TBasicFrame.seSymbolWidthChange(Sender: TObject);
begin
  Chart.Legend.SymbolWidth := seSymbolWidth.Value;
end;

end.


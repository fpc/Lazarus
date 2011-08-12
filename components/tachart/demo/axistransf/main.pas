unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Forms, Spin, StdCtrls, TAFuncSeries, TAGraph,
  TASeries, TASources, TAStyles, TATools, TATransformations, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    catIndependent1Zoom: TLinearAxisTransform;
    catIndependent2Zoom: TLinearAxisTransform;
    catIndependent2: TChartAxisTransformations;
    catTAutoAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAuto: TChartAxisTransformations;
    cbAuto: TCheckBox;
    catUser: TChartAxisTransformations;
    catUserUserDefinedAxisTransform1: TUserDefinedAxisTransform;
    catIndependent1: TChartAxisTransformations;
    ChartIndependent: TChart;
    ChartIndependentLineSeries1: TLineSeries;
    ChartIndependentLineSeries2: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointDragTool1: TDataPointDragTool;
    ChartUser: TChart;
    ChartUserConstantLine1: TConstantLine;
    ChartUserLineSeries1: TLineSeries;
    csStripes: TChartStyles;
    ChartLog: TChart;
    cfsLog: TFuncSeries;
    cbLog: TCheckBox;
    ChartTWinterBar: TBarSeries;
    clsLogPoints: TLineSeries;
    ChartT: TChart;
    catLog: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform2: TLinearAxisTransform;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    catT: TChartAxisTransformations;
    catTFahrToCel: TLinearAxisTransform;
    ChartTSummer: TLineSeries;
    ChartTWinterLine: TLineSeries;
    fseIndependent1: TFloatSpinEdit;
    fseIndependent2: TFloatSpinEdit;
    lblIndependentScale1: TLabel;
    lblIndependentScale2: TLabel;
    PageControl1: TPageControl;
    pnlIndependentControls: TPanel;
    pnlLogControls: TPanel;
    pnlAutoControls: TPanel;
    rcsUser: TRandomChartSource;
    rcsTSummer: TRandomChartSource;
    rcsTWinter: TRandomChartSource;
    lsLinear: TTabSheet;
    tsIndependent: TTabSheet;
    tsUser: TTabSheet;
    tsLog: TTabSheet;
    procedure cbAutoChange(Sender: TObject);
    procedure cbLogChange(Sender: TObject);
    procedure catUserUserDefinedAxisTransform1AxisToGraph(
      AX: Double; out AT: Double);
    procedure ChartLogFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
    procedure fseIndependent1Change(Sender: TObject);
    procedure fseIndependent2Change(Sender: TObject);
  private
    procedure FillIndependentSource;
  end;

var
  Form1: TForm1; 

implementation

uses
  Math, SysUtils, TAChartAxis, TAChartUtils;

{$R *.lfm}

function MyFunc(AX: Double): Double;
begin
  Result := Power(10, AX) + 3;
end;

{ TForm1 }

procedure TForm1.cbAutoChange(Sender: TObject);
begin
  catTAutoAutoScaleAxisTransform1.Enabled := cbAuto.Checked;
  catTAutoScaleAxisTransform1.Enabled := cbAuto.Checked;
end;

procedure TForm1.cbLogChange(Sender: TObject);
begin
  ChartAxisTransformations1LogarithmAxisTransform1.Enabled := cbLog.Checked;
end;

procedure TForm1.catUserUserDefinedAxisTransform1AxisToGraph(
  AX: Double; out AT: Double);
const
  R1 = 8.0;
  C = 2.5;
  R2 = R1 / C;
var
  zx: Double;
begin
  zx := ChartUserConstantLine1.Position;
  if Abs(AX - zx) > R1 then AT := AX
  else if AX < zx - R2 then AT := zx - R1
  else if AX > zx + R2 then AT := zx + R1
  else AT := (AX - zx + R2) * C + zx - R1;
end;

procedure TForm1.ChartLogFuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := MyFunc(AX);
end;

procedure TForm1.FillIndependentSource;

var
  i: Integer;
  v1, v2: Double;
begin
  RandSeed := 923875;
  v1 := 0;
  v2 := 0;
  for i := 1 to 100 do begin
    v1 += Random - 0.48;
    v2 += Random - 0.52;
    ChartIndependentLineSeries1.AddXY(i, v1);
    ChartIndependentLineSeries2.AddXY(i, v2);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  x: Double;
begin
  for i := 0 to 50 do begin
    with cfsLog.Extent do
      x := i / 50 * (XMax - XMin) + XMin;
    clsLogPoints.AddXY(x + Random - 0.5, MyFunc(x) + Random - 0.5);
  end;
  FillIndependentSource;
end;

procedure TForm1.fseIndependent1Change(Sender: TObject);
begin
  catIndependent1Zoom.Scale := fseIndependent1.Value;
end;

procedure TForm1.fseIndependent2Change(Sender: TObject);
begin
  catIndependent2Zoom.Scale := fseIndependent2.Value;
end;

end.


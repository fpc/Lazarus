unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Forms, StdCtrls, TAFuncSeries, TAGraph,
  TASeries, TASources, TAStyles, TATools, TATransformations;

type

  { TForm1 }

  TForm1 = class(TForm)
    catTAutoAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAuto: TChartAxisTransformations;
    cbAuto: TCheckBox;
    catUser: TChartAxisTransformations;
    catUserUserDefinedAxisTransform1: TUserDefinedAxisTransform;
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
    PageControl1: TPageControl;
    pnlLogControls: TPanel;
    pnlAutoControls: TPanel;
    rcsUser: TRandomChartSource;
    rcsTSummer: TRandomChartSource;
    rcsTWinter: TRandomChartSource;
    lsLinear: TTabSheet;
    tsUser: TTabSheet;
    tsLog: TTabSheet;
    procedure cbAutoChange(Sender: TObject);
    procedure cbLogChange(Sender: TObject);
    procedure catUserUserDefinedAxisTransform1AxisToGraph(
      AX: Double; out AT: Double);
    procedure ChartLogFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
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
  R2 = R1 * 0.5 / C;
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
  catUserUserDefinedAxisTransform1.OnAxisToGraph :=
    @catUserUserDefinedAxisTransform1AxisToGraph;
end;

end.


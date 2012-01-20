unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Forms, Spin, StdCtrls, TAFuncSeries, TAGraph,
  TASeries, TASources, TAStyles, TATools, TATransformations, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    AxLabelSource: TListChartSource;
    catCumulNormDistrCumulNormDistrAxisTransform1: TCumulNormDistrAxisTransform;
    catCumulNormDistrLinearAxisTransform1: TLinearAxisTransform;
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
    cbPercent: TCheckBox;
    cbUseAxisTransform: TCheckBox;
    ChartCumulNormDistr: TChart;
    catCumulNormDistr: TChartAxisTransformations;
    ChartIndependent: TChart;
    ChartIndependentLineSeries1: TLineSeries;
    ChartIndependentLineSeries2: TLineSeries;
    ChartCumulNormDistrLineSeries1: TLineSeries;
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
    edDataCount: TSpinEdit;
    fseIndependent1: TFloatSpinEdit;
    fseIndependent2: TFloatSpinEdit;
    lblTolerance: TLabel;
    lblDataCount: TLabel;
    lblIndependentScale1: TLabel;
    lblIndependentScale2: TLabel;
    PageControl1: TPageControl;
    pnCumulNormDistr: TPanel;
    pnlIndependentControls: TPanel;
    pnlLogControls: TPanel;
    pnlAutoControls: TPanel;
    rcsUser: TRandomChartSource;
    rcsTSummer: TRandomChartSource;
    rcsTWinter: TRandomChartSource;
    rgRandDistr: TRadioGroup;
    seTolerance: TSpinEdit;
    tsLinear: TTabSheet;
    tsCumulNormDistr: TTabSheet;
    tsIndependent: TTabSheet;
    tsUser: TTabSheet;
    tsLog: TTabSheet;
    procedure cbAutoChange(Sender: TObject);
    procedure cbLogChange(Sender: TObject);
    procedure catUserUserDefinedAxisTransform1AxisToGraph(
      AX: Double; out AT: Double);
    procedure cbPercentChange(Sender: TObject);
    procedure cbUseAxisTransformChange(Sender: TObject);
    procedure ChartLogFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure edDataCountChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fseIndependent1Change(Sender: TObject);
    procedure fseIndependent2Change(Sender: TObject);
    procedure rgRandDistrClick(Sender: TObject);
    procedure seToleranceChange(Sender: TObject);
  private
    procedure FillIndependentSource;
    procedure FillCumulNormDistrSource;
  end;

var
  Form1: TForm1; 

implementation

uses
  Math, StrUtils, SysUtils, TAChartAxis, TAChartUtils;

{$R *.lfm}

var
  GaussDevAvail: Boolean = false;
  GaussDev: Double = 0.0;

// Create a random number with normal distribution, mean value 0, standard
// deviation 1. See Numerical Recipes.
function RndNormal: Double;
var
  fac, r, v1, v2: Double;
begin
  if GaussDevAvail then
    Result := GaussDev
  else begin
    repeat
      v1 := 2.0 * Random - 1.0;
      v2 := 2.0 * Random - 1.0;
      r := Sqr(v1) + Sqr(v2);
    until (r > 0.0) and (r < 1.0);
    fac := Sqrt(-2.0 * Ln(r) / r);
    GaussDev := v1 * fac;
    Result := v2 * fac;
  end;
  GaussDevAvail := not GaussDevAvail;
end;

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

procedure TForm1.cbPercentChange(Sender: TObject);
begin
  catCumulNormDistrLinearAxisTransform1.Enabled := cbPercent.Checked;
  FillCumulNormDistrSource;
  ChartCumulNormDistr.LeftAxis.Title.Caption :=
    'Cumulative probability' + IfThen(cbPercent.Checked, ' (%)', '');
end;

procedure TForm1.cbUseAxisTransformChange(Sender: TObject);
begin
  catCumulNormDistrCumulNormDistrAxisTransform1.Enabled :=
    cbUseAxisTransform.Checked;
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

procedure TForm1.edDataCountChange(Sender: TObject);
begin
  FillCumulNormDistrSource;
end;

procedure TForm1.FillCumulNormDistrSource;
var
  i: Integer;
  y: Double;
  s: TListChartSource;
begin
  RandSeed := 976896;
  s := ChartCumulNormDistrLineSeries1.ListSource;
  s.BeginUpdate;
  try
    s.Clear;
    // Add random test data as x values --> random values will
    // get sorted in ascending direction automatically.
    s.Sorted := false;
    for i := 1 to edDataCount.Value do
      case rgRandDistr.ItemIndex of
        0: s.Add(Random, 0);
        1: s.Add(RndNormal, 0);
      end;
    s.Sorted := true;
    // Calculate cumulative probability from index in sorted list.
    for i := 0 to s.Count - 1 do begin
      y := (i + 1) / (s.Count + 1); // Add 1 since y=0 and y=1 are not valid.
      s.Item[i]^.Y := IfThen(CbPercent.Checked, y * 100, y);
    end;
  finally
    s.EndUpdate;
  end;
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
  FillCumulNormDistrSource;
  seTolerance.Value := ChartLog.LeftAxis.Intervals.Tolerance;
end;

procedure TForm1.fseIndependent1Change(Sender: TObject);
begin
  catIndependent1Zoom.Scale := fseIndependent1.Value;
end;

procedure TForm1.fseIndependent2Change(Sender: TObject);
begin
  catIndependent2Zoom.Scale := fseIndependent2.Value;
end;

procedure TForm1.rgRandDistrClick(Sender: TObject);
begin
  FillCumulNormDistrSource;
end;

procedure TForm1.seToleranceChange(Sender: TObject);
begin
  ChartLog.LeftAxis.Intervals.Tolerance := seTolerance.Value;
end;

end.


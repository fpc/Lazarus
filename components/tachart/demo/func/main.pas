unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, RTTICtrls, Spin, StdCtrls, Forms, Graphics,
  TAFuncSeries, TAGraph, TALegendPanel, TASeries, TACustomSource, TASources,
  TATools, TATransformations, TAExpressionSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbBSpline: TTICheckBox;
    cbLogY: TTICheckBox;
    cbDomain: TCheckBox;
    cbRotate: TCheckBox;
    cbInterpolate: TCheckBox;
    cbMultLegend: TCheckBox;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1FuncSeries1: TFuncSeries;
    Chart1UserDrawnSeries1: TUserDrawnSeries;
    Chart1XAxis: TConstantLine;
    Chart1YAxis: TConstantLine;
    catSpline: TChartAxisTransformations;
    catSplineLogarithmAxisTransform: TLogarithmAxisTransform;
    cmbPalette: TComboBox;
    ExpressionChart: TChart;
    ExpressionSeries: TExpressionSeries;
    chParametric: TChart;
    chParametricParametricCurveSeries1: TParametricCurveSeries;
    chAutoExtentY: TChart;
    ChartSpline: TChart;
    ChartColorMap: TChart;
    ChartColorMapColorMapSeries1: TColorMapSeries;
    ChartLegendPanel1: TChartLegendPanel;
    ChartSplineCubicSplineSeries1: TCubicSplineSeries;
    ChartSplineLineSeries1: TLineSeries;
    ChartSplineBSplineSeries1: TBSplineSeries;
    cbAutoExtentY: TCheckBox;
    chAutoExtentYFuncSeries1: TFuncSeries;
    chtsColorMap: TChartToolset;
    chtsColorMapPanDragTool1: TPanDragTool;
    chtsColorMapZoomDragTool1: TZoomDragTool;
    EdExpression: TEdit;
    EdExprDomain: TEdit;
    EdExprParamA: TEdit;
    EdExprParamB: TEdit;
    lblPalette: TLabel;
    LblExpression: TLabel;
    LblExprDomain: TLabel;
    LblExprParamA: TLabel;
    LblExprParamB: TLabel;
    lblA: TLabel;
    lblB: TLabel;
    lblD: TLabel;
    lblC: TLabel;
    lblK: TLabel;
    lblJ: TLabel;
    lblSplineDegree: TLabel;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlParametric: TPanel;
    pnlAutoExtentY: TPanel;
    pnSpline: TPanel;
    RandomChartSource1: TRandomChartSource;
    iseSplineDegree: TTISpinEdit;
    icbSplineRandomX: TTICheckBox;
    cbCubic: TTICheckBox;
    seJ: TSpinEdit;
    seK: TSpinEdit;
    stEq: TStaticText;
    tsExpression: TTabSheet;
    tbA: TTrackBar;
    tbB: TTrackBar;
    tbC: TTrackBar;
    tbD: TTrackBar;
    tsParametric: TTabSheet;
    Timer1: TTimer;
    tsAutoExtentY: TTabSheet;
    tsSpline: TTabSheet;
    tsDomain: TTabSheet;
    tsColorMap: TTabSheet;
    Splitter1: TSplitter;
    UserDefinedChartSource1: TUserDefinedChartSource;
    procedure cbAutoExtentYChange(Sender: TObject);
    procedure cbDomainChange(Sender: TObject);
    procedure cbInterpolateChange(Sender: TObject);
    procedure cbMultLegendChange(Sender: TObject);
    procedure cbRotateChange(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1UserDrawnSeries1Draw(ACanvas: TCanvas; const ARect: TRect);
    procedure ChartColorMapColorMapSeries1Calculate(const AX, AY: Double; out
      AZ: Double);
    procedure chAutoExtentYFuncSeries1Calculate(const AX: Double; out
      AY: Double);
    procedure chParametricParametricCurveSeries1Calculate(const AT: Double; out
      AX, AY: Double);
    procedure cmbPaletteChange(Sender: TObject);
    procedure EdExprDomainEditingDone(Sender: TObject);
    procedure EdExpressionEditingDone(Sender: TObject);
    procedure EdExprParamAEditingDone(Sender: TObject);
    procedure EdExprParamBEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure iseSplineDegreeChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ParamChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  end;

var
  Form1: TForm1; 

implementation

uses
  SysUtils, Math,
  TAChartUtils, TALegend;

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbAutoExtentYChange(Sender: TObject);
begin
  chAutoExtentYFuncSeries1.ExtentAutoY := cbAutoExtentY.Checked;
end;

procedure TForm1.cbDomainChange(Sender: TObject);
var
  i: Integer;
begin
  with Chart1FuncSeries1.DomainExclusions do begin
    Clear;
    Epsilon := 1e-7;
    if cbDomain.Checked then
      for i := -10 to 10 do
        AddPoint(i * Pi);
  end;
end;

procedure TForm1.cbInterpolateChange(Sender: TObject);
begin
  ChartColorMapColorMapSeries1.Interpolate := cbInterpolate.Checked;
end;

procedure TForm1.cbMultLegendChange(Sender: TObject);
begin
  with ChartColorMapColorMapSeries1.Legend do
    if cbMultLegend.Checked then
      Multiplicity := lmPoint
    else
      Multiplicity := lmSingle;
end;

procedure TForm1.cbRotateChange(Sender: TObject);
begin
  with Chart1FuncSeries1 do
    if cbRotate.Checked then begin
      AxisIndexX := 0;
      AxisIndexY := 1;
    end
    else begin
      AxisIndexX := 1;
      AxisIndexY := 0;
    end;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := 1 / Sin(AX);
end;

procedure TForm1.Chart1UserDrawnSeries1Draw(
  ACanvas: TCanvas; const ARect: TRect);
var
  a: TDoublePoint = (X: -1; Y: -1);
  b: TDoublePoint = (X: 1; Y: 1);
  r: TRect;
begin
  Unused(ARect);
  r.TopLeft := Chart1.GraphToImage(a);
  r.BottomRight := Chart1.GraphToImage(b);
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Ellipse(r);
end;

procedure TForm1.ChartColorMapColorMapSeries1Calculate(
  const AX, AY: Double; out AZ: Double);
begin
  AZ := Sin(10 * Sqr(AX) + 17 * Sqr(AY));
end;

procedure TForm1.chAutoExtentYFuncSeries1Calculate(
  const AX: Double; out AY: Double);
begin
  AY := Sin(AX * 2) + 3 * Cos(AX * 3) + 2 * Cos(AX * AX * 5);
end;

procedure TForm1.chParametricParametricCurveSeries1Calculate(
  const AT: Double; out AX, AY: Double);
var
  a, b, c, d: Double;
begin
  a := tbA.Position / tbA.Frequency;
  b := tbB.Position / tbB.Frequency;
  c := tbC.Position / tbC.Frequency;
  d := tbD.Position / tbD.Frequency;
  AX := Cos(a * AT) - IntPower(Cos(b * AT), seJ.Value);
  AY := Sin(c * AT) - IntPower(Sin(d * AT), seK.Value);
end;

procedure TForm1.cmbPaletteChange(Sender: TObject);
begin
  if cmbPalette.ItemIndex < cmbPalette.Items.Count-1 then begin
    ChartColorMapColorMapSeries1.ColorSource := nil;
    ChartColorMapColorMapSeries1.BuiltinPalette := TColorMapPalette(cmbPalette.ItemIndex);
  end else
    ChartColorMapColorMapSeries1.ColorSource := ListChartSource1;
end;

procedure TForm1.EdExprDomainEditingDone(Sender: TObject);
begin
  ExpressionSeries.Domain := EdExprDomain.Text;
end;

procedure TForm1.EdExpressionEditingDone(Sender: TObject);
begin
  ExpressionSeries.Expression := EdExpression.Text;
end;

procedure TForm1.EdExprParamAEditingDone(Sender: TObject);
begin
  ExpressionSeries.Params.ValueByName['a'] := StrToFloat(EdExprParamA.Text);
end;

procedure TForm1.EdExprParamBEditingDone(Sender: TObject);
begin
  ExpressionSeries.Params.ValueByName['b'] := StrToFloat(EdExprParamB.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdExpression.Text := ExpressionSeries.Expression;
  EdExprDomain.Text := ExpressionSeries.Domain;
  EdExprParamA.Text := FloatToStr(ExpressionSeries.Params.ValueByName['a']);
  EdExprParamB.Text := FloatToStr(ExpressionSeries.Params.ValueByName['b']);
end;

procedure TForm1.iseSplineDegreeChange(Sender: TObject);
begin
  (Sender as TTISpinEdit).EditingDone;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  Timer1.Enabled := PageControl1.ActivePage = tsAutoExtentY;
end;

procedure TForm1.ParamChange(Sender: TObject);
begin
  chParametric.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with chAutoExtentYFuncSeries1.Extent do begin
    XMin := XMin + 0.05;
    XMax := XMax + 0.05;
  end;
end;

procedure TForm1.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := AIndex - ASource.PointsNumber / 2;
  AItem.Y := Cos(AItem.X);
end;

end.


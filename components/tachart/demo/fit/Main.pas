unit Main;

{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons, TAGraph, TASeries, TASources,
  TAFuncSeries, TATransformations;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    cbTestFunction: TComboBox;
    Chart: TChart;
    cbDrawFitRangeOnly: TCheckBox;
    FitSeries: TFitSeries;
    DataSeries: TLineSeries;
    cbFitRangeUseMin:TCheckBox;
    cbFitRangeUseMax:TCheckBox;
    cbFitEquation: TComboBox;
    cbLogX: TCheckBox;
    cbLogY: TCheckBox;
    ChartAxisTransformations: TChartAxisTransformations;
    LogarithmAxisTransform: TLogarithmAxisTransform;
    edFitRangeMax:TFloatSpinEdit;
    edNoiseY: TFloatSpinEdit;
    edFitRangeMin:TFloatSpinEdit;
    gbFitRange:TGroupBox;
    gbDataGeneration: TGroupBox;
    gbFitting: TGroupBox;
    gbResults: TGroupBox;
    lblFitOrder:TLabel;
    lblNoiseY: TLabel;
    lblFitEquation: TLabel;
    lblOfRange: TLabel;
    lblTestFunction: TLabel;
    lbResults: TListBox;
    ListChartSource: TListChartSource;
    pnlParams: TPanel;
    edFitOrder:TSpinEdit;
    pnlLog: TPanel;
    pnlChart: TPanel;
    SaveDialog: TSaveDialog;
    btnSave: TSpeedButton;
    procedure btnSaveClick(Sender: TObject);
    procedure cbDrawFitRangeOnlyClick(Sender: TObject);
    procedure cbFitEquationSelect(Sender: TObject);
    procedure cbFitRangeUseMaxClick(Sender:TObject);
    procedure cbFitRangeUseMinClick(Sender:TObject);
    procedure cbLogClick(Sender: TObject);
    procedure cbTestFunctionSelect(Sender: TObject);
    procedure edFitOrderChange(Sender:TObject);
    procedure edFitRangeMaxChange(Sender:TObject);
    procedure edFitRangeMinChange(Sender:TObject);
    procedure edNoiseYChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FitCompleteHandler(Sender:TObject);
  private
    procedure CreateData;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  Math, TAChartAxis, TAChartUtils, TACustomSource;

const
  // Parameters used for data generation; should be reproduced by the fit.
  POLY_PARAMS: array[0..2] of Double = (100, -8, 0.2);
  LIN_PARAMS : array[0..1] of Double = (100.0, -2.5);
  EXP_PARAMS : array[0..1] of Double = (10.0, -0.05);
  PWR_PARAMS : array[0..1] of Double = (3.0, -0.5);

  // Min and max for x axis of the various test functions
  // positive numbers only because of the logarithms involved in this example.
  XRANGE : array[TFitEquation, 0..1] of Double = (
    (0.1, 50),
    (1, 20),
    (0.001, 100),
    (1, 20)
  );

{ TfrmMain }

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  s: TStream;
  fs: TFormatSettings;
  si: PChartDataItem;
begin
  if not SaveDialog.Execute then exit;
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  s := TFileStream.Create(SaveDialog.FileName, fmCreate);
  try
    for si in ListChartSource do
      s.WriteAnsiString(Format('%.9g'#9'%.9g'#13#10, [si^.X, si^.Y], fs));
  finally
    s.Free;
  end;
end;

procedure TfrmMain.cbDrawFitRangeOnlyClick(Sender: TObject);
begin
  FitSeries.DrawFitRangeOnly := cbDrawFitRangeOnly.Checked;
end;

procedure TfrmMain.cbFitEquationSelect(Sender: TObject);
var
  eq: TFitEquation;
begin
  eq := TFitEquation(cbFitEquation.ItemIndex);
  FitSeries.FitEquation := eq;
  edFitOrder.Enabled := (eq = fePolynomial);
  lblFitOrder.Enabled := edFitOrder.Enabled;
end;

procedure TfrmMain.cbFitRangeUseMaxClick(Sender:TObject);
begin
  edFitRangeMax.Visible := cbFitRangeUseMax.Checked;
  FitSeries.FitRange.UseMax := cbFitRangeUseMax.Checked;
  cbDrawFitRangeOnly.Enabled := cbFitRangeUseMin.Checked or cbFitRangeUseMax.Checked;
end;

procedure TfrmMain.cbFitRangeUseMinClick(Sender:TObject);
begin
  edFitRangeMin.Visible := cbFitRangeUseMin.Checked;
  FitSeries.FitRange.UseMin := cbFitRangeUseMin.Checked;
  cbDrawFitRangeOnly.Enabled := cbFitRangeUseMin.Checked or cbFitRangeUseMax.Checked;
end;

procedure TfrmMain.cbLogClick(Sender: TObject);
var
  axis: TChartAxis;
begin
  if Sender = cbLogX then
    axis := Chart.BottomAxis
  else
    axis := Chart.LeftAxis;
  if (Sender as TCheckbox).Checked then begin
    axis.Transformations := ChartAxisTransformations;
    axis.Intervals.Options :=
      [aipUseMinLength, aipUseCount, aipGraphCoords, aipUseNiceSteps];
    axis.Intervals.NiceSteps :=
      Format('%g|%g|%g|%g', [Log10(2), Log10(3), Log10(5), Log10(10)]);
  end else begin
    axis.Transformations := nil;
    axis.Intervals.Options := [aipUseMinLength, aipUseMaxLength, aipUseNiceSteps];
    axis.Intervals.NiceSteps := '0.2|0.5|1.0';
  end;
end;

procedure TfrmMain.cbTestFunctionSelect(Sender: TObject);
begin
  CreateData;
end;

procedure TfrmMain.CreateData;
const
  N = 100;
var
  i: Integer;
  x, y, xmin, xmax, ymin, ymax, maxNoise: Double;
  xarr, yarr: array of Double;
begin
  RandSeed := 875876;   // Reproducible noise for testing.

  // Calculate test data and store in temporary arrays.
  // This is because noise is relative to the data range in this example.
  xmin := XRANGE[TFitEquation(cbTestFunction.ItemIndex), 0];
  xmax := XRANGE[TFitEquation(cbTestFunction.ItemIndex), 1];
  SetLength(xarr, N);
  SetLength(yarr, N);
  for i := 0 to High(xarr) do begin
    x := xmin + (xmax - xmin) / (N - 1) * i;
    case TFitEquation(cbTestFunction.ItemIndex) of
      fePolynomial: y := POLY_PARAMS[0] + POLY_PARAMS[1]*x + POLY_PARAMS[2]*x*x;
      feLinear    : y := LIN_PARAMS[0] + LIN_PARAMS[1]*x;
      feExp       : y := EXP_PARAMS[0]*Exp(EXP_PARAMS[1]*x);
      fePower     : y := PWR_PARAMS[0]*Power(x, PWR_PARAMS[1]);
    end;
    xarr[i] := x;
    yarr[i] := y;
  end;

  // Add noise to the y values, and add data to line series.
  ymin := MinValue(yarr);
  ymax := MaxValue(yarr);
  maxNoise := edNoiseY.Value * (ymax - ymin) * 0.01;
  DataSeries.BeginUpdate;
  try
    DataSeries.Clear;
    for i := 0 to High(xarr) do begin
      x := xarr[i];
      y := yarr[i] + maxNoise * (Random - 0.5);
      if TFitEquation(cbTestFunction.ItemIndex) = feExp then
        // Make sure that the noise generation does not produce negative
        // values for the exponential data set.
        while y < 0 do
          y := yarr[i] + maxNoise * (Random - 0.5);
      DataSeries.AddXY(x, y);
    end;
  finally
    DataSeries.EndUpdate;
  end;
end;

procedure TfrmMain.edFitOrderChange(Sender:TObject);
begin
  // Needs one parameter more than degree of fit polynomial.
  FitSeries.ParamCount := edFitOrder.Value + 1;
end;

procedure TfrmMain.edFitRangeMaxChange(Sender:TObject);
begin
  FitSeries.FitRange.Max := edFitRangeMax.Value;
end;

procedure TfrmMain.edFitRangeMinChange(Sender:TObject);
begin
  FitSeries.FitRange.Min := edFitRangeMin.Value;
end;

procedure TfrmMain.edNoiseYChange(Sender: TObject);
begin
  CreateData;
end;

procedure TfrmMain.FitCompleteHandler(Sender:TObject);
var
  i: Integer;
begin
  with lbResults.Items do begin
    BeginUpdate;
    Clear;
    case TFitEquation(cbFitEquation.ItemIndex) of
      fePolynomial:
        for i := 0 to FitSeries.ParamCount - 1 do
          Add(Format('b[%d] = %g', [i, FitSeries.Param[i]]));
      else
        Add(Format('a = %g', [FitSeries.Param[0]]));
        Add(Format('b = %g', [FitSeries.Param[1]]));
    end;
    EndUpdate;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
const
  FMT = '%g';
begin
  with cbTestFunction do begin
    Items.Add(ParamsToEquation(fePolynomial, POLY_PARAMS, FMT));
    Items.Add(ParamsToEquation(feLinear, LIN_PARAMS, FMT));
    Items.Add(ParamsToEquation(feExp, EXP_PARAMS, FMT));
    Items.Add(ParamsToEquation(fePower, PWR_PARAMS, FMT));
    ItemIndex := Ord(fePolynomial);
  end;

  FitSeries.FitRange.Min := edFitRangeMin.Value;
  FitSeries.FitRange.Max := edFitRangeMax.Value;

  CreateData;
end;

end.


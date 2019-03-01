unit Main;

{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons, ComCtrls,
  TAGraph, TASources, TAFuncSeries, TATransformations, Types;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Bevel1: TBevel;
    BtnLoad: TButton;
    cbTestFunction: TComboBox;
    Chart: TChart;
    cbDrawFitRangeOnly: TCheckBox;
    cbFitParam0Fixed: TCheckBox;
    cbFitParam1Fixed: TCheckBox;
    cbShowErrorbars: TCheckBox;
    cbShowConfidenceIntervals: TCheckBox;
    cbShowPredictionIntervals: TCheckBox;
    UpperConfIntervalSeries: TFuncSeries;
    LowerConfIntervalSeries: TFuncSeries;
    UpperPredIntervalSeries: TFuncSeries;
    LowerPredIntervalSeries: TFuncSeries;
    FitSeries: TFitSeries;
    cbFitRangeUseMin:TCheckBox;
    cbFitRangeUseMax:TCheckBox;
    cbFitEquation: TComboBox;
    cbLogX: TCheckBox;
    cbLogY: TCheckBox;
    ChartAxisTransformations: TChartAxisTransformations;
    edFitParam0: TFloatSpinEdit;
    edFitParam1: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lblNoiseY1: TLabel;
    LogarithmAxisTransform: TLogarithmAxisTransform;
    edFitRangeMax:TFloatSpinEdit;
    edNoiseY: TFloatSpinEdit;
    edFitRangeMin:TFloatSpinEdit;
    gbFitRange:TGroupBox;
    gbDataGeneration: TGroupBox;
    gbFitting: TGroupBox;
    lblFitOrder:TLabel;
    lblNoiseY: TLabel;
    lblFitEquation: TLabel;
    lblOfRange: TLabel;
    lblTestFunction: TLabel;
    lbResults: TListBox;
    ListChartSource: TListChartSource;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    pnlParams: TPanel;
    edFitOrder:TSpinEdit;
    pnlLog: TPanel;
    pnlChart: TPanel;
    SaveDialog: TSaveDialog;
    btnSave: TSpeedButton;
    EdPointsCount: TSpinEdit;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BtnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbDrawFitRangeOnlyClick(Sender: TObject);
    procedure cbFitEquationSelect(Sender: TObject);
    procedure cbShowConfidenceIntervalsChange(Sender: TObject);
    procedure cbShowErrorbarsChange(Sender: TObject);
    procedure cbShowPredictionIntervalsChange(Sender: TObject);
    procedure EdPointsCountChange(Sender: TObject);
    procedure FixedParamsChanged(Sender: TObject);
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
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FReportDecimals: Integer;
    FDemoData: Boolean;
    procedure CreateData;
    procedure OpenFile(const AFileName: string);
    function PrepareFixedParams: String;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  Math, typ, spe, StrUtils,
  TAChartAxis, TATypes, TAChartUtils, TACustomSource, TAFitLib, TAFitUtils;

const
  // Parameters used for data generation; should be reproduced by the fit.
  POLY_PARAMS: array[0..2] of Double = (100, -8, 0.2);
  LIN_PARAMS: array[0..1] of Double = (100.0, -2.5);
  EXP_PARAMS: array[0..1] of Double = (10.0, -0.05);
  PWR_PARAMS: array[0..1] of Double = (3.0, -0.5);
  HARMONIC_PARAMS: array[0..3] of Double = (2.0, 1.0, 1/3, 1/5);

  // Min and max for x axis of the various test functions
  // positive numbers only because of the logarithms involved in this example.
  XRANGE : array[TFitEquation, 0..1] of Double = (
    (0.1, 50),
    (1, 20),
    (0.001, 100),
    (1, 20),
    (0.1, 20)
  );


function RoundToError(AValue, AError: Double): String;
var
  n: Integer;
begin
  if IsNaN(AError) or (AError = 0) then
    Result := Format('%g', [AValue])
  else begin
    n := Round(Log10(AError));
    if n < 0 then
      Result := Format('%.*f ± %.*f', [1 - n, AValue, 1 - n, AError])
    else if n = 0 then
      Result := Format('%.1f ± %.1f', [AValue, AError])
    else
      Result := Format('%.0f ± %.0f', [RoundTo(AValue, n), RoundTo(AError, n)]);
  end;
end;


{ TfrmMain }

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  s: TStream;
  fs: TFormatSettings;
  i: Integer;
  si: PChartDataItem;
  line: String;
  dyp, dyn: Double;
begin
  if not SaveDialog.Execute then exit;
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  s := TFileStream.Create(SaveDialog.FileName, fmCreate);
  try
    for i := 0 to ListChartSource.Count-1 do begin
      si := ListChartSource.item[i];
      line := Format('%.9g'#9'%.9g', [si^.X, si^.Y], fs);
      if cbShowErrorBars.Checked and ListChartSource.HasYErrorBars then begin
        ListChartSource.GetYErrorBarValues(i, dyp, dyn);
        line := Format('%s'#9'%.9g', [line, (dyp + dyn) / 2], fs);
      end;
      line := line + LineEnding;
      s.WriteBuffer(line[1], Length(line));
    end;
  finally
    s.Free;
  end;
end;

procedure TfrmMain.BtnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenFile(OpenDialog1.Filename);
end;

procedure TfrmMain.cbDrawFitRangeOnlyClick(Sender: TObject);
begin
  FitSeries.DrawFitRangeOnly := cbDrawFitRangeOnly.Checked;
end;

function HarmonicBaseFunc(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := sin(x * (2*Param - 1));
end;

procedure TfrmMain.cbFitEquationSelect(Sender: TObject);
var
  eq: TFitEquation;
begin
  eq := TFitEquation(cbFitEquation.ItemIndex);
  FitSeries.FitEquation := eq;
  edFitOrder.Enabled := (eq = fePolynomial);
  lblFitOrder.Enabled := edFitOrder.Enabled;

  cbFitParam0Fixed.Caption := 'a = ';
  cbFitParam1Fixed.Caption := 'b = ';
  case eq of
    fePolynomial:
      begin
        cbFitParam0Fixed.Caption := 'b0 = ';
        cbFitParam1Fixed.Caption := 'b1 = ';
        edFitParam0.Value := POLY_PARAMS[0];
        edFitParam1.Value := POLY_PARAMS[1];
      end;
    feLinear:
      begin
        edFitParam0.Value := LIN_PARAMS[0];
        edFitParam1.Value := LIN_PARAMS[1];
      end;
    feExp:
      begin
        edFitParam0.Value := EXP_PARAMS[0];
        edFitParam1.Value := EXP_PARAMS[1];
      end;
    fePower:
      begin
        edFitParam0.Value := PWR_PARAMS[0];
        edFitParam1.value := PWR_PARAMS[1];
      end;
    feCustom:
      begin
        FitSeries.ParamCount := 4;
        FitSeries.SetFitBasisFunc(0, @FitBaseFunc_Const, '');
        FitSeries.SetFitBasisFunc(1, @HarmonicBaseFunc, 'sin(x)');
        FitSeries.SetFitBasisFunc(2, @HarmonicBaseFunc, 'sin(3 x)');
        FitSeries.SetFitBasisFunc(3, @HarmonicBaseFunc, 'sin(5 x)');
        cbFitParam0Fixed.Caption := 'b0 = ';
        cbFitParam1Fixed.Caption := 'b1 = ';
        edFitParam0.Value := HARMONIC_PARAMS[0];
        edFitParam1.Value := HARMONIC_PARAMS[1];
      end;
  end;
end;

procedure TfrmMain.cbShowConfidenceIntervalsChange(Sender: TObject);
begin
  UpperConfIntervalSeries.Active := cbShowConfidenceIntervals.Checked;
  LowerConfIntervalSeries.Active := cbShowConfidenceIntervals.Checked;
end;

procedure TfrmMain.cbShowErrorbarsChange(Sender: TObject);
begin
  FitSeries.YErrorbars.Visible := cbShowErrorBars.Checked;
  if CbShowErrorBars.Checked then begin
    if FDemoData then
      FitSeries.ListSource.YErrorBarData.Kind := ebkPercent
    else begin
      FitSeries.ListSource.YErrorBarData.Kind := ebkChartSource;
      FitSeries.ListSource.YErrorBarData.IndexPlus := 1;
    end;
    FitSeries.Pointer.Style := psCircle;
  end else begin
    FitSeries.ListSource.YErrorBarData.Kind := ebkNone;
    FitSeries.Pointer.Style := psDiagCross;
  end;
  if FDemoData then
    CreateData;
end;

procedure TfrmMain.cbShowPredictionIntervalsChange(Sender: TObject);
begin
  UpperPredIntervalSeries.Active := cbShowPredictionIntervals.Checked;
  LowerPredIntervalSeries.Active := cbShowPredictionIntervals.Checked;
end;

procedure TfrmMain.EdPointsCountChange(Sender: TObject);
begin
  CreateData;
end;

procedure TfrmMain.FixedParamsChanged(Sender: TObject);
begin
  FitSeries.FixedParams := PrepareFixedParams;
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
var
  i, n: Integer;
  x, y, xmin, xmax, ymin, ymax: Double;
  xarr, yarr: array of Double;
begin
  RandSeed := 875876;   // Reproducible noise for testing.
  n := EdPointsCount.Value;
  if n = 0 then begin
    MessageDlg('No data', mtError, [mbOK], 0);
    exit;
  end;

  // Calculate test data and store in temporary arrays.
  // This is because noise is relative to the data range in this example.
  xmin := XRANGE[TFitEquation(cbTestFunction.ItemIndex), 0];
  xmax := XRANGE[TFitEquation(cbTestFunction.ItemIndex), 1];
  SetLength(xarr, n);
  SetLength(yarr, n);
  for i := 0 to High(xarr) do begin
    x := xmin + (xmax - xmin) / (n - 1) * i;
    case TFitEquation(cbTestFunction.ItemIndex) of
      fePolynomial:
        y := POLY_PARAMS[0] + POLY_PARAMS[1]*x + POLY_PARAMS[2]*x*x;
      feLinear:
        y := LIN_PARAMS[0] + LIN_PARAMS[1]*x;
      feExp:
        y := EXP_PARAMS[0] * Exp(EXP_PARAMS[1]*x);
      fePower:
        y := PWR_PARAMS[0] * Power(x, PWR_PARAMS[1]);
      feCustom:
        y := HARMONIC_PARAMS[0] + HARMONIC_PARAMS[1] * sin(x) +
             HARMONIC_PARAMS[2] * sin(3*x) + HARMONIC_PARAMS[3] * sin(5*x);
    end;
    xarr[i] := x;
    yarr[i] := y;
  end;

  // Add noise to the y values, and add data to line series.
  ymin := MinValue(yarr);
  ymax := MaxValue(yarr);
  FitSeries.BeginUpdate;
  try
    FitSeries.Clear;
    for i := 0 to High(xarr) do begin
      x := xarr[i];
      y := yarr[i] + randg(0, yarr[i] * edNoiseY.value * 0.01);
      {
      if TFitEquation(cbTestFunction.ItemIndex) = feExp then
        // Make sure that the noise generation does not produce negative
        // values for the exponential data set.
        while y <= 0 do
          y := yarr[i] + maxNoise * (Random - 0.5);
          }
      FitSeries.AddXY(x, y);
    end;
    if CbShowErrorBars.Checked then begin
      FitSeries.ListSource.YErrorBarData.Kind := ebkPercent;
      FitSeries.ListSource.YErrorBarData.ValuePlus := edNoiseY.Value;
    end else
      FitSeries.ListSource.YErrorBarData.Kind := ebkNone;
  finally
    FitSeries.EndUpdate;
    FDemoData := True;
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

function MyFormatFloat(x: Double; StdFormat, ExpFormat: String): String;
begin
  if (abs(x) <= 1E-6) or (abs(x) >= 1E6) then
    Result := Format(ExpFormat, [x])
  else
    Result := Format(StdFormat, [x]);
end;

procedure TfrmMain.FitCompleteHandler(Sender:TObject);
const
  {$IF FPC_FullVersion >= 30004}
  MASK = '%-4s %10s %10s %10s %10s';
  CONF_MASK = '%-4s %10s %10s %10s';
  {$ELSE}
  MASK = '%-4s %10s %10s %10s';
  {$IFEND}
  EXP_FMT = '%.3e';
  STD_FMT = '%.5f';
  PARAM_NAME: array[0..1] of String = ('a', 'b');
var
  i: Integer;
  L: Integer;
  decsep: Char;
  paramName: String;
  confL, confH: Double;
begin
  decsep := DefaultFormatSettings.DecimalSeparator;
  with lbResults.Items do begin
    BeginUpdate;
    try
      Clear;
      DefaultFormatSettings.DecimalSeparator := '.';
      case FitSeries.ErrCode of
        fitOK:
          begin
            Add('PARAMETERS');
            {$IF FPC_FullVersion >= 30004}
            Add(Format(MASK, ['Name', 'Value', 'Std.Error', 't value', 'p (>|t|)']));
            {$ELSE}
            Add(Format(MASK, ['Name', 'Value', 'Std.Error', 't value']));
            {$IFEND}
            for i := 0 to FitSeries.ParamCount - 1 do begin
              case FitSeries.FitEquation of
                fePolynomial, feCustom:
                  paramName := Format('b[%d]', [i]);
                else
                  paramName := PARAM_NAME[i];
              end;
              Add(Format(MASK, [
                paramName,
                MyFormatFloat(FitSeries.Param[i], STD_FMT, EXP_FMT),
                MyFormatFloat(FitSeries.ParamError[i], STD_FMT, EXP_FMT),
                MyFormatFloat(FitSeries.Param_tValue[i], STD_FMT, EXP_FMT)
                {$IF FPC_FullVersion >= 30004},
                MyFormatFloat(FitSeries.Param_pValue[i], STD_FMT, EXP_FMT)
                {$IFEND}
              ]));
            end;
            Add('');
            {$IF FPC_FullVersion >= 30004}
            Add('CONFIDENCE LIMITS');
            Add(Format(CONF_MASK, ['Name', 'Value', 'Lower', 'Upper']));
            for i := 0 to FitSeries.ParamCount - 1 do begin
              case FitSeries.FitEquation of
                fePolynomial, feCustom:
                  paramname := Format('b[%d]', [i]);
                else
                  paramname := PARAM_NAME[i];
              end;
              FitSeries.GetConfidenceLimits(i, confL, confH);
              Add(Format(CONF_MASK, [
                paramName,
                MyFormatFloat(FitSeries.Param[i], STD_FMT, EXP_FMT),
                MyFormatFloat(confL, STD_FMT, EXP_FMT),
                MyFormatFloat(confH, STD_FMT, EXP_FMT)
              ]));
            end;
            Add('');
            {$IFEND}
            Add('ANALYSIS OF VARIANCE');
            lbResults.Canvas.Font.Assign(lbResults.Font);
            FReportDecimals := 5;
            FitSeries.FitStatistics.Report_ANOVA(lbResults.Items, #9, '%.'+IntToStr(FReportDecimals)+'f');
            Add('');
            Add('VARIANCE-COVARIANCE MATRIX');
            FitSeries.FitStatistics.Report_VarCovar(lbResults.Items);

            {$IF FPC_FullVersion >= 30004}
            UpperConfIntervalSeries.OnCalculate := @FitSeries.GetUpperConfidenceInterval;
            LowerConfIntervalSeries.OnCalculate := @FitSeries.GetLowerConfidenceInterval;
            UpperPredIntervalSeries.OnCalculate := @FitSeries.GetUpperPredictionInterval;
            LowerPredIntervalSeries.OnCalculate := @FitSeries.GetLowerPredictionInterval;
            {$IFEND}
          end;
        fitDimError:
          Add('The lengths of the data vectors do not match.');
        fitMoreParamsThanValues:
          Add('There are more fitting parameters than data values.');
        fitNoFitParams:
          Add('No fit parameters specified');
        fitSingular:
          Add('Matrix is (nearly) singular');
      end;
    finally
      EndUpdate;
      DefaultFormatSettings.DecimalSeparator := decsep;
    end;
  end;
end;

procedure TfrmMain.lbResultsDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  s: String;
  p: Integer;
  i: Integer;
  w: Integer;
  lb: TListBox;
begin
  lb := Control as TListbox;
  lb.Canvas.FillRect(ARect);
  s := lb.Items[Index];
  p := pos(#9, lb.Items[Index]);
  if p = 0 then
    lb.Canvas.TextOut(ARect.Left+2, ARect.Top, lb.Items[Index])
  else begin
    s := Copy(lb.Items[Index], 1, p-1);
    lb.Canvas.TextOut(ARect.Left+2, ARect.Top, s);
    s := Copy(lb.Items[Index], p+1, MaxInt);
    if TryStrToInt(s, i) then begin
      p := FReportDecimals + 1;
      while p > 0 do begin
        s := s + ' ';
        dec(p);
      end;
    end;
    w := lb.Canvas.TextWidth(s);
    lb.Canvas.Textout(ARect.Right - w - 2, ARect.Top, s);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  eq: IFitEquationText;
begin
  eq := TFitEquationText.Create;
  with cbTestFunction do begin
    Items.Add(eq.Equation(fePolynomial).Params(POLY_PARAMS));
    Items.Add(eq.Equation(feLinear).Params(LIN_PARAMS));
    Items.Add(eq.Equation(feExp).Params(EXP_PARAMS));
    Items.Add(eq.Equation(fePower).Params(PWR_PARAMS));
    Items.Add('y = 2 + sin(x) + 1/3 * sin(3x) + 1/5 * sin(5x)');
    ItemIndex := Ord(fePolynomial);
  end;

  FitSeries.FitRange.Min := edFitRangeMin.Value;
  FitSeries.FitRange.Max := edFitRangeMax.Value;

  CreateData;

  cbTestFunction.ItemIndex := 0;
  cbFitEquation.ItemIndex := 0;
end;

procedure TfrmMain.OpenFile(const AFilename: String);
var
  L, LC: TStrings;
  x, y, dy: Double;
  i: Integer;
  p: Integer;
  res: Integer;
  s: String;
  delim: Char;
begin
  if not FileExists(AFileName) then begin
    ShowMessage('File not found.');
    exit;
  end;

  FitSeries.Clear;
  L := TStringList.Create;
  LC := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    if pos(';', L[0]) > 0 then
      LC.Delimiter := ';'
    else
    if pos(#9, L[0]) > 0 then
      LC.Delimiter := #9
    else begin
      ShowMessage('Unknown or no delimiter.');
      exit;
    end;

    LC.StrictDelimiter := true;
    LC.DelimitedText := L[0];
    if LC.Count = 3 then begin
      FitSeries.ListSource.YCount := 2;
      FitSeries.ListSource.YErrorBarData.Kind := ebkChartSource;
      FitSeries.ListSource.YErrorBarData.IndexPlus := 1;
    end else begin
      FitSeries.ListSource.YCount := 1;
      FitSeries.ListSource.YErrorBarData.Kind := ebkNone;
    end;

    for i:=0 to L.Count-1 do begin
      LC.DelimitedText := L[i];
      val(LC[0], x, res);
      if res <> 0 then
        Continue;
      val(LC[1], y, res);
      if res <> 0 then
        Continue;
      if FitSeries.ListSource.YCount > 1 then begin
        val(LC[2], dy, res);
        FitSeries.AddXY(x, y, [dy]);
      end else
        FitSeries.AddXY(x, y);
    end;
    FDemoData := false;
  finally
    L.Free;
  end;
end;


function TfrmMain.PrepareFixedParams: String;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result:= '';
  if cbFitParam0Fixed.Checked then
    Result := FloatToStr(edFitParam0.Value, fs);
  if cbFitParam1Fixed.Checked then
    Result := Result + ';' + FloatToStr(edFitParam1.Value, fs);
end;

end.


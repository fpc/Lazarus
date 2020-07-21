{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TAFitUtils;

{$H+}

interface

uses
  SysUtils, Classes, TAChartUtils, TAFitLib;

type
  TFitEquation = (
    fePolynomial,  // y = b0 + b1*x + b2*x^2 + ... bn*x^n
    feLinear,      // y = a + b*x
    feExp,         // y = a * exp(b * x)
    fePower,       // y = a * x^b
    feCustom       // y = b0 + b1*F1(x) + b2*F2(x) + ... bn*Fn(x),
                   //    Fi(x) = custom "fit base function" provided by calling SetFitBasisFunc() method
  );

  IFitEquationText = interface
    function BasisFuncs(const ATexts: array of string): IFitEquationText;
    function DecimalSeparator(AValue: Char): IFitEquationText;
    function Equation(AEquation: TFitEquation): IFitEquationText;
    function X(AText: String): IFitEquationText;
    function Y(AText: String): IFitEquationText;
    function NumFormat(AFormat: String): IFitEquationText;
    function NumFormats(const AFormats: array of String): IFitEquationText;
    function Params(const AParams: array of Double): IFitEquationText;
    function TextFormat(AFormat: TChartTextFormat): IFitEquationText;
    function Get: String;
  end;

  TFitEmptyEquationText = class(TInterfacedObject, IFitEquationText)
  public
    function BasisFuncs(const ATexts: array of string): IFitEquationText;
    function DecimalSeparator(AValue: Char): IFitEquationText;
    function Equation(AEquation: TFitEquation): IFitEquationText;
    function X(AText: String): IFitEquationText;
    function Y(AText: String): IFitEquationText;
    function NumFormat(AFormat: String): IFitEquationText;
    function NumFormats(const AFormats: array of String): IFitEquationText;
    function Params(const AParams: array of Double): IFitEquationText;
    function TextFormat(AFormat: TChartTextFormat): IFitEquationText;
    function Get: String;
  end;

  TFitEquationText = class(TInterfacedObject, IFitEquationText)
  strict private
    FBasisFunc: array of string;
    FDecSep: Char;
    FEquation: TFitEquation;
    FX: String;
    FY: String;
    FNumFormat: String;
    FNumFormats: array of String;
    FParams: array of Double;
    FTextFormat: TChartTextFormat;
    function GetNumFormat(AIndex: Integer): String;
  public
    constructor Create;
    function BasisFuncs(const ATexts: array of string): IFitEquationText;
    function DecimalSeparator(AValue: Char): IFitEquationText;
    function Equation(AEquation: TFitEquation): IFitEquationText;
    function X(AText: String): IFitEquationText;
    function Y(AText: String): IFitEquationText;
    function NumFormat(AFormat: String): IFitEquationText;
    function NumFormats(const AFormats: array of String): IFitEquationText;
    function Params(const AParams: array of Double): IFitEquationText;
    function TextFormat(AFormat: TChartTextFormat): IFitEquationText;
    function Get: String;
  end;

  TFitStatistics = class(TObject)
  private
    fN: Integer;        // number of observations
    fM: Integer;        // number of (adjusted) fit parameters (fixed params not included)
    fSST: Double;       // total sum of squares (yi - ybar)^2
    fSSR: Double;       // regression sum of squares (yhat - ybar)^2
    fSSE: Double;       // error sum of squares (yi - yhat)^2
    fAlpha: Double;     // significance level for hypothesis tests
    fxbar: Double;      // mean x value
    fSSx: Double;       // sum of squares (xi - xbar)³
    fVarCovar: array of array of Double;
    fTValue: Double;    // t-value
    procedure CalcTValue;
    function GetVarCovar(i, j: Integer): Double;
    procedure SetAlpha(AValue: Double);
  public
    constructor Create(aFitResults: TFitResults; aAlpha: Double = 0.05);
    procedure Report_ANOVA(AText: TStrings; ASeparator: String = ': ';
      ANumFormat: String = '%f'; AExpFormat: String = '%.3e'; NaNStr: String = 'n/a');
    procedure Report_VarCovar(AText: TSTrings; ANumFormat: String = '%12.6f');
  public
    function AdjR2: Double;
    function Chi2: Double;
    function DOF: Integer;   // Degrees of freedom
    function F: Double;
    property N: Integer read fN;   // Number of data points
    property M: Integer read fM;   // Number of fit parameters
    function ReducedChi2: Double;
    function R2: Double;
    function ResidualStdError: Double;
    property Alpha: Double read FAlpha write SetAlpha;
    property SST: Double read fSST;
    property SSR: Double read fSSR;
    property SSE: Double read fSSE;
    property VarCovar[i, j: Integer]: Double read GetVarCovar;
    property xBar: Double read fXBar;
    property SSx: Double read fSSx;
  public
    {$IF FPC_FullVersion >= 30004}
    function Fcrit: Double;
    function pValue: Double;
    property tValue: Double read ftValue;
    {$ENDIF}
  end;

  operator := (AEq: IFitEquationText): String; inline;

implementation

uses
  Math, StrUtils, spe, TAChartStrConsts;

operator := (AEq: IFitEquationText): String;
begin
  Result := AEq.Get;
end;


{ TFitEmptyEquationText }

function TFitEmptyEquationText.BasisFuncs(const ATexts: array of string): IFitEquationText;
begin
  Unused(ATexts);
  Result := Self;
end;

function TFitEmptyEquationText.DecimalSeparator(AValue: Char): IFitEquationText;
begin
  Unused(AValue);
  Result := Self;
end;

function TFitEmptyEquationText.Equation(
  AEquation: TFitEquation): IFitEquationText;
begin
  Unused(AEquation);
  Result := Self;
end;

function TFitEmptyEquationText.Get: String;
begin
  Result := '';
end;

function TFitEmptyEquationText.NumFormat(AFormat: String): IFitEquationText;
begin
  Unused(AFormat);
  Result := Self;
end;

function TFitEmptyEquationText.NumFormats(
  const AFormats: array of String): IFitEquationText;
begin
  Unused(AFormats);
  Result := Self;
end;

function TFitEmptyEquationText.Params(
  const AParams: array of Double): IFitEquationText;
begin
  Unused(AParams);
  Result := Self;
end;

function TFitEmptyEquationText.TextFormat(AFormat: TChartTextFormat): IFitEquationText;
begin
  Unused(AFormat);
  Result := Self;
end;

function TFitEmptyEquationText.X(AText: String): IFitEquationText;
begin
  Unused(AText);
  Result := Self;
end;

function TFitEmptyEquationText.Y(AText: String): IFitEquationText;
begin
  Unused(AText);
  Result := Self;
end;

{ TFitEquationText }

constructor TFitEquationText.Create;
begin
  FX := 'x';
  FY := 'y';
  FNumFormat := '%.9g';
  FDecSep := DefaultFormatSettings.DecimalSeparator;
end;

function TFitEquationText.BasisFuncs(const ATexts: array of string): IFitEquationText;
var
  i: Integer;
begin
  SetLength(FBasisFunc, Length(ATexts));
  for i := 0 to High(FBasisFunc) do
    FBasisFunc[i] := ATexts[i];
  Result := Self;
end;

function TFitEquationText.DecimalSeparator(AValue: Char): IFitEquationText;
begin
  FDecSep := AValue;
  Result := self;
end;

function TFitEquationText.Equation(AEquation: TFitEquation): IFitEquationText;
begin
  FEquation := AEquation;
  Result := Self;
end;

function TFitEquationText.Get: String;
const
  TIMES: array[boolean] of string = ('*', '&middot;');
var
  fs: TFormatSettings;
  res: String;

  // Returns the function term, e.g. "x^3"
  function FuncTerm(i: Integer): String;
  const
    POWER: array[boolean] of string = ('%s^%d', '%s<sup>%d</sup>');
  begin
    if FEquation in [feCustom] then
      Result := FBasisFunc[i]
    else
    if i = 0 then
      Result := ''
    else
    if i = 1 then
      Result := FX
    else
      Result := Format(POWER[FTextFormat = tfHTML], [FX, i]);
  end;

  // Creates a product term "value*f(x)".
  // "value*" is omitted if 1. "*f(x)" is omitted if constant.
  function ProductTerm(i: Integer): String;
  var
    fx: String;
  begin
    if FParams[i] = 0.0 then
      exit('');
    fx := FuncTerm(i);
    if abs(FParams[i]) <> 1.0 then begin
      Result := Format(GetNumFormat(i), [FParams[i]], fs);
      if fx <> '' then
        Result := Result + TIMES[FTextFormat = tfHTML] + fx;
    end else
    if FParams[i] = -1.0 then
      Result := '-' + FX
    else
      Result := FX;
  end;

  // First term in expression: no plus sign
  // Other terms: both plus and minus signs, enclosed by spaces.
  function FixSign(s: String): String;
  begin
    if (s <> '') and (res <> '') then begin
      if s[1] = '-' then begin
        Insert(' ', s, 2);
        s := ' ' + s;
      end else
        s := ' + ' + s;
    end;
    Result := s;
  end;

  // Creates the constant term for feExp or fePower; includes the multiplication sign.
  function ConstTerm: String;
  begin
    if FParams[0] = 1.0 then
      Result := ''
    else if FParams[0] = -1.0 then
      Result := '-'
    else
      Result := Format(GetNumFormat(0), [FParams[0]], fs) + TIMES[FTextFormat = tfHTML];
  end;

  // Creates the exponential term, e.g. 'exp(-1.2*x)' or 'e<sup>-1.2&middot;x</sup>'
  function ExpTerm: string;
  const
    EXP: array[boolean] of String = ('exp(%s)', 'e<sup>%s</sup>');
  begin
    Result := Format(EXP[FTextFormat = tfHTML], [ProductTerm(1)]);
  end;

  // Creates the power term, e.g. 'x^1.2' or 'x<sup>1.2</sup>'
  function PowerTerm: String;
  var
    mask: String;
  begin
    if FTextFormat = tfNormal then
      mask := '%s^' + GetNumFormat(1)
    else
      mask := '%s<sup>' + GetNumFormat(1) + '</sup>';
    Result := Format(mask, [FX, FParams[1]], fs);
  end;

var
  i: Integer;
begin
  if Length(FParams) = 0 then
    exit('');

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := FDecSep;

  Result := Format('%s = ', [FY]);
  case FEquation of
    feLinear, fePolynomial, feCustom:
      begin
        res := '';
        for i := 0 to High(FParams) do
          res += FixSign(ProductTerm(i));
        Result += res;
      end;
    feExp:
      Result += ConstTerm + ExpTerm;
    fePower:
      Result += ConstTerm + PowerTerm;
  end;
end;

function TFitEquationText.GetNumFormat(AIndex: Integer): String;
begin
  if AIndex < Length(FNumFormats) then
    Result := FNumFormats[AIndex]
  else
    Result := FNumFormat;
end;

function TFitEquationText.NumFormat(AFormat: String): IFitEquationText;
begin
  FNumFormat := AFormat;
  Result := Self;
end;

function TFitEquationText.NumFormats(
  const AFormats: array of String): IFitEquationText;
var
  i: Integer;
begin
  SetLength(FNumFormats, Length(AFormats));
  for i := 0 to High(AFormats) do
    FNumFormats[i] := AFormats[i];
  Result := Self;
end;

function TFitEquationText.Params(
  const AParams: array of Double): IFitEquationText;
var
  i: Integer;
begin
  SetLength(FParams, Length(AParams));
  for i := 0 to High(AParams) do
    FParams[i] := AParams[i];
  Result := Self;
end;

function TFitEquationText.TextFormat(AFormat: TChartTextFormat): IFitEquationText;
begin
  FTextFormat := AFormat;
  Result := Self;
end;

function TFitEquationText.X(AText: String): IFitEquationText;
begin
  FX := AText;
  Result := Self;
end;

function TFitEquationText.Y(AText: String): IFitEquationText;
begin
  FY := AText;
  Result := Self;
end;


{ TFitStatistics }

constructor TFitStatistics.Create(aFitResults: TFitResults;
  aAlpha: Double = 0.05);
var
  i, j, L: Integer;
begin
  fN := aFitResults.N;
  fM := aFitResults.M;
  fSSR := aFitResults.SSR;
  fSSE := aFitResults.SSE;
  fSST := aFitResults.SSR + aFitResults.SSE;
  fAlpha := aAlpha;
  L := Length(aFitResults.CovarianceMatrix);
  SetLength(fVarCovar, L, L);
  for j := 0 to L-1 do
    for i := 0 to L-1 do
      fVarCovar[i, j] := aFitResults.CovarianceMatrix[i, j];
  fXBar := aFitResults.XBar;
  fSSx := aFitResults.SSx;
  CalcTValue;
end;

{ Coefficient of determination, adjusted to number of data points and fit
  parameters. Should be close to 1 ("good" fit). "0" means: "poor" fit }
function TFitStatistics.AdjR2: Double;
begin
  if DOF > 0 then
    Result := 1.0 - (1.0 - R2) * (N - 1) / DOF
  else
    Result := NaN;
end;

procedure TFitStatistics.CalcTValue;
begin
  fTValue := NaN;
  {$IF FPC_FullVersion >= 30004}
  if (fAlpha > 0) and (fN > fM) then
    fTValue := invtdist(fAlpha, fN - fM, 2)
  {$IFEND}
end;

{ Total variance of data values minus calculated values, weighted by
  data error.
  For a "moderately" good fit Chi2 is approximately equal to the degrees of
  freedom (DOF). }
function TFitStatistics.Chi2: Double;
begin
  Result := SSE;
end;

{ Degrees of freedom }
function TFitStatistics.DOF: Integer;
begin
  Result := N - M;
end;

function TFitStatistics.F: Double;
begin
  if (M > 1) and (N <> M) and (SSE <> 0) then
    Result := (SSR/(M-1)) / (SSE/(N-M))
  else
    Result := NaN;
end;

{$IF FPC_FullVersion >= 30004}
function TFitStatistics.Fcrit: Double;
begin
  if (M = 1) then
    Result := NaN
  else
    Result := InvFDist(FAlpha, M-1, N-M);
end;
{$IFEND}

function TFitStatistics.GetVarCovar(i, j: Integer): Double;
begin
  Result := fVarCovar[i, j];
end;

{$IF FPC_FullVersion >= 30004}
{ Probability that the scatter of the data around the fitted curve is by chance.
  Should be several 0.1, the higher the better.
  According to Numerical Recipes, very small (<< 0.1) values mean
  - wrong model
  - error bars too small
  - errors are not normally distributed
  Values close to 1.0 mean
  - error bars overestimaged. }
function TFitStatistics.pValue: Double;
begin
  if DOF > 0 then
    Result := chi2dist(Chi2, DOF)
  else
    Result := NaN;
end;
{$IFEND}

{ Variance normalized to the degrees of freedem. Should be about 1 for
  a "moderately" good fit. }
function TFitStatistics.ReducedChi2: Double;
begin
  if (DOF > 0) then
    Result := SSE / DOF
  else
    Result := NaN;
end;

{ Coefficient of determination: 0 -> "poor" fit, 1 -> "good" fit }
function TFitStatistics.R2: Double;
begin
  Result := SSR / SST;
end;

{ Mean residual standard error of fit: The smaller the better }
function TFitStatistics.ResidualStdError: Double;
begin
  if DOF > 0 then
    Result := sqrt(SSE / DOF)
  else
    Result := NaN;
end;

procedure TFitStatistics.Report_ANOVA(AText: TStrings; ASeparator: String = ': ';
  ANumFormat: String = '%f'; AExpFormat: String = '%.3e'; NaNStr: String = 'n/a');
const
  PRECISION = 3;
begin
  AText.Add(rsFitNumObservations + ASeparator + IntToStr(N));
  AText.Add(rsFitNumFitParams + ASeparator + IntToStr(M));
  AText.Add(rsFitDegreesOfFreedom + ASeparator + IntToStr(DOF));
  AText.Add(rsFitTotalSumOfSquares + ASeparator + FloatToStrEx(SST, PRECISION, ANumFormat, AExpFormat, NaNStr));
  AText.Add(rsFitRegressionSumOfSquares + ASeparator + FloatToStrEx(SSR, PRECISION, ANumFormat, AExpFormat, NaNStr));
  AText.Add(rsFitErrorSumOfSquares + ASeparator + FloatToStrEx(SSE, PRECISION, ANumFormat, AExpFormat, NaNStr));
  AText.Add(rsFitCoefficientOfDetermination + ASeparator + FloatToStrEx(R2, PRECISION, ANumFormat, '', NaNStr));
  AText.Add(rsFitAdjCoefficientOfDetermination + ASeparator + FloatToStrEx(AdjR2, PRECISION, ANumFormat, '', NaNStr));
  AText.Add(rsFitChiSquared + ASeparator + FloatToStrEx(Chi2, PRECISION, ANumFormat, AExpFormat, NaNStr));
  AText.Add(rsFitReducedChiSquared + ASeparator + FloatToStrEx(ReducedChi2, PRECISION, ANumFormat, AExpFormat, NaNStr));
  AText.Add(rsFitResidualStandardError + ASeparator + FloatToStrEx(ResidualStdError, PRECISION, ANumFormat, AExpFormat, NaNStr));
  AText.Add(rsFitVarianceRatio + ASeparator + FloatToStrEx(F, PRECISION, ANumFormat, AExpFormat, NaNStr));
  {
  AText.Add(Format('Fcrit(%d, %d)', [M-1, DOF]) + ASeparator +
    Format(IfThen(Fcrit < 1E-3, FMT, ANumFormat), [Fcrit]));
    }
  {$IF FPC_FullVersion >= 30004}
  AText.Add(rsFitTValue + ASeparator + FloatToStrEx(FtValue, PRECISION, ANumFormat, AExpFormat, NaNStr));
  AText.Add(rsFitPValue + ASeparator + FloatToStrEx(pValue, PRECISION, ANumFormat, AExpFormat, NaNStr));
  {$IFEND}
end;

procedure TFitStatistics.Report_VarCovar(AText: TStrings; ANumFormat: String = '%12.6f');
var
  i, j: Integer;
  s, t: String;
  w: Integer;
begin
  t := Copy(ANumFormat, 1, pos('.', ANumFormat)-1);
  Delete(t, 1, 1);
  w := StrToInt(t);

  for i := 0 to M-1 do begin
    s := '';
    for j := 0 to M-1 do
      if IsNaN(VarCovar[i, j]) then
        s := s + Format('%*s', [w, '---'])
      else
        s := s + Format(ANumFormat, [VarCovar[i, j]]);
    AText.Add(s);
  end;
end;

procedure TFitStatistics.SetAlpha(AValue: Double);
begin
  fAlpha := AValue;
  CalcTValue;
end;

end.


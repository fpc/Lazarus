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
                   //    Fi(x) = custom "fit base function" provided by event
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
      ANumFormat: String = '%f');
    procedure Report_VarCovar(AText: TSTrings; ANumFormat: String = '%12.6f');
  public
    function AdjR2: Double;
    function Chi2: Double;
    function DOF: Integer;   // Degrees of freedom
    function F: Double;
    property N: Integer read fN;
    property M: Integer read fM;
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
  // Note: the constant term is skipped! --> BasisFunc[0] belongs to Index = 1
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
var
  ps: String = '';
  s: String;
  i: Integer;
  fs: TFormatSettings;
begin
  if Length(FParams) = 0 then
    exit('');

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := FDecSep;

  Result := Format('%s = ' + GetNumFormat(0), [FY, FParams[0]], fs);
  if FEquation = feCustom then
    for i := 1 to High(FParams) do begin
      if FParams[i] = 0 then
        Continue;
      if FTextFormat = tfNormal then
        s := '*%s'
      else
        s := '&middot;%s';
      Result += Format(' %s ' + GetNumFormat(i) + s,
        [IfThen(FParams[i] > 0, '+', '-'), Abs(FParams[i]), FBasisFunc[i-1]], fs
      );
    end
  else
  if FEquation in [fePolynomial, feLinear] then
    for i := 1 to High(FParams) do begin
      if FParams[i] = 0 then
        continue;
      if FTextFormat = tfNormal then
      begin
        if i > 1 then ps := Format('^%d', [i]);
        s := '*%s%s';
      end else
      begin
        if i > 1 then ps := Format('<sup>%d</sup>', [i]);
        s := '&middot;%s%s';
      end;
      Result += Format(' %s ' + GetNumFormat(i) + s,
        [IfThen(FParams[i] > 0, '+', '-'), Abs(FParams[i]), FX, ps], fs
      );
    end
  else if (Length(FParams) >= 2) and (FParams[0] <> 0) and (FParams[1] <> 0) then
    case FEquation of
      feExp:
        if FTextFormat = tfNormal then
          Result += Format(' * exp(' + GetNumFormat(1) +' * %s)',
            [FParams[1], FX], fs
          )
        else
          Result += Format(' &middot; e<sup>' + GetNumFormat(1) + '&middot; %s</sup>',
            [FParams[1], FX], fs
          );
      fePower:
        if FTextFormat = tfNormal then
          Result += Format(' * %s^' + GetNumFormat(1),
            [FX, FParams[1]], fs
          )
        else
          Result += Format(' &middot; %s<sup>' + GetNumFormat(1) + '</sup>',
            [FX, FParams[1]], fs
          );
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
  Result := 1.0 - (1.0 - R2) * (N - 1) / DOF;
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
  ANumFormat: String = '%f');
const
  FMT = '%.3f';
begin
  AText.Add(rsFitNumObservations + ASeparator + IntToStr(N));
  AText.Add(rsFitNumFitParams + ASeparator + IntToStr(M));
  AText.Add(rsFitDegreesOfFreedom + ASeparator + IntToStr(DOF));
  AText.Add(rsFitTotalSumOfSquares + ASeparator +
    Format(IfThen(SST > 1E6, FMT, ANumFormat), [SST]));
  AText.Add(rsFitRegressionSumOfSquares + ASeparator +
    Format(IfThen(SST > 1E6, FMT, ANumFormat), [SSR]));
  AText.Add(rsFitErrorSumOfSquares + ASeparator +
    Format(ifThen(SST > 1E6, FMT, ANumFormat), [SSE]));
  AText.Add(rsFitCoefficientOfDetermination + ASeparator + Format(ANumFormat, [R2]));
  AText.Add(rsFitAdjCoefficientOfDetermination + ASeparator + Format(ANumFormat, [AdjR2]));
  AText.Add(rsFitChiSquared + ASeparator + Format(ANumFormat, [Chi2]));
  AText.Add(rsFitReducedChiSquared + ASeparator + Format(ANumFormat, [ReducedChi2]));
  AText.Add(rsFitResidualStandardError + ASeparator + Format(ANumFormat, [ResidualStdError]));
  AText.Add(rsFitVarianceRatio + ASeparator + Format(ANumFormat, [F]));
  {
  AText.Add(Format('Fcrit(%d, %d)', [M-1, DOF]) + ASeparator +
    Format(IfThen(Fcrit < 1E-3, FMT, ANumFormat), [Fcrit]));
    }
  {$IF FPC_FullVersion >= 30004}
  AText.Add(rsFitPValue + ASeparator +
    Format(IfThen(pValue < 1E-3, '%.3e', ANumFormat), [pValue]));
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


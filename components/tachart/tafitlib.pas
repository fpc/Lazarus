{ This unit implements a more versatile linear fit than the routines in unit
  ipf of FCL's numlib.

  Author: Werner Pamler
}

unit TAFitLib;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  typ;   // data types of numlib

type
  TArbFloatArray = array of ArbFloat;
  TArbFloatMatrix = array of array of ArbFloat;

  TFitFunc = function(x: ArbFloat; Param: Integer): ArbFloat; // is nested;

  TFitParam = record
    Func: TFitFunc;
    CustomFunc: TFitFunc;
    CustomFuncName: String;
    Value: ArbFloat;
    Fixed: Boolean;
  end;
  TFitParamArray = array of TFitParam;

  TFitErrCode = (
    fitOK,                     // everything ok
    fitDimError,               // The lengths of the data vectors do not match.
    fitMoreParamsThanValues,   // There are more fitting parameters than data values
    fitNoFitParams,            // No fit parameters specified
    fitSingular,               // Matrix is (nearly) singular
    fitNoBaseFunctions         // No user-provided base functions
  );

  TFitResults = record
    ErrCode: TFitErrCode;
    ParamValues: TArbFloatArray;
    CovarianceMatrix: TArbFloatMatrix;
    N: Integer;                // Number of observations
    M: Integer;                // Number of fit parameters
    SSR: ArbFloat;             // regression sum of squares (yhat - ybar)²
    SSE: ArbFloat;             // error sum of squares (yi - yhat)²
    xbar: ArbFloat;            // mean x value
    SSx: ArbFloat;             // sum of squares (xi - xbar)²
  end;

  { for compatibility with TAChart of Lazarus version <= 1.8.x }
  TSimpleFitResults = record
    ErrCode: TFitErrCode;
    ParamValues: TArbFloatArray;
  end;

function LinearFit(const x, y, dy: TArbFloatArray;
  FitParams: TFitParamArray): TFitResults;

// Some basic fit basis functions for linear least-squares fitting
function FitBaseFunc_Const({%H-}x: ArbFloat; {%H-}Param: Integer): ArbFloat;
function FitBaseFunc_Linear(x: ArbFloat; {%H-}Param: Integer): ArbFloat;
function FitBaseFunc_Square(x: ArbFloat; {%H-}Param: Integer): ArbFloat;
function FitBaseFunc_Cube(x: ArbFloat; {%H-}Param: Integer): ArbFloat;
function FitBaseFunc_Poly(x: ArbFloat; Param: Integer): ArbFloat;
function FitBaseFunc_Sin(x: ArbFloat; Param: Integer): ArbFloat;
function FitBaseFunc_Cos(x: ArbFloat; Param: Integer): ArbFloat;


implementation

uses
  SysUtils,
  math,   // IsNaN
  sle,    // Solving linear system of equations
  spe,    // Incomplete gamma function
  inv;    // Inverse matrix

function FitBaseFunc_Const(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := 1.0;
end;

function FitBaseFunc_Linear(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := x;
end;

function FitBaseFunc_Square(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := x*x;
end;

function FitBaseFunc_Cube(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := x*x*x;
end;

{ Param is the degree of the polynomial term }
function FitBaseFunc_Poly(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := 1.0;
  if Param > 0 then
    while Param > 0 do begin
      Result := Result * x;
      dec(Param);
    end
  else
  if Param < 0 then
    while Param < 0 do begin
      Result := Result / x;
      inc(Param);
    end;
end;

function FitBaseFunc_Sin(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := sin(x * Param);
end;

function FitBaseFunc_Cos(x: ArbFloat; Param: Integer): ArbFloat;
begin
  Result := cos(x * Param);
end;

{ calculates the best-fit value for each y value }
function CalcBestFitValues(const x, y: TArbFloatArray; n, m: Integer;
  FitParams: TFitParamArray): TArbFloatArray;
var
  i, j: Integer;
begin
  SetLength(Result, Length(y));
  for i := 0 to n - 1 do begin
    Result[i] := 0.0;
    for j := 0 to m - 1 do
      Result[i] += fitParams[j].Value * FitParams[j].Func(x[i], j);
  end;
end;

{ Calculates the sum of squares for evaluating the fit statistics }
procedure CalcSumOfSquares(const y, dy, yhat: TArbFloatArray;
  var SSE, SSR: ArbFloat);
var
  hasSig: Boolean;
  ybar: Double;
  sig2, totalsig2: Double;
  i, n: Integer;
begin
  n := Length(y);
  hasSig := (dy <> nil) and (Length(dy) > 0);
  Assert(n = Length(yhat));
  if hasSig then
    Assert(n = Length(dy));

  // Calculate (weighted) average y data value
  ybar := 0.0;
  if hasSig then begin
    totalsig2 := 0.0;
    for i := 0 to n - 1 do begin
      sig2 := 1.0 / sqr(dy[i]);
      totalsig2 := totalsig2 + sig2;
      ybar := ybar + y[i] * sig2;
    end;
    ybar := ybar / totalsig2;
  end else begin
    for i := 0 to n - 1 do
      ybar := ybar + y[i];
    ybar := ybar / n;
  end;

  // Calculate sum of squares
  SSR := 0.0;
  SSE := 0.0;
  if hasSig then
    for i := 0 to n - 1 do begin
      sig2 := 1.0 / sqr(dy[i]);
      SSR += sqr(yhat[i] - ybar) * sig2;
      SSE += sqr(y[i] - yhat[i]) * sig2;
    end
  else
    for i := 0 to n - 1 do begin
      SSR += sqr(yhat[i] - ybar);
      SSE += sqr(y[i] - yhat[i]);
    end;
end;

{ Fits a linear combination of the functions defined in FitParams to the data
  arrays provides in x and y. dy contains the error bars of y (std deviation).
  Besides the fit basis functions, the FitParams also contain information
  whether a parameter will be held fixed during fitting ("FitParams[].Fixed").
  In this case, the fixed parameter value is specified in "FitParams[].Values".
  Fit results are returned as a record TFitResults. The field ErrCode is fitOK
  if the fit was successful or contains an error code otherwise. If ErrCode=fitOK
  the TFitResults contain the fitted parameters in field ParamValues[] and their
  standard errors in ParamErros[0], as well as some statistical characterization
  in the other fields (GOF = goodness-of-fit, DOF = degrees of freedom).

  Ref:
  - Numerical Recipes, Ch 14, Modelling of data, General linear least squares }
function LinearFit(const x, y, dy: TArbFloatArray;
  FitParams: TFitParamArray): TFitResults;
var
  alpha: TArbFloatArray;
  beta: TArbFloatArray;
  xx: TArbFloatArray;
  funcs: TArbFloatArray;
  list: Array of Integer;
  ycalc: TArbFloatArray;
  fp: TFitParam;
  n, m, mfit: Integer;
  i, j, k, jk, kj: Integer;
  hasSig: Boolean;
  ym, wt, sig2, chi2: ArbFloat;
  ca: ArbFloat = 0.0;
  term: ArbInt = 0;
begin
  SetLength(Result.ParamValues, 0);
  SetLength(Result.CovarianceMatrix, 0);

  // Check parameters
  n := Length(x);
  if n <> Length(y) then begin
    Result.ErrCode := fitDimError;
    exit;
  end;
  hasSig := (dy <> nil) and (Length(dy) > 0);
  if hasSig and (n <> Length(dy)) then begin
    Result.ErrCode := fitDimError;
    exit;
  end;

  m := Length(FitParams);
  if m < 1 then begin
    Result.ErrCode := fitNoFitParams;
    exit;
  end;
  if m > n then begin
    Result.ErrCode := fitMoreParamsThanValues;
    exit;
  end;

  // Prepare index list for parameters to be used for fitting
  SetLength(list, m);
  mfit := 0;
  for j := 0 to m - 1 do
    if not FitParams[j].Fixed then begin
      list[mfit] := j;
      inc(mfit);
    end;
  SetLength(list, mfit);
  if mfit = 0 then begin
    Result.ErrCode := fitNoFitParams;
    exit;
  end;

  // Prepare array for matrix alpha (mfit x mfit) and vector m (length mfit)
  SetLength(alpha, mfit * mfit);
  SetLength(beta, mfit);
  FillChar(alpha[0], mfit * mfit * SizeOf(ArbFloat), 0);
  FillChar(beta[0], mfit * SizeOf(ArbFloat), 0);

  // Prepare array for values of base functions
  SetLength(funcs, m);

  // Populate matrix alpha and vector beta
  for i := 0 to n - 1 do begin
    // Calculate values of base functions at x[i]
    for j := 0 to m - 1 do
      funcs[j] := FitParams[j].Func(x[i], j);
    // Subtract the function values of the fixed terms from the constant parameter.
    ym := y[i];
    if mfit < m then
      for j := 0 to m - 1 do begin
        fp := FitParams[j];
        if fp.Fixed then ym := ym - fp.Value * funcs[j];
      end;
    // Prepare factor with standard error
    if hasSig then sig2 := 1.0 / sqr(dy[i]) else sig2 := 1.0;
    // Calculate matrix alpha and vector beta. Note: alpha is symmetric
    for j := 0 to mfit - 1 do begin
      wt := funcs[list[j]] * sig2;
      for k := 0 to j do begin
        jk := j * mfit + k;
        alpha[jk] := alpha[jk] + wt * funcs[list[k]];
      end;
      beta[j] := beta[j] + ym * wt;
    end;
  end;
  // Fill above the diagonal for symmetry
  for j := 1 to mfit-1 do
    for k := 0 to j - 1 do begin
      kj := k * mfit + j;
      jk := j * mfit + k;
      alpha[kj] := alpha[jk];
    end;

  // Solve equation system
  //    alpha * xx = beta, xx contains the fitted parameters
  SetLength(xx, mfit);
  slegen(mfit, mfit, alpha[0], beta[0], xx[0], ca, term);

  // Check error conditions
  if term = 3 then begin
    // error in input values: mfit < 1
    Result.ErrCode := fitNoFitParams;
    exit;
    // This parameter error already should have been detected. Something is very wrong...
  end;

  if term = 2 then begin
    // the solution could not have been determined because the matrix is (almost) singular.
    Result.ErrCode := fitSingular;
    exit;
  end;

  // term = 1 --> success
  // Copy solution to correct index of ParamValues of the FitResult record.
  Result.ErrCode := fitOK;
  Result.N := n;
  Result.M := mfit;
  SetLength(Result.ParamValues, m);
  for j := 0 to m - 1 do
    Result.ParamValues[j] := FitParams[j].Value;
  for j := 0 to mfit-1 do begin
    Result.ParamValues[list[j]] := xx[j];
    FitParams[list[j]].Value := xx[j];
  end;

  // Calculate sum of squares for statistical analysis
  ycalc := CalcBestFitValues(x, y, n, m, FitParams);
  CalcSumOfSquares(y, dy, ycalc, Result.SSE, Result.SSR);
  SetLength(ycalc, 0);
  if n > mfit then
    chi2 := Result.SSE / (n - mfit)
  else
    chi2 := NaN;

  // Calculate inverse of alpha. This is (almost) the variance-covariance matrix
  invgen(mfit, mfit, alpha[0], term);
  // Get variance-covariance matrix.
  for i:=0 to High(alpha) do
    alpha[i] := alpha[i] * chi2;

  if term = 1 then begin
    // Extract variance/covariance matrix
    SetLength(Result.CovarianceMatrix, m, m);
    for j := 0 to m - 1 do
      for i := 0 to m - 1 do
        Result.CovarianceMatrix[i, j] := NaN;
    for k := 0 to High(alpha) do begin
      j := k div mfit;
      i := k mod mfit;
      Result.CovarianceMatrix[list[i], list[j]] := alpha[k];
      Result.CovarianceMatrix[list[j], list[i]] := alpha[k];
    end;
  end;

  // Calculate x mean and sum of squares (needed for confidence intervals)
  Result.xbar := 0;
  for i := 0 to n - 1 do Result.xbar += x[i];
  Result.xbar := Result.xbar / n;
  Result.SSx := 0;
  for i := 0 to n - 1 do Result.SSx += sqr(x[i] - Result.xbar);

  // Clean up
  SetLength(alpha, 0);
  SetLength(beta, 0);
  SetLength(funcs, 0);
  SetLength(xx, 0);
end;

end.

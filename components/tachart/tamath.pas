{

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Alexander Klenin, Werner Pamler

}
unit TAMath;

{$H+}

interface

uses
  Classes, SysUtils; 

function CumulNormDistr(AX: Double): Double;
function InvCumulNormDistr(AX: Double): Double;

procedure EnsureOrder(var A, B: Integer); overload; inline;
procedure EnsureOrder(var A, B: Double); overload; inline;

procedure ExpandRange(var ALo, AHi: Double; ACoeff: Double);

function SafeInfinity: Double; inline;
function SafeInRange(AValue, ABound1, ABound2: Double): Boolean;
function SafeMin(A, B: Double): Double;
function SafeNan: Double; inline;

implementation

uses
  Math, spe, TAChartUtils;

// Cumulative normal distribution
// x = -INF ... INF --> Result = 0 ... 1
function CumulNormDistr(AX: Double): Double;
begin
  if AX > 0 then
    Result := (speerf(AX / Sqrt(2)) + 1) * 0.5
  else if AX < 0 then
    Result := (1 - speerf(-AX / Sqrt(2))) * 0.5
  else
    Result := 0;
end;

// Inverse cumulative normal distribution.
// x = 0 ... 1 --> Result = -INF ... +INF
// Algorithm by Peter John Acklam.
// http://home.online.no/~pjacklam/notes/invnorm/index.html
function InvCumulNormDistr(AX: Double): Double;
const
  A: array[1..6] of Double = (
    -3.969683028665376e+01,
    +2.209460984245205e+02,
    -2.759285104469687e+02,
    +1.383577518672690e+02,
    -3.066479806614716e+01,
    +2.506628277459239e+00
  );
  B: array[1..5] of Double = (
    -5.447609879822406e+01,
    +1.615858368580409e+02,
    -1.556989798598866e+02,
    +6.680131188771972e+01,
    -1.328068155288572e+01
  );
  C: array[1..6] of Double = (
    -7.784894002430293e-03,
    -3.223964580411365e-01,
    -2.400758277161838e+00,
    -2.549732539343734e+00,
    +4.374664141464968e+00,
    +2.938163982698783e+00
  );
  D: array[1..4] of Double = (
    +7.784695709041462e-03,
    +3.224671290700398e-01,
    +2.445134137142996e+00,
    +3.754408661907416e+00
  );
  // Switching points between regions.
  P_LOW = 0.02425;
  P_HIGH = 1 - P_LOW;
var
  q, r: Extended;
begin
  if AX <= 0 then
    Result := NegInfinity
  else if AX < P_LOW then begin
    // Rational approximation for lower region.
    q := Sqrt(-2 * Ln(AX));
    Result :=
      (((((C[1] * q + C[2]) * q + C[3]) * q + C[4]) * q + C[5]) * q + C[6]) /
      ((((D[1] * q + D[2]) * q + D[3]) * q + D[4]) * q + 1);
  end
  else if AX <= P_HIGH then begin
    // Rational approximation for central region.
    q := AX - 0.5 ;
    r := q * q ;
    Result :=
      (((((A[1] * r + A[2]) * r + A[3]) * r + A[4]) * r + A[5]) * r + A[6]) * q /
      (((((B[1] * r + B[2]) * r + B[3]) * r + B[4]) * r + B[5]) * r + 1);
  end
  else if AX < 1 then begin
    // Rational approximation for upper region.
    q := Sqrt(-2 * Ln(1 - AX));
    Result :=
      -(((((C[1] * q + C[2]) * q + C[3]) * q + C[4]) * q + C[5]) * q + C[6]) /
      ((((D[1] * q + D[2]) * q + D[3]) * q + D[4]) * q + 1);
  end else
    Result := SafeInfinity;
end;

procedure EnsureOrder(var A, B: Integer); overload; inline;
begin
  if A > B then
    Exchange(A, B);
end;

procedure EnsureOrder(var A, B: Double); overload; inline;
begin
  if A > B then
    Exchange(A, B);
end;

procedure ExpandRange(var ALo, AHi: Double; ACoeff: Double);
var
  d: Double;
begin
  d := AHi - ALo;
  ALo -= d * ACoeff;
  AHi += d * ACoeff;
end;

function SafeInfinity: Double;
begin
  {$PUSH}{$R-}{$Q-}
  Result := Infinity;
  {$POP}
end;

function SafeInRange(AValue, ABound1, ABound2: Double): Boolean;
begin
  EnsureOrder(ABound1, ABound2);
  Result := InRange(AValue, ABound1, ABound2);
end;

function SafeMin(A, B: Double): Double;
begin
  if IsNan(A) then
    Result := B
  else if IsNan(B) then
    Result := A
  else if A < B then
    Result := A
  else
    Result := B;
end;

function SafeNan: Double;
begin
  {$PUSH}{$R-}{$Q-}
  Result := NaN;
  {$POP}
end;

end.


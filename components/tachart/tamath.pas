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

 Authors: Alexander Klenin

}
unit TAMath;

{$H+}

interface

uses
  Classes, SysUtils; 

procedure EnsureOrder(var A, B: Integer); overload; inline;
procedure EnsureOrder(var A, B: Double); overload; inline;

function SafeInfinity: Double; inline;
function SafeInRange(AValue, ABound1, ABound2: Double): Boolean;
function SafeMin(A, B: Double): Double;
function SafeNan: Double; inline;

implementation

uses
  Math, TAChartUtils;

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


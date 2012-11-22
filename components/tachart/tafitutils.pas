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
unit TAFitUtils;

{$H+}

interface

type
  TFitEquation = (
    fePolynomial, // y = b0 + b1*x + b2*x^2 + ... bn*x^n
    feLinear,     // y = a + b*x
    feExp,        // y = a * exp(b * x)
    fePower       // y = a * x^b
  );

  IFitEquationText = interface
    function Equation(AEquation: TFitEquation): IFitEquationText;
    function X(AText: String): IFitEquationText;
    function Y(AText: String): IFitEquationText;
    function NumFormat(AFormat: String): IFitEquationText;
    function NumFormats(const AFormats: array of String): IFitEquationText;
    function Params(const AParams: array of Double): IFitEquationText;
    function Get: String;
  end;

  TFitEquationText = class(TInterfacedObject, IFitEquationText)
  strict private
    FEquation: TFitEquation;
    FX: String;
    FY: String;
    FNumFormat: String;
    FNumFormats: array of String;
    FParams: array of Double;
    function GetNumFormat(AIndex: Integer): String;
  public
    constructor Create;
    function Equation(AEquation: TFitEquation): IFitEquationText;
    function X(AText: String): IFitEquationText;
    function Y(AText: String): IFitEquationText;
    function NumFormat(AFormat: String): IFitEquationText;
    function NumFormats(const AFormats: array of String): IFitEquationText;
    function Params(const AParams: array of Double): IFitEquationText;
    function Get: String;
  end;


implementation

uses
  StrUtils, SysUtils;

{ TFitEquationText }

constructor TFitEquationText.Create;
begin
  FX := 'x';
  FY := 'y';
  FNumFormat := '%.9g';
end;

function TFitEquationText.Equation(AEquation: TFitEquation): IFitEquationText;
begin
  FEquation := AEquation;
  Result := Self;
end;

function TFitEquationText.Get: String;
var
  ps: String = '';
  i: Integer;
begin
  if Length(FParams) = 0 then exit('');
  Result := Format('%s = ' + GetNumFormat(0), [FY, FParams[0]]);
  if FEquation in [fePolynomial, feLinear] then
    for i := 1 to High(FParams) do begin
      if FParams[i] = 0 then continue;
      if i > 1 then ps := Format('^%d', [i]);
      Result += Format(
        ' %s ' + GetNumFormat(i) + '*%s%s',
        [IfThen(FParams[i] > 0, '+', '-'), Abs(FParams[i]), FX, ps]);
    end
  else if (Length(FParams) >= 2) and (FParams[0] <> 0) and (FParams[1] <> 0) then
    case FEquation of
      feExp:
        Result += Format(' * exp(' + GetNumFormat(1) +' * %s)', [FParams[1], FX]);
      fePower:
        Result += Format(' * %s^' + GetNumFormat(1), [FX, FParams[1]]);
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

end.


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
  TAChartUtils;

type
  TFitEquation = (
    fePolynomial, // y = b0 + b1*x + b2*x^2 + ... bn*x^n
    feLinear,     // y = a + b*x
    feExp,        // y = a * exp(b * x)
    fePower       // y = a * x^b
  );

  IFitEquationText = interface
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

  operator :=(AEq: IFitEquationText): String; inline;

implementation

uses
  StrUtils, SysUtils;

operator := (AEq: IFitEquationText): String;
begin
  Result := AEq.Get;
end;


{ TFitEmptyEquationText }

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

end.


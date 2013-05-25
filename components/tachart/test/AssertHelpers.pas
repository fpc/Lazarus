{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit AssertHelpers;

{$mode objfpc}

interface

uses
  FPCUnit;

type
  TAssertHelper = class helper for TAssert
  public
    class procedure AssertEquals(
      const AMessage: String;
      const AExpected, AActual: array of Double; ADelta: Double = 0.0); overload;
    class procedure AssertEquals(
      const AExpected, AActual: array of Double; ADelta: Double = 0.0); overload;
    class procedure AssertEquals(
      const AMessage: String; const AExpected, AActual: array of Boolean); overload;
    class procedure AssertEquals(
      const AExpected, AActual: array of Boolean); overload;
  end;

implementation

uses
  SysUtils;

function BooleanArrayEqual(const AA, AB: array of Boolean): Boolean;
var
  len: Integer;
begin
  len := Length(AA);
  if len <> Length(AB) then exit(false);
  if len = 0 then exit(true);
  Result := CompareByte(AA[0], AB[0], len) = 0;
end;

function BooleanArrayToStr(const AData: array of Boolean): String;
var
  b: Boolean;
begin
  Result := '';
  for b in AData do
    Result += BoolToStr(b, 'true,', 'false,');
  Delete(Result, Length(Result), 1);
end;

function DoubleArrayEqual(const AA, AB: array of Double; ADelta: Double): Boolean;
var
  i: Integer;
begin
  if Length(AA) <> Length(AB) then exit(false);
  for i := 0 to High(AA) do
    if Abs(AA[i] - AB[i]) > ADelta then exit(false);
  Result := true;
end;

function DoubleArrayToStr(const AData: array of Double): String;
var
  a: Double;
begin
  Result := '';
  for a in AData do
    Result += Format('%g,', [a]);
  Delete(Result, Length(Result), 1);
end;

{ TAssertHelper }

class procedure TAssertHelper.AssertEquals(
  const AExpected, AActual: array of Boolean);
begin
  AssertEquals('', AExpected, AActual);
end;

class procedure TAssertHelper.AssertEquals(
  const AExpected, AActual: array of Double; ADelta: Double);
begin
  AssertEquals('', AExpected, AActual, ADelta);
end;

class procedure TAssertHelper.AssertEquals(
  const AMessage: String; const AExpected, AActual: array of Boolean);
var
  expectedStr, actualStr: String;
begin
  if BooleanArrayEqual(AExpected, AActual) then exit;
  expectedStr := BooleanArrayToStr(AExpected);
  actualStr := BooleanArrayToStr(AActual);
  Fail(AMessage + ComparisonMsg(expectedStr, actualStr));
end;

class procedure TAssertHelper.AssertEquals(
  const AMessage: String; const AExpected, AActual: array of Double;
  ADelta: Double);
var
  expectedStr, actualStr: String;
begin
  if DoubleArrayEqual(AExpected, AActual, ADelta) then exit;
  expectedStr := DoubleArrayToStr(AExpected);
  actualStr := DoubleArrayToStr(AActual);
  Fail(AMessage + ComparisonMsg(expectedStr, actualStr));
end;


initialization
  Random; // Workaround for issue #21808.

end.


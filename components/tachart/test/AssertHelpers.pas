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
  end;

implementation

uses
  SysUtils;

function DoubleArrayToStr(const AData: array of Double): String;
var
  a: Double;
begin
  Result := '';
  for a in AData do
    Result += Format('%g,', [a]);
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

{ TAssertHelper }

class procedure TAssertHelper.AssertEquals(
  const AExpected, AActual: array of Double; ADelta: Double);
begin
  AssertEquals('', AExpected, AActual, ADelta);
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
  Fail(AMessage + Format(SCompare, [expectedStr, actualStr]));
end;

end.


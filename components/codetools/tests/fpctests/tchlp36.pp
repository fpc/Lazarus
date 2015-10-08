{ helper methods also influence calls to a parent's method in a derived class }
program tchlp36;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test: Integer;
  end;

  TTestSub = class(TTest)
    function AccessTest: Integer;
  end;

  TTestHelper = class helper for TTest
    function Test: Integer;
  end;

function TTest.Test: Integer;
begin
  Result := 1;
end;

function TTestSub.AccessTest: Integer;
begin
  Result := inherited Test{declaration:TTestHelper.Test};
end;

function TTestHelper.Test: Integer;
begin
  Result := 2;
end;

var
  t: TTestSub;
  res: Integer;
begin
  t := TTestSub.Create;
  res := t.AccessTest{declaration:TTestSub.AccessTest};
  Writeln('f.AccessTest: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.


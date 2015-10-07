{ a method defined in a parent helper has higher priority than a method defined
  in the parent of the extended class - test 1}
program tchlp47;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test: Integer;
  end;

  TTestSub = class(TTest)
  end;

  TTestSubHelper = class helper for TTestSub
    function Test: Integer;
  end;

  TTestSubHelperSub = class helper(TTestSubHelper) for TTestSub
    function AccessTest: Integer;
  end;

function TTest.Test: Integer;
begin
  Result := 1;
end;

function TTestSubHelper.Test: Integer;
begin
  Result := 2;
end;

function TTestSubHelperSub.AccessTest: Integer;
begin
  Result := Test{declaration:TTestSubHelper.Test};
end;

var
  t: TTestSub;
  res: Integer;
begin
  t := TTestSub.Create;
  res := t.AccessTest{declaration:TTestSubHelperSub.AccessTest};
  Writeln('t.AccessTest: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.

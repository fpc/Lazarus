{ the extended class has higher priority than the parent class when
  searching for symbols }
program tchlp41;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test(aRecurse: Boolean): Integer;
  end;

  TTestHelper = class helper for TTest
    function Test(aRecurse: Boolean): Integer;
  end;

  TTestHelperSub = class helper(TTestHelper) for TTest
    function Test(aRecurse: Boolean): Integer;
  end;

function TTest.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TTestHelper.Test(aRecurse: Boolean): Integer;
begin
  Result := 2;
end;

function TTestHelperSub.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test{declaration:TTest.Test}(False)
  else
    Result := 3;
end;

var
  t: TTest;
  res: Integer;
begin
  t := TTest.Create;
  res := t.Test{declaration:TTestHelperSub.Test}(True);
  Writeln('t.Test: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.

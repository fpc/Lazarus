{ test that helpers can access the methods of the parent helper using
  "inherited" }
program tchlp46;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class

  end;

  TTestHelper = class helper for TTest
    function Test(aRecurse: Boolean): Integer;
  end;

  TTestHelperSub = class helper(TTestHelper) for TTest
    function Test(aRecurse: Boolean): Integer;
  end;

function TTestHelper.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TTestHelperSub.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test{declaration:TTestHelper.Test}(False)
  else
    Result := 2;
end;

var
  t: TTest;
  res: Integer;
begin
  t := TTest.Create;
  res := t.Test{declaration:TTestHelperSub.test}(True);
  Writeln('t.Test: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.

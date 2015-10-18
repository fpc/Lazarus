{ without "inherited" the methods of the helper are called first }
program tchlp50;

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

function TTest.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TTestHelper.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := Test{declaration:TTestHelper.Test}(False)
  else
    Result := 2;
end;

var
  t: TTest;
  res: Integer;
begin
  t := TTest.Create;
  res := t.Test{declaration:TTestHelper.Test}(True);
  Writeln('t.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.

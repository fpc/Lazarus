{ methods of the extended class can be called using "inherited" }
program tchlp40;

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
    Result := inherited Test{declaration:TTest.Test}(False)
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
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.

{ a class helper can access methods defined in the parent of the extended
  class }
program tchlp49;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test(aRecurse: Boolean): Integer;
  end;

  TTestSub = class(TTest)
  end;

  TTestSubHelper = class helper for TTestSub
    function Test(aRecurse: Boolean): Integer;
  end;

function TTest.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TTestSubHelper.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test{declaration:TTest.Test}(False)
  else
    Result := 2;
end;

var
  t: TTestSub;
  res: Integer;
begin
  t := TTestSub.Create;
  res := t.Test{declaration:TTestSubHelper.Test}(True);
  Writeln('t.Test: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.


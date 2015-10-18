{ the extended type is searched first for a inherited method even if it's
  defined as "override" }
program tchlp42;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test(aRecurse: Boolean): Integer; virtual;
  end;

  TObjectHelper = class helper for TObject
    function Test(aRecurse: Boolean): Integer; virtual;
  end;

  TTestHelper = class helper(TObjectHelper) for TTest
    function Test(aRecurse: Boolean): Integer; override;
  end;

function TTest.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TObjectHelper.Test(aRecurse: Boolean): Integer;
begin
  Result := 2;
end;

function TTestHelper.Test(aRecurse: Boolean): Integer;
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
  res := t.Test{declaration:TTestHelper.Test}(True);
  Writeln('t.Test: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.

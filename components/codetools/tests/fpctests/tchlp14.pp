{ %NORUN }

{ class helpers can access (strict) protected, public and published members -
  here: strict protected }
program tchlp14;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uchlp12;

type
  TTestHelper = class helper for TTest
    function AccessTest: Integer;
  end;

function TTestHelper.AccessTest: Integer;
begin
  Result := Test3;
end;

begin
end.


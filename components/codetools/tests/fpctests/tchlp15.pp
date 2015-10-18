{ %NORUN }

{ class helpers can access (strict) protected, public and published members -
  here: protected }
program tchlp15;

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
  Result := Test4;
end;

begin
end.


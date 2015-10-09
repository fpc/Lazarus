{ helpers may override existing default properties }
program tchlp7;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
  private
    function GetTest(aIndex: Integer): String;
  public
    property Test[Index: Integer]: String read GetTest; default;
  end;

  TTestHelper = class helper for TTest
    function GetTest(aIndex: Integer): Integer;
    property Test[Index: Integer]: Integer read GetTest; default;
  end;

function TTest.GetTest(aIndex: Integer): String;
begin
  Result := chr(aIndex);
end;

function TTestHelper.GetTest(aIndex: Integer): Integer;
begin
  Result := aIndex;
end;

var
  t: TTest;
  res1: Integer;
begin
  t := TTest.Create;
  res1{guesstype:integer} := t[3];
  res2{guesstype:integer} := t.GetTest(3);
  Writeln('value: ', res);
  if res <> 3 then
    Halt(1);
  Writeln('ok');
end.

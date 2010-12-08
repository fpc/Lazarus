program WatchesPrg;
{$H-}

uses sysutils;

procedure Foo;
var
  TestInt: Integer;
  TesTShortString: String[10];
  TestAnsiString: AnsiString;
  TestPChar: PChar;

  function SubFoo(var AVal1: Integer; AVal2: Integer) : Integer;
  begin
    AVal1 := 2 * AVal2;
    inc(AVal2);
  end;

begin
  TestInt := 3;
  TesTShortString := IntToStr(TestInt);
  TestAnsiString := TesTShortString + ' Foo';
  TestPChar := @TestAnsiString[2];
  SubFoo(TestInt, 5);
  writeln(TestPChar);
end;

begin
  Foo
end.

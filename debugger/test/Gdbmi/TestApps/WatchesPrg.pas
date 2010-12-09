program WatchesPrg;
{$H-}

uses sysutils;

procedure Foo(
  ArgAnsiString1: AnsiString; var ArgAnsiString2: AnsiString; const ArgAnsiString3: AnsiString;
  ArgChar1: Char; var ArgChar2: Char; const ArgChar3: Char
);
var
  TestInt: Integer;
  TesTShortString: String[10];
  TestAnsiString: AnsiString;
  TestPChar: PChar;

  function SubFoo(var AVal1: Integer; AVal2: Integer) : Integer;
  begin
    AVal1 := 2 * AVal2;
    Result := AVal2;
    inc(AVal2);   // First BreakBoint
  end;

begin
  TestInt := 3;
  TesTShortString := IntToStr(TestInt) + ':';
  TestAnsiString := TesTShortString + ' Foo';
  TestPChar := @TestAnsiString[2];
  SubFoo(TestInt, 5);
  writeln(TestPChar);
  writeln(ArgAnsiString1, ArgAnsiString2, ArgAnsiString3, ArgChar1, ArgChar2, ArgChar3); // breakpoint 2
end;

var
  a2: ansistring;
  c2: Char;
begin
  a2 := 'def';
  c2 := 'Y';
  Foo('abc', a2, 'ghi', 'X', c2, 'Z');
end.

unit TestGlobals;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  classes, sysutils, process;

var
  PascalTestSuite: TTestSuite;
  NonPascalTestSuite: TTestSuite;
  BugsTestSuite: TTestSuite;

procedure AddToPascalTestSuite(ATestClass: TClass);
procedure AddToNonPascalTestSuite(ATestClass: TClass);
procedure AddToBugsTestSuite(ATest: TTest);

function LinesToStr(Args: array of const): string;

implementation

procedure AddToPascalTestSuite(ATestClass: TClass);
begin
  PascalTestSuite.AddTestSuiteFromClass(ATestClass);
end;

procedure AddToNonPascalTestSuite(ATestClass: TClass);
begin
  NonPascalTestSuite.AddTestSuiteFromClass(ATestClass);
end;

procedure AddToBugsTestSuite(ATest: TTest);
begin
  BugsTestSuite.AddTest(ATest);
end;

function LinesToStr(Args: array of const): string;
var
  s: String;
  i: Integer;
begin
  s:='';
  for i:=Low(Args) to High(Args) do
    case Args[i].VType of
      vtChar:         s += Args[i].VChar+LineEnding;
      vtString:       s += Args[i].VString^+LineEnding;
      vtPChar:        s += Args[i].VPChar+LineEnding;
      vtWideChar:     s += AnsiString(Args[i].VWideChar)+LineEnding;
      vtPWideChar:    s += AnsiString(Args[i].VPWideChar)+LineEnding;
      vtAnsiString:   s += AnsiString(Args[i].VAnsiString)+LineEnding;
      vtWidestring:   s += AnsiString(WideString(Args[i].VWideString))+LineEnding;
      vtUnicodeString:s += AnsiString(UnicodeString(Args[i].VUnicodeString))+LineEnding;
    end;
  Result:=s;
end;

initialization
  GetTestRegistry.TestName := 'All tests';
  PascalTestSuite := TTestSuite.Create('Pascal tests');
  GetTestRegistry.AddTest(PascalTestSuite);
  NonPascalTestSuite := TTestSuite.Create('No Pascal tests');
  GetTestRegistry.AddTest(NonPascalTestSuite);
  BugsTestSuite := TTestSuite.Create('Bugs');
  GetTestRegistry.AddTest(BugsTestSuite);

end.


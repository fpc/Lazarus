unit TestGlobals;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit;

type
  TAssertHelper = class helper for TAssert
  public
    class procedure AssertEqualsFileName(const AMessage: string; Expected, Actual: string);
  end;


function LinesToStr(Args: array of const): string;

implementation

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

{ TAssertHelper }

class procedure TAssertHelper.AssertEqualsFileName(const AMessage: string; Expected, Actual: string);
begin
  AssertTrue(ComparisonMsg(AMessage ,Expected, Actual), SameFileName(Expected,Actual),CallerAddr);
end;

end.


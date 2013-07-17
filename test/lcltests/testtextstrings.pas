{
 Test all with:
    ./runtests --format=plain --suite=TTestTextStrings

 Test specific with:
    ./runtests --format=plain --suite=TestTextStringsBasic
}
unit TestTextStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, ExtCtrls, TextStrings,
  fpcunit, testglobals;

type

  { TTestTextStrings }

  TTestTextStrings = class(TTestCase)
  published
    procedure TestTextStringsBasic;
  end;

implementation

{ TTestTextStrings }

procedure TTestTextStrings.TestTextStringsBasic;
var
  ts: TTextStrings;
begin
  ts:=TTextStrings.Create;
  try
    ts.AddObject('a',TObject(123));
    ts.AddObject('b',TObject(234));
    AssertEquals('ab',2,ts.Count);
    AssertEquals('a at 0','a',ts[0]);
    AssertEquals('b at 1','b',ts[1]);
    AssertEquals('123 at 0',123,integer(PtrUInt(ts.Objects[0])));
    AssertEquals('234 at 1',234,integer(PtrUInt(ts.Objects[1])));

    ts.Delete(0);
    AssertEquals('b',1,ts.Count);
    AssertEquals('b at 0','b',ts[0]);
    AssertEquals('234 at 0',234,integer(PtrUInt(ts.Objects[0])));
  finally
    ts.Free;
  end;
end;

initialization
  AddToLCLTestSuite(TTestTextStrings);

end.


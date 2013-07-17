{
 Test all with:
    ./runtests --format=plain --suite=TTestTextStrings

 Test specific with:
    ./runtests --format=plain --suite=TestTextStringsBasic
    ./runtests --format=plain --suite=TestTextStringsLists
    ./runtests --format=plain --suite=TestTextStringsTexts
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
    procedure TestTextStringsLists;
    procedure TestTextStringsTexts;
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

procedure TTestTextStrings.TestTextStringsLists;
var
  ts1: TTextStrings;
  ts2: TTextStrings;
begin
  ts1:=TTextStrings.Create;
  ts2:=TTextStrings.Create;
  try
    ts1.AddObject('a',TObject(123));
    ts1.AddObject('b',TObject(234));
    ts2.AddObject('c',TObject(345));
    ts2.AddObject('d',TObject(456));
    ts1.AddStrings(ts2);

    AssertEquals('count',4,ts1.Count);
    AssertEquals('a at 0','a',ts1[0]);
    AssertEquals('123 at 0',123,integer(PtrUInt(ts1.Objects[0])));
    AssertEquals('b at 1','b',ts1[1]);
    AssertEquals('234 at 1',234,integer(PtrUInt(ts1.Objects[1])));
    AssertEquals('c at 2','c',ts1[2]);
    AssertEquals('345 at 2',345,integer(PtrUInt(ts1.Objects[2])));
    AssertEquals('d at 3','d',ts1[3]);
    AssertEquals('456 at 3',456,integer(PtrUInt(ts1.Objects[3])));
  finally
    ts1.Free;
    ts2.Free;
  end;
end;

procedure TTestTextStrings.TestTextStringsTexts;
var
  ts1: TTextStrings;
begin
  ts1:=TTextStrings.Create;
  try
    ts1.Text:='asdasdasd asdasdasdasd asdqawrqwr'#13#10'asdasdasd asdasdasdasd asdqawrqwr'#13#10#13#10'asdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd asdqawrqwrasdasdasd asdasdasdasd'#13#10;
    AssertEquals('count',4,ts1.Count);
  finally
    ts1.Free;
  end;
end;

initialization
  AddToLCLTestSuite(TTestTextStrings);

end.


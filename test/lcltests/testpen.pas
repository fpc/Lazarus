unit TestPen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit,
  Interfaces, LCLType, LCLIntf,
  testglobals;

type

  { TTestPen }

  TTestPen = class(TTestCase)
  published
    procedure TestOne;
  end;

implementation

{ TTestPen }

procedure TTestPen.TestOne;
var
  APen: HPEN;
  LP, Test: TLogPen;
begin
  LP.lopnColor := $FF0000;
  LP.lopnStyle := PS_DASHDOTDOT;
  LP.lopnWidth := Point(2, 0);
  APen := CreatePenIndirect(LP);
  AssertFalse('Creating of APen failed', APen = 0);
  //  APen created
  AssertEquals('Wrong return value of GetObject(APen, 0, nil)',
     SizeOf(TLogPen), GetObject(APen, 0, nil));
  GetObject(APen, SizeOf(Test), @Test);
  AssertTrue( 'Pens are not equal', CompareMem(@Test, @LP, SizeOf(TLogPen)));
  DeleteObject(APen);
end;

initialization
  AddToLCLTestSuite(TTestPen);

end.


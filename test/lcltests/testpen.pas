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
    procedure TestTwo;
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

procedure TTestPen.TestTwo;
const
  Dashes: array[0..3] of DWord = (1, 1, 1, 1);
  PenStyle: DWord = PS_GEOMETRIC or PS_USERSTYLE or PS_ENDCAP_FLAT;
  PenWidth: DWord = 1;
var
  lb: TLogBrush;
  Pen: HPen;
  DataSize: Integer;
  ExtPen: PExtLogPen;
begin
  lb.lbColor := $FF;
  lb.lbHatch := 0;
  lb.lbStyle := BS_SOLID;
  Pen := ExtCreatePen(PenStyle, PenWidth, lb, Length(Dashes), @Dashes);
  AssertFalse('Creating of Pen failed', Pen = 0);
  // Pen created
  DataSize := GetObject(Pen, 0, nil);
  AssertEquals('Wrong return value of GetObject(Pen, 0, nil)',
     SizeOf(TExtLogPen) + (Length(Dashes) - 1) * SizeOf(DWord), DataSize);

  ExtPen := AllocMem(DataSize);
  GetObject(Pen, DataSize, ExtPen);
  AssertEquals('Pen Style is not equal', PenStyle, ExtPen^.elpPenStyle);
  AssertEquals('Pen Width is not equal', PenWidth, ExtPen^.elpWidth);
  AssertEquals('Pen Color is not equal', lb.lbColor, ExtPen^.elpColor);
  AssertEquals('Pen Hatch is not equal', lb.lbHatch, QWord(ExtPen^.elpHatch));
  AssertEquals('Pen Brush Style is not equal', lb.lbStyle, ExtPen^.elpBrushStyle);
  AssertEquals('Pen Dashes Count is not equal', Length(Dashes), ExtPen^.elpNumEntries);
  AssertTrue('Pen Dashes are not equal', CompareDWord(Dashes[0], ExtPen^.elpStyleEntry[0], ExtPen^.elpNumEntries)=0);

  FreeMem(ExtPen);
  DeleteObject(Pen);
end;

initialization
  AddToLCLTestSuite(TTestPen);

end.


unit TestBookMarks;
(* Test all kind of SynEditMarks
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, LCLProc,
  SynEdit, SynEditMarks, SynEditKeyCmds;

type

  { TTestBookMarks }

  TTestBookMarks = class(TTestBase)
  protected
    //procedure SetUp; override;
    //procedure TearDown; override;
    //procedure ReCreateEdit; reintroduce;
    function TestText1: TStringArray;
    procedure CheckMarks(Name: String; MarkList: Array of Integer);
    function AddMark(Y, X: Integer) : TSynEditMark;
  published
    procedure TestMarks;
  end;

implementation




{ TTestBookMarks }

function TTestBookMarks.TestText1: TStringArray;
begin
  SetLength(Result, 8);
  Result[0] := 'abc';
  Result[1] := 'def 123;';
  Result[2] := 'xyz 098765';
  Result[3] := '';
  Result[4] := '1';
  Result[5] := '    mno...';
  Result[6] := '  ABCDEF';
  Result[7] := '  321';
end;

procedure TTestBookMarks.CheckMarks(Name: String; MarkList: array of Integer);
var
  m: TSynEditMark;
  i: Integer;
begin
  AssertEquals(Name + ' Count: ', length(MarkList) div 2, SynEdit.Marks.Count);

  for i := 0 to (length(MarkList) div 2) - 1 do begin
    m := SynEdit.Marks[i];
    AssertEquals(Name + ' Mark( '+IntToStr(i)+' ) Line: ',    MarkList[i*2+0], m.Line);
    AssertEquals(Name + ' Mark( '+IntToStr(i)+' ) Column: ',  MarkList[i*2+1], m.Column);
    AssertEquals(Name + ' Mark( '+IntToStr(i)+' ) IndexOf: ', i,               SynEdit.Marks.IndexOf(m));
  end;
end;

function TTestBookMarks.AddMark(Y, X: Integer): TSynEditMark;
begin
  Result := TSynEditMark.Create(SynEdit);
  Result.Line := Y;
  Result.Column := X;
  SynEdit.Marks.Add(Result);
end;

procedure TTestBookMarks.TestMarks;
var
  m, m1, m2: TSynEditMark;

  procedure AssertMarksCountOnLine(Name: String; ALine: Integer; AExpCnt: integer = 0);
  var
    l: TSynEditMarkLine;
    c: Integer;
  begin
    l := SynEdit.Marks.Line[ALine];
    if l = nil
    then c := 0
    else c:= l.Count;
    AssertEquals(Name + ' LineCount for line '+IntToStr(ALine)+' is '+IntToStr(AExpCnt), AExpCnt, c);
  end;

begin
  ReCreateEdit;
  SetLines(TestText1);
  CheckMarks('empty', []);

  m := AddMark(3,2);
  CheckMarks('3/2', [ 3,2 ]);

  AssertMarksCountOnLine('mfl2', 2);
  Assert(SynEdit.Marks.Line[3][0] = m, 'mfl3');

  m1 := AddMark(5,2);
  CheckMarks('3/2 - 5/1', [ 3,2,   5,2 ]);
  AssertMarksCountOnLine('mfl1 /2', 1);
  AssertMarksCountOnLine('mfl2 /2', 2);
  Assert(SynEdit.Marks.Line[3][0] = m, 'mfl3 /2');  Assert(SynEdit.Marks.Line[3].Count = 1, 'mfl3a /2');
  AssertMarksCountOnLine('mfl4 /2', 4);
  Assert(SynEdit.Marks.Line[5][0] = m1, 'mfl5 /2');  Assert(SynEdit.Marks.Line[5].Count = 1, 'mfl5a /2');
  AssertMarksCountOnLine('mfl6 /2', 6);

  m2 := AddMark(3,1);
  // colums are in order, in which they where added
  CheckMarks('3/1 - 3/2 - 5/1', [ 3,2,   3,1,   5,2 ]);
  AssertMarksCountOnLine('mfl1 /3', 1);
  AssertMarksCountOnLine('mfl2 /3', 2);
  Assert(SynEdit.Marks.Line[3][0] = m, 'mfl3 /3');
    Assert(SynEdit.Marks.Line[3][1] = m2, 'mfl3 /3');
    Assert(SynEdit.Marks.Line[3].Count = 2, 'mfl3a /3');
  AssertMarksCountOnLine('mfl4 /3', 4);
  Assert(SynEdit.Marks.Line[5][0] = m1, 'mfl5 /3');  Assert(SynEdit.Marks.Line[5].Count = 1, 'mfl5a /3');
  AssertMarksCountOnLine('mfl6 /3', 6);


  // check movement
  SynEdit.CaretXY := Point(1, 3); // between marks (first mar is not *behind*
  SynEdit.CommandProcessor(ecChar, 'M', nil);
  CheckMarks('ecChar between marks : 3/1 - 3/3 - 5/1', [ 3,3,   3,1,   5,2 ]);

  SynEdit.CaretXY := Point(1, 5); // before mark
  SynEdit.CommandProcessor(ecLineBreak, ' ', nil);
  CheckMarks('ecLineBreak before mark : 3/1 - 3/3 - 6/1', [ 3,3,   3,1,   6,2 ]);

  SynEdit.CaretXY := Point(2, 6); // after mark
  SynEdit.CommandProcessor(ecLineBreak, ' ', nil);
  CheckMarks('ecLineBreak after mark : 3/1 - 3/3 - 6/1', [ 3,3,   3,1,   6,2 ]);

  SynEdit.CaretXY := Point(2, 4); // prev line
  SynEdit.CommandProcessor(ecLineBreak, ' ', nil);
  CheckMarks('ecLineBreak prev line : 3/1 - 3/3 - 7/1', [ 3,3,   3,1,   7,2 ]);

  SynEdit.CaretXY := Point(1, 3); // between marks (first mar is not *behind*
  SynEdit.CommandProcessor(ecDeleteChar, 'M', nil);
  CheckMarks('ecDeleteChar between marks : 3/1 - 3/2 - 5/1', [ 3,2,   3,1,   7,2 ]);

  SynEdit.CaretXY := Point(1, 6); // prev line
  SynEdit.CommandProcessor(ecChar, '>', nil);
  SynEdit.CaretXY := Point(1, 7); // begin line
  SynEdit.CommandProcessor(ecDeleteLastChar, ' ', nil);
  CheckMarks('ecDeleteLine begin line : 3/1 - 3/3 - 6/3', [ 3,2,   3,1,   6,3 ]);

  SynEdit.CaretXY := Point(1, 5); // prev line
  SynEdit.CommandProcessor(ecDeleteLastChar, ' ', nil);
  CheckMarks('ecDeleteLine 2-prev line : 3/1 - 3/3 - 6/3', [ 3,2,   3,1,   5,3 ]);

  SynEdit.CaretXY := Point(1, 4); // direct prev line
  SynEdit.CommandProcessor(ecLineBreak, ' ', nil);
  CheckMarks('ecLineBreak direct-prev line : 3/1 - 3/3 - 7/1', [ 3,2,   3,1,   6,3 ]);

  // more marks
  AddMark(5,1);
  AddMark(1,1);
  AddMark(1,2);
  CheckMarks('add more : 1/1 - 1/2 - 3/2 - 3/1 - 5/1 - 6/3', [ 1,1,   1,2,   3,2,   3,1,   5,1,   6,3 ]);

  m := SynEdit.Marks[1];
  SynEdit.Marks.Delete(1);
  CheckMarks('delete(1): ', [ 1,1,   3,2,   3,1,   5,1,   6,3 ]);
  m.Free;
  CheckMarks('delete(1): ', [ 1,1,   3,2,   3,1,   5,1,   6,3 ]);

  m := SynEdit.Marks[4];
  SynEdit.Marks.Delete(4);
  m.Free;
  CheckMarks('delete(1): ', [ 1,1,   3,2,   3,1,   5,1 ]);

  m := SynEdit.Marks[0];
  SynEdit.Marks.Delete(0);
  m.Free;
  CheckMarks('delete(0): ', [ 3,2,   3,1,   5,1 ]);

  SynEdit.Marks.ClearLine(3);
  CheckMarks('ClearLine(3): ', [ 5,1 ]);

end;

initialization

  RegisterTest(TTestBookMarks); 
end.


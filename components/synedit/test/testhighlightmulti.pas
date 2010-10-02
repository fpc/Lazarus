unit TestHighlightMulti;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, fpcunit, testregistry, TestBase, Forms,
  SynEditHighlighter, SynHighlighterMulti,
  SynHighlighterLFM, SynHighlighterXML, SynHighlighterPas;

type

  { TTestHighlightMulti }

  TTestHighlightMulti = class(TTestBase)
  protected
    procedure SetRealLinesText;
    procedure SetRealLinesText2;
    procedure DumpLines(ALines: TSynHLightMultiVirtualLines);
    procedure DumpSections(ASectList: TSynHLightMultiSectionList);
    procedure DumpRanges(ARangeList: TSynHighlighterRangeList);
    procedure DumpAll(Hl: TSynMultiSyn);
  published
    procedure TestSectionList;
    procedure TestVirtualLines;
    procedure TestMultiHL;
  end;

implementation

{ TTestHighlightMulti }

procedure TTestHighlightMulti.SetRealLinesText;
begin
  ReCreateEdit;
  SetLines(['begin foo',
            '',
            '  Foo(bar);',
            '  abc;',
            '  ',
            'test me some more',
            'xyz 123',
            '',
            'end'
           ]);
end;

procedure TTestHighlightMulti.SetRealLinesText2;
begin
  SetLines(['<mydoc>',
            '<t>foo</t><pas>unit foo;</pas><lfm>object Bar: TForm</lfm>',
            '<t>123</t><pas>uses </pas>a<pas>OtherUnit;</pas>',
            '<lfm>  Left = </lfm>b<lfm>10',
            '  right = 20</lfm><pas>interface</pas>',
            '<pas>type a = Integer;</pas>',
            '<lfm>end</lfm>',
            '<pas>implementation</pas>',
            '<t/>',
            '<pas>end</pas>',
            '</mydoc>'
           ]);
end;

procedure TTestHighlightMulti.DumpLines(ALines: TSynHLightMultiVirtualLines);
var
  i: Integer;
begin
  for i := 0 to ALines.Count - 1 do
    debugln([i,' len=',length(ALines[i]),': ',ALines[i]]);
end;

procedure TTestHighlightMulti.DumpSections(ASectList: TSynHLightMultiSectionList);
var
  s: TSynHLightMultiVirtualSection;
  i: Integer;
begin
  for i := 0 to ASectList.Count - 1 do begin
    s := ASectList[i];
    debugln([i,' Start=',dbgs(s.StartPos),' End=',dbgs(s.EndPos),' VLine=',s.VirtualLine,' TokenStart=',dbgs(s.TokenStartPos),' TokenEnd=',dbgs(s.TokenEndPos)]);
  end;
end;

procedure TTestHighlightMulti.DumpRanges(ARangeList: TSynHighlighterRangeList);
var
  i: Integer;
begin
  for i := 0 to ARangeList.Count - 1 do debugln([i,' Range=',ARangeList[i]]);
end;

procedure TTestHighlightMulti.DumpAll(Hl: TSynMultiSyn);
var
  i: Integer;
begin  // ensure CurrentLines are set
  debugln(['--- Default']);
  DumpLines(hl.DefaultVirtualLines);
  DumpSections(hl.DefaultVirtualLines.SectionList);
  debugln(['-']);
  DumpRanges(TSynHighlighterRangeList(hl.CurrentLines.Ranges[hl]));
  for i := 0 to hl.Schemes.Count - 1 do begin
    debugln(['-- ',i,' ', dbgs(hl.Schemes[i].Highlighter)]);
    DumpLines(hl.Schemes[i].VirtualLines);
    DumpSections(hl.Schemes[i].VirtualLines.SectionList);
  end;
end;

procedure TTestHighlightMulti.TestSectionList;
var
  sl: TSynHLightMultiSectionList;
  Name: String;

  procedure AddSect(y1, x1, y2, x2: Integer; ExpectInsPos: Integer);
  var
    s: TSynHLightMultiVirtualSection;
    i: Integer;
  begin
    i := sl.IndexOfFirstSectionAtLineIdx(y1, x1, True);
    AssertEquals(Format('%s / Insert %d,%d - %d,%d at Pos %d', [Name, Y1, X1, Y2, X2, ExpectInsPos]),
      ExpectInsPos, i);
    s.StartPos.y := y1;
    s.StartPos.x := x1;
    s.EndPos.y := y2;
    s.EndPos.x := x2;
    sl.Insert(i, s);
  end;
  procedure AddSect(v1, Pos: Integer);
  var
    s: TSynHLightMultiVirtualSection;
  begin
    s.VirtualLine := v1;
    s.StartPos.y := 0;
    s.StartPos.x := 0;
    s.EndPos.y := 0;
    s.EndPos.x := 0;
    sl.Insert(Pos, s);
  end;

  procedure TestIndex(AName: String; Y, X: Integer; UseNext: Boolean; Expected: Integer);
  const BoolName: Array [Boolean] of String = ('F', 'T');
  begin
    //debugln([Format('===== %s / %s: Sect %d, %d (%s)', [Name, AName, Y, X, BoolName[UseNext]])]);
    AssertEquals(Format('%s / %s: %d, %d (%s)', [Name, AName, Y, X, BoolName[UseNext]]),
      Expected, sl.IndexOfFirstSectionAtLineIdx(Y, X, UseNext));
  end;
  procedure TestIndex(AName: String; V: Integer; Expected: Integer);
  begin
    //debugln([Format('===== %s / %s: %d, %d (%s)', [Name, AName, Y, X, BoolName[UseNext]])]);
    AssertEquals(Format('%s / %s: Virtual %d', [Name, AName, V]),
      Expected, sl.IndexOfFirstSectionAtVirtualIdx(V));
  end;

begin
  {%region IndexOfFirstSectionAtLineIdx}
  sl := TSynHLightMultiSectionList.Create;
  Name := 'Empty List';
  TestIndex('', -1, -1, False,  -1);
  TestIndex('', -1, -1, True,    0);
  TestIndex('',  0,  0, False,  -1);
  TestIndex('',  1,  0, False,  -1);

  // Add one section, starts and ends are 0/0 (contains exactly the one char at 0/0)
  // Expect to be inserted at list-pos=0
  AddSect(0,0, 0,0,  0);
  Name := 'List (0-0)';
  AssertEquals('Count = 1', sl.Count, 1);
  TestIndex('Before', -1, -1, False,  -1);
  TestIndex('Before', -1, -1, True ,   0);
  TestIndex('Before',  0, -1, False,  -1);
  TestIndex('Before',  0, -1, True ,   0);
  TestIndex('Inside',  0,  0, False,   0);
  TestIndex('Inside',  0,  0, True ,   0);
  TestIndex('After',   0,  1, False,  -1);
  TestIndex('After',   0,  1, True ,   1);
  TestIndex('After',   1, -1, False,  -1);
  TestIndex('After',   1, -1, True ,   1);
  TestIndex('After',   1,  0, False,  -1);
  TestIndex('After',   1,  0, True ,   1);

  // Add 2nd section 1/1 to 3/0 (multi-line)
  // Expect to be inserted at list-pos=1
  AddSect(1,1, 3,0,  1);
  Name := 'List (0-0), (1-3)';
  AssertEquals('Count = 2', sl.Count, 2);
  TestIndex('Before',   -1, -1, False,  -1);
  TestIndex('Before',   -1, -1, True ,   0);
  TestIndex('Before',    0, -1, False,  -1);
  TestIndex('Before',    0, -1, True ,   0);
  TestIndex('Inside 1',  0,  0, False,   0);
  TestIndex('Inside 1',  0,  0, True ,   0);
  TestIndex('After 1',   0,  1, False,  -1);
  TestIndex('After 1',   0,  1, True ,   1);
  TestIndex('Before 2',  1,  0, False,  -1);
  TestIndex('Before 2',  1,  0, True ,   1);
  TestIndex('Begin 2',   1,  1, False,   1);
  TestIndex('Begin 2',   1,  1, True ,   1);
  TestIndex('Inside 2',  1,  2, False,   1);
  TestIndex('Inside 2',  1,  2, True ,   1);
  TestIndex('Inside 2',  2,  0, False,   1);
  TestIndex('Inside 2',  2,  0, True ,   1);
  TestIndex('AtEnd 2',   3,  0, False,   1);
  TestIndex('AtEnd 2',   3,  0, True ,   1);
  TestIndex('After 2',   3,  1, False,  -1);
  TestIndex('After 2',   3,  1, True ,   2);
  TestIndex('After 2',   4,  0, False,  -1);
  TestIndex('After 2',   4,  0, True ,   2);

  AddSect(3,1, 4,0,  2);
  Name := 'List (0-0), (1-3), (3-4)';
  AssertEquals('Count = 3', sl.Count, 3);
  TestIndex('Before',   -1, -1, False,  -1);
  TestIndex('Before',   -1, -1, True ,   0);
  TestIndex('Before',    0, -1, False,  -1);
  TestIndex('Before',    0, -1, True ,   0);
  TestIndex('Inside 1',  0,  0, False,   0);
  TestIndex('Inside 1',  0,  0, True ,   0);
  TestIndex('After 1',   0,  1, False,  -1);
  TestIndex('After 1',   0,  1, True ,   1);
  TestIndex('Before 2',  1,  0, False,  -1);
  TestIndex('Before 2',  1,  0, True ,   1);
  TestIndex('Begin 2',   1,  1, False,   1);
  TestIndex('Begin 2',   1,  1, True ,   1);
  TestIndex('Inside 2',  1,  2, False,   1);
  TestIndex('Inside 2',  1,  2, True ,   1);
  TestIndex('Inside 2',  2,  0, False,   1);
  TestIndex('Inside 2',  2,  0, True ,   1);
  TestIndex('AtEnd 2',   3,  0, False,   1);
  TestIndex('AtEnd 2',   3,  0, True ,   1);
  TestIndex('Begin 3',   3,  1, False,   2);
  TestIndex('Begin 3',   3,  1, True ,   2);
  TestIndex('Inside 3',  3,  2, False,   2);
  TestIndex('Inside 3',  3,  2, True ,   2);
  TestIndex('AtEnd 3',   4,  0, False,   2);
  TestIndex('AtEnd 3',   4,  0, True ,   2);
  TestIndex('After 3',   4,  1, False,  -1);
  TestIndex('After 3',   4,  1, True ,   3);

  AddSect(0,2, 1,0,  1);
  Name := 'List (0-0), (0-1), (1-3), (3-4)';
  AssertEquals('Count = 4', sl.Count, 4);
  TestIndex('Before',   -1, -1, False,  -1);
  TestIndex('Before',   -1, -1, True ,   0);
  TestIndex('Before',    0, -1, False,  -1);
  TestIndex('Before',    0, -1, True ,   0);
  TestIndex('Inside 1',  0,  0, False,   0);
  TestIndex('Inside 1',  0,  0, True ,   0);
  TestIndex('After 1',   0,  1, False,  -1);
  TestIndex('After 1',   0,  1, True ,   1);

  TestIndex('Begin 2',   0,  2, False,   1);
  TestIndex('Begin 2',   0,  2, True ,   1);
  TestIndex('AtEnd 2',   1,  0, False,   1);
  TestIndex('AtEnd 2',   1,  0, True ,   1);

  TestIndex('Begin 3',   1,  1, False,   2);
  TestIndex('Begin 3',   1,  1, True ,   2);
  TestIndex('Inside 3',  1,  2, False,   2);
  TestIndex('Inside 3',  1,  2, True ,   2);
  TestIndex('Inside 3',  2,  0, False,   2);
  TestIndex('Inside 3',  2,  0, True ,   2);
  TestIndex('AtEnd 3',   3,  0, False,   2);
  TestIndex('AtEnd 3',   3,  0, True ,   2);
  TestIndex('Begin 4',   3,  1, False,   3);
  TestIndex('Begin 4',   3,  1, True ,   3);
  TestIndex('Inside 4',  3,  2, False,   3);
  TestIndex('Inside 4',  3,  2, True ,   3);
  TestIndex('AtEnd 4',   4,  0, False,   3);
  TestIndex('AtEnd 4',   4,  0, True ,   3);
  TestIndex('After 4',   4,  1, False,  -1);
  TestIndex('After 4',   4,  1, True ,   4);
  sl.Free;
  {%endregion}

  {%region IndexOfFirstSectionAtVirtualIdx}
  sl := TSynHLightMultiSectionList.Create;
  Name := 'Empty List';
  TestIndex('', -1, -1);
  TestIndex('',  0, -1);
  TestIndex('',  1, -1);

  AddSect(0, 0);
  Name := 'List (0)';
  AssertEquals('Count = 1', sl.Count, 1);
  TestIndex('Before',  0, 0);
  TestIndex('After',   1, 0);

  AddSect(2, 1);
  Name := 'List (0, 2)';
  AssertEquals('Count = 2', sl.Count, 2);
  TestIndex('',  0, 0);
  TestIndex('',  1, 0);
  TestIndex('',  2, 1);
  TestIndex('',  3, 1);

  AddSect(0, 1);
  Name := 'List (0, 0, 2)';
  AssertEquals('Count = 3', sl.Count, 3);
  TestIndex('',  0, 0);
  TestIndex('',  1, 1);
  TestIndex('',  2, 2);
  TestIndex('',  3, 2);

  AddSect(2, 2);
  Name := 'List (0, 0, 2, 2)';
  AssertEquals('Count = 4', sl.Count, 4);
  TestIndex('',  0, 0);
  TestIndex('',  1, 1);
  TestIndex('',  2, 2);
  TestIndex('',  3, 3);

  AddSect(0, 1);
  Name := 'List (0, 0, 0, 2, 2)';
  AssertEquals('Count = 5', sl.Count, 5);
  TestIndex('',  0, 0);
  TestIndex('',  1, 2);
  TestIndex('',  2, 3);
  TestIndex('',  3, 4);

  AddSect(2, 4);
  Name := 'List (0, 0, 0, 2, 2, 2)';
  TestIndex('',  0, 0);
  TestIndex('',  1, 2);
  TestIndex('',  2, 3);
  TestIndex('',  3, 5);

  AddSect(2, 4);
  Name := 'List (0, 0, 0, 2, 2, 2, 2)';
  TestIndex('',  0, 0);
  TestIndex('',  1, 2);
  TestIndex('',  2, 3);
  TestIndex('',  3, 6);

  AddSect(2, 4);
  Name := 'List (0, 0, 0, 2, 2, 2, 2, 2)';
  TestIndex('',  0, 0);
  TestIndex('',  1, 2);
  TestIndex('',  2, 3);
  TestIndex('',  3, 7);

  AddSect(4, 8);
  Name := 'List (0, 0, 0, 2, 2, 2, 2, 2, 4)';
  TestIndex('',  0, 0);
  TestIndex('',  1, 2);
  TestIndex('',  2, 3);
  TestIndex('',  3, 7);
  TestIndex('',  4, 8);
  TestIndex('',  5, 8);

  sl.Free;
  {%endregion}
end;

procedure TTestHighlightMulti.TestVirtualLines;
var
  vl: TSynHLightMultiVirtualLines;
  Name: String;
  r: TSynHighlighterRangeList;

  procedure AddSect(y1, x1, y2, x2, v: Integer; ExpectInsPos: Integer);
  var
    s: TSynHLightMultiVirtualSection;
    i: Integer;
    sl: TSynHLightMultiSectionList;
  begin
    sl := vl.SectionList;
    i := sl.IndexOfFirstSectionAtLineIdx(y1, x1, True);
    AssertEquals(Format('%s / Insert %d,%d - %d,%d at Pos %d', [Name, Y1, X1, Y2, X2, ExpectInsPos]),
      ExpectInsPos, i);
    s.StartPos.y := y1;
    s.StartPos.x := x1;
    s.EndPos.y := y2;
    s.EndPos.x := x2;
    s.VirtualLine := v;
    sl.Insert(i, s);
  end;

  procedure TestVLines(AName: String; ExpLines: Array of String);
  var
    i: Integer;
  begin
    AssertEquals(Format('%s / %s: Count', [Name, AName]), length(ExpLines), vl.Count);
    for i := 0 to high(ExpLines) do
      AssertEquals(Format('%s / %s: Line %d', [Name, AName, i]), ExpLines[i], vl[i]);
  end;

  procedure InitVRange;
  var
    i: Integer;
  begin
    for i := 0 to r.Count - 1 do r.Range[i] := Pointer(PtrUInt(i+1));
  end;
  procedure TestVRange(AName: String; ExpRanges: Array of Integer);
  var
    i: Integer;
  begin
    AssertEquals(Format('%s / %s: Range-Count', [Name, AName]), length(ExpRanges), r.Count);
    for i := 0 to high(ExpRanges) do
      if ExpRanges[i] <> -1 then
        AssertEquals(Format('%s / %s: Line %d', [Name, AName, i]), ExpRanges[i], PtrUInt(r[i]));
  end;

  function RPoint(Y,X: Integer): TPoint;
  begin
    Result := Point(X,Y);
  end;

begin
  SetRealLinesText;
  vl := TSynHLightMultiVirtualLines.Create(SynEdit.TextBuffer);
  r := TSynHighlighterRangeList.Create;
  vl.Ranges[self] := r;

  AssertEquals('Line Count', 0, vl.Count);
  AssertEquals('Line out of range', '', vl[0]);

  AddSect(1,1, 2,2, 0, 0);
  AddSect(2,4, 2,5, 1, 1);
  TestVLines('Line 2, continue from block with vline=0 (1,1-2,2),(2,4-2,5)', ['', '  oo']);

  vl.SectionList.Count := 0;

  AddSect(1,1, 2,5, 0, 0);
  TestVLines('(1,1-2,5)', ['', '  Foo']);
  AddSect(3,3, 3,3, 2, 1);
  TestVLines('(1,1-2,5),(3,3)', ['', '  Foo', 'a']);
  AddSect(3,5, 3,6, 2, 2);
  TestVLines('(1,1-2,5),(3,3),(3,5-3,6)', ['', '  Foo', 'ac;']);
  AddSect(3,1, 3,1, 2, 1);
  TestVLines('(1,1-2,5),(3,3),(3,5-3,6)', ['', '  Foo', ' ac;']);
  AddSect(5,1, 5,4, 3, 4);
  TestVLines('(1,1-2,5)..(5,1-5,4)',      ['', '  Foo', ' ac;', 'test']);
  AddSect(6,5, 8,3, 4, 5);
  TestVLines('(1,1-2,5),..,(6,5-8,3)',    ['', '  Foo', ' ac;', 'test', '123', '', 'end']);

  r.Capacity := 7;
  r.Count := 7;

  Name := 'Scan(1,1-2,5) to (1,1-2,4)';
  InitVRange;
  TestVRange('selftest', [1,2,3,4,5,6,7]);
  vl.PrepareRegionScan(0);
  vl.RegionScanUpdate0rInsertRegion(RPoint(1,1), RPoint(2,4), 0, 0);
  vl.FinishRegionScan(2);
  TestVLines('',    ['', '  Fo', ' ac;', 'test', '123', '', 'end']);
  TestVRange('', [1,2,3,4,5,6,7]);

  Name := 'Scan(1,1-2,4) to (2,1-2,5)';
  InitVRange;
  vl.PrepareRegionScan(0);
  vl.RegionScanUpdate0rInsertRegion(RPoint(2,1), RPoint(2,5), 0, 0);
  vl.FinishRegionScan(2);
  TestVLines('',    ['  Foo', ' ac;', 'test', '123', '', 'end']);
  TestVRange('', [2,3,4,5,6,7]);

  Name := 'Scan (FirstEnd) (2,1-2,5) to (2,1-2,3)';
  InitVRange;
  vl.PrepareRegionScan(0);
  vl.RegionScanUpdateFirstRegionEnd(RPoint(2,3), 0);
  vl.FinishRegionScan(2);
  TestVLines('',    ['  F', ' ac;', 'test', '123', '', 'end']);
  TestVRange('', [1,2,3,4,5,6]);

  Name := 'Scan(2,1-2,3) to (1,1-2,5) del(3,*)';
  InitVRange;
  vl.PrepareRegionScan(1);
  vl.RegionScanUpdate0rInsertRegion(RPoint(1,1), RPoint(2,5), 0, 0);
  vl.FinishRegionScan(3);
  TestVLines('',    ['', '  Foo', 'test', '123', '', 'end']);
  TestVRange('', [-1,-1,3,4,5,6]);

  Name := 'Scan (Last Start) (6,5-8,3) to (6,1-8,3)';
  InitVRange;
  vl.PrepareRegionScan(7);
  vl.RegionScanUpdateLastRegionStart(RPoint(6,1), 0, 7);
  vl.FinishRegionScan(7);
  TestVLines('',    ['', '  Foo', 'test', 'xyz 123', '', 'end']);
  TestVRange('', [1,2,3,4,5,6]);

  (* 0 Start=(x=1,y=1) End=(x=5,y=2) VLine=0
     1 Start=(x=1,y=5) End=(x=4,y=5) VLine=2
     2 Start=(x=1,y=6) End=(x=3,y=8) VLine=3  *)
  Name := 'Scan (Last Start) (6,1-8,3) to (6,2-8,3)';
  InitVRange;
  vl.PrepareRegionScan(7);
  vl.RegionScanUpdateLastRegionStart(RPoint(6,2), 0, 7);
  vl.FinishRegionScan(8);
  TestVLines('',    ['', '  Foo', 'test', 'yz 123', '', 'end']);
  TestVRange('', [1,2,3,4,5,6]);

  (* 0 Start=(x=1,y=1) End=(x=5,y=2) VLine=0
     1 Start=(x=1,y=5) End=(x=4,y=5) VLine=2
     2 Start=(x=2,y=6) End=(x=3,y=8) VLine=3  *)
  Name := 'Scan (Last Start) (6,2-8,3) to (6,3-8,3) del (5,*)';
  InitVRange;
  vl.PrepareRegionScan(5);
  vl.RegionScanUpdateLastRegionStart(RPoint(6,3), 0, 7);
  vl.FinishRegionScan(8);
  TestVLines('',    ['', '  Foo', 'z 123', '', 'end']);
  TestVRange('', [1,2,4,5,6]);

  (* 0 Start=(x=1,y=1) End=(x=5,y=2) VLine=0
     1 Start=(x=3,y=6) End=(x=3,y=8) VLine=2  *)
  Name := 'Scan insert (4,*)';
  InitVRange;
  vl.PrepareRegionScan(3);
  vl.RegionScanUpdate0rInsertRegion(RPoint(4,1), RPoint(4,3), 0, 0);
  vl.FinishRegionScan(5);
  TestVLines('',    ['', '  Foo', '  ', 'z 123', '', 'end']);
  TestVRange('', [1,2,-1,3,4,5]);

  (* 0 Start=(x=1,y=1) End=(x=5,y=2) VLine=0
     1 Start=(x=1,y=4) End=(x=3,y=4) VLine=2
     2 Start=(x=3,y=6) End=(x=3,y=8) VLine=3  *)

  (* RealLine Delete/Insert *)

  Name := 'RealDelete 1,1';
  InitVRange;
  vl.RealLinesDeleted(1,1);
  TestVLines('', ['', '  a', 'st me some more', 'xyz 123', '']);
  TestVRange('', [2,3,4,5,6]);

  (* 0 Start=(x=1,y=1) End=(x=5,y=1) VLine=0
     1 Start=(x=1,y=3) End=(x=3,y=3) VLine=1
     2 Start=(x=3,y=5) End=(x=3,y=7) VLine=2  *)

  Name := 'RealDelete 2,2';
  InitVRange;
  vl.RealLinesDeleted(2,2);
  TestVLines('', ['', 'abc;', '  ', 'tes']);
  TestVRange('', [1,3,4,5]);

  (* 0 Start=(x=1,y=1) End=(x=5,y=1) VLine=0
     1 Start=(x=3,y=3) End=(x=3,y=5) VLine=1  *)

  Name := 'Realinsert 0,1';
  InitVRange;
  vl.RealLinesInserted(0,1);
  TestVLines('', ['  Foo', '', 'test me some more', 'xyz']);
  TestVRange('', [1,2,3,4]);

  (* 0 Start=(x=1,y=2) End=(x=5,y=2) VLine=0
     1 Start=(x=3,y=4) End=(x=3,y=6) VLine=1  *)

  Name := 'Realinsert 5,1';
  InitVRange;
  vl.RealLinesInserted(5,1);
  TestVLines('', ['  Foo', '', 'test me some more', 'xyz 123', '']);
  TestVRange('', [1,2, -1, 3,4]);

  (* 0 Start=(x=1,y=2) End=(x=5,y=2) VLine=0
     1 Start=(x=3,y=4) End=(x=3,y=7) VLine=1  *)

  Name := 'RealDelete 0,4';
  InitVRange;
  vl.RealLinesDeleted(0,4);
  TestVLines('', ['gin foo', '', '  Foo(bar);', '  a']);
  TestVRange('', [2,3,4,5]);

  (* 0 Start=(x=3,y=0) End=(x=3,y=3) VLine=0  *)

  vl.Ranges[self] := nil;
  r.Free;
  vl.Free;

  // Todo: Updates of Range list
end;

procedure TTestHighlightMulti.TestMultiHL;
var
  MultiHl: TSynMultiSyn;
  XmlHl: TSynXMLSyn;
  LfmHl: TSynLFMSyn;
  PasHl: TSynPasSyn;
  LfmScheme, PasScheme: TSynHighlighterMultiScheme;
  Name: String;

  procedure TestHLTokens(AName: STring; ALine: Integer; AExpTokens: Array of string);
  var
    tkpos: PChar;
    tklen: integer;
    i: Integer;
  begin
    MultiHl.StartAtLineIndex(ALine);
    for i := 0 to high(AExpTokens) do begin
      AssertFalse(Format('%s / %s line %d token-no %d: not yet eol', [Name, AName, ALine, i]), MultiHl.GetEol);
      MultiHl.GetTokenEx(tkpos, tklen);
      MultiHl.Next;
      AssertEquals(Format('%s / %s line %d token-no %d ', [Name, AName, ALine, i]), AExpTokens[i], copy(tkpos, 1, tklen));
    end;
    AssertTrue(Format('%s / %s line %d after run get eol', [Name, AName, ALine, i]), MultiHl.GetEol);
  end;

  procedure CreateSetupXmlLfmPas(Order: String);
    procedure CreateLfmScheme;
    begin
      LfmScheme := TSynHighlighterMultiScheme(MultiHl.Schemes.Add);
      LfmScheme.CaseSensitive := False;
      LfmScheme.StartExpr := '<lfm>';
      LfmScheme.EndExpr := '</lfm>';
    end;
    procedure CreatePasScheme;
    begin
      PasScheme := TSynHighlighterMultiScheme(MultiHl.Schemes.Add);
      PasScheme.CaseSensitive := False;
      PasScheme.StartExpr := '<pas>';
      PasScheme.EndExpr := '</pas>';
    end;
  var
    i: Integer;
  begin
    MultiHl := TSynMultiSyn.Create(Form);
    XmlHl := TSynXMLSyn.Create(Form);
    LfmHl := TSynLFMSyn.Create(Form);
    PasHl := TSynPasSyn.Create(Form);
    MultiHl.Name := 'MULTIhl';
    XmlHl.Name := 'XMLhl';
    LfmHl.Name := 'LFMhl';
    PasHl.Name := 'PAShl';

    for i := 1 to length(Order) do begin
      case Order[i] of
        'l': CreateLfmScheme;
        'p': CreatePasScheme;
        'L': LfmScheme.Highlighter := LfmHl;
        'P': PasScheme.Highlighter := PasHl;
        'D': MultiHl.DefaultHighlighter := XmlHl;
        'H': SynEdit.Highlighter := MultiHl;
        't': SetRealLinesText2;
        ' ': ;
      end;
      //if synedit.Highlighter <> nil then begin MultiHl.CurrentLines := SynEdit.TextBuffer; DumpAll(MultiHl); end;
      SynEdit.SimulatePaintText;
      //SynEdit.Invalidate;
      //Application.ProcessMessages;
    end;
  end;

  procedure TearDownXmlLfmPas(Order: String);
  var
    i: Integer;
  begin
    for i := 1 to length(Order) do begin
      case Order[i] of
        'L': LfmScheme.Highlighter := nil;
        'P': PasScheme.Highlighter := nil;
        'D': MultiHl.DefaultHighlighter := nil;
        'H': SynEdit.Highlighter := nil;
        'p': FreeAndNil(PasHl);
        'l': FreeAndNil(LfmHl);
        'x': FreeAndNil(XmlHl);
        'm': FreeAndNil(MultiHl);
        ' ': ;
      end;
      SynEdit.Invalidate;
      Application.ProcessMessages;
    end;
    FreeAndNil(PasHl);
    FreeAndNil(LfmHl);
    FreeAndNil(XmlHl);
    FreeAndNil(MultiHl);
  end;

  procedure TestXmlLfmPas;
  begin
    MultiHl.CurrentLines := SynEdit.TextBuffer;
    //debugln('DEFAULT:'); DumpSections(MultiHl.DefaultVirtualLines.SectionList);
    //debugln('S0:'); DumpSections(MultiHl.Schemes[0].VirtualLines.SectionList);
    //debugln('S1:'); DumpSections(MultiHl.Schemes[1].VirtualLines.SectionList);

    TestHLTokens('', 0, ['<', 'mydoc', '>']);
    TestHLTokens('', 1, ['<','t','>','foo','</','t','>',
                         '<pas>','unit',' ','foo',';','</pas>',
                         '<lfm>','object',' ','Bar',':',' ','TForm','</lfm>']);
    TestHLTokens('', 2, ['<','t','>','123','</','t','>',
                         '<pas>','uses',' ','</pas>','a','<pas>','OtherUnit',';','</pas>']);

    //debugln('DEFAULT:'); DumpSections(MultiHl.DefaultVirtualLines.SectionList); DumpLines(MultiHl.DefaultVirtualLines);
    //debugln('S0:'); DumpSections(MultiHl.Schemes[0].VirtualLines.SectionList);  DumpLines(MultiHl.Schemes[0].VirtualLines);
    //debugln('S1:'); DumpSections(MultiHl.Schemes[1].VirtualLines.SectionList);  DumpLines(MultiHl.Schemes[1].VirtualLines);

    MultiHl.CurrentLines := SynEdit.TextBuffer;
    SynEdit.SimulatePaintText;
    //SynEdit.Invalidate;
    //Application.ProcessMessages; // do the real paint, and see we do not crash
  end;

  procedure RunXmlLfmPas(InitText: String);
  var
    TearDownOrder: String;

    procedure RunOrder(Order: String);
    begin
      PushBaseName('Order='+Order);
      ReCreateEdit;
      SynEdit.Text := InitText;
      CreateSetupXmlLfmPas(Order);
      TestXmlLfmPas;
      TearDownXmlLfmPas(TearDownOrder);
      PopBaseName;
    end;
  begin
    // run the setup in different orders, to check none of them crashes
    TearDownOrder := 'HDLP';
    PushBaseName('TearDownOrder='+TearDownOrder);

    RunOrder('D lL pP H t');
    RunOrder('D lL pP t H');

    RunOrder('H t D lL pP');
    RunOrder('H D t lL pP');
    RunOrder('H D lL t pP');
    RunOrder('H D lL pP t');

    RunOrder('H t lL pP D');
    RunOrder('H lL pP t D');

    RunOrder('D H t lL pP');
    RunOrder('D H lL pP t');

    RunOrder('H t lp D LP');
    RunOrder('H lp t D LP');
    RunOrder('H lp D t LP');
    RunOrder('H lp D LP t');

    RunOrder('t H D lL pP');
    RunOrder('t H lp D LP');

    TearDownOrder := 'mxlp';
    PushBaseName('TearDownOrder='+TearDownOrder);
    RunOrder('D lL pP t H');
    RunOrder('H t D lL pP');

    TearDownOrder := 'xlpm';
    PushBaseName('TearDownOrder='+TearDownOrder);
    RunOrder('D lL pP t H');
    RunOrder('H t D lL pP');

    TearDownOrder := 'DLPH';
    PushBaseName('TearDownOrder='+TearDownOrder);
    RunOrder('D lL pP t H');
    RunOrder('H t D lL pP');

    TearDownOrder := 'LPDH';
    PushBaseName('TearDownOrder='+TearDownOrder);
    RunOrder('D lL pP t H');
    RunOrder('H t D lL pP');
  end;

begin
  form.Width := 700;

  PushBaseName('SetupXmlLfmPas start empty');
  RunXmlLfmPas('');

  PopPushBaseName('SetupXmlLfmPas start empty with line');
  RunXmlLfmPas('' + LineEnding);

  PopPushBaseName('SetupXmlLfmPas start 1 line');
  RunXmlLfmPas('<dummy>foo<lfm>' + LineEnding);
end;

initialization

  RegisterTest(TTestHighlightMulti);
end.


unit TestHighlightMulti;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, LCLProc, testregistry, TestBase, Forms, SynEditHighlighter,
  SynHighlighterMulti, SynHighlighterLFM, SynHighlighterXML, SynHighlighterPas, SynEditKeyCmds,
  LazSynEditText, SynEditTextBuffer, SynEditTypes, LazLoggerBase;

type

  { TTestHighlightMulti }

  TTestHighlightMulti = class(TTestBase)
  private
    FMultiHl: TSynMultiSyn;
    FXmlHl: TSynXMLSyn;
    {FEmptyScheme, }FPasScheme: TSynHighlighterMultiScheme;
    FAtXmlEl, FAtXmlSym, FAtXmlTxt: TSynHighlighterAttributes;
    FAtPasMark, FAtPasSym, FAtPasId, FAtPasKey, {FAtPasSp, }FAtPasCom: TSynHighlighterAttributes;
    FPasHl: TSynPasSyn;

    procedure InitMultiEMpty;
    procedure FinishMultiEmpty;
    procedure InitMultiXmlPasHl;
    procedure FinishMultiXmlPasHl;

  private
    // Track lines that SynEdit invalidates
    FHLCLow, FHLCHigh:Integer;
    procedure ResetHighlightChanged;
    procedure DoHighlightChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
  protected
    procedure SetRealLinesText;
    procedure SetRealLinesText2;
    procedure SetRealLinesText3;
    function  GetRealLinesText4(ALen1, ALen2: Integer): TStringArray;
    procedure DumpLines(ALines: TSynHLightMultiVirtualLines);
    procedure DumpSections(ASectList: TSynHLightMultiSectionList);
    procedure DumpRanges(ARangeList: TSynHighlighterRangeList);
    procedure DumpAll(Hl: TSynMultiSyn);
    procedure CheckTokensForLine(Name: String; HL: TSynCustomHighlighter;
                                 LineIdx: Integer; ExpAttr: Array of TSynHighlighterAttributes);
  published
    procedure TestSectionList;
    procedure TestVirtualLines;
    procedure TestMultiHL;
    procedure TestMultiHLEdit;
    procedure TestMultiHLEdit2;
  end;

implementation

{ TTestHighlightMulti }

procedure TTestHighlightMulti.InitMultiEMpty;
begin
  FMultiHl := TSynMultiSyn.Create(Form);
  FXmlHl := TSynXMLSyn.Create(Form);
  FXmlHl.ElementAttri.Foreground := 255;

  FMultiHl.DefaultHighlighter := FXmlHl;
  //FEmptyScheme := TSynHighlighterMultiScheme(FMultiHl.Schemes.Add);
  FMultiHl.Schemes.Add; // FEmptyScheme

  FAtXmlEl  := FXmlHl.ElementAttri;
  FAtXmlSym := FXmlHl.SymbolAttri;
  FAtXmlTxt := FXmlHl.TextAttri;

  SynEdit.Highlighter := FMultiHl;
  FMultiHl.CurrentLines := SynEdit.TextBuffer;
end;

procedure TTestHighlightMulti.FinishMultiEmpty;
begin
  SynEdit.Highlighter := nil;
  FreeAndNil(FXmlHl);
  FreeAndNil(FMultiHl);
end;

procedure TTestHighlightMulti.InitMultiXmlPasHl;
begin
  FMultiHl := TSynMultiSyn.Create(Form);

  FXmlHl := TSynXMLSyn.Create(Form);
  FXmlHl.ElementAttri.Foreground := 255;
  FMultiHl.DefaultHighlighter := FXmlHl;

  FPasScheme := TSynHighlighterMultiScheme(FMultiHl.Schemes.Add);
  FPasScheme.CaseSensitive := False;
  FPasScheme.StartExpr := '<pas>';
  FPasScheme.EndExpr := '</pas>';
  FPasHl := TSynPasSyn.Create(Form);
  FPasScheme.Highlighter := FPasHl;

  SynEdit.Highlighter := FMultiHl;
  FMultiHl.CurrentLines := SynEdit.TextBuffer;

  FAtXmlEl  := FXmlHl.ElementAttri;
  FAtXmlSym := FXmlHl.SymbolAttri;
  FAtXmlTxt := FXmlHl.TextAttri;

  FAtPasMark:= FPasScheme.MarkerAttri;
  FAtPasSym := FPasHl.SymbolAttri;
  FAtPasId  := FPasHl.IdentifierAttri;
  FAtPasKey := FPasHl.KeyAttri;
  //FAtPasSp  := FPasHl.SpaceAttri;
  FAtPasCom := FPasHl.CommentAttri;
end;

procedure TTestHighlightMulti.FinishMultiXmlPasHl;
begin
  SynEdit.Highlighter := nil;
  FreeAndNil(FXmlHl);
  FreeAndNil(FPasHl);
  FreeAndNil(FMultiHl);
end;

procedure TTestHighlightMulti.ResetHighlightChanged;
begin
  FHLCLow := -1;
  FHLCHigh := -1;
end;

procedure TTestHighlightMulti.DoHighlightChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
begin
  if FHLCLow < 0
  then FHLCLow := AIndex
  else FHLCLow := Min(FHLCLow, AIndex);

  if FHLCHigh < 0
  then FHLCHigh := AIndex + ACount
  else FHLCHigh := Max(FHLCHigh, AIndex + ACount);
end;

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

procedure TTestHighlightMulti.SetRealLinesText3;
begin
  SetLines(['abc',
            'abc',
            'test<pas>unit a;',
            '//Comment',
            '//Comment',
            'uses Foo;',
            '//</pas><x>',
            'def',
            'def',
            'def',
            'x<pas>interface',
            '//Comment',
            '//</pas></x>',
            'abc'
           ]);
end;

function TTestHighlightMulti.GetRealLinesText4(ALen1, ALen2: Integer
  ): TStringArray;
var
  i, c: Integer;
begin
  SetLength(Result, 20);
  Result[0] := 'abc';
  c := 1;

  if ALen1 >= 0 then begin
    Result[c] := 'def';              inc(c);
    Result[c] := 'test<pas>unit a;'; inc(c);
    for i := 0 to ALen1-1 do begin
      Result[c] := 'foo;';              inc(c);
    end;
    Result[c-1] := Result[c-1] + '//</pas></x>';
    Result[c] := 'fed';              inc(c);
  end;

  if ALen2 >= 0 then begin
    Result[c] := 'mno';              inc(c);
    Result[c] := 'test<pas>unit b;'; inc(c);
    for i := 0 to ALen2-1 do begin
      Result[c] := 'bar;';              inc(c);
    end;
    Result[c-1] := Result[c-1] + '//</pas></x>';
    Result[c] := 'omn';              inc(c);
  end;

  Result[c] := 'xyz';              inc(c);
  Result[c] := '';                 inc(c);
  SetLength(Result, c);
end;

procedure TTestHighlightMulti.DumpLines(ALines: TSynHLightMultiVirtualLines);
var
  i: Integer;
begin
  ALines.Debug;
end;

procedure TTestHighlightMulti.DumpSections(ASectList: TSynHLightMultiSectionList);
begin
  ASectList.Debug;
end;

procedure TTestHighlightMulti.DumpRanges(ARangeList: TSynHighlighterRangeList);
var
  i: Integer;
begin
  DebugLnEnter(['>> --- list ', PtrUint(ARangeList)]);
  for i := 0 to ARangeList.Count - 1 do debugln([i,' Range=',ARangeList[i]]);
  DebugLnExit();
end;

procedure TTestHighlightMulti.DumpAll(Hl: TSynMultiSyn);
var
  i: Integer;
begin  // ensure CurrentLines are set
  DebugLnEnter('>>');
  DebugLnEnter(['--> Default / Lines ', PtrUint(hl.DefaultVirtualLines)]);
    DumpLines(hl.DefaultVirtualLines);
  DebugLnExit();
  DebugLnEnter(['--> Ranges  HL: ', PtrUInt(hl)]);
    DumpRanges(TSynHighlighterRangeList(hl.CurrentLines.Ranges[hl]));
  DebugLnExit;
  for i := 0 to hl.Schemes.Count - 1 do begin
    DebugLnEnter(['--> Scheme=',i,' ', dbgs(hl.Schemes[i].Highlighter), ' ', PtrUint(hl.Schemes[i].VirtualLines)]);
      DumpLines(hl.Schemes[i].VirtualLines);
    DebugLnExit;
  end;
  DebugLnExit('<<');
end;

procedure TTestHighlightMulti.CheckTokensForLine(Name: String; HL: TSynCustomHighlighter;
  LineIdx: Integer; ExpAttr: array of TSynHighlighterAttributes);
var
  c: Integer;
  tk: TSynHighlighterAttributes;
  tkName: String;
begin
  HL.StartAtLineIndex(LineIdx);
  c := 0;
  while not HL.GetEol do begin
    if c >= length(ExpAttr) then begin
      inc(c);
      break;
    end;
    //DebugLn([HL.GetToken,' (',HL.GetTokenID ,') at ', HL.GetTokenPos]);
    tk := HL.GetTokenAttribute;
    if tk <> nil
    then tkName := tk.StoredName
    else tkName := '<nil>';
    AssertTrue(Format('%s Attrib Line=%d pos=%d exp=%s got=%s',
                      [Name, LineIdx, c,  ExpAttr[c].StoredName, tkName]),
               ExpAttr[c] = tk);
    HL.Next;
    inc(c);
  end;
  AssertEquals(Name+ 'TokenId Line='+IntToStr(LineIdx)+'  amount of tokens', length(ExpAttr), c );
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
        AssertEquals(Format('%s / %s: Line %d', [Name, AName, i]), ExpRanges[i], Integer(PtrUInt(r[i])));
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
  vl.RegionScanUpdateOrInsertRegion(RPoint(1,1), RPoint(2,4), 0, 0);
  vl.FinishRegionScan(2);
  TestVLines('',    ['', '  Fo', ' ac;', 'test', '123', '', 'end']);
  TestVRange('', [1,2,3,4,5,6,7]);

  Name := 'Scan(1,1-2,4) to (2,1-2,5)';
  InitVRange;
  vl.PrepareRegionScan(0);
  vl.RegionScanUpdateOrInsertRegion(RPoint(2,1), RPoint(2,5), 0, 0);
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
  vl.RegionScanUpdateOrInsertRegion(RPoint(1,1), RPoint(2,5), 0, 0);
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
  vl.RegionScanUpdateOrInsertRegion(RPoint(4,1), RPoint(4,3), 0, 0);
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
if MultiHl.CurrentLines <> nil then DumpAll(MultiHl) else debugln('#');
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
  Name := '';
  form.Width := 700;

  PushBaseName('SetupXmlLfmPas start empty');
  RunXmlLfmPas('');

  PopPushBaseName('SetupXmlLfmPas start empty with line');
  RunXmlLfmPas('' + LineEnding);

  PopPushBaseName('SetupXmlLfmPas start 1 line');
  RunXmlLfmPas('<dummy>foo<lfm>' + LineEnding);
end;

procedure TTestHighlightMulti.TestMultiHLEdit;
var
  i, j, undo, redo, ud, rd: Integer;

begin
  SynEdit.ViewedTextBuffer.AddChangeHandler(senrHighlightChanged, @DoHighlightChanged);
  {%region Issue 0022745}
    PushBaseName('Insert at end, create new section at end');   // Issue 0022745
    SynEdit.ClearAll;
    InitMultiXmlPasHl;

    SynEdit.TestTypeText(1, 1,  '<html>'+#13);
    SynEdit.TestTypeText(  ''+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
    SynEdit.TestTypeText(  ''+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  ''+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
    SynEdit.TestTypeText(  '<pas>'+#13);
    SynEdit.TestTypeText(  '</pas>'+#13);
//DumpAll(FMultiHl);

    FinishMultiXmlPasHl;
    PopBaseName;




    PushBaseName('0022745');   // Issue 0022745
    SynEdit.ClearAll;
    InitMultiXmlPasHl;

    SynEdit.TestTypeText(1, 1,
      '<html>'+#13+
      ''+#13+
      '<pas>'+#13+
      '</pas>'+#13+
      '<pas>'+#13+
      '</pas>'+#13,
      True
      );

      //PushBaseName('Insert "<pas>"');
      //  CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark]);
      //  CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasMark]);
      //  AssertEquals(BaseTestName + 'Section Count def', 0, FMultiHl.DefaultVirtualLines.SectionList.Count);
      //  AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

    FinishMultiXmlPasHl;
    PopBaseName;

  {%endregion}

  {%region Issue 0022519}
    PushBaseName('append after EOT (default scheme)');   // Issue 0022519
    for undo := 0 to 2 do
    for redo := 0 to 2 do begin
      SynEdit.ClearAll;
      SynEdit.Options := SynEdit.Options - [eoGroupUndo];
      InitMultiEMpty;

      PushBaseName('Insert "html"');
        for ud := 0 to undo do begin
          SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1), '<html>');
          for rd := 0 to redo do begin
            CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
            AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
            if rd < redo then begin SynEdit.Undo; SynEdit.Redo; end
          end;
          if ud < undo then SynEdit.Undo;
        end;

      PopPushBaseName('Insert "a"');
        //SynEdit.SetTextBetweenPoints(Point(1,2), Point(1,2), '<a>');
        SynEdit.CaretXY := point(7,1);
        SynEdit.CommandProcessor(ecLineBreak, '', nil);
        SynEdit.CommandProcessor(ecChar, '<', nil);
        SynEdit.CommandProcessor(ecChar, 'a', nil);
        for ud := 0 to undo do begin
          SynEdit.CommandProcessor(ecChar, '>', nil);
          for rd := 0 to redo do begin
            CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
            CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
            AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
            if rd < redo then begin SynEdit.Undo; SynEdit.Redo; end
          end;
          if ud < undo then SynEdit.Undo;
        end;

      PopPushBaseName('Insert empty');
        for ud := 0 to undo do begin
          SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1), ''+LineEnding);
          for rd := 0 to redo do begin
            CheckTokensForLine('1st line=html', FMultiHl, 0, []);
            CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
            CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
            AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
            if rd < redo then begin SynEdit.Undo; SynEdit.Redo; end
          end;
          if ud < undo then SynEdit.Undo;
        end;

      PopPushBaseName('Delete empty');
        for ud := 0 to undo do begin
          SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,2), '');
          for rd := 0 to redo do begin
            CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
            CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
            AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
            if rd < redo then begin SynEdit.Undo; SynEdit.Redo; end
          end;
          if ud < undo then SynEdit.Undo;
        end;
    end;
    SynEdit.Options := SynEdit.Options + [eoGroupUndo];


    SynEdit.ClearAll;
    PushBaseName('Insert "html"');
      SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1), '<html>'+LineEnding);
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

    PopPushBaseName('Insert "b"');
      SynEdit.SetTextBetweenPoints(Point(1,2), Point(1,2), '<b>'+LineEnding);
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

    PushBaseName('Insert "foo"');
      SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1), '<foo>x'+LineEnding);
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym, FAtXmlTxt]);
      CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

    SynEdit.Undo;
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
    SynEdit.Undo;
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
    SynEdit.Undo;

    SynEdit.Redo;
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
    SynEdit.Redo;
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
    SynEdit.Redo;
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym, FAtXmlTxt]);
      CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

    SynEdit.Undo;
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
    SynEdit.Undo;
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
    SynEdit.Undo;

    FinishMultiEmpty;
    PopBaseName;
    PopBaseName;
  {%endregion}

  {%region edit text in single default region (scan end before eot) // Issue 0022519}
    for j := 0 to 5 do begin
      // 0: do a loop for lines 1 to 5 in the same setup
      // 1..5 do each line with an entire setup of it's own
      PushBaseName('edit text in single default region (scan end before eot) j='+ IntToStr(j));   // Issue 0022519
      SynEdit.ClearAll;
      InitMultiEMpty;

      PushBaseName('Insert "html"');
        SynEdit.TestTypeText(1, 1, '<html>'+#13+'a<body>');
        CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
        CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlTxt, FAtXmlSym, FAtXmlEl, FAtXmlSym]);
        AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

      PushBaseName('Insert "p"');
        SynEdit.TestTypeText(1, 2, '<p>'+#13+'</p>a'+#13+'<foo></foo>'+#13);
        CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
        CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
        CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtXmlSym, FAtXmlEl, FAtXmlSym, FAtXmlTxt]);
        CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtXmlSym, FAtXmlEl, FAtXmlSym,FAtXmlSym, FAtXmlEl, FAtXmlSym]);
        CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtXmlTxt, FAtXmlSym, FAtXmlEl, FAtXmlSym]);
        AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

      i := j;
      if j = 0 then i := 1;
      while ( (j = 0) and (i <= 5)) or (i = j) do begin
        PushBaseName('"a" line '+IntToStr(i));
          SynEdit.TestTypeText(3, i, 'a');
          CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtXmlSym, FAtXmlEl, FAtXmlSym, FAtXmlTxt]);
          CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtXmlSym, FAtXmlEl, FAtXmlSym,FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtXmlTxt, FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);


        PushBaseName('"a" line '+IntToStr(i)+' undo');
          SynEdit.Undo;
          CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtXmlSym, FAtXmlEl, FAtXmlSym, FAtXmlTxt]);
          CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtXmlSym, FAtXmlEl, FAtXmlSym,FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtXmlTxt, FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

        PushBaseName('"a" line '+IntToStr(i)+' redo');
          SynEdit.Redo;
          CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtXmlSym, FAtXmlEl, FAtXmlSym, FAtXmlTxt]);
          CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtXmlSym, FAtXmlEl, FAtXmlSym,FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtXmlTxt, FAtXmlSym, FAtXmlEl, FAtXmlSym]);
          AssertEquals(BaseTestName + 'Section Count', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);

        inc(i);
      end;

      FinishMultiEmpty;
      PopBaseName;
      PopBaseName;
    end; // for j
  {%endregion}

  {%region edit text in single pascal region (scan end before eot) // Issue 0022519}
    for i := 1 to 6 do begin
      PushBaseName('edit text in single pascal region (scan end before eot) j='+ IntToStr(j));   // Issue 0022519
      SynEdit.ClearAll;
      InitMultiXmlPasHl;

      PushBaseName('Insert "<pas>"');
        SynEdit.TestTypeText(1, 1, '<pas>'+#13+'</pas>');
        CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark]);
        CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasMark]);
        AssertEquals(BaseTestName + 'Section Count def', 0, FMultiHl.DefaultVirtualLines.SectionList.Count);
        AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

      PushBaseName('Insert "unit..."');
        SynEdit.TestTypeText(1, 2, 'unit'+#13+'Foo;'+#13+'uses'+#13+'Bar;'+#13);
//DumpAll(FMultiHl);
        CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark]);
        CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasKey]);          // unit
        CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasId, FAtPasSym]); // Foo;
        CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasKey]);         // uses
        CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasId, FAtPasSym]); // Bar;
        CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasMark]);
        AssertEquals(BaseTestName + 'Section Count def', 0, FMultiHl.DefaultVirtualLines.SectionList.Count);
        AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

      PushBaseName('"//" line '+IntToStr(i));
        if i = 1
        then SynEdit.TestTypeText(6, i, '//')
        else if i = 6
        then SynEdit.TestTypeText(1, i, '//')
        else SynEdit.TestTypeText(5, i, '//');
//DumpAll(FMultiHl);
        if i = 1
        then CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark, FAtPasCom])
        else CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark]);
        if i = 2
        then CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasKey, FAtPasCom])          // unit
        else CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasKey]);          // unit
        if i = 3
        then CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasId, FAtPasSym, FAtPasCom]) // Foo;
        else CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasId, FAtPasSym]); // Foo;
        if i = 4
        then CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasKey, FAtPasCom])         // uses
        else CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasKey]);         // uses
        if i = 5
        then CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasId, FAtPasSym, FAtPasCom]) // Bar;
        else CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasId, FAtPasSym]); // Bar;
        AssertEquals(BaseTestName + 'Section Count def', 0, FMultiHl.DefaultVirtualLines.SectionList.Count);
        AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);
        if i = 6
        then CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasCom, FAtPasMark])
        else CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasMark]);

      PushBaseName('undo //');
        SynEdit.Undo;
//DumpAll(FMultiHl);
        CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark]);
        CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasKey]);          // unit
        CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasId, FAtPasSym]); // Foo;
        CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasKey]);         // uses
        CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasId, FAtPasSym]); // Bar;
        CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasMark]);
        AssertEquals(BaseTestName + 'Section Count def', 0, FMultiHl.DefaultVirtualLines.SectionList.Count);
        AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

      PushBaseName('"(*" line '+IntToStr(i));
        ResetHighlightChanged;
        if i = 1
        then SynEdit.TestTypeText(6, i, '(*')
        else if i = 6
        then SynEdit.TestTypeText(1, i, '(*')
        else SynEdit.TestTypeText(5, i, '(*');
//DumpAll(FMultiHl);
        if i = 1
        then CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark, FAtPasCom])
        else CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark]);
        if i = 2
        then CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasKey, FAtPasCom])          // unit
        else if i < 2
        then CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasCom])
        else CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasKey]);          // unit
        if i = 3
        then CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasId, FAtPasSym, FAtPasCom]) // Foo;
        else if i < 3
        then CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasCom]) // Foo;
        else CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasId, FAtPasSym]); // Foo;
        if i = 4
        then CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasKey, FAtPasCom])         // uses
        else if i < 4
        then CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasCom])         // uses
        else CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasKey]);
        if i = 5
        then CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasId, FAtPasSym, FAtPasCom]) // Bar;
        else if i < 5
        then CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasCom]) // Bar;
        else CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasId, FAtPasSym]); // Bar;
        if i = 6
        then CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasCom, FAtPasMark])
        else CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasMark]);
        AssertEquals(BaseTestName + 'Section Count def', 0, FMultiHl.DefaultVirtualLines.SectionList.Count);
        AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

        // either invalidate from line-index i-1, or 1 before (max 1 before)
//DebugLn(['FHLCLow=', FHLCLow, '  FHLCHigh=',FHLCHigh]);
        AssertTrue('Top inval', (FHLCLow = Max(-1,i-2)) or (FHLCLow = i-1));  // HL currently sends -1
        //AssertTrue('Top inval', FHLCLow in [Max(0,i-2), i-1]);
        AssertTrue('bottom inval', FHLCHigh = 5);

      PushBaseName('undo (*');
        SynEdit.Undo;
//DumpAll(FMultiHl);
        CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtPasMark]);
        CheckTokensForLine('2nd line=html', FMultiHl, 1, [FAtPasKey]);          // unit
        CheckTokensForLine('3rd line=html', FMultiHl, 2, [FAtPasId, FAtPasSym]); // Foo;
        CheckTokensForLine('4th line=html', FMultiHl, 3, [FAtPasKey]);         // uses
        CheckTokensForLine('5th line=html', FMultiHl, 4, [FAtPasId, FAtPasSym]); // Bar;
        CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasMark]);
        AssertEquals(BaseTestName + 'Section Count def', 0, FMultiHl.DefaultVirtualLines.SectionList.Count);
        AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);



      FinishMultiXmlPasHl;
      PopBaseName;
      PopBaseName;
    end; // for j
  {%endregion}

  {%region edit text in single pascal region after default}
    // ensure virtuallines by nested HL are mapped for invalidation
    PushBaseName('edit text in pascal region after default (scan end before eot) j='+ IntToStr(j));   // Issue 0022519
    SynEdit.ClearAll;
    InitMultiXmlPasHl;

    PushBaseName('Insert "<pas>"');
      SynEdit.TestTypeText(1, 1, 'x'+#13+'x'+#13+'<pas>'+#13+'</pas>');

    PushBaseName('Insert "unit..."');
      SynEdit.TestTypeText(1, 4, 'unit'+#13+'Foo;'+#13+'uses'+#13+'Bar;'+#13+'var'+#13+'xx');
//DumpAll(FMultiHl);
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlTxt]);
      CheckTokensForLine('2st line=html', FMultiHl, 1, [FAtXmlTxt]);
      CheckTokensForLine('3st line=html', FMultiHl, 2, [FAtPasMark]);
      CheckTokensForLine('4nd line=html', FMultiHl, 3, [FAtPasKey]);          // unit
      CheckTokensForLine('5rd line=html', FMultiHl, 4, [FAtPasId, FAtPasSym]); // Foo;
      CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasKey]);         // uses
      CheckTokensForLine('7th line=html', FMultiHl, 6, [FAtPasId, FAtPasSym]); // Bar;
      CheckTokensForLine('8th line=html', FMultiHl, 7, [FAtPasKey]);
      CheckTokensForLine('9th line=html', FMultiHl, 8, [FAtPasId, FAtPasMark]);
      AssertEquals(BaseTestName + 'Section Count def', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);


    PushBaseName('"(*" line '+IntToStr(i));
      ResetHighlightChanged;
      SynEdit.TestTypeText(5, 5, '(*'); // after FOO
//DumpAll(FMultiHl);
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlTxt]);
      CheckTokensForLine('2st line=html', FMultiHl, 1, [FAtXmlTxt]);
      CheckTokensForLine('3st line=html', FMultiHl, 2, [FAtPasMark]);
      CheckTokensForLine('4nd line=html', FMultiHl, 3, [FAtPasKey]);          // unit
      CheckTokensForLine('5rd line=html', FMultiHl, 4, [FAtPasId, FAtPasSym, FAtPasCom]); // Foo;
      CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasCom]);         // uses
      CheckTokensForLine('7th line=html', FMultiHl, 6, [FAtPasCom]); // Bar;
      CheckTokensForLine('8th line=html', FMultiHl, 7, [FAtPasCom]);
      CheckTokensForLine('9th line=html', FMultiHl, 8, [FAtPasCom, FAtPasMark]);
      AssertEquals(BaseTestName + 'Section Count def', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);
//DebugLn(['FHLCLow=', FHLCLow, '  FHLCHigh=',FHLCHigh]);
        AssertTrue('Top inval', (FHLCLow = 3) or (FHLCLow = 4));  // HL currently sends -1
        //AssertTrue('Top inval', FHLCLow in [Max(0,i-2), i-1]);
        AssertTrue('bottom inval', FHLCHigh = 8);


    PushBaseName('undo (*');
      SynEdit.Undo;
//DumpAll(FMultiHl);
      CheckTokensForLine('1st line=html', FMultiHl, 0, [FAtXmlTxt]);
      CheckTokensForLine('2st line=html', FMultiHl, 1, [FAtXmlTxt]);
      CheckTokensForLine('3st line=html', FMultiHl, 2, [FAtPasMark]);
      CheckTokensForLine('4nd line=html', FMultiHl, 3, [FAtPasKey]);          // unit
      CheckTokensForLine('5rd line=html', FMultiHl, 4, [FAtPasId, FAtPasSym]); // Foo;
      CheckTokensForLine('6th line=html', FMultiHl, 5, [FAtPasKey]);         // uses
      CheckTokensForLine('7th line=html', FMultiHl, 6, [FAtPasId, FAtPasSym]); // Bar;
      CheckTokensForLine('8th line=html', FMultiHl, 7, [FAtPasKey]);
      CheckTokensForLine('9th line=html', FMultiHl, 8, [FAtPasId, FAtPasMark]);
      AssertEquals(BaseTestName + 'Section Count def', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

    FinishMultiXmlPasHl;
    PopBaseName;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('append after EOT - after sub-scheme');
    SynEdit.ClearAll;
    SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1), 'a<pas>unit</pas>');
    InitMultiXmlPasHl;
    FMultiHl.CurrentLines := SynEdit.TextBuffer;
    SynEdit.SimulatePaintText;
      CheckTokensForLine('1st line', FMultiHl, 0, [FAtXmlTxt, FAtPasMark, FAtPasKey, FAtPasMark]);
      AssertEquals(BaseTestName + 'Section Count def', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

    SynEdit.CaretXY := point(17,1);
    SynEdit.CommandProcessor(ecLineBreak, '', nil);
    SynEdit.CommandProcessor(ecChar, '<', nil);
    SynEdit.CommandProcessor(ecChar, 'a', nil);
    SynEdit.CommandProcessor(ecChar, '>', nil);
//DumpAll(FMultiHl);
      CheckTokensForLine('1st line', FMultiHl, 0, [FAtXmlTxt, FAtPasMark, FAtPasKey, FAtPasMark]);
      CheckTokensForLine('2nd line', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count def', 2, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('append after EOT - after sub-scheme');
    SynEdit.ClearAll;
    SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1), 'a<pas>unit</pas>');
    InitMultiXmlPasHl;
    FMultiHl.CurrentLines := SynEdit.TextBuffer;
    SynEdit.SimulatePaintText;
      CheckTokensForLine('1st line', FMultiHl, 0, [FAtXmlTxt, FAtPasMark, FAtPasKey, FAtPasMark]);
      AssertEquals(BaseTestName + 'Section Count def', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

    SynEdit.CaretXY := point(17,1);
    SynEdit.CommandProcessor(ecLineBreak, '', nil);
    SynEdit.CommandProcessor(ecChar, '<', nil);
    SynEdit.CommandProcessor(ecChar, 'a', nil);
    SynEdit.CommandProcessor(ecChar, '>', nil);
//DumpAll(FMultiHl);
      CheckTokensForLine('1st line', FMultiHl, 0, [FAtXmlTxt, FAtPasMark, FAtPasKey, FAtPasMark]);
      CheckTokensForLine('2nd line', FMultiHl, 1, [FAtXmlSym, FAtXmlEl, FAtXmlSym]);
      AssertEquals(BaseTestName + 'Section Count def', 2, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('append after EOT - in sub-scheme');
    SynEdit.ClearAll;
    SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1), 'a<pas>unit');
    InitMultiXmlPasHl;
    FMultiHl.CurrentLines := SynEdit.TextBuffer;
    SynEdit.SimulatePaintText;
      CheckTokensForLine('1st line', FMultiHl, 0, [FAtXmlTxt, FAtPasMark, FAtPasKey]);
      AssertEquals(BaseTestName + 'Section Count def', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

      SynEdit.CaretXY := point(17,1);
      SynEdit.CommandProcessor(ecLineBreak, '', nil);
//DumpAll(FMultiHl);
      SynEdit.CommandProcessor(ecChar, ';', nil);
      CheckTokensForLine('1st line', FMultiHl, 0, [FAtXmlTxt, FAtPasMark, FAtPasKey]);
      CheckTokensForLine('2nd line', FMultiHl, 1, [FAtPasSym]);
      AssertEquals(BaseTestName + 'Section Count def', 1, FMultiHl.DefaultVirtualLines.SectionList.Count);
      AssertEquals(BaseTestName + 'Section Count pas', 1, FPasScheme.VirtualLines.SectionList.Count);

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region multiple section (same scheme) on one line}
    PushBaseName('multiple section (same scheme) on one line');
    SynEdit.ClearAll;
    InitMultiXmlPasHl;

    SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,1),
      'abc'+LineEnding+
      'test<pas>unit a;</pas><x><pas>uses </pas></x><pas> Foo;</pas>a'+LineEnding+
      'abc'+LineEnding
    );
    SynEdit.SetTextBetweenPoints(Point(1,1), Point(1,3), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at begin of section (no overlap / 1 line)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,3), Point(1,4), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at begin of section (no overlap / 2 line)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,3), Point(1,5), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at begin of section (overlap / 1 line of sect)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,2), Point(1,4), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at begin of section (overlap / 2 line of sect)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,2), Point(1,5), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at middle of section (1 line)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,4), Point(1,5), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at end of section (no overlap 1 line)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,7), Point(1,8), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at end of section (no overlap 2 line)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,6), Point(1,8), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

  {%region}
    PushBaseName('longer section, delete part at end of section (overlap 1 line of sect)');
    SetRealLinesText3;
    InitMultiXmlPasHl;
    SynEdit.SimulatePaintText;

    SynEdit.SetTextBetweenPoints(Point(1,7), Point(1,9), '');

    FinishMultiXmlPasHl;
    PopBaseName;
  {%endregion}

end;

procedure TTestHighlightMulti.TestMultiHLEdit2;
var
  i1, i2, del1, del2, redo, rd: Integer;
  DoSim1, DoSim2: Boolean;
  txt: TStringArray;
begin
  for i1 := -1 to 2 do
  for i2 := -1 to 2 do
  for DoSim1 := False to True do
  for DoSim2 := False to True do
  for del1 := 1        to 8 + i1+1 + i2+1 do
  for del2 := del1 + 1 to 9 + i1+1 + i2+1 do
  for redo := 0 to 2 do
  begin
    txt := GetRealLinesText4(i1, i2);
    if (del1 >= Length(txt)) or (del2 > Length(txt)) then
      continue;

    PushBaseName(format('i1=%d, i2=%d / D1=%d, D2=%d / Sim:%s,%s ', [i1, i2, Del1, Del2, dbgs(DoSim1), dbgs(DoSim2)]));
    //debugln(['############ ', i1, ' ', i2, ' d ', del1,' ',del2, ' ',DoSim1,DoSim2]);

    ReCreateEdit;
    SynEdit.ViewedTextBuffer.AddChangeHandler(senrHighlightChanged, @DoHighlightChanged);
    InitMultiXmlPasHl;

    if not DoSim1 then
      SynEdit.BeginUpdate;
    SynEdit.Text := LinesToText(txt);
    if DoSim1 then
      SynEdit.SimulatePaintText;


    if (not DoSim2) then
      SynEdit.BeginUpdate;
    SynEdit.SetTextBetweenPoints(Point(1,del1), Point(1,del2), '');
    if (not DoSim1) then
      SynEdit.EndUpdate;
    if DoSim2 then
      SynEdit.SimulatePaintText;

    for rd := 0 to redo - 1 do begin
      SynEdit.Undo;
      if DoSim2 then
        SynEdit.SimulatePaintText;
      SynEdit.Redo;
      if DoSim2 then
        SynEdit.SimulatePaintText;
    end;


    SynEdit.ClearAll;
    if (not DoSim2) then
      SynEdit.EndUpdate;
    SynEdit.SimulatePaintText;

    SynEdit.Undo;
    SynEdit.SimulatePaintText;
    SynEdit.Redo;
    SynEdit.SimulatePaintText;
    SynEdit.ClearAll;
    SynEdit.SimulatePaintText;

    FinishMultiXmlPasHl;
    PopBaseName;
  end;

end;

initialization

  RegisterTest(TTestHighlightMulti);
  DebugLogger.FindOrRegisterLogGroup('SYNDEBUG_MULTIHL', True)^.Enabled := True;

end.


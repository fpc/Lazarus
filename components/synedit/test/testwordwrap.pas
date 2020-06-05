unit TestWordWrap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, TestBase, SynEditViewedLineMap, SynEditMiscClasses,
  SynEditTypes, SynEditWrappedView,
  LazSynEditText, SynEditHighlighterFoldBase, LazLoggerBase,
  SynEditKeyCmds, SynEdit, SynEditPointClasses, testregistry;

type
  TIntArray = Array of integer;

  { TExpWraps }

  TExpWraps = object
    w: Array of Integer;
    len: Integer;
    function Init(const a: array of integer): TExpWraps;
    procedure SetCapacity(l: Integer);
    procedure Fill(AFrom, ATo: integer; AIncrease: Integer = 1);
    procedure Join(const a: TExpWraps; AInsertPos: Integer = -1);
    procedure Join(const a: array of integer; AInsertPos: Integer = -1);
    procedure SpliceArray(ADelFrom, ADelCount: integer);
  end;

  { TTestWordWrap }

  TTestWordWrap = class(TTestBase)
  private
    FTree: TSynLineMapAVLTree;
    procedure AssertRealToWrapOffsets(const AName: String; ALine: TSynWordWrapLineMap;
      const ExpWrapOffsets: TExpWraps; AStartOffs: Integer = 0);
    procedure AssertWrapToRealOffset(const AName: String; ALine: TSynWordWrapLineMap;
      const ExpRealAndSubOffsets: TExpWraps; AStartOffs: Integer = 0);
    procedure AssertLineForWraps(const AName: String; ALine: TSynWordWrapLineMap;
      const ExpWrapForEachLine: TExpWraps; AnExpAllValid: Boolean = False);
    procedure InitLine(ALine: TSynWordWrapLineMap;
      const AWrapValues: TExpWraps);
    function OnPageNeeded(AMapTree: TSynLineMapAVLTree): TSynEditLineMapPage;
    procedure ValidateWraps(ALine: TSynWordWrapLineMap;
      const AWrapValues: TExpWraps; AStartOffs: Integer = 0; ABackward: Boolean = False);
    procedure ValidateNeededWraps(ALine: TSynWordWrapLineMap; const AWrapValues: TExpWraps);

    procedure ValidateTreeWraps(const AWrapValues: TExpWraps; AStartOffs: Integer = 0);
    procedure AssertTreeForWraps(const AName: String; const ExpWrapForEachLine: TExpWraps; AStartOffs: Integer = 0);

    function CreateTree(APageJoinSize, APageSplitSize, APageJoinDistance: Integer): TSynLineMapAVLTree;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWordWrapLineMap;
    procedure TestWordWrapLineMapInvalidate;
    procedure TestWordWrapLineMapInvalidateNoneContineous;
    procedure TestWordWrapLineMapValidate;
    procedure TestWordWrapLineMapMerge;
    procedure TestWordWrapLineMapMergeInvalidate;

    procedure TestWordWrapTreeInsertThenDelete;
    procedure TestWordWrapTreeDeleteThenInsert;
  end;

  TPointType = (ptViewed, ptAlternateViewed, ptPhys, ptLog);

  TPointSpecs = record
    XY: array [TPointType] of TPoint;
    LogOffs: Integer;
  end;

  TCommandAndPointSpecs = record
    Exp: TPointSpecs;
    Cmd: Array of TSynEditorCommand;
    RunOnlyIf: Boolean;
  end;

  { TTestWordWrapPluginBase }

  TTestWordWrapPluginBase = class(TTestBase)
  private
    procedure ClearCaret;
    procedure SetCaret(SourcePt: TPointType; APos: TPoint);
    procedure TestCaret(AName: String; SourcePt, ExpPt: TPointType; AnExp: TPoint;
      AnExpOffs: Integer = -1);
  protected
    FWordWrap: TLazSynEditLineWrapPlugin;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: TPoint); overload;
    procedure AddLines(AFirstLineIdx, ACount, ALen: Integer; AnID: String; SkipBeginUpdate: Boolean = False);
    procedure InternalCheckLine(AName: String; dsp: TLazSynDisplayView; ALine: TLineIdx; AExpTextStart: String; NoTrim: Boolean = False);
    procedure CheckLine(AName: String; ALine: TLineIdx; AExpTextStart: String; NoTrim: Boolean = False);
    procedure CheckLines(AName: String; AStartLine: TLineIdx; AExpTextStart: array of String; NoTrim: Boolean = False);
    procedure CheckXyMap(AName: String; APhysTExtXY, AViewedXY: TPoint; OnlyViewToText: Boolean = False);
    procedure CheckXyMap(AName: String; APhysTExtX, APhysTExtY, AViewedX, AViewedY: integer; OnlyViewToText: Boolean = False);

    procedure CheckXyMap(AName: String; APoints: TPointSpecs);
    procedure CheckXyMap(AName: String; APoints: TPointSpecs;
      ATestCommands: array of TCommandAndPointSpecs);

    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestWordWrapPlugin = class(TTestWordWrapPluginBase)
  published
    procedure TestEditorWrap;
    procedure TestEditorEdit;
  end;

implementation

function p(VX, VY,  AVX, AVY,  PX, PY,  LX, LY: Integer; Offs: Integer = -1): TPointSpecs; overload;
begin
  with Result do begin
    XY[ptViewed].X          := VX;
    XY[ptViewed].Y          := VY;
    XY[ptAlternateViewed].X := AVX;
    XY[ptAlternateViewed].Y := AVY;
    XY[ptPhys].X            := PX;
    XY[ptPhys].Y            := PY;
    XY[ptLog].X             := LX;
    XY[ptLog].Y             := LY;
    LogOffs                 := Offs;
  end;
end;

function p(VX, VY,  PX, PY,  LX, LY: Integer; Offs: Integer = -1): TPointSpecs; overload;
begin
  Result := p(VX, VY, -1, -1, PX, PY, LX, LY, Offs);
end;

function c(Cmd: Array of TSynEditorCommand; VX, VY,  AVX, AVY,  PX, PY,  LX, LY: Integer; Offs: Integer = -1; RunOnlyIf: Boolean = True): TCommandAndPointSpecs; overload;
begin
  Result.Exp := p(VX, VY, AVX, AVY, PX, PY, LX, LY, Offs);
  SetLength(Result.Cmd, Length(Cmd));
  move(Cmd[0], Result.Cmd[0], SizeOf(cmd[0]) * Length(Cmd));
  Result.RunOnlyIf := RunOnlyIf;
end;

function c(Cmd: Array of TSynEditorCommand; VX, VY,  PX, PY,  LX, LY: Integer; Offs: Integer = -1; RunOnlyIf: Boolean = True): TCommandAndPointSpecs; overload;
begin
  Result := c(Cmd, VX, VY, -1, -1, PX, PY, LX, LY, Offs, RunOnlyIf);
end;

function c(Cmd: TSynEditorCommand; VX, VY,  PX, PY,  LX, LY: Integer; Offs: Integer = -1; RunOnlyIf: Boolean = True): TCommandAndPointSpecs; overload;
begin
  Result := c([Cmd], VX, VY, -1, -1, PX, PY, LX, LY, Offs, RunOnlyIf);
end;

function FillArray(AFrom, ATo: integer; AIncrease: Integer = 1): TIntArray;
var
  i: Integer;
begin
  SetLength(Result, ATo - AFrom + 1);
  for i := 0 to high(Result) do
    Result[i] := AFrom + i * AIncrease;
end;

{ TExpWraps }

function TExpWraps.Init(const a: array of integer): TExpWraps;
begin
  len := Length(a);
  if len > 0 then begin
    SetCapacity(len);
    move(a[0], w[0], SizeOf(w[0]) * len);
  end;
  Result := self;
end;

procedure TExpWraps.SetCapacity(l: Integer);
begin
  if Length(w) < l then
    SetLength(w, l*2);
end;

procedure TExpWraps.Fill(AFrom, ATo: integer; AIncrease: Integer);
var
  p: PLongInt;
  i: Integer;
begin
  len := ATo - AFrom + 1;
  SetCapacity(len);
  p := @w[0];
  for i := 0 to len - 1 do begin
    p^ := AFrom;
    inc(p);
    inc(AFrom, AIncrease);
  end;
end;

procedure TExpWraps.Join(const a: TExpWraps; AInsertPos: Integer);
var
  i, old: Integer;
begin
  if AInsertPos < 0 then
    AInsertPos := Len;

  i := (Len-AInsertPos);
  old := len;
  len := len + a.len;
  if i < 0 then
    len := len - i;
  SetCapacity(len);

  if i > 0 then begin
    move(w[AInsertPos], w[AInsertPos+a.len], sizeof(w[0]) * i);
  end
  else
  if i < 0 then begin
    FillDWord(w[old], -i, 1);
  end;
  move(a.w[0], w[AInsertPos], sizeof(w[0]) * a.len);
end;

procedure TExpWraps.Join(const a: array of integer; AInsertPos: Integer);
var
  i, la, old: Integer;
begin
  if AInsertPos < 0 then
    AInsertPos := Len;

  i := (Len-AInsertPos);
  la := Length(a);
  old := len;
  len := len + la;
  if i < 0 then
    len := len - i;
  SetCapacity(len);

  if i > 0 then begin
    move(w[AInsertPos], w[AInsertPos+la], sizeof(w[0]) * i);
  end
  else
  if i < 0 then begin
    FillDWord(w[old], -i, 1);
  end;
  move(a[0], w[AInsertPos], sizeof(w[0]) * la);
end;

procedure TExpWraps.SpliceArray(ADelFrom, ADelCount: integer);
var
  i: Integer;
begin
  len := len - ADelCount;

  i := Length(w) - ADelFrom - ADelCount;
  if i > 0 then
    move(w[ADelFrom+ADelCount], w[ADelFrom], sizeof(w[0]) * (i));
end;

{ TTestWordWrap }

procedure TTestWordWrap.AssertRealToWrapOffsets(const AName: String;
  ALine: TSynWordWrapLineMap; const ExpWrapOffsets: TExpWraps;
  AStartOffs: Integer);
var
  i: Integer;
begin
  for i := 0 to ExpWrapOffsets.len - 1 do
    AssertEquals(format('%s: RealToWrap Idx %d StartOffs: %d ', [AName, i, AStartOffs]),
      ExpWrapOffsets.w[i], ALine.WrappedOffsetFor[AStartOffs + i]);
end;

procedure TTestWordWrap.AssertWrapToRealOffset(const AName: String;
  ALine: TSynWordWrapLineMap; const ExpRealAndSubOffsets: TExpWraps;
  AStartOffs: Integer);
var
  i, sub, r: Integer;
begin
  for i := 0 to ExpRealAndSubOffsets.len div 2 - 1 do begin
    r := ALine.GetOffsetForWrap(AStartOffs + i, sub);
    AssertEquals(format('%s: WrapToReal Idx %d StartOffs: %d ', [AName, i, AStartOffs]),
      ExpRealAndSubOffsets.w[i*2], r);
    AssertEquals(format('%s: WrapToReal(SUB) Idx %d StartOffs: %d ', [AName, i, AStartOffs]),
      ExpRealAndSubOffsets.w[i*2+1], sub);
  end;
end;

procedure TTestWordWrap.AssertLineForWraps(const AName: String;
  ALine: TSynWordWrapLineMap; const ExpWrapForEachLine: TExpWraps;
  AnExpAllValid: Boolean);
var
  i, j, ExpWrap, TestWrapToReal, GotReal, sub: Integer;
begin
  if AnExpAllValid then
    AssertTrue(AName + ' - all lines valid', ALine.FirstInvalidLine < 0);
  i := 0;
  while (i < ExpWrapForEachLine.len) and (ExpWrapForEachLine.w[i] = 1) do
    inc(i);
  if i = ExpWrapForEachLine.len then
    i := 0;
  AssertEquals(Format('%s: Offset', [AName]), i, ALine.Offset);

  j := ExpWrapForEachLine.len - 1;
  while (j >= 0) and (ExpWrapForEachLine.w[j] = 1) do
    dec(j);
  AssertEquals(Format('%s: RealCount', [AName]), j + 1 - i, ALine.RealCount);

  ExpWrap := 0;
  TestWrapToReal := 0;
  for i := 0 to ExpWrapForEachLine.len - 1 do begin
    AssertEquals(Format('%s: RealToWrap Idx %d', [AName, i]), ExpWrap, ALine.WrappedOffsetFor[i]);
    ExpWrap := ExpWrap + ExpWrapForEachLine.w[i];

    for j := 0 to ExpWrapForEachLine.w[i] - 1 do begin
      GotReal := ALine.GetOffsetForWrap(TestWrapToReal, sub);
      AssertEquals(Format('%s: WrapToReal Idx %d', [AName, TestWrapToReal]), i, GotReal);
      AssertEquals(Format('%s: WrapToReal Idx %d SUB', [AName, TestWrapToReal]), j, sub);
      inc(TestWrapToReal);
    end;
  end;
end;

procedure TTestWordWrap.InitLine(ALine: TSynWordWrapLineMap;
  const AWrapValues: TExpWraps);
begin
  ALine.DeleteLinesAtOffset(0, ALine.RealCount + ALine.Offset);
  if AWrapValues.len > 0 then begin
    ALine.InsertLinesAtOffset(0, AWrapValues.len);
    ValidateWraps(ALine, AWrapValues);
  end;
  AssertEquals('all valid', -1, ALine.FirstInvalidLine);
end;

function TTestWordWrap.OnPageNeeded(AMapTree: TSynLineMapAVLTree
  ): TSynEditLineMapPage;
begin
  Result := TSynWordWrapIndexPage.Create(AMapTree);
  //TSynWordWrapIndexPage(Result).FSynEditWrappedPlugin := Self;
end;

procedure TTestWordWrap.ValidateWraps(ALine: TSynWordWrapLineMap;
  const AWrapValues: TExpWraps; AStartOffs: Integer; ABackward: Boolean);
var
  i: Integer;
begin
  if ABackward then begin
    for i := AWrapValues.len - 1 downto 0 do
      ALine.ValidateLine(AStartOffs + i, AWrapValues.w[i]);
  end
  else begin
    for i := 0 to AWrapValues.len - 1 do
      ALine.ValidateLine(AStartOffs + i, AWrapValues.w[i]);
  end;
  ALine.EndValidate;
end;

procedure TTestWordWrap.ValidateNeededWraps(ALine: TSynWordWrapLineMap;
  const AWrapValues: TExpWraps);
var
  i: Integer;
begin
  i := ALine.FirstInvalidLine;
  while i >= 0 do begin
    ALine.ValidateLine(i, AWrapValues.w[i]);
    i := ALine.FirstInvalidLine;
  end;
  ALine.EndValidate;
end;

procedure TTestWordWrap.ValidateTreeWraps(const AWrapValues: TExpWraps;
  AStartOffs: Integer);
var
  i: Integer;
  LowLine, HighLine: TLineIdx;
begin
  while FTree.NextBlockForValidation(LowLine, HighLine) do begin
    for i := LowLine to HighLine do begin
      AssertTrue(i-AStartOffs < AWrapValues.len);
      FTree.ValidateLine(i, AWrapValues.w[i-AStartOffs]);
    end;
  end;
  FTree.EndValidate;
end;

procedure TTestWordWrap.AssertTreeForWraps(const AName: String;
  const ExpWrapForEachLine: TExpWraps; AStartOffs: Integer);
var
  i, w: Integer;
  sub: TLineIdx;
begin
  w := AStartOffs;
  for i := 0 to (ExpWrapForEachLine.len - 1) do begin
    AssertEquals(Format('%s // l=%d getWrap', [AName, i]),
      w,
      FTree.GetWrapLineForForText(AStartOffs + i)
    );
    w := w + ExpWrapForEachLine.w[i];
    AssertEquals(Format('%s // l=%d getLine', [AName, i]),
      i,
      FTree.GetLineForForWrap(w-1, sub)
    );
    AssertEquals(Format('%s // l=%d sub', [AName, i]),
      ExpWrapForEachLine.w[i]-1,
      sub
    );
  end;
end;

function TTestWordWrap.CreateTree(APageJoinSize, APageSplitSize,
  APageJoinDistance: Integer): TSynLineMapAVLTree;
begin
  Result := TSynLineMapAVLTree.Create(APageJoinSize, APageSplitSize, APageJoinDistance);
  Result.PageCreatorProc := @OnPageNeeded;
end;

procedure TTestWordWrap.SetUp;
begin
  FTree := CreateTree(15, 60, 20);
  inherited SetUp;
end;

procedure TTestWordWrap.TearDown;
begin
  inherited TearDown;
  FTree.Free;
end;

procedure TTestWordWrap.TestWordWrapLineMap;
var
  ALine: TSynWordWrapLineMap;
  ANode: TSynEditLineMapPage;
  i: Integer;
  ATestName: String;
  w: TExpWraps;
begin
  ANode := FTree.FindPageForLine(0, afmCreate).Page;
  ALine := TSynWordWrapIndexPage(ANode).SynWordWrapLineMapStore;
  ALine.InsertLinesAtOffset(0, 5);
  ALine.InvalidateLines(2,3);
  ValidateWraps(ALine, w.init([1, 1, 3, 3, 1]));
  AssertLineForWraps('', ALine, w.init([1, 1, 3, 3, 1,   1,1]));
  //AssertRealToWrapOffsets('', ALine, [0, 1, 2, 5, 8, 9, 10]);
  //AssertWrapToRealOffset('', ALine, [0,0,  1,0,  2,0, 2,1, 2,2,  3,0, 3,1, 3,2,  4,0,  5,0]);
  AssertEquals('all valid', -1, ALine.FirstInvalidLine);

  for i := 1 to 2 do begin

    // insert into offset
    ATestName := 'Insert at start of "Offset"';
    ALine.InsertLinesAtOffset(0, 2);
    ValidateWraps(ALine, w.init([2, 2]), 0, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([2, 2,   1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(0, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Insert at middle of "Offset"';
    ALine.InsertLinesAtOffset(1, 2);
    ValidateWraps(ALine, w.init([2, 2]), 1, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1,   2, 2,   1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(1, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Insert at end of "Offset"';
    ALine.InsertLinesAtOffset(2, 2);
    ValidateWraps(ALine, w.init([2, 2]), 2, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1,   2, 2,   3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(2, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);



    ATestName := 'Insert at start of "Offset" - single lines';
    ALine.InsertLinesAtOffset(0, 2);
    ValidateWraps(ALine, w.init([1, 1]), 0, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1,   1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(0, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Insert at middle of "Offset" - single lines';
    ALine.InsertLinesAtOffset(1, 2);
    ValidateWraps(ALine, w.init([1, 1]), 1, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1,   1, 1,   1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(1, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Insert at end of "Offset" - single lines';
    ALine.InsertLinesAtOffset(2, 2);
    ValidateWraps(ALine, w.init([1, 1]), 2, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1,   1, 1,   3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(2, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);



    ATestName := 'Insert at start of "Offset" - single/wrap lines';
    ALine.InsertLinesAtOffset(0, 2);
    ValidateWraps(ALine, w.init([1, 2]), 0, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 2,   1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(1, 1);
    AssertLineForWraps(ATestName, ALine, w.init([1,   1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);
    ALine.DeleteLinesAtOffset(0, 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Delete mixed offset/data';
    ALine.DeleteLinesAtOffset(1, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1,    3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.InsertLinesAtOffset(1, 2);
    ValidateWraps(ALine, w.init([1, 3]), 1, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    // insert into data
    ATestName := 'Insert at middle of Data';
    ALine.InsertLinesAtOffset(3, 2);
    ValidateWraps(ALine, w.init([2, 2]), 3, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3,   2, 2,   3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(3, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);



    ATestName := 'Insert at middle of Data';
    ALine.InsertLinesAtOffset(3, 2);
    ValidateWraps(ALine, w.init([2, 2]), 3, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3,   2, 2,   3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(3, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);



    // insert after data
    ATestName := 'Insert at end of Data';
    ALine.InsertLinesAtOffset(5, 1);
    ValidateWraps(ALine, w.init([4]), 5, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   4,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(5, 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Insert at end of Data - single line';
    ALine.InsertLinesAtOffset(5, 1);
    ValidateWraps(ALine, w.init([1]), 5, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(5, 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Insert behind end of Data';
    ALine.InsertLinesAtOffset(6, 1);
    ValidateWraps(ALine, w.init([4]), 6, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1, 4,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(6, 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Insert behind end of Data - single line';
    ALine.InsertLinesAtOffset(6, 1);
    ValidateWraps(ALine, w.init([1]), 6, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.DeleteLinesAtOffset(6, 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);


    ATestName := 'Delete mixed data/after';
    ALine.DeleteLinesAtOffset(3, 2);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3,    1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

    ALine.InsertLinesAtOffset(3, 2);
    ValidateWraps(ALine, w.init([3, 1]), 3, i mod 1 = 1);
    AssertLineForWraps(ATestName, ALine, w.init([1, 1, 3, 3, 1,   1,1]));
    AssertEquals('all valid', -1, ALine.FirstInvalidLine);

  end;

  ALine.InvalidateLines(0, 4);
  ValidateWraps(ALine, w.init([1,1,1,1,1]), 0, False);
  AssertLineForWraps('', ALine, w.init([1, 1, 1, 1, 1,   1,1]));
  AssertEquals('all valid', -1, ALine.FirstInvalidLine);

  ALine.InsertLinesAtOffset(0, 5);
  ValidateWraps(ALine, w.init([1, 1, 3, 3, 1]));
  AssertLineForWraps('', ALine, w.init([1, 1, 3, 3, 1,   1,1]));
  AssertEquals('all valid', -1, ALine.FirstInvalidLine);

  ALine.InvalidateLines(0, 4);
  ValidateWraps(ALine, w.init([1,1,1,1,1]), 0, True);
  AssertLineForWraps('', ALine, w.init([1, 1, 1, 1, 1,   1,1]));
  AssertEquals('all valid', -1, ALine.FirstInvalidLine);

end;

procedure TTestWordWrap.TestWordWrapLineMapInvalidate;
var
  ANode1: TSynWordWrapIndexPage;
  ALine1: TSynWordWrapLineMap;
  //ATestName: String;
  w: TExpWraps;
begin
  // invalidate and insert/remove lines
  ANode1 := TSynWordWrapIndexPage(FTree.FindPageForLine(0, afmCreate).Page);
  ALine1 := ANode1.SynWordWrapLineMapStore;

  InitLine(ALine1, w.init([1]));
  ALine1.InvalidateLines(3,6);
  AssertEquals('invalid', 3, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 6, ALine1.LastInvalidLine);

  ALine1.DeleteLinesAtOffset(6,2);
  AssertEquals('invalid', 3, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 5, ALine1.LastInvalidLine);

  ALine1.InsertLinesAtOffset(2,1);
  AssertEquals('invalid', 2, ALine1.FirstInvalidLine);
  ValidateWraps(ALine1, w.init([1]), 2);

  AssertEquals('invalid', 4, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 6, ALine1.LastInvalidLine);

  ALine1.InsertLinesAtOffset(5,1);
  AssertEquals('invalid', 4, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 7, ALine1.LastInvalidLine);

  ALine1.DeleteLinesAtOffset(4,1);
  AssertEquals('invalid', 4, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 6, ALine1.LastInvalidLine);

end;

procedure TTestWordWrap.TestWordWrapLineMapInvalidateNoneContineous;
var
  ANode1: TSynWordWrapIndexPage;
  ALine1: TSynWordWrapLineMap;
  //ATestName: String;
  w: TExpWraps;
begin
  // invalidate and insert/remove lines
  ANode1 := TSynWordWrapIndexPage(FTree.FindPageForLine(0, afmCreate).Page);
  ALine1 := ANode1.SynWordWrapLineMapStore;

  InitLine(ALine1, w.init([1]));
  ALine1.InvalidateLines(30,31);
  ALine1.InvalidateLines(32,35);
  ALine1.InvalidateLines(40,41);
  ALine1.InvalidateLines(10,11);
  ALine1.InvalidateLines(20,21);
  ALine1.InvalidateLines(22,23);

  AssertEquals('invalid first from', 10, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   11, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       41, ALine1.LastInvalidLine);

  ALine1.ValidateLine(10, 1);
  AssertEquals('invalid first from', 11, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   11, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       41, ALine1.LastInvalidLine);

  ALine1.ValidateLine(11, 1);
  AssertEquals('invalid first from', 20, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   23, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       41, ALine1.LastInvalidLine);

  ALine1.ValidateLine(20, 1);
  AssertEquals('invalid first from', 21, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   23, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       41, ALine1.LastInvalidLine);

  ALine1.ValidateLine(21, 1);
  ALine1.ValidateLine(22, 1);
  ALine1.ValidateLine(23, 1);
  AssertEquals('invalid first from', 30, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   35, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       41, ALine1.LastInvalidLine);

  ALine1.ValidateLine(30, 1);
  ALine1.ValidateLine(31, 1);
  ALine1.ValidateLine(32, 1);
  ALine1.ValidateLine(33, 1);
  ALine1.ValidateLine(34, 1);
  ALine1.ValidateLine(35, 1);
  AssertEquals('invalid first from', 40, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   41, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       41, ALine1.LastInvalidLine);

  ALine1.ValidateLine(40, 1);
  AssertEquals('invalid first from', 41, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   41, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       41, ALine1.LastInvalidLine);

  ALine1.ValidateLine(41, 1);
  AssertEquals('invalid first from', -1, ALine1.FirstInvalidLine);
  AssertEquals('invalid first to',   -1, ALine1.FirstInvalidEndLine);
  AssertEquals('invalid last',       -1, ALine1.LastInvalidLine);

end;

procedure TTestWordWrap.TestWordWrapLineMapValidate;
var
  ANode1: TSynWordWrapIndexPage;
  ALine1: TSynWordWrapLineMap;
  ATestName: String;
  w: TExpWraps;
  i: Integer;
begin
  // invalidate/ re-validate => increase/decrease offset/tail by switching between wrap and one-line lines
  ANode1 := TSynWordWrapIndexPage(FTree.FindPageForLine(0, afmCreate).Page);
  ALine1 := ANode1.SynWordWrapLineMapStore;

  ATestName := 'fill one-lines at start - increasing';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 0 to 3 do begin
    ALine1.InvalidateLines(0, 3);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;

  ATestName := 'fill one-lines at start - decreasing';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 3 downto 0 do begin
    ALine1.InvalidateLines(0, 3);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;



  ATestName := 'fill one-lines at end - decreasing';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 9 downto 7 do begin
    ALine1.InvalidateLines(7, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;

  ATestName := 'fill one-lines at end - increasing';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 7 to 9 do begin
    ALine1.InvalidateLines(7, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;


  ATestName := 'fill one-lines - all, incr';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 0 to 9 do begin
    ALine1.InvalidateLines(0, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;

  ATestName := 'fill one-lines - all, decr';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 9 downto 0 do begin
    ALine1.InvalidateLines(0, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;


  ATestName := 'fill one-lines - all, incr then decr';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 0 to 4 do begin
    ALine1.InvalidateLines(0, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;
  for i := 9 downto 5 do begin
    ALine1.InvalidateLines(0, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;

  ATestName := 'fill one-lines - all, decr then incr';
  InitLine(ALine1, w.init(FillArray(10, 19)));
  w.Join([1,1]);
  for i := 9 downto 5 do begin
    ALine1.InvalidateLines(0, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;
  for i := 0 to 4 do begin
    ALine1.InvalidateLines(0, 9);
    w.w[i] := 1;
    ValidateNeededWraps(ALine1, w);
    AssertLineForWraps(Format('%s %d', [ATestName, i]), ALine1, w, True);
  end;

end;

procedure TTestWordWrap.TestWordWrapLineMapMerge;
var
  ANode1, ANode2: TSynWordWrapIndexPage;
  ALine1, ALine2: TSynWordWrapLineMap;
  ATestName: String;
  w: TExpWraps;
begin
  ANode1 := TSynWordWrapIndexPage(FTree.FindPageForLine(0, afmCreate).Page);
  ANode2 := TSynWordWrapIndexPage(FTree.FindPageForLine(100, afmCreate).Page);
  ALine1 := ANode1.SynWordWrapLineMapStore;
  ALine2 := ANode2.SynWordWrapLineMapStore;

  ATestName := 'Insert at start: no-offset => no-offset';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 0, 3);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 3);
  AssertLineForWraps('', ALine1, w.init([4, 5, 6,   2, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: no-offset => offset';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 0, 3);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 3);
  AssertLineForWraps('', ALine1, w.init([4, 5, 6,   1, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: offset => no offset';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 0, 3);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 3);
  AssertLineForWraps('', ALine1, w.init([1, 5, 6,   2, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: offset => offset';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 0, 3);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 3);
  AssertLineForWraps('', ALine1, w.init([1, 5, 6,   1, 1, 3, 3, 1,   1,1]));


  ATestName := 'Insert at start: no-offset 2nd => no-offset';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 1, 2);
  //ALine1.InsertLinesFromPage(ALine2, 1, 0, 2);
  AssertLineForWraps('', ALine1, w.init([5, 6,   2, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: no-offset 2nd => offset';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 1, 2);
  //ALine1.InsertLinesFromPage(ALine2, 1, 0, 2);
  AssertLineForWraps('', ALine1, w.init([5, 6,   1, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: offset 2nd => no offset';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 1, 2);
  //ALine1.InsertLinesFromPage(ALine2, 1, 0, 2);
  AssertLineForWraps('', ALine1, w.init([5, 6,   2, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: offset 2nd => offset';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 1, 2);
  //ALine1.InsertLinesFromPage(ALine2, 1, 0, 2);
  AssertLineForWraps('', ALine1, w.init([5, 6,   1, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: offset 3rd => no offset';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6, 7]));
  ALine2.MoveLinesAtEndTo(ALine1, 2, 2);
  //ALine1.InsertLinesFromPage(ALine2, 2, 0, 2);
  AssertLineForWraps('', ALine1, w.init([6, 7,   2, 1, 3, 3, 1,   1,1]));

  ATestName := 'Insert at start: offset 3rd => offset';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6, 7]));
  ALine2.MoveLinesAtEndTo(ALine1, 2, 2);
  //ALine1.InsertLinesFromPage(ALine2, 2, 0, 2);
  AssertLineForWraps('', ALine1, w.init([6, 7,   1, 1, 3, 3, 1,   1,1]));


  ATestName := 'Insert at start: overlen';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 0, 4);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 4);
  AssertLineForWraps(ATestName, ALine1, w.init([4, 5, 6, 1,   1, 1, 3, 3, 1,   1,1]));

  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtEndTo(ALine1, 1, 4);
  //ALine1.InsertLinesFromPage(ALine2, 1, 0, 4);
  AssertLineForWraps(ATestName, ALine1, w.init([5, 6, 1, 1,   1, 1, 3, 3, 1,   1,1]));

  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1]));
  ALine2.MoveLinesAtEndTo(ALine1, 1, 4);
  //ALine1.InsertLinesFromPage(ALine2, 1, 0, 4);
  AssertLineForWraps(ATestName, ALine1, w.init([1, 1, 1, 1,   1, 1, 3, 3, 1,   1,1]));



  ATestName := 'Insert at end';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtStartTo(ALine1, 2, 5);
  //ALine1.InsertLinesFromPage(ALine2, 0, 5, 3);
  AssertLineForWraps(ATestName, ALine1, w.init([1, 1, 3, 3, 1,   4, 5, 6,   1,1]));

  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.MoveLinesAtStartTo(ALine1, 2, 6);
  //ALine1.InsertLinesFromPage(ALine2, 0, 6, 3);
  AssertLineForWraps(ATestName, ALine1, w.init([1, 1, 3, 3, 1,   1, 4, 5, 6,   1,1]));


  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6]));
  ALine2.MoveLinesAtStartTo(ALine1, 2, 5);
  //ALine1.InsertLinesFromPage(ALine2, 0, 5, 3);
  AssertLineForWraps(ATestName, ALine1, w.init([1, 1, 3, 3, 1,   1, 5, 6,   1,1]));

  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([1, 5, 6]));
  ALine2.MoveLinesAtStartTo(ALine1, 2, 6);
  //ALine1.InsertLinesFromPage(ALine2, 0, 6, 3);
  AssertLineForWraps(ATestName, ALine1, w.init([1, 1, 3, 3, 1,   1, 1, 5, 6,   1,1]));

end;

procedure TTestWordWrap.TestWordWrapLineMapMergeInvalidate;
var
  ANode1, ANode2: TSynWordWrapIndexPage;
  ALine1, ALine2: TSynWordWrapLineMap;
  ATestName: String;
  w: TExpWraps;
begin
  ANode1 := TSynWordWrapIndexPage(FTree.FindPageForLine(0, afmCreate).Page);
  ANode2 := TSynWordWrapIndexPage(FTree.FindPageForLine(100, afmCreate).Page);
  ALine1 := ANode1.SynWordWrapLineMapStore;
  ALine2 := ANode2.SynWordWrapLineMapStore;

  ATestName := 'Insert at start: target inval';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine1.InvalidateLines(1, 1);
  ALine2.MoveLinesAtEndTo(ALine1, 0, 3);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 3);
  AssertLineForWraps('', ALine1, w.init([4, 5, 6,   2, 1, 3, 3, 1,   1,1]));
  AssertEquals('invalid', 4, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 4, ALine1.LastInvalidLine);

  ATestName := 'Insert at start: source inval';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.InvalidateLines(1, 1);
  ALine2.MoveLinesAtEndTo(ALine1, 0, 3);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 3);
  AssertLineForWraps('', ALine1, w.init([4, 5, 6,   2, 1, 3, 3, 1,   1,1]));
  AssertEquals('invalid', 1, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 1, ALine1.LastInvalidLine);

  ATestName := 'Insert at start: source inval';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.InvalidateLines(0, 1);
  ALine2.MoveLinesAtEndTo(ALine1, 1, 2);
  //ALine1.InsertLinesFromPage(ALine2, 1, 0, 2);
  AssertLineForWraps('', ALine1, w.init([5, 6,   2, 1, 3, 3, 1,   1,1]));
  AssertEquals('invalid', 0, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 0, ALine1.LastInvalidLine);

  ATestName := 'Insert at start: source inval';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.InvalidateLines(0, 1);
  ALine2.MoveLinesAtEndTo(ALine1, 2, 1);
  //ALine1.InsertLinesFromPage(ALine2, 2, 0, 1);
  AssertLineForWraps('', ALine1, w.init([6,   2, 1, 3, 3, 1,   1,1]));
  AssertEquals('invalid', -1, ALine1.FirstInvalidLine);
  AssertEquals('invalid', -1, ALine1.LastInvalidLine);

  ATestName := 'Insert at start: both inval';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine1.InvalidateLines(1, 1);
  ALine2.InvalidateLines(2, 2);
  ALine2.MoveLinesAtEndTo(ALine1, 0, 3);
  //ALine1.InsertLinesFromPage(ALine2, 0, 0, 3);
  AssertLineForWraps('', ALine1, w.init([4, 5, 6,   2, 1, 3, 3, 1,   1,1]));
  AssertEquals('invalid', 2, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 4, ALine1.LastInvalidLine);


  ATestName := 'Insert at end: source inval';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([4, 5, 6]));
  ALine2.InvalidateLines(2, 2);
  ALine2.MoveLinesAtStartTo(ALine1, 2, 5);
  //ALine1.InsertLinesFromPage(ALine2, 0, 5, 3);
  AssertLineForWraps(ATestName, ALine1, w.init([1, 1, 3, 3, 1,   4, 5, 6,   1,1]));
  AssertEquals('invalid', 7, ALine1.FirstInvalidLine);
  AssertEquals('invalid', 7, ALine1.LastInvalidLine);



  ATestName := 'Insert from end to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtEndTo(ALine2, 3, 3);
  AssertLineForWraps(ATestName, ALine2, w.init([3, 1, 1,  1,1,1]));

  ATestName := 'Insert from end to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtEndTo(ALine2, 2, 3);
  AssertLineForWraps(ATestName, ALine2, w.init([3, 3, 1,  1,1,1]));

  ATestName := 'Insert from end to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtEndTo(ALine2, 1, 3);
  AssertLineForWraps(ATestName, ALine2, w.init([1, 3, 3,  1,1,1]));

  ATestName := 'Insert from end to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtEndTo(ALine2, 1, 4);
  AssertLineForWraps(ATestName, ALine2, w.init([1, 3, 3, 1,  1,1,1]));

  ATestName := 'Insert from after end to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtEndTo(ALine2, 6, 3);
  AssertLineForWraps(ATestName, ALine2, w.init([1, 1, 1,  1,1,1]));


  ATestName := 'Insert from start to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtStartTo(ALine2, 2, 0);
  AssertLineForWraps(ATestName, ALine2, w.init([1, 1, 3,    1,1,1]));

  ATestName := 'Insert from start to empty';
  InitLine(ALine1, w.init([2, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtStartTo(ALine2, 2, 0);
  AssertLineForWraps(ATestName, ALine2, w.init([2, 1, 3,    1,1,1,1]));

  ATestName := 'Insert from start to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtStartTo(ALine2, 1, 0);
  AssertLineForWraps(ATestName, ALine2, w.init([1, 1,     1,1,1,1]));

  ATestName := 'Insert from start to empty';
  InitLine(ALine1, w.init([1, 1, 3, 3, 1]));
  InitLine(ALine2, w.init([]));
  ALine1.MoveLinesAtStartTo(ALine2, 2, 3);
  AssertLineForWraps(ATestName, ALine2, w.init([1, 1, 1,  1, 1, 3,    1,1,1]));


end;


procedure TTestWordWrap.TestWordWrapTreeInsertThenDelete;
var
  CurWraps: TExpWraps;
  InsPos, InsLen, DelPos, DelCount: Integer;
begin
  FTree.Free;
  FTree := CreateTree(2, 9, 4);
//  FTree := TSynLineMapAVLTree.Create(TSynWordWrapIndexPage, 2, 11, 4);
  for DelPos := 0 to 26 do
  for DelCount := 1 to Min(5, 27-DelPos) do
  for InsPos := 0 to 29 do
  for InsLen := 1 to 4 do begin
    FTree.Clear;

    // init
    CurWraps.Fill(10, 10+26);
    FTree.AdjustForLinesInserted(0, 27, 0);
    ValidateTreeWraps(CurWraps);
    //FTree.DebugDump;
    AssertTreeForWraps(Format('Before ins at pos %d Len %d', [InsPos, InsLen]), CurWraps);

    // ins
    CurWraps.Join(FillArray(500, 499+InsLen), InsPos);
    FTree.AdjustForLinesInserted(InsPos, InsLen, 0);
    //FTree.DebugDump;
    ValidateTreeWraps(CurWraps);
    //FTree.DebugDump;
    AssertTreeForWraps(Format('After ins at pos %d Len %d ins at pos %d Len %d', [DelPos, DelCount, InsPos, InsLen]), CurWraps);

    // del
    FTree.AdjustForLinesDeleted(DelPos, DelCount, 0);
    CurWraps.SpliceArray(DelPos, DelCount);
    AssertTrue(Format('valid After del at pos %d Len %d ins at pos %d Len %d', [DelPos, DelCount, InsPos, InsLen]),
      not FTree.NeedsValidation);
    AssertTreeForWraps(Format('After del at pos %d Len %d ins at pos %d Len %d', [DelPos, DelCount, InsPos, InsLen]), CurWraps);
  end;
end;

procedure TTestWordWrap.TestWordWrapTreeDeleteThenInsert;
var
  CurWraps: TExpWraps;
  InsPos, InsLen, DelPos, DelCount: Integer;
begin
  FTree.Free;
  FTree := CreateTree(2, 9, 4);
//  FTree := TSynLineMapAVLTree.Create(TSynWordWrapIndexPage, 2, 11, 4);
  for DelPos := 0 to 26 do
  for DelCount := 1 to Min(5, 27-DelPos) do
  for InsPos := 0 to 29 do
  for InsLen := 1 to 4 do begin
//if  (InsPos<>15) or (InsLen<>1) or (DelPos<>0) or (DelCount<>1) then continue;
    FTree.Clear;

    // init
    CurWraps.Fill(10, 10+26);
    FTree.AdjustForLinesInserted(0, 27, 0);
    ValidateTreeWraps(CurWraps);
    //FTree.DebugDump;
    AssertTreeForWraps(Format('Before del at pos %d Len %d ins at pos %d Len %d', [DelPos, DelCount, InsPos, InsLen]), CurWraps);

    // del
    FTree.AdjustForLinesDeleted(DelPos, DelCount, 0);
    CurWraps.SpliceArray(DelPos, DelCount);
    AssertTrue(Format('valid After del at pos %d Len %d ins at pos %d Len %d', [DelPos, DelCount, InsPos, InsLen]),
      not FTree.NeedsValidation);
    AssertTreeForWraps(Format('After del at pos %d Len %d ins at pos %d Len %d', [DelPos, DelCount, InsPos, InsLen]), CurWraps);

    // ins
    FTree.AdjustForLinesInserted(InsPos, InsLen, 0);
    CurWraps.Join(FillArray(500, 499+InsLen), InsPos);
    //FTree.DebugDump;
    ValidateTreeWraps(CurWraps);
    //FTree.DebugDump;
    AssertTreeForWraps(Format('After del/ins : del at pos %d Len %d ins at pos %d Len %d', [DelPos, DelCount, InsPos, InsLen]), CurWraps);
  end;
end;

{ TTestWordWrapPluginBase }

procedure TTestWordWrapPluginBase.ClearCaret;
begin
  SynEdit.CaretXY := Point(2,2);
  SynEdit.CaretXY := Point(1,1);
end;

procedure TTestWordWrapPluginBase.SetCaret(SourcePt: TPointType; APos: TPoint);
begin
  case SourcePt of
    ptViewed:          SynEdit.CaretObj.ViewedLineCharPos := Apos;
    ptAlternateViewed: SynEdit.CaretObj.ViewedLineCharPos := Apos;
    ptPhys:            SynEdit.CaretObj.LineCharPos       := Apos;
    ptLog:             SynEdit.CaretObj.LineBytePos       := APos;
  end;
end;

procedure TTestWordWrapPluginBase.TestCaret(AName: String;SourcePt, ExpPt: TPointType;
  AnExp: TPoint; AnExpOffs: Integer);
var
  got: TPoint;
  src, dest: String;
begin
  case ExpPt of
    ptViewed:          got := SynEdit.CaretObj.ViewedLineCharPos;
    ptAlternateViewed: got := SynEdit.CaretObj.ViewedLineCharPos;
    ptPhys:            got := SynEdit.CaretObj.LineCharPos;
    ptLog:             got := SynEdit.CaretObj.LineBytePos;
  end;
  writestr(src, SourcePt);
  writestr(dest, ExpPt);
  AssertEquals(Format('%s (%s -> %s)', [AName, src, dest]), AnExp, got);
  if (ExpPt = ptLog) and (AnExpOffs >= 0) then
    AssertEquals(Format('%s (%s -> %s) Offs: ', [AName, src, dest]), AnExpOffs, SynEdit.CaretObj.BytePosOffset);
end;

class procedure TTestWordWrapPluginBase.AssertEquals(const AMessage: string;
  Expected, Actual: TPoint);
begin
  AssertEquals(AMessage, dbgs(Expected), dbgs(Actual));
end;

procedure TTestWordWrapPluginBase.AddLines(AFirstLineIdx, ACount,
  ALen: Integer; AnID: String; SkipBeginUpdate: Boolean);
var
  i, j: Integer;
  l: String;
begin
  if not SkipBeginUpdate then SynEdit.BeginUpdate;
  for i := 0 to ACount - 1 do begin
    l := '';
    j := 0;
    while Length(l) < ALen do begin
      l := l + copy(AnID+'_'+IntToStr(i)+'_'+IntToStr(j) + '            ',1,12);
      inc(j);
    end;
    l := copy(l, 1, ALen);
    SynEdit.Lines.Insert(AFirstLineIdx + i, l);
  end;
  if not SkipBeginUpdate then SynEdit.EndUpdate;
end;

procedure TTestWordWrapPluginBase.InternalCheckLine(AName: String;
  dsp: TLazSynDisplayView; ALine: TLineIdx; AExpTextStart: String;
  NoTrim: Boolean);
var
  gotRealLine: TLineIdx;
  gotStartPos, GotLineLen: Integer;
  gotTokenOk: Boolean;
  gotToken: TLazSynDisplayTokenInfo;
  gotText: PChar;
  s: String;
begin
  dsp.SetHighlighterTokensLine(ALine, gotRealLine, gotStartPos, GotLineLen);
  gotTokenOk := dsp.GetNextHighlighterToken(gotToken);
  if gotTokenOk then
    gotText := gotToken.TokenStart
  else
    gotText := '';

  if AExpTextStart = '' then begin
    AssertEquals(AName, '', gotText);
    AssertEquals(AName, 0, GotLineLen);
    exit;
  end
  else
  if gotText = '' then begin
    AssertTrue(AName, False);
    exit;
  end;

  if NoTrim then
    s := copy(gotText, 1, Length(AExpTextStart))
  else
    s := copy(Trim(gotText), 1, Length(AExpTextStart));
  if not(AExpTextStart = s) then begin
    debugln(['Failed ', AName, ' ', ALine, ':']);
    DebugLn(['GOT: "', StringReplace(gotText, #9, '#9', [rfReplaceAll]), '"']);
    DebugLn(['EXP: "', StringReplace(AExpTextStart, #9, '#9', [rfReplaceAll]), '"']);
  end;
  AssertEquals(AName, AExpTextStart, s);
end;

procedure TTestWordWrapPluginBase.CheckLine(AName: String; ALine: TLineIdx;
  AExpTextStart: String; NoTrim: Boolean);
var
  v: TSynEditStringsLinked;
  dsp: TLazSynDisplayView;
begin
  v := SynEdit.TextViewsManager.SynTextView[SynEdit.TextViewsManager.Count - 1];
  dsp := v.DisplayView;
  dsp.InitHighlighterTokens(nil);
  try
    InternalCheckLine(AName, dsp, ALine, AExpTextStart, NoTrim);
  finally
    dsp.FinishHighlighterTokens;
  end;
end;

procedure TTestWordWrapPluginBase.CheckLines(AName: String;
  AStartLine: TLineIdx; AExpTextStart: array of String; NoTrim: Boolean);
var
  v: TSynEditStringsLinked;
  dsp: TLazSynDisplayView;
  i, gotStartPos, GotLineLen: Integer;
  gotTokenOk: Boolean;
  gotToken: TLazSynDisplayTokenInfo;
  s: String;
  gotRealLine: TLineIdx;
begin
  v := SynEdit.TextViewsManager.SynTextView[SynEdit.TextViewsManager.Count - 1];
  dsp := v.DisplayView;
  dsp.InitHighlighterTokens(nil);
  try
    try
      for i := 0 to Length(AExpTextStart)-1 do
        InternalCheckLine(AName, dsp, AStartLine+i, AExpTextStart[i], NoTrim);
    except
      dsp.FinishHighlighterTokens;
      dsp.InitHighlighterTokens(nil);
      for i := 0 to Length(AExpTextStart)-1 do begin
        dsp.SetHighlighterTokensLine(AStartLine+i, gotRealLine, gotStartPos, GotLineLen);
        s := '';
        while dsp.GetNextHighlighterToken(gotToken) and (gotToken.TokenLength > 0) do
          s := s + copy(gotToken.TokenStart, 1, gotToken.TokenLength);
        debugln('Line %d (real %d): "%s" %d/%d start %d',
          [AStartLine+i, gotRealLine, StringReplace(s, #9, '#9', [rfReplaceAll]), length(s), GotLineLen, gotStartPos]);
      end;
      raise;
    end;
  finally
    dsp.FinishHighlighterTokens;
  end;
end;

procedure TTestWordWrapPluginBase.CheckXyMap(AName: String; APhysTExtXY,
  AViewedXY: TPoint; OnlyViewToText: Boolean);
var
  v: TSynEditStringsLinked;
  GotTextXY, GotViewXY: TPoint;
begin
  v := SynEdit.TextViewsManager.SynTextView[SynEdit.TextViewsManager.Count - 1];
  GotTextXY := v.ViewXYToTextXY(AViewedXY);
  GotViewXY := v.TextXYToViewXY(APhysTExtXY);

  AssertTrue(Format('%s: Viewed %s to Text %s (exp) => got %s', [AName, dbgs(AViewedXY), dbgs(APhysTExtXY), dbgs(GotTextXY)]),
             (GotTextXY.x = APhysTExtXY.x) and (GotTextXY.y = APhysTExtXY.y) );
  if not OnlyViewToText then
  AssertTrue(Format('%s: Text %s to viewed %s (exp) => got %s', [AName, dbgs(APhysTExtXY), dbgs(AViewedXY), dbgs(GotViewXY)]),
             (GotViewXY.x = AViewedXY.x) and (GotViewXY.y = AViewedXY.y) );
end;

procedure TTestWordWrapPluginBase.CheckXyMap(AName: String; APhysTExtX,
  APhysTExtY, AViewedX, AViewedY: integer; OnlyViewToText: Boolean);
begin
  CheckXyMap(AName, Point(APhysTExtX, APhysTExtY), Point(AViewedX, AViewedY), OnlyViewToText);
end;

procedure TTestWordWrapPluginBase.CheckXyMap(AName: String;
  APoints: TPointSpecs);
var
  StartP, TestP: TPointType;
begin
  CheckXyMap(AName + 'XyMap', APoints.xy[ptPhys], APoints.xy[ptViewed]);
  if APoints.xy[ptAlternateViewed].x > 0 then
    CheckXyMap(AName+ 'XyMap(a)', APoints.xy[ptPhys], APoints.xy[ptAlternateViewed], True);


  for TestP in TPointType do begin
    if TestP = ptAlternateViewed then
      continue;

    ClearCaret;
    SynEdit.CaretXY := APoints.XY[ptPhys];
    TestCaret(AName, ptPhys, TestP, APoints.XY[TestP], APoints.LogOffs);

    if APoints.LogOffs <= 0 then begin
      ClearCaret;
      SynEdit.LogicalCaretXY := APoints.XY[ptLog];
      TestCaret(AName, ptLog, TestP, APoints.XY[TestP], APoints.LogOffs);
    end;

    AName := AName + ' [CaretObj] ';
    for StartP in TPointType do begin
      if APoints.xy[StartP].x <= 0 then
        Continue;
    if (StartP = ptAlternateViewed) then // and (TestP = ptViewed) then
      continue;
    if (StartP = ptLog) and (APoints.LogOffs > 0) then
      continue;

      ClearCaret;
      SetCaret(StartP, APoints.XY[StartP]);
      TestCaret(AName, StartP, TestP, APoints.XY[TestP], APoints.LogOffs);
    end;
  end;

end;

procedure TTestWordWrapPluginBase.CheckXyMap(AName: String;
  APoints: TPointSpecs; ATestCommands: array of TCommandAndPointSpecs);
var
  i: Integer;
  StartP, TestP: TPointType;
  n: string;
  c: TSynEditorCommand;
begin
  CheckXyMap(AName+'(p)', APoints);

  for i := 0 to Length(ATestCommands) - 1 do begin
    if not ATestCommands[i].RunOnlyIf then
      Continue;
    n := AName+'(p'+IntToStr(i)+')';
    CheckXyMap(n, ATestCommands[i].Exp);

    for StartP in TPointType do begin
      if APoints.xy[StartP].x <= 0 then
        Continue;
      if (StartP = ptAlternateViewed) then // and (TestP = ptViewed) then
        continue;
      if (StartP = ptLog) and (APoints.LogOffs > 0) then
        continue;

      for TestP in TPointType do begin
        if TestP = ptAlternateViewed then
          continue;
        ClearCaret;
        SetCaret(StartP, APoints.XY[StartP]);
        for c in ATestCommands[i].Cmd do
          SynEdit.ExecuteCommand(c, '', nil);

        TestCaret(n, StartP, TestP, ATestCommands[i].Exp.XY[TestP], ATestCommands[i].Exp.LogOffs);
      end;
    end;
  end;
end;

procedure TTestWordWrapPluginBase.SetUp;
begin
  inherited SetUp;
  FWordWrap := TLazSynEditLineWrapPlugin.Create(SynEdit);
end;

procedure TTestWordWrapPluginBase.TearDown;
begin
  inherited TearDown;
end;

{ TTestWordWrapPlugin }

procedure TTestWordWrapPlugin.TestEditorWrap;
var
  SkipTab, AllowPastEOL, KeepX: Boolean;
begin
  SynEdit.Options := [];
  SynEdit.TabWidth := 4;

  SetLines([
    'abc def ' + 'ABC DEFG ' + 'XYZ',
    //'A'  #9'B'  #9'C ' + 'DEF G'#9'H'  #9 + ''   #9   #9'xy',
    'A'#9'B'#9'C ' + 'DEF G'#9'H'#9 + #9#9'xy',
    'äää ööö ' + 'ÄÄÄ ÖÖÖ ' + 'ÜÜÜ',
    '999'
  ]);

  SetSynEditWidth(10);
  CheckLines('', 0, [
    'abc def ',
    'ABC DEFG ',
    'XYZ',
    'A'#9'B'#9'C ',
    'DEF G'#9'H'#9,
    #9#9'xy',
    'äää ööö ',
    'ÄÄÄ ÖÖÖ ',
    'ÜÜÜ',
    '999'
  ], True);


  //CheckXyMap('', pt(4,3,   21,1,   21,1); // after "Z" EOL
  for AllowPastEOL in boolean do
  for KeepX in boolean do
  for SkipTab in boolean do begin
    if AllowPastEOL
    then SynEdit.Options := SynEdit.Options + [eoScrollPastEol]
    else SynEdit.Options := SynEdit.Options - [eoScrollPastEol];
    if SkipTab
    then SynEdit.Options2 := SynEdit.Options2 + [eoCaretSkipTab]
    else SynEdit.Options2 := SynEdit.Options2 - [eoCaretSkipTab];
    if KeepX
    then SynEdit.Options := SynEdit.Options + [eoKeepCaretX]
    else SynEdit.Options := SynEdit.Options - [eoKeepCaretX];

    FWordWrap.CaretWrapPos := wcpEOL;
    CheckXyMap('wcpEOL', p( 1,1,           1,1,    1,1),
                        [c(ecRight,                         2,1,   2,1,    2,1,0),
                         c(ecDown,                          2,2,  10,1,   10,1,0),
                         c([ecDown,ecDown],                 2,3,  19,1,   19,1,0),
                         c([ecDown,ecDown, ecRight],        3,3,  20,1,   20,1,0),
                         c([ecDown,ecDown, ecRight,ecDown], 2,4,   2,2,    2,2,0, SkipTab),
                         c([ecDown,ecDown, ecRight,ecDown], 3,4,   3,2,    2,2,1, not SkipTab),
                         c([ecDown,ecDown,ecDown],          1,4,   1,2,    1,2,0, KeepX),
                         c([ecDown,ecDown,ecDown],          2,4,   2,2,    2,2,0, not KeepX),
                         c([ecDown,ecDown,ecDown,ecDown],   2,5,  12,2,    8,2,0)
                        ]);
    CheckXyMap('wcpEOL', p( 2,1,           2,1,    2,1),
                        [c(ecDown,                          2,2,  10,1,   10,1,0)
                        ]);
    CheckXyMap('wcpEOL', p( 7,1,           7,1,    7,1), // after "e"
                        [c([ecColSelDown],              7,4,   7,2,   4,2,1,  not SkipTab ),    // A#9B#|9  // 1 in tab
                         c([ecColSelDown],              6,4,   6,2,   4,2,0,  SkipTab ),    // A#9B|#9  // 1 in tab
                         c([ecColSelDown,ecColSelDown], 7,7,   7,3,  12,3,0,  (not SkipTab) or KeepX ),
                         c([ecColSelDown,ecColSelDown], 6,7,   6,3,  10,3,0,  SkipTab and not KeepX ),
                         c([ecColSelDown,ecColSelDown,ecColSelDown], 4,10,   4,4,  4,4,0, not AllowPastEOL),
                         c([ecColSelDown,ecColSelDown,ecColSelDown], 7,10,   7,4,  7,4,0, AllowPastEOL and ((not SkipTab) or KeepX) )
                        ]);
    CheckXyMap('wcpEOL', p( 8,1,           8,1,    8,1));
    if AllowPastEOL then
    CheckXyMap('wcpEOL', p( 9,1,   1,2,    9,1,    9,1), // def |
                        [c([ecDown],                        9,2,  17,1,   17,1,0), // DEFG|
                         c([ecDown,ecDown],                 9,3,  26,1,   26,1,0), // XYZ     |
                         c([ecDown,ecDown,ecDown],          9,4,   9,2,    5,2,0), // A#9B#9|C
                         c([ecDown,ecDown,ecDown,ecDown],   8,5,  18,2,   14,2,0,  SkipTab),     // G#9|H#9
                         c([ecDown,ecDown,ecDown,ecDown],   9,5,  19,2,   14,2,1,  not SkipTab), // G#9H#|9 //in #9
                         c([ecDown,ecDown,ecDown,ecDown,ecDown],   9,6,  29,2,   17,2,0,  (not SkipTab) or KeepX), // before x
                         c([ecDown,ecDown,ecDown,ecDown,ecDown],   5,6,  25,2,   16,2,0,  SkipTab       and not KeepX),  // in tab, before x
                         c([ecDown,ecDown,ecDown,ecDown,ecDown,ecDown],   9,7,   9,3,   15,3,0,  (not SkipTab) or KeepX),
                         c([ecDown,ecDown,ecDown,ecDown,ecDown,ecDown],   5,7,   5,3,    8,3,0,  SkipTab and not KeepX)
                        ])
    else // not AllowPastEOL then
    CheckXyMap('wcpEOL', p( 9,1,   1,2,    9,1,    9,1), // after " " / still on line 1
                        [c([ecDown],                        9,2,  17,1,   17,1,0),             // DEFG|
                         c([ecDown,ecDown],                 4,3,  21,1,   21,1,0),             // XYZ|
                         c([ecDown,ecDown,ecDown],          9,4,   9,2,    5,2,0,  KeepX),     // A#9B#9|C
                         c([ecDown,ecDown,ecDown],          4,4,   4,2,    2,2,2,  (not KeepX) and (not SkipTab) ), // A#|9B#9C //in tab
                         c([ecDown,ecDown,ecDown],          2,4,   2,2,    2,2,0,  (not KeepX) and SkipTab)  // A#|9B#9C //in tab
                        ]);
    CheckXyMap('wcpEOL', p( 2,2,          10,1,   10,1));
    CheckXyMap('wcpEOL', p( 4,2,          12,1,   12,1), // ABC| DE
                         [c([ecColSelDown],              2,5,  12,2,   8,2,0),             // D|EF G#9
                          c([ecColSelDown,ecColSelDown], 4,8,  12,3,   21,3,0),
                          c([ecColSelDown,ecColSelDown,ecColSelDown], 4,10,  4,4,   4,4,0,  not AllowPastEOL)
                         ]);
    CheckXyMap('wcpEOL', p( 9,2,          17,1,   17,1)); // after "G"
    CheckXyMap('wcpEOL', p(10,2,   1,3,   18,1,   18,1)); // after " "
    CheckXyMap('wcpEOL', p( 2,3,          19,1,   19,1)); // after "X"
    CheckXyMap('wcpEOL', p( 4,3,          21,1,   21,1)); // after "Z" EOL
    if AllowPastEOL then
    CheckXyMap('wcpEOL', p( 5,3,          22,1,   22,1)); // at EOL + 1

    CheckXyMap('wcpEOL', p( 1,4,           1,2,    1,2),
                        [c([ecDown],                  2,5,  12,2,   8,2,0),
                         c([ecDown,ecDown],           5,6,  25,2,  16,2,0,  SkipTab),
                         c([ecDown,ecDown],           2,6,  22,2,  15,2,1,  not SkipTab),
                         c([ecDown,ecDown,ecDown],    1,7,   1,3,   1,3,0,  KeepX),
                         c([ecDown,ecDown,ecDown],    5,7,   5,3,   8,3,0,  (SkipTab) and not KeepX)
                        ]);
    CheckXyMap('wcpEOL', p( 2,4,           2,2,    2,2, 0)); // before tab
    if not SkipTab then
    CheckXyMap('wcpEOL', p( 3,4,           3,2,    2,2, 1)); // 1 inside tab
    CheckXyMap('wcpEOL', p( 5,4,           5,2,    3,2, 0)); // after tab
    CheckXyMap('wcpEOL', p( 9,4,           9,2,    5,2)); // after tab, before "C"
    CheckXyMap('wcpEOL', p(10,4,          10,2,    6,2)); // after "C"
    CheckXyMap('wcpEOL', p(11,4,   1,5,   11,2,    7,2)); // after " "
    CheckXyMap('wcpEOL', p( 2,5,          12,2,    8,2)); // after "D"
    CheckXyMap('wcpEOL', p( 8,5,          18,2,   14,2)); // after "H"
    if not SkipTab then
    CheckXyMap('wcpEOL', p( 9,5,          19,2,   14,2, 1)); // in #9
    if not SkipTab then
    CheckXyMap('wcpEOL', p(10,5,          20,2,   14,2, 2)); // in #9
    CheckXyMap('wcpEOL', p(11,5,   1,6,   21,2,   15,2)); // after #9
    if not SkipTab then
    CheckXyMap('wcpEOL', p( 2,6,          22,2,   15,2, 1)); // in #9 / next line
    CheckXyMap('wcpEOL', p( 5,6,          25,2,   16,2)); // after 1st #9 / next line
    CheckXyMap('wcpEOL', p(10,6,          30,2,   18,2)); // after "x"
    CheckXyMap('wcpEOL', p(11,6,          31,2,   19,2)); // after "z" EOL

    CheckXyMap('wcpEOL', p( 1,7,           1,3,    1,3));
    CheckXyMap('wcpEOL', p( 2,7,           2,3,    3,3));
    CheckXyMap('wcpEOL', p( 8,7,           8,3,   14,3)); // after "ö"
    CheckXyMap('wcpEOL', p( 9,7,   1,8,    9,3,   15,3)); // after " "
    CheckXyMap('wcpEOL', p( 2,8,          10,3,   17,3)); // after "Ä"
    CheckXyMap('wcpEOL', p( 9,8,   1,9,   17,3,   29,3)); // after " "

    FWordWrap.CaretWrapPos := wcpBOL;
    CheckXyMap('wcpBOL', p( 1,1,           1,1,    1,1),
                        [c(ecRight,                         2,1,   2,1,    2,1,0),
                         c(ecDown,                          1,2,   9,1,    9,1,0),
                         c([ecDown,ecDown],                 1,3,  18,1,   18,1,0),
                         c([ecDown,ecDown, ecRight,ecRight],        3,3,  20,1,   20,1,0),
                         c([ecDown,ecDown, ecRight,ecRight,ecDown], 2,4,   2,2,    2,2,0, SkipTab),
                         c([ecDown,ecDown, ecRight,ecRight,ecDown], 3,4,   3,2,    2,2,1, not SkipTab),
                         c([ecDown,ecDown,ecDown],          1,4,   1,2,    1,2,0),
                         c([ecDown,ecDown,ecDown,ecDown],   1,5,  11,2,    7,2,0)
                        ]);
    CheckXyMap('wcpBOL', p( 2,1,           2,1,    2,1),
                        [c(ecDown,                          2,2,  10,1,   10,1,0)
                        ]);
    CheckXyMap('wcpBOL', p( 7,1,           7,1,    7,1)); // after "e"
    CheckXyMap('wcpBOL', p( 8,1,           8,1,    8,1));
    CheckXyMap('wcpBOL', p( 1,2,   9,1,    9,1,    9,1)); // after " " / still on line 1
    CheckXyMap('wcpBOL', p( 2,2,          10,1,   10,1));
    CheckXyMap('wcpBOL', p( 9,2,          17,1,   17,1), // after "G"
                        [c([ecUp],                          8,1,   8,1,   8,1,0),
                         c([ecUp, ecDown],                  9,2,  17,1,  17,1,0,  KeepX),
                         c([ecUp, ecDown],                  8,2,  16,1,  16,1,0,  not KeepX)
                        ]);
    CheckXyMap('wcpBOL', p( 1,3,  10,2,   18,1,   18,1)); // after " "
    CheckXyMap('wcpBOL', p( 2,3,          19,1,   19,1)); // after "X"
    CheckXyMap('wcpBOL', p( 4,3,          21,1,   21,1)); // after "Z" EOL
    if AllowPastEOL then
    CheckXyMap('wcpBOL', p( 5,3,          22,1,   22,1)); // at EOL + 1

    CheckXyMap('wcpBOL', p( 1,4,           1,2,    1,2),
                        [c([ecDown],                  1,5,  11,2,   7,2,0),
                         c([ecDown,ecDown],           1,6,  21,2,  15,2,0),
                         c([ecDown,ecDown,ecDown],    1,7,   1,3,   1,3,0)
                        ]);
    CheckXyMap('wcpBOL', p( 2,4,           2,2,    2,2, 0)); // before tab
    if not SkipTab then
    CheckXyMap('wcpBOL', p( 3,4,           3,2,    2,2, 1)); // 1 inside tab
    CheckXyMap('wcpBOL', p( 5,4,           5,2,    3,2, 0)); // after tab
    CheckXyMap('wcpBOL', p( 9,4,           9,2,    5,2)); // after tab, before "C"
    CheckXyMap('wcpBOL', p(10,4,          10,2,    6,2), // after "C"
                        [c([ecDown],                        8,5,  18,2,   14,2,0, SkipTab),      // G#9H#|9
                         c([ecDown],                       10,5,  20,2,   14,2,2, not SkipTab),  // G#9H#|9
                         c([ecDown,ecDown],                10,6,  30,2,   18,2,0,  KeepX),       // #9#9X|Y
                         c([ecDown,ecDown],                 5,6,  25,2,   16,2,0,  (not KeepX) and SkipTab)    // #9#9X|Y
                        ]);
    CheckXyMap('wcpBOL', p( 1,5,  11,4,   11,2,    7,2)); // after " "
    CheckXyMap('wcpBOL', p( 2,5,          12,2,    8,2)); // after "D"
    CheckXyMap('wcpBOL', p( 8,5,          18,2,   14,2)); // after "H"
    if not SkipTab then
    CheckXyMap('wcpBOL', p( 9,5,          19,2,   14,2, 1)); // in #9
    if not SkipTab then
    CheckXyMap('wcpBOL', p(10,5,          20,2,   14,2, 2)); // in #9
    CheckXyMap('wcpBOL', p( 1,6,  11,5,   21,2,   15,2)); // after #9
    if not SkipTab then
    CheckXyMap('wcpBOL', p( 2,6,          22,2,   15,2, 1)); // in #9 / next line
    CheckXyMap('wcpBOL', p( 5,6,          25,2,   16,2)); // after 1st #9 / next line
    CheckXyMap('wcpBOL', p(10,6,          30,2,   18,2)); // after "x"
    CheckXyMap('wcpBOL', p(11,6,          31,2,   19,2), // after "y" EOL
                        [c([ecUp],                        8,5,  18,2,   14,2,0, SkipTab),      // G#9H#|9
                         c([ecUp],                       10,5,  20,2,   14,2,2, not SkipTab),  // G#9H#|9
                         c([ecUp,ecDown],                11,6,  31,2,   19,2,0,  KeepX)
                        ]);

    CheckXyMap('wcpBOL', p( 1,7,           1,3,    1,3));
    CheckXyMap('wcpBOL', p( 2,7,           2,3,    3,3));
    CheckXyMap('wcpBOL', p( 8,7,           8,3,   14,3)); // after "ö"
    CheckXyMap('wcpBOL', p( 1,8,   9,7,    9,3,   15,3)); // after " "
    CheckXyMap('wcpBOL', p( 2,8,          10,3,   17,3)); // after "Ä"
    CheckXyMap('wcpBOL', p( 1,9,   9,8,   17,3,   29,3)); // after " "

  end;





  SynEdit.ExecuteCommand(ecEditorBottom, '', nil);
  AssertEquals('ecEditorBottom', 4 ,SynEdit.CaretY);
  AssertEquals('ecEditorBottom', 10 ,SynEdit.CaretObj.ViewedLineCharPos.y);

  SetSynEditWidth(65);
  AddLines(0, 6000, 60, 'A');

  CheckLine('', 0, 'A_0_0');
  CheckLine('', 1, 'A_1_0');
  CheckLine('', 2, 'A_2_0');
  CheckLine('', 3, 'A_3_0');

  SetSynEditWidth(35);
  CheckLine('', 0, 'A_0_0');
  CheckLine('', 1, 'A_0_3');
  CheckLine('', 2, 'A_1_0');

// '		'

end;

procedure TTestWordWrapPlugin.TestEditorEdit;
begin
  SynEdit.Options := [];
  SynEdit.TabWidth := 4;

  SetLines([
    'abc def ' + 'ABC DEFG ' + 'XYZ',
    '',
    //'A'  #9'B'  #9'C ' + 'DEF G'#9'H'  #9 + ''   #9   #9'xy',
    'A'#9'B'#9'C ' + 'DEF G'#9'H'#9 + #9#9'xy',
    '',
    'äää ööö ' + 'ÄÄÄ ÖÖÖ ' + 'ÜÜÜ',
    '',
    '999'
  ]);
  SetSynEditWidth(10);

  SetSynEditWidth(10);
  CheckLines('', 0, [
    'abc def ',
    'ABC DEFG ',
    'XYZ',
    '',
    'A'#9'B'#9'C ',
    'DEF G'#9'H'#9,
    #9#9'xy',
    '',
    'äää ööö ',
    'ÄÄÄ ÖÖÖ ',
    'ÜÜÜ',
    '',
    '999'
  ], True);

  SynEdit.BeginUpdate;
  SynEdit.TestTypeText(1,7, '4 ');
  SynEdit.TestTypeText(1,3, '2 ');
  SynEdit.TestTypeText(1,5, '3 ');
  SynEdit.TestTypeText(1,1, '1 ');
  SynEdit.EndUpdate;

  CheckLines('', 0, [
    '1 abc def ',
    'ABC DEFG ',
    'XYZ',
    '',
    '2 A'#9'B'#9'C ',
    'DEF G'#9'H'#9,
    #9#9'xy',
    '',
    '3 äää ööö ',
    'ÄÄÄ ÖÖÖ ',
    'ÜÜÜ',
    '',
    '4 999'
  ], True);
end;

initialization

  RegisterTest(TTestWordWrap);
  RegisterTest(TTestWordWrapPlugin);
end.


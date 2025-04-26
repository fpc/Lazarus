unit TestLazStorageMemCase1;

{$mode objfpc}{$H+}
{$WARN 3124 off : Inlining disabled}

{ $DEFINE TEST_WITH_SLOW} // If defined, disable heaptrc

interface

uses
  Classes, SysUtils, math, LazListClasses, LazLoggerBase, LazListClassesBase, fpcunit, testutils,
  testregistry;

type

  TIntArray = Array of Integer;

  { TFastTestCase }

  TFastTestCase = class(TTestCase)
  public
    class procedure AssertTrue(const AMessage: string; ACondition: boolean; AErrorAddrs: Pointer = nil); overload;
    class procedure AssertTrue(const AMessage: string; const AFormatArgs: array of const; ACondition: boolean; AErrorAddrs: Pointer = nil); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: integer); overload;
    class procedure AssertEquals(const AMessage: string; const AFormatArgs: array of const; Expected, Actual: integer); overload;
  end;

  { TTestLazMemWrapper }

  generic TTestLazMemWrapper<T> = object
  private
    FTested: T;
    FExpected: Array of Integer;
    FHasMemInit: Boolean;
    function GetItems(AnIndex: Integer): Integer;
    procedure SetItems(AnIndex: Integer; AValue: Integer);
  public
    constructor Create(AnHasMemInit: Boolean);
    destructor destroy;
    function Insert(Avalue: Integer; Caller: TFastTestCase): Integer;
    function Insert(AnIndex: Integer; Avalue: Integer; Caller: TFastTestCase): PInteger;
    function Insert(AnIndex: Integer; const Avalues: TIntArray; Caller: TFastTestCase): PInteger;
    procedure InsertExpected(AnIndex: Integer; Avalue: Integer);
    procedure InsertExpected(AnIndex: Integer; const Avalues: array of Integer);
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer);
    procedure Delete(AIndex, ACount: Integer);
    procedure Clear;
    procedure AssertExp(const AName: String; Caller: TFastTestCase); inline;
    procedure AssertExp(const AName: String; const AFormatArgs: array of const; Caller: TFastTestCase);

    function TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
    function Count: Integer;
    function ItemPointer(AIndex: Integer): PInteger;
    property Items[AnIndex: Integer]: Integer read GetItems write SetItems;
    property Tested: T read FTested;
  end;

  { TTestListCapacityController }

  TTestListCapacityController = object(TLazListAspectCapacityExp0x8000)
  private
    procedure SetGrowStep(AValue: Integer);
    procedure SetShrinkStep(AValue: Integer);
  public
    FGrowStep: Integer;
    FShrinkStep: Integer;
    property GrowStep: Integer write SetGrowStep;
    property ShrinkStep: Integer write SetShrinkStep;
    procedure Init(); inline;
    function GrowCapacity(ARequired, ACurrent: Integer): Integer; inline;
    function ShrinkCapacity({%H-}ARequired, ACurrent: Integer): Integer; inline;
  end;

  { TTestLazShiftBufferListObj }

  TTestLazShiftBufferListObj = object(specialize TGenLazShiftListVarSize<Pointer, specialize TGenListConfigVarSize_3<TLazListAspectMemInitNone, TTestListCapacityController, TLazListAspectRangeNoIndexCheck> >)
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 26 {$ELSE} 17 {$ENDIF};
    TEST_MAX_STEP = MaxInt;
  protected
    property FGrowStep:   Integer read FCapacity.FGrowStep write FCapacity.FGrowStep;
    property FShrinkStep: Integer read FCapacity.FShrinkStep write FCapacity.FShrinkStep;
  public
    constructor Create;
    function TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazShiftBufferListObjMemInit }

  TTestLazShiftBufferListObjMemInit = object(
    specialize TGenLazShiftListVarSize<Pointer, specialize TGenListConfigVarSize_3<TLazListAspectMemInitZero, TTestListCapacityController, TLazListAspectRangeNoIndexCheck> >
  )
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 26 {$ELSE} 17 {$ENDIF};
    TEST_MAX_STEP = MaxInt;
  protected
    property FGrowStep:   Integer read FCapacity.FGrowStep write FCapacity.FGrowStep;
    property FShrinkStep: Integer read FCapacity.FShrinkStep write FCapacity.FShrinkStep;
  public
    constructor Create;
    function TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazGenLazShiftBufferListObj }

  TTestLazGenLazShiftBufferListObj = object(
    specialize TGenLazShiftListFixedType<integer, specialize TGenListConfigFixSize_3<TLazListAspectMemInitNone, TTestListCapacityController, TLazListAspectRangeNoIndexCheck> >
  )
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 26 {$ELSE} 15 {$ENDIF};
    TEST_MAX_STEP = MaxInt;
  protected
    property FGrowStep:   Integer read FCapacity.FGrowStep write FCapacity.FGrowStep;
    property FShrinkStep: Integer read FCapacity.FShrinkStep write FCapacity.FShrinkStep;
  public
    function  TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazRoundBufferListMem }

  TTestLazRoundBufferListMem = object(specialize TGenLazRoundListVarSize<Pointer, specialize TGenListConfigVarSize_3<TLazListAspectMemInitNone, TTestListCapacityController, TLazListAspectRangeNoIndexCheck> >)
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 26 {$ELSE} 17 {$ENDIF};
    TEST_MAX_STEP = MaxInt;
  protected
    property FGrowStep:   Integer read FCapacity.FGrowStep write FCapacity.FGrowStep;
    property FShrinkStep: Integer read FCapacity.FShrinkStep write FCapacity.FShrinkStep;
  public
    constructor Create;
    function  TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazRoundBufferListMemMemInit }

  TTestLazRoundBufferListMemMemInit = object(
    specialize TGenLazRoundListVarSize<Pointer, specialize TGenListConfigVarSize_3<TLazListAspectMemInitZero, TTestListCapacityController, TLazListAspectRangeNoIndexCheck> >
  )
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 26 {$ELSE} 17 {$ENDIF};
    TEST_MAX_STEP = MaxInt;
  protected
    property FGrowStep:   Integer read FCapacity.FGrowStep write FCapacity.FGrowStep;
    property FShrinkStep: Integer read FCapacity.FShrinkStep write FCapacity.FShrinkStep;
  public
    constructor Create;
    function  TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazGenRoundBufferListMem }

  TTestLazGenRoundBufferListMem = object(
    specialize TGenLazRoundListFixedType<Integer, specialize TGenListConfigFixSize_3<TLazListAspectMemInitNone, TTestListCapacityController, TLazListAspectRangeNoIndexCheck> >
  )
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 26 {$ELSE} 15 {$ENDIF};
    TEST_MAX_STEP = MaxInt;
  protected
    property FGrowStep:   Integer read FCapacity.FGrowStep write FCapacity.FGrowStep;
    property FShrinkStep: Integer read FCapacity.FShrinkStep write FCapacity.FShrinkStep;
  public
    function  TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;


  { TTestLazPagedListMem }

  generic TTestLazPagedListMemBase<PageConf: TLazListPageConfig> = object(
    specialize TGenLazPagedListVarSize<Pointer, specialize TGenListConfigVarSizeNoIdx_2<     TLazListAspectMemInitNone, TTestListCapacityController>, PageConf>
  )
  private
    procedure SetFGrowStep(AValue: Integer);
    procedure SetFShrinkStep(AValue: Integer);
  protected
    property FGrowStep:   Integer write SetFGrowStep;
    property FShrinkStep: Integer write SetFShrinkStep;
  public
    function  TestInsertRows(AIndex, ACount: Integer): PInteger;
    procedure TestDeleteRows(AIndex, ACount: Integer);
  end;

  TTestLazPagedListMem = specialize TTestLazPagedListMemBase<TLazListPageConfig>;

  { TTestLazPagedListMem0 }

  TTestLazPagedListMem0 = object(TTestLazPagedListMem)
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 18 {$ELSE} 11 {$ENDIF};
    TEST_MAX_STEP = 5;
  public
    constructor Create;
  end;

  { TTestLazPagedListMem1 }

  TTestLazPagedListMem1 = object(TTestLazPagedListMem)
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 14*2 {$ELSE} 19 {$ENDIF};
    TEST_MAX_STEP = {$IFDEF TEST_WITH_SLOW} 5 {$ELSE} 4 {$ENDIF};
  public
    constructor Create;
  end;

  { TTestLazPagedListMem2 }

  TTestLazPagedListMem2 = object(TTestLazPagedListMem)
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 8*4+2 {$ELSE} 19 {$ENDIF};
    TEST_MAX_STEP = {$IFDEF TEST_WITH_SLOW} 5 {$ELSE} 2 {$ENDIF};
  public
    constructor Create;
  end;

  { TTestLazPagedListMem3 }

  TTestLazPagedListMem3 = object(TTestLazPagedListMem)
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 7*8+4 {$ELSE} 25 {$ENDIF};
    TEST_MAX_STEP = {$IFDEF TEST_WITH_SLOW} 3 {$ELSE} 1 {$ENDIF};
  public
    constructor Create;
  end;

  { TTestLazPagedListMem4 }

  TTestLazPagedListMem4 = object(TTestLazPagedListMem)
  protected const
    TEST_MAX_CNT = {$IFDEF TEST_WITH_SLOW} 6*16+2 {$ELSE} 50 {$ENDIF};
    TEST_MAX_STEP = 1;
  public
    constructor Create;
  end;

  TTestListClassesPageSizeExpConst_3 = object(__TLazListAspectPageSizeExpConstBase)
  public const
    FPageSizeExp = 3;
    FPageSizeMask = Cardinal(not(qword(-1) << FPageSizeExp));
  end;

  { TTestLazPagedListMemConstPg_3 }

  TTestLazPagedListMemConstPg_3 = object(specialize TTestLazPagedListMemBase<
    specialize TGenListPageConfigConst<TTestListClassesPageSizeExpConst_3>
  >)
  protected const
    TEST_MAX_CNT = 14;
    TEST_MAX_STEP = 2;
  public
    constructor Create;
  end;

  { TTestLazPagedListMemInit }

  TTestLazPagedListMemInit = object(
    specialize TGenLazPagedListVarSize<Pointer, specialize TGenListConfigVarSizeNoIdx_2<TLazListAspectMemInitZero, TLazListAspectCapacityExp0x8000>, TLazListPageConfig>
  )
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    procedure Create(APageSizeExp: Integer; AnItemSize: Integer);
    function  TestInsertRows(AIndex, ACount: Integer): PInteger;
    procedure TestDeleteRows(AIndex, ACount: Integer);
  end;

  { TTestLazPagedListMemInit0 }

  TTestLazPagedListMemInit0 = object(TTestLazPagedListMemInit)
  protected const
    TEST_MAX_CNT = 11;
    TEST_MAX_STEP = 4;
  public
    constructor Create;
  end;

  { TTestLazPagedListMemInit1 }

  TTestLazPagedListMemInit1 = object(TTestLazPagedListMemInit)
  protected const
    TEST_MAX_CNT = 19;
    TEST_MAX_STEP = 3;
  public
    constructor Create;
  end;

  { TTestLazPagedListMemInit2 }

  TTestLazPagedListMemInit2 = object(TTestLazPagedListMemInit)
  protected const
    TEST_MAX_CNT = 19;
    TEST_MAX_STEP = 2;
  public
    constructor Create;
  end;

  { TTestLazPagedListMemInit3 }

  TTestLazPagedListMemInit3 = object(TTestLazPagedListMemInit)
  protected const
    TEST_MAX_CNT = 25;
    TEST_MAX_STEP = 1;
  public
    constructor Create;
  end;

  { TTestLazPagedListMemInit4 }

  TTestLazPagedListMemInit4 = object(TTestLazPagedListMemInit)
  protected const
    TEST_MAX_CNT = 35;
    TEST_MAX_STEP = 0;
  public
    constructor Create;
  end;

  TTestProc = procedure(const name: string) of object;

  { TTestRunnerList }

  generic TTestRunnerList<TListTypeX> = class(TFastTestCase)
  protected type TListType = specialize TTestLazMemWrapper<TListTypeX>;
  protected
    Caller: TFastTestCase;
    GrowStep: Integer;
    ShrinkStep: Integer;
    function HasMemInit: boolean; virtual;
    procedure TestNew(const {%H-}name: string);
    procedure TestMove(const {%H-}name: string);
    procedure RunIndex(const {%H-}name: string);
    procedure TestSequence(const name: string; const a: Array of Integer); // Old test from a previous version
    procedure TestSequenceEx(const n: string; a: Array of Integer);

    procedure RunSeq(const {%H-}name: string);
    procedure RunSeqEx(const {%H-}name: string);
    procedure RunTests(AProc: TTestProc);
  published
    procedure TestCreate;
    procedure TestMove;
    procedure TestShrink;
    procedure TestIndex;
    procedure TestSeq;
    procedure TestSeqEx;
  end;

  { TTestRunnerListMemInit }

  generic TTestRunnerListMemInit<TListTypeX> = class(specialize TTestRunnerList<TListTypeX>)
  protected
    function HasMemInit: boolean; override;
  end;

  { TTestRunnerListAnsiString }

  generic TTestRunnerListAnsiString<TListTypeX> = class(TFastTestCase)
  protected
    // MUST be "var param" to avoid ref-count changes due to the call itself
    procedure AssertStringIsUnique(var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
    procedure AssertStringIsNotUnique(var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
    procedure GetNewString(var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}

    function GetUsedHeap: ptruint;
    procedure DoTestInsert;
    procedure DoTestDelete;
    procedure DoTestAssign;
  published
    procedure TestInsert;
    procedure TestDelete;
    procedure TestAssign;
  end;


  TTestListMem = specialize TTestRunnerList<TTestLazShiftBufferListObj>;
  TTestListMemMemInit = specialize TTestRunnerListMemInit<TTestLazShiftBufferListObjMemInit>;
  TTestListMemSpecialized = specialize TTestRunnerList<TTestLazGenLazShiftBufferListObj>;

  TTestListRoundMem = specialize TTestRunnerList<TTestLazRoundBufferListMem>;
  TTestListRoundMemInit = specialize TTestRunnerListMemInit<TTestLazRoundBufferListMemMemInit>;
  TTestListRoundMemSpecialized = specialize TTestRunnerList<TTestLazGenRoundBufferListMem>;

  TTestListPagedMem0 = specialize TTestRunnerList<TTestLazPagedListMem0>;
  TTestListPagedMem1 = specialize TTestRunnerList<TTestLazPagedListMem1>;
  TTestListPagedMem2 = specialize TTestRunnerList<TTestLazPagedListMem2>;
  TTestListPagedMem3 = specialize TTestRunnerList<TTestLazPagedListMem3>;
  TTestListPagedMem4 = specialize TTestRunnerList<TTestLazPagedListMem4>;
  TTestListPagedMemConst3 = specialize TTestRunnerList<TTestLazPagedListMemConstPg_3>;
  TTestListPagedMemInit0 = specialize TTestRunnerListMemInit<TTestLazPagedListMemInit0>;
  TTestListPagedMemInit1 = specialize TTestRunnerListMemInit<TTestLazPagedListMemInit1>;
  TTestListPagedMemInit2 = specialize TTestRunnerListMemInit<TTestLazPagedListMemInit2>;
  TTestListPagedMemInit3 = specialize TTestRunnerListMemInit<TTestLazPagedListMemInit3>;
  TTestListPagedMemInit4 = specialize TTestRunnerListMemInit<TTestLazPagedListMemInit4>;


  TTestShiftBufferAnsiString = specialize TGenLazShiftListFixedType<
    AnsiString, specialize TGenListConfigFixSize_3<specialize TLazListAspectMemInitManagedRefCnt<AnsiString>,
    TLazListAspectCapacityExp0x8000, TLazListAspectRangeNoIndexCheck> >;

  TTestRoundBufferAnsiString = specialize TGenLazRoundListFixedType<
    AnsiString, specialize TGenListConfigFixSize_3<specialize TLazListAspectMemInitManagedRefCnt<AnsiString>,
    TLazListAspectCapacityExp0x8000, TLazListAspectRangeNoIndexCheck> >;

  TTestPagedBufferAnsiString8 = object(specialize TGenLazPagedListFixedType<
    AnsiString, specialize TGenListConfigFixSizeNoIdx_2<specialize TLazListAspectMemInitManagedRefCnt<AnsiString>,
    TLazListAspectCapacityExp0x8000>, TLazListPageConfig>)
  public
    procedure Create; overload; // expsize = 3
  end;

  TTestPagedBufferAnsiString4 = object(TTestPagedBufferAnsiString8)
  public
    procedure Create; overload; // expsize = 2
  end;


  TTestListShiftMemAnsiString = specialize TTestRunnerListAnsiString<TTestShiftBufferAnsiString>;
  TTestListRoundMemAnsiString = specialize TTestRunnerListAnsiString<TTestRoundBufferAnsiString>;
  TTestListPagedMemAnsiString4 = specialize TTestRunnerListAnsiString<TTestPagedBufferAnsiString4>;
  TTestListPagedMemAnsiString8 = specialize TTestRunnerListAnsiString<TTestPagedBufferAnsiString8>;


function CreateArray(ALow,ACount: integer): TIntArray;
function JoinArrays(const a,b: array of integer): TIntArray;
function JoinArrays(const a,b,c: array of integer): TIntArray;

implementation

function CreateArray(ALow,ACount: integer): TIntArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := i+ALow;
end;

function JoinArrays(const a,b: array of integer): TIntArray;
var
  i,j: Integer;
begin
  SetLength(Result{%H-}, length(a)+Length(b));
  if Length(a) > 0 then
    move(a[0], Result[0], Length(a) * SizeOf(a[0]));
  if Length(b) > 0 then
    move(b[0], Result[Length(a)], Length(b) * SizeOf(b[0]));
end;

function JoinArrays(const a,b,c: array of integer): TIntArray;
var
  i,j: Integer;
begin
  SetLength(Result{%H-}, length(a)+Length(b)+Length(c));
  if Length(a) > 0 then
    move(a[0], Result[0], Length(a) * SizeOf(a[0]));
  if Length(b) > 0 then
    move(b[0], Result[Length(a)], Length(b) * SizeOf(b[0]));
  if Length(c) > 0 then
    move(c[0], Result[Length(a)+Length(b)], Length(c) * SizeOf(c[0]));
end;

{ TFastTestCase }

class procedure TFastTestCase.AssertTrue(const AMessage: string; ACondition: boolean;
  AErrorAddrs: Pointer);
begin
  if ACondition then exit;
  inherited AssertTrue(AMessage, ACondition, AErrorAddrs);
end;

class procedure TFastTestCase.AssertTrue(const AMessage: string;
  const AFormatArgs: array of const; ACondition: boolean; AErrorAddrs: Pointer);
begin
  if ACondition then exit;
  inherited AssertTrue(Format(AMessage, AFormatArgs), ACondition);
end;

class procedure TFastTestCase.AssertEquals(const AMessage: string; Expected, Actual: integer);
begin
  if Expected = Actual then exit;
  inherited AssertEquals(AMessage, Expected, Actual);
end;

class procedure TFastTestCase.AssertEquals(const AMessage: string;
  const AFormatArgs: array of const; Expected, Actual: integer);
begin
  if Expected = Actual then exit;
  inherited AssertEquals(Format(AMessage, AFormatArgs), Expected, Actual);
end;

{ TTestLazMemWrapper }

function TTestLazMemWrapper.GetItems(AnIndex: Integer): Integer;
begin
  Result := PInteger(FTested.ItemPointer[AnIndex])^;
end;

procedure TTestLazMemWrapper.SetItems(AnIndex: Integer; AValue: Integer);
begin
  PInteger(FTested.ItemPointer[AnIndex])^ := AValue;
end;

constructor TTestLazMemWrapper.Create(AnHasMemInit: Boolean);
begin
  FHasMemInit := AnHasMemInit;
  FTested.create;
end;

destructor TTestLazMemWrapper.destroy;
begin
  FTested.destroy;
end;

function TTestLazMemWrapper.Insert(Avalue: Integer; Caller: TFastTestCase): Integer;
begin
  Result := 0;
  while (Result < FTested.Count) do begin
    if (Items[Result] > Avalue) then break;
    inc(Result);
  end;
  FTested.TestInsertRows(Result, 1);
  if FHasMemInit then
    Caller.AssertEquals('insert-mem-init %d', [Result], 0, Items[Result]);
  Items[Result] := Avalue;
end;

function TTestLazMemWrapper.Insert(AnIndex: Integer; Avalue: Integer; Caller: TFastTestCase): PInteger;
begin
  Result := Insert(AnIndex, CreateArray(Avalue, 1), Caller);
end;

function TTestLazMemWrapper.Insert(AnIndex: Integer; const Avalues: TIntArray;
  Caller: TFastTestCase): PInteger;
var
  i: Integer;
begin
  //debugln(['TTestLazMemWrapper.Insert ',AnIndex,'  ',Length(Avalues)]);
  Result := FTested.TestInsertRows(AnIndex, length(Avalues));
  for i := 0 to high(Avalues) do begin
    if FHasMemInit then
      Caller.AssertEquals('insert-mem-init %d', [i+AnIndex], 0, Items[i+AnIndex]);
    Items[i+AnIndex] := Avalues[i];
  end;
  if FExpected = nil then
    //FExpected := JoinArrays(Avalues, [])
    FExpected := Avalues
  else if AnIndex = 0 then
    FExpected := JoinArrays(Avalues, FExpected)
  else if AnIndex = Length(FExpected) then
    FExpected := JoinArrays(FExpected, Avalues)
  else {$PUSH}{$R-}
    FExpected := JoinArrays(FExpected[0..(AnIndex-1)], Avalues, FExpected[AnIndex..high(FExpected)]);
    {$POP}
end;

procedure TTestLazMemWrapper.InsertExpected(AnIndex: Integer; Avalue: Integer);
begin
  InsertExpected(AnIndex, CreateArray(Avalue, 1));
end;

procedure TTestLazMemWrapper.InsertExpected(AnIndex: Integer; const Avalues: array of Integer);
begin
  if FExpected = nil then
    FExpected := JoinArrays(Avalues, [])
  else {$PUSH}{$R-}
    FExpected := JoinArrays(FExpected[0..(AnIndex-1)], Avalues, FExpected[AnIndex..high(FExpected)]);
    {$POP}
end;

procedure TTestLazMemWrapper.MoveRows(AFromIndex, AToIndex, ACount: Integer);
var
  i, x: Integer;
begin
  x := -99;
  if FHasMemInit then x := 0;
  FTested.MoveRows(AFromIndex, AToIndex, ACount);
  move(FExpected[AFromIndex], FExpected[AToIndex], ACount * SizeOf(FExpected[0]));
  if AFromIndex < AToIndex then
    for i := AFromIndex to min(AFromIndex + ACount, AToIndex)-1 do begin
      if not FHasMemInit then
        Items[i] := x;
      FExpected[i] := x;
    end
  else
    for i := max(AFromIndex, AToIndex+ACount) to AFromIndex+ACount-1 do begin
      if not FHasMemInit then
        Items[i] := x;
      FExpected[i] := x;
    end;
end;

procedure TTestLazMemWrapper.Delete(AIndex, ACount: Integer);
begin
  //debugln(['TTestLazMemWrapper.Delete ',AIndex,'  ',ACount]);
  TestDeleteRows(AIndex, ACount);
  {$PUSH}{$R-}
  FExpected := JoinArrays(FExpected[0..AIndex-1], FExpected[(AIndex+ACount)..High(FExpected)]);
  {$POP}
end;

procedure TTestLazMemWrapper.Clear;
begin
  FTested.TestDeleteRows(0, FTested.Count);
  FTested.Capacity := 0;
  FExpected := nil;
  Assert(0 = FTested.Count);
  Assert(0 = FTested.Capacity);
end;

procedure TTestLazMemWrapper.AssertExp(const AName: String; Caller: TFastTestCase);
begin
  AssertExp(AName, [], Caller);
end;

procedure TTestLazMemWrapper.AssertExp(const AName: String; const AFormatArgs: array of const;
  Caller: TFastTestCase);
var
  i: Integer;
  s: String;
begin
  try
    if Length(FExpected) <> Count then
      Caller.AssertEquals(' %s Expect Count %d, %d', [Format(AName, AFormatArgs), Length(FExpected), Count], Length(FExpected), Count);
    for i := 0 to FTested.Count-1 do
      if FExpected[i] <> Items[i] then
        Caller.AssertEquals('%s Test %d / %d, %d', [Format(AName, AFormatArgs), i, FExpected[i], Items[i]], FExpected[i], Items[i]);
  except
    on e: Exception do begin
      FTested.DebugDump;
      dbgout(['EXPECTED' +
      ' ', length(FExpected), ': ']);
      s :='';
      for i := 0 to length(FExpected) - 1 do s := s + dbgs(FExpected[i])+ ', ';
      debugln(s);

      raise e;
    end;
  end;
end;

function TTestLazMemWrapper.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := FTested.TestInsertRows(AIndex, ACount);
end;

procedure TTestLazMemWrapper.TestDeleteRows(AIndex, ACount: Integer);
begin
  FTested.TestDeleteRows(AIndex, ACount);
end;

function TTestLazMemWrapper.Count: Integer;
begin
  result := FTested.Count;
end;

function TTestLazMemWrapper.ItemPointer(AIndex: Integer): PInteger;
begin
  Result := PInteger(FTested.ItemPointer[AIndex]);
end;

{ TTestListCapacityController }

procedure TTestListCapacityController.SetGrowStep(AValue: Integer);
begin
  FGrowStep := AValue;
end;

procedure TTestListCapacityController.SetShrinkStep(AValue: Integer);
begin
  FShrinkStep := AValue;
end;

procedure TTestListCapacityController.Init();
begin
  FGrowStep := 0;
  FShrinkStep := 0;
  inherited Init;
end;

function TTestListCapacityController.GrowCapacity(ARequired, ACurrent: Integer): Integer;
begin
  if FGrowStep < 0 then exit(inherited GrowCapacity(ARequired, ACurrent));

  Result := ARequired + FGrowStep;
end;

function TTestListCapacityController.ShrinkCapacity(ARequired, ACurrent: Integer): Integer;
begin
  if FGrowStep < 0 then exit(inherited ShrinkCapacity(ARequired, ACurrent));

  if ARequired = 0 then exit(0);
  if FShrinkStep < 0 then exit(-1);
  if ACurrent - FShrinkStep >  ARequired then
    Result := ARequired
  else
    Result := -1;
end;

{ TTestLazShiftBufferListObj }

constructor TTestLazShiftBufferListObj.Create;
begin
  inherited Create(SizeOf(Integer));
end;

function TTestLazShiftBufferListObj.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRows(AIndex, ACount);
end;

procedure TTestLazShiftBufferListObj.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRows(AIndex, ACount);
end;

{ TTestLazShiftBufferListObjMemInit }

constructor TTestLazShiftBufferListObjMemInit.Create;
begin
  inherited Create(SizeOf(integer));
end;

function TTestLazShiftBufferListObjMemInit.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRows(AIndex, ACount);
end;

procedure TTestLazShiftBufferListObjMemInit.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRows(AIndex, ACount);
end;

{ TTestLazGenLazShiftBufferListObj }

function TTestLazGenLazShiftBufferListObj.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRows(AIndex, ACount);
end;

procedure TTestLazGenLazShiftBufferListObj.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRows(AIndex, ACount);
end;

{ TTestLazRoundBufferListMem }

constructor TTestLazRoundBufferListMem.Create;
begin
  inherited Create(SizeOf(Integer));
end;

function TTestLazRoundBufferListMem.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRows(AIndex, ACount);
end;

procedure TTestLazRoundBufferListMem.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRows(AIndex, ACount);
end;

{ TTestLazRoundBufferListMemMemInit }

constructor TTestLazRoundBufferListMemMemInit.Create;
begin
  inherited Create(SizeOf(integer));
end;

function TTestLazRoundBufferListMemMemInit.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRows(AIndex, ACount);
end;

procedure TTestLazRoundBufferListMemMemInit.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRows(AIndex, ACount);
end;

{ TTestLazGenRoundBufferListMem }

function TTestLazGenRoundBufferListMem.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRows(AIndex, ACount);
end;

procedure TTestLazGenRoundBufferListMem.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRows(AIndex, ACount);
end;

{ TTestLazPagedListMemBase }

procedure TTestLazPagedListMemBase.SetFGrowStep(AValue: Integer);
begin
  FCapacity.GrowStep := AValue;
end;

procedure TTestLazPagedListMemBase.SetFShrinkStep(AValue: Integer);
begin
  FCapacity.ShrinkStep := AValue;
end;

function TTestLazPagedListMemBase.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  InsertRows(AIndex, ACount);
  Result := PInteger(ItemPointer[AIndex]);
end;

procedure TTestLazPagedListMemBase.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRows(AIndex, ACount);
end;

{ TTestLazPagedListMem0 }

constructor TTestLazPagedListMem0.Create;
begin
  inherited Create(0, SizeOf(Integer));
end;

{ TTestLazPagedListMem1 }

constructor TTestLazPagedListMem1.Create;
begin
  inherited Create(1, SizeOf(Integer));
end;

{ TTestLazPagedListMem2 }

constructor TTestLazPagedListMem2.Create;
begin
  inherited Create(2, SizeOf(Integer));
end;

{ TTestLazPagedListMem3 }

constructor TTestLazPagedListMem3.Create;
begin
  inherited Create(3, SizeOf(Integer));
end;

{ TTestLazPagedListMem4 }

constructor TTestLazPagedListMem4.Create;
begin
  inherited Create(4, SizeOf(Integer));
end;

{ TTestLazPagedListMemConstPg_3 }

constructor TTestLazPagedListMemConstPg_3.Create;
begin
  inherited Create(SizeOf(Integer));
end;

{ TTestLazPagedListMemInit }

function TTestLazPagedListMemInit.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazShiftBufferListObj.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazPagedListMemInit.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if Capacity - FShrinkStep >  ARequired then
    Result := ARequired
  else
    Result := -1;
end;

procedure TTestLazPagedListMemInit.Create(APageSizeExp: Integer; AnItemSize: Integer);
begin
  inherited Create(APageSizeExp, AnItemSize);
end;

function TTestLazPagedListMemInit.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  inherited InsertRows(AIndex, ACount);
  Result := PInteger(ItemPointer[AIndex]);
end;

procedure TTestLazPagedListMemInit.TestDeleteRows(AIndex, ACount: Integer);
begin
  inherited DeleteRows(AIndex, ACount);
end;

{ TTestLazPagedListMemInit0 }

constructor TTestLazPagedListMemInit0.Create;
begin
  inherited Create(0, SizeOf(Integer));
end;

{ TTestLazPagedListMemInit1 }

constructor TTestLazPagedListMemInit1.Create;
begin
  inherited Create(1, SizeOf(Integer));
end;

{ TTestLazPagedListMemInit2 }

constructor TTestLazPagedListMemInit2.Create;
begin
  inherited Create(2, SizeOf(Integer));
end;

{ TTestLazPagedListMemInit3 }

constructor TTestLazPagedListMemInit3.Create;
begin
  inherited Create(3, SizeOf(Integer));
end;

{ TTestLazPagedListMemInit4 }

constructor TTestLazPagedListMemInit4.Create;
begin
  inherited Create(4, SizeOf(Integer));
end;

{ TTestRunnerList }

function TTestRunnerList.HasMemInit: boolean;
begin
  Result := False;
end;

procedure TTestRunnerList.TestNew(const name: string);
var
  ListWrapper, ListWrapper2: TListType;
  i, j, k: Integer;
  p: PInteger;
begin
  ListWrapper.Create(HasMemInit);
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;
  ListWrapper2.Create(HasMemInit);
  ListWrapper2.FTested.FGrowStep := GrowStep;
  ListWrapper2.FTested.FShrinkStep := ShrinkStep;

  for i := 1 to 25 do begin
    p := ListWrapper.Insert(0, CreateArray(1, i), Self);
    Caller.AssertEquals('', {%H-}PtrInt(ListWrapper.ItemPointer(0)), {%H-}PtrInt(p));
    ListWrapper.AssertExp('Insert %d at 0', [i], Caller);

    for j := 0 to i do
    for k := 1 to 25 do begin
      p := ListWrapper.Insert(j, CreateArray(100*k, k), Self);
      Caller.AssertEquals('', {%H-}PtrInt(ListWrapper.ItemPointer(j)), {%H-}PtrInt(p));
      ListWrapper.AssertExp('Insert %d at %d', [k, j], Caller);

      ListWrapper.Delete(j, k);
      ListWrapper.AssertExp('Delete %d at %d', [k, j], Caller);

      Caller.AssertEquals('', i, ListWrapper.Count);

      // start form empty, may have different free-at-start
      ListWrapper2.Clear;
      p := ListWrapper2.Insert(0, CreateArray(k, i), Self);
      ListWrapper2.AssertExp('Insert %d at 0', [i], Caller);

      p := ListWrapper2.Insert(j, CreateArray(100*k, k), Self);
      Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer(j)), {%H-}PtrInt(p));
      ListWrapper2.AssertExp('ListWrapper2 Insert %d at %d', [k, j], Caller);
      ListWrapper2.Delete(j, k);
      ListWrapper2.AssertExp('ListWrapper2 Delete %d at %d', [k, j], Caller);
      Caller.AssertEquals('', i, ListWrapper.Count);

      if  byte(k) in [1,9,10,11,20] then begin
        // test with space at start
        ListWrapper2.Clear;
        p := ListWrapper2.Insert(0, CreateArray(k, i+10), Self);
        ListWrapper2.Delete(0, 10);
        ListWrapper2.AssertExp('Insert %d at 0', [i], Caller);

        p := ListWrapper2.Insert(j, CreateArray(100*k, k), Self);
        Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer(j)), {%H-}PtrInt(p));
        ListWrapper2.AssertExp('ListWrapper2 Insert %d at %d', [k, j], Caller);
        ListWrapper2.Delete(j, k);
        ListWrapper2.AssertExp('ListWrapper2 Delete %d at %d', [k, j], Caller);
        Caller.AssertEquals('', i, ListWrapper.Count);

        // test with space at end
        ListWrapper2.Clear;
        p := ListWrapper2.Insert(0, CreateArray(k, i+10), Self);
        ListWrapper2.Delete(i, 10);
        ListWrapper2.AssertExp('Insert %d at 0', [i], Caller);

        p := ListWrapper2.Insert(j, CreateArray(100*k, k), Self);
        Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer(j)), {%H-}PtrInt(p));
        ListWrapper2.AssertExp('ListWrapper2 Insert %d at %d', [k, j], Caller);
        ListWrapper2.Delete(j, k);
        ListWrapper2.AssertExp('ListWrapper2 Delete %d at %d', [k, j], Caller);
        Caller.AssertEquals('', i, ListWrapper.Count);

      end;

    end;


    for k := 1 to (i div 2) - 1 do begin
      ListWrapper2.Clear;
      p := ListWrapper2.Insert(0, CreateArray(k, i), Self);
      ListWrapper2.Delete(i-k, k);
      ListWrapper2.Delete(0, k);
      ListWrapper2.AssertExp('ListWrapper2 Delete %d at %d', [i, k], Caller);

      p := ListWrapper2.Insert((i-(2*k)) div 2, CreateArray(90*k, 2*k), Self);
      Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer((i-(2*k)) div 2)), {%H-}PtrInt(p));
      ListWrapper2.AssertExp('ListWrapper2 Delete %d at %d', [i, k], Caller);
    end;


    for j := 0 to i-1 do
    for k := 1 to i-j do begin
      ListWrapper2.Clear;
      p := ListWrapper2.Insert(0, CreateArray(1, i), Self);
      ListWrapper2.Delete(j, k);
      ListWrapper2.AssertExp('ListWrapper2 Delete(2) %d at %d', [k, j], Caller);
    end;

    ListWrapper.Clear;
  end;

  ListWrapper.destroy;
  ListWrapper2.destroy;
end;

procedure TTestRunnerList.TestMove(const name: string);
var
  ListWrapper: TListType;
  InitCnt, FromPos, ToPos, MoveLen, DelCnt, InsCnt: Integer;
begin
  ListWrapper.Create(HasMemInit);
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;

  for InitCnt := 2 to ListWrapper.FTested.TEST_MAX_CNT do
    for FromPos := 0 to InitCnt-1 do // from
    for ToPos := 0 to InitCnt-1 do // to
    for MoveLen := 1 to InitCnt-Max(FromPos,ToPos)-1 do // len
    begin
      if FromPos=ToPos then continue;
      ListWrapper.Insert(0, CreateArray(1, InitCnt), Self);
      //ListWrapper.AssertExp('Insert %d at 0', [InitCnt], Caller);

      ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
      ListWrapper.AssertExp('MOV %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen], Caller);
      ListWrapper.Clear;


      if (FromPos < min(8, ListWrapper.FTested.TEST_MAX_CNT div 4)) or
         (ToPos < min(8, ListWrapper.FTested.TEST_MAX_CNT div 4)) or
         (FromPos > ListWrapper.FTested.TEST_MAX_CNT - min(5, ListWrapper.FTested.TEST_MAX_CNT div 6)) or
         (ToPos > ListWrapper.FTested.TEST_MAX_CNT - min(5, ListWrapper.FTested.TEST_MAX_CNT div 6))
      then begin
        // vary the GAP at start
        for DelCnt := 1 to min(7, ListWrapper.FTested.TEST_MAX_CNT div 4) do begin
          ListWrapper.Create(HasMemInit);
          ListWrapper.FTested.FGrowStep := GrowStep;
          ListWrapper.FTested.FShrinkStep := ShrinkStep;
          ListWrapper.Insert(0, CreateArray(1, InitCnt+DelCnt), Self);
          ListWrapper.Delete(0, DelCnt);
          ListWrapper.AssertExp('Insert %d at 0', [InitCnt], Caller);

          ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
          ListWrapper.AssertExp('MOV %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen], Caller);
          ListWrapper.Clear;
        end;

        for InsCnt := 1 to min(4, InitCnt-1) do begin
          ListWrapper.Create(HasMemInit);
          ListWrapper.FTested.FGrowStep := GrowStep;
          ListWrapper.FTested.FShrinkStep := ShrinkStep;
          ListWrapper.Insert(0, CreateArray(1, InitCnt-InsCnt), Self);
          ListWrapper.Insert(0, CreateArray(100, InsCnt), Self);
          ListWrapper.AssertExp('Insert %d at 0', [InitCnt], Caller);

          ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
          ListWrapper.AssertExp('MOV %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen], Caller);
          ListWrapper.Clear;
        end;
      end;

    end;
  ListWrapper.destroy;


  for InitCnt := 3 to 10 do
    for FromPos := 0 to InitCnt-1 do // from
    for ToPos := 0 to InitCnt-1 do // to
    for DelCnt := 1 to min(InitCnt-2, 5) do // del
    for MoveLen := 1 to InitCnt-Max(FromPos,ToPos)-1 do // len
    begin
      if FromPos=ToPos then continue;
      ListWrapper.Create(HasMemInit);
      ListWrapper.FTested.FGrowStep := GrowStep;
      ListWrapper.FTested.FShrinkStep := ShrinkStep;

      ListWrapper.Insert(0, CreateArray(1, InitCnt), Self);
      ListWrapper.Delete(0, DelCnt);
      ListWrapper.Insert(InitCnt-DelCnt, CreateArray(100, DelCnt), Self); // fill roundbuffer
      ListWrapper.AssertExp('Insert %d at 0', [InitCnt], Caller);

      ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
      ListWrapper.AssertExp('MOV3 %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen], Caller);
      ListWrapper.Clear;
      ListWrapper.destroy;
    end;

end;

procedure TTestRunnerList.RunIndex(const name: string);
var
  ListWrapper: TListType;
  InitCnt, DelCnt, InsCnt, i, val: Integer;
begin
  ListWrapper.Create(HasMemInit);
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;

  for InitCnt := 1 to ListWrapper.FTested.TEST_MAX_CNT do
  for DelCnt := 0 to InitCnt-1 do
  begin
    //debugln(['>>>>>>>>>> TestMoveA ',InitCnt,',',FromPos,',',ToPos,',',MoveLen]);
    ListWrapper.Clear;
    ListWrapper.Insert(0, CreateArray(1, InitCnt), Self);
    if DelCnt > 0 then
      ListWrapper.Delete(0, DelCnt);

    for i := 0 to ListWrapper.Count - 1 do begin
      val := i+1+DelCnt;
      AssertEquals('Index (del) of %d', [i+1+DelCnt], i, ListWrapper.FTested.IndexOf(@val));
    end;

    for InsCnt := 1 to min({$IFDEF TEST_WITH_SLOW} 50 {$ELSE} 30 {$ENDIF}, ListWrapper.FTested.TEST_MAX_CNT) - InitCnt do begin
      i := ListWrapper.Count;
      ListWrapper.Insert(i, CreateArray(i+1+DelCnt, InsCnt), Self);

      for i := 0 to ListWrapper.Count - 1 do begin
        val := i+1+DelCnt;
        AssertEquals('Index (ins) of %d', [i+1+DelCnt], i, ListWrapper.FTested.IndexOf(@val));
      end;
    end;
  end;
  ListWrapper.destroy;
end;

procedure TTestRunnerList.TestSequence(const name: string; const a: array of Integer);
var
  ListWrapper: TListType;
  i, j, k, n, m, o: Integer;
begin
  ListWrapper.Create(HasMemInit);
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;

  for i := 0 to high(a) do begin
    ListWrapper.Insert(a[i], Self);
    Caller.AssertTrue('%s Test Cnt %d %d ', [name, i, ListWrapper.Count], ListWrapper.Count = i+1);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d / %d, %d', [name, i, j, ListWrapper.Items[j], ListWrapper.Items[j-1]], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(ListWrapper.count-1, 1);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := 0 to high(a) do begin
    k := ListWrapper.Insert(a[i], Self);
    Caller.AssertEquals('%s Test %d %d', [name, i, j],a[i], ListWrapper.Items[k]);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;
  while ListWrapper.count> 1 do begin
    ListWrapper.TestDeleteRows(ListWrapper.count-2, 2);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := 0 to high(a) do begin
    ListWrapper.Insert(a[i], Self);
  end;
  for j := 1 to ListWrapper.Count-1 do
    Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(0, 1);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := high(a) downto 0 do begin
    k := ListWrapper.Insert(a[i], Self);
    Caller.AssertEquals('%s Test idx %d %d / %d %d', [name, i, j, k, ListWrapper.Items[k]],a[i], ListWrapper.Items[k]);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d /  / %d %d', [name, i, j, ListWrapper.Items[j], ListWrapper.Items[j-1]], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(0, Min(ListWrapper.count, 2));
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := high(a) downto 0 do begin
    k := ListWrapper.Insert(a[i], Self);
  end;
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(ListWrapper.count div 2, 1);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;


  for m := 0 to length(a)-1 do begin
    for n := 0 to m do begin
      ListWrapper.Clear;
      for i := 0 to m do begin
        k := ListWrapper.Insert(a[i], Self);
        Caller.AssertEquals('%s Test %d %d', [name, n, i],a[i], ListWrapper.Items[k]);
      end;
      for j := 1 to ListWrapper.Count-1 do
        Caller.AssertTrue('%s Test %d %d', [name, n, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
      k := ListWrapper.Items[n];
      ListWrapper.TestDeleteRows(n, 1);
      for j := 1 to ListWrapper.Count-1 do
        Caller.AssertTrue('%s Test %d %d %d %d', [name, n, j, ListWrapper.Items[j], ListWrapper.Items[j-1]], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
      for j := 0 to ListWrapper.Count-1 do
        Caller.AssertTrue('%s Test %d %d - idx %d <> %d', [name, n, j, k, ListWrapper.Items[j]], ListWrapper.Items[j] <> k);
      while ListWrapper.count > 1 do begin
        o := Max(0,Min(ListWrapper.count-2, n));
        k := ListWrapper.Items[o];
        ListWrapper.TestDeleteRows(o, 2);
        for j := 1 to ListWrapper.Count-1 do begin
          Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] > ListWrapper.Items[j-1]);
          Caller.AssertTrue('%s Test %d %d', [name, i, j], ListWrapper.Items[j] <> k);
        end;
      end;

    end;
  end;

  ListWrapper.Destroy;
end;

procedure TTestRunnerList.TestSequenceEx(const n: string; a: array of Integer);
var
  i, j: Integer;
  b: Array of Integer;
begin
  for i := 1 to length(a) do begin
    TestSequence(n+IntToStr(i),a);
    j := a[0];
    if Length(a) > 1 then
      move(a[1],a[0],(Length(a)-1)*SizeOf(a[0]));
    a[high(a)] := j;
  end;

  b := nil;
  SetLength(b, Length(a));
  for i := 0 to length(a)-1 do
    b[i] := a[high(a)-i];

  for i := 1 to length(b) do begin
    TestSequence(n+IntToStr(i),b);
    j := b[0];
    if Length(b) > 1 then
      move(b[1],b[0],(Length(b)-1)*SizeOf(b[0]));
    b[high(b)] := j;
    {$IFnDEF TEST_WITH_SLOW}
    break;
    {$ENDIF}
  end;
end;

procedure TTestRunnerList.RunSeq(const name: string);
begin
      TestSequence('XXX', [3,2,1,12,11,10,9,8,7,6,5,4]);
      TestSequence('XXX', [4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3]);
end;

procedure TTestRunnerList.RunSeqEx(const name: string);
begin
      TestSequenceEx('1', [1,2]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,11,12]);
      TestSequenceEx('1', [1,99,2,98,3,97,4,96,5,95,6,94]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,11,12,-1]);
      {$IFDEF TEST_WITH_SLOW}
      TestSequenceEx('1', [1,2,99,98,3,4,97,96,5,6,95,94,7,8,93,92,9,10]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,-1]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,-1]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,-1]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,-1]);
      TestSequenceEx('1', [1,2,3,4,5,6,-1]);
      TestSequenceEx('1', [1,2,3,4,5,-1]);
      TestSequenceEx('1', [1,2,3,4,-1]);
      {$ENDIF}
end;

procedure TTestRunnerList.RunTests(AProc: TTestProc);
var
  i1, i2: Integer;
begin
  Caller := self;
  for i1 := -1 to min(TListType.FTested.TEST_MAX_STEP, {$IFDEF TEST_WITH_SLOW} 5 {$ELSE} 3 {$ENDIF})
  do begin
    for i2 := 0 to min(TListType.FTested.TEST_MAX_STEP, 3) do begin
      case i1 of
       -1: GrowStep := -1;
        0: GrowStep := 0;
        1: GrowStep := 1;
        2: GrowStep := 4;
        3: GrowStep := 8;
        4: GrowStep := 2;
        5: GrowStep := 40;
      end;
      case i2 of
        0: ShrinkStep := -1;
        1: ShrinkStep :=  1;
        2: ShrinkStep :=  4;
        3: ShrinkStep := 99;
        //4: ShrinkStep := -2; // inherited exp shrink
      end;

      AProc('');
      if i1 = -1 then break; // GrowStep=-1 => ingnore ShrinkStep, use inherited;
    end;
  end;
end;

procedure TTestRunnerList.TestCreate;
begin
  RunTests(@TestNew);
end;

procedure TTestRunnerList.TestMove;
begin
  RunTests(@TestMove);
end;

procedure TTestRunnerList.TestShrink;
var
  ListWrapper: TListType;
begin
  ListWrapper.Create(HasMemInit);
  ListWrapper.FTested.InsertRows(0, 1000);
  AssertTrue('grow '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity >= 1000);

  ListWrapper.FTested.DeleteRows(2, 997);
  AssertTrue('shrink '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity < 100);

  ListWrapper.destroy;


  // test internal
  ListWrapper.Create(HasMemInit);
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;
  ListWrapper.Insert(0, CreateArray(1, 1000), Self);
  AssertTrue('internal grow '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity >= 1000);

  ListWrapper.Delete(2, 997);
  AssertTrue('internal shrink '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity < 100);

  ListWrapper.destroy;
end;

procedure TTestRunnerList.TestIndex;
begin
  RunTests(@RunIndex);
end;

procedure TTestRunnerList.TestSeq;
begin
  RunTests(@RunSeq);
end;

procedure TTestRunnerList.TestSeqEx;
begin
  RunTests(@RunSeqEx);
end;

{ TTestRunnerListMemInit }

function TTestRunnerListMemInit.HasMemInit: boolean;
begin
  Result := True;
end;

{ TTestRunnerListAnsiString }
{$INLINE OFF}  {$Optimization NOAUTOINLINE}

procedure TTestRunnerListAnsiString.AssertStringIsUnique(var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
var
  p: PChar;
begin
  p := pchar(s);
  UniqueString(s);
  //AssertTrue(p = PChar(s)); // This only works with some compiler setting / but at the end memory alloc is checked, so we know all strings were released
end;

procedure TTestRunnerListAnsiString.AssertStringIsNotUnique(var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
var
  p: PChar;
begin
  p := pchar(s);
  UniqueString(s);
  AssertTrue(p <> PChar(s));
end;

procedure TTestRunnerListAnsiString.GetNewString(var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
begin
  s := IntToStr(Random(99)); // a unique string with refcount=1
end;

function TTestRunnerListAnsiString.GetUsedHeap: ptruint;
var
  h: THeapStatus;
begin
  h := GetHeapStatus;
  Result := h.TotalAllocated;
end;

procedure TTestRunnerListAnsiString.DoTestInsert;
var
  TstLst: TListTypeX;

  procedure GetItem(idx: integer; var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
  begin
    s := TstLst.Items[idx]; // creates a temp string var for the "result" of the getter proc
  end;

(* There MUST NOT be any temporary string vars for "s"
   Therefore
   - do all modification/calls/assignments in none-inlined helper procs
   - always pass the string var as "var param", as that takes a pointer to the declared var
*)
var
  s: array of AnsiString;
  InsOrder, InsCnt, i, InsCnt1, InsCnt2, InsCnt3, j: Integer;
begin
  for InsOrder := 0 to 4 do
  for InsCnt := 1 to 20 do begin
    TstLst.Create;

    SetLength(s, InsCnt);
    case InsOrder of
      0: begin
        // full capacity
        TstLst.InsertRows(0,InsCnt);
        for i := 0 to InsCnt - 1 do begin
          GetNewString(s[i]);
          TstLst.Items[i] := s[i];
        end;
      end;
      1: begin
        // Slowly increase list size
        for i := 0 to InsCnt - 1 do begin
          TstLst.InsertRows(i,1);
          GetNewString(s[i]);
          TstLst.Items[i] := s[i];
        end;
      end;
      2: begin
        // full capacity // backward
        TstLst.InsertRows(0,InsCnt);
        for i := InsCnt - 1 downto 0 do begin
          GetNewString(s[i]);
          TstLst.Items[i] := s[i];
        end;
      end;
      3: begin
        // Slowly increase list size // backwards
        for i := InsCnt - 1 downto 0 do begin
          TstLst.InsertRows(0,1);
          GetNewString(s[i]);
          TstLst.Items[0] := s[i];
        end;
      end;
      4: begin
        // insert into existing
        for i := 0 to InsCnt - 1 do
        if (i and 1) = 0 then begin
          TstLst.InsertRows(i div 2, 1);
          GetNewString(s[i]);
          TstLst.Items[i div 2] := s[i];
        end;
        // now 2nd round goes between existing
        for i := 0 to InsCnt - 1 do
        if (i and 1) = 1 then begin
          TstLst.InsertRows(i div 2 + 1, 1);
          GetNewString(s[i]);
          TstLst.Items[i div 2 + 1] := s[i];
        end;
      end;
    end;

    for i := 0 to InsCnt - 1 do begin
      AssertStringIsNotUnique(s[i]);
      GetItem(i, s[i]); // restore orig var, as stored in list
      AssertStringIsNotUnique(s[i]);
      GetItem(i, s[i]); // restore orig var, as stored in list
    end;

    for i := 0 to InsCnt - 1 do begin
      TstLst.DeleteRows(0,1);
      AssertStringIsUnique(s[i]);
    end;

    TstLst.Destroy;
    s := nil;
  end;

  // insert blocks, out of order
  for InsCnt1 := 1 to 20 do
  for InsCnt2 := 1 to 20 do
  for InsCnt3 := 1 to 20 do begin
    TstLst.Create;
    InsCnt := InsCnt1+InsCnt2+InsCnt3;
    SetLength(s, InsCnt);

    // InsCnt3 first, push to end later
    j := InsCnt1+InsCnt2;
    TstLst.InsertRows(0,InsCnt3);
    for i := 0 to InsCnt3 - 1 do begin
      GetNewString(s[j+i]);
      TstLst.Items[i] := s[j+i];
    end;

    // InsCnt1, at start
    TstLst.InsertRows(0,InsCnt1);
    for i := 0 to InsCnt1 - 1 do begin
      GetNewString(s[i]);
      TstLst.Items[i] := s[i];
    end;

    // InsCnt2, insert into middle
    j := InsCnt1;
    TstLst.InsertRows(InsCnt1,InsCnt2);
    for i := 0 to InsCnt2 - 1 do begin
      GetNewString(s[j+i]);
      TstLst.Items[j+i] := s[j+i];
    end;


    for i := 0 to InsCnt - 1 do begin
      AssertStringIsNotUnique(s[i]);
      GetItem(i, s[i]); // restore orig var, as stored in list
      AssertStringIsNotUnique(s[i]);
      GetItem(i, s[i]); // restore orig var, as stored in list
    end;

    for i := 0 to InsCnt - 1 do begin
      TstLst.DeleteRows(0,1);
      AssertStringIsUnique(s[i]);
    end;

    TstLst.Destroy;
    s := nil;
  end;
end;

procedure TTestRunnerListAnsiString.DoTestDelete;
var
  TstLst: TListTypeX;

  procedure GetItem(idx: integer; var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
  begin
    s := TstLst.Items[idx]; // creates a temp string var for the "result" of the getter proc
  end;

(* There MUST NOT be any temporary string vars for "s"
   Therefore
   - do all modification/calls/assignments in none-inlined helper procs
   - always pass the string var as "var param", as that takes a pointer to the declared var
*)
var
  s: array of AnsiString;
  DelOrder, DelCnt, i, DelCnt1, DelCnt2, DelCnt3, j: Integer;
begin
  for DelOrder := 0 to 3 do
  for DelCnt := 1 to 20 do begin
    TstLst.Create;

    SetLength(s, DelCnt);
    TstLst.InsertRows(0,DelCnt);
    for i := 0 to DelCnt - 1 do begin
      GetNewString(s[i]);
      TstLst.Items[i] := s[i];
    end;

    for i := 0 to DelCnt - 1 do begin
      AssertStringIsNotUnique(s[i]);
      GetItem(i, s[i]); // restore orig var, as stored in list
    end;

    case DelOrder of
      0: begin
        // full capacity
        TstLst.DeleteRows(0,DelCnt);
        for i := 0 to DelCnt - 1 do begin
          AssertStringIsUnique(s[i]);
        end;
      end;
      1: begin
        // Slowly decrease list size
        for i := DelCnt - 1 downto 0 do begin
          TstLst.DeleteRows(i,1);
          AssertStringIsUnique(s[i]);
        end;
      end;
      2: begin
        // Slowly decrease list size // backwards
        for i := 0 to DelCnt - 1 do begin
          TstLst.DeleteRows(0,1);
          AssertStringIsUnique(s[i]);
        end;
      end;
      3: begin
        // delete between existing
        for i := DelCnt - 1 downto 0 do
        if (i and 1) = 0 then begin
          TstLst.DeleteRows(i, 1);
          AssertStringIsUnique(s[i]);
        end;
        // now the rest
        for i := DelCnt - 1 downto 0 do
        if (i and 1) = 1 then begin
          TstLst.DeleteRows(i div 2, 1);
          AssertStringIsUnique(s[i]);
        end;
      end;
    end;
    TstLst.Destroy;
    s := nil;
  end;


  // delete blocks, out of order
  for DelCnt1 := 1 to 20 do
  for DelCnt2 := 1 to 20 do
  for DelCnt3 := 1 to 20 do begin
    TstLst.Create;
    DelCnt := DelCnt1+DelCnt2+DelCnt3;
    SetLength(s, DelCnt);
    TstLst.InsertRows(0,DelCnt);
    for i := 0 to DelCnt - 1 do begin
      GetNewString(s[i]);
      TstLst.Items[i] := s[i];
    end;

    for i := 0 to DelCnt - 1 do begin
      AssertStringIsNotUnique(s[i]);
      GetItem(i, s[i]); // restore orig var, as stored in list
    end;


    // DelCnt2 in the middle
    j := DelCnt1;
    TstLst.DeleteRows(DelCnt1,DelCnt2);
    for i := 0 to DelCnt2 - 1 do
      AssertStringIsUnique(s[j+i]);

    // DelCnt1, at start
    TstLst.DeleteRows(0,DelCnt1);
    for i := 0 to DelCnt1 - 1 do
      AssertStringIsUnique(s[i]);

    // DelCnt3, the rest (now at start)
    j := DelCnt1+DelCnt2;
    TstLst.DeleteRows(0,DelCnt3);
    for i := 0 to DelCnt3 - 1 do
      AssertStringIsUnique(s[j+i]);

    TstLst.Destroy;
    s := nil;
  end;
end;

procedure TTestRunnerListAnsiString.DoTestAssign;
var
  TstLst: TListTypeX;

  procedure GetItem(idx: integer; var s: ansistring); {$IF FPC_FULLVERSION > 030300} noinline; {$ENDIF}
  begin
    s := TstLst.Items[idx]; // creates a temp string var for the "result" of the getter proc
  end;

(* There MUST NOT be any temporary string vars for "s"
   Therefore
   - do all modification/calls/assignments in none-inlined helper procs
   - always pass the string var as "var param", as that takes a pointer to the declared var
*)
var
  s: array of AnsiString;
  s2: AnsiString;
  InsCnt, i, AsgnIdx: Integer;
begin
  for InsCnt := 1 to 20 do begin
    TstLst.Create;
    SetLength(s, InsCnt);
    TstLst.InsertRows(0,InsCnt);
    for i := 0 to InsCnt - 1 do begin
      GetNewString(s[i]);
      TstLst.Items[i] := s[i];
    end;

    for i := 0 to InsCnt - 1 do begin
      AssertStringIsNotUnique(s[i]);
      GetItem(i, s[i]); // restore orig var, as stored in list
    end;

    for AsgnIdx := 1 to InsCnt do begin
      GetNewString(s2);
      TstLst.Items[AsgnIdx] := s2;
      AssertStringIsUnique(s[i]);
      GetItem(i, s[i]); // update to new item

      for i := 0 to InsCnt - 1 do begin
        AssertStringIsNotUnique(s[i]);
        GetItem(i, s[i]); // restore orig var, as stored in list
      end;
    end;

    TstLst.DeleteRows(0,InsCnt);
    for i := 0 to InsCnt - 1 do
      AssertStringIsUnique(s[i]);
  end;
end;

procedure TTestRunnerListAnsiString.TestInsert;
var
  mem: PtrUInt;
begin
  mem := GetUsedHeap;
  DoTestInsert;
  AssertEquals('All mem freed', mem, GetUsedHeap);
end;

procedure TTestRunnerListAnsiString.TestDelete;
var
  mem: PtrUInt;
begin
  mem := GetUsedHeap;
  DoTestDelete;
  AssertEquals('All mem freed', mem, GetUsedHeap);
end;

procedure TTestRunnerListAnsiString.TestAssign;
var
  mem: PtrUInt;
begin
  mem := GetUsedHeap;
  DoTestDelete;
  AssertEquals('All mem freed', mem, GetUsedHeap);
end;

{ TTestPagedBufferAnsiString8 }

procedure TTestPagedBufferAnsiString8.Create;
begin
  Create(3);
end;

{ TTestPagedBufferAnsiString4 }

procedure TTestPagedBufferAnsiString4.Create;
begin
  Create(2);
end;

initialization
  RegisterTest(TTestListMem );
  RegisterTest(TTestListMemMemInit );
  RegisterTest(TTestListMemSpecialized );
  RegisterTest(TTestListRoundMem );
  RegisterTest(TTestListRoundMemInit );
  RegisterTest(TTestListRoundMemSpecialized );
  RegisterTest(TTestListPagedMem0 );
  RegisterTest(TTestListPagedMem1 );
  RegisterTest(TTestListPagedMem2 );
  RegisterTest(TTestListPagedMem3 );
  RegisterTest(TTestListPagedMem4 );
  RegisterTest(TTestListPagedMemConst3 );
  RegisterTest(TTestListPagedMemInit0 );
  RegisterTest(TTestListPagedMemInit1 );
  RegisterTest(TTestListPagedMemInit2 );
  RegisterTest(TTestListPagedMemInit3 );
  RegisterTest(TTestListPagedMemInit4 );

  RegisterTest(TTestListShiftMemAnsiString);
  RegisterTest(TTestListRoundMemAnsiString);
  RegisterTest(TTestListPagedMemAnsiString4);
  RegisterTest(TTestListPagedMemAnsiString8);

end.


unit TestLazStorageMemCase1;

{$mode objfpc}{$H+}

{$DEFINE TEST_SKIP_SLOW}

interface

uses
  Classes, SysUtils, math, LazListClasses, LazLoggerBase, fpcunit, testutils, testregistry;

type

  { TTestLazMemWrapper }

  generic TTestLazMemWrapper<T> = object
  private
    FTested: T;
    FExpected: Array of Integer;
    function GetItems(AnIndex: Integer): Integer;
    procedure SetItems(AnIndex: Integer; AValue: Integer);
  public
    constructor Create;
    destructor destroy;
    function Insert(Avalue: Integer): Integer;
    function Insert(AnIndex: Integer; Avalue: Integer): PInteger;
    function Insert(AnIndex: Integer; Avalues: array of Integer): PInteger;
    procedure InsertExpected(AnIndex: Integer; Avalue: Integer);
    procedure InsertExpected(AnIndex: Integer; Avalues: array of Integer);
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer);
    procedure Delete(AIndex, ACount: Integer);
    procedure Clear;
    procedure AssertExp(AName: String; Caller: TTestCase);

    function TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
    function Count: Integer;
    function ItemPointer(AIndex: Integer): PInteger;
    property Items[AnIndex: Integer]: Integer read GetItems write SetItems;
    property Tested: T read FTested;
  end;

  { TTestLazShiftBufferListObj }

  TTestLazShiftBufferListObj = object(TLazShiftBufferListObj)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 20 {$ELSE} 17 {$ENDIF};
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    constructor Create;
    function TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazGenLazShiftBufferListObj }

  TTestLazGenLazShiftBufferListObj = object(specialize TLazShiftBufferListObjGen<Integer>)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 20 {$ELSE} 15 {$ENDIF};
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    function  TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazRoundBufferListMem }

  TTestLazRoundBufferListMem = object(TLazRoundBufferListObj)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 20 {$ELSE} 17 {$ENDIF};
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    constructor Create;
    function  TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazGenRoundBufferListMem }

  TTestLazGenRoundBufferListMem = object(specialize TLazRoundBufferListObjGen<Integer>)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 20 {$ELSE} 15 {$ENDIF};
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    function  TestInsertRows(AIndex, ACount: Integer): PInteger; inline;
    procedure TestDeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazPagedListMem }
  TTestLazPagedListMem = object(TLazPagedListObj)
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    function  TestInsertRows(AIndex, ACount: Integer): PInteger;
    procedure TestDeleteRows(AIndex, ACount: Integer);
  end;

  { TTestLazPagedListMem0 }

  TTestLazPagedListMem0 = object(TTestLazPagedListMem)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 15 {$ELSE} 11 {$ENDIF};
  public
    constructor Create;
  end;

  { TTestLazPagedListMem1 }

  TTestLazPagedListMem1 = object(TTestLazPagedListMem)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 25 {$ELSE} 19 {$ENDIF};
  public
    constructor Create;
  end;

  { TTestLazPagedListMem2 }

  TTestLazPagedListMem2 = object(TTestLazPagedListMem)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 25 {$ELSE} 19 {$ENDIF};
  public
    constructor Create;
  end;

  { TTestLazPagedListMem3 }

  TTestLazPagedListMem3 = object(TTestLazPagedListMem)
  protected
    const TEST_MAX_CNT = {$IFnDEF TEST_SKIP_SLOW} 35 {$ELSE} 25 {$ENDIF};
  public
    constructor Create;
  end;


  TTestProc = procedure(name: string) of object;

  { TTestRunnerList }

  generic TTestRunnerList<TListTypeX> = class(TTestCase)
  protected type TListType = specialize TTestLazMemWrapper<TListTypeX>;
  protected
    Caller: TTestCase;
    GrowStep: Integer;
    ShrinkStep: Integer;
    procedure TestNew({%H-}name: string);
    procedure TestMove({%H-}name: string);
    procedure TestSequence(name: string; a: Array of Integer); // Old test from a previous version
    procedure TestSequenceEx(n: string; a: Array of Integer);

    procedure RunSeq({%H-}name: string);
    procedure RunSeqEx({%H-}name: string);
    procedure RunTests(AProc: TTestProc);
  published
    procedure TestCreate;
    procedure TestMove;
    procedure TestShrink;
    procedure TestSeq;
    procedure TestSeqEx;
  end;


  TTestListMem = specialize TTestRunnerList<TTestLazShiftBufferListObj>;
  TTestListMemSpecialized = specialize TTestRunnerList<TTestLazGenLazShiftBufferListObj>;
  TTestListRoundMem = specialize TTestRunnerList<TTestLazRoundBufferListMem>;
  TTestListRoundMemSpecialized = specialize TTestRunnerList<TTestLazGenRoundBufferListMem>;
  TTestListPagedMem0 = specialize TTestRunnerList<TTestLazPagedListMem0>;
  TTestListPagedMem1 = specialize TTestRunnerList<TTestLazPagedListMem1>;
  TTestListPagedMem2 = specialize TTestRunnerList<TTestLazPagedListMem2>;
  TTestListPagedMem3 = specialize TTestRunnerList<TTestLazPagedListMem3>;


  TIntArray = Array of Integer;

function CreateArray(ALow,ACount: integer): TIntArray;
function JoinArrays(a,b: array of integer): TIntArray;
function JoinArrays(a,b,c: array of integer): TIntArray;

implementation

function CreateArray(ALow,ACount: integer): TIntArray;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := i+ALow;
end;

function JoinArrays(a,b: array of integer): TIntArray;
var
  i,j: Integer;
begin
  SetLength(Result, length(a)+Length(b));
  for i := 0 to high(a) do
    Result[i] := a[i];
  j := Length(a);
  for i := 0 to high(b) do
    Result[j+i] := b[i];
end;

function JoinArrays(a,b,c: array of integer): TIntArray;
var
  i,j: Integer;
begin
  SetLength(Result, length(a)+Length(b)+Length(c));
  for i := 0 to high(a) do
    Result[i] := a[i];
  j := Length(a);
  for i := 0 to high(b) do
    Result[j+i] := b[i];
  j := j + Length(b);
  for i := 0 to high(c) do
    Result[j+i] := c[i];
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

constructor TTestLazMemWrapper.Create;
begin
  FTested.create;
end;

destructor TTestLazMemWrapper.destroy;
begin
  FTested.destroy;
end;

function TTestLazMemWrapper.Insert(Avalue: Integer): Integer;
begin
  Result := 0;
  while (Result < FTested.Count) do begin
    if (Items[Result] > Avalue) then break;
    inc(Result);
  end;
  FTested.TestInsertRows(Result, 1);
  Items[Result] := Avalue;
end;

function TTestLazMemWrapper.Insert(AnIndex: Integer; Avalue: Integer): PInteger;
begin
  Result := Insert(AnIndex, CreateArray(Avalue, 1));
end;

function TTestLazMemWrapper.Insert(AnIndex: Integer; Avalues: array of Integer): PInteger;
var
  i: Integer;
begin
  //debugln(['TTestLazMemWrapper.Insert ',AnIndex,'  ',Length(Avalues)]);
  Result := FTested.TestInsertRows(AnIndex, length(Avalues));
  for i := 0 to high(Avalues) do
    Items[i+AnIndex] := Avalues[i];
  if FExpected = nil then
    FExpected := JoinArrays(Avalues, [])
  else {$PUSH}{$R-}
    FExpected := JoinArrays(FExpected[0..(AnIndex-1)], Avalues, FExpected[AnIndex..high(FExpected)]);
    {$POP}
end;

procedure TTestLazMemWrapper.InsertExpected(AnIndex: Integer; Avalue: Integer);
begin
  InsertExpected(AnIndex, CreateArray(Avalue, 1));
end;

procedure TTestLazMemWrapper.InsertExpected(AnIndex: Integer; Avalues: array of Integer);
begin
  if FExpected = nil then
    FExpected := JoinArrays(Avalues, [])
  else {$PUSH}{$R-}
    FExpected := JoinArrays(FExpected[0..(AnIndex-1)], Avalues, FExpected[AnIndex..high(FExpected)]);
    {$POP}
end;

procedure TTestLazMemWrapper.MoveRows(AFromIndex, AToIndex, ACount: Integer);
var
  i: Integer;
begin
  FTested.MoveRows(AFromIndex, AToIndex, ACount);
  move(FExpected[AFromIndex], FExpected[AToIndex], ACount * SizeOf(FExpected[0]));
  if AFromIndex < AToIndex then
    for i := AFromIndex to AToIndex-1 do begin
      Items[i] := -99;
      FExpected[i] := -99;
    end
  else
    for i := AToIndex+ACount to AFromIndex+ACount-1 do begin
      Items[i] := -99;
      FExpected[i] := -99;
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

procedure TTestLazMemWrapper.AssertExp(AName: String; Caller: TTestCase);
var
  i: Integer;
  s: String;
begin
  try
    Caller.AssertEquals(Format(AName+' Expect Count %d, %d', [Length(FExpected), Count]), Length(FExpected), Count);
    for i := 0 to FTested.Count-1 do
      Caller.AssertEquals(Format(AName+' Test %d / %d, %d', [i, FExpected[i], Items[i]]), FExpected[i], Items[i]);
  except
    on e: Exception do begin
      FTested.DebugDump;
      dbgout(['EXPECTED ', length(FExpected), ': ']);
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

{ TTestLazShiftBufferListObj }

function TTestLazShiftBufferListObj.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazShiftBufferListObj.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazShiftBufferListObj.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if Capacity - FShrinkStep >  ARequired then
    Result := ARequired
  else
    Result := -1;
end;

constructor TTestLazShiftBufferListObj.Create;
begin
  inherited Create(SizeOf(Integer));
end;

function TTestLazShiftBufferListObj.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazShiftBufferListObj.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TTestLazGenLazShiftBufferListObj }

function TTestLazGenLazShiftBufferListObj.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazGenLazShiftBufferListObj.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazGenLazShiftBufferListObj.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if Capacity - FShrinkStep >  ARequired then
    Result := ARequired
  else
    Result := -1;
end;

function TTestLazGenLazShiftBufferListObj.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazGenLazShiftBufferListObj.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TTestLazRoundBufferListMem }

function TTestLazRoundBufferListMem.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazRoundBufferListMem.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazRoundBufferListMem.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if Capacity - FShrinkStep >  ARequired then
    Result := ARequired
  else
    Result := -1;
end;

constructor TTestLazRoundBufferListMem.Create;
begin
  inherited Create(SizeOf(Integer));
end;

function TTestLazRoundBufferListMem.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazRoundBufferListMem.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TTestLazGenRoundBufferListMem }

function TTestLazGenRoundBufferListMem.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazGenLazShiftBufferListObj.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazGenRoundBufferListMem.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if Capacity - FShrinkStep >  ARequired then
    Result := ARequired
  else
    Result := -1;
end;

function TTestLazGenRoundBufferListMem.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  Result := InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazGenRoundBufferListMem.TestDeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TTestLazPagedListMem }

function TTestLazPagedListMem.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazShiftBufferListObj.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazPagedListMem.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if Capacity - FShrinkStep >  ARequired then
    Result := ARequired
  else
    Result := -1;
end;

function TTestLazPagedListMem.TestInsertRows(AIndex, ACount: Integer): PInteger;
begin
  inherited InsertRows(AIndex, ACount);
  //InsertRowsEx(AIndex, ACount, @GrowCapacity);
  Result := PInteger(ItemPointer[AIndex]);
end;

procedure TTestLazPagedListMem.TestDeleteRows(AIndex, ACount: Integer);
begin
  inherited DeleteRows(AIndex, ACount);
  //DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
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

{ TTestRunnerList }

procedure TTestRunnerList.TestNew(name: string);
var
  ListWrapper, ListWrapper2: TListType;
  i, j, k: Integer;
  p: PInteger;
begin
  ListWrapper.Create;
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;
  ListWrapper2.Create;
  ListWrapper2.FTested.FGrowStep := GrowStep;
  ListWrapper2.FTested.FShrinkStep := ShrinkStep;

  for i := 1 to 25 do begin
    p := ListWrapper.Insert(0, CreateArray(1, i));
    Caller.AssertEquals('', {%H-}PtrInt(ListWrapper.ItemPointer(0)), {%H-}PtrInt(p));
    ListWrapper.AssertExp(format('Insert %d at 0', [i]), Caller);

    for j := 0 to i do
    for k := 1 to 25 do begin
      p := ListWrapper.Insert(j, CreateArray(100*k, k));
      Caller.AssertEquals('', {%H-}PtrInt(ListWrapper.ItemPointer(j)), {%H-}PtrInt(p));
      ListWrapper.AssertExp(format('Insert %d at %d', [k, j]), Caller);

      ListWrapper.Delete(j, k);
      ListWrapper.AssertExp(format('Delete %d at %d', [k, j]), Caller);

      Caller.AssertEquals('', i, ListWrapper.Count);

      // start form empty, may have different free-at-start
      ListWrapper2.Clear;
      p := ListWrapper2.Insert(0, CreateArray(k, i));
      ListWrapper2.AssertExp(format('Insert %d at 0', [i]), Caller);

      p := ListWrapper2.Insert(j, CreateArray(100*k, k));
      Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer(j)), {%H-}PtrInt(p));
      ListWrapper2.AssertExp(format('ListWrapper2 Insert %d at %d', [k, j]), Caller);
      ListWrapper2.Delete(j, k);
      ListWrapper2.AssertExp(format('ListWrapper2 Delete %d at %d', [k, j]), Caller);
      Caller.AssertEquals('', i, ListWrapper.Count);

      if  byte(k) in [1,9,10,11,20] then begin
        // test with space at start
        ListWrapper2.Clear;
        p := ListWrapper2.Insert(0, CreateArray(k, i+10));
        ListWrapper2.Delete(0, 10);
        ListWrapper2.AssertExp(format('Insert %d at 0', [i]), Caller);

        p := ListWrapper2.Insert(j, CreateArray(100*k, k));
        Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer(j)), {%H-}PtrInt(p));
        ListWrapper2.AssertExp(format('ListWrapper2 Insert %d at %d', [k, j]), Caller);
        ListWrapper2.Delete(j, k);
        ListWrapper2.AssertExp(format('ListWrapper2 Delete %d at %d', [k, j]), Caller);
        Caller.AssertEquals('', i, ListWrapper.Count);

        // test with space at end
        ListWrapper2.Clear;
        p := ListWrapper2.Insert(0, CreateArray(k, i+10));
        ListWrapper2.Delete(i, 10);
        ListWrapper2.AssertExp(format('Insert %d at 0', [i]), Caller);

        p := ListWrapper2.Insert(j, CreateArray(100*k, k));
        Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer(j)), {%H-}PtrInt(p));
        ListWrapper2.AssertExp(format('ListWrapper2 Insert %d at %d', [k, j]), Caller);
        ListWrapper2.Delete(j, k);
        ListWrapper2.AssertExp(format('ListWrapper2 Delete %d at %d', [k, j]), Caller);
        Caller.AssertEquals('', i, ListWrapper.Count);

      end;

    end;


    for k := 1 to (i div 2) - 1 do begin
      ListWrapper2.Clear;
      p := ListWrapper2.Insert(0, CreateArray(k, i));
      ListWrapper2.Delete(i-k, k);
      ListWrapper2.Delete(0, k);
      ListWrapper2.AssertExp(format('ListWrapper2 Delete %d at %d', [i, k]), Caller);

      p := ListWrapper2.Insert((i-(2*k)) div 2, CreateArray(90*k, 2*k));
      Caller.AssertEquals('', {%H-}PtrInt(ListWrapper2.ItemPointer((i-(2*k)) div 2)), {%H-}PtrInt(p));
      ListWrapper2.AssertExp(format('ListWrapper2 Delete %d at %d', [i, k]), Caller);
    end;


    for j := 0 to i-1 do
    for k := 1 to i-j do begin
      ListWrapper2.Clear;
      p := ListWrapper2.Insert(0, CreateArray(1, i));
      ListWrapper2.Delete(j, k);
      ListWrapper2.AssertExp(format('ListWrapper2 Delete(2) %d at %d', [k, j]), Caller);
    end;

    ListWrapper.Clear;
  end;

  ListWrapper.destroy;
  ListWrapper2.destroy;
end;

procedure TTestRunnerList.TestMove(name: string);
var
  ListWrapper: TListType;
  InitCnt, FromPos, ToPos, MoveLen, DelCnt, InsCnt: Integer;
begin
  ListWrapper.Create;
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;

  for InitCnt := 2 to ListWrapper.FTested.TEST_MAX_CNT do
    for FromPos := 0 to InitCnt-1 do // from
    for ToPos := 0 to InitCnt-1 do // to
    for MoveLen := 1 to InitCnt-Max(FromPos,ToPos)-1 do // len
    begin
      if FromPos=ToPos then continue;
//debugln(['>>>>>>>>>> TestMoveA ',InitCnt,',',FromPos,',',ToPos,',',MoveLen]);
      ListWrapper.Insert(0, CreateArray(1, InitCnt));
      //ListWrapper.AssertExp(format('Insert %d at 0', [InitCnt]), Caller);

      ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
      ListWrapper.AssertExp(format('MOV %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen]), Caller);
      ListWrapper.Clear;


      if (FromPos < min(8, ListWrapper.FTested.TEST_MAX_CNT div 4)) or
         (ToPos < min(8, ListWrapper.FTested.TEST_MAX_CNT div 4)) or
         (FromPos > ListWrapper.FTested.TEST_MAX_CNT - min(5, ListWrapper.FTested.TEST_MAX_CNT div 6)) or
         (ToPos > ListWrapper.FTested.TEST_MAX_CNT - min(5, ListWrapper.FTested.TEST_MAX_CNT div 6))
      then begin
        // vary the GAP at start
        for DelCnt := 1 to min(7, ListWrapper.FTested.TEST_MAX_CNT div 4) do begin
          ListWrapper.Create;
          ListWrapper.FTested.FGrowStep := GrowStep;
          ListWrapper.FTested.FShrinkStep := ShrinkStep;
          ListWrapper.Insert(0, CreateArray(1, InitCnt+DelCnt));
          ListWrapper.Delete(0, DelCnt);
          ListWrapper.AssertExp(format('Insert %d at 0', [InitCnt]), Caller);

          ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
          ListWrapper.AssertExp(format('MOV %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen]), Caller);
          ListWrapper.Clear;
        end;

        for InsCnt := 1 to min(4, InitCnt-1) do begin
          ListWrapper.Create;
          ListWrapper.FTested.FGrowStep := GrowStep;
          ListWrapper.FTested.FShrinkStep := ShrinkStep;
          ListWrapper.Insert(0, CreateArray(1, InitCnt-InsCnt));
          ListWrapper.Insert(0, CreateArray(100, InsCnt));
          ListWrapper.AssertExp(format('Insert %d at 0', [InitCnt]), Caller);

          ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
          ListWrapper.AssertExp(format('MOV %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen]), Caller);
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
      ListWrapper.Create;
      ListWrapper.FTested.FGrowStep := GrowStep;
      ListWrapper.FTested.FShrinkStep := ShrinkStep;

      ListWrapper.Insert(0, CreateArray(1, InitCnt));
      ListWrapper.Delete(0, DelCnt);
      ListWrapper.Insert(InitCnt-DelCnt, CreateArray(100, DelCnt)); // fill roundbuffer
      ListWrapper.AssertExp(format('Insert %d at 0', [InitCnt]), Caller);

      ListWrapper.MoveRows(FromPos,ToPos,MoveLen);
      ListWrapper.AssertExp(format('MOV3 %d / %d %d %d ', [InitCnt,FromPos,ToPos,MoveLen]), Caller);
      ListWrapper.Clear;
      ListWrapper.destroy;
    end;

end;

procedure TTestRunnerList.TestSequence(name: string; a: array of Integer);
var
  ListWrapper: TListType;
  i, j, k, n, m, o: Integer;
begin
  ListWrapper.Create;
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;

  for i := 0 to high(a) do begin
    ListWrapper.Insert(a[i]);
    Caller.AssertTrue(Format(name+' Test Cnt %d %d ', [i, ListWrapper.Count]), ListWrapper.Count = i+1);
//for j := 0 to ListWrapper.Count-1 do dbgout([ListWrapper.Items[j],', ']); debugln(' <<<');
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d / %d, %d', [i, j, ListWrapper.Items[j], ListWrapper.Items[j-1]]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(ListWrapper.count-1, 1);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := 0 to high(a) do begin
    k := ListWrapper.Insert(a[i]);
    Caller.AssertEquals(Format(name+' Test %d %d', [i, j]),a[i], ListWrapper.Items[k]);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;
  while ListWrapper.count> 1 do begin
    ListWrapper.TestDeleteRows(ListWrapper.count-2, 2);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := 0 to high(a) do begin
    ListWrapper.Insert(a[i]);
  end;
  for j := 1 to ListWrapper.Count-1 do
    Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(0, 1);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := high(a) downto 0 do begin
    k := ListWrapper.Insert(a[i]);
    Caller.AssertEquals(Format(name+' Test idx %d %d / %d %d', [i, j, k, ListWrapper.Items[k]]),a[i], ListWrapper.Items[k]);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d /  / %d %d', [i, j, ListWrapper.Items[j], ListWrapper.Items[j-1]]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(0, Min(ListWrapper.count, 2));
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;

  ListWrapper.Clear;
  for i := high(a) downto 0 do begin
    k := ListWrapper.Insert(a[i]);
  end;
  while ListWrapper.count> 0 do begin
    ListWrapper.TestDeleteRows(ListWrapper.count div 2, 1);
    for j := 1 to ListWrapper.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
  end;


  for m := 0 to length(a)-1 do begin
    for n := 0 to m do begin
      ListWrapper.Clear;
      for i := 0 to m do begin
        k := ListWrapper.Insert(a[i]);
        Caller.AssertEquals(Format(name+' Test %d %d', [n, i]),a[i], ListWrapper.Items[k]);
      end;
      for j := 1 to ListWrapper.Count-1 do
        Caller.AssertTrue(Format(name+' Test %d %d', [n, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
      k := ListWrapper.Items[n];
      ListWrapper.TestDeleteRows(n, 1);
      for j := 1 to ListWrapper.Count-1 do
        Caller.AssertTrue(Format(name+' Test %d %d %d %d', [n, j, ListWrapper.Items[j], ListWrapper.Items[j-1]]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
      for j := 0 to ListWrapper.Count-1 do
        Caller.AssertTrue(Format(name+' Test %d %d - idx %d <> %d', [n, j, k, ListWrapper.Items[j]]), ListWrapper.Items[j] <> k);
      while ListWrapper.count > 1 do begin
        o := Max(0,Min(ListWrapper.count-2, n));
        k := ListWrapper.Items[o];
        ListWrapper.TestDeleteRows(o, 2);
        for j := 1 to ListWrapper.Count-1 do begin
          Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] > ListWrapper.Items[j-1]);
          Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), ListWrapper.Items[j] <> k);
        end;
      end;

    end;
  end;

  ListWrapper.Destroy;
end;

procedure TTestRunnerList.TestSequenceEx(n: string; a: array of Integer);
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

  SetLength(b, Length(a));
  for i := 0 to length(a)-1 do
    b[i] := a[high(a)-i];

  for i := 1 to length(b) do begin
    TestSequence(n+IntToStr(i),b);
    j := b[0];
    if Length(b) > 1 then
      move(b[1],b[0],(Length(b)-1)*SizeOf(b[0]));
    b[high(b)] := j;
    {$IFDEF TEST_SKIP_SLOW}
    break;
    {$ENDIF}
  end;
end;

procedure TTestRunnerList.RunSeq(name: string);
begin
      TestSequence('XXX', [3,2,1,12,11,10,9,8,7,6,5,4]);
      TestSequence('XXX', [4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3]);
end;

procedure TTestRunnerList.RunSeqEx(name: string);
begin
      TestSequenceEx('1', [1,2]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,11,12]);
//GrowStep := 1 * 4;
      TestSequenceEx('1', [1,99,2,98,3,97,4,96,5,95,6,94]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,11,12,-1]);
      {$IFnDEF TEST_SKIP_SLOW}
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
  for i1 := 0 to 2 do begin
    for i2 := 0 to 3 do begin
      GrowStep := i1 * 4;
      case i2 of
        0: ShrinkStep := -1;
        1: ShrinkStep :=  1;
        2: ShrinkStep :=  4;
        3: ShrinkStep := 99;
      end;

      AProc('');
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
  ListWrapper.Create;
  ListWrapper.FTested.InsertRows(0, 1000);
  AssertTrue('grow '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity >= 1000);

  ListWrapper.FTested.DeleteRows(2, 997);
  AssertTrue('shrink '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity < 100);

  ListWrapper.destroy;


  // test internal
  ListWrapper.Create;
  ListWrapper.FTested.FGrowStep := GrowStep;
  ListWrapper.FTested.FShrinkStep := ShrinkStep;
  ListWrapper.Insert(0, CreateArray(1, 1000));
  AssertTrue('internal grow '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity >= 1000);

  ListWrapper.Delete(2, 997);
  AssertTrue('internal shrink '+IntToStr(ListWrapper.FTested.Capacity), ListWrapper.FTested.Capacity < 100);

  ListWrapper.destroy;
end;

procedure TTestRunnerList.TestSeq;
begin
  RunTests(@RunSeq);
end;

procedure TTestRunnerList.TestSeqEx;
begin
  RunTests(@RunSeqEx);
end;

initialization

  RegisterTest(TTestListMem );
  RegisterTest(TTestListMemSpecialized );
  RegisterTest(TTestListRoundMem );
  RegisterTest(TTestListRoundMemSpecialized );
  RegisterTest(TTestListPagedMem0 );
  RegisterTest(TTestListPagedMem1 );
  RegisterTest(TTestListPagedMem2 );
  RegisterTest(TTestListPagedMem3 );

end.


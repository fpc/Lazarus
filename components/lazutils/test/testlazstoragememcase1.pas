unit TestLazStorageMemCase1;

{$mode objfpc}{$H+}

{$DEFINE TEST_SKIP_SLOW}

interface

uses
  Classes, SysUtils, math, LazListClasses, LazLoggerBase, fpcunit, testutils, testregistry;

type

  { TTestStorageMem }

  TTestStorageMem = class(TTestCase)
  published
    procedure TestMem;
    procedure TestMemSpecialized;
    procedure TestMemRound;
    procedure TestMemRoundSpecialized;
    procedure TestMemPaged;
    procedure TestMemClass;
    procedure TestMemSpecializedClass;
  end;

implementation

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
    procedure Insert(AnIndex: Integer; Avalues: array of Integer);
    procedure Delete(AIndex, ACount: Integer);
    procedure Clear;
    procedure AssertExp(AName: String; Caller: TTestStorageMem);

    procedure InsertRows(AIndex, ACount: Integer); inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
    function Count: Integer;
    property Items[AnIndex: Integer]: Integer read GetItems write SetItems;
  end;

  { TTestLazDualCapacityListMem }

  TTestLazDualCapacityListMem = object(TLazDualCapacityListMem)
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    constructor Create;
    procedure InsertRows(AIndex, ACount: Integer); inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazGenDualCapacityListMem }

  TTestLazGenDualCapacityListMem = object(specialize TLazGenDualCapacityListMem<Integer>)
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    procedure InsertRows(AIndex, ACount: Integer); inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazRoundBufferListMem }

  TTestLazRoundBufferListMem = object(TLazRoundBufferListMem)
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    constructor Create;
    procedure InsertRows(AIndex, ACount: Integer); inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazGenRoundBufferListMem }

  TTestLazGenRoundBufferListMem = object(specialize TLazGenRoundBufferListMem<Integer>)
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    procedure InsertRows(AIndex, ACount: Integer); inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TTestLazPagedListMem }
  TTestLazPagedListMem = object(TLazPagedListMem)
  protected
    FGrowStep: Integer;
    FShrinkStep: Integer;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
  public
    procedure InsertRows(AIndex, ACount: Integer);
    procedure DeleteRows(AIndex, ACount: Integer);
  end;

  { TTestLazPagedListMem0 }

  TTestLazPagedListMem0 = object(TTestLazPagedListMem)
    constructor Create;
  end;

  { TTestLazPagedListMem1 }

  TTestLazPagedListMem1 = object(TTestLazPagedListMem)
    constructor Create;
  end;

  { TTestLazPagedListMem2 }

  TTestLazPagedListMem2 = object(TTestLazPagedListMem)
    constructor Create;
  end;

  { TTestLazPagedListMem3 }

  TTestLazPagedListMem3 = object(TTestLazPagedListMem)
    constructor Create;
  end;


  { TTestRunnerList }

  generic TTestRunnerList<TListTypeX> = object
  type TListType = specialize TTestLazMemWrapper<TListTypeX>;
  public
    Caller: TTestStorageMem;
    GrowStep: Integer;
    ShrinkStep: Integer;
    procedure TestNew(name: string);
    procedure TestSequence(name: string; a: Array of Integer); // Old test from a previous version
    procedure TestSequenceEx(n: string; a: Array of Integer);
    procedure RunTest(ACaller: TTestStorageMem);
  end;

  TTestListMem = specialize TTestRunnerList<TTestLazDualCapacityListMem>;
  TTestListMemSpecialized = specialize TTestRunnerList<TTestLazGenDualCapacityListMem>;
  TTestListRoundMem = specialize TTestRunnerList<TTestLazRoundBufferListMem>;
  TTestListRoundMemSpecialized = specialize TTestRunnerList<TTestLazGenRoundBufferListMem>;
  TTestListPagedMem0 = specialize TTestRunnerList<TTestLazPagedListMem0>;
  TTestListPagedMem1 = specialize TTestRunnerList<TTestLazPagedListMem1>;
  TTestListPagedMem2 = specialize TTestRunnerList<TTestLazPagedListMem2>;
  TTestListPagedMem3 = specialize TTestRunnerList<TTestLazPagedListMem3>;

  TIntArray = Array of Integer;

function CreateArray(ALow,ACount: integer): TIntArray;
var
  i,j: Integer;
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
  FTested.InsertRows(Result, 1);
  Items[Result] := Avalue;
end;

procedure TTestLazMemWrapper.Insert(AnIndex: Integer; Avalues: array of Integer);
var
  i: Integer;
begin
  //debugln(['TTestLazMemWrapper.Insert ',AnIndex,'  ',Length(Avalues)]);
  FTested.InsertRows(AnIndex, length(Avalues));
  for i := 0 to high(Avalues) do
    Items[i+AnIndex] := Avalues[i];
  if FExpected = nil then
    FExpected := JoinArrays(Avalues, [])
  else {$PUSH}{$R-}
    FExpected := JoinArrays(FExpected[0..(AnIndex-1)], Avalues, FExpected[AnIndex..high(FExpected)]);
    {$POP}
end;

procedure TTestLazMemWrapper.Delete(AIndex, ACount: Integer);
begin
  //debugln(['TTestLazMemWrapper.Delete ',AIndex,'  ',ACount]);
  DeleteRows(AIndex, ACount);
  {$PUSH}{$R-}
  FExpected := JoinArrays(FExpected[0..AIndex-1], FExpected[(AIndex+ACount)..High(FExpected)]);
  {$POP}
end;

procedure TTestLazMemWrapper.Clear;
begin
  FTested.DeleteRows(0, FTested.Count);
  FTested.Capacity := 0;
  FExpected := nil;
  Assert(0 = FTested.Count);
  Assert(0 = FTested.Capacity);
end;

procedure TTestLazMemWrapper.AssertExp(AName: String; Caller: TTestStorageMem);
var
  i: Integer;
begin
  Caller.AssertEquals(Format(AName+' Expect Count %d, %d', [Length(FExpected), Count]), Length(FExpected), Count);
  for i := 0 to FTested.Count-1 do
    Caller.AssertEquals(Format(AName+' Test %d / %d, %d', [i, FExpected[i], Items[i]]), FExpected[i], Items[i]);
end;

procedure TTestLazMemWrapper.InsertRows(AIndex, ACount: Integer);
begin
  FTested.InsertRows(AIndex, ACount);
end;

procedure TTestLazMemWrapper.DeleteRows(AIndex, ACount: Integer);
begin
  FTested.DeleteRows(AIndex, ACount);
end;

function TTestLazMemWrapper.Count: Integer;
begin
  result := FTested.Count;
end;

{ TTestLazDualCapacityListMem }

function TTestLazDualCapacityListMem.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazDualCapacityListMem.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazDualCapacityListMem.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if ARequired - Count > FShrinkStep then
    Result := Count
  else
    Result := -1;
end;

constructor TTestLazDualCapacityListMem.Create;
begin
  inherited Create(SizeOf(Integer));
end;

procedure TTestLazDualCapacityListMem.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazDualCapacityListMem.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TTestLazGenDualCapacityListMem }

function TTestLazGenDualCapacityListMem.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazGenDualCapacityListMem.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazGenDualCapacityListMem.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if ARequired - Count > FShrinkStep then
    Result := Count
  else
    Result := -1;
end;

procedure TTestLazGenDualCapacityListMem.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazGenDualCapacityListMem.DeleteRows(AIndex, ACount: Integer);
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
  if ARequired - Count > FShrinkStep then
    Result := Count
  else
    Result := -1;
end;

constructor TTestLazRoundBufferListMem.Create;
begin
  inherited Create(SizeOf(Integer));
end;

procedure TTestLazRoundBufferListMem.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazRoundBufferListMem.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TTestLazGenRoundBufferListMem }

function TTestLazGenRoundBufferListMem.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazGenDualCapacityListMem.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazGenRoundBufferListMem.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if ARequired - Count > FShrinkStep then
    Result := Count
  else
    Result := -1;
end;

procedure TTestLazGenRoundBufferListMem.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazGenRoundBufferListMem.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TTestLazPagedListMem }

function TTestLazPagedListMem.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(FGrowStep >= 0, 'TTestLazDualCapacityListMem.GrowCapacity: FGrowStep >= 0');
  Result := ARequired + FGrowStep;
end;

function TTestLazPagedListMem.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if FShrinkStep < 0 then exit(-1);
  if ARequired - Count > FShrinkStep then
    Result := Count
  else
    Result := -1;
end;

procedure TTestLazPagedListMem.InsertRows(AIndex, ACount: Integer);
begin
  inherited InsertRows(AIndex, ACount);
  //InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TTestLazPagedListMem.DeleteRows(AIndex, ACount: Integer);
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
  c, c2: TListType;
  i, j, k: Integer;
begin
  c.Create;
  c.FTested.FGrowStep := GrowStep;
  c.FTested.FShrinkStep := ShrinkStep;
  c2.Create;
  c2.FTested.FGrowStep := GrowStep;
  c2.FTested.FShrinkStep := ShrinkStep;

  for i := 1 to 25 do begin
    c.Insert(0, CreateArray(1, i));
    c.AssertExp(format('Insert %d at 0', [i]), Caller);

    for j := 0 to i do
    for k := 1 to 25 do begin
      c.Insert(j, CreateArray(100*k, k));
      c.AssertExp(format('Insert %d at %d', [k, j]), Caller);

      c.Delete(j, k);
      c.AssertExp(format('Delete %d at %d', [k, j]), Caller);

      Caller.AssertEquals('', i, c.Count);

      // start form empty, may have different free-at-start
      c2.Clear;
      c2.Insert(0, CreateArray(k, i));
      c2.AssertExp(format('Insert %d at 0', [i]), Caller);

      c2.Insert(j, CreateArray(100*k, k));
      c2.AssertExp(format('c2 Insert %d at %d', [k, j]), Caller);
      c2.Delete(j, k);
      c2.AssertExp(format('c2 Delete %d at %d', [k, j]), Caller);
      Caller.AssertEquals('', i, c.Count);
    end;

    for j := 0 to i-1 do
    for k := 1 to i-j do begin
      c2.Clear;
      c2.Insert(0, CreateArray(1, i));
      c2.Delete(j, k);
      c2.AssertExp(format('c2 Delete(2) %d at %d', [k, j]), Caller);
    end;

    c.Clear;
  end;

  c.destroy;
  c2.destroy;
end;

procedure TTestRunnerList.TestSequence(name: string; a: array of Integer);
var
  c: TListType;
  i, j, k, n, m, o: Integer;
begin
  c.Create;
  c.FTested.FGrowStep := GrowStep;
  c.FTested.FShrinkStep := ShrinkStep;

  for i := 0 to high(a) do begin
    c.Insert(a[i]);
    Caller.AssertTrue(Format(name+' Test Cnt %d %d ', [i, c.Count]), c.Count = i+1);
//for j := 0 to c.Count-1 do dbgout([c.Items[j],', ']); debugln(' <<<');
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d / %d, %d', [i, j, c.Items[j], c.Items[j-1]]), c.Items[j] > c.Items[j-1]);
  end;
  while c.count> 0 do begin
    c.DeleteRows(c.count-1, 1);
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
  end;

  c.Clear;
  for i := 0 to high(a) do begin
    k := c.Insert(a[i]);
    Caller.AssertEquals(Format(name+' Test %d %d', [i, j]),a[i], c.Items[k]);
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
  end;
  while c.count> 1 do begin
    c.DeleteRows(c.count-2, 2);
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
  end;

  c.Clear;
  for i := 0 to high(a) do begin
    c.Insert(a[i]);
  end;
  for j := 1 to c.Count-1 do
    Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
  while c.count> 0 do begin
    c.DeleteRows(0, 1);
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
  end;

  c.Clear;
  for i := high(a) downto 0 do begin
    k := c.Insert(a[i]);
    Caller.AssertEquals(Format(name+' Test idx %d %d / %d %d', [i, j, k, c.Items[k]]),a[i], c.Items[k]);
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d /  / %d %d', [i, j, c.Items[j], c.Items[j-1]]), c.Items[j] > c.Items[j-1]);
  end;
  while c.count> 0 do begin
    c.DeleteRows(0, Min(c.count, 2));
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
  end;

  c.Clear;
  for i := high(a) downto 0 do begin
    k := c.Insert(a[i]);
  end;
  while c.count> 0 do begin
    c.DeleteRows(c.count div 2, 1);
    for j := 1 to c.Count-1 do
      Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
  end;


  for m := 0 to length(a)-1 do begin
    for n := 0 to m do begin
      c.Clear;
      for i := 0 to m do begin
        k := c.Insert(a[i]);
        Caller.AssertEquals(Format(name+' Test %d %d', [n, i]),a[i], c.Items[k]);
      end;
      for j := 1 to c.Count-1 do
        Caller.AssertTrue(Format(name+' Test %d %d', [n, j]), c.Items[j] > c.Items[j-1]);
      k := c.Items[n];
      c.DeleteRows(n, 1);
      for j := 1 to c.Count-1 do
        Caller.AssertTrue(Format(name+' Test %d %d %d %d', [n, j, c.Items[j], c.Items[j-1]]), c.Items[j] > c.Items[j-1]);
      for j := 0 to c.Count-1 do
        Caller.AssertTrue(Format(name+' Test %d %d - idx %d <> %d', [n, j, k, c.Items[j]]), c.Items[j] <> k);
      while c.count > 1 do begin
        o := Max(0,Min(c.count-2, n));
        k := c.Items[o];
        c.DeleteRows(o, 2);
        for j := 1 to c.Count-1 do begin
          Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] > c.Items[j-1]);
          Caller.AssertTrue(Format(name+' Test %d %d', [i, j]), c.Items[j] <> k);
        end;
      end;

    end;
  end;

  c.Destroy;
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

procedure TTestRunnerList.RunTest(ACaller: TTestStorageMem);
var
  i1, i2: Integer;
begin
  Caller := ACaller;
  for i1 := 0 to 2 do begin
    for i2 := 0 to 3 do begin
      GrowStep := i1 * 4;
      case i2 of
        0: ShrinkStep := -1;
        1: ShrinkStep :=  1;
        2: ShrinkStep :=  4;
        3: ShrinkStep := 99;
      end;

      TestNew('');
      //(*
      TestSequence('XXX', [3,2,1,12,11,10,9,8,7,6,5,4]);
      TestSequence('XXX', [4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3]);

      TestSequenceEx('1', [1,2]);
      TestSequenceEx('1', [1,2,3,4,5,6,7,8,9,10,11,12]);
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
//      *)
    end;
  end;
end;

procedure TTestStorageMem.TestMem;
var t: TTestListMem;
begin
  t.RunTest(Self);
end;

procedure TTestStorageMem.TestMemSpecialized;
var t: TTestListMemSpecialized;
begin
  t.RunTest(Self);
end;

procedure TTestStorageMem.TestMemRound;
var t: TTestListRoundMem;
begin
  t.RunTest(Self);
end;

procedure TTestStorageMem.TestMemRoundSpecialized;
var t: TTestListRoundMemSpecialized;
begin
  t.RunTest(Self);
end;

procedure TTestStorageMem.TestMemPaged;
var
  t0: TTestListPagedMem0;
  t1: TTestListPagedMem1;
  t2: TTestListPagedMem2;
  t3: TTestListPagedMem3;
begin
  t1.RunTest(Self);
  t2.RunTest(Self);
  t3.RunTest(Self);
  t0.RunTest(Self);
end;

procedure TTestStorageMem.TestMemClass;
const
  TestVal: array[0..2] of Integer = (11, 22, 33);
var
  list: TLazDualCapacityList;
begin
  list := TLazDualCapacityList.Create(SizeOf(Integer));
  list.Add(@TestVal[0]);
  list.Insert(1, @TestVal[1]);
  list.Insert(0, @TestVal[2]);

  AssertEquals('', 33, PInteger(list.ItemPointer[0])^);
  AssertEquals('', 11, PInteger(list.ItemPointer[1])^);
  AssertEquals('', 22, PInteger(list.ItemPointer[2])^);

  list.Free;
end;

type
  TTestClass = specialize TLazGenDualCapacityList<Integer>;
procedure TTestStorageMem.TestMemSpecializedClass;
var
  list: TTestClass;
begin
  list := TTestClass.Create;
  list.Add(11);
  list.Insert(1, 22);
  list.Insert(0, 33);

  AssertEquals('', 33, list.Items[0]);
  AssertEquals('', 11, list.Items[1]);
  AssertEquals('', 22, list.Items[2]);

  list.Free;

end;



initialization

  RegisterTest(TTestStorageMem);
end.


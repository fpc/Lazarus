{
 /***************************************************************************
                               LazListClasses.pas
                               ---------------
                   Initial Revision  : May 2015


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit LazListClasses;

{$mode objfpc}{$H+}

(* * TLazStorageMem, TLazStorageList

    A list with capacity at both sides of the data, to allow quick insert/delete
    at both sides (first or last element).

    The "object" provides the base functionality. As on "object" it does not
    require an additional memory allocation

  * TLazRoundBufferList, TLazRoundBufferListMem

    A list that can wrap around at the boundaries of the capacity.

  * TLazPagedListMemBase

    A List that stores data in pages. Changing Capacity does not require to
    reallocate all memory

    Todo: None of the lists provides handling of managed types (ref counts, string, array)
    Todo: List with Gap
*)

interface
{no $INLINE off}

uses
  Classes, SysUtils, math, LazLoggerBase;

type

  TLazStorageMemShrinkProc = function(ARequired: Integer): Integer of object;
  TLazStorageMemGrowProc = function(ARequired: Integer): Integer of object;

  generic TLazListClassesItemSize<T> = object
  protected
    const
    ItemSize = SizeOf(T);
  end;

  TLazListClassesVarItemSize = object
  protected
    ItemSize: Integer;
  end;

  { TLazListClassesInternalMem }

  TLazListClassesInternalMem = object
  private
  const
    CNT_OFFS  = SizeOf(Pointer);
    DATA_OFFS = CNT_OFFS + (2 * SizeOf(Integer));
  private
    // Keep the size small, if no entries exist
    //   FMem:  FLowElemPointer: PByte; FCount, FCapacity: Integer; Array of <FItemSize
    FMem: PByte;

    function GetDataPointer: PByte; inline;
    function GetFirstItemPointer: PByte; // inline;
    procedure SetCapacity(AValue: Cardinal); inline;
    function GetCapacity: Cardinal; inline;
    function GetCount: Integer; inline;
    procedure SetCount(AValue: Integer); inline;
    procedure SetFirstItemPointer(AValue: PByte);
  public
    procedure Init; inline;
    procedure Alloc(AByteSize: Integer); inline;
    procedure Free; inline;
    function IsAllocated: Boolean; inline;

    property Capacity: Cardinal read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property DataPointer: PByte read GetDataPointer;
    property FirstItemPointer: PByte read GetFirstItemPointer write SetFirstItemPointer;
  end;

  { TLazDualCapacityListMemBase }

  generic TLazDualCapacityListMemBase<TPItemT, TSizeT> = object
  private
    FMem: TLazListClassesInternalMem;
    FItemSize: TSizeT; // May be zero size

    function GetItemPointer(Index: Integer): TPItemT; inline;
    procedure SetCapacity(AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
  protected
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;

    procedure SetCapacityEx(AValue, AnInsertPos, AnInsertSize: Integer);
    procedure InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc);
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc);
  public
    procedure Create;
    procedure Destroy;
    procedure InsertRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    procedure SwapEntries(AIndex1, AIndex2: Integer); inline;
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
  end;

  { TLazDualCapacityListMem }

  TLazDualCapacityListMem = object(specialize TLazDualCapacityListMemBase<Pointer, TLazListClassesVarItemSize>)
  public
    procedure Create(AnItemSize: Integer);
  end;

  { TLazGenDualCapacityListMem }

  generic TLazGenDualCapacityListMem<T> = object
  private type
    TItemSize = specialize TLazListClassesItemSize<T>;
    PT = ^T;
    TListType = specialize TLazDualCapacityListMemBase<PT, TItemSize>;
  private
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetItemPointer(Index: Integer): PT; inline;
    procedure SetCapacity(AValue: Integer); inline;
  protected
    FList: TListType;
    procedure InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc); inline;
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc); inline;
  public
    procedure Create;
    procedure Destroy;
    procedure InsertRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    function IndexOf(AnItem: T): integer;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: PT read GetItemPointer;
  end;

  { TLazRoundBufferListMemBase }

  generic TLazRoundBufferListMemBase<TPItemT, TSizeT> = object
  private
    // Keep the size small, if no entries exist
    // FMem:  FLowElemPointer: PByte; FCount, FCapacity_in_bytes: Integer; Array of <FItemSize
    FMem: TLazListClassesInternalMem;
    FItemSize: TSizeT; // May be zero size

    function GetItemPointer(Index: Integer): TPItemT; inline;
    procedure SetCapacity(AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
  protected
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity({%H-}ARequired: Integer): Integer;

    procedure SetCapacityEx(AValue, AnInsertPos, AnInsertSize: Integer);
    procedure InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc);
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc);
  public
    procedure Create;
    procedure Destroy;
    procedure InsertRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    procedure SwapEntries(AIndex1, AIndex2: Integer); inline;
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
  end;

  { TLazRoundBufferListMem }

  TLazRoundBufferListMem = object(specialize TLazRoundBufferListMemBase<Pointer, TLazListClassesVarItemSize>)
  public
    procedure Create(AnItemSize: Integer);
  end;

  { TLazGenRoundBufferListMem }

  generic TLazGenRoundBufferListMem<T> = object
  private type
    TItemSize = specialize TLazListClassesItemSize<T>;
    PT = ^T;
    TListType = specialize TLazRoundBufferListMemBase<PT, TItemSize>;
  private
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetItemPointer(Index: Integer): PT; inline;
    procedure SetCapacity(AValue: Integer); inline;
  protected
    FList: TListType;
    procedure InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc); inline;
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc); inline;
  public
    procedure Create;
    procedure Destroy;
    procedure InsertRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    function IndexOf(AnItem: T): integer;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: PT read GetItemPointer;
  end;

  { TLazFixedRoundBufferListMemBase }

  generic TLazFixedRoundBufferListMemBase<TPItemT, TSizeT> = object(specialize TLazRoundBufferListMemBase<TPItemT, TSizeT>)
  protected
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity({%H-}ARequired: Integer): Integer;
    property Mem: TLazListClassesInternalMem read FMem;
  public
    procedure Create(AItemSize: TSizeT; ACapacity: Integer);
    procedure InsertRows(AIndex, ACount: Integer); inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TLazPagedListMemBase }

  generic TLazPagedListMemBase<TPItemT, TSizeT> = object
  private type
    TPageType = specialize TLazFixedRoundBufferListMemBase<TPItemT, TSizeT>;
    PPageType = ^TPageType;
    TPageSize = specialize TLazListClassesItemSize<TPageType>;
    TPageListType = specialize TLazDualCapacityListMemBase<PPageType, TPageSize>;
    //TPageListType = specialize TLazRoundBufferListMemBase<PPageType, TPageSize>;
  private
    FGrowProc: TLazStorageMemGrowProc;
    FShrinkProc: TLazStorageMemShrinkProc;
    FExtraCapacityNeeded: Integer;
    FPages: TPageListType;
    FItemSize: TSizeT; // May be zero size
    FPageSiteExp: Integer;
    FPageSiteMask: Cardinal;
    FFirstPageEmpty: Integer;
    FCount: Integer;

    function GetPagePointer(Index: Integer): PPageType; inline;
    function GetPageSubIdx(Index: Integer): Integer; inline; // except for page=0
    function GetItemPageIdx(Index: Integer): Integer; inline;
    function GetItemPointer(Index: Integer): TPItemT; inline;
    procedure SetCapacity(AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function CalculateCount: Integer; //inline;
    function GetPageCount: Integer; inline;
    procedure MoveEntriesBetweenPages(SourcePage, TargetPage: PPageType;
      SourceIndex, TargetIndex, EntryCount: Integer);
    procedure SplitPage(ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer;
      AFillFirstCount: Integer = -1; AFillLastCount: Integer = -1;
      AExtraCapacityNeeded: Integer = 0);
    function BubbleEntriesToEnd(PageIndex, EntryCount: Integer): Boolean;
    function BubbleEntriesToStart(PageIndex, EntryCount: Integer): Boolean;
    function BubbleEntriesFromEnd(PageIndex, EntryCount: Integer): Boolean;
    function BubbleEntriesFromStart(PageIndex, EntryCount: Integer): Boolean;
  protected
    function GrowCapacity(ARequiredPages: Integer): Integer;
    function ShrinkCapacity(ARequiredPages: Integer): Integer;
    procedure InsertPages(AIndex, ACount: Integer; AExtraCapacityNeeded: Integer = 0); inline;
    procedure InsertFilledPages(AIndex, ACount: Integer;
      AFillFirstCount: Integer = -1; AFillLastCount: Integer = -1;
      AExtraCapacityNeeded: Integer = 0); inline;
    procedure DeletePages(AIndex, ACount: Integer); inline;
    property PagePointer[Index: Integer]: PPageType read GetPagePointer;
  public
    procedure Create(AnPageSiteExp: Integer);
    procedure  Destroy;
    procedure InsertRows(AIndex, ACount: Integer);
    procedure DeleteRows(AIndex, ACount: Integer);
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
    property PageCount: Integer read GetPageCount;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
    property GrowProc: TLazStorageMemGrowProc read FGrowProc write FGrowProc;
    property ShrinkProc: TLazStorageMemShrinkProc read FShrinkProc write FShrinkProc;
  end;

  TLazPagedListMemParent = specialize TLazPagedListMemBase<Pointer, TLazListClassesVarItemSize>;
    
  TLazPagedListMem = object(TLazPagedListMemParent)
  public
    procedure Create(AnPageSiteExp: Integer; AnItemSize: Integer);
  end;

  { TLazDualCapacityList }

  TLazDualCapacityList = class
  private
    FListMem: TLazDualCapacityListMem;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItemPointer(Index: Integer): Pointer;
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
  public
    constructor Create(AnItemSize: Integer);
    destructor Destroy; override;

    function Add(ItemPointer: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    //function IndexOf(ItemPointer: Pointer): Integer;
    procedure Insert(Index: Integer; ItemPointer: Pointer);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property ItemPointer[Index: Integer]: Pointer read GetItemPointer;
  end;

  { TLazGenDualCapacityList }

  generic TLazGenDualCapacityList<T> = class
  private type
    TListMem = specialize TLazGenDualCapacityListMem<T>;
    PT = ^T;
  private
    FListMem: TListMem;
    function Get(Index: Integer): T;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItemPointer(Index: Integer): PT;
    procedure Put(Index: Integer; AValue: T);
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Item: T): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function IndexOf(Item: T): Integer;
    procedure Insert(Index: Integer; Item: T);
    function Remove(Item: T): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property ItemPointer[Index: Integer]: PT read GetItemPointer;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

implementation

{ TLazListClassesInternalMem }

function TLazListClassesInternalMem.GetDataPointer: PByte;
begin
  if FMem = nil
  then Result := nil
  else Result := PByte(FMem+DATA_OFFS);
end;

function TLazListClassesInternalMem.GetFirstItemPointer: PByte;
begin
  if FMem = nil
  then Result := nil
  else Result := PPointer(FMem)[0];
end;

procedure TLazListClassesInternalMem.SetCapacity(AValue: Cardinal);
begin
  assert(FMem <> nil, 'TLazListClassesInternalMem.SetCapacity: FMem <> nil');
  PCardinal(FMem+CNT_OFFS)[1] := AValue;
end;

function TLazListClassesInternalMem.GetCapacity: Cardinal;
begin
  if FMem = nil
  then Result := 0
  else Result := PCardinal(FMem+CNT_OFFS)[1];
end;

function TLazListClassesInternalMem.GetCount: Integer;
begin
  if FMem = nil
  then Result := 0
  else Result := PInteger(FMem+CNT_OFFS)[0];
end;

procedure TLazListClassesInternalMem.SetCount(AValue: Integer);
begin
  assert(FMem <> nil, 'TLazListClassesInternalMem.SetCount: FMem <> nil');
  PInteger(FMem+CNT_OFFS)[0] := AValue;
end;

procedure TLazListClassesInternalMem.SetFirstItemPointer(AValue: PByte);
begin
  assert(FMem <> nil, 'TLazListClassesInternalMem.SetFirstItemPointer: FMem <> nil');
  PPointer(FMem)[0] := AValue;
end;

procedure TLazListClassesInternalMem.Init;
begin
  FMem := nil;
end;

procedure TLazListClassesInternalMem.Alloc(AByteSize: Integer);
begin
  Free;
  FMem := Getmem(DATA_OFFS + AByteSize);
end;

procedure TLazListClassesInternalMem.Free;
begin
  if FMem <> nil then
    Freemem(FMem);
  FMem := nil;
end;

function TLazListClassesInternalMem.IsAllocated: Boolean;
begin
  Result := FMem <> nil;
end;

{ TLazDualCapacityListMemBase }

function TLazDualCapacityListMemBase.GetItemPointer(Index: Integer): TPItemT;
begin
  if not FMem.IsAllocated
  then Result := nil
  else Result := TPItemT(FMem.FirstItemPointer + (Index * FItemSize.ItemSize));
end;

procedure TLazDualCapacityListMemBase.SetCapacity(AValue: Integer);
begin
  SetCapacityEx(AValue, 0, 0);
end;

function TLazDualCapacityListMemBase.GetCapacity: Integer;
begin
  Result := FMem.Capacity;
end;

function TLazDualCapacityListMemBase.GetCount: Integer;
begin
  Result := FMem.Count;
end;

function TLazDualCapacityListMemBase.GrowCapacity(ARequired: Integer): Integer;
begin
  Result := ARequired;
end;

function TLazDualCapacityListMemBase.ShrinkCapacity(ARequired: Integer): Integer;
begin
  if ARequired > Count * 2 then
    Result := Count
  else
    Result := -1;
end;

procedure TLazDualCapacityListMemBase.SetCapacityEx(AValue, AnInsertPos, AnInsertSize: Integer);
var
  NewMem: TLazListClassesInternalMem;
  Pos1, Cnt, NewCnt: Integer;
  PTarget, PSource: PByte;
begin
  Cnt := Count;
  NewCnt := Cnt + AnInsertSize;
  if AValue < NewCnt then
    AValue := NewCnt;

  if AValue = 0 then begin
    FMem.Free;
    exit;
  end;

  if AnInsertSize = 0 then begin;
    if (AValue = Capacity) then
      exit;
    AnInsertPos := 0;
  end;

  NewMem.Init;
  NewMem.Alloc(AValue * FItemSize.ItemSize);

  Pos1 := Cardinal(AValue-NewCnt) div 2;
  PTarget := NewMem.DataPointer + (Pos1 * FItemSize.ItemSize);

  NewMem.FirstItemPointer := PTarget;
  NewMem.Count := NewCnt;
  NewMem.Capacity := AValue;
  assert((NewMem.FirstItemPointer >= NewMem.DataPointer) and (NewMem.FirstItemPointer < NewMem.DataPointer + NewMem.Capacity * FItemSize.ItemSize), 'TLazDualCapacityListMemBase.InsertRowsEx: (NewMem.FirstItemPointer >= NewMem.NewMem+NewMem.DATA_OFFS) and (NewMem.FirstItemPointer < NewMem.NewMem+NewMem.DATA_OFFS + NewMem.Capacity * FItemSize.ItemSize)');

  if Cnt > 0 then begin
    PSource := FMem.FirstItemPointer;
    if AnInsertPos > 0 then
      Move(PSource^, PTarget^, (AnInsertPos * FItemSize.ItemSize));

    PSource := PSource + (AnInsertPos * FItemSize.ItemSize);
    PTarget := PTarget + ((AnInsertPos + AnInsertSize) * FItemSize.ItemSize);
    if AnInsertPos < Cnt then
      Move(PSource^, PTarget^, ((Cnt - AnInsertPos) * FItemSize.ItemSize));
  end;

  FMem.Free;
  FMem := NewMem;
end;

procedure TLazDualCapacityListMemBase.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc);
var
  Cnt, Cap, CntFreeFront, CntFreeEnd, Middle, i: Integer;
  CanFront, CanEnd: Boolean;
  PTarget, PSource: PByte;
begin
  if ACount = 0 then exit;
  if not assigned(AGrowProc) then
    AGrowProc := @GrowCapacity;

  Cnt := Count;
  Cap := Capacity;
  assert((ACount>0) and (AIndex>=0) and (AIndex<=Cnt), 'TLazDualCapacityListMem.InsertRows: (ACount>0) and (AIndex>=0) and (AIndex<=Cnt)');

  if Cnt + ACount > Cap then begin
    SetCapacityEx(AGrowProc(Cnt + ACount), AIndex, ACount);
    exit;
  end;

  CntFreeFront := (FMem.FirstItemPointer - FMem.DataPointer) div FItemSize.ItemSize;
  CntFreeEnd   := Cap - CntFreeFront - Cnt;
  CanFront := CntFreeFront >= ACount;
  CanEnd   := CntFreeEnd >= ACount;

  if not(CanFront or CanEnd)
  then begin
    i := AGrowProc(Cnt + ACount);
    if i > Cap then begin
      SetCapacityEx(AGrowProc(Cnt + ACount), AIndex, ACount);
      exit;
    end;

    Middle := 0;
  end
  else
    Middle := Cardinal(Cnt) div 2;

  if CanFront and ((AIndex < Middle) or (not CanEnd)) then begin
    // use space at front of list
    i := ACount;
    if (AIndex = Cnt) and (CntFreeFront-ACount > CntFreeEnd) then             // move all entries;
      i := i + Max(Cardinal(CntFreeFront-ACount-CntFreeEnd) div 2 - 1, 0);    // Make some room at the end of the list

    PSource := FMem.FirstItemPointer;
    PTarget := PSource - (i * FItemSize.ItemSize);
    if AIndex > 0 then
      Move(PSource^, PTarget^, AIndex * FItemSize.ItemSize);

    assert(PTarget >= FMem.DataPointer, 'TLazDualCapacityListMem.InsertRows: PTarget >= FMem+DATA_OFFS');
    FMem.FirstItemPointer := PTarget;
    FMem.Count := Cnt + ACount;
  end
  else
  if CanEnd then begin
    // use space at end of list
    if (AIndex = 0) and (CntFreeEnd-ACount > CntFreeFront) then             // move all entries;
      i := max(Cardinal(CntFreeEnd-ACount-CntFreeFront) div 2 - 1, 0)    // Make some room at the end of the list
    else
      i := 0;

    PSource := FMem.FirstItemPointer + (AIndex * FItemSize.ItemSize);
    PTarget := PSource + ((ACount + i) * FItemSize.ItemSize);
    if Cnt-AIndex > 0 then
      Move(PSource^, PTarget^, (Cnt-AIndex) * FItemSize.ItemSize);

    if i > 0 then begin
      assert(PSource + (i * FItemSize.ItemSize) >= FMem.DataPointer, 'TLazDualCapacityListMem.InsertRows: PSource + (i * FItemSize.ItemSize) >= FMem+DATA_OFFS');
      FMem.FirstItemPointer := PSource + (i * FItemSize.ItemSize);
    end;
    FMem.Count := Cnt + ACount;
  end
  else
  begin
 	// split to both ends
    assert((cap >= ACount) and (CntFreeFront> 0) and (CntFreeEnd > 0), 'TLazDualCapacityListMem.InsertRows: (cap >= ACount) and (CntFreeFront> 0) and (CntFreeEnd > 0)');
    i := Max(Cardinal(Cap-Cnt-ACount) div 2 - 1, 0);

    PSource := FMem.FirstItemPointer;
    PTarget := PSource - ((CntFreeFront - i) * FItemSize.ItemSize);
    if AIndex > 0 then
      Move(PSource^, PTarget^, AIndex * FItemSize.ItemSize);

    FMem.FirstItemPointer := PTarget;
    FMem.Count := Cnt + ACount;

    assert((ACount>CntFreeFront-i) and (ACount-(CntFreeFront - i)<=CntFreeEnd), 'TLazDualCapacityListMem.InsertRows: (ACount>CntFreeFront-i) and (ACount-(CntFreeFront - i)<=CntFreeEnd)');
    PSource := PSource + (AIndex * FItemSize.ItemSize);
    PTarget := PSource + ((ACount - (CntFreeFront - i)) * FItemSize.ItemSize);
    if Cnt-AIndex > 0 then
      Move(PSource^, PTarget^, (Cnt-AIndex) * FItemSize.ItemSize);
  end;

  assert((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity * FItemSize.ItemSize), 'TLazDualCapacityListMemBase.InsertRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity * FItemSize.ItemSize)');
end;

procedure TLazDualCapacityListMemBase.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
var
 Cnt, Middle, i: Integer;
  PTarget, PSource: PByte;
begin
  if ACount = 0 then exit;

  Cnt := Count;
  assert((ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt), 'TLazDualCapacityListMem.InsertRows: (ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt)');
  Middle := Cardinal(Cnt) div 2;

  if AIndex < Middle then begin
    // use space at front of list
    PSource := FMem.FirstItemPointer;
    PTarget := PSource + (ACount * FItemSize.ItemSize);
    if AIndex > 0 then
      Move(PSource^, PTarget^, AIndex * FItemSize.ItemSize);
    FMem.FirstItemPointer := PTarget;
    FMem.Count := Cnt - ACount;
  end
  else begin
    // use space at end of list
    i := AIndex + ACount;
    PSource := FMem.FirstItemPointer + (i * FItemSize.ItemSize);
    PTarget := PSource - (ACount * FItemSize.ItemSize);
    if Cnt-i > 0 then
      Move(PSource^, PTarget^, (Cnt-i) * FItemSize.ItemSize);
    FMem.Count := Cnt - ACount;
  end;
  if not assigned(AShrinkProc) then
    i := ShrinkCapacity(Count)
  else
    i := AShrinkProc(Count);
  if i >= 0 then
    SetCapacityEx(i, 0, 0)
  else
  if (Count = 0) then
    FMem.FirstItemPointer := FMem.DataPointer + (FMem.Capacity div 2) * FItemSize.ItemSize;
//debugln(['TLazRoundBufferListMemBase.DeleteRowsEx ', dbgs(FMem.IsAllocated), ' ',FMem.FirstItemPointer,' ', FMem.DataPointer,' ', FMem.Capacity]);
  assert((not FMem.IsAllocated) or ((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity * FItemSize.ItemSize)), 'TLazDualCapacityListMemBase.InsertRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity * FItemSize.ItemSize)');
end;

procedure TLazDualCapacityListMemBase.Create;
begin
  FMem.Init;
end;

procedure TLazDualCapacityListMemBase.Destroy;
begin
  FMem.Free;
end;

procedure TLazDualCapacityListMemBase.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TLazDualCapacityListMemBase.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

procedure TLazDualCapacityListMemBase.SwapEntries(AIndex1, AIndex2: Integer);
var
  t: PByte;
begin
  t := Getmem(FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex1))^, t^, FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex2))^, PByte(GetItemPointer(AIndex1))^, FItemSize.ItemSize);
  Move(t^, PByte(GetItemPointer(AIndex2))^, FItemSize.ItemSize);
  FreeMem(t);
end;

procedure TLazDualCapacityListMemBase.DebugDump;
var i : integer; s:string;
begin
  if fmem.IsAllocated then begin
    dbgout(['TLazFixedRoundBufferListMemBase.Dump ', FMem.Capacity, ' , ',FMem.Count,
    ' --- ', fmem.datapointer, ' , ',FMem.FirstItemPointer,' --- ', ': ']);
     s :='';
    for i := 0 to FMem.Count - 1 do s := s +dbgMemRange(itempointer[i], FItemSize.ItemSize )+ ', ';
    debugln(s);
  end
  else debugln(['TLazFixedRoundBufferListMemBase.Dump NONE']);
end;

{ TLazDualCapacityListMem }

procedure TLazDualCapacityListMem.Create(AnItemSize: Integer);
begin
  fItemSize.ItemSize := AnItemSize;
  inherited Create;
end;

{ TLazGenDualCapacityListMem }

function TLazGenDualCapacityListMem.GetCapacity: Integer;
begin
  Result := FList.GetCapacity;
end;

function TLazGenDualCapacityListMem.GetCount: Integer;
begin
  Result := FList.GetCount;
end;

function TLazGenDualCapacityListMem.GetItemPointer(Index: Integer): PT;
begin
  Result := FList.GetItemPointer(Index);
end;

procedure TLazGenDualCapacityListMem.SetCapacity(AValue: Integer);
begin
  FList.SetCapacity(AValue);
end;

procedure TLazGenDualCapacityListMem.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc);
begin
  FList.InsertRowsEx(AIndex, ACount, AGrowProc);
end;

procedure TLazGenDualCapacityListMem.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
begin
  FList.DeleteRowsEx(AIndex, ACount, AShrinkProc);
end;

procedure TLazGenDualCapacityListMem.Create;
begin
  FList.Create;
end;

procedure TLazGenDualCapacityListMem.Destroy;
begin
  FList.Destroy;
end;

procedure TLazGenDualCapacityListMem.InsertRows(AIndex, ACount: Integer);
begin
  FList.InsertRows(AIndex, ACount);
end;

procedure TLazGenDualCapacityListMem.DeleteRows(AIndex, ACount: Integer);
begin
  FList.DeleteRows(AIndex, ACount);
end;

function TLazGenDualCapacityListMem.IndexOf(AnItem: T): integer;
var
  p: PT;
begin
  Result := Count - 1;
  p := ItemPointer[Result];
  while Result >= 0 do begin
    if p^ = AnItem then exit;
    dec(p);
    dec(Result);
  end;
end;

{ TLazRoundBufferListMemBase }

function TLazRoundBufferListMemBase.GetItemPointer(Index: Integer): TPItemT;
var
  c: Integer;
begin
  if not FMem.IsAllocated
  then Result := nil
  else begin
    Result := TPItemT(FMem.FirstItemPointer + (Index * FItemSize.ItemSize));
    c := FMem.Capacity;
    if Result >= TPItemT(FMem.DataPointer + c)
      then Result := TPItemT(PByte(Result) - c);
  end;
end;

procedure TLazRoundBufferListMemBase.SetCapacity(AValue: Integer);
begin
  SetCapacityEx(AValue, 0, 0);
end;

function TLazRoundBufferListMemBase.GetCapacity: Integer;
begin
  Result := FMem.Capacity div FItemSize.ItemSize;
end;

function TLazRoundBufferListMemBase.GetCount: Integer;
begin
  Result := FMem.Count;
end;

function TLazRoundBufferListMemBase.GrowCapacity(ARequired: Integer): Integer;
begin
  Result := ARequired;
end;

function TLazRoundBufferListMemBase.ShrinkCapacity(ARequired: Integer): Integer;
begin
  //if ARequired > Count * 2 then
  //  Result := Count
  //else
    Result := -1;
end;

procedure TLazRoundBufferListMemBase.SetCapacityEx(AValue, AnInsertPos, AnInsertSize: Integer);
var
  NewMem: TLazListClassesInternalMem;
  Pos1, Cnt, NewCnt, siz, siz2: Integer;
  PTarget, PSource, m: PByte;
begin
  Cnt := Count;
  NewCnt := Cnt + AnInsertSize;
  if AValue < NewCnt then
    AValue := NewCnt;

  if AValue = 0 then begin
    FMem.Free;
    exit;
  end;

  if AnInsertSize = 0 then begin;
    if (AValue = Capacity) then
      exit;
    AnInsertPos := 0;
  end;

  NewMem.Init;
  NewMem.Alloc(AValue * FItemSize.ItemSize);

  Pos1 := Cardinal(AValue-NewCnt) div 2;
  PTarget := NewMem.DataPointer + (Pos1 * FItemSize.ItemSize);

  NewMem.FirstItemPointer := PTarget;
  NewMem.Count := NewCnt;
  NewMem.Capacity := AValue * FItemSize.ItemSize;
  assert((NewMem.FirstItemPointer >= NewMem.DataPointer) and (NewMem.FirstItemPointer < NewMem.DataPointer + NewMem.Capacity), 'TLazDualCapacityListMemBase.InsertRowsEx: (NewMem.FirstItemPointer >= NewMem.NewMem+NewMem.DATA_OFFS) and (NewMem.FirstItemPointer < NewMem.NewMem+NewMem.DATA_OFFS + NewMem.Capacity)');

  if Cnt > 0 then begin
    PSource := FMem.FirstItemPointer;
    m := FMem.DataPointer + FMem.Capacity;
    if AnInsertPos > 0 then begin
      siz := (AnInsertPos * FItemSize.ItemSize);
      siz2 := m - PSource;
      if siz > siz2 then begin
        Move(PSource^, PTarget^, siz2);
        Move(FMem.DataPointer^, (PTarget+siz2)^, siz - siz2);
      end
      else
        Move(PSource^, PTarget^, siz);
    end;

    if AnInsertPos < Cnt then begin
      PSource := PByte(ItemPointer[AnInsertPos]);
      PTarget := PTarget + ((AnInsertPos + AnInsertSize) * FItemSize.ItemSize);
      siz := ((Cnt - AnInsertPos) * FItemSize.ItemSize);
      siz2 := m - PSource;
      if siz > siz2 then begin
        Move(PSource^, PTarget^, siz2);
        Move(FMem.DataPointer^, (PTarget+siz2)^, siz - siz2);
      end
      else
        Move(PSource^, PTarget^, siz);
    end;
  end;

  FMem.Free;
  FMem := NewMem;
end;

procedure TLazRoundBufferListMemBase.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc);
var
  Cnt, Cap, Middle: Integer;
  siz, siz2: Integer;
  PTarget, PSource, m: PByte;
begin
  if ACount = 0 then exit;
  if not Assigned(AGrowProc) then
    AGrowProc := @GrowCapacity;

  Cnt := Count;
  Cap := FMem.Capacity;
  assert((ACount>0) and (AIndex>=0) and (AIndex<=Cnt), 'TLazDualCapacityListMem.InsertRows: (ACount>0) and (AIndex>=0) and (AIndex<=Cnt)');

  if Cnt + ACount > Capacity then begin
    SetCapacityEx(AGrowProc(Cnt + ACount), AIndex, ACount);
    exit;
  end;

  Middle := Cardinal(Cnt) div 2;
//debugln(['TLazRoundBufferListMemBase.InsertRowsEx ',AIndex,', ',ACount, ',   ', Cnt, ' ,',Cap,' # ', Middle, ' ',FMem.DataPointer,'  ', FMem.FirstItemPointer]);

  if (AIndex < Middle) or (AIndex = 0) then begin
    // use space at front of list
    PSource := FMem.FirstItemPointer;
    PTarget := PSource - (ACount * FItemSize.ItemSize);
    if PTarget < FMem.DataPointer then
      PTarget := PTarget + Cap;

    FMem.FirstItemPointer := PTarget;
    FMem.Count := Cnt + ACount;

    if AIndex > 0 then begin
      siz := AIndex * FItemSize.ItemSize;
      m := FMem.DataPointer + Cap;
      while siz > 0 do begin
        siz2 := Min(siz, m - PSource);
        siz2 := Min(siz2, m - PTarget);
        Move(PSource^, PTarget^, siz2);
        siz := siz - siz2;
        inc(PSource, siz2);
        if PSource >= m then
          PSource := PSource - Cap;
        inc(PTarget, siz2);
        if PTarget >= m then
          PTarget := PTarget - Cap;
      end;
    end;
  end
  else
  begin
    // use space at end of list
    PSource := PByte(ItemPointer[Cnt]);
    PTarget := PSource + (ACount * FItemSize.ItemSize);
    if PTarget > FMem.DataPointer + Cap then
      PTarget := PTarget - Cap;

    FMem.Count := Cnt + ACount;

    if AIndex < Cnt then begin
      siz := (Cnt-AIndex) * FItemSize.ItemSize;
      m := FMem.DataPointer;
      while siz > 0 do begin
        siz2 := Min(siz, PSource - m);
        siz2 := Min(siz2, PTarget - m);
        Move((PSource-siz2)^, (PTarget-siz2)^, siz2);
        siz := siz - siz2;
        dec(PSource, siz2);
        if PSource <= m then
          PSource := PSource + Cap;
        dec(PTarget, siz2);
        if PTarget <= m then
          PTarget := PTarget + Cap;
      end;
    end;
  end;
  assert((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity), 'TLazDualCapacityListMemBase.InsertRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity)');
end;

procedure TLazRoundBufferListMemBase.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
var
  Cnt, Cap, Middle, i, siz, siz2: Integer;
  PTarget, PSource, m: PByte;
begin
  if ACount = 0 then exit;
  //DebugLn(['TLazRoundBufferListMemBase.DeleteRowsEx ',AIndex,'  ',ACount, '  # ',FMem.DataPointer,  ', ',FMem.FirstItemPointer, ', ',FMem.Count]);

  Cnt := Count;
  Cap := FMem.Capacity;
  assert((ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt), 'TLazDualCapacityListMemBase.DeleteRowsEx: (ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt)');
  Middle := Cardinal(Cnt) div 2;

  if (AIndex < Middle) or (AIndex = 0) then begin
    // make space at front of list
    PTarget := PByte(ItemPointer[AIndex+ACount]);

    if AIndex > 0 then begin
      PSource := PByte(ItemPointer[AIndex]);
      siz := AIndex * FItemSize.ItemSize;
      m := FMem.DataPointer;
      while siz > 0 do begin
        siz2 := Min(siz, PSource - m);
        siz2 := Min(siz2, PTarget - m);
        Move((PSource-siz2)^, (PTarget-siz2)^, siz2);
        siz := siz - siz2;
        dec(PSource, siz2);
        if PSource <= m then
          PSource := PSource + Cap;
        dec(PTarget, siz2);
        if PTarget <= m then
          PTarget := PTarget + Cap;
      end;
      if PTarget = m + Cap then
        PTarget := m;
    end;

    FMem.FirstItemPointer := PTarget;
    FMem.Count := Cnt - ACount;
  end
  else begin
    // make space at end of list
    PSource := PByte(ItemPointer[AIndex+ACount]);
    PTarget := PByte(ItemPointer[AIndex]);

    if AIndex < Cnt-ACount then begin
      siz := (cnt - (AIndex+ACount)) * FItemSize.ItemSize;
      m := FMem.DataPointer + Cap;
      while siz > 0 do begin
        siz2 := Min(siz, m - PSource);
        siz2 := Min(siz2, m - PTarget);
        Move(PSource^, PTarget^, siz2);
        siz := siz - siz2;
        inc(PSource, siz2);
        if PSource >= m then
          PSource := PSource - Cap;
        inc(PTarget, siz2);
        if PTarget >= m then
          PTarget := PTarget - Cap;
      end;
    end;

    FMem.Count := Cnt - ACount;
  end;

  if not Assigned(AShrinkProc) then
    i := ShrinkCapacity(Count)
  else
    i := AShrinkProc(Count);
  if i >= 0 then
    SetCapacityEx(i, 0, 0)
  else
  if (Count = 0) then
    FMem.FirstItemPointer := FMem.DataPointer;
  assert((not FMem.IsAllocated) or ((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity)), 'TLazDualCapacityListMemBase.InsertRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity)');
end;

procedure TLazRoundBufferListMemBase.Create;
begin
  FMem.Init;
end;

procedure TLazRoundBufferListMemBase.Destroy;
begin
  FMem.Free;
end;

procedure TLazRoundBufferListMemBase.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TLazRoundBufferListMemBase.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

procedure TLazRoundBufferListMemBase.SwapEntries(AIndex1, AIndex2: Integer);
var
  t: PByte;
begin
  t := Getmem(FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex1))^, t^, FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex2))^, PByte(GetItemPointer(AIndex1))^, FItemSize.ItemSize);
  Move(t^, PByte(GetItemPointer(AIndex2))^, FItemSize.ItemSize);
  FreeMem(t);
end;

procedure TLazRoundBufferListMemBase.DebugDump;
var i : integer; s:string;
begin
  if fmem.IsAllocated then begin
    dbgout(['TLazFixedRoundBufferListMemBase.Dump ', FMem.Capacity, ' , ',FMem.Count,
    ' --- ', fmem.datapointer, ' , ',FMem.FirstItemPointer,' --- ', ': ']);
     s :='';
    for i := 0 to FMem.Count - 1 do s := s +dbgMemRange(itempointer[i], FItemSize.ItemSize )+ ', ';
    debugln(s);
  end
  else debugln(['TLazFixedRoundBufferListMemBase.Dump NONE']);
end;

{ TLazRoundBufferListMem }

procedure TLazRoundBufferListMem.Create(AnItemSize: Integer);
begin
  FItemSize.ItemSize := AnItemSize;
  inherited Create;
end;

{ TLazGenRoundBufferListMem }

function TLazGenRoundBufferListMem.GetCapacity: Integer;
begin
  Result := FList.GetCapacity;
end;

function TLazGenRoundBufferListMem.GetCount: Integer;
begin
  Result := FList.GetCount;
end;

function TLazGenRoundBufferListMem.GetItemPointer(Index: Integer): PT;
begin
  Result := FList.GetItemPointer(Index);
end;

procedure TLazGenRoundBufferListMem.SetCapacity(AValue: Integer);
begin
  FList.SetCapacity(AValue);
end;

procedure TLazGenRoundBufferListMem.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc);
begin
  FList.InsertRowsEx(AIndex, ACount, AGrowProc);
end;

procedure TLazGenRoundBufferListMem.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
begin
  FList.DeleteRowsEx(AIndex, ACount, AShrinkProc);
end;

procedure TLazGenRoundBufferListMem.Create;
begin
  FList.Create;
end;

procedure TLazGenRoundBufferListMem.Destroy;
begin
  FList.Destroy;
end;

procedure TLazGenRoundBufferListMem.InsertRows(AIndex, ACount: Integer);
begin
  FList.InsertRows(AIndex, ACount);
end;

procedure TLazGenRoundBufferListMem.DeleteRows(AIndex, ACount: Integer);
begin
  FList.DeleteRows(AIndex, ACount);
end;

function TLazGenRoundBufferListMem.IndexOf(AnItem: T): integer;
var
  p: PT;
begin
  Result := Count - 1;
  p := ItemPointer[Result];
  while Result >= 0 do begin
    if p^ = AnItem then exit;
    dec(p);
    dec(Result);
  end;
end;

{ TLazFixedRoundBufferListMemBase }

function TLazFixedRoundBufferListMemBase.GrowCapacity(ARequired: Integer): Integer;
begin
  assert(False, 'TLazFixedRoundBufferListMemBase.GrowCapacity: False');
  Result := ARequired;
end;

function TLazFixedRoundBufferListMemBase.ShrinkCapacity(ARequired: Integer): Integer;
begin
  Result := -1;
end;

procedure TLazFixedRoundBufferListMemBase.Create(AItemSize: TSizeT; ACapacity: Integer);
begin
  inherited Create;
  FItemSize := AItemSize;
  SetCapacity(ACapacity);
end;

procedure TLazFixedRoundBufferListMemBase.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TLazPagedListMemBase }

function TLazPagedListMemBase.GetPageSubIdx(Index: Integer): Integer;
begin
  Result := Cardinal(Index+FFirstPageEmpty) and FPageSiteMask;
end;

function TLazPagedListMemBase.GetPagePointer(Index: Integer): PPageType;
begin
  assert((Index >= 0) and (Index < PageCount), 'TLazPagedListMemBase.GetPageSubIdx: (Index >= 0) and (Index < PageCount)');
  Result := FPages.ItemPointer[Index];
end;

function TLazPagedListMemBase.GetItemPageIdx(Index: Integer): Integer;
begin
  Result := Cardinal(Index+FFirstPageEmpty) >> FPageSiteExp;
end;

function TLazPagedListMemBase.GetItemPointer(Index: Integer): TPItemT;
var
  p: PPageType;
  i, i2: Integer;
begin
  i2 := Index + FFirstPageEmpty;
  i := Cardinal(i2) >> FPageSiteExp;
  if i = 0 then begin
    p := FPages.ItemPointer[0];
    if p = nil then exit(nil);
    Result := p^.ItemPointer[{Cardinal(}Index{) and FPageSiteMask}]
  end
  else begin
    p := FPages.ItemPointer[i];
    if p = nil then exit(nil);
    Result := p^.ItemPointer[Cardinal(i2) and FPageSiteMask];
  end;
end;

procedure TLazPagedListMemBase.SetCapacity(AValue: Integer);
var
  i, PagesNeeded: Integer;
begin
  if AValue < 0 then
    AValue := 0;;
  i := (1 << FPageSiteExp);
  PagesNeeded := Cardinal(AValue + i - 1) div i;
  if i <= FPages.Count then
    exit;
  FPages.Capacity := PagesNeeded;
end;

function TLazPagedListMemBase.GetCapacity: Integer;
begin
  Result := FPages.Capacity * (1 << FPageSiteExp);
end;

function TLazPagedListMemBase.CalculateCount: Integer;
begin
  Result := (1 << FPageSiteExp) * Max(0, PageCount-2);
  if PageCount > 0 then
    Result := Result + PagePointer[0]^.Count;
  if PageCount > 1 then
    Result := Result + PagePointer[PageCount-1]^.Count;
end;

function TLazPagedListMemBase.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TLazPagedListMemBase.MoveEntriesBetweenPages(SourcePage, TargetPage: PPageType;
  SourceIndex, TargetIndex, EntryCount: Integer);
var
  MaxS, MaxD, Siz: Integer;
  PTarget, PSource: PByte;
begin
  if SourceIndex = -1 then
    SourceIndex := SourcePage^.Count - EntryCount;
  if TargetIndex = -1 then
    TargetIndex := TargetPage^.Count;

  TargetPage^.InsertRows(TargetIndex, EntryCount);

  PSource := PByte(SourcePage^.ItemPointer[SourceIndex]);
  PTarget := PByte(TargetPage^.ItemPointer[TargetIndex]);
  siz := EntryCount * FItemSize.ItemSize;

  MaxS := SourcePage^.Mem.DataPointer + SourcePage^.Mem.Capacity - PSource;
  MaxD := TargetPage^.Mem.DataPointer + SourcePage^.Mem.Capacity - PTarget;

  if MaxS < MaxD then begin
    if Siz <= MaxS then begin
      Move(PSource^, PTarget^, Siz);
    end
    else begin
      Move(PSource^, PTarget^, MaxS);
      PSource := PByte(SourcePage^.Mem.DataPointer);
      PTarget := PTarget + MaxS;
      Siz := Siz - MaxS;
      MaxD := MaxD - MaxS;

      if Siz <= MaxD then begin
        Move(PSource^, PTarget^, Siz);
      end
      else begin
        Move(PSource^, PTarget^, MaxD);
        Move((PSource+MaxD)^, (TargetPage^.Mem.DataPointer)^, Siz - MaxD);
      end;
    end;
  end
  else begin
    if Siz <= MaxD then begin
      Move(PSource^, PTarget^, Siz);
    end
    else begin
      Move(PSource^, PTarget^, MaxD);
      PTarget := PByte(TargetPage^.Mem.DataPointer);
      PSource := PSource + MaxD;
      Siz := Siz - MaxD;
      MaxS := MaxS - MaxD;

      if Siz <= MaxS then begin
        Move(PSource^, PTarget^, Siz);
      end
      else begin
        Move(PSource^, PTarget^, MaxS);
        Move((SourcePage^.Mem.DataPointer)^, (PTarget+MaxS)^, Siz - MaxS);
      end;
    end;
  end;

  SourcePage^.DeleteRows(SourceIndex, EntryCount);
end;

procedure TLazPagedListMemBase.SplitPage(ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer;
  AFillFirstCount: Integer; AFillLastCount: Integer; AExtraCapacityNeeded: Integer);
var
  c, PCap: Integer;
  CurPage, NewPage: PPageType;
begin
  //debugln(['TLazPagedListMemBase.SplitPage ',ASourcePageIdx, ', ', ASplitAtIdx, ', ',AnExtraPages]);
  PCap := Cardinal(1) << FPageSiteExp;
  assert(AFillFirstCount <= PCap - ASplitAtIdx, 'TLazPagedListMemBase.SplitPage: AFillFirstCount <= PCap - ASplitAtIdx');
  assert(AFillLastCount <= ASplitAtIdx + PCap - PagePointer[ASourcePageIdx]^.Count, 'TLazPagedListMemBase.SplitPage: AFillLastCount <= ASplitAtIdx');
  if AFillFirstCount < 0 then
    AFillFirstCount := PCap - ASplitAtIdx;
  if AFillLastCount < 0 then
    AFillLastCount := ASplitAtIdx;
  if ASplitAtIdx < (1 << (FPageSiteExp-1)) then begin
    InsertFilledPages(ASourcePageIdx, AnExtraPages+1, AFillFirstCount, -1, AExtraCapacityNeeded);
    NewPage := PagePointer[ASourcePageIdx];
    CurPage := PagePointer[ASourcePageIdx + AnExtraPages + 1];
    if ASplitAtIdx > 0 then
      MoveEntriesBetweenPages(CurPage, NewPage, 0, 0, ASplitAtIdx);
    if AFillLastCount > 0 then
      CurPage^.InsertRows(0, AFillLastCount);
  end
  else begin
    InsertFilledPages(ASourcePageIdx+1, AnExtraPages+1, -1, AFillLastCount, AExtraCapacityNeeded);
    CurPage := PagePointer[ASourcePageIdx];
    NewPage := PagePointer[ASourcePageIdx + AnExtraPages + 1];
    c := CurPage^.Count - ASplitAtIdx;
    if c > 0 then
      MoveEntriesBetweenPages(CurPage, NewPage, ASplitAtIdx, AFillLastCount, c);
    if AFillFirstCount > 0 then
      CurPage^.InsertRows(ASplitAtIdx, AFillFirstCount);
  end;
end;

function TLazPagedListMemBase.BubbleEntriesToEnd(PageIndex, EntryCount: Integer): Boolean;
var
  c, CurIdx, PCap: Integer;
  CurPage, PrevPage: PPageType;
begin
  //debugln(['--> TLazPagedListMemBase.BubbleEntriesToEnd ',PageIndex, ', ',EntryCount]);
  Result := False;
  PCap := Cardinal(1) << FPageSiteExp;
  CurIdx := PageCount-1;
  CurPage := PagePointer[CurIdx];

  c := EntryCount - (PCap - CurPage^.Count);
  if (c > 0) then begin
    InsertPages(CurIdx+1, 1);
    Result := True;
    CurPage := PagePointer[CurIdx];
    MoveEntriesBetweenPages(CurPage, PagePointer[CurIdx+1], -1, 0, c);
  end;

  while CurIdx > PageIndex do begin
    dec(CurIdx);
    PrevPage := PagePointer[CurIdx];
    MoveEntriesBetweenPages(PrevPage, CurPage, PrevPage^.Count-EntryCount, 0, EntryCount);
    CurPage := PrevPage;
  end;
end;

function TLazPagedListMemBase.BubbleEntriesToStart(PageIndex, EntryCount: Integer): Boolean;
var
  c, CurIdx, PCap: Integer;
  CurPage, NextPage: PPageType;
begin
  //debugln(['<-- TLazPagedListMemBase.BubbleEntriesToStart ',PageIndex, ', ',EntryCount]);
  Result := False;
  PCap := Cardinal(1) << FPageSiteExp;
  CurIdx := 0;
  CurPage := PagePointer[CurIdx];

  c := EntryCount - (PCap - CurPage^.Count);
  if (c > 0) then begin
    CurIdx := 1;
    inc(PageIndex);
    InsertPages(0, 1);
    Result := True;
    CurPage := PagePointer[CurIdx];
    MoveEntriesBetweenPages(CurPage, PagePointer[0], 0, 0, c);
  end;

  while CurIdx < PageIndex do begin
    inc(CurIdx);
    NextPage := PagePointer[CurIdx];
    MoveEntriesBetweenPages(NextPage, CurPage, 0, CurPage^.Count, EntryCount);
    CurPage := NextPage;
  end;
end;

function TLazPagedListMemBase.BubbleEntriesFromEnd(PageIndex, EntryCount: Integer): Boolean;
var
  MaxIdx: Integer;
  CurPage, NextPage: PPageType;
begin
  //debugln(['--< TLazPagedListMemBase.BubbleEntriesFromEnd ',PageIndex, ', ',EntryCount]);
  Result := False;
  MaxIdx := PageCount-2;
  if PageIndex = MaxIdx+1 then
    exit;
  CurPage := PagePointer[PageIndex];

  while PageIndex < MaxIdx do begin
    inc(PageIndex);
    NextPage := PagePointer[PageIndex];
    MoveEntriesBetweenPages(NextPage, CurPage, 0, -1, EntryCount);
    CurPage := NextPage;
  end;

  inc(PageIndex);
  NextPage := PagePointer[PageIndex]; // last page
  if EntryCount < NextPage^.Count then begin
    MoveEntriesBetweenPages(NextPage, CurPage, 0, -1, EntryCount);
  end
  else begin
    MoveEntriesBetweenPages(NextPage, CurPage, 0, -1, NextPage^.Count);
    DeletePages(PageIndex, 1);
    Result := True;
  end;
end;

function TLazPagedListMemBase.BubbleEntriesFromStart(PageIndex, EntryCount: Integer): Boolean;
var
  CurPage, PrevPage: PPageType;
begin
  //debugln(['>-- TLazPagedListMemBase.BubbleEntriesFromStart ',PageIndex, ', ',EntryCount]);
  Result := False;
  if PageIndex = 0 then
    exit;
  CurPage := PagePointer[PageIndex];

  while PageIndex > 1 do begin
    dec(PageIndex);
    PrevPage := PagePointer[PageIndex];
    MoveEntriesBetweenPages(PrevPage, CurPage, PrevPage^.Count - EntryCount, 0, EntryCount);
    CurPage := PrevPage;
  end;

  PrevPage := PagePointer[0];
  if EntryCount < PrevPage^.Count then begin
    MoveEntriesBetweenPages(PrevPage, CurPage, PrevPage^.Count - EntryCount, 0, EntryCount);
  end
  else begin
    MoveEntriesBetweenPages(PrevPage, CurPage, 0, 0, PrevPage^.Count);
    DeletePages(0, 1);
    Result := True;
  end;
end;

function TLazPagedListMemBase.GrowCapacity(ARequiredPages: Integer): Integer;
begin
  ARequiredPages := ARequiredPages + FExtraCapacityNeeded;
  if assigned(FGrowProc) then
    Result := FGrowProc(ARequiredPages)
  else
    Result := ARequiredPages;
end;

function TLazPagedListMemBase.ShrinkCapacity(ARequiredPages: Integer): Integer;
begin
  if assigned(FShrinkProc) then
    Result := FShrinkProc(ARequiredPages)
  else
  if ARequiredPages > PageCount * 2 then
    Result := PageCount
  else
    Result := -1;
end;

procedure TLazPagedListMemBase.InsertPages(AIndex, ACount: Integer;
  AExtraCapacityNeeded: Integer);
var
  i, c: Integer;
begin
  FExtraCapacityNeeded := AExtraCapacityNeeded;
  c := 1 << FPageSiteExp;
  FPages.InsertRowsEx(AIndex, ACount, @GrowCapacity);
  for i := AIndex to AIndex + ACount - 1 do
    PagePointer[i]^.Create(FItemSize, c);
end;

procedure TLazPagedListMemBase.InsertFilledPages(AIndex, ACount: Integer;
  AFillFirstCount: Integer; AFillLastCount: Integer; AExtraCapacityNeeded: Integer);
var
  i, c, h: Integer;
  p: PPageType;
begin
  FExtraCapacityNeeded := AExtraCapacityNeeded;
  FPages.InsertRowsEx(AIndex, ACount, @GrowCapacity);
  c := 1 << FPageSiteExp;
  h := AIndex + ACount - 1;
  if (ACount = 1) and (AFillFirstCount < 0) then
    AFillFirstCount := AFillLastCount;
  p := PagePointer[AIndex]; // pages are NOT a roundbuffer
  for i := AIndex to h do begin
    //p := PagePointer[i];
    p^.Create(FItemSize, c);
    if (i = AIndex) and (AFillFirstCount >= 0) then
      p^.InsertRows(0, AFillFirstCount)
    else
    if (i = h) and (AFillLastCount >= 0) then
      p^.InsertRows(0, AFillLastCount)
    else
      p^.InsertRows(0, c);
    inc(p);
  end;
end;

procedure TLazPagedListMemBase.DeletePages(AIndex, ACount: Integer);
var
  i: Integer;
begin
  //p := PagePointer[AIndex]; // pages are NOT a roundbuffer
  for i := AIndex to AIndex + ACount - 1 do
    PagePointer[i]^.Destroy;
  FPages.DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

procedure TLazPagedListMemBase.Create(AnPageSiteExp: Integer);
begin
  FPageSiteExp := AnPageSiteExp;
  FPageSiteMask := not(Cardinal(-1) << FPageSiteExp);
  FCount := 0;
  FFirstPageEmpty := 0;
  FPages.Create;
end;

procedure TLazPagedListMemBase.Destroy;
begin
  FPages.Destroy;
end;

procedure TLazPagedListMemBase.InsertRows(AIndex, ACount: Integer);
var
  ExtraPagesNeeded, CurPageAvail, CurPageCount, SubIndex, InvSubIndex: Integer;
  n1, n2, c, PCap, PCapHalf, InsertPageIdx: Integer;
  CurPage: PPageType;
begin
  assert((AIndex >= 0) and (AIndex <= FCount), 'TLazPagedListMemBase.InsertRows: (AIndex >= 0) and (AIndex <= FCount)');
  if ACount <= 0 then
    exit;
  FCount := FCount + ACount;

  PCap := Cardinal(1) << FPageSiteExp;

  InsertPageIdx := GetItemPageIdx(AIndex);
  if InsertPageIdx = 0
  then SubIndex := AIndex
  else SubIndex := GetPageSubIdx(AIndex);

  c := PageCount;
  if c > 0 then begin
    if InsertPageIdx = c then begin
      if ((c = 1) and (FFirstPageEmpty > 0)) or (FCount = 0) then begin
        InsertPageIdx := 0;
        SubIndex := AIndex;
      end;
    end;

    if InsertPageIdx < c then begin
      CurPage := PagePointer[InsertPageIdx];
      CurPageCount := CurPage^.Count;
      CurPageAvail := Min(PCap - CurPageCount, ACount);
      //debugln(['+++ ', AIndex,' ', ACount, ' # ', InsertPageIdx,' ',SubIndex, ' # ', CurPageAvail]);

      if CurPageAvail > 0 then begin
        PagePointer[InsertPageIdx]^.InsertRows(SubIndex, CurPageAvail);
        if SubIndex = CurPageCount then begin
          SubIndex := 0;
          inc(InsertPageIdx);
        end;
        ACount := ACount - CurPageAvail;
        if ACount = 0 then begin
          if InsertPageIdx <= 1 then
            FFirstPageEmpty := PCap - PagePointer[0]^.Count;
//DebugDump;
          assert(FCount = CalculateCount, 'TLazPagedListMemBase.InsertRows: FCount = CalculateCount');
          exit;
        end;
      end;
    end;
  end;

  ExtraPagesNeeded := Cardinal(ACount) >> FPageSiteExp;
  ACount := ACount - ExtraPagesNeeded * PCap;
  assert(ACount>=0, 'TLazPagedListMemBase.InsertRows: ACount>=0');

  if (InsertPageIdx = PageCount) and ((ACount > 0) or (ExtraPagesNeeded > 0)) then begin
    // Append, prev node must be full
    assert((PageCount=0) or (PagePointer[PageCount-1]^.Count=PCap), 'TLazPagedListMemBase.InsertRows: (PageCount=0) or (PagePointer[PageCount-1]^.Count=PCap)');
    assert(SubIndex = 0, 'TLazPagedListMemBase.InsertRows: SubIndex = 0');
    if ACount = 0 then
      InsertFilledPages(InsertPageIdx, ExtraPagesNeeded, -1, -1)
    else
      InsertFilledPages(InsertPageIdx, ExtraPagesNeeded+1, -1, ACount);
    if InsertPageIdx <= 1 then
      FFirstPageEmpty := PCap - PagePointer[0]^.Count;
//DebugDump;
    assert(FCount = CalculateCount, 'TLazPagedListMemBase.InsertRows: FCount = CalculateCount');
    exit;
  end;

  if ACount = 0 then begin
    if ExtraPagesNeeded > 0 then
      SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded-1, -1, -1);
    if InsertPageIdx <= 1 then
      FFirstPageEmpty := PCap - PagePointer[0]^.Count;
//DebugDump;
    assert(FCount = CalculateCount, 'TLazPagedListMemBase.InsertRows: FCount = CalculateCount');
    exit;
  end;

  PCapHalf := Cardinal(PCap) div 2;
  If InsertPageIdx < Cardinal(PageCount) div 2 then begin

    if ACount > PCapHalf then begin
      // insert new page, then "delete-bubble"
      { * insert 900 into:   [][]   [0..700|701..999]                      [][][][]
           1) SPLIT+add:      [][]   [0..700|+200]        [+700|701..999]   [][][][]
           2) bubble 100:     [>][>] [+100/100..800|+200] [+700|701..999]   [][][][]

         * insert 1900 into:  [][]   [0..700|701..999]                              [][][][]
           1) SPLIT+add:      [][]   [0..700|+200]        [+1000] [+700|701..999]   [][][][]
           2) bubble 100:     [>][>] [+100/100..800|+200] [+700|701..999]   [][][][]
      }
      n2 := Min(SubIndex, ACount);
      n1 := Min(PCap-SubIndex, ACount - n2);
      if ExtraPagesNeeded = 0 then begin
        SplitPage(InsertPageIdx, SubIndex, 0, n1, n2, 0);
        if ACount < SubIndex then
          MoveEntriesBetweenPages(PagePointer[InsertPageIdx], PagePointer[InsertPageIdx+1], -1, 0, SubIndex-ACount);
      end
      else begin
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded, n1, -1, 0);
        if ACount < SubIndex then begin
          CurPage := PagePointer[InsertPageIdx+1];
          CurPage^.DeleteRows(0, SubIndex-ACount); // TODO: avoid creating them in first
          MoveEntriesBetweenPages(PagePointer[InsertPageIdx], CurPage, -1, 0, SubIndex-ACount);
        end;
      end;
      if PCap - ACount > 0 then
        BubbleEntriesFromStart(InsertPageIdx, PCap - ACount);
    end
    else begin
      // normal insert
      {  * insert 100 into:    []     [1000]      [0..50|51..999]       [][][][]
           1) bubble 100(prev) [<]    [0..900]    [0..50|51..899]       [][][][]
           2) move 50          [<]    [0..950]    [51..899]       [][][][]
           3) add:             []     [0.950,+50] [+50,51..999]  [][][][]

         * insert 100 into:    []     [0..500|501..999]       [][][][]
           1) bubble 100       [<]    [0..400|401..899]       [][][][]
           2) add:             []     [0..400,+100,501..999]  [][][][]

           * insert 1100 into: []     [1000]      [0..50|51..999]                 [][][][]
           1) bubble (prev)    [<]    [0..900]    [0..50|51..899]                 [][][][]
           2) move 50          [<]    [0..950]    [51..899]                       [][][][]
           3) INS(SPLIT at 0)  [<]    [0..950]    [+1000]         [51..899]       [][][][]
           3)  add:            []     [0.950,+50] [+1000]         [+50,51..999]  [][][][]
      }
      if ACount > SubIndex then begin
        if InsertPageIdx = 0 then begin
          InsertPages(0, 1);
          inc(InsertPageIdx);
        end
        else
        if BubbleEntriesToStart(InsertPageIdx-1, ACount) then
          inc(InsertPageIdx);
        CurPage := PagePointer[InsertPageIdx-1];
        if SubIndex > 0 then
          MoveEntriesBetweenPages(PagePointer[InsertPageIdx], CurPage, 0, -1, SubIndex);
        CurPage^.InsertRows(CurPage^.Count, ACount - SubIndex);
        ACount := SubIndex;
        SubIndex := 0;
      end
      else begin
        if BubbleEntriesToStart(InsertPageIdx, ACount) then
          inc(InsertPageIdx);
        SubIndex := SubIndex - ACount;
      end;

      if (ExtraPagesNeeded > 0) then
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded - 1, -1, SubIndex+ACount, 0)
      else
      if ACount > 0 then
        PagePointer[InsertPageIdx]^.InsertRows(SubIndex, ACount);
    end;
  end

  else begin
    InvSubIndex := PCap - SubIndex;
    if ACount > PCapHalf then begin
      // insert new page, then "delete-bubble"
      // Same as above, but bubble to end
      n1 := Min(InvSubIndex, ACount);
      n2 := Min(SubIndex, ACount - n1);
      if ExtraPagesNeeded = 0 then begin
        SplitPage(InsertPageIdx, SubIndex, 0, n1, n2, 0);
        if ACount < InvSubIndex then
          MoveEntriesBetweenPages(PagePointer[InsertPageIdx+1], PagePointer[InsertPageIdx], 0, -1, InvSubIndex-ACount);
      end
      else begin
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded, -1, n2, 0);
        if ACount < InvSubIndex then begin
          CurPage := PagePointer[InsertPageIdx+ExtraPagesNeeded];
          CurPage^.DeleteRows(0, InvSubIndex-ACount); // TODO: avoid creating them in first
          MoveEntriesBetweenPages(PagePointer[InsertPageIdx+ExtraPagesNeeded+1], CurPage, 0, -1, InvSubIndex-ACount);
        end;
      end;
      if PCap - ACount > 0 then
        BubbleEntriesFromEnd(InsertPageIdx+ExtraPagesNeeded+1, PCap - ACount);
    end
    else begin
      // Same as above, but bubble to end
      // normal insert
      if ACount > InvSubIndex then begin
        c := PageCount;
        if InsertPageIdx = c-1 then
          InsertPages(c, 1)
        else
          BubbleEntriesToEnd(InsertPageIdx+1, ACount);
        CurPage := PagePointer[InsertPageIdx+1];
        MoveEntriesBetweenPages(PagePointer[InsertPageIdx], CurPage, -1, 0, InvSubIndex);
        CurPage^.InsertRows(0, ACount - InvSubIndex);
        ACount := InvSubIndex;
      end
      else begin
        BubbleEntriesToEnd(InsertPageIdx, ACount);
      end;

      CurPage := PagePointer[InsertPageIdx];
      if (ExtraPagesNeeded > 0) then begin
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded - 1, -1, SubIndex+ACount, 0)
      end
      else
        CurPage^.InsertRows(SubIndex, ACount);
    end;

  end;
  FFirstPageEmpty := PCap - PagePointer[0]^.Count;

//DebugDump;
  assert(FCount = CalculateCount, 'TLazPagedListMemBase.InsertRows: FCount = CalculateCount');
end;

procedure TLazPagedListMemBase.DeleteRows(AIndex, ACount: Integer);
var
  PCap, PCapHalf, c: Integer;
  DelPageIdx, SubIndex, DelCountCur, DelCountNext, DelPagesNeeded: Integer;
  CurPage, NextPage: PPageType;
begin
  assert((AIndex >= 0) and (AIndex + ACount <= FCount), 'TLazPagedListMemBase.DeleteRows: (AIndex >= 0) and (AIndex + ACount <= FCount)');
  if ACount <= 0 then
    exit;
  FCount := FCount - ACount;

  PCap := Cardinal(1) << FPageSiteExp;
  PCapHalf := Cardinal(PCap) div 2;
  DelPageIdx := GetItemPageIdx(AIndex);
  if DelPageIdx = 0
  then SubIndex := AIndex
  else SubIndex := GetPageSubIdx(AIndex);


  CurPage := PagePointer[DelPageIdx];

  DelCountNext := Max(0, ACount - (CurPage^.Count - SubIndex));
  DelCountCur := ACount - DelCountNext;
  DelPagesNeeded := Cardinal(DelCountNext) >> FPageSiteExp;
  c := DelPagesNeeded * PCap;
  ACount := ACount - c;
  DelCountNext := DelCountNext - c;
  //debugln(['!!! ', AIndex,' ',ACount,' // ',DelPageIdx,'(',PageCount,') ', SubIndex, ' # ',  DelCountCur,', ',DelCountNext, ' -- ',CurPage^.Count, ', ',DelPagesNeeded, ' ## ',FFirstPageEmpty, ' # ',FCount]);
  assert(ACount=DelCountCur+DelCountNext, 'TLazPagedListMemBase.DeleteRows: ACount=DelCountCur+DelCountNext');


  if DelCountNext > 0 then begin
    NextPage := PagePointer[DelPageIdx+DelPagesNeeded+1];
    if DelCountNext = NextPage^.Count then begin
      assert(DelPageIdx+DelPagesNeeded+1 = PageCount-1, 'TLazPagedListMemBase.DeleteRows: DelPageIdx+DelPagesNeeded+1 = PageCount-1');
      inc(DelPagesNeeded);
      ACount := ACount - DelCountNext;
      DelCountNext := 0;
    end
    else begin
      NextPage^.DeleteRows(0, DelCountNext);
      if DelPageIdx+DelPagesNeeded+1 = PageCount-1 then begin
        ACount := ACount - DelCountNext;
        DelCountNext := 0;
      end;
    end;
  end;

  if DelCountCur = CurPage^.Count then begin
    inc(DelPagesNeeded);
    ACount := ACount - DelCountCur;
    DelCountCur := DelCountNext;
    DelCountNext := 0;

    DeletePages(DelPageIdx , DelPagesNeeded);
  end
  else begin
    CurPage^.DeleteRows(SubIndex, DelCountCur);
    if (DelPageIdx = 0) or (DelPageIdx = PageCount-1) then begin
      ACount := ACount - DelCountCur;
      DelCountCur := 0;
      assert(ACount<PCap, 'TLazPagedListMemBase.DeleteRows: ACount<PCap');
    end
    else
    if ACount >= PCap then begin
      assert((DelCountNext>0) and (DelCountNext<PCap), 'TLazPagedListMemBase.DeleteRows: (DelCountNext>0) and (DelCountNext<PCap)');
      CurPage := PagePointer[DelPageIdx+DelPagesNeeded+1];
      c := CurPage^.Count;
      if c > 0 then
        MoveEntriesBetweenPages(CurPage, PagePointer[DelPageIdx], 0, -1, c);

      DelCountCur := DelCountCur + DelCountNext - PCap;
      ACount := DelCountCur;
      DelCountNext := 0;
      assert((DelCountCur >= 0) and (DelCountCur <= PCap), 'TLazPagedListMemBase.DeleteRows: (DelCountCur >= 0) and (DelCountCur <= PCap)');
      assert(PagePointer[DelPageIdx+DelPagesNeeded+1]^.Count = 0, 'TLazPagedListMemBase.DeleteRows: PagePointer[DelPageIdx+DelPagesNeeded+1]^.Count = 0');
      inc(DelPagesNeeded);
    end;

    if DelPagesNeeded > 0 then
      DeletePages(DelPageIdx + 1 , DelPagesNeeded);

    if DelCountCur = 0 then begin
      DelCountCur := DelCountNext;
      DelCountNext := 0;
      inc(DelPageIdx);
    end;
  end;
  assert(ACount=DelCountCur+DelCountNext, 'TLazPagedListMemBase.DeleteRows: ACount=DelCountCur+DelCountNext');


  if ACount > PCapHalf then begin
  // ***** bubble out
    if Cardinal(DelPageIdx) < Cardinal(PageCount) div 2 then begin
      // to start
      assert((DelCountNext = 0) or (DelPageIdx>0), 'TLazPagedListMemBase.DeleteRows: (DelCountNext = 0) or (DelPageIdx>0)');
      if DelCountNext > 0 then
        MoveEntriesBetweenPages(PagePointer[DelPageIdx], PagePointer[DelPageIdx+1], -1, 0, DelCountNext);
      if PCap-DelCountCur > 0 then
        if BubbleEntriesToStart(DelPageIdx, PCap-ACount) then
          inc(DelPageIdx);
    end
    else begin
      // to end
      if DelCountNext > 0 then begin
        BubbleEntriesToEnd(DelPageIdx+1, PCap-ACount);
        CurPage := PagePointer[DelPageIdx];
        MoveEntriesBetweenPages(CurPage, PagePointer[DelPageIdx+1], -1, 0, PagePointer[DelPageIdx]^.Count);
      end
      else
        BubbleEntriesToEnd(DelPageIdx, PCap-ACount);
    end;
    if PagePointer[DelPageIdx]^.Count = 0 then
      DeletePages(DelPageIdx, 1)
    else
      assert((DelPageIdx=0) or (DelPageIdx=PageCount-1), 'TLazPagedListMemBase.DeleteRows: (DelPageIdx=0) or (DelPageIdx=PageCount-1)');
  end

  else
  if ACount > 0 then begin
  // ***** bubble in
    if Cardinal(DelPageIdx) < Cardinal(PageCount) div 2 then begin
      // from start
      if DelCountNext > 0 then begin
        if PageCount = 2 then begin
          if PagePointer[DelPageIdx+1]^.Count = 0 then
            DeletePages(DelPageIdx+1, 1);
          // else do nothing last page has data and can be kept
        end
        else begin
          assert(PagePointer[DelPageIdx+1]^.Count > 0, 'TLazPagedListMemBase.DeleteRows: NextPage^.Count > 0');
          CurPage := PagePointer[DelPageIdx];
          DelCountNext := Min(DelCountNext, CurPage^.Count);
          if DelCountNext > 0 then begin
            MoveEntriesBetweenPages(CurPage, PagePointer[DelPageIdx+1], -1, 0, DelCountNext);
            assert((PagePointer[DelPageIdx+1]^.Count = PCap) or (CurPage^.Count = 0), 'TLazPagedListMemBase.DeleteRows: (PagePointer[DelPageIdx+1]^.Count = PCap) or (CurPage^.Count = 0)');
          end;
        end;
      end;
      if PagePointer[DelPageIdx]^.Count = 0 then begin
        assert(DelPageIdx = 0, 'TLazPagedListMemBase.DeleteRows: DelPageIdx = 0');
        DeletePages(DelPageIdx, 1);
      end
      else
      if DelPageIdx > 0 then
        BubbleEntriesFromStart(DelPageIdx, ACount);
    end
    else begin
      // from end
      if DelCountNext > 0 then begin
        assert(PageCount > 2, 'TLazPagedListMemBase.DeleteRows: PageCount > 2');
        CurPage := PagePointer[DelPageIdx+1];
        DelCountNext := Min(DelCountNext, CurPage^.Count);
        if DelCountNext > 0 then begin
          MoveEntriesBetweenPages(CurPage, PagePointer[DelPageIdx], 0, -1, DelCountCur);
          assert((PagePointer[DelPageIdx]^.Count = PCap) or (CurPage^.Count = 0), 'TLazPagedListMemBase.DeleteRows: (PagePointer[DelPageIdx]^.Count = PCap) or (CurPage^.Count = 0)');
        end;
        inc(DelPageIdx);
      end;
      if PagePointer[DelPageIdx]^.Count = 0 then begin
        assert(DelPageIdx = PageCount-1, 'TLazPagedListMemBase.DeleteRows: DelPageIdx = PageCount');
        DeletePages(DelPageIdx, 1);
      end
      else
      if DelPageIdx < PageCount-1 then
        BubbleEntriesFromEnd(DelPageIdx, ACount);
    end;
  end;

  if FCount > 0 then
    FFirstPageEmpty := PCap - PagePointer[0]^.Count
  else
    FFirstPageEmpty := 0;
  assert(FCount = CalculateCount, 'TLazPagedListMemBase.DeleteRows: FCount = CalculateCount');
end;

procedure TLazPagedListMemBase.DebugDump;
var i : integer;
begin
  if fpages.fmem.IsAllocated then begin
    debugln(['PAGED .Dump ', fpages.fmem.Capacity, ' , ',fpages.fmem.Count,' -- ',FCount,' = ',CalculateCount ,'  ', FFirstPageEmpty, ': ']);
    for i := 0 to fpages.Count - 1 do FPages.ItemPointer[i]^.DebugDump;
  end
  else debugln(['PAGED .Dump NONE']);
end;

{ TLazPagedListMem }

procedure TLazPagedListMem.Create(AnPageSiteExp: Integer; AnItemSize: Integer);
begin
  FItemSize.ItemSize := AnItemSize;
  inherited Create(AnPageSiteExp);
end;

{ TLazDualCapacityList }

function TLazDualCapacityList.GetCapacity: Integer;
begin
  Result := FListMem.Capacity;
end;

function TLazDualCapacityList.GetCount: Integer;
begin
  Result := FListMem.Count;
end;

function TLazDualCapacityList.GetItemPointer(Index: Integer): Pointer;
begin
  Result := FListMem.ItemPointer[Index];
end;

procedure TLazDualCapacityList.SetCapacity(AValue: Integer);
begin
  FListMem.Capacity := AValue;
end;

procedure TLazDualCapacityList.SetCount(AValue: Integer);
begin
  if AValue > FListMem.Count then
    FListMem.InsertRows(FListMem.Count, AValue - FListMem.Count)
  else
  if AValue < FListMem.Count then
    FListMem.DeleteRows(AValue, FListMem.Count - AValue);
end;

constructor TLazDualCapacityList.Create(AnItemSize: Integer);
begin
  FListMem.Create(AnItemSize);
end;

destructor TLazDualCapacityList.Destroy;
begin
  FListMem.Destroy;
end;

function TLazDualCapacityList.Add(ItemPointer: Pointer): Integer;
begin
  Result := FListMem.Count;
  FListMem.InsertRows(Result, 1);
  Move(ItemPointer^, FListMem.ItemPointer[Result]^, FListMem.FItemSize.ItemSize);
end;

procedure TLazDualCapacityList.Clear;
begin
  FListMem.Capacity := 0;
end;

procedure TLazDualCapacityList.Delete(Index: Integer);
begin
  FListMem.DeleteRows(Index, 1);
end;

procedure TLazDualCapacityList.Insert(Index: Integer; ItemPointer: Pointer);
begin
  FListMem.InsertRows(Index, 1);
  Move(ItemPointer^, FListMem.ItemPointer[Index]^, FListMem.FItemSize.ItemSize);
end;

{ TLazGenDualCapacityList }

function TLazGenDualCapacityList.GetCapacity: Integer;
begin
  Result := FListMem.Capacity;
end;

function TLazGenDualCapacityList.Get(Index: Integer): T;
begin
  Result := FListMem.ItemPointer[Index]^;
end;

function TLazGenDualCapacityList.GetCount: Integer;
begin
  Result := FListMem.Count;
end;

function TLazGenDualCapacityList.GetItemPointer(Index: Integer): PT;
begin
  Result := FListMem.ItemPointer[Index];
end;

procedure TLazGenDualCapacityList.Put(Index: Integer; AValue: T);
begin
  FListMem.ItemPointer[Index]^ := AValue;
end;

procedure TLazGenDualCapacityList.SetCapacity(AValue: Integer);
begin
  FListMem.Capacity := AValue;
end;

procedure TLazGenDualCapacityList.SetCount(AValue: Integer);
begin
  if AValue > FListMem.Count then
    FListMem.InsertRows(FListMem.Count, AValue - FListMem.Count)
  else
  if AValue < FListMem.Count then
    FListMem.DeleteRows(AValue, FListMem.Count - AValue);
end;

constructor TLazGenDualCapacityList.Create;
begin
  FListMem.Create;
end;

destructor TLazGenDualCapacityList.Destroy;
begin
  FListMem.Destroy;
end;

function TLazGenDualCapacityList.Add(Item: T): Integer;
begin
  Result := FListMem.Count;
  FListMem.InsertRows(Result, 1);
  FListMem.ItemPointer[Result]^ := Item;
end;

procedure TLazGenDualCapacityList.Clear;
begin
  FListMem.Capacity := 0;
end;

procedure TLazGenDualCapacityList.Delete(Index: Integer);
begin
  FListMem.DeleteRows(Index, 1);
end;

function TLazGenDualCapacityList.IndexOf(Item: T): Integer;
begin
  Result := FListMem.IndexOf(Item);
end;

procedure TLazGenDualCapacityList.Insert(Index: Integer; Item: T);
begin
  FListMem.InsertRows(Index, 1);
  FListMem.ItemPointer[Index]^ := Item;
end;

function TLazGenDualCapacityList.Remove(Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

end.


{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Initial Revision  : May 2015

 This unit provides generics for list classes. The lists are provided as
 "object" and "class" version.
 Each list can be specialized to either hold items of a given type (specialized
 to type at compile time) or to hold untyped data the size of with can be
 specified at runtime as argument to the constructor.

 *** The lists are currently not suitable for managed types, such as string. ***

 ****************************************
 * TLazShiftBufferList

   This list is designed for shift/unshift/pop/push operations.

   The first list element is not forced to the start of the allocated memory.
   Instead it allows a gap (some of the over-allocated memory / List.Capacity)
   in front of the first element.

   Therefore elements can be added/removed at either the begin or end of the
   list, withouth any need to move the other elemnts in the list.

 ****************************************
 * TLazRoundBufferList

   The first element of the list can be anywhere within the allocated memory
   (capacity). If the elements of the list reach the end of the memory, the list
   will wrap arount and continues in the available memory at the start of the
   allocation.

   This list can be used for a first-in, first-out queue. If the list does never
   exceed the size set by its capacity, then elements can be pushed/shifted
   from/to the list without any need to reallocate or move entries to new
   locations.

 ****************************************
 * TLazPagedListMem

   This list organize its data into pages of fixed size. If the list grows or
   shrinks it can allocate extra pages or free existing pages. It does not need
   to reallocate its entire memory.
   This means that growing needs less extra memory than conventional lists.
   The page size is specified in the call to the constructor. The size has to be
   a power of 2. The constructor takes the exponent as argument. (e.g. an
   argument of "10" will give a size of 1024)

   The list also acts like a TLazShiftBufferList.


 *****************************************************************************
 * Variants of the above lists.

   Note about "object" variants:
     "object"s are stored on the stack, as such the memory of the object itself
     is freed when the variable goes out of scope. The objects do however
     allocate additional memory on the heap.
   => So it is necessary to call Destroy.
   => It is also necessary to call Create. (Unless you can gurantee that the
      memory of the object has been zero filled)

   The samples below show the available variants of the above list.
   The constructor for each sample is included.


 ****************************************
 * Helpers for specializing the variants

   Either of the following can be specified to generics that take a "TSizeT"

   type
   generic TLazListClassesItemSize<T> = object

     Used for specializing lists with a fixed item size.
     The size will be:
       sizeof(T)

   type
   TLazListClassesVarItemSize = object

     Used for specializing list with a configurable item size. The size must
     be set in the constructor and can not be changed after this.
     When using this, you need to add a constructor setting:
       fItemSize.ItemSize := ASize; // "ASize" is your size

 ****************************************
 * Variants for TLazShiftBufferList

   generic TLazShiftBufferListObjBase<TPItemT, TSizeT> = object
     procedure Create;

     This is the base for all other variants. Usually you do not use this
     directly, but use one of the other variants below.
     TSizeT: See above
     TPItemT: The type of the item.
              Can be "Pointer" or any type. Must match TSizeT

   TLazShiftBufferListObj = object
     procedure Create(AnItemSize: Integer);

     The list as an "object"
     Use the "ItemPointer" method to get a pointer to the item-data.
     The pointer is only valid while there are no insert/delete/capacity operations.

   generic TLazShiftBufferListObjGen<T> = object
     procedure Create;

     The list as a generic for a typed "object"
     This list an "Items" method to access the list entries.

   TLazShiftBufferList = class
     procedure Create(AnItemSize: Integer);

     The pointer-list as an "class"

   generic TLazShiftBufferListGen<T> = class
     procedure Create;

     The typed-list as an "class"

 ****************************************
 * Variants for TLazRoundBufferList

   generic TLazRoundBufferListObjBase<TPItemT, TSizeT> = object
     procedure Create;

   TLazRoundBufferListObj = object
     procedure Create(AnItemSize: Integer);

   generic TLazRoundBufferListObjGen<T> = object
     procedure Create;


 ****************************************
 * Variants for TLazPagedListObj

   generic TLazPagedListObjBase<TPItemT, TSizeT> = object
     procedure Create(APageSizeExp: Integer);
     // pagesize := 2 ^^ APageSizeExp

   TLazPagedListObj = object
    procedure Create(APageSizeExp: Integer; AnItemSize: Integer);

 ********************************************************************************
 * Notes

 * MoveRows(From, To, Cnt)

   - Can handle overlaps
   - The Data in the "from" block will be undefined afterwards
     (except for overlaps with "To")

}

unit LazListClasses;

{$mode objfpc}{$H+}

interface
{$IFDEF LazListClassTestCase}
  {$INLINE off}
  {$STACKFRAMES on}
  {$ASSERTIONS on}
  {$IMPLICITEXCEPTIONS off} // use with debugln
{$ELSE}
  {$INLINE on}
  {$STACKFRAMES off}
  {$ASSERTIONS off}
{$ENDIF}

uses
  Classes, SysUtils, math, LazLoggerBase;

type

  TLazStorageMemShrinkProc = function(ARequired: Integer): Integer of object;
  TLazStorageMemGrowProc = function(ARequired: Integer): Integer of object;

  (* TLazListClassesItemSize
     Helper to specialize lists for a give type
  *)
  generic TLazListClassesItemSize<T> = object
  protected
    const
    ItemSize = SizeOf(T);
  end;

  (* TLazListClassesVarItemSize
     Helper to specialize lists for runtime specified size "TList.Create(ASize)"
  *)
  TLazListClassesVarItemSize = object
  protected
    ItemSize: Integer;
  end;

  { TLazListClassesInternalMem
    Internally used helper object
  }

  TLazListClassesInternalMem = object
  protected type
    TMemRecord = record
      FirstItem: record
        case integer of
          1: (Ptr: PByte;);
          2: (Idx: Integer;);
        end;
      Count: Integer;
      Capacity: Cardinal;
      Data: record end; // The address for the first byte of data. This is a dummy field
    end;
    PMemRecord = ^TMemRecord;
  private
    FMem: PMemRecord;

    function GetCapacityFast: Cardinal; inline;
    function GetDataPointer: PByte; inline;
    function GetFirstItemIndex: Integer; inline;
    function GetFirstItemPointer: PByte;  inline;
    procedure SetCapacity(AValue: Cardinal); inline;
    function GetCapacity: Cardinal; inline;
    function GetCount: Integer; inline;
    procedure SetCount(AValue: Integer); inline;
    procedure SetFirstItemIndex(AValue: Integer);
    procedure SetFirstItemPointer(AValue: PByte); inline;
  protected
    property CapacityFast: Cardinal read GetCapacityFast;
  public
    procedure Init; inline;
    procedure Alloc(AByteSize: Integer); inline;
    procedure Free; inline;
    function IsAllocated: Boolean; inline;

    property Capacity: Cardinal read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property DataPointer: PByte read GetDataPointer;
    // Lists can use either FirstItemPointer or FirstItemIndex
    // Only RoundBuffer uses FirstItemIndex
    property FirstItemPointer: PByte read GetFirstItemPointer write SetFirstItemPointer;
    property FirstItemIndex: Integer read GetFirstItemIndex write SetFirstItemIndex;
  end;

  { TLazShiftBufferListObjBase }

  generic TLazShiftBufferListObjBase<TPItemT, TSizeT> = object
  private
    FMem: TLazListClassesInternalMem;
    FItemSize: TSizeT; // May be zero size

    function GetItemPointer(Index: Integer): TPItemT; inline;
    function GetItemPointerFast(Index: Integer): TPItemT; inline;
    procedure SetCapacity(AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
  protected
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;

    function SetCapacityEx(AValue, AnInsertPos, AnInsertSize: Integer): TPItemT;
    function InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc): TPItemT;
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc);
    property ItemPointerFast[Index: Integer]: TPItemT read GetItemPointerFast;
  public
    procedure Create;
    procedure Destroy;
    function InsertRows(AIndex, ACount: Integer): TPItemT; inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer);
    procedure SwapEntries(AIndex1, AIndex2: Integer); inline;
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
  end;

  { TLazShiftBufferListObj }

  TLazShiftBufferListObj = object(specialize TLazShiftBufferListObjBase<Pointer, TLazListClassesVarItemSize>)
  public
    procedure Create(AnItemSize: Integer);
  end;

  { TLazShiftBufferListObjGen }

  generic TLazShiftBufferListObjGen<T> = object
  private type
    TItemSize = specialize TLazListClassesItemSize<T>;
    PT = ^T;
    TListType = specialize TLazShiftBufferListObjBase<PT, TItemSize>;
  private
    FList: TListType;
  // forwarded methods
  private
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetItemPointer(Index: Integer): PT; inline;
    function GetItemPointerFast(Index: Integer): PT; inline;
    procedure SetCapacity(AValue: Integer); inline;
  protected
    function InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc): PT; inline;
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc); inline;
    property ItemPointerFast[Index: Integer]: PT read GetItemPointerFast;
  public
    procedure Create;
    procedure Destroy;
    function InsertRows(AIndex, ACount: Integer): PT; inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer); inline;
    procedure SwapEntries(AIndex1, AIndex2: Integer); inline;
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: PT read GetItemPointer;
  // new extra methods
  private
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; AValue: T);
  public
    function IndexOf(AnItem: T): integer;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

  { TLazRoundBufferListObjBase }

  generic TLazRoundBufferListObjBase<TPItemT, TSizeT> = object
  private
    // Keep the size small, if no entries exist
    // FMem:  FLowElemPointer: PByte; FCount, FCapacity_in_bytes: Integer; Array of <FItemSize
    FMem: TLazListClassesInternalMem;
    FItemSize: TSizeT; // May be zero size

    function GetItemPointer(Index: Integer): TPItemT; inline;
    function GetItemPointerFast(Index: Integer): TPItemT; inline;
    procedure SetCapacity(AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
  protected
    procedure InternalMoveUp(AFromEnd, AToEnd: PByte; AByteCnt, AByteCap: Integer); inline;
    procedure InternalMoveDown(AFrom, ATo: PByte; AByteCnt: Integer; AUpperBound: PByte); inline;
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity({%H-}ARequired: Integer): Integer;

    function  SetCapacityEx(AValue, AnInsertPos, AnInsertSize: Integer): TPItemT;
    function  InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc): TPItemT;
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc);
    property ItemPointerFast[Index: Integer]: TPItemT read GetItemPointerFast;
  public
    procedure Create;
    procedure Destroy;
    function  InsertRows(AIndex, ACount: Integer): TPItemT; inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer);
    procedure SwapEntries(AIndex1, AIndex2: Integer); inline;
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
  end;

  { TLazRoundBufferListObj }

  TLazRoundBufferListObj = object(specialize TLazRoundBufferListObjBase<Pointer, TLazListClassesVarItemSize>)
  public
    procedure Create(AnItemSize: Integer);
  end;

  { TLazRoundBufferListObjGen }

  generic TLazRoundBufferListObjGen<T> = object
  private type
    TItemSize = specialize TLazListClassesItemSize<T>;
    PT = ^T;
    TListType = specialize TLazRoundBufferListObjBase<PT, TItemSize>;
  private
    FList: TListType;
  // forwarded methods
  private
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetItemPointer(Index: Integer): PT; inline;
    function GetItemPointerFast(Index: Integer): PT; inline;
    procedure SetCapacity(AValue: Integer); inline;
  protected
    function InsertRowsEx(AIndex, ACount: Integer; AGrowProc: TLazStorageMemGrowProc): PT; inline;
    procedure DeleteRowsEx(AIndex, ACount: Integer; AShrinkProc: TLazStorageMemShrinkProc); inline;
    property ItemPointerFast[Index: Integer]: PT read GetItemPointerFast;
  public
    procedure Create;
    procedure Destroy;
    function InsertRows(AIndex, ACount: Integer): PT; inline; // can be re-introduced, to change GrowProc
    procedure DeleteRows(AIndex, ACount: Integer); inline; // can be re-introduced, to change ShrinkProc
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer); inline;
    procedure SwapEntries(AIndex1, AIndex2: Integer); inline;
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemPointer[Index: Integer]: PT read GetItemPointer;
  // new extra methods
  private
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; AValue: T);
  public
    function IndexOf(AnItem: T): integer;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

  { TLazFixedRoundBufferListMemBase }

  generic TLazFixedRoundBufferListMemBase<TPItemT, TSizeT> = object(specialize TLazRoundBufferListObjBase<TPItemT, TSizeT>)
  private
    function GetItemPointerMasked(AnIndex, AMask: Integer): TPItemT; inline; // AMask: Bitmask for Capacity
    function GetItemByteOffsetMasked(AnIndex, AMask: Integer): Integer; inline; // AMask: Bitmask for Capacity
    function GetFirstItemByteOffset: Integer; inline; // AMask: Bitmask for Capacity
  protected
    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity({%H-}ARequired: Integer): Integer;
    property Mem: TLazListClassesInternalMem read FMem;

    // Special Methods for use with PagedList
    procedure AdjustFirstItemOffset(ACount, AMask: Integer); inline; // For bubbling / shift the buffer
    procedure InsertRowsAtStart(ACount, AMask: Integer); inline;
    procedure InsertRowsAtEnd(ACount: Integer); inline;
    procedure InsertRowsAtBoundary(AnAtStart: Boolean; ACount, AMask: Integer);
    procedure DeleteRowsAtStart(ACount, AMask: Integer); inline;
    procedure DeleteRowsAtEnd(ACount: Integer); inline;
    procedure DeleteRowsAtBoundary(AnAtStart: Boolean; ACount, AMask: Integer); inline;
    procedure MoveRowsToOther(AFromOffset, AToOffset, ACount, ACap: Integer; AnOther: TLazFixedRoundBufferListMemBase); inline;
    procedure MoveBytesToOther(AFromByteOffset, AToByteOffset, AByteCount, AByteCap: Integer; AnOther: TLazFixedRoundBufferListMemBase);
    property ItemPointerMasked[AnIndex, AMask: Integer]: TPItemT read GetItemPointerMasked;
  public
    procedure Create(AItemSize: TSizeT; ACapacity: Integer);
    function InsertRows(AIndex, ACount: Integer): TPItemT; inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
  end;

  { TLazPagedListObjBase }

  generic TLazPagedListObjBase<TPItemT, TSizeT> = object
  private type
    TPageType = specialize TLazFixedRoundBufferListMemBase<TPItemT, TSizeT>;
    PPageType = ^TPageType;
    TPageSize = specialize TLazListClassesItemSize<TPageType>;
    TPageListType = specialize TLazShiftBufferListObjBase<PPageType, TPageSize>;
  private
    FGrowProc: TLazStorageMemGrowProc;
    FShrinkProc: TLazStorageMemShrinkProc;
    FExtraCapacityNeeded: Integer;
    FPages: TPageListType;
    FItemSize: TSizeT;
    FPageSizeMask, FPageSizeExp: Integer;
    FFirstPageEmpty: Integer;
    FCount: Integer;

    function GetPagePointer(PageIndex: Integer): PPageType; inline;
    function GetPageSubIdx(Index: Integer): Integer; inline; // except for page=0
    function GetItemPageIdx(Index: Integer): Integer; inline;
    function GetItemPointer(Index: Integer): TPItemT; inline;
    procedure SetCapacity(AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetPageCount: Integer; inline;
    procedure JoinPageWithNext(APageIdx, AJoinEntryIdx, AnExtraDelPages: Integer); inline;
    procedure SplitPageToFront(ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer; AExtraCapacityNeeded: Integer = 0); inline;
    procedure SplitPageToBack(ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer; AExtraCapacityNeeded: Integer = 0); inline;
    procedure SplitPage(ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer; AExtraCapacityNeeded: Integer = 0);
    procedure BubbleEntriesDown(ASourceStartIdx, ATargetEndIdx, AnEntryCount: Integer); inline;
    procedure BubbleEntriesUp(ASourceStartIdx, ATargetEndIdx, AnEntryCount: Integer); inline;
    procedure InternalBubbleEntriesDown(ASourceStartIdx, ATargetEndIdx, AnEntryCount: Integer);
    procedure InternalBubbleEntriesUp(ASourceStartIdx, ATargetEndIdx, AnEntryCount: Integer);
    procedure SwapPagesUp(ASourceStartIndex, ATargetStartIndex, ATargetEndIndex: Integer); inline;
    procedure SwapPagesDown(ASourceStartIndex, ATargetStartIndex, ATargetEndIndex: Integer); inline;
    procedure InternalMoveRowsDown(AFromIndex, AToIndex, ACount: Integer); inline;
    procedure InternalMoveRowsUp(AFromIndex, AToIndex, ACount: Integer); inline;
    procedure InsertFilledPages(AIndex, ACount: Integer; AExtraCapacityNeeded: Integer = 0); inline;
    procedure DeletePages(AIndex, ACount: Integer); inline;
  protected
    function GrowCapacity(ARequiredPages: Integer): Integer;
    function ShrinkCapacity(ARequiredPages: Integer): Integer;
    property PagePointer[PageIndex: Integer]: PPageType read GetPagePointer;
  public
    procedure Create(APageSizeExp: Integer);
    procedure Destroy;
    procedure InsertRows(AIndex, ACount: Integer);
    procedure DeleteRows(AIndex, ACount: Integer);
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer);
    procedure DebugDump;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
    property PageCount: Integer read GetPageCount;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
    property GrowProc: TLazStorageMemGrowProc read FGrowProc write FGrowProc;
    property ShrinkProc: TLazStorageMemShrinkProc read FShrinkProc write FShrinkProc;
  end;

  TLazPagedListObjParent = specialize TLazPagedListObjBase<Pointer, TLazListClassesVarItemSize>;

  TLazPagedListObj = object(TLazPagedListObjParent)
  public
    procedure Create(APageSizeExp: Integer; AnItemSize: Integer);
  end;


  { TLazShiftBufferList }

  TLazShiftBufferList = class
  private
    FListMem: TLazShiftBufferListObj;
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

  { TLazShiftBufferListGen }

  generic TLazShiftBufferListGen<T> = class
  private type
    TListMem = specialize TLazShiftBufferListObjGen<T>;
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
  Result := @(FMem^.Data);
end;

function TLazListClassesInternalMem.GetFirstItemIndex: Integer;
begin
  Result := FMem^.FirstItem.Idx;
end;

function TLazListClassesInternalMem.GetFirstItemPointer: PByte;
begin
  Result := FMem^.FirstItem.Ptr;
end;

function TLazListClassesInternalMem.GetCapacityFast: Cardinal;
begin
  Result := FMem^.Capacity;
end;

procedure TLazListClassesInternalMem.SetCapacity(AValue: Cardinal);
begin
  assert(FMem <> nil, 'TLazListClassesInternalMem.SetCapacity: FMem <> nil');
  FMem^.Capacity := AValue;
end;

function TLazListClassesInternalMem.GetCapacity: Cardinal;
begin
  if FMem = nil
  then Result := 0
  else Result := FMem^.Capacity;
end;

function TLazListClassesInternalMem.GetCount: Integer;
begin
  if FMem = nil
  then Result := 0
  else Result := FMem^.Count;
end;

procedure TLazListClassesInternalMem.SetCount(AValue: Integer);
begin
  assert(FMem <> nil, 'TLazListClassesInternalMem.SetCount: FMem <> nil');
  FMem^.Count := AValue;
end;

procedure TLazListClassesInternalMem.SetFirstItemIndex(AValue: Integer);
begin
  FMem^.FirstItem.Idx := AValue;
end;

procedure TLazListClassesInternalMem.SetFirstItemPointer(AValue: PByte);
begin
  assert(FMem <> nil, 'TLazListClassesInternalMem.SetFirstItemPointer: FMem <> nil');
  FMem^.FirstItem.Ptr := AValue;
end;

procedure TLazListClassesInternalMem.Init;
begin
  FMem := nil;
end;

procedure TLazListClassesInternalMem.Alloc(AByteSize: Integer);
begin
  Free;
  FMem := Getmem(SizeOf(TMemRecord) + AByteSize);
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

{ TLazShiftBufferListObjBase }

function TLazShiftBufferListObjBase.GetItemPointer(Index: Integer): TPItemT;
begin
  assert((not FMem.IsAllocated) or (Cardinal(Index) <= FMem.Capacity), 'TLazShiftBufferListObjBase.GetItemPointer: (not FMem.IsAllocated) or (Index <= FMem.Capacity)');
  if not FMem.IsAllocated
  then Result := nil
  else Result := TPItemT(FMem.FirstItemPointer + (Index * FItemSize.ItemSize));
end;

function TLazShiftBufferListObjBase.GetItemPointerFast(Index: Integer): TPItemT;
begin
  assert(Cardinal(Index) <= FMem.Capacity, 'TLazShiftBufferListObjBase.GetItemPointerFast: Index <= FMem.Capacity');
  Result := TPItemT(FMem.FirstItemPointer + (Index * FItemSize.ItemSize));
end;

procedure TLazShiftBufferListObjBase.SetCapacity(AValue: Integer);
begin
  SetCapacityEx(AValue, 0, 0);
end;

function TLazShiftBufferListObjBase.GetCapacity: Integer;
begin
  Result := FMem.Capacity;
end;

function TLazShiftBufferListObjBase.GetCount: Integer;
begin
  Result := FMem.Count;
end;

function TLazShiftBufferListObjBase.GrowCapacity(ARequired: Integer): Integer;
begin
  Result := Min(ARequired * 2, ARequired + $8000);
end;

function TLazShiftBufferListObjBase.ShrinkCapacity(ARequired: Integer): Integer;
begin
  assert(ARequired <= Capacity, 'TLazShiftBufferListObjBase.ShrinkCapacity: ARequired <= Capacity');
  if ARequired * 4 < Capacity then
    Result := ARequired * 2
  else
    Result := -1;
end;

function TLazShiftBufferListObjBase.SetCapacityEx(AValue, AnInsertPos,
  AnInsertSize: Integer): TPItemT;
var
  NewMem: TLazListClassesInternalMem;
  Pos1, Cnt, NewCnt, c: Integer;
  PTarget, PSource: PByte;
begin
  Result := nil;
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

  {%H-}NewMem.Init;
  NewMem.Alloc(AValue * FItemSize.ItemSize);

  Pos1 := Cardinal(AValue-NewCnt) div 2;
  PTarget := NewMem.DataPointer + (Pos1 * FItemSize.ItemSize);

  NewMem.FirstItemPointer := PTarget;
  NewMem.Count := NewCnt;
  NewMem.Capacity := AValue;
  assert((NewMem.FirstItemPointer >= NewMem.DataPointer) and (NewMem.FirstItemPointer < NewMem.DataPointer + NewMem.Capacity {%H-}* FItemSize.ItemSize), 'TLazShiftBufferListObjBase.InsertRowsEx: (NewMem.FirstItemPointer >= NewMem.NewMem+NewMem.DATA_OFFS) and (NewMem.FirstItemPointer < NewMem.NewMem+NewMem.DATA_OFFS + NewMem.Capacity * FItemSize.ItemSize)');

  if Cnt > 0 then begin
    PSource := FMem.FirstItemPointer;
    if AnInsertPos > 0 then begin
      c := AnInsertPos * FItemSize.ItemSize;
      Move(PSource^, PTarget^, c);
      PSource := PSource + c;
      Result := TPItemT(PTarget+c);
    end
    else
      Result := TPItemT(PTarget);
    PTarget := PTarget + ((AnInsertPos + AnInsertSize) * FItemSize.ItemSize);

    if AnInsertPos < Cnt then
      Move(PSource^, PTarget^, ((Cnt - AnInsertPos) * FItemSize.ItemSize));
  end
  else begin
    assert(AnInsertPos=0, 'TLazShiftBufferListObjBase.SetCapacityEx: AnInsertPos=0');
    Result := TPItemT(PTarget);
  end;

  FMem.Free;
  FMem := NewMem;
end;

function TLazShiftBufferListObjBase.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc): TPItemT;
var
  Cnt, Cap, CntFreeFront, CntFreeEnd, Middle, i, c: Integer;
  CanFront, CanEnd: Boolean;
  PTarget, PSource: PByte;
begin
  Result := nil;
  if ACount = 0 then exit;

  Cnt := Count;
  Cap := Capacity;
  assert((ACount>0) and (AIndex>=0) and (AIndex<=Cnt), 'TLazShiftBufferListObj.InsertRows: (ACount>0) and (AIndex>=0) and (AIndex<=Cnt)');

  if Cnt + ACount > Cap then begin
    if not assigned(AGrowProc) then
      AGrowProc := @GrowCapacity;
    Result := SetCapacityEx(AGrowProc(Cnt + ACount), AIndex, ACount);
    exit;
  end;

  CntFreeFront := (FMem.FirstItemPointer - FMem.DataPointer) div FItemSize.ItemSize;
  CntFreeEnd   := Cap - CntFreeFront - Cnt;
  CanFront := CntFreeFront >= ACount;
  CanEnd   := CntFreeEnd >= ACount;

  if not(CanFront or CanEnd)
  then begin
    if not assigned(AGrowProc) then
      AGrowProc := @GrowCapacity;
    i := AGrowProc(Cnt + ACount);
    if i > Cap then begin
      Result := SetCapacityEx(AGrowProc(Cnt + ACount), AIndex, ACount);
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
    c := AIndex * FItemSize.ItemSize;
    if AIndex > 0 then
      Move(PSource^, PTarget^, c);
    Result := TPItemT(PTarget + c);

    assert(PTarget >= FMem.DataPointer, 'TLazShiftBufferListObj.InsertRows: PTarget >= FMem+DATA_OFFS');
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
      assert(PSource + (i * FItemSize.ItemSize) >= FMem.DataPointer, 'TLazShiftBufferListObj.InsertRows: PSource + (i * FItemSize.ItemSize) >= FMem+DATA_OFFS');
      PSource := PSource + (i * FItemSize.ItemSize);
      FMem.FirstItemPointer := PSource;
    end;
    Result := TPItemT(PSource);
    FMem.Count := Cnt + ACount;
  end
  else
  begin
 	// split to both ends
    assert((cap >= ACount) and (CntFreeFront> 0) and (CntFreeEnd > 0), 'TLazShiftBufferListObj.InsertRows: (cap >= ACount) and (CntFreeFront> 0) and (CntFreeEnd > 0)');
    i := Max(Cardinal(Cap-Cnt-ACount) div 2 - 1, 0);

    PSource := FMem.FirstItemPointer;
    PTarget := PSource - ((CntFreeFront - i) * FItemSize.ItemSize);
    c := AIndex * FItemSize.ItemSize;
    if AIndex > 0 then
      Move(PSource^, PTarget^, c);
    Result := TPItemT(PTarget + c);

    FMem.FirstItemPointer := PTarget;
    FMem.Count := Cnt + ACount;

    assert((ACount>CntFreeFront-i) and (ACount-(CntFreeFront - i)<=CntFreeEnd), 'TLazShiftBufferListObj.InsertRows: (ACount>CntFreeFront-i) and (ACount-(CntFreeFront - i)<=CntFreeEnd)');
    PSource := PSource + c;
    PTarget := PSource + ((ACount - (CntFreeFront - i)) * FItemSize.ItemSize);
    if Cnt-AIndex > 0 then
      Move(PSource^, PTarget^, (Cnt-AIndex) * FItemSize.ItemSize);
  end;

  assert((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize), 'TLazShiftBufferListObjBase.InsertRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity * FItemSize.ItemSize)');
end;

procedure TLazShiftBufferListObjBase.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
var
  Cnt, Middle, i: Integer;
  PTarget, PSource: PByte;
begin
  if ACount = 0 then exit;

  Cnt := Count;
  assert((ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt), 'TLazShiftBufferListObj.InsertRows: (ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt)');
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
    FMem.FirstItemPointer := FMem.DataPointer + (FMem.Capacity div 2) * Cardinal(FItemSize.ItemSize);
  assert((not FMem.IsAllocated) or ((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize)), 'TLazShiftBufferListObjBase.InsertRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity * FItemSize.ItemSize)');
end;

procedure TLazShiftBufferListObjBase.Create;
begin
  FMem.Init;
end;

procedure TLazShiftBufferListObjBase.Destroy;
begin
  FMem.Free;
end;

function TLazShiftBufferListObjBase.InsertRows(AIndex, ACount: Integer): TPItemT;
begin
  Result := InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TLazShiftBufferListObjBase.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

procedure TLazShiftBufferListObjBase.MoveRows(AFromIndex, AToIndex, ACount: Integer);
var
  BytesToMove, DistanceToMove: Integer;
  p, pFrom, pTo: PByte;
begin
  assert((AFromIndex>=0) and (AToIndex>=0) and (AFromIndex+ACount<=Count) and (AToIndex+ACount<=Count), 'TLazShiftBufferListObjBase.MoveRows: (AFromIndex>=0) and (AToIndex>=0) and (AFromIndex+ACount<=Count) and (AToIndex+ACount<=Count)');

  BytesToMove := FItemSize.ItemSize * ACount;
  pFrom := PByte(GetItemPointer(AFromIndex));
  pTo   := PByte(GetItemPointer(AToIndex));

  if (ACount << 1) > Count then begin
    p := FMem.FirstItemPointer;
    if AToIndex < AFromIndex then begin
      // free at end? (instead of moving entries down, move surroundings up
      DistanceToMove := pFrom - pTo;
      if (FMem.DataPointer + (FMem.Capacity - Count) * FItemSize.ItemSize - p) > DistanceToMove then begin
        Move(p^, (p + DistanceToMove)^, pTo - p);
        pFrom := pFrom + BytesToMove;
        Move(pFrom^, (pFrom + DistanceToMove)^, p + Count * FItemSize.ItemSize - pFrom);
        FMem.FirstItemPointer := FMem.FirstItemPointer + DistanceToMove;
        assert((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize), 'TLazShiftBufferListObjBase.MoveRows: (FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize)');
        exit;
      end;
    end
    else begin
      // free at front (instead of moving entries up, move surroundings down
      DistanceToMove := pTo - pFrom;
      if (FMem.FirstItemPointer - FMem.DataPointer) > DistanceToMove then begin
        Move(p^, (p - DistanceToMove)^, pFrom - p);
        pFrom := pFrom + BytesToMove;
        Move((pFrom + DistanceToMove)^, pFrom^, p + Count * FItemSize.ItemSize - pFrom - DistanceToMove);
        FMem.FirstItemPointer := FMem.FirstItemPointer - DistanceToMove;
        assert((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize), 'TLazShiftBufferListObjBase.MoveRows: (FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize)');
        exit;
      end;
    end;
  end;

  Move(pFrom^, pTo^, BytesToMove);
  assert((FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize), 'TLazShiftBufferListObjBase.MoveRows: (FMem.FirstItemPointer >= FMem.DataPointer) and (FMem.FirstItemPointer < FMem.DataPointer + FMem.Capacity {%H-}* FItemSize.ItemSize)');
end;

procedure TLazShiftBufferListObjBase.SwapEntries(AIndex1, AIndex2: Integer);
var
  t: PByte;
begin
  t := Getmem(FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex1))^, t^, FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex2))^, PByte(GetItemPointer(AIndex1))^, FItemSize.ItemSize);
  Move(t^, PByte(GetItemPointer(AIndex2))^, FItemSize.ItemSize);
  FreeMem(t);
end;

procedure TLazShiftBufferListObjBase.DebugDump;
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

{ TLazShiftBufferListObj }

procedure TLazShiftBufferListObj.Create(AnItemSize: Integer);
begin
  fItemSize.ItemSize := AnItemSize;
  inherited Create;
end;

{ TLazShiftBufferListObjGen }

function TLazShiftBufferListObjGen.Get(Index: Integer): T;
begin
  Result := FList.ItemPointer[Index]^;
end;

function TLazShiftBufferListObjGen.GetCapacity: Integer;
begin
  Result := FList.GetCapacity;
end;

function TLazShiftBufferListObjGen.GetCount: Integer;
begin
  Result := FList.GetCount;
end;

function TLazShiftBufferListObjGen.GetItemPointer(Index: Integer): PT;
begin
  Result := FList.GetItemPointer(Index);
end;

function TLazShiftBufferListObjGen.GetItemPointerFast(Index: Integer): PT;
begin
  Result := FList.ItemPointerFast[Index];
end;

procedure TLazShiftBufferListObjGen.Put(Index: Integer; AValue: T);
begin
  FList.ItemPointer[Index]^ := AValue;
end;

procedure TLazShiftBufferListObjGen.SetCapacity(AValue: Integer);
begin
  FList.SetCapacity(AValue);
end;

function TLazShiftBufferListObjGen.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc): PT;
begin
  Result := FList.InsertRowsEx(AIndex, ACount, AGrowProc);
end;

procedure TLazShiftBufferListObjGen.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
begin
  FList.DeleteRowsEx(AIndex, ACount, AShrinkProc);
end;

procedure TLazShiftBufferListObjGen.Create;
begin
  FList.Create;
end;

procedure TLazShiftBufferListObjGen.Destroy;
begin
  FList.Destroy;
end;

function TLazShiftBufferListObjGen.InsertRows(AIndex, ACount: Integer): PT;
begin
  Result := FList.InsertRows(AIndex, ACount);
end;

procedure TLazShiftBufferListObjGen.DeleteRows(AIndex, ACount: Integer);
begin
  FList.DeleteRows(AIndex, ACount);
end;

procedure TLazShiftBufferListObjGen.MoveRows(AFromIndex, AToIndex, ACount: Integer);
begin
  FList.MoveRows(AFromIndex, AToIndex, ACount);
end;

procedure TLazShiftBufferListObjGen.SwapEntries(AIndex1, AIndex2: Integer);
begin
  FList.SwapEntries(AIndex1, AIndex2);
end;

procedure TLazShiftBufferListObjGen.DebugDump;
begin
  FList.DebugDump;
end;

function TLazShiftBufferListObjGen.IndexOf(AnItem: T): integer;
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

{ TLazRoundBufferListObjBase }

function TLazRoundBufferListObjBase.GetItemPointer(Index: Integer): TPItemT;
var
  c: Integer;
begin
  if not FMem.IsAllocated
  then Result := nil
  else begin
    c := FMem.Capacity;
    assert(Index <= c, 'TLazRoundBufferListObjBase.GetItemPointer: Index <= c');
    Index := FMem.FirstItemIndex + Index;
    if Index >= c then
      Index := Index - c;
    Result := TPItemT(FMem.DataPointer + Index * FItemSize.ItemSize);
  end;
end;

function TLazRoundBufferListObjBase.GetItemPointerFast(Index: Integer): TPItemT;
var
  c: Cardinal;
begin
  c := FMem.Capacity;
  assert(Cardinal(Index) <= c, 'TLazRoundBufferListObjBase.GetItemPointerFast: Index <= c');
  Index := FMem.FirstItemIndex + Index;
  if Cardinal(Index) >= c then
    Index := Index - c;
  Result := TPItemT(FMem.DataPointer + Index * FItemSize.ItemSize);
end;

procedure TLazRoundBufferListObjBase.SetCapacity(AValue: Integer);
begin
  SetCapacityEx(AValue, 0, 0);
end;

function TLazRoundBufferListObjBase.GetCapacity: Integer;
begin
  Result := FMem.Capacity;
end;

function TLazRoundBufferListObjBase.GetCount: Integer;
begin
  Result := FMem.Count;
end;

procedure TLazRoundBufferListObjBase.InternalMoveUp(AFromEnd, AToEnd: PByte; AByteCnt,
  AByteCap: Integer);
var
  c: Integer;
  l: PByte;
begin
  assert(AFromEnd <> AToEnd, 'TLazRoundBufferListObjBase.InternalMoveUp: AFrom <> ATo');
  l := FMem.DataPointer;
  if AToEnd = l then AToEnd := l + AByteCap;
  if AFromEnd = l then AFromEnd := l + AByteCap;

  if AToEnd < AFromEnd then begin
    c := Min(AToEnd - l, AByteCnt);
    AFromEnd := AFromEnd - c;
    AToEnd := AToEnd - c;
    Move(AFromEnd^, AToEnd^, c);
    AByteCnt := AByteCnt - c;
    if AByteCnt = 0 then
      exit;
    AToEnd := l + AByteCap;
  end;

  c := Min(AFromEnd - l, AByteCnt);
  AFromEnd := AFromEnd - c;
  AToEnd := AToEnd - c;
  Move(AFromEnd^, AToEnd^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  AFromEnd := l + AByteCap;

  c := Min(AToEnd - l, AByteCnt);
  AFromEnd := AFromEnd - c;
  AToEnd := AToEnd - c;
  Move(AFromEnd^, AToEnd^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  AToEnd := l + AByteCap;

  Move((AFromEnd-AByteCnt)^, (AToEnd-AByteCnt)^, AByteCnt);
end;

procedure TLazRoundBufferListObjBase.InternalMoveDown(AFrom, ATo: PByte; AByteCnt: Integer;
  AUpperBound: PByte);
var
  c: Integer;
begin
  assert(AFrom <> ATo, 'TLazRoundBufferListObjBase.InternalMoveDown: AFrom <> ATo');
  if ATo > AFrom then begin
    c := Min(AUpperBound - ATo, AByteCnt);
    Move(AFrom^, ATo^, c);
    AByteCnt := AByteCnt - c;
    if AByteCnt = 0 then
      exit;
    ATo := FMem.DataPointer; // ATo + c - AByteCap;
    AFrom := AFrom + c;
  end;

  c := Min(AUpperBound - AFrom, AByteCnt);
  Move(AFrom^, ATo^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  AFrom := FMem.DataPointer; // AFrom + c - AByteCap;
  ATo := ATo + c;

  c := Min(AUpperBound - ATo, AByteCnt);
  Move(AFrom^, ATo^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  ATo := FMem.DataPointer; // ATo + c - AByteCap;
  AFrom := AFrom + c;

  Move(AFrom^, ATo^, AByteCnt);
end;

function TLazRoundBufferListObjBase.GrowCapacity(ARequired: Integer): Integer;
begin
  Result := Min(ARequired * 2, ARequired + $8000);
end;

function TLazRoundBufferListObjBase.ShrinkCapacity(ARequired: Integer): Integer;
begin
  assert(ARequired <= Capacity, 'TLazShiftBufferListObjBase.ShrinkCapacity: ARequired <= Capacity');
  if ARequired * 4 < Capacity then
    Result := ARequired * 2
  else
    Result := -1;
end;

function TLazRoundBufferListObjBase.SetCapacityEx(AValue, AnInsertPos,
  AnInsertSize: Integer): TPItemT;
var
  NewMem: TLazListClassesInternalMem;
  Pos1, Cnt, NewCnt, siz, siz2: Integer;
  PTarget, PSource, m: PByte;
begin
  Result := nil;
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

  {%H-}NewMem.Init;
  NewMem.Alloc(AValue * FItemSize.ItemSize);

  Pos1 := Cardinal(AValue-NewCnt) div 2;
  PTarget := NewMem.DataPointer + (Pos1 * FItemSize.ItemSize);

  NewMem.FirstItemIndex:= Pos1;
  NewMem.Count := NewCnt;
  NewMem.Capacity := AValue;
  assert((NewMem.FirstItemIndex >= 0) and (NewMem.FirstItemIndex {%H-}< NewMem.Capacity), 'TLazShiftBufferListObjBase.InsertRowsEx: (NewMem.FirstItemIndex >= NewMem.NewMem+NewMem.DATA_OFFS) and (NewMem.FirstItemIndex < NewMem.NewMem+NewMem.DATA_OFFS + NewMem.Capacity)');

  if Cnt > 0 then begin
    m := FMem.DataPointer;
    PSource := m + (FMem.FirstItemIndex * FItemSize.ItemSize);
    m := m + FMem.Capacity * Cardinal(FItemSize.ItemSize);
    if AnInsertPos > 0 then begin
      siz := (AnInsertPos * FItemSize.ItemSize);
      siz2 := m - PSource;
      if siz > siz2 then begin
        Move(PSource^, PTarget^, siz2);
        Move(FMem.DataPointer^, (PTarget+siz2)^, siz - siz2);
      end
      else
        Move(PSource^, PTarget^, siz);
      Result := TPItemT(PTarget + siz);
    end
    else
      Result := TPItemT(PTarget);

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
  end
  else begin
    assert(AnInsertPos=0, 'TLazShiftBufferListObjBase.SetCapacityEx: AnInsertPos=0');
    Result := TPItemT(PTarget);
  end;

  FMem.Free;
  FMem := NewMem;
end;

function TLazRoundBufferListObjBase.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc): TPItemT;
var
  Cnt, Cap: Integer;
  siz, PSourceIdx, PTargetIdx: Integer;
  PTarget, PSource, m: PByte;
begin
  Result := nil;
  if ACount = 0 then exit;

  Cnt := Count;
  Cap := FMem.Capacity * Cardinal(FItemSize.ItemSize);
  assert((ACount>0) and (AIndex>=0) and (AIndex<=Cnt), 'TLazShiftBufferListObj.InsertRows: (ACount>0) and (AIndex>=0) and (AIndex<=Cnt)');

  if Cnt + ACount > Capacity then begin
    if not Assigned(AGrowProc) then
      AGrowProc := @GrowCapacity;
    Result := SetCapacityEx(AGrowProc(Cnt + ACount), AIndex, ACount);
    exit;
  end;

  if (AIndex = 0) or (Cardinal(AIndex) < Cardinal(Cnt) div 2) then begin
    // use space at front of list
    PSourceIdx := FMem.FirstItemIndex;
    PTargetIdx := PSourceIdx - ACount;
    if PtrInt(PTargetIdx) < 0 then
      PTargetIdx := PTargetIdx + Capacity;
    FMem.FirstItemIndex := PTargetIdx;
    FMem.Count := Cnt + ACount;

    PTarget := FMem.DataPointer + PTargetIdx * FItemSize.ItemSize;

    if AIndex > 0 then begin
      PSource := FMem.DataPointer + PSourceIdx * FItemSize.ItemSize;
      siz := AIndex * FItemSize.ItemSize;
      Result := TPItemT(PTarget + siz);
      m := FMem.DataPointer + Cap;
      if PByte(Result) >= m then
        Result := TPItemT(PByte(Result) - Cap);
      InternalMoveDown(PSource, PTarget, siz, m);
    end
    else
      Result := TPItemT(PTarget);
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
      Result := TPItemT(PSource - siz);
      if PByte(Result) < m then
        Result := TPItemT(PByte(Result) + Cap);
      InternalMoveUp(PSource, PTarget, siz, Cap);
    end
    else
      Result := TPItemT(PSource);
  end;

  assert((FMem.FirstItemIndex >= 0) and (FMem.FirstItemIndex {%H-}< FMem.Capacity), 'TLazShiftBufferListObjBase.InsertRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity)');
end;

procedure TLazRoundBufferListObjBase.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
var
  Cnt, Cap, Middle, i, siz, siz2: Integer;
  PTarget, PSource, m: PByte;
begin
  if ACount = 0 then exit;

  Cnt := Count;
  Cap := FMem.Capacity * Cardinal(FItemSize.ItemSize);
  assert((ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt), 'TLazShiftBufferListObjBase.DeleteRowsEx: (ACount>0) and (AIndex>=0) and (AIndex+ACount<=Cnt)');
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

    i := FMem.FirstItemIndex + ACount;
    if i >= Capacity then
      i := i - Capacity;
    FMem.FirstItemIndex := i;
    FMem.Count := Cnt - ACount;
  end
  else begin
    // make space at end of list
    if AIndex < Cnt-ACount then begin
      PSource := PByte(ItemPointer[AIndex+ACount]);
      PTarget := PByte(ItemPointer[AIndex]);
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
    FMem.FirstItemIndex:= 0;
  assert((not FMem.IsAllocated) or ((FMem.FirstItemIndex >= 0) and (FMem.FirstItemIndex {%H-}< FMem.Capacity)), 'TLazShiftBufferListObjBase.DeleteRowsEx: (FMem.FirstItemPointer >= FMem.FMem+FMem.DATA_OFFS) and (FMem.FirstItemPointer < FMem.FMem+FMem.DATA_OFFS + FMem.Capacity)');
end;

procedure TLazRoundBufferListObjBase.Create;
begin
  FMem.Init;
end;

procedure TLazRoundBufferListObjBase.Destroy;
begin
  FMem.Free;
end;

function TLazRoundBufferListObjBase.InsertRows(AIndex, ACount: Integer): TPItemT;
begin
  Result := InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TLazRoundBufferListObjBase.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

procedure TLazRoundBufferListObjBase.MoveRows(AFromIndex, AToIndex, ACount: Integer);
var
  Cnt, CapBytes, c, Diff: Integer;
  BytesToMove: Integer;
  u, pFrom, pTo: PByte;
  Cap: Cardinal;
begin
  assert((AFromIndex>=0) and (AToIndex>=0) and (AFromIndex+ACount<=Count) and (AToIndex+ACount<=Count), 'TLazShiftBufferListObjBase.MoveRows: (AFromIndex>=0) and (AToIndex>=0) and (AFromIndex+ACount<=Count) and (AToIndex+ACount<=Count)');

  Cnt := Count;
  Cap := Capacity;
  CapBytes := Cap * Cardinal(FItemSize.ItemSize);

  if (ACount * 2) >= Cnt then begin
    if AToIndex < AFromIndex then begin
      // FirstItemIndex = FirstItemIndex + Diff; // move ALL down
      Diff := AFromIndex-AToIndex;
      // Save data in front of AToIndex; move it in front of AFromIndex;
      InternalMoveUp(PByte(GetItemPointer(AToIndex)), PByte(GetItemPointer(AFromIndex)),
                     AToIndex * FItemSize.ItemSize, CapBytes);
      // Move data after END-OF-SOURCE up (moving behind current count (wrap around capacity if needed))
      c := Cnt + Diff;
      if Cardinal(c) > Cap then
        c := Cardinal(c) - Cap;
      InternalMoveUp(PByte(GetItemPointer(Cnt)),
                     PByte(GetItemPointer(c)), // Cnt=Cap will be handled by GetItemPointer
                     (Cnt - (AFromIndex+ACount)) * FItemSize.ItemSize, CapBytes);
      c := FMem.FirstItemIndex + Diff;
      if Cardinal(c) >= Cap then
        c := Cardinal(c) - Cap;
      FMem.FirstItemIndex := c;
    end
    else begin
      // FirstItemIndex = FirstItemIndex - Diff; // move ALL up
      Diff := AToIndex-AFromIndex;
      u := FMem.DataPointer + CapBytes;
      // Save data from after Target; move it after Source
      InternalMoveDown(PByte(GetItemPointer(AToIndex+ACount)),
                       PByte(GetItemPointer(AFromIndex+ACount)),
                       (Cnt - (AToIndex+ACount)) * FItemSize.ItemSize, u);
      // Move data before SOURCE down (may be below 0 / wrap)
      InternalMoveDown(PByte(GetItemPointer(0)), PByte(GetItemPointer(Cap-Diff)),
                       AFromIndex * FItemSize.ItemSize, u);
      c := FMem.FirstItemIndex - Diff;
      if c < 0 then
        c := Cap - Cardinal(-c);
      FMem.FirstItemIndex := c;
    end;
  end
  else begin
    // normal move
    BytesToMove := FItemSize.ItemSize * ACount;
    if AFromIndex > AToIndex then begin
      pFrom := PByte(GetItemPointer(AFromIndex));
      pTo   := PByte(GetItemPointer(AToIndex));
      InternalMoveDown(pFrom, pTo, BytesToMove, FMem.DataPointer + CapBytes)
    end
    else begin
      pFrom := PByte(GetItemPointer(AFromIndex+ACount));
      pTo   := PByte(GetItemPointer(AToIndex+ACount));
      InternalMoveUp(pFrom, pTo, BytesToMove, CapBytes);
    end;
  end;
  assert((FMem.FirstItemIndex >= 0) and (FMem.FirstItemIndex < Capacity), 'TLazRoundBufferListObjBase.MoveRows: (FMem.FirstItemIndex >= 0) and (FMem.FirstItemIndex < Capacity)');
end;

procedure TLazRoundBufferListObjBase.SwapEntries(AIndex1, AIndex2: Integer);
var
  t: PByte;
begin
  t := Getmem(FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex1))^, t^, FItemSize.ItemSize);
  Move(PByte(GetItemPointer(AIndex2))^, PByte(GetItemPointer(AIndex1))^, FItemSize.ItemSize);
  Move(t^, PByte(GetItemPointer(AIndex2))^, FItemSize.ItemSize);
  FreeMem(t);
end;

procedure TLazRoundBufferListObjBase.DebugDump;
var i , c: integer; s:string;
begin
  if fmem.IsAllocated then begin
    dbgout(['TLazRoundBufferListObjBase.Dump ', FMem.Capacity, ' , ',FMem.Count,
    ' --- ', fmem.datapointer, ' , ',FMem.FirstItemIndex,' --- ', ': ']);
     s :='';
    c := FMem.Count;
    for i := 0 to FMem.Capacity - 1 do begin
      if i = c then s := s + '# ';
      s := s +dbgMemRange(itempointer[i], FItemSize.ItemSize )+ ', ';
    end;
    debugln(s);
  end
  else debugln(['TLazRoundBufferListObjBase.Dump NONE']);
end;

{ TLazRoundBufferListObj }

procedure TLazRoundBufferListObj.Create(AnItemSize: Integer);
begin
  FItemSize.ItemSize := AnItemSize;
  inherited Create;
end;

{ TLazRoundBufferListObjGen }

function TLazRoundBufferListObjGen.GetCapacity: Integer;
begin
  Result := FList.GetCapacity;
end;

function TLazRoundBufferListObjGen.GetCount: Integer;
begin
  Result := FList.GetCount;
end;

function TLazRoundBufferListObjGen.GetItemPointer(Index: Integer): PT;
begin
  Result := FList.GetItemPointer(Index);
end;

function TLazRoundBufferListObjGen.GetItemPointerFast(Index: Integer): PT;
begin
  Result := FList.GetItemPointer(Index);
end;

procedure TLazRoundBufferListObjGen.SetCapacity(AValue: Integer);
begin
  FList.SetCapacity(AValue);
end;

function TLazRoundBufferListObjGen.InsertRowsEx(AIndex, ACount: Integer;
  AGrowProc: TLazStorageMemGrowProc): PT;
begin
  Result := FList.InsertRowsEx(AIndex, ACount, AGrowProc);
end;

procedure TLazRoundBufferListObjGen.DeleteRowsEx(AIndex, ACount: Integer;
  AShrinkProc: TLazStorageMemShrinkProc);
begin
  FList.DeleteRowsEx(AIndex, ACount, AShrinkProc);
end;

procedure TLazRoundBufferListObjGen.Create;
begin
  FList.Create;
end;

procedure TLazRoundBufferListObjGen.Destroy;
begin
  FList.Destroy;
end;

function TLazRoundBufferListObjGen.InsertRows(AIndex, ACount: Integer): PT;
begin
  Result := FList.InsertRows(AIndex, ACount);
end;

procedure TLazRoundBufferListObjGen.DeleteRows(AIndex, ACount: Integer);
begin
  FList.DeleteRows(AIndex, ACount);
end;

procedure TLazRoundBufferListObjGen.MoveRows(AFromIndex, AToIndex, ACount: Integer);
begin
  FList.MoveRows(AFromIndex, AToIndex, ACount);
end;

procedure TLazRoundBufferListObjGen.SwapEntries(AIndex1, AIndex2: Integer);
begin
  FList.SwapEntries(AIndex1, AIndex2);
end;

procedure TLazRoundBufferListObjGen.DebugDump;
begin
  FList.DebugDump;
end;

function TLazRoundBufferListObjGen.Get(Index: Integer): T;
begin
  Result := FList.ItemPointer[Index]^;
end;

procedure TLazRoundBufferListObjGen.Put(Index: Integer; AValue: T);
begin
  FList.ItemPointer[Index]^ := AValue;
end;

function TLazRoundBufferListObjGen.IndexOf(AnItem: T): integer;
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
  Result := Min(ARequired * 2, ARequired + $8000);
end;

function TLazFixedRoundBufferListMemBase.ShrinkCapacity(ARequired: Integer): Integer;
begin
  Result := -1;
end;

procedure TLazFixedRoundBufferListMemBase.AdjustFirstItemOffset(ACount, AMask: Integer);
begin
  Mem.FirstItemIndex := (Mem.FirstItemIndex + ACount) and AMask;
end;

procedure TLazFixedRoundBufferListMemBase.InsertRowsAtStart(ACount, AMask: Integer);
begin
  Mem.FirstItemIndex := (Mem.FirstItemIndex - ACount) and AMask;
  Mem.Count := Mem.Count + ACount;
end;

procedure TLazFixedRoundBufferListMemBase.InsertRowsAtEnd(ACount: Integer);
begin
  Mem.Count := Mem.Count + ACount;
end;

procedure TLazFixedRoundBufferListMemBase.InsertRowsAtBoundary(AnAtStart: Boolean; ACount,
  AMask: Integer);
begin
  if AnAtStart then begin
    Mem.FirstItemIndex := (Mem.FirstItemIndex - ACount) and AMask;
  end;
  Mem.Count := Mem.Count + ACount;
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRowsAtStart(ACount, AMask: Integer);
begin
  Mem.FirstItemIndex := (Mem.FirstItemIndex + ACount) and AMask;
  Mem.Count := Mem.Count - ACount;
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRowsAtEnd(ACount: Integer);
begin
  Mem.Count := Mem.Count - ACount;
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRowsAtBoundary(AnAtStart: Boolean; ACount,
  AMask: Integer);
begin
  if AnAtStart then begin
    Mem.FirstItemIndex := (Mem.FirstItemIndex + ACount) and AMask;
  end;
  Mem.Count := Mem.Count - ACount;
end;

procedure TLazFixedRoundBufferListMemBase.MoveRowsToOther(AFromOffset, AToOffset, ACount, ACap: Integer; AnOther: TLazFixedRoundBufferListMemBase);
begin
  MoveBytesToOther(
    ((AFromOffset + Mem.FirstItemIndex) and (ACap-1)) * FItemSize.ItemSize,
    ((AToOffset + AnOther.Mem.FirstItemIndex) and (ACap-1)) * FItemSize.ItemSize,
    ACount * FItemSize.ItemSize,
    ACap * FItemSize.ItemSize,
    AnOther);
end;

procedure TLazFixedRoundBufferListMemBase.MoveBytesToOther(AFromByteOffset, AToByteOffset,
  AByteCount, AByteCap: Integer; AnOther: TLazFixedRoundBufferListMemBase);
var
  CSrc, CDst: Integer;
  PSource, PTarget, SrcHigh, DstHigh: PByte;
begin
  PSource := FMem.DataPointer;
  SrcHigh := PSource + AByteCap;
  PSource := PSource + AFromByteOffset;
  assert(PSource < SrcHigh, 'TLazFixedRoundBufferListMemBase.MoveBytesToOther: PSource < SrcHigh');
  //if PSource >= SrcHigh then PSource := PSource - AByteCap;

  PTarget := AnOther.FMem.DataPointer;
  DstHigh := PTarget + AByteCap;
  PTarget := PTarget + AToByteOffset;
  assert(PTarget < DstHigh, 'TLazFixedRoundBufferListMemBase.MoveBytesToOther: PTarget < DstHigh');
  //if PTarget >= DstHigh then PTarget := PTarget - AByteCap;

  CSrc := SrcHigh - PSource;
  CDst := DstHigh - PTarget;

  if CSrc > CDst then begin
    CDst := Min(CDst, AByteCount);
    Move(PSource^, PTarget^, CDst);
    AByteCount := AByteCount - CDst;
    if AByteCount = 0 then exit;
    PTarget := AnOther.FMem.DataPointer;
    PSource := PSource + CDst;

    CSrc := Min(SrcHigh - PSource, AByteCount);
    Move(PSource^, PTarget^, CSrc);
    AByteCount := AByteCount - CSrc;
    if AByteCount = 0 then exit;
    PSource := FMem.DataPointer;
    PTarget := PTarget + CSrc;
    Move(PSource^, PTarget^, AByteCount);
  end
  else if CSrc = CDst then begin
    CSrc := Min(CSrc, AByteCount);
    Move(PSource^, PTarget^, CSrc);
    AByteCount := AByteCount - CSrc;
    if AByteCount = 0 then exit;
    PSource := FMem.DataPointer;
    PTarget := AnOther.FMem.DataPointer;
    Move(PSource^, PTarget^, AByteCount);
  end
  else begin
    CSrc := Min(CSrc, AByteCount);
    Move(PSource^, PTarget^, CSrc);
    AByteCount := AByteCount - CSrc;
    if AByteCount = 0 then exit;
    PSource := FMem.DataPointer;
    PTarget := PTarget + CSrc;

    CDst := Min(DstHigh - PTarget, AByteCount);
    Move(PSource^, PTarget^, CDst);
    AByteCount := AByteCount - CDst;
    if AByteCount = 0 then exit;
    PTarget := AnOther.FMem.DataPointer;
    PSource := PSource + CDst;
    Move(PSource^, PTarget^, AByteCount);
  end;
end;

function TLazFixedRoundBufferListMemBase.GetItemPointerMasked(AnIndex, AMask: Integer): TPItemT;
begin
  Result := TPItemT(
    FMem.DataPointer + ((AnIndex + Mem.FirstItemIndex) and AMask) * FItemSize.ItemSize
  );
end;

function TLazFixedRoundBufferListMemBase.GetItemByteOffsetMasked(AnIndex,
  AMask: Integer): Integer;
begin
  Result := ((AnIndex + Mem.FirstItemIndex) and AMask) * FItemSize.ItemSize;
end;

function TLazFixedRoundBufferListMemBase.GetFirstItemByteOffset: Integer;
begin
  Result := Mem.FirstItemIndex * FItemSize.ItemSize;
end;

procedure TLazFixedRoundBufferListMemBase.Create(AItemSize: TSizeT; ACapacity: Integer);
begin
  inherited Create;
  FItemSize := AItemSize;
  SetCapacity(ACapacity);
end;

function TLazFixedRoundBufferListMemBase.InsertRows(AIndex, ACount: Integer): TPItemT;
begin
  Result := InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

{ TLazPagedListObjBase }

function TLazPagedListObjBase.GetPageSubIdx(Index: Integer): Integer;
begin
  Result := Cardinal(Index+FFirstPageEmpty) and FPageSizeMask;
end;

function TLazPagedListObjBase.GetPagePointer(PageIndex: Integer): PPageType;
begin
  assert((PageIndex >= 0) and (PageIndex < PageCount), 'TLazPagedListObjBase.GetPageSubIdx: (PageIndex >= 0) and (PageIndex < PageCount)');
  Result := FPages.ItemPointerFast[PageIndex];
end;

function TLazPagedListObjBase.GetItemPageIdx(Index: Integer): Integer;
begin
  Result := (Index+FFirstPageEmpty) >> FPageSizeExp;
end;

function TLazPagedListObjBase.GetItemPointer(Index: Integer): TPItemT;
var
  p: PPageType;
  i: Integer;
begin
  assert((Index>=0) and (Index<FCount), 'TLazPagedListObjBase.GetItemPointer: (Index>=0) and (Index<FCount)');
  i := Index + FFirstPageEmpty;
  p := FPages.ItemPointerFast[i >> FPageSizeExp];
  assert(p<>nil, 'TLazPagedListObjBase.GetItemPointer: p<>nil');
  Result := p^.ItemPointerMasked[Cardinal(i), FPageSizeMask];
end;

procedure TLazPagedListObjBase.SetCapacity(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  AValue := (AValue + FPageSizeMask) >> FPageSizeExp;
  if AValue <= FPages.Count then
    exit;
  FPages.Capacity := AValue;
end;

function TLazPagedListObjBase.GetCapacity: Integer;
begin
  Result := FPages.Capacity << FPageSizeExp;
end;

function TLazPagedListObjBase.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TLazPagedListObjBase.JoinPageWithNext(APageIdx, AJoinEntryIdx,
  AnExtraDelPages: Integer);
var
  PCap: Integer;
begin
  // delete last page(s), if AJoinEntryIdx=0 // next page does not need to exist
  PCap := FPageSizeMask + 1;
  if AJoinEntryIdx * 2 <= FPageSizeMask then begin
    if AJoinEntryIdx > 0 then
      PagePointer[APageIdx]^.MoveRowsToOther(0, 0,
        AJoinEntryIdx, PCap, PagePointer[APageIdx + 1 + AnExtraDelPages]^);
    DeletePages(APageIdx, 1 + AnExtraDelPages);
  end
  else begin
    PagePointer[APageIdx + 1 + AnExtraDelPages]^.MoveRowsToOther(AJoinEntryIdx, AJoinEntryIdx,
      PCap - AJoinEntryIdx, PCap, PagePointer[APageIdx]^);
    DeletePages(APageIdx + 1, 1 + AnExtraDelPages);
  end;
end;

procedure TLazPagedListObjBase.SplitPageToFront(ASourcePageIdx, ASplitAtIdx,
  AnExtraPages: Integer; AExtraCapacityNeeded: Integer);
begin
  // Can split the none-existing page[pagecount], IF ASplitAtIdx=0 // simply insert a page
  InsertFilledPages(ASourcePageIdx, AnExtraPages+1, AExtraCapacityNeeded);
  if ASplitAtIdx > 0 then
    PagePointer[ASourcePageIdx + AnExtraPages + 1]^.MoveRowsToOther(0, 0, ASplitAtIdx, FPageSizeMask+1, PagePointer[ASourcePageIdx]^);
end;

procedure TLazPagedListObjBase.SplitPageToBack(ASourcePageIdx, ASplitAtIdx,
  AnExtraPages: Integer; AExtraCapacityNeeded: Integer);
var
  c: Integer;
begin
  InsertFilledPages(ASourcePageIdx+1, AnExtraPages+1, AExtraCapacityNeeded);
  c := FPageSizeMask + 1 - ASplitAtIdx;
  if c > 0 then
    PagePointer[ASourcePageIdx]^.MoveRowsToOther(ASplitAtIdx, ASplitAtIdx, c, FPageSizeMask+1, PagePointer[ASourcePageIdx + AnExtraPages + 1]^);
end;

procedure TLazPagedListObjBase.SplitPage(ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer;
  AExtraCapacityNeeded: Integer);
begin
  if ASplitAtIdx <= FPageSizeMask >> 1 then
    SplitPageToFront(ASourcePageIdx, ASplitAtIdx, AnExtraPages, AExtraCapacityNeeded)
  else
    SplitPageToBack(ASourcePageIdx, ASplitAtIdx, AnExtraPages, AExtraCapacityNeeded);
end;

procedure TLazPagedListObjBase.BubbleEntriesDown(ASourceStartIdx, ATargetEndIdx,
  AnEntryCount: Integer);
var
  HighPage, LowPage: PPageType;
  ReverseEntryCount, PageCapacity: Integer;
begin
  PageCapacity := FPageSizeMask + 1;
    (*   *** Reverse Bubble

    bubble 900 down
      |                    |                    |                    |                    |
       AA------------------ xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxBB
       xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxBB AA------------------   // page up
       xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxBB ------------------AA   // adjust (rotated page)
         xxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx BB----------------AA   // bubbled 100 up // and adjust
       AAxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx BB----------------AA   // moved/restored
       AAxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx ------------------BB   // adjust (rotated page)

    *)
  if AnEntryCount*2 > PageCapacity then begin
    ReverseEntryCount := PageCapacity - AnEntryCount;
    SwapPagesUp(ATargetEndIdx, ASourceStartIdx, ASourceStartIdx);      // swapped
    HighPage := PagePointer[ASourceStartIdx];
    LowPage := PagePointer[ATargetEndIdx];
    HighPage^.AdjustFirstItemOffset(ReverseEntryCount, FPageSizeMask); // rotate
    InternalBubbleEntriesUp(ATargetEndIdx, ASourceStartIdx, ReverseEntryCount); // bubble
    LowPage^.AdjustFirstItemOffset(-ReverseEntryCount, FPageSizeMask); // adjust after bubble
    HighPage^.MoveRowsToOther(AnEntryCount, 0, ReverseEntryCount, PageCapacity, LowPage^); // moved/restored
    // TODO: the caller may just undo this last rotate....
    HighPage^.AdjustFirstItemOffset(ReverseEntryCount, FPageSizeMask); // rotated page
  end
  else
    InternalBubbleEntriesDown(ASourceStartIdx, ATargetEndIdx, AnEntryCount);
end;

procedure TLazPagedListObjBase.BubbleEntriesUp(ASourceStartIdx, ATargetEndIdx,
  AnEntryCount: Integer);
var
  LowPage, HighPage: PPageType;
  ReverseEntryCount, PageCapacity: Integer;
begin
  PageCapacity := FPageSizeMask + 1;
  if AnEntryCount*2 > PageCapacity then begin
    (*   *** Reverse Bubble
    l: Sourcte
    T: Target  // E: Target end

    bubble 900 up
      |  l                 |T                E  |                    |                    |
       AAxxxxxxxxxxxxxxxxxx ------------------BB
       ------------------BB AAxxxxxxxxxxxxxxxxxx     // page down
       BB------------------ AAxxxxxxxxxxxxxxxxxx     // adjust (rotated page)
       BB----------------AA xxxxxxxxxxxxxxxxxx       // bubbled 100 down // and adjust
       ------------------AA xxxxxxxxxxxxxxxxxxBB     // moved/restored
       AA------------------ xxxxxxxxxxxxxxxxxxBB     // adjust (rotated page)

    bubble 900 up
      |  l                 |T                   |                 E  |                    |
       AAxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx ------------------BB
       ------------------BB AAxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx   // 1 page down: E > l
       BB------------------ AAxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx   // adjust (rotated page)
       BB----------------AA xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxx     // bubbled 100 down // and adjust
       ------------------AA xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxBB   // moved/restored
       AA------------------ xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxBB   // adjust (rotated page)

    *)
    ReverseEntryCount := PageCapacity - AnEntryCount;
    SwapPagesDown(ATargetEndIdx, ASourceStartIdx, ASourceStartIdx);     // swapped
    LowPage := PagePointer[ASourceStartIdx];
    HighPage := PagePointer[ATargetEndIdx];
    LowPage^.AdjustFirstItemOffset(-ReverseEntryCount, FPageSizeMask); // rotate page
    InternalBubbleEntriesDown(ATargetEndIdx, ASourceStartIdx, ReverseEntryCount); // bubble
    HighPage^.AdjustFirstItemOffset(ReverseEntryCount, FPageSizeMask); // adjust after bubble
    LowPage^.MoveRowsToOther(0, AnEntryCount, ReverseEntryCount, PageCapacity, HighPage^); // moved/restored
    // TODO: the caller may just undo this last rotate....
    LowPage^.AdjustFirstItemOffset(-ReverseEntryCount, FPageSizeMask); // rotate page
  end
  else
    InternalBubbleEntriesUp(ASourceStartIdx, ATargetEndIdx, AnEntryCount);
end;

procedure TLazPagedListObjBase.InternalBubbleEntriesDown(ASourceStartIdx, ATargetEndIdx,
  AnEntryCount: Integer);
var
  CurPage, NextPage: PPageType;
  i, PageCapacity, PageCapacityBytes, CountBytes: Integer;
begin
  assert(ASourceStartIdx > ATargetEndIdx, 'TLazPagedListObjBase.InternalBubbleEntriesDown: ASourceStartIdx > ATargetEndIdx');

  PageCapacity := (FPageSizeMask+1);
  PageCapacityBytes := PageCapacity * FItemSize.ItemSize;
  CountBytes := AnEntryCount * FItemSize.ItemSize;

  CurPage := PagePointer[ATargetEndIdx];
  NextPage := CurPage + 1;
  NextPage^.MoveBytesToOther(NextPage^.GetFirstItemByteOffset,
    CurPage^.GetItemByteOffsetMasked(CurPage^.Count - AnEntryCount, FPageSizeMask),
    CountBytes, PageCapacityBytes, CurPage^);
  if ASourceStartIdx - ATargetEndIdx = 1 then
    exit;
  NextPage^.AdjustFirstItemOffset(AnEntryCount, FPageSizeMask);

  CurPage := NextPage;
  for i := 0 to ASourceStartIdx - ATargetEndIdx - 3 do begin
    NextPage := CurPage + 1;
    NextPage^.MoveBytesToOther(NextPage^.GetFirstItemByteOffset,
      CurPage^.GetItemByteOffsetMasked(PageCapacity - AnEntryCount, FPageSizeMask),
      CountBytes, PageCapacityBytes, CurPage^);
    NextPage^.AdjustFirstItemOffset(AnEntryCount, FPageSizeMask);
    CurPage := NextPage;
  end;

  NextPage := CurPage + 1;
  NextPage^.MoveBytesToOther(NextPage^.GetFirstItemByteOffset,
    CurPage^.GetItemByteOffsetMasked(PageCapacity - AnEntryCount, FPageSizeMask),
    CountBytes, PageCapacityBytes, CurPage^);
end;

procedure TLazPagedListObjBase.InternalBubbleEntriesUp(ASourceStartIdx, ATargetEndIdx,
  AnEntryCount: Integer);
var
  CurPage, NextPage: PPageType;
  i, PageCapacity, PageCapacityBytes, CountBytes: Integer;
begin
  assert(ASourceStartIdx < ATargetEndIdx, 'TLazPagedListObjBase.InternalBubbleEntriesUp: ASourceStartIdx < ATargetEndIdx');

  PageCapacity := (FPageSizeMask+1);
  PageCapacityBytes := PageCapacity * FItemSize.ItemSize;
  CountBytes := AnEntryCount * FItemSize.ItemSize;

  CurPage := PagePointer[ATargetEndIdx];
  for i := 0 to ATargetEndIdx - ASourceStartIdx - 2 do begin
    NextPage := CurPage - 1;
    NextPage^.MoveBytesToOther(NextPage^.GetItemByteOffsetMasked(PageCapacity - AnEntryCount, FPageSizeMask),
      CurPage^.GetFirstItemByteOffset,
      CountBytes, PageCapacityBytes, CurPage^);
    NextPage^.AdjustFirstItemOffset(-AnEntryCount, FPageSizeMask);
    CurPage := NextPage;
  end;

  NextPage := CurPage - 1;
  NextPage^.MoveBytesToOther(NextPage^.GetItemByteOffsetMasked(NextPage^.Count - AnEntryCount, FPageSizeMask),
    CurPage^.GetFirstItemByteOffset,
    CountBytes, PageCapacityBytes, CurPage^);
end;

procedure TLazPagedListObjBase.SwapPagesUp(ASourceStartIndex, ATargetStartIndex,
  ATargetEndIndex: Integer);
var
  Cnt, Diff: Integer;
  TempPages: Array of TPageType;
begin
  Cnt := ATargetEndIndex - ATargetStartIndex + 1;
  Diff := ATargetStartIndex - ASourceStartIndex;
  assert(Diff > 0, 'TLazPagedListObjBase.MoveRows: Diff > 0');

  if Diff > Cnt then begin
    SetLength(TempPages, Cnt);
    move(PagePointer[ASourceStartIndex]^, TempPages[0], Cnt * SizeOf(TempPages[0]));
    FPages.MoveRows(ASourceStartIndex + Cnt, ASourceStartIndex, Diff);
    move(TempPages[0], PagePointer[ATargetStartIndex]^, Cnt * SizeOf(TempPages[0]));
  end else begin
    SetLength(TempPages, Diff);
    move(PagePointer[ATargetEndIndex - Diff + 1]^, TempPages[0], Diff * SizeOf(TempPages[0]));
    FPages.MoveRows(ASourceStartIndex, ATargetStartIndex, Cnt);
    move(TempPages[0], PagePointer[ASourceStartIndex]^, Diff * SizeOf(TempPages[0]));
  end;
end;

procedure TLazPagedListObjBase.SwapPagesDown(ASourceStartIndex, ATargetStartIndex,
  ATargetEndIndex: Integer);
var
  Cnt, Diff: Integer;
  TempPages: Array of TPageType;
begin
  Cnt := ATargetEndIndex - ATargetStartIndex + 1;
  Diff := ASourceStartIndex - ATargetStartIndex;
  assert(Diff > 0, 'TLazPagedListObjBase.MoveRows: Diff > 0');

  if Diff > Cnt then begin
    SetLength(TempPages, Cnt);
    move(PagePointer[ASourceStartIndex]^, TempPages[0], Cnt * SizeOf(TempPages[0]));
    FPages.MoveRows(ATargetStartIndex, ATargetStartIndex + Cnt, Diff);
    move(TempPages[0], PagePointer[ATargetStartIndex]^, Cnt * SizeOf(TempPages[0]));
  end else begin
    SetLength(TempPages, Diff);
    move(PagePointer[ATargetStartIndex]^, TempPages[0], Diff * SizeOf(TempPages[0]));
    FPages.MoveRows(ASourceStartIndex, ATargetStartIndex, Cnt);
    move(TempPages[0], PagePointer[ASourceStartIndex + Cnt - Diff]^, Diff * SizeOf(TempPages[0]));
  end;
end;

function TLazPagedListObjBase.GrowCapacity(ARequiredPages: Integer): Integer;
begin
  ARequiredPages := ARequiredPages + FExtraCapacityNeeded;
  FExtraCapacityNeeded := 0;
  if assigned(FGrowProc) then
    Result := FGrowProc(ARequiredPages)
  else
    Result := Min(ARequiredPages * 2, ARequiredPages + $8000);
end;

function TLazPagedListObjBase.ShrinkCapacity(ARequiredPages: Integer): Integer;
begin
  assert(ARequiredPages <= FPages.Capacity, 'TLazShiftBufferListObjBase.ShrinkCapacity: ARequired <= FPages.Capacity');
  if assigned(FShrinkProc) then
    Result := FShrinkProc(ARequiredPages)
  else
  if ARequiredPages * 4 < FPages.Capacity then
    Result := ARequiredPages * 2
  else
    Result := -1;
end;

procedure TLazPagedListObjBase.InsertFilledPages(AIndex, ACount: Integer;
  AExtraCapacityNeeded: Integer);
var
  i, c, h: Integer;
  p: PPageType;
begin
  FExtraCapacityNeeded := AExtraCapacityNeeded;
  p := FPages.InsertRowsEx(AIndex, ACount, @GrowCapacity);
  c := FPageSizeMask + 1;
  h := AIndex + ACount - 1;
  for i := AIndex to h do begin
    p^.Create(FItemSize, c);
    p^.InsertRowsAtEnd(c);
    inc(p);
  end;
end;

procedure TLazPagedListObjBase.DeletePages(AIndex, ACount: Integer);
var
  i: Integer;
  p: PPageType;
begin
  p := PagePointer[AIndex]; // pages are NOT a roundbuffer
  for i := AIndex to AIndex + ACount - 1 do begin
    //PagePointer[i]^.Destroy;
    p^.Destroy;
    inc(p);
  end;
  FPages.DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

procedure TLazPagedListObjBase.Create(APageSizeExp: Integer);
begin
  FPageSizeExp := APageSizeExp;
  FPageSizeMask := Integer(not(Cardinal(-1) << FPageSizeExp));
  FCount := 0;
  FFirstPageEmpty := 0;
  FPages.Create;
  FGrowProc := nil;
  FShrinkProc := nil;
end;

procedure TLazPagedListObjBase.Destroy;
begin
  if FCount > 0 then
    DeletePages(0, PageCount);
  FPages.Destroy;
end;

procedure TLazPagedListObjBase.InsertRows(AIndex, ACount: Integer);
var
  ExtraPagesNeeded, SubIndex, SubCount: Integer;
  PgCnt, PCap, InsertPageIdx, c,
    AIndexAdj, TmpDeleteRows, LastPageEmpty: Integer;
begin
  assert((AIndex >= 0) and (AIndex <= FCount), 'TLazPagedListObjBase.InsertRows: (AIndex >= 0) and (AIndex <= FCount)');
  if ACount <= 0 then
    exit;
  PCap := FPageSizeMask + 1;
  PgCnt := PageCount;
  FCount := FCount + ACount;
  SubCount := ACount and FPageSizeMask;

//DebugLn();debugln(['***### TLazPagedListObjBase.InsertRows Idx:',AIndex,' cnt:',ACount, ' Pcnt:',PgCnt, ' fcnt:', FCount, '  FFirstPageEmpty:', FFirstPageEmpty]);DebugDump;
  if PgCnt = 0 then begin
    // No data yet
    ExtraPagesNeeded := ((ACount-1) >> FPageSizeExp) + 1;
    InsertFilledPages(0, ExtraPagesNeeded);
    FFirstPageEmpty := (PCap - SubCount) and FPageSizeMask;
    FFirstPageEmpty := FFirstPageEmpty div 2; // keep some capacity in the last node too
  assert((((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TLazPagedListObjBase.InsertRows: (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>0)');
    exit;
  end
  else if (FCount <= PCap) and (PgCnt = 1) then begin
    // keep it in one page
    FFirstPageEmpty := FFirstPageEmpty - SubCount;
    if FFirstPageEmpty < 0 then begin
      PagePointer[0]^.AdjustFirstItemOffset(FFirstPageEmpty, FPageSizeMask);
      FFirstPageEmpty := 0;
    end;
    if AIndex > 0 then
      MoveRows(SubCount, 0, AIndex);
  assert((((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TLazPagedListObjBase.InsertRows: (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>0)');
    exit;
  end;

  AIndexAdj := AIndex + FFirstPageEmpty;
  InsertPageIdx := (AIndexAdj) >> FPageSizeExp;
  SubIndex := AIndexAdj and FPageSizeMask;

  ExtraPagesNeeded := ACount - SubCount;
  if (ExtraPagesNeeded > 0) then
    ExtraPagesNeeded := ExtraPagesNeeded >> FPageSizeExp;

  If Cardinal(InsertPageIdx) * 2 <= Cardinal(PgCnt) then begin
    if SubCount * 2 <= PCap then begin
      if SubCount > 0 then begin
        FFirstPageEmpty := FFirstPageEmpty - SubCount;
        if FFirstPageEmpty < 0 then begin
          InsertFilledPages(0, 1);
          FFirstPageEmpty := FFirstPageEmpty + PCap;
          inc(InsertPageIdx);
          assert(FFirstPageEmpty>0, 'TLazPagedListObjBase.InsertRows: FFirstPageEmpty>0');
        end;
        if AIndex > 0 then
          MoveRows(SubCount, 0, AIndex);
      end;
      if ExtraPagesNeeded > 0 then
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded - 1);
    end
    else begin
      SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded);
      TmpDeleteRows := PCap - SubCount;
      assert(TmpDeleteRows>0, 'TLazPagedListObjBase.InsertRows: TmpDeleteRows>0');
      if AIndex > 0 then
        MoveRows(0, TmpDeleteRows, AIndex);
      FFirstPageEmpty := FFirstPageEmpty + TmpDeleteRows;
      if FFirstPageEmpty > PCap then begin
        FFirstPageEmpty := FFirstPageEmpty - PCap;
        DeletePages(0, 1);
      end;
    end;
  end
  else begin
    c := FCount - ACount;
    LastPageEmpty := (PCap - (c + FFirstPageEmpty)) and FPageSizeMask;
    assert(LastPageEmpty >= 0, 'TLazPagedListObjBase.InsertRows: LastPageEmpty >= 0');
    if SubCount * 2 <= PCap then begin
      if SubCount > 0 then begin
        if LastPageEmpty < SubCount then
          InsertFilledPages(PgCnt, 1);
        if AIndex < c then begin
          MoveRows(AIndex, AIndex + SubCount, c - AIndex);
        end;
      end;
      if ExtraPagesNeeded > 0 then
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded - 1);
    end
    else begin
      TmpDeleteRows := PCap - SubCount;
      SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded);
      assert(TmpDeleteRows>0, 'TLazPagedListObjBase.InsertRows: TmpDeleteRows>0');
      if AIndex < c then begin
        c := FCount + TmpDeleteRows;
        AIndex := AIndex + ((ExtraPagesNeeded + 1) << FPageSizeExp);
        MoveRows(AIndex, AIndex - TmpDeleteRows, c - AIndex);
      end;
      if LastPageEmpty + TmpDeleteRows >= PCap then
        DeletePages(PageCount-1, 1);
    end;
  end;
//debugln('<<< INS done'); DebugDump;
  assert((((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TLazPagedListObjBase.InsertRows: (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>0)');
end;

procedure TLazPagedListObjBase.DeleteRows(AIndex, ACount: Integer);
var
  PCap, c: Integer;
  DelPageIdx, SubIndex, AIndexAdj, SubCount, ExtraPagesNeeded, LastPageUsed, TmpInsertRows: Integer;
begin
//  debugln(['<<<<<<< TLazPagedListObjBase.DeleteRows ', AIndex, ', ', ACount]); DebugDump;
  assert((AIndex >= 0) and (AIndex + ACount <= FCount), 'TLazPagedListObjBase.DeleteRows: (AIndex >= 0) and (AIndex + ACount <= FCount)');
  if ACount <= 0 then
    exit;
  PCap := FPageSizeMask + 1;
  FCount := FCount - ACount;

  AIndexAdj := AIndex + FFirstPageEmpty;
  DelPageIdx:= (AIndexAdj) >> FPageSizeExp;
  SubIndex := AIndexAdj and FPageSizeMask;

  SubCount := ACount and FPageSizeMask;
  ExtraPagesNeeded := ACount - SubCount;
  if (ExtraPagesNeeded > 0) then
    ExtraPagesNeeded := ExtraPagesNeeded >> FPageSizeExp;

  If Cardinal(DelPageIdx) * 2 < Cardinal(PageCount) then begin
    if (SubCount * 2 <= PCap) or (PageCount = 1) then begin
      if ExtraPagesNeeded > 0 then
        JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesNeeded - 1);
      if (SubCount > 0) then begin
        if (AIndex > 0) then
          MoveRows(0, SubCount, AIndex);
        LastPageUsed := PCap - FFirstPageEmpty;
        if LastPageUsed > SubCount then begin
          FFirstPageEmpty := FFirstPageEmpty + SubCount;
        end
        else begin
          DeletePages(0, 1);
          FFirstPageEmpty := SubCount - LastPageUsed;
        end;
assert(FFirstPageEmpty<PCap, 'TLazPagedListObjBase.DeleteRows: FFirstPageEmpty<PCap');
      end;
    end
    else begin
      TmpInsertRows := PCap - SubCount;
      assert(TmpInsertRows>0, 'TLazPagedListObjBase.DeleteRows: TmpInsertRows>0');
      FFirstPageEmpty := FFirstPageEmpty - TmpInsertRows;
      if FFirstPageEmpty < 0 then begin
        InsertFilledPages(0, 1); // TODO: what if this needs capacity??
        FFirstPageEmpty := FFirstPageEmpty + PCap;
        inc(DelPageIdx);
      end;
      if (AIndex > 0) then
        MoveRows(TmpInsertRows, 0, AIndex);
      SubIndex := SubIndex - TmpInsertRows;
      if SubIndex < 0 then begin
        SubIndex := SubIndex + PCap;
        DelPageIdx := DelPageIdx - 1;
      end;
      JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesNeeded);
    end;
  end
  else begin
    c := FCount + ACount;
    LastPageUsed := (c + FFirstPageEmpty) and FPageSizeMask;
    if LastPageUsed = 0 then LastPageUsed := PCap;
    if SubCount * 2 <= PCap then begin
      if ExtraPagesNeeded > 0 then begin
        JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesNeeded - 1);
        c := c - (ExtraPagesNeeded << FPageSizeExp);
      end;
      if (SubCount > 0) then begin
        if (c - AIndex - SubCount > 0) then
          MoveRows(AIndex + SubCount, AIndex, c - AIndex - SubCount);
        if LastPageUsed <= SubCount then
          DeletePages(PageCount - 1, 1);
      end;
    end
    else begin
      TmpInsertRows := PCap - SubCount;
      assert(TmpInsertRows>0, 'TLazPagedListObjBase.DeleteRows: TmpInsertRows>0');
      if LastPageUsed + TmpInsertRows > PCap then
        InsertFilledPages(PageCount, 1); // TODO: what if this needs capacity??
      AIndex := AIndex + SubCount;
      if (AIndex < c) then
        MoveRows(AIndex, AIndex + TmpInsertRows, c - AIndex);
      JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesNeeded);
    end;
  end;
//debugln([' DEL DONE <<<<<<<<< ']);DebugDump;
  if (FCount = 0) and (PageCount > 0) then
    DeletePages(0, 1);
assert((FPages.Count =0) or (FCount>0), 'TLazPagedListObjBase.DeleteRows: (FPages.Count =0) or (FCount>0)');
assert((FPages.Count=0)or(((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TLazPagedListObjBase.DeleteRows: (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSizeMask) and (((FPages.Count << FPageSizeExp)-FFirstPageEmpty-FCount)>0)');
end;

procedure TLazPagedListObjBase.InternalMoveRowsDown(AFromIndex, AToIndex, ACount: Integer);
var
  FromIndexAdj, ToIndexAdj: Integer;
  SourceStartPage, SourceStartSubIndex, TargetStartPage, TargetStartSubIndex: Integer;
  TargetEndPage, TargetEndSubIndex: Integer;
  Diff, PageCapacity, MovePgCnt, BblCnt: Integer;
  c, p1, p2: Integer;
  SrcPage, DstPage: PPageType;
begin
  PageCapacity := FPageSizeMask+1;

  (* Calculate FROM page, and index in page *)
  FromIndexAdj := AFromIndex + FFirstPageEmpty;
  SourceStartPage     := FromIndexAdj >> FPageSizeExp;
  SourceStartSubIndex := FromIndexAdj and FPageSizeMask;

  (* Calculate TO page, and index in page *)
  ToIndexAdj := AToIndex + FFirstPageEmpty;
  TargetStartPage     := ToIndexAdj >> FPageSizeExp;
    TargetStartSubIndex := ToIndexAdj and FPageSizeMask;

  ToIndexAdj := ToIndexAdj + aCount - 1;
  TargetEndPage     := ToIndexAdj >> FPageSizeExp;
  TargetEndSubIndex := (ToIndexAdj and FPageSizeMask) + 1; // NEXT

//debugln(['####******************** MoveRows DOWN ',AFromIndex, ' to ', AToIndex, ' cnt=',ACount, '   SourceStartPage=',SourceStartPage, ' SourceStartSubIndex=',SourceStartSubIndex, '   //  TargetStartPage=',TargetStartPage, ' TargetStartSubIndex=',TargetStartSubIndex,  ' / TargetEndPage=',TargetEndPage, ' TargetEndSubIndex=',TargetEndSubIndex]);{--}DebugDump;

  Diff := AFromIndex - AToIndex;

  (* The "gap" in the target-end page may be bigger/smaller than BubbleCnt.
   * (BubbleCnt is "Diff and FPageSizeMask")
   * Move appropriate amounts of entries into the target-end page.
   * If BubbleCnt=0, then this will leave no "gap" in the target-end page.
   *)
  c := Min(ACount, PageCapacity - SourceStartSubIndex);
  DstPage := PagePointer[TargetStartPage];
  if TargetStartPage = SourceStartPage then begin
    // Example: 1
    DstPage^.MoveRows(SourceStartSubIndex, TargetStartSubIndex, c);
    SourceStartSubIndex := SourceStartSubIndex + c;
    TargetStartSubIndex := TargetStartSubIndex + c;  // TargetStartSubIndex is smaller than c
  end
  else begin
    // Example: 3
    c := Min(c, PageCapacity - TargetStartSubIndex);
    SrcPage := PagePointer[SourceStartPage];
    SrcPage^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex, c, PageCapacity, DstPage^);
    SourceStartSubIndex := SourceStartSubIndex + c;
    TargetStartSubIndex := TargetStartSubIndex + c;
  end;

  ACount := ACount - c;
  if ACount = 0 then
    exit;

  if SourceStartSubIndex = PageCapacity then begin
    SourceStartSubIndex := 0;
    Inc(SourceStartPage);
  end;
  if TargetStartSubIndex = PageCapacity then begin
    TargetStartSubIndex := 0;
    Inc(TargetStartPage);
  end;
  assert((TargetStartSubIndex = 0) or (PageCapacity - TargetStartSubIndex = Diff and FPageSizeMask), 'TLazPagedListObjBase.MoveRows: TargetStartSubIndex at end-of-cell OR at BubbleCnt');
  assert((TargetStartSubIndex = 0) or (SourceStartSubIndex = 0), 'TLazPagedListObjBase.MoveRows: (TargetStartSubIndex = PageCapacity) or (SourceStartSubIndex = PageCapacity)');


  // Full Pages AND/OR bubble
  if (TargetStartPage < TargetEndPage) then begin
    (*
     * Move full pages, if needed
     *)
    p1 := TargetStartPage;
    if TargetStartSubIndex > 0 then inc(p1);
    p2 := TargetEndPage;
    if TargetEndSubIndex < PageCapacity then dec(p2); // TODO: TargetEndSubIndex < TargetPage.Count
    MovePgCnt := p2 - p1 + 1;
    if (SourceStartPage > p1) and (MovePgCnt > 0) then begin
      SwapPagesDown(SourceStartPage, p1, p2);
//debugln(['SWAP:',SourceStartPage,',  ',p1,',',p2]);{--}DebugDump;
    end;


    (*
     * Bubble
     * or adjust Source/Target... for the move // (p2 <= TargetEndPage)
     *)
    BblCnt := Diff and FPageSizeMask;
    if (BblCnt > 0) and ((p2 > TargetStartPage) or (TargetStartSubIndex = 0)) then begin
      assert((p1 = TargetStartPage) or (PageCapacity - TargetStartSubIndex = BblCnt), 'TLazPagedListObjBase.InternalMoveRowsDown: (p2 = TargetEndPage) or (PageCapacity TargetStartSubIndex = BblCnt)');
      if TargetStartSubIndex = 0 then begin
        // Example: 3  // may or may not bubble // maybe only adjust
        PagePointer[TargetStartPage]^.AdjustFirstItemOffset(BblCnt, FPageSizeMask);
        dec(MovePgCnt);  // only affects how "ACount" is adjusted
        SourceStartSubIndex := SourceStartSubIndex + (PageCapacity - BblCnt);
        if SourceStartSubIndex >= PageCapacity then begin
          SourceStartSubIndex := SourceStartSubIndex - PageCapacity;
          Inc(SourceStartPage);
        end;
        ACount := ACount - (PageCapacity - BblCnt);
        TargetStartSubIndex := PageCapacity - BblCnt;
      end;

      if p2 > TargetStartPage then begin
        BubbleEntriesDown(p2, TargetStartPage, BblCnt);
        PagePointer[p2]^.AdjustFirstItemOffset(BblCnt, FPageSizeMask);
      end;
    end;


    if MovePgCnt > 0 then begin
      ACount := ACount - (MovePgCnt << FPageSizeExp);
      assert(ACount>=0, 'TLazPagedListObjBase.InternalMoveRowsDown: ACount>=0');
      if ACount = 0 then
        exit;
      TargetStartPage := TargetStartPage + MovePgCnt;
      SourceStartPage := SourceStartPage + MovePgCnt;
    end;
  end;
//{--}DebugDump;
  assert((TargetEndPage-TargetStartPage<=1), 'TLazPagedListObjBase.InternalMoveRowsDown / Bubbled all but the last page: (TargetEndPage-TargetStartPage<=1)');


  // move
  while ACount > 0 do begin
    c := min(min(PageCapacity - TargetStartSubIndex, PageCapacity - SourceStartSubIndex), ACount);
    assert(c>0, 'TLazPagedListObjBase.MoveRows: c>0');
    SrcPage := PagePointer[SourceStartPage];
    if SourceStartPage = TargetStartPage then
      SrcPage^.MoveRows(SourceStartSubIndex, TargetStartSubIndex, c)
    else
      SrcPage^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex, c, PageCapacity, PagePointer[TargetStartPage]^);

    ACount := ACount - c;
    if ACount = 0 then
      exit;

    TargetStartSubIndex := TargetStartSubIndex + c;
    SourceStartSubIndex := SourceStartSubIndex + c;
    if SourceStartSubIndex = PageCapacity then begin
      SourceStartSubIndex := 0;
      Inc(SourceStartPage);
    end;
    if TargetStartSubIndex = PageCapacity then begin
      TargetStartSubIndex := 0;
      Inc(TargetStartPage);
    end;
  end;

end;

procedure TLazPagedListObjBase.InternalMoveRowsUp(AFromIndex, AToIndex, ACount: Integer);
var
  FromIndexAdj, ToIndexAdj: Integer;
  TargetStartPage, TargetStartSubIndex: Integer;
  SourceEndPage, SourceEndSubIndex, TargetEndPage, TargetEndSubIndex: Integer;
  Diff, PageCapacity, MovePgCnt, BblCnt: Integer;
  c, p1, p2: Integer;
  SrcPage, DstPage: PPageType;
begin
  PageCapacity := FPageSizeMask+1;

  (* Calculate FROM page, and index in page *)
  FromIndexAdj := AFromIndex + FFirstPageEmpty + aCount - 1;
  SourceEndPage       := FromIndexAdj >> FPageSizeExp;
  SourceEndSubIndex   := (FromIndexAdj and FPageSizeMask) + 1; // NEXT

  (* Calculate TO page, and index in page *)
  ToIndexAdj := AToIndex + FFirstPageEmpty;
  TargetStartPage     := ToIndexAdj >> FPageSizeExp;
  TargetStartSubIndex := ToIndexAdj and FPageSizeMask;

  ToIndexAdj := ToIndexAdj + aCount - 1;
  TargetEndPage     := ToIndexAdj >> FPageSizeExp;
  TargetEndSubIndex := (ToIndexAdj and FPageSizeMask) + 1; // NEXT

//debugln(['******************** MoveRows UP ',AFromIndex, ' to ', AToIndex, ' cnt=',ACount, ' / SourceEndPage=',SourceEndPage, ' SourceEndSubIndex=',SourceEndSubIndex, '  //  TargetStartPage=',TargetStartPage, ' TargetStartSubIndex=',TargetStartSubIndex,  ' / TargetEndPage=',TargetEndPage, ' TargetEndSubIndex=',TargetEndSubIndex]);{--}DebugDump;

  Diff := AToIndex - AFromIndex;

  (* Examples

1)  move up cnt=2300 from 3850-6150 to 4150-6450  dist 300
         |                    |                    |                    |                    |
      xxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx xxx
      xxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx       $$$  // moved 150
      xxx       $$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$$$ $$$$$$$$$  // bubbled 300
             $$$$$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$$$ $$$$$$$$$  // moved 150

2)  move up cnt=2300 from 3850-6150 to 5150-7450  dist 1300
         |                    |                    |                    |                    |
      xxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx xxx
      xxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx                            $$$  // moved 150
      xxx                      xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx       $$$  // moved page
.... bubble 300 / move 150

3)  move up cnt=2300 from 3450-5750 to 3750-6050  dist 300
                 |                    |                    |                    |                    |
      xxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
      xxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxx       $  // moved 50 // TargetEndSubIndex = 0
      xxxxxxxxxxx       $$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$$$ $  // bubbled 300
      xxxxx       $$$$$$$$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$$$ $  // moved 300
... move 250


  *** Reverse Bubble

  move up cnt=2300 from 3850-6150 to 4750-7050  dist 900
         |                    |                    |                    |                    |
      xxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx xxx
      xxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx xx                   $  // moved 50 into high target node
instead of bubble 900 up
      xxx xx                ~~ xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx $  // swapped high source down
      xxx xx                $$ $$$$$$$$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$   $  // bubbled 100 down
      xxx                   $$ $$$$$$$$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$~~ $  // moved 100 back to target
----                        ^^ belong to start of node
                         $$$$$ $$$$$$$$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$$$ $  // moved 150

      *)

  (* The "gap" in the target-end page may be bigger/smaller than BubbleCnt.
   * (BubbleCnt is "Diff and FPageSizeMask")
   * Move appropriate amounts of entries into the target-end page.
   * If BubbleCnt=0, then this will leave no "gap" in the target-end page.
   *)
  c := Min(ACount, SourceEndSubIndex);
  DstPage := PagePointer[TargetEndPage];
  if TargetEndPage = SourceEndPage then begin
    // Example: 1
    SourceEndSubIndex := SourceEndSubIndex - c;
    TargetEndSubIndex := TargetEndSubIndex - c;  // TargetEndSubIndex is greater than c
    DstPage^.MoveRows(SourceEndSubIndex, TargetEndSubIndex, c);
  end
  else begin
    // Example: 3
    c := Min(c, TargetEndSubIndex);
    SrcPage := PagePointer[SourceEndPage];
    SourceEndSubIndex := SourceEndSubIndex - c;
    TargetEndSubIndex := TargetEndSubIndex - c;
    SrcPage^.MoveRowsToOther(SourceEndSubIndex, TargetEndSubIndex, c, PageCapacity, DstPage^);
  end;

  ACount := ACount - c;
  if ACount = 0 then
    exit;

  if SourceEndSubIndex = 0 then begin
    SourceEndSubIndex := PageCapacity;
    Dec(SourceEndPage);
  end;
  if TargetEndSubIndex = 0 then begin
    TargetEndSubIndex := PageCapacity;
    Dec(TargetEndPage);
  end;
  assert((TargetEndSubIndex = PageCapacity) or (TargetEndPage=0) or (TargetEndSubIndex = Diff and FPageSizeMask), 'TLazPagedListObjBase.MoveRows: TargetEndSubIndex at end-of-cell OR at BubbleCnt');
  assert((TargetEndSubIndex = PageCapacity) or (TargetEndPage=0) or (SourceEndSubIndex = PageCapacity) or (SourceEndPage=0), 'TLazPagedListObjBase.MoveRows: (TargetEndSubIndex = PageCapacity) or (SourceEndSubIndex = PageCapacity)');



  // Full Pages AND/OR bubble
  if (TargetStartPage < TargetEndPage) then begin
    (*
     * Move full pages, if needed
     *)
    p1 := TargetStartPage;
    if TargetStartSubIndex > 0 then inc(p1);
    p2 := TargetEndPage;
    if TargetEndSubIndex < PageCapacity then dec(p2);  // TODO: TargetEndSubIndex < TargetPage.Count ????
    MovePgCnt := p2 - p1 + 1;
    if (SourceEndPage < p2) and (MovePgCnt > 0) then begin
      SwapPagesUp(SourceEndPage - MovePgCnt + 1, p1, p2);
    end;


    (*
     * Bubble
     * or adjust Source/Target... for the move // (p1 >= TargetEndPage)
     *)
    BblCnt := Diff and FPageSizeMask;
    if (BblCnt > 0) and ((p1 < TargetEndPage) or (TargetEndSubIndex = PageCapacity)) then begin
      assert((p2 = TargetEndPage) or (p2=0) or (TargetEndSubIndex = BblCnt), 'TLazPagedListObjBase.MoveRows: (p2 < TargetEndPage) or (TargetEndSubIndex = BblCnt)');
      if TargetEndSubIndex = PageCapacity then begin
        // Example: 3  // may or may not bubble // maybe only adjust
        PagePointer[TargetEndPage]^.AdjustFirstItemOffset(-BblCnt, FPageSizeMask);
        dec(MovePgCnt); // only affects how "ACount" is adjusted
        SourceEndSubIndex := SourceEndSubIndex - (PageCapacity - BblCnt);
        if SourceEndSubIndex <= 0 then begin
          SourceEndSubIndex := SourceEndSubIndex + PageCapacity;
          dec(SourceEndPage);
        end;
        ACount := ACount - (PageCapacity - BblCnt);
        TargetEndSubIndex := BblCnt;
      end;

      if p1 < TargetEndPage then begin
        BubbleEntriesUp(p1, TargetEndPage, BblCnt);
        PagePointer[p1]^.AdjustFirstItemOffset(-BblCnt, FPageSizeMask);
      end;
    end;


    if MovePgCnt > 0 then begin
      ACount := ACount - (MovePgCnt << FPageSizeExp);
      assert(ACount>=0, 'TLazPagedListObjBase.InternalMoveRowsUp: ACount>=0');
      if ACount = 0 then
        exit;
      TargetEndPage := TargetEndPage - MovePgCnt;
      SourceEndPage := SourceEndPage - MovePgCnt;
    end;
  end;
  assert((TargetEndPage>=0), 'TLazPagedListObjBase.MoveRows: (TargetEndPage>=0)');
  assert(TargetEndSubIndex > 0, 'TLazPagedListObjBase.MoveRows: TargetEndSubIndex > 0');
  assert((TargetEndPage-TargetStartPage<=1), 'TLazPagedListObjBase.InternalMoveRowsDown / Bubbled all but the last page: (TargetEndPage-TargetStartPage<=1)');



  // move
  while ACount > 0 do begin
    c := min(min(TargetEndSubIndex, SourceEndSubIndex), ACount);
    assert(c>0, 'TLazPagedListObjBase.MoveRows: c>0');
    TargetEndSubIndex := TargetEndSubIndex - c;
    SourceEndSubIndex := SourceEndSubIndex - c;
    SrcPage := PagePointer[SourceEndPage];
    if SourceEndPage = TargetEndPage then
      SrcPage^.MoveRows(SourceEndSubIndex, TargetEndSubIndex, c)
    else
      SrcPage^.MoveRowsToOther(SourceEndSubIndex, TargetEndSubIndex, c, PageCapacity, PagePointer[TargetEndPage]^);

    ACount := ACount - c;
    if ACount = 0 then
      exit;
    if SourceEndSubIndex = 0 then begin
      SourceEndSubIndex := PageCapacity;
      Dec(SourceEndPage);
    end;
    if TargetEndSubIndex = 0 then begin
      TargetEndSubIndex := PageCapacity;
      Dec(TargetEndPage);
    end;
  end;

end;

procedure TLazPagedListObjBase.MoveRows(AFromIndex, AToIndex, ACount: Integer);
begin
  assert((AFromIndex>=0) and (AToIndex>=0), 'TLazPagedListObjBase.MoveRows: (AFromIndex>=0) and (AToIndex>=0)');

  if AFromIndex < AToIndex then
    InternalMoveRowsUp(AFromIndex, AToIndex, ACount)
  else
    InternalMoveRowsDown(AFromIndex, AToIndex, ACount);
end;

procedure TLazPagedListObjBase.DebugDump;
var i : integer;
begin
  if fpages.fmem.IsAllocated then begin
    debugln(['PAGED .Dump  Pages.Capacity: ', fpages.fmem.Capacity, ', P.Count: ',fpages.fmem.Count,' -- Count:',FCount,'   FirstPageEmpty: ', FFirstPageEmpty, ': ']);
    for i := 0 to fpages.Count - 1 do FPages.ItemPointer[i]^.DebugDump;
  end
  else debugln(['PAGED .Dump NONE']);
end;

{ TLazPagedListObj }

procedure TLazPagedListObj.Create(APageSizeExp: Integer; AnItemSize: Integer);
begin
  FItemSize.ItemSize := AnItemSize;
  inherited Create(APageSizeExp);
end;

{ TLazShiftBufferList }

function TLazShiftBufferList.GetCapacity: Integer;
begin
  Result := FListMem.Capacity;
end;

function TLazShiftBufferList.GetCount: Integer;
begin
  Result := FListMem.Count;
end;

function TLazShiftBufferList.GetItemPointer(Index: Integer): Pointer;
begin
  Result := FListMem.ItemPointer[Index];
end;

procedure TLazShiftBufferList.SetCapacity(AValue: Integer);
begin
  FListMem.Capacity := AValue;
end;

procedure TLazShiftBufferList.SetCount(AValue: Integer);
begin
  if AValue > FListMem.Count then
    FListMem.InsertRows(FListMem.Count, AValue - FListMem.Count)
  else
  if AValue < FListMem.Count then
    FListMem.DeleteRows(AValue, FListMem.Count - AValue);
end;

constructor TLazShiftBufferList.Create(AnItemSize: Integer);
begin
  FListMem.Create(AnItemSize);
end;

destructor TLazShiftBufferList.Destroy;
begin
  FListMem.Destroy;
end;

function TLazShiftBufferList.Add(ItemPointer: Pointer): Integer;
begin
  Result := FListMem.Count;
  FListMem.InsertRows(Result, 1);
  Move(ItemPointer^, FListMem.ItemPointer[Result]^, FListMem.FItemSize.ItemSize);
end;

procedure TLazShiftBufferList.Clear;
begin
  FListMem.Capacity := 0;
end;

procedure TLazShiftBufferList.Delete(Index: Integer);
begin
  FListMem.DeleteRows(Index, 1);
end;

procedure TLazShiftBufferList.Insert(Index: Integer; ItemPointer: Pointer);
begin
  FListMem.InsertRows(Index, 1);
  Move(ItemPointer^, FListMem.ItemPointer[Index]^, FListMem.FItemSize.ItemSize);
end;

{ TLazShiftBufferListGen }

function TLazShiftBufferListGen.GetCapacity: Integer;
begin
  Result := FListMem.Capacity;
end;

function TLazShiftBufferListGen.Get(Index: Integer): T;
begin
  Result := FListMem.ItemPointer[Index]^;
end;

function TLazShiftBufferListGen.GetCount: Integer;
begin
  Result := FListMem.Count;
end;

function TLazShiftBufferListGen.GetItemPointer(Index: Integer): PT;
begin
  Result := FListMem.ItemPointer[Index];
end;

procedure TLazShiftBufferListGen.Put(Index: Integer; AValue: T);
begin
  FListMem.ItemPointer[Index]^ := AValue;
end;

procedure TLazShiftBufferListGen.SetCapacity(AValue: Integer);
begin
  FListMem.Capacity := AValue;
end;

procedure TLazShiftBufferListGen.SetCount(AValue: Integer);
begin
  if AValue > FListMem.Count then
    FListMem.InsertRows(FListMem.Count, AValue - FListMem.Count)
  else
  if AValue < FListMem.Count then
    FListMem.DeleteRows(AValue, FListMem.Count - AValue);
end;

constructor TLazShiftBufferListGen.Create;
begin
  FListMem.Create;
end;

destructor TLazShiftBufferListGen.Destroy;
begin
  FListMem.Destroy;
end;

function TLazShiftBufferListGen.Add(Item: T): Integer;
begin
  Result := FListMem.Count;
  FListMem.InsertRows(Result, 1);
  FListMem.ItemPointer[Result]^ := Item;
end;

procedure TLazShiftBufferListGen.Clear;
begin
  FListMem.Capacity := 0;
end;

procedure TLazShiftBufferListGen.Delete(Index: Integer);
begin
  FListMem.DeleteRows(Index, 1);
end;

function TLazShiftBufferListGen.IndexOf(Item: T): Integer;
begin
  Result := FListMem.IndexOf(Item);
end;

procedure TLazShiftBufferListGen.Insert(Index: Integer; Item: T);
begin
  FListMem.InsertRows(Index, 1);
  FListMem.ItemPointer[Index]^ := Item;
end;

function TLazShiftBufferListGen.Remove(Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

end.


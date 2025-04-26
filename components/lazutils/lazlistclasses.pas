{
 *****************************************************************************
  This file is part of LazUtils.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.

 *****************************************************************************
 *****************************************************************************

 Initial Revision  : May 2015

 This unit provides generics for list classes.
 Each list can be specialized to either hold items of a given type (specialized
 to type at compile time) or to hold untyped data the size of with can be
 specified at runtime as argument to the constructor.

 Note about "object" variants:
   "object"s are stored on the stack, as such the memory of the object itself
   is freed when the variable goes out of scope. The objects do however
   allocate additional memory on the heap.
 => So it is necessary to call Destroy.
 => It is also necessary to call Create. (Unless you can gurantee that the
    memory of the object has been zero filled)

 To use the lists with managed types, the correct MemInit must be configured

 ****************************************
 * TGenLazShiftList

   This list is designed for shift/unshift/pop/push operations.

   The first list element is not forced to the start of the allocated memory.
   Instead it allows a gap (some of the over-allocated memory / List.Capacity)
   in front of the first element.

   Therefore elements can be added/removed at either the begin or end of the
   list, withouth any need to move the other elemnts in the list.

 ****************************************
 * TGenLazRoundList

   The first element of the list can be anywhere within the allocated memory
   (capacity). If the elements of the list reach the end of the memory, the list
   will wrap arount and continues in the available memory at the start of the
   allocation.

   This list can be used for a first-in, first-out queue. If the list does never
   exceed the size set by its capacity, then elements can be pushed/shifted
   from/to the list without any need to reallocate or move entries to new
   locations.

 ****************************************
 * TGenLazPagedList

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

 => VarSize
    Lists take an "ItemSize" in the constructor.
    Elements are accessed via ItemPointer (which may be a plain pointer or typed pointer)

 => FixedSize
    Lists are compiled for a fixed itemsize (usually "SizeOf(type)"
 => FixedType
    In addition to FixedSize there is an "Items" property
    and "IndexOf" can take an itemn (instead of a pointer)
    For this, items need to have a compare operator.


   The samples below show the available variants of the above list.
   The constructor for each sample is included.



 ****************************************
 * Helpers for specializing the variants

   Either of the following can be specified to generics that take a "TSizeT"

   type
   generic TLazListAspectItemSiz<T> = object

     Used for specializing lists with a fixed item size.
     The size will be:
       sizeof(T)

   type
   TLazListAspectVarItemSize = object

     Used for specializing list with a configurable item size. The size must
     be set in the constructor and can not be changed after this.
     When using this, you need to add a constructor setting:
       fItemSize.ItemSize := ASize; // "ASize" is your size



}

unit LazListClasses;

{$mode objfpc}{$H+}
{$WARN 3124 off : Inlining disabled}
{$WARN 6018 off : unreachable code}
interface
{$IFDEF LazListClassTestCase}
  {$INLINE off}
{$ELSE}
  {$Optimization REMOVEEMPTYPROCS}
  {$Optimization DFA}
  (* DEADSTORE, DEADVALUES: important when calls to empty procs are removed *)
{$If FPC_FULLVERSION <> 30301}
  {$Optimization DEADSTORE} // needs DFA
{$EndIf}
  {$Optimization DEADVALUES}
  { $Optimization CONSTPROP}
  { $Optimization CSE}
{$ENDIF}

uses
  Classes, SysUtils, math,
  LazListClassesBase,
  LazLoggerBase;

type

  TLazStorageMemShrinkProc = function(ARequired: Integer): Integer of object;
  TLazStorageMemGrowProc = function(ARequired: Integer): Integer of object;

  (* TLazListAspectItemSiz
     Helper to specialize lists for a give type
  *)
  generic TLazListAspectItemSiz<T> = object
  protected
    const
    ItemSize = SizeOf(T);
  end;

  (* TLazListAspectVarItemSize
     Helper to specialize lists for runtime specified size "TList.Create(ASize)"
  *)
  TLazListAspectVarItemSize = object
  public
    ItemSize: Cardinal;
  end;

  { TLazListAspectMemInitZero }

  TLazListAspectMemInitZero = object(TLazListAspectMemInitNone)
  public
    class procedure InitMem(const AMem: Pointer; const AnItemCount: integer; const AnItemSize: Cardinal); inline; static;
    class procedure FinalizeMem(const {%H-}AMem: Pointer; const {%H-}AnItemCount: integer; const {%H-}AnItemSize: Cardinal); inline; static;
  end;

  { TLazListAspectMemAllocZero }

  TLazListAspectMemAllocZero = object(TLazListAspectMemInitNone)
  public
    class procedure InitMemOnce(const AMem: Pointer; const AnItemCount: integer; const AnItemSize: Cardinal); inline; static;
  end;

  { TLazListAspectMemInitManagedRefCnt }

  generic TLazListAspectMemInitManagedRefCnt<TItemT> = object(TLazListAspectMemInitNone)
  private type
    PItemT = ^TItemT;
  public
    class procedure InitMem(const AMem: Pointer; const AnItemCount: integer; const AnItemSize: Cardinal); inline; static;
    class procedure FinalizeMem(const AMem: Pointer; const AnItemCount: integer; const AnItemSize: Cardinal); inline; static;
  end;

  { TLazListAspectCapacityExp0x8000 }

  TLazListAspectCapacityExp0x8000 = object(TLazListAspectCapacitySimple)
  public
    class function GrowCapacity(const ARequired, {%H-}ACurrent: Integer): Integer; inline; static;
    class function ShrinkCapacity(const ARequired, ACurrent: Integer): Integer; inline; static;
  end;

  { TGenListAspectCapacityCallback }

  generic TGenListAspectCapacityCallback<TFALLBACK> = object(TLazListAspectCapacitySimple)
  public
    FGrowProc: TLazStorageMemGrowProc;
    FShrinkProc: TLazStorageMemShrinkProc;
    FFallBack: TFALLBACK;
    procedure Init(); inline;
    function GrowCapacity(const ARequired, ACurrent: Integer): Integer; inline;
    function ShrinkCapacity(const ARequired, ACurrent: Integer): Integer; inline;
  end;

  TLazListAspectCapacityCallback = specialize TGenListAspectCapacityCallback<TLazListAspectCapacitySimple>;

  { TLazListAspectCapacityFixed }

  TLazListAspectCapacityFixed = object(TLazListAspectCapacitySimple)
    class function GrowCapacity(const ARequired, {%H-}ACurrent: Integer): Integer; inline; static;
    class function ShrinkCapacity(const ARequired, ACurrent: Integer): Integer; inline; static;
  end;

  TLazListAspectCapacityFieldConst = class(TLazListAspectCapacityFieldMem)
  public type
    TCapacityOrdT = record end;

    TCAWrap = class
      public type
      generic TCapacityAccessor<TMemHeaderT; TData> = object
      public
        class function ReadCapacity(const AMem: Pointer): Cardinal; inline; static;
        class function ReadCapacitySafe(const AMem: Pointer): Cardinal; inline; static;
        class procedure WriteCapacity(const AMem: Pointer; AValue: Cardinal); inline; static;
      end;
    end;
  end;
  //TLazListAspectCapacityFieldConst_999 = class(TLazListAspectCapacityFieldConst)
  //public const
  //  CapValue = 999;
  //end;

  {$if FPC_FULLVERSION > 30300}
  generic TGenListAspectCapacityFieldConst<const _CapVal: Cardinal> = class(TLazListAspectCapacityFieldConst)
  public const
    CapValue = _CapVal;
  end;
  {$endif}

  { TLazListAspectRangeIndexCheckReturnNil }

  TLazListAspectRangeIndexCheckReturnNil = object
  public
    generic class function CheckIndex<T>(const {%H}AList: T; const {%H-}AnIndex: Integer; var {%H-}Res: Pointer): Boolean; inline; static;
    generic class function CheckInsert<T>(const {%H}AList: T; var {%H-}AnIndex: Integer; var {%H-}Res: Pointer): Boolean; inline; static;
    generic class function CheckDelete<T>(const {%H}AList: T; var {%H-}AnIndex, {%H-}ACount: Integer): Boolean; inline; static;
    generic class function CheckMove<T>(const {%H}AList: T; var {%H-}AFromIndex, {%H-}AToIndex, {%H-}ACount: Integer): Boolean; inline; static;
    generic class function CheckSwap<T>(const {%H}AList: T; var {%H-}AnIndex1, {%H-}AnIndex2: Integer): Boolean; inline; static;
  end;

  { TLazListAspectRangeIndexCheckExcept }

  TLazListClassesIndexError = class(Exception) end;

  TLazListAspectRangeIndexCheckExcept = object
  public
    generic class function CheckIndex<T>(const {%H}AList: T; const {%H-}AnIndex: Integer; var {%H-}Res: Pointer): Boolean; inline; static;
    generic class function CheckInsert<T>(const {%H}AList: T; var {%H-}AnIndex: Integer; var {%H-}Res: Pointer): Boolean; inline; static;
    generic class function CheckDelete<T>(const {%H}AList: T; var {%H-}AnIndex, {%H-}ACount: Integer): Boolean; inline; static;
    generic class function CheckMove<T>(const {%H}AList: T; var {%H-}AFromIndex, {%H-}AToIndex, {%H-}ACount: Integer): Boolean; inline; static;
    generic class function CheckSwap<T>(const {%H}AList: T; var {%H-}AnIndex1, {%H-}AnIndex2: Integer): Boolean; inline; static;
  end;

  (* *** ******************** *
     ***
     *** Config classes
     ***
   * *** ******************** *)

  generic TGenListConfig_4<_TSizeT, _TInitMemT, _TCapacityT, _TIdxCheckT> = class(TLazListConfig)
  public type
    TSizeT     = _TSizeT;
    TInitMemT  = _TInitMemT;
    TCapacityT = _TCapacityT;
    TIdxCheckT = _TIdxCheckT;
  end;

  (* *** Fixed size // only for generics expecting & wrapping this into _TC *)

  TLazListConfigFixSize = class abstract (TLazListConfig)
  public type
    generic _TC<_T; _B: TLazListConfig> = class(_B)
    public type
      TSizeT = specialize TLazListAspectItemSiz<_T>;
    end;
  end;


  generic TGenListConfigFixSize_3<_TInitMemT, _TCapacityT, _TIdxCheckT> = class(TLazListConfigFixSize)
  public type
    TInitMemT  = _TInitMemT;
    TCapacityT = _TCapacityT;
    TIdxCheckT = _TIdxCheckT;
  end;

  (* *** Var size *)

  TLazListConfigVarSize = class(TLazListConfig)
  public type
    TSizeT = TLazListAspectVarItemSize;
  end;

  generic TGenListConfigVarSize_3<_TInitMemT, _TCapacityT, _TIdxCheckT> = class(TLazListConfigVarSize)
  public type
    TInitMemT  = _TInitMemT;
    TCapacityT = _TCapacityT;
    TIdxCheckT = _TIdxCheckT;
  end;

  (* *** Presets *)

  (* TLazListAspectCapacityExp0x8000 *)

  generic TGenListConfigExp_3<_TSizeT, _TInitMemT, _TIdxCheckT> =
    class( specialize TGenListConfig_4<_TSizeT, _TInitMemT,
      TLazListAspectCapacityExp0x8000,
      _TIdxCheckT>)
    end;
  generic TGenListConfigVarSizeExp_2<_TInitMemT, _TIdxCheckT> =
    class(specialize TGenListConfigVarSize_3<_TInitMemT,
      TLazListAspectCapacityExp0x8000, _TIdxCheckT>)
    end;
  generic TGenListConfigFixSizeExp_2<_TInitMemT, _TIdxCheckT> =
    class(specialize TGenListConfigFixSize_3<_TInitMemT,
      TLazListAspectCapacityExp0x8000, _TIdxCheckT>)
    end;

  (* TLazListAspectMemInitNone, TLazListAspectRangeNoIndexCheck *)
  generic TGenListConfig_1<_TSizeT> =
    class(specialize TGenListConfigExp_3<_TSizeT,
      TLazListAspectMemInitNone, TLazListAspectRangeNoIndexCheck>)
    end;
  TLazListConfigFixSize_0 =
    class(specialize TGenListConfigFixSizeExp_2<TLazListAspectMemInitNone, TLazListAspectRangeNoIndexCheck>)
    end;
  TLazListConfigVarSize_0 =
    class(specialize TGenListConfigVarSizeExp_2< TLazListAspectMemInitNone, TLazListAspectRangeNoIndexCheck>)
    end;

  (* TLazListAspectRangeNoIndexCheck *)

  generic TGenListConfigVarSizeNoIdx_2<_TInitMemT, _TCapacityT> =
    class(specialize TGenListConfigVarSize_3<_TInitMemT, _TCapacityT, TLazListAspectRangeNoIndexCheck>)
    end;
  generic TGenListConfigFixSizeNoIdx_2<_TInitMemT, _TCapacityT> =
    class(specialize TGenListConfigFixSize_3<_TInitMemT, _TCapacityT, TLazListAspectRangeNoIndexCheck>)
    end;

  (* ADD TCapacityFieldT *)

  generic TGenListClasses_CapacityFld<TConfT: TLazListConfig; _TCapacityFieldT: TLazListAspectCapacityFieldMem> = class(TConfT)
  public type
    TCapacityFieldT = _TCapacityFieldT;
  end;


  { TTypeToPointerGeneric
    Inline pointer type for specializing base classes
  }
  generic TTypeToPointerGeneric<T> = class public type PT = ^T; end;

  (*                              *
   * TLazShiftList variants *
   *                              *)

  { TGenLazShiftList }

  generic TGenLazShiftList<TPItemT; _TConfT: TLazListConfig> = object(
    specialize __TGenLazListClassesInternalBase<_TConfT>
  )
  private
    FMem: TLazListClassesInternalMem;

    function GetItemPointer(const Index: Integer): TPItemT; inline;
    function GetItemPointerFast(const Index: PtrUInt): TPItemT; inline;
    function GetFirstItemPointerFast: TPItemT; inline;
    function GetItemSize: Cardinal; inline;
    procedure SetCapacity(const AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
  protected
    // For access by subclasses, if it contains fields
    FItemSize: TSizeT; // May be zero size
    FCapacity: TCapacityT;
    FCapacityAccessor: TCapacityAccessorT;
    FInitMem:  TInitMemT;
    FIndexChecker: TIdxCheckT;
  protected
    function SetCapacityEx(const AValue, AnInsertPos: Cardinal; const AnInsertSize: Integer): TPItemT;
    property ItemPointerFast[Index: PtrUInt]: TPItemT read GetItemPointerFast;
  public
    procedure Create;
    procedure Destroy;
    function InsertRows(AnIndex, ACount: Integer): TPItemT;
    procedure DeleteRows(AnIndex, ACount: Integer);
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer);
    procedure SwapEntries(AnIndex1, AnIndex2: Integer);
    procedure DebugDump;

    function IndexOf(const AnItem: TPItemT): integer; // compare data pointed to
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemSize: Cardinal read GetItemSize;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
  end;

  { TGenLazShiftListVarSize }

  generic TGenLazShiftListVarSize<TPItemT; _TConfT: TLazListConfigVarSize> = object(
    specialize TGenLazShiftList<TPItemT, _TConfT>
  )
  public
    procedure Create(const AnItemSize: Cardinal);
  end;

  generic TLazShiftBufferListObjBase<TPItemT, _TSizeT> = object(
    specialize TGenLazShiftList<TPItemT, specialize TGenListConfig_1<_TSizeT> >
  )
  end deprecated;

  TLazShiftBufferListObj = object(
    specialize TGenLazShiftListVarSize<Pointer, TLazListConfigVarSize_0>
  )
  end;


  { TGenLazShiftListFixedSize }

  generic TGenLazShiftListFixedSize<T; _TConfT: TLazListConfigFixSize> = object(
    specialize TGenLazShiftList<
      specialize TTypeToPointerGeneric<T>.PT, _TConfT.specialize _TC<T, _TConfT>
    >
  )
  end;

  generic TGenLazShiftListFixedType<T; _TConfT: TLazListConfigFixSize> = object(specialize TGenLazShiftListFixedSize<T, _TConfT>)
  private type
    PT = ^T;
  private
    function Get(const Index: Integer): T; inline;
    procedure Put(const Index: Integer; AValue: T); inline;
  public
    function IndexOf(AnItem: T): integer; overload;
    property Items[Index: Integer]: T read Get write Put; default;
  end;


  { TLazShiftBufferListObjGen }

  generic TLazShiftBufferListObjGen<T> = object(specialize TGenLazShiftListFixedType<T, TLazListConfigFixSize_0>)
  end;

  (*                              *
   * TLazRoundList variants *
   *                              *)

  { TGenLazRoundList }

  generic TGenLazRoundList<TPItemT; _TConfT: TLazListConfig> = object(
    specialize __TGenLazListClassesInternalBase<_TConfT>
  )
  private
    // Keep the size small, if no entries exist
    // FMem:  FLowElemPointer: PByte; FCount, FCapacity_in_bytes: Integer; Array of <FItemSize
    FMem: TLazListClassesInternalMem;

    function GetItemPointer(const Index: Integer): TPItemT; inline;
    function GetItemPointerFast(const Index: Cardinal): TPItemT; inline; overload;
    function GetItemPointerFast(const Index, ACapacity: Cardinal): TPItemT; inline; overload;
    function GetItemPointerFast(const Index, ACapacity: Cardinal; const AnMPtr: Pointer): TPItemT; inline; overload;
    function GetItemSize: Cardinal; inline;
    procedure SetCapacity(const AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;

    function IndexOf(const AnItem: TPItemT; const AFirstIdx: integer): integer; overload;
  protected
    // For access by subclasses, if it contains fields
    FItemSize: TSizeT; // May be zero size
    FCapacity: TCapacityT;
    FCapacityAccessor: TCapacityAccessorT;
    FInitMem:  TInitMemT;
    FIndexChecker: TIdxCheckT;

    procedure InternalMoveUp(const AFromEnd, AToEnd: PByte; AByteCnt: Cardinal; const AByteCap: Integer);
    procedure InternalMoveDown(const AFrom, ATo: PByte; AByteCnt: Cardinal; const AUpperBound: PByte);
    procedure MoveRowsUp(const AFromIndex, AToIndex, ACount: Cardinal);
    procedure MoveRowsDown(const AFromIndex, AToIndex, ACount: Cardinal);

    function SetCapacityEx(const AValue, AnInsertPos: Cardinal; const AnInsertSize: Integer): TPItemT;
    property ItemPointerFast[Index: Cardinal]: TPItemT read GetItemPointerFast;
  public
    procedure Create;
    procedure Destroy;
    function  InsertRows(AnIndex, ACount: Integer): TPItemT;
    procedure DeleteRows(AnIndex, ACount: Integer);
    procedure MoveRows( AFromIndex, AToIndex, ACount: Integer);
    procedure SwapEntries(AnIndex1, AnIndex2: Integer);
    procedure DebugDump;
    function IndexOf(AnItem: TPItemT): integer;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemSize: Cardinal read GetItemSize;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
  end;

  { TGenLazRoundListVarSize }

  generic TGenLazRoundListVarSize<TPItemT; _TConfT: TLazListConfigVarSize> = object(
    specialize TGenLazRoundList<TPItemT, _TConfT>
  )
  public
    procedure Create(const AnItemSize: Cardinal);
  end;

  generic TLazRoundBufferListObjBase<TPItemT, _TSizeT> = object(
    specialize TGenLazRoundList<TPItemT, specialize TGenListConfig_1<_TSizeT> >
  )
  end deprecated;

  TLazRoundBufferListObj = object(specialize TGenLazRoundListVarSize<Pointer, TLazListConfigVarSize_0>)
  end;


  { TGenLazRoundListFixedSize }

  generic TGenLazRoundListFixedSize<T; _TConfT: TLazListConfigFixSize> =
    object(specialize TGenLazRoundList<specialize TTypeToPointerGeneric<T>.PT,
      _TConfT.specialize _TC<T, _TConfT>
    >
  )
  end;

  generic TGenLazRoundListFixedType<T; _TConfT: TLazListConfigFixSize> = object(specialize TGenLazRoundListFixedSize<T, _TConfT>)
  private type
    PT = ^T;
  private
    function Get(const Index: Integer): T; inline;
    procedure Put(const Index: Integer; AValue: T); inline;
  public
    function IndexOf(AnItem: T): integer; overload;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

  generic TLazRoundBufferListObjGen<T> = object(specialize TGenLazRoundListFixedType<T, TLazListConfigFixSize_0>)
  end;

  (*                           *
   * TLazPagedList variants *
   *                           *)

  { __TLazListAspectPageSizeExpConstBase }

  __TLazListAspectPageSizeExpConstBase = object
  public
    procedure Init({%H-}APageSizeExp: Integer);
  end;

  {$if FPC_FULLVERSION > 30300}
  generic TLazListAspectPageSizeExpConst<const T: Cardinal> = object(__TLazListAspectPageSizeExpConstBase)
  public const
    FPageSizeExp = T;
    FPageSizeMask = Cardinal(not(Cardinal(-1) << FPageSizeExp));
  end;
  {$endif}

  TLazListAspectPageSizeExpConst_8 = object(__TLazListAspectPageSizeExpConstBase)
  public const
    FPageSizeExp = 8;
    FPageSizeMask = Cardinal(not(qword(-1) << FPageSizeExp));
  end;

  TLazListAspectPageSizeExpConst_10 = object(__TLazListAspectPageSizeExpConstBase)
  public const
    FPageSizeExp = 10;
    FPageSizeMask = Cardinal(not(qword(-1) << FPageSizeExp));
  end;

  TLazListAspectPageSizeExpConst_12 = object(__TLazListAspectPageSizeExpConstBase)
  public const
    FPageSizeExp = 12;
    FPageSizeMask = Cardinal(not(qword(-1) << FPageSizeExp));
  end;


  generic TGenListPageConfig<_TPageSizeT; _TPageCapacityFieldT: TLazListAspectCapacityFieldMem> = class(TLazListPageConfig)
  public type
    TPageSizeT = _TPageSizeT;
    TPageCapacityFieldT = _TPageCapacityFieldT;
  end;


  __Internal_LazListClassesPgConfConstCapFld = class
  public type
    generic TCapacityAccessor<TMemHeaderT; TData> = object
    public
      class function ReadCapacity(const AMem: Pointer): Cardinal; inline; static;
      class function ReadCapacitySafe(const AMem: Pointer): Cardinal; inline; static;
      class procedure WriteCapacity(const AMem: Pointer; AValue: Cardinal); inline; static;
    end;
  end;

  generic TGenListPageConfigConst<_TPageSizeT> = class(TLazListPageConfig)
  public type
    TPageSizeT = _TPageSizeT;

    TPageCapacityFieldT = class(TLazListAspectCapacityFieldMem)
    public type
      TCapacityOrdT = record end;
      TPageSizeT = _TPageSizeT;
      TCAWrap = __Internal_LazListClassesPgConfConstCapFld;
    end;
  end;

  { TLazFixedRoundBufferListMemBase
    For use as page in PageList
  }

  generic TLazFixedRoundBufferListMemBase<TPItemT, _TSizeT; _TCapacityFieldT: TLazListAspectCapacityFieldMem> = object(
    specialize TGenLazRoundList<
      TPItemT,
      specialize TGenListClasses_CapacityFld<specialize TGenListConfig_1<_TSizeT>, _TCapacityFieldT>
    >
  )
  private type
    PLazFixedRoundBufferListMemBase = ^TLazFixedRoundBufferListMemBase;
  private
    function GetItemPointerMasked(const AnIndex, AMask: PtrUInt): TPItemT; inline; // AMask: Bitmask for Capacity
    function GetItemByteOffsetMasked(const AnIndex, AMask: PtrUInt): Integer; inline; // AMask: Bitmask for Capacity
    function GetFirstItemByteOffset: Integer; inline; // AMask: Bitmask for Capacity
  protected
    property Mem: TLazListClassesInternalMem read FMem;

    // Special Methods for use with PagedList
    procedure AdjustFirstItemOffset(const ACount: Integer; const AMask: Cardinal); inline; // For bubbling / shift the buffer
    procedure InsertRowsAtStart(const ACount: Integer; const AMask: Cardinal); inline;
    procedure InsertRowsAtEnd(const ACount: Integer); inline;
    procedure InsertRowsAtBoundary(const AnAtStart: Boolean; const ACount: Integer; const AMask: Cardinal);
    procedure DeleteRowsAtStart(const ACount: Integer; const AMask: Cardinal); inline;
    procedure DeleteRowsAtEnd(const ACount: Integer); inline;
    procedure DeleteRowsAtBoundary(const AnAtStart: Boolean; const ACount: Integer; const AMask: Cardinal); inline;
    procedure MoveRowsToOther(const AFromOffset, AToOffset, ACount, ACap: Integer; AnOther: PLazFixedRoundBufferListMemBase); inline;
    procedure MoveBytesToOther(AFromByteOffset, AToByteOffset, AByteCount, AByteCap: Integer; AnOther: PLazFixedRoundBufferListMemBase);
    property ItemPointerMasked[AnIndex, AMask: PtrUInt]: TPItemT read GetItemPointerMasked;
  public
    procedure Create(const AItemSize: _TSizeT; ACapacity: Integer);
  end;

  { TGenLazPagedList }

  generic TGenLazPagedList<TPItemT; _TConfT: TLazListConfig; _TPageConfT: TLazListPageConfig> = object(
    specialize __TGenLazPagedListInternalBase<_TConfT, _TPageConfT>
  )
  private type
    TOuter_TInitMemT = TInitMemT;
    TOuter_TSizeT = TSizeT;
    TOuter_TCapacityT = TCapacityT;
    TOuter_TPageCapacityFieldT = TPageCapacityFieldT;

    { TPageType }

    TPageType = object(specialize TLazFixedRoundBufferListMemBase<TPItemT, TOuter_TSizeT, TOuter_TPageCapacityFieldT>)
    protected
      procedure InitMem(const AnInitMem: TOuter_TInitMemT; const AnItemSize, ACap: Cardinal); inline;
      procedure InitMem(const AnInitMem: TOuter_TInitMemT; const AnIndex, AnItemCount: integer; const AnItemSize, ACap: Cardinal); inline;
      procedure FinalizeMem(const AnInitMem: TOuter_TInitMemT; const AnItemSize, ACap: Cardinal); inline;
      procedure FinalizeMem(const AnInitMem: TOuter_TInitMemT; const AnIndex, AnItemCount: integer; const AnItemSize, ACap: Cardinal); inline;
    end;
    PPageType = ^TPageType;
//    TPageSize = specialize TLazListAspectItemSiz<TPageType>;

    { TPageCapacityController }

    TPageCapacityController = object(
      specialize TGenListAspectCapacityCallback<TOuter_TCapacityT>
    )
    protected
      FExtraCapacityNeeded: Integer;
      procedure Init(); inline;
      function GrowCapacity(const ARequired, ACurrent: Integer): Integer; inline;
    end;
    TPageListType = specialize TGenLazShiftListFixedSize<TPageType,
      specialize TGenListConfigFixSize_3<TLazListAspectMemInitNone, TPageCapacityController, TLazListAspectRangeNoIndexCheck> >;
  strict private
    procedure _InsertRows(const AnIndex, ACount, APageSizeMask, APageSizeExp: Integer); inline;
  private
    FPages: TPageListType;
    FFirstPageEmpty: Cardinal;
    FCount: Integer;

    function GetItemSize: Cardinal; inline;
    function GetPagePointer(const PageIndex: Integer): PPageType; inline;
    function GetItemPointer(const Index: Integer): TPItemT; inline;
    procedure SetCapacity(const AValue: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetPageCount: Integer; inline;
    procedure JoinPageWithNext(const APageIdx, AJoinEntryIdx, AnExtraDelPages: Integer); inline;
    procedure SplitPageToFront(const ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer; const AExtraCapacityNeeded: Integer = 0); inline;
    procedure SplitPageToBack(const ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer; const AExtraCapacityNeeded: Integer = 0); inline;
    procedure SplitPage(const ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer; const AExtraCapacityNeeded: Integer = 0);
    procedure _DoInitMem(const AnIndex, ACount: Integer; _ItemSize, _PageSize: Cardinal); inline;
    procedure DoInitMem(const AnIndex, ACount: Integer);
    procedure DoFinalizeMem(const AnIndex, ACount: Integer);
    procedure InternalBubbleEntriesDown(const ASourceStartIdx, ATargetEndIdx, AnEntryCount: Integer); inline;
    procedure InternalBubbleEntriesUp(const ASourceStartIdx, ATargetEndIdx, AnEntryCount: Integer); inline;
    procedure MovePagesUp(const ASourceStartIndex, ATargetStartIndex, ATargetEndIndex: Integer); inline;
    procedure MovePagesDown(const ASourceStartIndex, ATargetStartIndex, ATargetEndIndex: Integer); inline;
    procedure InternalMoveRowsDown(AFromIndex, AToIndex, ACount: Integer);
    procedure InternalMoveRowsUp(AFromIndex, AToIndex, ACount: Integer);
    procedure InsertFilledPages(const AnIndex, ACount: Integer; const AExtraCapacityNeeded: Integer = 0); inline;
    procedure DeletePages(const AnIndex, ACount: Integer); inline;
    procedure MoveRowsQuick(const AFromIndex, AToIndex, ACount: Integer); inline;
  protected
    FItemSize: TSizeT;  // TODO: sync with pages
    FPageSize: TPageSizeT;
    FInitMem:  TInitMemT;
    FIndexChecker: TIdxCheckT;
    property FCapacity: TCapacityT read FPages.FCapacity.FFallBack;

    property PagePointer[PageIndex: Integer]: PPageType read GetPagePointer;
  public
    procedure Create;
    procedure Create(const APageSizeExp: Integer);
    procedure Destroy;
    procedure InsertRows(AnIndex, ACount: Integer);
    procedure DeleteRows(AnIndex, ACount: Integer);
    procedure MoveRows(AFromIndex, AToIndex, ACount: Integer);
    procedure DebugDump;
    function IndexOf(const AnItem: TPItemT): integer;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
    property PageCount: Integer read GetPageCount;
    property ItemPointer[Index: Integer]: TPItemT read GetItemPointer;
    property GrowProc: TLazStorageMemGrowProc     read FPages.FCapacity.FGrowProc   write FPages.FCapacity.FGrowProc;
    property ShrinkProc: TLazStorageMemShrinkProc read FPages.FCapacity.FShrinkProc write FPages.FCapacity.FShrinkProc;
    property ItemSize: Cardinal read GetItemSize;
  end;

  { TGenLazPagedListVarSize }

  generic TGenLazPagedListVarSize<TPItemT; _TConfT: TLazListConfigVarSize; _TPageConfT: TLazListPageConfig> = object(
    specialize TGenLazPagedList<TPItemT, _TConfT, _TPageConfT>
  )
  public
    procedure Create(const AnItemSize: Cardinal);
    procedure Create(const APageSizeExp: Integer; const AnItemSize: Cardinal);
  end;

  generic TLazPagedListObjBase<TPItemT, _TSizeT> = object(
    specialize TGenLazPagedList<TPItemT, specialize TGenListConfig_1<_TSizeT>, TLazListPageConfig>
  )
  end deprecated;

  TLazPagedListObj = object(specialize TGenLazPagedListVarSize<Pointer, TLazListConfigVarSize_0, TLazListPageConfig>)
  end;

  { TGenLazPagedListFixedSize }

  generic TGenLazPagedListFixedSize<T; _TConfT: TLazListConfigFixSize; _TPageConfT: TLazListPageConfig> =
    object(specialize TGenLazPagedList<specialize TTypeToPointerGeneric<T>.PT, _TConfT.specialize _TC<T, _TConfT>, _TPageConfT>
  )
  end;

  generic TGenLazPagedListFixedType<T; _TConfT: TLazListConfigFixSize; _TPageConfT: TLazListPageConfig> = object(specialize TGenLazPagedListFixedSize<T, _TConfT, _TPageConfT>)
  private type
    PT = ^T;
  private
    function Get(const Index: Integer): T; inline;
    procedure Put(const Index: Integer; AValue: T); inline;
  public
    function IndexOf(const AnItem: T): integer; overload;
    property Items[Index: Integer]: T read Get write Put; default;
  end;


  { TLazShiftList }

  TLazShiftList = class
  private
    FListMem: TLazShiftBufferListObj;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItemPointer(const Index: Integer): Pointer;
    procedure SetCapacity(const AValue: Integer);
    procedure SetCount(const AValue: Integer);
  public
    constructor Create(const AnItemSize: Cardinal);
    destructor Destroy; override;

    function Add(const ItemPointer: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer);
    //function IndexOf(ItemPointer: Pointer): Integer;
    procedure Insert(const Index: Integer; ItemPointer: Pointer);
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
    function Get(const Index: Integer): T;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItemPointer(const Index: Integer): PT;
    procedure Put(const Index: Integer; AValue: T);
    procedure SetCapacity(const AValue: Integer);
    procedure SetCount(const AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Item: T): Integer;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer);
    function IndexOf(const Item: T): Integer;
    procedure Insert(const Index: Integer; Item: T);
    function Remove(const Item: T): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property ItemPointer[Index: Integer]: PT read GetItemPointer;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

implementation

{ TLazListAspectMemInitZero }

class procedure TLazListAspectMemInitZero.InitMem(const AMem: Pointer;
  const AnItemCount: integer; const AnItemSize: Cardinal);
begin
  FillChar(AMem^, AnItemCount * AnItemSize, 0);
end;

class procedure TLazListAspectMemInitZero.FinalizeMem(const AMem: Pointer;
  const AnItemCount: integer; const AnItemSize: Cardinal);
begin
  // Nothing
end;

{ TLazListAspectMemAllocZero }

class procedure TLazListAspectMemAllocZero.InitMemOnce(const AMem: Pointer;
  const AnItemCount: integer; const AnItemSize: Cardinal);
begin
  FillChar(AMem^, AnItemCount * AnItemSize, 0);
end;

{ TLazListAspectMemInitManagedRefCnt }

class procedure TLazListAspectMemInitManagedRefCnt.InitMem(const AMem: Pointer;
  const AnItemCount: integer; const AnItemSize: Cardinal);
begin
  FillChar(AMem^, AnItemCount * AnItemSize, 0);
end;

class procedure TLazListAspectMemInitManagedRefCnt.FinalizeMem(const AMem: Pointer;
  const AnItemCount: integer; const AnItemSize: Cardinal);
var
  i: Integer;
  m: Pointer;
begin
  m := AMem;
  for i := 0 to AnItemCount-1 do begin
    PItemT(m)^ := Default(TItemT);
    inc(m, AnItemSize);
  end;
end;

{ TLazListAspectCapacityExp0x8000 }

class function TLazListAspectCapacityExp0x8000.GrowCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  Result := Min(ARequired * 2, ARequired + $8000);
end;

class function TLazListAspectCapacityExp0x8000.ShrinkCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  assert(ARequired <= ACurrent, 'TLazListAspectCapacityExp0x8000.ShrinkCapacity: ARequired <= ACurrent');
  if (ARequired * 4 < ACurrent) then
    Result := ARequired * 2
  else
    Result := -1;
end;

{ TGenListAspectCapacityCallback }

procedure TGenListAspectCapacityCallback.Init();
begin
  FGrowProc := nil;
  FShrinkProc := nil;
  FFallBack.Init;
end;

function TGenListAspectCapacityCallback.GrowCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  if FGrowProc <> nil then
    Result := FGrowProc(ARequired)
  else
    Result := FFallBack.GrowCapacity(ARequired, ACurrent);
end;

function TGenListAspectCapacityCallback.ShrinkCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  if FShrinkProc <> nil then
    Result := FShrinkProc(ARequired)
  else
    Result := FFallBack.ShrinkCapacity(ARequired, ACurrent);
end;

{ TLazListAspectCapacityFixed }

class function TLazListAspectCapacityFixed.GrowCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  Result := ACurrent;
  if ARequired > ACurrent then
    raise Exception.Create('Not allowed');
end;

class function TLazListAspectCapacityFixed.ShrinkCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  Result := -1; // don't shrink
end;

{ TLazListAspectCapacityFieldConst.TCAWrap.TCapacityAccessor }

class function TLazListAspectCapacityFieldConst.TCAWrap.TCapacityAccessor.ReadCapacity(const AMem: Pointer
  ): Cardinal;
begin
  Result := TData.CapValue;
end;

class function TLazListAspectCapacityFieldConst.TCAWrap.TCapacityAccessor.ReadCapacitySafe(
  const AMem: Pointer): Cardinal;
begin
  Result := TData.CapValue;
end;

class procedure TLazListAspectCapacityFieldConst.TCAWrap.TCapacityAccessor.WriteCapacity(
  const AMem: Pointer; AValue: Cardinal);
begin
  assert((AValue = 0) or (AValue = TData.CapValue), 'TLazListAspectCapacityFieldConst.TCAWrap.TCapacityAccessor.WriteCapacity: (AValue = 0) or (AValue = TData.CapValue)');
  // nothing
end;

{ TLazListAspectRangeIndexCheckReturnNil }

generic class function TLazListAspectRangeIndexCheckReturnNil.CheckIndex<T>(const AList: T;
  const AnIndex: Integer; var Res: Pointer): Boolean;
begin
  Result := (AnIndex >= 0) and (AnIndex < AList.Count);
  Res := nil;
end;

generic class function TLazListAspectRangeIndexCheckReturnNil.CheckInsert<T>(const AList: T;
  var AnIndex: Integer; var Res: Pointer): Boolean;
begin
  Result := (AnIndex >= 0) and (AnIndex < AList.Count);
  Res := nil;
end;

generic class function TLazListAspectRangeIndexCheckReturnNil.CheckDelete<T>(const AList: T;
  var AnIndex, ACount: Integer): Boolean;
begin
  Result := (AnIndex >= 0) and (AnIndex + ACount <= AList.Count);
end;

generic class function TLazListAspectRangeIndexCheckReturnNil.CheckMove<T>(const AList: T;
  var AFromIndex, AToIndex, ACount: Integer): Boolean;
var
  c: Integer;
begin
  Result := (AFromIndex >= 0) and (AToIndex >= 0);
  if Result then begin
    c := AList.Count;
    Result := (AFromIndex + ACount <= c) and (AToIndex + ACount <= c);
  end;
end;

generic class function TLazListAspectRangeIndexCheckReturnNil.CheckSwap<T>(const AList: T;
  var AnIndex1, AnIndex2: Integer): Boolean;
var
  c: Integer;
begin
  Result := (AnIndex1 >= 0) and (AnIndex2 >= 0);
  if Result then begin
    c := AList.Count;
    Result := (AnIndex1 < c) and (AnIndex2 < c);
  end;
end;

{ TLazListAspectRangeIndexCheckExcept }

generic class function TLazListAspectRangeIndexCheckExcept.CheckIndex<T>(const AList: T;
  const AnIndex: Integer; var Res: Pointer): Boolean;
begin
  Result := (AnIndex >= 0) and (AnIndex < AList.Count);
  if not Result then
    raise TLazListClassesIndexError.Create('Index out of range');
end;

generic class function TLazListAspectRangeIndexCheckExcept.CheckInsert<T>(const AList: T;
  var AnIndex: Integer; var Res: Pointer): Boolean;
begin
  Result := (AnIndex >= 0) and (AnIndex <= AList.Count);
  if not Result then
    raise TLazListClassesIndexError.Create('Index out of range');
end;

generic class function TLazListAspectRangeIndexCheckExcept.CheckDelete<T>(const AList: T;
  var AnIndex, ACount: Integer): Boolean;
begin
  Result := (AnIndex >= 0) and (AnIndex + ACount <= AList.Count);
  if not Result then
    raise TLazListClassesIndexError.Create('Index out of range');
end;

generic class function TLazListAspectRangeIndexCheckExcept.CheckMove<T>(const AList: T;
  var AFromIndex, AToIndex, ACount: Integer): Boolean;
var
  c: Integer;
begin
  Result := (AFromIndex >= 0) and (AToIndex >= 0);
  if Result then begin
    c := AList.Count;
    Result := (AFromIndex + ACount <= c) and (AToIndex + ACount <= c);
  end;            ;
  if not Result then
    raise TLazListClassesIndexError.Create('Index out of range');
end;

generic class function TLazListAspectRangeIndexCheckExcept.CheckSwap<T>(const AList: T;
  var AnIndex1, AnIndex2: Integer): Boolean;
var
  c: Integer;
begin
  Result := (AnIndex1 >= 0) and (AnIndex2 >= 0);
  if Result then begin
    c := AList.Count;
    Result := (AnIndex1 < c) and (AnIndex2 < c);
  end;
  if not Result then
    raise TLazListClassesIndexError.Create('Index out of range');
end;

{ TGenLazShiftList }

function TGenLazShiftList.GetItemPointer(const Index: Integer): TPItemT;
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  if not FIndexChecker.specialize CheckIndex<TGenLazShiftList>(Self, Index, Result) then exit;

  MPtr := FMem.FMem;
  assert((not FMem.IsAllocated) or (Cardinal(Index) <= FCapacityAccessor.ReadCapacitySafe(FMem.FMem)), 'TGenLazShiftList.GetItemPointer: (not FMem.IsAllocated) or (Cardinal(Index) <= FCapacityAccessor.ReadCapacitySafe(FMem.FMem))');
  Result := TPItemT(@MPtr^.Data + PtrUInt(MPtr^.FirstItemIdx + Cardinal(Index)) * FItemSize.ItemSize);
end;

function TGenLazShiftList.GetItemPointerFast(const Index: PtrUInt): TPItemT;
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  assert(Cardinal(Index) <= FCapacityAccessor.ReadCapacitySafe(MPtr), 'TGenLazShiftList.GetItemPointerFast: Cardinal(Index) <= FCapacityAccessor.ReadCapacitySafe(MPtr)');
  Result := TPItemT(@MPtr^.Data + (MPtr^.FirstItemIdx + Index) * FItemSize.ItemSize);
end;

function TGenLazShiftList.GetFirstItemPointerFast: TPItemT;
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  Result := TPItemT(@MPtr^.Data + MPtr^.FirstItemIdx * FItemSize.ItemSize);
end;

function TGenLazShiftList.GetItemSize: Cardinal;
begin
  Result := FItemSize.ItemSize;
end;

procedure TGenLazShiftList.SetCapacity(const AValue: Integer);
var
  Cnt: Integer;
begin
  Cnt := Count;
  if AValue < Cnt then
    SetCapacityEx(Cnt, 0, 0)
  else
    SetCapacityEx(AValue, 0, 0);
end;

function TGenLazShiftList.GetCapacity: Integer;
begin
  Result := FCapacityAccessor.ReadCapacitySafe(FMem.FMem);
end;

function TGenLazShiftList.GetCount: Integer;
begin
  Result := FMem.Count;
end;

function TGenLazShiftList.SetCapacityEx(const AValue, AnInsertPos: Cardinal;
  const AnInsertSize: Integer): TPItemT;
var
  NewMem: TLazListClassesInternalMem;
  Cnt, NewCnt, NewIdx: Cardinal;
  Offs: PtrUInt;
  PSource, PData: PByte;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  assert((AnInsertPos = 0) or (AnInsertSize <> 0), 'TGenLazShiftList._SetCapacityEx: (AnInsertPos = 0) or (AnInsertSize <> 0)');

  Result := nil;
  if AValue = 0 then begin
    assert((AnInsertSize<=0) and (AnInsertPos=0), 'TGenLazShiftList._SetCapacityEx: (AnInsertSize<=0) and (AnInsertPos=0)');
    FMem.Free;
    exit;
  end;

  MPtr := FMem.FMem;
  if MPtr = nil then begin
    Cnt := 0;
    NewCnt := AnInsertSize;
  end
  else begin
    if (AValue = FCapacityAccessor.ReadCapacity(MPtr)) then begin
      assert(AnInsertSize = 0, 'TGenLazShiftList._SetCapacityEx: AnInsertSize = 0');
      exit;
    end;

    Cnt := MPtr^.Count;
    NewCnt := Cnt + AnInsertSize;
  end;
  assert(NewCnt <= AValue, 'TGenLazShiftList._SetCapacityEx: NewCnt <= AValue');


  {%H-}NewMem.Init;
  NewMem.Alloc(AValue * FItemSize.ItemSize);
  NewIdx := Cardinal(AValue-NewCnt) div 2;
  MPtr := NewMem.FMem;
  MPtr^.FirstItemIdx := NewIdx;
  Result := TPItemT(@MPtr^.Data);

  NewMem.Count := NewCnt;
  FCapacityAccessor.WriteCapacity(MPtr, AValue);
  assert((NewIdx <= AValue - NewCnt), 'TGenLazShiftList._SetCapacityEx: (NewIdx <= AValue - NewCnt)');

  if Cnt <> 0 then begin
    FInitMem.InitMemOnce(Result, NewIdx, FItemSize.ItemSize);
    Result := TPItemT(PByte(Result) + (NewIdx * FItemSize.ItemSize));
    PSource := PByte(GetFirstItemPointerFast);
    if AnInsertPos <> 0 then begin
      Offs := AnInsertPos * FItemSize.ItemSize;
      Move(PSource^, Result^, Offs);
      PSource := PSource + Offs;
      Result := TPItemT(PByte(Result)+Offs);
    end;

    if AnInsertSize < 0 then begin
      if AnInsertPos < NewCnt then begin
        PSource := PSource + (Cardinal(-AnInsertSize) * FItemSize.ItemSize);
        Offs := (Cardinal(NewCnt - AnInsertPos) * FItemSize.ItemSize);
        Move(PSource^, Result^, Offs);
      end
      else
        Offs := 0;
      FInitMem.InitMemOnce(PByte(Result)+Offs, AValue-NewCnt-NewIdx, FItemSize.ItemSize);
    end
    else begin
      if AnInsertPos < Cnt then begin
        Offs := (Cardinal(Cnt - AnInsertPos) * FItemSize.ItemSize);
        PData := (PByte(Result) + (Cardinal(AnInsertSize) * FItemSize.ItemSize));
        Move(PSource^,
             PData^,
             Offs
        );
        FInitMem.InitMemOnce(PByte(Result),AnInsertSize, FItemSize.ItemSize);
      end
      else begin
        Offs := 0;
        NewCnt :=  NewCnt - AnInsertSize; // still need to InitMemOnce
        PData := PByte(Result);
      end;
      FInitMem.InitMemOnce(PByte(PData)+Offs, AValue-NewCnt-NewIdx, FItemSize.ItemSize);
    end;
  end
  else begin
    assert(AnInsertPos=0, 'TGenLazShiftList.SetCapacityEx: AnInsertPos=0');
    FInitMem.InitMemOnce(Result, AValue, FItemSize.ItemSize);
    Result := TPItemT(PByte(Result) + (NewIdx * FItemSize.ItemSize));
  end;

  FMem.Free;
  FMem := NewMem;
end;

function TGenLazShiftList.InsertRows(AnIndex, ACount: Integer): TPItemT;
var
  Cnt, Cap, CntFreeFront, CntFreeEnd, c, c2: Cardinal;
  i: Integer;
  Offs: PtrUInt;
  CanFront, CanEnd: Boolean;
  PTarget, PSource: PByte;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  if not FIndexChecker.specialize CheckInsert<TGenLazShiftList>(Self, AnIndex, Result) then exit;

  Result := nil;
  if ACount = 0 then exit;

  MPtr := FMem.FMem;
  if MPtr <> nil then begin
    Cnt := MPtr^.Count;
    Cap := FCapacityAccessor.ReadCapacity(MPtr)
  end
  else begin
    Cnt := 0;
    Cap := 0;
  end;
  assert((ACount>0) and (AnIndex<=Cnt), 'TGenLazShiftList._InsertRows: (ACount>0) and (AnIndex<=Cnt)');

  if Cnt + ACount > Cap then begin
    Result := SetCapacityEx(FCapacity.GrowCapacity(Cnt + ACount, Cap), AnIndex, ACount);
    FInitMem.InitMem(Result, ACount, FItemSize.ItemSize);
    exit;
  end;

  CntFreeFront := MPtr^.FirstItemIdx;
  CntFreeEnd   := Cap - CntFreeFront - Cnt;
  CanFront := CntFreeFront >= ACount;
  CanEnd   := CntFreeEnd >= ACount;

  if not(CanFront or CanEnd)
  then begin
    i := FCapacity.GrowCapacity(Cnt + ACount, Cap);
    if i > Cap then begin
      Result := SetCapacityEx(i, AnIndex, ACount);
      FInitMem.InitMem(Result, ACount, FItemSize.ItemSize);
      exit;
    end;
  end;

  if CanFront and ((Cardinal(AnIndex) * 2 < Cardinal(Cnt)) or (not CanEnd)) then begin
    // use space at front of list
    c := ACount;
    c2 := CntFreeFront-ACount;
    if (AnIndex = Cnt) and (c2 >= CntFreeEnd+2) then             // move all entries;
      c := c + (Cardinal(c2-CntFreeEnd) div 2 - 1);    // Make some room at the end of the list

    PSource := PByte(GetFirstItemPointerFast);
    PTarget := PSource - (c * FItemSize.ItemSize);
    if AnIndex <> 0 then begin
      Offs := AnIndex * FItemSize.ItemSize;
      Move(PSource^, PTarget^, Offs);
      Result := TPItemT(PTarget + Offs);
    end
    else
      Result := TPItemT(PTarget);
    FInitMem.InitMem(Result, ACount, FItemSize.ItemSize);

    assert(PTarget >= @MPtr^.Data, 'TGenLazShiftList._InsertRows: PTarget >= @MPtr^.Data');
    MPtr^.FirstItemIdx := MPtr^.FirstItemIdx - c;
    FMem.Count := Cnt + ACount;
  end
  else
  if CanEnd then begin
    // use space at end of list
    c2 := CntFreeEnd-ACount;
    if (AnIndex = 0) and (c2 >= CntFreeFront+2) then             // move all entries;
      c := (Cardinal(c2-CntFreeFront) div 2 - 1)    // Make some room at the end of the list
    else
      c := 0;

    PSource := PByte(GetItemPointerFast(AnIndex));
    Offs := Cnt - AnIndex;
    if Offs <> 0 then begin
      PTarget := PSource + (Cardinal(ACount + c) * FItemSize.ItemSize);
      Move(PSource^, PTarget^, Offs * FItemSize.ItemSize);
    end;

    if c <> 0 then begin
      assert(PSource + (c * FItemSize.ItemSize) >= @MPtr^.Data, 'TGenLazShiftList._InsertRows: PSource + (c * FItemSize.ItemSize) >= @MPtr^.Data');
      PSource := PSource + (c * FItemSize.ItemSize);
      MPtr^.FirstItemIdx := MPtr^.FirstItemIdx + AnIndex + c;
    end;
    FInitMem.InitMem(PSource, ACount, FItemSize.ItemSize);
    Result := TPItemT(PSource);
    FMem.Count := Cnt + ACount;
  end
  else
  begin
 	// split to both ends
    assert((cap >= ACount) and (CntFreeFront> 0) and (CntFreeEnd > 0), 'TGenLazShiftList._InsertRows: (cap >= ACount) and (CntFreeFront> 0) and (CntFreeEnd > 0)');
    i := Max(integer(Cardinal(Cap-Cnt-ACount) div 2) - 1, 0); // remaining free space AFTER insert
    i := CntFreeFront - i;

    PSource := PByte(GetFirstItemPointerFast);
    PTarget := PSource - (Cardinal(i) * FItemSize.ItemSize);
    if AnIndex <> 0 then begin
      Offs := AnIndex * FItemSize.ItemSize;
      Move(PSource^, PTarget^, Offs);
      Result := TPItemT(PTarget + Offs);
    end
    else begin
      Offs := 0;
      Result := TPItemT(PTarget);
    end;

    MPtr^.FirstItemIdx := MPtr^.FirstItemIdx - (i) ;
    FMem.Count := Cnt + ACount;

    assert((ACount>i) and (ACount-(i)<=CntFreeEnd), 'TGenLazShiftList._InsertRows: (ACount>i) and (ACount-(i)<=CntFreeEnd)');
    PSource := PSource + Offs;
    Offs := Cnt - AnIndex;
    if Offs <> 0 then begin
      PTarget := PSource + ((ACount - (i)) * FItemSize.ItemSize);
      Move(PSource^, PTarget^, Offs * FItemSize.ItemSize);
    end;

    FInitMem.InitMem(Result, ACount, FItemSize.ItemSize);
  end;

  assert((MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr)), 'TGenLazShiftList.InsertRows: (MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr))');
end;

procedure TGenLazShiftList.DeleteRows(AnIndex, ACount: Integer);
var
  Cnt, c: Cardinal;
  IdxOffs: PtrUInt;
  i: Integer;
  PSource: PByte;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  if not FIndexChecker.specialize CheckDelete<TGenLazShiftList>(Self, AnIndex, ACount) then exit;

  if ACount = 0 then exit;

  MPtr := FMem.FMem;
  Cnt := MPtr^.Count - ACount;
  assert((ACount>0)  and (AnIndex<=Cnt), 'TGenLazShiftList._DeleteRows: (ACount>0)  and (AnIndex<=Cnt)');

  PSource := PByte(GetFirstItemPointerFast);
  IdxOffs := AnIndex * FItemSize.ItemSize;
  FInitMem.FinalizeMem(PSource + IdxOffs, ACount, FItemSize.ItemSize);

  c := FCapacityAccessor.ReadCapacity(MPtr);
  i := FCapacity.ShrinkCapacity(Cnt, c);
  if (i >= 0) and (i < c) then begin
    SetCapacityEx(i, AnIndex, -ACount);
    exit;
  end;

  if Cardinal(AnIndex) * 2 < Cnt then begin
    // use space at front of list
    if AnIndex <> 0 then begin
      Move(PSource^,
           (PSource + (ACount * FItemSize.ItemSize))^,
           IdxOffs);
    end;
    MPtr^.FirstItemIdx := MPtr^.FirstItemIdx + ACount;
  end
  else begin
    // use space at end of list
    if Cnt > AnIndex then begin
      PSource := PSource + IdxOffs; // This is the target now
      Move((PSource + (ACount * FItemSize.ItemSize))^,
           PSource^,
           (Cnt-AnIndex) * FItemSize.ItemSize);
    end;
  end;

  MPtr^.Count := Cnt;
end;

procedure TGenLazShiftList.Create;
begin
  FCapacity.Init;
  FInitMem.Init;
  FMem.Init;
end;

procedure TGenLazShiftList.Destroy;
begin
  if @FInitMem.FinalizeMem <> @TLazListAspectMemInitNone.FinalizeMem then
    DeleteRows(0, Count);
  FMem.Free;
end;

procedure TGenLazShiftList.MoveRows(AFromIndex, AToIndex, ACount: Integer);
var
  DistanceToMove: PtrUInt;
  p, pFrom, pTo: PByte;
  Cnt: Cardinal;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  if not FIndexChecker.specialize CheckMove<TGenLazShiftList>(Self, AFromIndex, AToIndex, ACount) then exit;

  MPtr := FMem.FMem;
  Cnt := MPtr^.Count;
  assert((AFromIndex+ACount<=Cnt) and (AToIndex+ACount<=Cnt), 'TGenLazShiftList._MoveRows: (AFromIndex+ACount<=Cnt) and (AToIndex+ACount<=Cnt)');

  pFrom := PByte(GetItemPointerFast(AFromIndex));
  pTo   := PByte(GetItemPointerFast(AToIndex));

  if (ACount << 1) > Cnt then begin
    // more than half => ranges will overlap: so InitMem only needs to deal with overlap-move
    p := PByte(GetFirstItemPointerFast);
    if AToIndex < AFromIndex then begin
      // free at end? (instead of moving entries down, move surroundings up
       if FCapacityAccessor.ReadCapacity(MPtr) - (MPtr^.FirstItemIdx + Cnt) >= AFromIndex - AToIndex then begin
        DistanceToMove := pFrom - pTo;
        Move(p^, (p + DistanceToMove)^, pTo - p);
        pFrom := pFrom + FItemSize.ItemSize * ACount;
        Move(pFrom^, (pFrom + DistanceToMove)^, p + Cnt * FItemSize.ItemSize - pFrom);
        MPtr^.FirstItemIdx := MPtr^.FirstItemIdx + (AFromIndex - AToIndex);
        assert((MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr)), 'TGenLazShiftList.MoveRows: (MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr))');
        FInitMem.InitMem(pFrom, AFromIndex-AToIndex, FItemSize.ItemSize);
        exit;
      end;
    end
    else begin
      // free at front (instead of moving entries up, move surroundings down
      if MPtr^.FirstItemIdx >= (AToIndex - AFromIndex) then begin
        DistanceToMove := pTo - pFrom;
        Move(p^, (p - DistanceToMove)^, pFrom - p);
        // pTo is used as "PFrom_2"
        pTo := pFrom + FItemSize.ItemSize * ACount;
        Move((pTo + DistanceToMove)^, pTo^, p + Cnt * FItemSize.ItemSize - pTo - DistanceToMove);
        MPtr^.FirstItemIdx := MPtr^.FirstItemIdx - (AToIndex - AFromIndex);
        assert((MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr)), 'TGenLazShiftList.MoveRows: (MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr))');
        FInitMem.InitMem(pFrom-DistanceToMove, AToIndex-AFromIndex, FItemSize.ItemSize);
        exit;
      end;
    end;
  end;

  Move(pFrom^, pTo^, FItemSize.ItemSize * ACount);
  if AFromIndex < AToIndex then
    FInitMem.InitMem(pFrom, min(ACount, AToIndex-AFromIndex), FItemSize.ItemSize)
  else
    FInitMem.InitMem(pFrom + max(0, (ACount - (AFromIndex-AToIndex))) * FItemSize.ItemSize,
      min(ACount, AFromIndex-AToIndex), FItemSize.ItemSize);
  assert((MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr)), 'TGenLazShiftList.MoveRows: (MPtr^.FirstItemIdx < FCapacityAccessor.ReadCapacity(MPtr))');
end;

procedure TGenLazShiftList.SwapEntries(AnIndex1, AnIndex2: Integer);
var
  t: PByte;
begin
  if not FIndexChecker.specialize CheckSwap<TGenLazShiftList>(Self, AnIndex1, AnIndex2) then exit;

  t := Getmem(FItemSize.ItemSize);
  Move(PByte(GetItemPointerFast(AnIndex1))^, t^, FItemSize.ItemSize);
  Move(PByte(GetItemPointerFast(AnIndex2))^, PByte(GetItemPointerFast(AnIndex1))^, FItemSize.ItemSize);
  Move(t^, PByte(GetItemPointerFast(AnIndex2))^, FItemSize.ItemSize);
  FreeMem(t);
end;

procedure TGenLazShiftList.DebugDump;
var i : integer; s:string;
begin
  if fmem.IsAllocated then begin
    dbgout(['TLazFixedRoundBufferListMemBase.Dump ', FCapacityAccessor.ReadCapacitySafe(FMem.FMem), ' , ',FMem.Count,
    ' --- ', @fmem.FMem^.Data, ' , ',FMem.FMem^.FirstItemIdx,' --- ', ': ']);
     s :='';
    for i := 0 to FMem.Count - 1 do s := s +dbgMemRange(itempointer[i], FItemSize.ItemSize )+ ', ';
    debugln(s);
  end
  else debugln(['TLazFixedRoundBufferListMemBase.Dump NONE']);
end;

function TGenLazShiftList.IndexOf(const AnItem: TPItemT): integer;
var
  p: Pointer;
  c: Integer;
  s: Cardinal;
begin
  c := Count;
  if c = 0 then
    exit(-1);

  s := FItemSize.ItemSize;
  p := ItemPointerFast[0];
  Result := 0;
  while Result < c do begin
    if CompareMem(p, AnItem, s) then exit;
    p := p + s;
    inc(Result);
  end;
  Result := -1;
end;

{ TGenLazShiftListVarSize }

procedure TGenLazShiftListVarSize.Create(const AnItemSize: Cardinal);
begin
  fItemSize.ItemSize := AnItemSize;
  inherited Create;
end;

{ TGenLazShiftListFixedType }

function TGenLazShiftListFixedType.Get(const Index: Integer): T;
begin
  Result := ItemPointer[Index]^;
end;

procedure TGenLazShiftListFixedType.Put(const Index: Integer; AValue: T);
begin
  ItemPointerFast[Index]^ := AValue;
end;

function TGenLazShiftListFixedType.IndexOf(AnItem: T): integer;
var
  p: PT;
  c: Integer;
  s: Cardinal;
begin
  c := Count;
  if c = 0 then
    exit(-1);

  s := FItemSize.ItemSize;
  p := ItemPointerFast[0];
  Result := 0;
  while Result < c do begin
    if p^ = AnItem then exit;
    p := pointer(p) + s;
    inc(Result);
  end;
  Result := -1;
end;

{ TGenLazRoundList }

function TGenLazRoundList.GetItemPointer(const Index: Integer): TPItemT;
var
  Idx, c: Cardinal;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  if not FIndexChecker.specialize CheckIndex<TGenLazRoundList>(Self, Index, Result) then exit;

  MPtr := FMem.FMem;
  assert(Index <= FCapacityAccessor.ReadCapacity(MPtr), 'TGenLazRoundList.GetItemPointer: Index <= FCapacityAccessor.ReadCapacity(MPtr)');
  Idx := MPtr^.FirstItemIdx + Cardinal(Index);
  c := FCapacityAccessor.ReadCapacity(MPtr);
  if Idx >= c then
    Idx := Idx - c;
  Result := TPItemT(@MPtr^.Data + Idx * FItemSize.ItemSize);
end;

function TGenLazRoundList.GetItemPointerFast(const Index: Cardinal): TPItemT;
var
  Idx, c: Cardinal;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  assert(Cardinal(Index) <= FCapacityAccessor.ReadCapacity(MPtr), 'TGenLazRoundList.GetItemPointerFast: Cardinal(Index) <= FCapacityAccessor.ReadCapacity(MPtr)');
  Idx := MPtr^.FirstItemIdx + Index;
  c := FCapacityAccessor.ReadCapacity(MPtr);
  if Idx >= c then
    Idx := Idx - c;
  Result := TPItemT(@MPtr^.Data + Idx * FItemSize.ItemSize);
end;

function TGenLazRoundList.GetItemPointerFast(const Index, ACapacity: Cardinal
  ): TPItemT;
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
  Idx: Cardinal;
begin
  MPtr := FMem.FMem;
  assert(Index <= ACapacity, 'TGenLazRoundList.GetItemPointerFast: Index <= ACapacity');
  Idx := MPtr^.FirstItemIdx + Index;
  if Idx >= ACapacity then
    Idx := Idx - ACapacity;
  Result := TPItemT(@MPtr^.Data + Idx * FItemSize.ItemSize);
end;

function TGenLazRoundList.GetItemPointerFast(const Index, ACapacity: Cardinal;
  const AnMPtr: Pointer): TPItemT;
var
  MPtr: TLazListClassesInternalMem.PMemRecord absolute AnMPtr;
  Idx: Cardinal;
begin
  assert(Index <= ACapacity, 'TGenLazRoundList.GetItemPointerFast: Index <= ACapacity');
  Idx := MPtr^.FirstItemIdx + Index;
  if Idx >= ACapacity then
    Idx := Idx - ACapacity;
  Result := TPItemT(@MPtr^.Data + Idx * FItemSize.ItemSize);
end;

function TGenLazRoundList.GetItemSize: Cardinal;
begin
  Result := FItemSize.ItemSize;
end;

procedure TGenLazRoundList.SetCapacity(const AValue: Integer);
var
  Cnt: Integer;
begin
  Cnt := Count;
  if AValue < Cnt then
    SetCapacityEx(Cnt, 0, 0)
  else
    SetCapacityEx(AValue, 0, 0);
end;

function TGenLazRoundList.GetCapacity: Integer;
begin
  Result := FCapacityAccessor.ReadCapacitySafe(FMem.FMem);
end;

function TGenLazRoundList.GetCount: Integer;
begin
  Result := FMem.Count;
end;

procedure TGenLazRoundList.InternalMoveUp(const AFromEnd, AToEnd: PByte; AByteCnt: Cardinal;
  const AByteCap: Integer);
var
  c: Integer;
  l, pToEnd, pFromEnd: PByte;
begin
  assert(AFromEnd <> AToEnd, 'TGenLazRoundList.InternalMoveUp: AFromEnd <> AToEnd');
  l := @FMem.FMem^.Data;
  pToEnd := AToEnd;
  pFromEnd := AFromEnd;
  if pToEnd = l then pToEnd := l + AByteCap;
  if pFromEnd = l then pFromEnd := l + AByteCap;

  if pToEnd < pFromEnd then begin
    c := Min(pToEnd - l, AByteCnt);
    pFromEnd := pFromEnd - c;
    pToEnd := pToEnd - c;
    Move(pFromEnd^, pToEnd^, c);
    AByteCnt := AByteCnt - c;
    if AByteCnt = 0 then
      exit;
    pToEnd := l + AByteCap;
  end;

  c := Min(pFromEnd - l, AByteCnt);
  pFromEnd := pFromEnd - c;
  pToEnd := pToEnd - c;
  Move(pFromEnd^, pToEnd^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  pFromEnd := l + AByteCap;

  c := Min(pToEnd - l, AByteCnt);
  pFromEnd := pFromEnd - c;
  pToEnd := pToEnd - c;
  Move(pFromEnd^, pToEnd^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  pToEnd := l + AByteCap;

  Move((pFromEnd-AByteCnt)^, (pToEnd-AByteCnt)^, AByteCnt);
end;

procedure TGenLazRoundList.InternalMoveDown(const AFrom, ATo: PByte; AByteCnt: Cardinal;
  const AUpperBound: PByte);
var
  c: Integer;
  l, pFrom, pTo: PByte;
begin
  assert(AFrom <> ATo, 'TGenLazRoundList.InternalMoveDown: AFrom <> ATo');
  l := @FMem.FMem^.Data;
  pFrom := AFrom;
  pTo := ATo;
  if pTo > pFrom then begin
    c := Min(AUpperBound - pTo, AByteCnt);
    Move(pFrom^, pTo^, c);
    AByteCnt := AByteCnt - c;
    if AByteCnt = 0 then
      exit;
    pTo := l; // pTo + c - AByteCap;
    pFrom := pFrom + c;
  end;

  c := Min(AUpperBound - pFrom, AByteCnt);
  Move(pFrom^, pTo^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  pFrom := l; // pFrom + c - AByteCap;
  pTo := pTo + c;

  c := Min(AUpperBound - pTo, AByteCnt);
  Move(pFrom^, pTo^, c);
  AByteCnt := AByteCnt - c;
  if AByteCnt = 0 then
    exit;
  pTo := l; // pTo + c - AByteCap;
  pFrom := pFrom + c;

  Move(pFrom^, pTo^, AByteCnt);
end;

procedure TGenLazRoundList.MoveRowsUp(const AFromIndex, AToIndex, ACount: Cardinal);
var
  Cnt, Cap, CapBytes, c, Diff: Cardinal;
  BytesToMove: Cardinal;
  u, pFrom, pTo: PByte;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  assert((AToIndex>AFromIndex) and (AToIndex+ACount<=Count), 'TGenLazRoundList._MoveRowsUp: (AToIndex>AFromIndex) and (AToIndex+ACount<=Count)');

  MPtr := FMem.FMem;
  Cnt := MPtr^.Count;
  Cap := FCapacityAccessor.ReadCapacity(MPtr);
  CapBytes := Cap * Cardinal(FItemSize.ItemSize);

  if (ACount * 2) >= Cnt then begin
    // FirstItemIndex = FirstItemIndex - Diff; // move ALL up
    Diff := AToIndex-AFromIndex;
    u := @MPtr^.Data + CapBytes;
    // Save data from after Target; move it after Source
    InternalMoveDown(PByte(GetItemPointerFast(AToIndex+ACount, Cap, MPtr)),
                     PByte(GetItemPointerFast(AFromIndex+ACount, Cap, MPtr)),
                     (Cnt - (AToIndex+ACount)) * FItemSize.ItemSize, u);
    // Move data before SOURCE down (may be below 0 / wrap)
    InternalMoveDown(PByte(GetItemPointerFast(0, Cap, MPtr)), PByte(GetItemPointerFast(Cap-Diff, Cap, MPtr)),
                     AFromIndex * FItemSize.ItemSize, u);
    c := MPtr^.FirstItemIdx;
    if c < Diff then
      c := c + Cap;
    c := c - Diff;
    MPtr^.FirstItemIdx := c;

    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then begin
      c := (AFromIndex + c);
      if c >= Cap then
        c := 2*Cap - c
      else
        c := Cap - c;
      if c < Diff then begin
        FInitMem.InitMem(PByte(GetItemPointerFast(AFromIndex, Cap, MPtr)), c, FItemSize.ItemSize);
        FInitMem.InitMem(@MPtr^.Data, Diff - c, FItemSize.ItemSize);
      end
      else
        FInitMem.InitMem(PByte(GetItemPointerFast(AFromIndex, Cap, MPtr)), Diff, FItemSize.ItemSize);
    end;
  end
  else begin
    // normal move
    BytesToMove := FItemSize.ItemSize * ACount;
    pFrom := PByte(GetItemPointerFast(AFromIndex+ACount, Cap, MPtr));
    pTo   := PByte(GetItemPointerFast(AToIndex+ACount, Cap, MPtr));
    InternalMoveUp(pFrom, pTo, BytesToMove, CapBytes);

    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then begin
      Diff := min(ACount, AToIndex-AFromIndex);
      c := (AFromIndex + MPtr^.FirstItemIdx);
      if c >= Cap then
        c := 2*Cap - c
      else
        c := Cap - c;
      if c < Diff then begin
        FInitMem.InitMem(PByte(GetItemPointerFast(AFromIndex, Cap, MPtr)), c, FItemSize.ItemSize);
        FInitMem.InitMem(@MPtr^.Data, Diff - c, FItemSize.ItemSize);
      end
      else
        FInitMem.InitMem(PByte(GetItemPointerFast(AFromIndex, Cap, MPtr)), Diff, FItemSize.ItemSize);
    end;
  end;
  assert((MPtr^.FirstItemIdx < Capacity), 'TGenLazRoundList._MoveRowsUp: (MPtr^.FirstItemIdx < Capacity)');
end;

procedure TGenLazRoundList.MoveRowsDown(const AFromIndex, AToIndex, ACount: Cardinal);
var
  Cnt, Cap, CapBytes, c, Diff: Cardinal;
  BytesToMove, f: Cardinal;
  pFrom, pTo: PByte;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  assert((AFromIndex>AToIndex) and (AFromIndex+ACount<=Count), 'TGenLazRoundList._MoveRowsDown: (AFromIndex>AToIndex) and (AFromIndex+ACount<=Count)');

  MPtr := FMem.FMem;
  Cnt := MPtr^.Count;
  Cap := FCapacityAccessor.ReadCapacity(MPtr);
  CapBytes := Cap * Cardinal(FItemSize.ItemSize);

  if (ACount * 2) >= Cnt then begin
    // FirstItemIndex = FirstItemIndex + Diff; // move ALL down
    Diff := AFromIndex-AToIndex;
    // Save data in front of AToIndex; move it in front of AFromIndex;
    InternalMoveUp(PByte(GetItemPointerFast(AToIndex, Cap, MPtr)), PByte(GetItemPointerFast(AFromIndex, Cap, MPtr)),
                   AToIndex * FItemSize.ItemSize, CapBytes);
    // Move data after END-OF-SOURCE up (moving behind current count (wrap around capacity if needed))
    c := Cnt + Diff;
    if Cardinal(c) > Cap then
      c := Cardinal(c) - Cap;
    InternalMoveUp(PByte(GetItemPointerFast(Cnt, Cap, MPtr)),
                   PByte(GetItemPointerFast(c, Cap, MPtr)), // Cnt=Cap will be handled by GetItemPointerFast
                   (Cnt - (AFromIndex+ACount)) * FItemSize.ItemSize, CapBytes);
    c := MPtr^.FirstItemIdx + Diff;
    if Cardinal(c) >= Cap then
      c := Cardinal(c) - Cap;
    MPtr^.FirstItemIdx := c;

    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then begin
      f := AToIndex + ACount;
      c := (f + c);
      if c >= Cap then
        c := 2*Cap - c
      else
        c := Cap - c;
      if c < Diff then begin
        FInitMem.InitMem(PByte(GetItemPointerFast(f, Cap, MPtr)), c, FItemSize.ItemSize);
        FInitMem.InitMem(@MPtr^.Data, Diff - c, FItemSize.ItemSize);
      end
      else
        FInitMem.InitMem(PByte(GetItemPointerFast(AToIndex+ACount, Cap, MPtr)), Diff, FItemSize.ItemSize);
    end;
  end
  else begin
    // normal move
    BytesToMove := FItemSize.ItemSize * ACount;
    pFrom := PByte(GetItemPointerFast(AFromIndex, Cap, MPtr));
    pTo   := PByte(GetItemPointerFast(AToIndex, Cap, MPtr));
    InternalMoveDown(pFrom, pTo, BytesToMove, @MPtr^.Data + CapBytes);

    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then begin
      Diff := AFromIndex-AToIndex;
      f := AFromIndex;
      if ACount > Diff then
        f := f + ACount - Diff;
      Diff := min(ACount, Diff);
      c := (f + MPtr^.FirstItemIdx);
      if c >= Cap then
        c := 2*Cap - c
      else
        c := Cap - c;
      if c < Diff then begin
        FInitMem.InitMem(PByte(GetItemPointerFast(f, Cap, MPtr)), c, FItemSize.ItemSize);
        FInitMem.InitMem(@MPtr^.Data, Diff - c, FItemSize.ItemSize);
      end
      else
        FInitMem.InitMem(PByte(GetItemPointerFast(f, Cap, MPtr)), Diff, FItemSize.ItemSize);
    end;
  end;
  assert((MPtr^.FirstItemIdx < Capacity), 'TGenLazRoundList._MoveRowsDown: (MPtr^.FirstItemIdx < Capacity)');
end;

function TGenLazRoundList.SetCapacityEx(const AValue, AnInsertPos: Cardinal;
  const AnInsertSize: Integer): TPItemT;
var
  NewMem: TLazListClassesInternalMem;
  Cnt, NewCnt, NewIdx, OldFrstIdx, c: Cardinal;
  siz, siz2: PtrUInt;
  PSource, PData: PByte;
  NewMPtr, MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  assert((AnInsertPos = 0) or (AnInsertSize <> 0), 'TGenLazShiftList._SetCapacityEx: (AnInsertPos = 0) or (AnInsertSize <> 0)');

  Result := nil;
  if AValue = 0 then begin
    assert((AnInsertSize<=0) and (AnInsertPos=0), 'TGenLazShiftList._SetCapacityEx: (AnInsertSize<=0) and (AnInsertPos=0)');
    FMem.Free;
    exit;
  end;

  MPtr := FMem.FMem;
  if MPtr = nil then begin
    Cnt := 0;
    NewCnt := AnInsertSize;
  end
  else begin
    if (AValue = FCapacityAccessor.ReadCapacity(MPtr)) then begin
      assert(AnInsertSize = 0, 'TGenLazShiftList._SetCapacityEx: AnInsertSize = 0');
      exit;
    end;

    Cnt := MPtr^.Count;
    NewCnt := Cnt + AnInsertSize;
  end;
  assert(NewCnt <= AValue, 'TGenLazShiftList._SetCapacityEx: NewCnt <= AValue');


  {%H-}NewMem.Init;
  NewMem.Alloc(AValue * FItemSize.ItemSize);
  NewIdx := Cardinal(AValue-NewCnt) div 2;
  NewMPtr := NewMem.FMem;
  NewMPtr^.FirstItemIdx:= NewIdx;
  Result := TPItemT(@NewMPtr^.Data);

  NewMem.Count := NewCnt;
  FCapacityAccessor.WriteCapacity(NewMem.FMem, AValue);
  assert((NewMPtr^.FirstItemIdx {%H-}< AValue), 'TGenLazRoundList._SetCapacityEx: (NewMPtr^.FirstItemIdx {%H-}< AValue)');

  if Cnt <> 0 then begin
    FInitMem.InitMemOnce(Result, NewIdx, FItemSize.ItemSize);
    Result := TPItemT(PByte(Result) + (NewIdx * FItemSize.ItemSize));
    PData := @MPtr^.Data;
    OldFrstIdx := MPtr^.FirstItemIdx;
    PSource := PData + (OldFrstIdx * FItemSize.ItemSize);
    c := FCapacityAccessor.ReadCapacity(MPtr);
    if OldFrstIdx + Cnt > c then
      PData := PData + c * Cardinal(FItemSize.ItemSize)
    else
      PData := PByte(high(PtrUInt));
    if AnInsertPos <> 0 then begin
      siz := (AnInsertPos * FItemSize.ItemSize);
      siz2 := PtrUInt(PData) - PtrUInt(PSource);
      if siz > siz2 then begin
        Move(PSource^, Result^, siz2);
        Move(MPtr^.Data, (PByte(Result)+siz2)^, siz - siz2);
        PSource := @MPtr^.Data + (siz - siz2);
      end
      else begin
        Move(PSource^, Result^, siz);
        PSource := PSource + siz;
      end;
      Result := TPItemT(PByte(Result) + siz);
    end;

    if AnInsertSize < 0 then begin
      if AnInsertPos < NewCnt then begin
        //PSource := PByte(GetItemPointerFast(AnInsertPos-AnInsertSize, c, MPtr));
        PSource := PByte(GetItemPointerFast(AnInsertPos-AnInsertSize));
        siz := ((NewCnt - AnInsertPos) * FItemSize.ItemSize);
        siz2 := PtrUInt(PData) - PtrUInt(PSource);
        if siz > siz2 then begin
          Move(PSource^, PByte(Result)^, siz2);
          Move(MPtr^.Data, (PByte(Result)+siz2)^, siz - siz2);
        end
        else
          Move(PSource^, PByte(Result)^, siz);
      end
      else
        siz := 0;
      FInitMem.InitMemOnce(PByte(Result)+siz, AValue-NewCnt-NewIdx, FItemSize.ItemSize);
    end
    else begin
      if AnInsertPos < Cnt then begin
        siz := ((Cnt - AnInsertPos) * FItemSize.ItemSize);
        siz2 := PtrUInt(PData) - PtrUInt(PSource);
        // PData becomes target
        PData := PByte(Result) + (AnInsertSize * FItemSize.ItemSize);
        if siz > siz2 then begin
          Move(PSource^, PData^, siz2);
          Move(MPtr^.Data, (PData+siz2)^, siz - siz2);
        end
        else
          Move(PSource^, PData^, siz);

        FInitMem.InitMemOnce(PByte(Result),AnInsertSize, FItemSize.ItemSize);
      end
      else begin
        siz := 0;
        NewCnt :=  NewCnt - AnInsertSize; // still need to InitMemOnce
        PData := PByte(Result);
      end;
      FInitMem.InitMemOnce(PByte(PData)+siz, AValue-NewCnt-NewIdx, FItemSize.ItemSize);
    end;
  end
  else begin
    assert(AnInsertPos=0, 'TGenLazRoundList._SetCapacityEx: AnInsertPos=0');
    FInitMem.InitMemOnce(Result, AValue, FItemSize.ItemSize);
    Result := TPItemT(PByte(Result) + (NewIdx * FItemSize.ItemSize));
  end;

  FMem.Free;
  FMem := NewMem;
end;

function TGenLazRoundList.InsertRows(AnIndex, ACount: Integer): TPItemT;
var
  Cnt, Cap, CapBytes, siz, c: Cardinal;
  PSourceIdx, PTargetIdx: Cardinal;
  PTarget, PSource, m: PByte;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  if not FIndexChecker.specialize CheckInsert<TGenLazRoundList>(Self, AnIndex, Result) then exit;

  Result := nil;
  if ACount = 0 then exit;

  MPtr := FMem.FMem;
  if MPtr = nil then begin
    Cnt := 0;
    Cap := 0;
  end
  else begin
    Cnt := MPtr^.Count;
    Cap := FCapacityAccessor.ReadCapacity(MPtr);
  end;

  assert((ACount>0) and (AnIndex<=Cnt), 'TGenLazRoundList._InsertRows: (ACount>0) and (AnIndex<=Cnt)');

  if Cnt + ACount > Cap then begin
    Result := SetCapacityEx(FCapacity.GrowCapacity(Cnt + ACount, Cap), AnIndex, ACount);
    FInitMem.InitMem(Result, ACount, FItemSize.ItemSize);
    exit;
  end;

  MPtr := FMem.FMem;
  CapBytes := Cap * FItemSize.ItemSize;
  if (AnIndex = 0) or (Cardinal(AnIndex) < Cardinal(Cnt) div 2) then begin
    // use space at front of list
    PSourceIdx := MPtr^.FirstItemIdx;
    if ACount > PSourceIdx then
      PTargetIdx := PSourceIdx + Cap - ACount
    else
      PTargetIdx := PSourceIdx - ACount;
    MPtr^.FirstItemIdx := PTargetIdx;
    FMem.Count := Cnt + ACount;

    PTarget := @MPtr^.Data + PTargetIdx * FItemSize.ItemSize;

    if AnIndex <> 0 then begin
      PSource := @MPtr^.Data + PSourceIdx * FItemSize.ItemSize;
      siz := AnIndex * FItemSize.ItemSize;
      Result := TPItemT(PTarget + siz);
      m := @MPtr^.Data + CapBytes;
      if PByte(Result) >= m then
        Result := TPItemT(PByte(Result) - CapBytes);
      InternalMoveDown(PSource, PTarget, siz, m);
    end
    else
      Result := TPItemT(PTarget);
  end
  else
  begin
    // use space at end of list
    PSource := PByte(ItemPointerFast[Cnt, Cap, MPtr]);
    PTarget := PSource + (ACount * FItemSize.ItemSize);
    if PTarget > @MPtr^.Data + CapBytes then
      PTarget := PTarget - CapBytes;

    FMem.Count := Cnt + ACount;

    if AnIndex < Cnt then begin
      siz := (Cnt-AnIndex) * FItemSize.ItemSize;
      m := @MPtr^.Data;
      Result := TPItemT(PSource - siz);
      if PByte(Result) < m then
        Result := TPItemT(PByte(Result) + CapBytes);
      InternalMoveUp(PSource, PTarget, siz, CapBytes);
    end
    else
      Result := TPItemT(PSource);
  end;

  c := (AnIndex + MPtr^.FirstItemIdx);
  if c >= cap then
    c := 2*Cap - c
  else
    c := Cap - c;
  if c < ACount then begin
    FInitMem.InitMem(PByte(GetItemPointerFast(AnIndex, Cap, MPtr)), c, FItemSize.ItemSize);
    FInitMem.InitMem(@MPtr^.Data, ACount - c, FItemSize.ItemSize);
  end
  else
    FInitMem.InitMem(PByte(GetItemPointerFast(AnIndex, Cap, MPtr)), ACount, FItemSize.ItemSize);

  assert((MPtr^.FirstItemIdx {%H-}< FCapacityAccessor.ReadCapacity(MPtr)), 'TGenLazRoundList._InsertRows: (MPtr^.FirstItemIdx {%H-}< FCapacityAccessor.ReadCapacity(MPtr))');
end;

procedure TGenLazRoundList.DeleteRows(AnIndex, ACount: Integer);
var
  Cnt, Cap, CapBytes, c: Cardinal;
  i: Integer;
  siz: Integer;
  PTarget, PSource, m: PByte;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  if not FIndexChecker.specialize CheckDelete<TGenLazRoundList>(Self, AnIndex, ACount) then exit;

  if ACount = 0 then exit;

  MPtr := FMem.FMem;
  Cnt := MPtr^.Count - ACount;
  Cap := FCapacityAccessor.ReadCapacity(MPtr);
  assert((ACount>0) and (AnIndex<=Cnt), 'TGenLazRoundList._DeleteRows: (ACount>0) and (AnIndex<=Cnt)');

  c := FCapacityAccessor.ReadCapacity(MPtr);
  i := FCapacity.ShrinkCapacity(Cnt, Cap);
  if (i >= 0) and (i < c) then begin
    FInitMem.FinalizeMem(ItemPointerFast[AnIndex, Cap, MPtr], ACount, FItemSize.ItemSize);
    SetCapacityEx(i, AnIndex, -ACount);
    exit;
  end;

  CapBytes := Cap * Cardinal(FItemSize.ItemSize);
  if (AnIndex * 2 < Cnt) or (AnIndex = 0) then begin
    // make space at front of list
    PTarget := PByte(ItemPointerFast[AnIndex+ACount, Cap, MPtr]);
    PSource := PByte(ItemPointerFast[AnIndex, Cap, MPtr]);
    FInitMem.FinalizeMem(PSource, ACount, FItemSize.ItemSize);
    if AnIndex <> 0 then begin
      siz := AnIndex * FItemSize.ItemSize;
      m := @MPtr^.Data;
      InternalMoveUp(PSource, PTarget, siz, CapBytes);
    end;

    c := MPtr^.FirstItemIdx + ACount;
    if c >= Cap then
      c := c - Cap;
    MPtr^.FirstItemIdx := c;
  end
  else begin
    // make space at end of list
    FInitMem.FinalizeMem(PByte(ItemPointerFast[AnIndex, Cap, MPtr]), ACount, FItemSize.ItemSize);
    if AnIndex < Cnt then begin
      PSource := PByte(ItemPointerFast[AnIndex+ACount, Cap, MPtr]);
      PTarget := PByte(ItemPointerFast[AnIndex, Cap, MPtr]);
      siz := (cnt - AnIndex) * FItemSize.ItemSize;
      m := @MPtr^.Data + CapBytes;
      InternalMoveDown(PSource, PTarget, siz, m);
    end;

  end;

  FMem.Count := Cnt;

  assert((not FMem.IsAllocated) or (FMem.FMem^.FirstItemIdx {%H-}< FCapacityAccessor.ReadCapacity(FMem.FMem)), 'TGenLazRoundList._DeleteRows: (not FMem.IsAllocated) or (FMem.FMem^.FirstItemIdx {%H-}< FCapacityAccessor.ReadCapacity(FMem.FMem))');
end;

function TGenLazRoundList.IndexOf(const AnItem: TPItemT; const AFirstIdx: integer): integer;
var
  p: Pointer;
  c, c2: Integer;
  MPtr: TLazListClassesInternalMem.PMemRecord;
  Cap: Cardinal;
begin
  c := FMem.Count;
  if AFirstIdx >= c then
    exit(-1);

  MPtr := FMem.FMem;
  Cap := FCapacityAccessor.ReadCapacity(MPtr);
  c2 := Cap - MPtr^.FirstItemIdx;

  Result := AFirstIdx;
  if Result < c2 then begin
    p := ItemPointerFast[Result, Cap, MPtr];
    while Result < c2 do begin
      if CompareMem(p, AnItem, FItemSize.ItemSize) then exit;
      inc(Result);
      p := p + FItemSize.ItemSize;
    end;
  end;

  p := ItemPointerFast[Result, Cap, MPtr];
  while Result < c do begin
    if CompareMem(p, AnItem, FItemSize.ItemSize) then exit;
    inc(Result);
    p := p + FItemSize.ItemSize;
  end;

  Result := -1;
end;

procedure TGenLazRoundList.Create;
begin
  FCapacity.Init;
  FInitMem.Init;
  FMem.Init;
end;

procedure TGenLazRoundList.Destroy;
begin
  if @FInitMem.FinalizeMem <> @TLazListAspectMemInitNone.FinalizeMem then
    DeleteRows(0, Count);
  FMem.Free;
end;

procedure TGenLazRoundList.MoveRows(AFromIndex, AToIndex, ACount: Integer);
begin
  if not FIndexChecker.specialize CheckMove<TGenLazRoundList>(Self, AFromIndex, AToIndex, ACount) then exit;
  if AToIndex < AFromIndex then
    MoveRowsDown(AFromIndex, AToIndex, ACount)
  else
    MoveRowsUp(AFromIndex, AToIndex, ACount);
end;

procedure TGenLazRoundList.SwapEntries(AnIndex1, AnIndex2: Integer);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
  t, t1, t2: PByte;
  Cap: Cardinal;
begin
  if not FIndexChecker.specialize CheckSwap<TGenLazRoundList>(Self, AnIndex1, AnIndex2) then exit;

  MPtr := FMem.FMem;
  Cap := FCapacityAccessor.ReadCapacity(MPtr);
  t1 := PByte(GetItemPointerFast(AnIndex1, Cap, MPtr));
  t2 := PByte(GetItemPointerFast(AnIndex2, Cap, MPtr));

  t := Getmem(FItemSize.ItemSize);
  Move(t1^, t^, FItemSize.ItemSize);
  Move(t2^, t1^, FItemSize.ItemSize);
  Move(t^, t2^, FItemSize.ItemSize);
  FreeMem(t);
end;

procedure TGenLazRoundList.DebugDump;
var i , c: integer; s:string;
begin
  if fmem.IsAllocated then begin
    dbgout(['TGenLazRoundList.Dump ', FCapacityAccessor.ReadCapacitySafe(FMem.FMem), ' , ',FMem.Count,
    ' --- ', @fmem.FMem^.Data, ' , ',FMem.FMem^.FirstItemIdx,' --- ', ': ']);
     s :='';
    c := FMem.Count;
    for i := 0 to FCapacityAccessor.ReadCapacitySafe(FMem.FMem) - 1 do begin
      if i = c then s := s + '# ';
      s := s +dbgMemRange(itempointer[i], FItemSize.ItemSize )+ ', ';
    end;
    debugln(s);
  end
  else debugln(['TGenLazRoundList.Dump NONE']);
end;

function TGenLazRoundList.IndexOf(AnItem: TPItemT): integer;
//var
//  p: Pointer;
//  c, c2: Integer;
//  s: Cardinal;
begin
  exit(IndexOf(AnItem, 0));
  ////////////////////
  //c := Count;
  //if c = 0 then
  //  exit(-1);
  //
  //s := FItemSize.ItemSize;
  //c2 := FCapacityAccessor.ReadCapacitySafe(FMem.FMem) - FMem.FMem^.FirstItemIdx;
  //
  //p := ItemPointerFast[0];
  //Result := 0;
  //while Result < c2 do begin
  //  if CompareMem(p, AnItem, s) then exit;
  //  inc(Result);
  //  p := p + FItemSize.ItemSize;
  //end;
  //
  //p := ItemPointerFast[Result];
  //while Result < c do begin
  //  if CompareMem(p, AnItem, s) then exit;
  //  inc(Result);
  //  p := p + FItemSize.ItemSize;
  //end;
  // Result:=-1;
end;

{ TGenLazRoundListVarSize }

procedure TGenLazRoundListVarSize.Create(const AnItemSize: Cardinal);
begin
  FItemSize.ItemSize := AnItemSize;
  inherited Create;
end;

{ TGenLazRoundListFixedType }

function TGenLazRoundListFixedType.Get(const Index: Integer): T;
begin
  Result := ItemPointer[Index]^;
end;

procedure TGenLazRoundListFixedType.Put(const Index: Integer; AValue: T);
begin
  ItemPointerFast[Index]^ := AValue;
end;

function TGenLazRoundListFixedType.IndexOf(AnItem: T): integer;
var
  p: PT;
  c, c2: Integer;
  s: Cardinal;
  MPtr: TLazListClassesInternalMem.PMemRecord;
  Cap: Cardinal;
begin
  c := FMem.Count;
  if c = 0 then
    exit(-1);

  MPtr := FMem.FMem;
  s := FItemSize.ItemSize;
  Cap := FCapacityAccessor.ReadCapacity(MPtr);
  c2 := Cap - MPtr^.FirstItemIdx;

  p := ItemPointerFast[0, Cap, MPtr];
  Result := 0;
  while Result < c2 do begin
    if p^ = AnItem then exit;
    inc(Result);
    p := p + FItemSize.ItemSize;
  end;

  p := ItemPointerFast[Result, Cap, MPtr];
  while Result < c do begin
    if p^ = AnItem then exit;
    inc(Result);
    p := p + FItemSize.ItemSize;
  end;
  Result := -1;
end;

{ __TLazListAspectPageSizeExpConstBase }

procedure __TLazListAspectPageSizeExpConstBase.Init(APageSizeExp: Integer);
begin
  raise Exception.Create('not allowed');
end;

{ __Internal_LazListClassesPgConfConstCapFld.TPageCapacityFieldT }

class function __Internal_LazListClassesPgConfConstCapFld.TCapacityAccessor.ReadCapacity(const AMem: Pointer
  ): Cardinal;
begin
  Result := TData.TPageSizeT.FPageSizeMask+1;
end;

class function __Internal_LazListClassesPgConfConstCapFld.TCapacityAccessor.ReadCapacitySafe(
  const AMem: Pointer): Cardinal;
begin
  Result := TData.TPageSizeT.FPageSizeMask+1;
end;

class procedure __Internal_LazListClassesPgConfConstCapFld.TCapacityAccessor.WriteCapacity(
  const AMem: Pointer; AValue: Cardinal);
begin
  assert((AValue = 0) or (AValue = TData.TPageSizeT.FPageSizeMask+1), '__Internal_LazListClassesPgConfConstCapFld.TCapacityAccessor.WriteCapacity: (AValue = 0) or (AValue = TData.TPageSizeT.FPageSizeMask+1)');
  // nothing
end;

{ TLazFixedRoundBufferListMemBase }

procedure TLazFixedRoundBufferListMemBase.AdjustFirstItemOffset(const ACount: Integer;
  const AMask: Cardinal);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  {$PUSH}{$R-}{$Q-} // may overflow
  MPtr^.FirstItemIdx := (MPtr^.FirstItemIdx + Cardinal(ACount)) and AMask;
  {$POP}
end;

procedure TLazFixedRoundBufferListMemBase.InsertRowsAtStart(const ACount: Integer;
  const AMask: Cardinal);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  {$PUSH}{$R-}{$Q-} // may overflow
  MPtr^.FirstItemIdx := (MPtr^.FirstItemIdx - Cardinal(ACount)) and AMask;
  {$POP}
  Mem.Count := MPtr^.Count + ACount;
end;

procedure TLazFixedRoundBufferListMemBase.InsertRowsAtEnd(const ACount: Integer);
begin
  Mem.Count := Mem.FMem^.Count + ACount;
end;

procedure TLazFixedRoundBufferListMemBase.InsertRowsAtBoundary(const AnAtStart: Boolean;
  const ACount: Integer; const AMask: Cardinal);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  if AnAtStart then begin
    {$PUSH}{$R-}{$Q-} // may overflow
    MPtr^.FirstItemIdx := (MPtr^.FirstItemIdx - Cardinal(ACount)) and AMask;
    {$POP}
  end;
  Mem.Count := MPtr^.Count + ACount;
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRowsAtStart(const ACount: Integer;
  const AMask: Cardinal);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  {$PUSH}{$R-}{$Q-} // may overflow
  MPtr^.FirstItemIdx := (MPtr^.FirstItemIdx + Cardinal(ACount)) and AMask;
  {$POP}
  Mem.Count := MPtr^.Count - ACount;
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRowsAtEnd(const ACount: Integer);
begin
  Mem.Count := Mem.FMem^.Count - ACount;
end;

procedure TLazFixedRoundBufferListMemBase.DeleteRowsAtBoundary(const AnAtStart: Boolean;
  const ACount: Integer; const AMask: Cardinal);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  if AnAtStart then begin
    {$PUSH}{$R-}{$Q-} // may overflow
    MPtr^.FirstItemIdx := (MPtr^.FirstItemIdx + Cardinal(ACount)) and AMask;
    {$POP}
  end;
  Mem.Count := MPtr^.Count - ACount;
end;

procedure TLazFixedRoundBufferListMemBase.MoveRowsToOther(const AFromOffset, AToOffset, ACount,
  ACap: Integer; AnOther: PLazFixedRoundBufferListMemBase);
begin
  assert((ACount <= ACap >> 1) or (ACount = 1), 'TLazFixedRoundBufferListMemBase.MoveRowsToOther: (ACount <= ACap >> 1) or (ACount = 1)');
  assert(ACount > 0, 'TLazFixedRoundBufferListMemBase.MoveRowsToOther: ACount > 0');
  assert(AnOther <> @Self, 'TLazFixedRoundBufferListMemBase.MoveRowsToOther: AnOther <> @Self');
  MoveBytesToOther(
    ((AFromOffset + Mem.FMem^.FirstItemIdx) and (ACap-1)) * FItemSize.ItemSize,
    ((AToOffset + AnOther^.Mem.FMem^.FirstItemIdx) and (ACap-1)) * FItemSize.ItemSize,
    ACount * FItemSize.ItemSize,
    ACap * FItemSize.ItemSize,
    AnOther);
end;

procedure TLazFixedRoundBufferListMemBase.MoveBytesToOther(AFromByteOffset, AToByteOffset,
  AByteCount, AByteCap: Integer; AnOther: PLazFixedRoundBufferListMemBase);
var
  CSrc, CDst: Integer;
  PSource, PTarget, SrcHigh, DstHigh: PByte;
  MPtr, OtherMPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  OtherMPtr := AnOther^.FMem.FMem;
  PSource := @MPtr^.Data;
  SrcHigh := PSource + AByteCap;
  PSource := PSource + AFromByteOffset;
  assert(PSource < SrcHigh, 'TLazFixedRoundBufferListMemBase.MoveBytesToOther: PSource < SrcHigh');
  //if PSource >= SrcHigh then PSource := PSource - AByteCap;

  PTarget := @OtherMPtr^.Data;
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
    PTarget := @OtherMPtr^.Data;
    PSource := PSource + CDst;

    CSrc := Min(SrcHigh - PSource, AByteCount);
    Move(PSource^, PTarget^, CSrc);
    AByteCount := AByteCount - CSrc;
    if AByteCount = 0 then exit;
    PSource := @MPtr^.Data;
    PTarget := PTarget + CSrc;
    Move(PSource^, PTarget^, AByteCount);
  end
  else if CSrc = CDst then begin
    CSrc := Min(CSrc, AByteCount);
    Move(PSource^, PTarget^, CSrc);
    AByteCount := AByteCount - CSrc;
    if AByteCount = 0 then exit;
    PSource := @MPtr^.Data;
    PTarget := @OtherMPtr^.Data;
    Move(PSource^, PTarget^, AByteCount);
  end
  else begin
    CSrc := Min(CSrc, AByteCount);
    Move(PSource^, PTarget^, CSrc);
    AByteCount := AByteCount - CSrc;
    if AByteCount = 0 then exit;
    PSource := @MPtr^.Data;
    PTarget := PTarget + CSrc;

    CDst := Min(DstHigh - PTarget, AByteCount);
    Move(PSource^, PTarget^, CDst);
    AByteCount := AByteCount - CDst;
    if AByteCount = 0 then exit;
    PTarget := @OtherMPtr^.Data;
    PSource := PSource + CDst;
    Move(PSource^, PTarget^, AByteCount);
  end;
end;

function TLazFixedRoundBufferListMemBase.GetItemPointerMasked(const AnIndex, AMask: PtrUInt
  ): TPItemT;
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  Result := TPItemT(
//    @MPtr^.Data + ((AnIndex + PtrUInt(MPtr^.FirstItemIdx)) and AMask) * PtrUInt(FItemSize.ItemSize)
    @MPtr^.Data + ((AnIndex + MPtr^.FirstItemIdx) and AMask) * FItemSize.ItemSize
  );
end;

function TLazFixedRoundBufferListMemBase.GetItemByteOffsetMasked(const AnIndex, AMask: PtrUInt
  ): Integer;
begin
  Result := ((AnIndex + Mem.FMem^.FirstItemIdx) and AMask) * FItemSize.ItemSize;
end;

function TLazFixedRoundBufferListMemBase.GetFirstItemByteOffset: Integer;
begin
  Result := Mem.FMem^.FirstItemIdx * FItemSize.ItemSize;
end;

procedure TLazFixedRoundBufferListMemBase.Create(const AItemSize: _TSizeT; ACapacity: Integer);
begin
  inherited Create;
  FItemSize := AItemSize;
  SetCapacity(ACapacity);
end;

{ TGenLazPagedList.TPageType }

procedure TGenLazPagedList.TPageType.InitMem(const AnInitMem: TOuter_TInitMemT;
  const AnItemSize, ACap: Cardinal);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  AnInitMem.InitMem(@MPtr^.Data, ACap, AnItemSize);
end;

procedure TGenLazPagedList.TPageType.InitMem(const AnInitMem: TOuter_TInitMemT; const AnIndex,
  AnItemCount: integer; const AnItemSize, ACap: Cardinal);
var
  c, Idx: Integer;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  c := ACap;
  assert(Cardinal(AnIndex) <= c, 'TGenLazPagedList.TPageType.InitMem: Cardinal(AnIndex) <= c');
  Idx := MPtr^.FirstItemIdx + AnIndex;
  c := c - Idx;
  if c > 0 then begin
    if c >= AnItemCount then begin
      AnInitMem.InitMem(@MPtr^.Data + Idx * AnItemSize, AnItemCount, AnItemSize);
      exit;
    end;
    AnInitMem.InitMem(@MPtr^.Data + Idx * AnItemSize, c, AnItemSize);
    if AnItemCount > c then
      AnInitMem.InitMem(@MPtr^.Data, AnItemCount - c, AnItemSize);
  end
  else begin
    AnInitMem.InitMem(@MPtr^.Data - c * AnItemSize, AnItemCount, AnItemSize);
  end;
end;

procedure TGenLazPagedList.TPageType.FinalizeMem(const AnInitMem: TOuter_TInitMemT;
  const AnItemSize, ACap: Cardinal);
var
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  AnInitMem.FinalizeMem(@MPtr^.Data, ACap, AnItemSize);
end;

procedure TGenLazPagedList.TPageType.FinalizeMem(const AnInitMem: TOuter_TInitMemT; const AnIndex,
  AnItemCount: integer; const AnItemSize, ACap: Cardinal);
var
  c, Idx: Integer;
  MPtr: TLazListClassesInternalMem.PMemRecord;
begin
  MPtr := FMem.FMem;
  c := ACap;
  assert(Cardinal(AnIndex) <= c, 'TGenLazPagedList.TPageType.FinalizeMem: Cardinal(AnIndex) <= c');
  Idx := MPtr^.FirstItemIdx + AnIndex;
  c := c - Idx;
  if c > 0 then begin
    if c >= AnItemCount then begin
      AnInitMem.FinalizeMem(@MPtr^.Data + Idx * AnItemSize, AnItemCount, AnItemSize);
      exit;
    end;
    AnInitMem.FinalizeMem(@MPtr^.Data + Idx * AnItemSize, c, AnItemSize);
    if AnItemCount > c then
      AnInitMem.FinalizeMem(@MPtr^.Data, AnItemCount - c, AnItemSize);
  end
  else begin
    AnInitMem.FinalizeMem(@MPtr^.Data - c * AnItemSize, AnItemCount, AnItemSize);
  end;
end;

{ TGenLazPagedList.TPageCapacityController }

procedure TGenLazPagedList.TPageCapacityController.Init();
begin
  FExtraCapacityNeeded := 0;
  inherited Init;
end;

function TGenLazPagedList.TPageCapacityController.GrowCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  Result := inherited GrowCapacity(ARequired + FExtraCapacityNeeded, ACurrent);
  FExtraCapacityNeeded := 0;
end;

{ TGenLazPagedListVarSize }

procedure TGenLazPagedListVarSize.Create(const AnItemSize: Cardinal);
begin
  FItemSize.ItemSize := AnItemSize;
  inherited Create;
end;

procedure TGenLazPagedListVarSize.Create(const APageSizeExp: Integer; const AnItemSize: Cardinal);
begin
  FItemSize.ItemSize := AnItemSize;
  inherited Create(APageSizeExp);
end;

{ TGenLazPagedList }

function TGenLazPagedList.GetPagePointer(const PageIndex: Integer): PPageType;
begin
  assert((PageIndex >= 0) and (PageIndex < FPages.Count), 'TGenLazPagedList.GetPagePointer: (PageIndex >= 0) and (PageIndex < FPages.Count)');
  Result := FPages.ItemPointerFast[PageIndex];
end;

function TGenLazPagedList.GetItemSize: Cardinal;
begin
  Result := FItemSize.ItemSize;
end;

function TGenLazPagedList.GetItemPointer(const Index: Integer): TPItemT;
var
  p: PPageType;
  i: ptruint;
begin
  if not FIndexChecker.specialize CheckIndex<TGenLazPagedList>(Self, Index, Result) then exit;

  assert((Index>=0) and (Index<FCount), 'TGenLazPagedList.GetItemPointer: (Index>=0) and (Index<FCount)');
  i := Cardinal(Index) + FFirstPageEmpty;
  p := FPages.ItemPointerFast[i >> FPageSize.FPageSizeExp];
  assert(p<>nil, 'TGenLazPagedList.GetItemPointer: p<>nil');
  Result := p^.ItemPointerMasked[i, FPageSize.FPageSizeMask];
end;

procedure TGenLazPagedList.SetCapacity(const AValue: Integer);
var
  V: integer;
begin
  if AValue <= 0 then begin
    FPages.Capacity := 0;
    exit;
  end;
  V := (AValue + FPageSize.FPageSizeMask) >> FPageSize.FPageSizeExp;
  if V <= FPages.Count then
    exit;
  FPages.Capacity := V;
end;

function TGenLazPagedList.GetCapacity: Integer;
begin
  Result := FPages.Capacity << FPageSize.FPageSizeExp;
end;

function TGenLazPagedList.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TGenLazPagedList.JoinPageWithNext(const APageIdx, AJoinEntryIdx, AnExtraDelPages: Integer
  );
var
  PCap: Cardinal;
begin
  // delete last page(s), if AJoinEntryIdx=0 // next page does not need to exist
  PCap := FPageSize.FPageSizeMask + 1;
  if AJoinEntryIdx * 2 <= FPageSize.FPageSizeMask then begin
    if AJoinEntryIdx > 0 then
      FPages.ItemPointerFast[APageIdx]^.MoveRowsToOther(0, 0,
        AJoinEntryIdx, PCap, FPages.ItemPointerFast[APageIdx + 1 + AnExtraDelPages]);
    DeletePages(APageIdx, 1 + AnExtraDelPages);
  end
  else begin
    FPages.ItemPointerFast[APageIdx + 1 + AnExtraDelPages]^.MoveRowsToOther(AJoinEntryIdx, AJoinEntryIdx,
      PCap - AJoinEntryIdx, PCap, FPages.ItemPointerFast[APageIdx]);
    DeletePages(APageIdx + 1, 1 + AnExtraDelPages);
  end;
end;

procedure TGenLazPagedList.SplitPageToFront(const ASourcePageIdx, ASplitAtIdx,
  AnExtraPages: Integer; const AExtraCapacityNeeded: Integer);
begin
  // Can split the none-existing page[pagecount], IF ASplitAtIdx=0 // simply insert a page
  InsertFilledPages(ASourcePageIdx, AnExtraPages+1, AExtraCapacityNeeded);
  if ASplitAtIdx > 0 then
    FPages.ItemPointerFast[ASourcePageIdx + AnExtraPages + 1]^.MoveRowsToOther(0, 0, ASplitAtIdx, FPageSize.FPageSizeMask+1, FPages.ItemPointerFast[ASourcePageIdx]);
end;

procedure TGenLazPagedList.SplitPageToBack(const ASourcePageIdx, ASplitAtIdx,
  AnExtraPages: Integer; const AExtraCapacityNeeded: Integer);
var
  c: Integer;
begin
  InsertFilledPages(ASourcePageIdx+1, AnExtraPages+1, AExtraCapacityNeeded);
  c := FPageSize.FPageSizeMask + 1 - ASplitAtIdx;
  if c > 0 then
    FPages.ItemPointerFast[ASourcePageIdx]^.MoveRowsToOther(ASplitAtIdx, ASplitAtIdx, c, FPageSize.FPageSizeMask+1, FPages.ItemPointerFast[ASourcePageIdx + AnExtraPages + 1]);
end;

procedure TGenLazPagedList.SplitPage(const ASourcePageIdx, ASplitAtIdx, AnExtraPages: Integer;
  const AExtraCapacityNeeded: Integer);
begin
  if ASplitAtIdx <= FPageSize.FPageSizeMask >> 1 then
    SplitPageToFront(ASourcePageIdx, ASplitAtIdx, AnExtraPages, AExtraCapacityNeeded)
  else
    SplitPageToBack(ASourcePageIdx, ASplitAtIdx, AnExtraPages, AExtraCapacityNeeded);
end;

procedure TGenLazPagedList._DoInitMem(const AnIndex, ACount: Integer; _ItemSize, _PageSize: Cardinal);
var
  i, Idx, p1s, p2, p2s: Integer;
  TmpPagePtr: PPageType;
begin
  i := AnIndex + FFirstPageEmpty;
  Idx := i >> FPageSize.FPageSizeExp;
  p1s := i and (_PageSize-1);
  i := i + ACount -1;
  p2  := i >> FPageSize.FPageSizeExp;
  p2s := (i and (_PageSize-1)) + 1;

  TmpPagePtr := FPages.ItemPointerFast[Idx];
  if Idx = p2 then begin
    TmpPagePtr^.InitMem(FInitMem, p1s, p2s - p1s, _ItemSize, _PageSize);
  end
  else begin
    TmpPagePtr^.InitMem(FInitMem, p1s, _PageSize-p1s, _ItemSize, _PageSize);
    inc(TmpPagePtr);
    for i := 0 to p2 - Idx - 2 do begin
      TmpPagePtr^.InitMem(FInitMem, _ItemSize, _PageSize);
      inc(TmpPagePtr);
    end;
    TmpPagePtr^.InitMem(FInitMem, 0, p2s, _ItemSize, _PageSize);
  end;
end;

procedure TGenLazPagedList.DoInitMem(const AnIndex, ACount: Integer);
begin
  _DoInitMem(AnIndex, ACount, FItemSize.ItemSize, FPageSize.FPageSizeMask+1);
end;

procedure TGenLazPagedList.DoFinalizeMem(const AnIndex, ACount: Integer);
var
  i, Idx, p1s, p2, p2s: Integer;
  TmpPagePtr: PPageType;
begin
  i := AnIndex + FFirstPageEmpty;
  Idx := i >> FPageSize.FPageSizeExp;
  p1s := i and FPageSize.FPageSizeMask;
  i := i + ACount -1;
  p2  := i >> FPageSize.FPageSizeExp;
  p2s := (i and FPageSize.FPageSizeMask) + 1;

  TmpPagePtr := FPages.ItemPointerFast[Idx];
  if Idx = p2 then begin
    TmpPagePtr^.FinalizeMem(FInitMem, p1s, p2s - p1s, FItemSize.ItemSize, FPageSize.FPageSizeMask+1);
  end
  else begin
    TmpPagePtr^.FinalizeMem(FInitMem, p1s, FPageSize.FPageSizeMask+1-p1s, FItemSize.ItemSize, FPageSize.FPageSizeMask+1);
    inc(TmpPagePtr);
    for i := 0 to p2 - Idx - 2 do begin
      TmpPagePtr^.FinalizeMem(FInitMem, FItemSize.ItemSize, FPageSize.FPageSizeMask+1);
      inc(TmpPagePtr);
    end;
    TmpPagePtr^.FinalizeMem(FInitMem, 0, p2s, FItemSize.ItemSize, FPageSize.FPageSizeMask+1);
  end;
end;

procedure TGenLazPagedList.InternalBubbleEntriesDown(const ASourceStartIdx, ATargetEndIdx,
  AnEntryCount: Integer);
var
  CurPage, NextPage: PPageType;
  i, PageCapacity, PageCapacityBytes, CountBytes, M: Integer;
begin
  assert(ASourceStartIdx > ATargetEndIdx, 'TGenLazPagedList.InternalBubbleEntriesDown: ASourceStartIdx > ATargetEndIdx');

  M := FPageSize.FPageSizeMask;
  PageCapacity := (M+1);
  PageCapacityBytes := PageCapacity * FItemSize.ItemSize;
  CountBytes := AnEntryCount * FItemSize.ItemSize;

  CurPage := FPages.ItemPointerFast[ATargetEndIdx];
  NextPage := CurPage + 1;

  NextPage^.MoveBytesToOther(NextPage^.GetFirstItemByteOffset,
    CurPage^.GetItemByteOffsetMasked(M+1 - AnEntryCount, M),
    CountBytes, PageCapacityBytes, CurPage);

  for i := 0 to ASourceStartIdx - ATargetEndIdx - 2 do begin
    NextPage^.AdjustFirstItemOffset(AnEntryCount, M);
    CurPage := NextPage;
    NextPage := CurPage + 1;

    NextPage^.MoveBytesToOther(NextPage^.GetFirstItemByteOffset,
      CurPage^.GetItemByteOffsetMasked(PageCapacity - AnEntryCount, M),
      CountBytes, PageCapacityBytes, CurPage);
  end;
end;

procedure TGenLazPagedList.InternalBubbleEntriesUp(const ASourceStartIdx, ATargetEndIdx,
  AnEntryCount: Integer);
var
  CurPage, NextPage: PPageType;
  i, PageCapacity, PageCapacityBytes, CountBytes, M: Integer;
begin
  assert(ASourceStartIdx < ATargetEndIdx, 'TGenLazPagedList.InternalBubbleEntriesUp: ASourceStartIdx < ATargetEndIdx');

  M := FPageSize.FPageSizeMask;
  PageCapacity := (M+1);
  PageCapacityBytes := PageCapacity * FItemSize.ItemSize;
  CountBytes := AnEntryCount * FItemSize.ItemSize;

  CurPage := FPages.ItemPointerFast[ATargetEndIdx];
  for i := 0 to ATargetEndIdx - ASourceStartIdx - 2 do begin
    NextPage := CurPage - 1;

    NextPage^.MoveBytesToOther(NextPage^.GetItemByteOffsetMasked(PageCapacity - AnEntryCount, M),
      CurPage^.GetFirstItemByteOffset,
      CountBytes, PageCapacityBytes, CurPage);
    NextPage^.AdjustFirstItemOffset(-AnEntryCount, M);
    CurPage := NextPage;
  end;

  NextPage := CurPage - 1;
  NextPage^.MoveBytesToOther(NextPage^.GetItemByteOffsetMasked(M+1 - AnEntryCount, M),
    CurPage^.GetFirstItemByteOffset,
    CountBytes, PageCapacityBytes, CurPage);
end;

procedure TGenLazPagedList.MovePagesUp(const ASourceStartIndex, ATargetStartIndex,
  ATargetEndIndex: Integer);
var
  Cnt, Diff, MoveSize: Integer;
  TempPages: Array of TPageType;
begin
  Cnt := ATargetEndIndex - ATargetStartIndex + 1;
  Diff := ATargetStartIndex - ASourceStartIndex;
  assert(Diff > 0, 'TGenLazPagedList.MovePagesUp: Diff > 0');

  if Diff >= Cnt then begin
    SetLength(TempPages{%H-}, Cnt);
    MoveSize := Cnt * SizeOf(TempPages[0]);
    move(FPages.ItemPointerFast[ATargetStartIndex]^, TempPages[0], MoveSize);
    FPages.MoveRows(ASourceStartIndex, ATargetStartIndex, Cnt);
    move(TempPages[0], FPages.ItemPointerFast[ASourceStartIndex]^, MoveSize);
  end else begin
    SetLength(TempPages{%H-}, Diff);
    MoveSize := Diff * SizeOf(TempPages[0]);
    move(FPages.ItemPointerFast[ASourceStartIndex+Cnt]^, TempPages[0], MoveSize);
    FPages.MoveRows(ASourceStartIndex, ATargetStartIndex, Cnt);
    move(TempPages[0], FPages.ItemPointerFast[ASourceStartIndex]^, MoveSize);
  end;
end;

procedure TGenLazPagedList.MovePagesDown(const ASourceStartIndex, ATargetStartIndex,
  ATargetEndIndex: Integer);
var
  Cnt, Diff, MoveSize: Integer;
  TempPages: Array of TPageType;
begin
  Cnt := ATargetEndIndex - ATargetStartIndex + 1;
  Diff := ASourceStartIndex - ATargetStartIndex;
  assert(Diff > 0, 'TGenLazPagedList.MovePagesDown: Diff > 0');

  if Diff >= Cnt then begin
    SetLength(TempPages{%H-}, Cnt);
    MoveSize := Cnt * SizeOf(TempPages[0]);
    move(FPages.ItemPointerFast[ATargetStartIndex]^, TempPages[0], MoveSize);
    FPages.MoveRows(ASourceStartIndex, ATargetStartIndex, Cnt);
    move(TempPages[0], FPages.ItemPointerFast[ASourceStartIndex]^, MoveSize);
  end else begin
    SetLength(TempPages{%H-}, Diff);
    MoveSize := Diff * SizeOf(TempPages[0]);
    move(FPages.ItemPointerFast[ATargetStartIndex]^, TempPages[0], MoveSize);
    FPages.MoveRows(ASourceStartIndex, ATargetStartIndex, Cnt);
    move(TempPages[0], FPages.ItemPointerFast[ATargetStartIndex+Cnt]^, MoveSize);
  end;
end;

procedure TGenLazPagedList.InsertFilledPages(const AnIndex, ACount: Integer;
  const AExtraCapacityNeeded: Integer);
var
  i, c, h: Integer;
  p: PPageType;
begin
  FPages.FCapacity.FExtraCapacityNeeded := AExtraCapacityNeeded;
  p := FPages.InsertRows(AnIndex, ACount);
  FPages.FCapacity.FExtraCapacityNeeded := 0;
  c := FPageSize.FPageSizeMask + 1;
  h := AnIndex + ACount - 1;
  for i := AnIndex to h do begin
    p^.Create(FItemSize, c);
    p^.InsertRowsAtEnd(c);
    FInitMem.InitMemOnce(@p^.Mem.FMem^.Data, c, FItemSize.ItemSize);
    inc(p);
  end;
end;

procedure TGenLazPagedList.DeletePages(const AnIndex, ACount: Integer);
var
  i: Integer;
  p: PPageType;
begin
  p := FPages.ItemPointerFast[AnIndex]; // pages are NOT a roundbuffer
  for i := AnIndex to AnIndex + ACount - 1 do begin
    //FPages.ItemPointerFast[i]^.Destroy;
    p^.Destroy;
    inc(p);
  end;
  FPages.DeleteRows(AnIndex, ACount);
end;

procedure TGenLazPagedList.MoveRowsQuick(const AFromIndex, AToIndex, ACount: Integer);
begin
  if AFromIndex < AToIndex then
    InternalMoveRowsUp(AFromIndex, AToIndex, ACount)
  else
    InternalMoveRowsDown(AFromIndex, AToIndex, ACount);
end;

procedure TGenLazPagedList.Create;
begin
  FInitMem.Init;

  FCount := 0;
  FFirstPageEmpty := 0;
  FPages.Create;
end;

procedure TGenLazPagedList.Create(const APageSizeExp: Integer);
begin
  FPageSize.Init(APageSizeExp);
  Create;
end;

procedure TGenLazPagedList.Destroy;
begin
  if FCount > 0 then begin
    if @FInitMem.FinalizeMem <> @TLazListAspectMemInitNone.FinalizeMem then
      DoFinalizeMem(0, FCount);
    DeletePages(0, FPages.Count);
  end;
  FPages.Destroy;
end;

procedure TGenLazPagedList._InsertRows(const AnIndex, ACount, APageSizeMask, APageSizeExp: Integer);
var
  InitIndex, InitCount: integer;
  ExtraPagesNeeded, SubIndex, SubCount, NewFirstPageEmpty: Integer;
  PgCnt, PCap, InsertPageIdx, c, i,
    AIndexAdj, TmpDeleteRows, LastPageEmpty: Integer;
begin
  InitIndex := AnIndex;
  InitCount := ACount;
  assert((AnIndex >= 0) and (AnIndex <= FCount), 'TGenLazPagedList._InsertRows: (AnIndex >= 0) and (AnIndex <= FCount)');
  if ACount <= 0 then
    exit;
  PCap := APageSizeMask + 1;
  PgCnt := FPages.Count;
  FCount := FCount + ACount;
  SubCount := ACount and APageSizeMask;

//DebugLn();debugln(['***### TGenLazPagedList.InsertRows Idx:',AnIndex,' cnt:',ACount, ' Pcnt:',PgCnt, ' fcnt:', FCount, '  FFirstPageEmpty:', FFirstPageEmpty]);DebugDump;
  if PgCnt = 0 then begin
    // No data yet
    ExtraPagesNeeded := ((ACount-1) >> APageSizeExp) + 1;
    InsertFilledPages(0, ExtraPagesNeeded);
    NewFirstPageEmpty := (PCap - SubCount) and APageSizeMask;
    FFirstPageEmpty := NewFirstPageEmpty div 2; // keep some capacity in the last node too
  assert((((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)<=APageSizeMask) and (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TGenLazPagedList._InsertRows: (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)<=APageSizeMask) and (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)>=0)');
    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then
      DoInitMem(InitIndex, InitCount);
    exit;
  end
  else if (FCount <= PCap) and (PgCnt = 1) then begin
    // keep it in one page
    NewFirstPageEmpty := FFirstPageEmpty - SubCount;
    if NewFirstPageEmpty < 0 then begin
      FPages.ItemPointerFast[0]^.AdjustFirstItemOffset(NewFirstPageEmpty, APageSizeMask);
      NewFirstPageEmpty := 0;
    end;
    FFirstPageEmpty := NewFirstPageEmpty;
    if AnIndex > 0 then
      InternalMoveRowsDown(SubCount, 0, AnIndex);
  assert((((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)<=APageSizeMask) and (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TGenLazPagedList._InsertRows: (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)<=APageSizeMask) and (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)>=0)');
    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then
      DoInitMem(InitIndex, InitCount);
    exit;
  end;

  AIndexAdj := AnIndex + FFirstPageEmpty;
  InsertPageIdx := (AIndexAdj) >> APageSizeExp;
  SubIndex := AIndexAdj and APageSizeMask;

  ExtraPagesNeeded := ACount - SubCount;
  if (ExtraPagesNeeded > 0) then
    ExtraPagesNeeded := ExtraPagesNeeded >> APageSizeExp;

  If Cardinal(InsertPageIdx) * 2 <= Cardinal(PgCnt) then begin
    if SubCount * 2 <= PCap then begin
      if SubCount > 0 then begin
        NewFirstPageEmpty := FFirstPageEmpty - SubCount;
        if NewFirstPageEmpty < 0 then begin
          InsertFilledPages(0, 1);
          NewFirstPageEmpty := NewFirstPageEmpty + PCap;
          inc(InsertPageIdx);
          assert(NewFirstPageEmpty>0, 'TGenLazPagedList._InsertRows: NewFirstPageEmpty>0');
        end;
        FFirstPageEmpty := NewFirstPageEmpty;
        if AnIndex > 0 then
          InternalMoveRowsDown(SubCount, 0, AnIndex);
      end;
      if ExtraPagesNeeded > 0 then
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded - 1);
    end
    else begin
      SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded);
      TmpDeleteRows := PCap - SubCount;
      assert(TmpDeleteRows>0, 'TGenLazPagedList._InsertRows: TmpDeleteRows>0');
      if AnIndex > 0 then
        InternalMoveRowsUp(0, TmpDeleteRows, AnIndex);
      NewFirstPageEmpty := FFirstPageEmpty + TmpDeleteRows;
      if NewFirstPageEmpty > PCap then begin
        NewFirstPageEmpty := NewFirstPageEmpty - PCap;
        DeletePages(0, 1);
      end;
      FFirstPageEmpty := NewFirstPageEmpty;
    end;
  end
  else begin
    c := FCount - ACount;
    LastPageEmpty := (PCap - (c + FFirstPageEmpty)) and APageSizeMask;
    assert(LastPageEmpty >= 0, 'TGenLazPagedList._InsertRows: LastPageEmpty >= 0');
    if SubCount * 2 <= PCap then begin
      if SubCount > 0 then begin
        if LastPageEmpty < SubCount then
          InsertFilledPages(PgCnt, 1);
        if AnIndex < c then begin
          InternalMoveRowsUp(AnIndex, AnIndex + SubCount, c - AnIndex);
        end;
      end;
      if ExtraPagesNeeded > 0 then
        SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded - 1);
    end
    else begin
      TmpDeleteRows := PCap - SubCount;
      SplitPage(InsertPageIdx, SubIndex, ExtraPagesNeeded);
      assert(TmpDeleteRows>0, 'TGenLazPagedList._InsertRows: TmpDeleteRows>0');
      if AnIndex < c then begin
        c := FCount + TmpDeleteRows;
        i := AnIndex + ((ExtraPagesNeeded + 1) << APageSizeExp);
        InternalMoveRowsDown(i, i - TmpDeleteRows, c - i);
      end;
      if LastPageEmpty + TmpDeleteRows >= PCap then
        DeletePages(FPages.Count-1, 1);
    end;
  end;
//debugln('<<< INS done'); DebugDump;
  assert((((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)<=APageSizeMask) and (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TGenLazPagedList._InsertRows: (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)<=APageSizeMask) and (((FPages.Count << APageSizeExp)-FFirstPageEmpty-FCount)>=0)');
  if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then
    DoInitMem(InitIndex, InitCount);
end;

procedure TGenLazPagedList.InsertRows(AnIndex, ACount: Integer);
var dummy: pointer;
begin
  if not FIndexChecker.specialize CheckInsert<TGenLazPagedList>(Self, AnIndex, dummy) then exit;
  _InsertRows(AnIndex, ACount, FPageSize.FPageSizeMask, FPageSize.FPageSizeExp);
end;

procedure TGenLazPagedList.DeleteRows(AnIndex, ACount: Integer);
var
  PCap, c, i: Integer;
  DelPageIdx, SubIndex, AIndexAdj, SubCount, ExtraPagesToDelete, LastPageUsed, TmpInsertRows: Integer;
  FrstPgE: Cardinal;
  NewFirstPageEmpty: Integer;
begin
  if not FIndexChecker.specialize CheckDelete<TGenLazPagedList>(Self, AnIndex, ACount) then exit;

//  debugln(['<<<<<<< TGenLazPagedList.DeleteRows ', AnIndex, ', ', ACount]); DebugDump;
  assert((AnIndex >= 0) and (AnIndex + ACount <= FCount), 'TGenLazPagedList.DeleteRows: (AnIndex >= 0) and (AnIndex + ACount <= FCount)');
  if ACount <= 0 then
    exit;

  if @TInitMemT.FinalizeMem <> @TLazListAspectMemInitNone.FinalizeMem then
    DoFinalizeMem(AnIndex, ACount);

  PCap := FPageSize.FPageSizeMask + 1;
  FCount := FCount - ACount;

  FrstPgE := FFirstPageEmpty;
  AIndexAdj := AnIndex + FrstPgE;
  DelPageIdx:= (AIndexAdj) >> FPageSize.FPageSizeExp;
  SubIndex := AIndexAdj and FPageSize.FPageSizeMask;

  SubCount := ACount and FPageSize.FPageSizeMask;
  ExtraPagesToDelete := ACount - SubCount;
  if (ExtraPagesToDelete > 0) then
    ExtraPagesToDelete := ExtraPagesToDelete >> FPageSize.FPageSizeExp;

  If Cardinal(DelPageIdx) * 2 < Cardinal(FPages.Count) then begin
    if (SubCount * 2 <= PCap) or (FPages.Count = 1) then begin
      if ExtraPagesToDelete > 0 then
        JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesToDelete - 1);
      if (SubCount > 0) then begin
        if (AnIndex > 0) then
          InternalMoveRowsUp(0, SubCount, AnIndex);
        LastPageUsed := PCap - FrstPgE;
        if LastPageUsed > SubCount then begin
          FFirstPageEmpty := FrstPgE + SubCount;
        end
        else begin
          DeletePages(0, 1);
          FFirstPageEmpty := SubCount - LastPageUsed;
        end;
        assert(FFirstPageEmpty<PCap, 'TGenLazPagedList.DeleteRows: FFirstPageEmpty<PCap');
      end;
    end
    else begin
      TmpInsertRows := PCap - SubCount;
      assert(TmpInsertRows>0, 'TGenLazPagedList.DeleteRows: TmpInsertRows>0');
      NewFirstPageEmpty := FrstPgE - TmpInsertRows;
      if NewFirstPageEmpty < 0 then begin
        InsertFilledPages(0, 1); // TODO: what if this needs capacity??
        NewFirstPageEmpty := NewFirstPageEmpty + PCap;
        inc(DelPageIdx);
      end;
      FFirstPageEmpty := NewFirstPageEmpty;
      if (AnIndex > 0) then
        InternalMoveRowsDown(TmpInsertRows, 0, AnIndex);
      SubIndex := SubIndex - TmpInsertRows;
      if SubIndex < 0 then begin
        SubIndex := SubIndex + PCap;
        DelPageIdx := DelPageIdx - 1;
      end;
      JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesToDelete);
    end;
  end
  else begin
    c := FCount + ACount;
    LastPageUsed := (c + FrstPgE) and FPageSize.FPageSizeMask;
    if LastPageUsed = 0 then LastPageUsed := PCap;
    if SubCount * 2 <= PCap then begin
      if ExtraPagesToDelete > 0 then begin
        JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesToDelete - 1);
        c := c - (ExtraPagesToDelete << FPageSize.FPageSizeExp);
      end;
      if (SubCount > 0) then begin
        if (c - AnIndex - SubCount > 0) then
          InternalMoveRowsDown(AnIndex + SubCount, AnIndex, c - AnIndex - SubCount);
        if LastPageUsed <= SubCount then
          DeletePages(FPages.Count - 1, 1);
      end;
    end
    else begin
      TmpInsertRows := PCap - SubCount;
      assert(TmpInsertRows>0, 'TGenLazPagedList.DeleteRows: TmpInsertRows>0');
      if LastPageUsed + TmpInsertRows > PCap then
        InsertFilledPages(FPages.Count, 1); // TODO: what if this needs capacity??
      i := AnIndex + SubCount;
      if (i < c) then
        InternalMoveRowsUp(i, i + TmpInsertRows, c - i);
      JoinPageWithNext(DelPageIdx, SubIndex, ExtraPagesToDelete);
    end;
  end;
//debugln([' DEL DONE <<<<<<<<< ']);DebugDump;
  if (FCount = 0) and (FPages.Count > 0) then
    DeletePages(0, 1);
assert((FPages.Count =0) or (FCount>0), 'TGenLazPagedList.DeleteRows: (FPages.Count =0) or (FCount>0)');
assert((FPages.Count=0)or(((FPages.Count << FPageSize.FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSize.FPageSizeMask) and (((FPages.Count << FPageSize.FPageSizeExp)-FFirstPageEmpty-FCount)>=0), 'TGenLazPagedList.DeleteRows: (FPages.Count=0)or(((FPages.Count << FPageSize.FPageSizeExp)-FFirstPageEmpty-FCount)<=FPageSize.FPageSizeMask) and (((FPages.Count << FPageSize.FPageSizeExp)-FFirstPageEmpty-FCount)>=0)');
end;

procedure TGenLazPagedList.InternalMoveRowsDown(AFromIndex, AToIndex, ACount: Integer);
var
  PageCapacity, PageHalfCap: integer;
  SourceStartPage, SourceStartSubIndex, SourceEndPage, SourceEndSubIndex: Integer;
  TargetStartPage, TargetStartSubIndex, TargetEndPage, TargetEndSubIndex: Integer;
  i, j, c, c2: integer;
  BubbleDiff, RevBubbleDiff: integer;
  SourceAfterEndSubIndex, FirstMovePage, LastMovePage: Integer;
  MovePgCnt, MoveFromPage, MoveToPage, MoveFromFirstEmptyPage: integer;
  RotateFirst, RotateLast,
  FirstPageNotMovedRows,
  BubbleFirstCnt: Integer;
  TrgEndPagePtr, MovLastPagePtr, MovFrmPagePtr, MovToPagePtr, TmpPagePtr,
    SrcStartPagePtr, TrgStartPagePtr: PPageType;
  LastPageDone: Boolean;
begin
  PageCapacity := FPageSize.FPageSizeMask+1;
  PageHalfCap  := PageCapacity >> 1;

  BubbleDiff := (AFromIndex - AToIndex) and FPageSize.FPageSizeMask;

  (* Calculate FROM page, and index in page *)
  AFromIndex := AFromIndex + FFirstPageEmpty;
  SourceStartPage       := AFromIndex >> FPageSize.FPageSizeExp;
  SourceStartSubIndex   := (AFromIndex and FPageSize.FPageSizeMask);

  AFromIndex := AFromIndex + aCount - 1;
  SourceEndPage       := AFromIndex >> FPageSize.FPageSizeExp;
  SourceEndSubIndex   := (AFromIndex and FPageSize.FPageSizeMask) + 1; // NEXT

  (* Calculate TO page, and index in page *)
  AToIndex := AToIndex + FFirstPageEmpty;
  TargetStartPage     := AToIndex >> FPageSize.FPageSizeExp;
  TargetStartSubIndex := AToIndex and FPageSize.FPageSizeMask;

  AToIndex := AToIndex + aCount - 1;
  TargetEndPage     := AToIndex >> FPageSize.FPageSizeExp;
  TargetEndSubIndex := (AToIndex and FPageSize.FPageSizeMask) + 1; // NEXT


  //if ACount <= 3*PageHalfCap then begin
  //if ACount <= 2*PageCapacity then begin
  i := PageCapacity - max(SourceStartSubIndex, TargetStartSubIndex);
  if i > PageHalfCap then i := 0;
  c := min(SourceEndSubIndex, TargetEndSubIndex);
  if c > PageHalfCap then c := 0;
  if ACount - i - c <= PageHalfCap then begin
    (* **************************** *
     *    Max 2 source pages        *
     *    No full page moves needed *
     * **************************** *)
    MovFrmPagePtr := FPages.ItemPointerFast[SourceStartPage];
    MovToPagePtr := FPages.ItemPointerFast[TargetStartPage];

    if (TargetStartSubIndex < SourceStartSubIndex) then begin //or (TargetStartPage = SourceStartPage) then begin
      c := PageCapacity - SourceStartSubIndex;
      if c > ACount then c := ACount;
      if (TargetStartPage = SourceStartPage) then
        MovFrmPagePtr^.MoveRowsDown(SourceStartSubIndex, TargetStartSubIndex, c)
      else
        MovFrmPagePtr^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex, c, PageCapacity, MovToPagePtr);
      TargetStartSubIndex := TargetStartSubIndex + c;

      dec(ACount, c);
      if ACount > 0 then begin
        c := PageCapacity - TargetStartSubIndex;
        if c > ACount then c := ACount;
        inc(MovFrmPagePtr);
        MovFrmPagePtr^.MoveRowsToOther(0, TargetStartSubIndex, c, PageCapacity, MovToPagePtr);

        dec(ACount, c);
        inc(MovToPagePtr);
      end;
    end
    else begin // TargetStartSubIndex <= SourceStartSubIndex  //// AND TargetStartPage < SourceStartPage
      c := PageCapacity - TargetStartSubIndex;
      if c > ACount then c := ACount;
      MovFrmPagePtr^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex, c, PageCapacity, MovToPagePtr);
      SourceStartSubIndex := SourceStartSubIndex + c;

      dec(ACount, c);
      if ACount > 0 then begin
        inc(MovToPagePtr);
        if SourceStartSubIndex < PageCapacity then begin
          c := PageCapacity - SourceStartSubIndex;
          if c > ACount then c := ACount;
          if MovFrmPagePtr = MovToPagePtr then
            MovFrmPagePtr^.MoveRowsDown(SourceStartSubIndex, 0, c)
          else
            MovFrmPagePtr^.MoveRowsToOther(SourceStartSubIndex, 0, c, PageCapacity, MovToPagePtr);
          TargetStartSubIndex := c;

          dec(ACount, c);
        end;
        inc(MovFrmPagePtr);
      end;
    end;

    if ACount > 0 then begin
      assert(MovFrmPagePtr=FPages.ItemPointerFast[SourceEndPage], 'TGenLazPagedList.InternalMoveRowsDown: MovFrmPagePtr=FPages.ItemPointerFast[SourceEndPage]');
      assert(MovToPagePtr=FPages.ItemPointerFast[TargetEndPage], 'TGenLazPagedList.InternalMoveRowsDown: MovToPagePtr=FPages.ItemPointerFast[TargetEndPage]');
      if MovFrmPagePtr = MovToPagePtr then begin
        MovFrmPagePtr^.MoveRowsDown(SourceEndSubIndex-ACount, TargetEndSubIndex-ACount, ACount);
      end
      else
        MovFrmPagePtr^.MoveRowsToOther(SourceEndSubIndex-ACount, TargetEndSubIndex-ACount, ACount, PageCapacity, MovToPagePtr);
    end;
    exit;
  end;




  if BubbleDiff > PageHalfCap then begin
    // more than half of each page needs to bubble, move extra page

    if TargetStartSubIndex >= PageHalfCap then begin
      (** ----- LLLL -----
       ** First target page will not be replaced by move.
       ** Instead move data into first target node. (Fill the first target node)
       ** ---------------- *)
      assert(SourceStartSubIndex <=TargetStartSubIndex, 'TGenLazPagedList.InternalMoveRowsDown: SourceStartSubIndex <=TargetStartSubIndex');

      c := PageCapacity - TargetStartSubIndex;
      FPages.ItemPointerFast[SourceStartPage]^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex, c, PageCapacity, FPages.ItemPointerFast[TargetStartPage]);
      SourceStartSubIndex := SourceStartSubIndex + c;
      assert(SourceStartSubIndex>=PageHalfCap, 'TGenLazPagedList.InternalMoveRowsDown: SourceStartSubIndex>=PageHalfCap');
      assert(SourceStartSubIndex<=PageCapacity, 'TGenLazPagedList.InternalMoveRowsDown: SourceStartSubIndex<=PageCapacity');
      if SourceStartSubIndex = PageCapacity then begin
        SourceStartPage     := SourceStartPage + 1;
        SourceStartSubIndex := 0;
      end;
      TargetStartPage     := TargetStartPage + 1;
      TargetStartSubIndex := 0;
    end;

    (* **************************** *
     *                              *
     *    Move Pages and Bubble     *
     *                              *
     * **************************** *)

    RevBubbleDiff := PageCapacity - BubbleDiff;
    BubbleFirstCnt := RevBubbleDiff;

    SourceAfterEndSubIndex := 0;
    FirstMovePage  := 0;
    FirstPageNotMovedRows := PageCapacity;               // >>

    (** ----- ffff ----- Don't last first page *)
    if SourceEndSubIndex <= PageHalfCap then begin
      SourceAfterEndSubIndex := SourceEndSubIndex;
      dec(SourceEndPage);
      SourceEndSubIndex := PageCapacity;
      // TargetStart.... is only used, when 1) this block was not run  2) relative to SourceAfterEndSubIndex (old value)
    end
    else
    if SourceEndSubIndex < PageCapacity then begin //  no ffff
      BubbleFirstCnt := BubbleFirstCnt - (PageCapacity - SourceEndSubIndex);
      if BubbleFirstCnt < 0 then BubbleFirstCnt := 0;
    end;

    (** ----- >>>> ----- Don't move first page *)
    if SourceStartSubIndex >= PageHalfCap then begin
      assert(TargetStartSubIndex <= SourceStartSubIndex, 'TGenLazPagedList.InternalMoveRowsDown: TargetStartSubIndex <= SourceStartSubIndex');
      FirstMovePage := 1;   // Don't move first source page
      FirstPageNotMovedRows := SourceStartSubIndex;
    end;

    MovePgCnt := SourceEndPage - SourceStartPage + 1 - FirstMovePage;
    assert(MovePgCnt > 0, 'TGenLazPagedList.InternalMoveRowsDown: MovePgCnt > 0');
    assert(MovePgCnt <= TargetEndPage - TargetStartPage + 1, 'TGenLazPagedList.InternalMoveRowsDown: MovePgCnt <= TargetEndPage - TargetStartPage + 1');

    (** ----- PPPP -----
     ** Move pages
     ** ---------------- *)
    MoveFromPage := SourceEndPage;
//    MoveToPage   := TargetStartPage + 1 - MovePgCnt;
    MoveToPage   := TargetStartPage - 1 + MovePgCnt;
    MovePagesDown(MoveFromPage - MovePgCnt + 1, MoveToPage - MovePgCnt + 1, MoveToPage);



    (** ----------------
     ** Rotate moved-source pages (now empty / containing "to be restored"-target data.
     ** Make space for bubble and "to be restored"-source data
        Either page was moved, so they must each have had MORE than half = PP
        MoveFromPage           may contain  ~~~~ or >>>> at start | Example 20
                               may receive  ---- at start         | Example 10,20
        MoveFromFirstEmptyPage  may contain  ____ at end           | Example 10,11,12, 20,21,23,...
                               may receive  ==== or ^^^^ at end   | Example 15
     ** ---------------- *)
    RotateFirst := 0;
    RotateLast  := 0;
    MoveFromFirstEmptyPage  := MoveFromPage - MovePgCnt +1;
    if MoveFromFirstEmptyPage < MoveToPage+1 then
      MoveFromFirstEmptyPage  := MoveToPage+1;  // source and dest overlap

// XXXXXXXXXXXXXXXXXXXXXX SourceAfterEndSubIndex = 0
    if (MoveFromPage = MoveFromFirstEmptyPage) and (SourceAfterEndSubIndex = 0) and
       ( (MoveToPage < SourceStartPage) or
         ( (MoveToPage = SourceStartPage) and (TargetEndSubIndex < SourceStartSubIndex) )
       ) and
       (TargetEndSubIndex >= PageHalfCap) and
       (TargetEndSubIndex < FirstPageNotMovedRows) // ~~~~ has more elements than >>>>
    then begin
      // Rotate according to needs of FIRST page
      //assert( assert(BubbleFirstCnt=0, 'TGenLazPagedList.InternalMoveRowsDown:  assert(BubbleFirstCnt=0');
      RotateLast := -(PageCapacity - TargetEndSubIndex);   // Move ~~~~ away (to end of page) // Example 30 (or mix of ~~ and >>_
    end
    else begin
      (* if FirstMovePage = 0 then FirstPageNotMovedRows is also 0
         FirstMovePage= 0: RotateLast:= BubbleFirstCnt                       // space for ^^^^            (Moves ____ away)
         FirstMovePage<>0: RotateLast:= BubbleFirstCnt+FirstPageNotMovedRows; // space for ^^^^ and >>>>   (Moves ____ away)
      *)
      if MoveToPage < MoveFromFirstEmptyPage - 1 then
        RotateLast := -(PageCapacity - FirstPageNotMovedRows)                    // Don't bubble into MoveFromFirstEmptyPage
      else
        RotateLast := -BubbleFirstCnt - (PageCapacity - FirstPageNotMovedRows);
    end;

    if (FirstMovePage = 0) and (SourceStartSubIndex > 0) and (SourceStartPage > MoveToPage)
    then
      RotateLast := RotateLast - SourceStartSubIndex;   // ==== Example 31   (Moves ____ or ~~~~ away)

    MovLastPagePtr := FPages.ItemPointerFast[MoveFromFirstEmptyPage];
    MovLastPagePtr^.AdjustFirstItemOffset(RotateLast, FPageSize.FPageSizeMask);


    MovFrmPagePtr := FPages.ItemPointerFast[MoveFromPage];
    MovToPagePtr := FPages.ItemPointerFast[MoveToPage];
    TrgEndPagePtr := FPages.ItemPointerFast[TargetStartPage]; // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX TrgEndPagePtr wrong name???? Well, end in negative dir....

    if MoveFromPage <> MoveFromFirstEmptyPage then begin
      // There is a separate first page to rotate
      if (SourceEndSubIndex < PageCapacity) then // only true, if there is no ffff
        RotateFirst := (PageCapacity - SourceEndSubIndex); // space needed for ---- (Moves ~~~ or >>>> out of the way)

      MovFrmPagePtr^.AdjustFirstItemOffset(RotateFirst, FPageSize.FPageSizeMask);
    end
    else
      RotateFirst := RotateLast;


    (** ----- "----" -----
     ** Restore first page
     ** ---------------- *)
    if (SourceEndSubIndex < PageCapacity) then // only true, if there is no ffff
      MovToPagePtr^.MoveRowsToOther(SourceEndSubIndex, SourceEndSubIndex, PageCapacity - SourceEndSubIndex, PageCapacity, MovFrmPagePtr);


    (** ----- ^^^^ -----
     ** Bubble down from first page / rotate
     ** ---------------- *)
    if BubbleFirstCnt > 0 then begin
assert(SourceEndSubIndex-BubbleFirstCnt=PageCapacity-RevBubbleDiff, 'TGenLazPagedList.InternalMoveRowsDown: SourceEndSubIndex-BubbleFirstCnt=PageCapacity-RevBubbleDiff');
      MovToPagePtr^.MoveRowsToOther(SourceEndSubIndex - BubbleFirstCnt, 0, BubbleFirstCnt, PageCapacity, MovToPagePtr+1);
    end;
    MovToPagePtr^.AdjustFirstItemOffset(-RevBubbleDiff, FPageSize.FPageSizeMask);

    (** ----- ^^^^ -----
     ** Bubble down between pages
     ** ---------------- *)
    if TargetStartPage < MoveToPage then begin
      InternalBubbleEntriesUp(TargetStartPage, MoveToPage, RevBubbleDiff);
      TrgEndPagePtr^.AdjustFirstItemOffset(-RevBubbleDiff, FPageSize.FPageSizeMask);
    end;


    (** ----- ==== -----
     ** Restore end of Source area
     ** ---------------- *)
    if (FirstMovePage = 0) and (SourceStartSubIndex > 0) then begin
      i := 0; //SourceStartSubIndex;
      if (MovePgCnt = 1) or (TargetStartPage < MoveToPage)
      then
        i := RevBubbleDiff;
      if (MoveFromPage - MovePgCnt > MoveToPage) then begin
        TrgEndPagePtr^.MoveRowsToOther(i, 0,
          SourceStartSubIndex, PageCapacity, MovLastPagePtr);
      end
      else
      if (MoveFromPage - MovePgCnt = MoveToPage) then begin
        // only in front of ^^  | Example 12
        c := SourceStartSubIndex - BubbleFirstCnt;
        i := i + BubbleFirstCnt;
        if c > 0 then begin
          TrgEndPagePtr^.MoveRowsToOther(i, SourceStartSubIndex-c,
            c, PageCapacity, MovLastPagePtr);
        end;
      end;
    end;


    (** ----- ____ -----
     ** Restore data from last page that was overwritten by move
     ** ---------------- *)
    if TargetStartSubIndex > 0 then begin
       MovLastPagePtr^.MoveRowsToOther(
         (-RotateLast) and FPageSize.FPageSizeMask, 0,
         TargetStartSubIndex, PageCapacity, TrgEndPagePtr);
    end;


    (** ----- >>>> -----
     ** Move rows to last page
     ** ---------------- *)
    if FirstMovePage <> 0 then begin
       assert(TargetStartSubIndex <= RevBubbleDiff, 'TGenLazPagedList.InternalMoveRowsDown: TargetStartSubIndex <= RevBubbleDiff');
       if MoveToPage >= SourceStartPage then begin
         TmpPagePtr := MovFrmPagePtr;
         c := RotateFirst;
       end
       else begin
         assert(SourceStartPage <> MoveFromPage, 'TGenLazPagedList.InternalMoveRowsDown: SourceStartPage <> MoveFromPage');
         TmpPagePtr := FPages.ItemPointerFast[SourceStartPage];
         c := 0;
       end;

       assert(FirstPageNotMovedRows-TargetStartSubIndex=BubbleDiff, 'TGenLazPagedList.InternalMoveRowsDown: FirstPageNotMovedRows-TargetStartSubIndex=BubbleDiff');
       j := FirstPageNotMovedRows - c;
       TmpPagePtr^.MoveRowsToOther(j, TargetStartSubIndex,
         PageCapacity - FirstPageNotMovedRows, PageCapacity, TrgEndPagePtr);
    end;

    (** ----- ~~~~ -----
     ** Restore data from last page that was overwritten by move
     ** ---------------- *)
    if (PageCapacity - SourceEndSubIndex > RevBubbleDiff) then begin // only true, if there is no ffff
      j := (TargetEndSubIndex - RotateFirst) and FPageSize.FPageSizeMask;
      MovFrmPagePtr^.MoveRowsToOther(j, TargetEndSubIndex, PageCapacity - TargetEndSubIndex, PageCapacity, MovToPagePtr);
    end;


    if SourceAfterEndSubIndex > 0 then begin
      (** ----- ffff -----
       ** First page
       ** ---------------- *)
      assert(TargetEndSubIndex >= SourceAfterEndSubIndex, 'TGenLazPagedList.InternalMoveRowsDown: TargetEndSubIndex >= SourceAfterEndSubIndex');
      FPages.ItemPointerFast[SourceEndPage+1]^.MoveRowsToOther(0, TargetEndSubIndex - SourceAfterEndSubIndex,
        SourceAfterEndSubIndex, PageCapacity, FPages.ItemPointerFast[TargetEndPage]);
    end;


  end
  else
  begin
    // LESS than half of each page needs to bubble

    (* ****************************************** *
     *                                            *
     *    Last Page                               *
     *                                            *
     *    If the last page has less than          *
     *    PageHalfCap entries to move             *
     *                                            *
     * ****************************************** *)

    LastPageDone := False;
    if SourceStartSubIndex >= PageHalfCap then begin
      (** ----- LLLL -----
       ** First source page will not be moved.
       ** Instead move data into first target node. Data that goes behind the bubble
       ** ---------------- *)

      assert(SourceStartSubIndex - BubbleDiff = TargetStartSubIndex, 'TGenLazPagedList.InternalMoveRowsDown: SourceStartSubIndex - BubbleDiff = TargetStartSubIndex');
      if TargetStartPage < SourceStartPage then
        FPages.ItemPointerFast[SourceStartPage]^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex, PageCapacity - SourceStartSubIndex, PageCapacity, FPages.ItemPointerFast[TargetStartPage])
      else
      if (BubbleDiff > 0) then    // make room for bubble up
        FPages.ItemPointerFast[SourceStartPage]^.MoveRowsDown(SourceStartSubIndex, TargetStartSubIndex, PageCapacity - SourceStartSubIndex);
// TODO: known to be less than half the page InternalMove // but wrapped for bytes

      TargetStartSubIndex := PageCapacity - BubbleDiff; // TargetStartSubIndex - SourceStartSubIndex;  // may end up PageCapacity;
// XXX if TargetStartSubIndex = PageCapacity then XXXX;
      inc(SourceStartPage);
      SourceStartSubIndex := 0;
      LastPageDone := True;
    end;

    (* **************************** *
     *                              *
     *    Move Pages and Bubble     *
     *                              *
     * **************************** *)

    LastMovePage := 0;
    SourceAfterEndSubIndex := SourceEndSubIndex; // original before ffff
    MoveFromPage := SourceEndPage;
    MoveToPage   := TargetEndPage;
    MovePgCnt    := MoveFromPage - SourceStartPage + 1;

    (** ----- PPPP pppp LLLL -----
     ** Move full pages
     ** ---------------- *)
    if TargetEndPage < SourceEndPage then begin
      if (TargetEndSubIndex <= PageHalfCap) or    // Example 10: skip moving first page
         (SourceEndSubIndex <= PageHalfCap)
      then begin
        LastMovePage := 1;
        dec(MoveFromPage);
        dec(MovePgCnt);
        if TargetEndSubIndex <= SourceEndSubIndex then
          dec(MoveToPage);
        SourceEndSubIndex := 0; // don't rotate
      end;
    end;

    MoveFromFirstEmptyPage  := MoveFromPage - MovePgCnt + 1;
    if MoveFromFirstEmptyPage <= MoveToPage then
      MoveFromFirstEmptyPage  := MoveToPage+1;

    (** ----------------
     ** Rotate moved-source pages (now empty / containing "to be restored"-target data.
     ** Make space for bubble and "to be restored"-source data
        MoveFromPage           may contain  ~~~~ at start
                               may receive  ---- at start
        MoveFromFirstEmptyPage  may contain  ____ at end
                               may receive  ==== at end
     ** ---------------- *)
    RotateFirst := 0;
    RotateLast  := 0;
    MovLastPagePtr := nil;
    MovFrmPagePtr  := nil;

    if MoveToPage < MoveFromPage then begin
      (** ---------- Move the pages ---------------- *)
      if (MovePgCnt > 0) then
        MovePagesDown(MoveFromPage - MovePgCnt + 1, MoveToPage - MovePgCnt + 1, MoveToPage);

      assert(MoveFromFirstEmptyPage < FPages.Count, 'TGenLazPagedList.InternalMoveRowsDown: MoveFromFirstEmptyPage < FPages.Count');
      MovLastPagePtr := FPages.ItemPointerFast[MoveFromFirstEmptyPage];
      MovFrmPagePtr := FPages.ItemPointerFast[MoveFromPage];

      if (MoveToPage < SourceStartPage) and
         (SourceStartSubIndex > 0) and // There may be === // If LastPageDone then ALWAYS FALSE
         (TargetStartSubIndex < PageHalfCap) // There is NO "<<<<" / Otherwise no ____
      then begin
        //assert(   assert(MoveFromPage+MovePgCnt-1 = SourceStartPage, 'TGenLazPagedList.InternalMoveRowsDown:    assert(MoveFromPage+MovePgCnt-1 = SourceStartPage');
        RotateLast := TargetStartSubIndex; // Make space for ==== (Move ___ out of the way)
      end;

      if (MoveFromPage = MoveFromFirstEmptyPage) then begin
        RotateLast := RotateLast + PageCapacity - SourceEndSubIndex; // Make space for ---- (Move ~~~~ out of the way)
        RotateFirst := RotateLast;
      end
      else begin
        RotateFirst := PageCapacity - SourceEndSubIndex; // Make space for ---- (Move ~~~~ out of the way)
        MovFrmPagePtr^.AdjustFirstItemOffset(RotateFirst, FPageSize.FPageSizeMask);
      end;

      MovLastPagePtr^.AdjustFirstItemOffset(RotateLast, FPageSize.FPageSizeMask);



      (** ----- ==== -----
       ** Restore last source page.
       ** ---------------- *)
      if (SourceStartSubIndex > 0) and // (SourceEndRestoreCnt > 0) and
         (MoveToPage < SourceStartPage)
      then begin
        if (TargetEndPage = SourceStartPage) then begin
          c := Min(SourceAfterEndSubIndex, TargetEndSubIndex);
          if c < SourceStartSubIndex then
            FPages.ItemPointerFast[MoveToPage-MovePgCnt+1]^.MoveRowsToOther(c, c, SourceStartSubIndex - c, PageCapacity, MovLastPagePtr);
        end
        else begin
          FPages.ItemPointerFast[MoveToPage-MovePgCnt+1]^.MoveRowsToOther(0, 0,
            SourceStartSubIndex, PageCapacity, MovLastPagePtr);
        end;
      end;
    end;


    if not LastPageDone then begin
      TrgEndPagePtr := FPages.ItemPointerFast[TargetStartPage];
      if BubbleDiff > 0 then begin
        (** ----- <<<< -----
         ** Bubble out of the  last source page
         ** ---------------- *)
        if TargetStartSubIndex >= PageHalfCap then begin
          assert(TargetStartPage < TargetEndPage, 'TGenLazPagedList.InternalMoveRowsDown: TargetStartPage < TargetEndPage');
          //assert(   assert(SourceStartPage-MoveToPage+MoveFromPage = TargetStartPage+1, 'TGenLazPagedList.InternalMoveRowsDown:    assert(SourceStartPage-MoveToPage+MoveFromPage = TargetStartPage+1');

          TmpPagePtr := TrgEndPagePtr;
          inc(TrgEndPagePtr);
          TrgEndPagePtr^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex,
            PageCapacity - TargetStartSubIndex, PageCapacity, TmpPagePtr);

          SourceStartSubIndex := SourceStartSubIndex + PageCapacity - TargetStartSubIndex;
          TargetStartSubIndex := 0;
          inc(TargetStartPage);
        end;

        (** ---------------- Rotate last target page. // Make space for bubble-in of ^^^^ ---------------- *)
        if (TargetStartPage = TargetEndPage) and ((PageCapacity - TargetEndSubIndex) >= BubbleDiff) then begin
          c := TargetEndSubIndex - TargetStartSubIndex;
          TrgEndPagePtr^.MoveRowsDown(TargetStartSubIndex + BubbleDiff, TargetStartSubIndex, c);
        end
        else begin
          c := PageCapacity - TargetStartSubIndex - BubbleDiff;
          assert(c>=0, 'TGenLazPagedList.InternalMoveRowsDown: c>=0');
          if c > 0 then
            TrgEndPagePtr^.MoveRowsDown(TargetStartSubIndex + BubbleDiff, TargetStartSubIndex, c);
        end;
      end;

      (** ----- ____ -----
       ** Restore last target page.
       ** ---------------- *)
        if (MoveToPage < MoveFromPage) and (TargetStartSubIndex > 0) then begin
// Note: assert from moveup...
          MovLastPagePtr^.MoveRowsToOther(
            (PageCapacity - RotateLast) and FPageSize.FPageSizeMask, 0,
            TargetStartSubIndex, PageCapacity, TrgEndPagePtr);
        end;
    end;


    MovToPagePtr := FPages.ItemPointerFast[MoveToPage];

    if (BubbleDiff > 0) and (TargetStartPage < MoveToPage) then begin
      (** ----- ^^^^ -----
       ** Bubble between pages
       ** ---------------- *)
      InternalBubbleEntriesDown(MoveToPage, TargetStartPage, BubbleDiff);
      if (LastMovePage = 0) then begin
        if (SourceEndSubIndex = 0) then
          MovToPagePtr^.AdjustFirstItemOffset(BubbleDiff, FPageSize.FPageSizeMask)
        else
          MovToPagePtr^.MoveRowsDown(SourceEndSubIndex-TargetEndSubIndex, 0, TargetEndSubIndex);
      end;
    end;

    (* **************************** *
     *                              *
     *    First Page                *
     *                              *
     * **************************** *)

    TrgStartPagePtr := FPages.ItemPointerFast[TargetEndPage];
    if LastMovePage > 0 then begin // Example 10
      (** ----- ffff  -----
       ** First source page was NOT moved
       ** Move the data to the target (may be split to 2 target pages)
       ** ---------------- *)

      (** -----
       ** Rotate data in lowest MOVED target page, to make room Check, there may be room already) ** *)
      if MoveToPage > TargetStartPage then
        MovToPagePtr^.AdjustFirstItemOffset(BubbleDiff, FPageSize.FPageSizeMask);

      (** ----- ffff ----- ** *)
      SrcStartPagePtr := FPages.ItemPointerFast[SourceEndPage];

      c  := SourceAfterEndSubIndex;
      c2 := TargetEndSubIndex;  // 2nd half of Ff
      if c > c2 then begin
        SrcStartPagePtr^.MoveRowsToOther(c-c2, 0, c2, PageCapacity, TrgStartPagePtr);
        dec(TrgStartPagePtr);
        c := c - c2;
        SrcStartPagePtr^.MoveRowsToOther(0, PageCapacity - c, c, PageCapacity, TrgStartPagePtr);
      end
      else
        SrcStartPagePtr^.MoveRowsToOther(0, TargetEndSubIndex - c, c, PageCapacity, TrgStartPagePtr);
    end

    else
    if MoveToPage < MoveFromPage
    then begin
      (** ----- "----" -----
       ** Restore data from first page that was "over moved"
       ** ---------------- *)
      assert((PageCapacity-SourceEndSubIndex)+(PageCapacity-TargetEndSubIndex)<=PageCapacity, 'TGenLazPagedList.InternalMoveRowsDown: (PageCapacity-SourceEndSubIndex)+(PageCapacity-TargetEndSubIndex)<=PageCapacity');
      assert(TargetEndSubIndex>=PageHalfCap, 'TGenLazPagedList.InternalMoveRowsDown: TargetEndSubIndex>=PageHalfCap');

      (** ----- "----" -----
       ** Move back to start of first page ** *)
      if SourceEndSubIndex < PageCapacity then begin
        assert(MovFrmPagePtr<>nil, 'TGenLazPagedList.InternalMoveRowsDown: MovFrmPagePtr<>nil');
        MovToPagePtr^.MoveRowsToOther(SourceEndSubIndex, SourceEndSubIndex, PageCapacity - SourceEndSubIndex, PageCapacity, MovFrmPagePtr);
      end;
    end;

    (** ----- ~~~~ -----
     ** Restore data at start of first target page
     ** ---------------- *)
    if (TargetEndPage < SourceStartPage) and (PageCapacity - TargetEndSubIndex < PageHalfCap) then begin
      c := PageCapacity - TargetEndSubIndex;
      if c > 0 then begin
        assert(MovFrmPagePtr<>nil, 'TGenLazPagedList.InternalMoveRowsDown: MovFrmPagePtr<>nil');
        MovFrmPagePtr^.MoveRowsToOther((TargetEndSubIndex-RotateFirst) and FPageSize.FPageSizeMask, TargetEndSubIndex, c, PageCapacity, TrgStartPagePtr);
      end;
    end;

  end;
end;

procedure TGenLazPagedList.InternalMoveRowsUp(AFromIndex, AToIndex, ACount: Integer);
var
  PageCapacity, PageHalfCap: integer;
  SourceStartPage, SourceStartSubIndex, SourceEndPage, SourceEndSubIndex: Integer;
  TargetStartPage, TargetStartSubIndex, TargetEndPage, TargetEndSubIndex: Integer;
  i, j, c, c2: integer;
  BubbleDiff, RevBubbleDiff: integer;
  SourceBeforeStartSubIndex, FirstMovePage, LastMovePage: Integer;
  MovePgCnt, MoveFromPage, MoveToPage, MoveFromLastEmptyPage: integer;
  RotateFirst, RotateLast,
  LastPageNotMovedRows,
  BubbleFirstCnt: Integer;
  TrgEndPagePtr, MovLastPagePtr, MovFrmPagePtr, MovToPagePtr, TmpPagePtr,
    SrcStartPagePtr, TrgStartPagePtr: PPageType;
  LastPageDone: Boolean;
begin
  PageCapacity := FPageSize.FPageSizeMask+1;
  PageHalfCap  := PageCapacity >> 1;

  BubbleDiff := (AToIndex - AFromIndex) and FPageSize.FPageSizeMask;

  (* Calculate FROM page, and index in page *)
  AFromIndex := AFromIndex + FFirstPageEmpty;
  SourceStartPage       := AFromIndex >> FPageSize.FPageSizeExp;
  SourceStartSubIndex   := (AFromIndex and FPageSize.FPageSizeMask);

  AFromIndex := AFromIndex + aCount - 1;
  SourceEndPage       := AFromIndex >> FPageSize.FPageSizeExp;
  SourceEndSubIndex   := (AFromIndex and FPageSize.FPageSizeMask) + 1; // NEXT

  (* Calculate TO page, and index in page *)
  AToIndex := AToIndex + FFirstPageEmpty;
  TargetStartPage     := AToIndex >> FPageSize.FPageSizeExp;
  TargetStartSubIndex := AToIndex and FPageSize.FPageSizeMask;

  AToIndex := AToIndex + aCount - 1;
  TargetEndPage     := AToIndex >> FPageSize.FPageSizeExp;
  TargetEndSubIndex := (AToIndex and FPageSize.FPageSizeMask) + 1; // NEXT


  //if ACount <= 3*PageHalfCap then begin
  //if ACount <= 2*PageCapacity then begin
  i := PageCapacity - max(SourceStartSubIndex, TargetStartSubIndex);
  if i > PageHalfCap then i := 0;
  c := min(SourceEndSubIndex, TargetEndSubIndex);
  if c > PageHalfCap then c := 0;
  if ACount - i - c <= PageHalfCap then begin
    (* **************************** *
     *    Max 2 source pages        *
     *    No full page moves needed *
     * **************************** *)
    MovFrmPagePtr := FPages.ItemPointerFast[SourceEndPage];
    MovToPagePtr := FPages.ItemPointerFast[TargetEndPage];

    if (TargetEndSubIndex > SourceEndSubIndex) then begin //or (TargetEndPage = SourceEndPage) then begin
      c := SourceEndSubIndex;
      if c > ACount then c := ACount;
      TargetEndSubIndex := TargetEndSubIndex - c;
      if (TargetEndPage = SourceEndPage) then
        MovFrmPagePtr^.MoveRowsUp(SourceEndSubIndex-c, TargetEndSubIndex, c)
      else
        MovFrmPagePtr^.MoveRowsToOther(SourceEndSubIndex-c, TargetEndSubIndex, c, PageCapacity, MovToPagePtr);

      dec(ACount, c);
      if ACount > 0 then begin
        c := TargetEndSubIndex;
        if c > ACount then c := ACount;
        dec(MovFrmPagePtr);
        MovFrmPagePtr^.MoveRowsToOther(PageCapacity - c, TargetEndSubIndex-c, c, PageCapacity, MovToPagePtr);

        dec(ACount, c);
        dec(MovToPagePtr);
      end;
    end
    else begin // TargetEndSubIndex <= SourceEndSubIndex  //// AND TargetEndPage > SourceEndPage
      c := TargetEndSubIndex;
      if c > ACount then c := ACount;
      SourceEndSubIndex := SourceEndSubIndex - c;
      MovFrmPagePtr^.MoveRowsToOther(SourceEndSubIndex, TargetEndSubIndex-c, c, PageCapacity, MovToPagePtr);

      dec(ACount, c);
      if ACount > 0 then begin
        dec(MovToPagePtr);
        if SourceEndSubIndex > 0 then begin
          c := SourceEndSubIndex;
          if c > ACount then c := ACount;
          TargetEndSubIndex := PageCapacity - c;
          if MovFrmPagePtr = MovToPagePtr then
            MovFrmPagePtr^.MoveRowsUp(SourceEndSubIndex-c, TargetEndSubIndex, c)
          else
            MovFrmPagePtr^.MoveRowsToOther(SourceEndSubIndex-c, TargetEndSubIndex, c, PageCapacity, MovToPagePtr);

          dec(ACount, c);
        end;
        dec(MovFrmPagePtr);
      end;
    end;

    if ACount > 0 then begin
      assert(MovFrmPagePtr=FPages.ItemPointerFast[SourceStartPage], 'TGenLazPagedList.InternalMoveRowsUp: MovFrmPagePtr=FPages.ItemPointerFast[SourceStartPage]');
      assert(MovToPagePtr=FPages.ItemPointerFast[TargetStartPage], 'TGenLazPagedList.InternalMoveRowsUp: MovToPagePtr=FPages.ItemPointerFast[TargetStartPage]');
      if MovFrmPagePtr = MovToPagePtr then begin
        MovFrmPagePtr^.MoveRowsUp(SourceStartSubIndex, TargetStartSubIndex, ACount);
      end
      else
        MovFrmPagePtr^.MoveRowsToOther(SourceStartSubIndex, TargetStartSubIndex, ACount, PageCapacity, MovToPagePtr);
    end;
    exit;
  end;



  (* When moving full pages, the following areas may need to be restored
     (Some data may not need to be restored, if pages overlap)
          | --PPPPP^ | PPPPP=== |  | ~~~***** | ******__
      Mov:| ~~~***** | ******__ |  | --PPPPP^ | PPPPP=== | // Pages MOVED, NOT bubbled yet
      Rot:| --~~~*** | __***=== |  | --PPPPP^ | PPPPP=== | // Restored Source, NOT bubbled yet
                                                           // (part of restoration may be AFTER bubble)
                                                           // ROTATED to keep "Target restoration"
                                                           // (may rotate LEFT OR RIGHT)
      Bbl:| --~~~*** | __***=== |  | -- PPPPP | ^PPPPP== | // Bubbled
      Fin:| --****** | *****=== |  | ~~~PPPPP | ^PPPPP__ | // Restored Target

     PP Data to be moved, PAGE will be moved
     ^^ Data to be moved, BUBBLE between pages
     ** TARGET area, data will be overwritten (may be LOST, may not be kept in the old-source-area)

     ---- Data in first source page, before start of source
     ==== Data in last source page, after end of source
     ~~~~ Data in first target page, before start of target
     ____ Data in last target page, after end of target

     Partial restoration (If last source and first target overlap):
     ==== For BubbleDiff > PageHalfCap
          | --^^^PPP | ^^^PP==* | ******** | ****____ | // Move extra page, then bubble down
     ==== For BubbleDiff <= PageHalfCap
          | -----P^^ | PPPPP==* | *******_ | ........ | // Move one a page, first page moves separate "ffff"
     ~~~~ For BubbleDiff <= PageHalfCap
          | --PPPPP^ | PP~~***** | ******__ |  // Last Source page is not moved (as page).
                                               // First Target page is moved (to make room for first source page)

  *)


  if BubbleDiff > PageHalfCap then begin
    // more than half of each page needs to bubble, move extra page

    (*
    *** Content at the end may be excluded from the PAGE-MOVES
        Each of >>>> or LLLL can be on their own, without the other.
        Or they can be in the order given in the example.

    Example 1 & 2
                            *******   **
              | ---PPPPP | >>LL**** | **...... |             // Don't move last page
          LL :| ---PPPPP | >>****** | LL...... |             // Move LL first
          Mov:| >>       | ---PPPPP | LL...... |             // Move Page
          Rot:| ---   >> |  PPPPP   | LL...... |             // Roteate / Restore ---- / Bubble
          Fin:| ---      |  PPPPP>> | LL...... |             // Move >>

              | ---PPPPP | >>LL.... | ~******* | **...... |             // Don't move last page
          LL :| ---PPPPP | >>  .... | ~******* | LL...... |             // Move LL first
          Mov:| ~        | >>  .... | ---PPPPP | LL...... |             // Move Page
          Rot:| ---    ~ | >>  .... |  PPPPP   | LL...... |             // Restore ----
          Fin:| ---      |     .... | ~PPPPP>> | LL...... |             // Move >> // Restore ~~~~

     LLLL At the end of source, if the Last target page will get <= PageHalfCap
          Moving entries, rather than pages saves restoring large ____
          - This will be done upfront. And the source/target range will be reduced
          - There will be no need for ____
     >>>> If the (remaining) last source page is <= PageHalfCap
          - This would otherwise need to override the LLLL, and then bubble down
          - If there was no LLLL, then it would cause a full page restoration after
            the last target page (as it would need to bubble down from after the target)


    *** First page may not be moved as page, if less <= PageHalfCap
        This avoids restoring a bigger ----

    Example 3
                              *****   ******
              | .....fff | ^^^PPPPP | >*****__ |
          Mov:| .....fff | >     __ | ^^^PPPPP |
          Rot:| .....fff |      ^^^ | PPPPP>__ | // Rotate / Restore / Bubble
          Fin | .....    |   fff^^^ | PPPPP>__ | // Move ffff

     ffff At start of source <= PageHalfCap
          Avoid a large ---- restoration
          There will be no ----

    *** Special BubbleFirstCnt when first target is <= PageHalfCap
        The ---- is taken out of the bubble
    Example
              | -^PPPPPP | ^^PPPPP= | ...... * | ******** | *****___ |
          Mov:| ******** | *****___ | ...... * | -^PPPPPP | ^^PPPPP= |
          Fin:| -        |        = | ...... ^ | PPPPPP^^ | PPPPP___ |

    ***
    Example 4 / Bubble down into ____ (after move, before restore)
              | --^PPPPP | ^^^PP==* | ******** | ****____ |
          Mov:| ******** | ****____ |   ^PPPPP | ^^^PP==*  |
          Rot:| --****** | *____    |   ^PPPPP | ^^^PP==*  | Restore ---- / Rotate
          Bbl:| --****** | *____==^ | PPPPP^^^ | PP     *  | Bubble / Restore ====
          Fin:| --****** | ***__==^ | PPPPP^^^ | PP  ____  | Restore ____



          5) DIFF= 7 (7)        // Move = 2 / Bubble=1
              | ---PPPPP | >~...... |
              | >~...... | ---PPPPP |  MIX OF >> ~~
              | ---      |  ~PPPPP> |
          6) DIFF= 7 (7)        // Move = 2 / Bubble=1
              | --PPPPP= | ~.....__ |
              | ~.....__ | --PPPPP= |  MIX OF >> ~~
              | --     = | ~PPPPP__|


    *** Restoration needed after move (non moved content, that got moved with the pages)
      --  First Source (Low) |  MOVED TO first target       (if not ffff)
      ==  Last  Source (High)|  MOVED TO last target        (if not >> or LL / if not overlapping)
      ~~  First Target (Low) |  MOVED TO first source       (if not ffff / if not overlapping source)

    *)


    if TargetEndSubIndex <= PageHalfCap then begin
      (** ----- LLLL -----
       ** Last target page will not be replaced by move.
       ** Instead move data into last target node. (Fill the last target node)
       ** ---------------- *)
      assert(SourceEndSubIndex >=TargetEndSubIndex, 'TGenLazPagedList.InternalMoveRowsUp: SourceEndSubIndex >=TargetEndSubIndex');

      FPages.ItemPointerFast[SourceEndPage]^.MoveRowsToOther(SourceEndSubIndex-TargetEndSubIndex, 0, TargetEndSubIndex, PageCapacity, FPages.ItemPointerFast[TargetEndPage]);
      SourceEndSubIndex := SourceEndSubIndex - TargetEndSubIndex;
      assert(SourceEndSubIndex<=PageHalfCap, 'TGenLazPagedList.InternalMoveRowsUp: SourceEndSubIndex<=PageHalfCap');
      if SourceEndSubIndex = 0 then begin
        SourceEndPage     := SourceEndPage - 1;
        SourceEndSubIndex := PageCapacity;
      end;
      TargetEndPage     := TargetEndPage - 1;
      TargetEndSubIndex := PageCapacity;
    end;

    (* **************************** *
     *                              *
     *    Move Pages and Bubble     *
     *                              *
     * **************************** *)

    RevBubbleDiff := PageCapacity - BubbleDiff;
    BubbleFirstCnt := RevBubbleDiff;

    SourceBeforeStartSubIndex := 0;
    LastMovePage  := 0;
    LastPageNotMovedRows := 0;               // >>

    (** ----- ffff ----- Don't move first page *)
    if SourceStartSubIndex >= PageHalfCap then begin
      SourceBeforeStartSubIndex := SourceStartSubIndex;
      inc(SourceStartPage);
      SourceStartSubIndex := 0;
      // TargetStart.... is only used, when 1) this block was not run  2) relative to SourceBeforeStartSubIndex (old value)
    end
    else
    if SourceStartSubIndex > 0 then begin //  no ffff
      BubbleFirstCnt := BubbleFirstCnt - SourceStartSubIndex;
      if BubbleFirstCnt < 0 then BubbleFirstCnt := 0;
    end;

    (** ----- >>>> ----- Don't move last page *)
    if SourceEndSubIndex <= PageHalfCap then begin
      assert(TargetEndSubIndex >= SourceEndSubIndex, 'TGenLazPagedList.InternalMoveRowsUp: TargetEndSubIndex >= SourceEndSubIndex');
      LastMovePage := 1;   // Don't move last source page
      LastPageNotMovedRows := SourceEndSubIndex;
    end;

    MovePgCnt := SourceEndPage - SourceStartPage + 1 - LastMovePage;
    assert(MovePgCnt > 0, 'TGenLazPagedList.InternalMoveRowsUp: MovePgCnt > 0');
    assert(MovePgCnt <= TargetEndPage - TargetStartPage + 1, 'TGenLazPagedList.InternalMoveRowsUp: MovePgCnt <= TargetEndPage - TargetStartPage + 1');

    (** ----- PPPP -----
     ** Move pages
     ** ---------------- *)
    MoveFromPage := SourceStartPage;
    MoveToPage   := TargetEndPage + 1 - MovePgCnt;
    MovePagesUp(MoveFromPage, MoveToPage, MoveToPage + MovePgCnt - 1);



    (** ----------------
     ** Rotate moved-source pages (now empty / containing "to be restored"-target data.
     ** Make space for bubble and "to be restored"-source data
        Either page was moved, so they must each have had MORE than half = PP
        MoveFromPage           may contain  ~~~~ or >>>> at start | Example 1, 2
                               may receive  ---- at start         | Example 1, 2
        MoveFromLastEmptyPage  may contain  ____ at end           | Example 3
                               may receive  ==== or ^^^^ at end   | Example 3, 4
     ** ---------------- *)
    RotateFirst := 0;
    RotateLast  := 0;
    MoveFromLastEmptyPage  := MoveFromPage + MovePgCnt -1;
    if MoveFromLastEmptyPage > MoveToPage-1 then
      MoveFromLastEmptyPage  := MoveToPage-1;  // source and dest overlap

    if (MoveFromPage = MoveFromLastEmptyPage) and (SourceBeforeStartSubIndex = 0) and
       ( (MoveToPage > SourceEndPage) or
         ( (MoveToPage = SourceEndPage) and (TargetStartSubIndex > SourceEndSubIndex) )
       ) and
       (TargetStartSubIndex <= PageHalfCap) and
       (TargetStartSubIndex > LastPageNotMovedRows) // ~~~~ has more elements than >>>>
    then begin
      // Rotate according to needs of FIRST page
      assert(BubbleFirstCnt=0, 'TGenLazPagedList.InternalMoveRowsUp: BubbleFirstCnt=0');
      RotateLast := TargetStartSubIndex;   // Move ~~~~ away (to end of page) // Example 5 (or mix of ~~ and >>_
    end
    else begin
      (* if LastMovePage = 0 then LastPageNotMovedRows is also 0
         LastMovePage= 0: RotateLast:= BubbleFirstCnt                       // space for ^^^^            (Moves ____ away)
         LastMovePage<>0: RotateLast:= BubbleFirstCnt+LastPageNotMovedRows; // space for ^^^^ and >>>>   (Moves ____ away)
      *)
      if MoveToPage > MoveFromLastEmptyPage + 1 then
        RotateLast := LastPageNotMovedRows                    // Don't bubble into MoveFromLastEmptyPage
      else
        RotateLast := BubbleFirstCnt + LastPageNotMovedRows;
    end;

    if (LastMovePage = 0) and (SourceEndSubIndex < PageCapacity) and (SourceEndPage < MoveToPage)
    then
      RotateLast := RotateLast + (PageCapacity - SourceEndSubIndex);   // ==== Example 6   (Moves ____ or ~~~~ away)

    MovLastPagePtr := FPages.ItemPointerFast[MoveFromLastEmptyPage];
    MovLastPagePtr^.AdjustFirstItemOffset(RotateLast, FPageSize.FPageSizeMask);


    MovFrmPagePtr := FPages.ItemPointerFast[MoveFromPage];
    MovToPagePtr := FPages.ItemPointerFast[MoveToPage];
    TrgEndPagePtr := FPages.ItemPointerFast[TargetEndPage];

    if MoveFromPage <> MoveFromLastEmptyPage then begin
      // There is a separate first page to rotate
      if (SourceStartSubIndex > 0) then // only true, if there is no ffff
        RotateFirst := -SourceStartSubIndex; // space needed for ---- (Moves ~~~ or >>>> out of the way)

      MovFrmPagePtr^.AdjustFirstItemOffset(RotateFirst, FPageSize.FPageSizeMask);
    end
    else
      RotateFirst := RotateLast;


    (** ----- "----" -----
     ** Restore first page
     ** ---------------- *)
    if (SourceStartSubIndex > 0) then // only true, if there is no ffff
      MovToPagePtr^.MoveRowsToOther(0, 0, SourceStartSubIndex, PageCapacity, MovFrmPagePtr);


    (** ----- ^^^^ -----
     ** Bubble down from first page / rotate
     ** ---------------- *)
    if BubbleFirstCnt > 0 then begin
      MovToPagePtr^.MoveRowsToOther(SourceStartSubIndex, PageCapacity-BubbleFirstCnt, BubbleFirstCnt, PageCapacity, MovToPagePtr-1);
    end;
    MovToPagePtr^.AdjustFirstItemOffset(RevBubbleDiff, FPageSize.FPageSizeMask);

    (** ----- ^^^^ -----
     ** Bubble down between pages
     ** ---------------- *)
    if TargetEndPage > MoveToPage then begin
      InternalBubbleEntriesDown(TargetEndPage, MoveToPage, RevBubbleDiff);
      TrgEndPagePtr^.AdjustFirstItemOffset(RevBubbleDiff, FPageSize.FPageSizeMask);
    end;


    (** ----- ==== -----
     ** Restore end of Source area
     ** ---------------- *)
    if (LastMovePage = 0) and (SourceEndSubIndex < PageCapacity) then begin
      i := SourceEndSubIndex;
      if (MovePgCnt = 1) or (TargetEndPage > MoveToPage)
      then
        i := i - RevBubbleDiff;
      if (MoveFromPage + MovePgCnt < MoveToPage) then begin
        TrgEndPagePtr^.MoveRowsToOther(i, SourceEndSubIndex,
          PageCapacity-SourceEndSubIndex, PageCapacity, MovLastPagePtr);
      end
      else
      if (MoveFromPage + MovePgCnt = MoveToPage) then begin
        // only in front of ^^  | Example 12
        c := PageCapacity - SourceEndSubIndex - BubbleFirstCnt;
        if c > 0 then begin
          TrgEndPagePtr^.MoveRowsToOther(i, SourceEndSubIndex,
            c, PageCapacity, MovLastPagePtr);
        end;
      end;
    end;


    (** ----- ____ -----
     ** Restore data from last page that was overwritten by move
     ** ---------------- *)
    if TargetEndSubIndex < PageCapacity then begin
       MovLastPagePtr^.MoveRowsToOther(
         (TargetEndSubIndex-RotateLast) and FPageSize.FPageSizeMask, TargetEndSubIndex,
         PageCapacity - TargetEndSubIndex, PageCapacity, TrgEndPagePtr);
    end;


    (** ----- >>>> -----
     ** Move rows to last page
     ** ---------------- *)
    if LastMovePage <> 0 then begin
       assert(TargetEndSubIndex >= RevBubbleDiff, 'TGenLazPagedList.InternalMoveRowsUp: TargetEndSubIndex >= RevBubbleDiff');
       if MoveToPage <= SourceEndPage then begin
         TmpPagePtr := MovFrmPagePtr;
         c := RotateFirst;
       end
       else begin
         assert(SourceEndPage <> MoveFromPage, 'TGenLazPagedList.InternalMoveRowsUp: SourceEndPage <> MoveFromPage');
         TmpPagePtr := FPages.ItemPointerFast[SourceEndPage];
         c := 0;
       end;

       assert(TargetEndSubIndex-LastPageNotMovedRows=BubbleDiff, 'TGenLazPagedList.InternalMoveRowsUp: TargetEndSubIndex-LastPageNotMovedRows=BubbleDiff');
       j := (PageCapacity - c) and FPageSize.FPageSizeMask;
       TmpPagePtr^.MoveRowsToOther(j, TargetEndSubIndex - LastPageNotMovedRows,
         LastPageNotMovedRows, PageCapacity, TrgEndPagePtr);
    end;

    (** ----- ~~~~ -----
     ** Restore data from last page that was overwritten by move
     ** ---------------- *)
    if (SourceStartSubIndex > RevBubbleDiff) then begin // only true, if there is no ffff
      j := (PageCapacity - RotateFirst) and FPageSize.FPageSizeMask;
      MovFrmPagePtr^.MoveRowsToOther(j, 0, TargetStartSubIndex, PageCapacity, MovToPagePtr);
    end;


    if SourceBeforeStartSubIndex > 0 then begin
      (** ----- ffff -----
       ** First page
       ** ---------------- *)
      assert(TargetStartSubIndex <= SourceBeforeStartSubIndex, 'TGenLazPagedList.InternalMoveRowsUp: TargetStartSubIndex <= SourceBeforeStartSubIndex');
      FPages.ItemPointerFast[SourceStartPage-1]^.MoveRowsToOther(SourceBeforeStartSubIndex, TargetStartSubIndex,
        PageCapacity - SourceBeforeStartSubIndex, PageCapacity, FPages.ItemPointerFast[TargetStartPage]);
    end;


  end
  else
  begin
    // LESS than half of each page needs to bubble

    (*
    *** Content at the end may be excluded from the PAGE-MOVES
        This happens EITHER if the last source OR if the last target page <= PageHalfCap
        Only one (or none) of those can be true.

        Example 10, 11 / Last source <= PageHalfCap
                                  *****   ********   ***
                  | -PPPPP^^ | PPPPPP^^ | L******* | ***..... |
              LL :| -PPPPP^^ | PPPPPP^^ | ******** | **L..... |
              Mov:|    ***** | -PPPP^^  | PPPPPP^^ | **L..... |
              Fin:| -        |    PPPPP | ^^PPPPPP | ^^L..... | // Bubble / Restore ----

                  | -PPPPP^^ | PPPPPP^^ | L....... | ~~~***** | ******** | ***.....
              LL :| -PPPPP^^ | PPPPPP^^ |  ....... | ...***** | ******** | **L.....
              Mov:| ~~~      |          |  ....... | -PPPPP^^ | PPPPPP^^ | **L.....
              Bbl:| -~~~     |          |  ....... |    PPPPP | ^^PPPPPP | ^^L..... // Bubble / Rotate
                                                                                    // Restore ----
              Fin:| -        |          |  ....... | ~~~PPPPP | ^^PPPPPP | ^^L.....

         LLLL Last source < PageHalfCap (CONDITION DIFFERS from the other branch)
          Dont move page for last source
          Moving entries, rather than pages saves restoring large ====
          - This will be done upfront. And the source/target range will be reduced
          - There will be no need for ====

    *   Special BubbleLastCnt when last target is <= PageHalfCap
    *   Don't move page over last target
        Example 12 / Last target <= PageHalfCap //
                  | .fff^^^^ | PPPP<<<= | .....*** | ******** | ***.....
              Mov:| .fff^^^^ | ******** | .....*** | PPPP<<<= | ***.....
              << :| .fff^^^^ | *******= | .....*** | PPPP     | <<<..... // <<< / restore ====
              Fin:| .        |        = | .....fff | ^^^^PPPP | <<<..... // Bubble / Move fff

         <<<< Last target <= PageHalfCap
          Moving entries, rather than pages saves restoring large ____
          - There will be no need for ____
          - ONLY can happen, if NOT example 10, 11


    **** Content at the start may be excluded from PAGE-MOVES
         If either first-source or first-target are <= PageHalfCap

        Example 13 / first source <= PageHalfCap
                   / first soruce idx >= PageHalfCap
                  | .....fff | PPPPPP== | ......** | *******_ |
              Mov:| .....fff | *******_ | ......** | PPPPPP== |
              Bbl:| .....fff | _     == | ......** |  PPPPPP  | // Rotate / Restore ====/ Bubble
              Fin:| .....    |       == | ......ff | fPPPPPP  | // Restore ____ / Move ffff

                  | ..... ff | PPPPP<== | ~******* | *....... |
              Mov:| ..... ff | ~******* | PPPPP<== | <....... | // <<<< / Move pages
              Bbl:| ..... ff | ~     == |    PPPPP | <....... | // Restore ====/ Bubble
              Fin:| .....    |       == | ~ffPPPPP | <....... | // Move ffff / Restore ~~~~

        Example 14 / first target <= PageHalfCap  (same as Example 12)
                   / first target idx >= PageHalfCap
                  | .fff^^^^ | PPPP<<<= | .....*** | ******** | ***.....
              Mov:| .fff^^^^ | ******** | .....*** | PPPP   = | <<<..... // Move Pages / Move <<<<
              Bbl:| .fff^^^^ | *******= | .....*** |     PPPP | <<<..... // Restore ====/ Bubble
              Fin:| .        |        = | .....fff | ^^^^PPPP | <<<..... // Move ffff

     ffff EITHER: First source < PageHalfCap  // avoid large ----
          OR:     First target < PageHalfCap  // avoid large ~~~~ (ffff includes ^^^^ of first page)

    *)

    (* ****************************************** *
     *                                            *
     *    Last Page                               *
     *                                            *
     *    If the last page has less than          *
     *    PageHalfCap entries to move             *
     *                                            *
     * ****************************************** *)

    LastPageDone := False;
    if SourceEndSubIndex <= PageHalfCap then begin
      (** ----- LLLL -----
       ** Last source page will not be moved.
       ** Instead move data into last target node. Data that goes behind the bubble
       ** ---------------- *)

      assert(TargetEndSubIndex - SourceEndSubIndex = BubbleDiff, 'TGenLazPagedList.InternalMoveRowsUp: TargetEndSubIndex - SourceEndSubIndex = BubbleDiff');
      if TargetEndPage > SourceEndPage then
        FPages.ItemPointerFast[SourceEndPage]^.MoveRowsToOther(0, BubbleDiff, SourceEndSubIndex, PageCapacity, FPages.ItemPointerFast[TargetEndPage])
      else
      if (BubbleDiff > 0) then    // make room for bubble up
        FPages.ItemPointerFast[SourceEndPage]^.MoveRowsUp(0, BubbleDiff, SourceEndSubIndex);
// TODO: known to be less than half the page InternalMove // but wrapped for bytes
        //FPages.ItemPointerFast[SourceEndPage]^.InternalMoveUp(0, BubbleDiff, SourceEndSubIndex);

      TargetEndSubIndex := BubbleDiff; // TargetEndSubIndex - SourceEndSubIndex;  // may end up zero / max = PageHalfCap;
      dec(SourceEndPage);
      SourceEndSubIndex := PageCapacity;
      LastPageDone := True;
    end;

    (* **************************** *
     *                              *
     *    Move Pages and Bubble     *
     *                              *
     * **************************** *)

    FirstMovePage := 0;
    SourceBeforeStartSubIndex := SourceStartSubIndex; // original in case of ffff
    MoveFromPage := SourceStartPage;
    MoveToPage   := TargetStartPage;
    MovePgCnt    := SourceEndPage - MoveFromPage + 1;

    (** ----- PPPP pppp LLLL -----
     ** Move full pages
     ** ---------------- *)
    if TargetStartPage > SourceStartPage then begin
      if (TargetStartSubIndex >= PageHalfCap) or    // Example 10: skip moving first page
         (SourceStartSubIndex >= PageHalfCap)
      then begin
        FirstMovePage := 1;
        inc(MoveFromPage);
        dec(MovePgCnt);
        if TargetStartSubIndex >= SourceStartSubIndex then
          inc(MoveToPage);
        SourceStartSubIndex := 0; // don't rotate
      end;
    end;

    MoveFromLastEmptyPage  := MoveFromPage + MovePgCnt - 1;
    if MoveFromLastEmptyPage >= MoveToPage then
      MoveFromLastEmptyPage  := MoveToPage-1;

    (** ----------------
     ** Rotate moved-source pages (now empty / containing "to be restored"-target data.
     ** Make space for bubble and "to be restored"-source data
        MoveFromPage           may contain  ~~~~ at start
                               may receive  ---- at start
        MoveFromLastEmptyPage  may contain  ____ at end
                               may receive  ==== at end
     ** ---------------- *)
    RotateFirst := 0;
    RotateLast  := 0;
    MovLastPagePtr := nil;
    MovFrmPagePtr  := nil;

    if MoveToPage > MoveFromPage then begin
      (** ---------- Move the pages ---------------- *)
      if (MovePgCnt > 0) then
        MovePagesUp(MoveFromPage, MoveToPage, MoveToPage + MovePgCnt - 1);

      assert(MoveFromLastEmptyPage >= 0, 'TGenLazPagedList.InternalMoveRowsUp: MoveFromLastEmptyPage >= 0');
      MovLastPagePtr := FPages.ItemPointerFast[MoveFromLastEmptyPage];
      MovFrmPagePtr := FPages.ItemPointerFast[MoveFromPage];

      if (MoveToPage > SourceEndPage) and
         (SourceEndSubIndex < PageCapacity) and // There may be ==== // If LastPageDone then ALWAYS FALSE
         (TargetEndSubIndex > PageHalfCap)      // There are NO "<<<<" / Otherwise no ____ Example 12
      then begin
        assert(MoveFromPage+MovePgCnt-1 = SourceEndPage, 'TGenLazPagedList.InternalMoveRowsUp: MoveFromPage+MovePgCnt-1 = SourceEndPage');
        RotateLast := -(PageCapacity - TargetEndSubIndex); // Make space for ==== (Move ___ out of the way)
      end;

      if (MoveFromPage = MoveFromLastEmptyPage) then begin
        RotateLast := RotateLast -SourceStartSubIndex; // Make space for ---- (Move ~~~~ out of the way)
        RotateFirst := RotateLast;
      end
      else begin
        RotateFirst := -SourceStartSubIndex; // Make space for ---- (Move ~~~~ out of the way)
        MovFrmPagePtr^.AdjustFirstItemOffset(RotateFirst, FPageSize.FPageSizeMask);
      end;

      MovLastPagePtr^.AdjustFirstItemOffset(RotateLast, FPageSize.FPageSizeMask);



      (** ----- ==== -----
       ** Restore last source page.
       ** ---------------- *)
      if (SourceEndSubIndex < PageCapacity) and // (SourceEndRestoreCnt > 0) and
         (MoveToPage > SourceEndPage)
      then begin
        c2 := PageCapacity - SourceEndSubIndex;
        if (TargetStartPage = SourceEndPage) then begin
          c := PageCapacity - Max(SourceBeforeStartSubIndex, TargetStartSubIndex);
          if c < c2 then
            FPages.ItemPointerFast[MoveToPage+MovePgCnt-1]^.MoveRowsToOther(SourceEndSubIndex, SourceEndSubIndex,
              c2 - c, PageCapacity, MovLastPagePtr);
        end
        else begin
          FPages.ItemPointerFast[MoveToPage+MovePgCnt-1]^.MoveRowsToOther(SourceEndSubIndex, SourceEndSubIndex,
            c2, PageCapacity, MovLastPagePtr);
        end;
      end;
    end;


    if not LastPageDone then begin
      TrgEndPagePtr := FPages.ItemPointerFast[TargetEndPage];
      if BubbleDiff > 0 then begin
        (** ----- <<<< -----
         ** Bubble out of the  last source page
         ** ---------------- *)
        if TargetEndSubIndex <= PageHalfCap then begin
          assert(TargetEndPage > TargetStartPage, 'TGenLazPagedList.InternalMoveRowsUp: TargetEndPage > TargetStartPage');
          assert(SourceEndPage+MoveToPage-MoveFromPage = TargetEndPage-1, 'TGenLazPagedList.InternalMoveRowsUp: SourceEndPage+MoveToPage-MoveFromPage = TargetEndPage-1');

          TmpPagePtr := TrgEndPagePtr;
          dec(TrgEndPagePtr);
          TrgEndPagePtr^.MoveRowsToOther(SourceEndSubIndex-TargetEndSubIndex, 0,
            TargetEndSubIndex, PageCapacity, TmpPagePtr);

          SourceEndSubIndex := SourceEndSubIndex - TargetEndSubIndex;
          TargetEndSubIndex := PageCapacity;
          dec(TargetEndPage);
        end;

        (** ---------------- Rotate last target page. // Make space for bubble-in of ^^^^ ---------------- *)
        if (TargetEndPage = TargetStartPage) and (TargetStartSubIndex >= BubbleDiff) then begin
          c := TargetEndSubIndex - TargetStartSubIndex;
          TrgEndPagePtr^.MoveRowsUp(TargetStartSubIndex - BubbleDiff, TargetStartSubIndex, c);
        end
        else begin
          c := TargetEndSubIndex - BubbleDiff;
          assert(c>=0, 'TGenLazPagedList.InternalMoveRowsUp: c>=0');
          if c > 0 then
            TrgEndPagePtr^.MoveRowsUp(0, BubbleDiff, c);
        end;
      end;

      (** ----- ____ -----
       ** Restore last target page.
       ** ---------------- *)
        if (MoveToPage > MoveFromPage) and (TargetEndSubIndex < PageCapacity) then begin
          assert(MovLastPagePtr<>nil, 'TGenLazPagedList.InternalMoveRowsUp: MovLastPagePtr<>nil');
          MovLastPagePtr^.MoveRowsToOther(
            TargetEndSubIndex-RotateLast, TargetEndSubIndex,
            PageCapacity-TargetEndSubIndex, PageCapacity, TrgEndPagePtr);
        end;
    end;


    MovToPagePtr := FPages.ItemPointerFast[MoveToPage];

    if (BubbleDiff > 0) and (TargetEndPage > MoveToPage) then begin
      (** ----- ^^^^ -----
       ** Bubble between pages
       ** ---------------- *)
      InternalBubbleEntriesUp(MoveToPage, TargetEndPage, BubbleDiff);
      if (FirstMovePage = 0) then begin
        if (SourceStartSubIndex = 0) then
          MovToPagePtr^.AdjustFirstItemOffset(-BubbleDiff, FPageSize.FPageSizeMask)
        else
          MovToPagePtr^.MoveRowsUp(SourceStartSubIndex, TargetStartSubIndex, PageCapacity - TargetStartSubIndex);
      end;
    end;

    (* **************************** *
     *                              *
     *    First Page                *
     *                              *
     * **************************** *)

    TrgStartPagePtr := FPages.ItemPointerFast[TargetStartPage];
    if FirstMovePage > 0 then begin // Example 13, 14
      (** ----- ffff  -----
       ** First source page was NOT moved
       ** Move the data to the target (may be split to 2 target pages)
       ** ---------------- *)

      (** -----
       ** Rotate data in lowest MOVED target page, to make room Check, there may be room already) ** *)
      if MoveToPage < TargetEndPage then
        MovToPagePtr^.AdjustFirstItemOffset(-BubbleDiff, FPageSize.FPageSizeMask);

      (** ----- ffff ----- ** *)
      SrcStartPagePtr := FPages.ItemPointerFast[SourceStartPage];

      c  := PageCapacity - SourceBeforeStartSubIndex;
      c2 := PageCapacity - TargetStartSubIndex;
      if c > c2 then begin
        SrcStartPagePtr^.MoveRowsToOther(SourceBeforeStartSubIndex, TargetStartSubIndex, c2, PageCapacity, TrgStartPagePtr);
        inc(TrgStartPagePtr);
        SrcStartPagePtr^.MoveRowsToOther(SourceBeforeStartSubIndex + c2, 0, c-c2, PageCapacity, TrgStartPagePtr);
      end
      else
        SrcStartPagePtr^.MoveRowsToOther(SourceBeforeStartSubIndex, TargetStartSubIndex, c, PageCapacity, TrgStartPagePtr);
    end

    else
    if MoveToPage > MoveFromPage
    then begin
      (** ----- "----" -----
       ** Restore data from first page that was "over moved"
       ** ---------------- *)
      assert(SourceStartSubIndex+TargetStartSubIndex<=PageCapacity, 'TGenLazPagedList.InternalMoveRowsUp: SourceStartSubIndex+TargetStartSubIndex<=PageCapacity');
      assert(TargetStartSubIndex<=PageHalfCap, 'TGenLazPagedList.InternalMoveRowsUp: TargetStartSubIndex<=PageHalfCap');

      (** ----- "----" -----
       ** Move back to start of first page ** *)
      if SourceStartSubIndex > 0 then begin
        assert(MovFrmPagePtr<>nil, 'TGenLazPagedList.InternalMoveRowsUp: MovFrmPagePtr<>nil');
        MovToPagePtr^.MoveRowsToOther(0, 0, SourceStartSubIndex, PageCapacity, MovFrmPagePtr);
      end;
    end;

    (** ----- ~~~~ -----
     ** Restore data at start of first target page
     ** ---------------- *)
    if (TargetStartPage > SourceEndPage) and (TargetStartSubIndex < PageHalfCap) then begin
// /////// the source end page would have been done in LLLL
      if TargetStartSubIndex > 0 then begin
        assert(MovFrmPagePtr<>nil, 'TGenLazPagedList.InternalMoveRowsUp: MovFrmPagePtr<>nil');
        MovFrmPagePtr^.MoveRowsToOther((0-RotateFirst) and FPageSize.FPageSizeMask, 0, TargetStartSubIndex, PageCapacity, TrgStartPagePtr);
      end;
    end;

  end;
end;

procedure TGenLazPagedList.MoveRows(AFromIndex, AToIndex, ACount: Integer);
var
  i: Integer;
begin
  if not FIndexChecker.specialize CheckMove<TGenLazPagedList>(Self, AFromIndex, AToIndex, ACount) then exit;

  assert((AFromIndex>=0) and (AToIndex>=0), 'TGenLazPagedList.MoveRows: (AFromIndex>=0) and (AToIndex>=0)');

  if AFromIndex < AToIndex then begin
    InternalMoveRowsUp(AFromIndex, AToIndex, ACount);
    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then begin
      i := AFromIndex + ACount - AToIndex;
      if i < 0 then
        i := 0;
      DoInitMem(AFromIndex, ACount - i);
    end;
  end
  else begin
    InternalMoveRowsDown(AFromIndex, AToIndex, ACount);
    if @TInitMemT.InitMem <> @TLazListAspectMemInitNone.InitMem then begin
      i := AToIndex + ACount - AFromIndex;
      if i < 0 then
        i := 0;
      DoInitMem(AFromIndex + i, ACount - i);
    end;
  end;
end;

procedure TGenLazPagedList.DebugDump;
var i : integer;
begin
  if fpages.fmem.IsAllocated then begin
    debugln(['PAGED .Dump  Pages.Capacity: ', fpages.Capacity, ', P.Count: ',fpages.fmem.Count,' -- Count:',FCount,'   FirstPageEmpty: ', FFirstPageEmpty, ': ']);
    for i := 0 to fpages.Count - 1 do FPages.ItemPointer[i]^.DebugDump;
  end
  else debugln(['PAGED .Dump NONE']);
end;

function TGenLazPagedList.IndexOf(const AnItem: TPItemT): integer;
var
  i, r: Integer;
  p: PPageType;
  FrstPgE: Cardinal;
begin
  if FCount = 0 then
    exit(-1);

  p := FPages.ItemPointerFast[0];
  FrstPgE := FFirstPageEmpty;
  r := p^.IndexOf(AnItem, FrstPgE);
  if r >= FrstPgE then
    exit(r - FrstPgE);

  Result := FPageSize.FPageSizeMask + 1 - FrstPgE;
  for i := 1 to FPages.Count - 1 do begin
    inc(p);
    r := p^.IndexOf(AnItem);
    if r >= 0 then begin
      Result := Result + r;
      if Result >= FCount then
        exit(-1);
      exit;
    end;
    //Result := Result + p^.Count;
    Result := Result + FPageSize.FPageSizeMask + 1;
  end;
  Result := -1;
end;

{ TGenLazPagedListFixedType }

function TGenLazPagedListFixedType.Get(const Index: Integer): T;
begin
  Result := ItemPointer[Index]^;
end;

procedure TGenLazPagedListFixedType.Put(const Index: Integer; AValue: T);
begin
  ItemPointer[Index]^ := AValue;
end;

function TGenLazPagedListFixedType.IndexOf(const AnItem: T): integer;
begin
  exit(IndexOf(@AnItem));
end;

{ TLazShiftList }

function TLazShiftList.GetCapacity: Integer;
begin
  Result := FListMem.Capacity;
end;

function TLazShiftList.GetCount: Integer;
begin
  Result := FListMem.Count;
end;

function TLazShiftList.GetItemPointer(const Index: Integer): Pointer;
begin
  Result := FListMem.ItemPointer[Index];
end;

procedure TLazShiftList.SetCapacity(const AValue: Integer);
begin
  FListMem.Capacity := AValue;
end;

procedure TLazShiftList.SetCount(const AValue: Integer);
begin
  if AValue > FListMem.Count then
    FListMem.InsertRows(FListMem.Count, AValue - FListMem.Count)
  else
  if AValue < FListMem.Count then
    FListMem.DeleteRows(AValue, FListMem.Count - AValue);
end;

constructor TLazShiftList.Create(const AnItemSize: Cardinal);
begin
  FListMem.Create(AnItemSize);
end;

destructor TLazShiftList.Destroy;
begin
  FListMem.Destroy;
end;

function TLazShiftList.Add(const ItemPointer: Pointer): Integer;
begin
  Result := FListMem.Count;
  FListMem.InsertRows(Result, 1);
  Move(ItemPointer^, FListMem.ItemPointerFast[Result]^, FListMem.FItemSize.ItemSize);
end;

procedure TLazShiftList.Clear;
begin
  FListMem.Capacity := 0;
end;

procedure TLazShiftList.Delete(const Index: Integer);
begin
  FListMem.DeleteRows(Index, 1);
end;

procedure TLazShiftList.Insert(const Index: Integer; ItemPointer: Pointer);
begin
  FListMem.InsertRows(Index, 1);
  Move(ItemPointer^, FListMem.ItemPointerFast[Index]^, FListMem.FItemSize.ItemSize);
end;

{ TLazShiftBufferListGen }

function TLazShiftBufferListGen.GetCapacity: Integer;
begin
  Result := FListMem.Capacity;
end;

function TLazShiftBufferListGen.Get(const Index: Integer): T;
begin
  Result := FListMem.ItemPointer[Index]^;
end;

function TLazShiftBufferListGen.GetCount: Integer;
begin
  Result := FListMem.Count;
end;

function TLazShiftBufferListGen.GetItemPointer(const Index: Integer): PT;
begin
  Result := FListMem.ItemPointer[Index];
end;

procedure TLazShiftBufferListGen.Put(const Index: Integer; AValue: T);
begin
  FListMem.ItemPointerFast[Index]^ := AValue;
end;

procedure TLazShiftBufferListGen.SetCapacity(const AValue: Integer);
begin
  FListMem.Capacity := AValue;
end;

procedure TLazShiftBufferListGen.SetCount(const AValue: Integer);
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

function TLazShiftBufferListGen.Add(const Item: T): Integer;
begin
  Result := FListMem.Count;
  FListMem.InsertRows(Result, 1);
  FListMem.ItemPointerFast[Result]^ := Item;
end;

procedure TLazShiftBufferListGen.Clear;
begin
  FListMem.Capacity := 0;
end;

procedure TLazShiftBufferListGen.Delete(const Index: Integer);
begin
  FListMem.DeleteRows(Index, 1);
end;

function TLazShiftBufferListGen.IndexOf(const Item: T): Integer;
begin
  Result := FListMem.IndexOf(Item);
end;

procedure TLazShiftBufferListGen.Insert(const Index: Integer; Item: T);
begin
  FListMem.InsertRows(Index, 1);
  FListMem.ItemPointerFast[Index]^ := Item;
end;

function TLazShiftBufferListGen.Remove(const Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

end.


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
}
unit LazListClassesBase;

{$mode objfpc}{$H+}
{$WARN 3124 off : Inlining disabled}
interface
{$IFDEF LazListClassTestCase}
  {$INLINE off}
{$ELSE}
  {$INLINE on}
  {$Optimization AUTOINLINE}
  {$Optimization REMOVEEMPTYPROCS}
  {$Optimization DFA}
  (* DEADSTORE, DEADVALUES: important when calls to empty procs are removed *)
  {$Optimization DEADSTORE} // needs DFA
  {$Optimization DEADVALUES}
  {$Optimization CONSTPROP}
  {$Optimization CSE}
  // needs speed eval
  { $Optimization USELOADMODIFYSTORE}

  { $Optimization REGVAR}
  { $Optimization STACKFRAME}
{$ENDIF}
{$IMPLICITEXCEPTIONS off}

type
  {$WriteableConst off}

  { TLazListAspectMemInitNone }

  TLazListAspectMemInitNone = object
  public
    class procedure Init(); inline; static;

    (* InitMem: First init
       - for InsertRows
       - for emptied Source area of move (old data is NOT finalized, since it moved)
    *)
    class procedure InitMem(const {%H-}AMem: Pointer; const {%H-}AnItemCount: Integer; const {%H-}AnItemSize: Cardinal); inline; static;

    (* InitMemOnce: Init once after alloc
       - Called for newly allocated mem
       - Use instead of Init, this mem will receive a call to "Init" too.
    *)
    class procedure InitMemOnce(const {%H-}AMem: Pointer; const {%H-}AnItemCount: Integer; const {%H-}AnItemSize: Cardinal); inline; static;

    (* FinalizeMem: (e.g. decrease ref count)
       - Before DeleteRows
       - Before Move for Target area, finalize old data.
       - NOT in Item[x] := / "Item[x]" uses a typed assignment of the data
       - No need to actually init/clean the underlaying memory
    *)
    class procedure FinalizeMem(const {%H-}AMem: Pointer; const {%H-}AnItemCount: Integer; const {%H-}AnItemSize: Cardinal); inline; static;

    // TODO: Maybe add...
    (* AcceptData: (e.g. increase ref count)
       - In Item[x] := // Not in ItemPointer[x]
       - No need to actually init/clean the underlaying memory
    *)
    (* ReleaseData: (e.g. decrease ref count)
       - In Item[x] := // Not in ItemPointer[x]
       - No need to actually init/clean the underlaying memory
    *)
  end;

  { TLazListAspectCapacitySimple }

  TLazListAspectCapacitySimple = object
  public
    class procedure Init(); inline; static;
    class function GrowCapacity(const ARequired, {%H-}ACurrent: Integer): Integer; inline; static;
    class function ShrinkCapacity(const ARequired, {%H-}ACurrent: Integer): Integer; inline; static;

  end;

  TLazListAspectCapacityFieldMem = class
  private type
    TCapacityOrdT = Cardinal;

    // Put into a wrapper, so it can be moved outside, as nested generics aren't allowed
    TCAWrap = class
    private type
      generic TCapacityAccessor<TMemHeaderT, TData> = object
      public type
        PMemHeaderT = ^TMemHeaderT;
      public
        class function ReadCapacity(const AMem: PMemHeaderT): Cardinal; inline; static;
        class function ReadCapacitySafe(const AMem: PMemHeaderT): Cardinal; inline; static;
        class procedure WriteCapacity(const AMem: PMemHeaderT; AValue: Cardinal); inline; static;
      end;
    end;
  end;

  { TLazListAspectRangeNoIndexCheck }

  TLazListAspectRangeNoIndexCheck = object
  public
    generic class function CheckIndex<T>(const {%H-}AList: T; const {%H-}AnIndex: Integer; var {%H-}Res: Pointer): Boolean; inline; static;
    generic class function CheckInsert<T>(const {%H-}AList: T; var {%H-}AnIndex: Integer; var {%H-}Res: Pointer): Boolean; inline; static;
    generic class function CheckDelete<T>(const {%H-}AList: T; var {%H-}AnIndex, {%H-}ACount: Integer): Boolean; inline; static;
    generic class function CheckMove<T>(const {%H-}AList: T; var {%H-}AFromIndex, {%H-}AToIndex, {%H-}ACount: Integer): Boolean; inline; static;
    generic class function CheckSwap<T>(const {%H-}AList: T; var {%H-}AnIndex1, {%H-}AnIndex2: Integer): Boolean; inline; static;
  end;

  { TLazListAspectPageSizeExp }

  TLazListAspectPageSizeExp = object
  public
    FPageSizeMask, FPageSizeExp: Cardinal;
    procedure Init(APageSizeExp: Cardinal);
  end;


  generic __Internal__TMemRecord<TCapacityMemT> = record
    FirstItemIdx: Cardinal;
    Count: Integer;
    Capacity: TCapacityMemT;
    Data: byte; // Dummy byte: The address for the first byte of data. This is a dummy field (pbyte for pointer math)
  end;

  generic TGenListClassesInternalMem<TMemRecordT> = object
  public type
    TMemRecord = TMemRecordT;
    PMemRecord = ^TMemRecord;
  public
    FMem: PMemRecord;

    function GetCount: Integer; inline;
    procedure SetCount(const AValue: Integer); inline;
  public
    procedure Init; inline;
    procedure Alloc(const AByteSize: Cardinal); inline;
    procedure Free; inline;
    function IsAllocated: Boolean; inline;
    property Count: Integer read GetCount write SetCount;
  end;


  TLazListConfig = class
  private type
    TSizeT = object
    public const
      ItemSize = 1;
    end;

    TInitMemT       = TLazListAspectMemInitNone;
    TCapacityT      = TLazListAspectCapacitySimple;
    TCapacityFieldT = TLazListAspectCapacityFieldMem;
    TIdxCheckT      = TLazListAspectRangeNoIndexCheck;
  end;


  generic __TGenLazListClassesInternalBase<_CONF_: TLazListConfig> = object
  private type
    _TCapacityOrdT  = _CONF_.TCapacityFieldT.TCapacityOrdT;
  protected type
    TMemHeaderT     = specialize __Internal__TMemRecord<_TCapacityOrdT>;
    PMemHeaderT     = TMemHeaderT;

    TSizeT             = _CONF_.TSizeT;
    TInitMemT          = _CONF_.TInitMemT;
    TCapacityT         = _CONF_.TCapacityT;
    TCapacityAccessorT = _CONF_.TCapacityFieldT.TCAWrap.specialize TCapacityAccessor<TMemHeaderT, _CONF_.TCapacityFieldT>;
    TIdxCheckT         = _CONF_.TIdxCheckT;
    TLazListClassesInternalMem = specialize TGenListClassesInternalMem<TMemHeaderT>;
  end;

  TLazListPageConfig = class
  private type
    TPageSizeT = TLazListAspectPageSizeExp;
    TPageCapacityFieldT = TLazListAspectCapacityFieldMem;
  end;

  generic __TGenLazPagedListInternalBase<_TConfT: TLazListConfig; _TPageConfT: TLazListPageConfig> = object(specialize __TGenLazListClassesInternalBase<_TConfT>)
  protected type
    TPageSizeT          = _TPageConfT.TPageSizeT;
    TPageCapacityFieldT = _TPageConfT.TPageCapacityFieldT;
  end;


implementation

{ TLazListAspectMemInitNone }

class procedure TLazListAspectMemInitNone.Init();
begin
  //
end;

class procedure TLazListAspectMemInitNone.InitMem(const AMem: Pointer;
  const AnItemCount: Integer; const AnItemSize: Cardinal);
begin
  //
end;

class procedure TLazListAspectMemInitNone.InitMemOnce(const AMem: Pointer;
  const AnItemCount: Integer; const AnItemSize: Cardinal);
begin
  //
end;

class procedure TLazListAspectMemInitNone.FinalizeMem(const AMem: Pointer;
  const AnItemCount: Integer; const AnItemSize: Cardinal);
begin
  //
end;

{ TLazListAspectCapacitySimple }

class procedure TLazListAspectCapacitySimple.Init();
begin
  // nothing
end;

class function TLazListAspectCapacitySimple.GrowCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  Result := ARequired;
end;

class function TLazListAspectCapacitySimple.ShrinkCapacity(const ARequired, ACurrent: Integer
  ): Integer;
begin
  Result := ARequired;
end;

class function TLazListAspectCapacityFieldMem.TCAWrap.TCapacityAccessor.ReadCapacity(const AMem: PMemHeaderT): Cardinal;
begin
  Result := AMem^.Capacity;
end;

class function TLazListAspectCapacityFieldMem.TCAWrap.TCapacityAccessor.ReadCapacitySafe(const AMem: PMemHeaderT): Cardinal;
begin
  if AMem = nil then
    Result := 0
  else
    Result := AMem^.Capacity;
end;

class procedure TLazListAspectCapacityFieldMem.TCAWrap.TCapacityAccessor.WriteCapacity(const AMem: PMemHeaderT; AValue: Cardinal);
begin
  AMem^.Capacity := AValue;
end;

{ TLazListAspectRangeNoIndexCheck }

generic class function TLazListAspectRangeNoIndexCheck.CheckIndex<T>(const AList: T;
  const AnIndex: Integer; var Res: Pointer): Boolean;
begin
  Result := True;
end;

generic class function TLazListAspectRangeNoIndexCheck.CheckInsert<T>(const AList: T;
  var AnIndex: Integer; var Res: Pointer): Boolean;
begin
  Result := True;
end;

generic class function TLazListAspectRangeNoIndexCheck.CheckDelete<T>(const AList: T;
  var AnIndex, ACount: Integer): Boolean;
begin
  Result := True;
end;

generic class function TLazListAspectRangeNoIndexCheck.CheckMove<T>(const AList: T;
  var AFromIndex, AToIndex, ACount: Integer): Boolean;
begin
  Result := True;
end;

generic class function TLazListAspectRangeNoIndexCheck.CheckSwap<T>(const AList: T; var AnIndex1,
  AnIndex2: Integer): Boolean;
begin
  Result := True;
end;

{ TLazListAspectPageSizeExp }

procedure TLazListAspectPageSizeExp.Init(APageSizeExp: Cardinal);
begin
    FPageSizeExp := APageSizeExp;
    FPageSizeMask := Integer(not(Cardinal(-1) << APageSizeExp));
end;

{ TGenListClassesInternalMem }

function TGenListClassesInternalMem.GetCount: Integer;
begin
  if FMem = nil
  then Result := 0
  else Result := FMem^.Count;
end;

procedure TGenListClassesInternalMem.SetCount(const AValue: Integer);
begin
  assert(FMem <> nil, 'TGenListClassesInternalMem.SetCount: FMem <> nil');
  FMem^.Count := AValue;
end;

procedure TGenListClassesInternalMem.Init;
begin
  FMem := nil;
end;

procedure TGenListClassesInternalMem.Alloc(const AByteSize: Cardinal);
begin
  Free;
  FMem := Getmem(PtrUInt(@PMemRecord(nil)^.Data) + AByteSize);
end;

procedure TGenListClassesInternalMem.Free;
begin
  if FMem <> nil then
    Freemem(FMem);
  FMem := nil;
end;

function TGenListClassesInternalMem.IsAllocated: Boolean;
begin
  Result := FMem <> nil;
end;

end.


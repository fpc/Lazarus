{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

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
unit LazEditLineItemLists;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  SysUtils, LazListClasses, LazListClassesBase;

type
  TLazEditChildLineItemsList = class;

  { TLazEditLineItems }

  TLazEditLineItems = class
  protected
    function GetCount: Integer; virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    procedure SetCapacity(const AValue: Integer); virtual;

    procedure TextChanged(AnIndex, ACount: Integer); virtual;  // for children
  public
    procedure Insert(AnIndex, ACount: Integer); virtual;
    procedure Delete(AnIndex, ACount: Integer); virtual;
    procedure Move(AFromIndex, AToIndex, ACount: Integer); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
  end;


  { TLazEditChildLineItemsList }

  TLazEditChildLineItemsList = class
  private type
    TChildEntry = record
        ID: Pointer;
        Child: TLazEditLineItems;
      end;
    PChildEntry = ^TChildEntry;
    TChildrenList = specialize TGenLazShiftListFixedSize<TChildEntry, TLazListConfigFixSize_0>;
  private
    FChildren: TChildrenList;
    function GetChildren(AnIndex: Integer): TLazEditLineItems; inline;
    function GetChildrenByID(AnId: Pointer): TLazEditLineItems;
    procedure SetChildrenByID(AnId: Pointer; AValue: TLazEditLineItems);
  public
    constructor Create;
    destructor Destroy; override;

    (* During any Child operation there must be no calls to add/remove children from the list *)
    procedure ChildrenTextChanged(AnIndex, ACount: Integer);
    procedure ChildrenInsert(AnIndex, ACount: Integer);
    procedure ChildrenDelete(AnIndex, ACount: Integer);
    procedure ChildrenMove(AFromIndex, AToIndex, ACount: Integer);
    procedure ChildrenSetCapacity(const AValue: Integer);

  public
    (* New Children are added by assigning them to a new ID *)
    function Count: integer; inline;
    property Children[AnIndex: Integer]: TLazEditLineItems read GetChildren;
    property ChildrenByID[AnId: Pointer]: TLazEditLineItems read GetChildrenByID write SetChildrenByID; default;
  end;


  TWrapGenBaseLazEditLineItems = class
  private type
    TBase = TLazEditLineItems;
  end;

  (* TGenLazEditParentLineItems
     ***  Must be most outer wrapper ***
     ==>  Then "inherited Insert()" will have ManagerLock set
  *)

  generic TGenLazEditParentLineItems<_B: TWrapGenBaseLazEditLineItems> = class(_B.TBase)
  private
    FManagerLockCount: integer;
    FManagedList: TLazEditChildLineItemsList;
    function GetManagerLocked: boolean; inline;
  protected
    procedure SetCapacity(const AValue: Integer); override;
    procedure CallLineTextChanged(AnIndex: Integer; ACount: Integer = 1); inline;

    procedure IncManagerLock; inline;
    procedure DecManagerLock; inline;
    property  ManagerLocked: boolean read GetManagerLocked;
    property ManagedList: TLazEditChildLineItemsList read FManagedList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert(AnIndex, ACount: Integer); override;
    procedure Delete(AnIndex, ACount: Integer); override;
    procedure Move(AFromIndex, AToIndex, ACount: Integer); override;
  end;

  { TGenLazEditLineItems }

  generic TGenLazEditLineItems<_B: TWrapGenBaseLazEditLineItems; _ListType> = class(_B.TBase)
  private type
    __BASE = _B.TBase;
    TLineItemList = _ListType;
  private
    FLineItems: TLineItemList;
    function GetItemPointer(AnIndex: Integer): Pointer; inline;
  protected
    function GetCount: Integer; override;
    function GetCapacity: Integer; override;
    procedure SetCapacity(const AValue: Integer); override;

    property ItemPointer[AnIndex: Integer]: Pointer read GetItemPointer;
  public
    destructor Destroy; override;
    procedure Insert(AnIndex, ACount: Integer); override;
    procedure Delete(AnIndex, ACount: Integer); override;
    procedure Move(AFromIndex, AToIndex, ACount: Integer); override;
  end;

  { TGenLazEditLineItemsVarSize }

  generic TGenLazEditLineItemsVarSize<_B: TWrapGenBaseLazEditLineItems; _ListType> = class(specialize TGenLazEditLineItems<_B, _ListType>)
  private
    function GetItemSize: Integer; inline;
    procedure SetItemSize(AValue: Integer);
  protected
    property ItemSize: Integer read GetItemSize write SetItemSize;
  public
    constructor Create; experimental;
    constructor Create(const AnItemSize: Cardinal);
  end;

  generic TGenLazEditLineItemsVarSizeShiftList<_TInitMemT> = class(
    specialize TGenLazEditLineItemsVarSize<
      TWrapGenBaseLazEditLineItems,
      specialize TGenLazShiftListVarSize<
        Pointer,
        {$IFDEF AssertSynMemIndex}
        specialize TGenListConfigVarSize_3<_TInitMemT, TLazListAspectCapacityExp0x8000, TLazListAspectRangeIndexCheckExcept>
        {$ELSE}
        specialize TGenListConfigVarSize_3<_TInitMemT, TLazListAspectCapacityExp0x8000, TLazListAspectRangeNoIndexCheck>
        {$ENDIF}
      >
    >
  )
  end;

  TLazEditLineItemsShiftList = class(
    specialize TGenLazEditLineItemsVarSizeShiftList<TLazListAspectMemInitNone>
  )
  end;

  { TGenLazEditLineItemsFixSize }

  generic TGenLazEditLineItemsFixSize<_B: TWrapGenBaseLazEditLineItems; _ListType> = class(specialize TGenLazEditLineItems<_B, _ListType>)
  public
    constructor Create;
  end;

  { TGenLazEditLineItemsFixedShiftList }

  generic TGenLazEditLineItemsFixedShiftList<T, _TInitMemT> = class(
    specialize TGenLazEditLineItemsFixSize<
      TWrapGenBaseLazEditLineItems,
      specialize TGenLazShiftListFixedSize<
        T,
        {$IFDEF AssertSynMemIndex}
        specialize TGenListConfigFixSize_3<_TInitMemT, TLazListAspectCapacityExp0x8000, TLazListAspectRangeIndexCheckExcept>
        {$ELSE}
        specialize TGenListConfigFixSize_3<_TInitMemT, TLazListAspectCapacityExp0x8000, TLazListAspectRangeNoIndexCheck>
        {$ENDIF}
      >
    >
  )
  protected type
    PTItem = ^T;
  private
    function GetItemPointer(AnIndex: Integer): PTItem; inline;
  public
    property ItemPointer[AnIndex: Integer]: PTItem read GetItemPointer;
  end;

implementation

{ TLazEditChildLineItemsList }

function TLazEditChildLineItemsList.GetChildren(AnIndex: Integer): TLazEditLineItems;
begin
  Result := FChildren.ItemPointer[AnIndex]^.Child;
end;

function TLazEditChildLineItemsList.GetChildrenByID(AnId: Pointer): TLazEditLineItems;
var
  c: Integer;
  p: PChildEntry;
begin
  Result := nil;
  c := Count;
  if c = 0 then
    exit;

  p := FChildren.ItemPointer[0];
  while c > 0 do begin
    if p^.ID = AnId then begin
      Result := p^.Child;
      exit;
    end;

    inc(p);
    dec(c);
  end;
end;

procedure TLazEditChildLineItemsList.SetChildrenByID(AnId: Pointer; AValue: TLazEditLineItems
  );
var
  c: Integer;
  p: PChildEntry;
begin
  c := Count;
  if c > 0 then begin
    p := FChildren.ItemPointer[0];
    while c > 0 do begin
      if p^.ID = AnId then begin
        if AValue = nil then
          FChildren.DeleteRows(Count - c, 1)
        else
          p^.Child := AValue;
        exit;
      end;

      inc(p);
      dec(c);
    end;
  end;

  if AValue = nil then // nothing to delete
    exit;

  c := Count;
  FChildren.InsertRows(c, 1);
  p := FChildren.ItemPointer[c];
  p^.ID    := AnId;
  p^.Child := AValue;
end;

constructor TLazEditChildLineItemsList.Create;
begin
  FChildren.Create;
end;

destructor TLazEditChildLineItemsList.Destroy;
begin
  inherited Destroy;
  FChildren.Destroy;
end;

procedure TLazEditChildLineItemsList.ChildrenTextChanged(AnIndex, ACount: Integer);
var
  c: Integer;
  p: PChildEntry;
begin
  c := Count;
  if c = 0 then
    exit;

  p := FChildren.ItemPointer[0];
  while c > 0 do begin
    p^.Child.TextChanged(AnIndex, ACount);
    inc(p);
    dec(c);
  end;
end;

procedure TLazEditChildLineItemsList.ChildrenInsert(AnIndex, ACount: Integer);
var
  c: Integer;
  p: PChildEntry;
begin
  c := Count;
  if c = 0 then
    exit;

  p := FChildren.ItemPointer[0];
  while c > 0 do begin
    p^.Child.Insert(AnIndex, ACount);
    inc(p);
    dec(c);
  end;
end;

procedure TLazEditChildLineItemsList.ChildrenDelete(AnIndex, ACount: Integer);
var
  c: Integer;
  p: PChildEntry;
begin
  c := Count;
  if c = 0 then
    exit;

  p := FChildren.ItemPointer[0];
  while c > 0 do begin
    p^.Child.Delete(AnIndex, ACount);
    inc(p);
    dec(c);
  end;
end;

procedure TLazEditChildLineItemsList.ChildrenMove(AFromIndex, AToIndex, ACount: Integer);
var
  c: Integer;
  p: PChildEntry;
begin
  c := Count;
  if c = 0 then
    exit;

  p := FChildren.ItemPointer[0];
  while c > 0 do begin
    p^.Child.Move(AFromIndex, AToIndex, ACount);
    inc(p);
    dec(c);
  end;
end;

procedure TLazEditChildLineItemsList.ChildrenSetCapacity(const AValue: Integer);
var
  c: Integer;
  p: PChildEntry;
begin
  c := Count;
  if c = 0 then
    exit;

  p := FChildren.ItemPointer[0];
  while c > 0 do begin
    p^.Child.Capacity := AValue;
    inc(p);
    dec(c);
  end;
end;

function TLazEditChildLineItemsList.Count: integer;
begin
  Result := FChildren.Count;
end;

{ TLazEditLineItems }

procedure TLazEditLineItems.SetCapacity(const AValue: Integer);
begin
  // empty
end;

procedure TLazEditLineItems.TextChanged(AnIndex, ACount: Integer);
begin
  // empty
end;

procedure TLazEditLineItems.Insert(AnIndex, ACount: Integer);
begin
  // empty
end;

procedure TLazEditLineItems.Delete(AnIndex, ACount: Integer);
begin
  // empty
end;

procedure TLazEditLineItems.Move(AFromIndex, AToIndex, ACount: Integer);
begin
  // empty
end;

{ TGenLazEditParentLineItems }

function TGenLazEditParentLineItems.GetManagerLocked: boolean;
begin
  Result := FManagerLockCount <> 0;
end;

procedure TGenLazEditParentLineItems.SetCapacity(const AValue: Integer);
begin
  IncManagerLock;
  inherited SetCapacity(AValue);
  DecManagerLock;
  if not ManagerLocked then begin
    FManagedList.ChildrenSetCapacity(AValue);
  end;
end;

procedure TGenLazEditParentLineItems.CallLineTextChanged(AnIndex: Integer; ACount: Integer);
begin
  if not ManagerLocked then
    FManagedList.ChildrenTextChanged(AnIndex, ACount);
end;

procedure TGenLazEditParentLineItems.IncManagerLock;
begin
  inc(FManagerLockCount);
end;

procedure TGenLazEditParentLineItems.DecManagerLock;
begin
  dec(FManagerLockCount);
end;

constructor TGenLazEditParentLineItems.Create;
begin
  FManagedList := TLazEditChildLineItemsList.Create;
  inherited Create;
end;

destructor TGenLazEditParentLineItems.Destroy;
begin
  inherited Destroy;
  FManagedList.Free;
end;

procedure TGenLazEditParentLineItems.Insert(AnIndex, ACount: Integer);
begin
  IncManagerLock;
  inherited Insert(AnIndex, ACount);
  DecManagerLock;
  if not ManagerLocked then
    FManagedList.ChildrenInsert(AnIndex, ACount);
end;

procedure TGenLazEditParentLineItems.Delete(AnIndex, ACount: Integer);
begin
  IncManagerLock;
  inherited Delete(AnIndex, ACount);
  DecManagerLock;
  if not ManagerLocked then
    FManagedList.ChildrenDelete(AnIndex, ACount);
end;

procedure TGenLazEditParentLineItems.Move(AFromIndex, AToIndex, ACount: Integer);
begin
  IncManagerLock;
  inherited Move(AFromIndex, AToIndex, ACount);
  DecManagerLock;
  if not ManagerLocked then
    FManagedList.ChildrenMove(AFromIndex, AToIndex, ACount);
end;

{ TGenLazEditLineItems }

function TGenLazEditLineItems.GetItemPointer(AnIndex: Integer): Pointer;
begin
  Result := FLineItems.ItemPointer[AnIndex];
end;

function TGenLazEditLineItems.GetCount: Integer;
begin
  Result := FLineItems.Count;
end;

function TGenLazEditLineItems.GetCapacity: Integer;
begin
  Result := FLineItems.Capacity;
end;

procedure TGenLazEditLineItems.SetCapacity(const AValue: Integer);
begin
  FLineItems.Capacity := AValue;
end;

procedure TGenLazEditLineItems.Move(AFromIndex, AToIndex, ACount: Integer);
begin
  FLineItems.MoveRows(AFromIndex, AToIndex, ACount);
  if @__BASE.Move <> @TLazEditLineItems.Move then
    inherited Move(AFromIndex, AToIndex, ACount);
end;

destructor TGenLazEditLineItems.Destroy;
begin
  inherited Destroy;
  FLineItems.Destroy;
end;

procedure TGenLazEditLineItems.Insert(AnIndex, ACount: Integer);
begin
  FLineItems.InsertRows(AnIndex, ACount);
  if @__BASE.Insert <> @TLazEditLineItems.Insert then
    inherited Insert(AnIndex, ACount);
end;

procedure TGenLazEditLineItems.Delete(AnIndex, ACount: Integer);
begin
  FLineItems.DeleteRows(AnIndex, ACount);
  if @__BASE.Delete <> @TLazEditLineItems.Delete then
    inherited Delete(AnIndex, ACount);
end;

{ TGenLazEditLineItemsVarSize }

function TGenLazEditLineItemsVarSize.GetItemSize: Integer;
begin
  Result := FLineItems.ItemSize;
end;

procedure TGenLazEditLineItemsVarSize.SetItemSize(AValue: Integer);
begin
  if (FLineItems.Capacity <> 0) then raise Exception.Create('Not allowe dto change ItemSize');
  FLineItems.Destroy;
  FLineItems.Create(AValue);
end;

constructor TGenLazEditLineItemsVarSize.Create;
begin
  FLineItems.Create(0);
  inherited Create;
end;

constructor TGenLazEditLineItemsVarSize.Create(const AnItemSize: Cardinal);
begin
  FLineItems.Create(AnItemSize);
  inherited Create;
end;

{ TGenLazEditLineItemsFixSize }

constructor TGenLazEditLineItemsFixSize.Create;
begin
  FLineItems.Create;
  inherited Create;
end;

{ TGenLazEditLineItemsFixedShiftList }

function TGenLazEditLineItemsFixedShiftList.GetItemPointer(AnIndex: Integer): PTItem;
begin
  Result := FLineItems.ItemPointer[AnIndex];
end;

end.


unit DbgIntfMiscClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazClasses;

type

  (* TDbgEntityValue are created with a refcount of 0 (zero)
  *)

  TDbgEntityValuesList = class;
  TDbgEntitiesThreadStackList = class;

  { TDbgEntityValue
    Locals, Watches, Registers
  }

  TDbgEntityValue = class(TRefCountedObject)
  private
    FOwner: TDbgEntityValuesList;
    FFlags: set of (devImmutable);
    function GetImmutable: Boolean;
    function GetStackFrame: Integer;
    function GetThreadId: Integer;
    procedure SetImmutable(AValue: Boolean);
  protected
    procedure DoAssign({%H-}AnOther: TDbgEntityValue); virtual;
    property Owner: TDbgEntityValuesList read FOwner;
  public
    procedure Assign({%H-}AnOther: TDbgEntityValue);
    property ThreadId: Integer read GetThreadId;
    property StackFrame: Integer read GetStackFrame;
    property Immutable: Boolean read GetImmutable write SetImmutable; // mainly used by assert
  end;

  { TDbgEntityValuesList
    All Values for a specifer Thread/StackFrame
  }

  TDbgEntityValuesList = class(TRefCountedObject)
  private
    FStackFrame: Integer;
    FThreadId: Integer;
    FFlags: set of (devlImmutable);
    FList: TRefCntObjList;
    FOwner: TDbgEntitiesThreadStackList;
    function GetEntry(AnIndex: Integer): TDbgEntityValue;
    function GetImmutable: Boolean;
    procedure SetImmutable(AValue: Boolean);
  protected
    function  CreateEntry: TDbgEntityValue; virtual; abstract;
    procedure DoAssign(AnOther: TDbgEntityValuesList); virtual;      // assert other has same thread/stack
    procedure DoAssignListContent(AnOther: TDbgEntityValuesList); virtual;      // assert other has same thread/stack
    procedure DoCleared; virtual;
    procedure DoAdded({%H-}AnEntry: TDbgEntityValue); virtual;
    procedure Init; virtual;
    property Owner: TDbgEntitiesThreadStackList read FOwner;
  public
    constructor Create(AThreadId, AStackFrame: Integer);
    destructor Destroy; override;
    procedure Assign(AnOther: TDbgEntityValuesList);       // assert other has same thread/stack
    procedure Add(AnEntry: TDbgEntityValue);
    procedure Clear;
    function Count: Integer;
    property Entries[AnIndex: Integer]: TDbgEntityValue read GetEntry;

    property ThreadId: Integer read FThreadId;
    property StackFrame: Integer read FStackFrame;
    property Immutable: Boolean read GetImmutable write SetImmutable; // mainly used by assert
  end;

  TDbgValuesThreadList = record
    ThreadId: Integer;
    List: TRefCntObjList;
  end;

  { TDbgEntitiesThreadStackList }

  TDbgEntitiesThreadStackList = class(TRefCountedObject)
  private
    FList: array of TDbgValuesThreadList;
    FFlags: set of (devtsImmutable);
    function GetEntry(AThreadId, AStackFrame: Integer): TDbgEntityValuesList;
    function GetEntryByIdx(AnIndex: Integer): TDbgEntityValuesList;
    function GetHasEntry(AThreadId, AStackFrame: Integer): Boolean;
    function GetImmutable: Boolean;
    function IndexOfThread(AThreadId: Integer; ACreateSubList: Boolean = False): Integer;
    procedure SetImmutable(AValue: Boolean);
  protected
    function  CreateEntry(AThreadId, AStackFrame: Integer): TDbgEntityValuesList; virtual; abstract;
    procedure DoAssign(AnOther: TDbgEntitiesThreadStackList); virtual;
    procedure DoAssignListContent(AnOther: TDbgEntitiesThreadStackList); virtual;
    procedure DoCleared; virtual;
    procedure DoAdded({%H-}AnEntry: TDbgEntityValuesList); virtual;
  public
    destructor Destroy; override;
    procedure Assign(AnOther: TDbgEntitiesThreadStackList);
    procedure Add(AnEntry: TDbgEntityValuesList);
    procedure Clear;
    function Count: Integer;
    property EntriesByIdx[AnIndex: Integer]: TDbgEntityValuesList read GetEntryByIdx;
    // Entries will automatically be created
    property Entries[AThreadId, AStackFrame: Integer]: TDbgEntityValuesList read GetEntry; default;
    property HasEntry[AThreadId, AStackFrame: Integer]: Boolean read GetHasEntry;
    property Immutable: Boolean read GetImmutable write SetImmutable; // used by assert
  end;

  { TDelayedUdateItem }

  TDelayedUdateItem = class(TCollectionItem)
  private
    FUpdateCount: Integer;
    FDoChanged: Boolean;
  protected
    procedure Changed;
    procedure DoChanged; virtual;
    procedure DoEndUpdate; virtual; // even if not changed
  public
    procedure Assign(ASource: TPersistent); override;
    procedure BeginUpdate; virtual;
    constructor Create(ACollection: TCollection); override;
    procedure EndUpdate;
    function IsUpdating: Boolean;
  end;

  { TRefCountedColectionItem }

  TRefCountedColectionItem = class(TDelayedUdateItem)
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;
    procedure AddReference;
    procedure ReleaseReference;
  private
    FRefCount: Integer;
  protected
    procedure DoFree; virtual;
    property  RefCount: Integer read FRefCount;
  end;

procedure ReleaseRefAndNil(var ARefCountedObject);

implementation

procedure ReleaseRefAndNil(var ARefCountedObject);
begin
  Assert( (Pointer(ARefCountedObject) = nil) or
          (TObject(ARefCountedObject) is TRefCountedObject) or
          (TObject(ARefCountedObject) is TRefCountedColectionItem),
         'ReleaseRefAndNil requires TRefCountedObject');

  if Pointer(ARefCountedObject) = nil then
    exit;

  if (TObject(ARefCountedObject) is TRefCountedObject) then
    TRefCountedObject(ARefCountedObject).ReleaseReference
  else
  if (TObject(ARefCountedObject) is TRefCountedColectionItem) then
    TRefCountedColectionItem(ARefCountedObject).ReleaseReference;

  Pointer(ARefCountedObject) := nil;
end;

{ TDbgEntityValue }

function TDbgEntityValue.GetImmutable: Boolean;
begin
  Result := (devImmutable in FFlags) or ((FOwner <> nil) and FOwner.Immutable);
end;

function TDbgEntityValue.GetStackFrame: Integer;
begin
  Result := FOwner.StackFrame;
end;

function TDbgEntityValue.GetThreadId: Integer;
begin
  Result := FOwner.ThreadId;
end;

procedure TDbgEntityValue.SetImmutable(AValue: Boolean);
begin
  assert((AValue = True) or not(Immutable), 'TDbgEntityValue.SetImmutable Not allowed to set to false');
  if AValue then Include(FFlags, devImmutable);
end;

procedure TDbgEntityValue.DoAssign(AnOther: TDbgEntityValue);
begin
  //
end;

procedure TDbgEntityValue.Assign(AnOther: TDbgEntityValue);
begin
  Assert(not Immutable, 'TDbgEntityValue.Assign Immutable');
  DoAssign(AnOther);
end;

{ TDbgEntityValuesList }

function TDbgEntityValuesList.GetImmutable: Boolean;
begin
  Result := devlImmutable in FFlags;
end;

function TDbgEntityValuesList.GetEntry(AnIndex: Integer): TDbgEntityValue;
begin
  Result := TDbgEntityValue(FList[AnIndex]);
end;

procedure TDbgEntityValuesList.SetImmutable(AValue: Boolean);
begin
  assert((AValue = True) or not(devlImmutable in FFlags), 'TDbgEntityValuesList.SetImmutable Not allowed to set to false');
  if AValue then Include(FFlags, devlImmutable);
end;

procedure TDbgEntityValuesList.DoCleared;
begin
  //
end;

procedure TDbgEntityValuesList.DoAdded(AnEntry: TDbgEntityValue);
begin

end;

procedure TDbgEntityValuesList.Init;
begin
  //
end;

constructor TDbgEntityValuesList.Create(AThreadId, AStackFrame: Integer);
begin
  inherited Create;
  FFlags := [];
  FThreadId   := AThreadId;
  FStackFrame := AStackFrame;
  FList := TRefCntObjList.Create;
  Init;
end;

destructor TDbgEntityValuesList.Destroy;
begin
  Exclude(FFlags, devlImmutable);
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDbgEntityValuesList.DoAssign(AnOther: TDbgEntityValuesList);
begin
  DoAssignListContent(AnOther);
end;

procedure TDbgEntityValuesList.DoAssignListContent(AnOther: TDbgEntityValuesList);
var
  e: TDbgEntityValue;
  i: Integer;
begin
  for i := 0 to AnOther.FList.Count - 1 do begin
    e := CreateEntry;
    e.FOwner := Self;
    e.Assign(TDbgEntityValue(AnOther.FList[i]));
    FList.Add(e);
  end;
end;

procedure TDbgEntityValuesList.Assign(AnOther: TDbgEntityValuesList);
begin
  Assert(not Immutable, 'TDbgEntityValuesList.Assign Immutable');
  Assert((FThreadId = AnOther.FThreadId) and (FStackFrame = AnOther.FStackFrame), 'TDbgEntityValuesList.Assign same thread and stack');

  Clear;
  DoAssign(AnOther);
end;

procedure TDbgEntityValuesList.Add(AnEntry: TDbgEntityValue);
begin
  Assert(not Immutable, 'TDbgEntityValuesList.Add  Immutable');
  AnEntry.FOwner := Self;
  FList.Add(AnEntry);
  DoAdded(AnEntry);
end;

procedure TDbgEntityValuesList.Clear;
var
  i: Integer;
begin
  Assert(not Immutable, 'TDbgEntityValuesList.Clear Immutable');
  if FList.Count = 0 then
    exit;
  for i := 0 to FList.Count - 1 do
    TDbgEntityValue(FList[i]).FOwner := nil;
  FList.Clear;
  DoCleared;
end;

function TDbgEntityValuesList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TDbgEntitiesThreadStackList }

function TDbgEntitiesThreadStackList.GetEntry(AThreadId, AStackFrame: Integer): TDbgEntityValuesList;
var
  i, j: Integer;
begin
  i := IndexOfThread(AThreadId);
  if i >= 0 then begin
    // TODO: binary search / need sorted list
    for j := 0 to FList[i].List.Count - 1 do begin
      Result := TDbgEntityValuesList(FList[i].List[j]);
      if Result.StackFrame = AStackFrame then
        exit;
    end;
  end;

  if Immutable then begin
    Result := nil;
    exit;
  end;

  Result := CreateEntry(AThreadId, AStackFrame);
  Add(Result);
end;

function TDbgEntitiesThreadStackList.GetEntryByIdx(AnIndex: Integer): TDbgEntityValuesList;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while AnIndex >= FList[i].List.Count do begin
    dec(AnIndex, FList[i].List.Count);
    inc(i);
    if i >= Length(FList) then
      exit;
  end;
  Result := TDbgEntityValuesList(FList[i].List[AnIndex]);
end;

function TDbgEntitiesThreadStackList.GetHasEntry(AThreadId, AStackFrame: Integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  i := IndexOfThread(AThreadId);
  if i < 0 then exit;
  // TODO: binary search / need sorted list
  for j := 0 to FList[i].List.Count - 1 do begin
    if TDbgEntityValuesList(FList[i].List[j]).StackFrame = AStackFrame then begin
      Result := True;
      exit;
    end;
  end;
end;

function TDbgEntitiesThreadStackList.GetImmutable: Boolean;
begin
  Result := devtsImmutable in FFlags;
end;

function TDbgEntitiesThreadStackList.IndexOfThread(AThreadId: Integer;
  ACreateSubList: Boolean): Integer;
begin
  Result := length(FList) - 1;
  while (Result >= 0) and (FList[Result].ThreadId <> AThreadId) do
    dec(Result);
  if (Result >= 0) or (not ACreateSubList) then
    exit;

  Result := length(FList);
  SetLength(FList, Result + 1);
  FList[Result].ThreadId := AThreadId;
  FList[Result].List := TRefCntObjList.Create;
end;

procedure TDbgEntitiesThreadStackList.SetImmutable(AValue: Boolean);
begin
  assert((AValue = True) or not(devtsImmutable in FFlags), 'TDbgEntityValuesList.SetImmutable Not allowed to set to false');
  if AValue then Include(FFlags, devtsImmutable);
end;

procedure TDbgEntitiesThreadStackList.DoCleared;
begin
  //
end;

procedure TDbgEntitiesThreadStackList.DoAdded(AnEntry: TDbgEntityValuesList);
begin
  //
end;

destructor TDbgEntitiesThreadStackList.Destroy;
begin
  Exclude(FFlags, devtsImmutable);
  Clear;
  inherited Destroy;
end;

procedure TDbgEntitiesThreadStackList.DoAssign(AnOther: TDbgEntitiesThreadStackList);
begin
  DoAssignListContent(AnOther);
end;

procedure TDbgEntitiesThreadStackList.DoAssignListContent(AnOther: TDbgEntitiesThreadStackList);
var
  i, j: Integer;
  t: Integer;
  e, o: TDbgEntityValuesList;
begin
  SetLength(FList, length(AnOther.FList));
  for i := 0 to Length(FList) - 1 do begin
    t := AnOther.FList[i].ThreadId;
    FList[i].ThreadId := t;
    FList[i].List := TRefCntObjList.Create;
    for j := 0 to AnOther.FList[i].List.Count - 1 do begin
      o := TDbgEntityValuesList(AnOther.FList[i].List[j]);
      e := CreateEntry(t, o.StackFrame);
      e.FOwner := Self;
      e.Assign(o);
      FList[i].List.Add(e);
    end;
  end;
end;

procedure TDbgEntitiesThreadStackList.Assign(AnOther: TDbgEntitiesThreadStackList);
begin
  Assert(not Immutable, 'TDbgEntitiesThreadStackList.Assign Immutable');
  Clear;
  DoAssign(AnOther);
end;

procedure TDbgEntitiesThreadStackList.Add(AnEntry: TDbgEntityValuesList);
var
  i: Integer;
begin
  Assert(not Immutable, 'TDbgEntitiesThreadStackList.Add Immutable');
  Assert((AnEntry.FOwner = nil) or (AnEntry.FOwner = Self), 'TDbgEntitiesThreadStackList.Add Entry.FThreadStackList');
  AnEntry.FOwner := Self;
  i := IndexOfThread(AnEntry.ThreadId, True);
  FList[i].List.Add(AnEntry);
  DoAdded(AnEntry);
end;

procedure TDbgEntitiesThreadStackList.Clear;
var
  i, j: Integer;
begin
  Assert(not Immutable, 'TDbgEntitiesThreadStackList.Clear Immutable');
  if Length(FList) = 0 then
    exit;
  for i := 0 to Length(FList) - 1 do begin
    for j := 0 to FList[i].List.Count - 1 do
      TDbgEntityValuesList(FList[i].List[j]).FOwner := nil;
    FList[i].List.Free;
  end;
  SetLength(FList, 0);
  DoCleared;
end;

function TDbgEntitiesThreadStackList.Count: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FList) - 1 do
    Result := Result + FList[i].List.Count;
end;


{ TDelayedUdateItem }

procedure TDelayedUdateItem.Assign(ASource: TPersistent);
begin
  BeginUpdate;
  try
    inherited Assign(ASource);
  finally
    EndUpdate;
  end;
end;

procedure TDelayedUdateItem.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then FDoChanged := False;
end;

procedure TDelayedUdateItem.Changed;
begin
  if FUpdateCount > 0
  then FDoChanged := True
  else DoChanged;
end;

constructor TDelayedUdateItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUpdateCount := 0;
end;

procedure TDelayedUdateItem.DoChanged;
begin
  inherited Changed(False);
end;

procedure TDelayedUdateItem.DoEndUpdate;
begin
  //
end;

procedure TDelayedUdateItem.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then raise EInvalidOperation.Create('TDelayedUdateItem.EndUpdate');
  if (FUpdateCount = 0)
  then DoEndUpdate;
  if (FUpdateCount = 0) and FDoChanged
  then begin
    DoChanged;
    FDoChanged := False;
  end;
end;

function TDelayedUdateItem.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;


{ TRefCountedColectionItem }

constructor TRefCountedColectionItem.Create(ACollection: TCollection);
begin
  FRefCount := 0;
  inherited Create(ACollection);
end;

destructor TRefCountedColectionItem.Destroy;
begin
  Assert(FRefcount = 0, 'Destroying referenced object');
  inherited Destroy;
end;

procedure TRefCountedColectionItem.AddReference;
begin
  Inc(FRefcount);
end;

procedure TRefCountedColectionItem.ReleaseReference;
begin
  Assert(FRefCount > 0, 'TRefCountedObject.ReleaseReference  RefCount > 0');
  Dec(FRefCount);
  if FRefCount = 0 then DoFree;
end;

procedure TRefCountedColectionItem.DoFree;
begin
  Self.Free;
end;

end.


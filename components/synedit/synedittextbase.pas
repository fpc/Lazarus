{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}
unit SynEditTextBase;

{$I synedit.inc}
{$IFOPT C+}
  {$DEFINE AssertSynMemIndex}
{$ENDIF}
{$IFDEF SynAssert}
  {$DEFINE AssertSynMemIndex}
{$ENDIF}
{$IFDEF SynUndoDebug}
  {$Define SynUndoDebugItems}
  {$Define SynUndoDebugCalls}
{$ENDIF}


interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc,
  // LazUtils
  LazMethodList,
  // SynEdit
  SynEditMiscProcs, SynEditKeyCmds;

type

  TSynEditUndoList = class;
  TSynEditUndoItem = class;

type

  { TSynEditStorageMem }

  TSynEditStorageMem = class
  private
    FItemSize: Integer;
    FMem: PByte;
    FCount, FCapacity: Integer;
    function GetItemPointer(Index: Integer): Pointer; inline;
    procedure SetItemSize(const AValue: Integer);
  protected
    function  GetInintialForItemSize: Integer; virtual;
    procedure SetCapacity(const AValue: Integer); virtual;
    procedure SetCount(const AValue: Integer); virtual;
    procedure Move(AFrom, ATo, ALen: Integer); virtual;

    property Mem: PByte read FMem;
    property ItemPointer[Index: Integer]: Pointer read GetItemPointer;
    // ItemSize must be set in the constructor, and never be changed
    property ItemSize: Integer read FItemSize write SetItemSize;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertRows(AIndex, ACount: Integer); virtual;
    procedure DeleteRows(AIndex, ACount: Integer); virtual;
    property Capacity: Integer read FCapacity write SetCapacity;
    // Capacity must be maintained by owner (Shrink)
    property Count: Integer read FCount write SetCount;
  end;

  { TSynManagedStorageMem }

  TSynManagedStorageMem = class(TSynEditStorageMem)
  protected
    // Todo: Add Flags,which updates are required
    procedure LineTextChanged(AIndex: Integer; ACount: Integer = 1); virtual;
    procedure InsertedLines(AIndex, ACount: Integer); virtual;
    procedure DeletedLines(AIndex, ACount: Integer); virtual;
  end;


  { TSynManagedStorageMemList }

  TSynManagedStorageMemList = class
  private
    FStorageMemList: Array of TSynManagedStorageMem;
    FClassList: Array of Pointer;
    function GetStorageMems(Index: Pointer): TSynManagedStorageMem;
    procedure SetStorageMems(Index: Pointer; const AValue: TSynManagedStorageMem);
    procedure SetChildCapacities(const AValue: Integer);
    procedure SetChildCounts(const AValue: Integer);
  public
    procedure ChildInsertRows(AIndex, ACount: Integer);
    procedure ChildDeleteRows(AIndex, ACount: Integer);
    procedure CallMove(AFrom, ATo, ALen: Integer);
    procedure CallLineTextChanged(AIndex: Integer; ACount: Integer = 1);
    procedure CallInsertedLines(AIndex, ACount: Integer);
    procedure CallDeletedLines(AIndex, ACount: Integer);
    property ChildCapacities: Integer write SetChildCapacities;
    property ChildCounts: Integer write SetChildCounts;
    property StorageMems[Index: Pointer]: TSynManagedStorageMem
             read GetStorageMems write SetStorageMems; default;
  end;

  { TSynEditStringsBase }

  TSynEditStringsBase = class(TStrings)
  protected
    function GetRange(Index: Pointer): TSynManagedStorageMem; virtual; abstract;
    procedure PutRange(Index: Pointer; const ARange: TSynManagedStorageMem); virtual; abstract;
  public
    procedure SendHighlightChanged(aIndex, aCount: Integer); virtual; abstract;
    function  GetPChar(ALineIndex: Integer): PChar;                                       // experimental
    function  GetPChar(ALineIndex: Integer; out ALen: Integer): PChar; virtual; abstract; // experimental
    property Ranges[Index: Pointer]: TSynManagedStorageMem read GetRange write PutRange;
  end;

  { TSynEditUndoItem }

  TSynEditUndoItem = class(TObject)
  protected
    // IsEqual is only needed/implemented for Carets
    function IsEqualContent(AnItem: TSynEditUndoItem): Boolean; virtual;
    function DebugString: String; virtual;
  public
    function IsEqual(AnItem: TSynEditUndoItem): Boolean;
    function IsCaretInfo: Boolean; virtual;
    function PerformUndo(Caller: TObject): Boolean; virtual; abstract;
  end;

  { TSynEditUndoGroup }

  TSynEditUndoGroup = class(TObject)
  private
    FItems: Array of TSynEditUndoItem;
    FCount, FCapacity: Integer;
    FReason: TSynEditorCommand;
    function GetItem(Index: Integer): TSynEditUndoItem;
    procedure Grow(ANeededCapacity: Integer = 0);
  protected
    Function HasUndoInfo: Boolean;
    procedure Append(AnUndoGroup: TSynEditUndoItem);
    function  GetLast: TSynEditUndoItem;  // Excludes caret // does not POP
    function  PopLast: TSynEditUndoItem;  // Excludes caret
    procedure TranferTo(AnUndoGroup: TSynEditUndoGroup);
    function CanMergeWith(AnUndoGroup: TSynEditUndoGroup): Boolean;
    procedure MergeWith(AnUndoGroup: TSynEditUndoGroup);
  public
    constructor Create;
    Destructor Destroy; override;

    procedure Assign(AnUndoGroup: TSynEditUndoGroup);
    procedure Add(AnItem: TSynEditUndoItem);
    procedure Clear(OnlyFreeItems: Boolean = False);
    procedure Insert(AIndex: Integer; AnItem: TSynEditUndoItem);
    procedure Delete(AIndex: Integer);
    function  Pop: TSynEditUndoItem;
    property Count: Integer read FCount;
    property Items [Index: Integer]: TSynEditUndoItem read GetItem;
    property Reason: TSynEditorCommand read FReason write FReason;
  end;


  TSynGetCaretUndoProc = function: TSynEditUndoItem of object;
  TSynUpdateCaretUndoProc = procedure(var AnUndoItem: TSynEditUndoItem; AnIsBeginUndo: Boolean) of object;

  { TSynEditUpdateCaretUndoProcList }

  TSynEditUpdateCaretUndoProcList = Class(TMethodList)
  public
    procedure CallSearchUpdateCaretUndoProcs(var AnUndoItem: TSynEditUndoItem; AnIsBeginUndo: Boolean);
  end;

  { TSynEditUndoList }

  TSynEditUndoList = class(TObject)
  private
    FGroupUndo: Boolean;
    FIsInsideRedo: Boolean;
    FUndoGroup: TSynEditUndoGroup;
    FInGroupCount: integer;
    fFullUndoImposible: boolean;
    fItems: TList;
    fLockCount: integer;
    fMaxUndoActions: integer;
    fOnAdded: TNotifyEvent;
    FOnNeedCaretUndo: TSynGetCaretUndoProc;
    FOnNeedCaretUndoList: TSynEditUpdateCaretUndoProcList;
    fUnModifiedItem: integer;
    FForceGroupEnd: Boolean;
    procedure EnsureMaxEntries;
    function GetCanUndo: boolean;
    function GetCurrentReason: TSynEditorCommand;
    function GetItemCount: integer;
    procedure SetCurrentReason(const AValue: TSynEditorCommand);
    procedure SetMaxUndoActions(Value: integer);
  public
    {$IFDEF SynUndoDebugCalls}
    DebugName: string;
    {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(AChange: TSynEditUndoItem);
    // "LastChange: Either in current Group, or in last Group, if no current
    procedure AppendToLastChange(AChange: TSynEditUndoItem);
    function  GetLastChange: TSynEditUndoItem;  // Excludes caret
    function  PopLastChange: TSynEditUndoItem;  // Excludes caret
    procedure BeginBlock;
    procedure EndBlock;
    procedure Clear;
    procedure Lock;
    function PopItem: TSynEditUndoGroup;
    function PeekItem: TSynEditUndoGroup;
    procedure Unlock;
    function IsLocked: Boolean;
    procedure MarkTopAsUnmodified;
    procedure ForceGroupEnd;
    function RealCount: Integer;
    function IsTopMarkedAsUnmodified: boolean;
    function UnModifiedMarkerExists: boolean;
    {$IFDEF SynUndoDebugBeginEnd}
    property InGroupCount: integer read FInGroupCount;
    {$ENDIF}
  public
    procedure RegisterUpdateCaretUndo(AnUpdateProc: TSynUpdateCaretUndoProc);
    procedure UnregisterUpdateCaretUndo(AnUpdateProc: TSynUpdateCaretUndoProc);

    property CanUndo: boolean read GetCanUndo;
    property FullUndoImpossible: boolean read fFullUndoImposible;
    property ItemCount: integer read GetItemCount;
    property MaxUndoActions: integer read fMaxUndoActions
      write SetMaxUndoActions;
    property IsInsideRedo: Boolean read FIsInsideRedo write FIsInsideRedo;
    property OnAddedUndo: TNotifyEvent read fOnAdded write fOnAdded;
    property OnNeedCaretUndo : TSynGetCaretUndoProc
      read FOnNeedCaretUndo write FOnNeedCaretUndo;
    property GroupUndo: Boolean read FGroupUndo write FGroupUndo;
    property CurrentGroup: TSynEditUndoGroup read FUndoGroup;
    property CurrentReason: TSynEditorCommand read GetCurrentReason
      write SetCurrentReason;
  end;

  ESynEditStorageMem = class(Exception);

implementation

const
  SListIndexOutOfBounds = 'Invalid stringlist index %d';

procedure ListIndexOutOfBounds(Index: integer);
begin
  raise ESynEditStorageMem.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

{ TSynEditUpdateCaretUndoProcList }

procedure TSynEditUpdateCaretUndoProcList.CallSearchUpdateCaretUndoProcs(var AnUndoItem: TSynEditUndoItem;
  AnIsBeginUndo: Boolean);
var
  i: LongInt;
begin
  i:=Count;
  while NextDownIndex(i) do
    TSynUpdateCaretUndoProc(Items[i])(AnUndoItem, AnIsBeginUndo);
end;

{ TSynEditStringsBase }

function TSynEditStringsBase.GetPChar(ALineIndex: Integer): PChar;
var
  l: Integer;
begin
  Result := GetPChar(ALineIndex, l);
end;

{ TSynEditUndoList }

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  // Create and keep one undo group => avoids resizing the FItems list
  FOnNeedCaretUndoList := TSynEditUpdateCaretUndoProcList.Create;
  FUndoGroup := TSynEditUndoGroup.Create;
  FIsInsideRedo := False;
  fItems := TList.Create;
  fMaxUndoActions := 1024;
  fUnModifiedItem:=-1;
  FForceGroupEnd := False;
end;

destructor TSynEditUndoList.Destroy;
begin
  Clear;
  fItems.Free;
  FreeAndNil(FUndoGroup);
  FreeAndNil(FOnNeedCaretUndoList);
  inherited Destroy;
end;

procedure TSynEditUndoList.AddChange(AChange: TSynEditUndoItem);
var
  ugroup: TSynEditUndoGroup;
begin
  if fLockCount > 0 then begin
    AChange.Free;
    exit;
  end;

  if FInGroupCount > 0 then
    FUndoGroup.Add(AChange)
  else begin
    ugroup := TSynEditUndoGroup.Create;
    ugroup.Add(AChange);
    fItems.Add(ugroup);
    if Assigned(fOnAdded) then
      fOnAdded(Self);
  end;

  EnsureMaxEntries;
end;

procedure TSynEditUndoList.AppendToLastChange(AChange: TSynEditUndoItem);
var
  cur: Boolean;
begin
  cur := FUndoGroup.HasUndoInfo;
  if (fLockCount <> 0) or ((fItems.Count = 0) and not cur) then begin
    AChange.Free;
    exit;
  end;

  if cur then
    FUndoGroup.Append(AChange)
  else
    TSynEditUndoGroup(fItems[fItems.Count-1]).Append(AChange);

  // Do not callback to synedit, or Redo Info is lost
  EnsureMaxEntries;
end;

function TSynEditUndoList.GetLastChange: TSynEditUndoItem;
var
  cur: Boolean;
begin
  Result := nil;
  cur := FUndoGroup.HasUndoInfo;
  if (fItems.Count = 0) and (not cur) then
    exit;

  if cur then
    Result :=  FUndoGroup.GetLast
  else
    Result :=  TSynEditUndoGroup(fItems[fItems.Count-1]).GetLast;
end;

function TSynEditUndoList.PopLastChange: TSynEditUndoItem;
var
  cur: Boolean;
begin
  Result := nil;
  cur := FUndoGroup.HasUndoInfo;
  if (fItems.Count = 0) and (not cur) then
    exit;

  if cur then
    Result :=  FUndoGroup.PopLast
  else
    Result :=  TSynEditUndoGroup(fItems[fItems.Count-1]).PopLast;
end;

procedure TSynEditUndoList.BeginBlock;
var
  c: TSynEditUndoItem;
begin
  Inc(FInGroupCount);
  if (FInGroupCount = 1) then begin
    FUndoGroup.Clear;
    c := nil;
    if assigned(FOnNeedCaretUndo) then
      c := FOnNeedCaretUndo();
    FOnNeedCaretUndoList.CallSearchUpdateCaretUndoProcs(c, True);
    if c <> nil then
      FUndoGroup.add(c);
  end;
  {$IFDEF SynUndoDebugCalls}
  DebugLnEnter(['>> TSynEditUndoList.BeginBlock ', DebugName, ' ', DbgSName(self), ' ', dbgs(Self), ' fLockCount=', fLockCount, ' Cnt=', fItems.Count, ' FInGroupCount=', FInGroupCount, ' fUnModifiedItem=', fUnModifiedItem]);
  {$ENDIF}
end;

procedure TSynEditUndoList.Clear;
var
  i: integer;
begin
  {$IFDEF SynUndoDebugCalls}
  DebugLn(['>> TSynEditUndoList.Clear ', DebugName, ' ', DbgSName(self), ' ', dbgs(Self), ' fLockCount=', fLockCount, ' Cnt=', fItems.Count, ' FInGroupCount=', FInGroupCount]);
  {$ENDIF}
  for i := 0 to fItems.Count - 1 do
    TSynEditUndoGroup(fItems[i]).Free;
  fItems.Clear;
  fFullUndoImposible := FALSE;
  fUnModifiedItem:=-1;
end;

procedure TSynEditUndoList.EndBlock;
var
  ugroup: TSynEditUndoGroup;
  c: TSynEditUndoItem;
begin
  if FInGroupCount > 0 then begin
    Dec(FInGroupCount);
    if (FInGroupCount = 0) and FUndoGroup.HasUndoInfo then
    begin
      // Keep position for REDO; Do not replace if present
      if (not FUndoGroup.Items[FUndoGroup.Count - 1].IsCaretInfo) then begin
        c := nil;
        if assigned(FOnNeedCaretUndo) then
          c := FOnNeedCaretUndo();
        FOnNeedCaretUndoList.CallSearchUpdateCaretUndoProcs(c, False);
        if c <> nil then
          FUndoGroup.add(c);
      end;
      if (fItems.Count > 0) and FGroupUndo and (not IsTopMarkedAsUnmodified) and
        (not FForceGroupEnd) and
        FUndoGroup.CanMergeWith(TSynEditUndoGroup(fItems[fItems.Count - 1])) then
      begin
        FUndoGroup.MergeWith(TSynEditUndoGroup(fItems[fItems.Count - 1]));
        FUndoGroup.TranferTo(TSynEditUndoGroup(fItems[fItems.Count - 1]));
      end else begin
        ugroup := TSynEditUndoGroup.Create;
        FUndoGroup.TranferTo(ugroup);
        fItems.Add(ugroup);
      end;
      if Assigned(fOnAdded) then
        fOnAdded(Self);
      FIsInsideRedo := False;
      FForceGroupEnd := False;
    end;
    {$IFDEF SynUndoDebugCalls}
    DebugLnExit(['<< TSynEditUndoList.EndBlock ', DebugName, ' ', DbgSName(self), ' ', dbgs(Self), ' fLockCount=', fLockCount, ' Cnt=', fItems.Count, ' FInGroupCount=', FInGroupCount, ' fUnModifiedItem=', fUnModifiedItem]);
  end else begin
    DebugLn(['** EXTRA TSynEditUndoList.EndBlock ', DebugName, ' ', DbgSName(self), ' ', dbgs(Self), ' fLockCount=', fLockCount, ' Cnt=', fItems.Count, ' FInGroupCount=', FInGroupCount, ' fUnModifiedItem=', fUnModifiedItem]);
    {$ENDIF}
  end;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoGroup;
begin
  if fItems.Count > fMaxUndoActions then begin
    fFullUndoImposible := TRUE;
    while fItems.Count > fMaxUndoActions do begin
      Item := TSynEditUndoGroup(fItems[0]);
      Item.Free;
      fItems.Delete(0);
      if fUnModifiedItem>=0 then dec(fUnModifiedItem);
    end;
  end;
end;

function TSynEditUndoList.GetCanUndo: boolean;
begin
  Result := fItems.Count > 0;
end;

function TSynEditUndoList.GetCurrentReason: TSynEditorCommand;
begin
  Result := FUndoGroup.Reason;
end;

function TSynEditUndoList.GetItemCount: integer;
begin
  Result := fItems.Count;
end;

procedure TSynEditUndoList.SetCurrentReason(const AValue: TSynEditorCommand);
begin
  if FUndoGroup.Reason = ecNone then
    FUndoGroup.Reason := AValue;
end;

procedure TSynEditUndoList.Lock;
begin
  Inc(fLockCount);
  {$IFDEF SynUndoDebugCalls}
  DebugLnEnter(['>> TSynEditUndoList.Lock ', DebugName, ' ', DbgSName(self), ' ', dbgs(Self), ' fLockCount=', fLockCount, ' Cnt=', fItems.Count, ' FInGroupCount=', FInGroupCount]);
  {$ENDIF}
end;

function TSynEditUndoList.PopItem: TSynEditUndoGroup;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then begin
    Result := TSynEditUndoGroup(fItems[iLast]);
    fItems.Delete(iLast);
    if fUnModifiedItem>fItems.Count then
      fUnModifiedItem:=-1;
  end;
end;

function TSynEditUndoList.PeekItem: TSynEditUndoGroup;
begin
  Result := nil;
  if fItems.Count > 0 then
    Result := TSynEditUndoGroup(fItems[fItems.Count - 1]);
end;

procedure TSynEditUndoList.SetMaxUndoActions(Value: integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fMaxUndoActions then begin
    fMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

function TSynEditUndoList.RealCount: Integer;
begin
  Result := fItems.Count;
  if (FInGroupCount > 0) and FUndoGroup.HasUndoInfo then
    Result := Result + 1;
end;

procedure TSynEditUndoList.Unlock;
begin
  if fLockCount > 0 then
    Dec(fLockCount);
  {$IFDEF SynUndoDebugCalls}
  DebugLnExit(['<< TSynEditUndoList.UnLock ', DebugName, ' ', DbgSName(self), ' ', dbgs(Self), ' fLockCount=', fLockCount, ' Cnt=', fItems.Count, ' FInGroupCount=', FInGroupCount]);
  {$ENDIF}
end;

function TSynEditUndoList.IsLocked: Boolean;
begin
  Result := fLockCount > 0;
end;

procedure TSynEditUndoList.MarkTopAsUnmodified;
begin
  fUnModifiedItem := RealCount;
end;

procedure TSynEditUndoList.ForceGroupEnd;
begin
  FForceGroupEnd := True;
end;

function TSynEditUndoList.IsTopMarkedAsUnmodified: boolean;
begin
  Result := (RealCount = fUnModifiedItem);
end;

function TSynEditUndoList.UnModifiedMarkerExists: boolean;
begin
  Result := fUnModifiedItem >= 0;
end;

procedure TSynEditUndoList.RegisterUpdateCaretUndo(AnUpdateProc: TSynUpdateCaretUndoProc);
begin
  FOnNeedCaretUndoList.Add(TMethod(AnUpdateProc));
end;

procedure TSynEditUndoList.UnregisterUpdateCaretUndo(AnUpdateProc: TSynUpdateCaretUndoProc);
begin
  FOnNeedCaretUndoList.Remove(TMethod(AnUpdateProc));
end;

{ TSynEditUndoItem }

function TSynEditUndoItem.IsEqualContent(AnItem: TSynEditUndoItem): Boolean;
begin
  Result := False;
end;

function TSynEditUndoItem.IsEqual(AnItem: TSynEditUndoItem): Boolean;
begin
  Result := (ClassType = AnItem.ClassType) and IsEqualContent(AnItem);
end;

function TSynEditUndoItem.DebugString: String;
begin
  Result := '';
end;

function TSynEditUndoItem.IsCaretInfo: Boolean;
begin
  Result := False;
end;

(*
function TSynEditUndoItem.ChangeStartPos: TPoint;
begin
  if (fChangeStartPos.Y < fChangeEndPos.Y)
    or ((fChangeStartPos.Y = fChangeEndPos.Y) and (fChangeStartPos.X < fChangeEndPos.X))
  then result := fChangeStartPos
  else result := fChangeEndPos;
end;

function TSynEditUndoItem.ChangeEndPos: TPoint;
begin
  if (fChangeStartPos.Y < fChangeEndPos.Y)
    or ((fChangeStartPos.Y = fChangeEndPos.Y) and (fChangeStartPos.X < fChangeEndPos.X))
  then result := fChangeEndPos
  else result := fChangeStartPos;
end;
*)

{ TSynEditUndoGroup }

procedure TSynEditUndoGroup.Grow(ANeededCapacity: Integer);
begin
  if ANeededCapacity > 0 then
    FCapacity := Max(ANeededCapacity, FCapacity)
  else
    FCapacity := FCapacity + Max(10, FCapacity Div 8);
  SetLength(FItems, FCapacity);
end;

function TSynEditUndoGroup.HasUndoInfo: Boolean;
var
  i: Integer;
begin
  i := 0;
  while i < FCount do
    if FItems[i].IsCaretInfo then
      inc(i)
    else
      exit(true);
  Result := False;
end;

procedure TSynEditUndoGroup.Append(AnUndoGroup: TSynEditUndoItem);
var
  i: Integer;
begin
  i := FCount - 1;
  while (i >= 0) and FItems[i].IsCaretInfo do
    dec(i);
  inc(i);
  Insert(i, AnUndoGroup);
end;

function TSynEditUndoGroup.GetLast: TSynEditUndoItem;
var
  i: Integer;
begin
  Result := nil;
  i := FCount - 1;
  while (i >= 0) and FItems[i].IsCaretInfo do
    dec(i);
   if i >= 0 then
     Result := FItems[i];
end;

function TSynEditUndoGroup.PopLast: TSynEditUndoItem;
var
  i: Integer;
begin
  Result := nil;
  i := FCount - 1;
  while (i >= 0) and FItems[i].IsCaretInfo do
    dec(i);
   if i >= 0 then begin
     Result := FItems[i];
     Delete(i);
   end;
end;

procedure TSynEditUndoGroup.TranferTo(AnUndoGroup: TSynEditUndoGroup);
begin
  //AnUndoGroup.Assign(self);
  AnUndoGroup.Clear(True);
  AnUndoGroup.FCapacity := Count;
  AnUndoGroup.FCount := Count;
  AnUndoGroup.FItems := FItems;
  FItems := nil;
  FCapacity := 0;
  FCount := 0; // Do not clear; that would free the items

  AnUndoGroup.FReason := Reason;
end;

function TSynEditUndoGroup.CanMergeWith(AnUndoGroup: TSynEditUndoGroup): Boolean;
begin
  // Check if the other group can be merged to the START of this node
  if AnUndoGroup.Count = 0 then exit(True);
  Result := (FReason <> ecNone) and (AnUndoGroup.Reason = FReason)
        and Items[0].IsCaretInfo
        and AnUndoGroup.Items[AnUndoGroup.Count - 1].IsEqual(Items[0]);
end;

procedure TSynEditUndoGroup.MergeWith(AnUndoGroup: TSynEditUndoGroup);
begin
  // Merge other group to start
  AnUndoGroup.Pop.Free;
  if AnUndoGroup.Count = 0 then
    exit;

  Grow(Count + AnUndoGroup.Count); // since we replace item[0], this is one extra

  If AnUndoGroup.Count > 1 then
    System.Move(FItems[1], FItems[AnUndoGroup.Count],
                (FCount - 1) * SizeOf(TSynEditUndoItem));
  assert(Count > 0, 'TSynEditUndoGroup.MergeWith: Count > 0');
  FItems[0].Free;
  System.Move(AnUndoGroup.FItems[0], FItems[0],
              (AnUndoGroup.Count) * SizeOf(TSynEditUndoItem));
  FCount := FCount + AnUndoGroup.FCount - 1;
  AnUndoGroup.FCount := 0;
end;

function TSynEditUndoGroup.GetItem(Index: Integer): TSynEditUndoItem;
begin
  Result := FItems[Index];
end;

constructor TSynEditUndoGroup.Create;
begin
  FCount := 0;
  FCapacity := 0;
end;

destructor TSynEditUndoGroup.Destroy;
begin
  Clear(True);
  FItems := nil;
  inherited Destroy;
end;

procedure TSynEditUndoGroup.Assign(AnUndoGroup: TSynEditUndoGroup);
begin
  Clear;
  FCapacity := AnUndoGroup.Count;
  FCount := FCapacity;
  SetLength(FItems, FCapacity);
  if FCapacity = 0 then
    exit;
  System.Move(AnUndoGroup.FItems[0], FItems[0], FCapacity * SizeOf(TSynEditUndoItem));
  FReason := AnUndoGroup.Reason;
end;

procedure TSynEditUndoGroup.Add(AnItem: TSynEditUndoItem);
begin
  if (FCount > 0) and AnItem.IsCaretInfo
     and FItems[FCount - 1].IsCaretInfo then
  begin
    FItems[FCount - 1].Free;
    FItems[FCount - 1] := AnItem;
    exit;
  end;
  if FCount >= FCapacity then
    Grow;
  FItems[FCount] := AnItem;
  inc (FCount);
end;

procedure TSynEditUndoGroup.Clear(OnlyFreeItems: Boolean);
begin
  while FCount > 0 do begin
    dec(FCount);
    FItems[FCount].Free;
  end;
  if (not OnlyFreeItems) and (FCapacity > 100) then begin
    FCapacity := 100;
    SetLength(FItems, FCapacity);
  end;
  FReason := ecNone;
end;

procedure TSynEditUndoGroup.Insert(AIndex: Integer; AnItem: TSynEditUndoItem);
begin
  if FCount >= FCapacity then
    Grow;
  If AIndex < FCount then
    System.Move(FItems[AIndex], FItems[AIndex+1],
                (FCount - AIndex) * SizeOf(TSynEditUndoItem));
  FItems[AIndex] := AnItem;
  inc (FCount);
end;

procedure TSynEditUndoGroup.Delete(AIndex: Integer);
begin
  {$IFDEF AssertSynMemIndex}
  if (AIndex < 0) or (AIndex >= FCount) then
    raise Exception.Create(Format('Bad AIndex cnt= %d cap= %d idx= %d',[FCount, FCapacity, AIndex]));
  {$ENDIF}
  If AIndex < FCount - 1 then
    System.Move(FItems[AIndex+1], FItems[AIndex],
                (FCount - AIndex - 1) * SizeOf(TSynEditUndoItem));
  dec (FCount);
end;

function TSynEditUndoGroup.Pop: TSynEditUndoItem;
begin
  if FCount <= 0 then
    exit(nil);
  dec(FCount);
  Result := FItems[FCount];
end;

{ TSynEditStorageMem }

function TSynEditStorageMem.GetItemPointer(Index: Integer): Pointer;
begin
  {$IFDEF AssertSynMemIndex}
  if (Index < 0) or (Index >= FCapacity) or (FCount > FCapacity) then
    raise Exception.Create(Format('Bad Index cnt= %d cap= %d idx= %d',[FCount, FCapacity, Index]));
  {$ENDIF}
  Result := Pointer(FMem + Index * ItemSize);
end;

procedure TSynEditStorageMem.SetItemSize(const AValue: Integer);
begin
  if (FCapacity <> 0) then raise Exception.Create('Not allowe dto change ItemSize');
  if FItemSize = AValue then exit;
  FItemSize := AValue;
end;

function TSynEditStorageMem.GetInintialForItemSize: Integer;
begin
  Result := 0;
end;

procedure TSynEditStorageMem.SetCapacity(const AValue: Integer);
begin
  {$IFDEF AssertSynMemIndex}
  if (AValue < 0) or (AValue < FCount) then raise Exception.Create('Bad Capacity');
  {$ENDIF}
  if FCapacity = AValue then exit;
  FMem := ReallocMem(FMem, AValue * ItemSize);
  if AValue > FCapacity then
    FillChar((FMem+FCapacity*ItemSize)^, (AValue-FCapacity)*ItemSize, 0);
  FCapacity := AValue;
end;

procedure TSynEditStorageMem.SetCount(const AValue: Integer);
begin
  {$IFDEF AssertSynMemIndex}
  if (AValue < 0) or (AValue > FCapacity) then raise Exception.Create('Bad Count');
  {$ENDIF}
  FCount := AValue;
end;

constructor TSynEditStorageMem.Create;
begin
  FItemSize := GetInintialForItemSize;
  FCapacity := 0;
  FCount := 0;
end;

destructor TSynEditStorageMem.Destroy;
begin
  SetCount(0);
  SetCapacity(0);
  inherited Destroy;
end;

procedure TSynEditStorageMem.InsertRows(AIndex, ACount: Integer);
begin
  if (AIndex < 0) or (AIndex > Count) then
    ListIndexOutOfBounds(AIndex);
  if Capacity < Count + ACount then
    SetCapacity(Count + ACount + 8);
  if AIndex < Count then
    Move(AIndex, AIndex + ACount, Count - AIndex);
  Count := Count + ACount;
end;

procedure TSynEditStorageMem.DeleteRows(AIndex, ACount: Integer);
var
  LinesAfter: Integer;
begin
  if (AIndex < 0) or (AIndex + ACount > Count) then
    ListIndexOutOfBounds(AIndex);
  LinesAfter := Count - (AIndex + ACount);
  if LinesAfter > 0 then
    Move(AIndex + ACount, AIndex, LinesAfter);
  Count := Count - ACount;
  if (Capacity > 16) and (Capacity > Count * 2) then
    Capacity := Capacity - (Count div 2);
end;

procedure TSynEditStorageMem.Move(AFrom, ATo, ALen: Integer);
var
  len: Integer;
begin
  {$IFDEF AssertSynMemIndex}
  if (FCount > FCapacity) or (aTo=AFrom) or
     (AFrom < 0) or (AFrom >= FCapacity) or
     (ATo < 0) or (ATo >= FCapacity) then
    raise Exception.Create('Bad Move');
  {$ENDIF}
  if ATo < AFrom then begin
    Len := Min(ALen, AFrom-ATo);
    System.Move((FMem+AFrom*ItemSize)^, (FMem+ATo*ItemSize)^, Alen*ItemSize);
    FillChar((FMem+(AFrom+ALen-Len)*ItemSize)^, Len*ItemSize, 0);
  end else begin
    Len := Min(ALen, ATo-AFrom);
    System.Move((FMem+AFrom*ItemSize)^, (FMem+ATo*ItemSize)^, Alen*ItemSize);
    FillChar((FMem+AFrom*ItemSize)^, Len*ItemSize, 0);
  end;
end;

{ TSynManagedStorageMem }

procedure TSynManagedStorageMem.LineTextChanged(AIndex: Integer; ACount: Integer = 1);
begin  // empty base class
end;

procedure TSynManagedStorageMem.InsertedLines(AIndex, ACount: Integer);
begin  // empty base class
end;

procedure TSynManagedStorageMem.DeletedLines(AIndex, ACount: Integer);
begin  // empty base class
end;

{ TSynManagedStorageMemList }

function TSynManagedStorageMemList.GetStorageMems(Index: Pointer): TSynManagedStorageMem;
var
  i: Integer;
begin
  Result := nil;
  i := length(FClassList);
  while (i > 0) and (Result = nil) do begin
    dec(i);
    if FClassList[i] = Index then
      Result := FStorageMemList[i];
  end;
end;

procedure TSynManagedStorageMemList.SetStorageMems(Index: Pointer;
  const AValue: TSynManagedStorageMem);
var
  i, j: Integer;
begin
  i := length(FClassList) - 1;
  while (i >= 0) and (FClassList[i] <> Index) do
    dec(i);

  if i < 0 then begin
    if AValue = nil then begin
      debugln('Removing none existent range');
      exit;
    end;
    j := length(FClassList);
    SetLength(FClassList, j + 1);
    SetLength(FStorageMemList, j + 1);
    FClassList[j]      := Index;
    FStorageMemList[j] := AValue;
  end
  else begin
    if AValue <> nil then
      DebugLn(['TSynEditStringMemory.SetRange - Overwriting old range at index=', i, ' index=', dbgs(Index)]);
    FStorageMemList[i] := AValue;
    if AValue = nil then begin
      for j := i to length(FClassList) - 2 do begin
        FClassList[j]      := FClassList[j+1];
        FStorageMemList[j] := FStorageMemList[j+1];
      end;
      SetLength(FClassList, length(FClassList) - 1);
      SetLength(FStorageMemList, length(FStorageMemList) - 1);
    end;
  end;
end;

procedure TSynManagedStorageMemList.SetChildCapacities(const AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].Capacity := AValue;
end;

procedure TSynManagedStorageMemList.SetChildCounts(const AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].Count := AValue;
end;

procedure TSynManagedStorageMemList.ChildInsertRows(AIndex, ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].InsertRows(AIndex, ACount);
end;

procedure TSynManagedStorageMemList.ChildDeleteRows(AIndex, ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].DeleteRows(AIndex, ACount);
end;

procedure TSynManagedStorageMemList.CallMove(AFrom, ATo, ALen: Integer);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].Move(AFrom, ATo, ALen);
end;

procedure TSynManagedStorageMemList.CallLineTextChanged(AIndex: Integer; ACount: Integer = 1);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].LineTextChanged(AIndex, ACount);
end;

procedure TSynManagedStorageMemList.CallInsertedLines(AIndex, ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].InsertedLines(AIndex, ACount);
end;

procedure TSynManagedStorageMemList.CallDeletedLines(AIndex, ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to high(FStorageMemList) do
    FStorageMemList[i].DeletedLines(AIndex, ACount);
end;

end.


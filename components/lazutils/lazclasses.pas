{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazClasses;

{$mode objfpc}{$H+}

interface

uses
  sysutils, Classes,
  // LazUtils
  LazMethodList;

type

  { TFreeNotifyingObject }

  TFreeNotifyingObject = class
  private
    FFreeNotificationList: TMethodList;
  public
    destructor Destroy; override;
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
  end;

  { TRefCountedObject }

  TRefCountedObject = class(TFreeNotifyingObject)
  private
    FRefCount, FInDecRefCount: Integer;
    {$IFDEF WITH_REFCOUNT_DEBUG}
    {$IFDEF WITH_REFCOUNT_LEAK_DEBUG}
    FDebugNext, FDebugPrev: TRefCountedObject;
    {$ENDIF}
    FDebugList: TStringList;
    FInDestroy: Boolean;
    procedure DbgAddName(DebugIdAdr: Pointer = nil; DebugIdTxt: String = '');
    procedure DbgRemoveName(DebugIdAdr: Pointer = nil; DebugIdTxt: String = '');
    {$ENDIF}
  protected
    procedure DoFree; virtual;
    procedure DoReferenceAdded; virtual;
    procedure DoReferenceReleased; virtual;
    property  RefCount: Integer read FRefCount;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
    procedure ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
    {$IFDEF WITH_REFCOUNT_DEBUG}
    procedure DbgRenameReference(DebugIdAdr: Pointer; DebugIdTxt: String);
    procedure DbgRenameReference(OldDebugIdAdr: Pointer; OldDebugIdTxt: String; DebugIdAdr: Pointer; DebugIdTxt: String = '');
    {$ENDIF}
  end;

  { TRefCntObjList }

  TRefCntObjList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;


procedure ReleaseRefAndNil(var ARefCountedObject {$IFDEF WITH_REFCOUNT_DEBUG}; DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''{$ENDIF});
procedure NilThenReleaseRef(var ARefCountedObject {$IFDEF WITH_REFCOUNT_DEBUG}; DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''{$ENDIF});

implementation
{$IFDEF WITH_REFCOUNT_DEBUG}
uses LazLoggerBase;
{$IFDEF WITH_REFCOUNT_LEAK_DEBUG}
var FUnfreedRefObjList: TRefCountedObject = nil;
{$ENDIF}
{$ENDIF}

{ TFreeNotifyingObject }

destructor TFreeNotifyingObject.Destroy;
begin
  if FFreeNotificationList <> nil then
    FFreeNotificationList.CallNotifyEvents(Self);
  inherited Destroy;
  FreeAndNil(FFreeNotificationList);
end;

procedure TFreeNotifyingObject.AddFreeNotification(ANotification: TNotifyEvent);
begin
  if FFreeNotificationList = nil then
    FFreeNotificationList := TMethodList.Create;
  FFreeNotificationList.Add(TMethod(ANotification));
end;

procedure TFreeNotifyingObject.RemoveFreeNotification(ANotification: TNotifyEvent);
begin
  if FFreeNotificationList = nil then
    exit;
  FFreeNotificationList.Remove(TMethod(ANotification));
end;

{ TRefCountedObject }

procedure TRefCountedObject.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
begin
  {$IFDEF WITH_REFCOUNT_DEBUG}
  Assert(not FInDestroy, 'Adding reference while destroying');
  DbgAddName(DebugIdAdr, DebugIdTxt);
  {$ENDIF}
  Inc(FRefcount);
  // call only if overridden
  If TMethod(@DoReferenceAdded).Code <> Pointer(@TRefCountedObject.DoReferenceAdded) then
    DoReferenceAdded;
end;

{$IFDEF WITH_REFCOUNT_DEBUG}
procedure TRefCountedObject.DbgAddName(DebugIdAdr: Pointer; DebugIdTxt: String);
var
  s: String;
begin
  if FDebugList = nil then FDebugList := TStringList.Create;
  if (DebugIdAdr <> nil) or (DebugIdTxt <> '') then
    s := inttostr(PtrUInt(DebugIdAdr))+': '+DebugIdTxt
  else
    s := 'not named';
  if FDebugList.indexOf(s) < 0 then
    FDebugList.AddObject(s, TObject(1))
  else begin
    if s <> 'not named' then
      debugln(['TRefCountedObject.AddReference Duplicate ref ', s]);
    FDebugList.Objects[FDebugList.IndexOf(s)] :=
      TObject(PtrUint(FDebugList.Objects[FDebugList.IndexOf(s)])+1);
  end;
end;

procedure TRefCountedObject.DbgRemoveName(DebugIdAdr: Pointer; DebugIdTxt: String);
var
  s: String;
begin
  if FDebugList = nil then FDebugList := TStringList.Create;
  if (DebugIdAdr <> nil) or (DebugIdTxt <> '') then
    s := inttostr(PtrUInt(DebugIdAdr))+': '+DebugIdTxt
  else
    s := 'not named';
  assert(FDebugList.indexOf(s) >= 0, 'Has reference (entry) for '+s);
  assert(PtrUint(FDebugList.Objects[FDebugList.IndexOf(s)]) > 0, 'Has reference (> 0) for '+s);
  if PtrUint(FDebugList.Objects[FDebugList.IndexOf(s)]) = 1 then
    FDebugList.Delete(FDebugList.IndexOf(s))
  else
    FDebugList.Objects[FDebugList.IndexOf(s)] :=
      TObject(PtrInt(FDebugList.Objects[FDebugList.IndexOf(s)])-1);
end;
{$ENDIF}

procedure TRefCountedObject.DoFree;
begin
  {$IFDEF WITH_REFCOUNT_DEBUG}
  Assert(not FInDestroy, 'TRefCountedObject.DoFree: Double destroy');
  FInDestroy := True;
  {$ENDIF}
  Self.Free;
end;

procedure TRefCountedObject.DoReferenceAdded;
begin
  //
end;

procedure TRefCountedObject.DoReferenceReleased;
begin
  //
end;

constructor TRefCountedObject.Create;
begin
  FRefCount := 0;
  FInDecRefCount := 0;
  {$IFDEF WITH_REFCOUNT_DEBUG}
  if FDebugList = nil then
    FDebugList := TStringList.Create;
  {$IFDEF WITH_REFCOUNT_LEAK_DEBUG}
  FDebugNext := FUnfreedRefObjList;
  FUnfreedRefObjList := Self;
  if FDebugNext <> nil then FDebugNext.FDebugPrev := Self;
  {$ENDIF}
  {$ENDIF}
  inherited;
end;

destructor TRefCountedObject.Destroy;
begin
  {$IFDEF WITH_REFCOUNT_DEBUG}
  FreeAndNil(FDebugList);
  {$IFDEF WITH_REFCOUNT_LEAK_DEBUG}
  if not( (FDebugPrev=nil) and (FDebugNext = nil) and (FUnfreedRefObjList <> self) ) then begin
    if FDebugPrev <> nil then begin
      Assert(FDebugPrev.FDebugNext = Self);
      FDebugPrev.FDebugNext := FDebugNext;
    end
    else begin
      Assert(FUnfreedRefObjList = Self);
      FUnfreedRefObjList := FDebugNext;
    end;
    if FDebugNext <> nil then begin
      Assert(FDebugNext.FDebugPrev = Self);
      FDebugNext.FDebugPrev := FDebugPrev;
    end;
  end;
  {$ENDIF}
  {$ENDIF}
  Assert(FRefcount = 0, 'Destroying referenced object');
  inherited;
end;

procedure TRefCountedObject.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
begin
  if Self = nil then exit;
  {$IFDEF WITH_REFCOUNT_DEBUG}
  DbgRemoveName(DebugIdAdr, DebugIdTxt);
  {$ENDIF}
  Assert(FRefCount > 0, 'TRefCountedObject.ReleaseReference  RefCount > 0');

  Dec(FRefCount);
  inc(FInDecRefCount);
  // call only if overridden

  // Do not check for RefCount = 0, since this was done, by whoever decreased it;
  If TMethod(@DoReferenceReleased).Code <> Pointer(@TRefCountedObject.DoReferenceReleased) then
    DoReferenceReleased;

  dec(FInDecRefCount);
  if (FRefCount = 0) and (FInDecRefCount = 0) then
    DoFree;
end;

{$IFDEF WITH_REFCOUNT_DEBUG}
procedure TRefCountedObject.DbgRenameReference(DebugIdAdr: Pointer; DebugIdTxt: String);
begin
  DbgRemoveName(nil, '');
  DbgAddName(DebugIdAdr, DebugIdTxt);
end;

procedure TRefCountedObject.DbgRenameReference(OldDebugIdAdr: Pointer; OldDebugIdTxt: String;
  DebugIdAdr: Pointer; DebugIdTxt: String);
begin
  DbgRemoveName(OldDebugIdAdr, OldDebugIdTxt);
  DbgAddName(DebugIdAdr, DebugIdTxt);
end;
{$ENDIF}

{ TRefCntObjList }

procedure TRefCntObjList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  case Action of
    lnAdded:   TRefCountedObject(Ptr).AddReference;
    lnExtracted,
    lnDeleted: TRefCountedObject(Ptr).ReleaseReference;
  end;
end;

procedure ReleaseRefAndNil(var ARefCountedObject {$IFDEF WITH_REFCOUNT_DEBUG}; DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''{$ENDIF});
begin
  Assert( (Pointer(ARefCountedObject) = nil) or
          (TObject(ARefCountedObject) is TRefCountedObject),
         'ReleaseRefAndNil requires TRefCountedObject');

  if Pointer(ARefCountedObject) = nil then
    exit;

  if (TObject(ARefCountedObject) is TRefCountedObject) then
    TRefCountedObject(ARefCountedObject).ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
  Pointer(ARefCountedObject) := nil;
end;

procedure NilThenReleaseRef(var ARefCountedObject {$IFDEF WITH_REFCOUNT_DEBUG}; DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''{$ENDIF});
var
  RefObj: TRefCountedObject;
begin
  Assert( (Pointer(ARefCountedObject) = nil) or
          (TObject(ARefCountedObject) is TRefCountedObject),
         'ReleaseRefAndNil requires TRefCountedObject');

  if Pointer(ARefCountedObject) = nil then
    exit;

  if (TObject(ARefCountedObject) is TRefCountedObject) then
    RefObj := TRefCountedObject(ARefCountedObject)
  else RefObj := nil;
  Pointer(ARefCountedObject) := nil;

  if RefObj <> nil then
    RefObj.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
end;

end .


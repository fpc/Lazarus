unit LazClasses;

{$mode objfpc}{$H+}

interface

uses
  sysutils, Classes, LazMethodList;

type

  { TFreeNotifyingObject }

  TFreeNotifyingObject = class
  private
    FFreeNotificationList: TMethodList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFreeeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeeNotification(ANotification: TNotifyEvent);
  end;

  { TRefCountedObject }

  TRefCountedObject = class(TFreeNotifyingObject)
  private
    FRefCount: Integer;
    {$IFDEF WITH_REFCOUNT_DEBUG}
    FDebugList: TStringList;
    FInDestroy: Boolean;
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
  end;

  { TRefCntObjList }

  TRefCntObjList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;


procedure ReleaseRefAndNil(var ARefCountedObject);

implementation
{$IFDEF WITH_REFCOUNT_DEBUG}
uses LazLoggerBase;
{$ENDIF}

{ TFreeNotifyingObject }

constructor TFreeNotifyingObject.Create;
begin
  FFreeNotificationList := TMethodList.Create;
  inherited Create;
end;

destructor TFreeNotifyingObject.Destroy;
begin
  FFreeNotificationList.CallNotifyEvents(Self);
  inherited Destroy;
  FreeAndNil(FFreeNotificationList);
end;

procedure TFreeNotifyingObject.AddFreeeNotification(ANotification: TNotifyEvent);
begin
  FFreeNotificationList.Add(TMethod(ANotification));
end;

procedure TFreeNotifyingObject.RemoveFreeeNotification(ANotification: TNotifyEvent);
begin
  FFreeNotificationList.Remove(TMethod(ANotification));
end;

{ TRefCountedObject }

procedure TRefCountedObject.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
{$IFDEF WITH_REFCOUNT_DEBUG}
var
  s: String;
{$ENDIF}
begin
  {$IFDEF WITH_REFCOUNT_DEBUG}
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
  {$ENDIF}
  Inc(FRefcount);
  // call only if overridden
  If TMethod(@DoReferenceAdded).Code <> Pointer(@TRefCountedObject.DoReferenceAdded) then
    DoReferenceAdded;
end;

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
  {$IFDEF WITH_REFCOUNT_DEBUG}
  if FDebugList = nil then
    FDebugList := TStringList.Create;
  {$ENDIF}
  inherited;
end;

destructor TRefCountedObject.Destroy;
begin
  {$IFDEF WITH_REFCOUNT_DEBUG}
  FDebugList.Free;
  {$ENDIF}
  Assert(FRefcount = 0, 'Destroying referenced object');
  inherited;
end;

procedure TRefCountedObject.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
{$IFDEF WITH_REFCOUNT_DEBUG}
var
  s: String;
{$ENDIF}
begin
  if Self = nil then exit;
  {$IFDEF WITH_REFCOUNT_DEBUG}
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
  {$ENDIF}
  Assert(FRefCount > 0, 'TRefCountedObject.ReleaseReference  RefCount > 0');
  Dec(FRefCount);
  // call only if overridden
  If TMethod(@DoReferenceReleased).Code <> Pointer(@TRefCountedObject.DoReferenceReleased) then
    DoReferenceReleased;
  if FRefCount = 0 then DoFree;
end;

{ TRefCntObjList }

procedure TRefCntObjList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  case Action of
    lnAdded:   TRefCountedObject(Ptr).AddReference;
    lnExtracted,
    lnDeleted: TRefCountedObject(Ptr).ReleaseReference;
  end;
end;

procedure ReleaseRefAndNil(var ARefCountedObject);
begin
  Assert( (Pointer(ARefCountedObject) = nil) or
          (TObject(ARefCountedObject) is TRefCountedObject),
         'ReleaseRefAndNil requires TRefCountedObject');

  if Pointer(ARefCountedObject) = nil then
    exit;

  if (TObject(ARefCountedObject) is TRefCountedObject) then
    TRefCountedObject(ARefCountedObject).ReleaseReference;
  Pointer(ARefCountedObject) := nil;
end;

end .


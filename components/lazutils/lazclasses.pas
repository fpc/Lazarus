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
  protected
    procedure DoFree; virtual;
    property  RefCount: Integer read FRefCount;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure AddReference;
    procedure ReleaseReference;
  end;

  { TRefCntObjList }

  TRefCntObjList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;


procedure ReleaseRefAndNil(var ARefCountedObject);

implementation

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

procedure TRefCountedObject.AddReference;
begin
  Inc(FRefcount);
end;

procedure TRefCountedObject.DoFree;
begin
  Self.Free;
end;

constructor TRefCountedObject.Create;
begin
  FRefCount := 0;
  inherited;
end;

destructor TRefCountedObject.Destroy;
begin
  Assert(FRefcount = 0, 'Destroying referenced object');
  inherited;
end;

procedure TRefCountedObject.ReleaseReference;
begin
  Assert(FRefCount > 0, 'TRefCountedObject.ReleaseReference  RefCount > 0');
  Dec(FRefCount);
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

end.


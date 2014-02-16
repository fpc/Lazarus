unit DbgIntfMiscClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazClasses;

type

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
    procedure BeginUpdate;
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


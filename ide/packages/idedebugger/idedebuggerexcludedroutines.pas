unit IdeDebuggerExcludedRoutines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazClasses, Laz2_XMLCfg, IdeDebuggerUtils,
  IdeDbgConfigItemCheckListBoxFrame, LazDebuggerIntfExcludedRoutines,
  LazDebuggerIntfSynchronizedList;

type

  TIdeDebuggerExeProcSelectorList = class;

  { TIdeDebuggerExeProcSelector }

  TIdeDebuggerExeProcSelector = class(specialize TDbgExeProcSelectorTemplate<TObject>)
  private
    FId: QWord;
    FChangeStamp: QWord;
    FUpdateLock: integer;
    FNeedChanged: boolean;
    function GetNameMatchValue: TStringArray;
    procedure SetNameMatchValue(AValue: TStringArray);
  protected
    function GetID: QWord;
    function GetChangeStamp: QWord;

    procedure Changed; override;
    procedure DoChanged; virtual;
  public
    constructor Create; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    // Assign does not trigger Changed()
    procedure Assign(AnOther: TIdeDebuggerExeProcSelector); virtual;
    property ID: QWord read GetID;
    property ChangeStamp: QWord read GetChangeStamp;

    property NameMatchValue: TStringArray read GetNameMatchValue write SetNameMatchValue;
  end;
  TIdeDebuggerExeProcSelectorClass = class of TIdeDebuggerExeProcSelector;

  { TIdeDebuggerExeProcSelectorList }

  TIdeDebuggerExeProcSelectorList = class(specialize TDbgExeProcSelectorListTemplate<TObject, TIdeDebuggerExeProcSelector>)
  private
    FChangeStamp: QWord;
    FUpdateLock: integer;
    FNeedDoChange, FInAssign: Boolean;
  protected
    procedure DoChanged; virtual;
    procedure Changed;
    function EntryClass: TIdeDebuggerExeProcSelectorClass; virtual;
  public
    destructor Destroy; override;
    procedure Assign(ASource: TIdeDebuggerExeProcSelectorList);

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Clear; reintroduce;
    function Add(AnEntry: TIdeDebuggerExeProcSelector): integer;
    procedure Delete(AnIndex: integer);
    procedure Remove(AnEntry: TIdeDebuggerExeProcSelector);

    function HighestChangeStamp: QWord;
  end;


  TIdeDebuggerExcludeRoutineMainList = class;

  { TIdeDebuggerExcludedRoutine }

  TIdeDebuggerExcludedRoutine = class(
    specialize TRefCountedGeneric<TIdeDebuggerExeProcSelector>
    ,
    IDbgSynchronizedOriginEntryIntf
  )
  private
    FOwner: TIdeDebuggerExcludeRoutineMainList;
    FIsDeleted: Boolean;
  protected
    procedure DoChanged; override;
  public
    constructor Create(AnOwner: TIdeDebuggerExcludeRoutineMainList);
    constructor Create(AnOwner: TIdeDebuggerExcludeRoutineMainList; AnId: QWord);
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    destructor Destroy; override;
    procedure UpdateFrom(AnEntry: TIdeDebuggerExeProcSelector);
    procedure SetDeleted(AValue: Boolean);
    function IsDeleted: boolean;
    property Owner: TIdeDebuggerExcludeRoutineMainList read FOwner;
  end;

  { TIdeDebuggerExcludeRoutineMainList }

  TIdeDebuggerExcludeRoutineMainList = class(
    specialize TDbgExcludedRoutinesTemplate<TObject>,
    IDbgSynchronizedOriginListIntf,
    IDbgExcludedRoutinesIntf
  )
  private type
    TIdeDebuggerExeProcIdMap = specialize TFPGMap<QWord, TIdeDebuggerExcludedRoutine>;
  private
    FChangeStamp: QWord;
    FMap: TIdeDebuggerExeProcIdMap;
    FUpdateLock: integer;
    FNeedDoChange: Boolean;

    function GetChangeStamp: QWord;
    function GetSynchronizedListIntf: IDbgSynchronizedOriginListIntf;
    function GetSyncOriginEntry(AnIndex: Integer): IDbgSynchronizedOriginEntryIntf;

    function GetEntry(AnIndex: Integer): IDbgExeProcSelectorIntf;
    function GetEntryById(AnId: QWord): TIdeDebuggerExcludedRoutine;
    function GetEntryByIndex(AnIndex: integer): TIdeDebuggerExcludedRoutine;
  protected
    procedure Changed;
    procedure DoChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SetAllDeleted;
    procedure Add(AnEntry: TIdeDebuggerExeProcSelector);

    function Count: integer;
    property Entries[AnIndex: integer]: TIdeDebuggerExcludedRoutine read GetEntryByIndex;
    property EntriesById[AnId: QWord]: TIdeDebuggerExcludedRoutine read GetEntryById;
    property ChangeStamp: QWord read GetChangeStamp;
  end;

  { TIdeDebuggerExcludeRoutineConf }

  TIdeDebuggerExcludeRoutineConf = class(TIdeDebuggerExeProcSelector)
  private
    FName: string;
    FEnabled: Boolean;
    function GetNameMatchTextValue: string;
    procedure SetEnabled(AValue: Boolean);
    procedure SetName(AValue: string);
    procedure SetNameMatchTextValue(AValue: string);
  public
    constructor Create; override;
    procedure Assign(AnOther: TIdeDebuggerExeProcSelector); override;
    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
  published
    property Name: string read FName write SetName;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FileMatchKind;
    property FileMatchValue;
    property NameMatchKind;
    property NameMatchTextValue: string read GetNameMatchTextValue write SetNameMatchTextValue;
  end;

  { TIdeDebuggerExcludeRoutineConfList }

  TIdeDebuggerExcludeRoutineConfList = class(
    specialize TChangeNotificationGeneric<TIdeDebuggerExeProcSelectorList>,
    IIdeDbgConfigItemCheckListIntf
  )
  private
    function DcclItem(AnIndex: integer): TObject;
    function DcclItemName(AnIndex: integer): String;
    function DcclItemEnabled(AnIndex: integer): Boolean;
    procedure DcclSetItemEnabled(AnIndex: integer; AValue: Boolean);
    procedure DcclMoveItem(AnIndex, ANewIndex: integer);
    function GetIdeEntry(AnIndex: integer): TIdeDebuggerExcludeRoutineConf;
  protected
    function EntryClass: TIdeDebuggerExeProcSelectorClass; override;
    procedure DoChanged; override;
  public
    //constructor Create;
    destructor Destroy; override;

    procedure AssignEnabledTo(ADest: TIdeDebuggerExcludeRoutineMainList);

    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);

    property Entries[AnIndex: integer]: TIdeDebuggerExcludeRoutineConf read GetIdeEntry; default;
  end;


implementation

function NextExcludedRoutinesChangeStamp: QWord;
const
  TheGlobalChangeStamp: QWord = 1;
begin
  Result := TheGlobalChangeStamp;
  inc(TheGlobalChangeStamp);
end;

function NextExcludedRoutinesID: QWord;
const
  TheGlobalNextEntryId: QWord = 0;
begin
  inc(TheGlobalNextEntryId);
  Result := TheGlobalNextEntryId;
end;

{ TIdeDebuggerExeProcSelector }

function TIdeDebuggerExeProcSelector.GetChangeStamp: QWord;
begin
  Result := FChangeStamp;
  if Result = 0 then begin
    Result := NextExcludedRoutinesChangeStamp;
    FChangeStamp := Result;
  end;
end;

procedure TIdeDebuggerExeProcSelector.Changed;
begin
  FChangeStamp := 0;
  if FUpdateLock > 0 then begin
    FNeedChanged := True;
    exit;
  end;
  FNeedChanged := False;

  inherited Changed;
  DoChanged;
end;

procedure TIdeDebuggerExeProcSelector.DoChanged;
begin
  //
end;

constructor TIdeDebuggerExeProcSelector.Create;
begin
  inherited Create;
end;

function TIdeDebuggerExeProcSelector.GetNameMatchValue: TStringArray;
begin
  Result := inherited NameMatchValue;
end;

procedure TIdeDebuggerExeProcSelector.SetNameMatchValue(AValue: TStringArray);
var
  i: integer;
  s: String;
begin
  i := Length(AValue) - 1;
  while i >= 0 do begin
    s := Trim(AValue[i]);
    if s = '' then
      Delete(AValue, i, 1)
    else
      AValue[i] := s;
    dec(i);
  end;
  inherited NameMatchValue := AValue;
end;

function TIdeDebuggerExeProcSelector.GetID: QWord;
begin
  if FId = 0 then
    FId := NextExcludedRoutinesID;
  Result := FId;
end;

procedure TIdeDebuggerExeProcSelector.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TIdeDebuggerExeProcSelector.EndUpdate;
begin
  if FUpdateLock > 0 then begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then begin
      if FNeedChanged then
        DoChanged;
    end;
  end;
end;

procedure TIdeDebuggerExeProcSelector.Assign(AnOther: TIdeDebuggerExeProcSelector);
begin
  BeginUpdate;
  FId            := AnOther.GetId; // At this point it must have one, and forever keep it
  FileMatchKind  := AnOther.FileMatchKind;
  FileMatchValue := AnOther.FileMatchValue;
  NameMatchKind  := AnOther.NameMatchKind;
  NameMatchValue := AnOther.NameMatchValue;
  FNeedChanged := False;
  FChangeStamp := AnOther.FChangeStamp; // don't use getter, copy zero for unset
  EndUpdate;
end;

{ TIdeDebuggerExeProcSelectorList }

procedure TIdeDebuggerExeProcSelectorList.DoChanged;
begin
  //
end;

procedure TIdeDebuggerExeProcSelectorList.Changed;
begin
  if not FInAssign then
    FChangeStamp := 0;
  if FUpdateLock > 0 then begin
    FNeedDoChange := True;
    exit;
  end;

  FNeedDoChange := False;
  DoChanged;
end;

function TIdeDebuggerExeProcSelectorList.EntryClass: TIdeDebuggerExeProcSelectorClass;
begin
  Result := TIdeDebuggerExeProcSelector;
end;

destructor TIdeDebuggerExeProcSelectorList.Destroy;
begin
  BeginUpdate;
  Clear;
  inherited Destroy;
  DoDestroy;
end;

procedure TIdeDebuggerExeProcSelectorList.Assign(ASource: TIdeDebuggerExeProcSelectorList);
var
  i: Integer;
  e: TIdeDebuggerExeProcSelector;
begin
  FInAssign := True;
  BeginUpdate;
  Clear;
  for i := 0 to ASource.Count - 1 do begin
    e := EntryClass.Create;
    e.Assign(ASource[i]);
    Add(e);
  end;
  EndUpdate;
  FInAssign := False;
end;

procedure TIdeDebuggerExeProcSelectorList.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TIdeDebuggerExeProcSelectorList.EndUpdate;
begin
  if FUpdateLock > 0 then begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then begin
      if FNeedDoChange then
        Changed;
    end;
  end;
end;

procedure TIdeDebuggerExeProcSelectorList.Clear;
begin
  BeginUpdate;
  inherited Clear;
  Changed;
  EndUpdate;
end;

function TIdeDebuggerExeProcSelectorList.Add(AnEntry: TIdeDebuggerExeProcSelector): integer;
begin
  BeginUpdate;
  Result := List.Add(AnEntry);
  Changed;
  EndUpdate;
end;

procedure TIdeDebuggerExeProcSelectorList.Delete(AnIndex: integer);
begin
  BeginUpdate;
  List.Delete(AnIndex);
  Changed;
  EndUpdate;
end;

procedure TIdeDebuggerExeProcSelectorList.Remove(AnEntry: TIdeDebuggerExeProcSelector);
begin
  BeginUpdate;
  List.Remove(AnEntry);
  Changed;
  EndUpdate;
end;

function TIdeDebuggerExeProcSelectorList.HighestChangeStamp: QWord;
var
  c, c2: QWord;
  i: Integer;
begin
  c := FChangeStamp;
  for i := 0 to Count - 1 do begin
    c2 := Entries[i].ChangeStamp;
    if c2 > c then
      c := c2;
  end;

  FChangeStamp := c;
  Result := FChangeStamp;
end;

{ TIdeDebuggerExcludedRoutine }

procedure TIdeDebuggerExcludedRoutine.DoChanged;
begin
  inherited DoChanged;
  FOwner.Changed;
end;

constructor TIdeDebuggerExcludedRoutine.Create(AnOwner: TIdeDebuggerExcludeRoutineMainList);
begin
  FOwner := AnOwner;
  inherited Create;
  AddReference;
end;

constructor TIdeDebuggerExcludedRoutine.Create(AnOwner: TIdeDebuggerExcludeRoutineMainList;
  AnId: QWord);
begin
  Create(AnOwner);
  FId := AnId;
end;

procedure TIdeDebuggerExcludedRoutine.BeginUpdate;
begin
  if FUpdateLock = 0 then
    AddReference;
  inherited BeginUpdate;
end;

procedure TIdeDebuggerExcludedRoutine.EndUpdate;
begin
  inherited EndUpdate;
  if FUpdateLock = 0 then
    ReleaseReference;
end;

destructor TIdeDebuggerExcludedRoutine.Destroy;
begin
  FOwner.FMap.Remove(ID);
  inherited Destroy;
end;

procedure TIdeDebuggerExcludedRoutine.UpdateFrom(AnEntry: TIdeDebuggerExeProcSelector);
begin
  assert((Id=0) or (ID=AnEntry.ID), 'TIdeDebuggerExcludedRoutine.UpdateFrom: (Id=0) or (ID=AnEntry.ID)');
  if (FChangeStamp > 0) and (FChangeStamp >= AnEntry.ChangeStamp) then
    exit;

  BeginUpdate;
  Assign(AnEntry);
  Changed;
  EndUpdate;
end;

procedure TIdeDebuggerExcludedRoutine.SetDeleted(AValue: Boolean);
begin
  if FIsDeleted = AValue then
    exit;
  FIsDeleted := AValue;
  Changed;
  if FIsDeleted then
    ReleaseReference
  else
    AddReference;
end;

function TIdeDebuggerExcludedRoutine.IsDeleted: boolean;
begin
  Result := FIsDeleted;
end;

{ TIdeDebuggerExcludeRoutineMainList }

function TIdeDebuggerExcludeRoutineMainList.Count: integer;
begin
  Result := FMap.Count;
end;

function TIdeDebuggerExcludeRoutineMainList.GetEntryById(AnId: QWord): TIdeDebuggerExcludedRoutine;
var
  i: Integer;
begin
  i := FMap.IndexOf(AnId);
  if i >= 0 then
    Result := FMap.Data[i]
  else
    Result := nil;
end;

function TIdeDebuggerExcludeRoutineMainList.GetEntryByIndex(AnIndex: integer
  ): TIdeDebuggerExcludedRoutine;
begin
  Result := FMap.Data[AnIndex];
end;

procedure TIdeDebuggerExcludeRoutineMainList.Changed;
begin
  if FUpdateLock > 0 then begin
    FNeedDoChange := True;
  end;

  FNeedDoChange := False;
  DoChanged;
end;

procedure TIdeDebuggerExcludeRoutineMainList.DoChanged;
begin
  FChangeStamp := 0;
  CallHandlersDoChange(Self);
end;

function TIdeDebuggerExcludeRoutineMainList.GetChangeStamp: QWord;
begin
  Result := FChangeStamp;
  if Result = 0 then begin
    Result := NextExcludedRoutinesChangeStamp;
    FChangeStamp := Result;
  end;
end;

function TIdeDebuggerExcludeRoutineMainList.GetSynchronizedListIntf: IDbgSynchronizedOriginListIntf;
begin
  Result := Self;
end;

function TIdeDebuggerExcludeRoutineMainList.GetSyncOriginEntry(AnIndex: Integer
  ): IDbgSynchronizedOriginEntryIntf;
begin
  Result := FMap.Data[AnIndex];
end;

function TIdeDebuggerExcludeRoutineMainList.GetEntry(AnIndex: Integer): IDbgExeProcSelectorIntf;
begin
  Result := FMap.Data[AnIndex];
end;

constructor TIdeDebuggerExcludeRoutineMainList.Create;
begin
  FMap := TIdeDebuggerExeProcIdMap.Create;
  inherited Create;
end;

destructor TIdeDebuggerExcludeRoutineMainList.Destroy;
begin
  SetAllDeleted;
  inherited Destroy;
  DoDestroy;
  FMap.Free;
end;

procedure TIdeDebuggerExcludeRoutineMainList.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TIdeDebuggerExcludeRoutineMainList.EndUpdate;
begin
  if FUpdateLock > 0 then begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then begin
      if FNeedDoChange then
        Changed;
    end;
  end;
end;

procedure TIdeDebuggerExcludeRoutineMainList.SetAllDeleted;
var
  c: Integer;
begin
  c := FMap.Count - 1;
  while c >= 0 do begin
    FMap.Data[c].SetDeleted(True);
    dec(c);
  end;
end;

procedure TIdeDebuggerExcludeRoutineMainList.Add(AnEntry: TIdeDebuggerExeProcSelector);
var
  e: TIdeDebuggerExcludedRoutine;
begin
  e := EntriesById[AnEntry.ID];

  if e <> nil then begin
    e.SetDeleted(False);
  end
  else begin
    e := TIdeDebuggerExcludedRoutine.Create(Self, AnEntry.ID); // has reference
    FMap.Add(e.Id, e);
    Changed;
  end;

  e.UpdateFrom(AnEntry)

end;

{ TIdeDebuggerExcludeRoutineConf }

procedure TIdeDebuggerExcludeRoutineConf.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  Changed;
end;

function TIdeDebuggerExcludeRoutineConf.GetNameMatchTextValue: string;
begin
  Result := string.Join(LineEnding, NameMatchValue);
end;

procedure TIdeDebuggerExcludeRoutineConf.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
  Changed;
end;

procedure TIdeDebuggerExcludeRoutineConf.SetNameMatchTextValue(AValue: string);
begin
  StringReplace(AValue, #10, #13, [rfReplaceAll]);
  NameMatchValue := AValue.Split([#13], TStringSplitOptions([TStringSplitOptions.ExcludeEmpty]));
end;

constructor TIdeDebuggerExcludeRoutineConf.Create;
begin
  inherited Create;
  FEnabled := True;
end;

procedure TIdeDebuggerExcludeRoutineConf.Assign(AnOther: TIdeDebuggerExeProcSelector);
var
  Src: TIdeDebuggerExcludeRoutineConf absolute AnOther;
begin
  BeginUpdate;
  inherited Assign(AnOther);

  if AnOther is TIdeDebuggerExcludeRoutineConf then begin
    FName    := Src.FName;
    FEnabled := Src.FEnabled;
  end;
  EndUpdate;
end;

procedure TIdeDebuggerExcludeRoutineConf.LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig;
  const APath: string);
var
  c: Boolean;
begin
  c := AConfig.CheckPropertyDefault;
  AConfig.CheckPropertyDefault := True;
  AConfig.ReadObject(APath, Self);
  AConfig.CheckPropertyDefault := c;
end;

procedure TIdeDebuggerExcludeRoutineConf.SaveDataToXMLConfig(const AConfig: TRttiXMLConfig;
  const APath: string);
var
  c: Boolean;
begin
  c := AConfig.CheckPropertyDefault;
  AConfig.CheckPropertyDefault := True;
  AConfig.WriteObject(APath, Self);
  AConfig.CheckPropertyDefault := c;
end;

{ TIdeDebuggerExcludeRoutineConfList }

function TIdeDebuggerExcludeRoutineConfList.DcclItem(AnIndex: integer): TObject;
begin
  Result := Entries[AnIndex];
end;

function TIdeDebuggerExcludeRoutineConfList.DcclItemName(AnIndex: integer): String;
begin
  Result := Entries[AnIndex].Name;
end;

function TIdeDebuggerExcludeRoutineConfList.DcclItemEnabled(AnIndex: integer): Boolean;
begin
  Result := Entries[AnIndex].Enabled;
end;

procedure TIdeDebuggerExcludeRoutineConfList.DcclSetItemEnabled(AnIndex: integer; AValue: Boolean
  );
begin
  Entries[AnIndex].Enabled := AValue;
end;

procedure TIdeDebuggerExcludeRoutineConfList.DcclMoveItem(AnIndex, ANewIndex: integer);
begin
  List.Move(AnIndex, ANewIndex);
end;

function TIdeDebuggerExcludeRoutineConfList.GetIdeEntry(AnIndex: integer
  ): TIdeDebuggerExcludeRoutineConf;
begin
  Result := TIdeDebuggerExcludeRoutineConf(inherited Entries[AnIndex]);
end;

function TIdeDebuggerExcludeRoutineConfList.EntryClass: TIdeDebuggerExeProcSelectorClass;
begin
  Result := TIdeDebuggerExcludeRoutineConf;
end;

procedure TIdeDebuggerExcludeRoutineConfList.DoChanged;
begin
  inherited DoChanged;
  CallChangeNotifications;
end;

destructor TIdeDebuggerExcludeRoutineConfList.Destroy;
begin
  inherited Destroy;
  FreeChangeNotifications;
end;

procedure TIdeDebuggerExcludeRoutineConfList.AssignEnabledTo(
  ADest: TIdeDebuggerExcludeRoutineMainList);
var
  i: Integer;
  e: TIdeDebuggerExcludeRoutineConf;
begin
  for i := 0 to Count - 1 do begin
    e := Entries[i];
    if e.Enabled then
      ADest.Add(e);
  end;
end;

procedure TIdeDebuggerExcludeRoutineConfList.LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig;
  const APath: string);
var
  i, c: Integer;
  obj: TIdeDebuggerExcludeRoutineConf;
begin
  Clear;
  c := AConfig.GetChildCount(APath);
  for i := 0 to c - 1 do begin
    obj := TIdeDebuggerExcludeRoutineConf(EntryClass.Create);
    obj.LoadDataFromXMLConfig(AConfig, APath + 'Entry[' + IntToStr(i+1) + ']/');
    Add(obj)
  end;
  CallChangeNotifications;
end;

procedure TIdeDebuggerExcludeRoutineConfList.SaveDataToXMLConfig(const AConfig: TRttiXMLConfig;
  const APath: string);
var
  i: Integer;
begin
  AConfig.DeletePath(APath);
  for i := 0 to Count - 1 do
    Entries[i].SaveDataToXMLConfig(AConfig, APath + 'Entry[' + IntToStr(i+1) + ']/');
end;

end.


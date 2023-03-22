unit IdeDebuggerBackendValueConv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Laz2_XMLCfg, LazClasses, lazCollections,
  LazDebuggerValueConverter;

type

  { TIdeDbgValueConvertSelector }

  TIdeDbgValueConvertSelector = class(TFreeNotifyingObject, ILazDbgValueConvertSelectorIntf)
  private
    FConverter: ILazDbgValueConverterIntf;
    FMatchTypeNames: TStrings;
    FEnabled: Boolean;
    FName: String;

    procedure SetConverter(AValue: ILazDbgValueConverterIntf);
  protected
    function GetConverter: ILazDbgValueConverterIntf;

    function AllowedTypeNames: TStrings;
  public
    constructor Create(AConverter: ILazDbgValueConverterIntf);
    destructor Destroy; override;

    function CreateCopy: TIdeDbgValueConvertSelector;
    procedure Assign(ASource: TIdeDbgValueConvertSelector);
    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
  published
    property Converter: ILazDbgValueConverterIntf read FConverter write SetConverter;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Name: String read FName write FName;
    property MatchTypeNames: TStrings read FMatchTypeNames;
  end;
  TIdeDbgValueConvertSelectorClass = class of TIdeDbgValueConvertSelector;

  { TIdeDbgValueConvertSelectorList }

  TIdeDbgValueConvertSelectorList = class(
    specialize TFPGObjectList<TIdeDbgValueConvertSelector>,
    ILazDbgValueConvertSelectorListIntf
  )
  private
    FLock: TLazMonitor;
    FChanged: Boolean;
    FOnChanged: TNotifyEvent;
    function Count: Integer;
    function Get(Index: Integer): ILazDbgValueConvertSelectorIntf;
    function GetIdeItems(Index: Integer): TIdeDbgValueConvertSelector;
    procedure PutIdeItems(Index: Integer; AValue: TIdeDbgValueConvertSelector);
    procedure SetChanged(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TIdeDbgValueConvertSelectorList);

    procedure Lock;
    procedure Unlock;

    procedure AssignEnabledTo(ADest: TIdeDbgValueConvertSelectorList; AnAppend: Boolean = False);

    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);

    function IdeItemByName(AName: String): TIdeDbgValueConvertSelector;

    property IdeItems[Index: Integer]: TIdeDbgValueConvertSelector read GetIdeItems write PutIdeItems; default;
    property Changed: Boolean read FChanged write SetChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

var
  ValueConverterSelectorList: TIdeDbgValueConvertSelectorList;

implementation

{ TIdeDbgValueConvertSelector }

procedure TIdeDbgValueConvertSelector.SetConverter(AValue: ILazDbgValueConverterIntf);
begin
  if FConverter = AValue then Exit;
  if FConverter <> nil then
    FConverter.ReleaseReference;
  FConverter := AValue;
  if FConverter <> nil then
    FConverter.AddReference;
end;

function TIdeDbgValueConvertSelector.GetConverter: ILazDbgValueConverterIntf;
begin
  Result := FConverter;
end;

function TIdeDbgValueConvertSelector.AllowedTypeNames: TStrings;
begin
  Result := FMatchTypeNames;
end;

constructor TIdeDbgValueConvertSelector.Create(AConverter: ILazDbgValueConverterIntf);
begin
  inherited Create;
  Converter := AConverter;
  FMatchTypeNames := TStringList.Create;
  TStringList(FMatchTypeNames).CaseSensitive := False;
  TStringList(FMatchTypeNames).Sorted := True;
end;

destructor TIdeDbgValueConvertSelector.Destroy;
begin
  inherited Destroy;
  FMatchTypeNames.Free;
  FConverter.ReleaseReference;
end;

function TIdeDbgValueConvertSelector.CreateCopy: TIdeDbgValueConvertSelector;
begin
  Result := TIdeDbgValueConvertSelectorClass(ClassType).Create(nil);
  Result.Assign(Self);
end;

procedure TIdeDbgValueConvertSelector.Assign(ASource: TIdeDbgValueConvertSelector);
begin
  Converter := ASource.FConverter.CreateCopy;
  FMatchTypeNames.Assign(ASource.FMatchTypeNames);
  FName     := ASource.FName;
  FEnabled  := ASource.FEnabled;
end;

procedure TIdeDbgValueConvertSelector.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  s: String;
  RegEntry: TLazDbgValueConvertRegistryEntryClass;
begin
  AConfig.ReadObject(APath + 'Filter/', Self);
  MatchTypeNames.CommaText := AConfig.GetValue(APath + 'Filter/MatchTypeNames', '');

  s := AConfig.GetValue(APath + 'ConvClass', '');
  RegEntry := ValueConverterRegistry.FindByConvertorClassName(s);
  if RegEntry = nil then
    exit;

  Converter := RegEntry.CreateValueConvertorIntf;
  AConfig.ReadObject(APath + 'Conv/', Converter.GetObject);
end;

procedure TIdeDbgValueConvertSelector.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
begin
  AConfig.WriteObject(APath + 'Filter/', Self);
  AConfig.SetDeleteValue(APath + 'Filter/MatchTypeNames', MatchTypeNames.CommaText, '');

  AConfig.SetValue(APath + 'ConvClass', Converter.GetObject.ClassName);
  AConfig.WriteObject(APath + 'Conv/', Converter.GetObject);
end;

{ TIdeDbgValueConvertSelectorList }

function TIdeDbgValueConvertSelectorList.Count: Integer;
begin
  Result := inherited Count;
end;

function TIdeDbgValueConvertSelectorList.Get(Index: Integer
  ): ILazDbgValueConvertSelectorIntf;
begin
  Result := Items[Index];
end;

function TIdeDbgValueConvertSelectorList.GetIdeItems(Index: Integer
  ): TIdeDbgValueConvertSelector;
begin
  Result := TIdeDbgValueConvertSelector(Items[Index]);
  assert(Result is TIdeDbgValueConvertSelector, 'TIdeDbgValueConvertSelectorList.GetIdeItems: Result is TIdeDbgValueConvertSelector');
end;

procedure TIdeDbgValueConvertSelectorList.PutIdeItems(Index: Integer;
  AValue: TIdeDbgValueConvertSelector);
begin
  Items[Index] := AValue;
end;

procedure TIdeDbgValueConvertSelectorList.SetChanged(AValue: Boolean);
begin
  if FChanged = AValue then Exit;
  FChanged := AValue;
  if FChanged and (FOnChanged <> nil) then
    FOnChanged(Self);
end;

constructor TIdeDbgValueConvertSelectorList.Create;
begin
  inherited Create(True);
  FLock := TLazMonitor.create;
end;

destructor TIdeDbgValueConvertSelectorList.Destroy;
begin
  inherited Destroy;
  FLock.Free;
end;

procedure TIdeDbgValueConvertSelectorList.Assign(
  ASource: TIdeDbgValueConvertSelectorList);
var
  i: Integer;
begin
  Clear;
  inherited Count := ASource.Count;
  for i := 0 to Count - 1 do
    Items[i] := ASource[i].CreateCopy;
end;

procedure TIdeDbgValueConvertSelectorList.Lock;
begin
  FLock.Acquire;
end;

procedure TIdeDbgValueConvertSelectorList.Unlock;
begin
  FLock.Leave;
end;

procedure TIdeDbgValueConvertSelectorList.AssignEnabledTo(
  ADest: TIdeDbgValueConvertSelectorList; AnAppend: Boolean);
var
  i: Integer;
begin
  if not AnAppend then
    ADest.Clear;

  for i := 0 to Count - 1 do
    if IdeItems[i].Enabled then
      ADest.Add(Items[i].CreateCopy);
end;

procedure TIdeDbgValueConvertSelectorList.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i, c: Integer;
  obj: TIdeDbgValueConvertSelector;
begin
  clear;
  c := AConfig.GetChildCount(APath);
  for i := 0 to c - 1 do begin
    obj := TIdeDbgValueConvertSelector.Create(nil);
    obj.LoadDataFromXMLConfig(AConfig, APath + 'Entry[' + IntToStr(i+1) + ']/');
    if obj.Converter <> nil then
      Add(obj)
    else
      obj.Free;
  end
end;

procedure TIdeDbgValueConvertSelectorList.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i: Integer;
begin
  AConfig.DeletePath(APath);
  for i := 0 to Count - 1 do
    IdeItems[i].SaveDataToXMLConfig(AConfig, APath + 'Entry[' + IntToStr(i+1) + ']/');
end;

function TIdeDbgValueConvertSelectorList.IdeItemByName(AName: String
  ): TIdeDbgValueConvertSelector;
var
  i: Integer;
begin
  Result := nil;
  i := Count - 1;
  while (i >= 0) and (IdeItems[i].Name <> AName) do
    dec(i);
  if i >= 0 then
    Result := IdeItems[i];
end;

initialization
  ValueConverterSelectorList := TIdeDbgValueConvertSelectorList.Create;
  ValueConverterConfigList := ValueConverterSelectorList;

finalization
  ValueConverterConfigList := nil;
  FreeAndNil(ValueConverterSelectorList);

end.


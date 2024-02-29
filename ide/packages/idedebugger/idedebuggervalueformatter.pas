unit IdeDebuggerValueFormatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, StrUtils,
  Laz2_XMLCfg,
  // LazDebuggerIntf
  LazDebuggerIntf, DbgUtilsTypePatternList,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf, IdeDebuggerDisplayFormats;

type

  TLazDbgIdeValFormatterOriginalValue = (vfovHide, vfovAtEnd, vfovAtFront);

  { TIdeDbgValueFormatterSelector }

  TIdeDbgValueFormatterSelector = class
  private
    FOriginalValue: TLazDbgIdeValFormatterOriginalValue;
    FValFormatter: ILazDbgIdeValueFormatterIntf;
    FFilterDisplayFormats: TValueDisplayFormats;
    FMatchTypeNames: TDbgTypePatternList;
    FEnabled: Boolean;
    FName: String;
    FValFormatterRegEntry: TLazDbgIdeValueFormatterRegistryEntryClass;

    procedure FreeValFormater;
    function GetMatchInherited: boolean;
    function GetMatchTypeNames: TStrings;
    procedure SetFilterDisplayFormats(AValue: TValueDisplayFormats);
  public
    constructor Create;
    constructor Create(AFormatter: TLazDbgIdeValueFormatterRegistryEntryClass);
    destructor Destroy; override;

    function CreateCopy: TIdeDbgValueFormatterSelector;
    procedure Assign(ASource: TIdeDbgValueFormatterSelector);
    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);

    function IsMatchingTypeName(ATypeName: String): boolean;
    function IsMatchingInheritedTypeName(ATypeName: String): boolean;
    property FilterDisplayFormats: TValueDisplayFormats read FFilterDisplayFormats write SetFilterDisplayFormats;
  published
    property ValFormatter: ILazDbgIdeValueFormatterIntf read FValFormatter;
    property ValFormatterRegEntry: TLazDbgIdeValueFormatterRegistryEntryClass read FValFormatterRegEntry;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Name: String read FName write FName;
    property OriginalValue: TLazDbgIdeValFormatterOriginalValue read FOriginalValue write FOriginalValue;
    property MatchTypeNames: TStrings read GetMatchTypeNames;
    property MatchInherited: boolean read GetMatchInherited;
  end;
  TIdeDbgValueFormatterSelectorClass = class of TIdeDbgValueFormatterSelector;

  { TIdeDbgValueFormatterSelectorList }

  TIdeDbgValueFormatterSelectorList = class(specialize TFPGObjectList<TIdeDbgValueFormatterSelector>)
  private
    FChanged: Boolean;
    FDefaultsAdded: integer;
    FOnChanged: TNotifyEvent;
    procedure SetChanged(AValue: Boolean);
  public
    procedure Assign(ASource: TIdeDbgValueFormatterSelectorList);
    procedure AssignEnabledTo(ADest: TIdeDbgValueFormatterSelectorList; AnAppend: Boolean = False);

    function IndexOf(AName: String): Integer; overload;

    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);

    property Changed: Boolean read FChanged write SetChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; experimental;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; deprecated 'For values from older backends only - to be removed as backends are upgraded';
  published
    // This will only be saved to disk, if "Changed = True", i.e. if the list itself changed
    // Do not copy in assign / do not clear
    property DefaultsAdded: integer read FDefaultsAdded write FDefaultsAdded;
  end;

var
  ValueFormatterSelectorList: TIdeDbgValueFormatterSelectorList;


implementation

{ TIdeDbgValueFormatterSelector }

procedure TIdeDbgValueFormatterSelector.FreeValFormater;
begin
  if FValFormatter <> nil then
    FValFormatter.Free;
  FValFormatter := nil;
end;

function TIdeDbgValueFormatterSelector.GetMatchInherited: boolean;
begin
  Result := FMatchTypeNames.MatchesInheritedTypes;
end;

function TIdeDbgValueFormatterSelector.GetMatchTypeNames: TStrings;
begin
  Result := FMatchTypeNames;
end;

procedure TIdeDbgValueFormatterSelector.SetFilterDisplayFormats( AValue: TValueDisplayFormats);
begin
  if FFilterDisplayFormats = AValue then Exit;
  if FValFormatter <> nil then
    AValue := AValue * DisplayFormatMask(FValFormatter.SupportedDisplayFormatFilters);
  FFilterDisplayFormats := AValue;
end;

constructor TIdeDbgValueFormatterSelector.Create;
begin
  inherited Create;
  FMatchTypeNames := TDbgTypePatternList.Create;
  FFilterDisplayFormats := [low(TValueDisplayFormats)..high(TValueDisplayFormats)] - [vdfCategoryMemDump];
end;

constructor TIdeDbgValueFormatterSelector.Create(
  AFormatter: TLazDbgIdeValueFormatterRegistryEntryClass);
begin
  Create;

  FValFormatterRegEntry := AFormatter;
  if FValFormatterRegEntry <> nil then begin
    FValFormatter := FValFormatterRegEntry.CreateValueFormatter;
    FFilterDisplayFormats := FFilterDisplayFormats * DisplayFormatMask(FValFormatter.SupportedDisplayFormatFilters);
  end;
end;

destructor TIdeDbgValueFormatterSelector.Destroy;
begin
  inherited Destroy;
  FMatchTypeNames.Free;
  FreeValFormater;
end;

function TIdeDbgValueFormatterSelector.CreateCopy: TIdeDbgValueFormatterSelector;
begin
  Result := TIdeDbgValueFormatterSelectorClass(ClassType).Create(FValFormatterRegEntry);
  Result.Assign(Self);
end;

procedure TIdeDbgValueFormatterSelector.Assign(ASource: TIdeDbgValueFormatterSelector);
begin
  FreeValFormater;

  FValFormatter := ASource.FValFormatter.CreateCopy;
  FMatchTypeNames.Assign(ASource.FMatchTypeNames);
  FName     := ASource.FName;
  FEnabled  := ASource.FEnabled;
  FOriginalValue := ASource.FOriginalValue;
  FFilterDisplayFormats := ASource.FFilterDisplayFormats;
end;

procedure TIdeDbgValueFormatterSelector.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  s: String;
  Def: TObject;
  df: TValueDisplayFormats;
begin
  FreeValFormater;
  AConfig.ReadObject(APath + 'Filter/', Self);
  MatchTypeNames.CommaText := AConfig.GetValue(APath + 'Filter/MatchTypeNames', '');

  s := AConfig.GetValue(APath + 'FormatterClass', '');
  FValFormatterRegEntry := ValueFormatterRegistry.FindByFormatterClassName(s);
  if FValFormatterRegEntry = nil then
    exit;

  FValFormatter := FValFormatterRegEntry.CreateValueFormatter;

  df := [];
  AConfig.GetValue(APath + 'FilterDisplayFormats', df, FFilterDisplayFormats, TypeInfo(TValueDisplayFormats));

  Def := FValFormatter.GetDefaultsObject;
  AConfig.ReadObject(APath + 'Formatter/', FValFormatter.GetObject, Def);
  Def.Free;
end;

procedure TIdeDbgValueFormatterSelector.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  Def: TObject;
begin
  AConfig.WriteObject(APath + 'Filter/', Self);
  AConfig.SetDeleteValue(APath + 'Filter/MatchTypeNames', MatchTypeNames.CommaText, '');

  AConfig.SetValue(APath + 'FormatterClass', FValFormatterRegEntry.GetClassName);
  AConfig.SetValue(APath + 'FilterDisplayFormats', FFilterDisplayFormats, TypeInfo(TValueDisplayFormats));

  Def := FValFormatter.GetDefaultsObject;
  AConfig.WriteObject(APath + 'Formatter/', FValFormatter.GetObject, Def);
  Def.Free;
end;

function TIdeDbgValueFormatterSelector.IsMatchingTypeName(ATypeName: String): boolean;
begin
  Result := FMatchTypeNames.CheckTypeName(ATypeName);
end;

function TIdeDbgValueFormatterSelector.IsMatchingInheritedTypeName(
  ATypeName: String): boolean;
begin
  Result := FMatchTypeNames.CheckInheritedTypeName(ATypeName);
end;

{ TIdeDbgValueFormatterSelectorList }

procedure TIdeDbgValueFormatterSelectorList.SetChanged(AValue: Boolean);
begin
  if FChanged = AValue then
    exit;
  FChanged := AValue;

  if FOnChanged <> nil then
    FOnChanged(Self);
end;

procedure TIdeDbgValueFormatterSelectorList.Assign(
  ASource: TIdeDbgValueFormatterSelectorList);
var
  i: Integer;
begin
  Clear;
  inherited Count := ASource.Count;
  for i := 0 to Count - 1 do
    Items[i] := ASource[i].CreateCopy;
end;

procedure TIdeDbgValueFormatterSelectorList.AssignEnabledTo(
  ADest: TIdeDbgValueFormatterSelectorList; AnAppend: Boolean);
var
  i: Integer;
begin
  if not AnAppend then
    ADest.Clear;

  for i := 0 to Count - 1 do
    if Items[i].Enabled then
      ADest.Add(Items[i].CreateCopy);
end;

function TIdeDbgValueFormatterSelectorList.IndexOf(AName: String): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].Name <> AName) do
    dec(Result);
end;

procedure TIdeDbgValueFormatterSelectorList.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i, c: Integer;
  obj: TIdeDbgValueFormatterSelector;
  Def: TIdeDbgValueFormatterSelectorList;
begin
  clear;
  c := AConfig.GetChildCount(APath+'Formatters/');
  for i := 0 to c - 1 do begin
    obj := TIdeDbgValueFormatterSelector.Create(nil);
    obj.LoadDataFromXMLConfig(AConfig, APath + 'Formatters/Entry[' + IntToStr(i+1) + ']/');
    if obj.ValFormatter <> nil then
      Add(obj)
    else
      obj.Free;
  end;

  Def := TIdeDbgValueFormatterSelectorList.Create;
  AConfig.ReadObject(APath+'Conf/', Self, Def);
  Def.Free;
end;

procedure TIdeDbgValueFormatterSelectorList.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i: Integer;
  Def: TIdeDbgValueFormatterSelectorList;
begin
  AConfig.DeletePath(APath+'Formatters/');
  for i := 0 to Count - 1 do
    Items[i].SaveDataToXMLConfig(AConfig, APath + 'Formatters/Entry[' + IntToStr(i+1) + ']/');

  Def := TIdeDbgValueFormatterSelectorList.Create;
  AConfig.WriteObject(APath+'Conf/', Self, Def);
  Def.Free;
end;

function TIdeDbgValueFormatterSelectorList.FormatValue(
  AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String): Boolean;
var
  i, j: Integer;
  a: IWatchResultDataIntf;
  f: TIdeDbgValueFormatterSelector;
  v: ILazDbgIdeValueFormatterIntf;
begin
  for i := 0 to Count - 1 do begin
    f := Items[i];
    v := f.ValFormatter;
    if (not (vffFormatValue in v.SupportedFeatures))
       or (v = nil)
       or
       ( ADisplayFormat.MemDump and (
           ( not(vffValueMemDump in v.SupportedFeatures) ) or
           ( (vdfgCategory in v.SupportedDisplayFormatFilters) and (not (vdfCategoryMemDump in f.FFilterDisplayFormats)) )
       ) )
       or
       ( (not ADisplayFormat.MemDump) and (
           ( not(vffValueData in v.SupportedFeatures) ) or
           ( (vdfgCategory        in v.SupportedDisplayFormatFilters) and (not (vdfCategoryData in f.FFilterDisplayFormats)) ) or
           ( (vdfgBase            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.NumBaseFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgSign            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.NumSignFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgNumChar         in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.NumCharFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgEnum            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.EnumFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgBool            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.BoolFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgChar            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.CharFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgFloat           in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.FloatFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgStruct          in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.StructFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgStructAddress   in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.StructAddrFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgPointer         in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.PointerFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgPointerDeref    in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.PointerDerefFormat in f.FFilterDisplayFormats)) )
       ) )
    then
      continue;

    if not f.IsMatchingTypeName(AWatchValue.TypeName) then begin
      if not f.MatchInherited then
        continue;
      j := AWatchValue.AnchestorCount - 1;
      while (j >= 0) do begin
        a := AWatchValue.Anchestors[j];
        if (a <> nil) and f.IsMatchingInheritedTypeName(a.TypeName) then
          break;
        dec(j);
      end;
      if j < 0 then
        Continue;
    end;
    Result := f.ValFormatter.FormatValue(AWatchValue, ADisplayFormat, AWatchResultPrinter, APrintedValue);
    if Result then begin
      case f.OriginalValue of
        vfovAtEnd:   APrintedValue := APrintedValue + ' = ' + AWatchResultPrinter.PrintWatchValue(AWatchValue, ADisplayFormat);
        vfovAtFront: APrintedValue := AWatchResultPrinter.PrintWatchValue(AWatchValue, ADisplayFormat) + ' = ' + APrintedValue;
      end;
      exit;
    end;
  end;
  Result := False;
end;

function TIdeDbgValueFormatterSelectorList.FormatValue(aDBGType: TDBGType;
  aValue: string; ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String
  ): boolean;
var
  i: Integer;
  f: TIdeDbgValueFormatterSelector;
  v: ILazDbgIdeValueFormatterIntf;
begin
  Result := aDBGType <> nil;
  if not Result then
    exit;
  for i := 0 to Count - 1 do begin
    f := Items[i];
    v := f.ValFormatter;
    if (not (vffFormatOldValue in v.SupportedFeatures))
       or (v = nil)
       or
       ( ADisplayFormat.MemDump and (
           ( not(vffValueMemDump in v.SupportedFeatures) ) or
           ( (vdfgCategory in v.SupportedDisplayFormatFilters) and (not (vdfCategoryMemDump in f.FFilterDisplayFormats)) )
       ) )
       or
       ( (not ADisplayFormat.MemDump) and (
           ( not(vffValueData in v.SupportedFeatures) ) or
           ( (vdfgCategory        in v.SupportedDisplayFormatFilters) and (not (vdfCategoryData in f.FFilterDisplayFormats)) ) or
           ( (vdfgBase            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.NumBaseFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgSign            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.NumSignFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgNumChar         in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.NumCharFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgEnum            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.EnumFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgBool            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.BoolFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgChar            in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.CharFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgFloat           in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.FloatFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgStruct          in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.StructFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgStructAddress   in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.StructAddrFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgPointer         in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.PointerFormat in f.FFilterDisplayFormats)) ) or
           ( (vdfgPointerDeref    in v.SupportedDisplayFormatFilters) and (not (ADisplayFormat.PointerDerefFormat in f.FFilterDisplayFormats)) )
       ) )
    then
      continue;

    if not f.IsMatchingTypeName(aDBGType.TypeName) then
      continue;
    Result := f.ValFormatter.FormatValue(aDBGType, aValue, ADisplayFormat, APrintedValue);
    if Result then begin
      case f.OriginalValue of
        vfovAtEnd:   APrintedValue := APrintedValue + ' = ' + aValue;
        vfovAtFront: APrintedValue := aValue + ' = ' + APrintedValue;
      end;
      exit;
    end;
  end;
  Result := False;
end;

initialization
  ValueFormatterSelectorList := TIdeDbgValueFormatterSelectorList.Create;

finalization
  FreeAndNil(ValueFormatterSelectorList);
end.


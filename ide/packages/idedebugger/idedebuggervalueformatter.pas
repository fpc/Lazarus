unit IdeDebuggerValueFormatter;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, StrUtils,
  Laz2_XMLCfg, LazClasses,
  // LazDebuggerIntf
  LazDebuggerIntf, DbgUtilsTypePatternList,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf, IdeDebuggerDisplayFormats,
  IdeDebuggerUtils;

type

  TLazDbgIdeValFormatterOriginalValue = (vfovHide, vfovAtEnd, vfovAtFront);

  IIdeDbgValueFormatterIntf = interface ['{2BEC59F6-7CD8-4CFE-B399-C25AFCADB700}']
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat; ANestLevel: integer;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String;
      const AWatchedVarUpper, AWatchedExprUpper: String // must be already uppercase
      ): Boolean;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String;
                         const AWatchedVarUpper, AWatchedExprUpper: String // must be already uppercase
                        ): boolean; deprecated 'For values from older backends only - to be removed as backends are upgraded';
  end;

  { TIdeDbgValueFormatterSelector }

  TIdeDbgValueFormatterSelector = class(TFreeNotifyingObject, IIdeDbgValueFormatterIntf)
  private
    FLimitByNestLevel: Boolean;
    FLimitByNestMax: integer;
    FLimitByNestMin: integer;
    FOriginalValue: TLazDbgIdeValFormatterOriginalValue;
    FValFormatter: ILazDbgIdeValueFormatterIntf;
    FMatchTypeNames: TDbgTypePatternList;
    FEnabled: Boolean;
    FName: String;
    FValFormatterRegEntry: TLazDbgIdeValueFormatterRegistryEntryClass;
    FInFormatValue: boolean;

    procedure FreeValFormater;
    function GetMatchInherited: boolean;
    function GetMatchTypeNames: TStrings;
    function DoFormatValue(AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean;
    function MatchesAll(AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat; ANestLevel: integer;
                        const AWatchedVarUpper, AWatchedExprUpper: String;
                        var AWatchedTypeUpper: String; var AWatchedTypeUpperDone: Boolean): Boolean;
  public
    constructor Create;
    constructor Create(AFormatter: TLazDbgIdeValueFormatterRegistryEntryClass);
    destructor Destroy; override;

    function CreateCopy: TIdeDbgValueFormatterSelector;
    procedure Assign(ASource: TIdeDbgValueFormatterSelector);
    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);

    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat; ANestLevel: integer;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String;
      const AWatchedVarUpper, AWatchedExprUpper: String // must be already uppercase
      ): Boolean; experimental;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String;
                         const AWatchedVarUpper, AWatchedExprUpper: String // must be already uppercase
                        ): boolean; deprecated 'For values from older backends only - to be removed as backends are upgraded';

    function IsMatchingTypeName(const ATypeNameUpper: String): boolean; inline;
    function IsMatchingInheritedTypeName(ATypeName: String): boolean; inline;
    function IsMatchingName(const AWatchedVarUpper, AWatchedExprUpper: String): boolean; inline;
  published
    property ValFormatter: ILazDbgIdeValueFormatterIntf read FValFormatter;
    property ValFormatterRegEntry: TLazDbgIdeValueFormatterRegistryEntryClass read FValFormatterRegEntry;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Name: String read FName write FName;
    property OriginalValue: TLazDbgIdeValFormatterOriginalValue read FOriginalValue write FOriginalValue;
    property MatchTypeNames: TStrings read GetMatchTypeNames;
    property MatchInherited: boolean read GetMatchInherited;
    property LimitByNestLevel: Boolean read FLimitByNestLevel write FLimitByNestLevel;
    property LimitByNestMin: integer read FLimitByNestMin write FLimitByNestMin;
    property LimitByNestMax: integer read FLimitByNestMax write FLimitByNestMax;
  end;
  TIdeDbgValueFormatterSelectorClass = class of TIdeDbgValueFormatterSelector;

  { TIdeDbgValueFormatterSelectorList }

  TIdeDbgValueFormatterSelectorList = class(
    specialize TChangeNotificationGeneric< specialize TFPGObjectList<TIdeDbgValueFormatterSelector> >,
    IIdeDbgValueFormatterIntf
  )
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

    function ItemByName(AName: String): TIdeDbgValueFormatterSelector;

    property Changed: Boolean read FChanged write SetChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    destructor Destroy; override;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat; ANestLevel: integer;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String;
      const AWatchedVarUpper, AWatchedExprUpper: String
      ): Boolean;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String;
                         const AWatchedVarUpper, AWatchedExprUpper: String
                        ): boolean; deprecated 'For values from older backends only - to be removed as backends are upgraded';
  published
    // This will only be saved to disk, if "Changed = True", i.e. if the list itself changed
    // Do not copy in assign / do not clear
    property DefaultsAdded: integer read FDefaultsAdded write FDefaultsAdded;
  end;

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

function TIdeDbgValueFormatterSelector.DoFormatValue(AWatchValue: IWatchResultDataIntf;
  ADisplayFormat: TWatchDisplayFormat; AWatchResultPrinter: IWatchResultPrinter; out
  APrintedValue: String): Boolean;
begin
  FInFormatValue := True;
  Result := ValFormatter.FormatValue(AWatchValue, ADisplayFormat, AWatchResultPrinter, APrintedValue);
  if Result then begin
    case OriginalValue of
      vfovAtEnd:   APrintedValue := APrintedValue + ' = ' + AWatchResultPrinter.PrintWatchValue(AWatchValue, ADisplayFormat);
      vfovAtFront: APrintedValue := AWatchResultPrinter.PrintWatchValue(AWatchValue, ADisplayFormat) + ' = ' + APrintedValue;
    end;
  end;
  FInFormatValue := False;
end;

constructor TIdeDbgValueFormatterSelector.Create;
begin
  inherited Create;
  FMatchTypeNames := TDbgTypePatternList.Create;
end;

constructor TIdeDbgValueFormatterSelector.Create(
  AFormatter: TLazDbgIdeValueFormatterRegistryEntryClass);
begin
  Create;

  FValFormatterRegEntry := AFormatter;
  if FValFormatterRegEntry <> nil then begin
    FValFormatter := FValFormatterRegEntry.CreateValueFormatter;
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

  FLimitByNestLevel := ASource.FLimitByNestLevel;
  FLimitByNestMin   := ASource.FLimitByNestMin;
  FLimitByNestMax   := ASource.FLimitByNestMax;
end;

procedure TIdeDbgValueFormatterSelector.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  s: String;
  Def, o: TObject;
  intf: ILazDbgIdeValueFormatterConfigStorageIntf;
begin
  FreeValFormater;
  AConfig.ReadObject(APath + 'Filter/', Self);
  MatchTypeNames.CommaText := AConfig.GetValue(APath + 'Filter/MatchTypeNames', '');

  s := AConfig.GetValue(APath + 'FormatterClass', '');
  FValFormatterRegEntry := ValueFormatterRegistry.FindByFormatterClassName(s);
  if FValFormatterRegEntry = nil then
    exit;

  FValFormatter := FValFormatterRegEntry.CreateValueFormatter;

  o := FValFormatter.GetObject;
  if o <> nil then begin
    Def := FValFormatter.GetDefaultsObject;
    AConfig.ReadObject(APath + 'Formatter/', o, Def);
    Def.Free;
  end;
  if FValFormatter.GetInterface(ILazDbgIdeValueFormatterConfigStorageIntf, intf) and (intf <> nil) then
    intf.LoadDataFromXMLConfig(AConfig, APath);
end;

procedure TIdeDbgValueFormatterSelector.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  Def, o: TObject;
  intf: ILazDbgIdeValueFormatterConfigStorageIntf;
begin
  AConfig.WriteObject(APath + 'Filter/', Self);
  AConfig.SetDeleteValue(APath + 'Filter/MatchTypeNames', MatchTypeNames.CommaText, '');

  AConfig.SetValue(APath + 'FormatterClass', FValFormatterRegEntry.GetClassName);

  o := FValFormatter.GetObject;
  if o <> nil then begin
    Def := FValFormatter.GetDefaultsObject;
    AConfig.WriteObject(APath + 'Formatter/', o, Def);
    Def.Free;
  end;
  if FValFormatter.GetInterface(ILazDbgIdeValueFormatterConfigStorageIntf, intf) and (intf <> nil) then
    intf.SaveDataToXMLConfig(AConfig, APath);
end;

function TIdeDbgValueFormatterSelector.MatchesAll(AWatchValue: IWatchResultDataIntf;
  ADisplayFormat: TWatchDisplayFormat; ANestLevel: integer; const AWatchedVarUpper,
  AWatchedExprUpper: String; var AWatchedTypeUpper: String; var AWatchedTypeUpperDone: Boolean
  ): Boolean;
var
  j: Integer;
  a: IWatchResultDataIntf;
begin
  Result := False;
  if (ValFormatter = nil) or
     (not (vffFormatValue in ValFormatter.SupportedFeatures)) or
     ((vffSkipOnRecursion in ValFormatter.SupportedFeatures) and FInFormatValue) or
     (not (AWatchValue.ValueKind in ValFormatter.SupportedDataKinds)) or
     ( ADisplayFormat.MemDump       and (not(vffValueMemDump in ValFormatter.SupportedFeatures)) ) or
     ( (not ADisplayFormat.MemDump) and (not(vffValueData in ValFormatter.SupportedFeatures)) ) or
     ( LimitByNestLevel and ( (LimitByNestMin > ANestLevel) or (LimitByNestMax < ANestLevel) ) )
  then
    exit;

  if not AWatchedTypeUpperDone then begin
    AWatchedTypeUpper := AnsiUpperCase(AWatchValue.TypeName);
    AWatchedTypeUpperDone := True;
  end;
  if not IsMatchingTypeName(AWatchedTypeUpper) then begin
    if not MatchInherited then
      exit;
    j := AWatchValue.AnchestorCount - 1;
    while (j >= 0) do begin
      a := AWatchValue.Anchestors[j];
      if (a <> nil) and IsMatchingInheritedTypeName(a.TypeName) then
        break;
      dec(j);
    end;
    if j < 0 then
      exit;
  end;

  if not IsMatchingName(AWatchedVarUpper, AWatchedExprUpper) then
    exit;

  Result := True;
end;

function TIdeDbgValueFormatterSelector.FormatValue(AWatchValue: IWatchResultDataIntf;
  ADisplayFormat: TWatchDisplayFormat; ANestLevel: integer;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String;
  const AWatchedVarUpper, AWatchedExprUpper: String): Boolean;
var
  AWatchedTypeUpper: String;
  AWatchedTypeUpperDone: Boolean;
begin
  AWatchedTypeUpperDone := False;
  Result := MatchesAll(AWatchValue, ADisplayFormat, ANestLevel,
    AWatchedVarUpper, AWatchedExprUpper, AWatchedTypeUpper, AWatchedTypeUpperDone);
  if Result then
    Result := DoFormatValue(AWatchValue, ADisplayFormat, AWatchResultPrinter, APrintedValue);
end;

function TIdeDbgValueFormatterSelector.FormatValue(aDBGType: TDBGType; aValue: string;
  ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String;
  const AWatchedVarUpper, AWatchedExprUpper: String): boolean;
begin
  Result := False;
  if (ValFormatter = nil) or
     (not (vffFormatOldValue in ValFormatter.SupportedFeatures)) or
     ((vffSkipOnRecursion in ValFormatter.SupportedFeatures) and FInFormatValue) or
     ( ADisplayFormat.MemDump       and (not(vffValueMemDump in ValFormatter.SupportedFeatures)) ) or
     ( (not ADisplayFormat.MemDump) and (not(vffValueData in ValFormatter.SupportedFeatures)) ) or
     ( LimitByNestLevel and (LimitByNestMin > 0) ) // only level 0 for old style watches
  then
    exit;

  if not IsMatchingTypeName(AnsiUpperCase(aDBGType.TypeName)) then
    exit;
  if not IsMatchingName(AWatchedVarUpper, AWatchedExprUpper) then
    exit;

  FInFormatValue := True;
  Result := ValFormatter.FormatValue(aDBGType, aValue, ADisplayFormat, APrintedValue);
  if Result then begin
    case OriginalValue of
      vfovAtEnd:   APrintedValue := APrintedValue + ' = ' + aValue;
      vfovAtFront: APrintedValue := aValue + ' = ' + APrintedValue;
    end;
  end;
  FInFormatValue := False;
end;

function TIdeDbgValueFormatterSelector.IsMatchingTypeName(const ATypeNameUpper: String): boolean;
begin
  Result := FMatchTypeNames.CheckUpperCaseTypeName(ATypeNameUpper);
end;

function TIdeDbgValueFormatterSelector.IsMatchingInheritedTypeName(
  ATypeName: String): boolean;
begin
  Result := FMatchTypeNames.CheckInheritedTypeName(ATypeName);
end;

function TIdeDbgValueFormatterSelector.IsMatchingName(const AWatchedVarUpper,
  AWatchedExprUpper: String): boolean;
begin
  Result := not (FMatchTypeNames.MatchesName or FMatchTypeNames.MatchesExpression);
  if Result then
    exit;
  Result := (AWatchedVarUpper = '') or FMatchTypeNames.CheckUpperCaseName(AWatchedVarUpper);
  if not Result then
    Result := (AWatchedExprUpper = '') or FMatchTypeNames.CheckUpperCaseExpression(AWatchedExprUpper);
end;

{ TIdeDbgValueFormatterSelectorList }

procedure TIdeDbgValueFormatterSelectorList.SetChanged(AValue: Boolean);
begin
  if FChanged <> AValue then begin
    FChanged := AValue;

    if FOnChanged <> nil then
      FOnChanged(Self);
  end;
  CallChangeNotifications;
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
  CallChangeNotifications;
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

function TIdeDbgValueFormatterSelectorList.ItemByName(AName: String): TIdeDbgValueFormatterSelector;
var
  i: Integer;
begin
  Result := nil;
  i := Count - 1;
  while (i >= 0) and (Items[i].Name <> AName) do
    dec(i);
  if i >= 0 then
    Result := Items[i];
end;

destructor TIdeDbgValueFormatterSelectorList.Destroy;
begin
  FreeChangeNotifications;
  inherited Destroy;
end;

function TIdeDbgValueFormatterSelectorList.FormatValue(AWatchValue: IWatchResultDataIntf;
  ADisplayFormat: TWatchDisplayFormat; ANestLevel: integer;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String; const AWatchedVarUpper,
  AWatchedExprUpper: String): Boolean;
var
  i: Integer;
  f: TIdeDbgValueFormatterSelector;
  AWatchedTypeUpper: String;
  AWatchedTypeUpperDone: Boolean;
begin
  AWatchedTypeUpperDone := False;
  for i := 0 to Count - 1 do begin
    f := Items[i];
    if not f.MatchesAll(AWatchValue, ADisplayFormat, ANestLevel,
                        AWatchedVarUpper, AWatchedExprUpper, AWatchedTypeUpper, AWatchedTypeUpperDone)
    then
      continue;

    Result := f.DoFormatValue(AWatchValue, ADisplayFormat, AWatchResultPrinter, APrintedValue);
    if Result then
      exit;
  end;
  Result := False;
end;

function TIdeDbgValueFormatterSelectorList.FormatValue(aDBGType: TDBGType; aValue: string;
  ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String; const AWatchedVarUpper,
  AWatchedExprUpper: String): boolean;
var
  i: Integer;
begin
  Result := aDBGType <> nil;
  if not Result then
    exit;
  for i := 0 to Count - 1 do begin
    Result := Items[i].FormatValue(aDBGType, aValue, ADisplayFormat, APrintedValue, AWatchedVarUpper, AWatchedExprUpper);
    if Result then
      exit;
  end;
  Result := False;
end;

end.


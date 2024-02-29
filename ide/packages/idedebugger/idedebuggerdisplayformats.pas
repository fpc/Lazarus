unit IdeDebuggerDisplayFormats;

{$mode objfpc}{$H+}

interface

uses
  // IdeIntf
  IdeDebuggerWatchValueIntf,
  // LazUtils
  Laz2_XMLCfg, Classes, fgl,
  // IdeDebugger
  IdeDebuggerStringConstants;

type
  TDisplayFormatTarget = (
    dtfGlobal,
    dtfHint,
    dtfWatches,
    dtfLocals,
    dtfInspect,
    dtfEvalMod
  );

  TWatchDisplayFormatList = specialize TFPGList<TWatchDisplayFormat>;

  { TDisplayFormatConfig }

  TDisplayFormatConfig = class
  private
    FDefaultDisplayFormats: array [TDisplayFormatTarget] of TWatchDisplayFormat;
  private
    FChanged: Boolean;
    FOnChanged: TNotifyEvent;
    function GetDefaultDisplayFormats(AnIndex: TDisplayFormatTarget): TWatchDisplayFormat;
    procedure SetDefaultDisplayFormats(AnIndex: TDisplayFormatTarget; AValue: TWatchDisplayFormat);
  public
    constructor Create;
    procedure Clear;

    procedure Assign(ASource: TDisplayFormatConfig);
    procedure AddToTargetedList(AList: TWatchDisplayFormatList; ATarget: TDisplayFormatTarget);

    procedure LoadFromXml(AXMLCfg: TRttiXMLConfig; APath: String);
    procedure SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String);

    property DefaultDisplayFormats[AnIndex: TDisplayFormatTarget]: TWatchDisplayFormat
      read GetDefaultDisplayFormats write SetDefaultDisplayFormats; default;
    property Changed: Boolean read FChanged write FChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

procedure LoadDisplayFormatFromXMLConfig(const AConfig: TXMLConfig; const APath: string; var ADisplayFormat: TWatchDisplayFormat);
procedure SaveDisplayFormatToXMLConfig(const AConfig: TXMLConfig; const APath: string; ADisplayFormat: TWatchDisplayFormat);

function DisplayFormatName(ADispFormat: TValueDisplayFormat): string;
function DisplayFormatGroupName(ADispFormat: TValueDisplayFormat): string;
function DisplayFormatGroupName(ADispFormatGroup: TValueDisplayFormatGroup): string;

function DisplayFormatCount(ADispFormats: TValueDisplayFormats): integer;
function DisplayFormatMask(ADispFormatGroups: TValueDisplayFormatGroups): TValueDisplayFormats;

const
  {$WRITEABLECONST OFF}
  DataKindToDisplayFormatGroups: array [TWatchResultDataKind] of TValueDisplayFormatGroups = (
    [],                                                 // rdkUnknown
    [],                                                 // rdkError
    [],                                                 // rdkPrePrinted
    [{pointer}],                                        // rdkString
    [],                                                 // rdkWideString
    [vdfgChar, vdfgBase, vdfgSign],                     // rdkChar
    [vdfgBase, vdfgSign, vdfgNumChar],                  // rdkSignedNumVal
    [vdfgBase, vdfgSign, vdfgNumChar],                  // rdkUnsignedNumVal
    [vdfgPointer, vdfgPointerDeref],                    // rdkPointerVal
    [vdfgFloat],                                        // rdkFloatVal
    [vdfgBool, vdfgBase, vdfgSign],                     // rdkBool
    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkEnum
    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkEnumVal
    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkSet
    [],                                                 // rdkVariant
    [],                                                 // rdkPCharOrString
    [],                                                 // rdkArray
    [vdfgStruct, vdfgStructAddress, vdfgPointer],       // rdkStruct
    [],                                                 // rdkConvertRes
    [],                                                 // rdkFunction
    [],                                                 // rdkProcedure
    [],                                                 // rdkFunctionRef
    []                                                  // rdkProcedureRe
  );

implementation

const
  {$WRITEABLECONST OFF}
  XmlDisplayFormatTargetNames: array[TDisplayFormatTarget] of string = (
    'dtfGlobal',
    'dtfHint',
    'dtfWatches',
    'dtfLocals',
    'dtfInspect',
    'dtfEvalMod'
  );

procedure LoadDisplayFormatFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
  var ADisplayFormat: TWatchDisplayFormat);
begin
  ADisplayFormat := DefaultWatchDisplayFormat;
  ADisplayFormat.MemDump := AConfig.GetValue(APath + 'IsMemDump', False);
  AConfig.GetValue(APath + 'Num',          ord(vdfBaseDefault),          ADisplayFormat.NumBaseFormat,              TypeInfo(TValueDisplayFormatBase));
  AConfig.GetValue(APath + 'Sign',         ord(vdfSignDefault),          ADisplayFormat.NumSignFormat,             TypeInfo(TValueDisplayFormatSign));
  AConfig.GetValue(APath + 'NumChar',      ord(vdfNumCharDefault),       ADisplayFormat.NumCharFormat,          TypeInfo(TValueDisplayFormatNumChar));
  AConfig.GetValue(APath + 'Enum',         ord(vdfEnumDefault),          ADisplayFormat.EnumFormat,             TypeInfo(TValueDisplayFormatEnum));
  AConfig.GetValue(APath + 'EnumBase',     ord(vdfBaseDefault),          ADisplayFormat.EnumBaseFormat,          TypeInfo(TValueDisplayFormatBase));
  AConfig.GetValue(APath + 'EnumSign',     ord(vdfSignDefault),          ADisplayFormat.EnumSignFormat,         TypeInfo(TValueDisplayFormatSign));
  AConfig.GetValue(APath + 'Bool',         ord(vdfBoolDefault),          ADisplayFormat.BoolFormat,             TypeInfo(TValueDisplayFormatBool));
  AConfig.GetValue(APath + 'BoolBase',     ord(vdfBaseDefault),          ADisplayFormat.BoolBaseFormat,          TypeInfo(TValueDisplayFormatBase));
  AConfig.GetValue(APath + 'BoolSign',     ord(vdfSignDefault),          ADisplayFormat.BoolSignFormat,         TypeInfo(TValueDisplayFormatSign));
  AConfig.GetValue(APath + 'Char',         ord(vdfCharDefault),          ADisplayFormat.CharFormat,             TypeInfo(TValueDisplayFormatChar));
  AConfig.GetValue(APath + 'CharBase',     ord(vdfBaseDefault),          ADisplayFormat.CharBaseFormat,          TypeInfo(TValueDisplayFormatBase));
  AConfig.GetValue(APath + 'CharSign',     ord(vdfSignDefault),          ADisplayFormat.CharSignFormat,         TypeInfo(TValueDisplayFormatSign));
  AConfig.GetValue(APath + 'Float',        ord(vdfFloatDefault),         ADisplayFormat.FloatFormat,            TypeInfo(TValueDisplayFormatFloat));
  AConfig.GetValue(APath + 'Struct',       ord(vdfStructDefault),        ADisplayFormat.StructFormat,           TypeInfo(TValueDisplayFormatStruct));
  AConfig.GetValue(APath + 'StructAddr',   ord(vdfStructAddressDefault), ADisplayFormat.StructAddrFormat,       TypeInfo(TValueDisplayFormatStructAddr));
  AConfig.GetValue(APath + 'StructPtr',    ord(vdfPointerDefault),       ADisplayFormat.StructPointerFormat,    TypeInfo(TValueDisplayFormatPointer));
  AConfig.GetValue(APath + 'StructBase',   ord(vdfBaseDefault),          ADisplayFormat.StructPointerBaseFormat, TypeInfo(TValueDisplayFormatBase));
  AConfig.GetValue(APath + 'StructSign',   ord(vdfSignDefault),          ADisplayFormat.StructPointerSignFormat,TypeInfo(TValueDisplayFormatSign));
  AConfig.GetValue(APath + 'Pointer',      ord(vdfPointerDefault),       ADisplayFormat.PointerFormat,          TypeInfo(TValueDisplayFormatPointer));
  AConfig.GetValue(APath + 'PointerDeref', ord(vdfPointerDerefDefault),  ADisplayFormat.PointerDerefFormat,     TypeInfo(TValueDisplayFormatPointerDeref));
  AConfig.GetValue(APath + 'PointerBase',  ord(vdfBaseDefault),          ADisplayFormat.PointerBaseFormat,       TypeInfo(TValueDisplayFormatBase));
  AConfig.GetValue(APath + 'PointerSign',  ord(vdfSignDefault),          ADisplayFormat.PointerSignFormat,      TypeInfo(TValueDisplayFormatSign));
end;

procedure SaveDisplayFormatToXMLConfig(const AConfig: TXMLConfig; const APath: string;
  ADisplayFormat: TWatchDisplayFormat);
begin
  AConfig.SetDeleteValue(APath + 'IsMemDump',    ADisplayFormat.MemDump, False);
  AConfig.SetDeleteValue(APath + 'Num',          ADisplayFormat.NumBaseFormat,              ord(vdfBaseDefault),          TypeInfo(TValueDisplayFormatBase));
  AConfig.SetDeleteValue(APath + 'Sign',         ADisplayFormat.NumSignFormat,             ord(vdfSignDefault),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'NumChar',      ADisplayFormat.NumCharFormat,          ord(vdfNumCharDefault),       TypeInfo(TValueDisplayFormatNumChar));
  AConfig.SetDeleteValue(APath + 'Enum',         ADisplayFormat.EnumFormat,             ord(vdfEnumDefault),          TypeInfo(TValueDisplayFormatEnum));
  AConfig.SetDeleteValue(APath + 'EnumBase',     ADisplayFormat.EnumBaseFormat,          ord(vdfBaseDefault),          TypeInfo(TValueDisplayFormatBase));
  AConfig.SetDeleteValue(APath + 'EnumSign',     ADisplayFormat.EnumSignFormat,         ord(vdfSignDefault),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'Bool',         ADisplayFormat.BoolFormat,             ord(vdfBoolDefault),          TypeInfo(TValueDisplayFormatBool));
  AConfig.SetDeleteValue(APath + 'BoolBase',     ADisplayFormat.BoolBaseFormat,          ord(vdfBaseDefault),          TypeInfo(TValueDisplayFormatBase));
  AConfig.SetDeleteValue(APath + 'BoolSign',     ADisplayFormat.BoolSignFormat,         ord(vdfSignDefault),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'Char',         ADisplayFormat.CharFormat,             ord(vdfCharDefault),          TypeInfo(TValueDisplayFormatChar));
  AConfig.SetDeleteValue(APath + 'CharBase',     ADisplayFormat.CharBaseFormat,          ord(vdfBaseDefault),          TypeInfo(TValueDisplayFormatBase));
  AConfig.SetDeleteValue(APath + 'CharSign',     ADisplayFormat.CharSignFormat,         ord(vdfSignDefault),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'Float',        ADisplayFormat.FloatFormat,            ord(vdfFloatDefault),         TypeInfo(TValueDisplayFormatFloat));
  AConfig.SetDeleteValue(APath + 'Struct',       ADisplayFormat.StructFormat,           ord(vdfStructDefault),        TypeInfo(TValueDisplayFormatStruct));
  AConfig.SetDeleteValue(APath + 'StructAddr',   ADisplayFormat.StructAddrFormat,       ord(vdfStructAddressDefault), TypeInfo(TValueDisplayFormatStructAddr));
  AConfig.SetDeleteValue(APath + 'StructPtr',    ADisplayFormat.StructPointerFormat,    ord(vdfPointerDefault),       TypeInfo(TValueDisplayFormatPointer));
  AConfig.SetDeleteValue(APath + 'StructBase',   ADisplayFormat.StructPointerBaseFormat, ord(vdfBaseDefault),          TypeInfo(TValueDisplayFormatBase));
  AConfig.SetDeleteValue(APath + 'StructSign',   ADisplayFormat.StructPointerSignFormat,ord(vdfSignDefault),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'Pointer',      ADisplayFormat.PointerFormat,          ord(vdfPointerDefault),       TypeInfo(TValueDisplayFormatPointer));
  AConfig.SetDeleteValue(APath + 'PointerDeref', ADisplayFormat.PointerDerefFormat,     ord(vdfPointerDerefDefault),  TypeInfo(TValueDisplayFormatPointerDeref));
  AConfig.SetDeleteValue(APath + 'PointerBase',  ADisplayFormat.PointerBaseFormat,       ord(vdfBaseDefault),          TypeInfo(TValueDisplayFormatBase));
  AConfig.SetDeleteValue(APath + 'PointerSign',  ADisplayFormat.PointerSignFormat,      ord(vdfSignDefault),          TypeInfo(TValueDisplayFormatSign));
end;

function DisplayFormatName(ADispFormat: TValueDisplayFormat): string;
begin
  Result := '?';
  WriteStr(Result, ADispFormat);
  case ADispFormat of
    vdfBaseDefault:          Result := DispFormatBaseDefault;
    vdfBaseDecimal:          Result := DispFormatBaseDecimal;
    vdfBaseHex:              Result := DispFormatBaseHex;
    vdfBaseOct:              Result := DispFormatBaseOct;
    vdfBaseBin:              Result := DispFormatBaseBin;
    vdfBasePointer:          Result := DispFormatBasePointer;
    vdfSignDefault:          Result := DispFormatSignDefault;
    vdfSignSigned:           Result := DispFormatSignSigned;
    vdfSignUnsigned:         Result := DispFormatSignUnsigned;
    vdfNumCharDefault:       Result := DispFormatNumCharDefault;
    vdfNumCharOff:           Result := DispFormatNumCharOff;
    vdfNumCharOrdAndUnicode: Result := DispFormatNumCharOrdAndUnicode;
    vdfNumCharOnlyUnicode:   Result := DispFormatNumCharOnlyUnicode;
    vdfEnumDefault:          Result := DispFormatEnumDefault;
    vdfEnumName:             Result := DispFormatEnumName;
    vdfEnumOrd:              Result := DispFormatEnumOrd;
    vdfEnumNameAndOrd:       Result := DispFormatEnumNameAndOrd;
    vdfBoolDefault:          Result := DispFormatBoolDefault;
    vdfBoolName:             Result := DispFormatBoolName;
    vdfBoolOrd:              Result := DispFormatBoolOrd;
    vdfBoolNameAndOrd:       Result := DispFormatBoolNameAndOrd;
    vdfCharDefault:          Result := DispFormatCharDefault;
    vdfCharLetter:           Result := DispFormatCharLetter;
    vdfCharOrd:              Result := DispFormatCharOrd;
    vdfCharLetterAndOrd:     Result := DispFormatCharLetterAndOrd;
    vdfFloatDefault:         Result := DispFormatFloatDefault;
    vdfFloatPoint:           Result := DispFormatFloatPoint;
    vdfFloatScientific:      Result := DispFormatFloatScientific;
    vdfStructDefault:        Result := DispFormatStructDefault;
    vdfStructValOnly:        Result := DispFormatStructValOnly;
    vdfStructFields:         Result := DispFormatStructFields;
    vdfStructFull:           Result := DispFormatStructFull;
    vdfStructAddressDefault: Result := DispFormatStructAddressDefault;
    vdfStructAddressOff:     Result := DispFormatStructAddressOff;
    vdfStructAddressOn:      Result := DispFormatStructAddressOn;
    vdfStructAddressOnly:    Result := DispFormatStructAddressOnly;
    vdfPointerDefault:       Result := DispFormatPointerDefault;
    vdfPointerAddress:       Result := DispFormatPointerAddress;
    vdfPointerTypedAddress:  Result := DispFormatPointerTypedAddress;
    vdfPointerDerefDefault:  Result := DispFormatPointerDerefDefault;
    vdfPointerDerefOff:      Result := DispFormatPointerDerefOff;
    vdfPointerDerefOn:       Result := DispFormatPointerDerefOn;
    vdfPointerDerefOnly:     Result := DispFormatPointerDerefOnly;
    vdfCategoryData:         Result := DispFormatCategoryData;
    vdfCategoryMemDump:      Result := DispFormatCategoryMemDump;
  end;
end;

function DisplayFormatGroupName(ADispFormat: TValueDisplayFormat): string;
begin
  Result := DisplayFormatGroupName(ValueDisplayFormatGroupMap[ADispFormat]);
end;

function DisplayFormatGroupName(ADispFormatGroup: TValueDisplayFormatGroup): string;
begin
  case ADispFormatGroup of
    vdfgBase:          Result := DispFormatGroupBase;
    vdfgSign:          Result := DispFormatGroupSign;
    vdfgNumChar:       Result := DispFormatGroupNumChar;
    vdfgEnum:          Result := DispFormatGroupEnum;
    vdfgBool:          Result := DispFormatGroupBool;
    vdfgChar:          Result := DispFormatGroupChar;
    vdfgFloat:         Result := DispFormatGroupFloat;
    vdfgStruct:        Result := DispFormatGroupStruct;
    vdfgStructAddress: Result := DispFormatGroupStructAddress;
    vdfgPointer:       Result := DispFormatGroupPointer;
    vdfgPointerDeref:  Result := DispFormatGroupPointerDeref;
    vdfgCategory:      Result := DispFormatGroupCategory;
    else Result := '?';
  end;
end;

function DisplayFormatCount(ADispFormats: TValueDisplayFormats): integer;
var
  d: TValueDisplayFormat;
begin
  Result := 0;
  for d := low(TValueDisplayFormat) to high(TValueDisplayFormat) do
    if d in ADispFormats then
      inc(Result);
end;

function DisplayFormatMask(ADispFormatGroups: TValueDisplayFormatGroups): TValueDisplayFormats;
var
  g: TValueDisplayFormatGroup;
begin
  Result := [];
  for g := low(TValueDisplayFormatGroup) to high(TValueDisplayFormatGroup) do
    if g in ADispFormatGroups then
      Result := Result + ValueDisplayFormatMaskMap[g];
end;

{ TDisplayFormatConfig }

function TDisplayFormatConfig.GetDefaultDisplayFormats(AnIndex: TDisplayFormatTarget
  ): TWatchDisplayFormat;
begin
  Result := FDefaultDisplayFormats[AnIndex];
end;

procedure TDisplayFormatConfig.SetDefaultDisplayFormats(AnIndex: TDisplayFormatTarget;
  AValue: TWatchDisplayFormat);
var
  c: Boolean;
begin
  c := FDefaultDisplayFormats[AnIndex] = AValue;
  FDefaultDisplayFormats[AnIndex] := AValue;
  if c then begin
    FChanged := True;
    if (FOnChanged <> nil) then
      FOnChanged(Self);
  end;
end;

constructor TDisplayFormatConfig.Create;
begin
  inherited Create;
  Clear;
end;

procedure TDisplayFormatConfig.Clear;
var
  i: TDisplayFormatTarget;
begin
  for i in TDisplayFormatTarget do
    FDefaultDisplayFormats[i] := DefaultWatchDisplayFormat;
end;

procedure TDisplayFormatConfig.Assign(ASource: TDisplayFormatConfig);
var
  i: TDisplayFormatTarget;
  c: Boolean;
begin
  c := False;
  for i in TDisplayFormatTarget do begin
    c := c or (FDefaultDisplayFormats[i] = ASource.FDefaultDisplayFormats[i]);
    FDefaultDisplayFormats[i] := ASource.FDefaultDisplayFormats[i];
  end;
  if c then begin
    FChanged := True;
    if (FOnChanged <> nil) then
      FOnChanged(Self);
  end;
end;

procedure TDisplayFormatConfig.AddToTargetedList(AList: TWatchDisplayFormatList;
  ATarget: TDisplayFormatTarget);
begin
  if not (FDefaultDisplayFormats[dtfGlobal] = DefaultWatchDisplayFormat) then
    AList.Add(FDefaultDisplayFormats[dtfGlobal]);
  if ATarget <> dtfGlobal then
    if not (FDefaultDisplayFormats[ATarget] = DefaultWatchDisplayFormat) then
      AList.Add(FDefaultDisplayFormats[ATarget]);
end;

procedure TDisplayFormatConfig.LoadFromXml(AXMLCfg: TRttiXMLConfig; APath: String);
var
  i: TDisplayFormatTarget;
begin
  for i in TDisplayFormatTarget do
    LoadDisplayFormatFromXMLConfig(AXMLCfg, APath + XmlDisplayFormatTargetNames[i] + '/', FDefaultDisplayFormats[i]);
end;

procedure TDisplayFormatConfig.SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String);
var
  i: TDisplayFormatTarget;
begin
  for i in TDisplayFormatTarget do
    SaveDisplayFormatToXMLConfig(AXMLCfg, APath + XmlDisplayFormatTargetNames[i] + '/', FDefaultDisplayFormats[i]);
end;

end.


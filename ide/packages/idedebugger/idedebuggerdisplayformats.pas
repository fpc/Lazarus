unit IdeDebuggerDisplayFormats;

{$mode objfpc}{$H+}

interface

uses
  // IdeIntf
  IdeDebuggerWatchValueIntf,
  // LazUtils
  Laz2_XMLCfg, Classes, fgl,
  // IdeDebugger
  IdeDebuggerStringConstants, IdeDebuggerUtils;

type
  TDisplayFormatTarget = (
    dtfGlobal,
    dtfHint,
    dtfWatches,
    dtfLocals,
    dtfInspect,
    dtfEvalMod
  );

  { TWatchDisplayFormatList }

  TWatchDisplayFormatList = class(specialize TFPGList<TWatchDisplayFormat>)
  protected
    function Get(Index: Integer): TWatchDisplayFormat; inline;
    procedure Put(Index: Integer; const Item: TWatchDisplayFormat); inline;
  public
    property Items[Index: Integer]: TWatchDisplayFormat read Get write Put; default;
  end;

  { TDisplayFormatConfig }

  TDisplayFormatConfig = class(specialize TChangeNotificationGeneric<TObject>)
  private
    FDefaultDisplayFormats: array [TDisplayFormatTarget] of TWatchDisplayFormat;
  private
    FGLobalDefault: Boolean;
    FChanged: Boolean;
    FOnChanged: TNotifyEvent;
    procedure DoChanged;
    function GetDefaultDisplayFormats(AnIndex: TDisplayFormatTarget): TWatchDisplayFormat;
    procedure SetChanged(AValue: Boolean);
    procedure SetDefaultDisplayFormats(AnIndex: TDisplayFormatTarget; AValue: TWatchDisplayFormat);
  public
    constructor Create(AGLobalDefault: Boolean = False);
    destructor Destroy; override;
    procedure Clear;

    procedure Assign(ASource: TDisplayFormatConfig);
    procedure AddToTargetedList(AList: TWatchDisplayFormatList; ATarget: TDisplayFormatTarget);

    procedure LoadFromXml(AXMLCfg: TRttiXMLConfig; APath: String);
    procedure SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String);

    property DefaultDisplayFormats[AnIndex: TDisplayFormatTarget]: TWatchDisplayFormat
      read GetDefaultDisplayFormats write SetDefaultDisplayFormats; default;
    property Changed: Boolean read FChanged write SetChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

procedure LoadDisplayFormatFromXMLConfig(const AConfig: TXMLConfig; const APath: string; var ADisplayFormat: TWatchDisplayFormat);
procedure SaveDisplayFormatToXMLConfig(const AConfig: TXMLConfig; const APath: string; ADisplayFormat: TWatchDisplayFormat);

function DisplayFormatName(ADispFormat: TValueDisplayFormat): string;
function DisplayFormatGroupName(ADispFormat: TValueDisplayFormat): string;
function DisplayFormatGroupName(ADispFormatGroup: TValueDisplayFormatGroup): string;

function DisplayFormatCount(ADispFormats: TValueDisplayFormats): integer;
function DisplayFormatMask(ADispFormatGroups: TValueDisplayFormatGroups): TValueDisplayFormats;

//const
//  {$WRITEABLECONST OFF}
//  DataKindToDisplayFormatGroups: array [TWatchResultDataKind] of TValueDisplayFormatGroups = (
//    [],                                                 // rdkUnknown
//    [],                                                 // rdkError
//    [],                                                 // rdkPrePrinted
//    [{pointer}],                                        // rdkString
//    [],                                                 // rdkWideString
//    [vdfgChar, vdfgBase, vdfgSign],                     // rdkChar
//    [vdfgBase, vdfgSign, vdfgNumChar],                  // rdkSignedNumVal
//    [vdfgBase, vdfgSign, vdfgNumChar],                  // rdkUnsignedNumVal
//    [vdfgAddress, vdfgPointerDeref],                    // rdkPointerVal
//    [vdfgFloat],                                        // rdkFloatVal
//    [vdfgBool, vdfgBase, vdfgSign],                     // rdkBool
//    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkEnum
//    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkEnumVal
//    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkSet
//    [],                                                 // rdkVariant
//    [],                                                 // rdkPCharOrString
//    [],                                                 // rdkArray
//    [vdfgStruct, vdfgStructAddress, vdfgAddress],       // rdkStruct
//    [],                                                 // rdkConvertRes
//    [],                                                 // rdkFunction
//    [],                                                 // rdkProcedure
//    [],                                                 // rdkFunctionRef
//    []                                                  // rdkProcedureRe
//  );

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

  ADisplayFormat.Num.UseInherited := AConfig.GetValue (APath + 'NumInherit', DefaultWatchDisplayFormat.Num.UseInherited);
   AConfig.GetValue(APath + 'Num',           ord(DefaultWatchDisplayFormat.Num.BaseFormat),            ADisplayFormat.Num.BaseFormat,           TypeInfo(TValueDisplayFormatBase));
   AConfig.GetValue(APath + 'Sign',          ord(DefaultWatchDisplayFormat.Num.SignFormat),            ADisplayFormat.Num.SignFormat,           TypeInfo(TValueDisplayFormatSign));
   ADisplayFormat.Num.MinDigits[vdfBaseDecimal]  := AConfig.GetValue(APath + 'DigitsDec', DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseDecimal]);
   ADisplayFormat.Num.MinDigits[vdfBaseHex]      := AConfig.GetValue(APath + 'DigitsHex', DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseHex]);
   ADisplayFormat.Num.MinDigits[vdfBaseOct]      := AConfig.GetValue(APath + 'DigitsOct', DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseOct]);
   ADisplayFormat.Num.MinDigits[vdfBaseBin]      := AConfig.GetValue(APath + 'DigitsBin', DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseBin]);
   ADisplayFormat.Num.SeparatorDec            := AConfig.GetValue(APath + 'SepDec',    DefaultWatchDisplayFormat.Num.SeparatorDec);
   AConfig.GetValue(APath + 'SepHex',        ord(DefaultWatchDisplayFormat.Num.SeparatorHexBin),       ADisplayFormat.Num.SeparatorHexBin,      TypeInfo(TValueDisplayFormatHexSeperator));
  ADisplayFormat.Num2.UseInherited               := AConfig.GetValue(APath + 'Num2Inherit', DefaultWatchDisplayFormat.Num2.UseInherited);
   ADisplayFormat.Num2.Visible                := AConfig.GetValue(APath + 'Num2.Visible', DefaultWatchDisplayFormat.Num2.Visible);
   AConfig.GetValue(APath + 'Num2',          ord(DefaultWatchDisplayFormat.Num2.BaseFormat),          ADisplayFormat.Num2.BaseFormat,          TypeInfo(TValueDisplayFormatBase));
   AConfig.GetValue(APath + 'Sign2',         ord(DefaultWatchDisplayFormat.Num2.SignFormat),          ADisplayFormat.Num2.SignFormat,          TypeInfo(TValueDisplayFormatSign));
   ADisplayFormat.Num2.MinDigits[vdfBaseDecimal] := AConfig.GetValue(APath + 'DigitsDec2', DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseDecimal]);
   ADisplayFormat.Num2.MinDigits[vdfBaseHex]     := AConfig.GetValue(APath + 'DigitsHex2', DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseHex]);
   ADisplayFormat.Num2.MinDigits[vdfBaseOct]     := AConfig.GetValue(APath + 'DigitsOct2', DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseOct]);
   ADisplayFormat.Num2.MinDigits[vdfBaseBin]     := AConfig.GetValue(APath + 'DigitsBin2', DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseBin]);
   ADisplayFormat.Num2.SeparatorDec           := AConfig.GetValue(APath + 'SepDec2',    DefaultWatchDisplayFormat.Num2.SeparatorDec);
   AConfig.GetValue(APath + 'SepHex2',        ord(DefaultWatchDisplayFormat.Num2.SeparatorHexBin),       ADisplayFormat.Num2.SeparatorHexBin,    TypeInfo(TValueDisplayFormatHexSeperator));
  ADisplayFormat.Enum.UseInherited := AConfig.GetValue(APath + 'ENumInherit', DefaultWatchDisplayFormat.Enum.UseInherited);
   AConfig.GetValue(APath + 'Enum',         ord(DefaultWatchDisplayFormat.Enum.MainFormat),               ADisplayFormat.Enum.MainFormat,              TypeInfo(TValueDisplayFormatEnum));
   AConfig.GetValue(APath + 'EnumBase',     ord(DefaultWatchDisplayFormat.Enum.BaseFormat),           ADisplayFormat.Enum.BaseFormat,          TypeInfo(TValueDisplayFormatBase));
   AConfig.GetValue(APath + 'EnumSign',     ord(DefaultWatchDisplayFormat.Enum.SignFormat),           ADisplayFormat.Enum.SignFormat,          TypeInfo(TValueDisplayFormatSign));
  ADisplayFormat.EnumVal.UseInherited := AConfig.GetValue(APath + 'ENumValInherit', DefaultWatchDisplayFormat.EnumVal.UseInherited);
   AConfig.GetValue(APath + 'EnumVal',         ord(DefaultWatchDisplayFormat.EnumVal.MainFormat),           ADisplayFormat.EnumVal.MainFormat,          TypeInfo(TValueDisplayFormatEnum));
   AConfig.GetValue(APath + 'EnumValBase',     ord(DefaultWatchDisplayFormat.EnumVal.BaseFormat),           ADisplayFormat.EnumVal.BaseFormat,          TypeInfo(TValueDisplayFormatBase));
   AConfig.GetValue(APath + 'EnumValSign',     ord(DefaultWatchDisplayFormat.EnumVal.SignFormat),           ADisplayFormat.EnumVal.SignFormat,          TypeInfo(TValueDisplayFormatSign));
  ADisplayFormat.Bool.UseInherited := AConfig.GetValue(APath + 'BoolInherit', DefaultWatchDisplayFormat.Bool.UseInherited);
   AConfig.GetValue(APath + 'Bool',         ord(DefaultWatchDisplayFormat.Bool.MainFormat),               ADisplayFormat.Bool.MainFormat,              TypeInfo(TValueDisplayFormatBool));
   AConfig.GetValue(APath + 'BoolBase',     ord(DefaultWatchDisplayFormat.Bool.BaseFormat),           ADisplayFormat.Bool.BaseFormat,          TypeInfo(TValueDisplayFormatBase));
   AConfig.GetValue(APath + 'BoolSign',     ord(DefaultWatchDisplayFormat.Bool.SignFormat),           ADisplayFormat.Bool.SignFormat,          TypeInfo(TValueDisplayFormatSign));
  ADisplayFormat.Char.UseInherited := AConfig.GetValue(APath + 'CharInherit', DefaultWatchDisplayFormat.Char.UseInherited);
   AConfig.GetValue(APath + 'Char',         ord(DefaultWatchDisplayFormat.Char.MainFormat),               ADisplayFormat.Char.MainFormat,              TypeInfo(TValueDisplayFormatChar));
   AConfig.GetValue(APath + 'CharBase',     ord(DefaultWatchDisplayFormat.Char.BaseFormat),           ADisplayFormat.Char.BaseFormat,          TypeInfo(TValueDisplayFormatBase));
   AConfig.GetValue(APath + 'CharSign',     ord(DefaultWatchDisplayFormat.Char.SignFormat),           ADisplayFormat.Char.SignFormat,          TypeInfo(TValueDisplayFormatSign));
  ADisplayFormat.Float.UseInherited := AConfig.GetValue(APath + 'FloatInherit', DefaultWatchDisplayFormat.Float.UseInherited);
   AConfig.GetValue(APath + 'Float',        ord(DefaultWatchDisplayFormat.Float.NumFormat),              ADisplayFormat.Float.NumFormat,             TypeInfo(TValueDisplayFormatFloat));
   ADisplayFormat.Float.Precission := AConfig.GetValue(APath + 'Float.Precission', DefaultWatchDisplayFormat.Float.Precission);
  ADisplayFormat.Struct.UseInherited := AConfig.GetValue(APath + 'StructInherit', DefaultWatchDisplayFormat.Struct.UseInherited);
   AConfig.GetValue(APath + 'Struct',       ord(DefaultWatchDisplayFormat.Struct.DataFormat),             ADisplayFormat.Struct.DataFormat,            TypeInfo(TValueDisplayFormatStruct));
   AConfig.GetValue(APath + 'StructPtr',    ord(DefaultWatchDisplayFormat.Struct.ShowPointerFormat),      ADisplayFormat.Struct.ShowPointerFormat,     TypeInfo(TValueDisplayFormatStructPointer));
  ADisplayFormat.Struct.Address.UseInherited := AConfig.GetValue(APath + 'StructAddrInherit', DefaultWatchDisplayFormat.Struct.Address.UseInherited);
   AConfig.GetValue(APath + 'StructAddr',   ord(DefaultWatchDisplayFormat.Struct.Address.TypeFormat),      ADisplayFormat.Struct.Address.TypeFormat,     TypeInfo(TValueDisplayFormatAddress));
   AConfig.GetValue(APath + 'StructBase',   ord(DefaultWatchDisplayFormat.Struct.Address.BaseFormat),  ADisplayFormat.Struct.Address.BaseFormat, TypeInfo(TValueDisplayFormatBase));
   ADisplayFormat.Struct.Address.Signed := AConfig.GetValue(APath + 'StructSign',   DefaultWatchDisplayFormat.Struct.Address.Signed);
   ADisplayFormat.Struct.Address.NoLeadZero := AConfig.GetValue(APath + 'StructLeadZero',   DefaultWatchDisplayFormat.Struct.Address.NoLeadZero);
  ADisplayFormat.Pointer.UseInherited := AConfig.GetValue(APath + 'PointerInherit', DefaultWatchDisplayFormat.Pointer.UseInherited);
   AConfig.GetValue(APath + 'PointerDeref', ord(DefaultWatchDisplayFormat.Pointer.DerefFormat),       ADisplayFormat.Pointer.DerefFormat,      TypeInfo(TValueDisplayFormatPointerDeref));
  ADisplayFormat.Pointer.Address.UseInherited := AConfig.GetValue(APath + 'PointerAddrInherit', DefaultWatchDisplayFormat.Pointer.Address.UseInherited);
   AConfig.GetValue(APath + 'PointerAddr',  ord(DefaultWatchDisplayFormat.Pointer.Address.TypeFormat),     ADisplayFormat.Pointer.Address.TypeFormat,    TypeInfo(TValueDisplayFormatAddress));
   AConfig.GetValue(APath + 'PointerBase',  ord(DefaultWatchDisplayFormat.Pointer.Address.BaseFormat),        ADisplayFormat.Pointer.Address.BaseFormat,       TypeInfo(TValueDisplayFormatBase));
   ADisplayFormat.Pointer.Address.Signed := AConfig.GetValue(APath + 'PointerSign', DefaultWatchDisplayFormat.Pointer.Address.Signed);
   ADisplayFormat.Pointer.Address.NoLeadZero := AConfig.GetValue(APath + 'PointerLeadZero', DefaultWatchDisplayFormat.Pointer.Address.NoLeadZero);
  ADisplayFormat.MultiLine.UseInherited := AConfig.GetValue(APath + 'MultiLineInherit', DefaultWatchDisplayFormat.MultiLine.UseInherited);
   ADisplayFormat.MultiLine.MaxMultiLineDepth := AConfig.GetValue(APath + 'MultiLineMaxWrapDepth', DefaultWatchDisplayFormat.MultiLine.MaxMultiLineDepth);
  ADisplayFormat.MemDump := AConfig.GetValue(APath + 'IsMemDump', DefaultWatchDisplayFormat.MemDump);
end;

procedure SaveDisplayFormatToXMLConfig(const AConfig: TXMLConfig; const APath: string;
  ADisplayFormat: TWatchDisplayFormat);
begin
  AConfig.SetDeleteValue(APath + 'NumInherit',         ADisplayFormat.Num.UseInherited,              DefaultWatchDisplayFormat.Num.UseInherited);
   AConfig.SetDeleteValue(APath + 'Num',               ADisplayFormat.Num.BaseFormat,             ord(DefaultWatchDisplayFormat.Num.BaseFormat),           TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'Sign',              ADisplayFormat.Num.SignFormat,             ord(DefaultWatchDisplayFormat.Num.SignFormat),           TypeInfo(TValueDisplayFormatSign));
   AConfig.SetDeleteValue(APath + 'DigitsDec',         ADisplayFormat.Num.MinDigits[vdfBaseDecimal], DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseDecimal]);
   AConfig.SetDeleteValue(APath + 'DigitsHex',         ADisplayFormat.Num.MinDigits[vdfBaseHex],     DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseHex]);
   AConfig.SetDeleteValue(APath + 'DigitsOct',         ADisplayFormat.Num.MinDigits[vdfBaseOct],     DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseOct]);
   AConfig.SetDeleteValue(APath + 'DigitsBin',         ADisplayFormat.Num.MinDigits[vdfBaseBin],     DefaultWatchDisplayFormat.Num.MinDigits[vdfBaseBin]);
   AConfig.SetDeleteValue(APath + 'SepDec',            ADisplayFormat.Num.SeparatorDec,           DefaultWatchDisplayFormat.Num.SeparatorDec);
   AConfig.SetDeleteValue(APath + 'SepHex',            ADisplayFormat.Num.SeparatorHexBin,        ord(DefaultWatchDisplayFormat.Num.SeparatorHexBin),       TypeInfo(TValueDisplayFormatHexSeperator));
  AConfig.SetDeleteValue(APath + 'Num2Inherit',        ADisplayFormat.Num2.UseInherited,             DefaultWatchDisplayFormat.Num2.UseInherited);
   AConfig.SetDeleteValue(APath + 'Num2.Visible',       ADisplayFormat.Num2.Visible,               DefaultWatchDisplayFormat.Num2.Visible);
   AConfig.SetDeleteValue(APath + 'Num2',              ADisplayFormat.Num2.BaseFormat,            ord(DefaultWatchDisplayFormat.Num2.BaseFormat),          TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'Sign2',             ADisplayFormat.Num2.SignFormat,            ord(DefaultWatchDisplayFormat.Num2.SignFormat),          TypeInfo(TValueDisplayFormatSign));
   AConfig.SetDeleteValue(APath + 'DigitsDec2',        ADisplayFormat.Num2.MinDigits[vdfBaseDecimal], DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseDecimal]);
   AConfig.SetDeleteValue(APath + 'DigitsHex2',        ADisplayFormat.Num2.MinDigits[vdfBaseHex],     DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseHex]);
   AConfig.SetDeleteValue(APath + 'DigitsOct2',        ADisplayFormat.Num2.MinDigits[vdfBaseOct],     DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseOct]);
   AConfig.SetDeleteValue(APath + 'DigitsBin2',        ADisplayFormat.Num2.MinDigits[vdfBaseBin],     DefaultWatchDisplayFormat.Num2.MinDigits[vdfBaseBin]);
   AConfig.SetDeleteValue(APath + 'SepDec2',           ADisplayFormat.Num2.SeparatorDec,           DefaultWatchDisplayFormat.Num2.SeparatorDec);
   AConfig.SetDeleteValue(APath + 'SepHex2',           ADisplayFormat.Num2.SeparatorHexBin,        ord(DefaultWatchDisplayFormat.Num2.SeparatorHexBin),       TypeInfo(TValueDisplayFormatHexSeperator));
  AConfig.SetDeleteValue(APath + 'ENumInherit',        ADisplayFormat.Enum.UseInherited,              DefaultWatchDisplayFormat.Enum.UseInherited);
   AConfig.SetDeleteValue(APath + 'Enum',              ADisplayFormat.Enum.MainFormat,                 ord(DefaultWatchDisplayFormat.Enum.MainFormat),              TypeInfo(TValueDisplayFormatEnum));
   AConfig.SetDeleteValue(APath + 'EnumBase',          ADisplayFormat.Enum.BaseFormat,             ord(DefaultWatchDisplayFormat.Enum.BaseFormat),          TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'EnumSign',          ADisplayFormat.Enum.SignFormat,             ord(DefaultWatchDisplayFormat.Enum.SignFormat),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'ENumValInherit',     ADisplayFormat.EnumVal.UseInherited,        DefaultWatchDisplayFormat.EnumVal.UseInherited);
   AConfig.SetDeleteValue(APath + 'EnumVal',           ADisplayFormat.EnumVal.MainFormat,          ord(DefaultWatchDisplayFormat.EnumVal.MainFormat),              TypeInfo(TValueDisplayFormatEnum));
   AConfig.SetDeleteValue(APath + 'EnumValBase',       ADisplayFormat.EnumVal.BaseFormat,          ord(DefaultWatchDisplayFormat.EnumVal.BaseFormat),          TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'EnumValSign',       ADisplayFormat.EnumVal.SignFormat,          ord(DefaultWatchDisplayFormat.EnumVal.SignFormat),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'BoolInherit',        ADisplayFormat.Bool.UseInherited,              DefaultWatchDisplayFormat.Bool.UseInherited);
   AConfig.SetDeleteValue(APath + 'Bool',              ADisplayFormat.Bool.MainFormat,                 ord(DefaultWatchDisplayFormat.Bool.MainFormat),              TypeInfo(TValueDisplayFormatBool));
   AConfig.SetDeleteValue(APath + 'BoolBase',          ADisplayFormat.Bool.BaseFormat,             ord(DefaultWatchDisplayFormat.Bool.BaseFormat),          TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'BoolSign',          ADisplayFormat.Bool.SignFormat,             ord(DefaultWatchDisplayFormat.Bool.SignFormat),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'CharInherit',        ADisplayFormat.Char.UseInherited,              DefaultWatchDisplayFormat.Char.UseInherited);
   AConfig.SetDeleteValue(APath + 'Char',              ADisplayFormat.Char.MainFormat,                 ord(DefaultWatchDisplayFormat.Char.MainFormat),              TypeInfo(TValueDisplayFormatChar));
   AConfig.SetDeleteValue(APath + 'CharBase',          ADisplayFormat.Char.BaseFormat,             ord(DefaultWatchDisplayFormat.Char.BaseFormat),          TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'CharSign',          ADisplayFormat.Char.SignFormat,             ord(DefaultWatchDisplayFormat.Char.SignFormat),          TypeInfo(TValueDisplayFormatSign));
  AConfig.SetDeleteValue(APath + 'FloatInherit',       ADisplayFormat.Float.UseInherited,             DefaultWatchDisplayFormat.Float.UseInherited);
   AConfig.SetDeleteValue(APath + 'Float',             ADisplayFormat.Float.NumFormat,                ord(DefaultWatchDisplayFormat.Float.NumFormat),             TypeInfo(TValueDisplayFormatFloat));
   AConfig.SetDeleteValue(APath + 'Float.Precission',   ADisplayFormat.Float.Precission,            DefaultWatchDisplayFormat.Float.Precission);
  AConfig.SetDeleteValue(APath + 'StructInherit',      ADisplayFormat.Struct.UseInherited,            DefaultWatchDisplayFormat.Struct.UseInherited);
   AConfig.SetDeleteValue(APath + 'Struct',            ADisplayFormat.Struct.DataFormat,               ord(DefaultWatchDisplayFormat.Struct.DataFormat),            TypeInfo(TValueDisplayFormatStruct));
   AConfig.SetDeleteValue(APath + 'StructPtr',         ADisplayFormat.Struct.ShowPointerFormat,        ord(DefaultWatchDisplayFormat.Struct.ShowPointerFormat),     TypeInfo(TValueDisplayFormatStructPointer));
  AConfig.SetDeleteValue(APath + 'StructAddrInherit',  ADisplayFormat.Struct.Address.UseInherited,        DefaultWatchDisplayFormat.Struct.Address.UseInherited);
   AConfig.SetDeleteValue(APath + 'StructAddr',        ADisplayFormat.Struct.Address.TypeFormat,        ord(DefaultWatchDisplayFormat.Struct.Address.TypeFormat),     TypeInfo(TValueDisplayFormatAddress));
   AConfig.SetDeleteValue(APath + 'StructBase',        ADisplayFormat.Struct.Address.BaseFormat,    ord(DefaultWatchDisplayFormat.Struct.Address.BaseFormat), TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'StructSign',        ADisplayFormat.Struct.Address.Signed,        DefaultWatchDisplayFormat.Struct.Address.Signed);
   AConfig.SetDeleteValue(APath + 'StructLeadZero',    ADisplayFormat.Struct.Address.NoLeadZero,    DefaultWatchDisplayFormat.Struct.Address.NoLeadZero);
  AConfig.SetDeleteValue(APath + 'PointerInherit',     ADisplayFormat.Pointer.UseInherited,           DefaultWatchDisplayFormat.Pointer.UseInherited);
   AConfig.SetDeleteValue(APath + 'PointerDeref',      ADisplayFormat.Pointer.DerefFormat,         ord(DefaultWatchDisplayFormat.Pointer.DerefFormat),      TypeInfo(TValueDisplayFormatPointerDeref));
  AConfig.SetDeleteValue(APath + 'PointerAddrInherit', ADisplayFormat.Pointer.Address.UseInherited,       DefaultWatchDisplayFormat.Pointer.Address.UseInherited);
   AConfig.SetDeleteValue(APath + 'PointerAddr',       ADisplayFormat.Pointer.Address.TypeFormat,       ord(DefaultWatchDisplayFormat.Pointer.Address.TypeFormat),    TypeInfo(TValueDisplayFormatAddress));
   AConfig.SetDeleteValue(APath + 'PointerBase',       ADisplayFormat.Pointer.Address.BaseFormat,   ord(DefaultWatchDisplayFormat.Pointer.Address.BaseFormat),       TypeInfo(TValueDisplayFormatBase));
   AConfig.SetDeleteValue(APath + 'PointerSign',       ADisplayFormat.Pointer.Address.Signed,       DefaultWatchDisplayFormat.Pointer.Address.Signed);
   AConfig.SetDeleteValue(APath + 'PointerLeadZero',   ADisplayFormat.Pointer.Address.NoLeadZero,   DefaultWatchDisplayFormat.Pointer.Address.NoLeadZero);
  AConfig.SetDeleteValue(APath + 'MultiLineInherit',   ADisplayFormat.MultiLine.UseInherited,       DefaultWatchDisplayFormat.MultiLine.UseInherited);
   AConfig.SetDeleteValue(APath + 'MultiLineMaxWrapDepth',ADisplayFormat.MultiLine.MaxMultiLineDepth, DefaultWatchDisplayFormat.MultiLine.MaxMultiLineDepth);
  AConfig.SetDeleteValue(APath + 'IsMemDump',          ADisplayFormat.MemDump, False);
end;

function DisplayFormatName(ADispFormat: TValueDisplayFormat): string;
begin
  Result := '?';
  WriteStr(Result, ADispFormat);
  case ADispFormat of
    vdfBaseDecimal:          Result := DispFormatBaseDecimal;
     vdfBaseHex:             Result := DispFormatBaseHex;
     vdfBaseOct:             Result := DispFormatBaseOct;
     vdfBaseBin:             Result := DispFormatBaseBin;
     vdfBaseChar:            Result := DispFormatBaseChar;
    vdfSignAuto:             Result := DispFormatSignAuto;
     vdfSignSigned:          Result := DispFormatSignSigned;
     vdfSignUnsigned:        Result := DispFormatSignUnsigned;
    vdfEnumName:             Result := DispFormatEnumName;
     vdfEnumOrd:             Result := DispFormatEnumOrd;
     vdfEnumNameAndOrd:      Result := DispFormatEnumNameAndOrd;
    vdfBoolName:             Result := DispFormatBoolName;
     vdfBoolOrd:             Result := DispFormatBoolOrd;
     vdfBoolNameAndOrd:      Result := DispFormatBoolNameAndOrd;
    vdfCharLetter:           Result := DispFormatCharLetter;
     vdfCharOrd:             Result := DispFormatCharOrd;
     vdfCharLetterAndOrd:    Result := DispFormatCharLetterAndOrd;
    vdfFloatPoint:           Result := DispFormatFloatPoint;
     vdfFloatScientific:     Result := DispFormatFloatScientific;
    vdfStructValOnly:        Result := DispFormatStructValOnly;
     vdfStructFields:        Result := DispFormatStructFields;
     vdfStructFull:          Result := DispFormatStructFull;
    vdfStructPointerOff:     Result := DispFormatStructAddressOff;
     vdfStructPointerOn:     Result := DispFormatStructAddressOn;
     vdfStructPointerOnly:   Result := DispFormatStructAddressOnly;
    vdfAddressPlain:         Result := DispFormatPointerAddressPlain;
     vdfAddressTyped:        Result := DispFormatPointerAddressTyped;
    vdfPointerDerefOff:      Result := DispFormatPointerDerefOff;
     vdfPointerDerefOn:      Result := DispFormatPointerDerefOn;
     vdfPointerDerefOnly:    Result := DispFormatPointerDerefOnly;
     vdfCategoryData:        Result := DispFormatCategoryData;
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
    vdfgEnum:          Result := DispFormatGroupEnum;
    vdfgBool:          Result := DispFormatGroupBool;
    vdfgChar:          Result := DispFormatGroupChar;
    vdfgFloat:         Result := DispFormatGroupFloat;
    vdfgStruct:        Result := DispFormatGroupStruct;
    vdfgStructAddress: Result := DispFormatGroupStructAddress;
    vdfgPointerDeref:  Result := DispFormatGroupPointerDeref;
    vdfgAddress:       Result := DispFormatGroupAddress;
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

{ TWatchDisplayFormatList }

function TWatchDisplayFormatList.Get(Index: Integer): TWatchDisplayFormat;
begin
  if (Index < 0) or (Count = 0) then
    Result := DefaultWatchDisplayFormat
  else
    Result := inherited Get(Index);
end;

procedure TWatchDisplayFormatList.Put(Index: Integer; const Item: TWatchDisplayFormat);
begin
  inherited Put(Index, Item);
end;

{ TDisplayFormatConfig }

procedure TDisplayFormatConfig.DoChanged;
begin
  if (FOnChanged <> nil) then
    FOnChanged(Self);
  CallChangeNotifications;
end;

function TDisplayFormatConfig.GetDefaultDisplayFormats(AnIndex: TDisplayFormatTarget
  ): TWatchDisplayFormat;
begin
  Result := FDefaultDisplayFormats[AnIndex];
end;

procedure TDisplayFormatConfig.SetChanged(AValue: Boolean);
begin
  FChanged := AValue;
  if AValue then
    CallChangeNotifications;
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
    DoChanged;
  end;
end;

constructor TDisplayFormatConfig.Create(AGLobalDefault: Boolean);
begin
  FGLobalDefault := AGLobalDefault;
  inherited Create;
  Clear;
end;

destructor TDisplayFormatConfig.Destroy;
begin
  inherited Destroy;
  FreeChangeNotifications;
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
    DoChanged;
  end;
end;

procedure TDisplayFormatConfig.AddToTargetedList(AList: TWatchDisplayFormatList;
  ATarget: TDisplayFormatTarget);
begin
  If FGLobalDefault then
    FDefaultDisplayFormats[dtfGlobal].MakeAllOverrides;

  if (FDefaultDisplayFormats[dtfGlobal].HasOverrides) then
    AList.Add(FDefaultDisplayFormats[dtfGlobal]);
  if ATarget <> dtfGlobal then
    if (FDefaultDisplayFormats[ATarget].HasOverrides) then
      AList.Add(FDefaultDisplayFormats[ATarget]);
end;

procedure TDisplayFormatConfig.LoadFromXml(AXMLCfg: TRttiXMLConfig; APath: String);
var
  i: TDisplayFormatTarget;
begin
  for i in TDisplayFormatTarget do
    LoadDisplayFormatFromXMLConfig(AXMLCfg, APath + XmlDisplayFormatTargetNames[i] + '/', FDefaultDisplayFormats[i]);
  CallChangeNotifications;
end;

procedure TDisplayFormatConfig.SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String);
var
  i: TDisplayFormatTarget;
begin
  for i in TDisplayFormatTarget do
    SaveDisplayFormatToXMLConfig(AXMLCfg, APath + XmlDisplayFormatTargetNames[i] + '/', FDefaultDisplayFormats[i]);
end;

end.


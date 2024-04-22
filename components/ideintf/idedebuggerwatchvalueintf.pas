{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Interface for the package IdeDebugger
}
unit IdeDebuggerWatchValueIntf experimental;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$ModeSwitch advancedrecords}

interface

uses
  LazDebuggerIntf, LazDebuggerIntfBaseTypes, LazLoggerBase, SysUtils;

type

  TValueDisplayFormatGroup = (
    vdfgBase, vdfgSign,
    vdfgEnum, vdfgBool,
    vdfgChar,
    vdfgFloat,
    vdfgStruct, vdfgStructAddress,
    vdfgPointerDeref,
    vdfgAddress,
    vdfgCategory
  );
  TValueDisplayFormatGroups = set of TValueDisplayFormatGroup;

  TValueDisplayFormat = (
    // ordinals
    vdfBaseDecimal, vdfBaseHex, vdfBaseOct, vdfBaseBin, vdfBaseChar,
    // signed numbers
    vdfSignAuto, vdfSignSigned, vdfSignUnsigned,
    // enum
    vdfEnumName, vdfEnumOrd, vdfEnumNameAndOrd,
    // bool
    vdfBoolName, vdfBoolOrd, vdfBoolNameAndOrd,
    // char
    vdfCharLetter, vdfCharOrd, vdfCharLetterAndOrd,
    // float
    vdfFloatPoint, vdfFloatScientific,
    // structures
    vdfStructValOnly, vdfStructFields, vdfStructFull,
    // structures with pointer
    vdfStructPointerOff, vdfStructPointerOn, vdfStructPointerOnly,
    // pointer deref
    vdfPointerDerefOff, vdfPointerDerefOn, vdfPointerDerefOnly,
    // address
    vdfAddressPlain, vdfAddressTyped,

    // Categories
    vdfCategoryData, vdfCategoryMemDump   // For use in the set TValueDisplayFormats
  );
  TValueDisplayFormats = set of TValueDisplayFormat;

  TValueDisplayFormatBase         = vdfBaseDecimal          .. vdfBaseChar;
  TValueDisplayFormatSign         = vdfSignAuto             .. vdfSignUnsigned;
  TValueDisplayFormatEnum         = vdfEnumName             .. vdfEnumNameAndOrd;
  TValueDisplayFormatBool         = vdfBoolName             .. vdfBoolNameAndOrd;
  TValueDisplayFormatChar         = vdfCharLetter           .. vdfCharLetterAndOrd;
  TValueDisplayFormatFloat        = vdfFloatPoint           .. vdfFloatScientific;
  TValueDisplayFormatStruct       = vdfStructValOnly        .. vdfStructFull;
  TValueDisplayFormatStructPointer= vdfStructPointerOff     .. vdfStructPointerOnly;
  TValueDisplayFormatPointerDeref = vdfPointerDerefOff      .. vdfPointerDerefOnly;
  TValueDisplayFormatAddress      = vdfAddressPlain         .. vdfAddressTyped;

  TValueDisplayFormatCategory     = vdfCategoryData .. vdfCategoryMemDump;
  TValueDisplayFormatCategories   = set of TValueDisplayFormatCategory;


  TValueDisplayFormatHexSeperator = (vdfhsNone, vdfhsByte, vdfhsWord, vdfhsLong);
  TValueDisplayFormatHexSeperators = set of TValueDisplayFormatHexSeperator;
  TValueDisplayNumDigitsArray = array [TValueDisplayFormatBase] of integer; // Entry for Char is not used

  { TWatchDisplayFormatNum }

  TWatchDisplayFormatNum2 = packed record
    UseInherited:          boolean;
    Visible:               boolean;
    BaseFormat:            TValueDisplayFormatBase;
    SignFormat:            TValueDisplayFormatSign;
    MinDigits:             TValueDisplayNumDigitsArray;
    SeparatorDec:          boolean;
    SeparatorHexBin:       TValueDisplayFormatHexSeperator;
    class operator = (a,b: TWatchDisplayFormatNum2): boolean;
  end;
  TWatchDisplayFormatNum = packed record
    UseInherited:          boolean;
    BaseFormat:            TValueDisplayFormatBase;
    SignFormat:            TValueDisplayFormatSign;
    MinDigits:             TValueDisplayNumDigitsArray;
    SeparatorDec:          boolean;
    SeparatorHexBin:       TValueDisplayFormatHexSeperator;
    class operator = (a,b: TWatchDisplayFormatNum): boolean;
    class operator := (a: TWatchDisplayFormatNum2): TWatchDisplayFormatNum;
  end;
  TWatchDisplayFormatEnum = packed record
    UseInherited:          boolean;
    MainFormat:            TValueDisplayFormatEnum;
    BaseFormat:            TValueDisplayFormatBase;
    SignFormat:            TValueDisplayFormatSign;
    class operator = (a,b: TWatchDisplayFormatEnum): boolean;
  end;
  TWatchDisplayFormatBool = packed record
    UseInherited:          boolean;
    MainFormat:            TValueDisplayFormatBool;
    BaseFormat:            TValueDisplayFormatBase;
    SignFormat:            TValueDisplayFormatSign;
    class operator = (a,b: TWatchDisplayFormatBool): boolean;
  end;
  TWatchDisplayFormatChar = packed record
    UseInherited:          boolean;
    MainFormat:            TValueDisplayFormatChar;
    BaseFormat:            TValueDisplayFormatBase;
    SignFormat:            TValueDisplayFormatSign;
    class operator = (a,b: TWatchDisplayFormatChar): boolean;
  end;
  TWatchDisplayFormatFloat = packed record
    UseInherited:          boolean;
    NumFormat:             TValueDisplayFormatFloat;
    Precission:            Integer;
    class operator = (a,b: TWatchDisplayFormatFloat): boolean;
  end;

  TWatchDisplayFormatAddr = packed record
    UseInherited:          boolean;
    TypeFormat:            TValueDisplayFormatAddress;
    BaseFormat:            TValueDisplayFormatBase;
    Signed:                boolean;
    class operator = (a,b: TWatchDisplayFormatAddr): boolean;
  end;

  TWatchDisplayFormatStruct = packed record
    UseInherited:          boolean;
    DataFormat:            TValueDisplayFormatStruct;
    ShowPointerFormat:     TValueDisplayFormatStructPointer;
    Address:               TWatchDisplayFormatAddr;
    class operator = (a,b: TWatchDisplayFormatStruct): boolean;
  end;
  TWatchDisplayFormatPointer = packed record
    UseInherited:          boolean;
    DerefFormat:           TValueDisplayFormatPointerDeref;
    Address:               TWatchDisplayFormatAddr;
    class operator = (a,b: TWatchDisplayFormatPointer): boolean;
  end;

  { TWatchDisplayFormat }

  TWatchDisplayFormat = packed record
    Num:     TWatchDisplayFormatNum;
    Num2:    TWatchDisplayFormatNum2;
    Enum:    TWatchDisplayFormatEnum;
    EnumVal: TWatchDisplayFormatEnum;
    Bool:    TWatchDisplayFormatBool;
    Char:    TWatchDisplayFormatChar;
    Float:   TWatchDisplayFormatFloat;
    Struct:  TWatchDisplayFormatStruct;
    Pointer: TWatchDisplayFormatPointer;

    MemDump: boolean;

    class operator = (a,b: TWatchDisplayFormat): boolean;
    function HasOverrides: boolean;
    procedure MakeAllOverrides;
    procedure CopyInheritedFrom(AnOther: TWatchDisplayFormat);
  end;
  PWatchDisplayFormat = ^TWatchDisplayFormat;

operator = (a,b: TValueDisplayNumDigitsArray): boolean;

const
  {$WRITEABLECONST OFF}
  DefaultWatchDisplayFormat:  TWatchDisplayFormat = (
    Num: (UseInherited:         True;
          BaseFormat:           vdfBaseDecimal;
          SignFormat:           vdfSignAuto;
          MinDigits:            (0, 0, 0, 0, 0);
          SeparatorDec:         False;
          SeparatorHexBin:      vdfhsNone;
         );
    Num2: (UseInherited:        True;
           Visible:             False;
           BaseFormat:          vdfBaseHex;
           SignFormat:          vdfSignAuto;
           MinDigits:           (0, 0, 0, 0, 0);
           SeparatorDec:        False;
           SeparatorHexBin:     vdfhsNone;
          );
    Enum: (UseInherited:        True;
           MainFormat:          vdfEnumName;
           BaseFormat:          vdfBaseDecimal;
           SignFormat:          vdfSignAuto;
          );
    EnumVal: (UseInherited:     True;
           MainFormat:          vdfEnumNameAndOrd;
           BaseFormat:          vdfBaseDecimal;
           SignFormat:          vdfSignAuto;
          );
    Bool: (UseInherited:        True;
           MainFormat:          vdfBoolName;
           BaseFormat:          vdfBaseDecimal;
           SignFormat:          vdfSignAuto;
          );
    Char: (UseInherited:        True;
           MainFormat:          vdfCharLetter;
           BaseFormat:          vdfBaseDecimal;
           SignFormat:          vdfSignAuto;
          );
    Float: (UseInherited:       True;
            NumFormat:          vdfFloatPoint;
            Precission:         0;
           );
    Struct: (UseInherited:      True;
             DataFormat:        vdfStructFields;
             ShowPointerFormat: vdfStructPointerOff;
             Address: (UseInherited:  True;
                       TypeFormat:     vdfAddressPlain;
                       BaseFormat: vdfBaseHex;
                       Signed:     False;
                      );
            );
    Pointer: (UseInherited:     True;
              DerefFormat:      vdfPointerDerefOn;
              Address: (UseInherited: True;
                        TypeFormat:    vdfAddressPlain;
                        BaseFormat:vdfBaseHex;
                        Signed:    False;
                       );
             );
    MemDump:       False;
  );

  ValueDisplayFormatGroupMap: array [TValueDisplayFormat] of TValueDisplayFormatGroup = (
    vdfgBase, vdfgBase, vdfgBase, vdfgBase, vdfgBase,
    vdfgSign, vdfgSign, vdfgSign,
    vdfgEnum, vdfgEnum, vdfgEnum,
    vdfgBool, vdfgBool, vdfgBool,
    vdfgChar, vdfgChar, vdfgChar,
    vdfgFloat, vdfgFloat,
    vdfgStruct, vdfgStruct, vdfgStruct,
    vdfgStructAddress, vdfgStructAddress, vdfgStructAddress,
    vdfgPointerDeref, vdfgPointerDeref, vdfgPointerDeref,
    vdfgAddress, vdfgAddress,
    vdfgCategory, vdfgCategory
  );

  ValueDisplayFormatMaskMap: array [TValueDisplayFormatGroup] of TValueDisplayFormats = (
    [low(TValueDisplayFormatBase)         .. high(TValueDisplayFormatBase)],         // vdfgBase
    [low(TValueDisplayFormatSign)         .. high(TValueDisplayFormatSign)],         // vdfgSign
    [low(TValueDisplayFormatEnum)         .. high(TValueDisplayFormatEnum)],         // vdfgEnum
    [low(TValueDisplayFormatBool)         .. high(TValueDisplayFormatBool)],         // vdfgBool
    [low(TValueDisplayFormatChar)         .. high(TValueDisplayFormatChar)],         // vdfgChar
    [low(TValueDisplayFormatFloat)        .. high(TValueDisplayFormatFloat)],        // vdfgFloat
    [low(TValueDisplayFormatStruct)       .. high(TValueDisplayFormatStruct)],       // vdfgStruct
    [low(TValueDisplayFormatStructPointer)   .. high(TValueDisplayFormatStructPointer)],   // vdfgStructAddress
    [low(TValueDisplayFormatPointerDeref) .. high(TValueDisplayFormatPointerDeref)], // vdfgPointerDeref
    [low(TValueDisplayFormatAddress)      .. high(TValueDisplayFormatAddress)],      // vdfgAddress
    [low(TValueDisplayFormatCategory)     .. high(TValueDisplayFormatCategory)]      // vdfgCategory
  );

type

  TWatchResultDataKind = (
    rdkUnknown,
    rdkError, rdkPrePrinted,
    rdkString, rdkWideString, rdkChar,
    rdkSignedNumVal, rdkUnsignedNumVal, rdkPointerVal, rdkFloatVal,
    rdkBool, rdkEnum, rdkEnumVal, rdkSet,
    rdkVariant,
    rdkPCharOrString,
    rdkArray,
    rdkStruct,
    rdkConvertRes,
    rdkFunction, rdkProcedure,
    rdkFunctionRef, rdkProcedureRef
  );
  TWatchResultDataKinds = set of TWatchResultDataKind;

  IWatchResultDataIntf = interface;

  (* IWatchResultPrinter & ValueFormmatters:
     If a Valueformatter's FormatValue is called, it is given an IWatchResultPrinter.
     Calling PrintWatchValue on this, will format the value WITHOUT ANY ValueFormmatters.
     This can be changed by
     - Setting a specific ValueFormatter with SetValueFormatter
     - Specifing flags
       ~ "Default Valueformatter" is the formatter that was used before calling
         (the outermost) value formatter.
         This is either the global list, or list set by the IDE, or a specific
         fomatter set by the IDE (e.g. due to watch properties specifing a
         formatter)
       ~ "Current Valueformatter" is the formatter or list that was in use before
         the current formatter was called.
         This can be the "default", or it can be the formatter specified by the
         next outer value formatter (if they are called nested)
     In either case, the top "AResValue" passed from a value formatter to
     IWatchResultPrinter is NOT going to any value formatter (to avoid circles).
     The exception is, if wpfResValueIsNestedValue is used, as then the value
     should be a nested value.
  *)
  TWatchResultPrinterFlag = (
    wpfResValueIsNestedValue,        // inc nest level / allow ValueFormatter directly on new top AResValue
    wpfUseCurrentValueFormatterList,
    wpfUseDefaultValueFormatterList
  );
  TWatchResultPrinterFlags = set of TWatchResultPrinterFlag;

  IWatchResultPrinter = interface ['{DD3F17BB-0875-4882-8D8B-348A30166984}']
    function PrintWatchValue(AResValue: IWatchResultDataIntf;
                             const ADispFormat: TWatchDisplayFormat;
                             AFlags: TWatchResultPrinterFlags = []
                            ): String;

    //procedure SetValueFormatter
  end;


  TWatchResultDataFieldInfoIntf = record
    FieldName: String;
    FieldVisibility: TLzDbgFieldVisibility;
    FieldFlags: TLzDbgFieldFlags;
    Field: IWatchResultDataIntf;
    Owner: IWatchResultDataIntf; // defined in class
  end;


  IWatchResultDataIntf = interface ['{C69FF0ED-94F1-4F1A-BB67-CCED487A3DE5}']
//    function GetClassID: TWatchResultDataClassID;
    function GetInternalObject: TObject;

    function GetValueKind: TWatchResultDataKind;
    function GetTypeName: String;

    function GetAsString: String;
    function GetAsDesc: String;
    function GetAsWideString: WideString;
    function GetAsQWord: QWord;
    function GetAsInt64: Int64;
    function GetAsFloat: Extended;

    function GetByteSize: Integer;
    function GetFloatPrecission: TLzDbgFloatPrecission; experimental;
    function GetCount: Integer;
    function GetLength: Integer;
    function GetElementName(AnIndex: integer): String;
    function GetDerefDataIntf: IWatchResultDataIntf;

    function GetArrayType: TLzDbgArrayType;
    function GetBoundTypeIntf: IWatchResultDataIntf;
    function GetLowBound: Int64;
    function GetSelectedEntryIntf: IWatchResultDataIntf;
    function GetDataAddress: TDBGPtr;
    function GetHasDataAddress: Boolean;

    function GetStructType: TLzDbgStructType;
    function GetAnchestorIntf: IWatchResultDataIntf;
    function GetAnchestorCount: Integer;
    function GetAnchestorsIntf(AnIndex: Integer): IWatchResultDataIntf;
    function GetDirectFieldCount: Integer;
    function GetFieldCount: Integer;
    function GetFieldsIntf(AnIndex: Integer): TWatchResultDataFieldInfoIntf;
//    function GetConvertedRes: IWatchResultDataIntf; experimental;

    function GetFieldVisibility: TLzDbgFieldVisibility;



//////    property ClassID: TWatchResultDataClassID read GetClassId;
    property ValueKind: TWatchResultDataKind read GetValueKind;
    property TypeName: String read GetTypeName;

    property AsString: String read GetAsString;
    property AsDesc: String read GetAsDesc;
    property AsWideString: WideString read GetAsWideString;
    property AsQWord: QWord read GetAsQWord;
    property AsInt64: Int64 read GetAsInt64;
    property AsFloat: Extended read GetAsFloat;

    property ByteSize: Integer read GetByteSize;
    property FloatPrecission: TLzDbgFloatPrecission read GetFloatPrecission; experimental;
    property DerefData: IWatchResultDataIntf read GetDerefDataIntf;

    property ElementName[AnIndex: Integer]: String read GetElementName;

    // Array
    property ArrayType: TLzDbgArrayType read GetArrayType;
    property LowBound: Int64 read GetLowBound;
    property Count: Integer read GetCount;  // Count of Entries evaluated
    property ArrayLength: Integer read GetLength; // Declared Length
    property BoundType: IWatchResultDataIntf read GetBoundTypeIntf;
    procedure SetSelectedIndex(AnIndex: Integer);
    property SelectedEntry: IWatchResultDataIntf read GetSelectedEntryIntf;

    property DataAddress: TDBGPtr read GetDataAddress;
    property HasDataAddress: Boolean read GetHasDataAddress;

    // Struct
    property StructType:  TLzDbgStructType read GetStructType;
    property Anchestor: IWatchResultDataIntf read GetAnchestorIntf;
    property AnchestorCount: Integer read GetAnchestorCount;
    property Anchestors[AnIndex: Integer]: IWatchResultDataIntf read GetAnchestorsIntf;

    property FieldCount:       Integer read GetFieldCount;
    property DirectFieldCount: Integer read GetDirectFieldCount; // without inherited fields
    property Fields[AnIndex: Integer]: TWatchResultDataFieldInfoIntf read GetFieldsIntf;

//    property ConvertedRes: IWatchResultDataIntf read GetConvertedRes; experimental;

    // variant
    property FieldVisibility: TLzDbgFieldVisibility read GetFieldVisibility;
  end;

function dbgs(vdf: TValueDisplayFormat): string; overload;
function dbgs(vdfs: TValueDisplayFormats): string; overload;
function dbgs(vds: TValueDisplayFormatHexSeperator): string; overload;
function dbgs(vds: TValueDisplayFormatHexSeperators): string; overload;
function dbgs(vdnda: TValueDisplayNumDigitsArray): string; overload;
function dbgs(df: TWatchDisplayFormat): string; overload;

implementation

{ TWatchDisplayFormat }

function dbgs(vdf: TValueDisplayFormat): string;
begin
  WriteStr(Result, vdf);
end;

function dbgs(vdfs: TValueDisplayFormats): string;
var
  d: TValueDisplayFormat;
begin
  Result := '';
  for d in TValueDisplayFormats do
    if d in vdfs then
      Result := Result + dbgs(d) + ',';
  if Result <> '' then begin
    Result := '[' + Result;
    Result[Length(Result)] := ']';
  end
  else
    Result := '[]';
end;

function dbgs(vds: TValueDisplayFormatHexSeperator): string;
begin
  WriteStr(Result, vds);
end;

function dbgs(vds: TValueDisplayFormatHexSeperators): string;
var
  d: TValueDisplayFormatHexSeperator;
begin
  Result := '';
  for d in TValueDisplayFormatHexSeperators do
    if d in vds then
      Result := Result + dbgs(d) + ',';
  if Result <> '' then begin
    Result := '[' + Result;
    Result[Length(Result)] := ']';
  end
  else
    Result := '[]';
end;

function dbgs(vdnda: TValueDisplayNumDigitsArray): string;
var
  i: TValueDisplayFormatBase;
begin
  Result := '[';
  for i := low(TValueDisplayNumDigitsArray) to high(TValueDisplayNumDigitsArray) do
    Result := Result + IntToStr(vdnda[i]) + ',';
  Result[Length(Result)] := ']';
end;

function dbgs(df: TWatchDisplayFormat): string;
begin
  Result :=
    'Num: ' +dbgs(df.Num.UseInherited)+' '+dbgs(df.Num.BaseFormat)+' '+dbgs(df.Num.SignFormat)+' '+
             dbgs(df.Num.MinDigits)+' '+dbgs(df.Num.SeparatorDec)+' '+dbgs(df.Num.SeparatorHexBin)+ LineEnding+
    'Num2: '+' '+dbgs(df.Num2.UseInherited)+' '+dbgs(df.Num2.Visible)+' '+dbgs(df.Num2.BaseFormat)+' '+dbgs(df.Num2.SignFormat)+' '+
            dbgs(df.Num2.MinDigits)+' '+dbgs(df.Num2.SeparatorDec)+' '+dbgs(df.Num2.SeparatorHexBin) + LineEnding+
    'Enum: '+dbgs(df.Enum.UseInherited)+' '+dbgs(df.Enum.MainFormat)+' '+dbgs(df.Enum.BaseFormat)+' '+dbgs(df.Enum.SignFormat) + LineEnding+
    'EnumVal: '+dbgs(df.EnumVal.UseInherited)+' '+dbgs(df.EnumVal.MainFormat)+' '+dbgs(df.EnumVal.BaseFormat)+' '+dbgs(df.EnumVal.SignFormat) + LineEnding+
    'Bool: '+dbgs(df.Bool.UseInherited)+' '+dbgs(df.Bool.MainFormat)+' '+dbgs(df.Bool.BaseFormat)+' '+dbgs(df.Bool.SignFormat) + LineEnding+
    'Char: '+dbgs(df.Char.UseInherited)+' '+dbgs(df.Char.MainFormat)+' '+dbgs(df.Char.BaseFormat)+' '+dbgs(df.Char.SignFormat) + LineEnding+
    'Float: '+dbgs(df.Float.UseInherited)+' '+dbgs(df.Float.NumFormat)+' '+dbgs(df.Float.Precission) + LineEnding+
    'Struct: '+dbgs(df.Struct.UseInherited)+' '+dbgs(df.Struct.DataFormat)+' '+dbgs(df.Struct.ShowPointerFormat) + LineEnding+
    'Addr: '+dbgs(df.Struct.Address.UseInherited)+' '+dbgs(df.Struct.Address.TypeFormat)+' '+dbgs(df.Struct.Address.BaseFormat)+' '+dbgs(df.Struct.Address.Signed) + LineEnding+
    'Ptr: '+dbgs(df.Pointer.UseInherited)+' '+dbgs(df.Pointer.DerefFormat) + LineEnding+
    'Addr: '+dbgs(df.Pointer.Address.UseInherited)+' '+dbgs(df.Pointer.Address.TypeFormat)+' '+dbgs(df.Pointer.Address.BaseFormat)+' '+dbgs(df.Pointer.Address.Signed) + LineEnding+
    'Dmp: '+dbgs(df.MemDump);
end;

operator = (a, b: TValueDisplayNumDigitsArray): boolean;
var
  i: TValueDisplayFormatBase;
begin
  Result := False;
  for i := low(a) to high(a) do
    if a[i] <> b[i] then
      exit;
  Result := True;
end;

{ TWatchDisplayFormatNum }

class operator TWatchDisplayFormatNum. = (a, b: TWatchDisplayFormatNum): boolean;
begin
  Result :=
    (a.UseInherited              = b.UseInherited) and
    (a.BaseFormat                = b.BaseFormat) and
    (a.SignFormat                = b.SignFormat) and
    (a.MinDigits                 = b.MinDigits) and
    (a.SeparatorDec              = b.SeparatorDec) and
    (a.SeparatorHexBin           = b.SeparatorHexBin)
  ;
end;

class operator TWatchDisplayFormatNum. := (a: TWatchDisplayFormatNum2): TWatchDisplayFormatNum;
begin
  Result := DefaultWatchDisplayFormat.Num;
  Result.UseInherited       := a.UseInherited;
  Result.BaseFormat         := a.BaseFormat;
  Result.SignFormat         := a.SignFormat;
  Result.MinDigits          := a.MinDigits;
  Result.SeparatorDec       := a.SeparatorDec;
  Result.SeparatorHexBin    := a.SeparatorHexBin;
end;

{ TWatchDisplayFormatNum2 }

class operator TWatchDisplayFormatNum2. = (a, b: TWatchDisplayFormatNum2): boolean;
begin
  Result :=
    (a.UseInherited             = b.UseInherited) and
    (a.Visible                  = b.Visible) and
    (a.BaseFormat               = b.BaseFormat) and
    (a.SignFormat               = b.SignFormat) and
    (a.MinDigits                = b.MinDigits) and
    (a.SeparatorDec             = b.SeparatorDec) and
    (a.SeparatorHexBin          = b.SeparatorHexBin)
  ;
end;

{ TWatchDisplayFormatEnum }

class operator TWatchDisplayFormatEnum. = (a, b: TWatchDisplayFormatEnum): boolean;
begin
  Result :=
    (a.UseInherited             = b.UseInherited) and
    (a.MainFormat               = b.MainFormat) and
    (a.BaseFormat               = b.BaseFormat) and
    (a.SignFormat               = b.SignFormat)
  ;
end;

{ TWatchDisplayFormatBool }

class operator TWatchDisplayFormatBool. = (a, b: TWatchDisplayFormatBool): boolean;
begin
  Result :=
    (a.UseInherited             = b.UseInherited) and
    (a.MainFormat               = b.MainFormat) and
    (a.BaseFormat               = b.BaseFormat) and
    (a.SignFormat               = b.SignFormat)
  ;
end;

{ TWatchDisplayFormatChar }

class operator TWatchDisplayFormatChar. = (a, b: TWatchDisplayFormatChar): boolean;
begin
  Result :=
    (a.UseInherited             = b.UseInherited) and
    (a.MainFormat               = b.MainFormat) and
    (a.BaseFormat               = b.BaseFormat) and
    (a.SignFormat               = b.SignFormat)
  ;
end;

{ TWatchDisplayFormatFloat }

class operator TWatchDisplayFormatFloat. = (a, b: TWatchDisplayFormatFloat): boolean;
begin
  Result :=
    (a.UseInherited            = b.UseInherited) and
    (a.NumFormat               = b.NumFormat) and
    (a.Precission              = b.Precission)
  ;
end;

{ TWatchDisplayFormatAddr }

class operator TWatchDisplayFormatAddr. = (a, b: TWatchDisplayFormatAddr): boolean;
begin
  Result :=
    (a.UseInherited           = b.UseInherited) and
    (a.TypeFormat             = b.TypeFormat) and
    (a.BaseFormat             = b.BaseFormat) and
    (a.Signed                 = b.Signed)
  ;
end;

{ TWatchDisplayFormatStruct }

class operator TWatchDisplayFormatStruct. = (a, b: TWatchDisplayFormatStruct): boolean;
begin
  Result :=
    (a.UseInherited           = b.UseInherited) and
    (a.DataFormat             = b.DataFormat) and
    (a.ShowPointerFormat      = b.ShowPointerFormat) and
    (a.Address                = b.Address)
  ;
end;

{ TWatchDisplayFormatPointer }

class operator TWatchDisplayFormatPointer. = (a, b: TWatchDisplayFormatPointer): boolean;
begin
  Result :=
    (a.UseInherited          = b.UseInherited) and
    (a.DerefFormat           = b.DerefFormat) and
    (a.Address               = b.Address)
  ;
end;

class operator TWatchDisplayFormat. = (a, b: TWatchDisplayFormat): boolean;
begin
  Result :=
    (a.Num     = b.Num) and
    (a.Num2    = b.Num2) and
    (a.Enum    = b.Enum) and
    (a.EnumVal = b.EnumVal) and
    (a.Bool    = b.Bool) and
    (a.Char    = b.Char) and
    (a.Float   = b.Float) and
    (a.Struct  = b.Struct) and
    (a.Pointer = b.Pointer) and
    (a.MemDump = b.MemDump);
end;

function TWatchDisplayFormat.HasOverrides: boolean;
begin
  Result := not(Num.UseInherited and Num2.UseInherited and
                Enum.UseInherited and EnumVal.UseInherited and Bool.UseInherited and Char.UseInherited and
                Float.UseInherited and
                Struct.UseInherited and Struct.Address.UseInherited and
                Pointer.UseInherited and Pointer.Address.UseInherited
               );
end;

procedure TWatchDisplayFormat.MakeAllOverrides;
begin
  Num.UseInherited             := False;
  Num2.UseInherited            := False;
  Enum.UseInherited            := False;
  EnumVal.UseInherited         := False;
  Bool.UseInherited            := False;
  Char.UseInherited            := False;
  Float.UseInherited           := False;
  Struct.UseInherited          := False;
  Struct.Address.UseInherited  := False;
  Pointer.UseInherited         := False;
  Pointer.Address.UseInherited := False;
end;

procedure TWatchDisplayFormat.CopyInheritedFrom(AnOther: TWatchDisplayFormat);
var
  a: TWatchDisplayFormatAddr;
begin
  if Num.UseInherited             then Num             := AnOther.Num;
  if Num2.UseInherited            then Num2            := AnOther.Num2;
  if Enum.UseInherited            then Enum            := AnOther.Enum;
  if EnumVal.UseInherited         then EnumVal         := AnOther.EnumVal;
  if Bool.UseInherited            then Bool            := AnOther.Bool;
  if Char.UseInherited            then Char            := AnOther.Char;
  if Float.UseInherited           then Float           := AnOther.Float;
  a := Struct.Address;
  if Struct.UseInherited          then Struct          := AnOther.Struct;
  if not a.UseInherited           then Struct.Address  := a;
  a := Pointer.Address;
  if Pointer.UseInherited         then Pointer         := AnOther.Pointer;
  if not a.UseInherited           then Pointer.Address  := a;
end;

end.


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

interface

uses
  LazDebuggerIntf, LazDebuggerIntfBaseTypes, IdeIntfStrConsts;

type

  TValueDisplayFormatGroup = (
    vdfgBase, vdfgSign, vdfgNumChar,
    vdfgEnum, vdfgBool,
    vdfgChar,
    vdfgFloat,
    vdfgStruct, vdfgStructAddress,
    vdfgPointer, vdfgPointerDeref,
    vdfgCategory
  );
  TValueDisplayFormatGroups = set of TValueDisplayFormatGroup;

  TValueDisplayFormat = (
    // ordinals
    vdfBaseDefault, vdfBaseDecimal, vdfBaseHex, vdfBaseOct, vdfBaseBin, vdfBasePointer,
    // signed numbers
    vdfSignDefault, vdfSignSigned, vdfSignUnsigned,
    // num as char
    vdfNumCharDefault, vdfNumCharOff, vdfNumCharOrdAndUnicode, vdfNumCharOnlyUnicode,
    // enum
    vdfEnumDefault, vdfEnumName, vdfEnumOrd, vdfEnumNameAndOrd,
    // bool
    vdfBoolDefault, vdfBoolName, vdfBoolOrd, vdfBoolNameAndOrd,
    // char
    vdfCharDefault, vdfCharLetter, vdfCharOrd, vdfCharLetterAndOrd,
    // float
    vdfFloatDefault, vdfFloatPoint, vdfFloatScientific,
    // structures
    vdfStructDefault, vdfStructValOnly, vdfStructFields, vdfStructFull,
    // structures with pointer
    vdfStructAddressDefault, vdfStructAddressOff, vdfStructAddressOn, vdfStructAddressOnly,
    // pointer
    vdfPointerDefault, vdfPointerAddress, vdfPointerTypedAddress,
    // pointer deref
    vdfPointerDerefDefault, vdfPointerDerefOff, vdfPointerDerefOn, vdfPointerDerefOnly,

    // Categories
    vdfCategoryData, vdfCategoryMemDump   // For use in the set TValueDisplayFormats
  );
  TValueDisplayFormats = set of TValueDisplayFormat;

  TValueDisplayFormatBase         = vdfBaseDefault          .. vdfBasePointer;
  TValueDisplayFormatSign         = vdfSignDefault          .. vdfSignUnsigned;
  TValueDisplayFormatNumChar      = vdfNumCharDefault       .. vdfNumCharOnlyUnicode;
  TValueDisplayFormatEnum         = vdfEnumDefault          .. vdfEnumNameAndOrd;
  TValueDisplayFormatBool         = vdfBoolDefault          .. vdfBoolNameAndOrd;
  TValueDisplayFormatChar         = vdfCharDefault          .. vdfCharLetterAndOrd;
  TValueDisplayFormatFloat        = vdfFloatDefault         .. vdfFloatScientific;
  TValueDisplayFormatStruct       = vdfStructDefault        .. vdfStructFull;
  TValueDisplayFormatStructAddr   = vdfStructAddressDefault .. vdfStructAddressOnly;
  TValueDisplayFormatPointer      = vdfPointerDefault       .. vdfPointerTypedAddress;
  TValueDisplayFormatPointerDeref = vdfPointerDerefDefault  .. vdfPointerDerefOnly;

  TValueDisplayFormatCategory     = vdfCategoryData .. vdfCategoryMemDump;
  TValueDisplayFormatCategories   = set of TValueDisplayFormatCategory;


  TWatchDisplayFormat = packed record
    NumBaseFormat:            TValueDisplayFormatBase;
     NumSignFormat:           TValueDisplayFormatSign;
     NumCharFormat:           TValueDisplayFormatNumChar;
    EnumFormat:               TValueDisplayFormatEnum;
     EnumBaseFormat:          TValueDisplayFormatBase;
     EnumSignFormat:          TValueDisplayFormatSign;
    BoolFormat:               TValueDisplayFormatBool;
     BoolBaseFormat:          TValueDisplayFormatBase;
     BoolSignFormat:          TValueDisplayFormatSign;
    CharFormat:               TValueDisplayFormatChar;
     CharBaseFormat:          TValueDisplayFormatBase;
     CharSignFormat:          TValueDisplayFormatSign;
    FloatFormat:              TValueDisplayFormatFloat;
    StructFormat:             TValueDisplayFormatStruct;
     StructAddrFormat:        TValueDisplayFormatStructAddr;
     StructPointerFormat:     TValueDisplayFormatPointer;
     StructPointerBaseFormat: TValueDisplayFormatBase;
     StructPointerSignFormat: TValueDisplayFormatSign;
    PointerFormat:            TValueDisplayFormatPointer;
     PointerDerefFormat:      TValueDisplayFormatPointerDeref;
     PointerBaseFormat:       TValueDisplayFormatBase;
     PointerSignFormat:       TValueDisplayFormatSign;
    MemDump: ByteBool;
  end;

const
  {$WRITEABLECONST OFF}
  DefaultWatchDisplayFormat:  TWatchDisplayFormat = (
    NumBaseFormat:            vdfBaseDefault;
    NumSignFormat:            vdfSignDefault;
    NumCharFormat:            vdfNumCharDefault;
    EnumFormat:               vdfEnumDefault;
     EnumBaseFormat:          vdfBaseDefault;
     EnumSignFormat:          vdfSignDefault;
    BoolFormat:               vdfBoolDefault;
     BoolBaseFormat:          vdfBaseDefault;
     BoolSignFormat:          vdfSignDefault;
    CharFormat:               vdfCharDefault;
     CharBaseFormat:          vdfBaseDefault;
     CharSignFormat:          vdfSignDefault;
    FloatFormat:              vdfFloatDefault;
    StructFormat:             vdfStructDefault;
     StructAddrFormat:        vdfStructAddressDefault;
     StructPointerFormat:     vdfPointerDefault;
     StructPointerBaseFormat: vdfBaseDefault;
     StructPointerSignFormat: vdfSignDefault;
    PointerFormat:            vdfPointerDefault;
     PointerDerefFormat:      vdfPointerDerefDefault;
     PointerBaseFormat:       vdfBaseDefault;
     PointerSignFormat:       vdfSignDefault;
    MemDump:       False;
  );

  ValueDisplayFormatGroupMap: array [TValueDisplayFormat] of TValueDisplayFormatGroup = (
    vdfgBase, vdfgBase, vdfgBase, vdfgBase, vdfgBase, vdfgBase,
    vdfgSign, vdfgSign, vdfgSign,
    vdfgNumChar, vdfgNumChar, vdfgNumChar, vdfgNumChar,
    vdfgEnum, vdfgEnum, vdfgEnum, vdfgEnum,
    vdfgBool, vdfgBool, vdfgBool, vdfgBool,
    vdfgChar, vdfgChar, vdfgChar, vdfgChar,
    vdfgFloat, vdfgFloat, vdfgFloat,
    vdfgStruct, vdfgStruct, vdfgStruct, vdfgStruct,
    vdfgStructAddress, vdfgStructAddress, vdfgStructAddress, vdfgStructAddress,
    vdfgPointer, vdfgPointer, vdfgPointer,
    vdfgPointerDeref, vdfgPointerDeref, vdfgPointerDeref, vdfgPointerDeref,
    vdfgCategory, vdfgCategory
  );

  ValueDisplayFormatMaskMap: array [TValueDisplayFormatGroup] of TValueDisplayFormats = (
    [low(TValueDisplayFormatBase)         .. high(TValueDisplayFormatBase)],         // vdfgBase
    [low(TValueDisplayFormatSign)         .. high(TValueDisplayFormatSign)],         // vdfgSign
    [low(TValueDisplayFormatNumChar)      .. high(TValueDisplayFormatNumChar)],      // vdfgNumChar
    [low(TValueDisplayFormatEnum)         .. high(TValueDisplayFormatEnum)],         // vdfgEnum
    [low(TValueDisplayFormatBool)         .. high(TValueDisplayFormatBool)],         // vdfgBool
    [low(TValueDisplayFormatChar)         .. high(TValueDisplayFormatChar)],         // vdfgChar
    [low(TValueDisplayFormatFloat)        .. high(TValueDisplayFormatFloat)],        // vdfgFloat
    [low(TValueDisplayFormatStruct)       .. high(TValueDisplayFormatStruct)],       // vdfgStruct
    [low(TValueDisplayFormatStructAddr)   .. high(TValueDisplayFormatStructAddr)],   // vdfgStructAddress
    [low(TValueDisplayFormatPointer)      .. high(TValueDisplayFormatPointer)],      // vdfgPointer
    [low(TValueDisplayFormatPointerDeref) .. high(TValueDisplayFormatPointerDeref)], // vdfgPointerDeref
    [low(TValueDisplayFormatCategory)     .. high(TValueDisplayFormatCategory)]      // vdfgCategory
  );

operator = (a,b: TWatchDisplayFormat): Boolean;

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

  IWatchResultDataIntf = interface;

  IWatchResultPrinter = interface
    function PrintWatchValue(AResValue: IWatchResultDataIntf; const ADispFormat: TWatchDisplayFormat): String;
  end;


  TWatchResultDataFieldInfoIntf = record
    FieldName: String;
    FieldVisibility: TLzDbgFieldVisibility;
    FieldFlags: TLzDbgFieldFlags;
    Field: IWatchResultDataIntf;
    Owner: IWatchResultDataIntf; // defined in class
  end;


  IWatchResultDataIntf = interface
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


implementation

operator = (a, b: TWatchDisplayFormat): Boolean;
begin
  Result :=
    (a.NumBaseFormat           = b.NumBaseFormat) and
    (a.NumSignFormat           = b.NumSignFormat) and
    (a.NumCharFormat           = b.NumCharFormat) and
    (a.EnumFormat              = b.EnumFormat) and
    (a.EnumBaseFormat          = b.EnumBaseFormat) and
    (a.EnumSignFormat          = b.EnumSignFormat) and
    (a.BoolFormat              = b.BoolFormat) and
    (a.BoolBaseFormat          = b.BoolBaseFormat) and
    (a.BoolSignFormat          = b.BoolSignFormat) and
    (a.CharFormat              = b.CharFormat) and
    (a.CharBaseFormat          = b.CharBaseFormat) and
    (a.CharSignFormat          = b.CharSignFormat) and
    (a.FloatFormat             = b.FloatFormat) and
    (a.StructFormat            = b.StructFormat) and
    (a.StructAddrFormat        = b.StructAddrFormat) and
    (a.StructPointerFormat     = b.StructPointerFormat) and
    (a.StructPointerBaseFormat = b.StructPointerBaseFormat) and
    (a.StructPointerSignFormat = b.StructPointerSignFormat) and
    (a.PointerFormat           = b.PointerFormat) and
    (a.PointerDerefFormat      = b.PointerDerefFormat) and
    (a.PointerBaseFormat       = b.PointerBaseFormat) and
    (a.PointerSignFormat       = b.PointerSignFormat) and
    (a.MemDump                 = b.MemDump);
end;

end.


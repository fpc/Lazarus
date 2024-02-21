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

  TWatchDisplayFormat =
    (wdfDefault,
     wdfStructure,
     wdfChar, wdfString,
     wdfDecimal, wdfUnsigned, wdfFloat, wdfHex,
     wdfPointer,
     wdfMemDump, wdfBinary
    );
  TWatchDisplayFormats = set of TWatchDisplayFormat;

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
    function PrintWatchValue(AResValue: IWatchResultDataIntf; ADispFormat: TWatchDisplayFormat): String;
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

end.


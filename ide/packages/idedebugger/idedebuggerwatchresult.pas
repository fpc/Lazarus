unit IdeDebuggerWatchResult;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, Types, IdeDebuggerUtils, LazDebuggerIntf,
  LazDebuggerIntfBaseTypes, LazDebuggerValueConverter, LazUTF8, Laz2_XMLCfg,
  LazLoggerBase;

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
  TWatchResultData = class;
  TWatchResultDataError = class;
  TWatchResultStorage = class;
  PWatchResultTypeStructFieldInfo = ^TWatchResultTypeStructFieldInfo;
  TOverrideTemplateData = TWatchResultDataError;

  { TWatchResultValue }

  TWatchResultValue = object
  protected
    function GetIsDephtlessData: Boolean; inline;
    function GetAsString: String; inline;
    function GetAsWideString: WideString; inline;
    function GetAsQWord: QWord; inline;
    function GetAsInt64: Int64; inline;
    function GetAsFloat: Extended; inline;
    function GetByteSize: Integer; inline;                         // Int, Enum
    function GetFloatPrecission: TLzDbgFloatPrecission; inline;
    function GetLowBound: Int64; inline;                           // static array
    function GetCount: Integer; inline;                            // Set (Active Elements)
    function GetLength: Integer; inline;                           // Array (Declared Len / Actual Len)
    function GetElementName(AnIndex: integer): String; inline;     // Set/Array
    function GetDerefData: TWatchResultData; inline;               // Ptr
    function GetBoundType: TWatchResultData; inline;               // Ptr

    function GetEntryTemplate: TWatchResultData; inline; // NESTED TYPE FOR NESTED STORAGE

    function GetDataAddress: TDBGPtr; inline;

    // struct
    function GetStructType:  TLzDbgStructType;  inline;
//    function GetStructFlags: TLzDbgStructFlags;  inline;
    function GetAnchestor: TWatchResultData;  inline;   // TWatchResTypeStruct ;
    function GetFieldCount: Integer; inline;
    function GetField(AnIndex: Integer): TWatchResultData;  inline;
    function GetFieldInfo(AnIndex: Integer): PWatchResultTypeStructFieldInfo;  inline;


    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure DoFree;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueTextBase }

  TWatchResultValueTextBase = object(TWatchResultValue)
  private
    FText: String;
  protected
    function GetIsDephtlessData: Boolean; inline; // TODO: if the debugger introduces different len -limits depending on depth....
    property GetAsString: String read FText;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValuePrePrinted }

  TWatchResultValuePrePrinted = object(TWatchResultValueTextBase)
  protected const
    VKind = rdkPrePrinted;
  end;

  { TWatchResultValueString }

  TWatchResultValueString = object(TWatchResultValueTextBase)
  protected const
    VKind = rdkString;
  private
    FAddress: TDBGPtr;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    property GetDataAddress: TDBGPtr read FAddress;
  end;

  { TWatchResultValueWideString }

  TWatchResultValueWideString = object(TWatchResultValue)
  protected const
    VKind = rdkWideString;
  private
    FWideText: WideString;
    FAddress: TDBGPtr;
  protected
    function GetIsDephtlessData: Boolean; inline; // TODO: if the debugger introduces different len -limits depending on depth....
    property GetAsWideString: WideString read FWideText;
    function GetAsString: String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    property GetDataAddress: TDBGPtr read FAddress;
  end;

  { TWatchResultValueOrdNumBase }

  TWatchResultValueOrdNumBase = object(TWatchResultValue)
  private
    FNumValue: QWord;
  protected
    function GetIsDephtlessData: Boolean; inline;
    property GetAsQWord: QWord read FNumValue;
    function GetAsInt64: Int64; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultTypeOrdNum }

  TWatchResultTypeOrdNum = object(TWatchResultValue)
  private
    FNumByteSize: Integer; // SmallInt
  protected
    property GetByteSize: Integer read FNumByteSize;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueSignedNum }

  TWatchResultValueSignedNum = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkSignedNumVal;
  protected
    function GetAsString: String; inline;
  end;

  { TWatchResultValueUnsignedNum }

  TWatchResultValueUnsignedNum = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkUnsignedNumVal;
  protected
    function GetAsString: String; inline;
  end;

  { TWatchResultValueChar }

  TWatchResultValueChar = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkChar;
  protected
    function GetAsString: String; inline;
  end;

  { TWatchResultValuePointer }

  TWatchResultValuePointer = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkPointerVal;
  protected
    //function GetIsDephtlessData: Boolean; inline; // Done in TWatchResultDataPointer
    function GetAsString: String; inline;
    property GetDataAddress: TDBGPtr read FNumValue;
  end;

  { TWatchResultTypePointer }

  TWatchResultTypePointer = object(TWatchResultValue)
  private
    FDerefData: TWatchResultData; // This may contain "Value"-Data. Will be stored in NestedStorage
  protected
    property GetDerefData: TWatchResultData read FDerefData;
    property GetEntryTemplate: TWatchResultData read FDerefData;
    function GetAsString: String; inline;
    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure DoFree;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueFloat }

  TWatchResultValueFloat = object(TWatchResultValue)
  protected const
    VKind = rdkFloatVal;
  private
    FFloatValue: Extended;
  protected
    function GetIsDephtlessData: Boolean; inline;
    property GetAsFloat: Extended read FFloatValue;
    function GetAsString: String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultTypeFloat }

  TWatchResultTypeFloat = object(TWatchResultValue)
  private
    FFloatPrecission: TLzDbgFloatPrecission;
  protected
    property FloatPrecission: TLzDbgFloatPrecission read FFloatPrecission;
    function GetAsString: String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueBoolean }

  TWatchResultValueBoolean = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkBool;
  protected
    function GetAsString: String; inline;
  end;

  { TWatchResultValueEnumBase }

  TWatchResultValueEnumBase = object(TWatchResultValueOrdNumBase)
  private
    FName: String;
  protected
    property GetAsString: String read FName;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueEnum }

  TWatchResultValueEnum = object(TWatchResultValueEnumBase)
  protected const
    VKind = rdkEnum;
  end;

  { TWatchResultValueEnumVal }

  TWatchResultValueEnumVal = object(TWatchResultValueEnumBase)
  protected const
    VKind = rdkEnumVal;
  end;

  { TWatchResultValueSet }

  TWatchResultValueSet = object(TWatchResultValue)
  protected const
    VKind = rdkSet;
  private
    FNames: Array of String;
  protected
    function GetIsDephtlessData: Boolean; inline;
    function GetCount: Integer; inline;
    function GetElementName(AnIndex: integer): String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueError }

  TWatchResultValueError = object(TWatchResultValueTextBase)
  protected const
    VKind = rdkError;
  end;

  TWatchResultValueVariant = object(TWatchResultValue)
  protected const
    VKind = rdkVariant;
  private
    FVariantData: TWatchResultData; // This may contain "Value"-Data. Will be stored in NestedStorage
    FVisibility: TLzDbgFieldVisibility;
    FName: String;
  protected
    property GetDerefData: TWatchResultData read FVariantData;
    property GetAsString: String read FName;
    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure DoFree;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueArrayBase }

  TWatchResultValueArrayBase = object(TWatchResultValue)
  private
    FEntries: TWatchResultStorage;
  protected
    function GetCount: Integer;
    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure DoFree;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultTypeArrayBase }

  TWatchResultTypeArrayBase = object(TWatchResultValue)
  private
    FEntryTemplate: TWatchResultData;
  protected
    property GetEntryTemplate: TWatchResultData read FEntryTemplate;
    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure DoFree;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValuePCharOrString }

  TWatchResultValuePCharOrString = object(TWatchResultValueArrayBase)
  protected const
    VKind = rdkPCharOrString;
  end;

  { TWatchResultValueArray }

  TWatchResultValueArray = object(TWatchResultValueArrayBase)
  protected const
    VKind = rdkArray;
  end;

  { TWatchResultValueUnknowArray }

  TWatchResultValueUnknowArray = object(TWatchResultValueArray)
  private
    FLowBound: Int64;
    FLength: Integer;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    property GetLowBound: Int64 read FLowBound;
    property GetLength: Integer read FLength;
  end;

  { TWatchResultValueDynArray }

  TWatchResultValueDynArray = object(TWatchResultValueArray)
  private
    FAddress: TDBGPtr;
    FLength: Integer;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    property GetDataAddress: TDBGPtr read FAddress;
    property GetLength: Integer read FLength;
  end;

  { TWatchResultValueStatArray }

  TWatchResultValueStatArray = object(TWatchResultValueArray)
  end;

  { TWatchResultTypeArray }

  TWatchResultTypeArray = object(TWatchResultTypeArrayBase)
  private
  protected
  end;

  { TWatchResultTypeStatArray }

  TWatchResultTypeStatArray = object(TWatchResultTypeArrayBase)
  private
    FBoundType: TWatchResultData;
    FLowBound: Int64;
    FLength: Integer;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    property GetLowBound: Int64 read FLowBound;
    property GetLength: Integer read FLength;
  end;


  TWatchResultTypeStructFieldInfo = packed record
    FieldName: String;
    FieldVisibility: TLzDbgFieldVisibility;
    FieldFlags: TLzDbgFieldFlags;
    Field: TWatchResultData;
  end;



//  TLzDbgStructFlag      = (dsfNil, dsfDummyAnchestor);
//  TLzDbgStructFlags     = set of TLzDbgStructFlag;

  { TWatchResultValueStruct }

  TWatchResultValueStruct = object(TWatchResultValue)
  protected const
    VKind = rdkStruct;
  end;

  { TWatchResultValueStructWithRef }

  TWatchResultValueStructWithRef = object(TWatchResultValueStruct)
  private
    FDataAddress: TDBGPtr;
  public
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    property GetDataAddress: TDBGPtr read FDataAddress;
  end;

  { TWatchResultValueConverted }

  TWatchResultValueConverted = object(TWatchResultValue)
  protected const
    VKind = rdkConvertRes;
  end;


  { TWatchResultTypeStructBase }

  TWatchResultTypeStructBase = object(TWatchResultValue)
  private
    FFieldData: packed array of TWatchResultTypeStructFieldInfo;
  public
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    function GetFieldCount: Integer; inline;
    function GetFieldInfo(AnIndex: Integer): PWatchResultTypeStructFieldInfo;  inline;
    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure CopyFieldsProtoFrom(const ASource: TWatchResultTypeStructBase); inline;
    procedure DoFree;
  end;

  { TWatchResultTypeStruct }

  TWatchResultTypeStruct = object(TWatchResultTypeStructBase)
  private
    FStructType: TLzDbgStructType;
//    FStructFlags: TLzDbgStructFlags; // dsfDummyAnchestor
    FAnchestor: TWatchResultData;
  public
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
    property GetStructType:  TLzDbgStructType read FStructType;
//    property GetStructFlags: TLzDbgStructFlags read FStructFlags;
    property GetAnchestor: TWatchResultData read FAnchestor;
    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure DoFree;
  end;

  { TWatchResultTypeConverted }

  TWatchResultTypeConverted = object(TWatchResultTypeStructBase)
  private
    FHandler: ILazDbgValueConverterIntf;
  public
    procedure AfterAssign(ATypeOnly: Boolean = False);
    procedure DoFree;
  end;


  { TWatchResultTypeProc }

  TWatchResultTypeProc = object(TWatchResultValue)
  private
    FText: String;
    FLoc: String;
  protected
    property GetAsString: String read FText;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  TWatchResultValueFunc = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkFunction;
  end;

  TWatchResultValueProc = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkProcedure;
  end;

  TWatchResultValueFuncRef = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkFunctionRef;
  end;

  TWatchResultValueProcRef = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkProcedureRef;
  end;



  { TWatchResultStorageOverrides }

  generic TWatchResultStorageOverrides<_OVERRIDE_DATA> = object
  private type
    TWatchResultStorageOverrideEntry = packed record
      FIndex: integer;
      FData: _OVERRIDE_DATA;
    end;
    TWatchResultStorageOverrideEntries = packed array of TWatchResultStorageOverrideEntry;
  private
    FOverrideEntries: TWatchResultStorageOverrideEntries;
    FOverrideCount: Integer;
  public
    procedure Assign(ASource: TWatchResultStorageOverrides);
    procedure Add(AnIndex: Integer; const AnOverrideData: _OVERRIDE_DATA); inline;
    function  Get(AnIndex: Integer; out AnOverrideData: _OVERRIDE_DATA): Boolean;
    procedure Clear; // doesnt yet call afterFree for nested data
    procedure AfterLastAdd;
  end;

  { TWatchResultStorageOverridesWithData }

  generic TWatchResultStorageOverridesWithData<_OVERRIDE_DATA, _ENTRY_DATA> = object(specialize TWatchResultStorageOverrides<_OVERRIDE_DATA>)
  private type
    TEntryDataArray = packed array of _ENTRY_DATA;
  private const
    DATA_OVERRIDE_MARK_B = $D2;
    DATA_OVERRIDE_MARK_W = $D24B;
    DATA_OVERRIDE_MARK_L = $D24B4BD2;
  public
    procedure Add(AnIndex: Integer; var AStoredData: _ENTRY_DATA; const AData: _OVERRIDE_DATA); reintroduce; // inline;
    function  Get(AnIndex: Integer; const AStoredData: _ENTRY_DATA; out AData: _OVERRIDE_DATA): Boolean; reintroduce; // inline;
    procedure Clear(var AStoredData: TEntryDataArray; AFirst: Integer = 0); inline;
    procedure Clear(AnIndex: Integer; var AStoredData: _ENTRY_DATA); // inline;
  end;


  { TWatchResultStorage }

  PWatchResultStorage = ^TWatchResultStorage;

  TWatchResultStorage = class
  public const
    TAG_CNT     = 'Cnt';
    TAG_ERR     = 'IsErr';
    TAG_ALL_ERR = 'AllErrLvl';
  protected
    function  GetCount: integer; virtual; abstract;
    procedure SetCount(AValue: integer); virtual; abstract;
    procedure SetNestedStorage(AValue: TWatchResultStorage); virtual;
    function  GetNestedStorage: TWatchResultStorage; virtual;
    function  GetNestedStoragePtr: PWatchResultStorage; virtual;
    procedure Assign(ASource: TWatchResultStorage); virtual; abstract;
    procedure ImportOverrides(ASource: TWatchResultStorage;
                              var AnOverrideTemplate: TOverrideTemplateData); virtual;
  public
    function CreateCopy: TWatchResultStorage; //virtual;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    ANestLvl: Integer=0); virtual; abstract;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer=0); virtual; abstract;

    procedure SaveToIndex(AnIndex: Integer;
                          AData: TWatchResultData;
                          var AnEntryTemplate: TWatchResultData;
                          var AnOverrideTemplate: TOverrideTemplateData
                         ); virtual; abstract;
    procedure LoadFromIndex(AnIndex: Integer;
                            out AData: TWatchResultData;
                            const AnEntryTemplate: TWatchResultData;
                            var AnOverrideTemplate: TOverrideTemplateData
                           ); virtual; abstract;
    property Count: integer read GetCount write SetCount;
    property NestedStorage: TWatchResultStorage read GetNestedStorage write SetNestedStorage;
    property NestedStoragePtr: PWatchResultStorage read GetNestedStoragePtr;
  end;
  TWatchResultStorageClass = class of TWatchResultStorage;

  TWatchResultDataClassID = (
    wdPrePrint,  // TWatchResultDataPrePrinted
    wdString,    // TWatchResultDataString
    wdWString,   // TWatchResultDataWideString
    wdChar,      // TWatchResultDataChar
    wdSNum,      // TWatchResultDataSignedNum
    wdUNum,      // TWatchResultDataUnSignedNum
    wdPtr,       // TWatchResultDataPointer
    wdFloat,     // TWatchResultDataFloat
    wdBool,      // TWatchResultDataBoolean
    wdEnum,      // TWatchResultDataEnum
    wdEnumVal,   // TWatchResultDataEnumVal
    wdSet,       // TWatchResultDataSet
    wdVar,       // TWatchResultDataVariant
    wdPChrStr,   // TWatchResultDataPCharOrString
    wdArray,     // TWatchResultDataArray
    wdDynA,      // TWatchResultDataDynArray
    wdStatA,     // TWatchResultDataStatArray
    wdStruct,    // TWatchResultDataStruct
    wdStructRef, // TWatchResultDataRefStruct
    wdConverted, // TWatchResultDataConverted
    wdFunc,      // TWatchResultDataFunc,
    wdProc,      // TWatchResultDataProc,
    wdFuncRef,   // TWatchResultDataFuncRef,
    wdProcRef,   // TWatchResultDataProcRef,
    wdErr        // TWatchResultDataError
  );

  //TWatchResultDataFlag = (wdfNoData);
  //TWatchResultDataFlags = set of TWatchResultDataFlag;

  TWatchResultDataFieldInfo = record
    FieldName: String;
    FieldVisibility: TLzDbgFieldVisibility;
    FieldFlags: TLzDbgFieldFlags;
    Field: TWatchResultData;
    Owner: TWatchResultData; // defined in class
  end;

  { TWatchResultDataEnumerator }

  TWatchResultDataEnumerator = class
  private
    FSource: TWatchResultData;
    FIndex: Integer;
    function GetCurrent: TWatchResultDataFieldInfo; virtual;
  public
    constructor Create(ARes: TWatchResultData);
    function MoveNext: Boolean; virtual;
    property Current: TWatchResultDataFieldInfo read GetCurrent;
  end;

  //TWatchResultDataEnumerator = class

  { TWatchResultData }

  TWatchResultData = class abstract // (TRefCountedObject)
  private
    FTypeName: String;
//    FDataFlags: TWatchResultDataFlags;
  //  Addr: TDbgPtr;
  // MemDump
    function GetBackendValueHandler: ILazDbgValueConverterIntf; virtual;
    function GetClassID: TWatchResultDataClassID; virtual; //abstract;
  protected
    class function GetStorageClass: TWatchResultStorageClass; virtual; abstract;
    function CreateStorage: TWatchResultStorage; virtual; abstract;
    function MaybeUpdateProto(var AProtoData: TWatchResultData;
                              var AnOverrideTemplate: TOverrideTemplateData;
                              AStorage: PWatchResultStorage;
                              ARecurse: boolean = False;
                              ASkipStorage: boolean = False
                             ): boolean; virtual; abstract;

    procedure AfterSaveToIndex(AStorage: TWatchResultStorage; AnIndex: Integer;
                               var AnEntryTemplate: TWatchResultData;
                               var AnOverrideTemplate: TOverrideTemplateData
                              ); virtual;
    procedure AfterLoadFromIndex(AStorage: TWatchResultStorage; AnIndex: Integer;
                                 const AnEntryTemplate: TWatchResultData;
                                 var AnOverrideTemplate: TOverrideTemplateData
                                ); virtual;
    procedure ClearData; virtual; abstract;
  protected
    function GetValueKind: TWatchResultDataKind; virtual; //abstract;
    function GetTypeName: String; virtual;
    function GetIsFullDephtEvaluated: Boolean; virtual;

    function GetAsString: String; virtual; abstract;
    function GetAsDesc: String; virtual; abstract;
    function GetAsWideString: WideString; virtual; abstract;
    function GetAsQWord: QWord; virtual; abstract;
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsFloat: Extended; virtual; abstract;

    function GetByteSize: Integer; virtual; abstract;
    function GetFloatPrecission: TLzDbgFloatPrecission; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetLength: Integer; virtual; abstract;
    function GetElementName(AnIndex: integer): String; virtual; abstract;
    function GetDerefData: TWatchResultData; virtual; abstract;
    function GetNestedType: TWatchResultData; virtual; abstract;

    function GetArrayType: TLzDbgArrayType; virtual; abstract;
    function GetBoundType: TWatchResultData; virtual; abstract;
    function GetLowBound: Int64; virtual; abstract;
    function GetSelectedEntry: TWatchResultData;  virtual; abstract;
    function GetDataAddress: TDBGPtr; virtual; abstract;
    function GetHasDataAddress: Boolean; virtual;

    function GetStructType: TLzDbgStructType; virtual; abstract;
    function GetAnchestor: TWatchResultData; virtual; abstract;
    function GetAnchestorCount: Integer; virtual; abstract;
    function GetAnchestors(AnIndex: Integer): TWatchResultData; virtual; abstract;
    function GetDirectFieldCount: Integer; virtual; abstract;
    function GetFieldCount: Integer; virtual; abstract;
    function GetFields(AnIndex: Integer): TWatchResultDataFieldInfo; virtual; abstract;
    function GetConvertedRes: TWatchResultData; virtual;

    function GetFieldVisibility: TLzDbgFieldVisibility; virtual; abstract;

  public
    constructor CreateEmpty;
    class function CreateFromXMLConfig(const AConfig: TXMLConfig; const APath: string): TWatchResultData; overload;
    class function CreateFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean = False): TWatchResultData; overload;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean = False); virtual;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean = False); virtual;
    procedure Assign(ASource: TWatchResultData; ATypeOnly: Boolean = False); virtual;
    function  CreateCopy(ATypeOnly: Boolean = False): TWatchResultData;

    (* HandleExpressionSuffix
       if a value other than "self" is returned, the caller is responsible to free it
    *)
    function HandleExpressionSuffix(ASuffix: String): TWatchResultData; virtual;

  function GetEnumerator: TWatchResultDataEnumerator; virtual;
  public
    property ValueKind: TWatchResultDataKind read GetValueKind;
    property TypeName: String read GetTypeName;
    property IsFullDephtEvaluated: Boolean read GetIsFullDephtEvaluated;

    property AsString: String read GetAsString;
    property AsDesc: String read GetAsDesc;
    property AsWideString: WideString read GetAsWideString;
    property AsQWord: QWord read GetAsQWord;
    property AsInt64: Int64 read GetAsInt64;
    property AsFloat: Extended read GetAsFloat;

    property ByteSize: Integer read GetByteSize;
    property FloatPrecission: TLzDbgFloatPrecission read GetFloatPrecission;
    property DerefData: TWatchResultData read GetDerefData;
    property NestedType: TWatchResultData read GetNestedType; // NESTED TYPE FOR NESTED STORAGE

    property ElementName[AnIndex: Integer]: String read GetElementName;
    property BackendValueHandler: ILazDbgValueConverterIntf read GetBackendValueHandler;

    // Array
    property ArrayType: TLzDbgArrayType read GetArrayType;
    property LowBound: Int64 read GetLowBound;
    property Count: Integer read GetCount;  // Count of Entries evaluated
    property ArrayLength: Integer read GetLength; // Declared Length
    property BoundType: TWatchResultData read GetBoundType;
    procedure SetSelectedIndex(AnIndex: Integer); virtual;
    property SelectedEntry: TWatchResultData read GetSelectedEntry;

    property DataAddress: TDBGPtr read GetDataAddress;
    property HasDataAddress: Boolean read GetHasDataAddress;

    // Struct
    property StructType:  TLzDbgStructType read GetStructType;
//    property StructFlags: TLzDbgStructFlags read FStructFlags;
    property Anchestor: TWatchResultData read GetAnchestor;
    property AnchestorCount: Integer read GetAnchestorCount;
    property Anchestors[AnIndex: Integer]: TWatchResultData read GetAnchestors;

    property FieldCount:       Integer read GetFieldCount;
    property DirectFieldCount: Integer read GetDirectFieldCount; // without inherited fields
    property Fields[AnIndex: Integer]: TWatchResultDataFieldInfo read GetFields;

    property ConvertedRes: TWatchResultData read GetConvertedRes;

    // variant
    property FieldVisibility: TLzDbgFieldVisibility read GetFieldVisibility;
  end;
  TWatchResultDataClass = class of TWatchResultData;

  { TWatchResultDataEx - Declare Setters for TCurrentResData }

  TWatchResultDataEx = class(TWatchResultData)
  public
    procedure SetTypeName(ATypeName: String);
    procedure SetDerefData(ADerefData: TWatchResultData); virtual;
    procedure SetDataAddress(AnAddr: TDbgPtr); virtual;

    procedure SetEntryPrototype(AnEntry: TWatchResultData); virtual;
    procedure WriteEntryToStorage(AnIndex: Integer); virtual;
    procedure WriteValueToStorage(AnIndex: Integer; AValue: TWatchResultData); virtual;
    procedure SetEntryCount(ACount: Integer); virtual;

    procedure SetAnchestor(AnAnchestor: TWatchResultData); virtual;
    procedure SetFieldCount(ACount: integer); virtual;
    procedure SetField(AnIndex: Integer;
                       AFieldName: String;
                       AVisibility: TLzDbgFieldVisibility;
                       AFlags: TLzDbgFieldFlags;
                       AData: TWatchResultData
                      ); virtual;
    function  AddField(AFieldName: String;
                       AVisibility: TLzDbgFieldVisibility;
                       AFlags: TLzDbgFieldFlags;
                       AData: TWatchResultData
                      ): Integer;virtual;
    procedure SetFieldData(AnIndex: Integer; AData: TWatchResultData); virtual;
  end;


  { TGenericWatchResultData }

  generic TGenericWatchResultData<_DATA> = class(TWatchResultDataEx)
  protected type
    { TGenericBasicWatchResultStorage }

    TGenericBasicWatchResultStorage = class(TWatchResultStorage)
    strict private type
      TDataArray = packed array of _DATA;
    protected
      FDataArray: TDataArray;
    protected
      function GetCount: integer; override;
      procedure SetCount(AValue: integer); override;
      procedure AssignDataArray(ASource: TWatchResultStorage); virtual;
      procedure Assign(ASource: TWatchResultStorage); override;
      procedure ImportOverrides(ASource: TWatchResultStorage;
                                var AnOverrideTemplate: TOverrideTemplateData); override;
    public
      destructor Destroy; override; // DoFree() for items
      procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                      const AnEntryTemplate: TWatchResultData;
                                      var AnOverrideTemplate: TOverrideTemplateData;
                                      ANestLvl: Integer=0); override;
      procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer=0); override;

      procedure SaveToIndex(AnIndex: Integer;
                            AData: TWatchResultData;
                            var AnEntryTemplate: TWatchResultData;
                            var AnOverrideTemplate: TOverrideTemplateData
                           ); override;
      procedure LoadFromIndex(AnIndex: Integer;
                              out AData: TWatchResultData;
                              const AnEntryTemplate: TWatchResultData;
                              var AnOverrideTemplate: TOverrideTemplateData
                             ); override;
    end;

    { TGenericWatchResultStorage }

    TGenericWatchResultStorage = class(TGenericBasicWatchResultStorage)
    private type
      _ERROR_DATA = TWatchResultValueError;
      _ERROR_CLASS = TWatchResultDataError;
      TErrorStorage = specialize TWatchResultStorageOverridesWithData<_ERROR_DATA, _DATA>;
    strict private
      FErrorStore: TErrorStorage;
    protected
      procedure SetCount(AValue: integer); override;
      procedure AssignDataArray(ASource: TWatchResultStorage); override;
      procedure Assign(ASource: TWatchResultStorage); override;
      procedure ImportOverrides(ASource: TWatchResultStorage;
                                var AnOverrideTemplate: TOverrideTemplateData); override;
    public
      destructor Destroy; override;
      procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                      const AnEntryTemplate: TWatchResultData;
                                      var AnOverrideTemplate: TOverrideTemplateData;
                                      ANestLvl: Integer=0); override;
      procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer=0); override;

      procedure SaveToIndex(AnIndex: Integer;
                            AData: TWatchResultData;
                            var AnEntryTemplate: TWatchResultData;
                            var AnOverrideTemplate: TOverrideTemplateData
                           ); override;
      procedure LoadFromIndex(AnIndex: Integer;
                              out AData: TWatchResultData;
                              const AnEntryTemplate: TWatchResultData;
                              var AnOverrideTemplate: TOverrideTemplateData
                             ); override;
    end;

    { TGenericNestedWatchResultStorage }

    TGenericNestedWatchResultStorage = class(TGenericWatchResultStorage)
    private
      FNestedStorage: TWatchResultStorage;
    protected
      procedure SetCount(AValue: integer); override;
      function  GetNestedStorage: TWatchResultStorage; override;
      procedure SetNestedStorage(AValue: TWatchResultStorage); override;
      function GetNestedStoragePtr: PWatchResultStorage; override;
      procedure Assign(ASource: TWatchResultStorage); override;
    public
      destructor Destroy; override;
      procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                      const AnEntryTemplate: TWatchResultData;
                                      var AnOverrideTemplate: TOverrideTemplateData;
                                      ANestLvl: Integer=0); override;
      procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer=0); override;
    end;

  { TGenericWatchResultData }
  private
    FData: _DATA;
  protected
    class function GetStorageClass: TWatchResultStorageClass; override;
    function CreateStorage: TWatchResultStorage; override;
    class procedure UpdateStorage(AStorage: PWatchResultStorage;
                                  const AData: TWatchResultData;
                                  var AnOverrideTemplate: TOverrideTemplateData);
    function MaybeUpdateProto(var AProtoData: TWatchResultData;
                              var AnOverrideTemplate: TOverrideTemplateData;
                              AStorage: PWatchResultStorage;
                              ARecurse: boolean = False;
                              ASkipStorage: boolean = False
                             ): boolean; override;
    procedure ClearData; override;
  protected
    function GetValueKind: TWatchResultDataKind; override;
    function GetIsFullDephtEvaluated: Boolean; override;

    function GetAsString: String; override;
    function GetAsDesc: String; override;
    function GetAsWideString: WideString; override;
    function GetAsQWord: QWord; override;
    function GetAsInt64: Int64; override;
    function GetAsFloat: Extended; override;

    function GetByteSize: Integer; override;
    function GetFloatPrecission: TLzDbgFloatPrecission; override;
    function GetCount: Integer; override;
    function GetLength: Integer; override;
    function GetElementName(AnIndex: integer): String; override;
    function GetDerefData: TWatchResultData; override;
    function GetNestedType: TWatchResultData; override;

    function GetArrayType: TLzDbgArrayType; override;
    function GetBoundType: TWatchResultData; override;
    function GetLowBound: Int64; override;
    function GetSelectedEntry: TWatchResultData; override;
    function GetDataAddress: TDBGPtr; override;

    function GetStructType: TLzDbgStructType; override;
    function GetAnchestor: TWatchResultData; override; // TODO move to With_Type
    function GetAnchestorCount: Integer; override;
    function GetAnchestors(AnIndex: Integer): TWatchResultData; override;
    function GetDirectFieldCount: Integer; override;
    function GetFieldCount: Integer; override;
    function GetFields(AnIndex: Integer): TWatchResultDataFieldInfo; override;

    function GetFieldVisibility: TLzDbgFieldVisibility; override;
  public
    destructor Destroy; override;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean = False); override;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean = False); override;
    procedure Assign(ASource: TWatchResultData; ATypeOnly: Boolean = False); override;
  end;

  { TGenericWatchResultDataWithType }

  generic TGenericWatchResultDataWithType<_DATA, _TYPE> = class(specialize TGenericWatchResultData<_DATA>)
  private
    FType: _TYPE;
  protected
    function GetByteSize: Integer; override;
    function GetFloatPrecission: TLzDbgFloatPrecission; override;

    function GetNestedType: TWatchResultData; override;
    function GetBoundType: TWatchResultData; override;
    function GetLowBound: Int64; override;
    function GetSelectedEntry: TWatchResultData; override;

    function GetStructType: TLzDbgStructType; override;
    function GetAnchestor: TWatchResultData; override;
    function GetDirectFieldCount: Integer; override;
    function GetFieldCount: Integer; override;
    function GetFields(AnIndex: Integer): TWatchResultDataFieldInfo; override;
  public
    destructor Destroy; override;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean = False); override;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean = False); override;
    procedure Assign(ASource: TWatchResultData; ATypeOnly: Boolean = False); override;
  end;

  { TWatchResultDataPrePrinted }

  TWatchResultDataPrePrinted = class(specialize TGenericWatchResultData<TWatchResultValuePrePrinted>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(APrintedVal: String);
  end;

  { TWatchResultDataString }

  TWatchResultDataString = class(specialize TGenericWatchResultData<TWatchResultValueString>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetHasDataAddress: Boolean; override;
    function GetDataAddress: TDBGPtr; override;
    procedure SetDataAddress(AnAddr: TDbgPtr); override;
  public
    constructor Create(AStringVal: String);
  end;

  { TWatchResultDataWideString }

  TWatchResultDataWideString = class(specialize TGenericWatchResultData<TWatchResultValueWideString>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetHasDataAddress: Boolean; override;
    function GetDataAddress: TDBGPtr; override;
    procedure SetDataAddress(AnAddr: TDbgPtr); override;
  public
    constructor Create(AStringVal: WideString);
  end;

  { TGenericWatchResultDataSizedNum }

  generic TGenericWatchResultDataSizedNum<_DATA> = class(specialize TGenericWatchResultDataWithType<_DATA, TWatchResultTypeOrdNum>)
  protected
    function GetAsQWord: QWord; override;
    function GetAsInt64: Int64; override;
  end;

  { TWatchResultDataChar }

  TWatchResultDataChar = class(specialize TGenericWatchResultDataSizedNum<TWatchResultValueChar>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(ANumValue: QWord; AByteSize: Integer = 0);
  end;

  { TWatchResultDataSignedNum }

  TWatchResultDataSignedNum = class(specialize TGenericWatchResultDataSizedNum<TWatchResultValueSignedNum>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(ANumValue: Int64; AByteSize: Integer = 0);
  end;

  { TWatchResultDataUnSignedNum }

  TWatchResultDataUnSignedNum = class(specialize TGenericWatchResultDataSizedNum<TWatchResultValueUnsignedNum>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(ANumValue: QWord; AByteSize: Integer = 0);
  end;

  { TWatchResultDataPointer }

  TWatchResultDataPointer = class(specialize TGenericWatchResultDataWithType<TWatchResultValuePointer, TWatchResultTypePointer>)
  private
    FCurrentDerefData: TWatchResultData; // needed if this is an array element
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetIsFullDephtEvaluated: Boolean; override;
    function GetHasDataAddress: Boolean; override;
    function GetAsString: String; override;
    function GetDerefData: TWatchResultData; override;
    class function GetStorageClass: TWatchResultStorageClass; override;
    function CreateStorage: TWatchResultStorage; override;
    function MaybeUpdateProto(var AProtoData: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData;
      AStorage: PWatchResultStorage; ARecurse: boolean = False;
      ASkipStorage: boolean = False): boolean;
      override;
    procedure AfterSaveToIndex(AStorage: TWatchResultStorage; AnIndex: Integer;
                               var AnEntryTemplate: TWatchResultData;
                               var AnOverrideTemplate: TOverrideTemplateData
                              ); override;
    procedure AfterLoadFromIndex(AStorage: TWatchResultStorage; AnIndex: Integer;
                                 const AnEntryTemplate: TWatchResultData;
                                 var AnOverrideTemplate: TOverrideTemplateData
                                ); override;
    procedure ClearData; override;
  public
    procedure SetDerefData(ADerefData: TWatchResultData); override;
  public
    constructor Create(AnAddr: TDBGPtr);
  end;

  { TWatchResultDataFloat }

  TWatchResultDataFloat = class(specialize TGenericWatchResultDataWithType<TWatchResultValueFloat, TWatchResultTypeFloat>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(AFloatValue: Extended; APrecission: TLzDbgFloatPrecission);
  end;

  { TWatchResultDataBoolean }

  TWatchResultDataBoolean = class(specialize TGenericWatchResultDataSizedNum<TWatchResultValueBoolean>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(AnOrdBoolValue: QWord; AByteSize: Integer = 0);
    constructor Create(ABoolValue: Boolean);
  end;

  { TWatchResultDataEnum }

  TWatchResultDataEnum = class(specialize TGenericWatchResultDataSizedNum<TWatchResultValueEnum>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(ANumValue: QWord; AName: String; AByteSize: Integer = 0);
  end;

  { TWatchResultDataEnumVal }

  TWatchResultDataEnumVal = class(specialize TGenericWatchResultDataSizedNum<TWatchResultValueEnumVal>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(ANumValue: QWord; AName: String; AByteSize: Integer = 0);
  end;

  { TWatchResultDataSet }

  TWatchResultDataSet = class(specialize TGenericWatchResultData<TWatchResultValueSet>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(const ANames: TStringDynArray);
  end;

  { TWatchResultDataVariant }

  TWatchResultDataVariant = class(specialize TGenericWatchResultData<TWatchResultValueVariant>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetFieldVisibility: TLzDbgFieldVisibility; override;
  public
    constructor Create(AName: String; AVisibility: TLzDbgFieldVisibility);
    procedure SetDerefData(ADerefData: TWatchResultData); override;
  end;

  { TWatchResultDataArrayBase }

  generic TWatchResultDataArrayBase<_DATA, _TYPE> = class(specialize TGenericWatchResultDataWithType<_DATA, _TYPE>)
  private
    FCurrentElement: TWatchResultData;
    FOverrideTemplateData: TOverrideTemplateData;
  protected
    function GetIsFullDephtEvaluated: Boolean; override;
  public
    procedure SetEntryPrototype(AnEntry: TWatchResultData); override;
    procedure WriteValueToStorage(AnIndex: Integer; AValue: TWatchResultData); override;
    procedure SetEntryCount(ACount: Integer); override;

    function MaybeUpdateProto(var AProtoData: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData;
      AStorage: PWatchResultStorage; ARecurse: boolean = False;
      ASkipStorage: boolean = False): boolean;
      override;
    procedure AfterSaveToIndex(AStorage: TWatchResultStorage; AnIndex: Integer;
      var AnEntryTemplate: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData); override;
    procedure AfterLoadFromIndex(AStorage: TWatchResultStorage;
      AnIndex: Integer; const AnEntryTemplate: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData); override;
    procedure ClearData; override;
  public
    destructor Destroy; override;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean = False); override;
    procedure Assign(ASource: TWatchResultData; ATypeOnly: Boolean = False); override;
    procedure SetSelectedIndex(AnIndex: Integer); override; // ReadEntryFromStorage
    function  GetSelectedEntry: TWatchResultData; override;
  end;

  { TWatchResultDataPCharOrString }

  TWatchResultDataPCharOrString = class(specialize TWatchResultDataArrayBase<TWatchResultValuePCharOrString, TWatchResultTypeArrayBase>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetIsFullDephtEvaluated: Boolean; override;
  public
    procedure SetEntryPrototype(AnEntry: TWatchResultData); override;
  end;

  { TWatchResultDataArray }

  TWatchResultDataArray = class(specialize TWatchResultDataArrayBase<TWatchResultValueUnknowArray, TWatchResultTypeArray>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetArrayType: TLzDbgArrayType; override;
    function GetLowBound: Int64; override;
  // ReadDataFromStorage should also procedure SetSelectedIndex(0) ???
  public
    constructor Create(ALength: Integer; ALowBound: Int64);
  end;

  { TWatchResultDataDynArray }

  TWatchResultDataDynArray = class(specialize TWatchResultDataArrayBase<TWatchResultValueDynArray, TWatchResultTypeArray>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetArrayType: TLzDbgArrayType; override;
    function GetHasDataAddress: Boolean; override;
  public
    constructor Create(ALength: Integer);
    procedure SetDataAddress(AnAddr: TDbgPtr); override;
  end;

  { TWatchResultDataStatArray }

  TWatchResultDataStatArray = class(specialize TWatchResultDataArrayBase<TWatchResultValueStatArray, TWatchResultTypeStatArray>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetArrayType: TLzDbgArrayType; override;
    function GetDataAddress: TDBGPtr; override;
    function GetLength: Integer; override;
  public
    constructor Create(ALength: Integer; ALowBound: Int64);
  end;



  { TGenericWatchResultDataStruct }

  generic TGenericWatchResultDataStruct<_DATA, _TYPE> = class(specialize TGenericWatchResultDataWithType<_DATA, _TYPE>)
  private type
    { TWatchResultDataStructVariantEnumerator }

    TWatchResultDataStructVariantEnumerator = class(TWatchResultDataEnumerator)
    protected
      function GetCurrent: TWatchResultDataFieldInfo; override;
    public
      constructor Create(ARes: TWatchResultData);
      function MoveNext: Boolean; override;
    end;


    { TWatchResultDataStructEnumerator }

    TWatchResultDataStructEnumerator = class(TWatchResultDataEnumerator)
    private
      FSubEnumerator: TWatchResultDataStructVariantEnumerator;
      FSubOwner: TWatchResultData;
    protected
      function GetCurrent: TWatchResultDataFieldInfo; override;
    public
      constructor Create(ARes: TWatchResultData);
      function MoveNext: Boolean; override;
    end;

    { TNestedFieldsWatchResultStorage }

    TNestedFieldsWatchResultStorage = class(TGenericWatchResultStorage)
    private
      FFieldsStorage: array of TWatchResultStorage;
      FOverrideTempl: array of TOverrideTemplateData;
      function GetStoredFieldCount: Integer;
      procedure SetStoredFieldCount(AValue: Integer);
    protected
      procedure SetCount(AValue: integer); override;
      procedure Assign(ASource: TWatchResultStorage); override;

      function GetNestedStorage(AnIndex: Integer): TWatchResultStorage; reintroduce;
      function GetNestedStoragePtr(AnIndex: Integer): PWatchResultStorage; reintroduce;
      procedure SetNestedStorage(AnIndex: Integer; AValue: TWatchResultStorage); reintroduce;
    public
      destructor Destroy; override;
      procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                      const AnEntryTemplate: TWatchResultData;
                                      var AnOverrideTemplate: TOverrideTemplateData;
                                      ANestLvl: Integer=0); override;
      procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer=0); override;

      property NestedStorage[AnIndex: Integer]: TWatchResultStorage read GetNestedStorage write SetNestedStorage;
      property NestedStoragePtr[AnIndex: Integer]: PWatchResultStorage read GetNestedStoragePtr;
      property StoredFieldCount: Integer read GetStoredFieldCount write SetStoredFieldCount;
    end;
    PNestedFieldsWatchResultStorage = ^TNestedFieldsWatchResultStorage;

  private
    FCurrentFields: array of TWatchResultData;
  protected
    class function GetStorageClass: TWatchResultStorageClass; override;
    function CreateStorage: TWatchResultStorage; override;
    function MaybeUpdateProto(var AProtoData: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData;
      AStorage: PWatchResultStorage; ARecurse: boolean = False;
      ASkipStorage: boolean = False): boolean;
      override;
    procedure AfterSaveToIndex(AStorage: TWatchResultStorage; AnIndex: Integer;
      var AnEntryTemplate: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData); override;
    procedure AfterLoadFromIndex(AStorage: TWatchResultStorage;
      AnIndex: Integer; const AnEntryTemplate: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData); override;
    procedure ClearData; override;
    function GetFields(AnIndex: Integer): TWatchResultDataFieldInfo; override;
  public
    function GetEnumerator: TWatchResultDataEnumerator; override;

    procedure SetFieldCount(ACount: integer); override;
    procedure SetField(AnIndex: Integer;
                       AFieldName: String;
                       AVisibility: TLzDbgFieldVisibility;
                       AFlags: TLzDbgFieldFlags;
                       AData: TWatchResultData
                      ); override;
    function  AddField(AFieldName: String;
                       AVisibility: TLzDbgFieldVisibility;
                       AFlags: TLzDbgFieldFlags;
                       AData: TWatchResultData
                      ): Integer; override;
    procedure SetFieldData(AnIndex: Integer; AData: TWatchResultData); override;
  end;

  { TGenericWatchResultDataStructWithAnchestor }

  generic TGenericWatchResultDataStructWithAnchestor<_DATA, _TYPE> = class(specialize TGenericWatchResultDataStruct<_DATA, _TYPE>)
  private type

    { TNestedFieldsAndAnchestorWatchResultStorage }

    TNestedFieldsAndAnchestorWatchResultStorage = class(TNestedFieldsWatchResultStorage)
    private
      FAnchestorStorage: TWatchResultStorage;
    protected
      procedure SetCount(AValue: integer); override;
      procedure Assign(ASource: TWatchResultStorage); override;

      function GetNestedStorage(AnIndex: Integer): TWatchResultStorage; reintroduce;
      function GetNestedStoragePtr(AnIndex: Integer): PWatchResultStorage; reintroduce;
      procedure SetNestedStorage(AnIndex: Integer; AValue: TWatchResultStorage); reintroduce;
    public
      destructor Destroy; override;
      procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                      const AnEntryTemplate: TWatchResultData;
                                      var AnOverrideTemplate: TOverrideTemplateData;
                                      ANestLvl: Integer=0); override;
      procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer=0); override;

      property NestedStorage[AnIndex: Integer]: TWatchResultStorage read GetNestedStorage write SetNestedStorage;
      property NestedStoragePtr[AnIndex: Integer]: PWatchResultStorage read GetNestedStoragePtr;
      property StoredFieldCount: Integer read GetStoredFieldCount write SetStoredFieldCount;
    end;
    PNestedFieldsAndAnchestorWatchResultStorage = ^TNestedFieldsAndAnchestorWatchResultStorage;

  private
    FCurrentAnchestor: TWatchResultData;
  protected
    class function GetStorageClass: TWatchResultStorageClass; override;
    function CreateStorage: TWatchResultStorage; override;
    function MaybeUpdateProto(var AProtoData: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData;
      AStorage: PWatchResultStorage; ARecurse: boolean = False;
      ASkipStorage: boolean = False): boolean;
      override;
    procedure AfterSaveToIndex(AStorage: TWatchResultStorage; AnIndex: Integer;
      var AnEntryTemplate: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData); override;
    procedure AfterLoadFromIndex(AStorage: TWatchResultStorage;
      AnIndex: Integer; const AnEntryTemplate: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData); override;
    procedure ClearData; override;
    function GetAnchestor: TWatchResultData; override;
  public
    procedure SetAnchestor(AnAnchestor: TWatchResultData); override;
  end;

  { TWatchResultDataStruct }

  TWatchResultDataStruct = class(specialize TGenericWatchResultDataStructWithAnchestor<TWatchResultValueStruct, TWatchResultTypeStruct>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(AStructType: TLzDbgStructType);
  end;

  { TWatchResultDataRefStruct }

  TWatchResultDataRefStruct = class(specialize TGenericWatchResultDataStructWithAnchestor<TWatchResultValueStructWithRef, TWatchResultTypeStruct>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetHasDataAddress: Boolean; override;
  public
    constructor Create(AStructType: TLzDbgStructType;
                       ADataAddress: TDBGPtr
                      );
  end;

  { TWatchResultDataConverted }

  TWatchResultDataConverted = class(specialize TGenericWatchResultDataStruct<TWatchResultValueConverted, TWatchResultTypeConverted>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    function GetBackendValueHandler: ILazDbgValueConverterIntf; override;
    function GetConvertedRes: TWatchResultData; override;
  public
    constructor Create(AHandler: ILazDbgValueConverterIntf);
  end;

  { TGenericWatchResultDataProc }

  generic TGenericWatchResultDataProc<_DATA> = class(specialize TGenericWatchResultDataWithType<_DATA, TWatchResultTypeProc>)
  protected
    function GetAsString: String; override; // TODO
    function GetAsDesc: String; override;
  public
    constructor Create(AnAddr: QWord; ALoc, ADesc: String);
  end;

  { TWatchResultDataFunc }

  TWatchResultDataFunc = class(specialize TGenericWatchResultDataProc<TWatchResultValueFunc>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  end;

  { TWatchResultDataProc }

  TWatchResultDataProc = class(specialize TGenericWatchResultDataProc<TWatchResultValueProc>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  end;

  { TWatchResultDataFuncRef }

  TWatchResultDataFuncRef = class(specialize TGenericWatchResultDataProc<TWatchResultValueFuncRef>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  end;

  { TWatchResultDataProcRef }

  TWatchResultDataProcRef = class(specialize TGenericWatchResultDataProc<TWatchResultValueProcRef>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  end;




  { TWatchResultDataError }

  TWatchResultDataError = class(specialize TGenericWatchResultData<TWatchResultValueError>)
  private type

    { TErrorDataStorage }

    TErrorDataStorage = class(TGenericBasicWatchResultStorage)
    public
      procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
        const APath: string; ANestLvl: Integer = 0); override;
      procedure LoadFromIndex(AnIndex: Integer; out AData: TWatchResultData;
        const AnEntryTemplate: TWatchResultData;
        var   AnOverrideTemplate: TOverrideTemplateData); override;
    end;
  private
    function GetClassID: TWatchResultDataClassID; override;
  protected
    class function GetStorageClass: TWatchResultStorageClass; override;
  public
    constructor Create(APrintedVal: String);
  end;

function dbgs(AResKind: TWatchResultDataKind): String; overload;

implementation

function dbgs(AResKind: TWatchResultDataKind): String;
begin
  WriteStr(Result, AResKind);
end;

const
  WatchResNameToClass: array [TWatchResultDataClassID] of TWatchResultDataClass = (
    TWatchResultDataPrePrinted,    // wdPrePrint
    TWatchResultDataString,        // wdString
    TWatchResultDataWideString,    // wdWString
    TWatchResultDataChar,          // wdChar
    TWatchResultDataSignedNum,     // wdSNum
    TWatchResultDataUnSignedNum,   // wdUNum
    TWatchResultDataPointer,       // wdPtr
    TWatchResultDataFloat,         // wdFloat
    TWatchResultDataBoolean,       // wdBool
    TWatchResultDataEnum,          // wdEnum
    TWatchResultDataEnumVal,       // wdEnumVal
    TWatchResultDataSet,           // wdSet
    TWatchResultDataVariant,       // wdVar
    TWatchResultDataPCharOrString, // wdPChrStr
    TWatchResultDataArray,         // wdArray,
    TWatchResultDataDynArray,      // wdDynA,
    TWatchResultDataStatArray,     // wdStatA,
    TWatchResultDataStruct,        // wdStruct
    TWatchResultDataRefStruct,     // wdStructRef
    TWatchResultDataConverted,     // wdConverted
    TWatchResultDataFunc,          // wdFunc
    TWatchResultDataProc,          // wdProc
    TWatchResultDataFuncRef,       // wdFuncRef
    TWatchResultDataProcRef,       // wdProcRef
    TWatchResultDataError          // wdErr
  );

{ TWatchResultValue }

function TWatchResultValue.GetIsDephtlessData: Boolean;
begin
  Result := False;
end;

function TWatchResultValue.GetAsString: String;
begin
  Result := '';
end;

function TWatchResultValue.GetAsWideString: WideString;
begin
  Result := '';
end;

function TWatchResultValue.GetAsQWord: QWord;
begin
  Result := 0;
end;

function TWatchResultValue.GetAsInt64: Int64;
begin
  Result := 0;
end;

function TWatchResultValue.GetAsFloat: Extended;
begin
  Result := 0;
end;

function TWatchResultValue.GetByteSize: Integer;
begin
  Result := 0;
end;

function TWatchResultValue.GetFloatPrecission: TLzDbgFloatPrecission;
begin
  Result := dfpSingle;
end;

function TWatchResultValue.GetLowBound: Int64;
begin
  Result := 0;
end;

function TWatchResultValue.GetCount: Integer;
begin
  Result := 0;
end;

function TWatchResultValue.GetLength: Integer;
begin
  Result := 0;
end;

function TWatchResultValue.GetElementName(AnIndex: integer): String;
begin
  Result := '';
end;

function TWatchResultValue.GetDerefData: TWatchResultData;
begin
  Result := nil;
end;

function TWatchResultValue.GetBoundType: TWatchResultData;
begin
  Result := nil;
end;

function TWatchResultValue.GetEntryTemplate: TWatchResultData;
begin
  Result := nil;
end;

function TWatchResultValue.GetDataAddress: TDBGPtr;
begin
  Result := 0;
end;

function TWatchResultValue.GetStructType: TLzDbgStructType;
begin
  Result := dstUnknown;
end;

function TWatchResultValue.GetAnchestor: TWatchResultData;
begin
  Result := nil;
end;

function TWatchResultValue.GetFieldCount: Integer;
begin
  Result := 0;
end;

function TWatchResultValue.GetField(AnIndex: Integer): TWatchResultData;
begin
  Result := Nil;
end;

function TWatchResultValue.GetFieldInfo(AnIndex: Integer
  ): PWatchResultTypeStructFieldInfo;
begin
  Result := Nil;
end;

procedure TWatchResultValue.AfterAssign(ATypeOnly: Boolean);
begin
  //
end;

procedure TWatchResultValue.DoFree;
begin
  //
end;

procedure TWatchResultValue.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  //
end;

procedure TWatchResultValue.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  //
end;

{ TWatchResultValueTextBase }

function TWatchResultValueTextBase.GetIsDephtlessData: Boolean;
begin
  Result := True;
end;

procedure TWatchResultValueTextBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FText := AConfig.GetValue(APath + 'Value', '');
end;

procedure TWatchResultValueTextBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetValue(APath + 'Value', FText);
end;

{ TWatchResultValueString }

procedure TWatchResultValueString.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FAddress := TDBGPtr(AConfig.GetValue(APath + 'Addr', 0));
end;

procedure TWatchResultValueString.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetDeleteValue(APath + 'Addr', Int64(FAddress), 0);
end;

{ TWatchResultValueWideString }

function TWatchResultValueWideString.GetIsDephtlessData: Boolean;
begin
  Result := True;
end;

function TWatchResultValueWideString.GetAsString: String;
begin
  Result := FWideText;
end;

procedure TWatchResultValueWideString.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FWideText := AConfig.GetValue(APath + 'Value', '');
  FAddress := TDBGPtr(AConfig.GetValue(APath + 'Addr', 0));
end;

procedure TWatchResultValueWideString.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetValue(APath + 'Value', FWideText);
  AConfig.SetDeleteValue(APath + 'Addr', Int64(FAddress), 0);
end;

{ TWatchResultValueOrdNumBase }

function TWatchResultValueOrdNumBase.GetIsDephtlessData: Boolean;
begin
  Result := True;
end;

function TWatchResultValueOrdNumBase.GetAsInt64: Int64;
begin
  Result := Int64(FNumValue);
end;

procedure TWatchResultValueOrdNumBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FNumValue := QWord(AConfig.GetValue(APath + 'Value', int64(0)));
end;

procedure TWatchResultValueOrdNumBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetValue(APath + 'Value', Int64(FNumValue));
end;

{ TWatchResultTypeOrdNum }

procedure TWatchResultTypeOrdNum.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  FNumByteSize := AConfig.GetValue(APath + 'BSize', 0);
end;

procedure TWatchResultTypeOrdNum.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetDeleteValue(APath + 'BSize', FNumByteSize, 0);
end;

{ TWatchResultValueSignedNum }

function TWatchResultValueSignedNum.GetAsString: String;
begin
  Result := IntToStr(Int64(FNumValue));
end;

{ TWatchResultValueUnsignedNum }

function TWatchResultValueUnsignedNum.GetAsString: String;
begin
  Result := IntToStr(QWord(FNumValue));
end;

{ TWatchResultValueChar }

function TWatchResultValueChar.GetAsString: String;
begin
  Result := WideChar(FNumValue);
end;

{ TWatchResultValuePointer }

function TWatchResultValuePointer.GetAsString: String;
begin
  Result := '$'+IntToHex(QWord(FNumValue), HexDigicCount(FNumValue, 0, True));
end;

{ TWatchResultTypePointer }

function TWatchResultTypePointer.GetAsString: String;
begin
  if FDerefData = nil then
    Result := ''
  else
    Result := '^: ' + FDerefData.AsString;
end;

procedure TWatchResultTypePointer.AfterAssign(ATypeOnly: Boolean);
begin
  FDerefData := FDerefData.CreateCopy(ATypeOnly);
end;

procedure TWatchResultTypePointer.DoFree;
begin
  FDerefData.Free;
end;

procedure TWatchResultTypePointer.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
var
  deref: TWatchResultData;
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  if AConfig.HasPath(APath + 'Deref', False) then begin
    deref := nil;
    if AnEntryTemplate <> nil then begin
      assert(AnEntryTemplate is TWatchResultDataPointer, 'TWatchResultTypePointer.LoadDataFromXMLConfig: AnEntryTemplate is TWatchResultDataPointer');
      deref := TWatchResultDataPointer(AnEntryTemplate).FType.FDerefData;
    end;
    FDerefData := TWatchResultData.CreateFromXMLConfig(AConfig,
      APath + 'Deref/',
      deref,
      AnOverrideTemplate,
      AnAsProto);
    end;
end;

procedure TWatchResultTypePointer.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  if FDerefData <> nil then
    FDerefData.SaveDataToXMLConfig(AConfig, APath + 'Deref/', AnAsProto)
  else
    AConfig.DeletePath(APath + 'Deref');
end;

{ TWatchResultValueFloat }

function TWatchResultValueFloat.GetIsDephtlessData: Boolean;
begin
  Result := True;
end;

function TWatchResultValueFloat.GetAsString: String;
begin
  Result := FloatToStr(FFloatValue);
end;

procedure TWatchResultValueFloat.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FFloatValue := AConfig.GetExtendedValue(APath + 'Value', 0);
end;

procedure TWatchResultValueFloat.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetExtendedValue(APath + 'Value', FFloatValue);
end;

{ TWatchResultTypeFloat }

function TWatchResultTypeFloat.GetAsString: String;
begin
  WriteStr(Result, FFloatPrecission);
end;

procedure TWatchResultTypeFloat.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  AConfig.GetValue(APath + 'Prec', int64(ord(dfpSingle)), FFloatPrecission, TypeInfo(TLzDbgFloatPrecission));
end;

procedure TWatchResultTypeFloat.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetDeleteValue(APath + 'Prec', FFloatPrecission, ord(dfpSingle), TypeInfo(TLzDbgFloatPrecission));
end;

{ TWatchResultValueBoolean }

function TWatchResultValueBoolean.GetAsString: String;
begin
  if FNumValue <> 0 then
    Result := 'True'
  else
    Result := 'False';
end;

{ TWatchResultValueEnumBase }

procedure TWatchResultValueEnumBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FName := AConfig.GetValue(APath + 'Value', '');
end;

procedure TWatchResultValueEnumBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetDeleteValue(APath + 'Value', FName, '');
end;

{ TWatchResultValueSet }

function TWatchResultValueSet.GetIsDephtlessData: Boolean;
begin
  Result := True;
end;

function TWatchResultValueSet.GetCount: Integer;
begin
  Result := Length(FNames);
end;

function TWatchResultValueSet.GetElementName(AnIndex: integer): String;
begin
  Result := FNames[AnIndex];
end;

procedure TWatchResultValueSet.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FNames := AConfig.GetValue(APath + 'Value', '').Split([',']);
end;

procedure TWatchResultValueSet.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetDeleteValue(APath + 'Value', ''.Join(',', FNames), '');
end;

{ TWatchResultValueVariant }

procedure TWatchResultValueVariant.AfterAssign(ATypeOnly: Boolean);
begin
  FVariantData := FVariantData.CreateCopy();
end;

procedure TWatchResultValueVariant.DoFree;
begin
  FVariantData.Free;
end;

procedure TWatchResultValueVariant.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
var
  p: String;
  d: TOverrideTemplateData;
begin
  if AnAsProto then
    exit;
  p := APath + 'var/';
  d := nil;
  if AConfig.HasPath(p, True) then
    FVariantData := TWatchResultData.CreateFromXMLConfig(AConfig, p, nil,
      d);
  assert(d=nil, 'TWatchResultValueVariant.LoadDataFromXMLConfig: d=nil');
end;

procedure TWatchResultValueVariant.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  if FVariantData <> nil then
    FVariantData.SaveDataToXMLConfig(AConfig, APath + 'var/', AnAsProto);
end;

{ TWatchResultValueArrayBase }

function TWatchResultValueArrayBase.GetCount: Integer;
begin
  if FEntries = nil then
    exit(0);
  Result := FEntries.Count;
end;

procedure TWatchResultValueArrayBase.AfterAssign(ATypeOnly: Boolean);
begin
  if FEntries <> nil then
    FEntries := FEntries.CreateCopy;
end;

procedure TWatchResultValueArrayBase.DoFree;
begin
  FEntries.Free;
end;

procedure TWatchResultValueArrayBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
var
  p: String;
begin
  if (AnEntryTemplate = nil) or (AnEntryTemplate.NestedType = nil) then
    exit;

  p := APath+'Entries/';
  if FEntries = nil then begin
    if (AConfig.GetValue(p+TWatchResultStorage.TAG_CNT, -1) > 0) then begin
      if (AConfig.GetValue(p+TWatchResultStorage.TAG_ALL_ERR, -1) = 0) and
         (AnEntryTemplate.NestedType.ValueKind <> rdkError)
      then
        FEntries := TWatchResultDataError.GetStorageClass.Create
      else
        FEntries := AnEntryTemplate.NestedType.CreateStorage; // (ErrStorageAtLevel)
    end;
  end;

  if FEntries <> nil then begin
    FEntries.LoadDataFromXMLConfig(AConfig,
      p,
      AnEntryTemplate.NestedType,
      AnOverrideTemplate);
  end;
end;

procedure TWatchResultValueArrayBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  if FEntries <> nil then
    FEntries.SaveDataToXMLConfig(AConfig, APath+'Entries/'); //, AnAsProto);
end;

{ TWatchResultTypeArrayBase }

procedure TWatchResultTypeArrayBase.AfterAssign(ATypeOnly: Boolean);
begin
  FEntryTemplate := FEntryTemplate.CreateCopy(ATypeOnly);
end;

procedure TWatchResultTypeArrayBase.DoFree;
begin
  FreeAndNil(FEntryTemplate);
end;

procedure TWatchResultTypeArrayBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  if AConfig.HasPath(APath+'Proto/', True) then
    FEntryTemplate := TWatchResultData.CreateFromXMLConfig(AConfig, APath+'Proto/', AnEntryTemplate, AnOverrideTemplate, True);
end;

procedure TWatchResultTypeArrayBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  if FEntryTemplate <> nil then
    FEntryTemplate.SaveDataToXMLConfig(AConfig, APath+'Proto/', True);
end;

{ TWatchResultValueUnknowArray }

procedure TWatchResultValueUnknowArray.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FLength := AConfig.GetValue(APath + 'Len', 0);
  FLowBound := AConfig.GetValue(APath + 'Low', 0);
end;

procedure TWatchResultValueUnknowArray.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetDeleteValue(APath + 'Len', FLength, 0);
  AConfig.SetDeleteValue(APath + 'Low', FLowBound, 0);
end;

{ TWatchResultValueDynArray }

procedure TWatchResultValueDynArray.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FLength := AConfig.GetValue(APath + 'Len', 0);
  FAddress := TDBGPtr(AConfig.GetValue(APath + 'Addr', 0));
end;

procedure TWatchResultValueDynArray.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetDeleteValue(APath + 'Len', FLength, 0);
  AConfig.SetDeleteValue(APath + 'Addr', Int64(FAddress), 0);
end;

{ TWatchResultTypeStatArray }

procedure TWatchResultTypeStatArray.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  if AConfig.HasPath(APath + 'Bound', False) then
    FBoundType := twatchresultdata.CreateFromXMLConfig(AConfig, APath+''+'Bound/', AnEntryTemplate, AnOverrideTemplate, AnAsProto);

  FLength := AConfig.GetValue(APath + 'Len', 0);
  FLowBound := AConfig.GetValue(APath + 'Low', 0);
end;

procedure TWatchResultTypeStatArray.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  if FBoundType <> nil then
    FBoundType.SaveDataToXMLConfig(AConfig, APath+'Bound/', AnAsProto);

  AConfig.SetDeleteValue(APath + 'Len', FLength, 0);
  AConfig.SetDeleteValue(APath + 'Low', FLowBound, 0);
end;

{ TWatchResultValueStructWithRef }

procedure TWatchResultValueStructWithRef.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  //inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);

  FDataAddress := TDBGPtr(AConfig.GetValue(APath + 'Addr', 0));
end;

procedure TWatchResultValueStructWithRef.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  //inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);

  AConfig.SetDeleteValue(APath + 'Addr', Int64(FDataAddress), 0);
end;

{ TWatchResultTypeStructBase }

procedure TWatchResultTypeStructBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
const
  Zero: array[0..3] of byte = (0,0,0,0);
var
  DataCnt: integer;
  i: Integer;
  p: String;
begin
  DataCnt := AConfig.GetValue(APath + 'Cnt', 0);
  SetLength(FFieldData, DataCnt);
  for i := 0 to DataCnt-1 do begin
    p := APath+'F'+IntToStr(i)+'/';
    FFieldData[i].FieldName := AConfig.GetValue(p + 'F-Name', '');
    AConfig.GetValue(p + 'F-Vis', int64(ord(dfvUnknown)), FFieldData[i].FieldVisibility, TypeInfo(TLzDbgFieldVisibility));
    AConfig.GetValue(p + 'F-Flg', Zero,                   FFieldData[i].FieldFlags, TypeInfo(TLzDbgFieldFlags));
    if AConfig.GetValue(p+'Empty', 0) = 0 then
      FFieldData[i].Field := TWatchResultData.CreateFromXMLConfig(AConfig, p);
  end;
end;

procedure TWatchResultTypeStructBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
var
  i: Integer;
  p: String;
begin
  AConfig.SetDeleteValue(APath + 'Cnt', Length(FFieldData), 0);
  for i := 0 to Length(FFieldData)-1 do begin
    p := APath+'F'+IntToStr(i)+'/';
    AConfig.SetDeleteValue(p + 'F-Name', FFieldData[i].FieldName, '');
    AConfig.SetDeleteValue(p + 'F-Vis', FFieldData[i].FieldVisibility, ord(dfvUnknown), TypeInfo(TLzDbgFieldVisibility));
    AConfig.SetDeleteValue(p + 'F-Flg', FFieldData[i].FieldFlags, 0, TypeInfo(TLzDbgFieldFlags));
    if FFieldData[i].Field <> nil then begin
      FFieldData[i].Field.SaveDataToXMLConfig(AConfig, p, AnAsProto);
      AConfig.DeleteValue(p+'Empty');
    end
    else
      AConfig.SetValue(p+'Empty',1);
  end;
end;

function TWatchResultTypeStructBase.GetFieldCount: Integer;
begin
  Result := Length(FFieldData);
end;

function TWatchResultTypeStructBase.GetFieldInfo(AnIndex: Integer
  ): PWatchResultTypeStructFieldInfo;
begin
  Result := @FFieldData[AnIndex];
end;

procedure TWatchResultTypeStructBase.AfterAssign(ATypeOnly: Boolean);
var
  i: Integer;
begin
  SetLength(FFieldData, Length(FFieldData));
  for i := 0 to Length(FFieldData) - 1 do
    FFieldData[i].Field := FFieldData[i].Field.CreateCopy(ATypeOnly);
end;

procedure TWatchResultTypeStructBase.CopyFieldsProtoFrom(
  const ASource: TWatchResultTypeStructBase);
var
  i: Integer;
begin
  FFieldData := ASource.FFieldData;
  SetLength(FFieldData, Length(FFieldData));
  for i := 0 to Length(FFieldData) - 1 do
    FFieldData[i].Field := ASource.FFieldData[i].Field.CreateCopy(True);
end;

procedure TWatchResultTypeStructBase.DoFree;
var
  i: Integer;
begin
  for i := 0 to Length(FFieldData) - 1 do
    FFieldData[i].Field.Free;
  FFieldData := nil;
end;

{ TWatchResultTypeStruct }

procedure TWatchResultTypeStruct.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  AConfig.GetValue(APath + 'SubID', int64(ord(dstUnknown)), FStructType, TypeInfo(TLzDbgStructType));
  if AConfig.HasPath(APath+'Anch/', True) then begin
    FAnchestor := TWatchResultData.CreateFromXMLConfig(AConfig, APath+'Anch/', AnEntryTemplate, AnOverrideTemplate, AnAsProto);
    assert((FAnchestor=nil) or (FAnchestor.ValueKind=rdkStruct), 'TWatchResultTypeStruct.LoadDataFromXMLConfig: (FAnchestor=nil) or (FAnchestor.ValueKind=rdkStruct)');
    if (FAnchestor <> nil) and (FAnchestor.ValueKind <> rdkStruct) then
      FreeAndNil(FAnchestor);
  end;

  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
end;

procedure TWatchResultTypeStruct.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  AConfig.SetDeleteValue(APath + 'SubID', FStructType, ord(dstUnknown), TypeInfo(TLzDbgStructType));
  if FAnchestor <> nil then
    FAnchestor.SaveDataToXMLConfig(AConfig, APath+'Anch/', AnAsProto);

  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
end;

procedure TWatchResultTypeStruct.AfterAssign(ATypeOnly: Boolean);
begin
  FAnchestor := FAnchestor.CreateCopy(ATypeOnly);
  inherited AfterAssign(ATypeOnly);
end;

procedure TWatchResultTypeStruct.DoFree;
begin
  inherited DoFree;
  FAnchestor.Free;
end;

{ TWatchResultTypeConverted }

procedure TWatchResultTypeConverted.AfterAssign(ATypeOnly: Boolean);
begin
  inherited AfterAssign(ATypeOnly);
  if FHandler <> nil then
    FHandler.AddReference;
end;

procedure TWatchResultTypeConverted.DoFree;
begin
  inherited DoFree;
  if FHandler <> nil then
    FHandler.ReleaseReference;
end;

{ TWatchResultTypeProc }

procedure TWatchResultTypeProc.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FText := AConfig.GetValue(APath + 'Desc', '');

end;

procedure TWatchResultTypeProc.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetValue(APath + 'Desc', FText);
end;

{ TWatchResultStorageOverrides }

procedure TWatchResultStorageOverrides.Assign(
  ASource: TWatchResultStorageOverrides);
var
  i: Integer;
begin
  Self := ASource;
  SetLength(FOverrideEntries, Length(FOverrideEntries));
  if @_OVERRIDE_DATA.AfterAssign <> @TWatchResultValue.AfterAssign then begin
    for i := 0 to FOverrideCount - 1 do begin
      FOverrideEntries[i].FData.AfterAssign;
    end;
  end;
end;

procedure TWatchResultStorageOverrides.Add(AnIndex: Integer;
  const AnOverrideData: _OVERRIDE_DATA);
begin
  if FOverrideCount >= Length(FOverrideEntries) then
    SetLength(FOverrideEntries, FOverrideCount + 16);
  assert((FOverrideCount = 0) or (FOverrideEntries[FOverrideCount-1].FIndex < AnIndex), 'TWatchResultStorageOverrides.Add: (FOverrideCount = 0) or (FOverrideEntries[FOverrideCount-1].FIndex < AnIndex)');

  FOverrideEntries[FOverrideCount].FIndex := AnIndex;
  FOverrideEntries[FOverrideCount].FData  := AnOverrideData;
  inc(FOverrideCount);
end;

function TWatchResultStorageOverrides.Get(AnIndex: Integer; out
  AnOverrideData: _OVERRIDE_DATA): Boolean;
var
  l, h, m: Integer;
begin
  l := 0;
  h := FOverrideCount-1;
  while h > l do begin
    m := (h+l) div 2;
    if FOverrideEntries[m].FIndex < AnIndex then
      l := m + 1
    else
      h := m;
  end;

  Result := l < FOverrideCount;
  if not Result then
    exit;
  AnOverrideData := FOverrideEntries[l].FData;
  Result := FOverrideEntries[l].FIndex = AnIndex;
end;

procedure TWatchResultStorageOverrides.Clear;
var
  i: Integer;
begin
  if @_OVERRIDE_DATA.DoFree <> @TWatchResultValue.DoFree then begin
    for i := 0 to FOverrideCount - 1 do begin
      FOverrideEntries[i].FData.DoFree;
    end;
  end;
  FOverrideEntries := nil;
  FOverrideCount := 0;
end;

procedure TWatchResultStorageOverrides.AfterLastAdd;
begin
  SetLength(FOverrideEntries, FOverrideCount);
end;

{ TWatchResultStorageOverridesWithData }

procedure TWatchResultStorageOverridesWithData.Add(AnIndex: Integer;
  var AStoredData: _ENTRY_DATA; const AData: _OVERRIDE_DATA);
var
  AStoredDataByte:  Byte absolute AStoredData;
  AStoredDataWord:  Word absolute AStoredData;
  AStoredDataDWord: DWord absolute AStoredData;
begin
  inherited Add(AnIndex, AData);
  AStoredData := Default(_ENTRY_DATA);
  case SizeOf(_ENTRY_DATA) of
    0:   ;
    1:   AStoredDataByte  := DATA_OVERRIDE_MARK_B;
    2,3: AStoredDataWord  := DATA_OVERRIDE_MARK_W;
    else AStoredDataDWord := DATA_OVERRIDE_MARK_L;
  end;
end;

function TWatchResultStorageOverridesWithData.Get(AnIndex: Integer;
  const AStoredData: _ENTRY_DATA; out AData: _OVERRIDE_DATA): Boolean;
var
  AStoredDataByte:  Byte absolute AStoredData;
  AStoredDataWord:  Word absolute AStoredData;
  AStoredDataDWord: DWord absolute AStoredData;
begin
  Result := False;
  case SizeOf(_ENTRY_DATA) of
    0:   ;
    1:   if AStoredDataByte  <> DATA_OVERRIDE_MARK_B then exit;
    2,3: if AStoredDataWord  <> DATA_OVERRIDE_MARK_W then exit;
    else if AStoredDataDWord <> DATA_OVERRIDE_MARK_L then exit;
  end;

  Result := inherited Get(AnIndex, AData);
end;

procedure TWatchResultStorageOverridesWithData.Clear(
  var AStoredData: TEntryDataArray; AFirst: Integer);
var
  i, c: Integer;
begin
  i := 0;
  while (i < FOverrideCount) and (FOverrideEntries[i].FIndex < AFirst) do
    inc(i);
  if i >= FOverrideCount then
    exit;
  c := i;
  while i < FOverrideCount do begin
    case SizeOf(_ENTRY_DATA) of
      0:   ;
      1:   assert(PByte (@AStoredData[FOverrideEntries[i].FIndex])^ = DATA_OVERRIDE_MARK_B, 'TWatchResultStorageOverridesWithData.Clear: Mark is set');
      2,3: assert(PWord (@AStoredData[FOverrideEntries[i].FIndex])^ = DATA_OVERRIDE_MARK_W, 'TWatchResultStorageOverridesWithData.Clear: Mark is set');
      else assert(PDWord(@AStoredData[FOverrideEntries[i].FIndex])^ = DATA_OVERRIDE_MARK_L, 'TWatchResultStorageOverridesWithData.Clear: Mark is set');
    end;
    FillByte(AStoredData[FOverrideEntries[i].FIndex], SizeOf(_ENTRY_DATA), 0);
    FOverrideEntries[i].FData.DoFree;

    inc(i);
  end;
  FOverrideCount := c;
  AfterLastAdd;
  //inherited Clear;
end;

procedure TWatchResultStorageOverridesWithData.Clear(AnIndex: Integer;
  var AStoredData: _ENTRY_DATA);
var
  AStoredDataByte:  Byte absolute AStoredData;
  AStoredDataWord:  Word absolute AStoredData;
  AStoredDataDWord: DWord absolute AStoredData;
  ADummy: _OVERRIDE_DATA;
begin
  case SizeOf(_ENTRY_DATA) of
    0:   ;
    1:   if AStoredDataByte  <> DATA_OVERRIDE_MARK_B then exit;
    2,3: if AStoredDataWord  <> DATA_OVERRIDE_MARK_W then exit;
    else if AStoredDataDWord <> DATA_OVERRIDE_MARK_L then exit;
  end;

  if inherited Get(AnIndex, ADummy) then begin
    case SizeOf(_ENTRY_DATA) of
      0:   ;
      1:   AStoredDataByte  := 0;
      2,3: AStoredDataWord  := 0;
      else AStoredDataDWord := 0;
    end;
  end;
end;

{ TWatchResultStorage }

function TWatchResultStorage.GetNestedStoragePtr: PWatchResultStorage;
begin
  Result := nil;
end;

procedure TWatchResultStorage.ImportOverrides(ASource: TWatchResultStorage;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  //
end;

procedure TWatchResultStorage.SetNestedStorage(
  AValue: TWatchResultStorage);
begin
  assert(False, 'TWatchResultStorage.SetNestedStorage: False');
end;

function TWatchResultStorage.GetNestedStorage: TWatchResultStorage;
begin
  Result := nil;
end;

function TWatchResultStorage.CreateCopy: TWatchResultStorage;
begin
  if Self = nil then
    exit(nil);
  Result := TWatchResultStorage(ClassType.Create);
  Result.Assign(Self);
end;

{ TWatchResultDataEnumerator }

function TWatchResultDataEnumerator.GetCurrent: TWatchResultDataFieldInfo;
begin
  Result := FSource.Fields[FIndex];
end;

constructor TWatchResultDataEnumerator.Create(ARes: TWatchResultData);
begin
  FSource := ARes;
  FIndex := -1;
end;

function TWatchResultDataEnumerator.MoveNext: Boolean;
begin
  inc(FIndex);
  Result := FIndex < FSource.FieldCount;
end;

{ TWatchResultData }

function TWatchResultData.GetValueKind: TWatchResultDataKind;
begin
  Result := rdkUnknown;
end;

function TWatchResultData.GetHasDataAddress: Boolean;
begin
  Result := False;
end;

constructor TWatchResultData.CreateEmpty;
begin
  //
end;

class function TWatchResultData.CreateFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string): TWatchResultData;
var
  d1: TWatchResultData;
  d2: TOverrideTemplateData;
begin
  d1 := nil;
  d2 := nil;
  Result := CreateFromXMLConfig(AConfig, APath, d1, d2, False);
end;

class function TWatchResultData.CreateFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean
  ): TWatchResultData;
var
  AnId: TWatchResultDataClassID;
begin
  Result := nil;
  try
    AConfig.GetValue(APath + 'CID', Int64(ord(wdPrePrint)), AnId, TypeInfo(TWatchResultDataClassID));
    Result := WatchResNameToClass[AnId].Create;
    Result.LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  except
    Result := TWatchResultDataError.Create('Error: Failed to load from XML'); // TODO: create a class, that will not overwrite the broken xml
  end;
end;

function TWatchResultData.GetClassID: TWatchResultDataClassID;
begin
  Result := wdPrePrint;
end;

function TWatchResultData.GetConvertedRes: TWatchResultData;
begin
  Result := Self;
end;

function TWatchResultData.GetTypeName: String;
begin
  Result := FTypeName;
end;

function TWatchResultData.GetIsFullDephtEvaluated: Boolean;
begin
  Result := False;
end;

function TWatchResultData.GetBackendValueHandler: ILazDbgValueConverterIntf;
begin
  Result := nil;
end;

procedure TWatchResultData.AfterSaveToIndex(AStorage: TWatchResultStorage;
  AnIndex: Integer; var AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  //
end;

procedure TWatchResultData.AfterLoadFromIndex(AStorage: TWatchResultStorage;
  AnIndex: Integer; const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  //
end;

procedure TWatchResultData.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  FTypeName := AConfig.GetValue(APath + 'TypeName', '');
end;

procedure TWatchResultData.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; AnAsProto: Boolean);
begin
  AConfig.SetDeleteValue(APath + 'CID', GetClassID, int64(ord(wdPrePrint)), TypeInfo(TWatchResultDataClassID));
  AConfig.SetDeleteValue(APath + 'TypeName', FTypeName, '');
end;

procedure TWatchResultData.Assign(ASource: TWatchResultData; ATypeOnly: Boolean
  );
begin
  FTypeName := ASource.FTypeName;
end;

function TWatchResultData.CreateCopy(ATypeOnly: Boolean): TWatchResultData;
begin
  if Self = nil then
    exit(nil);
  Result := TWatchResultData(ClassType.Create);
  Result.Assign(Self, ATypeOnly);
end;

function TWatchResultData.HandleExpressionSuffix(ASuffix: String
  ): TWatchResultData;
begin
  Result := Self;
  if ASuffix <> '' then
    Result := TWatchResultDataError.Create('Can''t evaluate: ' + ASuffix);
end;

function TWatchResultData.GetEnumerator: TWatchResultDataEnumerator;
begin
  Result := TWatchResultDataEnumerator.Create(Self);
end;

procedure TWatchResultData.SetSelectedIndex(AnIndex: Integer);
begin
  //
end;

{ TWatchResultDataEx }

procedure TWatchResultDataEx.SetTypeName(ATypeName: String);
begin
  FTypeName := ATypeName;
end;

procedure TWatchResultDataEx.SetDerefData(ADerefData: TWatchResultData);
begin
  assert(False, 'TWatchResultDataEx.SetDerefData: False');
end;

procedure TWatchResultDataEx.SetDataAddress(AnAddr: TDbgPtr);
begin
  assert(False, 'TWatchResultDataEx.SetDataAddress: False');
end;

procedure TWatchResultDataEx.SetEntryPrototype(AnEntry: TWatchResultData);
begin
  assert(False, 'TWatchResultDataEx.SetEntryPrototype: False');
end;

procedure TWatchResultDataEx.WriteEntryToStorage(AnIndex: Integer);
begin
  assert(False, 'TWatchResultDataEx.WriteEntryToStorage: False');
end;

procedure TWatchResultDataEx.WriteValueToStorage(AnIndex: Integer;
  AValue: TWatchResultData);
begin
  assert(False, 'TWatchResultDataEx.WriteValueToStorage: False');
end;

procedure TWatchResultDataEx.SetEntryCount(ACount: Integer);
begin
  assert(False, 'TWatchResultDataEx.SetEntryCount: False');
end;

procedure TWatchResultDataEx.SetAnchestor(AnAnchestor: TWatchResultData);
begin
  assert(False, 'TWatchResultData.SetAnchestor: False');
end;

procedure TWatchResultDataEx.SetFieldCount(ACount: integer);
begin
  assert(False, 'TWatchResultDataEx.SetFieldCount: False');
end;

procedure TWatchResultDataEx.SetField(AnIndex: Integer; AFieldName: String;
  AVisibility: TLzDbgFieldVisibility; AFlags: TLzDbgFieldFlags;
  AData: TWatchResultData);
begin
  assert(False, 'TWatchResultDataEx.SetField: False');
end;

function TWatchResultDataEx.AddField(AFieldName: String;
  AVisibility: TLzDbgFieldVisibility; AFlags: TLzDbgFieldFlags;
  AData: TWatchResultData): Integer;
begin
  assert(False, 'TWatchResultData.AddField: False');
  Result := 0;
end;

procedure TWatchResultDataEx.SetFieldData(AnIndex: Integer; AData: TWatchResultData);
begin
  assert(False, 'TWatchResultData.SetFieldData: False');
end;

{ TGenericWatchResultData.TGenericBasicWatchResultStorage }

function TGenericWatchResultData.TGenericBasicWatchResultStorage.GetCount: integer;
begin
  Result := Length(FDataArray);
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.SetCount(
  AValue: integer);
var
  i: SizeInt;
begin
  if @_DATA.DoFree <> @TWatchResultValue.DoFree then begin
    for i := AValue to Length(FDataArray) - 1 do
      FDataArray[i].DoFree;
  end;
  SetLength(FDataArray, AValue);
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.AssignDataArray
  (ASource: TWatchResultStorage);
var
  Src: TGenericBasicWatchResultStorage absolute ASource;
  i: Integer;
begin
  FDataArray := Src.FDataArray;
  SetLength(FDataArray, Length(FDataArray));
  if @_DATA.AfterAssign <> @TWatchResultValue.AfterAssign then begin
    for i := 0 to Length(FDataArray) - 1 do
      FDataArray[i].AfterAssign;
  end;
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.Assign(
  ASource: TWatchResultStorage);
begin
  assert(ASource.ClassType = ClassType, 'TGenericWatchResultDataArrayStorageHelper.Assign: ASource.ClassType = ClassType');
  if not (ASource is TGenericBasicWatchResultStorage) then
    exit;

  AssignDataArray(ASource);
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.ImportOverrides
  (ASource: TWatchResultStorage; var AnOverrideTemplate: TOverrideTemplateData);
begin
  assert(False, 'TGenericWatchResultData.TGenericBasicWatchResultStorage.ImportOverrides: False');
end;

destructor TGenericWatchResultData.TGenericBasicWatchResultStorage.Destroy;
begin
  Count := 0;
  inherited Destroy;
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.LoadDataFromXMLConfig
  (const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; ANestLvl: Integer);
var
  l: Int64;
  i: Integer;
  PathNst: String;
begin
  PathNst := '';
  if ANestLvl > 0 then PathNst := 'N'+IntToStr(ANestLvl);

  l := AConfig.GetValue(APath+TAG_CNT+PathNst, 0);
  SetLength(FDataArray, l);

  if ANestLvl > 0 then PathNst := PathNst+'/';
  for i := 0 to Length(FDataArray) - 1 do
    FDataArray[i].LoadDataFromXMLConfig(AConfig, APath+'E'+IntToStr(i)+'/'+PathNst, AnEntryTemplate, AnOverrideTemplate, False);
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.SaveDataToXMLConfig
  (const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer);
var
  i: Integer;
  PathNst: String;
begin
  PathNst := '';
  if ANestLvl > 0 then PathNst := 'N'+IntToStr(ANestLvl);

  AConfig.SetDeleteValue(APath+TAG_CNT+PathNst, Length(FDataArray), 0);

  if ANestLvl > 0 then PathNst := PathNst+'/';
  for i := 0 to Length(FDataArray) - 1 do
    FDataArray[i].SaveDataToXMLConfig(AConfig, APath+'E'+IntToStr(i)+'/'+PathNst, False);
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.SaveToIndex(
  AnIndex: Integer; AData: TWatchResultData;
  var AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  assert(AData.GetStorageClass = ClassType, 'TGenericWatchResultData.TGenericBasicWatchResultStorage.SaveToIndex: AData.GetStorageClass = ClassType');
  assert(AData is TGenericWatchResultData);

  FDataArray[AnIndex] := TGenericWatchResultData(AData).FData;
  AData.AfterSaveToIndex(Self, AnIndex, AnEntryTemplate, AnOverrideTemplate);
  TGenericWatchResultData(AData).FData := Default(_DATA);
end;

procedure TGenericWatchResultData.TGenericBasicWatchResultStorage.LoadFromIndex(
  AnIndex: Integer; out AData: TWatchResultData;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  assert(AnEntryTemplate <> nil);
  assert(AnEntryTemplate is TGenericWatchResultData, 'TGenericWatchResultData.TGenericBasicWatchResultStorage.LoadFromIndex: AnEntryTemplate is _DATA');
  TGenericWatchResultData(AnEntryTemplate).FData := FDataArray[AnIndex];
  AData := AnEntryTemplate;
  AData.AfterLoadFromIndex(Self, AnIndex, AnEntryTemplate, AnOverrideTemplate);
end;

{ TGenericWatchResultData.TGenericWatchResultStorage }

procedure TGenericWatchResultData.TGenericWatchResultStorage.SetCount( AValue: integer);
begin
  if AValue < Count then
    FErrorStore.Clear(FDataArray, AValue);
  inherited SetCount(AValue);
  FErrorStore.AfterLastAdd; // TODO: TRIM, once only finished // TODO maybe only explicit
end;

procedure TGenericWatchResultData.TGenericWatchResultStorage.AssignDataArray(
  ASource: TWatchResultStorage);
var
  Src: TGenericWatchResultStorage absolute ASource;
  DoAfter: Boolean;
  i: Integer;
  e: _ERROR_DATA;
begin
  assert(ASource is TGenericWatchResultStorage, 'TGenericWatchResultData.TGenericWatchResultStorage.AssignDataArray: ASource is TGenericWatchResultStorage');
  SetLength(FDataArray, Length(Src.FDataArray));
  DoAfter := @_DATA.AfterAssign <> @TWatchResultValue.AfterAssign;
  for i := 0 to Length(FDataArray) - 1 do begin
    if not Src.FErrorStore.Get(i, Src.FDataArray[i], e) then begin
      FDataArray[i] := Src.FDataArray[i];
      if DoAfter then
        FDataArray[i].AfterAssign;
    end
    else
      case SizeOf(_DATA) of
        0:   ;
        1:   PByte (@FDataArray[i])^ := PByte (@Src.FDataArray[i])^;
        2,3: PWord (@FDataArray[i])^ := PWord (@Src.FDataArray[i])^;
        else PDWord(@FDataArray[i])^ := PDWord(@Src.FDataArray[i])^;
      end;
  end;
end;

procedure TGenericWatchResultData.TGenericWatchResultStorage.Assign(
  ASource: TWatchResultStorage);
var
  Src: TGenericWatchResultStorage absolute ASource;
begin
  if (ASource is TGenericWatchResultStorage) then
    FErrorStore.Assign(Src.FErrorStore);
  inherited Assign(ASource);
end;

procedure TGenericWatchResultData.TGenericWatchResultStorage.ImportOverrides(
  ASource: TWatchResultStorage; var AnOverrideTemplate: TOverrideTemplateData);
var
  AnErrSource: TWatchResultDataError.TErrorDataStorage absolute ASource;
  i: Integer;
begin
  assert(ASource.ClassType = TWatchResultDataError.TErrorDataStorage, 'TGenericWatchResultData.TGenericWatchResultStorage.ImportOverrides: ASource.ClassInfo = TWatchResultDataError.TErrorDataStorage');
  Count := ASource.Count;

  for i := 0 to Count - 1 do begin
    if _ERROR_DATA(AnErrSource.FDataArray[i]).FText <> '' then // TODO: support for not-assigned entries
      FErrorStore.Add(i, FDataArray[i], AnErrSource.FDataArray[i]);
    AnErrSource.FDataArray[i] := Default(_ERROR_DATA);
  end;

  assert(AnOverrideTemplate = nil, 'TGenericWatchResultData.TGenericWatchResultStorage.ImportOverrides: AnOverrideTemplate = nil');
  //if AnOverrideTemplate = nil then
  AnOverrideTemplate := _ERROR_CLASS.CreateEmpty;
end;

destructor TGenericWatchResultData.TGenericWatchResultStorage.Destroy;
begin
  FErrorStore.Clear(FDataArray);
  inherited Destroy;
end;

procedure TGenericWatchResultData.TGenericWatchResultStorage.LoadDataFromXMLConfig
  (const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; ANestLvl: Integer);
var
  l: Int64;
  i: Integer;
  e: _ERROR_DATA;
  PathNst, p: String;
  AllErr: Boolean;
begin
  PathNst := '';
  if ANestLvl > 0 then PathNst := 'N'+IntToStr(ANestLvl);

  AllErr := (AConfig.GetValue(APath+TWatchResultStorage.TAG_ALL_ERR, -1) = 0) and
            (AnEntryTemplate.ValueKind <> rdkError);

  l := AConfig.GetValue(APath+TAG_CNT+PathNst, 0);
  SetLength(FDataArray, l);

  if ANestLvl > 0 then PathNst := PathNst+'/';
  for i := 0 to Length(FDataArray) - 1 do begin
    p := APath+'E'+IntToStr(i)+'/'+PathNst;

    if AllErr or (AConfig.GetValue(p+TAG_ERR, 0) = 1) then begin
      e := Default(_ERROR_DATA);
      e.LoadDataFromXMLConfig(AConfig, p, AnEntryTemplate, AnOverrideTemplate, False);
      FErrorStore.Add(i, FDataArray[i], e);
      if AnOverrideTemplate = nil then
        AnOverrideTemplate := _ERROR_CLASS.CreateEmpty;
    end
    else
      FDataArray[i].LoadDataFromXMLConfig(AConfig, p, AnEntryTemplate, AnOverrideTemplate, False);
  end;
end;

procedure TGenericWatchResultData.TGenericWatchResultStorage.SaveDataToXMLConfig
  (const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer);
var
  i: Integer;
  e: _ERROR_DATA;
  PathNst, p: String;
begin
  PathNst := '';
  if ANestLvl > 0 then PathNst := 'N'+IntToStr(ANestLvl);

  AConfig.SetDeleteValue(APath+TAG_CNT+PathNst, Length(FDataArray), 0);

  if ANestLvl > 0 then PathNst := PathNst+'/';
  for i := 0 to Length(FDataArray) - 1 do begin
    p := APath+'E'+IntToStr(i)+'/'+PathNst;

    if FErrorStore.Get(i, FDataArray[i], e) then begin
      AConfig.SetValue(p+TAG_ERR, 1);
      e.SaveDataToXMLConfig(AConfig, p, False);
    end
    else begin
      AConfig.DeleteValue(p+TAG_ERR);
      FDataArray[i].SaveDataToXMLConfig(AConfig, p, False);
    end;
  end;
end;

procedure TGenericWatchResultData.TGenericWatchResultStorage.SaveToIndex(
  AnIndex: Integer; AData: TWatchResultData;
  var AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  if AData.ValueKind = rdkError then begin
    assert(AData is _ERROR_CLASS, '');
    if AnOverrideTemplate = nil then
      AnOverrideTemplate := _ERROR_CLASS.CreateEmpty;
    FErrorStore.Add(AnIndex, FDataArray[AnIndex], _ERROR_CLASS(AData).FData);
    AData.AfterSaveToIndex(Self, AnIndex, AnEntryTemplate, AnOverrideTemplate);
    _ERROR_CLASS(AData).FData := Default(_ERROR_DATA);
    exit;
  end;

  inherited SaveToIndex(AnIndex, AData, AnEntryTemplate, AnOverrideTemplate);
end;

procedure TGenericWatchResultData.TGenericWatchResultStorage.LoadFromIndex(
  AnIndex: Integer; out AData: TWatchResultData;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  if AnOverrideTemplate <> nil then begin
    assert(AnOverrideTemplate is _ERROR_CLASS, 'TGenericWatchResultData.TGenericWatchResultStorage.LoadFromIndex: AnOverrideTemplate is _ERROR_DATA');
    if FErrorStore.Get(AnIndex, FDataArray[AnIndex], _ERROR_CLASS(AnOverrideTemplate).FData) then begin
      AData := AnOverrideTemplate;
      AData.AfterLoadFromIndex(Self, AnIndex, AnEntryTemplate, AnOverrideTemplate);
      exit;
    end;
  end;

  inherited LoadFromIndex(AnIndex, AData, AnEntryTemplate, AnOverrideTemplate);
end;

{ TGenericWatchResultData.TGenericNestedWatchResultStorage }

procedure TGenericWatchResultData.TGenericNestedWatchResultStorage.SetCount(
  AValue: integer);
begin
  inherited SetCount(AValue);
  if FNestedStorage <> nil then
    FNestedStorage.Count := AValue;
end;

function TGenericWatchResultData.TGenericNestedWatchResultStorage.GetNestedStorage: TWatchResultStorage;
begin
  Result := FNestedStorage;
end;

procedure TGenericWatchResultData.TGenericNestedWatchResultStorage.SetNestedStorage
  (AValue: TWatchResultStorage);
begin
  FNestedStorage := AValue;
  if FNestedStorage <> nil then
    FNestedStorage.Count := Count;
end;

function TGenericWatchResultData.TGenericNestedWatchResultStorage.GetNestedStoragePtr: PWatchResultStorage;
begin
  Result := @FNestedStorage;
end;

procedure TGenericWatchResultData.TGenericNestedWatchResultStorage.Assign(
  ASource: TWatchResultStorage);
begin
  inherited Assign(ASource);
  FNestedStorage := ASource.NestedStorage.CreateCopy;
end;

destructor TGenericWatchResultData.TGenericNestedWatchResultStorage.Destroy;
begin
  FreeAndNil(FNestedStorage);
  inherited Destroy;
end;

procedure TGenericWatchResultData.TGenericNestedWatchResultStorage.LoadDataFromXMLConfig
  (const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; ANestLvl: Integer);
var
  t: TWatchResultData;
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, ANestLvl);
  if FNestedStorage <> nil then begin

    if (AConfig.GetValue(APath+TAG_ALL_ERR, -1) = ANestLvl+1) and
    (AnEntryTemplate.ValueKind<> rdkError)
    then begin
      FNestedStorage.Free;
      FNestedStorage := TWatchResultDataError.GetStorageClass.Create;
    end;

    t := AnEntryTemplate;
    if t <> nil then
      t := t.NestedType;
    FNestedStorage.LoadDataFromXMLConfig(AConfig, APath, t, AnOverrideTemplate, ANestLvl+1);
  end;
end;

procedure TGenericWatchResultData.TGenericNestedWatchResultStorage.SaveDataToXMLConfig
  (const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, ANestLvl);
  if FNestedStorage <> nil then
    FNestedStorage.SaveDataToXMLConfig(AConfig, APath, ANestLvl+1);
end;

{ TGenericWatchResultData }

class function TGenericWatchResultData.GetStorageClass: TWatchResultStorageClass;
begin
  Result := TGenericWatchResultStorage;
end;

function TGenericWatchResultData.CreateStorage: TWatchResultStorage;
begin
  Result := GetStorageClass.Create;
end;

class procedure TGenericWatchResultData.UpdateStorage(
  AStorage: PWatchResultStorage; const AData: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
var
  OldStorage: TWatchResultStorage;
begin
  OldStorage := AStorage^;
  AStorage^ := AData.CreateStorage;
  AStorage^.ImportOverrides(OldStorage, AnOverrideTemplate);
  OldStorage.Free;
end;

function TGenericWatchResultData.MaybeUpdateProto(
  var AProtoData: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AStorage: PWatchResultStorage;
  ARecurse: boolean; ASkipStorage: boolean): boolean;
begin
  Result := (Self <> nil) and
            ( (AProtoData = nil) or
              ( (AProtoData.ValueKind = rdkError) and
                (Self.ValueKind <> rdkError)
            ) );

  if Result then begin
    if (AProtoData <> nil) then begin
      AProtoData.Free;
    end;
    AProtoData := Self.CreateCopy(True);

    if not ASkipStorage and (AStorage <> nil) and (AStorage^ <> nil) then
      UpdateStorage(AStorage, Self, AnOverrideTemplate);
  end;
end;

procedure TGenericWatchResultData.ClearData;
begin
  FData := Default(_DATA);
end;

function TGenericWatchResultData.GetValueKind: TWatchResultDataKind;
begin
  Result := FData.VKind;
end;

function TGenericWatchResultData.GetIsFullDephtEvaluated: Boolean;
begin
  Result := FData.GetIsDephtlessData;
end;

function TGenericWatchResultData.GetAsString: String;
begin
  Result := FData.GetAsString;
end;

function TGenericWatchResultData.GetAsDesc: String;
begin
  Result := '';
end;

function TGenericWatchResultData.GetAsWideString: WideString;
begin
  Result := FData.GetAsWideString;
end;

function TGenericWatchResultData.GetAsQWord: QWord;
begin
  Result := FData.GetAsQWord;
end;

function TGenericWatchResultData.GetAsInt64: Int64;
begin
  Result := FData.GetAsInt64;
end;

function TGenericWatchResultData.GetAsFloat: Extended;
begin
  Result := FData.GetAsFloat;
end;

function TGenericWatchResultData.GetCount: Integer;
begin
  Result := FData.GetCount;
end;

function TGenericWatchResultData.GetLength: Integer;
begin
  Result := FData.GetLength;
end;

function TGenericWatchResultData.GetElementName(AnIndex: integer): String;
begin
  Result := FData.GetElementName(AnIndex);
end;

function TGenericWatchResultData.GetDerefData: TWatchResultData;
begin
  Result := FData.GetDerefData;
end;

function TGenericWatchResultData.GetNestedType: TWatchResultData;
begin
  Result := FData.GetEntryTemplate;
end;

function TGenericWatchResultData.GetArrayType: TLzDbgArrayType;
begin
  Result := datUnknown;
end;

function TGenericWatchResultData.GetBoundType: TWatchResultData;
begin
  Result := FData.GetBoundType;
end;

function TGenericWatchResultData.GetLowBound: Int64;
begin
  Result := FData.GetLowBound;
end;

function TGenericWatchResultData.GetByteSize: Integer;
begin
  Result := FData.GetByteSize;
end;

function TGenericWatchResultData.GetFloatPrecission: TLzDbgFloatPrecission;
begin
  Result := FData.GetFloatPrecission;
end;

function TGenericWatchResultData.GetSelectedEntry: TWatchResultData;
begin
  Result := FData.GetEntryTemplate;
end;

function TGenericWatchResultData.GetDataAddress: TDBGPtr;
begin
  Result := FData.GetDataAddress;
end;

function TGenericWatchResultData.GetStructType: TLzDbgStructType;
begin
  Result := dstUnknown;
end;

function TGenericWatchResultData.GetAnchestor: TWatchResultData;
begin
  Result := Nil;
end;

function TGenericWatchResultData.GetAnchestorCount: Integer;
var
  a: TWatchResultData;
begin
  Result := 0;
  a := GetAnchestor;
  while a <> nil do begin
    inc(Result);
    a := a.GetAnchestor;
  end;
end;

function TGenericWatchResultData.GetAnchestors(AnIndex: Integer): TWatchResultData;
begin
  Result := GetAnchestor;
  if (AnIndex > 0) and (Result <> nil) then
    Result := Result.Anchestors[AnIndex - 1];
end;

function TGenericWatchResultData.GetDirectFieldCount: Integer;
begin
  Result := 0;
end;

function TGenericWatchResultData.GetFieldCount: Integer;
begin
  Result := 0;
end;

function TGenericWatchResultData.GetFields(AnIndex: Integer
  ): TWatchResultDataFieldInfo;
begin
  Result := Default(TWatchResultDataFieldInfo);
end;

function TGenericWatchResultData.GetFieldVisibility: TLzDbgFieldVisibility;
begin
  Result := dfvUnknown;
end;

destructor TGenericWatchResultData.Destroy;
begin
  FData.DoFree;
  inherited Destroy;
end;

procedure TGenericWatchResultData.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  if not AnAsProto then
    FData.LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
end;

procedure TGenericWatchResultData.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  if not AnAsProto then
    FData.SaveDataToXMLConfig(AConfig, APath, AnAsProto);
end;

procedure TGenericWatchResultData.Assign(ASource: TWatchResultData;
  ATypeOnly: Boolean);
var
  Src: TGenericWatchResultData absolute ASource;
begin
  inherited Assign(ASource, ATypeOnly);
  if ATypeOnly or (not (ASource is TGenericWatchResultData)) then
    exit;

  FData := Src.FData;
  FData.AfterAssign;
end;

{ TGenericWatchResultDataWithType }

function TGenericWatchResultDataWithType.GetByteSize: Integer;
begin
  Result := FType.GetByteSize;
end;

function TGenericWatchResultDataWithType.GetFloatPrecission: TLzDbgFloatPrecission;
begin
  Result := FType.GetFloatPrecission;
end;

function TGenericWatchResultDataWithType.GetNestedType: TWatchResultData;
begin
  Result := FType.GetEntryTemplate;
end;

function TGenericWatchResultDataWithType.GetBoundType: TWatchResultData;
begin
  Result := FType.GetBoundType;
end;

function TGenericWatchResultDataWithType.GetLowBound: Int64;
begin
  Result := FType.GetLowBound;
end;

function TGenericWatchResultDataWithType.GetSelectedEntry: TWatchResultData;
begin
  Result := FType.GetEntryTemplate;
end;

function TGenericWatchResultDataWithType.GetStructType: TLzDbgStructType;
begin
  Result := FType.GetStructType;
end;

function TGenericWatchResultDataWithType.GetAnchestor: TWatchResultData;
begin
  Result := FType.GetAnchestor;
end;

function TGenericWatchResultDataWithType.GetDirectFieldCount: Integer;
begin
  Result := FType.GetFieldCount;
end;

function TGenericWatchResultDataWithType.GetFieldCount: Integer;
var
  a: TWatchResultData;
begin
  Result := FType.GetFieldCount;
  a := Anchestor;
  while a <> nil do begin
    Result := Result + a .GetDirectFieldCount;
    a := a.Anchestor;
  end;
end;

function TGenericWatchResultDataWithType.GetFields(AnIndex: Integer): TWatchResultDataFieldInfo;
var
  AnAnchestor: TWatchResultData;
  AnAnchestorIdx: Integer;
  inf: PWatchResultTypeStructFieldInfo;
begin
  if AnIndex < FType.GetFieldCount then begin
    inf := FType.GetFieldInfo(AnIndex);
    Result.FieldName       := inf^.FieldName;
    Result.FieldVisibility := inf^.FieldVisibility;
    Result.FieldFlags      := inf^.FieldFlags;
    Result.Field           := inf^.Field;
    Result.Owner           := Self;
    exit;
  end;

  AnAnchestorIdx := AnIndex;
  AnAnchestor := Self;
  repeat
    AnAnchestorIdx := AnAnchestorIdx - AnAnchestor.GetDirectFieldCount;
    AnAnchestor := AnAnchestor.Anchestor;
  until (AnAnchestor = nil) or (AnAnchestorIdx < AnAnchestor.GetDirectFieldCount);

  if Anchestor = nil then begin
    Result := Default(TWatchResultDataFieldInfo);
    exit;
  end;

  Result := AnAnchestor.GetFields(AnAnchestorIdx);
end;

destructor TGenericWatchResultDataWithType.Destroy;
begin
  FType.DoFree;
  inherited Destroy;
end;

procedure TGenericWatchResultDataWithType.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
  FType.LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, AnAsProto);
end;

procedure TGenericWatchResultDataWithType.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  FType.SaveDataToXMLConfig(AConfig, APath, AnAsProto);
end;

procedure TGenericWatchResultDataWithType.Assign(ASource: TWatchResultData;
  ATypeOnly: Boolean);
var
  Src: TGenericWatchResultDataWithType absolute ASource;
begin
  inherited Assign(ASource, ATypeOnly);
  if not (ASource is TGenericWatchResultDataWithType) then
    exit;
  FType := Src.FType;
  FType.AfterAssign(ATypeOnly);
end;

{ TWatchResultDataPrePrinted }

function TWatchResultDataPrePrinted.GetClassID: TWatchResultDataClassID;
begin
  Result := wdPrePrint;
end;

constructor TWatchResultDataPrePrinted.Create(APrintedVal: String);
begin
  inherited Create;
  FData.FText := APrintedVal;
end;

{ TWatchResultDataString }

function TWatchResultDataString.GetClassID: TWatchResultDataClassID;
begin
  Result := wdString;
end;

function TWatchResultDataString.GetHasDataAddress: Boolean;
begin
  Result := True;
end;

function TWatchResultDataString.GetDataAddress: TDBGPtr;
begin
  Result := FData.GetDataAddress;
end;

procedure TWatchResultDataString.SetDataAddress(AnAddr: TDbgPtr);
begin
  FData.FAddress := AnAddr;
end;

constructor TWatchResultDataString.Create(AStringVal: String);
begin
  inherited Create;
  FData.FText := AStringVal;
end;

{ TWatchResultDataWideString }

function TWatchResultDataWideString.GetClassID: TWatchResultDataClassID;
begin
  Result := wdWString;
end;

function TWatchResultDataWideString.GetHasDataAddress: Boolean;
begin
  Result := True;
end;

function TWatchResultDataWideString.GetDataAddress: TDBGPtr;
begin
  Result := FData.FAddress;
end;

procedure TWatchResultDataWideString.SetDataAddress(AnAddr: TDbgPtr);
begin
  FData.FAddress := AnAddr;
end;

constructor TWatchResultDataWideString.Create(AStringVal: WideString);
begin
  inherited Create;
  FData.FWideText := AStringVal;
end;

{ TGenericWatchResultDataSizedNum }

function TGenericWatchResultDataSizedNum.GetAsQWord: QWord;
begin
  Result := FData.GetAsQWord;
  if (FType.GetByteSize > 0) and (FType.GetByteSize < 8) then
    Result := Result and not(QWord(-1) << (FType.GetByteSize<<3));
end;

function TGenericWatchResultDataSizedNum.GetAsInt64: Int64;
begin
  Result := FData.GetAsInt64;
  if (FType.GetByteSize > 0) and (FType.GetByteSize < 8) then begin
    if Result and (1 << ((FType.GetByteSize<<3) - 1)) <> 0 then
      Result := Result or (Int64(-1) << (FType.GetByteSize<<3))
    else
      Result := Result and not(Int64(-1) << (FType.GetByteSize<<3));
  end;
end;

{ TWatchResultDataChar }

function TWatchResultDataChar.GetClassID: TWatchResultDataClassID;
begin
  Result := wdChar;
end;

constructor TWatchResultDataChar.Create(ANumValue: QWord; AByteSize: Integer);
begin
  inherited Create();
  FData.FNumValue := QWord(ANumValue);
  FType.FNumByteSize := AByteSize;
end;

{ TWatchResultDataSignedNum }

function TWatchResultDataSignedNum.GetClassID: TWatchResultDataClassID;
begin
  Result := wdSNum;
end;

constructor TWatchResultDataSignedNum.Create(ANumValue: Int64;
  AByteSize: Integer);
begin
  inherited Create();
  FData.FNumValue := QWord(ANumValue);
  FType.FNumByteSize := AByteSize;
end;

{ TWatchResultDataUnSignedNum }

function TWatchResultDataUnSignedNum.GetClassID: TWatchResultDataClassID;
begin
  Result := wdUNum;
end;

constructor TWatchResultDataUnSignedNum.Create(ANumValue: QWord;
  AByteSize: Integer);
begin
  inherited Create();
  FData.FNumValue := QWord(ANumValue);
  FType.FNumByteSize := AByteSize;
end;

{ TWatchResultDataPointer }

function TWatchResultDataPointer.GetClassID: TWatchResultDataClassID;
begin
  Result := wdPtr;
end;

function TWatchResultDataPointer.GetIsFullDephtEvaluated: Boolean;
begin
  Result := (DerefData <> nil) and (DerefData.IsFullDephtEvaluated);
end;

function TWatchResultDataPointer.GetHasDataAddress: Boolean;
begin
  Result := True;
end;

function TWatchResultDataPointer.GetAsString: String;
begin
  Result := FData.GetAsString + FType.GetAsString;
end;

function TWatchResultDataPointer.GetDerefData: TWatchResultData;
begin
  if FCurrentDerefData <> nil then
    Result := FCurrentDerefData
  else
    Result := FType.GetDerefData;
end;

class function TWatchResultDataPointer.GetStorageClass: TWatchResultStorageClass;
begin
  Result := TGenericNestedWatchResultStorage;
end;

function TWatchResultDataPointer.CreateStorage: TWatchResultStorage;
begin
  Result := inherited CreateStorage;
  if FType.FDerefData <> nil then
    TGenericNestedWatchResultStorage(Result).FNestedStorage := FType.FDerefData.CreateStorage;
end;

function TWatchResultDataPointer.MaybeUpdateProto(
  var AProtoData: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AStorage: PWatchResultStorage;
  ARecurse: boolean; ASkipStorage: boolean): boolean;
var
  APtrProtoData: TWatchResultDataPointer absolute AProtoData;
  Store: PWatchResultStorage;
begin
  Result := inherited MaybeUpdateProto(AProtoData, AnOverrideTemplate, AStorage, ARecurse, ASkipStorage);
  if (not Result) and (AProtoData is TWatchResultDataPointer) and
     ARecurse and (FType.FDerefData <> nil)
  then begin
    Store := AStorage;
    if (Store <> nil) and (Store^ <> nil) then
      Store := Store^.NestedStoragePtr;
    Result := FType.FDerefData.MaybeUpdateProto(APtrProtoData.FType.FDerefData, AnOverrideTemplate,
      Store, ARecurse, ASkipStorage);
  end;
end;

procedure TWatchResultDataPointer.AfterSaveToIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  var AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
var
  APtrEntryTemplate: TWatchResultDataPointer absolute AnEntryTemplate;
begin
  (* SELF is part of TWatchResultDataArrayBase.FCurrentElement
     SELF and AnEntryTemplate can be the same
       -> if they are no update of FDerefData will happen
  *)
  assert(AStorage is TGenericNestedWatchResultStorage, 'TWatchResultDataPointer.AfterSaveToIndex: is TGenericNestedWatchResultStorage');

  inherited AfterSaveToIndex(AStorage, AnIndex, AnEntryTemplate, AnOverrideTemplate);

  if (FType.FDerefData <> nil) then begin
    assert(AnEntryTemplate is TWatchResultDataPointer, 'TWatchResultDataPointer.AfterSaveToIndex: AnEntryTemplate is TWatchResultDataPointer');

    FType.FDerefData.MaybeUpdateProto(
      APtrEntryTemplate.FType.FDerefData,
      AnOverrideTemplate,
      AStorage.NestedStoragePtr
    );

    if (AStorage.NestedStorage = nil) then
      AStorage.NestedStorage := FType.FDerefData.CreateStorage;

    AStorage.NestedStorage.SaveToIndex(AnIndex,
      FType.FDerefData,
      APtrEntryTemplate.FType.FDerefData,
      AnOverrideTemplate);
  end;
end;

procedure TWatchResultDataPointer.AfterLoadFromIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  assert(AStorage is TGenericNestedWatchResultStorage, 'TWatchResultDataPointer.AfterLoadFromIndex: is TGenericNestedWatchResultStorage');

  inherited AfterLoadFromIndex(AStorage, AnIndex, AnEntryTemplate, AnOverrideTemplate);

  if (FType.FDerefData <> nil) then begin
    if (AStorage.NestedStorage <> nil) then
      AStorage.NestedStorage.LoadFromIndex(AnIndex, FCurrentDerefData, FType.FDerefData, AnOverrideTemplate);
  end;
end;

procedure TWatchResultDataPointer.ClearData;
begin
  if (FType.FDerefData <> nil) then
    FType.FDerefData.ClearData;

  inherited ClearData;
end;

procedure TWatchResultDataPointer.SetDerefData(ADerefData: TWatchResultData);
begin
  FType.FDerefData := ADerefData;
end;

constructor TWatchResultDataPointer.Create(AnAddr: TDBGPtr);
begin
  inherited Create();
  FData.FNumValue := QWord(AnAddr);
end;

{ TWatchResultDataFloat }

function TWatchResultDataFloat.GetClassID: TWatchResultDataClassID;
begin
  Result := wdFloat;
end;

constructor TWatchResultDataFloat.Create(AFloatValue: Extended;
  APrecission: TLzDbgFloatPrecission);
begin
  inherited Create;
  FData.FFloatValue := AFloatValue;
  FType.FFloatPrecission := APrecission;
end;

{ TWatchResultDataBoolean }

function TWatchResultDataBoolean.GetClassID: TWatchResultDataClassID;
begin
  Result := wdBool;
end;

constructor TWatchResultDataBoolean.Create(AnOrdBoolValue: QWord;
  AByteSize: Integer);
begin
  inherited Create;
  FData.FNumValue    := AnOrdBoolValue;
  FType.FNumByteSize := AByteSize;
end;

constructor TWatchResultDataBoolean.Create(ABoolValue: Boolean);
begin
  inherited Create;
  if ABoolValue then
    FData.FNumValue := 1
  else
    FData.FNumValue := 0;
  FType.FNumByteSize := 0;
end;

{ TWatchResultDataEnum }

function TWatchResultDataEnum.GetClassID: TWatchResultDataClassID;
begin
  Result := wdEnum;
end;

constructor TWatchResultDataEnum.Create(ANumValue: QWord; AName: String;
  AByteSize: Integer);
begin
  inherited Create;
  FData.FNumValue    := ANumValue;
  FData.FName        := AName;
  FType.FNumByteSize := AByteSize;
end;

{ TWatchResultDataEnumVal }

function TWatchResultDataEnumVal.GetClassID: TWatchResultDataClassID;
begin
  Result := wdEnumVal;
end;

constructor TWatchResultDataEnumVal.Create(ANumValue: QWord; AName: String;
  AByteSize: Integer);
begin
  inherited Create;
  FData.FNumValue    := ANumValue;
  FData.FName        := AName;
  FType.FNumByteSize := AByteSize;
end;

{ TWatchResultDataSet }

function TWatchResultDataSet.GetClassID: TWatchResultDataClassID;
begin
  Result := wdSet;
end;

constructor TWatchResultDataSet.Create(const ANames: TStringDynArray);
begin
  inherited Create;
  FData.FNames := ANames;
end;

{ TWatchResultDataVariant }

function TWatchResultDataVariant.GetClassID: TWatchResultDataClassID;
begin
  Result := wdVar;
end;

function TWatchResultDataVariant.GetFieldVisibility: TLzDbgFieldVisibility;
begin
  Result := FData.FVisibility;
end;

constructor TWatchResultDataVariant.Create(AName: String;
  AVisibility: TLzDbgFieldVisibility);
begin
  inherited Create;
  FData.FName := AName;
  FData.FVisibility := AVisibility;
end;

procedure TWatchResultDataVariant.SetDerefData(ADerefData: TWatchResultData);
begin
  FData.FVariantData := ADerefData;
end;

{ TWatchResultDataArrayBase }

function TWatchResultDataArrayBase.GetIsFullDephtEvaluated: Boolean;
begin
  Result := FData.GetIsDephtlessData and  // No partly elevated elements
            (Count = ArrayLength);        // all elements present
  // TODO: if not GetIsDephtlessData => iterate all elements
end;

procedure TWatchResultDataArrayBase.SetEntryPrototype(AnEntry: TWatchResultData);
begin
  assert(FType.FEntryTemplate=nil, 'TWatchResultDataArrayBase.SetEntryPrototype: FType.FEntryTemplate=nil');

  if FType.FEntryTemplate = nil then   // never reached => done above
    FType.FEntryTemplate := AnEntry.CreateCopy(True);

  FCurrentElement := FType.FEntryTemplate; // Reference without ownership
end;

procedure TWatchResultDataArrayBase.WriteValueToStorage(AnIndex: Integer; AValue: TWatchResultData);
begin
  assert(AValue <> nil, 'TWatchResultDataArrayBase.WriteValueToStorage: AValue <> nil');

  if AValue.MaybeUpdateProto(
      FType.FEntryTemplate,
      FOverrideTemplateData,
      @FData.FEntries
    )
  then begin
    FCurrentElement := FType.FEntryTemplate;
  end;

  assert(FData.FEntries <> nil, 'TWatchResultDataArrayBase.WriteValueToStorage: FData.FEntries <> nil');
  assert(AnIndex<FData.FEntries.Count, 'TWatchResultDataArrayBase.WriteValueToStorage: AnIndex<FData.FEntries.Count');
  FData.FEntries.SaveToIndex(AnIndex, AValue, FType.FEntryTemplate, FOverrideTemplateData);
end;

procedure TWatchResultDataArrayBase.SetEntryCount(ACount: Integer);
begin
  assert((ACount=0) or (FType.FEntryTemplate<>nil), 'TWatchResultDataArrayBase.SetEntryCount: (ACount=0) or (FType.FEntryTemplate<>nil)');
  if ACount = 0 then begin
    FreeAndNil(FData.FEntries);
    exit;
  end;

  if FData.FEntries = nil then
    FData.FEntries := FType.FEntryTemplate.CreateStorage;
  FData.FEntries.Count := ACount;
end;

function TWatchResultDataArrayBase.MaybeUpdateProto(
  var AProtoData: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AStorage: PWatchResultStorage;
  ARecurse: boolean; ASkipStorage: boolean): boolean;
var
  AnArrayProtoData: TWatchResultDataArrayBase absolute AProtoData;
  Store: PWatchResultStorage;
begin
  Result := inherited MaybeUpdateProto(AProtoData, AnOverrideTemplate,
    AStorage, ARecurse, ASkipStorage);
  if (not Result) and (AProtoData is TWatchResultDataArrayBase) and
     ARecurse and (FType.FEntryTemplate <> nil)
  then begin
    Store := AStorage;
    if (Store <> nil) and (Store^ <> nil) then
      Store := Store^.NestedStoragePtr;
    Result := FType.FEntryTemplate.MaybeUpdateProto(AnArrayProtoData.FType.FEntryTemplate, AnOverrideTemplate,
      Store, ARecurse, ASkipStorage);
  end;
end;

procedure TWatchResultDataArrayBase.AfterSaveToIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  var AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
var
  AnArrayEntryTemplate: TWatchResultDataArrayBase absolute AnEntryTemplate;
begin
  assert(AnEntryTemplate is TWatchResultDataArrayBase, 'TWatchResultDataArrayBase.AfterSaveToIndex: AnEntryTemplate is TWatchResultDataArrayBase');
  (* SELF is part of TWatchResultDataArrayBase.FCurrentElement
     SELF and AnEntryTemplate can be the same
     -> if they are no update of FEntryTemplate will happen
  *)

  inherited AfterSaveToIndex(AStorage, AnIndex, AnEntryTemplate, AnOverrideTemplate);

  if (FType.FEntryTemplate <> nil) then begin
    FType.FEntryTemplate.MaybeUpdateProto(
      AnArrayEntryTemplate.FType.FEntryTemplate,
      AnOverrideTemplate,
      @FData.FEntries,
      True,
      True
    );
  end;

  if (AnOverrideTemplate = nil) and (FOverrideTemplateData <> nil) then
    AnOverrideTemplate := TGenericWatchResultStorage._ERROR_CLASS.CreateEmpty;
end;

procedure TWatchResultDataArrayBase.AfterLoadFromIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  inherited AfterLoadFromIndex(AStorage, AnIndex, AnEntryTemplate, AnOverrideTemplate);

  if FOverrideTemplateData = nil then
    FOverrideTemplateData := TOverrideTemplateData(AnOverrideTemplate.CreateCopy(True));
end;

procedure TWatchResultDataArrayBase.ClearData;
begin
  if (FType.FEntryTemplate <> nil) then
    FType.FEntryTemplate.ClearData;

  inherited ClearData;
end;

destructor TWatchResultDataArrayBase.Destroy;
begin
  FCurrentElement := Nil;
  if (FType.FEntryTemplate <> nil) and (FData.FEntries <> nil) then
    FType.FEntryTemplate.ClearData;

  if FOverrideTemplateData <> nil then begin
    assert(FOverrideTemplateData is TWatchResultDataError, 'TWatchResultDataArrayBase.Destroy: FOverrideTemplateData is TWatchResultDataError');
    //FOverrideTemplateData.ClearData;
    FreeAndNil(FOverrideTemplateData);
  end;

  inherited Destroy;
end;

procedure TWatchResultDataArrayBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AnAsProto: Boolean);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate, AnOverrideTemplate, True);
  if not AnAsProto then begin
    FData.LoadDataFromXMLConfig(AConfig, APath, Self, FOverrideTemplateData, False);
  end;
end;

procedure TWatchResultDataArrayBase.Assign(ASource: TWatchResultData;
  ATypeOnly: Boolean);
var
  Src: TWatchResultDataArrayBase absolute ASource;
begin
  if ASource is TWatchResultDataArrayBase then begin
    if Src.FOverrideTemplateData <> nil then
      FOverrideTemplateData := TOverrideTemplateData.CreateEmpty;
  end;

  // Do not copy _DATA from nested FEntryTemplate
  // Copy our own FData, since it's blocked by AProtoOnly
  if not ATypeOnly then begin
    FData := Src.FData;
    FData.AfterAssign(True);
  end;
  inherited Assign(ASource, True);
end;

procedure TWatchResultDataArrayBase.SetSelectedIndex(AnIndex: Integer);
begin
  assert((FData.FEntries<>nil) or (AnIndex=0), 'TWatchResultDataArrayBase.SetSelectedIndex: (FData.FEntries<>nil) or (AnIndex=0)');
  if FData.FEntries <> nil then
    FData.FEntries.LoadFromIndex(AnIndex, FCurrentElement, FType.FEntryTemplate, FOverrideTemplateData);
end;

function TWatchResultDataArrayBase.GetSelectedEntry: TWatchResultData;
begin
  Result := FCurrentElement;
end;

{ TWatchResultDataPCharOrString }

function TWatchResultDataPCharOrString.GetClassID: TWatchResultDataClassID;
begin
  Result := wdPChrStr;
end;

function TWatchResultDataPCharOrString.GetIsFullDephtEvaluated: Boolean;
begin
  Result := False; // TODO
end;

{ TWatchResultDataArray }

function TWatchResultDataArray.GetClassID: TWatchResultDataClassID;
begin
  Result := wdArray;
end;

procedure TWatchResultDataPCharOrString.SetEntryPrototype(
  AnEntry: TWatchResultData);
begin
  inherited SetEntryPrototype(AnEntry);
  FTypeName := AnEntry.FTypeName;
end;

function TWatchResultDataArray.GetArrayType: TLzDbgArrayType;
begin
  Result := datUnknown;
end;

function TWatchResultDataArray.GetLowBound: Int64;
begin
  Result := FData.GetLowBound;
end;

constructor TWatchResultDataArray.Create(ALength: Integer; ALowBound: Int64);
begin
  inherited Create;
  FData.FLength := ALength;
  FData.FLowBound := ALowBound;
end;

{ TWatchResultDataDynArray }

function TWatchResultDataDynArray.GetClassID: TWatchResultDataClassID;
begin
  Result := wdDynA;
end;

function TWatchResultDataDynArray.GetArrayType: TLzDbgArrayType;
begin
  Result := datDynArray;
end;

function TWatchResultDataDynArray.GetHasDataAddress: Boolean;
begin
  Result := True;
end;

constructor TWatchResultDataDynArray.Create(ALength: Integer);
begin
  inherited Create;
  FData.FLength := ALength;
end;

procedure TWatchResultDataDynArray.SetDataAddress(AnAddr: TDbgPtr);
begin
  FData.FAddress := AnAddr;
end;

{ TWatchResultDataStatArray }

function TWatchResultDataStatArray.GetClassID: TWatchResultDataClassID;
begin
  Result := wdStatA;
end;

function TWatchResultDataStatArray.GetArrayType: TLzDbgArrayType;
begin
  Result := datStatArray;
end;

function TWatchResultDataStatArray.GetDataAddress: TDBGPtr;
begin
  Result := FData.GetDataAddress;
end;

function TWatchResultDataStatArray.GetLength: Integer;
begin
  Result := FType.GetLength;
end;

constructor TWatchResultDataStatArray.Create(ALength: Integer; ALowBound: Int64
  );
begin
  inherited Create;
  FType.FLength := ALength;
  FType.FLowBound := ALowBound;
end;

{ TGenericWatchResultDataStruct.TWatchResultDataStructVariantEnumerator }

function TGenericWatchResultDataStruct.TWatchResultDataStructVariantEnumerator.GetCurrent: TWatchResultDataFieldInfo;
begin
  FSource.SetSelectedIndex(FIndex);
  Result := Default(TWatchResultDataFieldInfo);
  Result.Field           := FSource.SelectedEntry.DerefData;
  Result.FieldName       := FSource.SelectedEntry.AsString;
  Result.FieldVisibility := FSource.SelectedEntry.FieldVisibility;
end;

constructor TGenericWatchResultDataStruct.TWatchResultDataStructVariantEnumerator.Create
  (ARes: TWatchResultData);
begin
  inherited Create(ARes);
  FIndex := -1;
end;

function TGenericWatchResultDataStruct.TWatchResultDataStructVariantEnumerator.MoveNext: Boolean;
begin
  inc(FIndex);
  Result := FIndex < FSource.Count;
end;

{ TGenericWatchResultDataStruct.TWatchResultDataStructEnumerator }

function TGenericWatchResultDataStruct.TWatchResultDataStructEnumerator.GetCurrent: TWatchResultDataFieldInfo;
begin
  if FSubEnumerator <> nil then begin
    Result := FSubEnumerator.Current;
    Result.Owner := FSubOwner;
  end
  else
    Result := FSource.Fields[FIndex];
end;

constructor TGenericWatchResultDataStruct.TWatchResultDataStructEnumerator.Create
  (ARes: TWatchResultData);
begin
  inherited Create(ARes);
  FIndex := -1;
end;

function TGenericWatchResultDataStruct.TWatchResultDataStructEnumerator.MoveNext: Boolean;
begin
  if FSubEnumerator <> nil then begin
    Result := FSubEnumerator.MoveNext;
    if Result then
      exit
    else
      FreeAndNil(FSubEnumerator);
  end;

  repeat
    inc(FIndex);
    Result := FIndex < FSource.FieldCount;

    if Result and (dffVariant in FSource.Fields[FIndex].FieldFlags) then begin
      FSubOwner := FSource.Fields[FIndex].Owner;
      FSubEnumerator := TGenericWatchResultDataStruct.TWatchResultDataStructVariantEnumerator.Create(
        FSource.Fields[FIndex].Field
      );
      if not FSubEnumerator.MoveNext then begin
        FreeAndNil(FSubEnumerator);
        Continue;
      end;
    end;

    break;
  until True;
end;

{ TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage }

function TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.GetStoredFieldCount: Integer;
begin
  Result := Length(FFieldsStorage);
end;

procedure TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.SetStoredFieldCount
  (AValue: Integer);
var
  i: Integer;
begin
  if AValue = Length(FFieldsStorage) then
    exit;

  for i := AValue to Length(FFieldsStorage) - 1 do
    FreeAndNil(FFieldsStorage[i]);
  SetLength(FFieldsStorage, AValue);

  for i := AValue to Length(FOverrideTempl) - 1 do
    FreeAndNil(FOverrideTempl[i]);
  SetLength(FOverrideTempl, AValue);
end;

procedure TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.SetCount
  (AValue: integer);
var
  i: Integer;
begin
  inherited SetCount(AValue);
  for i := 0 to Length(FFieldsStorage) - 1 do
    if FFieldsStorage[i] <> nil then
      FFieldsStorage[i].SetCount(AValue);
end;

procedure TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.Assign(
  ASource: TWatchResultStorage);
var
  Src: TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage absolute ASource;
  i: Integer;
begin
  inherited Assign(ASource);

  if ASource is TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage then begin
    SetLength(FFieldsStorage, Length(Src.FFieldsStorage));
    for i := 0 to Length(FFieldsStorage) - 1 do
      FFieldsStorage[i] := Src.FFieldsStorage[i].CreateCopy;

    SetLength(FOverrideTempl, Length(Src.FOverrideTempl));
    for i := 0 to Length(FOverrideTempl) - 1 do
      FOverrideTempl[i] := TOverrideTemplateData(Src.FOverrideTempl[i].CreateCopy);
  end;
end;

function TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.GetNestedStorage
  (AnIndex: Integer): TWatchResultStorage;
begin
  Result := FFieldsStorage[AnIndex];
end;

function TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.GetNestedStoragePtr
  (AnIndex: Integer): PWatchResultStorage;
begin
  Result := @FFieldsStorage[AnIndex];
end;

procedure TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.SetNestedStorage
  (AnIndex: Integer; AValue: TWatchResultStorage);
begin
  assert((FFieldsStorage[AnIndex]=nil) or (AValue=nil), 'TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.SetNestedStorage: (FFieldsStorage[AnIndex]=nil) or (AValue=nil)');
  FFieldsStorage[AnIndex] := AValue;

  if AValue <> nil then
    AValue.Count := Count;
end;

destructor TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.Destroy;
begin
  StoredFieldCount := 0;
  inherited Destroy;
end;

procedure TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.LoadDataFromXMLConfig
  (const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; ANestLvl: Integer);
var
  t: TWatchResultData;
  i: Integer;
begin
  assert((AnEntryTemplate=nil) or (AnEntryTemplate is TGenericWatchResultDataStruct), 'TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.LoadDataFromXMLConfig: (AnEntryTemplate=nil) or (AnEntryTemplate is TGenericWatchResultDataStruct)');
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate,
    AnOverrideTemplate, ANestLvl);

  for i := 0 to Length(FFieldsStorage) - 1 do begin
    if FFieldsStorage[i] <> nil then begin
      t := AnEntryTemplate;
      if t <> nil then
        t := TGenericWatchResultDataStruct(t).FType.FFieldData[i].Field;
      FFieldsStorage[i].LoadDataFromXMLConfig(AConfig, APath+'F'+IntToStr(i)+'/', t, FOverrideTempl[i], ANestLvl);
    end;
  end;
end;

procedure TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.SaveDataToXMLConfig
  (const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer);
var
  i: Integer;
begin
  inherited SaveDataToXMLConfig(AConfig, APath, ANestLvl);

  for i := 0 to Length(FFieldsStorage) - 1 do
    if FFieldsStorage[i] <> nil then
      FFieldsStorage[i].SaveDataToXMLConfig(AConfig, APath+'F'+IntToStr(i)+'/', ANestLvl);
end;

{ TGenericWatchResultDataStruct }

class function TGenericWatchResultDataStruct.GetStorageClass: TWatchResultStorageClass;
begin
  Result := TNestedFieldsWatchResultStorage;
end;

function TGenericWatchResultDataStruct.CreateStorage: TWatchResultStorage;
var
  Store: TNestedFieldsWatchResultStorage absolute Result;
  i: Integer;
begin
  Result := inherited CreateStorage;

  Store.StoredFieldCount := Length(FType.FFieldData);
  for i := 0 to Length(FType.FFieldData) - 1 do
    if FType.FFieldData[i].Field <> nil then
      Store.NestedStorage[i] := FType.FFieldData[i].Field.CreateStorage;
end;

function TGenericWatchResultDataStruct.MaybeUpdateProto(
  var AProtoData: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AStorage: PWatchResultStorage;
  ARecurse: boolean; ASkipStorage: boolean): boolean;
var
  AStructProtoData: TGenericWatchResultDataStruct absolute AProtoData;
  FieldStore: PNestedFieldsWatchResultStorage absolute AStorage;
  dummy: TOverrideTemplateData;
  i: Integer;
begin
  Result := inherited MaybeUpdateProto(AProtoData, AnOverrideTemplate, AStorage, ARecurse, ASkipStorage);
  if Result or not ARecurse then
    exit;

  if (Length(AStructProtoData.FType.FFieldData) = 0) and
     (Length(FType.FFieldData) > 0)
  then
    AStructProtoData.FType.CopyFieldsProtoFrom(FType);
  assert((Length(FType.FFieldData)=0) or (Length(FType.FFieldData)=Length(AStructProtoData.FType.FFieldData)), 'TGenericWatchResultDataStruct.MaybeUpdateProto: (Length(FType.FFieldData)=0) or (Length(FType.FFieldData)=Length(AStructProtoData.FType.FFieldData))');


  assert((AStorage=nil) or (AStorage^=nil) or (AStorage^ is TNestedFieldsWatchResultStorage), 'TGenericWatchResultDataStruct.MaybeUpdateProto: (AStorage=nil) or (AStorage^=nil) or (AStorage^ is TNestedFieldsWatchResultStorage)');
  if (AStorage = nil) or (AStorage^ = nil)
  then begin
    if (ARecurse) then begin  // or "if not ASkipStorage and " ??
      dummy := nil;
      for i := 0 to Length(FType.FFieldData) - 1 do begin
        if FType.FFieldData[i].Field <> nil then begin
          FType.FFieldData[i].Field.MaybeUpdateProto(AStructProtoData.FType.FFieldData[i].Field,
            dummy, nil, ARecurse, ASkipStorage);
          assert(dummy = nil, 'TGenericWatchResultDataStruct.MaybeUpdateProto: dummy = nil');
        end;
      end;
    end;
  end

  else begin
    if (FieldStore^.StoredFieldCount = 0) then
      FieldStore^.StoredFieldCount := Length(FType.FFieldData);
    assert((length(FType.FFieldData)=0) or (FieldStore^.StoredFieldCount = Length(FType.FFieldData)), 'TGenericWatchResultDataStruct.MaybeUpdateProto: (length(FType.FFieldData)=0) or (FieldStore^.StoredFieldCount = Length(FType.FFieldData))');

    for i := 0 to Length(FType.FFieldData) - 1 do begin
      if FType.FFieldData[i].Field <> nil then begin
        FType.FFieldData[i].Field.MaybeUpdateProto(AStructProtoData.FType.FFieldData[i].Field,
          FieldStore^.FOverrideTempl[i], FieldStore^.NestedStoragePtr[i], ARecurse, ASkipStorage);
      end;
    end;
  end;
end;

procedure TGenericWatchResultDataStruct.AfterSaveToIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  var AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
var
  AStructEntryTemplate: TGenericWatchResultDataStruct absolute AnEntryTemplate;
  AStore: TNestedFieldsWatchResultStorage absolute AStorage;
  i: Integer;
begin
  inherited AfterSaveToIndex(AStorage, AnIndex, AnEntryTemplate, AnOverrideTemplate);

  if Length(FType.FFieldData) > AStore.StoredFieldCount then begin
    assert(AStore.StoredFieldCount=0, 'TGenericWatchResultDataStruct.AfterSaveToIndex: AStore.StoredFieldCount=0');
    AStore.StoredFieldCount := Length(FType.FFieldData);
    assert(length(AStructEntryTemplate.FType.FFieldData) = 0, 'TGenericWatchResultDataStruct.AfterSaveToIndex: length(AStructEntryTemplate.FType.FFieldData) = 0');
    AStructEntryTemplate.FType.FFieldData := FType.FFieldData;
    SetLength(AStructEntryTemplate.FType.FFieldData, Length(AStructEntryTemplate.FType.FFieldData));
    for i := 0 to Length(AStructEntryTemplate.FType.FFieldData) - 1 do
      AStructEntryTemplate.FType.FFieldData[i].Field := nil;
  end;
  for i := 0 to Length(FType.FFieldData) - 1 do begin
    if FType.FFieldData[i].Field <> nil then begin
      FType.FFieldData[i].Field.MaybeUpdateProto(
        AStructEntryTemplate.FType.FFieldData[i].Field,
        AStore.FOverrideTempl[i],
        AStore.NestedStoragePtr[i]
      );

      if (AStore.NestedStorage[i] = nil) then
        AStore.NestedStorage[i] := FType.FFieldData[i].Field.CreateStorage;

      AStore.NestedStorage[i].SaveToIndex(AnIndex,
        FType.FFieldData[i].Field,
        AStructEntryTemplate.FType.FFieldData[i].Field,
        AStore.FOverrideTempl[i]);
    end;
  end;
end;

procedure TGenericWatchResultDataStruct.AfterLoadFromIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
var
  AStore: TNestedFieldsWatchResultStorage absolute AStorage;
  i: Integer;
begin
  inherited AfterLoadFromIndex(AStorage, AnIndex, AnEntryTemplate,
    AnOverrideTemplate);

  if AStore.StoredFieldCount = 0 then begin
    SetLength(FCurrentFields, 0);
  end
  else begin
    assert(AStore.StoredFieldCount = Length(FType.FFieldData), 'TGenericWatchResultDataStruct.AfterLoadFromIndex: AStore.StoredFieldCount = Length(FType.FFieldData)');
    SetLength(FCurrentFields, Length(FType.FFieldData));
    if AStore.NestedStorage[0].Count = 0 then begin
      for i := 0 to Length(FCurrentFields) - 1 do
        FCurrentFields[i] := nil;
    end
    else begin
      for i := 0 to Length(FType.FFieldData) - 1 do begin
        if (AStore.NestedStorage[i] <> nil) then begin
          AStore.NestedStorage[i].LoadFromIndex(AnIndex, FCurrentFields[i], FType.FFieldData[i].Field, AStore.FOverrideTempl[i]);
        end
      end;
    end;
  end;
end;

procedure TGenericWatchResultDataStruct.ClearData;
var
  i: Integer;
begin
  inherited ClearData;

  for i := 0 to Length(FType.FFieldData) - 1 do
    if (FType.FFieldData[i].Field <> nil) then
      FType.FFieldData[i].Field.ClearData;
end;

function TGenericWatchResultDataStruct.GetFields(AnIndex: Integer
  ): TWatchResultDataFieldInfo;
begin
  Result := inherited GetFields(AnIndex);
  if (AnIndex < Length(FCurrentFields)) and (FCurrentFields[AnIndex] <> nil) then
    Result.Field := FCurrentFields[AnIndex];
end;

function TGenericWatchResultDataStruct.GetEnumerator: TWatchResultDataEnumerator;
begin
  Result := TWatchResultDataStructEnumerator.Create(Self);
end;

procedure TGenericWatchResultDataStruct.SetFieldCount(ACount: integer);
begin
  SetLength(FType.FFieldData, ACount);
  // DoFree for unused fields
end;

procedure TGenericWatchResultDataStruct.SetField(AnIndex: Integer;
  AFieldName: String; AVisibility: TLzDbgFieldVisibility;
  AFlags: TLzDbgFieldFlags; AData: TWatchResultData);
begin
  FType.FFieldData[AnIndex].FieldName := AFieldName;
  FType.FFieldData[AnIndex].FieldVisibility := AVisibility;
  FType.FFieldData[AnIndex].FieldFlags := AFlags;
  FType.FFieldData[AnIndex].Field := AData;
end;

function TGenericWatchResultDataStruct.AddField(AFieldName: String;
  AVisibility: TLzDbgFieldVisibility; AFlags: TLzDbgFieldFlags;
  AData: TWatchResultData): Integer;
begin
  Result := Length(FType.FFieldData);
  SetLength(FType.FFieldData, Result + 1);
  FType.FFieldData[Result].FieldName := AFieldName;
  FType.FFieldData[Result].FieldVisibility := AVisibility;
  FType.FFieldData[Result].FieldFlags := AFlags;
  FType.FFieldData[Result].Field := AData;
end;

procedure TGenericWatchResultDataStruct.SetFieldData(AnIndex: Integer;
  AData: TWatchResultData);
begin
  if AnIndex >= Length(FType.FFieldData) then
    SetLength(FType.FFieldData, AnIndex+1);
  FType.FFieldData[AnIndex].Field := AData;
end;

{ TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage }

procedure TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.SetCount
  (AValue: integer);
begin
  inherited SetCount(AValue);
  if FAnchestorStorage <> nil then
    FAnchestorStorage.SetCount(AValue);
end;

procedure TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.Assign
  (ASource: TWatchResultStorage);
var
  Src: TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage absolute ASource;
begin
  inherited Assign(ASource);

  if ASource is TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage then begin
    FAnchestorStorage := Src.FAnchestorStorage.CreateCopy;
  end;
end;

function TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.GetNestedStorage
  (AnIndex: Integer): TWatchResultStorage;
begin
  if AnIndex < 0 then
    Result := FAnchestorStorage
  else
    Result := inherited GetNestedStorage(AnIndex);
end;

function TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.GetNestedStoragePtr
  (AnIndex: Integer): PWatchResultStorage;
begin
  if AnIndex < 0 then
    Result := @FAnchestorStorage
  else
    Result := inherited GetNestedStoragePtr(AnIndex);
end;

procedure TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.SetNestedStorage
  (AnIndex: Integer; AValue: TWatchResultStorage);
begin
  if AnIndex < 0 then begin
    assert((FAnchestorStorage=nil) or (AValue=nil), 'TGenericWatchResultDataStruct.TNestedFieldsWatchResultStorage.SetNestedStorage: (FAnchestorStorage=nil) or (AValue=nil)');
    FAnchestorStorage := AValue;

    if AValue <> nil then
      AValue.Count := Count;
  end
  else
    inherited SetNestedStorage(AnIndex, AValue);
end;

destructor TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.Destroy;
begin
  FreeAndNil(FAnchestorStorage);
  inherited Destroy;
end;

procedure TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.LoadDataFromXMLConfig
  (const AConfig: TXMLConfig; const APath: string;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; ANestLvl: Integer);
var
  t: TWatchResultData;
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AnEntryTemplate,
    AnOverrideTemplate, ANestLvl);

  if FAnchestorStorage <> nil then begin
    t := AnEntryTemplate;
    if t <> nil then
      t := TGenericWatchResultDataStructWithAnchestor(t).FType.FAnchestor;

    FAnchestorStorage.LoadDataFromXMLConfig(AConfig, APath+'Anch/', t, AnOverrideTemplate, ANestLvl);
  end;
end;

procedure TGenericWatchResultDataStructWithAnchestor.TNestedFieldsAndAnchestorWatchResultStorage.SaveDataToXMLConfig
  (const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, ANestLvl);

  if FAnchestorStorage <> nil then
    FAnchestorStorage.SaveDataToXMLConfig(AConfig, APath+'Anch/', ANestLvl);
end;

{ TGenericWatchResultDataStructWithAnchestor }

class function TGenericWatchResultDataStructWithAnchestor.GetStorageClass: TWatchResultStorageClass;
begin
  Result := TNestedFieldsAndAnchestorWatchResultStorage;
end;

function TGenericWatchResultDataStructWithAnchestor.CreateStorage: TWatchResultStorage;
var
  Store: TNestedFieldsAndAnchestorWatchResultStorage absolute Result;
begin
  Result := inherited CreateStorage;

  if FType.FAnchestor <> nil then
    Store.NestedStorage[-1] := FType.FAnchestor.CreateStorage;
end;

function TGenericWatchResultDataStructWithAnchestor.MaybeUpdateProto(
  var AProtoData: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData; AStorage: PWatchResultStorage;
  ARecurse: boolean; ASkipStorage: boolean): boolean;
var
  AStructProtoData: TGenericWatchResultDataStructWithAnchestor absolute AProtoData;
  FieldStore: PNestedFieldsAndAnchestorWatchResultStorage absolute AStorage;
  dummy: TOverrideTemplateData;
begin
  Result := inherited MaybeUpdateProto(AProtoData, AnOverrideTemplate, AStorage, ARecurse, ASkipStorage);
  if Result or not ARecurse then
    exit;

  if (AStructProtoData.FType.FAnchestor = nil) and
     (FType.FAnchestor <> nil)
  then
    AStructProtoData.FType.FAnchestor := FType.FAnchestor.CreateCopy(True);


  assert((AStorage=nil) or (AStorage^=nil) or (AStorage^ is TNestedFieldsAndAnchestorWatchResultStorage), 'TGenericWatchResultDataStruct.MaybeUpdateProto: (AStorage=nil) or (AStorage^=nil) or (AStorage^ is TNestedFieldsWatchResultStorage)');
  if (AStorage = nil) or (AStorage^ = nil)
  then begin
    if (ARecurse) then begin  // or "if not ASkipStorage and " ??
      dummy := nil;
      if FType.FAnchestor <> nil then begin
        FType.FAnchestor.MaybeUpdateProto(AStructProtoData.FType.FAnchestor, dummy,
          nil, ARecurse, ASkipStorage);
        assert(dummy = nil, 'TGenericWatchResultDataStructWithAnchestor.MaybeUpdateProto: dummy = nil');
      end;
    end;
  end

  else begin
    if FType.FAnchestor <> nil then begin
      FType.FAnchestor.MaybeUpdateProto(AStructProtoData.FType.FAnchestor, AnOverrideTemplate,
        FieldStore^.NestedStoragePtr[-1], ARecurse, ASkipStorage);
    end;
  end;
end;

procedure TGenericWatchResultDataStructWithAnchestor.AfterSaveToIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  var AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
var
  AStructEntryTemplate: TGenericWatchResultDataStructWithAnchestor absolute AnEntryTemplate;
  AStore: TNestedFieldsAndAnchestorWatchResultStorage absolute AStorage;
begin
  inherited AfterSaveToIndex(AStorage, AnIndex, AnEntryTemplate,
    AnOverrideTemplate);

  if (FType.FAnchestor <> nil) then begin
    assert(AnEntryTemplate is TGenericWatchResultDataStructWithAnchestor, 'TGenericWatchResultDataStructWithAnchestor.AfterSaveToIndex: AnEntryTemplate is TGenericWatchResultDataStruct');

    FType.FAnchestor.MaybeUpdateProto(
      AStructEntryTemplate.FType.FAnchestor,
      AnOverrideTemplate,
      AStore.NestedStoragePtr[-1]
    );

    if (AStore.NestedStorage[-1] = nil) then
      AStore.NestedStorage[-1] := FType.FAnchestor.CreateStorage;

    AStore.NestedStorage[-1].SaveToIndex(AnIndex,
      FType.FAnchestor,
      AStructEntryTemplate.FType.FAnchestor,
      AnOverrideTemplate);
  end;
end;

procedure TGenericWatchResultDataStructWithAnchestor.AfterLoadFromIndex(
  AStorage: TWatchResultStorage; AnIndex: Integer;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
var
  AStore: TNestedFieldsAndAnchestorWatchResultStorage absolute AStorage;
begin
  inherited AfterLoadFromIndex(AStorage, AnIndex, AnEntryTemplate,
    AnOverrideTemplate);

  if (FType.FAnchestor <> nil) then begin
    if (AStore.NestedStorage[-1] <> nil) then
      AStore.NestedStorage[-1].LoadFromIndex(AnIndex, FCurrentAnchestor, FType.FAnchestor, AnOverrideTemplate);
  end;
end;

procedure TGenericWatchResultDataStructWithAnchestor.ClearData;
begin
  inherited ClearData;

  if FType.FAnchestor <> nil then
    FType.FAnchestor.ClearData;
end;

function TGenericWatchResultDataStructWithAnchestor.GetAnchestor: TWatchResultData;
begin
  if FCurrentAnchestor <> nil then
    Result := FCurrentAnchestor
  else
    Result := inherited GetAnchestor;
end;

procedure TGenericWatchResultDataStructWithAnchestor.SetAnchestor(
  AnAnchestor: TWatchResultData);
begin
  FType.FAnchestor := AnAnchestor;
end;

{ TWatchResultDataStruct }

function TWatchResultDataStruct.GetClassID: TWatchResultDataClassID;
begin
  Result := wdStruct;
end;

constructor TWatchResultDataStruct.Create(AStructType: TLzDbgStructType);
begin
  FType.FStructType := AStructType;
end;

{ TWatchResultDataRefStruct }

function TWatchResultDataRefStruct.GetClassID: TWatchResultDataClassID;
begin
  Result := wdStructRef;
end;

function TWatchResultDataRefStruct.GetHasDataAddress: Boolean;
begin
  Result := True;
end;

constructor TWatchResultDataRefStruct.Create(AStructType: TLzDbgStructType;
  ADataAddress: TDBGPtr);
begin
  FData.FDataAddress := ADataAddress;
  FType.FStructType := AStructType;
end;

{ TWatchResultDataConverted }

function TWatchResultDataConverted.GetClassID: TWatchResultDataClassID;
begin
  Result := wdConverted;
end;

function TWatchResultDataConverted.GetBackendValueHandler: ILazDbgValueConverterIntf;
begin
  Result := FType.FHandler;
end;

function TWatchResultDataConverted.GetConvertedRes: TWatchResultData;
begin
  if (FieldCount > 0) then
    Result := Fields[0].Field;

  if (FieldCount > 1) and
     ( (Fields[0].Field = nil) or
       (Fields[0].Field.ValueKind = rdkError)
     )
  then
    Result := Fields[1].Field;
end;

constructor TWatchResultDataConverted.Create(
  AHandler: ILazDbgValueConverterIntf);
begin
  assert((FType.FHandler=nil) or (FType.FHandler=AHandler) or (AHandler=nil), 'TWatchResultDataConverted.Create: (FType.FHandler=nil) or (FType.FHandler=AHandler) or (AHandler=nil)');
  if (AHandler <> FType.FHandler) and (AHandler <> nil) then begin
    if FType.FHandler <> nil then
      FType.FHandler.ReleaseReference;
    FType.FHandler := AHandler;
    if AHandler <> nil then
      AHandler.AddReference;
  end;
end;

{ TGenericWatchResultDataProc }

function TGenericWatchResultDataProc.GetAsString: String;
begin
  Result := FType.FLoc;
end;

function TGenericWatchResultDataProc.GetAsDesc: String;
begin
  Result := FType.FText;
end;

constructor TGenericWatchResultDataProc.Create(AnAddr: QWord; ALoc,
  ADesc: String);
begin
  FType.FText := ADesc;
  FType.FLoc := ALoc;
  FData.FNumValue := AnAddr;
  inherited Create();
end;

{ TWatchResultDataFunc }

function TWatchResultDataFunc.GetClassID: TWatchResultDataClassID;
begin
  Result := wdFunc;
end;

{ TWatchResultDataProc }

function TWatchResultDataProc.GetClassID: TWatchResultDataClassID;
begin
  Result := wdProc;
end;

{ TWatchResultDataFuncRef }

function TWatchResultDataFuncRef.GetClassID: TWatchResultDataClassID;
begin
  Result := wdFuncRef;
end;

{ TWatchResultDataProcRef }

function TWatchResultDataProcRef.GetClassID: TWatchResultDataClassID;
begin
  Result := wdProcRef;
end;

{ TWatchResultDataError.TErrorDataStorage }

procedure TWatchResultDataError.TErrorDataStorage.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; ANestLvl: Integer);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, ANestLvl);
  AConfig.SetDeleteValue(APath+TAG_ALL_ERR, ANestLvl, -1);
end;

procedure TWatchResultDataError.TErrorDataStorage.LoadFromIndex(
  AnIndex: Integer; out AData: TWatchResultData;
  const AnEntryTemplate: TWatchResultData;
  var AnOverrideTemplate: TOverrideTemplateData);
begin
  if AnEntryTemplate.ClassType <> TWatchResultDataError then begin
    if AnOverrideTemplate = nil then
      AnOverrideTemplate := TGenericWatchResultStorage._ERROR_CLASS.CreateEmpty;
    inherited LoadFromIndex(AnIndex, AData, AnOverrideTemplate, AnOverrideTemplate)
  end
  else
    inherited LoadFromIndex(AnIndex, AData, AnEntryTemplate, AnOverrideTemplate);
end;

{ TWatchResultDataError }

function TWatchResultDataError.GetClassID: TWatchResultDataClassID;
begin
  Result := wdErr;
end;

class function TWatchResultDataError.GetStorageClass: TWatchResultStorageClass;
begin
  Result := TErrorDataStorage;
end;

constructor TWatchResultDataError.Create(APrintedVal: String);
begin
  inherited Create;
  FData.FText := APrintedVal;
end;

end.


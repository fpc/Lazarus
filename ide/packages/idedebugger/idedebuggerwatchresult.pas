unit IdeDebuggerWatchResult;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, Types, IdeDebuggerUtils, LazDebuggerIntf,
  LazDebuggerIntfBaseTypes, LazUTF8, Laz2_XMLCfg, StrUtils;

type

  TWatchResultDataKind = (
    rdkUnknown,
    rdkError, rdkPrePrinted,
    rdkString, rdkWideString, rdkChar,
    rdkSignedNumVal, rdkUnsignedNumVal, rdkPointerVal, rdkFloatVal,
    rdkBool, rdkEnum, rdkEnumVal, rdkSet,
    rdkPCharOrString
  );

  TWatchResultData = class;
  TWatchResultDataError = class;
  TWatchResultStorage = class;
  TOverrideTemplateData = TWatchResultDataError;

  { TWatchResultValue }

  TWatchResultValue = object
  protected
    function GetAsString: String; inline;
    function GetAsWideString: WideString; inline;
    function GetAsQWord: QWord; inline;
    function GetAsInt64: Int64; inline;
    function GetAsFloat: Extended; inline;
    function GetByteSize: Integer; inline;                         // Int, Enum
    function GetFloatPrecission: TLzDbgFloatPrecission; inline;
    function GetCount: Integer; inline;                            // Set (Active Elements)
    function GetElementName(AnIndex: integer): String; inline;     // Set
    function GetDerefData: TWatchResultData; inline;               // Ptr

    function GetEntryTemplate: TWatchResultData; inline; // NESTED TYPE FOR NESTED STORAGE
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
  end;

  { TWatchResultValueWideString }

  TWatchResultValueWideString = object(TWatchResultValue)
  protected const
    VKind = rdkWideString;
  private
    FWideText: WideString;
  protected
    property GetAsWideString: WideString read FWideText;
    function GetAsString: String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
                                    const AnEntryTemplate: TWatchResultData;
                                    var AnOverrideTemplate: TOverrideTemplateData;
                                    AnAsProto: Boolean);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
  end;

  { TWatchResultValueOrdNumBase }

  TWatchResultValueOrdNumBase = object(TWatchResultValue)
  private
    FNumValue: QWord;
  protected
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
    function GetAsString: String; inline;
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
    wdPChrStr,   // TWatchResultDataPCharOrString
    wdErr        // TWatchResultDataError
  );

  { TWatchResultData }

  TWatchResultData = class // (TRefCountedObject)
  private
    FTypeName: String;
  //  ValidData: TWatchValueDataFlags;
  //  Addr: TDbgPtr;
  // MemDump
    function GetClassID: TWatchResultDataClassID; virtual; //abstract;
  protected
    class function GetStorageClass: TWatchResultStorageClass; virtual; abstract;
    function CreateStorage: TWatchResultStorage; virtual; abstract;
    function MaybeUpdateProto(var AProtoData: TWatchResultData;
                              var AnOverrideTemplate: TOverrideTemplateData;
                              AStorage: PWatchResultStorage;
                              ARecurse: boolean = False
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
    function GetAsString: String; virtual; abstract;
    function GetAsWideString: WideString; virtual; abstract;
    function GetAsQWord: QWord; virtual; abstract;
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsFloat: Extended; virtual; abstract;
    function GetByteSize: Integer; virtual; abstract;
    function GetFloatPrecission: TLzDbgFloatPrecission; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetElementName(AnIndex: integer): String; virtual; abstract;
    function GetDerefData: TWatchResultData; virtual; abstract;
    function GetNestedType: TWatchResultData; virtual; abstract;

    function GetSelectedEntry: TWatchResultData;  virtual; abstract;
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

  public
    property ValueKind: TWatchResultDataKind read GetValueKind;
    property TypeName: String read FTypeName;

    property AsString: String read GetAsString;
    property AsWideString: WideString read GetAsWideString;
    property AsQWord: QWord read GetAsQWord;
    property AsInt64: Int64 read GetAsInt64;
    property AsFloat: Extended read GetAsFloat;

    property ByteSize: Integer read GetByteSize;
    property FloatPrecission: TLzDbgFloatPrecission read GetFloatPrecission;
    property DerefData: TWatchResultData read GetDerefData;
    property NestedType: TWatchResultData read GetNestedType; // NESTED TYPE FOR NESTED STORAGE

    procedure SetSelectedIndex(AnIndex: Integer); virtual;
    property SelectedEntry: TWatchResultData read GetSelectedEntry;
  end;

  TWatchResultDataClass = class of TWatchResultData;

  { TWatchResultDataEx - Declare Setters for TCurrentResData }

  TWatchResultDataEx = class(TWatchResultData)
  public
    procedure SetTypeName(ATypeName: String);
    procedure SetDerefData(ADerefData: TWatchResultData); virtual;

    procedure SetEntryPrototype(AnEntry: TWatchResultData); virtual;
    procedure WriteEntryToStorage(AnIndex: Integer); virtual;
    procedure WriteValueToStorage(AnIndex: Integer; AValue: TWatchResultData); virtual;
    procedure SetEntryCount(ACount: Integer); virtual;
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
                              ARecurse: boolean = False
                             ): boolean; override;
    procedure ClearData; override;
  protected
    function GetValueKind: TWatchResultDataKind; override;
    function GetAsString: String; override;
    function GetAsWideString: WideString; override;
    function GetAsQWord: QWord; override;
    function GetAsInt64: Int64; override;
    function GetAsFloat: Extended; override;

    function GetByteSize: Integer; override;
    function GetFloatPrecission: TLzDbgFloatPrecission; override;
    function GetCount: Integer; override;
    function GetElementName(AnIndex: integer): String; override;
    function GetDerefData: TWatchResultData; override;
    function GetNestedType: TWatchResultData; override;

    function GetSelectedEntry: TWatchResultData; override;
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
    function GetSelectedEntry: TWatchResultData; override;
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
  public
    constructor Create(AStringVal: String);
  end;

  { TWatchResultDataWideString }

  TWatchResultDataWideString = class(specialize TGenericWatchResultData<TWatchResultValueWideString>)
  private
    function GetClassID: TWatchResultDataClassID; override;
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
    function GetAsString: String; override;
    function GetDerefData: TWatchResultData; override;
    class function GetStorageClass: TWatchResultStorageClass; override;
    function CreateStorage: TWatchResultStorage; override;
    function MaybeUpdateProto(var AProtoData: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData;
      AStorage: PWatchResultStorage; ARecurse: boolean = False): boolean;
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

  { TWatchResultDataArrayBase }

  generic TWatchResultDataArrayBase<_DATA, _TYPE> = class(specialize TGenericWatchResultDataWithType<_DATA, _TYPE>)
  private
    FCurrentElement: TWatchResultData;
    FOverrideTemplateData: TOverrideTemplateData;
  public
    procedure SetEntryPrototype(AnEntry: TWatchResultData); override;
    procedure WriteValueToStorage(AnIndex: Integer; AValue: TWatchResultData); override;
    procedure SetEntryCount(ACount: Integer); override;

    function MaybeUpdateProto(var AProtoData: TWatchResultData;
      var AnOverrideTemplate: TOverrideTemplateData;
      AStorage: PWatchResultStorage; ARecurse: boolean = False): boolean;
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
  public
    procedure SetEntryPrototype(AnEntry: TWatchResultData); override;
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

function PrintWatchValue(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat): String;

function dbgs(AResKind: TWatchResultDataKind): String; overload;

implementation

function PrintWatchValueEx(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;

  function PrintNumber(ANumValue: TWatchResultData; AnIsPointer: Boolean; ADispFormat: TWatchDisplayFormat): String;
  var
    num: QWord;
    n, i, j: Integer;
  begin
    case ADispFormat of
      //wdfString: // get pchar(num)^ ?
      wdfChar: begin
        num := ANumValue.AsQWord;
        Result := '';
        while num <> 0 do begin
          Result := chr(num and $ff) + Result;
          num := num >> 8;
        end;
        if Result <> '' then begin
          i := 1;
          while i <= length(Result) do begin
            j := UTF8CodepointStrictSize(@Result[i]);
            if j = 0 then begin
              Result := copy(Result, 1, i-1) + '''#$'+ IntToHex(byte(Result[i]), 2) + '''' + copy(Result, i + 6, 99);
              inc(i, 6);
            end
            else
              inc(i, j);
          end;
          Result := '''' + Result + '''';
        end
        else
          Result := '#$00';
      end;
      wdfUnsigned: begin
        Result := IntToStr(ANumValue.AsQWord)
      end;
      wdfHex: begin
        n := HexDigicCount(ANumValue.AsQWord, ANumValue.ByteSize, AnIsPointer);
        Result := '$'+IntToHex(ANumValue.AsQWord, n);
      end;
      wdfBinary: begin
        n := HexDigicCount(ANumValue.AsQWord, ANumValue.ByteSize, AnIsPointer);
        Result := '%'+IntToBin(Int64(ANumValue.AsQWord), n*4); // Don't get any extra leading 1
      end;
      wdfPointer: begin
        n := HexDigicCount(ANumValue.AsQWord, ANumValue.ByteSize, True);
        Result := '$'+IntToHex(ANumValue.AsQWord, n);
      end;
      else begin // wdfDecimal
        Result := IntToStr(ANumValue.AsInt64);
      end;
    end;
  end;

  function PrintChar: String;
  begin
    if ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary] then begin
      Result := '#' + PrintNumber(AResValue, False, ADispFormat);
      exit;
    end;
    case AResValue.ByteSize of
       //1: Result := QuoteText(SysToUTF8(char(Byte(AResValue.AsQWord))));
       1: Result := QuoteText(char(Byte(AResValue.AsQWord)));
       2: Result := QuoteWideText(WideChar(Word(AResValue.AsQWord)));
       else Result := '#' + PrintNumber(AResValue, False, wdfDecimal);
    end;
  end;

  function PrintBool: String;
  var
    c: QWord;
  begin
    c := AResValue.GetAsQWord;
    if c = 0 then
      Result := 'False'
    else
      Result := 'True';

    if (ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary]) then
      Result := Result + '(' + PrintNumber(AResValue, False, ADispFormat) + ')'
    else
    if (c > 1) then
      Result := Result + '(' + PrintNumber(AResValue, False, wdfDecimal) + ')';
  end;

  function PrintEnum: String;
  begin
    if (ADispFormat = wdfDefault) and (AResValue.ValueKind = rdkEnumVal) then
      ADispFormat := wdfStructure;
    case ADispFormat of
      wdfStructure:
        Result := AResValue.AsString + ' = ' +  PrintNumber(AResValue, False, wdfDecimal);
      wdfUnsigned,
      wdfDecimal,
      wdfHex,
      wdfBinary:
        Result := PrintNumber(AResValue, False, ADispFormat);
      else
        Result := AResValue.AsString;
    end;
  end;

  function PrintSet: String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to AResValue.GetCount - 1 do
      Result := Result + ',' + AResValue.GetElementName(i);
    if Result = '' then
      Result := '[]'
    else begin
      Result[1] := '[';
      Result := Result + ']'
    end;
  end;

var
  PointerValue: TWatchResultDataPointer absolute AResValue;
  ResTypeName: String;
  PtrDeref: TWatchResultData;
begin
  inc(ANestLvl);
  Result := '';
  case AResValue.ValueKind of
    rdkError:
      Result := 'Error: ' + AResValue.AsString;
    rdkUnknown:
      Result := 'Error: Unknown';
    rdkPrePrinted: begin
      Result := AResValue.AsString;
    end;
    rdkSignedNumVal,
    rdkUnsignedNumVal: begin
      if (ADispFormat = wdfPointer) and (AResValue.AsQWord = 0) then begin
        Result := 'nil';
      end
      else begin
        if (AResValue.ValueKind = rdkUnsignedNumVal) and (ADispFormat = wdfDecimal) then
          ADispFormat := wdfUnsigned
        else
        if not (ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary, wdfPointer]) then begin
          //wdfDefault, wdfStructure, wdfChar, wdfString, wdfFloat
          if AResValue.ValueKind = rdkUnsignedNumVal then
            ADispFormat := wdfUnsigned
          else
            ADispFormat := wdfDecimal;
        end;

        Result := PrintNumber(AResValue, False, ADispFormat);
      end;
    end;
    rdkPointerVal: begin
      ResTypeName := '';
      if (ADispFormat = wdfStructure) or
         ((ADispFormat = wdfDefault) and (PointerValue.DerefData = nil))
      then
        ResTypeName := AResValue.TypeName;

      if (ADispFormat in [wdfDefault, wdfStructure, wdfPointer]) and (AResValue.AsQWord = 0)
      then begin
        Result := 'nil';
      end
      else begin
        if not (ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary, wdfPointer]) then
          //wdfDefault, wdfStructure, wdfChar, wdfString, wdfFloat
          ADispFormat := wdfPointer;

        Result := PrintNumber(AResValue, True, ADispFormat);
      end;

      if ResTypeName <> '' then
        Result := ResTypeName + '(' + Result + ')';

      PtrDeref :=  PointerValue.DerefData;
      if PtrDeref <> nil then begin
        while (PtrDeref.ValueKind = rdkPointerVal) and (PtrDeref.DerefData <> nil) do begin
          Result := Result + '^';
          PtrDeref :=  PtrDeref.DerefData;
        end;
        Result := Result + '^: ' + PrintWatchValueEx(PointerValue.DerefData, wdfDefault, ANestLvl);
      end;
    end;
    rdkFloatVal: begin
      case AResValue.FloatPrecission of
        dfpSingle:   Result := FloatToStrF(AResValue.AsFloat, ffGeneral,  8, 0);
        dfpDouble:   Result := FloatToStrF(AResValue.AsFloat, ffGeneral, 12, 0);
        dfpExtended: Result := FloatToStrF(AResValue.AsFloat, ffGeneral, 15, 0);
      end;
    end;
    rdkChar:       Result := PrintChar;
    rdkString:     Result := QuoteText(AResValue.AsString);
    rdkWideString: Result := QuoteWideText(AResValue.AsWideString);
    rdkBool:       Result := PrintBool;
    rdkEnum, rdkEnumVal:
                   Result := PrintEnum;
    rdkSet:        Result := PrintSet;
    rdkPCharOrString: begin
      AResValue.SetSelectedIndex(0); // pchar res
      Result := 'PChar: ' + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl);
      AResValue.SetSelectedIndex(1); // string res
      Result := Result + LineEnding
              + 'String: ' + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl);
    end;
  end;
end;

function PrintWatchValue(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat): String;
begin
  Result := PrintWatchValueEx(AResValue, ADispFormat, -1);
end;

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
    TWatchResultDataPCharOrString, // wdPChrStr
    TWatchResultDataError          // wdErr
  );

{ TWatchResultValue }

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

function TWatchResultValue.GetCount: Integer;
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

function TWatchResultValue.GetEntryTemplate: TWatchResultData;
begin
  Result := nil;
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

{ TWatchResultValueWideString }

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
end;

procedure TWatchResultValueWideString.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string; AnAsProto: Boolean);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AnAsProto);
  AConfig.SetValue(APath + 'Value', FWideText);
end;

{ TWatchResultValueOrdNumBase }

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

{ TWatchResultData }

function TWatchResultData.GetValueKind: TWatchResultDataKind;
begin
  Result := rdkUnknown;
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
    FErrorStore.Clear(FDataArray, AValue-1);
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
begin
  PathNst := '';
  if ANestLvl > 0 then PathNst := 'N'+IntToStr(ANestLvl);

  l := AConfig.GetValue(APath+TAG_CNT+PathNst, 0);
  SetLength(FDataArray, l);

  if ANestLvl > 0 then PathNst := PathNst+'/';
  for i := 0 to Length(FDataArray) - 1 do begin
    p := APath+'E'+IntToStr(i)+'/'+PathNst;

    if AConfig.GetValue(p+TAG_ERR, 0) = 1 then begin
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
  ARecurse: boolean): boolean;
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

    if (AStorage <> nil) and (AStorage^ <> nil) then
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

function TGenericWatchResultData.GetAsString: String;
begin
  Result := FData.GetAsString;
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

function TGenericWatchResultDataWithType.GetSelectedEntry: TWatchResultData;
begin
  Result := FType.GetEntryTemplate;
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
  ARecurse: boolean): boolean;
var
  APtrProtoData: TWatchResultDataPointer absolute AProtoData;
  Store: PWatchResultStorage;
begin
  Result := inherited MaybeUpdateProto(AProtoData, AnOverrideTemplate, AStorage, ARecurse);
  if (not Result) and (AProtoData is TWatchResultDataPointer) and
     ARecurse and (FType.FDerefData <> nil)
  then begin
    Store := AStorage;
    if (Store <> nil) and (Store^ <> nil) then
      Store := Store^.NestedStoragePtr;
    Result := FType.FDerefData.MaybeUpdateProto(APtrProtoData.FType.FDerefData, AnOverrideTemplate,
      Store, ARecurse);
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

{ TWatchResultDataArrayBase }

procedure TWatchResultDataArrayBase.SetEntryPrototype(AnEntry: TWatchResultData);
begin
assert(FType.FEntryTemplate=nil, 'TWatchResultDataArrayBase.SetEntryPrototype: FType.FEntryTemplate=nil');
//
//  AnEntry.MaybeUpdateProto(
//    FType.FEntryTemplate,
//    FOverrideTemplateData,
//    @FData.FEntries
//  );

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

// TODO: require count to be already set???
  if FData.FEntries = nil then begin
    assert((AnIndex=0) and (FType.FEntryTemplate<>nil), 'TWatchResultDataArrayBase.WriteEntryToStorage: (AnIndex=0) and (FType.FEntryTemplate<>nil)');
    SetEntryCount(1);
  end;

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
  ARecurse: boolean): boolean;
var
  AnArrayProtoData: TWatchResultDataArrayBase absolute AProtoData;
  Store: PWatchResultStorage;
begin
  Result := inherited MaybeUpdateProto(AProtoData, AnOverrideTemplate,
    AStorage, ARecurse);
  if (not Result) and (AProtoData is TWatchResultDataArrayBase) and
     ARecurse and (FType.FEntryTemplate <> nil)
  then begin
    Store := AStorage;
    if (Store <> nil) and (Store^ <> nil) then
      Store := Store^.NestedStoragePtr;
    Result := FType.FEntryTemplate.MaybeUpdateProto(AnArrayProtoData.FType.FEntryTemplate, AnOverrideTemplate,
      Store, ARecurse);
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
      nil,
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

procedure TWatchResultDataPCharOrString.SetEntryPrototype(
  AnEntry: TWatchResultData);
begin
  inherited SetEntryPrototype(AnEntry);
  FTypeName := AnEntry.FTypeName;
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


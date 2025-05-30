unit FpdMemoryTools;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{$HINT 5024 OFF}

(* Tools to read data from Target or Own memory.

   This is to deal in one place with all conversion of data from target format
   to debugger format.

   A typical read would involve the following steps:

   The code calls MemoryManager with an Address, a Size and a space for the data.

   If the data needs to be extended (SizeOf(data) > TargetSize), then
   MemConvertor is called to decide where in the provided space the read data
   should initially be stored.

   Then the data is read using MemReader.

   And finally MemConvertor is given a chance to extend or convert the data.

*)

interface

uses
  Classes, SysUtils, math, DbgIntfBaseTypes, DbgIntfDebuggerBase, FpErrorMessages, LazClasses,
  AVL_Tree, LazDebuggerUtils, LazDebuggerIntfFloatTypes,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif};

const
  MINIMUM_MEMREAD_LIMIT = 1024;

type

  TBitAddr = 0..63; // 0-7 for mem read // 0-63 for register read
  TBitSize = -7..7;

  TFpDbgMemLocationType = (
    mlfUninitialized := 0,   // like invalid, but not known // This (0) is the initial value
    mlfInvalid,
    mlfTargetMem,            // an address in the target (debuggee) process
    mlfSelfMem,              // an address in this(the debuggers) process memory; the data is  in TARGET format (endian, ...)
    // the below will be mapped (and extended) according to endianess
    mlfTargetRegister,       // reads from the register
    mlfConstant,             // an (up to) SizeOf(TDbgPtr) (=8) Bytes Value (endian in format of debug process)
    mlfConstantDeref // A constant that can be used instead of an address (location parser),
                     // If a value (e.g. literal numeric constant 0x1234) has no address,
                     //   then this is treated as its virtual address.
                     // If (and only if) the value is attempted to be derefed, then
                     //   it will yield the constant as the result of the deref.
                     // It can also be tested for nil. The virtual address is never nil.
                     // Any other access must result in an error.
                     // Used for PFoo(1234)^ or TObject(1234).Foo
  );

  TFpDbgValueSize = packed record
    Size: Int64;        // Also used for stride => can be negative
    BitSize: TBitSize;  // Must have the same sign as Size
  end;
  PFpDbgValueSize = ^TFpDbgValueSize;

  TDbgAddressClass = byte;

  { TFpDbgMemLocation }

  TFpDbgMemLocation = packed record
    Address: TDbgPtr;
    MType: TFpDbgMemLocationType;
    BitOffset: TBitAddr;
    AddressClass: TDbgAddressClass;  // Used by AVR. 0 = data (or unspecified), 1 = progmem, 2 = EEPROM
    class operator = (a, b: TFpDbgMemLocation): boolean; inline;

    // for sorting as key in lists
    class operator < (a, b: TFpDbgMemLocation): boolean; inline;
    class operator > (a, b: TFpDbgMemLocation): boolean; inline;
  end;
  PFpDbgMemLocation = ^TFpDbgMemLocation;

  TFpDbgMemManager = class;
  TFpDbgMemManagerFlag = (mmfPartialRead);
  TFpDbgMemManagerFlags = set of TFpDbgMemManagerFlag;

  TByteDynArray = array of Byte;

  TFpDbgMemModel = class;

  { TDbgRegisterValue }

  TDbgRegisterValue = class;
  TRegisterFormatterProc = function(AReg: TDbgRegisterValue; AFormat: TRegisterDisplayFormat = rdDefault): String;

  TDbgRegisterValue = class
  private
    FDwarfIdx: cardinal;
    FName: string;
    FNumValue: TDBGPtr;
    FSize: byte;
    FMem: Pointer;
    FStrValue: string;
    FFormatter: TRegisterFormatterProc;
    function GetStrFormatted(AFormat: TRegisterDisplayFormat): string;
    function GetStrValue: string;
  public
    constructor Create(const AName: String);
    destructor Destroy; override;
    procedure Assign(ASource: TDbgRegisterValue);
    function HasEqualVal(AnOther: TDbgRegisterValue): Boolean;
    procedure SetValue(ANumValue: Integer; const AStrValue: string; ASize: byte; ADwarfIdx: cardinal);
    procedure SetValue(ANumValue: Int64; const AStrValue: string; ASize: byte; ADwarfIdx: cardinal);
    procedure SetValue(ANumValue: Cardinal; const AStrValue: string; ASize: byte; ADwarfIdx: cardinal);
    procedure SetValue(ANumValue: TDBGPtr; const AStrValue: string; ASize: byte; ADwarfIdx: cardinal);
    procedure SetValue(const AData: Pointer; ASize: byte; ADwarfIdx: cardinal; AFormatter: TRegisterFormatterProc);
    procedure Setx86EFlagsValue(ANumValue: TDBGPtr);
    property Name: string read FName;
    property NumValue: TDBGPtr read FNumValue;
    property StrValue: string read GetStrValue;
    property StrFormatted[AFormat: TRegisterDisplayFormat]: string read GetStrFormatted;
    property Size: byte read FSize;
    property Data: Pointer read FMem;
    property DwarfIdx: cardinal read FDwarfIdx;
  end;

  { TFpDbgLocationContext }

  TFpDbgLocationContext = class;

  TGetFrameBaseCallback = function(AContext: TFpDbgLocationContext; out AnError: TFpError): TDBGPtr of object;

  TFpDbgLocationContext = class(TRefCountedObject)
  private
    FCfaFrameBaseError: TFpError;
    FFrameBaseCallback: TGetFrameBaseCallback;
    FCfaFrameBaseCallback: TGetFrameBaseCallback;
    FFrameBaseError: TFpError;
    FFrameBase, FCfaFrameBase: TDBGPtr;

    function GetFrameBase: TDBGPtr;
    function GetCfaFrameBase: TDBGPtr;
    function GetLastMemError: TFpError;
    function GetPartialReadResultLenght: QWord;
  protected
    function GetAddress: TDbgPtr; virtual; abstract;
    function GetStackFrame: Integer; virtual; abstract;
    function GetThreadId: Integer; virtual; abstract;
    function GetSizeOfAddress: Integer; virtual; abstract;
    function GetMemManager: TFpDbgMemManager; virtual; abstract;
    function GetMemModel: TFpDbgMemModel; virtual; abstract;
  public
    property Address: TDbgPtr read GetAddress;
    property ThreadId: Integer read GetThreadId;
    property StackFrame: Integer read GetStackFrame;
    property SizeOfAddress: Integer read GetSizeOfAddress;
    property MemManager: TFpDbgMemManager read GetMemManager;
    property MemModel: TFpDbgMemModel read GetMemModel;
  public
    procedure SetFrameBaseCallback(ACallback: TGetFrameBaseCallback);
    procedure SetCfaFrameBaseCallback(ACallback: TGetFrameBaseCallback);
    property FrameBase: TDBGPtr read GetFrameBase;        // as requested by DW_OP_fbreg
    property FrameBaseError: TFpError read FFrameBaseError;
    property CfaFrameBase: TDBGPtr read GetCfaFrameBase;  // as requested by DW_OP_call_frame_cfa
    property FCfarameBaseError: TFpError read FCfaFrameBaseError;
  public
    procedure ClearLastMemError;
    property LastMemError: TFpError read GetLastMemError;
    property PartialReadResultLenght: QWord read GetPartialReadResultLenght;
  public
    function ReadMemory(const ASourceLocation: TFpDbgMemLocation; const ASize: TFpDbgValueSize;
                        const ADest: Pointer; const AFlags: TFpDbgMemManagerFlags = []
                       ): Boolean; inline;
    function ReadMemoryEx(const ASourceLocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr; ASize: TFpDbgValueSize; ADest: Pointer): Boolean; inline;
    (* ReadRegister needs a Context, to get the thread/stackframe
    *)
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean; inline;
    function ReadRegisterasAddress(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean; inline;

    function WriteMemory(const ADestLocation: TFpDbgMemLocation; const ASize: TFpDbgValueSize;
                         const ASource: Pointer; const AFlags: TFpDbgMemManagerFlags = []
                        ): Boolean; inline;

    // location will be invalid, if read failed
    function ReadAddress(const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize): TFpDbgMemLocation; inline;
    function ReadAddressEx(const ALocation: TFpDbgMemLocation;  AnAddressSpace: TDbgPtr;
                           ASize: TFpDbgValueSize): TFpDbgMemLocation; inline;

    // ALocation and AnAddress MUST NOT be the same variable on the callers side
    function ReadAddress    (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AnAddress: TFpDbgMemLocation): Boolean; inline;
    //function ReadAddress    (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
    //                         out AnAddress: TFpDbgMemLocation;
    //                         AnOpts: TFpDbgMemReadOptionsl): Boolean; inline;
    function ReadUnsignedInt(const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: QWord): Boolean; inline;

    function WriteUnsignedInt(const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             const AValue: QWord): Boolean; inline;

    //function ReadUnsignedInt(const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
    //                         out AValue: QWord;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean; inline;
    function ReadSignedInt  (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: Int64): Boolean; inline;
    function WriteSignedInt(const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             const AValue: Int64): Boolean; inline;
    //function ReadSignedInt  (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
    //                         out AValue: Int64;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean; inline;
    // //enum/set: may need bitorder swapped
    function ReadEnum       (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: QWord): Boolean; inline;
    function WriteEnum      (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             const AValue: QWord): Boolean; inline;
    //function ReadEnum       (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
    //                         out AValue: QWord;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean; inline;
    function ReadSet        (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: TBytes): Boolean; inline;
    function WriteSet        (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             const AValue: TBytes): Boolean; inline;
    //function ReadSet        (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
    //                         out AValue: TBytes;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean; inline;
    function ReadSingle      (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: Single): Boolean; inline;
    function ReadDouble      (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: Double): Boolean; inline;
    function ReadExtended    (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: TDbgExtended): Boolean; inline;
    function ReadFloat      (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
                             out AValue: TDbgExtended): Boolean; inline; deprecated;
    //function ReadFloat      (const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
    //                         out AValue: TDbgExtended;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean; inline;

    function ReadString(const ALocation: TFpDbgMemLocation; ALen: Int64; out AValue: RawByteString; AnIgnoreMaxStringLen: boolean = False): Boolean;
    function ReadWString(const ALocation: TFpDbgMemLocation; ALen: Int64; out AValue: WideString; AnIgnoreMaxStringLen: boolean = False): Boolean;

    function GetRegister(const ARegNum: Cardinal): TDbgRegisterValue;
  end;


  { TFpDbgMemReaderBase }

  TFpDbgMemReaderBase = class
  protected
    // ReadMemoryPartial: workaround for subclasses that do not support partial mem read
    function ReadMemoryPartial(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer; out ABytesRead: Cardinal): Boolean;
  public
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract; overload;
    // inherited Memreaders should implement partial size ReadMemory, and forward it to the TDbgProcess class
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer; out ABytesRead: Cardinal): Boolean; virtual; overload;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract;
    function WriteMemory(AnAddress: TDbgPtr; ASize: Cardinal; ASource: Pointer): Boolean; virtual; abstract; overload;
    // ReadRegister may need TargetMemConvertor
    // Register with reduced size are treated as unsigned
    // TODO: ReadRegister should only take THREAD-ID, not context
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean; virtual; abstract;
    function WriteRegister(ARegNum: Cardinal; const AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean; virtual; abstract;
    function RegisterSize(ARegNum: Cardinal): Integer; virtual; abstract;
    function RegisterNumber(ARegName: String; out ARegNum: Cardinal): Boolean; virtual; abstract;
    function GetRegister(const ARegNum: Cardinal; AContext: TFpDbgLocationContext): TDbgRegisterValue; virtual; abstract;
    // Registernum from name
  end;

  (* Options for all read operation // TODO *)

  TFpDbgMemReadOptions = record
    (* potential flags
         target bit start/shift (method to map dwarf2/3 bit offse to dwarf 4
         target bit size
         target assume big/little endian
         float precisson
         AddressSpace

         ( rmBigEndian, rmLittleEndian, rmUseAddressSpace, UseBitSize)
         AddressSpace, BitOffset, precission
    *)
  end;


  TFpDbgMemReadDataType = (
    rdtRawRead, rdtAddress, rdtSignedInt, rdtUnsignedInt, rdtfloat,
    rdtEnum, rdtSet
  );

  TFpDbgMemConvData = record
    SourceLocation: TFpDbgMemLocation;
    SourceSize: TFpDbgValueSize;
    SourceFullSize: QWord;
    DestSize: QWord;
  end;

  // Todo, cpu/language specific operations, endianess, sign extend, float .... default int value for bool
  // convert from debugge format to debuger format and back
  // TODO: currently it assumes target and own mem are in the same format

  { TFpDbgMemConvertor }

  TFpDbgMemConvertor = class
  public
    (* PrepareTargetRead
       called before every Read operation.
       In case of reading from a bit-offset more memory may be needed, and must be allocated here
    *)
    function PrepareTargetRead(AReadDataType: TFpDbgMemReadDataType;
                               var AConvData: TFpDbgMemConvData;
                               const ADest: Pointer
                              ): boolean; virtual; abstract;
    {function PrepareTargetRead(AReadDataType: TFpDbgMemReadDataType;
                               ASourceMemPointer: TDbgPtr; ADestPointer: Pointer;
                               ASourceMemSize, ADestSize: Cardinal;
                               AnOpts: TFpDbgMemReadOptions;
                               out AConvertorData: TFpDbgMemConvData
                              ): boolean; virtual; abstract;}

    (* FinishTargetRead
       called after every Read operation.
    *)
    function FinishTargetRead(AReadDataType: TFpDbgMemReadDataType;
                               const AConvData: TFpDbgMemConvData;
                               const TmpData: Pointer; // can be equal to ADest
                               const ADest: Pointer
                             ): boolean; virtual; abstract;
    {function FinishTargetRead(AReadDataType: TFpDbgMemReadDataType;
                              ASourceMemPointer: TDbgPtr; ADestPointer: Pointer;
                              ASourceMemSize, ADestSize: Cardinal;
                              AnOpts: TFpDbgMemReadOptions;
                              AConvertorData: TFpDbgMemConvData
                             ): boolean; virtual; abstract;}

    // just to free any data
    procedure FailedTargetRead(AConvertorData: TFpDbgMemConvData); virtual; abstract;

    (* AdjustIntPointer:
       To copy a smaller int/cardinal (e.g. word) into a bigger (e.g. dword),
       adjust ADestPointer so it points to the low value part of the dest
       No conversion
    *)
    function AdjustIntPointer(var ADataPointer: Pointer; ADataSize, ANewSize: Cardinal): Boolean; virtual; abstract;
    //(* SignExtend:
    //   Expects a signed integer value of ASourceSize bytes in the low value end
    //   of the memory (total memory ADataPointer, ADestSize)
    //   Does zero fill the memory, if no sign extend is needed
    //*)
    //procedure SignExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); virtual; abstract;
    //(* Expects an unsigned integer value of ASourceSize bytes in the low value end
    //   of the memory (total memory ADataPointer, ADestSize)
    //   Basically zero fill the memory
    //*)
    //procedure UnsignedExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); virtual; abstract;
  end;

  { TFpDbgMemConvertorLittleEndian }

  TFpDbgMemConvertorLittleEndian = class(TFpDbgMemConvertor)
  public
    function PrepareTargetRead(AReadDataType: TFpDbgMemReadDataType;
                               var AConvData: TFpDbgMemConvData;
                               const ADest: Pointer
                              ): boolean; override;

    function FinishTargetRead(AReadDataType: TFpDbgMemReadDataType;
      const AConvData: TFpDbgMemConvData; const TmpData: Pointer;
  const ADest: Pointer): boolean; override;
    procedure FailedTargetRead({%H-}AConvertorData: TFpDbgMemConvData); override;

    function AdjustIntPointer(var ADataPointer: Pointer; ADataSize, ANewSize: Cardinal): Boolean; override;


    //procedure SignExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); override;
    //procedure UnsignedExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); override;
  end;

  { TFpDbgMemCacheBase }

  TFpDbgMemCacheBase = class
  private
    FMemReader: TFpDbgMemReaderBase;
  protected
    // Init:
    //   will be called by Manager.AddCache after the memreader is set.
    //   Earliest chance to pre-read the memory
    procedure Init; virtual;
    property MemReader: TFpDbgMemReaderBase read FMemReader;
  public
  end;

  { TFpDbgMemCacheManagerBase }

  TFpDbgMemCacheManagerBase = class
  private
    FMemReader: TFpDbgMemReaderBase;
  protected
    procedure InitalizeCache(ACache: TFpDbgMemCacheBase); // must be called by AddCache
  public
    function AddCache(AnAddress: TDbgPtr; ASize: Cardinal): TFpDbgMemCacheBase; virtual;
    function AddCacheEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal): TFpDbgMemCacheBase; virtual;
    procedure RemoveCache(ACache: TFpDbgMemCacheBase); virtual;

    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; overload;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer; ABytesRead: Cardinal): Boolean; virtual; overload;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual;
  end;

  { TFpDbgMemCacheSimple
    MemCache for contineous mem / does not support AddressSpace
    TODO: Handle Overlaps
  }

  TFpDbgMemCacheSimple = class(TFpDbgMemCacheBase)
  private
    FCacheAddress: TDBGPtr;
    FCacheSize: Cardinal;
    FMem: Array of byte;
    FFailed: Boolean;
  public
    constructor Create(ACacheAddress: TDBGPtr; ACacheSize: Cardinal);
    function ContainsMemory(AnAddress: TDbgPtr; ASize: Cardinal): Boolean;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
    property CacheAddress: TDBGPtr read FCacheAddress;
    property CacheSize: Cardinal read FCacheSize;
  end;

  { TFpDbgMemCacheManagerSimple }

  TFpDbgMemCacheManagerSimple = class(TFpDbgMemCacheManagerBase)
  private
    FCaches: TAVLTree;
  public
    constructor Create;
    destructor Destroy; override;

    function HasMemory(AnAddress: TDbgPtr; ASize: Cardinal): Boolean;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override; overload;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer;
      ABytesRead: Cardinal): Boolean; override; overload;

    function AddCache(AnAddress: TDbgPtr; ASize: Cardinal): TFpDbgMemCacheBase;
      override;
    procedure RemoveCache(ACache: TFpDbgMemCacheBase); override;
  end;

  { TFpDbgMemLimits }

  TFpDbgMemLimits = class
  private
    FMaxArrayLen: QWord;
    FMaxMemReadSize: QWord;
    FMaxNullStringSearchLen: QWord;
    FMaxStringLen: QWord;
    procedure SetMaxMemReadSize(AValue: QWord);
  public
    constructor Create;
    property MaxMemReadSize: QWord read FMaxMemReadSize write SetMaxMemReadSize;
    property MaxStringLen: QWord read FMaxStringLen write FMaxStringLen;
    property MaxArrayLen: QWord read FMaxArrayLen write FMaxArrayLen;
    property MaxNullStringSearchLen: QWord read FMaxNullStringSearchLen write FMaxNullStringSearchLen;
  end;

  { TFpDbgMemModel }

  // Here Location is the fpdebug representation of an address
  // and Address is the target (pointer-only) representation of an address
  TFpDbgMemModel = class
    function UpdateLocationToCodeAddress(const ALocation: TFpDbgMemLocation): TFpDbgMemLocation; virtual;
    function LocationToAddress(const ALocation: TFpDbgMemLocation): TDBGPtr; virtual;
    function AddressToTargetLocation(const AAddress: TDBGPtr): TFpDbgMemLocation; virtual;
    function IsReadableMemory(const ALocation: TFpDbgMemLocation): Boolean; virtual;
    function IsReadableLocation(const ALocation: TFpDbgMemLocation): Boolean; virtual;
  end;

  (* TFpDbgMemManager
   * allows to to pretend reading from the target, by using its own memory, or
       a constant.
     This is useful if an object expects to read data from the target, but the
       caller needs to "fake" another value.
     E.g. A TObject variable has the address of the (internal) pointer to the
       object data:
       SomeMethod expects "Address of variable". At that address in the target
       memory it expects another address, the pointer to the object data.
     But when doing "TObject(1234)" then 1234 is the pointer to the object data.
       1234 can not be read from the target memory. MemManager will pretend.
    * Provides access to TFpDbgMemConvertor
    * TODO: allow to pre-read and cache Target mem (e.g. before reading all fields of a record
  *)

  { TFpDbgMemManager }

  TFpDbgMemManager = class
  private const
    TMP_MEM_SIZE = 4096;
    DEF_MAX_PCHAR_LEN = 32 * 1024;
  private
    FCacheManager: TFpDbgMemCacheManagerBase;
    FLastError: TFpError;
    FMemLimits: TFpDbgMemLimits;
    FMemReader: TFpDbgMemReaderBase;
    FPartialReadResultLenght: QWord;
    FTmpMem: array[0..(TMP_MEM_SIZE div 8)+1] of qword; // MUST have at least ONE extra byte
    FTargetMemConvertor: TFpDbgMemConvertor;
    FSelfMemConvertor: TFpDbgMemConvertor; // used when resizing constants (or register values, which are already in self format)
    FStartWirteableSelfMem: TDBGPtr;
    FLenWirteableSelfMem: Cardinal;
    FMemModel: TFpDbgMemModel;
    function GetCacheManager: TFpDbgMemCacheManagerBase;
    procedure BitShiftMem(ASrcMem, ADestMem: Pointer; ASrcSize, ADestSize: cardinal; ABitCnt: Integer);
  protected
    function ReadMemory(AReadDataType: TFpDbgMemReadDataType;
      const ASourceLocation: TFpDbgMemLocation; const ASourceSize: TFpDbgValueSize;
      const ADest: Pointer; const ADestSize: QWord; AContext: TFpDbgLocationContext;
      const AFlags: TFpDbgMemManagerFlags = []
    ): Boolean; virtual;
    function WriteMemory(AReadDataType: TFpDbgMemReadDataType;
      const ADestLocation: TFpDbgMemLocation; const ADestSize: TFpDbgValueSize;
      const ASource: Pointer; const ASourceSize: QWord; AContext: TFpDbgLocationContext;
      const AFlags: TFpDbgMemManagerFlags = []
    ): Boolean; virtual;
    function ReadMemoryEx(const ASourceLocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr; ASize: TFpDbgValueSize; ADest: Pointer; AContext: TFpDbgLocationContext = nil): Boolean;
    (* ReadRegister needs a Context, to get the thread/stackframe
    *)
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgLocationContext {= nil}): Boolean;
    function ReadRegisterAsAddress(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgLocationContext {= nil}): Boolean; virtual;
    property MemReader: TFpDbgMemReaderBase read FMemReader;
  public
    procedure SetCacheManager(ACacheMgr: TFpDbgMemCacheManagerBase);
    property CacheManager: TFpDbgMemCacheManagerBase read GetCacheManager;
  public
    constructor Create(AMemReader: TFpDbgMemReaderBase; AMemConvertor: TFpDbgMemConvertor;
      AMemModel: TFpDbgMemModel);
    constructor Create(AMemReader: TFpDbgMemReaderBase; ATargenMemConvertor, ASelfMemConvertor: TFpDbgMemConvertor;
      AMemModel: TFpDbgMemModel);
    destructor Destroy; override;
    procedure ClearLastError;

    procedure SetWritableSeflMem(AStartWirteableSelfMem: TDBGPtr; ALenWirteableSelfMem: Cardinal);
    procedure ClearWritableSeflMem;

    function RegisterSize(ARegNum: Cardinal): Integer; // This is not context dependent
    function RegisterNumber(ARegName: String; out ARegNum: Cardinal): Boolean;

    function SetLength(var ADest: TByteDynArray; ALength: Int64): Boolean; overload;
    function SetLength(var ADest: RawByteString; ALength: Int64): Boolean; overload;
    function SetLength(var ADest: AnsiString; ALength: Int64): Boolean; overload;
    function SetLength(var ADest: WideString; ALength: Int64): Boolean; overload;
    function CheckDataSize(ASize: Int64): Boolean;
    function ReadPChar(const ALocation: TFpDbgMemLocation; AMaxChars: Int64; out AValue: AnsiString; NoTrimToZero: Boolean = False): Boolean;
    function ReadPWChar(const ALocation: TFpDbgMemLocation; AMaxChars: Int64; out AValue: WideString): Boolean;

    property TargetMemConvertor: TFpDbgMemConvertor read FTargetMemConvertor;
    property SelfMemConvertor: TFpDbgMemConvertor read FSelfMemConvertor;
    property PartialReadResultLenght: QWord read FPartialReadResultLenght;
    property LastError: TFpError read FLastError;
    property MemLimits: TFpDbgMemLimits read FMemLimits;
    property MemModel: TFpDbgMemModel read FMemModel;
  end;

function NilLoc: TFpDbgMemLocation; inline;
function InvalidLoc: TFpDbgMemLocation; inline;
function UnInitializedLoc: TFpDbgMemLocation; inline;
function TargetLoc(AnAddress: TDbgPtr): TFpDbgMemLocation; inline;
function RegisterLoc(ARegNum: Cardinal): TFpDbgMemLocation; inline;
function SelfLoc(AnAddress: TDbgPtr): TFpDbgMemLocation; inline;
function SelfLoc(AnAddress: Pointer): TFpDbgMemLocation; inline;
function ConstLoc(AValue: QWord): TFpDbgMemLocation; inline;
function ConstDerefLoc(AValue: QWord): TFpDbgMemLocation; inline;

function AddBitOffset(const AnAddr: TFpDbgMemLocation; ABitOffset: Int64): TFpDbgMemLocation; inline;

function IsTargetAddr(const ALocation: TFpDbgMemLocation): Boolean; inline;
function IsAddress(const ALocation: TFpDbgMemLocation): Boolean; inline;
function IsConstData(const ALocation: TFpDbgMemLocation): Boolean; inline;
function IsInitializedLoc(const ALocation: TFpDbgMemLocation): Boolean; inline;
function IsValidLoc(const ALocation: TFpDbgMemLocation): Boolean; inline;     // Valid, Nil allowed
function IsReadableLoc(const ALocation: TFpDbgMemLocation): Boolean; inline;  // Valid and not Nil // can be const or reg
function IsReadableMem(const ALocation: TFpDbgMemLocation): Boolean; inline;  // Valid and target or self <> nil
function IsNilLoc(const ALocation: TFpDbgMemLocation): Boolean; inline;    // Valid AND NIL // Does not check mlfTargetRegister
// TODO: registers should be targed.... // May have to rename some of those
function IsTargetNil(const ALocation: TFpDbgMemLocation): Boolean; inline;    // valid targed = nil
function IsTargetNotNil(const ALocation: TFpDbgMemLocation): Boolean; inline; // valid targed <> nil
function IsTargetOrRegNotNil(const ALocation: TFpDbgMemLocation): Boolean; inline; // valid targed <> nil

function ZeroSize: TFpDbgValueSize; inline;
function SizeVal(const ASize: Int64): TFpDbgValueSize; inline;
function SizeFromBits(const ABits: Int64): TFpDbgValueSize; inline;

function IsZeroSize(const ASize: TFpDbgValueSize): Boolean; inline;
function IsByteSize(const ASize: TFpDbgValueSize): Boolean; inline;
function SizeToFullBytes(const ASize: TFpDbgValueSize): Int64; inline;  // Bytes needed to contain this size
function SizeToBits(const ASize: TFpDbgValueSize): Int64; inline;  // Bytes needed to contain this size

operator =  (const a,b: TFpDbgValueSize): Boolean; inline;
operator =  (const a: TFpDbgValueSize; b: Int64): Boolean; inline;
operator >  (const a: TFpDbgValueSize; b: Int64): Boolean; inline;
operator >= (const a: TFpDbgValueSize; b: Int64): Boolean; inline;
operator <  (const a: TFpDbgValueSize; b: Int64): Boolean; inline;
operator <= (const a: TFpDbgValueSize; b: Int64): Boolean; inline;

operator +  (const a,b: TFpDbgValueSize): TFpDbgValueSize; inline;
operator -  (const a,b: TFpDbgValueSize): TFpDbgValueSize; inline;
operator *  (const a: TFpDbgValueSize; b: Int64): TFpDbgValueSize; inline;

operator +  (const AnAddr: TFpDbgMemLocation; ASize: TFpDbgValueSize): TFpDbgMemLocation; inline;
operator +  (const AnAddr: TFpDbgMemLocation; AVal: Int64): TFpDbgMemLocation; inline;
operator -  (const AnAddr: TFpDbgMemLocation; AVal: Int64): TFpDbgMemLocation; inline;
operator +  (const AnAddr: TFpDbgMemLocation; AVal: QWord): TFpDbgMemLocation; inline;
operator -  (const AnAddr: TFpDbgMemLocation; AVal: QWord): TFpDbgMemLocation; inline;

function LocToAddr(const ALocation: TFpDbgMemLocation): TDbgPtr; inline;      // does not check valid
function LocToAddrOrNil(const ALocation: TFpDbgMemLocation): TDbgPtr; inline; // save version

function SignExtend(ASrcVal: QWord; ASrcSize: TFpDbgValueSize): Int64;
function BitMask(ASize: TFpDbgValueSize): QWord; inline;
function SignMask(ASize: TFpDbgValueSize): QWord; inline;

//function EmptyMemReadOpts:TFpDbgMemReadOptions;

function dbgs(const ALocation: TFpDbgMemLocation): String; overload;
function dbgs(const ASize: TFpDbgValueSize): String; overload;
function dbgs(const AReadDataType: TFpDbgMemReadDataType): String; overload;

const
  FULL_BIT_MASK = TDBGPtr(not(qword(0)));

implementation
var
  FPDBG_VERBOSE_MEM: PLazLoggerLogGroup;

function NilLoc: TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.MType := mlfTargetMem;
end;

function InvalidLoc: TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.MType := mlfInvalid;
end;

function UnInitializedLoc: TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.MType := mlfUninitialized;
end;

function TargetLoc(AnAddress: TDbgPtr): TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.Address := AnAddress;
  Result.MType := mlfTargetMem;
end;

function RegisterLoc(ARegNum: Cardinal): TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.Address := ARegNum;
  Result.MType := mlfTargetRegister;
end;

function SelfLoc(AnAddress: TDbgPtr): TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.Address := AnAddress;
  Result.MType := mlfSelfMem;
end;

function SelfLoc(AnAddress: Pointer): TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.Address := TDbgPtr(AnAddress);
  Result.MType := mlfSelfMem;
end;

function ConstLoc(AValue: QWord): TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.Address := AValue;
  Result.MType := mlfConstant;
end;

function ConstDerefLoc(AValue: QWord): TFpDbgMemLocation;
begin
  Result := Default(TFpDbgMemLocation);
  Result.Address := AValue;
  Result.MType := mlfConstantDeref;
end;

function AddBitOffset(const AnAddr: TFpDbgMemLocation; ABitOffset: Int64
  ): TFpDbgMemLocation;
begin
  {$PUSH}{$R-}{$Q-}
  Result := AnAddr;
  Result.Address := AnAddr.Address + ABitOffset div 8;
  if (ABitOffset < 0) and ((ABitOffset and 7) <> 0) then
    Result.Address := Result.Address - 1; // Going to ADD some bits back
                                          // E.g. b=-1 means (b and 7) = 7 and that means adding 7 bits, instead of substracting 1
  Result.BitOffset := ABitOffset and 7;
  {$POP}
end;

function IsTargetAddr(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := ALocation.MType = mlfTargetMem;
end;

function IsAddress(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := ALocation.MType in [mlfTargetMem, mlfSelfMem, mlfConstantDeref];
end;

function IsConstData(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := not(ALocation.MType in [mlfConstant, mlfConstantDeref]);
end;

function IsInitializedLoc(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := ALocation.MType <> mlfUninitialized;
end;

function IsValidLoc(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := not(ALocation.MType in [mlfInvalid, mlfUninitialized]);
end;

function IsReadableLoc(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (not(ALocation.MType in [mlfInvalid, mlfUninitialized])) and
            ( (not(ALocation.MType in [mlfTargetMem, mlfSelfMem])) or
              (ALocation.Address <> 0)
            );
end;

function IsReadableMem(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType in [mlfTargetMem, mlfSelfMem]) and
            (ALocation.Address <> 0);
end;

function IsNilLoc(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType in [mlfTargetMem, mlfSelfMem, mlfConstant]) and
            (ALocation.Address = 0);
end;

function IsTargetNil(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType = mlfTargetMem) and (ALocation.Address = 0);
end;

function IsTargetNotNil(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType = mlfTargetMem) and (ALocation.Address <> 0);
end;

function IsTargetOrRegNotNil(const ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := ((ALocation.MType = mlfTargetMem) and (ALocation.Address <> 0)) or
            (ALocation.MType = mlfTargetRegister);
end;

function ZeroSize: TFpDbgValueSize;
begin
  Result.Size := 0;
  Result.BitSize := 0;
end;

function SizeVal(const ASize: Int64): TFpDbgValueSize;
begin
  Result.Size := ASize;
  Result.BitSize := 0;
end;

function SizeFromBits(const ABits: Int64): TFpDbgValueSize;
begin
  Result.Size := ABits div 8;
  if ABits < 0 then
    Result.BitSize := -((-ABits) and 7)
  else
    Result.BitSize := ABits and 7;
end;

function IsZeroSize(const ASize: TFpDbgValueSize): Boolean;
begin
  Result := (ASize.Size = 0) and (ASize.BitSize = 0);
end;

function IsByteSize(const ASize: TFpDbgValueSize): Boolean;
begin
  Result := (ASize.BitSize = 0);
end;

function SizeToFullBytes(const ASize: TFpDbgValueSize): Int64;
begin
  assert((ASize.Size=0) or (ASize.BitSize=0) or ( (ASize.Size<0) = (ASize.BitSize<0) ), '(ASize.Size=0) or (ASize.BitSize=0) or ( (ASize.Size<0) = (ASize.BitSize<0) )');
  Result := ASize.Size;
  if ASize.BitSize = 0 then
    exit
  else
  if ASize.BitSize > 0 then
    inc(Result)
  else
    dec(Result);
end;

function SizeToBits(const ASize: TFpDbgValueSize): Int64;
begin
  assert((ASize.Size=0) or (ASize.BitSize=0) or ( (ASize.Size<0) = (ASize.BitSize<0) ), '(ASize.Size=0) or (ASize.BitSize=0) or ( (ASize.Size<0) = (ASize.BitSize<0) )');
  Result := ASize.Size * 8 + ASize.BitSize;
end;

operator = (const a, b: TFpDbgValueSize): Boolean;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  assert((b.Size=0) or (b.BitSize=0) or ( (b.Size<0) = (b.BitSize<0) ), '(b.Size=0) or (b.BitSize=0) or ( (b.Size<0) = (b.BitSize<0) )');
  Result := (a.Size = b.Size) and (a.BitSize = b.BitSize);
end;

operator = (const a: TFpDbgValueSize; b: Int64): Boolean;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  Result := (a.Size = b) and (a.BitSize = 0);
end;

operator>(const a: TFpDbgValueSize; b: Int64): Boolean;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  Result := (a.Size > b) or
            (a.Size = b) and (a.BitSize > 0);
end;

operator>=(const a: TFpDbgValueSize; b: Int64): Boolean;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  Result := (a.Size > b) or
            (a.Size = b) and (a.BitSize >= 0);
end;

operator<(const a: TFpDbgValueSize; b: Int64): Boolean;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  Result := (a.Size < b) or
            (a.Size = b) and (a.BitSize < 0);
end;

operator<=(const a: TFpDbgValueSize; b: Int64): Boolean;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  Result := (a.Size < b) or
            (a.Size = b) and (a.BitSize <= 0);
end;

operator + (const a, b: TFpDbgValueSize): TFpDbgValueSize;
var
  bits, low3bits: Int64;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  assert((b.Size=0) or (b.BitSize=0) or ( (b.Size<0) = (b.BitSize<0) ), '(b.Size=0) or (b.BitSize=0) or ( (b.Size<0) = (b.BitSize<0) )');
  {$PUSH}{$R-}{$Q-}
  bits := a.BitSize + b.BitSize;

  Result.Size := a.Size + b.Size + bits div 8;
  low3bits := bits and 7;
  if low3bits = 0 then
    Result.BitSize := 0
  else
  if (Result.Size < 0) or ( (Result.Size=0) and (bits<0) ) then begin
    if (bits > 0) then // bits have wrong sign
      Result.Size := Result.Size + 1;
    Result.BitSize := low3bits - 8;
  end
  else begin
    if (bits < 0) then // bits have wrong sign
      Result.Size := Result.Size - 1;
    Result.BitSize := low3bits;
  end;
  {$POP}
end;

operator - (const a, b: TFpDbgValueSize): TFpDbgValueSize;
var
  bits, low3bits: Int64;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  assert((b.Size=0) or (b.BitSize=0) or ( (b.Size<0) = (b.BitSize<0) ), '(b.Size=0) or (b.BitSize=0) or ( (b.Size<0) = (b.BitSize<0) )');
  {$PUSH}{$R-}{$Q-}
  bits := a.BitSize - b.BitSize;

  Result.Size := a.Size - b.Size + bits div 8;
  low3bits := bits and 7;
  if low3bits = 0 then
    Result.BitSize := 0
  else
  if (Result.Size < 0) or ( (Result.Size=0) and (bits<0) ) then begin
    if (bits > 0) then // bits have wrong sign
      Result.Size := Result.Size + 1;
    Result.BitSize := low3bits - 8;
  end
  else begin
    if (bits < 0) then // bits have wrong sign
      Result.Size := Result.Size - 1;
    Result.BitSize := low3bits;
  end;
  {$POP}
end;

operator * (const a: TFpDbgValueSize; b: Int64): TFpDbgValueSize;
var
  bits, low3bits: Int64;
begin
  assert((a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) ), '(a.Size=0) or (a.BitSize=0) or ( (a.Size<0) = (a.BitSize<0) )');
  {$PUSH}{$R-}{$Q-}
  bits := a.BitSize * b;

  Result.Size := a.Size * b + bits div 8;
  low3bits := bits and 7;
  if low3bits = 0 then
    Result.BitSize := 0
  else
  if (Result.Size < 0) or ( (Result.Size=0) and (bits<0) ) then begin
    if (bits > 0) then // bits have wrong sign
      Result.Size := Result.Size + 1;
    Result.BitSize := low3bits - 8;
  end
  else begin
    if (bits < 0) then // bits have wrong sign
      Result.Size := Result.Size - 1;
    Result.BitSize := low3bits;
  end;
  {$POP}
end;

operator + (const AnAddr: TFpDbgMemLocation; ASize: TFpDbgValueSize
  ): TFpDbgMemLocation;
var
  bits: Int64;
begin
  assert((ASize.Size=0) or (ASize.BitSize=0) or ( (ASize.Size<0) = (ASize.BitSize<0) ), '(ASize.Size=0) or (ASize.BitSize=0) or ( (ASize.Size<0) = (ASize.BitSize<0) )');
  assert(AnAddr.MType in [mlfSelfMem, mlfTargetMem], '+: AnAddr.MType in [mlfSelfMem, mlfTargetMem]');
  Result := AnAddr;
  {$PUSH}{$R-}{$Q-}
  bits := AnAddr.BitOffset + ASize.BitSize;

  Result.Address := AnAddr.Address + ASize.Size + bits div 8;
  if (bits < 0) and ((bits and 7) <> 0) then
    Result.Address := Result.Address - 1; // Going to ADD some bits back
                                          // E.g. bits=-1 means (bits and 7) = 7 and that means adding 7 bits, instead of substracting 1
  Result.BitOffset := bits and 7;
  {$POP}
end;

operator + (const AnAddr: TFpDbgMemLocation; AVal: Int64): TFpDbgMemLocation;
begin
  Result := AnAddr;
  {$PUSH}{$R-}{$Q-}
  Result.Address := AnAddr.Address + AVal;
  {$POP}
end;

operator - (const AnAddr: TFpDbgMemLocation; AVal: Int64): TFpDbgMemLocation;
begin
  Result := AnAddr;
  {$PUSH}{$R-}{$Q-}
  Result.Address := AnAddr.Address - AVal;
  {$POP}
end;

operator + (const AnAddr: TFpDbgMemLocation; AVal: QWord): TFpDbgMemLocation;
begin
  Result := AnAddr;
  {$PUSH}{$R-}{$Q-}
  Result.Address := AnAddr.Address + AVal;
  {$POP}
end;

operator - (const AnAddr: TFpDbgMemLocation; AVal: QWord): TFpDbgMemLocation;
begin
  Result := AnAddr;
  {$PUSH}{$R-}{$Q-}
  Result.Address := AnAddr.Address - AVal;
  {$POP}
end;

function LocToAddr(const ALocation: TFpDbgMemLocation): TDbgPtr;
begin
  assert(ALocation.MType = mlfTargetMem, 'LocToAddr for other than mlfTargetMem');
  Result := ALocation.Address;
end;

function LocToAddrOrNil(const ALocation: TFpDbgMemLocation): TDbgPtr;
begin
  if (ALocation.MType = mlfTargetMem) then
    Result := ALocation.Address
  else
    Result := 0;
end;

function SignExtend(ASrcVal: QWord; ASrcSize: TFpDbgValueSize): Int64;
var
  SourceFullSize, SBit: Int64;
  b: TBitSize;
begin
  Result := Int64(ASrcVal);

  SourceFullSize := SizeToFullBytes(ASrcSize);
  if SourceFullSize = 0 then
    exit;

  b := ASrcSize.BitSize;
  SBit := 8 * SourceFullSize;
  if b > 0 then
    SBit := SBit + b - 8;

  if (ASrcVal and (1 shl (SBit-1)) ) <> 0 then
    Result := Result or (int64(-1) shl SBit);
end;

function BitMask(ASize: TFpDbgValueSize): QWord;
var
  i: Int64;
begin
  Result := not(qword(0));
  i := SizeToBits(ASize);
  if i < 64 then
    Result := Result shr (64 - i);
end;

function SignMask(ASize: TFpDbgValueSize): QWord;
var
  i: Int64;
begin
  Result := 0;
  i := SizeToBits(ASize) - 1;
  if (i >= 0) and (i < 64) then
    Result := QWord(1) shl i;
end;

//function {%H-}EmptyMemReadOpts: TFpDbgMemReadOptions;
//begin
//  //
//end;

function dbgs(const ALocation: TFpDbgMemLocation): String;
begin
  Result := '';
  if not (ALocation.MType in [low(TFpDbgMemLocationType)..high(TFpDbgMemLocationType)]) then
    Result := 'Location=out-of-range'
  else
    WriteStr(Result, 'Location=', ALocation.Address, ':', ALocation.BitOffset, ', ', ALocation.MType);
end;

function dbgs(const ASize: TFpDbgValueSize): String;
begin
  WriteStr(Result, 'Size=', ASize.Size, ':', ASize.BitSize);
end;

function dbgs(const AReadDataType: TFpDbgMemReadDataType): String;
begin
  WriteStr(Result, AReadDataType);
end;

{ TDbgRegisterValue }

function TDbgRegisterValue.GetStrFormatted(AFormat: TRegisterDisplayFormat): string;
begin
  if FFormatter <> nil then
    exit(FFormatter(Self, AFormat));
  if FStrValue <> '' then
    exit(FStrValue);

  case AFormat of
    rdDefault: Result := IntToStr(FNumValue);
    rdHex:     Result := '$'+Dec64ToNumb(FNumValue, 0, 16);
    rdBinary:  Result := '%'+Dec64ToNumb(FNumValue, 0, 2);
    rdOctal:   Result := '&'+Dec64ToNumb(FNumValue, 0, 8);
    rdDecimal: Result := IntToStr(FNumValue);
    rdRaw:     Result := IntToStr(FNumValue);
  end;
end;

function TDbgRegisterValue.GetStrValue: string;
begin
  if (FStrValue = '') and (FFormatter <> nil) then
    FStrValue := FFormatter(Self);
  if (FStrValue = '') then
    FStrValue := '?';
  Result := FStrValue;
end;

constructor TDbgRegisterValue.Create(const AName: String);
begin
  FName:=AName;
end;

destructor TDbgRegisterValue.Destroy;
begin
  inherited Destroy;
  Freemem(FMem);
end;

procedure TDbgRegisterValue.Assign(ASource: TDbgRegisterValue);
begin
  FDwarfIdx := ASource.FDwarfIdx;
  FName     := ASource.FName;
  FNumValue := ASource.FNumValue;
  FSize     := ASource.FSize;
  FStrValue := ASource.FStrValue;
end;

function TDbgRegisterValue.HasEqualVal(AnOther: TDbgRegisterValue): Boolean;
begin
  Result :=
    (FNumValue = AnOther.FNumValue) and
    (FSize     = AnOther.FSize)     and
    (FStrValue = AnOther.FStrValue);
end;

procedure TDbgRegisterValue.SetValue(ANumValue: Integer; const AStrValue: string; ASize: byte;
  ADwarfIdx: cardinal);
var
  CNum: Cardinal absolute ANumValue;
begin
  SetValue(TDBGPtr(CNum), AStrValue, ASize, ADwarfIdx);
end;

procedure TDbgRegisterValue.SetValue(ANumValue: Int64; const AStrValue: string; ASize: byte;
  ADwarfIdx: cardinal);
begin
  SetValue(TDBGPtr(ANumValue), AStrValue, ASize, ADwarfIdx);
end;

procedure TDbgRegisterValue.SetValue(ANumValue: Cardinal; const AStrValue: string; ASize: byte;
  ADwarfIdx: cardinal);
begin
  SetValue(TDBGPtr(ANumValue), AStrValue, ASize, ADwarfIdx);
end;

procedure TDbgRegisterValue.SetValue(ANumValue: TDBGPtr;
  const AStrValue: string; ASize: byte; ADwarfIdx: cardinal);
begin
  FStrValue:=AStrValue;
  FNumValue:=ANumValue;
  FSize:=ASize;
  FDwarfIdx:=ADwarfIdx;
end;

procedure TDbgRegisterValue.SetValue(const AData: Pointer; ASize: byte; ADwarfIdx: cardinal;
  AFormatter: TRegisterFormatterProc);
begin
  FStrValue:='';
  FNumValue:=0;
  FSize := ASize;
  FDwarfIdx:=ADwarfIdx;
  FFormatter := AFormatter;
  if FMem <> nil then
    FMem := ReAllocMem(FMem, ASize)
  else
    FMem := AllocMem(ASize);
  move(AData^, FMem^, ASize);
end;

procedure TDbgRegisterValue.Setx86EFlagsValue(ANumValue: TDBGPtr);
var
  FlagS: string;
begin
  FlagS := '';
  if ANumValue and (1 shl 0) <> 0 then FlagS := FlagS + 'CF ';
  if ANumValue and (1 shl 2) <> 0 then FlagS := FlagS + 'PF ';
  if ANumValue and (1 shl 4) <> 0 then FlagS := FlagS + 'AF ';
  if ANumValue and (1 shl 6) <> 0 then FlagS := FlagS + 'ZF ';
  if ANumValue and (1 shl 7) <> 0 then FlagS := FlagS + 'SF ';
  if ANumValue and (1 shl 8) <> 0 then FlagS := FlagS + 'TF ';
  if ANumValue and (1 shl 9) <> 0 then FlagS := FlagS + 'IF ';
  if ANumValue and (1 shl 10) <> 0 then FlagS := FlagS + 'DF ';
  if ANumValue and (1 shl 11) <> 0 then FlagS := FlagS + 'OF ';
  if (ANumValue shr 12) and 3 <> 0 then FlagS := FlagS + 'IOPL=' + IntToStr((ANumValue shr 12) and 3);
  if ANumValue and (1 shl 14) <> 0 then FlagS := FlagS + 'NT ';
  if ANumValue and (1 shl 16) <> 0 then FlagS := FlagS + 'RF ';
  if ANumValue and (1 shl 17) <> 0 then FlagS := FlagS + 'VM ';
  if ANumValue and (1 shl 18) <> 0 then FlagS := FlagS + 'AC ';
  if ANumValue and (1 shl 19) <> 0 then FlagS := FlagS + 'VIF ';
  if ANumValue and (1 shl 20) <> 0 then FlagS := FlagS + 'VIP ';
  if ANumValue and (1 shl 21) <> 0 then FlagS := FlagS + 'ID ';

  SetValue(ANumValue, trim(FlagS),4,Cardinal(-1));
end;

{ TFpDbgLocationContext }

function TFpDbgLocationContext.GetLastMemError: TFpError;
begin
  Result := MemManager.LastError;
end;

function TFpDbgLocationContext.GetFrameBase: TDBGPtr;
begin
  if FFrameBaseCallback <> nil then begin
    FFrameBase := FFrameBaseCallback(Self, FFrameBaseError);
    if (FFrameBase = 0) and not IsError(FFrameBaseError) then
      FFrameBaseError := CreateError(fpErrAnyError, []);
    FFrameBaseCallback := nil;
  end;
  Result := FFrameBase;
end;

function TFpDbgLocationContext.GetCfaFrameBase: TDBGPtr;
begin
  if FCfaFrameBaseCallback <> nil then begin
    FCfaFrameBase := FCfaFrameBaseCallback(Self, FCfaFrameBaseError);
    if (FCfaFrameBase = 0) and not IsError(FCfaFrameBaseError) then
      FCfaFrameBaseError := CreateError(fpErrAnyError, []);
    FCfaFrameBaseCallback := nil;
  end;
  Result := FCfaFrameBase;
end;

function TFpDbgLocationContext.GetPartialReadResultLenght: QWord;
begin
  Result := MemManager.PartialReadResultLenght;
end;

procedure TFpDbgLocationContext.SetFrameBaseCallback(ACallback: TGetFrameBaseCallback);
begin
  FFrameBaseCallback := ACallback;
end;

procedure TFpDbgLocationContext.SetCfaFrameBaseCallback(ACallback: TGetFrameBaseCallback);
begin
  FCfaFrameBaseCallback := ACallback;
end;

procedure TFpDbgLocationContext.ClearLastMemError;
begin
  MemManager.ClearLastError;
end;

function TFpDbgLocationContext.ReadMemory(
  const ASourceLocation: TFpDbgMemLocation; const ASize: TFpDbgValueSize;
  const ADest: Pointer; const AFlags: TFpDbgMemManagerFlags): Boolean;
begin
  Result := MemManager.ReadMemory(rdtRawRead, ASourceLocation, ASize, ADest, ASize.Size, Self, AFlags);
end;

function TFpDbgLocationContext.ReadMemoryEx(
  const ASourceLocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr;
  ASize: TFpDbgValueSize; ADest: Pointer): Boolean;
begin
  Result := MemManager.ReadMemoryEx(ASourceLocation, AnAddressSpace, ASize, ADest, Self);
end;

function TFpDbgLocationContext.ReadRegister(ARegNum: Cardinal; out
  AValue: TDbgPtr): Boolean;
begin
  Result := MemManager.ReadRegister(ARegNum, AValue, Self);
end;

function TFpDbgLocationContext.ReadRegisterasAddress(ARegNum: Cardinal; out
  AValue: TDbgPtr): Boolean;
begin
  Result := MemManager.ReadRegisterAsAddress(ARegNum, AValue, Self);
end;

function TFpDbgLocationContext.WriteMemory(
  const ADestLocation: TFpDbgMemLocation; const ASize: TFpDbgValueSize;
  const ASource: Pointer; const AFlags: TFpDbgMemManagerFlags): Boolean;
begin
  Result := MemManager.WriteMemory(rdtRawRead, ADestLocation, ASize, ASource, ASize.Size, Self, AFlags);
end;

function TFpDbgLocationContext.ReadAddress(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize): TFpDbgMemLocation;
begin
  if MemManager.ReadMemory(rdtAddress, ALocation, ASize, @Result.Address, SizeOf(Result.Address), Self) then
    Result := MemModel.AddressToTargetLocation(Result.Address)
  else
    Result := InvalidLoc;
end;

function TFpDbgLocationContext.ReadAddressEx(
  const ALocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr;
  ASize: TFpDbgValueSize): TFpDbgMemLocation;
begin
  Result := InvalidLoc;
end;

function TFpDbgLocationContext.ReadAddress(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; out AnAddress: TFpDbgMemLocation): Boolean;
begin
  Result := MemManager.ReadMemory(rdtAddress, ALocation, ASize, @AnAddress.Address, (SizeOf(AnAddress.Address)), Self);
  if Result then
    AnAddress := MemModel.AddressToTargetLocation(AnAddress.Address)
  else
    AnAddress := InvalidLoc;
end;

function TFpDbgLocationContext.ReadUnsignedInt(
  const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize; out AValue: QWord
  ): Boolean;
begin
  Result := MemManager.ReadMemory(rdtUnsignedInt, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.WriteUnsignedInt(
  const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
  const AValue: QWord): Boolean;
begin
  Result := MemManager.WriteMemory(rdtUnsignedInt, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.ReadSignedInt(
  const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize; out AValue: Int64
  ): Boolean;
begin
  Result := MemManager.ReadMemory(rdtSignedInt, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.WriteSignedInt(
  const ALocation: TFpDbgMemLocation; ASize: TFpDbgValueSize;
  const AValue: Int64): Boolean;
begin
  Result := MemManager.WriteMemory(rdtSignedInt, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.ReadEnum(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; out AValue: QWord): Boolean;
begin
  Result := MemManager.ReadMemory(rdtEnum, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.WriteEnum(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; const AValue: QWord): Boolean;
begin
  Result := MemManager.WriteMemory(rdtEnum, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.ReadSet(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; out AValue: TBytes): Boolean;
begin
  System.SetLength(AValue, SizeToFullBytes(ASize));
  Result := ASize > 0;
  if Result then
    Result := MemManager.ReadMemory(rdtSet, ALocation, ASize, @AValue[0], Length(AValue), Self);
end;

function TFpDbgLocationContext.WriteSet(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; const AValue: TBytes): Boolean;
begin
  Result := MemManager.WriteMemory(rdtSet, ALocation, ASize, @AValue[0], Length(AValue), Self);
end;

function TFpDbgLocationContext.ReadSingle(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; out AValue: Single): Boolean;
begin
  Result := MemManager.ReadMemory(rdtfloat, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.ReadDouble(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; out AValue: Double): Boolean;
begin
  Result := MemManager.ReadMemory(rdtfloat, ALocation, ASize, @AValue, (SizeOf(AValue)), Self);
end;

function TFpDbgLocationContext.ReadExtended(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; out AValue: TDbgExtended): Boolean;
begin
  {$IF DBG_HAS_EXTENDED}
  Result := MemManager.ReadMemory(rdtfloat, ALocation, ASize, @AValue, Min(SizeOf(AValue), DBG_EXTENDED_SIZE), Self);
  {$ELSE}
  Result := MemManager.ReadMemory(rdtfloat, ALocation, ASize, @AValue, Min(SizeOf(AValue), SizeOf(TDbgExtended)), Self);
  {$ENDIF}
end;

function TFpDbgLocationContext.ReadFloat(const ALocation: TFpDbgMemLocation;
  ASize: TFpDbgValueSize; out AValue: TDbgExtended): Boolean;
begin
  {$IF DBG_HAS_EXTENDED}
  Result := MemManager.ReadMemory(rdtfloat, ALocation, ASize, @AValue, Min(SizeOf(AValue), DBG_EXTENDED_SIZE), Self);
  {$ELSE}
  Result := MemManager.ReadMemory(rdtfloat, ALocation, ASize, @AValue, Min(SizeOf(AValue), SizeOf(TDbgExtended)), Self);
  {$ENDIF}
end;

function TFpDbgLocationContext.ReadString(const ALocation: TFpDbgMemLocation; ALen: Int64; out
  AValue: RawByteString; AnIgnoreMaxStringLen: boolean): Boolean;
begin
  Result := False;
  AValue := '';

  if (not AnIgnoreMaxStringLen) and
     (MemManager.MemLimits.MaxStringLen > 0) and
     (ALen > MemManager.MemLimits.MaxStringLen)
  then
    ALen := MemManager.MemLimits.MaxStringLen;

  if ALen = 0 then begin
    Result := True;
    exit;
  end;

  if not MemManager.SetLength(AValue, ALen) then
    exit;

  Result := ReadMemory(ALocation, SizeVal(Length(AValue)), @AValue[1]);
  if not Result then
    AValue := ''
end;

function TFpDbgLocationContext.ReadWString(const ALocation: TFpDbgMemLocation; ALen: Int64; out
  AValue: WideString; AnIgnoreMaxStringLen: boolean): Boolean;
begin
  Result := False;
  AValue := '';

  if (not AnIgnoreMaxStringLen) and
     (MemManager.MemLimits.MaxStringLen > 0) and
     (ALen > MemManager.MemLimits.MaxStringLen)
  then
    ALen := MemManager.MemLimits.MaxStringLen;

  if ALen = 0 then begin
    Result := True;
    exit;
  end;

  if not MemManager.SetLength(AValue, ALen) then
    exit;

  Result := ReadMemory(ALocation, SizeVal(Length(AValue)*2), @AValue[1]);
  if not Result then
    AValue := ''
end;

function TFpDbgLocationContext.GetRegister(const ARegNum: Cardinal): TDbgRegisterValue;
begin
  Result := MemManager.MemReader.GetRegister(ARegNum, Self);
end;

{ TFpDbgMemLimits }

procedure TFpDbgMemLimits.SetMaxMemReadSize(AValue: QWord);
begin
  if (AValue <> 0) and (AValue < MINIMUM_MEMREAD_LIMIT) then
    AValue := MINIMUM_MEMREAD_LIMIT;
  if FMaxMemReadSize = AValue then Exit;
  FMaxMemReadSize := AValue;
end;

constructor TFpDbgMemLimits.Create;
begin
  FMaxMemReadSize := 512 * 1024 * 1024; // Do not try allocating more than 0.5 GB by default
  FMaxArrayLen    := 10000;
  FMaxStringLen   := 100 * 1024;
  FMaxNullStringSearchLen := 20 * 1024;
end;

{ TFpDbgMemModel }

function TFpDbgMemModel.UpdateLocationToCodeAddress(const
  ALocation: TFpDbgMemLocation): TFpDbgMemLocation;
begin
  Result := ALocation;
end;

function TFpDbgMemModel.LocationToAddress(const
  ALocation: TFpDbgMemLocation): TDBGPtr;
begin
  Result := LocToAddr(ALocation);
end;

function TFpDbgMemModel.AddressToTargetLocation(const
  AAddress: TDBGPtr): TFpDbgMemLocation;
begin
  Result := TargetLoc(AAddress);
end;

function TFpDbgMemModel.IsReadableMemory(const
  ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := IsReadableMem(ALocation);
end;

function TFpDbgMemModel.IsReadableLocation(const
  ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := IsReadableLoc(ALocation);
end;

{ TFpDbgMemReaderBase }

function TFpDbgMemReaderBase.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ADest: Pointer; out ABytesRead: Cardinal): Boolean;
begin
  Result := ReadMemoryPartial(AnAddress, ASize, ADest, ABytesRead);
end;

function TFpDbgMemReaderBase.ReadMemoryPartial(AnAddress: TDbgPtr;
  ASize: Cardinal; ADest: Pointer; out ABytesRead: Cardinal): Boolean;
var
  SizeRemaining, sz: Cardinal;
  Offs: Integer;
  Dummy: QWord;
begin
  ABytesRead := ASize;
  Result := ReadMemory(AnAddress, ASize, ADest);
  if Result then
    exit;

  SizeRemaining := ASize;
  Offs := 0;
  ABytesRead := 0;

  // check if the address is readable at all
  Result := ReadMemory(AnAddress, 1, @Dummy);
  if not Result then
    exit;

  while SizeRemaining > 0 do begin
    Result := False;
    sz := SizeRemaining;
    while (not Result) and (sz > 1) do begin
      sz := sz div 2;
      Result := ReadMemory(AnAddress, sz, Pointer(PByte(ADest) + Offs));
    end;
    if not Result then
      break;

    ABytesRead := ABytesRead + sz;
    Offs := Offs + sz;
    AnAddress := AnAddress + sz;
    SizeRemaining := SizeRemaining - sz;
  end;

  Result := ABytesRead > 0;
end;

{ TFpDbgMemConvertorLittleEndian }

function TFpDbgMemConvertorLittleEndian.PrepareTargetRead(
  AReadDataType: TFpDbgMemReadDataType; var AConvData: TFpDbgMemConvData;
  const ADest: Pointer): boolean;
begin
  Result := AConvData.SourceFullSize <= AConvData.DestSize;
  if not Result then
    exit;
  case AReadDataType of
    rdtAddress, rdtSignedInt, rdtUnsignedInt,
    rdtEnum, rdtSet: ;
    rdtfloat:
      Result := IsByteSize(AConvData.SourceSize) and
                ( (AConvData.SourceSize.Size = DBG_EXTENDED_SIZE) or
                  (AConvData.SourceSize.Size = SizeOf(Extended)) or
                  (AConvData.SourceSize.Size = SizeOf(Double)) or
                  (AConvData.SourceSize.Size = SizeOf(Single)) or
                  (AConvData.SourceSize.Size = SizeOf(real48))
                );
    rdtRawRead: ;
    else begin
      Assert(False, 'TFpDbgMemConvertorLittleEndian.PrepareTargetRead');
      Result := False;
    end;
  end;
end;

function TFpDbgMemConvertorLittleEndian.FinishTargetRead(
  AReadDataType: TFpDbgMemReadDataType;
  const AConvData: TFpDbgMemConvData; const TmpData: Pointer;
  const ADest: Pointer): boolean;
type
  Preal48 = ^real48;
var
  s: Boolean;
  b: TBitAddr;
begin
  Result := TmpData = ADest;
  if not Result then
    exit;
  case AReadDataType of
    rdtAddress, rdtUnsignedInt, rdtEnum, rdtSet: begin
        if AConvData.SourceFullSize < AConvData.DestSize then
          FillByte((ADest + AConvData.SourceFullSize)^, AConvData.DestSize-AConvData.SourceFullSize, $00);
        if AConvData.SourceSize.BitSize <> 0 then begin
          assert(AConvData.SourceFullSize > 0, 'TFpDbgMemConvertorLittleEndian.FinishTargetRead: AConvData.SourceFullSize > 0');
          PByte(ADest + AConvData.SourceFullSize - 1)^ := Byte(PByte(ADest + AConvData.SourceFullSize - 1)^ and
            (Byte($FF) shr (8 - AConvData.SourceSize.BitSize)));
        end;
      end;
    rdtSignedInt: begin
        if AConvData.SourceFullSize < AConvData.DestSize then begin
          b := AConvData.SourceSize.BitSize;
          s := False;
          if (AConvData.SourceFullSize > 0) then begin
            if b = 0 then
              s := ((PByte(ADest + AConvData.SourceFullSize - 1)^ and $80) <> 0)
            else
              s := ((PByte(ADest + AConvData.SourceFullSize - 1)^ and (1 shl (b-1)) ) <> 0);
          end;

          if s then
            FillByte((ADest + AConvData.SourceFullSize)^, AConvData.DestSize-AConvData.SourceFullSize, $FF)
          else
            FillByte((ADest + AConvData.SourceFullSize)^, AConvData.DestSize-AConvData.SourceFullSize, $00);
          if b <> 0 then begin
            assert(AConvData.SourceFullSize > 0, 'TFpDbgMemConvertorLittleEndian.FinishTargetRead: AConvData.SourceFullSize > 0');
            if s then
              PByte(ADest + AConvData.SourceFullSize - 1)^ := Byte(PByte(ADest + AConvData.SourceFullSize - 1)^ or
                (Byte($FF) shl b))
            else
              PByte(ADest + AConvData.SourceFullSize - 1)^ := Byte(PByte(ADest + AConvData.SourceFullSize - 1)^ and
                (Byte($FF) shr (8 - b)));
          end;
        end;
      end;
    rdtfloat: begin
      // Currently we have matching sizes, except for real48

      if (AConvData.SourceFullSize = DBG_EXTENDED_SIZE) then begin
        case AConvData.DestSize of
          {$IF DBG_HAS_EXTENDED}
          DBG_EXTENDED_SIZE: PDbgExtended(ADest)^ := PDbgExtended(ADest)^;
          {$ENDIF}
          SizeOf(double):       PDouble(ADest)^      := PDbgExtended(ADest)^;
          SizeOf(Single):       PSingle(ADest)^      := PDbgExtended(ADest)^;
          else assert(False, 'TFpDbgMemConvertorLittleEndian.FinishTargetRead: TargetSize not matching');
        end;
      end
      else
      if (AConvData.SourceFullSize = SizeOf(Extended)) then begin
        case AConvData.DestSize of
          {$IF DBG_HAS_EXTENDED}
          DBG_EXTENDED_SIZE: PDbgExtended(ADest)^ := PExtended(ADest)^;
          {$ENDIF}
          SizeOf(double):       PDouble(ADest)^      := PExtended(ADest)^;
          SizeOf(Single):       PSingle(ADest)^      := PExtended(ADest)^;
          else assert(False, 'TFpDbgMemConvertorLittleEndian.FinishTargetRead: TargetSize not matching');
        end;
      end
      else
      if (AConvData.SourceFullSize = SizeOf(Double)) then begin
        case AConvData.DestSize of
          {$IF DBG_HAS_EXTENDED}
          DBG_EXTENDED_SIZE: PDbgExtended(ADest)^ := PDouble(ADest)^;
          {$ENDIF}
          SizeOf(double):       PDouble(ADest)^      := PDouble(ADest)^;
          SizeOf(Single):       PSingle(ADest)^      := PDouble(ADest)^;
          else assert(False, 'TFpDbgMemConvertorLittleEndian.FinishTargetRead: TargetSize not matching');
        end;
      end
      else
      if (AConvData.SourceFullSize = SizeOf(Real48)) then begin
        case AConvData.DestSize of
          {$IF DBG_HAS_EXTENDED}
          DBG_EXTENDED_SIZE: PDbgExtended(ADest)^ := double(Preal48(ADest)^);
          {$ENDIF}
          SizeOf(double):       PDouble(ADest)^      := Preal48(ADest)^;
          //SizeOf(Single):       PSingle(ADest)^      := Preal48(ADest)^;
          else assert(False, 'TFpDbgMemConvertorLittleEndian.FinishTargetRead: TargetSize not matching');
        end;
      end
      else
      if (AConvData.SourceFullSize = SizeOf(Single)) then begin
        case AConvData.DestSize of
          {$IF DBG_HAS_EXTENDED}
          DBG_EXTENDED_SIZE: PDbgExtended(ADest)^ := PSingle(ADest)^;
          {$ENDIF}
          SizeOf(double):       PDouble(ADest)^      := PSingle(ADest)^;
          SizeOf(Single):       PSingle(ADest)^      := PSingle(ADest)^;
          else assert(False, 'TFpDbgMemConvertorLittleEndian.FinishTargetRead: TargetSize not matching');
        end;
      end
      else
        Result := False;
    end;
    rdtRawRead: ; // TODO: cut bits?
    else begin
      Assert(False, 'TFpDbgMemConvertorLittleEndian.FailedTargetRead');
      Result := False;
    end;
  end;
end;

procedure TFpDbgMemConvertorLittleEndian.FailedTargetRead(AConvertorData: TFpDbgMemConvData);
begin
  //
end;

function TFpDbgMemConvertorLittleEndian.AdjustIntPointer(
  var ADataPointer: Pointer; ADataSize, ANewSize: Cardinal): Boolean;
begin
  Result := ANewSize <= ADataSize;
  // no adjustment needed
end;

//procedure TFpDbgMemConvertorLittleEndian.SignExtend(ADataPointer: Pointer; ASourceSize,
//  ADestSize: Cardinal);
//begin
//  Assert(ASourceSize > 0, 'TFpDbgMemConvertorLittleEndian.SignExtend');
//  if ASourceSize >= ADestSize then
//    exit;
//
//  if (PByte(ADataPointer + ASourceSize - 1)^ and $80) <> 0 then
//    FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $ff)
//  else
//    FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $00)
//end;
//
//procedure TFpDbgMemConvertorLittleEndian.UnsignedExtend(ADataPointer: Pointer;
//  ASourceSize, ADestSize: Cardinal);
//begin
//  Assert(ASourceSize > 0, 'TFpDbgMemConvertorLittleEndian.SignExtend');
//  if ASourceSize >= ADestSize then
//    exit;
//
//  FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $00)
//end;

{ TFpDbgMemCacheBase }

procedure TFpDbgMemCacheBase.Init;
begin
  //
end;

{ TFpDbgMemCacheManagerBase }

procedure TFpDbgMemCacheManagerBase.InitalizeCache(ACache: TFpDbgMemCacheBase);
begin
  ACache.FMemReader := FMemReader;
  ACache.Init;
end;

function TFpDbgMemCacheManagerBase.AddCache(AnAddress: TDbgPtr; ASize: Cardinal
  ): TFpDbgMemCacheBase;
begin
  Result := nil;
end;

function TFpDbgMemCacheManagerBase.AddCacheEx(AnAddress,
  AnAddressSpace: TDbgPtr; ASize: Cardinal): TFpDbgMemCacheBase;
begin
  Result := nil;
end;

procedure TFpDbgMemCacheManagerBase.RemoveCache(ACache: TFpDbgMemCacheBase);
begin
  //
end;

function TFpDbgMemCacheManagerBase.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ADest: Pointer): Boolean;
begin
  Result := FMemReader.ReadMemory(AnAddress, ASize, ADest);
end;

function TFpDbgMemCacheManagerBase.ReadMemory(AnAddress: TDbgPtr;
  ASize: Cardinal; ADest: Pointer; ABytesRead: Cardinal): Boolean;
begin
  Result := FMemReader.ReadMemory(AnAddress, ASize, ADest, ABytesRead);
end;

function TFpDbgMemCacheManagerBase.ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Result := FMemReader.ReadMemoryEx(AnAddress, AnAddressSpace, ASize, ADest);
end;

{ TFpDbgMemCacheSimple }

constructor TFpDbgMemCacheSimple.Create(ACacheAddress: TDBGPtr;
  ACacheSize: Cardinal);
begin
  FCacheAddress := ACacheAddress;
  FCacheSize := ACacheSize;
end;

function TFpDbgMemCacheSimple.ContainsMemory(AnAddress: TDbgPtr; ASize: Cardinal
  ): Boolean;
begin
  Result := (ASize <= High(TDbgPtr) - AnAddress) and // not impossible memory range
            (AnAddress >= FCacheAddress) and (AnAddress + ASize <= FCacheAddress + FCacheSize);
end;

function TFpDbgMemCacheSimple.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ADest: Pointer): Boolean;
begin
  Result := False;
  if (ASize > High(TDbgPtr) - AnAddress) or // impossible memory range
     (AnAddress < FCacheAddress) or (AnAddress + ASize > FCacheAddress + FCacheSize) or
     FFailed
  then
    exit;

  if FMem = nil then begin
    SetLength(FMem, FCacheSize);
    if not MemReader.ReadMemory(FCacheAddress, FCacheSize, @FMem[0]) then begin
      FMem := nil;
      FFailed := True;
      exit;
    end;
  end;

  Result := true;
  move(FMem[AnAddress - FCacheAddress], PByte(ADest)^, ASize);
end;

{ TFpDbgMemCacheManagerSimple }

function CompareNodes(Item1, Item2: Pointer): Integer;
begin
  if TFpDbgMemCacheSimple(Item1).CacheAddress > TFpDbgMemCacheSimple(Item2).CacheAddress
  then Result := 1
  else
  if TFpDbgMemCacheSimple(Item1).CacheAddress < TFpDbgMemCacheSimple(Item2).CacheAddress
  then Result := -1
  else Result := 0;
end;

function CompareKey(Key, Item2: Pointer): Integer;
begin
  If PDBGPtr(Key)^ > TFpDbgMemCacheSimple(Item2).CacheAddress
  then Result := 1
  else
  If PDBGPtr(Key)^ < TFpDbgMemCacheSimple(Item2).CacheAddress
  then Result := -1
  else Result := 0;
end;

constructor TFpDbgMemCacheManagerSimple.Create;
begin
  FCaches := TAVLTree.Create;
  FCaches.OnCompare := @CompareNodes;
end;

destructor TFpDbgMemCacheManagerSimple.Destroy;
begin
  inherited Destroy;
  FCaches.Free;
end;

function TFpDbgMemCacheManagerSimple.HasMemory(AnAddress: TDbgPtr;
  ASize: Cardinal): Boolean;
var
  Node: TAVLTreeNode;
begin
  Result := False;
  if ASize > High(TDbgPtr) - AnAddress then // impossible memory range
    exit;

  Node := FCaches.FindNearestKey(@AnAddress, @CompareKey);
  if Node = nil then
    exit;

  if TFpDbgMemCacheSimple(Node.Data).CacheAddress > AnAddress then
    Node := Node.Precessor;;
  if Node = nil then
    exit;

  Result := (AnAddress >= TFpDbgMemCacheSimple(Node.Data).CacheAddress) and
            (AnAddress + ASize <= TFpDbgMemCacheSimple(Node.Data).CacheAddress +
             TFpDbgMemCacheSimple(Node.Data).CacheSize);
end;

function TFpDbgMemCacheManagerSimple.ReadMemory(AnAddress: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
var
  Node: TAVLTreeNode;
begin
  Node := FCaches.FindNearestKey(@AnAddress, @CompareKey);
  if Node = nil then
    exit(inherited ReadMemory(AnAddress, ASize, ADest));

  if TFpDbgMemCacheSimple(Node.Data).CacheAddress > AnAddress then
    Node := Node.Precessor;;
  if Node = nil then
    exit(inherited ReadMemory(AnAddress, ASize, ADest));

  if TFpDbgMemCacheSimple(Node.Data).ContainsMemory(AnAddress, ASize) then begin
    Result := TFpDbgMemCacheSimple(Node.Data).ReadMemory(AnAddress, ASize, ADest);
    exit;
  end;

  Result := inherited ReadMemory(AnAddress, ASize, ADest);
end;

function TFpDbgMemCacheManagerSimple.ReadMemory(AnAddress: TDbgPtr;
  ASize: Cardinal; ADest: Pointer; ABytesRead: Cardinal): Boolean;
var
  Node: TAVLTreeNode;
begin
  Node := FCaches.FindNearestKey(@AnAddress, @CompareKey);
  if Node = nil then
    exit(inherited ReadMemory(AnAddress, ASize, ADest, ABytesRead));

  if TFpDbgMemCacheSimple(Node.Data).CacheAddress > AnAddress then
    Node := Node.Precessor;;
  if Node = nil then
    exit(inherited ReadMemory(AnAddress, ASize, ADest, ABytesRead));

  // TODO: Allow to cache partial mem reads
  if TFpDbgMemCacheSimple(Node.Data).ContainsMemory(AnAddress, ASize) then begin
    ABytesRead := ASize;
    Result := TFpDbgMemCacheSimple(Node.Data).ReadMemory(AnAddress, ASize, ADest);
    exit;
  end;

  Result := inherited ReadMemory(AnAddress, ASize, ADest, ABytesRead);
end;

function TFpDbgMemCacheManagerSimple.AddCache(AnAddress: TDbgPtr;
  ASize: Cardinal): TFpDbgMemCacheBase;
begin
  Result := nil;
  if HasMemory(AnAddress, ASize) then
    exit;

  Result := TFpDbgMemCacheSimple.Create(AnAddress, ASize);
  InitalizeCache(Result);
  FCaches.Add(Result);
end;

procedure TFpDbgMemCacheManagerSimple.RemoveCache(ACache: TFpDbgMemCacheBase);
begin
  if ACache = nil then
    exit;

  FCaches.RemovePointer(ACache);
  ACache.Free;
end;

{ TFpDbgMemLocation }

class operator TFpDbgMemLocation. = (a, b: TFpDbgMemLocation): boolean;
begin
  Result := (a.Address = b.Address) and
            (a.MType = b.MType) and (a.BitOffset = b.BitOffset) and
            (a.AddressClass = b.AddressClass);
end;

class operator TFpDbgMemLocation.<(a, b: TFpDbgMemLocation): boolean;
begin
  Result := (a.Address < b.Address) or
            ( (a.Address = b.Address) and
              ( (a.MType < b.MType) or
                ( (a.MType = b.MType) and
                  (a.BitOffset < b.BitOffset) or
                  ( (a.BitOffset = b.BitOffset) and (a.AddressClass < b.AddressClass) )
                )
             ) );
end;

class operator TFpDbgMemLocation.>(a, b: TFpDbgMemLocation): boolean;
begin
  Result := (a.Address > b.Address) or
            ( (a.Address = b.Address) and
              ( (a.MType > b.MType) or
                ( (a.MType = b.MType) and
                  (a.BitOffset > b.BitOffset) or
                  ( (a.BitOffset = b.BitOffset) and (a.AddressClass > b.AddressClass) )
                )
             ) );
end;

{ TFpDbgMemManager }

function TFpDbgMemManager.GetCacheManager: TFpDbgMemCacheManagerBase;
begin
  If FCacheManager = nil then
    SetCacheManager(TFpDbgMemCacheManagerBase.Create);
  Result := FCacheManager;
end;

procedure TFpDbgMemManager.BitShiftMem(ASrcMem, ADestMem: Pointer; ASrcSize,
  ADestSize: cardinal; ABitCnt: Integer);
var
  Next, Cur: Byte;
begin
  Next := PByte(ASrcMem)^;
  dec(ADestSize);
  while ADestSize > 0 do begin
    Cur := Next;
    Next := PByte(ASrcMem + 1)^;
    PByte(ADestMem)^ := Byte(( Cur shr ABitCnt) or ( Next shl (8 - ABitCnt) ));
    ASrcMem := ASrcMem + 1;
    ADestMem := ADestMem + 1;
    dec(ADestSize);
  end;
  Cur := Next;
  Next := 0;
  if ASrcSize > ADestSize then
    Next := PByte(ASrcMem + 1)^;
  PByte(ADestMem)^ := Byte(( Cur shr ABitCnt) or ( Next shl (8 - ABitCnt) ));
end;

function TFpDbgMemManager.ReadMemory(AReadDataType: TFpDbgMemReadDataType;
  const ASourceLocation: TFpDbgMemLocation; const ASourceSize: TFpDbgValueSize;
  const ADest: Pointer; const ADestSize: QWord; AContext: TFpDbgLocationContext;
  const AFlags: TFpDbgMemManagerFlags): Boolean;
var
  ConvData: TFpDbgMemConvData;
  ReadData, ReadData2: Pointer;
  TmpVal: TDbgPtr;
  BitOffset, SourceExtraSize: Integer;
  SourceReadSize, SourceFullSize: QWord;
begin
  Result := False;
  FPartialReadResultLenght := SizeToFullBytes(ASourceSize);
  DebugLn(FPDBG_VERBOSE_MEM, ['$ReadMem: ', dbgs(AReadDataType),' ', dbgs(ASourceLocation), ' ', dbgs(ASourceSize), ' Dest ', ADestSize]);
  assert((AContext<>nil) or not(ASourceLocation.MType in [mlfTargetRegister]), 'TFpDbgMemManager.ReadMemory: (AContext<>nil) or not(ASourceLocation.MType in [mlfTargetRegister])');

  // To late for an error, Dest-mem is already allocated
  assert((FMemLimits.MaxMemReadSize = 0) or (SizeToFullBytes(ASourceSize) <= FMemLimits.MaxMemReadSize), 'TFpDbgMemManager.ReadMemory: (FMemLimits.MaxMemReadSize = 0) or (SizeToFullBytes(ASourceSize) <= FMemLimits.MaxMemReadSize)');

  if (ASourceLocation.MType in [mlfInvalid, mlfUninitialized]) or
     (ASourceSize <= 0)
  then begin
    FLastError := CreateError(fpInternalErrCanNotReadInvalidMem);
    exit;
  end;

  FLastError := NoError;

  ConvData.SourceLocation     := ASourceLocation;
  ConvData.SourceSize         := ASourceSize; // ONLY valid for target/self-mem // Currently set equal to ADestSize;
  ConvData.SourceFullSize     := SizeToFullBytes(ASourceSize);
  ConvData.DestSize           := ADestSize;

  if not TargetMemConvertor.PrepareTargetRead(AReadDataType, ConvData, ADest) then begin
    FLastError := CreateError(fpErrCanNotReadMemAtAddr, [ASourceLocation.Address]);
    exit;
  end;
  assert(ConvData.DestSize <= ADestSize, 'TFpDbgMemManager.ReadMemory: ConvData.DestSize <= ADestSize');

  // SourceFullSize: excluding any size needed for BitOffset
  SourceFullSize := ConvData.SourceFullSize;
  if (SourceFullSize > TMP_MEM_SIZE) and (SourceFullSize > ConvData.DestSize) then begin
    // The un-shifted (bit-offset) result must fully fit in either ADest or FTmpMem
    FLastError := CreateError(fpInternalErrFailedReadMem);
    exit;
  end;

  (* - If SourceFullSize does not fit into ADest,
       then FinishTargetRead *MUST* copy the desired part
     - If SourceFullSize is smaller than ADest,
       then targetconverter *MUST* fill/zero/compute the missing data.
       The read data will be alligned ot the first (smallest address) byte.
     - targetconverter MUST treat FTmpMem as read-only
  *)

  BitOffset := ConvData.SourceLocation.BitOffset;
  SourceExtraSize := (BitOffset + ConvData.SourceSize.BitSize + 7) div 8;

  case ASourceLocation.MType of
    mlfTargetMem, mlfSelfMem: begin
      assert(BitOffset < 8, 'TFpDbgMemManager.ReadMemory: BitOffset < 8');
      if QWord(ConvData.SourceSize.Size) > high(SourceReadSize) - SourceExtraSize then begin
        // bigger than max read size
        FLastError := CreateError(fpErrCanNotReadMemAtAddr, [ASourceLocation.Address]);
        exit;
      end;
      SourceReadSize := ConvData.SourceSize.Size + SourceExtraSize;
      // TODO: separate check for selfmem // requires selfmem to have a size field
      if (SourceReadSize > High(TDbgPtr) - ConvData.SourceLocation.Address) or
         ( (SourceReadSize > ConvData.DestSize) and ((SourceReadSize - ConvData.DestSize) > TMP_MEM_SIZE) )
      then begin
        FLastError := CreateError(fpErrCanNotReadMemAtAddr, [ASourceLocation.Address]);
        exit;
      end;

      case ASourceLocation.MType of
        mlfTargetMem: begin
            ReadData2 := nil;
            if SourceReadSize <= ConvData.DestSize then begin
              // full read to ADest
              ReadData := ADest;
              if mmfPartialRead in AFlags then
                Result := CacheManager.ReadMemory(ConvData.SourceLocation.Address, SourceReadSize, ADest, FPartialReadResultLenght)
              else
                Result := CacheManager.ReadMemory(ConvData.SourceLocation.Address, SourceReadSize, ADest);
            end
            else
            if SourceReadSize <= TMP_MEM_SIZE then begin
              // full read to FTmpMem;
              // This is the ONLY read that has ReadData <> ADest
              // *** FinishTargetRead must copy the data ***
              ReadData := @FTmpMem[0];
              // TODO: partial reads for bit shifting?
              Result := CacheManager.ReadMemory(ConvData.SourceLocation.Address, SourceReadSize, ReadData);
            end
            else begin
              // SPLIT read to ADest/FTmpMem;
              // BitOffset must be none zero, otherwise the data must fully fit in either ADest or FTmpMem
              // *** BitShift will copy the date into ADest ***
              assert(BitOffset <> 0, 'TFpDbgMemManager.ReadMemory: BitOffset <> 0');
              ReadData := ADest;
              ReadData2 := @FTmpMem[0];
              // TODO: partial reads for bit shifting?
              Result := CacheManager.ReadMemory(ConvData.SourceLocation.Address, ConvData.DestSize, ADest);
              if Result then
                Result := CacheManager.ReadMemory(ConvData.SourceLocation.Address + ConvData.DestSize, SourceReadSize - ConvData.DestSize, ReadData2);
            end;

            if Result and (BitOffset <> 0) then begin
              if (ReadData <> ADest) then begin
                // Read to FTmpMem only
                if (SourceFullSize <= ConvData.DestSize) then begin
                  BitShiftMem(ReadData, ADest, SourceReadSize, SourceFullSize, BitOffset);
                  ReadData := ADest;
                end
                else
                  BitShiftMem(ReadData, ReadData, SourceReadSize, SourceFullSize, BitOffset);
              end
              else
                // Read to ADest or SPLIT
                BitShiftMem(ReadData, ReadData, ConvData.DestSize, ConvData.DestSize, BitOffset);

              if ReadData2 <> nil then begin
                // SPLIT read; ReadData=ADest
                // Since SourceReadSize can have max 1 extra byte => there can only be one byte; which must fit into ADest after shifting BitOffset
                PByte(ADest+ConvData.DestSize-1)^ := Byte(PByte(ADest+ConvData.DestSize-1)^ or
                  (PByte(ReadData2)^ shl (8 - BitOffset) ));
              end;
            end;
            // ReadData is now a pointer to the FULL data. Either in ADest or FTmpMem

          end;
        mlfSelfMem: begin // Can be cached TargetMem, or can be constant data from dwarf
            try // accessinge SelfMem can fail, because there is on SelfmMem.Length that can be checked
              Result := True;
              if BitOffset <> 0 then begin
                // BitShift will copy the data
                if (SourceFullSize <= ConvData.DestSize) then
                  ReadData := @ConvData.SourceLocation.Address
                else
                  ReadData := @FTmpMem[0];
                BitShiftMem(@ConvData.SourceLocation.Address, ReadData, SourceReadSize, SourceFullSize, BitOffset)
              end
              else begin
                // no BitShift
                ReadData := ADest;
                if SourceFullSize > ConvData.DestSize then
                  ReadData := @ConvData.SourceLocation.Address  // ReadData has to be read-only // FinishTargetRead must copy the data
                else
                  move(Pointer(ConvData.SourceLocation.Address)^, ADest^, SourceFullSize);
              end;
            except
              Result := False;
            end;
          end;
      end;
    end;

    mlfConstant, mlfConstantDeref, mlfTargetRegister:
      begin
        If (BitOffset <> 0) or (not IsByteSize(ConvData.SourceSize)) then begin
          // Not yet supported
          FLastError := CreateError(fpErrCanNotReadMemAtAddr, [ConvData.SourceLocation.Address]);
          Result := False;
          exit;
        end;

        case ASourceLocation.MType of
          mlfConstant, mlfConstantDeref: begin
              TmpVal := ConvData.SourceLocation.Address;
              SourceReadSize := SizeOf(ConvData.SourceLocation.Address);
            end;
          mlfTargetRegister: begin
              SourceReadSize := FMemReader.RegisterSize(Cardinal(ConvData.SourceLocation.Address));
              if SourceReadSize = 0 then
                exit; // failed
              if not FMemReader.ReadRegister(Cardinal(ConvData.SourceLocation.Address), TmpVal, AContext) then begin
                FLastError := CreateError(fpErrFailedReadRegister);
                exit; // failed
              end
            end;
        end;

        if SourceReadSize < FPartialReadResultLenght then
          FPartialReadResultLenght := SourceReadSize;

        if SourceReadSize > ConvData.SourceSize.Size then
          SourceReadSize := ConvData.SourceSize.Size
        else
        if SourceReadSize <= ConvData.SourceSize.Size then begin
          ConvData.SourceSize         := SizeVal(SourceReadSize);
          ConvData.SourceFullSize     := SourceReadSize;
        end;

        ReadData := @TmpVal;

        if SizeOf(TmpVal) <> SourceReadSize then
          // TODO: only needed if ADestSize > SourceReadSize ?
          // Maybe do that after Move to ADest? // Maybe as part of FinishTargetRead ?
          if not FSelfMemConvertor.AdjustIntPointer(ReadData, SizeOf(TmpVal), SourceReadSize) then begin
            FLastError := CreateError(fpInternalErrFailedReadMem);
            exit;
          end;


        if SourceReadSize <= ConvData.DestSize then begin
          move(ReadData^, ADest^, Min(SizeOf(TmpVal), Int64(ConvData.DestSize))); // Little Endian only
          ReadData := ADest;
        end;

        Result := True;
      end;
  end;

  if Result then
    Result := TargetMemConvertor.FinishTargetRead(AReadDataType, ConvData, ReadData, ADest)
  else
    TargetMemConvertor.FailedTargetRead(ConvData);

  if (not Result) and (not IsError(FLastError)) then
    FLastError := CreateError(fpInternalErrFailedReadMem);
end;

function TFpDbgMemManager.WriteMemory(AReadDataType: TFpDbgMemReadDataType;
  const ADestLocation: TFpDbgMemLocation; const ADestSize: TFpDbgValueSize; const ASource: Pointer;
  const ASourceSize: QWord; AContext: TFpDbgLocationContext; const AFlags: TFpDbgMemManagerFlags
  ): Boolean;
var
  TmpVal: TDbgPtr;
  BitOffset: Integer;
  DestWriteSize: QWord;
begin
  Result := False;
  DebugLn(FPDBG_VERBOSE_MEM, ['$WriteMem: ', dbgs(AReadDataType),' ', dbgs(ADestLocation), ' ', dbgs(ADestSize), ' Source ', ASource]);
  assert(AContext<>nil, 'TFpDbgMemManager.WriteMemory: AContext<>nil');

  if (ADestLocation.MType in [mlfInvalid, mlfUninitialized]) or
     (ADestSize <= 0)
  then begin
    FLastError := CreateError(fpInternalErrCanNotWriteInvalidMem);
    exit;
  end;

  FLastError := NoError;

  // ToDo: Use a TargetMemConverter

  BitOffset := ADestLocation.BitOffset;
  //DestExtraSize := (BitOffset + ADestSize.BitSize + 7) div 8;

  case ADestLocation.MType of
    // ToDo: Add the ability to write to memory
    mlfTargetRegister:
      begin
        If (BitOffset <> 0) or (not IsByteSize(ADestSize)) then begin
          // Not yet supported
          FLastError := CreateError(fpErrCanNotWriteMemAtAddr, [ADestLocation.Address]);
          Result := False;
          exit;
        end;

        DestWriteSize := FMemReader.RegisterSize(Cardinal(ADestLocation.Address));
        if DestWriteSize = 0 then
          exit; // failed

        if SizeOf(TmpVal) < DestWriteSize then
          Exit; // failed

        move(ASource^, TmpVal, Min(SizeOf(TmpVal), Int64(ASourceSize))); // Little Endian only

        if (DestWriteSize < 8) and
           (TmpVal and (QWord($ffffffffffffffff) << (DestWriteSize*8)) <> 0) and
           ( (AReadDataType <> rdtSignedInt) or
             (TmpVal and (QWord($ffffffffffffffff) << (DestWriteSize*8)) <> (QWord($ffffffffffffffff) << (DestWriteSize*8)) )
           )
        then
          exit; // failed

        if not FMemReader.WriteRegister(Cardinal(ADestLocation.Address), TmpVal, AContext) then
          exit; // failed

        Result := True;
      end;
    mlfTargetMem:
      begin
        if (BitOffset = 0) and (ADestSize.BitSize = 0) and (ADestSize.Size > 0) then begin
          FMemReader.WriteMemory(LocToAddr(ADestLocation), SizeToFullBytes(ADestSize), ASource);
        end;
      end;
    mlfSelfMem:
      begin
        DestWriteSize := ADestSize.Size;
        if (BitOffset = 0) and (ADestSize.BitSize = 0) and
           (DestWriteSize > 0) and
           (FStartWirteableSelfMem <> 0) and
           (ADestLocation.Address >= FStartWirteableSelfMem) and
           (ADestLocation.Address + DestWriteSize <= FStartWirteableSelfMem + FLenWirteableSelfMem)
        then begin
          if ASourceSize < DestWriteSize then
            DestWriteSize := ASourceSize;
          move(ASource^, Pointer(PtrUint(ADestLocation.Address))^, ADestSize.Size);
          Result := True;
        end;
      end;
  end;

  if (not Result) and (not IsError(FLastError)) then
    FLastError := CreateError(fpErrFailedWriteMem);
end;



procedure TFpDbgMemManager.SetCacheManager(ACacheMgr: TFpDbgMemCacheManagerBase);
begin
  if FCacheManager = ACacheMgr then exit;
  FCacheManager.Free;
  FCacheManager := ACacheMgr;
  if FCacheManager <> nil then
    FCacheManager.FMemReader := FMemReader;
end;

constructor TFpDbgMemManager.Create(AMemReader: TFpDbgMemReaderBase;
  AMemConvertor: TFpDbgMemConvertor; AMemModel: TFpDbgMemModel);
begin
  FMemLimits := TFpDbgMemLimits.Create;
  FMemReader := AMemReader;
  FTargetMemConvertor := AMemConvertor;
  FSelfMemConvertor := AMemConvertor;
  FMemModel := AMemModel;
end;

constructor TFpDbgMemManager.Create(AMemReader: TFpDbgMemReaderBase; ATargenMemConvertor,
  ASelfMemConvertor: TFpDbgMemConvertor; AMemModel: TFpDbgMemModel);
begin
  FMemLimits := TFpDbgMemLimits.Create;
  FMemReader := AMemReader;
  FTargetMemConvertor := ATargenMemConvertor;
  FSelfMemConvertor := ASelfMemConvertor;
  FMemModel := AMemModel;
end;

destructor TFpDbgMemManager.Destroy;
begin
  SetCacheManager(nil);
  inherited Destroy;
  FMemLimits.Free;
end;

procedure TFpDbgMemManager.ClearLastError;
begin
  FLastError := NoError;
end;

procedure TFpDbgMemManager.SetWritableSeflMem(AStartWirteableSelfMem: TDBGPtr;
  ALenWirteableSelfMem: Cardinal);
begin
  FStartWirteableSelfMem := AStartWirteableSelfMem;
  FLenWirteableSelfMem := ALenWirteableSelfMem;
end;

procedure TFpDbgMemManager.ClearWritableSeflMem;
begin
  FStartWirteableSelfMem := 0;
  FLenWirteableSelfMem := 0;
end;

function TFpDbgMemManager.RegisterSize(ARegNum: Cardinal): Integer;
begin
  Result := FMemReader.RegisterSize(ARegNum);
end;

function TFpDbgMemManager.RegisterNumber(ARegName: String; out ARegNum: Cardinal
  ): Boolean;
begin
  Result := FMemReader.RegisterNumber(ARegName, ARegNum);
end;

function TFpDbgMemManager.ReadMemoryEx(
  const ASourceLocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr;
  ASize: TFpDbgValueSize; ADest: Pointer; AContext: TFpDbgLocationContext
  ): Boolean;
begin
  assert(AContext<>nil, 'TFpDbgMemManager.ReadMemoryEx: AContext<>nil');
  FLastError := NoError;
  if (ASourceLocation.BitOffset <> 0) then begin
    // Not supported to read at bit offset
    FLastError := CreateError(fpInternalErrFailedReadMem);
    Result := False;
    exit;
  end;

  // AnAddressSpace is ignored, when not actually reading from target address
  case ASourceLocation.MType of
    mlfTargetMem: Result := FMemReader.ReadMemoryEx(ASourceLocation.Address, AnAddressSpace, SizeToFullBytes(ASize), ADest);
    else
      Result := ReadMemory(rdtRawRead, ASourceLocation, ASize, ADest, ASize.Size, AContext);
  end;
  if (not Result) and (not IsError(FLastError)) then
    FLastError := CreateError(fpInternalErrFailedReadMem);
end;

function TFpDbgMemManager.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr;
  AContext: TFpDbgLocationContext): Boolean;
begin
  assert(AContext<>nil, 'TFpDbgMemManager.ReadRegister: AContext<>nil');
  FLastError := NoError;

  // TODO: If stackframe <> 0 then get the register internally (unroll stack)
  Result := FMemReader.ReadRegister(ARegNum, AValue, AContext);
  if not Result then
    FLastError := CreateError(fpErrFailedReadRegister);
end;

function TFpDbgMemManager.ReadRegisterAsAddress(ARegNum: Cardinal; out
  AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean;
begin
  Result := ReadRegister(ARegNum, AValue, AContext);
end;

function TFpDbgMemManager.SetLength(var ADest: TByteDynArray; ALength: Int64
  ): Boolean;
begin
  Result := False;
  if (FMemLimits.MaxMemReadSize > 0) and (ALength > FMemLimits.MaxMemReadSize) then begin
    FLastError := CreateError(fpErrReadMemSizeLimit);
    exit;
  end;
  Result := True;
  System.SetLength(ADest, ALength);
end;

function TFpDbgMemManager.SetLength(var ADest: RawByteString; ALength: Int64
  ): Boolean;
begin
  Result := False;
  if (ALength < 0) or
     ( (FMemLimits.MaxMemReadSize > 0) and (ALength > FMemLimits.MaxMemReadSize) )
  then begin
    FLastError := CreateError(fpErrReadMemSizeLimit);
    exit;
  end;
  Result := True;
  System.SetLength(ADest, ALength);
end;

function TFpDbgMemManager.SetLength(var ADest: AnsiString; ALength: Int64
  ): Boolean;
begin
  Result := False;
  if (ALength < 0) or
     ( (FMemLimits.MaxMemReadSize > 0) and (ALength > FMemLimits.MaxMemReadSize) )
  then begin
    FLastError := CreateError(fpErrReadMemSizeLimit);
    exit;
  end;
  Result := True;
  System.SetLength(ADest, ALength);
end;

function TFpDbgMemManager.SetLength(var ADest: WideString; ALength: Int64
  ): Boolean;
begin
  Result := False;
  if (ALength < 0) or
     ( (FMemLimits.MaxMemReadSize > 0) and (ALength * 2 > FMemLimits.MaxMemReadSize) )
  then begin
    FLastError := CreateError(fpErrReadMemSizeLimit);
    exit;
  end;
  Result := True;
  System.SetLength(ADest, ALength);
end;

function TFpDbgMemManager.CheckDataSize(ASize: Int64): Boolean;
begin
  Result := False;
  if (FMemLimits.MaxMemReadSize > 0) and (ASize > FMemLimits.MaxMemReadSize) then begin
    FLastError := CreateError(fpErrReadMemSizeLimit);
    exit;
  end;
  Result := True;
end;

function TFpDbgMemManager.ReadPChar(const ALocation: TFpDbgMemLocation;
  AMaxChars: Int64; out AValue: AnsiString; NoTrimToZero: Boolean): Boolean;
var
  i: QWord;
begin
  Result := False;
  if not MemModel.IsReadableLocation(ALocation) then begin
    FLastError := CreateError(fpInternalErrFailedReadMem);
    exit;
  end;
  if AMaxChars <= 0 then
    AMaxChars := DEF_MAX_PCHAR_LEN;

  i := MemLimits.MaxNullStringSearchLen;
  if i = 0 then
    i := MemLimits.MaxMemReadSize;
  if (i > 0) and (AMaxChars > i) then
    AMaxChars := i;

  SetLength(AValue, AMaxChars);
  if ReadMemory(rdtRawRead, ALocation, SizeVal(AMaxChars), @AValue[1], AMaxChars, nil, [mmfPartialRead]) then begin
    Result := True;
    i := PartialReadResultLenght;
    SetLength(AValue, i);
    if not NoTrimToZero then begin
      i := pos(#0, AValue);
      if i > 0 then
        SetLength(AValue, i-1);
    end;
    exit;
  end
end;

function TFpDbgMemManager.ReadPWChar(const ALocation: TFpDbgMemLocation;
  AMaxChars: Int64; out AValue: WideString): Boolean;
var
  i: QWord;
begin
  Result := False;
  if not MemModel.IsReadableLocation(ALocation) then begin
    FLastError := CreateError(fpInternalErrFailedReadMem);
    exit;
  end;
  if AMaxChars <= 0 then
    AMaxChars := DEF_MAX_PCHAR_LEN;

  i := MemLimits.MaxNullStringSearchLen;
  if i = 0 then
    i := MemLimits.MaxMemReadSize div 2;
  if (i > 0) and (AMaxChars > i) then
    AMaxChars := i;

  SetLength(AValue, AMaxChars);
  if ReadMemory(rdtRawRead, ALocation, SizeVal(AMaxChars*2), @AValue[1], AMaxChars*2, nil, [mmfPartialRead]) then begin
    Result := True;
    i := PartialReadResultLenght div 2;
    SetLength(AValue, i);
    i := pos(#0, AValue);
    if i > 0 then
      SetLength(AValue, i-1);
    exit;
  end
end;


initialization
  FPDBG_VERBOSE_MEM := DebugLogger.FindOrRegisterLogGroup('FPDBG_VERBOSE_MEM' {$IFDEF FPDBG_VERBOSE_MEM} , True {$ENDIF} );

end.

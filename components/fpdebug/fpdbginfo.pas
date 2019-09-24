unit FpDbgInfo;
(*
  About TFpValue and TFpSymbol

  * TFpSymbol
    Represents a Symbol or Identifier (stType or stValue)

  * TFpValue
    Holds the Value of a Symbol according to its type.

  TFpSymbol should not hold any Data, except for information that is in the
  debug info (dwarf/stabs).
  All Data read from the target must be in TFpValue.
  Target adta includes Address (can be indirect via ref or pointer, Size and
  Boundaries (Sub range / Array).

  This means that TFpSymbol (stType or stValue) should be re-usable. There can
  be multiple TFpValue for each TFpSymbol. (even for stValue, as in an
  Array the Symbol itself is repeated / Array of record: the same member occurs
  over and over)

  ---
  A Variable value in the target typically consists of:
  - TFpSymbol (stValue)
  - TFpSymbol (stType)
  - TFpValue

*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbgIntfBaseTypes, FpDbgLoader, FpdMemoryTools, FpErrorMessages,
  LazLoggerBase, LazClasses;

type
  { TFpDbgCircularRefCountedObject }

  TFpDbgCircularRefCountedObject = class(TRefCountedObject)
  private
    FCircleRefCount: Integer;
  protected
    (* InOrder to activate, and use an interited class must override
       DoReferenceAdded; and DoReferenceReleased;
       And Point then to
       DoPlainReferenceAdded; and DoPlainReferenceReleased;
    *)
    procedure DoPlainReferenceAdded; inline;
    procedure DoPlainReferenceReleased; inline;

    // Receive the *strong* reference (always set)
    // The circle back ref will only be set, if this is also referenced by others
    procedure AddCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
    procedure ReleaseCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};

    procedure MakePlainRefToCirclular;
    procedure MakeCirclularRefToPlain;

    function  CircleBackRefsActive: Boolean; inline;
    procedure CircleBackRefActiveChanged({%H-}NewActive: Boolean); virtual;
  end;

  { TFpDbgCircularRefCntObjList }

  TFpDbgCircularRefCntObjList = class(TRefCntObjList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TDbgSymbolType = (
    stNone,
    stValue,  // The symbol has a value (var, field, function, procedure (value is address of func/proc, so it can be called)
    stType    // The Symbol is a type (including proc/func declaration / without DW_AT_low_pc)
  );

  TDbgSymbolMemberVisibility =(
    svPrivate,
    svProtected,
    svPublic
  );

  TDbgSymbolFlag =(
    sfSubRange,     // This is a subrange, e.g 3..99
    sfDynArray,     // skArray is known to be a dynamic array
    sfStatArray,    // skArray is known to be a static array
    sfVirtual,      // skProcedure,skFunction:  virtual function (or overriden)
    sfParameter,    // Parameter to a function
    // unimplemented:
    sfInternalRef,  // TODO: (May not always be present) Internal ref/pointer e.g. var/constref parameters
    sfConst,         // The sym is a constant and cannot be modified
    sfVar,
    sfOut,
    sfpropGet,
    sfPropSet,
    sfPropStored
  );
  TDbgSymbolFlags = set of TDbgSymbolFlag;

  TFpSymbolField = (
    sfiName, sfiKind, sfiSymType, sfiAddress, sfiSize,
    sfiTypeInfo, sfiMemberVisibility,
    sfiForwardToSymbol
  );
  TFpSymbolFields = set of TFpSymbolField;

  TFpSymbol = class;

  TFpValueFieldFlag = (
    // svfAddress, svfDataAddress this symbol does have an address, but it may still be nil
    svfAddress, svfSize, svfSizeOfPointer,
    svfDataAddress, svfDataSize, svfDataSizeOfPointer,
    svfInteger, svfCardinal, svfFloat,
    svfString, svfWideString,
    svfBoolean,
    svfIdentifier,   // returned via AsString: a named value (enum, set-member)
    svfMembers,
    //svfParent, // TODO: for members, get the parent (object/record-fields, enum/set-members
    svfOrdinal       // AsCardinal ruturns an ordinal value, but the value is not represented as cardinal (e.g. bool, enum)
                     // if size > 8, then ordinal (if present) is based on a part only
  );
  TFpValueFieldFlags = set of TFpValueFieldFlag;

  { TFpValue }

  TFpValue = class(TFpDbgCircularRefCountedObject)
  protected
    function GetKind: TDbgSymbolKind; virtual;
    function GetFieldFlags: TFpValueFieldFlags; virtual;

    function GetAsBool: Boolean;  virtual;
    function GetAsCardinal: QWord; virtual;
    function GetAsInteger: Int64; virtual;
    function GetAsString: AnsiString; virtual;
    function GetAsWideString: WideString; virtual;
    function GetAsFloat: Extended; virtual;

    function GetAddress: TFpDbgMemLocation;  virtual;
    function GetSize: Integer;  virtual;  // returns -1, if not available
    function GetDataAddress: TFpDbgMemLocation;  virtual;
    function GetDataSize: Integer;  virtual;

    function GetHasBounds: Boolean; virtual;
    function GetOrdHighBound: Int64; virtual;
    function GetOrdLowBound: Int64; virtual;

    function GetMember({%H-}AIndex: Int64): TFpValue; virtual;
    function GetMemberByName({%H-}AIndex: String): TFpValue; virtual;
    function GetMemberCount: Integer; virtual;
    function GetIndexType({%H-}AIndex: Integer): TFpSymbol; virtual;
    function GetIndexTypeCount: Integer; virtual;
    function GetMemberCountEx(const AIndex: array of Int64): Integer; virtual;
    function GetMemberEx(const AIndex: Array of Int64): TFpValue; virtual;

    function GetDbgSymbol: TFpSymbol; virtual;
    function GetTypeInfo: TFpSymbol; virtual;
    function GetContextTypeInfo: TFpSymbol; virtual;

    function GetLastError: TFpError; virtual;
  public
    constructor Create;
    property RefCount;

    // Kind: determines which types of value are available
    property Kind: TDbgSymbolKind read GetKind;
    property FieldFlags: TFpValueFieldFlags read GetFieldFlags;

    property AsInteger: Int64 read GetAsInteger;
    property AsCardinal: QWord read GetAsCardinal;
    property AsBool: Boolean read GetAsBool;
    property AsString: AnsiString read GetAsString;
    property AsWideString: WideString read GetAsWideString;
    property AsFloat: Extended read GetAsFloat;

    (* * Address/Size
         Address of the variable (as returned by the "@" address of operator
       * DataAddress/DataSize
         Address of Data, if avail and diff from Address (e.g. String, TObject, DynArray, ..., BUT NOT record)
         Otherwise same as Address/Size
         For pointers, this is the address of the pointed-to data
    *)
    property Address: TFpDbgMemLocation read GetAddress;
    property Size: Integer read GetSize;
    property DataAddress: TFpDbgMemLocation read GetDataAddress; //
    property DataSize: Integer read GetDataSize;

    property HasBounds: Boolean  read GetHasBounds;
    property OrdLowBound: Int64  read GetOrdLowBound;   // need typecast for QuadWord
    property OrdHighBound: Int64 read GetOrdHighBound;  // need typecast for QuadWord
    // memdump
  public
    function GetTypeCastedValue(ADataVal: TFpValue): TFpValue; virtual; // only if Symbol is a type
// base class? Or Member includes member from base
    (* Member:
       * skClass, skStructure:
           stType: it excludes BaseClass (TODO: decide?)
           stValue: includes
       * skSet
           stType: all members
           stValue: only members set in value (Only impremented for DbgSymbolValue)
       * skArray: (differs from TFpSymbol)
         The values. The type of each Index-dimension is avail via IndexType
       * skPointer: deref the pointer, with index (0 = normal deref)
       NOTE: Values returned by Member/MemberByName are volatile.
             They maybe released or changed when Member is called again.
             To keep a returned Value a reference can be added (AddReference)
    *)
    property MemberCount: Integer read GetMemberCount;
    property Member[AIndex: Int64]: TFpValue read GetMember;
    property MemberByName[AIndex: String]: TFpValue read GetMemberByName; // Includes inheritance
    //  For Arrays (TODO pointers) only, the values stored in the array
    property IndexTypeCount: Integer read GetIndexTypeCount;
    property IndexType[AIndex: Integer]: TFpSymbol read GetIndexType;

    (* DbgSymbol: The TFpSymbol from which this value came, maybe nil.
                  Maybe a stType, then there is no Value *)
    property DbgSymbol: TFpSymbol read GetDbgSymbol;
    property TypeInfo: TFpSymbol read GetTypeInfo;
    property ContextTypeInfo: TFpSymbol read GetContextTypeInfo; // For members, the class in which this member is declared

    property LastError: TFpError read GetLastError;
  end;

  { TFpValueConstNumber }

  TFpValueConstNumber = class(TFpValue)
  private
    FValue: QWord;
    FSigned: Boolean;
  protected
    property Value: QWord read FValue write FValue;
    property Signed: Boolean read FSigned write FSigned;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsInteger: Int64; override;
  public
    constructor Create(AValue: QWord; ASigned: Boolean = True);
  end;

  { TFpValueConstChar }

  TFpValueConstChar = class(TFpValue) // skChar / Not for strings
  private
    FValue: String;
  protected
    property Value: String read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsString: AnsiString; override;
  public
    constructor Create(AValue: AnsiString);
  end;

  { TFpValueConstWideChar }

  TFpValueConstWideChar = class(TFpValue) // skChar / Not for strings
  private
    FValue: String;
  protected
    property Value: String read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsString: AnsiString; override;
  public
    constructor Create(AValue: AnsiString);
  end;

  { TFpValueConstFloat }

  TFpValueConstFloat = class(TFpValue)
  private
    FValue: Extended;
  protected
    property Value: Extended read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsFloat: Extended; override;
  public
    constructor Create(AValue: Extended);
  end;

  { TFpValueConstBool}

  TFpValueConstBool = class(TFpValue)
  private
    FValue: Boolean;
  protected
    property Value: Boolean read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsBool: Boolean; override;
    function GetAsCardinal: QWord; override;
  public
    constructor Create(AValue: Boolean);
  end;

  { TFpValueConstAddress }

  TFpValueConstAddress = class(TFpValue)
  private
    FAddress: TFpDbgMemLocation;
  protected
    property Address: TFpDbgMemLocation read FAddress write FAddress;
    function GetKind: TDbgSymbolKind; override; // skAddress
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAddress: TFpDbgMemLocation; override;
  public
    constructor Create(AnAddress: TFpDbgMemLocation);
  end;

  { TFpValueTypeDefinition }

  TFpValueTypeDefinition = class(TFpValue)
  private
    FSymbol: TFpSymbol; // stType
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetDbgSymbol: TFpSymbol; override;
  public
    constructor Create(ASymbol: TFpSymbol); // Only for stType
    destructor Destroy; override;
  end;

  { TFpSymbol }

  TFpSymbol = class(TFpDbgCircularRefCountedObject)
  private
    FEvaluatedFields: TFpSymbolFields;
    FLastError: TFpError;

    // Cached fields
    FName: String;
    FKind: TDbgSymbolKind;
    FSymbolType: TDbgSymbolType;
    FAddress: TFpDbgMemLocation;
    FSize: Integer;
    FTypeInfo: TFpSymbol;
    FMemberVisibility: TDbgSymbolMemberVisibility; // Todo: not cached

    function GetSymbolType: TDbgSymbolType; inline;
    function GetKind: TDbgSymbolKind; inline;
    function GetName: String; inline;
    function GetSize: Integer; inline;
    function GetAddress: TFpDbgMemLocation; inline;
    function GetTypeInfo: TFpSymbol; inline;
    function GetMemberVisibility: TDbgSymbolMemberVisibility; inline;
  protected
    function  GetLastError: TFpError; virtual;
    procedure SetLastError(AnError: TFpError);
    // NOT cached fields
    function GetChild({%H-}AIndex: Integer): TFpSymbol; virtual;
    function GetColumn: Cardinal; virtual;
    function GetFile: String; virtual;
    function GetFlags: TDbgSymbolFlags; virtual;
    function GetLine: Cardinal; virtual;
    function GetParent: TFpSymbol; virtual;

    function GetValueObject: TFpValue; virtual;
    function GetHasOrdinalValue: Boolean; virtual;
    function GetOrdinalValue: Int64; virtual;

    function GetNestedSymbol({%H-}AIndex: Int64): TFpSymbol; virtual;
    function GetNestedSymbolByName({%H-}AIndex: String): TFpSymbol; virtual;
    function GetNestedSymbolCount: Integer; virtual;
  protected
    property EvaluatedFields: TFpSymbolFields read FEvaluatedFields write FEvaluatedFields;
    // Cached fields
    procedure SetName(AValue: String); inline;
    procedure SetKind(AValue: TDbgSymbolKind); inline;
    procedure SetSymbolType(AValue: TDbgSymbolType); inline;
    procedure SetAddress(AValue: TFpDbgMemLocation); inline;
    procedure SetSize(AValue: Integer); inline;
    procedure SetTypeInfo(ASymbol: TFpSymbol); inline;
    procedure SetMemberVisibility(AValue: TDbgSymbolMemberVisibility); inline;

    procedure KindNeeded; virtual;
    procedure NameNeeded; virtual;
    procedure SymbolTypeNeeded; virtual;
    procedure AddressNeeded; virtual;
    procedure SizeNeeded; virtual;
    procedure TypeInfoNeeded; virtual;
    procedure MemberVisibilityNeeded; virtual;
    //procedure Needed; virtual;
  public
    constructor Create(const AName: String);
    constructor Create(const AName: String; AKind: TDbgSymbolKind; AAddress: TFpDbgMemLocation);
    destructor Destroy; override;
    // Basic info
    property Name:       String read GetName;
    property SymbolType: TDbgSymbolType read GetSymbolType;
    property Kind:       TDbgSymbolKind read GetKind;
    // Memory; Size is also part of type (byte vs word vs ...)
    // HasAddress // (register does not have)
    property Address:    TFpDbgMemLocation read GetAddress;    // used by Proc/func
    property Size:       Integer read GetSize; // In Bytes
    // TypeInfo used by
    // stValue (Variable): Type
    // stType: Pointer: type pointed to / Array: Element Type / Func: Result / Class: itheritance
    property TypeInfo: TFpSymbol read GetTypeInfo;
    // Location
    property FileName: String read GetFile;
    property Line: Cardinal read GetLine;
    property Column: Cardinal read GetColumn;
    // Methods for structures (record / class / enum)
    //         array: each member represents an index (enum or subrange) and has low/high bounds
    property MemberVisibility: TDbgSymbolMemberVisibility read GetMemberVisibility;
    property NestedSymbolCount: Integer read GetNestedSymbolCount;
    (* Member:
       * skClass, skStructure:
           stType: it excludes BaseClass (TODO: decide?)
           includes
       * skSet
           stType: all members
           stValue: only members set in value (Only impremented for DbgSymbolValue)
       * skArray:
         The type of each Index-dimension
         The count is the amount of dimensions
       NOTE: Values returned by Member/MemberByName are volatile.
             They maybe released or changed when Member is called again.
             To keep a returned Value a reference can be added (AddReference)
    *)
    property NestedSymbol[AIndex: Int64]: TFpSymbol read GetNestedSymbol;
    property NestedSymbolByName[AIndex: String]: TFpSymbol read GetNestedSymbolByName; // Includes inheritance
    //
    property Flags: TDbgSymbolFlags read GetFlags;
    property Parent: TFpSymbol read GetParent; deprecated;
    // for Subranges  // Type-Symbols only?
    // TODO: flag bounds as cardinal if needed
    function GetValueBounds(AValueObj: TFpValue; out ALowBound, AHighBound: Int64): Boolean; virtual;
    function GetValueLowBound(AValueObj: TFpValue; out ALowBound: Int64): Boolean; virtual;
    function GetValueHighBound(AValueObj: TFpValue; out AHighBound: Int64): Boolean; virtual;

    // VALUE
    property Value: TFpValue read GetValueObject; //deprecated 'rename / create';
    property HasOrdinalValue: Boolean read GetHasOrdinalValue;
    property OrdinalValue: Int64 read GetOrdinalValue;   //deprecated 'xxxx'; // need typecast for QuadWord

    // TypeCastValue| only fon stType symbols, may return nil
    // Returns a reference to caller / caller must release
    function TypeCastValue({%H-}AValue: TFpValue): TFpValue; virtual;

    property LastError: TFpError read GetLastError; experimental;
  end;

  { TFpSymbolForwarder }

  TFpSymbolForwarder = class(TFpSymbol)
  private
    FForwardToSymbol: TFpSymbol;
  protected
    procedure SetForwardToSymbol(AValue: TFpSymbol); inline;
    procedure ForwardToSymbolNeeded; virtual;
    function  GetForwardToSymbol: TFpSymbol; inline;
  protected
    function GetLastError: TFpError; override;
    procedure KindNeeded; override;
    procedure NameNeeded; override;
    procedure SymbolTypeNeeded; override;
    procedure SizeNeeded; override;
    procedure TypeInfoNeeded; override;
    procedure MemberVisibilityNeeded; override;

    function GetFlags: TDbgSymbolFlags; override;
    function GetValueObject: TFpValue; override;
    function GetHasOrdinalValue: Boolean; override;
    function GetOrdinalValue: Int64; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;
  public
    function GetValueBounds(AValueObj: TFpValue; out ALowBound, AHighBound: Int64): Boolean; override;
    function GetValueLowBound(AValueObj: TFpValue; out ALowBound: Int64): Boolean; override;
    function GetValueHighBound(AValueObj: TFpValue; out AHighBound: Int64): Boolean; override;
  end;

  { TFpDbgInfoContext }

  TFpDbgInfoContext = class(TFpDbgAddressContext)
  protected
    function GetSymbolAtAddress: TFpSymbol; virtual;
    function GetProcedureAtAddress: TFpValue; virtual;
    function GetMemManager: TFpDbgMemManager; virtual;
  public
    property SymbolAtAddress: TFpSymbol read GetSymbolAtAddress;
    property ProcedureAtAddress: TFpValue read GetProcedureAtAddress;
    // search this, and all parent context
    function FindSymbol(const {%H-}AName: String): TFpValue; virtual;
    property MemManager: TFpDbgMemManager read GetMemManager;
  end;

  { TDbgInfo }

  TDbgInfo = class(TObject)
  private
    FHasInfo: Boolean;
  protected
    procedure SetHasInfo;
  public
    constructor Create({%H-}ALoaderList: TDbgImageLoaderList); virtual;
    (* Context should be searched by Thread, and StackFrame. The Address can be
       derived from this.
       However a different Address may be froced.
       TODO: for now address may be needed, as stack decoding is not done yet
    *)
    function FindContext(AThreadId, AStackFrame: Integer; {%H-}AAddress: TDbgPtr = 0): TFpDbgInfoContext; virtual;
    function FindContext({%H-}AAddress: TDbgPtr): TFpDbgInfoContext; virtual; deprecated 'use FindContextFindContext(thread,stack,address)';
    function FindSymbol(const {%H-}AName: String): TFpSymbol; virtual; deprecated;
    function FindSymbol({%H-}AAddress: TDbgPtr): TFpSymbol; virtual; deprecated;
    property HasInfo: Boolean read FHasInfo;
    function GetLineAddresses(const AFileName: String; ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean; virtual;
    //property MemManager: TFpDbgMemReaderBase read GetMemManager write SetMemManager;
  end;

function dbgs(ADbgSymbolKind: TDbgSymbolKind): String; overload;

implementation

function dbgs(ADbgSymbolKind: TDbgSymbolKind): String;
begin
  Result := '';
  WriteStr(Result, ADbgSymbolKind);
end;

{ TFpValueConstChar }

function TFpValueConstChar.GetKind: TDbgSymbolKind;
begin
  Result := skChar;
end;

function TFpValueConstChar.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := [svfString]
end;

function TFpValueConstChar.GetAsString: AnsiString;
begin
  Result := Value;
end;

constructor TFpValueConstChar.Create(AValue: AnsiString);
begin
  inherited Create;
  FValue := AValue;
end;

{ TFpValueConstWideChar }

function TFpValueConstWideChar.GetKind: TDbgSymbolKind;
begin
  Result := skChar;
end;

function TFpValueConstWideChar.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := [svfString]
end;

function TFpValueConstWideChar.GetAsString: AnsiString;
begin
  Result := Value;
end;

constructor TFpValueConstWideChar.Create(AValue: AnsiString);
begin
  inherited Create;
  FValue := AValue;
end;

{ TFpDbgCircularRefCountedObject }

procedure TFpDbgCircularRefCountedObject.DoPlainReferenceAdded;
begin
  if (RefCount = FCircleRefCount + 1) then
    CircleBackRefActiveChanged(True);
end;

procedure TFpDbgCircularRefCountedObject.DoPlainReferenceReleased;
begin
  if (RefCount = FCircleRefCount) then
    CircleBackRefActiveChanged(False);
end;

procedure TFpDbgCircularRefCountedObject.AddCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
begin
  if CircleBackRefsActive then begin
    AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
    inc(FCircleRefCount);
  end
  else begin
    inc(FCircleRefCount);
    AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
  end;
end;

procedure TFpDbgCircularRefCountedObject.ReleaseCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
var
  i: Integer;
begin
  if self = nil then
    exit;
  Assert(FCircleRefCount > 0, 'ReleaseCirclularReference > 0');
  if CircleBackRefsActive then begin
    dec(FCircleRefCount);
    ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
  end
  else begin
    i := RefCount;
    ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
    if i > 1 then // if i was 1, then self is destroyed
      dec(FCircleRefCount);
  end;
end;

procedure TFpDbgCircularRefCountedObject.MakePlainRefToCirclular;
begin
  Assert(FCircleRefCount < RefCount, 'MakePlainRefToCirclular < max');
  inc(FCircleRefCount);
  if (RefCount = FCircleRefCount) then
    CircleBackRefActiveChanged(False);
end;

procedure TFpDbgCircularRefCountedObject.MakeCirclularRefToPlain;
begin
  Assert(FCircleRefCount > 0, 'MakeCirclularRefToPlain > 0');
  dec(FCircleRefCount);
  if (RefCount = FCircleRefCount + 1) then
    CircleBackRefActiveChanged(True);
end;

function TFpDbgCircularRefCountedObject.CircleBackRefsActive: Boolean;
begin
  Result := (RefCount > FCircleRefCount);
end;

procedure TFpDbgCircularRefCountedObject.CircleBackRefActiveChanged(NewActive: Boolean);
begin
  //
end;

{ TFpDbgCircularRefCntObjList }

procedure TFpDbgCircularRefCntObjList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  // Do NOT call inherited
  case Action of
    lnAdded:   TFpDbgCircularRefCountedObject(Ptr).AddCirclularReference;
    lnExtracted,
    lnDeleted: TFpDbgCircularRefCountedObject(Ptr).ReleaseCirclularReference;
  end;
end;

{ TDbgSymbolValue }

function TFpValue.GetAsString: AnsiString;
begin
  Result := '';
end;

function TFpValue.GetAsWideString: WideString;
begin
  Result := '';
end;

function TFpValue.GetDbgSymbol: TFpSymbol;
begin
  Result := nil;
end;

constructor TFpValue.Create;
begin
  inherited Create;
  AddReference;
end;

function TFpValue.GetTypeCastedValue(ADataVal: TFpValue): TFpValue;
begin
  assert(False, 'TFpValue.GetTypeCastedValue: False');
  Result := nil;
end;

function TFpValue.GetTypeInfo: TFpSymbol;
begin
  if (DbgSymbol <> nil) and (DbgSymbol.SymbolType = stValue) then
    Result := DbgSymbol.TypeInfo
  else
    Result := nil;
end;

function TFpValue.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := [];
end;

function TFpValue.GetIndexType(AIndex: Integer): TFpSymbol;
begin
  Result := nil;;
end;

function TFpValue.GetIndexTypeCount: Integer;
begin
  Result := 0;
end;

function TFpValue.GetMemberEx(const AIndex: array of Int64): TFpValue;
begin
  Result := nil;
end;

function TFpValue.GetMemberCountEx(const AIndex: array of Int64): Integer;
begin
  Result := 0;
end;

function TFpValue.GetAsFloat: Extended;
begin
  Result := 0;
end;

function TFpValue.GetContextTypeInfo: TFpSymbol;
begin
  Result := nil;
end;

function TFpValue.GetLastError: TFpError;
begin
  Result := NoError;
end;

function TFpValue.GetHasBounds: Boolean;
begin
  Result := False;
end;

function TFpValue.GetOrdHighBound: Int64;
begin
  Result := 0;
end;

function TFpValue.GetOrdLowBound: Int64;
begin
  Result := 0;
end;

function TFpValue.GetKind: TDbgSymbolKind;
begin
  Result := skNone;
end;

function TFpValue.GetMember(AIndex: Int64): TFpValue;
begin
  Result := nil;
end;

function TFpValue.GetMemberByName(AIndex: String): TFpValue;
begin
  Result := nil;
end;

function TFpValue.GetMemberCount: Integer;
begin
  Result := 0;
end;

function TFpValue.GetAddress: TFpDbgMemLocation;
begin
  Result := InvalidLoc;
end;

function TFpValue.GetDataAddress: TFpDbgMemLocation;
begin
  Result := Address;
end;

function TFpValue.GetDataSize: Integer;
begin
  Result := Size;
end;

function TFpValue.GetSize: Integer;
begin
  Result := -1;
end;

function TFpValue.GetAsBool: Boolean;
begin
  Result := False;
end;

function TFpValue.GetAsCardinal: QWord;
begin
  Result := 0;
end;

function TFpValue.GetAsInteger: Int64;
begin
  Result := 0;
end;

{ TPasParserConstNumberSymbolValue }

function TFpValueConstNumber.GetKind: TDbgSymbolKind;
begin
  if FSigned then
    Result := skInteger
  else
    Result := skCardinal;
end;

function TFpValueConstNumber.GetFieldFlags: TFpValueFieldFlags;
begin
  if FSigned then
    Result := [svfOrdinal, svfInteger]
  else
    Result := [svfOrdinal, svfCardinal];
end;

function TFpValueConstNumber.GetAsCardinal: QWord;
begin
  Result := FValue;
end;

function TFpValueConstNumber.GetAsInteger: Int64;
begin
  Result := Int64(FValue);
end;

constructor TFpValueConstNumber.Create(AValue: QWord; ASigned: Boolean);
begin
  inherited Create;
  FValue := AValue;
  FSigned := ASigned;
end;

{ TFpValueConstFloat }

function TFpValueConstFloat.GetKind: TDbgSymbolKind;
begin
  Result := skFloat;
end;

function TFpValueConstFloat.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := [svfFloat];
end;

function TFpValueConstFloat.GetAsFloat: Extended;
begin
  Result := FValue;
end;

constructor TFpValueConstFloat.Create(AValue: Extended);
begin
  inherited Create;
  FValue := AValue;
end;

{ TFpValueConstBool }

function TFpValueConstBool.GetKind: TDbgSymbolKind;
begin
  Result := skBoolean;
end;

function TFpValueConstBool.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := [svfOrdinal, svfBoolean];
end;

function TFpValueConstBool.GetAsBool: Boolean;
begin
  Result := FValue;
end;

function TFpValueConstBool.GetAsCardinal: QWord;
begin
  if FValue then
    Result := 1
  else
    Result := 0;
end;

constructor TFpValueConstBool.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

{ TDbgSymbolValueConstAddress }

function TFpValueConstAddress.GetKind: TDbgSymbolKind;
begin
  Result := skAddress;
end;

function TFpValueConstAddress.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := [svfAddress]
end;

function TFpValueConstAddress.GetAddress: TFpDbgMemLocation;
begin
  Result := FAddress;
end;

constructor TFpValueConstAddress.Create(AnAddress: TFpDbgMemLocation);
begin
  inherited Create;
  FAddress := AnAddress;
end;

{ TFpValueTypeDeclaration }

function TFpValueTypeDefinition.GetKind: TDbgSymbolKind;
begin
  Result := skNone;
end;

function TFpValueTypeDefinition.GetDbgSymbol: TFpSymbol;
begin
  Result := FSymbol;
end;

constructor TFpValueTypeDefinition.Create(ASymbol: TFpSymbol);
begin
  inherited Create;
  FSymbol := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpValueTypeDeclaration'){$ENDIF};
end;

destructor TFpValueTypeDefinition.Destroy;
begin
  inherited Destroy;
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpValueTypeDeclaration'){$ENDIF};
end;

{ TDbgInfoAddressContext }

function TFpDbgInfoContext.GetMemManager: TFpDbgMemManager;
begin
  Result := nil;
end;

function TFpDbgInfoContext.GetProcedureAtAddress: TFpValue;
begin
  Result := SymbolAtAddress.Value;
end;

function TFpDbgInfoContext.GetSymbolAtAddress: TFpSymbol;
begin
  Result := nil;
end;

function TFpDbgInfoContext.FindSymbol(const AName: String): TFpValue;
begin
  Result := nil;
end;

{ TFpSymbol }

constructor TFpSymbol.Create(const AName: String);
begin
  inherited Create;
  AddReference;
  if AName <> '' then
    SetName(AName);
end;

constructor TFpSymbol.Create(const AName: String; AKind: TDbgSymbolKind;
  AAddress: TFpDbgMemLocation);
begin
  Create(AName);
  SetKind(AKind);
  FAddress := AAddress;
end;

destructor TFpSymbol.Destroy;
begin
  SetTypeInfo(nil);
  inherited Destroy;
end;

function TFpSymbol.GetValueBounds(AValueObj: TFpValue; out ALowBound,
  AHighBound: Int64): Boolean;
begin
  Result := GetValueLowBound(AValueObj, ALowBound); // TODO: ond GetValueHighBound() // but all callers must check result;
  if not GetValueHighBound(AValueObj, AHighBound) then
    Result := False;
end;

function TFpSymbol.GetValueLowBound(AValueObj: TFpValue; out
  ALowBound: Int64): Boolean;
begin
  Result := False;
end;

function TFpSymbol.GetValueHighBound(AValueObj: TFpValue; out
  AHighBound: Int64): Boolean;
begin
  Result := False;
end;

function TFpSymbol.TypeCastValue(AValue: TFpValue): TFpValue;
begin
  Result := nil;
end;

function TFpSymbol.GetAddress: TFpDbgMemLocation;
begin
  if not(sfiAddress in FEvaluatedFields) then
    AddressNeeded;
  Result := FAddress;
end;

function TFpSymbol.GetTypeInfo: TFpSymbol;
begin
  if not(sfiTypeInfo in FEvaluatedFields) then
    TypeInfoNeeded;
  Result := FTypeInfo;
end;

function TFpSymbol.GetMemberVisibility: TDbgSymbolMemberVisibility;
begin
  if not(sfiMemberVisibility in FEvaluatedFields) then
    MemberVisibilityNeeded;
  Result := FMemberVisibility;
end;

function TFpSymbol.GetValueObject: TFpValue;
begin
  Result := nil;
end;

function TFpSymbol.GetKind: TDbgSymbolKind;
begin
  if not(sfiKind in FEvaluatedFields) then
    KindNeeded;
  Result := FKind;
end;

function TFpSymbol.GetName: String;
begin
  if not(sfiName in FEvaluatedFields) then
    NameNeeded;
  Result := FName;
end;

function TFpSymbol.GetSize: Integer;
begin
  if not(sfiSize in FEvaluatedFields) then
    SizeNeeded;
  Result := FSize;
end;

function TFpSymbol.GetSymbolType: TDbgSymbolType;
begin
  if not(sfiSymType in FEvaluatedFields) then
    SymbolTypeNeeded;
  Result := FSymbolType;
end;

function TFpSymbol.GetLastError: TFpError;
begin
  Result := FLastError;
end;

procedure TFpSymbol.SetLastError(AnError: TFpError);
begin
  FLastError := AnError;
end;

function TFpSymbol.GetHasOrdinalValue: Boolean;
begin
  Result := False;
end;

function TFpSymbol.GetOrdinalValue: Int64;
begin
  Result := 0;
end;

function TFpSymbol.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  Result := nil;
end;

function TFpSymbol.GetNestedSymbolByName(AIndex: String): TFpSymbol;
begin
  Result := nil;
end;

function TFpSymbol.GetNestedSymbolCount: Integer;
begin
  Result := 0;
end;

procedure TFpSymbol.SetAddress(AValue: TFpDbgMemLocation);
begin
  FAddress := AValue;
  Include(FEvaluatedFields, sfiAddress);
end;

procedure TFpSymbol.SetKind(AValue: TDbgSymbolKind);
begin
  FKind := AValue;
  Include(FEvaluatedFields, sfiKind);
end;

procedure TFpSymbol.SetSymbolType(AValue: TDbgSymbolType);
begin
  FSymbolType := AValue;
  Include(FEvaluatedFields, sfiSymType);
end;

procedure TFpSymbol.SetSize(AValue: Integer);
begin
  FSize := AValue;
  Include(FEvaluatedFields, sfiSize);
end;

procedure TFpSymbol.SetTypeInfo(ASymbol: TFpSymbol);
begin
  if FTypeInfo <> nil then begin
    //Assert((FTypeInfo.Reference = self) or (FTypeInfo.Reference = nil), 'FTypeInfo.Reference = self|nil');
    {$IFDEF WITH_REFCOUNT_DEBUG}FTypeInfo.ReleaseReference(@FTypeInfo, 'SetTypeInfo'); FTypeInfo := nil;{$ENDIF}
    ReleaseRefAndNil(FTypeInfo);
  end;
  FTypeInfo := ASymbol;
  Include(FEvaluatedFields, sfiTypeInfo);
  if FTypeInfo <> nil then begin
    FTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeInfo, 'SetTypeInfo'){$ENDIF};
  end;
end;

procedure TFpSymbol.SetMemberVisibility(AValue: TDbgSymbolMemberVisibility);
begin
  FMemberVisibility := AValue;
  Include(FEvaluatedFields, sfiMemberVisibility);
end;

procedure TFpSymbol.SetName(AValue: String);
begin
  FName := AValue;
  Include(FEvaluatedFields, sfiName);
end;

function TFpSymbol.GetChild(AIndex: Integer): TFpSymbol;
begin
  result := nil;
end;

function TFpSymbol.GetColumn: Cardinal;
begin
  Result := 0;
end;

function TFpSymbol.GetFile: String;
begin
  Result := '';
end;

function TFpSymbol.GetFlags: TDbgSymbolFlags;
begin
  Result := [];
end;

function TFpSymbol.GetLine: Cardinal;
begin
  Result := 0;
end;

function TFpSymbol.GetParent: TFpSymbol;
begin
  Result := nil;
end;

procedure TFpSymbol.KindNeeded;
begin
  SetKind(skNone);
end;

procedure TFpSymbol.NameNeeded;
begin
  SetName('');
end;

procedure TFpSymbol.SymbolTypeNeeded;
begin
  SetSymbolType(stNone);
end;

procedure TFpSymbol.AddressNeeded;
begin
  SetAddress(InvalidLoc);
end;

procedure TFpSymbol.SizeNeeded;
begin
  SetSize(0);
end;

procedure TFpSymbol.TypeInfoNeeded;
begin
  SetTypeInfo(nil);
end;

procedure TFpSymbol.MemberVisibilityNeeded;
begin
  SetMemberVisibility(svPrivate);
end;

{ TFpSymbolForwarder }

procedure TFpSymbolForwarder.SetForwardToSymbol(AValue: TFpSymbol);
begin
  FForwardToSymbol := AValue;
  EvaluatedFields :=  EvaluatedFields + [sfiForwardToSymbol];
end;

procedure TFpSymbolForwarder.ForwardToSymbolNeeded;
begin
  SetForwardToSymbol(nil);
end;

function TFpSymbolForwarder.GetForwardToSymbol: TFpSymbol;
begin
  if TMethod(@ForwardToSymbolNeeded).Code = Pointer(@TFpSymbolForwarder.ForwardToSymbolNeeded) then
    exit(nil);

  if not(sfiForwardToSymbol in EvaluatedFields) then
    ForwardToSymbolNeeded;
  Result := FForwardToSymbol;
end;

function TFpSymbolForwarder.GetLastError: TFpError;
var
  p: TFpSymbol;
begin
  Result := inherited GetLastError;
  if IsError(Result) then
    exit;
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.LastError;
end;

procedure TFpSymbolForwarder.KindNeeded;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetKind(p.Kind)
  else
    SetKind(skNone);  //  inherited KindNeeded;
end;

procedure TFpSymbolForwarder.NameNeeded;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetName(p.Name)
  else
    SetName('');  //  inherited NameNeeded;
end;

procedure TFpSymbolForwarder.SymbolTypeNeeded;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetSymbolType(p.SymbolType)
  else
    SetSymbolType(stNone);  //  inherited SymbolTypeNeeded;
end;

procedure TFpSymbolForwarder.SizeNeeded;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetSize(p.Size)
  else
    SetSize(0);  //  inherited SizeNeeded;
end;

procedure TFpSymbolForwarder.TypeInfoNeeded;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetTypeInfo(p.TypeInfo)
  else
    SetTypeInfo(nil);  //  inherited TypeInfoNeeded;
end;

procedure TFpSymbolForwarder.MemberVisibilityNeeded;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetMemberVisibility(p.MemberVisibility)
  else
    SetMemberVisibility(svPrivate);  //  inherited MemberVisibilityNeeded;
end;

function TFpSymbolForwarder.GetFlags: TDbgSymbolFlags;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Flags
  else
    Result := [];  //  Result := inherited GetFlags;
end;

function TFpSymbolForwarder.GetValueObject: TFpValue;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Value
  else
    Result := nil;  //  Result := inherited Value;
end;

function TFpSymbolForwarder.GetHasOrdinalValue: Boolean;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.HasOrdinalValue
  else
    Result := False;  //  Result := inherited GetHasOrdinalValue;
end;

function TFpSymbolForwarder.GetOrdinalValue: Int64;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.OrdinalValue
  else
    Result := 0;  //  Result := inherited GetOrdinalValue;
end;

function TFpSymbolForwarder.GetValueBounds(AValueObj: TFpValue; out
  ALowBound, AHighBound: Int64): Boolean;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.GetValueBounds(AValueObj, ALowBound, AHighBound)
  else
    Result := inherited GetValueBounds(AValueObj, ALowBound, AHighBound);
end;

function TFpSymbolForwarder.GetValueLowBound(AValueObj: TFpValue; out
  ALowBound: Int64): Boolean;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.GetValueLowBound(AValueObj, ALowBound)
  else
    Result := inherited GetValueLowBound(AValueObj, ALowBound);
end;

function TFpSymbolForwarder.GetValueHighBound(AValueObj: TFpValue; out
  AHighBound: Int64): Boolean;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.GetValueHighBound(AValueObj, AHighBound)
  else
    Result := inherited GetValueHighBound(AValueObj, AHighBound);
end;

function TFpSymbolForwarder.GetNestedSymbol(AIndex: Int64): TFpSymbol;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.NestedSymbol[AIndex]
  else
    Result := nil;  //  Result := inherited GetMember(AIndex);
end;

function TFpSymbolForwarder.GetNestedSymbolByName(AIndex: String): TFpSymbol;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.NestedSymbolByName[AIndex]
  else
    Result := nil;  //  Result := inherited GetMemberByName(AIndex);
end;

function TFpSymbolForwarder.GetNestedSymbolCount: Integer;
var
  p: TFpSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.NestedSymbolCount
  else
    Result := 0;  //  Result := inherited GetMemberCount;
end;

{ TDbgInfo }

constructor TDbgInfo.Create(ALoaderList: TDbgImageLoaderList);
begin
  inherited Create;
end;

function TDbgInfo.FindContext(AThreadId, AStackFrame: Integer;
  AAddress: TDbgPtr): TFpDbgInfoContext;
begin
  Result := nil;;
end;

function TDbgInfo.FindContext(AAddress: TDbgPtr): TFpDbgInfoContext;
begin
  Result := nil;
end;

function TDbgInfo.FindSymbol(const AName: String): TFpSymbol;
begin
  Result := nil;
end;

function TDbgInfo.FindSymbol(AAddress: TDbgPtr): TFpSymbol;
begin
  Result := nil;
end;

function TDbgInfo.GetLineAddresses(const AFileName: String; ALine: Cardinal;
  var AResultList: TDBGPtrArray): Boolean;
begin
  Result := False;
end;

procedure TDbgInfo.SetHasInfo;
begin
  FHasInfo := True;
end;

end.


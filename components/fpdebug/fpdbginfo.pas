unit FpDbgInfo;
(*
  About TFpDbgValue and TFpDbgSymbol

  * TFpDbgSymbol
    Represents a Symbol or Identifier (stType or stValue)

  * TFpDbgValue
    Holds the Value of a Symbol according to its type.

  TFpDbgSymbol should not hold any Data, except for information that is in the
  debug info (dwarf/stabs).
  All Data read from the target must be in TFpDbgValue.
  Target adta includes Address (can be indirect via ref or pointer, Size and
  Boundaries (Sub range / Array).

  This means that TFpDbgSymbol (stType or stValue) should be re-usable. There can
  be multiple TFpDbgValue for each TFpDbgSymbol. (even for stValue, as in an
  Array the Symbol itself is repeated / Array of record: the same member occurs
  over and over)

  ---
  A Variable value in the target typically consists of:
  - TFpDbgSymbol (stValue)
  - TFpDbgSymbol (stType)
  - TFpDbgValue

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

  TFpDbgSymbolField = (
    sfiName, sfiKind, sfiSymType, sfiAddress, sfiSize,
    sfiTypeInfo, sfiMemberVisibility,
    sfiForwardToSymbol
  );
  TFpDbgSymbolFields = set of TFpDbgSymbolField;

  TFpDbgSymbol = class;

  TFpDbgValueFieldFlag = (
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
  TFpDbgValueFieldFlags = set of TFpDbgValueFieldFlag;

  { TFpDbgValue }

  TFpDbgValue = class(TFpDbgCircularRefCountedObject)
  protected
    function GetKind: TDbgSymbolKind; virtual;
    function GetFieldFlags: TFpDbgValueFieldFlags; virtual;

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

    function GetMember({%H-}AIndex: Int64): TFpDbgValue; virtual;
    function GetMemberByName({%H-}AIndex: String): TFpDbgValue; virtual;
    function GetMemberCount: Integer; virtual;
    function GetIndexType({%H-}AIndex: Integer): TFpDbgSymbol; virtual;
    function GetIndexTypeCount: Integer; virtual;
    function GetMemberCountEx(const AIndex: array of Int64): Integer; virtual;
    function GetMemberEx(const AIndex: Array of Int64): TFpDbgValue; virtual;

    function GetDbgSymbol: TFpDbgSymbol; virtual;
    function GetTypeInfo: TFpDbgSymbol; virtual;
    function GetContextTypeInfo: TFpDbgSymbol; virtual;

    function GetLastError: TFpError; virtual;
  public
    constructor Create;
    property RefCount;

    // Kind: determines which types of value are available
    property Kind: TDbgSymbolKind read GetKind;
    property FieldFlags: TFpDbgValueFieldFlags read GetFieldFlags;

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
    function GetTypeCastedValue(ADataVal: TFpDbgValue): TFpDbgValue; virtual; // only if Symbol is a type
// base class? Or Member includes member from base
    (* Member:
       * skClass, skStructure:
           stType: it excludes BaseClass (TODO: decide?)
           stValue: includes
       * skSet
           stType: all members
           stValue: only members set in value (Only impremented for DbgSymbolValue)
       * skArray: (differs from TFpDbgSymbol)
         The values. The type of each Index-dimension is avail via IndexType
       * skPointer: deref the pointer, with index (0 = normal deref)
       NOTE: Values returned by Member/MemberByName are volatile.
             They maybe released or changed when Member is called again.
             To keep a returned Value a reference can be added (AddReference)
    *)
    property MemberCount: Integer read GetMemberCount;
    property Member[AIndex: Int64]: TFpDbgValue read GetMember;
    property MemberByName[AIndex: String]: TFpDbgValue read GetMemberByName; // Includes inheritance
    //  For Arrays (TODO pointers) only, the values stored in the array
    property IndexTypeCount: Integer read GetIndexTypeCount;
    property IndexType[AIndex: Integer]: TFpDbgSymbol read GetIndexType;

    (* DbgSymbol: The TFpDbgSymbol from which this value came, maybe nil.
                  Maybe a stType, then there is no Value *)
    property DbgSymbol: TFpDbgSymbol read GetDbgSymbol;
    property TypeInfo: TFpDbgSymbol read GetTypeInfo;
    property ContextTypeInfo: TFpDbgSymbol read GetContextTypeInfo; // For members, the class in which this member is declared

    property LastError: TFpError read GetLastError;
  end;

  { TFpDbgValueConstNumber }

  TFpDbgValueConstNumber = class(TFpDbgValue)
  private
    FValue: QWord;
    FSigned: Boolean;
  protected
    property Value: QWord read FValue write FValue;
    property Signed: Boolean read FSigned write FSigned;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsInteger: Int64; override;
  public
    constructor Create(AValue: QWord; ASigned: Boolean = True);
  end;

  { TFpDbgValueConstChar }

  TFpDbgValueConstChar = class(TFpDbgValue) // skChar / Not for strings
  private
    FValue: String;
  protected
    property Value: String read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsString: AnsiString; override;
  public
    constructor Create(AValue: AnsiString);
  end;

  { TFpDbgValueConstWideChar }

  TFpDbgValueConstWideChar = class(TFpDbgValue) // skChar / Not for strings
  private
    FValue: String;
  protected
    property Value: String read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsString: AnsiString; override;
  public
    constructor Create(AValue: AnsiString);
  end;

  { TFpDbgValueConstFloat }

  TFpDbgValueConstFloat = class(TFpDbgValue)
  private
    FValue: Extended;
  protected
    property Value: Extended read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsFloat: Extended; override;
  public
    constructor Create(AValue: Extended);
  end;

  { TFpDbgValueConstBool}

  TFpDbgValueConstBool = class(TFpDbgValue)
  private
    FValue: Boolean;
  protected
    property Value: Boolean read FValue write FValue;
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsBool: Boolean; override;
    function GetAsCardinal: QWord; override;
  public
    constructor Create(AValue: Boolean);
  end;

  { TFpDbgValueConstAddress }

  TFpDbgValueConstAddress = class(TFpDbgValue)
  private
    FAddress: TFpDbgMemLocation;
  protected
    property Address: TFpDbgMemLocation read FAddress write FAddress;
    function GetKind: TDbgSymbolKind; override; // skAddress
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAddress: TFpDbgMemLocation; override;
  public
    constructor Create(AnAddress: TFpDbgMemLocation);
  end;

  { TFpDbgValueTypeDefinition }

  TFpDbgValueTypeDefinition = class(TFpDbgValue)
  private
    FSymbol: TFpDbgSymbol; // stType
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetDbgSymbol: TFpDbgSymbol; override;
  public
    constructor Create(ASymbol: TFpDbgSymbol); // Only for stType
    destructor Destroy; override;
  end;

  { TFpDbgSymbol }

  TFpDbgSymbol = class(TFpDbgCircularRefCountedObject)
  private
    FEvaluatedFields: TFpDbgSymbolFields;
    FLastError: TFpError;

    // Cached fields
    FName: String;
    FKind: TDbgSymbolKind;
    FSymbolType: TDbgSymbolType;
    FAddress: TFpDbgMemLocation;
    FSize: Integer;
    FTypeInfo: TFpDbgSymbol;
    FMemberVisibility: TDbgSymbolMemberVisibility; // Todo: not cached

    function GetSymbolType: TDbgSymbolType; inline;
    function GetKind: TDbgSymbolKind; inline;
    function GetName: String; inline;
    function GetSize: Integer; inline;
    function GetAddress: TFpDbgMemLocation; inline;
    function GetTypeInfo: TFpDbgSymbol; inline;
    function GetMemberVisibility: TDbgSymbolMemberVisibility; inline;
  protected
    function  GetLastError: TFpError; virtual;
    procedure SetLastError(AnError: TFpError);
    // NOT cached fields
    function GetChild({%H-}AIndex: Integer): TFpDbgSymbol; virtual;
    function GetColumn: Cardinal; virtual;
    function GetCount: Integer; virtual;
    function GetFile: String; virtual;
    function GetFlags: TDbgSymbolFlags; virtual;
    function GetLine: Cardinal; virtual;
    function GetParent: TFpDbgSymbol; virtual;

    function GetValueObject: TFpDbgValue; virtual;
    function GetHasOrdinalValue: Boolean; virtual;
    function GetOrdinalValue: Int64; virtual;

    function GetHasBounds: Boolean; virtual;
    function GetOrdHighBound: Int64; virtual;
    function GetOrdLowBound: Int64; virtual;

    function GetMember({%H-}AIndex: Int64): TFpDbgSymbol; virtual;
    function GetMemberByName({%H-}AIndex: String): TFpDbgSymbol; virtual;
    function GetMemberCount: Integer; virtual;
  protected
    property EvaluatedFields: TFpDbgSymbolFields read FEvaluatedFields write FEvaluatedFields;
    // Cached fields
    procedure SetName(AValue: String); inline;
    procedure SetKind(AValue: TDbgSymbolKind); inline;
    procedure SetSymbolType(AValue: TDbgSymbolType); inline;
    procedure SetAddress(AValue: TFpDbgMemLocation); inline;
    procedure SetSize(AValue: Integer); inline;
    procedure SetTypeInfo(AValue: TFpDbgSymbol); inline;
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
    property TypeInfo: TFpDbgSymbol read GetTypeInfo;
    // Location
    property FileName: String read GetFile;
    property Line: Cardinal read GetLine;
    property Column: Cardinal read GetColumn;
    // Methods for structures (record / class / enum)
    //         array: each member represents an index (enum or subrange) and has low/high bounds
    property MemberVisibility: TDbgSymbolMemberVisibility read GetMemberVisibility;
    property MemberCount: Integer read GetMemberCount;
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
    property Member[AIndex: Int64]: TFpDbgSymbol read GetMember;
    property MemberByName[AIndex: String]: TFpDbgSymbol read GetMemberByName; // Includes inheritance
    //
    property Flags: TDbgSymbolFlags read GetFlags;
    property Count: Integer read GetCount; deprecated 'use MemberCount instead';
    property Parent: TFpDbgSymbol read GetParent; deprecated;
    // for Subranges
    property HasBounds: Boolean read GetHasBounds;
    property OrdLowBound: Int64 read GetOrdLowBound;  //deprecated 'xxxx'; // need typecast for QuadWord
    property OrdHighBound: Int64 read GetOrdHighBound;  //deprecated 'xxxx'; // need typecast for QuadWord
    // VALUE
    property Value: TFpDbgValue read GetValueObject; //deprecated 'rename / create';
    property HasOrdinalValue: Boolean read GetHasOrdinalValue;
    property OrdinalValue: Int64 read GetOrdinalValue;   //deprecated 'xxxx'; // need typecast for QuadWord

    // TypeCastValue| only fon stType symbols, may return nil
    // Returns a reference to caller / caller must release
    function TypeCastValue({%H-}AValue: TFpDbgValue): TFpDbgValue; virtual;

    property LastError: TFpError read GetLastError; experimental;
  end;

  { TDbgSymbolForwarder }

  TDbgSymbolForwarder = class(TFpDbgSymbol)
  private
    FForwardToSymbol: TFpDbgSymbol;
  protected
    procedure SetForwardToSymbol(AValue: TFpDbgSymbol); inline;
    procedure ForwardToSymbolNeeded; virtual;
    function  GetForwardToSymbol: TFpDbgSymbol; inline;
  protected
    function GetLastError: TFpError; override;
    procedure KindNeeded; override;
    procedure NameNeeded; override;
    procedure SymbolTypeNeeded; override;
    procedure SizeNeeded; override;
    procedure TypeInfoNeeded; override;
    procedure MemberVisibilityNeeded; override;

    function GetFlags: TDbgSymbolFlags; override;
    function GetValueObject: TFpDbgValue; override;
    function GetHasOrdinalValue: Boolean; override;
    function GetOrdinalValue: Int64; override;
    function GetHasBounds: Boolean; override;
    function GetOrdLowBound: Int64; override;
    function GetOrdHighBound: Int64; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;
  end;

  { TFpDbgInfoContext }

  TFpDbgInfoContext = class(TFpDbgAddressContext)
  protected
    function GetSymbolAtAddress: TFpDbgSymbol; virtual;
    function GetProcedureAtAddress: TFpDbgValue; virtual;
    function GetMemManager: TFpDbgMemManager; virtual;
  public
    property SymbolAtAddress: TFpDbgSymbol read GetSymbolAtAddress;
    property ProcedureAtAddress: TFpDbgValue read GetProcedureAtAddress;
    // search this, and all parent context
    function FindSymbol(const {%H-}AName: String): TFpDbgValue; virtual;
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
    function FindSymbol(const {%H-}AName: String): TFpDbgSymbol; virtual; deprecated;
    function FindSymbol({%H-}AAddress: TDbgPtr): TFpDbgSymbol; virtual; deprecated;
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

{ TFpDbgValueConstChar }

function TFpDbgValueConstChar.GetKind: TDbgSymbolKind;
begin
  Result := skChar;
end;

function TFpDbgValueConstChar.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := [svfString]
end;

function TFpDbgValueConstChar.GetAsString: AnsiString;
begin
  Result := Value;
end;

constructor TFpDbgValueConstChar.Create(AValue: AnsiString);
begin
  inherited Create;
  FValue := AValue;
end;

{ TFpDbgValueConstWideChar }

function TFpDbgValueConstWideChar.GetKind: TDbgSymbolKind;
begin
  Result := skChar;
end;

function TFpDbgValueConstWideChar.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := [svfString]
end;

function TFpDbgValueConstWideChar.GetAsString: AnsiString;
begin
  Result := Value;
end;

constructor TFpDbgValueConstWideChar.Create(AValue: AnsiString);
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

function TFpDbgValue.GetAsString: AnsiString;
begin
  Result := '';
end;

function TFpDbgValue.GetAsWideString: WideString;
begin
  Result := '';
end;

function TFpDbgValue.GetDbgSymbol: TFpDbgSymbol;
begin
  Result := nil;
end;

constructor TFpDbgValue.Create;
begin
  inherited Create;
  AddReference;
end;

function TFpDbgValue.GetTypeCastedValue(ADataVal: TFpDbgValue): TFpDbgValue;
begin
  assert(False, 'TFpDbgValue.GetTypeCastedValue: False');
  Result := nil;
end;

function TFpDbgValue.GetTypeInfo: TFpDbgSymbol;
begin
  if (DbgSymbol <> nil) and (DbgSymbol.SymbolType = stValue) then
    Result := DbgSymbol.TypeInfo
  else
    Result := nil;
end;

function TFpDbgValue.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := [];
end;

function TFpDbgValue.GetIndexType(AIndex: Integer): TFpDbgSymbol;
begin
  Result := nil;;
end;

function TFpDbgValue.GetIndexTypeCount: Integer;
begin
  Result := 0;
end;

function TFpDbgValue.GetMemberEx(const AIndex: array of Int64): TFpDbgValue;
begin
  Result := nil;
end;

function TFpDbgValue.GetMemberCountEx(const AIndex: array of Int64): Integer;
begin
  Result := 0;
end;

function TFpDbgValue.GetAsFloat: Extended;
begin
  Result := 0;
end;

function TFpDbgValue.GetContextTypeInfo: TFpDbgSymbol;
begin
  Result := nil;
end;

function TFpDbgValue.GetLastError: TFpError;
begin
  Result := NoError;
end;

function TFpDbgValue.GetHasBounds: Boolean;
begin
  Result := False;
end;

function TFpDbgValue.GetOrdHighBound: Int64;
begin
  Result := 0;
end;

function TFpDbgValue.GetOrdLowBound: Int64;
begin
  Result := 0;
end;

function TFpDbgValue.GetKind: TDbgSymbolKind;
begin
  Result := skNone;
end;

function TFpDbgValue.GetMember(AIndex: Int64): TFpDbgValue;
begin
  Result := nil;
end;

function TFpDbgValue.GetMemberByName(AIndex: String): TFpDbgValue;
begin
  Result := nil;
end;

function TFpDbgValue.GetMemberCount: Integer;
begin
  Result := 0;
end;

function TFpDbgValue.GetAddress: TFpDbgMemLocation;
begin
  Result := InvalidLoc;
end;

function TFpDbgValue.GetDataAddress: TFpDbgMemLocation;
begin
  Result := Address;
end;

function TFpDbgValue.GetDataSize: Integer;
begin
  Result := Size;
end;

function TFpDbgValue.GetSize: Integer;
begin
  Result := -1;
end;

function TFpDbgValue.GetAsBool: Boolean;
begin
  Result := False;
end;

function TFpDbgValue.GetAsCardinal: QWord;
begin
  Result := 0;
end;

function TFpDbgValue.GetAsInteger: Int64;
begin
  Result := 0;
end;

{ TPasParserConstNumberSymbolValue }

function TFpDbgValueConstNumber.GetKind: TDbgSymbolKind;
begin
  if FSigned then
    Result := skInteger
  else
    Result := skCardinal;
end;

function TFpDbgValueConstNumber.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  if FSigned then
    Result := [svfOrdinal, svfInteger]
  else
    Result := [svfOrdinal, svfCardinal];
end;

function TFpDbgValueConstNumber.GetAsCardinal: QWord;
begin
  Result := FValue;
end;

function TFpDbgValueConstNumber.GetAsInteger: Int64;
begin
  Result := Int64(FValue);
end;

constructor TFpDbgValueConstNumber.Create(AValue: QWord; ASigned: Boolean);
begin
  inherited Create;
  FValue := AValue;
  FSigned := ASigned;
end;

{ TFpDbgValueConstFloat }

function TFpDbgValueConstFloat.GetKind: TDbgSymbolKind;
begin
  Result := skFloat;
end;

function TFpDbgValueConstFloat.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := [svfFloat];
end;

function TFpDbgValueConstFloat.GetAsFloat: Extended;
begin
  Result := FValue;
end;

constructor TFpDbgValueConstFloat.Create(AValue: Extended);
begin
  inherited Create;
  FValue := AValue;
end;

{ TFpDbgValueConstBool }

function TFpDbgValueConstBool.GetKind: TDbgSymbolKind;
begin
  Result := skBoolean;
end;

function TFpDbgValueConstBool.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := [svfOrdinal, svfBoolean];
end;

function TFpDbgValueConstBool.GetAsBool: Boolean;
begin
  Result := FValue;
end;

function TFpDbgValueConstBool.GetAsCardinal: QWord;
begin
  if FValue then
    Result := 1
  else
    Result := 0;
end;

constructor TFpDbgValueConstBool.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

{ TDbgSymbolValueConstAddress }

function TFpDbgValueConstAddress.GetKind: TDbgSymbolKind;
begin
  Result := skAddress;
end;

function TFpDbgValueConstAddress.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := [svfAddress]
end;

function TFpDbgValueConstAddress.GetAddress: TFpDbgMemLocation;
begin
  Result := FAddress;
end;

constructor TFpDbgValueConstAddress.Create(AnAddress: TFpDbgMemLocation);
begin
  inherited Create;
  FAddress := AnAddress;
end;

{ TFpDbgValueTypeDeclaration }

function TFpDbgValueTypeDefinition.GetKind: TDbgSymbolKind;
begin
  Result := skNone;
end;

function TFpDbgValueTypeDefinition.GetDbgSymbol: TFpDbgSymbol;
begin
  Result := FSymbol;
end;

constructor TFpDbgValueTypeDefinition.Create(ASymbol: TFpDbgSymbol);
begin
  inherited Create;
  FSymbol := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpDbgValueTypeDeclaration'){$ENDIF};
end;

destructor TFpDbgValueTypeDefinition.Destroy;
begin
  inherited Destroy;
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpDbgValueTypeDeclaration'){$ENDIF};
end;

{ TDbgInfoAddressContext }

function TFpDbgInfoContext.GetMemManager: TFpDbgMemManager;
begin
  Result := nil;
end;

function TFpDbgInfoContext.GetProcedureAtAddress: TFpDbgValue;
begin
  Result := SymbolAtAddress.Value;
end;

function TFpDbgInfoContext.GetSymbolAtAddress: TFpDbgSymbol;
begin
  Result := nil;
end;

function TFpDbgInfoContext.FindSymbol(const AName: String): TFpDbgValue;
begin
  Result := nil;
end;

{ TFpDbgSymbol }

constructor TFpDbgSymbol.Create(const AName: String);
begin
  inherited Create;
  AddReference;
  if AName <> '' then
    SetName(AName);
end;

constructor TFpDbgSymbol.Create(const AName: String; AKind: TDbgSymbolKind;
  AAddress: TFpDbgMemLocation);
begin
  Create(AName);
  SetKind(AKind);
  FAddress := AAddress;
end;

destructor TFpDbgSymbol.Destroy;
begin
  SetTypeInfo(nil);
  inherited Destroy;
end;

function TFpDbgSymbol.TypeCastValue(AValue: TFpDbgValue): TFpDbgValue;
begin
  Result := nil;
end;

function TFpDbgSymbol.GetAddress: TFpDbgMemLocation;
begin
  if not(sfiAddress in FEvaluatedFields) then
    AddressNeeded;
  Result := FAddress;
end;

function TFpDbgSymbol.GetTypeInfo: TFpDbgSymbol;
begin
  if not(sfiTypeInfo in FEvaluatedFields) then
    TypeInfoNeeded;
  Result := FTypeInfo;
end;

function TFpDbgSymbol.GetMemberVisibility: TDbgSymbolMemberVisibility;
begin
  if not(sfiMemberVisibility in FEvaluatedFields) then
    MemberVisibilityNeeded;
  Result := FMemberVisibility;
end;

function TFpDbgSymbol.GetValueObject: TFpDbgValue;
begin
  Result := nil;
end;

function TFpDbgSymbol.GetKind: TDbgSymbolKind;
begin
  if not(sfiKind in FEvaluatedFields) then
    KindNeeded;
  Result := FKind;
end;

function TFpDbgSymbol.GetName: String;
begin
  if not(sfiName in FEvaluatedFields) then
    NameNeeded;
  Result := FName;
end;

function TFpDbgSymbol.GetSize: Integer;
begin
  if not(sfiSize in FEvaluatedFields) then
    SizeNeeded;
  Result := FSize;
end;

function TFpDbgSymbol.GetSymbolType: TDbgSymbolType;
begin
  if not(sfiSymType in FEvaluatedFields) then
    SymbolTypeNeeded;
  Result := FSymbolType;
end;

function TFpDbgSymbol.GetLastError: TFpError;
begin
  Result := FLastError;
end;

procedure TFpDbgSymbol.SetLastError(AnError: TFpError);
begin
  FLastError := AnError;
end;

function TFpDbgSymbol.GetHasBounds: Boolean;
begin
  Result := False;
end;

function TFpDbgSymbol.GetOrdHighBound: Int64;
begin
  Result := 0;
end;

function TFpDbgSymbol.GetOrdLowBound: Int64;
begin
  Result := 0;
end;

function TFpDbgSymbol.GetHasOrdinalValue: Boolean;
begin
  Result := False;
end;

function TFpDbgSymbol.GetOrdinalValue: Int64;
begin
  Result := 0;
end;

function TFpDbgSymbol.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  Result := nil;
end;

function TFpDbgSymbol.GetMemberByName(AIndex: String): TFpDbgSymbol;
begin
  Result := nil;
end;

function TFpDbgSymbol.GetMemberCount: Integer;
begin
  Result := 0;
end;

procedure TFpDbgSymbol.SetAddress(AValue: TFpDbgMemLocation);
begin
  FAddress := AValue;
  Include(FEvaluatedFields, sfiAddress);
end;

procedure TFpDbgSymbol.SetKind(AValue: TDbgSymbolKind);
begin
  FKind := AValue;
  Include(FEvaluatedFields, sfiKind);
end;

procedure TFpDbgSymbol.SetSymbolType(AValue: TDbgSymbolType);
begin
  FSymbolType := AValue;
  Include(FEvaluatedFields, sfiSymType);
end;

procedure TFpDbgSymbol.SetSize(AValue: Integer);
begin
  FSize := AValue;
  Include(FEvaluatedFields, sfiSize);
end;

procedure TFpDbgSymbol.SetTypeInfo(AValue: TFpDbgSymbol);
begin
  if FTypeInfo <> nil then begin
    //Assert((FTypeInfo.Reference = self) or (FTypeInfo.Reference = nil), 'FTypeInfo.Reference = self|nil');
    {$IFDEF WITH_REFCOUNT_DEBUG}FTypeInfo.ReleaseReference(@FTypeInfo, 'SetTypeInfo'); FTypeInfo := nil;{$ENDIF}
    ReleaseRefAndNil(FTypeInfo);
  end;
  FTypeInfo := AValue;
  Include(FEvaluatedFields, sfiTypeInfo);
  if FTypeInfo <> nil then begin
    FTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeInfo, 'SetTypeInfo'){$ENDIF};
  end;
end;

procedure TFpDbgSymbol.SetMemberVisibility(AValue: TDbgSymbolMemberVisibility);
begin
  FMemberVisibility := AValue;
  Include(FEvaluatedFields, sfiMemberVisibility);
end;

procedure TFpDbgSymbol.SetName(AValue: String);
begin
  FName := AValue;
  Include(FEvaluatedFields, sfiName);
end;

function TFpDbgSymbol.GetChild(AIndex: Integer): TFpDbgSymbol;
begin
  result := nil;
end;

function TFpDbgSymbol.GetColumn: Cardinal;
begin
  Result := 0;
end;

function TFpDbgSymbol.GetCount: Integer;
begin
  Result := 0;
end;

function TFpDbgSymbol.GetFile: String;
begin
  Result := '';
end;

function TFpDbgSymbol.GetFlags: TDbgSymbolFlags;
begin
  Result := [];
end;

function TFpDbgSymbol.GetLine: Cardinal;
begin
  Result := 0;
end;

function TFpDbgSymbol.GetParent: TFpDbgSymbol;
begin
  Result := nil;
end;

procedure TFpDbgSymbol.KindNeeded;
begin
  SetKind(skNone);
end;

procedure TFpDbgSymbol.NameNeeded;
begin
  SetName('');
end;

procedure TFpDbgSymbol.SymbolTypeNeeded;
begin
  SetSymbolType(stNone);
end;

procedure TFpDbgSymbol.AddressNeeded;
begin
  SetAddress(InvalidLoc);
end;

procedure TFpDbgSymbol.SizeNeeded;
begin
  SetSize(0);
end;

procedure TFpDbgSymbol.TypeInfoNeeded;
begin
  SetTypeInfo(nil);
end;

procedure TFpDbgSymbol.MemberVisibilityNeeded;
begin
  SetMemberVisibility(svPrivate);
end;

{ TDbgSymbolForwarder }

procedure TDbgSymbolForwarder.SetForwardToSymbol(AValue: TFpDbgSymbol);
begin
  FForwardToSymbol := AValue;
  EvaluatedFields :=  EvaluatedFields + [sfiForwardToSymbol];
end;

procedure TDbgSymbolForwarder.ForwardToSymbolNeeded;
begin
  SetForwardToSymbol(nil);
end;

function TDbgSymbolForwarder.GetForwardToSymbol: TFpDbgSymbol;
begin
  if TMethod(@ForwardToSymbolNeeded).Code = Pointer(@TDbgSymbolForwarder.ForwardToSymbolNeeded) then
    exit(nil);

  if not(sfiForwardToSymbol in EvaluatedFields) then
    ForwardToSymbolNeeded;
  Result := FForwardToSymbol;
end;

function TDbgSymbolForwarder.GetLastError: TFpError;
var
  p: TFpDbgSymbol;
begin
  Result := inherited GetLastError;
  if IsError(Result) then
    exit;
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.LastError;
end;

procedure TDbgSymbolForwarder.KindNeeded;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetKind(p.Kind)
  else
    SetKind(skNone);  //  inherited KindNeeded;
end;

procedure TDbgSymbolForwarder.NameNeeded;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetName(p.Name)
  else
    SetName('');  //  inherited NameNeeded;
end;

procedure TDbgSymbolForwarder.SymbolTypeNeeded;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetSymbolType(p.SymbolType)
  else
    SetSymbolType(stNone);  //  inherited SymbolTypeNeeded;
end;

procedure TDbgSymbolForwarder.SizeNeeded;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetSize(p.Size)
  else
    SetSize(0);  //  inherited SizeNeeded;
end;

procedure TDbgSymbolForwarder.TypeInfoNeeded;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetTypeInfo(p.TypeInfo)
  else
    SetTypeInfo(nil);  //  inherited TypeInfoNeeded;
end;

procedure TDbgSymbolForwarder.MemberVisibilityNeeded;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetMemberVisibility(p.MemberVisibility)
  else
    SetMemberVisibility(svPrivate);  //  inherited MemberVisibilityNeeded;
end;

function TDbgSymbolForwarder.GetFlags: TDbgSymbolFlags;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Flags
  else
    Result := [];  //  Result := inherited GetFlags;
end;

function TDbgSymbolForwarder.GetValueObject: TFpDbgValue;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Value
  else
    Result := nil;  //  Result := inherited Value;
end;

function TDbgSymbolForwarder.GetHasOrdinalValue: Boolean;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.HasOrdinalValue
  else
    Result := False;  //  Result := inherited GetHasOrdinalValue;
end;

function TDbgSymbolForwarder.GetOrdinalValue: Int64;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.OrdinalValue
  else
    Result := 0;  //  Result := inherited GetOrdinalValue;
end;

function TDbgSymbolForwarder.GetHasBounds: Boolean;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.HasBounds
  else
    Result := False;  //  Result := inherited GetHasBounds;
end;

function TDbgSymbolForwarder.GetOrdLowBound: Int64;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.OrdLowBound
  else
    Result := 0;  //  Result := inherited GetOrdLowBound;
end;

function TDbgSymbolForwarder.GetOrdHighBound: Int64;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.OrdHighBound
  else
    Result := 0;  //  Result := inherited GetOrdHighBound;
end;

function TDbgSymbolForwarder.GetMember(AIndex: Int64): TFpDbgSymbol;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Member[AIndex]
  else
    Result := nil;  //  Result := inherited GetMember(AIndex);
end;

function TDbgSymbolForwarder.GetMemberByName(AIndex: String): TFpDbgSymbol;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.MemberByName[AIndex]
  else
    Result := nil;  //  Result := inherited GetMemberByName(AIndex);
end;

function TDbgSymbolForwarder.GetMemberCount: Integer;
var
  p: TFpDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.MemberCount
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

function TDbgInfo.FindSymbol(const AName: String): TFpDbgSymbol;
begin
  Result := nil;
end;

function TDbgInfo.FindSymbol(AAddress: TDbgPtr): TFpDbgSymbol;
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


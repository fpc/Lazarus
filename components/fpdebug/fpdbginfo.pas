unit FpDbgInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpDbgLoader, LazLoggerBase, LazClasses;

type
  TDbgPtr = QWord; // PtrUInt;

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

    procedure AddCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};
    procedure ReleaseCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr: Pointer = nil; DebugIdTxt: String = ''){$ENDIF};

    procedure MakePlainRefToCirclular;
    procedure MakeCirclularRefToPlain;

    function  CircleBackRefsActive: Boolean; inline;
    procedure CircleBackRefActiveChanged(NewActive: Boolean); virtual;
  end;

  { TFpDbgCircularRefCntObjList }

  TFpDbgCircularRefCntObjList = class(TRefCntObjList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;


  TFpDbgMemReaderBase = class
  public
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract;
    function ReadRegister(ARegNum: Integer; out AValue: TDbgPtr): Boolean; virtual; abstract;
  end;

  TDbgSymbolType = (
    stNone,
    stValue,  // The symbol has a value (var, field, function, procedure (value is address of func/proc, so it can be called)
    stType    // The Symbol is a type (including proc/func declaration / without DW_AT_low_pc)
  );

  TDbgSymbolKind = (
    skNone,          // undefined type
//    skUser,          // userdefined type, this sym refers to another sym defined elswhere
    skInstance,      // the main exe/dll, containing all other syms
    skUnit,          // contains syms defined in this unit
    //--------------------------------------------------------------------------
    skRecord,        // the address member is the relative location within the
    skObject,        // structure: type TFoo=object end;
    skClass,
    skInterface,
    skProcedure,
    skFunction,
    //--------------------------------------------------------------------------
    skArray,
    //--------------------------------------------------------------------------
    skPointer,
    skInteger,       // Basic types, these cannot have references or children
    skCardinal,      // only size matters ( char(1) = Char, char(2) = WideChar
    skBoolean,       // cardinal(1) = Byte etc.
    skChar,
    skFloat,
    skString,
    skAnsiString,
    skCurrency,
    skVariant,
    skWideString,
    skEnum,       // Variable holding an enum / enum type
    skEnumValue,  // a single element from an enum
    skSet,
    //--------------------------------------------------------------------------
    skRegister       // the Address member is the register number
    //--------------------------------------------------------------------------
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

  TDbgSymbolField = (
    sfiName, sfiKind, sfiSymType, sfiAddress, sfiSize,
    sfiTypeInfo, sfiMemberVisibility,
    sfiForwardToSymbol
  );
  TDbgSymbolFields = set of TDbgSymbolField;

  TDbgSymbol = class;

  TDbgSymbolBase = class(TFpDbgCircularRefCountedObject)
  end;

  TDbgSymbolValueFieldFlag = (
    // svfAddress, svfDataAddress this symbol does have an address, but it may still be nil
    svfAddress, svfSize, svfSizeOfPointer,
    svfDataAddress, svfDataSize, svfDataSizeOfPointer,
    svfInteger, svfCardinal,
    svfString, svfWideString,
    svfBoolean,
    svfMembers,
    svfOrdinal       // AsCardinal ruturns an ordinal value, but the value is not represented as cardinal (e.g. bool, enum)
                     // if size > 8, then ordinal (if present) is based on a part only
  );
  TDbgSymbolValueFieldFlags = set of TDbgSymbolValueFieldFlag;

  { TDbgSymbolValue }

  TDbgSymbolValue = class(TDbgSymbolBase)
  protected
    function GetKind: TDbgSymbolKind; virtual;
    function GetFieldFlags: TDbgSymbolValueFieldFlags; virtual;

    function GetAsBool: Boolean;  virtual;
    function GetAsCardinal: QWord; virtual;
    function GetAsInteger: Int64; virtual;
    function GetAsString: AnsiString; virtual;
    function GetAsWideString: WideString; virtual;

    function GetAddress: TDbgPtr;  virtual;
    function GetSize: Integer;  virtual;
    function GetDataAddress: TDbgPtr;  virtual;
    function GetDataSize: Integer;  virtual;

    function GetMember(AIndex: Integer): TDbgSymbolValue; virtual;
    function GetMemberByName(AIndex: String): TDbgSymbolValue; virtual;
    function GetMemberCount: Integer; virtual;
    function GetDbgSymbol: TDbgSymbol; virtual;
    function GetTypeInfo: TDbgSymbol; virtual;
  public
    constructor Create;
    // Kind: determines which types of value are available
    property Kind: TDbgSymbolKind read GetKind;
    property FieldFlags: TDbgSymbolValueFieldFlags read GetFieldFlags;

    property AsInteger: Int64 read GetAsInteger;
    property AsCardinal: QWord read GetAsCardinal;
    property AsBool: Boolean read GetAsBool;
    property AsString: AnsiString read GetAsString;
    property AsWideString: WideString read GetAsWideString;
    // complex
    // double

    property Address: TDbgPtr read GetAddress;     // Address of variable
    property Size: Integer read GetSize;           // Size of variable
    property DataAddress: TDbgPtr read GetDataAddress; // Address of Data, if avail (e.g. String, TObject, ..., BUT NOT record)
    property DataSize: Integer read GetDataSize;       // Sive of Data, if avail (e.g. String, TObject, ..., BUT NOT record)
    // memdump
  public
    (* Member:
       For TypeInfo (skType) it excludes BaseClass
       For Value (skValue): ???
    *)
// base class? Or Member inncludes member from base
    property MemberCount: Integer read GetMemberCount;
    property Member[AIndex: Integer]: TDbgSymbolValue read GetMember;
    property MemberByName[AIndex: String]: TDbgSymbolValue read GetMemberByName; // Includes inheritance

    (* DbgSymbol: The TDbgSymbol from which this value came, maybe nil.
                  Maybe a stType, then there is no Value *)
    property DbgSymbol: TDbgSymbol read GetDbgSymbol;
    property TypeInfo: TDbgSymbol read GetTypeInfo;
  end;

  { TDbgSymbol }

  TDbgSymbol = class(TDbgSymbolBase)
  private
    FEvaluatedFields: TDbgSymbolFields;

    // Cached fields
    FName: String;
    FKind: TDbgSymbolKind;
    FSymbolType: TDbgSymbolType;
    FAddress: TDbgPtr;
    FSize: Integer;
    FTypeInfo: TDbgSymbol;
    FMemberVisibility: TDbgSymbolMemberVisibility; // Todo: not cached

    function GetSymbolType: TDbgSymbolType; inline;
    function GetKind: TDbgSymbolKind; inline;
    function GetName: String; inline;
    function GetSize: Integer; inline;
    function GetAddress: TDbgPtr; inline;
    function GetTypeInfo: TDbgSymbol; inline;
    function GetMemberVisibility: TDbgSymbolMemberVisibility; inline;
  protected
    // NOT cached fields
    function GetChild({%H-}AIndex: Integer): TDbgSymbol; virtual;
    function GetColumn: Cardinal; virtual;
    function GetCount: Integer; virtual;
    function GetFile: String; virtual;
    function GetFlags: TDbgSymbolFlags; virtual;
    function GetLine: Cardinal; virtual;
    function GetParent: TDbgSymbol; virtual;

    function GetValueObject: TDbgSymbolValue; virtual;
    function GetHasOrdinalValue: Boolean; virtual;
    function GetOrdinalValue: Int64; virtual;

    function GetHasBounds: Boolean; virtual;
    function GetOrdHighBound: Int64; virtual;
    function GetOrdLowBound: Int64; virtual;

    function GetMember({%H-}AIndex: Integer): TDbgSymbol; virtual;
    function GetMemberByName({%H-}AIndex: String): TDbgSymbol; virtual;
    function GetMemberCount: Integer; virtual;
  protected
    property EvaluatedFields: TDbgSymbolFields read FEvaluatedFields write FEvaluatedFields;
    // Cached fields
    procedure SetName(AValue: String); inline;
    procedure SetKind(AValue: TDbgSymbolKind); inline;
    procedure SetSymbolType(AValue: TDbgSymbolType); inline;
    procedure SetAddress(AValue: TDbgPtr); inline;
    procedure SetSize(AValue: Integer); inline;
    procedure SetTypeInfo(AValue: TDbgSymbol); inline;
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
    constructor Create(const AName: String; AKind: TDbgSymbolKind; AAddress: TDbgPtr);
    destructor Destroy; override;
    // Basic info
    property Name:       String read GetName;
    property SymbolType: TDbgSymbolType read GetSymbolType;
    property Kind:       TDbgSymbolKind read GetKind;
    // Memory; Size is also part of type (byte vs word vs ...)
    // HasAddress // (register does not have)
    property Address:    TDbgPtr read GetAddress;
    property Size:       Integer read GetSize; // In Bytes
    // TypeInfo used by
    // stValue (Variable): Type
    // stType: Pointer: type pointed to / Array: Element Type / Func: Result / Class: itheritance
    property TypeInfo: TDbgSymbol read GetTypeInfo;
    // Location
    property FileName: String read GetFile;
    property Line: Cardinal read GetLine;
    property Column: Cardinal read GetColumn;
    // Methods for structures (record / class / enum)
    //         array: each member represents an index (enum or subrange) and has low/high bounds
    property MemberVisibility: TDbgSymbolMemberVisibility read GetMemberVisibility;
    property MemberCount: Integer read GetMemberCount;
    (* Member:
       For TypeInfo (skType) it excludes BaseClass
       For Value (skValue): ???
    *)
    property Member[AIndex: Integer]: TDbgSymbol read GetMember;
    property MemberByName[AIndex: String]: TDbgSymbol read GetMemberByName; // Includes inheritance
    //
    property Flags: TDbgSymbolFlags read GetFlags;
    property Count: Integer read GetCount; deprecated;
    property Parent: TDbgSymbol read GetParent; deprecated;
    // for Subranges
    property HasBounds: Boolean read GetHasBounds;
    property OrdLowBound: Int64 read GetOrdLowBound; // need typecast for QuadWord
    property OrdHighBound: Int64 read GetOrdHighBound; // need typecast for QuadWord
    // VALUE
    property Value: TDbgSymbolValue read GetValueObject;
    property HasOrdinalValue: Boolean read GetHasOrdinalValue;
    property OrdinalValue: Int64 read GetOrdinalValue; // need typecast for QuadWord

    // TypeCastValue| only fon stType symbols, may return nil
    // Returns a reference to caller / caller must release
    function TypeCastValue(AValue: TDbgSymbolValue): TDbgSymbolValue; virtual;
  end;

  { TDbgSymbolForwarder }

  TDbgSymbolForwarder = class(TDbgSymbol)
  private
    FForwardToSymbol: TDbgSymbol;
  protected
    procedure SetForwardToSymbol(AValue: TDbgSymbol); inline;
    procedure ForwardToSymbolNeeded; virtual;
    function  GetForwardToSymbol: TDbgSymbol; inline;
  protected
    procedure KindNeeded; override;
    procedure NameNeeded; override;
    procedure SymbolTypeNeeded; override;
    procedure SizeNeeded; override;
    procedure TypeInfoNeeded; override;
    procedure MemberVisibilityNeeded; override;

    function GetFlags: TDbgSymbolFlags; override;
    function GetValueObject: TDbgSymbolValue; override;
    function GetHasOrdinalValue: Boolean; override;
    function GetOrdinalValue: Int64; override;
    function GetHasBounds: Boolean; override;
    function GetOrdLowBound: Int64; override;
    function GetOrdHighBound: Int64; override;
    function GetMember(AIndex: Integer): TDbgSymbol; override;
    function GetMemberByName(AIndex: String): TDbgSymbol; override;
    function GetMemberCount: Integer; override;
  end;

  { TDbgInfoAddressContext }

  TDbgInfoAddressContext = class(TRefCountedObject)
  protected
    function GetAddress: TDbgPtr; virtual; abstract;
    function GetSymbolAtAddress: TDbgSymbol; virtual;
    function GetMemReader: TFpDbgMemReaderBase; virtual;
    function GetSizeOfAddress: Integer; virtual;
  public
    property Address: TDbgPtr read GetAddress;
    property SymbolAtAddress: TDbgSymbol read GetSymbolAtAddress;
    // search this, and all parent context
    function FindSymbol(const {%H-}AName: String): TDbgSymbol; virtual;
    property MemReader: TFpDbgMemReaderBase read GetMemReader;
    property SizeOfAddress: Integer read GetSizeOfAddress;
  end;

  { TDbgInfo }

  TDbgInfo = class(TObject)
  private
    FHasInfo: Boolean;
  protected
    procedure SetHasInfo;
  public
    constructor Create({%H-}ALoader: TDbgImageLoader); virtual;
    function FindContext({%H-}AAddress: TDbgPtr): TDbgInfoAddressContext; virtual;
    function FindSymbol(const {%H-}AName: String): TDbgSymbol; virtual; deprecated;
    function FindSymbol({%H-}AAddress: TDbgPtr): TDbgSymbol; virtual; deprecated;
    property HasInfo: Boolean read FHasInfo;
    function GetLineAddress(const {%H-}AFileName: String; {%H-}ALine: Cardinal): TDbgPtr; virtual;
  end;

function dbgs(ADbgSymbolKind: TDbgSymbolKind): String; overload;

implementation

function dbgs(ADbgSymbolKind: TDbgSymbolKind): String;
begin
  Result := '';
  WriteStr(Result, ADbgSymbolKind);
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
begin
  Assert(FCircleRefCount > 0, 'ReleaseCirclularReference > 0');
  if CircleBackRefsActive then begin
    dec(FCircleRefCount);
    ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
  end
  else begin
    ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(DebugIdAdr, DebugIdTxt){$ENDIF};
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

function TDbgSymbolValue.GetAsString: AnsiString;
begin
  Result := '';
end;

function TDbgSymbolValue.GetAsWideString: WideString;
begin
  Result := '';
end;

function TDbgSymbolValue.GetDbgSymbol: TDbgSymbol;
begin
  Result := nil;
end;

constructor TDbgSymbolValue.Create;
begin
  inherited Create;
  AddReference;
end;

function TDbgSymbolValue.GetTypeInfo: TDbgSymbol;
begin
  if (DbgSymbol <> nil) and (DbgSymbol.SymbolType = stValue) then
    Result := DbgSymbol.TypeInfo
  else
    Result := nil;
end;

function TDbgSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := [];
end;

function TDbgSymbolValue.GetKind: TDbgSymbolKind;
begin
  Result := skNone;
end;

function TDbgSymbolValue.GetMember(AIndex: Integer): TDbgSymbolValue;
begin
  Result := nil;
end;

function TDbgSymbolValue.GetMemberByName(AIndex: String): TDbgSymbolValue;
begin
  Result := nil;
end;

function TDbgSymbolValue.GetMemberCount: Integer;
begin
  Result := 0;
end;

function TDbgSymbolValue.GetAddress: TDbgPtr;
begin
  Result := 0;
end;

function TDbgSymbolValue.GetDataAddress: TDbgPtr;
begin
  Result := 0;
end;

function TDbgSymbolValue.GetDataSize: Integer;
begin
  Result := 0;
end;

function TDbgSymbolValue.GetSize: Integer;
begin
  Result := 0;
end;

function TDbgSymbolValue.GetAsBool: Boolean;
begin
  Result := False;
end;

function TDbgSymbolValue.GetAsCardinal: QWord;
begin
  Result := 0;
end;

function TDbgSymbolValue.GetAsInteger: Int64;
begin
  Result := 0;
end;

{ TDbgInfoAddressContext }

function TDbgInfoAddressContext.GetMemReader: TFpDbgMemReaderBase;
begin
  Result := nil;
end;

function TDbgInfoAddressContext.GetSizeOfAddress: Integer;
begin
  Result := -1;
end;

function TDbgInfoAddressContext.GetSymbolAtAddress: TDbgSymbol;
begin
  Result := nil;
end;

function TDbgInfoAddressContext.FindSymbol(const AName: String): TDbgSymbol;
begin
  Result := nil;
end;

{ TDbgSymbol }

constructor TDbgSymbol.Create(const AName: String);
begin
  inherited Create;
  AddReference;
  if AName <> '' then
    SetName(AName);
end;

constructor TDbgSymbol.Create(const AName: String; AKind: TDbgSymbolKind; AAddress: TDbgPtr);
begin
  Create(AName);
  SetKind(AKind);
  FAddress := AAddress;
end;

destructor TDbgSymbol.Destroy;
begin
  SetTypeInfo(nil);
  inherited Destroy;
end;

function TDbgSymbol.TypeCastValue(AValue: TDbgSymbolValue): TDbgSymbolValue;
begin
  Result := nil;
end;

function TDbgSymbol.GetAddress: TDbgPtr;
begin
  if not(sfiAddress in FEvaluatedFields) then
    AddressNeeded;
  Result := FAddress;
end;

function TDbgSymbol.GetTypeInfo: TDbgSymbol;
begin
  if not(sfiTypeInfo in FEvaluatedFields) then
    TypeInfoNeeded;
  Result := FTypeInfo;
end;

function TDbgSymbol.GetMemberVisibility: TDbgSymbolMemberVisibility;
begin
  if not(sfiMemberVisibility in FEvaluatedFields) then
    MemberVisibilityNeeded;
  Result := FMemberVisibility;
end;

function TDbgSymbol.GetValueObject: TDbgSymbolValue;
begin
  Result := nil;
end;

function TDbgSymbol.GetKind: TDbgSymbolKind;
begin
  if not(sfiKind in FEvaluatedFields) then
    KindNeeded;
  Result := FKind;
end;

function TDbgSymbol.GetName: String;
begin
  if not(sfiName in FEvaluatedFields) then
    NameNeeded;
  Result := FName;
end;

function TDbgSymbol.GetSize: Integer;
begin
  if not(sfiSize in FEvaluatedFields) then
    SizeNeeded;
  Result := FSize;
end;

function TDbgSymbol.GetSymbolType: TDbgSymbolType;
begin
  if not(sfiSymType in FEvaluatedFields) then
    SymbolTypeNeeded;
  Result := FSymbolType;
end;

function TDbgSymbol.GetHasBounds: Boolean;
begin
  Result := False;
end;

function TDbgSymbol.GetOrdHighBound: Int64;
begin
  Result := 0;
end;

function TDbgSymbol.GetOrdLowBound: Int64;
begin
  Result := 0;
end;

function TDbgSymbol.GetHasOrdinalValue: Boolean;
begin
  Result := False;
end;

function TDbgSymbol.GetOrdinalValue: Int64;
begin
  Result := 0;
end;

function TDbgSymbol.GetMember(AIndex: Integer): TDbgSymbol;
begin
  Result := nil;
end;

function TDbgSymbol.GetMemberByName(AIndex: String): TDbgSymbol;
begin
  Result := nil;
end;

function TDbgSymbol.GetMemberCount: Integer;
begin
  Result := 0;
end;

procedure TDbgSymbol.SetAddress(AValue: TDbgPtr);
begin
  FAddress := AValue;
  Include(FEvaluatedFields, sfiAddress);
end;

procedure TDbgSymbol.SetKind(AValue: TDbgSymbolKind);
begin
  FKind := AValue;
  Include(FEvaluatedFields, sfiKind);
end;

procedure TDbgSymbol.SetSymbolType(AValue: TDbgSymbolType);
begin
  FSymbolType := AValue;
  Include(FEvaluatedFields, sfiSymType);
end;

procedure TDbgSymbol.SetSize(AValue: Integer);
begin
  FSize := AValue;
  Include(FEvaluatedFields, sfiSize);
end;

procedure TDbgSymbol.SetTypeInfo(AValue: TDbgSymbol);
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

procedure TDbgSymbol.SetMemberVisibility(AValue: TDbgSymbolMemberVisibility);
begin
  FMemberVisibility := AValue;
  Include(FEvaluatedFields, sfiMemberVisibility);
end;

procedure TDbgSymbol.SetName(AValue: String);
begin
  FName := AValue;
  Include(FEvaluatedFields, sfiName);
end;

function TDbgSymbol.GetChild(AIndex: Integer): TDbgSymbol;
begin
  result := nil;
end;

function TDbgSymbol.GetColumn: Cardinal;
begin
  Result := 0;
end;

function TDbgSymbol.GetCount: Integer;
begin
  Result := 0;
end;

function TDbgSymbol.GetFile: String;
begin
  Result := '';
end;

function TDbgSymbol.GetFlags: TDbgSymbolFlags;
begin
  Result := [];
end;

function TDbgSymbol.GetLine: Cardinal;
begin
  Result := 0;
end;

function TDbgSymbol.GetParent: TDbgSymbol;
begin
  Result := nil;
end;

procedure TDbgSymbol.KindNeeded;
begin
  SetKind(skNone);
end;

procedure TDbgSymbol.NameNeeded;
begin
  SetName('');
end;

procedure TDbgSymbol.SymbolTypeNeeded;
begin
  SetSymbolType(stNone);
end;

procedure TDbgSymbol.AddressNeeded;
begin
  SetAddress(0);
end;

procedure TDbgSymbol.SizeNeeded;
begin
  SetSize(0);
end;

procedure TDbgSymbol.TypeInfoNeeded;
begin
  SetTypeInfo(nil);
end;

procedure TDbgSymbol.MemberVisibilityNeeded;
begin
  SetMemberVisibility(svPrivate);
end;

{ TDbgSymbolForwarder }

procedure TDbgSymbolForwarder.SetForwardToSymbol(AValue: TDbgSymbol);
begin
  FForwardToSymbol := AValue;
  EvaluatedFields :=  EvaluatedFields + [sfiForwardToSymbol];
end;

procedure TDbgSymbolForwarder.ForwardToSymbolNeeded;
begin
  SetForwardToSymbol(nil);
end;

function TDbgSymbolForwarder.GetForwardToSymbol: TDbgSymbol;
begin
  if TMethod(@ForwardToSymbolNeeded).Code = Pointer(@TDbgSymbolForwarder.ForwardToSymbolNeeded) then
    exit(nil);

  if not(sfiForwardToSymbol in EvaluatedFields) then
    ForwardToSymbolNeeded;
  Result := FForwardToSymbol;
end;

procedure TDbgSymbolForwarder.KindNeeded;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetKind(p.Kind)
  else
    SetKind(skNone);  //  inherited KindNeeded;
end;

procedure TDbgSymbolForwarder.NameNeeded;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetName(p.Name)
  else
    SetName('');  //  inherited NameNeeded;
end;

procedure TDbgSymbolForwarder.SymbolTypeNeeded;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetSymbolType(p.SymbolType)
  else
    SetSymbolType(stNone);  //  inherited SymbolTypeNeeded;
end;

procedure TDbgSymbolForwarder.SizeNeeded;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetSize(p.Size)
  else
    SetSize(0);  //  inherited SizeNeeded;
end;

procedure TDbgSymbolForwarder.TypeInfoNeeded;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetTypeInfo(p.TypeInfo)
  else
    SetTypeInfo(nil);  //  inherited TypeInfoNeeded;
end;

procedure TDbgSymbolForwarder.MemberVisibilityNeeded;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    SetMemberVisibility(p.MemberVisibility)
  else
    SetMemberVisibility(svPrivate);  //  inherited MemberVisibilityNeeded;
end;

function TDbgSymbolForwarder.GetFlags: TDbgSymbolFlags;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Flags
  else
    Result := [];  //  Result := inherited GetFlags;
end;

function TDbgSymbolForwarder.GetValueObject: TDbgSymbolValue;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Value
  else
    Result := nil;  //  Result := inherited Value;
end;

function TDbgSymbolForwarder.GetHasOrdinalValue: Boolean;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.HasOrdinalValue
  else
    Result := False;  //  Result := inherited GetHasOrdinalValue;
end;

function TDbgSymbolForwarder.GetOrdinalValue: Int64;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.OrdinalValue
  else
    Result := 0;  //  Result := inherited GetOrdinalValue;
end;

function TDbgSymbolForwarder.GetHasBounds: Boolean;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.HasBounds
  else
    Result := False;  //  Result := inherited GetHasBounds;
end;

function TDbgSymbolForwarder.GetOrdLowBound: Int64;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.OrdLowBound
  else
    Result := 0;  //  Result := inherited GetOrdLowBound;
end;

function TDbgSymbolForwarder.GetOrdHighBound: Int64;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.OrdHighBound
  else
    Result := 0;  //  Result := inherited GetOrdHighBound;
end;

function TDbgSymbolForwarder.GetMember(AIndex: Integer): TDbgSymbol;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.Member[AIndex]
  else
    Result := nil;  //  Result := inherited GetMember(AIndex);
end;

function TDbgSymbolForwarder.GetMemberByName(AIndex: String): TDbgSymbol;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.MemberByName[AIndex]
  else
    Result := nil;  //  Result := inherited GetMemberByName(AIndex);
end;

function TDbgSymbolForwarder.GetMemberCount: Integer;
var
  p: TDbgSymbol;
begin
  p := GetForwardToSymbol;
  if p <> nil then
    Result := p.MemberCount
  else
    Result := 0;  //  Result := inherited GetMemberCount;
end;

{ TDbgInfo }

constructor TDbgInfo.Create(ALoader: TDbgImageLoader);
begin
  inherited Create;
end;

function TDbgInfo.FindContext(AAddress: TDbgPtr): TDbgInfoAddressContext;
begin
  Result := nil;
end;

function TDbgInfo.FindSymbol(const AName: String): TDbgSymbol;
begin
  Result := nil;
end;

function TDbgInfo.FindSymbol(AAddress: TDbgPtr): TDbgSymbol;
begin
  Result := nil;
end;

function TDbgInfo.GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
begin
  Result := 0;
end;

procedure TDbgInfo.SetHasInfo;
begin
  FHasInfo := True;
end;

end.


unit FpDbgDwarfFreePascal;

{$mode objfpc}{$H+}
{$TYPEDADDRESS on}

interface

uses
  Classes, SysUtils, Types, math, FpDbgDwarfDataClasses, FpDbgDwarf, FpDbgInfo,
  FpDbgUtil, FpDbgDwarfConst, FpErrorMessages, FpdMemoryTools, DbgIntfBaseTypes,
  LazLoggerBase;

type

  {%Region * ***** SymbolClassMap ***** *}

  { TFpDwarfFreePascalSymbolClassMap }

  TFpDwarfFreePascalSymbolClassMap = class(TFpDwarfDefaultSymbolClassMap)
  strict private
    class var ExistingClassMap: TFpSymbolDwarfClassMap;
  private
    FCompilerVersion: Cardinal;
  protected
    function CanHandleCompUnit(ACU: TDwarfCompilationUnit; AHelperData: Pointer): Boolean; override;
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; override;
  public
    class function GetInstanceForCompUnit(ACU: TDwarfCompilationUnit): TFpSymbolDwarfClassMap; override;
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
  public
    constructor Create(ACU: TDwarfCompilationUnit; AHelperData: Pointer); override;
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpSymbol;
      ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfFreePascalSymbolClassMapDwarf2 }

  TFpDwarfFreePascalSymbolClassMapDwarf2 = class(TFpDwarfFreePascalSymbolClassMap)
  strict private
    class var ExistingClassMap: TFpSymbolDwarfClassMap;
  protected
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; override;
  public
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
  public
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    //class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpSymbol;
    //  ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfFreePascalSymbolClassMapDwarf3 }

  TFpDwarfFreePascalSymbolClassMapDwarf3 = class(TFpDwarfFreePascalSymbolClassMap)
  strict private
    class var ExistingClassMap: TFpSymbolDwarfClassMap;
  protected
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; override;
  public
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
  public
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    //class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpSymbol;
    //  ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  {%EndRegion }

  {%Region * ***** Context ***** *}

  { TFpDwarfFreePascalAddressContext }

  TFpDwarfFreePascalAddressContext = class(TFpDwarfInfoAddressContext)
  private
    FOuterNestContext: TFpDbgInfoContext;
    FOuterNotFound: Boolean;
  protected
    function FindLocalSymbol(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpValue): Boolean; override;
  public
    destructor Destroy; override;
  end;

  {%EndRegion }

  {%Region * ***** Value & Types ***** *}

  (* *** Class vs ^Record vs ^Object *** *)

  { TFpSymbolDwarfFreePascalTypeDeclaration }

  TFpSymbolDwarfFreePascalTypeDeclaration = class(TFpSymbolDwarfTypeDeclaration)
  protected
   // fpc encodes classes as pointer, not ref (so Obj1 = obj2 compares the pointers)
   // typedef > pointer > srtuct
   // while a pointer to class/object: pointer > typedef > ....
    function DoGetNestedTypeInfo: TFpSymbolDwarfType; override;
  end;

  { TFpSymbolDwarfFreePascalTypePointer }

  TFpSymbolDwarfFreePascalTypePointer = class(TFpSymbolDwarfTypePointer)
  private
    FIsInternalPointer: Boolean;
    function GetIsInternalPointer: Boolean; inline;
    function IsInternalDynArrayPointer: Boolean; inline;
  protected
    procedure TypeInfoNeeded; override;
    procedure KindNeeded; override;
    function DoReadStride(AValueObj: TFpValueDwarf; out AStride: TFpDbgValueSize): Boolean; override;
    procedure ForwardToSymbolNeeded; override;
    function GetNextTypeInfoForDataAddress(ATargetType: TFpSymbolDwarfType): TFpSymbolDwarfType; override;
    function GetDataAddressNext(AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation;
      out ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean; override;
    function GetTypedValueObject(ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType = nil): TFpValueDwarf; override;
    function DoReadDataSize(const AValueObj: TFpValue; out ADataSize: TFpDbgValueSize): Boolean; override;
  public
    property IsInternalPointer: Boolean read GetIsInternalPointer write FIsInternalPointer; // Class (also DynArray, but DynArray is handled without this)
  end;

  { TFpSymbolDwarfFreePascalTypeStructure }

  TFpSymbolDwarfFreePascalTypeStructure = class(TFpSymbolDwarfTypeStructure)
  protected
    procedure KindNeeded; override;
  end;

  (* *** Record vs ShortString *** *)

  { TFpSymbolDwarfV2FreePascalTypeStructure }

  TFpSymbolDwarfV2FreePascalTypeStructure = class(TFpSymbolDwarfFreePascalTypeStructure)
  private
    FIsShortString: (issUnknown, issShortString, issStructure);
    function IsShortString: Boolean;
  protected
    function GetTypedValueObject(ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType = nil): TFpValueDwarf; override;
    procedure KindNeeded; override;
    function GetNestedSymbolCount: Integer; override;
    //function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
  end;

  { TFpValueDwarfV2FreePascalShortString }

  TFpValueDwarfV2FreePascalShortString = class(TFpValueDwarf)
  protected
    function IsValidTypeCast: Boolean; override;
    function GetInternMemberByName(AIndex: String): TFpValue;
    procedure Reset; override;
  private
    FValue: String;
    FValueDone: Boolean;
  protected
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

  (* *** "Open Array" in params *** *)

  { TFpSymbolDwarfFreePascalSymbolTypeArray }

  TFpSymbolDwarfFreePascalSymbolTypeArray = class(TFpSymbolDwarfTypeArray)
  public
    function GetTypedValueObject(ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType = nil): TFpValueDwarf; override;
  end;

  { TFpValueDwarfFreePascalArray }

  TFpValueDwarfFreePascalArray = class(TFpValueDwarfArray)
  protected
    function GetMemberCount: Integer; override;
    function DoGetStride(out AStride: TFpDbgValueSize): Boolean; override;
    function DoGetMainStride(out AStride: TFpDbgValueSize): Boolean; override;
    function DoGetDimStride(AnIndex: integer; out AStride: TFpDbgValueSize): Boolean; override;
  end;

  (* *** Array vs AnsiString *** *)

  { TFpSymbolDwarfV3FreePascalSymbolTypeArray }

  TFpSymbolDwarfV3FreePascalSymbolTypeArray = class(TFpSymbolDwarfFreePascalSymbolTypeArray)
  private type
    TArrayOrStringType = (iasUnknown, iasArray, iasShortString, iasAnsiString, iasUnicodeString);
  private
    FArrayOrStringType: TArrayOrStringType;
    function GetInternalStringType: TArrayOrStringType;
  protected
    procedure KindNeeded; override;
  public
    function GetTypedValueObject(ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType = nil): TFpValueDwarf; override;
  end;

  { TFpValueDwarfV3FreePascalString }

  TFpValueDwarfV3FreePascalString = class(TFpValueDwarf) // short & ansi...
  private
    FValue: String;
    FValueDone: Boolean;
  protected
    function IsValidTypeCast: Boolean; override;
    procedure Reset; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

  {%EndRegion }

implementation

{ TFpDwarfFreePascalSymbolClassMap }

function TFpDwarfFreePascalSymbolClassMap.CanHandleCompUnit(
  ACU: TDwarfCompilationUnit; AHelperData: Pointer): Boolean;
begin
  Result := (FCompilerVersion = PtrUInt(AHelperData)) and
            inherited CanHandleCompUnit(ACU, AHelperData);
end;

class function TFpDwarfFreePascalSymbolClassMap.GetExistingClassMap: PFpDwarfSymbolClassMap;
begin
  Result := @ExistingClassMap;
end;

class function TFpDwarfFreePascalSymbolClassMap.GetInstanceForCompUnit(
  ACU: TDwarfCompilationUnit): TFpSymbolDwarfClassMap;
var
  s: String;
  i, j, AVersion: Integer;
begin
  AVersion := 0;
  s := LowerCase(ACU.Producer)+' ';
  i := pos('free pascal', s) + 11;

  if i > 11 then begin
    while (i < Length(s)) and (s[i] in [' ', #9]) do
      inc(i);
    delete(s, 1, i - 1);
    i := pos('.', s);
    if (i > 1) then begin
      j := StrToIntDef(copy(s, 1, i - 1), 0);
      if (j >= 0) then
        AVersion := j * $10000;
      delete(s, 1, i);
    end;
    if (AVersion > 0) then begin
      i := pos('.', s);
      if (i > 1) then begin
        j := StrToIntDef(copy(s, 1, i - 1), 0);
        if (j >= 0) and (j < 99) then
          AVersion := AVersion + j * $100
        else
          AVersion := 0;
        delete(s, 1, i);
      end;
    end;
    if (AVersion > 0) then begin
      i := pos(' ', s);
      if (i > 1) then begin
        j := StrToIntDef(copy(s, 1, i - 1), 0);
        if (j >= 0) and (j < 99) then
          AVersion := AVersion + j
        else
          AVersion := 0;
      end;
    end;
  end;

  Result := DoGetInstanceForCompUnit(ACU, Pointer(PtrUInt(AVersion)));
end;

class function TFpDwarfFreePascalSymbolClassMap.ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean;
var
  s: String;
begin
  s := LowerCase(ACU.Producer);
  Result := pos('free pascal', s) > 0;
end;

constructor TFpDwarfFreePascalSymbolClassMap.Create(ACU: TDwarfCompilationUnit;
  AHelperData: Pointer);
begin
  FCompilerVersion := PtrUInt(AHelperData);
  inherited Create(ACU, AHelperData);
end;

function TFpDwarfFreePascalSymbolClassMap.GetDwarfSymbolClass(
  ATag: Cardinal): TDbgDwarfSymbolBaseClass;
begin
  case ATag of
    DW_TAG_typedef:          Result := TFpSymbolDwarfFreePascalTypeDeclaration;
    DW_TAG_pointer_type:     Result := TFpSymbolDwarfFreePascalTypePointer;
    DW_TAG_structure_type,
    DW_TAG_class_type:       Result := TFpSymbolDwarfFreePascalTypeStructure;
    DW_TAG_array_type:       Result := TFpSymbolDwarfFreePascalSymbolTypeArray;
    else                     Result := inherited GetDwarfSymbolClass(ATag);
  end;
end;

function TFpDwarfFreePascalSymbolClassMap.CreateContext(AThreadId, AStackFrame: Integer;
  AnAddress: TDBGPtr; ASymbol: TFpSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext;
begin
  Result := TFpDwarfFreePascalAddressContext.Create(AThreadId, AStackFrame, AnAddress, ASymbol, ADwarf);
end;

{ TFpDwarfFreePascalSymbolClassMapDwarf2 }

class function TFpDwarfFreePascalSymbolClassMapDwarf2.GetExistingClassMap: PFpDwarfSymbolClassMap;
begin
  Result := @ExistingClassMap;
end;

class function TFpDwarfFreePascalSymbolClassMapDwarf2.ClassCanHandleCompUnit(
  ACU: TDwarfCompilationUnit): Boolean;
begin
  Result := inherited ClassCanHandleCompUnit(ACU);
  Result := Result and (ACU.Version < 3);
end;

function TFpDwarfFreePascalSymbolClassMapDwarf2.GetDwarfSymbolClass(
  ATag: Cardinal): TDbgDwarfSymbolBaseClass;
begin
  case ATag of
    DW_TAG_structure_type:
      Result := TFpSymbolDwarfV2FreePascalTypeStructure; // maybe record
  //  // TODO:
  //  //DW_TAG_reference_type:   Result := TFpSymbolDwarfTypeRef;
  //  //DW_TAG_typedef:          Result := TFpSymbolDwarfTypeDeclaration;
  //  //DW_TAG_pointer_type:     Result := TFpSymbolDwarfTypePointer;
  //  //
  //  //DW_TAG_base_type:        Result := TFpSymbolDwarfTypeBasic;
  //  //DW_TAG_subrange_type:    Result := TFpSymbolDwarfTypeSubRange;
  //  //DW_TAG_enumeration_type: Result := TFpSymbolDwarfTypeEnum;
  //  //DW_TAG_enumerator:       Result := TFpSymbolDwarfDataEnumMember;
  //  //DW_TAG_array_type:       Result := TFpSymbolDwarfTypeArray;
  //  ////
  //  //DW_TAG_compile_unit:     Result := TFpSymbolDwarfUnit;
  //
    else
      Result := inherited GetDwarfSymbolClass(ATag);
  end;
end;

{ TFpDwarfFreePascalSymbolClassMapDwarf3 }

class function TFpDwarfFreePascalSymbolClassMapDwarf3.GetExistingClassMap: PFpDwarfSymbolClassMap;
begin
  Result := @ExistingClassMap;
end;

class function TFpDwarfFreePascalSymbolClassMapDwarf3.ClassCanHandleCompUnit(
  ACU: TDwarfCompilationUnit): Boolean;
begin
  Result := inherited ClassCanHandleCompUnit(ACU);
  Result := Result and (ACU.Version >= 3);
end;

function TFpDwarfFreePascalSymbolClassMapDwarf3.GetDwarfSymbolClass(
  ATag: Cardinal): TDbgDwarfSymbolBaseClass;
begin
  case ATag of
    DW_TAG_array_type:
      Result := TFpSymbolDwarfV3FreePascalSymbolTypeArray;
  //  DW_TAG_structure_type:
  //    Result := TFpSymbolDwarfV2FreePascalTypeStructure; // maybe record
  //  // TODO:
  //  //DW_TAG_reference_type:   Result := TFpSymbolDwarfTypeRef;
  //  //DW_TAG_typedef:          Result := TFpSymbolDwarfTypeDeclaration;
  //  //DW_TAG_pointer_type:     Result := TFpSymbolDwarfTypePointer;
  //  //
  //  //DW_TAG_base_type:        Result := TFpSymbolDwarfTypeBasic;
  //  //DW_TAG_subrange_type:    Result := TFpSymbolDwarfTypeSubRange;
  //  //DW_TAG_enumeration_type: Result := TFpSymbolDwarfTypeEnum;
  //  //DW_TAG_enumerator:       Result := TFpSymbolDwarfDataEnumMember;
  //  //DW_TAG_array_type:       Result := TFpSymbolDwarfTypeArray;
  //  ////
  //  //DW_TAG_compile_unit:     Result := TFpSymbolDwarfUnit;
  //
    else
      Result := inherited GetDwarfSymbolClass(ATag);
  end;
end;

{ TFpDwarfFreePascalAddressContext }

function TFpDwarfFreePascalAddressContext.FindLocalSymbol(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpValue): Boolean;
const
  parentfp: string = 'parentfp';
  parentfp2: string = '$parentfp';
  selfname: string = 'self';
  // TODO: get reg num via memreader name-to-num
  RegFp64 = 6;
  RegPc64 = 16;
  RegFp32 = 5;
  RegPc32 = 8;
var
  StartScopeIdx, RegFp, RegPc: Integer;
  ParentFpVal: TFpValue;
  SearchCtx: TFpDwarfFreePascalAddressContext;
  par_fp, cur_fp, prev_fp, pc: TDbgPtr;
  d, i: Integer;
  ParentFpSym: TFpSymbolDwarf;
begin
  if Dwarf.Image64Bit then begin
    RegFP := RegFp64;
    RegPc := RegPc64;
  end
  else begin
    RegFP := RegFp32;
    RegPc := RegPc32;
  end;
  Result := False;
  if (Length(AName) = length(selfname)) and (CompareUtf8BothCase(PNameUpper, PNameLower, @selfname[1])) then begin
    ADbgValue := GetSelfParameter;
    if ADbgValue <> nil then begin
      ADbgValue.AddReference;
      Result := True;
      exit;
    end;
  end;

  StartScopeIdx := InfoEntry.ScopeIndex;
  Result := inherited FindLocalSymbol(AName, PNameUpper, PNameLower, InfoEntry, ADbgValue);
  if Result then
    exit;

  if FOuterNotFound then
    exit;

  if FOuterNestContext <> nil then begin
    ADbgValue := FOuterNestContext.FindSymbol(AName); // TODO: pass upper/lower
    Result := True; // self, global was done by outer
    exit;
  end;


  InfoEntry.ScopeIndex := StartScopeIdx;
  if not InfoEntry.GoNamedChildEx(@parentfp[1], @parentfp[1]) then begin
    InfoEntry.ScopeIndex := StartScopeIdx;
    if not InfoEntry.GoNamedChildEx(@parentfp2[1], @parentfp2[1]) then begin
      FOuterNotFound := True;
      exit;
    end;
  end;

  ParentFpSym := TFpSymbolDwarf.CreateSubClass(AName, InfoEntry);
  ParentFpVal := ParentFpSym.Value;
  ApplyContext(ParentFpVal);
  if not (svfOrdinal in ParentFpVal.FieldFlags) then begin
    DebugLn('no ordinal for parentfp');
    ParentFpSym.ReleaseReference;
    ParentFpVal.ReleaseReference;
    FOuterNotFound := True;
    exit;
  end;

  par_fp := ParentFpVal.AsCardinal;
  ParentFpVal.ReleaseReference;
  ParentFpSym.ReleaseReference;
    DebugLn(['par_fp=',par_fp]);
  if par_fp = 0 then begin
    DebugLn('no ordinal for parentfp');
    FOuterNotFound := True;
    exit;
  end;

  i := StackFrame + 1;
  SearchCtx := TFpDwarfFreePascalAddressContext.Create(ThreadId, i, 0, Symbol, Dwarf);

  cur_fp := 0;
  if MemManager.ReadRegister(RegFp, cur_fp, Self) then begin
    if cur_fp > par_fp then
      d := -1  // cur_fp must go down
    else
      d := 1;  // cur_fp must go up
    while not (cur_fp = par_fp) do begin
      SearchCtx.StackFrame := i;
      // TODO: get reg num via memreader name-to-num
      prev_fp := cur_fp;
      if not MemManager.ReadRegister(RegFp, cur_fp, SearchCtx) then
        break;
      inc(i);
      if (cur_fp = prev_fp) or ((cur_fp < prev_fp) xor (d = -1)) then
        break;  // wrong direction
      if i > StackFrame + 200 then break; // something wrong? // TODO better check
    end;
    dec(i);
  end;

  if (par_fp <> cur_fp) or (cur_fp = 0) or
     (i <= 0) or
     not MemManager.ReadRegister(RegPc, pc, SearchCtx)
  then begin
    FOuterNotFound := True;
    SearchCtx.ReleaseReference;
    exit;
  end;

  SearchCtx.ReleaseReference;

  FOuterNestContext := Dwarf.FindContext(ThreadId, i, pc);

  ADbgValue := FOuterNestContext.FindSymbol(AName); // TODO: pass upper/lower
  Result := True; // self, global was done by outer
end;

destructor TFpDwarfFreePascalAddressContext.Destroy;
begin
  FOuterNestContext.ReleaseReference;
  inherited Destroy;
end;

{ TFpSymbolDwarfV2FreePascalTypeStructure }

function TFpSymbolDwarfV2FreePascalTypeStructure.IsShortString: Boolean;
var
  LenSym, StSym, StSymType: TFpSymbol;
begin
  if FIsShortString <> issUnknown then
    exit(FIsShortString = issShortString);

  Result := False;
  FIsShortString := issStructure;
  if (inherited NestedSymbolCount <> 2) then
    exit;

  if (Name <> 'ShortString') and (Name <> 'LongString') then  // DWARF-2 => user types are all caps
    exit;

  LenSym := inherited NestedSymbolByName['length'];
  if (LenSym = nil) or (LenSym.Kind <> skCardinal) // or (LenSym.Size <> 1) // not implemented yet
  then
    exit;

  StSym := inherited NestedSymbolByName['st'];
  if (StSym = nil) then
    exit;
  StSymType := StSym.TypeInfo;
  if (StSymType = nil) or (StSymType.Kind <> skArray) or not (StSymType is TFpSymbolDwarfTypeArray) then
    exit;

  FIsShortString := issShortString;
  Result := True;
end;

function TFpSymbolDwarfV2FreePascalTypeStructure.GetTypedValueObject(
  ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType): TFpValueDwarf;
begin
  if AnOuterType = nil then
    AnOuterType := Self;
  if not IsShortString then
    Result := inherited GetTypedValueObject(ATypeCast, AnOuterType)
  else
    Result := TFpValueDwarfV2FreePascalShortString.Create(AnOuterType);
end;

procedure TFpSymbolDwarfV2FreePascalTypeStructure.KindNeeded;
begin
  if not IsShortString then
    inherited KindNeeded
  else
    SetKind(skString);
end;

function TFpSymbolDwarfV2FreePascalTypeStructure.GetNestedSymbolCount: Integer;
begin
  if IsShortString then
    Result := 0
  else
    Result := inherited GetNestedSymbolCount;
end;

{ TFpSymbolDwarfFreePascalTypeDeclaration }

function TFpSymbolDwarfFreePascalTypeDeclaration.DoGetNestedTypeInfo: TFpSymbolDwarfType;
var
  ti: TFpSymbolDwarfType;
begin
  Result := inherited DoGetNestedTypeInfo;

  // Is internal class pointer?
  // Do not trigged any cached property of the pointer
  if (Result = nil) or
     not (Result is TFpSymbolDwarfFreePascalTypePointer)
  then
    exit;

  ti := TFpSymbolDwarfFreePascalTypePointer(Result).NestedTypeInfo;
  // only if it is NOT a declaration
  if (ti <> nil) and (ti is TFpSymbolDwarfTypeStructure) then
    TFpSymbolDwarfFreePascalTypePointer(Result).IsInternalPointer := True;
end;

{ TFpSymbolDwarfFreePascalTypePointer }

function TFpSymbolDwarfFreePascalTypePointer.GetIsInternalPointer: Boolean;
begin
  Result := FIsInternalPointer or IsInternalDynArrayPointer;
end;

function TFpSymbolDwarfFreePascalTypePointer.IsInternalDynArrayPointer: Boolean;
var
  ti: TFpSymbol;
begin
  Result := False;
  ti := NestedTypeInfo;  // Same as TypeInfo, but does not try to be forwarded
  Result := (ti <> nil) and (ti is TFpSymbolDwarfTypeArray);
  if Result then
    Result := (sfDynArray in ti.Flags);
end;

procedure TFpSymbolDwarfFreePascalTypePointer.TypeInfoNeeded;
var
  p: TFpSymbol;
begin
  p := NestedTypeInfo;
  if IsInternalPointer and (p <> nil) then
    p := p.TypeInfo;
  SetTypeInfo(p);
end;

procedure TFpSymbolDwarfFreePascalTypePointer.KindNeeded;
var
  k: TDbgSymbolKind;
begin
  if IsInternalPointer then begin
      k := NestedTypeInfo.Kind;
      if k in [skObject, skRecord] then   // TODO
        SetKind(skInterface)
      else
        SetKind(k);
  end
  else
    inherited;
end;

function TFpSymbolDwarfFreePascalTypePointer.DoReadStride(
  AValueObj: TFpValueDwarf; out AStride: TFpDbgValueSize): Boolean;
begin
  if IsInternalPointer then
    Result := NestedTypeInfo.ReadStride(AValueObj, AStride)
  else
    Result := inherited DoReadStride(AValueObj, AStride);
end;

procedure TFpSymbolDwarfFreePascalTypePointer.ForwardToSymbolNeeded;
begin
  if IsInternalPointer then
    SetForwardToSymbol(NestedTypeInfo) // Same as TypeInfo, but does not try to be forwarded
  else
    SetForwardToSymbol(nil); // inherited ForwardToSymbolNeeded;
end;

function TFpSymbolDwarfFreePascalTypePointer.GetNextTypeInfoForDataAddress(
  ATargetType: TFpSymbolDwarfType): TFpSymbolDwarfType;
begin
  if IsInternalPointer then
    Result := NestedTypeInfo
  else
    Result := inherited;
end;

function TFpSymbolDwarfFreePascalTypePointer.GetDataAddressNext(
  AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation; out
  ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean;
begin
  if (not IsInternalPointer) and (ATargetType = nil) then exit(True);

  Result := inherited GetDataAddressNext(AValueObj, AnAddress, ADoneWork, ATargetType);
  if (not Result) or ADoneWork then
    exit;

  Result := AValueObj.MemManager <> nil;
  if not Result then
    exit;
  AnAddress := AValueObj.MemManager.ReadAddress(AnAddress, SizeVal(CompilationUnit.AddressSize));
  Result := IsValidLoc(AnAddress);

  if (not Result) and
     IsError(AValueObj.MemManager.LastError)
  then
    SetLastError(AValueObj, AValueObj.MemManager.LastError);
end;

function TFpSymbolDwarfFreePascalTypePointer.GetTypedValueObject(
  ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType): TFpValueDwarf;
begin
  if AnOuterType = nil then
    AnOuterType := Self;
  if IsInternalPointer then
    Result := NestedTypeInfo.GetTypedValueObject(ATypeCast, AnOuterType)
  else
    Result := inherited GetTypedValueObject(ATypeCast, AnOuterType);
end;

function TFpSymbolDwarfFreePascalTypePointer.DoReadDataSize(
  const AValueObj: TFpValue; out ADataSize: TFpDbgValueSize): Boolean;
begin
  if Kind = skClass then begin
    // TODO: get/adjust a value object to have the deref address // see ConstRefOrExprFromAttrData
    Result := NestedTypeInfo.ReadSize(AValueObj, ADataSize);
    if not Result then
      ADataSize := ZeroSize;
  end
  else
    Result := inherited DoReadDataSize(AValueObj, ADataSize);
end;

{ TFpSymbolDwarfFreePascalTypeStructure }

procedure TFpSymbolDwarfFreePascalTypeStructure.KindNeeded;
var
  t: TDbgSymbolKind;
begin
  (* DW_TAG_structure_type
     - Is either objec or record.
     - Except: fpc < 3.0 => can be class or interface too
     DW_TAG_class_type
     - Is either class, interface, or object (object only with virtual methods)

     tested up to fpc 3.2 beta
  *)
  if (InformationEntry.AbbrevTag = DW_TAG_interface_type) then begin
    SetKind(skInterface);
  end
  else
  if TypeInfo <> nil then begin // inheritance
    t := TypeInfo.Kind;
    if t = skRecord then
      t := skObject; // could be skInterface
    SetKind(t); // skClass, skInterface or skObject
  end
  else
  begin
    if NestedSymbolByName['_vptr$TOBJECT'] <> nil then
      SetKind(skClass)
    else
    if NestedSymbolByName['_vptr$'+Name] <> nil then // vptr is only present for skObject with virtual methods/Constructor
      SetKind(skObject)
    else
    if (InformationEntry.AbbrevTag = DW_TAG_class_type) then
      SetKind(skObject)   // could be skInterface  // fix in TFpSymbolDwarfFreePascalTypePointer.KindNeeded
    else
      SetKind(skRecord);  // could be skObject(?) or skInterface   // fix in TFpSymbolDwarfFreePascalTypePointer.KindNeeded
  end;
end;

{ TFpValueDwarfV2FreePascalShortString }

function TFpValueDwarfV2FreePascalShortString.IsValidTypeCast: Boolean;
begin
  // currently only allow this / used by array access
  Result := TypeCastSourceValue is TFpValueConstAddress;
end;

function TFpValueDwarfV2FreePascalShortString.GetInternMemberByName(
  AIndex: String): TFpValue;
begin
  if HasTypeCastInfo then begin
    Result := TypeInfo.GetNestedValueByName(AIndex);
    TFpValueDwarf(Result).StructureValue := Self;
    if (TFpValueDwarf(Result).Context = nil) then
      TFpValueDwarf(Result).Context := Context;
  end
  else
    Result := MemberByName[AIndex];
end;

procedure TFpValueDwarfV2FreePascalShortString.Reset;
begin
  inherited Reset;
  FValueDone := False;
end;

function TFpValueDwarfV2FreePascalShortString.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfString];
end;

function TFpValueDwarfV2FreePascalShortString.GetAsString: AnsiString;
var
  len: QWord;
  Size: TFpDbgValueSize;
  LenSym, StSym: TFpValueDwarf;
begin
  if FValueDone then
    exit(FValue);

  LenSym := TFpValueDwarf(GetInternMemberByName('length'));
  assert(LenSym is TFpValueDwarf, 'LenSym is TFpValueDwarf');
  len := LenSym.AsCardinal;
  LenSym.ReleaseReference;

  if not GetSize(Size) then begin;
    SetLastError(CreateError(fpErrAnyError));
    exit('');
  end;
  if (Size < len) then begin
    SetLastError(CreateError(fpErrAnyError));
    exit('');
  end;

  StSym := TFpValueDwarf(GetInternMemberByName('st'));
  assert(StSym is TFpValueDwarf, 'StSym is TFpValueDwarf');


  SetLength(Result, len);
  if len > 0 then
    if not MemManager.ReadMemory(StSym.DataAddress, SizeVal(len), @Result[1]) then begin
      Result := ''; // TODO: error
      SetLastError(MemManager.LastError);
      StSym.ReleaseReference;
      exit;
    end;
  StSym.ReleaseReference;

  FValue := Result;
  FValueDone := True;
end;

function TFpValueDwarfV2FreePascalShortString.GetAsWideString: WideString;
begin
  Result := GetAsString;
end;

{ TFpSymbolDwarfFreePascalSymbolTypeArray }

function TFpSymbolDwarfFreePascalSymbolTypeArray.GetTypedValueObject(
  ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType): TFpValueDwarf;
begin
  if AnOuterType = nil then
    AnOuterType := Self;
  Result := TFpValueDwarfFreePascalArray.Create(AnOuterType, Self);
end;

{ TFpValueDwarfFreePascalArray }

function TFpValueDwarfFreePascalArray.GetMemberCount: Integer;
var
  t, t2: TFpSymbol;
  Info: TDwarfInformationEntry;
  n: AnsiString;
  UpperBoundSym: TFpSymbolDwarf;
  val: TFpValue;
  l, h: Int64;
  Addr: TFpDbgMemLocation;
begin
  Result := 0;
  t := TypeInfo;
  if (t.Kind <> skArray) or (t.NestedSymbolCount < 1) then // IndexTypeCount;
    exit(inherited GetMemberCount);

  t2 := t.NestedSymbol[0]; // IndexType[0];
  if not (t2 is TFpSymbolDwarfTypeSubRange) then
    exit(inherited GetMemberCount);


  TFpSymbolDwarfTypeSubRange(t2).GetValueBounds(Self, l, h);
  if (l <> 0) or
     (TFpSymbolDwarfTypeSubRange(t2).LowBoundState <> rfConst) or
     (TFpSymbolDwarfTypeSubRange(t2).HighBoundState <> rfNotFound) or
     (TFpSymbolDwarfTypeSubRange(t2).CountState <> rfNotFound)
  then
    exit(inherited GetMemberCount);

  // Check for open array param
  if (t is TFpSymbolDwarfTypeArray) and
     (DbgSymbol is TFpSymbolDwarfDataParameter) // open array exists only as param
  then begin
    Info := TFpSymbolDwarfDataParameter(DbgSymbol).InformationEntry.Clone;
    Info.GoNext;
    if Info.HasValidScope and
       Info.HasAttrib(DW_AT_location) and  // the high param must have a location / cannot be a constant
       Info.ReadName(n)
    then begin
      if (n <> '') and (n[1] = '$') then // dwarf3 // TODO: make required in dwarf3
        delete(n, 1, 1);
      if (copy(n,1,4) = 'high') and (UpperCase(copy(n, 5, length(n))) = UpperCase(DbgSymbol.Name)) then begin
        UpperBoundSym := TFpSymbolDwarf.CreateSubClass('', Info);
        if UpperBoundSym <> nil then begin
          val := UpperBoundSym.Value;
          TFpValueDwarf(val).Context := Context;
          h := Val.AsInteger;
          val.ReleaseReference;
          if h >= 0 then begin
          {$PUSH}{$Q-}
            if QWord(h) > 5000 - 1 then
              h := 5000 - 1;
          {$POP}
          Result := h + 1;
          end
          else
            Result := 0;
          Info.ReleaseReference;
          UpperBoundSym.ReleaseReference;
          exit;
        end;
      end;
    end;
    Info.ReleaseReference;
  end;

  // dynamic array
  if (sfDynArray in t.Flags) and (AsCardinal <> 0) and GetDwarfDataAddress(Addr) then begin
    if not (IsReadableMem(Addr) and (LocToAddr(Addr) > AddressSize)) then
      exit(0); // dyn array, but bad data
    Addr.Address := Addr.Address - AddressSize;
    //debugln(['TFpValueDwarfArray.GetMemberCount  XXXXXXXXXXXXXXX dwarf 2 read len']);
    if MemManager.ReadSignedInt(Addr, SizeVal(AddressSize), h) then begin
      Result := Integer(h)+1;
      exit;
    end
    else
      SetLastError(MemManager.LastError);
    Result := 0;
    exit;
  end;

  // Should not be here. There is no knowledeg how many members there are
  Result := inherited GetMemberCount;
end;

function TFpValueDwarfFreePascalArray.DoGetStride(out AStride: TFpDbgValueSize
  ): Boolean;
begin
  if (TFpDwarfFreePascalSymbolClassMapDwarf3(TypeInfo.CompilationUnit.DwarfSymbolClassMap).FCompilerVersion >= $030300)
  then
    Result := inherited DoGetStride(AStride)
  else
    Result := TFpSymbolDwarfType(TypeInfo.NestedSymbol[0]).ReadStride(Self, AStride);
end;

function TFpValueDwarfFreePascalArray.DoGetMainStride(out
  AStride: TFpDbgValueSize): Boolean;
begin
  if (TFpDwarfFreePascalSymbolClassMapDwarf3(TypeInfo.CompilationUnit.DwarfSymbolClassMap).FCompilerVersion >= $030300)
  then
    Result := inherited DoGetMainStride(AStride)
  else
    Result := GetMemberSize(AStride);
end;

function TFpValueDwarfFreePascalArray.DoGetDimStride(AnIndex: integer; out
  AStride: TFpDbgValueSize): Boolean;
begin
  if (TFpDwarfFreePascalSymbolClassMapDwarf3(TypeInfo.CompilationUnit.DwarfSymbolClassMap).FCompilerVersion >= $030300)
  then
    Result := inherited DoGetDimStride(AnIndex, AStride)
  else
  begin
    Result := True;
    AStride := ZeroSize;
  end;
end;

{ TFpSymbolDwarfV3FreePascalSymbolTypeArray }

function TFpSymbolDwarfV3FreePascalSymbolTypeArray.GetInternalStringType: TArrayOrStringType;
var
  Info: TDwarfInformationEntry;
  t: Cardinal;
  t2: TFpSymbol;
  CharSize: TFpDbgValueSize;
begin
  Result := FArrayOrStringType;
  if Result <> iasUnknown then
    exit;

  FArrayOrStringType := iasArray;
  Result := FArrayOrStringType;

  t2 := TypeInfo;
  if (t2 = nil) or (t2.Kind <> skChar) then
    exit;

  // TODO: check lowbound = 1 (const)

  Info := InformationEntry.FirstChild;
  if Info = nil then
    exit;

  while Info.HasValidScope do begin
    t := Info.AbbrevTag;
    if (t = DW_TAG_enumeration_type) then
      break;
    if (t = DW_TAG_subrange_type) then begin
      if Info.HasAttrib(DW_AT_byte_stride) or Info.HasAttrib(DW_AT_type) then
        break;

      // This is a string
      // TODO: check the location parser, if it is a reference
      //FIsShortString := iasShortString;
      if not t2.ReadSize(nil, CharSize) then
        CharSize := ZeroSize; // TODO: error
      if (CharSize.Size = 2) then
        FArrayOrStringType := iasUnicodeString
      else
        FArrayOrStringType := iasAnsiString;
      Result := FArrayOrStringType;
      break;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

function TFpSymbolDwarfV3FreePascalSymbolTypeArray.GetTypedValueObject(
  ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType): TFpValueDwarf;
begin
  if AnOuterType = nil then
    AnOuterType := Self;
  if GetInternalStringType in [{iasShortString,} iasAnsiString, iasUnicodeString] then
    Result := TFpValueDwarfV3FreePascalString.Create(AnOuterType)
  else
    Result := inherited GetTypedValueObject(ATypeCast, AnOuterType);
end;

procedure TFpSymbolDwarfV3FreePascalSymbolTypeArray.KindNeeded;
begin
  case GetInternalStringType of
    iasShortString:
      SetKind(skString);
    iasAnsiString:
      SetKind(skString); // TODO
    iasUnicodeString:
      SetKind(skWideString);
    else
      inherited KindNeeded;
  end;
end;

{ TFpValueDwarfV3FreePascalString }

function TFpValueDwarfV3FreePascalString.IsValidTypeCast: Boolean;
var
  f: TFpValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(TypeInfo.Kind in [skString, skWideString], 'TFpValueDwarfArray.IsValidTypeCast: TypeInfo.Kind = skArray');

  f := TypeCastSourceValue.FieldFlags;
  if (f * [svfAddress, svfSize, svfSizeOfPointer] = [svfAddress]) or
     (svfOrdinal in f)
  then
    exit;

  //if sfDynArray in TypeInfo.Flags then begin
  //  // dyn array
  //  if (svfOrdinal in f)then
  //    exit;
  //  if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
  //     (TypeCastSourceValue.Size = TypeInfo.CompilationUnit.AddressSize)
  //  then
  //    exit;
  //  if (f * [svfAddress, svfSizeOfPointer] = [svfAddress, svfSizeOfPointer]) then
  //    exit;
  //end
  //else begin
  //  // stat array
  //  if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
  //     (TypeCastSourceValue.Size = TypeInfo.Size)
  //  then
  //    exit;
  //end;
  Result := False;
end;

procedure TFpValueDwarfV3FreePascalString.Reset;
begin
  inherited Reset;
  FValueDone := False;
end;

function TFpValueDwarfV3FreePascalString.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  case TypeInfo.Kind of
    skWideString: Result := Result + [svfWideString];
    else          Result := Result + [svfString];
  end;
end;

function TFpValueDwarfV3FreePascalString.GetAsString: AnsiString;
var
  t, t2: TFpSymbol;
  LowBound, HighBound, i: Int64;
  Addr, Addr2: TFpDbgMemLocation;
  WResult: UnicodeString;
  AttrData: TDwarfAttribData;
begin
  if FValueDone then
    exit(FValue);

  // TODO: error handling
  FValue := '';
  Result := '';
  FValueDone := True;

  // get length
  t := TypeInfo;
  if t.NestedSymbolCount < 1 then // subrange type
    exit;

  t2 := t.NestedSymbol[0]; // subrange type
  if not( (t2 is TFpSymbolDwarfType) and TFpSymbolDwarfType(t2).GetValueBounds(self, LowBound, HighBound) )
  then
    exit;

  GetDwarfDataAddress(Addr);
  if (not IsValidLoc(Addr)) and (svfOrdinal in TypeCastSourceValue.FieldFlags) then
    Addr := TargetLoc(TypeCastSourceValue.AsCardinal);
  if not IsReadableLoc(Addr) then
    exit;

  assert((TypeInfo <> nil) and (TypeInfo.CompilationUnit <> nil) and (TypeInfo.CompilationUnit.DwarfSymbolClassMap is TFpDwarfFreePascalSymbolClassMapDwarf3), 'TFpValueDwarfV3FreePascalString.GetAsString: (Owner <> nil) and (Owner.CompilationUnit <> nil) and (TypeInfo.CompilationUnit.DwarfSymbolClassMap is TFpDwarfFreePascalSymbolClassMapDwarf3)');
  if (TFpDwarfFreePascalSymbolClassMapDwarf3(TypeInfo.CompilationUnit.DwarfSymbolClassMap).FCompilerVersion > 0) and
     (TFpDwarfFreePascalSymbolClassMapDwarf3(TypeInfo.CompilationUnit.DwarfSymbolClassMap).FCompilerVersion < $030100)
  then begin
    if t.Kind = skWideString then begin
      if (t2 is TFpSymbolDwarfTypeSubRange) and (LowBound = 1) then begin
        if (TFpSymbolDwarfTypeSubRange(t2).InformationEntry.GetAttribData(DW_AT_upper_bound, AttrData)) and
           (TFpSymbolDwarfTypeSubRange(t2).InformationEntry.AttribForm[AttrData.Idx] = DW_FORM_block1) and
           (IsReadableMem(Addr) and (LocToAddr(Addr) > AddressSize))
        then begin
          // fpc issue 0035359
          // read data and check for DW_OP_shr ?
          Addr2 := Addr;
          Addr2.Address := Addr2.Address - AddressSize;
          if MemManager.ReadSignedInt(Addr2, SizeVal(AddressSize), i) then begin
            if (i shr 1) = HighBound then
              HighBound := i;
          end
        end;
      end;
    end;
  end;

  if HighBound < LowBound then
    exit; // empty string

  // TODO: XXXXX Dynamic max limit
  {$PUSH}{$Q-}
  if QWord(HighBound - LowBound) > 5000 then
    HighBound := LowBound + 5000;
  {$POP}

  if t.Kind = skWideString then begin
    SetLength(WResult, HighBound-LowBound+1);

    if not MemManager.ReadMemory(Addr, SizeVal((HighBound-LowBound+1)*2), @WResult[1]) then begin
      WResult := '';
      SetLastError(MemManager.LastError);
    end;
    Result := WResult;
  end else begin
    SetLength(Result, HighBound-LowBound+1);

    if not MemManager.ReadMemory(Addr, SizeVal(HighBound-LowBound+1), @Result[1]) then begin
      Result := '';
      SetLastError(MemManager.LastError);
    end;
  end;

  FValue := Result;

end;

function TFpValueDwarfV3FreePascalString.GetAsWideString: WideString;
begin
  // todo: widestring, but currently that is encoded as PWideChar
  Result := GetAsString;
end;

initialization
  DwarfSymbolClassMapList.AddMap(TFpDwarfFreePascalSymbolClassMapDwarf2);
  DwarfSymbolClassMapList.AddMap(TFpDwarfFreePascalSymbolClassMapDwarf3);

end.


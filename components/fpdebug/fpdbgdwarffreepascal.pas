unit FpDbgDwarfFreePascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, math, FpDbgDwarfDataClasses, FpDbgDwarf, FpDbgInfo,
  FpDbgUtil, FpDbgDwarfConst, FpErrorMessages, FpdMemoryTools, DbgIntfBaseTypes,
  LazLoggerBase;

type

  (* ***** SymbolClassMap *****
  *)

  { TFpDwarfFreePascalSymbolClassMap }

  TFpDwarfFreePascalSymbolClassMap = class(TFpDwarfDefaultSymbolClassMap)
  strict private
    class var ExistingClassMap: TFpDwarfSymbolClassMap;
  protected
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; override;
  public
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
  public
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol;
      ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfFreePascalSymbolClassMapDwarf2 }

  TFpDwarfFreePascalSymbolClassMapDwarf2 = class(TFpDwarfFreePascalSymbolClassMap)
  strict private
    class var ExistingClassMap: TFpDwarfSymbolClassMap;
  protected
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; override;
  public
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
  public
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    //class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol;
    //  ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfFreePascalSymbolClassMapDwarf3 }

  TFpDwarfFreePascalSymbolClassMapDwarf3 = class(TFpDwarfFreePascalSymbolClassMap)
  strict private
    class var ExistingClassMap: TFpDwarfSymbolClassMap;
  private
    FCompilerVersion: Cardinal;
  protected
    function CanHandleCompUnit(ACU: TDwarfCompilationUnit; AHelperData: Pointer): Boolean; override;
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; override;
  public
    class function GetInstanceForCompUnit(ACU: TDwarfCompilationUnit): TFpDwarfSymbolClassMap; override;
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
  public
    constructor Create(ACU: TDwarfCompilationUnit; AHelperData: Pointer); override;
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    //class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol;
    //  ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  (* ***** Context *****
  *)

  { TFpDwarfFreePascalAddressContext }

  TFpDwarfFreePascalAddressContext = class(TFpDwarfInfoAddressContext)
  private
    FOuterNestContext: TFpDbgInfoContext;
    FOuterNotFound: Boolean;
  protected
    function FindLocalSymbol(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean; override;
  public
    destructor Destroy; override;
  end;

  (* ***** Value & Types *****
  *)

  (* *** Record vs ShortString *** *)

  { TFpDwarf2FreePascalSymbolTypeStructure }

  TFpDwarf2FreePascalSymbolTypeStructure = class(TFpDwarfSymbolTypeStructure)
  private
    FIsShortString: (issUnknown, issShortString, issStructure);
    function IsShortString: Boolean;
  protected
    function GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue; override;
    procedure KindNeeded; override;
    function GetMemberCount: Integer; override;
    //function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
  end;

  { TFpDwarfV2ValueFreePascalShortString }

  TFpDwarfV2ValueFreePascalShortString = class(TFpDwarfValue)
  protected
    function IsValidTypeCast: Boolean; override;
    function GetInternMemberByName(AIndex: String): TFpDbgValue;
    procedure Reset; override;
  private
    FValue: String;
    FValueDone: Boolean;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

  (* *** "Open Array" in params *** *)

  { TFpDwarfFreePascalSymbolTypeArray }

  TFpDwarfFreePascalSymbolTypeArray = class(TFpDwarfSymbolTypeArray)
  protected
    function GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue; override;
  end;

  { TFpDwarfValueFreePascalArray }

  TFpDwarfValueFreePascalArray = class(TFpDwarfValueArray)
  protected
    function GetMemberCount: Integer; override;
  end;

  (* *** Array vs AnsiString *** *)

  { TFpDwarfV3FreePascalSymbolTypeArray }

  TFpDwarfV3FreePascalSymbolTypeArray = class(TFpDwarfFreePascalSymbolTypeArray)
  private type
    TArrayOrStringType = (iasUnknown, iasArray, iasShortString, iasAnsiString, iasUnicodeString);
  private
    FArrayOrStringType: TArrayOrStringType;
    function GetInternalStringType: TArrayOrStringType;
  protected
    function GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue; override;
    procedure KindNeeded; override;
  end;

  { TFpDwarfV3ValueFreePascalString }

  TFpDwarfV3ValueFreePascalString = class(TFpDwarfValue) // short & ansi...
  private
    FValue: String;
    FValueDone: Boolean;
  protected
    function IsValidTypeCast: Boolean; override;
    procedure Reset; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

implementation

{ TFpDwarfFreePascalSymbolClassMap }

class function TFpDwarfFreePascalSymbolClassMap.GetExistingClassMap: PFpDwarfSymbolClassMap;
begin
  Result := @ExistingClassMap;
end;

class function TFpDwarfFreePascalSymbolClassMap.ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean;
var
  s: String;
begin
  s := LowerCase(ACU.Producer);
  Result := pos('free pascal', s) > 0;
end;

function TFpDwarfFreePascalSymbolClassMap.GetDwarfSymbolClass(
  ATag: Cardinal): TDbgDwarfSymbolBaseClass;
begin
  case ATag of
    DW_TAG_array_type:
      Result := TFpDwarfFreePascalSymbolTypeArray;
    else
      Result := inherited GetDwarfSymbolClass(ATag);
  end;
end;

function TFpDwarfFreePascalSymbolClassMap.CreateContext(AThreadId, AStackFrame: Integer;
  AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext;
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
      Result := TFpDwarf2FreePascalSymbolTypeStructure; // maybe record
  //  // TODO:
  //  //DW_TAG_reference_type:   Result := TFpDwarfSymbolTypeRef;
  //  //DW_TAG_typedef:          Result := TFpDwarfSymbolTypeDeclaration;
  //  //DW_TAG_pointer_type:     Result := TFpDwarfSymbolTypePointer;
  //  //
  //  //DW_TAG_base_type:        Result := TFpDwarfSymbolTypeBasic;
  //  //DW_TAG_subrange_type:    Result := TFpDwarfSymbolTypeSubRange;
  //  //DW_TAG_enumeration_type: Result := TFpDwarfSymbolTypeEnum;
  //  //DW_TAG_enumerator:       Result := TFpDwarfSymbolValueEnumMember;
  //  //DW_TAG_array_type:       Result := TFpDwarfSymbolTypeArray;
  //  ////
  //  //DW_TAG_compile_unit:     Result := TFpDwarfSymbolUnit;
  //
    else
      Result := inherited GetDwarfSymbolClass(ATag);
  end;
end;

{ TFpDwarfFreePascalSymbolClassMapDwarf3 }

function TFpDwarfFreePascalSymbolClassMapDwarf3.CanHandleCompUnit(
  ACU: TDwarfCompilationUnit; AHelperData: Pointer): Boolean;
begin
  Result := (FCompilerVersion = PtrUInt(AHelperData)) and
            inherited CanHandleCompUnit(ACU, AHelperData);
end;

class function TFpDwarfFreePascalSymbolClassMapDwarf3.GetExistingClassMap: PFpDwarfSymbolClassMap;
begin
  Result := @ExistingClassMap;
end;

class function TFpDwarfFreePascalSymbolClassMapDwarf3.GetInstanceForCompUnit(
  ACU: TDwarfCompilationUnit): TFpDwarfSymbolClassMap;
var
  s: String;
  i, j, v: Integer;
begin
  s := LowerCase(ACU.Producer)+' ';
  v := 0;
  i := pos('free pascal', s) + 11;
  if i > 11 then begin
    while (i < Length(s)) and (s[i] in [' ', #9]) do
      inc(i);
    delete(s, 1, i - 1);
    i := pos('.', s);
    if (i > 1) then begin
      j := StrToIntDef(copy(s, 1, i - 1), 0);
      if (j >= 0) then
        v := j * $10000;
      delete(s, 1, i);
    end;
    if (v > 0) then begin
      i := pos('.', s);
      if (i > 1) then begin
        j := StrToIntDef(copy(s, 1, i - 1), 0);
        if (j >= 0) and (j < 99) then
          v := v + j * $100
        else
          v := 0;
        delete(s, 1, i);
      end;
    end;
    if (v > 0) then begin
      i := pos(' ', s);
      if (i > 1) then begin
        j := StrToIntDef(copy(s, 1, i - 1), 0);
        if (j >= 0) and (j < 99) then
          v := v + j
        else
          v := 0;
      end;
    end;
  end;

  Result := DoGetInstanceForCompUnit(ACU, Pointer(PtrUInt(v)));
end;

class function TFpDwarfFreePascalSymbolClassMapDwarf3.ClassCanHandleCompUnit(
  ACU: TDwarfCompilationUnit): Boolean;
begin
  Result := inherited ClassCanHandleCompUnit(ACU);
  Result := Result and (ACU.Version >= 3);
end;

constructor TFpDwarfFreePascalSymbolClassMapDwarf3.Create(
  ACU: TDwarfCompilationUnit; AHelperData: Pointer);
begin
  FCompilerVersion := PtrUInt(AHelperData);
  inherited;
end;

function TFpDwarfFreePascalSymbolClassMapDwarf3.GetDwarfSymbolClass(
  ATag: Cardinal): TDbgDwarfSymbolBaseClass;
begin
  case ATag of
    DW_TAG_array_type:
      Result := TFpDwarfV3FreePascalSymbolTypeArray;
  //  DW_TAG_structure_type:
  //    Result := TFpDwarf2FreePascalSymbolTypeStructure; // maybe record
  //  // TODO:
  //  //DW_TAG_reference_type:   Result := TFpDwarfSymbolTypeRef;
  //  //DW_TAG_typedef:          Result := TFpDwarfSymbolTypeDeclaration;
  //  //DW_TAG_pointer_type:     Result := TFpDwarfSymbolTypePointer;
  //  //
  //  //DW_TAG_base_type:        Result := TFpDwarfSymbolTypeBasic;
  //  //DW_TAG_subrange_type:    Result := TFpDwarfSymbolTypeSubRange;
  //  //DW_TAG_enumeration_type: Result := TFpDwarfSymbolTypeEnum;
  //  //DW_TAG_enumerator:       Result := TFpDwarfSymbolValueEnumMember;
  //  //DW_TAG_array_type:       Result := TFpDwarfSymbolTypeArray;
  //  ////
  //  //DW_TAG_compile_unit:     Result := TFpDwarfSymbolUnit;
  //
    else
      Result := inherited GetDwarfSymbolClass(ATag);
  end;
end;

{ TFpDwarfFreePascalAddressContext }

function TFpDwarfFreePascalAddressContext.FindLocalSymbol(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean;
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
  ParentFpVal: TFpDbgValue;
  SearchCtx: TFpDwarfFreePascalAddressContext;
  par_fp, cur_fp, prev_fp, pc: TDbgPtr;
  d, i: Integer;
  ParentFpSym: TFpDwarfSymbol;
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
      AddRefToVal(ADbgValue);
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
    if ADbgValue <> nil then
      AddRefToVal(ADbgValue);
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

  ParentFpSym := TFpDwarfSymbol.CreateSubClass(AName, InfoEntry);
  ParentFpVal := ParentFpSym.Value;
  ApplyContext(ParentFpVal);
  //TFpDwarfSymbol(ADbgValue.DbgSymbol).ParentTypeInfo := TFpDwarfSymbolValueProc(FSymbol);
  if not (svfOrdinal in ParentFpVal.FieldFlags) then begin
    DebugLn('no ordinal for parentfp');
    ParentFpSym.ReleaseReference;
    FOuterNotFound := True;
    exit;
  end;

  par_fp := ParentFpVal.AsCardinal;
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
      not MemManager.ReadRegister(RegPc, pc, SearchCtx)
  then begin
    FOuterNotFound := True;
    SearchCtx.ReleaseReference;
    exit;
  end;

  SearchCtx.ReleaseReference;

  FOuterNestContext := Dwarf.FindContext(ThreadId, i, pc);

  ADbgValue := FOuterNestContext.FindSymbol(AName); // TODO: pass upper/lower
  if ADbgValue <> nil then
    AddRefToVal(ADbgValue);
  Result := True; // self, global was done by outer
end;

destructor TFpDwarfFreePascalAddressContext.Destroy;
begin
  FOuterNestContext.ReleaseReference;
  inherited Destroy;
end;

{ TFpDwarf2FreePascalSymbolTypeStructure }

function TFpDwarf2FreePascalSymbolTypeStructure.IsShortString: Boolean;
var
  LenSym, StSym, StSymType: TFpDbgSymbol;
begin
  if FIsShortString <> issUnknown then
    exit(FIsShortString = issShortString);

  Result := False;
  FIsShortString := issStructure;
  if (inherited MemberCount <> 2) then
    exit;

  LenSym := inherited MemberByName['length'];
  if (LenSym = nil) or (LenSym.Kind <> skCardinal) // or (LenSym.Size <> 1) // not implemented yet
  then
    exit;

  StSym := inherited MemberByName['st'];
  if (StSym = nil) then
    exit;
  StSymType := StSym.TypeInfo;
  if (StSymType = nil) or (StSymType.Kind <> skArray) or not (StSymType is TFpDwarfSymbolTypeArray) then
    exit;

  // If it were a user declared array, fpc puts the stride in the subrange
  if not TFpDwarfSymbolTypeArray(StSymType).InformationEntry.HasAttrib(DW_AT_byte_stride) then
    exit;
  // check the subrange?

  FIsShortString := issShortString;
  Result := True;
end;

function TFpDwarf2FreePascalSymbolTypeStructure.GetTypedValueObject(
  ATypeCast: Boolean): TFpDwarfValue;
begin
  if not IsShortString then
    Result := inherited GetTypedValueObject(ATypeCast)
  else
    Result := TFpDwarfV2ValueFreePascalShortString.Create(Self);
end;

procedure TFpDwarf2FreePascalSymbolTypeStructure.KindNeeded;
begin
  if not IsShortString then
    inherited KindNeeded
  else
    SetKind(skString);
end;

function TFpDwarf2FreePascalSymbolTypeStructure.GetMemberCount: Integer;
begin
  if IsShortString then
    Result := 0
  else
    Result := inherited GetMemberCount;
end;

{ TFpDwarfV2ValueFreePascalShortString }

function TFpDwarfV2ValueFreePascalShortString.IsValidTypeCast: Boolean;
begin
  // currently only allow this / used by array access
  Result := TypeCastSourceValue is TFpDbgValueConstAddress;
end;

function TFpDwarfV2ValueFreePascalShortString.GetInternMemberByName(
  AIndex: String): TFpDbgValue;
var
  tmp: TFpDbgSymbol;
begin
  if HasTypeCastInfo then begin
    Result := nil;
    tmp := TypeCastTargetType.MemberByName[AIndex];
    if (tmp <> nil) then begin
      assert((tmp is TFpDwarfSymbolValue), 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
      Result := tmp.Value;

      TFpDwarfValue(Result).StructureValue := Self;
      if (TFpDwarfValue(Result).Context = nil) then
        TFpDwarfValue(Result).Context := Context;
    end;
  end
  else
    Result := MemberByName[AIndex];
end;

procedure TFpDwarfV2ValueFreePascalShortString.Reset;
begin
  inherited Reset;
  FValueDone := False;
end;

function TFpDwarfV2ValueFreePascalShortString.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfString];
end;

function TFpDwarfV2ValueFreePascalShortString.GetAsString: AnsiString;
var
  len: QWord;
  LenSym, StSym: TFpDwarfValue;
begin
  if FValueDone then
    exit(FValue);

  LenSym := TFpDwarfValue(GetInternMemberByName('length'));
  assert(LenSym is TFpDwarfValue, 'LenSym is TFpDwarfValue');
  len := LenSym.AsCardinal;

  if (TypeInfo.Size < 0) or (len > TypeInfo.Size) then begin
    FLastError := CreateError(fpErrAnyError);
    exit('');
  end;

  StSym := TFpDwarfValue(GetInternMemberByName('st'));
  assert(StSym is TFpDwarfValue, 'StSym is TFpDwarfValue');



  SetLength(Result, len);
  if len > 0 then
    if not MemManager.ReadMemory(StSym.DataAddress, len, @Result[1]) then begin
      Result := ''; // TODO: error
      FLastError := MemManager.LastError;
      exit;
    end;

  FValue := Result;
  FValueDone := True;
end;

function TFpDwarfV2ValueFreePascalShortString.GetAsWideString: WideString;
begin
  Result := GetAsString;
end;

{ TFpDwarfFreePascalSymbolTypeArray }

function TFpDwarfFreePascalSymbolTypeArray.GetTypedValueObject(
  ATypeCast: Boolean): TFpDwarfValue;
begin
  Result := TFpDwarfValueFreePascalArray.Create(Self);
end;

{ TFpDwarfValueFreePascalArray }

function TFpDwarfValueFreePascalArray.GetMemberCount: Integer;
var
  t, t2: TFpDbgSymbol;
  Info: TDwarfInformationEntry;
  n: AnsiString;
  UpperBoundSym: TFpDwarfSymbol;
  val: TFpDbgValue;
  l, h: Int64;
  Addr: TFpDbgMemLocation;
begin
  Result := 0;
  t := TypeInfo;
  if (t.Kind <> skArray) or (t.MemberCount < 1) then // IndexTypeCount;
    exit(inherited GetMemberCount);

  t2 := t.Member[0]; // IndexType[0];
  if not (t2 is TFpDwarfSymbolTypeSubRange) then
    exit(inherited GetMemberCount);


  TFpDwarfSymbolTypeSubRange(t2).GetValueBounds(Self, l, h);
  if (l <> 0) or
     (TFpDwarfSymbolTypeSubRange(t2).LowBoundState <> rfConst) or
     (TFpDwarfSymbolTypeSubRange(t2).HighBoundState <> rfNotFound) or
     (TFpDwarfSymbolTypeSubRange(t2).CountState <> rfNotFound)
  then
    exit(inherited GetMemberCount);

  // Check for open array param
  if (t is TFpDwarfSymbolTypeArray) and
     (DbgSymbol is TFpDwarfSymbolValueParameter) // open array exists only as param
  then begin
    Info := TFpDwarfSymbolValueParameter(DbgSymbol).InformationEntry.Clone;
    Info.GoNext;
    if Info.HasValidScope and
       Info.HasAttrib(DW_AT_location) and  // the high param must have a location / cannot be a constant
       Info.ReadName(n)
    then begin
      if (n <> '') and (n[1] = '$') then // dwarf3 // TODO: make required in dwarf3
        delete(n, 1, 1);
      if (copy(n,1,4) = 'high') and (UpperCase(copy(n, 5, length(n))) = UpperCase(DbgSymbol.Name)) then begin
        UpperBoundSym := TFpDwarfSymbol.CreateSubClass('', Info);
        if UpperBoundSym <> nil then begin
          val := UpperBoundSym.Value;
          TFpDwarfValue(val).Context := Context;
          //l := t2.OrdLowBound;
          h := Val.AsInteger;
          if h > l then begin
          {$PUSH}{$Q-}
            if QWord(h - l) > 5000 then
              h := l + 5000;
          {$POP}
          Result := h - l + 1;
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
  if (sfDynArray in t.Flags) and (AsCardinal <> 0) and
    GetDwarfDataAddress(Addr, TFpDwarfSymbolType(Owner))
  then begin
    if not (IsReadableMem(Addr) and (LocToAddr(Addr) > AddressSize)) then
      exit(0); // dyn array, but bad data
    Addr.Address := Addr.Address - AddressSize;
    //debugln(['TFpDwarfValueArray.GetMemberCount  XXXXXXXXXXXXXXX dwarf 2 read len']);
    if MemManager.ReadSignedInt(Addr, AddressSize, h) then begin
      Result := Integer(h)+1;
      exit;
    end
    else
      FLastError := MemManager.LastError;
    Result := 0;
    exit;
  end;

  // Should not be here. There is no knowledeg how many members there are
  Result := inherited GetMemberCount;
end;

{ TFpDwarfV3FreePascalSymbolTypeArray }

function TFpDwarfV3FreePascalSymbolTypeArray.GetInternalStringType: TArrayOrStringType;
var
  Info: TDwarfInformationEntry;
  t: Cardinal;
  t2: TFpDbgSymbol;
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
      if (t2.Size = 2) then
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

function TFpDwarfV3FreePascalSymbolTypeArray.GetTypedValueObject(
  ATypeCast: Boolean): TFpDwarfValue;
begin
  if GetInternalStringType in [{iasShortString,} iasAnsiString, iasUnicodeString] then
    Result := TFpDwarfV3ValueFreePascalString.Create(Self)
  else
    Result := inherited GetTypedValueObject(ATypeCast);
end;

procedure TFpDwarfV3FreePascalSymbolTypeArray.KindNeeded;
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

{ TFpDwarfV3ValueFreePascalString }

function TFpDwarfV3ValueFreePascalString.IsValidTypeCast: Boolean;
var
  f: TFpDbgValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(TypeCastTargetType.Kind in [skString, skWideString], 'TFpDwarfValueArray.IsValidTypeCast: TypeCastTargetType.Kind = skArray');

  f := TypeCastSourceValue.FieldFlags;
  if (f * [svfAddress, svfSize, svfSizeOfPointer] = [svfAddress]) or
     (svfOrdinal in f)
  then
    exit;

  //if sfDynArray in TypeCastTargetType.Flags then begin
  //  // dyn array
  //  if (svfOrdinal in f)then
  //    exit;
  //  if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
  //     (TypeCastSourceValue.Size = FOwner.CompilationUnit.AddressSize)
  //  then
  //    exit;
  //  if (f * [svfAddress, svfSizeOfPointer] = [svfAddress, svfSizeOfPointer]) then
  //    exit;
  //end
  //else begin
  //  // stat array
  //  if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
  //     (TypeCastSourceValue.Size = TypeCastTargetType.Size)
  //  then
  //    exit;
  //end;
  Result := False;
end;

procedure TFpDwarfV3ValueFreePascalString.Reset;
begin
  inherited Reset;
  FValueDone := False;
end;

function TFpDwarfV3ValueFreePascalString.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfString];
end;

function TFpDwarfV3ValueFreePascalString.GetAsString: AnsiString;
var
  t, t2: TFpDbgSymbol;
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
  if t.MemberCount < 1 then // subrange type
    exit;

  t2 := t.Member[0]; // subrange type
  if not( (t2 is TFpDwarfSymbolType) and TFpDwarfSymbolType(t2).GetValueBounds(self, LowBound, HighBound) )
  then
    exit;

  Addr := DataAddr;
  if (not IsValidLoc(Addr)) and (svfOrdinal in TypeCastSourceValue.FieldFlags) then
    Addr := TargetLoc(TypeCastSourceValue.AsCardinal);
  if not IsReadableLoc(Addr) then
    exit;

  assert((Owner <> nil) and (Owner.CompilationUnit <> nil) and (Owner.CompilationUnit.DwarfSymbolClassMap is TFpDwarfFreePascalSymbolClassMapDwarf3), 'TFpDwarfV3ValueFreePascalString.GetAsString: (Owner <> nil) and (Owner.CompilationUnit <> nil) and (Owner.CompilationUnit.DwarfSymbolClassMap is TFpDwarfFreePascalSymbolClassMapDwarf3)');
  if (TFpDwarfFreePascalSymbolClassMapDwarf3(Owner.CompilationUnit.DwarfSymbolClassMap).FCompilerVersion > 0) and
     (TFpDwarfFreePascalSymbolClassMapDwarf3(Owner.CompilationUnit.DwarfSymbolClassMap).FCompilerVersion < $030100)
  then begin
    if t.Kind = skWideString then begin
      if (t2 is TFpDwarfSymbolTypeSubRange) and (LowBound = 1) then begin
        if (TFpDwarfSymbolTypeSubRange(t2).InformationEntry.GetAttribData(DW_AT_upper_bound, AttrData)) and
           (TFpDwarfSymbolTypeSubRange(t2).InformationEntry.AttribForm[AttrData.Idx] = DW_FORM_block1) and
           (IsReadableMem(Addr) and (LocToAddr(Addr) > AddressSize))
        then begin
          // fpc issue 0035359
          // read data and check for DW_OP_shr ?
          Addr2 := Addr;
          Addr2.Address := Addr2.Address - AddressSize;
          if MemManager.ReadSignedInt(Addr2, AddressSize, i) then begin
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

    if not MemManager.ReadMemory(Addr, (HighBound-LowBound+1)*2, @WResult[1]) then begin
      WResult := '';
      FLastError := MemManager.LastError;
    end;
    Result := WResult;
  end else begin
    SetLength(Result, HighBound-LowBound+1);

    if not MemManager.ReadMemory(Addr, HighBound-LowBound+1, @Result[1]) then begin
      Result := '';
      FLastError := MemManager.LastError;
    end;
  end;

  FValue := Result;

end;

function TFpDwarfV3ValueFreePascalString.GetAsWideString: WideString;
begin
  // todo: widestring, but currently that is encoded as PWideChar
  Result := GetAsString;
end;

initialization
  DwarfSymbolClassMapList.AddMap(TFpDwarfFreePascalSymbolClassMapDwarf2);
  DwarfSymbolClassMapList.AddMap(TFpDwarfFreePascalSymbolClassMapDwarf3);

end.


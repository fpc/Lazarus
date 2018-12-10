unit FpDbgDwarfFreePascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpDbgDwarfDataClasses, FpDbgDwarf, FpDbgInfo, FpDbgUtil,
  FpDbgDwarfConst, FpErrorMessages, DbgIntfBaseTypes, LazLoggerBase;

type

  { TFpDwarfFreePascalSymbolClassMap }

  TFpDwarfFreePascalSymbolClassMap = class(TFpDwarfDefaultSymbolClassMap)
  public
    class function HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
    //class function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol;
      ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfFreePascalSymbolClassMapDwarf2 }

  TFpDwarfFreePascalSymbolClassMapDwarf2 = class(TFpDwarfFreePascalSymbolClassMap)
  public
    class function HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
    class function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    //class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol;
    //  ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfFreePascalSymbolClassMapDwarf3 }

  TFpDwarfFreePascalSymbolClassMapDwarf3 = class(TFpDwarfFreePascalSymbolClassMap)
  public
    class function HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
    //class function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    //class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol;
    //  ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

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

  { TFpDwarfValue2FreePascalShortString }

  TFpDwarfValue2FreePascalShortString = class(TFpDwarfValue)
  private
    FValue: String;
    FValueDone: Boolean;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

implementation

{ TFpDwarfFreePascalSymbolClassMap }

class function TFpDwarfFreePascalSymbolClassMap.HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean;
var
  s: String;
begin
  s := LowerCase(ACU.Producer);
  Result := pos('free pascal', s) > 0;
end;

class function TFpDwarfFreePascalSymbolClassMap.CreateContext(AThreadId, AStackFrame: Integer;
  AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext;
begin
  Result := TFpDwarfFreePascalAddressContext.Create(AThreadId, AStackFrame, AnAddress, ASymbol, ADwarf);
end;

{ TFpDwarfFreePascalSymbolClassMapDwarf2 }

class function TFpDwarfFreePascalSymbolClassMapDwarf2.HandleCompUnit(
  ACU: TDwarfCompilationUnit): Boolean;
begin
  Result := inherited HandleCompUnit(ACU);
  Result := Result and (ACU.Version < 3);
end;

class function TFpDwarfFreePascalSymbolClassMapDwarf2.GetDwarfSymbolClass(
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

class function TFpDwarfFreePascalSymbolClassMapDwarf3.HandleCompUnit(
  ACU: TDwarfCompilationUnit): Boolean;
begin
  Result := inherited HandleCompUnit(ACU);
  Result := Result and (ACU.Version >= 3);
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
    Result := TFpDwarfValue2FreePascalShortString.Create(Self);
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

{ TFpDwarfValue2FreePascalShortString }

function TFpDwarfValue2FreePascalShortString.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfString];
end;

function TFpDwarfValue2FreePascalShortString.GetAsString: AnsiString;
var
  len: QWord;
  LenSym, StSym: TFpDwarfValue;
begin
  if FValueDone then
    exit(FValue);

  if HasTypeCastInfo then begin
    FLastError := CreateError(fpErrAnyError);
    exit('');
  end;

  LenSym := TFpDwarfValue(inherited MemberByName['length']);
  assert(LenSym is TFpDwarfValue, 'LenSym is TFpDwarfValue');
  len := LenSym.AsCardinal;

  if (TypeInfo.Size < 0) or (len > TypeInfo.Size) then begin
    FLastError := CreateError(fpErrAnyError);
    exit('');
  end;

  StSym := TFpDwarfValue(inherited MemberByName['st']);
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

function TFpDwarfValue2FreePascalShortString.GetAsWideString: WideString;
begin
  Result := GetAsString;
end;

initialization
  DwarfSymbolClassMapList.AddMap(TFpDwarfFreePascalSymbolClassMapDwarf2);
  DwarfSymbolClassMapList.AddMap(TFpDwarfFreePascalSymbolClassMapDwarf3);

end.


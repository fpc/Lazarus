unit FpWatchResultData;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}

interface

uses
  FpDbgInfo, FpPascalBuilder, FpdMemoryTools, FpErrorMessages, FpDbgDwarf,
  FpDbgDwarfDataClasses, DbgIntfBaseTypes, LazClasses, fgl, Math, SysUtils,
  LazDebuggerIntf;

type

  TDbgPtrList = specialize TFPGList<TDBGPtr>;

  { TFpWatchResultConvertor }

  TFpWatchResultConvertor = class
  private const
    MAX_RECURSE_LVL = 10;
    MAX_RECURSE_LVL_ARRAY = 5;
    MAX_RECURSE_LVL_PTR = 8; // max depth for a chain of pointers starting at the initial value
  private
    FContext: TFpDbgLocationContext;
    FExtraDepth: Boolean;
    FFirstIndexOffs: Integer;
    FRecurseCnt, FRecurseCntLow,
    FRecursePointerCnt,
    FRecurseInstanceCnt, FRecurseDynArray: integer;
    FRecurseAddrList: TDbgPtrList;
    FLastValueKind: TDbgSymbolKind;
    FHasEmbeddedPointer: Boolean;
    FOuterArrayIdx, FTotalArrayCnt: integer;
    FRepeatCount: Integer;
  protected
    function CheckError(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): boolean;

    procedure AddTypeNameToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf; ADeref: Boolean = False);

    function PointerToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function NumToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function CharToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function StringToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function WideStringToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function BoolToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function EnumToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function SetToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function FloatToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function ArrayToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function StructToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function ProcToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function DoValueToResData(AnFpValue: TFpValue;
                              AnResData: TLzDbgWatchDataIntf
                             ): Boolean; virtual;
    function DoWriteWatchResultData(AnFpValue: TFpValue;
                                  AnResData: TLzDbgWatchDataIntf
                                 ): Boolean;
    function DoWritePointerWatchResultData(AnFpValue: TFpValue;
                                  AnResData: TLzDbgWatchDataIntf;
                                  AnAddr: TDbgPtr
                                 ): Boolean;

    property RecurseCnt: Integer read FRecurseCnt;
  public
    constructor Create(AContext: TFpDbgLocationContext);
    destructor Destroy; override;

    function WriteWatchResultData(AnFpValue: TFpValue;
                                  AnResData: TLzDbgWatchDataIntf;
                                  ARepeatCount: Integer = 0
                                 ): Boolean;

    property Context: TFpDbgLocationContext read FContext write FContext;
    property ExtraDepth: Boolean read FExtraDepth write FExtraDepth;
    property FirstIndexOffs: Integer read FFirstIndexOffs write FFirstIndexOffs;
    //property RepeatCount: Integer read FRepeatCount write SetRepeatCount;
  end;



implementation

{ TFpWatchResultConvertor }

function TFpWatchResultConvertor.CheckError(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): boolean;
begin
  Result := AnFpValue = nil;
  if Result then
    exit;
  Result := IsError(AnFpValue.LastError);
  if Result then begin
    if AnResData <> nil then
      AnResData.CreateError(ErrorHandler.ErrorAsString(AnFpValue.LastError));
    AnFpValue.ResetError;
  end;
end;

procedure TFpWatchResultConvertor.AddTypeNameToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf; ADeref: Boolean);
var
  t: TFpSymbol;
  TpName: String;
begin
  t := AnFpValue.TypeInfo;
  if ADeref and (t <> nil) then
    t := t.TypeInfo;
  if (t <> nil) and
     GetTypeName(TpName, t, [tnfNoSubstitute]) and
     (TpName <> '')
  then
    AnResData.SetTypeName(TpName);
end;

function TFpWatchResultConvertor.PointerToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  DerefRes: TLzDbgWatchDataIntf;
  DerefVal: TFpValue;
  addr: QWord;
begin
  Result := True;
  addr := AnFpValue.AsCardinal;
  AnResData.CreatePointerValue(addr);
  AddTypeNameToResData(AnFpValue, AnResData);

  if CheckError(AnFpValue, AnResData) then
    exit;

  if addr = 0 then
    exit;

  if svfString in AnFpValue.FieldFlags then begin
    // PChar: Get zero-terminated string, rather than just one single char
    DerefRes := AnResData.SetDerefData;
    if DerefRes <> nil then begin
      DerefRes.CreateString(AnFpValue.AsString);
      AddTypeNameToResData(AnFpValue, DerefRes, True);
      CheckError(AnFpValue, DerefRes);
    end;
  end
  else
  if svfWideString in AnFpValue.FieldFlags then begin
    // PWideChar: Get zero-terminated string, rather than just one single char
    DerefRes := AnResData.SetDerefData;
    if DerefRes <> nil then begin
      DerefRes.CreateWideString(AnFpValue.AsWideString);
      AddTypeNameToResData(AnFpValue, DerefRes, True);
      CheckError(AnFpValue, DerefRes);
    end;
  end
  else begin
    DerefVal := AnFpValue.Member[0];
    if IsError(AnFpValue.LastError) then begin
      CheckError(AnFpValue, AnResData.SetDerefData);
    end
    else
    if (DerefVal <> nil) then begin
      DerefRes := nil;
      if (DerefVal.Kind in [skString, skAnsiString, skChar, skWideString,
          skInteger, skCardinal, skBoolean, skFloat, skCurrency, skEnum, skSet])
      then begin
        (* (Nested) Pointer to
           - Pascal-String type
           - Any basic type (any type that has no reference or internal pointer)
             (skChar should not happen: Should be PChar above)
           - Any other
        *)
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then begin
          // In case of nested pointer MAX_RECURSE_LVL may already be reached. Make an exception here, to allow one more.
          dec(FRecurseCnt);
          DoWritePointerWatchResultData(DerefVal, DerefRes, addr);
          inc(FRecurseCnt);
        end;
      end
      else
      if (DerefVal.Kind =skPointer) and (svfString in DerefVal.FieldFlags) then begin
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then begin
          DerefRes.CreateString(DerefVal.AsString);
          AddTypeNameToResData(DerefVal, DerefRes, True);
        end;
      end
      else
      if (DerefVal.Kind =skPointer) and (svfWideString in DerefVal.FieldFlags) then begin
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then begin
          DerefRes.CreateWideString(DerefVal.AsString);
          AddTypeNameToResData(DerefVal, DerefRes, True);
        end;
      end
      else begin
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then
          DoWritePointerWatchResultData(DerefVal, DerefRes, addr);
      end;

      CheckError(DerefVal, DerefRes);
      DerefVal.ReleaseReference;
    end;
  end;
end;

function TFpWatchResultConvertor.NumToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
begin
  Result := True;
  if AnFpValue.Kind = skCardinal then
    AnResData.CreateNumValue(AnFpValue.AsCardinal, False, SizeToFullBytes(AnFpValue.DataSize))
  else
    AnResData.CreateNumValue(QWord(AnFpValue.AsInteger), True, SizeToFullBytes(AnFpValue.DataSize));
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.CharToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
begin
  Result := True;
  AnResData.CreateCharValue(AnFpValue.AsCardinal, SizeToFullBytes(AnFpValue.DataSize));
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.StringToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
begin
  Result := True;
  AnResData.CreateString(AnFpValue.AsString);
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.WideStringToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
begin
  Result := True;
  AnResData.CreateWideString(AnFpValue.AsWideString);
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.BoolToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
begin
  Result := True;
  AnResData.CreateBoolValue(AnFpValue.AsCardinal, SizeToFullBytes(AnFpValue.DataSize));
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.EnumToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  ValSize: TFpDbgValueSize;
begin
  Result := True;
  if not( (svfSize in AnFpValue.FieldFlags) and AnFpValue.GetSize(ValSize) ) then
    ValSize := ZeroSize;
  if IsError(AnFpValue.LastError) then
    ValSize := ZeroSize;
  AnFpValue.ResetError;

  AnResData.CreateEnumValue(AnFpValue.AsCardinal, AnFpValue.AsString, SizeToFullBytes(ValSize), AnFpValue.Kind=skEnumValue);
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.SetToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  m: TFpValue;
  Names: array of String;
  i: Integer;
begin
  Result := True;
  SetLength(Names, AnFpValue.MemberCount);
  for i := 0 to AnFpValue.MemberCount-1 do begin
    m := AnFpValue.Member[i];
    if svfIdentifier in m.FieldFlags then
      Names[i] := m.AsString
    else
    if svfOrdinal in m.FieldFlags then // set of byte
      Names[i] := IntToStr(m.AsCardinal)
    else
      Names[i] := '';
    m.ReleaseReference;
  end;
  AnResData.CreateSetValue(Names);
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.FloatToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  p: TLzDbgFloatPrecission;
  s: TFpDbgValueSize;
begin
  Result := True;

  p := dfpSingle;
  if AnFpValue.GetSize(s) then begin
    if SizeToFullBytes(s) > SizeOf(Double) then
      p := dfpExtended
    else
    if SizeToFullBytes(s) > SizeOf(Single) then
      p := dfpDouble
  end;
  AnResData.CreateFloatValue(AnFpValue.AsFloat, p);
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.ArrayToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
const
  MAX_TOTAL_ARRAY_CNT = 5000;
  MAX_TOTAL_ARRAY_CNT_EXTRA_DEPTH = 3500; // reset
var
  Cnt, i, CurRecurseDynArray, OuterIdx: Integer;
  LowBnd, StartIdx: Int64;
  Addr: TDBGPtr;
  ti: TFpSymbol;
  EntryRes: TLzDbgWatchDataIntf;
  MemberValue: TFpValue;
begin
  Result := True;

  Cnt := AnFpValue.MemberCount;
  CurRecurseDynArray := FRecurseDynArray;
  OuterIdx := FOuterArrayIdx;

  if (AnFpValue.IndexTypeCount = 0) or (not AnFpValue.IndexType[0].GetValueLowBound(AnFpValue, LowBnd)) then
    LowBnd := 0;

  Addr := 0;
  ti := AnFpValue.TypeInfo;
  if (ti = nil) or (ti.Flags * [sfDynArray, sfStatArray] = []) then begin
    EntryRes := AnResData.CreateArrayValue(datUnknown, Cnt, LowBnd);
  end
  else
  if (sfDynArray in ti.Flags) or (LowBnd = 0) then begin // LowBnd = 0 => there is some bug, reporting some dyn arrays as stat.
    EntryRes := AnResData.CreateArrayValue(datDynArray, Cnt, 0);
    if AnFpValue.FieldFlags * [svfInteger, svfCardinal] <> [] then
      Addr := AnFpValue.AsCardinal
    else
    if svfDataAddress in AnFpValue.FieldFlags then
      Addr := AnFpValue.DataAddress.Address;
    AnResData.SetDataAddress(Addr);

    if FRecurseCnt >= 0 then
      inc(FRecurseDynArray);
  end
  else begin
    EntryRes := AnResData.CreateArrayValue(datStatArray, Cnt, LowBnd);
  end;

  AddTypeNameToResData(AnFpValue, AnResData);

  try
    if (Cnt <= 0) or
       (FHasEmbeddedPointer) or
       (FRecurseCnt > MAX_RECURSE_LVL_ARRAY) or
       ( (FRecurseCnt > 0) and (FTotalArrayCnt > MAX_TOTAL_ARRAY_CNT) )
    then
      exit;

    StartIdx := 0;
    If (FOuterArrayIdx < 0) and (FRecurseCnt = FRecurseCntLow) then
      StartIdx := FFirstIndexOffs;
    Cnt := max(1, Cnt - StartIdx);

    if (Context.MemManager.MemLimits.MaxArrayLen > 0) and (Cnt > Context.MemManager.MemLimits.MaxArrayLen) then
      Cnt := Context.MemManager.MemLimits.MaxArrayLen;

    If (FOuterArrayIdx < 0) and (FRecurseCnt = FRecurseCntLow) and (FRepeatCount > 0) then Cnt := FRepeatCount
    else if (FRecurseCnt > 1) and (FOuterArrayIdx >  10) and (Cnt >   10) then Cnt := 10
    else if (FRecurseCnt > 1) and (FOuterArrayIdx >   1) and (Cnt >   20) then Cnt := 20
    else if (FRecurseCnt > 0) and (FOuterArrayIdx > 100) and (Cnt >   10) then Cnt := 10
    else if (FRecurseCnt > 0) and (FOuterArrayIdx >   1) and (Cnt >   50) then Cnt := 50;

    /////////////////////
    // add mem read cache ??
    // Bound types

    inc(FTotalArrayCnt, Cnt);
    for i := StartIdx to StartIdx + Cnt - 1 do begin
      if (FRecurseCnt < 0) and (FTotalArrayCnt > MAX_TOTAL_ARRAY_CNT_EXTRA_DEPTH) then
        FTotalArrayCnt := MAX_TOTAL_ARRAY_CNT_EXTRA_DEPTH;
      if i > FOuterArrayIdx then
        FOuterArrayIdx := i;
      MemberValue := AnFpValue.Member[i+LowBnd];
      EntryRes := AnResData.SetNextArrayData;
      if MemberValue = nil then
        EntryRes.CreateError('Error: Could not get member')
      else
        DoWritePointerWatchResultData(MemberValue, EntryRes, Addr);
      MemberValue.ReleaseReference;
    end;

  finally
    FRecurseDynArray := CurRecurseDynArray;
    FOuterArrayIdx := OuterIdx;
  end
end;

function TFpWatchResultConvertor.StructToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
type
  TAnchestorMap = specialize TFPGMap<PtrUInt, TLzDbgWatchDataIntf>;
var
  vt: TLzDbgStructType;
  Cache: TFpDbgMemCacheBase;
  AnchestorMap: TAnchestorMap;
  i, j, WasRecurseInstanceCnt: Integer;
  MemberValue: TFpValue;
  ti, sym: TFpSymbol;
  ResAnch, ResField, TopAnch, UnkAnch: TLzDbgWatchDataIntf;
  MbName: String;
  MBVis: TLzDbgFieldVisibility;
  Addr: TDBGPtr;
begin
  Result := True;

  case AnFpValue.Kind of
    skRecord:    vt := dstRecord;
    skObject:    vt := dstObject;
    skClass:     vt := dstClass;
    skInterface: vt := dstInterface;
    else         vt := dstUnknown;
  end;

  if not Context.MemManager.CheckDataSize(SizeToFullBytes(AnFpValue.DataSize)) then begin
    AnResData.CreateError(ErrorHandler.ErrorAsString(Context.LastMemError));
    exit;
  end;

  Addr := 0;
  if (AnFpValue.Kind in [skClass, skInterface]) then begin
    if AnFpValue.FieldFlags * [svfInteger, svfCardinal] <> [] then
      Addr := AnFpValue.AsCardinal
    else
    if svfDataAddress in AnFpValue.FieldFlags then
      Addr := AnFpValue.DataAddress.Address;
  end;

  AnResData.CreateStructure(vt, Addr);
  AddTypeNameToResData(AnFpValue, AnResData);

  if (AnFpValue.Kind in [skClass, skInterface]) and
     ( (Addr = 0) or
       (FRecurseInstanceCnt >= 1) or (FRecurseDynArray >= 2)
     )
  then
    exit;
  if FHasEmbeddedPointer then
    exit;

  if Context.MemManager.CacheManager <> nil then
    Cache := Context.MemManager.CacheManager.AddCache(AnFpValue.DataAddress.Address, SizeToFullBytes(AnFpValue.DataSize))
  else
    Cache := nil;

  AnchestorMap := TAnchestorMap.Create;
  WasRecurseInstanceCnt := FRecurseInstanceCnt;
  if (AnFpValue.Kind in [skClass, skInterface]) and (FRecurseCnt >= 0) then
    inc(FRecurseInstanceCnt);
  try
    TopAnch := AnResData;
    UnkAnch := nil;
    ti := AnFpValue.TypeInfo;
    if ti <> nil then
      ti := ti.InternalTypeInfo;

    if ti <> nil then begin
      AnchestorMap.Add(PtrUInt(ti), AnResData);

      if (AnFpValue.Kind in [skObject, skClass, skInterface]) then begin
        ti := ti.TypeInfo;
        ResAnch := AnResData;
        while ti <> nil do begin
          ResAnch := ResAnch.SetAnchestor(ti.Name);
          AnchestorMap.Add(PtrUInt(ti), ResAnch);
          ti := ti.TypeInfo;
        end;
        TopAnch := ResAnch;
      end;
    end;

    for i := 0 to AnFpValue.MemberCount-1 do begin
      MemberValue := AnFpValue.Member[i];
      if (MemberValue = nil) or (MemberValue.Kind in [skProcedure, skFunction]) then begin
        MemberValue.ReleaseReference;
        (* Has Field
           - $vmt => Constructor or Destructor
           - $vmt_aftercontstruction_local => Constructor
        *)
        continue;
      end;

      ResAnch := nil;
      ti := MemberValue.ParentTypeInfo;
      if ti <> nil then
        ti := ti.InternalTypeInfo;
      j := AnchestorMap.IndexOf(PtrUInt(ti));
      if j >= 0 then begin
        ResAnch := AnchestorMap.Data[j];
      end
      else
      if UnkAnch <> nil then begin
        ResAnch := UnkAnch;
      end
      else begin
        UnkAnch := TopAnch.SetAnchestor('');
        ResAnch := UnkAnch;
      end;

      sym := MemberValue.DbgSymbol;
      if sym <> nil then begin
        MbName := sym.Name;
        case sym.MemberVisibility of
          svPrivate:   MBVis := dfvPrivate;
          svProtected: MBVis := dfvProtected;
          svPublic:    MBVis := dfvPublic;
          else         MBVis := dfvUnknown;
        end;
      end
      else begin
        MbName := '';
        MBVis := dfvUnknown;
      end;

      ResField := ResAnch.AddField(MbName, MBVis, []);
      if not DoWritePointerWatchResultData(MemberValue, ResField, Addr) then
        ResField.CreateError('Unknown');

      MemberValue.ReleaseReference;
    end;
  finally
    FRecurseInstanceCnt := WasRecurseInstanceCnt;
    AnchestorMap.Free;
    if Cache <> nil then
      Context.MemManager.CacheManager.RemoveCache(Cache)
  end;
end;

function TFpWatchResultConvertor.ProcToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  addr: TDBGPtr;
  s, LocName, TpName: String;
  t, sym: TFpSymbol;
  proc: TFpSymbolDwarf;
  par: TFpValueDwarf;
begin
  Result := True;
  addr := AnFpValue.DataAddress.Address;

  LocName := '';
  if AnFpValue.Kind in [skFunctionRef, skProcedureRef] then begin
    t := AnFpValue.TypeInfo;
    sym := AnFpValue.DbgSymbol;
    proc := nil;
    if (sym <> nil) and (sym is TFpSymbolDwarfDataProc) then
      proc := TFpSymbolDwarf(sym)
    else
    if t <> nil then
      proc := TFpSymbolDwarf(TDbgDwarfSymbolBase(t).CompilationUnit.Owner.FindProcSymbol(addr));

    if proc <> nil then begin
      LocName := proc.Name;
      if (proc is TFpSymbolDwarfDataProc) then begin
        par := TFpSymbolDwarfDataProc(proc).GetSelfParameter; // Has no Context set, but we only need TypeInfo.Name
        if (par <> nil) and (par.TypeInfo <> nil) then
          LocName := par.TypeInfo.Name + '.' + LocName;
        par.ReleaseReference;
      end;
      ReleaseRefAndNil(proc);
    end;
  end
  else
    t := AnFpValue.DbgSymbol;

  GetTypeAsDeclaration(s, t);

  case AnFpValue.Kind of
    skProcedure:    AnResData.CreateProcedure(addr, False, LocName, s);
    skFunction:     AnResData.CreateProcedure(addr, True, LocName, s);
    skProcedureRef: AnResData.CreateProcedureRef(addr, False, LocName, s);
    skFunctionRef:  AnResData.CreateProcedureRef(addr, True, LocName, s);
  end;
  AddTypeNameToResData(AnFpValue, AnResData);
end;

function TFpWatchResultConvertor.DoValueToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  PrettyPrinter: TFpPascalPrettyPrinter;
  s: String;
begin
  Result := False;
  case AnFpValue.Kind of
    skPointer:  Result := PointerToResData(AnFpValue, AnResData);
    skInteger,
    skCardinal: Result := NumToResData(AnFpValue, AnResData);
    skFloat:    Result := FloatToResData(AnFpValue, AnResData);

    skChar:       Result := CharToResData(AnFpValue, AnResData);
    skString,
    skAnsiString: Result := StringToResData(AnFpValue, AnResData);
    skWideString: Result := WideStringToResData(AnFpValue, AnResData);

    skRecord,
    skObject,
    skClass,
    skInterface: Result := StructToResData(AnFpValue, AnResData);

    skNone: ;
    skType: ;
    skInstance: ;
    skUnit: ;
    skProcedure,
    skFunction,
    skProcedureRef,
    skFunctionRef: Result := ProcToResData(AnFpValue, AnResData);
    skSimple: ;
    skBoolean:   Result := BoolToResData(AnFpValue, AnResData);
    skCurrency: ;
    skVariant: ;
    skEnum,
    skEnumValue: Result := EnumToResData(AnFpValue, AnResData);
    skSet:       Result := SetToResData(AnFpValue, AnResData);
    skArray:     Result := ArrayToResData(AnFpValue, AnResData);
    skRegister: ;
    skAddress: ;
  end;
  if Result then
    CheckError(AnFpValue, AnResData)
  else
  if FRecurseCnt > 0 then begin
    PrettyPrinter := TFpPascalPrettyPrinter.Create(Context.SizeOfAddress);
    PrettyPrinter.Context := Context;
    PrettyPrinter.PrintValue(s, AnFpValue, wdfDefault, 1, [], [ppvSkipClassBody]);
    AnResData.CreatePrePrinted(s);
    PrettyPrinter.Free;
    Result := True;
  end;

end;

function TFpWatchResultConvertor.DoWriteWatchResultData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  DidHaveEmbeddedPointer: Boolean;
begin
  // FRecurseCnt should be handled by the caller
  Result := (FRecurseCnt > MAX_RECURSE_LVL) or (AnFpValue = nil);
  if Result then
    exit;

  Result := False;

  DidHaveEmbeddedPointer := FHasEmbeddedPointer;
  if (FRecurseCnt <= 0) and
     ( (FLastValueKind = skPointer) or (FRecurseCnt=-1) ) and
     (AnFpValue.Kind = skPointer) and
     (FRecursePointerCnt < MAX_RECURSE_LVL_PTR)
  then begin
    inc(FRecursePointerCnt);
  end
  else begin
    inc(FRecurseCnt);
    if (AnFpValue.Kind = skPointer) then
      FHasEmbeddedPointer := True
    else
    if FHasEmbeddedPointer and (FLastValueKind <> skPointer) then
      exit(True); // not an error
      // Allow only one level, after an embedded pointer (pointer nested in other data-type)
  end;
  FLastValueKind := AnFpValue.Kind;
  try
    Result := DoValueToResData(AnFpValue, AnResData);
  finally
    if FRecursePointerCnt > 0 then
      dec(FRecursePointerCnt)
    else
      dec(FRecurseCnt);
    FHasEmbeddedPointer := DidHaveEmbeddedPointer;
  end;
end;

function TFpWatchResultConvertor.DoWritePointerWatchResultData(
  AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf; AnAddr: TDbgPtr
  ): Boolean;
begin
  if FRecurseAddrList.IndexOf(AnAddr) >= 0 then
    exit(True);
  if AnAddr <> 0 then
    FRecurseAddrList.Add(AnAddr);
  Result := DoWriteWatchResultData(AnFpValue, AnResData);
  if AnAddr <> 0 then
    FRecurseAddrList.Remove(AnAddr);
end;

constructor TFpWatchResultConvertor.Create(AContext: TFpDbgLocationContext);
begin
  inherited Create;
  FRecurseAddrList := TDbgPtrList.Create;
  FContext := AContext;
end;

destructor TFpWatchResultConvertor.Destroy;
begin
  inherited Destroy;
  FRecurseAddrList.Free;
end;

function TFpWatchResultConvertor.WriteWatchResultData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf; ARepeatCount: Integer): Boolean;
begin
  Result := False;
  if AnResData = nil then
    exit;
  if AnFpValue = nil then begin
    AnResData.CreateError('No Data');
    exit;
  end;
  if CheckError(AnFpValue, AnResData) then
    exit;

  FRecurseAddrList.Clear;
  FRepeatCount := ARepeatCount;
  FRecurseCnt         := -1;
  if FExtraDepth then
    FRecurseCnt         := -2;
  FRecurseInstanceCnt :=  0;
  FRecurseDynArray    :=  0;
  FRecursePointerCnt := 0;
  FRecurseCntLow := FRecurseCnt+1;
  FOuterArrayIdx := -1;
  FTotalArrayCnt :=  0;

  FLastValueKind := AnFpValue.Kind;
  FHasEmbeddedPointer := False;
  Result := DoWriteWatchResultData(AnFpValue, AnResData);
end;

end.


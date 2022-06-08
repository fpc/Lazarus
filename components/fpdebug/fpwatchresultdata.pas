unit FpWatchResultData;

{$mode objfpc}{$H+}

interface

uses
  FpDbgInfo, FpPascalBuilder, FpdMemoryTools, FpErrorMessages, FpDbgDwarf,
  DbgIntfBaseTypes, fgl, SysUtils, LazDebuggerIntf;

type

  { TFpWatchResultConvertor }

  TFpWatchResultConvertor = class
  private const
    MAX_RECURSE_LVL = 32;
    NEST_PTR_RECURSE_LVL = 6; // must be less-or-equal than MAX_RECURSE_LVL
  private
    FContext: TFpDbgLocationContext;
    FRecurseCnt, FRecurseInstanceCnt: integer;
    FOuterArrayIdx: integer;
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

    function DoWriteWatchResultData(AnFpValue: TFpValue;
                                  AnResData: TLzDbgWatchDataIntf
                                 ): Boolean;
  public
    constructor Create(AContext: TFpDbgLocationContext);

    function WriteWatchResultData(AnFpValue: TFpValue;
                                  AnResData: TLzDbgWatchDataIntf;
                                  ARepeatCount: Integer = 0
                                 ): Boolean;

    property Context: TFpDbgLocationContext read FContext write FContext;
  end;



implementation

{ TFpWatchResultConvertor }

function TFpWatchResultConvertor.CheckError(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): boolean;
begin
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
begin
  Result := True;
  AnResData.CreatePointerValue(AnFpValue.AsCardinal);
  AddTypeNameToResData(AnFpValue, AnResData);

  if CheckError(AnFpValue, AnResData) then
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
           * NO pointer => pointer must check NEST_PTR_RECURSE_LVL
                        => pointer is NOT ALLOWED to ignore MAX_RECURSE_LVL
           * NO struct, array, variant... => May include pointers or internal refs
        *)
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then begin
          // In case of nested pointer MAX_RECURSE_LVL may already be reached. Make an exception here, to allow one more.
          dec(FRecurseCnt);
          DoWriteWatchResultData(DerefVal, DerefRes);
          inc(FRecurseCnt);
        end;
      end
      else
      if (DerefVal.Kind =skPointer) and (svfString in DerefVal.FieldFlags) then begin
        // PPChar / Pointer to PChar: Ignore NEST_PTR_RECURSE_LVL
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then begin
          DerefRes.CreateString(DerefVal.AsString);
          AddTypeNameToResData(DerefVal, DerefRes, True);
        end;
      end
      else
      if (DerefVal.Kind =skPointer) and (svfWideString in DerefVal.FieldFlags) then begin
        // PPWideChar / Pointer to PWideChar: Ignore NEST_PTR_RECURSE_LVL
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then begin
          DerefRes.CreateWideString(DerefVal.AsString);
          AddTypeNameToResData(DerefVal, DerefRes, True);
        end;
      end
      else
      if (DerefVal.Kind =skPointer) and (FRecurseCnt <= NEST_PTR_RECURSE_LVL) then begin
        // Nested Pointer
        DerefRes := AnResData.SetDerefData;
        if DerefRes <> nil then begin
          DoWriteWatchResultData(DerefVal, DerefRes);
        end;
      end;
      // Currently do NOT deref for struct, array, ...

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
var
  Cnt, i, RecurseInst, OuterIdx: Integer;
  LowBnd: Int64;
  Addr: TDBGPtr;
  ti: TFpSymbol;
  EntryRes: TLzDbgWatchDataIntf;
  MemberValue: TFpValue;
begin
  Result := True;
  if FRecurseCnt > NEST_PTR_RECURSE_LVL then
    exit;

  Cnt := AnFpValue.MemberCount;
  RecurseInst := FRecurseInstanceCnt;
  OuterIdx := FOuterArrayIdx;

  if (AnFpValue.IndexTypeCount = 0) or (not AnFpValue.IndexType[0].GetValueLowBound(AnFpValue, LowBnd)) then
    LowBnd := 0;

  ti := AnFpValue.TypeInfo;
  if (ti = nil) or (ti.Flags * [sfDynArray, sfStatArray] = []) then begin
    EntryRes := AnResData.CreateArrayValue(datUnknown, Cnt, LowBnd);
  end
  else
  if (sfDynArray in ti.Flags) or (LowBnd = 0) then begin // LowBnd = 0 => there is some bug, reporting some dyn arrays as stat.
    EntryRes := AnResData.CreateArrayValue(datDynArray, Cnt, 0);
    Addr := 0;
    if AnFpValue.FieldFlags * [svfInteger, svfCardinal] <> [] then
      Addr := AnFpValue.AsCardinal
    else
    if svfDataAddress in AnFpValue.FieldFlags then
      Addr := AnFpValue.DataAddress.Address;
    AnResData.SetDataAddress(Addr);

    inc(FRecurseInstanceCnt);
  end
  else begin
    EntryRes := AnResData.CreateArrayValue(datStatArray, Cnt, LowBnd);
  end;

  AddTypeNameToResData(AnFpValue, AnResData);

  try
    if Cnt <= 0 then
      exit;

    if (Context.MemManager.MemLimits.MaxArrayLen > 0) and (Cnt > Context.MemManager.MemLimits.MaxArrayLen) then
      Cnt := Context.MemManager.MemLimits.MaxArrayLen;

    If (FOuterArrayIdx < 0) and (FRepeatCount > 0) then Cnt := FRepeatCount
    else if (FRecurseCnt > 1) and (FOuterArrayIdx <   5) and (Cnt >    5) then Cnt := 5
    else if (FRecurseCnt > 1)                            and (Cnt >    3) then Cnt := 3
    else if (FRecurseCnt > 0) and (FOuterArrayIdx <  25) and (Cnt >   50) then Cnt := 50
    else if (FRecurseCnt > 0) and (FOuterArrayIdx < 100) and (Cnt >   10) then Cnt := 10
    else if (FRecurseCnt > 0)                            and (Cnt >    5) then Cnt := 5
    else if (Cnt > 1000) then Cnt := 1000;

    /////////////////////
    // add mem read cache ??
    // Bound types

    for i := 0 to Cnt - 1 do begin
      if i > FOuterArrayIdx then
        FOuterArrayIdx := i;
      MemberValue := AnFpValue.Member[i+LowBnd];
      EntryRes := AnResData.SetNextArrayData;
      if MemberValue = nil then
        EntryRes.CreateError('Error: Could not get member')
      else
        DoWriteWatchResultData(MemberValue, EntryRes);
      MemberValue.ReleaseReference;
    end;

  finally
    FRecurseInstanceCnt := RecurseInst;
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
  i, j: Integer;
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

  if (AnFpValue.Kind in [skClass, skInterface]) and (Addr = 0) then
    exit;

  if Context.MemManager.CacheManager <> nil then
    Cache := Context.MemManager.CacheManager.AddCache(AnFpValue.DataAddress.Address, SizeToFullBytes(AnFpValue.DataSize))
  else
    Cache := nil;

  AnchestorMap := TAnchestorMap.Create;
  if (AnFpValue.Kind in [skClass, skInterface]) then
    inc(FRecurseInstanceCnt);
  try
    if (AnFpValue.Kind in [skClass, skObject]) and (FRecurseCnt > NEST_PTR_RECURSE_LVL) then
      exit;
    if (AnFpValue.Kind in [skClass, skInterface]) and (FRecurseInstanceCnt >= 2) then
      exit;


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
      if not DoWriteWatchResultData(MemberValue, ResField) then
        ResField.CreateError('Unknown');

      MemberValue.ReleaseReference;
    end;
  finally
    if (AnFpValue.Kind in [skClass, skInterface]) then
      dec(FRecurseInstanceCnt);
    AnchestorMap.Free;
    if Cache <> nil then
      Context.MemManager.CacheManager.RemoveCache(Cache)
  end;
end;

function TFpWatchResultConvertor.DoWriteWatchResultData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  PrettyPrinter: TFpPascalPrettyPrinter;
  s: String;
begin
  // FRecurseCnt should be handled by the caller
  Result := FRecurseCnt > MAX_RECURSE_LVL;
  if Result then
    exit;

  Result := True;
  if AnResData = nil then
    exit;

  if AnFpValue = nil then begin
    AnResData.CreateError('No Data');
    exit;
  end;

  Result := False;
  inc(FRecurseCnt);
  try
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
      skProcedure: ;
      skFunction: ;
      skProcedureRef: ;
      skFunctionRef: ;
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
  finally
    dec(FRecurseCnt);
  end;
end;

constructor TFpWatchResultConvertor.Create(AContext: TFpDbgLocationContext);
begin
  inherited Create;
  FContext := AContext;
end;

function TFpWatchResultConvertor.WriteWatchResultData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf; ARepeatCount: Integer): Boolean;
begin
  if CheckError(AnFpValue, AnResData) then
    exit;

  FRepeatCount := ARepeatCount;
  FRecurseCnt := -1;
  FRecurseInstanceCnt := 0;
  FOuterArrayIdx := -1;
  Result := DoWriteWatchResultData(AnFpValue, AnResData);
end;

end.


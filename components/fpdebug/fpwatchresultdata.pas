unit FpWatchResultData;

{$mode objfpc}{$H+}

interface

uses
  FpDbgInfo, FpPascalBuilder, FpdMemoryTools, FpErrorMessages, DbgIntfBaseTypes,
  fgl, SysUtils, LazDebuggerIntf;

type

  { TFpWatchResultConvertor }

  TFpWatchResultConvertor = class
  private const
    MAX_RECURSE_LVL = 32;
    NEST_PTR_RECURSE_LVL = 6; // must be less-or-equal than MAX_RECURSE_LVL
  private
    FContext: TFpDbgLocationContext;
    FRecurseCnt: integer;
  protected
    function CheckError(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): boolean;

    procedure AddTypeNameToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf; ADeref: Boolean = False);

    function PointerToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function NumToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function StringToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function WideStringToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function EnumToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
    function SetToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;

    function FloatToResData(AnFpValue: TFpValue; AnResData: TLzDbgWatchDataIntf): Boolean;
  public
    constructor Create(AContext: TFpDbgLocationContext);

    function WriteWatchResultData(AnFpValue: TFpValue;
                                  AnResData: TLzDbgWatchDataIntf
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
     GetTypeName(TpName, t, []) and
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
          WriteWatchResultData(DerefVal, DerefRes);
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
          WriteWatchResultData(DerefVal, DerefRes);
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

constructor TFpWatchResultConvertor.Create(AContext: TFpDbgLocationContext);
begin
  inherited Create;
  FContext := AContext;
end;

function TFpWatchResultConvertor.WriteWatchResultData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
begin
  // FRecurseCnt should be handled by the caller
  Result := FRecurseCnt > MAX_RECURSE_LVL;
  if Result then
    exit;

  Result := False;
  inc(FRecurseCnt);
  try
    case AnFpValue.Kind of
      skPointer:  Result := PointerToResData(AnFpValue, AnResData);
      skInteger,
      skCardinal: Result := NumToResData(AnFpValue, AnResData);
      skFloat:    Result := FloatToResData(AnFpValue, AnResData);

      skChar: ;
      skString,
      skAnsiString: Result := StringToResData(AnFpValue, AnResData);
      skWideString: Result := WideStringToResData(AnFpValue, AnResData);

      skRecord,
      skObject,
      skClass,
      skInterface: ;
      skNone: ;
      skType: ;
      skInstance: ;
      skUnit: ;
      skProcedure: ;
      skFunction: ;
      skProcedureRef: ;
      skFunctionRef: ;
      skSimple: ;
      skBoolean: ;
      skCurrency: ;
      skVariant: ;
      skEnum,
      skEnumValue: Result := EnumToResData(AnFpValue, AnResData);
      skSet:       Result := SetToResData(AnFpValue, AnResData);
      skArray: ;
      skRegister: ;
      skAddress: ;
    end;
    if Result then
      CheckError(AnFpValue, AnResData);
  finally
    dec(FRecurseCnt);
  end;
end;

end.


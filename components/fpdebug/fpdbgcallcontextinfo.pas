unit FpDbgCallContextInfo;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
interface

uses
  DbgIntfBaseTypes, math,
  FpDbgInfo,
  FpdMemoryTools,
  FpDbgDwarfDataClasses,
  FpDbgDwarf,
  FpDbgClasses, FpErrorMessages, FpDbgUtil, LazLoggerBase, LazClasses;

type

  { TFpValueCallParamStringByRef }

  TFpValueCallParamStringByRef = class(TFpValueDwarfPointer)
    function GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
      ATargetType: TFpSymbolDwarfType = nil): Boolean; override;
  end;

  { TFpSymbolCallParamStringByRef }

  TFpSymbolCallParamStringByRef = class(TFpSymbolDwarfTypeBasic) // act as pointer
  protected
    procedure KindNeeded; override;
    procedure Init; override;
  public
    constructor Create(AName: String; AStringVarAddress: TDBGPtr);
    function GetTypedValueObject(ATypeCast: Boolean; AnOuterType: TFpSymbolDwarfType = nil): TFpValueDwarf; override;
  end;

  { TFpSymbolDwarfFunctionResult }

  TFpSymbolDwarfFunctionResult = class(TFpSymbolDwarfDataWithLocation)
  protected
    function GetValueAddress(AValueObj: TFpValueDwarf; out AnAddress: TFpDbgMemLocation): Boolean; override;
    procedure Init; override;
  public
    constructor Create(const AName: String;
                       const AAddress: TFpDbgMemLocation;
                       ATypeSymbol: TFpSymbol); overload;
  end;

  { TFpDbgInfoCallContext }

  TFpDbgInfoCallContext = class(TFpDbgAbstractCallContext)
  private
    FDbgProcess: TDbgProcess;
    FDbgThread: TDbgThread;
    FLastError: TFpError;
    FNextParamRegister: Integer;
    FOrigStackPtr: TDBGPtr;
    FPreparedStack: Array of Byte;
    FNeedStringResInFinalize: Boolean;
    FStringResultMem: TDBGPtr;

    function AllocStack(ASize: Integer): TDbgPtr;
    function InternalCreateParamSymbol(ParameterMemLocation: TFpDbgMemLocation; ASymbolType: TFpSymbol; AName: String): TFpValue;
    function InternalCreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; AName: String): TFpValue; inline;
    function AddRecordParam(var ParamSymbol: TFpValue; AParamSymbolType: TFpSymbol; AValue: TFpValue): Boolean;
    function InternalAddStringResult: Boolean;
  public
    constructor Create(const ABaseContext: TFpDbgLocationContext;
      AMemReader: TFpDbgMemReaderBase;
      AMemConverter: TFpDbgMemConvertor;
      ADbgProcess: TDbgProcess;
      ADbgThread: TDbgThread);
    destructor Destroy; override;
    function WriteStack: Boolean; // called by controller-command

    function CreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; AName: String = ''): TFpValue; virtual;

    function AddParam(AParamSymbolType: TFpSymbol; AValue: TFpValue): Boolean;
    function AddOrdinalParam(AParamSymbolType: TFpSymbol; AValue: QWord): Boolean;
    (* AddStringResult:
       Must be called before any AddParam.
       Except for "Self": In case of a method, AddParm(self) must be set before the StringResult
     *)
    function AddStringResult: Boolean;
    function AddOrdinalViaRefAsParam(AValue: QWord): Boolean;  // For string dec-ref
    function FinalizeParams: Boolean;

    // The caller must take care to call DecRef for the result
    function GetStringResultAsPointer(out AStringAsPtr: TDbgPtr): Boolean;
    function GetStringResult(out AVal: TFpValue; AStringSymbolType: TFpSymbol = nil): Boolean;
    function GetWideStringResult(out AVal: TFpValue; AStringSymbolType: TFpSymbol = nil): Boolean;

    property LastError: TFpError read FLastError;
  end;

implementation

{ TFpValueCallParamStringByRef }

function TFpValueCallParamStringByRef.GetDwarfDataAddress(out
  AnAddress: TFpDbgMemLocation; ATargetType: TFpSymbolDwarfType): Boolean;
begin
  AnAddress := Address;
  Result := IsReadableLoc(AnAddress);
end;

{ TFpSymbolCallParamStringByRef }

procedure TFpSymbolCallParamStringByRef.KindNeeded;
begin
  SetKind(skPointer);
end;

procedure TFpSymbolCallParamStringByRef.Init;
begin
  inherited Init;
  EvaluatedFields := EvaluatedFields + [sfiAddress];
end;

function TFpSymbolCallParamStringByRef.GetTypedValueObject(ATypeCast: Boolean;
  AnOuterType: TFpSymbolDwarfType): TFpValueDwarf;
begin
  Result := TFpValueCallParamStringByRef.Create(AnOuterType);
end;

constructor TFpSymbolCallParamStringByRef.Create(AName: String;
  AStringVarAddress: TDBGPtr);
begin
  inherited Create(AName, skPointer, TargetLoc(AStringVarAddress));
  SetTypeInfo(TFpSymbolDwarfTypePointer.Create(AName));
  TypeInfo.ReleaseReference;
  Init;
end;

{ TFpSymbolDwarfFunctionResult }

function TFpSymbolDwarfFunctionResult.GetValueAddress(AValueObj: TFpValueDwarf; out AnAddress: TFpDbgMemLocation): Boolean;
begin
  AnAddress := Address;
  Result := IsInitializedLoc(AnAddress);
end;

procedure TFpSymbolDwarfFunctionResult.Init;
begin
  inherited Init;
  EvaluatedFields := EvaluatedFields + [sfiAddress];
end;

constructor TFpSymbolDwarfFunctionResult.Create(const AName: String;
  const AAddress: TFpDbgMemLocation; ATypeSymbol: TFpSymbol);
begin
  inherited Create(AName, ATypeSymbol.Kind, AAddress); // TODO: intercept at TDbgDwarfSymbolBase and call Init?
  SetTypeInfo(ATypeSymbol);
  Init;
end;

{ TFpDbgInfoCallContext }

function TFpDbgInfoCallContext.AllocStack(ASize: Integer): TDbgPtr;
begin
  Result := FDbgThread.GetStackPointerRegisterValue;
  if FOrigStackPtr = 0 then
    FOrigStackPtr := Result;
  dec(Result, ASize);
  FDbgThread.SetStackPointerRegisterValue(Result);
end;

function TFpDbgInfoCallContext.InternalCreateParamSymbol(
  ParameterMemLocation: TFpDbgMemLocation; ASymbolType: TFpSymbol; AName: String
  ): TFpValue;
var
  ParamSymbol: TFpSymbol;// TFpSymbolDwarfFunctionResult;
begin
  Result := nil;
  if not IsValidLoc(ParameterMemLocation) then begin
    FLastError := CreateError(fpErrAnyError, ['Too many params']);
    exit;
  end;
  ParamSymbol := TFpSymbolDwarfFunctionResult.Create(AName, ParameterMemLocation, ASymbolType);
  try
    Result := ParamSymbol.Value;
  finally
    ParamSymbol.ReleaseReference;
  end;
  TFpValueDwarf(Result).Context := Self;
end;

function TFpDbgInfoCallContext.InternalCreateParamSymbol(AParamIndex: Integer;
  ASymbolType: TFpSymbol; AName: String): TFpValue;
begin
  Result := InternalCreateParamSymbol(FDbgProcess.CallParamDefaultLocation(AParamIndex),
    ASymbolType, AName);
end;

function TFpDbgInfoCallContext.AddRecordParam(var ParamSymbol: TFpValue;
  AParamSymbolType: TFpSymbol; AValue: TFpValue): Boolean;
// Only intel/amd
  function ReadRecFromMem(Addr: TDbgPtr; sz: Integer; out Data: QWord): Boolean;
  var
    d: QWord;
  begin
    Result := FDbgProcess.ReadData(addr, sz, d);
    if Result then begin
      Data := 0;
      Move(d, Data, sz);
    end
    else begin
      FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
    end;
  end;

  function HasUnalignedFields: Boolean;
  var
    i: Integer;
    FldLoc: TFpDbgMemLocation;
    FldSize: TFpDbgValueSize;
    RecAddr, FldOffs: TDBGPtr;
    m: TFpValue;
  begin
    Result := False;
    RecAddr := AValue.Address.Address;
    m := nil;
    for i := 0 to AValue.MemberCount - 1 do begin
      m.ReleaseReference;
      m := AValue.Member[i];
      FldLoc := m.Address;
      Result := FldLoc.BitOffset <> 0;
      if Result then
        break;
      FldOffs := FldLoc.Address - RecAddr;
      FldSize := m.DataSize;
      Result := FldSize.BitSize <> 0;
      if Result then
        break;
      Result := FldOffs mod FldSize.Size <> 0;
      if Result then
        break;
    end;
    m.ReleaseReference;
  end;

var
  {$If defined(UNIX)}
  i: Integer;
  {$ENDIF}
  RecSize: Integer;
  RecLoc: TFpDbgMemLocation;
  RecAddr, RecData: TDBGPtr;
  l: SizeInt;
begin
  RecSize := SizeToFullBytes(AValue.DataSize);
  Result := not IsError(AValue.LastError);
  if Result then begin
    RecLoc := AValue.Address;
    Result := IsValidLoc(RecLoc);
    RecAddr := RecLoc.Address;
  end;
  if not Result then begin
    FLastError := AValue.LastError;
    exit;
  end;

  {$IF defined(WINDOWS)}
  if FDbgProcess.Mode = dm32 then begin
    if (RecSize <= FDbgProcess.PointerSize) then begin
      Result := ReadRecFromMem(RecAddr, RecSize, RecData);
      if not Result then
        exit;

      l := Length(FPreparedStack);
      SetLength(FPreparedStack, l + 4);
      if l > 0 then
        move(FPreparedStack[0], FPreparedStack[4], SizeOf(FPreparedStack[0]) * l);
      PDWord(@FPreparedStack[0])^ := RecData;

      dec(FNextParamRegister); // no register used
      exit;
    end;

    ParamSymbol.AsCardinal := RecAddr;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  end
  else begin
    if (RecSize <= FDbgProcess.PointerSize) and (RecSize in [2,4,8]) then begin
      Result := ReadRecFromMem(RecAddr, RecSize, RecData);
      RecAddr := RecData;
    end;

    if Result then begin
      ParamSymbol.AsCardinal := RecAddr;
      Result := not IsError(ParamSymbol.LastError);
      FLastError := ParamSymbol.LastError;
    end;
  end;
  {$ElseIf defined(UNIX)}
  if FDbgProcess.Mode = dm64 then begin
    if (RecSize <= 16) and not HasUnalignedFields then begin
      // Use 1 or 2 registers
      While RecSize > 0 do begin
        Result := ReadRecFromMem(RecAddr, Min(8, RecSize), RecData);
        if not Result then
          exit;

        ParamSymbol.AsCardinal := RecData;
        Result := not IsError(ParamSymbol.LastError);
        if not result then begin
          FLastError := ParamSymbol.LastError;
          exit;
        end;
        dec(RecSize, 8);
        inc(RecAddr, 8);
        if RecSize > 0 then begin
          inc(FNextParamRegister);
          ParamSymbol.ReleaseReference;
          ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, AParamSymbolType, '');
        end;
      end;
    end
    else begin
      // on the stack
      dec(FNextParamRegister); // no register used
      i := (RecSize+7) and $FFFFFFF8;
      l := Length(FPreparedStack);
      SetLength(FPreparedStack, l + i);
      Result := FDbgProcess.ReadData(RecAddr, RecSize, FPreparedStack[l]);
      if not Result then begin
        FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
        exit;
      end;
    end;
  end
  else begin
    // 32bit linux
    Result := False;
    FLastError := CreateError(fpErrAnyError, ['not supported']);
  end;
  {$Else}
    Result := False;
    FLastError := CreateError(fpErrAnyError, ['not supported']);
  {$ENDIF}
end;

function TFpDbgInfoCallContext.InternalAddStringResult: Boolean;
var
  ParamSymbol: TFpValue;
  RefSym: TFpSymbolCallParamStringByRef;
begin
  RefSym := TFpSymbolCallParamStringByRef.Create('', 0);
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, RefSym, '');
  try
    Result := ParamSymbol <> nil;
    if not Result then
      exit;
    ParamSymbol.AsCardinal := FStringResultMem;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  finally
    ParamSymbol.ReleaseReference;
    RefSym.ReleaseReference;
  end;
  inc(FNextParamRegister);
end;

constructor TFpDbgInfoCallContext.Create(
  const ABaseContext: TFpDbgLocationContext; AMemReader: TFpDbgMemReaderBase;
  AMemConverter: TFpDbgMemConvertor; ADbgProcess: TDbgProcess;
  ADbgThread: TDbgThread);
begin
  inherited Create(ABaseContext, AMemReader, AMemConverter);
  FDbgProcess := ADbgProcess;
  FDbgThread := ADbgThread;
  FNextParamRegister := 0;
end;

destructor TFpDbgInfoCallContext.Destroy;
begin
  inherited Destroy;

  if FOrigStackPtr <> 0 then
    FDbgThread.SetStackPointerRegisterValue(FOrigStackPtr);
end;

function TFpDbgInfoCallContext.WriteStack: Boolean;
var
  m: TDBGPtr;
begin
  if Length(FPreparedStack) = 0 then
    exit;

  m := AllocStack(Length(FPreparedStack));
  Result := FDbgProcess.WriteData(m, Length(FPreparedStack), FPreparedStack[0]);
  if not Result then
    FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
end;

function TFpDbgInfoCallContext.CreateParamSymbol(AParamIndex: Integer;
  ASymbolType: TFpSymbol; AName: String): TFpValue;
begin
  if AName = '' then
    AName := ASymbolType.Name;
  Result := InternalCreateParamSymbol(AParamIndex, ASymbolType.TypeInfo, AName);
end;

function TFpDbgInfoCallContext.AddParam(AParamSymbolType: TFpSymbol; AValue: TFpValue): Boolean;
var
  ParamSymbol: TFpValue;
begin
  Result := False;
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, AParamSymbolType, '');
  Result := ParamSymbol <> nil;
  if not Result then
    exit;
  try
    if AValue.Kind = skRecord then
      Result := AddRecordParam(ParamSymbol, AParamSymbolType, AValue)
    else
      ParamSymbol.AsCardinal := AValue.AsCardinal;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  finally
    ParamSymbol.ReleaseReference;
  end;
  inc(FNextParamRegister);
end;

function TFpDbgInfoCallContext.AddOrdinalParam(AParamSymbolType: TFpSymbol; AValue: QWord): Boolean;
var
  ParamSymbol: TFpValue;
begin
  Result := False;
  if AParamSymbolType = nil then
    AParamSymbolType := TFpSymbolCallParamStringByRef.Create('', 0)
  else
    AParamSymbolType.AddReference;
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, AParamSymbolType, '');
  AParamSymbolType.ReleaseReference;
  Result := ParamSymbol <> nil;
  if not Result then
    exit;
  try
    ParamSymbol.AsCardinal := AValue;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  finally
    ParamSymbol.ReleaseReference;
  end;
  inc(FNextParamRegister);
end;

function TFpDbgInfoCallContext.AddStringResult: Boolean;
var
  ANil: QWord;
begin
  Result := True;
  ANil := 0;
  FStringResultMem := AllocStack(32); // TODO: only Win64 needs 32 alignemnt
  FDbgProcess.WriteData(FStringResultMem, FDbgProcess.PointerSize, ANil);

  FNeedStringResInFinalize := FDbgProcess.Mode = dm32;
  if not FNeedStringResInFinalize then
    Result := InternalAddStringResult;
end;

function TFpDbgInfoCallContext.AddOrdinalViaRefAsParam(AValue: QWord): Boolean;
var
  ParamSymbol: TFpValue;
  m: TDBGPtr;
  RefSym: TFpSymbolCallParamStringByRef;
begin
  m := AllocStack(32);
  RefSym := TFpSymbolCallParamStringByRef.Create('', m);
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, RefSym, '');
  try
    Result := ParamSymbol <> nil;
    if not Result then
      exit;
    ParamSymbol.AsCardinal := m;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  finally
    ParamSymbol.ReleaseReference;
    RefSym.ReleaseReference;
  end;
  inc(FNextParamRegister);
end;

function TFpDbgInfoCallContext.FinalizeParams: Boolean;
begin
  Result := True;
  if FNeedStringResInFinalize then
    Result := InternalAddStringResult;
end;

function TFpDbgInfoCallContext.GetStringResultAsPointer(out
  AStringAsPtr: TDbgPtr): Boolean;
begin
  Result := FDbgProcess.ReadAddress(FStringResultMem, AStringAsPtr);
  if not Result then
    FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
end;

function TFpDbgInfoCallContext.GetStringResult(out AVal: TFpValue;
  AStringSymbolType: TFpSymbol): Boolean;
var
  Addr, l: TDbgPtr;
  ResSymbol: TFpValue;
  s: String;
  r: Cardinal;
begin
  AVal := nil;
  Result := GetStringResultAsPointer(Addr);
  if not Result then
    exit;

  if AStringSymbolType <> nil then begin
    ResSymbol := InternalCreateParamSymbol(ConstDerefLoc(Addr), AStringSymbolType, 'result');
    AVal := TFpValueConstString.Create(ResSymbol.AsString);
    Result := IsError(ResSymbol.LastError);
    if not Result then
      FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
    ReleaseRefAndNil(ResSymbol);
    exit;
  end;

  s := '';
  if Addr <> 0 then begin
    Result := FDbgProcess.ReadAddress(Addr - FDbgProcess.PointerSize, l);
    if Result then begin
      l := min(l, 1000*1024);
      SetLength(s, l);
      Result := FDbgProcess.ReadData(Addr, l, s[1], r);
      SetLength(s,r);
    end;
    if not Result then begin
      FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
      exit;
    end;
  end;
  AVal := TFpValueConstString.Create(s);
end;

function TFpDbgInfoCallContext.GetWideStringResult(out AVal: TFpValue;
  AStringSymbolType: TFpSymbol): Boolean;
var
  Addr, l: TDbgPtr;
  ResSymbol: TFpValue;
  s: WideString;
  r: Cardinal;
begin
  AVal := nil;
  Result := GetStringResultAsPointer(Addr);
  if not Result then
    exit;

  if AStringSymbolType <> nil then begin
    ResSymbol := InternalCreateParamSymbol(ConstDerefLoc(Addr), AStringSymbolType, 'result');
    AVal := TFpValueConstString.Create(ResSymbol.AsString);
    Result := IsError(ResSymbol.LastError);
    if not Result then
      FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
    ReleaseRefAndNil(ResSymbol);
    exit;
  end;

  s := '';
  if Addr <> 0 then begin
    Result := FDbgProcess.ReadAddress(Addr - FDbgProcess.PointerSize, l);
    if Result then begin
      l := min(l, 1000*1024);
      SetLength(s, l);
      Result := FDbgProcess.ReadData(Addr, l*2, s[1], r);
      SetLength(s,r);
    end;
    if not Result then begin
      FLastError := CreateError(fpErrAnyError, ['failed to read mem']);
      exit;
    end;
  end;
  AVal := TFpValueConstString.Create(s);
end;

end.


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

  { TFpSymbolCallParamOrdinalOrPointer }

  TFpSymbolCallParamOrdinalOrPointer = class(TFpSymbolDwarfTypeBasic) // act as pointer
  private type
    { TFpValueCallParamStringByRef }

    TFpValueCallParamStringByRef = class(TFpValueDwarfPointer)
      function GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
        ATargetType: TFpSymbolDwarfType = nil): Boolean; override;
    end;

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
    property NewAddress: TFpDbgMemLocation write SetAddress;
  end;

  { TFpDbgInfoCallContext }

  TOnCallRoutineHitBreapoint = procedure(AnAddress: TDBGPtr; out ACanContinue: Boolean) of object;

  TFpDbgInfoCallContext = class(TFpDbgAbstractCallContext)
  private
    FDbgProcess: TDbgProcess;
    FDbgThread: TDbgThread;
    FLastError: TFpError;
    FNextParamRegister: Integer;
    FOnCallRoutineHitBreapoint: TOnCallRoutineHitBreapoint;
    FOrigStackPtr: TDBGPtr;
    FPreparedStack: Array of Byte;
    FNeedStringResInFinalize: Boolean;
    FStringResultMem: TDBGPtr;

    function AllocStack(ASize: Integer): TDbgPtr;
    function CreatePreparedStackLocation(ASize: Integer): TFpDbgMemLocation;
    function InternalGetLocation(AParamIndex: Integer; ASize: Integer = 0): TFpDbgMemLocation;
    function InternalCreateParamSymbol(ParameterMemLocation: TFpDbgMemLocation; ASymbolType: TFpSymbol; AName: String): TFpValue;
    function InternalCreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; AName: String): TFpValue; inline;
    function AddRecordParam(AParamSymbolType: TFpSymbol; AValue: TFpValue): Boolean;
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
    function AddOrdinalParam(AValue: QWord): Boolean; inline;
    (* AddOrdinalViaRefAsParam
       TODO: need size of the ordinal -- currently using SizeOfAddr in target
    *)
    function AddOrdinalViaRefAsParam(AValue: QWord): Boolean; inline;  // For string dec-ref
    function AddOrdinalViaRefAsParam(AValue: QWord; out ATargetParamAddr: TDBGPtr): Boolean;
    (* AddStringResult:
       Must be called before any AddParam.
       Except for "Self": In case of a method, AddParm(self) must be set before the StringResult
     *)
    function AddStringResult: Boolean;
    function FinalizeParams: Boolean;

    // The caller must take care to call DecRef for the result
    function GetStringResultAsPointer(out AStringAsPtr: TDbgPtr): Boolean;
    function GetStringResult(out AVal: TFpValue; AStringSymbolType: TFpSymbol = nil): Boolean;
    function GetWideStringResult(out AVal: TFpValue; AStringSymbolType: TFpSymbol = nil): Boolean;

    property LastError: TFpError read FLastError;
    property OnCallRoutineHitBreapoint: TOnCallRoutineHitBreapoint read FOnCallRoutineHitBreapoint write FOnCallRoutineHitBreapoint;
  end;

implementation

var
  FPDBG_FUNCCALL: PLazLoggerLogGroup;

{ TFpSymbolCallParamOrdinalOrPointer.TFpValueCallParamStringByRef }

function TFpSymbolCallParamOrdinalOrPointer.TFpValueCallParamStringByRef.GetDwarfDataAddress
  (out AnAddress: TFpDbgMemLocation; ATargetType: TFpSymbolDwarfType): Boolean;
begin
  AnAddress := Address;
  Result := IsReadableLoc(AnAddress);
end;

{ TFpSymbolCallParamOrdinalOrPointer }

procedure TFpSymbolCallParamOrdinalOrPointer.KindNeeded;
begin
  SetKind(skPointer);
end;

procedure TFpSymbolCallParamOrdinalOrPointer.Init;
begin
  inherited Init;
  EvaluatedFields := EvaluatedFields + [sfiAddress];
end;

function TFpSymbolCallParamOrdinalOrPointer.GetTypedValueObject(ATypeCast: Boolean;
  AnOuterType: TFpSymbolDwarfType): TFpValueDwarf;
begin
  Result := TFpValueCallParamStringByRef.Create(AnOuterType);
end;

constructor TFpSymbolCallParamOrdinalOrPointer.Create(AName: String;
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

  {$IF defined(WINDOWS)}
  if FDbgProcess.Mode = dm64 then begin
    ASize := (ASize + 32) and not(31); // keep aligned to 32 bytes
  end
  else
  {$ENDIF}
  ASize := (ASize + 8) and not(7); // keep aligned to 8 bytes

  dec(Result, ASize);
  FDbgThread.SetStackPointerRegisterValue(Result);
end;

function TFpDbgInfoCallContext.CreatePreparedStackLocation(ASize: Integer
  ): TFpDbgMemLocation;
var
  l: SizeInt;
begin
  l := Length(FPreparedStack);
  SetLength(FPreparedStack, l + ASize);

  if FDbgProcess.Mode = dm32 then begin
    if l > 0 then
      move(FPreparedStack[0], FPreparedStack[ASize], SizeOf(FPreparedStack[0]) * l);
    l := 0;
  end;

  Result := SelfLoc(@FPreparedStack[l]);
end;

function TFpDbgInfoCallContext.InternalGetLocation(AParamIndex: Integer;
  ASize: Integer): TFpDbgMemLocation;
begin
  if (FDbgProcess.Mode = dm32) and (ASize = 8) then begin
    Result := CreatePreparedStackLocation(ASize);
    exit;
  end;

  Result := FDbgProcess.CallParamDefaultLocation(AParamIndex);
  if IsValidLoc(Result) then
    exit;

  if ASize = 0 then
    ASize := SizeOfAddress;
  Result := CreatePreparedStackLocation(ASize);
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
  Result := InternalCreateParamSymbol(InternalGetLocation(AParamIndex), ASymbolType, AName);
end;

function TFpDbgInfoCallContext.AddRecordParam(AParamSymbolType: TFpSymbol;
  AValue: TFpValue): Boolean;
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
      FLastError := CreateError(fpErrAnyError, ['failed to read record data from memory']);
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
  ParamSymbol: TFpValue;

  procedure InitParamSymbol;
  begin
    ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, AParamSymbolType, '');
    inc(FNextParamRegister);
    if Length(FPreparedStack) > 0 then
      MemManager.SetWritableSeflMem(TDBGPtr(@FPreparedStack[0]), Length(FPreparedStack));
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
  ParamSymbol := nil;
  try
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

        exit;
      end;

      InitParamSymbol;
      ParamSymbol.AsCardinal := RecAddr;
      Result := not IsError(ParamSymbol.LastError);
      FLastError := ParamSymbol.LastError;
    end
    else begin
      InitParamSymbol;
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
          ParamSymbol.ReleaseReference;
          InitParamSymbol;
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
        end;
      end
      else begin
        // on the stack
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
        if (RecSize <= 4) then begin
          Result := ReadRecFromMem(RecAddr, 4, RecData);
          if not Result then
            exit;

          l := Length(FPreparedStack);
          SetLength(FPreparedStack, l + 4);
          if l > 0 then
            move(FPreparedStack[0], FPreparedStack[4], SizeOf(FPreparedStack[0]) * l);
          PDWord(@FPreparedStack[0])^ := RecData;
        end
        else begin
          InitParamSymbol;
          ParamSymbol.AsCardinal := RecAddr;
          Result := not IsError(ParamSymbol.LastError);
          FLastError := ParamSymbol.LastError;
        end;
    end;
    {$Else}
      Result := False;
      FLastError := CreateError(fpErrAnyError, ['record as param are not supported']);
    {$ENDIF}
  finally
    ParamSymbol.ReleaseReference;
    MemManager.ClearWritableSeflMem;
  end;
end;

function TFpDbgInfoCallContext.InternalAddStringResult: Boolean;
var
  ParamSymbol: TFpValue;
  RefSym: TFpSymbolCallParamOrdinalOrPointer;
begin
  RefSym := TFpSymbolCallParamOrdinalOrPointer.Create('', 0);
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, RefSym, '');
  try
    Result := ParamSymbol <> nil;
    if not Result then begin
      exit;
    end;
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
  debugln(FPDBG_FUNCCALL, ['CallRoutine END (CTX DESTROY)']);
end;

function TFpDbgInfoCallContext.WriteStack: Boolean;
var
  m: TDBGPtr;
begin
  Result := True;
  if Length(FPreparedStack) = 0 then
    exit;

  {$IF defined(WINDOWS)}
  if FDbgProcess.Mode = dm64 then begin
    m := AllocStack(Length(FPreparedStack)+32) + 32;
  end

  else
  {$ENDIF}
  m := AllocStack(Length(FPreparedStack));

  Result := FDbgProcess.WriteData(m, Length(FPreparedStack), FPreparedStack[0]);
  if not Result then
    FLastError := CreateError(fpErrAnyError, ['failed to write call info to stack memory']);
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
  ParamSymbol: TFpSymbolDwarfFunctionResult;
  ParamValue: TFpValue;
  s: TFpDbgValueSize;
begin
  if AValue.Kind = skRecord then begin
    Result := AddRecordParam(AParamSymbolType, AValue);
    exit;
  end;

  Result := False;

//  ParamValue := InternalCreateParamSymbol(FNextParamRegister, AParamSymbolType, '');
  ParamValue := nil;
  ParamSymbol := TFpSymbolDwarfFunctionResult.Create('', InvalidLoc, AParamSymbolType);
  try
    ParamValue := ParamSymbol.Value;
    TFpValueDwarf(ParamValue).Context := Self;
    if (FDbgProcess.Mode = dm32) and (ParamValue.GetSize(s)) and (SizeToFullBytes(s) = 8) then
      ParamSymbol.NewAddress := InternalGetLocation(FNextParamRegister, SizeToFullBytes(s))
    else
      ParamSymbol.NewAddress := InternalGetLocation(FNextParamRegister);

    Result := ParamValue <> nil;
    if not Result then
      exit;
    if Length(FPreparedStack) > 0 then
      MemManager.SetWritableSeflMem(TDBGPtr(@FPreparedStack[0]), Length(FPreparedStack));
    ParamValue.AsCardinal := AValue.AsCardinal;
    Result := not IsError(ParamValue.LastError);
    FLastError := ParamValue.LastError;
  finally
    ParamSymbol.ReleaseReference;
    ParamValue.ReleaseReference;
    MemManager.ClearWritableSeflMem;
  end;
  inc(FNextParamRegister);
end;

function TFpDbgInfoCallContext.AddOrdinalParam(AParamSymbolType: TFpSymbol; AValue: QWord): Boolean;
var
  ParamSymbol: TFpValue;
begin
  debugln(FPDBG_FUNCCALL, ['TFpDbgInfoCallContext.AddOrdinalParam ',FNextParamRegister]);
  Result := False;
  if AParamSymbolType = nil then
    AParamSymbolType := TFpSymbolCallParamOrdinalOrPointer.Create('', 0)
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

function TFpDbgInfoCallContext.AddOrdinalParam(AValue: QWord): Boolean;
begin
  AddOrdinalParam(nil, AValue);
end;

function TFpDbgInfoCallContext.AddStringResult: Boolean;
var
  ANil: QWord;
begin
  Result := True;
  ANil := 0;
  FStringResultMem := AllocStack(32); // TODO: only Win64 needs 32 alignemnt
  Result := FDbgProcess.WriteData(FStringResultMem, FDbgProcess.PointerSize, ANil);
  if not Result then begin
    FLastError := CreateError(fpErrAnyError, ['Error writing result param to stack memory']);
    exit;
  end;

  FNeedStringResInFinalize := FDbgProcess.Mode = dm32;
  if not FNeedStringResInFinalize then
    Result := InternalAddStringResult;
end;

function TFpDbgInfoCallContext.AddOrdinalViaRefAsParam(AValue: QWord): Boolean;
var
  m: TDBGPtr;
begin
  AddOrdinalViaRefAsParam(AValue, m);
end;

function TFpDbgInfoCallContext.AddOrdinalViaRefAsParam(AValue: QWord; out
  ATargetParamAddr: TDBGPtr): Boolean;
var
  ParamSymbol: TFpValue;
  RefSym: TFpSymbolCallParamOrdinalOrPointer;
begin
  ATargetParamAddr := AllocStack(32);
  // TODO: need size of the ordinal
  Result := FDbgProcess.WriteData(ATargetParamAddr, FDbgProcess.PointerSize, AValue);
  if not Result then begin
    FLastError := CreateError(fpErrAnyError, ['Error writing param param to stack memory']);
    exit;
  end;

  RefSym := TFpSymbolCallParamOrdinalOrPointer.Create('', ATargetParamAddr);
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, RefSym, '');
  try
    Result := ParamSymbol <> nil;
    if not Result then
      exit;
    ParamSymbol.AsCardinal := ATargetParamAddr;
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
    FLastError := CreateError(fpErrAnyError, ['failed to read result from stack mem']);
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
      FLastError := ResSymbol.LastError;
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
      FLastError := CreateError(fpErrAnyError, ['failed to read result from mem']);
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
      FLastError := ResSymbol.LastError;
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
      FLastError := CreateError(fpErrAnyError, ['failed to read result from mem']);
      exit;
    end;
  end;
  AVal := TFpValueConstString.Create(s);
end;

initialization
  FPDBG_FUNCCALL := DebugLogger.FindOrRegisterLogGroup('FPDBG_FUNCCALL' {$IFDEF FPDBG_FUNCCALL} , True {$ENDIF} );
end.


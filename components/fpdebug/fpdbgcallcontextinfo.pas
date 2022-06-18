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
    FNeedStringResInFinalize: Boolean;
    FStringResultMem: TDBGPtr;

    function AllocStack(ASize: Integer): TDbgPtr;
    function InternalCreateParamSymbol(ParameterMemLocation: TFpDbgMemLocation; ASymbolType: TFpSymbol; AName: String): TFpValue;
    function InternalCreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; AName: String): TFpValue; inline;
    function InternalAddStringResult: Boolean;
  public
    constructor Create(const ABaseContext: TFpDbgLocationContext;
      AMemReader: TFpDbgMemReaderBase;
      AMemConverter: TFpDbgMemConvertor;
      ADbgProcess: TDbgProcess;
      ADbgThread: TDbgThread);
    destructor Destroy; override;

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

function TFpDbgInfoCallContext.InternalAddStringResult: Boolean;
var
  ParamSymbol: TFpValue;
begin
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister,
    TFpSymbolCallParamStringByRef.Create('', 0),
    ''
  );
  try
    Result := ParamSymbol <> nil;
    if not Result then
      exit;
    ParamSymbol.AsCardinal := FStringResultMem;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  finally
    ParamSymbol.ReleaseReference;
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
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister, AParamSymbolType, '');
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
begin
  m := AllocStack(32);
  ParamSymbol := InternalCreateParamSymbol(FNextParamRegister,
    TFpSymbolCallParamStringByRef.Create('', m),
    ''
  );
  Result := ParamSymbol <> nil;
  if not Result then
    exit;
  try
    ParamSymbol.AsCardinal := m;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  finally
    ParamSymbol.ReleaseReference;
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


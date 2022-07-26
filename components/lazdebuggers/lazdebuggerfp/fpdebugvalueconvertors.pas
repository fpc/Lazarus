unit FpDebugValueConvertors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FpDbgInfo, FpdMemoryTools, FpDbgCallContextInfo,
  FpPascalBuilder, FpErrorMessages, FpDbgClasses, FpDbgUtil, DbgIntfBaseTypes,
  lazCollections, LazClasses, LCLProc, FpDebugDebuggerBase,
  LazDebuggerIntfBaseTypes;

type
  TDbgSymbolKinds = set of TDbgSymbolKind;

  (* TFpDbgValueConverter and descendants
     - A TFpDbgValueConverter should be immutable, once in the list.
       To change settings a new instance can be set to TFpDbgConverterConfig
       This allows for TFpDbgValueConverter to be used outside the lock (reduces lock time)
     - Any setting that the IDE may need to store, should be published
  *)

  TFpDbgValueConverter = class(TRefCountedObject)
  private
    FLastErrror: TFpError;
  public
    class function GetName: String; virtual; abstract;
    class function GetSupportedKinds: TDbgSymbolKinds; virtual;
    procedure Assign(ASource: TFpDbgValueConverter);
    function CreateCopy: TFpDbgValueConverter; virtual;
    function ConvertValue(ASourceValue: TFpValue;
                          AnFpDebugger: TFpDebugDebuggerBase;
                          AnExpressionScope: TFpDbgSymbolScope
                         ): TFpValue; virtual; abstract;
    procedure SetError(AnError: TFpError);
    property LastErrror: TFpError read FLastErrror;
  end;
  TFpDbgValueConverterClass = class of TFpDbgValueConverter;

  { TFpDbgValueConverterClassList }

  TFpDbgValueConverterClassList = class(specialize TFPGList<TFpDbgValueConverterClass>)
    function FindByClassName(AName: String): TFpDbgValueConverterClass;
  end;


  { TFpDbgConverterConfig }

  TFpDbgConverterConfig = class(TFreeNotifyingObject)
  private
    FConverter: TFpDbgValueConverter;
    FMatchKinds: TDbgSymbolKinds;
    FMatchTypeNames: TStrings;
    procedure SetConverter(AValue: TFpDbgValueConverter);
  public
    constructor Create(AConverter: TFpDbgValueConverter);
    destructor Destroy; override;
    function CreateCopy: TFpDbgConverterConfig; virtual;
    procedure Assign(ASource: TFpDbgConverterConfig); virtual;

    function CheckMatch(AValue: TFpValue): Boolean;
    property Converter: TFpDbgValueConverter read FConverter write SetConverter;

    property MatchKinds: TDbgSymbolKinds read FMatchKinds write FMatchKinds;
    property MatchTypeNames: TStrings read FMatchTypeNames;
  end;
  TFpDbgConverterConfigClass = class of TFpDbgConverterConfig;

  { TFpDbgConverterConfigList }

  TFpDbgConverterConfigList = class(specialize TFPGObjectList<TFpDbgConverterConfig>)
  private
    FLock: TLazMonitor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TFpDbgConverterConfigList);
    procedure Lock;
    procedure Unlock;
  end;

  { TFpDbgValueConverterVariantToLStr }

  TFpDbgValueConverterVariantToLStr = class(TFpDbgValueConverter)
  private
    function GetProcAddrFromMgr(AnFpDebugger: TFpDebugDebuggerBase; AnExpressionScope: TFpDbgSymbolScope): TDbgPtr;
  public
    class function GetName: String; override;
    class function GetSupportedKinds: TDbgSymbolKinds; override;
    function ConvertValue(ASourceValue: TFpValue;
                          AnFpDebugger: TFpDebugDebuggerBase;
                          AnExpressionScope: TFpDbgSymbolScope
                         ): TFpValue; override;
  end;

  { TFpDbgValueConverterJsonForDebug }

  TFpDbgValueConverterJsonForDebug = class(TFpDbgValueConverter)
  private
    function GetProcAddr(AnFpDebugger: TFpDebugDebuggerBase; AnExpressionScope: TFpDbgSymbolScope): TDBGPtr;
  public
    class function GetName: String; override;
    class function GetSupportedKinds: TDbgSymbolKinds; override;
    function ConvertValue(ASourceValue: TFpValue;
                          AnFpDebugger: TFpDebugDebuggerBase;
                          AnExpressionScope: TFpDbgSymbolScope
                         ): TFpValue; override;
  end;


function ValueConverterClassList: TFpDbgValueConverterClassList;
function ValueConverterConfigList: TFpDbgConverterConfigList;

implementation
var
  TheValueConverterClassList: TFpDbgValueConverterClassList = nil;
  TheValueConverterList: TFpDbgConverterConfigList = nil;


function ValueConverterClassList: TFpDbgValueConverterClassList;
begin
  if TheValueConverterClassList = nil then
    TheValueConverterClassList := TFpDbgValueConverterClassList.Create;
  Result := TheValueConverterClassList;
end;

function ValueConverterConfigList: TFpDbgConverterConfigList;
begin
  if TheValueConverterList = nil then
    TheValueConverterList := TFpDbgConverterConfigList.Create;
  Result := TheValueConverterList;
end;

{ TFpDbgValueConverterClassList }

function TFpDbgValueConverterClassList.FindByClassName(AName: String
  ): TFpDbgValueConverterClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    if Items[i].ClassName = AName then
      exit(Items[i]);
end;

{ TFpDbgValueConverter }

function TFpDbgValueConverter.CreateCopy: TFpDbgValueConverter;
begin
  Result := TFpDbgValueConverterClass(ClassType).Create;
  Result.Assign(Self);
end;

procedure TFpDbgValueConverter.SetError(AnError: TFpError);
begin
  FLastErrror := AnError;
end;

class function TFpDbgValueConverter.GetSupportedKinds: TDbgSymbolKinds;
begin
  Result := [low(TDbgSymbolKinds)..high(TDbgSymbolKinds)];
end;

procedure TFpDbgValueConverter.Assign(ASource: TFpDbgValueConverter);
begin
  //
end;

{ TFpDbgConverterConfig }

procedure TFpDbgConverterConfig.SetConverter(AValue: TFpDbgValueConverter);
begin
  if FConverter = AValue then Exit;
  FConverter.ReleaseReference;
  FConverter := AValue;
  if FConverter <> nil then
    FConverter.AddReference;
end;

function TFpDbgConverterConfig.CreateCopy: TFpDbgConverterConfig;
begin
  Result := TFpDbgConverterConfigClass(ClassType).Create(nil);
  Result.Assign(Self);
end;

constructor TFpDbgConverterConfig.Create(AConverter: TFpDbgValueConverter);
begin
  inherited Create;
  Converter := AConverter;
  FMatchTypeNames := TStringList.Create;
  TStringList(FMatchTypeNames).CaseSensitive := False;
  TStringList(FMatchTypeNames).Sorted := True;
end;

destructor TFpDbgConverterConfig.Destroy;
begin
  inherited Destroy;
  FMatchTypeNames.Free;
  FConverter.ReleaseReference;
end;

procedure TFpDbgConverterConfig.Assign(ASource: TFpDbgConverterConfig);
begin
  FMatchKinds := ASource.FMatchKinds;
  FMatchTypeNames.Assign(ASource.FMatchTypeNames);
  Converter := ASource.FConverter.CreateCopy;
end;

function TFpDbgConverterConfig.CheckMatch(AValue: TFpValue): Boolean;
var
  t: TFpSymbol;
  TpName: String;
begin
  t := AValue.TypeInfo;
  Result := (AValue.Kind in (FMatchKinds * Converter.GetSupportedKinds)) and
            (t <> nil) and
            GetTypeName(TpName, t, [tnfNoSubstitute]) and
            (FMatchTypeNames.IndexOf(TpName) >= 0);
end;

{ TFpDbgConverterConfigList }

constructor TFpDbgConverterConfigList.Create;
begin
  inherited Create(True);
  FLock := TLazMonitor.create;
end;

destructor TFpDbgConverterConfigList.Destroy;
begin
  inherited Destroy;
  FLock.Free;
end;

procedure TFpDbgConverterConfigList.Assign(ASource: TFpDbgConverterConfigList);
var
  i: Integer;
begin
  Clear;
  Count := ASource.Count;
  for i := 0 to Count - 1 do
    Items[i] := ASource[i].CreateCopy;
end;

procedure TFpDbgConverterConfigList.Lock;
begin
  FLock.Acquire;
end;

procedure TFpDbgConverterConfigList.Unlock;
begin
  FLock.Leave;
end;

{ TFpDbgValueConverterVariantToLStr }

function TFpDbgValueConverterVariantToLStr.GetProcAddrFromMgr(
  AnFpDebugger: TFpDebugDebuggerBase; AnExpressionScope: TFpDbgSymbolScope
  ): TDbgPtr;
var
  ProcSym: TFpSymbol;
  MgrAddr, Fnd: TDBGPtr;
  CallContext: TFpDbgInfoCallContext;
  CurProc: TDbgProcess;
begin
  Result := 0;
  CurProc := AnFpDebugger.DbgController.CurrentProcess;
  if CurProc = nil then
    exit;

  ProcSym := nil;
  ProcSym := CurProc.FindProcSymbol('SYSTEM_$$_GETVARIANTMANAGER$TVARIANTMANAGER');
  try
    if (ProcSym = nil) or (not (ProcSym.Kind = skProcedure)) or
       (not IsTargetAddr(ProcSym.Address))
    then
      exit;

    CallContext := nil;
    MgrAddr := AnFpDebugger.DbgController.CurrentThread.AllocStackMem(1024); // enough space for the record
    CallContext := AnFpDebugger.DbgController.Call(ProcSym.Address, AnExpressionScope.LocationContext,
      AnFpDebugger.MemReader, AnFpDebugger.MemConverter);
    try
      CallContext.AddOrdinalParam(nil, MgrAddr);
      CallContext.FinalizeParams;
      AnFpDebugger.DbgController.ProcessLoop;

      if not CallContext.IsValid then
        exit;

      if CurProc.ReadAddress(MgrAddr + 8 * CurProc.PointerSize, Fnd) then
        Result := Fnd;

    finally
      AnFpDebugger.DbgController.AbortCurrentCommand;
      CallContext.ReleaseReference;
      AnFpDebugger.DbgController.CurrentThread.RestoreStackMem;
    end;
  finally
    ProcSym.ReleaseReference;
  end;
end;

class function TFpDbgValueConverterVariantToLStr.GetName: String;
begin
  Result := 'Call SysVarToLStr';
end;

class function TFpDbgValueConverterVariantToLStr.GetSupportedKinds: TDbgSymbolKinds;
begin
  Result := [skRecord];
end;

function TFpDbgValueConverterVariantToLStr.ConvertValue(ASourceValue: TFpValue;
  AnFpDebugger: TFpDebugDebuggerBase; AnExpressionScope: TFpDbgSymbolScope
  ): TFpValue;
var
  NewResult, ProcVal, m: TFpValue;
  ProcSym: TFpSymbol;
  CallContext: TFpDbgInfoCallContext;
  StringAddr, ProcAddr, StringDecRefAddress: TDbgPtr;
  ProcLoc: TFpDbgMemLocation;
  r: Boolean;
begin
  Result := nil;
  if (ASourceValue.Kind <> skRecord) or
     (AnFpDebugger.DbgController.CurrentProcess = nil) or
     ( (AnFpDebugger.DbgController.CurrentProcess.Mode = dm32) and
       (ASourceValue.DataSize.Size <> 16)
     ) or
     ( (AnFpDebugger.DbgController.CurrentProcess.Mode = dm64) and
       (ASourceValue.DataSize.Size <> 24)
     )
  then begin
    SetError(CreateError(fpErrAnyError, ['Value not a variant']));
    exit;
  end;
  m := ASourceValue.MemberByName['vtype'];
  r := (m = nil) or (SizeToFullBytes(m.DataSize) <> 2);
  m.ReleaseReference;
  if r then begin
    SetError(CreateError(fpErrAnyError, ['Value not a variant']));
    exit;
  end;

  ProcVal := nil;
  ProcSym := nil;
  try
(*
    //VARIANTS_$$_SYSVARTOLSTR$ANSISTRING$VARIANT
    //U_$SYSTEM_$$_VARIANTMANAGER
    //SYSTEM_$$_GETVARIANTMANAGER$TVARIANTMANAGER

    ProcVal := AnExpressionScope.FindSymbol('sysvartolstr', 'variants');
    if ProcVal <> nil then begin
      ProcSym := ProcVal.DbgSymbol;
      if ProcSym <> nil then
        ProcSym.AddReference;
    end;
    if (ProcSym = nil) or (not (ProcSym.Kind = skProcedure)) or
       (not IsTargetAddr(ProcSym.Address))
    then
      ProcSym := AnFpDebugger.DbgController.CurrentProcess.FindProcSymbol('sysvartolstr');

    if (ProcSym = nil) or (not IsTargetAddr(ProcSym.Address)) then
      exit;

    ProcLoc := ProcSym.Address
*)

    if not IsTargetAddr(ASourceValue.Address) then begin
      SetError(CreateError(fpErrAnyError, ['Value not in memory']));
      exit;
    end;

    ProcAddr := AnFpDebugger.GetCachedData(pointer(TFpDbgValueConverterVariantToLStr));
    if ProcAddr = 0 then begin
      ProcAddr := GetProcAddrFromMgr(AnFpDebugger, AnExpressionScope);
      if ProcAddr = 0 then begin
        SetError(CreateError(fpErrAnyError, ['SysVarToLStr not found']));
        exit;
      end;
      AnFpDebugger.SetCachedData(pointer(TFpDbgValueConverterVariantToLStr), ProcAddr);
    end;
    ProcLoc := TargetLoc(ProcAddr);

    StringDecRefAddress := AnFpDebugger.GetCached_FPC_ANSISTR_DECR_REF;
    if (StringDecRefAddress = 0) then begin
      SetError(CreateError(fpErrAnyError, ['STRING_DEC_REF not found']));
      exit;
    end;

    StringAddr := 0;
    CallContext := AnFpDebugger.DbgController.Call(ProcLoc, AnExpressionScope.LocationContext,
      AnFpDebugger.MemReader, AnFpDebugger.MemConverter);
    try
      CallContext.AddStringResult;
      CallContext.FinalizeParams; // force the string as first param (32bit) // TODO
      CallContext.AddOrdinalParam(nil, ASourceValue.DataAddress.Address);
      AnFpDebugger.DbgController.ProcessLoop;

      if not CallContext.IsValid then begin
        if (IsError(CallContext.LastError)) then
          SetError(CallContext.LastError)
        else
        if (CallContext.Message <> '') then
          SetError(CreateError(fpErrAnyError, [CallContext.Message]));
        exit;
      end;

      if not CallContext.GetStringResultAsPointer(StringAddr) then begin
        SetError(CallContext.LastError);
        exit;
      end;

      if not CallContext.GetStringResult(NewResult) then begin
        SetError(CallContext.LastError);
        exit;
      end;

      Result := NewResult;
    finally
      AnFpDebugger.DbgController.AbortCurrentCommand;
      CallContext.ReleaseReference;

      AnFpDebugger.CallTargetFuncStringDecRef(StringDecRefAddress, StringAddr, AnExpressionScope.LocationContext);
    end;
  finally
    ProcVal.ReleaseReference;
    ProcSym.ReleaseReference;
  end;
end;

{ TFpDbgValueConverterJsonForDebug }

function TFpDbgValueConverterJsonForDebug.GetProcAddr(
  AnFpDebugger: TFpDebugDebuggerBase; AnExpressionScope: TFpDbgSymbolScope
  ): TDBGPtr;
var
  CurProc: TDbgProcess;
  ProcSymVal: TFpValue;
  ProcSym: TFpSymbol;
begin
  Result := AnFpDebugger.GetCachedData(pointer(TFpDbgValueConverterJsonForDebug));
  if Result <> 0 then
    exit;

  CurProc := AnFpDebugger.DbgController.CurrentProcess;
  if CurProc = nil then
    exit;

  ProcSymVal := AnExpressionScope.FindSymbol('JsonForDebug');
  if ProcSymVal <> nil then begin
    if (ProcSymVal.Kind = skProcedure) and IsTargetAddr(ProcSymVal.DataAddress) then begin
      Result := ProcSymVal.DataAddress.Address;
      AnFpDebugger.SetCachedData(pointer(TFpDbgValueConverterJsonForDebug), Result);
      ProcSymVal.ReleaseReference;
      exit;
    end;
    Result := 0;
    Result := ProcSymVal.DataAddress.Address;
  end;

  ProcSym := CurProc.FindProcSymbol('JsonForDebug');
  if (ProcSym <> nil) and (ProcSym.Kind = skProcedure) and
     (IsTargetAddr(ProcSym.Address))
  then begin
    Result := ProcSym.Address.Address;
    AnFpDebugger.SetCachedData(pointer(TFpDbgValueConverterJsonForDebug), Result);
  end;
  ProcSym.ReleaseReference;
end;

class function TFpDbgValueConverterJsonForDebug.GetName: String;
begin
  Result := 'Call JsonForDebug';
end;

class function TFpDbgValueConverterJsonForDebug.GetSupportedKinds: TDbgSymbolKinds;
begin
  Result := [low(Result)..high(Result)];
end;

function TFpDbgValueConverterJsonForDebug.ConvertValue(ASourceValue: TFpValue;
  AnFpDebugger: TFpDebugDebuggerBase; AnExpressionScope: TFpDbgSymbolScope
  ): TFpValue;
var
  CurProccess: TDbgProcess;
  TpName, JsonText: String;
  ProcAddr, SetLenProc, DecRefProc,
  TpNameAddr, TpNewNameAddr, TpNameRefAddr, TextAddr, TextRefAddr: TDbgPtr;
  CallContext: TFpDbgInfoCallContext;
  r: Boolean;
  AddrSize: Integer;
begin
  Result := nil;

  if (not (svfAddress in ASourceValue.FieldFlags)) or
     (not IsTargetAddr(ASourceValue.Address))
  then begin
    SetError(CreateError(fpErrAnyError, ['Value not in memory']));
    exit;
  end;

  TpName := '';
  if ASourceValue.TypeInfo <> nil then
    TpName := ASourceValue.TypeInfo.Name;

  if TpName = '' then begin
    SetError(CreateError(fpErrAnyError, ['no typename']));
    exit;
  end;

  ProcAddr := GetProcAddr(AnFpDebugger, AnExpressionScope);
  if ProcAddr = 0 then begin
    SetError(CreateError(fpErrAnyError, ['JsonForDebug not found']));
    exit;
  end;

  CurProccess := AnFpDebugger.DbgController.CurrentProcess;
  SetLenProc := AnFpDebugger.GetCached_FPC_ANSISTR_SETLENGTH;
  DecRefProc := AnFpDebugger.GetCached_FPC_ANSISTR_DECR_REF;
  if (SetLenProc = 0) or (DecRefProc = 0) or (CurProccess = nil)
  then begin
    SetError(CreateError(fpErrAnyError, ['internal error']));
    exit;
  end;


  TpNameAddr := 0;
  TpNewNameAddr := 0;
  TpNameRefAddr := 0;
  TextAddr := 0;
  TextRefAddr := 0;
  try
    if (not AnFpDebugger.CreateAnsiStringInTarget(SetLenProc, TpNameAddr, TpName, AnExpressionScope.LocationContext)) or
       (TpNameAddr = 0)
    then begin
      TpNameAddr := 0;
      SetError(CreateError(fpErrAnyError, ['failed to set param']));
      exit;
    end;


    CallContext := AnFpDebugger.DbgController.Call(TargetLoc(ProcAddr), AnExpressionScope.LocationContext,
      AnFpDebugger.MemReader, AnFpDebugger.MemConverter);

    if (not CallContext.AddOrdinalParam(ASourceValue.Address.Address)) or
       (not CallContext.AddOrdinalViaRefAsParam(TpNameAddr, TpNameRefAddr)) or
       (not CallContext.AddOrdinalViaRefAsParam(0, TextRefAddr))
    then begin
      SetError(CreateError(fpErrAnyError, ['failed to set param']));
      exit;
    end;

    CallContext.FinalizeParams; // force the string as first param (32bit) // TODO

    AnFpDebugger.DbgController.ProcessLoop;

    if not CallContext.IsValid then begin
      if (IsError(CallContext.LastError)) then
        SetError(CallContext.LastError)
      else
      if (CallContext.Message <> '') then
        SetError(CreateError(fpErrAnyError, [CallContext.Message]));
      exit;
    end;

    r := True;
    if not CurProccess.ReadAddress(TpNameRefAddr, TpNewNameAddr) then begin
      r := False;
      TpNewNameAddr := 0;
    end;
    if not CurProccess.ReadAddress(TextRefAddr, TextAddr) then begin
      r := False;
      TextAddr:= 0;
    end;

    if not AnFpDebugger.ReadAnsiStringFromTarget(TpNewNameAddr, TpName) then
      r := False;
    if not AnFpDebugger.ReadAnsiStringFromTarget(TextAddr, JsonText) then
      r := False;

    if not r then begin
      SetError(CreateError(fpErrAnyError, ['failed to get result']));
      exit;
    end;


    AnFpDebugger.DbgController.AbortCurrentCommand;
    CallContext.ReleaseReference;

    Result := TFpValueConstString.Create(JsonText);
    TFpValueConstString(Result).SetTypeName(TpName);

  finally
    if TpNewNameAddr <> 0 then
      TpNameAddr := TpNewNameAddr;
    if TpNameAddr <> 0 then
      AnFpDebugger.CallTargetFuncStringDecRef(DecRefProc, TpNameAddr, AnExpressionScope.LocationContext);
    if TextAddr <> 0 then
      AnFpDebugger.CallTargetFuncStringDecRef(DecRefProc, TextAddr, AnExpressionScope.LocationContext);
  end;
end;

initialization
  ValueConverterClassList.Add(TFpDbgValueConverterVariantToLStr);
  ValueConverterClassList.Add(TFpDbgValueConverterJsonForDebug);

finalization;
  FreeAndNil(TheValueConverterClassList);
  FreeAndNil(TheValueConverterList);

end.


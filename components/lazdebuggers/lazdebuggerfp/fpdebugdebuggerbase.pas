unit FpDebugDebuggerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Math, FPDbgController, FpdMemoryTools, FpDbgClasses,
  FpDbgUtil, FpDbgInfo, FpDbgCallContextInfo, DbgIntfDebuggerBase,
  DbgIntfBaseTypes, LazLoggerBase, FpDebugDebuggerUtils,
  LazDebuggerIntfBaseTypes;

type

  { TFpDebugDebuggerBase }

  TFpDebugDebuggerBase = class(TDebuggerIntf)
  private type
    TCachedDbgPtrMap = specialize TFPGMap<Pointer, TDbgPtr>;
  protected
    FDbgController: TDbgController;
    FMemManager: TFpDbgMemManager;
    FMemReader: TDbgMemReader;
    FMemConverter: TFpDbgMemConvertorLittleEndian;
    FLockList: TFpDbgLockList;
    FWorkQueue: TFpThreadPriorityWorkerQueue;

    FCached_FPC_ANSISTR_DECR_REF: TDbgPtr;
    FCached_FPC_WIDESTR_DECR_REF: TDbgPtr;
    FCached_FPC_ANSISTR_SETLENGTH: TDbgPtr;
    FCached_FPC_WIDESTR_SETLENGTH: TDbgPtr;
    FCached_Data: TCachedDbgPtrMap;

    function GetCached_FPC_Func_Addr(var ACacheVar: TDbgPtr; const AName: String): TDbgPtr;
  public
    destructor Destroy; override;

    // All caches must only be accessed it the debug-thread
    function GetCached_FPC_ANSISTR_DECR_REF: TDBGPtr; inline;
    function GetCached_FPC_WIDESTR_DECR_REF: TDBGPtr; inline;
    function GetCached_FPC_ANSISTR_SETLENGTH: TDBGPtr; inline;
    function GetCached_FPC_WIDESTR_SETLENGTH: TDBGPtr; inline;
    procedure SetCachedData(AKey: Pointer; AValue: TDBGPtr);
    function GetCachedData(AKey: Pointer): TDBGPtr;
    procedure ClearCachedData;

    procedure CallTargetFuncStringDecRef(const AProcAddr: TDbgPtr;
      const AStringDataAddr: TDBGPtr;
      const ABaseContext: TFpDbgLocationContext; AMemReader: TFpDbgMemReaderBase = nil; AMemConverter: TFpDbgMemConvertor = nil);
    function CallTargetFuncStringSetLength(const ASetLengthProcAddr: TDbgPtr;
      out AStringDataAddr: TDBGPtr;
      const AStringLen: cardinal;
      const ABaseContext: TFpDbgLocationContext; AMemReader: TFpDbgMemReaderBase = nil; AMemConverter: TFpDbgMemConvertor = nil
    ): Boolean;
    function CreateAnsiStringInTarget(const ASetLengthProcAddr: TDbgPtr;
      out AStringDataAddr: TDBGPtr;
      const AStringContent: String;
      const ABaseContext: TFpDbgLocationContext; AMemReader: TFpDbgMemReaderBase = nil; AMemConverter: TFpDbgMemConvertor = nil
    ): Boolean;
    function CreateWideStringInTarget(const ASetLengthProcAddr: TDbgPtr;
      out AStringDataAddr: TDBGPtr;
      const AStringContent: WideString;
      const ABaseContext: TFpDbgLocationContext; AMemReader: TFpDbgMemReaderBase = nil; AMemConverter: TFpDbgMemConvertor = nil
    ): Boolean;

    function ReadAnsiStringFromTarget(AStringAddr: TDBGPtr; out AString: String): boolean;

    property DbgController: TDbgController read FDbgController;
    property MemManager:    TFpDbgMemManager read FMemManager;
    property MemReader:     TDbgMemReader read FMemReader;
    property MemConverter:  TFpDbgMemConvertorLittleEndian read FMemConverter;
    property LockList:      TFpDbgLockList read FLockList;
    property WorkQueue:     TFpThreadPriorityWorkerQueue read FWorkQueue;
  end;


implementation

{ TFpDebugDebuggerBase }

function TFpDebugDebuggerBase.GetCached_FPC_Func_Addr(var ACacheVar: TDbgPtr;
  const AName: String): TDbgPtr;
var
  FunctSymbol: TFpSymbol;
begin
  Result := ACacheVar;
  if Result <> 0 then
    exit;

  FunctSymbol := DbgController.CurrentProcess.FindProcSymbol(AName);

  if (FunctSymbol <> nil) and (IsTargetNotNil(FunctSymbol.Address)) then
    ACacheVar := FunctSymbol.Address.Address;

  FunctSymbol.ReleaseReference;
  Result := ACacheVar;
end;

destructor TFpDebugDebuggerBase.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FCached_Data);
end;

function TFpDebugDebuggerBase.GetCached_FPC_ANSISTR_DECR_REF: TDBGPtr;
begin
  Result := GetCached_FPC_Func_Addr(FCached_FPC_ANSISTR_DECR_REF, 'FPC_ANSISTR_DECR_REF');
end;

function TFpDebugDebuggerBase.GetCached_FPC_WIDESTR_DECR_REF: TDBGPtr;
begin
  Result := GetCached_FPC_Func_Addr(FCached_FPC_WIDESTR_DECR_REF, 'FPC_WIDESTR_DECR_REF');
end;

function TFpDebugDebuggerBase.GetCached_FPC_ANSISTR_SETLENGTH: TDBGPtr;
begin
  Result := GetCached_FPC_Func_Addr(FCached_FPC_ANSISTR_SETLENGTH, 'FPC_ANSISTR_SETLENGTH');
end;

function TFpDebugDebuggerBase.GetCached_FPC_WIDESTR_SETLENGTH: TDBGPtr;
begin
  Result := GetCached_FPC_Func_Addr(FCached_FPC_WIDESTR_SETLENGTH, 'FPC_WIDESTR_SETLENGTH');
end;

procedure TFpDebugDebuggerBase.SetCachedData(AKey: Pointer; AValue: TDBGPtr);
begin
  if FCached_Data = nil then
    FCached_Data := TCachedDbgPtrMap.Create;
  FCached_Data[AKey] := AValue;
end;

function TFpDebugDebuggerBase.GetCachedData(AKey: Pointer): TDBGPtr;
var
  i: Integer;
begin
  Result := 0;
  if FCached_Data <> nil then begin
    i := FCached_Data.IndexOf(AKey);
    if i >= 0 then
      Result := FCached_Data.Data[i];
  end;
end;

procedure TFpDebugDebuggerBase.ClearCachedData;
begin
  FCached_FPC_ANSISTR_DECR_REF := 0;
  FCached_FPC_WIDESTR_DECR_REF := 0;
  FCached_FPC_ANSISTR_SETLENGTH := 0;
  if FCached_Data <> nil then
    FCached_Data.Clear;
end;

procedure TFpDebugDebuggerBase.CallTargetFuncStringDecRef(
  const AProcAddr: TDbgPtr; const AStringDataAddr: TDBGPtr;
  const ABaseContext: TFpDbgLocationContext; AMemReader: TFpDbgMemReaderBase;
  AMemConverter: TFpDbgMemConvertor);
var
  CallContext: TFpDbgInfoCallContext;
begin
  if AStringDataAddr = 0 then
    exit;
  if AMemReader = nil then
    AMemReader := MemReader;
  if AMemConverter = nil then
    AMemConverter := MemConverter;

  CallContext := DbgController.Call(TargetLoc(AProcAddr), ABaseContext, AMemReader, AMemConverter);
  try
    CallContext.AddOrdinalViaRefAsParam(AStringDataAddr);
    CallContext.FinalizeParams;
    DbgController.ProcessLoop;
  finally
    DbgController.AbortCurrentCommand;
    CallContext.ReleaseReference;
  end;
end;

function TFpDebugDebuggerBase.CallTargetFuncStringSetLength(
  const ASetLengthProcAddr: TDbgPtr; out AStringDataAddr: TDBGPtr;
  const AStringLen: cardinal; const ABaseContext: TFpDbgLocationContext;
  AMemReader: TFpDbgMemReaderBase; AMemConverter: TFpDbgMemConvertor): Boolean;
var
  CallContext: TFpDbgInfoCallContext;
  AParamAddr: TDBGPtr;
begin
  if AMemReader = nil then
    AMemReader := MemReader;
  if AMemConverter = nil then
    AMemConverter := MemConverter;

  Result := False;
  CallContext := DbgController.Call(TargetLoc(ASetLengthProcAddr), ABaseContext, AMemReader, AMemConverter);
  try
    if not CallContext.AddOrdinalViaRefAsParam(0, AParamAddr) then
      exit;
    if not CallContext.AddOrdinalParam(AStringLen) then
      exit;
    // Some versions may take a TCodePage // Lazarus defaults to utf-8 // CP_UTF8 = 65001;
    // This value is ignored if not needed
    if not CallContext.AddOrdinalParam(65001) then
      exit;
    CallContext.FinalizeParams;
    DbgController.ProcessLoop;

    if not CallContext.IsValid then
      exit;

    Result := DbgController.CurrentProcess.ReadAddress(AParamAddr, AStringDataAddr);
  finally
    DbgController.AbortCurrentCommand;
    CallContext.ReleaseReference;
  end;
end;

function TFpDebugDebuggerBase.CreateAnsiStringInTarget(
  const ASetLengthProcAddr: TDbgPtr; out AStringDataAddr: TDBGPtr;
  const AStringContent: String; const ABaseContext: TFpDbgLocationContext;
  AMemReader: TFpDbgMemReaderBase; AMemConverter: TFpDbgMemConvertor): Boolean;
begin
  if AStringContent = '' then begin
    AStringDataAddr := 0;
    Result := True;
    exit;
  end;

  Result := CallTargetFuncStringSetLength(ASetLengthProcAddr, AStringDataAddr, length(AStringContent),
    ABaseContext, AMemReader, AMemConverter);
  if not Result then
    exit;

  Result := AStringDataAddr <> 0;
  if not Result then
    exit;

  DbgController.CurrentProcess.WriteData(AStringDataAddr, Length(AStringContent), AStringContent[1]);
end;

function TFpDebugDebuggerBase.CreateWideStringInTarget(
  const ASetLengthProcAddr: TDbgPtr; out AStringDataAddr: TDBGPtr;
  const AStringContent: WideString; const ABaseContext: TFpDbgLocationContext;
  AMemReader: TFpDbgMemReaderBase; AMemConverter: TFpDbgMemConvertor): Boolean;
begin
  if AStringContent = '' then begin
    AStringDataAddr := 0;
    Result := True;
    exit;
  end;

  Result := CallTargetFuncStringSetLength(ASetLengthProcAddr, AStringDataAddr, length(AStringContent),
    ABaseContext, AMemReader, AMemConverter);
  if not Result then
    exit;

  Result := AStringDataAddr <> 0;
  if not Result then
    exit;

  DbgController.CurrentProcess.WriteData(AStringDataAddr, Length(AStringContent) * 2, AStringContent[1]);
end;

function TFpDebugDebuggerBase.ReadAnsiStringFromTarget(AStringAddr: TDBGPtr;
  out AString: String): boolean;
var
  CurProcess: TDbgProcess;
  l: TDBGPtr;
  r: Cardinal;
begin
  AString := '';
  Result := AStringAddr = 0;
  if Result then
    exit;

  CurProcess := DbgController.CurrentProcess;
  Result := CurProcess <> nil;
  if not Result then
    exit;

  {$PUSH}{$Q-}{$R-}
  Result := CurProcess.ReadAddress(AStringAddr - CurProcess.PointerSize, l);
  if not Result then
    exit;

  l := Min(l, MemManager.MemLimits.MaxStringLen);
  if (not MemManager.CheckDataSize(l)) then
    exit;

  SetLength(AString, l);
  if l > 0 then begin
    Result := CurProcess.ReadData(AStringAddr, l, AString[1], r);
    SetLength(AString,r);
  end;
  {$POP}
end;

end.


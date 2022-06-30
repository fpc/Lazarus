unit FpDebugDebuggerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FPDbgController, FpdMemoryTools, FpDbgClasses,
  FpDbgUtil, FpDbgInfo, DbgIntfDebuggerBase, LazLoggerBase,
  FpDebugDebuggerUtils, LazDebuggerIntfBaseTypes;

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
    FCached_Data: TCachedDbgPtrMap;
  public
    destructor Destroy; override;

    // All caches must only be accessed it the debug-thread
    function GetCached_FPC_ANSISTR_DECR_REF: TDBGPtr;
    function GetCached_FPC_WIDESTR_DECR_REF: TDBGPtr;
    procedure SetCachedData(AKey: Pointer; AValue: TDBGPtr);
    function GetCachedData(AKey: Pointer): TDBGPtr;
    procedure ClearCachedData;


    property DbgController: TDbgController read FDbgController;
    property MemManager:    TFpDbgMemManager read FMemManager;
    property MemReader:     TDbgMemReader read FMemReader;
    property MemConverter:  TFpDbgMemConvertorLittleEndian read FMemConverter;
    property LockList:      TFpDbgLockList read FLockList;
    property WorkQueue:     TFpThreadPriorityWorkerQueue read FWorkQueue;
  end;


implementation

{ TFpDebugDebuggerBase }

destructor TFpDebugDebuggerBase.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FCached_Data);
end;

function TFpDebugDebuggerBase.GetCached_FPC_ANSISTR_DECR_REF: TDBGPtr;
var
  StringDecRefSymbol: TFpSymbol;
begin
  Result := FCached_FPC_ANSISTR_DECR_REF;
  if Result <> 0 then
    exit;

  StringDecRefSymbol := DbgController.CurrentProcess.FindProcSymbol('FPC_ANSISTR_DECR_REF');

  if (StringDecRefSymbol <> nil) and (IsTargetNotNil(StringDecRefSymbol.Address)) then
    FCached_FPC_ANSISTR_DECR_REF := StringDecRefSymbol.Address.Address;

  StringDecRefSymbol.ReleaseReference;
  Result := FCached_FPC_ANSISTR_DECR_REF;
end;

function TFpDebugDebuggerBase.GetCached_FPC_WIDESTR_DECR_REF: TDBGPtr;
var
  StringDecRefSymbol: TFpSymbol;
begin
  Result := FCached_FPC_WIDESTR_DECR_REF;
  if Result <> 0 then
    exit;

  StringDecRefSymbol := DbgController.CurrentProcess.FindProcSymbol('FPC_WIDESTR_DECR_REF');

  if (StringDecRefSymbol <> nil) and (IsTargetNotNil(StringDecRefSymbol.Address)) then
    FCached_FPC_WIDESTR_DECR_REF := StringDecRefSymbol.Address.Address;

  StringDecRefSymbol.ReleaseReference;
  Result := FCached_FPC_WIDESTR_DECR_REF;
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
  if FCached_Data <> nil then
    FCached_Data.Clear;
end;

end.


unit FpDebugValueConvertors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FpDbgInfo, FpdMemoryTools, FpDbgCallContextInfo,
  FpPascalBuilder, FpErrorMessages, FpDbgClasses, FpDbgUtil, DbgIntfBaseTypes,
  lazCollections, LazClasses, LCLProc, StrUtils, FpDebugDebuggerBase, FpDebugStringConstants,
  LazDebuggerValueConverter, LazDebuggerIntfBaseTypes;

type
  TDbgSymbolKinds = set of TDbgSymbolKind;

  (* TFpDbgValueConverter and descendants
     - A TFpDbgValueConverter should be immutable, once in the list.
       To change settings a new instance can be set to TFpDbgConverterConfig
       This allows for TFpDbgValueConverter to be used outside the lock (reduces lock time)
     - Any setting that the IDE may need to store, should be published
  *)

  TFpDbgValueConverter = class(TRefCountedObject, TLazDbgValueConverterIntf)
  private
    FLastErrror: TFpError;
  protected
    function GetObject: TObject;
    function GetSettingsFrame: TLazDbgValueConverterSettingsFrameIntf; virtual;
    procedure Init; virtual;
  public
    class function GetName: String; virtual; abstract;
    class function GetSupportedKinds: TDbgSymbolKinds; virtual;
    constructor Create; virtual;
    procedure Assign(ASource: TFpDbgValueConverter); virtual;
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

  TFpDbgConverterConfig = class(TFreeNotifyingObject, TLazDbgValueConvertSelectorIntf)
  private
    FConverter: TFpDbgValueConverter;
    FMatchKinds: TDbgSymbolKinds;
    FMatchTypeNames: TStrings;
    procedure SetConverter(AValue: TFpDbgValueConverter);
  protected
    function GetBackendSpecificObject: TObject; deprecated;
    function GetConverter: TLazDbgValueConverterIntf;
  public
    constructor Create(AConverter: TFpDbgValueConverter);
    destructor Destroy; override;
    function CreateCopy: TFpDbgConverterConfig; virtual;
    procedure Assign(ASource: TFpDbgConverterConfig); virtual;

    function CheckMatch(AValue: TFpValue): Boolean;
    function CheckTypeMatch(AValue: TFpValue): Boolean;
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

function TFpDbgValueConverter.GetObject: TObject;
begin
  Result := Self;
end;

function TFpDbgValueConverter.GetSettingsFrame: TLazDbgValueConverterSettingsFrameIntf;
begin
  Result := nil;
end;

procedure TFpDbgValueConverter.Init;
begin
  //
end;

class function TFpDbgValueConverter.GetSupportedKinds: TDbgSymbolKinds;
begin
  Result := [low(TDbgSymbolKinds)..high(TDbgSymbolKinds)];
end;

constructor TFpDbgValueConverter.Create;
begin
  inherited Create;
  Init;
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

function TFpDbgConverterConfig.GetBackendSpecificObject: TObject;
begin
  Result := Self;
end;

function TFpDbgConverterConfig.GetConverter: TLazDbgValueConverterIntf;
begin
  Result := FConverter;
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
begin
  Result := (AValue.Kind in (FMatchKinds * Converter.GetSupportedKinds)) and
            CheckTypeMatch(AValue);
end;

function TFpDbgConverterConfig.CheckTypeMatch(AValue: TFpValue): Boolean;
  function MatchPattern(const AName, APattern: String): Boolean;
  var
    NamePos, PatternPos, p: Integer;
  begin
    Result := False;
    if APattern = '' then
      exit;

    NamePos := 1;
    PatternPos := 1;

    while PatternPos <= Length(APattern) do begin
      if APattern[PatternPos] = '*' then begin
        inc(PatternPos);
      end
      else begin
        p := PatternPos;
        PatternPos := PosEx('*', APattern, p);
        if PatternPos < 1 then
          PatternPos := Length(APattern)+1;
        if PatternPos-p > Length(AName)+1 - NamePos then
          break;

        NamePos := PosEx(Copy(APattern, p, PatternPos-p), AName, NamePos);
        if (NamePos < 1) or
           ( (p = 1) and (NamePos <> 1) ) // APattern does not start with *
        then
          break;

        inc(NamePos, PatternPos-p);
      end;
    end;

    Result := (PatternPos = Length(APattern)+1) and
              ( (NamePos = Length(AName)+1) or
                ( (APattern[Length(APattern)] = '*') and
                  (NamePos <= Length(AName)+1)
                )
              );
  end;
var
  i, CnIdx: Integer;
  TpName, Pattern, ValClassName, ValUnitName: String;
  t: TFpSymbol;
  HasMaybeUnitDot: Boolean;
begin
  t := AValue.TypeInfo;
  Result := (t <> nil) and GetTypeName(TpName, t, [tnfNoSubstitute]);
  if not Result then
    exit;

  TpName := LowerCase(TpName);
  i := FMatchTypeNames.Count;
  while i > 0 do begin
    dec(i);
    Pattern := LowerCase(trim(FMatchTypeNames[i]));

    HasMaybeUnitDot := (pos('.', Pattern) > 1) and
                       (AValue.Kind in [skClass]); // only class supports unitnames (via rtti)

    if AnsiStrLIComp('is:', @Pattern[1], 3) = 0 then begin
      Delete(Pattern, 1, 3);
      Pattern := trim(Pattern);

      if  (AValue.Kind in [skRecord, skClass, skObject, skInterface]) then begin
        ValClassName := TpName;
        while t <> nil do begin
          Result := MatchPattern(ValClassName, Pattern);
          if Result then
            exit;
          t := t.TypeInfo;
          if (t = nil) or not GetTypeName(ValClassName, t, [tnfNoSubstitute]) then
            break;
          ValClassName := LowerCase(ValClassName);
        end;

        CnIdx := 0;
        while AValue.GetInstanceClassName(@ValClassName, @ValUnitName, CnIdx) and
              (ValClassName <> '')
        do begin
          ValClassName := LowerCase(ValClassName);
          if (ValClassName = TpName) and (not HasMaybeUnitDot) then
            Break;
          Result := MatchPattern(ValClassName, Pattern);
          if Result then
            exit;

          if HasMaybeUnitDot and (ValUnitName <> '') then begin
            ValUnitName := LowerCase(ValUnitName);
            Result := MatchPattern(ValUnitName+'.'+ValClassName, Pattern);
            if Result then
              exit;
          end;

          inc(CnIdx);
        end;
        AValue.ResetError;

        Continue;
      end;
    end;

    Result := MatchPattern(TpName, Pattern);
    if Result then
      exit;
    if HasMaybeUnitDot then begin
      if AValue.GetInstanceClassName(@ValClassName, @ValUnitName) and
         (ValUnitName <> '') and (ValClassName <> '')
      then begin
        Result := MatchPattern(ValUnitName+'.'+ValClassName, Pattern);
        if Result then
          exit;
      end;
      AValue.ResetError;
    end;
  end;
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
  Result := drsCallSysVarToLStr;
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

initialization
  ValueConverterClassList.Add(TFpDbgValueConverterVariantToLStr);

finalization;
  FreeAndNil(TheValueConverterClassList);
  FreeAndNil(TheValueConverterList);

end.


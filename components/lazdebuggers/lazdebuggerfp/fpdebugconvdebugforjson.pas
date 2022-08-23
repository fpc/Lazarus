unit FpDebugConvDebugForJson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  FpDebugStringConstants, FpDebugValueConvertors, FpDebugDebuggerBase,
  LazDebuggerValueConverter, LazDebuggerIntfBaseTypes, FpDbgInfo, FpDbgClasses,
  FpdMemoryTools, FpDbgCallContextInfo, FpErrorMessages, DbgIntfBaseTypes;

type

  { TJsonForDebugSettingsFrame }

  TJsonForDebugSettingsFrame = class(TFrame, TLazDbgValueConverterSettingsFrameIntf)
    edFuncName: TEdit;
    edJsonAddress: TEdit;
    edJsonTypename: TEdit;
    lblFuncName: TLabel;
    lblJsonAddress: TLabel;
    lblJsonTypename: TLabel;
  private

  protected
    procedure ReadFrom(AConvertor: TLazDbgValueConverterIntf);
    function WriteTo(AConvertor: TLazDbgValueConverterIntf): Boolean;
    function GetFrame: TObject;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TFpDbgValueConverterJsonForDebug }

  TFpDbgValueConverterJsonForDebug = class(TFpDbgValueConverter)
  private
    FFunctionName: String;
    FJsonAddressKey: String;
    FJsonTypenameKey: String;
    function FunctionNameIsStored: Boolean;
    function GetProcAddr(AnFpDebugger: TFpDebugDebuggerBase; AnExpressionScope: TFpDbgSymbolScope): TDBGPtr;
    function JsonAddressKeyIsStored: Boolean;
  protected
    function GetSettingsFrame: TLazDbgValueConverterSettingsFrameIntf; override;
    procedure Init; override;
  public
    class function GetName: String; override;
    function GetRegistryEntry: TLazDbgValueConvertRegistryEntryClass; override;
    procedure Assign(ASource: TFpDbgValueConverter); override;
    function ConvertValue(ASourceValue: TFpValue;
                          AnFpDebugger: TFpDebugDebuggerBase;
                          AnExpressionScope: TFpDbgSymbolScope
                         ): TFpValue; override;

  published
    property FunctionName: String read FFunctionName write FFunctionName stored FunctionNameIsStored;
    property JsonAddressKey: String read FJsonAddressKey write FJsonAddressKey stored JsonAddressKeyIsStored;
    property JsonTypenameKey: String read FJsonTypenameKey write FJsonTypenameKey;
  end;

  { TFpDbgValueConverterJsonForDebugRegistryEntry }

  TFpDbgValueConverterJsonForDebugRegistryEntry = class(TFpDbgValueConverterRegistryEntry)
  public
    class function GetConvertorClass: TClass; override;
  end;

implementation

{$R *.lfm}

{ TJsonForDebugSettingsFrame }

procedure TJsonForDebugSettingsFrame.ReadFrom(
  AConvertor: TLazDbgValueConverterIntf);
var
  c: TFpDbgValueConverterJsonForDebug;
begin
  if not (AConvertor.GetObject is TFpDbgValueConverterJsonForDebug) then
    exit;

  c := TFpDbgValueConverterJsonForDebug(AConvertor.GetObject);

  edFuncName.Text := c.FFunctionName;
  edJsonAddress.Text := c.FJsonAddressKey;
  edJsonTypename.Text := c.FJsonTypenameKey;
end;

function TJsonForDebugSettingsFrame.WriteTo(
  AConvertor: TLazDbgValueConverterIntf): Boolean;
var
  c: TFpDbgValueConverterJsonForDebug;
begin
  Result := False;
  if not (AConvertor.GetObject is TFpDbgValueConverterJsonForDebug) then
    exit;

  c := TFpDbgValueConverterJsonForDebug(AConvertor.GetObject);

  Result :=
    (c.FFunctionName    <> trim(edFuncName.Text)) or
    (c.FJsonAddressKey  <> trim(edJsonAddress.Text)) or
    (c.FJsonTypenameKey <> trim(edJsonTypename.Text));

  c.FFunctionName    := trim(edFuncName.Text);
  c.FJsonAddressKey  := trim(edJsonAddress.Text);
  c.FJsonTypenameKey := trim(edJsonTypename.Text);
end;

function TJsonForDebugSettingsFrame.GetFrame: TObject;
begin
  Result := Self;
end;

constructor TJsonForDebugSettingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  lblFuncName.Caption := drsFunctionName;
  lblJsonAddress.Caption := drsKeyForAddress;
  lblJsonTypename.Caption := drsKeyForTypename;
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
  Result := AnFpDebugger.GetCachedData(pointer(Self));
  if Result <> 0 then
    exit;

  CurProc := AnFpDebugger.DbgController.CurrentProcess;
  if CurProc = nil then
    exit;

  ProcSymVal := AnExpressionScope.FindSymbol(FFunctionName);
  if ProcSymVal <> nil then begin
    if (ProcSymVal.Kind = skProcedure) and IsTargetAddr(ProcSymVal.DataAddress)
    //and
    //   (ProcSymVal.NestedSymbolCount = 3)
    then begin
      Result := ProcSymVal.DataAddress.Address;
      AnFpDebugger.SetCachedData(pointer(TFpDbgValueConverterJsonForDebug), Result);
      ProcSymVal.ReleaseReference;
      exit;
    end;
    Result := 0;
    Result := ProcSymVal.DataAddress.Address;
  end;

  ProcSym := CurProc.FindProcSymbol(FFunctionName);
  if (ProcSym <> nil) and (ProcSym.Kind = skProcedure) and
     (IsTargetAddr(ProcSym.Address))
  then begin
    Result := ProcSym.Address.Address;
    AnFpDebugger.SetCachedData(pointer(TFpDbgValueConverterJsonForDebug), Result);
  end;
  ProcSym.ReleaseReference;
end;

function TFpDbgValueConverterJsonForDebug.JsonAddressKeyIsStored: Boolean;
begin
  Result := FJsonAddressKey <> 'Address';
end;

function TFpDbgValueConverterJsonForDebug.FunctionNameIsStored: Boolean;
begin
  Result := FFunctionName <> 'JsonForDebug';
end;

function TFpDbgValueConverterJsonForDebug.GetSettingsFrame: TLazDbgValueConverterSettingsFrameIntf;
begin
  Result := TJsonForDebugSettingsFrame.Create(nil);
end;

procedure TFpDbgValueConverterJsonForDebug.Init;
begin
  inherited Init;
  FFunctionName := 'JsonForDebug';
  FJsonAddressKey := 'Address'
end;

class function TFpDbgValueConverterJsonForDebug.GetName: String;
begin
  Result := drsCallJsonForDebug;
end;

function TFpDbgValueConverterJsonForDebug.GetRegistryEntry: TLazDbgValueConvertRegistryEntryClass;
begin
  Result := TFpDbgValueConverterJsonForDebugRegistryEntry;
end;

procedure TFpDbgValueConverterJsonForDebug.Assign(ASource: TFpDbgValueConverter);
begin
  inherited Assign(ASource);
  if ASource is TFpDbgValueConverterJsonForDebug then begin
    FFunctionName    := TFpDbgValueConverterJsonForDebug(ASource).FFunctionName;
    FJsonAddressKey  := TFpDbgValueConverterJsonForDebug(ASource).FJsonAddressKey;
    FJsonTypenameKey := TFpDbgValueConverterJsonForDebug(ASource).FJsonTypenameKey;
  end;
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

{ TFpDbgValueConverterJsonForDebugRegistryEntry }

class function TFpDbgValueConverterJsonForDebugRegistryEntry.GetConvertorClass: TClass;
begin
  Result := TFpDbgValueConverterJsonForDebug;
end;

initialization
  ValueConverterRegistry.Add(TFpDbgValueConverterJsonForDebugRegistryEntry);

end.


unit FpDbgCallContextInfo;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
interface

uses
  DbgIntfBaseTypes,
  FpDbgInfo,
  FpdMemoryTools,
  FpDbgDwarfDataClasses,
  FpDbgDwarf,
  FpDbgClasses, FpErrorMessages;

type
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
    FLastError: TFpError;
  public
    constructor Create(const ABaseContext: TFpDbgLocationContext;
      AMemReader: TFpDbgMemReaderBase;
      AMemConverter: TFpDbgMemConvertor;
      ADbgProcess: TDbgProcess);
    function CreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; AName: String = ''): TFpValue; virtual;
    function AddParam(AParamIndex: Integer; AParamSymbolType: TFpSymbol; AValue: TFpValue): Boolean;
    property LastError: TFpError read FLastError;
  end;

implementation

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

constructor TFpDbgInfoCallContext.Create(
  const ABaseContext: TFpDbgLocationContext; AMemReader: TFpDbgMemReaderBase;
  AMemConverter: TFpDbgMemConvertor; ADbgProcess: TDbgProcess);
begin
  inherited Create(ABaseContext, AMemReader, AMemConverter);
  FDbgProcess := ADbgProcess;
end;

function TFpDbgInfoCallContext.CreateParamSymbol(AParamIndex: Integer;
  ASymbolType: TFpSymbol; AName: String): TFpValue;
var
  ParameterMemLocation: TFpDbgMemLocation;
  ParamSymbol: TFpSymbol;// TFpSymbolDwarfFunctionResult;
begin
  ParameterMemLocation := FDbgProcess.CallParamDefaultLocation(AParamIndex);
  if AName = '' then
    AName := ASymbolType.Name;
  ParamSymbol := TFpSymbolDwarfFunctionResult.Create(AName, ParameterMemLocation, ASymbolType.TypeInfo);
  try
    Result := ParamSymbol.Value;
  finally
    ParamSymbol.ReleaseReference;
  end;
  TFpValueDwarf(Result).Context := Self;
end;

function TFpDbgInfoCallContext.AddParam(AParamIndex: Integer;
  AParamSymbolType: TFpSymbol; AValue: TFpValue): Boolean;
var
  ParamSymbol: TFpValue;
begin
  Result := False;
  ParamSymbol := CreateParamSymbol(AParamIndex, AParamSymbolType);
  try
    ParamSymbol.AsCardinal := AValue.AsCardinal;
    Result := not IsError(ParamSymbol.LastError);
    FLastError := ParamSymbol.LastError;
  finally
    ParamSymbol.ReleaseReference;
  end;
end;

end.


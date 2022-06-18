unit FpDbgCallContextInfo;

{$mode objfpc}{$H+}

interface

uses
  DbgIntfBaseTypes,
  FpDbgInfo,
  FpdMemoryTools,
  FpDbgDwarfDataClasses,
  FpDbgDwarf,
  FpDbgClasses;

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
  public
    function CreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; ADbgProcess: TDbgProcess; AName: String = ''): TFpValue; virtual;
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

function TFpDbgInfoCallContext.CreateParamSymbol(AParamIndex: Integer;
  ASymbolType: TFpSymbol; ADbgProcess: TDbgProcess; AName: String): TFpValue;
var
  ParameterMemLocation: TFpDbgMemLocation;
  ParamSymbol: TFpSymbol;// TFpSymbolDwarfFunctionResult;
begin
  ParameterMemLocation := ADbgProcess.CallParamDefaultLocation(AParamIndex);
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

end.


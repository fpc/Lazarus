unit FpDbgCallContextInfo;

{$mode objfpc}{$H+}

interface

uses
  DbgIntfBaseTypes,
  FpDbgInfo,
  FpdMemoryTools,
  FpDbgDwarfDataClasses,
  FpDbgDwarf;

type
  { TFpDbgInfoCallContext }

  TFpDbgInfoCallContext = class(TFpDbgAbstractCallContext)
  public
    function CreateParamSymbol(AParamIndex: Integer; ASymbol: TFpSymbol): TFpValue; override;
  end;

implementation

{ TFpDbgInfoCallContext }

function TFpDbgInfoCallContext.CreateParamSymbol(AParamIndex: Integer; ASymbol: TFpSymbol): TFpValue;
var
  ParameterMemLocation: TFpDbgMemLocation;
  TypeSymbol: TFpSymbol;
  ParamSymbol: TFpSymbolDwarfFunctionResult;
begin
  ParameterMemLocation := RegisterLoc(5);
  TypeSymbol := ASymbol.TypeInfo;
  ParamSymbol := TFpSymbolDwarfFunctionResult.Create(ASymbol.Name, TDbgDwarfSymbolBase(ASymbol).InformationEntry, TypeSymbol.Kind, ParameterMemLocation);
  try
    Result := ParamSymbol.Value;
  finally
    ParamSymbol.ReleaseReference;
  end;
  TFpValueDwarf(Result).Context := Self;
end;

end.


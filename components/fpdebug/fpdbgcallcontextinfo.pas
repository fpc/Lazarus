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
  { TFpDbgInfoCallContext }

  TFpDbgInfoCallContext = class(TFpDbgAbstractCallContext)
  public
    function CreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; ADbgProcess: TDbgProcess): TFpValue; virtual;
  end;

implementation

{ TFpDbgInfoCallContext }

function TFpDbgInfoCallContext.CreateParamSymbol(AParamIndex: Integer; ASymbolType: TFpSymbol; ADbgProcess: TDbgProcess): TFpValue;
var
  ParameterMemLocation: TFpDbgMemLocation;
  TypeSymbol: TFpSymbol;
  ParamSymbol: TFpSymbolDwarfFunctionResult;
begin
  ParameterMemLocation := ADbgProcess.CallParamDefaultLocation(AParamIndex);
  TypeSymbol := ASymbolType.TypeInfo;
  ParamSymbol := TFpSymbolDwarfFunctionResult.Create(ASymbolType.Name, TDbgDwarfSymbolBase(ASymbolType).InformationEntry, TypeSymbol.Kind, ParameterMemLocation);
  try
    Result := ParamSymbol.Value;
  finally
    ParamSymbol.ReleaseReference;
  end;
  TFpValueDwarf(Result).Context := Self;
end;

end.


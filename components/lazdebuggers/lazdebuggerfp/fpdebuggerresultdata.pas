unit FpDebuggerResultData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpWatchResultData, FpDbgInfo, FpdMemoryTools,
  DbgIntfBaseTypes, FpDebugValueConvertors, FpDebugDebuggerBase,
  LazDebuggerIntf;

type

  { TFpLazDbgWatchResultConvertor }

  TFpLazDbgWatchResultConvertor = class(TFpWatchResultConvertor)
  private
    FDebugger: TFpDebugDebuggerBase;
    FExpressionScope: TFpDbgSymbolScope;
    FValConvList: TFpDbgConverterConfigList;
    FValConfig: TFpDbgConverterConfig;

    FOuterKind: TDbgSymbolKind;
    FOuterKindLvl: Integer;
    FMainValueIsArray: Boolean;
    FArrayItemConv: TFpDbgValueConverter;

    function GetValConv(AnFpValue: TFpValue): TFpDbgValueConverter; inline;
  public
    constructor Create(AContext: TFpDbgLocationContext);
    destructor Destroy; override;

    function DoValueToResData(AnFpValue: TFpValue;
      AnResData: TLzDbgWatchDataIntf): Boolean; override;
    property ValConvList: TFpDbgConverterConfigList read FValConvList write FValConvList;
    property ValConfig: TFpDbgConverterConfig read FValConfig write FValConfig;
    property Debugger: TFpDebugDebuggerBase read FDebugger write FDebugger;
    property ExpressionScope: TFpDbgSymbolScope read FExpressionScope write FExpressionScope;
  end;

implementation

{ TFpLazDbgWatchResultConvertor }

function TFpLazDbgWatchResultConvertor.GetValConv(AnFpValue: TFpValue
  ): TFpDbgValueConverter;
var
  i: Integer;
begin
  Result := nil;
  if (ValConfig <> nil) then begin
    if ValConfig.CheckMatch(AnFpValue) then
      Result := ValConfig.Converter;
    if Result <> nil then
      Result.AddReference;
  end;
  if (ValConvList <> nil) then begin
    ValConvList.Lock;
    try
      i := ValConvList.Count - 1;
      while (i >= 0) and (not ValConvList[i].CheckMatch(AnFpValue)) do
        dec(i);
      if i >= 0 then
        Result := ValConvList[i].Converter;
      if Result <> nil then
        Result.AddReference;
    finally
      ValConvList.Unlock;
    end;
  end;
end;

constructor TFpLazDbgWatchResultConvertor.Create(AContext: TFpDbgLocationContext
  );
begin
  inherited Create(AContext);
  FOuterKindLvl := -99
end;

destructor TFpLazDbgWatchResultConvertor.Destroy;
begin
  inherited Destroy;
  FArrayItemConv.ReleaseReference;
end;

function TFpLazDbgWatchResultConvertor.DoValueToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  NewFpVal: TFpValue;
  CurConv: TFpDbgValueConverter;
  AnResFld: TLzDbgWatchDataIntf;
begin
  Result := False;
  if RecurseCnt <= 0 then begin
    FOuterKind := AnFpValue.Kind;
    FOuterKindLvl := RecurseCnt + 1;
  end;

  if (RecurseCnt =-1) and (AnFpValue.Kind in [skArray]) then
    FMainValueIsArray := True;

  CurConv := nil;
  NewFpVal := nil;
  try
    if (RecurseCnt = 0) and (FMainValueIsArray) then begin
      if FArrayItemConv = nil then
        FArrayItemConv := GetValConv(AnFpValue);
      CurConv := FArrayItemConv;
    end
    else
    if (not FMainValueIsArray) and
       ( (RecurseCnt <= 0) or
         ( (RecurseCnt = FOuterKindLvl) and (FOuterKind in [skClass, skRecord, skObject, skInstance, skInterface]) )
       )
    then begin
      CurConv := GetValConv(AnFpValue);
    end;

    if (CurConv <> nil) then begin
      AnResData.CreateStructure(dstInternal);
      AnResFld := AnResData.AddField('', dfvUnknown, []);

      NewFpVal := CurConv.ConvertValue(AnFpValue, Debugger, ExpressionScope);
      if NewFpVal <> nil then begin
        Result := inherited DoValueToResData(NewFpVal, AnResFld);
      end
      else begin
        AnResFld.CreateError('Conversion failed');
        Result := True;
      end;
      AnResData := AnResData.AddField('', dfvUnknown, []);
    end;
  finally
    if CurConv <> FArrayItemConv then
      CurConv.ReleaseReference;
    NewFpVal.ReleaseReference;
  end;

  if inherited DoValueToResData(AnFpValue, AnResData) then
    Result := True;
end;

end.


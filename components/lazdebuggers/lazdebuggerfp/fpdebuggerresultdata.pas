unit FpDebuggerResultData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpWatchResultData, FpDbgInfo, DbgIntfBaseTypes,
  FpDebugValueConvertors, FpDebugDebuggerBase, LazDebuggerIntf;

type

  { TFpLazDbgWatchResultConvertor }

  TFpLazDbgWatchResultConvertor = class(TFpWatchResultConvertor)
  private
    FDebugger: TFpDebugDebuggerBase;
    FExpressionScope: TFpDbgSymbolScope;
    FValConvList: TFpDbgConverterConfigList;
    FValConfig: TFpDbgConverterConfig;

    FOuterKind: TDbgSymbolKind;
    FMainValueIsArray: Boolean;
    FArrayItemConv: TFpDbgValueConverter;

    function GetValConv(AnFpValue: TFpValue): TFpDbgValueConverter; inline;
  public
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

destructor TFpLazDbgWatchResultConvertor.Destroy;
begin
  inherited Destroy;
  FArrayItemConv.ReleaseReference;
end;

function TFpLazDbgWatchResultConvertor.DoValueToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  NewRes: TFpValue;
  CurConv: TFpDbgValueConverter;
begin
  NewRes := nil;
  Result := False;
  if RecurseCnt = 0 then
    FOuterKind := AnFpValue.Kind;

  if (RecurseCnt =-1) and (AnFpValue.Kind in [skArray]) then
    FMainValueIsArray := True;

  CurConv := nil;
  try
    if (RecurseCnt = 0) and (FMainValueIsArray) then begin
      if FArrayItemConv = nil then
        FArrayItemConv := GetValConv(AnFpValue);
      CurConv := FArrayItemConv;
    end
    else
    if (not FMainValueIsArray) and
       ( (RecurseCnt = 0) or
         ( (RecurseCnt = 1) and (FOuterKind in [skClass, skRecord, skObject, skInstance, skInterface]) )
       )
    then begin
      CurConv := GetValConv(AnFpValue);
    end;

    if (CurConv <> nil) then begin
      NewRes := CurConv.ConvertValue(AnFpValue, Debugger, ExpressionScope);
      if NewRes <> nil then
        AnFpValue := NewRes
      else
      if FMainValueIsArray then begin
        AnResData.CreateError('Conversion failed');
        Result := True;
        exit;
      end;
    end;
    if CurConv <> FArrayItemConv then
      CurConv.ReleaseReference;

    Result := inherited DoValueToResData(AnFpValue, AnResData);
  finally
    NewRes.ReleaseReference;
  end;
end;

end.


unit FpDebuggerResultData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpWatchResultData, FpDbgInfo, FpdMemoryTools,
  FpErrorMessages, DbgIntfBaseTypes, LazClasses, FpDebugValueConvertors,
  FpDebugDebuggerBase, LazDebuggerIntf;

type

  { TFpLazDbgWatchResultConvertor }

  TFpLazDbgWatchResultConvertor = class(TFpWatchResultConvertor)
  private
    FDebugger: TFpDebugDebuggerBase;
    FExpressionScope: TFpDbgSymbolScope;
    FValConvList: TFpDbgConverterConfigList;
    FValConfig: TFpDbgConverterConfig;

    FExtraDephtLevelIsArray: Boolean; // defExtraDepth / RecurseCnt=-1
    FExtraDephtLevelItemConv: TFpDbgValueConverter;
    FLevelZeroKind: TDbgSymbolKind;
    FLevelZeroArrayConv: TFpDbgValueConverter;   // All itens in array have same type / optimize and keep converter
    FInArray: Boolean;
    FMaxTotalConv, FMaxArrayConv, FCurMaxArrayConv: Integer;
    FNoConvert: Boolean;

    function GetValConv(AnFpValue: TFpValue): TFpDbgValueConverter; inline;
    procedure SetMaxArrayConv(AValue: Integer);
    procedure SetMaxTotalConv(AValue: Integer);
  public
    destructor Destroy; override;

    function DoValueToResData(AnFpValue: TFpValue;
      AnResData: TLzDbgWatchDataIntf): Boolean; override;
    property ValConvList: TFpDbgConverterConfigList read FValConvList write FValConvList;
    property ValConfig: TFpDbgConverterConfig read FValConfig write FValConfig;
    property Debugger: TFpDebugDebuggerBase read FDebugger write FDebugger;
    property ExpressionScope: TFpDbgSymbolScope read FExpressionScope write FExpressionScope;
    property MaxArrayConv: Integer read FMaxArrayConv write SetMaxArrayConv;
    property MaxTotalConv: Integer read FMaxTotalConv write SetMaxTotalConv;
  end;

implementation

{ TFpLazDbgWatchResultConvertor }

function TFpLazDbgWatchResultConvertor.GetValConv(AnFpValue: TFpValue
  ): TFpDbgValueConverter;
var
  i: Integer;
begin
  Result := nil;
  if (FNoConvert) or
     (FInArray and (FMaxArrayConv <= 0))
  then
    exit;

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

procedure TFpLazDbgWatchResultConvertor.SetMaxArrayConv(AValue: Integer);
begin
  if FMaxArrayConv = AValue then Exit;
  FMaxArrayConv := AValue;
  FCurMaxArrayConv := AValue;
end;

procedure TFpLazDbgWatchResultConvertor.SetMaxTotalConv(AValue: Integer);
begin
  if FMaxTotalConv = AValue then Exit;
  FMaxTotalConv := AValue;
  FNoConvert := FMaxTotalConv <= 0;
end;

destructor TFpLazDbgWatchResultConvertor.Destroy;
begin
  inherited Destroy;
  FExtraDephtLevelItemConv.ReleaseReference;
  FLevelZeroArrayConv.ReleaseReference;
end;

function TFpLazDbgWatchResultConvertor.DoValueToResData(AnFpValue: TFpValue;
  AnResData: TLzDbgWatchDataIntf): Boolean;
var
  NewFpVal: TFpValue;
  CurConv: TFpDbgValueConverter;
  AnResFld: TLzDbgWatchDataIntf;
  WasInArray: Boolean;
begin
  Result := False;

  if (RecurseCnt  = -1) and (AnFpValue.Kind in [skArray]) then
    FExtraDephtLevelIsArray := True;

  if RecurseCnt = 0 then begin
    FLevelZeroKind := AnFpValue.Kind;
    FCurMaxArrayConv := FMaxArrayConv;
    if not FExtraDephtLevelIsArray then
      ReleaseRefAndNil(FLevelZeroArrayConv);
  end;

  WasInArray := FInArray;
  if (RecurseCnt >= 0) and (AnFpValue.Kind in [skArray]) then
    FInArray := True;


  CurConv := nil;
  NewFpVal := nil;
  try
    if (RecurseCnt = 0) and (FExtraDephtLevelIsArray) then begin
      if FExtraDephtLevelItemConv = nil then
        FExtraDephtLevelItemConv := GetValConv(AnFpValue);
      CurConv := FExtraDephtLevelItemConv;
    end
    else
    if (RecurseCnt = 1) and (FLevelZeroKind = skArray) then begin
      if FLevelZeroArrayConv = nil then
        FLevelZeroArrayConv := GetValConv(AnFpValue);
      CurConv := FLevelZeroArrayConv;
    end
    else begin
      CurConv := GetValConv(AnFpValue);
    end;

    if (CurConv <> nil) then begin
      if (FMaxTotalConv <= 0) then
        CurConv := nil
      else
        dec(FMaxTotalConv);

      if FInArray then begin
        if (FCurMaxArrayConv <= 0) then
          CurConv := nil
        else
          dec(FCurMaxArrayConv);
      end;

      AnResData.CreateStructure(dstInternal);
      AnResFld := AnResData.AddField('', dfvUnknown, []);
      if (CurConv <> nil) then begin

        NewFpVal := CurConv.ConvertValue(AnFpValue, Debugger, ExpressionScope);
        if NewFpVal <> nil then begin
          Result := inherited DoValueToResData(NewFpVal, AnResFld);
        end
        else begin
          if IsError(CurConv.LastErrror) then
            AnResFld.CreateError(ErrorHandler.ErrorAsString(CurConv.LastErrror))
          else
            AnResFld.CreateError('Conversion failed');
          Result := True;
        end;
      end
      else
        AnResFld.CreateError('');

      AnResData := AnResData.AddField('', dfvUnknown, []);
    end;
  finally
    if (CurConv <> FExtraDephtLevelItemConv) and
       (CurConv <> FLevelZeroArrayConv)
    then
      CurConv.ReleaseReference;
    NewFpVal.ReleaseReference;
  end;

  if inherited DoValueToResData(AnFpValue, AnResData) then
    Result := True;
  FInArray := WasInArray;
end;

end.


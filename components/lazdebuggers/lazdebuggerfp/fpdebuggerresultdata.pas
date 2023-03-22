unit FpDebuggerResultData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpWatchResultData, FpDbgInfo, FpdMemoryTools,
  FpErrorMessages, DbgIntfBaseTypes, LazClasses, FpDebugValueConvertors,
  FpDebugDebuggerBase, LazDebuggerIntf, LazDebuggerValueConverter;

type

  { TFpLazDbgWatchResultConvertor }

  TFpLazDbgWatchResultConvertor = class(TFpWatchResultConvertor)
  private
    FDebugger: TFpDebugDebuggerBase;
    FExpressionScope: TFpDbgSymbolScope;
    FValConvList: ILazDbgValueConvertSelectorListIntf;
    FValConfig: ILazDbgValueConvertSelectorIntf;

    FExtraDephtLevelIsArray: Boolean; // defExtraDepth / RecurseCnt=-1
    FExtraDephtLevelItemConv: TFpDbgValueConverter;
    FLevelZeroKind: TDbgSymbolKind;
    FLevelZeroArrayConv: TFpDbgValueConverter;   // All itens in array have same type / optimize and keep converter
    FInArray, FInNonConvert: Boolean;
    FMaxTotalConv, FMaxArrayConv, FCurMaxArrayConv: Integer;
    FNoConvert: Boolean;

    function GetValConv(AnFpValue: TFpValue; IgnoreInstanceClass: boolean = False): TFpDbgValueConverter; inline;
    procedure SetMaxArrayConv(AValue: Integer);
    procedure SetMaxTotalConv(AValue: Integer);
  public
    destructor Destroy; override;

    function DoValueToResData(AnFpValue: TFpValue;
      AnResData: IDbgWatchDataIntf): Boolean; override;
    property ValConvList: ILazDbgValueConvertSelectorListIntf read FValConvList write FValConvList;
    property ValConfig: ILazDbgValueConvertSelectorIntf read FValConfig write FValConfig;
    property Debugger: TFpDebugDebuggerBase read FDebugger write FDebugger;
    property ExpressionScope: TFpDbgSymbolScope read FExpressionScope write FExpressionScope;
    property MaxArrayConv: Integer read FMaxArrayConv write SetMaxArrayConv;
    property MaxTotalConv: Integer read FMaxTotalConv write SetMaxTotalConv;
  end;

implementation

{ TFpLazDbgWatchResultConvertor }

function TFpLazDbgWatchResultConvertor.GetValConv(AnFpValue: TFpValue;
  IgnoreInstanceClass: boolean): TFpDbgValueConverter;
var
  i, c: Integer;
begin
  Result := nil;
  if (FNoConvert) or
     (FInArray and (FMaxArrayConv <= 0))
  then
    exit;

  if (ValConfig <> nil) then begin
    if ValConfig.CheckMatch(AnFpValue, IgnoreInstanceClass) then
      Result := ValConfig.GetConverter.GetObject as TFpDbgValueConverter;
    if Result <> nil then
      Result.AddReference;
  end
  else
  if (ValConvList <> nil) then begin
    ValConvList.Lock;
    try
      i := 0;
      c := ValConvList.Count;
      while (i < c) and (not ValConvList[i].CheckMatch(AnFpValue, IgnoreInstanceClass)) do
        inc(i);
      if i < c then
        Result := ValConvList[i].GetConverter.GetObject as TFpDbgValueConverter;
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
  AnResData: IDbgWatchDataIntf): Boolean;
var
  NewFpVal: TFpValue;
  CurConv: TFpDbgValueConverter;
  AnResFld, AnResFld2: IDbgWatchDataIntf;
  WasInArray, WasInNonConvert: Boolean;
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
  WasInNonConvert := FInNonConvert;
  if (RecurseCnt >= 0) and (AnFpValue.Kind in [skArray]) then
    FInArray := True;


  if not FInNonConvert then begin
    CurConv := nil;
    NewFpVal := nil;
    try
      if (RecurseCnt = 0) and (FExtraDephtLevelIsArray) then begin
        if FExtraDephtLevelItemConv = nil then
          FExtraDephtLevelItemConv := GetValConv(AnFpValue, RecurseCnt <> RecurseCntLow);
        CurConv := FExtraDephtLevelItemConv;
        if CurConv <> nil then
          CurConv.AddReference;
      end
      else
      if (RecurseCnt = 1) and (FLevelZeroKind = skArray) then begin
        if FLevelZeroArrayConv = nil then
          FLevelZeroArrayConv := GetValConv(AnFpValue, RecurseCnt <> RecurseCntLow);
        CurConv := FLevelZeroArrayConv;
        if CurConv <> nil then
          CurConv.AddReference;
      end
      else begin
        CurConv := GetValConv(AnFpValue, RecurseCnt <> RecurseCntLow);
      end;

      if (CurConv <> nil) and CurConv.CanHandleValue(AnFpValue, Debugger) then begin
        AnResFld := AnResData.CreateValueHandlerResult(CurConv);

        if (FMaxTotalConv <= 0) then
          ReleaseRefAndNil(CurConv)
        else
        if CurConv.NeedConversionLimit then
          dec(FMaxTotalConv);

        if FInArray then begin
          if (FCurMaxArrayConv <= 0) then
            ReleaseRefAndNil(CurConv)
          else
          if CurConv.NeedConversionLimit then
            dec(FCurMaxArrayConv);
        end;

        if (CurConv <> nil) then begin
          FInNonConvert := True;

          AnResFld2 := AnResFld;
          NewFpVal := CurConv.ConvertValue(AnFpValue, Debugger, ExpressionScope, AnResFld2);
          if AnResFld2 = AnResFld then begin
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
          if (AnResFld2 <> nil) and (NewFpVal <> nil) then
            Result := inherited DoValueToResData(NewFpVal, AnResFld2);
        end
        else
          AnResFld.CreateError('');

        AnResData := AnResData.SetDerefData;
      end;
    finally
      CurConv.ReleaseReference;
      NewFpVal.ReleaseReference;
    end;
  end;

  if inherited DoValueToResData(AnFpValue, AnResData) then
    Result := True;
  FInArray := WasInArray;
  FInNonConvert := WasInNonConvert;
end;

end.


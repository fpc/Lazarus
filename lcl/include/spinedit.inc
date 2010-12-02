{%MainUnit ../spin.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

}

procedure TCustomFloatSpinEdit.UpdateControl;
begin
  if MaxValue < MinValue then FMaxValue := MinValue;
  FValue := GetLimitedValue(FValue);
  
  if (not HandleAllocated) then Exit;

  if ([csLoading, csDestroying] * ComponentState <> []) then
    FUpdatePending := True
  else
  begin
    TWSCustomFloatSpinEditClass(WidgetSetClass).UpdateControl(Self);
    FValueChanged := True;
    FUpdatePending := False;
  end;
end;

class procedure TCustomFloatSpinEdit.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomFloatSpinEdit;
end;

function TCustomFloatSpinEdit.RealGetText: TCaption;
begin
  if HandleAllocated then
    Result := inherited RealGetText
  else
    Result := ValueToStr(FValue);
end;

procedure TCustomFloatSpinEdit.TextChanged;
var
  PrevValue: Double;
begin
  PrevValue := FValue;
  FValueChanged := True;

  if Value <> PrevValue then
  begin
    FValueEmpty := False;
    inherited;
  end;
end;

procedure TCustomFloatSpinEdit.SetMaxValue(const AValue: Double);
begin
  if FMaxValue = AValue then Exit;
  FMaxValue := AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.SetMinValue(const AValue: Double);
begin
  if FMinValue = AValue then Exit;
  FMinValue := AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.SetValueEmpty(const AValue: Boolean);
begin
  if FValueEmpty = AValue then Exit;
  FValueEmpty := AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.SetIncrement(const AIncrement: Double);
begin
  if AIncrement = FIncrement then Exit;
  FIncrement := AIncrement;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateControl;
end;

procedure TCustomFloatSpinEdit.Loaded;
begin
  inherited Loaded;
  if FUpdatePending then UpdateControl;
end;

procedure TCustomFloatSpinEdit.KeyPress(var Key: char);
{Disallow any key that is not a digit, decimalseparator, + or -
 For ease of use translate any decimalpoint or comma to DecimalSeparator
 Tab and backSpace ofcourse should be passed onto inherited KeyPress
 If FDecimals = 0 (as in TSpinEdit), disallow decimalseparator also
 We could make it more sophisticated and only allow +/- at front,
 DecimalSeparator only if not already in text, but this way is Delphi compatible
}
begin
  inherited KeyPress(Key);
  if (Key in ['.',',']) then Key := DefaultFormatSettings.Decimalseparator;
  if not (Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator,'+','-',#8,#9]) then Key := #0;
  if (Key = DefaultFormatSettings.DecimalSeparator) and (FDecimals = 0) then Key := #0;
end;

class function TCustomFloatSpinEdit.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 50;
  Result.CY := 23;
end;

procedure TCustomFloatSpinEdit.SetValue(const AValue: Double);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
  
  // clear FValueChanged to prevent getting the old value from the widget
  FValueChanged := False;
  FUpdatePending := True;
  UpdateControl;
end;

function TCustomFloatSpinEdit.GetValue: Double;
begin
  if HandleAllocated and FValueChanged 
    and not (wcfCreatingHandle in FWinControlFlags) then 
  begin
    FValue := TWSCustomFloatSpinEditClass(WidgetSetClass).GetValue(Self);
    FValueChanged := False;
  end;
  Result := FValue;
end;

procedure TCustomFloatSpinEdit.SetDecimals(ADecimals: Integer);
begin
  if FDecimals = ADecimals then Exit;
  FDecimals := ADecimals;
  UpdateControl;
end;

constructor TCustomFloatSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csSpinEdit;
  
  FIncrement := 1;
  FDecimals := 2;
  FValue := 0;
  FMinValue := 0;
  FMaxValue := 100;
  FUpdatePending := True;
  FValueChanged := True;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

function TCustomFloatSpinEdit.GetLimitedValue(const AValue: Double): Double;
begin
  Result := AValue;
  if FMaxValue >= FMinValue then
  begin
    if Result < FMinValue then Result := FMinValue;
    if Result > FMaxValue then Result := FMaxValue;
  end;
end;

function TCustomFloatSpinEdit.ValueToStr(const AValue: Double): String;
begin
  Result := FloatToStrF(GetLimitedValue(AValue), ffFixed, 20, DecimalPlaces);
end;

function TCustomFloatSpinEdit.StrToValue(const S: String): Double;

  // this is a workaround for the fpc rtl bug in StrToFloatDef function
  // which happens when  DecimalSeparator = ThousandSeparator
  // (look at issue 0013293)
  function MyStrToFloatDef(S: String; Def: Double): Double;
  var
    V: Extended;
  begin
    if TextToFloat(PChar(S), V) then
      Result := V
    else
      Result := Def;
  end;

begin
  try
    Result := GetLimitedValue(MyStrToFloatDef(S, FValue));
  except
    Result := FValue;
  end;
end;

procedure TCustomFloatSpinEdit.FinalizeWnd;
begin
  GetValue;
  inherited FinalizeWnd;
end;

{ TCustomSpinEdit }

function TCustomSpinEdit.GetIncrement: integer;
begin
  Result:=round(FIncrement);
end;

function TCustomSpinEdit.GetMaxValue: integer;
begin
  Result:=round(FMaxValue);
end;

function TCustomSpinEdit.GetMinValue: integer;
begin
  Result:=round(FMinValue);
end;

function TCustomSpinEdit.GetValue: integer;
begin
  Result:=round(inherited GetValue);
end;

procedure TCustomSpinEdit.SetIncrement(const AValue: integer);
begin
  if Increment = AValue then exit;
  inherited SetIncrement(AValue);
end;

procedure TCustomSpinEdit.SetMaxValue(const AValue: integer);
begin
  if MaxValue=AValue then exit;
  inherited SetMaxValue(AValue);
end;

procedure TCustomSpinEdit.SetMinValue(const AValue: integer);
begin
  if MinValue=AValue then exit;
  inherited SetMinValue(AValue);
end;

procedure TCustomSpinEdit.SetValue(const AValue: integer);
begin
  if Value=AValue then exit;
  inherited SetValue(AValue);
end;

constructor TCustomSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  
  FDecimals := 0;
end;

// included by spin.pp

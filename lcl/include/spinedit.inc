{%MainUnit ../spin.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

procedure TCustomFloatSpinEdit.UpdateControl;
begin
  // Both Min and Max can be < 0 and Max is read first. Don't adjust. Issue #38810.
  //if MaxValue < MinValue then FMaxValue := MinValue;
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

procedure TCustomFloatSpinEdit.Change;
begin
  //This needs to be done here, not in TextChanged, since the WS causes a Change, but not a TextChanged.
  if FForceModifiedIsFalseInOnChange then
  begin
    Modified := False;
    FForceModifiedIsFalseInOnChange := False;
  end;
  inherited Change;
end;

procedure TCustomFloatSpinEdit.EditingDone;
begin
  UpdateControl;
  inherited EditingDone;
end;

function TCustomFloatSpinEdit.RealGetText: TCaption;
begin
  if HandleAllocated then
    Result := inherited RealGetText
  else
    Result := ValueToStr(FValue);
end;

procedure TCustomFloatSpinEdit.RealSetText(const AValue: TCaption);
var
  NewValue: Double;
begin
  if TryStrToFloat(AValue, NewValue) then
    Value := NewValue
  else
    inherited RealSetText(AValue);
end;

procedure TCustomFloatSpinEdit.TextChanged;
var
  PrevValue: Double;
begin
  PrevValue := FValue;
  FValueChanged := True;
  if Value <> PrevValue then FValueEmpty := False;
  inherited;
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

function TCustomFloatSpinEdit.MaxValueStored: Boolean;
begin
  Result := not SameValue(FMaxValue, Double(DefMaxValue)); //use typecast to force use of correct overload. Issue #39792
end;

procedure TCustomFloatSpinEdit.KeyPress(var Key: char);
{Disallow any key that is not a digit, decimalseparator, + or -
 For ease of use translate any decimalpoint or comma to DecimalSeparator
 Tab, BackSpace, Cut, Paste, Copy, Undo of course should be passed onto inherited KeyPress
 If FDecimals = 0 (as in TSpinEdit), disallow decimalseparator also
 We could make it more sophisticated and only allow +/- at front,
 DecimalSeparator only if not already in text, but this way is Delphi compatible
}
begin
  inherited KeyPress(Key);
  if (Key in ['.',',']) then Key := DefaultFormatSettings.Decimalseparator;
  if not (Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator,'+','-','e','E',#8,#9,^C,^X,^V,^Z]) then Key := #0;
  if (Key = DefaultFormatSettings.DecimalSeparator) and (FDecimals = 0) then Key := #0;
end;

class function TCustomFloatSpinEdit.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 50;
  Result.CY := 23;
end;

procedure TCustomFloatSpinEdit.SetValue(const AValue: Double);
var
  ValueFromText, OldValue: Double;
begin
  if SameValue(FValue, AValue)
    //if you set text by code (or paste it) and text is not a valid float, then FValue will hold the previous value
    //and in that case we should not exit here...
    and (TryStrToFloat(Text, ValueFromText) and SameValue(ValueFromText, FValue)) then Exit;
  OldValue := FValue;
  FValue := AValue;
  
  // clear FValueChanged to prevent getting the old value from the widget
  FValueChanged := False;
  FUpdatePending := True;

  //UpdateControl will trigger an OnChange through the WS code
  //Modified will no be reset to True by the WS code, but in this case we need it to be False
  if not (csLoading in ComponentState) then
    FForceModifiedIsFalseInOnChange := True;
  UpdateControl;

  if not HandleAllocated
  and ([csLoading, csDestroying] * ComponentState = [])
  and not SameValue(OldValue, FValue) then
    Change;
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

procedure TCustomFloatSpinEdit.SetEditorEnabled(AValue: Boolean);
begin
  if FEditorEnabled <> AValue then
  begin
    FEditorEnabled := AValue;
    if HandleAllocated then
      TWSCustomFloatSpinEditClass(WidgetSetClass).SetEditorEnabled(Self, AValue);
  end;
end;

function TCustomFloatSpinEdit.IncrementStored: Boolean;
begin
  Result := not SameValue(FIncrement, Double(DefIncrement)); //use typecast to force use of correct overload. Issue #39792
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

  FForceModifiedIsFalseInOnChange := False;
  FIncrement := DefIncrement;
  FDecimals := DefDecimals;
  FMaxValue := DefMaxValue;
  FUpdatePending := True;
  FValueChanged := True;
  FEditorEnabled := True;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

function TCustomFloatSpinEdit.GetLimitedValue(const AValue: Double): Double;
begin
  Result := AValue;
  //Delphi does not constrain when MinValue = MaxValue, and does if MaxValue < MinValue,
  //but the latter makes absolutely no sense at all.
  if FMaxValue > FMinValue then
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
begin
  try
    Result := GetLimitedValue(StrToFloatDef(S, FValue));
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

function TCustomSpinEdit.GetLimitedValue(const AValue: Double): Double;
begin
  //The WS may call this function for both TSpinEdit and TFloatSPinEdit, so we cannot change signature to have integer parameters
  Result := inherited GetLimitedValue(AValue);
  if Result > MaxInt then
    Result := MaxInt;
  if Result < Low(Integer) then
    Result := Low(Integer);
end;

// included by spin.pp

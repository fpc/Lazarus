{%MainUnit spinex.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

const
  NvbStrings: Array[TNullValueBehaviour] of string = (
    'nvbShowTextHint',
    'nvbLimitedNullValue',
    'nvbMinValue',
    'nvbMaxValue',
    'nvbInitialValue'
  );

  Digits = ['0'..'9'];
  AllowedControlChars = [#8,#9,^C,^X,^V,^Z];

function DbgS(ANvb: TNullValueBehaviour): String;
begin
  Result := NvbStrings[ANvb];
end;

{ TSpinEditEx }


procedure TSpinEditExBase.UpdateControl;
var
  ANumber: T;
begin
  if (MaxValue < MinValue) then FMaxValue := MinValue;
  if (FNullValueBehaviour <> nvbShowTextHint) then
    FValue := GetLimitedValue(FValue);

  if (not HandleAllocated) then Exit;

  if ([csLoading, csDestroying] * ComponentState <> []) then
    FUpdatePending := True
  else
  begin
    FUpdatePending := False;
    //Update the Text
    if (FNullValueBehaviour = nvbShowTextHint) then
    begin
      if not FSettingValue then
      begin
        if TextIsNumber(Text, ANumber) then
          Text := ValueToStr(GetLimitedValue(ANumber))
        else
          Text := EmptyStr;
      end
      else
      begin
        if IsOutOfLimits(FValue) then
          Text := EmptyStr
        else
          Text := ValueToStr(FValue);
      end;
    end
    else
      Text := ValueToStr(GetLimitedValue(FValue));
  end;
end;

procedure TSpinEditExBase.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if ReadOnly then Exit;
  Case Direction of
    updUp: SpinUpDown(True);
    updDown: SpinUpDown(False);
  end;
end;

procedure TSpinEditExBase.UpDownClick(Sender: TObject; {%H-}Button: TUDBtnType);
begin
  BuddyClick;
end;

function TSpinEditExBase.GetBuddyClassType: TControlClass;
begin
  Result := TUpDown;
end;

procedure TSpinEditExBase.DoEnter;
begin
  inherited DoEnter;
  FInitialValue := GetValue;
end;

function TSpinEditExBase.RealGetText: TCaption;
begin
  if HandleAllocated then
    Result := inherited RealGetText
  else
    Result := ValueToStr(FValue);
end;

procedure TSpinEditExBase.Reset;
begin
  if IsMasked then
    inherited Reset
  else
    Value := FInitialValue;
end;

procedure TSpinEditExBase.EditEditingDone;
begin
  inherited EditEditingDone;
  GetValue;
  {$ifdef debugspinex}
  debugln(['TSpinEditExBase.EditingDone:']);
  debugln(['  FValue = ',FValue,' Text = "',Text,'"']);
  {$endif}
  UpdateControl;
end;

procedure TSpinEditExBase.EditChange;
begin
  {$ifdef debugspinex}
  debugln('TSpinEditExBase.EditChange');
  {$endif}
  inherited EditChange;
end;

procedure TSpinEditExBase.EditKeyDown(var Key: word; Shift: TShiftState);
begin
  inherited EditKeyDown(Key, Shift);
  if (Key = VK_Escape) and (Shift = []) then
  begin
    Key := 0;
    Reset;
  end
  else
  if FArrowKeys and (Key = VK_UP) and (Shift = []) then
  begin
    Key := 0;
    SpinUpDown(True);
  end
  else
  if FArrowKeys and (Key = VK_Down) and (Shift = []) then
  begin
    Key := 0;
    SpinUpDown(False);
  end
end;

procedure TSpinEditExBase.SetMaxValue(const AValue: T);
begin
  if FMaxValue = AValue then Exit;
  FMaxValue := AValue;
  UpdateControl;
end;

procedure TSpinEditExBase.SetMinValue(const AValue: T);
begin
  if FMinValue = AValue then Exit;
  FMinValue := AValue;
  UpdateControl;
end;

procedure TSpinEditExBase.SetIncrement(const AIncrement: T);
begin
  if AIncrement = FIncrement then Exit;
  if AIncrement > 0 then
    FIncrement := AIncrement
  else
    FIncrement := -AIncrement;
end;

procedure TSpinEditExBase.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateControl;
end;

procedure TSpinEditExBase.Loaded;
begin
  inherited Loaded;
  UpDown.MinRepeatInterval := FMinRepeatValue;
  if FUpdatePending then UpdateControl;
end;

function TSpinEditExBase.MaxValueStored: Boolean;
begin
  Result := not SameValue(FMaxValue, DefMaxValue);
end;

procedure TSpinEditExBase.EditMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  inherited EditMouseWheelUp(Shift, MousePos, Handled);
  if not Handled then
    SpinUpDown(True);
end;

procedure TSpinEditExBase.EditMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  inherited EditMouseWheelDown(Shift, MousePos, Handled);
  if not Handled then
    SpinUpDown(False);
end;

procedure TSpinEditExBase.SetValue(const AValue: T);
var
  ValueFromText: T;
begin
  {$ifdef debugspinex}
  debugln(['TSpinEditExBase.SetValue: AValue = ',AValue, ' , FValue=',FValue,' Text="',Text,'"']);
  {$endif}
  if (FValue = AValue)
    //if you set text by code (or paste it) and text is not a valid float, then FValue will hold the previous value
    //and in that case we should not exit here...
    and ({TryStrToFloat(Text, ValueFromText, FFS)} TextIsNumber(Text, ValueFromText) and (ValueFromText = FValue)) then Exit;
  FSettingValue := True;
  FValue := AValue;

  FUpdatePending := True;
  UpdateControl;
  FSettingValue := False;
end;

function TSpinEditExBase.GetValue: T;
begin
  if HandleAllocated
    and not (wcfCreatingHandle in FWinControlFlags) then
  begin
    FValue := StrToValue(Text);
  end;
  Result := FValue;
end;

function TSpinEditExBase.IncrementStored: Boolean;
begin
  Result := not SameValue(FIncrement, DefIncrement);
end;

function TSpinEditExBase.IsLimited: Boolean;
begin
  Result := MaxValue > MinValue;
end;

function TSpinEditExBase.IsOutOfLimits(AValue: T): Boolean;
begin
  Result := IsLimited and ((AValue < MinValue) or (AValue > MaxValue));
end;

function TSpinEditExBase.GetEdit: TGEEdit;
begin
  Result := BaseEditor;
end;

procedure TSpinEditExBase.SetMinRepeatValue(AValue: Byte);
begin
  if FMinRepeatValue = AValue then Exit;
  FMinRepeatValue := AValue;
  if not (csLoading in ComponentState) then
    UpDown.MinRepeatInterval := FMinRepeatValue;
end;

procedure TSpinEditExBase.SpinUpDown(Up: Boolean);
var
  OldValue, NewValue: T;
begin
  if not TextIsNumber(Text, OldValue) then
    NewValue := MinValue
  else
  begin
    if IsOutOfLimits(OldValue) then
      NewValue := GetLimitedValue(OldValue)
    else
    begin
      if Up then
        NewValue := GetLimitedValue(SafeInc(OldValue))
      else
        NewValue := GetLimitedValue(SafeDec(OldValue));
    end;
  end;
  SetValue(NewValue);
end;

function TSpinEditExBase.GetNullValue: T;
begin
  Result := FNullValue;
end;

function TSpinEditExBase.GetUpDown: TUpDown;
begin
  Result := TUpDown(Buddy);
end;

procedure TSpinEditExBase.SetNullValue(AValue: T);
begin
  if (FNullValue = AValue) then Exit;
  FNullValue := AValue;
  UpdateControl;
end;

constructor TSpinEditExBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FArrowKeys := True;
  FIncrement := DefIncrement;
  FMaxValue := DefMaxValue;
  FUpdatePending := True;
  FSettingValue := False;
  FNullValueBehaviour := nvbMinValue;
  FMinRepeatValue := DefMinRepeatValue;

  UpDown.ControlStyle := UpDown.ControlStyle + [csNoDesignVisible];
  Edit.Alignment := taRightJustify;

  {
    A note regarding the Updown control.
    It is by design that UpDown is not set to associate with the Edit.
    Amongst others, it would make it impossible to use with floats,
    nor have a NullValue.
    It also does align as it should when associated.
  }
  UpDown.OnChangingEx := @UpDownChangingEx;
  //OnCick signature of TUpDown differs from TControl.OnClick,
  //Yhe assigning of OnClick in inherited constructor
  //sets TControl(Buddy).OnClick to fire BuddyClick, and that won't do
  //since TUpDown does not fire a regular TControl.OnClick event
  UpDown.OnClick := @UpDownClick;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

function TSpinEditExBase.GetLimitedValue(const AValue: T): T;
begin
  Result := AValue;
  //Delphi does not constrain when MinValue = MaxValue, and does if MaxValue > MinValue,
  //but the latter makes absolutely no sense at all.
  if FMaxValue > FMinValue then
  begin
    if Result < FMinValue then Result := FMinValue;
    if Result > FMaxValue then Result := FMaxValue;
  end;
end;

procedure TSpinEditExBase.FinalizeWnd;
begin
  GetValue;
  inherited FinalizeWnd;
end;


{ TCustomFloatSpinEditEx }

function TCustomFloatSpinEditEx.GetDecimalSeparator: Char;
begin
  Result := FFS.DecimalSeparator;
end;

procedure TCustomFloatSpinEditEx.SetDecimalSeparator(AValue: Char);
begin
  if (AValue = FFS.DecimalSeparator) then Exit;
  FFS.DecimalSeparator := AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEditEx.EditKeyPress(var Key: char);
begin
  inherited EditKeyPress(Key);
  {
   Disallow any key that is not a digit, decimalseparator or '+', '-' or 'E/e'
   For ease of use translate any decimalpoint or comma to DecimalSeparator
   Tab, BackSpace, Cut, Paste, Copy, Undo of course should be passed onto inherited KeyPress
   If FDecimals = 0, disallow decimalseparator also
  }
  if (Key in ['.',',']) then Key := FFS.Decimalseparator;
  if not (Key in (Digits + AllowedControlChars + [FFS.DecimalSeparator,'-','+','e','E'])) then Key := #0;
  if (Key = FFS.DecimalSeparator) and (FDecimals = 0) then Key := #0;
  //if (Key = '-') and IsLimited and (MinValue >= 0) then Key := #0;
end;

function TCustomFloatSpinEditEx.TextIsNumber(const S: String; out ANumber: Double
  ): Boolean;
begin
  {$ifdef debugspinex}
  DbgOut(['TextIsNumber, S ="',S,'": Result = ']);
  {$endif}
  try
    Result := TryStrToFloat(S, ANumber, FFS);
  except
    Result := False;
  end;
  {$ifdef debugspinex}
  debugln([Result]);
  {$endif}
end;

function TCustomFloatSpinEditEx.SafeInc(AValue: Double): Double;
begin
  if ((AValue > 0) and (AValue > (MaxDouble-FIncrement))) then
    Result := MaxDouble
  else
    Result := AValue + FIncrement;
end;

function TCustomFloatSpinEditEx.SafeDec(AValue: Double): Double;
begin
  if (AValue < 0) and ((-MaxDouble + FIncrement) > AValue) then
    Result := -MaxDouble
  else
    Result := AValue - FIncrement;
end;

procedure TCustomFloatSpinEditEx.SetDecimals(ADecimals: Integer);
begin
  if (FDecimals = ADecimals) or (ADecimals < 0) then Exit;
  //if we increment DecimalPlaces first set FValue to correctly have the current DecimalPlaces decimals,
  //and only then do the increment (GetValue will update FValue to have FDecimalPlaces decimals)
  //Issue #0034370
  if (ADecimals > FDecimals) then GetValue;
  FDecimals := ADecimals;
  UpdateControl;
end;


function TCustomFloatSpinEditEx.ValueToStr(const AValue: Double): String;
begin
  Result := FloatToStrF(GetLimitedValue(AValue), ffFixed, 20, DecimalPlaces, FFS);
end;

function TCustomFloatSpinEditEx.StrToValue(const S: String): Double;
var
  Def, D: Double;
begin
  {$ifdef debugspinex}
  debugln(['TCustomFloatSpinEditEx.StrToValue: S="',S,'"']);
  {$endif}
  case FNullValueBehaviour of
    nvbShowTextHint: Def := FNullValue;
    nvbLimitedNullValue: Def := GetLimitedValue(FNullValue);
    nvbMinValue: Def := FMinValue;
    nvbMaxValue: Def := MaxValue;
    nvbInitialValue: Def := FInitialValue;
  end;
  try
    if (FNullValueBehaviour = nvbShowTextHint)then
    begin
      if TextIsNumber(S, D)
      then
        Result := D
      else
        Result := Def;
    end
    else
      Result := GetLimitedValue(StrToFloatDef(S, Def, FFS));
  except
    Result := Def;
  end;
  {$ifdef debugspinex}
  debugln(['  Result=',(Result)]);
  {$endif}
end;

constructor TCustomFloatSpinEditEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFS := DefaultFormatSettings;
  FFS.DecimalSeparator := DefDecimalSeparator;
  FDecimals := DefDecimals;
end;



{ TCustomSpinEditEx }

procedure TCustomSpinEditEx.EditKeyPress(var Key: char);
begin
  inherited EditKeyPress(Key);
  {Disallow any key that is not a digit or -
   Tab, BackSpace, Cut, Paste, Copy, Undo of course should be passed onto inherited KeyPress
  }
  inherited EditKeyPress(Key);
  if not (Key in (Digits + AllowedControlChars + ['-'])) then Key := #0;
  if (Key = '-') and IsLimited and (MinValue >= 0) then Key := #0;
end;

function TCustomSpinEditEx.SafeInc(AValue: Int64): Int64;
begin
  if ((AValue > 0) and (AValue > (High(Int64)-FIncrement))) then
    Result := High(Int64)
  else
    Result := AValue + FIncrement;
end;

function TCustomSpinEditEx.SafeDec(AValue: Int64): Int64;
begin
  if (AValue < 0) and ((Low(Int64) + FIncrement) > AValue) then
    Result := Low(Int64)
  else
    Result := AValue - FIncrement;
end;

function TCustomSpinEditEx.TextIsNumber(const S: String; out ANumber: Int64
  ): Boolean;
var
  N: Int64;
begin
  {$ifdef debugspinex}
  DbgOut(['TCustomSpinEditEx.TextIsNumber: S = "',S,'" Result = ']);
  {$endif}

  try
    Result := TryStrToInt64(S, N);
    ANumber := N;
  except
    Result := False;
  end;
  {$ifdef debugspinex}
  debugln([Result]);
  {$endif}
end;


function TCustomSpinEditEx.ValueToStr(const AValue: Int64): String;
begin
  Result := IntToStr(AValue);
end;

function TCustomSpinEditEx.StrToValue(const S: String): Int64;
var
  Def, N: Int64;
begin
  {$ifdef debugspinex}
  debugln(['TCustomSpinEditEx.StrToValue: S="',S,'"']);
  {$endif}
  case FNullValueBehaviour of
    nvbShowTextHint: Def := FNullValue;
    nvbLimitedNullValue: Def := GetLimitedValue(FNullValue);
    nvbMinValue: Def := FMinValue;
    nvbMaxValue: Def := MaxValue;
    nvbInitialValue: Def := FInitialValue;
  end;
  try
    if (FNullValueBehaviour = nvbShowTextHint)then
    begin
      if TextIsNumber(S, N)
      then
        Result := N
      else
        Result := Def;
    end
    else
      Result := GetLimitedValue(StrToInt64Def(S, Def));
  except
    Result := Def;
  end;
  {$ifdef debugspinex}
  debugln(['  Result=',(Result)]);
  {$endif}
end;



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

function DbgS(ANvb: TNullValueBehaviour): String;
begin
  Result := NvbStrings[ANvb];
end;

procedure TCustomFloatSpinEditEx.UpdateControl;
var
  D: Double;
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
        if TextIsNumber(Text, D) then
          Text := ValueToStr(GetLimitedValue(D))
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


procedure TCustomFloatSpinEditEx.SetDecimalSeparator(AValue: Char);
begin
  if (AValue = FFS.DecimalSeparator) then Exit;
  FFS.DecimalSeparator := AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEditEx.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if ReadOnly then Exit;
  Case Direction of
    updUp: SpinUpDown(True);
    updDown: SpinUpDown(False);
  end;
end;

procedure TCustomFloatSpinEditEx.UpDownClick(Sender: TObject; {%H-}Button: TUDBtnType);
begin
  BuddyClick;
end;


function TCustomFloatSpinEditEx.GetBuddyClassType: TControlClass;
begin
  Result := TUpDown;
end;



procedure TCustomFloatSpinEditEx.DoEnter;
begin
  inherited DoEnter;
  FInitialValue := GetValue;
end;

function TCustomFloatSpinEditEx.RealGetText: TCaption;
begin
  if HandleAllocated then
    Result := inherited RealGetText
  else
    Result := ValueToStr(FValue);
end;

procedure TCustomFloatSpinEditEx.Reset;
begin
  if IsMasked then
    inherited Reset
  else
    Value := FInitialValue;
end;

procedure TCustomFloatSpinEditEx.EditEditingDone;
begin
  inherited EditEditingDone;
  GetValue;
  //debugln(['TCustomFloatSpinEditEx.EditingDone:']);
  //debugln(Format('  FValue = %.2f, Text = "%s"',[FValue,Text]));
  UpdateControl;
end;

procedure TCustomFloatSpinEditEx.EditChange;
begin
  //debugln('TCustomFloatSpinEditEx.EditChange');
  inherited EditChange;
end;

procedure TCustomFloatSpinEditEx.EditKeyDown(var Key: word; Shift: TShiftState);
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

procedure TCustomFloatSpinEditEx.SetMaxValue(const AValue: Double);
begin
  if FMaxValue = AValue then Exit;
  FMaxValue := AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEditEx.SetMinValue(const AValue: Double);
begin
  if FMinValue = AValue then Exit;
  FMinValue := AValue;
  UpdateControl;
end;


procedure TCustomFloatSpinEditEx.SetIncrement(const AIncrement: Double);
begin
  if AIncrement = FIncrement then Exit;
  FIncrement := AIncrement;
  //UpdateControl;
end;

function TCustomFloatSpinEditEx.TextIsNumber(const S: String; out D: Double): Boolean;
begin
  //DbgOut(['TextIsNumber, S ="',S,'": Result = ']);
  try
    Result := TryStrToFloat(S, D, FFS);
  except
    Result := False;
  end;
  //debugln([Result]);
end;

procedure TCustomFloatSpinEditEx.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateControl;
end;

procedure TCustomFloatSpinEditEx.Loaded;
begin
  inherited Loaded;
  UpDown.MinRepeatInterval := FMinRepeatValue;
  if FUpdatePending then UpdateControl;
end;

procedure TCustomFloatSpinEditEx.EditKeyPress(var Key: char);
{Disallow any key that is not a digit, decimalseparator or -
 For ease of use translate any decimalpoint or comma to DecimalSeparator
 Tab, BackSpace, Cut, Paste, Copy, Undo of course should be passed onto inherited KeyPress
 If FDecimals = 0 (as in TSpinEditEx), disallow decimalseparator also
}
begin
  inherited EditKeyPress(Key);
  if (Key in ['.',',']) then Key := FFS.Decimalseparator;
  if not (Key in ['0'..'9', FFS.DecimalSeparator,'-',#8,#9,^C,^X,^V,^Z]) then Key := #0;
  if (Key = FFS.DecimalSeparator) and (FDecimals = 0) then Key := #0;
  if (Key = '-') and IsLimited and (MinValue >= 0) then Key := #0;
end;


procedure TCustomFloatSpinEditEx.SetValue(const AValue: Double);
var
  ValueFromText: Extended;
begin
  //debugln(Format('TCustomFloatSpinEditEx.SetValue: AValue = %.2f, FValue=%.2f, Text="%s"',[AValue,fValue,Text]));
  if (FValue = AValue)
    //if you set text by code (or paste it) and text is not a valid float, then FValue will hold the previous value
    //and in that case we should not exit here...
    and (TryStrToFloat(Text, ValueFromText, FFS) and (ValueFromText = FValue)) then Exit;
  FSettingValue := True;
  FValue := AValue;

  FUpdatePending := True;
  UpdateControl;
  FSettingValue := False;
end;

function TCustomFloatSpinEditEx.GetValue: Double;
begin
  if HandleAllocated
    and not (wcfCreatingHandle in FWinControlFlags) then
  begin
    FValue := StrToValue(Text);
  end;
  Result := FValue;
end;

function TCustomFloatSpinEditEx.IsLimited: Boolean;
begin
  Result := MaxValue > MinValue;
end;

function TCustomFloatSpinEditEx.IsOutOfLimits(AValue: Double): Boolean;
begin
  Result := IsLimited and ((AValue < MinValue) or (AValue > MaxValue));
end;


function TCustomFloatSpinEditEx.GetDecimalSeparator: Char;
begin
  Result := FFS.DecimalSeparator;
end;

function TCustomFloatSpinEditEx.GetEdit: TGEEdit;
begin
  Result := BaseEditor;
end;

procedure TCustomFloatSpinEditEx.SetMinRepeatValue(AValue: Byte);
begin
  if FMinRepeatValue = AValue then Exit;
  FMinRepeatValue := AValue;
  if not (csLoading in ComponentState) then
    UpDown.MinRepeatInterval := FMinRepeatValue;
end;


procedure TCustomFloatSpinEditEx.SpinUpDown(Up: Boolean);
var
  OldValue, NewValue: Double;
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
        NewValue := GetLimitedValue(OldValue + Increment)
      else
        NewValue := GetLimitedValue(OldValue - Increment)
    end;
  end;
  SetValue(NewValue);
end;

function TCustomFloatSpinEditEx.GetNullValue: Double;
begin
  Result := FNullValue;
end;

function TCustomFloatSpinEditEx.GetUpDown: TUpDown;
begin
  Result := TUpDown(Buddy);
end;

procedure TCustomFloatSpinEditEx.SetNullValue(AValue: Double);
begin
  if (FNullValue = AValue) then Exit;
  FNullValue := AValue;
  UpdateControl;
end;

procedure TCustomFloatSpinEditEx.SetDecimals(ADecimals: Integer);
begin
  if FDecimals = ADecimals then Exit;
  FDecimals := ADecimals;
  UpdateControl;
end;

constructor TCustomFloatSpinEditEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFS := DefaultFormatSettings;
  FFS.DecimalSeparator := '.';
  FArrowKeys := True;
  FIncrement := 1;
  FDecimals := 2;
  FValue := 0;
  FMinValue := 0;
  FMaxValue := 100;
  FUpdatePending := True;
  FSettingValue := False;
  FNullValueBehaviour := nvbMinValue;
  FMinRepeatValue := 100;

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

function TCustomFloatSpinEditEx.GetLimitedValue(const AValue: Double): Double;
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

function TCustomFloatSpinEditEx.ValueToStr(const AValue: Double): String;
begin
  Result := FloatToStrF(GetLimitedValue(AValue), ffFixed, 20, DecimalPlaces, FFS);
end;

function TCustomFloatSpinEditEx.StrToValue(const S: String): Double;
var
  Def, D: Double;
begin
  //debugln(['TCustomFloatSpinEditEx.StrToValue: S="',S,'"']);
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
  //debugln(['  Result=',(Result)]);
end;

procedure TCustomFloatSpinEditEx.FinalizeWnd;
begin
  GetValue;
  inherited FinalizeWnd;
end;

{ TCustomSpinEditEx }

function TCustomSpinEditEx.GetIncrement: integer;
begin
  Result:=round(FIncrement);
end;

function TCustomSpinEditEx.GetMaxValue: integer;
begin
  Result:=round(FMaxValue);
end;

function TCustomSpinEditEx.GetMinValue: integer;
begin
  Result:=round(FMinValue);
end;

function TCustomSpinEditEx.GetNullValue: integer;
begin
  Result:=round(inherited GetNullValue);
end;

function TCustomSpinEditEx.GetValue: integer;
begin
  Result:=round(inherited GetValue);
end;


procedure TCustomSpinEditEx.SetIncrement(const AValue: integer);
begin
  if Increment = AValue then exit;
  inherited SetIncrement(AValue);
end;

procedure TCustomSpinEditEx.SetMaxValue(const AValue: integer);
begin
  if MaxValue=AValue then exit;
  inherited SetMaxValue(AValue);
end;

procedure TCustomSpinEditEx.SetMinValue(const AValue: integer);
begin
  if MinValue=AValue then exit;
  inherited SetMinValue(AValue);
end;

procedure TCustomSpinEditEx.SetValue(const AValue: integer);
begin
  if Value=AValue then exit;
  inherited SetValue(AValue);
end;

function TCustomSpinEditEx.TextIsNumber(const S: String; out D: Double): Boolean;
var
  N: Integer;
begin
  //DbgOut(['TCustomSpinEditEx.TextIsNumber: S = "',S,'" Result = ']);
  try
    Result := TryStrToInt(S, N);
    D := N;
  except
    Result := False;
  end;
  //debugln([Result]);
end;

procedure TCustomSpinEditEx.SetNullValue(AValue: integer);
begin
  if (GetNullValue = AValue) then Exit;
  inherited SetNullValue(AValue);
end;

constructor TCustomSpinEditEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  NumbersOnly := True;
  FDecimals := 0;
end;

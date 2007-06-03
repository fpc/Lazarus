{%MainUnit ../spin.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  if (FMinValue <> 0) and (FValue < FMinValue) then FValue := FMinValue;
  if (FMaxValue <> 0) and (FValue > FMaxValue) then FValue := FMaxValue;
  
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

function TCustomFloatSpinEdit.ValueIsStored: Boolean;
begin
  Result := True; // fpc bug, default value is always 0
end;

function TCustomFloatSpinEdit.RealGetText: TCaption;
begin
  if HandleAllocated then
    Result := inherited RealGetText
  else
    Result := FloatToStrF(FValue, ffFixed, 20, DecimalPlaces);
end;

procedure TCustomFloatSpinEdit.TextChanged;
var
  PrevValue: Single;
begin
  PrevValue := FValue;
  FValueChanged := True;
  
  if Value <> PrevValue then FValueEmpty := False;
  
  inherited;
end;

procedure TCustomFloatSpinEdit.SetMaxValue(const AValue: Single);
begin
  if FMaxValue = AValue then Exit;
  FMaxValue := AValue;
  UpdateControl;
end;

function TCustomFloatSpinEdit.MaxValueIsStored: Boolean;
begin
  Result := True; // fpc bug, default value is always 0
end;

function TCustomFloatSpinEdit.MinValueIsStored: Boolean;
begin
  Result := True; // fpc bug, default value is always 0
end;

procedure TCustomFloatSpinEdit.SetMinValue(const AValue: Single);
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

procedure TCustomFloatSpinEdit.SetIncrement(const AIncrement: Single);
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

procedure TCustomFloatSpinEdit.SetValue(const AValue: Single);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
  
  // clear FValueChanged to prevent getting the old value from the widget
  FValueChanged := False;
  FUpdatePending := True;
  UpdateControl;
end;

function TCustomFloatSpinEdit.GetValue: Single;
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

  SetInitialBounds(0, 0, 50, 23);
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
  fDecimals:=0;
end;

// included by spin.pp

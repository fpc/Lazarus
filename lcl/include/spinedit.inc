// included by spin.pp

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

}

{-----------------------------------------------------------------------------}
procedure TSpinEdit.UpdateControl;
begin
  if MaxValue<MinValue then fMaxValue:=MinValue;
  if Value<fMinValue then Value:=fMinValue;
  if Value>fMaxValue then Value:=fMaxValue;
  if (not HandleAllocated) then exit;
  if ([csLoading,csDestroying]*ComponentState<>[]) then begin
    fValueNeedsUpdate:=true;
    exit;
  end;
  CNSendMessage(LM_SetProperties,Self,nil);
  fValueNeedsUpdate:=false;
end;

function TSpinEdit.ValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;

procedure TSpinEdit.SetMaxValue(const AValue: single);
begin
  if FMaxValue=AValue then exit;
  FMaxValue:=AValue;
  UpdateControl;
end;

function TSpinEdit.Climb_RateIsStored: boolean;
begin
  Result:=fClimbRate<>1;
end;

function TSpinEdit.MaxValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;

function TSpinEdit.MinValueIsStored: boolean;
begin
  Result:=true; // fpc bug, default value is always 0
end;


{-----------------------------------------------------------------------------}

procedure TSpinEdit.SetMinValue(const AValue: single);
begin
  if FMinValue=AValue then exit;
  FMinValue:=AValue;
  UpdateControl;
end;

procedure TSpinEdit.SetClimbRate(Num : Single);
begin
  if fClimbRate = Num then exit;
  fClimbRate := Num;
  UpdateControl;
end;

procedure TSpinEdit.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateControl;
end;

procedure TSpinEdit.Loaded;
begin
  inherited Loaded;
  if fValueNeedsUpdate then UpdateControl;
end;

{-----------------------------------------------------------------------------}
Procedure TSpinEdit.SetValue(num : Single);
begin
  if FValue = Num then exit;
  FValue := Num;
  fValueNeedsUpdate:=true;
  UpdateControl;
end;

{-----------------------------------------------------------------------------}
Function TSpinEdit.GetValue : Single;
Var
  Temp : Single;
begin
  if HandleAllocated and (not fValueNeedsUpdate) then begin
    CNSendMessage(LM_GETVALUE,Self,@Temp);
    FValue := Temp;
  end;
  GetValue := fValue;
end;

{-----------------------------------------------------------------------------}
procedure TSpinEdit.SetDecimals(Num : Integer);
begin
  if fDecimals = Num then exit;
  fDecimals := Num;
  UpdateControl;
end;

{-----------------------------------------------------------------------------}
constructor TSpinEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csSpinEdit;

  fClimbRate := 1;
  fDecimals := 2;
  fValue := 0;
  fValueNeedsUpdate := true;
  fMinValue := 0;
  fMaxValue := 100;

  SetBounds(1,1,50,20);
end;

{-----------------------------------------------------------------------------}
destructor TSpinEdit.Destroy;
begin
  inherited Destroy;
end;

// included by spin.pp


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
  if not HandleAllocated then exit;
  if MaxValue<MinValue then fMaxValue:=MinValue;
  if Value<fMinValue then Value:=fMinValue;
  if Value>fMaxValue then Value:=fMaxValue;
  CNSendMessage(LM_SetProperties,Self,nil);
  fValueNeedsUpdate:=false;
end;

procedure TSpinEdit.SetMaxValue(const AValue: single);
begin
  if FMaxValue=AValue then exit;
  FMaxValue:=AValue;
  UpdateControl;
end;

{$IFDEF TestFloatRTTI}
function TSpinEdit.GetMySingle2: single;
begin
  Result:=FMySingle2;
end;

function TSpinEdit.GetMyExtended2: Extended;
begin
  Result:=FMyExtended2;
end;

function TSpinEdit.GetMyExtendeds3(Index: integer): Extended;
begin
  Result:=FMyExtended3;
end;

function TSpinEdit.GetMySingles3(Index: integer): single;
begin
  Result:=FMySingle3;
end;

function TSpinEdit.GetMydouble2: double;
begin
  Result:=FMydouble2;
end;

function TSpinEdit.GetMydoubles3(Index: integer): double;
begin
  Result:=FMydouble3;
end;

procedure TSpinEdit.SetMyExtended2(const AValue: Extended);
begin
  writeln('TSpinEdit.SetMyExtended2 ',AValue);
  FMyExtended2:=AValue;
end;

procedure TSpinEdit.SetMyExtendeds3(Index: integer; const AValue: Extended);
begin
  writeln('TSpinEdit.SetMyExtendeds3 ',AValue);
  FMyExtended3:=AValue;
end;

procedure TSpinEdit.SetMySingle2(const AValue: single);
begin
  writeln('TSpinEdit.SetMySingle2 ',AValue);
  FMySingle2:=AValue;
end;

procedure TSpinEdit.SetMySingles3(Index: integer; const AValue: single);
begin
  writeln('TSpinEdit.SetMySingles3 ',AValue);
  FMySingle3:=AValue;
end;

procedure TSpinEdit.SetMydouble2(const AValue: double);
begin
  writeln('TSpinEdit.SetMydouble2 ',AValue,' ',HexStr(Cardinal(@AValue),8));
  FMydouble2:=AValue;
end;

procedure TSpinEdit.SetMydoubles3(Index: integer; const AValue: double);
begin
  writeln('TSpinEdit.SetMydoubles3 ',AValue);
  FMydouble3:=AValue;
end;
{$ENDIF}

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
  fValue := 1;
  fValueNeedsUpdate := true;
  fMinValue := 1;
  fMaxValue := 100;

  SetBounds(1,1,50,20);
end;

{-----------------------------------------------------------------------------}
destructor TSpinEdit.Destroy;
begin
  inherited Destroy;
end;

// included by spin.pp


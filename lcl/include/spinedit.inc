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
  CNSendMessage(LM_SetProperties,Self,nil);
  fValueNeedsUpdate:=false;
end;

{-----------------------------------------------------------------------------}
procedure TSpinEdit.SetClimbRate(Num : Single);
begin
  if fClimbRate = Num then exit;
  fClimbRate := Num;
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
  Temp : Real;
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

  SetBounds(1,1,50,20);
end;

{-----------------------------------------------------------------------------}
destructor TSpinEdit.Destroy;
begin
  inherited Destroy;
end;

// included by spin.pp


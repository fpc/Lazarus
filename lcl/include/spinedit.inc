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
 CNSendMessage(LM_SetProperties,self,nil);
end;

{-----------------------------------------------------------------------------}
procedure TSpinEdit.SetClimbRate(Num : Single);
begin
 fClimbRate := Num;
 UpdateControl;
end;

{-----------------------------------------------------------------------------}
Procedure TSpinEdit.SetValue(num : Single);
begin
 FValue := Num;
 UpdateControl;
end;

{-----------------------------------------------------------------------------}
Function TSpinEdit.GetValue : Single;
Var
  Temp : Real;
begin
 CNSendMessage(LM_GETVALUE,Self,@Temp);
 FValue := Temp;
 getvalue := fValue;
end;

{-----------------------------------------------------------------------------}
procedure TSpinEdit.SetDecimals(Num : Integer);
begin
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

 setbounds(1,1,50,20);

end;

{-----------------------------------------------------------------------------}
destructor TSpinEdit.Destroy;
begin
  inherited Destroy;
end;

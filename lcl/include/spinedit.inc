{-----------------------------------------------------------------------------}
procedure TSpinEdit.SetClimbRate(Num : Single);
begin
 fClimbRate := Num;
// ******************************
// the following line has been commented out so that gtk dependency can be
// removed from all files. The following function needs to be moved.   MAH
//
// PgtkSpinButton(fComponent)^.Climb_rate := fCLimbRate;
end;

{-----------------------------------------------------------------------------}
Procedure TSpinEdit.SetValue(num : Single);
begin
 FValue := Num;
// ******************************
// the following line has been commented out so that gtk dependency can be
// removed from all files. The following function needs to be moved.   MAH
//
// gtk_spin_button_set_value(PgtkSpinButton(fComponent),FValue);
end;

{-----------------------------------------------------------------------------}
Function TSpinEdit.GetValue : Single;
begin
 getvalue := fValue;
end;

{-----------------------------------------------------------------------------}
procedure TSpinEdit.SetDecimals(Num : Integer);
begin
 fDecimals := Num;
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

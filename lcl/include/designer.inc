{******************************************************************************
                                  TDesigner
 ******************************************************************************}
{ TDesigner }

function TDesigner.GetIsControl: Boolean;
begin
// commented out because of a compiler error
//  Result := (FCustomForm <> nil) and FCustomForm.IsControl;
Result := true;
end;

procedure TDesigner.SetIsControl(Value: Boolean);
begin
//commented out because of a compiler error
//  if (FCustomForm <> nil) then FCustomForm.IsControl := Value;

end;


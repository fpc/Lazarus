{%MainUnit ../stdctrls.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{------------------------------------------------------------------------------}

class procedure TToggleBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterToggleBox;
end;

class function TToggleBox.GetControlClassDefaultSize: TSize;
begin
  //Use TButton's size
  Result := TButton.GetControlClassDefaultSize;
end;

procedure TToggleBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style and not BS_3STATE) or BS_AUTOCHECKBOX or BS_PUSHLIKE;
end;

constructor TToggleBox.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csToggleBox;
  TabStop := True;
  ParentColor := False;
end;

{------------------------------------------------------------------------------}

// included by stdctrls.pp

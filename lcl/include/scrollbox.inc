{%MainUnit ../forms.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

class procedure TScrollBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterScrollBox;
end;

constructor TScrollBox.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  fCompStyle:= csScrollBox;
  ControlStyle := ControlStyle + [csCaptureMouse];
  AutoScroll := True;
  BorderStyle := bsSingle;
end;

// included by forms.pp

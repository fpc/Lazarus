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
  ControlStyle := ControlStyle + [csCaptureMouse]
    - [csOpaque] + [csParentBackground]; // we need the default background
  AutoScroll := True;
  BorderStyle := bsSingle;
end;

procedure TScrollBox.CMParentColorChanged(var Message: TLMessage);
begin
  UpdateParentColorChange;
  inherited;
end;

procedure TScrollBox.Loaded;
begin
  inherited Loaded;
  UpdateParentColorChange;
end;

procedure TScrollBox.SetParentBackground(const AParentBackground: Boolean);
begin
  if ParentBackground=AParentBackground then
    Exit;
  inherited;
  UpdateParentColorChange;
end;

procedure TScrollBox.UpdateParentColorChange;
begin
  if ParentColor or ParentBackground then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;

// included by forms.pp

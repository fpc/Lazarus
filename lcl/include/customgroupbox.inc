{%MainUnit ../stdctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

class procedure TCustomGroupBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomGroupBox;
end;

class function TCustomGroupBox.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 185;
  Result.CY := 105;
end;

procedure TCustomGroupBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_GROUPBOX;
end;

{------------------------------------------------------------------------------
  function TCustomGroupBox.Create
------------------------------------------------------------------------------}
constructor TCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csGroupBox;
  ControlStyle := ControlStyle + [csAcceptsControls];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

// included by stdctrls.pp

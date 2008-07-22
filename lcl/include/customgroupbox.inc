{%MainUnit ../stdctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

class function TCustomGroupBox.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=185;
  Result.Y:=105;
end;

{------------------------------------------------------------------------------
  function TCustomGroupBox.Create
------------------------------------------------------------------------------}
constructor TCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csGroupBox;
  ControlStyle := ControlStyle + [csAcceptsControls];
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
end;

// included by stdctrls.pp

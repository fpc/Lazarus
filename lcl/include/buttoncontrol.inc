// included by stdctrls.pp
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

function TButtonControl.IsCheckedStored: boolean;
begin
  Result := true;
  //Result := (ActionLink = nil)
  //  or not TButtonActionLink(ActionLink).IsCheckedLinked;
end;

function TButtonControl.GetChecked: Boolean;
begin
  GetChecked := False;
end;

procedure TButtonControl.SetChecked(Value: Boolean);
begin
  // this is done in the overriden methods
end;

constructor TButtonControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle-csMultiClicks-[csAcceptsControls];
end;

// included by stdctrls.pp


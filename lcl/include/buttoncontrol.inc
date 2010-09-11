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

function TButtonControl.IsCheckedStored: boolean;
begin
  Result := true;
  //Result := (ActionLink = nil)
  //  or not TButtonActionLink(ActionLink).IsCheckedLinked;
end;

procedure TButtonControl.WMDefaultClicked(var Message: TLMessage);
begin
  Click;
end;

class procedure TButtonControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterButtonControl;
end;

function TButtonControl.GetChecked: Boolean;
begin
  GetChecked := False;
end;

procedure TButtonControl.SetChecked(Value: Boolean);
begin
  // this is done in the overriden methods
end;

procedure TButtonControl.DoOnChange;
begin

  if [csLoading, csDestroying, csDesigning] * ComponentState <> [] then Exit;
  EditingDone;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TButtonControl.CMWantSpecialKey(var Message: TLMessage);
begin
  // button controls do not want any special keys
  Message.Result := 0;
end;

procedure TButtonControl.Click;
begin
  DoOnChange;
  inherited Click;
end;

constructor TButtonControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle-csMultiClicks-[csAcceptsControls,csCaptureMouse];
end;

{ TButtonActionLink }

procedure TButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClientButton := AClient as TButtonControl;
end;

function TButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result:=inherited IsCheckedLinked
          and (FClientButton.Checked = (Action as TCustomAction).Checked);
end;

procedure TButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then begin
    FClientButton.ClicksDisabled := True;
    try
      FClientButton.Checked := Value;
    finally
      FClientButton.ClicksDisabled := False;
    end;
  end;
end;

// included by stdctrls.pp

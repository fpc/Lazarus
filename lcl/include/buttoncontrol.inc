{%MainUnit ../stdctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  if not ((csClickEvents in ControlStyle) and (csClicked in ControlState)) then // prevent double click in case of csClickEvents, because clicks are send in MouseUp
    Click;
end;

class procedure TButtonControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterButtonControl;
end;

function TButtonControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TButtonActionLink;
end;

function TButtonControl.GetChecked: Boolean;
begin
  Result := False;
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

procedure TButtonControl.Click;
begin
  DoOnChange;
  inherited Click;
end;

constructor TButtonControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle-csMultiClicks-[csAcceptsControls,csCaptureMouse];
  AccessibleRole := larButton;
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
  if IsCheckedLinked then
  begin
    FClientButton.ClicksDisabled := True;
    try
      FClientButton.Checked := Value;
    finally
      FClientButton.ClicksDisabled := False;
    end;
  end;
end;

// included by stdctrls.pp

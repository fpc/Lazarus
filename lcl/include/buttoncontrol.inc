{%MainUnit ../stdctrls.pp}
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

function TButtonControl.UseOnChangeIsStored: boolean;
begin
  Result:=true;
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
  if Checked=fLastCheckedOnChange then exit;
  fLastCheckedOnChange:=Checked;
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  EditingDone;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TButtonControl.Loaded;
begin
  inherited Loaded;
  fLastCheckedOnChange:=Checked;
end;

procedure TButtonControl.Click;
begin
  DoOnChange;
  inherited Click;
end;

function TButtonControl.ColorIsStored: boolean;
begin
  Result:=(Color<>clBtnFace);
  if Result and ParentColor and (Parent<>nil) then
    Result:=false;
end;

constructor TButtonControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUseOnChange:=DefaultButtonControlUseOnChange;
  ControlStyle:=ControlStyle-csMultiClicks-[csAcceptsControls];
  Color:=clBtnFace;
end;

// included by stdctrls.pp


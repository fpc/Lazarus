{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               TRadioButton
 ******************************************************************************

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

  current design flaws:

  - derived from TCustomCheckBox instead of TButtonControl

  Delphi compatibility:

   - derived from TCustomCheckBox instead of TButtonControl
   - alignment property is missing
   - lots of unknown issues
     
  TODO:

    - check for Delphi compatibility
    * Who's responsible for the fGroup - pointer and who'll free the
      memory allocated for it?????? - automatically managed by GTK+ when
      destroying widget, no worry (MB)
    * make serious tests
    
    - GTK interface : handle reparenting
      
  Bugs:

    - s.a. TCustomCheckbox
}

{------------------------------------------------------------------------------
  Method: TRadioButton.Create
  Params:  aOwner : owner of this object
  Returns: Nothing

  Create a new TRadioButton
 ------------------------------------------------------------------------------}
constructor TRadioButton.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csRadioButton;
  AutoSize := True;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.RealSetText
  Params:  Value: TCaption
  Returns: nothing

  Change the caption, and then recreate to update, then call to AutoSize
 ------------------------------------------------------------------------------}
procedure TRadioButton.RealSetText(const Value: TCaption);
begin
  if Text = Value then
    Exit;
  inherited RealSetText(Value);
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TRadioButton.DoClickOnChange;
begin
  TabStop := FState = cbChecked;
  //inherited calls DoOnChange
  if FState = cbChecked then
    inherited DoClickOnChange
  else
    DoOnChange;
end;

procedure TRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // BS_AUTORADIOBUTTON may hang the application,
  // if the radiobuttons are not consecutive controls.
  Params.Style := (Params.Style and not BS_3STATE) or BS_RADIOBUTTON;
end;

class procedure TRadioButton.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterRadioButton;
end;

procedure TRadioButton.ApplyChanges;
var
  Sibling: TControl;
  i: Integer;
begin
  inherited ApplyChanges;
  //the siblings are unchecked by the widgetset. When the handle is not allocated,
  //the widgetset is not notified so it cannot update the siblings
  if not HandleAllocated and (FState = cbChecked) and
   (Parent <> nil) and (not (csLoading in ComponentState)) then
  begin
    for i := 0 to Parent.ControlCount - 1 do
    begin
      Sibling := Parent.Controls[i];
      if (Sibling is TRadioButton) and (Sibling <> Self) then
        TRadioButton(Sibling).Checked := False;
    end;
  end;
end;

function TRadioButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SetFocus;
    Result := True;
  end else
    Result := inherited;
end;

// included by stdctrls.pp



// included by stdctrls.pp

{******************************************************************************
                               TCustomCheckbox
 ******************************************************************************

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

  current design flaws:

  - To always get the real state of the component we should have a
    callback in this class. Since the OnClick callback is already assigned
    in TControl, we can't use it here. (s.a. Bugs section below!)

  Delphi compatibility:

   - GTK does not support the cbGrayed state so it's not handled
   - alignment property is missing
   - lots of unknown issues

  TODO:

    - check for Delphi compatibility
    - test if fState / Checked is always set right

  Bugs:

    - Seems as if you can't (yet) use the State/Checked properties in
      the OnClick handler. Not sure if there some bug inside this class
      or if the Callback is called before the state of the component has
      been updated.
}

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.SetState
  Params:  value: new state of the object
  Returns: Nothing

  Set new state of the checkbox.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.SetState(Value : TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.GetState
  Params:  none
  Returns: current state of the object

  Get current state of the checkbox. To get the real state a call to the
  interface is made here.
 ------------------------------------------------------------------------------}
function TCustomCheckBox.GetState : TCheckBoxState;
begin
  if HandleAllocated
  then SendMsgToInterface (LM_GETVALUE, Self, @fState);	// get the actual state of the component
  GetState := fState;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.Create
  Params:  TheOwner: the owner of the class
  Returns: Nothing

  Constructor for custom checkbox.
 ------------------------------------------------------------------------------}
constructor TCustomCheckBox.Create(TheOwner : TComponent);
begin
  Inherited Create(TheOwner);
  fCompStyle := csCheckbox;
  FState       := cbUnchecked;
  FAllowGrayed := True;
  TabStop := true;
  Height := 20;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.InitializeWnd
  Params: none
  Returns: Nothing

  Set all properties after visual component has been created. Will be called
  from TWinControl.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.InitializeWnd;
begin
  inherited InitializeWnd;
  ApplyChanges;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.GetChecked
  Params:  none
  Returns: current state of the object

  Get current state of the checkbox and return it as boolean.
 ------------------------------------------------------------------------------}
function TCustomCheckBox.GetChecked : Boolean;
begin
  GetChecked := (GetState = cbChecked);
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.SetChecked
  Params:  Value - new state of checkbox
  Returns: Nothing

  Set the new state of the checkbox as boolean.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.SetChecked(Value : Boolean);
var
  OldState : TCheckBoxState;
begin
  OldState := FState;
  If Value then
    FState := cbChecked
  else
    FState := cbUnChecked;
  if FState <> OldState then
  begin
    if Assigned(Action)
    and (Action is TCustomAction) then
      TCustomAction(Action).Checked := FState=cbChecked;
    ApplyChanges;
    if UseOnChange then begin
      DoOnChange;
    end else begin
      if not ClicksDisabled then Click;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.Toggle
  Params: none
  Returns: Nothing

  Toggle the current state of the checkbox.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.Toggle;
begin
  SetChecked(not GetChecked);
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.ApplyChanges
  Params: none
  Returns: Nothing

  Sends message to update the visual apperance of the object.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.ApplyChanges;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then begin
    SendMsgToInterface(LM_SetValue,Self,@fState);
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomCheckBox.SetText(const Value: TCaption);
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.SetText(const Value: TCaption);
var
  ParseStr : String;
  AccelIndex : Longint;
begin
  Inherited SetText(Value);
  If (not HandleAllocated) or (csDesigning in ComponentState) then exit;
  ParseStr := Value;
  AccelIndex := DeleteAmpersands(ParseStr);
  If AccelIndex > -1 then begin
    With FShortcut do begin
      Handle := Self.Handle;
      OldKey := NewKey;
      NewKey := Char2VK(ParseStr[AccelIndex]);
    end;
    SendMsgToInterface(LM_SETSHORTCUT, Self, @FShortcut);
  end;
end;

// included by stdctrls.pp

{
  $Log$
  Revision 1.17  2004/02/22 22:52:57  micha
  split interface into non-lcl and lcl-component dependent parts

  Revision 1.16  2004/02/05 09:45:33  mattias
  implemented Actions for TSpeedButton, TMenuItem, TCheckBox

  Revision 1.15  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.14  2003/09/16 11:35:14  mattias
  started TDBCheckBox

  Revision 1.13  2003/03/25 16:56:57  mattias
  implemented TButtonControl.UseOnChange

  Revision 1.12  2003/03/25 16:29:53  mattias
  fixed sending TButtonControl.OnClick on every change

  Revision 1.11  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.10  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.9  2002/10/09 10:22:54  lazarus
  MG: fixed client origin coordinates

  Revision 1.8  2002/10/03 00:08:50  lazarus
  AJ: TCustomLabel Autosize, TCustomCheckbox '&' shortcuts started

  Revision 1.7  2002/09/10 07:33:37  lazarus
  MG: fixed TCheckBox.DoAutoSize on loading

  Revision 1.6  2002/08/27 14:33:37  lazarus
  MG: fixed designer component deletion

  Revision 1.5  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.4  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.3  2002/04/02 16:44:41  lazarus
  MG: bugfixes, finddeclaration cache dependencies

  Revision 1.2  2002/02/08 16:45:09  lazarus
  MG: added codetools options

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.2  2000/06/22 20:57:07  lazarus
  *** empty log message ***

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.6  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.5  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.4  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

  Revision 1.3  1999/12/30 19:04:13  lazarus
   - Made TRadiobutton work again
   - Some more cleanups to checkbox code
           stoppok

}
// included by stdctrls.pp


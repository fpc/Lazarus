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
  then CNSendMessage (LM_GETVALUE, Self, @fState);	// get the actual state of the component
  GetState := fState;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for custom checkbox.
 ------------------------------------------------------------------------------}
constructor TCustomCheckBox.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FState       := cbUnchecked;
  FAllowGrayed := True;
  Height:=20;
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
    ApplyChanges;
    if Assigned(OnClick) then OnClick(Self);
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
  if GetChecked then
    SetChecked(False)
  else
    SetChecked(True);
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.ApplyChanges
  Params: none
  Returns: Nothing

  Sends message to update the visual apperance of the object.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.ApplyChanges;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
    CNSendMessage(LM_SetValue,Self,@fState);
end;

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
    CNSendMessage(LM_SETSHORTCUT, Self, @FShortcut);
  end;
end;
{
  $Log$
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


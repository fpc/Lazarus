// included by stdctrls.pp

{******************************************************************************
                                TCustomLabel
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
}

{------------------------------------------------------------------------------
  Method: TCustomLabel.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csLabel;
  FLayout := tlTop;
  FShowAccelChar:= true;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  SetBounds(0, 0, 65, 17);
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.GetLabelText
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TCustomLabel.GetLabelText: String;
begin
  GetLabelText:= Caption;
end;

procedure TCustomLabel.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FFocusControl) and (Operation = opRemove) then FFocusControl:= nil;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetAlignment
  Params: Value - new proterty value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetAlignment(Value : TAlignment);
begin
  if fAlignment <> value then begin
    fAlignment:= value;
    HandleNeeded;
    Invalidate;
    CNSendMessage(LM_SETPROPERTIES, Self, nil);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetFocusControl
  Params: Val - new property value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetFocusControl(Val : TWinControl);
begin
  if Val <> FFocusControl then begin
    FFocusControl:= Val;
    if Val <> nil then Val.FreeNotification(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetLayout
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetLayout(Value : TTextLayout);
begin
  if FLayout <> Value then begin
    FLayout:= Value;
    CNSendMessage(LM_SETPROPERTIES, Self, nil);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetShowAccelChar
  Params: Val - new property value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetShowAccelChar(Val : boolean);
begin
  if Val <> FShowAccelChar then begin
    FShowAccelChar:= Val;
    if HandleAllocated then CNSendMessage(LM_SETLABEL, Self, PChar(Caption));
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetWordWrap
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetWordWrap(Value : Boolean);
begin
  if fWordWrap <> value then begin
    fWordWrap:= value;
    HandleNeeded;
    Invalidate;
    CNSendMessage(LM_SETPROPERTIES, Self, nil);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.WMActivate
  Params: Message: Activation message
  Returns: Nothing

  Received when the label has a registered activation shortcut for focuscontrol.
 ------------------------------------------------------------------------------}
procedure TCustomLabel.WMActivate(var Message: TLMActivate);
begin
  if (FFocusControl <> nil) and (FFocusControl.CanFocus) then
    FFocusControl.SetFocus;
end;

// included by stdctrls.pp

{ =============================================================================

  $Log$
  Revision 1.6  2002/09/03 20:02:01  lazarus
  Intermediate UI patch to show a bug.

  Revision 1.5  2002/09/03 11:32:49  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.4  2002/08/30 10:06:07  lazarus
  Fixed alignment of multiline TLabel.
  Simplified and prettified MessageBoxen.

  Revision 1.3  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.3  2000/05/30 22:28:41  lazarus
  MWE:
    Applied patches from Vincent Snijders:
    + Added GetWindowRect
    * Fixed horz label alignment
    + Added vert label alignment


}

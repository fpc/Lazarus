{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                TCustomStaticText
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
  Method: TCustomStaticText.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csStaticText;
  fAlignment := taLeftJustify;
  FShowAccelChar:= true;
  FStaticBorderStyle:=sbsNone;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  SetInitialBounds(0, 0, 65, 17);
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.GetLabelText
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TCustomStaticText.GetLabelText: String;
begin
  Result := Caption;  
end;

procedure TCustomStaticText.RealSetText(const AValue: TCaption);
begin
  if Text=AValue then exit;
  inherited RealSetText(AValue);
  InvalidatePreferredSize;
  AdjustSize;
end;

{------------------------------------------------------------------------------
  procedure TCustomStaticText.Notification(AComponent: TComponent;
    Operation: TOperation);
 ------------------------------------------------------------------------------}
procedure TCustomStaticText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FFocusControl) and (Operation = opRemove) then
    FFocusControl:= nil;
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.SetAlignment
  Params: Value - new proterty value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomStaticText.SetAlignment(Value: TAlignment);
begin
  if fAlignment <> value then begin
    fAlignment:= value;
    if HandleAllocated then
      TWSCustomStaticTextClass(WidgetSetClass).SetAlignment(Self, Value);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.SetFocusControl
  Params: Val - new property value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomStaticText.SetFocusControl(Val: TWinControl);
begin
  if Val <> FFocusControl then begin
    FFocusControl:= Val;
    if Val <> nil then Val.FreeNotification(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.SetShowAccelChar
  Params: Val - new property value
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomStaticText.SetShowAccelChar(Val: boolean);
begin
  if Val <> FShowAccelChar then begin
    FShowAccelChar:= Val;
    if HandleAllocated then begin
      TWSWinControlClass(WidgetSetClass).SetText(Self,Caption);
      InvalidatePreferredSize;
    end;
  end;
end;

Procedure TCustomStaticText.SetStaticBorderStyle(Value : TStaticBorderStyle);

  procedure RaiseNotImplemented;
  begin
    raise Exception.Create('TCustomStaticText.SetStaticBorderStyle is not implemented yet');
  end;

begin
  If FStaticBorderStyle <> Value then begin
    if ([csDesigning,csLoading]*ComponentState=[csDesigning]) then
      RaiseNotImplemented;
    FStaticBorderStyle := Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomStaticText.WMActivate
  Params: Message: Activation message
  Returns: Nothing

  Received when the label has a registered activation shortcut for focuscontrol.
 ------------------------------------------------------------------------------}
procedure TCustomStaticText.WMActivate(var Message: TLMActivate);
begin
  if (FFocusControl <> nil) and (FFocusControl.CanFocus) then
    FFocusControl.SetFocus;
end;

// included by stdctrls.pp

{ =============================================================================

  $Log$
  Revision 1.15  2005/06/25 01:42:38  mattias
  moved InvalidatePreferredSize from gtk intf to lcl

  Revision 1.14  2005/06/24 14:03:33  mattias
  added some anchorside autosizing, activated new generic autosizing, removed old special DoAutoSize methods

  Revision 1.13  2005/01/26 17:36:02  mattias
  added error message for TStaticText.BorderStyle not implemented during designing

  Revision 1.12  2005/01/24 12:23:11  mattias
  fixed TColorButton.Paint

  Revision 1.11  2005/01/11 21:36:36  micha
  remove TStaticText.Layout property, not supported by delphi, hard to implement

  Revision 1.10  2005/01/07 20:51:11  micha
  swap TCustomStaticText and TCustomLabel

  Revision 1.19  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.18  2004/09/25 15:05:38  mattias
  implemented Rename Identifier

  Revision 1.17  2004/09/22 14:50:18  micha
  convert LM_SETPROPERTIES message for tcustomlabel to interface methods

  Revision 1.16  2004/09/10 09:43:12  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.15  2004/04/20 23:39:01  marc
  * Fixed setting of TWincontrol.Text during load

  Revision 1.14  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.13  2004/03/08 22:36:01  mattias
  added TWinControl.ParentFormInitializeWnd

  Revision 1.12  2004/03/07 09:37:20  mattias
  added workaround for AutoSize in TCustomLabel

  Revision 1.11  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.9  2003/02/28 19:10:25  mattias
  added new ... dialog

  Revision 1.8  2002/10/16 17:06:33  lazarus
  MG: fixed handle allocating in TLabel

  Revision 1.7  2002/10/03 00:08:50  lazarus
  AJ: TCustomLabel Autosize, TCustomCheckbox '&' shortcuts started

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

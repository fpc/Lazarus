{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                     TEdit
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

procedure TCustomEdit.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight);
  // ignore width
  PreferredWidth:=0;
end;

procedure TCustomEdit.CreateWnd;
begin
  inherited CreateWnd;
  TWSCustomEditClass(WidgetSetClass).SetCharCase(Self, FCharCase);
  TWSCustomEditClass(WidgetSetClass).SetEchoMode(Self, FEchoMode);
  TWSCustomEditClass(WidgetSetClass).SetMaxLength(Self, FMaxLength);
  TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, FPasswordChar);
  TWSCustomEditClass(WidgetSetClass).SetReadOnly(Self, FReadOnly);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  //FCompStyle is set here because TEdit inherits from this.
  //TCustomMemo also inherits from here but it's create changes fcompstyle to csMemo
  FCompStyle := csEdit;
  FMaxLength:= -1;
  ParentColor := false;
  TabStop := true;
  SetInitialBounds(0,0,80,23);
  FEchoMode := emNormal;
  BorderStyle := bsSingle;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.GetSelText
  Params: ---
  Returns: selected text

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
function TCustomEdit.GetSelText : string;
begin
  Result:= Copy(Text, SelStart + 1, SelLength)
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SetSelText
  Params: val - new string for text-field
  Returns: nothings

  Replace the selected part of text-field with "val".
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetSelText(const Val : string);
var
  OldText, NewText: string;
begin
  OldText:=Text;
  NewText:=LeftStr(OldText,SelStart)+Val
             +RightStr(OldText,length(OldText)-SelStart-SelLength);
  Text:=NewText;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.GetSelStart
  Params: ---
  Returns: starting index of selected text

  Returns starting index of selected text
 ------------------------------------------------------------------------------}
function TCustomEdit.GetSelStart : integer;
begin
  if HandleAllocated then
    FSelStart:= TWSCustomEditClass(WidgetSetClass).GetSelStart(Self);
  Result:= FSelStart;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SetSelStart
  Params: val -
  Returns: nothing

  Sets starting index for selected text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetSelStart(Val : integer);
begin
  FSelStart:= Val;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetSelStart(Self, Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.GetSelLength
  Params: ---
  Returns: length of selected text

  Returns length of selected text
 ------------------------------------------------------------------------------}
function TCustomEdit.GetSelLength : integer;
begin
  if HandleAllocated then
    FSelLength := TWSCustomEditClass(WidgetSetClass).GetSelLength(Self);
  Result:= FSelLength;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SetSelLength
  Params: val -
  Returns: nothing

  Sets length of selected text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetSelLength(Val : integer);
begin
  if Val<0 then Val:=0;
  FSelLength := Val;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetSelLength(Self, Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.SelectAll
  Params: -
  Returns: nothing

  Select entire text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SelectAll;
begin
  if Text <> '' then begin
    SetSelStart(0);
    SetSelLength(Length(Text));
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.ClearSelection
  Params: -
  Returns: nothing

  Delete selected text.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.ClearSelection;
begin
  if SelLength > 0 then
    SelText := '';
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.ClearSelection
  Params: -
  Returns: nothing

  Copy selected text to clipboard.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.CopyToClipboard;
begin
  if (EchoMode = emNormal) and (SelLength > 0) then
    Clipboard.AsText := SelText;
end;

{------------------------------------------------------------------------------
  Method: TCustomEdit.ClearSelection
  Params: -
  Returns: nothing

  Move selected text to clipboard.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.CutToClipboard;
begin
  CopyToClipboard;
  ClearSelection;
end;

procedure TCustomEdit.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    SelText := Clipboard.AsText;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.GetModified
  Params:  none
  Returns: FModified
 ------------------------------------------------------------------------------}
Function TCustomEdit.GetModified : Boolean;
Begin
  Result := FModified;
End;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetCharCase
  Params:  Value to set FCharCase to
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomEdit.SetCharCase(Value : TEditCharCase);
Begin
  if FCharCase <> value then
  Begin
    FCharCase := Value;
    // update interface, it might do the case conversion itself.
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetCharCase(Self, Value);
    if FCharCase = ecUpperCase then
      Text := Uppercase(Text)
    else if FCharCase = ecLowerCase then
      Text := Lowercase(Text);
  end;
End;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetMaxLength
  Params:  Value to set FMaxLength to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetMaxLength(Value : Integer);
begin
  if Value=MaxLength then exit;
  FMaxLength := Value;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetMaxLength(Self, Value);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetModified(Value : Boolean);
begin
  FModified := Value;
end;

procedure TCustomEdit.SetPasswordChar(const AValue: Char);
begin
  if FPasswordChar=AValue then exit;
  FPasswordChar:=AValue;
  if HandleAllocated then
    TWSCustomEditClass(WidgetSetClass).SetPasswordChar(Self, AValue);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetEchoMode
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetEchoMode(Val : TEchoMode);
begin
  if (Val <> FEchoMode) then begin
    FEchoMode:= Val;
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetEchoMode(Self, Val);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetReadOnly
  Params:  Value to set FReadOnly to
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetReadOnly(Value : Boolean);
begin
  if FreadOnly <> Value then begin
    FReadOnly := Value;
    if HandleAllocated then
      TWSCustomEditClass(WidgetSetClass).SetReadOnly(Self, Value);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.RealSetText
  Params:  Override of text setup to watch for max length
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.RealSetText(const Value: TCaption);
begin
  if (MaxLength > 0) and (Length(Value) > MaxLength) then
    inherited RealSetText(Copy(Value, 1, MaxLength))
  else
    inherited RealSetText(Value);
end;

{------------------------------------------------------------------------------
  function TCustomEdit.ChildClassAllowed(ChildClass: TClass): boolean;
------------------------------------------------------------------------------}
function TCustomEdit.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  // no childs
  Result:=false;
end;

procedure TCustomEdit.ControlKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Application<>nil then Application.DoTabKey(Self,Key,Shift);
  inherited ControlKeyDown(Key, Shift);
end;

procedure TCustomEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key=VK_RETURN then EditingDone;
end;

procedure TCustomEdit.WMChar(var Message: TLMChar);
begin
  // all normal characters are handled by the Edit
  //debugln('TCustomEdit.WMChar ',DbgSName(Self),' ',dbgs(Message.CharCode));
  if Message.CharCode in [ord('A')..ord('Z'),ord('a')..ord('z')] then
    // eat normal keys, so they don't trigger accelerators
    Message.Result := 1
  else
    inherited WMChar(Message);
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomEdit.CMTextChanged(var Message : TLMessage);
var
  Temp : String;
Begin
  //check to see if the charcase should affect the text.
  if FCharCase = ecUppercase then
  Begin
    Temp := Uppercase(Text);
    if Temp <> Text then Text := Temp;
  end
  else
  if FCharCase = ecLowercase then
  Begin
    Temp := Lowercase(Text);
    if Temp <> Text then Text := Temp;
  end;

  Modified := True;
  if HandleAllocated then Change;
End;

Procedure TCustomEdit.Change;
Begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomEdit.InitializeWnd;
var
  ASelStart, ASelLength : integer;
begin
  inherited InitializeWnd;
  if FSelStart <> FSelLength then begin
    ASelStart:= FSelStart;
    ASelLength:= FSelLength;
    SelStart:= ASelStart;
    SelLength:= ASelLength;
  end;
end;

// included by stdctrls.pp

{
  $Log$
  Revision 1.40  2005/07/25 23:11:48  mattias
  fixed TEdit to eat normal keys and avoid triggering accelerators

  Revision 1.39  2005/03/10 09:02:11  mattias
  handle tab key in ControlKeyDown in TCustomEdit and TCustomComboBox

  Revision 1.38  2005/03/08 10:32:47  mattias
  BorderStyle for TCustomEdit in win32 intf  from Jesus

  Revision 1.37  2005/01/26 23:39:16  mattias
  added TMemo.SelLength negative check

  Revision 1.36  2004/12/27 16:51:19  mattias
  implemented dialog to edit basic help db settings

  Revision 1.35  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.34  2004/09/21 13:28:10  micha
  convert LM_SETPROPERTIES to interface methods for TCustomEdit

  Revision 1.33  2004/09/07 10:26:16  micha
  fix logs to get rid of comment level 2 warning

  Revision 1.32  2004/09/07 09:44:46  micha
  convert lcl messages to new interface using methods: LM_G/SETSELSTART, LM_G/SETSELLEN, LM_G/SETLIMITTEXT

  Revision 1.31  2004/08/25 20:16:12  marc
  * fixed infinite for maxlength

  Revision 1.30  2004/07/07 22:26:58  mattias
  fixed showing grabers for boundless components

  Revision 1.29  2004/05/31 19:32:34  vincents
  fixed using ecUpperCase in win32

  Revision 1.28  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.27  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.26  2004/03/07 09:37:20  mattias
  added workaround for AutoSize in TCustomLabel

  Revision 1.25  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.23  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.22  2004/02/04 00:04:37  mattias
  added some TEdit ideas to TSpinEdit

  Revision 1.21  2004/01/23 20:31:43  mattias
  fixed TCustomEdit copying to clipboard

  Revision 1.20  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.19  2003/11/01 18:58:15  mattias
  added clipboard support for TCustomEdit from Colin

  Revision 1.18  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.17  2003/06/13 11:58:46  mattias
  fixed readonly of properties

  Revision 1.16  2002/12/27 17:12:37  mattias
  added more Delphi win32 compatibility functions

  Revision 1.15  2002/12/22 23:25:34  mattias
  fixed setting TEdit properties after creating handle

  Revision 1.14  2002/12/12 17:47:46  mattias
  new constants for compatibility

  Revision 1.13  2002/11/16 11:22:57  mbukovjan
  Fixes to MaxLength. TCustomMemo now has MaxLength, too.

  Revision 1.12  2002/11/13 08:40:44  lazarus
  MB: Fixed selection start/end/text for edits and combos. Add support for memos.

  Revision 1.11  2002/10/01 18:00:04  lazarus
  AJ: Initial TUpDown, minor property additions to improve reading Delphi created forms.

  Revision 1.10  2002/09/07 12:14:50  lazarus
  EchoMode for TCustomEdit. emNone not implemented for GTK+, falls back to emPassword
  behaviour.

  Revision 1.9  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.8  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.7  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.6  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.5  2002/02/25 16:48:13  lazarus
  MG: new IDE window layout system

  Revision 1.4  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.3  2001/01/04 15:09:05  lazarus
  Tested TCustomEdit.Readonly, MaxLength and CharCase.
  Shane

  Revision 1.2  2001/01/04 13:52:00  lazarus
  Minor changes to TEdit.
  Not tested.
  Shane

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.2  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...


}

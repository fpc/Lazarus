// included by stdctrls.pp

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

Procedure TCustomEdit.DoAutoSize;
var
  TM : TTextMetric;
  DC : HDC;
  OldFont : hFont;
begin
  //TCustomEdit.AutoSize only affects Height!!
  If AutoSize and Not AutoSizing then begin
    AutoSizing := True;
    DC := GetDC(0);
    OldFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TM);
    SelectObject(DC, OldFont);
    ReleaseDC(0, DC);
    //TextHeight + 5 top space/border + 5 bottom space/border
    Height := TM.tmHeight + 10;
    AutoSizing := False;
  end;
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
  Height:=23;
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
  NewText:=LeftStr(OldText,SelStart-1)+Val
             +RightStr(OldText,length(OldText)-SelStart-SelLength+1);
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
    FSelStart:= CNSendMessage(LM_GETSELSTART, Self, nil);
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
    CNSendMessage(LM_SETSELSTART, Self, Pointer(Val));
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
    FSelLength := CNSendMessage(LM_GETSELLEN, Self, nil);
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
  FSelLength:= Val;
  if HandleAllocated then
    CNSendMessage(LM_SETSELLEN, Self, Pointer(Val));
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
    CNSendMessage(LM_SETPROPERTIES, Self, nil);
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
    CNSendMessage(LM_SETPROPERTIES, Self, nil);
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
      CNSendMessage(LM_SETPROPERTIES, Self, nil);
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
      CNSendMessage(LM_SETPROPERTIES, Self, nil);
  end;  
end;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetText
  Params:  Override of text setup to watch for max length
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomEdit.SetText(const Value: TCaption);
begin
  if (MaxLength >= 0) and (Length(Value) > MaxLength) then
    inherited SetText(Copy(Value, 1, MaxLength))
  else
    inherited SetText(Value);
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
  //check to see if the charcase should effect the text.
  if FCharCase = ecUppercase then
  Begin
    Temp := Uppercase(text);
    if Temp <> Text then Text := Temp;
  end
  else
  if FCharCase = ecLowercase then
  Begin
    Temp := Lowercase(text);
    if Temp <> Text then Text := Temp;
  end;

  Modified := True;
  if HandleAllocated then Change;
End;

Procedure TCustomEdit.Change;
Begin
  //inherited Change;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomEdit.InitializeWnd;
var ASelStart, ASelLength : integer;
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

{ =============================================================================

  $Log$
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

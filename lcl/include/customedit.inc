// included by stdctrls.pp

(******************************************************************************
                                     TEdit
 ******************************************************************************)

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
  Height:=23;
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
    if FCharCase = ecUpperCase then Text := Uppercase(Text)
       else
    if FCharCase = ecLowerCase then Text := Lowercase(Text);
   end;
End;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetMaxLength
  Params:  Value to set FMaxLength to
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomEdit.SetMaxLength(Value : Integer);
Begin
FMaxLength := Value;
CNSendMessage(LM_SETPROPERTIES, Self, nil);
End;
{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomEdit.SetModified(Value : Boolean);
Begin
FModified := Value;
End;



{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetReadOnly
  Params:  Value to set FReadOnly to
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomEdit.SetReadOnly(Value : Boolean);
Begin
FReadOnly := Value;
CNSendMessage(LM_SETPROPERTIES, Self, nil);
End;

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

// included by stdctrls.pp

{ =============================================================================

  $Log$
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

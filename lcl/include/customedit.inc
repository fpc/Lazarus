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
//TODO: Call SendMessage
End;

{------------------------------------------------------------------------------
  Method:  TCustomEdit.SetModified
  Params:  Value to set FModified to
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomEdit.CMTextChanged(var Message : TLMessage);
Begin
//inherited CMTextChanged(MEssage);
Modified := True;
if HandleAllocated then Change;
End;

Procedure TCustomEdit.Change;
Begin
//inherited Change;
if Assigned(FOnChange) then FOnChange(Self);
end;


{------------------------------------------------------------------------------
  Method:  TCustomEdit.AttachSignals
  Params:  none
  Returns: Nothing

  Gets called after the window is created but before the controls are created 
  and the cached propeties are set. 
  This is the only place where a call to SetCallBack is made.
 ------------------------------------------------------------------------------}
procedure TCustomEdit.AttachSignals; 
begin
  inherited AttachSignals;
  SetCallback(LM_CHANGED);
  SetCallback(LM_CUTTOCLIP);
  SetCallback(LM_COPYTOCLIP);
  SetCallback(LM_PASTEFROMCLIP);
end;

{ =============================================================================

  $Log$
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

{******************************************************************************
                                  TCommonDialog
 ******************************************************************************}
{------------------------------------------------------------------------------
  Method: TCommonDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCommonDialog.Create (AOwner : TComponent);
begin
   inherited Create(AOwner);
end;

function TCommonDialog.Execute : boolean;
begin
  FUserChoice := mrNone;
  CNSendMessage(LM_CREATE, Self, nil);
  Result:= DoExecute;
  CNSendMessage(LM_DESTROY, Self, nil);
  FHandle := 0;
end;

function TCommonDialog.DoExecute : boolean;
begin
  if Assigned(FOnShow) then FOnShow(Self);
  Assert(False, 'Trace:Calling dialog - LM_SHOWMODAL');
  CNSendMessage(LM_SHOWMODAL, Self, nil);
  repeat
     Application.ProcessMessages;
  until FUserChoice <> mrNone;
  Result := (FUserChoice = mrOk);
  if Assigned(FOnClose) then FOnClose(Self);

end;

{ =============================================================================

  $Log$
  Revision 1.2  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.8  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.7  2000/03/24 14:40:41  lazarus
  A little polishing and bug fixing.

  Revision 1.6  2000/02/28 19:16:03  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.5  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.4  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.3  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick


}

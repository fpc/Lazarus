{******************************************************************************
                                  TTimer
 ******************************************************************************}
{
  TTimer Delphi compatibility:

    In contrast to the Delphi timer there are some more public
    properties and functions, because the GTK interface needs
    access to them. To avoid invalid access to them, there's a
    new private variable "AllowIDAccess" which controls access
    to the timerID property which is vital for the class.
    I'm not sure if this approach is thread safe but it works
    for the moment. Just keep in mind not to modify the TimerID
    from your application.

    The procedure Timer is also public now, because the gtk-
    implementation of the timer makes this neccessary (as far as
    I know now)

}

const
  cIdNoTimer = -1;        { timer ID for an invalid timer }

{------------------------------------------------------------------------------
  Method: TTimer.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for a timer.
 ------------------------------------------------------------------------------}
constructor TTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 1000;
  FTimerID  := cIdNoTimer;
  FEnabled  := true;
  AllowIdAccess := false;
end;

{------------------------------------------------------------------------------
  Method: TTimer.Destroy
  Params:  Nothing
  Returns: Nothing

  Destructor for a timer.
 ------------------------------------------------------------------------------}
destructor TTimer.Destroy;
begin
  KillTimer;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TTimer.SetTimerID
  Params:  Value - a timer id
  Returns: Nothing

  Sets an id for the timer if access to the id is currently allowed.
 ------------------------------------------------------------------------------}
procedure TTimer.SetTimerID  (Value : integer);
begin
  if AllowIdAccess then FTimerID := value;
end;

{------------------------------------------------------------------------------
  Method: TTimer.KillTimer
  Params:  Nothing
  Returns: Nothing

  Kills the current timer object.
 ------------------------------------------------------------------------------}
procedure TTimer.KillTimer;
begin
  if FTimerID <> cIdNoTimer then
    CNSendMessage(LM_DESTROY, Self, nil);
  FTimerID := cIdNoTimer;
end;

{------------------------------------------------------------------------------
  Method: TTimer.UpdateTimer
  Params:  Nothing
  Returns: Nothing

  Updates the timer to match the current properties.
 ------------------------------------------------------------------------------}
procedure TTimer.UpdateTimer;
begin
  KillTimer;
  if (FEnabled) and (FInterval > 0) and Assigned (FOnTimer) then
  begin
     AllowIdAccess := true;
     CNSendMessage(LM_CREATE, Self, nil);
     AllowIdAccess := false;
 end;
end;

{------------------------------------------------------------------------------
  Method: TTimer.Timer
  Params:  msg - message to be dispatched
  Returns: Nothing

  Is called when the timer has expired and calls users OnTimer function.
 ------------------------------------------------------------------------------}
procedure TTimer.Timer (var msg);
begin
  Assert(False, 'Trace:Timer received a message -TIMER');
  if Assigned (FOnTimer) and (FEnabled) and (FInterval > 0) then
      FOnTimer(Self);
end;

{------------------------------------------------------------------------------
  Method: TTimer.SetOnTimer
  Params:  value - users notification function
  Returns: Nothing

  Assigns the users notification callback.
 ------------------------------------------------------------------------------}
procedure TTimer.SetOnTimer (value : TNotifyEvent);
begin
  Assert(False, 'Trace:SETTING TIMER CALLBACK');
  FOnTimer := value;
  UpdateTimer;
end;

{------------------------------------------------------------------------------
  Method: TTimer.SetEnabled
  Params:  value - new "enabled" state of the timer
  Returns: Nothing

  En/Disables the timer
 ------------------------------------------------------------------------------}
procedure TTimer.SetEnabled (value : boolean);
begin
  Assert(False, 'Trace:In TTimer.SetEnabled');
  if (value <> FEnabled) then
  begin
     FEnabled := value;
     UpdateTimer;
  end;
end;

{------------------------------------------------------------------------------
  Method: TTimer.SetInterval
  Params:  value - timer interval
  Returns: Nothing

  Sets interval for the timer.
 ------------------------------------------------------------------------------}
procedure TTimer.SetInterval (value : cardinal);
begin
  Assert(False, 'Trace:In TTimer.SetInterval');
  if (value <> FInterval) then
  begin
     FInterval := value;
     UpdateTimer;
  end;
end;

 {
  $Log$
  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.6  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.5  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.4  1999/09/16 21:14:27  lazarus
    Some cleanups to the timer class. (moved some comments to timer.inc,
    added more comments and changed TTimer.Timer from function to procedure)
      Stoppok

  Revision 1.3  1999/08/07 17:59:24  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new CNSendMessage function.   MAH

  Revision 1.2  1999/07/31 06:39:31  lazarus

       Modified the IntCNSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }

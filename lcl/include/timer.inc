// included by extctrls.pp

{******************************************************************************
                                  TTimer
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
{
  TTimer Delphi compatibility:

    nearly 100% compatible, only WndProc is missing

  TODO: -

  Possible improvements: -

  Bugs: unknown
}

const
  cIdNoTimer = -1;        { timer ID for an invalid timer }
  SNoTimers = 'No timers available';

{------------------------------------------------------------------------------
  Method:  TimerCBProc
  Params:  handle  - handle (self) of the TTimer instance
           message - should be LM_Timer, currently unused (s. Win32 API)
           IDEvent - currently unused (s. Win32 API)
           Time    - currently unused (s. Win32 API)
  Returns: Nothing

  Callback for a timer which will call TTimer.Timer. This proc will be used
  if the InterfaceObject uses a callback instead of delivering a LM_Timer 
  message.
 ------------------------------------------------------------------------------}
procedure TimerCBProc(Handle: HWND; message : cardinal; IDEvent: Integer;
  Time: Cardinal);
begin
  // Cast Handle back to timer
  if (Handle<>0) then
    TTimer(Handle).Timer (message);
end;

{------------------------------------------------------------------------------
  Method: TTimer.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for a timer.
 ------------------------------------------------------------------------------}
constructor TTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval    := 1000;
  FTimerHandle := cIdNoTimer;
  FEnabled     := true;
end;

{------------------------------------------------------------------------------
  Method: TTimer.Destroy
  Params:  Nothing
  Returns: Nothing

  Destructor for a timer.
 ------------------------------------------------------------------------------}
destructor TTimer.Destroy;
begin
  FOnTimer:=nil;
  FEnabled:=false;
  KillTimer;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TTimer.KillTimer
  Params:  Nothing
  Returns: Nothing

  Kills the current timer object.
 ------------------------------------------------------------------------------}
procedure TTimer.KillTimer;
begin
  Assert(False, 'Trace:In TTimer.KillTimer');
  if FTimerHandle <> cIdNoTimer then begin
    InterfaceObject.KillTimer (integer(Self), 1);
    FTimerHandle := cIdNoTimer;
  end;
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
    FTimerHandle := InterfaceObject.SetTimer(Integer(Self), 1,
                      FInterval, @TimerCBProc);
  if FTimerHandle=0 then begin
    FTimerHandle:=cIdNoTimer;
    raise EOutOfResources.Create(SNoTimers);
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
  Assert(false, 'Trace:Timer received a message -TIMER');
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
  if Value=FOnTimer then exit;
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
  if (Value <> FEnabled) then
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

// included by extctrls.pp

{
  $Log$
  Revision 1.7  2002/10/15 16:01:36  lazarus
  MG: fixed timers

  Revision 1.6  2002/10/14 19:00:49  lazarus
  MG: fixed zombie timers

  Revision 1.5  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.4  2001/11/19 21:48:54  lazarus
  MG: fixed splash timer AV, incomplete project loading, application save as

  Revision 1.3  2001/09/30 08:34:50  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.2  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

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

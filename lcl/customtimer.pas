{
 /***************************************************************************
                               customtimer.pas
                               ---------------
                         Lazarus Component Library TCustomTimer

 ***************************************************************************/

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
unit CustomTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLLinux, LCLType, LMessages, VCLGlobals;


type
  { TCustomTimer }
  {
    @abstract(A free running timer.)
    Introduced and (currently) maintained by Stefan Hille (stoppok@osibisa.ms.sub.org)
  }
  TCustomTimer = class (TComponent)
  private
    FInterval     : Cardinal;
    FTimerHandle  : integer;
    FOnTimer      : TNotifyEvent;
    FEnabled      : Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure KillTimer;
  protected
    procedure Timer (var msg); message LM_Timer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

implementation

const
  cIdNoTimer = -1;        { timer ID for an invalid timer }
  SNoTimers = 'No timers available';

{------------------------------------------------------------------------------
  Method:  TimerCBProc
  Params:  handle  - handle (self) of the TCustomTimer instance
           message - should be LM_Timer, currently unused (s. Win32 API)
           IDEvent - currently unused (s. Win32 API)
           Time    - currently unused (s. Win32 API)
  Returns: Nothing

  Callback for a timer which will call TCustomTimer.Timer. This proc will be used
  if the InterfaceObject uses a callback instead of delivering a LM_Timer
  message.
 ------------------------------------------------------------------------------}
procedure TimerCBProc(Handle: HWND; message : cardinal; IDEvent: Integer;
  Time: Cardinal);
begin
  // Cast Handle back to timer
  if (Handle<>0) then
    TCustomTimer(Handle).Timer (message);
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for a timer.
 ------------------------------------------------------------------------------}
constructor TCustomTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval    := 1000;
  FTimerHandle := cIdNoTimer;
  FEnabled     := true;
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.Destroy
  Params:  Nothing
  Returns: Nothing

  Destructor for a timer.
 ------------------------------------------------------------------------------}
destructor TCustomTimer.Destroy;
begin
  FOnTimer:=nil;
  FEnabled:=false;
  KillTimer;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.KillTimer
  Params:  Nothing
  Returns: Nothing

  Kills the current timer object.
 ------------------------------------------------------------------------------}
procedure TCustomTimer.KillTimer;
begin
  Assert(False, 'Trace:In TCustomTimer.KillTimer');
  if FTimerHandle <> cIdNoTimer then begin
    LCLLinux.KillTimer (integer(Self), 1);
    FTimerHandle := cIdNoTimer;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.UpdateTimer
  Params:  Nothing
  Returns: Nothing

  Updates the timer to match the current properties.
 ------------------------------------------------------------------------------}
procedure TCustomTimer.UpdateTimer;
begin
  KillTimer;
  if (FEnabled) and (FInterval > 0) and Assigned (FOnTimer) then begin
    FTimerHandle := LCLLinux.SetTimer(Integer(Self), 1,
                      FInterval, @TimerCBProc);
    if FTimerHandle=0 then begin
      FTimerHandle:=cIdNoTimer;
      raise EOutOfResources.Create(SNoTimers);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.Timer
  Params:  msg - message to be dispatched
  Returns: Nothing

  Is called when the timer has expired and calls users OnTimer function.
 ------------------------------------------------------------------------------}
procedure TCustomTimer.Timer (var msg);
begin
  Assert(false, 'Trace:Timer received a message -TIMER');
  if Assigned (FOnTimer) and (FEnabled) and (FInterval > 0) then
    FOnTimer(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.SetOnTimer
  Params:  value - users notification function
  Returns: Nothing

  Assigns the users notification callback.
 ------------------------------------------------------------------------------}
procedure TCustomTimer.SetOnTimer (value : TNotifyEvent);
begin
  if Value=FOnTimer then exit;
  Assert(False, 'Trace:SETTING TIMER CALLBACK');
  FOnTimer := value;
  UpdateTimer;
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.SetEnabled
  Params:  value - new "enabled" state of the timer
  Returns: Nothing

  En/Disables the timer
 ------------------------------------------------------------------------------}
procedure TCustomTimer.SetEnabled (value : boolean);
begin
  Assert(False, 'Trace:In TCustomTimer.SetEnabled');
  if (Value <> FEnabled) then
  begin
    FEnabled := value;
    UpdateTimer;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.SetInterval
  Params:  value - timer interval
  Returns: Nothing

  Sets interval for the timer.
 ------------------------------------------------------------------------------}
procedure TCustomTimer.SetInterval (value : cardinal);
begin
  Assert(False, 'Trace:In TCustomTimer.SetInterval');
  if (value <> FInterval) then
  begin
    FInterval := value;
    UpdateTimer;
  end;
end;

end.


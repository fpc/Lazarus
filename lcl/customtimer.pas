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
  TCustomTimer = class (TComponent)
  private
    FInterval     : Cardinal;
    FOnStartTimer: TNotifyEvent;
    FOnStopTimer: TNotifyEvent;
    FTimerHandle  : integer;
    FOnTimer      : TNotifyEvent;
    FEnabled      : Boolean;
    procedure Timer (var msg); message LM_Timer;
  protected
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetInterval(Value: Cardinal); virtual;
    procedure SetOnTimer(Value: TNotifyEvent); virtual;
    procedure DoOnTimer; virtual;
    procedure UpdateTimer; virtual;
    procedure KillTimer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
    property OnStartTimer: TNotifyEvent read FOnStartTimer write FOnStartTimer;
    property OnStopTimer: TNotifyEvent read FOnStopTimer write FOnStopTimer;
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
  if FTimerHandle <> cIdNoTimer then begin
    LCLLinux.KillTimer (integer(Self), 1);
    FTimerHandle := cIdNoTimer;
    if Assigned(OnStopTimer) then OnStopTimer(Self);
  end;
end;

procedure TCustomTimer.Loaded;
begin
  inherited Loaded;
  UpdateTimer;
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
  if (FEnabled) and (FInterval > 0)
  and (([csDesigning,csLoading]*ComponentState=[]))
  and Assigned (FOnTimer) then begin
    FTimerHandle := LCLLinux.SetTimer(Integer(Self), 1,
                                      FInterval, @TimerCBProc);
    if FTimerHandle=0 then begin
      FTimerHandle:=cIdNoTimer;
      raise EOutOfResources.Create(SNoTimers);
    end;
    if Assigned(OnStartTimer) then OnStartTimer(Self);
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
  if (FEnabled) and (FInterval > 0) then
    DoOnTimer;
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
  FOnTimer := value;
  UpdateTimer;
end;

{------------------------------------------------------------------------------
  procedure TCustomTimer.DoOnTimer;

 ------------------------------------------------------------------------------}
procedure TCustomTimer.DoOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.SetEnabled
  Params:  value - new "enabled" state of the timer
  Returns: Nothing

  En/Disables the timer
 ------------------------------------------------------------------------------}
procedure TCustomTimer.SetEnabled (value : boolean);
begin
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
  if (value <> FInterval) then
  begin
    FInterval := value;
    UpdateTimer;
  end;
end;

end.


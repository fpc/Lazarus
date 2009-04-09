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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  Classes, SysUtils, LCLProc, LCLStrConsts, LCLType, InterfaceBase;

type

  { TCustomTimer }
  
  TCustomTimer = class (TComponent)
  private
    FInterval     : Cardinal;
    FOnStartTimer: TNotifyEvent;
    FOnStopTimer: TNotifyEvent;
    FTimerHandle  : THandle;
    FOnTimer      : TNotifyEvent;
    FEnabled      : Boolean;
    procedure Timer;
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
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
    property OnStartTimer: TNotifyEvent read FOnStartTimer write FOnStartTimer;
    property OnStopTimer: TNotifyEvent read FOnStopTimer write FOnStopTimer;
  end;


implementation

const
  cIdNoTimer = THandle(-1);        { timer ID for an invalid timer }

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
    //DebugLn(['TCustomTimer.KillTimer ',dbgsName(Self)]);
    WidgetSet.DestroyTimer(FTimerHandle);
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
  and (([csDesigning,csLoading,csDestroying]*ComponentState=[]))
  and Assigned (FOnTimer) then begin
    //DebugLn(['TCustomTimer.UpdateTimer ',dbgsName(Self),' WidgetSet.CreateTimer']);
    FTimerHandle := WidgetSet.CreateTimer(FInterval, @Timer);
    if FTimerHandle=0 then begin
      FTimerHandle:=cIdNoTimer;
      raise EOutOfResources.Create(SNoTimers);
    end;
    if Assigned(OnStartTimer) then OnStartTimer(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTimer.Timer
  Returns: Nothing

  Is called when the timer has expired and calls users OnTimer function.
 ------------------------------------------------------------------------------}
procedure TCustomTimer.Timer;
begin
  {$IFDEF VerboseTimer}
  DebugLn(['TCustomTimer.Timer ',dbgsName(Self),' ',FEnabled,' ',FInterval]);
  {$ENDIF}
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
  // Value=FOnTimer only compares code part
  if CompareByte(Value,FOnTimer,SizeOf(Value))=0 then exit;
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

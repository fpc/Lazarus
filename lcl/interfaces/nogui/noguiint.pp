{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
 }

unit NoGUIInt;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  {$IFDEF TraceGdiCalls}
  LineInfo,
  {$ENDIF}
  // rtl+fcl
  Types, Classes, SysUtils,
  // interfacebase
  InterfaceBase,
  // LCL
  Dialogs, Controls, Forms,
  LCLProc, LCLIntf, LCLType, LCLPlatformDef, Graphics, Menus, Themes,
  // LazUtils
  GraphType,
  // widgetset
  WSLCLClasses;

type

  { TNoGUITimer }

  TNoGUITimer = class
  public
    Interval: integer; // ms
    TimerFunc: TWSTimerProc;
    NextTick: QWord; // GetTickCount64 when due
  end;

  { TNoGUIWidgetSet }

  TNoGUIWidgetSet = class(TWidgetSet)
    procedure NoGUIWidgetSetWakeMainThread(Sender: TObject);
  protected
    FTimers: TFPList; // list of TNoGUITimer
    FWakeEvent: PRTLEvent; // nil if no thread manager installed
    function NextTimerDelay: integer;
    procedure CheckTimers;
  public
    procedure PassCmdLineOptions; override;
  public
    function LCLPlatform: TLCLPlatform; override;
    // Application
    procedure AppInit(var {%H-}ScreenInfo: TScreenInfo); override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetTitle(const {%H-}ATitle: string); override;
    function EnumFontFamiliesEx({%H-}DC: HDC; {%H-}lpLogFont: PLogFont; {%H-}Callback: FontEnumExProc; {%H-}Lparam: LParam; {%H-}Flags: dword): longint; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : TLCLHandle; override;
    function DestroyTimer(TimerHandle: TLCLHandle) : boolean; override;
    procedure DestroyLCLComponent(Sender: TObject); virtual;
  public
  end;

var
  NoGUIWidgetSet: TNoGUIWidgetSet;

implementation

{ TNoGUIWidgetSet }

procedure TNoGUIWidgetSet.NoGUIWidgetSetWakeMainThread(Sender: TObject);
// Called by thread to wake up the main thread
begin
  if FWakeEvent<>nil then
    RTLEventSetEvent(FWakeEvent);
end;

function TNoGUIWidgetSet.NextTimerDelay: integer;
// returns ms until the earliest timer is due, 0 if overdue, -1 if no timers
var
  i: Integer;
  aTimer: TNoGUITimer;
  NowTick, Earliest: QWord;
begin
  Result:=-1;
  if FTimers.Count=0 then
    exit;
  Earliest:=High(QWord);
  for i:=0 to FTimers.Count-1 do
  begin
    aTimer:=TNoGUITimer(FTimers[i]);
    if aTimer.NextTick<Earliest then
      Earliest:=aTimer.NextTick;
  end;
  NowTick:=GetTickCount64;
  if Earliest<=NowTick then
    Result:=0
  else if Earliest-NowTick>High(integer) then
    Result:=High(integer)
  else
    Result:=Earliest-NowTick;
end;

procedure TNoGUIWidgetSet.CheckTimers;
var
  TimersDue: TFPList;
  NowTick: QWord;
  i: Integer;
  aTimer: TNoGUITimer;
begin
  if FTimers.Count=0 then
    exit;
  NowTick:=GetTickCount64;
  TimersDue:=nil;
  try
    for i:=0 to FTimers.Count-1 do
    begin
      aTimer:=TNoGUITimer(FTimers[i]);
      if aTimer.NextTick<=NowTick then
      begin
        if TimersDue=nil then
          TimersDue:=TFPList.Create;
        TimersDue.Add(aTimer);
      end;
    end;
    if TimersDue=nil then
      exit;
    for i:=0 to TimersDue.Count-1 do
    begin
      aTimer:=TNoGUITimer(TimersDue[i]);
      // an earlier callback may have destroyed this aTimer
      if FTimers.IndexOf(aTimer)<0 then
        continue;
      // set next tick before calling, the callback may destroy the aTimer
      aTimer.NextTick:=NowTick+QWord(aTimer.Interval);
      aTimer.TimerFunc();
    end;
  finally
    TimersDue.Free;
  end;
end;

procedure TNoGUIWidgetSet.PassCmdLineOptions;
begin
  inherited PassCmdLineOptions;
end;

function TNoGUIWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:=lpNoGUI;
end;

procedure TNoGUIWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin

end;

procedure TNoGUIWidgetSet.AppProcessMessages;
begin
  if IsMultiThread then
    CheckSynchronize;
  CheckTimers;
end;

procedure TNoGUIWidgetSet.AppWaitMessage;
var
  Delay: Integer;
begin
  Delay:=NextTimerDelay;
  if Delay=0 then
    exit;
  if FWakeEvent<>nil then
  begin
    if Delay<0 then
      RTLEventWaitFor(FWakeEvent)
    else
      RTLEventWaitFor(FWakeEvent,Delay);
  end else if Delay>0 then
    // no thread manager: nothing can wake us except timers
    Sleep(Delay);
  // no thread manager and no timers: return at once to avoid hanging forever
end;

procedure TNoGUIWidgetSet.AppTerminate;
begin

end;

procedure TNoGUIWidgetSet.AppMinimize;
begin

end;

procedure TNoGUIWidgetSet.AppRestore;
begin

end;

procedure TNoGUIWidgetSet.AppBringToFront;
begin

end;

procedure TNoGUIWidgetSet.AppSetTitle(const ATitle: string);
begin

end;

function TNoGUIWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin
  Result:=0;
end;

constructor TNoGUIWidgetSet.Create;
begin
  inherited Create;
  FTimers:=TFPList.Create;
  FWakeEvent:=RTLEventCreate;
  WakeMainThread:=@NoGUIWidgetSetWakeMainThread;
end;

destructor TNoGUIWidgetSet.Destroy;
var
  i: Integer;
begin
  if WakeMainThread=@NoGUIWidgetSetWakeMainThread then
    WakeMainThread:=nil;
  for i:=0 to FTimers.Count-1 do
    TNoGUITimer(FTimers[i]).Free;
  FreeAndNil(FTimers);
  if FWakeEvent<>nil then
  begin
    RTLEventDestroy(FWakeEvent);
    FWakeEvent:=nil;
  end;
  inherited Destroy;
end;

function TNoGUIWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc
  ): TLCLHandle;
var
  aTimer: TNoGUITimer;
begin
  Result:=0;
  if not Assigned(TimerFunc) then
    exit;
  if Interval<1 then
    Interval:=1;
  aTimer:=TNoGUITimer.Create;
  aTimer.Interval:=Interval;
  aTimer.TimerFunc:=TimerFunc;
  aTimer.NextTick:=GetTickCount64+QWord(Interval);
  FTimers.Add(aTimer);
  Result:=TLCLHandle(aTimer);
end;

function TNoGUIWidgetSet.DestroyTimer(TimerHandle: TLCLHandle): boolean;
var
  i: Integer;
begin
  Result:=false;
  if TimerHandle=0 then
    exit;
  i:=FTimers.IndexOf(Pointer(TimerHandle));
  if i<0 then
    exit;
  FTimers.Delete(i);
  TNoGUITimer(Pointer(TimerHandle)).Free;
  Result:=true;
end;

procedure TNoGUIWidgetSet.DestroyLCLComponent(Sender: TObject);
begin

end;

end.

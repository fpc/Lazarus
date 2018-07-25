{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit lazCollections;

{$mode objfpc}{$H+}

interface

uses
  sysutils, syncobjs,
  // LazUtils
  LazSysUtils;

type

  { TLazMonitor }

  TLazMonitor = class(TCriticalSection)
  private
    FSpinCount: integer;
    class var FDefaultSpinCount: integer;
    class function GetDefaultSpinCount: integer; static;
    class procedure SetDefaultSpinCount(AValue: integer); static;
    function GetSpinCount: integer;
    procedure SetSpinCount(AValue: integer);
  public
    constructor create;
    procedure Acquire; override;
    property SpinCount: integer read GetSpinCount write SetSpinCount;
    class property DefaultSpinCount: integer read GetDefaultSpinCount write SetDefaultSpinCount;
  end;

  { TThreadedQueue }

  generic TLazThreadedQueue<T> = class
  private
    FMonitor: TLazMonitor;
    FList: array of T;
    FPushTimeout: Cardinal;
    FPopTimeout: Cardinal;
    FQueueSize: integer;
    FTotalItemsPopped: QWord;
    FTotalItemsPushed: QWord;
    FHasRoomEvent: PRTLEvent;
    FHasItemEvent: PRTLEvent;
    FShutDown: boolean;
    function TryPushItem(const AItem: T): boolean;
    function TryPopItem(out AItem: T): boolean;
  public
    constructor create(AQueueDepth: Integer = 10; PushTimeout: cardinal = INFINITE; PopTimeout: cardinal = INFINITE);
    destructor Destroy; override;
    procedure Grow(ADelta: integer);
    function PushItem(const AItem: T): TWaitResult;
    function PopItem(out AItem: T): TWaitResult;
    function PopItemTimeout(out AItem: T; Timeout: cardinal): TWaitResult;
    procedure DoShutDown;
    property QueueSize: integer read FQueueSize;
    property TotalItemsPopped: QWord read FTotalItemsPopped;
    property TotalItemsPushed: QWord read FTotalItemsPushed;
    property ShutDown: boolean read FShutDown;
  end;


implementation

{ TLazMonitor }

function TLazMonitor.GetSpinCount: integer;
begin
  result := FSpinCount;
end;

procedure TLazMonitor.SetSpinCount(AValue: integer);
begin
  InterLockedExchange(FSpinCount, AValue);
end;

class function TLazMonitor.GetDefaultSpinCount: integer; static;
begin
  result := FDefaultSpinCount;
end;

class procedure TLazMonitor.SetDefaultSpinCount(AValue: integer); static;
begin
  InterLockedExchange(FDefaultSpinCount, AValue);
end;

constructor TLazMonitor.create;
begin
  FSpinCount:=FDefaultSpinCount;
  inherited;
end;

procedure TLazMonitor.Acquire;
const
  YieldTreshold = 10;
  Sleep1Treshold = 20;
  Sleep0Treshold = 5;
var
  i,j: integer;
  Waitcount: integer;
  ASpinCount: integer;
  Sp: integer;
begin
  ASpinCount:=FSpinCount;
  for Sp := 0 to ASpinCount-1 do
  begin
    Waitcount:=1;
    for i := 0 to YieldTreshold-1 do
      begin
      if TryEnter then
        Exit;
      {$PUSH}
      {$OPTIMIZATION OFF}
      for j := 0 to Waitcount-1 do
        begin
        end;
      {$POP}
      Waitcount:=Waitcount*2;
      end;

    for i := 0 to Sleep1Treshold-1 do
      begin
      if TryEnter then
        Exit;
      sleep(1);
      end;

    for i := 0 to Sleep0Treshold do
      begin
      if TryEnter then
        Exit;
      sleep(0);
      end;
  end;

  inherited Acquire;
end;

{ TThreadedQueue }

function TLazThreadedQueue.TryPushItem(const AItem: T): boolean;
begin
  FMonitor.Enter;
  try
    result := FTotalItemsPushed-FTotalItemsPopped<FQueueSize;
    if result then
      begin
      FList[FTotalItemsPushed mod FQueueSize]:=AItem;
      inc(FTotalItemsPushed);
      RTLeventSetEvent(FHasItemEvent);
      end
    else
      RTLeventResetEvent(FHasRoomEvent);
  finally
    FMonitor.Leave;
  end;
end;

function TLazThreadedQueue.TryPopItem(out AItem: T): boolean;
begin
  FMonitor.Enter;
  try
    result := FTotalItemsPushed>FTotalItemsPopped;
    if result then
      begin
      AItem := FList[FTotalItemsPopped mod FQueueSize];
      inc(FTotalItemsPopped);
      RTLeventSetEvent(FHasRoomEvent);
      end
    else
      RTLeventResetEvent(FHasItemEvent);
  finally
    FMonitor.Leave;
  end;
end;

constructor TLazThreadedQueue.create(AQueueDepth: Integer; PushTimeout: cardinal; PopTimeout: cardinal);
begin
  FMonitor:=TLazMonitor.create;
  Grow(AQueueDepth);
  FHasRoomEvent:=RTLEventCreate;
  RTLeventSetEvent(FHasRoomEvent);
  FHasItemEvent:=RTLEventCreate;
  FPushTimeout:=PushTimeout;
  FPopTimeout:=PopTimeout;
end;

destructor TLazThreadedQueue.Destroy;
begin
  DoShutDown;
  RTLeventdestroy(FHasRoomEvent);
  RTLeventdestroy(FHasItemEvent);
  FMonitor.Free;
  inherited Destroy;
end;

procedure TLazThreadedQueue.Grow(ADelta: integer);
begin
  FMonitor.Enter;
  try
    FQueueSize:=FQueueSize+ADelta;
    setlength(FList, FQueueSize);
  finally
    FMonitor.Leave;
  end;
end;

function TLazThreadedQueue.PushItem(const AItem: T): TWaitResult;
var
  tc, ltc: int64;
begin
  if (FPushTimeout<>INFINITE) and (FPushTimeout<>0) then
    begin
    tc := GetTickCount64;
    ltc := 0;
    end;
  if TryPushItem(AItem) then
    result := wrSignaled
  else
    begin
    repeat
    if FPushTimeout=0 then
      begin
      result := wrTimeout;
      Exit;
      end
    else if FPushTimeout=INFINITE then
      RTLeventWaitFor(FHasRoomEvent)
    else
      begin
      RTLeventWaitFor(FHasRoomEvent, FPushTimeout - ltc);
      ltc := GetTickCount64-tc;
      if ltc > FPushTimeout then
        begin
        result := wrTimeout;
        Exit;
        end;
      end;
    if FShutDown then
      begin
      result := wrAbandoned;
      exit;
      end;
    until TryPushItem(AItem);
    result := wrSignaled;
    end;
end;

function TLazThreadedQueue.PopItem(out AItem: T): TWaitResult;
begin
  result := PopItemTimeout(AItem, FPopTimeout);
end;

function TLazThreadedQueue.PopItemTimeout(out AItem: T; Timeout: cardinal): TWaitResult;
var
  tc, ltc: int64;
begin
  if (Timeout<>INFINITE) and (Timeout<>0) then
    begin
    tc := GetTickCount64;
    ltc := 0;
    end;
  if TryPopItem(AItem) then
    result := wrSignaled
  else
    begin
    if Timeout=0 then
      begin
      result := wrTimeout;
      Exit;
      end;
    repeat
    if Timeout=INFINITE then
      RTLeventWaitFor(FHasItemEvent)
    else
      begin
      RTLeventWaitFor(FHasItemEvent, Timeout - ltc);
      ltc := GetTickCount64-tc;
      if ltc > Timeout then
        begin
        result := wrTimeout;
        Exit;
        end;
      end;
    if FShutDown then
      begin
      result := wrAbandoned;
      exit;
      end;
    until TryPopItem(AItem);
    result := wrSignaled;
    end;
end;

procedure TLazThreadedQueue.DoShutDown;
begin
  FShutDown:=true;
  RTLeventSetEvent(FHasRoomEvent);
  RTLeventResetEvent(FHasItemEvent);
end;

initialization
  TLazMonitor.DefaultSpinCount:=3;
end.


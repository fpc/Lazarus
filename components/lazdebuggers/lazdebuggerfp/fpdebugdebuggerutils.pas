{
 ---------------------------------------------------------------------------
 FpDebugDebuggerUtils
 ---------------------------------------------------------------------------

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

unit FpDebugDebuggerUtils;

{$mode objfpc}{$H+}

interface

uses
  FpDbgUtil, LazLoggerBase, sysutils, Classes, syncobjs;

type

  TFpThreadWorkerPriority = (
    twpUser,
    twpThread, twpStack, twpLocal, twpWatch,
    twpContinue
  );

const
  twpInspect = twpWatch;
  twpDefault = twpUser;
type

  { TFpThreadPriorityWorkerItem }

  TFpThreadPriorityWorkerItem = class(TFpThreadWorkerItem)
  private
    FPriority: TFpThreadWorkerPriority;
  public
    constructor Create(APriority: TFpThreadWorkerPriority);
    function DebugText: String; override;
    property Priority: TFpThreadWorkerPriority read FPriority;
  end;

  { TFpThreadPriorityWorkerQueue }

  TFpThreadPriorityWorkerQueue = class(TFpThreadWorkerQueue)
  private
    function GetOnQueueIdle: TThreadMethod;
    procedure SetOnQueueIdle(AValue: TThreadMethod);
  protected type
    TFpDbgTypedFifoQueue2 = TFpDbgTypedFifoQueue;
    TFpDbgPriorytyFifoQueue = class(TFpDbgTypedFifoQueue2)
    private
      FOnQueueIdle: TThreadMethod;
      FQueuedThread: TThread;
      FQueues: array[TFpThreadWorkerPriority] of TFpDbgTypedFifoQueue2;
      FLowestAvail: TFpThreadWorkerPriority;
    public
      constructor create(AQueueDepth: Integer = 10);
      destructor Destroy; override;
      function PushItem(const AItem: TFpThreadWorkerItem): Boolean; override;
      function PopItem(out AItem: TFpThreadWorkerItem): Boolean; override;
    end;
  protected
    function CreateFifoQueue(AQueueDepth: Integer): TLazTypedFifoQueue; override;
  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: cardinal = INFINITE; PopTimeout: cardinal = INFINITE);
    procedure Lock; inline;
    procedure Unlock; inline;
    function Count: Integer;
    property OnQueueIdle: TThreadMethod read GetOnQueueIdle write SetOnQueueIdle;
  end;


implementation

var
  FPDBG_QUEUE: PLazLoggerLogGroup;

{ TFpThreadPriorityWorkerItem }

constructor TFpThreadPriorityWorkerItem.Create(
  APriority: TFpThreadWorkerPriority);
begin
  FPriority := APriority;
end;

function TFpThreadPriorityWorkerItem.DebugText: String;
begin
  WriteStr(Result, FPriority);
  Result := inherited DebugText + '[' + Result + ':' + IntToStr(ord(FPriority)) + ']';
end;

{ TFpThreadPriorityWorkerQueue.TFpDbgPriorytyFifoQueue }

constructor TFpThreadPriorityWorkerQueue.TFpDbgPriorytyFifoQueue.create(
  AQueueDepth: Integer);
var
  a: TFpThreadWorkerPriority;
begin
  inherited create(0);
  for a in TFpThreadWorkerPriority do
    FQueues[a] := TFpDbgTypedFifoQueue2.create(AQueueDepth);
end;

destructor TFpThreadPriorityWorkerQueue.TFpDbgPriorytyFifoQueue.Destroy;
var
  a: TFpThreadWorkerPriority;
begin
  TThread.RemoveQueuedEvents(FQueuedThread, FOnQueueIdle);
  inherited Destroy;
  for a in TFpThreadWorkerPriority do
    FQueues[a].Free;
end;

function TFpThreadPriorityWorkerQueue.TFpDbgPriorytyFifoQueue.PushItem(
  const AItem: TFpThreadWorkerItem): Boolean;
begin
  TThread.RemoveQueuedEvents(FQueuedThread, FOnQueueIdle);
  inc(FTotalItemsPushed);
  if not (AItem is TFpThreadPriorityWorkerItem) then begin
    Result := FQueues[twpDefault].PushItem(AItem);
    if twpDefault < FLowestAvail then
      FLowestAvail := twpDefault;
  end
  else begin
    Result := FQueues[TFpThreadPriorityWorkerItem(AItem).FPriority].PushItem(AItem);
    if TFpThreadPriorityWorkerItem(AItem).FPriority < FLowestAvail then
      FLowestAvail := TFpThreadPriorityWorkerItem(AItem).FPriority;
  end;
end;

function TFpThreadPriorityWorkerQueue.TFpDbgPriorytyFifoQueue.PopItem(out
  AItem: TFpThreadWorkerItem): Boolean;
begin
  Result := FQueues[FLowestAvail].PopItem(AItem);
  while (not Result) and (FLowestAvail < high(FLowestAvail)) do begin
    inc(FLowestAvail);
    Result := FQueues[FLowestAvail].PopItem(AItem);
  end;
  if Result then begin
    inc(FTotalItemsPopped)
  end
  else begin
    // IDLE => there is only one worker thread, so no other items are running
    FQueuedThread := TThread.CurrentThread;
    TThread.Queue(FQueuedThread, FOnQueueIdle);
  end;
  assert(result or (TotalItemsPushed=TotalItemsPopped), 'TFpThreadPriorityWorkerQueue.TFpDbgPriorytyFifoQueue.PopItem: result or (TotalItemsPushed=TotalItemsPopped)');
end;

{ TFpThreadPriorityWorkerQueue }

function TFpThreadPriorityWorkerQueue.GetOnQueueIdle: TThreadMethod;
begin
  Result := TFpDbgPriorytyFifoQueue(FifoQueue).FOnQueueIdle;
end;

procedure TFpThreadPriorityWorkerQueue.SetOnQueueIdle(AValue: TThreadMethod);
begin
  TFpDbgPriorytyFifoQueue(FifoQueue).FOnQueueIdle := AValue;
end;

function TFpThreadPriorityWorkerQueue.CreateFifoQueue(AQueueDepth: Integer
  ): TLazTypedFifoQueue;
begin
  Result := TFpDbgPriorytyFifoQueue.Create(AQueueDepth);
end;

constructor TFpThreadPriorityWorkerQueue.Create(AQueueDepth: Integer;
  PushTimeout: cardinal; PopTimeout: cardinal);
begin
  inherited Create(AQueueDepth, PushTimeout, PopTimeout);
  FLogGroup := FPDBG_QUEUE;
end;

procedure TFpThreadPriorityWorkerQueue.Lock;
begin
  inherited Lock;
end;

procedure TFpThreadPriorityWorkerQueue.Unlock;
begin
  inherited Unlock;
end;

function TFpThreadPriorityWorkerQueue.Count: Integer;
begin
  Result := TotalItemsPushed - TotalItemsPopped;
end;

initialization
  FPDBG_QUEUE := DebugLogger.FindOrRegisterLogGroup('FPDBG_QUEUE' {$IFDEF FPDBG_QUEUE} , True {$ENDIF} );
end.


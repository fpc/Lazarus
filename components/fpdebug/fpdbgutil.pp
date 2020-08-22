{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgutil.pp  -  Native freepascal debugger - Utilities
 ---------------------------------------------------------------------------

 This unit contains utility functions

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

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
unit FpDbgUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, math, LazUTF8, lazCollections, LazClasses,
  UTF8Process, syncobjs;

type
  THexValueFormatFlag = (hvfSigned, hvfPrefixPositive, hvfIncludeHexchar);
  THexValueFormatFlags = set of THexValueFormatFlag;

  TFpThreadWorkerQueue = class;
  TFpWorkerThread = class;

  { TFpThreadWorkerItem }

  TFpThreadWorkerItem = class
  private const
    TWSTATE_NEW =     cardinal(0);
    TWSTATE_RUNNING = cardinal(1);
    TWSTATE_WAITING = cardinal(2);
    TWSTATE_DONE =    cardinal(3);
  private
    FDone: Cardinal;
    FRefCnt: LongInt;
  protected
    procedure DoExecute; virtual;
    procedure DoFinished; virtual;

    procedure Execute(MyWorkerThread: TFpWorkerThread); // called by worker thread
    procedure WaitFor(AnMainWaitEvent: PRTLEvent); // called by main thread => calls DoExecute, if needed
    procedure Cancel(AnMainWaitEvent: PRTLEvent); // called by main thread => calls DoExecute, if needed
  public
    procedure AddRef;
    procedure DecRef;
  end;

  { TFpWorkerThread }

  TFpWorkerThread = class(TThread)
  private
    FQueue: TFpThreadWorkerQueue;
  public
    constructor Create(AQueue: TFpThreadWorkerQueue);
    procedure Execute; override;
    property Queue: TFpThreadWorkerQueue read FQueue;
  end;

  { TFpThreadWorkerQueue }

  TFpThreadWorkerQueue = class(specialize TLazThreadedQueue<TFpThreadWorkerItem>)
  private type
    TFpWorkerThreadList = specialize TFPGObjectList<TFpWorkerThread>;
  strict private
    FWantedCount, FCurrentCount: Integer;
    FThreadMonitor: TLazMonitor;
    FWorkerThreadList: TFpWorkerThreadList;
    FMainWaitEvent: PRTLEvent;
    function GetCurrentCount: Integer;
    function GetThreadCount: integer;
    function GetWantedCount: Integer;
    procedure SetThreadCount(AValue: integer);
  protected
    function RemoveThread(Item: TFpWorkerThread): Integer;
    property WantedCount: Integer read GetWantedCount;
    property CurrentCount: Integer read GetCurrentCount;
    property ThreadMonitor: TLazMonitor read FThreadMonitor;
  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: cardinal = INFINITE; PopTimeout: cardinal = INFINITE);
    destructor Destroy; override; // Will not wait for the threads.

    procedure Clear; // Not thread safe // remove all none running items
    function PushItem(const AItem: TFpThreadWorkerItem): TWaitResult;

    procedure WaitForItem(const AItem: TFpThreadWorkerItem); // called by main thread => calls DoExecute, if needed
    procedure RemoveItem(const AItem: TFpThreadWorkerItem); // wait but do not execute

    property ThreadCount: integer read GetThreadCount write SetThreadCount; // Not thread safe
    property MainWaitEvent: PRTLEvent read FMainWaitEvent;
  end;

  { TFpGlobalThreadWorkerQueue }

  TFpGlobalThreadWorkerQueue = class(TFpThreadWorkerQueue)
  private
    FRefCnt: LongInt;
  public
    destructor Destroy; override;
    procedure AddRef;
    procedure DecRef;
  end;
  
function CompareUtf8BothCase(AnUpper, AnLower, AnUnknown: PChar): Boolean;

// Optimistic upper/lower case. Attempt Ansi only, but if none ansi is found, do utf8
function QuickUtf8UpperCase(const AText: String): String;
function QuickUtf8LowerCase(const AText: String): String;

function AlignPtr(Src: Pointer; Alignment: Byte): Pointer;
function HexValue(const AValue; ASize: Byte; AFlags: THexValueFormatFlags): String;
procedure Log(const AText: String; const AParams: array of const); overload;
procedure Log(const AText: String); overload;
function FormatAddress(const AAddress): String;

function GetFpDbgGlobalWorkerQueue: TFpGlobalThreadWorkerQueue;

property FpDbgGlobalWorkerQueue: TFpGlobalThreadWorkerQueue read GetFpDbgGlobalWorkerQueue;

implementation

uses
  LazLoggerBase,
  FpDbgClasses;

var
  TheFpDbgGlobalWorkerQueue: TFpGlobalThreadWorkerQueue = nil;

function GetFpDbgGlobalWorkerQueue: TFpGlobalThreadWorkerQueue;
begin
  if TheFpDbgGlobalWorkerQueue = nil then
    TheFpDbgGlobalWorkerQueue := TFpGlobalThreadWorkerQueue.Create(50);

  Result := TheFpDbgGlobalWorkerQueue;
end;

function CompareUtf8BothCase(AnUpper, AnLower, AnUnknown: PChar): Boolean;
var
  p: PChar;
begin
  Result := False;
  while (AnUpper^ <> #0) and (AnUnknown^ <> #0) do begin
    p := AnUnknown;

    if (AnUpper^ = AnUnknown^) then begin
      // maybe uppercase
      inc(AnUpper);
      inc(AnUnknown);
      while ((byte(AnUpper^) and $C0) = $C0) and (AnUpper^ = AnUnknown^) do begin
        inc(AnUpper);
        inc(AnUnknown);
      end;

      if ((byte(AnUpper^) and $C0) <> $C0) then begin // equal to upper
        inc(AnLower);
        while ((byte(AnLower^) and $C0) = $C0) do
          inc(AnLower);
        Continue;
      end;
    end
    else begin
      // skip the first byte / continuation bytes are skipped if lower matches
      inc(AnUpper);
      inc(AnUnknown);
    end;

    // Not upper, try lower
    if (AnLower^ = p^) then begin
      inc(AnLower);
      inc(p);
      while ((byte(AnLower^) and $C0) = $C0) and (AnLower^ = p^) do begin
        inc(AnLower);
        inc(p);
      end;

      if ((byte(AnLower^) and $C0) <> $C0) then begin // equal to lower
        // adjust upper and unknown to codepoint
        while ((byte(AnUpper^) and $C0) = $C0) do
          inc(AnUnknown);
        while ((byte(AnUnknown^) and $C0) = $C0) do
          inc(AnUnknown);
        Continue;
      end;
    end;

    Result := False;
    exit;
  end;

  Result := AnUpper^ = AnUnknown^;  // both #0
end;

function QuickUtf8UpperCase(const AText: String): String;
var
  src, dst: PChar;
  c: Integer;
  t: Char;
begin
  SetLength(Result, Length(AText));
  if Result = '' then
    exit;

  src := @AText[1];
  dst := @Result[1];
  c := Length(Result);
  while c > 0 do begin
    t := src^;
    if (ord(t) and 128) <>  0 then
      exit(UTF8UpperCase(AText));
    if (t in ['a'..'z']) then
      t := chr(ord(t) - 32);
    dst^ := t;
    dec(c);
    inc(src);
    inc(dst);
  end;
end;

function QuickUtf8LowerCase(const AText: String): String;
var
  src, dst: PChar;
  c: Integer;
  t: Char;
begin
  SetLength(Result, Length(AText));
  if Result = '' then
    exit;

  src := @AText[1];
  dst := @Result[1];
  c := Length(Result);
  while c > 0 do begin
    t := src^;
    if (ord(t) and 128) <>  0 then
      exit(UTF8UpperCase(AText));
    if (t in ['A'..'Z']) then
      t := chr(ord(t) + 32);
    dst^ := t;
    dec(c);
    inc(src);
    inc(dst);
  end;
end;

function AlignPtr(Src: Pointer; Alignment: Byte): Pointer;
begin
  Result := Pointer(((PtrUInt(Src) + Alignment - 1) and not PtrUInt(Alignment - 1)));
end;

function FormatAddress(const AAddress): String;
begin
  Result := HexValue(AAddress, DBGPTRSIZE[GMode], [hvfIncludeHexchar]);
end;

function HexValue(const AValue; ASize: Byte; AFlags: THexValueFormatFlags): String;
var
  i: Int64;
  p: PByte;
begin
  Result := '';
  if ASize > 8
  then begin
    Result := 'HexValue: size to large';
    Exit;
  end;
  if ASize = 0
  then begin
    Exit;
  end;

  p := @AValue;
  if p[ASize - 1] < $80
  then Exclude(AFlags, hvfSigned);

  if hvfSigned in AFlags
  then i := -1
  else i := 0;

  Move(AValue, i, ASize);
  if hvfSigned in AFlags
  then begin
    i := not i + 1;
    Result := '-';
  end
  else begin
    if hvfPrefixPositive in AFlags
    then Result := '+';
  end;
  if hvfIncludeHexchar in AFlags
  then Result := Result + '$';

  Result := Result + HexStr(i, ASize * 2);
end;

procedure Log(const AText: String; const AParams: array of const); overload;
begin
  DebugLn(Format(AText, AParams));
end;

procedure Log(const AText: String); overload;
begin
  DebugLn(AText);
end;

type

  { TFpThreadWorkerTerminateItem }

  TFpThreadWorkerTerminateItem = class(TFpThreadWorkerItem)
  end;

{ TFpGlobalThreadWorkerQueue }

destructor TFpGlobalThreadWorkerQueue.Destroy;
begin
  Assert(InterLockedExchangeAdd(FRefCnt, 0) = 0);
  inherited Destroy;
end;

procedure TFpGlobalThreadWorkerQueue.AddRef;
begin
  (* There are
     - The current/fpdebug-main thread
     - Maybe the IDE/main thread (if used LazDebuggerFp)
       (however, fpdebug and IDE will rarely run both at high load => they can be counted as one)
     - At least one, maybe more threads in the debugged target app
     So no more than half the cpu-core count will be allocated for workers
  *)
  if InterLockedIncrement(FRefCnt) = 1 then
    ThreadCount := Min(Max(1, GetSystemThreadCount div 2), 10);
end;

procedure TFpGlobalThreadWorkerQueue.DecRef;
begin
  if InterLockedDecrement(FRefCnt) = 0 then
    ThreadCount := 0;
end;

{ TFpThreadWorkerItem }

procedure TFpThreadWorkerItem.DoExecute;
begin
  //
end;

procedure TFpThreadWorkerItem.DoFinished;
begin
  if InterLockedExchangeAdd(FRefCnt, 0) <= 0 then
    Destroy;
end;

procedure TFpThreadWorkerItem.Execute(MyWorkerThread: TFpWorkerThread);
var
  st: Cardinal;
begin
  st := InterLockedExchange(FDone, TWSTATE_RUNNING);
  if st = TWSTATE_NEW then begin
    DoExecute;

    st := InterLockedExchange(FDone, TWSTATE_DONE);
    if st = TWSTATE_WAITING then
      RTLeventSetEvent(MyWorkerThread.Queue.MainWaitEvent);
  end;
end;

procedure TFpThreadWorkerItem.WaitFor(AnMainWaitEvent: PRTLEvent);
var
  st: Cardinal;
begin
  st := InterLockedExchange(FDone, TWSTATE_WAITING);
  if st = TWSTATE_NEW then begin
    DoExecute;
  end
  else
  if st = TWSTATE_RUNNING then begin
    RTLeventWaitFor(AnMainWaitEvent);
    RTLeventResetEvent(AnMainWaitEvent);
  end;
end;

procedure TFpThreadWorkerItem.Cancel(AnMainWaitEvent: PRTLEvent);
var
  st: Cardinal;
begin
  st := InterLockedExchange(FDone, TWSTATE_WAITING); // Prevent thread form executing this
  if st = TWSTATE_RUNNING then begin
    RTLeventWaitFor(AnMainWaitEvent);
    RTLeventResetEvent(AnMainWaitEvent);
  end;
end;

procedure TFpThreadWorkerItem.AddRef;
begin
  InterLockedIncrement(FRefCnt);
end;

procedure TFpThreadWorkerItem.DecRef;
begin
  if Self = nil then
    exit;
  if InterLockedDecrement(FRefCnt) <= 0 then
    DoFinished;
end;

{ TFpWorkerThread }

constructor TFpWorkerThread.Create(AQueue: TFpThreadWorkerQueue);
begin
  FQueue := AQueue;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TFpWorkerThread.Execute;
var
  WorkItem: TFpThreadWorkerItem;
begin
  while not Terminated do begin
    FQueue.PopItem(WorkItem);
    if WorkItem = nil then
      Continue;

    if WorkItem is TFpThreadWorkerTerminateItem then begin
      WorkItem.DecRef;
      if Terminated then // no need to check
        break;

      if FQueue.CurrentCount > FQueue.WantedCount then begin
        FQueue.ThreadMonitor.Enter;
        try
          if FQueue.ThreadCount > FQueue.WantedCount then
            break;
        finally
          FQueue.ThreadMonitor.Leave;
        end;
      end;
      Continue;
    end;

    WorkItem.Execute(Self);
    WorkItem.DecRef;
  end;
  FQueue.RemoveThread(Self);
end;

{ TFpThreadWorkerQueue }

function TFpThreadWorkerQueue.GetThreadCount: integer;
begin
  FThreadMonitor.Enter;
  try
    Result := FWorkerThreadList.Count;
  finally
    FThreadMonitor.Leave;
  end;
end;

function TFpThreadWorkerQueue.GetCurrentCount: Integer;
begin
  Result := InterLockedExchangeAdd(FCurrentCount, 0);
end;

function TFpThreadWorkerQueue.GetWantedCount: Integer;
begin
  Result := InterLockedExchangeAdd(FWantedCount, 0);
end;

procedure TFpThreadWorkerQueue.SetThreadCount(AValue: integer);
var
  c: Integer;
begin
  FThreadMonitor.Enter;
  try
    InterLockedExchange(FWantedCount, AValue);
    FWantedCount := AValue;

    c := FWorkerThreadList.Count;
    if c > AValue then begin
      while c > AValue do begin
        dec(c);
        PushItem(TFpThreadWorkerTerminateItem.Create); // will terminate one thread, if no more work is to be done
      end;
      InterLockedExchange(FCurrentCount, FWorkerThreadList.Count);
    end

    else
    begin
      // increase
      FWorkerThreadList.Count := AValue;
      InterLockedExchange(FCurrentCount, AValue);
      while c < AValue do begin
        FWorkerThreadList[c] := TFpWorkerThread.Create(Self);
        inc(c);
      end;
    end;
  finally
    FThreadMonitor.Leave;
  end;
end;

function TFpThreadWorkerQueue.RemoveThread(Item: TFpWorkerThread): Integer;
begin
  FThreadMonitor.Enter;
  try
    FWorkerThreadList.Remove(Item);
    InterLockedExchange(FCurrentCount, FWorkerThreadList.Count);
  finally
    FThreadMonitor.Leave;
  end;
end;

constructor TFpThreadWorkerQueue.Create(AQueueDepth: Integer;
  PushTimeout: cardinal; PopTimeout: cardinal);
begin
  FThreadMonitor:=TLazMonitor.create;
  inherited create(AQueueDepth, PushTimeout, PopTimeout);
  FMainWaitEvent := RTLEventCreate;
  FWorkerThreadList := TFpWorkerThreadList.Create(False);
end;

destructor TFpThreadWorkerQueue.Destroy;
var
  WorkItem: TFpThreadWorkerItem;
  i: Integer;
begin
  FThreadMonitor.Enter;
  try
    for i := 0 to FWorkerThreadList.Count - 1 do
      FWorkerThreadList[i].Terminate; // also signals that the queue is no longer valid
  finally
    FThreadMonitor.Leave;
  end;

  Clear;
  ThreadCount := 0;

  // Wait for threads.
  while CurrentCount > 0 do begin
    sleep(1);
    if TotalItemsPushed = TotalItemsPopped then
      ThreadCount := 0; // Add more TFpThreadWorkerTerminateItem
  end;

  // Free any TFpThreadWorkerTerminateItem items that were not picked up
  Clear;

  inherited Destroy;
  FWorkerThreadList.Free;
  RTLeventdestroy(FMainWaitEvent);
  FThreadMonitor.Free;
end;

procedure TFpThreadWorkerQueue.Clear;
var
  WorkItem: TFpThreadWorkerItem;
begin
  while PopItemTimeout(WorkItem, 1) = wrSignaled do
    WorkItem.DecRef;
end;

function TFpThreadWorkerQueue.PushItem(const AItem: TFpThreadWorkerItem
  ): TWaitResult;
begin
  if TotalItemsPopped = TotalItemsPushed then
    Grow(Min(QueueSize, 100));
  AItem.AddRef;
  Result := inherited PushItem(AItem);
end;

procedure TFpThreadWorkerQueue.RemoveItem(const AItem: TFpThreadWorkerItem);
begin
  if AItem <> nil then
    AItem.Cancel(Self.MainWaitEvent);
end;

procedure TFpThreadWorkerQueue.WaitForItem(const AItem: TFpThreadWorkerItem);
begin
  AItem.WaitFor(Self.MainWaitEvent);
end;

finalization
  TheFpDbgGlobalWorkerQueue.Free;
end.


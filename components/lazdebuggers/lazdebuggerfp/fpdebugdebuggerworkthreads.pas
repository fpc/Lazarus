{
 ---------------------------------------------------------------------------
 FpDebugDebuggerWorkThreads
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

unit FpDebugDebuggerWorkThreads;

(*
  This unit contains the classes for executing work in the worker thread:
  - The general structure of the classes
  - The code that is to be executed in the worker thread
      procedure DoExecute;

  - The classes are extended in the main FpDebugDebugger unit with any code
    running in the main debugger thread.

  This split accross the units should help with identifying what may be accessed
  in the worker thread.
*)

{$mode objfpc}{$H+}
{$IF FPC_Fullversion=30202}{$Optimization NOPEEPHOLE}{$ENDIF}
{$TYPEDADDRESS on}
{$ModeSwitch advancedrecords}

interface

uses
  FpDebugDebuggerUtils, FpDebugValueConvertors, DbgIntfDebuggerBase,
  DbgIntfBaseTypes, FpDbgClasses, FpDbgUtil, FPDbgController, FpPascalBuilder,
  FpdMemoryTools, FpDbgInfo, FpPascalParser, FpErrorMessages,
  FpDebugDebuggerBase, FpDebuggerResultData, FpDbgCallContextInfo, FpDbgDwarf,
  FpDbgDwarfDataClasses, FpWatchResultData, LazDebuggerIntf,
  LazDebuggerValueConverter, Forms, fgl, math, Classes, sysutils, LazClasses,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif};

type

  TFpDbgAsyncMethod = procedure() of object;

  { TFpDbgDebggerThreadWorkerItem }

  TFpDbgDebggerThreadWorkerItem = class(TFpThreadPriorityWorkerItem)
  protected type
    THasQueued = (hqNotQueued, hqQueued, hqBlocked);
  protected
    FDebugger: TFpDebugDebuggerBase;
    FHasQueued: THasQueued;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; APriority: TFpThreadWorkerPriority);

    procedure Queue(aMethod: TDataEvent; Data: PtrInt = 0);
    (* Unqueue_DecRef also prevents new queuing
       Unqueue_DecRef allows for destruction (no more access to object)
       => therefor UnQueue_DecRef and ALL/most methods executing  unqueue_DecRef are named *_DecRef
    *)
    procedure UnQueue_DecRef(ABlockQueuing: Boolean = True);
  end;

  { TFpDbgDebggerThreadWorkerLinkedItem }

  TFpDbgDebggerThreadWorkerLinkedItem = class(TFpDbgDebggerThreadWorkerItem)
  protected
    FNextWorker: TFpDbgDebggerThreadWorkerLinkedItem; // linked list for use by TFPCallStackSupplier
    procedure DoRemovedFromLinkedList; virtual;
  end;

  { TFpDbgDebggerThreadWorkerLinkedList }

  TFpDbgDebggerThreadWorkerLinkedList = object
  private
    FNextWorker: TFpDbgDebggerThreadWorkerLinkedItem;
    FLocked: Boolean;
  public
    procedure Add(AWorkItem: TFpDbgDebggerThreadWorkerLinkedItem); // Does not add ref / uses existing ref
    procedure ClearFinishedWorkers;
    procedure RequestStopForWorkers;
    procedure WaitForWorkers(AStop: Boolean); // Only call in IDE thread (main thread)
  end;

  { TFpThreadWorkerControllerRun }

  TFpThreadWorkerControllerRun = class(TFpDbgDebggerThreadWorkerItem)
  private
    FWorkerThreadId: TThreadID;
  protected
    FStartSuccessfull: boolean;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase);
    property StartSuccesfull: boolean read FStartSuccessfull;
    property WorkerThreadId: TThreadID read FWorkerThreadId;
  end;

  { TFpThreadWorkerRunLoop }

  TFpThreadWorkerRunLoop = class(TFpDbgDebggerThreadWorkerItem)
  protected
    procedure LoopFinished_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase);
  end;

  { TFpThreadWorkerRunLoopAfterIdle }

  TFpThreadWorkerRunLoopAfterIdle = class(TFpDbgDebggerThreadWorkerItem)
  protected
    procedure CheckIdleOrRun_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase);
  end;

  { TFpThreadWorkerAsyncMeth }

  TFpThreadWorkerAsyncMeth = class(TFpDbgDebggerThreadWorkerItem)
  protected
    FAsyncMethod: TFpDbgAsyncMethod;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; AnAsyncMethod: TFpDbgAsyncMethod);
  end;

  { TFpThreadWorkerPrepareCallStackEntryList }

  TFpThreadWorkerPrepareCallStackEntryList = class(TFpDbgDebggerThreadWorkerLinkedItem)
  (* Do not accesss   CallStackEntryList.Items[]   while this is running *)
  protected
    FRequiredMinCount: Integer;
    FThread: TDbgThread;
    procedure PrepareCallStackEntryList(AFrameRequired: Integer; AThread: TDbgThread);
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; ARequiredMinCount: Integer; APriority: TFpThreadWorkerPriority = twpStack);
    constructor Create(ADebugger: TFpDebugDebuggerBase; ARequiredMinCount: Integer; AThread: TDbgThread);
  end;

  { TFpThreadWorkerCallStackCount }

  TFpThreadWorkerCallStackCount = class(TFpThreadWorkerPrepareCallStackEntryList)
  protected
    procedure UpdateCallstack_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  end;

  { TFpThreadWorkerCallEntry }

  TFpThreadWorkerCallEntry = class(TFpThreadWorkerPrepareCallStackEntryList)
  protected
    FCallstackIndex: Integer;
    FValid: Boolean;
    FSrcClassName, FFunctionName, FSourceFile: String;
    FAnAddress: TDBGPtr;
    FLine: Integer;
    FParamAsString: String;
    procedure UpdateCallstackEntry_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  end;

  { TFpThreadWorkerThreads }

  TFpThreadWorkerThreads = class(TFpThreadWorkerPrepareCallStackEntryList)
  protected
    procedure UpdateThreads_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase);
  end;

  { TFpThreadWorkerLocals }

  TFpThreadWorkerLocals = class(TFpDbgDebggerThreadWorkerLinkedItem)
  protected type
  protected
    FLocals: IDbgLocalsListIntf;
    FThreadId, FStackFrame: Integer;
    procedure UpdateLocals_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  end;

  { TFpThreadWorkerModify }

  TFpThreadWorkerModify = class(TFpDbgDebggerThreadWorkerLinkedItem)
  private
    FExpression, FNewVal: String;
    FStackFrame, FThreadId: Integer;
    FSuccess: Boolean;
  protected
    procedure DoCallback_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
    property Success: Boolean read FSuccess;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase;
                       //APriority: TFpThreadWorkerPriority;
                       const AnExpression, ANewValue: String;
                       AStackFrame, AThreadId: Integer
                      );
    function DebugText: String; override;
  end;

  { TFpThreadWorkerEvaluate }

  TFpThreadWorkerEvaluate = class(TFpDbgDebggerThreadWorkerLinkedItem)
  private
    FAllowFunctions, FAllowFunctionsAllThread: Boolean;
    FExpressionScope: TFpDbgSymbolScope;

    function DoWatchFunctionCall(AnExpressionPart: TFpPascalExpressionPart;
      AFunctionValue, ASelfValue: TFpValue; AParams: TFpPascalExpressionPartList;
      out AResult: TFpValue; var AnError: TFpError): boolean;
  protected
    FWatchValue: IDbgWatchValueIntf;
    function EvaluateExpression(const AnExpression: String;
                                AStackFrame, AThreadId: Integer;
                                ADispFormat: TWatchDisplayFormat;
                                ARepeatCnt: Integer;
                                AnEvalFlags: TWatcheEvaluateFlags;
                                out AResText: String;
                                out ATypeInfo: TDBGType
                               ): Boolean;
  public
  end;

  { TFpThreadWorkerEvaluateExpr }

  TFpThreadWorkerEvaluateExpr = class(TFpThreadWorkerEvaluate)
  private
    FExpression: String;
    FStackFrame, FThreadId: Integer;
    FDispFormat: TWatchDisplayFormat;
    FRepeatCnt: Integer;
    FEvalFlags: TWatcheEvaluateFlags;
  protected
    FRes: Boolean;
    FResText: String;
    FResDbgType: TDBGType;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase;
                       APriority: TFpThreadWorkerPriority;
                       const AnExpression: String;
                       AStackFrame, AThreadId: Integer;
                       ADispFormat: TWatchDisplayFormat;
                       ARepeatCnt: Integer;
                       AnEvalFlags: TWatcheEvaluateFlags
                      );
    function DebugText: String; override;
  end;

  { TFpThreadWorkerCmdEval }

  TFpThreadWorkerCmdEval = class(TFpThreadWorkerEvaluateExpr)
  protected
    FCallback: TDBGEvaluateResultCallback;
    procedure DoCallback_DecRef(Data: PtrInt = 0);
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase;
                       APriority: TFpThreadWorkerPriority;
                       const AnExpression: String;
                       AStackFrame, AThreadId: Integer;
                       AnEvalFlags: TWatcheEvaluateFlags;
                       ACallback: TDBGEvaluateResultCallback
                      );
    destructor Destroy; override;
    procedure Abort;
  end;

  { TFpThreadWorkerWatchValueEval }

  TFpThreadWorkerWatchValueEval = class(TFpThreadWorkerEvaluateExpr)
  protected
    procedure UpdateWatch_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  end;

  { TFpThreadWorkerBreakPoint }

  TFpThreadWorkerBreakPoint = class(TFpDbgDebggerThreadWorkerItem)
  public
    procedure RemoveBreakPoint_DecRef; virtual;
    procedure AbortSetBreak; virtual;
  end;

  { TFpThreadWorkerBreakPointSet }

  TFpThreadWorkerBreakPointSet = class(TFpThreadWorkerBreakPoint)
  private
    FInternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint;
    FKind: TDBGBreakPointKind;
    FAddress: TDBGPtr;
    FSource: String;
    FLine: Integer;
    FStackFrame, FThreadId: Integer;
    FWatchData: String;
    FWatchScope: TDBGWatchPointScope;
    FWatchKind: TDBGWatchPointKind;
  protected
    FResetBreakPoint: Boolean;
    procedure UpdateBrkPoint_DecRef(Data: PtrInt = 0); virtual; abstract;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; AnAddress: TDBGPtr);
    constructor Create(ADebugger: TFpDebugDebuggerBase; ASource: String; ALine: Integer);
    constructor Create(ADebugger: TFpDebugDebuggerBase;
      AWatchData: String; AWatchScope: TDBGWatchPointScope; AWatchKind: TDBGWatchPointKind;
      AStackFrame, AThreadId: Integer);
    property InternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint read FInternalBreakpoint;
  end;

  { TFpThreadWorkerBreakPointRemove }

  TFpThreadWorkerBreakPointRemove = class(TFpThreadWorkerBreakPoint)
  protected
    FInternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint;
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; AnInternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint);
    property InternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint read FInternalBreakpoint;
  end;

implementation
var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_FUNCCALL: PLazLoggerLogGroup;

{ TFpDbgDebggerThreadWorkerItem }

constructor TFpDbgDebggerThreadWorkerItem.Create(ADebugger: TFpDebugDebuggerBase;
  APriority: TFpThreadWorkerPriority);
begin
  inherited Create(APriority);
  FDebugger := ADebugger;
  AddRef;
end;

procedure TFpDbgDebggerThreadWorkerItem.Queue(aMethod: TDataEvent; Data: PtrInt
  );
begin
  FDebugger.LockList.Lock;
  try
    if (FHasQueued <> hqBlocked) then begin
      assert(FHasQueued = hqNotQueued, 'TFpDbgDebggerThreadWorkerItem.Queue: FHasQueued = hqNotQueued');
      FHasQueued := hqQueued;
      AddRef;
      Application.QueueAsyncCall(aMethod, 0);
    end;
  finally
    FDebugger.LockList.UnLock;
  end;
end;

procedure TFpDbgDebggerThreadWorkerItem.UnQueue_DecRef(ABlockQueuing: Boolean);
var
  HasQ: THasQueued;
begin
  FDebugger.LockList.Lock;
  HasQ := FHasQueued;
  if ABlockQueuing then begin
    FHasQueued := hqBlocked;
    FDebugger.LockList.UnLock; // unlock first.
    Application.RemoveAsyncCalls(Self);
  end
  else begin
    FHasQueued := hqNotQueued;
    try
      Application.RemoveAsyncCalls(Self);
    finally
      FDebugger.LockList.UnLock;
    end;
  end;

  if HasQ = hqQueued then
    DecRef; // may call destroy
end;

{ TFpDbgDebggerThreadWorkerLinkedItem }

procedure TFpDbgDebggerThreadWorkerLinkedItem.DoRemovedFromLinkedList;
begin
  UnQueue_DecRef;
end;

{ TFpDbgDebggerThreadWorkerLinkedList }

procedure TFpDbgDebggerThreadWorkerLinkedList.Add(
  AWorkItem: TFpDbgDebggerThreadWorkerLinkedItem);
begin
  AWorkItem.FNextWorker := FNextWorker;
  FNextWorker := AWorkItem;
end;

procedure TFpDbgDebggerThreadWorkerLinkedList.ClearFinishedWorkers;
var
  WorkItem, w: TFpDbgDebggerThreadWorkerLinkedItem;
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpDbgDebggerThreadWorkerLinkedList.ClearFinishedCountWorkers: system.ThreadID = classes.MainThreadID');
  if FLocked then
    exit;
  FLocked := True;
  WorkItem := FNextWorker;
  try
    while (WorkItem <> nil) and (WorkItem.RefCount = 1) do begin
      w := WorkItem;
      WorkItem := w.FNextWorker;
      w.DoRemovedFromLinkedList;
      w.DecRef;
    end;
  finally
    FNextWorker := WorkItem;
    FLocked := False;
  end;
end;

procedure TFpDbgDebggerThreadWorkerLinkedList.RequestStopForWorkers;
var
  WorkItem: TFpDbgDebggerThreadWorkerLinkedItem;
begin
  WorkItem := FNextWorker;
  while (WorkItem <> nil) do begin
    WorkItem.RequestStop;
    WorkItem := WorkItem.FNextWorker;
  end;
end;

procedure TFpDbgDebggerThreadWorkerLinkedList.WaitForWorkers(AStop: Boolean);
var
  WorkItem, w: TFpDbgDebggerThreadWorkerLinkedItem;
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpDbgDebggerThreadWorkerLinkedList.WaitForWorkers: system.ThreadID = classes.MainThreadID');
  assert(not FLocked, 'TFpDbgDebggerThreadWorkerLinkedList.WaitForWorkers: not FLocked');
  if AStop then
    RequestStopForWorkers;

  FLocked := True;
  WorkItem := FNextWorker;
  FNextWorker := nil;
  try
    while (WorkItem <> nil) do begin
      w := WorkItem;
      WorkItem := w.FNextWorker;
      if w.IsCancelled then
        w.FDebugger.WorkQueue.RemoveItem(w)
      else
        w.FDebugger.WorkQueue.WaitForItem(w);
      w.DoRemovedFromLinkedList;
      w.DecRef;
    end;
  finally
    FLocked := False;
  end;
end;

{ TFpThreadWorkerControllerRun }

procedure TFpThreadWorkerControllerRun.DoExecute;
begin
  FStartSuccessfull := FDebugger.DbgController.Run;
  FWorkerThreadId := ThreadID;
end;

constructor TFpThreadWorkerControllerRun.Create(ADebugger: TFpDebugDebuggerBase);
begin
  inherited Create(ADebugger, twpContinue);
end;

{ TFpThreadWorkerRunLoop }

procedure TFpThreadWorkerRunLoop.DoExecute;
begin
  FDebugger.ClearCachedData;
  FDebugger.DbgController.ProcessLoop;
  Queue(@LoopFinished_DecRef);
end;

constructor TFpThreadWorkerRunLoop.Create(ADebugger: TFpDebugDebuggerBase);
begin
  inherited Create(ADebugger, twpContinue);
end;

{ TFpThreadWorkerRunLoopAfterIdle }

procedure TFpThreadWorkerRunLoopAfterIdle.DoExecute;
begin
  Queue(@CheckIdleOrRun_DecRef);
end;

constructor TFpThreadWorkerRunLoopAfterIdle.Create(ADebugger: TFpDebugDebuggerBase);
begin
  inherited Create(ADebugger, twpContinue);
end;

{ TFpThreadWorkerAsyncMeth }

procedure TFpThreadWorkerAsyncMeth.DoExecute;
begin
  FAsyncMethod();
end;

constructor TFpThreadWorkerAsyncMeth.Create(ADebugger: TFpDebugDebuggerBase;
  AnAsyncMethod: TFpDbgAsyncMethod);
begin
  inherited Create(ADebugger, twpUser);
  FAsyncMethod := AnAsyncMethod;
end;

{ TFpThreadWorkerPrepareCallStackEntryList }

procedure TFpThreadWorkerPrepareCallStackEntryList.PrepareCallStackEntryList(
  AFrameRequired: Integer; AThread: TDbgThread);
var
  ThreadCallStack: TDbgCallstackEntryList;
  CurCnt, ReqCnt: Integer;
begin
  ThreadCallStack := AThread.CallStackEntryList;

  if ThreadCallStack = nil then begin
    AThread.PrepareCallStackEntryList(-2); // Only create the list
    ThreadCallStack := AThread.CallStackEntryList;
    if ThreadCallStack = nil then
      exit;
  end;

  FDebugger.LockList.GetLockFor(ThreadCallStack);
  try
    CurCnt := ThreadCallStack.Count;
    while (not StopRequested) and
          ( (FRequiredMinCount > CurCnt) or (FRequiredMinCount < 0) ) and
          (not ThreadCallStack.HasReadAllAvailableFrames)
    do begin
      ReqCnt := Min(CurCnt + 5, FRequiredMinCount);
      AThread.PrepareCallStackEntryList(ReqCnt);
      CurCnt := ThreadCallStack.Count;
      if CurCnt < ReqCnt then
        exit;
    end;
  finally
    FDebugger.LockList.FreeLockFor(ThreadCallStack);
  end;
end;

procedure TFpThreadWorkerPrepareCallStackEntryList.DoExecute;
var
  t: TDbgThread;
begin
  if FRequiredMinCount < -1 then
    exit;
  if FThread = nil then begin
    for t in FDebugger.DbgController.CurrentProcess.ThreadMap do begin
      PrepareCallStackEntryList(FRequiredMinCount, t);
      if StopRequested then
        break;
    end;
  end
  else
    PrepareCallStackEntryList(FRequiredMinCount, FThread);
end;

constructor TFpThreadWorkerPrepareCallStackEntryList.Create(
  ADebugger: TFpDebugDebuggerBase; ARequiredMinCount: Integer;
  APriority: TFpThreadWorkerPriority);
begin
  inherited Create(ADebugger, APriority);
  FRequiredMinCount := ARequiredMinCount;
  FThread := nil;
end;

constructor TFpThreadWorkerPrepareCallStackEntryList.Create(
  ADebugger: TFpDebugDebuggerBase; ARequiredMinCount: Integer; AThread: TDbgThread);
begin
  Create(ADebugger, ARequiredMinCount);
  FThread := AThread;
end;

{ TFpThreadWorkerCallStackCount }

procedure TFpThreadWorkerCallStackCount.DoExecute;
begin
  inherited DoExecute;
  Queue(@UpdateCallstack_DecRef);
end;

{ TFpThreadWorkerCallEntry }

procedure TFpThreadWorkerCallEntry.DoExecute;
var
  PrettyPrinter: TFpPascalPrettyPrinter;
  Prop: TFpDebugDebuggerProperties;
  DbgCallStack: TDbgCallstackEntry;
begin
  inherited DoExecute;

  DbgCallStack := FThread.CallStackEntryList[FCallstackIndex];
  FValid := (DbgCallStack <> nil) and (not StopRequested);
  if FValid then begin
    Prop := TFpDebugDebuggerProperties(FDebugger.GetProperties);
    PrettyPrinter := TFpPascalPrettyPrinter.Create(DBGPTRSIZE[FDebugger.DbgController.CurrentProcess.Mode]);
    PrettyPrinter.Context := FDebugger.DbgController.DefaultContext;

    FDebugger.MemManager.MemLimits.MaxArrayLen            := Prop.MemLimits.MaxStackArrayLen;
    FDebugger.MemManager.MemLimits.MaxStringLen           := Prop.MemLimits.MaxStackStringLen;
    FDebugger.MemManager.MemLimits.MaxNullStringSearchLen := Prop.MemLimits.MaxStackNullStringSearchLen;

    FSrcClassName := DbgCallStack.SrcClassName;
    FAnAddress := DbgCallStack.AnAddress;
    FFunctionName := DbgCallStack.FunctionName;
    FSourceFile := DbgCallStack.SourceFile;
    FLine := DbgCallStack.Line;

    FParamAsString := GetParamsAsString(FThread, DbgCallStack, FDebugger.MemManager, FDebugger.TargetWidth, PrettyPrinter);
    PrettyPrinter.Free;

    FDebugger.MemManager.MemLimits.MaxArrayLen            := Prop.MemLimits.MaxArrayLen;
    FDebugger.MemManager.MemLimits.MaxStringLen           := Prop.MemLimits.MaxStringLen;
    FDebugger.MemManager.MemLimits.MaxNullStringSearchLen := Prop.MemLimits.MaxNullStringSearchLen;
  end;

  Queue(@UpdateCallstackEntry_DecRef);
end;

{ TFpThreadWorkerThreads }

procedure TFpThreadWorkerThreads.DoExecute;
begin
  inherited DoExecute;
  Queue(@UpdateThreads_DecRef);
end;

constructor TFpThreadWorkerThreads.Create(ADebugger: TFpDebugDebuggerBase);
begin
  inherited Create(ADebugger, 1, twpThread);
end;

{ TFpThreadWorkerLocals }

procedure TFpThreadWorkerLocals.DoExecute;
var
  LocalScope: TFpDbgSymbolScope;
  ProcVal, m: TFpValue;
  i: Integer;
  WatchResConv: TFpLazDbgWatchResultConvertor;
  ResData: IDbgWatchDataIntf;
begin
  LocalScope := FDebugger.DbgController.CurrentProcess.FindSymbolScope(FThreadId, FStackFrame);
  if (LocalScope = nil) or (LocalScope.SymbolAtAddress = nil) then begin
    LocalScope.ReleaseReference;
    exit;
  end;

  ProcVal := LocalScope.ProcedureAtAddress;
  if (ProcVal = nil) then begin
    LocalScope.ReleaseReference;
    exit;
  end;

  WatchResConv := TFpLazDbgWatchResultConvertor.Create(LocalScope.LocationContext);
  WatchResConv.MaxArrayConv := TFpDebugDebuggerProperties(FDebugger.GetProperties).MemLimits.MaxArrayConversionCnt;
  WatchResConv.MaxTotalConv := TFpDebugDebuggerProperties(FDebugger.GetProperties).MemLimits.MaxTotalConversionCnt;
  WatchResConv.Debugger := FDebugger;
  WatchResConv.ExpressionScope := LocalScope;

  for i := 0 to ProcVal.MemberCount - 1 do begin
    m := ProcVal.Member[i];
    if m <> nil then begin
      ResData := FLocals.Add(m.DbgSymbol.Name);
      if not  WatchResConv.WriteWatchResultData(m, ResData)
      then begin
        ResData.CreateError('Unknown Error');
      end;
      m.ReleaseReference;
    end;
    if StopRequested then
      Break;
  end;

  WatchResConv.Free;
  ProcVal.ReleaseReference;
  LocalScope.ReleaseReference;

  Queue(@UpdateLocals_DecRef);
end;

{ TFpThreadWorkerModify }

procedure TFpThreadWorkerModify.DoExecute;
var
  APasExpr: TFpPascalExpression;
  ResValue: TFpValue;
  ExpressionScope: TFpDbgSymbolScope;
  i64: int64;
  c64: QWord;
begin
  FSuccess := False;
  ExpressionScope := FDebugger.DbgController.CurrentProcess.FindSymbolScope(FThreadId, FStackFrame);
  if ExpressionScope = nil then
    exit;

  APasExpr := TFpPascalExpression.Create(FExpression, ExpressionScope, True);
  APasExpr.IntrinsicPrefix := TFpDebugDebuggerProperties(FDebugger.GetProperties).IntrinsicPrefix;
  APasExpr.Parse;
  try
    APasExpr.ResultValue; // trigger full validation
    if not APasExpr.Valid then
      exit;

    ResValue := APasExpr.ResultValue;
    if ResValue = nil then
      exit;

    FSuccess := True;
    try
      case ResValue.Kind of
        skInteger:   if TryStrToInt64(FNewVal, i64)
                     then ResValue.AsInteger := i64
                     else FSuccess := False;
        skCardinal:  if TryStrToQWord(FNewVal, c64)
                     then ResValue.AsCardinal := c64
                     else FSuccess := False;
        skBoolean:   case LowerCase(trim(FNewVal)) of
            'true':  ResValue.AsBool := True;
            'false': ResValue.AsBool := False;
            otherwise FSuccess := False;
          end;
        skChar:      ResValue.AsString := FNewVal;
        skEnum:      ResValue.AsString := FNewVal;
        skSet:       ResValue.AsString := FNewVal;
        skPointer:   if TryStrToQWord(FNewVal, c64) then
                     ResValue.AsCardinal := c64
                     else FSuccess := False;
        //skFloat: ;
        //skCurrency: ;
        //skVariant: ;
        otherwise
          FSuccess := False;
      end;
    except
      FSuccess := False;
    end;

  finally
    APasExpr.Free;
    ExpressionScope.ReleaseReference;
    Queue(@DoCallback_DecRef);
  end;
end;

constructor TFpThreadWorkerModify.Create(ADebugger: TFpDebugDebuggerBase;
  const AnExpression, ANewValue: String; AStackFrame, AThreadId: Integer);
begin
  inherited Create(ADebugger, twpModify);
  FExpression := AnExpression;
  FNewVal := ANewValue;
  FStackFrame := AStackFrame;
  FThreadId := AThreadId;
end;

function TFpThreadWorkerModify.DebugText: String;
begin
  Result := inherited DebugText;
end;

{ TFpThreadWorkerEvaluate }

function TFpThreadWorkerEvaluate.DoWatchFunctionCall(
  AnExpressionPart: TFpPascalExpressionPart; AFunctionValue,
  ASelfValue: TFpValue; AParams: TFpPascalExpressionPartList; out
  AResult: TFpValue; var AnError: TFpError): boolean;
var
  FunctionSymbolData, FunctionSymbolType, FunctionResultSymbolType,
  TempSymbol, StringSymbol: TFpSymbol;
  ExprParamVal: TFpValue;
  ProcAddress: TFpDbgMemLocation;
  FunctionResultDataSize: TFpDbgValueSize;

  SelfTypeSym: TFpSymbol;
  ParameterSymbolArr: array of record
    ParamVal: TFpValue;
    TypeSym: TFpSymbol;
    TempAnsiStringDataAddr: TDbgPtr;
    TempWideStringDataAddr: TDbgPtr;
  end;
  CallContext: TFpDbgInfoCallContext;
  PCnt, i, FoundIdx: Integer;
  rk: TDbgSymbolKind;
  StringResultAddr, StringResultDecRefAddress: TDBGPtr;
  StringAnsiSetLenAddress, StringAnsiDecRefAddress: TDBGPtr;
  StringWideSetLenAddress, StringWideDecRefAddress: TDBGPtr;
  ParRes: Boolean;
begin
  Result := False;
  if FExpressionScope = nil then
    exit;
(*
   AFunctionValue =>  TFpValueDwarfSubroutine  // gotten from <== TFpSymbolDwarfDataProc.GetValueObject;
                   .DataSympol = TFpSymbolDwarfDataProc  from which we were created
                   .TypeSymbol = TFpSymbolDwarfTypeProc.TypeInfo : TFpSymbolDwarfType

   AFunctionFpSymbol => TFpSymbolDwarfTypeProc;
   val
*)

  FunctionSymbolData := AFunctionValue.DbgSymbol;  // AFunctionValue . FDataSymbol
  FunctionSymbolType := FunctionSymbolData.TypeInfo;
  FunctionResultSymbolType := FunctionSymbolType.TypeInfo;

  if not (FunctionResultSymbolType.Kind in [skInteger, skCurrency, skPointer, skEnum,
      skCardinal, skBoolean, skChar, skClass, skString, skAnsiString, skWideString])
  then begin
    DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, ['Error result kind  ', dbgs(FunctionSymbolType.Kind)]);
    AnError := CreateError(fpErrAnyError, ['Result type of function not supported']);
    exit;
  end;

  // TODO: pass a value object
  if (not FunctionResultSymbolType.ReadSize(nil, FunctionResultDataSize)) or
     (FunctionResultDataSize >  FDebugger.MemManager.RegisterSize(0))
  then begin
    DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, ['Error result size', dbgs(FunctionResultDataSize)]);
    //ReturnMessage := 'Unable to call function. The size of the function-result exceeds the content-size of a register.';
    AnError := CreateError(fpErrAnyError, ['Result type of function not supported']);
    exit;
  end;

  try
    ParameterSymbolArr := nil;
    StringResultDecRefAddress := 0; // Wide OR Ansi-DecRef
    StringAnsiDecRefAddress := 0;
    StringWideDecRefAddress := 0;
    StringAnsiSetLenAddress := 0;
    StringWideSetLenAddress := 0;
    StringResultAddr := 0;
    SelfTypeSym := nil;

    if (FunctionResultSymbolType.Kind in [skString, skAnsiString, skWideString])
    then begin
      if (FunctionResultSymbolType.Kind = skWideString) then
        StringResultDecRefAddress := FDebugger.GetCached_FPC_WIDESTR_DECR_REF
      else
        StringResultDecRefAddress := FDebugger.GetCached_FPC_ANSISTR_DECR_REF;

      if (StringResultDecRefAddress = 0) then begin
        DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, ['Error result kind  ', dbgs(FunctionSymbolType.Kind)]);
        AnError := CreateError(fpErrAnyError, ['Result type of function not supported']);
        exit;
      end;

      for i := 0 to FunctionSymbolType.NestedSymbolCount - 1 do begin
        StringSymbol := FunctionSymbolType.NestedSymbol[i];
        if sfParameter in StringSymbol.Flags then
          Continue;
        if StringSymbol.Name = '$result' then
          break;
      end;
      if StringSymbol = nil then begin
        AnError := CreateError(fpErrAnyError, ['Result for string not found']);
        exit;
      end;
    end;

    // check params

    ProcAddress := AFunctionValue.EntryPCAddress;
    if not IsReadableLoc(ProcAddress) then begin
      DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, ['Error proc addr']);
      AnError := CreateError(fpErrAnyError, ['Unable to calculate function address']);
      exit;
    end;

    PCnt := AParams.Count;
    FoundIdx := 0;
    if ASelfValue <> nil then
      FoundIdx := -1;

    SetLength(ParameterSymbolArr, PCnt);
    for i := 0 to High(ParameterSymbolArr) do begin
      ParameterSymbolArr[i].ParamVal := nil;
      ParameterSymbolArr[i].TypeSym  := nil;
      ParameterSymbolArr[i].TempAnsiStringDataAddr:= 0;
      ParameterSymbolArr[i].TempWideStringDataAddr:= 0;
    end;

    for i := 0 to FunctionSymbolType.NestedSymbolCount - 1 do begin
      TempSymbol := FunctionSymbolType.NestedSymbol[i];
      if sfParameter in TempSymbol.Flags then begin
        if FoundIdx >= PCnt then begin
          FoundIdx := -2; // error
          break;
        end;

        // Type Compatibility
        if FoundIdx = -1 then begin
          // TODO: check self param
          SelfTypeSym := TempSymbol;
          SelfTypeSym.AddReference;
        end
        else begin
          ExprParamVal := AParams.Items[FoundIdx].ResultValue;
          if (ExprParamVal = nil) then begin
            DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'Internal error for arg %d ', [FoundIdx]);
            AnError := AnExpressionPart.Expression.Error;
            if not IsError(AnError) then
              AnError := CreateError(fpErrAnyError, ['internal error, computing parameter']);
            exit;
          end;

          rk := ExprParamVal.Kind;
          if not(rk in [skInteger, {skCurrency,} skPointer, skEnum, skCardinal, skBoolean, skChar, skClass, skRecord, skString, skAnsiString, skWideString])
          then begin
            DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'Error not supported kind arg %d : %s ', [FoundIdx, dbgs(rk)]);
            AnError := CreateError(fpErrAnyError, ['parameter type not supported']);
            exit;
          end;
          // Handle string/char - literals, constants, expression-results
          if (rk in [skString, skAnsiString, skChar]) and (ExprParamVal.FieldFlags * [svfAddress, svfDataAddress] = []) and
             (TempSymbol.Kind in [skString, skAnsiString])
          then begin
            StringAnsiDecRefAddress := FDebugger.GetCached_FPC_ANSISTR_DECR_REF;
            StringAnsiSetLenAddress := FDebugger.GetCached_FPC_ANSISTR_SETLENGTH;
            if (StringAnsiDecRefAddress = 0) or (StringAnsiSetLenAddress = 0) or
               (not FDebugger.CreateAnsiStringInTarget(StringAnsiSetLenAddress,
                    ParameterSymbolArr[FoundIdx].TempAnsiStringDataAddr,
                    ExprParamVal.AsString, FExpressionScope.LocationContext) )
            then begin
              AnError := CreateError(fpErrAnyError, ['constant string failed']);
              ParameterSymbolArr[FoundIdx].TempAnsiStringDataAddr := 0;
              exit;
            end;
          end
          else
          // Handle wide-string/char - literals, constants, expression-results
          if (rk in [skWideString, skString, skAnsiString, skChar]) and (ExprParamVal.FieldFlags * [svfAddress, svfDataAddress] = []) and
             (TempSymbol.Kind in [skWideString])
          then begin
            StringWideDecRefAddress := FDebugger.GetCached_FPC_WIDESTR_DECR_REF;
            StringWideSetLenAddress := FDebugger.GetCached_FPC_WIDESTR_SETLENGTH;
            if (StringWideDecRefAddress = 0) or (StringWideSetLenAddress = 0) or
               (not FDebugger.CreateWideStringInTarget(StringWideSetLenAddress,
                    ParameterSymbolArr[FoundIdx].TempWideStringDataAddr,
                    ExprParamVal.AsWideString, FExpressionScope.LocationContext) )
            then begin
              AnError := CreateError(fpErrAnyError, ['constant string failed']);
              ParameterSymbolArr[FoundIdx].TempWideStringDataAddr := 0;
              exit;
            end;
          end
          else
          if (TempSymbol.Kind <> rk) and
             ( (TempSymbol.Kind in [skInteger, skCardinal]) <> (rk in [skInteger, skCardinal]) )
          then begin
            DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'Error kind mismatch for arg %d : %s <> %s', [FoundIdx, dbgs(TempSymbol.Kind), dbgs(rk)]);
            AnError := CreateError(fpErrAnyError, ['wrong type for parameter']);
            exit;
          end;

          TempSymbol.AddReference;
          ParameterSymbolArr[FoundIdx].ParamVal := ExprParamVal;
          ParameterSymbolArr[FoundIdx].TypeSym  := TempSymbol;
        end;
        //if not IsTargetOrRegNotNil(FDebugger.DbgController.CurrentProcess.CallParamDefaultLocation(FoundIdx)) then begin
        //  DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'error to many args / not supported / arg > %d ', [FoundIdx]);
        //  AnError := CreateError(fpErrAnyError, ['too many parameter / not supported']);
        //  exit;
        //end;
        inc(FoundIdx)
      end;
    end;

    if (FoundIdx <> PCnt) then begin
      DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, ['Error param count']);
      AnError := CreateError(fpErrAnyError, ['wrong amount of parameters']);
      exit;
    end;


    CallContext := FDebugger.DbgController.Call(ProcAddress, FExpressionScope.LocationContext,
      FDebugger.MemReader, FDebugger.MemConverter);
    if CallContext = nil then begin
      AnError := CreateError(fpErrAnyError, ['function call not possible']);
      exit;
    end;

    try
      if (ASelfValue <> nil) then begin
        if not CallContext.AddParam(SelfTypeSym.TypeInfo, ASelfValue) then begin
          DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'Internal error for self');
          AnError := CallContext.LastError;
          exit;
        end;
      end;

      if (StringResultDecRefAddress <> 0) then begin
        if not CallContext.AddStringResult then begin
          DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'Internal error for string result');
          AnError := CallContext.LastError;
          exit;
        end;
      end;

      for i := 0 to High(ParameterSymbolArr) do begin
        if ParameterSymbolArr[i].TempAnsiStringDataAddr <> 0 then
          ParRes := CallContext.AddOrdinalParam(ParameterSymbolArr[i].TempAnsiStringDataAddr)
        else
        if ParameterSymbolArr[i].TempWideStringDataAddr <> 0 then
          ParRes := CallContext.AddOrdinalParam(ParameterSymbolArr[i].TempWideStringDataAddr)
        else
          ParRes := CallContext.AddParam(ParameterSymbolArr[i].TypeSym.TypeInfo, ParameterSymbolArr[i].ParamVal);
        if not ParRes then begin
          DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'Internal error for arg %d ', [i]);
          AnError := CallContext.LastError;
          exit;
        end;
      end;

      if not CallContext.FinalizeParams then begin
        DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, 'Internal error after params');
        AnError := CallContext.LastError;
        exit;
      end;

      FDebugger.BeforeWatchEval(CallContext);
      FDebugger.RunProcessLoop(not FAllowFunctionsAllThread);

      if not CallContext.IsValid then begin
        DebugLn(FPDBG_FUNCCALL or DBG_WARNINGS, ['Error in call ',CallContext.Message]);
        //ReturnMessage := CallContext.Message;
        AnError := CallContext.LastError;
        if not IsError(AnError) then
          if CallContext.Message <> '' then
            AnError := CreateError(fpErrAnyError, [CallContext.Message])
          else
            AnError := CreateError(fpErrAnyError, ['Error in function execution']);
        exit;
      end;

      if (FunctionResultSymbolType.Kind in [skString, skAnsiString, skWideString]) then begin
        if not CallContext.GetStringResultAsPointer(StringResultAddr) then begin
          AnError := CallContext.LastError;
        end
        else
        if (FunctionResultSymbolType.Kind = skWideString) then begin
          if not CallContext.GetWideStringResult(AResult, FunctionSymbolType.TypeInfo) then begin
            AnError := CallContext.LastError;
          end;
        end
        else begin
          if not CallContext.GetStringResult(AResult, FunctionSymbolType.TypeInfo) then begin
            AnError := CallContext.LastError;
          end;
        end;
      end
      else begin
        AResult := CallContext.CreateParamSymbol(-1, FunctionSymbolType, FunctionSymbolData.Name);
      end;
      Result := AResult <> nil;
    finally
      FDebugger.DbgController.AbortCurrentCommand(True);
      CallContext.ReleaseReference;
    end;

    if (FunctionResultSymbolType.Kind in [skString, skAnsiString, skWideString]) and (StringResultAddr <> 0) then begin
      FDebugger.CallTargetFuncStringDecRef(StringResultDecRefAddress, StringResultAddr, FExpressionScope.LocationContext);
    end;

  finally
    FDebugger.DbgController.CurrentThread.RestoreStackMem;

    SelfTypeSym.ReleaseReference;
    for i := 0 to High(ParameterSymbolArr) do begin
      if ParameterSymbolArr[i].TypeSym <> nil then
        ParameterSymbolArr[i].TypeSym.ReleaseReference;
      if ParameterSymbolArr[i].TempAnsiStringDataAddr <> 0 then
        FDebugger.CallTargetFuncStringDecRef(StringAnsiDecRefAddress, ParameterSymbolArr[i].TempAnsiStringDataAddr, FExpressionScope.LocationContext);
      if ParameterSymbolArr[i].TempWideStringDataAddr <> 0 then
        FDebugger.CallTargetFuncStringDecRef(StringWideDecRefAddress, ParameterSymbolArr[i].TempWideStringDataAddr, FExpressionScope.LocationContext);
      end;
  end;

end;

function TFpThreadWorkerEvaluate.EvaluateExpression(const AnExpression: String;
  AStackFrame, AThreadId: Integer; ADispFormat: TWatchDisplayFormat;
  ARepeatCnt: Integer; AnEvalFlags: TWatcheEvaluateFlags; out AResText: String;
  out ATypeInfo: TDBGType): Boolean;
var
  APasExpr, PasExpr2: TFpPascalExpression;
  PrettyPrinter: TFpPascalPrettyPrinter;
  ResValue: TFpValue;
  CastName, ResText2: String;
  WatchResConv: TFpLazDbgWatchResultConvertor;
  ResData: IDbgWatchDataIntf;
  i: Integer;
begin
  Result := False;
  AResText := '';
  ATypeInfo := nil;

  FExpressionScope := FDebugger.DbgController.CurrentProcess.FindSymbolScope(AThreadId, AStackFrame);
  if FExpressionScope = nil then begin
    if FWatchValue <> nil then
      FWatchValue.Validity := ddsInvalid;
    exit;
  end;

  PrettyPrinter := nil;
  APasExpr := TFpPascalExpression.Create(AnExpression, FExpressionScope, True);
  APasExpr.IntrinsicPrefix := TFpDebugDebuggerProperties(FDebugger.GetProperties).IntrinsicPrefix;
  APasExpr.Parse;
  try
    if FAllowFunctions and (dfEvalFunctionCalls in FDebugger.EnabledFeatures) then
      APasExpr.OnFunctionCall  := @DoWatchFunctionCall;
    APasExpr.ResultValue; // trigger full validation
    if not APasExpr.Valid then begin
      ErrorHandler.OnErrorTextLookup := @GetErrorText;
      AResText := ErrorHandler.ErrorAsString(APasExpr.Error);
      if FWatchValue <> nil then begin
        FWatchValue.Value := AResText;
        FWatchValue.Validity := ddsError;
      end;
      exit;
    end;

    ResValue := APasExpr.ResultValue;
    if ResValue = nil then begin
      AResText := 'Error';
      if FWatchValue <> nil then begin
        FWatchValue.Value := AResText;
        FWatchValue.Validity := ddsError;
      end;
      exit;
    end;

    if StopRequested then begin
      if FWatchValue <> nil then
        FWatchValue.Validity := ddsInvalid;
      exit;
    end;
    if (ResValue.Kind = skClass) and (ResValue.AsCardinal <> 0) and
       (not IsError(ResValue.LastError)) and (defClassAutoCast in AnEvalFlags)
    then begin
      if ResValue.GetInstanceClassName(CastName) then begin
        PasExpr2 := TFpPascalExpression.Create(CastName+'('+AnExpression+')', FExpressionScope, True);
        PasExpr2.IntrinsicPrefix := TFpDebugDebuggerProperties(FDebugger.GetProperties).IntrinsicPrefix;
        PasExpr2.Parse;
        PasExpr2.ResultValue;
        if PasExpr2.Valid then begin
          APasExpr.Free;
          APasExpr := PasExpr2;
          ResValue := APasExpr.ResultValue;
        end
        else
          PasExpr2.Free;
      end
      else begin
        ResValue.ResetError; // in case GetInstanceClassName did set an error
        // TODO: indicate that typecasting to instance failed
      end;
    end;

    if StopRequested then begin
      if FWatchValue <> nil then
        FWatchValue.Validity := ddsInvalid;
      exit;
    end;

    if (ResValue <> nil) and (ResValue.Kind = skAddress) then
      ADispFormat := wdfMemDump;

    if (FWatchValue <> nil) and (ResValue <> nil) and
       (ADispFormat <> wdfMemDump)   // TODO
    then begin
      WatchResConv := TFpLazDbgWatchResultConvertor.Create(FExpressionScope.LocationContext);
      WatchResConv.MaxArrayConv := TFpDebugDebuggerProperties(FDebugger.GetProperties).MemLimits.MaxArrayConversionCnt;
      WatchResConv.MaxTotalConv := TFpDebugDebuggerProperties(FDebugger.GetProperties).MemLimits.MaxTotalConversionCnt;
      WatchResConv.ExtraDepth := defExtraDepth in FWatchValue.EvaluateFlags;
      WatchResConv.FirstIndexOffs := FWatchValue.FirstIndexOffs;
      if not (defSkipValConv in AnEvalFlags) then begin
        if (FWatchValue.GetDbgValConverter <> nil) and
           (FWatchValue.GetDbgValConverter.GetConverter.GetObject is TFpDbgValueConverter)
        then
          WatchResConv.ValConfig := FWatchValue.GetDbgValConverter
        else
          WatchResConv.ValConvList := ValueConverterConfigList;
        WatchResConv.Debugger := FDebugger;
      end;
      WatchResConv.ExpressionScope := FExpressionScope;
      ResData := FWatchValue.ResData;
      Result := WatchResConv.WriteWatchResultData(ResValue, ResData, FWatchValue.RepeatCount);

      if Result and APasExpr.HasPCharIndexAccess and not IsError(ResValue.LastError) then begin
      // TODO: Only dwarf 2
        ResData := ResData.SetPCharShouldBeStringValue;
        if ResData <> nil then begin
          APasExpr.FixPCharIndexAccess := True;
          APasExpr.ResetEvaluation;
          ResValue := APasExpr.ResultValue;
          WatchResConv.WriteWatchResultData(ResValue, ResData, FWatchValue.RepeatCount);
        end;
      end;

      WatchResConv.Free;
      if Result then
        exit;
    end;

    PrettyPrinter := TFpPascalPrettyPrinter.Create(FExpressionScope.SizeOfAddress);
    PrettyPrinter.Context := FExpressionScope.LocationContext;

    if defNoTypeInfo in AnEvalFlags then
      Result := PrettyPrinter.PrintValue(AResText, ResValue, ADispFormat, ARepeatCnt)
    else
      Result := PrettyPrinter.PrintValue(AResText, ATypeInfo, ResValue, ADispFormat, ARepeatCnt);

    // PCHAR/String
    if Result and APasExpr.HasPCharIndexAccess and not IsError(ResValue.LastError) then begin
    // TODO: Only dwarf 2
      APasExpr.FixPCharIndexAccess := True;
      APasExpr.ResetEvaluation;
      ResValue := APasExpr.ResultValue;
      if (ResValue=nil) or (not PrettyPrinter.PrintValue(ResText2, ResValue, ADispFormat, ARepeatCnt)) then
        ResText2 := 'Failed';
      AResText := 'PChar: '+AResText+ LineEnding + 'String: '+ResText2;
    end;

    if Result then
      Result := not IsError(ResValue.LastError) // AResText should be set from Prettyprinter
    else
      AResText := 'Error';

    if not Result then
      FreeAndNil(ATypeInfo);
    if FWatchValue <> nil then begin
      FWatchValue.Value := AResText;
      FWatchValue.TypeInfo := ATypeInfo;
      FWatchValue.Validity := ddsValid;
    end;
  finally
    PrettyPrinter.Free;
    APasExpr.Free;
    FExpressionScope.ReleaseReference;
  end;
end;

{ TFpThreadWorkerEvaluateExpr }

procedure TFpThreadWorkerEvaluateExpr.DoExecute;
begin
  FRes := EvaluateExpression(FExpression, FStackFrame, FThreadId,
    FDispFormat, FRepeatCnt, FEvalFlags, FResText, FResDbgType);
end;

constructor TFpThreadWorkerEvaluateExpr.Create(ADebugger: TFpDebugDebuggerBase;
  APriority: TFpThreadWorkerPriority; const AnExpression: String; AStackFrame,
  AThreadId: Integer; ADispFormat: TWatchDisplayFormat; ARepeatCnt: Integer;
  AnEvalFlags: TWatcheEvaluateFlags);
begin
  inherited Create(ADebugger, APriority);
  FExpression := AnExpression;
  FStackFrame := AStackFrame;
  FThreadId := AThreadId;
  FDispFormat := ADispFormat;
  FRepeatCnt := ARepeatCnt;
  FEvalFlags := AnEvalFlags;
  FAllowFunctions := defAllowFunctionCall in AnEvalFlags;
  FAllowFunctionsAllThread := defFunctionCallRunAllThreads in AnEvalFlags;
  FRes := False;
end;

function TFpThreadWorkerEvaluateExpr.DebugText: String;
begin
  Result := inherited DebugText;
  if self = nil then exit;
  Result := Format('%s Expr: "%s" T: %s S: %s', [Result, FExpression, dbgs(FThreadId), dbgs(FStackFrame)]);
end;

{ TFpThreadWorkerCmdEval }

procedure TFpThreadWorkerCmdEval.DoCallback_DecRef(Data: PtrInt);
var
  CB: TDBGEvaluateResultCallback;
  Dbg: TFpDebugDebuggerBase;
  Res: Boolean;
  ResText: String;
  ResDbgType: TDBGType;
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCmdEval.DoCallback_DecRef: system.ThreadID = classes.MainThreadID');
  CB := nil;
  try
    if FEvalFlags * [defNoTypeInfo, defSimpleTypeInfo, defFullTypeInfo] = [defNoTypeInfo] then
      FreeAndNil(FResText);

    if (FCallback <> nil) then begin
      // All to local vars, because SELF may be destroyed before/while the callback happens
      CB := FCallback;
      Dbg := FDebugger;
      Res := FRes;
      ResText := FResText;
      ResDbgType := FResDbgType;
      FResDbgType := nil; // prevent from being freed => will be freed in callback
      FCallback := nil; // Ensure callback is never called a 2nd time (e.g. if Self.Abort is called, while in Callback)
      (* We cannot call Callback here, because ABORT can be called, and prematurely call UnQueue_DecRef,
         removing the last ref to this object *)
    end;
  except
  end;

  UnQueue_DecRef;

  // Self may now be invalid, unless FDebugger.FEvalWorkItem still has a reference.
  // Abort may be called (during CB), removing this refence.
  // Abort would be called, if a new Evaluate Request is made. FEvalWorkItem<>nil
  if CB <> nil then
    CB(Dbg, Res, ResText, ResDbgType);
end;

procedure TFpThreadWorkerCmdEval.DoExecute;
begin
  inherited DoExecute;
  Queue(@DoCallback_DecRef);
end;

constructor TFpThreadWorkerCmdEval.Create(ADebugger: TFpDebugDebuggerBase;
  APriority: TFpThreadWorkerPriority; const AnExpression: String; AStackFrame,
  AThreadId: Integer; AnEvalFlags: TWatcheEvaluateFlags;
  ACallback: TDBGEvaluateResultCallback);
begin
  inherited Create(ADebugger, APriority, AnExpression, AStackFrame, AThreadId, wdfDefault, 0,
    AnEvalFlags);
  FCallback := ACallback;
end;

destructor TFpThreadWorkerCmdEval.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FResDbgType);
end;

procedure TFpThreadWorkerCmdEval.Abort;
begin
  RequestStop;
  FDebugger.WorkQueue.RemoveItem(Self);
  DoCallback_DecRef;
end;

{ TFpThreadWorkerWatchValueEval }

procedure TFpThreadWorkerWatchValueEval.DoExecute;
begin
  inherited DoExecute;
  Queue(@UpdateWatch_DecRef);
end;

{ TFpThreadWorkerBreakPoint }

procedure TFpThreadWorkerBreakPoint.RemoveBreakPoint_DecRef;
begin
  //
end;

procedure TFpThreadWorkerBreakPoint.AbortSetBreak;
begin
  //
end;

{ TFpThreadWorkerBreakPointSet }

procedure TFpThreadWorkerBreakPointSet.DoExecute;
var
  CurContext: TFpDbgSymbolScope;
  WatchPasExpr: TFpPascalExpression;
  R: TFpValue;
  s: TFpDbgValueSize;
begin
  case FKind of
    bpkAddress:
      FInternalBreakpoint := FDebugger.DbgController.CurrentProcess.AddBreak(FAddress, True);
    bpkSource:
      FInternalBreakpoint := FDebugger.DbgController.CurrentProcess.AddBreak(FSource, FLine, True);
    bpkData: begin
      CurContext := FDebugger.DbgController.CurrentProcess.FindSymbolScope(FThreadId, FStackFrame);
      if CurContext <> nil then begin
        WatchPasExpr := TFpPascalExpression.Create(FWatchData, CurContext, True);
        WatchPasExpr.IntrinsicPrefix := TFpDebugDebuggerProperties(FDebugger.GetProperties).IntrinsicPrefix;
        WatchPasExpr.Parse;
        R := WatchPasExpr.ResultValue; // Address and Size
        // TODO: Cache current value
        if WatchPasExpr.Valid and IsTargetNotNil(R.Address) and R.GetSize(s) then begin
          // pass context
          FInternalBreakpoint := FDebugger.DbgController.CurrentProcess.AddWatch(R.Address.Address, SizeToFullBytes(s), FWatchKind, FWatchScope);
        end;
        WatchPasExpr.Free;
        CurContext.ReleaseReference;
      end;
    end;
  end;
  if FResetBreakPoint then begin
    FDebugger.DbgController.CurrentProcess.RemoveBreak(FInternalBreakpoint);
    FreeAndNil(FInternalBreakpoint);
  end;
  Queue(@UpdateBrkPoint_DecRef);
end;

constructor TFpThreadWorkerBreakPointSet.Create(ADebugger: TFpDebugDebuggerBase; AnAddress: TDBGPtr);
begin
  FKind := bpkAddress;
  FAddress := AnAddress;
  inherited Create(ADebugger, twpUser);
end;

constructor TFpThreadWorkerBreakPointSet.Create(
  ADebugger: TFpDebugDebuggerBase; ASource: String; ALine: Integer);
begin
  FKind := bpkSource;
  FSource := ASource;
  FLine   := ALine;
  inherited Create(ADebugger, twpUser);
end;

constructor TFpThreadWorkerBreakPointSet.Create(
  ADebugger: TFpDebugDebuggerBase; AWatchData: String;
  AWatchScope: TDBGWatchPointScope; AWatchKind: TDBGWatchPointKind;
  AStackFrame, AThreadId: Integer);
begin
  FKind := bpkData;
  FWatchData  := AWatchData;
  FWatchScope := AWatchScope;
  FWatchKind  := AWatchKind;
  FStackFrame := AStackFrame;
  FThreadId   := AThreadId;
  inherited Create(ADebugger, twpUser);
end;

{ TFpThreadWorkerBreakPointRemove }

procedure TFpThreadWorkerBreakPointRemove.DoExecute;
begin
  if (FDebugger.DbgController <> nil) and (FDebugger.DbgController.CurrentProcess <> nil) then
    FDebugger.DbgController.CurrentProcess.RemoveBreak(FInternalBreakpoint);
  FreeAndNil(FInternalBreakpoint);
end;

constructor TFpThreadWorkerBreakPointRemove.Create(
  ADebugger: TFpDebugDebuggerBase;
  AnInternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint);
begin
  FInternalBreakpoint := AnInternalBreakpoint;
  inherited Create(ADebugger, twpUser);
end;

initialization
  DBG_VERBOSE     := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS    := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_FUNCCALL := DebugLogger.FindOrRegisterLogGroup('FPDBG_FUNCCALL' {$IFDEF FPDBG_FUNCCALL} , True {$ENDIF} );

end.


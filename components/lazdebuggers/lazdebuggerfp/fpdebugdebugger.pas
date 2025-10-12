{
 ---------------------------------------------------------------------------
 FpDebugDebugger
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

unit FpDebugDebugger;

{$mode objfpc}{$H+}
{$IF FPC_Fullversion=30202}{$Optimization NOPEEPHOLE}{$ENDIF}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, {$IfDef WIN64}windows,{$EndIf} SysUtils, fgl, math, process,
  Forms, Dialogs, syncobjs,
  Maps, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, LazUTF8, lazCollections,
  DbgIntfDebuggerBase, DbgIntfProcess, LazDebuggerIntfBaseTypes,
  FpDebugDebuggerUtils, FpDebugDebuggerWorkThreads, FpDebugDebuggerBase, LazDebuggerIntf,
  // FpDebug
  {$IFDEF FPDEBUG_THREAD_CHECK} FpDbgCommon, {$ENDIF}
  FpDbgClasses, FpDbgInfo, FpErrorMessages, FpPascalBuilder, FpdMemoryTools,
  FpPascalParser, FPDbgController, FpDbgDwarfDataClasses, FpDbgDwarfFreePascal,
  FpDbgDwarf, FpDbgUtil, FpDbgCallContextInfo,
  // use converters
  FpDebugValueConvertors, FpDebugConvDebugForJson;

type

  TFpDebugDebugger = class;
  TFPBreakpoint = class;

  (* WorkerThreads:
     The below subclasses implement ONLY work that is done in the MAIN THREAD.
  *)

  { TFpDbgDebggerThreadWorkerItemHelper }

  TFpDbgDebggerThreadWorkerItemHelper = class helper for TFpDbgDebggerThreadWorkerItem
  protected
    function FpDebugger: TFpDebugDebugger; inline;
  end;

  { TFpThreadWorkerRunLoopUpdate }

  TFpThreadWorkerRunLoopUpdate = class(TFpThreadWorkerRunLoop)
  protected
     procedure LoopFinished_DecRef(Data: PtrInt = 0); override;
  end;

  { TFpThreadWorkerRunLoopAfterIdleUpdate }

  TFpThreadWorkerRunLoopAfterIdleUpdate = class(TFpThreadWorkerRunLoopAfterIdle)
  protected
    procedure CheckIdleOrRun_DecRef(Data: PtrInt = 0); override;
  end;

  { TFpThreadWorkerCallStackCountUpdate }

  TFpThreadWorkerCallStackCountUpdate = class(TFpThreadWorkerCallStackCount)
  private
    FCallstack: TCallStackBase;
    procedure DoCallstackFreed_DecRef(Sender: TObject);
  protected
    procedure UpdateCallstack_DecRef(Data: PtrInt = 0); override;
    procedure DoRemovedFromLinkedList; override;
  public
    constructor Create(ADebugger: TFpDebugDebugger; ACallstack: TCallStackBase; ARequiredMinCount: Integer);
    //procedure RemoveCallStack_DecRef;
  end;

  { TFpThreadWorkerCallEntryUpdate }

  TFpThreadWorkerCallEntryUpdate = class(TFpThreadWorkerCallEntry)
  private
    FCallstack: TCallStackBase;
    FCallstackEntry: TCallStackEntry;
    procedure DoCallstackFreed_DecRef(Sender: TObject);
    procedure DoCallstackEntryFreed_DecRef(Sender: TObject);
  protected
    procedure DoRemovedFromLinkedList; override;
    procedure UpdateCallstackEntry_DecRef(Data: PtrInt = 0); override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; AThread: TDbgThread; ACallstackEntry: TCallStackEntry; ACallstack: TCallStackBase = nil);
    //procedure RemoveCallStackEntry_DecRef;
  end;

  { TFpThreadWorkerThreadsUpdate }

  TFpThreadWorkerThreadsUpdate = class(TFpThreadWorkerThreads)
  protected
    procedure UpdateThreads_DecRef(Data: PtrInt = 0); override;
  end;

  { TFpThreadWorkerLocalsUpdate }

  TFpThreadWorkerLocalsUpdate = class(TFpThreadWorkerLocals)
  protected
    procedure UpdateLocals_DecRef(Data: PtrInt = 0); override;
    procedure DoRemovedFromLinkedList; override; // _DecRef
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; ALocals: IDbgLocalsListIntf);
  end;

  { TFpThreadWorkerModifyUpdate }

  TFpThreadWorkerModifyUpdate = class(TFpThreadWorkerModify)
  protected
    procedure DoCallback_DecRef(Data: PtrInt = 0); override;
  end;

  { TFpThreadWorkerWatchValueEvalUpdate }

  TFpThreadWorkerWatchValueEvalUpdate = class(TFpThreadWorkerWatchValueEval)
  private
    procedure DoWachCanceled(Sender: IDbgDataRequestIntf; Data: TDbgDataRequestEventData);
  protected
    procedure UpdateWatch_DecRef(Data: PtrInt = 0); override;
    procedure DoRemovedFromLinkedList; override; // _DecRef
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; AWatchValue: IDbgWatchValueIntf);
  end;

  { TFpThreadWorkerBreakPointSetUpdate }

  TFpThreadWorkerBreakPointSetUpdate = class(TFpThreadWorkerBreakPointSet)
  private
    FDbgBreakPoint: TFPBreakpoint;
  protected
     procedure UpdateBrkPoint_DecRef(Data: PtrInt = 0); override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; ADbgBreakPoint: TFPBreakpoint); overload;
    procedure AbortSetBreak; override;
    property DbgBreakPoint: TFPBreakpoint read FDbgBreakPoint;
  end;

  { TFpThreadWorkerBreakPointRemoveUpdate }

  TFpThreadWorkerBreakPointRemoveUpdate = class(TFpThreadWorkerBreakPointRemove)
  protected
    procedure DoUnQueued; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; ADbgBreakPoint: TFPBreakpoint); overload;
  end;

  { TDbgControllerStepOverOrFinallyCmd
    Step over with detection for finally blocks
  }

  TDbgControllerStepOverOrFinallyCmd = class(TDbgControllerStepOverLineCmd)
  private
    FFinState: (fsNone, fsMov, fsCall, fsInFin, fsDonePrologue, fsDonePrologue2);
  protected
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread;
      out Finished: boolean); override;
  end;

  { TDbgControllerStepOverFirstFinallyLineCmd }

  TDbgControllerStepOverFirstFinallyLineCmd = class(TDbgControllerStepOverLineCmd)
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread;
      out Finished: boolean); override;
  end;

  { TDbgControllerStepThroughFpcSpecialHandler }

  TDbgControllerStepThroughFpcSpecialHandler = class(TDbgControllerStepOverInstructionCmd)
  private
    FAfterFinCallAddr: TDbgPtr;
    FDone, FIsLeave: Boolean;
    FInteralFinished: Boolean;
  protected
    procedure DoResolveEvent(var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean); override;
    procedure InternalContinue(AProcess: TDbgProcess; AThread: TDbgThread); override;
    procedure Init; override;
  public
    constructor Create(AController: TDbgController; AnAfterFinCallAddr: TDbgPtr; AnIsLeave: Boolean); reintroduce;
    property InteralFinished: Boolean read FInteralFinished;
  end;

  { TFpDebugExceptionStepping }

  TFpDebugExceptionStepping = class
  (* Methods in this class are either called:
     - by the debug-thread / in the context of the debug-thread
     - by the main-thread / But ONLY if the debug-thread is paused
     Starting the debug-thread uses "RTLeventSetEvent" which triggers a
     memory barrier. So memberfields can be used savely between all methods.
  *)
  private
  type
    TBreakPointLoc = (
      bplRaise, bplReRaise, bplBreakError, bplRunError,
      bplPopExcept, bplCatches,
      {$IFDEF WIN64}
      bplRtlUnwind, bplFpcSpecific, bplRtlRestoreContext,
      bplSehW64Finally, bplSehW64Except, bplSehW64Unwound,
      {$ENDIF}
      {$IFDEF MSWINDOWS} // 32 bit or WOW
      bplFpcExceptHandler,
      bplFpcFinallyHandler,
      bplFpcLeaveHandler,
      bplSehW32Except,
      bplSehW32Finally,
      {$ENDIF}

      bplStepOut  // Step out of Pop/Catches
    );
    TBreakPointLocs = set of TBreakPointLoc;
    TExceptStepState = (esNone,
      esStoppedAtRaise,  // Enter dsPause, next step is "stop to finally"
      esIgnoredRaise,    // Keep dsRun, stop at finally/except *IF* outside current stepping frame
      esStepToFinally,
      esStepSehFinallyProloque,
      esSteppingFpcSpecialHandler,
      esAtWSehExcept
    );

    { TFrameList }

    TFrameList = class(specialize TFPGList<TDbgPtr>)
    public
      procedure RemoveOutOfScopeFrames(const ACurFrame: TDbgPtr); inline;
    end;

    { TAddressFrameList }

    TAddressFrameList = class(specialize TFPGMapObject<TDbgPtr, TFrameList>)
      FLastRemoveCheck: TDBGPtr;
      procedure DoRemoveOutOfScopeFrames(const ACurFrame: TDbgPtr; ABreakPoint: TFpDbgBreakpoint);
    public
      function Add(const AnAddress: TDbgPtr): TFrameList; inline; overload;
      function Add(const AnAddress, AFrame: TDbgPtr): boolean; inline; overload; // True if already exist
      function Remove(const AnAddress, AFrame: TDbgPtr): boolean; inline; // True if last frame for address
      procedure RemoveOutOfScopeFrames(const ACurFrame: TDbgPtr; ABreakPoint: TFpDbgBreakpoint); inline;
    end;
  const
    DBGPTRSIZE: array[TFPDMode] of Integer = (4, 8);
  private
    FDebugger: TFpDebugDebugger;
    FBreakPoints: array[TBreakPointLoc] of TFpDbgBreakpoint;
    FBreakEnabled: TBreakPointLocs;
    FBreakNewEnabled: TBreakPointLocs;
    {$IFDEF WIN64}
    FAddressFrameListSehW64Except,
    FAddressFrameListSehW64Finally: TAddressFrameList;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    FAddressFrameListSehW32Except: TAddressFrameList;
    FAddressFrameListSehW32Finally: TAddressFrameList;
    {$ENDIF}
    FState: TExceptStepState;
    function GetCurrentCommand: TDbgControllerCmd; inline;
    function GetCurrentProcess: TDbgProcess; inline;
    function GetCurrentThread: TDbgThread; inline;
    function GetDbgController: TDbgController; inline;
    function dbgs(st: TExceptStepState): string;
    function dbgs(loc: TBreakPointLoc): string;
    function dbgs(locs: TBreakPointLocs): string;
  protected
    property DbgController: TDbgController read GetDbgController;
    property CurrentProcess: TDbgProcess read GetCurrentProcess;
    property CurrentThread: TDbgThread read GetCurrentThread;
    property CurrentCommand: TDbgControllerCmd read GetCurrentCommand;

    procedure EnableBreaks(ALocs: TBreakPointLocs);
    procedure EnableBreaksDirect(ALocs: TBreakPointLocs); // only in dbg thread
    procedure DisableBreaks(ALocs: TBreakPointLocs);
    procedure DisableBreaksDirect(ALocs: TBreakPointLocs); // only in dbg thread
    procedure SetStepOutAddrDirect(AnAddr: TDBGPtr); // only in dbg thread

    procedure DoExceptionRaised(var &continue: boolean);
    //procedure DoPopExcptStack;
    procedure DoRtlUnwindEx;
  public
    constructor Create(ADebugger: TFpDebugDebugger);
    destructor Destroy; override;

    procedure DoProcessLoaded;
    procedure DoNtDllLoaded(ALib: TDbgLibrary);
    //procedure DoLibraryLoaded(ALib: TDbgLibrary);  // update breakpoints
    procedure DoDbgStopped;

    procedure ThreadBeforeLoop(Sender: TObject);
    procedure ThreadProcessLoopCycle(var AFinishLoopAndSendEvents: boolean;
      var AnEventType: TFPDEvent; var ACurCommand: TDbgControllerCmd; var AnIsFinished: boolean);
    function  BreakpointHit(var &continue: boolean; const Breakpoint: TFpDbgBreakpoint): boolean;
    procedure UserCommandRequested(var ACommand: TDBGCommand);

//    procedure ClearState;
  end;

  TBreakPointUpdateInfo = record
    InternalBreak: TFpDbgBreakpoint;
    NewState: TFpDbgBreakpointState;
  end;
  TBreakPointUpdateList = specialize TLazThreadedQueue<TBreakPointUpdateInfo>;

  TThreadIdList = specialize TFPGList<Integer>;

  { TFpDebugDebugger }

  TFpDebugDebugger = class(TFpDebugDebuggerBase)
  private type
    TFpDebugStringQueue = class(specialize TLazThreadedQueue<string>);
  private
    FIsIdle: Boolean;
    FPrettyPrinter: TFpPascalPrettyPrinter;
    FStartupCommand: TDBGCommand;
    FStartuRunToFile: string;
    FStartuRunToLine: LongInt;
    (* Each thread must only lock max one item at a time.
       This ensures the locking will be dead-lock free.
    *)
    FWorkerThreadId: TThreadID;
    FEvalWorkItem: TFpThreadWorkerCmdEval;
    FQuickPause, FRunQuickPauseTasks, FPauseForEvent, FInternalPauseForEvent, FSendingEvents: boolean;
    FExceptionStepper: TFpDebugExceptionStepping;
    FConsoleOutputThread: TThread;
    // Helper vars to run in debug-thread
    FCacheLine, FCacheBytesRead: cardinal;
    FCacheFileName: string;
    FCacheLib: TDbgLibrary;
    FCacheBreakpoint: TFpDbgBreakpoint;
    FCacheLocation, FCacheLocation2: TDBGPtr;
    FCacheBoolean: boolean;
    FCachePointer: pointer;
    FCacheThreadId, FCacheStackFrame: Integer;
    FCacheContext: TFpDbgSymbolScope;
    FBreakUpdateList: TBreakPointUpdateList;
    FFpDebugOutputQueue: TFpDebugStringQueue;
    FFpDebugOutputAsync: integer;
    FUseConsoleWinBuffer: boolean;
    FUseConsoleWinPos: boolean;
    FUseConsoleWinSize: boolean;
    FConsoleWinBuffer: TPoint;
    FConsoleWinPos: TPoint;
    FConsoleWinSize: TPoint;
    //
    procedure DoDebugOutput(Data: PtrInt);
    procedure DoThreadDebugOutput(Sender: TObject; ProcessId,
      ThreadId: Integer; AMessage: String);
    function GetClassInstanceName(AnAddr: TDBGPtr): string;
    function ReadAnsiString(AnAddr: TDbgPtr): string;
    procedure HandleSoftwareException(out AnExceptionLocation: TDBGLocationRec; var continue: boolean);
    // HandleBreakError: Default handler for range-check etc
    procedure HandleBreakError(var continue: boolean);
    // HandleRunError: Software called RuntimeError
    procedure HandleRunError(var continue: boolean);
    procedure FreeDebugThread;
    procedure Do_Thread_BreakStateChanged(Sender: TFpDbgBreakpoint; ANewState: TFpDbgBreakpointState);
    procedure DoBreakStateChanged(Data: PtrInt);

    procedure FDbgControllerHitBreakpointEvent(var continue: boolean;
      const Breakpoint: TFpDbgBreakpoint; AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
    procedure EnterPause(ALocationAddr: TDBGLocationRec; AnInternalPause: Boolean = False);
    procedure FDbgControllerCreateProcessEvent(var {%H-}continue: boolean);
    procedure FDbgControllerProcessExitEvent(AExitCode: DWord);
    procedure FDbgControllerExceptionEvent(var continue: boolean; const ExceptionClass, ExceptionMessage: string);
    procedure FDbgControllerDebugInfoLoaded(Sender: TObject);
    procedure FDbgControllerLibraryLoaded(var continue: boolean; ALibraries: TDbgLibraryArr);
    procedure FDbgControllerLibraryUnloaded(var continue: boolean; ALibraries: TDbgLibraryArr);
    function GetDebugInfo: TDbgInfo;
  protected
    procedure GetCurrentThreadAndStackFrame(out AThreadId, AStackFrame: Integer);
    function GetContextForEvaluate(const ThreadId, StackFrame: Integer): TFpDbgSymbolScope;

    function CreateMemReader: TDbgMemReader; virtual;
    function CreateMemConverter: TFpDbgMemConvertor; virtual;
    function CreateMemManager: TFpDbgMemManager; virtual;
    function CreateMemModel: TFpDbgMemModel; virtual;
    function CreateLineInfo: TDBGLineInfo; override;
    function CreateWatches: TWatchesSupplier; override;
    function CreateThreads: TThreadsSupplier; override;
    function CreateLocals: TLocalsSupplier; override;
    function  CreateRegisters: TRegisterSupplier; override;
    function CreateCallStack: TCallStackSupplier; override;
    function CreateDisassembler: TDBGDisassembler; override;
    function CreateBreakPoints: TDBGBreakPoints; override;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const;
                             const ACallback: TMethod): Boolean; override;
    function ChangeFileName: Boolean; override;

    // On Linux, communication with the debuggee is only allowed from within
    // the thread that created the debuggee. So a method to execute functions
    // within the debug-thread is necessary.
    function  ExecuteInDebugThread(AMethod: TFpDbgAsyncMethod): boolean;
    procedure StartDebugLoop(AState: TDBGState = dsRun);
    procedure DebugLoopFinished({%H-}Data: PtrInt);
    (* Any item that requests a QuickPause must be called from RunQuickPauseTasks
       A QuickPause may skip changing the debugger.State.
    *)
    procedure QuickPause;
    procedure RunQuickPauseTasks(AForce: Boolean = False);
    procedure DoRelease; override;
    procedure CheckAndRunIdle;
    procedure DoBeforeState(const OldState: TDBGState); override;
    procedure DoState(const OldState: TDBGState); override;
    function GetIsIdle: Boolean; override;
    function GetCommands: TDBGCommands; override;

  protected
    // Helper vars to run in debug-thread
    FSuspendedThreads: TThreadIdList;
    FCallStackEntryListThread: TDbgThread;
    FCallStackEntryListFrameRequired: Integer;
    procedure DoAddBreakFuncLib;
    procedure DoAddBreakLocation;
    procedure DoReadData;
    procedure DoReadPartialData;
    procedure DoFindContext;
    procedure DoSetStackFrameForBasePtr;
    //
    function AddBreak(const ALocation: TDbgPtr; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function AddBreak(const AFuncName: String; ALib: TDbgLibrary = nil; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; inline;
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData; out ABytesRead: Cardinal): Boolean; inline;
    function ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean;
    function SetStackFrameForBasePtr(ABasePtr: TDBGPtr; ASearchAssert: boolean = False;
      CurAddr: TDBGPtr = 0): TDBGPtr;
    function  FindSymbolScope(AThreadId, AStackFrame: Integer): TFpDbgSymbolScope; inline;
    procedure StopAllWorkers(AWait: Boolean = False);
    function IsPausedAndValid: boolean; // ready for eval watches/stack....

    procedure DoProcessMessages;
    property DebugInfo: TDbgInfo read GetDebugInfo;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    procedure LockCommandProcessing; override;
    procedure UnLockCommandProcessing; override;
    function GetLocationRec(AnAddress: TDBGPtr=0; AnAddrOffset: Integer = 0): TDBGLocationRec;
    function GetLocation: TDBGLocationRec; override;

    procedure ThreadHandleBreakPointInCallRoutine(AnAddress: TDBGPtr; out ACanContinue: Boolean);
    procedure BeforeWatchEval(ACallContext: TFpDbgInfoCallContext); override;
    procedure RunProcessLoop(OnlyCurrentThread: Boolean); override;
    procedure SetConsoleWinPos(ALeft, ATop: Integer); override;
    procedure UnSetConsoleWinPos; override;
    procedure SetConsoleWinSize(AWidth, AHeight: Integer); override;
    procedure UnSetConsoleWinSize; override;
    procedure SetConsoleWinBuffer(AColumns, ARows: Integer); override;
    procedure UnSetConsoleWinBuffer; override;

    class function Caption: String; override;
    class function NeedsExePath: boolean; override;
    class function RequiredCompilerOpts({%H-}ATargetCPU, {%H-}ATargetOS: String): TDebugCompilerRequirements; override;
    class function CreateProperties: TDebuggerProperties; override;
    class function  GetSupportedCommands: TDBGCommands; override;
    class function SupportedCommandsFor(AState: TDBGState): TDBGCommands; override;
    class function SupportedFeatures: TDBGFeatures; override;
  end;

  { TFpLineInfo }

  TFpLineInfo = class(TDBGLineInfo) //class(TGDBMILineInfo)
  private
    FRequestedSources: TStringListUTF8Fast;
  protected
    function  FpDebugger: TFpDebugDebugger;
    procedure DoStateChange(const {%H-}AOldState: TDBGState); override;
    procedure ClearSources;
    procedure DebugInfoChanged(ADbgInstance: TDbgInstance);
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    function Count: Integer; override;
    function HasAddress(const AIndex: Integer; const ALine: Integer): Boolean; override;
    function GetInfo({%H-}AAddress: TDbgPtr; out {%H-}ASource, {%H-}ALine, {%H-}AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
    procedure Cancel(const {%H-}ASource: String); override;
  end;

  { TFPWatches }

  TFPWatches = class(TWatchesSupplier)
  protected
    FWatchEvalWorkers: TFpDbgDebggerThreadWorkerLinkedList;
    function  FpDebugger: TFpDebugDebugger;
    procedure StopWorkes;
    procedure DoStateLeavePause; override;
    procedure InternalRequestData(AWatchValue: IDbgWatchValueIntf); override;
  public
    destructor Destroy; override;
  end;

  { TFPCallStackSupplier }

  TFPCallStackSupplier = class(TCallStackSupplier)
  private
    FPrettyPrinter: TFpPascalPrettyPrinter;
    FInitialFrame: Integer;
    FThreadForInitialFrame: Integer;
    FCallStackWorkers: TFpDbgDebggerThreadWorkerLinkedList;
  protected
    function  FpDebugger: TFpDebugDebugger;
    procedure StopWorkes;
    procedure DoStateLeavePause; override;
  public
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    procedure RequestCount(ACallstack: TCallStackBase); override;
    procedure RequestAtLeastCount(ACallstack: TCallStackBase;
      ARequiredMinCount: Integer); override;
    procedure RequestEntries(ACallstack: TCallStackBase); override;
    procedure RequestCurrent(ACallstack: TCallStackBase); override;
    procedure UpdateCurrentIndex; override;
  end;

  { TFPLocals }

  TFPLocals = class(TLocalsSupplier)
  protected
    FLocalWorkers: TFpDbgDebggerThreadWorkerLinkedList;
    function  FpDebugger: TFpDebugDebugger;
    procedure StopWorkes;
    procedure DoStateLeavePause; override;
  public
    destructor Destroy; override;
    procedure RequestData(ALocals: IDbgLocalsListIntf); override;
  end;

  { TFPRegisters }

  TFPRegisters = class(TRegisterSupplier)
  private
    FThr: TDbgThread;
    FRegisterList: TDbgRegisterValueList;
    procedure GetRegisterValueList();
  public
    procedure RequestData(ARegisters: TRegisters); override;
  end;

  { TFPThreads }

  TFPThreads = class(TThreadsSupplier)
  protected
    FThreadWorkers: TFpDbgDebggerThreadWorkerLinkedList;
    procedure DoStateEnterPause; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure StopWorkes;
    procedure DoStateLeavePause; override;
    procedure RequestEntries;  // Only fill the list, no data for entries yet
  public
    destructor Destroy; override;
    procedure RequestMasterData; override;
    procedure ChangeCurrentThread(ANewId: Integer); override;
    procedure SetSuspended(AThread: TThreadEntry; ASuspended: Boolean); override;
  end;

  { TFPDBGDisassembler }

  TFPDBGDisassembler = class(TDBGDisassembler)
  private
    FInPrepare: boolean;
  protected
    function PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean; override;
  end;

  { TFPBreakpoint }

  TFPBreakpoint = class(TDBGBreakPoint)
  private
    FThreadWorker: TFpThreadWorkerBreakPoint;
    FSetBreakFlag: boolean;
    FResetBreakFlag: boolean;
    FLocationChanged: boolean;
    FInternalBreakpoint: FpDbgClasses.TFpDbgBreakpoint;
    FIsSet: boolean;
    FBrkLogStackLimit: Integer;
    FBrkLogStackResult: array of String;
    FBrkLogExpr, FBrkLogResult: String;
    FNeedCheckChangeFlags: boolean;
    procedure MaybeAbortWorker(AWait: Boolean = false);
    procedure SetBreak;
    procedure ResetBreak;
    procedure ThreadLogExpression;
    procedure ThreadLogCallStack;
  protected
    procedure DoLogExpression(const AnExpression: String); override;
    procedure DoLogCallStack(const Limit: Integer); override;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure DoPropertiesChanged(AChanged: TDbgBpChangeIndicators); override;
    procedure DoChanged; override;
    procedure DoEndUpdate; override;
    procedure CheckChangeFlags;
    property  Validity: TValidState write SetValid;
  public
    destructor Destroy; override;
  end;

  { TFPBreakpoints }

  TFPBreakpoints = class(TDBGBreakPoints)
  public
    function Find(AIntBReakpoint: FpDbgClasses.TFpDbgBreakpoint): TDBGBreakPoint;
  end;

procedure Register;

implementation

uses
  FpDbgDisasX86;

var
  DBG_VERBOSE, DBG_WARNINGS, DBG_BREAKPOINTS, FPDBG_COMMANDS: PLazLoggerLogGroup;

type

  { TFpDbgMemReader }

  TFpDbgMemReader = class(TDbgMemReader)
  private
    FFpDebugDebugger: TFpDebugDebugger;
    FRegNum: Cardinal;
    FRegValue: TDbgPtr;
    FRegContext: TFpDbgLocationContext;
    FRegResult: Boolean;
    procedure DoReadRegister;
    procedure DoRegisterSize;
  protected
    function GetDbgProcess: TDbgProcess; override;
    function GetDbgThread(AContext: TFpDbgLocationContext): TDbgThread; override;
  public
    constructor create(AFpDebugDebuger: TFpDebugDebugger);
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override; overload;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer;
      out ABytesRead: Cardinal): Boolean; override; overload;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr;
      AContext: TFpDbgLocationContext): Boolean; override;
    function RegisterSize(ARegNum: Cardinal): Integer; override;
    //WriteMemory is not overwritten. It must ONLY be called in the debug-thread
  end;

  { TFpWaitForConsoleOutputThread }

  TFpWaitForConsoleOutputThread = class(TThread)
  private
    FFpDebugDebugger: TFpDebugDebugger;
    FHasConsoleOutputQueued: PRTLEvent;
    procedure DoHasConsoleOutput(Data: PtrInt);
  public
    constructor Create(AFpDebugDebugger: TFpDebugDebugger);
    destructor Destroy; override;
    procedure Execute; override;
  end;


procedure Register;
begin
  //RegisterDebugger(TFpDebugDebugger);
end;

{ TFpDebugExceptionStepping.TFrameList }

procedure TFpDebugExceptionStepping.TFrameList.RemoveOutOfScopeFrames(
  const ACurFrame: TDbgPtr);
var
  i: Integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    if Items[i] < ACurFrame then
      Delete(i);
    dec(i);
  end;
end;

{ TFpThreadWorkerModifyUpdate }

procedure TFpThreadWorkerModifyUpdate.DoCallback_DecRef(Data: PtrInt);
begin
  if not Success then begin
    if Assigned(FDebugger.OnFeedback) then
      FDebugger.OnFeedback(self, 'Failed to modify value', '', ftError, [frOk]);
  end;
  //
  FDebugger.Locals.TriggerInvalidateLocalsValues;
  FDebugger.Watches.TriggerInvalidateWatchValues;
  FDebugger.CallStack.CurrentCallStackList.Clear;

  UnQueue_DecRef;
end;

{ TFpDbgDebggerThreadWorkerItemHelper }

function TFpDbgDebggerThreadWorkerItemHelper.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(FDebugger);
end;

{ TFpThreadWorkerRunLoopUpdate }

procedure TFpThreadWorkerRunLoopUpdate.LoopFinished_DecRef(Data: PtrInt);
var
  dbg: TFpDebugDebugger;
begin
  dbg := FpDebugger;
  UnQueue_DecRef;
  // self may now be invalid
  dbg.DebugLoopFinished(0);
end;

{ TFpThreadWorkerRunLoopAfterIdleUpdate }

procedure TFpThreadWorkerRunLoopAfterIdleUpdate.CheckIdleOrRun_DecRef(
  Data: PtrInt);
var
  WorkItem: TFpThreadWorkerRunLoopAfterIdleUpdate;
  c: LongInt;
begin
  FpDebugger.FWorkQueue.Lock;
  FpDebugger.DoProcessMessages;
  FpDebugger.CheckAndRunIdle;
  (* IdleThreadCount could (race condition) be to high.
     Then DebugHistory may loose ONE item. (only one working thread.
     Practically this is unlikely, since the thread had time to set
     the count, since the Lock started.
  *)
  c := FpDebugger.FWorkQueue.Count + FpDebugger.FWorkQueue.ThreadCount - FpDebugger.FWorkQueue.IdleThreadCount;
  FpDebugger.FWorkQueue.Unlock;

  if c = 0 then begin
    FPDebugger.DoProcessMessages;
    FpDebugger.StartDebugLoop;
  end
  else begin
    WorkItem := TFpThreadWorkerRunLoopAfterIdleUpdate.Create(FpDebugger);
    FpDebugger.FWorkQueue.PushItem(WorkItem);
    WorkItem.DecRef;
  end;
  UnQueue_DecRef;
end;

{ TFpThreadWorkerCallStackCountUpdate }

procedure TFpThreadWorkerCallStackCountUpdate.UpdateCallstack_DecRef(
  Data: PtrInt);
var
  CList: TDbgCallstackEntryList;
  dbg: TFpDebugDebugger;
begin
  // Runs in IDE thread (TThread.Queue)
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallStackCount.UpdateCallstack_DecRef: system.ThreadID = classes.MainThreadID');

  if (FCallstack <> nil) then begin
    FCallstack.RemoveFreeNotification(@DoCallstackFreed_DecRef);

    if (FThread = nil) then
      CList := nil
    else
      CList := FThread.CallStackEntryList;

    if CList <> nil then begin
      DebugLn((DBG_VERBOSE or DBG_WARNINGS) and (FRequiredMinCount < 0) and not CList.HasReadAllAvailableFrames, ['UpdateCallstack_DecRef: ERROR needed full count, but CList is not marked as AllAvail']);
      if (CList.HasReadAllAvailableFrames) or (FRequiredMinCount = -1) then begin
        FCallstack.Count := CList.Count;
        FCallstack.SetCountValidity(ddsValid);
      end
      else begin
        FCallstack.SetHasAtLeastCountInfo(ddsValid, CList.Count);
      end;

(* TODO: this creates a new worker in RequestEntries
   We need to get the entries directly, without issuing a request.
   Then set, address and (if avail) proc-name, line and unit. Arguments are not avail
*)
      //// save whatever we have to history // limit to reduce time
      //if StopRequested and (CList <> nil) then
      //  FCallstack.PrepareRange(0, Min(CList.Count, 10));
    end
    else begin
      FCallstack.SetCountValidity(ddsInvalid);
      FCallstack.SetHasAtLeastCountInfo(ddsInvalid);
    end;

    FCallstack := nil;
  end;

  dbg := FpDebugger;
  UnQueue_DecRef;
  TFPCallStackSupplier(dbg.CallStack).FCallStackWorkers.ClearFinishedWorkers;
end;

procedure TFpThreadWorkerCallStackCountUpdate.DoRemovedFromLinkedList;
begin
  UpdateCallstack_DecRef;  // This trigger PrepareRange => but that still needs to be exec in thread? (or wait for lock)
end;

procedure TFpThreadWorkerCallStackCountUpdate.DoCallstackFreed_DecRef(
  Sender: TObject);
begin
  // Runs in IDE thread (because it is called by FCallstack)
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallStackCount.DoCallstackFreed_DecRef: system.ThreadID = classes.MainThreadID');
  FCallstack := nil;
  RequestStop;
  UnQueue_DecRef;
end;

constructor TFpThreadWorkerCallStackCountUpdate.Create(
  ADebugger: TFpDebugDebugger; ACallstack: TCallStackBase;
  ARequiredMinCount: Integer);
var
  AThread: TDbgThread;
begin
  // Runs in IDE thread (TThread.Queue)
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallStackCount.Create: system.ThreadID = classes.MainThreadID');
  FCallstack := ACallstack;
  FCallstack.AddFreeNotification(@DoCallstackFreed_DecRef);
  if not ADebugger.FDbgController.CurrentProcess.GetThread(FCallstack.ThreadId, AThread) then
    ARequiredMinCount := -2;  // error
  inherited Create(ADebugger, ARequiredMinCount, AThread);
end;

//procedure TFpThreadWorkerCallStackCountUpdate.RemoveCallStack_DecRef;
//begin
//  // Runs in IDE thread (TThread.Queue)
//  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallStackCount.RemoveCallStack_DecRef: system.ThreadID = classes.MainThreadID');
//  RequestStop;
//  if (FCallstack <> nil) then begin
//    FCallstack.RemoveFreeNotification(@DoCallstackFreed_DecRef);
//    FCallstack := nil;
//  end;
//  UnQueue_DecRef;
//end;

{ TFpThreadWorkerCallEntryUpdate }

procedure TFpThreadWorkerCallEntryUpdate.DoCallstackFreed_DecRef(Sender: TObject
  );
begin
  // Runs in IDE thread (because it is called by FCallstack)
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallEntry.DoCallstackFreed_DecRef: system.ThreadID = classes.MainThreadID');
  FCallstack := nil;
  DoCallstackEntryFreed_DecRef(nil);
end;

procedure TFpThreadWorkerCallEntryUpdate.DoCallstackEntryFreed_DecRef(
  Sender: TObject);
begin
  // Runs in IDE thread (because it is called by FCallstack)
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallEntry.DoCallstackEntryFreed_DecRef: system.ThreadID = classes.MainThreadID');
  FCallstackEntry := nil;
  RequestStop;
  UnQueue_DecRef;
end;

procedure TFpThreadWorkerCallEntryUpdate.DoRemovedFromLinkedList;
begin
  UpdateCallstackEntry_DecRef;
end;

procedure TFpThreadWorkerCallEntryUpdate.UpdateCallstackEntry_DecRef(
  Data: PtrInt);
var
  dbg: TFpDebugDebugger;
  c: String;
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallEntry.UpdateCallstackEntry_DecRef: system.ThreadID = classes.MainThreadID');

  if FCallstack <> nil then
    FCallstack.RemoveFreeNotification(@DoCallstackFreed_DecRef);

  if FCallstackEntry <> nil then begin
    FCallstackEntry.RemoveFreeNotification(@DoCallstackEntryFreed_DecRef);

    if FCallstackEntry.Validity = ddsRequested then begin
      if not FValid then
        FCallstackEntry.Validity := ddsInvalid
      else begin
        c := FSrcClassName;
        if c <> '' then
          c := c + '.';
        FCallstackEntry.Init(FAnAddress, nil, c + FFunctionName + FParamAsString,
          ExtractFileName(FSourceFile), FSourceFile, FLine, ddsValid);
      end;
    end;

    if FCallstack <> nil then
      FCallstack.DoEntriesUpdated;
  end;
  FCallstack := nil;
  FCallstackEntry := nil;

  dbg := FpDebugger;
  UnQueue_DecRef;
  TFPCallStackSupplier(dbg.CallStack).FCallStackWorkers.ClearFinishedWorkers;
end;

constructor TFpThreadWorkerCallEntryUpdate.Create(
  ADebugger: TFpDebugDebuggerBase; AThread: TDbgThread;
  ACallstackEntry: TCallStackEntry; ACallstack: TCallStackBase);
begin
  // Runs in IDE thread (TThread.Queue)
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallEntry.Create: system.ThreadID = classes.MainThreadID');
  FCallstack := ACallstack;
  if FCallstack <> nil then
    FCallstack.AddFreeNotification(@DoCallstackFreed_DecRef);

  FCallstackEntry := ACallstackEntry;
  FCallstackEntry.AddFreeNotification(@DoCallstackEntryFreed_DecRef);
  FCallstackIndex := FCallstackEntry.Index;

  inherited Create(ADebugger, ACallstackEntry.Index+1, AThread);
end;

//procedure TFpThreadWorkerCallEntryUpdate.RemoveCallStackEntry_DecRef;
//begin
//  // Runs in IDE thread (TThread.Queue)
//  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerCallEntry.RemoveCallStackEntry_DecRef: system.ThreadID = classes.MainThreadID');
//  RequestStop;
//  if FCallstack <> nil then begin
//    FCallstack.RemoveFreeNotification(@DoCallstackFreed_DecRef);
//    FCallstack := nil;
//  end;
//  if (FCallstackEntry <> nil) then begin
//    FCallstackEntry.RemoveFreeNotification(@DoCallstackEntryFreed_DecRef);
//    FCallstackEntry := nil;
//  end;
//  UnQueue_DecRef;
//end;

{ TFpThreadWorkerThreadsUpdate }

procedure TFpThreadWorkerThreadsUpdate.UpdateThreads_DecRef(Data: PtrInt);
var
  Threads: TThreadsSupplier;
  ThreadArray: TFPDThreadArray;
  i, j: Integer;
  CallStack: TDbgCallstackEntryList;
  t, n: TThreadEntry;
  FpThr: TDbgThread;
  c: TDbgCallstackEntry;
  dbg: TFpDebugDebuggerBase;
  ThrState: TDbgThreadState;
begin
  Threads := FDebugger.Threads;

  if (Threads.CurrentThreads <> nil) then begin
    ThreadArray := FpDebugger.FDbgController.CurrentProcess.GetThreadArray;
    for i := 1 to high(ThreadArray) do begin
      j := i;
      while (j > 0) and (ThreadArray[j].Num < ThreadArray[j-1].Num) do begin
        FpThr := ThreadArray[j];
        ThreadArray[j] := ThreadArray[j-1];
        ThreadArray[j-1] := FpThr;
        dec(j);
      end;
    end;
    for i := 0 to high(ThreadArray) do begin
      FpThr := ThreadArray[i];
      ThrState := dtsPaused;
      if FpDebugger.FSuspendedThreads.IndexOf(FpThr.ID) >= 0 then
        ThrState := dtsSuspended;

      CallStack := FpThr.CallStackEntryList;
      t := Threads.CurrentThreads.EntryById[FpThr.ID];
      if Assigned(CallStack) and (CallStack.Count > 0) then begin
        c := CallStack.Items[0];
        if t = nil then begin
          n := Threads.CurrentThreads.CreateEntry(c.AnAddress, nil, c.FunctionName, c.SourceFile, '', c.Line, FpThr.ID, FpThr.ID, FpThr.Name, ThrState, ddsValid, FpThr.Num);
          Threads.CurrentThreads.Add(n);
          n.Free;
        end
        else
          t.Init(c.AnAddress, nil, c.FunctionName, c.SourceFile, '', c.Line, FpThr.ID, FpThr.ID, FpThr.Name, ThrState, ddsValid, FpThr.Num);
      end
      else begin
        if t = nil then begin
          n := Threads.CurrentThreads.CreateEntry(0, nil, '', '', '', 0, FpThr.ID, FpThr.ID, FpThr.Name, ThrState, ddsValid, FpThr.Num);
          Threads.CurrentThreads.Add(n);
          n.Free;
        end
        else
          t.Init(0, nil, '', '', '', 0, FpThr.ID, FpThr.ID, FpThr.Name, ThrState, ddsValid, FpThr.Num);
      end;
    end;

    Threads.CurrentThreads.SetValidity(ddsValid);
  end;

  dbg := FDebugger;
  UnQueue_DecRef;
  TFPThreads(dbg.Threads).FThreadWorkers.ClearFinishedWorkers;
end;

{ TFpThreadWorkerLocalsUpdate }

procedure TFpThreadWorkerLocalsUpdate.UpdateLocals_DecRef(Data: PtrInt);
var
  dbg: TFpDebugDebugger;
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerLocals.UpdateLocals_DecRef: system.ThreadID = classes.MainThreadID');

  if FLocals <> nil then begin
    FLocals.Validity := ddsValid;
    FLocals.EndUpdate;
    FLocals := nil;
  end;

  dbg := FpDebugger;
  UnQueue_DecRef;
  TFPLocals(dbg.Locals).FLocalWorkers.ClearFinishedWorkers;
end;

procedure TFpThreadWorkerLocalsUpdate.DoRemovedFromLinkedList;
begin
  if FLocals <> nil then begin
    if FHasQueued = hqQueued then begin
      UpdateLocals_DecRef;
      exit;
    end
    else begin
      FLocals.Validity := ddsInvalid;
      FLocals.EndUpdate;
    end;
    FLocals := nil;
  end;
  UnQueue_DecRef;
end;

constructor TFpThreadWorkerLocalsUpdate.Create(ADebugger: TFpDebugDebuggerBase;
  ALocals: IDbgLocalsListIntf);
begin
  // Runs in IDE thread (TThread.Queue)
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerLocals.Create: system.ThreadID = classes.MainThreadID');
  FLocals := ALocals;
  FLocals.BeginUpdate;
  FThreadId := ALocals.ThreadId;
  FStackFrame := ALocals.StackFrame;
  inherited Create(ADebugger, twpLocal);
end;

{ TFpThreadWorkerWatchValueEvalUpdate }

procedure TFpThreadWorkerWatchValueEvalUpdate.DoWachCanceled(
  Sender: IDbgDataRequestIntf; Data: TDbgDataRequestEventData);
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerWatchValueEvalUpdate.DoWachCanceled: system.ThreadID = classes.MainThreadID');
  RequestStop;
  UnQueue_DecRef;
  if IsCancelled then begin
  ///
  end;
end;

procedure TFpThreadWorkerWatchValueEvalUpdate.UpdateWatch_DecRef(Data: PtrInt);
var
  dbg: TFpDebugDebuggerBase;
  w: IDbgWatchValueIntf;
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerWatchValueEval.UpdateWatch_DecRef: system.ThreadID = classes.MainThreadID');

  if FWatchValue <> nil then begin
    w := FWatchValue;
    FWatchValue := nil;
    w.RemoveNotification(weeCancel, @DoWachCanceled);
    w.EndUpdate;
  end;

  dbg := FDebugger;
  UnQueue_DecRef;
  TFPWatches(dbg.Watches).FWatchEvalWorkers.ClearFinishedWorkers;
end;

procedure TFpThreadWorkerWatchValueEvalUpdate.DoRemovedFromLinkedList;
var
  w: IDbgWatchValueIntf;
begin
  if FWatchValue <> nil then begin
    w := FWatchValue;
    FWatchValue := nil;
    w.RemoveNotification(weeCancel, @DoWachCanceled);
    if w.Validity = ddsRequested then
      w.Validity :=  ddsInvalid;
    w.EndUpdate;
  end;
  UnQueue_DecRef;
end;

constructor TFpThreadWorkerWatchValueEvalUpdate.Create(
  ADebugger: TFpDebugDebuggerBase; AWatchValue: IDbgWatchValueIntf);
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerWatchValueEval.Create: system.ThreadID = classes.MainThreadID');
  FWatchValue := AWatchValue;
  FWatchValue.BeginUpdate;
  FWatchValue.AddNotification(weeCancel, @DoWachCanceled);

  inherited Create(ADebugger, twpWatch, FWatchValue.Expression, FWatchValue.StackFrame, FWatchValue.ThreadId,
    FWatchValue.RepeatCount, FWatchValue.EvaluateFlags);
end;

{ TFpThreadWorkerBreakPointSetUpdate }

procedure TFpThreadWorkerBreakPointSetUpdate.UpdateBrkPoint_DecRef(Data: PtrInt
  );
var
  WorkItem: TFpThreadWorkerBreakPointRemoveUpdate;
begin
  assert(system.ThreadID = classes.MainThreadID, 'TFpThreadWorkerBreakPointSetUpdate.UpdateBrkPoint_DecRef: system.ThreadID = classes.MainThreadID');

  if FDbgBreakPoint <> nil then begin
    assert(FDbgBreakPoint.FThreadWorker = Self, 'TFpThreadWorkerBreakPointSetUpdate.UpdateBrkPoint_DecRef: FDbgBreakPoint.FThreadWorker = Self');
    FDbgBreakPoint.FThreadWorker := nil;
    DecRef;
  end
  else
    FResetBreakPoint := 1;

  if FResetBreakPoint <> 0 then begin
    if InternalBreakpoint <> nil then begin
      WorkItem := TFpThreadWorkerBreakPointRemoveUpdate.Create(FDebugger, InternalBreakpoint);
      FpDebugger.FWorkQueue.PushItem(WorkItem);
      WorkItem.DecRef;
    end;
  end
  else
  if FDbgBreakPoint <> nil then begin
    assert(FDbgBreakPoint.FInternalBreakpoint = nil, 'TFpThreadWorkerBreakPointSetUpdate.UpdateBrkPoint_DecRef: FDbgBreakPoint.FInternalBreakpoint = nil');
    FDbgBreakPoint.FInternalBreakpoint := InternalBreakpoint;
    if not assigned(InternalBreakpoint) then
      FDbgBreakPoint.Validity := vsInvalid // pending?
    else begin
      case InternalBreakpoint.State of
        bksUnknown: FDbgBreakPoint.Validity := vsUnknown;
        bksOk:      FDbgBreakPoint.Validity := vsValid;
        bksFailed:  FDbgBreakPoint.Validity := vsInvalid;
        bksPending: FDbgBreakPoint.Validity := vsPending;
      end;
      InternalBreakpoint.On_Thread_StateChange := @TFpDebugDebugger(FDebugger).Do_Thread_BreakStateChanged;
    end;
  end;

  UnQueue_DecRef;
end;

constructor TFpThreadWorkerBreakPointSetUpdate.Create(
  ADebugger: TFpDebugDebuggerBase; ADbgBreakPoint: TFPBreakpoint);
var
  CurThreadId, CurStackFrame: Integer;
begin
  FDbgBreakPoint := ADbgBreakPoint;
  case ADbgBreakPoint.Kind of
    bpkAddress: inherited Create(ADebugger, ADbgBreakPoint.Address);
    bpkSource:  inherited Create(ADebugger, ADbgBreakPoint.Source, ADbgBreakPoint.Line);
    bpkData: begin
      TFpDebugDebugger(ADebugger).GetCurrentThreadAndStackFrame(CurThreadId, CurStackFrame);
      inherited Create(ADebugger, ADbgBreakPoint.WatchData, ADbgBreakPoint.WatchScope,
        ADbgBreakPoint.WatchKind, CurStackFrame, CurThreadId);
    end;
  end;
end;

procedure TFpThreadWorkerBreakPointSetUpdate.AbortSetBreak;
begin
  InterLockedExchange(FResetBreakPoint, 1);
  FDbgBreakPoint := nil;
end;

{ TFpThreadWorkerBreakPointRemoveUpdate }

procedure TFpThreadWorkerBreakPointRemoveUpdate.DoUnQueued;
begin
  if FInternalBreakpoint = nil then
    exit;
  FInternalBreakpoint.FreeByDbgProcess := True;
  inherited DoUnQueued;
end;

constructor TFpThreadWorkerBreakPointRemoveUpdate.Create(
  ADebugger: TFpDebugDebuggerBase; ADbgBreakPoint: TFPBreakpoint);
begin
  inherited Create(ADebugger, ADbgBreakPoint.FInternalBreakpoint);
end;

{ TDbgControllerStepOverFirstFinallyLineCmd }

procedure TDbgControllerStepOverFirstFinallyLineCmd.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
begin
  Finished := (FThread.CompareStepInfo(0, True) <> dcsiSameLine) or
              (NextInstruction.IsReturnInstruction) or IsSteppedOut;

  if Finished then
    AnEvent := deFinishedStep
  else
  if AnEvent in [deFinishedStep] then
    AnEvent:=deInternalContinue;
end;

{ TDbgControllerStepOverOrFinallyCmd }

procedure TDbgControllerStepOverOrFinallyCmd.InternalContinue(
  AProcess: TDbgProcess; AThread: TDbgThread);
var
  Instr: TDbgAsmInstruction;
begin
{
32bit
00000000004321D3 89E8                     mov eax,ebp
00000000004321D5 E866FEFFFF               call -$0000019A

64bit
00000001000374AE 4889C1                   mov rcx,rax
00000001000374B1 488D15D3FFFFFF           lea rdx,[rip-$0000002D]
00000001000374B8 4989E8                   mov rax,rbp
00000001000374BB E89022FEFF               call -$0001DD70
}
  if (AThread = FThread) then begin
    Instr := NextInstruction;

    if FFinState in [fsInFin] then begin
      if (TX86AsmInstruction(Instr).X86OpCode = OPmov) then begin
        if FProcess.Mode = dm32 then begin
          if CompareText(TX86AsmInstruction(Instr).X86Instruction.Operand[1].Value, 'EBP') = 0 then
            FFinState := fsDonePrologue;
        end
        else begin
          if CompareText(TX86AsmInstruction(Instr).X86Instruction.Operand[1].Value, 'RBP') = 0 then
            FFinState := fsDonePrologue;
        end;
      end;
    end
    else
    if FFinState in [fsNone, fsMov, fsCall] then begin
      if Instr is TX86AsmInstruction then begin
        case TX86AsmInstruction(Instr).X86OpCode of
          OPmov:
            if FProcess.Mode = dm32 then begin
              if CompareText(TX86AsmInstruction(Instr).X86Instruction.Operand[2].Value, 'EBP') = 0 then
                FFinState := fsMov;
            end
            else begin
              if CompareText(TX86AsmInstruction(Instr).X86Instruction.Operand[2].Value, 'RBP') = 0 then
                FFinState := fsMov;
            end;
          OPcall:
            if FFinState = fsMov then begin
              CheckForCallAndSetBreak;
              FProcess.Continue(FProcess, FThread, True); // Step into
              FFinState := fsCall;
              exit;
            end;
          else
            FFinState := fsNone;
        end;
      end;
    end;
  end;
  inherited InternalContinue(AProcess, AThread);
end;

procedure TDbgControllerStepOverOrFinallyCmd.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
var
  sym: TFpSymbol;
  InFin: Boolean;
  Instr: TDbgAsmInstruction;
begin
  if FFinState = fsCall then begin
    sym := FProcess.FindProcSymbol(FThread.GetInstructionPointerRegisterValue);
    InFin := pos('fin$', sym.Name) > 0;
    sym.ReleaseReference;

    if InFin then begin
      FFinState := fsInFin;

      FThread.StoreStepInfo; // Safety net. If line changes, prologue must be over. (fpc < 3.3.1 did have the prologue on the line number of the last linne of the finally)
      Finished := False;
      RemoveHiddenBreak;
      if AnEvent = deFinishedStep then
        AnEvent := deInternalContinue;
      exit;
    end
    else
      FFinState := fsNone;
  end
  else
  if FFinState in [fsDonePrologue, fsDonePrologue2] then begin
    if FFinState = fsDonePrologue then begin
      Instr := NextInstruction;
      if (TX86AsmInstruction(Instr).X86OpCode = OPlea) then begin
        if FProcess.Mode = dm32 then begin
          if CompareText(TX86AsmInstruction(Instr).X86Instruction.Operand[1].Value, 'ESP') = 0 then
            FFinState := fsDonePrologue2;
        end
        else begin
          if CompareText(TX86AsmInstruction(Instr).X86Instruction.Operand[1].Value, 'RSP') = 0 then
            FFinState := fsDonePrologue2;
        end;
      end;
      if FFinState = fsDonePrologue2 then begin
        inherited DoResolveEvent(AnEvent, AnEventThread, Finished);
        exit;
      end;
    end;
    Finished := True;
    AnEvent := deFinishedStep;
    exit;
  end;

  inherited DoResolveEvent(AnEvent, AnEventThread, Finished);
end;

{ TDbgControllerStepThroughFpcSpecialHandler }

procedure TDbgControllerStepThroughFpcSpecialHandler.DoResolveEvent(
  var AnEvent: TFPDEvent; AnEventThread: TDbgThread; out Finished: boolean);
begin
  AnEvent := deInternalContinue;
  Finished := False;
  if FInteralFinished then
    exit;

  if IsAtOrOutOfHiddenBreakFrame then
    RemoveHiddenBreak;

  FInteralFinished := IsSteppedOut or FDone or ((not HasHiddenBreak) and (NextInstruction.IsReturnInstruction));
  if FInteralFinished then begin
    RemoveHiddenBreak;
    Finished := FIsLeave;
    if Finished then
      AnEvent := deFinishedStep;
  end;
end;

procedure TDbgControllerStepThroughFpcSpecialHandler.InternalContinue(
  AProcess: TDbgProcess; AThread: TDbgThread);
begin
  if FInteralFinished then begin
    CallProcessContinue(False);
    exit;
  end;

  {$PUSH}{$Q-}{$R-}
  if (AThread = FThread) and
     (NextInstruction.IsCallInstruction) and
     (FThread.GetInstructionPointerRegisterValue + NextInstruction.InstructionLength = FAfterFinCallAddr)
  then begin
    RemoveHiddenBreak;
    FProcess.Continue(FProcess, FThread, True);
    FDone := True;
// TODO: last step => then single line step
    exit;
  end;
  {$POP}
  inherited InternalContinue(AProcess, AThread);
end;

procedure TDbgControllerStepThroughFpcSpecialHandler.Init;
begin
  InitStackFrameInfo;
  inherited Init;
end;

constructor TDbgControllerStepThroughFpcSpecialHandler.Create(
  AController: TDbgController; AnAfterFinCallAddr: TDbgPtr; AnIsLeave: Boolean);
begin
  FAfterFinCallAddr := AnAfterFinCallAddr;
  FIsLeave := AnIsLeave;
  inherited Create(AController);
end;

{ TFpDebugExceptionStepping.TAddressFrameList }

function TFpDebugExceptionStepping.TAddressFrameList.Add(
  const AnAddress: TDbgPtr): TFrameList;
begin
  Result := TFrameList.Create;
  inherited Add(AnAddress, Result);
end;

function TFpDebugExceptionStepping.TAddressFrameList.Add(const AnAddress,
  AFrame: TDbgPtr): boolean;
var
  i: Integer;
  Frames: TFrameList;
begin
  if AFrame < FLastRemoveCheck then
    FLastRemoveCheck := 0;
  Result := False;
  i := IndexOf(AnAddress);

  if i >= 0 then
    Frames := Data[i]
  else
    Frames := Add(AnAddress);

  Result := IndexOf(AFrame) >= 0;
  if Result then
    exit;
  Frames.Add(AFrame);
end;

function TFpDebugExceptionStepping.TAddressFrameList.Remove(const AnAddress,
  AFrame: TDbgPtr): boolean;
var
  i: Integer;
  Frames: TFrameList;
begin
  i := IndexOf(AnAddress);
  Result := i < 0;
  if Result then
    exit;

  Frames := Data[i];
  Frames.Remove(AFrame);
  Result := Frames.Count = 0;
  if Result then
    Delete(i);
end;

procedure TFpDebugExceptionStepping.TAddressFrameList.RemoveOutOfScopeFrames(
  const ACurFrame: TDbgPtr; ABreakPoint: TFpDbgBreakpoint);
begin
  if ACurFrame = FLastRemoveCheck then
    exit;
  DoRemoveOutOfScopeFrames(ACurFrame, ABreakPoint);
end;

procedure TFpDebugExceptionStepping.TAddressFrameList.DoRemoveOutOfScopeFrames(
  const ACurFrame: TDbgPtr; ABreakPoint: TFpDbgBreakpoint);
var
  i: Integer;
  f: TFrameList;
begin
  FLastRemoveCheck := ACurFrame;

  i := Count - 1;
  while i >= 0 do begin
    f := Data[i];
    f.RemoveOutOfScopeFrames(ACurFrame);
    if f.Count = 0 then begin
      ABreakPoint.RemoveAddress(Keys[i]);
      Delete(i);
    end;
    dec(i);
  end;
end;

{ TFPThreads }

procedure TFPThreads.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  Changed;
end;

procedure TFPThreads.DoStateChange(const AOldState: TDBGState);
begin
  inherited DoStateChange(AOldState);
  if (Debugger.State in [dsPause, dsInternalPause]) then // Make sure we have threads first // this can be removed, once threads are KEPT between pauses
    RequestEntries;
end;

procedure TFPThreads.StopWorkes;
begin
  FThreadWorkers.RequestStopForWorkers;
end;

procedure TFPThreads.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  FThreadWorkers.WaitForWorkers(True);
end;

procedure TFPThreads.RequestEntries;
var
  ThreadArray: TFPDThreadArray;
  i, j: Integer;
  ThreadEntry: TThreadEntry;
  FpThr: TDbgThread;
begin
  if CurrentThreads = nil then exit;
  if Debugger = nil then Exit;
  if not TFpDebugDebugger(Debugger).IsPausedAndValid then exit;

  CurrentThreads.Clear;

  ThreadArray := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.GetThreadArray;
  for i := 1 to high(ThreadArray) do begin
    j := i;
    while (j > 0) and (ThreadArray[j].Num < ThreadArray[j-1].Num) do begin
      FpThr := ThreadArray[j];
      ThreadArray[j] := ThreadArray[j-1];
      ThreadArray[j-1] := FpThr;
      dec(j);
    end;
  end;
  for i := 0 to high(ThreadArray) do begin
    // TODO: Maybe get the address. If FpDebug has already read the ThreadState.
    if TFpDebugDebugger(Debugger).FSuspendedThreads.IndexOf(ThreadArray[i].ID) < 0 then
      ThreadEntry := CurrentThreads.CreateEntry(0, nil, '', '', '', 0, ThreadArray[i].ID, ThreadArray[i].ID, ThreadArray[i].Name, dtsPaused, ddsValid, ThreadArray[i].Num)
    else
      ThreadEntry := CurrentThreads.CreateEntry(0, nil, '', '', '', 0, ThreadArray[i].ID, ThreadArray[i].ID, ThreadArray[i].Name, dtsSuspended, ddsValid, ThreadArray[i].Num);
    try
      CurrentThreads.Add(ThreadEntry);
    finally
      ThreadEntry.Free;
    end;
  end;

  if TFpDebugDebugger(Debugger).FDbgController.CurrentThread = nil then
    CurrentThreads.CurrentThreadId := 0 // TODO: only until controller is guranteed to have a currentthread
  else
    CurrentThreads.CurrentThreadId := TFpDebugDebugger(Debugger).FDbgController.CurrentThread.ID;
  // Do NOT set validity // keep ddsUnknown;
end;

destructor TFPThreads.Destroy;
begin
  FThreadWorkers.WaitForWorkers(True);
  inherited Destroy;
end;

procedure TFPThreads.RequestMasterData;
var
  WorkItem: TFpThreadWorkerThreadsUpdate;
begin
  if CurrentThreads = nil then exit;
  if Debugger = nil then Exit;

  if not (Debugger.State in [dsPause, dsInternalPause {, dsRun}]) then begin  // Make sure we have threads first // this can be removed, once threads are KEPT between pauses
    CurrentThreads.Clear;
    Exit;
  end;

  WorkItem := TFpThreadWorkerThreadsUpdate.Create(TFpDebugDebugger(Debugger));
  TFpDebugDebugger(Debugger).FWorkQueue.PushItem(WorkItem);
  FThreadWorkers.Add(WorkItem);
end;

procedure TFPThreads.ChangeCurrentThread(ANewId: Integer);
begin
  inherited ChangeCurrentThread(ANewId);
  if not(Debugger.State in [dsPause, dsInternalPause]) then exit;

  TFpDebugDebugger(Debugger).FDbgController.CurrentThreadId := ANewId;
  if CurrentThreads <> nil then
    CurrentThreads.CurrentThreadId := ANewId;
  Changed;
end;

procedure TFPThreads.SetSuspended(AThread: TThreadEntry; ASuspended: Boolean);
var
  FpThread: TDbgThread;
begin
  //inherited SetSuspended(AThreadId, ASuspended);
  if (AThread = nil) or (TFpDebugDebugger(Debugger).State <> dsPause) then
    exit;

  if (not TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.GetThread(AThread.ThreadId, FpThread)) or
     (FpThread = nil)
  then
    exit;

  if ASuspended then begin
    if TFpDebugDebugger(Debugger).FSuspendedThreads.IndexOf(AThread.ThreadId) < 0 then begin
      TFpDebugDebugger(Debugger).FSuspendedThreads.Add(AThread.ThreadId);
      FpThread.IncSuspendCount;
      AThread.SetThreadStateOnly(dtsSuspended);
    end;
  end
  else begin
    if TFpDebugDebugger(Debugger).FSuspendedThreads.IndexOf(AThread.ThreadId) >= 0 then begin
      TFpDebugDebugger(Debugger).FSuspendedThreads.Remove(AThread.ThreadId);
      FpThread.DecSuspendCount;
      AThread.SetThreadStateOnly(dtsPaused);
    end;
  end;

  Changed;
end;

{ TFpWaitForConsoleOutputThread }

procedure TFpWaitForConsoleOutputThread.DoHasConsoleOutput(Data: PtrInt);
var
  s: string;
begin
  if (Data=0) or assigned(TFpDebugDebugger(Data).FConsoleOutputThread) then
  begin
    s := FFpDebugDebugger.FDbgController.CurrentProcess.GetConsoleOutput;
    RTLeventSetEvent(FHasConsoleOutputQueued);
    if Assigned(FFpDebugDebugger.OnConsoleOutput) then
      FFpDebugDebugger.OnConsoleOutput(self, s);
  end;
end;

constructor TFpWaitForConsoleOutputThread.Create(AFpDebugDebugger: TFpDebugDebugger);
begin
  Inherited create(false);
  FHasConsoleOutputQueued := RTLEventCreate;
  FFpDebugDebugger := AFpDebugDebugger;
end;

destructor TFpWaitForConsoleOutputThread.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  RTLeventdestroy(FHasConsoleOutputQueued);
  inherited Destroy;
end;

procedure TFpWaitForConsoleOutputThread.Execute;
var
  res: integer;
begin
  while not terminated do
  begin
    res := FFpDebugDebugger.FDbgController.CurrentProcess.CheckForConsoleOutput(100);
    if res<0 then
      Terminate
    else if res>0 then
    begin
      RTLeventResetEvent(FHasConsoleOutputQueued);
      Application.QueueAsyncCall(@DoHasConsoleOutput, PtrInt(FFpDebugDebugger));
      RTLeventWaitFor(FHasConsoleOutputQueued);
    end;
  end;
end;

{ TFpDbgMemReader }

function TFpDbgMemReader.GetDbgProcess: TDbgProcess;
begin
  result := FFpDebugDebugger.FDbgController.CurrentProcess;
end;

function TFpDbgMemReader.GetDbgThread(AContext: TFpDbgLocationContext): TDbgThread;
var
  Process: TDbgProcess;
begin
  Process := GetDbgProcess;
  if (AContext = nil) or not Process.GetThread(AContext.ThreadId, Result) then
    Result := FFpDebugDebugger.FDbgController.CurrentThread;
end;

procedure TFpDbgMemReader.DoReadRegister;
begin
  FRegResult := inherited ReadRegister(FRegNum, FRegValue, FRegContext);
end;

procedure TFpDbgMemReader.DoRegisterSize;
begin
  FRegValue := inherited RegisterSize(FRegNum);
end;

constructor TFpDbgMemReader.create(AFpDebugDebuger: TFpDebugDebugger);
begin
  FFpDebugDebugger := AFpDebugDebuger;
end;

function TFpDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  result := FFpDebugDebugger.ReadData(AnAddress, ASize, ADest^);
end;

function TFpDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ADest: Pointer; out ABytesRead: Cardinal): Boolean;
begin
  result := FFpDebugDebugger.ReadData(AnAddress, ASize, ADest^, ABytesRead);
end;

function TFpDbgMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Assert(AnAddressSpace>0,'TFpDbgMemReader.ReadMemoryEx ignores AddressSpace');
  result := FFpDebugDebugger.ReadData(AnAddress, ASize, ADest^);
end;

function TFpDbgMemReader.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr;
  AContext: TFpDbgLocationContext): Boolean;
begin
  // Shortcut, if in debug-thread / do not use Self.F*
  if ThreadID = FFpDebugDebugger.FWorkerThreadId then
    exit(inherited ReadRegister(ARegNum, AValue, AContext));

  FRegNum := ARegNum;
  FRegContext := AContext;
  FRegValue := 0; // TODO: error detection
  FFpDebugDebugger.ExecuteInDebugThread(@DoReadRegister);
  AValue := FRegValue;
  result := FRegResult;
end;

function TFpDbgMemReader.RegisterSize(ARegNum: Cardinal): Integer;
begin
  // Shortcut, if in debug-thread / do not use Self.F*
  if ThreadID = FFpDebugDebugger.FWorkerThreadId then
    exit(inherited RegisterSize(ARegNum));

  FRegNum := ARegNum;
  FFpDebugDebugger.ExecuteInDebugThread(@DoRegisterSize);
  result := FRegValue;
end;

{ TFPCallStackSupplier }

function TFPCallStackSupplier.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPCallStackSupplier.StopWorkes;
begin
  FCallStackWorkers.RequestStopForWorkers;
end;

procedure TFPCallStackSupplier.DoStateLeavePause;
begin
  FCallStackWorkers.WaitForWorkers(True);
  FInitialFrame := 0;
  FThreadForInitialFrame := 0;
  if (TFpDebugDebugger(Debugger).FDbgController <> nil) and
     (TFpDebugDebugger(Debugger).FDbgController.CurrentProcess <> nil)
  then
    TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.ThreadsClearCallStack;
  inherited DoStateLeavePause;
end;

constructor TFPCallStackSupplier.Create(const ADebugger: TDebuggerIntf);
begin
  inherited Create(ADebugger);
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
end;

destructor TFPCallStackSupplier.Destroy;
begin
  FCallStackWorkers.WaitForWorkers(True);
  inherited Destroy;
  FPrettyPrinter.Free;
end;

procedure TFPCallStackSupplier.RequestCount(ACallstack: TCallStackBase);
begin
  RequestAtLeastCount(ACallstack, -1);
end;

procedure TFPCallStackSupplier.RequestAtLeastCount(ACallstack: TCallStackBase;
  ARequiredMinCount: Integer);
var
  WorkItem: TFpThreadWorkerCallStackCountUpdate;
begin
  if not FpDebugger.IsPausedAndValid then begin
    ACallstack.SetCountValidity(ddsInvalid);
    exit;
  end;

  WorkItem := TFpThreadWorkerCallStackCountUpdate.Create(FpDebugger, ACallstack, ARequiredMinCount);
  FpDebugger.FWorkQueue.PushItem(WorkItem);
  FCallStackWorkers.Add(WorkItem);
end;

procedure TFPCallStackSupplier.RequestEntries(ACallstack: TCallStackBase);
var
  e: TCallStackEntry;
  It: TMapIterator;
  t: TDbgThread;
  WorkItem: TFpThreadWorkerCallEntryUpdate;
  i: Integer;
begin
  It := TMapIterator.Create(ACallstack.RawEntries);
  if not It.Locate(ACallstack.LowestUnknown )
  then if not It.EOM
  then It.Next;

  if not FpDebugger.IsPausedAndValid then begin
    while (not IT.EOM) and (TCallStackEntry(It.DataPtr^).Index <= ACallstack.HighestUnknown) do begin
      TCallStackEntry(It.DataPtr^).Validity := ddsInvalid;
      IT.Next;
    end;
    It.Free;
    exit;
  end;

  if not FpDebugger.FDbgController.CurrentProcess.GetThread(ACallstack.ThreadId, t) then
    t := nil;

  i := 0;
  while (not IT.EOM) and (TCallStackEntry(It.DataPtr^).Index <= ACallstack.HighestUnknown)
  do begin
    e := TCallStackEntry(It.DataPtr^);
    It.Next;
    inc(i);
    if e.Validity = ddsRequested then
    begin
      if t = nil then
        e.Validity := ddsInvalid
      else
      begin
        if IT.EOM or ((i and 31) = 0) or
           (e.Index = ACallstack.HighestUnknown)
        then
          WorkItem := TFpThreadWorkerCallEntryUpdate.Create(FpDebugger, t, e, ACallstack)
        else
          WorkItem := TFpThreadWorkerCallEntryUpdate.Create(FpDebugger, t, e);
        FpDebugger.FWorkQueue.PushItem(WorkItem);
        FCallStackWorkers.Add(WorkItem);
      end;
    end;
  end;
  It.Free;
end;

procedure TFPCallStackSupplier.RequestCurrent(ACallstack: TCallStackBase);
begin
  if (FThreadForInitialFrame <> 0) and (FThreadForInitialFrame = ACallstack.ThreadId) then begin
    ACallstack.CurrentIndex := FInitialFrame;
    FInitialFrame := 0;
    FThreadForInitialFrame := 0;
  end
  else
    ACallstack.CurrentIndex := 0;
  ACallstack.SetCurrentValidity(ddsValid);
end;

procedure TFPCallStackSupplier.UpdateCurrentIndex;
var
  tid, idx: Integer;
  cs: TCallStackBase;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    exit;
  end;

  tid := Debugger.Threads.CurrentThreads.CurrentThreadId;
  cs := TCallStackBase(CurrentCallStackList.EntriesForThreads[tid]);
  idx := cs.NewCurrentIndex;  // NEW-CURRENT

  if cs <> nil then begin
    cs.CurrentIndex := idx;
    cs.SetCurrentValidity(ddsValid);
  end;
end;

{ TFPLocals }

function TFPLocals.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPLocals.StopWorkes;
begin
  FLocalWorkers.RequestStopForWorkers;
end;

procedure TFPLocals.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  FLocalWorkers.WaitForWorkers(True);
end;

destructor TFPLocals.Destroy;
begin
  FLocalWorkers.WaitForWorkers(True);
  inherited Destroy;
end;

procedure TFPLocals.RequestData(ALocals: IDbgLocalsListIntf);
var
  WorkItem: TFpThreadWorkerLocalsUpdate;
begin
  if not FpDebugger.IsPausedAndValid then begin
    ALocals.Validity := ddsInvalid;
    exit;
  end;

  WorkItem := TFpThreadWorkerLocalsUpdate.Create(FpDebugger, ALocals);
  FLocalWorkers.Add(WorkItem);
  FpDebugger.FWorkQueue.PushItem(WorkItem);
end;

{ TFPBreakpoints }

function TFPBreakpoints.Find(AIntBReakpoint: FpDbgClasses.TFpDbgBreakpoint): TDBGBreakPoint;
var
  i: integer;
begin
  for i := 0 to count-1 do
    if TFPBreakpoint(Items[i]).FInternalBreakpoint=AIntBReakpoint then
      begin
      result := TFPBreakpoint(Items[i]);
      Exit;
      end;
  result := nil;
end;

procedure TFPBreakpoint.MaybeAbortWorker(AWait: Boolean);
begin
  if FThreadWorker <> nil then begin
    assert(FThreadWorker is TFpThreadWorkerBreakPointSetUpdate, 'TFPBreakpoint.ResetBreak: FThreadWorker is TFpThreadWorkerBreakPointSetUpdate');
    assert(FInternalBreakpoint = nil, 'TFPBreakpoint.ResetBreak: FInternalBreakpoint = nil');
    FThreadWorker.AbortSetBreak;
    if AWait then
      TFpDebugDebugger(Debugger).FWorkQueue.WaitForItem(FThreadWorker);
    FThreadWorker.DecRef;
    FThreadWorker := nil;
  end;
end;

procedure TFPBreakpoint.SetBreak;
begin
  debuglnEnter(DBG_BREAKPOINTS, ['>> TFPBreakpoint.SetBreak  ADD ',FSource,':',FLine,'/',dbghex(Address),' ' ]);
  //Wrong assert. If the editor is not locked and lines are removed, then breakpoints can move several times.
  //assert((FThreadWorker = nil) or ( (FThreadWorker is TFpThreadWorkerBreakPointSetUpdate) and (TFpThreadWorkerBreakPointSetUpdate(FThreadWorker).DbgBreakPoint = nil) ), 'TFPBreakpoint.SetBreak: (FThreadWorker = nil) or ( (FThreadWorker is TFpThreadWorkerBreakPointSetUpdate) and (TFpThreadWorkerBreakPointSetUpdate(FThreadWorker).DbgBreakPoint = nil) )');
  assert((FInternalBreakpoint=nil) or FLocationChanged, 'TFPBreakpoint.SetBreak: (FInternalBreakpoint=nil) or FLocationChanged');
  MaybeAbortWorker;

  FThreadWorker := TFpThreadWorkerBreakPointSetUpdate.Create(TFpDebugDebugger(Debugger), Self);
  if FLocationChanged then begin
    TFpThreadWorkerBreakPointSetUpdate(FThreadWorker).InternalBreakpoint := FInternalBreakpoint;
    FInternalBreakpoint := nil;
    FLocationChanged := False;
  end;
  TFpDebugDebugger(Debugger).FWorkQueue.PushItem(FThreadWorker);

  FValid := vsUnknown;
  FIsSet:=true;
  debuglnExit(DBG_BREAKPOINTS, ['<< TFPBreakpoint.SetBreak ' ]);
end;

procedure TFPBreakpoint.ResetBreak;
var
  WorkItem: TFpThreadWorkerBreakPointRemoveUpdate;
begin
  FIsSet:=false;
  if FThreadWorker <> nil then begin
    debugln(DBG_BREAKPOINTS, ['>> TFPBreakpoint.ResetBreak  CANCEL / REMOVE ',FSource,':',FLine,'/',dbghex(Address),' ' ]);
    assert(FInternalBreakpoint = nil, 'TFPBreakpoint.ResetBreak: FInternalBreakpoint = nil');
    MaybeAbortWorker;
    exit;
  end;

  // If Debugger is not assigned, the Controller's currentprocess is already
  // freed. And so are the corresponding InternalBreakpoint's.
  if assigned(Debugger) and assigned(FInternalBreakpoint) then
    begin
    FInternalBreakpoint.On_Thread_StateChange := nil;
    debuglnEnter(DBG_BREAKPOINTS, ['>> TFPBreakpoint.ResetBreak  REMOVE ',FSource,':',FLine,'/',dbghex(Address),' ' ]);
    WorkItem := TFpThreadWorkerBreakPointRemoveUpdate.Create(TFpDebugDebugger(Debugger), Self);
    TFpDebugDebugger(Debugger).FWorkQueue.PushItem(WorkItem);
    WorkItem.DecRef;
    FInternalBreakpoint := nil;
    debuglnExit(DBG_BREAKPOINTS, ['<< TFPBreakpoint.ResetBreak ' ]);
    end;
end;

procedure TFPBreakpoint.ThreadLogExpression;
var
  dbg: TFpDebugDebugger;
  Context: TFpDbgSymbolScope;
  PasExpr: TFpPascalExpression;
  PrettyPrinter: TFpPascalPrettyPrinter;
  s: String;
begin
  dbg := TFpDebugDebugger(Debugger);
  Context := dbg.GetContextForEvaluate(dbg.FDbgController.CurrentThreadId, 0);
  if Context <> nil then begin
    PrettyPrinter := nil;
    PasExpr := TFpPascalExpression.Create(FBrkLogExpr, Context, True);
    try
      PasExpr.IntrinsicPrefix := TFpDebugDebuggerProperties(dbg.GetProperties).IntrinsicPrefix;
      PasExpr.Parse;
      PasExpr.ResultValue; // trigger full validation
      if PasExpr.Valid then begin
        PrettyPrinter := TFpPascalPrettyPrinter.Create(Context.SizeOfAddress);
        PrettyPrinter.Context := Context.LocationContext;
        if PrettyPrinter.PrintValue(s, PasExpr.ResultValue) then begin
          FBrkLogResult := s;
          FBrkLogExpr := '';
        end;
      end;
    finally
      PasExpr.Free;
      PrettyPrinter.Free;
      Context.ReleaseReference;
    end;
  end;
end;

procedure TFPBreakpoint.ThreadLogCallStack;
var
  dbg: TFpDebugDebugger;
  thr: TDbgThread;
  CStack: TDbgCallstackEntryList;
  s: String;
  e: TDbgCallstackEntry;
  i, c: Integer;
begin
  dbg := TFpDebugDebugger(Debugger);
  thr := dbg.DbgController.CurrentThread;
  if thr = nil then
    exit;

  thr.PrepareCallStackEntryList(FBrkLogStackLimit);
  CStack := thr.CallStackEntryList;

  c := min(FBrkLogStackLimit, CStack.Count);
  SetLength(FBrkLogStackResult, c);
  for i := 0 to c - 1 do begin
    e := CStack[i];

    s := e.SourceFile;
    if s <> '' then
      s := s + ':' + IntToStr(e.Line)
    else
      s := IntToHex(e.AnAddress, 8);

    FBrkLogStackResult[i] := s + ' ' + e.FunctionName + LineEnding;
  end;
end;

procedure TFPBreakpoint.DoLogExpression(const AnExpression: String);
begin
  FBrkLogExpr := AnExpression;
  FBrkLogResult := '';
  TFpDebugDebugger(Debugger).ExecuteInDebugThread(@ThreadLogExpression);
  if FBrkLogExpr = '' then
    TFpDebugDebugger(Debugger).DoDbgEvent(ecBreakpoint, etBreakpointEvaluation, FBrkLogResult);
end;

procedure TFPBreakpoint.DoLogCallStack(const Limit: Integer);
var
  i: Integer;
begin
  if Limit <= 0 then
    exit;
  FBrkLogStackLimit := Limit;
  FBrkLogResult := '';
  TFpDebugDebugger(Debugger).ExecuteInDebugThread(@ThreadLogCallStack);
  for i := 0 to Length(FBrkLogStackResult) - 1 do
    TFpDebugDebugger(Debugger).DoDbgEvent(ecBreakpoint, etBreakpointStackDump, FBrkLogStackResult[i]);
end;

destructor TFPBreakpoint.Destroy;
begin
  if FInternalBreakpoint <> nil then
    FInternalBreakpoint.On_Thread_StateChange := nil;
  (* No need to request a pause. This will run, as soon as the debugger gets to the next pause.
     If the next pause is a hit on this breakpoint, then it will be ignored
  *)
  ResetBreak;
  MaybeAbortWorker(True);
  inherited Destroy;
end;

procedure TFPBreakpoint.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger.State in [dsPause, dsInternalPause]) or
     (TFpDebugDebugger(Debugger).FSendingEvents and (Debugger.State in [dsRun, dsInit]))
  then
    begin
    if (Enabled and not FIsSet) or FLocationChanged then
      begin
      FSetBreakFlag:=true;
      CheckChangeFlags;
      end
    else if (not enabled) and FIsSet then
      begin
      FResetBreakFlag:=true;
      CheckChangeFlags;
      end;
    end
  else if Debugger.State = dsStop then
    begin
    ResetBreak;
    end;
  inherited DoStateChange(AOldState);
end;

procedure TFPBreakpoint.DoPropertiesChanged(AChanged: TDbgBpChangeIndicators);
var
  ADebugger: TFpDebugDebugger;
  PauseReq: Boolean;
begin
  ADebugger := TFpDebugDebugger(Debugger);
  PauseReq := False;

  if ciLocation in AChanged then begin
    if Enabled then begin
      FLocationChanged := True;
      if (ADebugger.State in [dsPause, dsInternalPause, dsInit]) or TFpDebugDebugger(Debugger).FSendingEvents then begin
        FSetBreakFlag := True;
      end
      else if (ADebugger.State = dsRun) then begin
        ADebugger.QuickPause;
        PauseReq := True;
      end;
    end;
  end;

  if ciEnabled in AChanged then begin
    if (ADebugger.State in [dsPause, dsInternalPause, dsInit]) or TFpDebugDebugger(Debugger).FSendingEvents then begin
      if Enabled and not FIsSet then begin
        FSetBreakFlag := True;
      end
      else if not Enabled and FIsSet then begin
        FResetBreakFlag := True;
        FLocationChanged := False;
      end;
    end
    else if (ADebugger.State = dsRun) then begin
      if Enabled and (not FIsSet) then begin
        if not PauseReq then
          ADebugger.QuickPause;
      end
      else
      if (not Enabled) and FIsSet then
        ADebugger.FRunQuickPauseTasks := True;
    end;
  end;

  Changed;
end;

procedure TFPBreakpoint.DoChanged;
begin
  CheckChangeFlags;
  inherited DoChanged;
end;

procedure TFPBreakpoint.DoEndUpdate;
begin
  inherited DoEndUpdate;
  CheckChangeFlags; // in case DoChange is not called
end;

procedure TFPBreakpoint.CheckChangeFlags;
begin
  FNeedCheckChangeFlags := IsUpdating and not IsUpdateEnding;
  if FNeedCheckChangeFlags then
    exit;


  if FResetBreakFlag and not FSetBreakFlag then
    ResetBreak
  else
  if FSetBreakFlag and not (TFpDebugDebugger(Debugger).State in [dsStop, dsError]) then
    SetBreak;

  FSetBreakFlag := false;
  FResetBreakFlag := false;
end;

{ TFPDBGDisassembler }

function TFPDBGDisassembler.PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean;
var
  ARange, AReversedRange: TDBGDisassemblerEntryRange;
  AnEntry: TDisassemblerEntry;
  CodeBin: TBytes;
  p: pointer;
  ADump,
  AStatement,
  ASrcFileName,
  AFuncName: string;
  ASrcFileLine: integer;
  i,j, sz, bytesDisassembled, bufOffset: Integer;
  Sym: TFpSymbol;
  StatIndex: integer;
  FirstIndex: integer;
  ALastAddr, tmpAddr, tmpPointer, prevInstructionSize: TDBGPtr;
  ADisassembler: TDbgAsmDecoder;
  AOffset: longint;
  RealReadLen: Cardinal;
  AnInfo: TDbgInstInfo;

  procedure AddInfoToRange(ALineAddr: TDBGPtr; ATargetRange: TDBGDisassemblerEntryRange);
  var
    j: integer;
  begin
    Sym := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.FindProcSymbol(ALineAddr);
    // If this is the last statement for this source-code-line, fill the
    // SrcStatementCount from the prior statements.
    if (assigned(sym) and ((ASrcFileName<>sym.FileName) or (ASrcFileLine<>sym.Line))) or
      (not assigned(sym) and ((ASrcFileLine<>0) or (ASrcFileName<>''))) then
      begin
      for j := 0 to StatIndex-1 do
        ATargetRange.EntriesPtr[FirstIndex+j]^.SrcStatementCount:=StatIndex;
      StatIndex:=0;
      FirstIndex:=ATargetRange.Count;
      end;

    if assigned(sym) then
      begin
      ASrcFileName:=sym.FileName;
      ASrcFileLine:=sym.Line;
      AFuncName := sym.Name;
      AOffset := int32(int64(ALineAddr) - int64(Sym.Address.Address));
      sym.ReleaseReference;
      end
    else
      begin
      ASrcFileName:='';
      AFuncName := '';
      ASrcFileLine:=0;
      AOffset := -1;
      end;
    AnEntry.Addr := ALineAddr;
    AnEntry.Dump := ADump;
    AnEntry.Statement := AStatement;
    AnEntry.SrcFileLine:=ASrcFileLine;
    AnEntry.SrcFileName:=ASrcFileName;
    AnEntry.FuncName := AFuncName;
    AnEntry.SrcStatementIndex:=StatIndex;
    AnEntry.Offset := AOffset;
    AnEntry.TargetAddr := 0;
    AnEntry.TargetName := '';
    AnEntry.TargetFile := '';
    AnEntry.TargetLine := 0;
    if AnInfo.InstrType = itJump then begin
      AnEntry.IsJump := True;
      {$PUSH}{$R-}{$Q-}
      AnEntry.TargetAddr := ALineAddr + AnInfo.InstrTargetOffs;
      {$POP}
      Sym := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.FindProcSymbol(AnEntry.TargetAddr);
      if Sym <> nil then begin
        AnEntry.TargetName := Sym.Name;
        AnEntry.TargetFile := Sym.FileName;
        AnEntry.TargetLine := Sym.Line;
        {$PUSH}{$R-}{$Q-}
        AOffset := int32(int64(AnEntry.TargetAddr) - int64(Sym.Address.Address));
        {$POP}
        if AOffset <> 0 then
          AnEntry.TargetName := AnEntry.TargetName + '+' + IntToStr(AOffset);
        Sym.ReleaseReference;
      end;
    end
    else
    if AnInfo.InstrTargetOffs <> 0 then begin
      {$PUSH}{$R-}{$Q-}
      AnEntry.TargetAddr := ALineAddr + AnInfo.InstrTargetOffs;
      {$POP}
    end;
    ATargetRange.Append(@AnEntry);
    inc(StatIndex);
  end;

const
  MAX_DISASS_DIST_TO_ENTRY = 10000;
begin
  Result := False;
  if (Debugger = nil) or not(Debugger.State = dsPause) or FInPrepare then
    exit;

  FInPrepare := True;
  try
  ADisassembler := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.Disassembler;
  Sym:=nil;
  ASrcFileLine:=0;
  ASrcFileName:='';
  StatIndex:=0;
  FirstIndex:=0;
  ARange := TDBGDisassemblerEntryRange.Create;
  ARange.RangeStartAddr:=AnAddr;
  ALastAddr:=0;

  if (ALinesBefore > 0) and
    ADisassembler.CanReverseDisassemble then
   begin
     AReversedRange := TDBGDisassemblerEntryRange.Create;
     tmpAddr := AnAddr;  // do not modify AnAddr in this loop
     // Large enough block of memory for whole loop
     sz := ADisassembler.MaxInstructionSize * ALinesBefore;
     SetLength(CodeBin, sz);

     // TODO: Check if AnAddr is at lower address than length(CodeBin)
     //       and ensure ReadData size doesn't exceed available target memory.
     //       Fill out of bounds memory in buffer with "safe" value e.g. 0
     if sz > AnAddr then
     begin
       FillByte(CodeBin[0], sz, 0);
       // offset into buffer where active memory should start
       bufOffset := sz - AnAddr;
       // size of active memory to read
       sz := integer(AnAddr);
     end
     else
     begin
       bufOffset := 0;
     end;

     // Everything now counts back from starting address...
     bytesDisassembled := 0;
     // Only read up to byte before this address
     if not TFpDebugDebugger(Debugger).ReadData(tmpAddr-sz, sz, CodeBin[bufOffset]) then
       DebugLn(Format('Reverse disassemble: Failed to read memory at %s.', [FormatAddress(tmpAddr)]))
     else
       for i := 0 to ALinesBefore-1 do
       begin
         if bytesDisassembled >= sz then
           break;

         tmpPointer := TDBGPtr(@CodeBin[bufOffset]) + TDBGPtr(sz) - TDBGPtr(bytesDisassembled);
         p := pointer(tmpPointer);
         ADisassembler.ReverseDisassemble(p, ADump, AStatement); // give statement before pointer p, pointer p points to decoded instruction on return
         prevInstructionSize := tmpPointer - PtrUInt(p);
         bytesDisassembled := bytesDisassembled + prevInstructionSize;
         DebugLn(DBG_VERBOSE, format('Disassembled: [%.8X:  %s] %s',[tmpAddr, ADump, Astatement]));

         Dec(tmpAddr, prevInstructionSize);
         AddInfoToRange(tmpAddr, AReversedRange);
       end;

     if AReversedRange.Count>0 then
     begin
       // Update start of range
       ARange.RangeStartAddr := tmpAddr;
       // Copy range in revese order of entries
       for i := 0 to AReversedRange.Count-1 do
       begin
         // Reverse order of statements
         with AReversedRange.Entries[AReversedRange.Count-1 - i] do
         begin
           for j := 0 to SrcStatementCount-1 do
             SrcStatementIndex := SrcStatementCount - 1 - j;
         end;

         ARange.Append(AReversedRange.EntriesPtr[AReversedRange.Count-1 - i]);
       end;
     end;
     // Entries are all pointers, don't free entries
     FreeAndNil(AReversedRange);
   end
  else
  if ALinesBefore > 0 then begin
    Sym := TFpDebugDebugger(Debugger).FDbgController.CurrentProcess.FindProcSymbol(AnAddr);
    if Sym <> nil then begin
      tmpAddr := Sym.Address.Address;
      Sym.ReleaseReference;
      {$PUSH}{$Q-}{$R-}
      if (tmpAddr < AnAddr) and (AnAddr - tmpAddr < MAX_DISASS_DIST_TO_ENTRY) then begin
      {$POP}
        sz := AnAddr - tmpAddr + ADisassembler.MaxInstructionSize;
        SetLength(CodeBin, sz);
        bytesDisassembled := 0;
        if not TFpDebugDebugger(Debugger).ReadData(tmpAddr, sz, CodeBin[0], RealReadLen) then begin
          DebugLn(Format('Disassemble: Failed to read memory at %s.', [FormatAddress(tmpAddr)]));
        end
        else begin
          while tmpAddr < AnAddr do begin
            p := @CodeBin[bytesDisassembled];
            ADisassembler.Disassemble(p, ADump, AStatement, AnInfo);

            prevInstructionSize := p - @CodeBin[bytesDisassembled];
            if prevInstructionSize = 0 then
              break;
            bytesDisassembled := bytesDisassembled + prevInstructionSize;
            if bytesDisassembled > RealReadLen then
              break;
            if tmpAddr + prevInstructionSize > AnAddr then
              break;

            AddInfoToRange(tmpAddr, ARange);
            ALastAddr:=tmpAddr;
            Inc(tmpAddr, prevInstructionSize);
          end;
          if tmpAddr <> AnAddr then begin
            AnEntry.Addr := tmpAddr;
            AnEntry.Dump := '???';
            AnEntry.Statement := '???';
            AnEntry.SrcFileLine:=-1;
            AnEntry.SrcFileName:='';
            AnEntry.FuncName := '';
            AnEntry.SrcStatementIndex:=StatIndex;
            AnEntry.Offset := -1;
            AnEntry.TargetAddr := 0;
            AnEntry.TargetName := '';
            AnEntry.TargetFile := '';
            AnEntry.TargetLine := 0;
            ARange.Append(@AnEntry);
            inc(StatIndex);
          end;
        end;
      end;
    end;
  end;

  if ALinesAfter > 0 then
  begin
    StatIndex:=0;
    FirstIndex:=ARange.Count;
    sz := ALinesAfter * ADisassembler.MaxInstructionSize;
    SetLength(CodeBin, sz);
    bytesDisassembled := 0;
    if not TFpDebugDebugger(Debugger).ReadData(AnAddr, sz, CodeBin[0], RealReadLen) then
      begin
      DebugLn(Format('Disassemble: Failed to read memory at %s.', [FormatAddress(AnAddr)]));
      inc(AnAddr);
      end
    else
      for i := 0 to ALinesAfter-1 do
        begin
        p := @CodeBin[bytesDisassembled];
        ADisassembler.Disassemble(p, ADump, AStatement, AnInfo);

        prevInstructionSize := p - @CodeBin[bytesDisassembled];
        bytesDisassembled := bytesDisassembled + prevInstructionSize;
        if bytesDisassembled > RealReadLen then
          break;

        AddInfoToRange(AnAddr, ARange);
        ALastAddr:=AnAddr;
        Inc(AnAddr, prevInstructionSize);
        end;
  end
  else
    ALastAddr := AnAddr;

  if ARange.Count>0 then
    begin
    ARange.RangeEndAddr:=ALastAddr;
    ARange.LastEntryEndAddr:={%H-}TDBGPtr(p);
    EntryRanges.AddRange(ARange);
    Changed;
    result := true;
    end
  else
    begin
    result := false;
    ARange.Free;
    end;
  finally
    FInPrepare := False;
  end;
end;

{ TFPRegisters }

procedure TFPRegisters.GetRegisterValueList();
begin
  FRegisterList :=  FThr.RegisterValueList;
end;

procedure TFPRegisters.RequestData(ARegisters: TRegisters);
var
  ARegisterList: TDbgRegisterValueList;
  i: Integer;
  ARegisterValue: TRegisterValue;
  thr: TDbgThread;
  frm: TDbgCallstackEntry;
begin
  if not TFpDebugDebugger(Debugger).IsPausedAndValid then begin
    ARegisters.DataValidity:=ddsInvalid;
    exit;
  end;

  if not TFpDebugDebugger(Debugger).FDbgController.MainProcess.GetThread(ARegisters.ThreadId, thr) then begin
    ARegisters.DataValidity:=ddsError;
    exit;
  end;

  ARegisterList := nil;
  if ARegisters.StackFrame = 0 then begin
    FThr := thr;
    TFpDebugDebugger(Debugger).ExecuteInDebugThread(@GetRegisterValueList);
    ARegisterList :=  FRegisterList;
  end
  else begin
    frm := thr.CallStackEntryList[ARegisters.StackFrame];
    if frm <> nil then
      ARegisterList := frm.RegisterValueList;
  end;

  if ARegisterList = nil then begin
    ARegisters.DataValidity:=ddsError;
    exit;
  end;

  for i := 0 to ARegisterList.Count-1 do
    begin
    ARegisterValue := ARegisters.EntriesByName[ARegisterList[i].Name];
    if ARegisterList[i].Size <= 8 then
      ARegisterValue.ValueObj.SetAsNum(ARegisterList[i].NumValue, ARegisterList[i].Size);
    ARegisterValue.ValueObj.SetAsText(ARegisterList[i].StrFormatted[ARegisterValue.DisplayFormat]);
    ARegisterValue.Modified := ARegisterList.IsModified[ARegisterList[i]];
    ARegisterValue.DataValidity:=ddsValid;
    end;
  ARegisters.DataValidity:=ddsValid;
end;

{ TFpLineInfo }

function TFpLineInfo.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFpLineInfo.DoStateChange(const AOldState: TDBGState);
begin
  //inherited DoStateChange(AOldState);
  if not (Debugger.State in [dsPause, dsInternalPause, dsRun]) then
    ClearSources;
end;

procedure TFpLineInfo.ClearSources;
begin
  FRequestedSources.Clear;
end;

procedure TFpLineInfo.DebugInfoChanged(ADbgInstance: TDbgInstance);
var
  i: Integer;
  Src: String;
begin
  if (ADbgInstance.DbgInfo = nil) or not(ADbgInstance.DbgInfo is TFpDwarfInfo) then
    exit;

  for i := 0 to FRequestedSources.Count - 1 do begin
    if FRequestedSources.Objects[i] = nil then begin
      Src := FRequestedSources[i];
      FRequestedSources.Objects[i] := TObject(TFpDwarfInfo(ADbgInstance.DbgInfo).GetLineAddressMap(Src));
      if FRequestedSources.Objects[i] <> nil then
        DoChange(Src);
    end;
  end;
end;

constructor TFpLineInfo.Create(const ADebugger: TDebuggerIntf);
begin
  FRequestedSources := TStringListUTF8Fast.Create;
  inherited Create(ADebugger);
end;

destructor TFpLineInfo.Destroy;
begin
  FreeAndNil(FRequestedSources);
  inherited Destroy;
end;

function TFpLineInfo.Count: Integer;
begin
  Result := FRequestedSources.Count;
end;

function TFpLineInfo.HasAddress(const AIndex: Integer; const ALine: Integer
  ): Boolean;
var
  Map, Map2: PDWarfLineMap;
  dummy: TDBGPtrArray;
  FullName, BaseName: String;
  i: Integer;
begin
  Result := False;
  if not((FpDebugger.DebugInfo <> nil) and (FpDebugger.DebugInfo is TFpDwarfInfo)) then
    exit;
  Map := PDWarfLineMap(FRequestedSources.Objects[AIndex]);
  if Map <> nil then
  begin
    dummy:=nil;
    Result := Map^.GetAddressesForLine(ALine, dummy, True);
  end;

  if map = nil then begin
    FullName := FRequestedSources[AIndex];
    BaseName := ExtractFileName(FullName);
    if (FullName <> BaseName) then begin
      i := FRequestedSources.IndexOf(BaseName);
      if i >= 0 then begin
        Map2 := PDWarfLineMap(FRequestedSources.Objects[i]);
        if (Map2 <> nil) and (Map2 <> Map) then begin
          dummy:=nil;
          Result := Map2^.GetAddressesForLine(ALine, dummy, True);
        end;
      end;
    end;
  end;
end;

function TFpLineInfo.GetInfo(AAddress: TDbgPtr; out ASource, ALine,
  AOffset: Integer): Boolean;
begin
  Result := False;
end;

function TFpLineInfo.IndexOf(const ASource: String): integer;
begin
  Result := FRequestedSources.IndexOf(ASource);
  (* For dsInit, dsPause:
     - DebugManager.DebuggerChangeState calls SourceEditorManager.FillExecutionMarks;
     - This happens, even if the data is not available.
     - TSourceEditor.FillExecutionMarks will call this function ("IndexOf") and create an
       empty non-functional map.
       (Happens for libraries, loaded with LoadLibrary during dsInit, if the source is open)
     - To avoid this => return -1
  *)
  if (Result >= 0) and (FRequestedSources.Objects[Result] = nil) then
    Result := -1;
end;

procedure TFpLineInfo.Request(const ASource: String);
var
  lmap, lmap2: PDWarfLineMap;
  lib: TDbgLibrary;
  BaseName: String;
begin
  lmap := nil;
  lmap2 := nil;

  BaseName := ExtractFileName(ASource);
  if IndexOf(BaseName) >= 0 then
    BaseName := ASource; // already got the basename
  if (FpDebugger.DebugInfo <> nil) and (FpDebugger.DebugInfo is TFpDwarfInfo) then begin
    lmap := TFpDwarfInfo(FpDebugger.DebugInfo).GetLineAddressMap(ASource);
    if (ASource <> BaseName) and (lmap <> nil) then
      lmap2 := TFpDwarfInfo(FpDebugger.DebugInfo).GetLineAddressMap(BaseName);
  end;

  if (lmap = nil) and (FpDebugger.DbgController <> nil) and (FpDebugger.DbgController.CurrentProcess <> nil) then begin
    for lib in FpDebugger.DbgController.CurrentProcess.LibMap do begin
      if (lib.DbgInfo <> nil) and  (lib.DbgInfo is TFpDwarfInfo) then begin
        lmap := TFpDwarfInfo(lib.DbgInfo).GetLineAddressMap(ASource);
        if lmap <> nil then begin
          if (ASource <> BaseName) then
            lmap2 := TFpDwarfInfo(lib.DbgInfo).GetLineAddressMap(BaseName);
          break;
        end;
      end;
    end;
  end;

  if (ASource <> BaseName) then
    FRequestedSources.AddObject(BaseName, TObject(lmap2));
  FRequestedSources.AddObject(ASource, TObject(lmap));
  if lmap <> nil then
    DoChange(ASource);
end;

procedure TFpLineInfo.Cancel(const ASource: String);
begin
  //
end;

{ TFPWatches }

function TFPWatches.FpDebugger: TFpDebugDebugger;
begin
  Result := TFpDebugDebugger(Debugger);
end;

procedure TFPWatches.StopWorkes;
begin
  FWatchEvalWorkers.RequestStopForWorkers;
end;

procedure TFPWatches.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  FWatchEvalWorkers.WaitForWorkers(True);
end;

procedure TFPWatches.InternalRequestData(AWatchValue: IDbgWatchValueIntf);
var
  WorkItem: TFpThreadWorkerWatchValueEvalUpdate;
begin
  if not FpDebugger.IsPausedAndValid then begin
    AWatchValue.Validity := ddsInvalid;
    exit;
  end;

  WorkItem := TFpThreadWorkerWatchValueEvalUpdate.Create(FpDebugger, AWatchValue);
  FpDebugger.FWorkQueue.PushItem(WorkItem);
  FWatchEvalWorkers.Add(WorkItem);
end;

destructor TFPWatches.Destroy;
begin
  FWatchEvalWorkers.WaitForWorkers(True);
  inherited Destroy;
end;

{ TFpDebugExceptionStepping }

function TFpDebugExceptionStepping.GetDbgController: TDbgController;
begin
  Result := FDebugger.FDbgController;
end;

function TFpDebugExceptionStepping.dbgs(st: TExceptStepState): string;
begin
  writestr(Result, st);
end;

function TFpDebugExceptionStepping.dbgs(loc: TBreakPointLoc): string;
begin
  writestr(Result, loc);
end;

function TFpDebugExceptionStepping.dbgs(locs: TBreakPointLocs): string;
var
  a: TBreakPointLoc;
begin
  Result := '';
  for a in locs do Result := Result + dbgs(a) +',';
end;

function TFpDebugExceptionStepping.GetCurrentProcess: TDbgProcess;
begin
  Result := FDebugger.FDbgController.CurrentProcess;
end;

function TFpDebugExceptionStepping.GetCurrentCommand: TDbgControllerCmd;
begin
  Result := FDebugger.FDbgController.CurrentCommand;
end;

function TFpDebugExceptionStepping.GetCurrentThread: TDbgThread;
begin
  Result := FDebugger.FDbgController.CurrentThread;
end;

procedure TFpDebugExceptionStepping.EnableBreaks(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Not in thread => only flag desired changes
  for a in ALocs do
    Include(FBreakNewEnabled, a);
end;

procedure TFpDebugExceptionStepping.EnableBreaksDirect(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Running in debug thread
  //debugln(['EnableBreaksDirect ', dbgs(ALocs)]);
  for a in ALocs do
    if FBreakPoints[a] <> nil then begin
      if not(a in FBreakEnabled) then
        FBreakPoints[a].SetBreak;
      Include(FBreakEnabled, a);
      Include(FBreakNewEnabled, a);
    end;
end;

procedure TFpDebugExceptionStepping.DisableBreaks(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Not in thread => only flag desired changes
  //debugln(['DisableBreaks ', dbgs(ALocs)]);
  for a in ALocs do
    Exclude(FBreakNewEnabled, a);
end;

procedure TFpDebugExceptionStepping.DisableBreaksDirect(ALocs: TBreakPointLocs);
var
  a: TBreakPointLoc;
begin
  // Running in debug thread
  //debugln(['DisableBreaksDirect ', dbgs(ALocs)]);
  for a in ALocs do
    if FBreakPoints[a] <> nil then begin
      if (a in FBreakEnabled) then
        FBreakPoints[a].ResetBreak;
      Exclude(FBreakEnabled, a);
      Exclude(FBreakNewEnabled, a);
    end;
end;

procedure TFpDebugExceptionStepping.SetStepOutAddrDirect(AnAddr: TDBGPtr);
begin
  FreeAndNil(FBreakPoints[bplStepOut]);
  FBreakPoints[bplStepOut] := CurrentProcess.AddBreak(AnAddr);
end;

procedure TFpDebugExceptionStepping.DoExceptionRaised(var &continue: boolean);
var
  AnExceptionLocation: TDBGLocationRec;
begin
  FDebugger.HandleSoftwareException(AnExceptionLocation, &continue);

  case &continue of
    True: begin
        if (CurrentCommand <> nil) and not(CurrentCommand is TDbgControllerContinueCmd) and
           (CurrentCommand.Thread = CurrentThread)
        then begin
          EnableBreaks([bplPopExcept, bplCatches{$IFDEF WIN64} , bplFpcSpecific {$ENDIF}]);
          FState := esIgnoredRaise; // currently stepping
        end;
      end;
    False:
      begin
        FDebugger.EnterPause(AnExceptionLocation);
        FState := esStoppedAtRaise;
      end;
  end;
end;

//procedure TFpDebugExceptionStepping.DoPopExcptStack;
//begin
//  // check if step over??
//  // clear breaks
//  DbgController.AbortCurrentCommand;
//  DbgController.StepOut;
//  FState := esNone;
//
//  DisableBreaks([bplPopExcept, bplCatches{$IFDEF WIN64} , bplFpcSpecific {$ENDIF}]);
//end;

procedure TFpDebugExceptionStepping.DoRtlUnwindEx;
begin

end;

constructor TFpDebugExceptionStepping.Create(ADebugger: TFpDebugDebugger);
begin
  FDebugger := ADebugger;
  {$IFDEF WIN64}
  FAddressFrameListSehW64Except := TAddressFrameList.Create(True);
  FAddressFrameListSehW64Finally := TAddressFrameList.Create(True);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FAddressFrameListSehW32Except:= TAddressFrameList.Create(True);
  FAddressFrameListSehW32Finally:= TAddressFrameList.Create(True);
  {$ENDIF}
end;

destructor TFpDebugExceptionStepping.Destroy;
begin
  inherited Destroy;
  {$IFDEF WIN64}
  FAddressFrameListSehW64Except.Destroy;
  FAddressFrameListSehW64Finally.Destroy;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FAddressFrameListSehW32Except.Destroy;
  FAddressFrameListSehW32Finally.Destroy;
  {$ENDIF}
end;

procedure TFpDebugExceptionStepping.DoProcessLoaded;
var
  InternalExceptionBPSettings: TInternalExceptionBreakPoints;
begin
  FBreakEnabled := [];
  FBreakNewEnabled := [];
  debuglnEnter(DBG_BREAKPOINTS, ['>> TFpDebugDebugger.SetSoftwareExceptionBreakpoints' ]);
  InternalExceptionBPSettings := TFpDebugDebuggerProperties(FDebugger.GetProperties).InternalExceptionBreakPoints;

  if ieBreakErrorBreakPoint in InternalExceptionBPSettings then
    FBreakPoints[bplBreakError]    := FDebugger.AddBreak('FPC_BREAK_ERROR');

  if ieRunErrorBreakPoint in InternalExceptionBPSettings then
    FBreakPoints[bplRunError]      := FDebugger.AddBreak('FPC_RUNERROR');

  if ieRaiseBreakPoint in InternalExceptionBPSettings then
  begin
    FBreakPoints[bplRaise]         := FDebugger.AddBreak('FPC_RAISEEXCEPTION');
    FBreakPoints[bplReRaise]       := FDebugger.AddBreak('FPC_RERAISE', nil,            False);
    FBreakPoints[bplPopExcept]     := FDebugger.AddBreak('FPC_POPADDRSTACK', nil,       False);
    FBreakPoints[bplCatches]       := FDebugger.AddBreak('FPC_CATCHES', nil,            False);
    {$IFDEF MSWINDOWS}
    if CurrentProcess.Mode = dm32 then begin
      FBreakPoints[bplFpcExceptHandler]  := FDebugger.AddBreak('__FPC_except_handler', nil,  False);
      FBreakPoints[bplFpcFinallyHandler] := FDebugger.AddBreak('__FPC_finally_handler', nil, False);
      FBreakPoints[bplFpcLeaveHandler]   := FDebugger.AddBreak('_FPC_leave', nil, False);
      FBreakPoints[bplSehW32Except]      := FDebugger.AddBreak(0, False);
      FBreakPoints[bplSehW32Finally]     := FDebugger.AddBreak(0, False);
    {$IfDef WIN64}
    end
    else
    if CurrentProcess.Mode = dm64 then begin
      FBreakPoints[bplFpcSpecific]   := FDebugger.AddBreak('__FPC_specific_handler', nil, False);
      FBreakPoints[bplSehW64Except]  := FDebugger.AddBreak(0, False);
      FBreakPoints[bplSehW64Finally] := FDebugger.AddBreak(0, False);
      FBreakPoints[bplSehW64Unwound] := FDebugger.AddBreak(0, False);
    {$EndIf}
    end;
    {$ENDIF}
  end;
  debuglnExit(DBG_BREAKPOINTS, ['<< TFpDebugDebugger.SetSoftwareExceptionBreakpoint' ]);
end;

procedure TFpDebugExceptionStepping.DoNtDllLoaded(ALib: TDbgLibrary);
begin
  {$IFDEF WIN64}
  if CurrentProcess.Mode = dm64 then begin
    debugln(DBG_BREAKPOINTS, ['SetSoftwareExceptionBreakpoint RtlUnwind']);
    DisableBreaksDirect([bplRtlUnwind, bplRtlRestoreContext]);
    FreeAndNil(FBreakPoints[bplRtlRestoreContext]);
    FBreakPoints[bplRtlRestoreContext] := FDebugger.AddBreak('RtlRestoreContext', ALib, False);
    FBreakPoints[bplRtlUnwind].Free;
    FBreakPoints[bplRtlUnwind] := FDebugger.AddBreak('RtlUnwindEx', ALib, False);
  end;
  {$ENDIF}
end;

procedure TFpDebugExceptionStepping.DoDbgStopped;
var
  a: TBreakPointLoc;
begin
  debuglnEnter(DBG_BREAKPOINTS, ['>> TFpDebugDebugger.FDbgControllerProcessExitEvent fpc_Raiseexception' ]);
  for a in TBreakPointLoc do
    FreeAndNil(FBreakPoints[a]);
  debuglnExit(DBG_BREAKPOINTS, ['<< TFpDebugDebugger.FDbgControllerProcessExitEvent ' ]);
end;

procedure TFpDebugExceptionStepping.ThreadBeforeLoop(Sender: TObject);
begin
  // Running in debug thread
  EnableBreaksDirect(FBreakNewEnabled - FBreakEnabled);
  DisableBreaksDirect(FBreakEnabled - FBreakNewEnabled);
  {$IFDEF WIN64}
  if assigned(FBreakPoints[bplSehW64Unwound]) then
    FBreakPoints[bplSehW64Unwound].RemoveAllAddresses;
  {$ENDIF}
end;

procedure TFpDebugExceptionStepping.ThreadProcessLoopCycle(
  var AFinishLoopAndSendEvents: boolean; var AnEventType: TFPDEvent;
  var ACurCommand: TDbgControllerCmd; var AnIsFinished: boolean);

  function CheckCommandFinishesInFrame(AFrameAddr: TDBGPtr): Boolean;
  begin
    Result := (ACurCommand is TDbgControllerHiddenBreakStepBaseCmd) and
              (TDbgControllerHiddenBreakStepBaseCmd(CurrentCommand).StoredStackFrameInfo <> nil);
    if not Result then
      exit; // none stepping command, does not stop
    if ACurCommand is TDbgControllerStepOutCmd then
      Result := TDbgControllerHiddenBreakStepBaseCmd(CurrentCommand).StoredStackFrameInfo.StoredStackFrame < AFrameAddr
    else
      Result := TDbgControllerHiddenBreakStepBaseCmd(CurrentCommand).StoredStackFrameInfo.StoredStackFrame <= AFrameAddr;
  end;

  {$IFDEF MSWINDOWS}
  procedure CheckSteppedOutFromW64SehFinally;
  var
    sym: TFpSymbol;
    r, IsLeave: Boolean;
  begin
    if (FState <> esNone) or (not(ACurCommand is TDbgControllerLineStepBaseCmd)) then
      exit;

    if (pos('fin$', TDbgControllerLineStepBaseCmd(ACurCommand).StartedInFuncName) < 1) then
      exit;

    if (not TDbgControllerLineStepBaseCmd(ACurCommand).IsSteppedOut) then begin
      {$IFDEF WIN64}
      EnableBreaksDirect([bplFpcSpecific]);
      {$ENDIF}
      exit;
    end;

    IsLeave := False;
    sym := CurrentProcess.FindProcSymbol(CurrentThread.GetInstructionPointerRegisterValue);
    if CurrentProcess.Mode = dm32 then begin
      IsLeave := (CompareText(sym.Name, '_FPC_LEAVE') = 0);
      r := (sym <> nil) and (sym.FileName <> '') and
           (not IsLeave) and
           (CompareText(sym.Name, '__FPC_FINALLY_HANDLER') <> 0);
    end
    else
      r := (sym <> nil) and (CompareText(sym.Name, '__FPC_SPECIFIC_HANDLER') <> 0) and
           (sym.FileName <> '');
    sym.ReleaseReference;
    if r then
      exit;

    FState := esSteppingFpcSpecialHandler;
    AFinishLoopAndSendEvents := False;
    ACurCommand := TDbgControllerStepThroughFpcSpecialHandler.Create(DbgController, CurrentThread.GetInstructionPointerRegisterValue, IsLeave);
  end;
  {$ENDIF}

  procedure StepOutFromPopCatches;
  begin
    ACurCommand := TDbgControllerStepOutCmd.Create(DbgController);
    TDbgControllerStepOutCmd(ACurCommand).SetReturnAdressBreakpoint(CurrentProcess, True);
  end;

const
  MaxFinallyHandlerCnt = 256; // more finally in a single proc is not probable....
var
  StepOutStackPos, ReturnAddress, PC: TDBGPtr;
  {$IFDEF WIN64}
  Rdx, Rcx, R8, R9, TargetSp, HData, ImgBase: TDBGPtr;
  i: Integer;
  EFlags, Cnt: Cardinal;
  FinallyData: Array of array [0..3] of DWORD; //TScopeRec
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Base, Addr, SP: TDBGPtr;
  Eax: TDBGPtr;
  {$ENDIF}
  o: Integer;
  n: String;
begin
  case AnEventType of
    deExitProcess, deDetachFromProcess: begin
      FDebugger.FExceptionStepper.DoDbgStopped;
      exit;
    end;
    deLoadLibrary: begin
      if (CurrentProcess <> nil) and (Length(CurrentProcess.LastLibrariesLoaded) > 0) then begin
        // On Windows there is always only one library loaded at a deLoadLibrary
        // event, so it is safe to only check the first item of LastLibrariesLoaded
        n := ExtractFileName(CurrentProcess.LastLibrariesLoaded[0].Name);
        if n = 'ntdll.dll' then
          FDebugger.FExceptionStepper.DoNtDllLoaded(CurrentProcess.LastLibrariesLoaded[0]);
      end;
      exit;
    end;
  end;

  if (CurrentThread = nil) then
    exit;

  FDebugger.FDbgController.DefaultContext; // Make sure it is avail and cached / so it can be called outside the thread

  PC := CurrentThread.GetInstructionPointerRegisterValue;
  {$IFDEF WIN64}
  if Assigned(FBreakPoints[bplSehW64Unwound]) and FBreakPoints[bplSehW64Unwound].HasLocation(PC)
  then begin
    FBreakPoints[bplSehW64Unwound].RemoveAllAddresses;
    AFinishLoopAndSendEvents := AnIsFinished or (FState = esStepToFinally);
    if AFinishLoopAndSendEvents then begin
      AnEventType := deFinishedStep; // only step commands can end up here
      exit;
    end;
  end;
  {$ENDIF}

  if (CurrentCommand = nil) then
    exit;

  // Needs to be correct thread, ignore events in other threads
  if (CurrentCommand.Thread <> CurrentThread)
  then begin
    if (assigned(FBreakPoints[bplPopExcept])    and FBreakPoints[bplPopExcept].HasLocation(PC))
    or (assigned(FBreakPoints[bplCatches])      and FBreakPoints[bplCatches].HasLocation(PC))
    or (assigned(FBreakPoints[bplStepOut])      and FBreakPoints[bplStepOut].HasLocation(PC))
    or (assigned(FBreakPoints[bplReRaise])      and FBreakPoints[bplReRaise].HasLocation(PC))
       {$IFDEF MSWINDOWS}
    or (assigned(FBreakPoints[bplSehW32Except])      and FBreakPoints[bplSehW32Except].HasLocation(PC))
    or (assigned(FBreakPoints[bplSehW32Finally])     and FBreakPoints[bplSehW32Finally].HasLocation(PC))
    or (assigned(FBreakPoints[bplFpcExceptHandler])  and FBreakPoints[bplFpcExceptHandler].HasLocation(PC))
    or (assigned(FBreakPoints[bplFpcFinallyHandler]) and FBreakPoints[bplFpcFinallyHandler].HasLocation(PC))
    or (assigned(FBreakPoints[bplFpcLeaveHandler])   and FBreakPoints[bplFpcLeaveHandler].HasLocation(PC))
       {$ENDIF}
       {$IFDEF WIN64}
    or (assigned(FBreakPoints[bplFpcSpecific])       and FBreakPoints[bplFpcSpecific].HasLocation(PC))
    or (assigned(FBreakPoints[bplRtlRestoreContext]) and FBreakPoints[bplRtlRestoreContext].HasLocation(PC))
    or (assigned(FBreakPoints[bplRtlUnwind])         and FBreakPoints[bplRtlUnwind].HasLocation(PC))
    or (assigned(FBreakPoints[bplSehW64Except])      and FBreakPoints[bplSehW64Except].HasLocation(PC))
    or (assigned(FBreakPoints[bplSehW64Finally])     and FBreakPoints[bplSehW64Finally].HasLocation(PC))
       {$ENDIF}
    then begin
      AFinishLoopAndSendEvents := False;
    end;
    exit;
  end;


  if (FState = esSteppingFpcSpecialHandler) and
     (ACurCommand is TDbgControllerStepThroughFpcSpecialHandler) and
     (TDbgControllerStepThroughFpcSpecialHandler(ACurCommand).InteralFinished)
  then begin
    if AnIsFinished then begin
      exit; // stepped out of _FPC_LEAVE;
    end
    else
    if TDbgControllerStepThroughFpcSpecialHandler(ACurCommand).FDone then begin
      FState := esNone;
      if ACurCommand.Thread = CurrentThread then
        ACurCommand := TDbgControllerStepOverFirstFinallyLineCmd.Create(DbgController);
      // else thread has gone => finish old command
    end
    else begin
      FState := esStepToFinally;
      {$IFDEF WIN64}
      EnableBreaksDirect([bplFpcSpecific]);
      {$ENDIF}
    end;
    AFinishLoopAndSendEvents := False;
    exit;
  end
  else
  if CurrentProcess.CurrentBreakpoint = nil then begin
    {$IFDEF MSWINDOWS}
    CheckSteppedOutFromW64SehFinally;
    {$ENDIF}
    exit;
  end;
  {$IFDEF WIN64}
  DisableBreaksDirect([bplRtlUnwind, bplSehW64Finally]); // bplRtlUnwind must always be unset;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  SP := CurrentThread.GetStackPointerRegisterValue;
  {$ENDIF}
  {$IFDEF WIN64}
  FAddressFrameListSehW64Except.RemoveOutOfScopeFrames(SP, FBreakPoints[bplSehW64Except]);
  if ACurCommand is TDbgControllerStepOutCmd then
    FAddressFrameListSehW64Finally.RemoveOutOfScopeFrames(SP+1, FBreakPoints[bplSehW64Finally]) // include current frame
  else
    FAddressFrameListSehW64Finally.RemoveOutOfScopeFrames(SP, FBreakPoints[bplSehW64Finally]);
  {$ENDIF}

  // bplPopExcept / bplCatches
  if (assigned(FBreakPoints[bplPopExcept]) and FBreakPoints[bplPopExcept].HasLocation(PC)) or
     (assigned(FBreakPoints[bplCatches]) and FBreakPoints[bplCatches].HasLocation(PC))
  then begin
    debugln(FPDBG_COMMANDS, ['@ bplPop/bplCatches ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;

    //DebugLn(['THreadProcLoop ', dbgs(FState), ' ', DbgSName(CurrentCommand)]);
    DisableBreaksDirect([bplPopExcept, bplCatches{$IFDEF WIN64} , bplFpcSpecific {$ENDIF}]); // FpcSpecific was not needed -> not SEH based code
    case FState of
      esIgnoredRaise: begin
          // bplReRaise may set them again
          if not (CurrentCommand is TDbgControllerHiddenBreakStepBaseCmd) then
            exit; // wrong command type // should not happen

          if AnIsFinished then begin
// FORCE the breakpoint WITHoUT FRAME => known to be without frame // optimized fpc may not have expected asm
            StepOutFromPopCatches;
          end
          else begin
            o := 0;
            if (CurrentCommand is TDbgControllerStepOutCmd) then
              o := 1; // frame must be less, not equal

            {$PUSH}{$Q-}{$R-}
            // GetStackBasePointerRegisterValue is still on parent frame
            if CheckCommandFinishesInFrame(CurrentThread.GetStackBasePointerRegisterValue - o)
            then begin
              // Insert a "step out" breakpoint, but leave control to the running command.
              StepOutStackPos := CurrentThread.GetStackPointerRegisterValue;
              if CurrentProcess.ReadAddress(StepOutStackPos, ReturnAddress) then
                SetStepOutAddrDirect(ReturnAddress)
              else
                StepOutFromPopCatches; // error reading mem
            end;
            {$POP}
          end;
        end;
      esStepToFinally: begin
          StepOutFromPopCatches;
        end;
    end;
    FState := esNone;
  end
  else
  // bplStepOut => part of esIgnoredRaise
  if assigned(FBreakPoints[bplStepOut]) and FBreakPoints[bplStepOut].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplStepOut ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := AnIsFinished;
    AnEventType := deFinishedStep;
    CurrentProcess.RemoveBreak(FBreakPoints[bplStepOut]);
    FreeAndNil(FBreakPoints[bplStepOut]);
  end
  else
  // bplReRaise
  if assigned(FBreakPoints[bplReRaise]) and FBreakPoints[bplReRaise].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplReRaise ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    EnableBreaksDirect([bplPopExcept, bplCatches{$IFDEF WIN64} , bplFpcSpecific {$ENDIF}]);
    // if not(FState = esStepToFinally) then
    FState := esIgnoredRaise;
  end
  {$IFDEF MSWINDOWS}
  (* ***** Win32 SEH Except ***** *)
  else
  if assigned(FBreakPoints[bplSehW32Except]) and FBreakPoints[bplSehW32Except].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplSehW32Except ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;

    //if (not (FState in [esStepToFinally])) and
    //   not(CurrentCommand is TDbgControllerHiddenBreakStepBaseCmd)
    //then
    //  exit; // wrong command type / should not happen
    if (FState = esIgnoredRaise) and
       (not CheckCommandFinishesInFrame(CurrentThread.GetStackBasePointerRegisterValue))
    then
      exit;

    AFinishLoopAndSendEvents := True; // Stop at this address
    FState := esAtWSehExcept;
    AnIsFinished := True;
    AnEventType := deFinishedStep;


  end
  (* ***** Win32 SEH Finally ***** *)
  else
  if assigned(FBreakPoints[bplSehW32Finally]) and FBreakPoints[bplSehW32Finally].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplSehW32Finally ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;

    // At the start of a finally the BasePointer is in EAX // reg 0
    Eax  := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(0).NumValue;
    FAddressFrameListSehW32Finally.RemoveOutOfScopeFrames(EAX, FBreakPoints[bplSehW32Finally]);

    if (ACurCommand is TDbgControllerHiddenBreakStepBaseCmd) and
       not CheckCommandFinishesInFrame(Eax)
    then
      exit;

    // step over proloque
    ACurCommand := TDbgControllerStepOverFirstFinallyLineCmd.Create(DbgController);
    FState := esStepSehFinallyProloque;
  end
  else
  (* ***** Win32 SEH ExceptHandler ***** *)
  if assigned(FBreakPoints[bplFpcExceptHandler]) and FBreakPoints[bplFpcExceptHandler].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplFpcExceptHandler ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    AnIsFinished := False;

    (*   TSEHFrame=record
           Next: PSEHFrame;
           Addr: Pointer;
           _EBP: PtrUint;
           HandlerArg: Pointer;
         end;
    *)
    {$PUSH}{$Q-}{$R-}
    if (not CurrentProcess.ReadAddress(SP + 8, Addr)) or (Addr = 0) then
      exit;
    if (not CurrentProcess.ReadAddress(Addr + 12, Addr)) or (Addr = 0) then
      exit;
    CurrentProcess.ReadAddress(Addr + 8, Base);
    {$POP}

    if Base <> 0 then
    FAddressFrameListSehW32Except.Add(Addr, Base);
    FBreakPoints[bplSehW32Except].AddAddress(Addr);
    FBreakPoints[bplSehW32Except].SetBreak;
  end
  else
  (* ***** Win32 SEH FinallyHandler ***** *)
  if assigned(FBreakPoints[bplFpcFinallyHandler]) and FBreakPoints[bplFpcFinallyHandler].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplFpcFinallyHandler ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    AnIsFinished := False;

    {$PUSH}{$Q-}{$R-}
    if (not CurrentProcess.ReadAddress(SP + 8, Addr)) or (Addr = 0) then
      exit;
    if (not CurrentProcess.ReadAddress(Addr + 12, Addr)) or (Addr = 0) then
      exit;
    CurrentProcess.ReadAddress(Addr + 8, Base);
    {$POP}

    if Base <> 0 then
      FAddressFrameListSehW32Finally.Add(Addr, Base);
    FBreakPoints[bplSehW32Finally].AddAddress(Addr);
    FBreakPoints[bplSehW32Finally].SetBreak;
  end
  else
  (* ***** Win32 SEH LeaveHandler ***** *)
  if assigned(FBreakPoints[bplFpcLeaveHandler]) and FBreakPoints[bplFpcLeaveHandler].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplFpcLeaveHandler ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    AnIsFinished := False;

    {$PUSH}{$Q-}{$R-}
    if (not CurrentProcess.ReadAddress(SP + 16, Addr)) or (Addr = 0) then
      exit;
    CurrentProcess.ReadAddress(Addr + 4, Base);
    {$POP}

    if Base <> 0 then
      FAddressFrameListSehW32Finally.Add(Addr, Base);
    FBreakPoints[bplSehW32Finally].AddAddress(Addr);
    FBreakPoints[bplSehW32Finally].SetBreak;
  end
  {$ENDIF}
  {$IFDEF WIN64}
  else
  (* ***** Win64 SEH ***** *)
  // bplFpcSpecific
  if assigned(FBreakPoints[bplFpcSpecific]) and FBreakPoints[bplFpcSpecific].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplFpcSpecific ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    AnIsFinished := False;
    EnableBreaksDirect([bplRtlUnwind]);

    if (FState = esIgnoredRaise) and not(CurrentCommand is TDbgControllerHiddenBreakStepBaseCmd) then
      exit; // wrong command type // should not happen

    (* TODO: Look at using DW_TAG_try_block https://bugs.freepascal.org/view.php?id=34881 *)

    (* Get parm in RCX:
       EXCEPTION_RECORD = record
          ExceptionCode : DWORD;
          ExceptionFlags : DWORD;
          ExceptionRecord : ^_EXCEPTION_RECORD;
          ExceptionAddress : PVOID;
          NumberParameters : DWORD;
          ExceptionInformation : array[0..(EXCEPTION_MAXIMUM_PARAMETERS)-1] of ULONG_PTR;
       end;     *)
    Rcx := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(2).NumValue; // rec: TExceptionRecord
    {$PUSH}{$Q-}{$R-}
    if (not CurrentProcess.ReadData(Rcx + 4, 4, EFlags)) or
       ((EFlags and 66) = 0) // rec.ExceptionFlags and EXCEPTION_UNWIND)=0
    then
      exit;

    (* Get FrameBasePointe (RPB) for finally block (passed in R8) *)
    R8  := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(8).NumValue;
    if (not CurrentProcess.ReadAddress(R8 + 160, Base)) or (Base = 0) then // RPB at finally
      exit;

    if ( (FState = esIgnoredRaise) or (ACurCommand is TDbgControllerHiddenBreakStepBaseCmd) ) and
       not CheckCommandFinishesInFrame(Base)
    then
      exit;

    if (not CurrentProcess.ReadAddress(R8 + 152, TargetSp)) then
      TargetSp := 0;

    // R9 = dispatch: TDispatcherContext
    R9  := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(9).NumValue;
    //dispatch.HandlerData
    if (not CurrentProcess.ReadAddress(R9 + 56, HData)) or (HData = 0) then
      exit;
      (* HandlerData = MaxScope: DWord, array of ^TScopeRec
          TScopeRec=record
            Typ: DWord;        { SCOPE_FINALLY: finally code in RvaHandler
                                 SCOPE_CATCHALL: unwinds to RvaEnd, RvaHandler is the end of except block
                                 SCOPE_IMPLICIT: finally code in RvaHandler, unwinds to RvaEnd
                                 otherwise: except with 'on' stmts, value is RVA of filter data }
            RvaStart: DWord;
            RvaEnd: DWord;
            RvaHandler: DWord;        *)
    if (not CurrentProcess.ReadData(HData, 4, Cnt)) or (Cnt = 0) or (Cnt > MaxFinallyHandlerCnt) then
      exit;

    if (not CurrentProcess.ReadAddress(R9 + 8, ImgBase)) or (ImgBase = 0) then
      exit;

    SetLength(FinallyData, Cnt);
    if (not CurrentProcess.ReadData(HData + 4, 16 * Cnt, FinallyData[0])) then
      exit;
    for i := 0 to Cnt - 1 do begin
      Addr := FinallyData[i][3];
      if (FinallyData[i][0] <> 0) or // scope^.Typ=SCOPE_FINALLY
         (Addr = 0)
      then
        Continue;
      if TargetSp <> 0 then
        FAddressFrameListSehW64Finally.Add(ImgBase + Addr, TargetSp);
      FBreakPoints[bplSehW64Finally].AddAddress(ImgBase + Addr);
    end;
    {$POP}
    FBreakPoints[bplSehW64Finally].SetBreak;
  end
  else
  // bplRtlRestoreContext
  if assigned(FBreakPoints[bplRtlRestoreContext]) and FBreakPoints[bplRtlRestoreContext].HasLocation(PC) then begin
    AFinishLoopAndSendEvents := False;
    AnIsFinished := False;

    if (CurrentCommand <> nil) and (CurrentCommand.Thread <> CurrentThread) then
      exit;
    debugln(FPDBG_COMMANDS, ['@ bplRtlRestoreContext ', DbgSName(CurrentCommand)]);
    // RCX = TContext
    Rcx := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(2).NumValue; // rsp at target
    if (Rcx <> 0) then begin
    if (not CurrentProcess.ReadAddress(Rcx + PtrUInt(@PCONTEXT(nil)^.Rip), Addr)) or (Addr = 0) then
      exit;

      FBreakPoints[bplSehW64Unwound].AddAddress(Addr);
      FBreakPoints[bplSehW64Unwound].SetBreak;
    end;
  end
  else
  // bplRtlUnwind
  if assigned(FBreakPoints[bplRtlUnwind]) and FBreakPoints[bplRtlUnwind].HasLocation(PC) then begin
    debugln(FPDBG_COMMANDS, ['@ bplRtlUnwind ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    AnIsFinished := False;

    // This is Win64 bit only
    // Must run for any thread => the thread may stop at a break in a finally block, and then attempt to step to except
    // maybe store the thread-id with each breakpoint // though SP register values should be unique
    Rcx := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(2).NumValue; // rsp at target
    Rdx := CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(1).NumValue;
    if (Rcx <> 0) and (Rdx <> 0) then begin
      FAddressFrameListSehW64Except.Add(Rdx, Rcx);
      FBreakPoints[bplSehW64Except].AddAddress(Rdx);
      FBreakPoints[bplSehW64Except].SetBreak;
    end;
  end
  else
  // bplSehW64Except
  if assigned(FBreakPoints[bplSehW64Except]) and FBreakPoints[bplSehW64Except].HasLocation(PC) then begin // always assigned
    debugln(FPDBG_COMMANDS, ['@ bplSehW64Except ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    if FAddressFrameListSehW64Except.Remove(PC, SP) then
      FBreakPoints[bplSehW64Except].RemoveAddress(PC);

    if (not (FState in [esStepToFinally, esSteppingFpcSpecialHandler])) and
       not(CurrentCommand is TDbgControllerHiddenBreakStepBaseCmd)
    then
      exit; // wrong command type / should not happen
    if (not CheckCommandFinishesInFrame(CurrentThread.GetStackBasePointerRegisterValue))
    then
      exit;

    AFinishLoopAndSendEvents := True; // Stop at this address
    FState := esAtWSehExcept;
    AnIsFinished := True;
    AnEventType := deFinishedStep;
  end
  else
  // bplSehW64Finally
  if assigned(FBreakPoints[bplSehW64Finally]) and FBreakPoints[bplSehW64Finally].HasLocation(PC) then begin // always assigned
    debugln(FPDBG_COMMANDS, ['@ bplSehW64Finally ', DbgSName(CurrentCommand)]);
    AFinishLoopAndSendEvents := False;
    if FAddressFrameListSehW64Finally.Remove(PC, SP) then
      FBreakPoints[bplSehW64Finally].RemoveAddress(PC);

    // At the start of a finally the BasePointer is in RCX // reg 2
    if (ACurCommand is TDbgControllerHiddenBreakStepBaseCmd) and
       not CheckCommandFinishesInFrame(CurrentThread.RegisterValueList.FindRegisterByDwarfIndex(2).NumValue)
    then
      exit;

    // step over proloque
    ACurCommand := TDbgControllerStepOverFirstFinallyLineCmd.Create(DbgController);
    FState := esStepSehFinallyProloque;
  end
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  else
    CheckSteppedOutFromW64SehFinally
  {$ENDIF}
  ;

end;

function TFpDebugExceptionStepping.BreakpointHit(var &continue: boolean;
  const Breakpoint: TFpDbgBreakpoint): boolean;
begin
  if FState in [esAtWSehExcept] then begin
    FDebugger.EnterPause(FDebugger.GetLocation);
    FState := esNone;
    exit(True);
  end;

  Result := Assigned(Breakpoint);
  if not Result then begin
    exit;
  end;

  if BreakPoint = FBreakPoints[bplRaise] then begin
    debugln(FPDBG_COMMANDS, ['@ bplRaise']);
    DoExceptionRaised(&continue);
  end
  else
  if BreakPoint = FBreakPoints[bplBreakError] then begin
    debugln(FPDBG_COMMANDS, ['@ bplBreakError']);
    FDebugger.HandleBreakError(&continue);
    if not &continue then
      FState := esNone;
  end
  else
  if BreakPoint = FBreakPoints[bplRunError] then begin
    debugln(FPDBG_COMMANDS, ['@ bplRunError']);
    FDebugger.HandleRunError(&continue);
    if not &continue then
      FState := esNone;
  end
  else
    Result := False;
end;

procedure TFpDebugExceptionStepping.UserCommandRequested(
  var ACommand: TDBGCommand);
var
  st: TExceptStepState;
begin
  // This only runs if the debugloop is paused
  st := FState;
  FState := esNone;
  DisableBreaks([bplPopExcept, bplCatches, bplReRaise,
    {$IFDEF MSWINDOWS}
    {$IFDEF WIN64}
    bplFpcSpecific, bplRtlRestoreContext, bplRtlUnwind,
    bplSehW64Finally, bplSehW64Except, bplSehW64Unwound,
    {$ENDIF}
    bplFpcExceptHandler ,bplFpcFinallyHandler, bplFpcLeaveHandler,
    bplSehW32Except, bplSehW32Finally,
    {$ENDIF}
    bplStepOut]);

  if ACommand in [dcStepInto, dcStepOver, dcStepOut, dcStepTo, dcRunTo, dcStepOverInstr{, dcStepIntoInstr}] then
    EnableBreaks([bplReRaise
      {$IFDEF MSWINDOWS}
      {$IFDEF WIN64} , bplRtlRestoreContext, bplFpcSpecific {$ENDIF}
      , bplFpcExceptHandler ,bplFpcFinallyHandler, bplFpcLeaveHandler
      , bplSehW32Except, bplSehW32Finally
      {$ENDIF}
      ]);

  case st of
    esStoppedAtRaise: begin
      if ACommand in [dcStepInto, dcStepOver, dcStepOut, dcStepTo, dcRunTo] then begin
        FState := esStepToFinally;
        ACommand := dcRun;
        FDebugger.FDbgController.&ContinueRun;
        EnableBreaks([bplPopExcept, bplCatches
          {$IFDEF MSWINDOWS}
          {$IFDEF WIN64} , bplFpcSpecific {$ENDIF}
          , bplFpcExceptHandler ,bplFpcFinallyHandler, bplFpcLeaveHandler
          , bplSehW32Except, bplSehW32Finally
          {$ENDIF}
          ]);
      end
    end;
  end;
end;


{ TFpDebugDebugger }

procedure TFpDebugDebugger.FDbgControllerProcessExitEvent(AExitCode: DWord);
var
  AThread: TFpWaitForConsoleOutputThread;
begin
  if assigned(FConsoleOutputThread) then
    begin
    AThread := TFpWaitForConsoleOutputThread(FConsoleOutputThread);
    FConsoleOutputThread := nil;
    AThread.Terminate;
    AThread.DoHasConsoleOutput(0);
    AThread.WaitFor;
    sleep(50);
    AThread.DoHasConsoleOutput(0);
    AThread.Free;
    end;

  SetExitCode(Integer(AExitCode));
  {$PUSH}{$R-}
  DoDbgEvent(ecProcess, etProcessExit, Format('Process exited with exit-code %u',[AExitCode]));
  {$POP}
  LockRelease;
  try
    StopAllWorkers;
    FreeDebugThread;
    SetState(dsStop); // after FreeDebugThread, which does ProcessMessages
  finally
    UnlockRelease;
  end;
end;

procedure TFpDebugDebugger.FDbgControllerExceptionEvent(var continue: boolean;
  const ExceptionClass, ExceptionMessage: string);
var
  ExceptItem: TBaseException;
begin
  if Exceptions.IgnoreAll then begin
    continue := True;
    exit;
  end;

  ExceptItem := Exceptions.Find(ExceptionClass);
  if (ExceptItem <> nil) and (ExceptItem.Enabled)
  then begin
    continue := True;
    exit;
  end;

  DoException(deExternal, ExceptionClass, GetLocation, ExceptionMessage, continue);
  if not continue then
    begin
    FDbgController.AbortCurrentCommand; // remove FCommand, in case any watch runs a TDbgControllerCallRoutineCmd
    SetState(dsPause);
    DoCurrent(GetLocation);
    end;
end;

function TFpDebugDebugger.GetDebugInfo: TDbgInfo;
begin
  Result := nil;
  if (FDbgController <> nil) and (FDbgController.CurrentProcess<> nil) then
    Result := FDbgController.CurrentProcess.DbgInfo;
end;

function TFpDebugDebugger.CreateLineInfo: TDBGLineInfo;
begin
  Result := TFpLineInfo.Create(Self);
end;

function TFpDebugDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TFPWatches.Create(Self);
end;

function TFpDebugDebugger.CreateThreads: TThreadsSupplier;
begin
  Result := TFPThreads.Create(Self);
end;

function TFpDebugDebugger.CreateLocals: TLocalsSupplier;
begin
  Result := TFPLocals.Create(Self);
end;

function TFpDebugDebugger.CreateRegisters: TRegisterSupplier;
begin
  Result := TFPRegisters.Create(Self);
end;

function TFpDebugDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result:=TFPCallStackSupplier.Create(Self);
end;

function TFpDebugDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result:=TFPDBGDisassembler.Create(Self);
end;

function TFpDebugDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TFPBreakPoints.Create(Self, TFPBreakpoint);
end;

procedure TFpDebugDebugger.FDbgControllerDebugInfoLoaded(Sender: TObject);
begin
  if (FDbgController <> nil) and (LineInfo <> nil) then begin
    TFpLineInfo(LineInfo).DebugInfoChanged(FDbgController.CurrentProcess);
  end;
end;

procedure TFpDebugDebugger.FDbgControllerLibraryLoaded(var continue: boolean; ALibraries: TDbgLibraryArr);
var
  n: String;
  i: Integer;
  ALib: TDbgLibrary;
begin
  for i := 0 to High(ALibraries) do begin
    ALib := ALibraries[i];
    n := ExtractFileName(ALib.Name);
    DoDbgEvent(ecModule, etModuleLoad, 'Loaded: ' + n + ' (' + ALib.Name +')');

    if ALib.DbgInfo <> nil then begin
      TFpLineInfo(LineInfo).DebugInfoChanged(ALib);
    end;
  end;
end;

procedure TFpDebugDebugger.FDbgControllerLibraryUnloaded(var continue: boolean; ALibraries: TDbgLibraryArr);
var
  n: String;
  i: Integer;
  ALib: TDbgLibrary;
begin
  for i := 0 to High(ALibraries) do
    begin
    ALib := ALibraries[i];
    n := ExtractFileName(ALib.Name);
    DoDbgEvent(ecModule, etModuleUnload, 'Unloaded: ' + n + ' (' + ALib.Name +')');
    end;
end;

procedure TFpDebugDebugger.GetCurrentThreadAndStackFrame(out AThreadId,
  AStackFrame: Integer);
var
  CurStackList: TCallStackBase;
begin
  AThreadId := Threads.CurrentThreads.CurrentThreadId;
  CurStackList := CallStack.CurrentCallStackList.EntriesForThreads[AThreadId];
  if CurStackList <> nil then begin
    AStackFrame := CurStackList.CurrentIndex;
    if AStackFrame < 0 then
      AStackFrame := 0;
  end
  else
    AStackFrame := 0;
end;

function TFpDebugDebugger.GetContextForEvaluate(const ThreadId,
  StackFrame: Integer): TFpDbgSymbolScope;
begin
  Result := FindSymbolScope(ThreadId, StackFrame);
end;

function TFpDebugDebugger.CreateMemReader: TDbgMemReader;
begin
  Result := TFpDbgMemReader.Create(self);
end;

function TFpDebugDebugger.CreateMemConverter: TFpDbgMemConvertor;
begin
  Result := TFpDbgMemConvertorLittleEndian.Create;
end;

function TFpDebugDebugger.CreateMemManager: TFpDbgMemManager;
begin
  Result := TFpDbgMemManager.Create(FMemReader, FMemConverter, FMemModel);
end;

function TFpDebugDebugger.CreateMemModel: TFpDbgMemModel;
begin
  Result := TFpDbgMemModel.Create;
end;

function TFpDebugDebugger.GetClassInstanceName(AnAddr: TDBGPtr): string;
var
  AnErr: TFpError;
begin
  Result := '';
  if (FDbgController.CurrentProcess <> nil) then
    TFpDwarfFreePascalSymbolClassMap.GetInstanceForDbgInfo(FDbgController.CurrentProcess.DbgInfo)
    .GetInstanceClassNameFromPVmt
      (AnAddr, FDbgController.DefaultContext, DBGPTRSIZE[FDbgController.CurrentProcess.Mode], @Result, nil, AnErr);
end;

procedure TFpDebugDebugger.DoThreadDebugOutput(Sender: TObject; ProcessId,
  ThreadId: Integer; AMessage: String);
begin
  FFpDebugOutputQueue.PushItem(Format('%d: %s', [ThreadId, AMessage]));
  if InterlockedExchange(FFpDebugOutputAsync, 1) <> 1 then
    Application.QueueAsyncCall(@DoDebugOutput, 0);
end;

procedure TFpDebugDebugger.DoDebugOutput(Data: PtrInt);
var
  s: string;
begin
  InterlockedExchange(FFpDebugOutputAsync, 0);
  while FFpDebugOutputQueue.PopItemTimeout(s, 50) = wrSignaled do
    EventLogHandler.LogCustomEvent(ecOutput, etOutputDebugString, s);
end;

function TFpDebugDebugger.ReadAnsiString(AnAddr: TDbgPtr): string;
var
  StrAddr: TDBGPtr;
  len: TDBGPtr;
begin
  result := '';
  if not ReadAddress(AnAddr, StrAddr) then
    Exit;
  if StrAddr = 0 then
    exit;
  ReadAddress(StrAddr-DBGPTRSIZE[FDbgController.CurrentProcess.Mode], len);
  // len > max len ....
  if (len = 0) or (len > MaxInt) then // MaxInt: not a valid string
    exit;
  if len > 16 * 1024 then
    len := 16 * 1024; // reading exception name/msg

  setlength(result, len);
  if not ReadData(StrAddr, len, result[1]) then
    result := '';
end;

procedure TFpDebugDebugger.HandleSoftwareException(out
  AnExceptionLocation: TDBGLocationRec; var continue: boolean);
var
  AnExceptionObjectLocation, ExceptIP, ExceptFramePtr: TDBGPtr;
  ExceptionClass: string;
  ExceptionMessage: string;
  ExceptItem: TBaseException;
  Offs: Integer;
  AnTObjSize: Int64;
  AnErr: TFpError;
begin
  Offs := 0;
  if not FDbgController.DefaultContext.ReadUnsignedInt(FDbgController.CurrentProcess.CallParamDefaultLocation(1),
    SizeVal(SizeOf(ExceptIP)), ExceptIP)
  then begin
    ExceptIP := 0;
    Offs := -1;
  end;
  AnExceptionLocation:=GetLocationRec(ExceptIP, Offs);

  if not FDbgController.DefaultContext.ReadUnsignedInt(FDbgController.CurrentProcess.CallParamDefaultLocation(0),
    SizeVal(SizeOf(AnExceptionObjectLocation)), AnExceptionObjectLocation)
  then
    AnExceptionObjectLocation := 0;
  ExceptionClass := '';
  ExceptionMessage := '';
  if AnExceptionObjectLocation <> 0 then begin
    if TFpDwarfFreePascalSymbolClassMap.GetInstanceForDbgInfo(FDbgController.CurrentProcess.DbgInfo)
       .GetInstanceSizeFromPVmt
       (AnExceptionObjectLocation, FDbgController.DefaultContext, DBGPTRSIZE[FDbgController.CurrentProcess.Mode], AnTObjSize, AnErr, -1)
    then
      {$PUSH}{$Q-}{$R-}
      ExceptionMessage := ReadAnsiString(AnExceptionObjectLocation+AnTObjSize);
      {$POP}


    ExceptionClass := GetClassInstanceName(AnExceptionObjectLocation);
  end;

  if Exceptions.IgnoreAll then begin
    continue := True;
    exit;
  end;

  ExceptItem := Exceptions.Find(ExceptionClass);
  if (ExceptItem <> nil) and (ExceptItem.Enabled)
  then begin
    continue := True;
    exit;
  end;

  DoException(deInternal, ExceptionClass, AnExceptionLocation, ExceptionMessage, continue);

  if not &continue then begin
    if FDbgController.DefaultContext.ReadUnsignedInt(FDbgController.CurrentProcess.CallParamDefaultLocation(2),
      SizeVal(SizeOf(ExceptFramePtr)), ExceptFramePtr)
    then
      ExceptIP := SetStackFrameForBasePtr(ExceptFramePtr, True, ExceptIP);
      if ExceptIP <> 0 then
          AnExceptionLocation:=GetLocationRec(ExceptIP); // Assert was corrected
  end;
end;

procedure TFpDebugDebugger.HandleBreakError(var continue: boolean);
var
  ErrNo: QWord;
  ExceptIP, ExceptFramePtr: TDBGPtr;
  ExceptName: string;
  ExceptItem: TBaseException;
  ExceptionLocation: TDBGLocationRec;
begin
  if not FDbgController.DefaultContext.ReadUnsignedInt(FDbgController.CurrentProcess.CallParamDefaultLocation(1),
    SizeVal(SizeOf(ExceptIP)), ExceptIP)
  then
    ExceptIP := 0;
  ExceptionLocation:=GetLocationRec(ExceptIP, -1);

  if FDbgController.DefaultContext.ReadUnsignedInt(FDbgController.CurrentProcess.CallParamDefaultLocation(0),
    SizeVal(SizeOf(LongInt)), ErrNo)
  then
    ExceptName := Format('RunError(%d)', [ErrNo])
  else begin
    ExceptName := 'RunError(unknown)';
    ErrNo := 0;
  end;

  if Exceptions.IgnoreAll then begin
    continue := True;
    exit;
  end;

  ExceptItem := Exceptions.Find(ExceptName);
  if (ExceptItem <> nil) and (ExceptItem.Enabled)
  then begin
    continue := True;
    exit;
  end;

  DoException(deRunError, ExceptName, ExceptionLocation, RunErrorText[ErrNo], continue);

  if not &continue then begin
    if FDbgController.DefaultContext.ReadUnsignedInt(FDbgController.CurrentProcess.CallParamDefaultLocation(2),
      SizeVal(SizeOf(ExceptFramePtr)), ExceptFramePtr)
    then
      SetStackFrameForBasePtr(ExceptFramePtr);

    EnterPause(ExceptionLocation);
  end;
end;

procedure TFpDebugDebugger.HandleRunError(var continue: boolean);
var
  ErrNo: QWord;
  ExceptName: string;
  ExceptItem: TBaseException;
  ExceptionLocation: TDBGLocationRec;
begin
  // NO Addr / No Frame
  ExceptionLocation:=GetLocationRec;

  if FDbgController.DefaultContext.ReadUnsignedInt(FDbgController.CurrentProcess.CallParamDefaultLocation(0),
    SizeVal(SizeOf(Word)), ErrNo)
  then
    ExceptName := Format('RunError(%d)', [ErrNo])
  else begin
    ExceptName := 'RunError(unknown)';
    ErrNo := 0;
  end;

  if Exceptions.IgnoreAll then begin
    continue := True;
    exit;
  end;

  ExceptItem := Exceptions.Find(ExceptName);
  if (ExceptItem <> nil) and (ExceptItem.Enabled)
  then begin
    continue := True;
    exit;
  end;

  DoException(deRunError, ExceptName, ExceptionLocation, RunErrorText[ErrNo], continue);

  if not &continue then begin
    EnterPause(ExceptionLocation);
  end;
end;

procedure TFpDebugDebugger.FreeDebugThread;
begin
  FWorkQueue.TerminateAllThreads(True);
  {$IFDEF FPDEBUG_THREAD_CHECK} CurrentFpDebugThreadIdForAssert := MainThreadID;{$ENDIF}
  DoProcessMessages // run the AsyncMethods
end;

procedure TFpDebugDebugger.Do_Thread_BreakStateChanged(
  Sender: TFpDbgBreakpoint; ANewState: TFpDbgBreakpointState);
var
  Info: TBreakPointUpdateInfo;
begin
  Info.InternalBreak := Sender;
  Info.NewState := ANewState;
  FBreakUpdateList.PushItem(Info);
  Application.QueueAsyncCall(@DoBreakStateChanged, 0);
end;

procedure TFpDebugDebugger.DoBreakStateChanged(Data: PtrInt);
var
  Info: TBreakPointUpdateInfo;
  ABreakPoint: TDBGBreakPoint;
begin
  while FBreakUpdateList.PopItemTimeout(Info, 0) = wrSignaled do begin
    ABreakPoint := TFPBreakpoints(BreakPoints).Find(Info.InternalBreak);
    case Info.NewState of
      bksUnknown: TFPBreakpoint(ABreakPoint).Validity := vsUnknown;
      bksOk:      TFPBreakpoint(ABreakPoint).Validity := vsValid;
      bksFailed:  TFPBreakpoint(ABreakPoint).Validity := vsInvalid;
      bksPending: TFPBreakpoint(ABreakPoint).Validity := vsPending;
    end;
  end;
end;

procedure TFpDebugDebugger.FDbgControllerHitBreakpointEvent(
  var continue: boolean; const Breakpoint: TFpDbgBreakpoint;
  AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
var
  ABreakPoint: TDBGBreakPoint;
  ALocationAddr: TDBGLocationRec;
  Context: TFpDbgSymbolScope;
  PasExpr: TFpPascalExpression;
  Opts: TFpInt3DebugBreakOptions;
  NeedInternalPause: Boolean;
begin
    // If a user single steps to an excepiton handler, do not open the dialog (there is no continue possible)
  try
    (* FExceptionStepper.BreakpointHit may call EnterPause, and with that set a location.
       In that case the EnterPause in the finally block will detect the state, and do nothing.
    *)
    if AnEventType = deBreakpoint then
      if FExceptionStepper.BreakpointHit(&continue, Breakpoint) then
        exit;

    if assigned(Breakpoint) then begin
      ABreakPoint := TFPBreakpoints(BreakPoints).Find(Breakpoint);
      if (ABreakPoint <> nil) and (ABreakPoint.Enabled) then begin

        // TODO: parse expression when breakpoin is created / so invalid expressions do not need to be handled here
        if ABreakPoint.Expression <> '' then begin
          Context := GetContextForEvaluate(FDbgController.CurrentThreadId, 0);
          if Context <> nil then begin
            PasExpr := nil;
            try
              PasExpr := TFpPascalExpression.Create(ABreakPoint.Expression, Context, True);
              PasExpr.IntrinsicPrefix := TFpDebugDebuggerProperties(GetProperties).IntrinsicPrefix;
              PasExpr.Parse;
              PasExpr.ResultValue; // trigger full validation
              if PasExpr.Valid and (svfBoolean in PasExpr.ResultValue.FieldFlags) and
                 (not PasExpr.ResultValue.AsBool) // false => do not pause
              then
                &continue := True;
            finally
              PasExpr.Free;
              Context.ReleaseReference;
            end;

            if &continue then
              exit;
          end;
        end;

        ALocationAddr := GetLocation;
        if Assigned(EventLogHandler) then
          EventLogHandler.LogEventBreakPointHit(ABreakpoint, ALocationAddr);

        NeedInternalPause := False;
        if assigned(ABreakPoint) then
          ABreakPoint.Hit(&continue, NeedInternalPause);
        FInternalPauseForEvent := FInternalPauseForEvent or NeedInternalPause;

        if (not &continue) and (ABreakPoint.Kind = bpkData) and (OnFeedback <> nil) then begin
          // For message use location(Address - 1)
          OnFeedback(self,
              Format('The Watchpoint for "%1:s" was triggered.%0:s%0:s', // 'Old value: %2:s%0:sNew value: %3:s',
                     [LineEnding, ABreakPoint.WatchData{, AOldVal, ANewVal}]),
              '', ftInformation, [frOk]);
        end;
      end
      else
        continue := True; // removed or disabled breakpoint
    end
    else
    if (AnEventType = deHardCodedBreakpoint) and (FDbgController.CurrentThread <> nil) then begin
      &continue:=true;
      Opts := TFpDebugDebuggerProperties(GetProperties).HandleDebugBreakInstruction;
      if not (dboIgnoreAll in Opts) then
        &continue:=False;
      if  continue then
        exit;
    end
    else if (AnEventType = deInternalContinue) and FQuickPause then begin
        &continue:=true;
        exit;
    end;


    if not continue then
      FPauseForEvent := True;

  finally
    if (not AMoreHitEventsPending) and (FPauseForEvent or FInternalPauseForEvent) then begin
      FQuickPause := False; // Ok, because we will SetState => RunQuickPauseTasks is not needed
      FRunQuickPauseTasks := False;

      if FPauseForEvent then
        &continue := False; // Only continue, if ALL events did say to continue

      ALocationAddr := GetLocation;
      if ALocationAddr.SrcLine = 0 then
        ALocationAddr.SrcLine := -2; // Prevent stack search for caller with source. Breakpoint hit should be at frame 0

      EnterPause(ALocationAddr, &continue);
    end;
  end;
end;

procedure TFpDebugDebugger.EnterPause(ALocationAddr: TDBGLocationRec;
  AnInternalPause: Boolean);
begin
  if AnInternalPause then begin
    if not (State in [dsPause, dsInternalPause]) then begin
      SetState(dsInternalPause);
    end;
  end
  else begin
    if State <> dsPause then begin
      FDbgController.AbortCurrentCommand; // remove FCommand, in case any watch runs a TDbgControllerCallRoutineCmd
      SetState(dsPause);
      DoCurrent(ALocationAddr);
    end;
  end;
end;

procedure TFpDebugDebugger.FDbgControllerCreateProcessEvent(var continue: boolean);
var
  addr: TDBGPtrArray;

  procedure CheckForSym(s: string);
  var
    Sym: TFpSymbol;
    l: integer;
  begin
    Sym := FDbgController.CurrentProcess.SymbolTableInfo.FindProcSymbol(s);
    if Sym <> nil then begin
      l := Length(addr);
      if IsTargetNotNil(Sym.Address) then begin
        SetLength(addr, l+1);
        addr[l] := Sym.Address.Address;
      end;
      Sym.ReleaseReference;
    end;
  end;

begin
  // This will trigger setting the breakpoints,
  // may also trigger the evaluation of the callstack or disassembler.
  FSendingEvents := True; // Let DoStateChange know that the debugger is paused
  RunQuickPauseTasks(True);
  FSendingEvents := False;

  FExceptionStepper.DoProcessLoaded;

  if assigned(OnConsoleOutput) then
    FConsoleOutputThread := TFpWaitForConsoleOutputThread.Create(self);

  case FStartupCommand of
    dcRunTo: begin
      &continue := False;
      if FDbgController.CurrentProcess.DbgInfo.HasInfo then begin
        addr:=nil;
        if FDbgController.CurrentProcess.DbgInfo.GetLineAddresses(FStartuRunToFile, FStartuRunToLine, addr, fsNext)
        then begin
          &continue := true;
          FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, addr));
        end;
      end;
      if not &continue then
        EnterPause(GetLocation);
    end;
    dcStepInto, dcStepOver: begin
      &continue := False;
      if FDbgController.CurrentProcess.SymbolTableInfo <> nil then begin
        addr:=nil;
        CheckForSym('PASCALMAIN');
        CheckForSym('MAIN');
        CheckForSym('$MAIN');
        CheckForSym('_MAIN');
        CheckForSym('MAIN_WRAPPER');
        if Length(addr) > 0 then begin
          &continue := true;
          FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, addr));
        end;
      end;
      if not &continue then
        EnterPause(GetLocation);
    end;
  end;
end;

function TFpDebugDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const; const ACallback: TMethod): Boolean;
var
  EvalFlags: TWatcheEvaluateFlags;
  AConsoleTty, ResText: string;
  addr: TDBGPtrArray;
  Cmd: TDBGCommand;
  WorkItem: TFpThreadWorkerControllerRun;
  AThreadId, AStackFrame: Integer;
  EvalWorkItem: TFpThreadWorkerCmdEval;
  WorkItemModify: TFpThreadWorkerModifyUpdate;
begin
  result := False;
  if assigned(FDbgController) then
    FDbgController.NextOnlyStopOnStartLine := TFpDebugDebuggerProperties(GetProperties).NextOnlyStopOnStartLine;

  // don't start new commands while exit event is processed
  if FDbgController.Event in [deExitProcess, deDetachFromProcess] then
    exit;

  if (ACommand in [dcRun, dcStepOver, dcStepInto, dcStepOut, dcStepTo, dcRunTo, dcJumpto,
      dcStepOverInstr, dcStepIntoInstr, dcAttach]) and
     not assigned(FDbgController.MainProcess)
  then
  begin
    try
      FExceptionStepper.DoDbgStopped; // clear any old internal breakpoints
    except
      assert(False, 'TFpDebugDebugger.RequestCommand: DoDbgStopped failed');
    end;
    FDbgController.ExecutableFilename:=FileName;
    AConsoleTty:=TFpDebugDebuggerProperties(GetProperties).ConsoleTty;
    FDbgController.ConsoleTty:=AConsoleTty;
    FDbgController.RedirectConsoleOutput:=AConsoleTty='';
    FDbgController.Params.Clear;
    if Arguments<>'' then
      CommandToList(Arguments, FDbgController.Params);
    FDbgController.WorkingDirectory:=WorkingDir;
    FDbgController.Environment:=Environment;
    {$ifdef windows}
    FDbgController.ForceNewConsoleWin:=TFpDebugDebuggerProperties(GetProperties).ForceNewConsole;
    {$endif windows}

    FDbgController.AttachToPid := 0;
    if ACommand = dcAttach then begin
      FDbgController.AttachToPid := StrToIntDef(String(AParams[0].VAnsiString), 0);
      Result := FDbgController.AttachToPid <> 0;
      if not Result then begin
        FileName := '';
        Exit;
      end;
    end;

    // Check if CreateDbgProcess returns a valid TDbgProcess
//    if ACommand <> dcAttach then begin
    if Assigned(FDbgController.CurrentProcess) then begin
      FDbgController.CurrentProcess.Config.UseConsoleWinPos    := FUseConsoleWinPos;
      FDbgController.CurrentProcess.Config.UseConsoleWinSize   := FUseConsoleWinSize;
      FDbgController.CurrentProcess.Config.UseConsoleWinBuffer := FUseConsoleWinBuffer;
      FDbgController.CurrentProcess.Config.ConsoleWinPos    := FConsoleWinPos;
      FDbgController.CurrentProcess.Config.ConsoleWinSize   := FConsoleWinSize;
      FDbgController.CurrentProcess.Config.ConsoleWinBuffer := FConsoleWinBuffer;

      FDbgController.CurrentProcess.Config.StdInRedirFile      := FileNameStdIn;
      FDbgController.CurrentProcess.Config.FileOverwriteStdIn  := FileOverwriteStdIn;
      FDbgController.CurrentProcess.Config.StdOutRedirFile     := FileNameStdOut;
      FDbgController.CurrentProcess.Config.FileOverwriteStdOut := FileOverwriteStdOut;
      FDbgController.CurrentProcess.Config.StdErrRedirFile     := FileNameStdErr;
      FDbgController.CurrentProcess.Config.FileOverwriteStdErr := FileOverwriteStdErr;

      FDbgController.CurrentProcess.Config.BreakpointSearchMaxLines := TFpDebugDebuggerProperties(GetProperties).BreakpointSearchMaxLines;

      FWorkQueue.Clear;
      FWorkQueue.ThreadCount := 1;
      {$IFDEF FPDEBUG_THREAD_CHECK} CurrentFpDebugThreadIdForAssert := FWorkQueue.Threads[0].ThreadID;{$ENDIF}
      WorkItem := TFpThreadWorkerControllerRun.Create(Self);
      FWorkQueue.PushItem(WorkItem);
      FWorkQueue.WaitForItem(WorkItem, True);
      Result := WorkItem.StartSuccesfull;
      FWorkerThreadId := WorkItem.WorkerThreadId;
      WorkItem.DecRef;
    end;

    if not result then begin
      // TDebuggerIntf.SetFileName has set the state to dsStop, to make sure
      // that dcRun could be requested. Reset the filename so that the state
      // is set to dsIdle again and is set to dsStop on the next try
      // to run.
      FileName := '';
      FreeDebugThread;

      if not IsError(FDbgController.LastError) then
        ResText := 'Error starting process in debugger'
      else
        ResText := GetFpErrorHandler.ErrorAsString(FDbgController.LastError);
      DoDbgEvent(ecProcess, etProcessExit, ResText); // or ecDebugger?
      if Assigned(OnFeedback) then
        OnFeedback(self, ResText, '', ftError, [frOk]);
      Exit;
    end;
    // TODO: any step commond should run to "main" or "pascalmain"
    // Currently disabled in TFpDebugDebugger.GetSupportedCommands
    FStartupCommand := ACommand;
    if ACommand = dcRunTo then begin
      FStartuRunToFile := AnsiString(AParams[0].VAnsiString);
      FStartuRunToLine := AParams[1].VInteger;
    end;
    StartDebugLoop(dsInit);
    exit;
  end;

  if ACommand in [dcRun, dcStop, dcStepIntoInstr, dcStepOverInstr,
                  dcStepTo, dcRunTo, dcStepOver, dcStepInto, dcStepOut, dcDetach]
  then begin
    StopAllWorkers(True);
  end;

  Cmd := ACommand;
  FExceptionStepper.UserCommandRequested(Cmd);
  case Cmd of
    dcRun:
      begin
        Result := True;
        StartDebugLoop;
      end;
    dcStop:
      begin
        FDbgController.Stop;
        if state=dsPause then
          begin
          StartDebugLoop;
          end;
        result := true;
      end;
    dcStepIntoInstr:
      begin
        FDbgController.StepIntoInstr;
        StartDebugLoop;
        result := true;
      end;
    dcStepOverInstr:
      begin
        FDbgController.StepOverInstr;
        StartDebugLoop;
        result := true;
      end;
    dcPause:
      begin
        Result := FDbgController.Pause;
      end;
    dcStepTo:
      begin
        result := false;
        if FDbgController.CurrentProcess.DbgInfo.HasInfo then
          begin
          addr:=nil;
          if FDbgController.CurrentProcess.DbgInfo.GetLineAddresses(AnsiString(AParams[0].VAnsiString), AParams[1].VInteger, addr{, fsNext})
          then begin
            result := true;
            FDbgController.InitializeCommand(TDbgControllerStepToCmd.Create(FDbgController, AnsiString(AParams[0].VAnsiString), AParams[1].VInteger));
            StartDebugLoop;
            end;
          end;
      end;
    dcRunTo:
      begin
        result := false;
        if FDbgController.CurrentProcess.DbgInfo.HasInfo then
          begin
          addr:=nil;
          if FDbgController.CurrentProcess.DbgInfo.GetLineAddresses(AnsiString(AParams[0].VAnsiString), AParams[1].VInteger, addr, fsNext)
          then begin
            result := true;
            FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, addr));
            StartDebugLoop;
            end;
          end;
      end;
    dcStepOver:
      begin
        FDbgController.InitializeCommand(TDbgControllerStepOverOrFinallyCmd.Create(FDbgController));
        StartDebugLoop;
        result := true;
      end;
    dcStepInto:
      begin
        FDbgController.Step;
        StartDebugLoop;
        result := true;
      end;
    dcStepOut:
      begin
        FDbgController.StepOut(True);
        StartDebugLoop;
        result := true;
      end;
    dcDetach:
      begin
        Result := FDbgController.Detach;
        if Result and (State in [dsPause, dsInternalPause]) then
          StartDebugLoop(State); // Keep current State
      end;
    dcEvaluate:
      begin
        EvalFlags := TWatcheEvaluateFlags(AParams[1].VInteger);
        GetCurrentThreadAndStackFrame(AThreadId, AStackFrame);
        if FEvalWorkItem <> nil then begin
          EvalWorkItem := FEvalWorkItem;
          FEvalWorkItem := nil;
          EvalWorkItem.Abort;
          EvalWorkItem.DecRef;
        end;
        if defFullTypeInfo in EvalFlags then
          FEvalWorkItem := TFpThreadWorkerCmdEval.Create(Self, twpInspect, String(AParams[0].VAnsiString),
            AStackFrame, AThreadId, EvalFlags, TDBGEvaluateResultCallback(ACallback))
        else
          FEvalWorkItem := TFpThreadWorkerCmdEval.Create(Self, twpUser, String(AParams[0].VAnsiString),
            AStackFrame, AThreadId, EvalFlags, TDBGEvaluateResultCallback(ACallback));
        FWorkQueue.PushItem(FEvalWorkItem);
        Result := True;
      end;
    dcModify:
      begin
        GetCurrentThreadAndStackFrame(AThreadId, AStackFrame);
        WorkItemModify := TFpThreadWorkerModifyUpdate.Create(Self, AnsiString(AParams[0].VAnsiString), AnsiString(AParams[1].VAnsiString),
          AStackFrame, AThreadId);
        FWorkQueue.PushItem(WorkItemModify);
        WorkItemModify.DecRef;
        Result := True;
      end;
    dcSendConsoleInput:
      begin
        FDbgController.CurrentProcess.SendConsoleInput(String(AParams[0].VAnsiString));
      end;
  end; {case}
end;

function TFpDebugDebugger.ChangeFileName: Boolean;
begin
  result := true;
end;

function TFpDebugDebugger.ExecuteInDebugThread(AMethod: TFpDbgAsyncMethod
  ): boolean;
var
  WorkItem: TFpThreadWorkerAsyncMeth;
begin
  assert(ThreadID <> FWorkerThreadId, 'TFpDebugDebugger.ExecuteInDebugThread: ThreadID <> FWorkerThreadId');
  //Result := True;
  //if ThreadID = FWorkerThreadId then begin
  //  AMethod();
  //  exit;
  //end;

  Result := False;

  WorkItem := TFpThreadWorkerAsyncMeth.Create(Self, AMethod);
  FWorkQueue.PushItem(WorkItem);
  FWorkQueue.WaitForItem(WorkItem, True);
  WorkItem.DecRef;
end;

procedure TFpDebugDebugger.StartDebugLoop(AState: TDBGState);
var
  WorkItem: TFpThreadWorkerRunLoopUpdate;
begin
  {$ifdef DBG_FPDEBUG_VERBOSE}
  DebugLn(DBG_VERBOSE, 'StartDebugLoop');
  {$endif DBG_FPDEBUG_VERBOSE}
  SetState(AState);
  WorkItem := TFpThreadWorkerRunLoopUpdate.Create(Self);
  FWorkQueue.PushItem(WorkItem);
  WorkItem.DecRef;
end;

procedure TFpDebugDebugger.DebugLoopFinished(Data: PtrInt);
var
  Cont: boolean;
  WorkItem: TFpThreadWorkerRunLoopAfterIdleUpdate;
  c: Integer;
begin
  LockRelease;
  try
    {$ifdef DBG_FPDEBUG_VERBOSE}
    DebugLn(DBG_VERBOSE, 'DebugLoopFinished');
    {$endif DBG_FPDEBUG_VERBOSE}

    (* Need to ensure CurrentThreadId is correct,
       because any callstack (never mind which to which IDE-thread object it belongs
       will always get the data for the current thread only
     TODO: callstacks need a field with the thread-id to which they belong *)
    if (Threads <> nil) and (Threads.CurrentThreads <> nil) and
       (FDbgController.CurrentThread <> nil)
    then
      Threads.CurrentThreads.CurrentThreadId := FDbgController.CurrentThreadId;

    FPauseForEvent := False;
    FInternalPauseForEvent := False;
    FSendingEvents := True;
    try
      FDbgController.SendEvents(Cont); // This may free the TFpDebugDebugger (self)
      if State = dsRun then
        RunQuickPauseTasks;
    finally
      FSendingEvents := False;
    end;

    FQuickPause:=false;

    if Cont then begin
      if State in [dsPause, dsInternalPause] then begin
        FWorkQueue.Lock;
        CheckAndRunIdle;
        (* IdleThreadCount could (race condition) be to high.
           Then DebugHistory may loose ONE item. (only one working thread.
           Practically this is unlikely, since the thread had time to set
           the count, since the Lock started.
        *)
        c := FWorkQueue.Count + FWorkQueue.ThreadCount - FWorkQueue.IdleThreadCount;
        FWorkQueue.Unlock;
        if c = 0 then
          DoProcessMessages;
      end
      else
        c := 0;

      if c = 0 then begin
        StartDebugLoop;
      end
      else begin
        WorkItem := TFpThreadWorkerRunLoopAfterIdleUpdate.Create(Self);
        FWorkQueue.PushItem(WorkItem);
        WorkItem.DecRef;
      end;
    end;
  finally
    UnlockRelease;
  end;
end;

procedure TFpDebugDebugger.QuickPause;
begin
  FQuickPause:=FDbgController.Pause;
end;

procedure TFpDebugDebugger.RunQuickPauseTasks(AForce: Boolean);
begin
  if AForce or
     FQuickPause or
     FRunQuickPauseTasks
  then
    TFPBreakpoints(Breakpoints).DoStateChange(dsRun);

  FRunQuickPauseTasks :=false;
  // FQuickPause will be reset by caller
end;

procedure TFpDebugDebugger.DoRelease;
begin
  DebugLn(DBG_VERBOSE, ['++++ dorelase  ', Dbgs(ptrint(FDbgController)), dbgs(state)]);
  if FWorkQueue <> nil then
    FWorkQueue.OnQueueIdle := nil;
//  SetState(dsDestroying);
  if (State <> dsDestroying) and //assigned(FFpDebugThread) and //???
     (FDbgController <> nil) and (FDbgController.MainProcess <> nil)
  then begin
    FDbgController.Stop;
    FDbgControllerProcessExitEvent(0); // Force exit;
  end;

  inherited DoRelease;
end;

procedure TFpDebugDebugger.CheckAndRunIdle;
begin
  if (not (State in [dsPause, dsInternalPause])) or
     (not Assigned(OnIdle)) or
     (FWorkQueue.Count <> 0)
  then
    exit;

  DebugLnEnter(DBG_VERBOSE, ['>> TFpDebugDebugger.CheckAndRunIdle ']);
  FIsIdle := True;
  try
    OnIdle(Self);
  except
    on E: Exception do
      DebugLn(['exception during idle ', E.ClassName, ': ', E.Message]);
  end;
  FIsIdle := False;
  DebugLnExit(DBG_VERBOSE, ['<< TFpDebugDebugger.CheckAndRunIdle ']);
end;

procedure TFpDebugDebugger.DoBeforeState(const OldState: TDBGState);
var
  EvalWorkItem: TFpThreadWorkerCmdEval;
begin
  if not (State in [dsPause, dsInternalPause]) then begin
    TFPThreads(Threads).StopWorkes;
    TFPCallStackSupplier(CallStack).StopWorkes;
    TFPWatches(Watches).StopWorkes;
    TFPLocals(Locals).StopWorkes;

    if FEvalWorkItem <> nil then begin
      EvalWorkItem := FEvalWorkItem;
      FEvalWorkItem := nil;
      EvalWorkItem.Abort;
      EvalWorkItem.DecRef;
    end;
  end;

  inherited DoBeforeState(OldState);
end;

procedure TFpDebugDebugger.DoState(const OldState: TDBGState);
begin
  LockRelease;
  try
    inherited DoState(OldState);
  finally
    UnlockRelease;
  end;
end;

function TFpDebugDebugger.GetIsIdle: Boolean;
begin
  Result := (FWorkQueue.Count = 0) or FIsIdle;
end;

procedure TFpDebugDebugger.DoAddBreakFuncLib;
begin
  FCacheBreakpoint := FDbgController.CurrentProcess.AddBreak(FCacheFileName, FCacheBoolean, FCacheLib, True);
end;

procedure TFpDebugDebugger.DoAddBreakLocation;
begin
  if FCacheLocation = 0 then
    FCacheBreakpoint := FDbgController.CurrentProcess.AddBreak(nil, FCacheBoolean)
  else
    FCacheBreakpoint := FDbgController.CurrentProcess.AddBreak(FCacheLocation, FCacheBoolean);
end;

procedure TFpDebugDebugger.DoReadData;
begin
  FCacheBoolean:=FDbgController.CurrentProcess.ReadData(FCacheLocation, FCacheLine, FCachePointer^);
end;

procedure TFpDebugDebugger.DoReadPartialData;
begin
  FCacheBoolean:=FDbgController.CurrentProcess.ReadData(FCacheLocation, FCacheLine, FCachePointer^, FCacheBytesRead);
end;

procedure TFpDebugDebugger.DoFindContext;
begin
  FCacheContext := FDbgController.CurrentProcess.FindSymbolScope(FCacheThreadId, FCacheStackFrame);
end;

procedure TFpDebugDebugger.DoSetStackFrameForBasePtr;
begin
  FDbgController.CurrentThread.PrepareCallStackEntryList(7);
  if (FCacheLocation = 0) and (FCacheLocation2 <> 0) then
    FCacheStackFrame := FDbgController.CurrentThread.FindCallStackEntryByInstructionPointer(FCacheLocation2, 15, 1)
  else
    FCacheStackFrame := FDbgController.CurrentThread.FindCallStackEntryByBasePointer(FCacheLocation, 30, 1);
end;

function TFpDebugDebugger.AddBreak(const ALocation: TDbgPtr; AnEnabled: Boolean
  ): TFpDbgBreakpoint;
begin
  // Shortcut, if in debug-thread / do not use Self.F*
  if ThreadID = FWorkerThreadId then
    if ALocation = 0 then exit(FDbgController.CurrentProcess.AddBreak(nil, AnEnabled))
                     else exit(FDbgController.CurrentProcess.AddBreak(ALocation, AnEnabled));

  FCacheLocation:=ALocation;
  FCacheBoolean:=AnEnabled;
  FCacheBreakpoint := nil;
  ExecuteInDebugThread(@DoAddBreakLocation);
  result := FCacheBreakpoint;
end;

function TFpDebugDebugger.AddBreak(const AFuncName: String; ALib: TDbgLibrary;
  AnEnabled: Boolean): TFpDbgBreakpoint;
begin
  // Shortcut, if in debug-thread / do not use Self.F*
  if ThreadID = FWorkerThreadId then
    exit(FDbgController.CurrentProcess.AddBreak(AFuncName, AnEnabled, ALib));

  FCacheFileName:=AFuncName;
  FCacheLib:=ALib;
  FCacheBoolean:=AnEnabled;
  FCacheBreakpoint := nil;
  ExecuteInDebugThread(@DoAddBreakFuncLib);
  result := FCacheBreakpoint;
end;

function TFpDebugDebugger.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
begin
  // Shortcut, if in debug-thread / do not use Self.F*
  if ThreadID = FWorkerThreadId then
    exit(FDbgController.CurrentProcess.ReadData(AAdress, ASize, AData));

  FCacheLocation := AAdress;
  FCacheLine:=ASize;
  FCachePointer := @AData;
  FCacheBoolean := False;
  ExecuteInDebugThread(@DoReadData);
  result := FCacheBoolean;
end;

function TFpDebugDebugger.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData; out ABytesRead: Cardinal): Boolean;
begin
  // Shortcut, if in debug-thread / do not use Self.F*
  if ThreadID = FWorkerThreadId then
    exit(FDbgController.CurrentProcess.ReadData(AAdress, ASize, AData, ABytesRead));

  FCacheLocation := AAdress;
  FCacheLine:=ASize;
  FCachePointer := @AData;
  FCacheBoolean := False;
  FCacheBytesRead := 0;
  ExecuteInDebugThread(@DoReadPartialData);
  result := FCacheBoolean;
  ABytesRead := FCacheBytesRead;
end;

function TFpDebugDebugger.ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean;
var
  dw: DWord;
  qw: QWord;
begin
  case FDbgController.CurrentProcess.Mode of
    dm32:
      begin
        result := ReadData(AAdress, sizeof(dw), dw);
        AData:=dw;
      end;
    dm64:
      begin
        result := ReadData(AAdress, sizeof(qw), qw);
        AData:=qw;
      end;
  end;
end;

function TFpDebugDebugger.SetStackFrameForBasePtr(ABasePtr: TDBGPtr;
  ASearchAssert: boolean; CurAddr: TDBGPtr): TDBGPtr;
const
  SYS_ASSERT_NAME = 'SYSUTILS_$$_ASSERT'; // AssertErrorHandler, in case the assert is hidden in the stack
var
  f: Integer;
  CList: TDbgCallstackEntryList;
  P: TFpSymbol;
begin
  assert(GetCurrentThreadId=MainThreadID, 'TFpDebugDebugger.SetStackFrameForBasePtr: GetCurrentThreadId=MainThreadID');

  Result := 0;
  if FDbgController.CurrentThread = nil then
    exit;
  FCacheLocation:=ABasePtr;
  FCacheLocation2:=CurAddr;
  ExecuteInDebugThread(@DoSetStackFrameForBasePtr);
  f := FCacheStackFrame;

  if (f >= 2) and ASearchAssert and (ABasePtr <> 0) then begin
    // stack is already prepared / exe in thread not needed
    CList := FDbgController.CurrentThread.CallStackEntryList;
    if (CList[f].AnAddress = CurAddr) then begin
      P := CList[f-2].ProcSymbol;
      if (P <> nil) and
         ( (P.Name = 'FPC_ASSERT') or (P.Name = 'fpc_assert') or
           (P.Name = 'ASSERT') or (P.Name = 'assert') or
           (CompareText(copy(P.Name, 1, length(SYS_ASSERT_NAME)), SYS_ASSERT_NAME) = 0) )
      then begin
        dec(f);
        Result := CList[f].AnAddress - 1;
      end;
    end;
  end
  else
  if (ABasePtr = 0) and (CurAddr <> 0) and (f > 0) then begin
    Result := CurAddr - 1; // found address on stack, so this is return address
  end;

  if f > 0 then begin
    TFPCallStackSupplier(CallStack).FThreadForInitialFrame := FDbgController.CurrentThread.ID;
    TFPCallStackSupplier(CallStack).FInitialFrame := f;
  end;
end;

function TFpDebugDebugger.FindSymbolScope(AThreadId, AStackFrame: Integer): TFpDbgSymbolScope;
begin
  if ThreadID = FWorkerThreadId then
    exit(FDbgController.CurrentProcess.FindSymbolScope(AThreadId, AStackFrame));

  assert(GetCurrentThreadId=MainThreadID, 'TFpDebugDebugger.FindSymbolScope: GetCurrentThreadId=MainThreadID');
  FCacheThreadId := AThreadId;
  FCacheStackFrame := AStackFrame;
  FCacheContext := nil;
  ExecuteInDebugThread(@DoFindContext);
  Result := FCacheContext;
end;

procedure TFpDebugDebugger.StopAllWorkers(AWait: Boolean);
begin
  TFPThreads(Threads).StopWorkes;
  TFPCallStackSupplier(CallStack).StopWorkes;
  TFPWatches(Watches).StopWorkes;
  TFPLocals(Locals).StopWorkes;
  if FEvalWorkItem <> nil then begin
    FEvalWorkItem.Abort;
    if AWait then
      WorkQueue.RemoveItem(FEvalWorkItem);
    FEvalWorkItem.DecRef;
    FEvalWorkItem := nil;
  end;

  if AWait then begin
    TFPThreads(Threads).FThreadWorkers.WaitForWorkers(True);
    TFPCallStackSupplier(CallStack).FCallStackWorkers.WaitForWorkers(True);
    TFPWatches(Watches).FWatchEvalWorkers.WaitForWorkers(True);
    TFPLocals(Locals).FLocalWorkers.WaitForWorkers(True);
  end;
end;

function TFpDebugDebugger.IsPausedAndValid: boolean;
begin
  Result := False;
  if self = nil then
    exit;
  Result := (State in [dsPause, dsInternalPause]) and
            (FDbgController <> nil) and
            (FDbgController.CurrentProcess <> nil);
end;

procedure TFpDebugDebugger.DoProcessMessages;
begin
  try
    Application.ProcessMessages;
  except
    on E: Exception do debugln(['Application.ProcessMessages crashed with ', E.Message]);
  end;
end;

constructor TFpDebugDebugger.Create(const AExternalDebugger: String);
begin
  ProcessMessagesProc := @DoProcessMessages;
  inherited Create(AExternalDebugger);
  FSuspendedThreads := TThreadIdList.Create;
  FLockList := TFpDbgLockList.Create;
  FWorkQueue := TFpThreadPriorityWorkerQueue.Create(100);
  FWorkQueue.OnQueueIdle := @CheckAndRunIdle;
  FFpDebugOutputQueue := TFpDebugStringQueue.create(100);
  FBreakUpdateList := TBreakPointUpdateList.create();
  FExceptionStepper := TFpDebugExceptionStepping.Create(Self);
  FPrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
  FMemReader    := CreateMemReader;
  FMemConverter := CreateMemConverter;
  FMemModel     := CreateMemModel;
  FMemManager   := CreateMemManager;
  FMemManager.MemLimits.MaxMemReadSize := TFpDebugDebuggerProperties(GetProperties).MemLimits.MaxMemReadSize;
  FMemManager.MemLimits.MaxArrayLen := TFpDebugDebuggerProperties(GetProperties).MemLimits.MaxArrayLen;
  FMemManager.MemLimits.MaxStringLen := TFpDebugDebuggerProperties(GetProperties).MemLimits.MaxStringLen;
  FMemManager.MemLimits.MaxNullStringSearchLen := TFpDebugDebuggerProperties(GetProperties).MemLimits.MaxNullStringSearchLen;
  FDbgController := TDbgController.Create(FMemManager, FMemModel);
  FDbgController.OnCreateProcessEvent:=@FDbgControllerCreateProcessEvent;
  FDbgController.OnHitBreakpointEvent:=@FDbgControllerHitBreakpointEvent;
  FDbgController.OnProcessExitEvent:=@FDbgControllerProcessExitEvent;
  FDbgController.OnExceptionEvent:=@FDbgControllerExceptionEvent;
  FDbgController.OnDebugInfoLoaded := @FDbgControllerDebugInfoLoaded;
  FDbgController.OnLibraryLoadedEvent := @FDbgControllerLibraryLoaded;
  FDbgController.OnLibraryUnloadedEvent := @FDbgControllerLibraryUnloaded;
  FDbgController.OnThreadDebugOutputEvent  := @DoThreadDebugOutput;
  FDbgController.NextOnlyStopOnStartLine := TFpDebugDebuggerProperties(GetProperties).NextOnlyStopOnStartLine;

  FDbgController.OnThreadProcessLoopCycleEvent:=@FExceptionStepper.ThreadProcessLoopCycle;
  FDbgController.OnThreadBeforeProcessLoop:=@FExceptionStepper.ThreadBeforeLoop;
end;

destructor TFpDebugDebugger.Destroy;
begin
  FWorkQueue.OnQueueIdle := nil;
  FWorkQueue.DoShutDown;
  StopAllWorkers;
  FWorkQueue.TerminateAllThreads(False);

  if state in [dsPause, dsInternalPause] then
    try
      SetState(dsStop);
    except
    end;
  FWorkQueue.TerminateAllThreads(True);
  DoProcessMessages; // run the AsyncMethods
  {$IFDEF FPDEBUG_THREAD_CHECK} CurrentFpDebugThreadIdForAssert := MainThreadID;{$ENDIF}

  Application.RemoveAsyncCalls(Self);

  FreeAndNil(FFpDebugOutputQueue);
  FreeAndNil(FBreakUpdateList);
  FreeAndNil(FDbgController);
  try
    FExceptionStepper.DoDbgStopped;
  except
    assert(False, 'TFpDebugDebugger.Destroy: DoDbgStopped failed');
  end;

  FreeAndNil(FPrettyPrinter);
  FreeAndNil(FMemModel);
  FreeAndNil(FMemManager);
  FreeAndNil(FMemConverter);
  FreeAndNil(FMemReader);
  FreeAndNil(FExceptionStepper);
  inherited Destroy;
  FreeAndNil(FWorkQueue);
  FreeAndNil(FLockList);
  FreeAndNil(FSuspendedThreads);
end;

function TFpDebugDebugger.GetLocationRec(AnAddress: TDBGPtr;
  AnAddrOffset: Integer): TDBGLocationRec;
var
  sym, symproc: TFpSymbol;
begin
  result.FuncName:='';
  result.SrcFile:='';
  result.SrcFullName:='';
  result.SrcLine:=0;

  if Assigned(FDbgController.CurrentProcess) then
    begin
    if AnAddress=0 then
      result.Address := FDbgController.DefaultContext.Address // DefaultContext has the InstrPtr cached
      //result.Address := FDbgController.CurrentThread.GetInstructionPointerRegisterValue
    else
      result.Address := AnAddress;

    {$PUSH}{$R-}{$Q-}
    sym := FDbgController.CurrentProcess.FindProcSymbol(result.Address + AnAddrOffset);
    {$POP}
    if sym = nil then
      Exit;

    result.SrcFile := ExtractFileName(sym.FileName);
    result.SrcLine := sym.Line;
    result.SrcFullName := sym.FileName;

    symproc := sym;
    //while not (symproc.kind in [skProcedure, skFunction]) do
    //  symproc := symproc.Parent;

    if assigned(symproc) then
      result.FuncName:=symproc.Name;
    sym.ReleaseReference;
    end;
end;

function TFpDebugDebugger.GetLocation: TDBGLocationRec;
begin
  Result:=GetLocationRec;
end;

procedure TFpDebugDebugger.ThreadHandleBreakPointInCallRoutine(
  AnAddress: TDBGPtr; out ACanContinue: Boolean);
begin
  with FExceptionStepper do
    ACanContinue := not(
      ( (FBreakPoints[bplRaise]      <> nil) and FBreakPoints[bplRaise].HasLocation(AnAddress) ) or
      ( (FBreakPoints[bplReRaise]    <> nil) and FBreakPoints[bplReRaise].HasLocation(AnAddress) ) or
      ( (FBreakPoints[bplBreakError] <> nil) and FBreakPoints[bplBreakError].HasLocation(AnAddress) ) or
      ( (FBreakPoints[bplRunError]   <> nil) and FBreakPoints[bplRunError].HasLocation(AnAddress) )
    );
end;

procedure TFpDebugDebugger.BeforeWatchEval(ACallContext: TFpDbgInfoCallContext);
begin
  ACallContext.OnCallRoutineHitBreapoint := @ThreadHandleBreakPointInCallRoutine;

  FExceptionStepper.DisableBreaks([bplPopExcept, bplCatches, //bplReRaise,
    {$IFDEF MSWINDOWS}
    {$IFDEF WIN64}
    bplFpcSpecific, bplRtlRestoreContext, bplRtlUnwind,
    bplSehW64Finally, bplSehW64Except, bplSehW64Unwound,
    {$ENDIF}
    bplFpcExceptHandler ,bplFpcFinallyHandler, bplFpcLeaveHandler,
    bplSehW32Except, bplSehW32Finally,
    {$ENDIF}
    bplStepOut]);
end;

procedure TFpDebugDebugger.RunProcessLoop(OnlyCurrentThread: Boolean);
var
  ct, t: TDbgThread;
begin
  ct := FDbgController.CurrentThread;

  if OnlyCurrentThread then
    for t in FDbgController.CurrentProcess.ThreadMap do
      if t <> ct then
        t.IncSuspendCount;

  FDbgController.ProcessLoop;

  if OnlyCurrentThread then
    for t in FDbgController.CurrentProcess.ThreadMap do
      if (t <> ct) and (t.SuspendCount > 0) then // new threads will have count=0
        t.DecSuspendCount;

  if FDbgController.Event in [deExitProcess, deDetachFromProcess] then
    Application.QueueAsyncCall(@DebugLoopFinished, 0);
end;

procedure TFpDebugDebugger.SetConsoleWinPos(ALeft, ATop: Integer);
begin
  FUseConsoleWinPos := True;
  FConsoleWinPos.X   := ALeft;
  FConsoleWinPos.Y    := ATop;
end;

procedure TFpDebugDebugger.UnSetConsoleWinPos;
begin
  FUseConsoleWinPos := False;
end;

procedure TFpDebugDebugger.SetConsoleWinSize(AWidth, AHeight: Integer);
begin
  FUseConsoleWinSize := True;
  FConsoleWinSize.X   := AWidth;
  FConsoleWinSize.Y    := AHeight;
end;

procedure TFpDebugDebugger.UnSetConsoleWinSize;
begin
  FUseConsoleWinSize := False;
end;

procedure TFpDebugDebugger.SetConsoleWinBuffer(AColumns, ARows: Integer);
begin
  FUseConsoleWinBuffer := True;
  FConsoleWinBuffer.X   := AColumns;
  FConsoleWinBuffer.Y    := ARows;
end;

procedure TFpDebugDebugger.UnSetConsoleWinBuffer;
begin
  FUseConsoleWinBuffer := False;
end;

class function TFpDebugDebugger.Caption: String;
begin
  Result:='FpDebug internal Dwarf-debugger';
end;

class function TFpDebugDebugger.NeedsExePath: boolean;
begin
  Result:=False;
end;

class function TFpDebugDebugger.RequiredCompilerOpts(ATargetCPU, ATargetOS: String): TDebugCompilerRequirements;
begin
  {$ifdef CD_Cocoa}{$DEFINE MacOS}
  if ATargetCPU = '' then ATargetCPU := 'x86_64';
  {$ENDIF}
  {$IFDEF Darwin}{$DEFINE MacOS}
  if ATargetCPU = '' then ATargetCPU := 'i386';
  {$ENDIF}
  {$IFDEF MacOs}
  if LowerCase(ATargetCPU) = 'i386' then
    Result:=[dcrDwarfOnly] // carbon
  else
    Result:=[dcrExternalDbgInfoOnly, dcrDwarfOnly]; // cocoa
  {$ELSE}
  Result:=[dcrDwarfOnly];
  {$ENDIF}
  Result := Result + [dcrPreferDwarf3];
end;

class function TFpDebugDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TFpDebugDebuggerProperties.Create;
end;

function TFpDebugDebugger.GetCommands: TDBGCommands;
begin
  Result := inherited GetCommands;
  if State in [dsStop, dsIdle] then
    Result := Result - [dcStepOut, dcStepIntoInstr, dcStepOverInstr];
end;

procedure TFpDebugDebugger.LockCommandProcessing;
begin
  //inherited LockCommandProcessing;
//  FWorkQueue.Lock;
end;

procedure TFpDebugDebugger.UnLockCommandProcessing;
begin
  //inherited UnLockCommandProcessing;
//  FWorkQueue.Unlock;
end;

class function TFpDebugDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result:=[dcRun, dcStop, dcStepIntoInstr, dcStepOverInstr, dcStepOver,
           dcStepTo, dcRunTo, dcPause, dcStepOut, dcStepInto, dcEvaluate, dcModify,
           dcSendConsoleInput
           {$IFDEF windows} , dcAttach, dcDetach {$ENDIF}
           {$IFDEF linux} , dcAttach, dcDetach {$ENDIF}
          ];
end;

class function TFpDebugDebugger.SupportedCommandsFor(AState: TDBGState
  ): TDBGCommands;
begin
  Result := inherited SupportedCommandsFor(AState);
  if AState = dsStop then
    Result := Result - [dcStepOut, dcStepIntoInstr, dcStepOverInstr];
end;

class function TFpDebugDebugger.SupportedFeatures: TDBGFeatures;
begin
  {$IF (defined(windows) or defined(linux)) and
       (defined(CPU386) or defined(CPUI386) or defined(CPUX86_64) or defined(CPUX64))
  }
  Result := [dfEvalFunctionCalls, dfThreadSuspension];
    {$IFDEF windows}
    Result := Result + [dfConsoleWinPos];
    {$ENDIF}
    if DBG_PROCESS_HAS_REDIRECT then
      Result := Result + [dfStdInOutRedirect];
  {$ELSE}
  Result := [dfNotSuitableForOsArch];
  {$ENDIF}
end;

initialization
  {$IFOPT T-}
  RegisterDebugger(TFpDebugDebugger);
  {$ENDIF}

  DBG_VERBOSE     := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS    := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  DBG_BREAKPOINTS := DebugLogger.FindOrRegisterLogGroup('DBG_BREAKPOINTS' {$IFDEF DBG_BREAKPOINTS} , True {$ENDIF} );
  FPDBG_COMMANDS  := DebugLogger.FindOrRegisterLogGroup('FPDBG_COMMANDS' {$IFDEF FPDBG_COMMANDS} , True {$ENDIF} );

end.


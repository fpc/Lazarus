{ $Id$ }
{                        ----------------------------------------------
                         GDBDebugger.pp  -  Debugger class forGDB
                         ----------------------------------------------

 @created(Wed Feb 23rd WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains debugger class for the GDB/MI debugger.


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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit GDBMIDebugger;

{$mode objfpc}
{$H+}

{$IFDEF GDMI_QUEUE_DEBUG}{$DEFINE DBGMI_QUEUE_DEBUG}{$ENDIF} // temporary, since renamed/spelling
{$IFDEF linux} {$DEFINE DBG_ENABLE_TERMINAL} {$ENDIF}

interface

uses
  Classes, SysUtils, Controls, Math, Variants, LCLProc, Dialogs, DebugUtils,
  Debugger, FileUtil, CmdLineDebugger, GDBTypeInfo, Maps, LCLIntf,
{$IFdef MSWindows}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
   Unix,BaseUnix,termio,
{$ENDIF}
{$IFDEF DBG_ENABLE_TERMINAL}
   PseudoTerminalDlg,
{$ENDIF}
  BaseDebugManager, GDBMIMiscClasses;

type
  TGDBMIProgramInfo = record
    State: TDBGState;
    BreakPoint: Integer; // ID of Breakpoint hit
    Signal: Integer;     // Signal no if we hit one
    SignalText: String;  // Signal text if we hit one
  end;

  TGDBMICmdFlags = set of (
    cfNoMiCommand, // the command is not a MI command
    cfIgnoreState, // ignore the result state of the command
    cfIgnoreError, // ignore errors
    cfExternal     // the command is a result from a user action
  );

  // The internal ExecCommand of the new Commands (object queue)
  TGDBMICommandFlag = (
    cfCheckState, // Copy CmdResult to DebuggerState, EXCEPT dsError,dsNone (e.g copy dsRun, dsPause, dsStop, dsIdle)
    cfCheckError  // Copy CmdResult to DebuggerState, ONLY if dsError
  );
  TGDBMICommandFlags = set of TGDBMICommandFlag;


  TGDBMICallback = procedure(const AResult: TGDBMIExecResult; const ATag: PtrInt) of object;
  TGDBMIPauseWaitState = (pwsNone, pwsInternal, pwsExternal);

  TGDBMITargetFlag = (
    tfHasSymbols,     // Debug symbols are present
    tfRTLUsesRegCall, // the RTL is compiled with RegCall calling convention
    tfClassIsPointer,  // with dwarf class names are pointer. with stabs they are not
    tfExceptionIsPointer, // Can happen, if stabs and dwarf are mixed
    tfFlagHasTypeObject,
    tfFlagHasTypeException,
    tfFlagHasTypeShortstring,
    //tfFlagHasTypePShortString,
    tfFlagHasTypePointer,
    tfFlagHasTypeByte
    //tfFlagHasTypeChar
  );
  TGDBMITargetFlags = set of TGDBMITargetFlag;

  TGDBMIDebuggerFlags = set of (
    dfImplicidTypes,     // Debugger supports implicit types (^Type)
    dfForceBreak,        // Debugger supports insertion of not yet known brekpoints
    dfForceBreakDetected
  );

  // Target info
  TGDBMITargetInfo = record
    TargetPID: Integer;
    TargetFlags: TGDBMITargetFlags;
    TargetCPU: String;
    TargetOS: String;
    TargetRegisters: array[0..2] of String;
    TargetPtrSize: Byte; // size in bytes
    TargetIsBE: Boolean;
  end;
  PGDBMITargetInfo = ^TGDBMITargetInfo;

  TConvertToGDBPathType = (cgptNone, cgptCurDir, cgptExeName);

  TGDBMIDebuggerFilenameEncoding = (
    gdfeNone, gdfeDefault, gdfeEscSpace, gdfeQuote
  );

  { TGDBMIDebuggerProperties }

  TGDBMIDebuggerProperties = class(TDebuggerProperties)
  private
    FEncodeCurrentDirPath: TGDBMIDebuggerFilenameEncoding;
    FEncodeExeFileName: TGDBMIDebuggerFilenameEncoding;
    {$IFDEF UNIX}
    FConsoleTty: String;
    {$ENDIF}
    FGDBOptions: String;
    FTimeoutForEval: Integer;
    FWarnOnTimeOut: Boolean;
    procedure SetTimeoutForEval(const AValue: Integer);
    procedure SetWarnOnTimeOut(const AValue: Boolean);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Debugger_Startup_Options: String read FGDBOptions write FGDBOptions;
    {$IFDEF UNIX}
    property ConsoleTty: String read FConsoleTty write FConsoleTty;
    {$ENDIF}
    property TimeoutForEval: Integer read FTimeoutForEval write SetTimeoutForEval;
    property WarnOnTimeOut: Boolean  read FWarnOnTimeOut write SetWarnOnTimeOut;
    property EncodeCurrentDirPath: TGDBMIDebuggerFilenameEncoding
             read FEncodeCurrentDirPath write FEncodeCurrentDirPath default gdfeDefault;
    property EncodeExeFileName: TGDBMIDebuggerFilenameEncoding
             read FEncodeExeFileName write FEncodeExeFileName default gdfeDefault;
  end;

  TGDBMIDebugger = class;

  { TGDBMIDebuggerCommand }

  TGDBMIDebuggerCommandState =
    ( dcsNone,         // Initial State
      dcsQueued,       // [None] => Queued behind other commands
      dcsExecuting,    // [None, Queued] => currently running
      // Final States, those lead to the object being freed, unless it still is referenced (Add/Release-Reference)
      dcsFinished,     // [Executing] => Finished Execution
      dcsCanceled,     // [Queued] => Never Executed
      // Flags, for Seenstates
      dcsInternalRefReleased // The internal reference has been released
    );
  TGDBMIDebuggerCommandStates = set of TGDBMIDebuggerCommandState;

  TGDBMIDebuggerCommandProperty = (dcpCancelOnRun);
  TGDBMIDebuggerCommandProperts = set of TGDBMIDebuggerCommandProperty;

  TGDBMIExecCommandType =
    ( ectContinue,         // -exec-continue
      ectRun,              // -exec-run
      ectRunTo,            // -exec-until [Source, Line]
      ectStepOver,         // -exec-next
      ectStepOut,          // -exec-finish
      ectStepInto,         // -exec-step
      // not yet used
      ectStepOverInstruction,  // -exec-next-instruction
      ectStepIntoInstruction,  // -exec-step-instruction
      ectReturn            // -exec-return (step out immediately, skip execution)
    );

  TGDBMIBreakpointReason = (gbrBreak, gbrWatchTrigger, gbrWatchScope);

  TGDBMIDebuggerCommand = class(TRefCountedObject)
  private
    FDefaultTimeOut: Integer;
    FLastExecwasTimeOut: Boolean;
    FOnCancel: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnExecuted: TNotifyEvent;
    FPriority: Integer;
    FProcessResultTimedOut: Boolean;
    FProperties: TGDBMIDebuggerCommandProperts;
    FQueueRunLevel: Integer;
    FState : TGDBMIDebuggerCommandState;
    FSeenStates: TGDBMIDebuggerCommandStates;
    FTheDebugger: TGDBMIDebugger; // Set during Execute
    FLastExecCommand: String;
    FLastExecResult: TGDBMIExecResult;
    FLogWarnings: String;
    function GetDebuggerProperties: TGDBMIDebuggerProperties;
    function GetDebuggerState: TDBGState;
    function GetTargetInfo: PGDBMITargetInfo;
  protected
    procedure SetDebuggerState(const AValue: TDBGState);
    procedure SetDebuggerErrorState(const AMsg: String; const AInfo: String = '');
    function  ErrorStateMessage: String; virtual;
    function  ErrorStateInfo: String; virtual;
    property  DebuggerState: TDBGState read GetDebuggerState;
    property  DebuggerProperties: TGDBMIDebuggerProperties read GetDebuggerProperties;
    property  TargetInfo: PGDBMITargetInfo read GetTargetInfo;
  protected
    procedure SetCommandState(NewState: TGDBMIDebuggerCommandState);
    procedure DoStateChanged(OldState: TGDBMIDebuggerCommandState); virtual;
    procedure DoLockQueueExecute; virtual;
    procedure DoUnockQueueExecute; virtual;
    function  DoExecute: Boolean; virtual; abstract;
    procedure DoOnExecuted;
    procedure DoCancel; virtual;
    procedure DoOnCanceled;
    property  SeenStates: TGDBMIDebuggerCommandStates read FSeenStates;
    property  QueueRunLevel: Integer read FQueueRunLevel write FQueueRunLevel;  // if queue is nested
  protected
    // ExecuteCommand does execute direct. It does not use the queue
    function  ExecuteCommand(const ACommand: String;
                             AFlags: TGDBMICommandFlags = [];
                             ATimeOut: Integer = -1
                            ): Boolean; overload;
    function  ExecuteCommand(const ACommand: String;
                             out AResult: TGDBMIExecResult;
                             AFlags: TGDBMICommandFlags = [];
                             ATimeOut: Integer = -1
                            ): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const;
                             AFlags: TGDBMICommandFlags;
                             ATimeOut: Integer = -1
                            ): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const;
                             out AResult: TGDBMIExecResult;
                             AFlags: TGDBMICommandFlags = [];
                             ATimeOut: Integer = -1
                            ): Boolean; overload;
    procedure DoTimeoutFeedback;
    function  ProcessResult(var AResult: TGDBMIExecResult; ATimeOut: Integer = -1): Boolean;
    function  ProcessGDBResultText(S: String): String;
    function  GetStackDepth(MaxDepth: integer): Integer;
    function  FindStackFrame(FP: TDBGPtr; StartAt, MaxDepth: Integer): Integer;
    function  GetFrame(const AIndex: Integer): String;
    function  GetText(const ALocation: TDBGPtr): String; overload;
    function  GetText(const AExpression: String; const AValues: array of const): String; overload;
    function  GetChar(const AExpression: String; const AValues: array of const): String; overload;
    function  GetFloat(const AExpression: String; const AValues: array of const): String;
    function  GetWideText(const ALocation: TDBGPtr): String;
    function  GetGDBTypeInfo(const AExpression: String; FullTypeInfo: Boolean = False;
                             AFlags: TGDBTypeCreationFlags = [];
                             AFormat: TWatchDisplayFormat = wdfDefault): TGDBType;
    function  GetClassName(const AClass: TDBGPtr): String; overload;
    function  GetClassName(const AExpression: String; const AValues: array of const): String; overload;
    function  GetInstanceClassName(const AInstance: TDBGPtr): String; overload;
    function  GetInstanceClassName(const AExpression: String; const AValues: array of const): String; overload;
    function  GetData(const ALocation: TDbgPtr): TDbgPtr; overload;
    function  GetData(const AExpression: String; const AValues: array of const): TDbgPtr; overload;
    function  GetStrValue(const AExpression: String; const AValues: array of const): String;
    function  GetIntValue(const AExpression: String; const AValues: array of const): Integer;
    function  GetPtrValue(const AExpression: String; const AValues: array of const; ConvertNegative: Boolean = False): TDbgPtr;
    function  CheckHasType(TypeName: String; TypeFlag: TGDBMITargetFlag): TGDBMIExecResult;
    function  PointerTypeCast: string;
    function  FrameToLocation(const AFrame: String = ''): TDBGLocationRec;
    procedure ProcessFrame(const ALocation: TDBGLocationRec); overload;
    procedure ProcessFrame(const AFrame: String = ''); overload;
    procedure DoDbgEvent(const ACategory: TDBGEventCategory; const AEventType: TDBGEventType; const AText: String);
    property  LastExecResult: TGDBMIExecResult read FLastExecResult;
    property  DefaultTimeOut: Integer read FDefaultTimeOut write FDefaultTimeOut;
    property  ProcessResultTimedOut: Boolean read FProcessResultTimedOut;       // single gdb command, took to long.Used to trigger timeout detection
    property  LastExecwasTimeOut: Boolean read FLastExecwasTimeOut;             // timeout, was confirmed (additional commands send and returned)
  public
    constructor Create(AOwner: TGDBMIDebugger);
    destructor Destroy; override;
    // DoQueued:   Called if queued *behind* others
    procedure DoQueued;
    // DoFinished: Called after processing is done
    //             defaults to Destroy the object
    procedure DoFinished;
    function  Execute: Boolean;
    procedure Cancel;
    function  DebugText: String; virtual;
    property  State: TGDBMIDebuggerCommandState read FState;
    property  OnExecuted: TNotifyEvent read FOnExecuted write FOnExecuted;
    property  OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property  OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property  Priority: Integer read FPriority write FPriority;
    property  Properties: TGDBMIDebuggerCommandProperts read FProperties write FProperties;
  end;

  { TGDBMIDebuggerCommandList }

  TGDBMIDebuggerCommandList = class(TRefCntObjList)
  private
    function Get(Index: Integer): TGDBMIDebuggerCommand;
    procedure Put(Index: Integer; const AValue: TGDBMIDebuggerCommand);
  public
    property Items[Index: Integer]: TGDBMIDebuggerCommand read Get write Put; default;
  end;

  { TGDBMIInternalBreakPoint }

  TGDBMIInternalBreakPoint = class
  private
    // -break-insert name
    FBreakID: Integer;
    FBreakAddr: TDBGPtr;
    // -break-insert *addr
    FInfoID: Integer;
    FInfoAddr: TDBGPtr;
    FName: string;
    procedure ClearBreak(ACmd: TGDBMIDebuggerCommand);
    procedure ClearInfo(ACmd: TGDBMIDebuggerCommand);
    function  BreakSet(ACmd: TGDBMIDebuggerCommand; ALoc: String; out AId: integer; out AnAddr: TDBGPtr): Boolean;
    function  GetAddr(ACmd: TGDBMIDebuggerCommand): TDBGPtr;
  public
    constructor Create(AName: string);
    procedure SetBoth(ACmd: TGDBMIDebuggerCommand);
    procedure SetAddr(ACmd: TGDBMIDebuggerCommand);
    procedure SetAtCustomAddr(ACmd: TGDBMIDebuggerCommand; AnAddr: TDBGPtr);
    procedure Clear(ACmd: TGDBMIDebuggerCommand);
    function  MatchAddr(AnAddr: TDBGPtr): boolean;
    function  MatchId(AnId: Integer): boolean;
    function  Enabled: boolean;
  end;

  { TGDBMIDebugger }

  TGDBMIDebugger = class(TCmdLineDebugger)
  private
    FCommandQueue: TGDBMIDebuggerCommandList;
    FCurrentCommand: TGDBMIDebuggerCommand;
    FCommandQueueExecLock: Integer;
    FCommandProcessingLock: Integer;

    FMainAddrBreak: TGDBMIInternalBreakPoint;
    FBreakAtMain: TDBGBreakPoint;
    FBreakErrorBreak: TGDBMIInternalBreakPoint;
    FRunErrorBreak: TGDBMIInternalBreakPoint;
    FExceptionBreak: TGDBMIInternalBreakPoint;
    FPauseWaitState: TGDBMIPauseWaitState;
    FInExecuteCount: Integer;
    FRunQueueOnUnlock: Boolean;
    FDebuggerFlags: TGDBMIDebuggerFlags;
    FSourceNames: TStringList; // Objects[] -> TMap[Integer|Integer] -> TDbgPtr
    FReleaseLock: Integer;
    FInProcessStopped: Boolean; // paused, but maybe state run

    // Internal Current values
    FCurrentStackFrame, FCurrentThreadId: Integer; // User set values
    FInternalStackFrame, FInternalThreadId: Integer; // Internal (update for every temporary change)
    FCurrentLocation: TDBGLocationRec;

    // GDB info (move to ?)
    FGDBVersion: String;
    FGDBCPU: String;
    FGDBPtrSize: integer; // PointerSize of the GDB-cpu
    FGDBOS: String;

    // Target info (move to record ?)
    FTargetInfo: TGDBMITargetInfo;

    FThreadGroups: TStringList;
    FTypeRequestCache: TGDBPTypeRequestCache;
    FMaxLineForUnitCache: TStringList;

    procedure DoPseudoTerminalRead(Sender: TObject);
    // Implementation of external functions
    function  GDBEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
    function  GDBEvaluate(const AExpression: String; var AResult: String;
      out ATypeInfo: TGDBType; EvalFlags: TDBGEvaluateFlags): Boolean;
    function  GDBModify(const AExpression, ANewValue: String): Boolean;
    function  GDBRun: Boolean;
    function  GDBPause(const AInternal: Boolean): Boolean;
    function  GDBStop: Boolean;
    function  GDBStepOver: Boolean;
    function  GDBStepInto: Boolean;
    function  GDBStepOverInstr: Boolean;
    function  GDBStepIntoInstr: Boolean;
    function  GDBStepOut: Boolean;
    function  GDBRunTo(const ASource: String; const ALine: Integer): Boolean;
    function  GDBJumpTo(const ASource: String; const ALine: Integer): Boolean;
    function  GDBDisassemble(AAddr: TDbgPtr; ABackward: Boolean; out ANextAddr: TDbgPtr;
                             out ADump, AStatement, AFile: String; out ALine: Integer): Boolean;
              deprecated;
    function  GDBSourceAdress(const ASource: String; ALine, AColumn: Integer; out AAddr: TDbgPtr): Boolean;

    // prevent destruction while nested in any call
    procedure LockRelease;
    procedure UnlockRelease;

    function  ConvertPascalExpression(var AExpression: String): Boolean;
    // ---
    procedure ClearSourceInfo;
    function  FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;

    // All ExecuteCommand functions are wrappers for the real (full) implementation
    // ExecuteCommandFull is never called directly
    function  ExecuteCommand(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags; var AResult: TGDBMIExecResult): Boolean; overload;
    function  ExecuteCommandFull(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: PtrInt; var AResult: TGDBMIExecResult): Boolean; overload;
    procedure RunQueue;
    procedure QueueCommand(const ACommand: TGDBMIDebuggerCommand; ForceQueue: Boolean = False);
    procedure UnQueueCommand(const ACommand: TGDBMIDebuggerCommand);
    procedure CancelAllQueued;
    procedure CancelBeforeRun;
    procedure CancelAfterStop;
    function  StartDebugging(AContinueCommand: TGDBMIExecCommandType): Boolean;
    function  StartDebugging(AContinueCommand: TGDBMIExecCommandType; AValues: array of const): Boolean;
    function  StartDebugging(AContinueCommand: TGDBMIDebuggerCommand = nil): Boolean;

  protected
    FErrorHandlingFlags: set of (ehfDeferReadWriteError, ehfGotReadError, ehfGotWriteError);
    FNeedStateToIdle: Boolean;
    {$IFDEF MSWindows}
    FPauseRequestInThreadID: Cardinal;
    {$ENDIF}
    {$IFDEF DBG_ENABLE_TERMINAL}
    FPseudoTerminal: TPseudoTerminal;
    procedure ProcessWhileWaitForHandles; override;
    {$ENDIF}
    procedure QueueExecuteLock;
    procedure QueueExecuteUnlock;

    function ConvertToGDBPath(APath: string; ConvType: TConvertToGDBPathType = cgptNone): string;
    function  ChangeFileName: Boolean; override;
    function  CreateBreakPoints: TDBGBreakPoints; override;
    function  CreateLocals: TLocalsSupplier; override;
    function  CreateLineInfo: TDBGLineInfo; override;
    function  CreateRegisters: TDBGRegisters; override;
    function  CreateCallStack: TCallStackSupplier; override;
    function  CreateDisassembler: TDBGDisassembler; override;
    function  CreateWatches: TWatchesSupplier; override;
    function  CreateThreads: TThreadsSupplier; override;
    function  GetSupportedCommands: TDBGCommands; override;
    function  GetCommands: TDBGCommands; override;
    function  GetTargetWidth: Byte; override;
    procedure InterruptTarget; virtual;
    function  ParseInitialization: Boolean; virtual;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
    procedure ClearCommandQueue;
    function  GetIsIdle: Boolean; override;
    procedure ResetStateToIdle; override;
    procedure DoState(const OldState: TDBGState); override;
    procedure DoBeforeState(const OldState: TDBGState); override;
    procedure DoReadError; override;
    procedure DoWriteError; override;
    procedure DoThreadChanged;
    property  TargetPID: Integer read FTargetInfo.TargetPID;
    property  TargetPtrSize: Byte read FTargetInfo.TargetPtrSize;
    property  TargetFlags: TGDBMITargetFlags read FTargetInfo.TargetFlags write FTargetInfo.TargetFlags;
    property  PauseWaitState: TGDBMIPauseWaitState read FPauseWaitState;
    property  DebuggerFlags: TGDBMIDebuggerFlags read FDebuggerFlags;
    procedure DoRelease; override;   // Destroy self (or schedule)

    procedure DoNotifyAsync(Line: String);
    procedure DoDbgBreakpointEvent(ABreakpoint: TDBGBreakPoint; Location: TDBGLocationRec;
                                   AReason: TGDBMIBreakpointReason;
                                   AOldVal: String = ''; ANewVal: String = '');
    procedure AddThreadGroup(const S: String);
    procedure RemoveThreadGroup(const S: String);
    function ParseLibraryLoaded(const S: String): String;
    function ParseLibraryUnLoaded(const S: String): String;
    function ParseThread(const S, EventText: String): String;
  public
    class function CreateProperties: TDebuggerProperties; override; // Creates debuggerproperties
    class function Caption: String; override;
    class function ExePaths: String; override;

    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;

    procedure Init; override;         // Initializes external debugger
    procedure Done; override;         // Kills external debugger
    function GetLocation: TDBGLocationRec; override;

    //LockCommandProcessing is more than just QueueExecuteLock
    //LockCommandProcessing also takes care to run the queue, if unlocked and not already running
    procedure LockCommandProcessing; override;
    procedure UnLockCommandProcessing; override;

    // internal testing
    procedure TestCmd(const ACommand: String); override;
  end;

resourcestring
  gdbmiErrorOnRunCommand = 'The debugger encountered an error when trying to '
    + 'run/step the application:%0:s%0:s%1:s%0:s%0:s'
    + 'Press "Ok" to continue debugging (paused), '
    + 'and correct the problem, or choose an alternative run command.%0:s'
    + 'Press "Stop" to end the debug session.';
  gdbmiErrorOnRunCommandWithWarning = '%0:s%0:sIn addition to the error the following '
    + 'warning was encountered:%0:s%0:s%1:s';
  gdbmiBreakPointErrorOnRunCommand = 'The debugger encountered an error when trying to '
    + 'run/step the application:%0:s%0:s%1:s%0:s%0:s'
    + 'Press "Ok" to remove the breakpoints and continue debugging (paused), '
    + 'and correct the problem, or choose an alternative run command.%0:s'
    + 'Press "Stop" to end the debug session.';
  gdbmiTimeOutForCmd = 'Time-out for command: "%s"';
  gdbmiFatalErrorOccured = 'Unrecoverable error: "%s"';
  gdbmiErrorStateGenericInfo = 'Error in: %1:s %0:s';
  gdbmiErrorStateInfoCommandError =
      '%0:sThe GDB command:%0:s"%1:s"%0:sreturned the error:%0:s"%2:s"%0:s';
  gdbmiErrorStateInfoCommandNoResult =
      '%0:sThe GDB command:%0:s"%1:s"%0:sdid not return any result.%0:s';
  gdbmiErrorStateInfoFailedWrite = '%0:sCould not send a command to GDB.%0:s';
  gdbmiErrorStateInfoFailedRead = '%0:sCould not read output from GDB.%0:s';
  gdbmiErrorStateInfoGDBGone = '%0:sThe GDB process is no longer running.%0:s';
  gdbmiWarningUnknowBreakPoint = 'The debugger reached an unexpected %1:s%0:s%0:s'
    + 'Press "Ok" to continue debugging (paused).%0:s'
    + 'Press "Stop" to end the debug session.';
  gdbmiTheDebuggerExperiencedAnUnknownCondition = 'The debugger experienced an'
    +' unknown condition';
  gdbmiPressIgnoreToContinueDebuggingThisMayNOTBeSafePres = 'Press "Ignore" to'
    +' continue debugging. This may NOT be safe. Press "Abort" to stop the '
    +'debugger.%0:sException: %1:s with message "%2:s"%0:sContext: %4:s. State'
    +': %5:s %0:s%0:s%3:s';


implementation

const
  GDBMIBreakPointReasonNames: Array[TGDBMIBreakpointReason] of string =
    ('Breakpoint', 'Watchpoint', 'Watchpoint (scope)');

type
  THackDBGType = class(TGDBType) end;

type
  TGDBMIEvaluationState = (esInvalid, esRequested, esValid);

  {%region       *****  TGDBMINameValueList and Parsers  *****   }

  { TGDBMINameValueBasedList }

  TGDBMINameValueBasedList = class
  protected
    FNameValueList: TGDBMINameValueList;
    procedure PreParse; virtual; abstract;
  public
    constructor Create;
    constructor Create(const AResultValues: String);
    constructor Create(AResult: TGDBMIExecResult);
    destructor  Destroy; override;
    procedure Init(AResultValues: string);
    procedure Init(AResult: TGDBMIExecResult);
  end;

  { TGDBMIMemoryDumpResultList }

  TGDBMIMemoryDumpResultList = class(TGDBMINameValueBasedList)
  private
    FAddr: TDBGPtr;
    function GetItem(Index: Integer): TPCharWithLen;
    function GetItemNum(Index: Integer): Integer;
    function GetItemTxt(Index: Integer): string;
  protected
    procedure PreParse; override;
  public
    // Expected input format: 1 row with hex values
    function Count: Integer;
    property Item[Index: Integer]: TPCharWithLen read GetItem;
    property ItemTxt[Index: Integer]: string  read GetItemTxt;
    property ItemNum[Index: Integer]: Integer read GetItemNum;
    property Addr: TDBGPtr read FAddr;
    function AsText(AStartOffs, ACount: Integer; AAddrWidth: Integer): string;
  end;

  {%endregion    *^^^*  TGDBMINameValueList and Parsers  *^^^*   }

const
  // priorities for commands
  GDCMD_PRIOR_IMMEDIATE = 999; // run immediate (request without callback)
  GDCMD_PRIOR_LINE_INFO = 100; // Line info should run asap
  GDCMD_PRIOR_DISASS    = 30;  // Run before watches
  GDCMD_PRIOR_USER_ACT  = 10;  // set/change/remove brkpoint
  GDCMD_PRIOR_THREAD    = 5;   // Run before watches, stack or locals
  GDCMD_PRIOR_STACK     = 2;   // Run before watches
  GDCMD_PRIOR_LOCALS    = 1;   // Run before watches (also registers etc)

type
  {%region       *****  TGDBMIDebuggerCommands  *****   }

  { TGDBMIDebuggerSimpleCommand }

  // not to be used for anything that runs/steps the app
  TGDBMIDebuggerSimpleCommand = class(TGDBMIDebuggerCommand)
  private
    FCommand: String;
    FFlags: TGDBMICmdFlags;
    FCallback: TGDBMICallback;
    FTag: PtrInt;
    FResult: TGDBMIExecResult;
  protected
    procedure DoStateChanged(OldState: TGDBMIDebuggerCommandState); override;
    function  DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger;
                       const ACommand: String;
                       const AValues: array of const;
                       const AFlags: TGDBMICmdFlags;
                       const ACallback: TGDBMICallback;
                       const ATag: PtrInt);
    function  DebugText: String; override;
    property  Result: TGDBMIExecResult read FResult;
  end;

  { TGDBMIDebuggerCommandInitDebugger }

  TGDBMIDebuggerCommandInitDebugger = class(TGDBMIDebuggerCommand)
  private
    FSuccess: Boolean;
  protected
    function  DoExecute: Boolean; override;
  public
    property Success: Boolean read FSuccess;
  end;

  { TGDBMIDebuggerCommandChangeFilename }

  TGDBMIDebuggerCommandChangeFilename = class(TGDBMIDebuggerCommand)
  private
    FErrorMsg: String;
    FSuccess: Boolean;
    FFileName: String;
  protected
    function  DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; AFileName: String);
    property Success: Boolean read FSuccess;
    property ErrorMsg: String read FErrorMsg;
  end;

  { TGDBMIDebuggerCommandStartDebugging }

  TGDBMIDebuggerCommandStartDebugging = class(TGDBMIDebuggerCommand)
  private
    FContinueCommand: TGDBMIDebuggerCommand;
    FSuccess: Boolean;
  protected
    function  DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; AContinueCommand: TGDBMIDebuggerCommand);
    destructor Destroy; override;
    function  DebugText: String; override;
    property ContinueCommand: TGDBMIDebuggerCommand read FContinueCommand;
    property Success: Boolean read FSuccess;
  end;

  { TGDBMIDebuggerCommandExecute }

  TGDBMIDebuggerCommandExecute = class(TGDBMIDebuggerCommand)
  private
    FNextExecQueued: Boolean;
    FResult: TGDBMIExecResult;
    FExecType: TGDBMIExecCommandType;
    FCommand: String;
    FCanKillNow, FDidKillNow: Boolean;
    FRunToSrc: String;
    FRunToLine: Integer;
    FStepBreakPoint: Integer;
  protected
    procedure DoLockQueueExecute; override;
    procedure DoUnockQueueExecute; override;
    function  ProcessRunning(var AStoppedParams: String; out AResult: TGDBMIExecResult): Boolean;
    function  ProcessStopped(const AParams: String; const AIgnoreSigIntState: Boolean): Boolean;
    {$IFDEF MSWindows}
    function FixThreadForSigTrap: Boolean;
    {$ENDIF}
    function  DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; const ExecType: TGDBMIExecCommandType);
    constructor Create(AOwner: TGDBMIDebugger; const ExecType: TGDBMIExecCommandType; Args: array of const);
    function  DebugText: String; override;
    property  Result: TGDBMIExecResult read FResult;
    property  NextExecQueued: Boolean read FNextExecQueued;
    function  KillNow: Boolean;
  end;

  { TGDBMIDebuggerCommandKill }

  TGDBMIDebuggerCommandKill = class(TGDBMIDebuggerCommand)
  protected
    function  DoExecute: Boolean; override;
  end;

  {%endregion    *^^^*  TGDBMIDebuggerCommands  *^^^*   }

  {%region      *****  Locals  *****   }

  { TGDBMIDebuggerCommandLocals }

  TGDBMIDebuggerCommandLocals = class(TGDBMIDebuggerCommand)
  private
    FLocals: TCurrentLocals;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; ALocals: TCurrentLocals);
    destructor Destroy; override;
    function DebugText: String; override;
  end;

  { TGDBMILocals }

  TGDBMILocals = class(TLocalsSupplier)
  private
    FCommandList: TList;
    procedure CancelEvaluation; deprecated;
    procedure DoEvaluationDestroyed(Sender: TObject);
  protected
    procedure CancelAllCommands;
    procedure RequestData(ALocals: TCurrentLocals); override;
  public
    procedure Changed;
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
  end;

  {%endregion   ^^^^^  Locals  ^^^^^   }

  {%region      *****  LineSymbolInfo  *****   }

  { TGDBMIDebuggerCommandLineSymbolInfo }

  TGDBMIDebuggerCommandLineSymbolInfo = class(TGDBMIDebuggerCommand)
  private
    FResult: TGDBMIExecResult;
    FSource: string;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; Source: string);
    function DebugText: String; override;
    property Result: TGDBMIExecResult read FResult;
    property Source: string read FSource;
  end;

  { TGDBMILineInfo }

  TGDBMILineInfo = class(TDBGLineInfo)
  private
    FSourceIndex: TStringList;
    FRequestedSources: TStringList;
    FSourceMaps: array of record
      Source: String;
      Map: TMap;
    end;
    FGetLineSymbolsCmdObj: TGDBMIDebuggerCommandLineSymbolInfo;
    procedure DoGetLineSymbolsDestroyed(Sender: TObject);
    procedure ClearSources;
    procedure AddInfo(const ASource: String; const AResult: TGDBMIExecResult);
    procedure DoGetLineSymbolsFinished(Sender: TObject);
  protected
    function GetSource(const AIndex: integer): String; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
    function Count: Integer; override;
    function GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr; override;
    function GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
  end;

  {%endregion   ^^^^^  LineSymbolInfo  ^^^^^   }

  {%region      *****  BreakPoints  *****  }

  { TGDBMIDebuggerCommandBreakPointBase }

  TGDBMIDebuggerCommandBreakPointBase = class(TGDBMIDebuggerCommand)
  protected
    function ExecCheckLineInUnit(ASource: string; ALine: Integer): Boolean;
    function ExecBreakDelete(ABreakId: Integer): Boolean;
    function ExecBreakEnabled(ABreakId: Integer; AnEnabled: Boolean): Boolean;
    function ExecBreakCondition(ABreakId: Integer; AnExpression: string): Boolean;
  end;

  { TGDBMIDebuggerCommandBreakInsert }

  TGDBMIDebuggerCommandBreakInsert = class(TGDBMIDebuggerCommandBreakPointBase)
  private
    FKind: TDBGBreakPointKind;
    FAddress: TDBGPtr;
    FSource: string;
    FLine: Integer;
    FEnabled: Boolean;
    FExpression: string;
    FReplaceId: Integer;

    FAddr: TDBGPtr;
    FBreakID: Integer;
    FHitCnt: Integer;
    FValid: Boolean;
    FWatchData: String;
    FWatchKind: TDBGWatchPointKind;
    FWatchScope: TDBGWatchPointScope;
  protected
    function ExecBreakInsert(out ABreakId, AHitCnt: Integer; out AnAddr: TDBGPtr): Boolean;
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; ASource: string; ALine: Integer;
                       AEnabled: Boolean; AnExpression: string; AReplaceId: Integer); overload;
    constructor Create(AOwner: TGDBMIDebugger; AAddress: TDBGPtr;
                       AEnabled: Boolean; AnExpression: string; AReplaceId: Integer); overload;
    constructor Create(AOwner: TGDBMIDebugger; AData: string; AScope: TDBGWatchPointScope;
                       AKind: TDBGWatchPointKind; AEnabled: Boolean; AnExpression: string; AReplaceId: Integer); overload;
    function DebugText: String; override;
    property Kind: TDBGBreakPointKind read FKind write FKind;
    property Address: TDBGPtr read FAddress write FAddress;
    property Source: string read FSource write FSource;
    property Line: Integer read FLine write FLine;
    property WatchData: String read FWatchData write FWatchData;
    property WatchScope: TDBGWatchPointScope read FWatchScope write FWatchScope;
    property WatchKind: TDBGWatchPointKind read FWatchKind write FWatchKind;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Expression: string read FExpression write FExpression;
    property ReplaceId: Integer read FReplaceId write FReplaceId;
    // result values
    property Addr: TDBGPtr read FAddr;
    property BreakID: Integer read FBreakID;
    property HitCnt: Integer read FHitCnt;
    property Valid: Boolean read FValid;
  end;

  { TGDBMIDebuggerCommandBreakRemove }

  TGDBMIDebuggerCommandBreakRemove = class(TGDBMIDebuggerCommandBreakPointBase)
  private
    FBreakId: Integer;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; ABreakId: Integer);
    function DebugText: String; override;
  end;

  { TGDBMIDebuggerCommandBreakUpdate }

  TGDBMIDebuggerCommandBreakUpdate = class(TGDBMIDebuggerCommandBreakPointBase)
  private
    FBreakID: Integer;
    FEnabled: Boolean;
    FExpression: string;
    FUpdateEnabled: Boolean;
    FUpdateExpression: Boolean;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; ABreakId: Integer);
    constructor Create(AOwner: TGDBMIDebugger; ABreakId: Integer; AnEnabled: Boolean);
    constructor Create(AOwner: TGDBMIDebugger; ABreakId: Integer; AnExpression: string);
    constructor Create(AOwner: TGDBMIDebugger; ABreakId: Integer; AnEnabled: Boolean; AnExpression: string);
    function DebugText: String; override;
    property UpdateEnabled: Boolean read FUpdateEnabled write FUpdateEnabled;
    property UpdateExpression: Boolean read FUpdateExpression write FUpdateExpression;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Expression: string read FExpression write FExpression;
  end;

  { TGDBMIBreakPoint       *****  BreakPoints  *****   }

  TGDBMIBreakPointUpdateFlag = (bufSetBreakPoint, bufEnabled, bufCondition);
  TGDBMIBreakPointUpdateFlags = set of TGDBMIBreakPointUpdateFlag;

  TGDBMIBreakPoint = class(TDBGBreakPoint)
  private
    FParsedExpression: String;
    FCurrentCmd: TGDBMIDebuggerCommandBreakPointBase;
    FUpdateFlags: TGDBMIBreakPointUpdateFlags;
    procedure SetBreakPoint;
    procedure ReleaseBreakPoint;
    procedure UpdateProperties(AFlags: TGDBMIBreakPointUpdateFlags);
    procedure DoCommandDestroyed(Sender: TObject);
    procedure DoCommandExecuted(Sender: TObject);
  protected
    FBreakID: Integer;
    procedure DoEndUpdate; override;
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure DoLogExpression(const AnExpression: String); override;
    procedure MakeInvalid;
    procedure SetAddress(const AValue: TDBGPtr); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure SetLocation(const ASource: String; const ALine: Integer); override;
    procedure SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
                       const AKind: TDBGWatchPointKind); override;
  end;

  { TGDBMIBreakPoints }

  TGDBMIBreakPoints = class(TDBGBreakPoints)
  protected
    function FindById(AnId: Integer): TGDBMIBreakPoint;
  end;
  {%endregion   ^^^^^  BreakPoints  ^^^^^   }

  {%region      *****  Register  *****   }

  { TGDBMIDebuggerCommandRegisterNames }
  TStringArray = Array of string;
  TBoolArray = Array of Boolean;

  TGDBMIDebuggerCommandRegisterNames = class(TGDBMIDebuggerCommand)
  private
    FNames: Array of String;
    function GetNames(Index: Integer): string;
  protected
    function DoExecute: Boolean; override;
  public
    //function DebugText: String; override;
    function Count: Integer;
    property Names[Index: Integer]: string read GetNames;
  end;

  { TGDBMIDebuggerCommandRegisterValues }

  TGDBMIDebuggerCommandRegisterValues = class(TGDBMIDebuggerCommand)
  private
    FRegistersToUpdate: TStringArray;
    FFormat: TRegisterDisplayFormat;
  protected
    function DoExecute: Boolean; override;
  public
    // updates the given array directly
    constructor Create(AOwner: TGDBMIDebugger;
                       RegistersToUpdate: TStringArray;
                       AFormat: TRegisterDisplayFormat = rdDefault
                      );
    function DebugText: String; override;
    property Format: TRegisterDisplayFormat read FFormat;
  end;

  { TGDBMIDebuggerCommandRegisterModified }

  TGDBMIDebuggerCommandRegisterModified = class(TGDBMIDebuggerCommand)
  private
    FModifiedToUpdate: TBoolArray;
  protected
    function DoExecute: Boolean; override;
  public
    // updates the given array directly
    constructor Create(AOwner: TGDBMIDebugger; ModifiedToUpdate: TBoolArray);
    function DebugText: String; override;
  end;

  { TGDBMIRegisters }

  TGDBMIRegisters = class(TDBGRegisters)
  private
    FRegNames: TStringArray;
    FRegValues: Array [TRegisterDisplayFormat] of TStringArray;
    FRegModified: TBoolArray;
    FFormats: Array of TRegisterDisplayFormat;

    FGetRegisterCmdObj: TGDBMIDebuggerCommandRegisterNames;
    FRegistersReqState: TGDBMIEvaluationState;
    FInRegistersNeeded: Boolean;

    FGetModifiedCmd: TGDBMIDebuggerCommandRegisterModified;
    FModifiedReqState: TGDBMIEvaluationState;
    FInModifiedNeeded: Boolean;

    FGetValuesCmdObj: Array [TRegisterDisplayFormat] of TGDBMIDebuggerCommandRegisterValues;
    FValuesReqState: Array [TRegisterDisplayFormat] of TGDBMIEvaluationState;
    FInValuesNeeded: Array [TRegisterDisplayFormat] of Boolean;

    function GetDebugger: TGDBMIDebugger;
    procedure RegistersNeeded;
    procedure ValuesNeeded(AFormat: TRegisterDisplayFormat);
    procedure ModifiedNeeded;
    procedure DoGetRegisterNamesDestroyed(Sender: TObject);
    procedure DoGetRegisterNamesFinished(Sender: TObject);
    procedure DoGetRegValuesDestroyed(Sender: TObject);
    procedure DoGetRegValuesFinished(Sender: TObject);
    procedure DoGetRegModifiedDestroyed(Sender: TObject);
    procedure DoGetRegModifiedFinished(Sender: TObject);
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure Invalidate;
    function GetCount: Integer; override;
    function GetModified(const AnIndex: Integer): Boolean; override;
    function GetName(const AnIndex: Integer): String; override;
    function GetValue(const AnIndex: Integer): String; override;
    property Debugger: TGDBMIDebugger read GetDebugger;
  public
    procedure Changed; override;
  end;

  {%endregion   ^^^^^  Register  ^^^^^   }

  {%region      *****  Watches  *****   }

  TGDBMIDebuggerParentFrameCache = record
      ThreadId: Integer;
      ParentFPList: Array of
        record
          fp, parentfp: string; // empty=unknown / '-'=evaluated-no-data
        end;
    end;
    PGDBMIDebuggerParentFrameCache = ^TGDBMIDebuggerParentFrameCache;

  { TGDBMIDebuggerCommandEvaluate }

  TGDBMIDebuggerCommandEvaluate = class(TGDBMIDebuggerCommand)
  private
    FEvalFlags: TDBGEvaluateFlags;
    FExpression: String;
    FDisplayFormat: TWatchDisplayFormat;
    FWatchValue: TCurrentWatchValue;
    FTextValue: String;
    FTypeInfo: TGDBType;
    FValidity: TDebuggerDataState;
    FTypeInfoAutoDestroy: Boolean;
    FThreadChanged, FStackFrameChanged: Boolean;
    function GetTypeInfo: TGDBType;
    procedure DoWatchFreed(Sender: TObject);
  protected
    function DoExecute: Boolean; override;
    function SelectContext: Boolean;
    procedure UnSelectContext;
  public
    constructor Create(AOwner: TGDBMIDebugger; AExpression: String; ADisplayFormat: TWatchDisplayFormat);
    constructor Create(AOwner: TGDBMIDebugger; AWatchValue: TCurrentWatchValue);
    destructor Destroy; override;
    function DebugText: String; override;
    property Expression: String read FExpression;
    property EvalFlags: TDBGEvaluateFlags read FEvalFlags write FEvalFlags;
    property DisplayFormat: TWatchDisplayFormat read FDisplayFormat;
    property TextValue: String read FTextValue;
    property TypeInfo: TGDBType read GetTypeInfo;
    property TypeInfoAutoDestroy: Boolean read FTypeInfoAutoDestroy write FTypeInfoAutoDestroy;
  end;

  { TGDBMIWatches }

  TGDBMIWatches = class(TWatchesSupplier)
  private
    FCommandList: TList;
    FParentFPList: Array of TGDBMIDebuggerParentFrameCache;
    procedure DoEvaluationDestroyed(Sender: TObject);
  protected
    function  GetParentFPList(AThreadId: Integer): PGDBMIDebuggerParentFrameCache;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure Changed;
    procedure Clear;
    procedure InternalRequestData(AWatchValue: TCurrentWatchValue); override;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
  end;

  {%endregion   ^^^^^  Watches  ^^^^^   }

  {%region      *****  Stack  *****   }

  TGDBMINameValueListArray = array of TGDBMINameValueList;

  { TGDBMIDebuggerCommandStack }

  TGDBMIDebuggerCommandStack = class(TGDBMIDebuggerCommand)
  private
    procedure DoCallstackFreed(Sender: TObject);
  protected
    FCallstack: TCurrentCallStack;
    FThreadChanged: Boolean;
    function  SelectThread: Boolean;
    procedure UnSelectThread;
  public
    constructor Create(AOwner: TGDBMIDebugger; ACallstack: TCurrentCallStack);
    destructor Destroy; override;
    property Callstack: TCurrentCallStack read FCallstack;
  end;

  { TGDBMIDebuggerCommandStackFrames }

  TGDBMIDebuggerCommandStackFrames = class(TGDBMIDebuggerCommandStack)
  protected
    function DoExecute: Boolean; override;
  end;

  { TGDBMIDebuggerCommandStackDepth }

  TGDBMIDebuggerCommandStackDepth = class(TGDBMIDebuggerCommandStack)
  private
    FDepth: Integer;
  protected
    function DoExecute: Boolean; override;
  public
    function DebugText: String; override;
    property Depth: Integer read FDepth;
  end;

  { TGDBMIDebuggerCommandStackSetCurrent }

  TGDBMIDebuggerCommandStackSetCurrent = class(TGDBMIDebuggerCommandStack)
  private
    FNewCurrent: Integer;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; ACallstack: TCurrentCallStack; ANewCurrent: Integer);
    function DebugText: String; override;
    property NewCurrent: Integer read FNewCurrent;
  end;

  { TGDBMICallStack }

  TGDBMICallStack = class(TCallStackSupplier)
  private
    FCommandList: TList;
    procedure DoDepthCommandExecuted(Sender: TObject);
    //procedure DoFramesCommandExecuted(Sender: TObject);
    procedure DoSetIndexCommandExecuted(Sender: TObject);
    procedure DoCommandDestroyed(Sender: TObject);
  protected
    procedure Clear;
    procedure RequestCount(ACallstack: TCurrentCallStack); override;
    procedure RequestCurrent(ACallstack: TCurrentCallStack); override;
    procedure RequestEntries(ACallstack: TCurrentCallStack); override;
    procedure UpdateCurrentIndex; override;
    procedure DoThreadChanged;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
  end;

  {%endregion   ^^^^^  Stack  ^^^^^   }

  {%region      *****  Disassembler  *****   }

const
  (*  Some values to calculate how many bytes to disassemble for a given amount of lines
      Those values are only guesses *)
  // DAssBytesPerCommandAvg: Average len: Used for LinesBefore/LinesAfter.
  // (should rather be to big than to small)
  DAssBytesPerCommandAvg = 8;
  // Max possible len of a statement in byte. Only used for up to 5 lines
  DAssBytesPerCommandMax = 24;
  // Maximum alignment between to procedures (for detecion of gaps, after dis-ass with source)
  DAssBytesPerCommandAlign = 16;
  // If we have a range with more then DAssRangeOverFuncTreshold * DAssBytesPerCommandAvg
  //  then prefer the Range-end as start, rather than the known func start
  //  (otherwhise re-dissassemble the whole function, including the part already known)
  // The assumption is, that no single *source* statement starting before this range,
  //  will ever reach into the next statement (where the next statement already started / mixed addresses)
  DAssRangeOverFuncTreshold = 15;
  // Never dis-assemble more bytes in a single go (actually, max-offset before requested addr)
  DAssMaxRangeSize = 4096;
type

  { TGDBMIDisassembleResultList }

  TGDBMIDisassembleResultList = class(TGDBMINameValueBasedList)
  private
    FCount: Integer;
    FHasSourceInfo: Boolean;
    FItems: array of record
        AsmEntry: TPCharWithLen;
        SrcFile: TPCharWithLen;
        SrcLine: TPCharWithLen;
        ParsedInfo: TDisassemblerEntry;
      end;
    HasItemPointerList: Boolean;
    ItemPointerList: Array of PDisassemblerEntry;
    function GetItem(Index: Integer): PDisassemblerEntry;
    function GetLastItem: PDisassemblerEntry;
    procedure ParseItem(Index: Integer);
    procedure SetCount(const AValue: Integer);
    procedure SetItem(Index: Integer; const AValue: PDisassemblerEntry);
    procedure SetLastItem(const AValue: PDisassemblerEntry);
  protected
    procedure PreParse; override;
  public
    property Count: Integer read FCount write SetCount;
    property HasSourceInfo: Boolean read FHasSourceInfo;
    property Item[Index: Integer]: PDisassemblerEntry read GetItem write SetItem;
    property LastItem: PDisassemblerEntry read GetLastItem write SetLastItem;
    function SortByAddress: Boolean;
  public
    // only valid as long a src object exists, and not modified
    constructor CreateSubList(ASource: TGDBMIDisassembleResultList; AStartIdx, ACount: Integer);
    procedure   InitSubList(ASource: TGDBMIDisassembleResultList; AStartIdx, ACount: Integer);
  end;

  { TGDBMIDisassembleResultFunctionIterator }

  TGDBMIDisassembleResultFunctionIterator = class
  private
    FCurIdx: Integer;
    FIndexOfLocateAddress: Integer;
    FOffsetOfLocateAddress: Integer;
    FIndexOfCounterAddress: Integer;
    FList: TGDBMIDisassembleResultList;
    FStartedAtIndex: Integer;
    FStartIdx, FMaxIdx: Integer;
    FLastSubListEndAddr: TDBGPtr;
    FAddressToLocate, FAddForLineAfterCounter: TDBGPtr;
    FSublistNumber: Integer;
  public
    constructor Create(AList: TGDBMIDisassembleResultList; AStartIdx: Integer;
                       ALastSubListEndAddr: TDBGPtr;
                       AnAddressToLocate, AnAddForLineAfterCounter: TDBGPtr);
    function EOL: Boolean;
    function NextSubList(var AResultList: TGDBMIDisassembleResultList): Boolean;

    // Current SubList
    function IsFirstSubList: Boolean;
    function CurrentFixedAddr(AOffsLimit: Integer): TDBGPtr; // Addr[0] - Offs[0]
    // About the next SubList
    function NextStartAddr: TDBGPtr;
    function NextStartOffs: Integer;
    // Overall
    function CountLinesAfterCounterAddr: Integer; // count up to Start of Current SubList

    property CurrentIndex: Integer read FCurIdx;
    property NextIndex: Integer read FStartIdx;
    property SublistNumber: Integer read FSublistNumber; // running count of sublists found

    property StartedAtIndex: Integer read FStartedAtIndex;
    property IndexOfLocateAddress: Integer read FIndexOfLocateAddress;
    property OffsetOfLocateAddress: Integer read FOffsetOfLocateAddress;
    property IndexOfCounterAddress: Integer read FIndexOfCounterAddress;
    property List: TGDBMIDisassembleResultList read FList;
  end;

  { TGDBMIDebuggerCommandDisassembe }

  TGDBMIDisAssAddrRange = record
     FirstAddr, LastAddr: TDBGPtr;
  end;

  TGDBMIDebuggerCommandDisassembe = class(TGDBMIDebuggerCommand)
  private
    FEndAddr: TDbgPtr;
    FLinesAfter: Integer;
    FLinesBefore: Integer;
    FOnProgress: TNotifyEvent;
    FStartAddr: TDbgPtr;
    FKnownRanges: TDBGDisassemblerEntryMap;
    FRangeIterator: TDBGDisassemblerEntryMapIterator;
    FMemDumpsNeeded: array of TGDBMIDisAssAddrRange;
    procedure DoProgress;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; AKnownRanges: TDBGDisassemblerEntryMap;
                       AStartAddr, AEndAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer);
    destructor Destroy; override;
    function DebugText: String; override;
    property StartAddr: TDbgPtr read FStartAddr write FStartAddr;
    property EndAddr:   TDbgPtr read FEndAddr   write FEndAddr;
    property LinesBefore: Integer read FLinesBefore write FLinesBefore;
    property LinesAfter:  Integer read FLinesAfter  write FLinesAfter;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  TGDBMIDisassembler = class(TDBGDisassembler)
  private
    FDisassembleEvalCmdObj: TGDBMIDebuggerCommandDisassembe;
    FLastExecAddr: TDBGPtr;
    procedure DoDisassembleExecuted(Sender: TObject);
    procedure DoDisassembleProgress(Sender: TObject);
    procedure DoDisassembleDestroyed(Sender: TObject);
  protected
    function PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): Boolean; override;
    function  HandleRangeWithInvalidAddr(ARange: TDBGDisassemblerEntryRange;AnAddr:
                 TDbgPtr; var ALinesBefore, ALinesAfter: Integer): boolean; override;
  public
    procedure Clear; override;
    function PrepareRange(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): Boolean; override;
  end;

  {%endregion   ^^^^^  Disassembler  ^^^^^   }

  {%region      *****  Threads  *****   }

  { TGDBMIDebuggerCommandThreads }

  TGDBMIDebuggerCommandThreads = class(TGDBMIDebuggerCommand)
  private
    FCurrentThreadId: Integer;
    FSuccess: Boolean;
    FThreads: Array of TThreadEntry;
    function GetThread(AnIndex: Integer): TThreadEntry;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger);
    destructor Destroy; override;
    //function DebugText: String; override;
    function Count: Integer;
    property Threads[AnIndex: Integer]: TThreadEntry read GetThread;
    property CurrentThreadId: Integer read FCurrentThreadId;
    property Success: Boolean read FSuccess;
  end;

  { TGDBMIDebuggerCommandChangeThread }

  TGDBMIDebuggerCommandChangeThread = class(TGDBMIDebuggerCommand)
  private
    FNewId: Integer;
    FSuccess: Boolean;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; ANewId: Integer);
    function DebugText: String; override;
    property Success: Boolean read FSuccess;
    property NewId: Integer read FNewId write FNewId;
  end;


  { TGDBMIThreads }

  TGDBMIThreads = class(TThreadsSupplier)
  private
    FGetThreadsCmdObj: TGDBMIDebuggerCommandThreads;
    FChangeThreadsCmdObj: TGDBMIDebuggerCommandChangeThread;

    function GetDebugger: TGDBMIDebugger;
    procedure ThreadsNeeded;
    procedure CancelEvaluation;
    procedure DoThreadsDestroyed(Sender: TObject);
    procedure DoThreadsFinished(Sender: TObject);
    procedure DoChangeThreadsDestroyed(Sender: TObject);
    procedure DoChangeThreadsFinished(Sender: TObject);
  protected
    procedure RequestMasterData; override;
    procedure ChangeCurrentThread(ANewId: Integer); override;
    property Debugger: TGDBMIDebugger read GetDebugger;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
  end;

  {%endregion   ^^^^^  Threads  ^^^^^   }

  {%region       *****  TGDBMIExpression  *****   }

  { TGDBMIExpression }
  // TGDBMIExpression was an attempt to make expression evaluation on Objects possible for GDB <= 5.2
  // It is not completed and buggy. Since 5.3 expression evaluation is OK, so maybe in future the
  // TGDBMIExpression will be completed to support older gdb versions

  TDBGExpressionOperator = (
    eoNone,
    eoNegate,
    eoPlus,
    eoSubstract,
    eoAdd,
    eoMultiply,
    eoPower,
    eoDivide,
    eoDereference,
    eoAddress,
    eoEqual,
    eoLess,
    eoLessOrEqual,
    eoGreater,
    eoGreaterOrEqual,
    eoNotEqual,
    eoIn,
    eoIs,
    eoAs,
    eoDot,
    eoComma,
    eoBracket,
    eoIndex,
    eoClose,
    eoAnd,
    eoOr,
    eoMod,
    eoNot,
    eoDiv,
    eoXor,
    eoShl,
    eoShr
  );

const
  OPER_LEVEL: array[TDBGExpressionOperator] of Byte = (
    {eoNone            } 0,
    {eoNegate          } 5,
    {eoPlus            } 5,
    {eoSubstract       } 7,
    {eoAdd             } 7,
    {eoMultiply        } 6,
    {eoPower           } 4,
    {eoDivide          } 6,
    {eoDereference     } 2,
    {eoAddress         } 4,
    {eoEqual           } 8,
    {eoLess            } 8,
    {eoLessOrEqual     } 8,
    {eoGreater         } 8,
    {eoGreaterOrEqual  } 8,
    {eoNotEqual        } 8,
    {eoIn              } 8,
    {eoIs              } 8,
    {eoAs              } 6,
    {eoDot             } 2,
    {eoComma           } 9,
    {eoBracket         } 1,
    {eoIndex           } 3,
    {eoClose           } 9,
    {eoAnd             } 6,
    {eoOr              } 7,
    {eoMod             } 6,
    {eoNot             } 5,
    {eoDiv             } 6,
    {eoXor             } 7,
    {eoShl             } 6,
    {eoShr             } 6
  );

type
  PGDBMISubExpression = ^TGDBMISubExpression;
  TGDBMISubExpression = record
    Opertor: TDBGExpressionOperator;
    Operand: String;
    Next, Prev: PGDBMISubExpression;
  end;

  PGDBMIExpressionResult = ^TGDBMIExpressionResult;
  TGDBMIExpressionResult = record
    Opertor: TDBGExpressionOperator;
//    Operand: String;
    Value: String;
    Info: TGDBType;
    Next, Prev: PGDBMIExpressionResult;
  end;


  TGDBMIExpression = class(TObject)
  private
    FList: PGDBMISubExpression;
    FStack: PGDBMIExpressionResult;
    FStackPtr: PGDBMIExpressionResult;
    procedure Push(var AResult: PGDBMIExpressionResult);
    procedure Pop(var AResult: PGDBMIExpressionResult);
    procedure DisposeList(AList: PGDBMIExpressionResult);

    function  Solve(const ADebuggerCommand: TGDBMIDebuggerCommand; ALimit: Byte; const ARight: String; out AValue: String; out AInfo: TGDBType): Boolean;
    function  SolveAddress(const ADebuggerCommand: TGDBMIDebuggerCommand; ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
    function  SolveMath(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
    function  SolveIn(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
    function  SolveIs(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
    function  SolveAs(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
    function  SolveDeref(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
    function  SolveDot(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft: PGDBMIExpressionResult; const ARight: String; out AVAlue: String; out AInfo: TGDBType): Boolean;
  protected
    function Evaluate(const ADebuggerCommand: TGDBMIDebuggerCommand; const AText: String; out AResult: String; out AResultInfo: TGDBType): Boolean;
  public
    constructor Create(const AExpression: String);
    destructor Destroy; override;
    function DumpExpression: String;
    function Evaluate(const ADebuggerCommand: TGDBMIDebuggerCommand; out AResult: String; out AResultInfo: TGDBType): Boolean;
  end;

  {%endregion    *^^^*  TGDBMIExpression  *^^^*   }

  { TGDBStringIterator }

  TGDBStringIterator=class
  protected
    FDataSize: Integer;
    FReadPointer: Integer;
    FParsableData: String;
  public
    constructor Create(const AParsableData: String);
    function ParseNext(out ADecomposable: Boolean; out APayload: String; out ACharStopper: Char): Boolean;
  end;

  TGDBMIExceptionInfo = record
    ObjAddr: String;
    Name: String;
  end;

{ =========================================================================== }
{ Some win32 stuff }
{ =========================================================================== }
{$IFdef MSWindows}
var
  DebugBreakAddr: Pointer = nil;
  // use our own version. Win9x doesn't support this, so it is a nice check
  _CreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall = nil;

procedure InitWin32;
var
  hMod: THandle;
begin
  // Check if we already are initialized
  if DebugBreakAddr <> nil then Exit;

  // normally you would load a lib, but since kernel32 is
  // always loaded we can use this (and we don't have to free it
  hMod := GetModuleHandle(kernel32);
  if hMod = 0 then Exit; //????

  DebugBreakAddr := GetProcAddress(hMod, 'DebugBreak');
  Pointer(_CreateRemoteThread) := GetProcAddress(hMod, 'CreateRemoteThread');
end;
{$ENDIF}

{ =========================================================================== }
{ Helpers }
{ =========================================================================== }

function CpuNameToPtrSize(const CpuName: String): Integer;
begin
  //'x86', 'i386', 'i486', 'i586', 'i686',
  //'ia64', 'x86_64', 'powerpc',
  //'sparc', 'arm'
  Result := 4;
  if (LowerCase(CpuName) = 'ia64') or (LowerCase(CpuName) = 'x86_64')
  then Result := 8;
end;

function TGDBMIDebugger.ConvertToGDBPath(APath: string; ConvType: TConvertToGDBPathType = cgptNone): string;
// GDB wants forward slashes in its filenames, even on win32.
var
  esc: TGDBMIDebuggerFilenameEncoding;
begin
  Result := APath;
  // no need to process empty filename
  if Result = '' then exit;

  case ConvType of
    cgptNone: esc := gdfeNone;
    cgptCurDir:
      begin
        esc := TGDBMIDebuggerProperties(GetProperties).FEncodeCurrentDirPath;
        //TODO: check FGDBOS
        //Unix/Windows can use gdfeEscSpace, but work without too;
        if esc = gdfeDefault
        then esc :=
        {$IFDEF darwin} gdfeQuote;
        {$ELSE}         gdfeNone;
        {$ENDIF}
      end;
    cgptExeName:
      begin
        esc := TGDBMIDebuggerProperties(GetProperties).FEncodeExeFileName;
        if esc = gdfeDefault
        then esc :=
        //Unix/Windows can use gdfeEscSpace, but work without too;
        {$IFDEF darwin} gdfeEscSpace;
        {$ELSE}         gdfeNone;
        {$ENDIF}
      end;
  end;

  {$WARNINGS off}
  if DirectorySeparator <> '/' then
    Result := StringReplace(Result, DirectorySeparator, '/', [rfReplaceAll]);
  {$WARNINGS on}
  if esc = gdfeEscSpace
  then Result := StringReplace(Result, ' ', '\ ', [rfReplaceAll]);
  if esc = gdfeQuote
  then Result := '\"' + Result + '\"';
  Result := '"' + Result + '"';
end;

{ TGDBMIDebuggerCommandChangeFilename }

function TGDBMIDebuggerCommandChangeFilename.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
begin
  Result := True;
  FSuccess := False;
  //Cleanup our own breakpoints
  FTheDebugger.FExceptionBreak.Clear(Self);
  FTheDebugger.FBreakErrorBreak.Clear(Self);
  FTheDebugger.FRunErrorBreak.Clear(Self);
  if DebuggerState = dsError then Exit;

  FSuccess := ExecuteCommand('-file-exec-and-symbols %s', [FFileName], R);
  if not FSuccess then exit;

  if  (R.State = dsError) and (FFileName <> '')
  then begin
    List := TGDBMINameValueList.Create(R);
    FErrorMsg := DeleteEscapeChars((List.Values['msg']));
    List.Free;
    FSuccess := False;
    Exit;
  end;

  if FFileName = ''
  then exit;

  if tfHasSymbols in TargetInfo^.TargetFlags
  then begin
    // Force setting language
    // Setting extensions dumps GDB (bug #508)
    FSuccess := ExecuteCommand('-gdb-set language pascal', [], [cfCheckError]);
    FSuccess := FSuccess and (DebuggerState <> dsError);
(*
    ExecuteCommand('-gdb-set extension-language .lpr pascal', False);
    if not FHasSymbols then Exit; // file-exec-and-symbols not allways result in no symbols
    ExecuteCommand('-gdb-set extension-language .lrs pascal', False);
    ExecuteCommand('-gdb-set extension-language .dpr pascal', False);
    ExecuteCommand('-gdb-set extension-language .pas pascal', False);
    ExecuteCommand('-gdb-set extension-language .pp pascal', False);
    ExecuteCommand('-gdb-set extension-language .inc pascal', False);
*)
  end;
end;

constructor TGDBMIDebuggerCommandChangeFilename.Create(AOwner: TGDBMIDebugger;
  AFileName: String);
begin
  FFileName := AFileName;
  inherited Create(AOwner);
end;

{ TGDBMIDebuggerCommandInitDebugger }

function TGDBMIDebuggerCommandInitDebugger.DoExecute: Boolean;
  function ParseGDBVersionMI: Boolean;
  var
    R: TGDBMIExecResult;
    S: String;
    List: TGDBMINameValueList;
  begin
    Result := ExecuteCommand('-gdb-version', R);
    Result := Result and (R.Values <> '');
    if (not Result) then exit;

    List := TGDBMINameValueList.Create(R);

    FTheDebugger.FGDBVersion := List.Values['version'];
    S := List.Values['target'];

    FTheDebugger.FGDBCPU := GetPart('', '-', S);
    GetPart('-', '-', S); // strip vendor
    FTheDebugger.FGDBOS := GetPart(['-'], ['-', ''], S);

    List.Free;

    if FTheDebugger.FGDBVersion <> ''
    then exit;

    // maybe a none MI result
    S := GetPart(['configured as \"'], ['\"'], R.Values, False, False);
    if Pos('--target=', S) <> 0 then
      S := GetPart('--target=', '', S);
    FTheDebugger.FGDBCPU := GetPart('', '-', S);
    GetPart('-', '-', S); // strip vendor
    FTheDebugger.FGDBOS := GetPart('-', '-', S);

    FTheDebugger.FGDBVersion := GetPart(['('], [')'], R.Values, False, False);
    if FTheDebugger.FGDBVersion <> '' then Exit;

    FTheDebugger.FGDBVersion := GetPart(['gdb '], [#10, #13], R.Values, True, False);
    if FTheDebugger.FGDBVersion <> '' then Exit;

    Result := False;
  end;

var
  R: TGDBMIExecResult;
begin
  Result := True;
  FSuccess := ExecuteCommand('-gdb-set confirm off', R);
  FSuccess := FSuccess and (r.State <> dsError);
  if (not FSuccess) then exit;
  // for win32, turn off a new console otherwise breaking gdb will fail
  // ignore the error on other platforms
  FSuccess := ExecuteCommand('-gdb-set new-console off', R);
  if (not FSuccess) then exit;

  // set the output width to a great value to avoid unexpected
  // new lines like in large functions or procedures
  ExecuteCommand('set width 50000', []);

  ParseGDBVersionMI;
end;

{ TGDBMIDebuggerCommandStackSetCurrent }

function TGDBMIDebuggerCommandStackSetCurrent.DoExecute: Boolean;
begin
  Result := True;
  ExecuteCommand('-stack-select-frame %d', [FNewCurrent], []);
  FTheDebugger.FInternalStackFrame := FNewCurrent;
end;

constructor TGDBMIDebuggerCommandStackSetCurrent.Create(AOwner: TGDBMIDebugger;
  ACallstack: TCurrentCallStack; ANewCurrent: Integer);
begin
  inherited Create(AOwner, ACallstack);
  FNewCurrent := ANewCurrent;
end;

function TGDBMIDebuggerCommandStackSetCurrent.DebugText: String;
begin
  Result := Format('%s: NewCurrent=%d', [ClassName, FNewCurrent]);
end;

procedure TGDBMIDebuggerCommandStack.DoCallstackFreed(Sender: TObject);
begin
  {$IFDEF DBGMI_QUEUE_DEBUG}
  debugln(['DoCallstackFreed: ', DebugText]);
  {$ENDIF}
  FCallstack := nil;
  Cancel;
end;

function TGDBMIDebuggerCommandStack.SelectThread: Boolean;
var
  R: TGDBMIExecResult;
  t: Integer;
begin
  Result := True;
  FThreadChanged := False;
  if (FCallstack = nil) or (dcsCanceled in SeenStates) then exit;
  t := FCallstack.ThreadId;
  if t = FTheDebugger.FCurrentThreadId then exit;
  FThreadChanged := True;
  Result := ExecuteCommand('-thread-select %d', [t], R);
  FTheDebugger.FInternalThreadId := t;
  Result := Result and (R.State <> dsError);
end;

procedure TGDBMIDebuggerCommandStack.UnSelectThread;
var
  R: TGDBMIExecResult;
begin
  if not FThreadChanged then exit;
  ExecuteCommand('-thread-select %d', [FTheDebugger.FCurrentThreadId], R);
  FTheDebugger.FInternalThreadId := FTheDebugger.FCurrentThreadId;
end;

constructor TGDBMIDebuggerCommandStack.Create(AOwner: TGDBMIDebugger;
  ACallstack: TCurrentCallStack);
begin
  inherited Create(AOwner);
  FCallstack := ACallstack;
  FCallstack.AddFreeeNotification(@DoCallstackFreed);
end;

destructor TGDBMIDebuggerCommandStack.Destroy;
begin
  if FCallstack <> nil
  then FCallstack.RemoveFreeeNotification(@DoCallstackFreed);
  inherited Destroy;
end;

{ TGDBMIBreakPoints }

function TGDBMIBreakPoints.FindById(AnId: Integer): TGDBMIBreakPoint;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    Result := TGDBMIBreakPoint(Items[n]);
    if  (Result.FBreakID = AnId)
    then Exit;
  end;
  Result := nil;
end;

{ TGDBMIDebuggerCommandKill }

function TGDBMIDebuggerCommandKill.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
  CmdRes: Boolean;
begin
  Result := True;
  // not supported yet
  // ExecuteCommand('-exec-abort');
  CmdRes := ExecuteCommand('kill', [], [], 1500); // Hardcoded timeout
  if CmdRes
  then CmdRes := ExecuteCommand('info program', R, [], 1500); // Hardcoded timeout
  if (not CmdRes)
  or (Pos('not being run', R.Values) <= 0)
  then begin
    FTheDebugger.DebugProcess.Terminate(0);
    SetDebuggerState(dsError); // failed to stop
    exit;
  end;
  SetDebuggerState(dsStop);
end;

{ TGDBMIDebuggerCommandChangeThread }

function TGDBMIDebuggerCommandChangeThread.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := True;
  FSuccess := ExecuteCommand('-thread-select %d', [FNewId], R);
  if FSuccess then
    FSuccess := R.State <> dsError;
  FTheDebugger.FCurrentThreadId := FNewId;
  FTheDebugger.FInternalThreadId:= FNewId;
end;

constructor TGDBMIDebuggerCommandChangeThread.Create(AOwner: TGDBMIDebugger; ANewId: Integer);
begin
  inherited Create(AOwner);
  FNewId := ANewId;
  FSuccess := False;
end;

function TGDBMIDebuggerCommandChangeThread.DebugText: String;
begin
  Result := Format('%s: NewId=%d', [ClassName, FNewId]);
end;

{ TGDBMIThreads }

procedure TGDBMIThreads.DoThreadsDestroyed(Sender: TObject);
begin
  if FGetThreadsCmdObj = Sender
  then FGetThreadsCmdObj:= nil;
end;

procedure TGDBMIThreads.DoThreadsFinished(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandThreads;
  i: Integer;
begin
  if Monitor = nil then exit;
  Cmd := TGDBMIDebuggerCommandThreads(Sender);
  if CurrentThreads = nil then exit;

  if not Cmd.Success then begin
    CurrentThreads.SetValidity(ddsInvalid);
    CurrentThreads.CurrentThreadId := Debugger.FCurrentThreadId;
    exit;
  end;

  CurrentThreads.Clear;
  for i := 0 to Cmd.Count - 1 do
    CurrentThreads.Add(Cmd.Threads[i]);

  CurrentThreads.SetValidity(ddsValid);
  CurrentThreads.CurrentThreadId := Cmd.CurrentThreadId;
  Debugger.FCurrentThreadId := CurrentThreads.CurrentThreadId;
end;

procedure TGDBMIThreads.DoChangeThreadsDestroyed(Sender: TObject);
begin
  if FChangeThreadsCmdObj = Sender
  then FChangeThreadsCmdObj := nil;
end;

procedure TGDBMIThreads.DoChangeThreadsFinished(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandChangeThread;
begin
  if Monitor = nil then exit;
  Cmd := TGDBMIDebuggerCommandChangeThread(Sender);

  Debugger.DoThreadChanged;
  if not Cmd.Success
  then exit;
  if CurrentThreads <> nil
  then CurrentThreads.CurrentThreadId := Cmd.NewId;
end;

function TGDBMIThreads.GetDebugger: TGDBMIDebugger;
begin
  Result := TGDBMIDebugger(inherited Debugger);
end;

procedure TGDBMIThreads.ThreadsNeeded;
var
  ForceQueue: Boolean;
begin
  if Debugger = nil then Exit;

  if (Debugger.State in [dsPause, dsInternalPause])
  then begin
    FGetThreadsCmdObj := TGDBMIDebuggerCommandThreads.Create(Debugger);
    FGetThreadsCmdObj.OnExecuted  := @DoThreadsFinished;
    FGetThreadsCmdObj.OnDestroy    := @DoThreadsDestroyed;
    FGetThreadsCmdObj.Properties := [dcpCancelOnRun];
    FGetThreadsCmdObj.Priority := GDCMD_PRIOR_THREAD;
    // If a ExecCmd is running, then defer exec until the exec cmd is done
    ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
              and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
              and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued)
              and (Debugger.State <> dsInternalPause);
    TGDBMIDebugger(Debugger).QueueCommand(FGetThreadsCmdObj, ForceQueue);
    (* DoEvaluationFinished may be called immediately at this point *)
  end;
end;

procedure TGDBMIThreads.CancelEvaluation;
begin
  if FGetThreadsCmdObj <> nil
  then begin
    FGetThreadsCmdObj.OnExecuted := nil;
    FGetThreadsCmdObj.OnDestroy := nil;
    FGetThreadsCmdObj.Cancel;
  end;
  FGetThreadsCmdObj := nil;
end;

constructor TGDBMIThreads.Create(const ADebugger: TDebugger);
begin
  inherited;
end;

destructor TGDBMIThreads.Destroy;
begin
  CancelEvaluation;
  inherited Destroy;
end;

procedure TGDBMIThreads.RequestMasterData;
begin
  ThreadsNeeded;
end;

procedure TGDBMIThreads.ChangeCurrentThread(ANewId: Integer);
var
  ForceQueue: Boolean;
begin
  if Debugger = nil then Exit;
  if not(Debugger.State in [dsPause, dsInternalPause]) then exit;

  if FChangeThreadsCmdObj <> nil then begin
    if FChangeThreadsCmdObj.State = dcsQueued then
      FChangeThreadsCmdObj.NewId := ANewId;
    exit;
  end;

  FChangeThreadsCmdObj := TGDBMIDebuggerCommandChangeThread.Create(Debugger, ANewId);
  FChangeThreadsCmdObj.OnExecuted  := @DoChangeThreadsFinished;
  FChangeThreadsCmdObj.OnDestroy   := @DoChangeThreadsDestroyed;
  FChangeThreadsCmdObj.Properties := [dcpCancelOnRun];
  FChangeThreadsCmdObj.Priority := GDCMD_PRIOR_USER_ACT;
  // If a ExecCmd is running, then defer exec until the exec cmd is done
  ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
            and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
            and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued)
            and (Debugger.State <> dsInternalPause);
  TGDBMIDebugger(Debugger).QueueCommand(FChangeThreadsCmdObj, ForceQueue);
  (* DoEvaluationFinished may be called immediately at this point *)
end;

{ TGDBMIDebuggerCommandThreads }

function TGDBMIDebuggerCommandThreads.GetThread(AnIndex: Integer): TThreadEntry;
begin
  Result := FThreads[AnIndex];
end;

function TGDBMIDebuggerCommandThreads.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
  List, EList, ArgList: TGDBMINameValueList;
  i, j: Integer;
  line, ThrId: Integer;
  func, filename, fullname: String;
  ThrName, ThrState: string;
  addr: TDBGPtr;
  Arguments: TStringList;
begin
(* TODO: none MI command
<info threads>
&"info threads\n"
~"  5 thread 4928.0x1f50  0x77755ca4 in ntdll!LdrAccessResource () from C:\\Windows\\system32\\ntdll.dll\n"
~"  4 thread 4928.0x12c8  0x77755ca4 in ntdll!LdrAccessResource () from C:\\Windows\\system32\\ntdll.dll\n"
~"* 1 thread 4928.0x1d18  TFORM1__BUTTON1CLICK (SENDER=0x209ef0, this=0x209a20) at unit1.pas:65\n"
^done
(gdb)

*)

  Result := True;

  if not ExecuteCommand('-thread-info', R)
  then exit;
  if r.State = dsError then exit;;
  List := TGDBMINameValueList.Create(R);
  EList := TGDBMINameValueList.Create;
  ArgList := TGDBMINameValueList.Create;
  try
    FCurrentThreadId := StrToIntDef(List.Values['current-thread-id'], -1);
    FTheDebugger.FInternalThreadId := FTheDebugger.FCurrentThreadId;
    if FCurrentThreadId < 0 then exit;
    FSuccess := True;

    List.SetPath('threads');
    SetLength(FThreads, List.Count);
    for i := 0 to List.Count - 1 do begin
      EList.Init(List.Items[i]^.Name);
      ThrId    := StrToIntDef(EList.Values['id'], -2);
      ThrName  := EList.Values['target-id'];
      ThrState := EList.Values['state'];
      EList.SetPath('frame');
      addr := StrToQWordDef(EList.Values['addr'], 0);
      func := EList.Values['func'];
      filename := ConvertGdbPathAndFile(EList.Values['file']);
      fullname := ConvertGdbPathAndFile(EList.Values['fullname']);
      line := StrToIntDef(EList.Values['line'], 0);

      EList.SetPath('args');
      Arguments := TStringList.Create;
      for j := 0 to EList.Count - 1 do begin
        ArgList.Init(EList.Items[j]^.Name);
        Arguments.Add(ArgList.Values['name'] + '=' + DeleteEscapeChars(ArgList.Values['value']));
      end;


      FThreads[i] := TThreadEntry.Create(
        0, addr,
        Arguments,
        func,
        FTheDebugger.UnitInfoProvider.GetUnitInfoFor(filename, fullname),
        line,
        ThrId,ThrName, ThrState
      );

      Arguments.Free;
    end;

  finally
    FreeAndNil(ArgList);
    FreeAndNil(EList);
    FreeAndNil(List);
  end;
end;

constructor TGDBMIDebuggerCommandThreads.Create(AOwner: TGDBMIDebugger);
begin
  inherited;
  FSuccess := False;
end;

destructor TGDBMIDebuggerCommandThreads.Destroy;
var
  i: Integer;
begin
  for i := 0 to length(FThreads) - 1 do FreeAndNil(FThreads[i]);
  FThreads := nil;
  inherited Destroy;
end;

function TGDBMIDebuggerCommandThreads.Count: Integer;
begin
  Result := length(FThreads);
end;

{ TGDBMIDebuggerCommandRegisterModified }

function TGDBMIDebuggerCommandRegisterModified.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
  n, idx: Integer;
begin
  Result := True;
  if length(FModifiedToUpdate) = 0
  then exit;

  for n := Low(FModifiedToUpdate) to High(FModifiedToUpdate) do
    FModifiedToUpdate[n] := False;

  ExecuteCommand('-data-list-changed-registers', [cfIgnoreError], R);
  if R.State = dsError then Exit;

  List := TGDBMINameValueList.Create(R, ['changed-registers']);
  for n := 0 to List.Count - 1 do
  begin
    idx := StrToIntDef(Unquote(List.GetString(n)), -1);
    if idx < Low(FModifiedToUpdate) then Continue;
    if idx > High(FModifiedToUpdate) then Continue;

    FModifiedToUpdate[idx] := True;
  end;
  FreeAndNil(List);
end;

constructor TGDBMIDebuggerCommandRegisterModified.Create(AOwner: TGDBMIDebugger;
  ModifiedToUpdate: TBoolArray);
begin
  inherited Create(AOwner);
  FModifiedToUpdate := ModifiedToUpdate;
end;

function TGDBMIDebuggerCommandRegisterModified.DebugText: String;
begin
  Result := Format('%s: Reg-Cnt=%d', [ClassName, length(FModifiedToUpdate)]);
end;

{ TGDBMINameValueBasedList }

constructor TGDBMINameValueBasedList.Create;
begin
  FNameValueList := TGDBMINameValueList.Create;
end;

constructor TGDBMINameValueBasedList.Create(const AResultValues: String);
begin
  FNameValueList := TGDBMINameValueList.Create(AResultValues);
  PreParse;
end;

constructor TGDBMINameValueBasedList.Create(AResult: TGDBMIExecResult);
begin
  Create(AResult.Values);
end;

destructor TGDBMINameValueBasedList.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FNameValueList);
end;

procedure TGDBMINameValueBasedList.Init(AResultValues: string);
begin
  FNameValueList.Init(AResultValues);
  PreParse;
end;

procedure TGDBMINameValueBasedList.Init(AResult: TGDBMIExecResult);
begin
  Init(AResult.Values);
end;

{ TGDBMIDisassembleResultList }

procedure TGDBMIDisassembleResultList.PreParse;
const
  SrcAndAsm = 'src_and_asm_line';
  SrcAndAsmLen = length(SrcAndAsm);
var
  Itm: PGDBMINameValue;
  SrcList: TGDBMINameValueList;
  i, j: Integer;
  SFile, SLine: TPCharWithLen;
begin
  {$IFDEF DBG_VERBOSE}
  // The "^done" is stripped already
  if (FNameValueList.Count <> 1) or
     (FNameValueList.IndexOf('asm_insns') < 0)
  then
    debugln(['WARNING: TGDBMIDisassembleResultList: Unexpected Entries']);
  {$ENDIF}
  HasItemPointerList := False;
  FNameValueList.SetPath('asm_insns');
  FCount := 0;
  SetLength(FItems, FNameValueList.Count * 4);
  FHasSourceInfo := False;
  SrcList := nil;
  for i := 0 to FNameValueList.Count - 1 do begin
    Itm := FNameValueList.Items[i];
    if (Itm^.Name.Len = SrcAndAsmLen)
    and (strlcomp(Itm^.Name.Ptr, PChar(SrcAndAsm), SrcAndAsmLen) = 0)
    then begin
      // Source and asm
      FHasSourceInfo := True;
      if SrcList = nil
      then SrcList := TGDBMINameValueList.Create(Itm^.Value)
      else SrcList.Init(Itm^.Value);
      SFile := SrcList.ValuesPtr['file'];
      SLine := SrcList.ValuesPtr['line'];
      SrcList.SetPath('line_asm_insn');

      if FCount + SrcList.Count >= length(FItems)
      then SetLength(FItems, FCount + SrcList.Count + 20);
      for j := 0 to SrcList.Count - 1 do begin
        FItems[FCount].AsmEntry   := SrcList.Items[j]^.Name;
        FItems[FCount].SrcFile    := SFile;
        FItems[FCount].SrcLine    := SLine;
        FItems[FCount].ParsedInfo.SrcStatementIndex := j;
        FItems[FCount].ParsedInfo.SrcStatementCount := SrcList.Count;
        inc(FCount);
      end;
    end
    else
    if (Itm^.Name.Len > 1)
    and (Itm^.Name.Ptr[0] = '{')
    and (Itm^.Value.Len = 0)
    then begin
      // Asm only
      if FCount + 1 >= length(FItems)
      then SetLength(FItems, FCount + 20);
      FItems[FCount].AsmEntry    := Itm^.Name;
      FItems[FCount].SrcFile.Ptr := nil;
      FItems[FCount].SrcFile.Len := 0;
      FItems[FCount].SrcLine.Ptr := nil;
      FItems[FCount].SrcLine.Len := 0;
      FItems[FCount].ParsedInfo.SrcStatementIndex := 0;
      FItems[FCount].ParsedInfo.SrcStatementCount := 0;
      inc(FCount);
    end
    else
    begin
      // unknown
      debugln(['WARNING: TGDBMIDisassembleResultList.Parse: unknown disass entry',
              DbgsPCLen(Itm^.Name),': ',DbgsPCLen(Itm^.Value)]);
    end;
  end;
  FreeAndNil(SrcList);
end;

function TGDBMIDisassembleResultList.GetLastItem: PDisassemblerEntry;
begin
  if HasItemPointerList
  then begin
    Result := ItemPointerList[Count - 1];
    exit;
  end;
  ParseItem(Count - 1);
  Result := @FItems[Count - 1].ParsedInfo;
end;

function TGDBMIDisassembleResultList.SortByAddress: Boolean;
var
  i, j: Integer;
  Itm1: PDisassemblerEntry;
begin
  Result := True;
  SetLength(ItemPointerList, FCount);
  for i := 0 to Count - 1 do begin
    Itm1 := Item[i];
    j := i - 1;
    while j >= 0 do begin
      if ItemPointerList[j]^.Addr > Itm1^.Addr
      then ItemPointerList[j+1] := ItemPointerList[j]
      else break;
      dec(j);
    end;
    ItemPointerList[j+1] := Itm1;
  end;
  HasItemPointerList := True;
end;

constructor TGDBMIDisassembleResultList.CreateSubList(ASource: TGDBMIDisassembleResultList;
  AStartIdx, ACount: Integer);
begin
  Create;
  InitSubList(ASource, AStartIdx, ACount);
end;

procedure TGDBMIDisassembleResultList.InitSubList(ASource: TGDBMIDisassembleResultList;
  AStartIdx, ACount: Integer);
var
  i: Integer;
begin
  SetLength(ItemPointerList, ACount);
  FCount := ACount;
  for i := 0 to ACount - 1 do
    ItemPointerList[i] := ASource.Item[AStartIdx + i];
  HasItemPointerList := True;
end;

function TGDBMIDisassembleResultList.GetItem(Index: Integer): PDisassemblerEntry;
begin
  if HasItemPointerList
  then begin
    Result := ItemPointerList[Index];
    exit;
  end;
  ParseItem(Index);
  Result := @FItems[Index].ParsedInfo;
end;

procedure TGDBMIDisassembleResultList.ParseItem(Index: Integer);
var
  AsmList: TGDBMINameValueList;
begin
  if FItems[Index].AsmEntry.Ptr = nil
  then exit;
  AsmList := TGDBMINameValueList.Create(FItems[Index].AsmEntry);

  FItems[Index].ParsedInfo.SrcFileName := ConvertGdbPathAndFile(PCLenToString(FItems[Index].SrcFile, True));
  FItems[Index].ParsedInfo.SrcFileLine := PCLenToInt(FItems[Index].SrcLine, 0);
  // SrcStatementIndex, SrcStatementCount are already set

  FItems[Index].ParsedInfo.Addr      := PCLenToQWord(AsmList.ValuesPtr['address'], 0);
  FItems[Index].ParsedInfo.Statement :=
    UnEscapeBackslashed(PCLenToString(AsmList.ValuesPtr['inst'], True), [uefTab], 16);
  FItems[Index].ParsedInfo.FuncName  := PCLenToString(AsmList.ValuesPtr['func-name'], True);
  FItems[Index].ParsedInfo.Offset    := PCLenToInt(AsmList.ValuesPtr['offset'], 0);

  FItems[Index].AsmEntry.Ptr := nil;
  FreeAndNil(AsmList);
end;

procedure TGDBMIDisassembleResultList.SetCount(const AValue: Integer);
begin
  if FCount = AValue then exit;
  if FCount > length(FItems)
  then raise Exception.Create('Invalid Count');
  FCount := AValue;
end;

procedure TGDBMIDisassembleResultList.SetItem(Index: Integer;
  const AValue: PDisassemblerEntry);
begin
  if HasItemPointerList
  then begin
    ItemPointerList[Index]^ := AValue^;
    exit;
  end;
  FItems[Index].ParsedInfo := AValue^;
  FItems[Index].AsmEntry.Ptr := nil;
end;

procedure TGDBMIDisassembleResultList.SetLastItem(const AValue: PDisassemblerEntry);
begin
  if HasItemPointerList
  then begin
    ItemPointerList[Count - 1]^ := AValue^;
    exit;
  end;
  FItems[Count - 1].ParsedInfo := AValue^;
  FItems[Count - 1].AsmEntry.Ptr := nil;
end;

{ TGDBMIDisassembleResultFunctionIterator }

constructor TGDBMIDisassembleResultFunctionIterator.Create(AList: TGDBMIDisassembleResultList;
  AStartIdx: Integer; ALastSubListEndAddr: TDBGPtr;
  AnAddressToLocate, AnAddForLineAfterCounter: TDBGPtr);
begin
  FList := AList;
  FStartedAtIndex := AStartIdx;
  FStartIdx := AStartIdx;
  FLastSubListEndAddr := ALastSubListEndAddr;
  FAddressToLocate := AnAddressToLocate;
  FAddForLineAfterCounter := AnAddForLineAfterCounter;
  FMaxIdx := FList.Count - 1;
  if FStartIdx > FMaxIdx
  then raise Exception.Create('internal error');
  FIndexOfLocateAddress := 1;
  FOffsetOfLocateAddress := -1;
  FIndexOfCounterAddress := -1;
  FSublistNumber := -1;
end;

function TGDBMIDisassembleResultFunctionIterator.EOL: Boolean;
begin
  Result := FStartIdx > FMaxIdx ;
end;

function TGDBMIDisassembleResultFunctionIterator.NextSubList
  (var AResultList: TGDBMIDisassembleResultList): Boolean;
var
  WasBeforeStart: Boolean;
  HasPrcName: Boolean;
  PrcBaseAddr: TDBGPtr;
  Itm: PDisassemblerEntry;
  NextIdx: Integer;
  HasLocate: Boolean;
begin
  FCurIdx := FStartIdx;
  if FStartIdx > FMaxIdx
  then raise Exception.Create('internal error');
  inc(FSublistNumber);

  (* The name may change in the middle of a function. Check for either:
     - change between no-name and has-name
     - change of the base-address (addr-offset), if the offset is valid (if has-name)
  *)
  HasPrcName := FList.Item[FStartIdx]^.FuncName <> ''; // can use offsets
  {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$ENDIF} // Overflow is allowed to occur
  PrcBaseAddr := FList.Item[FStartIdx]^.Addr - FList.Item[FStartIdx]^.Offset;
  {$POP}

  WasBeforeStart := FList.Item[FStartIdx]^.Addr < FAddressToLocate;
  HasLocate := False;

  NextIdx :=  FStartIdx + 1;
  while NextIdx <= FMaxIdx do
  begin
    Itm := FList.Item[NextIdx];
    {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$ENDIF} // Overflow is allowed to occur
    // Also check the next statement after PrcName.
    // If it has FOffsetOfLocateAddress > 0, then FAddressToLocate is in current block, but not matched
    if (Itm^.Addr = FAddressToLocate)
    then begin
      FIndexOfLocateAddress := NextIdx;
      FOffsetOfLocateAddress := 0;
      WasBeforeStart := False;
      HasLocate := True;
    end
    else if WasBeforeStart and (Itm^.Addr > FAddressToLocate)
    then begin
      FIndexOfLocateAddress := NextIdx - 1;
      FOffsetOfLocateAddress := FAddressToLocate - FList.Item[NextIdx-1]^.Addr;
      WasBeforeStart := False;
      HasLocate := True;
    end;
    if (FAddForLineAfterCounter > 0)
    and (  (Itm^.Addr = FAddForLineAfterCounter)
        or ((Itm^.Addr > FAddForLineAfterCounter) and (FIndexOfCounterAddress < 0)) )
    then FIndexOfCounterAddress := NextIdx;

    if (HasPrcName <> (Itm^.FuncName <> ''))
    or (HasPrcName and (PrcBaseAddr <> Itm^.Addr - Itm^.Offset))
    then break;
    {$POP}

    inc(NextIdx);
  end;

  if AResultList = nil
  then AResultList := TGDBMIDisassembleResultList.CreateSubList(FList, FStartIdx, NextIdx - FStartIdx)
  else AResultList.InitSubList(FList, FStartIdx, NextIdx - FStartIdx);
  FStartIdx := NextIdx;

  // Does the next address look good?
  // And is AStartAddrHit ok
  //Result := ((NextIdx > FMaxIdx) or (FList.Item[NextIdx]^.Offset = 0))
  //      and
  Result := ( (not HasLocate) or ((FIndexOfLocateAddress < 0) or (FOffsetOfLocateAddress = 0)) );
end;

function TGDBMIDisassembleResultFunctionIterator.IsFirstSubList: Boolean;
begin
  Result := FSublistNumber = 0;
end;

function TGDBMIDisassembleResultFunctionIterator.CountLinesAfterCounterAddr: Integer;
begin
  Result := -1;
  if FIndexOfCounterAddress >= 0 then
  Result := CurrentIndex - IndexOfCounterAddress - 1;
end;

function TGDBMIDisassembleResultFunctionIterator.CurrentFixedAddr(AOffsLimit: Integer): TDBGPtr;
begin
  Result := FList.Item[CurrentIndex]^.Addr - Min(FList.Item[CurrentIndex]^.Offset, AOffsLimit);
end;

function TGDBMIDisassembleResultFunctionIterator.NextStartAddr: TDBGPtr;
begin
  if NextIndex <= FMaxIdx
  then Result := FList.Item[NextIndex]^.Addr - FList.Item[NextIndex]^.Offset
  else Result := FLastSubListEndAddr;
end;

function TGDBMIDisassembleResultFunctionIterator.NextStartOffs: Integer;
begin
  if NextIndex <= FMaxIdx
  then Result := FList.Item[NextIndex]^.Offset
  else Result := 0;
end;

{ TGDBMIMemoryDumpResultList }

function TGDBMIMemoryDumpResultList.GetItemNum(Index: Integer): Integer;
begin
  Result := PCLenToInt(FNameValueList.Items[Index]^.Name, 0);
end;

function TGDBMIMemoryDumpResultList.GetItem(Index: Integer): TPCharWithLen;
begin
  Result := FNameValueList.Items[Index]^.Name;
end;

function TGDBMIMemoryDumpResultList.GetItemTxt(Index: Integer): string;
var
  itm: PGDBMINameValue;
begin
  itm := FNameValueList.Items[Index];
  if itm <> nil
  then Result := PCLenToString(itm^.Name, True)
  else Result := '';
end;

procedure TGDBMIMemoryDumpResultList.PreParse;
begin
  FNameValueList.SetPath('memory');
  if FNameValueList.Count = 0 then exit;
  FNameValueList.Init(FNameValueList.Items[0]^.Name);
  FAddr := PCLenToQWord(FNameValueList.ValuesPtr['addr'], 0);
  FNameValueList.SetPath('data');
end;

function TGDBMIMemoryDumpResultList.Count: Integer;
begin
  Result := FNameValueList.Count;
end;

function TGDBMIMemoryDumpResultList.AsText(AStartOffs, ACount: Integer;
  AAddrWidth: Integer): string;
var
  i: LongInt;
begin
  if AAddrWidth > 0
  then Result := IntToHex(addr + AStartOffs, AAddrWidth) + ':'
  else Result := '';
  for i := AStartOffs to AStartOffs + ACount do begin
    if i >= ACount then exit;
    Result := Result + ' ' + PCLenPartToString(Item[i], 3, 2);
  end;
end;

{ TGDBMIDisassembler }

procedure TGDBMIDisassembler.DoDisassembleDestroyed(Sender: TObject);
begin
  if FDisassembleEvalCmdObj = Sender
  then FDisassembleEvalCmdObj := nil;
end;

procedure TGDBMIDisassembler.DoDisassembleProgress(Sender: TObject);
begin
  Changed;;
end;

procedure TGDBMIDisassembler.DoDisassembleExecuted(Sender: TObject);
begin
  // Results were added from inside the TGDBMIDebuggerCommandDisassembe object
  FLastExecAddr := TGDBMIDebuggerCommandDisassembe(Sender).StartAddr;
  FDisassembleEvalCmdObj := nil;
  Changed;
end;

function TGDBMIDisassembler.PrepareEntries(AnAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer): Boolean;
var
  ForceQueue: Boolean;
begin
  Result := False;
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause])
  then exit;

  if (FDisassembleEvalCmdObj <> nil)
  then begin
    if FDisassembleEvalCmdObj.State <> dcsQueued
    then exit; // the request will be done again, after the next "Changed" (which should be the edn of the current command)

    if (AnAddr < FDisassembleEvalCmdObj.StartAddr)
    and (AnAddr >= FDisassembleEvalCmdObj.StartAddr
        - (ALinesAfter + FDisassembleEvalCmdObj.LinesBefore) * DAssBytesPerCommandAvg)
    then begin
      // merge before
      {$IFDEF DBG_VERBOSE}
      debugln(['INFO: TGDBMIDisassembler.PrepareEntries  MERGE request at START: NewStartAddr=', AnAddr,
               ' NewLinesBefore=', Max(ALinesBefore, FDisassembleEvalCmdObj.LinesBefore), ' OldStartAddr=', FDisassembleEvalCmdObj.StartAddr,
               '  OldLinesBefore=', FDisassembleEvalCmdObj.LinesBefore ]);
      {$ENDIF}
      FDisassembleEvalCmdObj.StartAddr := AnAddr;
      FDisassembleEvalCmdObj.LinesBefore := Max(ALinesBefore, FDisassembleEvalCmdObj.LinesBefore);
      exit;
    end;

    if (AnAddr > FDisassembleEvalCmdObj.EndAddr)
    and (AnAddr <= FDisassembleEvalCmdObj.EndAddr
        + (ALinesBefore + FDisassembleEvalCmdObj.LinesAfter) * DAssBytesPerCommandAvg)
    then begin
      // merge after
      {$IFDEF DBG_VERBOSE}
      debugln(['INFO: TGDBMIDisassembler.PrepareEntries  MERGE request at END: NewEndAddr=', AnAddr,
               ' NewLinesAfter=', Max(ALinesAfter, FDisassembleEvalCmdObj.LinesAfter), ' OldEndAddr=', FDisassembleEvalCmdObj.EndAddr,
               '  OldLinesAfter=', FDisassembleEvalCmdObj.LinesAfter ]);
      {$ENDIF}
      FDisassembleEvalCmdObj.EndAddr := AnAddr;
      FDisassembleEvalCmdObj.LinesAfter := Max(ALinesAfter, FDisassembleEvalCmdObj.LinesAfter);
      exit;
    end;

    exit;
  end;

  FDisassembleEvalCmdObj := TGDBMIDebuggerCommandDisassembe.Create
    (TGDBMIDebugger(Debugger), EntryRanges, AnAddr, AnAddr, ALinesBefore, ALinesAfter);
  FDisassembleEvalCmdObj.OnExecuted := @DoDisassembleExecuted;
  FDisassembleEvalCmdObj.OnProgress  := @DoDisassembleProgress;
  FDisassembleEvalCmdObj.OnDestroy  := @DoDisassembleDestroyed;
  FDisassembleEvalCmdObj.Priority := GDCMD_PRIOR_DISASS;
  FDisassembleEvalCmdObj.Properties := [dcpCancelOnRun];
  ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
            and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
            and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued)
            and (Debugger.State <> dsInternalPause);
  TGDBMIDebugger(Debugger).QueueCommand(FDisassembleEvalCmdObj, ForceQueue);
  (* DoDepthCommandExecuted may be called immediately at this point *)
  Result := FDisassembleEvalCmdObj = nil; // already executed
end;

function TGDBMIDisassembler.HandleRangeWithInvalidAddr(ARange: TDBGDisassemblerEntryRange;
  AnAddr: TDbgPtr; var ALinesBefore, ALinesAfter: Integer): boolean;
var
  i, c: Integer;
begin
  if AnAddr = FLastExecAddr
  then begin
    i := 0;
    c := ARange.Count;
    while i < c do
    begin
      if ARange.EntriesPtr[i]^.Addr > AnAddr
      then break;
      inc(i);
    end;
    if i > 0
    then dec(i);
    ALinesBefore := i;
    ALinesAfter := ARange.Count - 1 - i;
    Result := True;
    exit;
  end;
  Result := inherited HandleRangeWithInvalidAddr(ARange, AnAddr, ALinesBefore, ALinesAfter);
end;

procedure TGDBMIDisassembler.Clear;
begin
  inherited Clear;
  if FDisassembleEvalCmdObj <> nil
  then begin
    FDisassembleEvalCmdObj.OnExecuted := nil;
    FDisassembleEvalCmdObj.OnDestroy := nil;
    FDisassembleEvalCmdObj.Cancel;
  end;
  FDisassembleEvalCmdObj := nil;
end;

function TGDBMIDisassembler.PrepareRange(AnAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer): Boolean;
begin
  if AnAddr <> FLastExecAddr
  then FLastExecAddr := 0;
  Result := inherited PrepareRange(AnAddr, ALinesBefore, ALinesAfter);
end;

{ TGDBMIDebuggerCommandDisassembe }

procedure TGDBMIDebuggerCommandDisassembe.DoProgress;
begin
  if assigned(FOnProgress)
  then FOnProgress(Self);
end;

function TGDBMIDebuggerCommandDisassembe.DoExecute: Boolean;
  type
    TAddressValidity =
      (avFoundFunction, avFoundRange, avFoundStatement,  // known address
       avGuessed,                                        // guessed
       avExternRequest,                                  // As requested by external caller
       avPadded                                          // Padded, because address was not known for sure
      );
    TAddress = record
      Value, GuessedValue: TDBGPtr;
      Offset: Integer;
      Validity: TAddressValidity;
    end;

  const
    TrustedValidity = [avFoundFunction, avFoundRange, avFoundStatement];

  function InitAddress(AValue: TDBGPtr; AValidity: TAddressValidity;
    AnOffset: Integer = -1): TAddress;
  begin
    Result.Value          := AValue;
    Result.GuessedValue   := AValue;;
    Result.Offset   := AnOffset;
    Result.Validity := AValidity;
  end;

  procedure PadAddress(var AnAddr: TAddress; APad: Integer);
  begin
    {$PUSH}{$Q-}{$R-}// APad can be negative, but will be expanded to TDbgPtr (QWord)
    AnAddr.Value    := AnAddr.Value + APad;
    {$POP}
    AnAddr.Validity := avPadded;
    AnAddr.Offset   := -1;
  end;

  function DbgsAddr(const AnAddr: TAddress): string;
  const
    ValidityName: array [TAddressValidity] of string =
      ('FoundFunction', 'FoundRange', 'FoundStatemnet', 'Guessed', 'ExternRequest', 'Padded');
  begin
    Result := Format('[[ Value=%u, Guessed=%u, Offset=%d, Validity=%s ]]',
                     [AnAddr.Value, AnAddr.GuessedValue, AnAddr.Offset, ValidityName[AnAddr.Validity]]);
  end;

  function ExecDisassmble(AStartAddr, AnEndAddr: TDbgPtr; WithSrc: Boolean;
    AResultList: TGDBMIDisassembleResultList = nil;
    ACutBeforeEndAddr: Boolean = False): TGDBMIDisassembleResultList;
  var
    WS: Integer;
    R: TGDBMIExecResult;
  begin
    WS := 0;
    if WithSrc
    then WS := 1;;
    Result := AResultList;
    ExecuteCommand('-data-disassemble -s %u -e %u -- %d', [AStartAddr, AnEndAddr, WS], R);
    if Result <> nil
    then Result.Init(R)
    else Result := TGDBMIDisassembleResultList.Create(R);
    if ACutBeforeEndAddr and Result.HasSourceInfo
    then Result.SortByAddress;
    while ACutBeforeEndAddr and (Result.Count > 0) and (Result.LastItem^.Addr >= AnEndAddr)
    do Result.Count :=  Result.Count - 1;
  end;

  function ExecMemDump(AStartAddr: TDbgPtr; ACount: Cardinal;
    AResultList: TGDBMIMemoryDumpResultList = nil): TGDBMIMemoryDumpResultList;
  var
    R: TGDBMIExecResult;
  begin
    Result := AResultList;
    ExecuteCommand('-data-read-memory %u x 1 1 %u', [AStartAddr, ACount], R);
    if Result <> nil
    then Result.Init(R)
    else Result := TGDBMIMemoryDumpResultList.Create(R);
  end;

  // Set Value, based on GuessedValue
  function AdjustToKnowFunctionStart(var AStartAddr: TAddress): Boolean;
  var
    DisAssList: TGDBMIDisassembleResultList;
    DisAssItm: PDisassemblerEntry;
    s: TDBGPtr;
  begin
    Result := False;
    // TODO: maybe try "info symbol <addr>
    s := (AStartAddr.GuessedValue -1) div 4 * 4;  // 4 byte boundary
    DisAssList := ExecDisassmble(s, s+1, False);
    if DisAssList.Count > 0 then begin
      DisAssItm := DisAssList.Item[0];
      if (DisAssItm^.FuncName <> '') and (DisAssItm^.Addr <> 0) and (DisAssItm^.Offset >= 0)
      then begin
        AStartAddr.Value := DisAssItm^.Addr - DisAssItm^.Offset;       // This should always be good
        AStartAddr.Offset := 0;
        AStartAddr.Validity := avFoundFunction;
        Result := True;
      end;
    end;
    FreeAndNil(DisAssList);
  end;

  // Set Value, based on GuessedValue
  function AdjustToRangeOrKnowFunctionStart(var AStartAddr: TAddress;
    ARangeBefore: TDBGDisassemblerEntryRange): Boolean;
  begin
    Result := False;
    AStartAddr.Offset := -1;
    AStartAddr.Validity := avGuessed;
    if AdjustToKnowFunctionStart(AStartAddr)
    then begin
      // funtion found, check for range
      if (ARangeBefore <> nil) and (ARangeBefore.LastAddr > AStartAddr.Value)
      and (ARangeBefore.Count > DAssRangeOverFuncTreshold)
      and (ARangeBefore.EntriesPtr[ARangeBefore.Count - 1]^.Offset > DAssRangeOverFuncTreshold  * DAssBytesPerCommandAvg)
      then begin
        // got a big overlap, don't redo the whole function
        {$IFDEF DBG_VERBOSE}
        debugln(['INFO: Restarting inside previous range for known function-start=', DbgsAddr(AStartAddr),'  and ARangeBefore=', dbgs(ARangeBefore)]);
        {$ENDIF}
        // redo one statement
        {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$R-}{$ENDIF} // Overflow is allowed to occur
        AStartAddr.Value  := ARangeBefore.EntriesPtr[ARangeBefore.Count - 1]^.Addr;
        AStartAddr.Offset := ARangeBefore.EntriesPtr[ARangeBefore.Count - 1]^.Offset;
        AStartAddr.Validity := avFoundRange;
        //AStartAddr - ARangeBefore.EntriesPtr[ARangeBefore.Count - DAssRangeOverFuncTreshold]^.Addr ;
        {$POP}
      end
    end
    else begin
      {$IFDEF DBG_VERBOSE}
      debugln(['INFO: No known function-start for ', DbgsAddr(AStartAddr),'  ARangeBefore=', dbgs(ARangeBefore)]);
      {$ENDIF}
      // no function found // check distance to previous range
      // The distance of range before has been checked by the caller
      if (ARangeBefore <> nil)
      then begin
        {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$R-}{$ENDIF} // Overflow is allowed to occur
        AStartAddr.Value := ARangeBefore.EntriesPtr[ARangeBefore.Count - 1]^.Addr;
        AStartAddr.Offset := ARangeBefore.EntriesPtr[ARangeBefore.Count - 1]^.Offset;
        AStartAddr.Validity := avFoundRange;
        {$POP}
      end
      else begin
        AStartAddr.Value := AStartAddr.GuessedValue;
        AStartAddr.Offset := -1;
        AStartAddr.Validity := avGuessed;;
      end;
    end;
  end;

  procedure AdjustLastEntryEndAddr(const ARange: TDBGDisassemblerEntryRange;
    const ADisAssList: TGDBMIDisassembleResultList);
  var
    i: Integer;
    TmpAddr: TDBGPtr;
  begin
    if ARange.Count = 0 then exit;
    TmpAddr := ARange.LastAddr;
    i := 0;
    while (i < ADisAssList.Count) and (ADisAssList.Item[i]^.Addr <= TmpAddr) do inc(i);
    if i < ADisAssList.Count
    then ARange.LastEntryEndAddr := ADisAssList.Item[i]^.Addr
    else if ARange.LastEntryEndAddr <= ARange.RangeEndAddr
    then ARange.LastEntryEndAddr := ARange.RangeEndAddr + 1;
  end;

  procedure CopyToRange(const ADisAssList: TGDBMIDisassembleResultList;
    const ADestRange: TDBGDisassemblerEntryRange; AFromIndex, ACount: Integer;
    ASrcInfoDisAssList: TGDBMIDisassembleResultList = nil);
  var
    i, j, MinInSrc, MaxInSrc: Integer;
    ItmPtr, ItmPtr2, LastItem: PDisassemblerEntry;
  begin
    if ASrcInfoDisAssList = ADisAssList
    then ASrcInfoDisAssList := nil;
    // Clean end of range
    ItmPtr := ADisAssList.Item[AFromIndex];
    i := ADestRange.Count;
    while (i > 0) and (ADestRange.EntriesPtr[i-1]^.Addr >= ItmPtr^.Addr) do dec(i);
    {$IFDEF DBG_VERBOSE}
    if ADestRange.Count <> i then
    debugln(['NOTICE, CopyToRange: Removing ',i,' entries from the end of Range. AFromIndex=',AFromIndex, ' ACount=', ACount, ' Range=',dbgs(ADestRange)]);
    {$ENDIF}
    ADestRange.Count := i;
    if  i > 0 then begin
      ItmPtr2 := ADestRange.EntriesPtr[i-1];
      if ItmPtr2^.Dump <> '' then begin
        {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$R-}{$ENDIF} // Overflow is allowed to occur
        j := (ItmPtr^.Addr - ItmPtr2^.Addr) * 2;
        {$POP}
        {$IFDEF DBG_VERBOSE}
        if length(ItmPtr2^.Dump) > j then
        debugln(['NOTICE, CopyToRange: Shortening Dump at the end of Range. AFromIndex=',AFromIndex, ' ACount=', ACount, ' Range=',dbgs(ADestRange)]);
        {$ENDIF}
        if length(ItmPtr2^.Dump) > j then ItmPtr2^.Dump := copy(ItmPtr2^.Dump, 1, j);
      end;
    end;

    if ADestRange.Count = 0
    then ADestRange.RangeStartAddr := ADisAssList.Item[AFromIndex]^.Addr;

    if ADestRange.RangeEndAddr < ADisAssList.Item[AFromIndex+ACount-1]^.Addr
    then ADestRange.RangeEndAddr := ADisAssList.Item[AFromIndex+ACount-1]^.Addr;

    if ADisAssList.Count > AFromIndex + ACount
    then begin
      if ADestRange.LastEntryEndAddr < ADisAssList.Item[AFromIndex+ACount]^.Addr
      then ADestRange.LastEntryEndAddr := ADisAssList.Item[AFromIndex+ACount]^.Addr;
    end
    else
      if ADestRange.LastEntryEndAddr <= ADestRange.RangeEndAddr
      then ADestRange.LastEntryEndAddr := ADestRange.RangeEndAddr + 1;


    // Append new items
    LastItem := nil;
    MinInSrc := 0;
    if ASrcInfoDisAssList <> nil
    then MaxInSrc := ASrcInfoDisAssList.Count - 1;
    for i := AFromIndex to AFromIndex + ACount - 1 do begin
      ItmPtr := ADisAssList.Item[i];
      ItmPtr2 := nil;
      if ASrcInfoDisAssList <> nil
      then begin
        j := MinInSrc;
        while j <= MaxInSrc do begin
          ItmPtr2 := ASrcInfoDisAssList.Item[j];
          if ItmPtr2^.Addr = itmPtr^.Addr
          then break;
          inc(j);
        end;
        if j <= MaxInSrc
        then begin
          ItmPtr2^.Dump := ItmPtr^.Dump;
          ItmPtr := ItmPtr2;
        end
        else ItmPtr2 := nil;
      end;
      if (LastItem <> nil) then begin
        // unify strings, to keep only one instance
        if (ItmPtr^.SrcFileName = LastItem^.SrcFileName)
        then ItmPtr^.SrcFileName := LastItem^.SrcFileName;
        if (ItmPtr^.FuncName = LastItem^.FuncName)
        then ItmPtr^.FuncName:= LastItem^.FuncName;
      end;
      ADestRange.Append(ItmPtr);
      // now we can move the data, pointed to by ItmPtr // reduce search range
      if ItmPtr2 <> nil
      then begin
        // j is valid
        if j = MaxInSrc
        then dec(MaxInSrc)
        else if j = MinInSrc
        then inc(MinInSrc)
        else begin
          ASrcInfoDisAssList.Item[j] := ASrcInfoDisAssList.Item[MaxInSrc];
          dec(MaxInSrc);
        end;
      end;;
      LastItem := ItmPtr;
    end;
    // Src list may be reused for other addresses, so discard used entries
    if ASrcInfoDisAssList <> nil
    then begin
      for i := 0 to Min(MinInSrc - 1, MaxInSrc - MinInSrc) do
        ASrcInfoDisAssList.Item[i] := ASrcInfoDisAssList.Item[i + MinInSrc];
      ASrcInfoDisAssList.Count := MaxInSrc + 1 - MinInSrc;
    end;
  end;

  procedure AddMemDumpToRange(const ARange: TDBGDisassemblerEntryRange;
    AMemDump: TGDBMIMemoryDumpResultList; AFirstAddr, ALastAddr: TDBGPtr);
  var
    i, Cnt, FromIndex: Integer;
    Itm, NextItm: PDisassemblerEntry;
    Addr, Offs, Len: TDBGPtr;
    s: String;
  begin
    Cnt := ARange.Count;
    if ARange.FirstAddr > AFirstAddr
    then FromIndex := -1
    else FromIndex := ARange.IndexOfAddrWithOffs(AFirstAddr)-1;
    if FromIndex < -1
    then exit;

    NextItm := ARange.EntriesPtr[FromIndex + 1];
    while NextItm <> nil do
    begin
      inc(FromIndex);
      Itm := NextItm;
      if Itm^.Addr > ALastAddr
      then break;

      if FromIndex < Cnt - 1
      then NextItm := ARange.EntriesPtr[FromIndex + 1]
      else NextItm := nil;

      if (Itm^.Dump <> '')
      then Continue;
      Itm^.Dump := ' ';

      {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$R-}{$ENDIF} // Overflow is allowed to occur
      Addr := Itm^.Addr;
      Offs := TDBGPtr(Addr - AMemDump.Addr);
      if (Offs < 0) or (Offs >= AMemDump.Count)
      then Continue;

      if (NextItm <> nil) //and (NextItm^.Addr > Addr)
      then Len := NextItm^.Addr - Addr
      else Len := AMemDump.Count - 1 - Offs;
      if Offs + Len >= AMemDump.Count
      then Len := AMemDump.Count - 1 - Offs;
      if Len = 0
      then Continue;
      if Len > 32
      then Len := 32;
      {$POP}
      s := '';
      for i := Offs to Offs + Len - 1 do
        s := s + Copy(AMemDump.ItemTxt[i],3,2);
      Itm^.Dump := s;
    end;
  end;

  (* Known issues with GDB's disassembler results:
    ** "-data-disassemble -s ### -e ### -- 1" with source
       * Result may not be sorted by addresses
       =>
       * Result may be empty, even where "-- 0" (no src info) does return data
       => Remedy: disassemble those secions without src-info
         If function-offset is available, this can be done per function
       * Result may be missing src-info, even if src-info is available for parts of the result
         This seems to be the case, if no src info is available for the start address,
         then src-info for later addresses will be ignored.
       => Remedy: if function offset is available, disassembl;e per function
       * Contains address gaps, as it does not show fillbytes, between functions
    ** "-data-disassemble -s ### -e ### -- 0" without source (probably both (with/without src)
       * "func-name" may change, while "offset" keeps increasing
         This was seen after the end of a procedure, with 0x00 bytes filling up to the next proc
       => Remedy: None, can be ignored
       * In contineous disassemble a function may not be started at offset=0.
         This seems to happen after 0x00 fill bytes.
         The func-name changes and the offset restarts at a lower value (but not 0)
       => Remedy: discard data, and re-disassemble
  *)
  // Returns   True: If some data was added
  //           False: if failed to add anything
  function DoDisassembleRange(AFirstAddr, ALastAddr: TAddress;
    StopAfterAddress: TDBGPtr; StopAfterNumLines: Integer
    ): Boolean;

    procedure AddRangetoMemDumpsNeeded(NewRange: TDBGDisassemblerEntryRange);
    var
      i: Integer;
    begin
      i := length(FMemDumpsNeeded);
      if (i > 0)
      then begin
        if  (NewRange.RangeStartAddr <= FMemDumpsNeeded[0].FirstAddr)
        and (NewRange.LastEntryEndAddr + 1 >= FMemDumpsNeeded[0].FirstAddr)
        then FMemDumpsNeeded[0].FirstAddr := NewRange.RangeStartAddr
        else
        if  (NewRange.LastEntryEndAddr + 1 >= FMemDumpsNeeded[0].LastAddr)
        and (NewRange.RangeStartAddr <= FMemDumpsNeeded[0].LastAddr)
        then FMemDumpsNeeded[0].LastAddr := NewRange.LastEntryEndAddr + 1
        else
        if  (NewRange.RangeStartAddr <= FMemDumpsNeeded[i-1].FirstAddr)
        and (NewRange.LastEntryEndAddr + 1 >= FMemDumpsNeeded[i-1].FirstAddr)
        then FMemDumpsNeeded[i-1].FirstAddr := NewRange.RangeStartAddr
        else
        if  (NewRange.LastEntryEndAddr + 1 >= FMemDumpsNeeded[i-1].LastAddr)
        and (NewRange.RangeStartAddr <= FMemDumpsNeeded[i-1].LastAddr)
        then FMemDumpsNeeded[i-1].LastAddr := NewRange.LastEntryEndAddr + 1
        else begin
          SetLength(FMemDumpsNeeded, i + 1);
          FMemDumpsNeeded[i].FirstAddr := NewRange.RangeStartAddr;
          FMemDumpsNeeded[i].LastAddr := NewRange.LastEntryEndAddr + 1;
        end;
      end
      else begin
        SetLength(FMemDumpsNeeded, i + 1);
        FMemDumpsNeeded[i].FirstAddr := NewRange.RangeStartAddr;
        FMemDumpsNeeded[i].LastAddr := NewRange.LastEntryEndAddr + 1;
      end;
    end;

    procedure DoDisassembleSourceless(ASubFirstAddr, ASubLastAddr: TDBGPtr;
      ARange: TDBGDisassemblerEntryRange; SkipFirstAddresses: Boolean = False);
    var
      DisAssList, DisAssListCurrentSub: TGDBMIDisassembleResultList;
      DisAssIterator: TGDBMIDisassembleResultFunctionIterator;
      i: Integer;
    begin
      DisAssListCurrentSub := nil;
      DisAssList := ExecDisassmble(ASubFirstAddr, ASubLastAddr, False, nil, True);
      if DisAssList.Count > 0 then begin
        i := 0;
        if SkipFirstAddresses
        then i := 1; // skip the instruction exactly at ASubFirstAddr;
        DisAssIterator := TGDBMIDisassembleResultFunctionIterator.Create
          (DisAssList, i, ASubLastAddr, FStartAddr, 0);
        ARange.Capacity := Max(ARange.Capacity, ARange.Count  + DisAssList.Count);
        // add without source
        while not DisAssIterator.EOL
        do begin
          DisAssIterator.NextSubList(DisAssListCurrentSub);
          // ignore StopAfterNumLines, until we have at least the source;

          if (not DisAssIterator.IsFirstSubList) and (DisAssListCurrentSub.Item[0]^.Offset <> 0)
          then begin
            // Current block starts with offset. Adjust and disassemble again
            {$IFDEF DBG_VERBOSE}
            debugln(['WARNING: Sublist not at offset 0 (filling gap in/before Src-Info): FromIdx=', DisAssIterator.CurrentIndex, ' NextIdx=', DisAssIterator.NextIndex,
                     ' SequenceNo=', DisAssIterator.SublistNumber, ' StartIdx=', DisAssIterator.IndexOfLocateAddress, ' StartOffs=', DisAssIterator.OffsetOfLocateAddress]);
            {$ENDIF}
            DisAssListCurrentSub := ExecDisassmble(DisAssIterator.CurrentFixedAddr(DAssMaxRangeSize),
              DisAssIterator.NextStartAddr, False, DisAssListCurrentSub, True);
          end;

          CopyToRange(DisAssListCurrentSub, ARange, 0, DisAssListCurrentSub.Count);
        end;

        FreeAndNil(DisAssIterator);
      end;
      FreeAndNil(DisAssList);
      FreeAndNil(DisAssListCurrentSub);
    end;

  var
    DisAssIterator: TGDBMIDisassembleResultFunctionIterator;
    DisAssList, DisAssListCurrentSub, DisAssListWithSrc: TGDBMIDisassembleResultList;
    i, Cnt, DisAssStartIdx: Integer;
    NewRange: TDBGDisassemblerEntryRange;
    OrigLastAddress, OrigFirstAddress: TAddress;
    TmpAddr: TDBGPtr;
    BlockOk, SkipDisAssInFirstLoop, ContinueAfterSource: Boolean;
    Itm: TDisassemblerEntry;
  begin
    Result := False;
    DisAssList := nil;
    DisAssListCurrentSub := nil;
    DisAssListWithSrc := nil;
    DisAssIterator := nil;
    OrigFirstAddress := AFirstAddr;
    OrigLastAddress := ALastAddr;
    SkipDisAssInFirstLoop := False;

    NewRange := TDBGDisassemblerEntryRange.Create;
    // set some values, wil be adjusted later (in CopyToRange
    NewRange.RangeStartAddr := AFirstAddr.Value;
    NewRange.RangeEndAddr   := ALastAddr.Value;
    NewRange.LastEntryEndAddr := ALastAddr.Value;

    // No nice startingpoint found, just start to disassemble aprox 5 instructions before it
    // and hope that when we started in the middle of an instruction it get sorted out.
    // If so, the 4st for lines from the result must be discarded
    if not (AFirstAddr.Validity in TrustedValidity)
    then PadAddress(AFirstAddr, - 5 * DAssBytesPerCommandMax);

    // Adjust ALastAddr
    if ALastAddr.Value <= AFirstAddr.Value
    then begin
      ALastAddr.Value := AFirstAddr.Value;
      PadAddress(ALastAddr, 2 * DAssBytesPerCommandMax);
    end
    else
    if not (ALastAddr.Validity in TrustedValidity)
    then PadAddress(ALastAddr, 2 * DAssBytesPerCommandMax);

    {$IFDEF DBG_VERBOSE}
    DebugLnEnter(['INFO: DoDisassembleRange for AFirstAddr =', DbgsAddr(AFirstAddr),
    ' ALastAddr=', DbgsAddr(ALastAddr), ' OrigFirst=', DbgsAddr(OrigFirstAddress), ' OrigLastAddress=', DbgsAddr(OrigLastAddress),
    '  StopAffterAddr=', StopAfterAddress, ' StopAfterLines=',  StopAfterNumLines ]);
    try
    {$ENDIF}

    // check if we have an overall source-info
    // we can only do that, if we know the offset of firstaddr (limit to DAssRangeOverFuncTreshold avg lines, should be enough)
    // TODO: limit offset ONLY, if previous range known (already have disass)
    if (AFirstAddr.Offset >= 0)
    then DisAssListWithSrc := ExecDisassmble
          (AFirstAddr.Value - Min(AFirstAddr.Offset, DAssRangeOverFuncTreshold * DAssBytesPerCommandAvg),
           ALastAddr.Value, True);

    if (DisAssListWithSrc <> nil) and (DisAssListWithSrc.Count > 0) and DisAssListWithSrc.HasSourceInfo
    then begin
      (* ***
         *** Add the full source info
         ***
      *)
      Result := True;
      DisAssListWithSrc.SortByAddress;
      if DisAssListWithSrc.Item[0]^.Addr > AFirstAddr.Value
      then begin
        // fill in gap at start
        DoDisassembleSourceless(AFirstAddr.Value, DisAssListWithSrc.Item[0]^.Addr, NewRange);
      end;

      // Find out what comes after the disassembled source (need at least one statemnet, to determine end-add of last src-stmnt)
      TmpAddr := DisAssListWithSrc.LastItem^.Addr;
      ContinueAfterSource := OrigLastAddress.Value > TmpAddr;
      if ContinueAfterSource
      then TmpAddr := ALastAddr.Value;
      DisAssList := ExecDisassmble(DisAssListWithSrc.LastItem^.Addr,
                                   TmpAddr + 2 * DAssBytesPerCommandAlign, False);

      // Add the known source list
      if DisAssList.Count < 2
      then TmpAddr := ALastAddr.Value
      else TmpAddr := DisAssList.Item[1]^.Addr;

      DisAssIterator := TGDBMIDisassembleResultFunctionIterator.Create
        (DisAssListWithSrc, 0, TmpAddr , FStartAddr, StopAfterAddress);
      NewRange.Capacity := Max(NewRange.Capacity, NewRange.Count  + DisAssListWithSrc.Count);
      while not DisAssIterator.EOL
      do begin
        if (dcsCanceled in SeenStates) then break;
        DisAssIterator.NextSubList(DisAssListCurrentSub);
        CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count); // Do not add the Sourcelist as last param, or it will get re-sorted

        // check for gap
        if DisAssListCurrentSub.LastItem^.Addr < DisAssIterator.NextStartAddr - DAssBytesPerCommandAlign
        then begin
          {$IFDEF DBG_VERBOSE}
          debugln(['Info: Filling GAP in the middle of Source: Src-FromIdx=', DisAssIterator.CurrentIndex, ' Src-NextIdx=', DisAssIterator.NextIndex,
                   ' Src-SequenceNo=', DisAssIterator.SublistNumber, '  Last Address in Src-Block=', DisAssListCurrentSub.LastItem^.Addr ]);
          {$ENDIF}
          DoDisassembleSourceless(DisAssListCurrentSub.LastItem^.Addr, DisAssIterator.NextStartAddr, NewRange, True);
        end;
      end;
      i := DisAssIterator.CountLinesAfterCounterAddr;

      FreeAndNil(DisAssIterator);
      FreeAndNil(DisAssListWithSrc);
      FreeAndNil(DisAssListCurrentSub);
      // Source Completly Added

      if not ContinueAfterSource
      then begin
        AdjustLastEntryEndAddr(NewRange, DisAssList);
        AddRangetoMemDumpsNeeded(NewRange);
        FKnownRanges.AddRange(NewRange);  // NewRange is now owned by FKnownRanges
        NewRange := nil;
        FreeAndNil(DisAssList);
        exit;
      end;

      // continue with the DisAsslist for the remainder
      AFirstAddr.Validity := avFoundFunction; //  if we got source, then start is ok (original start is kept)
      DisAssStartIdx := 1;
      SkipDisAssInFirstLoop := True;
      if i > 0
      then StopAfterNumLines := StopAfterNumLines - i;
      (* ***
         *** Finished adding the full source info
         ***
      *)
    end
    else begin
      (* ***
         *** Full Source was not available
         ***
      *)
      if (DisAssListWithSrc <> nil) and (DisAssListWithSrc.Count > 0)
      then begin
        DisAssList := DisAssListWithSrc; // got data already
        DisAssListWithSrc := nil;
      end
      else begin
        DisAssList := ExecDisassmble(AFirstAddr.Value, ALastAddr.Value, False);
      end;

      if DisAssList.Count < 2
      then begin
        debugln('Error failed to get enough data for dsassemble');
        // create a dummy range, so we will not retry
        NewRange.Capacity := 1;
        NewRange.RangeStartAddr   := AFirstAddr.Value;
        if OrigLastAddress.Value > AFirstAddr.Value+1
        then NewRange.RangeEndAddr     := OrigLastAddress.Value
        else NewRange.RangeEndAddr     := AFirstAddr.Value+1;
        NewRange.LastEntryEndAddr := AFirstAddr.Value+1;
        Itm.Addr := AFirstAddr.Value;
        Itm.Dump := ' ';
        Itm.SrcFileLine := 0;
        Itm.Offset := 0;
        itm.Statement := '<error>';
        NewRange.Append(@Itm);
        FKnownRanges.AddRange(NewRange);  // NewRange is now owned by FKnownRanges
        NewRange := nil;
        FreeAndNil(DisAssList);
        exit;
      end;

      DisAssStartIdx := 0;
    end;

    // we may have gotten more lines than ask, and the last line we don't know the length
    Cnt := DisAssList.Count;
    if (ALastAddr.Validity = avPadded) or (DisAssList.LastItem^.Addr >= ALastAddr.Value)
    then begin
      ALastAddr.Value := DisAssList.LastItem^.Addr;
      ALastAddr.Validity := avFoundStatement;
      dec(Cnt);
      DisAssList.Count := Cnt;
    end;
    // ALastAddr.Value is now the address after the last statement;

    if (AFirstAddr.Validity = avPadded) // always False, if we had source-info
    then begin
      // drop up to 4 entries, if possible
      while (DisAssStartIdx < 4) and (DisAssStartIdx + 1 < Cnt) and (DisAssList.Item[DisAssStartIdx+1]^.Addr <= OrigFirstAddress.Value)
      do inc(DisAssStartIdx);
      AFirstAddr.Value := DisAssList.Item[DisAssStartIdx]^.Addr;
      AFirstAddr.Validity := avFoundStatement;
    end;


    NewRange.Capacity := Max(NewRange.Capacity, NewRange.Count  + Cnt);

    DisAssIterator := TGDBMIDisassembleResultFunctionIterator.Create
      (DisAssList, DisAssStartIdx, ALastAddr.Value, FStartAddr, StopAfterAddress);

    while not DisAssIterator.EOL
    do begin
      if (dcsCanceled in SeenStates) then break;
      BlockOk := DisAssIterator.NextSubList(DisAssListCurrentSub);

      // Do we have enough lines (without the current block)?
      if (DisAssIterator.CountLinesAfterCounterAddr > StopAfterNumLines)
      then begin
        {$IFDEF DBG_VERBOSE}
        DebugLn(['INFO: Got enough line in Iteration: CurrentIndex=', DisAssIterator.CurrentIndex]);
        {$ENDIF}
        NewRange.LastEntryEndAddr := DisAssIterator.NextStartAddr;
        //AdjustLastEntryEndAddr(NewRange, DisAssList);
        break;
      end;

      if (not DisAssIterator.IsFirstSubList) and (DisAssListCurrentSub.Item[0]^.Offset <> 0)
      then begin
        // Got List with Offset at start
        {$IFDEF DBG_VERBOSE}
        debugln(['WARNING: Sublist not at offset 0 (offs=',DisAssListCurrentSub.Item[0]^.Offset,'): FromIdx=', DisAssIterator.CurrentIndex, ' NextIdx=', DisAssIterator.NextIndex,
                 ' SequenceNo=', DisAssIterator.SublistNumber, ' StartIdx=', DisAssIterator.IndexOfLocateAddress, ' StartOffs=', DisAssIterator.OffsetOfLocateAddress]);
        {$ENDIF}
        // Current block starts with offset. Adjust and disassemble again
        // Try with source first, in case it returns dat without source
        DisAssListWithSrc := ExecDisassmble(DisAssIterator.CurrentFixedAddr(DAssMaxRangeSize),
          DisAssIterator.NextStartAddr, True, DisAssListWithSrc, True);
        if (DisAssListWithSrc.Count > 0)
        then begin
          if DisAssListWithSrc.HasSourceInfo
          then DisAssListWithSrc.SortByAddress;
          if (not DisAssListWithSrc.HasSourceInfo)
          or (DisAssListWithSrc.LastItem^.Addr > DisAssIterator.NextStartAddr - DAssBytesPerCommandAlign)
          then begin
            // no source avail, but got data
            // OR source and no gap
            CopyToRange(DisAssListWithSrc, NewRange, 0, DisAssListWithSrc.Count);
            Result := True;
            continue;
          end;
        end;

        //get the source-less code as reference
        DisAssListCurrentSub := ExecDisassmble(DisAssIterator.CurrentFixedAddr(DAssMaxRangeSize),
          DisAssIterator.NextStartAddr, False, DisAssListCurrentSub, True);
        CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count, DisAssListWithSrc);
        Result := Result or (DisAssListCurrentSub.Count > 0);
        continue;
      end;

      // Todo: Check for wrong start stmnt offset
      if BlockOk
      then begin
        // Got a good block
        if (DisAssListCurrentSub.Item[0]^.FuncName <> '')
        then begin
          // Try to get source-info (up to DisAssIterator.NextStartAddr)
          // Subtract offset from StartAddress, in case this is the first block
          //   (we may continue existing data, but src info must be retrieved in full, or may be incomplete)
          if  not( DisAssIterator.IsFirstSubList and SkipDisAssInFirstLoop )
          then begin
            DisAssListWithSrc := ExecDisassmble(DisAssIterator.CurrentFixedAddr(DAssMaxRangeSize),
                DisAssIterator.NextStartAddr, True, DisAssListWithSrc, True);
            // We may have less lines with source, as we stripped padding at the end
            if (DisAssListWithSrc <> nil) and DisAssListWithSrc.HasSourceInfo
            then begin
              CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count, DisAssListWithSrc);
              Result := Result or (DisAssListCurrentSub.Count > 0);
              continue;
            end;
          end;
        end;
        CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count);
        Result := Result or (DisAssListCurrentSub.Count > 0);
        continue;
      end;

      // Got a problematic block
      {$IFDEF DBG_VERBOSE}
      debugln(['WARNING: FindProcEnd reported an issue FromIdx=', DisAssIterator.CurrentIndex,' NextIdx=',
      DisAssIterator.NextIndex, ' StartIdx=', DisAssIterator.IndexOfLocateAddress, ' StartOffs=', DisAssIterator.OffsetOfLocateAddress]);
      {$ENDIF}
      //if DisAssIterator.IsFirstSubList and (not(AFirstAddr.Validity in TrustedValidity))
      //and (DisAssIterator.IndexOfLocateAddress >= DisAssIterator.CurrentIndex) // in current list
      //and (DisAssIterator.OffsetOfLocateAddress <> 0)
      //then begin
      //  // FStartAddr is in the middle of a statement. Maybe move the Range?
      //end;

      CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count);
      Result := Result or (DisAssListCurrentSub.Count > 0);
    end;

    if NewRange.LastEntryEndAddr > NewRange.RangeEndAddr
    then NewRange.RangeEndAddr := NewRange.LastEntryEndAddr;

    AddRangetoMemDumpsNeeded(NewRange);
    FKnownRanges.AddRange(NewRange);  // NewRange is now owned by FKnownRanges
    NewRange := nil;

    FreeAndNil(DisAssIterator);
    FreeAndNil(DisAssList);
    FreeAndNil(DisAssListCurrentSub);
    FreeAndNil(DisAssListWithSrc);
    {$IFDEF DBG_VERBOSE}
    finally
      DebugLnExit(['INFO: DoDisassembleRange finished' ]);
    end;
    {$ENDIF}
  end;

  procedure AddMemDumps;
  var
    i: Integer;
    MemDump: TGDBMIMemoryDumpResultList;
    Rng: TDBGDisassemblerEntryRange;
    FirstAddr: TDBGPtr;
  begin
    MemDump := nil;
    for i := 0 to length(FMemDumpsNeeded) - 1 do
    begin
      if (dcsCanceled in SeenStates) then break;
      FirstAddr := FMemDumpsNeeded[i].FirstAddr;
      Rng := FRangeIterator.GetRangeForAddr(FirstAddr, True);
      if rng <> nil
      then MemDump := ExecMemDump(FirstAddr, FMemDumpsNeeded[i].LastAddr - FirstAddr, MemDump);
      if DebuggerState <> dsError
      then begin
        while (Rng <> nil) and (Rng.FirstAddr <= FMemDumpsNeeded[i].LastAddr) do
        begin
          AddMemDumpToRange(Rng, MemDump, FMemDumpsNeeded[i].FirstAddr, FMemDumpsNeeded[i].LastAddr);
          Rng := FRangeIterator.NextRange;
        end;
      end;
    end;
    FreeAndNil(MemDump);
  end;

var
  TryStartAt, TryEndAt: TAddress;
  TmpAddr: TDBGPtr;
  GotCnt, LastGotCnt: Integer;
  RngBefore, RngAfter: TDBGDisassemblerEntryRange;
begin
  Result := True;

  if FEndAddr < FStartAddr
  then FEndAddr := FStartAddr;

  (* Try to find the boundaries for the unknown range containing FStartAddr
     If FStartAddr already has known disassembler data, then this will return
     the boundaries of the 1ast unknown section after FStartAddr
  *)
  // Guess the maximum Addr-Range which needs to be disassembled
  TryStartAt := InitAddress(FStartAddr, avExternRequest, -1);
  // Find the begin of the function at TryStartAt
  // or the rng before (if not to far back)

  RngBefore := FRangeIterator.GetRangeForAddr(FStartAddr, True);
  {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$R-}{$ENDIF} // Overflow is allowed to occur
  if (RngBefore <> nil)
  and (TryStartAt.Value > RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr)
  and (TryStartAt.Value - RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr > FLinesBefore * DAssBytesPerCommandAvg)
  then RngBefore := nil;
  {$POP}
  TmpAddr := FStartAddr - Min(FLinesBefore * DAssBytesPerCommandAvg, DAssMaxRangeSize);
  TryStartAt.GuessedValue := TmpAddr;
  AdjustToRangeOrKnowFunctionStart(TryStartAt, RngBefore);
  // check max size
  if (TryStartAt.Value < FStartAddr - Min(FStartAddr, DAssMaxRangeSize))
  then begin
    {$IFDEF DBG_VERBOSE}
    DebugLn(['INFO: Limit Range for Disass: FStartAddr=', FStartAddr, '  TryStartAt.Value=', TryStartAt.Value  ]);
    {$ENDIF}
    TryStartAt := InitAddress(TmpAddr, avGuessed);
  end;

  // Guess Maximum, will adjust later
  if TryStartAt.Value > FEndAddr then begin
    if (RngBefore <> nil) then begin
      GotCnt := RngBefore.IndexOfAddr(FEndAddr);
      LastGotCnt := RngBefore.IndexOfAddr(TryStartAt.Value);
      if (GotCnt >= 0) and (LastGotCnt >= 0) and (LastGotCnt > GotCnt) then
        FLinesAfter := Max(FLinesAfter - (LastGotCnt - GotCnt), 1);
    end;
    FEndAddr := TryStartAt.Value; // WARNING: modifying FEndAddr
  end;

  TryEndAt := InitAddress(FEndAddr + FLinesAfter * DAssBytesPerCommandAvg, avGuessed);

  // Read as many unknown ranges, until LinesAfter is met
  GotCnt := -1;
  while(True)
  do begin
    // check if we need any LinesAfter
    if (dcsCanceled in SeenStates) then break;
    LastGotCnt:= GotCnt;
    GotCnt := 0;
    TmpAddr := FEndAddr;
    if TryStartAt.Value > FEndAddr
    then
      TmpAddr := TryStartAt.Value;
    if RngBefore <> nil
    then begin
      TmpAddr := RngBefore.RangeEndAddr;
      if RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr > TmpAddr
      then TmpAddr := RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr;
      GotCnt := RngBefore.IndexOfAddrWithOffs(FEndAddr);
      if GotCnt >= 0 then begin
        GotCnt := RngBefore.Count - 1 - GotCnt;  // the amount of LinesAfter, that are already known
        if (GotCnt >= FLinesAfter)
        then break;
        // adjust end address
        TryEndAt := InitAddress(RngBefore.RangeEndAddr + (FLinesAfter-GotCnt) * DAssBytesPerCommandAvg, avGuessed);
      end
      else GotCnt := 0;
    end;
    if LastGotCnt >= GotCnt
    then begin
      debugln(['Disassembler: *** Failure to get any more lines while scanning forward LastGotCnt=',LastGotCnt, ' now GotCnt=',GotCnt, ' Requested=',FLinesAfter]);
      break;
    end;

    if (dcsCanceled in SeenStates) then break;
    RngAfter := FRangeIterator.NextRange;
    // adjust TryEndAt
    if (RngAfter <> nil) and (TryEndAt.Value >= RngAfter.RangeStartAddr)
    then begin
      TryEndAt.Value := RngAfter.RangeStartAddr;
      TryEndAt.Validity := avFoundRange;
    end;

    if (dcsCanceled in SeenStates) then break;
    // Try to disassemble the range
    if not DoDisassembleRange(TryStartAt, TryEndAt, TmpAddr, FLinesAfter-GotCnt)
    then begin
      // disassemble failed
      debugln(['ERROR: Failed to disassemble from ', DbgsAddr(TryStartAt),' to ', DbgsAddr(TryEndAt)]);
      break;
    end;

    // prepare the next range
    RngBefore := FRangeIterator.GetRangeForAddr(FStartAddr, False);
    if (RngBefore = nil)
    then begin
      debugln(['INTERNAL ERROR: (linesafter) Missing the data, that was just  disassembled: from ', DbgsAddr(TryStartAt),' to ', DbgsAddr(TryEndAt)]);
      break;
    end;

    TryStartAt.Value := RngBefore.RangeEndAddr;
    TryStartAt.Validity := avFoundRange;
    TryEndAt := InitAddress(FEndAddr + FLinesAfter * DAssBytesPerCommandAvg, avGuessed);
  end;

  // Find LinesBefore
  RngAfter := FRangeIterator.GetRangeForAddr(FStartAddr, False);
  GotCnt := -1;
  while(True)
  do begin
    if (dcsCanceled in SeenStates) then break;
    LastGotCnt:= GotCnt;
    if (RngAfter = nil)
    then begin
      debugln(['INTERNAL ERROR: (linesbefore) Missing the data, that was disassembled: from ', DbgsAddr(TryStartAt),' to ', DbgsAddr(TryEndAt)]);
      break;
    end;

    GotCnt := RngAfter.IndexOfAddrWithOffs(FStartAddr);  // already known before
    if GotCnt >= FLinesBefore
    then break;
    if LastGotCnt >= GotCnt
    then begin
      debugln(['Disassembler: *** Failure to get any more lines while scanning backward LastGotCnt=',LastGotCnt, ' now GotCnt=',GotCnt, ' Requested=',FLinesBefore]);
      break;
    end;

    TryEndAt := InitAddress(RngAfter.RangeStartAddr, avFoundRange);
    TmpAddr := TryEndAt.Value - Min((FLinesBefore - GotCnt) * DAssBytesPerCommandAvg, DAssMaxRangeSize);
    TryStartAt := InitAddress(TryEndAt.Value - 1, avGuessed);
    TryStartAt.GuessedValue := TmpAddr;
    // and adjust
    RngBefore := FRangeIterator.PreviousRange;
    {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$R-}{$ENDIF} // Overflow is allowed to occur
    if (RngBefore <> nil)
    and (TryStartAt.Value > RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr)
    and (TryStartAt.Value - RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr > (FLinesBefore - GotCnt) * DAssBytesPerCommandAvg)
    then RngBefore := nil;
    {$POP}
    AdjustToRangeOrKnowFunctionStart(TryStartAt, RngBefore);
    if (TryStartAt.Value < TryEndAt.Value - Min(TryEndAt.Value, DAssMaxRangeSize))
    then begin
      {$IFDEF DBG_VERBOSE}
      DebugLn(['INFO: Limit Range for Disass: TryEndAt.Value=', TryEndAt.Value, '  TryStartAt.Value=', TryStartAt.Value  ]);
      {$ENDIF}
      TryStartAt := InitAddress(TmpAddr, avGuessed);
    end;

    if (dcsCanceled in SeenStates) then break;
    // Try to disassemble the range
    if not DoDisassembleRange(TryStartAt, TryEndAt, 0, -1)
    then begin
      // disassemble failed
      debugln(['ERROR: Failed to disassemble from ', DbgsAddr(TryStartAt),' to ', DbgsAddr(TryEndAt)]);
      break;
    end;

    RngAfter := FRangeIterator.GetRangeForAddr(FStartAddr, False);
  end;

  DoProgress;
  AddMemDumps;
  DoProgress;
end;

constructor TGDBMIDebuggerCommandDisassembe.Create(AOwner: TGDBMIDebugger;
  AKnownRanges: TDBGDisassemblerEntryMap; AStartAddr, AEndAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer);
begin
  inherited Create(AOwner);
  FKnownRanges := AKnownRanges;
  FRangeIterator:= TDBGDisassemblerEntryMapIterator.Create(FKnownRanges);
  FStartAddr := AStartAddr;
  FEndAddr := AEndAddr;
  FLinesBefore := ALinesBefore;
  FLinesAfter := ALinesAfter;
end;

destructor TGDBMIDebuggerCommandDisassembe.Destroy;
begin
  FreeAndNil(FRangeIterator);
  inherited Destroy;
end;

function TGDBMIDebuggerCommandDisassembe.DebugText: String;
begin
  Result := Format('%s: FromAddr=%u ToAddr=%u LinesBefore=%d LinesAfter=%d',
                   [ClassName, FStartAddr, FEndAddr, FLinesBefore, FLinesAfter]);
end;

{ TGDBMIDebuggerCommandStartDebugging }

function TGDBMIDebuggerCommandStartDebugging.DoExecute: Boolean;

  function CheckFunction(const AFunction: String): Boolean;
  var
    R: TGDBMIExecResult;
    idx: Integer;
  begin
    ExecuteCommand('info functions %s', [AFunction], R, [cfCheckState]);
    idx := Pos(AFunction, R.Values);
    if idx <> 0
    then begin
      // Strip first
      Delete(R.Values, 1, idx + Length(AFunction) - 1);
      idx := Pos(AFunction, R.Values);
    end;
    Result := idx <> 0;
  end;

  procedure RetrieveRegcall;
  var
    R: TGDBMIExecResult;
  begin
    // Assume it is
    Include(TargetInfo^.TargetFlags, tfRTLUsesRegCall);

    ExecuteCommand('-data-evaluate-expression FPC_THREADVAR_RELOCATE_PROC', R);
    if R.State <> dsError then Exit; // guessed right

    // next attempt, posibly no symbols, try functions
    if CheckFunction('FPC_CPUINIT') then Exit; // function present --> not 1.0

    // this runerror is only defined for < 1.1 ?
    if not CheckFunction('$$_RUNERROR$') then Exit;

    // We are here in 2 cases
    // 1) there are no symbols at all
    //    We do not have to know the calling convention
    // 2) target is compiled with an earlier version than 1.9.2
    //    params are passes by stack
    Exclude(TargetInfo^.TargetFlags, tfRTLUsesRegCall);
  end;

  procedure SetTargetInfo(const AFileType: String);
  var
    FoundPtrSize, UseWin64ABI: Boolean;
  begin
    UseWin64ABI := False;
    // assume some defaults
    TargetInfo^.TargetPtrSize := GetIntValue('sizeof(%s)', [PointerTypeCast]);
    FoundPtrSize := (FLastExecResult.State <> dsError) and (TargetInfo^.TargetPtrSize > 0);
    if not FoundPtrSize
    then TargetInfo^.TargetPtrSize := 4;
    TargetInfo^.TargetIsBE := False;

    case StringCase(AFileType, [
      'efi-app-ia32', 'elf32-i386', 'pei-i386', 'elf32-i386-freebsd',
      'elf64-x86-64', 'pei-x86-64',
      'mach-o-be',
      'mach-o-le',
      'pei-arm-little',
      'pei-arm-big'
    ], True, False) of
      0..3: TargetInfo^.TargetCPU := 'x86';
      4: TargetInfo^.TargetCPU := 'x86_64'; //TODO: should we check, PtrSize must be 8, but what if not?
      5: begin
        TargetInfo^.TargetCPU := 'x86_64'; //TODO: should we check, PtrSize must be 8, but what if not?
        UseWin64ABI := True;
      end;
      6: begin
         //mach-o-be
        TargetInfo^.TargetIsBE := True;
        if FTheDebugger.FGDBCPU <> ''
        then TargetInfo^.TargetCPU := FTheDebugger.FGDBCPU
        else TargetInfo^.TargetCPU := 'powerpc'; // guess
      end;
      7: begin
        //mach-o-le
        if FoundPtrSize then begin
          if FTheDebugger.FGDBPtrSize = TargetInfo^.TargetPtrSize
          then TargetInfo^.TargetCPU := FTheDebugger.FGDBCPU
          else // guess
            case TargetInfo^.TargetPtrSize of
              4: TargetInfo^.TargetCPU := 'x86'; // guess
              8: TargetInfo^.TargetCPU := 'x86_64'; // guess
              else TargetInfo^.TargetCPU := 'x86'; // guess
            end
        end
        else begin
          if FTheDebugger.FGDBCPU <> ''
          then TargetInfo^.TargetCPU := FTheDebugger.FGDBCPU
          else TargetInfo^.TargetCPU := 'x86'; // guess
        end;
      end;
      8: begin
        TargetInfo^.TargetCPU := 'arm';
      end;
      9: begin
        TargetInfo^.TargetIsBE := True;
        TargetInfo^.TargetCPU := 'arm';
      end;
    else
      // Unknown filetype, use GDB cpu
      DebugLn('[WARNING] [Debugger.TargetInfo] Unknown FileType: %s, using GDB cpu', [AFileType]);

      TargetInfo^.TargetCPU := FTheDebugger.FGDBCPU;
      // Todo: check PtrSize and downgrade 64 bit cpu to 32 bit cpu, if required
    end;

    if not FoundPtrSize
    then TargetInfo^.TargetPtrSize := CpuNameToPtrSize(TargetInfo^.TargetCPU);

    case StringCase(TargetInfo^.TargetCPU, [
      'x86', 'i386', 'i486', 'i586', 'i686',
      'ia64', 'x86_64', 'powerpc',
      'sparc', 'arm'
    ], True, False) of
      0..4: begin // x86
        TargetInfo^.TargetRegisters[0] := '$eax';
        TargetInfo^.TargetRegisters[1] := '$edx';
        TargetInfo^.TargetRegisters[2] := '$ecx';
      end;
      5, 6: begin // ia64, x86_64
        if TargetInfo^.TargetPtrSize = 4
        then begin
          TargetInfo^.TargetRegisters[0] := '$eax';
          TargetInfo^.TargetRegisters[1] := '$edx';
          TargetInfo^.TargetRegisters[2] := '$ecx';
        end
        else if UseWin64ABI
        then begin
          TargetInfo^.TargetRegisters[0] := '$rcx';
          TargetInfo^.TargetRegisters[1] := '$rdx';
          TargetInfo^.TargetRegisters[2] := '$r8';
        end else
        begin
          TargetInfo^.TargetRegisters[0] := '$rdi';
          TargetInfo^.TargetRegisters[1] := '$rsi';
          TargetInfo^.TargetRegisters[2] := '$rdx';
        end;
      end;
      7: begin // powerpc
        TargetInfo^.TargetIsBE := True;
        // alltough darwin can start with r2, it seems that all OS start with r3
//        if UpperCase(FTargetInfo.TargetOS) = 'DARWIN'
//        then begin
//          FTargetInfo.TargetRegisters[0] := '$r2';
//          FTargetInfo.TargetRegisters[1] := '$r3';
//          FTargetInfo.TargetRegisters[2] := '$r4';
//        end
//        else begin
          TargetInfo^.TargetRegisters[0] := '$r3';
          TargetInfo^.TargetRegisters[1] := '$r4';
          TargetInfo^.TargetRegisters[2] := '$r5';
//        end;
      end;
      8: begin // sparc
        TargetInfo^.TargetIsBE := True;
        TargetInfo^.TargetRegisters[0] := '$g1';
        TargetInfo^.TargetRegisters[1] := '$o0';
        TargetInfo^.TargetRegisters[2] := '$o1';
      end;
      9: begin // arm
        TargetInfo^.TargetRegisters[0] := '$r0';
        TargetInfo^.TargetRegisters[1] := '$r1';
        TargetInfo^.TargetRegisters[2] := '$r2';
      end;
    else
      TargetInfo^.TargetRegisters[0] := '';
      TargetInfo^.TargetRegisters[1] := '';
      TargetInfo^.TargetRegisters[2] := '';
      DebugLn('[WARNING] [Debugger] Unknown target CPU: ', TargetInfo^.TargetCPU);
    end;

  end;

  {$IF defined(UNIX) or defined(DBG_ENABLE_TERMINAL)}
  procedure InitConsole;
  var
    R: TGDBMIExecResult;
    s: String;
    h: THandle;
    isConsole: Boolean;
  begin
      isConsole := False;
      // Make sure consule output will ot be mixed with gbd output
      {$IFDEF DBG_ENABLE_TERMINAL}
        {$IFDEF UNIX}
          (* DBG_ENABLE_TERMINAL and UNIX *)
          s := DebuggerProperties.ConsoleTty;
          if s = '' then begin
            FTheDebugger.FPseudoTerminal.Open;
            s := FTheDebugger.FPseudoTerminal.Devicename;
            isConsole := True;
          end;
        {$ELSE}
          (* only DBG_ENABLE_TERMINAL *)
          FTheDebugger.FPseudoTerminal.Open;
          s := FTheDebugger.FPseudoTerminal.Devicename;
          isConsole := True;
        {$ENDIF}
      {$ELSE}
          (* only UNIX *)
          s := DebuggerProperties.ConsoleTty;
          if s = '' then s := '/dev/null';
      {$ENDIF}

      if not isConsole then begin
        h := fileopen(S, fmOpenWrite);
        isConsole := IsATTY(h) = 1;
        FileClose(h);
      end;

      if isConsole then
        isConsole := ExecuteCommand('set inferior-tty %s', [s], R) and (r.State <> dsError);
      if not isConsole then
        ExecuteCommand('set inferior-tty /dev/null', []);
  end;
  {$ENDIF}

var
  R: TGDBMIExecResult;
  FileType, EntryPoint: String;
  List: TGDBMINameValueList;
  TargetPIDPart: String;
  CanContinue, HadTimeout: Boolean;
  CommandObj: TGDBMIDebuggerCommandExecute;
begin
  Result := True;
  FSuccess := False;

  try
    if not (DebuggerState in [dsStop])
    then begin
      Result := True;
      Exit;
    end;

    DebugLn(['TGDBMIDebugger.StartDebugging WorkingDir="', FTheDebugger.WorkingDir,'"']);
    if FTheDebugger.WorkingDir <> ''
    then begin
      // to workaround a possible bug in gdb, first set the workingdir to .
      // otherwise on second run within the same gdb session the workingdir
      // is set to c:\windows
      ExecuteCommand('-environment-cd %s', ['.'], []);
      ExecuteCommand('-environment-cd %s', [FTheDebugger.ConvertToGDBPath(UTF8ToSys(FTheDebugger.WorkingDir), cgptCurDir)], [cfCheckError]);
    end;

    TargetInfo^.TargetFlags := [tfHasSymbols]; // Set until proven otherwise

    // check if the exe is compiled with FPC >= 1.9.2
    // then the rtl is compiled with regcalls
    RetrieveRegCall;

    // also call execute -exec-arguments if there are no arguments in this run
    // so the possible arguments of a previous run are cleared
    ExecuteCommand('-exec-arguments %s', [FTheDebugger.Arguments], [cfCheckState]);

    {$IF defined(UNIX) or defined(DBG_ENABLE_TERMINAL)}
    InitConsole;
    {$ENDIF}

    ExecuteCommand('-gdb-set language pascal', [cfCheckError]);

    // collect timeouts
    HadTimeout := False;
    // check whether we need class cast dereference
    R := CheckHasType('TObject', tfFlagHasTypeObject);
    HadTimeout := HadTimeout and LastExecwasTimeOut;
    if R.State <> dsError
    then begin
      if UpperCase(LeftStr(R.Values, 15)) = UpperCase('type = ^TOBJECT')
      then include(TargetInfo^.TargetFlags, tfClassIsPointer);
    end;
    R := CheckHasType('Exception', tfFlagHasTypeException);
    HadTimeout := HadTimeout and LastExecwasTimeOut;
    if R.State <> dsError
    then begin
      if UpperCase(LeftStr(R.Values, 17)) = UpperCase('type = ^EXCEPTION')
      then include(TargetInfo^.TargetFlags, tfExceptionIsPointer);
    end;
    CheckHasType('Shortstring', tfFlagHasTypeShortstring);
    HadTimeout := HadTimeout and LastExecwasTimeOut;
    //CheckHasType('PShortstring', tfFlagHasTypePShortString);
    //HadTimeout := HadTimeout and LastExecwasTimeOut;
    CheckHasType('pointer', tfFlagHasTypePointer);
    HadTimeout := HadTimeout and LastExecwasTimeOut;
    CheckHasType('byte', tfFlagHasTypeByte);
    HadTimeout := HadTimeout and LastExecwasTimeOut;
    //CheckHasType('char', tfFlagHasTypeChar);
    //HadTimeout := HadTimeout and LastExecwasTimeOut;

    if HadTimeout then DoTimeoutFeedback;

    TargetInfo^.TargetCPU := '';
    TargetInfo^.TargetOS := FTheDebugger.FGDBOS; // try to detect ??

    // try to retrieve the filetype and program entry point
    FileType := '';
    EntryPoint := '';
    if ExecuteCommand('info file', R)
    then begin
      if rfNoMI in R.Flags
      then begin
        FileType := GetPart('file type ', '.', R.Values);
        EntryPoint := GetPart(['Entry point: '], [#10, #13, '\t'], R.Values);
      end
      else begin
        // OS X gdb has mi output here
        List := TGDBMINameValueList.Create(R, ['section-info']);
        FileType := List.Values['filetype'];
        EntryPoint := List.Values['entry-point'];
        List.Free;
      end;
      DebugLn('[Debugger] File type: ', FileType);
      DebugLn('[Debugger] Entry point: ', EntryPoint);
    end;

    SetTargetInfo(FileType);

    (* Set breakpoints *)

    if not (dfForceBreakDetected in FTheDebugger.FDebuggerFlags) then begin
      // detect if we can insert a not yet known break
      ExecuteCommand('-break-insert -f foo', R);
      if R.State <> dsError
      then begin
        Include(FTheDebugger.FDebuggerFlags, dfForceBreak);
        List := TGDBMINameValueList.Create(R, ['bkpt']);
        ExecuteCommand('-break-delete ' + List.Values['number']);
        List.Free;
      end
      else Exclude(FTheDebugger.FDebuggerFlags, dfForceBreak);
      Include(FTheDebugger.FDebuggerFlags, dfForceBreakDetected);
    end;

    // try Insert Break breakpoint
    // we might have rtl symbols
    if tfHasSymbols in TargetInfo^.TargetFlags
    then begin
      FTheDebugger.FMainAddrBreak.SetBoth(Self);
    end
    else begin
      DebugLn('TGDBMIDebugger.StartDebugging Note: Target has no symbols');
      FTheDebugger.FMainAddrBreak.Clear(Self);
    end;

    if not FTheDebugger.FMainAddrBreak.Enabled and (EntryPoint <> '')
    then begin
      // We could not set our initial break to get info and allow stepping
      // Try it with the program entry point
      FTheDebugger.FMainAddrBreak.SetAtCustomAddr(Self, StrToQWordDef(EntryPoint, 0));
    end;

    (* there is no handling for errors, before reaching entry point, so we do not need them yet *)
    //FTheDebugger.FExceptionBreak.SetBoth(Self);
    //FTheDebugger.FBreakErrorBreak.SetBoth(Self);
    //FTheDebugger.FRunErrorBreak.SetBoth(Self);

    TargetInfo^.TargetPID := 0;

    // fire the first step
    if FTheDebugger.FMainAddrBreak.Enabled
    then begin
      CommandObj := TGDBMIDebuggerCommandExecute.Create(FTheDebugger, ectRun);
      CommandObj.Execute;
      // some versions of gdb (OSX) output the PID here
      R := CommandObj.Result;
      TargetPIDPart := GetPart(['process '], [' local', ']'], R.Values, True);
      TargetInfo^.TargetPID := StrToIntDef(TargetPIDPart, 0);
      R.State := dsNone;
      CommandObj.DoFinished;
    end;

    FTheDebugger.FMainAddrBreak.Clear(Self);

    // try to find PID (if not already found)
    if (TargetInfo^.TargetPID = 0)
    and ExecuteCommand('info program', [], R, [cfCheckState])
    then begin
      TargetPIDPart := GetPart(['child process ', 'child thread ', 'lwp '],
                               [' ', '.', ')'], R.Values, True);
      TargetInfo^.TargetPID := StrToIntDef(TargetPIDPart, 0);
    end;

    // apple
    if (TargetInfo^.TargetPID = 0)
    and ExecuteCommand('info pid', [], R, [cfCheckState])
    and (R.State <> dsError)
    then begin
      List := TGDBMINameValueList.Create(R);
      TargetInfo^.TargetPID := StrToIntDef(List.Values['process-id'], 0);
      List.Free;
    end;

    // apple / MacPort 7.1 / 32 bit dwarf
    if (TargetInfo^.TargetPID = 0)
    and ExecuteCommand('info threads', [], R, [cfCheckState])
    and (R.State <> dsError)
    then begin
      TargetPIDPart := GetPart(['of process '], [' '], R.Values, True);
      TargetInfo^.TargetPID := StrToIntDef(TargetPIDPart, 0);
    end;

    if TargetInfo^.TargetPID = 0
    then begin
      Result := False;
      FSuccess := False;
      SetDebuggerState(dsError);
      Exit;
    end;

    DebugLn('[Debugger] Target PID: %u', [TargetInfo^.TargetPID]);

    // they may still exist from prev run, addr will be checked
    FTheDebugger.FExceptionBreak.SetAddr(Self);
    FTheDebugger.FBreakErrorBreak.SetAddr(Self);
    FTheDebugger.FRunErrorBreak.SetAddr(Self);

    if R.State = dsNone
    then begin
      SetDebuggerState(dsInit); // triggers all breakpoints to be set.
      if FTheDebugger.FBreakAtMain <> nil
      then begin
        CanContinue := False;
        TGDBMIBreakPoint(FTheDebugger.FBreakAtMain).Hit(CanContinue);
      end
      else CanContinue := True;

      if CanContinue and (FContinueCommand <> nil)
      then begin
        FTheDebugger.QueueCommand(FContinueCommand);
        FContinueCommand := nil;
      end else
        SetDebuggerState(dsPause);
    end
    else SetDebuggerState(R.State);

    if DebuggerState = dsPause
    then ProcessFrame;
  finally
    ReleaseRefAndNil(FContinueCommand);
  end;

  FSuccess := True;
end;

constructor TGDBMIDebuggerCommandStartDebugging.Create(AOwner: TGDBMIDebugger;
  AContinueCommand: TGDBMIDebuggerCommand);
begin
  inherited Create(AOwner);
  // AContinueCommand, takes over the current reference.
  // Caller will never Release it. So TGDBMIDebuggerCommandStartDebugging must do this
  FContinueCommand := AContinueCommand;
  FSuccess := False;
end;

destructor TGDBMIDebuggerCommandStartDebugging.Destroy;
begin
  ReleaseRefAndNil(FContinueCommand);
  inherited Destroy;
end;

function TGDBMIDebuggerCommandStartDebugging.DebugText: String;
var
  s: String;
begin
  s := '<none>';
  if FContinueCommand <> nil
  then s := FContinueCommand.DebugText;
  Result := Format('%s: ContinueCommand= %s', [ClassName, s]);
end;

{ TGDBMIDebuggerCommandExecute }

procedure TGDBMIDebuggerCommandExecute.DoLockQueueExecute;
begin
  // prevent lock
end;

procedure TGDBMIDebuggerCommandExecute.DoUnockQueueExecute;
begin
  // prevent lock
end;

function TGDBMIDebuggerCommandExecute.ProcessRunning(var AStoppedParams: String; out AResult: TGDBMIExecResult): Boolean;
var
  InLogWarning: Boolean;

  function DoExecAsync(var Line: String): Boolean;
  var
    S: String;
  begin
    Result := False;
    S := GetPart('*', ',', Line);
    case StringCase(S, ['stopped', 'started', 'disappeared']) of
      0: begin // stopped
        AStoppedParams := Line;
      end;
      1, 2:; // Known, but undocumented classes
    else
      // Assume targetoutput, strip char and continue
      DebugLn('[DBGTGT] *');
      Line := S + Line;
      Result := True;
    end;
  end;

  procedure DoStatusAsync(const Line: String);
  begin
    DebugLn('[Debugger] Status output: ', Line);
  end;

  procedure DoResultRecord(Line: String);
  var
    ResultClass: String;
  begin
    DebugLn('[WARNING] Debugger: unexpected result-record: ', Line);

    ResultClass := GetPart('^', ',', Line);
    if Line = ''
    then begin
      if AResult.Values <> ''
      then Include(AResult.Flags, rfNoMI);
    end
    else begin
      AResult.Values := Line;
    end;

    //Result := True;
    case StringCase(ResultClass, ['done', 'running', 'exit', 'error']) of
      0: begin // done
        AResult.State := dsIdle; // just indicate a ressult <> dsNone
      end;
      1: begin // running
        AResult.State := dsRun;
      end;
      2: begin // exit
        AResult.State := dsIdle;
      end;
      3: begin // error
        DebugLn('TGDBMIDebugger.ProcessResult Error: ', Line);
        // todo: implement with values
        if  (pos('msg=', Line) > 0)
        and (pos('not being run', Line) > 0)
        then AResult.State := dsStop
        else AResult.State := dsError;
      end;
    else
      //TODO: should that better be dsError ?
      //Result := False;
      AResult.State := dsIdle; // just indicate a ressult <> dsNone
      DebugLn('[WARNING] Debugger: Unknown result class: ', ResultClass);
    end;
  end;

  procedure DoConsoleStream(const Line: String);
  begin
    DebugLn('[Debugger] Console output: ', Line);
  end;

  procedure DoTargetStream(const Line: String);
  begin
    DebugLn('[Debugger] Target output: ', Line);
  end;

  procedure DoLogStream(const Line: String);
  const
    LogWarning = 'warning:';
  var
    Warning: String;
  begin
    DebugLn('[Debugger] Log output: ', Line);
    Warning := Line;
    if Copy(Warning, 1, 2) = '&"' then
      Delete(Warning, 1, 2);
    if Copy(Warning, Length(Warning) - 2, 3) = '\n"' then
      Delete(Warning, Length(Warning) - 2, 3);
    if LowerCase(Copy(Warning, 1, Length(LogWarning))) = LogWarning then
    begin
      InLogWarning := True;
      Delete(Warning, 1, Length(LogWarning));
      Warning := Trim(Warning);
      DoDbgEvent(ecOutput, etOutputDebugString, Warning);
    end;
    if InLogWarning then
      FLogWarnings := FLogWarnings + Warning + LineEnding;
    if Copy(Line, 1, 5) = '&"\n"' then
      InLogWarning := False;
  end;

var
  S: String;
  idx: Integer;
begin
  Result := True;
  AResult.State := dsNone;
  InLogWarning := False;
  FLogWarnings := '';
  while FTheDebugger.DebugProcessRunning do
  begin
    S := FTheDebugger.ReadLine;
    if S = '(gdb) ' then Break;

    while S <> '' do
    begin
      case S[1] of
        '^': DoResultRecord(S);
        '~': DoConsoleStream(S);
        '@': DoTargetStream(S);
        '&': DoLogStream(S);
        '*': if DoExecAsync(S) then Continue;
        '+': DoStatusAsync(S);
        '=': FTheDebugger.DoNotifyAsync(S);
      else
        // since target output isn't prefixed (yet?)
        // one of our known commands could be part of it.
        idx := Pos('*stopped', S);
        if idx  > 0
        then begin
          DebugLn('[DBGTGT] ', Copy(S, 1, idx - 1));
          Delete(S, 1, idx - 1);
          Continue;
        end
        else begin
          // normal target output
          DebugLn('[DBGTGT] ', S);
        end;
      end;
      Break;
    end;
  end;
end;

function TGDBMIDebuggerCommandExecute.ProcessStopped(const AParams: String;
  const AIgnoreSigIntState: Boolean): Boolean;

  function GetLocation: TDBGLocationRec; // update current location
  var
    R: TGDBMIExecResult;
    S: String;
    FP: TDBGPtr;
    i, cnt: longint;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      Result.SrcLine := -1;
      Result.SrcFile := '';
      Result.FuncName := '';
      // Get the frame and addr info from the call-params
      if tfRTLUsesRegCall in TargetInfo^.TargetFlags
      then begin
        Result.Address := GetPtrValue(TargetInfo^.TargetRegisters[1], []);
        FP := GetPtrValue(TargetInfo^.TargetRegisters[2], []);
      end else begin
        Result.Address := GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 3]);
        FP := GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 4]);
      end;

      if FP <> 0 then begin
        // try finding the stackframe
        cnt := GetStackDepth(33);  // do not search more than 32 deep, takes a lot of time
        i := FindStackFrame(Fp, 0, cnt);
        if i >= 0 then begin
          FTheDebugger.FCurrentStackFrame := i;
        end
        else begin
          ExecuteCommand('-stack-select-frame %u', [FTheDebugger.FCurrentStackFrame], R);
        end;

        if FTheDebugger.FCurrentStackFrame <> 0
        then begin
          // This frame should have all the info we need
          s := GetFrame(FTheDebugger.FCurrentStackFrame);
          if s <> '' then
            FTheDebugger.FCurrentLocation := FrameToLocation(S);
          Result.SrcFile     := FTheDebugger.FCurrentLocation.SrcFile;
          Result.SrcFullName := FTheDebugger.FCurrentLocation.SrcFullName;
          Result.FuncName    := FTheDebugger.FCurrentLocation.FuncName;
          Result.SrcLine     := FTheDebugger.FCurrentLocation.SrcLine;
        end;
        FTheDebugger.FInternalStackFrame := FTheDebugger.FCurrentStackFrame;
      end;

      if (Result.SrcLine = -1) or (Result.SrcFile = '') then begin
        Str(Result.Address, S);
        if ExecuteCommand('info line * POINTER(%s)', [S], R)
        then begin
            Result.SrcLine := StrToIntDef(GetPart('Line ', ' of', R.Values), -1);
            Result.SrcFile := ConvertGdbPathAndFile(GetPart('\"', '\"', R.Values));
        end;
      end;

      FTheDebugger.FCurrentLocation := Result;
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;
  end;

  function GetExceptionInfo: TGDBMIExceptionInfo;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      if tfRTLUsesRegCall in TargetInfo^.TargetFlags
      then  Result.ObjAddr := TargetInfo^.TargetRegisters[0]
      else begin
        if dfImplicidTypes in FTheDebugger.DebuggerFlags
        then Result.ObjAddr := Format('^%s($fp+%d)^', [PointerTypeCast, TargetInfo^.TargetPtrSize * 2])
        else Str(GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 2]), Result.ObjAddr);
      end;
      Result.Name := GetInstanceClassName(Result.ObjAddr, []);
      if Result.Name = ''
      then Result.Name := 'Unknown';
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;
  end;

  procedure ProcessException;
  var
    ExceptionMessage: String;
    CanContinue: Boolean;
    Location: TDBGLocationRec;
    ExceptInfo: TGDBMIExceptionInfo;
  begin
    if FTheDebugger.Exceptions.IgnoreAll
    then begin
      Result := True; //ExecuteCommand('-exec-continue')
      exit;
    end;

    ExceptInfo := GetExceptionInfo;
    // check if we should ignore this exception
    if (FTheDebugger.Exceptions.Find(ExceptInfo.Name) <> nil)
    then begin
      Result := True; //ExecuteCommand('-exec-continue')
      exit;
    end;

    FTheDebugger.QueueExecuteLock;
    try
      if (dfImplicidTypes in FTheDebugger.DebuggerFlags)
      then begin
        if (tfFlagHasTypeException in TargetInfo^.TargetFlags) then begin
          if tfExceptionIsPointer in TargetInfo^.TargetFlags
          then ExceptionMessage := GetText('Exception(%s).FMessage', [ExceptInfo.ObjAddr])
          else ExceptionMessage := GetText('^Exception(%s)^.FMessage', [ExceptInfo.ObjAddr]);
          //ExceptionMessage := GetText('^^Exception($fp+8)^^.FMessage', []);
        end else begin
          // Only works if Exception class is not changed. FMessage must be first member
          ExceptionMessage := GetText('^^char(^%s(%s)+1)^', [PointerTypeCast, ExceptInfo.ObjAddr]);
        end;
      end
      else ExceptionMessage := '### Not supported on GDB < 5.3 ###';

      Location := GetLocation;
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;

    FTheDebugger.DoException(deInternal, ExceptInfo.Name, Location, ExceptionMessage, CanContinue);
    if CanContinue
    then begin
      //ExecuteCommand('-exec-continue')
      Result := True; // outer funciton result
      exit;
    end;

    SetDebuggerState(dsPause); // after GetLocation => dsPause may run stack, watches etc
    FTheDebugger.DoCurrent(Location);
  end;

  procedure ProcessBreak;
  var
    ErrorNo: Integer;
    CanContinue: Boolean;
    Location: TDBGLocationRec;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      if tfRTLUsesRegCall in TargetInfo^.TargetFlags
      then ErrorNo := GetIntValue(TargetInfo^.TargetRegisters[0], [])
      else ErrorNo := Integer(GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 2]));
      ErrorNo := ErrorNo and $FFFF;

      Location := GetLocation;
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;


    FTheDebugger.DoException(deRunError, Format('RunError(%d)', [ErrorNo]), Location, '', CanContinue);
    if CanContinue
    then begin
      //ExecuteCommand('-exec-continue')
      Result := True; // outer funciton result
      exit;
    end;

    SetDebuggerState(dsPause); // after GetLocation => dsPause may run stack, watches etc
    FTheDebugger.DoCurrent(Location);
  end;

  procedure ProcessRunError;
  var
    ErrorNo: Integer;
    CanContinue: Boolean;
    Location: TDBGLocationRec;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      if tfRTLUsesRegCall in TargetInfo^.TargetFlags
      then ErrorNo := GetIntValue(TargetInfo^.TargetRegisters[0], [])
      else ErrorNo := Integer(GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 2]));
      ErrorNo := ErrorNo and $FFFF;

      Location := GetLocation;
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;

    FTheDebugger.DoException(deRunError, Format('RunError(%d)', [ErrorNo]), Location, '', CanContinue);
    if CanContinue
    then begin
      //ExecuteCommand('-exec-continue')
      Result := True; // outer funciton result
      exit;
    end;

    SetDebuggerState(dsPause); // after GetLocation => dsPause may run stack, watches etc
    ProcessFrame(GetFrame(1));
  end;

  procedure ProcessSignalReceived(const AList: TGDBMINameValueList);
  var
    SigInt, CanContinue: Boolean;
    S, F: String;
    {$IFdef MSWindows}
    fixed: Boolean;
    {$ENDIF}
  begin
    // TODO: check to run (un)handled

    S := AList.Values['signal-name'];
    F := AList.Values['frame'];
    {$IFdef MSWindows}
    SigInt := S = 'SIGTRAP';
    {$ELSE}
    SigInt := S = 'SIGINT';
    {$ENDIF}

    {$IFdef MSWindows}
    if SigInt and (FTheDebugger.PauseWaitState = pwsNone) and
       (pos('DbgUiConvertStateChangeStructure', FTheDebugger.FCurrentLocation.FuncName) > 0)
    then begin
      Result := True;
      exit;
    end;
    {$ENDIF}

    if not AIgnoreSigIntState  // not pwsInternal
    or not SigInt
    then begin
      // user-requested pause OR other signal (not sigint)
      // TODO: if SigInt, check that it was issued by IDE
      {$IFdef MSWindows}
      FTheDebugger.QueueExecuteLock;
      try
        fixed := FixThreadForSigTrap;
      finally
        FTheDebugger.QueueExecuteUnlock;
      end;
      // Before anything else goes => correct the thred
      if fixed
      then F := '';
      {$ENDIF}
      SetDebuggerState(dsPause);
    end;

    if not SigInt
    then FTheDebugger.DoException(deExternal, 'External: ' + S, FTheDebugger.FCurrentLocation, '', CanContinue);

    FTheDebugger.QueueExecuteLock;
    try
      if not AIgnoreSigIntState
      or not SigInt
      then ProcessFrame(F);
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;
  end;

  procedure ProcessBreakPoint(ABreakId: Integer; const List: TGDBMINameValueList;
    AReason: TGDBMIBreakpointReason; AOldVal: String = ''; ANewVal: String = '');
  var
    BreakPoint: TGDBMIBreakPoint;
    CanContinue: Boolean;
    Location: TDBGLocationRec;
  begin
    BreakPoint := nil;
    if ABreakId >= 0 then
      BreakPoint := TGDBMIBreakPoint(FTheDebugger.FindBreakpoint(ABreakID));

    if (BreakPoint <> nil) and (BreakPoint.Kind <> bpkData) and
       (AReason in [gbrWatchScope, gbrWatchTrigger])
    then BreakPoint := nil;

    if BreakPoint <> nil
    then begin
      CanContinue := False;
      FTheDebugger.QueueExecuteLock;
      try
        Location := FrameToLocation(List.Values['frame']);
        FTheDebugger.FCurrentLocation := Location;
      finally
        FTheDebugger.QueueExecuteUnlock;
      end;
      FTheDebugger.DoDbgBreakpointEvent(BreakPoint, Location, AReason, AOldVal, ANewVal);
      // Important: The Queue must be unlocked
      //   BreakPoint.Hit may evaluate stack and expressions
      //   SetDebuggerState may evaluate data for Snapshot
      BreakPoint.Hit(CanContinue);
      if CanContinue
      then begin
        // Important trigger State => as snapshot is taken in TDebugManager.DebuggerChangeState
        SetDebuggerState(dsInternalPause);
        Result := True;
      end
      else begin
        SetDebuggerState(dsPause);
        ProcessFrame(Location);
        // inform the user, why we stopped
        // TODO: Add a dedicated callback
        case AReason of
          gbrWatchTrigger: FTheDebugger.OnFeedback
             (self, Format('The Watchpoint for "%1:s" was triggered.%0:s%0:sOld value: %2:s%0:sNew value: %3:s',
                           [LineEnding, BreakPoint.WatchData, AOldVal, ANewVal]),
              '', ftInformation, [frOk]);
          gbrWatchScope: FTheDebugger.OnFeedback
             (self, Format('The Watchpoint for "%s" went out of scope', [BreakPoint.WatchData]),
              '', ftInformation, [frOk]);
        end;
      end;

      if AReason = gbrWatchScope
      then begin
        BreakPoint.ReleaseBreakPoint; // gdb should have released already => ignore error
        BreakPoint.Enabled := False;
        BreakPoint.FBreakID := 0; // removed by debugger, ID no longer exists
      end;

      exit;
    end;

    // The temp-at-start breakpoint is not checked. Ignore it
    if (DebuggerState = dsRun) and (FTheDebugger.TargetPID <> 0) // not in startup
    then begin
      debugln(['********** WARNING: breakpoint hit, but nothing known about it ABreakId=', ABreakID, ' brbtno=', List.Values['bkptno'] ]);
      {$IFDEF DBG_VERBOSE_BRKPOINT}
      debugln(['-*- List of breakpoints Cnt=', FTheDebugger.Breakpoints.Count]);
      for ABreakID := 0 to FTheDebugger.Breakpoints.Count - 1 do
        debugln(['* ',Dbgs(FTheDebugger.Breakpoints[ABreakID]), ':', DbgsName(FTheDebugger.Breakpoints[ABreakID]), ' ABreakId=',TGDBMIBreakPoint(FTheDebugger.Breakpoints[ABreakID]).FBreakID, ' Source=', FTheDebugger.Breakpoints[ABreakID].Source, ' Line=', FTheDebugger.Breakpoints[ABreakID].Line ]);
      debugln(['************************************************************************ ']);
      debugln(['************************************************************************ ']);
      debugln(['************************************************************************ ']);
      {$ENDIF}

      case FTheDebugger.OnFeedback
             (self, Format(gdbmiWarningUnknowBreakPoint,
                           [LineEnding, GDBMIBreakPointReasonNames[AReason]]),
              List.Text, ftWarning, [frOk, frStop]
             )
      of
        frOk: begin
            SetDebuggerState(dsPause);
            ProcessFrame(List.Values['frame']); // and jump to it
          end;
        frStop: begin
            FTheDebugger.Stop;
          end;
      end;

    end;
  end;

var
  List, List2: TGDBMINameValueList;
  Reason: String;
  BreakID: Integer;
  CanContinue: Boolean;
begin
  (* The Queue is not locked / This code can be interupted
     Therefore all calls to ExecuteCommand (gdb cmd) must be wrapped in QueueExecuteLock
  *)
  Result := False;
  FTheDebugger.FInProcessStopped := True;  // paused, but maybe state run

  List := TGDBMINameValueList.Create(AParams);
  List2 := nil;

  FTheDebugger.FCurrentStackFrame :=  0;
  FTheDebugger.FInternalStackFrame := 0;
  FTheDebugger.FCurrentThreadId := StrToIntDef(List.Values['thread-id'], -1);
  FTheDebugger.FInternalThreadId := FTheDebugger.FCurrentThreadId;
  FTheDebugger.FCurrentLocation := FrameToLocation(List.Values['frame']);

  try
    Reason := List.Values['reason'];
    if (Reason = 'exited-normally')
    then begin
      DoDbgEvent(ecProcess, etProcessExit, 'Process Exit: normally');
      SetDebuggerState(dsStop);
      Exit;
    end;

    if Reason = 'exited'
    then begin
      FTheDebugger.SetExitCode(StrToIntDef(List.Values['exit-code'], 0));
      DoDbgEvent(ecProcess, etProcessExit, 'Process Exit: ' + List.Values['exit-code']);
      SetDebuggerState(dsStop);
      Exit;
    end;

    if Reason = 'exited-signalled'
    then begin
      SetDebuggerState(dsStop);
      FTheDebugger.DoException(deExternal, 'External: ' + List.Values['signal-name'], FTheDebugger.FCurrentLocation, '', CanContinue);
      // ProcessFrame(List.Values['frame']);
      Exit;
    end;

    if Reason = 'signal-received'
    then begin
      ProcessSignalReceived(List);
      Exit;
    end;

    if Reason = 'watchpoint-trigger'
    then begin
      List2 := TGDBMINameValueList.Create(List.Values['wpt']);
      BreakID := StrToIntDef(List2.Values['number'], -1);
      // Use List2.Values['exp'] ? It may contain globalized expression
      List2.Init(List.Values['value']);
      ProcessBreakPoint(BreakID, List, gbrWatchTrigger, List2.Values['old'], List2.Values['new']);
      exit;
    end;

    if Reason = 'watchpoint-scope'
    then begin
      BreakID := StrToIntDef(List.Values['wpnum'], -1);
      ProcessBreakPoint(BreakID, List, gbrWatchScope);
      exit;
    end;

    if Reason = 'breakpoint-hit'
    then begin
      BreakID := StrToIntDef(List.Values['bkptno'], -1);
      if BreakID = -1
      then begin
        ProcessBreakPoint(BreakID, List, gbrBreak);
        SetDebuggerState(dsError);
        Exit;
      end;

      if FTheDebugger.FBreakErrorBreak.MatchId(BreakID)
      then begin
        ProcessBreak; // will set dsPause / unless CanContinue
        Exit;
      end;

      if FTheDebugger.FRunErrorBreak.MatchId(BreakID)
      then begin
        ProcessRunError; // will set dsPause / unless CanCuntinue
        Exit;
      end;

      if FTheDebugger.FExceptionBreak.MatchId(BreakID)
      then begin
        ProcessException; // will set dsPause / unless CanCuntinue
        Exit;
      end;

      if (FStepBreakPoint > 0) and (BreakID = FStepBreakPoint)
      then begin
        SetDebuggerState(dsPause);
        ProcessFrame(FTheDebugger.FCurrentLocation );
        exit;
      end;

      ProcessBreakPoint(BreakID, List, gbrBreak);
      exit;
    end;

    if Reason = 'function-finished'
    then begin
      SetDebuggerState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;

    if Reason = 'end-stepping-range'
    then begin
      SetDebuggerState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;

    if Reason = 'location-reached'
    then begin
      SetDebuggerState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;

    // Some versions of GDB do not give any reason if hitting a temporary breakpoint
    // (like the temp-at-main during startup)
    if (FTheDebugger.TargetPID <> 0) // not in startup
    then begin
      DebugLn('[WARNING] Debugger: Unknown stopped reason: ', Reason);
      SetDebuggerState(dsPause);
      ProcessFrame(List.Values['frame']);
    end;
  finally
    FTheDebugger.FInProcessStopped := False;
    List.Free;
    list2.Free;
  end;
end;

{$IFDEF MSWindows}
function TGDBMIDebuggerCommandExecute.FixThreadForSigTrap: Boolean;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
  s: string;
  n, ID1, ID2: Integer;
begin
  Result := False;
  if not ExecuteCommand('info program', R)
  then exit;
  S := GetPart(['.0x'], ['.'], R.Values, True, False); // From the line "using child thread"
  if PtrInt(StrToQWordDef('$'+S, 0)) <> FTheDebugger.FPauseRequestInThreadID
  then Exit;


  if not ExecuteCommand('-thread-list-ids', R)
  then Exit;
  List := TGDBMINameValueList.Create(R);
  try
    n := StrToIntDef(List.Values['number-of-threads'], 0);
    if n < 2 then Exit; //nothing to switch
    List.SetPath(['thread-ids']);
    if List.Count < 2 then Exit; // ???
    ID1 := StrToIntDef(List.Values['thread-id'], 0);
    List.Delete(0);
    ID2 := StrToIntDef(List.Values['thread-id'], 0);

    if ID1 = ID2 then Exit;
  finally
    List.Free;
  end;

  Result := ExecuteCommand('-thread-select %d', [ID2], []);
  FTheDebugger.FCurrentThreadId := ID2;
  FTheDebugger.FInternalThreadId := FTheDebugger.FCurrentThreadId;
end;
{$ENDIF}

function TGDBMIDebuggerCommandExecute.DoExecute: Boolean;
const
  BreaKErrMsg = 'not insert breakpoint ';
  WatchErrMsg = 'not insert hardware watchpoint ';

  function HandleBreakPointError(var ARes: TGDBMIExecResult; AError: String): Boolean;

  function ErrPos(s: string): integer;
  var
    i: SizeInt;
  begin
    Result := pos(BreaKErrMsg, s);
    if Result > 0
    then Result := Result + length(BreaKErrMsg);
    i := pos(WatchErrMsg, s);
    if (i > 0) and ( (i < Result) or (Result < 1) )
    then Result := i + length(WatchErrMsg);
  end;

  var
    c, i: Integer;
    bp: Array of Integer;
    s, s2: string;
    b: TGDBMIBreakPoint;
  begin
    Result := False;
    s := AError;
    c := 0;
    i := ErrPos(s);
    while i > 0 do begin
      s := copy(s, i, length(s));
      i := 1;
      while (i <= length(s)) and (s[i] in ['0'..'9']) do inc(i);
      if i > 1 then begin
        SetLength(bp, c+1);
        bp[c] := StrToIntDef(copy(s, 1, i-1), -1);
        if bp[c] >= 0 then inc(c);
      end;
      i := ErrPos(s);
    end;
    if c = 0 then exit;

    Result := True;

    if ARes.State = dsError
    then begin
      s := ARes.Values;
      if FLogWarnings <> ''
      then s2 := Format(gdbmiErrorOnRunCommandWithWarning, [LineEnding, FLogWarnings])
      else s2 := '';
      FLogWarnings := '';
    end else begin
      s := AError;
      s2 := '';
    end;

    case FTheDebugger.OnFeedback(self,
                                 Format(gdbmiBreakPointErrorOnRunCommand, [LineEnding, s]) + s2,
                                 ARes.Values, ftError, [frOk, frStop]
         ) of
      frOk: begin
          ARes.State := dsPause;
          ProcessFrame;
          FTheDebugger.FInProcessStopped := True;  // paused, but maybe state run
          try
            for i := 0 to length(bp)-1 do begin
              b := TGDBMIBreakPoints(FTheDebugger.BreakPoints).FindById(bp[i]);
              if b <> nil
              then begin
                if b.Kind = bpkData
                then b.Enabled := False
                else b.MakeInvalid;
              end
              else ExecuteCommand('-break-delete %d', [bp[i]], []);
            end;
          finally
          FTheDebugger.FInProcessStopped := False;  // paused, but maybe state run
          end;
        end;
      frStop: begin
          FTheDebugger.Stop;
          ARes.State := dsStop;
        end;
    end;

  end;

  function HandleRunError(var ARes: TGDBMIExecResult): Boolean;
  var
    s, s2: String;
    List: TGDBMINameValueList;
  begin
    Result := False; // keep the error state
    // check known errors
    if (Pos('program is not being run', ARes.Values) > 0) then begin  // Should lead to dsStop
      SetDebuggerState(dsError);
      exit;
    end;
    if (Pos(BreaKErrMsg, ARes.Values) > 0) or
       (Pos(BreaKErrMsg, FLogWarnings) > 0) or
       (Pos(WatchErrMsg, ARes.Values) > 0) or
       (Pos(WatchErrMsg, FLogWarnings) > 0)
    then begin
      Result := HandleBreakPointError(ARes, ARes.Values + FLogWarnings);
      if Result then exit;
    end;

    if assigned(FTheDebugger.OnFeedback) then begin
      List := TGDBMINameValueList.Create(ARes);
      s := List.Values['msg'];
      FreeAndNil(List);
      if FLogWarnings <> ''
      then s2 := Format(gdbmiErrorOnRunCommandWithWarning, [LineEnding, FLogWarnings])
      else s2 := '';
      FLogWarnings := '';
      if s <> '' then begin
        case FTheDebugger.OnFeedback(self,
                                     Format(gdbmiErrorOnRunCommand, [LineEnding, s]) + s2,
                                     ARes.Values, ftError, [frOk, frStop]
             ) of
          frOk: begin
              ARes.State := dsPause;
              ProcessFrame;
              Result := True;
            end;
          frStop: begin
              FTheDebugger.Stop;
              ARes.State := dsStop;
              Result := True;
              exit;
            end;
        end;
      end
    end;
  end;

  function CheckResultForError(var ARes: TGDBMIExecResult): Boolean;
  begin
    Result := False;
    if (ARes.State = dsError) and (not HandleRunError(ARes)) then begin
      DoDbgEvent(ecDebugger, etDefault, Format(gdbmiFatalErrorOccured, [ARes.Values]));
      SetDebuggerState(dsError);
      Result := True;
    end;
  end;

  function FindStackWithSymbols(StartAt,
    MaxDepth: Integer): Integer;
  var
    R: TGDBMIExecResult;
    List: TGDBMINameValueList;
  begin
    // Result;
    // -1 : Not found
    // -2 : FP is outside stack
    Result := StartAt;
    List := TGDBMINameValueList.Create('');
    try
      repeat
        if not ExecuteCommand('-stack-list-frames %d %d', [Result, Result], R)
        or (R.State = dsError)
        then begin
          Result := -1;
          break;
        end;

        List.Init(R.Values);
        List.SetPath('stack');
        if List.Count > 0 then List.Init(List.GetString(0));
        List.SetPath('frame');
        if List.Values['file'] <> ''
        then exit;

        inc(Result);
      until Result > MaxDepth;

      Result := -1;
    finally
      List.Free;
    end;
  end;

var
  FP: TDBGPtr;

  function DoContinueStepping: Boolean;
    procedure DoEndStepping;
    begin
      Result := True;
      FCommand := '';
      SetDebuggerState(dsPause);
      FTheDebugger.DoCurrent(FTheDebugger.FCurrentLocation);
    end;
  const
    MaxStackDepth = 99;
  var
    cnt, i: Integer;
    R: TGDBMIExecResult;
  begin
    // TODO: an exception can skip the step-end breakpoint....
    // TODO: the "break" breakpoint can stop on the current, instead of the next instruction

    Result := False;
    case FExecType of
      ectContinue, ectRun:
        begin
          FCommand := '-exec-continue';
          Result := True;
        end;
      ectRunTo:  // check if we are at correct location
        begin
          Result := not(
              ( (FTheDebugger.FCurrentLocation.SrcFile = FRunToSrc) or
                (FTheDebugger.FCurrentLocation.SrcFullName = FRunToSrc) ) and
              (FTheDebugger.FCurrentLocation.SrcLine = FRunToLine)
            );
          if not Result
          then DoEndStepping;  // location reached
        end;
      ectStepOver, ectStepOverInstruction, ectStepOut, ectStepInto:
        begin
          Result := FStepBreakPoint > 0;
          if Result
          then exit;

          i := -1;
          if FP <> 0 then begin
            cnt := GetStackDepth(MaxStackDepth);
            if FExecType = ectStepInto
            then i := FindStackWithSymbols(0, cnt)
            else i := FindStackFrame(Fp, 0, cnt);
            if (FExecType = ectStepOut) and (i >= 0)
            then inc(i);
          end;

          if (i = 0) or (i = -2)  // -2 already stepped out of the desired frame, enter dsPause
          then begin
            DoEndStepping;
            exit;
          end;

          if i > 0
          then begin
            // must use none gdbmi commands
            if (not ExecuteCommand('frame %d', [i], R)) or (R.State = dsError)
            then i := -3; // error to user
            if (i < 0) or (not ExecuteCommand('break', [i], R)) or (R.State = dsError)
            then i := -3; // error to user

            FStepBreakPoint := StrToIntDef(GetPart(['Breakpoint '], [' at '], R.Values), -1);
            if FStepBreakPoint < 0
            then i := -3;

            if i > 0 then begin
              Result := True;
              FCommand := '-exec-continue';
            end;
          end;
          if i < 0
          then begin
            DebugLn(['CommandExecute: exStepOver, frame not found: ', i]);
            DoEndStepping; // TODO: User-error feedback
          end;
        end;
      //ectStepOut:
      //  begin
      //  end;
      //ectStepInto:
      //  begin
      //  end;
      //ectStepOverInstruction:
      //  begin
      //  end;
      ectStepIntoInstruction:
        DoEndStepping;
      ectReturn:
        DoEndStepping;
    end;
  end;

var
  StoppedParams, RunWarnings: String;
  ContinueExecution, ContinueStep: Boolean;
  NextExecCmdObj: TGDBMIDebuggerCommandExecute;
  R: TGDBMIExecResult;
begin
  Result := True;
  FCanKillNow := False;
  FDidKillNow := False;
  FNextExecQueued := False;
  FP := 0;
  ContinueStep := False; // A step command was interupted, and is continued on breakpoint
  FStepBreakPoint := -1;

  try
    repeat
      FTheDebugger.QueueExecuteLock; // prevent other commands from executing
      try
        if (not ContinueStep) and
           (FExecType in [ectStepOver, ectStepInto, ectStepOut, ectStepOverInstruction, ectStepIntoInstruction])
        then FP := GetPtrValue('$fp', []);

        if not ExecuteCommand(FCommand, FResult)
        then exit;
        if CheckResultForError(FResult)
        then exit;
        RunWarnings := FLogWarnings;

        if (FResult.State <> dsNone)
        then SetDebuggerState(FResult.State);

        // if ContinueExecution will be true, the we ignore dsError..
        // TODO: check for cancelled
        StoppedParams := '';
        FCanKillNow := True;
        R.State := dsNone;
        if FResult.State = dsRun
        then Result := ProcessRunning(StoppedParams, R);
      finally
        FCanKillNow := False;
        // allow other commands to execute
        // e.g. source-line-info, watches.. all triggered in ProcessStopped)
        //TODO: prevent the next exec-command from running (or the order of SetLocation in Process Stopped is wrong)
        FTheDebugger.QueueExecuteUnlock;
      end;

      if FDidKillNow or CheckResultForError(R)
      then exit;
      if HandleBreakPointError(FResult, RunWarnings + LineEnding + FLogWarnings) then begin
        if FResult.State = dsStop then exit;
      end;

      ContinueExecution := False;
      ContinueStep := False;
      if StoppedParams <> ''
      then ContinueExecution := ProcessStopped(StoppedParams, FTheDebugger.PauseWaitState = pwsInternal);
      if ContinueExecution
      then begin
        ContinueStep := DoContinueStepping; // will set dsPause, if step has finished

        if (not ContinueStep) and (FCommand <> '') then begin
          // - Fall back to "old" behaviour and queue a new exec-continue
          // - Queue is unlocked, so nothing should be empty
          //   But make info available, if anything wants to queue
          FNextExecQueued := True;
          {$IFDEF DBGMI_QUEUE_DEBUG}
          DebugLn(['CommandExecute: Internal queuing -exec-continue (ContinueExecution = True)']);
          {$ENDIF}
          FTheDebugger.FPauseWaitState := pwsNone;
          NextExecCmdObj := TGDBMIDebuggerCommandExecute.Create(FTheDebugger, ectContinue);
          FTheDebugger.QueueExecuteLock; // force queue
          FTheDebugger.QueueCommand(NextExecCmdObj, DebuggerState = dsInternalPause); // TODO: ForceQueue, only until better means of queue control... (allow snapshot to run)
          FTheDebugger.QueueExecuteUnlock;
        end;
      end;

    until (not ContinueStep) or (FCommand = '');

  finally
    if FStepBreakPoint > 0
    then ExecuteCommand('-break-delete %d', [FStepBreakPoint], []);
    FStepBreakPoint := -1;
  end;

  if (not ContinueExecution) and (DebuggerState = dsRun) and
     (TargetInfo^.TargetPID <> 0) and (FTheDebugger.PauseWaitState <> pwsInternal)
  then begin
    // Handle the unforeseen
    if (StoppedParams <> '')
    then debugln(['ERROR: Got stop params, but did not change FTheDebugger.state: ', StoppedParams])
    else debugln(['ERROR: Got NO stop params at all, but was running']);
    SetDebuggerState(dsPause);
  end;
end;

constructor TGDBMIDebuggerCommandExecute.Create(AOwner: TGDBMIDebugger;
  const ExecType: TGDBMIExecCommandType);
begin
  Create(AOwner, ExecType, []);
end;

constructor TGDBMIDebuggerCommandExecute.Create(AOwner: TGDBMIDebugger;
  const ExecType: TGDBMIExecCommandType; Args: array of const);
begin
  inherited Create(AOwner);
  FQueueRunLevel := 0; // Execommands are only allowed at level 0
  FCanKillNow := False;
  FDidKillNow := False;;
  FNextExecQueued := False;
  FExecType := ExecType;
  case FExecType of
    ectContinue: FCommand := '-exec-continue';
    ectRun:      FCommand := '-exec-run';
    ectRunTo:
      begin
                 FCommand := Format('-exec-until %s:%d', Args);
                 FRunToSrc := AnsiString(Args[0].VAnsiString);
                 FRunToLine := Args[1].VInteger;
      end;
    ectStepOver: FCommand := '-exec-next';
    ectStepOut:  FCommand := '-exec-finish';
    ectStepInto: FCommand := '-exec-step';
    ectStepOverInstruction: FCommand := '-exec-next-instruction';
    ectStepIntoInstruction: FCommand := '-exec-step-instruction';
    ectReturn:   FCommand := '-exec-return';
  end;
end;

function TGDBMIDebuggerCommandExecute.DebugText: String;
begin
  Result := Format('%s: %s', [ClassName, FCommand]);
end;

function TGDBMIDebuggerCommandExecute.KillNow: Boolean;
var
  StoppedParams: String;
  R: TGDBMIExecResult;
begin
  Result := False;
  if not FCanKillNow then exit;
  // only here, if we are in ProcessRunning
  FDidKillNow := True;

  FTheDebugger.GDBPause(True);
  FTheDebugger.CancelAllQueued; // before ProcessStopped
  Result := ProcessRunning(StoppedParams, R);
  if ProcessResultTimedOut then begin
    // the uter Processrunning should stop, due to process no longer running
    FTheDebugger.DebugProcess.Terminate(0);
    Result := True;
    exit;
  end;
  if StoppedParams <> ''
  then ProcessStopped(StoppedParams, FTheDebugger.PauseWaitState = pwsInternal);

  ExecuteCommand('kill', [], 1500);
  Result := ExecuteCommand('info program', [], R);
  Result := Result and (Pos('not being run', R.Values) > 0);
  if Result
  then SetDebuggerState(dsStop);

  // Now give the ProcessRunning in the current DoExecute something
  FTheDebugger.SendCmdLn('print 1');
end;

{ TGDBMIDebuggerCommandLineSymbolInfo }

function TGDBMIDebuggerCommandLineSymbolInfo.DoExecute: Boolean;
var
  Src: String;
begin
  Result := True;
  ExecuteCommand('-symbol-list-lines %s', [FSource], FResult);

  if FResult.State = dsError
  then begin
    // the second trial: gdb can return info to file w/o path
    Src := ExtractFileName(FSource);
    if Src <> FSource
    then ExecuteCommand('-symbol-list-lines %s', [Src], FResult);
  end;
end;

constructor TGDBMIDebuggerCommandLineSymbolInfo.Create(AOwner: TGDBMIDebugger;
  Source: string);
begin
  inherited Create(AOwner);
  FSource := Source;
end;

function TGDBMIDebuggerCommandLineSymbolInfo.DebugText: String;
begin
  Result := Format('%s: Source=%s', [ClassName, FSource]);
end;

{ TGDBMIDebuggerCommandRegisterValues }

function TGDBMIDebuggerCommandRegisterValues.DoExecute: Boolean;
const
  // rdDefault, rdHex, rdBinary, rdOctal, rdDecimal, rdRaw
  FormatChar : array [TRegisterDisplayFormat] of string =
    ('N', 'x', 't', 'o', 'd', 'r');
var
  R: TGDBMIExecResult;
  List, ValList: TGDBMINameValueList;
  Item: PGDBMINameValue;
  n, idx: Integer;
begin
  Result := True;
  if length(FRegistersToUpdate) = 0
  then exit;

  for n := Low(FRegistersToUpdate) to High(FRegistersToUpdate) do
    FRegistersToUpdate[n] := '';

  ExecuteCommand('-data-list-register-values %s', [FormatChar[FFormat]], R);
  if R.State = dsError then Exit;


  ValList := TGDBMINameValueList.Create('');
  List := TGDBMINameValueList.Create(R, ['register-values']);
  for n := 0 to List.Count - 1 do
  begin
    Item := List.Items[n];
    ValList.Init(Item^.Name);
    idx := StrToIntDef(Unquote(ValList.Values['number']), -1);
    if (idx >= Low(FRegistersToUpdate)) and
       (idx <= High(FRegistersToUpdate))
    then FRegistersToUpdate[idx] := Unquote(ValList.Values['value']);
  end;
  FreeAndNil(List);
  FreeAndNil(ValList);
end;

constructor TGDBMIDebuggerCommandRegisterValues.Create(AOwner: TGDBMIDebugger;
  RegistersToUpdate: TStringArray; AFormat: TRegisterDisplayFormat = rdDefault);
begin
  inherited Create(AOwner);
  FRegistersToUpdate := RegistersToUpdate;
  FFormat := AFormat;
end;

function TGDBMIDebuggerCommandRegisterValues.DebugText: String;
begin
  Result := SysUtils.Format('%s: Reg-Cnt=%d', [ClassName, length(FRegistersToUpdate)]);
end;

{ TGDBMIDebuggerCommandRegisterNames }

function TGDBMIDebuggerCommandRegisterNames.GetNames(Index: Integer): string;
begin
  Result := FNames[Index];
end;

function TGDBMIDebuggerCommandRegisterNames.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
  n: Integer;
begin
  Result := True;
  ExecuteCommand('-data-list-register-names', R);
  if R.State = dsError then Exit;

  List := TGDBMINameValueList.Create(R, ['register-names']);
  SetLength(FNames, List.Count);
  for n := 0 to List.Count - 1 do
    FNames[n] := UnQuote(List.GetString(n));
  FreeAndNil(List);
end;

function TGDBMIDebuggerCommandRegisterNames.Count: Integer;
begin
  Result := length(FNames);
end;

{ TGDBMIDebuggerCommandStackDepth }

function TGDBMIDebuggerCommandStackDepth.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
  i, cnt: longint;
begin
  Result := True;
  FDepth := -1;
  try
    if not SelectThread then exit;

    ExecuteCommand('-stack-info-depth', R);
    List := TGDBMINameValueList.Create(R);
    cnt := StrToIntDef(List.Values['depth'], -1);
    FreeAndNil(List);
    if cnt = -1 then
    begin
      { In case of error some stackframes still can be accessed.
        Trying to find out how many...
        We try maximum 40 frames, because sometimes a corrupt stack and a bug in
        gdb may cooperate, so that -stack-info-depth X returns always X }
      i:=0;
      repeat
        inc(i);
        ExecuteCommand('-stack-info-depth %d', [i], R);
        List := TGDBMINameValueList.Create(R);
        cnt := StrToIntDef(List.Values['depth'], -1);
        FreeAndNil(List);
        if (cnt = -1) then begin
          // no valid stack-info-depth found, so the previous was the last valid one
          cnt:=i - 1;
        end;
      until (cnt<i) or (i=40);
    end;
    FDepth := cnt;
  finally
    UnSelectThread;
  end;
end;

function TGDBMIDebuggerCommandStackDepth.DebugText: String;
begin
  Result := Format('%s:', [ClassName]);
end;

{ TGDBMIDebuggerCommandStackFrames }

function TGDBMIDebuggerCommandStackFrames.DoExecute: Boolean;
var
  CurStartIdx: Integer;
  It: TMapIterator;

  procedure FreeList(var AList: TGDBMINameValueListArray);
  var
    i : Integer;
  begin
    for i := low(AList) to high(AList) do
      AList[i].Free;
  end;

  procedure UpdateEntry(AnEntry: TCallStackEntry; AArgInfo, AFrameInfo : TGDBMINameValueList);
  var
    n, e: Integer;
    Arguments: TStringList;
    List: TGDBMINameValueList;
    Arg: PGDBMINameValue;
    addr: TDbgPtr;
    func, filename, fullname, line : String;
  begin
    Arguments := TStringList.Create;

    if (AArgInfo <> nil) and (AArgInfo.Count > 0)
    then begin
      List := TGDBMINameValueList.Create('');
      for n := 0 to AArgInfo.Count - 1 do
      begin
        Arg := AArgInfo.Items[n];
        List.Init(Arg^.Name);
        Arguments.Add(List.Values['name'] + '=' + DeleteEscapeChars(List.Values['value']));
      end;
      FreeAndNil(List);
    end;

    addr := 0;
    func := '';
    filename := '';
    fullname := '';
    line := '';
    if AFrameInfo <> nil
    then begin
      Val(AFrameInfo.Values['addr'], addr, e);
      if e=0 then ;
      func := AFrameInfo.Values['func'];
      filename := ConvertGdbPathAndFile(AFrameInfo.Values['file']);
      fullname := ConvertGdbPathAndFile(AFrameInfo.Values['fullname']);
      line := AFrameInfo.Values['line'];
    end;

    AnEntry.Init(
      addr,
      Arguments,
      func,
      FTheDebugger.UnitInfoProvider.GetUnitInfoFor(filename, fullname),
      StrToIntDef(line, 0)
    );

    Arguments.Free;
  end;

  procedure PrepareArgs(var ADest: TGDBMINameValueListArray; AStart, AStop: Integer;
                        const ACmd, APath1, APath2: String);
  var
    R: TGDBMIExecResult;
    i, lvl : Integer;
    ResultList, SubList: TGDBMINameValueList;
  begin
    ExecuteCommand(ACmd, [AStart, AStop], R);

    if R.State = dsError
    then begin
      i := AStop - AStart;
      case i of
        0   : exit;
        1..5: begin
          while i >= 0 do
          begin
            PrepareArgs(ADest, AStart+i, AStart+i, ACmd, APath1, APath2);
            dec(i);
          end;
        end;
      else
        i := i div 2;
        PrepareArgs(ADest, AStart, AStart+i, ACmd, APath1, APath2);
        PrepareArgs(ADest, AStart+i+1, AStop, ACmd, APath1, APath2);
      end;
    end;

    ResultList := TGDBMINameValueList.Create(R, [APath1]);
    for i := 0 to ResultList.Count - 1 do
    begin
      SubList := TGDBMINameValueList.Create(ResultList.GetString(i), ['frame']);
      lvl := StrToIntDef(SubList.Values['level'], -1);
      if (lvl >= AStart) and (lvl <= AStop)
      then begin
        if APath2 <> ''
        then SubList.SetPath(APath2);
        ADest[lvl-CurStartIdx] := SubList;
      end
      else SubList.Free;
    end;
    ResultList.Free;
  end;

  procedure ExecForRange(AStartIdx, AEndIdx: Integer);
  var
    Args: TGDBMINameValueListArray;
    Frames: TGDBMINameValueListArray;
    e: TCallStackEntry;
  begin
    try
      CurStartIdx := AStartIdx;
      SetLength(Args, AEndIdx-AStartIdx+1);
      PrepareArgs(Args, AStartIdx, AEndIdx, '-stack-list-arguments 1 %d %d', 'stack-args', 'args');
      if (FCallstack = nil) or (dcsCanceled in SeenStates) then exit;

      SetLength(Frames, AEndIdx-AStartIdx+1);
      PrepareArgs(Frames, AStartIdx, AEndIdx, '-stack-list-frames %d %d', 'stack', '');
      if (FCallstack = nil) or (dcsCanceled in SeenStates) then exit;

      if not It.Locate(AStartIdx)
      then if not It.EOM
      then IT.Next;
      while it.Valid and (not It.EOM) do begin
        e := TCallStackEntry(It.DataPtr^);
        if e.Index > AEndIdx then break;
        UpdateEntry(e, Args[e.Index-AStartIdx], Frames[e.Index-AStartIdx]);
        It.Next;
      end;

    finally
      FreeList(Args);
      FreeList(Frames);
    end;
  end;

var
  StartIdx, EndIdx: Integer;
begin
  Result := True;
  if (FCallstack = nil) or (dcsCanceled in SeenStates) then exit;

  It := TMapIterator.Create(FCallstack.RawEntries);
  try
    //if It.Locate(AIndex)
    StartIdx := Max(FCallstack.LowestUnknown, 0);
    EndIdx   := FCallstack.HighestUnknown;
    while EndIdx >= StartIdx do begin
      if (FCallstack = nil) or (dcsCanceled in SeenStates) then break;
      {$IFDEF DBG_VERBOSE}
      debugln(['Callstach.Frames A StartIdx=',StartIdx, ' EndIdx=',EndIdx]);
      {$ENDIF}
      // search for existing blocks in the middle
      if not It.Locate(StartIdx)
      then if not It.EOM
      then IT.Next;
      StartIdx := TCallStackEntry(It.DataPtr^).Index;
      EndIdx := StartIdx;
      It.Next;
      while (not It.EOM) and (TCallStackEntry(It.DataPtr^).Index = EndIdx+1) do begin
        inc(EndIdx);
        It.Next;
      end;

      {$IFDEF DBG_VERBOSE}
      debugln(['Callstach.Frames B StartIdx=',StartIdx, ' EndIdx=',EndIdx]);
      {$ENDIF}
      ExecForRange(StartIdx, EndIdx);
      if (FCallstack = nil) or (dcsCanceled in SeenStates) then break;

      if FCallstack.LowestUnknown < StartIdx
      then StartIdx := FCallstack.LowestUnknown
      else StartIdx := EndIdx + 1;
      if FCallstack.HighestUnknown > EndIdx
      then EndIdx := FCallstack.HighestUnknown;
    end;
  finally
    IT.Free;
    if FCallstack <> nil
    then FCallstack.DoEntriesUpdated;
  end;
end;

{ TGDBMILineInfo }

procedure TGDBMILineInfo.DoGetLineSymbolsDestroyed(Sender: TObject);
begin
  if FGetLineSymbolsCmdObj = Sender
  then FGetLineSymbolsCmdObj := nil;
end;

procedure TGDBMILineInfo.ClearSources;
var
  n: Integer;
begin
  for n := Low(FSourceMaps) to High(FSourceMaps) do
    FSourceMaps[n].Map.Free;
  Setlength(FSourceMaps, 0);

  for n := 0 to FSourceIndex.Count - 1 do
    DoChange(FSourceIndex[n]);

  FSourceIndex.Clear;
end;

procedure TGDBMILineInfo.AddInfo(const ASource: String; const AResult: TGDBMIExecResult);
var
  ID: packed record
    Line, Column: Integer;
  end;
  Map: TMap;
  n, idx: Integer;
  LinesList, LineList: TGDBMINameValueList;
  Item: PGDBMINameValue;
  Addr: TDbgPtr;
begin
  n := FSourceIndex.IndexOf(ASource);
  if n = -1
  then begin
    idx := Length(FSourceMaps);
    SetLength(FSourceMaps, idx+1);
    FSourceMaps[idx].Map := nil;
    FSourceMaps[idx].Source := ASource;
    n := FSourceIndex.AddObject(ASource, TObject(PtrInt(idx)));
  end
  else idx := PtrInt(FSourceIndex.Objects[n]);

  LinesList := TGDBMINameValueList.Create(AResult, ['lines']);
  if LinesList = nil then Exit;

  Map := FSourceMaps[idx].Map;
  if Map = nil
  then begin
    // no map present
    Map := TMap.Create(its8, SizeOf(TDBGPtr));
    FSourceMaps[idx].Map := Map;
  end;

  ID.Column := 0;
  LineList := TGDBMINameValueList.Create('');
  for n := 0 to LinesList.Count - 1 do
  begin
    Item := LinesList.Items[n];
    LineList.Init(Item^.Name);
    if not TryStrToInt(Unquote(LineList.Values['line']), ID.Line) then Continue;
    if not TryStrToQWord(Unquote(LineList.Values['pc']), Addr) then Continue;
    // one line can have more than one address
    if Map.HasId(ID) then Continue;
    Map.Add(ID, Addr);
  end;
  LineList.Free;
  LinesList.Free;
  DoChange(ASource);
end;

function TGDBMILineInfo.Count: Integer;
begin
  Result := FSourceIndex.Count;
end;

function TGDBMILineInfo.GetSource(const AIndex: integer): String;
begin
  if AIndex < Low(FSourceMaps) then Exit('');
  if AIndex > High(FSourceMaps) then Exit('');

  Result := FSourceMaps[AIndex].Source;
end;

function TGDBMILineInfo.GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr;
var
  ID: packed record
    Line, Column: Integer;
  end;
  Map: TMap;
begin
  if AIndex < Low(FSourceMaps) then Exit(0);
  if AIndex > High(FSourceMaps) then Exit(0);

  Map := FSourceMaps[AIndex].Map;
  if Map = nil then Exit(0);

  ID.Line := ALine;
  // since we do not have column info we map all on column 0
  // ID.Column := AColumn;
  ID.Column := 0;
  if (Map = nil) then Exit(0);
  if not Map.GetData(ID, Result) then
    Result := 0;
end;

function TGDBMILineInfo.GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean;
begin
  Result := False;
end;

procedure TGDBMILineInfo.DoStateChange(const AOldState: TDBGState);
begin
  if not (Debugger.State in [dsPause, dsInternalPause, dsRun]) then
    ClearSources;
end;

function TGDBMILineInfo.IndexOf(const ASource: String): integer;
begin
  Result := FSourceIndex.IndexOf(ASource);
  if Result <> -1
  then Result := PtrInt(FSourceIndex.Objects[Result]);
end;

constructor TGDBMILineInfo.Create(const ADebugger: TDebugger);
begin
  FSourceIndex := TStringList.Create;
  FSourceIndex.Sorted := True;
  FSourceIndex.Duplicates := dupError;
  FSourceIndex.CaseSensitive := False;
  FRequestedSources := TStringList.Create;
  FRequestedSources.Sorted := True;
  FRequestedSources.Duplicates := dupError;
  FRequestedSources.CaseSensitive := False;
  inherited;
end;

destructor TGDBMILineInfo.Destroy;
begin
  ClearSources;
  FreeAndNil(FSourceIndex);
  FreeAndNil(FRequestedSources);
  inherited Destroy;
end;

procedure TGDBMILineInfo.DoGetLineSymbolsFinished(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandLineSymbolInfo;
  idx: LongInt;
begin
  Cmd := TGDBMIDebuggerCommandLineSymbolInfo(Sender);
  if Cmd.Result.State <> dsError
  then
    AddInfo(Cmd.Source, Cmd.Result);

  idx := FRequestedSources.IndexOf(Cmd.Source);
  if idx >= 0
  then FRequestedSources.Delete(idx);

  FGetLineSymbolsCmdObj := nil;
  // DoChange is calle in AddInfo
end;

procedure TGDBMILineInfo.Request(const ASource: String);
var
  idx: Integer;
begin
  if (ASource = '') or (Debugger = nil) or (FRequestedSources.IndexOf(ASource) >= 0)
  then Exit;

  idx := IndexOf(ASource);
  if (idx <> -1) and (FSourceMaps[idx].Map <> nil) then Exit; // already present

  // add empty entry, to prevent further requests
  FRequestedSources.Add(ASource);

  // Need to interupt debugger
  if Debugger.State = dsRun
  then TGDBMIDebugger(Debugger).GDBPause(True);

  FGetLineSymbolsCmdObj := TGDBMIDebuggerCommandLineSymbolInfo.Create(TGDBMIDebugger(Debugger), ASource);
  FGetLineSymbolsCmdObj.OnExecuted := @DoGetLineSymbolsFinished;
  FGetLineSymbolsCmdObj.OnDestroy   := @DoGetLineSymbolsDestroyed;
  FGetLineSymbolsCmdObj.Priority := GDCMD_PRIOR_LINE_INFO;
  TGDBMIDebugger(Debugger).QueueCommand(FGetLineSymbolsCmdObj);
  (* DoEvaluationFinished may be called immediately at this point *)
end;


{ =========================================================================== }
{ TGDBMIDebuggerProperties }
{ =========================================================================== }

procedure TGDBMIDebuggerProperties.SetTimeoutForEval(const AValue: Integer);
begin
  if FTimeoutForEval = AValue then exit;
  FTimeoutForEval := AValue;
  if (FTimeoutForEval <> -1) and (FTimeoutForEval < 50)
  then FTimeoutForEval := -1;
end;

procedure TGDBMIDebuggerProperties.SetWarnOnTimeOut(const AValue: Boolean);
begin
  if FWarnOnTimeOut = AValue then exit;
  FWarnOnTimeOut := AValue;
end;

constructor TGDBMIDebuggerProperties.Create;
begin
  {$IFDEF UNIX}
  FConsoleTty := '';
  {$ENDIF}
  {$IFDEF darwin}
  FTimeoutForEval := 250;
  {$ELSE darwin}
  FTimeoutForEval := -1;
  {$ENDIF}
  FWarnOnTimeOut := True;
  FEncodeCurrentDirPath := gdfeDefault;
  FEncodeExeFileName := gdfeDefault;
  inherited;
end;

procedure TGDBMIDebuggerProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  FGDBOptions := TGDBMIDebuggerProperties(Source).FGDBOptions;
  {$IFDEF UNIX}
  FConsoleTty := TGDBMIDebuggerProperties(Source).FConsoleTty;
  {$ENDIF}
  FTimeoutForEval := TGDBMIDebuggerProperties(Source).FTimeoutForEval;
  FWarnOnTimeOut  := TGDBMIDebuggerProperties(Source).FWarnOnTimeOut;
  FEncodeCurrentDirPath := TGDBMIDebuggerProperties(Source).FEncodeCurrentDirPath;
  FEncodeExeFileName := TGDBMIDebuggerProperties(Source).FEncodeExeFileName;
end;


{ =========================================================================== }
{ TGDBMIDebugger }
{ =========================================================================== }

class function TGDBMIDebugger.Caption: String;
begin
  Result := 'GNU debugger (gdb)';
end;

function TGDBMIDebugger.ChangeFileName: Boolean;
var
  S: String;
  Cmd: TGDBMIDebuggerCommandChangeFilename;
begin
  Result := False;
  S := ConvertToGDBPath(UTF8ToSys(FileName), cgptExeName);

  Cmd := TGDBMIDebuggerCommandChangeFilename.Create(Self, S);
  Cmd.AddReference;
  QueueCommand(Cmd);
  // if filename = '', then command may be queued
  if (FileName <> '') and (not Cmd.Success) then begin
    MessageDlg('Debugger', Format('Failed to load file: %s', [Cmd.ErrorMsg]), mtError, [mbOK], 0);
    Cmd.Cancel;
    Cmd.ReleaseReference;
    SetState(dsStop);
  end
  else begin
    Cmd.ReleaseReference;
  end;

  if not (inherited ChangeFileName) then Exit;
  Result:=true;
end;

constructor TGDBMIDebugger.Create(const AExternalDebugger: String);
begin
  FReleaseLock := 0;

  FMainAddrBreak   := TGDBMIInternalBreakPoint.Create('main');
  FBreakErrorBreak := TGDBMIInternalBreakPoint.Create('FPC_BREAK_ERROR');
  FRunErrorBreak   := TGDBMIInternalBreakPoint.Create('FPC_RUNERROR');
  FExceptionBreak  := TGDBMIInternalBreakPoint.Create('FPC_RAISEEXCEPTION');

  FCommandQueue := TGDBMIDebuggerCommandList.Create;
  FTargetInfo.TargetPID := 0;
  FTargetInfo.TargetFlags := [];
  FDebuggerFlags := [];
  FSourceNames := TStringList.Create;
  FSourceNames.Sorted := True;
  FSourceNames.Duplicates := dupError;
  FSourceNames.CaseSensitive := False;
  FCommandQueueExecLock := 0;
  FRunQueueOnUnlock := False;
  FThreadGroups := TStringList.Create;
  FTypeRequestCache := TGDBPTypeRequestCache.Create;
  FMaxLineForUnitCache := TStringList.Create;
  FInProcessStopped := False;
  FNeedStateToIdle := False;


{$IFdef MSWindows}
  InitWin32;
{$ENDIF}
  {$IFDEF DBG_ENABLE_TERMINAL}
  FPseudoTerminal := TPseudoTerminal.Create;
  FPseudoTerminal.OnCanRead :=@DoPseudoTerminalRead;
  {$ENDIF}

  inherited;
end;

function TGDBMIDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TGDBMIBreakPoints.Create(Self, TGDBMIBreakPoint);
end;

function TGDBMIDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result := TGDBMICallStack.Create(Self);
end;

function TGDBMIDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result := TGDBMIDisassembler.Create(Self);
end;

function TGDBMIDebugger.CreateLocals: TLocalsSupplier;
begin
  Result := TGDBMILocals.Create(Self);
end;

function TGDBMIDebugger.CreateLineInfo: TDBGLineInfo;
begin
  Result := TGDBMILineInfo.Create(Self);
end;

class function TGDBMIDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TGDBMIDebuggerProperties.Create;
end;

function TGDBMIDebugger.CreateRegisters: TDBGRegisters;
begin
  Result := TGDBMIRegisters.Create(Self);
end;

function TGDBMIDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TGDBMIWatches.Create(Self);
end;

function TGDBMIDebugger.CreateThreads: TThreadsSupplier;
begin
  Result := TGDBMIThreads.Create(Self);
end;

destructor TGDBMIDebugger.Destroy;
begin
  LockRelease;
  inherited;
  ClearCommandQueue;
  FreeAndNil(FCommandQueue);
  ClearSourceInfo;
  FreeAndNil(FSourceNames);
  FreeAndNil(FThreadGroups);
  {$IFDEF DBG_ENABLE_TERMINAL}
  FreeAndNil(FPseudoTerminal);
  {$ENDIF}
  FreeAndNil(FTypeRequestCache);
  FreeAndNil(FMaxLineForUnitCache);
  FreeAndNil(FMainAddrBreak);
  FreeAndNil(FBreakErrorBreak);
  FreeAndNil(FRunErrorBreak);
  FreeAndNil(FExceptionBreak);
end;

procedure TGDBMIDebugger.Done;
begin
  if State = dsDestroying
  then begin
    ClearCommandQueue;
    inherited Done;
    exit;
  end;

  LockRelease;
  try
    CancelAllQueued;
    if (DebugProcess <> nil) and DebugProcess.Running then begin
      if State = dsRun then GDBPause(True);
      ExecuteCommand('-gdb-exit', [], []);
    end;
    inherited Done;
  finally
    UnlockRelease;
  end;
end;

function TGDBMIDebugger.GetLocation: TDBGLocationRec;
begin
  Result := FCurrentLocation;
end;

procedure TGDBMIDebugger.LockCommandProcessing;
begin
  // Keep a different counter than QueueExecuteLock
  // So we can detect, if RunQueue was blocked by this
  inc(FCommandProcessingLock);
end;

procedure TGDBMIDebugger.UnLockCommandProcessing;
{$IFDEF DBGMI_QUEUE_DEBUG}
var
  c: Boolean;
{$ENDIF}
begin
  dec(FCommandProcessingLock);
  if (FCommandProcessingLock = 0)
  and FRunQueueOnUnlock
  then begin
    FRunQueueOnUnlock := False;
    // if FCommandQueueExecLock, then queu will be run, by however has that lock
    if (FCommandQueueExecLock = 0)
    then begin
      {$IFDEF DBGMI_QUEUE_DEBUG}
      c := FCommandQueue.Count > 0;
      if c then DebugLnEnter(['TGDBMIDebugger.UnLockCommandProcessing: Execute RunQueue ']);
      {$ENDIF}
      RunQueue;
      {$IFDEF DBGMI_QUEUE_DEBUG}
      if c then DebugLnExit(['TGDBMIDebugger.UnLockCommandProcessing: Finished RunQueue']);
      {$ENDIF}
    end
  end;
end;

procedure TGDBMIDebugger.DoState(const OldState: TDBGState);
begin
  FTypeRequestCache.Clear;
  if not (State in [dsRun, dsPause, dsInit, dsInternalPause])
  then FMaxLineForUnitCache.Clear;

  if State in [dsStop, dsError]
  then begin
    ClearSourceInfo;
    FPauseWaitState := pwsNone;
    // clear un-needed commands
    if State = dsError
    then CancelAllQueued
    else CancelAfterStop;
  end;
  if (State = dsError) and (DebugProcessRunning) then begin
    SendCmdLn('kill'); // try to kill the debugged process. bypass all queues.
    DebugProcess.Terminate(0);
  end;
  if (OldState in [dsPause, dsInternalPause]) and (State = dsRun)
  then begin
    FPauseWaitState := pwsNone;
    {$IFDEF MSWindows}
    FPauseRequestInThreadID := 0;
    {$ENDIF}
  end;

  CallStack.CurrentCallStackList.EntriesForThreads[FCurrentThreadId].CurrentIndex := FCurrentStackFrame;

  inherited DoState(OldState);
end;

procedure TGDBMIDebugger.DoBeforeState(const OldState: TDBGState);
begin
  inherited DoBeforeState(OldState);
  Threads.CurrentThreads.CurrentThreadId := FCurrentThreadId; // TODO: Works only because CurrentThreadId is always valid
end;

procedure TGDBMIDebugger.DoReadError;
begin
  include(FErrorHandlingFlags, ehfGotReadError);
  if not(ehfDeferReadWriteError in FErrorHandlingFlags)
  then inherited DoReadError;
end;

procedure TGDBMIDebugger.DoWriteError;
begin
  include(FErrorHandlingFlags, ehfGotWriteError);
  if not(ehfDeferReadWriteError in FErrorHandlingFlags)
  then inherited DoWriteError;
end;

procedure TGDBMIDebugger.DoThreadChanged;
begin
  TGDBMICallstack(CallStack).DoThreadChanged;
  TGDBMIRegisters(Registers).Changed;
end;

procedure TGDBMIDebugger.DoRelease;
begin
  SetState(dsDestroying);
  if FReleaseLock > 0
  then exit;

  inherited DoRelease;
end;

procedure TGDBMIDebugger.AddThreadGroup(const S: String);
var
  List: TGDBMINameValueList;
begin
  List := TGDBMINameValueList.Create(S);
  FThreadGroups.Values[List.Values['id']] := List.Values['pid'];
  List.Free;
end;

procedure TGDBMIDebugger.RemoveThreadGroup(const S: String);
begin
  // Some gdb info contains thread group which are already exited => don't remove them
end;

function TGDBMIDebugger.ParseLibraryLoaded(const S: String): String;
const
  DebugInfo: array[Boolean] of String = ('No Debug Info', 'Has Debug Info');
var
  List: TGDBMINameValueList;
  ThreadGroup: String;
begin
  // input: =library-loaded,id="C:\\Windows\\system32\\ntdll.dll",target-name="C:\\Windows\\system32\\ntdll.dll",host-name="C:\\Windows\\system32\\ntdll.dll",symbols-loaded="0",thread-group="i1"
  List := TGDBMINameValueList.Create(S);
  ThreadGroup := List.Values['thread-group'];
  Result := Format('Module Load: "%s". %s. Thread Group: %s (%s)', [ConvertGdbPathAndFile(List.Values['id']), DebugInfo[List.Values['symbols-loaded'] = '1'], ThreadGroup, FThreadGroups.Values[ThreadGroup]]);
  List.Free;
end;

function TGDBMIDebugger.ParseLibraryUnLoaded(const S: String): String;
var
  List: TGDBMINameValueList;
  ThreadGroup: String;
begin
  // input: =library-unloaded,id="C:\\Windows\\system32\\advapi32.dll",target-name="C:\\Windows\\system32\\advapi32.dll",host-name="C:\\Windows\\system32\\advapi32.dll",thread-group="i1"
  List := TGDBMINameValueList.Create(S);
  ThreadGroup := List.Values['thread-group'];
  Result := Format('Module Unload: "%s". Thread Group: %s (%s)', [ConvertGdbPathAndFile(List.Values['id']), ThreadGroup, FThreadGroups.Values[ThreadGroup]]);
  List.Free;
end;

function TGDBMIDebugger.ParseThread(const S, EventText: String): String;
var
  List: TGDBMINameValueList;
  ThreadGroup: String;
begin
  if EventText = 'thread-created' then
    Result := 'Thread Start: '
  else
    Result := 'Thread Exit: ';
  List := TGDBMINameValueList.Create(S);
  ThreadGroup := List.Values['group-id'];
  Result := Result + Format('Thread ID: %s. Thread Group: %s (%s)', [List.Values['id'], ThreadGroup, FThreadGroups.Values[ThreadGroup]]);
  List.Free;
end;

procedure TGDBMIDebugger.DoNotifyAsync(Line: String);
var
  EventText: String;
begin
  EventText := GetPart(['='], [','], Line, False, False);
  case StringCase(EventText, [
    'shlibs-added',
    'library-loaded',
    'library-unloaded',
    'shlibs-updated',
    'thread-group-started',
    'thread-group-exited',
    'thread-created',
    'thread-exited'], False, False) of
    0: DoDbgEvent(ecModule, etModuleLoad, Line);
    1: DoDbgEvent(ecModule, etModuleLoad, ParseLibraryLoaded(Line));
    2: DoDbgEvent(ecModule, etModuleUnload, ParseLibraryUnloaded(Line));
    3: DoDbgEvent(ecModule, etDefault, Line);
    4: AddThreadGroup(Line);
    5: RemoveThreadGroup(Line);
    6: DoDbgEvent(ecThread, etThreadStart, ParseThread(Line, EventText));
    7: DoDbgEvent(ecThread, etThreadExit, ParseThread(Line, EventText));
  else
    DebugLn('[WARNING] Debugger: Unexpected async-record: ', Line);
  end;
end;

procedure TGDBMIDebugger.DoDbgBreakpointEvent(ABreakpoint: TDBGBreakPoint;
  Location: TDBGLocationRec; AReason: TGDBMIBreakpointReason;
  AOldVal: String = ''; ANewVal: String = '');
var
  SrcName, Msg: String;
  SrcLine: Integer;
begin
  SrcName := Location.SrcFullName;
  if SrcName = '' then
    SrcName := Location.SrcFile;
  if (SrcName = '') and (ABreakPoint <> nil) and (ABreakPoint.Kind = bpkSource) then
    SrcName := ABreakpoint.Source;
  SrcLine := Location.SrcLine;
  if (SrcLine < 1) and (ABreakPoint <> nil) and (ABreakPoint.Kind = bpkSource) then
    SrcLine := ABreakpoint.Line;

  if ABreakpoint = nil then begin
    Msg := Format('Unknown %s', [GDBMIBreakPointReasonNames[AReason]]);
    if AReason = gbrWatchTrigger then
      Msg := Msg + Format(' changed from "%s" to "%s"', [AOldVal, ANewVal]);
  end
  else begin
    case ABreakPoint.Kind of
      bpkSource:  Msg := 'Source Breakpoint';
      bpkAddress: Msg := 'Address Breakpoint';
      bpkData:
        begin
          if AReason = gbrWatchScope then
            Msg := Format('Watchpoint for "%s" out of scope', [ABreakpoint.WatchData])
          else
            Msg := Format('Watchpoint for "%s" was triggered. Old value "%s", New Value "%s"', [ABreakpoint.WatchData, AOldVal, ANewVal]);
        end;
    end;
  end;

  if SrcName <> '' then begin
    DoDbgEvent(ecBreakpoint, etBreakpointHit,
               Format('%s at $%.' + IntToStr(TargetPtrSize * 2) + 'x: %s line %d',
                      [Msg, Location.Address, SrcName, SrcLine]));
  end
  else begin
    DoDbgEvent(ecBreakpoint, etBreakpointHit,
               Format('%s at $%.' + IntToStr(TargetPtrSize * 2) + 'x',
                      [Msg, Location.Address]));
  end;

end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AValues: array of const; const AFlags: TGDBMICmdFlags): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommandFull(ACommand, AValues, AFlags, nil, 0, R);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AValues: array of const; const AFlags: TGDBMICmdFlags;
  var AResult: TGDBMIExecResult): Boolean;
begin
  Result := ExecuteCommandFull(ACommand, AValues, AFlags, nil, 0, AResult);
end;

function TGDBMIDebugger.ExecuteCommandFull(const ACommand: String;
  const AValues: array of const; const AFlags: TGDBMICmdFlags;
  const ACallback: TGDBMICallback; const ATag: PtrInt;
  var AResult: TGDBMIExecResult): Boolean;
var
  CommandObj: TGDBMIDebuggerSimpleCommand;
begin
  CommandObj := TGDBMIDebuggerSimpleCommand.Create(Self, ACommand, AValues, AFlags, ACallback, ATag);
  CommandObj.AddReference;
  QueueCommand(CommandObj);
  Result := CommandObj.State in [dcsExecuting, dcsFinished];
  if Result
  then
    AResult := CommandObj.Result;
  CommandObj.ReleaseReference;
end;

procedure TGDBMIDebugger.RunQueue;
var
  R: Boolean;
  Cmd, NestedCurrentCmd, NestedCurrentCmdTmp: TGDBMIDebuggerCommand;
  SavedInExecuteCount: LongInt;
begin
  if FCommandQueue.Count = 0
  then exit;

  if FCommandProcessingLock > 0
  then begin
    FRunQueueOnUnlock := True;
    exit
  end;

  // Safeguard the NestLvl and outer CurrrentCmd
  SavedInExecuteCount := FInExecuteCount;
  NestedCurrentCmd := FCurrentCommand;
  LockRelease;
  try
    repeat
      Cmd := FCommandQueue[0];
      if (Cmd.QueueRunLevel >= 0) and (Cmd.QueueRunLevel < FInExecuteCount)
      then break;

      Inc(FInExecuteCount);

      FCommandQueue.Delete(0);
      {$IFDEF DBGMI_QUEUE_DEBUG}
      DebugLnEnter(['Executing (Recurse-Count=', FInExecuteCount-1, ') queued= ', FCommandQueue.Count, ' CmdPrior=', Cmd.Priority,' CmdMinRunLvl=', Cmd.QueueRunLevel, ' : "', Cmd.DebugText,'" State=',DBGStateNames[State],' PauseWaitState=',ord(FPauseWaitState) ]);
      {$ENDIF}
      // cmd may be canceled while executed => don't loose it while working with it
      Cmd.AddReference;
      NestedCurrentCmdTmp := FCurrentCommand;
      FCurrentCommand := Cmd;
      // excute, has it's own try-except block => so we don't have one here
      R := Cmd.Execute;
      Cmd.DoFinished;
      FCurrentCommand := NestedCurrentCmdTmp;
      Cmd.ReleaseReference;
      {$IFDEF DBGMI_QUEUE_DEBUG}
      DebugLnExit('Exec done');
      {$ENDIF}

      Dec(FInExecuteCount);
      // Do not add code with callbacks outside "FInExecuteCount"
      // Otherwhise "LockCommandProcessing" will fail to continue the queue

      // TODO: if the debugger can accept them into a separate queue, the set stae here
      // TODO: For now do not allow new session, before old session is finished
      // There may already be commands for the next run queued,
      // which will then set a new state.
      //if FNeedStateToIdle and (FInExecuteCount = 0)
      //then ResetStateToIdle;

      if State in [dsError, dsDestroying]
      then begin
        //DebugLn('[WARNING] TGDBMIDebugger:  ExecuteCommand "',Cmd,'" failed.');
        Break;
      end;

      if  FCommandQueue.Count = 0
      then begin
        if  (FInExecuteCount = 0)                        // not in Recursive call
        and (FPauseWaitState = pwsInternal)
        and (State = dsRun)
        then begin
          // reset state
          FPauseWaitState := pwsNone;
          // insert continue command
          Cmd := TGDBMIDebuggerCommandExecute.Create(Self, ectContinue);
          FCommandQueue.Add(Cmd);
          {$IFDEF DBGMI_QUEUE_DEBUG}
          debugln(['Internal Queueing: exec-continue']);
          {$ENDIF}
        end
        else Break; // Queue empty
      end;
    until not R;
    {$IFDEF DBGMI_QUEUE_DEBUG}
    debugln(['Leaving Queue with count: ', FCommandQueue.Count, ' Recurse-Count=', FInExecuteCount,' State=',DBGStateNames[State]]);
    {$ENDIF}
  finally
    UnlockRelease;
    FInExecuteCount := SavedInExecuteCount;
    FCurrentCommand := NestedCurrentCmd;
  end;

  if (FCommandQueue.Count = 0) and assigned(OnIdle)
  then OnIdle(Self);

  if FNeedStateToIdle and (FInExecuteCount = 0)
  then ResetStateToIdle;
end;

procedure TGDBMIDebugger.QueueCommand(const ACommand: TGDBMIDebuggerCommand; ForceQueue: Boolean = False);
var
  i, p: Integer;
  CanRunQueue: Boolean;
begin
  (* TODO: if an exec-command is queued, cancel watches-commands, etc (unless required for snapshot)
     This may occur if multiply exe are queued.
     Currently, they will be ForcedQueue, and end up, after the exec command => cancel by state change
  *)


  p := ACommand.Priority;
  i := 0;
  // CanRunQueue: The queue can be run for "ACommand"
  //  Either the queue is empty (so no other command will run)
  //  Or the first command on the queue is blocked by "QueueRunLevel"
  CanRunQueue := (FCommandQueue.Count = 0)
    or ( (FCommandQueue.Count > 0)
        and (FCommandQueue[0].QueueRunLevel >= 0)
        and (FCommandQueue[0].QueueRunLevel < FInExecuteCount)
       );

  if (ACommand is TGDBMIDebuggerCommandExecute) then begin
    // Execute-commands, must be queued at the end. They have QueueRunLevel, so they only run in the outer loop
    CanRunQueue := (FCommandQueue.Count = 0);
    i := FCommandQueue.Add(ACommand);
  end
  else
  if p > 0 then begin
    // Queue Pririty commands
    // TODO: check for "CanRunQueue": should be at start?
    while (i < FCommandQueue.Count)
    and (FCommandQueue[i].Priority >= p)
    and ( (ForceQueue)
       or (FCommandQueue[i].QueueRunLevel < 0)
       or (FCommandQueue[i].QueueRunLevel >= FInExecuteCount)
        )
    do inc(i);
    FCommandQueue.Insert(i, ACommand);
  end
  else begin
    // Queue normal commands
    if (not ForceQueue) and (FCommandQueue.Count > 0)
    and CanRunQueue  // first item is deferred, so new item inserted can run
    then
      FCommandQueue.Insert(0, ACommand)
    else
      i := FCommandQueue.Add(ACommand);
  end;

  // if other commands do run the queue,
  // make sure this command only runs after the CurrentCommand finished
  if ForceQueue and
    ( (ACommand.QueueRunLevel < 0) or (ACommand.QueueRunLevel >= FInExecuteCount) )
  then
    ACommand.QueueRunLevel := FInExecuteCount - 1;

  if (not CanRunQueue) or (FCommandQueueExecLock > 0)
  or (FCommandProcessingLock > 0) or ForceQueue
  then begin
    {$IFDEF DBGMI_QUEUE_DEBUG}
    debugln(['Queueing (Recurse-Count=', FInExecuteCount, ') at pos=', i, ' cnt=',FCommandQueue.Count-1, ' State=',DBGStateNames[State], ' Lock=',FCommandQueueExecLock, ' Forced=', dbgs(ForceQueue), ' Prior=',p, ': "', ACommand.DebugText,'"']);
    {$ENDIF}
    ACommand.DoQueued;

    // FCommandProcessingLock still must call RunQueue
    if FCommandProcessingLock = 0 then
      Exit;
  end;

  // If we are here we can process the command directly
  RunQueue;
end;

procedure TGDBMIDebugger.UnQueueCommand(const ACommand: TGDBMIDebuggerCommand);
begin
  FCommandQueue.Remove(ACommand);
end;

procedure TGDBMIDebugger.CancelAllQueued;
var
  i: Integer;
begin
  i := FCommandQueue.Count - 1;
  while i >= 0 do begin
    TGDBMIDebuggerCommand(FCommandQueue[i]).Cancel;
    dec(i);
    if i >= FCommandQueue.Count
    then i := FCommandQueue.Count - 1;
  end;
  if FCurrentCommand <> nil
  then FCurrentCommand.Cancel;
end;

procedure TGDBMIDebugger.CancelBeforeRun;
var
  i: Integer;
begin
  i := FCommandQueue.Count - 1;
  while i >= 0 do begin
    if dcpCancelOnRun in TGDBMIDebuggerCommand(FCommandQueue[i]).Properties
    then TGDBMIDebuggerCommand(FCommandQueue[i]).Cancel;
    dec(i);
    if i >= FCommandQueue.Count
    then i := FCommandQueue.Count - 1;
  end;
  if (FCurrentCommand <> nil) and (dcpCancelOnRun in FCurrentCommand.Properties)
  then FCurrentCommand.Cancel;
end;

procedure TGDBMIDebugger.CancelAfterStop;
var
  i: Integer;
begin
  i := FCommandQueue.Count - 1;
  while i >= 0 do begin
    if TGDBMIDebuggerCommand(FCommandQueue[i]) is TGDBMIDebuggerCommandExecute
    then TGDBMIDebuggerCommand(FCommandQueue[i]).Cancel;
    dec(i);
    if i >= FCommandQueue.Count
    then i := FCommandQueue.Count - 1;
  end;
  // do not cancel FCurrentCommand;
end;

class function TGDBMIDebugger.ExePaths: String;
begin
  {$IFdef MSWindows}
  Result := '$(LazarusDir)\mingw\bin\gdb.exe;C:\lazarus\mingw\bin\gdb.exe';
  {$ELSE}
  Result := '/usr/bin/gdb;/usr/local/bin/gdb;/opt/fpc/gdb';
  {$ENDIF}
end;

function TGDBMIDebugger.FindBreakpoint(
  const ABreakpoint: Integer): TDBGBreakPoint;
var
  n: Integer;
begin
  if  ABreakpoint > 0
  then
    for n := 0 to Breakpoints.Count - 1 do
    begin
      Result := Breakpoints[n];
      if TGDBMIBreakPoint(Result).FBreakID = ABreakpoint
      then Exit;
    end;
  Result := nil;
end;

function PosSetEx(const ASubStrSet, AString: string;
  const Offset: integer): integer;
begin
  for Result := Offset to Length(AString) do
    if Pos(AString[Result], ASubStrSet) > 0 then
      exit;
  Result := 0;
end;

function EscapeGDBCommand(const AInput: string): string;
var
  lPiece: string;
  I, lPos, len: integer;
begin
  lPos := 1;
  Result := '';
  repeat
    I := PosSetEx(#9#10#13, AInput, lPos);
    { copy unmatched characters }
    if I > 0 then
      len := I-lPos
    else
      len := Length(AInput)+1-lPos;
    Result := Result + Copy(AInput, lPos, len);
    { replace a matched character or be done }
    if I > 0 then
    begin
      case AInput[I] of
        #9:  lPiece := '\t';
        #10: lPiece := '\n';
        #13: lPiece := '\r';
      else
        lPiece := '';
      end;
      Result := Result + lPiece;
      lPos := I+1;
    end else
      exit;
  until false;
end;

function TGDBMIDebugger.GDBDisassemble(AAddr: TDbgPtr; ABackward: Boolean;
  out ANextAddr: TDbgPtr; out ADump, AStatement, AFile: String; out ALine: Integer): Boolean;
var
  NewEntryMap: TDBGDisassemblerEntryMap;
  CmdObj: TGDBMIDebuggerCommandDisassembe;
  Rng: TDBGDisassemblerEntryRange;
  i: Integer;
begin
  NewEntryMap := TDBGDisassemblerEntryMap.Create(itu8, SizeOf(TDBGDisassemblerEntryRange));
  CmdObj := TGDBMIDebuggerCommandDisassembe.Create(Self, NewEntryMap, AAddr, AAddr, -1, 2);
  CmdObj.AddReference;
  CmdObj.Priority := GDCMD_PRIOR_IMMEDIATE;
  QueueCommand(CmdObj);
  Result := CmdObj.State in [dcsExecuting, dcsFinished];

  Rng := NewEntryMap.GetRangeForAddr(AAddr);
  if Result and (Rng <> nil)
  then begin
    i := Rng.IndexOfAddr(AAddr);
    if ABackward
    then dec(i);

    if
    i >= 0
    then begin
      if i < Rng.Count
      then ANextAddr := Rng.EntriesPtr[i]^.Addr
      else ANextAddr := Rng.LastEntryEndAddr;

      ADump := Rng.EntriesPtr[i]^.Dump;
      AStatement := Rng.EntriesPtr[i]^.Statement;
      AFile := Rng.EntriesPtr[i]^.SrcFileName;
      ALine := Rng.EntriesPtr[i]^.SrcFileLine;
    end;
  end;

  if not Result
  then CmdObj.Cancel;

  CmdObj.ReleaseReference;
  FreeAndNil(NewEntryMap);
end;

procedure TGDBMIDebugger.DoPseudoTerminalRead(Sender: TObject);
begin
  {$IFDEF DBG_ENABLE_TERMINAL}
  if assigned(OnConsoleOutput)
  then OnConsoleOutput(self, FPseudoTerminal.Read);
  {$ENDIF}
end;

function TGDBMIDebugger.GDBEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
var
  S: String;
begin
  Result := True;

  if State = dsRun
  then GDBPause(True);
  if ASet then
  begin
    S := EscapeGDBCommand(AVariable);
    ExecuteCommand('-gdb-set env %s', [S], [cfIgnoreState, cfExternal]);
  end else begin
    S := AVariable;
    ExecuteCommand('unset env %s', [GetPart([], ['='], S, False, False)], [cfNoMiCommand, cfIgnoreState, cfExternal]);
  end;
end;

function TGDBMIDebugger.GDBEvaluate(const AExpression: String; var AResult: String;
  out ATypeInfo: TGDBType; EvalFlags: TDBGEvaluateFlags): Boolean;
var
  CommandObj: TGDBMIDebuggerCommandEvaluate;
begin
  CommandObj := TGDBMIDebuggerCommandEvaluate.Create(Self, AExpression, wdfDefault);
  CommandObj.EvalFlags := EvalFlags;
  CommandObj.AddReference;
  CommandObj.Priority := GDCMD_PRIOR_IMMEDIATE; // try run imediately
  QueueCommand(CommandObj);
  Result := CommandObj.State in [dcsExecuting, dcsFinished];
  AResult := CommandObj.TextValue;
  ATypeInfo := CommandObj.TypeInfo;
  if EvalFlags * [defNoTypeInfo, defSimpleTypeInfo, defFullTypeInfo] = [defNoTypeInfo]
  then FreeAndNil(ATypeInfo);
  CommandObj.ReleaseReference;
end;

function TGDBMIDebugger.GDBModify(const AExpression, ANewValue: String): Boolean;
var
  R: TGDBMIExecResult;
  S: String;
begin
  S := Trim(ANewValue);
  if (S <> '') and (S[1] in ['''', '#'])
  then begin
    if not ConvertPascalExpression(S) then Exit(False);
  end;

  Result := ExecuteCommand('-gdb-set var %s := %s', [AExpression, S], [cfIgnoreError, cfExternal], R)
        and (R.State <> dsError);

  TGDBMILocals(Locals).Changed;
  TGDBMIWatches(Watches).Changed;
  FTypeRequestCache.Clear;
end;

function TGDBMIDebugger.GDBJumpTo(const ASource: String; const ALine: Integer): Boolean;
begin
  Result := False;
end;

function TGDBMIDebugger.GDBPause(const AInternal: Boolean): Boolean;
begin
  if FInProcessStopped then exit;

  // Check if we already issued a break
  if FPauseWaitState = pwsNone
  then InterruptTarget;

  if AInternal
  then begin
    if FPauseWaitState = pwsNone
    then FPauseWaitState := pwsInternal;
  end
  else FPauseWaitState := pwsExternal;

  Result := True;
end;

function TGDBMIDebugger.GDBRun: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      FThreadGroups.Clear;
      Result := StartDebugging(ectContinue);
    end;
    dsPause: begin
      CancelBeforeRun;
      QueueCommand(TGDBMIDebuggerCommandExecute.Create(Self, ectContinue));
      Result := True;
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to run in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBRunTo(const ASource: String;
  const ALine: Integer): Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging(ectRunTo, [ASource, ALine]);
    end;
    dsPause: begin
      CancelBeforeRun;
      QueueCommand(TGDBMIDebuggerCommandExecute.Create(Self, ectRunTo, [ASource, ALine]));
      Result := True;
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to runto in idle state');
    end;
  end;

end;

function TGDBMIDebugger.GDBSourceAdress(const ASource: String; ALine, AColumn: Integer; out AAddr: TDbgPtr): Boolean;
var
  ID: packed record
    Line, Column: Integer;
  end;
  Map: TMap;
  idx, n: Integer;
  R: TGDBMIExecResult;
  LinesList, LineList: TGDBMINameValueList;
  Item: PGDBMINameValue;
  Addr: TDbgPtr;
begin
  Result := False;
  AAddr := 0;
  if ASource = ''
  then Exit;
  idx := FSourceNames.IndexOf(ASource);
  if (idx <> -1)
  then begin
    Map := TMap(FSourceNames.Objects[idx]);
    ID.Line := ALine;
    // since we do not have column info we map all on column 0
    // ID.Column := AColumn;
    ID.Column := 0;
    Result := (Map <> nil);
    if Result
    then Map.GetData(ID, AAddr);
    Exit;
  end;

  Result := ExecuteCommand('-symbol-list-lines %s', [ASource], [cfIgnoreError, cfExternal], R)
        and (R.State <> dsError);
  // if we have an .inc file then search for filename only since there are some
  // problems with locating file by full path in gdb in case only relative file
  // name is stored
  if not Result then
    Result := ExecuteCommand('-symbol-list-lines %s', [ExtractFileName(ASource)], [cfIgnoreError, cfExternal], R)
          and (R.State <> dsError);

  if not Result then Exit;

  Map := TMap.Create(its8, SizeOf(AAddr));
  FSourceNames.AddObject(ASource, Map);

  LinesList := TGDBMINameValueList.Create(R, ['lines']);
  if LinesList = nil then Exit(False);

  ID.Column := 0;
  LineList := TGDBMINameValueList.Create('');

  for n := 0 to LinesList.Count - 1 do
  begin
    Item := LinesList.Items[n];
    LineList.Init(Item^.Name);
    if not TryStrToInt(Unquote(LineList.Values['line']), ID.Line) then Continue;
    if not TryStrToQWord(Unquote(LineList.Values['pc']), Addr) then Continue;
    // one line can have more than one address
    if Map.HasId(ID) then Continue;
    Map.Add(ID, Addr);
    if ID.Line = ALine
    then AAddr := Addr;
  end;
  LineList.Free;
  LinesList.Free;
end;

procedure TGDBMIDebugger.LockRelease;
begin
  inc(FReleaseLock);
end;

procedure TGDBMIDebugger.UnlockRelease;
begin
  dec(FReleaseLock);
  if (FReleaseLock = 0) and (State = dsDestroying)
  then Release;
end;

function TGDBMIDebugger.GDBStepInto: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging;
    end;
    dsPause: begin
      CancelBeforeRun;
      QueueCommand(TGDBMIDebuggerCommandExecute.Create(Self, ectStepInto));
      Result := True;
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to step in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBStepOverInstr: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging;
    end;
    dsPause: begin
      CancelBeforeRun;
      QueueCommand(TGDBMIDebuggerCommandExecute.Create(Self, ectStepOverInstruction));
      Result := True;
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to step over instr in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBStepIntoInstr: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging;
    end;
    dsPause: begin
      CancelBeforeRun;
      QueueCommand(TGDBMIDebuggerCommandExecute.Create(Self, ectStepIntoInstruction));
      Result := True;
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to step in instr idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBStepOut: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging;
    end;
    dsPause: begin
      CancelBeforeRun;
      QueueCommand(TGDBMIDebuggerCommandExecute.Create(Self, ectStepOut));
      Result := True;
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to step out in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBStepOver: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging;
    end;
    dsPause: begin
      CancelBeforeRun;
      QueueCommand(TGDBMIDebuggerCommandExecute.Create(Self, ectStepOver));
      Result := True;
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to step over in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBStop: Boolean;
begin
  if State = dsError
  then begin
    // We don't know the state of the debugger,
    // force a reinit. Let's hope this works.
    DebugProcess.Terminate(0);
    Done;
    Result := True;
    Exit;
  end;

  if (FCurrentCommand is TGDBMIDebuggerCommandExecute)
  and TGDBMIDebuggerCommandExecute(FCurrentCommand).KillNow
  then begin
    {$IFDEF DBG_VERBOSE}
    debugln(['KillNow did stop']);
    {$ENDIF}
    Result := True;
    exit;
  end;

  if State = dsRun
  then GDBPause(True);

  CancelAllQueued;
  QueueCommand(TGDBMIDebuggerCommandKill.Create(Self));
  Result := True;
end;

function TGDBMIDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcStepOut,
             dcStepOverInstr, dcStepIntoInstr, dcRunTo, dcJumpto,
             dcBreak, dcWatch, dcLocal, dcEvaluate, dcModify, dcEnvironment,
             dcSetStackFrame, dcDisassemble
             {$IFDEF DBG_ENABLE_TERMINAL}, dcSendConsoleInput{$ENDIF}
            ];
end;

function TGDBMIDebugger.GetCommands: TDBGCommands;
begin
  if FNeedStateToIdle
  then Result := []
  else Result := inherited GetCommands;
end;

function TGDBMIDebugger.GetTargetWidth: Byte;
begin
  Result := FTargetInfo.TargetPtrSize*8;
end;

procedure TGDBMIDebugger.Init;

  procedure CheckGDBVersion;
  begin
    if FGDBVersion < '5.3'
    then begin
      DebugLn('[WARNING] Debugger: Running an old (< 5.3) GDB version: ', FGDBVersion);
      DebugLn('                    Not all functionality will be supported.');
    end
    else begin
      DebugLn('[Debugger] Running GDB version: ', FGDBVersion);
      Include(FDebuggerFlags, dfImplicidTypes);
    end;
  end;

var
  Options: String;
  Cmd: TGDBMIDebuggerCommandInitDebugger;
begin
  Exclude(FDebuggerFlags, dfForceBreakDetected);
  LockRelease;
  try
    FPauseWaitState := pwsNone;
    FErrorHandlingFlags := [];
    FInExecuteCount := 0;
    FNeedStateToIdle := False;
    Options := '-silent -i mi -nx';

    if Length(TGDBMIDebuggerProperties(GetProperties).Debugger_Startup_Options) > 0
    then Options := Options + ' ' + TGDBMIDebuggerProperties(GetProperties).Debugger_Startup_Options;

    if CreateDebugProcess(Options)
    then begin
      if not ParseInitialization
      then begin
        SetState(dsError);
      end
      else begin
        Cmd := TGDBMIDebuggerCommandInitDebugger.Create(Self);
        Cmd.AddReference;
        QueueCommand(Cmd);
        if not Cmd.Success then begin
          Cmd.Cancel;
          Cmd.ReleaseReference;
          SetState(dsError);
        end
        else begin
          Cmd.ReleaseReference;
          CheckGDBVersion;
          inherited Init;
        end;
      end;
    end
    else begin
      if DebugProcess = nil
      then MessageDlg('Debugger', 'Failed to create debug process for unknown reason', mtError, [mbOK], 0)
      else MessageDlg('Debugger', Format('Failed to create debug process: %s', [ReadLine]), mtError, [mbOK], 0);
      SetState(dsError);
    end;

    FGDBPtrSize := CpuNameToPtrSize(FGDBCPU); // will be set in StartDebugging
  finally
    UnlockRelease;
  end;
end;

procedure TGDBMIDebugger.InterruptTarget;
{$IFdef MSWindows}
  function TryNT: Boolean;
  var
    hProcess: THandle;
    hThread: THandle;
    E: Integer;
    Emsg: PChar;
  begin
    Result := False;

    hProcess := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, TargetPID);
    if hProcess = 0 then Exit;

    try
      hThread := _CreateRemoteThread(hProcess, nil, 0, DebugBreakAddr, nil, 0, FPauseRequestInThreadID);
      if hThread = 0
      then begin
        E := GetLastError;
        FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER, nil, E, 0, PChar(@Emsg), 0, nil);
        DebugLN('Error creating remote thread: ' + String(EMsg));
        // Yuck !
        // mixing handles and pointers, but it is how MS documented it
        LocalFree(HLOCAL(Emsg));
        Exit;
      end;
      Result := True;
      CloseHandle(hThread);
    finally
      CloseHandle(hProcess);
    end;
  end;
{$ENDIF}
begin
  {$IFDEF DBGMI_QUEUE_DEBUG}
  DebugLn(['TGDBMIDebugger.InterruptTarget: TargetPID=', TargetPID]);
  {$ENDIF}
  if TargetPID = 0 then Exit;
{$IFDEF UNIX}
  FpKill(TargetPID, SIGINT);
{$ENDIF}

{$IFdef MSWindows}
  // GenerateConsoleCtrlEvent is nice, but only works if both gdb and
  // our target have a console. On win95 and family this is our only
  // option, on NT4+ we have a choice. Since this is not likely that
  // we have a console, we do it the hard way. On XP there exists
  // DebugBreakProcess, but it does efectively the same.

  if (DebugBreakAddr = nil)
  or not Assigned(_CreateRemoteThread)
  or not TryNT
  then begin
    // We have no other choice than trying this
    {$IFDEF DBGMI_QUEUE_DEBUG}
    DebugLn(['TGDBMIDebugger.InterruptTarget: Send CTRL_BREAK_EVENT']);
    {$ENDIF}
    GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, TargetPID);
    Exit;
  end;
{$ENDIF}
end;

function TGDBMIDebugger.ParseInitialization: Boolean;
var
  Line, S: String;
begin
  Result := True;

  // Get initial debugger lines
  S := '';
  Line := ReadLine;
  while DebugProcessRunning and (Line <> '(gdb) ') do
  begin
    if Line <> ''
    then
      case Line[1] of
        '=': begin
          case StringCase(GetPart(['='], [','], Line, False, False),
            ['thread-group-added'])
          of
            0: {ignore};
          else
            S := S + Line + LineEnding;
          end;
        end;
      else
        S := S + Line + LineEnding;
      end;
    Line := ReadLine;
  end;
  if S <> ''
  then MessageDlg('Debugger', 'Initialization output: ' + LineEnding + S,
    mtInformation, [mbOK], 0);
end;

function TGDBMIDebugger.RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean;
var
  EvalFlags: TDBGEvaluateFlags;
begin
  LockRelease;
  try
    case ACommand of
      dcRun:         Result := GDBRun;
      dcPause:       Result := GDBPause(False);
      dcStop:        Result := GDBStop;
      dcStepOver:    Result := GDBStepOver;
      dcStepInto:    Result := GDBStepInto;
      dcStepOut:     Result := GDBStepOut;
      dcRunTo:       Result := GDBRunTo(String(AParams[0].VAnsiString), AParams[1].VInteger);
      dcJumpto:      Result := GDBJumpTo(String(AParams[0].VAnsiString), AParams[1].VInteger);
      dcEvaluate:    begin
                       EvalFlags := [];
                       if high(AParams) >= 3 then
                         EvalFlags := TDBGEvaluateFlags(AParams[3].VInteger);
                       Result := GDBEvaluate(String(AParams[0].VAnsiString),
                         String(AParams[1].VPointer^), TGDBType(AParams[2].VPointer^),
                         EvalFlags);
                     end;
      dcModify:      Result := GDBModify(String(AParams[0].VAnsiString), String(AParams[1].VAnsiString));
      dcEnvironment: Result := GDBEnvironment(String(AParams[0].VAnsiString), AParams[1].VBoolean);
      dcDisassemble: Result := GDBDisassemble(AParams[0].VQWord^, AParams[1].VBoolean, TDbgPtr(AParams[2].VPointer^),
                                              String(AParams[3].VPointer^), String(AParams[4].VPointer^),
                                              String(AParams[5].VPointer^), Integer(AParams[6].VPointer^));
      dcStepOverInstr: Result := GDBStepOverInstr;
      dcStepIntoInstr: Result := GDBStepIntoInstr;
      {$IFDEF DBG_ENABLE_TERMINAL}
      dcSendConsoleInput: FPseudoTerminal.Write(String(AParams[0].VAnsiString));
      {$ENDIF}
    end;
  finally
    UnlockRelease;
  end;
end;

procedure TGDBMIDebugger.ClearCommandQueue;
var
  i: Integer;
begin
  for i:=0 to FCommandQueue.Count-1 do begin
    TGDBMIDebuggerCommand(FCommandQueue[i]).ReleaseReference;
  end;
  FCommandQueue.Clear;
end;

function TGDBMIDebugger.GetIsIdle: Boolean;
begin
  Result := (FCommandQueue.Count = 0) and (State in [dsPause, dsInternalPause]);
end;

procedure TGDBMIDebugger.ResetStateToIdle;
begin
  if FInExecuteCount > 0 then begin
    {$IFDEF DBGMI_QUEUE_DEBUG}
    debugln(['Defer dsIdle:  Recurse-Count=', FInExecuteCount]);
    {$ENDIF}
    FNeedStateToIdle := True;
    exit;
  end;
  FNeedStateToIdle := False;
  inherited ResetStateToIdle;
end;

procedure TGDBMIDebugger.ClearSourceInfo;
var
  n: Integer;
begin
  for n := 0 to FSourceNames.Count - 1 do
    FSourceNames.Objects[n].Free;

  FSourceNames.Clear;
end;

function TGDBMIDebugger.ConvertPascalExpression(var AExpression: String): Boolean;
var
  R: String;
  P: PChar;
  InString, WasString, IsText, ValIsChar: Boolean;
  n: Integer;
  ValMode: Char;
  Value: QWord;

  function AppendValue: Boolean;
  var
    S: String;
  begin
    if ValMode = #0 then Exit(True);
    if not (ValMode in ['h', 'd', 'o', 'b']) then Exit(False);

    if ValIsChar
    then begin
      if not IsText
      then begin
        R := R + '"';
        IsText := True;
      end;
      R := R + '\' + OctStr(Value, 3);
      ValIsChar := False;
    end
    else begin
      if IsText
      then begin
        R := R + '"';
        IsText := False;
      end;
      Str(Value, S);
      R := R + S;
    end;
    Result := True;
    ValMode := #0;
  end;

begin
  R := '';
  Instring := False;
  WasString := False;
  IsText := False;
  ValIsChar := False;
  ValMode := #0;
  Value := 0;

  P := PChar(AExpression);
  for n := 1 to Length(AExpression) do
  begin
    if InString
    then begin
      case P^ of
        '''': begin
          InString := False;
          // delay setting terminating ", more characters defined through # may follow
          WasString := True;
        end;
        #0..#31,
        '"', '\',
        #128..#255: begin
          R := R + '\' + OctStr(Ord(P^), 3);
        end;
      else
        R := R + P^;
      end;
      Inc(P);
      Continue;
    end;

    case P^ of
      '''': begin
        if WasString
        then begin
          R := R + '\' + OctStr(Ord(''''), 3)
        end
        else begin
          if not AppendValue then Exit(False);
          if not IsText
          then R := R + '"';
        end;
        IsText := True;
        InString := True;
      end;
      '#': begin
        if not AppendValue then Exit(False);
        Value := 0;
        ValMode := 'D';
        ValIsChar := True;
      end;
      '$', '&', '%': begin
        if not (ValMode in [#0, 'D']) then Exit(False);
        ValMode := P^;
      end;
    else
      case ValMode of
        'D', 'd': begin
          case P^ of
            '0'..'9': Value := Value * 10 + Ord(P^) - Ord('0');
          else
            Exit(False);
          end;
          ValMode := 'd';
        end;
        '$', 'h': begin
          case P^ of
            '0'..'9': Value := Value * 16 + Ord(P^) - Ord('0');
            'a'..'f': Value := Value * 16 + Ord(P^) - Ord('a');
            'A'..'F': Value := Value * 16 + Ord(P^) - Ord('A');
          else
            Exit(False);
          end;
          ValMode := 'h';
        end;
        '&', 'o': begin
          case P^ of
            '0'..'7': Value := Value * 8 + Ord(P^) - Ord('0');
          else
            Exit(False);
          end;
          ValMode := 'o';
        end;
        '%', 'b': begin
          case P^ of
            '0': Value := Value shl 1;
            '1': Value := Value shl 1 or 1;
          else
            Exit(False);
          end;
          ValMode := 'b';
        end;
      else
        if IsText
        then begin
          R := R + '"';
          IsText := False;
        end;
        R := R + P^;
      end;
    end;
    WasString := False;
    Inc(p);
  end;

  if not AppendValue then Exit(False);
  if IsText then R := R + '"';
  AExpression := R;
  Result := True;
end;

function TGDBMIDebugger.StartDebugging(AContinueCommand: TGDBMIExecCommandType): Boolean;
begin
  Result := StartDebugging(TGDBMIDebuggerCommandExecute.Create(Self, AContinueCommand));
end;

function TGDBMIDebugger.StartDebugging(AContinueCommand: TGDBMIExecCommandType;
  AValues: array of const): Boolean;
begin
  Result := StartDebugging(TGDBMIDebuggerCommandExecute.Create(Self, AContinueCommand, AValues));
end;

function TGDBMIDebugger.StartDebugging(AContinueCommand: TGDBMIDebuggerCommand = nil): Boolean;
var
  Cmd: TGDBMIDebuggerCommandStartDebugging;
begin
  // We expect to be run immediately, no queue
  Cmd := TGDBMIDebuggerCommandStartDebugging.Create(Self, AContinueCommand);
  Cmd.AddReference;
  QueueCommand(Cmd);
  Result := Cmd.Success;
  if not Result
  then Cmd.Cancel;
  Cmd.ReleaseReference;
end;

{$IFDEF DBG_ENABLE_TERMINAL}
procedure TGDBMIDebugger.ProcessWhileWaitForHandles;
begin
  inherited ProcessWhileWaitForHandles;
  FPseudoTerminal.CheckCanRead;
end;
{$ENDIF}

procedure TGDBMIDebugger.QueueExecuteLock;
begin
  inc(FCommandQueueExecLock);
end;

procedure TGDBMIDebugger.QueueExecuteUnlock;
begin
  dec(FCommandQueueExecLock);
end;

procedure TGDBMIDebugger.TestCmd(const ACommand: String);
begin
  ExecuteCommand(ACommand, [], [cfIgnoreError]);
end;

{%region      *****  BreakPoints  *****  }

{ TGDBMIDebuggerCommandBreakPointBase }

function TGDBMIDebuggerCommandBreakPointBase.ExecCheckLineInUnit(ASource: string;
  ALine: Integer): Boolean;
var
  R: TGDBMIExecResult;
  i, m, n: Integer;
begin
  Result := ALine > 0;
  if not Result then exit;

  m := -1;
  i := FTheDebugger.FMaxLineForUnitCache.IndexOf(ASource);
  if i >= 0 then
    m := PtrInt(FTheDebugger.FMaxLineForUnitCache.Objects[i]);

  if ALine <= m then exit;;

  if ExecuteCommand('info line "' + ASource + '":' + IntToStr(ALine), R)
  and (R.State <> dsError)
  then begin
    m := pos('"', R.Values);  // find start of filename in messages
    n := pos('out of range', R.Values);
    Result := (n < 1) or (n >= m);
  end;

  if not Result then exit;

  if i < 0 then
    i := FTheDebugger.FMaxLineForUnitCache.Add(ASource);
  FTheDebugger.FMaxLineForUnitCache.Objects[i] := TObject(PtrInt(ALine));
end;

function TGDBMIDebuggerCommandBreakPointBase.ExecBreakDelete(ABreakId: Integer): Boolean;
begin
  Result := False;
  if ABreakID = 0 then Exit;

  Result := ExecuteCommand('-break-delete %d', [ABreakID], []);
end;

function TGDBMIDebuggerCommandBreakPointBase.ExecBreakEnabled(ABreakId: Integer;
  AnEnabled: Boolean): Boolean;
const
  // Use shortstring as fix for fpc 1.9.5 [2004/07/15]
  CMD: array[Boolean] of ShortString = ('disable', 'enable');
begin
  Result := False;
  if ABreakID = 0 then Exit;

  Result := ExecuteCommand('-break-%s %d', [CMD[AnEnabled], ABreakID], []);
end;

function TGDBMIDebuggerCommandBreakPointBase.ExecBreakCondition(ABreakId: Integer;
  AnExpression: string): Boolean;
begin
  Result := False;
  if ABreakID = 0 then Exit;

  Result := ExecuteCommand('-break-condition %d %s', [ABreakID, AnExpression], []);
end;

{ TGDBMIDebuggerCommandBreakInsert }

function TGDBMIDebuggerCommandBreakInsert.ExecBreakInsert(out ABreakId, AHitCnt: Integer; out
  AnAddr: TDBGPtr): Boolean;
var
  R: TGDBMIExecResult;
  ResultList: TGDBMINameValueList;
  WatchExpr, WatchDecl, WatchAddr: String;
  GdbRes: String;
begin
  Result := False;
  ABreakId := 0;
  AHitCnt := 0;
  AnAddr := 0;
  GdbRes := 'bkpt';
  case FKind of
    bpkSource:
      begin
        if (FSource = '') or (FLine < 0) then exit;
        Result := ExecCheckLineInUnit(FSource, FLine);
        if not Result then exit;

        if dfForceBreak in FTheDebugger.FDebuggerFlags
        then Result := ExecuteCommand('-break-insert -f %s:%d', [ExtractFileName(FSource), FLine], R)
        else Result := ExecuteCommand('-break-insert %s:%d',    [ExtractFileName(FSource), FLine], R);
      end;
    bpkAddress:
      begin
        if (FAddress = 0) then exit;
        if dfForceBreak in FTheDebugger.FDebuggerFlags
        then Result := ExecuteCommand('-break-insert -f *%u', [FAddress], R)
        else Result := ExecuteCommand('-break-insert *%u',    [FAddress], R);
      end;
    bpkData:
      begin
        if (FWatchData = '') then exit;
        WatchExpr := WatchData;
        if FWatchScope = wpsGlobal then begin
          Result := ExecuteCommand('ptype %s', [WatchExpr], R);
          Result := Result and (R.State <> dsError);
          if not Result then exit;
          WatchDecl := PCLenToString(ParseTypeFromGdb(R.Values).Name);
          Result := ExecuteCommand('-data-evaluate-expression @%s', [WatchExpr], R);
          Result := Result and (R.State <> dsError);
          if not Result then exit;
          WatchAddr := StripLN(GetPart('value="', '"', R.Values));
          WatchExpr := WatchDecl+'(' + WatchAddr + '^)';
        end;
        case FWatchKind of
          wpkWrite:     Result := ExecuteCommand('-break-watch %s', [WatchExpr], R);
          wpkRead:      Result := ExecuteCommand('-break-watch -r %s', [WatchExpr], R);
          wpkReadWrite: Result := ExecuteCommand('-break-watch -a %s', [WatchExpr], R);
        end;
        Result := Result and (R.State <> dsError);
        GdbRes := 'wpt';
      end;
  end;

  ResultList := TGDBMINameValueList.Create(R, [GdbRes]);
  ABreakID := StrToIntDef(ResultList.Values['number'], 0);
  AHitCnt  := StrToIntDef(ResultList.Values['times'], 0);
  AnAddr   := StrToQWordDef(ResultList.Values['addr'], 0);
  if ABreakID = 0
  then Result := False;
  ResultList.Free;
end;

function TGDBMIDebuggerCommandBreakInsert.DoExecute: Boolean;
begin
  Result := True;
  FValid := False;
  DefaultTimeOut := DebuggerProperties.TimeoutForEval;
  try
    if FReplaceId <> 0
    then ExecBreakDelete(FReplaceId);

    FValid := ExecBreakInsert(FBreakID, FHitCnt, FAddr);
    if not FValid then Exit;

    if (FExpression <> '') and not (dcsCanceled in SeenStates)
    then ExecBreakCondition(FBreakID, FExpression);

    if not (dcsCanceled in SeenStates)
    then ExecBreakEnabled(FBreakID, FEnabled);

    if dcsCanceled in SeenStates
    then begin
      ExecBreakDelete(FBreakID);
      FBreakID := 0;
      FValid := False;
      FAddr := 0;
      FHitCnt := 0;
    end;
  finally
    DefaultTimeOut := -1;
  end;
end;

constructor TGDBMIDebuggerCommandBreakInsert.Create(AOwner: TGDBMIDebugger; ASource: string;
  ALine: Integer; AEnabled: Boolean; AnExpression: string; AReplaceId: Integer);
begin
  inherited Create(AOwner);
  FKind := bpkSource;
  FSource := ASource;
  FLine := ALine;
  FEnabled := AEnabled;
  FExpression := AnExpression;
  FReplaceId := AReplaceId;
end;

constructor TGDBMIDebuggerCommandBreakInsert.Create(AOwner: TGDBMIDebugger;
  AAddress: TDBGPtr; AEnabled: Boolean; AnExpression: string;
  AReplaceId: Integer);
begin
  inherited Create(AOwner);
  FKind := bpkAddress;
  FAddress := AAddress;
  FEnabled := AEnabled;
  FExpression := AnExpression;
  FReplaceId := AReplaceId;
end;

constructor TGDBMIDebuggerCommandBreakInsert.Create(AOwner: TGDBMIDebugger; AData: string;
  AScope: TDBGWatchPointScope; AKind: TDBGWatchPointKind; AEnabled: Boolean;
  AnExpression: string; AReplaceId: Integer);
begin
  inherited Create(AOwner);
  FKind := bpkData;
  FWatchData := AData;
  FWatchScope := AScope;
  FWatchKind := AKind;
  FEnabled := AEnabled;
  FExpression := AnExpression;
  FReplaceId := AReplaceId;
end;

function TGDBMIDebuggerCommandBreakInsert.DebugText: String;
begin
  case FKind of
    bpkAddress:
      Result := Format('%s: Address=%x, Enabled=%s', [ClassName, FAddress, dbgs(FEnabled)]);
    bpkData:
      Result := Format('%s: Data=%s, Enabled=%s', [ClassName, FWatchData, dbgs(FEnabled)]);
    else
      Result := Format('%s: Source=%s, Line=%d, Enabled=%s', [ClassName, FSource, FLine, dbgs(FEnabled)]);
  end;
end;

{ TGDBMIDebuggerCommandBreakRemove }

function TGDBMIDebuggerCommandBreakRemove.DoExecute: Boolean;
begin
  Result := True;
  DefaultTimeOut := DebuggerProperties.TimeoutForEval;
  try
  ExecBreakDelete(FBreakId);
  finally
    DefaultTimeOut := -1;
  end;
end;

constructor TGDBMIDebuggerCommandBreakRemove.Create(AOwner: TGDBMIDebugger;
  ABreakId: Integer);
begin
  inherited Create(AOwner);
  FBreakId := ABreakId;
end;

function TGDBMIDebuggerCommandBreakRemove.DebugText: String;
begin
  Result := Format('%s: BreakId=%d', [ClassName, FBreakId]);
end;

{ TGDBMIDebuggerCommandBreakUpdate }

function TGDBMIDebuggerCommandBreakUpdate.DoExecute: Boolean;
begin
  Result := True;
  DefaultTimeOut := DebuggerProperties.TimeoutForEval;
  try
  if FUpdateExpression
  then ExecBreakCondition(FBreakID, FExpression);
  if FUpdateEnabled
  then ExecBreakEnabled(FBreakID, FEnabled);
  finally
    DefaultTimeOut := -1;
  end;
end;

constructor TGDBMIDebuggerCommandBreakUpdate.Create(AOwner: TGDBMIDebugger; ABreakId: Integer);
begin
  inherited Create(AOwner);
  FBreakID := ABreakId;
  FUpdateEnabled := False;
  FUpdateExpression := False;
end;

constructor TGDBMIDebuggerCommandBreakUpdate.Create(AOwner: TGDBMIDebugger;
  ABreakId: Integer; AnEnabled: Boolean);
begin
  inherited Create(AOwner);
  FBreakID := ABreakId;
  FEnabled := AnEnabled;
  FUpdateEnabled := True;
  FUpdateExpression := False;
end;

constructor TGDBMIDebuggerCommandBreakUpdate.Create(AOwner: TGDBMIDebugger;
  ABreakId: Integer; AnExpression: string);
begin
  inherited Create(AOwner);
  FBreakID := ABreakId;
  FExpression := AnExpression;
  FUpdateExpression := True;
  FUpdateEnabled := False;
end;

constructor TGDBMIDebuggerCommandBreakUpdate.Create(AOwner: TGDBMIDebugger;
  ABreakId: Integer; AnEnabled: Boolean; AnExpression: string);
begin
  inherited Create(AOwner);
  FBreakID := ABreakId;
  FEnabled := AnEnabled;
  FUpdateEnabled := True;
  FExpression := AnExpression;
  FUpdateExpression := True;
end;

function TGDBMIDebuggerCommandBreakUpdate.DebugText: String;
begin
  Result := Format('%s: BreakId=%d ChangeEnabled=%s NewEnable=%s ChangeEpression=%s NewExpression=%s',
   [ClassName, FBreakId, dbgs(FUpdateEnabled), dbgs(FEnabled), dbgs(FUpdateExpression), FExpression]);
end;

{ =========================================================================== }
{ TGDBMIBreakPoint }
{ =========================================================================== }

constructor TGDBMIBreakPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCurrentCmd := nil;
  FUpdateFlags := [];
  FBreakID := 0;
end;

destructor TGDBMIBreakPoint.Destroy;
begin
  ReleaseBreakPoint;
  if FCurrentCmd <> nil
  then begin
    // keep the command running
    FCurrentCmd.OnDestroy := nil;
    FCurrentCmd.OnCancel := nil;
    FCurrentCmd.OnExecuted := nil;
  end;
  inherited Destroy;
end;

procedure TGDBMIBreakPoint.DoEnableChange;
begin
  if (FBreakID = 0) and Enabled and
     (TGDBMIDebugger(Debugger).State in [dsPause, dsInternalPause, dsRun])
  then
    SetBreakPoint
  else
    UpdateProperties([bufEnabled]);
  inherited;
end;

procedure TGDBMIBreakPoint.DoExpressionChange;
var
  S: String;
begin
  S := Expression;
  if TGDBMIDebugger(Debugger).ConvertPascalExpression(S)
  then FParsedExpression := S
  else FParsedExpression := Expression;
  if (FBreakID = 0) and Enabled and
     (TGDBMIDebugger(Debugger).State in [dsPause, dsInternalPause, dsRun])
  then
    SetBreakPoint
  else
    UpdateProperties([bufCondition]);
  inherited;
end;

procedure TGDBMIBreakPoint.DoStateChange(const AOldState: TDBGState);
begin
  inherited DoStateChange(AOldState);

  case Debugger.State of
    dsInit: begin
      SetBreakpoint;
    end;
    dsStop: begin
      if FBreakID > 0
      then ReleaseBreakpoint;
    end;
  end;
end;

procedure TGDBMIBreakPoint.DoLogExpression(const AnExpression: String);
var
  s: String;
  t: TGDBType;
begin
  if TGDBMIDebugger(Debugger).GDBEvaluate(AnExpression, s, t, [defNoTypeInfo])
  then begin
    TGDBMIDebugger(Debugger).DoDbgEvent(ecBreakpoint, etBreakpointEvaluation, s);
  end;
end;

procedure TGDBMIBreakPoint.MakeInvalid;
begin
  BeginUpdate;
  ReleaseBreakPoint;
  SetValid(vsInvalid);
  Changed;
  EndUpdate;
end;

procedure TGDBMIBreakPoint.SetAddress(const AValue: TDBGPtr);
begin
  if (Address = AValue) then exit;
  inherited;
  if (Debugger = nil) then Exit;
  if TGDBMIDebugger(Debugger).State in [dsPause, dsInternalPause, dsRun]
  then SetBreakpoint;
end;

procedure TGDBMIBreakPoint.SetBreakpoint;
begin
  if Debugger = nil then Exit;
  if IsUpdating
  then begin
    FUpdateFlags := [bufSetBreakPoint];
    exit;
  end;

  if (FCurrentCmd <> nil)
  then begin
    // We can not be changed, while we get destroyed
    if (FCurrentCmd is TGDBMIDebuggerCommandBreakRemove)
    then begin
      SetValid(vsInvalid);
      exit;
    end;

    if (FCurrentCmd is TGDBMIDebuggerCommandBreakInsert) and (FCurrentCmd.State = dcsQueued)
    then begin
      // update the current object
      TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Kind := Kind;
      case Kind of
        bpkSource:
          begin
            TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Source := Source;
            TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Line := Line;
          end;
        bpkAddress:
          begin
            TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Address := Address;
          end;
        bpkData:
          begin
            TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).WatchData := WatchData;
            TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).WatchScope := WatchScope;
          end;
      end;
      TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Enabled := Enabled;
      TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Expression := FParsedExpression;
      exit;
    end;

    if (FCurrentCmd.State = dcsQueued)
    then begin
      // must be update for enabled or expression. both will be included in BreakInsert
      // cancel and schedule BreakInsert
      FCurrentCmd.OnDestroy := nil;
      FCurrentCmd.OnCancel := nil;
      FCurrentCmd.OnExecuted := nil;
      FCurrentCmd.Cancel;
    end
    else begin
      // let the command run (remove flags for enabled/condition)
      FUpdateFlags := [bufSetBreakPoint];
      exit;
    end;
  end;

  FUpdateFlags := [];
  case Kind of
    bpkSource:
      FCurrentCmd := TGDBMIDebuggerCommandBreakInsert.Create(TGDBMIDebugger(Debugger), Source, Line, Enabled, FParsedExpression, FBreakID);
    bpkAddress:
      FCurrentCmd := TGDBMIDebuggerCommandBreakInsert.Create(TGDBMIDebugger(Debugger), Address, Enabled, FParsedExpression, FBreakID);
    bpkData:
      FCurrentCmd := TGDBMIDebuggerCommandBreakInsert.Create(TGDBMIDebugger(Debugger), WatchData, WatchScope, WatchKind, Enabled, FParsedExpression, FBreakID);
  end;
  FBreakID := 0; // will be replaced => no longer valid
  FCurrentCmd.OnDestroy  := @DoCommandDestroyed;
  FCurrentCmd.OnExecuted  := @DoCommandExecuted;
  FCurrentCmd.Priority := GDCMD_PRIOR_USER_ACT;
  TGDBMIDebugger(Debugger).QueueCommand(FCurrentCmd);

  if Debugger.State = dsRun
  then TGDBMIDebugger(Debugger).GDBPause(True);
end;

procedure TGDBMIBreakPoint.DoCommandDestroyed(Sender: TObject);
begin
  if Sender = FCurrentCmd
  then FCurrentCmd := nil;
  // in case of cancelation
  if bufSetBreakPoint in FUpdateFlags
  then SetBreakPoint;
  if FUpdateFlags * [bufEnabled, bufCondition] <> []
  then UpdateProperties(FUpdateFlags);
end;

procedure TGDBMIBreakPoint.DoCommandExecuted(Sender: TObject);
begin
  if Sender = FCurrentCmd
  then FCurrentCmd := nil;

  if (Sender is TGDBMIDebuggerCommandBreakInsert)
  then begin
    // Check Insert Result
    BeginUpdate;

    if TGDBMIDebuggerCommandBreakInsert(Sender).Valid
    then SetValid(vsValid)
    else begin
      if (TGDBMIDebuggerCommandBreakInsert(Sender).Kind = bpkData) and
         (TGDBMIDebugger(Debugger).State = dsInit)
      then begin
        // disable data breakpoint, if unable to set (only at startup)
        SetValid(vsValid);
        SetEnabled(False);
      end
      else SetValid(vsInvalid);
    end;

    FBreakID := TGDBMIDebuggerCommandBreakInsert(Sender).BreakID;
    SetHitCount(TGDBMIDebuggerCommandBreakInsert(Sender).HitCnt);

    if Enabled
    and (TGDBMIDebugger(Debugger).FBreakAtMain = nil)
    then begin
      // Check if this BP is at the same location as the temp break
      if TGDBMIDebugger(Debugger).FMainAddrBreak.MatchAddr(TGDBMIDebuggerCommandBreakInsert(Sender).Addr)
      then TGDBMIDebugger(Debugger).FBreakAtMain := Self;
    end;

    EndUpdate;
  end;

  if bufSetBreakPoint in FUpdateFlags
  then SetBreakPoint;
  if FUpdateFlags * [bufEnabled, bufCondition] <> []
  then UpdateProperties(FUpdateFlags);
end;

procedure TGDBMIBreakPoint.DoEndUpdate;
begin
  if bufSetBreakPoint in FUpdateFlags
  then SetBreakPoint;
  if FUpdateFlags * [bufEnabled, bufCondition] <> []
  then UpdateProperties(FUpdateFlags);
  inherited DoChanged;
end;

procedure TGDBMIBreakPoint.ReleaseBreakPoint;
begin
  if Debugger = nil then Exit;

  FUpdateFlags := [];
  if (FCurrentCmd <> nil) and (FCurrentCmd is TGDBMIDebuggerCommandBreakRemove)
  then exit;

  // Cancel any other current command
  if (FCurrentCmd <> nil)
  then begin
    FCurrentCmd.OnDestroy := nil;
    FCurrentCmd.OnCancel := nil;
    FCurrentCmd.OnExecuted := nil;
    // if CurrenCmd is TGDBMIDebuggerCommandBreakInsert then it will remove itself
    FCurrentCmd.Cancel;
  end;

  if FBreakID = 0 then Exit;

  FCurrentCmd := TGDBMIDebuggerCommandBreakRemove.Create(TGDBMIDebugger(Debugger), FBreakID);
  FCurrentCmd.OnDestroy  := @DoCommandDestroyed;
  FCurrentCmd.OnExecuted  := @DoCommandExecuted;
  FCurrentCmd.Priority := GDCMD_PRIOR_USER_ACT;
  TGDBMIDebugger(Debugger).QueueCommand(FCurrentCmd);

  FBreakID:=0;
  SetHitCount(0);

  if Debugger.State = dsRun
  then TGDBMIDebugger(Debugger).GDBPause(True);
end;

procedure TGDBMIBreakPoint.SetLocation(const ASource: String; const ALine: Integer);
begin
  if (Source = ASource) and (Line = ALine) then exit;
  inherited;
  if (Debugger = nil) or (Source = '')  then Exit;
  if TGDBMIDebugger(Debugger).State in [dsPause, dsInternalPause, dsRun]
  then SetBreakpoint;
end;

procedure TGDBMIBreakPoint.SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind);
begin
  if (AData = WatchData) and (AScope = WatchScope) and (AKind = WatchKind) then exit;
  inherited SetWatch(AData, AScope, AKind);
  if (Debugger = nil) or (WatchData = '')  then Exit;
  if TGDBMIDebugger(Debugger).State in [dsPause, dsInternalPause, dsRun]
  then SetBreakpoint;
end;

procedure TGDBMIBreakPoint.UpdateProperties(AFlags: TGDBMIBreakPointUpdateFlags);
begin
  if (Debugger = nil) then Exit;
  if AFlags * [bufEnabled, bufCondition] = [] then Exit;
  if IsUpdating
  then begin
    if not(bufSetBreakPoint in FUpdateFlags)
    then FUpdateFlags := FUpdateFlags + AFlags;
    exit;
  end;

  if (FCurrentCmd <> nil)
  then begin
    // We can not be changed, while we get destroyed
    if (FCurrentCmd is TGDBMIDebuggerCommandBreakRemove)
    then begin
      SetValid(vsInvalid);
      exit;
    end;

    if (FCurrentCmd is TGDBMIDebuggerCommandBreakInsert) and (FCurrentCmd.State = dcsQueued)
    then begin
      if bufEnabled in AFlags
      then TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Enabled := Enabled;
      if bufCondition in AFlags
      then TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Expression := Expression;
      exit;
    end;

    if (FCurrentCmd is TGDBMIDebuggerCommandBreakUpdate) and (FCurrentCmd.State = dcsQueued)
    then begin
      // update the current object
      if bufEnabled in AFlags
      then begin
        TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).UpdateEnabled := True;
        TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).Enabled := Enabled;
      end;
      if bufCondition in AFlags
      then begin
        TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).UpdateExpression := True;
        TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).Expression := FParsedExpression;
      end;
      exit;
    end;

    if bufSetBreakPoint in FUpdateFlags
    then exit;

    // let the command run
    FUpdateFlags := FUpdateFlags + AFlags;
    exit;
  end;

  if (FBreakID = 0) then Exit;

  FUpdateFlags := FUpdateFlags - [bufEnabled, bufCondition];

  FCurrentCmd:= TGDBMIDebuggerCommandBreakUpdate.Create(TGDBMIDebugger(Debugger), FBreakID);
  if bufEnabled in AFlags
  then begin
    TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).UpdateEnabled := True;
    TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).Enabled := Enabled;
  end;
  if bufCondition in AFlags
  then begin
    TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).UpdateExpression := True;
    TGDBMIDebuggerCommandBreakUpdate(FCurrentCmd).Expression := FParsedExpression;
  end;
  FCurrentCmd.OnDestroy  := @DoCommandDestroyed;
  FCurrentCmd.OnExecuted  := @DoCommandExecuted;
  FCurrentCmd.Priority := GDCMD_PRIOR_USER_ACT;
  TGDBMIDebugger(Debugger).QueueCommand(FCurrentCmd);

  if Debugger.State = dsRun
  then TGDBMIDebugger(Debugger).GDBPause(True);
end;

{%endregion   ^^^^^  BreakPoints  ^^^^^  }

{%region      *****  Locals  *****  }
{ TGDBMIDebuggerCommandLocals }

function TGDBMIDebuggerCommandLocals.DoExecute: Boolean;

  procedure AddLocals(const AParams: String);
  var
    n, e: Integer;
    addr: TDbgPtr;
    LocList, List: TGDBMINameValueList;
    Item: PGDBMINameValue;
    S, Name, Value: String;
  begin
    LocList := TGDBMINameValueList.Create(AParams);
    List := TGDBMINameValueList.Create('');
    for n := 0 to LocList.Count - 1 do
    begin
      Item := LocList.Items[n];
      List.Init(Item^.Name);
      Name := List.Values['name'];
      if Name = 'this'
      then Name := 'Self';

      Value := DeleteEscapeChars(List.Values['value']);
      // try to deref. strings
      S := GetPart(['(pchar) ', '(ansistring) '], [], Value, True, False);
      if S <> ''
      then begin
        addr := 0;
        Val(S, addr, e);
        if e=0 then ;
        if addr = 0
        then Value := ''''''
        else Value := '''' + GetText(addr) + '''';
      end;

      FLocals.Add(Name, Value);
    end;
    FreeAndNil(List);
    FreeAndNil(LocList);
  end;

var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
begin
  Result := True;
  FLocals.Clear;
  // args
  ExecuteCommand('-stack-list-arguments 1 %0:d %0:d',
    [FTheDebugger.FCurrentStackFrame], R);
  if R.State <> dsError
  then begin
    List := TGDBMINameValueList.Create(R, ['stack-args', 'frame']);
    AddLocals(List.Values['args']);
    FreeAndNil(List);
  end;

  // variables
  ExecuteCommand('-stack-list-locals 1', R);
  if R.State <> dsError
  then begin
    List := TGDBMINameValueList.Create(R);
    AddLocals(List.Values['locals']);
    FreeAndNil(List);
  end;
  FLocals.SetDataValidity(ddsValid);
end;

constructor TGDBMIDebuggerCommandLocals.Create(AOwner: TGDBMIDebugger; ALocals: TCurrentLocals);
begin
  inherited Create(AOwner);
  FLocals := ALocals;
  FLocals.AddReference;
end;

destructor TGDBMIDebuggerCommandLocals.Destroy;
begin
  ReleaseRefAndNil(FLocals);
  inherited Destroy;
end;

function TGDBMIDebuggerCommandLocals.DebugText: String;
begin
  Result := Format('%s:', [ClassName]);
end;

{ =========================================================================== }
{ TGDBMILocals }
{ =========================================================================== }

procedure TGDBMILocals.Changed;
begin
  if Monitor <> nil
  then Monitor.Clear;
end;

constructor TGDBMILocals.Create(const ADebugger: TDebugger);
begin
  FCommandList := TList.Create;
  inherited;
end;

destructor TGDBMILocals.Destroy;
begin
  CancelAllCommands;
  inherited;
  FreeAndNil(FCommandList);
end;

procedure TGDBMILocals.CancelAllCommands;
var
  i: Integer;
begin
  for i := 0 to FCommandList.Count-1 do
    with TGDBMIDebuggerCommandStack(FCommandList[i]) do begin
      OnExecuted := nil;
      OnDestroy := nil;
      Cancel;
    end;
  FCommandList.Clear;
end;

procedure TGDBMILocals.RequestData(ALocals: TCurrentLocals);
var
  ForceQueue: Boolean;
  EvaluationCmdObj: TGDBMIDebuggerCommandLocals;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then Exit;

  EvaluationCmdObj := TGDBMIDebuggerCommandLocals.Create(TGDBMIDebugger(Debugger), ALocals);
  EvaluationCmdObj.OnDestroy   := @DoEvaluationDestroyed;
  EvaluationCmdObj.Priority := GDCMD_PRIOR_LOCALS;
  EvaluationCmdObj.Properties := [dcpCancelOnRun];
  ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
            and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
            and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued)
            and (Debugger.State <> dsInternalPause);
  FCommandList.add(EvaluationCmdObj);
  TGDBMIDebugger(Debugger).QueueCommand(EvaluationCmdObj, ForceQueue);
  (* DoEvaluationFinished may be called immediately at this point *)
end;

procedure TGDBMILocals.DoEvaluationDestroyed(Sender: TObject);
begin
  FCommandList.Remove(Sender);
end;

procedure TGDBMILocals.CancelEvaluation;
begin
end;

{%endregion   ^^^^^  BreakPoints  ^^^^^  }

{ =========================================================================== }
{ TGDBMIRegisters }
{ =========================================================================== }

procedure TGDBMIRegisters.Changed;
begin
  Invalidate;
  inherited Changed;
end;

procedure TGDBMIRegisters.DoStateChange(const AOldState: TDBGState);
begin
  if  Debugger <> nil
  then begin
    case Debugger.State of
      dsPause: DoChange;
      dsStop, dsInit:
      begin
        FRegistersReqState := esInvalid;
        Invalidate;
      end;
    else
      Invalidate
    end;
  end
  else Invalidate;
end;

procedure TGDBMIRegisters.Invalidate;
var
  n: Integer;
  i: TRegisterDisplayFormat;
begin
  for n := Low(FRegModified) to High(FRegModified) do
    FRegModified[n] := False;
  for i := low(TRegisterDisplayFormat) to high(TRegisterDisplayFormat) do begin
    for n := Low(FRegValues[i]) to High(FRegValues[i]) do
      FRegValues[i][n] := '';
    FValuesReqState[i] := esInvalid;
  end;
  FModifiedReqState := esInvalid;
end;

function TGDBMIRegisters.GetCount: Integer;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then RegistersNeeded;

  Result := Length(FRegNames)
end;

function TGDBMIRegisters.GetModified(const AnIndex: Integer): Boolean;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  and (FModifiedReqState <> esValid)
  then ModifiedNeeded;

  if  (FModifiedReqState = esValid)
  and (AnIndex >= Low(FRegModified))
  and (AnIndex <= High(FRegModified))
  then Result := FRegModified[AnIndex]
  else Result := False;
end;

function TGDBMIRegisters.GetName(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then RegistersNeeded;

  if  (FRegistersReqState = esValid)
  and (AnIndex >= Low(FRegNames))
  and (AnIndex <= High(FRegNames))
  then Result := FRegNames[AnIndex]
  else Result := '';
end;

function TGDBMIRegisters.GetValue(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then ValuesNeeded(Formats[AnIndex]);

  if  (FValuesReqState[FFormats[AnIndex]] = esValid)
  and (FRegistersReqState = esValid)
  and (AnIndex >= Low(FRegValues[Formats[AnIndex]]))
  and (AnIndex <= High(FRegValues[Formats[AnIndex]]))
  then Result := FRegValues[Formats[AnIndex]][AnIndex]
  else Result := '';
end;

procedure TGDBMIRegisters.DoGetRegisterNamesDestroyed(Sender: TObject);
begin
  if FGetRegisterCmdObj = Sender
  then FGetRegisterCmdObj := nil;
end;

procedure TGDBMIRegisters.DoGetRegisterNamesFinished(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandRegisterNames;
  n: Integer;
  f: TRegisterDisplayFormat;
begin
  Cmd := TGDBMIDebuggerCommandRegisterNames(Sender);

  SetLength(FRegNames, Cmd.Count);
  SetLength(FRegModified, Cmd.Count);
  SetLength(FFormats, Cmd.Count);
  for f := low(TRegisterDisplayFormat) to high(TRegisterDisplayFormat) do begin
    SetLength(FRegValues[f], Cmd.Count);
    FValuesReqState[f] := esInvalid;
  end;
  FModifiedReqState := esInvalid;
  for n := 0 to Cmd.Count - 1 do
  begin
    FRegNames[n] := Cmd.Names[n];
    for f := low(TRegisterDisplayFormat) to high(TRegisterDisplayFormat) do
      FRegValues[f][n] := '';
    FRegModified[n] := False;
    FFormats[n] := rdDefault;
  end;

  FGetRegisterCmdObj:= nil;
  FRegistersReqState := esValid;

  if not FInRegistersNeeded
  then Changed;
end;

procedure TGDBMIRegisters.RegistersNeeded;
var
  ForceQueue: Boolean;
begin
  if (Debugger = nil) or (FRegistersReqState in [esRequested, esValid])
  then Exit;

  if (Debugger.State in [dsPause, dsStop])
  then begin
    FInRegistersNeeded := True;
    FRegistersReqState := esRequested;
    SetLength(FRegNames, 0);

    FGetRegisterCmdObj := TGDBMIDebuggerCommandRegisterNames.Create(TGDBMIDebugger(Debugger));
    FGetRegisterCmdObj.OnExecuted := @DoGetRegisterNamesFinished;
    FGetRegisterCmdObj.OnDestroy   := @DoGetRegisterNamesDestroyed;
    FGetRegisterCmdObj.Priority := GDCMD_PRIOR_LOCALS;
    FGetRegisterCmdObj.Properties := [dcpCancelOnRun];
    ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
              and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
              and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued)
              and (Debugger.State <> dsInternalPause);
    TGDBMIDebugger(Debugger).QueueCommand(FGetRegisterCmdObj, ForceQueue);
    (* DoEvaluationFinished may be called immediately at this point *)
    FInRegistersNeeded := False;
  end;
end;

function TGDBMIRegisters.GetDebugger: TGDBMIDebugger;
begin
  Result := TGDBMIDebugger(inherited Debugger)
end;

procedure TGDBMIRegisters.DoGetRegValuesDestroyed(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandRegisterValues;
begin
  Cmd := TGDBMIDebuggerCommandRegisterValues(Sender);
  if FGetValuesCmdObj[Cmd.Format] = Sender
  then FGetValuesCmdObj[Cmd.Format] := nil;
end;

procedure TGDBMIRegisters.DoGetRegValuesFinished(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandRegisterValues;
begin
  Cmd := TGDBMIDebuggerCommandRegisterValues(Sender);
  FValuesReqState[Cmd.Format] := esValid;
  FGetValuesCmdObj[Cmd.Format] := nil;
  if not FInValuesNeeded[Cmd.Format]
  then inherited Changed;
end;

procedure TGDBMIRegisters.ValuesNeeded(AFormat: TRegisterDisplayFormat);
var
  ForceQueue: Boolean;
begin
  if  (Debugger <> nil) and (Debugger.State = dsPause)
  then RegistersNeeded;

  if (Debugger = nil)
  or (not (Debugger.State in [dsPause, dsStop]))
  or (FRegistersReqState <> esValid)
  or (FValuesReqState[AFormat] in [esRequested, esValid])
  or (Count = 0)
  then Exit;

  FInValuesNeeded[AFormat] := True;
  FValuesReqState[AFormat] := esRequested;

  FGetValuesCmdObj[AFormat] := TGDBMIDebuggerCommandRegisterValues.Create
    (Debugger,  FRegValues[AFormat], AFormat);
  FGetValuesCmdObj[AFormat].OnExecuted := @DoGetRegValuesFinished;
  FGetValuesCmdObj[AFormat].OnDestroy   := @DoGetRegValuesDestroyed;
  FGetValuesCmdObj[AFormat].Priority := GDCMD_PRIOR_LOCALS;
  FGetValuesCmdObj[AFormat].Properties := [dcpCancelOnRun];
  ForceQueue := (Debugger.FCurrentCommand <> nil)
            and (Debugger.FCurrentCommand is TGDBMIDebuggerCommandExecute)
            and (not TGDBMIDebuggerCommandExecute(Debugger.FCurrentCommand).NextExecQueued)
            and (Debugger.State <> dsInternalPause);
  Debugger.QueueCommand(FGetValuesCmdObj[AFormat], ForceQueue);
  (* DoEvaluationFinished may be called immediately at this point *)
  FInValuesNeeded[AFormat] := False;
end;

procedure TGDBMIRegisters.DoGetRegModifiedDestroyed(Sender: TObject);
begin
  if FGetModifiedCmd = Sender
  then FGetModifiedCmd := nil;
end;

procedure TGDBMIRegisters.DoGetRegModifiedFinished(Sender: TObject);
begin
  FModifiedReqState := esValid;
  FGetModifiedCmd := nil;
  if not FInModifiedNeeded
  then inherited Changed;
end;

procedure TGDBMIRegisters.ModifiedNeeded;
var
  ForceQueue: Boolean;
begin
  if  (Debugger <> nil) and (Debugger.State = dsPause)
  then RegistersNeeded;

  if (Debugger = nil)
  or (not (Debugger.State in [dsPause, dsStop]))
  or (FRegistersReqState <> esValid)
  or (FModifiedReqState in [esRequested, esValid])
  or (Count = 0)
  then Exit;

  FInModifiedNeeded := True;
  FModifiedReqState := esRequested;

  FGetModifiedCmd := TGDBMIDebuggerCommandRegisterModified.Create(Debugger,  FRegModified);
  FGetModifiedCmd.OnExecuted  := @DoGetRegModifiedFinished;
  FGetModifiedCmd.OnDestroy    := @DoGetRegModifiedDestroyed;
  FGetModifiedCmd.Priority := GDCMD_PRIOR_LOCALS;
  FGetModifiedCmd.Properties := [dcpCancelOnRun];
  ForceQueue := (Debugger.FCurrentCommand <> nil)
            and (Debugger.FCurrentCommand is TGDBMIDebuggerCommandExecute)
            and (not TGDBMIDebuggerCommandExecute(Debugger.FCurrentCommand).NextExecQueued)
            and (Debugger.State <> dsInternalPause);
  Debugger.QueueCommand(FGetModifiedCmd, ForceQueue);
  (* DoEvaluationFinished may be called immediately at this point *)
  FInModifiedNeeded := False;
end;

{ =========================================================================== }
{ TGDBMIWatches }
{ =========================================================================== }

procedure TGDBMIWatches.DoEvaluationDestroyed(Sender: TObject);
begin
  FCommandList.Remove(Sender);
end;

function TGDBMIWatches.GetParentFPList(AThreadId: Integer): PGDBMIDebuggerParentFrameCache;
var
  i: Integer;
begin
  for i := 0 to high(FParentFPList) do
    if FParentFPList[i].ThreadId = AThreadId
    then exit(@FParentFPList[i]);
  i := Length(FParentFPList);
  SetLength(FParentFPList, i + 1);
  FParentFPList[i].ThreadId := AThreadId;
  Result := @FParentFPList[i];
end;

procedure TGDBMIWatches.DoStateChange(const AOldState: TDBGState);
begin
  SetLength(FParentFPList, 0);
  inherited DoStateChange(AOldState);
end;

procedure TGDBMIWatches.Changed;
begin
  SetLength(FParentFPList, 0);
  if CurrentWatches <> nil
  then CurrentWatches.ClearValues;
end;

procedure TGDBMIWatches.Clear;
var
  i: Integer;
begin
  for i := 0 to FCommandList.Count-1 do
    with TGDBMIDebuggerCommandStack(FCommandList[i]) do begin
      OnExecuted := nil;
      OnDestroy := nil;
      Cancel;
    end;
  FCommandList.Clear;
end;

procedure TGDBMIWatches.InternalRequestData(AWatchValue: TCurrentWatchValue);
var
  ForceQueue: Boolean;
  EvaluationCmdObj: TGDBMIDebuggerCommandEvaluate;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    AWatchValue.Validity := ddsInvalid;
    Exit;
  end;

  EvaluationCmdObj := TGDBMIDebuggerCommandEvaluate.Create
    (TGDBMIDebugger(Debugger), AWatchValue);
  //EvaluationCmdObj.OnExecuted := @DoEvaluationFinished;
  EvaluationCmdObj.OnDestroy    := @DoEvaluationDestroyed;
  EvaluationCmdObj.Properties := [dcpCancelOnRun];
  // If a ExecCmd is running, then defer exec until the exec cmd is done
  ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
            and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
            and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued)
            and (Debugger.State <> dsInternalPause);
  FCommandList.Add(EvaluationCmdObj);
  TGDBMIDebugger(Debugger).QueueCommand(EvaluationCmdObj, ForceQueue);
  (* DoEvaluationFinished may be called immediately at this point *)
end;

constructor TGDBMIWatches.Create(const ADebugger: TDebugger);
begin
  FCommandList := TList.Create;
  inherited Create(ADebugger);
end;

destructor TGDBMIWatches.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FCommandList);
end;



{ =========================================================================== }
{ TGDBMICallStack }
{ =========================================================================== }

procedure TGDBMICallStack.DoDepthCommandExecuted(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandStackDepth;
begin
  FCommandList.Remove(Sender);
  Cmd := TGDBMIDebuggerCommandStackDepth(Sender);
  if Cmd.Callstack = nil then exit;
  if Cmd.Depth < 0 then begin
    Cmd.Callstack.SetCountValidity(ddsInvalid);
  end else begin
    Cmd.Callstack.Count := Cmd.Depth;
    Cmd.Callstack.SetCountValidity(ddsValid);
  end;
end;

procedure TGDBMICallStack.RequestCount(ACallstack: TCurrentCallStack);
var
  DepthEvalCmdObj: TGDBMIDebuggerCommandStackDepth;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause])
  then begin
    ACallstack.SetCountValidity(ddsInvalid);
    exit;
  end;

  DepthEvalCmdObj := TGDBMIDebuggerCommandStackDepth.Create(TGDBMIDebugger(Debugger), ACallstack);
  DepthEvalCmdObj.OnExecuted := @DoDepthCommandExecuted;
  DepthEvalCmdObj.OnDestroy   := @DoCommandDestroyed;
  DepthEvalCmdObj.Priority := GDCMD_PRIOR_STACK;
  FCommandList.Add(DepthEvalCmdObj);
  TGDBMIDebugger(Debugger).QueueCommand(DepthEvalCmdObj);
  (* DoDepthCommandExecuted may be called immediately at this point *)
end;

procedure TGDBMICallStack.RequestCurrent(ACallstack: TCurrentCallStack);
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    ACallstack.SetCurrentValidity(ddsInvalid);
    Exit;
  end;

  if ACallstack.ThreadId = TGDBMIDebugger(Debugger).FCurrentThreadId
  then ACallstack.CurrentIndex := TGDBMIDebugger(Debugger).FCurrentStackFrame
  else ACallstack.CurrentIndex := 0; // will be used, if thread is changed
  ACallstack.SetCurrentValidity(ddsValid);
end;

procedure TGDBMICallStack.RequestEntries(ACallstack: TCurrentCallStack);
var
  FramesEvalCmdObj: TGDBMIDebuggerCommandStackFrames;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then Exit;

  FramesEvalCmdObj := TGDBMIDebuggerCommandStackFrames.Create(TGDBMIDebugger(Debugger), ACallstack);
  //FramesEvalCmdObj.OnExecuted := @DoFramesCommandExecuted;
  FramesEvalCmdObj.OnDestroy  := @DoCommandDestroyed;
  FramesEvalCmdObj.Priority := GDCMD_PRIOR_STACK;
  FCommandList.Add(FramesEvalCmdObj);
  TGDBMIDebugger(Debugger).QueueCommand(FramesEvalCmdObj);
  (* DoFramesCommandExecuted may be called immediately at this point *)
end;

procedure TGDBMICallStack.DoCommandDestroyed(Sender: TObject);
begin
  FCommandList.Remove(Sender);
end;

procedure TGDBMICallStack.Clear;
var
  i: Integer;
begin
  for i := 0 to FCommandList.Count-1 do
    with TGDBMIDebuggerCommandStack(FCommandList[i]) do begin
      OnExecuted := nil;
      OnDestroy := nil;
      Cancel;
    end;
  FCommandList.Clear;
end;

procedure TGDBMICallStack.DoSetIndexCommandExecuted(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandStackSetCurrent;
begin
  Cmd := TGDBMIDebuggerCommandStackSetCurrent(Sender);
  TGDBMIDebugger(Debugger).FCurrentStackFrame := Cmd.NewCurrent;
  if Cmd.Callstack = nil then exit;
  Cmd.Callstack.CurrentIndex := Cmd.NewCurrent;
end;

procedure TGDBMICallStack.UpdateCurrentIndex;
var
  tid, idx: Integer;
  IndexCmd: TGDBMIDebuggerCommandStackSetCurrent;
  cs: TCurrentCallStack;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    exit;
  end;

  tid := Debugger.Threads.Monitor.CurrentThreads.CurrentThreadId;
  cs := TCurrentCallStack(CurrentCallStackList.EntriesForThreads[tid]);
  idx := cs.NewCurrentIndex;  // NEW-CURRENT
  if TGDBMIDebugger(Debugger).FCurrentStackFrame = idx then Exit;

  IndexCmd := TGDBMIDebuggerCommandStackSetCurrent.Create(TGDBMIDebugger(Debugger), cs, idx);
  IndexCmd.OnExecuted  := @DoSetIndexCommandExecuted;
  IndexCmd.OnDestroy  := @DoCommandDestroyed;
  IndexCmd.Priority := GDCMD_PRIOR_STACK;
  FCommandList.Add(IndexCmd);
  TGDBMIDebugger(Debugger).QueueCommand(IndexCmd);
  (* DoFramesCommandExecuted may be called immediately at this point *)
end;

procedure TGDBMICallStack.DoThreadChanged;
var
  tid, idx: Integer;
  IndexCmd: TGDBMIDebuggerCommandStackSetCurrent;
  cs: TCurrentCallStack;
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    exit;
  end;

  TGDBMIDebugger(Debugger).FCurrentStackFrame := -1;
  TGDBMIDebugger(Debugger).FInternalStackFrame := -1;
  tid := Debugger.Threads.Monitor.CurrentThreads.CurrentThreadId;
  cs := TCurrentCallStack(CurrentCallStackList.EntriesForThreads[tid]);
  idx := cs.CurrentIndex;  // CURRENT
  if idx < 0 then idx := 0;

  IndexCmd := TGDBMIDebuggerCommandStackSetCurrent.Create(TGDBMIDebugger(Debugger), cs, idx);
  IndexCmd.OnExecuted  := @DoSetIndexCommandExecuted;
  IndexCmd.OnDestroy  := @DoCommandDestroyed;
  IndexCmd.Priority := GDCMD_PRIOR_STACK;
  FCommandList.Add(IndexCmd);
  TGDBMIDebugger(Debugger).QueueCommand(IndexCmd);
  (* DoFramesCommandExecuted may be called immediately at this point *)
end;

constructor TGDBMICallStack.Create(const ADebugger: TDebugger);
begin
  FCommandList := TList.Create;
  inherited Create(ADebugger);
end;

destructor TGDBMICallStack.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FCommandList);
end;

{ =========================================================================== }
{ TGDBMIExpression }
{ =========================================================================== }

function GetSubExpression(var AExpression: PChar; var ALength: Integer; out AOperator: TDBGExpressionOperator; out AOperand: String): Boolean;
type
  TScanState = (
    ssNone,       // start scanning
    ssString,     // inside string
    ssEndString,  // just left a string, we may reenter if another ' is present
    ssOperand,    // reading operand
    ssOperator    // delimeter found, next must be operator
  );
var
  State: TScanState;

  function GetOperand(const AOperand: String): String;
  begin
    if (AOperand = '')
    or (AOperand[1] <> '''')
    then Result := AOperand
    else Result := ConvertToCString(AOperand);
  end;

  function GetOperator(AOperator: PChar; ALen: Integer): TDBGExpressionOperator;
  begin
    case AOperator[0] of
      '-': Result := eoSubstract;
      '+': Result := eoAdd;
      '*': begin
        if ALen = 1
        then Result := eoMultiply
        else Result := eoPower;
      end;
      '/': Result := eoDivide;
      '^': Result := eoDereference;
      '@': Result := eoAddress;
      '=': Result := eoEqual;
      '<': begin
        if ALen = 1
        then Result := eoLess
        else if AOperator[1] = '='
        then Result := eoLessOrEqual
        else Result := eoNotEqual;
      end;
      '>': begin
        if ALen = 1
        then Result := eoGreater
        else Result := eoGreaterOrEqual;
      end;
      '.': Result := eoDot;
      ',': Result := eoComma;
      '(': Result := eoBracket;
      '[': Result := eoIndex;
      ')': Result := eoClose;
      ']': Result := eoClose;
      'a', 'A': begin
        if AOperator[1] in ['s', 'S']
        then Result := eoAs
        else Result := eoAnd;
      end;
      'o', 'O': Result := eoOr;
      'i', 'I': begin
        if AOperator[1] in ['s', 'S']
        then Result := eoIs
        else Result := eoIn;
      end;
      'm', 'M': Result := eoMod;
      'n', 'N': Result := eoNot;
      'd', 'D': Result := eoDiv;
      'x', 'X': Result := eoXor;
      's', 'S': begin
        if AOperator[2] in ['l', 'L']
        then Result := eoShl
        else Result := eoShr;
      end;
    end;
    Inc(AExpression, ALen);
    Dec(ALength, ALen);
  end;

  function CheckOperator(const AOperator: String): Boolean;
  var
    len: Integer;
  begin
    len := Length(AOperator);
    if ALength <= len then Exit(False); // net char after operator too
    if not (AExpression[len] in [' ', #9, '(']) then Exit(False);
    if StrLIComp(AExpression, @AOperator[1], len) <> 0 then Exit(False);

    Result := True;
  end;

var
  Sub: String;
  len: Integer;
begin
  while (ALength > 0) and (AExpression^ in [#9, ' ']) do
  begin
    Dec(ALength);
    Inc(AExpression);
  end;
  if ALength = 0 then Exit;

  State := ssNone;
  Sub:='';
  while ALength > 0 do
  begin
    if AExpression^ = ''''
    then begin
      case State of
        ssOperand,
        ssOperator: Exit(False); //illegal
        ssNone:  State := ssString;
        ssString:State := ssEndString;
        ssEndString: State := ssString;
      end;
      Sub := Sub + AExpression^;
      Inc(AExpression);
      Dec(ALength);
      Continue;
    end;

    case State of
      ssString: begin
        Sub := Sub + AExpression^;
        Inc(AExpression);
        Dec(ALength);
        Continue;
      end;
      ssEndString: State := ssOperator;
      ssNone: State := ssOperand;
    end;

    case AExpression^ of
      ' ', #9: begin
        State := ssOperator;
        Inc(AExpression);
        Dec(ALength);
        Continue;
      end;
      '(', '[': begin
        AOperand := GetOperand(Sub);
        AOperator := GetOperator(AExpression, 1);
        Exit(True);
      end;
      ')', ']': begin
        AOperand := GetOperand(Sub);
        AOperator := GetOperator(AExpression, 1);
        Exit(True);
      end;
      '-', '+': begin
        if Sub = ''
        then begin
          //unary
          AOperand := '';
          if AExpression^ = '-'
          then AOperator := eoNegate
          else AOperator := eoPlus;
          Inc(AExpression);
          Dec(ALength);
        end
        else begin
          AOperand := GetOperand(Sub);
        end;
        Exit(True);
      end;
      '/', '^', '@', '=', ',': begin
        AOperand := GetOperand(Sub);
        AOperator := GetOperator(AExpression, 1);
        Exit(True);
      end;
      '*', '<', '>': begin
        AOperand := GetOperand(Sub);
        if ALength > 1
        then begin
          if AExpression[0] = '*'
          then begin
            if AExpression[1] = '*'
            then AOperator := GetOperator(AExpression, 2)
            else AOperator := GetOperator(AExpression, 1);
          end
          else begin
            if AExpression[1] = '='
            then AOperator := GetOperator(AExpression, 2)
            else AOperator := GetOperator(AExpression, 1);
          end;
        end
        else AOperator := GetOperator(AExpression, 1);
        Exit(True);
      end;
      '.': begin
        if (State <> ssOperand) or (Length(Sub) = 0) or not (Sub[1] in ['0'..'9'])
        then begin
          AOperand := GetOperand(Sub);
          AOperator := GetOperator(AExpression, 1);
          Exit(True);
        end;
      end;
    end;

    if (State = ssOperator)
    then begin
      len := 3;
      case AExpression^ of
        'a', 'A': begin
          if not CheckOperator('and') then Exit(False);
          if not CheckOperator('as') then Exit(False);
        end;
        'o', 'O': begin
          if not CheckOperator('or') then Exit(False);
          len := 2;
        end;
        'i', 'I': begin
          if not CheckOperator('in') then Exit(False);
          if not CheckOperator('is') then Exit(False);
        end;
        'm', 'M': begin
          if not CheckOperator('mod') then Exit(False);
        end;
        'd', 'D': begin
          if not CheckOperator('div') then Exit(False);
        end;
        'x', 'X': begin
          if not CheckOperator('xor') then Exit(False);
        end;
        's', 'S': begin
          if not (CheckOperator('shl') or CheckOperator('shr')) then Exit(False);
        end;
      else
        Exit(False);
      end;
      AOperand := GetOperand(Sub);
      AOperator := GetOperator(AExpression, len);
      Exit(True);
    end;

    if  (State = ssOperand)
    and (Sub = '')
    and CheckOperator('not')
    then begin
      AOperand := '';
      AOperator := GetOperator(AExpression, 3);
      Exit(True);
    end;

    Sub := Sub + AExpression^;
    Inc(AExpression);
    Dec(ALength);
  end;

  if not (State in [ssOperator, ssOperand, ssEndString]) then Exit(False);

  AOperand := GetOperand(Sub);
  AOperator := eoNone;
  Result := True;
end;

constructor TGDBMIExpression.Create(const AExpression: String);
var
  len: Integer;
  P: PChar;
  Run, Work: PGDBMISubExpression;
  Opertor: TDBGExpressionOperator;
  Operand: String;
begin
  inherited Create;
  len := Length(AExpression);
  p := PChar(AExpression);
  Run := nil;
  while (len > 0) and GetSubExpression(p, len, Opertor, Operand) do
  begin
    New(Work);
    Work^.Opertor := Opertor;
    Work^.Operand := Operand;
    Work^.Prev := Run;
    Work^.Next := nil;

    if FList = nil
    then FList := Work
    else Run^.Next := Work;
    Run := Work;
  end;
end;

destructor TGDBMIExpression.Destroy;
var
  Run, Work: PGDBMISubExpression;
begin
  Run := FList;
  while Run <> nil do
  begin
    Work := Run;
    Run := Work^.Next;
    Dispose(Work);
  end;

  inherited;
end;

procedure TGDBMIExpression.DisposeList(AList: PGDBMIExpressionResult);
var
  Temp: PGDBMIExpressionResult;
begin
  while AList <> nil do
  begin
    AList^.Info.Free;
    Temp := AList;
    AList := Alist^.Next;
    Dispose(Temp);
  end;
end;

function TGDBMIExpression.DumpExpression: String;
// Mainly used for debugging purposes
var
  Sub: PGDBMISubExpression;
  s: string;
begin
  Result := '';
  Sub := FList;
  while Sub <> nil do
  begin
    WriteStr(s, Sub^.Opertor);
    Result := Result + Sub^.Operand + ' ' +  s + ' ';
    Sub := Sub^.Next;
  end;
end;

function TGDBMIExpression.Evaluate(const ADebuggerCommand: TGDBMIDebuggerCommand; out AResult: String; out AResultInfo: TGDBType): Boolean;

const
  OPER_UNARY = [eoNot, eoNegate, eoPlus, eoAddress, eoBracket];

var
  Sub: PGDBMISubExpression;
  R: PGDBMIExpressionResult;
begin
  Result := True;
  Sub := FList;
  FStack := nil;
  FStackPtr := nil;
  New(R);
  FillByte(R^, SizeOf(R^), 0);
  while Sub <> nil do
  begin
    R^.Opertor := Sub^.Opertor;
    if Sub^.Operand = ''
    then begin
      if not (Sub^.OperTor in OPER_UNARY)
      then begin
        // check if we have a 2nd operator
        Result := False;
        if FStackPtr = nil then Break;
        case FStackPtr^.OperTor of
          eoClose, eoDereference: begin
            if not (Sub^.OperTor in [eoDot, eoDereference, eoIndex]) then Break;
          end;
          eoBracket: begin
            if Sub^.OperTor <> eoBracket then Break;
          end;
        end;
        Result := True;
      end;
      Push(R);
      Sub := Sub^.Next;
      Continue;
    end;
    if Sub^.OperTor in OPER_UNARY then Break;

    if (FStackPtr = nil)
    or (OPER_LEVEL[Sub^.OperTor] < OPER_LEVEL[FStackPtr^.OperTor])
    then begin
      if not Evaluate(ADebuggerCommand, Sub^.Operand, R^.Value, R^.Info)
      then begin
        Result := False;
        Break;
      end;
    end
    else begin
      if not Solve(ADebuggerCommand, OPER_LEVEL[Sub^.OperTor], Sub^.Operand, R^.Value, R^.Info)
      then begin
        Result := False;
        Break;
      end;
    end;

    Push(R);
    Sub := Sub^.Next;
  end;


  if Result and (FStackPtr <> nil)
  then begin
    New(R);
    FillByte(R^, SizeOf(R^), 0);
    Result := Solve(ADebuggerCommand, 255, '', R^.Value, R^.Info);
    Push(R); // make sure it gets cleaned later
  end;

  if Result
  then begin
    AResult := R^.Value;
    AResultInfo := R^.Info;
    R^.Info := nil;
  end;

  while FStackPtr <> nil do
  begin
    Pop(R);
    R^.Info.Free;
    Dispose(R);
  end;
end;

function TGDBMIExpression.Evaluate(const ADebuggerCommand: TGDBMIDebuggerCommand; const AText: String; out AResult: String; out AResultInfo: TGDBType): Boolean;
var
  R: TGDBMIExecResult;
  ResultList: TGDBMINameValueList;
begin
  // special cases
  if ATExt = ''
  then begin
    AResult := '';
    AResultInfo := nil;
    Exit(True);
  end;

  if AText = '""'
  then begin
    AResult := '0x0';
    AResultInfo := TGDBType.Create(skPointer, '^character');
    Exit(True);
  end;

  Result := ADebuggerCommand.ExecuteCommand('-data-evaluate-expression %s', [AText], R)
        and (R.State <> dsError);

  ResultList := TGDBMINameValueList.Create(R);
  if R.State = dsError
  then AResult := ResultList.Values['msg']
  else AResult := ResultList.Values['value'];
//  AResult := DeleteEscapeChars(AResult);
  ResultList.Free;
  if Result
  then AResultInfo := ADebuggerCommand.GetGDBTypeInfo(AText)
  else AResultInfo := nil;

  if AResultInfo = nil then Exit;

  //post format some results (for inscance a char is returned as "ord 'Value'"
  if AResultInfo.Kind <> skSimple then Exit;

  case StringCase(AResultInfo.TypeName, ['character'], true, false) of
    0: AResult := GetPart([' '], [], AResult);
  end;
end;

procedure TGDBMIExpression.Pop(var AResult: PGDBMIExpressionResult);
begin
  AResult := FStackPtr;
  if AResult = nil then Exit;
  FStackPtr := AResult^.Prev;
  if FStackPtr = nil
  then FStack := nil;
  AResult^.Next := nil;
  AResult^.Prev := nil;
end;

procedure TGDBMIExpression.Push(var AResult: PGDBMIExpressionResult);
begin
  if FStack = nil
  then begin
    FStack := AResult;
    FStackPtr := AResult;
  end
  else begin
    FStackPtr^.Next := AResult;
    AResult^.Prev := FStackPtr;
    FStackPtr := AResult;
  end;

  New(AResult);
  FillByte(AResult^, SizeOf(AResult^), 0);
end;

function TGDBMIExpression.Solve(const ADebuggerCommand: TGDBMIDebuggerCommand; ALimit: Byte; const ARight: String; out AValue: String; out AInfo: TGDBType): Boolean;
var
  StartPtr, Left: PGDBMIExpressionResult;
  Right: TGDBMIExpressionResult;
  Value: String;
  Info: TGDBType;
begin
  StartPtr := FStackPtr;
  while (ALimit >= OPER_LEVEL[StartPtr^.OperTor]) and (StartPtr^.Prev <> nil) do
    StartPtr := StartPtr^.Prev;

  // we will solve this till end of stack
  FStackPtr := StartPtr^.Prev;
  if FStackPtr = nil
  then FStack := nil
  else FStackPtr^.Next := nil;
  StartPtr^.Prev := nil;

  Left := StartPtr;
  FillChar(Right, SizeOf(Right), 0);

  repeat
    Info := nil;
    Value := '';
    case Left^.Opertor of
      eoNone: begin
        // only posible as first and only item on stack
        Result := (FStackPtr = nil) and (Left = StartPtr) and (ARight = '');
        if Result
        then begin
          Value := Left^.Value;
          Info := Left^.Info;
          Left^.Info := nil;
        end;
      end;
      eoNegate, eoPlus, eoSubstract, eoAdd,
      eoMultiply, eoPower, eoDivide, eoEqual,
      eoLess, eoLessOrEqual, eoGreater, eoGreaterOrEqual,
      eoNotEqual, eoAnd, eoOr, eoMod,
      eoNot, eoDiv, eoXor, eoShl,
      eoShr: begin
        if Left^.Next = nil
        then begin
          Result := Evaluate(ADebuggerCommand, ARight, Right.Value, Right.Info)
                and SolveMath(ADebuggerCommand, Left, @Right, Value, Info);
          FreeAndNil(Right.Info);
        end
        else Result := SolveMath(ADebuggerCommand, Left, Left^.Next, Value, Info);
      end;
      eoDereference: begin
        Result := (ARight = '') // right part can not have value
              and SolveDeref(ADebuggerCommand, Left, Value, Info);
      end;
      eoAddress: begin
        Result := (Left^.Info = nil);
        if not Result then Break;

        if Left^.Next = nil
        then begin
          Result := Evaluate(ADebuggerCommand, ARight, Right.Value, Right.Info)
                and SolveAddress(ADebuggerCommand, @Right, Value, Info);
          FreeAndNil(Right.Info);
        end
        else Result := SolveIn(ADebuggerCommand, Left, Left^.Next, Value, Info);
      end;
      eoDot: begin
        // its impossible to have next already resolved. Its a member of left
        Result := (Left^.Next = nil) and SolveDot(ADebuggerCommand, Left, ARight, Value, Info);
      end;
//    eoComma: begin
//    end;
      eoBracket: begin
        Result := Evaluate(ADebuggerCommand, ARight, Value, Info);
        // we can finish when closed
      end;
      eoIndex: begin
        if Left^.Info = nil
        then begin
          // only possible when part of "in"
          Result := (Left^.Prev <> nil)
                and (Left^.Prev^.OperTor = eoIn)
                and Evaluate(ADebuggerCommand, ARight, Value, Info);
        end
        else begin
          Result := Evaluate(ADebuggerCommand, ARight, Value, Info);
          // we can finish when closed
        end;
      end;
      eoIn: begin
        if Left^.Next = nil
        then begin
          Result := Evaluate(ADebuggerCommand, ARight, Right.Value, Right.Info)
                and SolveIn(ADebuggerCommand, Left, @Right, Value, Info);
          FreeAndNil(Right.Info);
        end
        else Result := SolveIn(ADebuggerCommand, Left, Left^.Next, Value, Info);
      end;
      eoIs: begin
        if Left^.Next = nil
        then begin
          Result := Evaluate(ADebuggerCommand, ARight, Right.Value, Right.Info)
                and SolveIs(ADebuggerCommand, Left, @Right, Value, Info);
          FreeAndNil(Right.Info);
        end
        else Result := SolveIs(ADebuggerCommand, Left, Left^.Next, Value, Info);
      end;
      eoAs: begin
        if Left^.Next = nil
        then begin
          Result := Evaluate(ADebuggerCommand, ARight, Right.Value, Right.Info)
                and SolveAs(ADebuggerCommand, Left, @Right, Value, Info);
          FreeAndNil(Right.Info);
        end
        else Result := SolveAs(ADebuggerCommand, Left, Left^.Next, Value, Info);
      end;
    else
      Result := False;
    end;

    if not Result then Break;
    if Left^.Next = nil then Break;
    Left := Left^.Next;

    Left^.Info.Free;
    Left^.Info := Info;
    Left^.Value := Value;
  until False;

  DisposeList(StartPtr);
  if Result
  then begin
    AValue := Value;
    AInfo := Info;
  end
  else begin
    AValue := '';
    AInfo := nil;
  end;
end;

function TGDBMIExpression.SolveAddress(const ADebuggerCommand: TGDBMIDebuggerCommand; ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
begin
  Result := False;
end;

function TGDBMIExpression.SolveAs(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
begin
  Result := False;
end;

function TGDBMIExpression.SolveDeref(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
var
  Eval: String;
begin
  Result := ALeft^.Info.Kind = skPointer;
  if not Result then Exit;

  Eval := '^' + ALeft^.Info.TypeName + '(' + ALeft^.Value + ')^';
  Result := Evaluate(ADebuggerCommand, Eval, AValue, AInfo);
end;

function TGDBMIExpression.SolveDot(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft: PGDBMIExpressionResult; const ARight: String; out AValue: String; out AInfo: TGDBType): Boolean;
var
  Prefix: String;
begin
  if not (ALeft^.Info.Kind in [skClass, skRecord]) then Exit(False);

  Prefix := '^' + ALeft^.Info.TypeName + '(' + ALeft^.Value + ')^.';
  Result := Evaluate(ADebuggerCommand, Prefix + ARight, AValue, AInfo);
  if Result then Exit;

  // maybe property
  Result := Evaluate(ADebuggerCommand, Prefix + 'F' + ARight, AValue, AInfo);

  //todo: method call
end;

function TGDBMIExpression.SolveIn(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
begin
  Result := False;
end;

function TGDBMIExpression.SolveIs(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
begin
  Result := False;
end;

function TGDBMIExpression.SolveMath(const ADebuggerCommand: TGDBMIDebuggerCommand; ALeft, ARight: PGDBMIExpressionResult; out AValue: String; out AInfo: TGDBType): Boolean;
const
  OPERATOR_TEXT: array[TDBGExpressionOperator] of string = (
    {eoNone            } '',
    {eoNegate          } '-',
    {eoPlus            } '',
    {eoSubstact        } '-',
    {eoAdd             } '+',
    {eoMultiply        } '*',
    {eoPower           } '',
    {eoDivide          } '/',
    {eoDereference     } '',
    {eoAddress         } '',
    {eoEqual           } '=',
    {eoLess            } '<',
    {eoLessOrEqual     } '<=',
    {eoGreater         } '>',
    {eoGreaterOrEqual  } '>=',
    {eoNotEqual        } '<>',
    {eoIn              } '',
    {eoIs              } '',
    {eoAs              } '',
    {eoDot             } '',
    {eoComma           } '',
    {eoBracket         } '',
    {eoIndex           } '',
    {eoClose           } '',
    {eoAnd             } 'and',
    {eoOr              } 'or',
    {eoMod             } 'mod',
    {eoNot             } 'not',
    {eoDiv             } 'div',
    {eoXor             } 'xor',
    {eoShl             } 'shl',
    {eoShr             } 'shr'
  );
var
  Eval: String;
begin
  case ALeft^.Opertor of
    eoAnd, eoOr, eoMod, eoNot,
    eoDiv, eoXor, eoShl,  eoShr: begin
      Eval := '(' + ALeft^.Value + ')' + OPERATOR_TEXT[ALeft^.Opertor] + '(' + ARight^.Value + ')';
    end
  else
    Eval := ALeft^.Value + OPERATOR_TEXT[ALeft^.Opertor] + ARight^.Value;
  end;

  Result := Evaluate(ADebuggerCommand, Eval, AValue, AInfo);
end;

{ TGDBStringIterator }

constructor TGDBStringIterator.Create(const AParsableData: String);
begin
  inherited Create;
  FParsableData := AParsableData;
  FReadPointer := 1;
  FDataSize := Length(AParsableData);
  DebugLn(AParsableData);
end;

function TGDBStringIterator.ParseNext(out ADecomposable: Boolean; out
  APayload: String; out ACharStopper: Char): Boolean;
var
  InStr: Boolean;
  InBrackets1, InBrackets2: Integer;
  c: Char;
  BeginString: Integer;
  EndString: Integer;
begin
  ADecomposable := False;
  InStr := False;
  InBrackets1 := 0;
  InBrackets2 := 0;
  BeginString := FReadPointer;
  EndString := FDataSize;
  ACharStopper := #0; //none
  while FReadPointer <= FDataSize do 
  begin
    c := FParsableData[FReadPointer];
    if c = '''' then InStr := not InStr;
    if not InStr 
    then begin
      case c of
        '{': Inc(InBrackets1);
        '}': Dec(InBrackets1);
        '[': Inc(InBrackets2);
        ']': Dec(InBrackets2);
      end;
      
      if (InBrackets1 = 0) and (InBrackets2 = 0) and (c in [',', '='])
      then begin
        EndString := FReadPointer - 1;
        Inc(FReadPointer); //Skip this char
        ACharStopper := c;
        Break;
      end;
    end;
    Inc(FReadPointer);
  end;
  
  //Remove boundary spaces.
  while BeginString<EndString do 
  begin
    if FParsableData[BeginString] <> ' ' then break;
    Inc(BeginString);
  end;
  
  while EndString > BeginString do 
  begin
    if FParsableData[EndString] <> ' ' then break;
    Dec(EndString);
  end;
  
  if (EndString - BeginString > 0)
  and (FParsableData[BeginString] = '{')
  then begin
    inc(BeginString);
    dec(EndString);
    ADecomposable := True;
  end;
  
  APayload := Copy(FParsableData, BeginString, EndString - BeginString + 1);
  Result := Length(APayload) > 0;
end;

{ TGDBMIDebuggerCommand }

function TGDBMIDebuggerCommand.GetDebuggerState: TDBGState;
begin
  Result := FTheDebugger.State;
end;

function TGDBMIDebuggerCommand.GetDebuggerProperties: TGDBMIDebuggerProperties;
begin
  Result := TGDBMIDebuggerProperties(FTheDebugger.GetProperties);
end;

function TGDBMIDebuggerCommand.GetTargetInfo: PGDBMITargetInfo;
begin
  Result := @FTheDebugger.FTargetInfo;
end;

procedure TGDBMIDebuggerCommand.SetDebuggerState(const AValue: TDBGState);
begin
  FTheDebugger.SetState(AValue);
end;

procedure TGDBMIDebuggerCommand.SetDebuggerErrorState(const AMsg: String;
  const AInfo: String);
begin
  FTheDebugger.SetErrorState(AMsg, AInfo);
end;

function TGDBMIDebuggerCommand.ErrorStateMessage: String;
begin
  Result := '';
  if ehfGotWriteError in FTheDebugger.FErrorHandlingFlags
  then Result := Result + Format(gdbmiErrorStateInfoFailedWrite, [LineEnding])
  else
  if ehfGotReadError in FTheDebugger.FErrorHandlingFlags
  then Result := Result + Format(gdbmiErrorStateInfoFailedRead, [LineEnding]);

  if not FTheDebugger.DebugProcessRunning
  then Result := Result + Format(gdbmiErrorStateInfoGDBGone, [LineEnding]);
end;

function TGDBMIDebuggerCommand.ErrorStateInfo: String;
begin
  Result := Format(gdbmiErrorStateGenericInfo, [LineEnding, DebugText]);
  if FLastExecResult.Values = ''
  then Result := Format(gdbmiErrorStateInfoCommandNoResult, [LineEnding, FLastExecCommand])
  else Result := Format(gdbmiErrorStateInfoCommandError, [LineEnding, FLastExecCommand, FLastExecResult.Values]);
  if not FTheDebugger.DebugProcessRunning
  then Result := Result + Format(gdbmiErrorStateInfoGDBGone, [LineEnding]);
end;

procedure TGDBMIDebuggerCommand.SetCommandState(NewState: TGDBMIDebuggerCommandState);
var
  OldState: TGDBMIDebuggerCommandState;
begin
  if FState = NewState
  then exit;
  OldState := FState;
  FState := NewState;
  Include(FSeenStates, NewState);
  DoStateChanged(OldState);
  if (State in [dcsFinished, dcsCanceled]) and not(dcsInternalRefReleased in FSeenStates)
  then begin
    Include(FSeenStates, dcsInternalRefReleased);
    ReleaseReference; //internal reference
  end;
end;

procedure TGDBMIDebuggerCommand.DoStateChanged(OldState: TGDBMIDebuggerCommandState);
begin
  // nothing
end;

procedure TGDBMIDebuggerCommand.DoLockQueueExecute;
begin
  FTheDebugger.QueueExecuteLock;
end;

procedure TGDBMIDebuggerCommand.DoUnockQueueExecute;
begin
  FTheDebugger.QueueExecuteUnlock;
end;

procedure TGDBMIDebuggerCommand.DoOnExecuted;
begin
  if assigned(FOnExecuted) then
    FOnExecuted(self);
end;

procedure TGDBMIDebuggerCommand.DoCancel;
begin
  // empty
end;

procedure TGDBMIDebuggerCommand.DoOnCanceled;
begin
  if assigned(FOnCancel) then
    FOnCancel(self);
end;

function TGDBMIDebuggerCommand.ExecuteCommand(const ACommand: String;
  AFlags: TGDBMICommandFlags = []; ATimeOut: Integer = -1): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommand(ACommand, R, AFlags, ATimeOut);
end;

function TGDBMIDebuggerCommand.ExecuteCommand(const ACommand: String;
  out AResult: TGDBMIExecResult; AFlags: TGDBMICommandFlags = [];
  ATimeOut: Integer = -1): Boolean;

  function RevorerTimeOut: Boolean;
  var
    R, R2: TGDBMIExecResult;
    List: TGDBMINameValueList;
    Got7: Boolean;
  begin
    Result := False;
    List := nil;
    try
      AResult.State := dsError;
      // send 2 commands: - if the "7" is received, it could be the original command
      //                  - but if the "1" is received, after the "7" we know we are in sync
      FTheDebugger.SendCmdLn('-data-evaluate-expression 7');
      FTheDebugger.SendCmdLn('-data-evaluate-expression 1');

      // Not expected to reach it's timeout, so we can use a high value.
      if not ProcessResult(R, Max(2*ATimeOut, 2500))
      then exit;

      // Got either:  Result for origonal "ACommand" (could be "7" too)   OR   got "7"
      List := TGDBMINameValueList.Create(R);
      Got7 := List.Values['value'] = '7';

      // Check next result,
      if not ProcessResult(R2, 500)
      then exit;

      // Got either:  "7"  OR  "1"
      // "1" => never got original result, but recovery was ok
      // "7" again => maybe recovery, must be followed by a "1" then
      List.Init(R2.Values);

      if Got7 and (List.Values['value'] = '1')
      then begin
        // timeout, without value, but recovery
        Result := True;
        DoDbgEvent(ecDebugger, etDefault, Format(gdbmiTimeOutForCmd, [ACommand]));
        // TODO: use feedback dialog
        FLastExecwasTimeOut := True;
        DoTimeoutFeedback;
      end
      else
      if List.Values['value'] = '7'
      then begin
        // Got a 2nd "7", check for a "1"
        if not ProcessResult(R2, 500)
        then exit;
        List.Init(R2.Values);
        if not(List.Values['value'] = '1')
        then exit;
        // full recovery, even got orig result
        Result := True;
        AResult := R;
      end;
    finally
      FreeAndNil(List);
    end;
  end;

begin
  AResult.Values := '';
  AResult.State := dsNone;
  AResult.Flags := [];
  FLastExecCommand := ACommand;
  FLastExecwasTimeOut := False;

  if (ATimeOut = -1) and (DefaultTimeOut > 0)
  then ATimeOut := DefaultTimeOut;

  try
    FTheDebugger.FErrorHandlingFlags := FTheDebugger.FErrorHandlingFlags
      + [ehfDeferReadWriteError] - [ehfGotReadError, ehfGotWriteError];

    FTheDebugger.SendCmdLn(ACommand);
    if ehfGotWriteError in FTheDebugger.FErrorHandlingFlags then begin
      ProcessResult(AResult, 50); // not expecting anything
      Result := False;
    end
    else begin
      Result := ProcessResult(AResult, ATimeOut);
      FLastExecResult := AResult;

      if ProcessResultTimedOut then
      Result := RevorerTimeOut;
    end;
  finally
    Exclude(FTheDebugger.FErrorHandlingFlags, ehfDeferReadWriteError);
  end;

  if not Result
  then begin
    // either gdb did not return a Result Record: "^xxxx,"
    // or the Result Record was not a known one: 'done', 'running', 'exit', 'error'
    DebugLn('[WARNING] TGDBMIDebugger:  ExecuteCommand "',ACommand,'" failed.');
    SetDebuggerErrorState(ErrorStateMessage, ErrorStateInfo);
    AResult.State := dsError;
  end;

  if (cfCheckError in AFlags) and (AResult.State = dsError)
  then SetDebuggerErrorState(ErrorStateMessage, ErrorStateInfo);

  if (cfCheckState in AFlags) and not (AResult.State in [dsError, dsNone])
  then SetDebuggerState(AResult.State);
end;

function TGDBMIDebuggerCommand.ExecuteCommand(const ACommand: String;
  const AValues: array of const; AFlags: TGDBMICommandFlags;
  ATimeOut: Integer = -1): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommand(ACommand, AValues, R, AFlags, ATimeOut);
end;

function TGDBMIDebuggerCommand.ExecuteCommand(const ACommand: String;
  const AValues: array of const; out AResult: TGDBMIExecResult;
  AFlags: TGDBMICommandFlags = []; ATimeOut: Integer = -1): Boolean;
begin
  Result := ExecuteCommand(Format(ACommand, AValues), AResult, AFlags, ATimeOut);
end;

procedure TGDBMIDebuggerCommand.DoTimeoutFeedback;
begin
  if DebuggerProperties.WarnOnTimeOut
  then MessageDlg('Warning', 'A timeout occured, the debugger will try to continue, but further error may occur later',
                  mtWarning, [mbOK], 0);
end;

function TGDBMIDebuggerCommand.ProcessResult(var AResult: TGDBMIExecResult;ATimeOut: Integer = -1): Boolean;
var
  InLogWarning: Boolean;

  function DoResultRecord(Line: String; CurRes: Boolean): Boolean;
  var
    ResultClass: String;
    OldResult: Boolean;
  begin
    ResultClass := GetPart('^', ',', Line);

    if Line = ''
    then begin
      if AResult.Values <> ''
      then Include(AResult.Flags, rfNoMI);
    end
    else begin
      AResult.Values := Line;
    end;

    OldResult := CurRes;
    Result := True;
    case StringCase(ResultClass, ['done', 'running', 'exit', 'error']) of
      0: begin // done
      end;
      1: begin // running
        AResult.State := dsRun;
      end;
      2: begin // exit
        AResult.State := dsIdle;
      end;
      3: begin // error
        DebugLn('TGDBMIDebugger.ProcessResult Error: ', Line);
        // todo: implement with values
        if  (pos('msg=', Line) > 0)
        and (pos('not being run', Line) > 0)
        then AResult.State := dsStop
        else AResult.State := dsError;
      end;
    else
      //TODO: should that better be dsError ?
      if OldResult and (AResult.State in [dsError, dsStop]) and
         (copy(ResultClass,1,6) = 'error"')
      then begin
        // Gdb 6.3.5 on Mac, does sometime return a 2nd mis-formatted error line
        // The line seems truncated, it simply is (note the misplaced quote): ^error"
        DebugLn('[WARNING] Debugger: Unknown result class (IGNORING): ', ResultClass);
      end
      else begin
        Result := False;
        DebugLn('[WARNING] Debugger: Unknown result class: ', ResultClass);
      end;
    end;
  end;

  procedure DoConsoleStream(Line: String);
  var
    len: Integer;
  begin
    // check for symbol info
    if Pos('no debugging symbols', Line) > 0
    then begin
      TargetInfo^.TargetFlags := TargetInfo^.TargetFlags - [tfHasSymbols];
      DoDbgEvent(ecDebugger, etDefault, Format('File ''%s'' has no debug symbols', [FTheDebugger.FileName]));
    end
    else begin
      // Strip surrounding ~" "
      len := Length(Line) - 3;
      if len < 0 then Exit;
      Line := Copy(Line, 3, len);
      // strip trailing \n (unless it is escaped \\n)
      if (len >= 2) and (Line[len - 1] = '\') and (Line[len] = 'n')
      then begin
        if len = 2
        then Line := LineEnding
        else if Line[len - 2] <> '\'
        then begin
          SetLength(Line, len - 2);
          Line := Line + LineEnding;
        end;
      end;

      AResult.Values := AResult.Values + Line;
    end;
  end;

  procedure DoTargetStream(const Line: String);
  begin
    DebugLn('[Debugger] Target output: ', Line);
  end;

  procedure DoLogStream(const Line: String);
  const
    LogWarning = '&"Warning:\n"';
  begin
    DebugLn('[Debugger] Log output: ', Line);
    if Line = '&"kill\n"'
    then AResult.State := dsStop
    else if LeftStr(Line, 8) = '&"Error '
    then AResult.State := dsError;
    if copy(Line, 1, length(LogWarning)) = LogWarning
    then InLogWarning := True;
    if InLogWarning
    then FLogWarnings := FLogWarnings + copy(Line, 3, length(Line)-5) + LineEnding;
    if copy(Line, 1, length(LogWarning)) = '&"\n"'
    then InLogWarning := False;
  end;

  procedure DoExecAsync(Line: String);
  var
    EventText: String;
  begin
    EventText := GetPart(['*'], [','], Line, False, False);
    if EventText = 'running'
    then
      DoDbgEvent(ecProcess, etProcessStart, 'Process Start: ' + FTheDebugger.FileName)
    else
      DebugLn('[WARNING] Debugger: Unexpected async-record: ', Line);
  end;

  procedure DoStatusAsync(const Line: String);
  begin
    DebugLn('[WARNING] Debugger: Unexpected async-record: ', Line);
  end;

var
  S: String;
begin
  Result := False;
  FProcessResultTimedOut := False;
  AResult.Values := '';
  AResult.Flags := [];
  AResult.State := dsNone;
  InLogWarning := False;
  FLogWarnings := '';
  repeat
    S := FTheDebugger.ReadLine(ATimeOut);
    if S = '(gdb) ' then Break;

    if s <> ''
    then case S[1] of
      '^': Result := DoResultRecord(S, Result);
      '~': DoConsoleStream(S);
      '@': DoTargetStream(S);
      '&': DoLogStream(S);
      '*': DoExecAsync(S);
      '+': DoStatusAsync(S);
      '=': FTheDebugger.DoNotifyAsync(S);
    else
      DebugLn('[WARNING] Debugger: Unknown record: ', S);
    end;
    {$IFDEF VerboseIDEToDo}{$message warning condition should also check end-of-file reached for process output stream}{$ENDIF}
    if FTheDebugger.ReadLineTimedOut
    then begin
      FProcessResultTimedOut := True;
      Result := False;
      break;
    end;
  until not FTheDebugger.DebugProcessRunning;
end;

function TGDBMIDebuggerCommand.ProcessGDBResultText(S: String): String;
var
  Trailor: String;
  n, len, idx: Integer;
  v: Integer;
begin

  // don't use ' as end terminator, there might be one as part of the text
  // since ' will be the last char, simply strip it.
  S := GetPart(['\t '], [], S);

  // Scan the string
  len := Length(S);
  // Set the resultstring initially to the same size
  SetLength(Result, len);
  n := 0;
  idx := 1;
  Trailor:='';
  while idx <= len do
  begin
    case S[idx] of
      '''': begin
        Inc(idx);
        // scan till end
        while idx <= len do
        begin
          case S[idx] of
            '''' : begin
              Inc(idx);
              if idx > len then Break;
              if S[idx] <> '''' then Break;
            end;
            '\' : begin
              Inc(idx);
              if idx > len then Break;
              case S[idx] of
                't': S[idx] := #9;
                'n': S[idx] := #10;
                'r': S[idx] := #13;
              end;
            end;
          end;
          Inc(n);
          Result[n] := S[idx];
          Inc(idx);
        end;
      end;
      '#': begin
        Inc(idx);
        v := 0;
        // scan till non number (correct input is assumed)
        while (idx <= len) and (S[idx] >= '0') and (S[idx] <= '9') do
        begin
          v := v * 10 + Ord(S[idx]) - Ord('0');
          Inc(idx)
        end;
        Inc(n);
        Result[n] := Chr(v and $FF);
      end;
      ',', ' ': begin
        Inc(idx); //ignore them;
      end;
      '<': begin
        // Debugger has returned something like <repeats 10 times>
        v := StrToIntDef(GetPart(['<repeats '], [' times>'], S), 0);
        // Since we deleted the first part of S, reset idx
        idx := 8; // the char after ' times>'
        len := Length(S);
        if v <= 1 then Continue;

        // limit the amount of repeats
        if v > 1000
        then begin
          Trailor := Trailor + Format('###(repeat truncated: %u -> 1000)###', [v]);
          v := 1000;
        end;

        // make sure result has some room
        SetLength(Result, Length(Result) + v - 1);
        while v > 1 do begin
          Inc(n);
          Result[n] := Result[n - 1];
          Dec(v);
        end;
      end;
    else
      // Debugger has returned something we don't know of
      // Append the remainder to our parsed result
      Delete(S, 1, idx - 1);
      Trailor := Trailor + '###(gdb unparsed remainder:' + S + ')###';
      Break;
    end;
  end;
  SetLength(Result, n);
  Result := Result + Trailor;
end;

function TGDBMIDebuggerCommand.GetStackDepth(MaxDepth: integer): Integer;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
begin
  Result := -1;
  if (MaxDepth < 0) and (not ExecuteCommand('-stack-info-depth', R))
  then exit;
  if (MaxDepth >= 0) and (not ExecuteCommand('-stack-info-depth %d', [MaxDepth], R))
  then exit;
  if R.State = dsError
  then exit;

  List := TGDBMINameValueList.Create(R);
  Result := StrToIntDef(List.Values['depth'], -1);
  FreeAndNil(List);
end;

function TGDBMIDebuggerCommand.FindStackFrame(FP: TDBGPtr; StartAt,
  MaxDepth: Integer): Integer;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
  Cur, Prv: QWord;
begin
  // Result;
  // -1 : Not found
  // -2 : FP is outside stack
  Result := StartAt;
  Cur := 0;
  List := TGDBMINameValueList.Create('');
  try
    repeat
      if not ExecuteCommand('-stack-select-frame %u', [Result], R)
      or (R.State = dsError)
      then begin
        Result := -1;
        break;
      end;

      if not ExecuteCommand('-data-evaluate-expression $fp', R)
      or (R.State = dsError)
      then begin
        Result := -1;
        break;
      end;

      List.Init(R.Values);
      Prv := Cur;
      Cur := StrToQWordDef(List.Values['value'], 0);
      if Fp = Cur then begin
        exit;
      end;

      if (Prv <> 0) and (Prv < Cur)
      then begin
        // FP is increasing
        if FP < Prv
        then begin
          Result := -2;
          exit;
        end;
      end;
      if (Prv <> 0) and (Prv > Cur)
      then begin
        // FP is decreasing
        if FP > Prv
        then begin
          Result := -2;
          exit;
        end;
      end;

      inc(Result);
    until Result > MaxDepth;

    Result := -1;
  finally
    List.Free;
  end;
end;

function TGDBMIDebuggerCommand.GetFrame(const AIndex: Integer): String;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
begin
  Result := '';
  if ExecuteCommand('-stack-list-frames %d %d', [AIndex, AIndex], R)
  then begin
    List := TGDBMINameValueList.Create(R, ['stack']);
    Result := List.Values['frame'];
    List.Free;
  end;
end;

function TGDBMIDebuggerCommand.GetText(const ALocation: TDBGPtr): String;
var
  S: String;
begin
  Str(ALocation, S);
  Result := GetText(S, []);
end;

function TGDBMIDebuggerCommand.GetText(const AExpression: String;
  const AValues: array of const): String;
var
  R: TGDBMIExecResult;
begin
  if not ExecuteCommand('x/s ' + AExpression, AValues, R, [],
                       DebuggerProperties.TimeoutForEval)
  then begin
    Result := '';
    Exit;
  end;
  Result := ProcessGDBResultText(StripLN(R.Values));
end;

function TGDBMIDebuggerCommand.GetChar(const AExpression: String;
  const AValues: array of const): String;
var
  R: TGDBMIExecResult;
begin
  if not ExecuteCommand('x/c ' + AExpression, AValues, R)
  then begin
    Result := '';
    Exit;
  end;
  Result := ProcessGDBResultText(StripLN(R.Values));
end;

function TGDBMIDebuggerCommand.GetFloat(const AExpression: String;
  const AValues: array of const): String;
var
  R: TGDBMIExecResult;
begin
  if not ExecuteCommand('x/f ' + AExpression, AValues, R)
  then begin
    Result := '';
    Exit;
  end;
  Result := ProcessGDBResultText(StripLN(R.Values));
end;

function TGDBMIDebuggerCommand.GetWideText(const ALocation: TDBGPtr): String;

  function GetWideChar(const ALocation: TDBGPtr): WideChar;
  var
    Address, S: String;
    R: TGDBMIExecResult;
  begin
    Str(ALocation, Address);
    if not ExecuteCommand('x/uh' + Address, [], R)
    then begin
      Result := #0;
      Exit;
    end;
    S := StripLN(R.Values);
    S := GetPart(['\t'], [], S);
    Result := WideChar(StrToIntDef(S, 0) and $FFFF);
  end;
var
  OneChar: WideChar;
  CurLocation: TDBGPtr;
  WStr: WideString;
begin
  WStr := '';
  CurLocation := ALocation;
  repeat
    OneChar := GetWideChar(CurLocation);
    if OneChar <> #0 then
    begin
      WStr := WStr + OneChar;
      CurLocation := CurLocation + 2;
    end;
  until (OneChar = #0);
  Result := UTF16ToUTF8(WStr);
end;

function TGDBMIDebuggerCommand.GetGDBTypeInfo(const AExpression: String;
  FullTypeInfo: Boolean = False; AFlags: TGDBTypeCreationFlags = [];
  AFormat: TWatchDisplayFormat = wdfDefault): TGDBType;
var
  R: TGDBMIExecResult;
  f: Boolean;
  AReq: PGDBPTypeRequest;
  CReq: TGDBPTypeRequest;
  i: Integer;
begin
  (*   Analyze what type is in AExpression
     * "whatis AExpr"
       This return the declared type of the expression (as in the pascal source)
       - The type may be replaced:
         - type TAlias = TOriginal; // TAlias may be reported as TOriginal
           type TAlias = type TOriginal; // Not guranteed, but not likely to be replaced
                                       // This leaves room for arbitraty names for all types
         - ^TFoo may be replaced by PFF, if PFF exist and is ^TFoo (seen with stabs, not dwarf)
       - The type may be prefixed by "&" for var param under dwarf (an fpc workaround)
         Under dwarf var param are hnadled by gdb, if casted or part of an expression,
           but not if standalone or dereferred ("^") only
         Under stabs "var param" have no indications, but are completely and correctly
           handled by gdb

     * ptype TheWhatisType
       Should return the base type info
       Since under dwarf classes are always pointers (again work in expression,
         but not standalone); a further "whatis" on the declared-type may be needed,
         to check if the type is a pointer or not.
         This may be limited, if types are strongly aliased over several levels...

     * tfClassIsPointer in TargetFlags
       usually true for dwarf, false for stabs. Can be detected with "ptype TObject"
       Dwarf:
         "ptype TObject" => ~"type = ^TOBJECT = class \n"
       Stabs:
         "ptype TObject" => ~ ~"type = TOBJECT = class \n"

     * Examples
       * Type-info for objects
         TFoo = Tobject; PFoo = ^TFoo;
         ArgTFoo: TFoo;    ArgPFoo: PFoo
         Dwarf:
           "whatis ArgTFoo\n" => ~"type = TFOO\n"    (for var-param ~"type = &TFOO\n")
           "ptype TFoo\n"     => ~"type = ^TFOO = class : public TOBJECT \n"

           whatis ArgPFoo\n"  => ~"type = PFOO\n"
           "ptype PFoo\n"     => ~"type = ^TFOO = class : public TOBJECT \n"

           // ptype is the same for TFoo and PFoo, so we need to find out if any is a pointer:
           // they both have "^", but PFoo does not have "= class"
           // (this may fial if pfoo is an alias for yet another name)
           "whatis TFoo\n"    => ~"type = ^TFOO = class \n"
           "whatis PFoo\n"    => ~"type = ^TFOO\n"

         Stabs:
           "whatis ArgTFoo\n" => ~"type = TFOO\n"    (same vor var param)
           "ptype TFoo\n"     => ~"type = TFOO = class : public TOBJECT \n"

           "whatis ArgPFoo\n" => ~"type = PFOO\n"
           ptype PFoo\n"      => ~"type = ^TFOO = class : public TOBJECT \n"

           // ptype gives desired info in stabs (and whatis, does not reveal anything)
           "whatis TFoo\n"    => ~"type = TFOO\n"
           "whatis PFoo\n"    => ~"type = PFOO\n"

         Limitations: Under Mac gdb 6.3.50 "whatis" does not work on types.
                      The info can not be obtained (with Dwarf: PFoo will be treated the same as TFoo)
       *

  *)

  if tfClassIsPointer in TargetInfo^.TargetFlags
  then AFlags := AFlags + [gtcfClassIsPointer];
  if FullTypeInfo
  then AFlags := AFlags + [gtcfFullTypeInfo];
  Result := TGdbType.CreateForExpression(AExpression, AFlags);
  while not Result.ProcessExpression do begin
    if Result.EvalError
    then break;
    AReq := Result.EvalRequest;
    while AReq <> nil do begin
      if (dcsCanceled in SeenStates) then begin
        FreeAndNil(Result);
        exit;
      end;

      i := FTheDebugger.FTypeRequestCache.IndexOf
        (FTheDebugger.FInternalThreadId, FTheDebugger.FInternalStackFrame, AReq^);
      if i >= 0 then begin
        {$IFDEF DBGMI_QUEUE_DEBUG}
        DebugLn(['DBG TypeRequest-Cache: Found entry for T=',  FTheDebugger.FInternalThreadId,
          ' F=', FTheDebugger.FInternalStackFrame, ' R="', AReq^.Request,'"']);
        {$ENDIF}
        CReq := FTheDebugger.FTypeRequestCache.Request[i];
        AReq^.Result := CReq.Result;
        AReq^.Error := CReq.Error;
      end
      else begin
        f :=  ExecuteCommand(AReq^.Request, R);
        if f and (R.State <> dsError) then begin
          if AReq^.ReqType = gcrtPType
          then AReq^.Result := ParseTypeFromGdb(R.Values)
          else begin
            AReq^.Result.GdbDescription := R.Values;
            AReq^.Result.Kind := ptprkSimple;
          end;
        end
        else begin
          AReq^.Result.GdbDescription := R.Values;
          AReq^.Error := R.Values;
        end;

        FTheDebugger.FTypeRequestCache.Add
          (FTheDebugger.FInternalThreadId, FTheDebugger.FInternalStackFrame, AReq^);
      end;

      AReq := AReq^.Next;
    end;
  end;

  if Result.EvalError then begin
    FreeAndNil(Result);
  end;
end;

function TGDBMIDebuggerCommand.GetClassName(const AClass: TDBGPtr): String;
var
  S: String;
begin
  // format has a problem with %u, so use Str for it
  Str(AClass, S);
  Result := GetClassName(S, []);
end;

function TGDBMIDebuggerCommand.GetClassName(const AExpression: String;
  const AValues: array of const): String;
var
  OK: Boolean;
  S: String;
  R: TGDBMIExecResult;
  ResultList: TGDBMINameValueList;
  UseShortString: Boolean;
  i: Integer;
begin
  Result := '';
  UseShortString := False;

  if dfImplicidTypes in FTheDebugger.DebuggerFlags
  then begin
    S := Format(AExpression, AValues);
    UseShortString := tfFlagHasTypeShortstring in TargetInfo^.TargetFlags;
    if UseShortString
    then s := Format('^^shortstring(%s+%d)^^', [S, TargetInfo^.TargetPtrSize * 3])
    else s := Format('^^char(%s+%d)^', [S, TargetInfo^.TargetPtrSize * 3]);
    OK :=  ExecuteCommand('-data-evaluate-expression %s',
          [S], R);
    if (not OK) or (LastExecResult.State = dsError)
    or (pos('value="#0', LastExecResult.Values) > 0)
    then begin
      OK :=  ExecuteCommand('-data-evaluate-expression ^char(^pointer(%s+%d)^)',
             [S, TargetInfo^.TargetPtrSize * 3], R);
      UseShortString := False;
    end;
  end
  else begin
    UseShortString := True;
    Str(TDbgPtr(GetData(AExpression + '+12', AValues)), S);
    OK := ExecuteCommand('-data-evaluate-expression pshortstring(%s)^', [S], R);
  end;

  if OK
  then begin
    ResultList := TGDBMINameValueList.Create(R);
    S := ResultList.Values['value'];
    if UseShortString then begin
      Result := GetPart('''', '''', S);
    end
    else begin
      s := ParseGDBString(s);
      if s <> ''
      then i := ord(s[1])
      else i := 1;
      if i <= length(s)-1 then begin
        Result := copy(s, 2, i);
      end
      else begin
        // fall back
        S := DeleteEscapeChars(S);
        Result := GetPart('''', '''', S);
      end;
    end;

    ResultList.Free;
  end;
end;

function TGDBMIDebuggerCommand.GetInstanceClassName(const AInstance: TDBGPtr): String;
var
  S: String;
begin
  Str(AInstance, S);
  Result := GetInstanceClassName(S, []);
end;

function TGDBMIDebuggerCommand.GetInstanceClassName(const AExpression: String;
  const AValues: array of const): String;
begin
  if dfImplicidTypes in FTheDebugger.DebuggerFlags
  then begin
    Result := GetClassName('^' + PointerTypeCast + '(' + AExpression + ')^', AValues);
  end
  else begin
    Result := GetClassName(GetData(AExpression, AValues));
  end;
end;

function TGDBMIDebuggerCommand.GetData(const ALocation: TDbgPtr): TDbgPtr;
var
  S: String;
begin
  Str(ALocation, S);
  Result := GetData(S, []);
end;

function TGDBMIDebuggerCommand.GetData(const AExpression: String;
  const AValues: array of const): TDbgPtr;
var
  R: TGDBMIExecResult;
  e: Integer;
begin
  Result := 0;
  if ExecuteCommand('x/d ' + AExpression, AValues, R)
  then Val(StripLN(GetPart('\t', '', R.Values)), Result, e);
  if e=0 then ;
end;

function TGDBMIDebuggerCommand.GetStrValue(const AExpression: String;
  const AValues: array of const): String;
var
  R: TGDBMIExecResult;
  ResultList: TGDBMINameValueList;
begin
  if ExecuteCommand('-data-evaluate-expression %s', [Format(AExpression, AValues)], R)
  then begin
    ResultList := TGDBMINameValueList.Create(R);
    Result := DeleteEscapeChars(ResultList.Values['value']);
    ResultList.Free;
  end
  else Result := '';
end;

function TGDBMIDebuggerCommand.GetIntValue(const AExpression: String;
  const AValues: array of const): Integer;
var
  e: Integer;
begin
  Result := 0;
  Val(GetStrValue(AExpression, AValues), Result, e);
  if e=0 then ;
end;

function TGDBMIDebuggerCommand.GetPtrValue(const AExpression: String;
  const AValues: array of const; ConvertNegative: Boolean = False): TDbgPtr;
var
  e: Integer;
  i: Int64;
  s: String;
begin
  Result := 0;
  s := GetStrValue(AExpression, AValues);
  if (s <> '') and (s[1] = '-')
  then begin
    Val(s, i, e);
    Result := TDBGPtr(i);
  end
  else Val(s, Result, e);
  if e=0 then ;
end;

function TGDBMIDebuggerCommand.CheckHasType(TypeName: String;
  TypeFlag: TGDBMITargetFlag): TGDBMIExecResult;
begin
  if not ExecuteCommand('ptype %s', [TypeName], Result, [], DebuggerProperties.TimeoutForEval) then begin
    Result.State := dsError;
    exit;
  end;
  if (LeftStr(Result.Values, 6) = 'type =') then
    include(TargetInfo^.TargetFlags, TypeFlag);
end;

function TGDBMIDebuggerCommand.PointerTypeCast: string;
begin
  if tfFlagHasTypePointer in TargetInfo^.TargetFlags
  then Result := 'POINTER'
  // TODO: check dfImplicidTypes support?
  else if tfFlagHasTypeByte in TargetInfo^.TargetFlags
  then Result := '^byte'
  else Result := '^char';
end;

function TGDBMIDebuggerCommand.FrameToLocation(const AFrame: String): TDBGLocationRec;
var
  S: String;
  e: Integer;
  Frame: TGDBMINameValueList;
begin
  // Do we have a frame ?
  if AFrame = ''
  then S := GetFrame(0)
  else S := AFrame;

  Frame := TGDBMINameValueList.Create(S);

  Result.Address := 0;
  Val(Frame.Values['addr'], Result.Address, e);
  if e=0 then ;
  Result.FuncName := Frame.Values['func'];
  Result.SrcFile := ConvertGdbPathAndFile(Frame.Values['file']);
  Result.SrcFullName := ConvertGdbPathAndFile(Frame.Values['fullname']);
  Result.SrcLine := StrToIntDef(Frame.Values['line'], -1);

  Frame.Free;
end;

procedure TGDBMIDebuggerCommand.ProcessFrame(const ALocation: TDBGLocationRec);
begin
  FTheDebugger.DoCurrent(ALocation);
  FTheDebugger.FCurrentLocation := ALocation;
end;

procedure TGDBMIDebuggerCommand.ProcessFrame(const AFrame: String);
var
  Location: TDBGLocationRec;
begin
  Location := FrameToLocation(AFrame);
  ProcessFrame(Location);
end;

procedure TGDBMIDebuggerCommand.DoDbgEvent(const ACategory: TDBGEventCategory;
  const AEventType: TDBGEventType; const AText: String);
begin
  FTheDebugger.DoDbgEvent(ACategory, AEventType, AText);
end;

constructor TGDBMIDebuggerCommand.Create(AOwner: TGDBMIDebugger);
begin
  FQueueRunLevel := -1;
  FState := dcsNone;
  FTheDebugger := AOwner;
  FDefaultTimeOut := -1;
  FPriority := 0;
  FProperties := [];
  AddReference; // internal reference
end;

destructor TGDBMIDebuggerCommand.Destroy;
begin
  if assigned(FOnDestroy)
  then FOnDestroy(Self);
  inherited Destroy;
end;

procedure TGDBMIDebuggerCommand.DoQueued;
begin
  SetCommandState(dcsQueued);
end;

procedure TGDBMIDebuggerCommand.DoFinished;
begin
  SetCommandState(dcsFinished);
end;

function TGDBMIDebuggerCommand.Execute: Boolean;
var
  I: Integer;
  Frames: PPointer;
  Report, Report2: string;
begin
  // Set the state first, so DoExecute can set an error-state
  SetCommandState(dcsExecuting);
  AddReference;
  DoLockQueueExecute;
  try
    Result := DoExecute;
    DoOnExecuted;
  except
    on e: Exception do begin
      try
        debugln(['ERROR: Exception occured in ',ClassName+'.DoExecute ',
                  e.ClassName, ' Msg="', e.Message, '" Addr=', dbgs(ExceptAddr),
                  ' Dbg.State=', dbgs(FTheDebugger.State)]);
        Report :=  BackTraceStrFunc(ExceptAddr);
        Report2 := Report;
        Frames := ExceptFrames;
        for I := 0 to ExceptFrameCount - 1 do begin
          Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
          if i < 5
          then Report2 := Report;
        end;
      except
      end;
      debugln(Report);

      if MessageDlg(gdbmiTheDebuggerExperiencedAnUnknownCondition,
        Format(gdbmiPressIgnoreToContinueDebuggingThisMayNOTBeSafePres,
        [LineEnding, e.ClassName, e.Message, Report2, ClassName, dbgs(FTheDebugger.State)]),
        mtWarning, [mbIgnore, mbAbort], 0, mbAbort) = mrAbort
      then begin
        try
          FTheDebugger.CancelAllQueued;
        finally
          FTheDebugger.Stop;
        end;
      end;
    end;
  end;
  // No re-raise in the except block. So no try-finally required
  DoUnockQueueExecute;
  ReleaseReference;
end;

procedure TGDBMIDebuggerCommand.Cancel;
begin
  {$IFDEF DBGMI_QUEUE_DEBUG}
  DebugLn(['Canceling: "', DebugText,'"']);
  {$ENDIF}
  FTheDebugger.UnQueueCommand(Self);
  DoCancel;
  DoOnCanceled;
  SetCommandState(dcsCanceled);
end;

function TGDBMIDebuggerCommand.DebugText: String;
begin
  Result := ClassName;
end;

{ TGDBMIDebuggerCommandList }

function TGDBMIDebuggerCommandList.Get(Index: Integer): TGDBMIDebuggerCommand;
begin
  Result := TGDBMIDebuggerCommand(inherited Items[Index]);
end;

procedure TGDBMIDebuggerCommandList.Put(Index: Integer; const AValue: TGDBMIDebuggerCommand);
begin
  inherited Items[Index] := AValue;
end;

{ TGDBMIInternalBreakPoint }

procedure TGDBMIInternalBreakPoint.ClearBreak(ACmd: TGDBMIDebuggerCommand);
begin
  if FBreakID = -1 then exit;
  ACmd.ExecuteCommand('-break-delete %d', [FBreakID], [cfCheckError]);
  FBreakID := -1;
  FBreakAddr := 0;
end;

procedure TGDBMIInternalBreakPoint.ClearInfo(ACmd: TGDBMIDebuggerCommand);
begin
  if FInfoID = -1 then exit;
  ACmd.ExecuteCommand('-break-delete %d', [FInfoID], [cfCheckError]);
  FInfoID := -1;
  FInfoAddr := 0;
end;

function TGDBMIInternalBreakPoint.BreakSet(ACmd: TGDBMIDebuggerCommand;
  ALoc: String; out AId: integer; out AnAddr: TDBGPtr): boolean;
var
  R: TGDBMIExecResult;
  ResultList: TGDBMINameValueList;
begin
  AId := -1;
  AnAddr := 0;
  ACmd.ExecuteCommand('-break-insert %s', [ALoc], R);
  Result := R.State <> dsError;
  if not Result then exit;

  ResultList := TGDBMINameValueList.Create(R, ['bkpt']);
  AId    := StrToIntDef(ResultList.Values['number'], -1);
  AnAddr := StrToQWordDef(ResultList.Values['addr'], 0);
  ResultList.Free;
end;

function TGDBMIInternalBreakPoint.GetAddr(ACmd: TGDBMIDebuggerCommand): TDBGPtr;
var
  R: TGDBMIExecResult;
  S: String;
begin
  Result := 0;
  if (not ACmd.ExecuteCommand('info address ' + FName, R)) or
     (R.State = dsError)
  then exit;
  S := GetPart(['at address ', ' at '], ['.', ' '], R.Values);
  if S <> '' then
    Result := StrToQWordDef(S, 0);
end;

constructor TGDBMIInternalBreakPoint.Create(AName: string);
begin
  FBreakID := -1;
  FBreakAddr := 0;
  FInfoID := -1;
  FInfoAddr := 0;
  FName := AName;
end;

(* Using -insert-break with a function name allows GDB to adjust the address
   to be behind the functions initialization.
   Which means values passed by register may no longer be accessible.
   Therefore we determine the address and force the breakpoint to it.
   This does not work for position independent executables (PIE), if the
   breakpoint is set before the application is run, because the real address
   is only known at run time.
   Therefore during startup a named break point is used as fallback.
*)
procedure TGDBMIInternalBreakPoint.SetBoth(ACmd: TGDBMIDebuggerCommand);
var
  A: TDBGPtr;
begin
  if ACmd.DebuggerState = dsError then Exit;

  // keep if already set
  if FBreakID < 0 then
    if not BreakSet(ACmd, FName, FBreakID, FBreakAddr) then exit;

  // Try to retrieve the address of the procedure
  A := GetAddr(ACmd);
  if A = 0 then exit;
  if (A <> FBreakAddr) and (A <> FInfoAddr) then begin
    ClearInfo(ACmd);
    BreakSet(ACmd, Format('*%u', [A]), FInfoID, FInfoAddr);
  end;
end;

procedure TGDBMIInternalBreakPoint.SetAddr(ACmd: TGDBMIDebuggerCommand);
var
  A: TDBGPtr;
begin
  if ACmd.DebuggerState = dsError then Exit;
  A := GetAddr(ACmd);

  if (A <> 0) and (A <> FInfoAddr) then begin
    ClearInfo(ACmd);
    BreakSet(ACmd, Format('*%u', [A]), FInfoID, FInfoAddr);
  end;

  if (A <> 0) and (A = FInfoAddr) then
    ClearBreak(ACmd);
end;

procedure TGDBMIInternalBreakPoint.SetAtCustomAddr(ACmd: TGDBMIDebuggerCommand; AnAddr: TDBGPtr);
begin
  if ACmd.DebuggerState = dsError then Exit;

  ClearInfo(ACmd);
  if AnAddr <> 0 then
    BreakSet(ACmd, Format('*%u', [AnAddr]), FInfoID, FInfoAddr);
end;

procedure TGDBMIInternalBreakPoint.Clear(ACmd: TGDBMIDebuggerCommand);
begin
  if ACmd.DebuggerState = dsError then Exit;
  ClearBreak(ACmd);
  ClearInfo(ACmd);
end;

function TGDBMIInternalBreakPoint.MatchAddr(AnAddr: TDBGPtr): boolean;
begin
  Result := (AnAddr <> 0) and
           ( (AnAddr = FBreakAddr) or (AnAddr = FInfoAddr) );
end;

function TGDBMIInternalBreakPoint.MatchId(AnId: Integer): boolean;
begin
  Result := (AnId >= 0) and
           ( (AnId = FBreakID) or (AnId = FInfoID) );
end;

function TGDBMIInternalBreakPoint.Enabled: boolean;
begin
  Result := (FBreakID >= 0)  or (FInfoID >= 0);
end;

{ TGDBMIDebuggerSimpleCommand }

procedure TGDBMIDebuggerSimpleCommand.DoStateChanged(OldState: TGDBMIDebuggerCommandState);
begin
  inherited DoStateChanged(OldState);
  if (State = dcsQueued) and (cfExternal in FFlags)
  then DebugLn('[WARNING] Debugger: Execution of external command "', FCommand, '" while queue exists');
end;

constructor TGDBMIDebuggerSimpleCommand.Create(AOwner: TGDBMIDebugger;
  const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags;
  const ACallback: TGDBMICallback; const ATag: PtrInt);
begin
  inherited Create(AOwner);
  FCommand := Format(ACommand, AValues);
  FFlags := AFlags;
  FCallback := ACallback;
  FTag := ATag;
  FResult.Values := '';
  FResult.State := dsNone;
  FResult.Flags := [];
end;

function TGDBMIDebuggerSimpleCommand.DebugText: String;
begin
  Result := Format('%s: %s', [ClassName, FCommand]);
end;

function TGDBMIDebuggerSimpleCommand.DoExecute: Boolean;
begin
  Result := True;
  if not ExecuteCommand(FCommand, FResult)
  then exit;

  if (FResult.State <> dsNone)
  and not (cfIgnoreState in FFlags)
  and ((FResult.State <> dsError) or not (cfIgnoreError in FFlags))
  then SetDebuggerState(FResult.State);

  if Assigned(FCallback)
  then FCallback(FResult, FTag);
end;

{ TGDBMIDebuggerCommandEvaluate }

function TGDBMIDebuggerCommandEvaluate.GetTypeInfo: TGDBType;
begin
  Result := FTypeInfo;
  // if the command wasn't executed, typeinfo may still get set, and need auto-destroy
  FTypeInfoAutoDestroy := FTypeInfo = nil;
end;

procedure TGDBMIDebuggerCommandEvaluate.DoWatchFreed(Sender: TObject);
begin
  {$IFDEF DBGMI_QUEUE_DEBUG}
  debugln(['DoWatchFreed: ', DebugText]);
  {$ENDIF}
  FWatchValue := nil;
  Cancel;
end;

function TGDBMIDebuggerCommandEvaluate.DoExecute: Boolean;
var
  TypeInfoFlags: TGDBTypeCreationFlags;

  function MakePrintable(const AString: String): String;
  var
    n: Integer;
    InString: Boolean;
  begin
    Result := '';
    InString := False;
    for n := 1 to Length(AString) do
    begin
      case AString[n] of
        ' '..#127, #128..#255: begin
          if not InString
          then begin
            InString := True;
            Result := Result + '''';
          end;
          Result := Result + AString[n];
          //if AString[n] = '''' then Result := Result + '''';
        end;
      else
        if InString
        then begin
          InString := False;
          Result := Result + '''';
        end;
        Result := Result + Format('#%d', [Ord(AString[n])]);
      end;
    end;
    if InString
    then Result := Result + '''';
  end;

  function FormatResult(const AInput: String): String;
  const
    INDENTSTRING = '  ';
  var
    Indent: String;
    i: Integer;
    InStr: Boolean;
    InBrackets: Boolean;
    Limit: Integer;
    Skip: Integer;
  begin
    Indent := '';
    Skip := 0;
    InStr := False;
    InBrackets := False;
    Limit := Length(AInput);
    Result := '';

    for i := 1 to Limit do
    begin
      if Skip>0
      then begin
        Dec(SKip);
        Continue;
      end;

      if AInput[i] in [#10, #13]
      then begin
        //Removes unneeded LineEnding.
        Continue;
      end;

      Result := Result + AInput[i];
      if InStr
      then begin
        InStr := AInput[i] <> '''';
        Continue;
      end;

      if InBrackets
      then begin
        InBrackets := AInput[i] <> ']';
        Continue;
      end;

      case AInput[i] of
        '[': begin
          InBrackets:=true;
        end;
        '''': begin
          InStr:=true;
        end;
        '{': begin
           if (i < Limit) and (AInput[i+1] <> '}')
           then begin
             Indent := Indent + INDENTSTRING;
             Result := Result + LineEnding + Indent;
           end;
        end;
        '}': begin
           if (i > 0) and (AInput[i-1] <> '{')
           then Delete(Indent, 1, Length(INDENTSTRING));
        end;
        ' ': begin
           if (i > 0) and (AInput[i-1] = ',')
           then Result := Result + LineEnding + Indent;
        end;
        '0': begin
           if (i > 4) and (i < Limit - 2)
           then begin
             //Pascalize pointers  "Var = 0x12345 => Var = $12345"
             if  (AInput[i-3] = ' ')
             and (AInput[i-2] = '=')
             and (AInput[i-1] = ' ')
             and (AInput[i+1] = 'x')
             then begin
               Skip := 1;
               Result[Length(Result)] := '$';
             end;
           end;
        end;
      end;

    end;
  end;

  function WhichIsFirst(const ASource: String; const ASearchable: array of Char): Integer;
  var
    j, k: Integer;
    InString: Boolean;
  begin
    InString := False;
    for j := 1 to Length(ASource) do
    begin
      if ASource[j] = '''' then InString := not InString;
      if InString then Continue;

      for k := Low(ASearchable) to High(ASearchable) do
      begin
        if ASource[j] = ASearchable[k] then Exit(j);
      end;
    end;
    Result := -1;
  end;

  function SkipPairs(var ASource: String; const ABeginChar: Char; const AEndChar: Char): String;
  var
    Deep,j: SizeInt;
    InString: Boolean;
  begin
    DebugLn('->->', ASource);
    Deep := 0;
    InString := False;

    for j := 1 to Length(ASource) do
    begin
      if ASource[j]='''' then InString := not InString;
      if InString then Continue;

      if ASource[j] = ABeginChar
      then begin
        Inc(Deep)
      end
      else begin
        if ASource[j] = AEndChar
        then Dec(Deep);
      end;

      if Deep=0
      then begin
        Result := Copy(ASource, 1, j);
        ASource := Copy(ASource, j + 1, Length(ASource) - j);
        Exit;
      end;
    end;
  end;

  function IsHexC(const ASource: String): Boolean;
  begin
    if Length(ASource) <= 2 then Exit(False);
    if ASource[1] <> '0' then Exit(False);
    Result := ASource[2] = 'x';
  end;

  function HexCToHexPascal(const ASource: String; MinChars: Byte = 0): String;
  var
    Zeros: String;
  begin
    if IsHexC(Asource)
    then begin
      Result := Copy(ASource, 3, Length(ASource) - 2);
      if Length(Result) < MinChars then
      begin
        SetLength(Zeros, MinChars - Length(Result));
        FillChar(Zeros[1], Length(Zeros), '0');
        Result := Zeros + Result;
      end;
      Result := '$' + Result;
    end
    else Result := ASource;
  end;

  procedure PutValuesInTypeRecord(const AType: TDBGType; const ATextInfo: String);
  var
    GDBParser: TGDBStringIterator;
    Payload: String;
    Composite: Boolean;
    StopChar: Char;
    j: Integer;
  begin
    GDBParser := TGDBStringIterator.Create(ATextInfo);
    GDBParser.ParseNext(Composite, Payload, StopChar);
    GDBParser.Free;

    if not Composite
    then begin
      //It is not a record
      debugln('Expected record, but found: "', ATextInfo, '"');
      exit;
    end;

    //Parse information between brackets...
    GDBParser := TGDBStringIterator.Create(Payload);
    for j := 0 to AType.Fields.Count-1 do
    begin
      if not GDBParser.ParseNext(Composite, Payload, StopChar)
      then begin
        debugln('Premature end of parsing');
        Break;
      end;

      if Payload <> AType.Fields[j].Name
      then begin
        debugln('Field name does not match, expected "', AType.Fields[j].Name, '" but found "', Payload,'"');
        Break;
      end;

      if StopChar <> '='
      then begin
        debugln('Expected assignement, but other found.');
        Break;
      end;

      //Field name verified...
      if not GDBParser.ParseNext(Composite, Payload, StopChar)
      then begin
        debugln('Premature end of parsing');
        Break;
      end;

      if Composite
      then THackDBGType(AType.Fields[j].DBGType).FKind := skRecord;

      AType.Fields[j].DBGType.Value.AsString := HexCToHexPascal(Payload);
    end;

    GDBParser.Free;
  end;

  procedure PutValuesInClass(const AType: TGDBType; ATextInfo: String);
  var
    //GDBParser: TGDBStringIterator;
    //Payload: String;
    //Composite: Boolean;
    //StopChar: Char;
    //j: Integer;
    AWarnText: string;
    StartPtr, EndPtr: PChar;

    Procedure SkipSpaces;
    begin
      while (StartPtr <= EndPtr) and (StartPtr^ = ' ') do inc(StartPtr);
    end;

    Procedure SkipToEndOfField(EndAtComma: Boolean = False);
    var
      i: Integer;
    begin
      // skip forward, past the next ",", but do NOT skip the closing "}"
      i := 1;
      while (StartPtr <= EndPtr) and (i > 0) do begin
        case StartPtr^ of
          '{': inc(i);
          '}': if i = 1
               then break  // do not skip }
               else dec(i);
          '''': begin
              inc(StartPtr);
              while (StartPtr <= EndPtr) and (StartPtr^ <> '''') do inc(StartPtr);
            end;
          ',': if (i = 1) then begin
              if EndAtComma then break;
              i := 0;
            end;
        end;
        inc(StartPtr);
      end;
      SkipSpaces;
    end;

    procedure ProcessAncestor(ATypeName: String);
    var
      HelpPtr, HelpPtr2: PChar;
      NewName, NewVal: String;
      i: Integer;
    begin
      inc(StartPtr); // skip '{'
      SkipSpaces;
      if StartPtr^ = '<' Then begin
        inc(StartPtr);
        HelpPtr := StartPtr;
        while (HelpPtr <= EndPtr) and (HelpPtr^ <> '>') do inc(HelpPtr);
        NewName := copy(StartPtr, 1, HelpPtr - StartPtr);
        StartPtr := HelpPtr + 1;
        SkipSpaces;
        if StartPtr^ <> '=' then begin
          debugln('WARNING: PutValuesInClass: Expected "=" for ancestor "' + NewName + '" in: ' + AWarnText);
          AWarnText := '';
          SkipToEndOfField;
          // continue fields, or end
        end
        else begin
          inc(StartPtr);
          SkipSpaces;
          if StartPtr^ <> '{'
          then begin
            //It is not a class
            debugln('WARNING: PutValuesInClass: Expected "{" for ancestor "' + NewName + '" in: ' + AWarnText);
            AWarnText := '';
            SkipToEndOfField;
          end
          else
            ProcessAncestor(NewName);
            if StartPtr^ = ',' then inc(StartPtr);
            SkipSpaces;
        end;
      end;

      // process fields in this ancestor
      while (StartPtr <= EndPtr) and (StartPtr^ <> '}') do begin
        HelpPtr := StartPtr;
        while (HelpPtr < EndPtr) and not (HelpPtr^ in [' ', '=', ',']) do inc(HelpPtr);
        NewName := uppercase(copy(StartPtr, 1, HelpPtr - StartPtr));  // name of field

        StartPtr := HelpPtr;
        SkipSpaces;
        if StartPtr^ <> '=' then begin
          debugln('WARNING: PutValuesInClass: Expected "=" for field"' + NewName + '" in: ' + AWarnText);
          AWarnText := '';
          SkipToEndOfField;
          continue;
        end;

        inc(StartPtr);
        SkipSpaces;
        HelpPtr := StartPtr;
        SkipToEndOfField(True);
        HelpPtr2 := StartPtr; // "," or "}"
        dec(HelpPtr2);
        while HelpPtr2^ = ' ' do dec(HelpPtr2);
        NewVal := copy(HelpPtr, 1, HelpPtr2 + 1 - HelpPtr);  // name of field

        i := AType.Fields.Count - 1;
        while (i >= 0)
        and ( (uppercase(AType.Fields[i].Name) <> NewName)
           or (uppercase(AType.Fields[i].ClassName) <> ATypeName) )
        do dec(i);

        if i < 0 then begin
          if (uppercase(ATypeName) <> 'TOBJECT') or (pos('vptr', NewName) < 1)
          then debugln('WARNING: PutValuesInClass: No field for "' + ATypeName + '"."' + NewName + '"');
        end
        else
          AType.Fields[i].DBGType.Value.AsString := HexCToHexPascal(NewVal);

        if (StartPtr^ <> '}') then inc(StartPtr);
        SkipSpaces;
      end;

      inc(StartPtr); // skip the }
    end;

  begin
    if ATextInfo = '' then exit;
    AWarnText := ATextInfo;
    StartPtr := @ATextInfo[1];
    EndPtr := @ATextInfo[length(ATextInfo)];

    while EndPtr^ = ' ' do dec(EndPtr);

    SkipSpaces;
    if StartPtr^ <> '{'
    then begin
      //It is not a class
      debugln('ERROR: PutValuesInClass: Expected class, but found: "', ATextInfo, '"');
      exit;
    end;

    ProcessAncestor(AType.TypeName);

////
(*


    GDBParser := TGDBStringIterator.Create(ATextInfo);
    GDBParser.ParseNext(Composite, Payload, StopChar);
    GDBParser.Free;

    if not Composite
    then begin
      //It is not a record
      debugln('Expected class, but found: "', ATextInfo, '"');
      exit;
    end;

    //Parse information between brackets...
    GDBParser := TGDBStringIterator.Create(Payload);
    try
      if not GDBParser.ParseNext(Composite, Payload, StopChar)
      then begin
        debugln('Premature end of parsing.');
        exit;
      end;

      //APayload holds the ancestor name
      if '<' + AType.Ancestor + '>' <> Payload
      then begin
        debugln('Ancestor does not match, expected ', AType.Ancestor,' but found ', Payload);
        exit;
      end;

      //Special hidden field, skip as a decomposable, parse and forget...
      if not GDBParser.ParseNext(Composite, Payload, StopChar)
      then begin
        debugln('Premature end of parsing.');
        exit;
      end;

      while GDBParser.ParseNext(Composite, Payload, StopChar) do
      begin
        if StopChar <> '='
        then begin
          debugln('Expected assignement, but other found.');
          exit;
        end;

        for j := 0 to AType.Fields.Count-1 do
        begin
          if Payload <> AType.Fields[j].Name then Continue;

          //Field name verified...
          if not GDBParser.ParseNext(Composite, Payload, StopChar)
          then begin
            debugln('Premature end of parsing.');
            exit;
          end;

          if Composite
          then THackDBGType(AType.Fields[j].DBGType).FKind := skRecord;
          AType.Fields[j].DBGType.Value.AsString := HexCToHexPascal(Payload);
          Break;
        end;
      end;
    finally
      GDBParser.Free;
    end;
*)
  end;

  procedure PutValuesInTree();
  var
    ValData: string;
  begin
    if not Assigned(FTypeInfo) then exit;

    ValData := FTextValue;
    case FTypeInfo.Kind of
      skClass: begin
        GetPart('','{',ValData);
        PutValuesInClass(FTypeInfo,ValData);
      end;
      skRecord: begin
        GetPart('','{',ValData);
        PutValuesInTypeRecord(FTypeInfo,ValData);
      end;
      skVariant: begin
        FTypeInfo.Value.AsString:=ValData;
      end;
      skEnum: begin
        FTypeInfo.Value.AsString:=ValData;
      end;
      skSet: begin
        FTypeInfo.Value.AsString:=ValData;
      end;
      skSimple: begin
        FTypeInfo.Value.AsString:=ValData;
      end;
//      skPointer: ;
    end;
  end;

  function SelectParentFrame(var aFrameIdx: Integer): Boolean;
  var
    R: TGDBMIExecResult;
    List: TGDBMINameValueList;
    ParentFp, Fp: String;
    i, j, ThreadId: Integer;
    FrameCache: PGDBMIDebuggerParentFrameCache;
  begin
    if FWatchValue <> nil
    then ThreadId := FWatchValue.ThreadId
    else ThreadId := FTheDebugger.FCurrentThreadId;
    FrameCache := TGDBMIWatches(FTheDebugger.Watches).GetParentFPList(ThreadId);
    List := nil;

    i := length(FrameCache^.ParentFPList);
    j := Max(i, aFrameIdx+1);
    if j >= i
    then SetLength(FrameCache^.ParentFPList, j + 3);

    // Did a previous check for parentfp fail?
    ParentFP := FrameCache^.ParentFPList[aFrameIdx].parentfp;
    if ParentFp = '-'
    then Exit(False);

    if ParentFp = '' then begin
      // not yet evaluated
      if ExecuteCommand('-data-evaluate-expression parentfp', R)
      and (R.State <> dsError)
      then begin
        List := TGDBMINameValueList.Create(R);
        ParentFP := List.Values['value'];
      end;
      if ParentFp = '' then begin
        FrameCache^.ParentFPList[aFrameIdx].parentfp := '-'; // mark as no parentfp
        List.Free;
        Exit(False);
      end;
      FrameCache^.ParentFPList[aFrameIdx].parentfp := ParentFp;
    end;

    if List = nil
    then List := TGDBMINameValueList.Create('');

    repeat
      Inc(aFrameIdx);
      i := length(FrameCache^.ParentFPList);
      j := Max(i, aFrameIdx+1);
      if j >= i
      then SetLength(FrameCache^.ParentFPList, j + 5);

      Fp := FrameCache^.ParentFPList[aFrameIdx].Fp;
      if Fp = '-'
      then begin
        List.Free;
        Exit(False);
      end;

      if (Fp = '') or (Fp = ParentFP) then begin
        if not ExecuteCommand('-stack-select-frame %u', [aFrameIdx], R)
        or (R.State = dsError)
         then begin
          FrameCache^.ParentFPList[aFrameIdx].Fp := '-'; // mark as no Fp (not accesible)
          List.Free;
          Exit(False);
        end;
        FStackFrameChanged := True; // Force UnSelectContext() to restore current frame
        FTheDebugger.FInternalStackFrame := aFrameIdx;

        if (Fp = '') then begin
          if not ExecuteCommand('-data-evaluate-expression $fp', R)
          or (R.State = dsError)
          then begin
            FrameCache^.ParentFPList[aFrameIdx].Fp := '-'; // mark as no Fp (not accesible)
            List.Free;
            Exit(False);
          end;
          List.Init(R.Values);
          Fp := List.Values['value'];
          if Fp = ''
          then Fp := '-';
          FrameCache^.ParentFPList[aFrameIdx].Fp := Fp;
        end;
      end;
    until ParentFP = Fp;
    List.Free;

    Result := True;
  end;

  function PascalizePointer(AString: String; const TypeCast: String = ''): String;
  var
    s: String;
  begin
    Result := AString;
    if not IsHexC(AString)
    then exit;

    // there may be data after the pointer
    s := GetPart([], [' '], AString, False, True);
    if s = '0x0'
    then begin
      Result := 'nil';
    end
    else begin
      // 0xabc0 => $0000ABC0
      Result := UpperCase(HexCToHexPascal(s, FTheDebugger.TargetWidth div 4));
    end;

    if TypeCast <> '' then
      Result := TypeCast + '(' + Result + ')';
    if AString <> '' then
      Result := Result + ' ' + AString;
  end;

  function FormatCurrency(const AString: String): String;
  var
    i, e: Integer;
    c: Currency;
  begin
    Result := AString;
    Val(Result, i, e);
    // debugger outputs 12345 for 1,2345 values
    if e=0 then
    begin
      c := i / 10000;
      Result := CurrToStr(c);
    end;
  end;

  function GetVariantValue(AString: String): String;

    function FormatVarError(const AString: String): String; inline;
    begin
      Result := 'Error('+AString+')';
    end;

  var
    VarList: TGDBMINameValueList;
    VType: Integer;
    Addr: TDbgPtr;
    dt: TDateTime;
    e: Integer;
  begin
    VarList := TGDBMINameValueList.Create('');
    try
      VarList.UseTrim := True;
      VarList.Init(AString);
      VType := StrToIntDef(VarList.Values['VTYPE'], -1);
      if VType = -1 then // can never happen if no error since varType is word
        Exit('variant: unknown type');
      case VType and not varTypeMask of
        0:
          begin
            case VType of
              varEmpty: Result := 'UnAssigned';
              varNull: Result := 'Null';
              varsmallint: Result := VarList.Values['VSMALLINT'];
              varinteger: Result := VarList.Values['VINTEGER'];
              varsingle: Result := VarList.Values['VSINGLE'];
              vardouble: Result := VarList.Values['VDOUBLE'];
              vardate:
                begin
                  // float number
                  Result := VarList.Values['VDATE'];
                  val(Result, dt, e);
                  if e = 0 then
                    Result := DateTimeToStr(dt);
                end;
              varcurrency: Result := FormatCurrency(VarList.Values['VCURRENCY']);
              varolestr: Result := VarList.Values['VOLESTR'];
              vardispatch: Result := PascalizePointer(VarList.Values['VDISPATCH'], 'IDispatch');
              varerror: Result := FormatVarError(VarList.Values['VERROR']);
              varboolean: Result := VarList.Values['VBOOLEAN'];
              varunknown: Result := PascalizePointer(VarList.Values['VUNKNOWN'], 'IUnknown');
              varshortint: Result := VarList.Values['VSHORTINT'];
              varbyte: Result := VarList.Values['VBYTE'];
              varword: Result := VarList.Values['VWORD'];
              varlongword: Result := VarList.Values['VLONGWORD'];
              varint64: Result := VarList.Values['VINT64'];
              varqword: Result := VarList.Values['VQWORD'];
              varstring:
                begin
                  // address of string
                  Result := VarList.Values['VSTRING'];
                  Val(Result, Addr, e);
                  if e = 0 then
                  begin
                    if Addr = 0 then
                      Result := ''''''
                    else
                      Result := MakePrintable(GetText(Addr));
                  end;
                end;
              varany:  Result := VarList.Values['VANY'];
            else
              Result := 'unsupported variant type: ' + VarTypeAsText(VType);
            end;
          end;
        varArray:
          begin
            Result := VarTypeAsText(VType);
            // TODO: show variant array data?
            // Result := VarList.Values['VARRAY'];
          end;
        varByRef:
          begin
            Result := VarList.Values['VPOINTER'];
            Val(Result, Addr, e);
            if e = 0 then
            begin
              if Addr = 0 then
                Result := '???'
              else
              begin
                // Result contains a valid address
                case VType xor varByRef of
                  varEmpty: Result := 'UnAssigned';
                  varNull: Result := 'Null';
                  varsmallint: Result := GetStrValue('psmallint(%s)^', [Result]);
                  varinteger: Result := GetStrValue('pinteger(%s)^', [Result]);
                  varsingle: Result := GetStrValue('psingle(%s)^', [Result]);
                  vardouble: Result := GetStrValue('pdouble(%s)^', [Result]);
                  vardate:
                    begin
                      // float number
                      Result := GetStrValue('pdatetime(%s)^', [Result]);
                      val(Result, dt, e);
                      if e = 0 then
                        Result := DateTimeToStr(dt);
                    end;
                  varcurrency: Result := FormatCurrency(GetStrValue('pcurrency(%s)^', [Result]));
                  varolestr:
                    begin
                      Result := GetStrValue('^pointer(%s)^', [Result]);
                      val(Result, Addr, e);
                      if e = 0 then
                        Result := MakePrintable(GetWideText(Addr));
                    end;
                  vardispatch: Result := PascalizePointer(GetStrValue('ppointer(%s)^', [Result]), 'IDispatch');
                  varerror: Result := FormatVarError(GetStrValue('phresult(%s)^', [Result]));
                  varboolean: Result := GetStrValue('pwordbool(%s)^', [Result]);
                  varunknown: Result := PascalizePointer(GetStrValue('ppointer(%s)^', [Result]), 'IUnknown');
                  varshortint: Result := GetStrValue('pshortint(%s)^', [Result]);
                  varbyte: Result := GetStrValue('pbyte(%s)^', [Result]);
                  varword: Result := GetStrValue('pword(%s)^', [Result]);
                  varlongword: Result := GetStrValue('plongword(%s)^', [Result]);
                  varint64: Result := GetStrValue('pint64(%s)^', [Result]);
                  varqword: Result := GetStrValue('pqword(%s)^', [Result]);
                  varstring: Result := MakePrintable(GetText('pansistring(%s)^', [Result]));
                else
                  Result := 'unsupported variant type: ' + VarTypeAsText(VType);
                end;
              end;
            end;
          end;
        else
          Result := 'unsupported variant type: ' + VarTypeAsText(VType);
      end;
    finally
      VarList.Free;
    end;
  end;

  function StripExprNewlines(const ASource: String): String;
  var
    len: Integer;
    srcPtr, dstPtr: PChar;
  begin
    len := Length(ASource);
    SetLength(Result, len);
    if len = 0 then Exit;
    srcPtr := @ASource[1];
    dstPtr := @Result[1];
    while len > 0 do
    begin
      case srcPtr^ of
        #0:;
        #10, #13: dstPtr^ := ' ';
      else
        dstPtr^ := srcPtr^;
      end;
      Dec(len);
      Inc(srcPtr);
      Inc(dstPtr);
    end;
  end;

  procedure FixUpResult(AnExpression: string; ResultInfo: TGDBType = nil);
  var
    addr: TDbgPtr;
    e: Integer;
    PrintableString: String;
    i: Integer;
    addrtxt: string;
  begin
    // Check for strings
    if ResultInfo = nil then
      ResultInfo := GetGDBTypeInfo(AnExpression, defFullTypeInfo in FEvalFlags, TypeInfoFlags);
    if (ResultInfo = nil) then Exit;
    FTypeInfo := ResultInfo;

    case ResultInfo.Kind of
      skPointer: begin
        addrtxt := GetPart([], [' '], FTextValue, False, False);
        Val(addrtxt, addr, e);
        if e <> 0 then
          Exit;

        AnExpression := Lowercase(ResultInfo.TypeName);
        case StringCase(AnExpression, ['char', 'character', 'ansistring', '__vtbl_ptr_type',
                                       'wchar', 'widechar', 'widestring', 'unicodestring',
                                       'pointer'])
        of
          0, 1, 2: begin // 'char', 'character', 'ansistring'
            // check for addr 'text' / 0x1234 'abc'
            i := length(addrtxt);
            if (i+3 <= length(FTextValue)) and (FTextValue[i+2] ='''')
            and (FTextValue[length(FTextValue)] ='''')
            then
              FTextValue := copy(FTextValue, i+2, length(FTextValue) - i - 1)
            else
            if Addr = 0
            then
              FTextValue := ''''''
            else
              FTextValue := MakePrintable(GetText(Addr));
              PrintableString := FTextValue;
          end;
          3: begin // '__vtbl_ptr_type'
            if Addr = 0
            then FTextValue := 'nil'
            else begin
              AnExpression := GetClassName(Addr);
              if AnExpression = '' then AnExpression := '???';
              FTextValue := 'class of ' + AnExpression + ' ' + FTextValue;
            end;
          end;
          4,5,6,7: begin // 'wchar', 'widechar'
            // widestring handling
            if Addr = 0
            then FTextValue := ''''''
            else FTextValue := MakePrintable(GetWideText(Addr));
            PrintableString := FTextValue;
          end;
          8: begin // pointer
            if Addr = 0
            then FTextValue := 'nil';
            FTextValue := PascalizePointer(FTextValue);
          end;
        else
          if Addr = 0
          then FTextValue := 'nil';
          if (Length(AnExpression) > 0)
          then begin
            if AnExpression[1] = 't'
            then begin
              AnExpression[1] := 'T';
              if Length(AnExpression) > 1 then AnExpression[2] := UpperCase(AnExpression[2])[1];
            end;
            FTextValue := PascalizePointer(FTextValue, AnExpression);
          end;

        end;

        ResultInfo.Value.AsPointer := Pointer(PtrUint(Addr));
        AnExpression := Format('$%x', [Addr]);
        if PrintableString <> ''
        then AnExpression := AnExpression + ' ' + PrintableString;
        ResultInfo.Value.AsString := AnExpression;
      end;

      skClass: begin
        Val(FTextValue, addr, e); //Get the class mem address
        if (e = 0) and (addr = 0)
        then FTextValue := 'nil';

        if (FTextValue <> '') and (FTypeInfo <> nil)
        then begin
          FTextValue := '<' + FTypeInfo.TypeName + '> = ' + FTextValue;
        end
        else
        if (e = 0) and (addr <> 0)
        then begin //No error ?
          AnExpression := GetInstanceClassName(Addr);
          if AnExpression = '' then AnExpression := '???'; //No instanced class found
          FTextValue := 'instance of ' + AnExpression + ' ' + FTextValue;
        end;
      end;

      skVariant: begin
        FTextValue := GetVariantValue(FTextValue);
      end;
      skRecord: begin
        FTextValue := 'record ' + ResultInfo.TypeName + ' '+ FTextValue;
      end;

      skSimple: begin
        if ResultInfo.TypeName = 'CURRENCY' then
          FTextValue := FormatCurrency(FTextValue)
        else
        if (ResultInfo.TypeName = '&ShortString') then
          FTextValue := GetStrValue('ShortString(%s)', [AnExpression]) // we have an address here, so we need to typecast
        else
        if saDynArray in ResultInfo.Attributes then  // may also be a string
          FTextValue := PascalizePointer(FTextValue)
        else
          FTextValue := FTextValue;
      end;
    end;

    PutValuesInTree;
    FTextValue := FormatResult(FTextValue);
  end;

  function AddAddressOfToExpression(const AnExpression: string; TypeInfo: TGDBType): String;
  var
    UseAt: Boolean;
  begin
    UseAt := True;
    case TypeInfo.Kind of // (skClass, skRecord, skEnum, skSet, skProcedure, skFunction, skSimple, skPointer, skVariant)
      skPointer: begin
          case StringCase(Lowercase(TypeInfo.TypeName),
                          ['char', 'character', 'ansistring', '__vtbl_ptr_type', 'wchar', 'widechar', 'pointer']
                         )
          of
            2: UseAt := False;
            3: UseAt := False;
          end;
        end;
    end;

    if UseAt
    then Result := '@(' + AnExpression + ')'
    else Result := AnExpression;
  end;

  function QuoteExpr(const AnExpression: string): string;
    var
      i, j, Cnt: integer;
    begin
    if pos(' ', AnExpression) < 1
    then exit(AnExpression);
    Cnt := length(AnExpression);
    SetLength(Result, 2 * Cnt + 2);
    Result[1] := '"';
    i := 1;
    j := 2;
    while i <= Cnt do begin
      if AnExpression[i] in ['"', '\']
      then begin
        Result[j] := '\';
        inc(j);
      end;
      Result[j] := AnExpression[i];
      inc(i);
      inc(j);
    end;
    Result[j] := '"';
    SetLength(Result, j + 1);
  end;

  function TryExecute(AnExpression: string): Boolean;

    procedure ParseLastError;
    var
      ResultList: TGDBMINameValueList;
    begin
      if (dcsCanceled in SeenStates)
      then begin
        FTextValue := '<Canceled>';
        FValidity := ddsInvalid;
        exit;
      end;
      ResultList := TGDBMINameValueList.Create(LastExecResult.Values);
      FTextValue := ResultList.Values['msg'];
      if FTextValue = ''
      then  FTextValue := '<Error>';
      FreeAndNil(ResultList);
      FValidity := ddsError;
    end;

    function PrepareExpr(var expr: string; NoAddressOp: Boolean = False): boolean;
    begin
      Assert(FTypeInfo = nil, 'Type info must be nil');
      FTypeInfo := GetGDBTypeInfo(expr, defFullTypeInfo in FEvalFlags, TypeInfoFlags);
      Result := FTypeInfo <> nil;
      if (not Result) then begin
        ParseLastError;
        exit;
      end;

      if NoAddressOp
      then expr := QuoteExpr(expr)
      else expr := QuoteExpr(AddAddressOfToExpression(expr, FTypeInfo));
    end;

  var
    ResultList: TGDBMINameValueList;
    R: TGDBMIExecResult;
    MemDump: TGDBMIMemoryDumpResultList;
    Size: integer;
    s: String;
  begin
    Result := False;

    case FDisplayFormat of
      wdfStructure:
        begin
          Result := ExecuteCommand('-data-evaluate-expression %s', [AnExpression], R);
          Result := Result and (R.State <> dsError);
          if (not Result) then begin
            ParseLastError;
            exit;
          end;

          ResultList := TGDBMINameValueList.Create(R.Values);
          if Result
          then FTextValue := ResultList.Values['value']
          else FTextValue := ResultList.Values['msg'];
          FTextValue := DeleteEscapeChars(FTextValue);
          ResultList.Free;

          if Result
          then begin
            FixUpResult(AnExpression);
            FValidity := ddsValid;
          end;
        end;
      wdfChar:
        begin
          Result := PrepareExpr(AnExpression);
          if not Result
          then exit;
          FValidity := ddsValid;
          FTextValue := GetChar(AnExpression, []);
          if LastExecResult.State = dsError
          then ParseLastError;
        end;
      wdfString:
        begin
          Result := PrepareExpr(AnExpression);
          if not Result
          then exit;
          FValidity := ddsValid;
          FTextValue := GetText(AnExpression, []); // GetText takes Addr
          if LastExecResult.State = dsError
          then ParseLastError;
        end;
      wdfDecimal:
        begin
          Result := PrepareExpr(AnExpression, True);
          if not Result
          then exit;
          FValidity := ddsValid;
          FTextValue := IntToStr(Int64(GetPtrValue(AnExpression, [], True)));
          if LastExecResult.State = dsError
          then ParseLastError;
        end;
      wdfUnsigned:
        begin
          Result := PrepareExpr(AnExpression, True);
          if not Result
          then exit;
          FValidity := ddsValid;
          FTextValue := IntToStr(GetPtrValue(AnExpression, [], True));
          if LastExecResult.State = dsError
          then ParseLastError;
        end;
      //wdfFloat:
      //  begin
      //    Result := PrepareExpr(AnExpression);
      //    if not Result
      //    then exit;
      //    FTextValue := GetFloat(AnExpression, []);  // GetFloat takes address
      //    if LastExecResult.State = dsError
      //    then FTextValue := '<error>';
      //  end;
      wdfHex:
        begin
          Result := PrepareExpr(AnExpression, True);
          if not Result
          then exit;
          FTextValue := IntToHex(GetPtrValue(AnExpression, [], True), 2);
          FValidity := ddsValid;
          if length(FTextValue) mod 2 = 1
          then FTextValue := '0'+FTextValue; // make it an even number of digets
          if LastExecResult.State = dsError
          then ParseLastError;
        end;
      wdfPointer:
        begin
          Result := PrepareExpr(AnExpression, True);
          if not Result
          then exit;
          FTextValue := PascalizePointer('0x' + IntToHex(GetPtrValue(AnExpression, [], True), TargetInfo^.TargetPtrSize*2));
          FValidity := ddsValid;
          if LastExecResult.State = dsError
          then FTextValue := '<error>';
        end;
      wdfMemDump:
        begin
          Result := PrepareExpr(AnExpression);
          if not Result
          then exit;

          Result := False;
          Size := 256;
          if (FTypeInfo <> nil) and (saInternalPointer in FTypeInfo.Attributes) then begin
            Result := ExecuteCommand('-data-read-memory %s^ x 1 1 %u', [AnExpression, Size], R);
            Result := Result and (R.State <> dsError);
            // nil ?
            if (R.State = dsError) and (pos('Unable to read memory', R.Values) > 0) then
              Size := TargetInfo^.TargetPtrSize;
          end;
          if (not Result) then begin
            Result := ExecuteCommand('-data-read-memory %s x 1 1 %u', [AnExpression, Size], R);
            Result := Result and (R.State <> dsError);
          end;
          if (not Result) then begin
            ParseLastError;
            exit;
          end;
          MemDump := TGDBMIMemoryDumpResultList.Create(R);
          FValidity := ddsValid;
          FTextValue := MemDump.AsText(0, MemDump.Count, TargetInfo^.TargetPtrSize*2);
          MemDump.Free;
        end;
      else // wdfDefault
        begin
          Result := False;
          Assert(FTypeInfo = nil, 'Type info must be nil');
          FTypeInfo := GetGDBTypeInfo(AnExpression, defFullTypeInfo in FEvalFlags,
            TypeInfoFlags + [gtcfExprEvaluate, gtcfExprEvalStrFixed], FDisplayFormat);

          if (FTypeInfo = nil) or (dcsCanceled in SeenStates)
          then begin
            ParseLastError;
            exit;
          end;
          if FTypeInfo.HasExprEvaluatedAsText then begin
            FTextValue := FTypeInfo.ExprEvaluatedAsText;
            FTextValue := DeleteEscapeChars(FTextValue);
            FValidity := ddsValid;
            Result := True;
            FixUpResult(AnExpression, FTypeInfo);

            if FTypeInfo.HasStringExprEvaluatedAsText then begin
              s := FTextValue;
              FTextValue := FTypeInfo.StringExprEvaluatedAsText;
              FTextValue := DeleteEscapeChars(FTextValue);
              FixUpResult(AnExpression, FTypeInfo);
              FTextValue := 'PCHAR: ' + s + LineEnding + 'STRING: ' + FTextValue;
            end;

            exit;
          end;

          debugln('############# Not expected to be here');
          FTextValue := '<ERROR>';
        end;
    end;

  end;

var
  S: String;
  ResultList: TGDBMINameValueList;
  Expr: TGDBMIExpression;
  frameidx: Integer;
begin
  if not SelectContext then begin
    FTextValue:='<Error>';
    FValidity := ddsError;
    exit;
  end;
  try
    FTextValue:='';
    FTypeInfo:=nil;
    TypeInfoFlags := [];
    if defClassAutoCast in FEvalFlags
    then include(TypeInfoFlags, gtcfAutoCastClass);


    S := StripExprNewlines(FExpression);

    if S = '' then Exit(false);
    if S[1] = '!'
    then begin
      //TESTING...
      Delete(S, 1, 1);
      Expr := TGDBMIExpression.Create(S);
      FTextValue := Expr.DumpExpression;
      FTextValue := FTextValue + LineEnding;
      Expr.Evaluate(Self, S, FTypeInfo);
      FreeAndNil(FTypeInfo);
      FTextValue := FTextValue + S;
      Expr.Free;
      Exit(True);
    end;

    ResultList := TGDBMINameValueList.Create('');
    // keep the internal stackframe => same as requested by watch
    frameidx := TGDBMIDebugger(FTheDebugger).FInternalStackFrame;
    DefaultTimeOut := DebuggerProperties.TimeoutForEval;
    try
      repeat
        if TryExecute(S)
        then Break;
        FreeAndNil(FTypeInfo);
        if (dcsCanceled in SeenStates)
        then break;
      until not SelectParentFrame(frameidx); // may set FStackFrameChanged to force UnSelectContext()

    finally
      DefaultTimeOut := -1;
      FreeAndNil(ResultList);
    end;
    Result := True;
  finally
    UnSelectContext;
    if FWatchValue <> nil then begin
      FWatchValue.SetValue(FTextValue);
      FWatchValue.SetTypeInfo(TypeInfo);
      FWatchValue.Validity := FValidity;
    end;
  end;
end;

function TGDBMIDebuggerCommandEvaluate.SelectContext: Boolean;
var
  R: TGDBMIExecResult;
  t, f: Integer;
begin
  Result := True;
  FThreadChanged := False;
  FStackFrameChanged := False;
  if FWatchValue = nil then exit;
  t := FWatchValue.ThreadId;
  f := FWatchValue.StackFrame;

  if t <> FTheDebugger.FCurrentThreadId then begin
    FThreadChanged := True;
    Result := ExecuteCommand('-thread-select %d', [t], R);
    FTheDebugger.FInternalThreadId := t;
    Result := Result and (R.State <> dsError);
  end;
  if not Result then exit;

  if (f <> FTheDebugger.FCurrentStackFrame) or FThreadChanged then begin
    FStackFrameChanged := True;
    Result := ExecuteCommand('-stack-select-frame %d', [f], R);
    FTheDebugger.FInternalStackFrame := f;
    Result := Result and (R.State <> dsError);
  end;
end;

procedure TGDBMIDebuggerCommandEvaluate.UnSelectContext;
var
  R: TGDBMIExecResult;
begin
  if FThreadChanged
  then ExecuteCommand('-thread-select %d', [FTheDebugger.FCurrentThreadId], R);
  FTheDebugger.FInternalThreadId := FTheDebugger.FCurrentThreadId;
  if FStackFrameChanged
  then ExecuteCommand('-stack-select-frame %d', [FTheDebugger.FCurrentStackFrame], R);
  FTheDebugger.FInternalStackFrame := FTheDebugger.FCurrentStackFrame;
end;

constructor TGDBMIDebuggerCommandEvaluate.Create(AOwner: TGDBMIDebugger; AExpression: String;
  ADisplayFormat: TWatchDisplayFormat);
begin
  inherited Create(AOwner);
  FWatchValue := nil;
  FExpression := AExpression;
  FDisplayFormat := ADisplayFormat;
  FTextValue := '';
  FTypeInfo:=nil;
  FEvalFlags := [];
  FTypeInfoAutoDestroy := True;
  FValidity := ddsValid;
end;

constructor TGDBMIDebuggerCommandEvaluate.Create(AOwner: TGDBMIDebugger;
  AWatchValue: TCurrentWatchValue);
begin
  Create(AOwner, AWatchValue.Watch.Expression, AWatchValue.DisplayFormat);
  FWatchValue := AWatchValue;
  FWatchValue.AddFreeeNotification(@DoWatchFreed);
end;

destructor TGDBMIDebuggerCommandEvaluate.Destroy;
begin
  if FWatchValue <> nil
  then FWatchValue.RemoveFreeeNotification(@DoWatchFreed);
  if FTypeInfoAutoDestroy
  then FreeAndNil(FTypeInfo);
  inherited Destroy;
end;

function TGDBMIDebuggerCommandEvaluate.DebugText: String;
begin
  if FWatchValue <> nil
  then Result := Format('%s: %s Thread=%d, Frame=%d', [ClassName, FExpression, FWatchValue.ThreadId, FWatchValue.StackFrame])
  else Result := Format('%s: %s', [ClassName, FExpression]);
end;

initialization
  RegisterDebugger(TGDBMIDebugger);

end.

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

interface

uses
  Classes, SysUtils, Controls, Math, Variants, LCLProc, Dialogs, LazConf, DebugUtils,
  Debugger, FileUtil, CmdLineDebugger, GDBTypeInfo, Maps, LCLIntf,
{$IFdef MSWindows}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
   Unix,BaseUnix,
{$ENDIF}
  BaseDebugManager;

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
    cfCheckState,
    cfCheckError
  );
  TGDBMICommandFlags = set of TGDBMICommandFlag;

  TGDBMIResultFlags = set of (
    rfNoMI         // flag is set if the output is not MI formatted
                   // some MI functions return normal output
                   // some normal functions return MI output
  );

  TGDBMIExecResult = record
    State: TDBGState;
    Values: String;
    Flags: TGDBMIResultFlags
  end;

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
    dfImplicidTypes,    // Debugger supports implicit types (^Type)
    dfForceBreak        // Debugger supports insertion of not yet known brekpoints
  );

  TGDBMIRTLCallingConvention = (ccDefault, ccRegCall, ccStdCall);

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

  { TGDBMIDebuggerProperties }

  TGDBMIDebuggerProperties = class(TDebuggerProperties)
  private
    FGDBOptions: String;
    FOverrideRTLCallingConvention: TGDBMIRTLCallingConvention;
  public
    constructor Create;
  published
    property OverrideRTLCallingConvention: TGDBMIRTLCallingConvention read FOverrideRTLCallingConvention write FOverrideRTLCallingConvention;
    property Debugger_Startup_Options: String read FGDBOptions write FGDBOptions;
  end;

  TGDBMIDebugger = class;

  { TGDBMIDebuggerCommand }

  TGDBMIDebuggerCommandState =
    ( dcsNone,         // Initial State
      dcsQueued,       // [None] => Queued behind other commands
      dcsExecuting,    // [None, Queued] => currently running
      // Final States, those lead to the object being freed, unless KeepFinished = True
      dcsFinished,     // [Executing] => Finished Execution
      dcsCanceled      // [Queued] => Never Executed
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

  TGDBMIDebuggerCommand = class
  private
    FDefaultTimeOut: Integer;
    FOnCancel: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnExecuted: TNotifyEvent;
    FKeepFinished: Boolean;
    FFreeLock: Integer;
    FFreeRequested: Boolean;
    FPriority: Integer;
    FProcessResultTimedOut: Boolean;
    FProperties: TGDBMIDebuggerCommandProperts;
    FQueueRunLevel: Integer;
    FState : TGDBMIDebuggerCommandState;
    FSeenStates: TGDBMIDebuggerCommandStates;
    FTheDebugger: TGDBMIDebugger; // Set during Execute
    FLastExecResult: TGDBMIExecResult;
    function GetDebuggerState: TDBGState;
    function GetTargetInfo: PGDBMITargetInfo;
    procedure SetKeepFinished(const AValue: Boolean);
  protected
    procedure SetDebuggerState(const AValue: TDBGState);
    procedure SetState(NewState: TGDBMIDebuggerCommandState);
    procedure DoStateChanged(OldState: TGDBMIDebuggerCommandState); virtual;
    procedure DoFree; virtual;
    procedure LockFree;
    procedure UnlockFree;
    procedure DoLockQueueExecute; virtual;
    procedure DoUnockQueueExecute; virtual;
    function  DoExecute: Boolean; virtual; abstract;
    procedure DoOnExecuted;
    procedure DoCancel; virtual;
    procedure DoOnCanceled;
    property SeenStates: TGDBMIDebuggerCommandStates read FSeenStates;
    property DebuggerState: TDBGState read GetDebuggerState;
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
    function  ProcessResult(var AResult: TGDBMIExecResult; ATimeOut: Integer = -1): Boolean;
    function  ProcessGDBResultText(S: String): String;
    function  GetFrame(const AIndex: Integer): String;
    function  GetText(const ALocation: TDBGPtr): String; overload;
    function  GetText(const AExpression: String; const AValues: array of const): String; overload;
    function  GetChar(const AExpression: String; const AValues: array of const): String; overload;
    function  GetFloat(const AExpression: String; const AValues: array of const): String;
    function  GetWideText(const ALocation: TDBGPtr): String;
    function  GetGDBTypeInfo(const AExpression: String): TGDBType;
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
    procedure ProcessFrame(const AFrame: String = '');
    procedure DoDbgEvent(const ACategory: TDBGEventCategory; const AText: String);
    property  TargetInfo: PGDBMITargetInfo read GetTargetInfo;
    property  LastExecResult: TGDBMIExecResult read FLastExecResult;
    property  DefaultTimeOut: Integer read FDefaultTimeOut write FDefaultTimeOut;
    property  ProcessResultTimedOut: Boolean read FProcessResultTimedOut;
    property  QueueRunLevel: Integer read FQueueRunLevel write FQueueRunLevel;  // if queue is nested
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
    property  KeepFinished: Boolean read FKeepFinished write SetKeepFinished;
    property  Priority: Integer read FPriority write FPriority;
    property  Properties: TGDBMIDebuggerCommandProperts read FProperties write FProperties;
  end;

  { TGDBMIDebugger }

  TGDBMIDebugger = class(TCmdLineDebugger)
  private
    FCommandQueue: TList;
    FCurrentCommand: TGDBMIDebuggerCommand;
    FCommandQueueExecLock: Integer;
    FCommandProcessingLock: Integer;

    FMainAddr: TDbgPtr;
    FBreakAtMain: TDBGBreakPoint;
    FBreakErrorBreakID: Integer;
    FRunErrorBreakID: Integer;
    FExceptionBreakID: Integer;
    FPauseWaitState: TGDBMIPauseWaitState;
    FInExecuteCount: Integer;
    FRunQueueOnUnlock: Boolean;
    FDebuggerFlags: TGDBMIDebuggerFlags;
    FCurrentStackFrame: Integer; // TODO: move to Stack-object
    FSourceNames: TStringList; // Objects[] -> TMap[Integer|Integer] -> TDbgPtr
    FReleaseLock: Integer;

    // GDB info (move to ?)
    FGDBVersion: String;
    FGDBCPU: String;
    FGDBPtrSize: integer; // PointerSize of the GDB-cpu
    FGDBOS: String;

    // Target info (move to record ?)
    FTargetInfo: TGDBMITargetInfo;

    // Implementation of external functions
    function  GDBEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
    function  GDBEvaluate(const AExpression: String; var AResult: String; out ATypeInfo: TGDBType): Boolean;
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

    procedure CallStackSetCurrent(AIndex: Integer);
    function  ConvertPascalExpression(var AExpression: String): Boolean;
    // ---
    procedure ClearSourceInfo;
    procedure GDBStopCallback(const AResult: TGDBMIExecResult; const ATag: PtrInt);
    function  FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
    procedure SelectStackFrame(AIndex: Integer);

    // All ExecuteCommand functions are wrappers for the real (full) implementation
    // ExecuteCommandFull is never called directly
    function  ExecuteCommand(const ACommand: String; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: PtrInt): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags; var AResult: TGDBMIExecResult): Boolean; overload;
    function  ExecuteCommandFull(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: PtrInt; var AResult: TGDBMIExecResult): Boolean; overload;
    procedure RunQueue;
    procedure QueueCommand(const ACommand: TGDBMIDebuggerCommand; ForceQueue: Boolean = False);
    procedure UnQueueCommand(const ACommand: TGDBMIDebuggerCommand);
    procedure CancelAllQueued;
    procedure CancelBeforeRun;
    function  StartDebugging(AContinueCommand: TGDBMIExecCommandType): Boolean;
    function  StartDebugging(AContinueCommand: TGDBMIExecCommandType; AValues: array of const): Boolean;
    function  StartDebugging(AContinueCommand: TGDBMIDebuggerCommand = nil): Boolean;

  protected
    {$IFDEF MSWindows}
    FPauseRequestInThreadID: Cardinal;
    {$ENDIF}
    procedure QueueExecuteLock;
    procedure QueueExecuteUnlock;

    function  ChangeFileName: Boolean; override;
    function  CreateBreakPoints: TDBGBreakPoints; override;
    function  CreateLocals: TDBGLocals; override;
    function  CreateLineInfo: TDBGLineInfo; override;
    function  CreateRegisters: TDBGRegisters; override;
    function  CreateCallStack: TDBGCallStack; override;
    function  CreateDisassembler: TDBGDisassembler; override;
    function  CreateWatches: TDBGWatches; override;
    function  GetSupportedCommands: TDBGCommands; override;
    function  GetTargetWidth: Byte; override;
    procedure InterruptTarget; virtual;
    function  ParseInitialization: Boolean; virtual;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
    procedure ClearCommandQueue;
    procedure DoState(const OldState: TDBGState); override;
    property  TargetPID: Integer read FTargetInfo.TargetPID;
    property  TargetPtrSize: Byte read FTargetInfo.TargetPtrSize;
    property  TargetFlags: TGDBMITargetFlags read FTargetInfo.TargetFlags write FTargetInfo.TargetFlags;
    property  PauseWaitState: TGDBMIPauseWaitState read FPauseWaitState;
    property  DebuggerFlags: TGDBMIDebuggerFlags read FDebuggerFlags;
    procedure DoRelease; override;   // Destroy self (or schedule)
  public
    class function CreateProperties: TDebuggerProperties; override; // Creates debuggerproperties
    class function Caption: String; override;
    class function ExePaths: String; override;

    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;

    procedure Init; override;         // Initializes external debugger
    procedure Done; override;         // Kills external debugger

    //LockCommandProcessing is more than just QueueExecuteLock
    //LockCommandProcessing also takes care to run the queue, if unlocked and not already running
    procedure LockCommandProcessing; override;
    procedure UnLockCommandProcessing; override;

    // internal testing
    procedure TestCmd(const ACommand: String); override;
  end;


implementation

type
  THackDBGType = class(TGDBType) end;

type
  TGDBMICpuRegister = record
    Name: String;
    Value: String;
    Modified: Boolean;
  end;
  TGDBMICpuRegisters = Array of TGDBMICpuRegister;

  TGDBMIEvaluationState = (esInvalid, esRequested, esValid);

  {%region       *****  TGDBMINameValueList and Parsers  *****   }

  PGDBMINameValue = ^TGDBMINameValue;
  TGDBMINameValue = record
    Name: TPCharWithLen;
    Value: TPCharWithLen;
  end;

  { TGDBMINameValueList }

  TGDBMINameValueList = class(TObject)
  private
    FText: String;
    FCount: Integer;
    FIndex: array of TGDBMINameValue;
    FUseTrim: Boolean;

    function Find(const AName : string): PGDBMINameValue;
    function GetItem(const AIndex: Integer): PGDBMINameValue;
    function GetString(const AIndex: Integer): string;
    function GetValue(const AName : string): string;
    function GetValuePtr(const AName: string): TPCharWithLen;
  public
    constructor Create(const AResultValues: String); overload;
    constructor Create(const AResultValues: TPCharWithLen); overload;
    constructor Create(AResult: TGDBMIExecResult); overload;
    constructor Create(const AResultValues: String; const APath: array of String); overload;
    constructor Create(AResult: TGDBMIExecResult; const APath: array of String); overload;
    procedure Delete(AIndex: Integer);
    procedure Init(const AResultValues: String);
    procedure Init(AResultValues: PChar; ALength: Integer);
    procedure Init(const AResultValues: TPCharWithLen);
    procedure SetPath(const APath: String); overload;
    procedure SetPath(const APath: array of String); overload;
    function IndexOf(const AName: string): Integer;
    property Count: Integer read FCount;
    property Items[const AIndex: Integer]: PGDBMINameValue read GetItem;
    property Values[const AName: string]: string read GetValue;
    property ValuesPtr[const AName: string]: TPCharWithLen read GetValuePtr;
    property UseTrim: Boolean read FUseTrim write FUseTrim;
  end;

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

  { TGDBMIDebuggerCommandStartDebugging }

  TGDBMIDebuggerCommandStartDebugging = class(TGDBMIDebuggerCommand)
  private
    FContinueCommand: TGDBMIDebuggerCommand;
    FSuccess: Boolean;
  protected
    function  DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; AContinueCommand: TGDBMIDebuggerCommand);
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
  protected
    procedure DoLockQueueExecute; override;
    procedure DoUnockQueueExecute; override;
    function  ProcessRunning(var AStoppedParams: String): Boolean;
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

  {%endregion    *^^^*  TGDBMIDebuggerCommands  *^^^*   }

  {%region      *****  Locals  *****   }

  { TGDBMIDebuggerCommandLocals }

  TGDBMIDebuggerCommandLocals = class(TGDBMIDebuggerCommand)
  private
    FResult: TStringList;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger);
    destructor Destroy; override;
    function DebugText: String; override;
    property Result: TStringList read FResult;
  end;

  { TGDBMILocals }

  TGDBMILocals = class(TDBGLocals)
  private
    FEvaluatedState: TGDBMIEvaluationState;
    FEvaluationCmdObj: TGDBMIDebuggerCommandLocals;
    FInLocalsNeeded: Boolean;
    FLocals: TStringList;
    procedure LocalsNeeded;
    procedure CancelEvaluation;
    procedure DoEvaluationDestroyed(Sender: TObject);
    procedure DoEvaluationFinished(Sender: TObject);
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure Invalidate;
    function GetCount: Integer; override;
    function GetName(const AnIndex: Integer): String; override;
    function GetValue(const AnIndex: Integer): String; override;
  public
    procedure Changed; override;
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
    function ExecBreakDelete(ABreakId: Integer): Boolean;
    function ExecBreakInsert(ASource: string; ALine: Integer; AEnabled: Boolean;
                             out ABreakId, AHitCnt: Integer; out AnAddr: TDBGPtr): Boolean;
    function ExecBreakEnabled(ABreakId: Integer; AnEnabled: Boolean): Boolean;
    function ExecBreakCondition(ABreakId: Integer; AnExpression: string): Boolean;
  end;

  { TGDBMIDebuggerCommandBreakInsert }

  TGDBMIDebuggerCommandBreakInsert = class(TGDBMIDebuggerCommandBreakPointBase)
  private
    FSource: string;
    FLine: Integer;
    FEnabled: Boolean;
    FExpression: string;
    FReplaceId: Integer;

    FAddr: TDBGPtr;
    FBreakID: Integer;
    FHitCnt: Integer;
    FValid: Boolean;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; ASource: string; ALine: Integer;
                       AEnabled: Boolean; AnExpression: string; AReplaceId: Integer);
    function DebugText: String; override;
    property Source: string read FSource write FSource;
    property Line: Integer read FLine write FLine;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Expression: string read FExpression write FExpression;
    property ReplaceId: Integer read FReplaceId write FReplaceId;
    // result values
    property BreakID: Integer read FBreakID;
    property Addr: TDBGPtr read FAddr;
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
    FBreakID: Integer;
    FParsedExpression: String;
    FCurrentCmd: TGDBMIDebuggerCommandBreakPointBase;
    FUpdateFlags: TGDBMIBreakPointUpdateFlags;
    procedure SetBreakPoint;
    procedure ReleaseBreakPoint;
    procedure UpdateProperties(AFlags: TGDBMIBreakPointUpdateFlags);
    procedure DoCommandDestroyed(Sender: TObject);
    procedure DoCommandExecuted(Sender: TObject);
  protected
    procedure DoEndUpdate; override;
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure SetLocation(const ASource: String; const ALine: Integer); override;
  end;

  {%endregion   ^^^^^  BreakPoints  ^^^^^   }

  {%region      *****  Register  *****   }

  { TGDBMIDebuggerCommandRegisterNames }

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
    FRegistersToUpdate: TGDBMICpuRegisters;
  protected
    function DoExecute: Boolean; override;
  public
    // updates the given array directly
    constructor Create(AOwner: TGDBMIDebugger; RegistersToUpdate: TGDBMICpuRegisters);
    function DebugText: String; override;
  end;

  { TGDBMIRegisters }

  TGDBMIRegisters = class(TDBGRegisters)
  private
    FRegisters: TGDBMICpuRegisters;

    FGetRegisterCmdObj: TGDBMIDebuggerCommandRegisterNames;
    FRegistersReqState: TGDBMIEvaluationState;
    FInRegistersNeeded: Boolean;

    FGetValuesCmdObj: TGDBMIDebuggerCommandRegisterValues;
    FValuesReqState: TGDBMIEvaluationState;
    FInValuesNeeded: Boolean;

    procedure RegistersNeeded;
    procedure ValuesNeeded;
    procedure DoGetRegisterNamesDestroyed(Sender: TObject);
    procedure DoGetRegisterNamesFinished(Sender: TObject);
    procedure DoGetRegValuesDestroyed(Sender: TObject);
    procedure DoGetRegValuesFinished(Sender: TObject);
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure Invalidate;
    function GetCount: Integer; override;
    function GetModified(const AnIndex: Integer): Boolean; override;
    function GetName(const AnIndex: Integer): String; override;
    function GetValue(const AnIndex: Integer): String; override;
  public
    procedure Changed; override;
  end;

  {%endregion   ^^^^^  Register  ^^^^^   }

  {%region      *****  Watches  *****   }

  { TGDBMIDebuggerCommandEvaluate }

  TGDBMIDebuggerCommandEvaluate = class(TGDBMIDebuggerCommand)
  private
    FExpression: String;
    FDisplayFormat: TWatchDisplayFormat;
    FTextValue: String;
    FTypeInfo: TGDBType;
    FTypeInfoAutoDestroy: Boolean;
    function GetTypeInfo: TGDBType;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; const AExpression: String;
      const ADisplayFormat: TWatchDisplayFormat);
    destructor Destroy; override;
    function DebugText: String; override;
    property Expression: String read FExpression;
    property DisplayFormat: TWatchDisplayFormat read FDisplayFormat;
    property TextValue: String read FTextValue;
    property TypeInfo: TGDBType read GetTypeInfo;
    property TypeInfoAutoDestroy: Boolean read FTypeInfoAutoDestroy write FTypeInfoAutoDestroy;
  end;

  { TGDBMIWatch }

  TGDBMIWatch = class(TDBGWatch)
  private
    FEvaluatedState: TGDBMIEvaluationState;
    FEvaluationCmdObj: TGDBMIDebuggerCommandEvaluate;
    FInEvaluationNeeded: Boolean;
    FValue: String;
    FTypeInfo: TGDBType;
    procedure EvaluationNeeded;
    procedure CancelEvaluation;
    procedure ClearOwned;
    procedure DoEvaluationFinished(Sender: TObject);
    procedure DoEvaluationDestroyed(Sender: TObject);
  protected
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoDisplayFormatChanged; override;
    procedure DoChange; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
    function  GetValue: String; override;
    function  GetTypeInfo: TDBGType; override;
    function  GetValid: TValidState; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Invalidate;
  end;


  { TGDBMIWatches }

  TGDBMIWatches = class(TDBGWatches)
  private
  protected
    FParentFPList: Array of Integer;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure Changed;
  public
  end;

  {%endregion   ^^^^^  Watches  ^^^^^   }

  {%region      *****  Stack  *****   }

  { TGDBMIDebuggerCommandStackFrames }

  TGDBMINameValueListArray = array of TGDBMINameValueList;

  TGDBMIDebuggerCommandStackFrames = class(TGDBMIDebuggerCommand)
  private
    FArgs: TGDBMINameValueListArray;
    FFrames: TGDBMINameValueListArray;
    FIndex: Integer;
    FCount: Integer;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TGDBMIDebugger; AIndex, ACount: Integer);
    destructor  Destroy; override;
    function DebugText: String; override;
    property Index: Integer read FIndex write FIndex;
    property Count: Integer read FCount write FCount;
    property Args: TGDBMINameValueListArray read FArgs;
    property Frames: TGDBMINameValueListArray read FFrames;
  end;

  { TGDBMIDebuggerCommandStackDepth }

  TGDBMIDebuggerCommandStackDepth = class(TGDBMIDebuggerCommand)
  private
    FDepth: Integer;
  protected
    function DoExecute: Boolean; override;
  public
    function DebugText: String; override;
    property Depth: Integer read FDepth;
  end;

  { TGDBMICallStack }

  TGDBMICallStack = class(TDBGCallStack)
  private
    FFramesEvalCmdObj: TGDBMIDebuggerCommandStackFrames;
    FDepthEvalCmdObj: TGDBMIDebuggerCommandStackDepth;
    FInEvalDepth: Boolean;
    FInEvalFrames: Boolean;
    function InternalCreateEntry(AIndex: Integer; AArgInfo, AFrameInfo: TGDBMINameValueList): TCallStackEntry;
    procedure DoDepthCommandExecuted(Sender: TObject);
    procedure DoFramesCommandExecuted(Sender: TObject);
    procedure DoDepthCommandDestroyed(Sender: TObject);
    procedure DoFramesCommandDestroyed(Sender: TObject);
  protected
    procedure Clear; override;
    function CheckCount: Boolean; override;
    function CreateStackEntry(AIndex: Integer): TCallStackEntry; override;
    procedure PrepareEntries(AIndex, ACount: Integer); override;

    function GetCurrent: TCallStackEntry; override;
    procedure SetCurrent(AValue: TCallStackEntry); override;
  public
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
    function CurrentFixedAddr: TDBGPtr; // Addr[0] - Offs[0]
    // About the next SubList
    function NextStartAddr: TDBGPtr;
    function NextStartOffs: Integer;
    // Overall
    function CountLinesAfterCounterAddr: Integer; // count up to Startof Current SubList

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
  private
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

function ConvertToGDBPath(APath: string): string;
// GDB wants forward slashes in its filenames, even on win32.
begin
  Result := APath;
  // no need to process empty filename
  if Result='' then exit;

  {$WARNINGS off}
  if DirectorySeparator <> '/' then
    Result := StringReplace(Result, DirectorySeparator, '/', [rfReplaceAll]);
  {$WARNINGS on}
  Result := '"' + Result + '"';
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

  FItems[Index].ParsedInfo.SrcFileName := PCLenToString(FItems[Index].SrcFile, True);
  FItems[Index].ParsedInfo.SrcFileLine := PCLenToInt(FItems[Index].SrcLine, 0);
  // SrcStatementIndex, SrcStatementCount are already set

  FItems[Index].ParsedInfo.Addr      := PCLenToQWord(AsmList.ValuesPtr['address'], 0);
  FItems[Index].ParsedInfo.Statement := PCLenToString(AsmList.ValuesPtr['inst'], True);
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
  Result := ((NextIdx > FMaxIdx) or (FList.Item[NextIdx]^.Offset = 0))
        and ( (not HasLocate) or ((FIndexOfLocateAddress < 0) or (FOffsetOfLocateAddress = 0)) );
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

function TGDBMIDisassembleResultFunctionIterator.CurrentFixedAddr: TDBGPtr;
begin
  Result := FList.Item[CurrentIndex]^.Addr - FList.Item[CurrentIndex]^.Offset;
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
begin
  Result := PCLenToString(FNameValueList.Items[Index]^.Name, True);
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
  if (Debugger = nil) or (Debugger.State <> dsPause)
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
            and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued);
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
    (avFoundFunction, avFoundRange, avFoundStatemnet,  // known address
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
  TrustedValidity = [avFoundFunction, avFoundRange, avFoundStatemnet];

  function InitAddress(AValue: TDBGPtr; AValidity: TAddressValidity;
    AnOffset: Integer = -1): TAddress;
  begin
    Result.Value          := AValue;
    Result.GuessedValue   := AValue;;
    Result.Offset   := AnOffset;
    Result.Validity := AValidity;
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
    ExecuteCommand('-data-disassemble -s %u -e %u -- %d', [AStartAddr, AnEndAddr, WS], R);
    if AResultList <> nil
    then begin
      Result := AResultList;
      Result.Init(R);
    end
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
    ExecuteCommand('-data-read-memory %u x 1 1 %u', [AStartAddr, ACount], R);
    if AResultList <> nil
    then begin
      Result := AResultList;
      Result.Init(R);
    end
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

  procedure CopyToRange(const ADisAssList: TGDBMIDisassembleResultList;
    const ADestRange: TDBGDisassemblerEntryRange; AFromIndex, ACount: Integer;
    const ASrcInfoDisAssList: TGDBMIDisassembleResultList = nil);
  var
    i, j, MinInSrc, MaxInSrc: Integer;
    ItmPtr, ItmPtr2, LastItem: PDisassemblerEntry;
  begin
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
    FromIndex := ARange.IndexOfAddrWithOffs(AFirstAddr)-1;
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

      if NextItm <> nil
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
            DisAssListCurrentSub := ExecDisassmble(DisAssIterator.CurrentFixedAddr,
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
    BlockOk, GotFullDisAss, SkipDisAssInFirstLoop: Boolean;
    s: String;
    Itm: TDisassemblerEntry;
  begin
    Result := False;
    DisAssList := nil;
    DisAssListCurrentSub := nil;
    DisAssListWithSrc := nil;
    DisAssIterator := nil;
    OrigFirstAddress := AFirstAddr;
    OrigLastAddress := ALastAddr;

    NewRange := TDBGDisassemblerEntryRange.Create;

    // No nice startingpoint found, just start to disassemble aprox 5 instructions before it
    // and hope that when we started in the middle of an instruction it get sorted out.
    // If so, the 4st for lines from the result must be discarded
    if not (AFirstAddr.Validity in TrustedValidity)
    then begin
      AFirstAddr.Value := AFirstAddr.Value - 5 * DAssBytesPerCommandMax;
      AFirstAddr.Validity := avPadded;
      AFirstAddr.Offset := -1;
    end;

    // Adjust ALastAddr
    if ALastAddr.Value < AFirstAddr.Value
    then begin
      ALastAddr.Value := AFirstAddr.Value + 2 * DAssBytesPerCommandMax;
      ALastAddr.Validity := avPadded;
    end
    else
    if not (ALastAddr.Validity in TrustedValidity)
    then begin
      ALastAddr.Value := ALastAddr.Value + 2 * DAssBytesPerCommandMax;
      ALastAddr.Validity := avPadded;
    end;

    {$IFDEF DBG_VERBOSE}
    DebugLnEnter(['INFO: DoDisassembleRange for AFirstAddr =', DbgsAddr(AFirstAddr),
    ' ALastAddr=', DbgsAddr(ALastAddr), ' OrigFirst=', DbgsAddr(OrigFirstAddress), ' OrigLastAddress=', DbgsAddr(OrigLastAddress),
    '  StopAffterAddr=', StopAfterAddress, ' StopAfterLines=',  StopAfterNumLines ]);
    try
    {$ENDIF}

    // check if we have an overall source-info
    // we can only do that, if we know the offset of firstaddr (limit to DAssRangeOverFuncTreshold avg lines, should be enough)
    if (AFirstAddr.Offset >= 0)
    then DisAssListWithSrc := ExecDisassmble
          (AFirstAddr.Value - Min(AFirstAddr.Offset, DAssRangeOverFuncTreshold * DAssBytesPerCommandAvg),
           ALastAddr.Value, True);
    GotFullDisAss := (DisAssListWithSrc <> nil) and (DisAssListWithSrc.Count > 0) and DisAssListWithSrc.HasSourceInfo;
    SkipDisAssInFirstLoop := False;

    if GotFullDisAss
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
      TmpAddr := DisAssListWithSrc.LastItem^.Addr + 2 * DAssBytesPerCommandAlign;
      if ALastAddr.Value > TmpAddr
      then TmpAddr := ALastAddr.Value;
      DisAssList := ExecDisassmble(DisAssListWithSrc.LastItem^.Addr, TmpAddr, False);

      // Add the known source list
      if DisAssList.Count < 2
      then TmpAddr := ALastAddr.Value
      else TmpAddr := DisAssList.Item[1]^.Addr;

      DisAssIterator := TGDBMIDisassembleResultFunctionIterator.Create
        (DisAssListWithSrc, 0, TmpAddr , FStartAddr, StopAfterAddress);
      NewRange.Capacity := Max(NewRange.Capacity, NewRange.Count  + DisAssListWithSrc.Count);
      while not DisAssIterator.EOL
      do begin
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

      FreeAndNil(DisAssIterator);
      FreeAndNil(DisAssListWithSrc);
      // Source Completly Added

      // DisAssList contains data after source (from index = 1)
      if DisAssList.Count < 2
      then begin
        // nothing to continue
        NewRange.RangeStartAddr := AFirstAddr.Value;
        NewRange.RangeEndAddr := OrigLastAddress.Value; // we have/will have tried our best to get everything to this value. If we didn't then neither will we later
        NewRange.LastEntryEndAddr := ALastAddr.Value;
        FKnownRanges.AddRange(NewRange);  // NewRange is now owned by FKnownRanges
        NewRange := nil;
        FreeAndNil(DisAssList);
        exit;
      end;


      // continue with the DisAsslist for the remainder
      GotFullDisAss := False;
      AFirstAddr.Validity := avFoundFunction; //  if we got source, then start is ok (original start is kept)
      DisAssStartIdx := 1;
      SkipDisAssInFirstLoop := True;
      StopAfterNumLines := StopAfterNumLines - NewRange.Count;
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
        NewRange.RangeEndAddr     := OrigLastAddress.Value;
        NewRange.LastEntryEndAddr := OrigLastAddress.Value;
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
      ALastAddr.Validity := avFoundStatemnet;
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
      AFirstAddr.Validity := avFoundStatemnet;
    end;


    NewRange.Capacity := Max(NewRange.Capacity, NewRange.Count  + Cnt);
    NewRange.RangeStartAddr := AFirstAddr.Value;
    NewRange.RangeEndAddr := OrigLastAddress.Value; // we have/will have tried our best to get everything to this value. If we didn't then neither will we later
    NewRange.LastEntryEndAddr := ALastAddr.Value;

    DisAssIterator := TGDBMIDisassembleResultFunctionIterator.Create
      (DisAssList, DisAssStartIdx, ALastAddr.Value, FStartAddr, StopAfterAddress);

    while not DisAssIterator.EOL
    do begin
      BlockOk := DisAssIterator.NextSubList(DisAssListCurrentSub);

      if (DisAssIterator.CountLinesAfterCounterAddr > StopAfterNumLines)       // got enough lines
      // only if the next block is good to go
      and not ( (not BlockOk) or (DisAssListCurrentSub.Item[0]^.Offset <> 0) or (not GotFullDisAss) )
      then begin
        NewRange.LastEntryEndAddr := DisAssIterator.NextStartAddr;
        NewRange.RangeEndAddr := DisAssIterator.NextStartAddr;
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
        if GotFullDisAss
        then begin
          //get the source-less code as reference
          DisAssListCurrentSub := ExecDisassmble(DisAssIterator.CurrentFixedAddr,
            DisAssIterator.NextStartAddr, False, DisAssListCurrentSub, True);
          CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count, DisAssListWithSrc);
          Result := Result or (DisAssListCurrentSub.Count > 0);
          continue;
        end
        else begin
          // Try source first
          DisAssListWithSrc := ExecDisassmble(DisAssIterator.CurrentFixedAddr,
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
          DisAssListCurrentSub := ExecDisassmble(DisAssIterator.CurrentFixedAddr,
            DisAssIterator.NextStartAddr, False, DisAssListCurrentSub, True);
          CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count, DisAssListWithSrc);
          Result := Result or (DisAssListCurrentSub.Count > 0);
          continue;
        end;
      end;

      // Todo: Check for wrong start stmnt offset
      if not BlockOk
      and (DisAssIterator.NextStartOffs <> 0)
      then begin
        // overlap into next proc
        {$IFDEF DBG_VERBOSE}
        debugln(['WARNING: FindProcEnd found an overlap (',DisAssIterator.NextStartOffs,') at block end: FromIdx=', DisAssIterator.CurrentIndex, ' NextIdx=', DisAssIterator.NextIndex,
        ' SequenceNo=', DisAssIterator.SublistNumber, ' StartIdx=', DisAssIterator.IndexOfLocateAddress, ' StartOffs=', DisAssIterator.OffsetOfLocateAddress]);
        {$ENDIF}
        s := DisAssListCurrentSub.LastItem^.Dump;
        s := copy(s, 1, Max(0, length(s) - DisAssIterator.NextStartOffs * 2));
        if s = ''
        then s := ' ';
        DisAssListCurrentSub.LastItem^.Dump := s;
        DisAssListCurrentSub.LastItem^.Statement := '';
        BlockOk := True;
      end;

      if BlockOk
      then begin
        // Got a good block
        if (DisAssListCurrentSub.Item[0]^.FuncName <> '')
        then begin
          // Try to get source-info (up to DisAssIterator.NextStartAddr)
          // Subtract offset from StartAddress, in case this is the first block
          //   (we may continue existing data, but src info must be retrieved in full, or may be incomplete)
          // If we are in IsFirstSubList, we already tried
          if (not GotFullDisAss)
          and not(DisAssIterator.IsFirstSubList and SkipDisAssInFirstLoop)
          then DisAssListWithSrc := ExecDisassmble(DisAssIterator.CurrentFixedAddr,
            DisAssIterator.NextStartAddr, True, DisAssListWithSrc, True);
          // We may have less lines with source, as we stripped padding at the end
          if (DisAssListWithSrc <> nil) and DisAssListWithSrc.HasSourceInfo
          then begin
            CopyToRange(DisAssListCurrentSub, NewRange, 0, DisAssListCurrentSub.Count, DisAssListWithSrc);
            Result := Result or (DisAssListCurrentSub.Count > 0);
            continue;
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
    MemDump := nil;;
    for i := 0 to length(FMemDumpsNeeded) - 1 do
    begin
      FirstAddr := FMemDumpsNeeded[i].FirstAddr;
      Rng := FRangeIterator.GetRangeForAddr(FirstAddr, True);
      if rng <> nil
      then MemDump := ExecMemDump(FirstAddr, FMemDumpsNeeded[i].LastAddr - FirstAddr, MemDump);
      while (Rng <> nil) and (Rng.FirstAddr <= FMemDumpsNeeded[i].LastAddr) do
      begin
        AddMemDumpToRange(Rng, MemDump, FMemDumpsNeeded[i].FirstAddr, FMemDumpsNeeded[i].LastAddr);
        Rng := FRangeIterator.NextRange;
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
  TryStartAt.GuessedValue := FStartAddr - FLinesBefore * DAssBytesPerCommandAvg;
  AdjustToRangeOrKnowFunctionStart(TryStartAt, RngBefore);

  // Guess Maximum, will adjust later
  TryEndAt := InitAddress(FEndAddr + FLinesAfter * DAssBytesPerCommandAvg, avGuessed);

  // Read as many unknown ranges, until LinesAfter is met
  GotCnt := -1;
  while(True)
  do begin
    // check if we need any LinesAfter
    LastGotCnt:= GotCnt;
    GotCnt := 0;
    TmpAddr := FEndAddr;
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
      debugln(['Disassembler: *** Failure to get any mor lines while scanning forward LastGotCnt=',LastGotCnt, ' now GotCnt=',GotCnt, ' Requested=',FLinesAfter]);
      break;
    end;

    RngAfter := FRangeIterator.NextRange;
    // adjust TryEndAt
    if (RngAfter <> nil) and (TryEndAt.Value >= RngAfter.RangeStartAddr)
    then begin
      TryEndAt.Value := RngAfter.RangeStartAddr;
      TryEndAt.Validity := avFoundRange;
    end;

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
    LastGotCnt:= GotCnt;
    if (RngAfter = nil)
    then begin
      debugln(['INTERNAL ERROR: (linesbefore) Missing the data, that was disassembled: from ', DbgsAddr(TryStartAt),' to ', DbgsAddr(TryEndAt)]);
      break;
    end;

    GotCnt := RngAfter.IndexOfAddrWithOffs(FEndAddr);  // already known before
    if GotCnt >= FLinesBefore
    then break;
    if LastGotCnt >= GotCnt
    then begin
      debugln(['Disassembler: *** Failure to get any mor lines while scanning backward LastGotCnt=',LastGotCnt, ' now GotCnt=',GotCnt, ' Requested=',FLinesBefore]);
      break;
    end;

    TryEndAt := InitAddress(RngAfter.RangeStartAddr, avFoundRange);
    TryStartAt := InitAddress(TryEndAt.Value - 1, avGuessed);
    TryStartAt.GuessedValue := TryEndAt.Value - (FLinesBefore - GotCnt) * DAssBytesPerCommandAvg;
    // and adjust
    RngBefore := FRangeIterator.PreviousRange;
    {$PUSH}{$IFnDEF DBGMI_WITH_DISASS_OVERFLOW}{$Q-}{$R-}{$ENDIF} // Overflow is allowed to occur
    if (RngBefore <> nil)
    and (TryStartAt.Value > RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr)
    and (TryStartAt.Value - RngBefore.EntriesPtr[RngBefore.Count - 1]^.Addr > (FLinesBefore - GotCnt) * DAssBytesPerCommandAvg)
    then RngBefore := nil;
    {$POP}
    AdjustToRangeOrKnowFunctionStart(TryStartAt, RngBefore);

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

  function InsertBreakPoint(const AName: String): Integer;
  var
    R: TGDBMIExecResult;
    S: String;
    ResultList: TGDBMINameValueList;
  begin
    // Try to retrieve the address of the procedure
    if ExecuteCommand('info address ' + AName, R)
    and (R.State <> dsError)
    then begin
      S := GetPart(['at address ', ' at '], ['.', ' '], R.Values);
      if S <> ''
      then begin
        ExecuteCommand('-break-insert *%u', [StrToQWordDef(S, 0)], R);
        if R.State = dsError then Exit(-1);
        ResultList := TGDBMINameValueList.Create(R, ['bkpt']);
        Result := StrToIntDef(ResultList.Values['number'], -1);
        ResultList.Free;
        Exit;
      end;
    end;

    ExecuteCommand('-break-insert %s', [AName], R);
    if R.State = dsError then Exit(-1);

    ResultList := TGDBMINameValueList.Create(R, ['bkpt']);
    Result := StrToIntDef(ResultList.Values['number'], -1);
    ResultList.Free;
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
      'elf64-x86-64',
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
      // Todo, check PtrSize and downgrade 64 bit cpu to 32 bit cpu, if required
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

  function SetTempMainBreak: Boolean;
  var
    R: TGDBMIExecResult;
    S: String;
    ResultList: TGDBMINameValueList;
  begin
    // Try to retrieve the address of main. Setting a break on main is past initialization
    if ExecuteCommand('info address main', R)
    and (R.State <> dsError)
    then begin
      S := GetPart(['at address ', ' at '], ['.', ' '], R.Values);
      if S <> ''
      then begin
        FTheDebugger.FMainAddr := StrToQWordDef(S, 0);
        ExecuteCommand('-break-insert -t *%u', [FTheDebugger.FMainAddr], R);
        Result := R.State <> dsError;
        if Result then Exit;
      end;
    end;

    ExecuteCommand('-break-insert -t main', R);
    Result := R.State <> dsError;
    if not Result then Exit;

    ResultList := TGDBMINameValueList.Create(R, ['bkpt']);
    FTheDebugger.FMainAddr := StrToQWordDef(ResultList.Values['addr'], 0);
    ResultList.Free;
  end;

var
  R: TGDBMIExecResult;
  FileType, EntryPoint: String;
  List: TGDBMINameValueList;
  TargetPIDPart: String;
  TempInstalled, CanContinue: Boolean;
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
      ExecuteCommand('-environment-cd %s', [ConvertToGDBPath(UTF8ToSys(FTheDebugger.WorkingDir))], [cfCheckError]);
    end;

    TargetInfo^.TargetFlags := [tfHasSymbols]; // Set until proven otherwise

    // check if the exe is compiled with FPC >= 1.9.2
    // then the rtl is compiled with regcalls
    RetrieveRegCall;

    // also call execute -exec-arguments if there are no arguments in this run
    // so the possible arguments of a previous run are cleared
    ExecuteCommand('-exec-arguments %s', [FTheDebugger.Arguments], [cfCheckState]);

    // set the output width to a great value to avoid unexpected
    // new lines like in large functions or procedures
    ExecuteCommand('set width 50000', []);

    if tfHasSymbols in TargetInfo^.TargetFlags
    then begin
      // Make sure we are talking pascal
      ExecuteCommand('-gdb-set language pascal', [cfCheckError]);
      TempInstalled := SetTempMainBreak;
    end
    else begin
      DebugLn('TGDBMIDebugger.StartDebugging Note: Target has no symbols');
      TempInstalled := False;
    end;

    // check whether we need class cast dereference
    R := CheckHasType('TObject', tfFlagHasTypeObject);
    if R.State <> dsError
    then begin
      if UpperCase(LeftStr(R.Values, 15)) = UpperCase('type = ^TOBJECT')
      then include(TargetInfo^.TargetFlags, tfClassIsPointer);
    end;
    R := CheckHasType('Exception', tfFlagHasTypeException);
    if R.State <> dsError
    then begin
      if UpperCase(LeftStr(R.Values, 17)) = UpperCase('type = ^EXCEPTION')
      then include(TargetInfo^.TargetFlags, tfExceptionIsPointer);
    end;
    CheckHasType('Shortstring', tfFlagHasTypeShortstring);
    //CheckHasType('PShortstring', tfFlagHasTypePShortString);
    CheckHasType('pointer', tfFlagHasTypePointer);
    CheckHasType('byte', tfFlagHasTypeByte);
    //CheckHasType('char', tfFlagHasTypeChar);

    // try Insert Break breakpoint
    // we might have rtl symbols
    if FTheDebugger.FExceptionBreakID = -1
    then FTheDebugger.FExceptionBreakID := InsertBreakPoint('FPC_RAISEEXCEPTION');
    if FTheDebugger.FBreakErrorBreakID = -1
    then FTheDebugger.FBreakErrorBreakID := InsertBreakPoint('FPC_BREAK_ERROR');
    if FTheDebugger.FRunErrorBreakID = -1
    then FTheDebugger.FRunErrorBreakID := InsertBreakPoint('FPC_RUNERROR');

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

    if not TempInstalled and (EntryPoint <> '')
    then begin
      // We could not set our initial break to get info and allow stepping
      // Try it with the program entry point
      FTheDebugger.FMainAddr := StrToQWordDef(EntryPoint, 0);
      ExecuteCommand('-break-insert -t *%u', [FTheDebugger.FMainAddr], R);
      TempInstalled := R.State <> dsError;
    end;

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

    TargetInfo^.TargetPID := 0;

    // fire the first step
    if TempInstalled
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

    if R.State = dsNone
    then begin
      SetDebuggerState(dsInit);
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
    FreeAndNil(FContinueCommand);
  end;

  FSuccess := True;
end;

constructor TGDBMIDebuggerCommandStartDebugging.Create(AOwner: TGDBMIDebugger;
  AContinueCommand: TGDBMIDebuggerCommand);
begin
  inherited Create(AOwner);
  FContinueCommand := AContinueCommand;
  FSuccess := False;
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

function TGDBMIDebuggerCommandExecute.ProcessRunning(var AStoppedParams: String): Boolean;

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

  procedure DoNotifyAsync(var Line: String);
  var
    S: String;
  begin
    S := GetPart(['='], [','], Line, False, False);
    case StringCase(S, ['shlibs-added', 'shlibs-updated',
      'library-loaded', 'library-unloaded'], False, False)
    of
      0..3: DoDbgEvent(ecModule, Line);
    else
      DebugLn('[Debugger] Notify output: ', Line);
    end;
  end;

  procedure DoResultRecord(const Line: String);
  begin
    DebugLn('[WARNING] Debugger: unexpected result-record: ', Line);
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
  begin
    DebugLn('[Debugger] Log output: ', Line);
  end;

var
  S: String;
  idx: Integer;
begin
  Result := True;
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
        '=': DoNotifyAsync(S);
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

  function GetLocation: TDBGLocationRec;
  var
    R: TGDBMIExecResult;
    S: String;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      Result.SrcLine := -1;
      Result.SrcFile := '';
      Result.FuncName := '';
      if tfRTLUsesRegCall in TargetInfo^.TargetFlags
      then Result.Address := GetPtrValue(TargetInfo^.TargetRegisters[1], [])
      else Result.Address := GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 3]);

      Str(Result.Address, S);
      if ExecuteCommand('info line * POINTER(%s)', [S], R)
      then begin
        Result.SrcLine := StrToIntDef(GetPart('Line ', ' of', R.Values), -1);
        Result.SrcFile := ConvertGdbPathAndFile(GetPart('\"', '\"', R.Values));
      end;
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

  procedure ProcessException(AInfo: TGDBMIExceptionInfo);
  var
    ExceptionMessage: String;
    CanContinue: Boolean;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      if (dfImplicidTypes in FTheDebugger.DebuggerFlags)
      then begin
        if (tfFlagHasTypeException in TargetInfo^.TargetFlags) then begin
          if tfExceptionIsPointer in TargetInfo^.TargetFlags
          then ExceptionMessage := GetText('Exception(%s).FMessage', [AInfo.ObjAddr])
          else ExceptionMessage := GetText('^Exception(%s)^.FMessage', [AInfo.ObjAddr]);
          //ExceptionMessage := GetText('^^Exception($fp+8)^^.FMessage', []);
        end else begin
          // Only works if Exception class is not changed. FMessage must be first member
          ExceptionMessage := GetText('^^char(^%s(%s)+1)^', [PointerTypeCast, AInfo.ObjAddr]);
        end;
      end
      else ExceptionMessage := '### Not supported on GDB < 5.3 ###';
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;

    FTheDebugger.DoException(deInternal, AInfo.Name, ExceptionMessage, CanContinue);
    if CanContinue
    then begin
      //ExecuteCommand('-exec-continue')
      Result := True; // outer funciton result
      exit;
    end
    else FTheDebugger.DoCurrent(GetLocation);
  end;

  procedure ProcessBreak;
  var
    ErrorNo: Integer;
    CanContinue: Boolean;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      if tfRTLUsesRegCall in TargetInfo^.TargetFlags
      then ErrorNo := GetIntValue(TargetInfo^.TargetRegisters[0], [])
      else ErrorNo := Integer(GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 2]));
      ErrorNo := ErrorNo and $FFFF;
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;

    FTheDebugger.DoException(deRunError, Format('RunError(%d)', [ErrorNo]), '', CanContinue);
    if CanContinue
    then begin
      //ExecuteCommand('-exec-continue')
      Result := True; // outer funciton result
      exit;
    end
    else FTheDebugger.DoCurrent(GetLocation);
  end;

  procedure ProcessRunError;
  var
    ErrorNo: Integer;
    CanContinue: Boolean;
  begin
    FTheDebugger.QueueExecuteLock;
    try
      if tfRTLUsesRegCall in TargetInfo^.TargetFlags
      then ErrorNo := GetIntValue(TargetInfo^.TargetRegisters[0], [])
      else ErrorNo := Integer(GetData('$fp+%d', [TargetInfo^.TargetPtrSize * 2]));
      ErrorNo := ErrorNo and $FFFF;
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;

    FTheDebugger.DoException(deRunError, Format('RunError(%d)', [ErrorNo]), '', CanContinue);
    if CanContinue
    then begin
      //ExecuteCommand('-exec-continue')
      Result := True; // outer funciton result
      exit;
    end
    else ProcessFrame(GetFrame(1));
  end;

  procedure ProcessSignalReceived(const AList: TGDBMINameValueList);
  var
    SigInt, CanContinue: Boolean;
    S, F: String;
    fixed: Boolean;
  begin
    // TODO: check to run (un)handled

    S := AList.Values['signal-name'];
    F := AList.Values['frame'];
    {$IFdef MSWindows}
    SigInt := S = 'SIGTRAP';
    {$ELSE}
    SigInt := S = 'SIGINT';
    {$ENDIF}
    if not AIgnoreSigIntState
    or not SigInt
    then begin
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
    then FTheDebugger.DoException(deExternal, 'External: ' + S, '', CanContinue);

    FTheDebugger.QueueExecuteLock;
    try
      if not AIgnoreSigIntState
      or not SigInt
      then ProcessFrame(F);
    finally
      FTheDebugger.QueueExecuteUnlock;
    end;
  end;

var
  List: TGDBMINameValueList;
  Reason: String;
  BreakID: Integer;
  BreakPoint: TGDBMIBreakPoint;
  CanContinue: Boolean;
  ExceptionInfo: TGDBMIExceptionInfo;
begin
  (* The Queue is not locked / This code can be interupted
     Therefore all calls to ExecuteCommand (gdb cmd) must be wrapped in QueueExecuteLock
  *)
  Result := False;
  FTheDebugger.FCurrentStackFrame :=  0;

  List := TGDBMINameValueList.Create(AParams);
  try
    Reason := List.Values['reason'];
    if (Reason = 'exited-normally')
    then begin
      SetDebuggerState(dsStop);
      Exit;
    end;

    if Reason = 'exited'
    then begin
      FTheDebugger.SetExitCode(StrToIntDef(List.Values['exit-code'], 0));
      SetDebuggerState(dsStop);
      Exit;
    end;

    if Reason = 'exited-signalled'
    then begin
      SetDebuggerState(dsStop);
      FTheDebugger.DoException(deExternal, 'External: ' + List.Values['signal-name'], '', CanContinue);
      // ProcessFrame(List.Values['frame']);
      Exit;
    end;

    if Reason = 'signal-received'
    then begin
      ProcessSignalReceived(List);
      Exit;
    end;

    if Reason = 'breakpoint-hit'
    then begin
      BreakID := StrToIntDef(List.Values['bkptno'], -1);
      if BreakID = -1
      then begin
        SetDebuggerState(dsError);
        // ???
        Exit;
      end;

      if BreakID = FTheDebugger.FBreakErrorBreakID
      then begin
        SetDebuggerState(dsPause);
        ProcessBreak;
        Exit;
      end;

      if BreakID = FTheDebugger.FRunErrorBreakID
      then begin
        SetDebuggerState(dsPause);
        ProcessRunError;
        Exit;
      end;

      if BreakID = FTheDebugger.FExceptionBreakID
      then begin
        ExceptionInfo := GetExceptionInfo;

        // check if we should ignore this exception
        if FTheDebugger.Exceptions.IgnoreAll
        or (FTheDebugger.Exceptions.Find(ExceptionInfo.Name) <> nil)
        then begin
          //ExecuteCommand('-exec-continue')
          Result := True;
          exit;
        end
        else begin
          SetDebuggerState(dsPause);
          ProcessException(ExceptionInfo);
        end;
        Exit;
      end;

      BreakPoint := TGDBMIBreakPoint(FTheDebugger.FindBreakpoint(BreakID));
      if BreakPoint <> nil
      then begin
        CanContinue := False;
        BreakPoint.Hit(CanContinue);
        if CanContinue
        then begin
          //ExecuteCommand('-exec-continue');
          Result := True;
          exit;
        end
        else begin
          SetDebuggerState(dsPause);
          ProcessFrame(List.Values['frame']);
        end;
      end;
      // The temp-at-start breakpoint is not checked. Ignore it
      if (DebuggerState = dsRun) and (FTheDebugger.TargetPID <> 0) // not in startup
      then begin
        debugln(['********** WARNING: breakpoint hit, but nothing known about it BreakId=', BreakID, ' brbtno=', List.Values['bkptno'] ]);
        {$IFDEF DBG_VERBOSE_BRKPOINT}
        debugln(['-*- List of breakpoints Cnt=', FTheDebugger.Breakpoints.Count]);
        for BreakID := 0 to FTheDebugger.Breakpoints.Count - 1 do
          debugln(['* ',Dbgs(FTheDebugger.Breakpoints[BreakID]), ':', DbgsName(FTheDebugger.Breakpoints[BreakID]), ' BreakId=',TGDBMIBreakPoint(FTheDebugger.Breakpoints[BreakID]).FBreakID, ' Source=', FTheDebugger.Breakpoints[BreakID].Source, ' Line=', FTheDebugger.Breakpoints[BreakID].Line ]);
        debugln(['************************************************************************ ']);
        debugln(['************************************************************************ ']);
        debugln(['************************************************************************ ']);
        {$ENDIF}
        SetDebuggerState(dsPause);
        ProcessFrame(List.Values['frame']); // and jump to it
      end;
      Exit;
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
    List.Free;
  end;
end;

{$IFDEF MSWindows}
function TGDBMIDebuggerCommandExecute.FixThreadForSigTrap: Boolean;
var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
  S: string;
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
end;
{$ENDIF}

function TGDBMIDebuggerCommandExecute.DoExecute: Boolean;
var
  StoppedParams: String;
  ContinueExecution: Boolean;
  NextExecCmdObj: TGDBMIDebuggerCommandExecute;
begin
  Result := True;
  FCanKillNow := False;
  FDidKillNow := False;
  FNextExecQueued := False;
  //ContinueExecution := True;

  FTheDebugger.QueueExecuteLock; // prevent other commands from executing
  try
    if not ExecuteCommand(FCommand, FResult)
    then exit;

    if (FResult.State <> dsNone)
    then SetDebuggerState(FResult.State);
    // if ContinueExecution will be true, the we ignore dsError..

    // TODO: chack for cancelled

    StoppedParams := '';
    FCanKillNow := True;
    if FResult.State = dsRun
    then Result := ProcessRunning(StoppedParams);

  finally
    FCanKillNow := False;
    FTheDebugger.QueueExecuteUnlock; // allow other commands from executing
  end;

  if FDidKillNow
  then exit;

  ContinueExecution := False;
  if StoppedParams <> ''
  then ContinueExecution := ProcessStopped(StoppedParams, FTheDebugger.PauseWaitState = pwsInternal);
  if ContinueExecution
  then begin
    // - The "old" behaviour was to queue a new exec-continue
    //   Keep the old behaviour for now: eventually change this procedure "DoExecute" do run a loop, until no continuation is needed)
    // - Queue is unlockes, so nothing should be queued after the continuation cmd
    //   But make info available, if anything wants to queue
    FNextExecQueued := True;
    {$IFDEF DBGMI_QUEUE_DEBUG}
    DebugLn(['CommandExecute: Internal queuing -exec-continue (ContinueExecution = True)']);
    {$ENDIF}
    FTheDebugger.FPauseWaitState := pwsNone;
    NextExecCmdObj := TGDBMIDebuggerCommandExecute.Create(FTheDebugger, ectContinue);
    // Queue it, so we execute once this Cmd exits; do not execute recursive
    FTheDebugger.QueueExecuteLock;
    FTheDebugger.QueueCommand(NextExecCmdObj);
    FTheDebugger.QueueExecuteUnlock;
  end;

  if (StoppedParams <> '') and (not ContinueExecution) and (DebuggerState = dsRun) and (TargetInfo^.TargetPID <> 0) then begin
    debugln(['ERROR: Got stop params, but did not change FTheDebugger.state: ', StoppedParams]);
    //SetDebuggerState(dsError); // we cannot be running anymore
  end;
  if (StoppedParams = '') and (not ContinueExecution) and (DebuggerState = dsRun) and (TargetInfo^.TargetPID <> 0) then begin
    debugln(['ERROR: Got NO stop params at all, but was running']);
    //SetDebuggerState(dsError); // we cannot be running anymore
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
  FCanKillNow := False;
  FDidKillNow := False;;
  FNextExecQueued := False;
  FExecType := ExecType;
  case FExecType of
    ectContinue: FCommand := '-exec-continue';
    ectRun:      FCommand := '-exec-run';
    ectRunTo:    FCommand := Format('-exec-until %s:%d', Args);
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
  Result := ProcessRunning(StoppedParams);
  if StoppedParams <> ''
  then ProcessStopped(StoppedParams, FTheDebugger.PauseWaitState = pwsInternal);

  ExecuteCommand('kill');
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
  begin
    FRegistersToUpdate[n].Value := '';
    FRegistersToUpdate[n].Modified := False;
  end;

  ExecuteCommand('-data-list-register-values N', R);
  if R.State = dsError then Exit;

  ValList := TGDBMINameValueList.Create('');
  List := TGDBMINameValueList.Create(R, ['register-values']);
  for n := 0 to List.Count - 1 do
  begin
    Item := List.Items[n];
    ValList.Init(Item^.Name);
    idx := StrToIntDef(Unquote(ValList.Values['number']), -1);
    if idx < Low(FRegistersToUpdate) then Continue;
    if idx > High(FRegistersToUpdate) then Continue;

    FRegistersToUpdate[idx].Value := Unquote(ValList.Values['value']);
  end;
  FreeAndNil(List);
  FreeAndNil(ValList);

  ExecuteCommand('-data-list-changed-registers', [cfIgnoreError], R);
  if R.State = dsError then Exit;

  List := TGDBMINameValueList.Create(R, ['changed-registers']);
  for n := 0 to List.Count - 1 do
  begin
    idx := StrToIntDef(Unquote(List.GetString(n)), -1);
    if idx < Low(FRegistersToUpdate) then Continue;
    if idx > High(FRegistersToUpdate) then Continue;

    FRegistersToUpdate[idx].Modified := True;
  end;
  FreeAndNil(List);
end;

constructor TGDBMIDebuggerCommandRegisterValues.Create(AOwner: TGDBMIDebugger;
  RegistersToUpdate: TGDBMICpuRegisters);
begin
  inherited Create(AOwner);
  FRegistersToUpdate := RegistersToUpdate;
end;

function TGDBMIDebuggerCommandRegisterValues.DebugText: String;
begin
  Result := Format('%s: Reg-Cnt=%d', [ClassName, length(FRegistersToUpdate)]);
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
end;

function TGDBMIDebuggerCommandStackDepth.DebugText: String;
begin
  Result := Format('%s:', [ClassName]);
end;

{ TGDBMIDebuggerCommandStackFrames }

function TGDBMIDebuggerCommandStackFrames.DoExecute: Boolean;

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
        ADest[lvl-FIndex] := SubList;
      end
      else SubList.Free;
    end;
    ResultList.Free;
  end;

var
  endidx: Integer;
begin
  Result := True;
  endidx := FIndex + FCount - 1;
  SetLength(FArgs, FCount);
  PrepareArgs(FArgs, FIndex, endidx, '-stack-list-arguments 1 %d %d', 'stack-args', 'args');

  SetLength(FFrames, FCount);
  PrepareArgs(FFrames, FIndex, endidx, '-stack-list-frames %d %d', 'stack', '');
end;

constructor TGDBMIDebuggerCommandStackFrames.Create(AOwner: TGDBMIDebugger; AIndex,
  ACount: Integer);
begin
  inherited Create(AOwner);
  FIndex := AIndex;
  FCount := ACount;
end;

destructor TGDBMIDebuggerCommandStackFrames.Destroy;
  procedure FreeList(var AList: TGDBMINameValueListArray);
  var
    i : Integer;
  begin
    for i := low(AList) to high(AList) do
      AList[i].Free;
  end;

begin
  inherited Destroy;
  FreeList(FArgs);
  FreeList(FFrames);
end;

function TGDBMIDebuggerCommandStackFrames.DebugText: String;
begin
  Result := Format('%s: Index=%d Count=%d', [ClassName, FIndex, FCount]);
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
  if not (Debugger.State in [dsPause, dsRun]) then
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

{ TGDBMINameValueList }

constructor TGDBMINameValueList.Create(const AResultValues: String);
begin
  inherited Create;
  Init(AResultValues);
end;

constructor TGDBMINameValueList.Create(const AResultValues: TPCharWithLen);
begin
  inherited Create;
  Init(AResultValues);
end;

constructor TGDBMINameValueList.Create(const AResultValues: String; const APath: array of String);
begin
  inherited Create;
  Init(AResultValues);
  SetPath(APath);
end;

constructor TGDBMINameValueList.Create(AResult: TGDBMIExecResult);
begin
  inherited Create;
  Init(AResult.Values);
end;

constructor TGDBMINameValueList.Create(AResult: TGDBMIExecResult; const APath: array of String);
begin
  inherited Create;
  Init(AResult.Values);
  SetPath(APath);
end;

procedure TGDBMINameValueList.Delete(AIndex: Integer);
begin
  if AIndex < 0 then Exit;
  if AIndex >= FCount then Exit;
  Dec(FCount);
  Move(FIndex[AIndex + 1], FIndex[AIndex], SizeOf(FIndex[0]) * (FCount - AIndex));
end;

function TGDBMINameValueList.Find(const AName: string): PGDBMINameValue;
var
  n: Integer;
begin
  n := IndexOf(AName);
  if n < 0 then Exit(nil);
  Result := @FIndex[n];
end;

function TGDBMINameValueList.GetItem(const AIndex: Integer): PGDBMINameValue;
begin
  if AIndex < 0 then Exit(nil);
  if AIndex >= FCount then Exit(nil);
  Result := @FIndex[AIndex];
end;

function TGDBMINameValueList.GetString(const AIndex : Integer) : string;
var
  len: Integer;
  item: PGDBMINameValue;
begin
  Result := '';
  if (AIndex < 0) or (AIndex >= FCount) then Exit;
  item := @FIndex[AIndex];
  if item = nil then Exit;

  len := Item^.Name.Len;
  if Item^.Value.Ptr <> nil then begin
    if (Item^.Value.Ptr-1) = '"' then inc(len, 2);
    len := len + 1 + Item^.Value.Len;
  end;

  SetLength(Result, len);
  Move(Item^.Name.Ptr^, Result[1], len);
end;

function TGDBMINameValueList.GetValue(const AName: string): string;
var
  item: PGDBMINameValue;
begin
  Result := '';
  if FCount = 0 then Exit;
  item := Find(AName);
  if item = nil then Exit;

  SetLength(Result, Item^.Value.Len);
  Move(Item^.Value.Ptr^, Result[1], Item^.Value.Len);
end;

function TGDBMINameValueList.GetValuePtr(const AName: string): TPCharWithLen;
var
  item: PGDBMINameValue;
begin
  Result.Ptr := nil;
  Result.Len := 0;
  if FCount = 0 then Exit;
  item := Find(AName);
  if item = nil then Exit;

  Result := item^.Value;
end;

procedure TGDBMINameValueList.Init(AResultValues: PChar; ALength: Integer);

  function FindNextQuote(ACurPtr, AEndPtr: PChar): PChar;
  begin
    Result := ACurPtr;
    while Result <= AEndPtr do
    begin
      case Result^ of
        '\': Inc(Result, 2);
        '"': Break;
      else
        Inc(Result);
      end;
    end;
  end;

  function FindClosingBracket(ACurPtr, AEndPtr: PChar): PChar;
  var
    deep: Integer;
  begin
    deep := 1;
    Result := ACurPtr;

    while Result <= AEndPtr do
    begin
      case Result^ of
        '\': Inc(Result);
        '"': Result := FindNextQuote(Result + 1, AEndPtr);
        '[', '{': Inc(deep);
        ']', '}': begin
          Dec(deep);
          if deep = 0 then break;
        end;
      end;
      Inc(Result);
    end;
  end;

  procedure Add(AStartPtr, AEquPtr, AEndPtr: PChar);
  var
    Item: PGDBMINameValue;
  begin
    if AEndPtr <= AStartPtr then Exit;

    // check space
    if Length(FIndex) <= FCount
    then SetLength(FIndex, FCount + 16);

    Item := @FIndex[FCount];
    if AEquPtr < AStartPtr
    then begin
      // trim spaces
      if UseTrim then
      begin
        while (AStartPtr < AEndPtr) and (AStartPtr^ = #32) do
          inc(AStartPtr);
        while (AEndPtr > AStartPtr) and (AEndPtr^ = #32) do
          dec(AEndPtr);
      end;

      // only name, no value
      Item^.Name.Ptr := AStartPtr;
      Item^.Name.Len := PtrUInt(AEndPtr) - PtrUInt(AStartPtr) + 1;
      Item^.Value.Ptr := nil;
      Item^.Value.Len := 0;
    end
    else begin
      // trim surrounding spaces
      if UseTrim then
      begin
        while (AStartPtr < AEquPtr) and (AStartPtr^ = #32) do
          inc(AStartPtr);
        while (AEndPtr > AEquPtr) and (AEndPtr^ = #32) do
          dec(AEndPtr);
      end;

      Item^.Name.Ptr := AStartPtr;
      Item^.Name.Len := PtrUInt(AEquPtr) - PtrUInt(AStartPtr);

      // trim name spaces
      if UseTrim then
        while (Item^.Name.Len > 0) and (Item^.Name.Ptr[Item^.Name.Len - 1] = #32) do
          dec(Item^.Name.Len);

      if (AEquPtr < AEndPtr - 1) and (AEquPtr[1] = '"') and (AEndPtr^ = '"')
      then begin
        // strip surrounding "
        Item^.Value.Ptr := AEquPtr + 2;
        Item^.Value.Len := PtrUInt(AEndPtr) - PtrUInt(AEquPtr) - 2;
      end
      else begin
        Item^.Value.Ptr := AEquPtr + 1;
        Item^.Value.Len := PtrUInt(AEndPtr) - PtrUInt(AEquPtr)
      end;
      // trim value spaces
      if UseTrim then
        while (Item^.Value.Len > 0) and (Item^.Value.Ptr[0] = #32) do
        begin
          inc(Item^.Value.Ptr);
          dec(Item^.Value.Len);
        end;
    end;

    Inc(FCount);
  end;

var
  CurPtr, StartPtr, EquPtr, EndPtr: PChar;
begin
  // clear
  FCount := 0;

  if AResultValues = nil then Exit;
  if ALength <= 0 then Exit;
  EndPtr := AResultValues + ALength - 1;

  // strip surrounding '[]' OR '{}' first
  case AResultValues^ of
    '[': begin
      if EndPtr^ = ']'
      then begin
        Inc(AResultValues);
        Dec(EndPtr);
      end;
    end;
    '{': begin
      if EndPtr^ = '}'
      then begin
        Inc(AResultValues);
        Dec(EndPtr);
      end;
    end;
  end;

  StartPtr := AResultValues;
  CurPtr := AResultValues;
  EquPtr := nil;
  while CurPtr <= EndPtr do
  begin
    case CurPtr^ of
      '\': Inc(CurPtr); // skip escaped char
      '"': CurPtr := FindNextQuote(CurPtr + 1, EndPtr);
      '[',
      '{': CurPtr := FindClosingBracket(CurPtr + 1, EndPtr);
      '=': EquPtr := CurPtr;
      ',': begin
        Add(StartPtr, EquPtr, CurPtr - 1);
        Inc(CurPtr);
        StartPtr := CurPtr;
        Continue;
      end;
    end;
    Inc(CurPtr);
  end;
  if StartPtr <= EndPtr
  then Add(StartPtr, EquPtr, EndPtr);
end;

procedure TGDBMINameValueList.Init(const AResultValues: TPCharWithLen);
begin
  Init(AResultValues.Ptr, AResultValues.Len)
end;

procedure TGDBMINameValueList.Init(const AResultValues: String);
begin
  FText := AResultValues;
  Init(PChar(FText), Length(FText));
end;

procedure TGDBMINameValueList.SetPath(const APath: String);
begin
  SetPath([APath]);
end;

procedure TGDBMINameValueList.SetPath(const APath: array of String);
var
  i: integer;
  Item: PGDBMINameValue;
begin
  for i := low(APath) to High(APath) do
  begin
    item := Find(APath[i]);
    if item = nil
    then begin
      FCount := 0;
      Exit;
    end;
    Init(Item^.Value);
  end;
end;

function TGDBMINameValueList.IndexOf(const AName: string): Integer;
var
  len: Integer;
begin
  len := Length(AName);
  Result := 0;
  while Result < FCount do begin
    if (FIndex[Result].Name.Len = len)
    and (strlcomp(FIndex[Result].Name.Ptr, PChar(AName), len) = 0)
    then exit;
    inc(Result);
  end;
  Result := -1;
end;


{ =========================================================================== }
{ TGDBMIDebuggerProperties }
{ =========================================================================== }

constructor TGDBMIDebuggerProperties.Create;
begin
  FOverrideRTLCallingConvention := ccDefault;
  inherited;
end;


{ =========================================================================== }
{ TGDBMIDebugger }
{ =========================================================================== }

procedure TGDBMIDebugger.CallStackSetCurrent(AIndex: Integer);
begin
  if FCurrentStackFrame = AIndex then Exit;
  FCurrentStackFrame := AIndex;
  SelectStackFrame(FCurrentStackFrame);

  TGDBMICallstack(CallStack).CurrentChanged;
  TGDBMILocals(Locals).Changed;
  TGDBMIWatches(Watches).Changed;
end;

class function TGDBMIDebugger.Caption: String;
begin
  Result := 'GNU debugger (gdb)';
end;

function TGDBMIDebugger.ChangeFileName: Boolean;
  procedure ClearBreakpoint(var ABreakID: Integer);
  begin
    if ABreakID = -1 then Exit;
    ExecuteCommand('-break-delete %d', [ABreakID], [cfIgnoreError]);
    ABreakID := -1;
  end;
var
  S: String;
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
begin
  Result := False;

  //Cleanup our own breakpoints
  ClearBreakpoint(FExceptionBreakID);
  ClearBreakpoint(FBreakErrorBreakID);
  ClearBreakpoint(FRunErrorBreakID);


  S := ConvertToGDBPath(UTF8ToSys(FileName));
  if not ExecuteCommand('-file-exec-and-symbols %s', [S], [cfIgnoreError], R) then Exit;
  if  (R.State = dsError)
  and (FileName <> '')
  then begin
    List := TGDBMINameValueList.Create(R);
    MessageDlg('Debugger', Format('Failed to load file: %s', [DeleteEscapeChars((List.Values['msg']))]), mtError, [mbOK], 0);
    List.Free;
    SetState(dsStop);
    Exit;
  end;
  if not (inherited ChangeFileName) then Exit;
  if State = dsError then Exit;
  if FileName = ''
  then begin
    Result := True;
    Exit;
  end;

  if tfHasSymbols in FTargetInfo.TargetFlags
  then begin
    // Force setting language
    // Setting extensions dumps GDB (bug #508)
    if not ExecuteCommand('-gdb-set language pascal', []) then exit;
    if State=dsError then exit;
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
  Result:=true;
end;

constructor TGDBMIDebugger.Create(const AExternalDebugger: String);
begin
  FReleaseLock := 0;
  FBreakErrorBreakID := -1;
  FRunErrorBreakID := -1;
  FExceptionBreakID := -1;
  FCommandQueue := TList.Create;
  FTargetInfo.TargetPID := 0;
  FTargetInfo.TargetFlags := [];
  FDebuggerFlags := [];
  FSourceNames := TStringList.Create;
  FSourceNames.Sorted := True;
  FSourceNames.Duplicates := dupError;
  FSourceNames.CaseSensitive := False;
  FCommandQueueExecLock := 0;
  FRunQueueOnUnlock := False;

{$IFdef MSWindows}
  InitWin32;
{$ENDIF}

  inherited;
end;

function TGDBMIDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TDBGBreakPoints.Create(Self, TGDBMIBreakPoint);
end;

function TGDBMIDebugger.CreateCallStack: TDBGCallStack;
begin
  Result := TGDBMICallStack.Create(Self);
end;

function TGDBMIDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result := TGDBMIDisassembler.Create(Self);
end;

function TGDBMIDebugger.CreateLocals: TDBGLocals;
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

function TGDBMIDebugger.CreateWatches: TDBGWatches;
begin
  Result := TGDBMIWatches.Create(Self, TGDBMIWatch);
end;

destructor TGDBMIDebugger.Destroy;
begin
  LockRelease;
  inherited;
  ClearCommandQueue;
  FreeAndNil(FCommandQueue);
  ClearSourceInfo;
  FreeAndNil(FSourceNames);
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
    if State = dsRun then GDBPause(True);
    ExecuteCommand('-gdb-exit', []);
    inherited Done;
  finally
    UnlockRelease;
  end;
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
  if State in [dsStop, dsError]
  then begin
    ClearSourceInfo;
    FPauseWaitState := pwsNone;
  end;
  if (OldState = dsPause) and (State = dsRun)
  then begin
    FPauseWaitState := pwsNone;
    {$IFDEF MSWindows}
    FPauseRequestInThreadID := 0;
    {$ENDIF}
  end;

  inherited DoState(OldState);
end;

procedure TGDBMIDebugger.DoRelease;
begin
  SetState(dsDestroying);
  if FReleaseLock > 0
  then exit;

  inherited DoRelease;
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AFlags: TGDBMICmdFlags): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommandFull(ACommand, [], AFlags, nil, 0, R);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: PtrInt): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommandFull(ACommand, [], AFlags, ACallback, ATag, R);
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
  CommandObj.KeepFinished := True;
  QueueCommand(CommandObj);
  Result := CommandObj.State in [dcsExecuting, dcsFinished];
  if Result
  then
    AResult := CommandObj.Result;
  CommandObj.KeepFinished := False;
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
      Cmd := TGDBMIDebuggerCommand(FCommandQueue[0]);
      if (Cmd.QueueRunLevel >= 0) and (Cmd.QueueRunLevel < FInExecuteCount)
      then break;

      Inc(FInExecuteCount);

      FCommandQueue.Delete(0);
      {$IFDEF DBGMI_QUEUE_DEBUG}
      DebugLnEnter(['Executing (Recurse-Count=', FInExecuteCount-1, ') queued= ', FCommandQueue.Count, ' CmdPrior=', Cmd.Priority,' CmdMinRunLvl=', Cmd.QueueRunLevel, ' : "', Cmd.DebugText,'" State=',DBGStateNames[State],' PauseWaitState=',ord(FPauseWaitState) ]);
      {$ENDIF}
      // cmd may be canceled while executed => don't loose it while working with it
      Cmd.LockFree;
      NestedCurrentCmdTmp := FCurrentCommand;
      FCurrentCommand := Cmd;
      // excute, has it's own try-except block => so we don't have one here
      R := Cmd.Execute;
      Cmd.DoFinished;
      FCurrentCommand := NestedCurrentCmdTmp;
      Cmd.UnlockFree;
      {$IFDEF DBGMI_QUEUE_DEBUG}
      DebugLnExit('Exec done');
      {$ENDIF}

      Dec(FInExecuteCount);
      // Do not add code with callbacks outside "FInExecuteCount"
      // Otherwhise "LockCommandProcessing" will fail to continue the queue

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
end;

procedure TGDBMIDebugger.QueueCommand(const ACommand: TGDBMIDebuggerCommand; ForceQueue: Boolean = False);
var
  i, p: Integer;
  CanRunQueue: Boolean;
begin
  p := ACommand.Priority;
  i := 0;
  CanRunQueue := (FCommandQueue.Count = 0)
    or ( (FCommandQueue.Count > 0)
        and (TGDBMIDebuggerCommand(FCommandQueue[0]).QueueRunLevel >= 0)
        and (TGDBMIDebuggerCommand(FCommandQueue[0]).QueueRunLevel < FInExecuteCount)
       );

  if p > 0 then begin
    while (i < FCommandQueue.Count)
    and (TGDBMIDebuggerCommand(FCommandQueue[i]).Priority >= p)
    and ( (ForceQueue)
       or (TGDBMIDebuggerCommand(FCommandQueue[i]).QueueRunLevel < 0)
       or (TGDBMIDebuggerCommand(FCommandQueue[i]).QueueRunLevel >= FInExecuteCount)
        )
    do inc(i);
    FCommandQueue.Insert(i, ACommand);
  end
  else begin
    if (not ForceQueue) and (FCommandQueue.Count > 0)
    and CanRunQueue  // first item is deffered, so new item inserted can run
    then
      FCommandQueue.Insert(0, ACommand)
    else
      i := FCommandQueue.Add(ACommand);
  end;

  // if other commands do run the queue,
  // make sure this command only runs after the CurrentCommand finished
  if ForceQueue then
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
end;

class function TGDBMIDebugger.ExePaths: String;
begin
  Result := '/usr/bin/gdb;/usr/local/bin/gdb;/opt/fpc/gdb';
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
  CmdObj.KeepFinished := True;
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

  CmdObj.KeepFinished := False;
  FreeAndNil(NewEntryMap);
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

function TGDBMIDebugger.GDBEvaluate(const AExpression: String; var AResult: String; out ATypeInfo: TGDBType): Boolean;
var
  CommandObj: TGDBMIDebuggerCommandEvaluate;
begin
  CommandObj := TGDBMIDebuggerCommandEvaluate.Create(Self, AExpression, wdfDefault);
  CommandObj.KeepFinished := True;
  CommandObj.Priority := GDCMD_PRIOR_IMMEDIATE; // try run imediately
  QueueCommand(CommandObj);
  Result := CommandObj.State in [dcsExecuting, dcsFinished];
  AResult := CommandObj.TextValue;
  ATypeInfo := CommandObj.TypeInfo;
  CommandObj.KeepFinished := False;
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
end;

function TGDBMIDebugger.GDBJumpTo(const ASource: String; const ALine: Integer): Boolean;
begin
  Result := False;
end;

function TGDBMIDebugger.GDBPause(const AInternal: Boolean): Boolean;
begin
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

  // not supported yet
  // ExecuteCommand('-exec-abort');
  Result := ExecuteCommand('kill', [cfNoMiCommand], @GDBStopCallback, 0);
end;

procedure TGDBMIDebugger.GDBStopCallback(const AResult: TGDBMIExecResult; const ATag: PtrInt);
var
  R: TGDBMIExecResult;
begin
  // verify stop
  if not ExecuteCommand('info program', [], [cfNoMICommand], R) then Exit;

  if Pos('not being run', R.Values) > 0
  then SetState(dsStop);
end;

function TGDBMIDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcStepOut,
             dcStepOverInstr, dcStepIntoInstr, dcRunTo, dcJumpto,
             dcBreak, dcWatch, dcLocal, dcEvaluate, dcModify, dcEnvironment,
             dcSetStackFrame, dcDisassemble];
end;

function TGDBMIDebugger.GetTargetWidth: Byte;
begin
  Result := FTargetInfo.TargetPtrSize*8;
end;

procedure TGDBMIDebugger.Init;
  function ParseGDBVersionMI: Boolean;
  var
    R: TGDBMIExecResult;
    S: String;
    List: TGDBMINameValueList;
  begin
    if not ExecuteCommand('-gdb-version', [], [], R)
    then Exit(False);

    if R.Values = '' then Exit(False);

    List := TGDBMINameValueList.Create(R);

    FGDBVersion := List.Values['version'];
    S := List.Values['target'];

    FGDBCPU := GetPart('', '-', S);
    GetPart('-', '-', S); // strip vendor
    FGDBOS := GetPart(['-'], ['-', ''], S);

    List.Free;

    Result := FGDBVersion <> '';
  end;

  procedure ParseGDBVersion;
  var
    R: TGDBMIExecResult;
    S: String;
  begin
    FGDBVersion := '';
    FGDBOS := '';
    FGDBCPU := '';

    if not ExecuteCommand('-gdb-version', [], [cfNoMiCommand], R) // No MI since the output is no MI
    then Exit;

    S := GetPart(['configured as \"'], ['\"'], R.Values, False, False);
    if Pos('--target=', S) <> 0 then
      S := GetPart('--target=', '', S);
    FGDBCPU := GetPart('', '-', S);
    GetPart('-', '-', S); // strip vendor
    FGDBOS := GetPart('-', '-', S);

    FGDBVersion := GetPart(['('], [')'], R.Values, False, False);
    if FGDBVersion <> '' then Exit;

    FGDBVersion := GetPart(['gdb '], [#10, #13], R.Values, True, False);
    if FGDBVersion <> '' then Exit;
  end;

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
begin
  LockRelease;
  try
    FPauseWaitState := pwsNone;
    FInExecuteCount := 0;

    Options := '-silent -i mi -nx';

    if Length(TGDBMIDebuggerProperties(GetProperties).Debugger_Startup_Options) > 0
    then Options := Options + ' ' + TGDBMIDebuggerProperties(GetProperties).Debugger_Startup_Options;

    if CreateDebugProcess(Options)
    then begin
      if not ParseInitialization
      then begin
        SetState(dsError);
        Exit;
      end;

      ExecuteCommand('-gdb-set confirm off', []);
      // for win32, turn off a new console otherwise breaking gdb will fail
      // ignore the error on other platforms
      ExecuteCommand('-gdb-set new-console off', [cfIgnoreError]);

      if not ParseGDBVersionMI then ParseGDBVersion;
      CheckGDBVersion;

      inherited Init;
    end
    else begin
      if DebugProcess = nil
      then MessageDlg('Debugger', 'Failed to create debug process for unknown reason', mtError, [mbOK], 0)
      else MessageDlg('Debugger', Format('Failed to create debug process: %s', [ReadLine]), mtError, [mbOK], 0);
      SetState(dsError);
    end;

    FGDBPtrSize := CpuNameToPtrSize(FGDBCPU);
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
      dcEvaluate:    Result := GDBEvaluate(String(AParams[0].VAnsiString), String(AParams[1].VPointer^),TGDBType(AParams[2].VPointer^));
      dcModify:      Result := GDBModify(String(AParams[0].VAnsiString), String(AParams[1].VAnsiString));
      dcEnvironment: Result := GDBEnvironment(String(AParams[0].VAnsiString), AParams[1].VBoolean);
      dcDisassemble: Result := GDBDisassemble(AParams[0].VQWord^, AParams[1].VBoolean, TDbgPtr(AParams[2].VPointer^),
                                              String(AParams[3].VPointer^), String(AParams[4].VPointer^),
                                              String(AParams[5].VPointer^), Integer(AParams[6].VPointer^));
      dcStepOverInstr: Result := GDBStepOverInstr;
      dcStepIntoInstr: Result := GDBStepIntoInstr;
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
    TGDBMIDebuggerCommand(FCommandQueue[i]).Free;
  end;
  FCommandQueue.Clear;
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

procedure TGDBMIDebugger.SelectStackFrame(AIndex: Integer);
begin
  ExecuteCommand('-stack-select-frame %d', [AIndex], [cfIgnoreError]);
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
  Cmd.KeepFinished := True;
  QueueCommand(Cmd);
  Result := Cmd.Success;
  if not Result
  then Cmd.Cancel;
  Cmd.KeepFinished := False;
end;

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
  ExecuteCommand(ACommand, [cfIgnoreError]);
end;

{%region      *****  BreakPoints  *****  }

{ TGDBMIDebuggerCommandBreakPointBase }

function TGDBMIDebuggerCommandBreakPointBase.ExecBreakDelete(ABreakId: Integer): Boolean;
begin
  Result := False;
  if ABreakID = 0 then Exit;

  Result := ExecuteCommand('-break-delete %d', [ABreakID], []);
end;

function TGDBMIDebuggerCommandBreakPointBase.ExecBreakInsert(ASource: string; ALine: Integer;
  AEnabled: Boolean; out ABreakId, AHitCnt: Integer; out AnAddr: TDBGPtr): Boolean;
var
  R: TGDBMIExecResult;
  ResultList: TGDBMINameValueList;
begin
  Result := False;
  ABreakId := 0;
  AHitCnt := 0;
  AnAddr := 0;
  If (ASource = '') or (ALine < 0) then exit;

  if dfForceBreak in FTheDebugger.FDebuggerFlags
  then Result := ExecuteCommand('-break-insert -f %s:%d', [ExtractFileName(ASource), ALine], R)
  else Result := ExecuteCommand('-break-insert %s:%d',    [ExtractFileName(ASource), ALine], R);

  ResultList := TGDBMINameValueList.Create(R, ['bkpt']);
  ABreakID := StrToIntDef(ResultList.Values['number'], 0);
  AHitCnt  := StrToIntDef(ResultList.Values['times'], 0);
  AnAddr   := StrToQWordDef(ResultList.Values['addr'], 0);
  if ABreakID = 0
  then Result := False;
  ResultList.Free;
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

function TGDBMIDebuggerCommandBreakInsert.DoExecute: Boolean;
begin
  Result := True;
  FValid := False;

  if FReplaceId <> 0
  then ExecBreakDelete(FReplaceId);

  FValid := ExecBreakInsert(FSource, FLine, FEnabled, FBreakID, FHitCnt, FAddr);
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
end;

constructor TGDBMIDebuggerCommandBreakInsert.Create(AOwner: TGDBMIDebugger; ASource: string;
  ALine: Integer; AEnabled: Boolean; AnExpression: string; AReplaceId: Integer);
begin
  inherited Create(AOwner);
  FSource := ASource;
  FLine := ALine;
  FEnabled := AEnabled;
  FExpression := AnExpression;
  FReplaceId := AReplaceId;
end;

function TGDBMIDebuggerCommandBreakInsert.DebugText: String;
begin
  Result := Format('%s: Source=%s, Line=%d, Enabled=%s', [ClassName, FSource, FLine, dbgs(FEnabled)]);
end;

{ TGDBMIDebuggerCommandBreakRemove }

function TGDBMIDebuggerCommandBreakRemove.DoExecute: Boolean;
begin
  Result := True;
  ExecBreakDelete(FBreakId);
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
  if FUpdateExpression
  then ExecBreakCondition(FBreakID, FExpression);
  if FUpdateEnabled
  then ExecBreakEnabled(FBreakID, FEnabled);
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
      TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Source  := Source;
      TGDBMIDebuggerCommandBreakInsert(FCurrentCmd).Line    := Line;
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
  FCurrentCmd:= TGDBMIDebuggerCommandBreakInsert.Create(TGDBMIDebugger(Debugger),
    Source, Line , Enabled, FParsedExpression, FBreakID);
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
    BeginUpdate;
    if TGDBMIDebuggerCommandBreakInsert(Sender).Valid
    then SetValid(vsValid)
    else SetValid(vsInvalid);
    FBreakID := TGDBMIDebuggerCommandBreakInsert(Sender).BreakID;
    SetHitCount(TGDBMIDebuggerCommandBreakInsert(Sender).HitCnt);

    if Enabled
    and (TGDBMIDebugger(Debugger).FBreakAtMain = nil)
    then begin
      // Check if this BP is at the same location as the temp break
      if TGDBMIDebuggerCommandBreakInsert(Sender).Addr = TGDBMIDebugger(Debugger).FMainAddr
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
  if TGDBMIDebugger(Debugger).State in [dsPause, dsRun]
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

      FResult.Add(Name + '=' + Value);
    end;
    FreeAndNil(List);
    FreeAndNil(LocList);
  end;

var
  R: TGDBMIExecResult;
  List: TGDBMINameValueList;
begin
  Result := True;
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
end;

constructor TGDBMIDebuggerCommandLocals.Create(AOwner: TGDBMIDebugger);
begin
  inherited Create(AOwner);
  FResult := TStringList.Create;
end;

destructor TGDBMIDebuggerCommandLocals.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FResult);
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
  Invalidate;
  inherited Changed;
end;

constructor TGDBMILocals.Create(const ADebugger: TDebugger);
begin
  FLocals := TStringList.Create;
  FLocals.Sorted := True;
  FEvaluatedState := esInvalid;
  FEvaluationCmdObj := nil;
  inherited;
end;

destructor TGDBMILocals.Destroy;
begin
  CancelEvaluation;
  inherited;
  FreeAndNil(FLocals);
end;

procedure TGDBMILocals.DoStateChange(const AOldState: TDBGState);
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    DoChange;
  end
  else begin
    Invalidate;
  end;
end;

procedure TGDBMILocals.Invalidate;
begin
  FEvaluatedState := esInvalid;
  CancelEvaluation;
  FLocals.Clear;
end;

function TGDBMILocals.GetCount: Integer;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    LocalsNeeded;
    if FEvaluatedState = esValid
    then Result := FLocals.Count
    else Result := 0;
  end
  else Result := 0;
end;

function TGDBMILocals.GetName(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    LocalsNeeded;
    Result := FLocals.Names[AnIndex];
  end
  else Result := '';
end;

function TGDBMILocals.GetValue(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    LocalsNeeded;
    Result := FLocals[AnIndex];
    Result := GetPart('=', '', Result);
  end
  else Result := '';
end;

procedure TGDBMILocals.DoEvaluationDestroyed(Sender: TObject);
begin
  if FEvaluationCmdObj = Sender
  then FEvaluationCmdObj := nil;
end;

procedure TGDBMILocals.DoEvaluationFinished(Sender: TObject);
var
  Cmd: TGDBMIDebuggerCommandLocals;
begin
  FLocals.Clear;
  FEvaluatedState := esValid;
  FEvaluationCmdObj := nil;
  Cmd := TGDBMIDebuggerCommandLocals(Sender);
  FLocals.Assign(Cmd.Result);
  // Do not recursively call, whoever is requesting the locals
  if not FInLocalsNeeded
  then inherited Changed;
end;

procedure TGDBMILocals.LocalsNeeded;
var
  ForceQueue: Boolean;
begin
  if Debugger = nil then Exit;
  if FEvaluatedState in [esRequested, esValid] then Exit;

  FLocals.Clear;
  FInLocalsNeeded := True;
  FEvaluatedState := esRequested;
  FEvaluationCmdObj := TGDBMIDebuggerCommandLocals.Create(TGDBMIDebugger(Debugger));
  FEvaluationCmdObj.OnExecuted := @DoEvaluationFinished;
  FEvaluationCmdObj.OnDestroy   := @DoEvaluationDestroyed;
  FEvaluationCmdObj.Priority := GDCMD_PRIOR_LOCALS;
  FEvaluationCmdObj.Properties := [dcpCancelOnRun];
  ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
            and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
            and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued);
  TGDBMIDebugger(Debugger).QueueCommand(FEvaluationCmdObj, ForceQueue);
  (* DoEvaluationFinished may be called immediately at this point *)
  FInLocalsNeeded := False;
end;

procedure TGDBMILocals.CancelEvaluation;
begin
  FEvaluatedState := esInvalid;
  if FEvaluationCmdObj <> nil
  then begin
    FEvaluationCmdObj.OnExecuted := nil;;
    FEvaluationCmdObj.OnDestroy := nil;;
    FEvaluationCmdObj.Cancel;
  end;
  FEvaluationCmdObj := nil;
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
begin
  for n := Low(FRegisters) to High(FRegisters) do
  begin
    FRegisters[n].Value := '';
    FRegisters[n].Modified := False;
  end;
  FValuesReqState := esInvalid;
end;

function TGDBMIRegisters.GetCount: Integer;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then RegistersNeeded;

  Result := Length(FRegisters)
end;

function TGDBMIRegisters.GetModified(const AnIndex: Integer): Boolean;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then ValuesNeeded;

  if  (FValuesReqState = esValid)
  and (FRegistersReqState = esValid)
  and (AnIndex >= Low(FRegisters))
  and (AnIndex <= High(FRegisters))
  then Result := FRegisters[AnIndex].Modified
  else Result := False;
end;

function TGDBMIRegisters.GetName(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then RegistersNeeded;

  if  (FRegistersReqState = esValid)
  and (AnIndex >= Low(FRegisters))
  and (AnIndex <= High(FRegisters))
  then Result := FRegisters[AnIndex].Name
  else Result := '';
end;

function TGDBMIRegisters.GetValue(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then ValuesNeeded;

  if  (FValuesReqState = esValid)
  and (FRegistersReqState = esValid)
  and (AnIndex >= Low(FRegisters))
  and (AnIndex <= High(FRegisters))
  then Result := FRegisters[AnIndex].Value
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
begin
  Cmd := TGDBMIDebuggerCommandRegisterNames(Sender);

  SetLength(FRegisters, Cmd.Count);
  for n := 0 to Cmd.Count - 1 do
  begin
    FRegisters[n].Name := Cmd.Names[n];
    FRegisters[n].Value := '';
    FRegisters[n].Modified := False;
  end;

  FGetRegisterCmdObj:= nil;
  FRegistersReqState := esValid;

  if not FInRegistersNeeded
  then Changed;
end;

procedure TGDBMIRegisters.DoGetRegValuesDestroyed(Sender: TObject);
begin
  if FGetValuesCmdObj = Sender
  then FGetValuesCmdObj := nil;
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
    SetLength(FRegisters, 0);

    FGetRegisterCmdObj := TGDBMIDebuggerCommandRegisterNames.Create(TGDBMIDebugger(Debugger));
    FGetRegisterCmdObj.OnExecuted := @DoGetRegisterNamesFinished;
    FGetRegisterCmdObj.OnDestroy   := @DoGetRegisterNamesDestroyed;
    FGetRegisterCmdObj.Priority := GDCMD_PRIOR_LOCALS;
    FGetRegisterCmdObj.Properties := [dcpCancelOnRun];
    ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
              and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
              and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued);
    TGDBMIDebugger(Debugger).QueueCommand(FGetRegisterCmdObj, ForceQueue);
    (* DoEvaluationFinished may be called immediately at this point *)
    FInRegistersNeeded := False;
  end;
end;

procedure TGDBMIRegisters.DoGetRegValuesFinished(Sender: TObject);
begin
  FValuesReqState := esValid;
  FGetValuesCmdObj := nil;
  if not FInValuesNeeded
  then inherited Changed;
end;

procedure TGDBMIRegisters.ValuesNeeded;
var
  ForceQueue: Boolean;
begin
  if (Debugger = nil) or (FValuesReqState in [esRequested, esValid]) or (Count = 0)
  then Exit;

  if (Debugger.State in [dsPause, dsStop])
  then begin
    FInValuesNeeded := True;
    FValuesReqState := esRequested;

    FGetValuesCmdObj := TGDBMIDebuggerCommandRegisterValues.Create(TGDBMIDebugger(Debugger), FRegisters);
    FGetValuesCmdObj.OnExecuted := @DoGetRegValuesFinished;
    FGetValuesCmdObj.OnDestroy   := @DoGetRegValuesDestroyed;
    FGetValuesCmdObj.Priority := GDCMD_PRIOR_LOCALS;
    FGetValuesCmdObj.Properties := [dcpCancelOnRun];
    ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
              and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
              and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued);
    TGDBMIDebugger(Debugger).QueueCommand(FGetValuesCmdObj, ForceQueue);
    (* DoEvaluationFinished may be called immediately at this point *)
    FInValuesNeeded := False;
  end;
end;

{ =========================================================================== }
{ TGDBMIWatch }
{ =========================================================================== }

constructor TGDBMIWatch.Create(ACollection: TCollection);
begin
  FEvaluatedState := esInvalid;
  FEvaluationCmdObj := nil;
  inherited;
end;

destructor TGDBMIWatch.Destroy;
begin
  CancelEvaluation;
  FreeAndNil(FTypeInfo);
  inherited;
end;

procedure TGDBMIWatch.DoEnableChange;
begin
  inherited;
end;

procedure TGDBMIWatch.DoExpressionChange;
begin
  CancelEvaluation;
  inherited;
end;

procedure TGDBMIWatch.DoDisplayFormatChanged;
begin
  CancelEvaluation;
  inherited;
end;

procedure TGDBMIWatch.DoChange;
begin
  Changed;
end;

procedure TGDBMIWatch.DoStateChange(const AOldState: TDBGState);
begin
  if Debugger = nil then Exit;

  if Debugger.State in [dsPause, dsStop]
  then begin
    ClearOwned;
    CancelEvaluation;
    if (Debugger.State = dsPause) or (AOldState in [dsRun, dsPause])
    then Changed;
  end;
end;

procedure TGDBMIWatch.Invalidate;
begin
  ClearOwned;
  CancelEvaluation;
end;

procedure TGDBMIWatch.DoEvaluationFinished(Sender: TObject);
begin
  ClearOwned;
  FValue := TGDBMIDebuggerCommandEvaluate(Sender).TextValue;
  FTypeInfo := TGDBMIDebuggerCommandEvaluate(Sender).TypeInfo;
  FEvaluationCmdObj := nil;
  FEvaluatedState := esValid;
  // Do not recursively call, whoever is requesting the watch
  if not FInEvaluationNeeded
  then Changed;
end;

procedure TGDBMIWatch.DoEvaluationDestroyed(Sender: TObject);
begin
  if FEvaluationCmdObj = Sender
  then FEvaluationCmdObj := nil;
end;

procedure TGDBMIWatch.EvaluationNeeded;
var
  ForceQueue: Boolean;
begin
  if FEvaluatedState in [esValid, esRequested] then Exit;
  if Debugger = nil then Exit;

  if (Debugger.State = dsPause)
  and Enabled
  then begin
    BeginUpdate; // aviod change early trigger bt SetValid
    try
      FInEvaluationNeeded := True;
      FEvaluatedState := esRequested;
      ClearOwned;
      SetValid(vsValid);
      FEvaluationCmdObj := TGDBMIDebuggerCommandEvaluate.Create
        (TGDBMIDebugger(Debugger), Expression, DisplayFormat);
      FEvaluationCmdObj.OnExecuted := @DoEvaluationFinished;
      FEvaluationCmdObj.OnDestroy   := @DoEvaluationDestroyed;
      FEvaluationCmdObj.Properties := [dcpCancelOnRun];
      // If a ExecCmd is running, then defer exec until the exec cmd is done
      ForceQueue := (TGDBMIDebugger(Debugger).FCurrentCommand <> nil)
                and (TGDBMIDebugger(Debugger).FCurrentCommand is TGDBMIDebuggerCommandExecute)
                and (not TGDBMIDebuggerCommandExecute(TGDBMIDebugger(Debugger).FCurrentCommand).NextExecQueued);
      TGDBMIDebugger(Debugger).QueueCommand(FEvaluationCmdObj, ForceQueue);
      (* DoEvaluationFinished may be called immediately at this point *)
      FInEvaluationNeeded := False;
    finally
      EndUpdate;
    end;
  end
  else begin
    SetValid(vsUnknown);
  end;
end;

procedure TGDBMIWatch.CancelEvaluation;
begin
  FEvaluatedState := esInvalid;
  if FEvaluationCmdObj <> nil
  then begin
    FEvaluationCmdObj.OnExecuted := nil;
    FEvaluationCmdObj.OnDestroy := nil;
    FEvaluationCmdObj.Cancel;
  end;
  FEvaluationCmdObj := nil;
end;

procedure TGDBMIWatch.ClearOwned;
begin
  FreeAndNil(FTypeInfo);
  FValue:='';
end;

function TGDBMIWatch.GetValue: String;
begin
  if  (Debugger <> nil)
  and (Debugger.State in [dsPause])
  and Enabled
  then begin
    EvaluationNeeded;
    case FEvaluatedState of
      esInvalid:   Result := inherited GetValue;
      esRequested: Result := '<evaluating>';
      esValid:     Result := FValue;
    end;
  end
  else Result := inherited GetValue;
end;

function TGDBMIWatch.GetTypeInfo: TDBGType;
begin
  if  (Debugger <> nil)
  and (Debugger.State in [dsPause])
  and Enabled
  then begin
    EvaluationNeeded;
    Result := FTypeInfo;
  end
  else Result := inherited GetTypeInfo;
end;

function TGDBMIWatch.GetValid: TValidState;
begin
  EvaluationNeeded;
  Result := inherited GetValid;
end;

{ =========================================================================== }
{ TGDBMIWatches }
{ =========================================================================== }

procedure TGDBMIWatches.DoStateChange(const AOldState: TDBGState);
begin
  SetLength(FParentFPList, 0);
  inherited DoStateChange(AOldState);
end;

procedure TGDBMIWatches.Changed;
var
  n: Integer;
begin
  SetLength(FParentFPList, 0);
  for n := 0 to Count - 1 do
    TGDBMIWatch(Items[n]).Invalidate;
  inherited Changed;
end;



{ =========================================================================== }
{ TGDBMICallStack }
{ =========================================================================== }

procedure TGDBMICallStack.DoDepthCommandExecuted(Sender: TObject);
begin
  FDepthEvalCmdObj := nil;
  SetCount(TGDBMIDebuggerCommandStackDepth(Sender).Depth);
  if not FInEvalDepth
  then Changed;
end;

function TGDBMICallStack.CheckCount: Boolean;
begin
  Result := False; // no valid count available
  if (Debugger = nil) or (Debugger.State <> dsPause) or (FDepthEvalCmdObj <> nil)
  then exit;

  FInEvalDepth := True;
  FDepthEvalCmdObj := TGDBMIDebuggerCommandStackDepth.Create(TGDBMIDebugger(Debugger));
  FDepthEvalCmdObj.OnExecuted := @DoDepthCommandExecuted;
  FDepthEvalCmdObj.OnDestroy   := @DoDepthCommandDestroyed;
  FDepthEvalCmdObj.Priority := GDCMD_PRIOR_STACK;
  TGDBMIDebugger(Debugger).QueueCommand(FDepthEvalCmdObj);
  (* DoDepthCommandExecuted may be called immediately at this point *)
  FInEvalDepth := False;
  Result := FDepthEvalCmdObj = nil; // count is good
end;

procedure TGDBMICallStack.DoDepthCommandDestroyed(Sender: TObject);
begin
  if FDepthEvalCmdObj = Sender
  then FDepthEvalCmdObj := nil;
end;

procedure TGDBMICallStack.DoFramesCommandDestroyed(Sender: TObject);
begin
  if FFramesEvalCmdObj = Sender
  then FFramesEvalCmdObj := nil;
end;

function TGDBMICallStack.InternalCreateEntry(AIndex: Integer; AArgInfo, AFrameInfo : TGDBMINameValueList) : TCallStackEntry;
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

  Result := TCallStackEntry.Create(
    AIndex,
    addr,
    Arguments,
    func,
    filename,
    fullname,
    StrToIntDef(line, 0)
  );

  Arguments.Free;
end;

procedure TGDBMICallStack.Clear;
begin
  if FDepthEvalCmdObj <> nil
  then begin
    FDepthEvalCmdObj.OnExecuted := nil;
    FDepthEvalCmdObj.OnDestroy := nil;
    FDepthEvalCmdObj.Cancel;
  end;
  FDepthEvalCmdObj := nil;

  if FFramesEvalCmdObj <> nil
  then begin
    FFramesEvalCmdObj.OnExecuted := nil;
    FFramesEvalCmdObj.OnDestroy := nil;
    FFramesEvalCmdObj.Cancel;
  end;
  FFramesEvalCmdObj := nil;
  inherited Clear;
end;

function TGDBMICallStack.CreateStackEntry(AIndex: Integer): TCallStackEntry;
var
  R: TGDBMIExecResult;
  ArgList, FrameList: TGDBMINameValueList;
begin
  if Debugger = nil then Exit;
  {$IFDEF DBGMI_QUEUE_DEBUG}
  debugln('WARNING: TGDBMICallStack.CreateStackEntry called');
  {$ENDIF}

  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-arguments 1 %0:d %0:d',
                                          [AIndex], [cfIgnoreError], R);
  // TODO: check what to display on error

  if R.State <> dsError
  then ArgList := TGDBMINameValueList.Create(R, ['stack-args', 'frame', 'args'])
  else ArgList := nil;


  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-frames %0:d %0:d',
                                          [AIndex], [cfIgnoreError], R);

  if R.State <> dsError
  then FrameList := TGDBMINameValueList.Create(R, ['stack', 'frame'])
  else FrameList := nil;

  Result := InternalCreateEntry(AIndex, ArgList, FrameList);

  FreeAndNil(ArgList);
  FreeAndNil(FrameList);
end;

function TGDBMICallStack.GetCurrent: TCallStackEntry;
var
  idx: Integer;
begin
  idx := TGDBMIDebugger(Debugger).FCurrentStackFrame;
  if (idx < 0) or (idx >= Count)
  then Result := nil
  else Result := Entries[idx];
end;

procedure TGDBMICallStack.DoFramesCommandExecuted(Sender: TObject);
var
  i, idx, endidx: Integer;
  Cmd: TGDBMIDebuggerCommandStackFrames;
begin
  if FFramesEvalCmdObj = Sender
  then FFramesEvalCmdObj := nil;

  Cmd := TGDBMIDebuggerCommandStackFrames(Sender);
  endidx := Cmd.Index + Cmd.Count - 1;

  idx := 0;
  for i := Cmd.Index to endidx do
  begin
    InternalSetEntry(i, InternalCreateEntry(i, Cmd.Args[idx], Cmd.Frames[idx]));
    inc(idx);
  end;

  if not FInEvalFrames
  then Changed;
end;

procedure TGDBMICallStack.PrepareEntries(AIndex, ACount: Integer);
const MergeOffset = 3;
var
  i, idx: Integer;
begin
  if (Debugger = nil) or (Debugger.State <> dsPause) or (ACount <= 0) then Exit;

  // create temporary evaluating entries, prevent requesting same frames again
  idx := 0;
  for i := AIndex to AIndex + ACount - 1 do
  begin
    InternalSetEntry(i, TCallStackEntry.Create(i, 0, nil, '', '', '', 0, cseRequested));
    inc(idx);
  end;

  if FFramesEvalCmdObj <> nil then begin
    if FFramesEvalCmdObj.State = dcsQueued then begin
      if (AIndex < FFramesEvalCmdObj.Index) and
         (AIndex + ACount - 1 >= FFramesEvalCmdObj.Index - MergeOffset)
      then begin
        // merge before or replace
        debugln(['TGDBMICallStack.PrepareEntries MERGE BEFORE AIndex=', AIndex, ' ACount=', ACount, ' Cmd.Index=', FFramesEvalCmdObj.Index, ' Cmd.Count=', FFramesEvalCmdObj.Count,
          '  NewCount=', Max(AIndex + ACount, FFramesEvalCmdObj.Index + FFramesEvalCmdObj.Count) - AIndex ]);
        FFramesEvalCmdObj.Index := AIndex;
        FFramesEvalCmdObj.Count :=
          Max(AIndex + ACount, FFramesEvalCmdObj.Index + FFramesEvalCmdObj.Count) - AIndex;
        exit;
      end
      else
      if (AIndex + ACount <= FFramesEvalCmdObj.Index + FFramesEvalCmdObj.Count + MergeOffset)
      then begin
        // after or inside
        debugln(['TGDBMICallStack.PrepareEntries MERGE AFTER AIndex=', AIndex, ' ACount=', ACount, ' Cmd.Index=', FFramesEvalCmdObj.Index, ' Cmd.Count=', FFramesEvalCmdObj.Count,
          ' NewCount=', Max(AIndex + ACount, FFramesEvalCmdObj.Index + FFramesEvalCmdObj.Count) - FFramesEvalCmdObj.Index ]);
        FFramesEvalCmdObj.Count :=
          Max(AIndex + ACount, FFramesEvalCmdObj.Index + FFramesEvalCmdObj.Count)
          - FFramesEvalCmdObj.Index;
        exit;
      end;
    end;
  end;


  FInEvalFrames := True;
  // Todo: keep the old reference too, so it can be canceled
  FFramesEvalCmdObj := TGDBMIDebuggerCommandStackFrames.Create(TGDBMIDebugger(Debugger), AIndex, ACount);
  FFramesEvalCmdObj.OnExecuted := @DoFramesCommandExecuted;
  FFramesEvalCmdObj.OnDestroy  := @DoFramesCommandDestroyed;
  FFramesEvalCmdObj.Priority := GDCMD_PRIOR_STACK;
  TGDBMIDebugger(Debugger).QueueCommand(FFramesEvalCmdObj);
  (* DoFramesCommandExecuted may be called immediately at this point *)
  FInEvalFrames := False;
end;

procedure TGDBMICallStack.SetCurrent(AValue: TCallStackEntry);
begin
  TGDBMIDebugger(Debugger).CallStackSetCurrent(AValue.Index);
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
const
  OPERATOR_TEXT: array[TDBGExpressionOperator] of string = (
    'eoNone',
    'eoNegate',
    'eoPlus',
    'eoSubstract',
    'eoAdd',
    'eoMultiply',
    'eoPower',
    'eoDivide',
    'eoDereference',
    'eoAddress',
    'eoEqual',
    'eoLess',
    'eoLessOrEqual',
    'eoGreater',
    'eoGreaterOrEqual',
    'eoNotEqual',
    'eoIn',
    'eoIs',
    'eoAs',
    'eoDot',
    'eoComma',
    'eoBracket',
    'eoIndex',
    'eoClose',
    'eoAnd',
    'eoOr',
    'eoMod',
    'eoNot',
    'eoDiv',
    'eoXor',
    'eoShl',
    'eoShr'
  );

var
  Sub: PGDBMISubExpression;
begin
  Result := '';
  Sub := FList;
  while Sub <> nil do
  begin
    Result := Result + Sub^.Operand + ' ' +  OPERATOR_TEXT[Sub^.Opertor] + ' ';
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

procedure TGDBMIDebuggerCommand.SetKeepFinished(const AValue: Boolean);
begin
  if FKeepFinished = AValue then exit;
  FKeepFinished := AValue;
  if (not FKeepFinished) and FFreeRequested
  then DoFree;
end;

function TGDBMIDebuggerCommand.GetDebuggerState: TDBGState;
begin
  Result := FTheDebugger.State;
end;

function TGDBMIDebuggerCommand.GetTargetInfo: PGDBMITargetInfo;
begin
  Result := @FTheDebugger.FTargetInfo;
end;

procedure TGDBMIDebuggerCommand.SetDebuggerState(const AValue: TDBGState);
begin
  FTheDebugger.SetState(AValue);
end;

procedure TGDBMIDebuggerCommand.SetState(NewState: TGDBMIDebuggerCommandState);
var
  OldState: TGDBMIDebuggerCommandState;
begin
  if FState = NewState
  then exit;
  OldState := FState;
  FState := NewState;
  Include(FSeenStates, NewState);
  DoStateChanged(OldState);
  if State in [dcsFinished, dcsCanceled]
  then DoFree;
end;

procedure TGDBMIDebuggerCommand.DoStateChanged(OldState: TGDBMIDebuggerCommandState);
begin
  // nothing
end;

procedure TGDBMIDebuggerCommand.DoFree;
begin
  FFreeRequested := True;
  if KeepFinished or (FFreeLock > 0)
  then exit;
  Self.Free;
end;

procedure TGDBMIDebuggerCommand.LockFree;
begin
  inc(FFreeLock);
end;

procedure TGDBMIDebuggerCommand.UnlockFree;
begin
  dec(FFreeLock);
  if (FFreeLock = 0) and FFreeRequested
  then DoFree;
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
var
  R: TGDBMIExecResult;
begin
  AResult.Values := '';
  AResult.State := dsNone;
  AResult.Flags := [];

  if (ATimeOut = -1) and (DefaultTimeOut > 0)
  then ATimeOut := DefaultTimeOut;

  FTheDebugger.SendCmdLn(ACommand);

  Result := ProcessResult(AResult, ATimeOut);
  FLastExecResult := AResult;

  if ProcessResultTimedOut then begin
    FTheDebugger.SendCmdLn('-data-evaluate-expression 1');
    Result := ProcessResult(AResult, Min(ATimeOut, 1000));

    if ProcessResultTimedOut then begin
      // still timed out
      Result := False; // => dsError
    end
    else begin
      MessageDlg('Warning', 'A timeout occured, the debugger will try to continue, but further error may occur later',
             mtWarning, [mbOK], 0);

      ProcessResult(R, 500); // catch the 2nd <gdb> prompt, if indeed any
      if ProcessResultTimedOut then
        AResult.State := dsError;
    end;
  end;

  if not Result
  then begin
    // either gdb did not return a Result Record: "^xxxx,"
    // or the Result Record was not a known one: 'done', 'running', 'exit', 'error'
    DebugLn('[WARNING] TGDBMIDebugger:  ExecuteCommand "',ACommand,'" failed.');
    SetDebuggerState(dsError);
  end;

  if (cfCheckError in AFlags) and (AResult.State = dsError)
  then SetDebuggerState(AResult.State);

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

function TGDBMIDebuggerCommand.ProcessResult(var AResult: TGDBMIExecResult;ATimeOut: Integer = -1): Boolean;

  function DoResultRecord(Line: String): Boolean;
  var
    ResultClass: String;
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
        // todo implement with values
        if  (pos('msg=', Line) > 0)
        and (pos('not being run', Line) > 0)
        then AResult.State := dsStop
        else AResult.State := dsError;
      end;
    else
      //TODO: should that better be dsError ?
      Result := False;
      DebugLn('[WARNING] Debugger: Unknown result class: ', ResultClass);
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
      DoDbgEvent(ecDebugger, Format('File ''%s'' has no debug symbols', [FTheDebugger.FileName]));
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
  begin
    DebugLn('[Debugger] Log output: ', Line);
    if Line = '&"kill\n"'
    then AResult.State := dsStop
    else if LeftStr(Line, 8) = '&"Error '
    then AResult.State := dsError;
  end;

  procedure DoExecAsync(Line: String);
  var
    EventText: String;
  begin
    EventText := GetPart(['*'], [','], Line, False, False);
    if EventText = 'running'
    then
      DoDbgEvent(ecProcess, Line)
    else
      DebugLn('[WARNING] Debugger: Unexpected async-record: ', Line);
  end;

  procedure DoStatusAsync(const Line: String);
  begin
    DebugLn('[WARNING] Debugger: Unexpected async-record: ', Line);
  end;

  procedure DoNotifyAsync(Line: String);
  var
    EventText: String;
  begin
    EventText := GetPart(['='], [','], Line, False, False);
    case StringCase(EventText, ['library-unloaded',
      'thread-created', 'thread-group-created',
      'thread-exited', 'thread-group-exited'], False, False)
    of
      0: DoDbgEvent(ecModule, Line);
      1..4: DoDbgEvent(ecThread, Line);
    else
      DebugLn('[WARNING] Debugger: Unexpected async-record: ', Line);
    end;
  end;

var
  S: String;
begin
  Result := False;
  FProcessResultTimedOut := False;
  AResult.Values := '';
  AResult.Flags := [];
  AResult.State := dsNone;
  repeat
    S := FTheDebugger.ReadLine(ATimeOut);
    if S = '(gdb) ' then Break;

    if s <> ''
    then case S[1] of
      '^': Result := DoResultRecord(S);
      '~': DoConsoleStream(S);
      '@': DoTargetStream(S);
      '&': DoLogStream(S);
      '*': DoExecAsync(S);
      '+': DoStatusAsync(S);
      '=': DoNotifyAsync(S);
    else
      DebugLn('[WARNING] Debugger: Unknown record: ', S);
    end;
    {$IFDEF VerboseIDEToDo}{$message warning condition should also check end-of-file reached for process output stream}{$ENDIF}
    if FTheDebugger.ReadLineTimedOut
    then begin
      FProcessResultTimedOut := True;
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
  if not ExecuteCommand('x/s ' + AExpression, AValues, R {$IFDEF DBG_WITH_TIMEOUT}, [], 2500{$ENDIF})
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

function TGDBMIDebuggerCommand.GetGDBTypeInfo(const AExpression: String): TGDBType;
var
  R: TGDBMIExecResult;
  f: Boolean;
  AReq: PGDBPTypeRequest;
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

  Result := TGdbType.CreateForExpression(AExpression, tfClassIsPointer in TargetInfo^.TargetFlags);
  while not Result.ProcessExpression do begin
    if Result.EvalError
    then break;
    AReq := Result.EvalRequest;
    while AReq <> nil do begin;
      f :=  ExecuteCommand(AReq^.Request, R);
      if f and (R.State <> dsError) then
        AReq^.Result := ParseTypeFromGdb(R.Values)
      else
        AReq^.Error := R.Values;
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
begin
  Result := '';

  if dfImplicidTypes in FTheDebugger.DebuggerFlags
  then begin
    S := Format(AExpression, AValues);
    if tfFlagHasTypeShortstring in TargetInfo^.TargetFlags
    then s := Format('^^shortstring(%s+%d)^^', [S, TargetInfo^.TargetPtrSize * 3])
    else s := Format('^^char(%s+%d)^+1', [S, TargetInfo^.TargetPtrSize * 3]);
    OK :=  ExecuteCommand('-data-evaluate-expression %s',
          [S], R);
    if (not OK) or (LastExecResult.State = dsError)
    then OK :=  ExecuteCommand('-data-evaluate-expression ^char(^pointer(%s+%d)^+1)',
          [S, TargetInfo^.TargetPtrSize * 3], R);
  end
  else begin
    Str(TDbgPtr(GetData(AExpression + '+12', AValues)), S);
    OK := ExecuteCommand('-data-evaluate-expression pshortstring(%s)^', [S], R);
  end;

  if OK
  then begin
    ResultList := TGDBMINameValueList.Create(R);
    S := DeleteEscapeChars(ResultList.Values['value']);
    Result := GetPart('''', '''', S);
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
  if not ExecuteCommand('ptype '+TypeName, Result) then begin
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

procedure TGDBMIDebuggerCommand.ProcessFrame(const AFrame: String);
var
  S: String;
  e: Integer;
  Frame: TGDBMINameValueList;
  Location: TDBGLocationRec;
begin
  // Do we have a frame ?
  if AFrame = ''
  then S := GetFrame(0)
  else S := AFrame;

  Frame := TGDBMINameValueList.Create(S);

  Location.Address := 0;
  Val(Frame.Values['addr'], Location.Address, e);
  if e=0 then ;
  Location.FuncName := Frame.Values['func'];
  Location.SrcFile := ConvertGdbPathAndFile(Frame.Values['file']);
  Location.SrcFullName := ConvertGdbPathAndFile(Frame.Values['fullname']);
  Location.SrcLine := StrToIntDef(Frame.Values['line'], -1);

  Frame.Free;

  FTheDebugger.DoCurrent(Location);
end;

procedure TGDBMIDebuggerCommand.DoDbgEvent(const ACategory: TDBGEventCategory;
  const AText: String);
begin
  FTheDebugger.DoDbgEvent(ACategory, AText);
end;

constructor TGDBMIDebuggerCommand.Create(AOwner: TGDBMIDebugger);
begin
  FQueueRunLevel := -1;
  FState := dcsNone;
  FTheDebugger := AOwner;
  FKeepFinished := False;
  FFreeRequested := False;
  FDefaultTimeOut := -1;
  FFreeLock := 0;
  FPriority := 0;
  FProperties := [];
end;

destructor TGDBMIDebuggerCommand.Destroy;
begin
  if assigned(FOnDestroy)
  then FOnDestroy(Self);
  inherited Destroy;
end;

procedure TGDBMIDebuggerCommand.DoQueued;
begin
  SetState(dcsQueued);
end;

procedure TGDBMIDebuggerCommand.DoFinished;
begin
  SetState(dcsFinished);
end;

function TGDBMIDebuggerCommand.Execute: Boolean;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  // Set the state first, so DoExecute can set an error-state
  SetState(dcsExecuting);
  LockFree;
  DoLockQueueExecute;
  try
    Result := DoExecute;
    DoOnExecuted;
  except
    on e: Exception do begin
      try
      debugln(['ERROR: Exception occured in DoExecute '+e.ClassName + ' Msg="'+ e.Message + '" Addr=', dbgs(ExceptAddr)]);
      Report :=  BackTraceStrFunc(ExceptAddr);
      Frames := ExceptFrames;
      for I := 0 to ExceptFrameCount - 1 do
        Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
      except end;
      debugln(Report);

      if MessageDlg('The debugger experienced an unknown condition.',
        Format('Press "Ignore" to continue debugging. This may NOT be save. Press "Abort to stop the debugger. %s'
          +'Exception: %s.with message "%s"',
        [LineEnding, e.ClassName, e.Message]),
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
  UnlockFree;
end;

procedure TGDBMIDebuggerCommand.Cancel;
begin
  FTheDebugger.UnQueueCommand(Self);
  DoCancel;
  DoOnCanceled;
  SetState(dcsCanceled);
end;

function TGDBMIDebuggerCommand.DebugText: String;
begin
  Result := ClassName;
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
  FTypeInfoAutoDestroy := False;
end;

function TGDBMIDebuggerCommandEvaluate.DoExecute: Boolean;

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
//      skEnum: ;
//      skSet: ;
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
    i, aFrame: Integer;
  begin
    if aFrameIdx < Length(TGDBMIWatches(FTheDebugger.Watches).FParentFPList) then begin
      aFrame := TGDBMIWatches(FTheDebugger.Watches).FParentFPList[aFrameIdx];
      if aFrame = -1
      then exit(False);

      inc(aFrameIdx);
      if not ExecuteCommand('-stack-select-frame %u', [aFrame], R)
      or (R.State = dsError)
      then
        Exit(False);
      Exit(True);
    end;

    i := length(TGDBMIWatches(FTheDebugger.Watches).FParentFPList);
    SetLength(TGDBMIWatches(FTheDebugger.Watches).FParentFPList, i + 1);
    TGDBMIWatches(FTheDebugger.Watches).FParentFPList[i] := -1; // assume failure
    inc(aFrameIdx);

    if i > 0
    then aFrame := TGDBMIWatches(FTheDebugger.Watches).FParentFPList[i-1]
    else aFrame := TGDBMIDebugger(FTheDebugger).FCurrentStackFrame;

    if not ExecuteCommand('-data-evaluate-expression parentfp', R)
    or (R.State = dsError)
    then Exit(False);

    List := TGDBMINameValueList.Create(R);
    ParentFP := List.Values['value'];
    repeat
      if not ExecuteCommand('-stack-select-frame %u', [aFrame+1], R)
      or (R.State = dsError)
      then begin
        List.Free;
        Exit(False);
      end;

      Inc(AFrame);

      if not ExecuteCommand('-data-evaluate-expression $fp', R)
      or (R.State = dsError)
      then begin
        List.Free;
        Exit(False);
      end;
      List.Init(R.Values);
      Fp := List.Values['value'];

    until ParentFP = Fp;
    List.Free;

    TGDBMIWatches(FTheDebugger.Watches).FParentFPList[i] := aFrame;
    Result := True;
  end;

  function PascalizePointer(AString: String; const TypeCast: String = ''): String;
  var
    s: String;
  begin
    // there may be data after the pointer
    s := GetPart([], [' '], AString, False, True);
    if IsHexC(s)
    then begin
      if s = '0x0'
      then begin
        Result := 'nil';
      end
      else begin
        // 0xabc0 => $0000ABC0
        Result := UpperCase(HexCToHexPascal(s, FTheDebugger.TargetWidth div 4));
      end;
    end
    else Result := s;
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
      ResultInfo := GetGDBTypeInfo(AnExpression);
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

  function TryExecute(AnExpression: string; StoreError: Boolean): Boolean;

    function PrepareExpr(var expr: string; NoAddressOp: Boolean = False): boolean;
    var
      ResultInfo: TGDBType;
    begin
      FTypeInfo := GetGDBTypeInfo(expr);
      Result := FTypeInfo <> nil;
      if (not Result) and StoreError
      then FTextValue := '<error>';
      if not Result
      then exit;

      if NoAddressOp
      then expr := QuoteExpr(expr)
      else expr := QuoteExpr(AddAddressOfToExpression(expr, FTypeInfo));
    end;

  var
    ResultList: TGDBMINameValueList;
    R: TGDBMIExecResult;
    MemDump: TGDBMIMemoryDumpResultList;
    Size: integer;
  begin
    Result := False;

    {$IFDEF DBG_WITH_TIMEOUT}
    DefaultTimeOut := 1500;
    try
    {$ENDIF}
    case FDisplayFormat of
      wdfStructure:
        begin
          Result := ExecuteCommand('-data-evaluate-expression %s', [AnExpression], R);
          Result := Result and (R.State <> dsError);
          if (not Result) and (not StoreError)
          then exit;

          ResultList := TGDBMINameValueList.Create(R.Values);
          if Result
          then FTextValue := ResultList.Values['value']
          else FTextValue := ResultList.Values['msg'];
          FTextValue := DeleteEscapeChars(FTextValue);
          ResultList.Free;

          if Result
          then FixUpResult(AnExpression);
        end;
      wdfChar:
        begin
          Result := PrepareExpr(AnExpression);
          if not Result
          then exit;
          FTextValue := GetChar(AnExpression, []);
          if LastExecResult.State = dsError
          then FTextValue := '<error>';
        end;
      wdfString:
        begin
          Result := PrepareExpr(AnExpression);
          if not Result
          then exit;
          FTextValue := GetText(AnExpression, []); // GetText takes Addr
          if LastExecResult.State = dsError
          then FTextValue := '<error>';
        end;
      wdfDecimal:
        begin
          Result := PrepareExpr(AnExpression, True);
          if not Result
          then exit;
          FTextValue := IntToStr(Int64(GetPtrValue(AnExpression, [], True)));
          if LastExecResult.State = dsError
          then FTextValue := '<error>';
        end;
      wdfUnsigned:
        begin
          Result := PrepareExpr(AnExpression, True);
          if not Result
          then exit;
          FTextValue := IntToStr(GetPtrValue(AnExpression, [], True));
          if LastExecResult.State = dsError
          then FTextValue := '<error>';
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
          if length(FTextValue) mod 2 = 1
          then FTextValue := '0'+FTextValue; // make it an even number of digets
          if LastExecResult.State = dsError
          then FTextValue := '<error>';
        end;
      wdfPointer:
        begin
          Result := PrepareExpr(AnExpression, True);
          if not Result
          then exit;
          FTextValue := PascalizePointer('0x' + IntToHex(GetPtrValue(AnExpression, [], True), TargetInfo^.TargetPtrSize*2));
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
          MemDump := TGDBMIMemoryDumpResultList.Create(R);
          FTextValue := MemDump.AsText(0, MemDump.Count, TargetInfo^.TargetPtrSize*2);
          MemDump.Free;
        end;
      else // wdfDefault
        begin
          Result := False;
          FTypeInfo := GetGDBTypeInfo(AnExpression);
          if FTypeInfo = nil
          then begin
            ResultList := TGDBMINameValueList.Create(LastExecResult.Values);
            FTextValue := ResultList.Values['msg'];
            FreeAndNil(ResultList);
            exit;
          end;

          if (dcsCanceled in SeenStates)
          then exit;

          if (saInternalPointer in FTypeInfo.Attributes)
          then begin
            Result := ExecuteCommand('-data-evaluate-expression %s%s', [AnExpression, '^'], R);
            Result := Result and (R.State <> dsError);
          end;

          if (not Result)
          and (saRefParam in FTypeInfo.Attributes) and (FTypeInfo.InternalTypeName <> '')
          then begin
            Result := ExecuteCommand('-data-evaluate-expression %s(%s)', [FTypeInfo.InternalTypeName, AnExpression], R);
            Result := Result and (R.State <> dsError);
          end;

          if (not Result)
          then Result := ExecuteCommand('-data-evaluate-expression %s', [AnExpression], R);
          Result := Result and (R.State <> dsError);

          if (not Result) and (not StoreError)
          then exit;

          ResultList := TGDBMINameValueList.Create(R.Values);
          if Result
          then FTextValue := ResultList.Values['value']
          else FTextValue := ResultList.Values['msg'];
          FTextValue := DeleteEscapeChars(FTextValue);
          ResultList.Free;

          if Result
          then FixUpResult(AnExpression, FTypeInfo);
        end;
    end;
    {$IFDEF DBG_WITH_TIMEOUT}
    finally
      DefaultTimeOut := -1;
    end
    {$ENDIF}

  end;

var
  S: String;
  ResultList: TGDBMINameValueList;
  Expr: TGDBMIExpression;
  frame, frameidx: Integer;
begin
  FTextValue:='';
  FTypeInfo:=nil;

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
  // original
  frame := TGDBMIDebugger(FTheDebugger).FCurrentStackFrame;
  frameidx := 0;
  try
    repeat
      if TryExecute(S, frame = -1)
      then Break;
      FreeAndNil(FTypeInfo);
      if (dcsCanceled in SeenStates)
      then break;
    until not SelectParentFrame(frameidx);

  finally
    if frameidx <> 0
    then begin
      // Restore current frame
      ExecuteCommand('-stack-select-frame %u', [frame], []);
    end;
    FreeAndNil(ResultList);
  end;
  Result := True;
end;

constructor TGDBMIDebuggerCommandEvaluate.Create(AOwner: TGDBMIDebugger;
  const AExpression: String; const ADisplayFormat: TWatchDisplayFormat);
begin
  inherited Create(AOwner);
  FExpression := AExpression;
  FDisplayFormat := ADisplayFormat;
  FTextValue := '';
  FTypeInfo:=nil;
  FTypeInfoAutoDestroy := True;
end;

destructor TGDBMIDebuggerCommandEvaluate.Destroy;
begin
  if FTypeInfoAutoDestroy
  then FreeAndNil(FTypeInfo);
  inherited Destroy;
end;

function TGDBMIDebuggerCommandEvaluate.DebugText: String;
begin
  Result := Format('%s: %s', [ClassName, FExpression]);
end;

initialization
  RegisterDebugger(TGDBMIDebugger);

end.

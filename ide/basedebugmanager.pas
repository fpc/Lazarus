{  $Id$  }
{
 /***************************************************************************
                         basedebugmanager.pp
                         -------------------
 TBaseDebugManager is the base class for TDebugManager, which controls all
 debugging related stuff in the IDE. The base class is mostly abstract.


 ***************************************************************************/

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
unit BaseDebugManager;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, Forms, Project, SourceMarks, Debugger, ProjectDefs,
  Laz_XMLCfg, IDEOptionsIntf, LazarusIDEStrConsts;

type
  TDebugDialogType = (
    ddtOutput,
    ddtEvents,
    ddtBreakpoints,
    ddtWatches,
    ddtLocals,
    ddtCallStack,
    ddtEvaluate,
    ddtRegisters,
    ddtAssembler,
    ddtInspect,
    ddtPseudoTerminal,
    ddtThreads,
    ddtHistory
    );

const
  // used as ID for layout storage.
  // Do not change. Do not translate
  DebugDialogNames: array [TDebugDialogType] of string = (
    'DbgOutput',
    'DbgEvents',
    'BreakPoints',
    'Watches',
    'Locals',
    'CallStack',
    'EvaluateModify',
    'Registers',
    'Assembler',
    'Inspect',
    'PseudoTerminal',
    'Threads',
    'DbgHistory'
    );

type

  { TBaseDebugManager }
  
  TDebugManagerState = (
    dmsInitializingDebuggerObject,
    dmsInitializingDebuggerObjectFailed,
    dmsDebuggerObjectBroken,  // the debugger entered the error state
    dmsWaitForRun, // waiting for call to RunDebugger, set by StartDebugging
    dmsRunning  // set by RunDebugger
    );
  TDebugManagerStates = set of TDebugManagerState;

  { TDebuggerOptions }

  TDebuggerOptions = class(TAbstractIDEEnvironmentOptions)
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
  end;

  TBaseDebugManager = class(TComponent)
  private
    function GetDebuggerClass(const AIndex: Integer): TDebuggerClass;
  protected
    FDestroying: boolean;
    FCallStack: TCallStackMonitor;
    FDisassembler: TIDEDisassembler;
    FExceptions: TIDEExceptions;
    FSignals: TIDESignals;
    FBreakPoints: TIDEBreakPoints;
    FBreakPointGroups: TIDEBreakPointGroups;
    FLocals: TLocalsMonitor;
    FLineInfo: TIDELineInfo;
    FWatches: TWatchesMonitor;
    FThreads: TThreadsMonitor;
    FRegisters: TIDERegisters;
    FSnapshots: TSnapshotManager;
    FManagerStates: TDebugManagerStates;
    function  FindDebuggerClass(const Astring: String): TDebuggerClass;
    function  GetState: TDBGState; virtual; abstract;
    function  GetCommands: TDBGCommands; virtual; abstract;
    {$IFDEF DBG_WITH_DEBUGGER_DEBUG}
    function GetDebugger: TDebugger; virtual; abstract;
    {$ENDIF}
  public
    procedure Reset; virtual; abstract;

    procedure ConnectMainBarEvents; virtual; abstract;
    procedure ConnectSourceNotebookEvents; virtual; abstract;
    procedure SetupMainBarShortCuts; virtual; abstract;
    procedure SetupSourceMenuShortCuts; virtual; abstract;
    procedure UpdateButtonsAndMenuItems; virtual; abstract;

    procedure LoadProjectSpecificInfo(XMLConfig: TXMLConfig;
                                      Merge: boolean); virtual; abstract;
    procedure SaveProjectSpecificInfo(XMLConfig: TXMLConfig;
                                      Flags: TProjectWriteFlags); virtual; abstract;

    function  DebuggerCount: Integer;

    procedure DoRestoreDebuggerMarks(AnUnitInfo: TUnitInfo); virtual; abstract;

    function InitDebugger: Boolean; virtual; abstract;
    
    function DoPauseProject: TModalResult; virtual; abstract;
    function DoShowExecutionPoint: TModalResult; virtual; abstract;
    function DoStepIntoProject: TModalResult; virtual; abstract;
    function DoStepOverProject: TModalResult; virtual; abstract;
    function DoStepOutProject: TModalResult; virtual; abstract;
    function DoStepIntoInstrProject: TModalResult; virtual; abstract;
    function DoStepOverInstrProject: TModalResult; virtual; abstract;
    function DoRunToCursor: TModalResult; virtual; abstract;
    function DoStopProject: TModalResult; virtual; abstract;
    procedure DoToggleCallStack; virtual; abstract;
    procedure DoSendConsoleInput(AText: String); virtual; abstract;
    procedure ProcessCommand(Command: word; var Handled: boolean); virtual; abstract;

    procedure LockCommandProcessing; virtual; abstract;
    procedure UnLockCommandProcessing; virtual; abstract;

    function StartDebugging: TModalResult; virtual; abstract; // set ToolStatus to itDebugger, but do not run debugger yet
    function RunDebugger: TModalResult; virtual; abstract; // run program, wait until program ends
    procedure EndDebugging; virtual; abstract;

    function Evaluate(const AExpression: String; var AResult: String;
                      var ATypeInfo: TDBGType;
                      EvalFlags: TDBGEvaluateFlags = []): Boolean; virtual; abstract; // Evaluates the given expression, returns true if valid
    function Modify(const AExpression: String; const ANewValue: String): Boolean; virtual; abstract; // Modify the given expression, returns true if valid

    function GetFullFilename(const AUnitinfo: TDebuggerUnitInfo;
                             out Filename: string; AskUserIfNotFound: Boolean): Boolean; virtual; abstract;
    function GetFullFilename(var Filename: string; AskUserIfNotFound: Boolean): Boolean; virtual; abstract;

    procedure EvaluateModify(const AExpression: String); virtual; abstract;
    procedure Inspect(const AExpression: String); virtual; abstract;

    function DoCreateBreakPoint(const AFilename: string; ALine: integer;
                                WarnIfNoDebugger: boolean): TModalResult; virtual; abstract;
    function DoCreateBreakPoint(const AFilename: string; ALine: integer;
                                WarnIfNoDebugger: boolean;
                                out ABrkPoint: TIDEBreakPoint): TModalResult; virtual; abstract;
    function DoCreateBreakPoint(const AnAddr: TDBGPtr;
                                WarnIfNoDebugger: boolean;
                                out ABrkPoint: TIDEBreakPoint): TModalResult; virtual; abstract;
    function DoDeleteBreakPoint(const AFilename: string; ALine: integer
                                ): TModalResult; virtual; abstract;
    function DoDeleteBreakPointAtMark(const ASourceMark: TSourceMark
                                     ): TModalResult; virtual; abstract;

    function ShowBreakPointProperties(const ABreakpoint: TIDEBreakPoint): TModalresult; virtual; abstract;
    function ShowWatchProperties(const AWatch: TCurrentWatch; AWatchExpression: String = ''): TModalresult; virtual; abstract;

    // Dialog routines
    procedure CreateDebugDialog(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean); virtual;
    procedure ViewDebugDialog(const ADialogType: TDebugDialogType;
                              BringToFront: Boolean = True; Show: Boolean = true;
                              DoDisableAutoSizing: boolean = false); virtual; abstract;
    procedure ViewDisassembler(AnAddr: TDBGPtr;
                              BringToFront: Boolean = True; Show: Boolean = true;
                              DoDisableAutoSizing: boolean = false); virtual; abstract;
  public
    property Commands: TDBGCommands read GetCommands;  // All current available commands of the debugger
    property Debuggers[const AIndex: Integer]: TDebuggerClass read GetDebuggerClass;
    property Destroying: boolean read FDestroying;
    property State: TDBGState read GetState;           // The current state of the debugger

    property BreakPoints: TIDEBreakPoints read FBreakpoints;   // A list of breakpoints for the current project
    property BreakPointGroups: TIDEBreakPointGroups read FBreakPointGroups;
    property Exceptions: TIDEExceptions read FExceptions;      // A list of exceptions we should ignore
    property CallStack: TCallStackMonitor read FCallStack;
    property Disassembler: TIDEDisassembler read FDisassembler;
    property Locals: TLocalsMonitor read FLocals;
    property LineInfo: TIDELineInfo read FLineInfo;
    property Registers: TIDERegisters read FRegisters;
    property Signals: TIDESignals read FSignals;               // A list of actions for signals we know of
    property Watches: TWatchesMonitor read FWatches;
    property Threads: TThreadsMonitor read FThreads;
    property Snapshots: TSnapshotManager read FSnapshots;
    {$IFDEF DBG_WITH_DEBUGGER_DEBUG}
    property Debugger: TDebugger read GetDebugger;
    {$ENDIF}
  end;

procedure RegisterDebugger(const ADebuggerClass: TDebuggerClass);

var
  DebugBoss: TBaseDebugManager;
  DebuggerOptions: TDebuggerOptions = nil;

implementation

var
  MDebuggerClasses: TStringList;

procedure RegisterDebugger(const ADebuggerClass: TDebuggerClass);
begin
  MDebuggerClasses.AddObject(ADebuggerClass.ClassName, TObject(Pointer(ADebuggerClass)));
end;


{ TBaseDebugManager }

function TBaseDebugManager.DebuggerCount: Integer;
begin
  Result := MDebuggerClasses.Count;
end;

function TBaseDebugManager.FindDebuggerClass(const AString: String): TDebuggerClass;
var
  idx: Integer;
begin
  idx := MDebuggerClasses.IndexOf(AString);
  if idx = -1
  then Result := nil
  else Result := TDebuggerClass(MDebuggerClasses.Objects[idx]);
end;

procedure TBaseDebugManager.CreateDebugDialog(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  //
end;

function TBaseDebugManager.GetDebuggerClass(const AIndex: Integer): TDebuggerClass;
begin
  Result := TDebuggerClass(MDebuggerClasses.Objects[AIndex]);
end;

{ TDebuggerOptions }

class function TDebuggerOptions.GetGroupCaption: string;
begin
  Result := dlgGroupDebugger;
end;

class function TDebuggerOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := DebuggerOptions;
end;

initialization
  RegisterIDEOptionsGroup(GroupDebugger, TDebuggerOptions);
  DebugBoss := nil;
  MDebuggerClasses := TStringList.Create;
  MDebuggerClasses.Sorted := True;
  MDebuggerClasses.Duplicates := dupError;
  
finalization
  FreeAndNil(MDebuggerClasses);
  
end.




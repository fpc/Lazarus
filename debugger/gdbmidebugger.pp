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

interface

uses
  Classes, Process, SysUtils, Dialogs, DBGUtils, Debugger, CmdLineDebugger,
  GDBTypeInfo;

type
  TGDBMIProgramInfo = record
    State: TDBGState;
    BreakPoint: Integer; // ID of Breakpoint hit
    Signal: Integer;     // Signal no if we hit one
    SignalText: String;  // Signal text if we hit one
  end;

  TGDBMIDebuggerState = (
    gdbmisWaitingForKill
    );
  TGDBMIDebuggerStates = set of TGDBMIDebuggerState;


  TGDBMICmdFlags = set of (cfNoMiCommand, cfIgnoreState, cfIgnoreError);
  TGDBMICallback = procedure(var AResultState: TDBGState; var AResultValues: String; const ATag: Integer) of object;

  { TGDBMIDebugger }

  TGDBMIDebugger = class(TCmdLineDebugger)
  private
    FCommandQueue: TStringList;
    FHasSymbols: Boolean;
    FTargetPID: Integer;
    FBreakErrorBreakID: Integer;
    FExceptionBreakID: Integer;
    fGDBMIStates: TGDBMIDebuggerStates;
    FVersion: String;
    function  FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
    function  GDBEvaluate(const AExpression: String; var AResult: String): Boolean;
    function  GDBGetText(const ALocation: Pointer): String; overload;
    function  GDBGetText(const AExpression: String; AValues: array of const): String; overload;
    function  GDBGetData(const ALocation: Pointer): Pointer; overload;
    function  GDBGetData(const AExpression: String; AValues: array of const): Pointer; overload;
    function  GDBRun: Boolean;
    function  GDBPause: Boolean;
    function  GDBStart(const AContinueCommand: String): Boolean;
    function  GDBStop: Boolean;
    function  GDBStop2: Boolean;
    function  GDBStepOver: Boolean;
    function  GDBStepInto: Boolean;
    function  GDBRunTo(const ASource: String; const ALine: Integer): Boolean;
    function  GDBJumpTo(const ASource: String; const ALine: Integer): Boolean;
    function  ProcessResult(var ANewState: TDBGState; var AResultValues: String; const ANoMICommand: Boolean): Boolean;
    function  ProcessRunning(var AStoppedParams: String): Boolean;
    function  ProcessStopped(const AParams: String): Boolean;
    function  ExecuteCommand(const ACommand: String; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; var AResultValues: String; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; var AResultValues: String; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; var AResultState: TDBGState; var AResultValues: String; const AFlags: TGDBMICmdFlags): Boolean; overload;
//    function  ExecuteCommand(const ACommand: String; AValues: array of const; {const AIgnoreError: Boolean;} var AResultState: TDBGState; var AResultValues: String; const ANoMICommand: Boolean): Boolean; overload;
    function  GetGDBTypeInfo(const AExpression: String): TGDBType;
  protected
    function  ChangeFileName: Boolean; override;
    function  CreateBreakPoints: TDBGBreakPoints; override;
    function  CreateLocals: TDBGLocals; override;
    function  CreateCallStack: TDBGCallStack; override;
    function  CreateWatches: TDBGWatches; override;
    procedure DoState(const OldState: TDBGState); override;
    function  GetSupportedCommands: TDBGCommands; override;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
  public
    constructor Create(const AExternalDebugger: String); {override;}
    destructor Destroy; override;

    procedure Init; override;         // Initializes external debugger
    procedure Done; override;         // Kills external debugger

    // internal testing
    procedure TestCmd(const ACommand: String); override;
  end;


implementation

type
  TGDBMIBreakPoints = class(TDBGBreakPoints)
  private
  protected
    procedure DoDebuggerStateChange; override;
    procedure SetBreakPoints(ResetAll: boolean);
    procedure InitTargetStart; override;
  public
  end;


  TGDBMIBreakPoint = class(TDBGBreakPoint)
  private
    FBreakID: Integer;
    procedure SetBreakPoint;
    procedure ReleaseBreakPoint;
    procedure UpdateEnable;
    procedure UpdateExpression;
  protected
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure InitTargetStart; override;
    procedure SetLocation(const ASource: String; const ALine: Integer); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Hit(var ACanContinue: Boolean);
  end;

  TGDBMILocals = class(TDBGLocals)
  private
    FLocals: TStringList;
    FLocalsValid: Boolean;
    procedure LocalsNeeded;
    procedure AddLocals(const AParams:String);
  protected
    procedure DoStateChange; override;
    function GetName(const AnIndex: Integer): String; override;
    function GetValue(const AnIndex: Integer): String; override;
  public
    function Count: Integer; override;
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
  end;

  TGDBMIWatch = class(TDBGWatch)
  private
    FEvaluated: Boolean;
    FValue: String;
    procedure EvaluationNeeded;
  protected
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoStateChange; override;
    function  GetValue: String; override;
    function  GetValid: TValidState; override;
  public
    constructor Create(ACollection: TCollection); override;
  end;
  
  TGDBMICallStack = class(TDBGCallStack)
  private
    FCount: Integer;  // -1 means uninitialized
  protected
    function CreateStackEntry(const AIndex: Integer): TDBGCallStackEntry; override; 
    procedure DoStateChange; override;
    function GetCount: Integer; override;
  public
    constructor Create(const ADebugger: TDebugger); 
  end;

  TGDBMIExpression = class(TObject)
  private
    FDebugger: TGDBMIDebugger; 
    FOperator: String;
    FLeft: TGDBMIExpression;
    FRight: TGDBMIExpression;
    procedure CreateSubExpression(const AExpression: String);
  protected
  public
    constructor Create(const ADebugger: TGDBMIDebugger; const AExpression: String);
    destructor Destroy; override;
    function DumpExpression: String;
    function GetExpression(var AResult: String): Boolean;
  end;
  
  PGDBMICmdInfo = ^TGDBMICmdInfo;
  TGDBMICmdInfo = record
    Flags: TGDBMICmdFlags;
    CallBack: TGDBMICallback;
  end;


function CreateMIValueList(AResultValues: String): TStringList;
var
  n: Integer;
  InString: Boolean;
  InList: Integer;
  c: Char;
begin
  Result := TStringList.Create;
  if AResultValues = '' then Exit;
  // strip surrounding '[]' and '{}' first
  case AResultValues[1] of
    '[': begin
      if AResultValues[Length(AResultValues)] = ']'
      then begin
        Delete(AResultValues, Length(AResultValues), 1);
        Delete(AResultValues, 1, 1);
      end;
    end;
    '{': begin
      if AResultValues[Length(AResultValues)] = '}'
      then begin
        Delete(AResultValues, Length(AResultValues), 1);
        Delete(AResultValues, 1, 1);
      end;
    end;
  end;

  n := 1;
  InString := False;
  InList := 0;
  c := #0;
  while (n <= Length(AResultValues)) do
  begin
    if c = '\'
    then begin
      // previous char was escape char
      c := #0;
      Inc(n);
      Continue;
    end;
    c := AResultValues[n];
    if c = '\'
    then begin
      Delete(AResultValues, n, 1);
      Continue;
    end;

    if InString
    then begin
      if  c = '"'
      then begin
        InString := False;
        Delete(AResultValues, n, 1);
        Continue;
      end;
    end
    else begin
      if InList > 0
      then begin
        if c in [']', '}']
        then Dec(InList);
      end
      else begin
        if c = ','
        then begin
          Result.Add(Copy(AResultValues, 1, n - 1));
          Delete(AResultValues, 1, n);
          n := 1;
          Continue;
        end
        else if c = '"'
        then begin
          InString := True;
          Delete(AResultValues, n, 1);
          Continue;
        end;
      end;
      if c in ['[', '{']
      then Inc(InList);
    end;
    Inc(n);
  end;
  if AResultValues <> ''
  then Result.Add(AResultValues);
end;

function CreateValueList(AResultValues: String): TStringList;
var
  n: Integer;
begin
  Result := TStringList.Create;
  if AResultValues = '' then Exit;
  n := Pos(' = ', AResultValues);
  if n > 0
  then begin
    Delete(AResultValues, n, 1);
    Delete(AResultValues, n + 1, 1);
  end;
  Result.Add(AResultValues);
end;


{ =========================================================================== }
{ TGDBMIDebugger }
{ =========================================================================== }

function TGDBMIDebugger.ChangeFileName: Boolean;
//var
//  S: String;
begin
  FHasSymbols := True; // True until proven otherwise
  Result := ExecuteCommand('-file-exec-and-symbols %s', [FileName], [])
            and inherited ChangeFileName;

  if Result and FHasSymbols
  then begin
    // Force setting language
    // Setting extensions dumps GDB (bug #508)
    ExecuteCommand('-gdb-set language pascal', []);
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

constructor TGDBMIDebugger.Create(const AExternalDebugger: String);
begin
  FBreakErrorBreakID := -1;
  FExceptionBreakID := -1;
  FCommandQueue := TStringList.Create;
  FTargetPID := 0;
  inherited;
end;

function TGDBMIDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TGDBMIBreakPoints.Create(Self, TGDBMIBreakPoint);
end;

function TGDBMIDebugger.CreateCallStack: TDBGCallStack; 
begin
  Result := TGDBMICallStack.Create(Self);
end;

function TGDBMIDebugger.CreateLocals: TDBGLocals;
begin
  Result := TGDBMILocals.Create(Self);
end;

function TGDBMIDebugger.CreateWatches: TDBGWatches;
begin
  Result := TDBGWatches.Create(Self, TGDBMIWatch);
end;

procedure TGDBMIDebugger.DoState(const OldState: TDBGState);
begin
  writeln('TGDBMIDebugger.DoState State=',DBGStateNames[State],' OldState=',DBGStateNames[OldState],' ',gdbmisWaitingForKill in fGDBMIStates);
  // check if state change should sent a notification or is internal
  if gdbmisWaitingForKill in fGDBMIStates then begin
    // we are waiting for the debugger to kill the process
    // -> hide the temporary dsPause state
    if State=dsPause then begin
      writeln('[TGDBMIDebugger.DoState] Hiding dsPause while waiting for kill');
      exit;
    end;
    Exclude(fGDBMIStates,gdbmisWaitingForKill);
  end;
  // send a notification
  inherited DoState(OldState);
end;

destructor TGDBMIDebugger.Destroy;
begin
  inherited;
  FreeAndNil(FCommandQueue);
end;

procedure TGDBMIDebugger.Done;
begin
  if State = dsRun then GDBPause;
  ExecuteCommand('-gdb-exit', []);
  inherited Done;
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AFlags: TGDBMICmdFlags): Boolean;
var
  S: String;
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, [], ResultState, S, AFlags);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  var AResultValues: String; const AFlags: TGDBMICmdFlags): Boolean;
var
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, [], ResultState, AResultValues, AFlags);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  AValues: array of const; const AFlags: TGDBMICmdFlags): Boolean;
var
  S: String;
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, AValues, ResultState, S, AFlags);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  AValues: array of const; var AResultValues: String;
  const AFlags: TGDBMICmdFlags): Boolean;
var
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, AValues, ResultState, AResultValues, AFlags);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  AValues: array of const; var AResultState: TDBGState;
  var AResultValues: String; const AFlags: TGDBMICmdFlags): Boolean;
var
  S: String;
  CmdInfo: PGDBMICmdInfo;
  R: Boolean;
  StoppedParams: String;
  //IsKill: Boolean;
begin
  Result := False; // Assume queued
  AResultValues := '';
  AResultState := dsNone;
  New(CmdInfo);
  CmdInfo^.Flags := AFlags;
  CmdInfo^.Callback := nil;
  FCommandQueue.AddObject(ACommand, TObject(CmdInfo));
  if FCommandQueue.Count > 1 then Exit;
  Result := True; // If we are here we can process the command directly
  repeat
    S := FCommandQueue[0];
    CmdInfo := PGDBMICmdInfo(FCommandQueue.Objects[0]);
    // Kill is a special case, since it requires additional
    // processing after the command is executed. Until we have
    // added a callback meganism, we handle it here
    //IsKill := S = 'kill';
    
    SendCmdLn(S, AValues);
    R := ProcessResult(AResultState, AResultValues, cfNoMICommand in CmdInfo^.Flags);
    if R
    then begin
      if (AResultState <> dsNone)
      and not (cfIgnoreState in CmdInfo^.Flags)
      and ((AResultState <> dsError) or not (cfIgnoreError in CmdInfo^.Flags))
      then SetState(AResultState);

      StoppedParams := '';
      if AResultState = dsRun
      then R := ProcessRunning(StoppedParams);
    end
    else begin
      writeln('WARNING: TGDBMIDebugger.ExecuteCommand Command="',ACommand,'" failed.');
    end;
    Dispose(CmdInfo);
    FCommandQueue.Delete(0);
    if StoppedParams <> ''
    then ProcessStopped(StoppedParams);
    //if IsKill
    //then GDBStop2;
  until not R or (FCommandQueue.Count = 0);
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

function TGDBMIDebugger.GDBEvaluate(const AExpression: String;
  var AResult: String): Boolean;
var
  ResultState: TDBGState;
  S, ResultValues: String;
  ResultList: TStringList;
  Expression: TGDBMIExpression;
begin
  Expression := TGDBMIExpression.Create(Self, AExpression);
  if not Expression.GetExpression(S)
  then S := AExpression;
  WriteLN('[GDBEval] AskExpr: ', AExpression, ' EvalExp:', S ,' Dump: ',
          Expression.DumpExpression);
  Expression.Free;
  
  Result := ExecuteCommand('-data-evaluate-expression %s', [S], ResultState,
                           ResultValues, [cfIgnoreError])
            and (ResultState <> dsError);

  ResultList := CreateMIValueList(ResultValues);
  if ResultState = dsError
  then AResult := ResultList.Values['msg']
  else AResult := ResultList.Values['value'];
  ResultList.Free;
end;

function TGDBMIDebugger.GDBGetData(const ALocation: Pointer): Pointer;
begin
  Result := GDBGetData('%u', [Integer(ALocation)]);
end;

function TGDBMIDebugger.GDBGetData(const AExpression: String;
  AValues: array of const): Pointer;
var
  S: String;
begin
  if not ExecuteCommand('x/d ' + AExpression, AValues, S, [cfNoMICommand])
  then Result := nil
  else Result := Pointer(StrToIntDef(StripLN(GetPart('\t', '', S)), 0));
end;

function TGDBMIDebugger.GDBGetText(const ALocation: Pointer): String;
begin
  Result := GDBGetText('%d', [Integer(ALocation)]);
end;

function TGDBMIDebugger.GDBGetText(const AExpression: String;
  AValues: array of const): String;
var
  S: String;
begin
  if not ExecuteCommand('x/s ' + AExpression, AValues, S, [cfNoMICommand])
  then begin
    Result := '';
  end
  else begin
    S := StripLN(S);
    // don't use ' as end terminator, there might be one as part of the text
    // since ' will be the last char, simply strip it.
    Result := GetPart(['\t '''], [], S);
    Delete(Result, Length(Result), 1);
  end;
end;

function TGDBMIDebugger.GDBJumpTo(const ASource: String;
  const ALine: Integer): Boolean;
begin
  Result := False;
end;

function TGDBMIDebugger.GDBPause: Boolean;
begin
  SendBreak(FTargetPID);
  Result := True;
end;

function TGDBMIDebugger.GDBRun: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := GDBStart('-exec-continue');
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-continue', []);
    end;
    dsIdle: begin
      WriteLN('[WARNING] Debugger: Unable to run in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBRunTo(const ASource: String;
  const ALine: Integer): Boolean;
begin
  case State of
    dsIdle, dsStop: begin
      Result := GDBStart(Format('-exec-until %s:%d', [ASource, ALine]));
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-until %s:%d', [ASource, ALine], []);
    end;
  else
    Result := False;
  end;

end;

function TGDBMIDebugger.GDBStart(const AContinueCommand: String): Boolean;
var
  S: String;
  ResultState: TDBGState;
  ResultList, BkptList: TStringList;
  TargetPIDPart: String;
begin
  Exclude(FGDBMIStates,gdbmisWaitingForKill);
  if State in [dsStop]
  then begin
    if WorkingDir <> ''
    then ExecuteCommand('-environment-cd %s', [WorkingDir], []);
  
    if FHasSymbols
    then begin
      // Maske sure we are talking pascal
      ExecuteCommand('-gdb-set language pascal', []);
      if Arguments <>''
      then ExecuteCommand('-exec-arguments %s', [Arguments], []);
      ExecuteCommand('-break-insert -t main', []);
      ExecuteCommand('-exec-run', []);

      // Insert Exception breakpoint
      if FExceptionBreakID = -1
      then begin
        ExecuteCommand('-break-insert FPC_RAISEEXCEPTION', [],  ResultState, S, [cfIgnoreError]);
        ResultList := CreateMIValueList(S);
        BkptList := CreateMIValueList(ResultList.Values['bkpt']);
        FExceptionBreakID := StrToIntDef(BkptList.Values['number'], -1);
        ResultList.Free;
        BkptList.Free;
      end;

      // Insert Break breakpoint
      if FBreakErrorBreakID = -1
      then begin
        ExecuteCommand('-break-insert FPC_BREAK_ERROR', [], ResultState, S, [cfIgnoreError]);
        ResultList := CreateMIValueList(S);
        BkptList := CreateMIValueList(ResultList.Values['bkpt']);
        FBreakErrorBreakID := StrToIntDef(BkptList.Values['number'], -1);
        ResultList.Free;
        BkptList.Free;
      end;

      // try to find PID
      if ExecuteCommand('info program', [], ResultState, S, [cfIgnoreError, cfNoMICommand])
      then begin
         TargetPIDPart:=GetPart('child process ', '.', S);
         if TargetPIDPart='' then
           TargetPIDPart:=GetPart('child Thread ', ' ', S);
         FTargetPID := StrToIntDef(TargetPIDPart, 0);

         WriteLN('[Debugger] Target PID: ', FTargetPID);
      end
      else begin
        FTargetPID := 0;
      end;
      
      if FTargetPID = 0
      then begin
        Result := False;
        SetState(dsError);
        Exit;
      end;
      
      if ResultState = dsNone
      then begin
        if AContinueCommand <> ''
        then Result := ExecuteCommand(AContinueCommand, [])
        else SetState(dsPause);
      end
      else SetState(ResultState);
    end;
  end;
  Result := True;
end;

function TGDBMIDebugger.GDBStepInto: Boolean;
begin
  case State of
    dsIdle, dsStop: begin
      Result := GDBStart('');
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-step', []);
    end;
  else
    Result := False;
  end;
end;

function TGDBMIDebugger.GDBStepOver: Boolean;
begin
  case State of
    dsIdle, dsStop: begin
      Result := GDBStart('');
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-next', []);
    end;
  else
    Result := False;
  end;
end;

function TGDBMIDebugger.GDBStop2: Boolean;
var
  S: String;
begin
  // Second pass stop
  Result := False;
  // verify stop
  if not ExecuteCommand('info program', [], S, [cfNoMICommand]) then Exit;

  if Pos('not being run', S) > 0
  then SetState(dsStop);
  Result := True;
end;

function TGDBMIDebugger.GDBStop: Boolean;
begin
  Result := False;
  Include(FGDBMIStates,gdbmisWaitingForKill);
  
  if State = dsError
  then begin
    // We don't know the state of the debugger, 
    // force a reinit. Let's hope this works.
    DebugProcess.Terminate(0);
    Done;
    Result := True;
    Exit;
  end;

  if State = dsRun
  then GDBPause;

  // not supported yet
  // ExecuteCommand('-exec-abort');
  if not ExecuteCommand('kill', [cfNoMiCommand]) then Exit;
  
  // the second part is handled in GDBStop2 (called by execute)
end;

function TGDBMIDebugger.GetGDBTypeInfo(const AExpression: String): TGDBType;
var
  ResultState: TDBGState;
  ResultValues: String;
begin
  if not ExecuteCommand('ptype %s', [AExpression], ResultState, ResultValues,
                        [cfIgnoreError, cfNoMiCommand])
  or (ResultState = dsError)
  then begin
    Result := nil;
  end
  else begin
    Result := TGdbType.CreateFromValues(ResultValues);
  end;
end;

function TGDBMIDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto,
             dcBreak{, dcWatch}, dcLocal, dcEvaluate, dcModify]
end;

procedure TGDBMIDebugger.Init;
var
  Line, S: String;
begin
  if CreateDebugProcess('-silent -i mi')
  then begin
    // Get initial debugger lines
    S := '';
    Line := StripLN(ReadLine);
    while DebugProcessRunning and (Line <> '(gdb) ') do
    begin
      S := S + Line + LINE_END;
      Line := StripLN(ReadLine);
    end;
    if S <> ''
    then MessageDlg('Debugger', 'Initialization output: ' + LINE_END + S, mtInformation, [mbOK], 0);

    ExecuteCommand('-gdb-set confirm off', []);
    
    // try to find the debugger version
    if ExecuteCommand('-gdb-version', [], S, [cfNoMiCommand]) // No MI since the output is no MI
    then FVersion := GetPart('(', ')', S)
    else FVersion := '';
    if FVersion < '5.3'
    then begin
      WriteLN('[WARNING] Debugger: Running an old (< 5.3) GDB version: ', FVersion);
      WriteLN('                    Not all functionality will be supported.');
    end
    else begin
      WriteLN('[Debugger] Running GDB version: ', FVersion);
    end;

    inherited Init;
  end
  else begin
    if DebugProcess = nil
    then MessageDlg('Debugger', 'Failed to create debug process for unknown reason', mtError, [mbOK], 0)
    else MessageDlg('Debugger', Format('Failed to create debug process: %s', [ReadLine]), mtError, [mbOK], 0);
    SetState(dsError);
  end;
end;

function TGDBMIDebugger.ProcessResult(var ANewState: TDBGState;
  var AResultValues: String; const ANoMICommand: Boolean): Boolean;
var
  S: String;
begin
  Result := False;
  AResultValues:='';
  S := StripLN(ReadLine);
  ANewState := dsNone;
  while DebugProcessRunning and (S <> '(gdb) ') do
  begin
    if S <> ''
    then begin
      case S[1] of
        '^': begin // result-record
          if ANoMICommand
          then begin
            S := GetPart('^', ',', S);
          end
          else begin
            AResultValues := S;
            S := GetPart('^', ',', AResultValues);
          end;
          if S = 'done'
          then begin
            Result := True;
          end
          else if S = 'running'
          then begin
            Result := True;
            ANewState := dsRun;
          end
          else if S = 'error'
          then begin
            Result := True;
            // todo implement with values
            if  (pos('msg=', AResultValues) > 0)
            and (pos('not being run', AResultValues) > 0)
            then ANewState := dsStop
            else ANewState := dsError;
          end
          else if S = 'exit'
          then begin
            Result := True;
            ANewState := dsIdle;
          end
          else WriteLN('[WARNING] Debugger: Unknown result class: ', S);
        end;
        '~': begin // console-stream-output
          // check for symbol info
          if Pos('no debugging symbols', S) > 0
          then begin
            FHasSymbols := False;
            WriteLN('[WARNING] Debugger: File ''',FileName, ''' has no debug symbols');
          end
          else if ANoMICommand 
          then begin
            // Strip surrounding ~" "
            S := Copy(S, 3, Length(S) - 3);
            if (RightStr(S, 2) = '\n') and (RightStr(S, 3) <> '\\n')
            then begin
              // Delete lineend symbol & add lineend
              S := Copy(S, 1, Length(S) - 2) + LINE_END;
            end;
            AResultValues := AResultValues + S;
          end
          else begin
            WriteLN('[Debugger] Console output: ', S);
          end;
        end;
        '@': begin // target-stream-output
          WriteLN('[Debugger] Target output: ', S);
        end;
        '&': begin // log-stream-output
          if S='&"kill\n"' then
            ANewState:=dsStop;
          WriteLN('[Debugger] Log output: ', S);
        end;
        '*', '+', '=': begin
          WriteLN('[WARNING] Debugger: Unexpected async-record: ', S);
        end;
      else
        WriteLN('[WARNING] Debugger: Unknown record: ', S);
      end;
    end;
    S := StripLN(ReadLine);
  end;
end;

function TGDBMIDebugger.ProcessRunning(var AStoppedParams: String): Boolean;
var
  S, AsyncClass: String;
  idx: Integer;
begin
  Result := True;

  S := StripLN(ReadLine);
  while DebugProcessRunning and (S <> '(gdb) ') do
  begin
    if S <> ''
    then begin
      case S[1] of
        '^': begin
          WriteLN('[WARNING] Debugger: unexpected result-record: ', S);
        end;
        '~': begin // console-stream-output
          WriteLN('[Debugger] Console output: ', S);
        end;
        '@': begin // target-stream-output
          WriteLN('[Debugger] Target output: ', S);
        end;
        '&': begin // log-stream-output
          WriteLN('[Debugger] Log output: ', S);
        end;
        '*': begin // exec-async-output
          AsyncClass := GetPart('*', ',', S);
          if AsyncClass = 'stopped'
          then begin
            AStoppedParams := S;
          end
          // Known, but undocumented classes
          else if AsyncClass = 'started'
          then begin
          end
          else if AsyncClass = 'disappeared'
          then begin
          end
          else begin
            // Assume targetoutput, strip char and continue
            WriteLN('[DBGTGT] *');
            S := AsyncClass + S;
            Continue;
          end;
        end;
        '+': begin // status-async-output
          WriteLN('[Debugger] Status output: ', S);
        end;
        '=': begin // notify-async-output
          WriteLN('[Debugger] Notify output: ', S);
        end;
      else
        // since target output isn't prefixed (yet?)
        // one of our known commands could be part of it.
        idx := Pos('*stopped', S);
        if idx  > 0
        then begin
          WriteLN('[DBGTGT] ', Copy(S, 1, idx - 1));
          Delete(S, 1, idx - 1);
          Continue;
        end
        else begin
          // normal target output
          WriteLN('[DBGTGT] ', S);
        end;
      end;
    end;
    S := StripLN(ReadLine);
  end;
end;

function TGDBMIDebugger.ProcessStopped(const AParams: String): Boolean;
  procedure ProcessFrame(const AFrame: String);
  var
    Frame: TStringList;
    Location: TDBGLocationRec;
  begin
    Frame := CreateMIValueList(AFrame);

    Location.Address := Pointer(StrToIntDef(Frame.Values['addr'], 0));
    Location.FuncName := Frame.Values['func'];
    Location.SrcFile := Frame.Values['file'];
    Location.SrcLine := StrToIntDef(Frame.Values['line'], -1);

    TGDBMILocals(Locals).AddLocals(Frame.Values['args']);
    Frame.Free;

    DoCurrent(Location);
  end;

  procedure ProcessException;
  var
    S: String;
    ExceptionName, ExceptionMessage: String;
    ResultList: TStringList;
    Location: TDBGLocationRec;
    CompactMode: Boolean;
  begin
    ExceptionName := 'Unknown';

    CompactMode := FVersion >= '5.3';
    
    if (CompactMode
        and ExecuteCommand(
              '-data-evaluate-expression ^^shortstring(^^pointer($fp+8)^^+12)^^',
              [], S, [cfIgnoreError]))
    or ((not CompactMode)
        and ExecuteCommand('-data-evaluate-expression pshortstring(%u)^',
              [Integer(GDBGetData(GDBGetData(GDBGetData('$fp+8', []))+12))],
              S, [cfIgnoreError]))
    then begin
      ResultList := CreateMIValueList(S);
      ExceptionName := ResultList.Values['value'];
      ExceptionName := GetPart('''', '''', ExceptionName);
      ResultList.Free;
    end;

    if CompactMode
    then begin
      ExceptionMessage := GDBGetText('^^Exception($fp+8)^^.FMessage', []);
      ExceptionMessage := DeleteBackSlashes(ExceptionMessage);
      writeln('ExceptionMessage={',ExceptionMessage,'}');
    end
    else ExceptionMessage := '### Not supported on GDB < 5.3 ###';

    Location.SrcLine := -1;
    Location.SrcFile := '';
    Location.FuncName := '';
    Location.Address := GDBGetData('$fp+12', []);
    
    if ExecuteCommand('info line * pointer(%d)', [Integer(Location.Address)],
                      S, [cfIgnoreError, cfNoMiCommand])
    then begin
      Location.SrcLine := StrToIntDef(GetPart('Line ', ' of', S), -1);
      Location.SrcFile := GetPart('\"', '\"', S);
    end;

    DoException(ExceptionName, ExceptionMessage);
    DoCurrent(Location);
  end;
  
  procedure ProcessBreak;
  var
    S: String;
    ErrorNo: Integer;
    Location: TDBGLocationRec;
  begin
    ErrorNo := Integer(GDBGetData('$fp+8', []));
    
    Location.SrcLine := -1;
    Location.SrcFile := '';
    Location.Address := GDBGetData('$fp+12', []);
    Location.FuncName := '';
    if ExecuteCommand('info line * pointer(%d)', [Integer(Location.Address)], S, [cfIgnoreError, cfNoMiCommand])
    then begin
      Location.SrcLine := StrToIntDef(GetPart('Line ', ' of', S), -1);
      Location.SrcFile := GetPart('\"', '\"', S);
    end;

    DoException(Format('RunError(%d)', [ErrorNo]), '');
    DoCurrent(Location);
  end;

var
  List: TStringList;
  S, Reason: String;
  BreakID: Integer;
  BreakPoint: TGDBMIBreakPoint;
  CanContinue: Boolean;
begin
  Result := True;
  List := CreateMIValueList(AParams);
  try
    Reason := List.Values['reason'];
    if (Reason = 'exited-normally') or (gdbmisWaitingForKill in fGDBMIStates)
    then begin
      SetState(dsStop);
      Exit;
    end;
    
    if Reason = 'exited'
    then begin
      SetExitCode(StrToIntDef(List.Values['exit-code'], 0));
      SetState(dsStop);
      Exit;
    end;
    
    if Reason = 'exited-signalled'
    then begin
      SetState(dsStop);
      DoException('External: ' + List.Values['signal-name'], '');
      // ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    if Reason = 'signal-received'
    then begin
      // TODO: check to run (un)handled
      SetState(dsPause);
      S := List.Values['signal-name'];
      if S <> 'SIGINT'
      then DoException('External: ' + S, '');
      ProcessFrame(List.Values['frame']);
      Exit;
    end;   
    
    if Reason = 'breakpoint-hit'
    then begin
      BreakID := StrToIntDef(List.Values['bkptno'], -1);
      if BreakID = -1
      then begin
        SetState(dsError);
        // ???
        Exit;
      end;

      if BreakID = FBreakErrorBreakID
      then begin
        SetState(dsPause);
        ProcessBreak;
        Exit;
      end;

      if BreakID = FExceptionBreakID
      then begin
        SetState(dsPause);
        ProcessException;
        Exit;
      end;
      
      BreakPoint := TGDBMIBreakPoint(FindBreakpoint(BreakID));
      if BreakPoint <> nil
      then begin
        CanContinue := False;
        BreakPoint.Hit(CanContinue);
        if CanContinue
        then begin
          ExecuteCommand('-exec-continue', []);
        end
        else begin
          SetState(dsPause);
          ProcessFrame(List.Values['frame']);
        end;
      end;
      Exit;
    end;
    
    if Reason = 'function-finished'
    then begin
      SetState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    if Reason = 'end-stepping-range'
    then begin
      SetState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    if Reason = 'location-reached'
    then begin
      SetState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    Result := False;
    WriteLN('[WARNING] Debugger: Unknown stopped reason: ', Reason);
  finally
    List.Free;
  end; 
end;

function TGDBMIDebugger.RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean;
begin
  case ACommand of
    dcRun:      Result := GDBRun;
    dcPause:    Result := GDBPause;
    dcStop:     Result := GDBStop;
    dcStepOver: Result := GDBStepOver;
    dcStepInto: Result := GDBStepInto;
    dcRunTo:    Result := GDBRunTo(String(APArams[0].VAnsiString), APArams[1].VInteger);
    dcJumpto:   Result := GDBJumpTo(String(APArams[0].VAnsiString), APArams[1].VInteger);
    dcEvaluate: Result := GDBEvaluate(String(APArams[0].VAnsiString), String(APArams[1].VPointer^));
  end;
end;


procedure TGDBMIDebugger.TestCmd(const ACommand: String);
begin
  ExecuteCommand(ACommand, [cfIgnoreError]);
end;

{ =========================================================================== }
{ TGDBMIBreakPoints }
{ =========================================================================== }

procedure TGDBMIBreakPoints.DoDebuggerStateChange;
begin
  inherited DoDebuggerStateChange;
  if (Debugger <> nil)
  and (Debugger.State in [dsStop, dsPause])
  then begin
    //SetBreakPoints;
  end;
end;

procedure TGDBMIBreakPoints.SetBreakPoints(ResetAll: boolean);
var
  n: Integer;
  BreakPoint: TGDBMIBreakPoint;
begin
  for n := 0 to Count - 1 do
  begin
    BreakPoint := TGDBMIBreakPoint(Items[n]);
    if (Breakpoint.FBreakID = 0) or ResetAll
    then BreakPoint.SetBreakPoint;
  end;
end;

procedure TGDBMIBreakPoints.InitTargetStart;
begin
  inherited InitTargetStart;
  SetBreakPoints(false);
end;

{ =========================================================================== }
{ TGDBMIBreakPoint }
{ =========================================================================== }

constructor TGDBMIBreakPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FBreakID := 0;
end;

destructor TGDBMIBreakPoint.Destroy;
begin
  ReleaseBreakPoint;
  inherited Destroy;
end;

procedure TGDBMIBreakPoint.DoEnableChange;
begin
  UpdateEnable;
  inherited;
end;

procedure TGDBMIBreakPoint.DoExpressionChange;
begin
  UpdateExpression;
  inherited;
end;

procedure TGDBMIBreakPoint.Hit(var ACanContinue: Boolean);
begin
  DoHit(HitCount + 1, ACanContinue);
end;

procedure TGDBMIBreakPoint.InitTargetStart;
begin
  // initialize values
  inherited InitTargetStart;
end;

procedure TGDBMIBreakPoint.SetBreakpoint;
var
  S: String;
  ResultList, BkptList: TStringList;
  ResultState: TDBGState;
begin
  if Debugger = nil then Exit;

  BeginUpdate;
  try
    if FBreakID<>0 then ReleaseBreakPoint;

    TGDBMIDebugger(Debugger).ExecuteCommand('-break-insert %s:%d', [
       ExtractFileName(Source), Line], ResultState, S, [cfIgnoreError]);
    ResultList := CreateMIValueList(S);
    BkptList := CreateMIValueList(ResultList.Values['bkpt']);
    FBreakID := StrToIntDef(BkptList.Values['number'], 0);
    SetHitCount(StrToIntDef(BkptList.Values['times'], 0));
    if FBreakID<>0 then
      SetValid(vsValid)
    else
      SetValid(vsInvalid);
    UpdateExpression;
    UpdateEnable;
    ResultList.Free;
    BkptList.Free;
  finally
    EndUpdate;
  end;
end;

procedure TGDBMIBreakPoint.ReleaseBreakPoint;
begin
  if  (FBreakID <> 0)
  and (Debugger <> nil)
  then begin
    TGDBMIDebugger(Debugger).ExecuteCommand('-break-delete %d', [FBreakID], []);
    FBreakID:=0;
    SetHitCount(0);
  end;
end;

procedure TGDBMIBreakPoint.SetLocation(const ASource: String;
  const ALine: Integer);
begin
  writeln('TGDBMIBreakPoint.SetLocation A ',Source = ASource,' ',Line = ALine);
  if (Source = ASource) and (Line = ALine) then exit;
  inherited;
  if Debugger = nil then Exit;
  if TGDBMIDebugger(Debugger).State in [dsStop, dsPause, dsIdle]
  then SetBreakpoint;
end;

procedure TGDBMIBreakPoint.UpdateEnable;
const
  CMD: array[Boolean] of String = ('disable', 'enable');
begin
  if (FBreakID = 0)
  or (Debugger = nil)
  then Exit;

  TGDBMIDebugger(Debugger).ExecuteCommand('-break-%s %d',
                                          [CMD[Enabled], FBreakID], []);
end;

procedure TGDBMIBreakPoint.UpdateExpression;
begin
end;

{ =========================================================================== }
{ TGDBMILocals }
{ =========================================================================== }

procedure TGDBMILocals.AddLocals(const AParams: String);
var
  n, addr: Integer;
  LocList, List: TStrings;
  S, Name, Value: String;
begin
  LocList := CreateMIValueList(AParams);
  for n := 0 to LocList.Count - 1 do
  begin
    List := CreateMIValueList(LocList[n]);
    Name := List.Values['name'];
    if Name = 'this'
    then Name := 'Self';

    Value := List.Values['value'];
    // try to deref. strings
    S := GetPart(['(pchar) ', '(ansistring) '], [], Value, True, False);
    if S <> ''
    then begin
      addr := StrToIntDef(S, 0);
      if addr = 0
      then Value := ''''''
      else Value := '''' + TGDBMIDebugger(Debugger).GDBGetText(Pointer(addr)) + '''';
    end;

    FLocals.Add(Name + '=' + Value);
    FreeAndNil(List);
  end;
  FreeAndNil(LocList);
end;

function TGDBMILocals.Count: Integer;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    LocalsNeeded;
    Result := FLocals.Count;
  end
  else Result := 0;
end;

constructor TGDBMILocals.Create(const ADebugger: TDebugger);
begin
  FLocals := TStringList.Create;
  FLocals.Sorted := True;
  FLocalsValid := False;
  inherited;
end;

destructor TGDBMILocals.Destroy;
begin
  inherited;
  FreeAndNil(FLocals);
end;

procedure TGDBMILocals.DoStateChange;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    DoChange;
  end
  else begin
    FLocalsValid := False;
    FLocals.Clear;
  end;
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

procedure TGDBMILocals.LocalsNeeded;
var
  S: String;
  List: TStrings;
begin
  if Debugger = nil then Exit;
  if not FLocalsValid
  then begin
    TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-locals 1', S, []);
    List := CreateMIValueList(S);
    AddLocals(List.Values['locals']);
    FreeAndNil(List);
    FLocalsValid := True;
  end;
end;

{ =========================================================================== }
{ TGDBMIWatch }
{ =========================================================================== }

constructor TGDBMIWatch.Create(ACollection: TCollection);
begin
  FEvaluated := False;
  inherited;
end;

procedure TGDBMIWatch.DoEnableChange;
begin
  inherited;
end;

procedure TGDBMIWatch.DoExpressionChange;
begin
  FEvaluated := False;
  inherited;
end;

procedure TGDBMIWatch.DoStateChange;
begin
  if Debugger = nil then Exit;

  if Debugger.State in [dsPause, dsStop]
  then FEvaluated := False;
  if Debugger.State = dsPause then Changed(False);
end;

procedure TGDBMIWatch.EvaluationNeeded;
var
  ExprIsValid: Boolean;
begin
  if FEvaluated then Exit;
  if Debugger = nil then Exit;

  if (Debugger.State in [dsPause, dsStop])
  and Enabled
  then begin
    ExprIsValid:=TGDBMIDebugger(Debugger).GDBEvaluate(Expression, FValue);
    if ExprIsValid then
      SetValid(vsValid)
    else
      SetValid(vsInvalid);
  end
  else begin
    SetValid(vsInvalid);
  end;
  FEvaluated := True;
end;

function TGDBMIWatch.GetValue: String;
begin
  if  (Debugger <> nil)
  and (Debugger.State in [dsStop, dsPause])
  and Enabled
  then begin
    EvaluationNeeded;
    Result := FValue;
  end
  else Result := inherited GetValue;
end;

function TGDBMIWatch.GetValid: TValidState;
begin
  EvaluationNeeded;
  Result := inherited GetValid;
end;

{ =========================================================================== }
{ TGDBMICallStack }
{ =========================================================================== }

constructor TGDBMICallStack.Create(const ADebugger: TDebugger); 
begin
  FCount := -1;
  inherited;
end;

function TGDBMICallStack.CreateStackEntry(const AIndex: Integer): TDBGCallStackEntry;
var                 
  n: Integer;
  S: String;
  Arguments, ArgList, List: TStrings;
begin
  if Debugger = nil then Exit;

  Arguments := TStringList.Create;
  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-arguments 1 %d %d',
                                          [AIndex, AIndex], S, []);
  List := CreateMIValueList(S);   
  S := List.Values['stack-args'];
  FreeAndNil(List);
  List := CreateMIValueList(S);
  S := List.Values['frame']; // all arguments
  FreeAndNil(List);
  List := CreateMIValueList(S);   
  S := List.Values['args'];
  FreeAndNil(List);
  
  ArgList := CreateMIValueList(S);
  for n := 0 to ArgList.Count - 1 do
  begin
    List := CreateMIValueList(ArgList[n]);
    Arguments.Add(List.Values['name'] + '=' + List.Values['value']);
    FreeAndNil(List);
  end;
  FreeAndNil(ArgList);
  
  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-frames %d %d',
                                          [AIndex, AIndex], S, []);
  List := CreateMIValueList(S);   
  S := List.Values['stack'];
  FreeAndNil(List);
  List := CreateMIValueList(S);   
  S := List.Values['frame'];
  FreeAndNil(List);
  List := CreateMIValueList(S);   
  Result := TDBGCallStackEntry.Create(
    AIndex, 
    Pointer(StrToIntDef(List.Values['addr'], 0)),
    Arguments,
    List.Values['func'],
    List.Values['file'],
    StrToIntDef(List.Values['line'], 0)
  );
  
  FreeAndNil(List);
  Arguments.Free;
end;

procedure TGDBMICallStack.DoStateChange; 
begin
  if Debugger.State <> dsPause 
  then FCount := -1;    
  inherited;
end;

function TGDBMICallStack.GetCount: Integer;
var
  S: String;
  List: TStrings;
begin
  if FCount = -1 
  then begin
    if Debugger = nil 
    then FCount := 0
    else begin
      TGDBMIDebugger(Debugger).ExecuteCommand('-stack-info-depth', S, []);
      List := CreateMIValueList(S);
      FCount := StrToIntDef(List.Values['depth'], 0);
      FreeAndNil(List);
    end;
  end;
  
  Result := FCount;
end;

{ =========================================================================== }
{ TGDBMIExpression }
{ =========================================================================== }

constructor TGDBMIExpression.Create(const ADebugger: TGDBMIDebugger; const AExpression: String);
begin
  inherited Create;
  FDebugger := ADebugger;
  FLeft := nil;
  FRight := nil;
  CreateSubExpression(Trim(AExpression));
end;

procedure TGDBMIExpression.CreateSubExpression(const AExpression: String);
  function CheckOperator(const APos: Integer; const AOperator: String): Boolean;
  var
    S: String;
  begin
    Result := False;
    if APos + Length(AOperator) > Length(AExpression) then Exit;
    if StrLIComp(@AExpression[APos], @AOperator[1], Length(AOperator)) <> 0 then Exit;
    if (APos > 1) and not (AExpression[APos - 1] in [' ', '(']) then Exit;
    if (APos + Length(AOperator) <= Length(AExpression)) and not (AExpression[APos + Length(AOperator)] in [' ', '(']) then Exit;

    S := Copy(AExpression, 1, APos - 1);
    if S <> ''
    then FLeft := TGDBMIExpression.Create(FDebugger, S);
    S := Copy(AExpression, APos + Length(AOperator), MaxInt);
    if S <> ''
    then FRight := TGDBMIExpression.Create(FDebugger, S);
    FOperator := AOperator;
    Result := True;
  end;
type
  TStringState = (ssNone, ssString, ssLeave);
var
  n: Integer;
  S, LastWord: String;
  HookCount: Integer;
  InString: TStringState;
  Sub: TGDBMIExpression;
begin
  HookCount := 0;
  InString := ssNone;
  LastWord := '';
  for n := 1 to Length(AExpression)  do
  begin
    if AExpression[n] = ''''
    then begin
      case InString of
        ssNone:  InString := ssString;
        ssString:InString := ssLeave;
        ssLeave: InString := ssString;
      end;
      S := S + AExpression[n];
      LastWord := '';
      Continue;
    end;
    if InString = ssString
    then begin
      S := S + AExpression[n];
      LastWord := '';
      Continue;
    end;
    InString := ssNone;

    case AExpression[n] of
      '(', '[': begin
        if HookCount = 0
        then begin
          SetLength(S, Length(S) - Length(LastWord));
          if S <> ''
          then FLeft := TGDBMIExpression.Create(FDebugger, S);
          if LastWord = ''
          then begin
            FOperator := AExpression[n];
          end
          else begin
            FOperator := LastWord;
            FRight := TGDBMIExpression.Create(FDebugger, '');
            FRight.FOperator := AExpression[n];
          end;
          LastWord := '';
          S := '';
        end;
        Inc(HookCount);
        if HookCount = 1
        then Continue;
      end;
      ')', ']': begin
        Dec(HookCount);
        if HookCount = 0
        then begin
          if S <> ''
          then begin
            if FRight = nil
            then FRight := TGDBMIExpression.Create(FDebugger, S)
            else FRight.FRight := TGDBMIExpression.Create(FDebugger, S);
          end;
          if n < Length(AExpression)
          then begin
            Sub := TGDBMIExpression.Create(FDebugger, '');
            Sub.FLeft := FLeft;
            Sub.FOperator := FOperator;
            Sub.FRight := FRight;
            FLeft := Sub;
            Sub := TGDBMIExpression.Create(FDebugger, Copy(AExpression, n + 1, MaxInt));
            if Sub.FLeft = nil
            then begin
              FOperator := Sub.FOperator;
              FRight := Sub.FRight;
              Sub.FRight := nil;
              Sub.Free;
            end
            else begin
              FOperator := '';
              FRight := Sub;
            end;
          end;
          Exit;
        end;
      end;
    end;
    if HookCount = 0
    then begin
      case AExpression[n] of
        '-', '+', '*', '/', '^', '@', '=', ',': begin
          if S <> ''
          then FLeft := TGDBMIExpression.Create(FDebugger, S);
          S := Copy(AExpression, n + 1, MaxInt);
          if Trim(S) <> ''
          then FRight := TGDBMIExpression.Create(FDebugger, S);
          FOperator := AExpression[n];
          Exit;
        end;
        'a', 'A': begin
          if CheckOperator(n, 'and') then Exit;
        end;
        'o', 'O': begin
          if CheckOperator(n, 'or') then Exit;
        end;
        'm', 'M': begin
          if CheckOperator(n, 'mod') then Exit;
        end;
        'd', 'D': begin
          if CheckOperator(n, 'div') then Exit;
        end;
        'x', 'X': begin
          if CheckOperator(n, 'xor') then Exit;
        end;
        's', 'S': begin
          if CheckOperator(n, 'shl') then Exit;
          if CheckOperator(n, 'shr') then Exit;
        end;
      end;
    end;

    if AExpression[n] = ' '
    then LastWord := ''
    else LastWord := LastWord + AExpression[n];
    S := S + AExpression[n];
  end;
  if S = AExpression
  then FOperator := S
  else CreateSubExpression(S);
end;

destructor TGDBMIExpression.Destroy;
begin
  FreeAndNil(FRight);
  FreeAndNil(FLeft);
  inherited;
end;

function TGDBMIExpression.DumpExpression: String;
// Mainly used for debugging purposes
begin
  if FLeft = nil
  then Result := ''
  else Result := '«L:' + FLeft.DumpExpression + '»';

  if FOperator = '('
  then Result := Result + '(«R:' + FRight.DumpExpression + '»)'
  else if FOperator = '['
  then Result := Result + '[«R:' + FRight.DumpExpression + '»]'
  else begin
    if (Length(FOperator) > 0)
    and (FOperator[1] = '''')
    then Result := Result + '«O:' + ConvertToCString(FOperator) + '»'
    else Result := Result + '«O:' + FOperator + '»';
    if FRight <> nil
    then Result := Result + '«R:' + FRight.DumpExpression + '»';
  end;
end;

function TGDBMIExpression.GetExpression(var AResult: String): Boolean;
var
  ResultState: TDBGState;
  S, ResultValues: String;
  List: TStrings;
  GDBType: TGDBType;
begin  
  Result := False;
  
  if FLeft = nil
  then AResult := ''
  else begin
    if not FLeft.GetExpression(S) then Exit;
    AResult := S;
  end;

  if FOperator = '('
  then begin
    if not FRight.GetExpression(S) then Exit;
    AResult := AResult + '(' + S + ')';
  end
  else if FOperator = '['
  then begin              
    if not FRight.GetExpression(S) then Exit;
    AResult := AResult + '[' + S + ']';
  end
  else begin
    if (Length(FOperator) > 0)
    and (FOperator[1] = '''')
    then AResult := AResult + ConvertToCString(FOperator)
    else begin                                           
      GDBType := FDebugger.GetGDBTypeInfo(FOperator);
      if GDBType = nil
      then begin
        // no type possible, use literal operator
        AResult := AResult + FOperator;
      end;

      if not FDebugger.ExecuteCommand('ptype %s', [FOperator],  ResultState,
                                       ResultValues, [cfIgnoreError, cfNoMiCommand])
      then Exit;
      
      if ResultState = dsError
      then begin
        // no type possible, use literal operator
        AResult := AResult + FOperator;
      end
      else begin
        WriteLN('PType result: ', ResultValues);
        List := CreateValueList(ResultValues);
        S := List.Values['type'];
        WriteLN('PType type: ', S);
        List.Free;
        if (S <> '') and (S[1] = '^') and (Pos('class', S) <> 0)
        then begin
          AResult := AResult + GetPart('^', ' ', S) + '(' + FOperator + ')';
        end
        else begin
          // no type possible or no class, use literal operator
          AResult := AResult + FOperator;
        end
      end;
    end;
    if FRight <> nil
    then begin
      if not FRight.GetExpression(S) then Exit;
      AResult := AResult + S;
    end;
  end;
  
  Result := True;
end;

end.
{ =============================================================================
  $Log$
  Revision 1.28  2003/06/09 17:20:43  mattias
  implemented stop debugging on rebuild

  Revision 1.27  2003/06/09 15:58:05  mattias
  implemented view call stack key and jumping to last stack frame with debug info

  Revision 1.26  2003/06/09 14:30:47  marc
  MWE: + Added working dir.

  Revision 1.25  2003/06/05 00:20:26  marc
  MWE: * Fixed initial run to cursor

  Revision 1.24  2003/06/03 10:29:22  mattias
  implemented updates between source marks and breakpoints

  Revision 1.23  2003/06/03 01:35:40  marc
  MWE: = Splitted TDBGBreakpoint into TBaseBreakPoint, TIDEBreakpoint and
         TDBGBreakPoint

  Revision 1.22  2003/06/02 21:37:30  mattias
  fixed debugger stop

  Revision 1.21  2003/05/30 00:53:09  marc
  MWE: * fixed debugger.stop

  Revision 1.20  2003/05/29 18:47:27  mattias
  fixed reposition sourcemark

  Revision 1.19  2003/05/29 17:40:10  marc
  MWE: * Fixed string resolving
       * Updated exception handling

  Revision 1.18  2003/05/29 07:25:02  mattias
  added Destroying flag, debugger now always shuts down

  Revision 1.17  2003/05/29 02:32:52  marc
  MWE: + Added GDB version check to exception parser

  Revision 1.16  2003/05/28 17:40:55  mattias
  recuced update notifications

  Revision 1.15  2003/05/28 08:46:24  mattias
  break;points dialog now gets the items without debugger

  Revision 1.14  2003/05/28 00:58:50  marc
  MWE: * Reworked breakpoint handling

  Revision 1.13  2003/05/27 20:58:12  mattias
  implemented enable and deleting breakpoint in breakpoint dlg

  Revision 1.12  2003/05/27 17:53:44  mattias
  fixed getting target PID for fpc1.1 programs

  Revision 1.11  2003/05/27 08:01:31  marc
  MWE: + Added exception break
       * Reworked adding/removing breakpoints
       + Added Unknown breakpoint type

  Revision 1.10  2003/05/23 14:12:51  mattias
  implemented restoring breakpoints

  Revision 1.9  2003/05/22 23:08:19  marc
  MWE: = Moved and renamed debuggerforms so that they can be
         modified by the ide
       + Added some parsing to evaluate complex expressions
         not understood by the debugger

  Revision 1.8  2002/11/05 22:41:13  lazarus
  MWE:
    * Some minor debugger updates
    + Added evaluate to debugboss
    + Added hint debug evaluation

  Revision 1.7  2002/05/10 06:57:48  lazarus
  MG: updated licenses

  Revision 1.6  2002/04/30 15:57:40  lazarus
  MWE:
    + Added callstack object and dialog
    + Added checks to see if debugger = nil
    + Added dbgutils

  Revision 1.5  2002/04/24 20:42:29  lazarus
  MWE:
    + Added watches
    * Updated watches and watchproperty dialog to load as resource
    = renamed debugger resource files from *.lrc to *.lrs
    * Temporary fixed language problems on GDB (bug #508)
    * Made Debugmanager dialog handling more generic

  Revision 1.4  2002/03/27 08:57:16  lazarus
  MG: reduced compiler warnings

  Revision 1.3  2002/03/23 15:54:30  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

  Revision 1.2  2002/03/12 23:55:36  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

  Revision 1.1  2002/03/09 02:03:59  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.6  2002/02/20 23:33:24  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.5  2002/02/06 08:58:29  lazarus
  MG: fixed compiler warnings and asking to create non existing files

  Revision 1.4  2002/02/05 23:16:48  lazarus
  MWE: * Updated tebugger
       + Added debugger to IDE

  Revision 1.3  2001/11/12 19:28:23  lazarus
  MG: fixed create, virtual constructors makes no sense

  Revision 1.2  2001/11/06 23:59:13  lazarus
  MWE: + Initial breakpoint support
       + Added exeption handling on process.free

  Revision 1.1  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.


}

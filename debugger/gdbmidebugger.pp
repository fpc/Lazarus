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
  Classes, Process, Debugger, CmdLineDebugger;


type
  TGDBMIProgramInfo = record
    State: TDBGState;
    BreakPoint: Integer; // ID of Breakpoint hit
    Signal: Integer;     // Signal no if we hit one
    SignalText: String;  // Signal text if we hit one
  end;


  TGDBMIDebugger = class(TCmdLineDebugger)
  private
    FCommandQueue: TStringList;
    FHasSymbols: Boolean;
    FStoppedParams: String;
    FTargetPID: Integer;
    function  FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
    function  GDBEvaluate(const AExpression: String; var AResult: String): Boolean;
    function  GDBRun: Boolean;
    function  GDBPause: Boolean;
    function  GDBStart: Boolean;
    function  GDBStop: Boolean;
    function  GDBStepOver: Boolean;
    function  GDBStepInto: Boolean;
    function  GDBRunTo(const ASource: String; const ALine: Integer): Boolean;
    function  GDBJumpTo(const ASource: String; const ALine: Integer): Boolean;
    function  ProcessResult(var ANewState: TDBGState; var AResultValues: String): Boolean;
    function  ProcessRunning: Boolean;
    function  ProcessStopped(const AParams: String): Boolean;
    function  ExecuteCommand(const ACommand: String): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; var AResultValues: String): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; var AResultValues: String): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; var AResultState: TDBGState; var AResultValues: String): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; const AIgnoreError: Boolean; var AResultState: TDBGState; var AResultValues: String): Boolean; overload;
  protected
    function  ChangeFileName: Boolean; override;
    function  CreateBreakPoints: TDBGBreakPoints; override;
    function  CreateLocals: TDBGLocals; override;
    function  CreateCallStack: TDBGCallStack; override;
    function  CreateWatches: TDBGWatches; override;
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

uses
  SysUtils, Dialogs, DBGUtils;

type
  TGDBMIBreakPoint = class(TDBGBreakPoint)
  private
    FBreakID: Integer;
    procedure SetBreakPoint;
  protected
    procedure DoActionChange; override;
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoStateChange; override;
    procedure SetLocation(const ASource: String; const ALine: Integer); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Hit;
  end;

  TGDBMILocals = class(TDBGLocals)
  private
    FLocals: TStringList;
    FLocalsValid: Boolean;
    procedure LocalsNeeded;
  protected
    procedure DoStateChange; override;
    function GetName(const AnIndex: Integer): String; override;
    function GetValue(const AnIndex: Integer): String; override;
  public
    procedure AddLocals(const AParams:String);
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
    function  GetValid: Boolean; override;
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


function CreateValueList(AResultValues: String): TStringList;
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



{ =========================================================================== }
{ TGDBMIDebugger }
{ =========================================================================== }

function TGDBMIDebugger.ChangeFileName: Boolean;
//var
//  S: String;
begin
  FHasSymbols := True; // True untilproven otherwise
  Result := ExecuteCommand('-file-exec-and-symbols %s', [FileName]) and inherited ChangeFileName;

  if Result and FHasSymbols
  then begin
    // Force setting language
    // Setting extensions dumps GDB (bug #508)
    ExecuteCommand('-gdb-set language pascal');
(*
    ExecuteCommand('-gdb-set extension-language .lpr pascal');
    if not FHasSymbols then Exit; // file-exec-and-symbols not allways result in no symbols
    ExecuteCommand('-gdb-set extension-language .lrs pascal');
    ExecuteCommand('-gdb-set extension-language .dpr pascal');
    ExecuteCommand('-gdb-set extension-language .pas pascal');
    ExecuteCommand('-gdb-set extension-language .pp pascal');
    ExecuteCommand('-gdb-set extension-language .inc pascal');
*)
  end;
end;

constructor TGDBMIDebugger.Create(const AExternalDebugger: String);
begin
  FCommandQueue := TStringList.Create;
  FTargetPID := 0;
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

function TGDBMIDebugger.CreateLocals: TDBGLocals;
begin
  Result := TGDBMILocals.Create(Self);
end;

function TGDBMIDebugger.CreateWatches: TDBGWatches;
begin
  Result := TDBGWatches.Create(Self, TGDBMIWatch);
end;

destructor TGDBMIDebugger.Destroy;
begin
  inherited;
  FreeAndNil(FCommandQueue);
end;

procedure TGDBMIDebugger.Done;
begin
  if State = dsRun then GDBPause;
  ExecuteCommand('-gdb-exit');
  inherited Done;
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String): Boolean;
var
  S: String;
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, [], False, ResultState, S);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; var AResultValues: String): Boolean;
var
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, [], False, ResultState, AResultValues);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; AValues: array of const): Boolean;
var
  S: String;
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, AValues, False, ResultState, S);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; AValues: array of const; var AResultValues: String): Boolean;
var
  ResultState: TDBGState;
begin
  Result := ExecuteCommand(ACommand, AValues, False, ResultState, AResultValues);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; AValues: array of const; var AResultState: TDBGState; var AResultValues: String): Boolean;
begin
  Result := ExecuteCommand(ACommand, AValues, False, AResultState, AResultValues);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; AValues: array of const; const AIgnoreError: Boolean; var AResultState: TDBGState; var AResultValues: String): Boolean;
begin
  FCommandQueue.Add(ACommand);
  if FCommandQueue.Count > 1 then Exit;
  repeat
    SendCmdLn(FCommandQueue[0], AValues);
    Result := ProcessResult(AResultState, AResultValues);
    if Result
    then begin
      if (AResultState <> dsNone)
      and ((AResultState <> dsError) or not AIgnoreError)
      then SetState(AResultState);
      if State = dsRun
      then Result := ProcessRunning;
    end;
    FCommandQueue.Delete(0);
    if FStoppedParams <> ''
    then begin
      ProcessStopped(FStoppedParams);
      FStoppedParams := '';
    end;
  until not Result or (FCommandQueue.Count = 0);
end;

function  TGDBMIDebugger.FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
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

function TGDBMIDebugger.GDBEvaluate(const AExpression: String; var AResult: String): Boolean;
var
  ResultState: TDBGState;
  ResultValues: String;
  ResultList: TStringList;
begin
  Result := ExecuteCommand('-data-evaluate-expression %s', [AExpression], True, ResultState, ResultValues)
    and (ResultState <> dsError);

  ResultList := CreateValueList(ResultValues);
  if ResultState = dsError
  then AResult := ResultList.Values['msg']
  else AResult := ResultList.Values['value'];
  ResultList.Free;
end;

function TGDBMIDebugger.GDBJumpTo(const ASource: String; const ALine: Integer): Boolean;
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
      GDBStart;
      if State = dsPause
      then begin
        Result := ExecuteCommand('-exec-continue');
      end
      else begin
        //error???
      end;
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-continue');
    end;
    dsIdle: begin
      WriteLN('[WARNING] Debugger: Unable to run in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBRunTo(const ASource: String; const ALine: Integer): Boolean;
begin
  Result := False;
  if State in [dsRun, dsError] then Exit;

  Result := ExecuteCommand('-exec-until %s:%d', [ASource, ALine]);
end;

function TGDBMIDebugger.GDBStart: Boolean;
var
  S: String;
  ResultState: TDBGState;
begin
  if State in [dsStop]
  then begin
    if FHasSymbols
    then begin
      if Arguments <>''
      then ExecuteCommand('-exec-arguments %s', [Arguments]);
      ExecuteCommand('-break-insert -t main');
      ExecuteCommand('-exec-run');

      // try to find PID
      SendCmdLn('info program', []);
      ReadLine; // skip repeated command
      S := ReadLine;
      FTargetPID := StrToIntDef(GetPart('child process ', '.', S), 0);
      if ProcessResult(ResultState, S)
      then begin
        if ResultState = dsNone
        then SetState(dsPause)
        else SetState(ResultState);
      end;
    end;
  end;
  Result := True;
end;

function TGDBMIDebugger.GDBStepInto: Boolean;
begin
  case State of
    dsIdle, dsStop: begin
      Result := GDBStart;
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-step');
    end;
  else
    Result := False;
  end;
end;

function TGDBMIDebugger.GDBStepOver: Boolean;
begin
  case State of
    dsIdle, dsStop: begin
      Result := GDBStart;
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-next');
    end;
  else
    Result := False;
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
    Exit;
  end;

  if State = dsRun
  then GDBPause;

  if State = dsPause
  then begin
    // not supported yet
    // ExecuteCommand('-exec-abort');
    ExecuteCommand('kill');
    SetState(dsStop); //assume stop until abort is supported;
  end;
  Result := True;
end;

function TGDBMIDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak{, dcWatch}, dcLocal, dcEvaluate, dcModify]
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

    ExecuteCommand('-gdb-set confirm off');
    inherited Init;
  end
  else begin
    if DebugProcess = nil
    then MessageDlg('Debugger', 'Failed to create debug process for unknown reason', mtError, [mbOK], 0)
    else MessageDlg('Debugger', Format('Failed to create debug process: %s', [ReadLine]), mtError, [mbOK], 0);
    SetState(dsError);
  end;
end;

function TGDBMIDebugger.ProcessResult(var ANewState: TDBGState; var AResultValues: String): Boolean;
var
  S: String;
begin
  Result := False;
  S := StripLN(ReadLine);
  ANewState := dsNone;
  while DebugProcessRunning and (S <> '(gdb) ') do
  begin
    if S <> ''
    then begin
      case S[1] of
        '^': begin // result-record
          AResultValues := S;
          S := GetPart('^', ',', AResultValues);
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
            ANewState := dsError;
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
            WriteLN('WARNING: File ''',FileName, ''' has no debug symbols');
          end
          else begin
            WriteLN('[Debugger] Console output: ', S);
          end;
        end;
        '@': begin // target-stream-output
          WriteLN('[Debugger] Target output: ', S);
        end;
        '&': begin // log-stream-output
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

function TGDBMIDebugger.ProcessRunning: Boolean;
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
            FStoppedParams := S;
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
    Frame := CreateValueList(AFrame);

    Location.Adress := Pointer(StrToIntDef(Frame.Values['addr'], 0));
    Location.FuncName := Frame.Values['func'];
    Location.SrcFile := Frame.Values['file'];
    Location.SrcLine := StrToIntDef(Frame.Values['line'], -1);

    TGDBMILocals(Locals).AddLocals(Frame.Values['args']);
    Frame.Free;

    DoCurrent(Location);
  end;
var
  List: TStringList;
  S, Reason: String;
  BreakPoint: TGDBMIBreakPoint;
begin
  Result := True;
  List := CreateValueList(AParams);
  Reason := List.Values['reason'];
  if Reason = 'exited-normally'
  then begin
    SetState(dsStop);
  end
  else if Reason = 'exited'
  then begin
    SetExitCode(StrToIntDef(List.Values['exit-code'], 0));
    SetState(dsStop);
  end
  else if Reason = 'exited-signalled'
  then begin
    SetState(dsStop);
    // TODO: define signal no
    DoException(0, List.Values['signal-name']);
    ProcessFrame(List.Values['frame']);
  end
  else if Reason = 'signal-received'
  then begin
    // TODO: check to run (un)handled
    // TODO: define signal no
    SetState(dsPause);
    S := List.Values['signal-name'];
    if S <> 'SIGINT'
    then DoException(0, S);
    ProcessFrame(List.Values['frame']);
  end
  else if Reason = 'breakpoint-hit'
  then begin
    BreakPoint := TGDBMIBreakPoint(FindBreakpoint(StrToIntDef(List.Values['bkptno'], -1)));
    if BreakPoint <> nil
    then begin
      BreakPoint.Hit;
      if (bpaStop in BreakPoint.Actions)
      then begin
        SetState(dsPause);
        ProcessFrame(List.Values['frame']);
      end
      else begin
        ExecuteCommand('-exec-continue');
      end;
    end;
  end
  else if Reason = 'function-finished'
  then begin
    SetState(dsPause);
    ProcessFrame(List.Values['frame']);
  end
  else if Reason = 'end-stepping-range'
  then begin
    SetState(dsPause);
    ProcessFrame(List.Values['frame']);
  end
  else if Reason = 'location-reached'
  then begin
    SetState(dsPause);
    ProcessFrame(List.Values['frame']);
  end
  else begin
    Result := False;
    WriteLN('[WARNING] Debugger: Unknown stopped reason: ', Reason);
  end;

  List.Free;
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
  ExecuteCommand(ACommand);
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
  if  (FBreakID <> 0)
  and (Debugger <> nil)
  then begin
    TGDBMIDebugger(Debugger).ExecuteCommand('-break-delete %d', [FBreakID]);
  end;

  inherited Destroy;
end;

procedure TGDBMIBreakPoint.DoActionChange;
begin
end;

procedure TGDBMIBreakPoint.DoEnableChange;
const
  CMD: array[Boolean] of String = ('disable', 'enable');
begin
  if (FBreakID = 0) 
  or (Debugger = nil)
  then Exit;

  TGDBMIDebugger(Debugger).ExecuteCommand('-break-%s %d', [CMD[Enabled], FBreakID]);
end;

procedure TGDBMIBreakPoint.DoExpressionChange;
begin
end;

procedure TGDBMIBreakPoint.DoStateChange;
begin
  inherited;
  if  (Debugger.State = dsStop)
  and (FBreakID = 0)
  then SetBreakpoint;
end;

procedure TGDBMIBreakPoint.Hit;
begin
  SetHitCount(HitCount + 1);
  if bpaEnableGroup in Actions
  then EnableGroups;
  if bpaDisableGroup in Actions
  then DisableGroups;
end;

procedure TGDBMIBreakPoint.SetBreakpoint;
var
  S: String;
  ResultList, BkptList: TStringList;
  ResultState: TDBGState;
begin
  if Debugger = nil then Exit;
  
  TGDBMIDebugger(Debugger).ExecuteCommand('-break-insert %s:%d', [Source, Line], True, ResultState, S);
  ResultList := CreateValueList(S);
  BkptList := CreateValueList(ResultList.Values['bkpt']);
  FBreakID := StrToIntDef(BkptList.Values['number'], 0);
  SetHitCount(StrToIntDef(BkptList.Values['times'], 0));
  SetValid(FBreakID <> 0);
  DoEnableChange;
  ResultList.Free;
  BkptList.Free;
end;


procedure TGDBMIBreakPoint.SetLocation(const ASource: String; const ALine: Integer);
begin
  inherited;
  if Debugger = nil then Exit;
  if TGDBMIDebugger(Debugger).State in [dsStop, dsPause, dsIdle]
  then SetBreakpoint;
end;

{ =========================================================================== }
{ TGDBMILocals }
{ =========================================================================== }

procedure TGDBMILocals.AddLocals(const AParams: String);
var
  n: Integer;
  LocList, List: TStrings;
  Name: String;
begin
  LocList := CreateValueList(AParams);
  for n := 0 to LocList.Count - 1 do
  begin
    List := CreateValueList(LocList[n]);
    Name := List.Values['name'];
    if Name = 'this'
    then Name := 'Self';
    FLocals.Add(Name + '=' + List.Values['value']);
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
    TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-locals 1', S);
    List := CreateValueList(S);
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
begin
  if FEvaluated then Exit;
  if Debugger = nil then Exit;

  if (Debugger.State in [dsPause, dsStop])
  and Enabled
  then begin
    SetValid(TGDBMIDebugger(Debugger).GDBEvaluate(Expression, FValue));
  end
  else begin
    SetValid(False);
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

function TGDBMIWatch.GetValid: Boolean;
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
  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-arguments 1 %d %d', [AIndex, AIndex], S);
  List := CreateValueList(S);   
  S := List.Values['stack-args'];
  FreeAndNil(List);
  List := CreateValueList(S);
  S := List.Values['frame']; // all arguments
  FreeAndNil(List);
  List := CreateValueList(S);   
  S := List.Values['args'];
  FreeAndNil(List);
  
  ArgList := CreateValueList(S);
  for n := 0 to ArgList.Count - 1 do
  begin
    List := CreateValueList(ArgList[n]);
    Arguments.Add(List.Values['name'] + '=' + List.Values['value']);
    FreeAndNil(List);
  end;
  FreeAndNil(ArgList);
  
  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-frames %d %d', [AIndex, AIndex], S);
  List := CreateValueList(S);   
  S := List.Values['stack'];
  FreeAndNil(List);
  List := CreateValueList(S);   
  S := List.Values['frame'];
  FreeAndNil(List);
  List := CreateValueList(S);   
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
      TGDBMIDebugger(Debugger).ExecuteCommand('-stack-info-depth', S);
      List := CreateValueList(S);
      FCount := StrToIntDef(List.Values['depth'], 0);
      FreeAndNil(List);
    end;
  end;
  
  Result := FCount;
end;

end.
{ =============================================================================
  $Log$
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

{ $Id$ }
{                        ----------------------------------------------  
                         GDBDebugger.pp  -  Debugger class forGDB
                         ---------------------------------------------- 
 
 @created(Wed Feb 23rd WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains debugger class for the GDB/MI debugger.
 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
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
    FTargetPID: Integer;
    function  FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
    procedure GDBRun; 
    procedure GDBPause;
    procedure GDBStart;  
    procedure GDBStop;  
    procedure GDBStepOver;         
    procedure GDBStepInto;          
    procedure GDBRunTo(const ASource: String; const ALine: Integer); 
    procedure GDBJumpTo(const ASource: String; const ALine: Integer);
    function  ProcessResult(const AIgnoreError: Boolean; var AResultValues: String): Boolean;
    function  ProcessRunning: Boolean;
    function  ProcessStopped(const AParams: String): Boolean;
    function  ExecuteCommand(const ACommand: String): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; var AResultValues: String): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; var AResultValues: String): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; AValues: array of const; const AIgnoreError: Boolean; var AResultValues: String): Boolean; overload;
  protected
    function  ChangeFileName: Boolean; override;
    function  CreateBreakPoints: TDBGBreakPoints; override;
    function  CreateWatches: TDBGWatches; override;
    function  GetSupportedCommands: TDBGCommands; override;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
  public
    constructor Create; {override;}
    destructor Destroy; override;
  
    procedure Init; override;         // Initializes external debugger
    procedure Done; override;         // Kills external debugger
    // internal testing
    procedure TestCmd(const ACommand: String); override;
  end;


implementation

uses
  SysUtils, Dialogs;
  
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
  
  TGDBMIWatch = class(TDBGWatch)
  private
  protected
    procedure DoEnableChange; override;
    function  GetValue: String; override;
    function  GetValid: Boolean; override;
    procedure SetExpression(const AValue: String); override;
    procedure SetValue(const AValue: String); override;
  public
  end;

function CreateValueList(AResultValues: String): TStringList;
var
  n: Integer;
  InString: Boolean;
  InList: Integer;
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
  while (n <= Length(AResultValues)) do
  begin
    if InString 
    then begin
      if AResultValues[n] = '"' 
      then begin
        InString := False;
        Delete(AResultValues, n, 1);
        Continue;
      end;
    end
    else begin
      if InList > 0 
      then begin
        if AResultValues[n] in [']', '}'] 
        then Dec(InList);
      end
      else begin
        if AResultValues[n] = ','
        then begin
          Result.Add(Copy(AResultValues, 1, n - 1));
          Delete(AResultValues, 1, n);
          n := 1;
          Continue;
        end 
        else if AResultValues[n] = '"'
        then begin
          InString := True;
          Delete(AResultValues, n, 1);
          Continue;
        end;
      end;
      if AResultValues[n] in ['[', '{'] 
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
var
  S: String;
begin
  SendCmdLn('-file-exec-and-symbols %s', [FileName]);
  S := ReadLine(True);
  FHasSymbols := Pos('no debugging symbols', S) = 0;
  if not FHasSymbols
  then WriteLN('WARNING: File ''',FileName, ''' has no debug symbols');  
  Result := ProcessResult(True, S) and inherited ChangeFileName;   
  
  if Result 
  then begin
    ExecuteCommand('set extention-language .lpr pascal');
    ExecuteCommand('set extention-language .lrc pascal');
    ExecuteCommand('set extention-language .dpr pascal');
    ExecuteCommand('set extention-language .pas pascal');
    ExecuteCommand('set extention-language .pp pascal');
    ExecuteCommand('set extention-language .inc pascal');
  end;
end;

constructor TGDBMIDebugger.Create;
begin
  FCommandQueue := TStringList.Create;
  FTargetPID := 0;
  inherited Create;
end;

function TGDBMIDebugger.CreateBreakPoints: TDBGBreakPoints; 
begin
  Result := TDBGBreakPoints.Create(Self, TGDBMIBreakPoint);
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
begin
  Result := ExecuteCommand(ACommand, [], S);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; var AResultValues: String): Boolean; 
begin
  Result := ExecuteCommand(ACommand, [], AResultValues);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; AValues: array of const): Boolean;
var
  S: String;
begin
  Result := ExecuteCommand(ACommand, AValues, S);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; AValues: array of const; var AResultValues: String): Boolean;
begin
  Result := ExecuteCommand(ACommand, AValues, False, AResultValues);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; AValues: array of const; const AIgnoreError: Boolean; var AResultValues: String): Boolean;
begin
  FCommandQueue.Add(ACommand);
  if FCommandQueue.Count > 1 then Exit;
  repeat
    SendCmdLn(FCommandQueue[0], AValues);
    Result := ProcessResult(AIgnoreError, AResultValues) and ((State <> dsRun) or ProcessRunning);
    FCommandQueue.Delete(0);
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

procedure TGDBMIDebugger.GDBJumpTo(const ASource: String; const ALine: Integer);
begin
end;

procedure TGDBMIDebugger.GDBPause;
begin
  SendBreak(FTargetPID);
end;

procedure TGDBMIDebugger.GDBRun;
begin
  case State of 
    dsIdle, dsStop: begin
      GDBStart;
      if State = dsPause
      then begin
        ExecuteCommand('-exec-continue'); 
      end
      else begin
        //error???
      end;
    end;
    dsPause: begin
      ExecuteCommand('-exec-continue');
    end;
  end;
end;

procedure TGDBMIDebugger.GDBRunTo(const ASource: String; const ALine: Integer); 
begin
  if State in [dsRun, dsError] then Exit;
  
  ExecuteCommand('-exec-until %s:%d', [ASource, ALine]);
end;

procedure TGDBMIDebugger.GDBStart;
var
  S: String;
begin
  if State in [dsIdle, dsStop]
  then begin
    if FHasSymbols
    then begin
      ExecuteCommand('-break-insert -t main');
      ExecuteCommand('-exec-run');
      
      // try to find PID
      SendCmdLn('info program', []);
      ReadLine; // skip repeated command
      S := ReadLine;
      FTargetPID := StrToIntDef(GetPart('child process ', '.', S), 0);
      if ProcessResult(False, S) 
      then SetState(dsPause);
    end;
  end;
end;

procedure TGDBMIDebugger.GDBStepInto;          
begin
  case State of 
    dsIdle, dsStop: begin
      GDBStart;
    end;
    dsPause: begin
      ExecuteCommand('-exec-step');
    end;
  end;
end;

procedure TGDBMIDebugger.GDBStepOver;         
begin
  case State of 
    dsIdle, dsStop: begin
      GDBStart;
    end;
    dsPause: begin
      ExecuteCommand('-exec-next');
    end;
  end;
end;

procedure TGDBMIDebugger.GDBStop;  
begin
  if State = dsRun 
  then GDBPause;     

  if State = dsPause 
  then begin
    // not supported yet
    // ExecuteCommand('-exec-abort');
    ExecuteCommand('kill');
    SetState(dsStop); //assume stop until abort is supported;
  end;
end;

function TGDBMIDebugger.GetSupportedCommands: TDBGCommands; 
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak{, dcWatch}]
end;

procedure TGDBMIDebugger.Init;
begin
  if CreateDebugProcess('/usr/bin/gdb -silent -i mi')
  then begin  
    ReadLine;  //flush first line
    ExecuteCommand('-gdb-set confirm off');
    ExecuteCommand('-gdb-set language pascal');
    inherited Init;
  end 
  else begin
    if DebugProcess = nil
    then MessageDlg('Debugger', 'Failed to create debug process for unknown reason', mtError, [mbOK], 0)
    else MessageDlg('Debugger', Format('Failed to create debug process: %s', [ReadLine]), mtError, [mbOK], 0);
    SetState(dsError);
  end;
end;

function TGDBMIDebugger.ProcessResult(const AIgnoreError: Boolean; var AResultValues: String): Boolean;
var
  S: String;
begin
  Result := False;
  S := StripLN(ReadLine);
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
            SetState(dsRun);
          end
          else if S = 'error'
          then begin
            Result := True;
            if not AIgnoreError
            then SetState(dsError);
          end
          else if S = 'exit'
          then begin
            Result := True;
            SetState(dsIdle);
          end
          else WriteLN('[WARNING] Debugger: Unknown result class: ', S);
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
            ProcessStopped(S);
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
  procedure FrameToLocation(const AFrame: String);
  var
    Frame: TStringList;  
    Location: TDBGLocationRec;
  begin
    Frame := CreateValueList(AFrame);   
        
    Location.Adress := Pointer(StrToIntDef(Frame.Values['addr'], 0)); 
    Location.FuncName := Frame.Values['func'];
    Location.SrcFile := Frame.Values['file']; 
    Location.SrcLine := StrToIntDef(Frame.Values['line'], -1);
    
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
    FrameToLocation(List.Values['frame']);    
  end
  else if Reason = 'signal-received'
  then begin
    // TODO: check to run (un)handled
    // TODO: define signal no
    SetState(dsPause);
    S := List.Values['signal-name'];
    if S <> 'SIGINT'
    then DoException(0, S);
    FrameToLocation(List.Values['frame']);    
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
        FrameToLocation(List.Values['frame']);    
      end
      else begin
        ExecuteCommand('-exec-continue');
      end;
    end;
  end
  else if Reason = 'function-finished'
  then begin
    SetState(dsPause);
    FrameToLocation(List.Values['frame']);    
  end
  else if Reason = 'end-stepping-range'
  then begin
    SetState(dsPause);
    FrameToLocation(List.Values['frame']);    
  end
  else if Reason = 'location-reached'
  then begin
    SetState(dsPause);
    FrameToLocation(List.Values['frame']);    
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
    dcRun:      GDBRun;
    dcPause:    GDBPause; 
    dcStop:     GDBStop;
    dcStepOver: GDBStepOver; 
    dcStepInto: GDBStepInto;
    dcRunTo:    GDBRunTo(String(APArams[0].VAnsiString), APArams[1].VInteger);
    dcJumpto:   GDBJumpTo(String(APArams[0].VAnsiString), APArams[1].VInteger);
  end;
  Result := True;
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
  if FBreakID <> 0
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
  if FBreakID = 0 then Exit;
  
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
  then; //TODO
  if bpaDisableGroup in Actions
  then; //TODO
end;
  
procedure TGDBMIBreakPoint.SetBreakpoint;
var
  S: String;
  ResultList, BkptList: TStringList;  
begin
  TGDBMIDebugger(Debugger).ExecuteCommand('-break-insert %s:%d', [Source, Line], True, S);
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
  if TGDBMIDebugger(Debugger).State in [dsStop, dsPause, dsIdle]
  then SetBreakpoint;
end;

{ =========================================================================== }
{ TGDBMIWatch }
{ =========================================================================== }

procedure TGDBMIWatch.DoEnableChange; 
begin
end;

function TGDBMIWatch.GetValue: String; 
begin
  if (Debugger.State in [dsStop, dsPause, dsIdle])
  and Valid 
  then begin
  end
  else Result := inherited GetValue;
end;

function TGDBMIWatch.GetValid: Boolean; 
begin
  Result:=false;
end;

procedure TGDBMIWatch.SetExpression(const AValue: String); 
begin
  if  (AValue <> Expression)
  and (Debugger.State in [dsStop, dsPause, dsIdle])
  then begin
    //TGDBMIDebugger(Debugger).SendCmdLn('', True);
  end;
end;

procedure TGDBMIWatch.SetValue(const AValue: String); 
begin
end;

end.
{ =============================================================================
  $Log$
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

{ $Id$ }
{                        ----------------------------------------------  
                         GDBDebugger.pp  -  Debugger class forGDB
                         ---------------------------------------------- 
 
 @created(Wed Feb 28st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains the Commandline debugger class for the GDB
 debugger.
 
 
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
unit GDBDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Process, LCLProc, Debugger, CmdLineDebugger;


type                       
  TGDBProgramInfo = record
    State: TDBGState;
    BreakPoint: Integer; // ID of Breakpoint hit
    Signal: Integer;     // Signal no if we hit one
    SignalText: String;  // Signal text if we hit one
  end;


  TGDBDebugger = class(TCmdLineDebugger)
  private
    FHasSymbols: Boolean;
    function  FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
    procedure GDBRun; 
    procedure GDBPause;
    procedure GDBStart;  
    procedure GDBStop;  
    procedure GDBStepOver;         
    procedure GDBStepInto;          
    procedure GDBRunTo(const ASource: String; const ALine: Integer); 
    procedure GDBJumpTo(const ASource: String; const ALine: Integer);
    function  SendCommand(const ACommand: String;
      Values: array of const): TStrings; // internally used by breakpoints and watches
    procedure RunCommand(const ACommand: String);
    function  GetLocation: TDBGLocationRec;
    function  GetProgramInfo(const AHandleResult: Boolean): TGDBProgramInfo;
  protected
    function  CreateBreakPoints: TDBGBreakPoints; override;
    function  CreateWatches: TDBGWatches; override;
    function  GetSupportedCommands: TDBGCommands; override;
    procedure SetFileName(const AValue: String); override;
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
  SysUtils;
  
type
  TGDBBreakPoint = class(TDBGBreakPoint)
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
  
  TGDBWatch = class(TDBGWatch)
  private
  protected
    procedure DoEnableChange; override;
    function  GetValue: String; override;
    function  GetValid: Boolean; override;
    procedure SetExpression(const AValue: String); override;
    procedure SetValue(const AValue: String); override;
  public
  end;
  
{ =========================================================================== }
{ TGDBDebugger }
{ =========================================================================== }

constructor TGDBDebugger.Create;
begin
  inherited Create;
end;

function TGDBDebugger.CreateBreakPoints: TDBGBreakPoints; 
begin
  Result := TDBGBreakPoints.Create(Self, TGDBBreakPoint);
end;

function TGDBDebugger.CreateWatches: TDBGWatches; 
begin
  Result := TDBGWatches.Create(Self, TGDBWatch);
end;

destructor TGDBDebugger.Destroy;
begin
  inherited;
end;

procedure TGDBDebugger.Done;
begin
  if State = dsRun then GDBPause;
  SendCmdLn('quit', False);
  inherited Done;
end;

function  TGDBDebugger.FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
var
  n: Integer;
begin                     
  if  ABreakpoint <> 0 
  then
    for n := 0 to Breakpoints.Count - 1 do
    begin
      Result := Breakpoints[n];
      if TGDBBreakpoint(Result).FBreakID = ABreakpoint
      then Exit;
    end;
  Result := nil;
end;

procedure TGDBDebugger.GDBJumpTo(const ASource: String; const ALine: Integer);
begin
end;

procedure TGDBDebugger.GDBPause;
begin
  SendBreak(TargetProcess.Handle);
end;

procedure TGDBDebugger.GDBRun;
var
  dState: TDBGState;
begin
  case State of 
    dsIdle, dsStop: begin
      GDBStart;
      dState := GetProgramInfo(False).State;
      if dState = dsPause
      then begin
        RunCommand('cont'); 
      end
      else begin
        DoCurrent(GetLocation);
        SetState(dState);
      end;
    end;
    dsPause: begin
      RunCommand('cont');
    end;
  end;
end;

procedure TGDBDebugger.GDBRunTo(const ASource: String; const ALine: Integer); 
begin
  if State in [dsRun, dsError] then Exit;
  
  SendCmdLn('tbreak %s:%d', [ASource, ALine], True);
  GDBRun;
end;

procedure TGDBDebugger.GDBStart;
var
  loc: TDBGLocationRec;
  dState: TDBGState;
  StopAdress: Integer;
  //idx:  Integer;
  S: String;
begin
  if State in [dsIdle, dsStop]
  then begin
    CreateTargetProcess(FileName);
    
    SendCommand('attach %d', [TargetProcess.Handle]);
    TargetProcess.Resume;
    SendCmdLn('cont', True);
    
    S := SendCommand('tbreak main', [TargetProcess.Handle]).Text;


    if Pos('Breakpoint', S) > 0
    then begin
//      TargetProcess.Resume;
      SetState(dsRun);
      StopAdress := -1;
    end
    else begin
      // No symbols or no main
      // Try to set a TBreak at first adress
      S := SendCommand('info files', [TargetProcess.Handle]).Text;
      StopAdress := StrToIntDef(StripLN('$' + GetPart('Entry point: 0x', '', S)), -1);
      if StopAdress <> -1
      then begin
        S := SendCommand('x/2i 0x%x', [StopAdress]).Text;   
        GetLine(S); //ignore first line
        S := GetPart('0x', ':', S);
        StopAdress := StrToIntDef(StripLN('$' + GetPart('', ' ', S)), -1);
        if StopAdress <> -1
        then begin
          SendCommand('tbreak *0x%x', [StopAdress]);
        end;
      end;
      
//      TargetProcess.Resume;
      SetState(dsRun);
    end;

    repeat
      SendCmdLn('cont', True);
      loc := GetLocation;
      dState := GetProgramInfo(False).State;
    until (loc.FuncName = 'main') or (Integer(loc.Adress) = StopAdress) or (dState <> dsPause);
  end;
end;

procedure TGDBDebugger.GDBStepInto;          
begin
  case State of 
    dsIdle, dsStop: begin
      GDBStart;
      GetProgramInfo(True);
    end;
    dsPause: begin
      RunCommand('step');
    end;
  end;
end;

procedure TGDBDebugger.GDBStepOver;         
begin
  case State of 
    dsIdle, dsStop: begin
      GDBStart;
      GetProgramInfo(True);
    end;
    dsPause: begin
      RunCommand('next');
    end;
  end;
end;

procedure TGDBDebugger.GDBStop;  
var
  dState: TDBGState;
begin
  if State = dsRun 
  then begin
    GDBPause;     
    // wait till pause is executed
    SendCmdLn('', True);
  end;

  dState := GetProgramInfo(False).State;
  if dState = dsPause 
  then begin
    SendCmdLn('kill', True);
    dState := GetProgramInfo(False).State;   
  end;

  if dState = dsStop
  then KillTargetProcess;
  SetState(dState);
end;

function TGDBDebugger.GetLocation: TDBGLocationRec;
var              
  n, idx: Integer;
  NoSrc: Boolean;
  S: String;
begin
  Result.Adress := nil;
  Result.FuncName := '';
  Result.SrcFile := '';
  Result.SrcLine := -1;
  
  SendCmdLn('info frame', True);
  for n := 0 to OutputLines.Count - 1 do
  begin
    S := OutputLines[n];
    idx := Pos('eip = 0x', S);
    if idx = 0 then Continue;

    // Get addr
    Delete(S, 1, idx + 7);
    idx := Pos('in', S);
    if idx = 0 then Break;
    Result.Adress := Pointer(StrToIntDef('$' + Copy(S, 1, idx - 2), 0));
    
    // get function
    Delete(S, 1, idx + 2);
    idx := Pos(' (', S);
    NoSrc := (idx = 0);
    if NoSrc
    then idx := Pos(';',S);
    Result.FuncName := Copy(S, 1, idx - 1);
    if NoSrc then Break;
    
    // getsource info
    Delete(S, 1, idx + 1);
    idx := Pos(':', S);
    if idx = 0 then Break;
    Result.SrcFile := Copy(S, 1, idx - 1);
    Delete(S, 1, idx);
    idx := Pos(')', S);
    if idx = 0 then Break;
    Result.SrcLine := StrToIntDef(Copy(S, 1, idx - 1), 0);
  end;
end;

function TGDBDebugger.GetProgramInfo(const AHandleResult: Boolean): TGDBProgramInfo;
var
  S, Signal: String;
  BreakPoint: TGDBBreakPoint;
begin                     
  // Loop since we might have hit a non-break breakpoint              
  while True do  
  begin
    Result.Breakpoint := 0;  
    Result.Signal := 0;
    Result.SignalText := '';
    Result.State := dsNone;
                
    SendCmdLn('info program', True);
    S := OutputLines.Text;
    if Pos('stopped', S) > 0 
    then begin
      Result.State := dsPause;
      if Pos('breakpoint ', S) > 0
      then begin
        Result.Breakpoint := StrToIntDef(GetPart('breakpoint ', '.', S), 0);
      end
      else if Pos('signal ', S) > 0
      then begin
        Signal := GetPart('signal ', ',', S);
        // TODO: translate to id
        Result.SignalText := GetPart(' ', '.', S);
      end;
    end
    else if Pos('not being run', S) > 0 
    then Result.State := dsStop;
    
    if AHandleResult
    then begin
      if Result.Breakpoint <> 0
      then begin
        BreakPoint := TGDBBreakPoint(FindBreakPoint(Result.Breakpoint));
        if BreakPoint <> nil
        then begin
          BreakPoint.Hit;
          
          if not (bpaStop in BreakPoint.Actions)
          then begin
            SendCmdLn('cont', True);
            Continue;
          end;
        end;
      end;
      SetState(Result.State);
      DoCurrent(GetLocation);

      if Result.SignalText <> ''
      then DoException(Result.Signal, Result.SignalText);
    end;
    Break;
  end;
end;

function TGDBDebugger.GetSupportedCommands: TDBGCommands; 
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak{, dcWatch}]
end;

procedure TGDBDebugger.Init;
begin
  WaitPrompt := '(gdb) ';
  if DebugProcess = nil 
  then begin
    CreateDebugProcess('/usr/bin/gdb -silent');
    SendCmdLn('', True);
  end;
  SendCmdLn('set confirm off', True);
  inherited Init;
end;
    
function TGDBDebugger.RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; 
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

procedure TGDBDebugger.RunCommand(const ACommand: String);
begin
  SetState(dsRun);
  SendCmdLn(ACommand, True);
  GetProgramInfo(True);
end;

function TGDBDebugger.SendCommand(const ACommand: String;
  Values: array of const): TStrings;
begin
  SendCmdLn(ACommand, Values, True);
  Result := OutputLines;
end;

procedure TGDBDebugger.SetFileName(const AValue: String); 
begin                            
  if AValue <> FileName
  then begin
    GDBStop;
    if AValue <> ''
    then begin
      SendCmdLn('file "%s"', [AValue], True);
      FHasSymbols := Pos('no debugging symbols', OutputLines.Text) = 0;
      if not FHasSymbols
      then DebugLn('WARNING: File ''',AValue, ''' has no debug symbols');
    end;
    inherited; 
  end;
end;


procedure TGDBDebugger.TestCmd(const ACommand: String);
begin
  SetState(dsRun);
  inherited TestCmd(ACommand);
  GetProgramInfo(True);
end;

{ =========================================================================== }
{ TGDBBreakPoint }
{ =========================================================================== }

constructor TGDBBreakPoint.Create(ACollection: TCollection); 
begin
  inherited Create(ACollection);
  FBreakID := 0;
end;

destructor TGDBBreakPoint.Destroy; 
begin
  if FBreakID <> 0
  then begin
    TGDBDebugger(Debugger).SendCommand('delete %d', [FBreakID]);
  end;
  
  inherited Destroy;
end;

procedure TGDBBreakPoint.DoActionChange;     
begin
  Changed(False);
end;

procedure TGDBBreakPoint.DoEnableChange;
const
  CMD: array[Boolean] of String = ('disable', 'enable');
begin
  if FBreakID = 0 then Exit;
  
  TGDBDebugger(Debugger).SendCommand('%s %d', [CMD[Enabled], FBreakID]);
  Changed(false);
end;

procedure TGDBBreakPoint.DoExpressionChange; 
begin
  Changed(False);
end;

procedure TGDBBreakPoint.DoStateChange; 
begin   
  inherited DoStateChange;
  if  (Debugger.State = dsStop)
  and (FBreakID = 0)
  then SetBreakpoint;
end;

procedure TGDBBreakPoint.Hit;
begin
  SetHitCount(HitCount + 1);

  if bpaEnableGroup in Actions
  then; //TODO
  if bpaDisableGroup in Actions
  then; //TODO
end;
  
procedure TGDBBreakPoint.SetBreakpoint;
var
  idx: Integer;
  S: String;
begin
  S := TGDBDebugger(Debugger).SendCommand('break %s:%d', [Source, Line])[0];
  idx := Pos(' at', S);
  if idx >0 
  then begin
    FBreakID := StrToIntDef(Copy(S, 12, idx - 12), 0);
  end;
  SetValid(FBreakID <> 0);
  DoEnableChange;
end;

  
procedure TGDBBreakPoint.SetLocation(const ASource: String; const ALine: Integer); 
begin  
  if (FSource = ASource) and (FLine = ALine) then exit;
  inherited;
  if TGDBDebugger(Debugger).State in [dsStop, dsPause, dsIdle]
  then SetBreakpoint;
end;

{ =========================================================================== }
{ TGDBWatch }
{ =========================================================================== }

procedure TGDBWatch.DoEnableChange; 
begin
end;

function TGDBWatch.GetValue: String; 
begin
  if (Debugger.State in [dsStop, dsPause, dsIdle])
  and Valid 
  then begin
  end
  else Result := inherited GetValue;
end;

function TGDBWatch.GetValid: Boolean; 
begin
  Result:=false;
end;

procedure TGDBWatch.SetExpression(const AValue: String); 
begin
  if  (AValue <> Expression)
  and (Debugger.State in [dsStop, dsPause, dsIdle])
  then begin
    //TGDBDebugger(Debugger).SendCmdLn('', True);
  end;
end;

procedure TGDBWatch.SetValue(const AValue: String); 
begin
end;

end.

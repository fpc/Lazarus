{ $Id$ }
{                        ----------------------------------------------  
                         GDBDebugger.pp  -  Debugger class forGDB
                         ---------------------------------------------- 
 
 @created(Wed Feb 28st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains the Commandline debugger class for the GDB
 debugger.
 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 
unit GDBDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Process, Debugger, CmdLineDebugger, DBGBreakPoint;

type                       

  TGDBDebugger = class(TCmdLineDebugger)
  private
    procedure GDBRun; 
    procedure GDBPause;
    procedure GDBStop;  
    procedure GDBStepOver;         
    procedure GDBStepInto;          
    procedure GDBRunTo(const ASource: String; const ALine: Integer); 
    procedure GDBJumpTo(const ASource: String; const ALine: Integer);
    procedure RunCommand(const ACommand: String);
    function  GetGDBState: TDBGState;
    function  GetLocation: TDBGLocationRec;
  protected
    procedure BreakActionChange(const ABreakPoint: TDBGBreakpoint); override;
    procedure BreakAdd(const ABreakPoint: TDBGBreakpoint); override;
    procedure BreakEnableChange(const ABreakPoint: TDBGBreakpoint); override;
    procedure BreakExpressionChange(const ABreakPoint: TDBGBreakpoint); override;
    procedure BreakRemove(const ABreakPoint: TDBGBreakpoint); override;

    function  GetFlags: TDBGCommands; override;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  
    procedure Init; override;         // Initializes external debugger
    procedure Done; override;         // Kills external debugger
    // internal testing
    procedure TestCmd(const ACommand: String); override;
  end;


implementation

uses
  SysUtils;
  
{ TGDBDebugger }

procedure TGDBDebugger.BreakActionChange(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TGDBDebugger.BreakAdd(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TGDBDebugger.BreakEnableChange(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TGDBDebugger.BreakExpressionChange(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TGDBDebugger.BreakRemove(const ABreakPoint: TDBGBreakpoint); 
begin
end;

constructor TGDBDebugger.Create;
begin
  inherited Create;
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

procedure TGDBDebugger.GDBJumpTo(const ASource: String; const ALine: Integer);
begin
end;

procedure TGDBDebugger.GDBPause;
begin
  SendBreak(TargetProcess.Handle);
end;

procedure TGDBDebugger.GDBRun; 
var
  loc: TDBGLocationRec;
  dState: TDBGState;
begin
  case State of 
    dsIdle, dsStop: begin
      CreateTargetProcess(FileName);
      
      SendCmdLn('file %s', [FileName], True);
      SendCmdLn('attach %d', [TargetProcess.Handle], True);
    
      TargetProcess.Resume;
      SetState(dsRun);
      
      repeat
        SendCmdLn('cont', True);
        loc := GetLocation;
        dState := GetGDBState;
      until ((loc.FuncName <> 'HEAP') and (loc.FuncName <> '_start')) or (dState <> dsPause);
      DoCurrent(loc);
      SetState(dState);
    end;
    dsPause: begin
      RunCommand('cont');
    end;
  end;
end;

procedure TGDBDebugger.GDBRunTo(const ASource: String; const ALine: Integer); 
begin
end;

procedure TGDBDebugger.GDBStepInto;          
begin
  RunCommand('step');
end;

procedure TGDBDebugger.GDBStepOver;         
begin
  RunCommand('next');
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

  dState := GetGDBState;
  if dState <> dsPause 
  then Exit;

  SendCmdLn('kill', True);
  dState := GetGDBState;   

  if dState = dsStop
  then KillTargetProcess;
  SetState(dState);
end;

function TGDBDebugger.GetFlags: TDBGCommands;
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak]
end;

function TGDBDebugger.GetGDBState: TDBGState;
var
  S: String;
begin
  SendCmdLn('info program', True);
  S := OutputLines.Text;
  WriteLn('Info: ',S);
  if Pos('stopped', S) > 0 
  then Result := dsPause
  else if Pos('not being run', S) > 0 
  then Result := dsStop
  else Result := dsNone;
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
  DoCurrent(GetLocation);
  SetState(GetGDBState);
end;

procedure TGDBDebugger.TestCmd(const ACommand: String);
begin
  SetState(dsRun);
  inherited TestCmd(ACommand);
  DoCurrent(GetLocation);
  SetState(GetGDBState);
end;

end.
{ =============================================================================
  $Log$
  Revision 1.2  2001/11/06 23:59:13  lazarus
  MWE: + Initial breakpoint support
       + Added exeption handling on process.free

  Revision 1.1  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.


}

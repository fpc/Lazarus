{ $Id$ }
{                        ----------------------------------------  
                           Debugger.pp  -  Debugger base classes
                         ---------------------------------------- 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the base class definitions of the debugger. These
 classes are only definitions. Implemented debuggers should be
 derived from these. 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 
unit Debugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, DBGWatch, DBGBreakpoint;

type
  TDBGLocationRec = record
    Adress: Pointer;
    FuncName: String;
    SrcFile: String;
    SrcLine: Integer;
  end;

  TDBGCommand = (dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch);
  TDBGCommands = set of TDBGCommand;
  TDBGState = (dsNone, dsIdle, dsStop, dsPause, dsRun, dsError);
  
(*
  Debugger states
  --------------------------------------------------------------------------
  dsNone: 
    The debug object is created, but no instance of an external debugger 
    exists.
    Initial state, leave with Init, enter with Done
  
  dsIdle:
    The external debugger is started, but no filename (or no other params 
    requred to start) were given.
    
  dsStop:
    (Optional) The execution of the target is stopped
    The external debugger is loaded and ready to (re)start the execution 
    of the target.
    Breakpoints, wathes etc can be defined
    
  dsPause:
    De debugger has paused the target. Targer variables canbe examined
    
  dsRun:
    The target is running.
    
  dsError:
    Something unforseen has happened. A shutdown of the debugger is in
    most cases needed.
  --------------------------------------------------------------------------
  
*)  

  TDBGOutputEvent = procedure(Sender: TObject; const AText: String) of object;
  TDBGCurrentLineEvent = procedure(Sender: TObject; const ALocation: TDBGLocationRec) of object;

  TDebugger = class(TObject)
  private
    FArguments: String;
    FBreakPoints: TDBGBreakPoints;
    FBreakPointGroups: TDBGBreakPointGroups;
    FFileName: String;                              
    FState: TDBGState;
    FWatches: TDBGWatches;
    FOnCurrent: TDBGCurrentLineEvent;
    FOnOutput: TDBGOutputEvent;
    FOnDbgOutput: TDBGOutputEvent;
    FOnState: TNotifyEvent;
    function  GetState: TDBGState;              
    function  ReqCmd(const ACommand: TDBGCommand; const AParams: array of const): Boolean; 
  protected  
    function  CreateBreakPoints: TDBGBreakPoints; virtual;
    function  CreateWatches: TDBGWatches; virtual;
    procedure DoCurrent(const ALocation: TDBGLocationRec);
    procedure DoDbgOutput(const AText: String);
    procedure DoOutput(const AText: String);
    procedure DoState;
    function  GetCommands: TDBGCommands; 
    function  GetSupportedCommands: TDBGCommands; virtual;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; virtual; abstract; // True if succesful
    procedure SetFileName(const AValue: String); virtual;
    procedure SetState(const AValue: TDBGState); 
  public
    constructor Create; virtual;
    destructor Destroy; override;
    
    procedure Init; virtual;                         // Initializes the debugger
    procedure Done; virtual;                         // Kills the debugger
    procedure Run;                                   // Starts / continues debugging
    procedure Pause;                                 // Stops running
    procedure Stop;                                  // quit debugging
    procedure StepOver;         
    procedure StepInto;          
    procedure RunTo(const ASource: String; const ALine: Integer); virtual;     // Executes til a certain point
    procedure JumpTo(const ASource: String; const ALine: Integer); virtual;    // No execute, only set exec point
    property SupportedCommands: TDBGCommands read GetSupportedCommands;        // All available commands of the debugger
    property Arguments: String read FArguments write FArguments;               // Arguments feed to the program
    property BreakPoints: TDBGBreakPoints read FBreakPoints;                   // list of all breakpoints
    property BreakPointGroups: TDBGBreakPointGroups read FBreakPointGroups;    // list of all breakpointgroups
    property Commands: TDBGCommands read GetCommands;                          // All current available commands of the debugger
    property FileName: String read FFileName write SetFileName;                // The name of the exe to be debugged
    property State: TDBGState read FState;                                     // The current stete of the debugger    
    property Watches: TDBGWatches read FWatches;                               // list of all watches localvars etc
    property OnCurrent: TDBGCurrentLineEvent read FOnCurrent write FOnCurrent; // Passes info about the current line being debugged
    property OnState: TNotifyEvent read FOnState write FOnState;               // Fires when the current state of the debugger changes
    property OnOutput: TDBGOutputEvent read FOnOutput write FOnOutput;         // Passes all output of the debugged target
    property OnDbgOutput: TDBGOutputEvent read FOnDbgOutput write FOnDbgOutput;// Passes all debuggeroutput        
  end;

implementation

uses
  SysUtils;
  
const
  COMMANDMAP: array[TDBGState] of TDBGCommands = (
  {dsNone } [], 
  {dsIdle } [],
  {dsStop } [dcRun, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch],
  {dsPause} [dcRun, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch],
  {dsRun  } [dcPause, dcStop, dcBreak, dcWatch],
  {dsError} []
  );

{ TDebugger }

constructor TDebugger.Create;
begin
  inherited Create;
  FOnState := nil;
  FOnCurrent := nil;
  FOnOutput := nil;
  FOnDbgOutput := nil;
  FState := dsNone;
  FArguments := '';
  FFilename := '';
  FBreakPoints := CreateBreakPoints;
  FWatches := CreateWatches;
  FBreakPointGroups := TDBGBreakPointGroups.Create;
end;

function TDebugger.CreateBreakPoints: TDBGBreakPoints; 
begin                                                            
  Result := TDBGBreakPoints.Create(Self, TDBGBreakPoint);
end;

function TDebugger.CreateWatches: TDBGWatches; 
begin         
  Result := TDBGWatches.Create(Self, TDBGWatch);
end;

destructor TDebugger.Destroy;
begin
  // don't call events
  FOnState := nil;
  FOnCurrent := nil;
  FOnOutput := nil;
  FOnDbgOutput := nil;

  if FState <> dsNone
  then Done;
  
  FBreakPointGroups.Free;
  FWatches.Free;
  inherited;
end;

procedure TDebugger.Done;
begin
  SetState(dsNone);
end;

procedure TDebugger.DoCurrent(const ALocation: TDBGLocationRec);
begin
  if Assigned(FOnCurrent) then FOnCurrent(Self, ALocation);
end;

procedure TDebugger.DoDbgOutput(const AText: String);
begin
  if Assigned(FOnDbgOutput) then FOnDbgOutput(Self, AText);
end;

procedure TDebugger.DoOutput(const AText: String);
begin
  if Assigned(FOnOutput) then FOnOutput(Self, AText);
end;

procedure TDebugger.DoState;
begin
  if Assigned(FOnState) then FOnState(Self);
end;

function TDebugger.GetCommands: TDBGCommands; 
begin
  Result := COMMANDMAP[State] * GetSupportedCommands;
end;

function TDebugger.GetState: TDBGState;
begin
  Result := FState;
end;

function TDebugger.GetSupportedCommands: TDBGCommands; 
begin
  Result := [];
end;

procedure TDebugger.Init;
begin
  SetState(dsIdle);
end;

procedure TDebugger.JumpTo(const ASource: String; const ALine: Integer);
begin
  ReqCmd(dcJumpTo, [ASource, ALine]);
end;

procedure TDebugger.Pause;
begin
  ReqCmd(dcPause, []);
end;

function TDebugger.ReqCmd(const ACommand: TDBGCommand; const AParams: array of const): Boolean; 
begin
  if FState = dsNone then Init;
  if ACommand in Commands 
  then Result := RequestCommand(ACommand, AParams)
  else Result := False;
end;

procedure TDebugger.Run;
begin
  ReqCmd(dcRun, []);
end;

procedure TDebugger.RunTo(const ASource: String; const ALine: Integer);
begin
  ReqCmd(dcRunTo, [ASource, ALine]);
end;

procedure TDebugger.SetFileName(const AValue: String);
begin
  if FFileName <> AValue
  then begin 
    if FState in [dsRun, dsPause]
    then Stop;
    FFileName := AValue;
    if FFilename = ''
    then SetState(dsIdle)
    else SetState(dsStop); 
  end;
end;

procedure TDebugger.SetState(const AValue: TDBGState); 
begin
  if AValue <> FState 
  then begin
    FState := AValue;
    DoState;
  end;
end;

procedure TDebugger.StepInto;
begin
  ReqCmd(dcStepInto, []);
end;

procedure TDebugger.StepOver;
begin
  ReqCmd(dcStepOver, []);
end;

procedure TDebugger.Stop;
begin
  ReqCmd(dcStop, []);
end;

end.
{ =============================================================================
  $Log$
  Revision 1.6  2002/02/05 23:16:48  lazarus
  MWE: * Updated tebugger
       + Added debugger to IDE

  Revision 1.5  2001/11/12 19:28:23  lazarus
  MG: fixed create, virtual constructors makes no sense

  Revision 1.4  2001/11/06 23:59:13  lazarus
  MWE: + Initial breakpoint support
       + Added exeption handling on process.free

  Revision 1.3  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.

  Revision 1.2  2001/10/18 13:01:31  lazarus
  MG: fixed speedbuttons numglyphs>1 and started IDE debugging

  Revision 1.1  2001/02/28 22:09:15  lazarus
  MWE:
    * Renamed DBGDebugger to Debugger

  Revision 1.2  2001/02/25 16:44:57  lazarus
  MWE:
    + Added header and footer

}

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

  TDBGCommand = (dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak);
  TDBGCommands = set of TDBGCommand;
  TDBGState = (dsNone, dsIdle, dsStop, dsPause, dsRun, dsError);

  TDBGOutputEvent = procedure(Sender: TObject; const AText: String) of object;
  TDBGCurrentLineEvent = procedure(Sender: TObject; const ALocation: TDBGLocationRec) of object;

  TDebugger = class(TObject)
  private
    FArguments: String;
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
    procedure DoCurrent(const ALocation: TDBGLocationRec);
    procedure DoDbgOutput(const AText: String);
    procedure DoOutput(const AText: String);
    procedure DoState;
    function  GetFlags: TDBGCommands; virtual;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; virtual; abstract; // True if succesful
    procedure SetFileName(const Value: String); virtual;
    procedure SetState(const Value: TDBGState); 
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
    property Arguments: String read FArguments write FArguments;               // Arguments feed to the program
    property BreakPointGroups: TDBGBreakPointGroups read FBreakPointGroups;    // list of all breakpoints
    property FileName: String read FFileName write SetFileName;                // The name of the exe to be debugged
    property Flags: TDBGCommands read GetFlags;                                // All available commands of the debugger
    property State: TDBGState read FState;                                     // The current stete of the debugger    
    property Watches: TDBGWatches read FWatches;                               // list of all watches localvars etc
    property OnCurrent: TDBGCurrentLineEvent read FOnCurrent write FOnCurrent; // Passes info about the current line being debugged
    property OnState: TNotifyEvent read FOnState write FOnState;               // Fires when the current state of the debugger changes
    property OnOutput: TDBGOutputEvent read FOnOutput write FOnOutput;         // Passes all output of the debugged target
    property OnDbgOutput: TDBGOutputEvent read FOnDbgOutput write FOnDbgOutput;// Passes all debuggeroutput        
  end;

  TInternalDebugger = class(TDebugger)
  private  
  protected
  public
    procedure BreakActionChange(const ABreakPoint: TDBGBreakpoint); virtual;
    procedure BreakAdd(const ABreakPoint: TDBGBreakpoint); virtual;
    procedure BreakEnableChange(const ABreakPoint: TDBGBreakpoint); virtual;
    procedure BreakExpressionChange(const ABreakPoint: TDBGBreakpoint); virtual;
    procedure BreakRemove(const ABreakPoint: TDBGBreakpoint); virtual;
  end;

implementation

uses
  SysUtils;
  
const
  COMMANDMAP: array[TDBGState] of TDBGCommands = (
  {dsNone } [dcBreak], 
  {dsIdle } [dcRun, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak],
  {dsStop } [dcRun, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak],
  {dsPause} [dcRun, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak],
  {dsRun  } [dcPause, dcStop, dcBreak],
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
  FBreakPointGroups := TDBGBreakPointGroups.Create(Self);
  FWatches := TDBGWatches.Create;
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

function TDebugger.GetState: TDBGState;
begin
  Result := FState;
end;

function TDebugger.GetFlags: TDBGCommands;
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
  if ACommand in (COMMANDMAP[FState] * Flags) 
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

procedure TDebugger.SetFileName(const Value: String);
begin
  FFileName := Value;
end;

procedure TDebugger.SetState(const Value: TDBGState); 
begin
  if Value <> FState 
  then begin
    FState := Value;
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

{ TInternalDebugger }

procedure TInternalDebugger.BreakActionChange(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TInternalDebugger.BreakAdd(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TInternalDebugger.BreakEnableChange(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TInternalDebugger.BreakExpressionChange(const ABreakPoint: TDBGBreakpoint); 
begin
end;

procedure TInternalDebugger.BreakRemove(const ABreakPoint: TDBGBreakpoint); 
begin
end;


end.
{ =============================================================================
  $Log$
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

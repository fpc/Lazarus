unit DBGDebugger;

{$mode objfpc}

interface

uses
  Classes, DBGWatch, DBGBreakpoint;

type
  TDBGCommandFlags = set of (dcfRun, dcfPause, dcfStop, dcfStepOver, dcfStepInto, dcfRunTo, dcfJumpto, dcfBreak);
  TDBGState = (dsStop, dsPause, dsRun, dsError);

  TDBGCurrentLineEvent = procedure(Sender: TObject; const ASource: String; const ALine: Integer) of object;

  TDebugger = class
  private
    FFileName: String;
    FBreakPointGroups: TDBGBreakPointGroups;
    FOnCurrent: TDBGCurrentLineEvent;
    FOnState: TNotifyEvent;
    FWatches: TDBGWatches;
  protected
    function  GetDBGState: TDBGState;           virtual;
    procedure SetFileName(const Value: String); virtual;
    function  GetFlags: TDBGCommandFlags;       virtual;
  public
    procedure Init; virtual;                         // Initializes external debugger
    procedure Done; virtual;                         // Kills external debugger
    procedure Run; virtual;                          // Starts / continues debugging
    procedure Pause; virtual;                        // Stops running
    procedure Stop; virtual;                         // quit debugging
    procedure StepOver; virtual;
    procedure StepInto; virtual;
    procedure RunTo(const ASource: String; const ALine: Integer); virtual;        // Executes til a certain point
    procedure JumpTo(const ASource: String; const ALine: Integer); virtual;       // No execute, only set exec point
    property BreakPointGroups: TDBGBreakPointGroups read FBreakPointGroups; // list of all breakpoints
    property FileName: String read FFileName write SetFileName;             // The name of the exe to be debugged
    property Flags: TDBGCommandFlags read GetFlags;                            // All available commands of the debugger
    property State: TDBGState read GetDBGState;
    property Watches: TDBGWatches read FWatches;   // list of all watches localvars etc
    property OnState: TNotifyEvent read FOnState write FOnState;          // Fires when the current state of the debugger changes
    property OnCurrent: TDBGCurrentLineEvent read FOnCurrent write FOnCurrent;        //Passes info about the current line being debugged
  end;


implementation

{ TDebugger }

procedure TDebugger.Done;
begin
end;


function TDebugger.GetDBGState: TDBGState;
begin
end;

function TDebugger.GetFlags: TDBGCommandFlags;
begin
end;

procedure TDebugger.Init;
begin
end;

procedure TDebugger.JumpTo(const ASource: String; const ALine: Integer);
begin
end;

procedure TDebugger.Pause;
begin
end;

procedure TDebugger.Run;
begin
end;

procedure TDebugger.RunTo(const ASource: String; const ALine: Integer);
begin
end;

procedure TDebugger.SetFileName(const Value: String);
begin
end;

procedure TDebugger.StepInto;
begin
end;

procedure TDebugger.StepOver;
begin
end;

procedure TDebugger.Stop;
begin
end;

end.

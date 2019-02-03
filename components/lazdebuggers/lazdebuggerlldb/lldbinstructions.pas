(*  This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2, 3 or any later version
    of the License (at your option).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)
unit LldbInstructions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, math, Classes, LazLoggerBase, DbgIntfDebuggerBase, DbgIntfBaseTypes,
  DebugInstructions, LldbHelper;

type

  (*
   *  Instructions
   *)

  TLldbInstruction = class;

  { TLldbInstructionQueue }

  TLldbInstructionQueue = class(TDBGInstructionQueue)
  private
  protected
    function CheckForIgnoredError(const AData: String): Boolean;
    procedure DoBeforeHandleLineReceived(var ALine: String); override;

    function GetSelectFrameInstruction(AFrame: Integer): TDBGInstruction; override;
    function GetSelectThreadInstruction(AThreadId: Integer): TDBGInstruction; override;
  public
    procedure CancelAllForCommand(ACommand: TObject); // Does NOT include the current or running instruction
  end;

  { TLldbInstruction }

  TLldbInstruction = class(TDBGInstruction)
  private
    FOwningCommand: TObject;
    function GetQueue: TLldbInstructionQueue;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
    procedure SetContentReceieved; reintroduce;

    property Queue: TLldbInstructionQueue read GetQueue;
    property NextInstruction;
  public
    property OwningCommand: TObject read FOwningCommand write FOwningCommand;
  end;

  { TLldbInstructionSettingSet }

  TLldbInstructionSettingSet = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AName, AValue: String; AGlobal: Boolean = False; AQuote: Boolean = False);
  end;

  { TLldbInstructionSettingRemove }

  TLldbInstructionSettingRemove = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AName, AValue: String; AQuote: Boolean = False);
  end;

  { TLldbInstructionSettingClear }

  TLldbInstructionSettingClear = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AName: String);
  end;

  { TLldbInstructionTargetCreate }

  TLldbInstructionTargetCreate = class(TLldbInstruction)
  private
    FRes: String;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AFile: String);
    property Res: String read FRes;
  end;

  { TLldbInstructionTargetDelete }

  TLldbInstructionTargetDelete = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create;
  end;

  { TLldbInstructionTargetStopHook }

  TLldbInstructionTargetStopHook = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(const ACmd: String);
  end;

  { TLldbInstructionProcessLaunch }

  TLldbInstructionProcessLaunch = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create();
  end;

  { TLldbInstructionProcessStep }

  TLldbInstructionProcessStepAction = (saContinue, saOver, saInto, saOut, saInsIn, saInsOver);

  TLldbInstructionProcessStep = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AStepAction: TLldbInstructionProcessStepAction; AThread: Integer = -1);
  end;

  { TLldbInstructionProcessKill }

  TLldbInstructionProcessKill = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create();
  end;

  { TLldbInstructionProcessInterrupt }

  TLldbInstructionProcessInterrupt = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create();
  end;

  { TLldbInstructionBreakOrWatchSet }

  TLldbInstructionBreakOrWatchSet = class(TLldbInstruction)
  private
    FBreakId: Integer;
    FLldbNoDisableError: Boolean;
    FState: TValidState;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    property BreakId: Integer read FBreakId;
    property State: TValidState read FState;
    property LldbNoDisableError: Boolean read FLldbNoDisableError;
  end;

  { TLldbInstructionBreakSet }

  TLldbInstructionBreakSet = class(TLldbInstructionBreakOrWatchSet)
  public
    constructor Create(AFileName: String; ALine: Integer; ADisabled: Boolean = False; AConditon: String = '');
    constructor Create(AMethod: String; ADisabled: Boolean = False; AConditon: String = '');
    constructor Create(AMethod: String; ADisabled: Boolean; ABeforePrologue: Boolean);
    constructor Create(AnAddress: TDBGPtr; ADisabled: Boolean = False; AConditon: String = '');
  end;

  { TLldbInstructionBreakModify }

  TLldbInstructionBreakModify = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AnId: Integer; ADisabled: Boolean);
    constructor Create(AnId: Integer; ADisabled: Boolean; AConditon: String);
  end;

  { TLldbInstructionBreakAddCommands }

  TLldbInstructionBreakAddCommands = class(TLldbInstruction)
  private
    FCommands: TStringArray;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
    procedure SendCommandDataToDbg(); override;
  public
    constructor Create(AnId: Integer; ACommands: Array of String);
  end;

  { TLldbInstructionBreakDelete }

  TLldbInstructionBreakDelete = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AnId: Integer);
  end;

  { TLldbInstructionWatchSet }

  TLldbInstructionWatchSet = class(TLldbInstructionBreakOrWatchSet)
  public
    constructor Create(AWatch: String; AKind: TDBGWatchPointKind);
  end;

  { TLldbInstructionWatchModify }

  TLldbInstructionWatchModify = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AnId: Integer; AConditon: String = '');
  end;

  { TLldbInstructionWatchDelete }

  TLldbInstructionWatchDelete = class(TLldbInstruction)
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AnId: Integer);
  end;

  { TLldbInstructionThreadSelect }

  TLldbInstructionThreadSelect = class(TLldbInstruction)
  private
    FIndex: Integer;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AnIndex: Integer);
  end;

  { TLldbInstructionFrameSelect }

  TLldbInstructionFrameSelect = class(TLldbInstruction)
  private
    FIndex: Integer;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AnIndex: Integer);
  end;

  { TLldbInstructionValueBase }

  TLldbInstructionValueBase = class(TLldbInstruction)
  private
    FCurly: Integer;
  protected
    function ParseStruct(ALine: string): Boolean;
  end;

  { TLldbInstructionLocals }

  TLldbInstructionLocals = class(TLldbInstructionValueBase)
  private
    FRes: TStringList;
    FCurVal, FCurName: String;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
    procedure SendCommandDataToDbg(); override;
  public
    constructor Create(AThread, AFrame: Integer);
    destructor Destroy; override;
    property Res: TStringList read FRes;
  end;

  { TLldbInstructionExpressionBase }

  TLldbInstructionExpressionBase = class(TLldbInstructionValueBase)
  private
    FRes: String;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    property Res: String read FRes;
  end;

  { TLldbInstructionExpression }

  TLldbInstructionExpression = class(TLldbInstructionExpressionBase)
  public
    constructor Create(AnExpression: String; AThread, AFrame: Integer);
  end;

  { TLldbInstructionReadExpression
    Reads data, if LLDB already printing it
  }

  TLldbInstructionReadExpression = class(TLldbInstructionExpressionBase)
  protected
    procedure SendCommandDataToDbg(); override;
  public
    constructor Create;
  end;

  { TLldbInstructionMemory }

  TArrayOfByte = array of byte;

  TLldbInstructionMemory = class(TLldbInstruction)
  private
    FRes: TArrayOfByte;
    FReading: Boolean;
  protected
    function ProcessInputFromDbg(const AData: String): Boolean; override;
    procedure SendCommandDataToDbg(); override;
  public
    constructor Create(AnAddress: TDBGPtr; ALen: Cardinal);
    destructor Destroy; override;
    property Res: TArrayOfByte read FRes;
  end;

  { TLldbInstructionRegister }

  TLldbInstructionRegister = class(TLldbInstruction)
  private
    FRes: TStringList;
    FReading: Boolean;
  protected
    procedure DoFree; override;
    function ProcessInputFromDbg(const AData: String): Boolean; override;
    procedure SendCommandDataToDbg(); override;
  public
    constructor Create(AThread, AFrame: Integer);
    destructor Destroy; override;
    property Res: TStringList read FRes;
  end;

  { TLldbInstructionThreadList }

  TLldbInstructionThreadList = class(TLldbInstruction)
  private
    FRes: TStringArray;
    FReading: Boolean;
  protected
    procedure SendCommandDataToDbg(); override;
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create();
    destructor Destroy; override;
    property Res: TStringArray read FRes;
  end;

  { TLldbInstructionThreadListReader }

  TLldbInstructionThreadListReader = class(TLldbInstructionThreadList)
  protected
    procedure SendCommandDataToDbg(); override;
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  end;

  { TLldbInstructionStackTrace }

  TLldbInstructionStackTrace = class(TLldbInstruction)
  private
    FRes: TStringArray;
    FReading: Boolean;
  protected
    procedure SendCommandDataToDbg(); override;
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(FrameCount: Integer; AThread: Integer);
    constructor Create(FrameCount, FirstFrame: Integer; AThread: Integer);
    destructor Destroy; override;
    property Res: TStringArray read FRes;
  end;

  { TLldbInstructionDisassem }

  TLldbInstructionDisassem = class(TLldbInstruction)
  private
    FRes: TStringList;
    FReading: Boolean;
  protected
    procedure SendCommandDataToDbg(); override;
    function ProcessInputFromDbg(const AData: String): Boolean; override;
  public
    constructor Create(AnAddr: TDBGPtr; NumLines: Cardinal);
    constructor CreateRange(StartAddr: TDBGPtr; EndAddr: TDBGPtr);
    destructor Destroy; override;
    property Res: TStringList read FRes;
end;

implementation

{ TLldbInstructionValueBase }

function TLldbInstructionValueBase.ParseStruct(ALine: string): Boolean;
var
  i: Integer;
begin
  i := 1;
  while i <= Length(ALine) do begin
    case ALine[i] of
      '"': break; // string always goes to end of line
      '{': inc(FCurly);
      '}': dec(FCurly);
    end;
    inc(i);
    if FCurly<0 then debugln(['ParseStruct curly too low ', FCurly]);
  end;
  Result := FCurly <= 0;
end;

{ TLldbInstructionBreakOrWatchSet }

function TLldbInstructionBreakOrWatchSet.ProcessInputFromDbg(const AData: String
  ): Boolean;
var
  i: Integer;
  found, found2: TStringArray;
begin
  Result := True;

  if StrMatches(AData, ['', 'unrecognized option', 'disable', '']) then begin
    FLldbNoDisableError := True;
    //MarkAsFailed; // next line will be error
    exit;
  end;

  if StrMatches(AData, ['Breakpoint ',': ', ''], found) then begin
    i := StrToIntDef(found[0], -1);
    if i = -1 then begin
      MarkAsFailed;
      exit;
    end;
    FBreakId:= i;

    if StrContains(found[1], 'pending') then
      FState := vsPending
    else
    if StrMatches(found[1], ['', ' locations'], found2) then begin
      if StrToIntDef(found2[0], 0) > 0 then
        FState := vsValid;
    end
    else
    if StrStartsWith(found[1], 'where = ') then
      FState := vsValid;

    MarkAsSuccess;
  end
//Breakpoint 41: where = lazarus.exe`CREATE + 2029 at synedit.pp:2123, address = 0x00764d2d
//Breakpoint 38: no locations (pending).
//Breakpoint 34: 3 locations.
  else
      Result := inherited;
end;

{ TLldbInstructionQueue }

function TLldbInstructionQueue.CheckForIgnoredError(const AData: String
  ): Boolean;
begin
  Result := True;
  if StrStartsWith(AData, 'error: ') then begin // ignore dwarf warnings
    if StrMatches(AData, ['error', 'unhandled type tag', 'DW_TAG_', '']) then // ignore dwarf warnings
      exit;
    if StrStartsWith(AData, 'error: need to add support for DW_TAG_') then // ignore dwarf warnings
      exit;
  end;
  Result := False;
end;

procedure TLldbInstructionQueue.DoBeforeHandleLineReceived(var ALine: String);
begin
  inherited DoBeforeHandleLineReceived(ALine); // Do first send to DebugOutput window

  while LeftStr(ALine, 7) = '(lldb) ' do begin
    Delete(ALine, 1, 7);
  end;

  if CheckForIgnoredError(ALine) then begin
    ALine := '';
    exit;
  end;

  // TODO: detect the echo, and flag if data is for RunningInstruction;

//  if LeftStr(ALine, 7) = 'error: ' then begin
//    // TODO: late error for previous instruction
//    ALine := '';
//  end;
//
//  ALine := '';
end;

function TLldbInstructionQueue.GetSelectFrameInstruction(AFrame: Integer
  ): TDBGInstruction;
begin
  Result := TLldbInstructionFrameSelect.Create(AFrame);
end;

function TLldbInstructionQueue.GetSelectThreadInstruction(AThreadId: Integer
  ): TDBGInstruction;
begin
  Result := TLldbInstructionThreadSelect.Create(AThreadId);
end;

procedure TLldbInstructionQueue.CancelAllForCommand(ACommand: TObject);
var
  Instr, NextInstr: TLldbInstruction;
begin
  NextInstr := TLldbInstruction(FirstInstruction);
  while NextInstr <> nil do begin
    Instr := NextInstr;
    NextInstr := TLldbInstruction(Instr.NextInstruction);
    if Instr.OwningCommand = ACommand then begin
      Instr.Cancel;
    end;
  end;

  if (RunningInstruction <> nil) and
     (TLldbInstruction(RunningInstruction).OwningCommand = ACommand) and
     (not RunningInstruction.IsCompleted)
  then begin
    RunningInstruction.OnFailure := nil;
    RunningInstruction.OnFinish := nil;
    RunningInstruction.Cancel;
  end;
end;

{ TLldbInstruction }

function TLldbInstruction.GetQueue: TLldbInstructionQueue;
begin
  Result := TLldbInstructionQueue(inherited Queue);
end;

function TLldbInstruction.ProcessInputFromDbg(const AData: String): Boolean;
begin
  Result := False;
  if LeftStr(AData, 7) = 'error: ' then begin
    Result := True;

    HandleError(ifeContentError);
    exit;
  end;
end;

procedure TLldbInstruction.SetContentReceieved;
begin
  inherited;
  MarkAsSuccess;
end;

{ TLldbInstructionSettingSet }

function TLldbInstructionSettingSet.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);

  if not Result then // if Result=true then self is destroyed;
    MarkAsSuccess;
  Result := true;
end;

constructor TLldbInstructionSettingSet.Create(AName, AValue: String;
  AGlobal: Boolean; AQuote: Boolean);
begin
  if AQuote then begin
    AValue := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
    AValue := StringReplace(AValue, '"', '\"', [rfReplaceAll]);
    AValue := '"'+AValue+'"';
  end;
  if AGlobal then
    inherited Create(Format('settings set -g -- %s %s', [AName, AValue]))
  else
    inherited Create(Format('settings set -- %s %s', [AName, AValue]));
end;

{ TLldbInstructionSettingRemove }

function TLldbInstructionSettingRemove.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);

  if not Result then // if Result=true then self is destroyed;
    MarkAsSuccess;
  Result := true;
end;

constructor TLldbInstructionSettingRemove.Create(AName, AValue: String;
  AQuote: Boolean);
begin
  if AQuote then begin
    AValue := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
    AValue := StringReplace(AValue, '"', '\"', [rfReplaceAll]);
    AValue := '"'+AValue+'"';
  end;
  inherited Create(Format('settings remove %s %s', [AName, AValue]));
end;

{ TLldbInstructionSettingClear }

function TLldbInstructionSettingClear.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);

  if not Result then // if Result=true then self is destroyed;
    MarkAsSuccess;
  Result := true;
end;

constructor TLldbInstructionSettingClear.Create(AName: String);
begin
  inherited Create(Format('settings clear %s', [AName]));
end;

{ TLldbInstructionTargetCreate }

function TLldbInstructionTargetCreate.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := True;
  if LeftStr(AData, 25) = 'Current executable set to' then begin
    FRes := AData;
    SetContentReceieved;
  end
  else
    Result := inherited;
end;

constructor TLldbInstructionTargetCreate.Create(AFile: String);
begin
  if pos(' ', AFile) > 0 then
    AFile := ''''+AFile+'''';
  inherited Create('target create '+AFile);
end;

{ TLldbInstructionTargetDelete }

function TLldbInstructionTargetDelete.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := True;
  if (LeftStr(AData, 27) = 'error: no targets to delete') or
     (LeftStr(AData, 17) = '1 targets deleted')
  then begin
    SetContentReceieved;
  end
  else
    Result := inherited;
end;

constructor TLldbInstructionTargetDelete.Create;
begin
  inherited Create('target delete 0');
end;

{ TLldbInstructionTargetStopHook }

function TLldbInstructionTargetStopHook.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := True;
  if StrStartsWith(AData, 'Stop hook ') and (pos(' added', AData) > 8) then begin
    SetContentReceieved;
  end
  else
    Result := inherited;
end;

constructor TLldbInstructionTargetStopHook.Create(const ACmd: String);
begin
  inherited Create(Format('target stop-hook add -o "%s"', [ACmd]));
end;

{ TLldbInstructionProcessLaunch }

function TLldbInstructionProcessLaunch.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  if StrStartsWith(AData, 'Process ') and (pos(' launched:', AData) > 8) then begin
    SetContentReceieved;
  end
  else
    Result := inherited;
  Result := True; // Ignore any "process stopped", before "launched"
end;

constructor TLldbInstructionProcessLaunch.Create();
begin
  inherited Create('process launch -n');
end;

{ TLldbInstructionProcessStep }

function TLldbInstructionProcessStep.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);
  SetContentReceieved;
end;

constructor TLldbInstructionProcessStep.Create(
  AStepAction: TLldbInstructionProcessStepAction; AThread: Integer);
begin
  case AStepAction of
  	saContinue: inherited Create('process continue');
  (*
    saOver: inherited Create('thread step-over', AThread);
  	saInto: inherited Create('thread step-in', AThread);
    saOut: inherited Create('thread step-out', AThread);
    saInsIn: inherited Create ('thread step-inst', AThread);
    saInsOver: inherited Create('thread step-inst-over', AThread);
  // *)
  //(*
    saOver: inherited Create(Format('thread step-over %d', [AThread]));
  	saInto: inherited Create(Format('thread step-in %d', [AThread]));
    saOut: inherited Create(Format('thread step-out %d', [AThread]));
    saInsIn: inherited Create (Format('thread step-inst %d', [AThread]));
    saInsOver: inherited Create(Format('thread step-inst-over %d', [AThread]));
  // *)
  end;
end;

{ TLldbInstructionProcessKill }

function TLldbInstructionProcessKill.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := True;
  if (LeftStr(AData, 8) = 'Process ') and (pos(' exited with status = ', AData) > 7) then begin
    SetContentReceieved;
  end
  else
      Result := inherited;
end;

constructor TLldbInstructionProcessKill.Create();
begin
  inherited Create('process kill');
end;

{ TLldbInstructionProcessInterrupt }

function TLldbInstructionProcessInterrupt.ProcessInputFromDbg(
  const AData: String): Boolean;
begin
  Result := True;
  SetContentReceieved;
end;

constructor TLldbInstructionProcessInterrupt.Create();
begin
  inherited Create('process interrupt');
end;

{ TLldbInstructionBreakSet }

constructor TLldbInstructionBreakSet.Create(AFileName: String; ALine: Integer;
  ADisabled: Boolean; AConditon: String);
begin
  FState := vsInvalid;
  if AConditon <> '' then AConditon := ' --condition ''' + AConditon + '''';
  if ADisabled then AConditon := AConditon + ' -d ';
  if pos(' ', AFileName) > 0 then
    AFileName := ''''+AFileName+'''';
  inherited Create(Format('breakpoint set --file %s --line %d', [AFileName, ALine]) + AConditon);
end;

constructor TLldbInstructionBreakSet.Create(AMethod: String;
  ADisabled: Boolean; AConditon: String);
begin
  FState := vsInvalid;
  if AConditon <> '' then AConditon := ' --condition ''' + AConditon + '''';
  if ADisabled then AConditon := AConditon + ' -d ';
  inherited Create(Format('breakpoint set --func %s', [AMethod]) + AConditon);
end;

constructor TLldbInstructionBreakSet.Create(AMethod: String;
  ADisabled: Boolean; ABeforePrologue: Boolean);
var
  s: String;
begin
  FState := vsInvalid;
  s := '';
  if ABeforePrologue then s := ' -K false ';
  if ADisabled then s := s + ' -d ';
  inherited Create(Format('breakpoint set --func %s', [AMethod]) + s);
end;

constructor TLldbInstructionBreakSet.Create(AnAddress: TDBGPtr;
  ADisabled: Boolean; AConditon: String);
begin
  FState := vsInvalid;
  if AConditon <> '' then AConditon := ' --condition ''' + AConditon + '''';
  if ADisabled then AConditon := AConditon + ' -d ';
  inherited Create(Format('breakpoint set --address %u', [AnAddress]) + AConditon);
end;

{ TLldbInstructionBreakModify }

function TLldbInstructionBreakModify.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);
  if not Result then
    MarkAsSuccess;
end;

constructor TLldbInstructionBreakModify.Create(AnId: Integer; ADisabled: Boolean
  );
begin
  if ADisabled
  then inherited Create(Format('breakpoint modify --disable %d', [AnId]))
  else inherited Create(Format('breakpoint modify --enable %d', [AnId]));
end;

constructor TLldbInstructionBreakModify.Create(AnId: Integer;
  ADisabled: Boolean; AConditon: String);
begin
  AConditon := ' --condition ''' + AConditon + '''';
  if ADisabled
  then AConditon := ' --disable' + AConditon
  else AConditon := ' --enable' + AConditon;
  inherited Create(Format('breakpoint modify %s %d', [AConditon, AnId]));
end;

{ TLldbInstructionBreakAddCommands }

function TLldbInstructionBreakAddCommands.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  if StrStartsWith(AData, 'version') then begin
    Result := True;
    MarkAsSuccess;
    Exit;
  end;
  Result := inherited ProcessInputFromDbg(AData);
end;

procedure TLldbInstructionBreakAddCommands.SendCommandDataToDbg();
var
  i: Integer;
begin
  inherited SendCommandDataToDbg();
  for i := 0 to length(FCommands) - 1 do
    Queue.SendDataToDBG(Self, FCommands[i]);
  Queue.SendDataToDBG(Self, 'DONE');
  Queue.SendDataToDBG(Self, 'version'); // end marker // do not sent before new prompt
end;

constructor TLldbInstructionBreakAddCommands.Create(AnId: Integer;
  ACommands: array of String);
var
  i: Integer;
begin
  inherited Create(Format('breakpoint command add %d', [AnId]));
  SetLength(FCommands, Length(ACommands));
  for i := 0 to Length(ACommands) - 1 do
    FCommands[i] := ACommands[i];
end;

{ TLldbInstructionBreakDelete }

function TLldbInstructionBreakDelete.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);
  if Result then exit;  // if Result=true then self is destroyed;

  //ReadLn "1 breakpoints deleted; 0 breakpoint locations disabled."
  if StrMatches(AData, ['', 'breakpoint', 'deleted', '']) then
    MarkAsSuccess;
  Result := true;

  //TODO: "error: No breakpoints exist to be deleted."
  // prevent from failing other instruction
end;

constructor TLldbInstructionBreakDelete.Create(AnId: Integer);
begin
  inherited Create(Format('breakpoint delete %d', [AnId]));
end;

{ TLldbInstructionWatchSet }

constructor TLldbInstructionWatchSet.Create(AWatch: String;
  AKind: TDBGWatchPointKind);
begin
  case AKind of
  	wpkWrite:     inherited Create(Format('watchpoint set variable -w write %s', [AWatch]));
    wpkRead:      inherited Create(Format('watchpoint set variable -w read %s', [AWatch]));
    wpkReadWrite: inherited Create(Format('watchpoint set variable -w read_write %s', [AWatch]));
  end;
end;

{ TLldbInstructionWatchModify }

function TLldbInstructionWatchModify.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);
  if not Result then
    MarkAsSuccess;
end;

constructor TLldbInstructionWatchModify.Create(AnId: Integer; AConditon: String
  );
begin
  inherited Create(Format('watchpoint modify --condition ''%s'' %d', [AConditon, AnId]));
end;

{ TLldbInstructionWatchDelete }

function TLldbInstructionWatchDelete.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);

  if not Result then // if Result=true then self is destroyed;
    MarkAsSuccess;
  Result := true;

  //TODO: "error: No breakpoints exist to be deleted."
  // prevent from failing other instruction
end;

constructor TLldbInstructionWatchDelete.Create(AnId: Integer);
begin
  inherited Create(Format('watchpoint delete %d', [AnId]));
end;

{ TLldbInstructionThreadSelect }

function TLldbInstructionThreadSelect.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);

  if not Result then begin // if Result=true then self is destroyed;
    Queue.SetKnownThread(FIndex);
    MarkAsSuccess;
  end;
  Result := true;
end;

constructor TLldbInstructionThreadSelect.Create(AnIndex: Integer);
begin
  FIndex := AnIndex;
  inherited Create(Format('thread select %d', [AnIndex]));
end;

{ TLldbInstructionFrameSelect }

function TLldbInstructionFrameSelect.ProcessInputFromDbg(const AData: String
  ): Boolean;
begin
  Result := inherited ProcessInputFromDbg(AData);

  if not Result then begin // if Result=true then self is destroyed;
    Queue.SetKnownThreadAndFrame(Queue.CurrentThreadId, FIndex);
    MarkAsSuccess;
  end;
  Result := true;
(* TODO: ?
ReadLn "* thread #3"
ReadLn "    frame #0: 0x7700eb6c ntdll.dll`NtDelayExecution + 12"

This falls through to TLldbDebugger.DoAfterLineReceived
and sets the current location in the editor.
*)
end;

constructor TLldbInstructionFrameSelect.Create(AnIndex: Integer);
begin
  FIndex := AnIndex;
  inherited Create(Format('frame select %d', [AnIndex]));
end;

{ TLldbInstructionLocals }

function TLldbInstructionLocals.ProcessInputFromDbg(const AData: String
  ): Boolean;
var
  found: TStringArray;
begin
  Result := True;

  if StrStartsWith(AData, 'version') then begin
    MarkAsSuccess;
    Exit;
  end;

  if FCurVal <> '' then begin
    FCurVal := FCurVal + AData;
    if ParseStruct(AData) then begin
      FRes.Values[FCurName] := FCurVal;
      FCurName := '';
      FCurVal := '';
    end;
    exit;
  end;

  if StrMatches(AData, ['(', ')', ' = ', ''], found) then begin
    FCurName := found[1];
    FCurVal := found[2];
    FCurly := 0;
    if ParseStruct(found[2]) then begin
      FRes.Values[FCurName] := FCurVal;
      FCurName := '';
      FCurVal := '';
    end;
    exit;
  end;

  Result := inherited ProcessInputFromDbg(AData);
end;

procedure TLldbInstructionLocals.SendCommandDataToDbg();
begin
  inherited SendCommandDataToDbg();
  Queue.SendDataToDBG(Self, 'version'); // end marker // do not sent before new prompt
end;

constructor TLldbInstructionLocals.Create(AThread, AFrame: Integer);
begin
  inherited Create('frame variable -P 1 -D 5', AThread, AFrame); // TODO: make -D 5 configurable
  FRes := TStringList.Create;
end;

destructor TLldbInstructionLocals.Destroy;
begin
  inherited Destroy;
  FRes.Free;
end;

{ TLldbInstructionExpressionBase }

function TLldbInstructionExpressionBase.ProcessInputFromDbg(const AData: String
  ): Boolean;
var
  found: TStringArray;
begin
  Result := True;

  if FRes <> '' then begin
    FRes := FRes + AData;
    if ParseStruct(AData) then
      SetContentReceieved;
    exit;
  end;

  if StrMatches(AData, ['(', ')', ' = ', ''], found) then begin
    FRes := AData;
    FCurly := 0;
    if ParseStruct(found[2]) then
      SetContentReceieved;
    exit;
  end;
// error: use of undeclared identifier 'i'
// (int) $0 = 133
// (LONGINT) I = 99
// (ANSISTRING) $1 = 0x005aac80
  Result := inherited ProcessInputFromDbg(AData);
end;

{ TLldbInstructionExpression }

constructor TLldbInstructionExpression.Create(AnExpression: String; AThread,
  AFrame: Integer);
begin
//  inherited Create(Format('expression -R -- %s', [UpperCase(AnExpression)]));
  inherited Create(Format('expression -T -- %s', [UpperCase(AnExpression)]), AThread, AFrame);
end;

{ TLldbInstructionReadExpression }

procedure TLldbInstructionReadExpression.SendCommandDataToDbg();
begin
  // do not sent data
  SetStateRunning;
end;

constructor TLldbInstructionReadExpression.Create;
begin
  inherited Create('');
end;

{ TLldbInstructionMemory }

function TLldbInstructionMemory.ProcessInputFromDbg(const AData: String
  ): Boolean;
var
  found: TStringArray;
  n, l, i: Integer;
  s: String;
begin
  Result := False;
  if StrStartsWith(AData, Command) then begin
    FReading := True;
  end;

  if not FReading then
    exit;

  Result := True;


  if StrMatches(AData, ['0x', ': ', ''], found) then begin
    // todo check the address
    l := Length(FRes);
    s := found[1];
    n := (Length(s)+1) div 5;
    SetLength(FRes, l+n);
    for i := l to l + n-1 do begin
      FRes[i] := StrToIntDef(copy(s,1,4), 0);
      delete(s,1,5);
    end;
    exit;
  end;
//<< << TCmdLineDebugger.ReadLn "0x005ff280: 0x60 0x10 0x77 0x04"


  if StrStartsWith(AData, 'version') then begin
    MarkAsSuccess;
    Exit;
  end;

  Result := inherited ProcessInputFromDbg(AData);
end;

procedure TLldbInstructionMemory.SendCommandDataToDbg();
begin
  inherited SendCommandDataToDbg();
  Queue.SendDataToDBG(Self, 'version'); // end marker // do not sent before new prompt
end;

constructor TLldbInstructionMemory.Create(AnAddress: TDBGPtr; ALen: Cardinal);
begin
  inherited Create(Format('memory read --force --size 1 --format x --count %u %u', [ALen, AnAddress]));
end;

destructor TLldbInstructionMemory.Destroy;
begin
  inherited Destroy;
  FRes := nil;
end;

{ TLldbInstructionRegister }

procedure TLldbInstructionRegister.DoFree;
begin
  FreeAndNil(FRes);
  inherited DoFree;
end;

function TLldbInstructionRegister.ProcessInputFromDbg(const AData: String
  ): Boolean;
var
  found: TStringArray;
  i: Integer;
  s, reg, val: String;
begin
  Result := False;
  if StrStartsWith(AData, Command) then begin
    FReading := True;
  end;

  if not FReading then
    exit;

  Result := True;

  if StrStartsWith(AData, 'General Purpose Registers:') then
    exit;

  if StrMatches(AData, ['  ', ' = ', ''], found) then begin
    if FRes = nil then FRes := TStringList.Create;
      reg := UpperCase(trim(found[0]));
      i := pos(' ', found[1]);
      if i < 1 then i := Length(found[1]);
      val := copy(found[1], 1, i);
      FRes.Values[reg] := val;
    exit;
  end;

  if StrStartsWith(AData, 'version') then begin
    MarkAsSuccess;
    Exit;
  end;

  Result := inherited ProcessInputFromDbg(AData);

(*
   << Finished Instruction: register read --all // True
  << Current Instruction:
  TDBGInstructionQueue.RunQueue nil / nil
  << << TCmdLineDebugger.ReadLn "General Purpose Registers:"
  << << TCmdLineDebugger.ReadLn "       eax = 0x00000000"
  << << TCmdLineDebugger.ReadLn "       ebx = 0x005AF750  VMT_$UNIT1_$$_TFORM1"
  << << TCmdLineDebugger.ReadLn "       ecx = 0x04696C7C"
  << << TCmdLineDebugger.ReadLn "       edx = 0x00000002"
  << << TCmdLineDebugger.ReadLn "       edi = 0x005AF750  VMT_$UNIT1_$$_TFORM1"
  << << TCmdLineDebugger.ReadLn "       esi = 0x046F1060"
  << << TCmdLineDebugger.ReadLn "       ebp = 0x0262FDF8"
  << << TCmdLineDebugger.ReadLn "       esp = 0x0262FDA8"
  << << TCmdLineDebugger.ReadLn "       eip = 0x004294A8  project1.exe`FORMCREATE + 104 at unit1.pas:39"
  << << TCmdLineDebugger.ReadLn "    eflags = 0b00000000000000000000001001000110"
  << << TCmdLineDebugger.ReadLn ""
< TLldbDebugger.UnlockRelease 1
*)
end;

procedure TLldbInstructionRegister.SendCommandDataToDbg();
begin
  inherited SendCommandDataToDbg();
  Queue.SendDataToDBG(Self, 'version'); // end marker // do not sent before new prompt
end;

constructor TLldbInstructionRegister.Create(AThread, AFrame: Integer);
begin
  inherited Create('register read --all', AThread, AFrame);
end;

destructor TLldbInstructionRegister.Destroy;
begin
  inherited Destroy;
  FRes.Free;
end;

{ TLldbInstructionThreadList }

procedure TLldbInstructionThreadList.SendCommandDataToDbg();
begin
  inherited SendCommandDataToDbg();
  Queue.SendDataToDBG(Self, 'version'); // end marker // do not sent before new prompt
end;

function TLldbInstructionThreadList.ProcessInputFromDbg(const AData: String
  ): Boolean;
var
  l: Integer;
begin
  Result := False;
  if StrStartsWith(AData, Command) then begin
    FReading := True;
    exit;
  end;

  if not FReading then
    exit;

  Result := True;

  if StrStartsWith(AData, 'Process ') then
    exit;


  if StrStartsWith(AData, '* thread #') or StrStartsWith(AData, '  thread #') then begin
DebugLn(['######### add ',AData]);
    l := Length(FRes);
    SetLength(FRes, l+1);
    FRes[l] := AData;
    exit;
  end;

  if StrStartsWith(AData, 'version') then begin
    MarkAsSuccess;
    Exit;
  end;

  Result := inherited ProcessInputFromDbg(AData);
end;

constructor TLldbInstructionThreadList.Create();
begin
  inherited Create('thread list');
end;

destructor TLldbInstructionThreadList.Destroy;
begin
  inherited Destroy;
  FRes := nil;
end;

{ TLldbInstructionThreadListReader }

procedure TLldbInstructionThreadListReader.SendCommandDataToDbg();
begin
  // send nothing
  SetStateRunning;
  FReading := True;
end;

function TLldbInstructionThreadListReader.ProcessInputFromDbg(
  const AData: String): Boolean;
begin
  if StrStartsWith(AData, '    frame ') then begin
    MarkAsFailed;
    Result := False;
    exit;
  end;

  Result := inherited ProcessInputFromDbg(AData);

  if StrMatches(AData, ['Process ', 'stopped']) then begin
    MarkAsSuccess;
    Exit;
  end;
end;

{ TLldbInstructionStackTrace }

procedure TLldbInstructionStackTrace.SendCommandDataToDbg();
begin
  inherited SendCommandDataToDbg();
  Queue.SendDataToDBG(Self, 'version'); // end marker // do not sent before new prompt
end;

function TLldbInstructionStackTrace.ProcessInputFromDbg(const AData: String
  ): Boolean;
var
  l: Integer;
begin
  Result := False;
  if StrStartsWith(AData, Command) then begin
    FReading := True;
    exit;
  end;

  if not FReading then
    exit;

  Result := True;

  if StrStartsWith(AData, '* thread ') then
    exit;


  if StrStartsWith(AData, '  * frame ') or StrStartsWith(AData, '    frame ') then begin
    l := Length(FRes);
    SetLength(FRes, l+1);
    FRes[l] := AData;
    exit;
  end;

  if StrStartsWith(AData, 'version') then begin
    MarkAsSuccess;
    Exit;
  end;

  Result := inherited ProcessInputFromDbg(AData);
end;

constructor TLldbInstructionStackTrace.Create(FrameCount: Integer;
  AThread: Integer);
begin
  inherited Create(Format('thread backtrace -c %d', [FrameCount]), AThread);
end;

constructor TLldbInstructionStackTrace.Create(FrameCount, FirstFrame: Integer;
  AThread: Integer);
begin
  inherited Create(Format('thread backtrace -s %d -c %d', [FirstFrame, FrameCount]), AThread);
end;

destructor TLldbInstructionStackTrace.Destroy;
begin
  inherited Destroy;
  FRes := nil;
end;

constructor TLldbInstructionDisassem.Create(AnAddr: TDBGPtr; NumLines: Cardinal);
var AddressString: String;
begin
  FRes := TStringList.Create;
  AddressString := IntToHex(AnAddr, 16);
  inherited Create(Format('disassemble -b --start-address  0x%s --count %d', [AddressString, NumLines]));
end;

constructor TLldbInstructionDisassem.CreateRange(StartAddr: TDBGPtr;
  EndAddr: TDBGPtr);
var StartAddressString, EndAddressString: String;
begin
  FRes := TStringList.Create;
  StartAddressString := IntToHex(StartAddr, 16);
  EndAddressString := IntToHex(EndAddr, 16);
  inherited Create(Format('disassemble -b --start-address  0x%s --end-address 0x%s', [StartAddressString, EndAddressString]));
end;


destructor TLldbInstructionDisassem.Destroy;
begin
  FRes.Free;
  inherited Destroy;
end;

function TLldbInstructionDisassem.ProcessInputFromDbg(const AData: String): Boolean;
var i: Integer;
    Address: TDBGPtr;
begin
  Result := False;
  if StrStartsWith(AData, Command) then begin
    FReading := True;
    exit;
  end;

  if not FReading then
    exit;

  Result := True;

  if StrStartsWith(AData, 'version') then begin
    MarkAsSuccess;
    Exit;
  end;

  Result := inherited ProcessInputFromDbg(AData);

  if not Result then
    FRes.Add(StringReplace(AData, '->', '  ',[]));

end;

procedure TLldbInstructionDisassem.SendCommandDataToDbg();
begin
  inherited SendCommandDataToDbg();
  Queue.SendDataToDBG(Self, 'version'); // end marker // do not sent before new prompt
end;


end.


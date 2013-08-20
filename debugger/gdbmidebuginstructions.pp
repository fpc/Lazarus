unit GDBMIDebugInstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, CmdLineDebugger, GDBMIMiscClasses, LazLoggerBase;

type

  { TGDBMICmdLineDebugger }

  TGDBMICmdLineDebugger = class(TCmdLineDebugger)
  protected
    FErrorHandlingFlags: set of (ehfDeferReadWriteError, ehfGotReadError, ehfGotWriteError);
    procedure DoReadError; override;
    procedure DoWriteError; override;
  end;

  { TGDBInstruction }

  TGDBInstructionFlag = (
    ifAutoDestroy,
    ifRequiresThread,
    ifRequiresStackFrame
  );
  TGDBInstructionFlags = set of TGDBInstructionFlag;

  TGDBInstructionResultFlag = (
    ifrComleted,
    ifrFailed
  );
  TGDBInstructionResultFlags = set of TGDBInstructionResultFlag;

  TGDBInstructionErrorFlag = (
    ifeWriteError,
    ifeReadError,
    ifeGdbNotRunning,
    ifeTimedOut,
    ifeRecoveredTimedOut
  );
  TGDBInstructionErrorFlags = set of TGDBInstructionErrorFlag;

  TGDBInstructionQueue = class;

  { TGDBInstruction }

  TGDBInstruction = class
  private
    FCommand: String;
    FFlags: TGDBInstructionFlags;
    FStackFrame: Integer;
    FThreadId: Integer;
  protected
    FResultFlags: TGDBInstructionResultFlags;
    FErrorFlags: TGDBInstructionErrorFlags;
    FTimeOut: Integer;
    procedure SendCommandDataToGDB(AQueue: TGDBInstructionQueue); virtual;
    function  ProcessInputFromGdb(const AData: String): Boolean; virtual; abstract; // True if data was handled
    function IsCompleted: boolean; virtual;                                        // No more InputFromGdb required

    procedure MarkAsSuccess;
    procedure HandleWriteError(ASender: TGDBInstruction); virtual;
    procedure HandleReadError; virtual;
    procedure HandleTimeOut; virtual;
    procedure HandleRecoveredTimeOut; virtual;
    procedure HandleNoGdbRunning; virtual;

    function  GetTimeOutVerifier: TGDBInstruction; virtual;
    procedure Init; virtual;
    procedure InternalCreate(ACommand: String;
                             AThread, AFrame: Integer; // ifRequiresThread, ifRequiresStackFrame will always be included
                             AFlags: TGDBInstructionFlags;
                             ATimeOut: Integer
                            );
  public
    constructor Create(ACommand: String;
                       AFlags: TGDBInstructionFlags = [];
                       ATimeOut: Integer = 0
                      );
    constructor Create(ACommand: String;
                       AThread: Integer;         // ifRequiresThread will always be included
                       AOtherFlags: TGDBInstructionFlags = [];
                       ATimeOut: Integer = 0
                      );
    constructor Create(ACommand: String;
                       AThread, AFrame: Integer; // ifRequiresThread, ifRequiresStackFrame will always be included
                       AOtherFlags: TGDBInstructionFlags = [];
                       ATimeOut: Integer = 0
                      );
    function IsSuccess: Boolean;
    property Command: String read FCommand;
    property ThreadId: Integer read FThreadId;
    property StackFrame: Integer read FStackFrame;
    property Flags: TGDBInstructionFlags read FFlags;
    property ResultFlags: TGDBInstructionResultFlags read FResultFlags;
    property ErrorFlags: TGDBInstructionErrorFlags read FErrorFlags;
    property TimeOut: Integer read FTimeOut;
  end;

  { TGDBInstructionVerifyTimeOut }

  TGDBInstructionVerifyTimeOutState = (
    vtSent, vtError,
    vtGotPrompt,
    vtGotPrompt7, vtGotPrompt7gdb, vtGotPrompt7and7, vtGotPrompt7and7gdb,
    vtGotPrompt1, vtGotPrompt1gdb,
    vtGot7, vtGot7gdb, vtGot7and7, vtGot7and7gdb, vtGot1, vtGot1gdb
  );

  TGDBInstructionVerifyTimeOut = class(TGDBInstruction)
  private
    FRunnigInstruction: TGDBInstruction;
    FList: TGDBMINameValueList;
    FPromptAfterErrorCount: Integer;
    FVal7Data: String;
    FState: TGDBInstructionVerifyTimeOutState;
  protected
    procedure SendCommandDataToGDB(AQueue: TGDBInstructionQueue); override;
    function ProcessInputFromGdb(const AData: String): Boolean; override;

    procedure HandleWriteError(ASender: TGDBInstruction); override;
    procedure HandleReadError; override;
    procedure HandleTimeOut; override;
    procedure HandleNoGdbRunning; override;
    function GetTimeOutVerifier: TGDBInstruction; override;
  public
    constructor Create(ARunnigInstruction: TGDBInstruction);
    destructor Destroy; override;
  end;

  { TGDBInstructionQueue }

  TGDBInstructionQueueFlag = (
    ifqValidThread,
    ifqValidStackFrame
  );
  TGDBInstructionQueueFlags = set of TGDBInstructionQueueFlag;

  TGDBInstructionQueue = class
  private
    FCurrentInstruction: TGDBInstruction;
    FCurrentStackFrame: Integer;
    FCurrunetThreadId: Integer;
    FDebugger: TGDBMICmdLineDebugger;
    FFlags: TGDBInstructionQueueFlags;

    procedure FinishCurrentInstruction;
    procedure SetCurrentInstruction(AnInstruction: TGDBInstruction);
  protected
    function SendDataToGDB(ASender: TGDBInstruction; AData: String): Boolean;
    //function ReadDataFromGDB(ASender: TGDBInstruction; AData: String): Boolean;
    procedure SelectThread(AThreadId: Integer);
    procedure SelectFrame(AFrame: Integer);
  public
    constructor Create(ADebugger: TGDBMICmdLineDebugger);
    procedure InvalidateThredAndFrame;
    procedure RunInstruction(AnInstruction: TGDBInstruction); // Wait for instruction to be finished, not queuing
    property CurrunetThreadId: Integer read FCurrunetThreadId;
    property CurrentStackFrame: Integer read FCurrentStackFrame;
    property Flags: TGDBInstructionQueueFlags read FFlags;
  end;

function dbgs(AState: TGDBInstructionVerifyTimeOutState): String; overload;

implementation

var
  DBGMI_TIMEOUT_DEBUG: PLazLoggerLogGroup;

const
  TIMEOUT_AFTER_WRITE_ERROR          = 50;
  TIMEOUT_FOR_SYNC_AFTER_TIMEOUT     = 2500; // extra timeout, while trying to recover from a suspected timeout
  TIMEOUT_FOR_SYNC_AFTER_TIMEOUT_MAX = 3000; // upper limit, when using 2*original_timeout

function dbgs(AState: TGDBInstructionVerifyTimeOutState): String; overload;
begin
  writestr(Result{%H-}, AState);
end;

{ TGDBInstructionVerifyTimeOut }

procedure TGDBInstructionVerifyTimeOut.SendCommandDataToGDB(AQueue: TGDBInstructionQueue);
begin
  AQueue.SendDataToGDB(Self, '-data-evaluate-expression 7');
  AQueue.SendDataToGDB(Self, '-data-evaluate-expression 1');
  FState := vtSent;
end;

function TGDBInstructionVerifyTimeOut.ProcessInputFromGdb(const AData: String): Boolean;
type
  TLineDataTipe = (ldOther, ldGdb, ldValue7, ldValue1);

  function CheckData(const ALineData: String): TLineDataTipe;
  begin
    Result := ldOther;
    if ALineData= '(gdb) ' then begin
      Result := ldGdb;
      exit;
    end;
    if copy(AData, 1, 6) = '^done,' then begin
      if FList = nil then
        FList := TGDBMINameValueList.Create(ALineData)
      else
        FList.Init(ALineData);
      if FList.Values['value'] = '7' then
        Result := ldValue7
      else
      if FList.Values['value'] = '1' then
        Result := ldValue1
    end;
  end;

  procedure SetError(APromptCount: Integer);
  begin
    FState := vtError;
    FPromptAfterErrorCount := APromptCount; // prompt for val7 and val1 needed
    FRunnigInstruction.HandleTimeOut;
    if FPromptAfterErrorCount <= 0 then
      FTimeOut := 50; // wait for timeout
  end;

begin
  if FState = vtError then begin
    dec(FPromptAfterErrorCount);
    if FPromptAfterErrorCount <= 0 then
      FTimeOut := 50; // wait for timeout
    exit;
  end;

  case CheckData(AData) of
    ldOther: begin
        debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Got other data']);
        FRunnigInstruction.ProcessInputFromGdb(AData);
      end;
    ldGdb:
      case FState of
        vtSent: begin
            debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Got prompt in order']);
            FState := vtGotPrompt;
            FRunnigInstruction.ProcessInputFromGdb(AData);
            if not FRunnigInstruction.IsCompleted then begin
              debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Prompt was not accepted']);
              SetError(2); // prompt for val=7 and val=1 needed
            end;
          end;
        vtGotPrompt7:     FState := vtGotPrompt7gdb;
        vtGotPrompt7and7: FState := vtGotPrompt7and7gdb;
        vtGotPrompt1:     FState := vtGotPrompt1gdb;
        vtGot7:           FState := vtGot7gdb;
        vtGot7and7:       FState := vtGot7and7gdb;
        vtGot1:           FState := vtGot1gdb;
        else begin
            debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Extra Prompt ']);
            if FState = vtGotPrompt
            then SetError(1)  // prompt val=1 needed
            else SetError(0); // no more prompt needed
          end;
      end;
    ldValue7:
      case FState of
        vtSent, vtGotPrompt: begin
            debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Got value 7']);
            FVal7Data := AData;
            if FState = vtSent
            then FState := vtGot7
            else FState := vtGotPrompt7;
          end;
        vtGotPrompt7gdb, vtGot7gdb: begin
            debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Got value 7 twice. Original Result?']);
            FRunnigInstruction.ProcessInputFromGdb(FVal7Data);
            if FState = vtGotPrompt7gdb
            then FState := vtGotPrompt7and7
            else FState := vtGot7and7;
          end;
        else begin
          debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Extra VAlue 7']);
          if FState in [vtGotPrompt7, vtGot7]
          then SetError(1)  // prompt val=1 needed
          else SetError(0); // no more prompt needed
        end;
      end;
    ldValue1:
      case FState of
        vtSent: begin
          debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Got other data']);
          FRunnigInstruction.ProcessInputFromGdb(AData);
        end;
        vtGotPrompt7gdb:     FState := vtGotPrompt1;
        vtGotPrompt7and7gdb: FState := vtGotPrompt1;
        vtGot7gdb:           FState := vtGot1;
        vtGot7and7gdb:       FState := vtGot1;
        else begin
          debugln(DBGMI_TIMEOUT_DEBUG, ['GDBMI TimeOut(', dbgs(FState), '): Wrong Value 1']);
          SetError(0);
        end;
      end;
  end;

  if FState = vtGot1gdb then begin
    // timeout, but recovored
    FRunnigInstruction.ProcessInputFromGdb('(gdb) '); // simulate prompt
    FRunnigInstruction.HandleRecoveredTimeOut;
  end;
  if FState in [vtGot1gdb, vtGotPrompt1gdb] then begin
    Include(FResultFlags, ifrComleted);
    if not FRunnigInstruction.IsCompleted then
      FRunnigInstruction.HandleTimeOut;
  end;

end;

procedure TGDBInstructionVerifyTimeOut.HandleWriteError(ASender: TGDBInstruction);
begin
  inherited HandleWriteError(ASender);
  FRunnigInstruction.HandleWriteError(ASender);
end;

procedure TGDBInstructionVerifyTimeOut.HandleReadError;
begin
  inherited HandleReadError;
  FRunnigInstruction.HandleReadError;
end;

procedure TGDBInstructionVerifyTimeOut.HandleTimeOut;
begin
  inherited HandleTimeOut;
  FRunnigInstruction.HandleTimeOut;
end;

procedure TGDBInstructionVerifyTimeOut.HandleNoGdbRunning;
begin
  inherited HandleNoGdbRunning;
  FRunnigInstruction.HandleNoGdbRunning;
end;

function TGDBInstructionVerifyTimeOut.GetTimeOutVerifier: TGDBInstruction;
begin
  Result := nil;
end;

constructor TGDBInstructionVerifyTimeOut.Create(ARunnigInstruction: TGDBInstruction);
var
  t: Integer;
begin
  FRunnigInstruction := ARunnigInstruction;
  t := FRunnigInstruction.TimeOut;
  t := max(TIMEOUT_FOR_SYNC_AFTER_TIMEOUT, Min(TIMEOUT_FOR_SYNC_AFTER_TIMEOUT_MAX, t * 2));
  inherited Create('', FRunnigInstruction.ThreadId, FRunnigInstruction.StackFrame,
                   FRunnigInstruction.Flags * [ifRequiresThread, ifRequiresStackFrame] + [ifAutoDestroy],
                   t);
end;

destructor TGDBInstructionVerifyTimeOut.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FList);
  if (FRunnigInstruction <> nil) and (ifAutoDestroy in FRunnigInstruction.Flags) then
    FRunnigInstruction.Free;
end;

{ TGDBMICmdLineDebugger }

procedure TGDBMICmdLineDebugger.DoReadError;
begin
  include(FErrorHandlingFlags, ehfGotReadError);
  if not(ehfDeferReadWriteError in FErrorHandlingFlags)
  then inherited DoReadError;
end;

procedure TGDBMICmdLineDebugger.DoWriteError;
begin
  include(FErrorHandlingFlags, ehfGotWriteError);
  if not(ehfDeferReadWriteError in FErrorHandlingFlags)
  then inherited DoWriteError;
end;

{ TGDBInstruction }

procedure TGDBInstruction.SendCommandDataToGDB(AQueue: TGDBInstructionQueue);
begin
  AQueue.SendDataToGDB(Self, FCommand);
end;

function TGDBInstruction.IsCompleted: boolean;
begin
  Result := FResultFlags * [ifrComleted, ifrFailed] <> [];
end;

procedure TGDBInstruction.MarkAsSuccess;
begin
  Include(FResultFlags, ifrComleted);
end;

procedure TGDBInstruction.HandleWriteError(ASender: TGDBInstruction);
begin
  //Include(FResultFlags, ifrFailed); // Do not fail yet
  Include(FErrorFlags,  ifeWriteError);
  if (FTimeOut = 0) or (FTimeOut > TIMEOUT_AFTER_WRITE_ERROR) then
    FTimeOut := TIMEOUT_AFTER_WRITE_ERROR;
end;

procedure TGDBInstruction.HandleReadError;
begin
  Include(FResultFlags, ifrFailed);
  Include(FErrorFlags,  ifeReadError);
end;

procedure TGDBInstruction.HandleTimeOut;
begin
  Include(FResultFlags, ifrFailed);
  Include(FErrorFlags, ifeTimedOut);
end;

procedure TGDBInstruction.HandleRecoveredTimeOut;
begin
  Include(FErrorFlags, ifeRecoveredTimedOut);
end;

procedure TGDBInstruction.HandleNoGdbRunning;
begin
  Include(FResultFlags, ifrFailed);
  Include(FErrorFlags,  ifeGdbNotRunning);
end;

function TGDBInstruction.GetTimeOutVerifier: TGDBInstruction;
begin
  if (ifeWriteError in ErrorFlags) then
    Result := nil
  else
    Result := TGDBInstructionVerifyTimeOut.Create(Self);
end;

procedure TGDBInstruction.Init;
begin
  //
end;

procedure TGDBInstruction.InternalCreate(ACommand: String; AThread, AFrame: Integer;
  AFlags: TGDBInstructionFlags; ATimeOut: Integer);
begin
  FCommand := ACommand;
  FThreadId   := AThread;
  FStackFrame := AFrame;
  FFlags := AFlags;
  FTimeOut := ATimeOut;
end;

constructor TGDBInstruction.Create(ACommand: String; AFlags: TGDBInstructionFlags;
  ATimeOut: Integer = 0);
begin
  InternalCreate(ACommand, -1, -1, AFlags, ATimeOut);
  Init;
end;

constructor TGDBInstruction.Create(ACommand: String; AThread: Integer;
  AOtherFlags: TGDBInstructionFlags; ATimeOut: Integer = 0);
begin
  InternalCreate(ACommand, AThread, -1,
                 AOtherFlags + [ifRequiresThread], ATimeOut);
  Init;
end;

constructor TGDBInstruction.Create(ACommand: String; AThread, AFrame: Integer;
  AOtherFlags: TGDBInstructionFlags; ATimeOut: Integer = 0);
begin
  InternalCreate(ACommand, AThread, AFrame,
                 AOtherFlags + [ifRequiresThread, ifRequiresStackFrame], ATimeOut);
  Init;
end;

function TGDBInstruction.IsSuccess: Boolean;
begin
  Result := ResultFlags * [ifrComleted, ifrFailed] = [ifrComleted]
end;

{ TGDBInstructionQueue }

procedure TGDBInstructionQueue.FinishCurrentInstruction;
var
  S: String;
  NewInstr: TGDBInstruction;
begin
  while (FCurrentInstruction <> nil) and
        (not FCurrentInstruction.IsCompleted)
  do begin
    if not FDebugger.DebugProcessRunning then begin
      FCurrentInstruction.HandleNoGdbRunning;
      break;
    end;

    S := FDebugger.ReadLine(FCurrentInstruction.TimeOut);
    // Readline, may go into Application.ProcessMessages.
    // If it does, it has not (yet) read any data.
    // Therefore, if it does, another nested call to readline will work, and data will be returned in the correct order.
    // If a nested readline reads all data, then the outer will have nothing to return.
    // TODO: need a flag, so the outer will immediately return empty.
    // TODO: also need a ReadlineCallCounter, to detect inner nested calls
    if (not FDebugger.ReadLineTimedOut) or (S <> '') then
      FCurrentInstruction.ProcessInputFromGdb(S);

    if (ehfGotReadError in FDebugger.FErrorHandlingFlags) then begin
      FCurrentInstruction.HandleReadError;
      break;
    end;
    if FDebugger.ReadLineTimedOut then begin
      NewInstr := FCurrentInstruction.GetTimeOutVerifier;
      if NewInstr <> nil then begin
        // TODO: Run NewInstr;
        FCurrentInstruction := NewInstr;
        FCurrentInstruction.SendCommandDataToGDB(Self);

      end
      else begin
        FCurrentInstruction.HandleTimeOut;
        break;
      end;
    end;

  end; // while
  if (FCurrentInstruction <> nil) and (ifAutoDestroy in FCurrentInstruction.Flags) then
    FCurrentInstruction.Free;
  FCurrentInstruction := nil;
end;

procedure TGDBInstructionQueue.SetCurrentInstruction(AnInstruction: TGDBInstruction);
begin
  FinishCurrentInstruction;
  FCurrentInstruction := AnInstruction;
end;

function TGDBInstructionQueue.SendDataToGDB(ASender: TGDBInstruction; AData: String): Boolean;
begin
  Result := True;
  FDebugger.FErrorHandlingFlags := FDebugger.FErrorHandlingFlags
    + [ehfDeferReadWriteError] - [ehfGotReadError, ehfGotWriteError];

  FDebugger.SendCmdLn(AData);

  if ehfGotWriteError in FDebugger.FErrorHandlingFlags then begin
    Result := False;
// TODO try reading, but ensure timeout
    if FCurrentInstruction <> nil then
      FCurrentInstruction.HandleWriteError(ASender)
    else
    if ASender <> nil then
      ASender.HandleWriteError(ASender);
  end;
end;

procedure TGDBInstructionQueue.SelectThread(AThreadId: Integer);
begin

end;

procedure TGDBInstructionQueue.SelectFrame(AFrame: Integer);
begin

end;

constructor TGDBInstructionQueue.Create(ADebugger: TGDBMICmdLineDebugger);
begin
  FDebugger := ADebugger;
end;

procedure TGDBInstructionQueue.InvalidateThredAndFrame;
begin
  FFlags := FFlags - [ifqValidThread, ifqValidStackFrame];
end;

procedure TGDBInstructionQueue.RunInstruction(AnInstruction: TGDBInstruction);
begin
  SetCurrentInstruction(AnInstruction);
  FCurrentInstruction.SendCommandDataToGDB(Self);
  FinishCurrentInstruction;
end;

initialization
  DBGMI_TIMEOUT_DEBUG := DebugLogger.RegisterLogGroup('DBGMI_TIMEOUT_DEBUG' {$IFDEF DBGMI_TIMEOUT_DEBUG} , True {$ENDIF} );

end.


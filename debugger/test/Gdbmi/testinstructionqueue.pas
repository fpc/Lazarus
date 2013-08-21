unit TestInstructionQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, DebugUtils, GDBTypeInfo, strutils, LCLProc,
  GDBMIDebugInstructions, LazLoggerBase;

type

  { TTestGdbInstructionQueue }

  TTestGdbInstructionQueue = class(TTestCase)
  private
  published
    procedure TestTimeout;
  end;


  TTestDbgControl = Record
    Action: (AEnd,
             aExpSend, aReadResp, aReadRespTimeOut,
             aKillDbgProcess);
    Data: String;
  end;
  PTestDbgControl = ^TTestDbgControl;

  { TTestDebugger }

  TTestDebugger = class(TGDBMICmdLineDebugger)
  private
    FTestDbgProcessRunning: Boolean;
    FTestData: PTestDbgControl;
    FTest: TTestGdbInstructionQueue;
    FTestReadLineTimedOut: Boolean;
  protected
    function GetDebugProcessRunning: Boolean; override;
    procedure SendCmdLn(const ACommand: String); override;
    function ReadLine(const APeek: Boolean; ATimeOut: Integer = - 1): String; override;
    function CreateDebugProcess(const AOptions: String): Boolean; override;
    function ReadLineTimedOut: Boolean; override;
  public
    procedure TestInit;
  end;

  TTestGDBInstruction = class(TGDBInstruction)
  protected
    FInput: String;
    function ProcessInputFromGdb(const AData: String): Boolean; override;
  end;

implementation

var
  DBG_CMD_ECHO, DBG_CMD_ECHO_FULL: PLazLoggerLogGroup;

{ TTestGDBInstruction }

function TTestGDBInstruction.ProcessInputFromGdb(const AData: String): Boolean;
begin
  if AData = '(gdb) ' then
    Include(FResultFlags, ifrComleted)
  else
    FInput := FInput + AData + LineEnding;
end;

{ TTestDebugger }

function TTestDebugger.GetDebugProcessRunning: Boolean;
begin
  Result := FTestDbgProcessRunning;
end;

procedure TTestDebugger.SendCmdLn(const ACommand: String);
begin
  if (DBG_CMD_ECHO_FULL <> nil) and (DBG_CMD_ECHO_FULL^.Enabled)
  then debugln(DBG_CMD_ECHO_FULL, '>> TCmdLineDebugger.SendCmdLn "',ACommand,'"')
  else debugln(DBG_CMD_ECHO,      '>> TCmdLineDebugger.SendCmdLn "',ACommand,'"');
  //If FTestData^.Action = AEnd then exit;
  FTest.AssertTrue('Action <> AEnd', FTestData^.Action <> AEnd);

  FTest.AssertTrue('Action = aExpSend', FTestData^.Action = aExpSend);
  FTest.AssertEquals('SendCmdLn()', FTestData^.Data, ACommand);
  inc(FTestData);
end;

function TTestDebugger.ReadLine(const APeek: Boolean; ATimeOut: Integer): String;
begin
  Result := '';
  FTestReadLineTimedOut := False;
  //If FTestData^.Action = AEnd then exit;
  FTest.AssertTrue('Action <> AEnd', FTestData^.Action <> AEnd);

  If FTestData^.Action = aReadResp then begin
    Result := FTestData^.Data;
    if ((DBG_CMD_ECHO_FULL <> nil) and (DBG_CMD_ECHO_FULL^. Enabled))
    then debugln(DBG_CMD_ECHO_FULL, '<< TCmdLineDebugger.ReadLn "',Result,'"')
    else if (length(Result) < 300)
    then debugln(DBG_CMD_ECHO, '<< TCmdLineDebugger.ReadLn "',Result,'"')
    else debugln(DBG_CMD_ECHO, ['<< TCmdLineDebugger.ReadLn "',copy(Result, 1, 200), '" ..(',length(Result)-250,').. "',copy(Result, length(Result)-99, 100),'"']);

    inc(FTestData);
    exit;
  end;

  If FTestData^.Action = aReadRespTimeOut then begin
    FTest.AssertTrue('can timeout', ATimeOut > 0);
    FTestReadLineTimedOut := True;
    debugln(DBG_CMD_ECHO_FULL, '<< TCmdLineDebugger.ReadLn -- TimeOut');
    inc(FTestData);
    exit;
  end;

  FTest.AssertTrue('Action = aReadResp', False);
end;

function TTestDebugger.CreateDebugProcess(const AOptions: String): Boolean;
begin
  Result := True;
end;

function TTestDebugger.ReadLineTimedOut: Boolean;
begin
  Result := FTestReadLineTimedOut;
end;

procedure TTestDebugger.TestInit;
begin
  FTestDbgProcessRunning := True;
end;

{ TTestGdbInstructionQueue }

const
  // No timeout
  TestControl1: array [0..3] of TTestDbgControl = (
    (Action: aExpSend; Data: '-test-send';),
    (Action: aReadResp; Data: '^done,foo';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: AEnd; Data: '';)
    );
  // Recover timeout
  TestControl2: array [0..9] of TTestDbgControl = (
    (Action: aExpSend; Data: '-test-send';),
    (Action: aReadResp; Data: '^done,foo';),
    (Action: aReadRespTimeOut; Data: '';),
    (Action: aExpSend; Data: '-data-evaluate-expression 7';),
    (Action: aExpSend; Data: '-data-evaluate-expression 1';),
    (Action: aReadResp; Data: '^done,value="7"';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: aReadResp; Data: '^done,value="1"';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: AEnd; Data: '';)
    );
  // late (gdb) / no timeout
  TestControl3: array [0..10] of TTestDbgControl = (
    (Action: aExpSend; Data: '-test-send';),
    (Action: aReadResp; Data: '^done,foo';),
    (Action: aReadRespTimeOut; Data: '';),
    (Action: aExpSend; Data: '-data-evaluate-expression 7';),
    (Action: aExpSend; Data: '-data-evaluate-expression 1';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: aReadResp; Data: '^done,value="7"';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: aReadResp; Data: '^done,value="1"';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: AEnd; Data: '';)
    );
  // late response + (gdb) / no timeout
  TestControl3A: array [0..10] of TTestDbgControl = (
    (Action: aExpSend; Data: '-test-send';),
    (Action: aReadRespTimeOut; Data: '';),
    (Action: aExpSend; Data: '-data-evaluate-expression 7';),
    (Action: aExpSend; Data: '-data-evaluate-expression 1';),
    (Action: aReadResp; Data: '^done,foo';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: aReadResp; Data: '^done,value="7"';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: aReadResp; Data: '^done,value="1"';),
    (Action: aReadResp; Data: '(gdb) ';),
    (Action: AEnd; Data: '';)
    );
  // timeout
  TestControl4: array [0..6] of TTestDbgControl = (
    (Action: aExpSend; Data: '-test-send';),
    (Action: aReadResp; Data: '^done,foo';),
    (Action: aReadRespTimeOut; Data: '';),
    (Action: aExpSend; Data: '-data-evaluate-expression 7';),
    (Action: aExpSend; Data: '-data-evaluate-expression 1';),
    (Action: aReadRespTimeOut; Data: '';),
    (Action: AEnd; Data: '';)
    );

procedure TTestGdbInstructionQueue.TestTimeout;
var
  Dbg: TTestDebugger;
  Queue: TGDBInstructionQueue;
  Instr: TTestGDBInstruction;
begin
  Dbg := TTestDebugger.Create('');
  Queue := TGDBInstructionQueue.Create(Dbg);

  // No timeout
  Instr := TTestGDBInstruction.Create('-test-send', [], 100);
  Instr.AddReference;
  Dbg.TestInit;
  dbg.FTest := Self;
  Dbg.FTestData := @TestControl1[0];
  Queue.RunInstruction(Instr);
  AssertTrue('ifrComleted', Instr.ResultFlags * [ifrComleted, ifrFailed] = [ifrComleted]);
  AssertTrue('no error', Instr.ErrorFlags = []);
  AssertEquals('data', '^done,foo'+LineEnding, Instr.FInput);
  Instr.ReleaseReference;

  // Recover timeout
  Instr := TTestGDBInstruction.Create('-test-send', [], 100);
  Instr.AddReference;
  Dbg.TestInit;
  dbg.FTest := Self;
  Dbg.FTestData := @TestControl2[0];
  Queue.RunInstruction(Instr);
  AssertTrue('ifrComleted', Instr.ResultFlags * [ifrComleted, ifrFailed] = [ifrComleted]);
  AssertTrue('no error, but warning', Instr.ErrorFlags = [ifeRecoveredTimedOut]);
  AssertEquals('data', '^done,foo'+LineEnding, Instr.FInput);
  Instr.ReleaseReference;

  // late (gdb) / no timeout
  Instr := TTestGDBInstruction.Create('-test-send', [], 100);
  Instr.AddReference;
  Dbg.TestInit;
  dbg.FTest := Self;
  Dbg.FTestData := @TestControl3[0];
  Queue.RunInstruction(Instr);
  AssertTrue('ifrComleted', Instr.ResultFlags * [ifrComleted, ifrFailed] = [ifrComleted]);
  AssertTrue('no error', Instr.ErrorFlags = []);
  AssertEquals('data', '^done,foo'+LineEnding, Instr.FInput);
  Instr.ReleaseReference;

  // late response + (gdb) / no timeout
  Instr := TTestGDBInstruction.Create('-test-send', [], 100);
  Instr.AddReference;
  Dbg.TestInit;
  dbg.FTest := Self;
  Dbg.FTestData := @TestControl3A[0];
  Queue.RunInstruction(Instr);
  AssertTrue('ifrComleted', Instr.ResultFlags * [ifrComleted, ifrFailed] = [ifrComleted]);
  AssertTrue('no error', Instr.ErrorFlags = []);
  AssertEquals('data', '^done,foo'+LineEnding, Instr.FInput);
  Instr.ReleaseReference;

  // timeout
  Instr := TTestGDBInstruction.Create('-test-send', [], 100);
  Instr.AddReference;
  Dbg.TestInit;
  dbg.FTest := Self;
  Dbg.FTestData := @TestControl4[0];
  Queue.RunInstruction(Instr);
  AssertTrue('ifrFailed', Instr.ResultFlags * [ifrComleted, ifrFailed] = [ifrFailed]);
  AssertTrue('error', Instr.ErrorFlags = [ifeTimedOut]);
  Instr.ReleaseReference;


  Queue.Free;
  Dbg.Free;
end;

initialization

  RegisterTest(TTestGdbInstructionQueue);
  DBG_CMD_ECHO      := DebugLogger.FindOrRegisterLogGroup('DBG_CMD_ECHO');
  DBG_CMD_ECHO_FULL := DebugLogger.FindOrRegisterLogGroup('DBG_CMD_ECHO_FULL');
end.


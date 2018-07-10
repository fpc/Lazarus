unit FPDServerDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  ssockets,
  fgl,
  forms,
  DbgIntfDebuggerBase,
  DbgIntfBaseTypes,
  maps,
  fpjson,
  jsonparser,
  BaseUnix,
  LazLoggerBase,
  process,
  dialogs,
  syncobjs,
  lazCollections,
  lazutf8sysutils,
  strutils,
  SysUtils, UTF8Process;

type
  TThreadedQueueString = specialize TLazThreadedQueue<string>;
  TFPDServerDebugger = class;

  { TFPDSendCommand }

  TFPDSendCommand = class
  protected
    FCommandUID: integer;
    FServerDebugger: TFPDServerDebugger;
    FAutomaticFree: boolean;
    function GetAsString: string; virtual;
    procedure ComposeJSon(AJsonObject: TJSONObject); virtual;
  public
    constructor create(AnAutomaticFree: boolean=true); virtual;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); virtual;
    procedure DoOnCommandReceived(ACommandResponse: TJSonObject); virtual;
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); virtual;
    property CommandUID: integer read FCommandUID;
    property AsString: string read GetAsString;
    property AutomaticFree: boolean read FAutomaticFree;
  end;

  { TFPDSendCommandList }

  TFPDCustomSendCommandList = specialize TFPGObjectList<TFPDSendCommand>;
  TFPDSendCommandList = class(TFPDCustomSendCommandList)
  public
    function SearchByUID(const ACommandUID: integer): TFPDSendCommand;
  end;

  { TFPDSendQuitDebugServerCommand }

  TFPDSendQuitDebugServerCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendRunCommand }

  TFPDSendRunCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendContinueCommand }

  TFPDSendContinueCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendNextCommand }

  TFPDSendNextCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepCommand }

  TFPDSendStepCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepIntoInstrCommand }

  TFPDSendStepIntoInstrCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepOverInstrCommand }

  TFPDSendStepOverInstrCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStopCommand }

  TFPDSendStopCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendStepOutCommand }

  TFPDSendStepOutCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  end;

  { TFPDSendFilenameCommand }

  TFPDSendFilenameCommand = class(TFPDSendCommand)
  private
    FFileName: string;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(AFileName: string); virtual;
  end;

  { TFPDSendAddBreakpointCommand }

  TFPDSendAddBreakpointCommand = class(TFPDSendCommand)
  private
    FFileName: string;
    FLineNr: integer;
    FLocation: TDBGPtr;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
    constructor create(AFileName: string; ALineNr: integer); virtual;
    constructor create(ALocation: TDBGPtr); virtual;
  end;

  { TFPDSendRemoveBreakpointCommand }

  TFPDSendRemoveBreakpointCommand = class(TFPDSendCommand)
  private
    FLocation: TDBGPtr;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(ALocation: TDBGPtr); virtual;
  end;

  { TFPDSendDoCurrentCommand }

  TFPDSendDoCurrentCommand = class(TFPDSendCommand)
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendEvaluateCommand }

  TFPDSendEvaluateCommand = class(TFPDSendCommand)
  private
    FExpression: string;
    FValidity: TDebuggerDataState;
    FMessage: string;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(AnAutomaticFree: boolean; AnExpression: string);
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
    property Validity: TDebuggerDataState read FValidity;
    property Message: string read FMessage;
  end;

  { TFPDSendWatchEvaluateCommand }

  TFPDSendWatchEvaluateCommand = class(TFPDSendCommand)
  private
    FWatchValue: TWatchValue;
    procedure DoWatchFreed(Sender: TObject);
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(AWatchValue: TWatchValue);
    destructor Destroy; override;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendCallStackCommand }

  TFPDSendCallStackCommand = class(TFPDSendCommand)
  private
    FCallStack: TCallStackBase;
    FCallStackSupplier: TCallStackSupplier;
    procedure DoCallStackFreed(Sender: TObject);
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(ACallStack: TCallStackBase; ACallStackSupplier: TCallStackSupplier);
    destructor Destroy; override;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendLocalsCommand }

  TFPDSendLocalsCommand = class(TFPDSendCommand)
  private
    FLocals: TLocals;
    procedure DoLocalsFreed(Sender: TObject);
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(ALocals: TLocals);
    destructor Destroy; override;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendRegistersCommand }

  TFPDSendRegistersCommand = class(TFPDSendCommand)
  private
    FRegisters: TRegisters;
    procedure DoRegistersFreed(Sender: TObject);
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(ARegisters: TRegisters);
    destructor Destroy; override;
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
    procedure DoOnCommandFailed(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSendDisassembleCommand }

  TFPDSendDisassembleCommand = class(TFPDSendCommand)
  private
    FDisassembler: TDBGDisassembler;
    FStartAddr: TDBGPtr;
    FLinesAfter: integer;
    FLinesBefore: integer;
  protected
    procedure ComposeJSon(AJsonObject: TJSONObject); override;
  public
    constructor create(ADisassembler: TDBGDisassembler; AStartAddr: TDBGPtr; ALinesBefore, ALinesAfter: integer);
    procedure DoOnCommandSuccesfull(ACommandResponse: TJSonObject); override;
  end;

  { TFPDSocketThread }

  TFPDSocketThread = class(TThread)
  private
    FPort: integer;
    FHostName: string;
    FConnectionIdentifier: integer;
    FDebugger: TFPDServerDebugger;
    FSendQueue: TThreadedQueueString;
    FErrMessage: string;
  protected
    procedure ReceivedCommand(Data: PtrInt);
    procedure ConnectionProblem(Data: PtrInt);
    procedure Execute; override;
  public
    constructor Create(ADebugger: TFPDServerDebugger; AHostName: string; APort: integer);
    procedure SendString(AString: string);
    destructor Destroy; override;
    property ConnectionIdentifier: integer read FConnectionIdentifier;
  end;

  { TFPDServerDebugger }

  TFPDServerDebugger = class(TDebuggerIntf)
  private
    FSocketThread: TFPDSocketThread;
    FDebugServerStartedAsChild: boolean;
    FIsConnected: boolean;
    FDebugProcess: TProcessUTF8;
    // This is a list of all commands send to the fpdebug-server, to handle the (asynchrounous)
    // callback when a command is a succes or failure.
    FCommandList: TFPDSendCommandList;
    function ConnectToFPDServer: boolean;
    procedure DisconnectFromFPDServer;
  protected
    // Overrides of TDebuggerIntf methods.
    function GetSupportedCommands: TDBGCommands; override;
    // Handle Notifications received from the FPDebug-server
    procedure HandleNotification(ANotification: TJSONObject);
    // Handle log-messages received from the FPDebug-server
    procedure HandleLog(ALog: TJSONObject);
    // Handle Events received from the FPDebug-server
    procedure HandleEvent(ANotification: TJSONObject);
    // Event-handlers for events received from the FPDebug-server
    procedure DoHandleCreateProcessEvent(AnEvent: TJSONObject);
    procedure DoHandleExitProcessEvent(AnEvent: TJSONObject);
    procedure DoHandleBreakpointEvent(AnEvent: TJSONObject);
    procedure DoHandleConsoleOutputEvent(AnEvent: TJSONObject);
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
    // This function is called (in the main thread) by the TFPDSocketThread on each JSon-object received
    // from the FPD-Server.
    procedure ReceivedCommand(ACommand: TJSONObject);
    // Queue a command for sending to the FPDebug-server.
    procedure QueueCommand(ACommand: TFPDSendCommand);
    // Overrides of TDebuggerIntf methods.
    class function Caption: String; override;
    function CreateBreakPoints: TDBGBreakPoints; override;
    function CreateWatches: TWatchesSupplier; override;
    function CreateLocals: TLocalsSupplier; override;
    function CreateRegisters: TRegisterSupplier; override;
    function CreateCallStack: TCallStackSupplier; override;
    function CreateDisassembler: TDBGDisassembler; override;
    function RequestCommand(const ACommand: TDBGCommand;
             const AParams: array of const;
             const ACallback: TMethod): Boolean; override;
    // These methods are called by several TFPDSendCommands after success or failure of a command. (Most common
    // because the TFPDSendCommands do not have access to TFPDServerDebugger's protected methods theirself)
    procedure DoOnRunFailed;
    procedure DoOnContinueSuccessfull;
    procedure DoOnDoCurrentSuccessfull(ALocRec: TDBGLocationRec);
    // This procedure is called when the socket-thread is shut-down.
    procedure DoOnConnectionProblem(AMessage: string);
  end;

procedure Register;

implementation

type

  { TFPBreakpoint }

  TFPBreakpoint = class(TDBGBreakPoint)
  private
    FSetBreakFlag: boolean;
    FResetBreakFlag: boolean;
    FIsSet: boolean;
    FUID: integer;
    procedure SetBreak;
    procedure ResetBreak;
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure DoEnableChange; override;
    procedure DoChanged; override;
    // Used in the succes or failure callbacks of the TFPDSendAddBreakpointCommand command to set the
    // validity of the underlying breakpoint.
    procedure SetValid;
    procedure SetInvalid;
  public
    destructor Destroy; override;
    property UID: integer read FUID;
  end;

  { TFPBreakpoints }

  TFPBreakpoints = class(TDBGBreakPoints)
  public
    function FindByUID(AnUID: integer): TFPBreakpoint;
  end;

  { TFPDBGDisassembler }

  TFPDBGDisassembler = class(TDBGDisassembler)
  protected
    function PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean; override;
  public
    // Used in the succes callback of the TFPDSendDisassembleCommand command to add
    // the retrieved range of assembly instructions.
    procedure AddRange(ARange: TDBGDisassemblerEntryRange);
  end;

  { TFPLocals }

  TFPLocals = class(TLocalsSupplier)
  public
    procedure RequestData(ALocals: TLocals); override;
  end;

  { TFPRegisters }

  TFPRegisters = class(TRegisterSupplier)
  public
    procedure RequestData(ARegisters: TRegisters); override;
  end;

  { TFPWatches }

  TFPWatches = class(TWatchesSupplier)
  protected
    procedure InternalRequestData(AWatchValue: TWatchValue); override;
  end;

  { TFPCallStackSupplier }

  TFPCallStackSupplier = class(TCallStackSupplier)
  public
    procedure RequestCount(ACallstack: TCallStackBase); override;
    procedure RequestEntries(ACallstack: TCallStackBase); override;
    procedure RequestCurrent(ACallstack: TCallStackBase); override;
    // Used in the succes callback of the TFPDSendCallStackCommand command to trigger
    // an update og the GUI after the callstack has been read.
    procedure DoUpdate;
  end;


procedure Register;
begin
  RegisterDebugger(TFPDServerDebugger);
end;

{ TFPDSendCommand }

var GCommandUID: integer = 0;

{ TFPRegisters }

procedure TFPRegisters.RequestData(ARegisters: TRegisters);
begin
  if (Debugger = nil) or not(Debugger.State = dsPause)
  then begin
    ARegisters.DataValidity:=ddsInvalid;
    exit;
  end;

  TFPDServerDebugger(Debugger).QueueCommand(TFPDSendRegistersCommand.create(ARegisters));
  ARegisters.DataValidity := ddsRequested;
end;

{ TFPDSendRegistersCommand }

procedure TFPDSendRegistersCommand.DoRegistersFreed(Sender: TObject);
begin
  FRegisters := nil;
end;

procedure TFPDSendRegistersCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','registers');
end;

constructor TFPDSendRegistersCommand.create(ARegisters: TRegisters);
begin
  inherited create(true);
  ARegisters.AddFreeNotification(@DoRegistersFreed);
  FRegisters := ARegisters;
end;

destructor TFPDSendRegistersCommand.Destroy;
begin
  if assigned(FRegisters) then
    FRegisters.RemoveFreeNotification(@DoRegistersFreed);
  inherited Destroy;
end;

procedure TFPDSendRegistersCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  JSonRegisterArr: TJSONArray;
  JSonRegisterEntryObj: TJSONObject;
  i: Integer;
  RegisterValue: TRegisterValue;
begin
  inherited DoOnCommandSuccesfull(ACommandResponse);
  if assigned(FRegisters) then
    begin
    FRegisters.Clear;

    JSonRegisterArr := ACommandResponse.Get('registers', TJSONArray(nil));
    if assigned(JSonRegisterArr) and (JSonRegisterArr.Count>0) then
      begin
      for i := 0 to JSonRegisterArr.Count - 1 do
        begin
        JSonRegisterEntryObj := JSonRegisterArr.Items[i] as TJSONObject;
        RegisterValue := FRegisters.EntriesByName[JSonRegisterEntryObj.Get('name', '')];
        RegisterValue.ValueObj.SetAsNum(JSonRegisterEntryObj.Get('numvalue', 0), JSonRegisterEntryObj.Get('size', 4));
        RegisterValue.ValueObj.SetAsText(JSonRegisterEntryObj.Get('value', ''));
        RegisterValue.DataValidity:=ddsValid;
        end;
      FRegisters.DataValidity := ddsValid;
      end
    else
      FRegisters.DataValidity := ddsInvalid;
    end;
end;

procedure TFPDSendRegistersCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  FRegisters.DataValidity := ddsInvalid;
end;

{ TFPDSendLocalsCommand }

procedure TFPDSendLocalsCommand.DoLocalsFreed(Sender: TObject);
begin
  FLocals:=nil;
end;

procedure TFPDSendLocalsCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','locals');
end;

constructor TFPDSendLocalsCommand.create(ALocals: TLocals);
begin
  inherited create(True);
  ALocals.AddFreeNotification(@DoLocalsFreed);
  FLocals := ALocals;
end;

destructor TFPDSendLocalsCommand.Destroy;
begin
  if assigned(FLocals) then
    FLocals.RemoveFreeNotification(@DoLocalsFreed);
  inherited Destroy;
end;

procedure TFPDSendLocalsCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  JSonLocalsArr: TJSONArray;
  JSonLocalsEntryObj: TJSONObject;
  i: Integer;
begin
  inherited DoOnCommandSuccesfull(ACommandResponse);
  if assigned(FLocals) then
    begin
    FLocals.Clear;
    JSonLocalsArr := ACommandResponse.Get('locals', TJSONArray(nil));
    if assigned(JSonLocalsArr) and (JSonLocalsArr.Count>0) then
      begin
      for i := 0 to JSonLocalsArr.Count - 1 do
        begin
        JSonLocalsEntryObj := JSonLocalsArr.Items[i] as TJSONObject;
        FLocals.Add(JSonLocalsEntryObj.Get('name', ''), JSonLocalsEntryObj.Get('value', ''));
        end;
      end;
    FLocals.SetDataValidity(ddsValid);
    end;
end;

procedure TFPDSendLocalsCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  FLocals.SetDataValidity(ddsInvalid);
end;

procedure TFPLocals.RequestData(ALocals: TLocals);
begin
  if (Debugger = nil) or not(Debugger.State = dsPause)
  then begin
    ALocals.SetDataValidity(ddsInvalid);
    exit;
  end;

  TFPDServerDebugger(Debugger).QueueCommand(TFPDSendLocalsCommand.create(ALocals));
  ALocals.SetDataValidity(ddsRequested);
end;

{ TFPDBGDisassembler }

function TFPDBGDisassembler.PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean;
begin
  TFPDServerDebugger(Debugger).QueueCommand(TFPDSendDisassembleCommand.create(self, AnAddr, ALinesBefore, ALinesAfter));
  result := false;
end;

procedure TFPDBGDisassembler.AddRange(ARange: TDBGDisassemblerEntryRange);
begin
  EntryRanges.AddRange(ARange);
  Changed;
end;

{ TFPCallStackSupplier }

procedure TFPCallStackSupplier.RequestCount(ACallstack: TCallStackBase);
begin
  if (Debugger = nil) or not(Debugger.State = dsPause)
  then begin
    ACallstack.SetCountValidity(ddsInvalid);
    exit;
  end;

  TFPDServerDebugger(Debugger).QueueCommand(TFPDSendCallStackCommand.create(ACallstack, Self));
  ACallstack.SetCountValidity(ddsRequested);
end;

procedure TFPCallStackSupplier.RequestEntries(ACallstack: TCallStackBase);
begin
  if (Debugger = nil) or not(Debugger.State = dsPause)
  then begin
    ACallstack.SetCountValidity(ddsInvalid);
    exit;
  end;
end;

procedure TFPCallStackSupplier.RequestCurrent(ACallstack: TCallStackBase);
begin
  ACallstack.CurrentIndex := 0;
  ACallstack.SetCurrentValidity(ddsValid);
end;

procedure TFPCallStackSupplier.DoUpdate;
begin
  Changed;
end;

{ TFPDSendWatchEvaluateCommand }

procedure TFPDSendWatchEvaluateCommand.DoWatchFreed(Sender: TObject);
begin
  FWatchValue:=nil;
end;

procedure TFPDSendWatchEvaluateCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','evaluate');
  AJsonObject.Add('expression',FWatchValue.Expression);
end;

constructor TFPDSendWatchEvaluateCommand.create(AWatchValue: TWatchValue);
begin
  inherited create(true);
  AWatchValue.AddFreeNotification(@DoWatchFreed);
  FWatchValue := AWatchValue;
end;

destructor TFPDSendWatchEvaluateCommand.Destroy;
begin
  FWatchValue.RemoveFreeNotification(@DoWatchFreed);
  inherited Destroy;
end;

procedure TFPDSendWatchEvaluateCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  s: string;
  i: TDebuggerDataState;
begin
  inherited DoOnCommandSuccesfull(ACommandResponse);

  if assigned(FWatchValue) then
    begin
    FWatchValue.Value:=ACommandResponse.Get('message','');
    s := ACommandResponse.Get('validity','');
    FWatchValue.Validity:=ddsError;
    for i := low(TDebuggerDataState) to high(TDebuggerDataState) do
      if DebuggerDataStateStr[i]=s then
        begin
        FWatchValue.Validity:=i;
        break;
        end;
    end;
end;

procedure TFPDSendWatchEvaluateCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  inherited DoOnCommandFailed(ACommandResponse);
  FWatchValue.Validity:=ddsInvalid;
end;

{ TFPWatches }

procedure TFPWatches.InternalRequestData(AWatchValue: TWatchValue);
begin
  TFPDServerDebugger(Debugger).QueueCommand(TFPDSendWatchEvaluateCommand.create(AWatchValue));
  inherited InternalRequestData(AWatchValue);
end;

{ TFPDSendEvaluateCommand }

procedure TFPDSendEvaluateCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','evaluate');
  AJsonObject.Add('expression',FExpression);
end;

constructor TFPDSendEvaluateCommand.create(AnAutomaticFree: boolean; AnExpression: string);
begin
  FExpression:=AnExpression;
  FValidity:=ddsRequested;
  inherited create(AnAutomaticFree);
end;

procedure TFPDSendEvaluateCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
var
  s: string;
  i: TDebuggerDataState;
begin
  inherited DoOnCommandSuccesfull(ACommandResponse);
  FMessage:=ACommandResponse.Get('message','');
  s := ACommandResponse.Get('validity','');
  FValidity:=ddsError;
  for i := low(TDebuggerDataState) to high(TDebuggerDataState) do
    if DebuggerDataStateStr[i]=s then
      begin
      FValidity:=i;
      break;
      end;
end;

procedure TFPDSendEvaluateCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  inherited DoOnCommandFailed(ACommandResponse);
  FValidity:=ddsInvalid;
end;

{ TFPDSendQuitDebugServerCommand }

procedure TFPDSendQuitDebugServerCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','quitdebugserver');
end;

{ TFPDSendRemoveBreakpointCommand }

procedure TFPDSendRemoveBreakpointCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  inherited ComposeJSon(AJsonObject);
  AJsonObject.Add('command','removebreakpoint');
  AJsonObject.Add('location', Dec2Numb(FLocation, 8, 16));
end;

constructor TFPDSendRemoveBreakpointCommand.create(ALocation: TDBGPtr);
begin
  inherited create;
  FLocation:=ALocation;
end;

{ TFPDSendResetBreakpointCommand }

function TFPDSendCommand.GetAsString: string;
var
  AJsonObject: TJSONObject;
begin
  AJsonObject := TJSONObject.Create;
  try
    ComposeJSon(AJsonObject);
    result := AJsonObject.AsJSON;
  finally
    AJsonObject.Free;
  end;
end;

procedure TFPDSendCommand.ComposeJSon(AJsonObject: TJSONObject);
begin
  AJsonObject.Add('uid', FCommandUID);
end;

constructor TFPDSendCommand.create(AnAutomaticFree: boolean);
begin
  inc(GCommandUID);
  FCommandUID := GCommandUID;
  FAutomaticFree:=AnAutomaticFree;
end;

procedure TFPDSendCommand.DoOnCommandSuccesfull(ACommandResponse: TJSonObject);
begin
  // Do nothing
end;

procedure TFPDSendCommand.DoOnCommandReceived(ACommandResponse: TJSonObject);
begin
  // Do nothing
end;

procedure TFPDSendCommand.DoOnCommandFailed(ACommandResponse: TJSonObject);
begin
  // Do nothing;
end;

{ TFPDSendCommandList }

function TFPDSendCommandList.SearchByUID(const ACommandUID: integer): TFPDSendCommand;
var
  i: Integer;
begin
  for i := 0 to count -1 do
  begin
    if Items[i].CommandUID = ACommandUID then
      begin
      result := items[i];
      exit;
      end;
  end;
  result := nil;
end;

{$I fpdserverdebuggercommands.inc}

{ TFPBreakpoint }

procedure TFPBreakpoint.SetBreak;
var
  ASendCommand: TFPDSendCommand;
begin
  ASendCommand := nil;
  case Kind of
    bpkAddress:  ASendCommand := TFPDSendAddBreakpointCommand.create(Address);
    bpkSource:   ASendCommand := TFPDSendAddBreakpointCommand.create(Source, Line);
  else
    Raise Exception.Create('Breakpoints of this kind are not suported.');
  end;
  if assigned(ASendCommand) then
    begin
    FUID:=ASendCommand.CommandUID;
    TFPDServerDebugger(Debugger).QueueCommand(ASendCommand);
    end;
  FIsSet:=true;
end;

procedure TFPBreakpoint.ResetBreak;
begin
  if assigned(Debugger) then
    TFPDServerDebugger(Debugger).QueueCommand(TFPDSendRemoveBreakpointCommand.create(Address));
  FIsSet:=false;
end;

procedure TFPBreakpoint.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger.State in [dsPause, dsInternalPause]) then
    begin
    if Enabled and not FIsSet then
      begin
      FSetBreakFlag:=true;
      Changed;
      end
    else if not enabled and FIsSet then
      begin
      FResetBreakFlag:=true;
      Changed;
      end;
    end
  else if Debugger.State = dsStop then
    begin
    FIsSet:=false;
    end;
  inherited DoStateChange(AOldState);
end;

procedure TFPBreakpoint.DoEnableChange;
begin
  if (Debugger.State in [dsPause, dsInit, dsRun]) then
    begin
    if Enabled and not FIsSet then
      FSetBreakFlag := True
    else if not Enabled and FIsSet then
      FResetBreakFlag := True;
    end;
  inherited;
end;

procedure TFPBreakpoint.DoChanged;
begin
  if FResetBreakFlag and not FSetBreakFlag then
    ResetBreak
  else if FSetBreakFlag then
    SetBreak;

  FSetBreakFlag := false;
  FResetBreakFlag := false;

  inherited DoChanged;
end;

procedure TFPBreakpoint.SetValid;
begin
  FValid:=vsValid;
end;

procedure TFPBreakpoint.SetInvalid;
begin
  FValid:=vsInvalid;
end;

destructor TFPBreakpoint.Destroy;
begin
  if FIsSet then
    ResetBreak;
  inherited Destroy;
end;

function TFPBreakpoints.FindByUID(AnUID: integer): TFPBreakpoint;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    if TFPBreakpoint(Items[i]).UID=AnUID then
      begin
      result := TFPBreakpoint(Items[i]);
      exit;
      end;
  result := nil;
end;

{ TFPDSocketThread }

procedure TFPDSocketThread.ReceivedCommand(Data: PtrInt);
var
  ACommand: TJSONObject;
begin
  ACommand := TObject(data) as TJSONObject;
  FDebugger.ReceivedCommand(ACommand);
  ACommand.Free;
end;

procedure TFPDSocketThread.ConnectionProblem(Data: PtrInt);
begin
  FDebugger.DoOnConnectionProblem(FErrMessage);
end;

procedure TFPDSocketThread.Execute;
const
  InputBufferSize = 1024;
var
  SendStr: string;
  s: string;
  i: integer;
  InputStr: string;
  JSonData: TJSONData;
  ASocket: TInetSocket;

  function ReadString: string;
  var
    InputBuffer: array[0..InputBufferSize-1] of char;
    s: string;
  begin
    // First see if there is a string left in the input-buffer.
    i := pos(#10, InputStr);
    if i > 0 then
      begin
      s := copy(InputStr, 1, i-1);
      delete(InputStr,1,i);
      result := s;
      exit;
      end;

    result := '';
    i := ASocket.Read(InputBuffer[0], InputBufferSize-1);
    if i=0 then
      begin
      // Connection closed
      FErrMessage := 'Connection with FPDebug-server closed.';
      Terminate;
      end
    else if i<0 then
      begin
      if ASocket.LastError<>35 {EAGAIN} then
        begin
        FErrMessage := 'Error during write to FPDebug-server. Socket-error: '+inttostr(ASocket.LastError);
        Terminate;
        end;
      end
    else if i > 0 then
      begin
      setlength(s,i);
      move(InputBuffer[0],s[1],i);
      InputStr:=InputStr+s;
      i := pos(#10, InputStr);
      if i > 0 then
        begin
        s := copy(InputStr, 1, i-1);
        delete(InputStr,1,i);
        result := s;
        end;
      end;
  end;

  function ReadSTringTimeout(ATimeout: integer): string;
  var
    tc: int64;
  begin
    tc := GetTickCount64;
    result := ReadString;
    while not terminated and (result='') and ((GetTickCount64-tc)<ATimeout) do
      begin
      sleep(1);
      result := ReadString;
      end;
  end;

var
  IsConnected: boolean;
begin
  IsConnected:=false;
  FErrMessage:='';
  try
    ASocket := TInetSocket.Create(FHostName, FPort);
    try
      if not assigned(ASocket) then
        begin
        FErrMessage:='Failed to connect to fpdebug-server at '+FHostName+':'+IntToStr(FPort);
        Terminate;
        end
      else
        begin
        // Set non-blocking
        fpfcntl(ASocket.Handle,F_SETFL,O_NONBLOCK);

        // Read and check FPDebug Server greeting
        s := ReadSTringTimeout(100);
        if s='Welcome to FPDebug-server.' then
          begin
          // Read connection-identifier
          s := ReadSTringTimeout(100);
          delete(s,length(s),1);
          s := copy(s, rpos(' ',s)+1, 5);
          FConnectionIdentifier:=StrToIntDef(s,-1);
          if FConnectionIdentifier>-1 then
            begin
            // Skip help-message
            s := ReadSTringTimeout(100);
            IsConnected:=True;
            end;
          end;

        if not IsConnected then
          begin
          FErrMessage:='Connected to '+FHostName+':'+inttostr(FPort)+', but failed to negotiate handshake.';
          Terminate;
          end;
        end;

      while not terminated do
        begin
        repeat
        s:=ReadString;
        if s<>'' then
          begin
          JSonData := GetJSON(s);
          if JSonData is TJSONObject then
            Application.QueueAsyncCall(@ReceivedCommand, ptrint(JSonData))
          else
            raise exception.CreateFmt('JSon-command %s is not a JSON-Object.',[s]);
          end;
        // When one string has been received, there is a large chance that there is more
        // input waiting. Keep checking for input until the input-buffer is empty, before
        // the thread starts waiting for new commands using FSendQueue.PopItem.
        until s='';

        if not terminated and (FSendQueue.PopItem(SendStr) = wrSignaled) then
          begin
          SendStr := SendStr + #10;
          i := ASocket.Write(SendStr[1], length(SendStr));

          if i < 0 then
            begin
            if ASocket.LastError=32 then
              begin
              // Lost connection
              end
            else
              DebugLn(Format('Error during write. Socket-error: %d',[ASocket.LastError]));
            Terminate;
            end
          else if i < length(SendStr) then
            raise exception.create('Message has not been send to client entirely');
          end;
        end;
    finally
      ASocket.Free;
    end;
  except
    on E: Exception do
      begin
      FErrMessage:='Exception on connection with FPDebug-server: ' + E.Message;
      end;
  end;

  // There are two different ways in which the thread can terminate:
  // 1: The thread terminates itself, due to a lost connection or similar problem. In that case the
  //    thread is freed in the TFPDServerDebugger.DoConnectionProblem method.
  // 2: TFPDServerDebugger.Destroy terminates the thread. In that case it will also free the thread, and
  //    the asynchrounous call to ConnectionProblem is removed from the async-queue.
  Application.QueueAsyncCall(@ConnectionProblem, 0);
end;

constructor TFPDSocketThread.Create(ADebugger: TFPDServerDebugger;
  AHostName: string; APort: integer);
begin
  FHostName:=AHostName;
  FPort:=APort;
  FDebugger := ADebugger;
  FSendQueue:=TThreadedQueueString.create(100, INFINITE, 100);
  inherited create(false);
end;

procedure TFPDSocketThread.SendString(AString: string);
begin
  if Assigned(FDebugger.OnDbgOutput) then
    FDebugger.OnDbgOutput(Self, 'send: '+AString);
  FSendQueue.PushItem(AString);
end;

destructor TFPDSocketThread.Destroy;
begin
  FSendQueue.Free;
  Application.RemoveAsyncCalls(Self);
  inherited destroy;
end;

{ TFPDServerDebugger }

function TFPDServerDebugger.ConnectToFPDServer: boolean;
var
  buff,s: string;
  dw: dword;
  tc: Int64;
  js: TJSONData;
  port: integer;
begin
  if not FIsConnected then
    begin
    result := false;
    port := -1;
    if pos('gdb', LowerCase(ExtractFileName(ExternalDebugger)))>0 then
      ShowMessage('The name of the external debugger contains ''gdb''. The currently selected FPDebug-debugger can not work in combination with gdb. The debugger will most likely fail to start.');
    FDebugProcess := TProcessUTF8.Create(nil);
    try
      try
        FDebugProcess.Executable:=ExternalDebugger;
        FDebugProcess.Options:=[poUsePipes, poNoConsole, poNewProcessGroup];
        FDebugProcess.Parameters.Add('--tcp');
        FDebugProcess.Parameters.Add('--daemon');
        FDebugProcess.Parameters.Add('--autoport');
        FDebugProcess.Parameters.Add('--interactive');
        FDebugProcess.ShowWindow:=swoNone;

        DoDbgOutput('Start debugger: '+FDebugProcess.Executable + ' ' + StringReplace(FDebugProcess.Parameters.Text,LineEnding,' ',[rfReplaceAll]));

        FDebugProcess.Execute;
        // Wait and scan output for tcp/ip port number
        s := '';
        buff := '';
        dw := 0;
        tc := GetTickCount64;
        while FDebugProcess.Running and ((GetTickCount64-tc)<5000) and (dw<1) do
          begin
          dw := FDebugProcess.Output.NumBytesAvailable;
          if dw > 0 then
            begin
            setlength(buff, dw);
            FDebugProcess.Output.ReadBuffer(buff[1], dw);
            s := s + buff;
            dw := pos(#10,s);
            end;
          sleep(5);
          end;
        if dw>0 then
          begin
          s := copy(s,1,dw);

          DoDbgOutput('recv stdin: '+S);

          js := GetJSON(s);
          try
            if js.JSONType=jtObject then
              port := TJSONObject(js).Get('port',-1);
          finally
            js.Free;
          end;
          if port<1 then
            ShowMessage('No valid TCP/IP port to bind to FPDebug Server');
          end
        else
          ShowMessage('Invalid response from FPDebug Server');
      except
        on E: Exception do
          ShowMessage('Failed to run FPDebug Server: '+E.Message);
      end;
    finally
      if port<1 then
        FDebugProcess.Free;
    end;

    if port>-1 then
      begin
      FSocketThread := TFPDSocketThread.Create(Self, '127.0.0.1', port);
      FDebugServerStartedAsChild:=true;
      FIsConnected:=true;
      result := true;
      end;
    end
  else
    result := true;
end;

procedure TFPDServerDebugger.DisconnectFromFPDServer;
begin
  if FDebugServerStartedAsChild then
    begin
    // Try to send the FPDebug server the command to terminate. It could be that
    // the server is already gone, but try anyway and give it some time to terminate
    // by itself.
    QueueCommand(TFPDSendQuitDebugServerCommand.create);
    WaitForThreadTerminate(FSocketThread.Handle, 1000);
    end;

  FSocketThread.Terminate;
  FSocketThread.WaitFor;
  FSocketThread.Free;

  if FDebugServerStartedAsChild then
    begin
    if FDebugProcess.Running then
      FDebugProcess.Terminate(1);
    FDebugProcess.Free;
    end;
end;

function TFPDServerDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result:=[dcRun, dcStepOver, dcStepInto, dcStepOut, dcStepOverInstr, dcStepIntoInstr, dcStop, dcEvaluate];
end;

procedure TFPDServerDebugger.DoHandleCreateProcessEvent(AnEvent: TJSONObject);
begin
  SetState(dsInternalPause);
  QueueCommand(TFPDSendContinueCommand.create);
end;

procedure TFPDServerDebugger.DoHandleExitProcessEvent(AnEvent: TJSONObject);
begin
  SetState(dsStop);
end;

procedure TFPDServerDebugger.DoHandleBreakpointEvent(AnEvent: TJSONObject);
var
  BrkLocation: string;
  Brk: TDBGBreakPoint;
  Continue: boolean;
begin
  BrkLocation:=AnEvent.Get('breakpointLocation','');
  if BrkLocation<>'' then
    begin
    Brk :=  BreakPoints.Find(Hex2Dec(BrkLocation));
    if not assigned(brk) then
      debugln('Break on unknown breakpoint')
    else
      begin
      brk.Hit(Continue);
      if Continue then
        begin
        QueueCommand(TFPDSendContinueCommand.create);
        Exit;
        end;
      end;
    end;

  SetState(dsPause);
  QueueCommand(TFPDSendDoCurrentCommand.create);
end;

procedure TFPDServerDebugger.DoHandleConsoleOutputEvent(AnEvent: TJSONObject);
var
  AMessage: string;
begin
  AMessage:=AnEvent.Get('message','');
  OnConsoleOutput(Self, AMessage);
end;

procedure TFPDServerDebugger.HandleNotification(ANotification: TJSONObject);
var
  NotificationType: string;
  UID: integer;
  SendCommand: TFPDSendCommand;
begin
  // Ignore notifications from other connections
  if ANotification.get('connIdentifier',-1)=FSocketThread.ConnectionIdentifier then
    begin
    NotificationType:=ANotification.Get('notificationType','');
    case NotificationType of
      'InvalidCommand':
        raise exception.CreateFmt('The FPD-Server complains about an invalid command: %s',[ANotification.get('message', '-')]);
      'ExecutedCommand', 'FailedCommand', 'ReceivedCommand':
        begin
        uid := ANotification.get('uid',-1);
        if uid > -1 then
          begin
          SendCommand := FCommandList.SearchByUID(uid);
          if assigned(SendCommand) then
            begin
            case NotificationType of
              'ExecutedCommand':
                begin
                SendCommand.DoOnCommandSuccesfull(ANotification);
                if SendCommand.AutomaticFree then
                  FCommandList.Remove(SendCommand)
                else
                  FCommandList.Extract(SendCommand);
                end;
              'FailedCommand'  :
                begin
                SendCommand.DoOnCommandFailed(ANotification);
                if SendCommand.AutomaticFree then
                  FCommandList.Remove(SendCommand)
                else
                  FCommandList.Extract(SendCommand);
                end;
              'ReceivedCommand':
                SendCommand.DoOnCommandReceived(ANotification);
            end; {case}
            end
          else
            debugln('Received command-notification for unknown command-uid '+inttostr(UID));
          end
        else
          debugln('Received command notification without UID');
        end;
    end; {case}
    end;
end;

procedure TFPDServerDebugger.HandleLog(ALog: TJSONObject);
var
  LogType: string;
  Message: string;
begin
  LogType:=ALog.get('logType','');
  Message:=ALog.Get('message','');
  case LogType of
    'debug'  : DebugLn(Message);
    'info'   : ShowMessage(Message);
    'error'  : raise Exception.Create(Message);
  else
    raise Exception.CreateFmt('Received unknown log-type from FPDebug-server. (%s)', [LogType]);
  end; {case}
end;

procedure TFPDServerDebugger.HandleEvent(ANotification: TJSONObject);
var
  EventName: string;
begin
  EventName:=ANotification.get('eventName','');
  case EventName of
    'CreateProcess' : DoHandleCreateProcessEvent(ANotification);
    'ExitProcess'   : DoHandleExitProcessEvent(ANotification);
    'BreakPoint'    : DoHandleBreakPointEvent(ANotification);
    'ConsoleOutput' : DoHandleConsoleOutputEvent(ANotification);
  else
    debugln('Received unknown event: '+EventName);
  end;
end;

procedure TFPDServerDebugger.QueueCommand(ACommand: TFPDSendCommand);
begin
  ACommand.FServerDebugger := self;
  FCommandList.Add(ACommand);
  FSocketThread.SendString(ACommand.AsString);
end;

constructor TFPDServerDebugger.Create(const AExternalDebugger: String);
begin
  inherited Create(AExternalDebugger);
  FCommandList := TFPDSendCommandList.Create(true);
end;

destructor TFPDServerDebugger.Destroy;
begin
  if FIsConnected then
    DisconnectFromFPDServer;
  FCommandList.Free;
  inherited Destroy;
end;

procedure TFPDServerDebugger.ReceivedCommand(ACommand: TJSONObject);
var
  TypeStr: string;
begin
  DoDbgOutput('recv: '+ACommand.AsJSON);

  TypeStr := ACommand.Get('type','');
  case TypeStr of
    'event'        : HandleEvent(ACommand);
    'log'          : HandleLog(ACommand);
    'notification' : HandleNotification(ACommand);
  else
    raise Exception.CreateFmt('Received unknown event-type. (%s)',[TypeStr]);
  end;
end;

class function TFPDServerDebugger.Caption: String;
begin
  Result:='FpDebug external Dwarf-debugger (fpdserver, alpha)';
end;

function TFPDServerDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TFPBreakPoints.Create(Self, TFPBreakpoint);
end;

function TFPDServerDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TFPWatches.Create(Self);
end;

function TFPDServerDebugger.CreateLocals: TLocalsSupplier;
begin
  Result := TFPLocals.Create(Self);
end;

function TFPDServerDebugger.CreateRegisters: TRegisterSupplier;
begin
  Result:=TFPRegisters.Create(Self);
end;

function TFPDServerDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result:=TFPCallStackSupplier.Create(Self);
end;

function TFPDServerDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result:=TFPDBGDisassembler.Create(Self);
end;

function TFPDServerDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const; const ACallback: TMethod): Boolean;
var
  ASendCommand: TFPDSendEvaluateCommand;
  tc: qword;
begin
  result := true;
  case ACommand of
    dcRun:
      begin
      if State in [dsPause, dsInternalPause] then
        begin
        QueueCommand(TFPDSendContinueCommand.create);
        end
      else
        begin
        result := ConnectToFPDServer;
        if result then
          begin
          QueueCommand(TFPDSendFilenameCommand.create(FileName));
          QueueCommand(TFPDSendRunCommand.create);
          SetState(dsInit);
          end
        else
          SetState(dsStop);
        end;
      end;
    dcStepOver:
      begin
      QueueCommand(TFPDSendNextCommand.create);
      SetState(dsRun);
      end;
    dcStepInto:
      begin
      QueueCommand(TFPDSendStepCommand.create);
      SetState(dsRun);
      end;
    dcStepIntoInstr:
      begin
      QueueCommand(TFPDSendStepIntoInstrCommand.create);
      SetState(dsRun);
      end;
    dcStepOverInstr:
      begin
      QueueCommand(TFPDSendStepOverInstrCommand.create);
      SetState(dsRun);
      end;
    dcStepOut:
      begin
      QueueCommand(TFPDSendStepOutCommand.create);
      SetState(dsRun);
      end;
    dcStop:
      begin
      QueueCommand(TFPDSendStopCommand.create);
      if state=dsPause then
        SetState(dsRun);
      end;
    dcEvaluate:
      begin
      ASendCommand := TFPDSendEvaluateCommand.create(False, String(AParams[0].VAnsiString));
      QueueCommand(ASendCommand);
      tc := GetTickCount64;
      repeat
      sleep(5);
      Application.ProcessMessages;
      until (ASendCommand.Validity<>ddsRequested) or ((GetTickCount64-tc)>2000);
      TDBGEvaluateResultCallback(ACallback)(Self, True, ASendCommand.Message, nil);
      Result := True;
      ASendCommand.Free;
      end
    else
      result := false;
  end;
end;

procedure TFPDServerDebugger.DoOnRunFailed;
begin
  // TDebuggerIntf.SetFileName has set the state to dsStop, to make sure
  // that dcRun could be requested. Reset the filename so that the state
  // is set to dsIdle again and is set to dsStop on the next try
  // to run.
  FileName := ''
end;

procedure TFPDServerDebugger.DoOnContinueSuccessfull;
begin
  SetState(dsRun);
end;

procedure TFPDServerDebugger.DoOnDoCurrentSuccessfull(ALocRec: TDBGLocationRec);
begin
  DoCurrent(ALocRec);
end;

procedure TFPDServerDebugger.DoOnConnectionProblem(AMessage: string);
begin
  if AMessage<>'' then
    ShowMessage(AMessage);
  FIsConnected:=false;
  DisconnectFromFPDServer;
  SetState(dsStop);
end;

end.


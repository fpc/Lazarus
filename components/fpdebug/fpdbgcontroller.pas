unit FPDbgController;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Maps,
  FpDbgUtil,
  LazLogger,
  FpDbgClasses;

type

  TOnCreateProcessEvent = procedure(var continue: boolean) of object;
  TOnHitBreakpointEvent = procedure(var continue: boolean) of object;
  TOnExceptionEvent = procedure(var continue: boolean) of object;
  TOnProcessExitEvent = procedure(ExitCode: DWord) of object;

  { TDbgController }

  TDbgController = class
  private
    FExecutableFilename: string;
    FOnCreateProcessEvent: TOnCreateProcessEvent;
    FOnDebugInfoLoaded: TNotifyEvent;
    FOnExceptionEvent: TOnExceptionEvent;
    FOnHitBreakpointEvent: TOnHitBreakpointEvent;
    FOnLog: TOnLog;
    FOnProcessExitEvent: TOnProcessExitEvent;
    FProcessMap: TMap;
    FExitCode: DWord;
    FPDEvent: TFPDEvent;
    procedure SetExecutableFilename(AValue: string);
    procedure SetOnLog(AValue: TOnLog);
    procedure DoOnDebugInfoLoaded(Sender: TObject);
  protected
    FMainProcess: TDbgProcess;
    FCurrentProcess: TDbgProcess;
    FCurrentThread: TDbgThread;
    procedure Log(AString: string);
    procedure Log(AString: string; Options: array of const);
    function GetProcess(const AProcessIdentifier: THandle; out AProcess: TDbgProcess): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Run: boolean;
    procedure Stop;
    procedure StepIntoStr;
    procedure ProcessLoop;
    procedure SendEvents(out continue: boolean);

    property ExecutableFilename: string read FExecutableFilename write SetExecutableFilename;
    property OnLog: TOnLog read FOnLog write SetOnLog;
    property CurrentProcess: TDbgProcess read FCurrentProcess;
    property MainProcess: TDbgProcess read FMainProcess;

    property OnCreateProcessEvent: TOnCreateProcessEvent read FOnCreateProcessEvent write FOnCreateProcessEvent;
    property OnHitBreakpointEvent: TOnHitBreakpointEvent read FOnHitBreakpointEvent write FOnHitBreakpointEvent;
    property OnProcessExitEvent: TOnProcessExitEvent read FOnProcessExitEvent write FOnProcessExitEvent;
    property OnExceptionEvent: TOnExceptionEvent read FOnExceptionEvent write FOnExceptionEvent;
    property OnDebugInfoLoaded: TNotifyEvent read FOnDebugInfoLoaded write FOnDebugInfoLoaded;
  end;

implementation

{ TDbgController }

procedure TDbgController.DoOnDebugInfoLoaded(Sender: TObject);
begin
  if Assigned(FOnDebugInfoLoaded) then
    FOnDebugInfoLoaded(Self);
end;

procedure TDbgController.SetExecutableFilename(AValue: string);
begin
  if FExecutableFilename=AValue then Exit;
  FExecutableFilename:=AValue;
end;

procedure TDbgController.SetOnLog(AValue: TOnLog);
begin
  if FOnLog=AValue then Exit;
  FOnLog:=AValue;
  //if Assigned(FCurrentProcess) then
  //  FCurrentProcess.OnLog:=FOnLog;
end;

destructor TDbgController.Destroy;
begin
  //FCurrentProcess.Free;
  inherited Destroy;
end;

function TDbgController.Run: boolean;
begin
  result := False;
  if assigned(FMainProcess) then
    begin
    Log('The debuggee is already running');
    Exit;
    end;

  if FExecutableFilename = '' then
    begin
    Log('No filename given to execute.');
    Exit;
    end;

  if not FileExists(FExecutableFilename) then
    begin
    Log('File %s does not exist.',[FExecutableFilename]);
    Exit;
    end;

  FCurrentProcess := OSDbgClasses.DbgProcessClass.StartInstance(FExecutableFilename, '');
  if assigned(FCurrentProcess) then
    begin
    FCurrentProcess.OnDebugInfoLoaded := @DoOnDebugInfoLoaded;
    FCurrentProcess.OnLog:=OnLog;
    Log('Got PID: %d, TID: %d', [FCurrentProcess.ProcessID, FCurrentProcess.ThreadID]);
    result := true;
    end;
end;

procedure TDbgController.Stop;
begin
  FMainProcess.TerminateProcess;
end;

procedure TDbgController.StepIntoStr;
begin
  FCurrentThread.SingleStep;
end;

procedure TDbgController.ProcessLoop;

var
  AFirstLoop: boolean;
  AProcessIdentifier: THandle;
  AThreadIdentifier: THandle;
  AExit: boolean;

begin
  AExit:=false;
  repeat
    if assigned(FCurrentProcess) and not assigned(FMainProcess) then
      begin
      FMainProcess:=FCurrentProcess;
      AFirstLoop:=true;
      end
    else
      begin
      AFirstLoop:=false;
      FCurrentProcess.Continue(FCurrentProcess, FCurrentThread);
      end;

    if not FCurrentProcess.WaitForDebugEvent(AProcessIdentifier, AThreadIdentifier) then Continue;

    FCurrentProcess := nil;
    FCurrentThread := nil;
    if not GetProcess(AProcessIdentifier, FCurrentProcess) and not AFirstLoop then Continue;

    if AFirstLoop then
      FCurrentProcess := FMainProcess;

    if not FCurrentProcess.GetThread(AThreadIdentifier, FCurrentThread)
    then Log('LOOP: Unable to retrieve current thread');

    FPDEvent:=FCurrentProcess.ResolveDebugEvent(FCurrentThread);
    FCurrentThread.SingleStepping:=false;
    case FPDEvent of
      deCreateProcess :
        begin
          FProcessMap.Add(AProcessIdentifier, FCurrentProcess);
        end;
      deExitProcess :
        begin
          if FCurrentProcess = FMainProcess then FMainProcess := nil;
          FExitCode:=FCurrentProcess.ExitCode;

          FProcessMap.Delete(AProcessIdentifier);
          FCurrentProcess.Free;
          FCurrentProcess := nil;
        end;
      deLoadLibrary :
        begin
          {if FCurrentProcess.GetLib(FCurrentProcess.LastEventProcessIdentifier, ALib)
          and (GImageInfo <> iiNone)
          then begin
            WriteLN('Name: ', ALib.Name);
            //if GImageInfo = iiDetail
            //then DumpPEImage(Proc.Handle, Lib.BaseAddr);
          end;
          if GBreakOnLibraryLoad
          then GState := dsPause;
          }
        end;
      deBreakpoint :
        begin
          debugln('Reached breakpoint at %s.',[FormatAddress(FCurrentProcess.GetInstructionPointerRegisterValue)]);
        end;
    end; {case}
    AExit:=true;
  until AExit;
end;

procedure TDbgController.SendEvents(out continue: boolean);
begin
  case FPDEvent of
    deCreateProcess:
      begin
        debugln('Create Process');
        continue:=true;
        if assigned(OnCreateProcessEvent) then
          OnCreateProcessEvent(continue);
      end;
    deBreakpoint:
      begin
        debugln('Breakpoint');
        continue:=false;
        if assigned(OnHitBreakpointEvent) then
          OnHitBreakpointEvent(continue);
      end;
    deExitProcess:
      begin
        debugln('Exit proces');
        continue := false;
        if assigned(OnProcessExitEvent) then
          OnProcessExitEvent(FExitCode);
      end;
    deException:
      begin
        debugln('Exception');
        continue:=false;
        if assigned(OnExceptionEvent) then
          OnExceptionEvent(continue);
      end;
    deLoadLibrary:
      begin
        debugln('LoadLibrary');
        continue:=true;
      end;
    deInternalContinue:
      begin
        debugln('Internal');
        continue := true;
      end;
    else
      raise exception.create('Unknown debug controler state');
  end;
end;

procedure TDbgController.Log(AString: string);
begin
  if assigned(@FOnLog) then
    FOnLog(AString)
  else
    DebugLn(AString);
end;

procedure TDbgController.Log(AString: string; Options: array of const);
begin
  OnLog(Format(AString, Options));
end;

function TDbgController.GetProcess(const AProcessIdentifier: THandle; out AProcess: TDbgProcess): Boolean;
begin
  Result := FProcessMap.GetData(AProcessIdentifier, AProcess) and (AProcess <> nil);
end;

constructor TDbgController.Create;
begin
  FProcessMap := TMap.Create(itu4, SizeOf(TDbgProcess));
end;

end.


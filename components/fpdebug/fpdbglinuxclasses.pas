unit FpDbgLinuxClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  BaseUnix,
  process,
  FpDbgClasses,
  FpDbgLoader,
  DbgIntfBaseTypes,
  FpDbgLinuxExtra,
  FpDbgInfo,
  FpDbgUtil,
  LazLoggerBase;

type
  user_regs_struct64 = record
    r15: cuint64;
    r14: cuint64;
    r13: cuint64;
    r12: cuint64;
    rbp: cuint64;
    rbx: cuint64;
    r11: cuint64;
    r10: cuint64;
    r9 : cuint64;
    r8 : cuint64;
    rax: cuint64;
    rcx: cuint64;
    rdx: cuint64;
    rsi: cuint64;
    rdi: cuint64;
    orig_rax: cuint64;
    rip: cuint64;
    cs : cuint64;
    eflags: cuint64;
    rsp: cuint64;
    ss : cuint64;
    fs_base: cuint64;
    gs_base: cuint64;
    ds : cuint64;
    es : cuint64;
    fs : cuint64;
    gs : cuint64;
  end;

  user_regs_struct32 = record
    ebx: cuint32;
    ecx: cuint32;
    edx: cuint32;
    esi: cuint32;
    edi: cuint32;
    ebp: cuint32;
    eax: cuint32;
    xds: cuint32;
    xes: cuint32;
    xfs: cuint32;
    xgs: cuint32;
    orig_eax: cuint32;
    eip: cuint32;
    xcs: cuint32;
    eflags: cuint32;
    esp: cuint32;
    xss: cuint32;
  end;

type

  { TDbgLinuxThread }

  TDbgLinuxThread = class(TDbgThread)
  private
    FUserRegsStruct32: user_regs_struct32;
    FUserRegsStruct64: user_regs_struct64;
    FUserRegsChanged: boolean;
  protected
    function ReadThreadState: boolean;
  public
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    procedure BeforeContinue; override;
    procedure LoadRegisterValues; override;
  end;

  { TDbgLinuxProcess }

  TDbgLinuxProcess = class(TDbgProcess)
  private
    FStatus: cint;
    FProcessStarted: boolean;
    FProcProcess: TProcess;
    FIsTerminating: boolean;
    FExceptionSignal: PtrUInt;
    procedure OnForkEvent(Sender : TObject);
  protected
    function InitializeLoader: TDbgImageLoader; override;
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
  public
    class function StartInstance(AFileName: string; AParams, AnEnvironment: TStrings; AWorkingDirectory: string; AOnLog: TOnLog): TDbgProcess; override;
    constructor Create(const AName: string; const AProcessID, AThreadID: Integer; AOnLog: TOnLog); override;
    destructor Destroy; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;

    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    procedure TerminateProcess; override;

    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
  end;

procedure RegisterDbgClasses;

implementation

procedure RegisterDbgClasses;
begin
  OSDbgClasses.DbgProcessClass:=TDbgLinuxProcess;
  OSDbgClasses.DbgThreadClass:=TDbgLinuxThread;
end;

Function WIFSTOPPED(Status: Integer): Boolean;
begin
  WIFSTOPPED:=((Status and $FF)=$7F);
end;

{ TDbgLinuxThread }

procedure TDbgLinuxProcess.OnForkEvent(Sender: TObject);
var
  e: integer;
begin
  fpPTrace(PTRACE_TRACEME, 0, nil, nil);
  e := fpgeterrno;
  if e <> 0 then
    begin
    writeln('Failed to start trace of process. Errcode: '+inttostr(e));
    end
end;

function TDbgLinuxThread.ReadThreadState: boolean;
var
  e: integer;
begin
  result := true;
  errno:=0;
  fpPTrace(PTRACE_GETREGS, Process.ProcessID, nil, @FUserRegsStruct64);
  e := fpgeterrno;
  if e <> 0 then
    begin
    log('Failed to read thread registers from processid '+inttostr(Process.ProcessID)+'. Errcode: '+inttostr(e));
    result := false;
    end;
  FUserRegsChanged:=false;
  FRegisterValueListValid:=false;
end;

function TDbgLinuxThread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  result := true;

  if Process.Mode=dm32 then
    Dec(FUserRegsStruct32.eip)
  else
    Dec(FUserRegsStruct64.rip);
  FUserRegsChanged:=true;
end;

procedure TDbgLinuxThread.BeforeContinue;
var
  e: integer;
begin
  if FUserRegsChanged then
    begin
    fpPTrace(PTRACE_SETREGS, Process.ProcessID, nil, @FUserRegsStruct64);
    e := fpgeterrno;
    if e <> 0 then
      begin
      log('Failed to set thread registers. Errcode: '+inttostr(e));
      end;
    FUserRegsChanged:=false;
    end;
end;

procedure TDbgLinuxThread.LoadRegisterValues;
begin
  if Process.Mode=dm32 then with FUserRegsStruct32 do
  begin
    FRegisterValueList.DbgRegisterAutoCreate['eax'].SetValue(eax, IntToStr(eax),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ecx'].SetValue(ecx, IntToStr(ecx),4,1);
    FRegisterValueList.DbgRegisterAutoCreate['edx'].SetValue(edx, IntToStr(edx),4,2);
    FRegisterValueList.DbgRegisterAutoCreate['ebx'].SetValue(ebx, IntToStr(ebx),4,3);
    FRegisterValueList.DbgRegisterAutoCreate['esp'].SetValue(esp, IntToStr(esp),4,4);
    FRegisterValueList.DbgRegisterAutoCreate['ebp'].SetValue(ebp, IntToStr(ebp),4,5);
    FRegisterValueList.DbgRegisterAutoCreate['esi'].SetValue(esi, IntToStr(esi),4,6);
    FRegisterValueList.DbgRegisterAutoCreate['edi'].SetValue(edi, IntToStr(edi),4,7);
    FRegisterValueList.DbgRegisterAutoCreate['eip'].SetValue(eip, IntToStr(eip),4,8);

    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(eflags);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(xcs, IntToStr(xcs),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(xss, IntToStr(xss),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(xds, IntToStr(xds),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(xes, IntToStr(xes),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(xfs, IntToStr(xfs),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(xgs, IntToStr(xgs),4,0);
  end else with FUserRegsStruct64 do
    begin
    FRegisterValueList.DbgRegisterAutoCreate['rax'].SetValue(rax, IntToStr(rax),8,0);
    FRegisterValueList.DbgRegisterAutoCreate['rbx'].SetValue(rbx, IntToStr(rbx),8,3);
    FRegisterValueList.DbgRegisterAutoCreate['rcx'].SetValue(rcx, IntToStr(rcx),8,2);
    FRegisterValueList.DbgRegisterAutoCreate['rdx'].SetValue(rdx, IntToStr(rdx),8,1);
    FRegisterValueList.DbgRegisterAutoCreate['rsi'].SetValue(rsi, IntToStr(rsi),8,4);
    FRegisterValueList.DbgRegisterAutoCreate['rdi'].SetValue(rdi, IntToStr(rdi),8,5);
    FRegisterValueList.DbgRegisterAutoCreate['rbp'].SetValue(rbp, IntToStr(rbp),8,6);
    FRegisterValueList.DbgRegisterAutoCreate['rsp'].SetValue(rsp, IntToStr(rsp),8,7);

    FRegisterValueList.DbgRegisterAutoCreate['r8'].SetValue(r8, IntToStr(r8),8,8);
    FRegisterValueList.DbgRegisterAutoCreate['r9'].SetValue(r9, IntToStr(r9),8,9);
    FRegisterValueList.DbgRegisterAutoCreate['r10'].SetValue(r10, IntToStr(r10),8,10);
    FRegisterValueList.DbgRegisterAutoCreate['r11'].SetValue(r11, IntToStr(r11),8,11);
    FRegisterValueList.DbgRegisterAutoCreate['r12'].SetValue(r12, IntToStr(r12),8,12);
    FRegisterValueList.DbgRegisterAutoCreate['r13'].SetValue(r13, IntToStr(r13),8,13);
    FRegisterValueList.DbgRegisterAutoCreate['r14'].SetValue(r14, IntToStr(r14),8,14);
    FRegisterValueList.DbgRegisterAutoCreate['r15'].SetValue(r15, IntToStr(r15),8,15);

    FRegisterValueList.DbgRegisterAutoCreate['rip'].SetValue(rip, IntToStr(rip),8,16);
    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(eflags);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(cs, IntToStr(cs),8,43);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(fs, IntToStr(fs),8,46);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(gs, IntToStr(gs),8,47);
  end;
  FRegisterValueListValid:=true;
end;

{ TDbgLinuxProcess }

function TDbgLinuxProcess.InitializeLoader: TDbgImageLoader;
begin
  result := TDbgImageLoader.Create(Name);
end;

function TDbgLinuxProcess.CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread;
begin
  IsMainThread:=true;
  if AthreadIdentifier>-1 then
    result := TDbgLinuxThread.Create(Self, AthreadIdentifier, AthreadIdentifier)
  else
    result := nil;
end;

constructor TDbgLinuxProcess.Create(const AName: string; const AProcessID,
  AThreadID: Integer; AOnLog: TOnLog);
begin
  inherited Create(AName, AProcessID, AThreadID, AOnLog);

  LoadInfo;

  if DbgInfo.HasInfo
  then FSymInstances.Add(Self);
end;

destructor TDbgLinuxProcess.Destroy;
begin
  FProcProcess.Free;
  inherited Destroy;
end;

class function TDbgLinuxProcess.StartInstance(AFileName: string; AParams, AnEnvironment: TStrings; AWorkingDirectory: string; AOnLog: TOnLog): TDbgProcess;
var
  PID: TPid;
  AProcess: TProcess;
  AnExecutabeFilename: string;
begin
  result := nil;

  AnExecutabeFilename:=ExcludeTrailingPathDelimiter(AFileName);
  if DirectoryExists(AnExecutabeFilename) then
  begin
    DebugLn(format('Can not debug %s, because it''s a directory',[AnExecutabeFilename]));
    Exit;
  end;

  if not FileExists(AFileName) then
  begin
    DebugLn(format('Can not find  %s.',[AnExecutabeFilename]));
    Exit;
  end;

  AProcess := TProcess.Create(nil);
  try
    AProcess.OnForkEvent:=@OnForkEvent;
    AProcess.Executable:=AnExecutabeFilename;
    AProcess.Parameters:=AParams;
    AProcess.Environment:=AnEnvironment;
    AProcess.CurrentDirectory:=AWorkingDirectory;
    AProcess.Execute;
    PID:=AProcess.ProcessID;

    sleep(100);
    result := TDbgLinuxProcess.Create(AFileName, Pid, -1, AOnLog);
  except
    AProcess.Free;
    raise;
  end;

  TDbgLinuxProcess(result).FProcProcess := AProcess;
end;

function TDbgLinuxProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData): Boolean;

var
  WordSize: byte;

  function ReadWordSize(Adr: TDbgPtr; out AVal: TDBGPtr): boolean;
  var
    e: integer;
  begin
    errno := 0;
    AVal := TDbgPtr(fpPTrace(PTRACE_PEEKDATA, Process.ProcessID, pointer(Adr), nil));
    e := fpgeterrno;
    if e <> 0 then
      begin
      log('Failed to read data at address '+FormatAddress(Adr)+' from processid '+inttostr(Process.ProcessID)+'. Errcode: '+inttostr(e));
      result := false;
      end
    else
      result := true;
  end;

var
  AVal: TDbgPtr;
  AAdressAlign: TDBGPtr;
  BytesRead: integer;
  ReadBytes: integer;
  PB: PByte;
  buf: pbyte;
begin
  BytesRead := 0;
  result := false;
  getmem(buf, ASize);
  try
    WordSize:=DBGPTRSIZE[Mode];
    if AAdress mod WordSize <> 0 then
      begin
      AAdressAlign := ((PtrUInt(AAdress)) and not PtrUInt(WordSize - 1));
      if not ReadWordSize(AAdressAlign, AVal) then
        Exit;
      pb := @AVal;
      BytesRead:=WordSize-(AAdress-AAdressAlign);
      if BytesRead>=ASize then
        BytesRead:=ASize;
      move(pb[AAdress-AAdressAlign], buf[0], BytesRead);
      inc(AAdressAlign, WordSize);
      end
    else
      AAdressAlign:=AAdress;

    while BytesRead<ASize do
      begin
      if not ReadWordSize(AAdressAlign, AVal) then
        exit;
      if WordSize<(ASize-BytesRead) then
        ReadBytes:=WordSize
      else
        ReadBytes:=(ASize-BytesRead);
      move(AVal, buf[BytesRead], ReadBytes);
      inc(BytesRead, ReadBytes);
      inc(AAdressAlign, WordSize);

      end;
    System.Move(buf^, AData, BytesRead);
  finally
    freemem(buf);
  end;
  MaskBreakpointsInReadData(AAdress, ASize, AData);
  result := true;
end;

function TDbgLinuxProcess.WriteData(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData): Boolean;
var
  e: integer;
  pi: TDBGPtr;
  WordSize: integer;
begin
  result := false;
  WordSize:=DBGPTRSIZE[Mode];

  if ASize>WordSize then
    log('Can not write more then '+IntToStr(WordSize)+' bytes.')
  else
    begin
    if ASize<WordSize then
      begin
      fpseterrno(0);
      pi := TDbgPtr(fpPTrace(PTRACE_PEEKDATA, Process.ProcessID, pointer(AAdress), nil));
      e := fpgeterrno;
      if e <> 0 then
        begin
        log('Failed to read data. Errcode: '+inttostr(e));
        result := false;
        exit;
        end;
      end;
    move(AData, pi, ASize);

    fpPTrace(PTRACE_POKEDATA, Process.ProcessID, pointer(AAdress), pointer(pi));
    e := fpgeterrno;
    if e <> 0 then
      begin
      log('Failed to write data. Errcode: '+inttostr(e));
      result := false;
      end;
    end;

  result := true;
end;

function TDbgLinuxProcess.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  if Mode=dm32 then
    result := TDbgLinuxThread(FMainThread).FUserRegsStruct32.eip
  else
    result := TDbgLinuxThread(FMainThread).FUserRegsStruct64.rip;
end;

function TDbgLinuxProcess.GetStackPointerRegisterValue: TDbgPtr;
begin
  if Mode=dm32 then
    result := TDbgLinuxThread(FMainThread).FUserRegsStruct32.esp
  else
    result := TDbgLinuxThread(FMainThread).FUserRegsStruct64.rsp;
end;

function TDbgLinuxProcess.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  if Mode=dm32 then
    result := TDbgLinuxThread(FMainThread).FUserRegsStruct32.ebp
  else
    result := TDbgLinuxThread(FMainThread).FUserRegsStruct64.rbp;
end;

procedure TDbgLinuxProcess.TerminateProcess;
begin
  FIsTerminating:=true;
  if fpkill(ProcessID,SIGKILL)<>0 then
    begin
    log('Failed to send SIGKILL to process %d. Errno: %d',[ProcessID, errno]);
    FIsTerminating:=false;
    end;
end;

function TDbgLinuxProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean;
var
  e: integer;
begin
  fpseterrno(0);
  AThread.NextIsSingleStep:=SingleStep;
  AThread.BeforeContinue;
  if SingleStep or assigned(FCurrentBreakpoint) then
    fpPTrace(PTRACE_SINGLESTEP, ProcessID, pointer(1), pointer(FExceptionSignal))
  else if FIsTerminating then
    fpPTrace(PTRACE_KILL, ProcessID, pointer(1), nil)
  else
    fpPTrace(PTRACE_CONT, ProcessID, pointer(1), pointer(FExceptionSignal));
  e := fpgeterrno;
  if e <> 0 then
    begin
    log('Failed to continue process. Errcode: '+inttostr(e));
    result := false;
    end
  else
    result := true;
end;

function TDbgLinuxProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
begin
  ThreadIdentifier:=1;

  ProcessIdentifier:=FpWaitPid(-1, FStatus, 0);

  result := ProcessIdentifier<>-1;
  if not result then
    Log('Failed to wait for debug event. Errcode: %d', [fpgeterrno]);
end;

function TDbgLinuxProcess.AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent;

begin
  FExceptionSignal:=0;
  if wifexited(FStatus) or wifsignaled(FStatus) then
    begin
    SetExitCode(wexitStatus(FStatus));

    result := deExitProcess
    end
  else if WIFSTOPPED(FStatus) then
    begin
    //log('Stopped ',FStatus, ' signal: ',wstopsig(FStatus));
    TDbgLinuxThread(AThread).ReadThreadState;
    case wstopsig(FStatus) of
      SIGTRAP:
        begin        if not FProcessStarted then
          begin
          result := deCreateProcess;
          FProcessStarted:=true;
          end
        else
          result := deBreakpoint;
        end;
      SIGBUS:
        begin
        ExceptionClass:='SIGBUS';
        FExceptionSignal:=SIGBUS;
        result := deException;
        end;
      SIGINT:
        begin
        ExceptionClass:='SIGINT';
        FExceptionSignal:=SIGINT;
        result := deException;
        end;
      SIGSEGV:
        begin
        ExceptionClass:='SIGSEGV';
        FExceptionSignal:=SIGSEGV;
        result := deException;
        end;
      SIGKILL:
        begin
        if FIsTerminating then
          result := deInternalContinue
        else
          begin
          ExceptionClass:='SIGKILL';
          FExceptionSignal:=SIGKILL;
          result := deException;
          end;
        end;
      else
        begin
        ExceptionClass:='Unknown exception code '+inttostr(wstopsig(FStatus));
        FExceptionSignal:=wstopsig(FStatus);
        result := deException;
        end;
    end; {case}
    if result=deException then
      ExceptionClass:='External: '+ExceptionClass;
    end
  else
    raise exception.CreateFmt('Received unknown status %d from process with pid=%d',[FStatus, ProcessID]);
end;

end.

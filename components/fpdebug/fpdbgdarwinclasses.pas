unit FpDbgDarwinClasses;

{$mode objfpc}{$H+}
{$linkframework security}

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
  MacOSAll,
  FpDbgUtil,
  LazLoggerBase;

type
  x86_thread_state32_t = record
    __eax: cuint;
    __ebx: cuint;
    __ecx: cuint;
    __edx: cuint;
    __edi: cuint;
    __esi: cuint;
    __ebp: cuint;
    __esp: cuint;
    __ss: cuint;
    __eflags: cuint;
    __eip: cuint;
    __cs: cuint;
    __ds: cuint;
    __es: cuint;
    __fs: cuint;
    __gs: cuint;
  end;

type

  { TDbgDarwinThread }

  TDbgDarwinThread = class(TDbgThread)
  private
    FThreadState: x86_thread_state32_t;
  protected
    function ReadThreadState: boolean;
  public
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    procedure LoadRegisterValues; override;
  end;

  { TDbgDarwinProcess }

  TDbgDarwinProcess = class(TDbgProcess)
  private
    FStatus: cint;
    FProcessStarted: boolean;
    FTaskPort: mach_port_name_t;
    FProcProcess: TProcess;
    FIsTerminating: boolean;
    function GetDebugAccessRights: boolean;
  protected
    function InitializeLoader: TDbgImageLoader; override;
  public
    class function StartInstance(AFileName: string; AParams: string): TDbgProcess; override;
    constructor Create(const AName: string; const AProcessID, AThreadID: Integer); override;
    destructor Destroy; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;

    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    procedure TerminateProcess; override;

    function Continue(AProcess: TDbgProcess; AThread: TDbgThread): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
    function ResolveDebugEvent(AThread: TDbgThread): TFPDEvent; override;
  end;

procedure RegisterDbgClasses;

implementation

type
  vm_map_t = mach_port_t;
  vm_offset_t = UIntPtr;
  vm_address_t = vm_offset_t;
  vm_size_t = UIntPtr;
  vm_prot_t = cint;
  mach_vm_address_t = uint64;
  mach_msg_Type_number_t = natural_t;
  mach_vm_size_t = uint64;
  task_t = mach_port_t;
  thread_act_t = mach_port_t;
  thread_act_array = array[0..255] of thread_act_t;
  thread_act_array_t = ^thread_act_array;
  thread_state_flavor_t = cint;
  thread_state_t = ^natural_t;

const
  x86_THREAD_STATE32    = 1;

  x86_THREAD_STATE32_COUNT: mach_msg_Type_number_t = sizeof(x86_thread_state32_t) div sizeof(cint);

function task_for_pid(target_tport: mach_port_name_t; pid: integer; var t: mach_port_name_t): kern_return_t; cdecl external name 'task_for_pid';
function mach_task_self: mach_port_name_t; cdecl external name 'mach_task_self';
function mach_error_string(error_value: mach_error_t): pchar; cdecl; external name 'mach_error_string';
function vm_protect(target_task: vm_map_t; adress: vm_address_t; size: vm_size_t; set_maximum: boolean_t; new_protection: vm_prot_t): kern_return_t; cdecl external name 'vm_protect';
function mach_vm_write(target_task: vm_map_t; address: mach_vm_address_t; data: vm_offset_t; dataCnt: mach_msg_Type_number_t): kern_return_t; cdecl external name 'mach_vm_write';
function mach_vm_read(target_task: vm_map_t; address: mach_vm_address_t; size: mach_vm_size_t; var data: vm_offset_t; var dataCnt: mach_msg_Type_number_t): kern_return_t; cdecl external name 'mach_vm_read';

function task_threads(target_task: task_t; var act_list: thread_act_array_t; var act_listCnt: mach_msg_type_number_t): kern_return_t; cdecl external name 'task_threads';
function thread_get_state(target_act: thread_act_t; flavor: thread_state_flavor_t; old_state: thread_state_t; var old_stateCnt: mach_msg_Type_number_t): kern_return_t; cdecl external name 'thread_get_state';
function thread_set_state(target_act: thread_act_t; flavor: thread_state_flavor_t; new_state: thread_state_t; old_stateCnt: mach_msg_Type_number_t): kern_return_t; cdecl external name 'thread_set_state';

procedure RegisterDbgClasses;
begin
  OSDbgClasses.DbgProcessClass:=TDbgDarwinProcess;
end;

Function WIFSTOPPED(Status: Integer): Boolean;
begin
  WIFSTOPPED:=((Status and $FF)=$7F);
end;

{ TDbgDarwinThread }

procedure OnForkEvent;
begin
  fpPTrace(PTRACE_TRACEME, 0, nil, nil);
end;

function TDbgDarwinThread.ReadThreadState: boolean;
var
  aKernResult: kern_return_t;
  old_StateCnt: mach_msg_Type_number_t;
begin
  old_StateCnt:=x86_THREAD_STATE32_COUNT;
  aKernResult:=thread_get_state(Id,x86_THREAD_STATE32, @FThreadState,old_StateCnt);
  if aKernResult <> KERN_SUCCESS then
    begin
    Log('Failed to call thread_get_state for thread %d. Mach error: '+mach_error_string(aKernResult),[Id]);
    end;
  FRegisterValueListValid:=false;
end;

function TDbgDarwinThread.ResetInstructionPointerAfterBreakpoint: boolean;
var
  aKernResult: kern_return_t;
  new_StateCnt: mach_msg_Type_number_t;
begin
  {$ifdef cpui386}
  Dec(FThreadState.__eip);
  {$else}
  Dec(FThreadState.__rip);
  {$endif}

  new_StateCnt := x86_THREAD_STATE32_COUNT;
  aKernResult:=thread_set_state(ID,x86_THREAD_STATE32, @FThreadState, new_StateCnt);
  if aKernResult <> KERN_SUCCESS then
    begin
    Log('Failed to call thread_set_state for thread %d. Mach error: '+mach_error_string(aKernResult),[Id]);
    end;
  result := true;
end;

procedure TDbgDarwinThread.LoadRegisterValues;
begin
  with FThreadState do
    begin
    FRegisterValueList.DbgRegisterAutoCreate['eax'].SetValue(__eax, IntToStr(__eax));
    FRegisterValueList.DbgRegisterAutoCreate['ecx'].SetValue(__ecx, IntToStr(__ecx));
    FRegisterValueList.DbgRegisterAutoCreate['edx'].SetValue(__edx, IntToStr(__edx));
    FRegisterValueList.DbgRegisterAutoCreate['ebx'].SetValue(__ebx, IntToStr(__ebx));
    FRegisterValueList.DbgRegisterAutoCreate['esp'].SetValue(__esp, IntToStr(__esp));
    FRegisterValueList.DbgRegisterAutoCreate['ebp'].SetValue(__ebp, IntToStr(__ebp));
    FRegisterValueList.DbgRegisterAutoCreate['esi'].SetValue(__esi, IntToStr(__esi));
    FRegisterValueList.DbgRegisterAutoCreate['edi'].SetValue(__edi, IntToStr(__edi));
    FRegisterValueList.DbgRegisterAutoCreate['eip'].SetValue(__eip, IntToStr(__eip));

    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(__eflags);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(__cs, IntToStr(__cs));
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(__ss, IntToStr(__ss));
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(__ds, IntToStr(__ds));
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(__es, IntToStr(__es));
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(__fs, IntToStr(__fs));
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(__gs, IntToStr(__gs));
  end;
  FRegisterValueListValid:=true;
end;

{ TDbgDarwinProcess }

function TDbgDarwinProcess.GetDebugAccessRights: boolean;
var
  authFlags: AuthorizationFlags;
  stat: OSStatus;
  author: AuthorizationRef;
  authItem: AuthorizationItem;
  authRights: AuthorizationRights;
begin
  result := false;
  authFlags := kAuthorizationFlagExtendRights or kAuthorizationFlagPreAuthorize or kAuthorizationFlagInteractionAllowed or ( 1 << 5);

  stat := AuthorizationCreate(nil, kAuthorizationEmptyEnvironment, authFlags, author);
  if stat <> errAuthorizationSuccess then
    begin
    debugln('Failed to create authorization. Authorization error: ' + inttostr(stat));
    exit;
    end;

  authItem.name:='system.privilege.taskport';
  authItem.flags:=0;
  authItem.value:=nil;
  authItem.valueLength:=0;

  authRights.count:=1;
  authRights.items:=@authItem;

  stat := AuthorizationCopyRights(author, authRights, kAuthorizationEmptyEnvironment, authFlags, nil);
  if stat <> errAuthorizationSuccess then
    begin
    debugln('Failed to get debug-(taskport)-privilege. Authorization error: ' + inttostr(stat));
    exit;
    end;
  result := true;
end;

function TDbgDarwinProcess.InitializeLoader: TDbgImageLoader;
var
  FObjFileName: string;
  dSYMFilename: string;
begin
  // JvdS: Mach-O binaries do not contain DWARF-debug info. Instead this info
  // is stored inside the .o files, and the executable contains a map (in stabs-
  // format) of all these .o files. An alternative to parsing this map and reading
  // those .o files a dSYM-bundle could be used, which could be generated
  // with dsymutil.
  dSYMFilename:=ChangeFileExt(Name, '.dSYM');
  dSYMFilename:=dSYMFilename+'/Contents/Resources/DWARF/'+ExtractFileName(Name);

  if ExtractFileExt(dSYMFilename)='.app' then
    dSYMFilename := ChangeFileExt(dSYMFilename,'');

  if FileExists(dSYMFilename) then
    result := TDbgImageLoader.Create(dSYMFilename)
  else
    begin
    log('No dSYM bundle ('+dSYMFilename+') found.');
    result := TDbgImageLoader.Create(Name);
    end;
end;

constructor TDbgDarwinProcess.Create(const AName: string; const AProcessID, AThreadID: Integer);
var
  aKernResult: kern_return_t;
begin
  inherited Create(AName, AProcessID, AThreadID);

  LoadInfo;

  if DbgInfo.HasInfo
  then FSymInstances.Add(Self);

  GetDebugAccessRights;
  aKernResult:=task_for_pid(mach_task_self, AProcessID, FTaskPort);
  if aKernResult <> KERN_SUCCESS then
    begin
    DebugLn('Failed to get task for process '+IntToStr(AProcessID)+'. Probably insufficient rights to debug applications. Mach error: '+mach_error_string(aKernResult));
    end;
end;

destructor TDbgDarwinProcess.Destroy;
begin
  FProcProcess.Free;
  inherited Destroy;
end;

class function TDbgDarwinProcess.StartInstance(AFileName: string; AParams: string): TDbgProcess;
var
  PID: TPid;
  stat: longint;
  AProcess: TProcess;
  AnExecutabeFilename: string;
begin
  result := nil;

  AnExecutabeFilename:=ExcludeTrailingPathDelimiter(AFileName);
  if DirectoryExists(AnExecutabeFilename) then
    begin
    if not (ExtractFileExt(AnExecutabeFilename)='.app') then
      begin
      DebugLn(format('Can not debug %s, because it''s a directory',[AnExecutabeFilename]));
      Exit;
      end;

    AnExecutabeFilename := AnExecutabeFilename + '/Contents/MacOS/' + ChangeFileExt(ExtractFileName(AnExecutabeFilename),'');
    if not FileExists(AFileName) then
      begin
      DebugLn(format('Can not find  %s.',[AnExecutabeFilename]));
      Exit;
      end;
    end;

  AProcess := TProcess.Create(nil);
  try
    AProcess.OnForkEvent:=@OnForkEvent;
    AProcess.Executable:=AnExecutabeFilename;
    AProcess.Execute;
    PID:=AProcess.ProcessID;

    sleep(100);
    result := TDbgDarwinProcess.Create(AFileName, Pid,-1);
  except
    AProcess.Free;
    raise;
  end;

  TDbgDarwinProcess(result).FProcProcess := AProcess;
end;

function TDbgDarwinProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData): Boolean;
var
  aKernResult: kern_return_t;
  cnt: mach_msg_Type_number_t;
  b: pointer;
begin
  result := false;

  aKernResult := mach_vm_read(FTaskPort, AAdress, ASize, PtrUInt(b), cnt);
  if aKernResult <> KERN_SUCCESS then
    begin
    DebugLn('Failed to read data at address '+FormatAddress(ProcessID)+'. Mach error: '+mach_error_string(aKernResult));
    Exit;
    end;
  System.Move(b^, AData, Cnt);
  result := true;
end;

function TDbgDarwinProcess.WriteData(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData): Boolean;
var
  aKernResult: kern_return_t;
begin
  result := false;
  aKernResult:=vm_protect(FTaskPort, PtrUInt(AAdress), ASize, boolean_t(false), 7 {VM_PROT_READ + VM_PROT_WRITE + VM_PROT_COPY});
  if aKernResult <> KERN_SUCCESS then
    begin
    DebugLn('Failed to call vm_protect for address '+FormatAddress(AAdress)+'. Mach error: '+mach_error_string(aKernResult));
    Exit;
    end;

  aKernResult := mach_vm_write(FTaskPort, AAdress, vm_offset_t(@AData), ASize);
  if aKernResult <> KERN_SUCCESS then
    begin
    DebugLn('Failed to write data at address '+FormatAddress(AAdress)+'. Mach error: '+mach_error_string(aKernResult));
    Exit;
    end;

  result := true;
end;

function TDbgDarwinProcess.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  result := TDbgDarwinThread(FMainThread).FThreadState.__eip;
end;

function TDbgDarwinProcess.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  result := TDbgDarwinThread(FMainThread).FThreadState.__ebp;

end;

procedure TDbgDarwinProcess.TerminateProcess;
begin
  FIsTerminating:=true;
  if fpkill(ProcessID,SIGKILL)<>0 then
    begin
    log('Failed to send SIGKILL to process %d. Errno: %d',[ProcessID, errno]);
    FIsTerminating:=false;
    end;
end;

function TDbgDarwinProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread): boolean;
var
  e: integer;
begin
  fpseterrno(0);
{$ifdef linux}
  fpPTrace(PTRACE_CONT, ProcessID, nil, nil);
{$endif linux}
{$ifdef darwin}
  if (AThread.SingleStepping) or assigned(FCurrentBreakpoint) then
    fpPTrace(PTRACE_SINGLESTEP, ProcessID, pointer(1), nil)
  else if FIsTerminating then
    fpPTrace(PTRACE_KILL, ProcessID, pointer(1), nil)
  else
    fpPTrace(PTRACE_CONT, ProcessID, pointer(1), nil);
{$endif darwin}
  e := fpgeterrno;
  if e <> 0 then
    begin
    writeln('Failed to continue process. Errcode: ',e);
    result := false;
    end
  else
    result := true;
end;

function TDbgDarwinProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
var
  aKernResult: kern_return_t;
  act_list: thread_act_array_t;
  act_listCtn: mach_msg_type_number_t;
  i: Integer;
  AThread: TDbgThread;
begin
  ProcessIdentifier:=FpWaitPid(-1, FStatus, 0);

  result := ProcessIdentifier<>-1;
  if not result then
    writeln('Failed to wait for debug event. Errcode: ', fpgeterrno)
  else if WIFSTOPPED(FStatus) then
    begin
    // Read thread-information
    aKernResult := task_threads(FTaskPort, act_list, act_listCtn);
    if aKernResult <> KERN_SUCCESS then
      begin
      Log('Failed to call task_threads. Mach error: '+mach_error_string(aKernResult));
      end;

    for i := 0 to act_listCtn-1 do
      begin
      if not GetThread(act_list^[i], AThread) then
        begin
        AThread := TDbgDarwinThread.Create(Self, act_list^[i], act_list^[i]);
        FThreadMap.Add(act_list^[i], AThread);
        if FMainThread=nil then
          FMainThread := AThread;
        end;
      end;
    ThreadIdentifier:=act_list^[0];
    TDbgDarwinThread(FMainThread).ReadThreadState;
    end
end;

function TDbgDarwinProcess.ResolveDebugEvent(AThread: TDbgThread): TFPDEvent;

begin
  if wifexited(FStatus) or wifsignaled(FStatus) then
    begin
    SetExitCode(wexitStatus(FStatus));
    writeln('Exit');
    // Clear all pending signals
    repeat
    until FpWaitPid(-1, FStatus, WNOHANG)<1;

    result := deExitProcess
    end
  else if WIFSTOPPED(FStatus) then
    begin
    writeln('Stopped ',FStatus, ' signal: ',wstopsig(FStatus));
    case wstopsig(FStatus) of
      SIGTRAP:
        begin
        if not FProcessStarted then
          begin
          result := deCreateProcess;
          FProcessStarted:=true;
          end
        else if assigned(FCurrentBreakpoint) then
          begin
          FCurrentBreakpoint.SetBreak;
          FCurrentBreakpoint:=nil;
          result := deInternalContinue;
          end
        else
          begin
          result := deBreakpoint;
          DoBreak(TDbgDarwinThread(FMainThread).FThreadState.__eip-1, FMainThread.ID);
          end;
        end;
      SIGBUS:
        begin
        writeln('Received SIGBUS');
        result := deException;
        end;
      SIGINT:
        begin
        writeln('Received SIGINT');
        result := deException;
        end;
      SIGSEGV:
        begin
        writeln('Received SIGSEGV');
        result := deException;
        end;
      SIGKILL:
        begin
        writeln('Received SIGKILL');
        if FIsTerminating then
          result := deInternalContinue
        else
          result := deException;
        end;
    end; {case}
    end
  else
    raise exception.CreateFmt('Received unknown status %d from process with pid=%d',[FStatus, ProcessID]);
end;

end.

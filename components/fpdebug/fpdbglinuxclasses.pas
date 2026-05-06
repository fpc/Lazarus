unit FpDbgLinuxClasses;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{$packrecords c}
{$modeswitch advancedrecords}
{off $define DebuglnLinuxDebugEvents}

interface

uses
  Classes,
  SysUtils,
  BaseUnix,
  termio, fgl,
  Contnrs,
  StrUtils,
  Types,
  FpDbgClasses,
  FpDbgLoader, FpDbgDisasX86,
  DbgIntfBaseTypes, DbgIntfDebuggerBase, DbgIntfProcess,
  FpDbgLinuxExtra,
  FpDbgInfo,
  FpDbgUtil,
  UTF8Process,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, Maps,
  FpDbgCommon, FpdMemoryTools,
  FpErrorMessages,
  FpImgReaderBase,
  FpDbgCpuX86, LazDebuggerIntfFloatTypes;

//function login_tty(__fd:longint):longint;cdecl;external 'c' name 'login_tty';
function openpty(__amaster:Plongint; __aslave:Plongint; __name:Pchar; __termp:pointer{Ptermios}; __winp:pointer{Pwinsize}):longint;cdecl;external 'util' name 'openpty';

const
  __WALL   = $40000000;

type

  { TFpDbgLinuxSignal }

  TFpDbgLinuxSignal = record
    PID: THandle;
    WaitStatus: cint;
    class operator = (a, b: TFpDbgLinuxSignal): boolean;
  end;

  { TFpDbgLinuxSignalQueue }

  TFpDbgLinuxSignalQueue = class(specialize TFPGList<TFpDbgLinuxSignal>)
  public
    procedure AddSignal(APID: THandle; AWaitStatus: cint); overload;
    function GetNextSignal(out APID: THandle; out AWaitStatus: cint): Boolean;
  end;

  { TDbgLinuxMemoryMapping }

  // Corresponds to the structure in /proc/[pid]/maps, see the Linux documentation
  // 'man 5 proc' for more info.
  TDbgLinuxMemoryMapping = class(TObject)
    AddressFrom: PtrInt;
    AddressTill: PtrInt;
    Rights: string;
    Offset: PtrInt;
    DeviceId: string;
    Inode: QWord;
    FileName: string;
  end;
  TDbgLinuxMemoryMappingList = class(TObjectList);

  { TDbgLinuxThread }

  TDbgLinuxThread = class(TDbgThread)
  private
    FExceptionSignal: cint;
    FIsPaused, FInternalPauseRequested, FIsInInternalPause: boolean;
    FHasExited: Boolean;
    FIsSteppingBreakPoint: boolean;
    function GetName: String; override;
  protected
    function RequestInternalPause: Boolean;
    function CheckSignalForPostponing(AWaitedStatus: cint): Boolean;
    procedure ResetPauseStates; virtual;
  public
    procedure ClearExceptionSignal; override;

    property IsPaused: boolean read FIsPaused;
  end;

  tDbgLinuxLibrary = class;

  { TDbgLinuxProcess }

  TDbgLinuxProcess = class(TDbgx86Process)
  private
    FPostponedSignals: TFpDbgLinuxSignalQueue;
    FStatus: cint;
    FProcessStarted: boolean;
    FProcProcess: TProcessWithRedirect;
    FIsTerminating: boolean;
    FMasterPtyFd: cint;
    FCurrentThreadId: THandle;
    FSingleSteppingThreadID: THandle;
    // This breakpoint is triggered after dynamic libraries have been (un)loaded
    FSOLibEventBreakpoint: TFpDbgBreakpointBase;
    {$ifndef VER2_6}
    procedure OnForkEvent(Sender : TObject);
    {$endif}
    function ReadWordSize(Adr: TDbgPtr; out AVal: TDBGPtr): boolean; inline;
    function WriteWordSize(Adr: TDbgPtr; AVal: TDBGPtr): boolean; inline;
  protected
    function GetRequiresExecutionInDebuggerThread: boolean; override;
    procedure InitializeLoaders; override;
    function DbgThreadClass: TDbgThreadClass; virtual; abstract;
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; override;
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; override;
    function CreateWatchPointData: TFpWatchPointData; override;
    // On Linux, dynamic loading of Libraries isn't done by the kernel, but by
    // a library. At startup a breakpoint is set within this library to provide
    // an hook for the event a library has been (un)loaded. This function
    // analyzes deBreakpoint events and checks if they are triggered by this
    // special breakpoint. And when this is the case, it evaluates the state
    // of the process to determine if a library has been (un)loaded or that
    // nothing has happened. And returns the corresponding TFPDEvent.
    // (deLoadLibrary, deUnloadLibrary, deInternalContinue or deBreakpoint when
    // the breakpoint is not this 'special' breakpoint).
    function CheckForSOLibDebugEvent(AThread: TDbgThread): TFPDEvent;
    // Searches in /proc/[pid]/maps for dynamically loaded libraries and
    // synchronizes these results with the list of loaded libraries.
    function SynchronizeProcMapsWithLibraryList: TFPDEvent;
    // Scan /proc/[pid]/maps and return the results
    function ObtainProcMaps: TDbgLinuxMemoryMappingList;
    procedure AddLib(const ALibrary: tDbgLinuxLibrary);
  public
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
      AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig = nil); override;
    destructor Destroy; override;

    function StartInstance(AParams, AnEnvironment: TStrings;
      AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags;
      out AnError: TFpError): boolean; override;

    function AttachToInstance(APid: Integer; out AnError: TFpError): boolean; override;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; override;
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData; out APartSize: Cardinal): Boolean; override;
    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; override;
    function CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation; override;

    function CheckForConsoleOutput(ATimeOutMs: integer): integer; override;
    function GetConsoleOutput: string; override;
    procedure SendConsoleInput(AString: string); override;

    procedure TerminateProcess; override;
    function Pause: boolean; override;
    function Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean; override;

    procedure LoadInfo; override;

    function CanContinueForWatchEval(ACurrentThread: TDbgThread): boolean; override;
    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; override;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; override;
  end;
  TDbgLinuxProcessClass = class of TDbgLinuxProcess;

  { tDbgLinuxLibrary }

  tDbgLinuxLibrary = class(TDbgLibrary)
  protected
    FLoadedTargetImageAddr: TDbgPtr;
    procedure InitializeLoaders; override;
  public
    constructor Create(const AProcess: TDbgProcess; const AFileName: string; const AModuleHandle: THandle; const ALoadedTargetImageAddr: TDbgPtr);
  end;


implementation

var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_LINUX: PLazLoggerLogGroup;
  GConsoleTty: string;
  GSlavePTyFd: cint;

type
  Tprocess_vm_readv = function(pid: pid_t; const local_iov: piovec;
    liovcnt: NativeUInt; const remote_iov: piovec; riovcnt: NativeUInt;
    flags: NativeUInt): ssize_t; cdecl;

var
  process_vm_lib: TLibHandle = 0;
  process_vm_readv: Tprocess_vm_readv = nil;

function dbgsThreadId(AThread: TDbgThread): string;
begin
  if AThread = nil then
    result := 'nil'
  else
    result := inttostr(AThread.ID);
end;

Function WIFSTOPPED(Status: Integer): Boolean;
begin
  WIFSTOPPED:=((Status and $FF)=$7F);
end;

{ tDbgLinuxLibrary }

procedure tDbgLinuxLibrary.InitializeLoaders;
var
  Loader: TDbgImageLoader;
begin
  Loader := TDbgImageLoader.Create(Name, nil, FLoadedTargetImageAddr);
  // The dynamic-loader (dl) on Linux also loads other stuff then ELF-
  // formatted libraries.
  // So it is reasonable likely that the loaded 'library' can not be handled
  // by the default readers from the loader.
  if Loader.IsValid then
    Loader.AddToLoaderList(LoaderList)
  else
    Loader.Free;
end;

constructor tDbgLinuxLibrary.Create(const AProcess: TDbgProcess; const AFileName: string; const AModuleHandle: THandle; const ALoadedTargetImageAddr: TDbgPtr);
begin
  FLoadedTargetImageAddr := ALoadedTargetImageAddr;
  Inherited Create(AProcess, AFileName, AModuleHandle);
  SetFileName(AFileName);

  LoadInfo;
end;

{ TFpDbgLinuxSignal }

class operator TFpDbgLinuxSignal.=(a, b: TFpDbgLinuxSignal): boolean;
begin
  result := a.Pid = b.Pid;
  assert(false);
end;

{ TFpDbgLinuxSignalQueue }

procedure TFpDbgLinuxSignalQueue.AddSignal(APID: THandle; AWaitStatus: cint);
var
  tmp: TFpDbgLinuxSignal;
begin
  tmp.PID := APid;
  tmp.WaitStatus := AWaitStatus;
  Add(tmp);
end;

function TFpDbgLinuxSignalQueue.GetNextSignal(out APID: THandle; out
  AWaitStatus: cint): Boolean;
var
  tmp: TFpDbgLinuxSignal;
begin
  Result := Count > 0;
  if not Result then
    exit;
  tmp := Items[0];
  APID := tmp.PID;
  AWaitStatus := tmp.WaitStatus;
  delete(0);
  DebugLn(DBG_VERBOSE, ['DEFERRED event for ',Apid]);
end;

{ TDbgLinuxThread }

{$ifndef VER2_6}
procedure TDbgLinuxProcess.OnForkEvent(Sender: TObject);
{$else}
procedure OnForkEvent;
{$endif VER2_6}
var
  ConsoleTtyFd: cint;
begin
  if fpPTrace(PTRACE_TRACEME, 0, nil, nil) <> 0 then
    writeln('Failed to start trace of process. Errcode: '+inttostr(fpgeterrno));

  ConsoleTtyFd := -1;
  if GConsoleTty<>'' then
    ConsoleTtyFd:=FpOpen(GConsoleTty,O_RDWR+O_NOCTTY)
  else if GSlavePTyFd>-1 then
    ConsoleTtyFd:=GSlavePTyFd;

  if ConsoleTtyFd>-1 then begin
    if FpSetsid <> -1 then
      FpIOCtl(ConsoleTtyFd, TIOCSCTTY, nil);

    if Config.StdInRedirFile = '' then begin
      //if DBG_PROCESS_HAS_REDIRECT then ????????????
      FpClose(0);
      FpDup2(ConsoleTtyFd,0);
    end;
    if Config.StdOutRedirFile = '' then begin
      FpClose(1);
      FpDup2(ConsoleTtyFd,1);
    end;
    if Config.StdErrRedirFile = '' then begin
      FpClose(2);
      FpDup2(ConsoleTtyFd,2);
    end;
    FpClose(ConsoleTtyFd);
  end
  else
    writeln('Failed to open tty '+GConsoleTty+'. Errno: '+inttostr(fpgeterrno));

end;

function TDbgLinuxThread.GetName: String;
var
  fh: THandle;
  n: array[0..30] of AnsiChar;
  c: LongInt;
begin
  Result := '';
  fh := FileOpen('/proc/' + IntToStr(Handle) + '/comm', fmOpenRead or fmShareDenyNone);
  if fh <> THandle(-1) then begin
    try
      c := FileRead(fh, n, 30);
      if c > 0 then begin
        n[c] := #0;
        Result := TrimRightSet(n, [' ', #10]);
      end;
    finally
      FileClose(fh);
    end;
  end;
  if Result = '' then
    Result := inherited GetName;
end;

function TDbgLinuxThread.RequestInternalPause: Boolean;
begin
  Result := False;
  if FHasExited then begin
    DebugLn(DBG_VERBOSE, ['PauseRequest for exited Thread ', ID]);
    exit;
  end;
  if FInternalPauseRequested or FIsPaused then
    exit;

  result := fpkill(ID, SIGSTOP)=0;
  {$IFDEF DebuglnLinuxDebugEvents}
  debugln(FPDBG_LINUX, 'TDbgLinuxThread.RequestInternalPause fpkill(%d, SIGSTOP) => %s', [ID, dbgs(Result)]);
  {$ENDIF}
  if not result then
    begin
    // TODO: errChld -> remove thread
    DebugLn(DBG_WARNINGS, 'Failed to send SIGTSTOP to process %d. Errno: %d',[ID, errno]);
    exit;
    end;

  FInternalPauseRequested := True;
end;

function TDbgLinuxThread.CheckSignalForPostponing(AWaitedStatus: cint): Boolean;
begin
  //Assert(not FIsPaused, 'Got WaitStatus while already paused');
  //assert(FExceptionSignal = 0, 'TDbgLinuxThread.CheckSignalForPostponing: FExceptionSignal = 0');
  if FHasExited then begin
    DebugLn(DBG_VERBOSE, ['Received double exit for Thread ', ID]);
    exit(False);
  end;
  Result := FIsPaused;
  DebugLn(DBG_VERBOSE and (Result), ['Warning: Thread already paused', ID]);
  if Result then
    exit;

  FIsPaused := True;
  FIsInInternalPause := False;

  if {FInternalPauseRequested and} (wstopsig(AWaitedStatus) = SIGSTOP) then begin
    DebugLn(DBG_VERBOSE and not FInternalPauseRequested, 'Received SigStop, but had not (yet) requested it. TId=', [Id]);
    FInternalPauseRequested := False;
    FIsInInternalPause := True;
    // no postpone
  end

  else
  if wstopsig(AWaitedStatus) = SIGTRAP then begin
    CheckAndResetInstructionPointerAfterBreakpoint;
    Result := True;
    // TODO: main loop should search all threads for breakpoints
  end

  else
  if wifexited(AWaitedStatus) and (ID <> Process.ProcessID) then begin
    FHasExited := True;
  end

  else
  begin
    // Handle later
    Result := True;
  end;

  //TODO: Handle all signals/exceptions/...
end;

procedure TDbgLinuxThread.ResetPauseStates;
begin
  FIsInInternalPause := False;
  FIsPaused := False;
  ClearExceptionSignal;
end;

procedure TDbgLinuxThread.ClearExceptionSignal;
begin
  FExceptionSignal := 0;
end;

{ TDbgLinuxProcess }

function TDbgLinuxProcess.GetRequiresExecutionInDebuggerThread: boolean;
begin
  Result := True;
end;

procedure TDbgLinuxProcess.InitializeLoaders;
var
  Loader: TDbgImageLoader;
begin
  Loader := TDbgImageLoader.Create(Name);
  if Loader.IsValid then
    Loader.AddToLoaderList(LoaderList)
  else
    Loader.Free;
end;

function TDbgLinuxProcess.CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread;
begin
  IsMainThread:=False;
  if AthreadIdentifier>-1 then
    begin
    IsMainThread := AthreadIdentifier=ProcessID;
    result := DbgThreadClass.Create(Self, AthreadIdentifier, AthreadIdentifier)
    end
  else
    result := nil;
end;

function TDbgLinuxProcess.CreateWatchPointData: TFpWatchPointData;
begin
  Result := TFpIntelWatchPointData.Create;
end;

function TDbgLinuxProcess.CheckForSOLibDebugEvent(AThread: TDbgThread): TFPDEvent;
var
  CurrentAddr: TDBGPtr;
  BList: TFpInternalBreakpointArray;
  SOLibBreakpointFound: Boolean;
  RegularBreakpointFound: Boolean;
  i: Integer;
  ProcMaps: TDbgLinuxMemoryMappingList;
  ProcMap: TDbgLinuxMemoryMapping;
begin
  // When we are not dealing with an SOLibDebugEvent, we have a 'normal'
  // Breakpoint.
  Result := deBreakpoint;

  // When the SOLib-breakpoint has not been set, this is not a SOLibDebugEvent.
  if not Assigned(FSOLibEventBreakpoint) then
    Exit;

  // Loop through all defined breakpoints at the
  // instruction-pointer location, and see if the SOLib or any other breakpoint
  // has been set on the current location,
  RegularBreakpointFound:=False;
  SOLibBreakpointFound:=False;
  CurrentAddr:=AThread.GetInstructionPointerRegisterValue;
  BList := FBreakMap.GetInternalBreaksAtLocation(CurrentAddr);
  if BList <> nil then
    begin
    for i := 0 to Length(BList) -1 do
      begin
      if BList[i] = FSOLibEventBreakpoint then
        SOLibBreakpointFound := True
      else
        RegularBreakpointFound := True;
      end;
    end;

  if RegularBreakpointFound then
    begin
    // Regular breakpoints have precedense. In case there is a library-change,
    // handle that one later.
    //if SOLibBreakpointFound then
    //  FAwaitingLibSOEventsPresent:=True;
    end
  else if SOLibBreakpointFound then
    Result := SynchronizeProcMapsWithLibraryList();
end;

function TDbgLinuxProcess.SynchronizeProcMapsWithLibraryList: TFPDEvent;
var
  ProcMaps: TDbgLinuxMemoryMappingList;
  i: Integer;
  ProcMap: TDbgLinuxMemoryMapping;
  AnId: TDbgPtr;
begin
  Result := deInternalContinue;
  FLibMap.ClearAddedAndRemovedLibraries;
  ProcMaps := ObtainProcMaps;
  try
    // The first entry is the application itself, which we skip, so start with
    // 1.
    for i := 1 to ProcMaps.Count -1 do
      begin
      ProcMap := ProcMaps.Items[i] as TDbgLinuxMemoryMapping;
      // Check if the ProcMap is the first entry of a valid library
      // If the Offset <> 0, it is probably not the 'main' entry we are looking
      // for. When there is no filename, we can not handle it, and when the
      // filename starts with '[', it is a general placeholder.
      if (ProcMap.Offset = 0) and (ProcMap.FileName <> '') and (Copy(ProcMap.FileName,1,1) <> '[') then
        begin
        // Check if this library is already known.
        AnId := TDBGPtr(ProcMap.AddressFrom);
        if not (FLibMap.HasId(AnId)) then
          begin
          // Add the library and trigger a deLoadLibrary event
          AddLib(tDbgLinuxLibrary.Create(Self, ProcMap.FileName, THandle(ProcMap.Inode), TDBGPtr(ProcMap.AddressFrom)));
          Result := deLoadLibrary;
          end
        end;
      end;
  finally
    ProcMaps.Free;
  end;
end;

function TDbgLinuxProcess.ObtainProcMaps: TDbgLinuxMemoryMappingList;

  procedure ParseMapsLine(const Line: string);
  var
    Mapping: TDbgLinuxMemoryMapping;
    Parts: TStringDynArray;
    Addresses: TStringDynArray;
  begin
    Mapping := TDbgLinuxMemoryMapping.Create;
    try
      Parts := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
      Addresses := Parts[0].Split(['-']);
      Mapping.AddressFrom:=PtrInt(Hex2Dec64(Addresses[0]));
      Mapping.AddressTill:=PtrInt(Hex2Dec64(Addresses[1]));

      Mapping.Rights:=Parts[1];
      Mapping.Offset:=Hex2Dec64(Parts[2]);
      Mapping.DeviceId:=Parts[3];
      Mapping.Inode:=StrToInt64(Parts[4]);
      if Length(Parts) > 5 then
        Mapping.FileName:=Parts[5]
      else
        Mapping.FileName:='';

      Result.Add(Mapping);
      Mapping := nil;
    finally
      Mapping.Free;
    end;
  end;

var
  FN: string;
  Buf: string;
  FS: TFileStream;
  BytesRead: Int64;
  TotalBytesRead: Int64;
  BlockLength: Int64;
  MapsStrings: TStringList;
  i: Integer;
begin
  Result := TDbgLinuxMemoryMappingList.Create(True);
  // All memory-mappings are retrieved from /proc/<ps>/maps and stored in
  // FMemoryMappingList
  FN := '/proc/'+IntToStr(ProcessID)+'/maps';

  // First read the contents of /proc/<ps>/maps and place it in a buffer

  // In principle the file should be read in one read-operation, or the file
  // might be changed while reading it. So we use a relatively large buffer.
  // (In principle it is still possible that the file isn't
  // being read in one operation. But let's ignore this for now.)
  FS := TFileStream.Create(FN, fmOpenRead);
  try
    BlockLength:=64*1024;
    TotalBytesRead := 0;
    repeat
      SetLength(Buf, TotalBytesRead + BlockLength);
      BytesRead := FS.Read(Buf[TotalBytesRead+1], BlockLength);
      TotalBytesRead:=TotalBytesRead+BytesRead;
    until BytesRead <= 0;
    SetLength(Buf, TotalBytesRead);
  finally
    FS.Free;
  end;

  // Now use a TStringList to parse the contents of the buffer.
  MapsStrings := TStringList.Create;
  try
    MapsStrings.Text := Buf;
    // Parse the file line-by-line and add the mappings to FMemoryMappingList
    for i := 0 to MapsStrings.Count -1 do
      ParseMapsLine(MapsStrings[i]);
  finally
    MapsStrings.Free;
  end;
end;

procedure TDbgLinuxProcess.AddLib(const ALibrary: tDbgLinuxLibrary);
begin
  AddLibrary(ALibrary, ALibrary.FLoadedTargetImageAddr);
end;

constructor TDbgLinuxProcess.Create(const AFileName: string;
  AnOsClasses: TOSDbgClasses; AMemManager: TFpDbgMemManager;
  AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig);
begin
  FMasterPtyFd:=-1;
  FSingleSteppingThreadID := -1;
  FPostponedSignals := TFpDbgLinuxSignalQueue.Create;
  inherited Create(AFileName, AnOsClasses, AMemManager, AMemModel, AProcessConfig);
end;

destructor TDbgLinuxProcess.Destroy;
begin
  FProcProcess.Free;
  FPostponedSignals.Free;

  if FMasterPtyFd>-1 then
    FpClose(FMasterPtyFd);
  FMasterPtyFd:=-1;

  inherited Destroy;
end;

function TDbgLinuxProcess.StartInstance(AParams, AnEnvironment: TStrings;
  AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags; out
  AnError: TFpError): boolean;
var
  AProcess: TProcessWithRedirect;
  AMasterPtyFd: cint;
  AnExecutabeFilename: string;
begin
  Result := false;
  if FMasterPtyFd>-1 then
    FpClose(FMasterPtyFd);
  FMasterPtyFd:=-1;

  AnExecutabeFilename:=ExcludeTrailingPathDelimiter(Name);
  if DirectoryExists(AnExecutabeFilename) then
  begin
    DebugLn(DBG_WARNINGS, 'Can not debug %s, because it''s a directory',[AnExecutabeFilename]);
    Exit;
  end;

  if not FileExists(Name) then
  begin
    DebugLn(DBG_WARNINGS, 'Can not find  %s.',[AnExecutabeFilename]);
    Exit;
  end;

  AMasterPtyFd:=-1;
  if siRediretOutput in AFlags then
    begin
    if AConsoleTty<>'' then
      DebugLn(DBG_VERBOSE, 'It is of no use to provide a console-tty when the console output is being redirected.');
    GConsoleTty:='';
    if openpty(@AMasterPtyFd, @GSlavePTyFd, nil, nil, nil) <> 0 then
      DebugLn(DBG_WARNINGS, 'Failed to open pseudo-tty. Errcode: '+inttostr(fpgeterrno));
    end
  else
    begin
    GSlavePTyFd:=-1;
    GConsoleTty:=AConsoleTty;
    end;

  AProcess := TProcessWithRedirect.Create(nil);
  try
    AProcess.OnForkEvent:=@OnForkEvent;
    AProcess.Executable:=AnExecutabeFilename;
    AProcess.Parameters:=AParams;
    AProcess.Environment:=AnEnvironment;
    AProcess.CurrentDirectory:=AWorkingDirectory;
    if DBG_PROCESS_HAS_REDIRECT then begin
      AProcess.SetRedirection(dtStdIn,  Config.StdInRedirFile,  Config.FileOverwriteStdIn);
      if (Config.StdOutRedirFile = Config.StdErrRedirFile) then begin
        if Config.StdOutRedirFile <> '' then begin
          FProcProcess.SetRedirection(dtStdOut, Config.StdOutRedirFile, Config.FileOverwriteStdOut or Config.FileOverwriteStdErr);
          FProcProcess.Options := FProcProcess.Options + [poStdErrToOutPut];
        end;
      end
      else begin
        FProcProcess.SetRedirection(dtStdOut, Config.StdOutRedirFile, Config.FileOverwriteStdOut);
        FProcProcess.SetRedirection(dtStdErr, Config.StdErrRedirFile, Config.FileOverwriteStdErr);
      end;
    end;

    AProcess.Execute;
    Init(AProcess.ProcessID, 0);
    FMasterPtyFd := AMasterPtyFd;
    FProcProcess := AProcess;
    sleep(100);
    Result := ProcessID > 0;
  except
    on E: Exception do
    begin
      DebugLn(DBG_WARNINGS, Format('Failed to start process "%s". Errormessage: "%s".',[Name, E.Message]));
      AProcess.Free;

    if GSlavePTyFd>-1 then
      FpClose(GSlavePTyFd);
    if AMasterPtyFd>-1 then
      FpClose(AMasterPtyFd);
    FMasterPtyFd:=-1;
    end;
  end;
end;

function TDbgLinuxProcess.AttachToInstance(APid: Integer; out AnError: TFpError
  ): boolean;
begin
  Result := fpPTrace(PTRACE_ATTACH, APid, nil, Pointer(PTRACE_O_TRACECLONE)) = 0;
  Init(APid, 0);
  // TODO: change the filename to the actual exe-filename. Load the correct dwarf info
end;

function TDbgLinuxProcess.ReadWordSize(Adr: TDbgPtr; out AVal: TDBGPtr
  ): boolean;
var
  e: integer;
begin
  AVal := TDbgPtr(fpPTrace(PTRACE_PEEKDATA, FCurrentThreadId, pointer(Adr), nil));
  e := fpgeterrno;
  Result := e = 0;
  if not Result then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to read data at address '+FormatAddress(Adr)+' from processid '+inttostr(FCurrentThreadId)+'. Errcode: '+inttostr(e));
    result := false;
    end;
end;

function TDbgLinuxProcess.WriteWordSize(Adr: TDbgPtr; AVal: TDBGPtr): boolean;
var
  e: LongInt;
begin
  fpPTrace(PTRACE_POKEDATA, FCurrentThreadId, pointer(Adr), pointer(AVal));
  e := fpgeterrno;
  Result := e = 0;
  if not Result then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to write data at address '+FormatAddress(Adr)+' from processid '+inttostr(FCurrentThreadId)+'. Errcode: '+inttostr(e));
    result := false;
    end;
end;

function TDbgLinuxProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData): Boolean;
var
  APartSize: Cardinal;
begin
  Result := ReadData(AAdress, ASize, AData, APartSize);
  Result := Result and (APartSize = ASize);
end;

function TDbgLinuxProcess.ReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; out AData; out APartSize: Cardinal): Boolean;
var
  WordSize, WordAlignOffset, BytesDone: integer;
  TmpBuffer: TDbgPtr;
  DataWritePtr: pbyte;
  RemainingDataSize: int64;
  TargetReadAddress: TDBGPtr;

  localiov, remoteiov: iovec;
  readcnt: ssize_t;
begin
  result := false;
  if ASize = 0 then
    exit;
  APartSize := 0;
  BytesDone := 0;
  try
    // since kernel >= 3.2
    // https://kernelnewbies.org/Linux_3.2#Cross_memory_attach
    if Assigned(process_vm_readv) then
    begin
      localiov.iov_base := @AData;
      localiov.iov_len := ASize;
      remoteiov.iov_base := Pointer(AAdress);
      remoteiov.iov_len := ASize;
      readcnt := process_vm_readv(ProcessID, @localiov, 1, @remoteiov, 1, 0);
      if readcnt > 0 then
        APartSize := Cardinal(readcnt);
      if ASize = APartSize then
        Exit;
    end;

    fpseterrno(0);
    TargetReadAddress := AAdress + APartSize;
    DataWritePtr      := @AData  + APartSize;
    RemainingDataSize := ASize   - APartSize;
    WordSize:=DBGPTRSIZE[Mode];
    WordAlignOffset := TargetReadAddress and TDBGPtr(WordSize - 1);

    {$ifNdef LINUX_NO_PTRACE_ALIGN}  // according to man, only peek/poke_user need align
    if WordAlignOffset <> 0 then begin
      {$PUSH}{$R-}{$Q-}
      if not ReadWordSize(TargetReadAddress-WordAlignOffset, TmpBuffer) then
        Exit;  // APartSize is still correct
      {$POP}

      BytesDone := WordSize - WordAlignOffset;
      if BytesDone > RemainingDataSize then
        BytesDone := RemainingDataSize;
      move(PByte(@TmpBuffer)[WordAlignOffset], DataWritePtr^, BytesDone);
      inc(DataWritePtr, BytesDone);
      inc(TargetReadAddress, BytesDone);
    end;
    {$endif}

    dec(RemainingDataSize, WordSize - 1); // full words only

    while BytesDone < RemainingDataSize do begin
      if not ReadWordSize(TargetReadAddress, TmpBuffer) then
        Exit;
      move(TmpBuffer, DataWritePtr^, WordSize);
      inc(DataWritePtr, WordSize);
      inc(BytesDone, WordSize);
      inc(TargetReadAddress, WordSize);
    end;

    RemainingDataSize := ASize - APartSize - BytesDone;
    assert((RemainingDataSize>=0) and (RemainingDataSize<WordSize));

    if RemainingDataSize > 0 then begin
      if not ReadWordSize(TargetReadAddress, TmpBuffer) then
        Exit;
      move(TmpBuffer, DataWritePtr^, RemainingDataSize);
      inc(BytesDone, RemainingDataSize);
    end;

  finally
    APartSize := APartSize + BytesDone;
    Result := APartSize > 0;
    if Result then
      MaskBreakpointsInReadData(AAdress, APartSize, AData);
  end;
end;

function TDbgLinuxProcess.WriteData(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData): Boolean;
var
  WordSize, BytesDone: integer;
  BufSize: int64;
  AVal: TDBGPtr;
  buf: PByte;
  AAdressAlign: TDBGPtr;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxProcess.WriteData');{$ENDIF}
  result := false;
  fpseterrno(0);
  BytesDone := 0;
  buf := @AData;
  BufSize := ASize;
  WordSize:=DBGPTRSIZE[Mode];

  {$ifNdef LINUX_NO_PTRACE_ALIGN}  // according to man, only peek/poke_user need align
  AAdressAlign := AAdress and (not TDBGPtr(WordSize - 1));
  if AAdressAlign <> AAdress then begin
    if not ReadWordSize(AAdressAlign, AVal) then
      Exit;
    BytesDone := WordSize - (AAdress-AAdressAlign);
    if BytesDone > ASize then
      BytesDone := ASize;
    move(buf[0], PByte(@AVal)[AAdress-AAdressAlign], BytesDone);
    if not WriteWordSize(AAdressAlign, AVal) then
      Exit;
    inc(AAdressAlign, WordSize);
  end;
  {$else}
  AAdressAlign := AAdress;
  {$endif}

  dec(BufSize, WordSize - 1); // full words only

  while BytesDone < BufSize do begin
    move(buf[BytesDone], AVal, WordSize);
    if not WriteWordSize(AAdressAlign, AVal) then
      Exit;
    inc(BytesDone, WordSize);
    inc(AAdressAlign, WordSize);
  end;

  BufSize := ASize - BytesDone;
  assert((BufSize>=0) and (BufSize<WordSize));

  if BufSize > 0 then begin
    if not ReadWordSize(AAdressAlign, AVal) then
      Exit;
    move(buf[BytesDone], AVal, BufSize);
    if not WriteWordSize(AAdressAlign, AVal) then
      Exit;
  end;

  result := true;
end;

function TDbgLinuxProcess.CallParamDefaultLocation(AParamIdx: Integer
  ): TFpDbgMemLocation;
begin
  case Mode of
    dm32: case AParamIdx of
       -1: Result := RegisterLoc(0); // EAX  // result
        0: Result := RegisterLoc(0); // EAX
        1: Result := RegisterLoc(2); // EDX
        2: Result := RegisterLoc(1); // ECX
      else
        Result := UnInitializedLoc;
      end;
    dm64: case AParamIdx of
       -1: Result := RegisterLoc(0); // RAX  // result
        0: Result := RegisterLoc(5); // RDI
        1: Result := RegisterLoc(4); // RSI
        2: Result := RegisterLoc(1); // RDX
        3: Result := RegisterLoc(2); // RCX
        4: Result := RegisterLoc(8); // R8
        5: Result := RegisterLoc(9); // R9
      else
        Result := UnInitializedLoc;
      end;
  end;
end;

function TDbgLinuxProcess.CheckForConsoleOutput(ATimeOutMs: integer): integer;
Var
  f: TfdSet;
  sleepytime: ttimeval;
begin
  sleepytime.tv_sec := ATimeOutMs div 1000;
  sleepytime.tv_usec := (ATimeOutMs mod 1000)*1000;
  FpFD_ZERO(f);
  fpFD_SET(FMasterPtyFd,f);
  result := fpselect(FMasterPtyFd+1,@f,nil,nil,@sleepytime);
end;

function TDbgLinuxProcess.GetConsoleOutput: string;
var
  ABytesAvailable: DWord;
  ABytesRead: cint;
  s: string;
begin
  if fpioctl(FMasterPtyFd, FIONREAD, @ABytesAvailable)<0 then
    ABytesAvailable := 0;

  result := '';
  while ABytesAvailable>0 do
  begin
    setlength(s, ABytesAvailable);
    ABytesRead := fpRead(FMasterPtyFd, s[1], ABytesAvailable);
    SetLength(s, ABytesRead);
    Result := Result + s;

    if fpioctl(FMasterPtyFd, FIONREAD, @ABytesAvailable)<0 then
      ABytesAvailable := 0;
  end;
end;

procedure TDbgLinuxProcess.SendConsoleInput(AString: string);
begin
  if FpWrite(FMasterPtyFd, AString[1], length(AString)) <> Length(AString) then
    DebugLn(DBG_WARNINGS, 'Failed to send input to console.');
end;

procedure TDbgLinuxProcess.TerminateProcess;
begin
  FIsTerminating:=true;
  if fpkill(ProcessID,SIGKILL)<>0 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to send SIGKILL to process %d. Errno: %d',[ProcessID, errno]);
    FIsTerminating:=false;
    end;
end;

function TDbgLinuxProcess.Pause: boolean;
begin
  result := fpkill(ProcessID, SIGTRAP)=0;
  PauseRequested:=true;
  if not result then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to send SIGTRAP to process %d. Errno: %d',[ProcessID, errno]);
    end;
end;

function TDbgLinuxProcess.Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean;
begin
  RemoveAllBreakPoints;
  fpPTrace(PTRACE_DETACH, AThread.ID, nil, pointer(TDbgLinuxThread(AThread).FExceptionSignal));
  Result := True;
end;

procedure TDbgLinuxProcess.LoadInfo;
var
  i: Integer;
  InterpSection: PDbgImageSection;
  Astat: Stat;
  ALib: TDbgLibrary;

begin
  inherited LoadInfo;

  // This would be strange, but you never know.
  if Assigned(FSOLibEventBreakpoint) then
    Raise Exception.Create('SOLib event-breakpoint already exists.');

  // Check if the library supports dynamic loading by searching for the
  // .interp section.
  for i := 0 to LoaderList.Count -1 do
    begin
    InterpSection := LoaderList.Items[i].Section['.interp'];
    if assigned(InterpSection) then
      begin
      // Try to retrieve the inode of the file (library) in the .interp section.
      // This is the filename of the library that handles the dynamic loading.
      if FpStat(PChar(InterpSection^.RawData), AStat) = 0 then
        begin
        // Syncronize all loaded libraries, and obtain the library for the
        // dynamic-loading, based on the .interp section.
        SynchronizeProcMapsWithLibraryList();
        if FLibMap.GetLib(Astat.st_ino, ALib) then
          begin
          // Set a breakpoint at _dl_debug_state. This procedure is called after
          // one or more libraries have been loaded. This breakpoint is used to
          // detect the (un)loading of libraries.
          FSOLibEventBreakpoint := AddInternalBreak('_dl_debug_state', False, ALib);
          TFpDbgBreakpointBase(FSOLibEventBreakpoint).FreeByDbgProcess := True;
          end
        end;
      end;
    end;
end;

function TDbgLinuxProcess.CanContinueForWatchEval(ACurrentThread: TDbgThread
  ): boolean;
begin
  Result := inherited CanContinueForWatchEval(ACurrentThread);
  Result := Result and (TDbgLinuxThread(ACurrentThread).FExceptionSignal = 0)
end;

function TDbgLinuxProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean;
  function CheckNoError: Boolean;
  var
    e: integer;
  begin
    e := fpgeterrno;
    Result := e = 0;
    if not Result then
      DebugLn(DBG_WARNINGS, 'Failed to continue process. Errcode: '+inttostr(e));
  end;

var
  ThreadToContinue: TDbgLinuxThread;
  WaitStatus: cint;
  PID: THandle;
  IP: TDBGPtr;
begin
  if (FSOLibEventBreakpoint <> nil) and not FSOLibEventBreakpoint.Enabled then
    FSOLibEventBreakpoint.Enabled:=True;
  {$IFDEF DebuglnLinuxDebugEvents}
  debuglnEnter(['>>>>> TDbgLinuxProcess.Continue TID:', dbgsThreadId(AThread), ' SingleStep:', SingleStep ]); try
  {$ENDIF}
  FSingleSteppingThreadID := -1;

  // Terminating process and all threads
  if FIsTerminating and (AThread <> nil) then begin
    fpseterrno(0);
    AThread.BeforeContinue;
    fpPTrace(PTRACE_KILL, AThread.ID, pointer(1), nil);
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
    exit;
  end;

  if (AThread <> nil) and TDbgLinuxThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
    AThread.NextIsSingleStep:=SingleStep;

  // check for pending events in other threads
  if FPostponedSignals.Count > 0 then begin
    {$IFDEF DebuglnLinuxDebugEvents}
    debugln(FPDBG_LINUX, ['Exit for DEFERRED event TID']);
    {$ENDIF}
    exit;
  end;

  // check other threads if they need a singlestep
  for TDbgThread(ThreadToContinue) in FThreadMap do
    if (ThreadToContinue <> AThread) and ThreadToContinue.FIsPaused and
       (ThreadToContinue.SuspendCount <= 0)
    then begin
      IP := ThreadToContinue.GetInstructionPointerRegisterValue;
      if HasInsertedBreakInstructionAtLocation(IP) or ThreadToContinue.NextIsSingleStep then begin
        TempRemoveBreakInstructionCode(IP);
        ThreadToContinue.BeforeContinue;

        while (ThreadToContinue.GetInstructionPointerRegisterValue = IP) do begin
          {$IFDEF DebuglnLinuxDebugEvents}
          Debugln(FPDBG_LINUX, ['Single-stepping other TID: ', ThreadToContinue.ID]);
          {$ENDIF}
          fpseterrno(0);
          fpPTrace(PTRACE_SINGLESTEP, ThreadToContinue.ID, pointer(1), pointer(TDbgLinuxThread(ThreadToContinue).FExceptionSignal));

          TDbgLinuxThread(ThreadToContinue).ResetPauseStates;
          ThreadToContinue.FIsPaused := True;
          if CheckNoError then begin
            PID := fpWaitPid(ThreadToContinue.ID, WaitStatus, __WALL);
            if PID <> ThreadToContinue.ID then begin
              DebugLn(DBG_WARNINGS, ['XXXXX Error single stepping other thread ', ThreadToContinue.ID, ' waitpid got ', PID, ', ',WaitStatus, ' err ', Errno]);
              break;
            end;
            if ThreadToContinue.NextIsSingleStep then begin
              FPostponedSignals.AddSignal(PID, WaitStatus);
              break;
            end;
            if (wstopsig(WaitStatus) = SIGTRAP) then
              break; // if the command jumps back an itself....
          end
          else begin
            DebugLn(DBG_WARNINGS, ['Error single stepping other thread ', ThreadToContinue.ID]);
            break;
          end;
        end;

      end;
    end;

  if FPostponedSignals.Count > 0 then begin
    {$IFDEF DebuglnLinuxDebugEvents}
    debugln(FPDBG_LINUX, ['Exit for DEFERRED SingleSteps event TID']);
    {$ENDIF}
    exit;
  end;

  if (AThread <> nil) and TDbgLinuxThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
  if HasInsertedBreakInstructionAtLocation(AThread.GetInstructionPointerRegisterValue) then begin
    TempRemoveBreakInstructionCode(AThread.GetInstructionPointerRegisterValue);
    TDbgLinuxThread(AThread).FIsSteppingBreakPoint := True;
    fpseterrno(0);
    AThread.BeforeContinue;
    {$IFDEF DebuglnLinuxDebugEvents}
    Debugln(FPDBG_LINUX, ['Single-stepping current']);
    {$ENDIF}
    FSingleSteppingThreadID := AThread.ID;
    fpPTrace(PTRACE_SINGLESTEP, AThread.ID, pointer(1), pointer(TDbgLinuxThread(AThread).FExceptionSignal));
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
    exit;
  end;

  RestoreTempBreakInstructionCodes;

  ThreadsBeforeContinue;

  // start all other threads
  for TDbgThread(ThreadToContinue) in FThreadMap do begin
    if (ThreadToContinue <> AThread) and (ThreadToContinue.FIsPaused) and
       (ThreadToContinue.SuspendCount <= 0)
    then begin
      fpseterrno(0);
      {$IFDEF DebuglnLinuxDebugEvents}
      Debugln(FPDBG_LINUX, ['RUN other TID: ', ThreadToContinue.ID]);
      {$ENDIF}
      fpPTrace(PTRACE_CONT, ThreadToContinue.ID, pointer(1), pointer(ThreadToContinue.FExceptionSignal));
      CheckNoError; // only log
      ThreadToContinue.ResetPauseStates;
    end;
  end;

  if (AThread <> nil) and TDbgLinuxThread(AThread).FIsPaused then  // in case of deInternal, it may not be paused and can be ignored
  if not FIsTerminating then begin
    fpseterrno(0);
    //AThread.BeforeContinue;
    {$IFDEF DebuglnLinuxDebugEvents}
    Debugln(FPDBG_LINUX, ['RUN ']);
    {$ENDIF}
    if AThread.NextIsSingleStep then
      fpPTrace(PTRACE_SINGLESTEP, AThread.ID, pointer(1), pointer(TDbgLinuxThread(AThread).FExceptionSignal))
    else
      fpPTrace(PTRACE_CONT, AThread.ID, pointer(1), pointer((TDbgLinuxThread(AThread).FExceptionSignal)));
    TDbgLinuxThread(AThread).ResetPauseStates;
    Result := CheckNoError;
  end;

  {$IFDEF DebuglnLinuxDebugEvents}
  finally debuglnExit(['<<<<< TDbgLinuxProcess.Continue ' ]); end;
  {$ENDIF}
end;

function TDbgLinuxProcess.WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean;
var
  PID: THandle;
begin
  ThreadIdentifier:=-1;
  ProcessIdentifier:=-1;

  if FSingleSteppingThreadID <> -1 then begin
    PID:=FpWaitPid(FSingleSteppingThreadID, FStatus, __WALL);
    if PID <> FSingleSteppingThreadID then
      DebugLn(DBG_WARNINGS, ['XXXXX Error: single stepping current thread ', FSingleSteppingThreadID, ' waitpid got ', PID, ', ',FStatus, ' err ', Errno]);
  end
  else
  If not FPostponedSignals.GetNextSignal(PID, FStatus) then
    PID:=FpWaitPid(-1, FStatus, __WALL);

  RestoreTempBreakInstructionCodes; // should only happen after single step, so all threads should be paused

  result := PID<>-1;
  if not result then
    DebugLn(DBG_WARNINGS, 'Failed to wait for debug event. Errcode: %d', [fpgeterrno])
  else
    begin
    ThreadIdentifier := PID;
    FCurrentThreadId := PID;

    if not FProcessStarted and (PID <> ProcessID) then
      DebugLn(DBG_WARNINGS, 'ThreadID of main thread does not match the ProcessID');

    ProcessIdentifier := ProcessID;
    {$IFDEF DebuglnLinuxDebugEvents}
    debugln(FPDBG_LINUX, ['##### GOT EVENT FOR ',pid, ' st ', FStatus]);
    {$ENDIF}
    end;
end;

function TDbgLinuxProcess.AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent;

  function ExistsPendingSignal(out PID: THandle; out WaitStatus: cint;
    out AThread: TDbgLinuxThread; ANoHang: Boolean): Boolean;
  var
    Opts: cint;
  begin
    AThread := nil;
    Opts := __WALL;
    if ANoHang then
      Opts := Opts or WNOHANG;

    PID:=FpWaitPid(-1, WaitStatus, Opts);
    Result := (PID <> 0) and (PID <> -1);
    if not Result then
      exit;

    if not FThreadMap.GetData(PID, AThread) then
      AThread := nil;
    DebugLn(DBG_VERBOSE, ['Got SIGNAL for thread: ', pid, ' Status: ',WaitStatus, ' Found thread:', AThread <> nil]);
  end;

//var
//  NewThreadID: culong;
var
  ThreadToPause, ThreadSignaled: TDbgLinuxThread;
  Pid: THandle;
  WaitStatus: cint;
  it: TThreadMapUnLockedEnumerator;
begin
  if AThread = nil then begin // should not happen... / just assume the most likely safe failbacks
    if FIsTerminating then
      result := deExitProcess
    else
      result := deInternalContinue;
  end;

  TDbgLinuxThread(AThread).FExceptionSignal:=0;
  TDbgLinuxThread(AThread).FIsPaused := True;
  if wifexited(FStatus) or wifsignaled(FStatus) then
    begin
    if AThread.ID=ProcessID then
      begin
      // Main thread stop -> application exited
      SetExitCode(wexitStatus(FStatus));
      result := deExitProcess
      end
    else
      begin
      // Thread stopped, just continue
      RemoveThread(AThread.Id);
      result := deInternalContinue;
      end;
    end
  else if WIFSTOPPED(FStatus) then
    begin
    //DebugLn(DBG_WARNINGS, 'Stopped ',FStatus, ' signal: ',wstopsig(FStatus));

    if (FStatus >> 8) = (SIGTRAP or (PTRACE_EVENT_CLONE << 8)) then
      begin
      // New thread started (stopped in 'parent' thread)
      Result := deInternalContinue;

      // Usefull in case of debugging:
      //if fpPTrace(PTRACE_GETEVENTMSG, AThread.ID, nil, @NewThreadID) = -1 then
      //  DebugLn(DBG_WARNINGS, 'Failed to retrieve ThreadId of new thread. Errcode: %d', [fpgeterrno]);
      Exit;
      end;

    if (not FProcessStarted) and (wstopsig(FStatus) <> SIGTRAP) then begin
      // attached, should be SigStop, but may be out of order
      debugln(DBG_VERBOSE, ['Attached ', wstopsig(FStatus)]);
      result := deCreateProcess;
      FProcessStarted:=true;
      if not wstopsig(FStatus) = SIGSTOP then
        FPostponedSignals.AddSignal(AThread.Id, FStatus);
    end

    else
    case wstopsig(FStatus) of
      SIGTRAP:
        begin
        if not FProcessStarted then
          begin
          result := deCreateProcess;
          FProcessStarted:=true;
          if fpPTrace(PTRACE_SETOPTIONS, ProcessID, nil,  Pointer( PTRACE_O_TRACECLONE) ) <> 0 then
            writeln('Failed to set set trace options. Errcode: '+inttostr(fpgeterrno));
          end
        else
// TODO: check it is not a real breakpoint
// or end of single step
//          if TDbgLinuxThread(AThread).FInternalPauseRequested then begin
//            DebugLn(DBG_VERBOSE, ['Received late SigTrag for thread ', AThread.ID]);
//            result := deInternalContinue; // left over signal
//          end
//          else
//            begin
            result := deBreakpoint; // or pause requested
            if not TDbgLinuxThread(AThread).FIsSteppingBreakPoint then
              AThread.CheckAndResetInstructionPointerAfterBreakpoint;
//            end;
        end;
      SIGBUS:
        begin
        ExceptionClass:='SIGBUS';
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGBUS;
        result := deException;
        end;
      SIGINT:
        begin
        ExceptionClass:='SIGINT';
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGINT;
        result := deException;
        end;
      SIGSEGV:
        begin
        ExceptionClass:='SIGSEGV';
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGSEGV;
        result := deException;
        end;
      SIGCHLD:
        begin
        TDbgLinuxThread(AThread).FExceptionSignal:=SIGCHLD;
        result := deInternalContinue;
        end;
      SIGKILL:
        begin
        if FIsTerminating then
          result := deInternalContinue
        else
          begin
          ExceptionClass:='SIGKILL';
          TDbgLinuxThread(AThread).FExceptionSignal:=SIGKILL;
          result := deException;
          end;
        end;
      SIGSTOP:
        begin
          // New thread (stopped within the new thread)
          result := deInternalContinue;
        end
      else
        begin
        ExceptionClass:='Unknown exception code '+inttostr(wstopsig(FStatus));
        TDbgLinuxThread(AThread).FExceptionSignal:=wstopsig(FStatus);
        result := deException;
        end;
    end; {case}
    if result=deException then
      ExceptionClass:='External: '+ExceptionClass;
    end
  else
    raise exception.CreateFmt('Received unknown status %d from process with pid=%d',[FStatus, ProcessID]);

  TDbgLinuxThread(AThread).FIsSteppingBreakPoint := False;


  if (result = deBreakpoint) and (AThread <> nil) then
    // Check if the breakpoint is the special breakpoint that is inserted to
    // detect the (un)loading of libraries.
    Result := CheckForSOLibDebugEvent(AThread);

  if Result in [deException, deBreakpoint, deFinishedStep] then begin // deFinishedStep will not be set here
    {$IFDEF DebuglnLinuxDebugEvents}
    debuglnenter('STOP ALL THREADS');
    {$ENDIF}
    // Signal all other threads to pause
    for TDbgThread(ThreadToPause) in FThreadMap do begin
      if (ThreadToPause <> AThread) then begin
        while  (not ThreadToPause.FIsPaused) do begin

          // Check if any thread is already interrupted
          while ExistsPendingSignal(Pid, WaitStatus, ThreadSignaled, True) do begin
            if (ThreadSignaled = nil) or
               (ThreadSignaled.CheckSignalForPostponing(WaitStatus))
            then
              FPostponedSignals.AddSignal(PID, WaitStatus);
          end;
          if ThreadToPause.FIsPaused or ThreadToPause.FHasExited then
            break;

          DebugLn(DBG_VERBOSE and (ThreadToPause.FInternalPauseRequested), ['Re-Request Internal pause for ', ThreadToPause.ID]);
          ThreadToPause.FInternalPauseRequested:=false;
          if not ThreadToPause.RequestInternalPause then // will fail, if already paused
             break;

          if ExistsPendingSignal(Pid, WaitStatus, ThreadSignaled, False) then begin
            if (ThreadSignaled = nil) or
               (ThreadSignaled.CheckSignalForPostponing(WaitStatus))
            then
              FPostponedSignals.AddSignal(PID, WaitStatus);
          end;

        end;
      end;
    end;
    {$IFDEF DebuglnLinuxDebugEvents}
    debuglnexit('<<');
    {$ENDIF}
  end;

  it := TThreadMapUnLockedEnumerator.Create(FThreadMap); // At this point no other thread (ide-main, ...) can add an iterator to the map
  it.First;
  while not it.EOM do begin
    TDbgThread(ThreadToPause) := it.Current;
    if ThreadToPause.FHasExited then begin
      Process.RemoveThread(ThreadToPause.ID); // TODO: postpone ?
      if ThreadToPause <> AThread then
        ThreadToPause.Free;
    end;
    it.Next;
  end;
  it.Free;

  {$IFDEF DebuglnLinuxDebugEvents}
  for TDbgThread(ThreadToPause) in FThreadMap do
  debugln(FPDBG_LINUX, [ThreadToPause.id, ' =athrd:', ThreadToPause = AThread, ' psd:', ThreadToPause.FIsPaused,ThreadToPause.FIsInInternalPause, ' exs:', ThreadToPause.FExceptionSignal, '  sstep:',ThreadToPause.NextIsSingleStep]);
  debugln(FPDBG_LINUX, '<<<<<<<<<<<<<<<<<<<<<<<<');
  {$ENDIF}

end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_LINUX := DebugLogger.FindOrRegisterLogGroup('FPDBG_LINUX' {$IFDEF DebuglnLinuxDebugEvents} , True {$ENDIF} );

  process_vm_lib := LoadLibrary('libc.' + SharedSuffix);
  if process_vm_lib = 0 then
    process_vm_lib := LoadLibrary('libc.' + SharedSuffix + '.6');
  if process_vm_lib <> 0 then
    process_vm_readv := Tprocess_vm_readv(GetProcAddress(process_vm_lib, 'process_vm_readv'));

finalization

  if process_vm_lib <> 0 then
    FreeLibrary(process_vm_lib);

end.

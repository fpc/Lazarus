{%main ../process.pp}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2008 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
{$DEFINE OS_HASEXITCODE}

{$IFDEF FPC_DOTTEDUNITS}
uses
   System.CTypes,
   UnixApi.Types,
   UnixApi.Unix,
   UnixApi.Base;
{$ELSE FPC_DOTTEDUNITS}
uses
   ctypes,
   UnixType,
   Unix,
   Baseunix;
{$ENDIF FPC_DOTTEDUNITS}

const
  SNoCommandLine        = 'Cannot execute empty command-line';
  SErrNoSuchProgram     = 'Executable not found: "%s"';
  SErrNoTerminalProgram = 'Could not detect X-Terminal program';
  SErrCannotFork        = 'Failed to Fork process';

Const
  // unused as of yet, I assume these are nice levels ?
  PriorityConstants : Array [TProcessPriority] of Integer =
                      (-10,19,0,-20,5,-5);  // was (20,20,0,-20) before introduction of last two levels.

Const
  GeometryOption : String = '-geometry';
  TitleOption : String ='-title';




procedure TProcess.CloseProcessHandles;

begin
 // Do nothing. Win32 call.
end;

Function TProcess.GetExitCode : Integer;

begin
  if (Not Running) and wifexited(FExitCode) then
    Result:=wexitstatus(FExitCode)
  else
    Result:=0;
end;


Function TProcess.PeekExitStatus : Boolean;
var
  res: cint;
begin
  repeat
    res:=fpWaitPid(Handle,pcint(@FExitCode),WNOHANG);
  until (res<>-1) or (fpgeterrno<>ESysEINTR);
  result:=res=Handle;
  If Not Result then
    FexitCode:=cardinal(-1); // was 0, better testable for abnormal exit.
end;

Type
  TPCharArray = Array[Word] of PAnsiChar;
  PPCharArray = ^TPcharArray;

Function StringsToPCharList(List : TStrings) : PPAnsiChar;

Var
  I : Integer;
  S : AnsiString;

begin
  I:=(List.Count)+1;
  GetMem(Result,I*sizeOf(PAnsiChar));
  PPCharArray(Result)^[List.Count]:=Nil;
  For I:=0 to List.Count-1 do
    begin
    {$if SIZEOF(CHAR)=2}
    S:=UTF8Encode(List[i]);
    {$ELSE}
    S:=List[i];
    {$ENDIF}
    Result[i]:=StrNew(PAnsiChar(S));
    end;
end;

Procedure FreePCharList(List : PPAnsiChar);

Var
  I : integer;

begin
  I:=0;
  While List[i]<>Nil do
    begin
    StrDispose(List[i]);
    Inc(I);
    end;
  FreeMem(List);
end;



Function DetectXterm : String;

  Function TestTerminal(S : String) : Boolean;

  begin
    Result:=FileSearch(s,GetEnvironmentVariable('PATH'),False)<>'';
    If Result then
      XTermProgram:=S;
  end;

  Function TestTerminals(Terminals : Array of String) : Boolean;

  Var
    I : integer;
  begin
    I:=Low(Terminals);
    Result:=False;
    While (Not Result) and (I<=High(Terminals)) do
      begin
      Result:=TestTerminal(Terminals[i]);
      inc(i);
      end;
  end;

Const
  xterm = 'xterm';
  Konsole   = 'konsole';
  GNomeTerm = 'gnome-terminal';
  // Windowmaker
  aterm = 'aterm';
  wterm = 'wterm';
  // Xfce
  xfceterm = 'xfce4-terminal';
  rxvt = 'rxvt';
  xtermemulator = 'x-terminal-emulator';

  DefaultTerminals : Array [1..6] of string
                   = (xtermemulator,xterm,aterm,wterm,rxvt,xfceterm);

Var
  D :String;

begin
  If (XTermProgram='') then
    begin
    // try predefined
    If Length(TryTerminals)>0 then
      TestTerminals(TryTerminals);
    // try session-specific terminal first.
    if (XTermProgram='') then
      begin
      D:=LowerCase(GetEnvironmentVariable('DESKTOP_SESSION'));
      If (Pos('kde',D)<>0) then
        begin
        TestTerminal(konsole);
        end
      else if (D='gnome') then
        begin
        TestTerminal(gnometerm);
        end
      else if (D='windowmaker') then
        begin
        If not TestTerminal(aterm) then
          TestTerminal(wterm);
        end
      else if (D='xfce') then
        TestTerminal(xfceterm);
      end;
    if (XTermProgram='') then
      TestTerminals(DefaultTerminals)
    end;
  Result:=XTermProgram;
  If (Result='') then
    Raise EProcess.Create(SErrNoTerminalProgram);
end;

Function MakeCommand(P : TProcess) : PPAnsiChar;

{$ifdef darwin}
Const
  TerminalApp = 'open';
{$endif}
{$ifdef haiku}
Const
  TerminalApp = 'Terminal';
{$endif}
  
Var
  Cmd : String;
  S  : TStringList;
  G : String;
  PA,PC : String;
  
begin
  PA:=P.ApplicationName;
  PC:=P.CommandLine;
  If (PA='') and (PC='') and (P.Executable='') then
    Raise EProcess.Create(SNoCommandline);
  S:=TStringList.Create;
  try
    if (PA='') and (PC='') then
      begin
      S.Assign(P.Parameters);
      S.Insert(0,P.Executable);
      end
    else
      begin
      If (PA<>'') then
        Cmd:=PA
      else
        Cmd:=PC;
      CommandToList(Cmd,S);
      end;
    if poNewConsole in P.Options then
      begin
      {$ifdef haiku}
      If (P.ApplicationName<>'') then
        begin
        S.Insert(0,P.ApplicationName);
        S.Insert(0,'--title');
        end;
      {$endif}
      {$if defined(darwin) or defined(haiku)}
      S.Insert(0,TerminalApp);
      {$else}
      S.Insert(0,'-e');
      If (PA<>'') then
        begin
        S.Insert(0,PA);
        S.Insert(0,'-title');
        end;
      if suoUseCountChars in P.StartupOptions then
        begin
        S.Insert(0,Format('%dx%d',[P.dwXCountChars,P.dwYCountChars]));
        S.Insert(0,'-geometry');
        end;
      If (P.XTermProgram<>'') then
        S.Insert(0,P.XTermProgram)
      else
        S.Insert(0,DetectXterm);
      {$endif}
      end;
    {$ifndef haiku}
    if (PA<>'') then
      begin
      S.Add(TitleOption);
      S.Add(PA);
      end;
    G:='';
    if (suoUseSize in P.StartupOptions) then
      g:=format('%dx%d',[P.dwXSize,P.dwYsize]);
    if (suoUsePosition in P.StartupOptions) then
      g:=g+Format('+%d+%d',[P.dwX,P.dwY]);
    if G<>'' then
      begin
      S.Add(GeometryOption);
      S.Add(g);
      end;
    {$endif}
    Result:=StringsToPcharList(S);
  Finally
    S.free;
  end;
end;

Function GetLastError : Integer;

begin
  Result:=-1;
end;

Type
  TPipeEnd = (peRead,peWrite);
  TPipePair = Array[TPipeEnd] of cint;

Procedure CreatePipes(Var HI,HO,HE : TPipePair; CI, CE : Boolean);

  Procedure CreatePair(Var P : TPipePair);

   begin
    If not CreatePipeHandles(P[peRead],P[peWrite]) then
      Raise EProcess.Create(SErrCannotCreatePipes);
   end;

  Procedure ClosePair(Var P : TPipePair);

  begin
    if (P[peRead]<>-1) then
      FileClose(P[peRead]);
    if (P[peWrite]<>-1) then
      FileClose(P[peWrite]);
  end;

begin
  HO[peRead]:=-1;HO[peWrite]:=-1;
  HI[peRead]:=-1;HI[peWrite]:=-1;
  HE[peRead]:=-1;HE[peWrite]:=-1;
  Try
    CreatePair(HO);
    if CI then
      CreatePair(HI);
    If CE then
      CreatePair(HE);
  except
    ClosePair(HO);
    ClosePair(HI);
    If CE then
      ClosePair(HE);
    Raise;
  end;
end;

Function safefpdup2(fildes, fildes2 : cInt): cInt;
begin
  repeat
    safefpdup2:=fpdup2(fildes,fildes2);
  until (safefpdup2<>-1) or (fpgeterrno<>ESysEINTR);
end;

Procedure TProcess.SysExecute;

Var
  PID      : Longint;
  FEnv     : PPAnsiChar;
  Argv     : PPAnsiChar;
  FoundName,
  PName    : String;

begin
  FDescriptors[phtInput].PrepareHandles;
  FDescriptors[phtOutput].PrepareHandles;
  if not (poStderrToOutPut in Options) then
    FDescriptors[phtError].PrepareHandles;
  Try
    if FEnvironment.Count<>0 then
      FEnv:=StringsToPcharList(FEnvironment)
    else
      FEnv:=Nil;
    Try
      Argv:=MakeCommand(Self);
      Try
        If (Argv<>Nil) and (ArgV[0]<>Nil) then
          PName:=StrPas(Argv[0])
        else
          begin
          // This should never happen, actually.
          PName:=ApplicationName;
          If (PName='') then
            PName:=CommandLine;
          end;

        if not FileExists(PName) then begin
          FoundName := ExeSearch(Pname,fpgetenv('PATH'));
          if FoundName<>'' then
            PName:=FoundName
          else
            raise EProcess.CreateFmt(SErrNoSuchProgram,[PName]);
        end;

{$if (defined(DARWIN) or defined(SUNOS))}
        { can't use vfork in case the child has to be
          suspended immediately, because with vfork the
          child borrows the execution thread of the parent
          unit it either exits or execs -> potential 
          deadlock depending on how quickly the SIGSTOP
          signal is delivered

          We also can't use vfork in case we have to change the working
          directory, use pipes or not use a console since calling anything but
          exec* or _exit after vfork is unsupported. For the same reason, also
          don't use vfork in case there is a forkevent (since we don't know
          what that one will call) }
        if (([poRunSuspended,PoUsePipes,poNoConsole] * Options) = []) and
           (FCurrentDirectory='') and
           not assigned(FForkEvent) then
          Pid:=fpvfork
        else
          Pid:=fpfork;
{$else}
        Pid:=fpfork;
{$endif}
        if Pid<0 then
          Raise EProcess.Create(SErrCannotFork);
        if (PID>0) then
          begin
            // Parent process. Copy process information.
            FProcessHandle:=PID;
            FThreadHandle:=PID;
            FProcessId:=PID;
            //FThreadId:=PID;
          end
        else
          begin
            { We're in the child }
            if (FCurrentDirectory<>'') then
               begin
{$push}{$i-}
                 ChDir(FCurrentDirectory);
                 { exit if the requested working directory does not exist (can
                   use DirectoryExists, that would not be atomic; cannot change
                   before forking because that would also change the CWD of the
                   parent, which could influence other threads }
                 if ioresult<>0 then
                   fpexit(127);
{$pop}
               end;
            if FDescriptors[phtInput].ResolveProcessHandle<>StdInputHandle then
               begin
               if FDescriptors[phtInput].IOType=iotPipe then
                 FDescriptors[phtInput].CloseOurHandle;
               safefpdup2(FDescriptors[phtInput].ResolveProcessHandle,0);
               end;
            if FDescriptors[phtOutput].ResolveProcessHandle<>StdOutputHandle then
               begin
               if FDescriptors[phtOutput].IOType=iotPipe then
                 FDescriptors[phtOutput].CloseOurHandle;
               safefpdup2(FDescriptors[phtOutput].ResolveProcessHandle,1);
               if (poStdErrToOutPut in Options) then
                 safefpdup2(FDescriptors[phtOutput].ResolveProcessHandle,2);
               end;
            if not (poStdErrToOutPut in Options) and (FDescriptors[phtError].ResolveProcessHandle<>StdErrorHandle) then
               begin
               if FDescriptors[phtOutput].IOType=iotPipe then
                 FDescriptors[phtError].CloseOurHandle;
               safefpdup2(FDescriptors[phtError].ResolveProcessHandle,2);
               end;
            if Assigned(FForkEvent) then
              FForkEvent(Self);
            if (poRunSuspended in Options) then
              fpkill(fpgetpid,SIGSTOP);
            if FEnv<>Nil then
              fpexecve(PName,Argv,Fenv)
            else
              fpexecv(PName,argv);
            fpExit(127);
          end
      Finally
        FreePcharList(Argv);
      end;
    Finally
      If (FEnv<>Nil) then
        FreePCharList(FEnv);
    end;
  Finally
    // Writeln(system.StdErr,'fork closing our handles');
    FDescriptors[phtInput].CloseTheirHandle;
    FDescriptors[phtOutput].CloseTheirHandle;
    if not (poStderrToOutPut in Options) then
      FDescriptors[phtError].CloseTheirHandle;
  end;
  FRunning:=True;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in FProcessOptions) and
      not (poRunSuspended in FProcessOptions) then
    WaitOnExit;
end;

Function TProcess.WaitOnExit : Boolean;
begin
  if FRunning then
    fexitcode:=waitprocess(handle);
  Result:=(fexitcode>=0);
  FRunning:=False;
end;

{ maybe some unixes might need a simpler solution }
{$define USE_GETTIMEOFDAY}

Function TProcess.WaitOnExit(Timeout : DWord) : Boolean;
var
  res: cint;
{$ifdef USE_GETTIMEOFDAY}
  timeout_tv,t : timeval;
  tz : timezone;
{$endif USE_GETTIMEOFDAY}

begin
  Result:=false;
{$ifdef USE_GETTIMEOFDAY}
  fpGetTimeOfDay(@timeout_tv,@tz);
  inc(timeout_tv.tv_sec,Timeout div 1000);
  inc(timeout_tv.tv_usec,(Timeout mod 1000)*1000);
{$endif USE_GETTIMEOFDAY}
  res:=fpWaitPid(Handle,pcint(@FExitCode),WNOHANG);
  while res=0 do
    begin
{$ifdef USE_GETTIMEOFDAY}
      fpGetTimeOfDay(@t,@tz);
      if (t.tv_sec>timeout_tv.tv_sec) or
        ((t.tv_sec=timeout_tv.tv_sec) and (t.tv_usec>timeout_tv.tv_usec)) then
        exit;
{$else USE_GETTIMEOFDAY}
      if Timeout=0 then
        Exit;
{$endif USE_GETTIMEOFDAY}
      Sleep(1);
      dec(Timeout);
      res:=fpWaitPid(Handle,pcint(@FExitCode),WNOHANG);
    end;
  result:=res=Handle;
  If Not Result then
    FexitCode:=cardinal(-1) // was 0, better testable for abnormal exit.
  else
    FRunning:=False;
end;

Function TProcess.Suspend : Longint;

begin
  If fpkill(Handle,SIGSTOP)<>0 then
    Result:=-1
  else
    Result:=1;
end;


Function TProcess.Resume : LongInt;

begin
  If fpKill(Handle,SIGCONT)<>0 then
    Result:=-1
  else
    Result:=0;
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  if aExitCode<>0 then ; // silence compiler warning
  Result:=fpkill(Handle,SIGTERM)=0;
  If Result then
    begin
    // Give the process some time to handle it. Sleeping may also yield to the process.
    if SignalWaitTime>0 then
      Sleep(SignalWaitTime);
    // Not handled yet ?
    if Running then
      Result:=fpkill(Handle,SIGKILL)=0;
    end;
  { the fact that the signal has been sent does not
    mean that the process has already handled the
    signal -> wait instead of calling getexitstatus }
  if Result then
    WaitOnExit;
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);

begin
  FShowWindow:=Value;
end;

function TIODescriptor.SysPrepareCreatedHandleForProcess(aHandle: THandle): THandle;
begin
  Result:=aHandle;
end;


function TIODescriptor.SysCreateFileNameHandle(const aFileName: string): THandle;

const
  DefaultRights = 438; // 438 = 666 octal which is rw rw rw
  ModeNames : Array[Boolean] of String = ('Reading','Writing');

begin
  if (aFileName='') then
    Raise EProcess.Create('No filename provided');
  case ProcessHandleType of
    phtInput:  Result:=FileOpen(aFileName,fmOpenRead);
    phtOutput,
    phtError: if FileExists(aFileName) then
                // No locking for existing file. (e.g. /dev/null cannot be locked)
                Result:=FileOpen(aFileName,fmOpenWrite {$IF FPC_Fullversion>=30301} or fmShareNoLocking {$ENDIF})
              else
                Result:=FileCreate(aFileName,fmShareDenyNone,DefaultRights)
  end;
  if (Result=-1) then
    Raise EProcess.CreateFmt('Could not open file "%s" for %s',[aFileName,ModeNames[ProcessHandleType<>phtInput]]);
end;

function TIODescriptor.SysNullFileName: string;
begin
  result:='/dev/null';
end;

function TIODescriptor.SysIsTypeSupported(AValue: TIOType): Boolean;
begin
  Result:=True;
end;


{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2008 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
Uses
  WinApi.Windows;
{$ELSE}
Uses
  Windows;
{$ENDIF}

const
  SNoCommandLine        = 'Cannot execute empty command-line';
  SErrCannotExecute     = 'Failed to execute %s : %d';
{  SErrNoSuchProgram     = 'Executable not found: "%s"';
  SErrNoTerminalProgram = 'Could not detect X-Terminal program';
}

Const
  PriorityConstants : Array [TProcessPriority] of Cardinal =
                      (HIGH_PRIORITY_CLASS,IDLE_PRIORITY_CLASS,
                       NORMAL_PRIORITY_CLASS,REALTIME_PRIORITY_CLASS,
                       BELOW_NORMAL_PRIORITY_CLASS,ABOVE_NORMAL_PRIORITY_CLASS);

procedure TProcess.CloseProcessHandles;
begin
  if (FProcessHandle<>0) then
    CloseHandle(FProcessHandle);
  if (FThreadHandle<>0) then
    CloseHandle(FThreadHandle);
end;

Function TProcess.PeekExitStatus : Boolean;
begin
  Result:=GetExitCodeProcess(ProcessHandle,FExitCode) and (FExitCode<>Still_Active);
  // wait up to 10ms extra till process really done to get rest of input bug #39821
  if not Result Then
    WaitForSingleObject(FProcessHandle,10);
end;

Function GetStartupFlags (P : TProcess; AllDescriptorsDefault: Boolean): Cardinal;

begin
  if AllDescriptorsDefault then
    Result:= 0
  else
    Result:= Startf_UseStdHandles;
  if suoUseShowWindow in P.StartupOptions then
    Result:=Result or startf_USESHOWWINDOW;
  if suoUSESIZE in P.StartupOptions then
    Result:=Result or startf_usesize;
  if suoUsePosition in P.StartupOptions then
    Result:=Result or startf_USEPOSITION;
  if suoUSECOUNTCHARS in P.Startupoptions then
    Result:=Result or startf_usecountchars;
  if suoUsefIllAttribute in P.StartupOptions then
    Result:=Result or startf_USEFILLATTRIBUTE;
end;

Function GetCreationFlags(P : TProcess) : Cardinal;

begin
  Result:=CREATE_UNICODE_ENVIRONMENT;
  if poNoConsole in P.Options then
    Result:=Result or CREATE_NO_WINDOW;
  if poNewConsole in P.Options then
    Result:=Result or Create_new_console;
  if poNewProcessGroup in P.Options then
    Result:=Result or CREATE_NEW_PROCESS_GROUP;
  If poRunSuspended in P.Options Then
    Result:=Result or Create_Suspended;
  if poDebugProcess in P.Options Then
    Result:=Result or DEBUG_PROCESS;
  if poDebugOnlyThisProcess in P.Options Then
    Result:=Result or DEBUG_ONLY_THIS_PROCESS;
  if poDefaultErrorMode in P.Options Then
    Result:=Result or CREATE_DEFAULT_ERROR_MODE;
  if poDetached in P.Options Then
    Result:=Result or DETACHED_PROCESS;

  result:=result or PriorityConstants[P.FProcessPriority];
end;

function WStrAsUniquePWideChar(var s: UnicodeString): PWideChar;
begin
  UniqueString(s);
  if s<>'' then
    Result:=PWideChar(s)
  else
    Result:=nil;
end;

Function StringsToWChars(List : TProcessStrings): pointer;

var
  EnvBlock: UnicodeString;
  I: Integer;

begin
  EnvBlock := '';
  For I:=0 to List.Count-1 do
    EnvBlock := EnvBlock + List[i] + #0;
  EnvBlock := EnvBlock + #0;
  GetMem(Result, Length(EnvBlock)*2);
  CopyMemory(Result, @EnvBlock[1], Length(EnvBlock)*2);
end;

Procedure InitProcessAttributes(P : TProcess; Out PA : TSecurityAttributes);

begin
  FillChar(PA,SizeOf(PA),0);
  PA.nLength := SizeOf(PA);
end;

Procedure InitThreadAttributes(P : TProcess; Out TA : TSecurityAttributes);

begin
  FillChar(TA,SizeOf(TA),0);
  TA.nLength := SizeOf(TA);
end;

Procedure InitStartupInfo(P : TProcess; AllDescriptorsDefault: Boolean; Out SI : STARTUPINFOW);

Const
  SWC : Array [TShowWindowOptions] of Cardinal =
             (0,SW_HIDE,SW_Maximize,SW_Minimize,SW_Restore,SW_Show,
             SW_ShowDefault,SW_ShowMaximized,SW_ShowMinimized,
               SW_showMinNOActive,SW_ShowNA,SW_ShowNoActivate,SW_ShowNormal);

begin
  FillChar(SI,SizeOf(SI),0);
  SI.cb:=SizeOf(SI);
  SI.dwFlags:=GetStartupFlags(P, AllDescriptorsDefault);
  if P.FShowWindow<>swoNone then
   SI.dwFlags:=SI.dwFlags or Startf_UseShowWindow
  else
    SI.dwFlags:=SI.dwFlags and not Startf_UseShowWindow;
  SI.wShowWindow:=SWC[P.FShowWindow];
  if P.FillAttribute<>0 then
    begin
    SI.dwFlags:=SI.dwFlags or Startf_UseFillAttribute;
    SI.dwFillAttribute:=P.FillAttribute;
    end;
   SI.dwXCountChars:=P.WindowColumns;
   SI.dwYCountChars:=P.WindowRows;
   SI.dwYsize:=P.WindowHeight;
   SI.dwXsize:=P.WindowWidth;
   SI.dwy:=P.WindowTop;
   SI.dwX:=P.WindowLeft;
end;

Function MaybeQuoteIfNotQuoted(Const S : TProcessString) : TProcessString;

begin
  If (Pos(' ',S)<>0) and (pos('"',S)=0) then
    Result:='"'+S+'"'
  else
     Result:=S;
end;


Procedure TProcess.SysExecute;
Var
  i : Integer;
  WName,WDir,WCommandLine : UnicodeString;
  PWName,PWDir,PWCommandLine : PWideChar;
  FEnv: pointer;
  FCreationFlags : Cardinal;
  FProcessAttributes : TSecurityAttributes;
  FThreadAttributes : TSecurityAttributes;
  FProcessInformation : TProcessInformation;
  FStartupInfo : STARTUPINFOW;
  Cmd : TProcessString;
  AllDescriptorsDefault: Boolean;

 begin
  fProcessInformation:=default(TProcessInformation);
  AllDescriptorsDefault :=
    (FDescriptors[phtInput].IOType = iotDefault) and
    (FDescriptors[phtOutput].IOType = iotDefault) and
    (FDescriptors[phtError].IOType = iotDefault) and 
    not (poStdErrToOutput in Options);
  FDescriptors[phtInput].PrepareHandles;
  FDescriptors[phtOutput].PrepareHandles;
  if not (poStdErrToOutput in Options) then
    FDescriptors[phtError].PrepareHandles;
  WName:='';
  WCommandLine:='';
  WDir:='';
  if (FApplicationName='') and (FCommandLine='') and (FExecutable='') then
    Raise EProcess.Create(SNoCommandline);
  if (FApplicationName<>'') then
    begin
    WName:=FApplicationName;
    WCommandLine:=FCommandLine;
    end
  else If (FCommandLine<>'') then
    WCommandLine:=FCommandLine
  else if (FExecutable<>'') then
    begin
    Cmd:=MaybeQuoteIfNotQuoted(Executable);
    For I:=0 to Parameters.Count-1 do
      Cmd:=Cmd+' '+MaybeQuoteIfNotQuoted(Parameters[i]);
    WCommandLine:=Cmd;
    end;
  If FCurrentDirectory<>'' then
    WDir:=FCurrentDirectory;
  if FEnvironment.Count<>0 then
    FEnv:=StringsToWChars(FEnvironment)
  else
    FEnv:=Nil;
  Try
    FCreationFlags:=GetCreationFlags(Self);
    InitProcessAttributes(Self,FProcessAttributes);
    InitThreadAttributes(Self,FThreadAttributes);
    InitStartupInfo(Self,AllDescriptorsDefault,FStartUpInfo);
    if not AllDescriptorsDefault then
      begin
      FStartupInfo.hStdInput:=FDescriptors[phtInput].ResolveProcessHandle;
      FStartupInfo.hStdOutput:=FDescriptors[phtOutput].ResolveProcessHandle;
      if Not(poStdErrToOutPut in Options) then
        FStartupInfo.hStdError:=FDescriptors[phtError].ResolveProcessHandle
      else
        FStartupInfo.hStdError:=FStartupInfo.hStdOutput;
      end;
    Try
      // Beware: CreateProcess can alter the strings
      // Beware: nil is not the same as a pointer to a #0
      PWName:=WStrAsUniquePWideChar(WName);
      PWCommandLine:=WStrAsUniquePWideChar(WCommandLine);
      PWDir:=WStrAsUniquePWideChar(WDir);
      If Not CreateProcessW (PWName,PWCommandLine,@FProcessAttributes,@FThreadAttributes,
                   FInheritHandles,FCreationFlags,FEnv,PWDir,FStartupInfo,
                   fProcessInformation) then
        Raise EProcess.CreateFmt(SErrCannotExecute,[FCommandLine,GetLastError]);
      FProcessHandle:=FProcessInformation.hProcess;
      FThreadHandle:=FProcessInformation.hThread;
      FThreadId:=FProcessInformation.dwThreadId;  
      FProcessID:=FProcessINformation.dwProcessID;
    Finally
      FDescriptors[phtInput].CloseTheirHandle;
      FDescriptors[phtOutput].CloseTheirHandle;
      if Not(poStdErrToOutPut in Options) then
        FDescriptors[phtError].CloseTheirHandle;
    end;
    FRunning:=True;
  Finally
    If FEnv<>Nil then
      FreeMem(FEnv);
  end;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in Options) and
      not (poRunSuspended in Options) then
    WaitOnExit;
end;

Function TProcess.WaitOnExit : Boolean;
Var
  R : DWord;
begin
  R:=WaitForSingleObject (FProcessHandle,Infinite);
  Result:=(R<>Wait_Failed);
  If Result then
    GetExitStatus;
  FRunning:=False;
end;

Function TProcess.WaitOnExit(Timeout : DWord) : Boolean;
Var
  R : DWord;
begin
  R:=WaitForSingleObject (FProcessHandle,Timeout);
  Result:=R=0;
  If Result then
    begin
      GetExitStatus;
      FRunning:=False;
    end;
end;

Function TProcess.Suspend : Longint;

begin
  Result:=SuspendThread(ThreadHandle);
end;

Function TProcess.Resume : LongInt;

begin
  Result:=ResumeThread(ThreadHandle);
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
  If ExitStatus=Still_active then
    Result:=TerminateProcess(Handle,AexitCode);
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);

begin
  FShowWindow:=Value;
end;

Function TIODescriptor.SysCreateFileNameHandle(const aFileName: string) : THandle;

const
  ModeNames : Array[Boolean] of String = ('Reading','Writing');

var
  Sec: SECURITY_ATTRIBUTES;

begin
  if (aFileName='') then
    Raise EProcess.Create('No filename set');
  Sec:=Default(SECURITY_ATTRIBUTES);
  sec.nLength := SizeOf(Sec);
  sec.bInheritHandle := True;
  case ProcessHandleType of
    phtInput:  Result:=CreateFileW(PWideChar(WideString(aFileName)), GENERIC_READ,
      FILE_SHARE_READ, @sec, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    phtOutput,
    phtError:
      begin
        Result:=CreateFileW(PWideChar(WideString(aFileName)), GENERIC_WRITE,
          FILE_SHARE_READ, @sec, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
        if not(Result=INVALID_HANDLE_VALUE) then
          FileSeek(Result, 0, 2);
      end;
  end;
  if (Result=INVALID_HANDLE_VALUE) then
    Raise EProcess.CreateFmt('Could not open file "%s" for %s',[aFileName,ModeNames[ProcessHandleType<>phtInput]]);
end;



function TIODescriptor.SysPrepareCreatedHandleForProcess(aHandle: THandle): THandle;

var
  oldHandle: THandle;
  Res : Boolean;
  
begin
  if (IOType in [iotDefault,iotFile]) or ((IOType=iotHandle) and FCustomHandleIsInheritable) then
    begin
    Result:=aHandle;
    exit;
    end;
  oldHandle := ahandle;
  ahandle:=THandle(INVALID_HANDLE_VALUE); 
  Res := DuplicateHandle
  ( GetCurrentProcess(),
    oldHandle,
    GetCurrentProcess(),
    @aHandle,
    0,
    true,
    DUPLICATE_SAME_ACCESS
  );
  if Res then begin
    // Either AutoCloseCustomHandle or set in OnGetHandle
    if (IOType=iotHandle) and not FCloseHandleOnExecute then
      FCloseHandleOnExecute:=True // the original CustomHandle is kept open / Set True for the duplicate handle
    else
      Res:=CloseHandle(oldHandle);
  end;
  if not Res then
    begin
    FileClose(aHandle);
    Raise EProcess.CreateFmt('Could not make handle %d inheritable',[aHandle]);
    end;
  Result:=aHandle;
end;    

function TIODescriptor.SysNullFileName: string;
begin
  result:='NULL';
end;

function TIODescriptor.SysIsTypeSupported(AValue: TIOType): Boolean;
begin
  Result:=True;
end;

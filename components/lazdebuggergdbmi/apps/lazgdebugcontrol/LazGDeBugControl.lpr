program LazGDeBugControl;
{off $DEFINE OLD_DebugBreak}

uses sysutils, windows;

var
  s: string;
  {$IFDEF OLD_DebugBreak}
  DebugBreakAddr: Pointer = nil;
  _CreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall = nil;
  PauseRequestInThreadID: DWORD;
  hThread: HANDLE;
  {$ELSE}
  DebugBreakProcess: function(hProcess: THandle): WINBOOL; stdcall = nil;
  {$ENDIF}
  pid: LongInt;
  hMod: HMODULE;
  hProcess: HANDLE;

begin
  hProcess := 0;
  try
    hMod := GetModuleHandle(kernel32);
    if hMod = 0 then
      Exit;

    {$IFDEF OLD_DebugBreak}
    DebugBreakAddr := GetProcAddress(hMod, 'DebugBreak');
    Pointer(_CreateRemoteThread) := GetProcAddress(hMod, 'CreateRemoteThread');
    if (DebugBreakAddr = nil) or (_CreateRemoteThread = nil) then
      exit;
    {$ELSE}
    Pointer(DebugBreakProcess) := GetProcAddress(hMod, 'DebugBreakProcess');
    if (DebugBreakProcess = nil) then
      exit;
    {$ENDIF}

    writeln('Ready');
    while true do begin
      readln(s);
      if s = 'exit' then
        exit;

      pid := StrToInt(s);
      if pid = 0 then
        exit;

      hProcess := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, pid);
      if hProcess = 0 then
        exit;

      {$IFDEF OLD_DebugBreak}
      hThread := _CreateRemoteThread(hProcess, nil, 0, DebugBreakAddr, nil, 0, PauseRequestInThreadID);
      if hThread = 0
      then begin
        writeln('Error: ', GetLastError);
        Exit;
      end;
      writeln('OK');
      CloseHandle(hThread);
      {$ELSE}
      if DebugBreakProcess(hProcess) then
        writeln('OK')
      else
      begin
        writeln('Error: ', GetLastError);
        exit;
      end;
      {$ENDIF}

      CloseHandle(hProcess);
      hProcess := 0;

    end;
  finally
    writeln('Bye');
    if hProcess <> 0 then
      CloseHandle(hProcess);
  end;
end.


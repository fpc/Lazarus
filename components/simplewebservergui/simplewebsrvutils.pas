{
  Author: Mattias Gaertner
}
unit SimpleWebSrvUtils;

{$mode objfpc}{$H+}

{$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}

interface

uses
  {$IFDEF MSWindows}
  Windows,
  {$ENDIF}
  {$IFDEF Unix}
  BaseUnix, Errors,
  {$ENDIF}
  Classes, SysUtils, Sockets, Process,
  LazLoggerBase, FileUtil,
  Dialogs,
  IDEDialogs;

type

  { TSimpleWebServerUtility }

  TSimpleWebServerUtility = class
  private
    FViewCaption: string;
    {$IFDEF Darwin}
    function FindProcessListeningOnPortMac(const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer): boolean;
    function KillProcessMac(aPID: integer): boolean;
    function FindFreePortMac(aStartPort: word): word;
    {$ENDIF}
    {$IFDEF Linux}
    function FindProcessListeningOnPortLinux(const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer): boolean;
    function KillProcessLinux(aPID: integer): boolean;
    function FindFreePortLinux(aStartPort: word): word;
    {$ENDIF}
    {$IFDEF MSWindows}
    function FindProcessListeningOnPortWin(const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer): boolean;
    function KillProcessWin(aPID: integer): boolean;
    function FindFreePortWin(aStartPort: word): word;
    {$ENDIF}
  public
    function FindProcessListeningOnPort(const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer): boolean;
    function KillProcess(aPID: integer): boolean;
    function FindFreePort(aStartPort: word; Interactive: boolean): word;
    function SameInAddr(const A,B: in_addr): boolean;
    property ViewCaption: string read FViewCaption write FViewCaption;
  end;

{$IFDEF MSWindows}
{$linklib iphlpapi}
{$linklib psapi}
const
  ANY_SIZE = 1;
type
  PMIB_TCPROW2 = ^MIB_TCPROW2;
  MIB_TCPROW2 = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: DWORD;
    dwTCPOffloadState: DWORD;
  end;

  PMIB_TCPTABLE2 = ^MIB_TCPTABLE2;
  MIB_TCPTABLE2 = record
    dwNumEntries: DWORD;
    table: array [0..ANY_SIZE - 1] of MIB_TCPROW2;
  end;
function GetTcpTable2(pTcpTable: PMIB_TCPTABLE2; var pdwSize: DWORD; bOrder: BOOL): DWORD; stdcall; external name 'GetTcpTable2';
function GetModuleFilenameExW(hndProcess: HANDLE; hndModule: HMODULE; lpFilename: LPWSTR; nSize: DWord): DWord; stdcall; external name 'GetModuleFileNameExW';
{$ENDIF}

function MaybeQuote(S : String) : String;
function ReadNext(const Line: string; var p: integer): string;
function GetNextIPPort(Port: word): word;

implementation

function ReadNext(const Line: string; var p: integer): string;
var
  l: SizeInt;
  StartP: Integer;
begin
  Result:='';
  l:=length(Line);
  if p>l then exit;
  while (p<=l) and (Line[p] in [' ',#9]) do inc(p);
  StartP:=p;
  while (p<=l) and not (Line[p] in [' ',#9]) do inc(p);
  Result:=copy(Line,StartP,p-StartP);
end;

function GetNextIPPort(Port: word): word;
begin
  if Port=high(word) then
    Result:=1024
  else
    Result:=Port+1;
end;

function MaybeQuote(S: String): String;
begin
  if Pos(' ',S)=0 then
    Result:=S
  else
    Result:='"'+S+'"';
end;

function TSimpleWebServerUtility.FindProcessListeningOnPort(
  const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer
  ): boolean;
begin
  Result:=false;
  aDesc:='';
  aPID:=0;

  try
    {$IFDEF Darwin}
    Result:=FindProcessListeningOnPortMac(IPAddr,aPort,aDesc,aPID);
    {$ENDIF}
    {$IFDEF Linux}
    Result:=FindProcessListeningOnPortLinux(IPAddr,aPort,aDesc,aPID);
    {$ENDIF}
    {$IFDEF MSWindows}
    Result:=FindProcessListeningOnPortWin(IPAddr,aPort,aDesc,aPID);
    {$ENDIF}
  except
    on E: Exception do begin
      IDEMessageDialog('Error',
        ViewCaption+':'+sLineBreak
        +'Error checking which process uses TCP port '+IntToStr(aPort)+sLineBreak
        +E.Message,mtError,[mbOK]);
    end;
  end;
end;

function TSimpleWebServerUtility.KillProcess(aPID: integer): boolean;
begin
  Result:=false;
  try
    {$IFDEF Darwin}
    Result:=KillProcessMac(aPID);
    {$ENDIF}
    {$IFDEF Linux}
    Result:=KillProcessLinux(aPID);
    {$ENDIF}
    {$IFDEF MSWindows}
    Result:=KillProcessWin(aPID);
    {$ENDIF}
  except
    on E: Exception do begin
      IDEMessageDialog('Error',
        ViewCaption+':'+sLineBreak
        +'Error killing process '+IntToStr(aPID)+sLineBreak
        +E.Message,mtError,[mbOK]);
    end;
  end;
end;

function TSimpleWebServerUtility.FindFreePort(aStartPort: word;
  Interactive: boolean): word;
begin
  Result:=0;
  try
    {$IFDEF Darwin}
    Result:=FindFreePortMac(aStartPort);
    {$ENDIF}
    {$IFDEF Linux}
    Result:=FindFreePortLinux(aStartPort);
    {$ENDIF}
    {$IFDEF MSWindows}
    Result:=FindFreePortWin(aStartPort);
    {$ENDIF}
  except
    on E: Exception do begin
      if not Interactive then
        exit(0);
      IDEMessageDialog('Error',
        ViewCaption+':'+sLineBreak
        +'Error finding free TCP port:'+sLineBreak
        +E.Message,mtError,[mbOK]);
    end;
  end;
end;

function TSimpleWebServerUtility.SameInAddr(const A, B: in_addr): boolean;
begin
  if (A.s_addr=0) or (B.s_addr=0) then
    Result:=true
  else
    Result:=A.s_addr=B.s_addr;
end;

{$IFDEF Darwin}
function ParseLsofAddr(s: string; out IPAddr: in_Addr; out aPort: word): boolean;
// e.g. 192.168.16.193:64174->35.82.112.36:443 (ESTABLISHED)
// 127.0.0.1:7777 (LISTEN)
// *:7000 (LISTEN)
var
  p, StartP: SizeInt;
  CurAddr: String;
  i: LongInt;
begin
  Result:=false;
  IPAddr.s_addr:=0;
  aPort:=0;

  p:=Pos(':',s);
  if p<1 then exit;
  CurAddr:=LeftStr(s,p-1);
  if CurAddr='*' then
    IPAddr.s_addr:=0
  else
    IPAddr:=StrToHostAddr(CurAddr);
  inc(p);
  if p>length(s) then exit;
  if s[p]='*' then
    exit(true); // ip:*
  StartP:=p;
  while (p<=length(s)) and (s[p] in ['0'..'9']) do inc(p);
  if p-StartP>5 then exit;
  i:=StrToIntDef(copy(s,StartP,p-StartP),0);
  if (i<=0) or (i>65535) then exit;
  aPort:=i;
  Result:=true;
end;

function TSimpleWebServerUtility.FindProcessListeningOnPortMac(
  const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer
  ): boolean;
const
  lsofparams = '-nPi4:';
var
  ExePath, Params, OutStr, s, PIDStr, CurLocalAddr, Line, CurUserName,
    CurArgs: String;
  sl: TStringList;
  i, p: Integer;
  CurPort: word;
  CurIPAddr: in_addr;
  UserPos, ArgsPos: SizeInt;
begin
  Result:=false;
  aDesc:='';
  aPID:=0;
  if aPort=0 then exit;

  // query lsof to find the PID listening on IPv4 tcp/udp port
  ExePath:=FindDefaultExecutablePath('lsof');
  if ExePath='' then
  begin
    debugln(['Hint: [20220114175144] FindProcessListeningOnPortMac "lsof" not found in PATH']);
    exit;
  end;
  Params:=lsofparams+IntToStr(aPort);
  if not RunCommand(ExePath,[Params],OutStr,[]) then
  begin
    debugln(['Hint: [20220114175148] FindProcessListeningOnPortMac could not run "'+ExePath+' '+Params+'"']);
    exit;
  end;

  sl:=TStringList.Create;
  try
    sl.Text:=OutStr;
    for i:=0 to sl.Count-1 do
    begin
      Line:=sl[i];
      // COMMAND     PID    USER   FD   TYPE             DEVICE SIZE/OFF NODE NAME
      p:=1;
      s:=ReadNext(Line,p); // command
      if s='' then continue;
      PIDStr:=ReadNext(Line,p); // PID
      if PIDStr='' then continue;
      s:=ReadNext(Line,p); // USER
      if s='' then continue;
      s:=ReadNext(Line,p); // FD
      if s='' then continue;
      s:=ReadNext(Line,p); // TYPE
      if s='' then continue;
      s:=ReadNext(Line,p); // DEVICE
      if s='' then continue;
      s:=ReadNext(Line,p); // SIZE
      if s='' then continue;
      s:=ReadNext(Line,p); // NODE
      if s='' then continue;
      CurLocalAddr:=ReadNext(Line,p); // NAME, e.g. 192.168.16.193:64174->35.82.112.36:443 (ESTABLISHED) or 127.0.0.1:7777 (LISTEN) or *:7000 (LISTEN)
      if CurLocalAddr='' then continue;
      if not ParseLsofAddr(CurLocalAddr,CurIPAddr,CurPort) then continue;
      if not SameInAddr(CurIPAddr,IPAddr) then continue;

      aPID:=StrToIntDef(PIDStr,0);
      if aPID>0 then
      begin
        debugln(['Hint: [20220117195255] FindProcessListeningOnPortMac ',CurLocalAddr,':',CurPort,' ',aPID]);
        Result:=true;
      end;
      break;
    end;
    if aPID=0 then exit;
  finally
    sl.Free;
  end;

  // query ps for command line of PID
  ExePath:=FindDefaultExecutablePath('ps');
  if ExePath='' then
  begin
    debugln(['Hint: [20220114182717] FindProcessListeningOnPortMac "ps" not found in PATH']);
    exit;
  end;
  if not RunCommand(ExePath,['-p',IntToStr(aPID),'-o','pid,user,args'],OutStr,[]) then
  begin
    debugln(['Hint: [20220114182751] FindProcessListeningOnPortMac could not run "'+ExePath+' -p '+IntToStr(aPID)+' -o pid,user,args"']);
    exit;
  end;

  //debugln(['FindProcessListeningOnPortLinux ps OutStr={',OutStr,'}']);
  sl:=TStringList.Create;
  try
    sl.Text:=OutStr;
    if sl.Count<2 then
    begin
      debugln(['Hint: [20220114182832] FindProcessListeningOnPortLinux ps returned no info']);
      exit;
    end;
    Line:=sl[0];
    UserPos:=Pos('USER',Line);
    if UserPos<1 then
    begin
      debugln(['Hint: [20220114182857] FindProcessListeningOnPortLinux ps returned no USER']);
      exit;
    end;
    ArgsPos:=Pos('ARGS',Line);
    if ArgsPos<1 then
    begin
      debugln(['Hint: [20220114182911] FindProcessListeningOnPortLinux ps returned no ARGS']);
      exit;
    end;
    Line:=sl[1];
    CurUserName:=Trim(copy(Line,UserPos,ArgsPos-UserPos));
    CurArgs:=Trim(copy(Line,ArgsPos,length(Line)));
    aDesc:='User: '+CurUserName+sLineBreak
      +'Args: '+CurArgs;
  finally
    sl.Free;
  end;
end;

function TSimpleWebServerUtility.KillProcessMac(aPID: integer): boolean;
var
  e: LongInt;
begin
  if FpKill(aPID,SIGTERM)=0 then exit(true);

  e:=fpgeterrno;
  IDEMessageDialog('Error',
    ViewCaption+':'+sLineBreak
    +'Unable to kill pid '+IntToStr(aPID)+sLineBreak
    +sys_errlist[e],
    mtError,[mbOk]);
  Result:=false;
end;

function TSimpleWebServerUtility.FindFreePortMac(aStartPort: word): word;
const
  lsofparams = '-nPi4';
var
  ExePath, Params, OutStr, s, CurLocalAddr, Line: String;
  sl: TStringList;
  i, p: Integer;
  CurPort: word;
  CurIPAddr: in_addr;
  Ports: array of word;
  l: SizeInt;
begin
  Result:=aStartPort;
  if aStartPort=0 then exit;

  // query lsof to find the IPv4 tcp/udp ports
  ExePath:=FindDefaultExecutablePath('lsof');
  if ExePath='' then
  begin
    debugln(['Hint: [20220114183430] FindFreePortMac "lsof" not found in PATH']);
    exit;
  end;
  Params:=lsofparams;
  if not RunCommand(ExePath,[Params],OutStr,[]) then
  begin
    debugln(['Hint: [20220114183449] FindFreePortMac could not run "'+ExePath+' '+Params+'"']);
    exit;
  end;

  Ports:=[];
  sl:=TStringList.Create;
  try
    sl.Text:=OutStr;
    for i:=0 to sl.Count-1 do
    begin
      Line:=sl[i];
      // COMMAND     PID    USER   FD   TYPE             DEVICE SIZE/OFF NODE NAME
      p:=1;
      s:=ReadNext(Line,p); // command
      if s='' then continue;
      s:=ReadNext(Line,p); // PID
      if s='' then continue;
      s:=ReadNext(Line,p); // USER
      if s='' then continue;
      s:=ReadNext(Line,p); // FD
      if s='' then continue;
      s:=ReadNext(Line,p); // TYPE
      if s='' then continue;
      s:=ReadNext(Line,p); // DEVICE
      if s='' then continue;
      s:=ReadNext(Line,p); // SIZE
      if s='' then continue;
      s:=ReadNext(Line,p); // NODE
      if s='' then continue;
      CurLocalAddr:=ReadNext(Line,p); // NAME, e.g. 192.168.16.193:64174->35.82.112.36:443 (ESTABLISHED) or 127.0.0.1:7777 (LISTEN) or *:7000 (LISTEN)
      if CurLocalAddr='' then continue;
      if not ParseLsofAddr(CurLocalAddr,CurIPAddr,CurPort) then continue;

      l:=length(Ports);
      SetLength(Ports,l+1);
      Ports[l]:=CurPort;
    end;
  finally
    sl.Free;
  end;

  l:=length(Ports);
  Result:=aStartPort;
  repeat
    i:=l-1;
    while (i>=0) and (Ports[i]<>Result) do dec(i);
    if i<0 then exit;
    Result:=GetNextIPPort(Result);
  until Result=aStartPort;
  Result:=0;
end;

{$ENDIF} // Darwin

{$IFDEF Linux}
function TSimpleWebServerUtility.FindProcessListeningOnPortLinux(
  const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer
  ): boolean;
const NetstatParams = '-nlptu4';
var
  ExePath, OutStr, Line, PIDPrg, CurLocalAddr, CurUserName, CurArgs: String;
  sl: TStringList;
  i: Integer;
  LocalAddrPos, ForeignAddrPos, PIDPos, p, UserPos, ArgsPos: SizeInt;
  CurPort: LongInt;
  LocalInAddr: in_addr;
begin
  Result:=false;
  aDesc:='';
  aPID:=0;
  if aPort=0 then exit;

  // query netstat to find the PID listening on IPv4 tcp/udp port
  ExePath:=FindDefaultExecutablePath('netstat');
  if ExePath='' then
  begin
    DebugLn(['Hint: [20220108222805] FindProcessListeningOnPortLinux "netstat" not found in PATH']);
    exit;
  end;
  if not RunCommand(ExePath,[NetstatParams],OutStr,[]) then
  begin
    debugln(['Hint: [20220108222923] FindProcessListeningOnPortLinux could not run "'+ExePath+' '+NetstatParams+'"']);
    exit;
  end;

  sl:=TStringList.Create;
  try
    sl.Text:=OutStr;
    for i:=0 to sl.Count-1 do
    begin
      Line:=sl[i];
      if (LeftStr(Line,5)='Proto') then
      begin
        LocalAddrPos:=Pos('Local Address',Line);
        ForeignAddrPos:=Pos('Foreign Address',Line);
        PIDPos:=Pos('PID/Program name',Line);
      end else if (LeftStr(Line,4)='tcp ') or (LeftStr(Line,4)='udp ') then
      begin
        CurLocalAddr:=Trim(copy(Line,LocalAddrPos,ForeignAddrPos-LocalAddrPos));
        p:=Pos(':',CurLocalAddr);
        if p<1 then continue;
        CurPort:=StrToIntDef(copy(CurLocalAddr,p+1,length(CurLocalAddr)),0);
        if CurPort<>aPort then continue;

        LocalInAddr:=StrToHostAddr(CurLocalAddr);
        if not SameInAddr(LocalInAddr,IPAddr) then continue;

        PIDPrg:=Trim(copy(Line,PIDPos,length(Line)));
        p:=Pos('/',PIDPrg);
        if p<1 then continue;
        aPID:=StrToIntDef(LeftStr(PIDPrg,p-1),0);
        if aPID>0 then
        begin
          aDesc:=copy(PIDPrg,p+1,length(PIDPrg));
          Result:=true;
        end;
        break;
      end;
    end;
    if aPID=0 then exit;
  finally
    sl.Free;
  end;

  // query ps for command line of PID
  ExePath:=FindDefaultExecutablePath('ps');
  if ExePath='' then
  begin
    debugln(['Hint: [20220108230143] FindProcessListeningOnPortLinux "ps" not found in PATH']);
    exit;
  end;
  if not RunCommand(ExePath,['-q',IntToStr(aPID),'-eo','pid,euser,args'],OutStr,[]) then
  begin
    debugln(['Hint: [20220108230145] FindProcessListeningOnPortLinux could not run "'+ExePath+' -q '+IntToStr(aPID)+' -eo pid,euser,args"']);
    exit;
  end;

  //debugln(['FindProcessListeningOnPortLinux ps OutStr={',OutStr,'}']);
  sl:=TStringList.Create;
  try
    sl.Text:=OutStr;
    if sl.Count<2 then
    begin
      debugln(['Hint: [20220108230953] FindProcessListeningOnPortLinux ps returned no info']);
      exit;
    end;
    Line:=sl[0];
    UserPos:=Pos('EUSER',Line);
    if UserPos<1 then
    begin
      debugln(['Hint: [20220108231209] FindProcessListeningOnPortLinux ps returned no euser']);
      exit;
    end;
    ArgsPos:=Pos('COMMAND',Line);
    if ArgsPos<1 then
    begin
      debugln(['Hint: [20220108231236] FindProcessListeningOnPortLinux ps returned no args']);
      exit;
    end;
    Line:=sl[1];
    CurUserName:=Trim(copy(Line,UserPos,ArgsPos-UserPos));
    CurArgs:=Trim(copy(Line,ArgsPos,length(Line)));
    aDesc:='User: '+CurUserName+sLineBreak
      +'Args: '+CurArgs;
  finally
    sl.Free;
  end;
end;

function TSimpleWebServerUtility.KillProcessLinux(aPID: integer): boolean;
var
  e: LongInt;
begin
  if FpKill(aPID,SIGTERM)=0 then exit(true);

  e:=fpgeterrno;
  IDEMessageDialog('Error',
    ViewCaption+':'+sLineBreak
    +'Unable to kill pid '+IntToStr(aPID)+sLineBreak
    +sys_errlist[e],
    mtError,[mbOk]);
  Result:=false;
end;

function TSimpleWebServerUtility.FindFreePortLinux(aStartPort: word): word;
const NetstatParams = '-nlptu4';
var
  ExePath, OutStr, Line, CurLocalAddr: String;
  sl: TStringList;
  i: Integer;
  LocalAddrPos, ForeignAddrPos, p, l: SizeInt;
  CurPort: LongInt;
  Ports: array of word;
begin
  Result:=0;
  // query netstat to find the IPv4 tcp/udp ports
  ExePath:=FindDefaultExecutablePath('netstat');
  if ExePath='' then
  begin
    debugln(['Hint: [20220110163919] FindFreePortLinux "netstat" not found in PATH']);
    IDEMessageDialog('Error',
       ViewCaption+':'+sLineBreak
       +'Unable to find netstat utility',mtError,[mbOk]);
    exit;
  end;
  if not RunCommand(ExePath,[NetstatParams],OutStr,[]) then
  begin
    debugln(['Hint: [20220110163930] FindFreePortLinux could not run "'+ExePath+' '+NetstatParams+'"']);
    IDEMessageDialog('Error',
       ViewCaption+':'+sLineBreak
       +'Could not run "'+ExePath+' '+NetstatParams+'"',mtError,[mbOk]);
    exit;
  end;

  Ports:=[];
  sl:=TStringList.Create;
  try
    sl.Text:=OutStr;
    for i:=0 to sl.Count-1 do
    begin
      Line:=sl[i];
      if (LeftStr(Line,5)='Proto') then
      begin
        LocalAddrPos:=Pos('Local Address',Line);
        ForeignAddrPos:=Pos('Foreign Address',Line);
      end else if (LeftStr(Line,4)='tcp ') or (LeftStr(Line,4)='udp ') then
      begin
        CurLocalAddr:=Trim(copy(Line,LocalAddrPos,ForeignAddrPos-LocalAddrPos));
        p:=Pos(':',CurLocalAddr);
        if p<1 then continue;
        CurPort:=StrToIntDef(copy(CurLocalAddr,p+1,length(CurLocalAddr)),0);
        if CurPort=0 then continue;

        l:=length(Ports);
        SetLength(Ports,l+1);
        Ports[l]:=CurPort;
      end;
    end;
  finally
    sl.Free;
  end;

  l:=length(Ports);
  Result:=aStartPort;
  repeat
    i:=l-1;
    while (i>=0) and (Ports[i]<>Result) do dec(i);
    if i<0 then exit;
    Result:=GetNextIPPort(Result);
  until Result=aStartPort;
  Result:=0;
end;

{$ENDIF} // Linux

{$IFDEF MSWindows}
function TSimpleWebServerUtility.FindProcessListeningOnPortWin(
  const IPAddr: in_addr; aPort: word; out aDesc: string; out aPID: integer
  ): boolean;
var
  pTCPTable: PMIB_TCPTABLE2;
  LocalPort: word;
  aSize, r: dword;
  LocalAddr: in_addr;
  i: Integer;
  h: HANDLE;
  CurExeName: array[0..MAX_PATH] of WideChar;
begin
  Result:=false;
  aDesc:='';
  aPID:=0;
  if aPort=0 then exit;

  aSize:=SizeOf(MIB_TCPTABLE2);
  pTCPTable:=GetMem(aSize);
  if pTCPTable=nil then exit;
  try
    r:=GetTcpTable2(pTCPTable,aSize,true);
    if r=ERROR_INSUFFICIENT_BUFFER then
    begin
      ReAllocMem(pTCPTable,aSize);
      if pTCPTable=nil then exit;
    end;

    r:=GetTcpTable2(pTCPTable,aSize,true);
    if r<>NO_ERROR then exit;

    {$R-}
    for i:=0 to pTCPTable^.dwNumEntries-1 do
    begin
      LocalPort:=NToHs(word(pTCPTable^.table[i].dwLocalPort));
      if LocalPort<>aPort then continue;
      LocalAddr.s_addr:=NToHl(pTCPTable^.table[i].dwLocalAddr);
      if not SameInAddr(LocalAddr,IPAddr) then continue;

      aPid:=pTCPTable^.table[i].dwOwningPid;
      if aPid=0 then continue;
      Result:=true;
      break;
    end;
    {$IFDEF RangeChecking}{$R+}{$ENDIF}
  finally
    if pTCPTable<>nil then
      Freemem(pTCPTable);
  end;
  if not Result then exit;

  h:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, WINBOOL(false), aPid);
  if h=0 then exit;
  try
    aSize:=GetModuleFilenameExW(h,0,@CurExeName[0],length(CurExeName));
    if aSize>0 then
    begin
      aDesc:=CurExeName;
    end;
  finally
    CloseHandle(h);
  end;
end;

function TSimpleWebServerUtility.KillProcessWin(aPID: integer): boolean;
var
  h: HANDLE;
begin
  if aPid<=0 then exit(false);
  h:=OpenProcess(PROCESS_TERMINATE, WINBOOL(false), aPid);
  if h=0 then exit;
  try
    Result:=TerminateProcess(h,0);
  finally
    CloseHandle(h);
  end;
  if not Result then
  begin
    IDEMessageDialog('Error',
      ViewCaption+':'+sLineBreak
      +'Unable to kill pid '+IntToStr(aPID),
      mtError,[mbOk]);
  end;
end;

function TSimpleWebServerUtility.FindFreePortWin(aStartPort: word): word;
var
  pTCPTable: PMIB_TCPTABLE2;
  aSize, r: DWord;
  i: Integer;
  LocalPort: Word;
  Ports: array of Word;
  l: SizeInt;
begin
  Result:=0;
  aSize:=SizeOf(MIB_TCPTABLE2);
  pTCPTable:=GetMem(aSize);
  if pTCPTable=nil then exit;
  try
    r:=GetTcpTable2(pTCPTable,aSize,true);
    if r=ERROR_INSUFFICIENT_BUFFER then
    begin
      ReAllocMem(pTCPTable,aSize);
      if pTCPTable=nil then exit;
    end;

    r:=GetTcpTable2(pTCPTable,aSize,true);
    if r<>NO_ERROR then exit;

    {$R-}
    Ports:=[];
    for i:=0 to pTCPTable^.dwNumEntries-1 do
    begin
      LocalPort:=NToHs(word(pTCPTable^.table[i].dwLocalPort));
      l:=length(Ports);
      SetLength(Ports,l+1);
      Ports[l]:=LocalPort;
    end;
    {$IFDEF RangeChecking}{$R+}{$ENDIF}
  finally
    if pTCPTable<>nil then
      Freemem(pTCPTable);
  end;

  l:=length(Ports);
  Result:=aStartPort;
  repeat
    i:=l-1;
    while (i>=0) and (Ports[i]<>Result) do dec(i);
    if i<0 then exit;
    Result:=GetNextIPPort(Result);
  until Result=aStartPort;
  Result:=0;
end;

{$ENDIF} // MSWindows

end.


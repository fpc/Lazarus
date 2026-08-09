unit LazDebuggerStarterUtils;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF LINUX}
  BaseUnix, Unix,
  {$ENDIF}
  LazFileUtils;

function FindPipeName(ASkipTmp: boolean = False): String;
function CreatePipe: String;
procedure RemovePipe(n: String);
function OpenReadPipe(n: String): LongInt;
function OpenWritePipe(n: String): LongInt;
function ReadPipe(FD: LongInt; ATimeOut: Integer): String;
function WritePipe(FD: LongInt; AText: String): integer;
procedure ClosePipe(FD: LongInt);

implementation

function FindPipeName(ASkipTmp: boolean): String;
var
  i: integer;
begin
  {$IFDEF LINUX}
  Result := '/tmp';
  if ASkipTmp or not DirectoryExistsUTF8(Result) then
    Result := GetCurrentDirUTF8;

  Result := AppendPathDelim(Result) + 'laz_fpdebug_starter_p_'+IntToStr(GetProcessID)+'_';

  if not FileExistsUTF8(Result) then
    exit;

  for i := 1 to 199 do
  if not FileExistsUTF8(Result+IntToStr(i)) then begin
    Result := Result+IntToStr(i);
    exit;
  end;

  {$ENDIF}
  Result := '';
end;

function CreatePipe: String;
var
  i: integer;
begin
  {$IFDEF LINUX}
  for i := 0 to 9 do begin
    Result := FindPipeName(i >= 7);
    if Fpmkfifo(Result, S_IRUSR or S_IWUSR) = 0 then
      exit;
  end;
  {$ENDIF}
  Result := '';
end;

procedure RemovePipe(n: String);
begin
  {$IFDEF LINUX}
  Fpunlink(n);
  {$ENDIF}
end;

function OpenReadPipe(n: String): LongInt;
begin
  {$IFDEF LINUX}
  Result := Fpopen(n, O_RDONLY or O_NONBLOCK);
  {$ELSE}
  Result := -1;
  {$ENDIF}
end;

function OpenWritePipe(n: String): LongInt;
begin
  {$IFDEF LINUX}
  Result := Fpopen(n, O_WRONLY or O_NONBLOCK);
  {$ELSE}
  Result := -1;
  {$ENDIF}
end;

function ReadPipe(FD: LongInt; ATimeOut: Integer): String;
const
  RLEN = 4096;
var
  {$IFDEF LINUX}
  pfd: pollfd;
  {$ENDIF}
  i: LongInt;
begin
  Result := '';
  {$IFDEF LINUX}
  pfd.FD := FD;
  pfd.events := POLLIN;
  pfd.revents := 0;
  if fpPoll(@pfd, 1, ATimeOut) <= 0 then
    exit;

  SetLength(Result, RLEN+1);
  i := fpRead(FD, Result[1], RLEN);
  if i > 0 then
    SetLength(Result, i)
  else
    Result := '';
  {$ENDIF}
end;

function WritePipe(FD: LongInt; AText: String): integer;
begin
  {$IFDEF LINUX}
  Result := Fpwrite(FD, AText[1], Length(AText));
  {$ELSE}
  Result := -1;
  {$ENDIF}
end;

procedure ClosePipe(FD: LongInt);
begin
  {$IFDEF LINUX}
  Fpclose(FD);
  {$ENDIF}
end;


end.


{%MainUnit fileutil.pas}

var
  //Function prototypes
  _ParamStrUtf8: Function(Param: Integer): string;

var
  ArgsW: Array of WideString;
  ArgsWCount: Integer;

//************ START "Stubs" that just call Ansi or Wide implementation

function ParamStrUTF8(Param: Integer): string;
begin
  Result := _ParamStrUtf8(Param);
end;

//************ END "Stubs" that just call Ansi or Wide implementation


//*************** START Non WideString implementations
{$ifndef wince}
function ParamStrUtf8Ansi(Param: Integer): String;
begin
  Result:=SysToUTF8(ObjPas.ParamStr(Param));
end;
{$endif wince}

//*************** END Non WideString impementations




//*************** START WideString impementations


procedure SetupCommandlineParametersWide;
var
  ArgLen, Start, CmdLen, i, j: SizeInt;
  argstart,
  Quote   : Boolean;
  Buf: array[0..259] of WChar;  // need MAX_PATH bytes, not 256!
  PCmdLineW: PWChar;
  CmdLineW: WideString;

  procedure AllocArg(Idx, Len:longint);
  begin
    if (Idx >= ArgsWCount) then
    begin
      SetLength(ArgsW, Idx + 1);
      SetLength(ArgsW[Idx], Len);
    end;
  end;


begin
  { create commandline, it starts with the executed filename which is argv[0] }
  { Win32 passes the command NOT via the args, but via getmodulefilename}
  ArgsWCount := 0;
  ArgLen := GetModuleFileNameW(0, @buf[0], sizeof(buf));

  //writeln('ArgLen = ',Arglen);

  buf[ArgLen] := #0; // be safe, no terminating 0 on XP
  allocarg(0,arglen);
  move(buf[0],ArgsW[0][1],arglen * SizeOf(WChar));

  //writeln('ArgsW[0] = ',ArgsW[0]);

  PCmdLineW := nil;
  { Setup cmdline variable }
  PCmdLineW := GetCommandLineW;
  CmdLen := StrLen(PCmdLineW);

  //writeln('StrLen(PCmdLineW) = ',CmdLen);

  SetLength(CmdLineW, CmdLen);
  Move(PCmdLineW^, CmdLineW[1], CmdLen * SizeOf(WChar));


  //debugln(CmdLineW);
  //for i := 1 to CmdLen do DbgOut(DbgS(i mod 10)); debugln;

  i := 1;
  while (i <= CmdLen) do
  begin
    //debugln('Next');
    //DbgOut('i=',DbgS(i),' CmdLineW[',DbgS(i),']=');if i<=CmdLen then debugln(CmdLineW[i]) else debugln('#0');
    //skip leading spaces
    while (i <= CmdLen) and (CmdLineW[i] <= #32) do Inc(i);
    //DbgOut('After skipping spaces: i=',DbgS(i),' CmdLineW[',DbgS(i),']=');if i<=CmdLen then debugln(CmdLineW[i]) else debugln('#0');
    if (i > CmdLen) then Break;
    Quote := False;
    Start := i;
    ArgLen := 0;
    while (i <= CmdLen) do
    begin //find next commandline parameter
      case CmdLineW[i] of
        #1..#32:
        begin
          if Quote then
          begin
            //debugln('i=',DbgS(i),': Space in Quote');
            Inc(ArgLen)
          end
          else
          begin
            //debugln('i=',DbgS(i),': Space in NOT Quote');
            Break;
          end;
        end;
        '"':
        begin
          if (i < CmdLen) and (CmdLineW[i+1] <> '"') then
          begin
            //debugln('i=',DbgS(i),': Quote := not Quote');
            Quote := not Quote
          end
          else
          begin
            //debugln('i=',DbgS(i),': Skip Quote');
            Inc(i);
          end;
        end;
        else Inc(ArgLen);
      end;//case
      Inc(i);


    end; //find next commandline parameter

    //debugln('ArgWCount=',DbgS(ArgsWCount),' Start=',DbgS(start),' ArgLen=',DbgS(arglen),' i=',DbgS(i));

    //we already have (a better) ArgW[0]
    if (ArgsWCount > 0) then
    begin //Process commadline parameter
      AllocArg(ArgsWCount, ArgLen);
      Quote := False;
      i := Start;
      j := 1;
      while (i <= CmdLen) do
      begin
        case CmdLineW[i] of
          #1..#32:
          begin
            if Quote then
            begin
              //if j > ArgLen then debugln('Error whitespace: j > ArgLen: j=',DbgS(j),' ArgLen=',DbgS(arglen));
              ArgsW[ArgsWCount][j] := CmdLineW[i];
              Inc(j);
            end
            else
              Break;
          end;
          '"':
          begin
            if (i < CmdLen) and (CmdLineW[i+1] <> '"') then
              Quote := not Quote
            else
              Inc(i);
          end;
          else
          begin
            //if j > ArgLen then debugln('Error Quote: j > ArgLen: j=',DbgS(j),' ArgLen=',DbgS(arglen));
            ArgsW[ArgsWCount][j] := CmdLineW[i];
            Inc(j);
          end;
        end;
        Inc(i);
      end;

      //debugln('ArgsW[',DbgS(ArgsWCount),'] = ',ArgsW[ArgsWCount]);
    end; // Process commandline parameter
    Inc(ArgsWCount);

  end;
  Dec(ArgsWCount);
end;

function ParamStrUtf8Wide(Param: Integer): String;
begin
  if ArgsWCount <> ParamCount then
  begin
    //DebugLn('Error: ParamCount <> ArgsWCount!');
    Result := SysToUtf8(ObjPas.ParamStr(Param));
  end
  else
  begin
    if (Param <= ArgsWCount) then
      Result := Utf8Encode(ArgsW[Param])
    else
      Result := '';
  end;
end;

//*************** END WideString impementations


{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
function GetFileDescription(const AFilename: string): string;
begin
  // date + time
  Result:=lrsModified;
  try
    Result:=Result+FormatDateTime('DD/MM/YYYY hh:mm',
                           FileDateToDateTime(FileAgeUTF8(AFilename)));
  except
    Result:=Result+'?';
  end;
end;

{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
begin
  Result:=Filename;
end;

function ConsoleToUTF8(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
var
  Dst: PChar;
begin
  {$ifdef WinCE}
  Result := SysToUTF8(s);
  {$else}
  Dst := AllocMem((Length(s) + 1) * SizeOf(Char));
  if OemToChar(PChar(s), Dst) then
    Result := StrPas(Dst)
  else
    Result := s;
  FreeMem(Dst);
  Result := SysToUTF8(Result);
  {$endif}
end;

function UTF8ToConsole(const s: string): string;
var
  Dst: PChar;
begin
  {$ifdef WinCE}
  Result := UTF8ToSys(s);
  {$else}
  Result := UTF8ToSys(s);
  Dst := AllocMem((Length(Result) + 1) * SizeOf(Char));
  if CharToOEM(PChar(Result), Dst) then
    Result := StrPas(Dst);
  FreeMem(Dst);
  {$endif}
end;


function ExtractShortPathNameUTF8(const FileName: String): String;
var
  lPathSize: DWORD;
  WideFileName, WideResult: UnicodeString;
begin
  // WinCE doesnt have this concept
  {$ifdef WinCE}
  Result := FileName;
  {$else}
  if Win32MajorVersion >= 5 then
  begin
    WideFileName := UTF8ToUTF16(FileName);
    SetLength(WideResult,Max_Path);
    lPathSize := GetShortPathNameW(PWideChar(WideFileName), PWideChar(WideResult), Length(WideResult));
    SetLength(WideResult,lPathSize);
    Result := UTF16ToUTF8(WideResult);
  end
  else
    Result:=SysToUTF8(SysUtils.ExtractShortPathName(UTF8ToSys(FileName)));
  {$endif}
end;


procedure InitFileUtil;
begin
  {$ifndef WinCE}
  if Win32MajorVersion <= 4 then
  begin
    _ParamStrUtf8 := @ParamStrUtf8Ansi;
  end
  else
  {$endif}
  begin
    try
      ArgsWCount := -1;
      _ParamStrUtf8 := @ParamStrUtf8Wide;
      SetupCommandlineParametersWide;
    Except
      ArgsWCount := -1;
    end;
  end;
end;

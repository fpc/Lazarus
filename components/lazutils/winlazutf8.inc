{%MainUnit lazutf8.pas}

{$IF DEFINED(UTF8_RTL) AND NOT DEFINED(WINCE)}
  {$DEFINE ArgsWAsUTF8}
{$ENDIF}

var
  //Function prototypes
  _ParamStrUtf8: Function(Param: Integer): string;

var
  ArgsW: Array of WideString;
  ArgsWCount: Integer; // length(ArgsW)+1
  {$IFDEF ArgsWAsUTF8}
  ArgsUTF8: Array of String; // the ArgsW array as UTF8
  OldArgV: PPChar = nil;
  {$IFEND}

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


{$IFDEF ArgsWAsUTF8}
procedure SetupArgvAsUtf8;
var
  i: Integer;
begin
  SetLength(ArgsUTF8,length(ArgsW));
  OldArgV:=argv;
  GetMem(argv,SizeOf(Pointer)*length(ArgsW));
  for i:=0 to length(ArgsW)-1 do
  begin
    ArgsUTF8[i]:=ArgsW{%H-}[i];
    argv[i]:=PChar(ArgsUTF8[i]);
  end;
end;
{$endif}

procedure SetupCommandlineParametersWide;
var
  ArgLen, Start, CmdLen, i, j: SizeInt;
  Quote   : Boolean;
  Buf: array[0..259] of WChar;  // need MAX_PATH bytes, not 256!
  PCmdLineW: PWideChar;
  CmdLineW: WideString;

  procedure AllocArg(Idx, Len:longint);
  begin
    if (Idx >= ArgsWCount) then
      SetLength(ArgsW, Idx + 1);
    SetLength(ArgsW[Idx], Len);
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
    begin //Process commandline parameter
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
  //Note:
  //On WinCe Argsv is a static function, so we cannot change it.
  //This might change in the future if Argsv on WinCE will be declared as a function variable
  {$IFDEF ArgsWAsUTF8}
  if DefaultSystemCodePage=CP_UTF8 then
    SetupArgvAsUtf8;
  {$IFEND}
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
      {$IFDEF ACP_RTL}
      Result := String(UnicodeString(ArgsW[Param]))
      {$ELSE}
      Result := Utf8Encode(ArgsW[Param])
      {$ENDIF ACP_RTL}
    else
      Result := '';
  end;
end;

{$IFNDEF WINCE}
function GetGetEnvironmentVariableCountWide: integer;
var
  hp,p : PWideChar;
begin
  Result:=0;
  p:=GetEnvironmentStringsW;
  if p=nil then exit;
  hp:=p;
  while hp^<>#0 do
  begin
    Inc(Result);
    hp:=hp+strlen(hp)+1;
  end;
  FreeEnvironmentStringsW(p);
end;


function GetEnvironmentStringWide(Index: Integer): UnicodeString;
var
  hp,p : PWideChar;
begin
  Result:='';
  p:=GetEnvironmentStringsW;
  if p=nil then exit;
  hp:=p;
  while (hp^<>#0) and (Index>1) do
  begin
    Dec(Index);
    hp:=hp+strlen(hp)+1;
  end;
  if (hp^<>#0) then
    Result:=hp;
  FreeEnvironmentStringsW(p);
end;
{$ENDIF WINCE}

function GetEnvironmentVariableWide(const EnvVar: string): UnicodeString;
{$IF FPC_FULLVERSION>=30000}
begin
  Result:=GetEnvironmentVariable(UTF8ToUTF16(EnvVar));
end;
{$ELSE}
var
  s, upperenv : Unicodestring;
  i : longint;
  hp,p : pwidechar;
begin
  Result:='';
  p:=GetEnvironmentStringsW;
  hp:=p;
  upperenv:=uppercase(envvar);
  while hp^<>#0 do
  begin
    s:=hp;
    i:=pos('=',s);
    if uppercase(copy(s,1,i-1))=upperenv then
    begin
      Result:=copy(s,i+1,length(s)-i);
      break;
    end;
    { next string entry}
    hp:=hp+strlen(hp)+1;
  end;
  FreeEnvironmentStringsW(p);
end;
{$ENDIF}


//*************** END WideString impementations

{$ifdef WinCE}
function ConsoleToUTF8(const s: string): string;// converts console encoding to UTF8
begin
  Result := SysToUTF8(s);
end;
{$else}
function ConsoleToUTF8(const s: string): string;// converts console encoding to UTF8
var
  Dst: PChar;
begin
  Dst := AllocMem((Length(s) + 1) * SizeOf(Char));
  if OemToChar(PChar(s), Dst) then
    Result := StrPas(Dst)
  else
    Result := s;
  FreeMem(Dst);
  Result := WinCPToUTF8(Result);
end;
{$endif not wince}

{$ifdef WinCe}
function UTF8ToConsole(const s: string): string; // converts UTF8 to console string (used by Write, WriteLn)
begin
  Result := UTF8ToSys(s);
end;
{$else}
function UTF8ToConsole(const s: string): string; // converts UTF8 to console string (used by Write, WriteLn)
var
  Dst: PChar;
begin
  {$ifndef NO_CP_RTL}
  Result := UTF8ToWinCP(s);
  {$else NO_CP_RTL}
  Result := UTF8ToSys(s); // Kept for compatibility
  {$endif NO_CP_RTL}
  Dst := AllocMem((Length(Result) + 1) * SizeOf(Char));
  if CharToOEM(PChar(Result), Dst) then
    Result := StrPas(Dst);
  FreeMem(Dst);
  {$ifndef NO_CP_RTL}
  SetCodePage(RawByteString(Result), CP_OEMCP, False);
  {$endif NO_CP_RTL}
end;
{$endif not WinCE}

{$ifdef WinCE}
function WinCPToUTF8(const s: string): string; inline;
begin
  Result := SysToUtf8(s);
end;
{$else}
// for all Windows supporting 8bit codepages (e.g. not WinCE)
function WinCPToUTF8(const s: string): string;
// result has codepage CP_ACP
var
  UTF16WordCnt: SizeInt;
  UTF16Str: UnicodeString;
begin
  Result:=s;
  if IsASCII(Result) then begin
    {$ifdef FPC_HAS_CPSTRING}
    // prevent codepage conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
    {$endif}
    exit;
  end;
  UTF16WordCnt:=MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pointer(s), length(s), nil, 0);
  // this will null-terminate
  if UTF16WordCnt>0 then
  begin
    setlength(UTF16Str, UTF16WordCnt);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pointer(s), length(s), @UTF16Str[1], UTF16WordCnt);
    Result:=UTF8Encode(UTF16Str);
    {$ifdef FPC_HAS_CPSTRING}
    // prevent codepage conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
    {$endif}
  end;
end;
{$endif not wince}

{$ifdef WinCe}
function UTF8ToWinCP(const s: string): string; inline;
begin
  Result := Utf8ToSys(s);
end;
{$else}
function UTF8ToWinCP(const s: string): string;
// result has codepage CP_ACP
var
  src: UnicodeString;
  len: LongInt;
begin
  Result:=s;
  if IsASCII(Result) then begin
    {$ifdef FPC_HAS_CPSTRING}
    // prevent codepage conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
    {$endif}
    exit;
  end;
  src:=UTF8Decode(s);
  if src='' then
    exit;
  len:=WideCharToMultiByte(CP_ACP,0,PUnicodeChar(src),length(src),nil,0,nil,nil);
  SetLength(Result,len);
  if len>0 then begin
    WideCharToMultiByte(CP_ACP,0,PUnicodeChar(src),length(src),@Result[1],length(Result),nil,nil);
    {$ifdef FPC_HAS_CPSTRING}
    // prevent codepage conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
    {$endif}
  end;
end;
{$endif not wince}

{$ifdef debugparamstrutf8}
procedure ParamStrUtf8Error;
var
  i: Integer;
begin
  writeln('Error in Windows WideString implementation of ParamStrUtf8');
  writeln('Using SysToUtf8(ParamsStr(Param)) as fallback');
  writeln('ParamCount = ',ParamCount,', ArgsWCount = ',ArgsWCount);
  for i := 0 to ParamCount do writeln('ParamStr(',i,') = "',ParamStr(i),'"');
  writeln;
  for i := 0 to ArgsWCount do writeln('ParamStrUtf8(',i,') = "',ArgsW[i],'"');
end;
{$endif}

function GetLocaleStr(aLocaleID, aLCType: Longint; const Def: string): String;
var
  L: Integer;
  {$IF FPC_FULLVERSION < 30000}
  ResultLen: Integer;
  {$ENDIF}
  Buf: array[0..255] of WideChar;
begin
  L := GetLocaleInfoW(aLocaleID, aLCType, Buf, SizeOf(Buf));
  if L > 0 then
  begin
    Result:='';
    {$IF FPC_FULLVERSION >= 30000}
    widestringmanager.Wide2AnsiMoveProc(PWideChar(@Buf[0]),Result,CP_UTF8,L-1);
    {$ELSE}
    ResultLen:=WideCharToMultiByte(CP_UTF8,0,PWideChar(@Buf[0]),L-1,nil,0,nil,nil);
    if ResultLen > 0 then
    begin
      SetLength(Result,ResultLen);
      WideCharToMultiByte(CP_UTF8,0,PWideChar(@Buf[0]),L-1,@result[1],ResultLen,nil,nil)
    end
    else
      Result:=Def;
    {$ENDIF}
  end
  else
    Result := Def;
end;

function GetLocaleCharUTF8(aLocaleID, aLCType: Longint; Def: Char): Char;
var
  Buf: array[0..3] of WideChar; // sdate allows 4 chars (3+ending #0)
  GLI, I: LongInt;
  WRes: WideChar;
begin
  //Use Widestring Api so it works on WinCE as well
  GLI := GetLocaleInfoW(aLocaleID, aLCType, Buf, Length(Buf)); // GLI is char count with the ending #0 char
  if GLI > 2 then
  begin // more than 1 char -> try to find first non-space character
    for I := 0 to GLI-2 do
    begin
      WRes := Buf[I];
      case Buf[I] of
        #32, #$00A0, #$2002, #$2003, #$2009, #$202F: begin end;// go over spaces
      else
        Break; // stop at non-space
      end;
    end;
  end else
  if GLI = 2 then // 1 char
    WRes := Buf[0]
  else
    WRes := Def;

  case WRes of
    #0..#127: Result := WRes;// ASCII - OK
    #$00A0: Result := ' ';   // non breakable space
    #$00B7: Result := '.';   // middle stop
    #$02D9: Result := '''';  // dot above, italian handwriting
    #$066B: Result := ',';   // arabic decimal separator, persian thousand separator
    #$066C: Result := '''';  // arabic thousand separator
    #$2002: Result := ' ';   // long space
    #$2003: Result := ' ';   // long space
    #$2009: Result := ' ';   // thin space
    #$202F: Result := ' ';   // narrow non breakable space
    #$2014: Result := '-';   // persian decimal mark
    #$2396: Result := '''';  // codepoint 9110 decimal separator
    { Utf8        Utf16
      C2 A0    -> 00A0
      C2 B7    -> 00B7
      CB 99    -> 02D9
      D9 AB    -> 066B
      D9 AC    -> 066C
      E2 80 82 -> 2002
      E2 80 83 -> 2003
      E2 80 89 -> 2009
      E2 80 AF -> 202F
      E2 80 94 -> 2014
      E2 8E 96 -> 2396
    }
  else // unicode character -> we need default ASCII char
    Result := Def;
  end;  //case
end;

procedure GetLocaleFormatSettingsUTF8(LCID: Integer; var aFormatSettings: TFormatSettings);
var
  HF  : Shortstring;
  LID : Windows.LCID;
  I,Day : longint;
begin
  LID := LCID;
  with aFormatSettings do
  begin
    { Date stuff }
    for I := 1 to 12 do
      begin
      ShortMonthNames[I]:=GetLocaleStr(LID,LOCALE_SABBREVMONTHNAME1+I-1,ShortMonthNames[i]);
      LongMonthNames[I]:=GetLocaleStr(LID,LOCALE_SMONTHNAME1+I-1,LongMonthNames[i]);
      end;
    for I := 1 to 7 do
      begin
      Day := (I + 5) mod 7;
      ShortDayNames[I]:=GetLocaleStr(LID,LOCALE_SABBREVDAYNAME1+Day,ShortDayNames[i]);
      LongDayNames[I]:=GetLocaleStr(LID,LOCALE_SDAYNAME1+Day,LongDayNames[i]);
      end;
    DateSeparator := GetLocaleCharUTF8(LID, LOCALE_SDATE, '/');
    ShortDateFormat := GetLocaleStr(LID, LOCALE_SSHORTDATE, 'm/d/yy');
    LongDateFormat := GetLocaleStr(LID, LOCALE_SLONGDATE, 'mmmm d, yyyy');
    { Time stuff }
    TimeSeparator := GetLocaleCharUTF8(LID, LOCALE_STIME, ':');
    TimeAMString := GetLocaleStr(LID, LOCALE_S1159, 'AM');
    TimePMString := GetLocaleStr(LID, LOCALE_S2359, 'PM');
    if StrToIntDef(GetLocaleStr(LID, LOCALE_ITLZERO, '0'), 0) = 0 then
      HF:='h'
    else
      HF:='hh';
    // No support for 12 hour stuff at the moment...
    ShortTimeFormat := HF+':nn';
    LongTimeFormat := HF + ':nn:ss';
    { Currency stuff }
    CurrencyString:=GetLocaleStr(LID, LOCALE_SCURRENCY, '');
    CurrencyFormat:=StrToIntDef(GetLocaleStr(LID, LOCALE_ICURRENCY, '0'), 0);
    NegCurrFormat:=StrToIntDef(GetLocaleStr(LID, LOCALE_INEGCURR, '0'), 0);
    { Number stuff }
    ThousandSeparator:=GetLocaleCharUTF8(LID, LOCALE_STHOUSAND, ',');
    DecimalSeparator:=GetLocaleCharUTF8(LID, LOCALE_SDECIMAL, '.');
    CurrencyDecimals:=StrToIntDef(GetLocaleStr(LID, LOCALE_ICURRDIGITS, '0'), 0);
    ListSeparator := GetLocaleCharUTF8(LID, LOCALE_SLIST, ',');
  end;
end;

procedure GetFormatSettingsUTF8;
begin
  {$ifndef wince}
  GetLocaleFormatSettingsUTF8(GetThreadLocale, FormatSettings);
  {$else}
  GetLocaleFormatSettingsUTF8(GetUserDefaultLCID, FormatSettings);
  {$endif}
end;

{$IFDEF UTF8_RTL}
function UTF8StrCompAnsiString(S1, S2: PChar): PtrInt;
begin
  Result:=UTF8CompareStrP(S1,S2);
end;

function UTF8StrICompAnsiString(S1, S2: PChar): PtrInt;
var
  U1, U2: String;
begin
  U1:=StrPas(S1);
  U2:=StrPas(S2);
  Result:=UTF8CompareText(U1,U2);
end;

function UTF8StrLCompAnsiString(S1, S2: PChar; Count: PtrUInt): PtrInt;
begin
  Result:=UTF8CompareStr(S1,Count,S2,Count);
end;

function UTF8StrLICompAnsiString(S1, S2: PChar; Count: PtrUInt): PtrInt;
var
  U1, U2: String;
begin
  if Count>0 then begin
    SetLength(U1,Count);
    Move(S1^,PByte(U1)^,Count);
    SetLength(U2,Count);
    Move(S2^,PByte(U2)^,Count);
    Result:=UTF8CompareText(U1,U2);
  end else
    Result:=0;
end;

function UTF8GetStandardCodePage(const stdcp: TStandardCodePageEnum
  ): TSystemCodePage;
begin
  Result:=CP_UTF8;
end;
{$ENDIF}

procedure InitLazUtf8;
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
      {$ifdef debugparamstrutf8}
      if ParamCount <> ArgsWCount then ParamStrUtf8Error;
      {$endif}
    Except
      begin
        ArgsWCount := -1;
        {$ifdef debugparamstrutf8}
        ParamStrUtf8Error;
        {$endif}
      end;
    end;
  end;
  {$IFDEF UTF8_RTL}
  GetFormatSettingsUTF8;
  widestringmanager.UpperAnsiStringProc:=@UTF8UpperString;
  widestringmanager.LowerAnsiStringProc:=@UTF8LowerString;
  widestringmanager.CompareStrAnsiStringProc:=@UTF8CompareStr;
  widestringmanager.CompareTextAnsiStringProc:=@UTF8CompareText;
  widestringmanager.StrCompAnsiStringProc:=@UTF8StrCompAnsiString;
  widestringmanager.StrICompAnsiStringProc:=@UTF8StrICompAnsiString;
  widestringmanager.StrLCompAnsiStringProc:=@UTF8StrLCompAnsiString;
  widestringmanager.StrLICompAnsiStringProc:=@UTF8StrLICompAnsiString;
  widestringmanager.GetStandardCodePageProc:=@UTF8GetStandardCodePage;
  // Does anyone need these two?
  //widestringmanager.StrLowerAnsiStringProc;
  //widestringmanager.StrUpperAnsiStringProc;
  {$IFEND}
end;

procedure FinalizeLazUTF8;
{$IFDEF ArgsWAsUTF8}
var
  p: PPChar;
{$ENDIF}
begin
  {$IFDEF ArgsWAsUTF8}
  // restore argv and free memory
  if OldArgV<>nil then
  begin
    p:=argv;
    argv:=OldArgV;
    Freemem(p);
  end;
  {$IFEND}
end;

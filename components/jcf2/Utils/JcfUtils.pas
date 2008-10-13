unit JcfUtils;

{$I JcfGlobal.inc}

interface
uses
  SysUtils, Classes;

const
  AnsiNull           = Char(#0);
  AnsiSoh            = Char(#1);
  AnsiStx            = Char(#2);
  AnsiEtx            = Char(#3);
  AnsiEot            = Char(#4);
  AnsiEnq            = Char(#5);
  AnsiAck            = Char(#6);
  AnsiBell           = Char(#7);
  AnsiBackspace      = Char(#8);
  AnsiTab            = Char(#9);
  AnsiLineFeed       = AnsiChar(#10);
  AnsiVerticalTab    = Char(#11);
  AnsiFormFeed       = Char(#12);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  AnsiSo             = Char(#14);
  AnsiSi             = Char(#15);
  AnsiDle            = Char(#16);
  AnsiDc1            = Char(#17);
  AnsiDc2            = Char(#18);
  AnsiDc3            = Char(#19);
  AnsiDc4            = Char(#20);
  AnsiNak            = Char(#21);
  AnsiSyn            = Char(#22);
  AnsiEtb            = Char(#23);
  AnsiCan            = Char(#24);
  AnsiEm             = Char(#25);
  AnsiEndOfFile      = Char(#26);
  AnsiEscape         = Char(#27);
  AnsiFs             = Char(#28);
  AnsiGs             = Char(#29);
  AnsiRs             = Char(#30);
  AnsiUs             = Char(#31);
  AnsiSpace          = Char(' ');
  AnsiComma          = Char(',');
  AnsiBackslash      = Char('\');
  AnsiForwardSlash   = Char('/');

  {$IFDEF MSWINDOWS}
  AnsiLineBreak = AnsiCrLf;
  PathSeparator    = '\';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  AnsiLineBreak = AnsiLineFeed;
  PathSeparator    = '/';
  {$ENDIF UNIX}
  DirDelimiter = PathSeparator;
  AnsiHexDigits      = ['0'..'9', 'A'..'F', 'a'..'f'];
  AnsiWhiteSpace     = [AnsiTab, AnsiLineFeed, AnsiVerticalTab,
    AnsiFormFeed, AnsiCarriageReturn, AnsiSpace];

  AnsiDoubleQuote = Char('"');
  AnsiSingleQuote = Char('''');


function CharIsControl(const C: Char): Boolean;
function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;
function CharIsDigit(const C: Char): Boolean;
function CharIsReturn(const C: Char): Boolean;
function CharIsWhiteSpace(const C: Char): Boolean;

function CharUpper(const C: Char): Char; 

function StrIsAlpha(const S: string): Boolean;
function StrIsAlphaNum(const S: string): Boolean;

function StrTrimQuotes(const S: string): string;

function StrAfter(const SubStr, S: string): string;
function StrBefore(const SubStr, S: string): string;
function StrChopRight(const S: string; N: Integer): string;
function StrLastPos(const SubStr, S: string): Integer;
function StrLeft(const S: string; Count: Integer): string;
function StrRestOf(const S: string; N: Integer ): string;
function StrRight(const S: string; Count: Integer): string;

function StrDoubleQuote(const S: string): string;
function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;

function StrCharCount(const S: string; C: Char): Integer;
function StrStrCount(const S, SubS: string): Integer;
function StrRepeat(const S: string; Count: Integer): string;
procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags = []);
function StrSearch(const Substr, S: string; const Index: Integer = 1): Integer;

function BooleanToStr(B: Boolean): string;
function StrToBoolean(const S: string): Boolean;

function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True);

function FileToString(const FileName: string): AnsiString;
procedure StringToFile(const FileName: string; const Contents: AnsiString);
function StrFillChar(const C: Char; Count: Integer): string;
function IntToStrZeroPad(Value, Count: Integer): String;
function PathExtractFileNameNoExt(const Path: string): string;
function GetWindowsTempFolder: string;
function FileGetSize(const FileName: string): Int64;
procedure ShellExecEx(const FileName: string; const Parameters: string = '');
function GetTickCount: DWord;
function IsMultiByte(const pcChar: WideChar): Boolean;

type
  EJcfConversionError = class(Exception)
  end;

implementation

uses
{$ifdef fpc}
  {$ifdef windows}
    Windows,
  {$endif}
  {$ifdef Unix}
    Unix,
  {$endif}
  LCLIntf, FileUtil
{$else}
  Windows
{$endif};

function CharIsAlpha(const C: Char): Boolean;
begin
  Result := C in ['a'..'z','A'..'Z'];
end;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  Result := CharIsAlpha(C) or CharIsDigit(C);
end;

function CharIsControl(const C: Char): Boolean;
begin
  Result := C <= #31;
end;

function CharIsDigit(const C: Char): Boolean;
begin
  Result := C in ['0'..'9'];
end;

function CharIsReturn(const C: Char): Boolean;
begin
  Result := C in [AnsiLineFeed, AnsiCarriageReturn];
end;

function CharIsWhiteSpace(const C: Char): Boolean;
begin
  Result := C in AnsiWhiteSpace;
end;

function CharUpper(const C: Char): Char;
begin
  // Paul: original code used char case table
  Result := UpCase(C);
end;

function StrIsAlpha(const S: string): Boolean;
var
  I, L: integer;
begin
  L := Length(S);
  Result := L > 0;
  for I := 1 to L do
    if not CharIsAlpha(S[I]) then
    begin
      Result := False;
      break;
    end;
end;

function StrIsAlphaNum(const S: string): Boolean;
var
  I, L: integer;
begin
  L := Length(S);
  Result := L > 0;
  for I := 1 to L do
    if not CharIsAlphaNum(S[I]) then
    begin
      Result := False;
      break;
    end;
end;

function StrTrimQuotes(const S: string): string;
var
  C1, C2: Char;
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if L >= 2 then
  begin
    C1 := Result[1];
    C2 := Result[L];
    if (C1 = C2) and (C1 in [AnsiSingleQuote, AnsiDoubleQuote]) then
    begin
      Delete(Result, L, 1);
      Delete(Result, 1, 1);
    end;
  end;
end;

function StrAfter(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrSearch(SubStr, S, 1);
  if P > 0 then
    Result := Copy(S, P + Length(SubStr), Length(S))
  else
    Result := '';
end;

function StrBefore(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrSearch(SubStr, S, 1);
  if P > 0 then
    Result := Copy(S, 1, P - 1)
  else
    Result := S;
end;

function StrChopRight(const S: string; N: Integer): string;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLastPos(const SubStr, S: string): Integer;
var
  NewPos: Integer;
begin
  Result := 0;
  while Result < Length(S) do
  begin
    NewPos := StrSearch(SubStr, S, Result + 1);
    if NewPos > 0 then
      Result := NewPos
    else
      break;
  end;
end;

function StrLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S, 1, Count);
end;

function StrRestOf(const S: string; N: Integer ): string;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: string; Count: Integer): string;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

function StrDoubleQuote(const S: string): string;
begin
  Result := AnsiDoubleQuote + S + AnsiDoubleQuote;
end;

function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;
var
  i: integer;
begin
  // if no delimiters passed then use default set
  if Delimiters = [] then
    Delimiters := AnsiWhiteSpace;
  Result := S;
  for i := 1 to Length(Result) do
    if (i = 1) or (Result[i - 1] in Delimiters) then
      Result[i] := UpCase(Result[i]);
end;

function StrCharCount(const S: string; C: Char): Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(Result);
end;

function StrStrCount(const S, SubS: string): Integer;
var
  P: integer;
begin
  Result := 0;
  P := 1;
  while P < Length(S) do
  begin
    P := StrSearch(Subs, S, P);
    if P > 0 then
    begin
      inc(Result);
      inc(P);
    end
    else
      break;
  end;
end;

function StrRepeat(const S: string; Count: Integer): string;
begin
  Result := '';
  while Count > 0 do
  begin
    Result := Result + S;
    Dec(Count);
  end;
end;

procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags = []);
begin
  S := StringReplace(S, Search, Replace, Flags);
end;

function StrSearch(const Substr, S: string; const Index: Integer = 1): Integer;
begin
  // Paul: I expect original code was more efficient :) 
  Result := Pos(SubStr, Copy(S, Index, Length(S))) + Index - 1;
end;

function BooleanToStr(B: Boolean): string;
const
  BoolToStrMap: array[Boolean] of String =
  (
 { false } 'False',
 { true  } 'True'
  );
begin
  Result := BoolToStrMap[B];
end;

function StrToBoolean(const S: string): Boolean;
var
  LowerS: String;
begin
  LowerS := LowerCase(S);
  if (LowerS = 'false') or (LowerS = 'no') then
    Result := False
  else
  if (LowerS = 'true') or (LowerS = 'yes') then
    Result := True
  else
    raise EJcfConversionError.Create('Cannot convert string [' + S + '] to boolean');
end;


function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;
begin
  // Paul: original code used comparision by char case table
  Result := StrSearch(LowerCase(SubStr), LowerCase(S), Index);
end;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True);
var
  i: integer;
begin
  if List <> nil then
    for i := List.Count - 1 downto 0 do
    begin
      List[i] := Trim(List[i]);
      if DeleteIfEmpty and (List[i] = '') then
        List.Delete(i);
    end;
end;

function FileToString(const FileName: string): AnsiString;
var
  S: TStream;
begin
  S := nil;
  try
    S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    SetLength(Result, S.Size);
    S.Read(PAnsiChar(Result)^, S.Size);
  finally
    S.Free;
  end;
end;

procedure StringToFile(const FileName: string; const Contents: AnsiString);
var
  S: TStream;
begin
  S := nil;
  try
    S := TFileStream.Create(FileName, fmCreate);
    S.Write(PAnsiChar(Contents)^, Length(Contents));
  finally
    S.Free;
  end;
end;

function StrFillChar(const C: Char; Count: Integer): string;
begin
  SetLength(Result, Count);
  if Count > 0 then
    FillChar(Result[1], Count, C);
end;

function IntToStrZeroPad(Value, Count: Integer): String;
begin
  Result := IntToStr(Value);
  while Length(Result) < Count do
    Result := '0' + Result;
end;

function PathRemoveExtension(const Path: string): string;
var
  p: Integer;
begin
  // from lazarus FileUtil
  Result := Path;
  p := Length(Result);
  while (p>0) do
  begin
    case Result[p] of
      PathDelim: Exit;
      '.': Result := copy(Result, 1, p-1);
    end;
    Dec(p);
  end;
end;

function PathExtractFileNameNoExt(const Path: string): string;
begin
  Result := PathRemoveExtension(ExtractFileName(Path));
end;

function PathRemoveSeparator(const Path: string): string;
begin
  Result := Path;
  if (Result <> '') and (Result[Length(Result)] = PathDelim) then
    Delete(Result, Length(Result), 1);
end;

function GetWindowsTempFolder: string;
{$ifndef fpc}
var
  buf: string;
{$endif}
begin
{$ifdef fpc}
  Result := GetTempDir;
{$else}
  SetLength(buf, MAX_PATH);
  SetLength(buf, GetTempPath(Length(buf) + SizeOf(char), PChar(buf)));
  Result:=buf;
  Result := IncludeTrailingPathDelimiter(Result);
{$endif}
end;

function FileGetSize(const FileName: string): Int64;
{$ifndef fpc}
var
  FileInfo: TSearchRec;
{$endif}
begin
{$ifdef fpc}
  Result := FileUtil.FileSize(FileName);
{$else}
  // from LCL FileUtil code
  FileInfo.Name := Filename;
  FileInfo.FindHandle := Windows.FindFirstFile(Windows.LPTSTR(FileInfo.Name), FileInfo.FindData);
  if FileInfo.FindHandle = Windows.Invalid_Handle_value then
  begin
    Result:=-1;
    Exit;
  end;
  Result := (int64(FileInfo.FindData.nFileSizeHigh) shl 32) + FileInfo.FindData.nFileSizeLow;
  Windows.FindClose(FileInfo.FindHandle);
{$endif}
end;

procedure ShellExecEx(const FileName: string; const Parameters: string = '');
begin
  {$ifdef windows}
    Windows.ShellExecute(0, 'open', PChar(FileName), PChar(Parameters), nil, SW_SHOW);
  {$endif}
  {$ifdef unix}
    Shell(format('%s %s',[FileName, Parameters]));
  {$endif}
end;

function GetTickCount: DWord;
begin
{$ifdef windows}
  Result := Windows.GetTickCount;
{$else}
  Result := LCLIntf.GetTickCount;
{$endif}
end;

function IsMultiByte(const pcChar: WideChar): Boolean;
begin
{$ifdef windows}
  Result := IsDBCSLeadByte(Byte(pcChar));
{$else}
  Result := False;
  // TODO: ?
{$endif}
end;


end.

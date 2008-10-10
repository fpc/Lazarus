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
const
  // CharType return values
  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;
function CharIsControl(const C: Char): Boolean;
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

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean = True): string;
procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True );

function FileToString(const FileName: string): AnsiString;
procedure StringToFile(const FileName: string; const Contents: AnsiString);
function StrFillChar(const C: Char; Count: Integer): string;
function IntToStrZeroPad(Value, Count: Integer): AnsiString;
function PathExtractFileNameNoExt(const Path: string): string;
function GetWindowsTempFolder: string;
function FileGetSize(const FileName: string): Int64;
procedure ShellExecEx(const FileName: string; const Parameters: string = '');

type
  EJcfConversionError = class(Exception)
  end;

implementation

function CharIsAlpha(const C: Char): Boolean;
begin
end;

function CharIsAlphaNum(const C: Char): Boolean;
begin
end;

function CharIsControl(const C: Char): Boolean;
begin
end;

function CharIsDigit(const C: Char): Boolean;
begin
end;

function CharIsReturn(const C: Char): Boolean;
begin
end;

function CharIsWhiteSpace(const C: Char): Boolean;
begin
end;

function CharUpper(const C: Char): Char;
begin
  // Paul: original code used char case table
  Result := UpCase(C);
end;

function StrIsAlpha(const S: string): Boolean;
begin
end;

function StrIsAlphaNum(const S: string): Boolean;
begin
end;

function StrTrimQuotes(const S: string): string;
begin
end;

function StrAfter(const SubStr, S: string): string;
begin
end;

function StrBefore(const SubStr, S: string): string;
begin
end;

function StrChopRight(const S: string; N: Integer): string;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLastPos(const SubStr, S: string): Integer;
begin
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
end;

function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;
begin
end;

function StrCharCount(const S: string; C: Char): Integer;
begin
end;

function StrStrCount(const S, SubS: string): Integer;
begin
end;

function StrRepeat(const S: string; Count: Integer): string;
begin
end;

procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags = []);
begin
  S := StringReplace(S, Search, Replace, Flags);
end;

function StrSearch(const Substr, S: string; const Index: Integer = 1): Integer;
begin
  // Paul: I expect original code was more efficient :) 
  Result := Pos(SubStr, Copy(S, Index, Length(S)));
end;

function BooleanToStr(B: Boolean): string;
begin
end;

function StrToBoolean(const S: string): Boolean;
begin
end;


function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;
begin
  // Paul: original code used comparision by char case table
  Result := StrSearch(LowerCase(S), LowerCase(S), Index);
end;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
begin
end;

function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean): string;
begin
end;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True );
begin
end;

function FileToString(const FileName: string): AnsiString;
begin
end;

procedure StringToFile(const FileName: string; const Contents: AnsiString);
begin
end;

function StrFillChar(const C: Char; Count: Integer): string;
begin
end;

function IntToStrZeroPad(Value, Count: Integer): AnsiString;
begin
end;

function PathRemoveExtension(const Path: string): string;
begin
end;

function PathExtractFileNameNoExt(const Path: string): string;
begin
end;

procedure StrResetLength(var S: string);
begin
end;

function PathRemoveSeparator(const Path: string): string;
begin
end;

function GetWindowsTempFolder: string;
begin
end;

function FileGetSize(const FileName: string): Int64;
begin
end;

procedure ShellExecEx(const FileName: string; const Parameters: string = '');
begin
end;

end.

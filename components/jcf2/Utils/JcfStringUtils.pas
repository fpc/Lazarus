unit JcfStringUtils;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfStringUtils, released October 2008.
The Initial Developer of the Original Code is Paul Ishenin 
Portions created by Paul Ishenin are Copyright (C) 1999-2008 Paul Ishenin
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

{
This unit contains string utility code
For use when the JCL string functions are not avaialable
}
interface

uses
  SysUtils, Classes, StrUtils;

const
  NativeNull           = Char(#0);
  NativeSoh            = Char(#1);
  NativeStx            = Char(#2);
  NativeEtx            = Char(#3);
  NativeEot            = Char(#4);
  NativeEnq            = Char(#5);
  NativeAck            = Char(#6);
  NativeBell           = Char(#7);
  NativeBackspace      = Char(#8);
  NativeTab            = Char(#9);
  NativeLineFeed       = AnsiChar(#10);
  NativeVerticalTab    = Char(#11);
  NativeFormFeed       = Char(#12);
  NativeCarriageReturn = AnsiChar(#13);
  NativeCrLf           = AnsiString(#13#10);
  NativeSo             = Char(#14);
  NativeSi             = Char(#15);
  NativeDle            = Char(#16);
  NativeDc1            = Char(#17);
  NativeDc2            = Char(#18);
  NativeDc3            = Char(#19);
  NativeDc4            = Char(#20);
  NativeNak            = Char(#21);
  NativeSyn            = Char(#22);
  NativeEtb            = Char(#23);
  NativeCan            = Char(#24);
  NativeEm             = Char(#25);
  NativeEndOfFile      = Char(#26);
  NativeEscape         = Char(#27);
  NativeFs             = Char(#28);
  NativeGs             = Char(#29);
  NativeRs             = Char(#30);
  NativeUs             = Char(#31);
  NativeSpace          = Char(' ');
  NativeComma          = Char(',');
  NativeBackslash      = Char('\');
  NativeForwardSlash   = Char('/');

  {$IFDEF MSWINDOWS}
  NativeLineBreak = NativeCrLf;
  PathSeparator    = '\';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  NativeLineBreak = NativeLineFeed;
  PathSeparator    = '/';
  {$ENDIF UNIX}
  DirDelimiter = PathSeparator;
  NativeHexDigits      = ['0'..'9', 'A'..'F', 'a'..'f'];
  NativeWhiteSpace     = [NativeTab, NativeLineFeed, NativeVerticalTab,
    NativeFormFeed, NativeCarriageReturn, NativeSpace];

  NativeDoubleQuote = Char('"');
  NativeSingleQuote = Char('''');

function CharInSet(const C: Char; const testSet: TSysCharSet): Boolean;
function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;
function CharIsWordChar(const c: Char): Boolean;
function CharIsControl(const C: Char): Boolean;
function CharIsDigit(const C: Char): Boolean;
function CharIsReturn(const C: Char): Boolean;
function CharIsWhiteSpace(const C: Char): Boolean;
function CharIsWhiteSpaceNoReturn(const c: Char): boolean;
function CharIsPuncChar(const c: Char): boolean;

function StrIsAlpha(const S: string): Boolean;
function StrIsAlphaNum(const S: string): Boolean;
function CharIsHexDigitDot(const c: Char): Boolean;
function CharIsBinDigit(const c: Char): Boolean;

function StrTrimQuotes(const S: string): string;
function StrAfter(const SubStr, S: string): string;
function StrBefore(const SubStr, S: string): string;
function StrChopRight(const S: string; N: Integer): string;
function StrLastPos(const SubStr, S: string): Integer;
function StrIPos(const SubStr, S: string): integer;

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
function StrIsOneOf(const S: string; const List: array of string): Boolean;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True);

function FileToString(const FileName: string): AnsiString;
procedure StringToFile(const FileName: string; const Contents: AnsiString);
function StrFillChar(const C: Char; Count: Integer): string;
function IntToStrZeroPad(Value, Count: Integer): String;
function StrPadLeft(const pcOriginal: string;
  const piDesiredLength: integer; const pcPad: Char): string;

//function WideStringReplace(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags): WideString;

function PathExtractFileNameNoExt(const Path: string): string;

function PadNumber(const pi: integer): string;
function StrHasAlpha(const str: String): boolean;

//offset in bytes of first char of the lines. 1 based.
procedure FindLineOffsets(const aStr: string; aLineStart, aLineEnd: integer;
                      out aLineStartOffset: integer; out aLineEndOffset:integer);
function SkipLeftSpaces(const aStr: string; aPos: integer): integer;
function SkipToNextLine(const aStr: string; aPos: integer): integer;
function HasStringAtLineStart(const aSourceCode: string; const aStr: string): boolean;

type
  EJcfConversionError = class(Exception)
  end;

implementation

// define CharInSet
function CharInSet(const C: Char; const testSet: TSysCharSet): Boolean;
begin
  Result := C in testSet;
end;

function CharIsAlpha(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z','A'..'Z']);
end;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  Result := CharIsAlpha(C) or CharIsDigit(C);
end;

function CharIsWordChar(const c: Char): Boolean;
begin
  Result := CharIsAlpha(c) or (c = '_');
end;

function CharIsControl(const C: Char): Boolean;
begin
  Result := C <= #31;
end;

function CharIsDigit(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['0'..'9']);
end;

function CharIsReturn(const C: Char): Boolean;
begin
  Result := CharInSet(C, [NativeLineFeed, NativeCarriageReturn]);
end;

function CharIsWhiteSpace(const C: Char): Boolean;
begin
  Result := CharInSet(C, NativeWhiteSpace) ;
end;

function CharIsWhiteSpaceNoReturn(const c: Char): boolean;
begin
  Result := False;
  if (c = #0) or CharIsReturn(c) then exit;
  // Result := CharIsWhiteSpace(c) and (c <> AnsiLineFeed) and (c <> AnsiCarriageReturn);
  Result := (ord(c) <= Ord(NativeSpace));
end;

function CharIsPuncChar(const c: Char): boolean;
begin
  Result := False;
  if CharIsWhiteSpace(c) then exit;
  if CharIsAlphaNum(c) then exit;
  if CharIsReturn(c) then exit;
  if CharIsControl(c) then exit;
  Result := True;
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

function CharIsHexDigitDot(const c: Char): Boolean;
const
  HexDigits: set of AnsiChar = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F',
    'a', 'b', 'c', 'd', 'e', 'f'];
begin
  Result := (c in HexDigits) or (c = '.');
end;

function CharIsBinDigit(const c: Char): Boolean;
const
  BinDigits: set of AnsiChar = ['0','1'];
begin
  Result := (c in BinDigits);
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
    if (C1 = C2) and (CharInSet(C1, [NativeSingleQuote, NativeDoubleQuote])) then
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

{ case-insensitive "pos" }
function StrIPos(const SubStr, S: string): integer;
begin
  // simple and inneficient implmentation
  Result := Pos(UpperCase(SubStr), UpperCase(s));
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
  Result := NativeDoubleQuote + S + NativeDoubleQuote;
end;

function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;
var
  i: integer;
begin
  // if no delimiters passed then use default set
  if Delimiters = [] then
    Delimiters := NativeWhiteSpace;
  Result := S;
  for i := 1 to Length(Result) do
    if (i = 1) or (CharInSet(Result[i - 1], Delimiters)) then
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
  Result := Pos(SubStr, Copy(S, Index, Length(S)));

  if Result > 0 then
    Result := Result + Index - 1;
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
  if (LowerS = 'false') or (LowerS = 'no') or (LowerS = '0') then
    Result := False
  else
  if (LowerS = 'true') or (LowerS = 'yes') or (LowerS = '1') or (LowerS = '-1') then
    Result := True
  else
    raise EJcfConversionError.Create('Cannot convert string [' + S + '] to boolean');
end;


function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;
begin
  // Paul: original code used comparision by char case table
  Result := StrSearch(LowerCase(SubStr), LowerCase(S), Index);
end;

function StrIsOneOf(const S: string; const List: array of string): Boolean;
var
  i: integer;
begin
  for i := Low(List) to High(List) do
    if CompareStr(List[i], S) = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
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

{ pad the string on the left had side until it fits }
function StrPadLeft(const pcOriginal: string;
  const piDesiredLength: integer; const pcPad: Char): string;
begin
  Result := pcOriginal;

  while (Length(Result) < piDesiredLength) do
  begin
    Result := pcPad + Result;
  end;

end;

// Based on FreePascal version of StringReplace
{function WideStringReplace(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags): WideString;
var
  Srch, OldP, RemS: WideString; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P: Integer;
begin
  Srch := S;
  OldP := OldPattern;
  if rfIgnoreCase in Flags then
  begin
    Srch := WideUpperCase(Srch);
    OldP := WideUpperCase(OldP);
  end;
  RemS := S;
  Result := '';
  while (Length(Srch) <> 0) do
  begin
    P := Pos(OldP, Srch);
    if P = 0 then
    begin
      Result := Result + RemS;
      Srch := '';
    end
    else
    begin
      Result := Result + Copy(RemS, 1, P - 1) + NewPattern;
      P := P + Length(OldP);
      RemS := Copy(RemS, P, Length(RemS) - P + 1);
      if not (rfReplaceAll in Flags) then
      begin
        Result := Result + RemS;
        Srch := '';
      end
      else
        Srch := Copy(Srch, P, Length(Srch) - P + 1);
    end;
  end;
end;
}
function PadNumber(const pi: integer): string;
begin
  Result := IntToStrZeroPad(pi, 3);
end;

function StrHasAlpha(const str: String): boolean;
var
  liLoop: integer;
begin
  Result := False;

  for liLoop := 1 to Length(str) do
  begin
    if CharIsAlpha(str[liLoop]) then
    begin
      Result := True;
      break;
    end;
  end;
end;

//offset in bytes of first char of the lines. 1 based.
procedure FindLineOffsets(const aStr: string; aLineStart, aLineEnd: integer;
                      out aLineStartOffset: integer; out aLineEndOffset:integer);
var
  lineCount:integer;
  len:integer;
  pC:PChar;
  offset:integer;
begin
  len:=length(aStr);
  pC:=@aStr[1];
  lineCount:=1;
  offset:=1;
  aLineStartOffset:=0;
  aLineEndOffset:=0;
  if len<1 then
    exit;
  if aLineStart=1 then
    aLineStartOffset:=offset;
  if (aLineEnd=1) then
    aLineEndOffset:=offset;
  while (offset<=len) and (lineCount<=aLineEnd) do
  begin
    while (offset<=len) and (pC^<>#10) do
    begin
      inc(offset);
      inc(pC);
    end;
    if (pC^=#10) and (offset<len) then
    begin
      inc(pC);
      inc(offset);
      inc(lineCount);
      if lineCount=aLineStart then
        aLineStartOffset:=offset;
      if lineCount=aLineEnd then
      begin
        aLineEndOffset:=offset;
        exit;
      end;
    end
    else
      exit;
  end;
end;

function SkipLeftSpaces(const aStr: string; aPos: integer): integer;
begin
  while (aPos > 1) do
  begin
    Dec(aPos);
    if not (aStr[aPos] in [' ',#9]) then
      break;
  end;
  Result := aPos;
end;

function SkipToNextLine(const aStr: string; aPos: integer): integer;
begin
  while (aPos > 1) and (aPos < Length(aStr)) do
  begin
    Inc(aPos);
    if aStr[aPos] = #10 then
    begin
      Inc(aPos);
      break;
    end;
  end;
  Result := aPos;
end;

function HasStringAtLineStart(const aSourceCode: string; const aStr: string): boolean;
var
  index, stringStart: integer;
begin
  index := 1;
  while (index > 0) and (index < length(aSourceCode)) do
  begin
    stringStart := PosEx(aStr, aSourceCode, index);
    if (stringStart > 0) then
    begin
      index := SkipLeftSpaces(aSourceCode, stringStart);
      if (index > 0) and ((index = 1) or (aSourceCode[index] in [#10, #13])) then
        exit(True);
      index := stringStart + length(aStr);
    end
    else
      break;
  end;
  Result := False;
end;

{------------------------------------------------------
  functions to manipulate file paths in strings }

function PathRemoveExtension(const Path: string): string;
var
  p: Integer;
begin
  // from Lazarus FileUtil
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

end.

{ $Id$ }
{                   -------------------------------------------
                     dbgutils.pp  -  Debugger utility routines
                    -------------------------------------------

 @created(Sun Apr 28st WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains a collection of debugger support routines.

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit DebugUtils;

{$mode objfpc}{$H+}

interface 

uses
  Classes,
  // LazUtils
  LazLoggerBase, LazUTF8,
  // DebuggerIntf
  DbgIntfBaseTypes;

type

  TPCharWithLen = record
    Ptr: PChar;
    Len: Integer;
  end;

  TGdbUnEscapeFlags = set of (uefOctal, uefTab, uefNewLine);

function GetLine(var ABuffer: String): String;
function ConvertToCString(const AText: String): String;
function ConvertPathDelims(const AFileName: String): String;
function DeleteEscapeChars(const AValue: String; const AEscapeChar: Char = '\'): String;
function MakePrintable(const AString: String): String; // Make a pascal like string
function UnEscapeBackslashed(const AValue: String; AFlags: TGdbUnEscapeFlags = [uefOctal]; ATabWidth: Integer = 0): String;
function UnQuote(const AValue: String): String;
function Quote(const AValue: String; AForce: Boolean=False): String;
function ConvertGdbPathAndFile(const AValue: String): String; // fix path, delim, unescape, and to utf8
function ParseGDBString(const AValue: String): String; // remove quotes(') and convert #dd chars: #9'ab'#9'x'
function GetLeadingAddr(var AValue: String; out AnAddr: TDBGPtr; ARemoveFromValue: Boolean = False): Boolean;
function UpperCaseSymbols(s: string): string;
function ConvertPascalExpression(var AExpression: String): Boolean;

procedure SmartWriteln(const s: string);

function PCLenPartToString(const AVal: TPCharWithLen; AStartOffs, ALen: Integer): String;
function PCLenToString(const AVal: TPCharWithLen; UnQuote: Boolean = False): String;
function PCLenToInt(const AVal: TPCharWithLen; Def: Integer = 0): Integer;
function PCLenToQWord(const AVal: TPCharWithLen; Def: QWord = 0): QWord;
function DbgsPCLen(const AVal: TPCharWithLen): String;

function DPtrMin(const a,b: TDBGPtr): TDBGPtr;
function DPtrMax(const a,b: TDBGPtr): TDBGPtr;

implementation

uses
  SysUtils;

{ SmartWriteln: }
var
  LastSmartWritelnStr: string;
  LastSmartWritelnCount: integer;
  LastSmartWritelnTime: double;

procedure SmartWriteln(const s: string);
var
  TimeDiff: TTimeStamp;
  i: Integer;
begin
  if (LastSmartWritelnCount>0) and (s=LastSmartWritelnStr) then begin
    TimeDiff:=DateTimeToTimeStamp(Now-LastSmartWritelnTime);
    if TimeDiff.Time<1000 then begin
      // repeating too fast
      inc(LastSmartWritelnCount);
      // write every 2nd, 4th, 8th, 16th, ... time
      i:=LastSmartWritelnCount;
      while (i>0) and ((i and 1)=0) do begin
        i:=i shr 1;
        if i=1 then begin
          DebugLn('Last message repeated %d times: "%s"',
            [LastSmartWritelnCount, LastSmartWritelnStr]);
          break;
        end;
      end;
      exit;
    end;
  end;
  LastSmartWritelnTime:=Now;
  LastSmartWritelnStr:=s;
  LastSmartWritelnCount:=1;
  DebugLn(LastSmartWritelnStr);
end;

function GetLine(var ABuffer: String): String;
var
  idx: Integer;
begin
  idx := Pos(#10, ABuffer);
  if idx = 0
  then Result := ''
  else begin
    Result := Copy(ABuffer, 1, idx);
    Delete(ABuffer, 1, idx);
  end;
end;

function ConvertToCString(const AText: String): String;
var
  srclen, dstlen, newlen: Integer;
  src, dst: PChar;
begin
  srclen := Length(AText);
  Setlength(Result, srclen);
  dstlen := srclen;
  src := @AText[1];
  dst := @Result[1];
  newlen := 0;
  while srclen > 0 do
  begin
    if newlen >= dstlen
    then begin
      Inc(dstlen, 8);
      SetLength(Result, dstlen);
      dst := @Result[newlen+1];
    end;
    case Src[0] of
      '''': begin
        if (srclen > 2) and (Src[1] = '''')
        then begin
          Inc(src);
          Dec(srclen);
          Continue;
        end;
        dst^ := '"';
      end;
      '"': begin
        if newlen+1 >= dstlen
        then begin
          Inc(dstlen, 8);
          SetLength(Result, dstlen);
          dst := @Result[newlen+1];
        end;
        dst^ := '"';
        Inc(dst);
        Inc(newlen);
        dst^ := '"';
      end;
    else
      dst^ := src^;
    end;
    Inc(src);
    Inc(dst);
    Inc(newlen);
    Dec(srclen);
  end;
  SetLength(Result, newlen);
end;

function ConvertPathDelims(const AFileName: String): String;
var
  i: Integer;
begin
  Result := AFileName;
  for i := 1 to length(Result) do
    if Result[i] in ['/','\'] then
      Result[i] := PathDelim;
end;

function MakePrintable(const AString: String): String; // Todo: Check invalid utf8
// Astring should not have quotes
var
  n, l, u: Integer;
  InString: Boolean;

  procedure ToggleInString;
  begin
    InString := not InString;
    Result := Result + '''';
  end;

begin
  Result := '';
  InString := False;
  n := 1;
  l := Length(AString);
  while n <= l do
  //for n := 1 to Length(AString) do
  begin
    case AString[n] of
      ' '..#127: begin
        if not InString then
          ToggleInString;
        Result := Result + AString[n];
        if AString[n] = '''' then Result := Result + '''';
      end;
    #192..#255: begin // Maybe utf8
        u := UTF8CodepointSize(@AString[n]);
        if (u > 0) and (n+u-1 <= l) then begin
          if not InString then
            ToggleInString;
          Result := Result + copy(AString, n, u);
          n := n + u - 1;
        end
        else begin
          if InString then
            ToggleInString;
          Result := Result + Format('#%d', [Ord(AString[n])]);
        end;
      end;
    else
      if InString then
        ToggleInString;
      Result := Result + Format('#%d', [Ord(AString[n])]);
    end;
    inc(n);
  end;
  if InString
  then Result := Result + '''';
end;

function UnEscapeBackslashed(const AValue: String; AFlags: TGdbUnEscapeFlags = [uefOctal]; ATabWidth: Integer = 0): String;
var
  c, cnt, len: Integer;
  Src, Dst: PChar;
begin
  len := Length(AValue);
  if len = 0 then Exit('');

  Src := @AValue[1];
  cnt := len;
  SetLength(Result, len); // allocate initial space

  Dst := @Result[1];
  while cnt > 0 do
  begin
    if (Src^ = '\') then begin
      case (Src+1)^ of
        '\' :
          begin
            inc(Src);
            dec(cnt);
          end;
        '0'..'7' :
          if uefOctal in AFlags then begin
            inc(Src);
            dec(cnt);
            c := 0;
            while (Src^ in ['0'..'7']) and (cnt > 0)
            do begin
              c := (c * 8) + ord(Src^) - ord('0');
              Inc(Src);
              Dec(cnt);
            end;
            //c := UnicodeToUTF8SkipErrors(c, Dst);
            //inc(Dst, c);
            Dst^ := chr(c and 255);
            if (c and 255) <> 0
            then Inc(Dst);
            if cnt = 0 then Break;
            continue;
          end;
        'n' :
          if uefNewLine in AFlags then begin
            inc(Src, 2);
            dec(cnt, 2);
            Dst^ := #10;
            Inc(Dst);
            continue;
          end;
        'r' :
          if uefNewLine in AFlags then begin
            inc(Src, 2);
            dec(cnt, 2);
            Dst^ := #13;
            Inc(Dst);
            continue;
          end;
        't' :
          if uefTab in AFlags then begin
            inc(Src, 2);
            dec(cnt, 2);
            if ATabWidth > 0 then begin;
              c := Dst - @Result[1];
              if Length(Result) < c + cnt + ATabWidth + 1 then begin
                SetLength(Result, Length(Result) + ATabWidth);
                Dst := @Result[1] + c;
              end;
              repeat
                Dst^ := ' ';
                Inc(Dst);
              until ((Dst - @Result[1]) mod ATabWidth) = 0;
            end
            else begin
              Dst^ := #9;
              Inc(Dst);
            end;
            continue;
          end;
      end;
    end;
    Dst^ := Src^;
    Inc(Dst);
    Inc(Src);
    Dec(cnt);
  end;

  SetLength(Result, Dst - @Result[1]); // adjust to actual length
end;

function UnQuote(const AValue: String): String;
var
  len: Integer;
begin
  len := Length(AValue);
  if  len < 2 then Exit(AValue);

  if (AValue[1] = '"') and (AValue[len] = '"')
  then Result := Copy(AValue, 2, len - 2)
  else Result := AValue;
end;

function Quote(const AValue: String; AForce: Boolean): String;
begin
  if (pos(' ', AValue) < 1) and (pos(#9, AValue) < 1) and (not AForce) then
    exit(AValue);
  Result := '"' + StringReplace(AValue, '"', '\"', [rfReplaceAll]) + '"';
end;

function ConvertGdbPathAndFile(const AValue: String): String;
begin
  Result := AnsiToUtf8(ConvertPathDelims(UnEscapeBackslashed(AValue, [uefOctal])));
end;

function ParseGDBString(const AValue: String): String;
var
  i, j, v: Integer;
  InQuote: Boolean;
begin
  if AValue = '' then exit('');

  SetLength(Result, length(AValue));
  j := 0;
  i := 0;
  InQuote := False;

  if copy(AValue,1,2) = '0x' then begin
    // skip leading address: 0x010aa00 'abc'
    i := 2;
    while (i < length(AValue)) and (AValue[i+1] in ['0'..'9', 'a'..'f', 'A'..'F']) do inc(i);
    while (i < length(AValue)) and (AValue[i+1] in [' ']) do inc(i);
  end;

  while i < length(AValue) do begin
    inc(i);
    If AValue[i] = '''' then begin
      if InQuote and (i < length(AValue)) and (AValue[i+1] = '''') then begin
        inc(i);
        inc(j);
        Result[j] := '''';
      end
      else begin
        InQuote := not InQuote;
      end;
      continue;
    end;
    if (AValue[i] = '\' ) and (i < length(AValue)) then begin // gdb escapes some chars, even it not pascalish
      inc(j);
      inc(i); // copy next char
      Result[j] := AValue[i];
      continue;
    end;
    if InQuote or not(AValue[i] = '#' ) then begin
      inc(j);
      Result[j] := AValue[i];
      continue;
    end;
    // must be #
    v := 0;
    inc(i);
    while (i < length(AValue)) and (AValue[i] in ['0'..'9']) do begin
      v:= v * 10 + ord(AValue[i]) - ord('0');
      inc(i);
    end;
    inc(j);
    Result[j] := chr(v and 255);
  end;
  SetLength(Result, j);
end;

function GetLeadingAddr(var AValue: String; out AnAddr: TDBGPtr;
  ARemoveFromValue: Boolean): Boolean;
var
  i, e: Integer;
begin
  AnAddr := 0;
  Result := (length(AValue) >= 2) and (AValue[1] = '0') and (AValue[2] = 'x');

  if not Result then exit;

  i := 2;
  while (i < length(AValue)) and (AValue[i+1] in ['0'..'9', 'a'..'f', 'A'..'F']) do inc(i);
  Result := i > 2;
  if not Result then exit;

  Val(copy(AValue,1 , i), AnAddr, e);
  Result := e = 0;
  if not Result then exit;

  if ARemoveFromValue then begin
    if (i < length(AValue)) and (AValue[i+1] in [' ']) then inc(i);
    delete(AValue, 1, i);
  end;
end;

function UpperCaseSymbols(s: string): string;
var
  i, l: Integer;
begin
  Result := s;
  i := 1;
  l := Length(Result);
  while (i <= l) do begin
    if Result[i] = '''' then begin
      inc(i);
      while (i <= l) and (Result[i] <> '''') do
        inc(i);
    end
    else
    if Result[i] = '"' then begin
      inc(i);
      while (i < l) and (Result[i] <> '"') do
        inc(i);
    end;
    (* uppercase due to https://sourceware.org/bugzilla/show_bug.cgi?id=17835
       gdb 7.7 and 7.8 fail to find members, if lowercased
       Alternative prefix with "self." if gdb returns &"Type TCLASSXXXX has no component named EXPRESSION.\n"
    *)
    if (i<=l) and  (Result[i] in ['a'..'z']) then
      Result[i] := UpperCase(Result[i])[1];
    inc(i);
  end;
end;

function ConvertPascalExpression(var AExpression: String): Boolean;
var
  QuoteChar, R: String;
  P: PChar;
  InString, WasString, IsText, ValIsChar: Boolean;
  n: Integer;
  ValMode: Char;
  Value: QWord;

  function AppendValue: Boolean;
  var
    S: String;
  begin
    if ValMode = #0 then Exit(True);
    if not (ValMode in ['h', 'd', 'o', 'b']) then Exit(False);

    if ValIsChar
    then begin
      if not IsText
      then begin
        R := R + '"';
        IsText := True;
      end;
      R := R + '\' + OctStr(Value, 3);
      ValIsChar := False;
    end
    else begin
      if IsText
      then begin
        R := R + '"';
        IsText := False;
      end;
      Str(Value, S);
      R := R + S;
    end;
    Result := True;
    ValMode := #0;
  end;

begin
  R := '';
  Instring := False;
  WasString := False;
  IsText := False;
  QuoteChar := '"';
  ValIsChar := False;
  ValMode := #0;
  Value := 0;

  P := PChar(AExpression);
  for n := 1 to Length(AExpression) do
  begin
    if InString
    then begin
      case P^ of
        '''': begin
          InString := False;
          // delay setting terminating ", more characters defined through # may follow
          WasString := True;
        end;
        #0..#31,
        '\',
        #128..#255: begin
          R := R + '\' + OctStr(Ord(P^), 3);
        end;
      else begin
          if p^ = QuoteChar then
            R := R + '\' + OctStr(Ord(P^), 3)
          else
            R := R + P^;
        end;
      end;
      Inc(P);
      Continue;
    end;

    case P^ of
      '''': begin
        if WasString
        then begin
          R := R + '\' + OctStr(Ord(''''), 3)
        end
        else begin
          if not AppendValue then Exit(False);
          if not IsText
          then begin
            QuoteChar := '"';
            // single CHAR ?
            if ( ((p+1)^ <> '''') and ((p+2)^ = '''') and not((p+3)^ in ['#', '''']) ) or
               ( ((p+1)^ = '''') and ((p+2)^ = '''') and ((p+3)^ = '''') and not((p+4)^ in ['#', '''']) )
            then
              QuoteChar := '''';
            R := R + QuoteChar;
          end
        end;
        IsText := True;
        InString := True;
      end;
      '#': begin
        if not AppendValue then Exit(False);
        Value := 0;
        ValMode := 'D';
        ValIsChar := True;
      end;
      '$', '&', '%': begin
        if not (ValMode in [#0, 'D']) then Exit(False);
        ValMode := P^;
      end;
    else
      case ValMode of
        'D', 'd': begin
          case P^ of
            '0'..'9': Value := Value * 10 + Ord(P^) - Ord('0');
          else
            Exit(False);
          end;
          ValMode := 'd';
        end;
        '$', 'h': begin
          case P^ of
            '0'..'9': Value := Value * 16 + Ord(P^) - Ord('0');
            'a'..'f': Value := Value * 16 + Ord(P^) - Ord('a');
            'A'..'F': Value := Value * 16 + Ord(P^) - Ord('A');
          else
            Exit(False);
          end;
          ValMode := 'h';
        end;
        '&', 'o': begin
          case P^ of
            '0'..'7': Value := Value * 8 + Ord(P^) - Ord('0');
          else
            Exit(False);
          end;
          ValMode := 'o';
        end;
        '%', 'b': begin
          case P^ of
            '0': Value := Value shl 1;
            '1': Value := Value shl 1 or 1;
          else
            Exit(False);
          end;
          ValMode := 'b';
        end;
      else
        if IsText
        then begin
          R := R + QuoteChar;
          IsText := False;
        end;
        R := R + P^;
      end;
    end;
    WasString := False;
    Inc(p);
  end;

  if not AppendValue then Exit(False);
  if IsText then R := R + QuoteChar;
  AExpression := R;
  Result := True;
end;

function DeleteEscapeChars(const AValue: String; const AEscapeChar: Char): String;
var
  cnt, len: Integer;
  Src, Dst: PChar;
begin
  len := Length(AValue);
  if len = 0 then Exit('');

  Src := @AValue[1];
  cnt := len;
  SetLength(Result, len); // allocate initial space

  Dst := @Result[1];
  while cnt > 0 do
  begin
    if Src^ = AEscapeChar
    then begin
      Dec(len);
      Dec(cnt);
      if cnt = 0 then Break;
      Inc(Src);
    end;
    Dst^ := Src^;
    Inc(Dst);
    Inc(Src);
    Dec(cnt);
  end;

  SetLength(Result, len); // adjust to actual length
end;

{ TPCharWithLen }

function PCLenPartToString(const AVal: TPCharWithLen; AStartOffs, ALen: Integer): String;
begin
  if AStartOffs + ALen > AVal.Len
  then ALen := AVal.Len - AStartOffs;
  if ALen <= 0
  then exit('');

  SetLength(Result, ALen);
  Move((AVal.Ptr+AStartOffs)^, Result[1], aLen)
end;

function PCLenToString(const AVal: TPCharWithLen; UnQuote: Boolean = False): String;
begin
  if UnQuote and (AVal.Len >= 2) and (AVal.Ptr[0] = '"') and (AVal.Ptr[AVal.Len-1] = '"')
  then begin
    SetLength(Result, AVal.Len - 2);
    if AVal.Len > 2
    then Move((AVal.Ptr+1)^, Result[1], AVal.Len - 2)
  end
  else begin
    SetLength(Result, AVal.Len);
    if AVal.Len > 0
    then Move(AVal.Ptr^, Result[1], AVal.Len)
  end;
end;

function PCLenToInt(const AVal: TPCharWithLen; Def: Integer = 0): Integer;
begin
  Result := StrToIntDef(PCLenToString(AVal, True), Def);
end;

function PCLenToQWord(const AVal: TPCharWithLen; Def: QWord = 0): QWord;
begin
  Result := StrToQWordDef(PCLenToString(AVal, True), Def);
end;

function DbgsPCLen(const AVal: TPCharWithLen): String;
begin
  Result := PCLenToString(AVal);
end;

function DPtrMin(const a, b: TDBGPtr): TDBGPtr;
begin
  if a < b then Result := a else Result := b;
end;

function DPtrMax(const a, b: TDBGPtr): TDBGPtr;
begin
  if a > b then Result := a else Result := b;
end;


initialization
  LastSmartWritelnCount:=0;

end.

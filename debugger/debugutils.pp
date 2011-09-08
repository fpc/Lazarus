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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit DebugUtils;

{$mode objfpc}{$H+}

interface 

uses
  Classes, LCLProc;

type

  { TDelayedUdateItem }

  TDelayedUdateItem = class(TCollectionItem)
  private
    FUpdateCount: Integer;
    FDoChanged: Boolean;
  protected
    procedure Changed;
    procedure DoChanged; virtual;
    procedure DoEndUpdate; virtual; // even if not changed
  public
    procedure Assign(ASource: TPersistent); override;
    procedure BeginUpdate;
    constructor Create(ACollection: TCollection); override;
    procedure EndUpdate;
    function IsUpdating: Boolean;
  end;


  TPCharWithLen = record
    Ptr: PChar;
    Len: Integer;
  end;

  TGdbUnEscapeFlags = set of (uefOctal, uefTab, uefNewLine);

function GetLine(var ABuffer: String): String;
function ConvertToCString(const AText: String): String;
function ConvertPathDelims(const AFileName: String): String;
function DeleteEscapeChars(const AValue: String; const AEscapeChar: Char = '\'): String;
function UnEscapeBackslashed(const AValue: String; AFlags: TGdbUnEscapeFlags = [uefOctal]; ATabWidth: Integer = 0): String;
function UnQuote(const AValue: String): String;
function ConvertGdbPathAndFile(const AValue: String): String; // fix path, delim, unescape, and to utf8
function ParseGDBString(const AValue: String): String; // remove quotes(') and convert #dd chars: #9'ab'#9'x'

procedure SmartWriteln(const s: string);

function PCLenPartToString(const AVal: TPCharWithLen; AStartOffs, ALen: Integer): String;
function PCLenToString(const AVal: TPCharWithLen; UnQuote: Boolean = False): String;
function PCLenToInt(const AVal: TPCharWithLen; Def: Integer = 0): Integer;
function PCLenToQWord(const AVal: TPCharWithLen; Def: QWord = 0): QWord;
function DbgsPCLen(const AVal: TPCharWithLen): String;

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

function Unquote(const AValue: String): String;
var
  len: Integer;
begin
  len := Length(AValue);
  if  len < 2 then Exit(AValue);

  if (AValue[1] = '"') and (AValue[len] = '"')
  then Result := Copy(AValue, 2, len - 2)
  else Result := AValue;
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
      InQuote := not InQuote;
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



{ TDelayedUdateItem }

procedure TDelayedUdateItem.Assign(ASource: TPersistent);
begin
  BeginUpdate;
  try
    inherited Assign(ASource);
  finally
    EndUpdate;
  end;
end;

procedure TDelayedUdateItem.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then FDoChanged := False;
end;

procedure TDelayedUdateItem.Changed;
begin
  if FUpdateCount > 0
  then FDoChanged := True
  else DoChanged;
end;

constructor TDelayedUdateItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUpdateCount := 0;
end;

procedure TDelayedUdateItem.DoChanged;
begin
  inherited Changed(False);
end;

procedure TDelayedUdateItem.DoEndUpdate;
begin
  //
end;

procedure TDelayedUdateItem.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then raise EInvalidOperation.Create('TDelayedUdateItem.EndUpdate');
  if (FUpdateCount = 0)
  then DoEndUpdate;
  if (FUpdateCount = 0) and FDoChanged
  then begin
    DoChanged;
    FDoChanged := False;
  end;
end;

function TDelayedUdateItem.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

initialization
  LastSmartWritelnCount:=0;

end.

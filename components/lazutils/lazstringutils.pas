{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Functions for string manipulation.

}
unit LazStringUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function LineEndingCount(const Txt: string; var LengthOfLastLine: integer): integer;
function ChangeLineEndings(const s, NewLineEnding: string): string;


implementation

function LineEndingCount(const Txt: string; var LengthOfLastLine: integer): integer;
var
  i, LastLineEndPos, TxtLen: integer;
begin
  i:=1;
  LastLineEndPos:=0;
  Result:=0;
  TxtLen:=length(Txt);
  while i<TxtLen do begin
    if (Txt[i] in [#10,#13]) then begin
      inc(Result);
      inc(i);
      if (i<=TxtLen) and (Txt[i] in [#10,#13]) and (Txt[i-1]<>Txt[i]) then
        inc(i);
      LastLineEndPos:=i-1;
    end else
      inc(i);
  end;
  LengthOfLastLine:=TxtLen-LastLineEndPos;
end;

function ChangeLineEndings(const s, NewLineEnding: string): string;
var
  p, NewLength, EndLen: Integer;
  Src, Dest, EndPos: PChar;
begin
  if s='' then begin
    Result:=s;
    exit;
  end;
  EndLen:=length(NewLineEnding);
  NewLength:=length(s);
  Src:=PChar(s);
  repeat
    case Src^ of
    #0:
      if Src-PChar(s)=length(s) then
        break
      else
        inc(Src);
    #10,#13:
      begin
        if (Src[1] in [#10,#13]) and (Src^<>Src[1]) then begin
          inc(Src,2);
          inc(NewLength,EndLen-2);
        end else begin
          inc(Src);
          inc(NewLength,EndLen-1);
        end;
      end;
    else
      inc(Src);
    end;
  until false;
  SetLength(Result,NewLength);
  Src:=PChar(s);
  Dest:=PChar(Result);
  EndPos:=Dest+NewLength;
  while (Dest<EndPos) do begin
    if Src^ in [#10,#13] then begin
      for p:=1 to EndLen do begin
        Dest^:=NewLineEnding[p];
        inc(Dest);
      end;
      if (Src[1] in [#10,#13]) and (Src^<>Src[1]) then
        inc(Src,2)
      else
        inc(Src);
    end else begin
      Dest^:=Src^;
      inc(Src);
      inc(Dest);
    end;
  end;
  //if Src-1<>@s[length(s)] then RaiseGDBException('');
end;

end.


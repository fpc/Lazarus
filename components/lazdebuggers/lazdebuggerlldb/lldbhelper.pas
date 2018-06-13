unit LldbHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

function LastPos(ASearch, AString: string): Integer;

function StrStartsWith(AString, AStart: string): Boolean;
function StrContains(AString, AFind: string): Boolean;
function StrMatches(AString: string; AFind: array of string): Boolean;
function StrMatches(AString: string; AFind: array of string; out AGapsContent: TStringArray): Boolean;

implementation

function LastPos(ASearch, AString: string): Integer;
var
  i: Integer;
begin
  i := pos(ASearch, AString);
  Result := i;
  while i > 0 do begin
    Result := i;
    i := PosEx(ASearch, AString, i + 1);
  end;
end;

function StrStartsWith(AString, AStart: string): Boolean;
begin
  Result := LeftStr(AString, Length(AStart)) = AStart;
end;

function StrContains(AString, AFind: string): Boolean;
begin
  Result := pos(AFind, AString) > 0;
end;

function StrMatches(AString: string; AFind: array of string): Boolean;
var
  Dummy: TStringArray;
begin
  Result := StrMatches(AString, AFind, Dummy);
end;

function StrMatches(AString: string; AFind: array of string; out
  AGapsContent: TStringArray): Boolean;
var
  FindIdx, FindLen, j, j2, ResIdx: Integer;
  OpenEnd: Boolean;
begin
  FindLen := Length(AFind);
  if FindLen = 0 then begin
    Result := False;
    AGapsContent := nil;
    exit;
  end;

  SetLength(AGapsContent, FindLen - 1);
  Result := StrStartsWith(AString, AFind[0]);
  if not Result then
    exit;
  Delete(AString, 1, Length(AFind[0]));

  OpenEnd := AFind[FindLen - 1] = '';
  if OpenEnd then
    dec(FindLen);

  FindIdx := 1;
  ResIdx := 0;
  while (FindIdx < FindLen) do begin
    if AFind[FindIdx] = '' then begin
      // empty string, match as far ahead as possible
      inc(FindIdx);
      j := LastPos(AFind[FindIdx], AString) - 1;
    end
    else
      j := pos(AFind[FindIdx], AString) - 1;
    Result := j >= 0;
    if not Result then
      exit;
    AGapsContent[ResIdx] := copy(AString, 1, j);
    Delete(AString, 1, j + Length(AFind[FindIdx]));
    inc(FindIdx);
    inc(ResIdx);
  end;
  if OpenEnd then begin
    AGapsContent[ResIdx] := AString;
    inc(ResIdx);
  end
  else
    Result := AString = '';
  SetLength(AGapsContent, ResIdx);
end;

end.


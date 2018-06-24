unit LldbHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, DbgIntfBaseTypes, strutils;

function LastPos(ASearch, AString: string): Integer;

function StrStartsWith(AString, AStart: string): Boolean;
function StrContains(AString, AFind: string): Boolean;
function StrMatches(AString: string; AFind: array of string): Boolean;
function StrMatches(AString: string; AFind: array of string; out AGapsContent: TStringArray): Boolean;

function ParseThreadLocation(AnInput: String; out AnId: Integer;
  out AnIsCurrent: Boolean; out AName: String; out AnAddr: TDBGPtr;
  out AFuncName: String; out AnArgs: TStringList; out AFile: String;
  out ALine: Integer; out AReminder: String): Boolean;
function ParseFrameLocation(AnInput: String; out AnId: Integer;
  out AnIsCurrent: Boolean; out AnAddr: TDBGPtr; out AFuncName: String;
  out AnArgs: TStringList; out AFile: String; out ALine: Integer;
  out AReminder: String): Boolean;
function ParseNewFrameLocation(AnInput: String; out AnId: Integer;
  out AnIsCurrent: Boolean; out AnAddr: TDBGPtr; out AFuncName: String;
  out AnArgs: TStringList; out AFile, AFullFile: String; out ALine: Integer;
  out AReminder: String): Boolean;

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

(* Examples
"* thread #1: tid = 0x1a1c, 0x0042951d project1.exe`FORMCREATE(this=0x00151060, SENDER=0x00151060) at unit1.pas:59, stop reason = breakpoint 2.1"
"  thread #2: tid = 0x16ac, 0x7700eb6c ntdll.dll`NtDelayExecution + 12"

"  * frame #0: 0x7700eb6c ntdll.dll`NtDelayExecution + 12"
"    frame #1: 0x767a5f5b KernelBase.dll`SleepEx + 155"
"    frame #3: 0x004521c9 project1.exe"
"    frame #7: 0x761a8654 kernel32.dll`BaseThreadInitThunk + 36"
"    frame #2: 0x048fa158"
"    frame #1: 0x0041ab6f project1.exe`DOCREATE(this=0x04a91060) at customform.inc:939"
"    frame #5: 0x00402a42 project1.exe`main at project1.lpr:19"
*)

procedure ParseLocation(AnInput: String; out AnAddr: TDBGPtr; out AFuncName: String;
  out AnArgs: TStringList; out AFile: String; out ALine: Integer; out AReminder: String);
var
  found: TStringArray;
  i, j, k: Integer;
begin
  if pos(' ', AnInput) = 0 then begin
    AnAddr := StrToInt64Def(AnInput, 0);
    AnInput := '';
  end
  else
  if StrMatches(AnInput, [''{addr}, ' '{remainder}, ''], found) then begin
    AnAddr := StrToInt64Def(found[0], 0);
    AnInput := found[1];
  end
  else
    AnAddr := 0;

  AnArgs := nil;
  AFile := '';
  ALine := 0;
  AReminder := '';
  if StrMatches(AnInput, [''{exe}, '`'{remainder}, ''], found) then begin
    AnInput := found[1];
    i := pos(' ', AnInput);
    j := pos('(', AnInput);
    k := pos(')', AnInput);
    if ((i = 0) or (i > j)) and (j > 1) and (k > j) then begin
      AFuncName := Copy(AnInput, 1, j-1);
      AnArgs := TStringList.Create;
      AnArgs.CommaText := copy(AnInput, j+1, k-j-1);
      AnInput := Copy(AnInput, k+1, Length(AnInput));
    end
    else begin
      i := Max(i, pos(', ', AnInput));
      if i = 0 then i := Length(AnInput) + 1;
      AFuncName := Copy(AnInput, 1, i-1);
      AnInput := Copy(AnInput, i, Length(AnInput));
    end;

    if StrStartsWith(AnInput, ' + ') and (Length(AnInput) >= 4) and (AnInput[4] in ['0'..'9']) then begin
      i := 4;
      while (Length(AnInput) > i) and (AnInput[i+1] in ['0'..'9']) do inc(i);
      delete(AnInput, 1, i);
    end;

    if StrMatches(AnInput, [' at ', ':', ''], found) then begin
      AFile := found[0];
      i := pos(', ', found[1]);
      if i = 0 then i := Length(found[1]) + 1;
      ALine := StrToIntDef(copy(found[1], 1, i-1), 0);
      AReminder := copy(found[1], i, Length(found[1]));
    end
    else
      AReminder := AnInput;
  end
  else begin
    AFuncName := AnInput;
  end;
end;

function ParseThreadLocation(AnInput: String; out AnId: Integer; out
  AnIsCurrent: Boolean; out AName: String; out AnAddr: TDBGPtr; out
  AFuncName: String; out AnArgs: TStringList; out AFile: String; out
  ALine: Integer; out AReminder: String): Boolean;
var
  found: TStringArray;
begin
  Result := False;
  AnIsCurrent := (Length(AnInput) > 1) and (AnInput[1] = '*');
  if AnIsCurrent then AnInput[1] := ' ';

  if not StrMatches(AnInput, ['  thread #'{id}, ': '{}, ''], found) then begin
    AnId := -1;
    AName := '';
    ParseLocation('', AnAddr, AFuncName, AnArgs, AFile, ALine, AReminder);
    exit;
  end;

  AnId := StrToIntDef(found[0], -1);
  AnInput := found[1];
  Result := True;

  if StrMatches(AnInput, ['tid = '{tid}, ', '{}, ''], found) then begin
    AName := found[0];
    AnInput := found[1];
  end
  else
    AName := '';

  ParseLocation(AnInput, AnAddr, AFuncName, AnArgs, AFile, ALine, AReminder);
end;

function ParseFrameLocation(AnInput: String; out AnId: Integer; out
  AnIsCurrent: Boolean; out AnAddr: TDBGPtr; out AFuncName: String; out
  AnArgs: TStringList; out AFile: String; out ALine: Integer; out
  AReminder: String): Boolean;
var
  found: TStringArray;
begin
  Result := False;
  AnIsCurrent := (Length(AnInput) > 3) and (AnInput[3] = '*');
  if AnIsCurrent then AnInput[3] := ' ';

  if not StrMatches(AnInput, ['    frame #'{id}, ': '{}, ''], found) then begin
    AnId := -1;
    ParseLocation('', AnAddr, AFuncName, AnArgs, AFile, ALine, AReminder);
    exit;
  end;

  AnId := StrToIntDef(found[0], -1);
  AnInput := found[1];
  Result := True;

  ParseLocation(AnInput, AnAddr, AFuncName, AnArgs, AFile, ALine, AReminder);
end;

function ParseNewFrameLocation(AnInput: String; out AnId: Integer; out
  AnIsCurrent: Boolean; out AnAddr: TDBGPtr; out AFuncName: String; out
  AnArgs: TStringList; out AFile, AFullFile: String; out ALine: Integer; out
  AReminder: String): Boolean;
var
  found: TStringArray;
  i, j, k: SizeInt;
begin
  Result := False;
  AnIsCurrent := (Length(AnInput) > 3) and (AnInput[3] = '*');
  if AnIsCurrent then AnInput[3] := ' ';

  if not StrMatches(AnInput, ['    frame #'{id}, ': '{addr},
    ' &&//FULL: '{fullfile}, ' &&//SHORT: '{file},' &&//LINE: '{line},
    ' &&//MOD: '{mod},' &&//FUNC: '{func}, '',' <<&&//FRAME', ''
     ], found) then begin
    AnId := -1;
    AnAddr :=    0;
    AFile :=     '';
    AFullFile := '';
    ALine :=     -1;
    AFuncName := '';
    AReminder := '';
    AnArgs := nil;
    exit;
  end;

  AnId :=      StrToIntDef(found[0], -1);
  AnAddr :=    StrToInt64Def(found[1], 0);
  AFullFile := found[2];
  AFile :=     found[3];
  ALine :=     StrToIntDef(found[4], -1);
  AFuncName := found[6];
  AnArgs := nil;
  AReminder := found[7];

  if AFuncName = '' then begin
    AFuncName := '<'+found[5]+'>';
  end
  else begin
    AnInput := AFuncName;
    i := pos(' ', AnInput);
    j := pos('(', AnInput);
    k := pos(')', AnInput);
    if ((i = 0) or (i > j)) and (j > 1) and (k > j) then begin
      AFuncName := Copy(AnInput, 1, j-1);
      AnArgs := TStringList.Create;
      AnArgs.CommaText := copy(AnInput, j+1, k-j-1);
      AnInput := Copy(AnInput, k+1, Length(AnInput));
    end;
  end;

  Result := True;
end;

end.


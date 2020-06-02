{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscProcs.pas, released 2000-04-07.
The Original Code is based on the mwSupportProcs.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditMiscProcs;

{$I synedit.inc}

interface

uses
  LCLIntf, LCLType, Classes, SynEditTypes, Graphics;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxListSize - 1] of integer;

function MulDiv(Factor1,Factor2,Divisor:integer):integer;{$IFDEF HasInline}inline;{$ENDIF}
function Max(x, y: integer): integer;{$IFDEF HasInline}inline;{$ENDIF}
function Min(x, y: integer): integer;{$IFDEF HasInline}inline;{$ENDIF}
function MinMax(x, mi, ma: integer): integer;{$IFDEF HasInline}inline;{$ENDIF}
procedure SwapInt(var l, r: integer);{$IFDEF HasInline}inline;{$ENDIF}
function maxPoint(P1, P2: TPoint): TPoint;
function minPoint(P1, P2: TPoint): TPoint;
function eqPoint(P1, P2: TPoint): Boolean;
procedure SwapPoint(var P1, P2: TPoint);

procedure InternalFillRect(dc: HDC; const rcPaint: TRect);

// search for the first char of set AChars in Line, starting at index Start
function StrScanForCharInSet(const Line: string; Start: integer;
  AChars: TSynIdentChars): integer;

function GetEOL(Line: PChar): PChar;

function CompareCarets(const FirstCaret, SecondCaret: TPoint): integer;

function fsNot (s : TFontStyles) : TFontStyles; inline;
function fsXor (s1,s2 : TFontStyles) : TFontStyles; inline;

function CreateTabsAndSpaces(StartPos, SpaceLen, TabWidth: integer;
  UseTabs: boolean): string;

procedure SynAssert(Condition: Boolean; Msg: String);
procedure SynAssert(Condition: Boolean; Msg: String; Args: Array of Const);

function ToIdx(APos: Integer): Integer; inline;
function ToPos(AIdx: Integer): Integer; inline;
function YToIdx(APointWithYPos: TPoint): TPoint; inline;
function YToPos(APointWithYIdx: TPoint): TPoint; inline;

implementation

uses
  SysUtils;

function ToIdx(APos: Integer): Integer; inline;
begin
  Result := APos - 1;
end;

function ToPos(AIdx: Integer): Integer; inline;
begin
  Result := AIdx + 1;
end;

function YToIdx(APointWithYPos: TPoint): TPoint; inline;
begin
  Result := APointWithYPos;
  dec(Result.Y);
end;

function YToPos(APointWithYIdx: TPoint): TPoint; inline;
begin
  Result := APointWithYIdx;
  inc(Result.Y);
end;

{* fontstyle utilities *}

function fsNot (s : TFontStyles) : TFontStyles; inline;
begin
  Result := [low(TFontStyle)..High(TFontStyle)] - s;
end;
function fsXor (s1,s2 : TFontStyles) : TFontStyles; inline;
begin
  Result := s1 + s2 - (s1*s2);
end;

{***}

function MulDiv(Factor1,Factor2,Divisor:integer):integer;
begin
  Result:=(int64(Factor1)*int64(Factor2)) div Divisor;
end;

function Max(x, y: integer): integer;
begin
  if x > y then Result := x else Result := y;
end;

function Min(x, y: integer): integer;
begin
  if x < y then Result := x else Result := y;
end;

function MinMax(x, mi, ma: integer): integer;
begin
  if (x < mi) then Result := mi
    else if (x > ma) then Result := ma else Result := x;
end;

procedure SwapInt(var l, r: integer);
var
  tmp: integer;
begin
  tmp := r;
  r := l;
  l := tmp;
end;

function maxPoint(P1, P2: TPoint): TPoint;
begin
  Result := P1;
  if (P2.y > P1.y) or ((P2.y = P1.y) and (P2.x > P1.x)) then
    Result := P2;
end;

function minPoint(P1, P2: TPoint): TPoint;
begin
  Result := P1;
  if (P2.y < P1.y) or ((P2.y = P1.y) and (P2.x < P1.x)) then
    Result := P2;
end;

function eqPoint(P1, P2: TPoint): Boolean;
begin
  Result := (P2.y = P1.y) and (P2.x = P1.x);
end;

procedure SwapPoint(var P1, P2: TPoint);
var
  tmp : TPoint;
begin
  tmp := P1;
  P1 := P2;
  P2 := tmp;
end;

procedure InternalFillRect(dc: HDC; const rcPaint: TRect);
begin
  ExtTextOut(dc, 0, 0, ETO_OPAQUE, @rcPaint, nil, 0, nil);
end;

{***}

function StrScanForCharInSet(const Line: string; Start: integer;
  AChars: TSynIdentChars): integer;
var
  p: PChar;
begin
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    p := PChar(@Line[Start]);
    repeat
      if p^ in AChars then
      begin
        Result := Start;
        exit;
      end;
      Inc(p);
      Inc(Start);
    until p^ = #0;
  end;
  Result := 0;
end;

function GetEOL(Line: PChar): PChar;
begin
  Result := Line;
  if Assigned(Result) then
    while not (Result^ in [#0, #10, #13]) do
      Inc(Result);
end;

function CompareCarets(const FirstCaret, SecondCaret: TPoint): integer;
begin
  if (FirstCaret.Y<SecondCaret.Y) then
    Result:=1
  else if (FirstCaret.Y>SecondCaret.Y) then
    Result:=-1
  else if (FirstCaret.X<SecondCaret.X) then
    Result:=1
  else if (FirstCaret.X>SecondCaret.X) then
    Result:=-1
  else
    Result:=0;
end;

function CreateTabsAndSpaces(StartPos, SpaceLen, TabWidth: integer;
  UseTabs: boolean): string;
var
  TabCount: Integer;
  EndPos: Integer;
  PosPlusOneTab: Integer;
begin
  Result:='';
  if not UseTabs then begin
    Result:=StringOfChar(' ',SpaceLen);
    exit;
  end;
  TabCount:=0;
  EndPos:=StartPos+SpaceLen;
  while StartPos<EndPos do begin
    PosPlusOneTab:=StartPos+TabWidth-((StartPos-1) mod TabWidth);
    if PosPlusOneTab<=EndPos then begin
      inc(TabCount);
      StartPos:=PosPlusOneTab;
    end else begin
      Result:=StringOfChar(' ',EndPos-StartPos);
      break;
    end;
  end;
  if TabCount>0 then
    Result:=StringOfChar(#9,TabCount)+Result;
end;

procedure SynAssert(Condition: Boolean; Msg: String);
begin
  if not Condition then raise Exception.Create(Msg);
end;

procedure SynAssert(Condition: Boolean; Msg: String; Args: array of const);
begin
  if not Condition then raise Exception.Create(Format(Msg, Args));
end;

end.


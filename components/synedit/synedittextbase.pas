{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}
unit SynEditTextBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, SynEditTypes;

type

  { TSynEditStrings }

  TSynEditStrings = class(TStrings)
  protected
    FTabWidth: integer;
    FIsUtf8: Boolean;
    function  GetIsUtf8 : Boolean; virtual;
    procedure SetIsUtf8(const AValue : Boolean); virtual;
    function  GetTabWidth : integer; virtual;
    procedure SetTabWidth(const AValue : integer); virtual;
    function GetFoldEndLevel(Index: integer): integer; virtual; abstract;
    function GetFoldMinLevel(Index: integer): integer; virtual; abstract;
    procedure SetFoldEndLevel(Index: integer; const AValue: integer); virtual; abstract;
    procedure SetFoldMinLevel(Index: integer; const AValue: integer); virtual; abstract;
    function GetRange(Index: integer): TSynEditRange; virtual; abstract;
    procedure PutRange(Index: integer; ARange: TSynEditRange); virtual; abstract;
    function GetExpandedString(Index: integer): string; virtual; abstract;
    function GetLengthOfLongestLine: integer; virtual; abstract;
    procedure SetTextStr(const Value: string); override;
  public
    constructor Create;
    procedure DeleteLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); virtual; abstract;
    procedure ClearRanges(ARange: TSynEditRange); virtual; abstract;
  public
    // Byte to Char
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function LogicalToPhysicalCol(const Line: string;
                                  LogicalPos: integer): integer;
    function LogicalToPhysicalCol(Line: PChar; LineLen: integer;
                  LogicalPos, StartBytePos, StartPhysicalPos: integer): integer;
    // Char to Byte
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalCol(const Line: string;
                                  PhysicalPos: integer): integer;
    function PhysicalToLogicalCol(const Line: string;
                 PhysicalPos, StartBytePos, StartPhysicalPos: integer): integer;
  public
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property TabWidth: integer read GetTabWidth write SetTabWidth;
    property IsUtf8: Boolean read GetIsUtf8 write SetIsUtf8;
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property FoldMinLevel[Index: integer]: integer read GetFoldMinLevel
                                                   write SetFoldMinLevel;
    property FoldEndLevel[Index: integer]: integer read GetFoldEndLevel
                                                   write SetFoldEndLevel;
  end;


implementation


{ TSynEditStrings }

constructor TSynEditStrings.Create;
begin
  inherited Create;
  TabWidth := 8;
  IsUtf8 := True;
end;

function TSynEditStrings.GetIsUtf8 : Boolean;
begin
  Result := FIsUtf8;
end;

function TSynEditStrings.GetTabWidth : integer;
begin
  Result := FTabWidth;
end;

procedure TSynEditStrings.SetIsUtf8(const AValue : Boolean);
begin
  FIsUtf8 := AValue;
end;

procedure TSynEditStrings.SetTabWidth(const AValue : integer);
begin
  FTabWidth := AValue;
end;

procedure TSynEditStrings.SetTextStr(const Value : string);
var
  StartPos: Integer;
  p: Integer;
  Len: Integer;
  sl: TStringList;
begin
  BeginUpdate;
  sl:=TStringList.Create;
  try
    Clear;
    p:=1;
    StartPos:=p;
    Len:=length(Value);
    while p<=Len do begin
      if not (Value[p] in [#10,#13]) then begin
        inc(p);
      end else begin
        sl.Add(copy(Value,StartPos,p-StartPos));
        inc(p);
        if (p<=Len) and (Value[p] in [#10,#13]) and (Value[p-1]<>Value[p]) then
          inc(p);
        StartPos:=p;
      end;
    end;
    if StartPos<=Len then
      sl.Add(copy(Value,StartPos,Len-StartPos+1));
    AddStrings(sl);
  finally
    sl.Free;
    EndUpdate;
  end;
end;

function TSynEditStrings.LogicalToPhysicalPos(const p : TPoint) : TPoint;
begin
  Result := p;
  if Result.Y - 1 < Count then
    Result.X:=LogicalToPhysicalCol(self[Result.Y - 1],Result.X);
end;

function TSynEditStrings.LogicalToPhysicalCol(const Line : string; LogicalPos : integer) : integer;
begin
  Result := LogicalToPhysicalCol(PChar(Pointer(Line)),length(Line),LogicalPos,1,1);
end;

function TSynEditStrings.LogicalToPhysicalCol(Line : PChar; LineLen : integer; LogicalPos, StartBytePos, StartPhysicalPos : integer) : integer;
var
  BytePos, ByteLen: integer;
  ScreenPos: integer;
begin
  ByteLen := LineLen;
  // map UTF8 and Tab chars
  ScreenPos := StartPhysicalPos;
  BytePos:= StartBytePos;
  while BytePos<LogicalPos do begin
    if (BytePos <= ByteLen) then begin
      if Line[BytePos-1] = #9 then begin
        inc(ScreenPos, TabWidth - ((ScreenPos-1) mod TabWidth));
        inc(BytePos);
      end else begin
        inc(ScreenPos);
        if IsUTF8 then
          inc(BytePos,UTF8CharacterLength(@Line[BytePos-1]))
        else
          inc(BytePos);
      end;
    end else begin
      // beyond end of line
      inc(ScreenPos,LogicalPos-BytePos);
      break;
    end;
  end;
  if (BytePos>LogicalPos) and (ScreenPos>StartPhysicalPos) then
    dec(ScreenPos);
  Result := ScreenPos;
end;

function TSynEditStrings.PhysicalToLogicalPos(const p : TPoint) : TPoint;
begin
  Result := p;
  if (Result.Y>=1) and (Result.Y <= Count) then
    Result.X:=PhysicalToLogicalCol(self[Result.Y - 1],Result.X,1,1);
end;

function TSynEditStrings.PhysicalToLogicalCol(const Line : string; PhysicalPos : integer) : integer;
begin
  Result:=PhysicalToLogicalCol(Line,PhysicalPos,1,1);
end;

function TSynEditStrings.PhysicalToLogicalCol(const Line : string; PhysicalPos, StartBytePos, StartPhysicalPos : integer) : integer;
var
  BytePos, ByteLen: integer;
  ScreenPos: integer;
  PLine: PChar;
begin
  ByteLen := Length(Line);
  ScreenPos := StartPhysicalPos;
  BytePos := StartBytePos;
  PLine := PChar(Line);
  // map utf and tab chars
  while ScreenPos < PhysicalPos do begin
    if (BytePos <= ByteLen) then begin
      if (PLine[BytePos-1] <> #9) then begin
        inc(ScreenPos);
        if IsUTF8 then
          inc(BytePos,UTF8CharacterLength(@PLine[BytePos-1]))
        else
          inc(BytePos);
      end else begin
        inc(ScreenPos, TabWidth - ((ScreenPos-1) mod TabWidth));
        inc(BytePos);
      end;
    end else begin
      // beyond end of line
      inc(BytePos,PhysicalPos-ScreenPos);
      break;
    end;
  end;
  if (ScreenPos>PhysicalPos) and (BytePos>1) and (BytePos-2<ByteLen)
  and (PLine[BytePos-2]=#9) then
    dec(BytePos);
  Result := BytePos;
end;

end.


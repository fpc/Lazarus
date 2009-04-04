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
unit SynEditTextTabExpander;

{$I synedit.inc}

interface

uses
LCLProc,
  Classes, SysUtils, SynEditTextBase;

const
  // Offset to add to LengthOfLine, if Line has no tabs.
  // (Length will still be valid if tab-width changes)
  NoTabLengthOffset = MaxInt div 2;

type

{ TSynEditStringTabExpander }

  TSynEditStringTabExpander = class(TSynEditStringsLinked)
  private
    FTabWidth: integer;
    FIndexOfLongestLine: Integer;
    function GetLengthOfLine(Index: Integer): integer;
    procedure SetLengthOfLine(Index: Integer; const AValue: integer);
    Procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    function ExpandedString(Index: integer): string;
    function ExpandedStringLength(Index: integer): Integer;
  protected
    function  GetTabWidth : integer;
    procedure SetTabWidth(const AValue : integer);
    function  GetExpandedString(Index: integer): string; override;
    function  GetLengthOfLongestLine: integer; override;
    property LengthOfLine[Index: Integer]: integer
      read GetLengthOfLine write SetLengthOfLine;
  public
    constructor Create(ASynStringSource: TSynEditStrings);
    destructor Destroy; override;
    function GetPhysicalCharWidths(const Line: String; Index: Integer): TPhysicalCharWidths; override;

    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
  public
    property TabWidth: integer read GetTabWidth write SetTabWidth;
  end;


implementation

function GetHasTabs(pLine: PChar): boolean;
begin
  if Assigned(pLine) then begin
    while (pLine^ <> #0) do begin
      if (pLine^ = #9) then break;
      Inc(pLine);
    end;
    Result := (pLine^ = #9);
  end else
    Result := FALSE;
end;

{ TSynEditStringTabExpander }

constructor TSynEditStringTabExpander.Create(ASynStringSource: TSynEditStrings);
begin
  FIndexOfLongestLine := -1;
  inherited Create(ASynStringSource);
  fSynStrings.RegisterAttribute(TSynEditStringTabExpander, SizeOf(Integer));
  TabWidth := 8;
  fSynStrings.AddChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  fSynStrings.AddChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
end;

destructor TSynEditStringTabExpander.Destroy;
begin
  fSynStrings.RemoveChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  fSynStrings.RemoveChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  inherited Destroy;
end;

function TSynEditStringTabExpander.GetLengthOfLine(Index: Integer): integer;
begin
  Result := Integer(PtrUInt(Attribute[TSynEditStringTabExpander, Index]));
end;

procedure TSynEditStringTabExpander.SetLengthOfLine(Index: Integer; const AValue: integer);
begin
   Attribute[TSynEditStringTabExpander, Index] := Pointer(PtrUInt(AValue));
end;

function TSynEditStringTabExpander.GetTabWidth: integer;
begin
  Result := FTabWidth;
end;

procedure TSynEditStringTabExpander.SetTabWidth(const AValue: integer);
var
  i: integer;
begin
  if FTabWidth = AValue then exit;

  FTabWidth := AValue;
  FIndexOfLongestLine := -1;
  for i := 0 to Count - 1 do
    if not(LengthOfLine[i] >= NoTabLengthOffset) then
      LengthOfLine[i] := -1;
end;

procedure TSynEditStringTabExpander.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
var
  i: integer;
begin
  FIndexOfLongestLine := -1;
  for i := AIndex to AIndex + ACount - 1 do
    LengthOfLine[i] := -1;
end;

function TSynEditStringTabExpander.ExpandedString(Index: integer): string;
var
  Line: String;
  CharWidths: TPhysicalCharWidths;
  i, j, l: Integer;
begin
  Line := fSynStrings[Index];
  if (Line = '') or (not GetHasTabs(PChar(Line))) then begin
    Result := Line;
    LengthOfLine[Index] := length(Result) + NoTabLengthOffset;
  end else begin
    CharWidths := GetPhysicalCharWidths(Line, Index);
    l := 0;
    for i := 0 to length(CharWidths)-1 do
      l := l + CharWidths[i];
    SetLength(Result, l);

    l := 1;
    for i := 1 to length(CharWidths) do begin
      if Line[i] <> #9 then begin
        Result[l] := Line[i];
        inc(l);
      end else begin
        for j := 1 to CharWidths[i-1] do begin
          Result[l] := ' ';
          inc(l);
        end;
      end;
    end;
    LengthOfLine[Index] := length(Result);
  end;
end;

function TSynEditStringTabExpander.ExpandedStringLength(Index: integer): Integer;
var
  Line: String;
  CharWidths: TPhysicalCharWidths;
  i: Integer;
begin
  Line := fSynStrings[Index];
  if (Line = '') or (not GetHasTabs(PChar(Line))) then begin
    Result := Length(Line);
    LengthOfLine[Index] := Result + NoTabLengthOffset;
  end else begin
    CharWidths := GetPhysicalCharWidths(Line, Index);
    Result := 0;
    for i := 0 to length(CharWidths)-1 do
      Result := Result + CharWidths[i];

    LengthOfLine[Index] := Result;
  end;
end;

function TSynEditStringTabExpander.GetExpandedString(Index: integer): string;
begin
  if (Index >= 0) and (Index < Count) then begin
    if LengthOfLine[Index] >= NoTabLengthOffset then
      Result := fSynStrings[Index]
    else
      Result := ExpandedString(Index);
  end else
    Result := '';
end;

function TSynEditStringTabExpander.GetPhysicalCharWidths(const Line: String;
  Index: Integer): TPhysicalCharWidths;
var
  i, p: Integer;
begin
  Result := inherited GetPhysicalCharWidths(Line, Index);
  p := 0;
  for i := 0 to length(Line) -1 do begin
    if Result[i] = 0 then continue;
    if Line[i+1] = #9 then
      Result[i] := FTabWidth - p mod FTabWidth;
    p := p + Result[i];
  end
end;

function TSynEditStringTabExpander.GetLengthOfLongestLine: integer;
var
  i, j, MaxLen: integer;
begin
  if fIndexOfLongestLine < 0 then begin
    MaxLen := 0;
    if Count > 0 then begin
      for i := 0 to Count - 1 do begin
        j := LengthOfLine[i];
        if j >= NoTabLengthOffset then j := j -  NoTabLengthOffset;
        if j < 0 then
          j := ExpandedStringLength(i);
        if j > MaxLen then begin
          MaxLen := j;
          fIndexOfLongestLine := i;
        end;
      end;
    end;
    exit(MaxLen);
  end;
  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < Count) then begin
    Result := LengthOfLine[fIndexOfLongestLine];
    if Result >= NoTabLengthOffset then Result := Result -  NoTabLengthOffset;
  end else
    Result := 0;
end;

end.


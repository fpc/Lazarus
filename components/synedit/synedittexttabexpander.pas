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
  Classes, SysUtils, SynEditTypes, SynEditTextBase, SynEditTextBuffer,
  SynEditMiscClasses, SynEditMiscProcs;

const
  // Offset to add to LengthOfLine, if Line has no tabs.
  // (Length will still be valid if tab-width changes)
  NoTabLengthOffset = MaxInt div 2;

type

{ TSynEditStringTabExpander }

TSynEditStringTabExpander = class(TSynEditStringsLinked)
  private
    FTabWidth: integer;
    FConvertTabsProc: TConvertTabsProcEx;
    FSimulateConvertTabsProc: TSimulateConvertTabsProcEx;
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

    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;

    // TODO: maybe use inherited for utf8?
    function LogicalToPhysicalCol(Line: PChar; LineLen: integer;
                  LogicalPos, StartBytePos,
                  StartPhysicalPos: integer): integer; override;
    function PhysicalToLogicalCol(const Line: string;
                  PhysicalPos, StartBytePos,
                  StartPhysicalPos: integer): integer; override;
  public
    property TabWidth: integer read GetTabWidth write SetTabWidth;
  end;


implementation

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
  FConvertTabsProc := GetBestConvertTabsProcEx(fTabWidth);
  FSimulateConvertTabsProc := GetBestSimulateConvertTabsProcEx(fTabWidth);
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
  HasTabs: boolean;
begin
  if fSynStrings[Index] = '' then begin
    Result := '';
    LengthOfLine[Index] := 0 + NoTabLengthOffset;
  end else begin
    Result := fConvertTabsProc(fSynStrings[Index], fTabWidth, HasTabs);
    if HasTabs then
      LengthOfLine[Index] := length(Result)
    else
      LengthOfLine[Index] := length(Result) + NoTabLengthOffset;
  end;
end;

function TSynEditStringTabExpander.ExpandedStringLength(Index: integer): Integer;
var
  HasTabs: boolean;
begin
  if fSynStrings[Index] = '' then begin
    Result := 0;
    LengthOfLine[Index] := 0 + NoTabLengthOffset;
  end else begin
    Result := fSimulateConvertTabsProc(fSynStrings[Index], fTabWidth, HasTabs);
    if HasTabs then
      LengthOfLine[Index] := Result
    else
      LengthOfLine[Index] := Result + NoTabLengthOffset;
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

function TSynEditStringTabExpander.LogicalToPhysicalCol(Line: PChar;
  LineLen: integer; LogicalPos, StartBytePos, StartPhysicalPos: integer): integer;
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

function TSynEditStringTabExpander.PhysicalToLogicalCol(const Line: string;
  PhysicalPos, StartBytePos, StartPhysicalPos: integer): integer;
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


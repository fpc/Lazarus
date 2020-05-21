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
  LCLProc, Classes, SysUtils, math, LazSynEditText, SynEditTextBase;

type

  // lines longer than 16383 chars, will be stored as unknown
  TLineLen = Word;
  PLineLen = ^TLineLen;

  { TSynEditStringTabData }

  TSynEditStringTabData = class(TSynManagedStorageMem)
  private
    FRefCount: Integer;
    function GetLineLen(Index: Integer): TLineLen;
    procedure SetLineLen(Index: Integer; const AValue: TLineLen);
  public
    constructor Create;
    procedure IncRefCount;
    procedure DecRefCount;
    property RefCount: Integer read FRefCount;
    property LineLen[Index: Integer]: TLineLen read GetLineLen write SetLineLen; default;
  end;

{ TSynEditStringTabExpander }

  TSynEditStringTabExpander = class(TSynEditStringsLinked)
  private
    FTabWidth: integer;
    FIndexOfLongestLine: Integer;
    FFirstUnknownLongestLine, FLastUnknownLongestLine: Integer;
    FTabData: TSynEditStringTabData;
    FLastLineHasTab: Boolean; // Last line, parsed by GetPhysicalCharWidths
    FLastLinePhysLen: Integer;
    FViewChangeStamp: int64;
    procedure TextBufferChanged(Sender: TObject);
    procedure LineTextChanged(Sender: TSynEditStrings; aIndex, aCount: Integer);
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    function ExpandedString(Index: integer): string;
    function ExpandedStringLength(Index: integer): Integer;
  protected
    procedure SetManager(AManager: TSynTextViewsManager); override;
    procedure SetSynStrings(AValue: TSynEditStrings); override;
    function GetViewChangeStamp: int64; override;
    function  GetTabWidth : integer;
    procedure SetTabWidth(const AValue : integer);
    function  GetExpandedString(Index: integer): string; override;
    function  GetLengthOfLongestLine: integer; override;
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;
  public
    constructor Create;
    destructor Destroy; override;

    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
  public
    property TabWidth: integer read GetTabWidth write SetTabWidth;
  end;


implementation

const
  // Offset to add to LengthOfLine, if Line has no tabs.
  // (Length will still be valid if tab-width changes)
  NO_TAB_IN_LINE_OFFSET = high(TLineLen) div 2;
  LINE_LEN_UNKNOWN = high(TLineLen);
  MAX_LINE_LEN_STORED = NO_TAB_IN_LINE_OFFSET - 1;

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

{ TSynEditStringTabData }

function TSynEditStringTabData.GetLineLen(Index: Integer): TLineLen;
begin
  Result := PLineLen(ItemPointer[Index])^;
end;

procedure TSynEditStringTabData.SetLineLen(Index: Integer; const AValue: TLineLen);
begin
  PLineLen(ItemPointer[Index])^ := AValue;
end;

constructor TSynEditStringTabData.Create;
begin
  inherited;
  ItemSize := SizeOf(TLineLen);
  FRefCount := 1;
end;

procedure TSynEditStringTabData.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TSynEditStringTabData.DecRefCount;
begin
  dec(FRefCount);
end;

{ TSynEditStringTabExpander }

constructor TSynEditStringTabExpander.Create;
begin
  FIndexOfLongestLine := -1;
  FFirstUnknownLongestLine := -1;
  FLastUnknownLongestLine := -1;
  inherited Create;
  TabWidth := 8;
end;

destructor TSynEditStringTabExpander.Destroy;
var
  Data: TSynEditStringTabData;
begin
  Data := TSynEditStringTabData(NextLines.Ranges[Self]);
  if Assigned(Data) then begin
    Data.DecRefCount;
    if Data.RefCount = 0 then begin
      NextLines.Ranges[Self] := nil;
      Data.Free;
    end;
  end;
  NextLines := nil;
  inherited Destroy;
end;

function TSynEditStringTabExpander.GetTabWidth: integer;
begin
  Result := FTabWidth;
end;

procedure TSynEditStringTabExpander.SetTabWidth(const AValue: integer);
begin
  if FTabWidth = AValue then exit;

  {$PUSH}{$Q-}{$R-}
  FViewChangeStamp := FViewChangeStamp + 1;
  {$POP}

  FTabWidth := AValue;
  FIndexOfLongestLine := -1;
  FFirstUnknownLongestLine := -1;
  FLastUnknownLongestLine := -1;
end;

function TSynEditStringTabExpander.GetViewChangeStamp: int64;
begin
  Result := inherited GetViewChangeStamp;
  {$PUSH}{$Q-}{$R-}
  Result := Result + FViewChangeStamp;
  {$POP}
end;

procedure TSynEditStringTabExpander.TextBufferChanged(Sender: TObject);
var
  Data: TSynEditStringTabData;
  i: integer;
begin
  // Using self, instead as class, to register tab-width-data
  // other shared edits can have different tab-width
  if (Sender <> nil) and
     (FTabData = TSynEditStringTabData(NextLines.Ranges[Self]))
  then
    exit;

  if Sender <> nil then begin
    Data := TSynEditStringTabData(TSynEditStrings(Sender).Ranges[Self]);
    if Assigned(Data) then begin
      Data.DecRefCount;
      if Data.RefCount = 0 then begin
        TSynEditStrings(Sender).Ranges[Self] := nil;
        Data.Free;
      end;
    end;
  end;
  FTabData := TSynEditStringTabData(NextLines.Ranges[Self]);
  if FTabData = nil then begin
    FTabData := TSynEditStringTabData.Create;
    NextLines.Ranges[Self] := FTabData;
    for i := 0 to Count - 1 do
      if not(FTabData[i] >= NO_TAB_IN_LINE_OFFSET) then
        FTabData[i] := LINE_LEN_UNKNOWN;
  end
  else
    FTabData.IncRefCount;
  LineTextChanged(TSynEditStrings(Sender), 0, Count);
end;

procedure TSynEditStringTabExpander.LineTextChanged(Sender: TSynEditStrings; aIndex,
  aCount: Integer);
var
  i: integer;
begin
  if (FIndexOfLongestLine >= AIndex) and (FIndexOfLongestLine < AIndex+ACount) then
    FIndexOfLongestLine := -1;
  if (FFirstUnknownLongestLine < 0) or (AIndex < FFirstUnknownLongestLine) then
    FFirstUnknownLongestLine := AIndex;
  if AIndex+ACount-1 > FLastUnknownLongestLine then
    FLastUnknownLongestLine := AIndex+ACount-1;
  for i := AIndex to AIndex + ACount - 1 do
    FTabData[i] := LINE_LEN_UNKNOWN;
end;

procedure TSynEditStringTabExpander.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
var
  i: integer;
begin
  if (FIndexOfLongestLine >= AIndex) then
    FIndexOfLongestLine := FIndexOfLongestLine + ACount;

  if ACount < 0 then begin
    if (FIndexOfLongestLine >= AIndex+ACount) and (FIndexOfLongestLine < AIndex) then
      FIndexOfLongestLine := -1;
    if (FFirstUnknownLongestLine >= 0) then begin
      if (AIndex < FFirstUnknownLongestLine) then
        FFirstUnknownLongestLine := Max(AIndex, FFirstUnknownLongestLine + ACount);
      if (AIndex < FLastUnknownLongestLine) then
        FLastUnknownLongestLine := Max(AIndex, FLastUnknownLongestLine + ACount);
    end;

    exit;
  end;

  if (FFirstUnknownLongestLine < 0) or (AIndex < FFirstUnknownLongestLine) then
    FFirstUnknownLongestLine := AIndex;
  if (AIndex < FLastUnknownLongestLine) or (FLastUnknownLongestLine < 0) then
    FLastUnknownLongestLine := Max(AIndex, FLastUnknownLongestLine) +ACount;
  for i := AIndex to AIndex + ACount - 1 do
    FTabData[i] := LINE_LEN_UNKNOWN;
end;

function TSynEditStringTabExpander.ExpandedString(Index: integer): string;
var
  Line: String;
  CharWidths: TPhysicalCharWidths;
  i, j, l: Integer;
begin
// this is only used by trimmer.lengthOfLongestLine / which is not called, if a tab module is present
  Line := NextLines[Index];
  if (Line = '') or (not GetHasTabs(PChar(Line))) then begin
    Result := Line;
    // xxx wrong double width // none latin ...
    //FTabData[Index] := length(Result) + NO_TAB_IN_LINE_OFFSET;
  end else begin
    CharWidths := GetPhysicalCharWidths(Pchar(Line), length(Line), Index);
    l := 0;
    for i := 0 to length(CharWidths)-1 do
      l := l + (CharWidths[i] and PCWMask);
    SetLength(Result, l);

    l := 1;
    for i := 1 to length(CharWidths) do begin
      if Line[i] <> #9 then begin
        Result[l] := Line[i];
        inc(l);
      end else begin
        for j := 1 to (CharWidths[i-1] and PCWMask) do begin
          Result[l] := ' ';
          inc(l);
        end;
      end;
    end;
    FTabData[Index] := length(Result);
  end;
end;

function TSynEditStringTabExpander.ExpandedStringLength(Index: integer): Integer;
var
  Line: String;
  CharWidths: TPhysicalCharWidths;
  i: Integer;
begin
  Line := NextLines[Index];
  if (Line = '') then begin
    Result := 0;
    FTabData[Index] := Result + NO_TAB_IN_LINE_OFFSET;
  end else begin
    i := length(Line);
    SetLength(CharWidths, i);
    DoGetPhysicalCharWidths(Pchar(Line), i, Index, @CharWidths[0]);
    Result := 0;
    for i := 0 to length(CharWidths)-1 do
      Result := Result + (CharWidths[i] and PCWMask);

    if FLastLineHasTab then // FLastLineHasTab is set by GetPhysicalCharWidths
      FTabData[Index] := Result
    else
      FTabData[Index] := Result + NO_TAB_IN_LINE_OFFSET;
  end;
end;

procedure TSynEditStringTabExpander.SetManager(AManager: TSynTextViewsManager);
begin
  if Manager <> nil then begin
    RemoveChangeHandler(senrLineChange, @LineTextChanged);
    RemoveChangeHandler(senrLineCount, @LineCountChanged);
    RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
  end;
  inherited SetManager(AManager);
  if Manager <> nil then begin
    AddChangeHandler(senrLineCount, @LineCountChanged);
    AddChangeHandler(senrLineChange, @LineTextChanged);
    AddNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
  end;
end;

procedure TSynEditStringTabExpander.SetSynStrings(AValue: TSynEditStrings);
begin
  inherited SetSynStrings(AValue);
  if NextLines <> nil then begin
    if FTabData = nil then
      TextBufferChanged(nil);
  end;
end;

function TSynEditStringTabExpander.GetExpandedString(Index: integer): string;
begin
  if (Index >= 0) and (Index < Count) then begin
    if FTabData[Index] >= NO_TAB_IN_LINE_OFFSET then
      Result := NextLines[Index]
    else
      Result := ExpandedString(Index);
  end else
    Result := '';
end;

procedure TSynEditStringTabExpander.DoGetPhysicalCharWidths(Line: PChar;
  LineLen, Index: Integer; PWidths: PPhysicalCharWidth);
var
  HasTab: Boolean;
  i, j: Integer;
begin
  inherited DoGetPhysicalCharWidths(Line, LineLen, Index, PWidths);
  HasTab := False;
  j := 0;
  for i := 0 to LineLen - 1 do begin
    if (PWidths^ and PCWMask) <> 0 then begin
      if Line^ = #9 then begin
        PWidths^ := (FTabWidth - (j mod FTabWidth) and PCWMask) or (PWidths^  and (not PCWMask));
        HasTab := True;
      end;
      j := j + (PWidths^ and PCWMask);
    end;
    inc(Line);
    inc(PWidths);
  end;
  FLastLineHasTab := HasTab;
  FLastLinePhysLen := j;
end;

function TSynEditStringTabExpander.GetLengthOfLongestLine: integer;
var
  Line: PChar;
  LineLen: Integer;
  CharWidths: PPhysicalCharWidth;
  i, j, m: Integer;
  Line1, Line2: Integer;
begin
  Result := 0;
  Line1 := 0;
  Line2 := Count - 1;

  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < Count) then begin
    Result := FTabData[fIndexOfLongestLine];
    if Result <> LINE_LEN_UNKNOWN then begin
      if Result >= NO_TAB_IN_LINE_OFFSET then Result := Result -  NO_TAB_IN_LINE_OFFSET;
      if (FFirstUnknownLongestLine < 0) then
        exit;
      // Result has the value from index
      Line1 := FFirstUnknownLongestLine;
      if (FLastUnknownLongestLine < Line2) then
        Line2 := FLastUnknownLongestLine;
    end
    else begin
      Result := 0;
      if (FFirstUnknownLongestLine < 0) then begin
        Line1 := fIndexOfLongestLine;
        Line2 := fIndexOfLongestLine;
      end
      else begin // TODO: Calculate for fIndexOfLongestLine, instead of extending the range
        Line1 := Min(fIndexOfLongestLine, FFirstUnknownLongestLine);
        if (FLastUnknownLongestLine < Line2) then
          Line2 := Max(fIndexOfLongestLine, FLastUnknownLongestLine);
      end;
    end;
  end;

  FFirstUnknownLongestLine := -1;
  FLastUnknownLongestLine := -1;

  try
    //Result := 0;
    m := 0;
    CharWidths := nil;
    for i := Line1 to Line2 do begin
      j := FTabData[i];
      if j = LINE_LEN_UNKNOWN then begin
        // embedd a copy of ExpandedStringLength
        // allows one to re-use CharWidths
        Line := NextLines.GetPChar(i,LineLen); // NextLines[i];
        j := 0;
        if (LineLen = 0) then begin
          FTabData[i] := j + NO_TAB_IN_LINE_OFFSET;
        end else begin
          if LineLen > m then begin
            ReAllocMem(CharWidths, LineLen * SizeOf(TPhysicalCharWidth));
            m := LineLen;
          end;
          DoGetPhysicalCharWidths(Line, LineLen, i, CharWidths);
          j := FLastLinePhysLen;

          if j > MAX_LINE_LEN_STORED then
            FTabData[i] := LINE_LEN_UNKNOWN
          else if FLastLineHasTab then // FLastLineHasTab is set by GetPhysicalCharWidths
            FTabData[i] := j
          else
            FTabData[i] := j + NO_TAB_IN_LINE_OFFSET;
        end;
      end
      else
      if j >= NO_TAB_IN_LINE_OFFSET then
        j := j -  NO_TAB_IN_LINE_OFFSET;
      if j > Result then begin
        Result := j;
        fIndexOfLongestLine := i;
      end;
    end;
  finally
    ReAllocMem(CharWidths, 0);
  end;
end;

end.


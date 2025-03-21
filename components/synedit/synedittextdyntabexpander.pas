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
unit SynEditTextDynTabExpander;

{$I synedit.inc}
{ $DEFINE SynDynTabExpander}

interface

uses
  Classes, SysUtils, math, LazSynEditText, SynEditTextBase,
  SynEditTextTabExpander, SynEditTypes, LazLoggerBase;

type
  IntArray = Array of integer;


  { TCachedColumnWidth }

  TCachedColumnWidth = object
  public
    HasInvalidEntry, NeedTopBoundsCheck, NeedBottomBoundsCheck: boolean;
    FirstLineIdx: IntIdx;
    ColumnWidths: array of IntArray;
    procedure SetFirstLineIdx(AnIdx: IntIdx);
    procedure Invalidate;
    procedure InvalidateLine(AnIdx: IntIdx); inline;
    procedure InvalidateLine(AnIdx, ACount: IntIdx);
    function IsValid: boolean; inline;
    function IsValidLine(AnIdx: IntIdx): boolean; inline;
    function ContainsLine(AnIdx: IntIdx): boolean; inline;
    procedure GetMergedInfo(AnIdx: IntIdx; out ATopIdx, ABottomIdx: IntIdx; out MergedMinColWidths: IntArray);
    procedure DebugDump(AMsg: String='');
  end;

{ TSynEditStringDynTabExpander }

  TSynEditStringDynTabExpander = class(TSynEditStringTabExpanderWithLongest)
  private const
    MAX_MERGE = 32;
  private
    FLastLinePhysLen: Integer;
    FStoredLongestLineIdx, FStoredLongestLineBlockBegin, FStoredLongestLineBlockEnd: IntIdx;
    FStoredLongestLineLen: Integer;
    FMinTabWidth: integer;
    FViewChangeStamp: int64;
    FTabWidth: integer;
    FCachedColumnWidth: TCachedColumnWidth;
    FIsInLineTextChanged: boolean;
    function ExpandedString(AnIndex: integer): string;
    function ExpandedStringLength(AnIndex: integer): Integer;
    function GetMinimumColumnWidths(AnIndex: integer): IntArray; inline;
    function GetMinimumColumnWidths(ALine: PChar; LineLen, AnIndex: integer; ALineIsTempText: boolean = False): IntArray;
    procedure SetMinTabWidth(AValue: integer);

    function GetTabCount(AnIndex: IntIdx): Integer;
    procedure MergeMinColumnWidth(var ATarget: IntArray; const ANew: IntArray; ANewLimit: Integer = high(integer));
    function GetUpdatedCache(AnIndex: IntIdx): TCachedColumnWidth;
  protected
    procedure LineCountChanged(Sender: TSynEditStrings; AnIndex, ACount : Integer); override;
    procedure LineTextChanged(Sender: TSynEditStrings; AnIndex, aCount: Integer); override;
    procedure SetTabWidth(const AValue: integer); override;
    function  GetTabWidth: integer; override;
    function  GetViewChangeStamp: int64; override;
    function  GetExpandedString(AnIndex: integer): string; override;
    function  GetKnownLengthOfLine(AnIndex: IntIdx): integer; override;
    function  GetIndexOfLongestLine(AStartIndex, AnEndIndex: IntIdx; out ALen: integer): integer; override;
    procedure SetLongestLineInfo(AnIndex: IntIdx; ALen: Integer); override;
    procedure DoGetPhysicalCharWidths(ALine: PChar; LineLen, AnIndex: Integer; PWidths: PPhysicalCharWidth); override;
  public
    constructor Create; override;
    property MinTabWidth: integer read FMinTabWidth write SetMinTabWidth;
  end;


implementation

const
  InvalidCache: IntArray = (-1);

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

function GetTabCount(pLine: PChar): integer;
begin
  Result := 0;
  if Assigned(pLine) then begin
    while (pLine^ <> #0) do begin
      if (pLine^ = #9) then
        inc(Result);
      Inc(pLine);
    end;
  end;
end;

{ TCachedColumnWidth }

procedure TCachedColumnWidth.SetFirstLineIdx(AnIdx: IntIdx);
var
  d: IntIdx;
  i: Integer;
begin
  if (FirstLineIdx >= 0) and (Length(ColumnWidths) > 0) then begin
    if AnIdx > FirstLineIdx then begin
      d := AnIdx - FirstLineIdx;
      if d < Length(ColumnWidths) then begin
        for i := 0 to d-1 do ColumnWidths[i] := nil;
        system.move(ColumnWidths[d], ColumnWidths[0], (Length(ColumnWidths)-d) * SizeOf(ColumnWidths[0]));
        system.FillByte(ColumnWidths[Length(ColumnWidths) - d], d * SizeOf(ColumnWidths[0]), 0);
        SetLength(ColumnWidths, Length(ColumnWidths) - d);
      end
      else
        ColumnWidths := nil;
    end
    else
    if AnIdx < FirstLineIdx then begin
      d := FirstLineIdx - AnIdx;
      SetLength(ColumnWidths, Length(ColumnWidths) + d);
      system.move(ColumnWidths[0], ColumnWidths[d], (Length(ColumnWidths)-d) * SizeOf(ColumnWidths[0]));
      system.FillByte(ColumnWidths[0], d * SizeOf(ColumnWidths[0]), 0);
    end;
  end;
  FirstLineIdx := AnIdx;
end;

procedure TCachedColumnWidth.Invalidate;
begin
  FirstLineIdx := -1;
  ColumnWidths := nil;
  HasInvalidEntry := False;
  NeedTopBoundsCheck    := True;
  NeedBottomBoundsCheck := True;
end;

procedure TCachedColumnWidth.InvalidateLine(AnIdx: IntIdx);
begin
  if (FirstLineIdx < 0) then
    exit;

  InvalidateLine(AnIdx, 1);
end;

procedure TCachedColumnWidth.InvalidateLine(AnIdx, ACount: IntIdx);
var
  AStart, AnEnd, i: Integer;
begin
  if (FirstLineIdx < 0) then
    exit;

  if (AnIdx < FirstLineIdx) and (AnIdx + ACount {- 1} >= FirstLineIdx {- 1}) then
    NeedTopBoundsCheck := True;
  if (AnIdx <= FirstLineIdx + Length(ColumnWidths)) and (AnIdx + ACount >= FirstLineIdx + Length(ColumnWidths)) then
    NeedBottomBoundsCheck := True;

  if (AnIdx + ACount-1 < FirstLineIdx) or (AnIdx >= FirstLineIdx + Length(ColumnWidths)) then
    exit;

  AnIdx := AnIdx-FirstLineIdx;

  AStart := Max(0, AnIdx);
  AnEnd := Min(AnIdx+ACount, Length(ColumnWidths)) - 1;

  while (AStart > 0) and (ColumnWidths[AStart-1] = ColumnWidths[AStart]) do
    dec(AStart);
  while (AnEnd < Length(ColumnWidths)-1) and (ColumnWidths[AnEnd+1] = ColumnWidths[AnEnd]) do
    inc(AnEnd);

  for i := AStart to AnEnd do
    ColumnWidths[i] := InvalidCache;
  HasInvalidEntry := True;
end;

function TCachedColumnWidth.IsValid: boolean;
begin
  Result := FirstLineIdx >= 0;
end;

function TCachedColumnWidth.IsValidLine(AnIdx: IntIdx): boolean;
begin
  Result := (FirstLineIdx >= 0) and
            (AnIdx >= FirstLineIdx) and (AnIdx < FirstLineIdx + Length(ColumnWidths)) and
            (ColumnWidths[AnIdx - FirstLineIdx] <> InvalidCache);
end;

function TCachedColumnWidth.ContainsLine(AnIdx: IntIdx): boolean;
begin
  Result := IsValid and (AnIdx >= FirstLineIdx) and (AnIdx < FirstLineIdx + Length(ColumnWidths))
end;

procedure TCachedColumnWidth.GetMergedInfo(AnIdx: IntIdx; out ATopIdx,
  ABottomIdx: IntIdx; out MergedMinColWidths: IntArray);
var
  l: Integer;
begin
  ATopIdx    := AnIdx;
  ABottomIdx := AnIdx;
  MergedMinColWidths := ColumnWidths[AnIdx - FirstLineIdx];
  if MergedMinColWidths = InvalidCache then
    exit;

  ATopIdx := ATopIdx - FirstLineIdx;
  while (ATopIdx > 0) and (ColumnWidths[ATopIdx-1] = ColumnWidths[ATopIdx]) do
    dec(ATopIdx);
  ATopIdx := ATopIdx + FirstLineIdx;

  l := Length(ColumnWidths) - 1;
  ABottomIdx := ABottomIdx - FirstLineIdx;
  while (ABottomIdx < l) and(ColumnWidths[ABottomIdx+1] = ColumnWidths[ABottomIdx]) do
    inc(ABottomIdx);
  ABottomIdx := ABottomIdx + FirstLineIdx;
end;

procedure TCachedColumnWidth.DebugDump(AMsg: String);
var
  i, j: Integer;
  s: String;
begin
  debugln('>>> %s: TCachedColumnWidth %d (len %d) / InvEntry %s / Bound %s %s', [AMsg, FirstLineIdx, Length(ColumnWidths), dbgs(HasInvalidEntry), dbgs(NeedTopBoundsCheck), dbgs(NeedBottomBoundsCheck)]);
  for i := 0 to Min(Length(ColumnWidths) - 1, 50) do begin
    s := '';
    for j := 0 to Length(ColumnWidths[i]) - 1 do
      s := s + IntToStr(ColumnWidths[i, j])+', ';
    debugln('    %s : %s', [dbgs((i=0) or (ColumnWidths[i] = ColumnWidths[i-1])), s]);
  end;
end;

{ TSynEditStringDynTabExpander }

constructor TSynEditStringDynTabExpander.Create;
begin
  FTabWidth := 1;
  inherited Create;
end;

function TSynEditStringDynTabExpander.ExpandedString(AnIndex: integer): string;
var
  Line: String;
  CharWidths: TPhysicalCharWidths;
  i, j, l: Integer;
begin
// this is only used by trimmer.lengthOfLongestLine / which is not called, if a tab module is present
  Line := NextLines[AnIndex];
  if (Line = '') or (not GetHasTabs(PChar(Line))) then begin
    Result := Line;
  end else begin
    CharWidths := GetPhysicalCharWidths(Pchar(Line), length(Line), AnIndex);
    l := 0;
    for i := 0 to length(CharWidths)-1 do
      l := l + (CharWidths[i] and PCWMask);
    Result := '';
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
  end;
end;

function TSynEditStringDynTabExpander.ExpandedStringLength(AnIndex: integer): Integer;
var
  Line: String;
  CharWidths: TPhysicalCharWidths;
  i: Integer;
begin
  Line := NextLines[AnIndex];
  if (Line = '') then begin
    Result := 0;
  end else begin
    i := length(Line);
    SetLength(CharWidths{%H-}, i);
    DoGetPhysicalCharWidths(Pchar(Line), i, AnIndex, @CharWidths[0]);
    Result := 0;
    for i := 0 to length(CharWidths)-1 do
      Result := Result + (CharWidths[i] and PCWMask);
  end;
end;

function TSynEditStringDynTabExpander.GetMinimumColumnWidths(AnIndex: integer
  ): IntArray;
var
  LTxt: String;
begin
  LTxt := NextLines[AnIndex];
  Result := GetMinimumColumnWidths(PChar(LTxt), Length(LTxt), AnIndex, False);
end;

function TSynEditStringDynTabExpander.GetMinimumColumnWidths(ALine: PChar;
  LineLen, AnIndex: integer; ALineIsTempText: boolean): IntArray;
var
  i, j, LastTabEnd, l, StoredLen: Integer;
  CharWidths: TPhysicalCharWidths;
  PWidths: PPhysicalCharWidth;
  HasStoredData, StoredHasTab: Boolean;
begin
  (* ALineIsTempText
     - The text in ALine is not the stored Line data.
     - When "ALineIsTempText = FALSE"
       => Other stored data for the index can be used/updated
  *)
  Result := nil;

  HasStoredData := False;
  if not ALineIsTempText then begin
    HasStoredData := TabData.GetLineInfo(AnIndex, StoredLen, StoredHasTab);
    if HasStoredData and not StoredHasTab then
      exit;
  end;

  if (ALine = nil) or (not GetHasTabs(ALine)) then begin
    // No tabs in ALine
    assert(ALineIsTempText or (not {%H-}StoredHasTab), 'TSynEditStringDynTabExpander.GetMinimumColumnWidths: ALineIsTempText or (not StoredHasTab)');
    if (not ALineIsTempText) and (not HasStoredData) then
      TabData.SetLineInfoUnknownEx(AnIndex, False);
    exit;
  end;
  assert(ALineIsTempText or (not HasStoredData) or StoredHasTab, 'TSynEditStringDynTabExpander.GetMinimumColumnWidths: ALineIsTempText or (not HasStoredData) or StoredHasTab');

  CharWidths := NextLines.GetPhysicalCharWidths(ALine, LineLen, AnIndex);
  PWidths := @CharWidths[0];
  LastTabEnd := 0;
  j := 0;
  for i := 0 to LineLen - 1 do begin
    if (PWidths^ and PCWMask) <> 0 then begin
      if ALine^ = #9 then begin
        l := Length(Result);
        SetLength(Result, l+1);
        Result[l] := Max(FTabWidth, j - LastTabEnd + FMinTabWidth);
        LastTabEnd := j;
      end
      else
        j := j + (PWidths^ and PCWMask);
    end;
    inc(ALine);
    inc(PWidths);
  end;

  assert(Length(Result)>0, 'TSynEditStringDynTabExpander.GetMinimumColumnWidths: Length(Result)>0');
  assert(ALineIsTempText or (not HasStoredData) or (TabData.LineLen[AnIndex] = Length(Result)), 'TSynEditStringDynTabExpander.GetMinimumColumnWidths: ALineIsTempText or (not HasStoredData) or (TabData.LineLen[AnIndex] = Length(Result))');

  if (not ALineIsTempText) and (not HasStoredData) then
    TabData.SetLineInfo(AnIndex, Length(Result), True); // amount of tabs
end;

procedure TSynEditStringDynTabExpander.SetTabWidth(const AValue: integer);
begin
  if FTabWidth = AValue then exit;

  {$PUSH}{$Q-}{$R-}
  FViewChangeStamp := FViewChangeStamp + 1;
  {$POP}

  FTabWidth := AValue;
  if FTabWidth < 1 then
    FTabWidth := 1;

  if NextLines <> nil then
    LineTextChanged(nil, 0, Count);
  InvalidateLongestLineInfo;
end;

function TSynEditStringDynTabExpander.GetTabWidth: integer;
begin
  Result := FTabWidth;
end;

procedure TSynEditStringDynTabExpander.LineTextChanged(Sender: TSynEditStrings;
  AnIndex, aCount: Integer);
begin
  if (Sender = Self) or FIsInLineTextChanged then
    exit;
  FIsInLineTextChanged := True;
  try
    inherited LineTextChanged(Sender, AnIndex, aCount);

    if FStoredLongestLineIdx >= 0 then begin
      if (FStoredLongestLineBlockEnd >= AnIndex) and (FStoredLongestLineBlockBegin < AnIndex+aCount) then
        FStoredLongestLineIdx := -1;
    end;

    // TODO: find minimum range
    SendNotification(senrLineChange, self, 0, Count-1);
    FCachedColumnWidth.InvalidateLine(AnIndex, aCount);
  finally
    FIsInLineTextChanged := False;
  end;
end;

procedure TSynEditStringDynTabExpander.LineCountChanged(
  Sender: TSynEditStrings; AnIndex, ACount: Integer);
begin
  inherited LineCountChanged(Sender, AnIndex, ACount);

  if FStoredLongestLineIdx >= 0 then begin
    if (FStoredLongestLineBlockEnd >= AnIndex) and (FStoredLongestLineBlockBegin < AnIndex+aCount) then begin
      FStoredLongestLineIdx := -1;
    end
    else
    if FStoredLongestLineBlockBegin >= AnIndex then begin
      FStoredLongestLineIdx        := FStoredLongestLineIdx + ACount;
      FStoredLongestLineBlockBegin := FStoredLongestLineBlockBegin + ACount;
      FStoredLongestLineBlockEnd   := FStoredLongestLineBlockEnd + ACount;
    end
    else
      assert(FStoredLongestLineBlockEnd < AnIndex, 'TSynEditStringDynTabExpander.LineCountChanged: FStoredLongestLineBlockEnd < AnIndex');
  end;

  SendNotification(senrLineChange, self, 0, Count-1);
  FCachedColumnWidth.Invalidate;
end;

procedure TSynEditStringDynTabExpander.SetMinTabWidth(AValue: integer);
begin
  if FMinTabWidth = AValue then Exit;
  FMinTabWidth := AValue;
  if FMinTabWidth < 1 then
    FMinTabWidth := 1;

  {$PUSH}{$Q-}{$R-}
  FViewChangeStamp := FViewChangeStamp + 1;
  {$POP}
  if NextLines <> nil then
    LineTextChanged(nil, 0, Count);
  InvalidateLongestLineInfo;
end;

function TSynEditStringDynTabExpander.GetTabCount(AnIndex: IntIdx): Integer;
var
  LTxt: String;
  i: Integer;
  StoredHasTab: Boolean;
begin
  if TabData.GetLineInfo(AnIndex, Result, StoredHasTab) then begin
    if not StoredHasTab then
      Result := 0;
    if (Result <> LINE_LEN_UNKNOWN) then
      exit;
  end;

  Result := 0;
  LTxt := NextLines[AnIndex];
  for i := 1 to Length(LTxt) - 1 do
    if LTxt[i] = #9 then
      inc(Result);

  if Result > 0 then
    TabData.SetLineInfo(AnIndex, Result, True) // amount of tabs
  else
    TabData.SetLineInfoUnknownEx(AnIndex, False);
end;

procedure TSynEditStringDynTabExpander.MergeMinColumnWidth(
  var ATarget: IntArray; const ANew: IntArray; ANewLimit: Integer);
var
  i: Integer;
  TrgLen, NewLen: Integer;
begin
  TrgLen := Length(ATarget);
  NewLen := Length(ANew);
  if NewLen > ANewLimit then
    NewLen := ANewLimit;

  if NewLen > TrgLen then begin
    SetLength(ATarget, NewLen);
    system.move(ANew[TrgLen], ATarget[TrgLen], (NewLen-TrgLen) * SizeOf(ATarget[0]) );
  end;

  for i := 0 to Min(NewLen, TrgLen) - 1 do
    if ANew[i] > ATarget[i] then
      ATarget[i] := ANew[i];
end;

function TSynEditStringDynTabExpander.GetUpdatedCache(AnIndex: IntIdx
  ): TCachedColumnWidth;
var
  Idx, LastLineIdx, CachedMergeTopIdx, CachedMergeBottomIdx: IntIdx;
  CachedMergeLineCnt, PrevLineTabCnt, LineCnt: Integer;
  CurMinColWidths, CachedMergedMinColWidths, NewMergedMinColWidths: IntArray;
  LTxt: String;

  procedure SaveNewMergeCache; inline;
  var
    i: IntIdx;
  begin
    if CachedMergeTopIdx >= 0 then begin
      if (not Result.IsValid) or
         (CachedMergeTopIdx < Result.FirstLineIdx)
      then
        Result.SetFirstLineIdx(CachedMergeTopIdx);
      if CachedMergeBottomIdx >= Result.FirstLineIdx + Length(Result.ColumnWidths) then
        SetLength(Result.ColumnWidths, CachedMergeBottomIdx- Result.FirstLineIdx + 1);
      for i := CachedMergeTopIdx - Result.FirstLineIdx to CachedMergeBottomIdx - Result.FirstLineIdx do
        Result.ColumnWidths[i] := NewMergedMinColWidths;
      CachedMergeTopIdx := -1;
    end;
  end;

  function CanMergeToBlock(NewLineTabCnt, BlockAdjacentLineTabCount, BlockMaxTabCount: Integer): boolean;/////////// inline;
  begin
    // Blocks can't have a lesser count in the middle, as this would leave independent columns in the upper and lower half
    Result := (NewLineTabCnt <= BlockAdjacentLineTabCount) or
              (BlockAdjacentLineTabCount = BlockMaxTabCount);
  end;

begin
  if FCachedColumnWidth.ContainsLine(AnIndex) then begin
    Result := FCachedColumnWidth;
    if Result.IsValid and not Result.HasInvalidEntry then
      exit;
  end
  else
    Result.Invalidate;

  // Look backward
  Idx := AnIndex + 1;
  CachedMergeTopIdx := -1;
  While (Idx >= 0) do begin
    dec(Idx);
    if Idx < 0 then
      break;

    if (Idx < Result.FirstLineIdx) and (not Result.NeedTopBoundsCheck) then
      break;
    if Result.IsValidLine(Idx) then begin
    // TODO: can the current block be merged to data in front?
      SaveNewMergeCache;
      continue;
    end;

    LTxt := NextLines[Idx];{%H-}
    if LTxt = '' then
      break;
    CurMinColWidths := GetMinimumColumnWidths(PChar(LTxt), Length(LTxt), Idx);
    if Length(CurMinColWidths) = 0 then
      break;

    if CachedMergeTopIdx < 0 then begin
      NewMergedMinColWidths := nil;

      if (Idx < AnIndex) or
         ( Result.ContainsLine(Idx + 1) and Result.IsValidLine(Idx + 1) )
      then begin
        Result.GetMergedInfo(Idx+1, CachedMergeTopIdx, CachedMergeBottomIdx, CachedMergedMinColWidths);
        assert(CachedMergedMinColWidths <> InvalidCache, 'TSynEditStringDynTabExpander.GetUpdatedCache: CachedMergedMinColWidths <> InvalidCache');
        assert(CachedMergedMinColWidths <> nil, 'TSynEditStringDynTabExpander.GetUpdatedCache: CachedMergedMinColWidths <> nil');

        CachedMergeLineCnt := CachedMergeBottomIdx - CachedMergeTopIdx + 1;
        if (CachedMergeLineCnt >= MAX_MERGE) or
           ( (CachedMergeLineCnt > 1) and  // if it is a single line, it can always merge
             (not CanMergeToBlock(Length(CurMinColWidths), GetTabCount(Idx+1), Length(CachedMergedMinColWidths)))
           )
        then begin
          CachedMergeTopIdx := -1;
        end
        else begin
          NewMergedMinColWidths := CurMinColWidths; // Target must be unique array
          CurMinColWidths := CachedMergedMinColWidths;
        end;
      end;

    end
    else begin
      // NewMergedMinColWidths in progress
      if not CanMergeToBlock(Length(CurMinColWidths), PrevLineTabCnt{%H-}, Length(NewMergedMinColWidths)) then
        SaveNewMergeCache;
    end;

    PrevLineTabCnt := Length(CurMinColWidths);
    if CachedMergeTopIdx < 0 then begin
      CachedMergeTopIdx := Idx;
      CachedMergeBottomIdx := Idx;
      NewMergedMinColWidths := CurMinColWidths;
      continue; // new block / nothing to merge
    end;
    CachedMergeTopIdx := Idx;

    MergeMinColumnWidth(NewMergedMinColWidths, CurMinColWidths);
    if {%H-}CachedMergeBottomIdx - CachedMergeTopIdx >= MAX_MERGE then
      SaveNewMergeCache;
  end;

  if Idx = AnIndex then begin // current line has no tabs
    assert(not Result.IsValid, 'TSynEditStringDynTabExpander.GetUpdatedCache: not Result.IsValid');
    Result.Invalidate;
    exit;
  end;

  SaveNewMergeCache;
  Result.SetFirstLineIdx(Idx+1);
  Result.NeedTopBoundsCheck := False;

  /////
  // Look forward
  Idx := AnIndex - 1;
  LineCnt := Count-1;
  LastLineIdx := Result.FirstLineIdx + Length(Result.ColumnWidths)-1;
  CachedMergeTopIdx := -1;
  NewMergedMinColWidths := nil;
  While (Idx <= LineCnt) do begin
    inc(Idx);
    if Idx > LineCnt then
      break;

    if (Idx > LastLineIdx) and (not Result.NeedBottomBoundsCheck) then
      break;
    if Result.IsValidLine(Idx) then begin
// TODO: can the current block be merged to data in front?
      SaveNewMergeCache;
      continue;
    end;

    LTxt := NextLines[Idx];
    if LTxt = '' then
      break;
    CurMinColWidths := GetMinimumColumnWidths(PChar(LTxt), Length(LTxt), Idx);
    if Length(CurMinColWidths) = 0 then
      break;

    if CachedMergeTopIdx < 0 then begin
      if (Idx > AnIndex) or
         ( Result.ContainsLine(Idx - 1) and Result.IsValidLine(Idx - 1) )
      then begin
        Result.GetMergedInfo(Idx-1, CachedMergeTopIdx, CachedMergeBottomIdx, CachedMergedMinColWidths);
        assert(CachedMergedMinColWidths <> InvalidCache, 'TSynEditStringDynTabExpander.GetUpdatedCache: CachedMergedMinColWidths <> InvalidCache');
        assert(CachedMergedMinColWidths <> nil, 'TSynEditStringDynTabExpander.GetUpdatedCache: CachedMergedMinColWidths <> nil');

        CachedMergeLineCnt := CachedMergeBottomIdx - CachedMergeTopIdx + 1;
        if (CachedMergeLineCnt >= MAX_MERGE) or
           ( (CachedMergeLineCnt > 1) and  // if it is a single line, it can always merge
             (not CanMergeToBlock(Length(CurMinColWidths), GetTabCount(Idx-1), Length(CachedMergedMinColWidths)))
           )
        then begin
          CachedMergeTopIdx := -1;
        end
        else begin
          NewMergedMinColWidths := CurMinColWidths; // Target must be unique array
          CurMinColWidths := CachedMergedMinColWidths;
        end;
      end;

    end
    else begin
      // NewMergedMinColWidths in progress
      if not CanMergeToBlock(Length(CurMinColWidths), PrevLineTabCnt{%H-}, Length(NewMergedMinColWidths)) then
        SaveNewMergeCache;
    end;

    PrevLineTabCnt := Length(CurMinColWidths);
    CachedMergeBottomIdx := Idx;
    if CachedMergeTopIdx < 0 then begin
      CachedMergeTopIdx := Idx;
      NewMergedMinColWidths := CurMinColWidths;
      continue; // new block / nothing to merge
    end;

    MergeMinColumnWidth(NewMergedMinColWidths, CurMinColWidths);
    if CachedMergeBottomIdx - CachedMergeTopIdx >= MAX_MERGE then
      SaveNewMergeCache;
  end;

  SaveNewMergeCache;
  Result.NeedBottomBoundsCheck := False;
  Result.HasInvalidEntry := False;
  assert(Idx<=Result.FirstLineIdx+Length(Result.ColumnWidths), 'TSynEditStringDynTabExpander.GetUpdatedCache: Idx<=Result.FirstLineIdx+Length(Result.ColumnWidths)');
  SetLength(Result.ColumnWidths, idx - Result.FirstLineIdx);

  if Length(Result.ColumnWidths) > 0 then
    FCachedColumnWidth := Result;

  {$IFDEF SynDynTabExpander}
  result.DebugDump(IntToStr(AnIndex));
  {$ENDIF}
end;

function TSynEditStringDynTabExpander.GetViewChangeStamp: int64;
begin
  Result := inherited GetViewChangeStamp;
  {$PUSH}{$Q-}{$R-}
  Result := Result + FViewChangeStamp;
  {$POP}
end;

function TSynEditStringDynTabExpander.GetExpandedString(AnIndex: integer
  ): string;
begin
  if (AnIndex >= 0) and (AnIndex < Count) then begin
    Result := ExpandedString(AnIndex);
  end else
    Result := '';
end;

function TSynEditStringDynTabExpander.GetKnownLengthOfLine(AnIndex: IntIdx
  ): integer;
var
  StoredLen: Integer;
  StoredHasTab: Boolean;
begin
  Result := LINE_LEN_UNKNOWN;
  if not TabData.GetLineInfo(AnIndex, StoredLen, StoredHasTab) then
    exit;
  if not StoredHasTab then
    Result := StoredLen;
  if AnIndex = FStoredLongestLineIdx then
    Result := FStoredLongestLineLen;
end;

function TSynEditStringDynTabExpander.GetIndexOfLongestLine(AStartIndex,
  AnEndIndex: IntIdx; out ALen: integer): integer;
var
  Line: PChar;
  LineLen: Integer;
  CharWidths: PPhysicalCharWidth;
  i, StoredLen, m: Integer;
  StoredHasTab, HasStoredData: Boolean;
begin
  Result := 0;
  ALen := 0;
  try
    m := 0;
    CharWidths := nil;
    for i := AStartIndex to AnEndIndex do begin
      HasStoredData := TabData.GetLineInfo(i, StoredLen, StoredHasTab);
      if (not HasStoredData) or StoredHasTab or (StoredLen = LINE_LEN_UNKNOWN) then begin
        // embedd a copy of ExpandedStringLength
        // allows one to re-use CharWidths
        Line := NextLines.GetPChar(i,LineLen); // NextLines[i];
        StoredLen := 0;
        if (LineLen = 0) then begin
          TabData.SetLineInfo(i, LineLen, False);
        end else begin
          if LineLen > m then begin
            ReAllocMem(CharWidths, LineLen * SizeOf(TPhysicalCharWidth));
            m := LineLen;
          end;
          FLastLinePhysLen := -2;
          DoGetPhysicalCharWidths(Line, LineLen, i, CharWidths);
          StoredLen := FLastLinePhysLen;
        end;
      end;

      if StoredLen > ALen then begin
        ALen := StoredLen;
        Result := i;
      end;
    end;
  finally
    ReAllocMem(CharWidths, 0);
  end;
end;

procedure TSynEditStringDynTabExpander.SetLongestLineInfo(AnIndex: IntIdx;
  ALen: Integer);
var
  i: IntIdx;
  c: Integer;
begin
  inherited SetLongestLineInfo(AnIndex, ALen);
  FStoredLongestLineLen := ALen;
  FStoredLongestLineIdx := AnIndex;

  if TabData.HasTab[AnIndex] then begin
    i := AnIndex - 1;
    while (i >= 0) and TabData.HasTab[i] do
      dec(i);
    FStoredLongestLineBlockBegin := i;

    i := AnIndex + 1;
    c := Count;
    while (i < c) and TabData.HasTab[i] do
      inc(i);
    FStoredLongestLineBlockEnd := i;
  end;
end;

procedure TSynEditStringDynTabExpander.DoGetPhysicalCharWidths(ALine: PChar;
  LineLen, AnIndex: Integer; PWidths: PPhysicalCharWidth);
var
  i, j, c, LastTabEnd, DynWidth, StoredLen, CurColCnt, TmpColCnt: Integer;
  MinColumnWidthsCurLine, MinColumnWidthsTmp, MinColumnWidthsTmpPrev: IntArray;
  NxtLine: String;
  LineIsTempText, IsTempText, StoredHasTab, HasStoredData,
    NeedLastPhysLen: Boolean;
  Cache: TCachedColumnWidth;

  procedure UpdateLastLinePhysLen;
  var
    i,j: integer;
  begin
    if not NeedLastPhysLen then
      exit;
    j := 0;
    for i := 0 to LineLen - 1 do begin
      j := j + (PWidths^ and PCWMask);
      inc(PWidths);
    end;
    FLastLinePhysLen := j;
  end;

begin
  NeedLastPhysLen := FLastLinePhysLen = -2;
  FLastLinePhysLen := 0;
  inherited DoGetPhysicalCharWidths(ALine, LineLen, AnIndex, PWidths);

  if ALine = '' then
    exit;

  if AnIndex < 0 then begin
    MinColumnWidthsCurLine := GetMinimumColumnWidths(ALine, LineLen, AnIndex, True);
  end
  else begin
    NxtLine := NextLines[AnIndex];
    LineIsTempText := (PChar(NxtLine) <> ALine) and
      ( (Length(NxtLine) <> LineLen) or (strlcomp(PChar(NxtLine), ALine, LineLen)<>0) ) ;

    if LineIsTempText then begin
      if not GetHasTabs(ALine) then begin
        UpdateLastLinePhysLen;
        exit;
      end;
    end
    else begin
      HasStoredData := TabData.GetLineInfo(AnIndex, StoredLen, StoredHasTab);
      if not HasStoredData then begin
        if not GetHasTabs(ALine) then begin
          UpdateLastLinePhysLen;
          if NeedLastPhysLen then
            TabData.SetLineInfo(AnIndex, FLastLinePhysLen, False)
          else
            TabData.SetLineInfoUnknownEx(AnIndex, False);
          exit;
        end;
      end
      else
      if not StoredHasTab then begin
        UpdateLastLinePhysLen;
        exit;
      end;
    end;


    Cache := GetUpdatedCache(AnIndex);
    assert(Cache.IsValid, 'TSynEditStringDynTabExpander.DoGetPhysicalCharWidths: Cache.IsValid');

    if LineIsTempText then begin
      MinColumnWidthsCurLine := GetMinimumColumnWidths(ALine, LineLen, AnIndex, True);
      MinColumnWidthsTmpPrev := Cache.ColumnWidths[AnIndex - Cache.FirstLineIdx];
    end
    else begin
      MinColumnWidthsCurLine := Cache.ColumnWidths[AnIndex - Cache.FirstLineIdx];
      MinColumnWidthsTmpPrev := MinColumnWidthsCurLine;
      SetLength(MinColumnWidthsCurLine, Length(MinColumnWidthsCurLine));
    end;

    CurColCnt := GetTabCount(AnIndex);
    TmpColCnt := CurColCnt;
    IsTempText := LineIsTempText;
    i := AnIndex;
    c := Cache.FirstLineIdx;
    while i > c do begin
      dec(i);
      MinColumnWidthsTmp := Cache.ColumnWidths[i - Cache.FirstLineIdx];
      if MinColumnWidthsTmp = MinColumnWidthsTmpPrev then begin
        if IsTempText then begin
          MinColumnWidthsTmp := GetMinimumColumnWidths(i);
          MergeMinColumnWidth(MinColumnWidthsCurLine, MinColumnWidthsTmp);
        end;
        continue;
      end;
      IsTempText := False;
      if i + 1 < AnIndex then
        TmpColCnt := min(TmpColCnt, GetTabCount(i+1));
      TmpColCnt := min(TmpColCnt, GetTabCount(i));
      assert(TmpColCnt > 0, 'TSynEditStringDynTabExpander.DoGetPhysicalCharWidths: TmpColCnt > 0');
      MergeMinColumnWidth(MinColumnWidthsCurLine, MinColumnWidthsTmp, TmpColCnt);
      MinColumnWidthsTmpPrev := MinColumnWidthsTmp;
    end;

    TmpColCnt := CurColCnt;
    IsTempText := LineIsTempText;
    i := AnIndex;
    MinColumnWidthsTmpPrev := MinColumnWidthsCurLine;
    c := Cache.FirstLineIdx + Length(Cache.ColumnWidths) - 1;
    while i < c do begin
      inc(i);
      MinColumnWidthsTmp := Cache.ColumnWidths[i - Cache.FirstLineIdx];
      if MinColumnWidthsTmp = MinColumnWidthsTmpPrev then begin
        if IsTempText then begin
          MinColumnWidthsTmp := GetMinimumColumnWidths(i);
          MergeMinColumnWidth(MinColumnWidthsCurLine, MinColumnWidthsTmp);
        end;
        continue;
      end;
      IsTempText := False;
      if i - 1 > AnIndex then
        TmpColCnt := min(TmpColCnt, GetTabCount(i-1));
      TmpColCnt := min(TmpColCnt, GetTabCount(i));
      assert(TmpColCnt > 0, 'TSynEditStringDynTabExpander.DoGetPhysicalCharWidths: TmpColCnt > 0');
      MergeMinColumnWidth(MinColumnWidthsCurLine, MinColumnWidthsTmp, TmpColCnt);
      MinColumnWidthsTmpPrev := MinColumnWidthsTmp;
    end;
  end;


  j := 0;
  c := 0;
  LastTabEnd := 0;
  for i := 0 to LineLen - 1 do begin
    if (PWidths^ and PCWMask) <> 0 then begin
      if ALine^ = #9 then begin
        DynWidth := Max(0, (MinColumnWidthsCurLine[c] - (j - LastTabEnd)));
        If DynWidth > PCWMask then
          DynWidth := PCWMask;
        PWidths^ := DynWidth or (PWidths^  and (not PCWMask));
        inc(c);
        j := j + DynWidth;
        LastTabEnd := j;
      end
      else
        j := j + (PWidths^ and PCWMask);
    end;
    inc(ALine);
    inc(PWidths);
  end;
  FLastLinePhysLen := j;
end;

end.


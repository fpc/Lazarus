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
  Classes, SysUtils, Math, LazSynEditText, SynEditTextBase, SynEditTypes;

type

  // lines longer than "High(TLineLen) div 2" chars, will be stored as unknown
  TStoredLineLen = DWord;
  PStoredLineLen = ^TStoredLineLen;

  { TSynEditStringLineLenData }

  TSynEditStringLineLenData = class(TSynManagedStorageMem)
  private
    FRefCount: Integer;
    procedure SetLineLen(Index: Integer; const AValue: TStoredLineLen);
  protected
    function GetLineLen(Index: Integer): TStoredLineLen; virtual;
  public
    constructor Create;
    procedure IncRefCount;
    procedure DecRefCount;
    property RefCount: Integer read FRefCount;
    property LineLen[Index: Integer]: TStoredLineLen read GetLineLen write SetLineLen; default;
  end;

  { TSynEditStringTabData }

  TSynEditStringTabData = class(TSynEditStringLineLenData)
  private const
    // Offset to add to LengthOfLine, if Line has no tabs.
    // (Length will still be valid if tab-width changes)
    NO_TAB_IN_LINE_OFFSET = TStoredLineLen(
                                ( ( (high(TStoredLineLen) >> 1) and high(Integer) ) << 1) and
                              not ( (high(TStoredLineLen) >> 1) and high(Integer) )
                            );
    MAX_LINE_LEN_STORED = NO_TAB_IN_LINE_OFFSET - 2;
    LINE_INFO_UNKNOWN = 0; // invalid entry
  public const
    LINE_LEN_UNKNOWN  = TStoredLineLen(NO_TAB_IN_LINE_OFFSET- 1);
    (* Special values
       - LINE_INFO_UNKNOWN                        : Nothing know, not even if tab exists
       - LINE_LEN_UNKNOWN                         : Tab exists, but nothing else known (an empty line can't have tabs, so 0 can be used as marker)
       - LINE_LEN_UNKNOWN + NO_TAB_IN_LINE_OFFSET : NO Tab exists, but nothing else known
    *)
  private
    function GetHasTab(Index: Integer): Boolean; inline;
    function GetRawData(Index: Integer): TStoredLineLen;
  protected
    function GetLineLen(Index: Integer): TStoredLineLen; override;
  public
    procedure SetLineInfo(AnIndex: Integer; ALineLen: Integer; AnHasTab: Boolean); inline;
    procedure SetLineInfoUnknown(AnIndex: Integer); inline;
    procedure SetLineInfoUnknownEx(AnIndex: Integer; AnHasTab: Boolean); inline;
    function  GetLineInfo(AnIndex: Integer; out ALineLen: Integer; out AnHasTab: Boolean): boolean; inline;

    property HasTab[Index: Integer]: Boolean read GetHasTab;
    property LineLen[Index: Integer]: TStoredLineLen read GetLineLen; default;
    property RawData[Index: Integer]: TStoredLineLen read GetRawData;
  end;

const
  LINE_LEN_UNKNOWN = TSynEditStringTabData.LINE_LEN_UNKNOWN;

type

  { TSynEditStringTabExpanderBase }

  TSynEditStringTabExpanderBase = class(TSynEditStringsLinked)
  protected
    function  GetTabWidth : integer; virtual; abstract;
    procedure SetTabWidth(const AValue : integer); virtual; abstract;
  public
    constructor Create; virtual;
    property TabWidth: integer read GetTabWidth write SetTabWidth;
  end;

  { TSynEditStringTabExpanderCommon }

  TSynEditStringTabExpanderCommon = class(TSynEditStringTabExpanderBase)
  private
    FTabData: TSynEditStringTabData;

    procedure ReleaseTabData;
  protected
    procedure TextBufferChanged(Sender: TObject); virtual;
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer); virtual;
    procedure LineTextChanged(Sender: TSynEditStrings; aIndex, aCount: Integer); virtual;
    procedure SetManager(AManager: TSynTextViewsManager); override;
    procedure SetSynStrings(AValue: TSynEditStrings); override;

    property TabData: TSynEditStringTabData read FTabData;
  public
    destructor Destroy; override;
  end;

  { TSynEditStringTabExpanderWithLongest }

  TSynEditStringTabExpanderWithLongest = class(TSynEditStringTabExpanderCommon)
  private
    FIndexOfLongestLine: Integer;
    FFirstUnknownLongestLine, FLastUnknownLongestLine: Integer;

  protected
    procedure InvalidateLongestLineInfo;
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer); override;
    procedure LineTextChanged(Sender: TSynEditStrings; aIndex, aCount: Integer); override;

    function  GetIndexOfLongestLine(AStartIndex, AnEndIndex: IntIdx; out ALen: integer): integer; virtual;
    function  GetKnownLengthOfLine(AnIndex: IntIdx): integer; virtual;
    procedure SetLongestLineInfo(AnIndex: IntIdx; ALen: Integer); virtual;
    function  GetLengthOfLongestLine: integer; override;
  public
    constructor Create; override;
  end;


  TSynEditStringTabExpander = class(TSynEditStringTabExpanderWithLongest)
  private
    FTabWidth: integer;
    FLastLineHasTab: Boolean; // Last line, parsed by GetPhysicalCharWidths
    FLastLinePhysLen: Integer;
    FViewChangeStamp: int64;
    function ExpandedString(Index: integer): string;
    function ExpandedStringLength(Index: integer): Integer;
  protected
    procedure TextBufferChanged(Sender: TObject); override;
    function GetViewChangeStamp: int64; override;
    function  GetTabWidth : integer; override;
    procedure SetTabWidth(const AValue : integer); override;
    function  GetExpandedString(Index: integer): string; override;
    function GetKnownLengthOfLine(AnIndex: IntIdx): integer; override;
    function GetIndexOfLongestLine(AStartIndex, AnEndIndex: IntIdx; out ALen: integer): integer; override;
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;
  public
    constructor Create; override;
  end;

  TSynEditStringTabExpanderClass = class of TSynEditStringTabExpanderBase;


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

{ TSynEditStringLineLenData }

function TSynEditStringLineLenData.GetLineLen(Index: Integer): TStoredLineLen;
begin
  Result := PStoredLineLen(ItemPointer[Index])^;
end;

procedure TSynEditStringLineLenData.SetLineLen(Index: Integer; const AValue: TStoredLineLen);
begin
  PStoredLineLen(ItemPointer[Index])^ := AValue;
end;

constructor TSynEditStringLineLenData.Create;
begin
  inherited;
  ItemSize := SizeOf(TStoredLineLen);
  FRefCount := 1;
end;

procedure TSynEditStringLineLenData.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TSynEditStringLineLenData.DecRefCount;
begin
  dec(FRefCount);
end;

{ TSynEditStringTabData }

function TSynEditStringTabData.GetHasTab(Index: Integer): Boolean;
var
  l: TStoredLineLen;
begin
  l := inherited GetLineLen(Index);
  Result := (l < NO_TAB_IN_LINE_OFFSET) and (l <> LINE_INFO_UNKNOWN);
end;

function TSynEditStringTabData.GetRawData(Index: Integer): TStoredLineLen;
begin
  Result := inherited GetLineLen(Index);
end;

function TSynEditStringTabData.GetLineLen(Index: Integer): TStoredLineLen;
begin
  Result := inherited GetLineLen(Index);
  if Result = LINE_INFO_UNKNOWN then
    Result := LINE_LEN_UNKNOWN // unkonwn len, but tabs exist
  else
  if (Result >= NO_TAB_IN_LINE_OFFSET) then
    Result := Result - NO_TAB_IN_LINE_OFFSET;
end;

procedure TSynEditStringTabData.SetLineInfo(AnIndex: Integer;
  ALineLen: Integer; AnHasTab: Boolean);
begin
  assert((not AnHasTab) or (ALineLen>0), 'TSynEditStringTabData.SetLineInfo: (not AnHasTab) or (ALineLen>0)');
  if ALineLen > MAX_LINE_LEN_STORED then
    ALineLen := LINE_LEN_UNKNOWN;
  if AnHasTab then
    inherited SetLineLen(AnIndex, ALineLen)
  else
    inherited SetLineLen(AnIndex, ALineLen + NO_TAB_IN_LINE_OFFSET);
end;

procedure TSynEditStringTabData.SetLineInfoUnknown(AnIndex: Integer);
begin
  inherited SetLineLen(AnIndex, LINE_INFO_UNKNOWN);
end;

procedure TSynEditStringTabData.SetLineInfoUnknownEx(AnIndex: Integer;
  AnHasTab: Boolean);
begin
  if AnHasTab then
    inherited SetLineLen(AnIndex, LINE_LEN_UNKNOWN)
  else
    inherited SetLineLen(AnIndex, LINE_LEN_UNKNOWN + NO_TAB_IN_LINE_OFFSET);
end;

function TSynEditStringTabData.GetLineInfo(AnIndex: Integer; out
  ALineLen: Integer; out AnHasTab: Boolean): boolean;
var
  d: TStoredLineLen;
begin
  d := inherited GetLineLen(AnIndex);
  Result := d <> LINE_INFO_UNKNOWN;
  AnHasTab := Result and (d < NO_TAB_IN_LINE_OFFSET);

  if not Result then
    ALineLen := LINE_LEN_UNKNOWN
  else
  if (d >= NO_TAB_IN_LINE_OFFSET) then
    ALineLen := d - NO_TAB_IN_LINE_OFFSET
  else
    ALineLen := d;
end;

{ TSynEditStringTabExpanderBase }

constructor TSynEditStringTabExpanderBase.Create;
begin
  inherited Create;
end;

{ TSynEditStringTabExpanderCommon }

procedure TSynEditStringTabExpanderCommon.ReleaseTabData;
var
  Data: TSynEditStringLineLenData;
begin
  Data := TSynEditStringTabData(NextLines.Ranges[Self]);
  if Assigned(Data) then begin
    Data.DecRefCount;
    if Data.RefCount = 0 then begin
      NextLines.Ranges[Self] := nil;
      Data.Free;
    end;
  end;
  FTabData := nil;
end;

procedure TSynEditStringTabExpanderCommon.TextBufferChanged(Sender: TObject);
var
  Data: TSynEditStringLineLenData;
begin
  // Using self, instead as class, to register tab-width-data
  // other shared edits can have different tab-width
  if (Sender <> nil) and
     (FTabData = TSynEditStringLineLenData(NextLines.Ranges[Self]))
  then
    exit;

  if Sender <> nil then begin
    Data := TSynEditStringLineLenData(TSynEditStrings(Sender).Ranges[Self]);
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
  end
  else
    FTabData.IncRefCount;
end;

procedure TSynEditStringTabExpanderCommon.LineCountChanged(
  Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
  //
end;

procedure TSynEditStringTabExpanderCommon.LineTextChanged(
  Sender: TSynEditStrings; aIndex, aCount: Integer);
var
  i: Integer;
begin
  for i := AIndex to AIndex + ACount - 1 do
    TabData.SetLineInfoUnknown(i);
end;

procedure TSynEditStringTabExpanderCommon.SetManager(
  AManager: TSynTextViewsManager);
begin
  if Manager <> nil then begin
    RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
    RemoveChangeHandler(senrLineCount, @LineCountChanged);
    RemoveChangeHandler(senrLineChange, @LineTextChanged);
  end;
  if AManager = nil then
    ReleaseTabData;
  inherited SetManager(AManager);
  if Manager <> nil then begin
    AddChangeHandler(senrLineChange, @LineTextChanged);
    AddChangeHandler(senrLineCount, @LineCountChanged);
    AddNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
  end;
end;

procedure TSynEditStringTabExpanderCommon.SetSynStrings(AValue: TSynEditStrings
  );
begin
  inherited SetSynStrings(AValue);
  if NextLines <> nil then begin
    if TabData = nil then
      TextBufferChanged(nil);
  end;
end;

destructor TSynEditStringTabExpanderCommon.Destroy;
begin
  ReleaseTabData;
  inherited Destroy;
  NextLines := nil;
end;

{ TSynEditStringTabExpanderWithLongest }

procedure TSynEditStringTabExpanderWithLongest.InvalidateLongestLineInfo;
begin
  FIndexOfLongestLine := -1;
  FFirstUnknownLongestLine := -1;
  FLastUnknownLongestLine := -1;
end;

procedure TSynEditStringTabExpanderWithLongest.LineCountChanged(
  Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
  inherited LineCountChanged(Sender, AIndex, ACount);

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
end;

procedure TSynEditStringTabExpanderWithLongest.LineTextChanged(
  Sender: TSynEditStrings; aIndex, aCount: Integer);
begin
  inherited LineTextChanged(Sender, aIndex, aCount);

  if (FIndexOfLongestLine >= AIndex) and (FIndexOfLongestLine < AIndex+ACount) then
    FIndexOfLongestLine := -1;
  if (FFirstUnknownLongestLine < 0) or (AIndex < FFirstUnknownLongestLine) then
    FFirstUnknownLongestLine := AIndex;
  if AIndex+ACount-1 > FLastUnknownLongestLine then
    FLastUnknownLongestLine := AIndex+ACount-1;
end;

function TSynEditStringTabExpanderWithLongest.GetIndexOfLongestLine(
  AStartIndex, AnEndIndex: IntIdx; out ALen: integer): integer;
begin
  ALen := High(Result) - 5;
  Result := -1;
end;

function TSynEditStringTabExpanderWithLongest.GetKnownLengthOfLine(
  AnIndex: IntIdx): integer;
begin
  Result := High(Result) - 5;
end;

procedure TSynEditStringTabExpanderWithLongest.SetLongestLineInfo(
  AnIndex: IntIdx; ALen: Integer);
begin
  fIndexOfLongestLine := AnIndex;
end;

function TSynEditStringTabExpanderWithLongest.GetLengthOfLongestLine: integer;
var
  NewIdx, NewLen: Integer;
  Line1, Line2: Integer;
begin
  Result := 0;
  Line1 := 0;
  Line2 := Count - 1;

  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < Count) then begin
    Result := GetKnownLengthOfLine(FIndexOfLongestLine);
    if Result <> LINE_LEN_UNKNOWN then begin
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

  NewIdx := GetIndexOfLongestLine(Line1, Line2, NewLen);
  if NewLen > Result then begin
    SetLongestLineInfo(NewIdx, NewLen);
    Result := NewLen;
  end;

  FFirstUnknownLongestLine := -1;
  FLastUnknownLongestLine := -1;
end;

constructor TSynEditStringTabExpanderWithLongest.Create;
begin
  InvalidateLongestLineInfo;
  inherited Create;
end;

{ TSynEditStringTabExpander }

constructor TSynEditStringTabExpander.Create;
begin
  inherited Create;
  TabWidth := 8;
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
  if NextLines <> nil then
    LineTextChanged(nil, 0, Count);
  InvalidateLongestLineInfo;
end;

function TSynEditStringTabExpander.GetViewChangeStamp: int64;
begin
  Result := inherited GetViewChangeStamp;
  {$PUSH}{$Q-}{$R-}
  Result := Result + FViewChangeStamp;
  {$POP}
end;

procedure TSynEditStringTabExpander.TextBufferChanged(Sender: TObject);
begin
  inherited TextBufferChanged(Sender);
  LineTextChanged(TSynEditStrings(Sender), 0, Count);
end;

function TSynEditStringTabExpander.ExpandedString(Index: integer): string;
var
  Line: String;
  CharWidths: TPhysicalCharWidths;
  i, j, l: Integer;
  HasTab: Boolean;
begin
// this is only used by trimmer.lengthOfLongestLine / which is not called, if a tab module is present
  Line := NextLines[Index];
  if (Line = '') or (not GetHasTabs(PChar(Line))) then begin
    Result := Line;
    // xxx wrong double width // none latin ...
    //TabData.SetLineInfo(Index, Length(Result, False));
  end else begin
    CharWidths := GetPhysicalCharWidths(Pchar(Line), length(Line), Index);
    l := 0;
    for i := 0 to length(CharWidths)-1 do
      l := l + (CharWidths[i] and PCWMask);
    Result := '';
    SetLength(Result, l);

    l := 1;
    HasTab := False;
    for i := 1 to length(CharWidths) do begin
      if Line[i] <> #9 then begin
        Result[l] := Line[i];
        inc(l);
      end else begin
        HasTab := True;
        for j := 1 to (CharWidths[i-1] and PCWMask) do begin
          Result[l] := ' ';
          inc(l);
        end;
      end;
    end;
    TabData.SetLineInfo(Index, Length(Result), HasTab);
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
    TabData.SetLineInfo(Index, 0, False);
  end else begin
    i := length(Line);
    SetLength(CharWidths{%H-}, i);
    DoGetPhysicalCharWidths(Pchar(Line), i, Index, @CharWidths[0]);
    Result := 0;
    for i := 0 to length(CharWidths)-1 do
      Result := Result + (CharWidths[i] and PCWMask);

    TabData.SetLineInfo(Index, Result, FLastLineHasTab);
  end;
end;

function TSynEditStringTabExpander.GetExpandedString(Index: integer): string;
begin
  if (Index >= 0) and (Index < Count) then begin
    if TabData.HasTab[Index] then
      Result := ExpandedString(Index)
    else
      Result := NextLines[Index];
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

function TSynEditStringTabExpander.GetKnownLengthOfLine(AnIndex: IntIdx
  ): integer;
begin
  Result := TabData[fIndexOfLongestLine];
end;

function TSynEditStringTabExpander.GetIndexOfLongestLine(AStartIndex,
  AnEndIndex: IntIdx; out ALen: integer): integer;
var
  Line: PChar;
  LineLen: Integer;
  CharWidths: PPhysicalCharWidth;
  i, j, m: Integer;
begin
  Result := 0;
  ALen := 0;
  try
    m := 0;
    CharWidths := nil;
    for i := AStartIndex to AnEndIndex do begin
      j := TabData[i];
      if j = LINE_LEN_UNKNOWN then begin
        // embedd a copy of ExpandedStringLength
        // allows one to re-use CharWidths
        Line := NextLines.GetPChar(i,LineLen); // NextLines[i];
        j := 0;
        if (LineLen = 0) then begin
          TabData.SetLineInfo(i, LineLen, False);
        end else begin
          if LineLen > m then begin
            ReAllocMem(CharWidths, LineLen * SizeOf(TPhysicalCharWidth));
            m := LineLen;
          end;
          DoGetPhysicalCharWidths(Line, LineLen, i, CharWidths);
          j := FLastLinePhysLen;
          TabData.SetLineInfo(i, j, FLastLineHasTab);
        end;
      end;

      if j > ALen then begin
        ALen := j;
        Result := i;
      end;
    end;
  finally
    ReAllocMem(CharWidths, 0);
  end;
end;

end.


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
unit SynPluginSyncroEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, Forms, Graphics, SynEditMiscClasses,
  LCLType, SynEdit, SynPluginSyncronizedEditBase, LazSynEditText, SynEditMiscProcs,
  SynEditMouseCmds, SynEditKeyCmds, SynEditTypes, LCLIntf, LazUTF8;

type

  TSynPluginSyncroEditLowerLineCacheEntry = record
    LineIndex: Integer;
    LineText: String;
  end;

  { TSynPluginSyncroEditLowerLineCache }

  TSynPluginSyncroEditLowerLineCache = class
  private
    FCaseSensitive: boolean;
    FLines: TSynEditStringsLinked;
    FLower: Array of TSynPluginSyncroEditLowerLineCacheEntry;
    function GetLowLine(aIndex: Integer): String;
    procedure SetLines(const AValue: TSynEditStringsLinked);
  protected
    Procedure LineTextChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
  public
    destructor Destroy; override;
    procedure Clear;
    property Lines: TSynEditStringsLinked read FLines write SetLines;
    property LowLines[aIndex: Integer]: String read GetLowLine; default;
  end;

  TSynPluginSyncroEditWordsHashEntry = record
    Count, Hash: Integer;
    LineIdx, BytePos, Len: Integer;
    Next: Integer;
    GrpId: Integer;
  end;
  PSynPluginSyncroEditWordsHashEntry = ^TSynPluginSyncroEditWordsHashEntry;

  { TSynPluginSyncroEditWordsList }

  TSynPluginSyncroEditWordsList = class
  private
    FCount: Integer;
    FFirstUnused, FFirstGap: Integer;
    function GetItem(aIndex: Integer): TSynPluginSyncroEditWordsHashEntry;
    procedure SetItem(aIndex: Integer; const AValue: TSynPluginSyncroEditWordsHashEntry);
  protected
    FList: Array of TSynPluginSyncroEditWordsHashEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function InsertEntry(aEntry: TSynPluginSyncroEditWordsHashEntry) : Integer;
    procedure DeleteEntry(aIndex: Integer);
    property Item[aIndex: Integer]: TSynPluginSyncroEditWordsHashEntry
      read GetItem write SetItem; default;
    property Count: Integer read FCount;
  end;

  { TSynPluginSyncroEditWordsHash }

  TSynPluginSyncroEditWordsHash = class
  private
    FLowerLines: TSynPluginSyncroEditLowerLineCache;
    FTableSize: Integer;
    FTable: Array of TSynPluginSyncroEditWordsHashEntry;
    FEntryCount: Integer;
    FWordCount, FMultiWordCount: Integer;
    FNextList: TSynPluginSyncroEditWordsList;

    function CalcHash(aWord: PChar;aLen: Integer): Integer;
    function CompareEntry(aEntry1, aEntry2: TSynPluginSyncroEditWordsHashEntry;
                          aWord1, aWord2: PChar): Boolean;
    function GetEntry(aModHash, aIndex: Integer): TSynPluginSyncroEditWordsHashEntry;

    procedure InsertEntry(aEntry: TSynPluginSyncroEditWordsHashEntry; aWord: PChar);
    procedure DeleteEntry(aEntry: TSynPluginSyncroEditWordsHashEntry; aWord: PChar);

    procedure Resize(ANewSize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    // Excpects PChat to an already lowercase word
    procedure AddWord(aLineIdx, aBytePos, aLen: Integer; aWord: PChar);
    procedure RemoveWord(aLen: Integer; aWord: PChar);
    function  GetWord(aWord: PChar; aLen: Integer): TSynPluginSyncroEditWordsHashEntry;
    function  GetWordP(aWord: PChar; aLen: Integer): PSynPluginSyncroEditWordsHashEntry;
    function  GetWordModHash(aWord: PChar; aLen: Integer): Integer;

    property LowerLines: TSynPluginSyncroEditLowerLineCache
      read FLowerLines write FLowerLines;
    property HashEntry[aModHash, aIndex: Integer]: TSynPluginSyncroEditWordsHashEntry
      read GetEntry;
    property HashSize: Integer read FTableSize;
    property EntryCount: Integer read FEntryCount;
    property WordCount: Integer read FWordCount;
    property MultiWordCount: Integer read FMultiWordCount;
  end;

  { TSynPluginSyncroEditMarkup }

  TSynPluginSyncroEditMarkup = class(TSynPluginSyncronizedEditMarkup)
  private
    FGlyphAtLine: Integer;
    FGlyphLastLine: Integer;
    FGutterGlyph: TBitmap;
    function GetGutterGlyphRect(aLine: Integer): TRect;
    function GetGutterGlyphRect: TRect;
    function GetGutterGlyphPaintLine: Integer;
    procedure SetGlyphAtLine(const AValue: Integer);
    procedure SetGutterGlyph(const AValue: TBitmap);
    procedure DoInvalidate;
  protected
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoEnabledChanged(Sender: TObject); override;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    procedure EndMarkup; override;

    property GlyphAtLine: Integer read FGlyphAtLine write SetGlyphAtLine;       // -1 for caret
    property GutterGlyph: TBitmap read FGutterGlyph write SetGutterGlyph;
    property GutterGlyphRect: TRect read GetGutterGlyphRect;
  end;

  { TSynPluginSyncroEditMouseActions }

  TSynPluginSyncroEditMouseActions = class(TSynEditMouseActions)
  public
    procedure ResetDefaults; override;
  end;

  { TSynEditSyncroEditKeyStrokesSelecting }

  TSynEditSyncroEditKeyStrokesSelecting = class(TSynEditKeyStrokes)
  public
    procedure ResetDefaults; override;
  end;

  { TSynEditSyncroEditKeyStrokes }

  TSynEditSyncroEditKeyStrokes = class(TSynEditKeyStrokes)
  public
    procedure ResetDefaults; override;
  end;

  { TSynEditSyncroEditKeyStrokesOffCell }

  TSynEditSyncroEditKeyStrokesOffCell = class(TSynEditKeyStrokes)
  public
    procedure ResetDefaults; override;
  end;

  TSynPluginSyncroEditModes = (spseIncative, spseSelecting, spseEditing, spseInvalid);
  { TSynPluginSyncroEdit }

  TSynPluginSyncroEdit = class(TSynPluginCustomSyncroEdit)
  private
    FCaseSensitive: boolean;
    FGutterGlyph: TBitmap;
    FLowerLines: TSynPluginSyncroEditLowerLineCache;
    FOnBeginEdit: TNotifyEvent;
    FOnEndEdit: TNotifyEvent;
    FOnModeChange: TNotifyEvent;
    FWordIndex: TSynPluginSyncroEditWordsHash;
    FWordScanCount: Integer;
    FCallQueued: Boolean;
    FEditModeQueued: Boolean;
    FLastSelStart, FLastSelEnd: TPoint;
    FParsedStart, FParsedStop: TPoint;
    FMouseActions: TSynPluginSyncroEditMouseActions;
    FMode: TSynPluginSyncroEditModes;

    FKeystrokesSelecting: TSynEditKeyStrokes;
    FKeystrokes, FKeyStrokesOffCell: TSynEditKeyStrokes;
    procedure SetCaseSensitive(AValue: boolean);
    procedure SetKeystrokesSelecting(const AValue: TSynEditKeyStrokes);
    procedure SetKeystrokes(const AValue: TSynEditKeyStrokes);
    procedure SetKeystrokesOffCell(const AValue: TSynEditKeyStrokes);
    function  GetMarkup: TSynPluginSyncroEditMarkup;
    function  Scan(AFrom, aTo: TPoint; BackWard: Boolean): TPoint;
    procedure SetGutterGlyph(const AValue: TBitmap);
    procedure SetMode(AValue: TSynPluginSyncroEditModes);
    function  UnScan(AFrom, aTo: TPoint; BackWard: Boolean): TPoint;
    procedure StartSyncroMode;
    procedure StopSyncroMode;
  protected
    procedure DoImageChanged(Sender: TObject);
    function  CreateMarkup: TSynPluginSyncronizedEditMarkup; override;
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoScanSelection(Data: PtrInt);
    procedure DoOnDeactivate; override;
    procedure DoPreActiveEdit(aX, aY, aCount, aLineBrkCnt: Integer; aUndoRedo: Boolean);
      override;

    function MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
                         HandleActionProc: TSynEditMouseActionHandler): Boolean;
    function DoHandleMouseAction(AnAction: TSynEditMouseAction;
                                 var AnInfo: TSynEditMouseActionInfo): Boolean;

    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoClear; override;
    procedure DoModeChanged;

    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);

    property Markup: TSynPluginSyncroEditMarkup read GetMarkup;
    property Mode: TSynPluginSyncroEditModes read FMode write SetMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive default false;
    property GutterGlyph: TBitmap read FGutterGlyph write SetGutterGlyph;
    property KeystrokesSelecting: TSynEditKeyStrokes
      read FKeystrokesSelecting write SetKeystrokesSelecting;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes;
    property KeystrokesOffCell: TSynEditKeyStrokes
      read FKeystrokesOffCell write SetKeystrokesOffCell;
    property OnModeChange: TNotifyEvent read FOnModeChange write FOnModeChange;
    property OnBeginEdit: TNotifyEvent read FOnBeginEdit write FOnBeginEdit;
    property OnEndEdit: TNotifyEvent read FOnEndEdit write FOnEndEdit;

  published
    property Enabled;
    property MarkupInfo;
    property MarkupInfoCurrent;
    property MarkupInfoSync;
    property MarkupInfoArea;
    property OnActivate;
    property OnDeactivate;
    property Editor;
  end;

const
  emcSynPSyncroEdGutterGlyph         = emcPluginFirstSyncro +  0;

  emcSynPSyncroEdCount               = 1;

  ecSynPSyncroEdStart              = ecPluginFirstSyncro +  0;

  ecSynPSyncroEdNextCell           = ecPluginFirstSyncro +  1;
  ecSynPSyncroEdNextCellSel        = ecPluginFirstSyncro +  2;
  ecSynPSyncroEdPrevCell           = ecPluginFirstSyncro +  3;
  ecSynPSyncroEdPrevCellSel        = ecPluginFirstSyncro +  4;
  ecSynPSyncroEdCellHome           = ecPluginFirstSyncro +  5;
  ecSynPSyncroEdCellEnd            = ecPluginFirstSyncro +  6;
  ecSynPSyncroEdCellSelect         = ecPluginFirstSyncro +  7;
  ecSynPSyncroEdEscape             = ecPluginFirstSyncro +  8;
  ecSynPSyncroEdNextFirstCell      = ecPluginFirstSyncro +  9;
  ecSynPSyncroEdNextFirstCellSel   = ecPluginFirstSyncro + 10;
  ecSynPSyncroEdPrevFirstCell      = ecPluginFirstSyncro + 11;
  ecSynPSyncroEdPrevFirstCellSel   = ecPluginFirstSyncro + 12;

  // If extending the list, reserve space in SynEditKeyCmds

  ecSynPSyncroEdCount              = 13;

implementation

const
  MAX_CACHE = 50; // Amount of lower-cased lines cached
  MAX_SYNC_ED_WORDS = 50;// 250;
  MAX_WORDS_PER_SCAN = 5000;
  MIN_PROCESS_MSG_TIME = (1/86400)/15;

Operator = (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y = P2.Y) and (P1.X = P2.X);
end;

Operator < (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y < P2.Y) or ( (P1.Y = P2.Y) and (P1.X < P2.X) );
end;

Operator <= (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y < P2.Y) or ( (P1.Y = P2.Y) and (P1.X <= P2.X) );
end;

Operator > (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y > P2.Y) or ( (P1.Y = P2.Y) and (P1.X > P2.X) );
end;

Operator >= (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y > P2.Y) or ( (P1.Y = P2.Y) and (P1.X >= P2.X) );
end;

{ TSynPluginSyncroEditLowerLineCache }

function TSynPluginSyncroEditLowerLineCache.GetLowLine(aIndex: Integer): String;
var
  i, l: Integer;

begin
  if FCaseSensitive then begin
    Result := FLines[aIndex];
    exit;
  end;

  l := length(FLower);
  for i := 0 to l-1 do
    if FLower[i].LineIndex = aIndex then
      exit(FLower[i].LineText);
  Result := UTF8LowerCase(FLines[aIndex]);
  if Result = '' then
    exit;
  if l < MAX_CACHE then begin
    inc(l);
    SetLength(FLower, l);
  end;
  for i := l-1 downto 1 do begin
    FLower[i].LineIndex := FLower[i-1].LineIndex;
    FLower[i].LineText  := FLower[i-1].LineText;
  end;
  FLower[0].LineIndex := aIndex;
  FLower[0].LineText  := Result;
end;

procedure TSynPluginSyncroEditLowerLineCache.SetLines(
  const AValue: TSynEditStringsLinked);
begin
  Clear;
  if FLines <> nil then begin
    fLines.RemoveChangeHandler(senrLineChange, @LineTextChanged);
    fLines.RemoveChangeHandler(senrLineCount, @LineTextChanged);
  end;
  FLines := AValue;
  if FLines <> nil then begin
    fLines.AddChangeHandler(senrLineChange, @LineTextChanged);
    fLines.AddChangeHandler(senrLineCount, @LineTextChanged);
  end;
end;

procedure TSynPluginSyncroEditLowerLineCache.LineTextChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
begin
  Clear;
end;

destructor TSynPluginSyncroEditLowerLineCache.Destroy;
begin
  Lines := nil;
  Clear;
  inherited Destroy;
end;

procedure TSynPluginSyncroEditLowerLineCache.Clear;
begin
  FLower := nil;
end;

{ TSynPluginSyncroEditWordsList }

function TSynPluginSyncroEditWordsList.GetItem(aIndex: Integer): TSynPluginSyncroEditWordsHashEntry;
begin
  Result := FList[aIndex];
end;

procedure TSynPluginSyncroEditWordsList.SetItem(aIndex: Integer;
  const AValue: TSynPluginSyncroEditWordsHashEntry);
begin
  FList[aIndex] := AValue;
end;

constructor TSynPluginSyncroEditWordsList.Create;
begin
  inherited;
  clear;
end;

destructor TSynPluginSyncroEditWordsList.Destroy;
begin
  inherited Destroy;
  clear;
end;

procedure TSynPluginSyncroEditWordsList.Clear;
begin
  FList := nil;
  FFirstGap := -1;
  FFirstUnused := 0;
  FCount := 0;
end;

function TSynPluginSyncroEditWordsList.InsertEntry(aEntry: TSynPluginSyncroEditWordsHashEntry): Integer;
begin
  inc(FCount);
  if FFirstGap >= 0 then begin
    Result := FFirstGap;
    FFirstGap := FList[Result].Next;
  end else begin
    if FFirstUnused >= length(FList) then
      SetLength(FList, Max(1024, length(FList)) * 4);
    Result := FFirstUnused;
    inc(FFirstUnused);
  end;
  FList[Result] := aEntry;
end;

procedure TSynPluginSyncroEditWordsList.DeleteEntry(aIndex: Integer);
begin
  dec(FCount);
  FList[aIndex].Next := FFirstGap;
  FFirstGap := aIndex;
end;

{ TSynPluginSyncroEditWordsHash }

function TSynPluginSyncroEditWordsHash.CalcHash(aWord: PChar; aLen: Integer): Integer;
var
  v, n, p, a, b, c, c1, i: Integer;
begin
  a := 0;
  b := 0;
  c := 0;
  c1 := 0;
  n := 1;
  p := 0;
  i := aLen;
  while i > 0 do begin
    v  := ord(aWord^);
    a := a     + v * (1 + (n mod 8));
    if a > 550 then a := a mod 550;
    b := b * 3 + v * n - p;
    if b > 550 then b := b mod 550;
    c1 := c1   + v * (1 + (a mod 11));
    c := c + c1;
    if c > 550 then c := c mod 550;
    dec(i);
    inc(aWord);
    inc(n);
    p := v;
  end;

  Result := (((aLen mod 11) * 550 + b) * 550 + c) * 550 + a;
end;

function TSynPluginSyncroEditWordsHash.CompareEntry(aEntry1,
  aEntry2: TSynPluginSyncroEditWordsHashEntry; aWord1, aWord2: PChar): Boolean;
var
  Line1, Line2: String;
begin
  Result := (aEntry1.Len = aEntry2.Len) and (aEntry1.Hash = aEntry2.Hash);
  if not Result then exit;

  if aWord1 = nil then begin
    Line1 := FLowerLines[aEntry1.LineIdx];
    aWord1 := @Line1[aEntry1.BytePos];
  end;
  if aWord2 = nil then begin
    Line2 := FLowerLines[aEntry2.LineIdx];
    aWord2 := @Line2[aEntry2.BytePos];
  end;

  Result := CompareMem(aWord1, aWord2, aEntry1.Len);
end;

function TSynPluginSyncroEditWordsHash.GetEntry(aModHash,
  aIndex: Integer): TSynPluginSyncroEditWordsHashEntry;
begin
  Result:= FTable[aModHash];
  while aIndex > 0 do begin
    if Result.Next < 0 then begin
      Result.Count := 0;
      Result.Hash := -1;
      exit;
    end;
    Result := FNextList[Result.Next];
    dec(aIndex);
  end;
end;

procedure TSynPluginSyncroEditWordsHash.InsertEntry
  (aEntry: TSynPluginSyncroEditWordsHashEntry;  aWord: PChar);
var
  j: LongInt;
  ModHash: Integer;
begin
  aEntry.GrpId := 0;
  if (FEntryCount >= FTableSize * 2 div 3) or (FNextList.Count > FTableSize div 8) then
    Resize(Max(FTableSize, 1024) * 4);

  ModHash := aEntry.Hash mod FTableSize;

  if (FTable[ModHash].Count > 0) then begin
    if CompareEntry(aEntry, FTable[ModHash], aWord, nil) then begin
      if FTable[ModHash].Count = 1 then
        inc(FMultiWordCount);
      FTable[ModHash].Count := FTable[ModHash].Count + aEntry.Count;
      exit;
    end;

    j := FTable[ModHash].Next;
    while j >= 0 do begin
      if CompareEntry(aEntry, FNextList[j], aWord, nil) then begin
        if FNextList[j].Count = 1 then
          inc(FMultiWordCount);
        FNextList.FList[j].Count := FNextList.FList[j].Count + aEntry.Count;
        exit;
      end;
      j := FNextList[j].Next;
    end;

    j := FNextList.InsertEntry(aEntry);
    FNextList.FList[j].Next := FTable[ModHash].Next;
    FTable[ModHash].Next := j;
    inc(FWordCount);

    exit;
  end;

  inc(FEntryCount);
  inc(FWordCount);
    //if (FEntryCount<20) or (FEntryCount mod 8192=0) then debugln(['entry add ', FEntryCount]);
  FTable[ModHash] := aEntry;
  FTable[ModHash].Next:= -1;
end;

procedure TSynPluginSyncroEditWordsHash.DeleteEntry(aEntry: TSynPluginSyncroEditWordsHashEntry;
  aWord: PChar);
var
  j, i: Integer;
  ModHash: Integer;
begin
  ModHash := aEntry.Hash mod FTableSize;

  if (FTable[ModHash].Count > 0) then begin
    if CompareEntry(aEntry, FTable[ModHash], aWord, nil) then begin
      FTable[ModHash].Count := FTable[ModHash].Count - 1;
      if FTable[ModHash].Count = 0 then begin
        j := FTable[ModHash].Next;
        if j >= 0 then begin
          FTable[ModHash] := FNextList[j];
          FNextList.DeleteEntry(j);
        end
        else
          dec(FEntryCount);
        dec(FWordCount);
      end
      else if FTable[ModHash].Count = 1 then
        dec(FMultiWordCount);
      exit;
    end;

    j := FTable[ModHash].Next;
    while j >= 0 do begin
      if CompareEntry(aEntry, FNextList[j], aWord, nil) then begin
        FNextList.FList[j].Count := FNextList.FList[j].Count - 1;
        if FNextList[j].Count = 0 then begin
          i := FNextList[j].Next;
          if i >= 0 then begin
            FNextList[j] := FNextList[i];
            FNextList.DeleteEntry(i);
          end;
          dec(FWordCount);
        end
        else if FNextList[j].Count = 1 then
          dec(FMultiWordCount);
        exit;
      end;
      j := FNextList[j].Next;
    end;

  end;
  // ?? there was no entry ??
end;

procedure TSynPluginSyncroEditWordsHash.Resize(ANewSize: Integer);
var
  OldTable: Array of TSynPluginSyncroEditWordsHashEntry;
  OldSize, i, j, k: Integer;
begin
  FEntryCount := 0;
  FWordCount := 0;
  FMultiWordCount := 0;
  if FTableSize = 0 then begin
    SetLength(FTable, ANewSize);
    FTableSize := ANewSize;
    exit;
  end;

  //debugln(['TSynPluginSyncroEditWordsHash.Resize ', ANewSize]);
  OldSize := FTableSize;
  SetLength(OldTable, FTableSize);
  System.Move(FTable[0], OldTable[0], FTableSize * SizeOf(TSynPluginSyncroEditWordsHashEntry));
  FillChar(FTable[0], FTableSize * SizeOf(TSynPluginSyncroEditWordsHashEntry), 0);
  SetLength(FTable, ANewSize);
  FTableSize := ANewSize;

  for i := 0 to OldSize - 1 do begin
    if OldTable[i].Count > 0 then begin
      InsertEntry(OldTable[i], nil);
      j := OldTable[i].Next;
      while j >= 0 do begin
        InsertEntry(FNextList[j], nil);
        k := j;
        j := FNextList[j].Next;
        FNextList.DeleteEntry(k);
      end;
    end;
  end;
end;

constructor TSynPluginSyncroEditWordsHash.Create;
begin
  inherited;
  FNextList := TSynPluginSyncroEditWordsList.Create;
  Clear;
end;

destructor TSynPluginSyncroEditWordsHash.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FNextList);
end;

procedure TSynPluginSyncroEditWordsHash.Clear;
begin
  FTable := nil;
  FTableSize := 0;
  FEntryCount := 0;
  FWordCount := 0;
  FMultiWordCount := 0;
  FNextList.Clear;
end;

procedure TSynPluginSyncroEditWordsHash.AddWord(aLineIdx, aBytePos, aLen: Integer;
  aWord: PChar);
var
  NewEntry: TSynPluginSyncroEditWordsHashEntry;
begin
  NewEntry.Hash := CalcHash(aWord, aLen);
  NewEntry.LineIdx := aLineIdx;
  NewEntry.BytePos := aBytePos;
  NewEntry.Len := aLen;
  NewEntry.Count := 1;
  InsertEntry(NewEntry, aWord);
end;

procedure TSynPluginSyncroEditWordsHash.RemoveWord(aLen: Integer; aWord: PChar);
var
  OldEntry: TSynPluginSyncroEditWordsHashEntry;
begin
  OldEntry.Count := 1;
  OldEntry.Hash := CalcHash(aWord, aLen);
  oldEntry.Len := aLen;
  DeleteEntry(OldEntry, aWord);
end;

function TSynPluginSyncroEditWordsHash.GetWord(aWord: PChar;
  aLen: Integer): TSynPluginSyncroEditWordsHashEntry;
var
  SearchEntry: TSynPluginSyncroEditWordsHashEntry;
begin
  Result.Hash := -1;
  Result.Count:= 0;
  if FTableSize < 1 then exit;

  SearchEntry.Hash := CalcHash(aWord, aLen);
  SearchEntry.Len := aLen;
  Result := FTable[SearchEntry.Hash mod FTableSize];
  while Result.Count > 0 do begin
    if CompareEntry(Result, SearchEntry, nil, aWord) then exit;
    if Result.Next < 0 then break;
    Result := FNextList[Result.Next];
  end;
  Result.Hash := -1;
  Result.Count:= 0;
end;

function TSynPluginSyncroEditWordsHash.GetWordP(aWord: PChar;
  aLen: Integer): PSynPluginSyncroEditWordsHashEntry;
var
  SearchEntry: TSynPluginSyncroEditWordsHashEntry;
begin
  Result := nil;
  if FTableSize < 1 then exit;

  SearchEntry.Hash := CalcHash(aWord, aLen);
  SearchEntry.Len := aLen;
  Result := @FTable[SearchEntry.Hash mod FTableSize];
  while Result^.Count > 0 do begin
    if CompareEntry(Result^, SearchEntry, nil, aWord) then exit;
    if Result^.Next < 0 then break;
    Result := @FNextList.FList[Result^.Next];
  end;
  Result := nil;
end;

function TSynPluginSyncroEditWordsHash.GetWordModHash(aWord: PChar; aLen: Integer): Integer;
begin
  if FTableSize < 1 then exit(-1);
  Result := CalcHash(aWord, aLen) mod FTableSize;
end;

{ TSynPluginSyncroEditMarkup }

procedure TSynPluginSyncroEditMarkup.DoInvalidate;
var
  rcInval: TRect;
begin
  if not Enabled then exit;
  if FGlyphLastLine <> -2 then begin
    if SynEdit.HandleAllocated then begin
      rcInval := GetGutterGlyphRect(FGlyphLastLine);
      // and make sure we trigger the Markup // TODO: triigger markup on gutter paint too
      rcInval.Right := Max(rcInval.Right, TCustomSynEdit(SynEdit).ClientRect.Right);
      InvalidateRect(SynEdit.Handle, @rcInval, False);
    end;
  end;
  if SynEdit.HandleAllocated then begin
    rcInval := GetGutterGlyphRect;
    // and make sure we trigger the Markup // TODO: triigger markup on gutter paint too
    rcInval.Right := Max(rcInval.Right, TCustomSynEdit(SynEdit).ClientRect.Right);
    InvalidateRect(SynEdit.Handle, @rcInval, False);
  end;
end;

procedure TSynPluginSyncroEditMarkup.DoCaretChanged(Sender: TObject);
begin
  inherited DoCaretChanged(Sender);
  DoInvalidate;
end;

procedure TSynPluginSyncroEditMarkup.DoTopLineChanged(OldTopLine: Integer);
var
  rcInval: TRect;
begin
  inherited DoTopLineChanged(OldTopLine);
  // Glyph may have drawn up to one Line above
  if FGlyphLastLine > 1 then begin
    if SynEdit.HandleAllocated then begin
      rcInval := GetGutterGlyphRect(FGlyphLastLine - 1);
      InvalidateRect(SynEdit.Handle, @rcInval, False);
    end;
  end;
  DoInvalidate;
end;

procedure TSynPluginSyncroEditMarkup.DoLinesInWindoChanged(OldLinesInWindow: Integer);
begin
  inherited DoLinesInWindoChanged(OldLinesInWindow);
  DoInvalidate;
end;

procedure TSynPluginSyncroEditMarkup.DoEnabledChanged(Sender: TObject);
var
  rcInval: TRect;
begin
  inherited DoEnabledChanged(Sender);
  if not Enabled then begin
    if FGlyphLastLine <> -2 then begin
      if SynEdit.HandleAllocated then begin
        rcInval := GetGutterGlyphRect(FGlyphLastLine);
        InvalidateRect(SynEdit.Handle, @rcInval, False);
      end;
    end;
    FGlyphLastLine := -2;
  end
  else
    DoInvalidate;
end;

procedure TSynPluginSyncroEditMarkup.EndMarkup;
var
  src, dst: TRect;
begin
  inherited EndMarkup;
  if (FGutterGlyph.Height > 0) then begin
    src :=  Classes.Rect(0, 0, FGutterGlyph.Width, FGutterGlyph.Height);
    dst := GutterGlyphRect;
    FGlyphLastLine := GetGutterGlyphPaintLine;
    TCustomSynEdit(SynEdit).Canvas.CopyRect(dst, FGutterGlyph.Canvas, src);
  end;
end;

procedure TSynPluginSyncroEditMarkup.SetGutterGlyph(const AValue: TBitmap);
begin
  if FGutterGlyph = AValue then exit;
  if FGutterGlyph = nil then
    FGutterGlyph := TBitMap.Create;
  FGutterGlyph.Assign(AValue);
  DoInvalidate;
end;

function TSynPluginSyncroEditMarkup.GetGutterGlyphRect(aLine: Integer): TRect;
begin
  Result :=  Classes.Rect(0, 0, FGutterGlyph.Width, FGutterGlyph.Height);
  if aLine = -1 then
    aLine := TCustomSynEdit(SynEdit).CaretY;
  Result.Top := Max( Min( RowToScreenRow(aLine)
                          * TCustomSynEdit(SynEdit).LineHeight,
                          TCustomSynEdit(SynEdit).ClientHeight - FGutterGlyph.Height),
                          0);
  Result.Bottom := Result.Bottom + Result.Top;
end;

function TSynPluginSyncroEditMarkup.GetGutterGlyphRect: TRect;
begin
  Result := GetGutterGlyphRect(GlyphAtLine);
end;

function TSynPluginSyncroEditMarkup.GetGutterGlyphPaintLine: Integer;
var
  i: Integer;
begin
  Result := FGlyphAtLine;
  if Result < 0 then
    Result := TCustomSynEdit(SynEdit).CaretY;
  if Result < TopLine then
    Result := TopLine;
  i := ScreenRowToRow(LinesInWindow);
  if Result > i then
    Result := i;
end;

procedure TSynPluginSyncroEditMarkup.SetGlyphAtLine(const AValue: Integer);
begin
  if FGlyphAtLine = AValue then exit;
  FGlyphAtLine := AValue;
  DoInvalidate;
end;

constructor TSynPluginSyncroEditMarkup.Create(ASynEdit: TSynEditBase);
begin
  FGutterGlyph := TBitMap.Create;
  FGlyphLastLine := -2;
  inherited;
end;

destructor TSynPluginSyncroEditMarkup.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FGutterGlyph);
end;

{ TSynPluginSyncroEdit }

function TSynPluginSyncroEdit.Scan(AFrom, aTo: TPoint; BackWard: Boolean): TPoint;
var
  x2: Integer;
  Line: String;
begin
  Result := AFrom;
  if BackWard then begin
    Line := FLowerLines[AFrom.y - 1];
    while (AFrom >= aTo) do begin
      AFrom.x :=  WordBreaker.PrevWordEnd(Line, AFrom.x, True);
      if AFrom.x < 0 then begin
        dec(AFrom.y);
        Line := FLowerLines[AFrom.y-1];
        AFrom.x := length(Line) + 1;
        continue;
      end;
      x2 :=  WordBreaker.PrevWordStart(Line, AFrom.x, True);
      if (AFrom.y > ATo.y) or (x2 >= ATo.x) then begin
        FWordIndex.AddWord(AFrom.y - 1, x2, AFrom.x - x2, @Line[x2]);
        Result := AFrom;
        Result.x := x2;
        inc(FWordScanCount);
        if FWordScanCount > MAX_WORDS_PER_SCAN then break;
      end;
      AFrom.x := x2;
    end;
  end
  else begin
    Line := FLowerLines[AFrom.y - 1];
    while (AFrom <= aTo) do begin
      AFrom.x :=  WordBreaker.NextWordStart(Line, AFrom.x, True);
      if AFrom.x < 0 then begin
        inc(AFrom.y);
        AFrom.x := 1;
        Line := FLowerLines[AFrom.y-1];
        continue;
      end;
      x2 :=  WordBreaker.NextWordEnd(Line, AFrom.x, True);
      if (AFrom.y < ATo.y) or (x2 <= ATo.x) then begin
        FWordIndex.AddWord(AFrom.y - 1, AFrom.x, x2-AFrom.x, @Line[AFrom.x]);
        Result := AFrom;
        Result.x := x2;
        inc(FWordScanCount);
        if FWordScanCount > MAX_WORDS_PER_SCAN then break;
      end;
      AFrom.x := x2;
    end;
  end;
end;

procedure TSynPluginSyncroEdit.SetKeystrokesSelecting(const AValue: TSynEditKeyStrokes);
begin
  if AValue = nil then
    FKeystrokesSelecting.Clear
  else
    FKeystrokesSelecting.Assign(AValue);
end;

procedure TSynPluginSyncroEdit.SetCaseSensitive(AValue: boolean);
begin
  FCaseSensitive := AValue;
  FLowerLines.FCaseSensitive := AValue;
  FLowerLines.Clear;
end;

procedure TSynPluginSyncroEdit.SetKeystrokes(const AValue: TSynEditKeyStrokes);
begin
  if AValue = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(AValue);
end;

procedure TSynPluginSyncroEdit.SetKeystrokesOffCell(const AValue: TSynEditKeyStrokes);
begin
  if AValue = nil then
    FKeyStrokesOffCell.Clear
  else
    FKeyStrokesOffCell.Assign(AValue);
end;

function TSynPluginSyncroEdit.GetMarkup: TSynPluginSyncroEditMarkup;
begin
  Result := TSynPluginSyncroEditMarkup(FMarkup);
end;

procedure TSynPluginSyncroEdit.SetGutterGlyph(const AValue: TBitmap);
begin
  if FGutterGlyph = AValue then exit;
  if FGutterGlyph = nil then begin
    FGutterGlyph := TBitMap.Create;
    FGutterGlyph.OnChange := @DoImageChanged;
  end;
  FGutterGlyph.Assign(AValue);
end;

procedure TSynPluginSyncroEdit.SetMode(AValue: TSynPluginSyncroEditModes);
begin
  if Mode = AValue then Exit;
  if (FMode= spseEditing) and Assigned(FOnEndEdit) then
    FOnEndEdit(Self);
  FMode := AValue;
  if (FMode= spseEditing) and Assigned(FOnBeginEdit) then
    FOnBeginEdit(Self);
  DoModeChanged;
end;

function TSynPluginSyncroEdit.UnScan(AFrom, aTo: TPoint; BackWard: Boolean): TPoint;
var
  x2: Integer;
  Line: String;
begin
  Result := AFrom;
  if BackWard then begin
    Line := FLowerLines[AFrom.y - 1];
    while (AFrom > aTo) do begin
      AFrom.x :=  WordBreaker.PrevWordEnd(Line, AFrom.x, True);
      if AFrom.x < 0 then begin
        dec(AFrom.y);
        Line := FLowerLines[AFrom.y-1];
        AFrom.x := length(Line) + 1;
        continue;
      end;
      x2 :=  WordBreaker.PrevWordStart(Line, AFrom.x, True);
      FWordIndex.RemoveWord(AFrom.x - x2, @Line[x2]);
      AFrom.x := x2;
      Result := AFrom;
      inc(FWordScanCount);
      if FWordScanCount > MAX_WORDS_PER_SCAN then break;
    end;
  end
  else begin
    Line := FLowerLines[AFrom.y - 1];
    while (AFrom < aTo) do begin
      AFrom.x :=  WordBreaker.NextWordStart(Line, AFrom.x, True);
      if AFrom.x < 0 then begin
        inc(AFrom.y);
        AFrom.x := 1;
        Line := FLowerLines[AFrom.y-1];
        continue;
      end;
      x2 :=  WordBreaker.NextWordEnd(Line, AFrom.x, True);
      FWordIndex.RemoveWord(x2-AFrom.x, @Line[AFrom.x]);
      AFrom.x := x2;
      Result := AFrom;
      inc(FWordScanCount);
      if FWordScanCount > MAX_WORDS_PER_SCAN then break;
    end;
  end;
end;

procedure TSynPluginSyncroEdit.StartSyncroMode;
var
  Pos, EndPos: TPoint;
  Line: String;
  x2, g: Integer;
  entry: PSynPluginSyncroEditWordsHashEntry;
  f: Boolean;
begin
  if FCallQueued then begin
    FEditModeQueued := True;
    exit;
  end;
  FEditModeQueued := False;
  if FWordIndex.MultiWordCount = 0 then exit;

  Mode :=  spseEditing;
  Active := True;
  AreaMarkupEnabled := True;
  SetUndoStart;

  // Reset them, since Selectionchanges are not tracked during spseEditing
  FLastSelStart := Point(-1,-1);
  FLastSelEnd := Point(-1,-1);

  Pos := SelectionObj.FirstLineBytePos;
  EndPos := SelectionObj.LastLineBytePos;

  with Cells.AddNew do begin
    LogStart := Pos;
    LogEnd := EndPos;
    Group := -1;
  end;
  MarkupArea.CellGroupForArea := -1;
  Markup.GlyphAtLine := Pos.y;

  g := 1;
  Line := FLowerLines[Pos.y-1];
  while (Pos <= EndPos) do begin
    Pos.x :=  WordBreaker.NextWordStart(Line, Pos.x, True);
    if Pos.x < 0 then begin
      inc(Pos.y);
      Pos.x := 1;
      Line := FLowerLines[Pos.y-1];
      continue;
    end;
    x2 :=  WordBreaker.NextWordEnd(Line, Pos.x, True);
    if (Pos.y < EndPos.y) or (x2 <= EndPos.x) then begin
      entry := FWordIndex.GetWordP(@Line[Pos.x], x2-Pos.x);
      f := False;
      if (entry <> nil) and (entry^.Count > 1) then begin;
        if (entry^.GrpId = 0) and (g <= MAX_SYNC_ED_WORDS) then begin
          entry^.GrpId := g;
          inc(g);
          f := True;
        end;
        if (entry^.GrpId > 0) then
          with Cells.AddNew do begin
            LogStart := Pos;
            LogEnd := Point(x2, Pos.y);
            Group := entry^.GrpId;
            FirstInGroup := f;
          end;
      end;

    end;
    Pos.x := x2;
  end;
  FWordIndex.Clear;

  CurrentCell := 1;
  SelectCurrentCell;
  if g = 1 then StopSyncroMode;
end;

procedure TSynPluginSyncroEdit.StopSyncroMode;
begin
  Active := False;
end;

procedure TSynPluginSyncroEdit.DoImageChanged(Sender: TObject);
begin
  if Markup <> nil then
    Markup.GutterGlyph := FGutterGlyph;
end;

function TSynPluginSyncroEdit.CreateMarkup: TSynPluginSyncronizedEditMarkup;
begin
  Result := TSynPluginSyncroEditMarkup.Create(Editor);
  if FGutterGlyph <> nil then
    TSynPluginSyncroEditMarkup(Result).GutterGlyph := FGutterGlyph;
end;

procedure TSynPluginSyncroEdit.DoSelectionChanged(Sender: TObject);
begin
  if Mode = spseEditing then exit;
  If (not SelectionObj.SelAvail) or (SelectionObj.ActiveSelectionMode = smColumn) then begin
    FLastSelStart := Point(-1,-1);
    FLastSelEnd := Point(-1,-1);
    if Active or PreActive then begin
      FWordIndex.Clear;
      Editor.Invalidate;
      Active := False;
      MarkupEnabled := False;
    end;
    Mode := spseIncative;
    exit;
  end;

  if Mode = spseInvalid then exit;

  if Mode = spseIncative then begin
    Cells.Clear;
    AreaMarkupEnabled := False;
    MarkupEnabled := False;
    PreActive := True;
  end;
  Mode := spseSelecting;
  Markup.GlyphAtLine := -1;
  if not FCallQueued then
    Application.QueueAsyncCall(@DoScanSelection, 0);
  FCallQueued := True;
end;

procedure TSynPluginSyncroEdit.DoScanSelection(Data: PtrInt);
var
  NewPos, NewEnd: TPoint;

  function InitParsedPoints: Boolean;
  // Find the first begin of a word, inside the block (if any)
  var
    x, y: Integer;
  begin
    if FParsedStart.y >= 0 then exit(True);
    y := NewPos.y;
    x := NewPos.x;
    while y <= NewEnd.y do begin
      x :=  WordBreaker.NextWordStart(FLowerLines[y-1], x, True);
      if (x > 0) and ((y < NewEnd.Y) or (x <= NewEnd.x)) then begin
        FParsedStart.y := y;
        FParsedStart.x := x;
        FParsedStop := FParsedStart;
        break;
      end;
      inc(y);
      x := 1;
    end;
    Result := FParsedStart.Y >= 0;
  end;

var
  i, j: Integer;
  StartTime, t: Double;
begin
  StartTime := now();
  while (FCallQueued) and (Mode = spseSelecting) do begin
    FCallQueued := False;
    FWordScanCount := 0;

    NewPos := SelectionObj.FirstLineBytePos;
    NewEnd := SelectionObj.LastLineBytePos;
    i := FLastSelEnd.y - FLastSelStart.y;
    j := NewEnd.y - NewPos.y;
    if (j < 1) or (j < i div 2) or
       (NewEnd <= FLastSelStart) or (NewPos >= FLastSelEnd )
    then begin
      // Scan from scratch
      FLastSelStart := Point(-1,-1);
      FLastSelEnd := Point(-1,-1);
      FWordIndex.Clear;
    end;

    if FLastSelStart.Y < 0 then begin
      FLastSelStart := NewPos;
      FLastSelEnd := FLastSelStart;
      FParsedStart := Point(-1,-1);
      FParsedStop := Point(-1,-1);
    end;

    if (NewPos = NewEnd) or (not InitParsedPoints) then begin
      if MarkupEnabled then Editor.Invalidate;
      MarkupEnabled := False;
      exit;
    end;

    if (NewPos < FLastSelStart) then
      FParsedStart := Scan(FParsedStart, NewPos, True)  // NewPos is the smaller point;
    else
    if (NewPos > FParsedStart) then
      FParsedStart := UnScan(FParsedStart, NewPos, False);

    if FWordScanCount > MAX_WORDS_PER_SCAN then begin
      FLastSelStart := FParsedStart;
    end
    else begin
      FLastSelStart := NewPos;

      if (NewEnd > FLastSelEnd) then
        FParsedStop := Scan(FParsedStop, NewEnd, False)  // NewPos is the greater point;
      else
      if (NewEnd < FParsedStop) then
        FParsedStop := UnScan(FParsedStop, NewEnd, True);

      FLastSelEnd := NewEnd;
      if FWordScanCount > MAX_WORDS_PER_SCAN then
        FLastSelEnd := FParsedStop;
    end;

    MarkupEnabled := FWordIndex.MultiWordCount > 0;
    //debugln(['COUNTS: ', FWordIndex.WordCount,' mult=',FWordIndex.MultiWordCount, ' hash=',FWordIndex.EntryCount]);

    if FWordScanCount > MAX_WORDS_PER_SCAN then begin
      FCallQueued := True;
      t := Now;
      if (t - StartTime > MIN_PROCESS_MSG_TIME) then begin
        Application.ProcessMessages;
        if not FEditModeQueued then
          Application.Idle(False);
        StartTime := t;
      end;
    end;
  end;
  FCallQueued := False;
  if FEditModeQueued and (Mode = spseSelecting) then
    StartSyncroMode;
  FEditModeQueued := False;
end;

procedure TSynPluginSyncroEdit.DoOnDeactivate;
begin
  Mode := spseIncative;
  AreaMarkupEnabled := False;
  Cells.Clear;
  inherited DoOnDeactivate;
end;

procedure TSynPluginSyncroEdit.DoPreActiveEdit(aX, aY, aCount, aLineBrkCnt: Integer;
  aUndoRedo: Boolean);
begin
  FWordIndex.Clear;
  Active := False;
  Mode := spseInvalid;
end;

function TSynPluginSyncroEdit.MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
var
  r: TRect;
begin
  Result := (Active or PreActive) and
            ( ((Mode = spseSelecting) and (MarkupEnabled = True)) or
              (Mode = spseEditing) );
  if not Result then exit;

  r := Markup.GutterGlyphRect;
  Result := (AnInfo.MouseX >= r.Left) and (AnInfo.MouseX < r.Right) and
            (AnInfo.MouseY >= r.Top) and (AnInfo.MouseY < r.Bottom);

  if Result then begin
    HandleActionProc(FMouseActions, AnInfo);
    AnInfo.IgnoreUpClick := True;
  end;
end;

function TSynPluginSyncroEdit.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;

  if AnAction.Command = emcSynPSyncroEdGutterGlyph then begin
    if Mode = spseSelecting then
      StartSyncroMode
    else
      StopSyncroMode;
    Result := true;
  end;
end;

procedure TSynPluginSyncroEdit.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  if Editor <> nil then begin
    SelectionObj.RemoveChangeHandler(@DoSelectionChanged);
    Editor.UnregisterCommandHandler(@ProcessSynCommand);
    Editor.UnRegisterKeyTranslationHandler(@TranslateKey);
    Editor.UnregisterMouseActionSearchHandler(@MaybeHandleMouseAction);
    Editor.UnregisterMouseActionExecHandler(@DoHandleMouseAction);
    FLowerLines.Lines := nil;
  end;
  inherited DoEditorRemoving(AValue);
end;

procedure TSynPluginSyncroEdit.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  if Editor <> nil then begin
    FLowerLines.Lines := ViewedTextBuffer;
    Editor.RegisterMouseActionSearchHandler(@MaybeHandleMouseAction);
    Editor.RegisterMouseActionExecHandler(@DoHandleMouseAction);
    Editor.RegisterCommandHandler(@ProcessSynCommand, nil);
    Editor.RegisterKeyTranslationHandler(@TranslateKey);
    SelectionObj.AddChangeHandler(@DoSelectionChanged);
  end;
end;

procedure TSynPluginSyncroEdit.DoClear;
begin
  FWordIndex.Clear;
  inherited DoClear;
end;

procedure TSynPluginSyncroEdit.DoModeChanged;
begin
  if Assigned(FOnModeChange) then
    FOnModeChange(Self);
end;

procedure TSynPluginSyncroEdit.TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
  var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; FinishComboOnly: Boolean;
  var ComboKeyStrokes: TSynEditKeyStrokes);
var
  keys: TSynEditKeyStrokes;
begin
  if (not (Active or  PreActive)) or Handled then
    exit;

  keys := nil;

  if Mode = spseSelecting then
    keys := FKeystrokesSelecting;

  if Mode = spseEditing then begin
    if CurrentCell < 0 then
      keys := FKeyStrokesOffCell
    else
      keys := FKeyStrokes;
  end;
  if keys = nil then exit;

  if not FinishComboOnly then
    keys.ResetKeyCombo;
  Command := keys.FindKeycodeEx(Code, SState, Data, IsStartOfCombo, FinishComboOnly, ComboKeyStrokes);

  Handled := (Command <> ecNone) or IsStartOfCombo;
  if IsStartOfCombo then
    ComboKeyStrokes := keys;
end;

procedure TSynPluginSyncroEdit.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  if Handled or AfterProcessing or not (Active or PreActive) then exit;

  if Mode = spseSelecting then begin
    // todo: finish word-hash calculations / check if any cells exist
    Handled := True;
    case Command of
      ecSynPSyncroEdStart: StartSyncroMode;
      else
        Handled := False;
    end;
  end;

  if Mode = spseEditing then begin
    Handled := True;
    case Command of
      ecSynPSyncroEdNextCell:          NextCell(False, True);
      ecSynPSyncroEdNextCellSel:       NextCell(True, True);
      ecSynPSyncroEdPrevCell:          PreviousCell(False, True);
      ecSynPSyncroEdPrevCellSel:       PreviousCell(True, True);
      ecSynPSyncroEdNextFirstCell:     NextCell(False, True, True);
      ecSynPSyncroEdNextFirstCellSel:  NextCell(True, True, True);
      ecSynPSyncroEdPrevFirstCell:     PreviousCell(False, True, True);
      ecSynPSyncroEdPrevFirstCellSel:  PreviousCell(True, True, True);
      ecSynPSyncroEdCellHome:          CellCaretHome;
      ecSynPSyncroEdCellEnd:           CellCaretEnd;
      ecSynPSyncroEdCellSelect:        SelectCurrentCell;
      ecSynPSyncroEdEscape:
        begin
          Clear;
          Active := False;
        end;
      else
        Handled := False;
    end;
  end;
end;

constructor TSynPluginSyncroEdit.Create(AOwner: TComponent);
begin
  Mode := spseIncative;
  FEditModeQueued := False;

  FMouseActions := TSynPluginSyncroEditMouseActions.Create(self);
  FMouseActions.ResetDefaults;

  FKeystrokes := TSynEditSyncroEditKeyStrokes.Create(Self);
  FKeystrokes.ResetDefaults;

  FKeyStrokesOffCell := TSynEditSyncroEditKeyStrokesOffCell.Create(self);
  FKeyStrokesOffCell.ResetDefaults;

  FKeystrokesSelecting := TSynEditSyncroEditKeyStrokesSelecting.Create(Self);
  FKeystrokesSelecting.ResetDefaults;

  FGutterGlyph := TBitMap.Create;
  FGutterGlyph.OnChange := @DoImageChanged;

  FLowerLines := TSynPluginSyncroEditLowerLineCache.Create;
  FWordIndex := TSynPluginSyncroEditWordsHash.Create;
  FWordIndex.LowerLines := FLowerLines;
  inherited Create(AOwner);
  MarkupInfoArea.Background := clMoneyGreen;
  MarkupInfo.FrameColor := TColor($98b498)
end;

destructor TSynPluginSyncroEdit.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
  FreeAndNil(FWordIndex);
  FreeAndNil(FLowerLines);
  FreeAndNil(FGutterGlyph);
  FreeAndNil(FMouseActions);
  FreeAndNil(FKeystrokes);
  FreeAndNil(FKeyStrokesOffCell);
  FreeAndNil(FKeystrokesSelecting);
end;

{ TSynPluginSyncroEditMouseActions }

procedure TSynPluginSyncroEditMouseActions.ResetDefaults;
begin
  Clear;
  AddCommand(emcSynPSyncroEdGutterGlyph, False, mbXLeft, ccAny, cdDown, [], []);
end;

{ TSynEditSyncroEditKeyStrokesSelecting }

procedure TSynEditSyncroEditKeyStrokesSelecting.ResetDefaults;
  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState);
  begin
    with Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;
begin
  Clear;
  AddKey(ecSynPSyncroEdStart,            VK_J, [ssCtrl]);
end;

{ TSynEditSyncroEditKeyStrokes }

procedure TSynEditSyncroEditKeyStrokes.ResetDefaults;
  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState);
  begin
    with Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;
begin
  Clear;
  AddKey(ecSynPSyncroEdNextCell,          VK_RIGHT,  [ssCtrl]);
  AddKey(ecSynPSyncroEdNextCellSel,       VK_TAB,    []);
  AddKey(ecSynPSyncroEdPrevCell,          VK_LEFT,   [ssCtrl]);
  AddKey(ecSynPSyncroEdPrevCellSel,       VK_TAB,    [ssShift]);

  AddKey(ecSynPSyncroEdCellHome,          VK_HOME,   []);
  AddKey(ecSynPSyncroEdCellEnd,           VK_END,    []);
  AddKey(ecSynPSyncroEdCellSelect,        VK_A,      [ssCtrl]);
  AddKey(ecSynPSyncroEdEscape,            VK_ESCAPE, []);
end;

{ TSynEditSyncroEditKeyStrokesOffCell }

procedure TSynEditSyncroEditKeyStrokesOffCell.ResetDefaults;
  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState);
  begin
    with Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;
begin
  Clear;
  AddKey(ecSynPSyncroEdNextCell,          VK_RIGHT,  [ssCtrl]);
  AddKey(ecSynPSyncroEdNextCellSel,       VK_TAB,    []);
  AddKey(ecSynPSyncroEdPrevCell,          VK_LEFT,   [ssCtrl]);
  AddKey(ecSynPSyncroEdPrevCellSel,       VK_TAB,    [ssShift]);

  AddKey(ecSynPSyncroEdEscape,            VK_ESCAPE, []);
end;

const
  EditorSyncroCommandStrs: array[0..12] of TIdentMapEntry = (
    (Value: ecSynPSyncroEdStart;            Name: 'ecSynPSyncroEdStart'),
    (Value: ecSynPSyncroEdNextCell;         Name: 'ecSynPSyncroEdNextCell'),
    (Value: ecSynPSyncroEdNextCellSel;      Name: 'ecSynPSyncroEdNextCellSel'),
    (Value: ecSynPSyncroEdPrevCell;         Name: 'ecSynPSyncroEdPrevCell'),
    (Value: ecSynPSyncroEdPrevCellSel;      Name: 'ecSynPSyncroEdPrevCellSel'),
    (Value: ecSynPSyncroEdCellHome;         Name: 'ecSynPSyncroEdCellHome'),
    (Value: ecSynPSyncroEdCellEnd;          Name: 'ecSynPSyncroEdCellEnd'),
    (Value: ecSynPSyncroEdCellSelect;       Name: 'ecSynPSyncroEdCellSelect'),
    (Value: ecSynPSyncroEdEscape;           Name: 'ecSynPSyncroEdEscape'),
    (Value: ecSynPSyncroEdNextFirstCell;    Name: 'ecSynPSyncroEdNextFirstCell'),
    (Value: ecSynPSyncroEdNextFirstCellSel; Name: 'ecSynPSyncroEdNextFirstCellSel'),
    (Value: ecSynPSyncroEdPrevFirstCell;    Name: 'ecSynPSyncroEdPrevFirstCell'),
    (Value: ecSynPSyncroEdPrevFirstCellSel; Name: 'ecSynPSyncroEdPrevFirstCellSel')
  );

function IdentToSyncroCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, EditorSyncroCommandStrs);
end;

function SyncroCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := (Cmd >= ecPluginFirstSyncro) and (Cmd - ecPluginFirstSyncro < ecSynPSyncroEdCount);
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, EditorSyncroCommandStrs);
end;

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(EditorSyncroCommandStrs) to High(EditorSyncroCommandStrs) do
    Proc(EditorSyncroCommandStrs[I].Name);
end;


initialization
  RegisterKeyCmdIdentProcs(@IdentToSyncroCommand,
                           @SyncroCommandToIdent);
  RegisterExtraGetEditorCommandValues(@GetEditorCommandValues);

end.


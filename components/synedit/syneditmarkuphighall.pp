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
unit SynEditMarkupHighAll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Controls, ExtCtrls,
  // LazUtils
  LazClasses, LazUTF8, LazMethodList,
  // SynEdit
  SynEditMarkup, SynEditTypes, SynEditSearch, SynEditMiscClasses,
  SynEditHighlighter, SynEditPointClasses, SynEditMiscProcs,
  SynEditTextBase, LazSynEditText;

type

  { TSynMarkupHighAllMatch }
  TSynMarkupHighAllMatch = Record
    StartPoint, EndPoint : TPoint;
  end;
  PSynMarkupHighAllMatch = ^TSynMarkupHighAllMatch;

  { TSynMarkupHighAllMatchList }

  TSynMarkupHighAllMatchList = class(TSynEditStorageMem)
  private
    function GetEndPoint(const Index : Integer) : TPoint;
    function GetPoint(const Index : Integer) : TPoint;
    function GetPointCount : Integer;
    function GetStartPoint(const Index : Integer) : TPoint;
    function  GetMatch(const Index : Integer) : TSynMarkupHighAllMatch;
    procedure SetEndPoint(const Index : Integer; const AValue : TPoint);
    procedure SetMatch(const Index : Integer; const AValue : TSynMarkupHighAllMatch);
    procedure SetStartPoint(const Index : Integer; const AValue : TPoint);
  protected
    function  GetInintialForItemSize: Integer; override;
    procedure SetCount(const AValue : Integer); override;
  public
    constructor Create;
    Function MaybeReduceCapacity : Boolean;
    function IndexOfFirstMatchForLine(ALine: Integer): Integer;
    function IndexOfLastMatchForLine(ALine: Integer): Integer;
    procedure Delete(AIndex: Integer; ACount: Integer = 1);
    procedure Insert(AIndex: Integer; ACount: Integer = 1);
    procedure Insert(AIndex: Integer; AStartPoint, AEndPoint: TPoint);
    property Match [const Index : Integer] : TSynMarkupHighAllMatch read GetMatch write SetMatch; default;
    property StartPoint [const Index : Integer] : TPoint read GetStartPoint write SetStartPoint;
    property EndPoint   [const Index : Integer] : TPoint read GetEndPoint write SetEndPoint;
    property PointCount : Integer read GetPointCount;
    property Point      [const Index : Integer] : TPoint read GetPoint;
  end;

  { TSynMarkupHighAllMultiMatchList - Allow matches with different markup / no overlap }

  TSynMarkupHighAllMultiMatchList = class(TSynMarkupHighAllMatchList)
  private
    FParentItemSize: Integer;
    function GetMarkupId(Index: Integer): Integer;
    procedure SetMarkupId(Index: Integer; AValue: Integer);
  protected
    function GetInintialForItemSize: Integer; override;
  public
    property MarkupId[Index: Integer]: Integer read GetMarkupId write SetMarkupId;
  end;

  { TSynEditMarkupHighlightMatches }

  TSynEditMarkupHighlightMatches = class(TSynEditMarkup)
  private
    FMatches : TSynMarkupHighAllMatchList;
    FNextPosIdx, FNextPosRow: Integer;
  protected
    function  HasDisplayAbleMatches: Boolean; virtual;
    function  CreateMatchList: TSynMarkupHighAllMatchList; virtual;
    function  MarkupIdForMatch(Idx: Integer): Integer; virtual;
    function  MarkupInfoForId(Idx: Integer): TSynSelectedColor; virtual;
    property  Matches: TSynMarkupHighAllMatchList read FMatches;

    function  GetMarkupAttrIdAtRowCol(const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
                                      out AStartPos, AnEndPos: Integer): Integer;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;

    procedure PrepareMarkupForRow(aRow: Integer); override;
    procedure EndMarkup; override;
    function  GetMarkupAttributeAtRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;
  end;


  { TSynEditMarkupHighlightAllBase }

  TSynEditMarkupHighlightAllBase = class(TSynEditMarkupHighlightMatches)
  private
    FNeedValidate, FNeedValidatePaint: Boolean;
    FMarkupEnabled: Boolean;

    FStartPoint : TPoint;        // First found position, before TopLine of visible area
    FSearchedEnd: TPoint;
    FFirstInvalidLine, FLastInvalidLine: Integer;
    FHideSingleMatch: Boolean;

    function GetMatchCount: Integer;
    procedure SetHideSingleMatch(AValue: Boolean);
    procedure DoFoldChanged(Sender: TSynEditStrings; aIndex, aCount: Integer);

    Procedure ValidateMatches(SkipPaint: Boolean = False);

  protected
    procedure SetLines(const AValue: TSynEditStringsLinked); override;
    function  HasSearchData: Boolean; virtual; abstract;
    function HasDisplayAbleMatches: Boolean; override;
    function  SearchStringMaxLines: Integer; virtual; abstract;
    procedure FindInitialize;  virtual; abstract;
    function  FindMatches(AStartPoint, AEndPoint: TPoint;
                          var AIndex: Integer;
                          AStopAfterLine: Integer = -1; // AEndPoint may be set further down, for multi-line matches
                          ABackward : Boolean = False
                         ): TPoint;  virtual; abstract; // returns searhed until point


    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoEnabledChanged(Sender: TObject); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override; // 1 based
    procedure DoVisibleChanged(AVisible: Boolean); override;
    function  HasVisibleMatch: Boolean; // does not check, if in visible line range. Only Count and DideSingleMatch
    property  MatchCount: Integer read GetMatchCount;
    property  MarkupEnabled: Boolean read FMarkupEnabled;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure DecPaintLock; override;

    // AFirst/ ALast are 1 based
    Procedure Invalidate(SkipPaint: Boolean = False);
    Procedure InvalidateLines(AFirstLine: Integer = 0; ALastLine: Integer = 0; SkipPaint: Boolean = False);
    Procedure SendLineInvalidation(AFirstIndex: Integer = -1;ALastIndex: Integer = -1);

    property HideSingleMatch: Boolean read FHideSingleMatch write SetHideSingleMatch;
  end;

  { TSynEditMarkupHighlightAll }

  TSynEditMarkupHighlightAll = class(TSynEditMarkupHighlightAllBase)
  private
    FSearch: TSynEditSearch;
    FSearchOptions: TSynSearchOptions;
    FSearchString: String;
    FSearchStringMaxLines: Integer;

    procedure SetSearchOptions(AValue: TSynSearchOptions);
    procedure SetSearchString(AValue: String);
  protected
    procedure SearchStringChanged; virtual;
    procedure DoOptionsChanged;virtual;

    function  HasSearchData: Boolean; override;
    function  SearchStringMaxLines: Integer; override;
    procedure FindInitialize;  override;
    function  FindMatches(AStartPoint, AEndPoint: TPoint;
                          var AIndex: Integer;
                          AStopAfterLine: Integer = -1; // AEndPoint may be set further down, for multi-line matches
                          ABackward : Boolean = False
                         ): TPoint;  override; // returns searhed until point
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    property SearchString : String read FSearchString write SetSearchString;
    property SearchOptions : TSynSearchOptions read FSearchOptions write SetSearchOptions;
  end;

  { TSynSearchDictionary }

  PSynSearchDictionaryNode = ^TSynSearchDictionaryNode;
  TSynSearchDictionaryNode = record
    NextCharMin, NextCharMax: Byte; // if char > 128 then char := 128+256 - char // move utf8 continuation block
    ItemIdx: Integer;  // Node is in dictionary
    NotFoundEntry, DictLink: PSynSearchDictionaryNode;
    NextEntry: Array [0..191] of PSynSearchDictionaryNode; // Max size 192, for utf8 start bytes
  end;

  TSynSearchDictFoundEvent =
    procedure(MatchEnd: PChar; MatchIdx: Integer;
              var IsMatch: Boolean; var StopSeach: Boolean
             ) of object;

  TSynSearchTermOptsBounds = (soNoBounds, soBothBounds, soBoundsAtStart, soBoundsAtEnd);

  { TSynSearchDictionary }

  TSynSearchDictionary = class(TObject)
  private
    FBuildLowerCaseDict: Boolean;
    FList: TStringList;
    FSortedList: TStringList;
    FRootNode: PSynSearchDictionaryNode;

    procedure ClearDictionary;
    procedure DeleteNode(aNode: PSynSearchDictionaryNode);
    procedure BuildDictionary;
    function GetTerms(AIndex: Integer): String;
    procedure SetTerms(AIndex: Integer; AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    {$IFDEF SynDictDebug}
    procedure DebugPrint(OnlySummary: Boolean = false);
    {$ENDIF}
    procedure Clear;

    function  Add(ATerm: String; ATag: Integer): Integer;
    function  IndexOf(ATerm: String): Integer;
    procedure Remove(ATerm: String);
    procedure Delete(AIndex: Integer);
    function  Count: Integer;
    property  Terms[AIndex: Integer]: String read GetTerms write SetTerms;

    function Search(AText: PChar; ATextLen: Integer; AFoundEvent: TSynSearchDictFoundEvent): PChar;
    function GetMatchAtChar(AText: PChar; ATextLen: Integer; AFoundEvent: TSynSearchDictFoundEvent = nil): Integer;

    property BuildLowerCaseDict: Boolean read FBuildLowerCaseDict write FBuildLowerCaseDict;
  end;

  TSynEditMarkupHighlightAllMulti = class;

  { TSynSearchTerm }

  TSynSearchTerm = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FMatchCase: Boolean;
    FMatchWordBounds: TSynSearchTermOptsBounds;
    FSearchTerm: String;
    procedure SetEnabled(AValue: Boolean);
    procedure SetMatchCase(AValue: Boolean);
    procedure SetMatchWordBounds(AValue: TSynSearchTermOptsBounds);
    procedure SetSearchTerm(AValue: String);
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function Equals(Other: TSynSearchTerm): boolean; reintroduce;
  published
    property SearchTerm: String read FSearchTerm write SetSearchTerm;
    property MatchWordBounds: TSynSearchTermOptsBounds read FMatchWordBounds write SetMatchWordBounds;
    property MatchCase: Boolean read FMatchCase write SetMatchCase;
    property Enabled: Boolean read FEnabled write SetEnabled; // Todo: Exclude from dict, but need to keep room for ID/Index
  end;

  TSynSearchTermClass = class of TSynSearchTerm;

  { TSynSearchTermList }

  TSynSearchTermList = class(TCollection)
  private
    FOnChanged: TNotifyEvent;
    function GetItem(Index: Integer): TSynSearchTerm;
    procedure SetItem(Index: Integer; AValue: TSynSearchTerm);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function  DefaultItemClass: TSynSearchTermClass; virtual;
  public
    constructor Create; overload;
    function Add: TSynSearchTerm; reintroduce;
    function IndexOfSearchTerm(ATerm: String; ASearchStartIdx: Integer = 0): Integer;
    function IndexOfSearchTerm(ATerm: TSynSearchTerm; ASearchStartIdx: Integer = 0): Integer;
    function IndexOfSearchTerm(ATerm: String; ACaseSensitive: Boolean; ASearchStartIdx: Integer = 0): Integer;
    property Items[Index: Integer]: TSynSearchTerm read GetItem write SetItem; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TSynSearchTermListClass = class of TSynSearchTermList;

  { TSynSearchTermDict }

  TSynSearchTermDict = class(TRefCountedObject)
  private
    FTerms: TSynSearchTermList;
    FDict: TSynSearchDictionary;
    FNextTermWithSameWord: Array of Integer;
    FChangedNotifyList: TMethodList;
    FChangeNotifyLock: Integer;
    FNeedNotify: Boolean;

    procedure DoTermsChanged(Sender: TObject);
    function GetItem(Index: Integer): TSynSearchTerm;
    procedure SetItem(Index: Integer; AValue: TSynSearchTerm);
  protected
    procedure MaybeInitDict;
    property Dict: TSynSearchDictionary read FDict;
    property Terms: TSynSearchTermList read FTerms;
  public
    constructor Create(ATermListClass: TSynSearchTermListClass);
    destructor Destroy; override;
    procedure IncChangeNotifyLock;
    procedure DecChangeNotifyLock;
    procedure RegisterChangedHandler(AEvent: TNotifyEvent);
    procedure UnRegisterChangedHandler(AEvent: TNotifyEvent);
    Procedure Assign(Src: TSynSearchTermDict); virtual;
    Procedure Assign(Src: TSynSearchTermList); virtual;

    procedure Clear;
    procedure ClearDictionary;
    function  Count: Integer;
    function  Add: TSynSearchTerm;
    procedure Delete(AIndex: Integer);
    function  IndexOfSearchTerm(ATerm: String): Integer;
    function  IndexOfSearchTerm(ATerm: TSynSearchTerm): Integer;

    procedure  Search(AText: PChar; ATextLen: Integer; AFoundEvent: TSynSearchDictFoundEvent);
    function   GetIndexForNextWordOccurrence(AIndex: Integer): Integer;

    property Items[Index: Integer]: TSynSearchTerm read GetItem write SetItem; default;
  end;

  { TSynEditMarkupHighlightAllMulti }

  TSynEditMarkupHighlightAllMulti = class(TSynEditMarkupHighlightAllBase)
  private
    FTermDict: TSynSearchTermDict;
    //FNextTermWIthSameWord: Array of Integer;

    FFindInsertIndex, FFindStartedAtIndex: Integer;
    FFindLineY: Integer;
    FFindLineText, FFindLineTextEnd, FFindLineTextLower, FFindLineTextLowerEnd: PChar;
    FBackward, FBackwardReplace: Boolean;
    FWordBreakChars: TSynIdentChars;

    procedure DoMatchFound(MatchEnd: PChar; MatchIdx: Integer; var IsMatch: Boolean;
      var StopSeach: Boolean);
    procedure SetTerms(AValue: TSynSearchTermDict);
    procedure SetWordBreakChars(AValue: TSynIdentChars);
  protected
    procedure DoTermsChanged(Sender: TObject);
    function  HasSearchData: Boolean; override;
    function  SearchStringMaxLines: Integer; override;
    procedure FindInitialize; override;
    function  FindMatches(AStartPoint, AEndPoint: TPoint;
                          var AIndex: Integer;
                          AStopAfterLine: Integer = -1; // AEndPoint may be set further down, for multi-line matches
                          ABackward : Boolean = False
                         ): TPoint;  override; // returns searched until point
    function CreateTermsList: TSynSearchTermDict; virtual;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    procedure  Clear;
    procedure  ResetWordBreaks;

    function  AddSearchTerm(ATerm: String): Integer;
    function  IndexOfSearchTerm(ATerm: String): Integer;
    procedure RemoveSearchTerm(ATerm: String);
    procedure DeleteSearchTerm(AIndex: Integer);

    property WordBreakChars: TSynIdentChars read FWordBreakChars write SetWordBreakChars;
    property Terms: TSynSearchTermDict read FTermDict write SetTerms;
  end;

  { TSynEditMarkupHighlightAllCaret }

  TSynEditMarkupHighlightAllCaret = class(TSynEditMarkupHighlightAll)
  private
    FTimer: TTimer;
    FTrim: Boolean;
    FWaitTime: Integer;
    FFullWord: Boolean;
    FFullWordMaxLen: Integer;
    FIgnoreKeywords: Boolean;
    FSelection: TSynEditSelection;
    FHighlighter: TSynCustomHighlighter;
    FLowBound, FUpBound, FOldLowBound, FOldUpBound: TPoint;
    FToggledWord: String;
    FToggledOption: TSynSearchOptions;
    FStateChanged, FValidateNeeded: Boolean;
    FWaitForHandle: Boolean;
    procedure SetFullWord(const AValue: Boolean);
    procedure SetFullWordMaxLen(const AValue: Integer);
    procedure SetHighlighter(const AValue: TSynCustomHighlighter);
    procedure SetIgnoreKeywords(const AValue: Boolean);
    procedure SetSelection(const AValue: TSynEditSelection);
    procedure SetTrim(const AValue: Boolean);
    procedure SetWaitTime(const AValue: Integer);
  protected
    procedure SearchStringChanged; override;
    procedure SelectionChanged(Sender: TObject);
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoOptionsChanged;override;
    procedure RestartTimer;
    procedure ScrollTimerHandler(Sender: TObject);
    function  GetCurrentText: String;
    function  GetCurrentOption: TSynSearchOptions;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure DecPaintLock; override;
    procedure CheckState;
    procedure ToggleCurrentWord;
    property  WaitTime: Integer read FWaitTime write SetWaitTime;
    property  Trim: Boolean read FTrim write SetTrim;
    property  FullWord: Boolean read FFullWord write SetFullWord;
    property  FullWordMaxLen: Integer read FFullWordMaxLen write SetFullWordMaxLen;
    property  IgnoreKeywords: Boolean read FIgnoreKeywords write SetIgnoreKeywords;
    property  Highlighter: TSynCustomHighlighter
      read FHighlighter write SetHighlighter;
    property  Selection: TSynEditSelection write SetSelection;
  end;

implementation
uses SynEdit;

const
  SEARCH_START_OFFS = 100; // Search n lises before/after visible area. (Before applies only, if no exact offset can not be calculated from searchtext)
  MATCHES_CLEAN_CNT_THRESHOLD = 2500; // Remove matches out of range, only if more Matches are present
  MATCHES_CLEAN_LINE_THRESHOLD = 300; // Remove matches out of range, only if they are at least n lines from visible area.
  MATCHES_CLEAN_LINE_KEEP = 200; // LinesKept, if cleaning. MUST be LESS than MATCHES_CLEAN_LINE_THRESHOLD

{ TSynEditMarkupHighlightMatches }

function TSynEditMarkupHighlightMatches.HasDisplayAbleMatches: Boolean;
begin
  Result := FMatches.Count > 0;
end;

function TSynEditMarkupHighlightMatches.CreateMatchList: TSynMarkupHighAllMatchList;
begin
  Result := TSynMarkupHighAllMatchList.Create;
end;

function TSynEditMarkupHighlightMatches.MarkupIdForMatch(Idx: Integer): Integer;
begin
  Result := 0;
end;

function TSynEditMarkupHighlightMatches.MarkupInfoForId(Idx: Integer): TSynSelectedColor;
begin
  Result := MarkupInfo;
end;

constructor TSynEditMarkupHighlightMatches.Create(ASynEdit: TSynEditBase);
begin
  FMatches := CreateMatchList;
  inherited Create(ASynEdit);
end;

destructor TSynEditMarkupHighlightMatches.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FMatches);
end;

procedure TSynEditMarkupHighlightMatches.PrepareMarkupForRow(aRow: Integer);
begin
  FNextPosRow := -1;
  if not HasDisplayAbleMatches then
    exit;

  if (FNextPosRow > 0) and (aRow > FNextPosRow) and
     ( (FNextPosIdx = -2) or // No match after FNextPosRow
       ( (FNextPosIdx >= 0) and (FNextPosIdx < FMatches.PointCount) and (aRow <= FMatches.Point[FNextPosIdx].y) )
      )
  then begin
    if (FNextPosIdx >= 0) and
      ( (aRow = FMatches.Point[FNextPosIdx].y) or (FNextPosIdx and 1 = 1) )
    then
      FNextPosRow := aRow;
    exit;
  end;

  FNextPosRow := aRow;
  FNextPosIdx := FMatches.IndexOfFirstMatchForLine(aRow) * 2;
  if (FNextPosIdx < 0) or (FNextPosIdx >= FMatches.PointCount) then
    exit;
  if (FMatches.Point[FNextPosIdx].y < aRow) then
    inc(FNextPosIdx);  // Use EndPoint
end;

procedure TSynEditMarkupHighlightMatches.EndMarkup;
begin
  inherited EndMarkup;
  FNextPosRow := -1;
end;

function TSynEditMarkupHighlightMatches.GetMarkupAttrIdAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; out AStartPos, AnEndPos: Integer): Integer;
var
  pos: Integer;
begin
  Result := -1;
  if (aRow <> FNextPosRow) or (FNextPosIdx < 0) then
    exit;

  while (FNextPosIdx < fMatches.PointCount) and
        (fMatches.Point[FNextPosIdx].y = aRow) and
        (fMatches.Point[FNextPosIdx].x <= aStartCol.Logical)
  do
    inc(FNextPosIdx);

  if FNextPosIdx >= fMatches.PointCount // last point was EndPoint => no markup
  then exit;

  pos := FNextPosIdx - 1;
  while (pos >= 0) and (fMatches.Point[pos].y = aRow) and
        (fMatches.Point[pos].x > aStartCol.Logical)
  do
    dec(pos);

  if pos < 0 then
    exit;

  //pos is the point at or before LogPos
  if (pos and 1)= 1 // the Point is a EndPoint => Outside Match
  then exit;

  if fMatches.Point[pos].y < aRow then
    AStartPos := -1
  else
    AStartPos := fMatches.Point[pos].x;
  if (pos = FMatches.PointCount) or (fMatches.Point[pos+1].y > aRow) then
    AnEndPos := -1
  else
    AnEndPos := fMatches.Point[pos+1].x;

  Result := MarkupIdForMatch(pos div 2);
end;

function TSynEditMarkupHighlightMatches.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  i, s, e: Integer;
begin
  result := nil;
  if not HasDisplayAbleMatches then
    exit;

  i := GetMarkupAttrIdAtRowCol(aRow, aStartCol, s, e);
  if i < 0 then
    exit;
  Result := MarkupInfoForId(i);
  Result.SetFrameBoundsLog(s, e);
end;

procedure TSynEditMarkupHighlightMatches.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out
  ANextPhys, ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if not HasDisplayAbleMatches then
    exit;
  if (aRow <> FNextPosRow) or (FNextPosIdx < 0) or
     (FNextPosIdx >= fMatches.PointCount) or (FMatches.Point[FNextPosIdx].y > aRow)
  then
    exit;

  while (FNextPosIdx < fMatches.PointCount) and
        (fMatches.Point[FNextPosIdx].y = aRow) and
        (fMatches.Point[FNextPosIdx].x <= aStartCol.Logical)
  do
    inc(FNextPosIdx);

  if FNextPosIdx >= fMatches.PointCount then
    exit;
  if fMatches.Point[FNextPosIdx].y <> aRow then
    exit;

  ANextLog := fMatches.Point[FNextPosIdx].x;
end;

{ TSynSearchTermDict }

procedure TSynSearchTermDict.DoTermsChanged(Sender: TObject);
begin
  if FDict = nil then
    exit;

  FDict.Clear;
  if FChangeNotifyLock > 0 then begin
    FNeedNotify := True;
    exit;
  end;
  FNeedNotify := False;
  FChangedNotifyList.CallNotifyEvents(Self);
end;

function TSynSearchTermDict.GetItem(Index: Integer): TSynSearchTerm;
begin
  Result := FTerms[Index];
end;

procedure TSynSearchTermDict.IncChangeNotifyLock;
begin
  inc(FChangeNotifyLock);
end;

procedure TSynSearchTermDict.DecChangeNotifyLock;
begin
  dec(FChangeNotifyLock);
  if FNeedNotify then
    DoTermsChanged(Self);
end;

procedure TSynSearchTermDict.SetItem(Index: Integer; AValue: TSynSearchTerm);
begin
  FTerms[Index] := AValue;
end;

procedure TSynSearchTermDict.MaybeInitDict;
var
  i, j: Integer;
  s: String;
begin
  if FDict.Count > 0 then
    exit;

  SetLength(FNextTermWIthSameWord, FTerms.Count);

  for i := 0 to FTerms.Count - 1 do begin
    FNextTermWIthSameWord[i] := -1;
    if not FTerms[i].Enabled then
      Continue;
    s := LowerCase(FTerms[i].SearchTerm);
    FDict.Add(FTerms[i].SearchTerm, i);
    for j := i + 1 to FTerms.Count - 1 do
      if LowerCase(FTerms[j].SearchTerm) = s then begin
        FNextTermWIthSameWord[i] := j;
        break;
      end;
  end;
end;

constructor TSynSearchTermDict.Create(ATermListClass: TSynSearchTermListClass);
begin
  inherited Create;
  FChangedNotifyList := TMethodList.Create;
  FNeedNotify := False;
  FTerms := ATermListClass.Create;
  FTerms.OnChanged := @DoTermsChanged;
  FDict  := TSynSearchDictionary.Create;
  FDict.BuildLowerCaseDict := True;
end;

destructor TSynSearchTermDict.Destroy;
begin
  inherited Destroy;
  FChangeNotifyLock := 1; // Disable notifications
  FreeAndNil(FDict);
  FreeAndNil(FTerms);
  FreeAndNil(FChangedNotifyList);
end;

procedure TSynSearchTermDict.RegisterChangedHandler(AEvent: TNotifyEvent);
begin
  FChangedNotifyList.Add(TMethod(AEvent));
end;

procedure TSynSearchTermDict.UnRegisterChangedHandler(AEvent: TNotifyEvent);
begin
  FChangedNotifyList.Remove(TMethod(AEvent));
end;

procedure TSynSearchTermDict.Assign(Src: TSynSearchTermDict);
begin
  IncChangeNotifyLock;
  FDict.Clear;
  FTerms.Assign(Src.FTerms);
  DecChangeNotifyLock;
end;

procedure TSynSearchTermDict.Assign(Src: TSynSearchTermList);
begin
  IncChangeNotifyLock;
  FDict.Clear;
  FTerms.Assign(Src);
  DecChangeNotifyLock;
end;

procedure TSynSearchTermDict.Clear;
begin
  IncChangeNotifyLock;
  FTerms.Clear;
  FDict.Clear;
  DecChangeNotifyLock;
end;

procedure TSynSearchTermDict.ClearDictionary;
begin
  FDict.Clear;
end;

function TSynSearchTermDict.Count: Integer;
begin
  Result := FTerms.Count;
end;

function TSynSearchTermDict.Add: TSynSearchTerm;
begin
  Result := TSynSearchTerm(FTerms.Add);
end;

procedure TSynSearchTermDict.Delete(AIndex: Integer);
begin
  FTerms.Delete(AIndex);
end;

function TSynSearchTermDict.IndexOfSearchTerm(ATerm: String): Integer;
begin
  Result := FTerms.IndexOfSearchTerm(ATerm);
end;

function TSynSearchTermDict.IndexOfSearchTerm(ATerm: TSynSearchTerm): Integer;
begin
  Result := FTerms.IndexOfSearchTerm(ATerm);
end;

procedure TSynSearchTermDict.Search(AText: PChar; ATextLen: Integer;
  AFoundEvent: TSynSearchDictFoundEvent);
begin
  MaybeInitDict;
  FDict.Search(AText, ATextLen, AFoundEvent);
end;

function TSynSearchTermDict.GetIndexForNextWordOccurrence(AIndex: Integer): Integer;
begin
  Result := FNextTermWIthSameWord[AIndex];
end;

{ TSynSearchTermList }


{ TSynSearchDictionary }

procedure TSynSearchDictionary.ClearDictionary;
begin
  DeleteNode(FRootNode);
  FRootNode := nil;
end;

procedure TSynSearchDictionary.DeleteNode(aNode: PSynSearchDictionaryNode);
var
  i: Integer;
begin
  if aNode = nil then
    exit;
  For i := 0 to aNode^.NextCharMax - aNode^.NextCharMin do
    DeleteNode(aNode^.NextEntry[i]);
  FreeMem(aNode);
end;

function CompareBinary(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1, s2: String;
  l: Integer;
begin
  Result := 0;
  s1 := List[Index1];
  s2 := List[Index2];
  l := Length(s1);
  if Length(s2) < l then begin
    l := Length(s2);
    if l > 0 then
      Result := CompareByte(s1[1], s2[1], l);
    if Result = 0 then
      Result := 1;
  end else begin
    if l > 0 then
      Result := CompareByte(s1[1], s2[1], l);
    if (Result = 0) and (Length(s2) > l) then
      Result := -1;
  end;
end;

procedure TSynSearchDictionary.BuildDictionary;

  function ChangeBytes(ATerm: String): String;
  var
    i: Integer;
    c: Char;
  begin
    (* Map utf8 continuation bytes (128..191) to the end of the char-range (192..255)
       This way the max size for "NextEntry" array will be 192 (for utf8 char start)
       or 64 for continuation
       Also by mapping #252..#255 to #188..#191, makes them a requiremnt for any
       node having a full 192 sized array. This will reduce the risk of worst case
       memory consumption, since they have 4 continuation bytes (array size 64)
       to bring down the average.
    *)
    SetLength(Result, Length(ATerm));
    for i := 1 to Length(ATerm) do begin
      c := ATerm[i];
      if c < #128
      then Result[i] := c
      else Result[i] := chr(383-ord(c));
    end;
  end;

  function GetNodeForCharAt(AListIndex, AMaxListIdx, ACharPos: Integer) :PSynSearchDictionaryNode;
  var
    c: Char;
    i, LastListIdx, MatchIdx, MinChar, MaxChar: Integer;
  begin
    // Find all continuation chars
    if ACharPos = 0 then begin
      LastListIdx := AMaxListIdx;
    end
    else begin
      c := FSortedList[AListIndex][ACharPos];
      LastListIdx := AListIndex;
      while (LastListIdx < AMaxListIdx) and
            (length(FSortedList[LastListIdx+1]) >= ACharPos) and
            (FSortedList[LastListIdx+1][ACharPos] = c)
      do
        inc(LastListIdx);
    end;

    if length(FSortedList[AListIndex]) = ACharPos then
      MatchIdx := PtrInt(FSortedList.Objects[AListIndex]) // this is a match, TODO: there could be sevelal matches of the same length
    else
      MatchIdx := -1;
    while (AListIndex <= LastListIdx) and (length(FSortedList[AListIndex]) = ACharPos) do begin
      // for identical words, store smallest matchidx (TODO: true case sensitive search)
      if PtrInt(FSortedList.Objects[AListIndex]) < MatchIdx then
        MatchIdx := PtrInt(FSortedList.Objects[AListIndex]);
      inc(AListIndex); // Skip match, if any
    end;

    if length(FSortedList[LastListIdx]) > ACharPos then begin
      // there are possible continuations
      MinChar := ord(FSortedList[AListIndex][ACharPos+1]);
      MaxChar := ord(FSortedList[LastListIdx][ACharPos+1]);
    end
    else begin
      // No continuatian
      MinChar := 1;
      MaxChar := 0;
    end;

    Result := AllocMem(PtrUInt(@PSynSearchDictionaryNode(nil)^.NextEntry[0]) +
                       PtrUInt(MaxChar - MinChar + 1)*SizeOf(PSynSearchDictionaryNode));
    Result^.NextCharMin := MinChar;
    Result^.NextCharMax := MaxChar;
    Result^.ItemIdx := MatchIdx;

    inc(ACharPos);
    for i := MinChar to MaxChar do begin
      c := FSortedList[AListIndex][ACharPos];
      if c = chr(i) then begin
        Result^.NextEntry[i-MinChar] := GetNodeForCharAt(AListIndex, LastListIdx, ACharPos);
        while (AListIndex < LastListIdx) and (FSortedList[AListIndex][ACharPos] = c) do
          inc(AListIndex);
      end
      else
        Result^.NextEntry[i-MinChar] := nil;
    end;
  end;

  function FindNode(ANodeValue: String) :PSynSearchDictionaryNode;
  var
    i, b, m: Integer;
  begin
    Result := FRootNode;
    for i := 1 to length(ANodeValue) do begin
      b := ord(ANodeValue[i]);
      m := Result^.NextCharMin;
      if (b < m) or (b > Result^.NextCharMax) or
         (Result^.NextEntry[b-m] = nil)
      then
        exit(nil);
      Result := Result^.NextEntry[b-m];
    end;
  end;

  procedure SetNotFoundNote(ANode: PSynSearchDictionaryNode; ANodeValue: String);
  var
    i, m: Integer;
  begin
    if ANodeValue <> '' then begin
      for i := 2 to length(ANodeValue) do begin
        ANode^.NotFoundEntry := FindNode(copy(ANodeValue, i, length(ANodeValue)));
        if ANode^.NotFoundEntry <> nil then
          break;
      end;
      if ANode^.NotFoundEntry = nil then
        ANode^.NotFoundEntry := FRootNode;
    end;

    m := ANode^.NextCharMin;
    for i := ANode^.NextCharMin to ANode^.NextCharMax do
      if ANode^.NextEntry[i-m] <> nil then
        SetNotFoundNote(ANode^.NextEntry[i-m], ANodeValue + chr(i));
  end;

  procedure FindDictLinks(ANode: PSynSearchDictionaryNode);
  var
    i, m: Integer;
    NotFound: PSynSearchDictionaryNode;
  begin
    NotFound := ANode^.NotFoundEntry;
    while (NotFound <> nil) and (NotFound^.ItemIdx < 0) do
      NotFound := NotFound^.NotFoundEntry;
    ANode^.DictLink := NotFound;

    m := ANode^.NextCharMin;
    for i := ANode^.NextCharMin to ANode^.NextCharMax do
      if ANode^.NextEntry[i-m] <> nil then
        FindDictLinks(ANode^.NextEntry[i-m]);
  end;

var
  i: Integer;
begin
  ClearDictionary;
  if FList.Count = 0 then
    exit;

  FSortedList.Clear;
  for i := 0 to FList.Count - 1 do begin
    if FBuildLowerCaseDict then // TODO: Create a case-insesitive dictionary
      FSortedList.AddObject(ChangeBytes(LowerCase(FList[i])), FList.Objects[i])
    else
      FSortedList.AddObject(ChangeBytes(FList[i]), FList.Objects[i]);
  end;
  FSortedList.CustomSort(@CompareBinary);

  FRootNode := GetNodeForCharAt(0, FSortedList.Count - 1, 0);
  SetNotFoundNote(FRootNode, '');
  FindDictLinks(FRootNode);
  FRootNode^.NotFoundEntry := nil;

  FSortedList.Clear;
end;

function TSynSearchDictionary.GetTerms(AIndex: Integer): String;
begin
  Result := FList[AIndex];
end;

procedure TSynSearchDictionary.SetTerms(AIndex: Integer; AValue: String);
begin
  FList[AIndex] := AValue;
  ClearDictionary;
end;

constructor TSynSearchDictionary.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.OwnsObjects := False;
  FSortedList := TStringList.Create;
  FSortedList.OwnsObjects := False;
  FBuildLowerCaseDict := False;
end;

destructor TSynSearchDictionary.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FList);
  FreeAndNil(FSortedList);
end;

{$IFDEF SynDictDebug}
procedure TSynSearchDictionary.DebugPrint(OnlySummary: Boolean);
var
  NCnt, ArrayLen, EmptyCnt: Integer;

  function FlipByte(b: Integer): Integer;
  begin
    if b < 128
    then Result := b
    else Result := 383-b;
  end;

  procedure DebugNode(ANode: PSynSearchDictionaryNode; APreFix: String = ''; AIndent: String = '');
  var
    i, j: Integer;
  begin
    inc(NCnt);
    if not OnlySummary then
      DebugLn([AIndent, 'Node for "', APreFix, '":  ItemIdx=', ANode^.ItemIdx,
               ' Min=', FlipByte(ANode^.NextCharMin), ' Max=', FlipByte(ANode^.NextCharMax),
               ' At ', IntToHex(PtrUInt(ANode), 2*sizeof(PtrUInt)),
               ' Not Found  ', IntToHex(PtrUInt(ANode^.NotFoundEntry), 2*sizeof(PtrUInt)),
               ' Dict ', IntToHex(PtrUInt(ANode^.DictLink), 2*sizeof(PtrUInt))
               ]);
    j := ANode^.NextCharMin;
    ArrayLen := ArrayLen + ANode^.NextCharMax - ANode^.NextCharMin + 1;
    for i := ANode^.NextCharMin to ANode^.NextCharMax do
      if ANode^.NextEntry[i-j] <> nil then begin
        if not OnlySummary then
          debugln([AIndent, '> ', FlipByte(i)]);
        DebugNode(ANode^.NextEntry[i-j], APreFix+chr(FlipByte(i)), AIndent+'  ');
      end
      else
        inc(EmptyCnt);
  end;
begin
  if FRootNode = nil then
    BuildDictionary;
  ArrayLen := 0;
  NCnt := 0;
  EmptyCnt := 0;
  DebugNode(FRootNode);
  DebugLn(['Nodes: ', NCnt, '  Sum(len(array))=', ArrayLen, ' Empty=', EmptyCnt]);
end;
{$ENDIF}

procedure TSynSearchDictionary.Clear;
begin
  FList.Clear;
  ClearDictionary;
end;

function TSynSearchDictionary.Add(ATerm: String; ATag: Integer): Integer;
begin
  Result := FList.AddObject(ATerm, TObject(PtrInt(ATag)));
  ClearDictionary;
end;

function TSynSearchDictionary.IndexOf(ATerm: String): Integer;
begin
  Result := FList.IndexOf(ATerm);
end;

procedure TSynSearchDictionary.Remove(ATerm: String);
begin
  FList.Delete(FList.IndexOf(ATerm));
  ClearDictionary;
end;

procedure TSynSearchDictionary.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
  ClearDictionary;
end;

function TSynSearchDictionary.Count: Integer;
begin
  Result := FList.Count;
end;

function TSynSearchDictionary.Search(AText: PChar; ATextLen: Integer;
  AFoundEvent: TSynSearchDictFoundEvent): PChar;
var
  DictLink, CurrentNode: PSynSearchDictionaryNode;
  b, m: Integer;
  IsMatch, DoWork: Boolean;
  TextEnd: PChar;
  HasNextNode: Boolean;
begin
  Result := nil;
  if AText = nil then
    exit;
  if FList.Count = 0 then
    exit;
  if FRootNode = nil then
    BuildDictionary;

  DoWork := True;
  CurrentNode := FRootNode;
  TextEnd := AText + ATextLen;

  Repeat
    b := ord(AText^);
    if b > 128 then b := 383 - b;
    m := CurrentNode^.NextCharMin;
    HasNextNode := (b >= m) and (b <= CurrentNode^.NextCharMax) and
                   (CurrentNode^.NextEntry[b-m] <> nil);

    if HasNextNode then begin
      // DictLink, before going to next node
      // If we do not have a next node, then we will continue with NotFoundEntry, so we do not need to test here (yet)
      DictLink := CurrentNode^.DictLink;
      if DictLink <> nil then begin
        repeat
        //while DictLink <> nil do begin
          IsMatch := True;
          Result := AText;
          if Assigned(AFoundEvent) then
            AFoundEvent(AText, DictLink^.ItemIdx, IsMatch, DoWork)
          else
            exit;
          if not DoWork then
            exit;
          if IsMatch then
            break;
          DictLink := DictLink^.DictLink;
        until DictLink = nil;
        if IsMatch then begin
          CurrentNode := FRootNode; // Do not do overlapping matches
          continue;
        end;
      end;
    end;

    if HasNextNode then begin
      if AText >= TextEnd then
        break;
      CurrentNode := CurrentNode^.NextEntry[b-m];                  // go on with next char
      inc(AText);
    end
    else begin
      CurrentNode := CurrentNode^.NotFoundEntry; // check current char again

      if CurrentNode = nil then begin
        if AText >= TextEnd then
          break;
        CurrentNode := FRootNode;
        inc(AText);
        Continue;
      end;
    end;


    // Check match in CurrentNode;
    if CurrentNode^.ItemIdx >= 0 then begin
      IsMatch := True;
      Result := AText;
      if Assigned(AFoundEvent) then
        AFoundEvent(AText, CurrentNode^.ItemIdx, IsMatch, DoWork)
      else
        exit;
      if not DoWork then
        exit;
      if IsMatch then
        CurrentNode := FRootNode; // Do not do overlapping matches
    end;

  until False;
end;

function TSynSearchDictionary.GetMatchAtChar(AText: PChar; ATextLen: Integer;
  AFoundEvent: TSynSearchDictFoundEvent): Integer;
var
  CurrentNode: PSynSearchDictionaryNode;
  b, m: Integer;
  TextEnd: PChar;
  IsMatch, DoWork: Boolean;
begin
  Result := -1;
  if FList.Count = 0 then
    exit;
  if FRootNode = nil then
    BuildDictionary;

  DoWork := True;
  CurrentNode := FRootNode;
  TextEnd := AText + ATextLen;
  b := ord(AText^);
  if b > 128 then b := 383 - b;

  while true do begin
    // CurrentNode is for (AText-1)^
    // b is for AText^
    if CurrentNode^.ItemIdx >= 0 then begin
      Result := CurrentNode^.ItemIdx;
      IsMatch := True;
      if Assigned(AFoundEvent) then
        AFoundEvent(AText, CurrentNode^.ItemIdx, IsMatch, DoWork)
      else
        exit;
      if (not DoWork) or (IsMatch) then
        exit;
    end;

    m := CurrentNode^.NextCharMin;
    if (b >= m) and (b <= CurrentNode^.NextCharMax) and
       (CurrentNode^.NextEntry[b-m] <> nil)
    then begin
      CurrentNode := CurrentNode^.NextEntry[b-m];
      inc(AText);
      if AText > TextEnd then
        exit;
      b := ord(AText^);
      if b > 128 then b := 383 - b;
      continue;
    end;

    exit;
  end;
end;

{ TSynSearchTerm }

procedure TSynSearchTerm.SetMatchCase(AValue: Boolean);
begin
  if FMatchCase = AValue then Exit;
  FMatchCase := AValue;
  Changed(False);
end;

procedure TSynSearchTerm.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TSynSearchTerm.SetMatchWordBounds(AValue: TSynSearchTermOptsBounds);
begin
  if FMatchWordBounds = AValue then Exit;
  FMatchWordBounds := AValue;
  Changed(False);
end;

procedure TSynSearchTerm.SetSearchTerm(AValue: String);
begin
  if FSearchTerm = AValue then Exit;
  FSearchTerm := AValue;
  Changed(False);
end;

constructor TSynSearchTerm.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMatchCase := False;
  FMatchWordBounds := soNoBounds;
  FEnabled := True;
end;

procedure TSynSearchTerm.Assign(Source: TPersistent);
begin
  if not(Source is TSynSearchTerm) then
    exit;
  FSearchTerm      := TSynSearchTerm(Source).FSearchTerm;
  FMatchCase       := TSynSearchTerm(Source).FMatchCase;
  FMatchWordBounds := TSynSearchTerm(Source).FMatchWordBounds;
  FEnabled         := TSynSearchTerm(Source).FEnabled;
  Changed(False);
end;

function TSynSearchTerm.Equals(Other: TSynSearchTerm): boolean;
begin
  Result := (FMatchCase       = Other.FMatchCase) and
            (FMatchWordBounds = Other.FMatchWordBounds) and
            (FSearchTerm      = Other.FSearchTerm);
end;

{ TSynSearchTermList }

function TSynSearchTermList.GetItem(Index: Integer): TSynSearchTerm;
begin
  Result := TSynSearchTerm(inherited GetItem(Index));
end;

procedure TSynSearchTermList.SetItem(Index: Integer; AValue: TSynSearchTerm);
begin
  inherited SetItem(Index, AValue);
end;

procedure TSynSearchTermList.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TSynSearchTermList.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TSynSearchTermList.DefaultItemClass: TSynSearchTermClass;
begin
  Result := TSynSearchTerm;
end;

constructor TSynSearchTermList.Create;
begin
  inherited Create(DefaultItemClass);
end;

function TSynSearchTermList.Add: TSynSearchTerm;
begin
  Result := TSynSearchTerm(inherited Add);
end;

function TSynSearchTermList.IndexOfSearchTerm(ATerm: String;
  ASearchStartIdx: Integer): Integer;
begin
  Result := IndexOfSearchTerm(ATerm, True, ASearchStartIdx);
end;

function TSynSearchTermList.IndexOfSearchTerm(ATerm: TSynSearchTerm;
  ASearchStartIdx: Integer): Integer;
var
  c: Integer;
begin
  Result := ASearchStartIdx;
  c := Count ;
  while (Result < c) and (not Items[Result].Equals(ATerm)) do
    inc(Result);
  if Result >= c then
    Result := -1;
end;

function TSynSearchTermList.IndexOfSearchTerm(ATerm: String; ACaseSensitive: Boolean;
  ASearchStartIdx: Integer): Integer;
var
  c: Integer;
begin
  Result := ASearchStartIdx;
  c := Count ;
  if ACaseSensitive then begin
    while (Result < c) and (Items[Result].SearchTerm <> ATerm) do
      inc(Result);
  end
  else begin
    ATerm := LowerCase(ATerm);
    while (Result < c) and (LowerCase(Items[Result].SearchTerm) <> ATerm) do
      inc(Result);
  end;
  if Result >= c then
    Result := -1;
end;

{ TSynEditMarkupHighlightAllMulti }

function TSynEditMarkupHighlightAllMulti.HasSearchData: Boolean;
begin
  Result := FTermDict.Count > 0;
end;

function TSynEditMarkupHighlightAllMulti.SearchStringMaxLines: Integer;
begin
  Result := 1; // Todo: implement multiline
end;

procedure TSynEditMarkupHighlightAllMulti.FindInitialize;
begin
  //
end;

procedure TSynEditMarkupHighlightAllMulti.DoMatchFound(MatchEnd: PChar; MatchIdx: Integer;
  var IsMatch: Boolean; var StopSeach: Boolean);
var
  i, NextInsertIdx, Len: Integer;
  o: TSynSearchTerm;
  MatchBegin: PChar;
begin
  Len := 0;
  MatchBegin := nil;
  while MatchIdx >= 0 do begin
    o := FTermDict[MatchIdx];

    if not o.Enabled then begin
      MatchIdx := FTermDict.GetIndexForNextWordOccurrence(MatchIdx);
      continue;
    end;

    Len := length(o.SearchTerm);
    MatchBegin := MatchEnd - Len - FFindLineTextLower + FFindLineText;

    if o.MatchCase and not(copy(MatchBegin, 1, Len) = o.SearchTerm) then begin
      MatchIdx := FTermDict.GetIndexForNextWordOccurrence(MatchIdx);
      continue;
    end;

    if (o.MatchWordBounds in [soBoundsAtStart, soBothBounds]) and
       not( (MatchBegin = FFindLineText) or
            ((MatchBegin-1)^ in WordBreakChars)
          )
    then begin
      MatchIdx := FTermDict.GetIndexForNextWordOccurrence(MatchIdx);
      continue;
    end;

    if (o.MatchWordBounds in [soBoundsAtEnd, soBothBounds]) and
       not( (MatchBegin+Len = FFindLineTextEnd) or
            ((MatchBegin+Len)^ in WordBreakChars)
          )
    then begin
      MatchIdx := FTermDict.GetIndexForNextWordOccurrence(MatchIdx);
      continue;
    end;

    break;
  end;

  IsMatch := False; // Continue for longer match //MatchIdx >= 0;
  if MatchIdx < 0 then
    exit;

  NextInsertIdx := FFindInsertIndex;
  if FBackwardReplace then
    inc(NextInsertIdx); // because FFindInsertIndex was not increased;
  i := NextInsertIdx;
  if (NextInsertIdx > FFindStartedAtIndex) then begin
    //only searching one line at a time. So only checking x
    Assert(FFindLineY = FMatches.EndPoint[NextInsertIdx-1].Y);
    While (i > FFindStartedAtIndex) and
          (MatchBegin-FFindLineText+1 < FMatches.EndPoint[i-1].X)  // Starts within or before previous
    do
      dec(i);
    if (i < NextInsertIdx) and (Len <= (FMatches.EndPoint[i].X - FMatches.StartPoint[i].X))
    then
      i := NextInsertIdx;
  end;

  if (i < NextInsertIdx) then begin
    //DebugLn(['Replacing match at idx=', i, ' Back:', FFindInsertIndex-i, ' y=', FFindLineY,
    //         ' x1=', FMatches.StartPoint[i].X, ' x2=', MatchBegin-FFindLineText+1, ' with longer. Len=', Len]);
    FMatches.StartPoint[i] := Point(MatchBegin-FFindLineText+1, FFindLineY);
    FMatches.EndPoint[i]   := Point(MatchBegin-FFindLineText+1+Len, FFindLineY);
    if i + 1 < FFindInsertIndex then
      FMatches.Delete(i+1, FFindInsertIndex - (i + 1));
    if not FBackward then
      FFindInsertIndex := i + 1
    else assert(i = FFindInsertIndex);
  end
  else begin
    if FBackwardReplace then begin
      // Replace, only keep last match
      FMatches.StartPoint[FFindInsertIndex] := Point(MatchBegin-FFindLineText+1, FFindLineY);
      FMatches.EndPoint[FFindInsertIndex]   := Point(MatchBegin-FFindLineText+1+Len, FFindLineY);
    end
    else
      FMatches.Insert(FFindInsertIndex,
                      Point(MatchBegin-FFindLineText+1, FFindLineY),
                      Point(MatchBegin-FFindLineText+1+Len, FFindLineY)
                     );
    if not FBackward then
      inc(FFindInsertIndex)
    else
      FBackwardReplace := True;
  end;
end;

procedure TSynEditMarkupHighlightAllMulti.SetTerms(AValue: TSynSearchTermDict);
begin
  if FTermDict = AValue then Exit;

  if FTermDict <> nil then begin
    FTermDict.UnRegisterChangedHandler(@DoTermsChanged);
    FTermDict.ReleaseReference;
  end;

  if AValue = nil then
    FTermDict := CreateTermsList
  else
    FTermDict := AValue;

  FTermDict.AddReference;
  FTermDict.RegisterChangedHandler(@DoTermsChanged);
end;

procedure TSynEditMarkupHighlightAllMulti.SetWordBreakChars(AValue: TSynIdentChars);
begin
  if FWordBreakChars = AValue then Exit;
  FWordBreakChars := AValue;
  Invalidate;
end;

procedure TSynEditMarkupHighlightAllMulti.DoTermsChanged(Sender: TObject);
begin
  if (FTermDict = nil) then
    exit;
  Invalidate;
end;

function TSynEditMarkupHighlightAllMulti.FindMatches(AStartPoint, AEndPoint: TPoint;
  var AIndex: Integer; AStopAfterLine: Integer; ABackward: Boolean): TPoint;
var
  LineLen: Integer;
  LineText, LineTextLower: String;
  x: integer;
begin
  //debugln(['FindMatches IDX=', AIndex, ' Cnt=', Matches.Count, ' LCnt=', AEndPoint.y-AStartPoint.y , ' # ', FTerms[0].SearchTerm, ' # ',dbgs(AStartPoint),' - ',dbgs(AEndPoint), ' AStopAfterLine=',AStopAfterLine, ' Back=', dbgs(ABackward), '  ']);
  FFindInsertIndex := AIndex;
  FFindStartedAtIndex := FFindInsertIndex;
  FBackward := ABackward; // Currently supports only finding a single match

  if ABackward then begin
    FBackwardReplace := False;
    x := 1;
    while AStartPoint.y <= AEndPoint.y do begin
      LineText := Lines[AEndPoint.y-1];
      LineTextLower := LowerCase(LineText);

      LineLen := Min(Length(LineTextLower), AEndPoint.x-1);
      if (AStartPoint.y = AEndPoint.y) and (AStartPoint.x > 1) then begin
        x := AStartPoint.x - 1;
        LineLen := Max(0, LineLen - x);
      end;

      if LineLen > 0 then begin
        FFindLineY := AEndPoint.Y;
        FFindStartedAtIndex := FFindInsertIndex;
        FFindLineText := @LineText[1];
        FFindLineTextEnd := FFindLineText + LineLen;
        FFindLineTextLower := @LineTextLower[1];
        FFindLineTextLowerEnd := FFindLineTextLower + LineLen;
        if LineLen > 0 then
          FTermDict.Search(@LineTextLower[x], LineLen, @DoMatchFound);
      end;

      if FBackwardReplace then
        break; // Only one supported

      dec(AEndPoint.y);
      AEndPoint.x := MaxInt;

      //if (AStopAfterLine >= 0) and (AStartPoint.Y-1 > AStopAfterLine) and
      //   (FFindInsertIndex > AIndex)
      //then begin
      //  AEndPoint := point(LineLen, AStartPoint.Y-1);
      //  break;
      //end;
    end;

    if FBackwardReplace then
      inc(FFindInsertIndex);
  end
  else begin
    while AStartPoint.y <= AEndPoint.y do begin
      LineText := Lines[AStartPoint.y-1];
      LineTextLower := LowerCase(LineText);

      LineLen := Length(LineTextLower);
      if AStartPoint.y = AEndPoint.y then
        LineLen := Min(LineLen, AEndPoint.x - AStartPoint.x + 1);

      if LineLen > 0 then begin
        FFindLineY := AStartPoint.Y;
        FFindStartedAtIndex := FFindInsertIndex;
        FFindLineText := @LineText[1];
        FFindLineTextEnd := FFindLineText + LineLen;
        FFindLineTextLower := @LineTextLower[1];
        FFindLineTextLowerEnd := FFindLineTextLower + LineLen;
        if LineLen > 0 then
          FTermDict.Search(@LineTextLower[1] + AStartPoint.x - 1, LineLen, @DoMatchFound);
      end;

      inc(AStartPoint.y);
      AStartPoint.x := 1;

      if (AStopAfterLine >= 0) and (AStartPoint.Y-1 > AStopAfterLine) and
         (FFindInsertIndex > AIndex)
      then begin
        AEndPoint := point(LineLen, AStartPoint.Y-1);
        break;
      end;
    end;
  end;

  AIndex := FFindInsertIndex;
  Result := AEndPoint;
end;

function TSynEditMarkupHighlightAllMulti.CreateTermsList: TSynSearchTermDict;
begin
  Result := TSynSearchTermDict.Create(TSynSearchTermList);
end;

constructor TSynEditMarkupHighlightAllMulti.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  Terms := CreateTermsList;
  ResetWordBreaks;
end;

destructor TSynEditMarkupHighlightAllMulti.Destroy;
begin
  inherited Destroy;
  FTermDict.UnRegisterChangedHandler(@DoTermsChanged);
  ReleaseRefAndNil(FTermDict);
end;

procedure TSynEditMarkupHighlightAllMulti.Clear;
begin
  FTermDict.Clear;
end;

procedure TSynEditMarkupHighlightAllMulti.ResetWordBreaks;
begin
  FWordBreakChars := TSynWordBreakChars + TSynWhiteChars;
  FTermDict.ClearDictionary;
  Invalidate;
end;

function TSynEditMarkupHighlightAllMulti.AddSearchTerm(ATerm: String): Integer;
var
  Itm: TSynSearchTerm;
begin
  Itm := FTermDict.Add;
  Itm.SearchTerm := ATerm;
  Result := Itm.Index;
end;

function TSynEditMarkupHighlightAllMulti.IndexOfSearchTerm(ATerm: String): Integer;
begin
  Result:= FTermDict.IndexOfSearchTerm(ATerm);
end;

procedure TSynEditMarkupHighlightAllMulti.RemoveSearchTerm(ATerm: String);
begin
  FTermDict.Delete(IndexOfSearchTerm(ATerm));
end;

procedure TSynEditMarkupHighlightAllMulti.DeleteSearchTerm(AIndex: Integer);
begin
  FTermDict.Delete(AIndex);
end;

{ TSynEditMarkupHighlightAll }

procedure TSynEditMarkupHighlightAll.SetSearchOptions(AValue: TSynSearchOptions);
begin
  if fSearchOptions = AValue then exit;
  fSearchOptions := AValue;
  FSearchStringMaxLines := -1;
  Invalidate;
  DoOptionsChanged;
end;

procedure TSynEditMarkupHighlightAll.SetSearchString(AValue: String);
begin
  if FSearchString = AValue then exit;
  FSearchString := AValue;
  FSearchStringMaxLines := -1;
  Invalidate; // bad if options and string search at the same time *and* string is <> ''

  SearchStringChanged;
end;

procedure TSynEditMarkupHighlightAll.SearchStringChanged;
begin
  //
end;

procedure TSynEditMarkupHighlightAll.DoOptionsChanged;
begin
  //
end;

function TSynEditMarkupHighlightAll.HasSearchData: Boolean;
begin
  Result := FSearchString <> '';
end;

function TSynEditMarkupHighlightAll.SearchStringMaxLines: Integer;
var
  i, j: Integer;
begin
  Result := FSearchStringMaxLines;
  if Result > 0 then
    exit;

  if (fSearchOptions * [ssoRegExpr, ssoRegExprMultiLine] = [])
  then begin
    // can not wrap around lines
    j := 1;
    i := Length(fSearchString);
    while i > 0 do begin
      if fSearchString[i] = #13 then begin
        inc(j);
        if (i > 1) and (fSearchString[i-1] = #10) then dec(i); // skip alternating
      end
      else
      if fSearchString[i] = #10 then begin
        inc(j);
        if (i > 1) and (fSearchString[i-1] = #13) then dec(i); // skip alternating
      end;
      dec(i);
    end;
    FSearchStringMaxLines := j;
  end
  else begin
    if (fSearchOptions * [ssoRegExpr, ssoRegExprMultiLine] = [ssoRegExpr]) then
      FSearchStringMaxLines := 1    // Only ssoRegExprMultiLine can expand accross lines (actually \n\r should anymay...)
    else
      FSearchStringMaxLines := 0; // Unknown
  end;

  Result := FSearchStringMaxLines;
end;

procedure TSynEditMarkupHighlightAll.FindInitialize;
begin
  fSearch.Pattern   := fSearchString;
  fSearch.Sensitive := ssoMatchCase in fSearchOptions;
  fSearch.Whole     := ssoWholeWord in fSearchOptions;
  fSearch.RegularExpressions := ssoRegExpr in fSearchOptions;
  fSearch.RegExprMultiLine   := ssoRegExprMultiLine in fSearchOptions;
  fSearch.Backwards := False;
end;

function TSynEditMarkupHighlightAll.FindMatches(AStartPoint, AEndPoint: TPoint;
  var AIndex: Integer; AStopAfterLine: Integer; ABackward: Boolean): TPoint;
var
  ptFoundStart, ptFoundEnd: TPoint;
begin
  fSearch.Backwards := ABackward;
  While (true) do begin
    if not fSearch.FindNextOne(Lines, AStartPoint, AEndPoint, ptFoundStart, ptFoundEnd)
    then break;
    AStartPoint := ptFoundEnd;

    FMatches.Insert(AIndex, ptFoundStart, ptFoundEnd);
    inc(AIndex); // BAckward learch needs final index to point to last inserted (currently support only find ONE)

    if (AStopAfterLine >= 0) and (ptFoundStart.Y > AStopAfterLine) then begin
      AEndPoint := ptFoundEnd;
      break;
    end;
  end;
  Result := AEndPoint;
end;

constructor TSynEditMarkupHighlightAll.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  FSearch := TSynEditSearch.Create;
  FSearchString:='';
  FSearchOptions := [];
end;

destructor TSynEditMarkupHighlightAll.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSearch);
end;

{ TSynMarkupHighAllMatchList }

constructor TSynMarkupHighAllMatchList.Create;
begin
  inherited Create;
  Count := 0;
  Capacity := 256;
end;

function TSynMarkupHighAllMatchList.MaybeReduceCapacity : Boolean;
begin
  if not( (Capacity > 512) and (Capacity > Count*4) )
  then exit(False);

  Capacity := Capacity div 2;
  result := true;
end;

function TSynMarkupHighAllMatchList.IndexOfFirstMatchForLine(ALine: Integer): Integer;
var
  l, h: Integer;
begin
  if Count = 0 then
    exit(-1);
  l := 0;
  h := Count -1;
  Result := (l+h) div 2;
  while (h > l) do begin
    if PSynMarkupHighAllMatch(ItemPointer[Result])^.EndPoint.y >= ALine then
      h := Result
    else
      l := Result + 1;
    Result := (l+h) div 2;
  end;
  if (PSynMarkupHighAllMatch(ItemPointer[Result])^.EndPoint.y < ALine) then
    inc(Result);
end;

function TSynMarkupHighAllMatchList.IndexOfLastMatchForLine(ALine: Integer): Integer;
var
  l, h: Integer;
begin
  if Count = 0 then
    exit(-1);
  l := 0;
  h := Count -1;
  Result := (l+h) div 2;
  while (h > l) do begin
    if PSynMarkupHighAllMatch(ItemPointer[Result])^.StartPoint.y <= ALine then
      l := Result + 1
    else
      h := Result;
    Result := (l+h) div 2;
  end;
  if (PSynMarkupHighAllMatch(ItemPointer[Result])^.StartPoint.y > ALine) then
    dec(Result);
end;

procedure TSynMarkupHighAllMatchList.Delete(AIndex: Integer; ACount: Integer);
begin
  if AIndex >= Count then
    exit;
  if AIndex + ACount > Count then
    ACount := Count - AIndex
  else
  DeleteRows(AIndex, ACount);
end;

procedure TSynMarkupHighAllMatchList.Insert(AIndex: Integer; ACount: Integer);
begin
  if AIndex > Count then
    exit;
  InsertRows(AIndex, ACount);
end;

procedure TSynMarkupHighAllMatchList.Insert(AIndex: Integer; AStartPoint, AEndPoint: TPoint);
begin
  Insert(AIndex);
  PSynMarkupHighAllMatch(ItemPointer[AIndex])^.StartPoint := AStartPoint;
  PSynMarkupHighAllMatch(ItemPointer[AIndex])^.EndPoint   := AEndPoint;
end;

procedure TSynMarkupHighAllMatchList.SetCount(const AValue : Integer);
begin
  if Count=AValue then exit;
  if (Capacity <= AValue) then begin
    Capacity := Max(Capacity, AValue) * 2;
    inherited SetCount(AValue);
  end
  else begin
    inherited SetCount(AValue);
    MaybeReduceCapacity;
  end;
end;

function TSynMarkupHighAllMatchList.GetPointCount : Integer;
begin
  result := Count * 2;
end;

function TSynMarkupHighAllMatchList.GetPoint(const Index : Integer) : TPoint;
begin
  if (Index and 1) = 0
  then Result := PSynMarkupHighAllMatch(ItemPointer[Index>>1])^.StartPoint
  else Result := PSynMarkupHighAllMatch(ItemPointer[Index>>1])^.EndPoint
end;

function TSynMarkupHighAllMatchList.GetStartPoint(const Index : Integer) : TPoint;
begin
  Result := PSynMarkupHighAllMatch(ItemPointer[Index])^.StartPoint;
end;

procedure TSynMarkupHighAllMatchList.SetStartPoint(const Index : Integer; const AValue : TPoint);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  PSynMarkupHighAllMatch(ItemPointer[Index])^.StartPoint := AValue;
end;

function TSynMarkupHighAllMatchList.GetInintialForItemSize: Integer;
begin
  Result := SizeOf(TSynMarkupHighAllMatch);
end;

function TSynMarkupHighAllMatchList.GetEndPoint(const Index : Integer) : TPoint;
begin
  Result := PSynMarkupHighAllMatch(ItemPointer[Index])^.EndPoint;
end;

procedure TSynMarkupHighAllMatchList.SetEndPoint(const Index : Integer; const AValue : TPoint);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  PSynMarkupHighAllMatch(ItemPointer[Index])^.EndPoint := AValue;
end;

function TSynMarkupHighAllMatchList.GetMatch(const Index: Integer): TSynMarkupHighAllMatch;
begin
  Result := PSynMarkupHighAllMatch(ItemPointer[Index])^;
end;

procedure TSynMarkupHighAllMatchList.SetMatch(const Index: Integer;
  const AValue: TSynMarkupHighAllMatch);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  PSynMarkupHighAllMatch(ItemPointer[Index])^ := AValue;
end;

{ TSynMarkupHighAllMultiMatchList }

function TSynMarkupHighAllMultiMatchList.GetMarkupId(Index: Integer): Integer;
begin
  Result := PInteger(ItemPointer[Index]+FParentItemSize)^;
end;

procedure TSynMarkupHighAllMultiMatchList.SetMarkupId(Index: Integer; AValue: Integer);
begin
  PInteger(ItemPointer[Index]+FParentItemSize)^ := AValue;
end;

function TSynMarkupHighAllMultiMatchList.GetInintialForItemSize: Integer;
begin
  Result := inherited GetInintialForItemSize;
  FParentItemSize := Result;
  Result := FParentItemSize + SizeOf(Integer);
end;

{ TSynEditMarkupHighlightAllBase }

constructor TSynEditMarkupHighlightAllBase.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  fStartPoint.y := -1;
  FSearchedEnd.y := -1;
  FFirstInvalidLine := 1;
  FLastInvalidLine := MaxInt;
  FHideSingleMatch := False;
  FMarkupEnabled := MarkupInfo.IsEnabled;
end;

destructor TSynEditMarkupHighlightAllBase.Destroy;
begin
  if Lines <> nil then
    Lines.RemoveChangeHandler(senrLineMappingChanged, @DoFoldChanged);
  inherited Destroy;
end;

procedure TSynEditMarkupHighlightAllBase.DecPaintLock;
begin
  inherited DecPaintLock;
  if (FPaintLock = 0) and FNeedValidate then
    ValidateMatches(not FNeedValidatePaint);
end;

procedure TSynEditMarkupHighlightAllBase.DoTopLineChanged(OldTopLine : Integer);
begin
  // {TODO: Only do a partial search on the new area}
  ValidateMatches(True);
end;

procedure TSynEditMarkupHighlightAllBase.DoLinesInWindoChanged(OldLinesInWindow : Integer);
begin
  // {TODO: Only do a partial search on the new area}
  ValidateMatches(True);
end;

procedure TSynEditMarkupHighlightAllBase.DoMarkupChanged(AMarkup : TSynSelectedColor);
begin
  If (not FMarkupEnabled) and MarkupInfo.IsEnabled then
    Invalidate
  else
    SendLineInvalidation;
  FMarkupEnabled := MarkupInfo.IsEnabled;
end;

procedure TSynEditMarkupHighlightAllBase.DoEnabledChanged(Sender: TObject);
begin
  Invalidate;
end;

function TSynEditMarkupHighlightAllBase.GetMatchCount: Integer;
begin
  Result := fMatches.Count;
end;

procedure TSynEditMarkupHighlightAllBase.SetHideSingleMatch(AValue: Boolean);
begin
  if FHideSingleMatch = AValue then Exit;
  FHideSingleMatch := AValue;
  if FMatches.Count = 1 then
    if FHideSingleMatch then
      Invalidate // TODO only need extend search
      //ValidateMatches()  // May find a 2nd, by extending startpos
    else
      SendLineInvalidation; // Show the existing match
end;

procedure TSynEditMarkupHighlightAllBase.DoFoldChanged(Sender: TSynEditStrings;
  aIndex, aCount: Integer);
begin
  InvalidateLines(aIndex+1, MaxInt, True);
end;

procedure TSynEditMarkupHighlightAllBase.ValidateMatches(SkipPaint: Boolean);
var
  LastLine : Integer;     // Last visible
  UnsentLineInvalidation: Integer;

  function IsPosValid(APos: TPoint): Boolean; // Check if point is in invalid range
  begin
    Result := (APos.y > 0) and
       ( (FFirstInvalidLine < 1) or (APos.y < FFirstInvalidLine) or
         ( (FLastInvalidLine > 0) and (APos.y > FLastInvalidLine) )
       );
  end;

  function HasInvalidationBetween(ARangeStart, ARangeEnd: TPoint): Boolean; // Check if point is in invalid range
  begin
    Result :=
      ((FFirstInvalidLine >= ARangeStart.y) and (FFirstInvalidLine <= ARangeEnd.y)) or
      ((FLastInvalidLine >= ARangeStart.y) and (FLastInvalidLine <= ARangeEnd.y));
  end;

  function IsStartAtMatch0: Boolean; // Check if FStartPoint = FMatches[0]
  begin
    Result := (FMatches.Count > 0) and
              (FStartPoint.y = FMatches.StartPoint[0].y)and (FStartPoint.x = FMatches.StartPoint[0].x);
  end;

  function IsEndAtMatch(APoint: TPoint): Boolean;
  begin
    Result := (FMatches.Count > 0) and
              (APoint.y = FMatches.EndPoint[FMatches.Count].y)and (APoint.x = FMatches.EndPoint[FMatches.Count].x);
  end;

  function AdjustedSearchStrMaxLines: Integer;
  begin
    Result := SearchStringMaxLines - 1;
    if Result < 0 then Result := SEARCH_START_OFFS;
  end;

  procedure MaybeSendLineInvalidation(AFirstIndex, ALastIndex: Integer);
  begin
    if SkipPaint or (ALastIndex < AFirstIndex) then
      exit;
    if HideSingleMatch and (FMatches.Count = 1) then begin
      assert((UnsentLineInvalidation < 0) and (AFirstIndex = 0) and (ALastIndex=0), 'UnsentLineInvalidation < 0');
      UnsentLineInvalidation := AFirstIndex;
      exit;
    end;

    SendLineInvalidation(AFirstIndex, ALastIndex);
    if UnsentLineInvalidation >= 0 then
      SendLineInvalidation(UnsentLineInvalidation, UnsentLineInvalidation);
    UnsentLineInvalidation := -1;
  end;

  procedure MaybeDropOldMatches;
  var
    Idx: Integer;
  begin
    // remove matches, that are too far off the current visible area
    if (FMatches.Count > MATCHES_CLEAN_CNT_THRESHOLD) then begin
      if TopLine - FMatches.EndPoint[0].y > MATCHES_CLEAN_LINE_THRESHOLD then begin
        Idx := FMatches.IndexOfFirstMatchForLine(TopLine - MATCHES_CLEAN_LINE_KEEP) - 1;
        FMatches.Delete(0, Idx);
        if FMatches.Count > 0
        then FStartPoint := FMatches.StartPoint[0]
        else FStartPoint.y := -1;
      end;
      if FMatches.StartPoint[FMatches.Count-1].y - LastLine > MATCHES_CLEAN_LINE_THRESHOLD then begin
        Idx := FMatches.IndexOfLastMatchForLine(LastLine  + MATCHES_CLEAN_LINE_KEEP) + 1;
        FMatches.Delete(Idx, FMatches.Count - Idx);
        if FMatches.Count > 0
        then FSearchedEnd := FMatches.EndPoint[FMatches.Count-1]
        else FSearchedEnd.y := -1;
      end;
    end;
  end;

  function DeleteInvalidMatches: Integer;
  var
    FirstInvalIdx, LastInvalIdx: Integer;
  begin
    // Delete Matches from the invalidated line range
    FirstInvalIdx := -1;
    LastInvalIdx  := -1;
    if (FFirstInvalidLine > 0) or (FLastInvalidLine > 0) then begin
      FirstInvalIdx := FMatches.IndexOfFirstMatchForLine(FFirstInvalidLine);
      LastInvalIdx  := FMatches.IndexOfLastMatchForLine(FLastInvalidLine);
      if (FirstInvalIdx >= 0) and (FirstInvalIdx <= LastInvalIdx) then begin
        if (not SkipPaint) and HasVisibleMatch then
          SendLineInvalidation(FirstInvalIdx, LastInvalIdx);
        FMatches.Delete(FirstInvalIdx, LastInvalIdx-FirstInvalIdx+1);
        if FirstInvalIdx > FMatches.Count then
          FirstInvalIdx := FMatches.Count;
      end;
    end;
    Result := FirstInvalIdx;
  end;

  function FindStartPoint(var AFirstKeptValidIdx: Integer): Boolean;
  var
    Idx : Integer;
  begin
    Result := False; // No Gap at start to fill

    if (FMatches.Count > 0) and (FMatches.StartPoint[0].y < TopLine) then begin
      // New StartPoint from existing matches
      Result := True;
      FStartPoint := FMatches.StartPoint[0];
      if AFirstKeptValidIdx = 0 then
        AFirstKeptValidIdx := -1;
    end

    else begin
      if SearchStringMaxLines > 0 then
        // New StartPoint at fixed offset
        FStartPoint := Point(1, TopLine - (SearchStringMaxLines - 1))
      else begin
        // New StartPoint Search backward
        Idx := 0;
        FindMatches(Point(1, Max(1, TopLine-SEARCH_START_OFFS)),
                    Point(1, TopLine),
                    Idx, 0, True); // stopAfterline=0, do only ONE find
        if Idx > 0 then begin
          FStartPoint := FMatches.StartPoint[0];
          if (AFirstKeptValidIdx >= 0) then
            inc(AFirstKeptValidIdx, Idx);
        end
        else
          FStartPoint := Point(1, TopLine)     // no previous match found
      end;
    end;
  end;

  procedure MaybeExtendForHideSingle;
  var
    EndOffsLine: Integer;
    Idx: Integer;
  begin
    // Check, if there is exactly one match in the visible lines
    if (not HideSingleMatch) or (Matches.Count <> 1) or
       (FMatches.StartPoint[0].y < TopLine) or (FMatches.StartPoint[0].y > LastLine)
    then
      exit;

    // search 2nd, if HideSingleMatch;
    EndOffsLine := min(LastLine+Max(SEARCH_START_OFFS, AdjustedSearchStrMaxLines), Lines.Count);
    if EndOffsLine > FSearchedEnd.y then begin
      FSearchedEnd.y := FSearchedEnd.y - AdjustedSearchStrMaxLines;
      if ComparePoints(FSearchedEnd, FMatches.EndPoint[0]) < 0 then
        FSearchedEnd := FMatches.EndPoint[0];
      Idx := 1;
      FSearchedEnd := FindMatches(FSearchedEnd,
                                  Point(Length(Lines[EndOffsLine - 1])+1, EndOffsLine),
                                  Idx, LastLine);
      SendLineInvalidation;
      if Idx > 1 then
        exit;
    end;

    // search back from start
    if FStartPoint.y < TopLine-SEARCH_START_OFFS then
      exit;
    Idx := 0;
    FindMatches(Point(1, Max(1, TopLine-SEARCH_START_OFFS)), FStartPoint,
                Idx, 0, True); // stopAfterline=0, do only ONE find // Search backwards
    if Idx > 0 then begin
      if ComparePoints(FStartPoint, FMatches.StartPoint[0]) = 0 then begin
        // bad search: did return endpoint
        FMatches.Delete(0);
        exit;
      end;
      FStartPoint := FMatches.StartPoint[0];
      SendLineInvalidation;
    end
  end;

  procedure FinishValidate;
  begin
    FFirstInvalidLine := 0;
    FLastInvalidLine := 0;
  end;

  procedure DoFullSearch(NeedStartPoint: Boolean);
  var
    dummy: Integer;
    EndOffsLine: Integer;
    Idx, Idx2: Integer;
    p: TPoint;
  begin
    FMatches.Count := 0;
    dummy := -1;
    if NeedStartPoint then
      FindStartPoint(dummy);

    EndOffsLine := min(LastLine+AdjustedSearchStrMaxLines, Lines.Count);

    if IsStartAtMatch0 then begin
      Idx := 1;
      p := FMatches.EndPoint[0];
    end else begin
      Idx := 0;
      p := FStartPoint;
    end;
    Idx2 := Idx;
    FSearchedEnd := FindMatches(p,
                                Point(Length(Lines[EndOffsLine - 1])+1, EndOffsLine),
                                Idx, LastLine);
    if (not SkipPaint) and (Idx > Idx2) and HasVisibleMatch then
      MaybeSendLineInvalidation(0, Idx-1);

    MaybeExtendForHideSingle;
    FinishValidate;
  end;

var
  OldStartPoint, OldEndPoint, GapStartPoint, GapEndPoint: TPoint;
  i, j, EndOffsLine : Integer;  // Stop search (LastLine + Offs)
  Idx, Idx2 : Integer;
  FirstKeptValidIdx: Integer; // The first index, kept after the removed invalidated range
  p, WorkStartPoint: TPoint;
  FindStartPointUsedExistingMatch: Boolean;
begin
  FNextPosIdx := -1;
  FNextPosRow := -1;
  if (FPaintLock > 0) or (not SynEdit.IsVisible) then begin
    FNeedValidate := True;
    if not SkipPaint then
      FNeedValidatePaint := True;
    exit;
  end;
  FNeedValidate := False;

  if (not HasSearchData) or (not MarkupInfo.IsEnabled) then begin
    if (not SkipPaint) and (fMatches.Count > 0) then
      SendLineInvalidation;
    fMatches.Count := 0;
    exit;
  end;

  LastLine := ScreenRowToRow(LinesInWindow+1);
  UnsentLineInvalidation := -1;

  MaybeDropOldMatches;
  FirstKeptValidIdx := DeleteInvalidMatches;
  //DebugLnEnter(['>>> ValidateMatches ', FFirstInvalidLine, '-',FLastInvalidLine, ' 1stKeepIdx: ', FirstKeptValidIdx, ' __Cnt=',FMatches.Count, '__   StartP=',dbgs(FStartPoint), ' SearchedToP=', dbgs(FSearchedEnd),  '  -- ', SynEdit.Name,'.',ClassName]); try
  FindInitialize;

  // Get old valid range as OldStartPoint to OldEndPoint
  OldStartPoint := FStartPoint;
  OldEndPoint   := FSearchedEnd;

  if not IsPosValid(FSearchedEnd) then
    FSearchedEnd.y := -1;

  if (OldStartPoint.y >= 0) and not IsPosValid(OldStartPoint) then
    OldStartPoint := Point(1,
      Min(FLastInvalidLine, MaxInt - AdjustedSearchStrMaxLines) + AdjustedSearchStrMaxLines);
  if (OldStartPoint.y < 0) and (FMatches.Count > 0) then
    OldStartPoint := FMatches.StartPoint[0];

  if (OldEndPoint.y >= 0) and not IsPosValid(OldEndPoint) then
    OldEndPoint := Point(1, FFirstInvalidLine - AdjustedSearchStrMaxLines);
  if (OldEndPoint.y < 0) and (FMatches.Count > 0) then
    OldEndPoint := FMatches.EndPoint[FMatches.Count-1];

  if (OldEndPoint.y <= OldStartPoint.y) or
     (OldEndPoint.y < 0) or (OldStartPoint.y < 0) or
     (OldStartPoint.y > LastLine + MATCHES_CLEAN_LINE_KEEP) or
     (OldEndPoint.y   < TopLine  - MATCHES_CLEAN_LINE_KEEP)
  then begin
    DoFullSearch(True);
    exit;
  end;

  // Find the minimum gap that needs to be cecalculated for invalidated lines
  GapStartPoint.y := -1;
  GapEndPoint.y   := -1;
  if FFirstInvalidLine > 0 then begin
    i := AdjustedSearchStrMaxLines;

    GapStartPoint := point(1, Max(1, FFirstInvalidLine - i));
    if (FirstKeptValidIdx > 0) and
       (ComparePoints(GapStartPoint, FMatches.EndPoint[FirstKeptValidIdx-1]) < 0)
    then
      GapStartPoint := FMatches.EndPoint[FirstKeptValidIdx-1];  // GapStartPoint  is before known good point

    j := Min(FLastInvalidLine, FLastInvalidLine-i) + i;
    GapEndPoint := point(length(SynEdit.Lines[j-1])+1, j);
    if (FirstKeptValidIdx >= 0) and (FirstKeptValidIdx < FMatches.Count) and
       (ComparePoints(GapEndPoint, FMatches.EndPoint[FirstKeptValidIdx]) > 0)
    then
      GapEndPoint := FMatches.EndPoint[FirstKeptValidIdx];  // GapEndPoint  is after known good point

    // Merge ranges (all points are valid / y >= 0)
    if (ComparePoints(GapEndPoint, OldStartPoint) <= 0) or
       (ComparePoints(OldEndPoint, GapStartPoint) <= 0)
    then begin
      // gap outside valid range
      GapStartPoint.y := -1;
      GapEndPoint.y   := -1;
    end
    else
    if (ComparePoints(OldStartPoint, GapStartPoint) >= 0) then begin
      // gap starts before valid range, move start point
      OldStartPoint := GapEndPoint;
      GapStartPoint.y := -1;
      GapEndPoint.y   := -1;
    end
    else
    if (ComparePoints(OldEndPoint, GapEndPoint) <= 0) then begin
      // gap ends after valid range, move end point
      OldEndPoint := GapStartPoint;
      GapStartPoint.y := -1;
      GapEndPoint.y   := -1;
    end;

    if (OldEndPoint.y <= OldStartPoint.y) or
       (OldEndPoint.y < 0) or (OldStartPoint.y < 0)
    then begin
      DoFullSearch(True);
      exit;
    end;
  end;

  // There is some valid range
  // There may be a gap (the gap needs to be inserted at FirstKeptValidIdx)
  //DebugLn(['valid: ',dbgs(OldStartPoint),' - ',dbgs(OldEndPoint), '   gap: ',dbgs(GapStartPoint),' - ',dbgs(GapEndPoint)]);

  if not ( IsPosValid(FStartPoint) and
           ( (IsStartAtMatch0 and (FStartPoint.y < TopLine)) or
             ( ( (FStartPoint.y < TopLine - AdjustedSearchStrMaxLines) or
                 ((FStartPoint.y = TopLine - AdjustedSearchStrMaxLines) and (FStartPoint.x = 1))
               ) and
               (FStartPoint.y > TopLine - Max(MATCHES_CLEAN_LINE_THRESHOLD, 2*AdjustedSearchStrMaxLines) )
             )
           )
         )
  then begin
    FindStartPointUsedExistingMatch := FindStartPoint(FirstKeptValidIdx);

    //, existing point must be in valid range, otherwise:
    if not FindStartPointUsedExistingMatch then begin
      if IsStartAtMatch0 then begin
        Idx := 1;
        WorkStartPoint := FMatches.EndPoint[0];
      end else begin
        Idx := 0;
        WorkStartPoint := FStartPoint;
      end;

      if ComparePoints(WorkStartPoint, OldEndPoint) >= 1 then begin
        // Behind valid range
        DoFullSearch(False);
        exit;
      end;

      if ComparePoints(WorkStartPoint, OldStartPoint) < 1 then begin
        // Gap before valid range
        if OldStartPoint.y > LastLine+SEARCH_START_OFFS then begin
           // Delete all, except StartPoint: New search has smaller range than gap
          DoFullSearch(False);
          exit;
        end
        else begin
          // *** Fill gap at start
          Idx2 := Idx;
          FindMatches(WorkStartPoint, OldStartPoint, Idx);
          //WorkStartPoint := OldStartPoint;
          if (not SkipPaint) and (Idx > Idx2) then // TODO: avoid, if only 1 and 1 to hide
            MaybeSendLineInvalidation(Idx2, Idx-1);
          if (FirstKeptValidIdx >= 0) and (Idx > Idx2) then
            inc(FirstKeptValidIdx, Idx-Idx2);
        end;
      end;

    end;
  end;

  FSearchedEnd := OldEndPoint;

  // Search for the Gap
  if (GapStartPoint.y >= 0) then begin
    Assert((FirstKeptValidIdx >= 0) or (FMatches.Count = 0), 'FirstKeptValidIdx > 0');
    if FirstKeptValidIdx < 0 then
      FirstKeptValidIdx := 0;
    if (GapStartPoint.y > LastLine) and
       ((not HideSingleMatch) or  (FirstKeptValidIdx > 1))
    then begin
      // no need to search, done with visible area
      FMatches.Delete(FirstKeptValidIdx, FMatches.Count);
      FSearchedEnd := GapStartPoint;
      FinishValidate;
      exit;
    end;

    Idx := FirstKeptValidIdx;
    GapStartPoint := FindMatches(GapStartPoint, GapEndPoint, Idx, LastLine);

    if (ComparePoints(GapStartPoint, GapEndPoint) < 0) and
       HideSingleMatch and (FirstKeptValidIdx < 2)
    then
      GapStartPoint := FindMatches(GapStartPoint, GapEndPoint, Idx, LastLine);

    if (not SkipPaint) and (Idx > FirstKeptValidIdx) then // TODO: avoid, if only 1 and 1 to hide
      MaybeSendLineInvalidation(FirstKeptValidIdx, Idx-1);

    if (ComparePoints(GapStartPoint, GapEndPoint) < 0) and
       ((not HideSingleMatch) or (FirstKeptValidIdx > 1))
    then begin
      // searched stopped in gap
      assert(GapStartPoint.y >= LastLine, 'GapStartPoint.y >= LastLine');
      FSearchedEnd := GapStartPoint;
      FinishValidate;
      exit;
    end;
  end;

  // Check at end
  if (OldEndPoint.y <= LastLine) then begin
    EndOffsLine := min(LastLine+AdjustedSearchStrMaxLines, Lines.Count); // Search only for visible new matches
    Idx  := FMatches.Count;
    Idx2 := Idx;
    OldEndPoint.y := OldEndPoint.y - AdjustedSearchStrMaxLines;
    if (FMatches.Count > 0) and (ComparePoints(OldEndPoint, FMatches.EndPoint[Idx-1]) < 0) then
      OldEndPoint := FMatches.EndPoint[Idx-1];
    p := Point(Length(Lines[EndOffsLine - 1])+1, EndOffsLine);
    if ComparePoints(OldEndPoint, p) < 0 then begin
      FSearchedEnd := FindMatches(OldEndPoint, p, Idx, LastLine);
      if (not SkipPaint) and (Idx > Idx2) and HasVisibleMatch then
        MaybeSendLineInvalidation(Idx2, Idx-1);
    end;
  end;

  MaybeExtendForHideSingle;
  FinishValidate;
  //finally  DebugLnExit(['  < ValidateMatches Cnt=',FMatches.Count, '  <<< # ', dbgs(FStartPoint), ' - ', dbgs(FSearchedEnd)]); end;
end;

procedure TSynEditMarkupHighlightAllBase.SetLines(
  const AValue: TSynEditStringsLinked);
begin
  if Lines <> nil then
    Lines.RemoveChangeHandler(senrLineMappingChanged, @DoFoldChanged);
  inherited SetLines(AValue);
  if Lines <> nil then
    Lines.AddChangeHandler(senrLineMappingChanged, @DoFoldChanged);
end;

function TSynEditMarkupHighlightAllBase.HasDisplayAbleMatches: Boolean;
begin
  Result := (inherited HasDisplayAbleMatches) and
            HasSearchData and
            ( (not HideSingleMatch) or (Matches.Count > 1) );
end;

procedure TSynEditMarkupHighlightAllBase.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  if (not HasSearchData) then exit;
  if ACountDiff = 0 then
    InvalidateLines(StartLine, EndLine+1)
  else
    InvalidateLines(StartLine, MaxInt); // LineCount changed
end;

procedure TSynEditMarkupHighlightAllBase.DoVisibleChanged(AVisible: Boolean);
begin
  inherited DoVisibleChanged(AVisible);
  if FNeedValidate and SynEdit.IsVisible then
    ValidateMatches(True);
end;

function TSynEditMarkupHighlightAllBase.HasVisibleMatch: Boolean;
begin
  Result := ( HideSingleMatch and (FMatches.Count > 1) ) or
            ( (not HideSingleMatch) and (FMatches.Count > 0) );
end;

procedure TSynEditMarkupHighlightAllBase.InvalidateLines(AFirstLine: Integer;
  ALastLine: Integer; SkipPaint: Boolean);
begin
  if AFirstLine < 1 then
    AFirstLine := 1;
  if (ALastLine < 1) then
    ALastLine := MaxInt
  else
  if ALastLine < AFirstLine then
    ALastLine := AFirstLine;


  if ( (FStartPoint.y < 0) or (ALastLine >= FStartPoint.y) ) and
     ( (FSearchedEnd.y < 0) or (AFirstLine <= FSearchedEnd.y) )
  then begin
    if (AFirstLine < FFirstInvalidLine) or (FFirstInvalidLine <= 0) then
      FFirstInvalidLine := AFirstLine;
    if (ALastLine > FLastInvalidLine) then
      FLastInvalidLine := ALastLine;
  end;

  ValidateMatches(SkipPaint);
end;

procedure TSynEditMarkupHighlightAllBase.SendLineInvalidation(AFirstIndex: Integer;
  ALastIndex: Integer);
var
  Pos: Integer;
  Line1, Line2: Integer;
begin
  // Inform SynEdit which lines need repainting
  if fMatches.Count = 0 then
    exit;

  if AFirstIndex < 0 then
    AFirstIndex := 0;
  if (ALastIndex < 0) or (ALastIndex > FMatches.Count - 1) then
    ALastIndex := FMatches.Count - 1;

  Line1 := fMatches.StartPoint[AFirstIndex].y;
  Line2 := fMatches.EndPoint[AFirstIndex].y;
  Pos := AFirstIndex;
  while (Pos < ALastIndex)
  do begin
    inc(Pos);
    if fMatches.EndPoint[Pos].y <= Line2 then
      Continue;
    if fMatches.StartPoint[Pos].y <= Line2 + 1 then begin
      Line2 := fMatches.EndPoint[Pos].y;
      Continue;
    end;

    InvalidateSynLines(Line1, Line2);
    Line1 := fMatches.StartPoint[Pos].y;
    Line2 := fMatches.EndPoint[Pos].y;
  end;

  InvalidateSynLines(Line1, Line2);
end;

procedure TSynEditMarkupHighlightAllBase.Invalidate(SkipPaint: Boolean);
begin
  if not SkipPaint then
    SendLineInvalidation;
  FStartPoint.y := -1;
  FSearchedEnd.y := -1;
  FMatches.Count := 0;
  FFirstInvalidLine := 1;
  FLastInvalidLine := MaxInt;
  ValidateMatches(SkipPaint);
end;

{ TSynEditMarkupHighlightAllCaret }

procedure TSynEditMarkupHighlightAllCaret.SetWaitTime(const AValue: Integer);
begin
  if FWaitTime = AValue then exit;
  FWaitTime := AValue;
  FTimer.Interval := FWaitTime;
  if FWaitTime = 0 then
    SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.SearchStringChanged;
begin
  if SearchString = '' then
    FLowBound.X := -1;
  FOldLowBound := FLowBound;
  FOldUpBound := FUpBound;
end;

procedure TSynEditMarkupHighlightAllCaret.SetFullWord(const AValue: Boolean);
begin
  if FFullWord = AValue then exit;
  FFullWord := AValue;
  SearchOptions := GetCurrentOption;
end;

procedure TSynEditMarkupHighlightAllCaret.SetFullWordMaxLen(const AValue: Integer);
begin
  if FFullWordMaxLen = AValue then exit;
  FFullWordMaxLen := AValue;
  SearchOptions := GetCurrentOption;
end;

procedure TSynEditMarkupHighlightAllCaret.SetHighlighter(const AValue: TSynCustomHighlighter);
begin
  if FHighlighter = AValue then exit;
  FHighlighter := AValue;
  if FIgnoreKeywords and (SearchString <> '') then
    ScrollTimerHandler(self);
end;

procedure TSynEditMarkupHighlightAllCaret.SetIgnoreKeywords(const AValue: Boolean);
begin
  if FIgnoreKeywords = AValue then exit;
  FIgnoreKeywords := AValue;
  if Assigned(FHighlighter) and (SearchString <> '') then
    ScrollTimerHandler(self);
end;

procedure TSynEditMarkupHighlightAllCaret.SetSelection(const AValue: TSynEditSelection);
begin
  if Assigned(FSelection) then
    FSelection.RemoveChangeHandler(@SelectionChanged);
  FSelection := AValue;
  if Assigned(FSelection) then
    FSelection.AddChangeHandler(@SelectionChanged);
end;

procedure TSynEditMarkupHighlightAllCaret.SetTrim(const AValue: Boolean);
begin
  if FTrim = AValue then exit;
  FTrim := AValue;
  if (SearchString <> '') then
    ScrollTimerHandler(self)
  else
    RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.CheckState;
var
  t: String;
begin
  if (not FStateChanged) or (Caret = nil) then
    exit;
  FStateChanged := False;

  t := GetCurrentText;
  if (SearchString = t) and (SearchOptions = GetCurrentOption) then begin
    SearchString := t; // Update old bounds
    exit;
  end;

  if (SearchString <> '') and
     ( ((CompareCarets(FLowBound, FOldLowBound) = 0) and
       (CompareCarets(Caret.LineBytePos, FUpBound) >= 0) and (MatchCount > 1) )
      OR ((CompareCarets(FUpBound, FOldUpBound) = 0) and
       (CompareCarets(Caret.LineBytePos, FLowBound) <= 0) and (MatchCount > 1) )
     )
  then begin
    ScrollTimerHandler(self);
    exit;
  end;

  SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.SelectionChanged(Sender: TObject);
begin
  FStateChanged := True; // Something changed, paint will be called
  inherited;
end;

procedure TSynEditMarkupHighlightAllCaret.DoCaretChanged(Sender: TObject);
begin
  FStateChanged := True; // Something changed, paint will be called
  inherited;
end;

procedure TSynEditMarkupHighlightAllCaret.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  FStateChanged := True; // Something changed, paint will be called
  inherited;
end;

procedure TSynEditMarkupHighlightAllCaret.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  IncPaintLock;
  try
    inherited DoMarkupChanged(AMarkup);
    SearchString := '';
    RestartTimer;
  finally
    DecPaintLock;
  end;
end;

procedure TSynEditMarkupHighlightAllCaret.RestartTimer;
begin
  FTimer.Enabled := False;
  if not SynEdit.HandleAllocated then begin
    FWaitForHandle := True;  // HandleCreation will call paintlock, check there
    exit;
  end;
  if (MarkupInfo.IsEnabled) and (FWaitTime > 0) then
    FTimer.Enabled := True;
end;

procedure TSynEditMarkupHighlightAllCaret.ScrollTimerHandler(Sender: TObject);
begin
  FTimer.Enabled := False;
  if not SynEdit.HandleAllocated then begin
    FWaitForHandle := True;  // HandleCreation will call paintlock, check there
    exit;
  end;
  FWaitForHandle := False;
  if (SearchString = GetCurrentText) and (SearchOptions = GetCurrentOption) then
    exit;
  SearchString := ''; // prevent double update
  SearchOptions := GetCurrentOption;
  SearchString := GetCurrentText;
end;

function TSynEditMarkupHighlightAllCaret.GetCurrentText: String;
  function TrimS(s: String): String;
  var
    i: Integer;
  begin
    i := 1;
    while (i <= length(s)) and (s[i] in [#1..#32]) do inc(i);
    Result := copy(s, i, MaxInt);
    i := length(Result);
    while (i > 0) and (Result[i] in [#1..#32]) do dec(i);
    Result := copy(Result, 1, i);
  end;
var
  LowBnd, UpBnd: TPoint;
  i: integer;
begin
  if Caret = nil then
    exit('');
  if FToggledWord <> '' then
    exit(FToggledWord);
  If TCustomSynEdit(SynEdit).SelAvail then begin
    LowBnd := TCustomSynEdit(SynEdit).BlockBegin;
    UpBnd := TCustomSynEdit(SynEdit).BlockEnd;
    i := UpBnd.y - LowBnd.y + 1;
    if (i > LowBnd.y) and (i > Lines.Count - UpBnd.y) then
      exit('');
    if FTrim then
      Result := TrimS(TCustomSynEdit(SynEdit).SelText)
    else
      Result := TCustomSynEdit(SynEdit).SelText;
    if TrimS(Result) = '' then Result := '';
    FLowBound := LowBnd;
    FUpBound := UpBnd;
  end else begin
    Result :=  TCustomSynEdit(SynEdit).GetWordAtRowCol(Caret.LineBytePos);
    if FIgnoreKeywords and assigned(FHighlighter)
       and FHighlighter.IsKeyword(Result) then
      Result := '';
    FLowBound.Y := Caret.LinePos;
    FUpBound.Y := Caret.LinePos;
    TCustomSynEdit(SynEdit).GetWordBoundsAtRowCol(Caret.LineBytePos, FLowBound.X, FUpBound.X);
  end;
end;

procedure TSynEditMarkupHighlightAllCaret.DoOptionsChanged;
begin
  if ssoMatchCase in SearchOptions then
    FToggledOption:=FToggledOption + [ssoMatchCase]
    else
    FToggledOption:=FToggledOption - [ssoMatchCase];
end;

function TSynEditMarkupHighlightAllCaret.GetCurrentOption: TSynSearchOptions;
begin
  if FToggledWord <> '' then
    exit(FToggledOption);
  If TCustomSynEdit(SynEdit).SelAvail or not(FFullWord) then
    Result := []
  else
    if (FFullWordMaxLen >0) and (UTF8Length(GetCurrentText) > FFullWordMaxLen) then
      Result := []
    else
      Result := [ssoWholeWord];
  if ssoMatchCase in SearchOptions then
    Result := Result + [ssoMatchCase];
end;

constructor TSynEditMarkupHighlightAllCaret.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  FWaitForHandle := False;
  FStateChanged := False;
  FValidateNeeded := False;
  HideSingleMatch := True;
  FFullWord := False;
  FWaitTime := 1500;
  FTrim := True;
  FLowBound := Point(-1, -1);
  FUpBound := Point(-1, -1);
  FOldLowBound := Point(-1, -1);
  FOldUpBound := Point(-1, -1);
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := FWaitTime;
  FTimer.OnTimer := @ScrollTimerHandler;
  MarkupInfo.Clear; // calls RestartTimer
end;

destructor TSynEditMarkupHighlightAllCaret.Destroy;
begin
  if Assigned(FSelection) then
    FSelection.RemoveChangeHandler(@SelectionChanged);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TSynEditMarkupHighlightAllCaret.DecPaintLock;
begin
  inherited DecPaintLock;
  if FWaitForHandle and SynEdit.HandleAllocated then
    ScrollTimerHandler(Self);
end;

procedure TSynEditMarkupHighlightAllCaret.ToggleCurrentWord;
var
  s: String;
begin
  if FToggledWord = '' then begin
    FToggledWord := GetCurrentText;
    FToggledOption :=  GetCurrentOption;
  end else begin
    s := FToggledWord;
    FToggledWord := '';
    if GetCurrentText <> s then begin
      FToggledWord := GetCurrentText;
      FToggledOption :=  GetCurrentOption;
    end;
  end;
  SearchString := FToggledWord;
  SearchOptions := GetCurrentOption;
  if FToggledWord = '' then begin
    RestartTimer;
  end else begin
    ScrollTimerHandler(self);
  end;
end;

end.


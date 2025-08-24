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
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Controls, ExtCtrls, Forms,
  // LazUtils
  LazClasses, LazUTF8, LazMethodList, LazLoggerBase,
  // LazEdit
  LazEditMiscProcs, LazSynEditText, LazEditTextAttributes,
  // SynEdit
  SynEditMarkup, SynEditTypes, SynEditSearch, SynEditMiscClasses,
  SynEditHighlighter, SynEditPointClasses, SynEditMiscProcs,
  SynEditTextBase;

type

  TSynEditMarkupHighlightMatches = class;

  { TSynMarkupHighAllMatch }
  TSynMarkupHighAllMatch = Record
    StartPoint, EndPoint : TPoint;
  end;
  PSynMarkupHighAllMatch = ^TSynMarkupHighAllMatch;

  { TSynEditGapStorageMem }

  TSynEditGapStorageMem = class(TSynEditStorageMem)
  strict private
    FGapStartIndex, FGapEndIndex: integer; // FGapEndIndex is first AFTER gap
    FIgnoreGapLock: integer;
    function GetCount: Integer;
    function GetItemPointer(Index: Integer): Pointer; reintroduce; inline;
    function GetItemPointerRaw(Index: Integer): Pointer;
  protected
    procedure SetCount(const AValue: Integer); override;
    procedure Move(AFrom, ATo, ALen: Integer); override;
    procedure MoveRaw(AFrom, ATo, ALen: Integer); inline; // handle gap as data
    procedure InitMem(var AFirstItem; AByteCount: Integer); override;
    function CountRaw: integer; inline;

    property ItemPointer[Index: Integer]: Pointer read GetItemPointer;
    property ItemPointerRaw[Index: Integer]: Pointer read GetItemPointerRaw;
    property GapStartIndex: integer read FGapStartIndex write FGapStartIndex;
    property GapEndIndex: integer read FGapEndIndex write FGapEndIndex;
  public
    constructor Create;
    destructor Destroy; override;
    // TODO: insert/delete: move into/from gap
    procedure InsertRows(AIndex, ACount: Integer); override;
    procedure DeleteRows(AIndex, ACount: Integer); override;
    //property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
  end;

  { TSynMarkupHighAllMatchList }

  TSynMarkupHighAllMatchList = class(TSynEditGapStorageMem)
  strict private
    FChangeStamp: int64;
    FOwner: TSynEditMarkupHighlightMatches;

    function GetEndPoint(const AnIndex : Integer) : TPoint;
    function GetPoint(const AnIndex : Integer) : TPoint;
    function GetPointCount : Integer;
    function GetStartPoint(const AnIndex : Integer) : TPoint;
    function  GetMatch(const AnIndex : Integer) : TSynMarkupHighAllMatch;
    procedure SetEndPoint(const AnIndex : Integer; const AValue : TPoint);
    procedure SetMatch(const AnIndex : Integer; const AValue : TSynMarkupHighAllMatch);
    procedure SetStartPoint(const AnIndex : Integer; const AValue : TPoint);
  protected
    function  GetInintialForItemSize: Integer; override;
    procedure SetCount(const AValue : Integer); override;

    function  StartValidation(AnFirstInvalidMatchLine, AnLastInvalidMatchLine, AnMatchLinesDiffCount: Integer; // create gap
                              out AFirstInvalidPoint, ALastInvalidPoint: TPoint): integer;
    procedure EndValidation; // limit gap size
    property Owner: TSynEditMarkupHighlightMatches read FOwner;
  public
    constructor Create(AnOwner: TSynEditMarkupHighlightMatches = nil);
    Function MaybeReduceCapacity : Boolean;

    (* IndexOfFirstMatchForLine
       AnIndex for the first Item (match) that ENDS AT/AFTER the ALine
       That is either it - includes ALine
                         - is after ALine (aka: is in the RANGE ALine..infinity)
       RESULT = 0..Count
       // Count => for not found (all items are before ALine)
     * IndexOfLastMatchForLine
       AnIndex for the last item that is STARTS BEFORE/At the ALine
       That is either it - includes ALine
                         - is before ALine (aka: is in the RANGE 0..ALine)
       RESULT = -1..Count-1
       // -1 => for not found (all items are after ALine)
    *)
    function IndexOfFirstMatchForLine(ALine: Integer): Integer;
    function IndexOfLastMatchForLine(ALine: Integer): Integer;
    (* IndexOf 0..count
       - if in GAP, or at End-Of-Match: Next match after the GAP for APoint
       - if in Match (excluding end): Index of that match
    *)
    function IndexOf(APoint: TPoint): Integer;
    procedure Clear;
    procedure SendInvalidateAll;
    procedure Delete(AnIndex: Integer; ACount: Integer = 1);
    procedure Insert(AnIndex: Integer; ACount: Integer = 1);
    procedure Insert(AnIndex: Integer; AStartPoint, AnEndPoint: TPoint);
    function Insert(AStartPoint, AnEndPoint: TPoint): integer;
    procedure SetMatchPoints(const AnIndex : Integer; AStartPoint, AnEndPoint: TPoint);
    property Match [const AnIndex : Integer] : TSynMarkupHighAllMatch read GetMatch write SetMatch; default;
    property StartPoint [const AnIndex : Integer] : TPoint read GetStartPoint write SetStartPoint;
    property EndPoint   [const AnIndex : Integer] : TPoint read GetEndPoint write SetEndPoint;
    property PointCount : Integer read GetPointCount;
    property Point      [const AnIndex : Integer] : TPoint read GetPoint;
    property ChangeStamp: int64 read FChangeStamp;
  end;

  { TSynMarkupHighAllMultiMatchList - Allow matches with different markup / no overlap }

  TSynMarkupHighAllMultiMatchList = class(TSynMarkupHighAllMatchList)
  private
    FParentItemSize: Integer;
    function GetMarkupId(AnIndex: Integer): Integer;
    procedure SetMarkupId(AnIndex: Integer; AValue: Integer);
  protected
    function GetInintialForItemSize: Integer; override;
  public
    property MarkupId[AnIndex: Integer]: Integer read GetMarkupId write SetMarkupId;
  end;

  { TSynMarkupHighAllValidRange }
  TSynMarkupHighAllValidRange = Record
    StartPoint, EndPoint : TPoint;
  end;

  TSynMarkupHighAllValidRanges = array of TSynMarkupHighAllValidRange;

  { TSynMarkupHighAllValidRangesHelper }

  TSynMarkupHighAllValidRangesHelper = type helper for TSynMarkupHighAllValidRanges
  private
    function GetGap(AnIndex: Integer): TSynMarkupHighAllValidRange;
  public
    procedure Delete(AnIndex, ACnt: integer);
    procedure Insert(AnIndex, ACnt: integer);
    procedure RemoveBefore(APoint: TPoint);
    procedure RemoveAfter(APoint: TPoint);
    procedure RemoveBetween(AStartPoint, AnEndPoint: TPoint);
    procedure AdjustForLinesChanged(AStartLine, ALineDiff: integer);
    procedure MarkValid(AStartPoint, AnEndPoint: TPoint);
    function  FindGapFor(APoint: TPoint; AReturnNext: boolean = False): Integer;
    function Count: integer;
    (* Gap
       0: before first block
       1..count-1: between blocks
       count: after last block
    *)
    property Gap[AnIndex: Integer]: TSynMarkupHighAllValidRange read GetGap;
  end;

  { TSynMarkupHighAllPointHelper }

  TSynMarkupHighAllPointHelper = type helper for TPoint
    function HasData: boolean;
  end;

  { TSynEditMarkupHighlightMatches }

  TSynEditMarkupHighlightMatches = class(TSynEditMarkup)
  private
    FMatches : TSynMarkupHighAllMatchList;
    FCurrentRowNextPosIdx, FCurrentRow: Integer;
  strict private
    FSendingInvalidationLock, FSkipSendingInvalidationLock: Integer;
    FPendingInvalidationStartLine, FPendingInvalidationEndLine: integer;
  protected
    procedure BeginSkipSendingInvalidation; inline;
    procedure EndSkipSendingInvalidation; inline;
    property  SkipSendingInvalidationLock: Integer read FSkipSendingInvalidationLock;
    procedure BeginSendingInvalidation; virtual;
    procedure EndSendingInvalidation; virtual;
    property  SendingInvalidationLock: Integer read FSendingInvalidationLock;
    procedure SendMatchLineInvalidation(AStartIndex, AnEndIndex: Integer); virtual;
    function  HasDisplayAbleMatches: Boolean; virtual;
    function  CreateMatchList: TSynMarkupHighAllMatchList; virtual;
    function  MarkupIdForMatch(Idx: Integer): Integer; virtual;
    function  MarkupInfoForId(Idx: Integer): TSynHighlighterAttributesModifier; virtual;
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
                                         const AnRtlInfo: TLazSynDisplayRtlInfo): TLazEditTextAttributeModifier; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;
  end;


  { TSynEditMarkupHighlightMultiMatches }

  TSynEditMarkupHighlightMultiMatches = class(TSynEditMarkupHighlightMatches)
  private
    FMarkupInfos: array of TSynHighlighterAttributesModifier;
    function GetMarkupInfoCount: integer;
    function GetMarkupInfos(AnIndex: Integer): TSynHighlighterAttributesModifier;
    function GetMatches: TSynMarkupHighAllMultiMatchList;
    procedure SetMarkupInfoCount(AValue: integer);
  protected
    function  CreateMatchList: TSynMarkupHighAllMatchList; override;
    function  MarkupIdForMatch(Idx: Integer): Integer; override;
    function  MarkupInfoForId(Idx: Integer): TSynHighlighterAttributesModifier; override;

    property  Matches: TSynMarkupHighAllMultiMatchList read GetMatches;
  public
    destructor Destroy; override;
    property MarkupInfoCount: integer read GetMarkupInfoCount write SetMarkupInfoCount;
    property MarkupInfos[AnIndex: Integer]: TSynHighlighterAttributesModifier read GetMarkupInfos;
  end;


  TSynEditMarkupHighlightAllScanMode = (
    (* All "Direct" modes
       - Scan visilble screen area immediately (non-async)
       - If "HideSingle" needs a 2nd match:
         Search immediately (non async) withing "ScanOffScreenLimit" lines before & after the visible screen area
     * "ForceAll" modes
       - Always scan all lines (e.g. for display in overview)
       - Ignores "ScanOffScreenLimit" (i.e does/may not search those first)
     * Any mode
       - Already found nodes may be kept, even outside the given limits (until they become invalide, or exceed "keep limits"
    *)
    smsmDirect,            //
    smsmDirectForceAll,    // Scan always ALL lines ("ScanOffScreenLimit" has no effect)

    smsmDirectExtendASync, // If "HideSingle" needs a 2nd match:
                           // Search "ScanOffScreenLimit" Immediately (non async)
                           // Continue ASYNC outside "ScanOffScreenLimit" until ONE is found.
    smsmDirectForceAllASync, // If "HideSingle" needs a 2nd match:
                           // Search "ScanOffScreenLimit" Immediately (non async)
                           // ALWAYS search ALL remaining lines (async)

    smsmASync,             // Scan visible area (non async)
                           // If "HideSingle" needs a 2nd match:
                           // ASync search "ScanOffScreenLimit" until one is found.
    smsmASyncForceAll      // Scan visible area (non async)
                           // Then scan all lines ASync ("ScanOffScreenLimit" has no effect)
  );

  { TSynEditMarkupHighlightAllBase }

  TSynEditMarkupHighlightAllBase = class(TSynEditMarkupHighlightMatches)
  public const
    DEFAULT_SCAN_OFFSCREEN_LIMIT = 200;
  strict private
    FFirstInvalidMatchLine, FLastInvalidMatchLine, FMatchLinesDiffCount: Integer;
  private
    FNeedValidate, FNeedValidatePaint: Boolean;
    FMarkupEnabled: Boolean;
    FScanMode: TSynEditMarkupHighlightAllScanMode;
    FScanOffScreenLimit: integer;

    FValidRanges: TSynMarkupHighAllValidRanges;

    FHideSingleMatch: Boolean;
    FInvalidationStartedWithHiddenMatch,
    FInvalidationStartedWithManyatches: boolean;
    FFlags: set of (smfNeedAsync, smfAsyncSkipPaint);

    function GetMatchCount: Integer;
    procedure SetHideSingleMatch(AValue: Boolean);
    procedure DoFoldChanged(Sender: TSynEditStrings; AnIndex, aCount: Integer);
    procedure SetScanOffScreenLimit(AValue: integer);

    Procedure InvalidateMatches(AFirstLine, ALastLine, ALineDiffCount: Integer);
    procedure AssertGapsValid;
    procedure DoAsyncScan(Data: PtrInt);
    function ValidateFillGaps: boolean;
    Procedure ValidateMatches(SkipPaint: Boolean = False);

  protected
    procedure BeginSendingInvalidation; override;
    procedure EndSendingInvalidation; override;
    procedure SendMatchLineInvalidation(AStartIndex, AnEndIndex: Integer); override;
    procedure SetLines(const AValue: TSynEditStringsLinked); override;
    function  HasSearchData: Boolean; virtual; abstract;
    function HasDisplayAbleMatches: Boolean; override;
    function  SearchStringMaxLines: Integer; virtual; abstract;
    procedure FindInitialize;  virtual; abstract;
    function  FindMatches(AStartPoint, AnEndPoint: TPoint;
                          var AnIndex: Integer;
                          AStopAfterLine: Integer = -1; // AnEndPoint may be set further down, for multi-line matches
                          ABackward : Boolean = False
                         ): TPoint;  virtual; abstract; // returns searched until point


    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoMarkupChanged(AMarkup: TLazEditTextAttribute); override;
    procedure DoEnabledChanged(Sender: TObject); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override; // 1 based
    procedure DoVisibleChanged(AVisible: Boolean); override;
    function  HasVisibleMatch: Boolean; deprecated 'use HasDisplayAbleMatches / to be removed in 5.99';
    property  MatchCount: Integer read GetMatchCount;
    property  MarkupEnabled: Boolean read FMarkupEnabled;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure DecPaintLock; override;

    // AFirst/ ALast are 1 based
    Procedure Invalidate(SkipPaint: Boolean = False);
    Procedure SendLineInvalidation(AFirstIndex: Integer = -1;ALastIndex: Integer = -1); //deprecated 'use invalidation via FMatches';

    property HideSingleMatch: Boolean read FHideSingleMatch write SetHideSingleMatch;

    property ScanMode: TSynEditMarkupHighlightAllScanMode read FScanMode write FScanMode default smsmDirect;
    (* ScanOffScreenLimit: Search for 2nd match, if HideSingleMatch is True
        1..MaxInt: Line-limit before and after visible area
        0: Don't search
       -1: Search entire text
       -2 and below: RESERVED
    *)
    property ScanOffScreenLimit: integer read FScanOffScreenLimit write SetScanOffScreenLimit default DEFAULT_SCAN_OFFSCREEN_LIMIT;
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
    function  FindMatches(AStartPoint, AnEndPoint: TPoint;
                          var AnIndex: Integer;
                          AStopAfterLine: Integer = -1; // AnEndPoint may be set further down, for multi-line matches
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
    function GetTerms(AnIndex: Integer): String;
    procedure SetTerms(AnIndex: Integer; AValue: String);
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
    procedure Delete(AnIndex: Integer);
    function  Count: Integer;
    property  Terms[AnIndex: Integer]: String read GetTerms write SetTerms;

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
    function GetItem(AnIndex: Integer): TSynSearchTerm;
    procedure SetItem(AnIndex: Integer; AValue: TSynSearchTerm);
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
    property Items[AnIndex: Integer]: TSynSearchTerm read GetItem write SetItem; default;
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
    function GetItem(AnIndex: Integer): TSynSearchTerm;
    procedure SetItem(AnIndex: Integer; AValue: TSynSearchTerm);
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
    procedure Delete(AnIndex: Integer);
    function  IndexOfSearchTerm(ATerm: String): Integer;
    function  IndexOfSearchTerm(ATerm: TSynSearchTerm): Integer;

    procedure  Search(AText: PChar; ATextLen: Integer; AFoundEvent: TSynSearchDictFoundEvent);
    function   GetIndexForNextWordOccurrence(AnIndex: Integer): Integer;

    property Items[AnIndex: Integer]: TSynSearchTerm read GetItem write SetItem; default;
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
    function  FindMatches(AStartPoint, AnEndPoint: TPoint;
                          var AnIndex: Integer;
                          AStopAfterLine: Integer = -1; // AnEndPoint may be set further down, for multi-line matches
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
    procedure DeleteSearchTerm(AnIndex: Integer);

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
    procedure DoMarkupChanged(AMarkup: TLazEditTextAttribute); override;
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

const
  DEFAULT_MULTILINE_SEARCH_OFFS =  100; // Search n lises before/after visible area. (Applies only, if "SearchStringMaxLines" return negative)
  AVOID_GAP_MAX_OFFS            =   50; // Don't create/levae a gap smaller than this before/after visible area.
  MATCHES_CLEAN_CNT_THRESHOLD   = 2500; // Remove matches out of range, only if more Matches are present
  MATCHES_CLEAN_LINE_THRESHOLD  =  300; // Remove matches out of range, only if they are at least n lines from visible area.
  MATCHES_CLEAN_LINE_KEEP       = 200; // LinesKept, if cleaning. MUST be LESS than MATCHES_CLEAN_LINE_THRESHOLD

function dbgs(const VR: TSynMarkupHighAllValidRange): String; overload;
begin
  Result := dbgs(VR.StartPoint) + '..' + dbgs(VR.EndPoint);
end;

function dbgs(const VR: TSynMarkupHighAllValidRanges): String; overload;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to VR.Count - 2 do
    Result := Result + dbgs(VR[i]) + ', ';
  i := VR.Count - 1;
  if i >= 0 then
    Result := Result + dbgs(VR[i]);
end;

{ TSynMarkupHighAllValidRangesHelper }

function TSynMarkupHighAllValidRangesHelper.GetGap(AnIndex: Integer): TSynMarkupHighAllValidRange;
begin
  Result.StartPoint := Point(1, 1);
  Result.EndPoint   := Point(1, MaxInt);
  if Count = 0 then
    exit;

  if (AnIndex < Count) then
    Result.EndPoint := Self[AnIndex].StartPoint;
  if (AnIndex > 0) then
    Result.StartPoint := Self[AnIndex-1].EndPoint;

  if (Result.StartPoint >= Result.EndPoint) then begin
    Result.StartPoint := Point(-1, -1);
    Result.EndPoint   := Point(-1, -1);
  end;
end;

procedure TSynMarkupHighAllValidRangesHelper.Delete(AnIndex, ACnt: integer);
var
  i: Integer;
  c: SizeInt;
begin
  i := AnIndex + ACnt;
  c := Length(Self) - i;
  if c > 0 then
    move(Self[i], Self[AnIndex], SizeOf(Self[0]) * c);
  SetLength(Self, Length(Self) - ACnt);
end;

procedure TSynMarkupHighAllValidRangesHelper.Insert(AnIndex, ACnt: integer);
var
  i: Integer;
  c: SizeInt;
begin
  i := AnIndex + ACnt;
  SetLength(Self, Length(Self) + ACnt);
  c := Length(Self) - i;
  if c > 0 then
    move(Self[AnIndex], Self[i], SizeOf(Self[0]) * c);
end;

procedure TSynMarkupHighAllValidRangesHelper.RemoveBefore(APoint: TPoint);
var
  i: Integer;
begin
  i := 0;
  while (i < Count) and (Self[i].EndPoint <= APoint) do
    inc(i);
  if i > 0 then
    Delete(0, i);
  if (Count > 0) and (Self[0].StartPoint < APoint) then
    Self[0].StartPoint := APoint;
end;

procedure TSynMarkupHighAllValidRangesHelper.RemoveAfter(APoint: TPoint);
var
  i: Integer;
begin
  i := Count - 1;
  while (i >= 0) and (Self[i].StartPoint >= APoint) do
    dec(i);
  inc(i);
  if i < Count then
    Delete(i, Count - i);
  dec(i);
  if (i >= 0) and (Self[i].EndPoint > APoint) then
    Self[i].EndPoint := APoint;
end;

procedure TSynMarkupHighAllValidRangesHelper.RemoveBetween(AStartPoint, AnEndPoint: TPoint);
var
  i, j: Integer;
  p: TPoint;
begin
  i := 0;
  while (i < Count) and (Self[i].EndPoint <= AStartPoint) do
    inc(i);
  if i = Count then
    exit;

  if (Self[i].StartPoint < AStartPoint) then begin
    p := Self[i].EndPoint;
    Self[i].EndPoint := AStartPoint;
    inc(i);

    if (p > AnEndPoint) then begin
      Insert(i, 1);
      Self[i].StartPoint := AnEndPoint;
      Self[i].EndPoint   := p;
      exit;
    end;
  end;

  j := i;
  while (j < Count) and (Self[j].EndPoint <= AnEndPoint) do
    inc(j);

  if j > i then
    Delete(i, j-i);

  if (i < Count) and (Self[i].StartPoint < AnEndPoint) then
    Self[i].StartPoint := AnEndPoint;
end;

procedure TSynMarkupHighAllValidRangesHelper.AdjustForLinesChanged(AStartLine, ALineDiff: integer);
var
  i: Integer;
begin
  i := 0;
  while (i < Count) and (Self[i].EndPoint.y < AStartLine) do
    inc(i);
  if i = Count then
    exit;

  if (Self[i].StartPoint.y < AStartLine) then begin
    Self[i].EndPoint.Y := Self[i].EndPoint.Y + ALineDiff;
    if Self[i].EndPoint.Y < AStartLine then
      Self[i].EndPoint := Point(1, AStartLine);

    if Self[i].EndPoint.Y <= Self[i].StartPoint.Y then
      Delete(i, 1)
    else
      inc(i);
  end;

  while (i < Count) do begin
    Self[i].StartPoint.Y := Self[i].StartPoint.Y + ALineDiff;
    if Self[i].StartPoint.Y < AStartLine then
      Self[i].StartPoint := Point(1, AStartLine);
    Self[i].EndPoint.Y   := Self[i].EndPoint.Y   + ALineDiff;

    if Self[i].EndPoint.Y <= Self[i].StartPoint.Y then
      Delete(i, 1)
    else
      inc(i);
  end;
end;

procedure TSynMarkupHighAllValidRangesHelper.MarkValid(AStartPoint, AnEndPoint: TPoint);
var
  i, j: Integer;
begin
  i := 0;
  while (i < Count) and (Self[i].EndPoint < AStartPoint) do
    inc(i);

  if (i = Count) or (Self[i].StartPoint > AnEndPoint)
  then begin
    Insert(i, 1);
    Self[i].StartPoint := AStartPoint;
    Self[i].EndPoint   := AnEndPoint;
    exit;
  end;

  if (Self[i].StartPoint > AStartPoint) then
    Self[i].StartPoint := AStartPoint;
  if Self[i].EndPoint >= AnEndPoint then
    exit;
  Self[i].EndPoint := AnEndPoint;
  inc(i);

  j := i;
  while (j < Count) and (Self[j].StartPoint <= AnEndPoint) do
    inc(j);
  if j > i then begin
    if (Self[j-1].EndPoint > AnEndPoint) then
      Self[i-1].EndPoint := Self[j-1].EndPoint;

    Delete(i, j-i);
  end;
end;

function TSynMarkupHighAllValidRangesHelper.FindGapFor(APoint: TPoint; AReturnNext: boolean
  ): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Self[Result].StartPoint > APoint) do
    dec(Result);

  if (not AReturnNext) and (Result >= 0) and (Self[Result].EndPoint > APoint) then
    exit(-1);

  inc(Result);
end;

function TSynMarkupHighAllValidRangesHelper.Count: integer;
begin
  Result := Length(Self);
end;

{ TSynMarkupHighAllPointHelper }

function TSynMarkupHighAllPointHelper.HasData: boolean;
begin
  Result := Self.Y >= 0;
end;

{ TSynEditGapStorageMem }

function TSynEditGapStorageMem.GetCount: Integer;
begin
  if FIgnoreGapLock > 0 then
    Result := inherited Count
  else
    Result := inherited Count - (FGapEndIndex - FGapStartIndex);
end;

function TSynEditGapStorageMem.GetItemPointer(Index: Integer): Pointer;
begin
  assert(FIgnoreGapLock = 0, 'TSynEditGapStorageMem.GetItemPointer: FIgnoreGapLock = 0');
  if Index >= FGapStartIndex then
    Index := Index + (FGapEndIndex - FGapStartIndex);
  Result := inherited ItemPointer[Index];
end;

function TSynEditGapStorageMem.GetItemPointerRaw(Index: Integer): Pointer;
begin
  assert(FIgnoreGapLock = 0, 'TSynEditGapStorageMem.GetItemPointerRaw: FIgnoreGapLock = 0');
  Result := inherited ItemPointer[Index];
end;

procedure TSynEditGapStorageMem.SetCount(const AValue: Integer);
begin
  if FIgnoreGapLock > 0 then
    inherited SetCount(AValue)
  else
    inherited SetCount(AValue + (FGapEndIndex - FGapStartIndex));
end;

procedure TSynEditGapStorageMem.Move(AFrom, ATo, ALen: Integer);
  procedure SkipGap(var i: integer); inline;
  begin
    if i >= FGapStartIndex then
      i := i + (FGapEndIndex - FGapStartIndex);
  end;
  procedure SkipAtGapOnly(var i: integer); inline;
  begin
    if i = FGapStartIndex then
      i := i + (FGapEndIndex - FGapStartIndex);
  end;
  procedure UnSkipAtGapOnly(var i: integer); inline;
  begin
    if i = FGapEndIndex then
      i := i - (FGapEndIndex - FGapStartIndex);
  end;

  procedure MoveSectionUp(var f, t, l1, l2: integer); inline;
  var
    l: Integer;
  begin
    l := Min(l1, l2);
    if l > 0 then begin
      inherited Move(f, t, l);
      inc(f, l);    SkipAtGapOnly(f);
      inc(t, l);    SkipAtGapOnly(t);
      dec(l1, l);
      dec(l2, l);
    end;
  end;

  procedure MoveSectionDown(var f, t, l1, l2: integer); inline;
  var
    l: Integer;
  begin
    l := Min(l1, l2);
    if l > 0 then begin
      dec(f, l);
      dec(t, l);
      inherited Move(f, t, l);
      UnSkipAtGapOnly(f);
      UnSkipAtGapOnly(t);
      dec(l1, l);
      dec(l2, l);
    end;
  end;
var
  SrcLen1, SrcLen2, DstLen1, DstLen2: Integer;
begin
  if FIgnoreGapLock > 0 then begin
    inherited Move(AFrom, ATo, ALen);
    exit;
  end;

  SrcLen1 := ALen;
  SrcLen2 := 0;
  DstLen1 := ALen;
  DstLen2 := 0;

  SkipGap(AFrom);
  SkipGap(ATo);

  if (AFrom < FGapStartIndex) and (AFrom + ALen > FGapStartIndex) then begin
    SrcLen1 := FGapStartIndex - AFrom;
    SrcLen2 := ALen - SrcLen1;
  end;
  if (ATo < FGapStartIndex) and (ATo + ALen > FGapStartIndex) then begin
    DstLen1 := FGapStartIndex - ATo;
    DstLen2 := ALen - DstLen1;
  end;

  if AFrom > ATo then begin
    // move down
    MoveSectionUp(AFrom, ATo, SrcLen1, DstLen1);
    MoveSectionUp(AFrom, ATo, SrcLen1, DstLen1);
    MoveSectionUp(AFrom, ATo, SrcLen2, DstLen2);
    MoveSectionUp(AFrom, ATo, SrcLen2, DstLen2);
  end
  else begin
    // move up / start at end
    Inc(AFrom, ALen);
    inc(ATo, ALen);
    MoveSectionDown(AFrom, ATo, SrcLen2, DstLen2);
    MoveSectionDown(AFrom, ATo, SrcLen2, DstLen2);
    MoveSectionDown(AFrom, ATo, SrcLen1, DstLen1);
    MoveSectionDown(AFrom, ATo, SrcLen1, DstLen1);
  end;
end;

procedure TSynEditGapStorageMem.MoveRaw(AFrom, ATo, ALen: Integer);
begin
  assert(FIgnoreGapLock = 0, 'TSynEditGapStorageMem.MoveRaw: FIgnoreGapLock = 0');
  inherited Move(AFrom, ATo, ALen);
end;

procedure TSynEditGapStorageMem.InitMem(var AFirstItem; AByteCount: Integer);
begin
  FillChar(AFirstItem, AByteCount, 255);
end;

function TSynEditGapStorageMem.CountRaw: integer;
begin
  assert(FIgnoreGapLock = 0, 'TSynEditGapStorageMem.CountRaw: FIgnoreGapLock = 0');
  Result := inherited Count;
end;

constructor TSynEditGapStorageMem.Create;
begin
  FGapStartIndex := -1;
  FGapEndIndex := -1;
  inherited Create;
end;

destructor TSynEditGapStorageMem.Destroy;
begin
  FGapStartIndex := -1;
  FGapEndIndex   := -1;
  inherited Destroy;
end;

procedure TSynEditGapStorageMem.InsertRows(AIndex, ACount: Integer);
begin
  assert(FIgnoreGapLock = 0, 'TSynEditGapStorageMem.InsertRows: FIgnoreGapLock = 0');
  if AIndex >= FGapStartIndex then begin
    AIndex := AIndex + (FGapEndIndex - FGapStartIndex);
  end
  else begin
    // move gap
    FGapStartIndex := FGapStartIndex + ACount;
    FGapEndIndex   := FGapEndIndex   + ACount;
  end;

  inc(FIgnoreGapLock);
  inherited InsertRows(AIndex, ACount);
  dec(FIgnoreGapLock);
end;

procedure TSynEditGapStorageMem.DeleteRows(AIndex, ACount: Integer);
var
  l: Integer;
begin
  assert(FIgnoreGapLock = 0, 'TSynEditGapStorageMem.DeleteRows: FIgnoreGapLock = 0');
  if AIndex >= FGapStartIndex then begin
    AIndex := AIndex + (FGapEndIndex - FGapStartIndex);
  end
  else begin
    // move gap
    if (AIndex + ACount > FGapStartIndex) then begin
      l := ACount;
      ACount := FGapStartIndex - AIndex;  // delete in front of gap
      FGapStartIndex := AIndex;           // move gap down
      FGapEndIndex := FGapEndIndex - ACount + (l - ACount); // extend gap at end
    end
    else begin
      FGapStartIndex := FGapStartIndex - ACount;
      FGapEndIndex   := FGapEndIndex   - ACount;
    end;
  end;

  inc(FIgnoreGapLock);
  inherited DeleteRows(AIndex, ACount);
  dec(FIgnoreGapLock);
end;

{ TSynEditMarkupHighlightMatches }

procedure TSynEditMarkupHighlightMatches.BeginSkipSendingInvalidation;
begin
  inc(FSkipSendingInvalidationLock);
end;

procedure TSynEditMarkupHighlightMatches.EndSkipSendingInvalidation;
begin
  dec(FSkipSendingInvalidationLock);
end;

procedure TSynEditMarkupHighlightMatches.BeginSendingInvalidation;
begin
  if FSendingInvalidationLock = 0 then begin
    FPendingInvalidationStartLine := -1;
    FPendingInvalidationEndLine   := -1;
  end;
  inc(FSendingInvalidationLock);
end;

procedure TSynEditMarkupHighlightMatches.EndSendingInvalidation;
begin
  dec(FSendingInvalidationLock);
  if FSendingInvalidationLock = 0 then begin
    if FPendingInvalidationStartLine >= 0 then
      InvalidateSynLines(FPendingInvalidationStartLine, FPendingInvalidationEndLine);
  end;
end;

procedure TSynEditMarkupHighlightMatches.SendMatchLineInvalidation(AStartIndex, AnEndIndex: Integer);
var
  i: Integer;
  m: TSynMarkupHighAllMatch;
  L1, L2: LongInt;
begin
  if (FSkipSendingInvalidationLock > 0) then
    exit;

  if FPendingInvalidationStartLine < 0 then begin
    if AStartIndex > AnEndIndex then
      exit;
    m := FMatches.Match[AStartIndex];
    FPendingInvalidationStartLine := m.StartPoint.Y;
    FPendingInvalidationEndLine   := m.EndPoint.Y;

    inc(AStartIndex);
  end;

  for i := AStartIndex to AnEndIndex do begin
    m := FMatches.Match[i];
    L1 := m.StartPoint.Y;
    if L1 < 0 then Continue;
    L2 := m.EndPoint.Y;

    if (L1 >= FPendingInvalidationStartLine) then begin
      if (L1 <= FPendingInvalidationEndLine + 1) then begin
        FPendingInvalidationEndLine := Max(FPendingInvalidationEndLine, L2);
        continue;
      end;

      // Start new pending block
      InvalidateSynLines(FPendingInvalidationStartLine, FPendingInvalidationEndLine);
    end
    else
    if (L2 >= FPendingInvalidationStartLine - 1) then begin
      FPendingInvalidationStartLine := Min(FPendingInvalidationStartLine, L1);
      continue;
    end
    else
      InvalidateSynLines(FPendingInvalidationStartLine, FPendingInvalidationEndLine);

    FPendingInvalidationStartLine := L1;
    FPendingInvalidationEndLine   := L2;
  end;
end;

function TSynEditMarkupHighlightMatches.HasDisplayAbleMatches: Boolean;
begin
  Result := FMatches.Count > 0;
end;

function TSynEditMarkupHighlightMatches.CreateMatchList: TSynMarkupHighAllMatchList;
begin
  Result := TSynMarkupHighAllMatchList.Create(Self);
end;

function TSynEditMarkupHighlightMatches.MarkupIdForMatch(Idx: Integer): Integer;
begin
  Result := 0;
end;

function TSynEditMarkupHighlightMatches.MarkupInfoForId(Idx: Integer): TSynHighlighterAttributesModifier;
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
  FCurrentRow := -1; // TODO: Why?
  if not HasDisplayAbleMatches then
    exit;

  if (FCurrentRow > 0) and (aRow > FCurrentRow) and
     ( (FCurrentRowNextPosIdx = -2) or // No match after FCurrentRow
       ( (FCurrentRowNextPosIdx >= 0) and (FCurrentRowNextPosIdx < FMatches.PointCount) and (aRow <= FMatches.Point[FCurrentRowNextPosIdx].y) )
      )
  then begin
    if (FCurrentRowNextPosIdx >= 0) and
      ( (aRow = FMatches.Point[FCurrentRowNextPosIdx].y) or (FCurrentRowNextPosIdx and 1 = 1) )
    then
      FCurrentRow := aRow;
    exit;
  end;

  FCurrentRow := aRow;
  FCurrentRowNextPosIdx := FMatches.IndexOfFirstMatchForLine(aRow) * 2;
  if (FCurrentRowNextPosIdx < 0) or (FCurrentRowNextPosIdx >= FMatches.PointCount) then
    exit;
  if (FMatches.Point[FCurrentRowNextPosIdx].y < aRow) then
    inc(FCurrentRowNextPosIdx);  // Use EndPoint
end;

procedure TSynEditMarkupHighlightMatches.EndMarkup;
begin
  inherited EndMarkup;
  FCurrentRow := -1;
end;

function TSynEditMarkupHighlightMatches.GetMarkupAttrIdAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; out AStartPos, AnEndPos: Integer): Integer;
var
  pos: Integer;
begin
  Result := -1;
  if (aRow <> FCurrentRow) or (FCurrentRowNextPosIdx < 0) then
    exit;

  while (FCurrentRowNextPosIdx < fMatches.PointCount) and
        (fMatches.Point[FCurrentRowNextPosIdx].y = aRow) and
        (fMatches.Point[FCurrentRowNextPosIdx].x <= aStartCol.Logical)
  do
    inc(FCurrentRowNextPosIdx);

  if FCurrentRowNextPosIdx >= fMatches.PointCount // last point was EndPoint => no markup
  then exit;

  pos := FCurrentRowNextPosIdx - 1;
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
  const AnRtlInfo: TLazSynDisplayRtlInfo): TLazEditTextAttributeModifier;
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
  if (aRow <> FCurrentRow) or (FCurrentRowNextPosIdx < 0) or
     (FCurrentRowNextPosIdx >= fMatches.PointCount) or (FMatches.Point[FCurrentRowNextPosIdx].y > aRow)
  then
    exit;

  while (FCurrentRowNextPosIdx < fMatches.PointCount) and
        (fMatches.Point[FCurrentRowNextPosIdx].y = aRow) and
        (fMatches.Point[FCurrentRowNextPosIdx].x <= aStartCol.Logical)
  do
    inc(FCurrentRowNextPosIdx);

  if FCurrentRowNextPosIdx >= fMatches.PointCount then
    exit;
  if fMatches.Point[FCurrentRowNextPosIdx].y <> aRow then
    exit;

  ANextLog := fMatches.Point[FCurrentRowNextPosIdx].x;
end;

{ TSynEditMarkupHighlightMultiMatches }

function TSynEditMarkupHighlightMultiMatches.GetMarkupInfoCount: integer;
begin
  Result := Length(FMarkupInfos);
end;

function TSynEditMarkupHighlightMultiMatches.GetMarkupInfos(AnIndex: Integer): TSynHighlighterAttributesModifier;
begin
  Result := FMarkupInfos[AnIndex];
end;

function TSynEditMarkupHighlightMultiMatches.GetMatches: TSynMarkupHighAllMultiMatchList;
begin
  Result := TSynMarkupHighAllMultiMatchList(inherited Matches);
end;

procedure TSynEditMarkupHighlightMultiMatches.SetMarkupInfoCount(AValue: integer);
var
  l, i: integer;
begin
  l := Length(FMarkupInfos);
  if l = AValue then
    exit;

  for i := AValue to l - 1 do
    FMarkupInfos[i].Free;
  SetLength(FMarkupInfos, AValue);
  for i := l to AValue - 1 do begin
    FMarkupInfos[i] := TSynSelectedColor.Create;
    FMarkupInfos[i].Clear;
  end;
end;

function TSynEditMarkupHighlightMultiMatches.CreateMatchList: TSynMarkupHighAllMatchList;
begin
  Result := TSynMarkupHighAllMultiMatchList.Create(Self);
end;

function TSynEditMarkupHighlightMultiMatches.MarkupIdForMatch(Idx: Integer): Integer;
begin
  Result := Matches.GetMarkupId(Idx);
end;

function TSynEditMarkupHighlightMultiMatches.MarkupInfoForId(Idx: Integer): TSynHighlighterAttributesModifier;
begin
  if (Idx < 0) or (Idx >= Length(FMarkupInfos)) then
    Result := MarkupInfo
   else
    Result := FMarkupInfos[Idx];
end;

destructor TSynEditMarkupHighlightMultiMatches.Destroy;
begin
  MarkupInfoCount := 0;
  inherited Destroy;
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

function TSynSearchTermDict.GetItem(AnIndex: Integer): TSynSearchTerm;
begin
  Result := FTerms[AnIndex];
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

procedure TSynSearchTermDict.SetItem(AnIndex: Integer; AValue: TSynSearchTerm);
begin
  FTerms[AnIndex] := AValue;
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
    s := FTerms[i].SearchTerm;
    FDict.Add(FTerms[i].SearchTerm, i);
    for j := i + 1 to FTerms.Count - 1 do
      if CompareText(FTerms[j].SearchTerm, s) = 0 then begin
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

procedure TSynSearchTermDict.Delete(AnIndex: Integer);
begin
  FTerms.Delete(AnIndex);
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

function TSynSearchTermDict.GetIndexForNextWordOccurrence(AnIndex: Integer): Integer;
begin
  Result := FNextTermWIthSameWord[AnIndex];
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
    Result := '';
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

function TSynSearchDictionary.GetTerms(AnIndex: Integer): String;
begin
  Result := FList[AnIndex];
end;

procedure TSynSearchDictionary.SetTerms(AnIndex: Integer; AValue: String);
begin
  FList[AnIndex] := AValue;
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

procedure TSynSearchDictionary.Delete(AnIndex: Integer);
begin
  FList.Delete(AnIndex);
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

function TSynSearchTermList.GetItem(AnIndex: Integer): TSynSearchTerm;
begin
  Result := TSynSearchTerm(inherited GetItem(AnIndex));
end;

procedure TSynSearchTermList.SetItem(AnIndex: Integer; AValue: TSynSearchTerm);
begin
  inherited SetItem(AnIndex, AValue);
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
    while (Result < c) and (CompareText(Items[Result].SearchTerm, ATerm) <> 0) do
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

    if o.MatchCase and (StrLComp(MatchBegin, PChar(o.SearchTerm), Len)<>0) then begin
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
    FMatches.SetMatchPoints(i, Point(MatchBegin-FFindLineText+1, FFindLineY),
                            Point(MatchBegin-FFindLineText+1+Len, FFindLineY)
                           );
    if i + 1 < FFindInsertIndex then
      FMatches.Delete(i+1, FFindInsertIndex - (i + 1));
    if not FBackward then
      FFindInsertIndex := i + 1
    else assert(i = FFindInsertIndex);
  end
  else begin
    if FBackwardReplace then begin
      // Replace, only keep last match
      FMatches.SetMatchPoints(FFindInsertIndex,
                              Point(MatchBegin-FFindLineText+1, FFindLineY),
                              Point(MatchBegin-FFindLineText+1+Len, FFindLineY)
                             );
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

function TSynEditMarkupHighlightAllMulti.FindMatches(AStartPoint, AnEndPoint: TPoint;
  var AnIndex: Integer; AStopAfterLine: Integer; ABackward: Boolean): TPoint;
var
  LineLen: Integer;
  LineText, LineTextLower: String;
  x: integer;
begin
  //debugln(['FindMatches IDX=', AnIndex, ' Cnt=', Matches.Count, ' LCnt=', AnEndPoint.y-AStartPoint.y , ' # ', FTerms[0].SearchTerm, ' # ',dbgs(AStartPoint),' - ',dbgs(AnEndPoint), ' AStopAfterLine=',AStopAfterLine, ' Back=', dbgs(ABackward), '  ']);
  FFindInsertIndex := AnIndex;
  FFindStartedAtIndex := FFindInsertIndex;
  FBackward := ABackward; // Currently supports only finding a single match

  if ABackward then begin
    FBackwardReplace := False;
    x := 1;
    while AStartPoint.y <= AnEndPoint.y do begin
      LineText := Lines[AnEndPoint.y-1];
      LineTextLower := LowerCase(LineText);

      LineLen := Min(Length(LineTextLower), AnEndPoint.x-1);
      if AnEndPoint.X <= LineLen then
        LineLen := AnEndPoint.x - 1;
      if (AStartPoint.y = AnEndPoint.y) and (AStartPoint.x > 1) then begin
        x := AStartPoint.x - 1;
        LineLen := Max(0, LineLen - x);
      end;

      if LineLen > 0 then begin
        FFindLineY := AnEndPoint.Y;
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

      dec(AnEndPoint.y);
      AnEndPoint.x := MaxInt;

      //if (AStopAfterLine >= 0) and (AStartPoint.Y-1 > AStopAfterLine) and
      //   (FFindInsertIndex > AnIndex)
      //then begin
      //  AnEndPoint := point(LineLen, AStartPoint.Y-1);
      //  break;
      //end;
    end;

    if FBackwardReplace then
      inc(FFindInsertIndex);
  end
  else begin
    while AStartPoint.y <= AnEndPoint.y do begin
      LineText := Lines[AStartPoint.y-1];
      LineTextLower := LowerCase(LineText);

      LineLen := Length(LineTextLower) + 1 - AStartPoint.x;
      if AStartPoint.y = AnEndPoint.y then
        LineLen := Min(LineLen, AnEndPoint.x - AStartPoint.x + 1);

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
         (FFindInsertIndex > AnIndex)
      then begin
        AnEndPoint := point(LineLen + 1, AStartPoint.Y-1);
        break;
      end;
    end;
  end;

  AnIndex := FFindInsertIndex;
  Result := AnEndPoint;
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

procedure TSynEditMarkupHighlightAllMulti.DeleteSearchTerm(AnIndex: Integer);
begin
  FTermDict.Delete(AnIndex);
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

function TSynEditMarkupHighlightAll.FindMatches(AStartPoint, AnEndPoint: TPoint;
  var AnIndex: Integer; AStopAfterLine: Integer; ABackward: Boolean): TPoint;
var
  ptFoundStart, ptFoundEnd: TPoint;
begin
  fSearch.Backwards := ABackward;
  While (true) do begin
    if not fSearch.FindNextOne(Lines, AStartPoint, AnEndPoint, ptFoundStart, ptFoundEnd)
    then break;
    AStartPoint := ptFoundEnd;

    FMatches.Insert(AnIndex, ptFoundStart, ptFoundEnd);
    inc(AnIndex); // BAckward learch needs final index to point to last inserted (currently support only find ONE)

    if (AStopAfterLine >= 0) and (ptFoundStart.Y > AStopAfterLine) then begin
      AnEndPoint := ptFoundEnd;
      break;
    end;
  end;
  Result := AnEndPoint;
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

constructor TSynMarkupHighAllMatchList.Create(AnOwner: TSynEditMarkupHighlightMatches);
begin
  inherited Create;
  FOwner := AnOwner;
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
    exit(0); // past the last element
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

function TSynMarkupHighAllMatchList.IndexOf(APoint: TPoint): Integer;
var
  l, h: Integer;
begin
  h := Count -1;
  if h < 0 then
    exit(0);
  l := 0;
  Result := (l+h) div 2;
  while (h > l) do begin
    if PSynMarkupHighAllMatch(ItemPointer[Result])^.EndPoint <= APoint then
      l := Result + 1
    else
      h := Result;
    Result := (l+h) div 2;
  end;
  if (PSynMarkupHighAllMatch(ItemPointer[Result])^.EndPoint <= APoint) then
    inc(Result);
end;

procedure TSynMarkupHighAllMatchList.Clear;
begin
  GapStartIndex := -1;
  GapEndIndex := -1;

  if (FOwner <> nil) and (Count > 0) then
    FOwner.SendMatchLineInvalidation(0, Count-1);

  Count := 0;
end;

procedure TSynMarkupHighAllMatchList.SendInvalidateAll;
begin
  if (FOwner <> nil) and (Count > 0) then
    FOwner.SendMatchLineInvalidation(0, Count-1);
end;

procedure TSynMarkupHighAllMatchList.Delete(AnIndex: Integer; ACount: Integer);
begin
  if AnIndex >= Count then
    exit;
  if AnIndex + ACount > Count then
    ACount := Count - AnIndex;
  {$PUSH}{$R-}{$Q-}FChangeStamp := FChangeStamp+1;{$POP}
  if FOwner <> nil then
    FOwner.SendMatchLineInvalidation(AnIndex, AnIndex+ACount-1);
  DeleteRows(AnIndex, ACount);
end;

procedure TSynMarkupHighAllMatchList.Insert(AnIndex: Integer; ACount: Integer);
var
  i: Integer;
begin
  if AnIndex > Count then
    exit;
  {$PUSH}{$R-}{$Q-}FChangeStamp := FChangeStamp+1;{$POP}

  if AnIndex = GapStartIndex then begin
    i := GapEndIndex - GapStartIndex;
    GapStartIndex := GapStartIndex + ACount;
    if ACount < i then
      exit;
    if ACount = i then begin
      GapStartIndex := -1;
      GapEndIndex   := -1;
      exit;
    end;
    ACount := ACount - i;
  end;

  InsertRows(AnIndex, ACount);
end;

procedure TSynMarkupHighAllMatchList.Insert(AnIndex: Integer; AStartPoint, AnEndPoint: TPoint);
var
  p: PSynMarkupHighAllMatch;
  g: Boolean;
begin
  {$PUSH}{$R-}{$Q-}FChangeStamp := FChangeStamp+1;{$POP}
  g := AnIndex = GapStartIndex;
  Insert(AnIndex);
  p := PSynMarkupHighAllMatch(ItemPointer[AnIndex]);
  if g then begin
    if (p^.StartPoint = AStartPoint) and
       (p^.EndPoint   = AnEndPoint) and
       (ItemSize = SizeOf(TSynMarkupHighAllMatch))
    then
      exit;

    if (FOwner <> nil) then
      FOwner.SendMatchLineInvalidation(AnIndex, AnIndex);
  end;

  p^.StartPoint := AStartPoint;
  p^.EndPoint   := AnEndPoint;
  if FOwner <> nil then
    FOwner.SendMatchLineInvalidation(AnIndex, AnIndex);
end;

function TSynMarkupHighAllMatchList.Insert(AStartPoint, AnEndPoint: TPoint): integer;
begin
  Result := IndexOf(AStartPoint);
  Insert(Result, AStartPoint, AnEndPoint);
end;

procedure TSynMarkupHighAllMatchList.SetMatchPoints(const AnIndex: Integer; AStartPoint,
  AnEndPoint: TPoint);
var
  p: PSynMarkupHighAllMatch;
  r: Boolean;
begin
  p := PSynMarkupHighAllMatch(ItemPointer[AnIndex]);
  if (p^.StartPoint = AStartPoint) and (p^.EndPoint = AnEndPoint) then
    exit;
  if FOwner <> nil then begin
    r := (p^.StartPoint.Y <> AStartPoint.Y) or (p^.EndPoint.Y <> AnEndPoint.Y);
    FOwner.SendMatchLineInvalidation(AnIndex, AnIndex);
  end;
  p^.StartPoint := AStartPoint;
  p^.EndPoint   := AnEndPoint;
  if (FOwner <> nil) and r then
    FOwner.SendMatchLineInvalidation(AnIndex, AnIndex);
end;

procedure TSynMarkupHighAllMatchList.SetCount(const AValue : Integer);
begin
  if Count=AValue then exit;
  {$PUSH}{$R-}{$Q-}FChangeStamp := FChangeStamp+1;{$POP}
  if (Capacity <= AValue) then begin
    Capacity := Max(Capacity, AValue) * 2;
    inherited SetCount(AValue);
  end
  else begin
    inherited SetCount(AValue);
    MaybeReduceCapacity;
  end;
end;

function TSynMarkupHighAllMatchList.StartValidation(AnFirstInvalidMatchLine,
  AnLastInvalidMatchLine, AnMatchLinesDiffCount: Integer; out AFirstInvalidPoint,
  ALastInvalidPoint: TPoint): integer;
var
  NewGapStart, NewGapEnd, d, sz, i: Integer;
  p: Pointer;
begin
  Result := -1;
  AFirstInvalidPoint := classes.Point(-1, -1);
  ALastInvalidPoint  := classes.Point(-1, -1);

  if AnFirstInvalidMatchLine < 0 then
    exit;

  NewGapStart := IndexOfFirstMatchForLine(AnFirstInvalidMatchLine);
  Result := NewGapStart;
  if NewGapStart >= Count then
    exit;
  NewGapEnd := IndexOfLastMatchForLine(AnLastInvalidMatchLine) + 1;
  if NewGapEnd <= NewGapStart then
    exit;

  // the first/last newly invalidated point
  AFirstInvalidPoint := StartPoint[NewGapStart];
  ALastInvalidPoint  := EndPoint[NewGapEnd - 1];

  if GapStartIndex >= 0 then begin
    // adjust new gap (raw index values)
    if GapStartIndex < NewGapStart then
      NewGapStart := NewGapStart + (GapEndIndex - GapStartIndex);
    if GapStartIndex < NewGapEnd then
      NewGapEnd := NewGapEnd + (GapEndIndex - GapStartIndex);

    // deal with old gap
    if GapStartIndex < NewGapStart then begin   // OLD gap in FRONT
      if GapEndIndex >= NewGapStart then begin   // overlap
        NewGapStart := GapStartIndex;
      end
      else begin                                 // move old gap // revalidation start at the new gap
        sz := NewGapStart - GapEndIndex;
        MoveRaw(GapEndIndex, GapStartIndex, sz);
        NewGapStart := NewGapStart - (GapEndIndex - GapStartIndex);
      end;
    end
    else begin                                 // OLD gap BEHIND -- or INSIDE
      if GapStartIndex <= NewGapEnd then begin  // overlap
        if GapEndIndex > NewGapEnd then
          NewGapEnd := GapEndIndex;
      end
      else begin                                 // move old gap // revalidation start at the new gap
        sz := GapStartIndex - NewGapEnd;
        assert(sz > 0, 'TSynMarkupHighAllMatchList.StartValidation: sz > 0');
        MoveRaw(NewGapEnd, GapEndIndex - sz, sz);
        NewGapEnd := NewGapEnd + (GapEndIndex - GapStartIndex);
      end;
    end;
  end;

  GapStartIndex := NewGapStart;
  GapEndIndex := NewGapEnd;

  d := AnMatchLinesDiffCount;
  if d > 0 then begin
    p := ItemPointerRaw[NewGapEnd];
    sz := ItemSize;
    for i := NewGapEnd to CountRaw - 1 do begin
      inc(PSynMarkupHighAllMatch(p)^.StartPoint.Y, d);
      inc(PSynMarkupHighAllMatch(p)^.EndPoint.Y, d);
      inc(p, sz);
    end;
  end;
end;

procedure TSynMarkupHighAllMatchList.EndValidation;
const
  MAX_GAP_SIZE = 100;
var
  s, e, i: Integer;
begin
  assert(GapEndIndex >= GapStartIndex, 'TSynMarkupHighAllMatchList.EndValidation: GapEndIndex >= GapStartIndex');

  if GapEndIndex > GapStartIndex then begin
    s := GapStartIndex;
    e := GapEndIndex;
    GapStartIndex := -1;
    GapEndIndex   := -1;

    if FOwner <> nil then
      FOwner.SendMatchLineInvalidation(s, e-1);

    if e > s + MAX_GAP_SIZE then begin
      i := e;
      e := s + MAX_GAP_SIZE;
      MoveRaw(i, e, CountRaw - i);
      Count := Count - (i - e);
    end;

    for i := s to e-1 do
      PSynMarkupHighAllMatch(ItemPointer[i])^.StartPoint.Y := -1;

    GapStartIndex := s;
    GapEndIndex   := e;
  end;
end;

function TSynMarkupHighAllMatchList.GetPointCount : Integer;
begin
  result := Count * 2;
end;

function TSynMarkupHighAllMatchList.GetPoint(const AnIndex : Integer) : TPoint;
begin
  if (AnIndex and 1) = 0
  then Result := PSynMarkupHighAllMatch(ItemPointer[AnIndex>>1])^.StartPoint
  else Result := PSynMarkupHighAllMatch(ItemPointer[AnIndex>>1])^.EndPoint;
end;

function TSynMarkupHighAllMatchList.GetStartPoint(const AnIndex : Integer) : TPoint;
begin
  Result := PSynMarkupHighAllMatch(ItemPointer[AnIndex])^.StartPoint;
end;

procedure TSynMarkupHighAllMatchList.SetStartPoint(const AnIndex : Integer; const AValue : TPoint);
begin
  assert(FOwner=nil, 'TSynMarkupHighAllMatchList.SetStartPoint: FOwner=nil');
  if AnIndex = Count
  then Count := Count + 1; // AutoIncrease
  PSynMarkupHighAllMatch(ItemPointer[AnIndex])^.StartPoint := AValue;
  {$PUSH}{$R-}{$Q-}FChangeStamp := FChangeStamp+1;{$POP}
end;

function TSynMarkupHighAllMatchList.GetInintialForItemSize: Integer;
begin
  Result := SizeOf(TSynMarkupHighAllMatch);
end;

function TSynMarkupHighAllMatchList.GetEndPoint(const AnIndex : Integer) : TPoint;
begin
  Result := PSynMarkupHighAllMatch(ItemPointer[AnIndex])^.EndPoint;
end;

procedure TSynMarkupHighAllMatchList.SetEndPoint(const AnIndex : Integer; const AValue : TPoint);
begin
  assert(FOwner=nil, 'TSynMarkupHighAllMatchList.SetEndPoint: FOwner=nil');
  if AnIndex = Count
  then Count := Count + 1; // AutoIncrease
  PSynMarkupHighAllMatch(ItemPointer[AnIndex])^.EndPoint := AValue;
  {$PUSH}{$R-}{$Q-}FChangeStamp := FChangeStamp+1;{$POP}
end;

function TSynMarkupHighAllMatchList.GetMatch(const AnIndex: Integer): TSynMarkupHighAllMatch;
begin
  Result := PSynMarkupHighAllMatch(ItemPointer[AnIndex])^;
end;

procedure TSynMarkupHighAllMatchList.SetMatch(const AnIndex: Integer;
  const AValue: TSynMarkupHighAllMatch);
var
  p: PSynMarkupHighAllMatch;
  r: Boolean;
begin
  if AnIndex = Count
  then Count := Count + 1; // AutoIncrease
  p := PSynMarkupHighAllMatch(ItemPointer[AnIndex]);
  r := (FOwner <> nil) and CompareMem(p, @AValue, SizeOf(TSynMarkupHighAllMatch));
  p^ := AValue;
  {$PUSH}{$R-}{$Q-}FChangeStamp := FChangeStamp+1;{$POP}
  if r then
    FOwner.SendMatchLineInvalidation(AnIndex, AnIndex);
end;

{ TSynMarkupHighAllMultiMatchList }

function TSynMarkupHighAllMultiMatchList.GetMarkupId(AnIndex: Integer): Integer;
begin
  Result := PInteger(ItemPointer[AnIndex]+FParentItemSize)^;
end;

procedure TSynMarkupHighAllMultiMatchList.SetMarkupId(AnIndex: Integer; AValue: Integer);
var
  p: PInteger;
begin
  p := PInteger(ItemPointer[AnIndex]+FParentItemSize);
  if p^ = AValue then
    exit;
  p^ := AValue;
  if (Owner <> nil) then
    Owner.SendMatchLineInvalidation(AnIndex, AnIndex);
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
  FScanMode := smsmDirect;
  FScanOffScreenLimit := DEFAULT_SCAN_OFFSCREEN_LIMIT;
  FFirstInvalidMatchLine := -1;
  FLastInvalidMatchLine := -1;
  FHideSingleMatch := False;
  FMarkupEnabled := MarkupInfo.IsEnabled;
end;

destructor TSynEditMarkupHighlightAllBase.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if Lines <> nil then
    Lines.RemoveChangeHandler(senrLineMappingChanged, @DoFoldChanged);
  inherited Destroy;
end;

procedure TSynEditMarkupHighlightAllBase.DecPaintLock;
begin
  inherited DecPaintLock;
  if (FPaintLock = 0) then begin
    if FNeedValidate then
      ValidateMatches(not FNeedValidatePaint)
    else
    if smfNeedAsync in FFlags then
      Application.QueueAsyncCall(@DoAsyncScan, 0);
  end;
end;

procedure TSynEditMarkupHighlightAllBase.DoTopLineChanged(OldTopLine : Integer);
begin
  ValidateMatches(True);
end;

procedure TSynEditMarkupHighlightAllBase.DoLinesInWindoChanged(OldLinesInWindow : Integer);
begin
  ValidateMatches(True);
end;

procedure TSynEditMarkupHighlightAllBase.DoMarkupChanged(AMarkup : TLazEditTextAttribute);
begin
  If (not FMarkupEnabled) and MarkupInfo.IsEnabled then
    Invalidate
  else
    FMatches.SendInvalidateAll;
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

  if FMatches.Count = 1 then
    SendLineInvalidation;

  FHideSingleMatch := AValue;

  if FMatches.Count = 1 then
    if FHideSingleMatch then
      Invalidate // TODO only need extend search
      //ValidateMatches()  // May find a 2nd, by extending startpos
    else
      FMatches.SendInvalidateAll;
end;

procedure TSynEditMarkupHighlightAllBase.DoFoldChanged(Sender: TSynEditStrings;
  AnIndex, aCount: Integer);
begin
  ValidateMatches(True);
end;

procedure TSynEditMarkupHighlightAllBase.SetScanOffScreenLimit(AValue: integer);
begin
  if AValue < -1 then
    raise Exception.Create('invalide limit');
  if FScanOffScreenLimit = AValue then
    exit;
  FScanOffScreenLimit := AValue;
  ValidateMatches(True); // Should only need to extend off screen
end;

procedure TSynEditMarkupHighlightAllBase.InvalidateMatches(AFirstLine, ALastLine,
  ALineDiffCount: Integer);
begin
  Application.RemoveAsyncCalls(Self);
  Exclude(FFlags, smfNeedAsync);

  if AFirstLine < 1 then
    AFirstLine := 1;
  if (ALastLine < 1) then
    ALastLine := MaxInt
  else
  if ALastLine < AFirstLine then
    ALastLine := AFirstLine;

  if (AFirstLine < FFirstInvalidMatchLine) or (FFirstInvalidMatchLine <= 0) then
    FFirstInvalidMatchLine := AFirstLine;
  if (ALastLine > FLastInvalidMatchLine) then
    FLastInvalidMatchLine := ALastLine;

  FMatchLinesDiffCount := FMatchLinesDiffCount + ALineDiffCount;
end;

procedure TSynEditMarkupHighlightAllBase.AssertGapsValid;
var
  i, j: Integer;
  g: TSynMarkupHighAllValidRange;
begin
  for i := 0 to FValidRanges.Count do begin
    g := FValidRanges.Gap[i];
    if (not g.StartPoint.HasData) or (g.StartPoint >= g.EndPoint) then continue;
    j := FMatches.IndexOf(g.StartPoint);
    assert((j>=FMatches.Count) or (j<0) or (FMatches.StartPoint[j] >= g.EndPoint), 'AssertGapsValid: before '+IntToStr(j));
    assert((j<=0) or (FMatches.EndPoint[j-1] <= g.StartPoint), 'AssertGapsValid: after'+IntToStr(j));
  end;
end;

procedure TSynEditMarkupHighlightAllBase.DoAsyncScan(Data: PtrInt);
var
  r, SkipPaint: Boolean;
begin
  if FPaintLock > 0 then begin
    Include(FFlags, smfNeedAsync);
    exit;
  end;
  Exclude(FFlags, smfNeedAsync);

  SkipPaint := (smfAsyncSkipPaint in FFlags) or (not FHideSingleMatch) or (FMatches.Count > 1);
  if SkipPaint then
    BeginSkipSendingInvalidation;
  r := ValidateFillGaps;
  if SkipPaint then
    EndSkipSendingInvalidation;
  if not r then begin
    Application.QueueAsyncCall(@DoAsyncScan, 0);
  end;
end;

function TSynEditMarkupHighlightAllBase.ValidateFillGaps: boolean;
var
  LastTextLine, OffsetForMultiLine: Integer;     // Last visible
  DoneSearch: boolean;

  procedure RepairMatches(AFirstBrokenIndex: integer; AGap: TSynMarkupHighAllValidRange);
  var
    IdxEnd: Integer;
  begin
    assert(False, 'RepairMatches');
    DebugLn('RepairMatches');
    IdxEnd := FMatches.IndexOf(AGap.EndPoint);
    if IdxEnd <= AFirstBrokenIndex then
      FMatches.Clear
    else
      FMatches.DeleteRows(AFirstBrokenIndex, IdxEnd - AFirstBrokenIndex);
  end;

  function FillGap(AGap: TSynMarkupHighAllValidRange; AStopAfterLine: integer): boolean;
  var
    i: Integer;
  begin
    Result := False;
    assert(AGap.StartPoint.HasData, 'FillGap: AGap.StartPoint.HasData');
    if not AGap.StartPoint.HasData then
      exit;

    if (AStopAfterLine > 0) and (AGap.StartPoint.Y > AStopAfterLine) then
      exit;

    if AGap.EndPoint.y > LastTextLine then begin
      AGap.EndPoint := min(AGap.EndPoint, Point(1, LastTextLine+1));
      if AGap.EndPoint = AGap.StartPoint then
        exit;
    end;
    assert(AGap.EndPoint > AGap.StartPoint, 'FillGap: AGap.EndPoint > AGap.StartPoint');

    DoneSearch := True;

    i := FMatches.IndexOf(AGap.StartPoint);
    Assert((i<=0) or (Matches.EndPoint[i-1] <= AGap.StartPoint), 'after prev point');
    Assert((i>=FMatches.Count) or (Matches.StartPoint[i] >= AGap.EndPoint), 'before next point');
    if ( (i > 0) and (Matches.EndPoint[i-1] > AGap.StartPoint) ) or
       ( (i < FMatches.Count) and (Matches.StartPoint[i] < AGap.EndPoint) )
    then begin
      RepairMatches(i, AGap);
      i := FMatches.IndexOf(AGap.StartPoint);
    end;

    if OffsetForMultiLine > 0 then begin
      AGap.StartPoint := Point(1, max(1, AGap.StartPoint.y - OffsetForMultiLine));
      if i > 0 then
        AGap.StartPoint := max(FMatches.EndPoint[i-1], AGap.StartPoint);
      AGap.EndPoint := Point(1, min(LastTextLine+1, AGap.EndPoint.y + OffsetForMultiLine + 1));
      if i < FMatches.Count then
        AGap.EndPoint := min(FMatches.StartPoint[i], AGap.EndPoint)
    end;
    if AStopAfterLine > AGap.EndPoint.Y then
      AStopAfterLine := -1;

    AGap.EndPoint := FindMatches(AGap.StartPoint, AGap.EndPoint, i, AStopAfterLine, False);

    assert(AGap.EndPoint.x >= 1, 'FillGap: AGap.EndPoint.x >= 1');
    if AGap.EndPoint.x = 0 then AGap.EndPoint.x := 1;
    assert(AGap.EndPoint > AGap.StartPoint, 'FillGap: AGap.EndPoint > AGap.StartPoint');
    FValidRanges.MarkValid(AGap.StartPoint, AGap.EndPoint);
    Result := True;
  end;

var
  TopScreenLine, LastScreenLine, LastScreenLineAdj: Integer;
  ExtStartLine, ExtEndLine, ExtEndLineAdj: integer;
  CurrentGapIdx, CntASync, Lim: Integer;
  CurrentGap: TSynMarkupHighAllValidRange;
  ExtStartPoint: TPoint;
begin
  Result := True; // All done
  DoneSearch := False;
  FindInitialize;

  TopScreenLine := TopLine;
  LastTextLine   := SynEdit.Lines.Count;
  LastScreenLine := SynEdit.PartialBottomLine;
  OffsetForMultiLine := SearchStringMaxLines - 1;
  if OffsetForMultiLine < 0 then
    OffsetForMultiLine := DEFAULT_MULTILINE_SEARCH_OFFS;

  (* Search in on-screen lines first  *)

  CurrentGapIdx := FValidRanges.FindGapFor(Point(1, TopScreenLine), True);
  if CurrentGapIdx >= 0 then begin
    LastScreenLineAdj := LastScreenLine + OffsetForMultiLine + AVOID_GAP_MAX_OFFS;

    CurrentGap := FValidRanges.Gap[CurrentGapIdx];
    if CurrentGap.StartPoint.y < Max(1, TopScreenLine - OffsetForMultiLine - AVOID_GAP_MAX_OFFS) then begin
      // FillGap will substract OffsetForMultiLine => but that still keeps AVOID_GAP_MAX_OFFS
      CurrentGap.StartPoint := Point(1, TopScreenLine);
      assert(CurrentGap.StartPoint < CurrentGap.EndPoint, 'TSynEditMarkupHighlightAllBase.ValidateMatches: CurrentGap.StartPoint < CurrentGap.EndPoint');
    end;

    repeat
      if CurrentGap.StartPoint.Y > LastScreenLine then
        break;
      if CurrentGap.EndPoint.y > LastScreenLineAdj then
        CurrentGap.EndPoint := Point(1, LastScreenLine+1); // FillGap will add OffsetForMultiLine;

      if not FillGap(CurrentGap, LastScreenLine) then
        break;

      CurrentGapIdx := FValidRanges.FindGapFor(Point(1, TopScreenLine), True);
      if CurrentGapIdx < 0 then
        break;
      CurrentGap := FValidRanges.Gap[CurrentGapIdx];
    until False;

    if DoneSearch then begin
      if ( (ScanMode in [smsmASync]) and HideSingleMatch and (FMatches.Count = 1) ) or
         ( (ScanMode in [smsmASyncForceAll]) )
      then
        exit(False); // maybe more to do
    end;
  end;


  (* Search 2nd match for HideSingleMatch  *)

  if HideSingleMatch and (FMatches.Count = 1) and (FScanOffScreenLimit <> 0) and
     not(ScanMode in [smsmDirectForceAll, smsmASyncForceAll])
  then begin
    // find before/after screen
    if (FScanOffScreenLimit = -1) or (FScanOffScreenLimit >= TopScreenLine) then
      ExtStartLine := 1
    else
      ExtStartLine := TopScreenLine - FScanOffScreenLimit;
    ExtStartPoint := point(1, ExtStartLine);
    if (FScanOffScreenLimit = -1) or (FScanOffScreenLimit >= high(LastScreenLine) - 2 - LastScreenLine) then
      ExtEndLine := high(LastScreenLine) - 2
    else
      ExtEndLine := LastScreenLine + FScanOffScreenLimit;
    {$PUSH}{$R-}{$Q-}
    ExtEndLineAdj := ExtEndLine + OffsetForMultiLine + AVOID_GAP_MAX_OFFS;
    if ExtEndLineAdj < ExtEndLine then
      ExtEndLineAdj := high(ExtEndLineAdj);
    {$POP}

    CurrentGapIdx := FValidRanges.FindGapFor(ExtStartPoint, True);
    if CurrentGapIdx >= 0 then begin
      CurrentGap := FValidRanges.Gap[CurrentGapIdx];
      if CurrentGap.StartPoint.y < Max(1, ExtStartLine - OffsetForMultiLine - AVOID_GAP_MAX_OFFS) then begin
        CurrentGap.StartPoint := Point(1, ExtStartLine);    // FillGap will substract OffsetForMultiLine => but that still keeps AVOID_GAP_MAX_OFFS
        assert(CurrentGap.StartPoint < CurrentGap.EndPoint, 'TSynEditMarkupHighlightAllBase.ValidateMatches: CurrentGap.StartPoint < CurrentGap.EndPoint');
      end;

      repeat
        if CurrentGap.StartPoint.Y > ExtEndLine then
          break;
        if CurrentGap.EndPoint.y > ExtEndLineAdj then
          CurrentGap.EndPoint := Point(1, ExtEndLine + 1);

        if not FillGap(CurrentGap, 0) then
          break;

        if FMatches.Count > 1 then
          break;

        CurrentGapIdx := FValidRanges.FindGapFor(ExtStartPoint, True);
        if CurrentGapIdx < 0 then
          break;
        CurrentGap := FValidRanges.Gap[CurrentGapIdx];
      until False;
    end;
  end;

  if (ScanMode in [smsmDirect, smsmASync]) or
     ( (ScanMode = smsmDirectExtendASync) and not(HideSingleMatch and (FMatches.Count = 1)) )
  then
    exit;

  if DoneSearch and not (ScanMode in [smsmDirectForceAll]) then
    exit(False); // maybe more to do


  (* Search all *)

  CntASync := 1000;
  if (ScanMode in [smsmDirectForceAll, smsmDirectForceAllASync, smsmASyncForceAll]) or
     ( (ScanMode = smsmDirectExtendASync) and HideSingleMatch and (FMatches.Count = 1) )
  then begin
    Lim := -1;
    if ScanMode = smsmDirectExtendASync then
      Lim := 0; // only find ONE
    // search all
    CurrentGapIdx := FValidRanges.FindGapFor(Point(1, 1), True);
    while (CurrentGapIdx >= 0) do begin
      CurrentGap := FValidRanges.Gap[CurrentGapIdx];

      if not (ScanMode in [smsmDirectForceAll]) then begin
        // limit search range
        if CurrentGap.EndPoint.Y > CurrentGap.StartPoint.y + CntASync + OffsetForMultiLine + AVOID_GAP_MAX_OFFS then
          CurrentGap.EndPoint := Point(1, CurrentGap.StartPoint.y + CntASync + 1);
        CntASync := CntASync - (CurrentGap.EndPoint.Y - CurrentGap.StartPoint.y);
      end;

      if not FillGap(CurrentGap, Lim) then
        break;

      if CntASync < 100 then
        break;
      CurrentGapIdx := FValidRanges.FindGapFor(Point(1, 1), True);
    end;
    if DoneSearch then
      Result := False; // maybe more to do
  end;
end;

procedure TSynEditMarkupHighlightAllBase.ValidateMatches(SkipPaint: Boolean);
var
  LastScreenLine: Integer;     // Last visible
  FirstKeptValidIdx: Integer; // The first index, kept after the removed invalidated range

  procedure MaybeDropOldMatches;
  var
    Idx: Integer;
  begin
    // remove matches, that are too far off the current visible area
    if (FMatches.Count <= MATCHES_CLEAN_CNT_THRESHOLD) then
      exit;

    if TopLine - FMatches.EndPoint[0].y > MATCHES_CLEAN_LINE_THRESHOLD then begin
      Idx := FMatches.IndexOfFirstMatchForLine(TopLine - MATCHES_CLEAN_LINE_KEEP);
      // Using the Idx as Count => Delete only "up to before that Idx"
      if Idx > 0 then begin
        FMatches.Delete(0, Idx);
        if FMatches.Count > 0 then
          FValidRanges.RemoveBefore(FMatches.StartPoint[0])
        else
          FValidRanges := nil;
        if FirstKeptValidIdx >= Idx then
          FirstKeptValidIdx := max(0, FirstKeptValidIdx - Idx);
          {$IFOPT C+} AssertGapsValid; {$ENDIF}
        if FMatches.Count = 0 then
          exit
      end;
    end;
    if FMatches.StartPoint[FMatches.Count-1].y - LastScreenLine > MATCHES_CLEAN_LINE_THRESHOLD then begin
      Idx := FMatches.IndexOfLastMatchForLine(LastScreenLine  + MATCHES_CLEAN_LINE_KEEP) + 1;
      // Deleting above the Idx (Idx is already "+ 1")
      if Idx < FMatches.Count then begin
        FMatches.Delete(Idx, FMatches.Count - Idx);
        if FMatches.Count > 0 then
          FValidRanges.RemoveAfter(FMatches.EndPoint[FMatches.Count-1])
        else
          FValidRanges := nil;
        if FirstKeptValidIdx >= Idx then
          FirstKeptValidIdx := Idx;
        {$IFOPT C+} AssertGapsValid; {$ENDIF}
      end;
    end;
  end;

var
  GapStartPoint, GapEndPoint: TPoint;
begin
  Application.RemoveAsyncCalls(Self);
  FFlags := FFlags - [smfNeedAsync, smfAsyncSkipPaint];

  FCurrentRowNextPosIdx := -1;
  FCurrentRow := -1;
  if (FPaintLock > 0) or (not SynEdit.IsVisible) then begin
    FNeedValidate := True;
    if not SkipPaint then
      FNeedValidatePaint := True;
    exit;
  end;
  FNeedValidate := False;
  FNeedValidatePaint := False;

  if SkipPaint then
    BeginSkipSendingInvalidation;
  BeginSendingInvalidation;
  try

    if (not HasSearchData) or (not MarkupInfo.IsEnabled) then begin
      fMatches.Clear;
      FValidRanges := nil;
      exit;
    end;

    LastScreenLine := SynEdit.PartialBottomLine;

    FirstKeptValidIdx := FMatches.StartValidation(FFirstInvalidMatchLine, FLastInvalidMatchLine, FMatchLinesDiffCount, GapStartPoint, GapEndPoint);
    FValidRanges.AdjustForLinesChanged(FLastInvalidMatchLine, FMatchLinesDiffCount);

    if ScanMode in [smsmDirect, smsmDirectExtendASync, smsmASync] then
      MaybeDropOldMatches;

    if FFirstInvalidMatchLine > 0 then begin
      Assert(FirstKeptValidIdx >= 0, 'ValidateMatches: FirstKeptValidIdx >= 0');
      // remove extra valid lines before the gap, in case a multiline match now goes into the gap
      if (not GapStartPoint.HasData) or (GapEndPoint.Y >= FFirstInvalidMatchLine) then begin
        GapStartPoint := point(1, FFirstInvalidMatchLine);
        assert((FirstKeptValidIdx=0) or (FMatches.EndPoint[FirstKeptValidIdx-1] < GapStartPoint), 'TSynEditMarkupHighlightAllBase.ValidateMatches: (FirstKeptValidIdx=0) or (FMatches.EndPoint[FirstKeptValidIdx-1] < GapStartPoint)');
      end;

      // remove extra valid lines after the gap, in case a multiline match now starts in the end of the gap
      if (not GapEndPoint.HasData) or (GapEndPoint.Y <= FLastInvalidMatchLine) then begin
        if FLastInvalidMatchLine >= MaxInt - 2 then
          FLastInvalidMatchLine := MaxInt - 2;
        GapEndPoint := point(1, FLastInvalidMatchLine + 1);
        assert((FirstKeptValidIdx = FMatches.Count) or (GapEndPoint <= FMatches.StartPoint[FirstKeptValidIdx]), 'TSynEditMarkupHighlightAllBase.ValidateMatches: (FirstKeptValidIdx = FMatches.Count) or (GapEndPoint <= FMatches.StartPoint[FirstKeptValidIdx])');
      end;

      FValidRanges.RemoveBetween(GapStartPoint, GapEndPoint);
    end;

    if not ValidateFillGaps then begin
      if SkipPaint then
        Include(FFlags, smfAsyncSkipPaint);
      Application.QueueAsyncCall(@DoAsyncScan, 0);
    end;

  finally
    FFirstInvalidMatchLine := -1;
    FLastInvalidMatchLine  := -1;
    FMatchLinesDiffCount   := 0;

    FMatches.EndValidation;
    EndSendingInvalidation;
    if SkipPaint then
      EndSkipSendingInvalidation;
    {$IFOPT C+} AssertGapsValid; {$ENDIF}
  end;
end;

procedure TSynEditMarkupHighlightAllBase.BeginSendingInvalidation;
begin
  inherited BeginSendingInvalidation;
  if SendingInvalidationLock = 1 then begin
    FInvalidationStartedWithHiddenMatch := (FMatches.Count = 1) and FHideSingleMatch;
    FInvalidationStartedWithManyatches  := (FMatches.Count > 1);
  end;
end;

procedure TSynEditMarkupHighlightAllBase.EndSendingInvalidation;
begin
  if (FMatches.Count = 1) and FHideSingleMatch and FInvalidationStartedWithManyatches then begin
    inherited SendMatchLineInvalidation(0,0);
  end;
  inherited EndSendingInvalidation;
end;

procedure TSynEditMarkupHighlightAllBase.SendMatchLineInvalidation(AStartIndex, AnEndIndex: Integer
  );
begin
  if (SkipSendingInvalidationLock > 0) then
    exit;

  if FInvalidationStartedWithHiddenMatch then begin
    if FMatches.Count > 1 then begin
      FInvalidationStartedWithHiddenMatch := False;
      inherited SendMatchLineInvalidation(0, FMatches.Count-1);
    end;
  end
  else
    inherited SendMatchLineInvalidation(AStartIndex, AnEndIndex);
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
            ( (not HideSingleMatch) or (Matches.Count > 1) );
end;

procedure TSynEditMarkupHighlightAllBase.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  if (not HasSearchData) then exit;
  if ACountDiff = 0 then
    InvalidateMatches(StartLine, EndLine+1, ACountDiff)
  else
    InvalidateMatches(StartLine, MaxInt, ACountDiff); // LineCount changed

  ValidateMatches;
end;

procedure TSynEditMarkupHighlightAllBase.DoVisibleChanged(AVisible: Boolean);
begin
  inherited DoVisibleChanged(AVisible);
  if FNeedValidate and SynEdit.IsVisible then
    ValidateMatches(True);
end;

function TSynEditMarkupHighlightAllBase.HasVisibleMatch: Boolean;
begin
  Result := HasDisplayAbleMatches;
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
  if SkipPaint then
    BeginSkipSendingInvalidation;
  BeginSendingInvalidation;
  try
    FValidRanges := nil;
    FMatches.Clear;
    InvalidateMatches(1, MaxInt, 0);
    ValidateMatches(SkipPaint);
  finally
    EndSendingInvalidation;
    if SkipPaint then
      EndSkipSendingInvalidation;
  end;
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
  if (not FStateChanged) or (Caret = nil) or (FPaintLock > 0) then
    exit;
  FStateChanged := False;

  t := GetCurrentText;
  if (SearchString = t) and (SearchOptions = GetCurrentOption) then begin
    SearchStringChanged; // Update old bounds
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

procedure TSynEditMarkupHighlightAllCaret.DoMarkupChanged(AMarkup: TLazEditTextAttribute);
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
  If SynEdit.SelAvail then begin
    LowBnd := SynEdit.BlockBegin;
    UpBnd := SynEdit.BlockEnd;
    i := UpBnd.y - LowBnd.y + 1;
    if (i > LowBnd.y) and (i > Lines.Count - UpBnd.y) then
      exit('');
    if FTrim then
      Result := TrimS(SynEdit.SelText)
    else
      Result := SynEdit.SelText;
    if TrimS(Result) = '' then Result := '';
    FLowBound := LowBnd;
    FUpBound := UpBnd;
  end else begin
    Result :=  SynEdit.GetWordAtRowCol(Caret.LineBytePos);
    if FIgnoreKeywords and assigned(FHighlighter)
       and FHighlighter.IsKeyword(Result) then
      Result := '';
    FLowBound.Y := Caret.LinePos;
    FUpBound.Y := Caret.LinePos;
    SynEdit.GetWordBoundsAtRowCol(Caret.LineBytePos, FLowBound.X, FUpBound.X);
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
  If SynEdit.SelAvail or not(FFullWord) then
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
  FScanMode := smsmDirectExtendASync;
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
  CheckState;
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


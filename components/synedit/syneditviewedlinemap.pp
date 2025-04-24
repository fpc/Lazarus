unit SynEditViewedLineMap;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$modeswitch AdvancedRecords}
{$Interfaces CORBA}
{ $INLINE off}

interface

uses
  Classes, SysUtils, LCLProc, LazUTF8, LazListClasses, LazLoggerBase,
  // SynEdit
  SynEditMiscClasses, LazSynEditText, SynEditTypes, SynEditMiscProcs;

const
  SYN_WORD_WRAP_SPLIT_SIZE     = 2500;  // At least  2 * SYN_WORD_WRAP_JOIN_SIZE
  SYN_WORD_WRAP_JOIN_SIZE      =  500;
  SYN_WORD_WRAP_JOIN_DISTANCE  =  300;  // Do NOT join if distance is bigger than this

  SYN_WORD_WRAP_GROW_SIZE  =   16;

type
  TSynWordWrapLineData = Integer;
  PSynWordWrapLineData = ^TSynWordWrapLineData;

  TRemoveFromInvalidListMode = (rfiDefault, rfiForce, rfiMarkAsValidating);

  TSynEditLineMapPage = class;
  TSynLineMapAVLTree = class;

  ISynLineWrapProvider = interface ['{B9569464-1B29-4F7F-BADC-08B7ECD73084}']
    function TextXYToLineXY(ATextXY: TPhysPoint): TViewedSubPoint_0;
    function ViewedSubLineXYToTextX(ARealLine: IntPos; ALineXY: TViewedSubPoint_0): Integer;
  end;

  TSynWordWrapInvalidLinesRecord = record
    First, Last: Integer;
  end;
  PSynWordWrapInvalidLinesRecord = ^TSynWordWrapInvalidLinesRecord;

  { TSynWordWrapInvalidLines }

  TSynWordWrapInvalidLinesRecordSize = specialize TLazListClassesItemSize<TSynWordWrapInvalidLinesRecord>;
  TSynWordWrapInvalidLines = object(specialize TLazShiftBufferListObjBase<PSynWordWrapInvalidLinesRecord, TSynWordWrapInvalidLinesRecordSize>)
  private
    function GetFirstInvalidEndLine: Integer; inline;
    function GetFirstInvalidLine: Integer; inline;
    function GetLastInvalidLine: Integer;
    function GetItem(AnIndex: Integer): TSynWordWrapInvalidLinesRecord; inline;

    function GrowCapacity(ARequired: Integer): Integer;
    function ShrinkCapacity(ARequired: Integer): Integer;
    procedure InsertRows(AIndex, ACount: Integer); inline;
    procedure DeleteRows(AIndex, ACount: Integer); inline;
  public
    function FindIndexFor(ALine: Integer): Integer; // find first Result.First after ALine
    procedure InvalidateLines(AFromOffset, AToOffset: Integer);
    procedure InsertInvalidateLines(AFromOffset, ACount: Integer);
    procedure InsertLines(AFromOffset, ACount: Integer; ASplit: Boolean = False);
    procedure RemoveLines(AFromOffset, ACount: Integer);
    procedure ValidateLines(AnOffset: Integer); inline;
    procedure MoveRangeAtStartTo(var ADestLines: TSynWordWrapInvalidLines; ASourceEndLine, AnAdjust: Integer);
    procedure MoveRangeAtEndTo(var ADestLines: TSynWordWrapInvalidLines; ASourceStartLine, AnAdjust: Integer);
    property FirstInvalidLine: Integer read GetFirstInvalidLine;
    property FirstInvalidEndLine: Integer read GetFirstInvalidEndLine;
    property LastInvalidLine: Integer read GetLastInvalidLine;
    property Item[AnIndex: Integer]: TSynWordWrapInvalidLinesRecord read GetItem;
  end;

  { TSynWordWrapLineMap }

  TSynWordWrapLineMap = class
  private
    FAvlNode: TSynEditLineMapPage;
    FInvalidLines: TSynWordWrapInvalidLines;
    FDeferredAdjustFromOffs, FDeferredAdjustFromVal: Integer;

    (* FWrappedExtraSums:
       Sum of all extra lines due to wrapping up to (and including) the current element.
       If a line wraps and needs 3 viewed-lines, then that is 2 extra lines
       - WrappedOffsetFor(n) => Viewed start of line n
       - FWrappedExtraSums[n] => Viewed start of line n+1
    *)
    FWrappedExtraSumsCount: Integer;
    FWrappedExtraSums: Array of TSynWordWrapLineData; // -1 based
    FOffsetAtStart: Integer;
  private
    function  GetCapacity: Integer; inline;
    procedure SetCapacity(AValue: Integer); inline;
    procedure GrowCapacity(ARequired: Integer);
    procedure ShrinkCapacity;

  private
    function GetViewedCount: Integer;
    function GetViewedRealCountDifference: Integer;
    function GetWrappedExtraSumBefore(ARealOffset: Integer): Integer; inline; // Must have: ARealOffset < FWrappedExtraSumsCount // ingnores FOffsetAtStart

    function GetWrappedOffsetFor(ARealOffset: IntIdx): IntIdx; inline;

    function  GetFirstInvalidLine: Integer; inline;
    function  GetFirstInvalidEndLine: Integer; inline;
    function  GetLastInvalidLine: Integer; inline;
    procedure AddToInvalidList; inline;
    procedure RemoveFromInvalidList(AMode: TRemoveFromInvalidListMode = rfiDefault); inline;
    procedure MaybeUpdateViewedSizeDifference;  inline;

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDumpData: String;

    property AvlNode: TSynEditLineMapPage read FAvlNode;
    property Offset: Integer read FOffsetAtStart;
    property RealCount: Integer read FWrappedExtraSumsCount;
    property ViewedCount: Integer read GetViewedCount;
    property ViewedRealCountDifference: Integer read GetViewedRealCountDifference; // viewed - real


    procedure InvalidateLines(AFromOffset, AToOffset: Integer);
    function  ValidateLine(ALineOffset, AWrappCount: Integer): boolean;
    procedure EndValidate;
    property FirstInvalidLine: Integer read GetFirstInvalidLine;
    property FirstInvalidEndLine: Integer read GetFirstInvalidEndLine;
    property LastInvalidLine: Integer read GetLastInvalidLine;

    procedure InsertLinesAtOffset(ALineOffset, ALineCount: Integer);
    procedure DeleteLinesAtOffset(ALineOffset, ALineCount: Integer; ADoNotShrink: Boolean = False);

    procedure MoveLinesAtStartTo(ADestPage: TSynWordWrapLineMap; ASourceEndLine, ATargetStartLine: Integer);
    procedure MoveLinesAtEndTo(ADestPage: TSynWordWrapLineMap; ASourceStartLine, ALineCount: Integer);

    function GetOffsetForWrap(AViewedOffset: IntIdx; out ASubOffset: IntIdx): IntIdx;
    property WrappedOffsetFor[ARealOffset: IntIdx]: IntIdx read GetWrappedOffsetFor;
  end;

  { TSynEditLineMapPageLinkedList }

  TSynEditLineMapPageLinkedList = class
  private
    FFirstEntry, FLastEntry: TSynEditLineMapPage;
  public
    procedure AddToInvalidList(AnEntry: TSynEditLineMapPage);
    procedure RemoveFromInvalidList(AnEntry: TSynEditLineMapPage; AMode: TRemoveFromInvalidListMode = rfiDefault);
  end;

  { TSynEditLineMapPage }

  TSynEditLineMapPage = class(specialize TGenericSynSizedDifferentialAVLNode<TSynEditLineMapPage>)
  private
    FSynWordWrapLineMap: TSynWordWrapLineMap;
    FSynEditWrappedProvider :ISynLineWrapProvider;
    FTree: TSynLineMapAVLTree;
    FPrevPageWithInvalid, FNextPageWithInvalid: TSynEditLineMapPage;

    procedure UpdateViewedSizeDifference;
    procedure MaybeJoinWithSibling;
  public
    property SynWordWrapLineMapStore: TSynWordWrapLineMap read FSynWordWrapLineMap; experimental; // 'For test case only';
  protected
    function GetWrappedOffsetFor(ARealOffset: IntIdx): IntIdx;
    function GetFirstInvalidLine: Integer;
    function GetFirstInvalidEndLine: Integer;
    function GetLastInvalidLine: Integer;
    function GetViewedRealCountDifference: Integer;

    function IsValid: boolean;
    procedure AddToInvalidList;
    procedure RemoveFromInvalidList(AMode: TRemoveFromInvalidListMode = rfiDefault);

    //property LeftSizeSum; // LeftSizeSum:  Lines after wrap, in nodes to the left
    property Tree: TSynLineMapAVLTree read FTree;

  public
    constructor Create(ATree: TSynLineMapAVLTree; ASynEditWrappedProvider :ISynLineWrapProvider);
    destructor Destroy; override;
    procedure DumpNode(ALine: Integer = 0; AnIndent: Integer = 0);

    property NodeLineOffset: Integer read FPositionOffset write FPositionOffset;    // LineOffset: Line-Number Offset to parent node
    procedure UpdateNodeSize(ANewSize: Integer);

    function CanExtendStartTo(ALineOffs: Integer; AnIgnoreJoinDist: Boolean = False): boolean;
    function CanExtendEndTo(ALineOffs: Integer; AnIgnoreJoinDist: Boolean = False): boolean;

    procedure InsertLinesAtOffset(ALineOffset, ALineCount: IntIdx);
    procedure DeleteLinesAtOffset(ALineOffset, ALineCount: IntIdx; ADoNotShrink: Boolean = False);

    procedure AdjustForLinesInserted(AStartLine, ALineCount: IntIdx; ABytePos: Integer);
    procedure AdjustForLinesDeleted(AStartLine, ALineCount: IntIdx; ABytePos: Integer);

    procedure MoveLinesAtStartTo(ADestPage: TSynEditLineMapPage; ASourceEndLine, ATargetStartLine: Integer);
    procedure MoveLinesAtEndTo(ADestPage: TSynEditLineMapPage; ASourceStartLine, ACount: Integer);

    property FirstInvalidLine: Integer read GetFirstInvalidLine;
    property FirstInvalidEndLine: Integer read GetFirstInvalidEndLine;
    property LastInvalidLine: Integer read GetLastInvalidLine;

    // must be FirstInvalidLine (or Last) => so FirstInvalidLine can be set.
    procedure EndValidate;
    function ValidateLine(ALineOffset, AWrappCount: Integer): boolean;
    procedure InvalidateLines(AFromOffset, AToOffset: Integer); // TODO: adjust offset
    function  ExtendAndInvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx): Boolean;

    function RealCount: Integer; // count of real lines
    function RealStartLine: Integer; // Offset
    function RealEndLine: Integer; // Offset + RealCount - 1;
    property ViewedRealCountDifference: Integer read GetViewedRealCountDifference; // viewed - real

    function GetOffsetForWrap(AWrapOffset: IntIdx; out ASubOffset: IntIdx): IntIdx;
    property WrappedOffsetFor[ARealOffset: IntIdx]: IntIdx read GetWrappedOffsetFor;

    function TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint_0; ANodeStartLine: IntIdx): TViewedPoint_0; virtual;
    function ViewXYIdxToTextXYIdx(AViewXYIdx: TViewedPoint_0; ANodeStartLine: IntIdx): TPhysPoint_0; virtual;
  end;

  TSynEditLineMapPageHolder = specialize TGenericSynSizedDifferentialAVLNodeHolder<TSynEditLineMapPage>;

  { TSynEditLineMapPageHolder }

  { TSynEditLineMapPageHolderHelper }

  TSynEditLineMapPageHolderHelper = record helper for TSynEditLineMapPageHolder
  strict private
    function GetFirstInvalidEndLine: Integer; inline;
    function GetFirstInvalidLine: Integer; inline;
    function GetLastInvalidLine: Integer; inline;
    function GetViewedCountDifferenceBefore: Integer; inline;
    function GetViewedLineCountDifference: Integer;  inline;

  public
    function HasPage: Boolean; inline;
    function CountAvailable(AMaxAllowed: Integer): integer; inline;

    procedure AdjustForLinesInserted(AStartLine, ALineCount, ABytePos: Integer); inline;
    procedure AdjustForLinesDeleted(AStartLine, ALineCount, ABytePos: Integer); inline;

    // APrevPage/ANextPage can be nil, or an already known next page
    procedure SplitNodeToPrev(var APrevPage: TSynEditLineMapPageHolder; ASourceEndLine: Integer);
    procedure SplitNodeToNext(var ANextPage: TSynEditLineMapPageHolder; ASourceStartLine: Integer);
    procedure SplitNodeToNewPrev(out APrevPage: TSynEditLineMapPageHolder; ASourceEndLine: Integer;
                                 ANewPageStartLine: Integer = -1);
    procedure SplitNodeToNewNext(out ANextPage: TSynEditLineMapPageHolder; ASourceStartLine: Integer);

    function CanExtendStartTo(ALineIdx: Integer; AnIgnoreJoinDist: Boolean = False): boolean;
    function CanExtendEndTo(ALineIdx: Integer; AnIgnoreJoinDist: Boolean = False): boolean;

    procedure AdjustPosition(AValue : Integer); // Must not change order with prev/next node

    function ValidateLine(ALineIdx, AWrappCount: Integer): boolean; inline;
    procedure InvalidateLines(AFromIdx, AToIdx: Integer);
    function  ExtendAndInvalidateLines(AFromIdx, AToIdx: TLineIdx): Boolean;

    function NextNodeLine: Integer;

    function RealCount: Integer; // count of real lines
    function RealStartLine: Integer; // Offset
    function RealEndLine: Integer; // Offset + RealCount - 1;

    function GetLineForForWrap(AWrapLine: TLineIdx; out AWrapOffset: TLineIdx): TLineIdx;

    function TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint_0): TViewedPoint_0;
    function ViewXYIdxToTextXYIdx(AViewXYIdx: TViewedPoint_0): TPhysPoint_0;

    property FirstInvalidLine: Integer read GetFirstInvalidLine;
    property FirstInvalidEndLine: Integer read GetFirstInvalidEndLine;
    property LastInvalidLine: Integer read GetLastInvalidLine;

    property ViewedCountDifferenceBefore: Integer read GetViewedCountDifferenceBefore; // viewed - real
    property ViewedLineCountDifference: Integer read GetViewedLineCountDifference;
  end;

  { TSynLineMapAVLTree }

  TSynLineMapAVLTree = class(specialize TGenericSynSizedDifferentialAVLTree<TSynEditLineMapPageHolder, TSynEditLineMapPage>)
  private
    FPageSplitSize: Integer;
    FPageJoinSize, FPageJoinDistance: Integer;
    FCurrentValidatingNode: TSynEditLineMapPageHolder;
    FCurrentValidatingNodeLastLine: Integer;
    FInvalidEntryList: TSynEditLineMapPageLinkedList;

    function GetViewedLineCountDifference: Integer;
    function NextNodeForValidation: TSynEditLineMapPageHolder;
  protected
    function CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode; override;

    (* Find Page by real Line *)
    function FindPageForLine(ALineIdx: IntIdx; AMode: TSynSizedDiffAVLFindMode = afmPrev) : TSynEditLineMapPageHolder;

    //(* Find Fold by wrapped Line  *)
    function FindPageForWrap(AViewedLineIdx: Integer): TSynEditLineMapPageHolder;

  public
    constructor Create;
    constructor Create(APageJoinSize, APageSplitSize, APageJoinDistance: Integer);
    destructor Destroy; override;
    procedure Clear; override;
    procedure DebugDump;

    procedure FreeNode(ANode: TSynEditLineMapPage); inline;
    procedure RemoveNode(ANode: TSynEditLineMapPage); reintroduce; inline;

    property Root: TSynSizedDifferentialAVLNode read FRoot write FRoot;
    property RootOffset : Integer read FRootOffset write FRootOffset;

    function NeedsValidation: Boolean;
    // ValidateLine must only be called AFTER NextBlockForValidation / with no modifications to the tree
    function NextBlockForValidation(out ALowLine, AHighLine: TLineIdx): boolean;
    procedure EndValidate;
    function ValidateLine(ALineIdx: TLineIdx; AWrappCount: Integer): boolean;
    procedure InvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx);

    procedure AdjustForLinesInserted(ALineIdx, ALineCount, ABytePos: Integer); reintroduce;
    procedure AdjustForLinesDeleted(AStartLine, ALineCount, ABytePos: Integer); reintroduce;

    function GetWrapLineForForText(ATextLine: TLineIdx): TLineIdx;
    function GetLineForForWrap(AWrapLine: TLineIdx; out AWrapOffset: TLineIdx): TLineIdx;
    property ViewedLineCountDifference: Integer read GetViewedLineCountDifference;

    function TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint_0): TViewedPoint_0; inline;
    function ViewXYIdxToTextXYIdx(AViewXYIdx: TViewedPoint_0): TPhysPoint_0; inline;

    property PageSplitSize: Integer read FPageSplitSize;
    property PageJoinSize: Integer read FPageJoinSize;
    property PageJoinDistance: Integer read FPageJoinDistance;
  end;


type
  TSynEditLineMappingView = class;

  { TLazSynDisplayLineMapping }

  TLazSynDisplayLineMapping = class abstract(TLazSynDisplayViewEx)
  private
    FCurWrapPage: TSynEditLineMapPageHolder;
  protected
    FCurWrappedLine: TLineIdx;
    FLineMappingView: TSynEditLineMappingView;
    FCurrentWrapSubline: IntIdx;
  public
    constructor Create(AWrappedView: TSynEditLineMappingView);
    procedure SetHighlighterTokensLine(AWrappedLine: TLineIdx; out
      ARealLine: TLineIdx; out ASubLineIdx, AStartBytePos, AStartPhysPos, ALineByteLen: Integer); override;
//    function GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean; override;
    procedure FinishHighlighterTokens; override;
    function TextToViewIndex(ATextIndex: TLineIdx): TLineRange; override;
    function ViewToTextIndex(AViewIndex: TLineIdx): TLineIdx; override;
    function ViewToTextIndexEx(AViewIndex: TLineIdx; out AViewRange: TLineRange): TLineIdx; override;
    function GetLinesCount: Integer; override;
  end;

  { TSynEditLineMappingView }

  TSynEditLineMappingView = class abstract(TSynEditStringsLinked)
  private
    FDisplayFiew: TLazSynDisplayLineMapping;
    FKnownLengthOfLongestLine: integer;
    FLineMappingData: TSynLineMapAVLTree;
    FPaintLock: Integer;
    FNotifyLinesChanged: Boolean;
    FNotifyLinesHandlers: TMethodList;
    procedure DoDecPaintLock(Sender: TObject);
    procedure DoIncPaintLock(Sender: TObject);
    procedure CallLinesChangedHandler; inline;
    procedure LineCountChanged(Sender: TSynEditStrings; aIndex, aCount: Integer);
    procedure LineTextChanged(Sender: TSynEditStrings; aIndex, aCount: Integer);
    Procedure LineEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
  protected
    function GetDisplayView: TLazSynDisplayView; override;
    function GetViewedCount: integer; override;
    procedure SetManager(AManager: TSynTextViewsManager); override;
    function CreateDisplayViewEx: TLazSynDisplayLineMapping; virtual; abstract;
    function CreateLineMappingData: TSynLineMapAVLTree; virtual; abstract;
    property LineMappingData: TSynLineMapAVLTree read FLineMappingData;
  public
    constructor Create;
    destructor Destroy; override;

    function GetLengthOfLongestLine: integer; override;
    function TextToViewIndex(aTextIndex: TLineIdx): TLinePos; override;
    function ViewToTextIndex(aViewIndex: TLineIdx): TLineIdx; override;
    function TextXYToViewXY(APhysTextXY: TPhysPoint): TViewedPoint; override;
    function ViewXYToTextXY(APhysViewXY: TViewedPoint): TPhysPoint; override;
    //function AddVisibleOffsetToTextIndex(aTextIndex: IntIdx; LineOffset: Integer): IntIdx; override; (* Add/Sub to/from TextPos (1-based) skipping folded *)

    procedure InvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx);
    procedure AddLinesChangedHandler(AHandler: TNotifyEvent);
    procedure RemoveLinesChangedHandler(AHandler: TNotifyEvent);
  public
    property KnownLengthOfLongestLine: integer read FKnownLengthOfLongestLine write FKnownLengthOfLongestLine;
  end;


implementation

procedure WrapInfoFillFrom(ATarget: PSynWordWrapLineData; ACount, AValue: Integer); inline;
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do begin
    ATarget^ := AValue;
    inc(ATarget);
  end;
end;

procedure WrapInfoCopyFromTo(ASource, ATarget: PSynWordWrapLineData; ACount: Integer); inline;
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do begin
    ATarget^ := ASource^;
    inc(ASource);
    inc(ATarget);
  end;
end;

procedure WrapInfoMoveUpFromTo(ASource, ATarget: PSynWordWrapLineData; ACount: Integer); inline;
var
  i: Integer;
begin
  inc(ASource, ACount - 1);
  inc(ATarget, ACount - 1);
  for i := 0 to ACount - 1 do begin
    ATarget^ := ASource^;
    dec(ASource);
    dec(ATarget);
  end;
end;

procedure WrapInfoCopyAndAdjustFromTo(ASource, ATarget: PSynWordWrapLineData; ACount, AnAdjustVal: Integer); inline;
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do begin
    ATarget^ := ASource^ + AnAdjustVal;
    inc(ASource);
    inc(ATarget);
  end;
end;

procedure WrapInfoMoveUpAndAdjustFromTo(ASource, ATarget: PSynWordWrapLineData; ACount, AnAdjustVal: Integer); inline;
var
  i: Integer;
begin
  inc(ASource, ACount - 1);
  inc(ATarget, ACount - 1);
  for i := 0 to ACount - 1 do begin
    ATarget^ := ASource^ + AnAdjustVal;
    dec(ASource);
    dec(ATarget);
  end;
end;

{ TSynWordWrapInvalidLines }

function TSynWordWrapInvalidLines.GetFirstInvalidEndLine: Integer;
begin
  if Count = 0 then
    Result := -1
  else
    Result := Item[0].Last;
end;

function TSynWordWrapInvalidLines.GetFirstInvalidLine: Integer;
begin
  if Count = 0 then
    Result := -1
  else
    Result := Item[0].First;
end;

function TSynWordWrapInvalidLines.GetLastInvalidLine: Integer;
var
  i: Integer;
begin
  i := Count;
  if i = 0 then
    Result := -1
  else
    Result := Item[i-1].Last;
end;

function TSynWordWrapInvalidLines.GetItem(AnIndex: Integer): TSynWordWrapInvalidLinesRecord;
begin
  Result := ItemPointer[AnIndex]^;
end;

function TSynWordWrapInvalidLines.GrowCapacity(ARequired: Integer): Integer;
begin
  Result := ARequired + 16;
end;

function TSynWordWrapInvalidLines.ShrinkCapacity(ARequired: Integer): Integer;
begin
  //if ARequired = 0 then
  //  Result := 0
  //else
  if ARequired * 8 < Count then
    Result := ARequired + 4
  else
    Result := -1;
end;

procedure TSynWordWrapInvalidLines.InsertRows(AIndex, ACount: Integer);
begin
  InsertRowsEx(AIndex, ACount, @GrowCapacity);
end;

procedure TSynWordWrapInvalidLines.DeleteRows(AIndex, ACount: Integer);
begin
  DeleteRowsEx(AIndex, ACount, @ShrinkCapacity);
end;

function TSynWordWrapInvalidLines.FindIndexFor(ALine: Integer): Integer;
var
  l, h: integer;
begin
  l := 0;
  h := Count-1;
  if (h < 0) then begin
    Result := 0;
    exit;
  end;

  Result := (l + h) div 2;
  while (h > l) do begin
    if PSynWordWrapInvalidLinesRecord(ItemPointer[Result])^.First >= ALine then
      h := Result
    else
      l := Result + 1;
    Result := cardinal(l + h) div 2;
  end;
  if PSynWordWrapInvalidLinesRecord(ItemPointer[Result])^.First < ALine then
    inc(Result);
end;

procedure TSynWordWrapInvalidLines.InvalidateLines(AFromOffset, AToOffset: Integer);
var
  i, j, c: Integer;
begin
  c := Count;
  i := FindIndexFor(AFromOffset);
  if (i > 0) and (Item[i-1].Last >= AFromOffset-1) then begin
    if (Item[i-1].Last >= AToOffset) then
      exit;
    dec(i);
    PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := AToOffset;
  end
  else
  if (i < c) and (Item[i].First = AToOffset+1) then begin
    PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.First := AFromOffset;
    if AToOffset > PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last then
      PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := AToOffset;
  end
  else begin
    assert((i = 0) or (Item[i-1].Last < AFromOffset-1), 'TSynWordWrapInvalidLines.InvalidateLines: (i = 0) or (Item[i-1].Last < AFromOffset-1)');
    InsertRows(i, 1);
    PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.First := AFromOffset;
    PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := AToOffset;
    inc(c);
  end;
  j := i + 1;
  while j < c do begin
    if (Item[j].Last <= AToOffset) then begin
      DeleteRows(j,1);
      dec(c);
    end
    else
    if (Item[j].First <= AToOffset+1) then begin
      PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := Item[j].Last;
      DeleteRows(j,1);
      dec(c);
    end
    else
      break;
  end;
end;

procedure TSynWordWrapInvalidLines.InsertInvalidateLines(AFromOffset, ACount: Integer);
begin
  InsertLines(AFromOffset, ACount);
  InvalidateLines(AFromOffset, AFromOffset + ACount - 1);
end;

procedure TSynWordWrapInvalidLines.InsertLines(AFromOffset, ACount: Integer; ASplit: Boolean);
var
  i, j: Integer;
begin
  i := Count;
  while i > 0 do begin
    dec(i);
    j := Item[i].First;
    if j >= AFromOffset then begin
      PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.First := j + ACount;
      PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := Item[i].Last + ACount;
    end
    else
    if Item[i].Last >= AFromOffset then begin
      if ASplit then begin
        j := PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last;
        PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := AFromOffset - 1;
        InsertInvalidateLines(AFromOffset+ACount, j-AFromOffset+ 1);
      end
      else
        PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := Item[i].Last + ACount;
    end
    else begin
      assert((Item[i].Last < AFromOffset), 'TSynWordWrapInvalidLines.InsertInvalidateLines: (Item[i].Last < AFromOffset)');
      break;
    end;
  end;
end;

procedure TSynWordWrapInvalidLines.RemoveLines(AFromOffset, ACount: Integer);
var
  i, f, l, k: Integer;
begin
  i := Count;
  while i > 0 do begin
    dec(i);
    f := ItemPointer[i]^.First;
    l := ItemPointer[i]^.Last;
    if f >= AFromOffset then begin
      k := Max(AFromOffset, f - ACount);
      if l - ACount < k then begin
        DeleteRows(i,1);
      end
      else begin
        ItemPointer[i]^.First := k;
        ItemPointer[i]^.Last := l - ACount;
      end;
    end
    else
    if l >= AFromOffset then begin
      k := Max(AFromOffset - 1, l - ACount);
      assert(k >= f, 'TSynWordWrapInvalidLines.RemoveLines: k >= f');
      PSynWordWrapInvalidLinesRecord(ItemPointer[i])^.Last := k;
    end
    else begin
      break;
    end;
  end;
end;

procedure TSynWordWrapInvalidLines.ValidateLines(AnOffset: Integer);
var
  i, c: Integer;
begin
  assert((AnOffset >= 0) and (AnOffset = FirstInvalidLine) or (AnOffset = LastInvalidLine), 'TSynWordWrapInvalidLines.ValidateLines: (AnOffset >= 0) and (AnOffset = FirstInvalidLine) or (AnOffset = LastInvalidLine)');
  i := Item[0].First;
  if AnOffset = i then begin
    if i < Item[0].Last then
      PSynWordWrapInvalidLinesRecord(ItemPointer[0])^.First := i + 1
    else
      DeleteRows(0,1);
  end
  else begin
    c := Count-1;
    i := Item[c].last;
    if i > Item[c].First then
      PSynWordWrapInvalidLinesRecord(ItemPointer[c])^.Last := i - 1
    else
      DeleteRows(c,1);
  end;
end;

procedure TSynWordWrapInvalidLines.MoveRangeAtStartTo(
  var ADestLines: TSynWordWrapInvalidLines; ASourceEndLine, AnAdjust: Integer);
var
  i, c, ItemFirst, ItemLast: Integer;
  DelTo: Integer;
begin
  DelTo := -1;
  c := Count;
  i := 0;
  while i < c do begin
    ItemFirst := ItemPointer[i]^.First;
    ItemLast := ItemPointer[i]^.Last;
    if ItemLast <= ASourceEndLine then begin
      ADestLines.InvalidateLines(ItemFirst + AnAdjust, ItemLast + AnAdjust);
      DelTo := i;
    end
    else
    if ItemFirst <= ASourceEndLine then begin
      ADestLines.InvalidateLines(ItemFirst + AnAdjust, ASourceEndLine + AnAdjust);
      ItemPointer[i]^.First := ASourceEndLine;
      break;
    end
    else
      break;
    inc(i);
  end;
  if DelTo >= 0 then
    DeleteRows(0, DelTo + 1);
end;

procedure TSynWordWrapInvalidLines.MoveRangeAtEndTo(
  var ADestLines: TSynWordWrapInvalidLines; ASourceStartLine, AnAdjust: Integer);
var
  i, ItemFirst, ItemLast: Integer;
  DelFrom: Integer;
begin
  i := Count;
  DelFrom := i;
  while i > 0 do begin
    dec(i);
    ItemFirst := ItemPointer[i]^.First;
    ItemLast := ItemPointer[i]^.Last;
    if ItemFirst >= ASourceStartLine then begin
      ADestLines.InvalidateLines(ItemFirst + AnAdjust, ItemLast + AnAdjust);
      DelFrom := i;
    end
    else
    if ItemLast >= ASourceStartLine then begin
      ADestLines.InvalidateLines(ASourceStartLine + AnAdjust, ItemLast + AnAdjust);
      ItemPointer[i]^.Last := ASourceStartLine - 1;
      break;
    end
    else
      break;
  end;
  if DelFrom < Count then
    DeleteRows(DelFrom, Count - DelFrom);
end;

{ TSynWordWrapLineMap }

function TSynWordWrapLineMap.GetCapacity: Integer;
begin
  Result := Length(FWrappedExtraSums);
end;

procedure TSynWordWrapLineMap.SetCapacity(AValue: Integer);
begin
  if AValue < FWrappedExtraSumsCount then
    AValue := FWrappedExtraSumsCount;
  SetLength(FWrappedExtraSums, AValue);
end;

procedure TSynWordWrapLineMap.GrowCapacity(ARequired: Integer);
begin
  if Capacity < ARequired then
    Capacity := ARequired + SYN_WORD_WRAP_GROW_SIZE;
end;

procedure TSynWordWrapLineMap.ShrinkCapacity;
var
  i: Integer;
begin
  i := 0;
  while (i < FWrappedExtraSumsCount) and (FWrappedExtraSums[i] = 0) do
    inc(i);
  if i > 0 then begin
    assert(i<=FWrappedExtraSumsCount, 'TSynWordWrapLineMap.ShrinkCapacity: i<=FWrappedExtraSumsCount');
    WrapInfoCopyFromTo(
      {$PUSH}{$R-}
      @FWrappedExtraSums[i],
      {$POP}
      @FWrappedExtraSums[0],
      FWrappedExtraSumsCount-i);
    FOffsetAtStart := FOffsetAtStart + i;
    FWrappedExtraSumsCount := FWrappedExtraSumsCount - i;
  end;

  if Capacity > FWrappedExtraSumsCount + 2 * SYN_WORD_WRAP_GROW_SIZE then
    Capacity := FWrappedExtraSumsCount + SYN_WORD_WRAP_GROW_SIZE;
end;

function TSynWordWrapLineMap.GetViewedCount: Integer;
begin
  assert((FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0), 'TSynWordWrapLineMap.GetViewedCount: (FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0)');
  Result := FWrappedExtraSumsCount;
  if FWrappedExtraSumsCount > 0 then
    Result := Result + FWrappedExtraSums[FWrappedExtraSumsCount - 1];
end;

function TSynWordWrapLineMap.GetViewedRealCountDifference: Integer;
begin
  assert((FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0), 'TSynWordWrapLineMap.GetViewedRealCountDifference: (FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0)');
  if FWrappedExtraSumsCount > 0 then
    Result := FWrappedExtraSums[FWrappedExtraSumsCount - 1]
  else
    Result := 0;
end;

function TSynWordWrapLineMap.GetWrappedExtraSumBefore(ARealOffset: Integer
  ): Integer;
begin
  if ARealOffset = 0 then
    Result := 0
  else
    Result := FWrappedExtraSums[ARealOffset-1];
end;

function TSynWordWrapLineMap.GetWrappedOffsetFor(ARealOffset: IntIdx): IntIdx;
begin
  Result := ARealOffset;
  if ARealOffset <= FOffsetAtStart then
    exit;
  ARealOffset := ARealOffset - FOffsetAtStart;
  if ARealOffset > FWrappedExtraSumsCount then begin
    if FWrappedExtraSumsCount > 0 then
      Result := Result + FWrappedExtraSums[FWrappedExtraSumsCount - 1];
  end
  else
  if ARealOffset > 0 then
    Result := Result + FWrappedExtraSums[ARealOffset - 1];
end;

procedure TSynWordWrapLineMap.AddToInvalidList;
begin
  FAvlNode.AddToInvalidList;
end;

procedure TSynWordWrapLineMap.RemoveFromInvalidList(
  AMode: TRemoveFromInvalidListMode);
begin
  FAvlNode.RemoveFromInvalidList(AMode);
end;

procedure TSynWordWrapLineMap.MaybeUpdateViewedSizeDifference;
begin
  if FirstInvalidLine < 0 then
    FAvlNode.UpdateViewedSizeDifference;
end;

procedure TSynWordWrapLineMap.InvalidateLines(AFromOffset,
  AToOffset: Integer);
begin
  assert((FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0), 'TSynWordWrapLineMap.InvalidateLines: (FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0)');
  FInvalidLines.InvalidateLines(AFromOffset, AToOffset);
  AddToInvalidList;
end;

function TSynWordWrapLineMap.ValidateLine(ALineOffset, AWrappCount: Integer): boolean;
var
  i, j, AdjustedLineOffset: Integer;
begin
  assert(ALineOffset >= 0, 'TSynWordWrapLineMap.ValidateLine: ALineOffset >= 0');
  assert((FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0), 'TSynWordWrapLineMap.ValidateLine: (FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0)');
  Result := True; // assume it was changed
  FInvalidLines.ValidateLines(ALineOffset);

  AdjustedLineOffset := ALineOffset - FOffsetAtStart;
  if FDeferredAdjustFromOffs > 0 then begin
    if AdjustedLineOffset < FDeferredAdjustFromOffs then begin
      EndValidate;
    end
    else begin
      j := FDeferredAdjustFromVal;
      //AWrappCount := AWrappCount + j;
      for i := FDeferredAdjustFromOffs to Min(FWrappedExtraSumsCount - 1, AdjustedLineOffset) do
        FWrappedExtraSums[i] := FWrappedExtraSums[i] + j;
      if AdjustedLineOffset < FWrappedExtraSumsCount - 1 then
        FDeferredAdjustFromOffs := AdjustedLineOffset + 1
      else
        FDeferredAdjustFromOffs := 0;
    end;
  end;


  if ALineOffset < FOffsetAtStart then begin
    assert(FWrappedExtraSumsCount>0, 'TSynWordWrapLineMap.ValidateLine: FWrappedExtraSumsCount>0');
    if AWrappCount <> 1 then begin
      i := FirstInvalidLine;
      if (i < 0) or (ALineOffset < i) then
        i := ALineOffset;
      j := FOffsetAtStart - i;
      assert(j>0, 'TSynWordWrapLineMap.ValidateLine: j>0');
      GrowCapacity(FWrappedExtraSumsCount + j);
      WrapInfoMoveUpAndAdjustFromTo(
        @FWrappedExtraSums[0],
        @FWrappedExtraSums[j],
        FWrappedExtraSumsCount,
        AWrappCount - 1);
      assert((ALineOffset-i)<=FWrappedExtraSumsCount, 'TSynWordWrapLineMap.ValidateLine: (ALineOffset-i)<=FWrappedExtraSumsCount');
      assert((ALineOffset-i)>=0, 'TSynWordWrapLineMap.ValidateLine: (ALineOffset-i)>=0');
      WrapInfoFillFrom(
        {$PUSH}{$R-}
        @FWrappedExtraSums[ALineOffset - i],
        {$POP}
        j - (ALineOffset - i),
        AWrappCount - 1);

      assert(FOffsetAtStart >= j, 'TSynWordWrapLineMap.ValidateLine: FOffsetAtStart >= j');
      FOffsetAtStart := FOffsetAtStart - j;
      FWrappedExtraSumsCount := FWrappedExtraSumsCount + j;
    end
    else
      Result := False; // wrap-count did not change
  end

  else
  begin
    if AdjustedLineOffset >= FWrappedExtraSumsCount then begin
      if AWrappCount > 1 then begin
        i := FWrappedExtraSumsCount;
  // TODO: check LastInvalidLine and SYN_WORD_WRAP_SPLIT_SIZE;
        GrowCapacity(AdjustedLineOffset + 1);
        FWrappedExtraSumsCount := AdjustedLineOffset + 1;
        WrapInfoFillFrom(
          @FWrappedExtraSums[i],
          FWrappedExtraSumsCount - i,
          GetWrappedExtraSumBefore(i));
        FWrappedExtraSums[AdjustedLineOffset] := AWrappCount - 1; // last element, no AdjustFrom() needed
        if AdjustedLineOffset > 0 then
          FWrappedExtraSums[AdjustedLineOffset] := FWrappedExtraSums[AdjustedLineOffset] + FWrappedExtraSums[AdjustedLineOffset-1];
      end
      else
        Result := False; // wrap-count did not change
    end

    else
    if (AWrappCount = 1) and (AdjustedLineOffset = FWrappedExtraSumsCount - 1) then begin
      // removing the last (tailing) wrap entry from list
      // that must be wrapped, so result=true / there was a change
      if AdjustedLineOffset = 0 then begin
        FWrappedExtraSumsCount := 0;
        FOffsetAtStart := 0;
      end
      else begin
// TODO: defer if there are further invalid lines after AdjustedLineOffset
        i := AdjustedLineOffset - 1; // i = FWrappedExtraSumsCount - 2
        j := FWrappedExtraSums[i];
        if j = 0 then begin
          FWrappedExtraSumsCount := 0;
          FOffsetAtStart := 0;
        end
        else begin
          dec(i);
          while (i >= 0) and (j = FWrappedExtraSums[i]) do
            dec(i);
          FWrappedExtraSumsCount := i + 2;
        end;
      end;
    end

    else
    begin
      assert((FDeferredAdjustFromOffs=0) or (FDeferredAdjustFromOffs=AdjustedLineOffset+1), 'TSynWordWrapLineMap.ValidateLine: (FDeferredAdjustFromOffs=0) or (FDeferredAdjustFromOffs=AdjustedLineOffset+1)');
      j := AWrappCount - 1 + GetWrappedExtraSumBefore(AdjustedLineOffset);
      i := j - FWrappedExtraSums[AdjustedLineOffset];
      Result := i <> 0;
      if Result then begin
        FDeferredAdjustFromVal  := FDeferredAdjustFromVal + i;
        FDeferredAdjustFromOffs := AdjustedLineOffset + 1;
        FWrappedExtraSums[AdjustedLineOffset] := j;
      end;
    end;
  end;
end;

procedure TSynWordWrapLineMap.EndValidate;
var
  v, i: Integer;
begin
  if FDeferredAdjustFromOffs > 0 then begin
    v := FDeferredAdjustFromVal;
    for i := FDeferredAdjustFromOffs to FWrappedExtraSumsCount - 1 do
      FWrappedExtraSums[i] := FWrappedExtraSums[i] + v;
  end;
  FDeferredAdjustFromOffs := 0;
  FDeferredAdjustFromVal  := 0;

  if (FInvalidLines.Count = 0) then begin
    FAvlNode.UpdateViewedSizeDifference;
    RemoveFromInvalidList;
    ShrinkCapacity;
  end;
end;

procedure TSynWordWrapLineMap.MoveLinesAtStartTo(ADestPage: TSynWordWrapLineMap;
  ASourceEndLine, ATargetStartLine: Integer);
var
  MinLineCount, TrgO1: Integer;
  W: TSynWordWrapLineData;
begin
  assert(ATargetStartLine >= ADestPage.FWrappedExtraSumsCount + ADestPage.FOffsetAtStart, 'TSynWordWrapLineMap.InsertLinesFromPage: ATargetStartLine > ADestPage.FWrappedExtraSumsCount + ADestPage.FOffsetAtStart');

  FInvalidLines.MoveRangeAtStartTo(ADestPage.FInvalidLines, ASourceEndLine, ATargetStartLine);

  if (FWrappedExtraSumsCount = 0) then
    exit;

  ASourceEndLine := ASourceEndLine - Offset;
  if ASourceEndLine < 0 then
    exit;

  if (ASourceEndLine > 0) and (ASourceEndLine < FWrappedExtraSumsCount) then begin
    W := FWrappedExtraSums[ASourceEndLine];
    while (ASourceEndLine > 0) and
          (FWrappedExtraSums[ASourceEndLine - 1] = W)
    do
      dec(ASourceEndLine);
  end;

  if ADestPage.FWrappedExtraSumsCount = 0 then begin
    // Target page is empty
    ADestPage.FOffsetAtStart := ATargetStartLine + Offset;
    assert(ADestPage.FOffsetAtStart >= 0, 'TSynWordWrapLineMap.MoveLinesAtStartTo: ADestPage.FOffsetAtStart >= 0');

    MinLineCount := Min(ASourceEndLine+1, FWrappedExtraSumsCount);
    ADestPage.GrowCapacity(MinLineCount);
    WrapInfoCopyFromTo(
      @FWrappedExtraSums[0],
      @ADestPage.FWrappedExtraSums[0],
      MinLineCount);
    ADestPage.FWrappedExtraSumsCount := MinLineCount;
    ADestPage.MaybeUpdateViewedSizeDifference;
    exit;
  end;

  ATargetStartLine  := ATargetStartLine + Offset - ADestPage.FOffsetAtStart;
  MinLineCount := Min(ASourceEndLine+1, FWrappedExtraSumsCount);
  TrgO1 := ADestPage.GetWrappedExtraSumBefore(ADestPage.FWrappedExtraSumsCount);

  ADestPage.GrowCapacity(ATargetStartLine + MinLineCount);
  if ATargetStartLine > ADestPage.FWrappedExtraSumsCount then begin
    WrapInfoFillFrom(
      @ADestPage.FWrappedExtraSums[ADestPage.FWrappedExtraSumsCount],
      ATargetStartLine - ADestPage.FWrappedExtraSumsCount,
      TrgO1);
  end;

  WrapInfoCopyAndAdjustFromTo(
    @FWrappedExtraSums[0],
    @ADestPage.FWrappedExtraSums[ATargetStartLine],
    MinLineCount,
    TrgO1);
  ADestPage.FWrappedExtraSumsCount := ATargetStartLine + MinLineCount;
  ADestPage.MaybeUpdateViewedSizeDifference;
end;

procedure TSynWordWrapLineMap.MoveLinesAtEndTo(ADestPage: TSynWordWrapLineMap;
  ASourceStartLine, ALineCount: Integer);
var
  OldOffset, SrcO1, SrcO2, MinLineCount: Integer;
  W: TSynWordWrapLineData;
begin
  assert(ASourceStartLine-FOffsetAtStart+ALineCount >= FWrappedExtraSumsCount, 'TSynWordWrapLineMap.MoveLinesAtEndTo: ASourceStartLine+ACount >= FWrappedExtraSumsCount');

  ADestPage.FInvalidLines.InsertLines(0, ALineCount);
  FInvalidLines.MoveRangeAtEndTo(ADestPage.FInvalidLines, ASourceStartLine, -ASourceStartLine);

  if (FWrappedExtraSumsCount = 0) then begin
    if ADestPage.FWrappedExtraSumsCount = 0 then
      exit;
    ADestPage.FOffsetAtStart := ADestPage.FOffsetAtStart + ALineCount;
    assert(ADestPage.FOffsetAtStart >= 0, 'TSynWordWrapLineMap.MoveLinesAtEndTo: ADestPage.FOffsetAtStart >= 0');
    exit;
  end;

  OldOffset := ADestPage.FOffsetAtStart;
  ADestPage.FOffsetAtStart := 0;
  if ASourceStartLine < Offset then begin
    ADestPage.FOffsetAtStart := Offset - ASourceStartLine;
    ASourceStartLine := 0;
    ALineCount := ALineCount - ADestPage.FOffsetAtStart;

    if ALineCount = 0 then begin
      ADestPage.FOffsetAtStart := ADestPage.FOffsetAtStart + OldOffset;
      assert(ADestPage.FOffsetAtStart >= 0, 'TSynWordWrapLineMap.MoveLinesAtEndTo: ADestPage.FOffsetAtStart >= 0');
      exit;
    end;
  end
  else
    ASourceStartLine := ASourceStartLine - Offset;


  if (ASourceStartLine > 0) and (ASourceStartLine < FWrappedExtraSumsCount) then begin
    SrcO2 := ASourceStartLine;
    W := FWrappedExtraSums[ASourceStartLine - 1];
    while (ASourceStartLine < FWrappedExtraSumsCount) and
          (FWrappedExtraSums[ASourceStartLine] = W)
    do
      inc(ASourceStartLine);
    ALineCount := ALineCount + SrcO2 - ASourceStartLine;
    ADestPage.FOffsetAtStart := ADestPage.FOffsetAtStart + ASourceStartLine - SrcO2;
    if ALineCount <= 0 then
      exit;
  end;


  SrcO1 := GetWrappedExtraSumBefore(Min(ASourceStartLine,              FWrappedExtraSumsCount));
  MinLineCount := Max(0, Min(ALineCount, FWrappedExtraSumsCount - ASourceStartLine));

  if ADestPage.FWrappedExtraSumsCount = 0 then begin
    // Moving to an empty page. Do NOT include any lines after FWrappedExtraSumsCount
    if MinLineCount > 0 then begin;
      ADestPage.GrowCapacity(MinLineCount);
      assert(ASourceStartLine<=FWrappedExtraSumsCount, 'TSynWordWrapLineMap.MoveLinesAtEndTo: ASourceStartLine<=FWrappedExtraSumsCount');
      WrapInfoCopyAndAdjustFromTo(
        @FWrappedExtraSums[ASourceStartLine],
        @ADestPage.FWrappedExtraSums[0],
        MinLineCount,
        -SrcO1);
    end;
    ADestPage.FWrappedExtraSumsCount := MinLineCount;
    ADestPage.MaybeUpdateViewedSizeDifference;
    exit;
  end;

  SrcO2 := GetWrappedExtraSumBefore(Min(ASourceStartLine + ALineCount, FWrappedExtraSumsCount));
  ADestPage.GrowCapacity(ADestPage.FWrappedExtraSumsCount + ALineCount + OldOffset);
  WrapInfoMoveUpAndAdjustFromTo(
    @ADestPage.FWrappedExtraSums[0],
    @ADestPage.FWrappedExtraSums[ALineCount + OldOffset],
    ADestPage.FWrappedExtraSumsCount,
    SrcO2 - SrcO1);
  if MinLineCount > 0 then begin
    assert(ASourceStartLine+MinLineCount<=FWrappedExtraSumsCount, 'TSynWordWrapLineMap.MoveLinesAtEndTo: ASourceStartLine+MinLineCount<=FWrappedExtraSumsCount');
    WrapInfoCopyAndAdjustFromTo(
      @FWrappedExtraSums[ASourceStartLine],
      @ADestPage.FWrappedExtraSums[0],
      MinLineCount,
      -SrcO1);
  end;
  WrapInfoFillFrom(
    @ADestPage.FWrappedExtraSums[MinLineCount],
    ALineCount - MinLineCount + OldOffset,
    SrcO2 - SrcO1);
  ADestPage.FWrappedExtraSumsCount := ADestPage.FWrappedExtraSumsCount + ALineCount + OldOffset;
  ADestPage.MaybeUpdateViewedSizeDifference;
end;

procedure TSynWordWrapLineMap.InsertLinesAtOffset(ALineOffset,
  ALineCount: Integer);
var
  j, k: Integer;
begin
  assert((FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0), 'TSynWordWrapLineMap.InsertLinesAtOffset: (FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0)');
  if ALineCount = 0 then
    exit;

  FInvalidLines.InsertInvalidateLines(ALineOffset, ALineCount);
  AddToInvalidList;

  if ALineOffset <= FOffsetAtStart then begin
    if FWrappedExtraSumsCount > 0 then
      FOffsetAtStart := FOffsetAtStart + ALineCount;
    assert(FOffsetAtStart >= 0, 'TSynWordWrapLineMap.MoveLinesAtEndTo: FOffsetAtStart >= 0');
    exit;
  end;
  ALineOffset := ALineOffset - FOffsetAtStart;

  if ALineOffset < FWrappedExtraSumsCount then begin
    GrowCapacity(FWrappedExtraSumsCount + ALineCount);
    move(FWrappedExtraSums[ALineOffset], FWrappedExtraSums[ALineOffset+ALineCount],
      sizeof(FWrappedExtraSums[0]) * (FWrappedExtraSumsCount - ALineOffset));
    FWrappedExtraSumsCount := FWrappedExtraSumsCount + ALineCount;
    j := GetWrappedExtraSumBefore(ALineOffset);
    for k := ALineOffset to ALineOffset + ALineCount - 1 do
      FWrappedExtraSums[k] := j;
    // TODO: only if NO invalid lines?
    FAvlNode.UpdateViewedSizeDifference;
  end;
end;

procedure TSynWordWrapLineMap.DeleteLinesAtOffset(ALineOffset,
  ALineCount: Integer; ADoNotShrink: Boolean);
var
  i, j, AMoveCount: Integer;
begin
  assert((FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0), 'TSynWordWrapLineMap.DeleteLinesAtOffset: (FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0)');
  if ALineCount = 0 then
    exit;
  FInvalidLines.RemoveLines(ALineOffset, ALineCount);

  if ALineOffset < FOffsetAtStart then begin
    i := Min(ALineCount, FOffsetAtStart - ALineOffset);
    FOffsetAtStart := FOffsetAtStart -i;
    assert(FOffsetAtStart >= 0, 'TSynWordWrapLineMap.MoveLinesAtEndTo: FOffsetAtStart >= 0');
    ALineCount := ALineCount - i;
    if ALineCount = 0 then
      exit;
    ALineOffset := ALineOffset - FOffsetAtStart;
  end
  else
    ALineOffset := ALineOffset - FOffsetAtStart;

  if ALineOffset < FWrappedExtraSumsCount then begin
    ALineCount := Min(ALineCount, FWrappedExtraSumsCount - ALineOffset);
    AMoveCount := FWrappedExtraSumsCount - (ALineCount + ALineOffset);
    assert(ALineCount>0, 'TSynWordWrapLineMap.DeleteLinesAtOffset: ALineCount>0');
    assert(ALineOffset+ALineCount<=FWrappedExtraSumsCount, 'TSynWordWrapLineMap.DeleteLinesAtOffset: ALineOffset+ALineCount<=FWrappedExtraSumsCount');
    if AMoveCount > 0 then
      WrapInfoCopyAndAdjustFromTo(
        @FWrappedExtraSums[ALineOffset + ALineCount],
        @FWrappedExtraSums[ALineOffset],
        AMoveCount,
        GetWrappedExtraSumBefore(ALineOffset) - FWrappedExtraSums[ALineOffset + ALineCount - 1] );
    FWrappedExtraSumsCount := FWrappedExtraSumsCount - ALineCount;

    if (ALineOffset > 0) and (ALineOffset = FWrappedExtraSumsCount) then begin
      i := FWrappedExtraSumsCount - 1;
      j := FWrappedExtraSums[i];
      dec(i);
      while (i >= 0) and (j = FWrappedExtraSums[i]) do
        dec(i);
      inc(i);

      if i < LastInvalidLine then
        i := LastInvalidLine;

      inc(i);
      if i < FWrappedExtraSumsCount then begin
        FWrappedExtraSumsCount := i;
      end;
    end;

    if FWrappedExtraSumsCount = 0 then
      FOffsetAtStart := 0;
  end;

  if (FInvalidLines.Count = 0) then begin
    ShrinkCapacity;
    RemoveFromInvalidList;
  end;

  // TODO: only if NO invalid lines?
  FAvlNode.UpdateViewedSizeDifference;
end;

function TSynWordWrapLineMap.GetOffsetForWrap(AViewedOffset: IntIdx; out
  ASubOffset: IntIdx): IntIdx;
var
  l, h: Integer;
begin
  Result := 0;

  ASubOffset := 0;
  if (FWrappedExtraSumsCount = 0) or (AViewedOffset <= FOffsetAtStart) then
    exit(AViewedOffset);
  AViewedOffset := AViewedOffset - FOffsetAtStart;
  if AViewedOffset >= FWrappedExtraSums[FWrappedExtraSumsCount - 1] + FWrappedExtraSumsCount then
    exit(AViewedOffset - FWrappedExtraSums[FWrappedExtraSumsCount - 1] + FOffsetAtStart);

  l := 0;
  h := FWrappedExtraSumsCount - 1;
  Result := FWrappedExtraSumsCount div 2;
  while h > l do begin
    if FWrappedExtraSums[Result]+Result >= AViewedOffset then
      h := Result
    else
      l := Result + 1;
    Result := (h+l) div 2;
  end;
  assert(h=l, 'TSynWordWrapLineMap.GetOffsetForWrap: h=l');

  if Result = 0 then
    ASubOffset := AViewedOffset
  else
    ASubOffset := AViewedOffset - FWrappedExtraSums[Result - 1] - Result;
  Result := Result + FOffsetAtStart;
end;

constructor TSynWordWrapLineMap.Create;
begin
  FInvalidLines.Create;
  inherited Create;
end;

destructor TSynWordWrapLineMap.Destroy;
begin
  FInvalidLines.Destroy;
  inherited Destroy;
end;

function TSynWordWrapLineMap.GetDumpData: String;
begin
  Result := format('Offs=%3d  Rlcnt=%4d  VCnt=%4d  Diff=%3d   Inval=%3d..%3d // ', [FOffsetAtStart, RealCount, ViewedCount, ViewedRealCountDifference, GetFirstInvalidLine, GetLastInvalidLine]);
  if FWrappedExtraSumsCount = 1 then
    Result := Result + IntToStr(FWrappedExtraSums[0])
  else if FWrappedExtraSumsCount = 2 then
    Result := Result + IntToStr(FWrappedExtraSums[0]) + ', ' + IntToStr(FWrappedExtraSums[1])
  else if FWrappedExtraSumsCount > 2 then
    Result := Result + IntToStr(FWrappedExtraSums[0]) + ' / ' +
              IntToStr(FWrappedExtraSums[FWrappedExtraSumsCount-2]) + ', ' + IntToStr(FWrappedExtraSums[FWrappedExtraSumsCount-1])
end;

function TSynWordWrapLineMap.GetFirstInvalidLine: Integer;
begin
  Result := FInvalidLines.FirstInvalidLine;
end;

function TSynWordWrapLineMap.GetFirstInvalidEndLine: Integer;
begin
  Result := FInvalidLines.FirstInvalidEndLine;
end;

function TSynWordWrapLineMap.GetLastInvalidLine: Integer;
begin
  Result := FInvalidLines.LastInvalidLine;
end;

{ TSynEditLineMapPageLinkedList }

procedure TSynEditLineMapPageLinkedList.AddToInvalidList(
  AnEntry: TSynEditLineMapPage);
begin
  if (AnEntry.FNextPageWithInvalid = nil) and (FLastEntry <> AnEntry)
  then begin
    if FLastEntry = nil then begin
      FFirstEntry := AnEntry;
      FLastEntry := AnEntry;
    end
    else begin
      AnEntry.FPrevPageWithInvalid := FLastEntry;
      AnEntry.FPrevPageWithInvalid.FNextPageWithInvalid := AnEntry;
      FLastEntry := AnEntry;
    end;
  end;
end;

procedure TSynEditLineMapPageLinkedList.RemoveFromInvalidList(
  AnEntry: TSynEditLineMapPage; AMode: TRemoveFromInvalidListMode);
begin
  if AnEntry.FNextPageWithInvalid = AnEntry then begin
    AnEntry.FNextPageWithInvalid := nil;
    exit;
  end;

  if FFirstEntry = AnEntry then begin
    FFirstEntry := AnEntry.FNextPageWithInvalid;
    if AnEntry.FNextPageWithInvalid <> nil then
      AnEntry.FNextPageWithInvalid.FPrevPageWithInvalid := nil
    else
      FLastEntry := nil;
  end
  else
  if AnEntry.FPrevPageWithInvalid <> nil then begin
    AnEntry.FPrevPageWithInvalid.FNextPageWithInvalid := AnEntry.FNextPageWithInvalid;
    if AnEntry.FNextPageWithInvalid <> nil then
      AnEntry.FNextPageWithInvalid.FPrevPageWithInvalid := AnEntry.FPrevPageWithInvalid
    else
      FLastEntry := AnEntry.FPrevPageWithInvalid;
  end;
  if (AMode = rfiMarkAsValidating) then
    AnEntry.FNextPageWithInvalid := AnEntry //// TODO: XXXXXXXXXXXXXXXXXXXXXXXX
  else
    AnEntry.FNextPageWithInvalid := nil;
  AnEntry.FPrevPageWithInvalid := nil;
end;

{ TSynEditLineMapPage }

procedure TSynEditLineMapPage.UpdateViewedSizeDifference;
begin
  UpdateNodeSize(FSynWordWrapLineMap.ViewedRealCountDifference);
end;

procedure TSynEditLineMapPage.MaybeJoinWithSibling;
var
  dummy, NextLineOffs, PrevLineOffs, NextLineDist, PrevLineDist, c: Integer;
  NextPage, PrevPage: TSynEditLineMapPage;
begin
  if (FSynWordWrapLineMap.FirstInvalidLine >= 0) then
    exit;
  if (RealCount = 0) and (FSize = 0) then begin
    Tree.FreeNode(Self);
  end
  else
  if (RealCount <= Tree.PageJoinSize) then begin
    NextLineOffs := 0;
    dummy := 0;
    NextPage := Successor(NextLineOffs, dummy);
    if NextPage <> nil then begin
      assert(NextLineOffs > RealEndLine, 'TSynWordWrapIndexPage.MaybeJoinWithSibling: NextLineOffs > RealEndLine');
      NextLineDist := NextLineOffs - (RealEndLine+1) + NextPage.RealStartLine;
      c := NextPage.RealCount;
      if ( (c <> 0) and (NextLineDist > Tree.PageJoinDistance) ) or
         (c > Tree.PageJoinSize) or
         (NextPage.FirstInvalidLine >= 0) or
         (not NextPage.CanExtendStartTo(-NextLineOffs + RealStartLine, True))
      then
        NextLineOffs := 0;
    end
    else
      NextLineOffs := 0;

    PrevLineOffs := 0;
    dummy := 0;
    PrevPage := Precessor(PrevLineOffs, dummy);
    if PrevPage <> nil then begin
      PrevLineOffs := -PrevLineOffs;
      assert(PrevLineOffs > PrevPage.RealEndLine, 'TSynWordWrapIndexPage.MaybeJoinWithSibling: -PrevLineOffs > PrevPage.RealEndLine');
      PrevLineDist := PrevLineOffs + RealStartLine - (PrevPage.RealEndLine+1);
      c := PrevPage.RealCount;
      if ( (c <> 0) and (PrevLineDist> Tree.PageJoinDistance) ) or
         (c > Tree.PageJoinSize) or
         (PrevPage.FirstInvalidLine >= 0) or
         (not PrevPage.CanExtendEndTo(PrevLineOffs + RealEndLine, True))
      then
        PrevLineOffs := 0;
    end
    else
      PrevLineOffs := 0;

  if (NextLineOffs > 0) and
     ( (PrevLineOffs = 0) or ({%H-}PrevLineDist > {%H-}NextLineDist) )
  then begin
    MoveLinesAtEndTo(NextPage, 0, NextLineOffs);
    Tree.FreeNode(Self);
    NextPage.AdjustPosition(-NextLineOffs);
  end
  else
  if (PrevLineOffs > 0)
  then begin
    MoveLinesAtStartTo(PrevPage, RealEndLine, PrevLineOffs);
    Tree.FreeNode(Self);
  end;

  end;
end;

function TSynEditLineMapPage.GetWrappedOffsetFor(ARealOffset: IntIdx): IntIdx;
begin
  Result := FSynWordWrapLineMap.GetWrappedOffsetFor(ARealOffset);
end;

function TSynEditLineMapPage.GetFirstInvalidLine: Integer;
begin
  Result := FSynWordWrapLineMap.FirstInvalidLine;
end;

function TSynEditLineMapPage.GetFirstInvalidEndLine: Integer;
begin
  Result := FSynWordWrapLineMap.FirstInvalidEndLine;
end;

function TSynEditLineMapPage.GetLastInvalidLine: Integer;
begin
  Result := FSynWordWrapLineMap.LastInvalidLine;
end;

function TSynEditLineMapPage.GetViewedRealCountDifference: Integer;
begin
  Result := FSynWordWrapLineMap.ViewedRealCountDifference;
end;

function TSynEditLineMapPage.IsValid: boolean;
begin
  Result := FSynWordWrapLineMap.FInvalidLines.Count = 0;
end;

procedure TSynEditLineMapPage.AddToInvalidList;
begin
  if not IsValid then
    Tree.FInvalidEntryList.AddToInvalidList(Self);
end;

procedure TSynEditLineMapPage.RemoveFromInvalidList(
  AMode: TRemoveFromInvalidListMode);
begin
  if IsValid or (AMode in [rfiForce, rfiMarkAsValidating]) then
    Tree.FInvalidEntryList.RemoveFromInvalidList(Self, AMode);
end;

constructor TSynEditLineMapPage.Create(ATree: TSynLineMapAVLTree;
  ASynEditWrappedProvider: ISynLineWrapProvider);
begin
  FTree := ATree;
  FSynEditWrappedProvider := ASynEditWrappedProvider;
  FSynWordWrapLineMap := TSynWordWrapLineMap.Create;
  FSynWordWrapLineMap.FAvlNode := Self;
  inherited Create;
end;

destructor TSynEditLineMapPage.Destroy;
begin
  FSynWordWrapLineMap.Destroy;
  inherited Destroy;
end;

procedure TSynEditLineMapPage.DumpNode(ALine: Integer; AnIndent: Integer);
var
  s: String;
begin
  s:= StringOfChar(' ', AnIndent)+IntToHex(AnIndent,1);
  ALine := ALine + NodeLineOffset;
  if Left <> nil then Left.DumpNode(ALine, AnIndent+1);
  DebugLn('%-10s WRAP  LnOffs=%5d LINE=%5d   LSzSum==%4d LineCnt=%4d   Sz=%3d %s', [
    s,
    NodeLineOffset, ALine, LeftSizeSum, RealCount, FSize,
    FSynWordWrapLineMap.GetDumpData
  ]);
  if Right <> nil then Right.DumpNode(ALine, AnIndent+1);
end;

procedure TSynEditLineMapPage.UpdateNodeSize(ANewSize: Integer);
begin
  if FSize <> ANewSize then begin
    AdjustParentLeftCount(ANewSize -FSize);
    FSize := ANewSize;
  end;
end;

function TSynEditLineMapPage.CanExtendStartTo(ALineOffs: Integer; AnIgnoreJoinDist: Boolean
  ): boolean;
begin
  Result := (RealEndLine - ALineOffs < Tree.PageSplitSize) and
            (AnIgnoreJoinDist or (RealStartLine - ALineOffs < Tree.PageJoinDistance));
end;

function TSynEditLineMapPage.CanExtendEndTo(ALineOffs: Integer; AnIgnoreJoinDist: Boolean): boolean;
begin
  Result := (ALineOffs - RealStartLine < Tree.PageSplitSize) and
            (AnIgnoreJoinDist or (ALineOffs - RealEndLine < Tree.PageJoinDistance));
end;

procedure TSynEditLineMapPage.InsertLinesAtOffset(ALineOffset,
  ALineCount: IntIdx);
begin
  FSynWordWrapLineMap.InsertLinesAtOffset(ALineOffset, ALineCount);
end;

procedure TSynEditLineMapPage.DeleteLinesAtOffset(ALineOffset,
  ALineCount: IntIdx; ADoNotShrink: Boolean);
begin
  FSynWordWrapLineMap.DeleteLinesAtOffset(ALineOffset, ALineCount, ADoNotShrink);
end;

procedure TSynEditLineMapPage.AdjustForLinesInserted(AStartLine,
  ALineCount: IntIdx; ABytePos: Integer);
var
  rs, re, LineOffs, dummy, Cnt: Integer;
  NextPage, PrevPage: TSynEditLineMapPage;
begin
  assert(AStartLine >= 0, 'TSynWordWrapIndexPage.AdjustForLinesInserted: AStartLine >= 0');

  rs := RealStartLine;
  re := RealEndLine;

  if (AStartLine <= rs) or (AStartLine > re) or
     (re - rs + 1 + ALineCount <= Tree.PageSplitSize)
  then begin
    InsertLinesAtOffset(AStartLine, ALineCount);
    if AStartLine = 0 then
      AdjustPosition(-ALineCount);
    exit;
  end;

  (* This node was NOT moved by the callers AdjustForLinesInserted.
     This would only have happened if AStartLine = 0
  *)
  assert(RealCount > 0, 'TSynWordWrapIndexPage.InsertLines: RealCount > 0');

  if AStartLine > rs + (re-rs) div 2 then begin
    // try split to next
    LineOffs := 0;
    dummy := 0;
    NextPage := Successor(LineOffs, dummy);
    if (NextPage<>nil) and NextPage.CanExtendStartTo(AStartLine + ALineCount - LineOffs) then begin
      //CurrentPage.SplitNodeToNext(NextPage, AStartLine);
      Cnt := LineOffs - (AStartLine + ALineCount);
      MoveLinesAtEndTo(NextPage, AStartLine, Cnt);
      NextPage.AdjustPosition(-Cnt);
      //CurrentPage.InsertLinesAtIndex(AStartLine, ACount);
      InsertLinesAtOffset(AStartLine, ALineCount);
      exit;
    end;
    LineOffs := 0;
    dummy := 0;
    PrevPage := Precessor(LineOffs, dummy);
    if (PrevPage<>nil) and PrevPage.CanExtendEndTo(AStartLine - 1  - LineOffs) then begin
      //CurrentPage.SplitNodeToPrev(PrevPage, AStartLine - 1);
      MoveLinesAtStartTo(PrevPage, AStartLine - 1, -LineOffs);
      AdjustPosition(AStartLine + ALineCount);
      //PrevPage.InsertLinesAtIndex(AStartLine, ACount);
      PrevPage.InsertLinesAtOffset(-LineOffs + AStartLine, ALineCount);
      exit;
    end;
    //CurrentPage.SplitNodeToNewNext(NextPage, AStartLine);
    NextPage := Tree.FindPageForLine(GetPosition + AStartLine + ALineCount, afmCreate).Page;
    MoveLinesAtEndTo(NextPage, AStartLine, Max(LastInvalidLine, RealCount));  // May be bigger than needed....
    //CurrentPage.InsertLinesAtIndex(AStartLine, ACount);
    InsertLinesAtOffset(AStartLine, ALineCount);
  end
  else begin
    // try split to prev
    LineOffs := 0;
    dummy := 0;
    PrevPage := Precessor(LineOffs, dummy);

    if (PrevPage<>nil) and PrevPage.CanExtendEndTo(AStartLine - 1 - LineOffs) then begin
      //CurrentPage.SplitNodeToPrev(PrevPage, AStartLine - 1);
      MoveLinesAtStartTo(PrevPage, AStartLine - 1, -LineOffs);
      AdjustPosition(AStartLine + ALineCount);
      //PrevPage.InsertLinesAtIndex(AStartLine, ACount);
      PrevPage.InsertLinesAtOffset(-LineOffs + AStartLine, ALineCount);
      exit;
    end;
    LineOffs := 0;
    dummy := 0;
    NextPage := Successor(LineOffs, dummy);
    if (NextPage<>nil) and NextPage.CanExtendStartTo(AStartLine + ALineCount - LineOffs) then begin
      //CurrentPage.SplitNodeToNext(NextPage, AStartLine { + ALineCount});
      Cnt := LineOffs - (AStartLine + ALineCount);
      MoveLinesAtEndTo(NextPage, AStartLine, Cnt);
      NextPage.AdjustPosition(-Cnt);
      //CurrentPage.InsertLinesAtIndex(AStartLine, ACount);
      InsertLinesAtOffset(AStartLine, ALineCount);
      exit;
    end;
    //CurrentPage.SplitNodeToNewPrev(PrevPage, AStartLine - 1);
    dummy := GetPosition;
    AdjustPosition(AStartLine + ALineCount);
    PrevPage := Tree.FindPageForLine(dummy, afmCreate).Page;
    MoveLinesAtStartTo(PrevPage, AStartLine - 1, 0);
    //PrevPage.InsertLinesAtIndex(AStartLine, ACount);
    PrevPage.InsertLinesAtOffset(AStartLine, ALineCount);
  end;
end;

procedure TSynEditLineMapPage.AdjustForLinesDeleted(AStartLine,
  ALineCount: IntIdx; ABytePos: Integer);
begin
  DeleteLinesAtOffset(AStartLine, ALineCount);
  MaybeJoinWithSibling;
end;

procedure TSynEditLineMapPage.MoveLinesAtStartTo(
  ADestPage: TSynEditLineMapPage; ASourceEndLine, ATargetStartLine: Integer);
begin
  assert(ADestPage is TSynEditLineMapPage, 'TSynEditLineMapPage.MoveLinesAtStartTo: ADestPage is TSynEditLineMapPage');
  FSynWordWrapLineMap.MoveLinesAtStartTo(TSynEditLineMapPage(ADestPage).FSynWordWrapLineMap, ASourceEndLine, ATargetStartLine);
  FSynWordWrapLineMap.DeleteLinesAtOffset(0, ASourceEndLine + 1);
end;

procedure TSynEditLineMapPage.MoveLinesAtEndTo(ADestPage: TSynEditLineMapPage;
  ASourceStartLine, ACount: Integer);
begin
  assert(ADestPage is TSynEditLineMapPage, 'TSynEditLineMapPage.MoveLinesAtEndTo: ADestPage is TSynEditLineMapPage');
  FSynWordWrapLineMap.MoveLinesAtEndTo(TSynEditLineMapPage(ADestPage).FSynWordWrapLineMap, ASourceStartLine, ACount);
  FSynWordWrapLineMap.DeleteLinesAtOffset(ASourceStartLine, ACount);
end;

procedure TSynEditLineMapPage.EndValidate;
begin
  FSynWordWrapLineMap.EndValidate;
  MaybeJoinWithSibling;
end;

function TSynEditLineMapPage.ValidateLine(ALineOffset, AWrappCount: Integer): boolean;
begin
  Result := FSynWordWrapLineMap.ValidateLine(ALineOffset, AWrappCount);
end;

procedure TSynEditLineMapPage.InvalidateLines(AFromOffset, AToOffset: Integer);
begin
  FSynWordWrapLineMap.InvalidateLines(AFromOffset, AToOffset);
end;

function TSynEditLineMapPage.ExtendAndInvalidateLines(AFromLineIdx,
  AToLineIdx: TLineIdx): Boolean;
begin
  Result := True;
  if AFromLineIdx < 0 then begin
    AdjustPosition(AFromLineIdx);
    InsertLinesAtOffset(0, -AFromLineIdx);
    AToLineIdx := AToLineIdx - AFromLineIdx;
    AFromLineIdx := 0;
  end;
  InvalidateLines(AFromLineIdx, AToLineIdx);
end;

function TSynEditLineMapPage.RealCount: Integer;
begin
  Result := FSynWordWrapLineMap.RealCount;
end;

function TSynEditLineMapPage.RealStartLine: Integer;
begin
  Result := FSynWordWrapLineMap.Offset;
end;

function TSynEditLineMapPage.RealEndLine: Integer;
begin
  Result := FSynWordWrapLineMap.Offset + FSynWordWrapLineMap.RealCount - 1;
end;

function TSynEditLineMapPage.GetOffsetForWrap(AWrapOffset: IntIdx; out ASubOffset: IntIdx): IntIdx;
begin
  Result := FSynWordWrapLineMap.GetOffsetForWrap(AWrapOffset, ASubOffset);
end;

function TSynEditLineMapPage.TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint_0;
  ANodeStartLine: IntIdx): TViewedPoint_0;
begin
  Result := FSynEditWrappedProvider.TextXYToLineXY(ATextXYIdx);
  Result.y := Result.y + ANodeStartLine + GetWrappedOffsetFor(ATextXYIdx.y - ANodeStartLine);
end;

function TSynEditLineMapPage.ViewXYIdxToTextXYIdx(AViewXYIdx: TViewedPoint_0;
  ANodeStartLine: IntIdx): TPhysPoint_0;
var
  SubOffset: Integer;
begin
  Result.y := ANodeStartLine + GetOffsetForWrap(AViewXYIdx.y - ANodeStartLine, SubOffset);
  Result.x := FSynEditWrappedProvider.ViewedSubLineXYToTextX(Result.y, Point(AViewXYIdx.x, SubOffset) );
end;

{ TSynEditLineMapPageHolderHelper }

function TSynEditLineMapPageHolderHelper.GetFirstInvalidLine: Integer;
begin
  if Page <> nil then begin
    Result := Page.FirstInvalidLine;
    if Result >= 0 then
      Result := Result + StartLine;
  end
  else
    Result := -1;
end;

function TSynEditLineMapPageHolderHelper.GetFirstInvalidEndLine: Integer;
begin
  if Page <> nil then begin
    Result := Page.FirstInvalidEndLine;
    if Result >= 0 then
      Result := Result + StartLine;
  end
  else
    Result := -1;
end;

function TSynEditLineMapPageHolderHelper.GetLastInvalidLine: Integer;
begin
  if Page <> nil then begin
    Result := Page.LastInvalidLine;
    if Result >= 0 then
      Result := Result + StartLine;
  end
  else
    Result := -1;
end;

function TSynEditLineMapPageHolderHelper.GetViewedCountDifferenceBefore: Integer;
begin
  Result := LeftSizeSum;
end;

function TSynEditLineMapPageHolderHelper.GetViewedLineCountDifference: Integer;
begin
  Result := 0;
  if Page <> nil then
    Result := Page.ViewedRealCountDifference;
end;

function TSynEditLineMapPageHolderHelper.HasPage: Boolean;
begin
  Result := Page <> nil;
end;

function TSynEditLineMapPageHolderHelper.CountAvailable(AMaxAllowed: Integer): integer;
begin
  if Page = nil then
    Result := 0
  else
    Result := AMaxAllowed - Page.RealCount;
  assert(Result >= 0, 'TSynEditLineMapPageHolderHelper.CountAvailable: Result >= 0');
end;

procedure TSynEditLineMapPageHolderHelper.AdjustForLinesInserted(AStartLine,
  ALineCount, ABytePos: Integer);
begin
  Page.AdjustForLinesInserted(AStartLine - StartLine, ALineCount, ABytePos);
end;

procedure TSynEditLineMapPageHolderHelper.AdjustForLinesDeleted(AStartLine,
  ALineCount, ABytePos: Integer);
begin
  Page.AdjustForLinesDeleted(AStartLine - StartLine, ALineCount, ABytePos);
end;

procedure TSynEditLineMapPageHolderHelper.SplitNodeToPrev(
  var APrevPage: TSynEditLineMapPageHolder; ASourceEndLine: Integer);
var
  ELine: Integer;
begin
  assert(ASourceEndLine >= StartLine, 'TSynEditLineMapPageHolderHelper.SplitNodeToPrev: ASourceEndLine > StartLine');
  if not APrevPage.HasPage then
    APrevPage := Prev;

  if not APrevPage.HasPage then begin
    SplitNodeToNewPrev(APrevPage, ASourceEndLine, StartLine);
    exit;
  end;

  assert(APrevPage.StartLine < StartLine, 'TSynEditLineMapPageHolderHelper.SplitNodeToPrev: APrevPage.StartLine < StartLine');
  ELine := ASourceEndLine - StartLine;
  Page.MoveLinesAtStartTo(
    APrevPage.Page,
    ELine,
    StartLine - APrevPage.StartLine);
  AdjustPosition(ELine + 1);
  assert(APrevPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolderHelper.SplitNodeToPrev: APrevPage.RealCount <= FPageSplitSize');
end;

procedure TSynEditLineMapPageHolderHelper.SplitNodeToNext(
  var ANextPage: TSynEditLineMapPageHolder; ASourceStartLine: Integer);
var
  Cnt: Integer;
begin
  assert(ASourceStartLine >= StartLine, 'TSynEditLineMapPageHolderHelper.SplitNodeToNext: ASourceStartLine > StartLine');
  if not ANextPage.HasPage then
    ANextPage := Next;

  if not ANextPage.HasPage then begin
    SplitNodeToNewNext(ANextPage, ASourceStartLine);
    exit;
  end;

  assert(ANextPage.StartLine > ASourceStartLine, 'TSynEditLineMapPageHolderHelper.SplitNodeToNext: ANextPage.StartLine > ASourceStartLine');
  Cnt := ANextPage.StartLine - ASourceStartLine;
  Page.MoveLinesAtEndTo(
    ANextPage.Page,
    ASourceStartLine - StartLine,
    Cnt);
  ANextPage.AdjustPosition(-Cnt);
  assert(ANextPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolderHelper.SplitNodeToNext: ANextPage.RealCount <= FPageSplitSize');
end;

procedure TSynEditLineMapPageHolderHelper.SplitNodeToNewPrev(out
  APrevPage: TSynEditLineMapPageHolder; ASourceEndLine: Integer;
  ANewPageStartLine: Integer);
var
  SLine, ELine, Offs: Integer;
begin
  assert(ASourceEndLine >= StartLine, 'TSynEditLineMapPageHolderHelper.SplitNodeToNewPrev: ASourceEndLine > StartLine');
  assert(ANewPageStartLine <= StartLine, 'TSynEditLineMapPageHolderHelper.SplitNodeToNewPrev: ANewPageStartLine <= StartLine');

  if ANewPageStartLine >= 0 then begin
    SLine := ANewPageStartLine;
    Offs := StartLine - ANewPageStartLine;
  end
  else begin
    SLine := StartLine;
    Offs := 0;
  end;
  ELine := ASourceEndLine - StartLine;
  AdjustPosition(ELine + 1);

  APrevPage := Page.FTree.FindPageForLine(SLine, afmCreate);
  Page.MoveLinesAtStartTo(
    APrevPage.Page,
    ELine,
    Offs);
  assert(APrevPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolderHelper.SplitNodeToPrev: APrevPage.RealCount <= Page.FTree.FPageSplitSize');
end;

procedure TSynEditLineMapPageHolderHelper.SplitNodeToNewNext(out
  ANextPage: TSynEditLineMapPageHolder; ASourceStartLine: Integer);
begin
  assert(ASourceStartLine > StartLine, 'TSynEditLineMapPageHolderHelper.SplitNodeToNewNext: ASourceStartLine > StartLine');

  ANextPage := Page.FTree.FindPageForLine(ASourceStartLine, afmCreate);

  Page.MoveLinesAtEndTo(
    ANextPage.Page,
    ASourceStartLine - StartLine,
    Max(Page.LastInvalidLine, Page.RealCount));  // May be bigger than needed....
  assert(ANextPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolderHelper.SplitNodeToNext: ANextPage.RealCount <= Page.FTree.FPageSplitSize');
end;

function TSynEditLineMapPageHolderHelper.CanExtendStartTo(ALineIdx: Integer;
  AnIgnoreJoinDist: Boolean): boolean;
begin
  // TODO: if the node has invalid lines in the offset or behind, then those may need to be included?
  Result := HasPage and
            (RealEndLine - ALineIdx < Page.FTree.FPageSplitSize) and
            (AnIgnoreJoinDist or (RealStartLine - ALineIdx < Page.FTree.FPageJoinDistance));
end;

function TSynEditLineMapPageHolderHelper.CanExtendEndTo(ALineIdx: Integer;
  AnIgnoreJoinDist: Boolean): boolean;
begin
  Result := HasPage and
            (ALineIdx - RealStartLine < Page.FTree.FPageSplitSize) and
            (AnIgnoreJoinDist or (ALineIdx - RealEndLine < Page.FTree.FPageJoinDistance));
end;

procedure TSynEditLineMapPageHolderHelper.AdjustPosition(AValue: Integer);
begin
  assert(HasPage, 'TSynEditLineMapPageHolderHelper.AdjustPosition: HasPage');
  Page.AdjustPosition(AValue);
  _ChangeStartLine(StartLine + AValue);
  //StartLine := StartLine + AValue;
end;

procedure TSynEditLineMapPageHolderHelper.InvalidateLines(AFromIdx, AToIdx: Integer);
begin
  assert(HasPage, 'TSynEditLineMapPageHolderHelper.InvalidateLines: HasPage');
  Page.InvalidateLines(AFromIdx-StartLine, AToIdx-StartLine);
end;

function TSynEditLineMapPageHolderHelper.ExtendAndInvalidateLines(AFromIdx,
  AToIdx: TLineIdx): Boolean;
begin
  Result := HasPage;
  if Result then
    Result := Page.ExtendAndInvalidateLines(AFromIdx-StartLine, AToIdx-StartLine);
end;

function TSynEditLineMapPageHolderHelper.ValidateLine(ALineIdx, AWrappCount: Integer): boolean;
begin
  Result := Page.ValidateLine(ALineIdx-StartLine, AWrappCount);
end;

function TSynEditLineMapPageHolderHelper.RealCount: Integer;
begin
  if HasPage then
    Result := Page.RealCount
  else
    Result := 0;
end;

function TSynEditLineMapPageHolderHelper.RealStartLine: Integer;
begin
  Result := StartLine + Page.RealStartLine;
end;

function TSynEditLineMapPageHolderHelper.RealEndLine: Integer;
begin
  Result := StartLine + Page.RealEndLine;
end;

function TSynEditLineMapPageHolderHelper.GetLineForForWrap(AWrapLine: TLineIdx; out
  AWrapOffset: TLineIdx): TLineIdx;
begin
  if (not HasPage) or (StartLine + ViewedCountDifferenceBefore > AWrapLine) then begin
    // no wrapped line before AWrapLine;
    Result := AWrapLine;
    AWrapOffset := 0;
    exit;
  end;

  Result := StartLine + Page.GetOffsetForWrap(AWrapLine - StartLine - ViewedCountDifferenceBefore, AWrapOffset);
end;

function TSynEditLineMapPageHolderHelper.TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint_0
  ): TViewedPoint_0;
begin
  Result := ATextXYIdx;
  if not HasPage then
    exit;
  Result := Page.TextXYIdxToViewXYIdx(ATextXYIdx, StartLine);
  Result.y := Result.y + ViewedCountDifferenceBefore;
end;

function TSynEditLineMapPageHolderHelper.ViewXYIdxToTextXYIdx(AViewXYIdx: TViewedPoint_0
  ): TPhysPoint_0;
begin
  Result := AViewXYIdx;
  if not HasPage then
    exit;
  Result.y := Result.y - ViewedCountDifferenceBefore;
  Result := Page.ViewXYIdxToTextXYIdx(Result, StartLine);
end;

function TSynEditLineMapPageHolderHelper.NextNodeLine: Integer;
begin
  assert(HasPage, 'TSynEditLineMapPageHolderHelper.NextNodeLinet: HasPage');
  Result := StartLine + RealCount;
end;

{ TSynLineMapAVLTree }

function TSynLineMapAVLTree.CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := TSynEditLineMapPage.Create(Self, nil);
end;

procedure TSynLineMapAVLTree.FreeNode(ANode: TSynEditLineMapPage);
begin
  RemoveNode(ANode);
  DisposeNode(TSynSizedDifferentialAVLNode(ANode));
end;

procedure TSynLineMapAVLTree.RemoveNode(ANode: TSynEditLineMapPage);
begin
  if (FInvalidEntryList <> nil) and
     ( (ANode.FPrevPageWithInvalid <> nil) or (ANode.FNextPageWithInvalid <> nil) or
       (FInvalidEntryList.FFirstEntry = ANode)
     )
  then
    FInvalidEntryList.RemoveFromInvalidList(ANode, rfiForce);
  inherited RemoveNode(ANode);
  ANode.FTree := nil;
end;

procedure TSynLineMapAVLTree.AdjustForLinesInserted(ALineIdx, ALineCount,
  ABytePos: Integer);
var
  CurrentPage: TSynEditLineMapPageHolder;
begin
  CurrentPage := FindPageForLine(ALineIdx, afmPrev);

  // inherited => moves any node with FStartLine >= AStartLine
  inherited AdjustForLinesInserted(ALineIdx, ALineCount);

  // CurrentPage.StartLine is still the old value
  if CurrentPage.HasPage then
    CurrentPage.AdjustForLinesInserted(ALineIdx, ALineCount, ABytePos)
  else
    InvalidateLines(ALineIdx, ALineIdx+ALineCount-1);
end;

procedure TSynLineMapAVLTree.AdjustForLinesDeleted(AStartLine, ALineCount,
  ABytePos: Integer);
var
  TmpPage, NextPage, FirstTmpPage: TSynEditLineMapPageHolder;
  CurLine, CountRemaining, d, LineAfter: Integer;
begin
  CurLine := AStartLine;
  CountRemaining := ALineCount;

  TmpPage := FindPageForLine(AStartLine, afmPrev);
  if not TmpPage.HasPage then begin
    TmpPage := FirstPage;
    if not TmpPage.HasPage then
      exit;
    d := TmpPage.StartLine - CurLine;
    CurLine := CurLine + d;
    CountRemaining := CountRemaining - d;
  end;

  if TmpPage.StartLine < AStartLine then begin // keep some
    FirstTmpPage := TmpPage;
    if CurLine + CountRemaining < FirstTmpPage.NextNodeLine then
      TmpPage.ClearData
    else
      TmpPage := TmpPage.Next;
    assert((not TmpPage.HasPage) or (TmpPage.StartLine > AStartLine), 'TSynLineMapAVLTree.AdjustForLinesDeleted: TmpPage.StartLine > AStartLine');
  end
  else
    {%H-}FirstTmpPage.ClearData;

  LineAfter := AStartLine + ALineCount;
  while TmpPage.HasPage and (LineAfter >= TmpPage.NextNodeLine) do begin
    NextPage := TmpPage.Next;
    FreeNode(TmpPage.Page);
    TmpPage := NextPage;
  end;

  if FirstTmpPage.HasPage then begin
    FirstTmpPage.AdjustForLinesDeleted(CurLine, CountRemaining, ABytePos);
  end;

  d := LineAfter - TmpPage.StartLine;
  if TmpPage.HasPage and (d > 0) then begin
    TmpPage.AdjustPosition(AStartLine - TmpPage.StartLine); // Only happens if "AStartLine - TmpPage.StartLine" is negative;
    inherited AdjustForLinesDeleted(AStartLine, ALineCount);

    TmpPage.AdjustForLinesDeleted(TmpPage.StartLine, d, 0);
  end
  else
    inherited AdjustForLinesDeleted(AStartLine, ALineCount);
end;

constructor TSynLineMapAVLTree.Create;
begin
  Create(SYN_WORD_WRAP_JOIN_SIZE, SYN_WORD_WRAP_SPLIT_SIZE, SYN_WORD_WRAP_JOIN_DISTANCE);
end;

constructor TSynLineMapAVLTree.Create(APageJoinSize, APageSplitSize,
  APageJoinDistance: Integer);
begin
  assert(APageSplitSize >= 2* APageJoinSize, 'TSynLineMapAVLTree.Create: APageSplitSize >= 2* APageJoinSize');
  FPageSplitSize := APageSplitSize;
  FPageJoinSize  := APageJoinSize;
  FPageJoinDistance := APageJoinDistance;
  FInvalidEntryList := TSynEditLineMapPageLinkedList.Create;
end;

destructor TSynLineMapAVLTree.Destroy;
begin
  inherited Destroy;
  FInvalidEntryList.Destroy;
end;

procedure TSynLineMapAVLTree.Clear;
begin
  inherited Clear;
  FInvalidEntryList.FFirstEntry := nil;
  FInvalidEntryList.FLastEntry := nil;
end;

procedure TSynLineMapAVLTree.DebugDump;
begin
  if FRoot <> nil then TSynEditLineMapPage(FRoot).DumpNode;
end;

function TSynLineMapAVLTree.FindPageForLine(ALineIdx: IntIdx;
  AMode: TSynSizedDiffAVLFindMode): TSynEditLineMapPageHolder;
var
  r: TSynEditLineMapPage;
  rStartLine, rWrappedBefore: Integer;
begin
  r := TSynEditLineMapPage(FindNodeAtPosition(ALineIdx, AMode, rStartLine, rWrappedBefore));
  Result{%H-}.Init(r, rStartLine, rWrappedBefore);
end;

function TSynLineMapAVLTree.FindPageForWrap(AViewedLineIdx: Integer): TSynEditLineMapPageHolder;
var
  nd, PrevNd: TSynEditLineMapPage;
  rRealStartLine, rSumViewedSizesBefore: Integer;
  PrvRealStartLine, PrvSumViewedSizesBefore, NewSumSizes: Integer;
begin
  Result{%H-}.ClearData;

  PrevNd := nil;
  PrvRealStartLine := 0;
  PrvSumViewedSizesBefore := 0;
  nd := TSynEditLineMapPage(FRoot);
  rRealStartLine := 0;
  rSumViewedSizesBefore := 0;
  if nd = nil then
    exit;

  while true do begin
    rRealStartLine := rRealStartLine + nd.FPositionOffset;
    NewSumSizes := rSumViewedSizesBefore + nd.LeftSizeSum;

    if AViewedLineIdx < rRealStartLine + NewSumSizes then begin
      nd := nd.Left;
      if nd = nil then begin
        nd := PrevNd;
        rRealStartLine := PrvRealStartLine;
        rSumViewedSizesBefore := PrvSumViewedSizesBefore;
        break;
      end;
      continue;
    end;

    rSumViewedSizesBefore := NewSumSizes;

    if AViewedLineIdx = rRealStartLine + rSumViewedSizesBefore then
      break;

    if nd.Right = nil then
      break;

    PrevNd := nd;
    PrvRealStartLine := rRealStartLine;
    PrvSumViewedSizesBefore := rSumViewedSizesBefore;

    rSumViewedSizesBefore := rSumViewedSizesBefore + nd.FSize;
    nd := nd.Right;
  end;

  Result.Init(nd, rRealStartLine, rSumViewedSizesBefore);
end;

function TSynLineMapAVLTree.NeedsValidation: Boolean;
begin
  Result := FInvalidEntryList.FFirstEntry <> nil;
end;

function TSynLineMapAVLTree.NextBlockForValidation(out ALowLine,
  AHighLine: TLineIdx): boolean;
begin
  if FCurrentValidatingNode.HasPage then begin
    ALowLine := FCurrentValidatingNode.FirstInvalidLine;
    if ALowLine >= 0 then begin
      Result := True;
      AHighLine := FCurrentValidatingNode.FirstInvalidEndLine;
      FCurrentValidatingNodeLastLine := -1; // If there was a next node, inval lines would be on that next node, and not on this
      exit;
    end;
    FCurrentValidatingNode.Page.EndValidate;
    FCurrentValidatingNode.ClearData;
  end;

  repeat
    Result := FInvalidEntryList.FFirstEntry <> nil;
    if not Result then
      exit;

    FCurrentValidatingNode := NextNodeForValidation; // TODO: directly fill FCurrentValidatingNode;
    Result := FCurrentValidatingNode.HasPage;
    assert(result, 'TSynLineMapAVLTree.NextBlockForValidation: result');
    ALowLine  := FCurrentValidatingNode.FirstInvalidLine;
    AHighLine := FCurrentValidatingNode.FirstInvalidEndLine;
    FCurrentValidatingNodeLastLine := -1; // If there was a next node, inval lines would be on that next node, and not on this
  until ALowLine >= 0;
end;

procedure TSynLineMapAVLTree.EndValidate;
begin
  if FCurrentValidatingNode.HasPage then begin
    FCurrentValidatingNode.Page.EndValidate;
    FCurrentValidatingNode.ClearData;
  end;
end;

function TSynLineMapAVLTree.ValidateLine(ALineIdx: TLineIdx; AWrappCount: Integer): boolean;
var
  RealStart, RealEnd: Integer;
  SblRealStart: Integer;
  NewScrEnd: Integer;
  SiblingNode: TSynEditLineMapPageHolder;
begin
  Result := True;
  assert(FCurrentValidatingNode.HasPage, 'TSynLineMapAVLTree.ValidateLine: FCurrentValidatingNode.HasPage');
  assert(ALineIdx >= FCurrentValidatingNode.StartLine, 'TSynLineMapAVLTree.ValidateLine: ALineIdx >= FCurrentValidatingNode.StartLine');

  if (FCurrentValidatingNodeLastLine > 0) and (ALineIdx > FCurrentValidatingNodeLastLine) then begin
    FCurrentValidatingNode.Page.EndValidate;
    FCurrentValidatingNode := FindPageForLine(ALineIdx);
    assert(FCurrentValidatingNode.HasPage, 'TSynLineMapAVLTree.ValidateLine: FCurrentValidatingNode.HasPage');
  end;

  RealStart := FCurrentValidatingNode.RealStartLine;
  RealEnd := FCurrentValidatingNode.RealEndLine;

  if ( (ALineIdx > RealEnd   - FPageSplitSize) and  // max extended start
       (ALineIdx < RealStart + FPageSplitSize)      // max extended end
     ) or
     (AWrappCount = 1)
  then begin
    Result := FCurrentValidatingNode.ValidateLine(ALineIdx, AWrappCount);
    exit;
  end;

  // need splitting
  FCurrentValidatingNode.Page.EndValidate;
  {%H-}SiblingNode.ClearData;

  if (ALineIdx < RealStart) then begin
    SiblingNode := FCurrentValidatingNode.Prev;
    if SiblingNode.CanExtendEndTo(ALineIdx) then begin
      // split to prev node
      SblRealStart := SiblingNode.RealStartLine;
      if RealStart - 1 - SblRealStart < FPageSplitSize then
        NewScrEnd := RealStart - 1 // entire FOffsetAtStart
      else
        NewScrEnd := ALineIdx + (FPageSplitSize - (ALineIdx - SblRealStart)) div 2; // additional space: (FPageSplitSize - SblRealStartDiff)
      assert(NewScrEnd < RealStart, 'TSynLineMapAVLTree.ValidateLine: NewScrEnd < RealStartLine');
      FCurrentValidatingNode.SplitNodeToPrev(SiblingNode, NewScrEnd);
    end
    else begin
      // split to new PREV node
      NewScrEnd := RealStart - 1;
      if NewScrEnd - ALineIdx >= FPageSplitSize then
        NewScrEnd := ALineIdx + FPageSplitSize;
      FCurrentValidatingNode.SplitNodeToNewPrev(SiblingNode, NewScrEnd);
    end;
    FCurrentValidatingNodeLastLine := NewScrEnd; // This may be BEFORE "RealEnd + FPageSplitSize"
  end

  else
  if (ALineIdx > RealEnd) then begin
    SiblingNode := FCurrentValidatingNode.Next;
    if SiblingNode.CanExtendStartTo(ALineIdx) then begin
      // split to next node
      FCurrentValidatingNode.SplitNodeToNext(SiblingNode, ALineIdx);
    end
    else begin
      // new node
      FCurrentValidatingNode.SplitNodeToNewNext(SiblingNode, ALineIdx);
    end;
  end;

  assert(SiblingNode.HasPage, 'TSynLineMapAVLTree.ValidateLine: SiblingNode.HasPage');
  FCurrentValidatingNode.Page.EndValidate;
  FCurrentValidatingNode := SiblingNode;

  Result := FCurrentValidatingNode.ValidateLine(ALineIdx, AWrappCount);
end;

procedure TSynLineMapAVLTree.InvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx
  );
var
  CurrentPage, NextPage, NewPage: TSynEditLineMapPageHolder;
  l: Integer;
begin
  CurrentPage := FindPageForLine(AFromLineIdx, afmPrev);
  if not CurrentPage.HasPage then begin
    CurrentPage := FirstPage;
    if not CurrentPage.HasPage then begin
      CurrentPage := FindPageForLine(AFromLineIdx, afmCreate);
    end;
  end;

  while CurrentPage.HasPage and (AFromLineIdx <= AToLineIdx) do begin
    NextPage := CurrentPage.Next;
    if NextPage.HasPage then
      l := Min(NextPage.StartLine - 1, AToLineIdx)
    else
      l := AToLineIdx;
    if not CurrentPage.ExtendAndInvalidateLines(AFromLineIdx, l) then begin
      if AFromLineIdx < CurrentPage.StartLine then begin
        NewPage := FindPageForLine(AFromLineIdx, afmCreate);
        NewPage.ExtendAndInvalidateLines(AFromLineIdx, CurrentPage.StartLine - 1 - AFromLineIdx); // Must be able to extend at end
      end;
      l := CurrentPage.RealEndLine;
      if AToLineIdx >= CurrentPage.StartLine then
        CurrentPage.InvalidateLines(CurrentPage.StartLine, l);
    end;
    AFromLineIdx := l + 1;
    if (AFromLineIdx > AToLineIdx) then
      break;
    CurrentPage := NextPage;
  end;

  if (AFromLineIdx <= AToLineIdx) then begin
    NewPage := FindPageForLine(AFromLineIdx, afmCreate);
    NewPage.ExtendAndInvalidateLines(AFromLineIdx, AToLineIdx); // Must be able to extend at end
  end;
end;

function TSynLineMapAVLTree.NextNodeForValidation: TSynEditLineMapPageHolder;
var
  n: TSynEditLineMapPage;
begin
  n := FInvalidEntryList.FFirstEntry;
  Result{%H-}.Init(n, n.GetPosition, n.GetSizesBeforeSum);
  n.RemoveFromInvalidList(rfiMarkAsValidating);
end;

function TSynLineMapAVLTree.GetViewedLineCountDifference: Integer;
var
  pg: TSynEditLineMapPageHolder;
begin
  pg := LastPage;
  if pg.HasPage then
    Result := pg.ViewedCountDifferenceBefore + pg.ViewedLineCountDifference
  else
    Result := 0;
end;

function TSynLineMapAVLTree.GetWrapLineForForText(ATextLine: TLineIdx
  ): TLineIdx;
var
  pg: TSynEditLineMapPageHolder;
begin
  pg := FindPageForLine(ATextLine);
  if not pg.HasPage then begin
    Result := ATextLine;
    exit;
  end;

  if pg.StartLine > ATextLine then begin
    // no wrapped line before AWrapLine;
    Result := ATextLine;
    exit;
  end;

  Result := pg.StartLine + pg.ViewedCountDifferenceBefore + pg.Page.WrappedOffsetFor[ATextLine - pg.StartLine];
end;

function TSynLineMapAVLTree.GetLineForForWrap(AWrapLine: TLineIdx; out
  AWrapOffset: TLineIdx): TLineIdx;
var
  pg: TSynEditLineMapPageHolder;
begin
  pg := FindPageForWrap(AWrapLine);
  // TODO: this may be the next node.
  Result := pg.GetLineForForWrap(AWrapLine, AWrapOffset);
end;

function TSynLineMapAVLTree.TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint_0
  ): TViewedPoint_0;
var
  pg: TSynEditLineMapPageHolder;
begin
  pg := FindPageForLine(ATextXYIdx.y);
  Result := pg.TextXYIdxToViewXYIdx(ATextXYIdx);
end;

function TSynLineMapAVLTree.ViewXYIdxToTextXYIdx(AViewXYIdx: TViewedPoint_0
  ): TPhysPoint_0;
var
  pg: TSynEditLineMapPageHolder;
begin
  pg := FindPageForWrap(AViewXYIdx.y);
  Result := pg.ViewXYIdxToTextXYIdx(AViewXYIdx);
end;

{ TLazSynDisplayLineMapping }

constructor TLazSynDisplayLineMapping.Create(AWrappedView: TSynEditLineMappingView);
begin
  FLineMappingView := AWrappedView;
  FCurWrappedLine := -1;
  inherited Create;
end;

procedure TLazSynDisplayLineMapping.SetHighlighterTokensLine(
  AWrappedLine: TLineIdx; out ARealLine: TLineIdx; out ASubLineIdx, AStartBytePos,
  AStartPhysPos, ALineByteLen: Integer);
var
  RealIdx: IntIdx;
begin
  if (not FCurWrapPage.HasPage) or (FCurWrapPage.StartLine > AWrappedLine) or
     (AWrappedLine >= FCurWrapPage.RealEndLine)
  then
    FCurWrapPage := FLineMappingView.FLineMappingData.FindPageForWrap(AWrappedLine);
  FCurWrappedLine := AWrappedLine;

  if FCurWrapPage.HasPage then
    RealIdx := FCurWrapPage.StartLine +
      FCurWrapPage.Page.GetOffsetForWrap(AWrappedLine - FCurWrapPage.StartLine - FCurWrapPage.ViewedCountDifferenceBefore, FCurrentWrapSubline)
  else begin
    RealIdx := AWrappedLine;
    FCurrentWrapSubline := 0;
  end;

  inherited SetHighlighterTokensLine(RealIdx, ARealLine, ASubLineIdx, AStartBytePos, AStartPhysPos, ALineByteLen);
  ASubLineIdx := FCurrentWrapSubline;
end;

procedure TLazSynDisplayLineMapping.FinishHighlighterTokens;
begin
  inherited FinishHighlighterTokens;
  FCurWrappedLine := -1;
  FCurWrapPage.ClearData;
end;

function TLazSynDisplayLineMapping.TextToViewIndex(ATextIndex: TLineIdx
  ): TLineRange;
begin
//TODO: inherited after wrappedview ????
  Result := inherited TextToViewIndex(ATextIndex);

  Result.Top := FLineMappingView.FLineMappingData.GetWrapLineForForText(Result.Top); // TODO also return the wrap size / if top = bottom
  Result.Bottom := FLineMappingView.FLineMappingData.GetWrapLineForForText(Result.Bottom+1)-1;
end;

function TLazSynDisplayLineMapping.ViewToTextIndex(AViewIndex: TLineIdx
  ): TLineIdx;
var
  SubLineOffset: TLineIdx;
begin
  Result := FLineMappingView.FLineMappingData.GetLineForForWrap(AViewIndex, SubLineOffset);
  Result := inherited ViewToTextIndex(Result);
end;

function TLazSynDisplayLineMapping.ViewToTextIndexEx(AViewIndex: TLineIdx; out
  AViewRange: TLineRange): TLineIdx;
var
  SubLineOffset: TLineIdx;
begin
  Result := FLineMappingView.FLineMappingData.GetLineForForWrap(AViewIndex, SubLineOffset);
  AViewRange := TextToViewIndex(Result); // TODO: reverts Text back to view ???
  Result := inherited ViewToTextIndex(Result);
end;

function TLazSynDisplayLineMapping.GetLinesCount: Integer;
begin
  Result := inherited GetLinesCount;
  Result := Result + FLineMappingView.FLineMappingData.ViewedLineCountDifference;
end;

{ TSynEditLineMappingView }

procedure TSynEditLineMappingView.LineCountChanged(Sender: TSynEditStrings;
  aIndex, aCount: Integer);
begin
//  if NextLines.IsInEditAction then exit;
  if aCount > 0 then
    FLineMappingData.AdjustForLinesInserted(aIndex, aCount, 0)
  else
    FLineMappingData.AdjustForLinesDeleted(aIndex, -aCount, 0);
  CallLinesChangedHandler;

end;

procedure TSynEditLineMappingView.DoDecPaintLock(Sender: TObject);
begin
  if FPaintLock > 0 then
    dec(FPaintLock);
  if (FPaintLock = 0) and FNotifyLinesChanged then
    CallLinesChangedHandler;
end;

procedure TSynEditLineMappingView.DoIncPaintLock(Sender: TObject);
begin
  inc(FPaintLock);
end;

procedure TSynEditLineMappingView.CallLinesChangedHandler;
begin
  if FPaintLock > 0 then begin
    FNotifyLinesChanged := True;
    exit;
  end;
  FNotifyLinesChanged := False;
  FNotifyLinesHandlers.CallNotifyEvents(Self);
end;

procedure TSynEditLineMappingView.LineTextChanged(Sender: TSynEditStrings;
  aIndex, aCount: Integer);
begin
  FLineMappingData.InvalidateLines(aIndex, aIndex+aCount-1);
  CallLinesChangedHandler;
end;

procedure TSynEditLineMappingView.LineEdited(Sender: TSynEditStrings; aLinePos,
  aBytePos, aCount, aLineBrkCnt: Integer; aText: String);
begin
//  if aLineBrkCnt<0
//  then LinesDeletedAtTextIndex(aLinePos, -aLineBrkCnt, ABytePos, true)
//  else if aLineBrkCnt > 0
//  then LinesInsertedAtTextIndex(aLinePos, aLineBrkCnt, ABytePos, true)
//  else begin
//    fFoldTree.AdjustColumn(aLinePos, aBytePos, aCount);
//    //if not(SkipFixFolding) then FixFoldingAtTextIndex(AStartIndex, AStartIndex+ALineCount+1)
//    //else
//    //if aLinePos < top + ALineCount then CalculateMaps;
//  end;
//  CallLinesChangedHandler;
end;

function TSynEditLineMappingView.GetDisplayView: TLazSynDisplayView;
begin
  if FDisplayFiew = nil then
    FDisplayFiew := TLazSynDisplayLineMapping.Create(Self);
  Result := FDisplayFiew;
end;

function TSynEditLineMappingView.GetViewedCount: integer;
begin
  Result := inherited GetCount;
  Result := Result + FLineMappingData.ViewedLineCountDifference;
end;

procedure TSynEditLineMappingView.SetManager(AManager: TSynTextViewsManager);
begin
  if Manager <> nil then begin
    RemoveChangeHandler(senrLineChange, @LineTextChanged);
    RemoveChangeHandler(senrLineCount, @LineCountChanged);
//    RemoveNotifyHandler(senrCleared, @LinesCleared);
    RemoveEditHandler(@LineEdited);
    RemoveNotifyHandler(senrBeforeDecPaintLock, @DoDecPaintLock);
    RemoveNotifyHandler(senrBeforeIncPaintLock, @DoIncPaintLock);
  end;
  inherited SetManager(AManager);
  if Manager <> nil then begin
    AddChangeHandler(senrLineCount, @LineCountChanged);
    AddChangeHandler(senrLineChange, @LineTextChanged);
//    AddNotifyHandler(senrCleared, @LinesCleared);
    AddEditHandler(@LineEdited);
    AddNotifyHandler(senrBeforeDecPaintLock, @DoDecPaintLock);
    AddNotifyHandler(senrBeforeIncPaintLock, @DoIncPaintLock);
  end;
end;

constructor TSynEditLineMappingView.Create;
begin
  inherited Create;
  FNotifyLinesHandlers := TMethodList.Create;
  FKnownLengthOfLongestLine := -1;
  FLineMappingData := CreateLineMappingData;
  FDisplayFiew := CreateDisplayViewEx;
end;

destructor TSynEditLineMappingView.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FNotifyLinesHandlers);
  FLineMappingData.Free;
  FDisplayFiew.Free;
end;

function TSynEditLineMappingView.GetLengthOfLongestLine: integer;
begin
  Result := FKnownLengthOfLongestLine;
  if Result < 0 then
    Result := inherited GetLengthOfLongestLine;
end;

function TSynEditLineMappingView.TextToViewIndex(aTextIndex: TLineIdx
  ): TLinePos;
begin
  aTextIndex := inherited TextToViewIndex(aTextIndex);
  Result := FLineMappingData.GetWrapLineForForText(aTextIndex); // TODO also return the wrap size / if top = bottom
end;

function TSynEditLineMappingView.ViewToTextIndex(aViewIndex: TLineIdx
  ): TLineIdx;
var
  SubLineOffset: TLineIdx;
begin
  aViewIndex := FLineMappingData.GetLineForForWrap(aViewIndex, SubLineOffset);
  Result := inherited ViewToTextIndex(aViewIndex);
end;

function TSynEditLineMappingView.TextXYToViewXY(APhysTextXY: TPhysPoint
  ): TViewedPoint;
begin
  Result := inherited TextXYToViewXY(APhysTextXY);
  Result := YToPos(FLineMappingData.TextXYIdxToViewXYIdx(YToIdx(Result)));
end;

function TSynEditLineMappingView.ViewXYToTextXY(APhysViewXY: TViewedPoint
  ): TPhysPoint;
begin
  Result := YToPos(FLineMappingData.ViewXYIdxToTextXYIdx(YToIdx(APhysViewXY)));
  Result := inherited ViewXYToTextXY(Result);
end;

procedure TSynEditLineMappingView.InvalidateLines(AFromLineIdx,
  AToLineIdx: TLineIdx);
begin
  FLineMappingData.InvalidateLines(AFromLineIdx, AToLineIdx);
  CallLinesChangedHandler;
end;

procedure TSynEditLineMappingView.AddLinesChangedHandler(AHandler: TNotifyEvent
  );
begin
  FNotifyLinesHandlers.Add(TMethod(AHandler));
end;

procedure TSynEditLineMappingView.RemoveLinesChangedHandler(
  AHandler: TNotifyEvent);
begin
  FNotifyLinesHandlers.Remove(TMethod(AHandler));
end;

end.


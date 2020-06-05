unit SynEditViewedLineMap experimental;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}
{$INLINE off}

interface

uses
  Classes, SysUtils, SynEditMiscClasses, LazSynEditText, SynEditTypes,
  SynEditMiscProcs, LazUTF8, LazLoggerBase, LCLProc;

const
  SYN_WORD_WRAP_SPLIT_SIZE     = 2500;  // At least  2 * SYN_WORD_WRAP_JOIN_SIZE
  SYN_WORD_WRAP_JOIN_SIZE      =  500;
  SYN_WORD_WRAP_JOIN_DISTANCE  =  300;  // Do NOT join if distance is bigger than this

  SYN_WORD_WRAP_GROW_SIZE  =   16;

type
  TRemoveFromInvalidListMode = (rfiDefault, rfiForce, rfiMarkAsValidating);

  TSynEditLineMapPage = class;
  TSynLineMapAVLTree = class;

  TLineMapPageCreatorProc = function(AMapTree: TSynLineMapAVLTree): TSynEditLineMapPage of object;

  { TSynEditLineMapPageLinkedList }

  TSynEditLineMapPageLinkedList = class
  private
    FFirstEntry, FLastEntry: TSynEditLineMapPage;
  public
    procedure AddToInvalidList(AnEntry: TSynEditLineMapPage);
    procedure RemoveFromInvalidList(AnEntry: TSynEditLineMapPage; AMode: TRemoveFromInvalidListMode = rfiDefault);
  end;

  { TSynEditLineMapPage }

  TSynEditLineMapPage = class(TSynSizedDifferentialAVLNode)
  private
    FTree: TSynLineMapAVLTree;
    FPrevPageWithInvalid, FNextPageWithInvalid: TSynEditLineMapPage;
  protected
function GetWrappedOffsetFor(ARealOffset: IntIdx): IntIdx;  virtual; abstract;
    function GetFirstInvalidLine: Integer; virtual;
    function GetFirstInvalidEndLine: Integer; virtual;
    function GetLastInvalidLine: Integer; virtual;
    function GetViewedRealCountDifference: Integer; virtual;

    function IsValid: boolean; virtual;
    procedure AddToInvalidList;
    procedure RemoveFromInvalidList(AMode: TRemoveFromInvalidListMode = rfiDefault);

    function Left: TSynEditLineMapPage;
    function Parent: TSynEditLineMapPage;
    function Right: TSynEditLineMapPage;
    property LeftSizeSum;                                                       // LeftSizeSum:  Lines after wrap, in nodes to the left
    property Tree: TSynLineMapAVLTree read FTree;

  public
    constructor Create(ATree: TSynLineMapAVLTree); virtual;
    procedure DumpNode(ALine: Integer = 0; AnIndent: Integer = 0); virtual;

    property NodeLineOffset: Integer read FPositionOffset write FPositionOffset;    // LineOffset: Line-Number Offset to parent node
    procedure UpdateNodeSize(ANewSize: Integer);

    function CanExtendEndTo(ALineIdx: Integer; AIgnoreJoinDist: Boolean = False): boolean; virtual;
    function CanExtendStartTo(ALineIdx: Integer; AIgnoreJoinDist: Boolean = False): boolean; virtual;

    function Precessor: TSynEditLineMapPage; reintroduce;
    function Successor: TSynEditLineMapPage; reintroduce;
    function Precessor(var aStartPosition, aSizesBeforeSum: Integer): TSynEditLineMapPage; reintroduce;
    function Successor(var aStartPosition, aSizesBeforeSum: Integer): TSynEditLineMapPage; reintroduce;

    procedure InsertLinesAtOffset(ALineOffset, ALineCount: IntIdx); virtual;
    procedure DeleteLinesAtOffset(ALineOffset, ALineCount: IntIdx; ADoNotShrink: Boolean = False); virtual;

    procedure AdjustForLinesInserted(AStartLine, ALineCount: IntIdx; ABytePos: Integer); virtual;
    procedure AdjustForLinesDeleted(AStartLine, ALineCount: IntIdx; ABytePos: Integer); virtual;
    procedure MoveLinesAtStartTo(ADestPage: TSynEditLineMapPage; ASourceEndLine, ATargetStartLine: Integer);  virtual;
    procedure MoveLinesAtEndTo(ADestPage: TSynEditLineMapPage; ASourceStartLine, ACount: Integer);  virtual;

    property FirstInvalidLine: Integer read GetFirstInvalidLine;
    property FirstInvalidEndLine: Integer read GetFirstInvalidEndLine;
    property LastInvalidLine: Integer read GetLastInvalidLine;

    // must be FirstInvalidLine (or Last) => so FirstInvalidLine can be set.
    procedure EndValidate; virtual;
    procedure ValidateLine(ALineOffset, AWrappCount: Integer); virtual;
    procedure InvalidateLines(AFromOffset, AToOffset: Integer); virtual; // TODO: adjust offset
    function  ExtendAndInvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx): Boolean; virtual;

    function RealCount: Integer; virtual; // count of real lines
    function RealStartLine: Integer; virtual; // Offset
    function RealEndLine: Integer; virtual; // Offset + RealCount - 1;
    property ViewedRealCountDifference: Integer read GetViewedRealCountDifference; // viewed - real

    function GetOffsetForWrap(AWrapOffset: IntIdx; out ASubOffset: IntIdx): IntIdx; virtual; abstract;
    property WrappedOffsetFor[ARealOffset: IntIdx]: IntIdx read GetWrappedOffsetFor;

    function TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint; ANodeStartLine: IntIdx): TPhysPoint; virtual;
    function ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint; ANodeStartLine: IntIdx): TPhysPoint; virtual;
  end;


  { TSynEditLineMapPageHolder }

  TSynEditLineMapPageHolder = record
  strict private
    FData: TSynEditLineMapPage;
    FStartLine: Integer;             // first line with wrap info
    FViewedCountDifferenceBefore: Integer; // Lines after wrap, before this node
    function GetFirstInvalidEndLine: Integer; inline;
    function GetFirstInvalidLine: Integer; inline;
    function GetLastInvalidLine: Integer; inline;
    function GetViewedLineCountDifference: Integer;  inline;
  public
    procedure Init(aData : TSynEditLineMapPage; aStartLine, aWrappedBefore: Integer);
    procedure ClearData;
    function Next: TSynEditLineMapPageHolder;
    function Prev: TSynEditLineMapPageHolder;
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

    function CanExtendStartTo(ALineIdx: Integer; AIgnoreJoinDist: Boolean = False): boolean;
    function CanExtendEndTo(ALineIdx: Integer; AIgnoreJoinDist: Boolean = False): boolean;

    procedure AdjustPosition(AValue : Integer); // Must not change order with prev/next node

    procedure ValidateLine(ALineIdx, AWrappCount: Integer); inline;
    procedure InvalidateLines(AFromIdx, AToIdx: Integer);
    function  ExtendAndInvalidateLines(AFromIdx, AToIdx: TLineIdx): Boolean;

    property StartLine: Integer read FStartLine; // real line
    function NextNodeLine: Integer;

    function RealCount: Integer; // count of real lines
    function RealStartLine: Integer; // Offset
    function RealEndLine: Integer; // Offset + RealCount - 1;

    function TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint): TPhysPoint;
    function ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint): TPhysPoint;

    property FirstInvalidLine: Integer read GetFirstInvalidLine;
    property FirstInvalidEndLine: Integer read GetFirstInvalidEndLine;
    property LastInvalidLine: Integer read GetLastInvalidLine;

    property ViewedCountDifferenceBefore: Integer read FViewedCountDifferenceBefore; // viewed - real
    property ViewedLineCountDifference: Integer read GetViewedLineCountDifference;
    property Page: TSynEditLineMapPage read FData;
  end;

  { TSynLineMapAVLTree }

  TSynLineMapAVLTree = class(TSynSizedDifferentialAVLTree)
  private
    FPageCreatorProc: TLineMapPageCreatorProc;
    FPageSplitSize: Integer;
    FPageJoinSize, FPageJoinDistance: Integer;
    FCurrentValidatingNode: TSynEditLineMapPageHolder;
    FCurrentValidatingNodeLastLine: Integer;
    FInvalidEntryList: TSynEditLineMapPageLinkedList;

    function GetViewedLineCountDifference: Integer;
    function NextNodeForValidation: TSynEditLineMapPageHolder;
  protected
    function CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode; override;
    procedure FreeNode(ANode: TSynEditLineMapPage);
    procedure RemoveNode(ANode: TSynEditLineMapPage);
  public
    (* Find Page by real Line *)
    function FirstPage: TSynEditLineMapPageHolder;
    function LastPage: TSynEditLineMapPageHolder;
    function FindPageForLine(ALineIdx: IntIdx; AMode: TSynSizedDiffAVLFindMode = afmPrev) : TSynEditLineMapPageHolder;

    //(* Find Fold by wrapped Line  *)
    function FindPageForWrap(AViewedLineIdx: Integer): TSynEditLineMapPageHolder;

  public
    constructor Create;
    constructor Create(APageJoinSize, APageSplitSize, APageJoinDistance: Integer);
    destructor Destroy; override;
    procedure Clear; override;
    procedure DebugDump;
    property PageCreatorProc: TLineMapPageCreatorProc read FPageCreatorProc write FPageCreatorProc;

    property Root: TSynSizedDifferentialAVLNode read FRoot write FRoot;
    property RootOffset : Integer read FRootOffset write FRootOffset;

    function NeedsValidation: Boolean;
    // ValidateLine must only be called AFTER NextBlockForValidation / with no modifications to the tree
    function NextBlockForValidation(out ALowLine, AHighLine: TLineIdx): boolean;
    procedure EndValidate;
    procedure ValidateLine(ALineIdx: TLineIdx; AWrappCount: Integer);
    procedure InvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx);

    procedure AdjustForLinesInserted(ALineIdx, ALineCount, ABytePos: Integer); reintroduce;
    procedure AdjustForLinesDeleted(AStartLine, ALineCount, ABytePos: Integer); reintroduce;

    function GetWrapLineForForText(ATextLine: TLineIdx): TLineIdx;
    function GetLineForForWrap(AWrapLine: TLineIdx; out AWrapOffset: TLineIdx): TLineIdx;
    property ViewedLineCountDifference: Integer read GetViewedLineCountDifference;

    function TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint): TPhysPoint; inline;
    function ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint): TPhysPoint; inline;

    property PageSplitSize: Integer read FPageSplitSize;
    //property PageJoinSize: Integer read FPageJoinSize;
    property PageJoinDistance: Integer read FPageJoinDistance;
  end;


type
  TSynEditLineMappingView = class;

  { TLazSynDisplayLineMapping }

  TLazSynDisplayLineMapping = class(TLazSynDisplayViewEx)
  private
    FCurWrapPage: TSynEditLineMapPageHolder;
  protected
    FCurWrappedLine: TLineIdx;
    FLineMappingView: TSynEditLineMappingView;
    FCurrentWrapSubline: IntIdx;
  public
    constructor Create(AWrappedView: TSynEditLineMappingView);
    procedure SetHighlighterTokensLine(AWrappedLine: TLineIdx; out
      ARealLine: TLineIdx; out AStartBytePos, ALineByteLen: Integer); override;
//    function GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean; override;
    procedure FinishHighlighterTokens; override;
    function TextToViewIndex(ATextIndex: TLineIdx): TLineRange; override;
    function ViewToTextIndex(AViewIndex: TLineIdx): TLineIdx; override;
    function ViewToTextIndexEx(AViewIndex: TLineIdx; out AViewRange: TLineRange): TLineIdx; override;
    function GetLinesCount: Integer; override;
  end;

  TWrapInfoForViewedXYProc = procedure(var AViewedXY: TPhysPoint; AFlags: TViewedXYInfoFlags; out AFirstViewedX: IntPos; ALogPhysConvertor: TSynLogicalPhysicalConvertor) of object;

  { TSynEditLineMappingView }

  TSynEditLineMappingView = class(TSynEditStringsLinked)
  private
    FDisplayFiew: TLazSynDisplayLineMapping;
    FWrapInfoForViewedXYProc: TWrapInfoForViewedXYProc;
    FKnownLengthOfLongestLine: integer;
    FLineMappingData: TSynLineMapAVLTree;
    FPaintLock: Integer;
    FNotifyLinesChanged: Boolean;
    FNotifyLinesHandlers: TMethodList;
    procedure DoDecPaintLock(Sender: TObject);
    procedure DoIncPaintLock(Sender: TObject);
    procedure CallLinesChangedHandler; inline;
    function GetPageMapCreator: TLineMapPageCreatorProc;
    procedure LineCountChanged(Sender: TSynEditStrings; aIndex, aCount: Integer);
    procedure LineTextChanged(Sender: TSynEditStrings; aIndex, aCount: Integer);
    Procedure LineEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
    procedure SetPageMapCreator(AValue: TLineMapPageCreatorProc);
  protected
    function GetDisplayView: TLazSynDisplayView; override;
    function GetViewedCount: integer; override;
    procedure SetManager(AManager: TSynTextViewsManager); override;
    procedure InternalGetInfoForViewedXY(AViewedXY: TPhysPoint;
      AFlags: TViewedXYInfoFlags; out AViewedXYInfo: TViewedXYInfo;
      ALogPhysConvertor: TSynLogicalPhysicalConvertor); override;
  public
    constructor Create; //(ASynEdit: TSynEdit);
    destructor Destroy; override;
    procedure SetDisplayView(ADisplayView: TLazSynDisplayLineMapping);

    function GetLengthOfLongestLine: integer; override;
    function TextToViewIndex(aTextIndex: TLineIdx): TLinePos; override;
    function ViewToTextIndex(aViewIndex: TLineIdx): TLineIdx; override;
    function TextXYToViewXY(APhysTextXY: TPhysPoint): TPhysPoint; override;
    function ViewXYToTextXY(APhysViewXY: TPhysPoint): TPhysPoint; override;
    //function AddVisibleOffsetToTextIndex(aTextIndex: IntIdx; LineOffset: Integer): IntIdx; override; (* Add/Sub to/from TextPos (1-based) skipping folded *)

    procedure InvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx);
    procedure AddLinesChangedHandler(AHandler: TNotifyEvent);
    procedure RemoveLinesChangedHandler(AHandler: TNotifyEvent);
  public
    property PageMapCreator: TLineMapPageCreatorProc read GetPageMapCreator write SetPageMapCreator;
    property KnownLengthOfLongestLine: integer read FKnownLengthOfLongestLine write FKnownLengthOfLongestLine;

    property Tree: TSynLineMapAVLTree read FLineMappingData; experimental;
    property WrapInfoForViewedXYProc: TWrapInfoForViewedXYProc read FWrapInfoForViewedXYProc write FWrapInfoForViewedXYProc;
  end;


implementation

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

function TSynEditLineMapPage.GetViewedRealCountDifference: Integer;
begin
  Result := 0;
end;

function TSynEditLineMapPage.IsValid: boolean;
begin
  Result := True;
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

function TSynEditLineMapPage.GetFirstInvalidEndLine: Integer;
begin
  Result := -1;
end;

function TSynEditLineMapPage.GetFirstInvalidLine: Integer;
begin
  Result := -1;
end;

function TSynEditLineMapPage.GetLastInvalidLine: Integer;
begin
  Result := -1;
end;

function TSynEditLineMapPage.Left: TSynEditLineMapPage;
begin
  Result := TSynEditLineMapPage(FLeft);
end;

function TSynEditLineMapPage.Parent: TSynEditLineMapPage;
begin
  Result := TSynEditLineMapPage(FParent);
end;

function TSynEditLineMapPage.Right: TSynEditLineMapPage;
begin
  Result := TSynEditLineMapPage(FRight);
end;

function TSynEditLineMapPage.CanExtendStartTo(ALineIdx: Integer;
  AIgnoreJoinDist: Boolean): boolean;
begin
  Result := False;
end;

function TSynEditLineMapPage.CanExtendEndTo(ALineIdx: Integer;
  AIgnoreJoinDist: Boolean): boolean;
begin
  Result := False;
end;

constructor TSynEditLineMapPage.Create(ATree: TSynLineMapAVLTree);
begin
  FTree := ATree;
  inherited Create;
end;

procedure TSynEditLineMapPage.DumpNode(ALine: Integer; AnIndent: Integer);
begin
  //
end;

procedure TSynEditLineMapPage.UpdateNodeSize(ANewSize: Integer);
begin
  if FSize <> ANewSize then begin
    AdjustParentLeftCount(ANewSize -FSize);
    FSize := ANewSize;
  end;
end;

procedure TSynEditLineMapPage.AdjustForLinesInserted(AStartLine,
  ALineCount: IntIdx; ABytePos: Integer);
begin
  //
end;

procedure TSynEditLineMapPage.AdjustForLinesDeleted(AStartLine,
  ALineCount: IntIdx; ABytePos: Integer);
begin
  //
end;

procedure TSynEditLineMapPage.InsertLinesAtOffset(ALineOffset,
  ALineCount: IntIdx);
begin
  //
end;

procedure TSynEditLineMapPage.DeleteLinesAtOffset(ALineOffset,
  ALineCount: IntIdx; ADoNotShrink: Boolean);
begin
  //
end;

procedure TSynEditLineMapPage.MoveLinesAtStartTo(
  ADestPage: TSynEditLineMapPage; ASourceEndLine, ATargetStartLine: Integer);
begin
  //
end;

procedure TSynEditLineMapPage.MoveLinesAtEndTo(ADestPage: TSynEditLineMapPage;
  ASourceStartLine, ACount: Integer);
begin
  //
end;

procedure TSynEditLineMapPage.EndValidate;
begin
  //
end;

procedure TSynEditLineMapPage.ValidateLine(ALineOffset, AWrappCount: Integer);
begin
  //
end;

procedure TSynEditLineMapPage.InvalidateLines(AFromOffset, AToOffset: Integer);
begin
  //
end;

function TSynEditLineMapPage.ExtendAndInvalidateLines(AFromLineIdx,
  AToLineIdx: TLineIdx): Boolean;
begin
  Result := False;
end;

function TSynEditLineMapPage.RealCount: Integer;
begin
  Result := 0;
end;

function TSynEditLineMapPage.RealStartLine: Integer;
begin
  Result := 0;
end;

function TSynEditLineMapPage.RealEndLine: Integer;
begin
  Result := 0;
end;

function TSynEditLineMapPage.TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint;
  ANodeStartLine: IntIdx): TPhysPoint;
begin
  Result := ATextXYIdx;
end;

function TSynEditLineMapPage.ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint;
  ANodeStartLine: IntIdx): TPhysPoint;
begin
  Result := AViewXYIdx;
end;

function TSynEditLineMapPage.Precessor: TSynEditLineMapPage;
begin
  Result := TSynEditLineMapPage(inherited Precessor);
end;

function TSynEditLineMapPage.Successor: TSynEditLineMapPage;
begin
  Result := TSynEditLineMapPage(inherited Successor);
end;

function TSynEditLineMapPage.Precessor(var aStartPosition,
  aSizesBeforeSum: Integer): TSynEditLineMapPage;
begin
  Result := TSynEditLineMapPage(inherited Precessor(aStartPosition, aSizesBeforeSum));
end;

function TSynEditLineMapPage.Successor(var aStartPosition,
  aSizesBeforeSum: Integer): TSynEditLineMapPage;
begin
  Result := TSynEditLineMapPage(inherited Successor(aStartPosition, aSizesBeforeSum));
end;

{ TSynEditLineMapPageHolder }

function TSynEditLineMapPageHolder.GetFirstInvalidLine: Integer;
begin
  if FData <> nil then begin
    Result := FData.FirstInvalidLine;
    if Result >= 0 then
      Result := Result + FStartLine;
  end
  else
    Result := -1;
end;

function TSynEditLineMapPageHolder.GetFirstInvalidEndLine: Integer;
begin
  if FData <> nil then begin
    Result := FData.FirstInvalidEndLine;
    if Result >= 0 then
      Result := Result + FStartLine;
  end
  else
    Result := -1;
end;

function TSynEditLineMapPageHolder.GetLastInvalidLine: Integer;
begin
  if FData <> nil then begin
    Result := FData.LastInvalidLine;
    if Result >= 0 then
      Result := Result + FStartLine;
  end
  else
    Result := -1;
end;

function TSynEditLineMapPageHolder.GetViewedLineCountDifference: Integer;
begin
  Result := 0;
  if FData <> nil then
    Result := FData.ViewedRealCountDifference;
end;

procedure TSynEditLineMapPageHolder.Init(aData: TSynEditLineMapPage; aStartLine,
  aWrappedBefore: Integer);
begin
  FData := aData;
  FStartLine :=  aStartLine;
  FViewedCountDifferenceBefore := aWrappedBefore;
end;

procedure TSynEditLineMapPageHolder.ClearData;
begin
  FData := nil;
end;

function TSynEditLineMapPageHolder.Next: TSynEditLineMapPageHolder;
var aStart, aBefore : Integer;
begin
  if FData <> nil then begin
    aStart := FStartLine;
    aBefore := FViewedCountDifferenceBefore;
    Result.FData := FData.Successor(aStart, aBefore);
    Result.FStartLine := aStart;
    Result.FViewedCountDifferenceBefore := aBefore;
  end
  else
    Result.FData := nil;
end;

function TSynEditLineMapPageHolder.Prev: TSynEditLineMapPageHolder;
var aStart, aBefore : Integer;
begin
  if FData <> nil then begin
    aStart := FStartLine;
    aBefore := FViewedCountDifferenceBefore;
    Result.FData := FData.Precessor(aStart, aBefore);
    Result.FStartLine := aStart;
    Result.FViewedCountDifferenceBefore := aBefore;
  end
  else
    Result.FData := nil;
end;

function TSynEditLineMapPageHolder.HasPage: Boolean;
begin
  Result := FData <> nil;
end;

function TSynEditLineMapPageHolder.CountAvailable(AMaxAllowed: Integer): integer;
begin
  if FData = nil then
    Result := 0
  else
    Result := AMaxAllowed - FData.RealCount;
  assert(Result >= 0, 'TSynEditLineMapPageHolder.CountAvailable: Result >= 0');
end;

procedure TSynEditLineMapPageHolder.AdjustForLinesInserted(AStartLine,
  ALineCount, ABytePos: Integer);
begin
  FData.AdjustForLinesInserted(AStartLine - FStartLine, ALineCount, ABytePos);
end;

procedure TSynEditLineMapPageHolder.AdjustForLinesDeleted(AStartLine,
  ALineCount, ABytePos: Integer);
begin
  FData.AdjustForLinesDeleted(AStartLine - FStartLine, ALineCount, ABytePos);
end;

procedure TSynEditLineMapPageHolder.SplitNodeToPrev(
  var APrevPage: TSynEditLineMapPageHolder; ASourceEndLine: Integer);
var
  ELine: Integer;
begin
  assert(ASourceEndLine >= FStartLine, 'TSynEditLineMapPageHolder.SplitNodeToPrev: ASourceEndLine > FStartLine');
  if not APrevPage.HasPage then
    APrevPage := Prev;

  if not APrevPage.HasPage then begin
    SplitNodeToNewPrev(APrevPage, ASourceEndLine, FStartLine);
    exit;
  end;

  assert(APrevPage.FStartLine < FStartLine, 'TSynEditLineMapPageHolder.SplitNodeToPrev: APrevPage.FStartLine < FStartLine');
  ELine := ASourceEndLine - FStartLine;
  FData.MoveLinesAtStartTo(
    APrevPage.FData,
    ELine,
    FStartLine - APrevPage.FStartLine);
  AdjustPosition(ELine + 1);
  assert(APrevPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolder.SplitNodeToPrev: APrevPage.RealCount <= FPageSplitSize');
end;

procedure TSynEditLineMapPageHolder.SplitNodeToNext(
  var ANextPage: TSynEditLineMapPageHolder; ASourceStartLine: Integer);
var
  Cnt: Integer;
begin
  assert(ASourceStartLine >= FStartLine, 'TSynEditLineMapPageHolder.SplitNodeToNext: ASourceStartLine > FStartLine');
  if not ANextPage.HasPage then
    ANextPage := Next;

  if not ANextPage.HasPage then begin
    SplitNodeToNewNext(ANextPage, ASourceStartLine);
    exit;
  end;

  assert(ANextPage.FStartLine > ASourceStartLine, 'TSynEditLineMapPageHolder.SplitNodeToNext: ANextPage.FStartLine > ASourceStartLine');
  Cnt := ANextPage.FStartLine - ASourceStartLine;
  FData.MoveLinesAtEndTo(
    ANextPage.FData,
    ASourceStartLine - FStartLine,
    Cnt);
  ANextPage.AdjustPosition(-Cnt);
  assert(ANextPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolder.SplitNodeToNext: ANextPage.RealCount <= FPageSplitSize');
end;

procedure TSynEditLineMapPageHolder.SplitNodeToNewPrev(out
  APrevPage: TSynEditLineMapPageHolder; ASourceEndLine: Integer;
  ANewPageStartLine: Integer);
var
  SLine, ELine, Offs: Integer;
begin
  assert(ASourceEndLine >= FStartLine, 'TSynEditLineMapPageHolder.SplitNodeToNewPrev: ASourceEndLine > FStartLine');
  assert(ANewPageStartLine <= FStartLine, 'TSynEditLineMapPageHolder.SplitNodeToNewPrev: ANewPageStartLine <= FStartLine');

  if ANewPageStartLine >= 0 then begin
    SLine := ANewPageStartLine;
    Offs := FStartLine - ANewPageStartLine;
  end
  else begin
    SLine := FStartLine;
    Offs := 0;
  end;
  ELine := ASourceEndLine - FStartLine;
  AdjustPosition(ELine + 1);

  APrevPage := FData.FTree.FindPageForLine(SLine, afmCreate);
  FData.MoveLinesAtStartTo(
    APrevPage.FData,
    ELine,
    Offs);
  assert(APrevPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolder.SplitNodeToPrev: APrevPage.RealCount <= Page.FTree.FPageSplitSize');
end;

procedure TSynEditLineMapPageHolder.SplitNodeToNewNext(out
  ANextPage: TSynEditLineMapPageHolder; ASourceStartLine: Integer);
begin
  assert(ASourceStartLine > FStartLine, 'TSynEditLineMapPageHolder.SplitNodeToNewNext: ASourceStartLine > FStartLine');

  ANextPage := FData.FTree.FindPageForLine(ASourceStartLine, afmCreate);

  FData.MoveLinesAtEndTo(
    ANextPage.FData,
    ASourceStartLine - FStartLine,
    Max(FData.LastInvalidLine, FData.RealCount));  // May be bigger than needed....
  assert(ANextPage.RealCount <= Page.FTree.FPageSplitSize, 'TSynEditLineMapPageHolder.SplitNodeToNext: ANextPage.RealCount <= Page.FTree.FPageSplitSize');
end;

function TSynEditLineMapPageHolder.CanExtendStartTo(ALineIdx: Integer;
  AIgnoreJoinDist: Boolean): boolean;
begin
  // TODO: if the node has invalid lines in the offset or behind, then those may need to be included?
  Result := HasPage and
            (RealEndLine - ALineIdx < Page.FTree.FPageSplitSize) and
            (AIgnoreJoinDist or (RealStartLine - ALineIdx < Page.FTree.FPageJoinDistance));
end;

function TSynEditLineMapPageHolder.CanExtendEndTo(ALineIdx: Integer;
  AIgnoreJoinDist: Boolean): boolean;
begin
  Result := HasPage and
            (ALineIdx - RealStartLine < Page.FTree.FPageSplitSize) and
            (AIgnoreJoinDist or (ALineIdx - RealEndLine < Page.FTree.FPageJoinDistance));
end;

procedure TSynEditLineMapPageHolder.AdjustPosition(AValue: Integer);
begin
  assert(HasPage, 'TSynEditLineMapPageHolder.AdjustPosition: HasPage');
  FData.AdjustPosition(AValue);
  FStartLine := FStartLine + AValue;
end;

procedure TSynEditLineMapPageHolder.InvalidateLines(AFromIdx, AToIdx: Integer);
begin
  assert(HasPage, 'TSynEditLineMapPageHolder.InvalidateLines: HasPage');
  FData.InvalidateLines(AFromIdx-FStartLine, AToIdx-FStartLine);
end;

function TSynEditLineMapPageHolder.ExtendAndInvalidateLines(AFromIdx,
  AToIdx: TLineIdx): Boolean;
begin
  Result := HasPage;
  if Result then
    Result := FData.ExtendAndInvalidateLines(AFromIdx-FStartLine, AToIdx-FStartLine);
end;

procedure TSynEditLineMapPageHolder.ValidateLine(ALineIdx, AWrappCount: Integer);
begin
  FData.ValidateLine(ALineIdx-FStartLine, AWrappCount);
end;

function TSynEditLineMapPageHolder.RealCount: Integer;
begin
  if HasPage then
    Result := FData.RealCount
  else
    Result := 0;
end;

function TSynEditLineMapPageHolder.RealStartLine: Integer;
begin
  Result := FStartLine + FData.RealStartLine;
end;

function TSynEditLineMapPageHolder.RealEndLine: Integer;
begin
  Result := FStartLine + FData.RealEndLine;
end;

function TSynEditLineMapPageHolder.TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint
  ): TPhysPoint;
begin
  Result := ATextXYIdx;
  if not HasPage then
    exit;
  Result := Page.TextXYIdxToViewXYIdx(ATextXYIdx, StartLine);
  Result.y := Result.y + ViewedCountDifferenceBefore;
end;

function TSynEditLineMapPageHolder.ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint
  ): TPhysPoint;
begin
  Result := AViewXYIdx;
  if not HasPage then
    exit;
  Result.y := Result.y - ViewedCountDifferenceBefore;
  Result := Page.ViewXYIdxToTextXYIdx(Result, StartLine);
end;

function TSynEditLineMapPageHolder.NextNodeLine: Integer;
begin
  assert(HasPage, 'TSynEditLineMapPageHolder.NextNodeLinet: HasPage');
  Result := FStartLine + RealCount;
end;

{ TSynLineMapAVLTree }

function TSynLineMapAVLTree.CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode;
begin
  if FPageCreatorProc = nil then
    exit(nil);
  Result := FPageCreatorProc(Self);
end;

procedure TSynLineMapAVLTree.FreeNode(ANode: TSynEditLineMapPage);
begin
  RemoveNode(ANode);
  DisposeNode(TSynSizedDifferentialAVLNode(ANode));
end;

procedure TSynLineMapAVLTree.RemoveNode(ANode: TSynEditLineMapPage);
begin
  if (FInvalidEntryList <> nil) and
     ( (ANode.FPrevPageWithInvalid <> nil) or (ANode.FNextPageWithInvalid <> nil))
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
  if not CurrentPage.HasPage then begin
    CurrentPage := FirstPage;
    if not CurrentPage.HasPage then begin
      CurrentPage := FindPageForLine(ALineIdx, afmCreate);
      if not CurrentPage.HasPage then
        exit;
    end;
  end;

  // inherited => moves any node with FStartLine >= AStartLine
  inherited AdjustForLinesInserted(ALineIdx, ALineCount);

// TODO ::::XXXXX   ONLY if inside the page / otherwise call invalidate
  CurrentPage.AdjustForLinesInserted(ALineIdx, ALineCount, ABytePos);
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
    if TmpPage.HasPage then begin
      d := TmpPage.StartLine - CurLine;
      CurLine := CurLine + d;
      CountRemaining := CountRemaining - d;
    end;
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
    FirstTmpPage.ClearData;

  LineAfter := AStartLine + ALineCount;
  while TmpPage.HasPage and (LineAfter >= TmpPage.NextNodeLine) do begin
    NextPage := TmpPage.Next;
    FreeNode(TmpPage.Page);
    TmpPage := NextPage;
  end;

  if FirstTmpPage.HasPage then begin
    FirstTmpPage.AdjustForLinesDeleted(CurLine, CountRemaining, ABytePos);
  end;

  if TmpPage.HasPage and (LineAfter - TmpPage.StartLine > 0) then begin
    TmpPage.AdjustForLinesDeleted(TmpPage.StartLine, LineAfter - TmpPage.StartLine, 0);
    TmpPage.AdjustPosition(AStartLine - TmpPage.StartLine);
  end
  else
    TmpPage.ClearData;

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
  FInvalidEntryList.Destroy;
  inherited Destroy;
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

function TSynLineMapAVLTree.FirstPage: TSynEditLineMapPageHolder;
var
  r : TSynEditLineMapPage;
  rStartLine : Integer;
  rWrappedBefore : Integer;
begin
  r := TSynEditLineMapPage(First(rStartLine, rWrappedBefore));
  Result.Init(r, rStartLine, rWrappedBefore);
end;

function TSynLineMapAVLTree.LastPage: TSynEditLineMapPageHolder;
var
  r : TSynEditLineMapPage;
  rStartLine : Integer;
  rWrappedBefore : Integer;
begin
  r := TSynEditLineMapPage(Last(rStartLine, rWrappedBefore));
  Result.Init(r, rStartLine, rWrappedBefore);
end;

function TSynLineMapAVLTree.FindPageForLine(ALineIdx: IntIdx;
  AMode: TSynSizedDiffAVLFindMode): TSynEditLineMapPageHolder;
var
  r: TSynEditLineMapPage;
  rStartLine, rWrappedBefore: Integer;
begin
  r := TSynEditLineMapPage(FindNodeAtPosition(ALineIdx, AMode, rStartLine, rWrappedBefore));
  Result.Init(r, rStartLine, rWrappedBefore);
end;

function TSynLineMapAVLTree.FindPageForWrap(AViewedLineIdx: Integer): TSynEditLineMapPageHolder;
var
  nd, PrevNd: TSynEditLineMapPage;
  rRealStartLine, rSumViewedSizesBefore: Integer;
  PrvRealStartLine, PrvSumViewedSizesBefore, NewSumSizes: Integer;
begin
  Result.ClearData;

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
  end;

  Result := FInvalidEntryList.FFirstEntry <> nil;
  if not Result then
    exit;

  FCurrentValidatingNode := NextNodeForValidation; // TODO: directly fill FCurrentValidatingNode;
  Result := FCurrentValidatingNode.HasPage;
  assert(result, 'TSynLineMapAVLTree.NextBlockForValidation: result');
  ALowLine  := FCurrentValidatingNode.FirstInvalidLine;
  AHighLine := FCurrentValidatingNode.FirstInvalidEndLine;
  FCurrentValidatingNodeLastLine := -1; // If there was a next node, inval lines would be on that next node, and not on this
end;

procedure TSynLineMapAVLTree.EndValidate;
begin
  if FCurrentValidatingNode.HasPage then begin
    FCurrentValidatingNode.Page.EndValidate;
    FCurrentValidatingNode.ClearData;
  end;
end;

procedure TSynLineMapAVLTree.ValidateLine(ALineIdx: TLineIdx;
  AWrappCount: Integer);
var
  RealStart, RealEnd: Integer;
  SblRealStart: Integer;
  NewScrEnd: Integer;
  SiblingNode: TSynEditLineMapPageHolder;
begin
  assert(FCurrentValidatingNode.HasPage, 'TSynLineMapAVLTree.ValidateLine: FCurrentValidatingNode.HasPage');
  assert(ALineIdx >= FCurrentValidatingNode.StartLine, 'TSynLineMapAVLTree.ValidateLine: ALineIdx >= FCurrentValidatingNode.StartLine');

  if (FCurrentValidatingNodeLastLine > 0) and (ALineIdx > FCurrentValidatingNodeLastLine) then begin
    FCurrentValidatingNode.Page.EndValidate;
    FCurrentValidatingNode := FindPageForLine(ALineIdx);
    assert(FCurrentValidatingNode.HasPage, 'TSynLineMapAVLTree.ValidateLine: FCurrentValidatingNode.HasPage');
  end;

  RealStart := FCurrentValidatingNode.RealStartLine;
  RealEnd := FCurrentValidatingNode.RealEndLine;

  if ( (ALineIdx >= RealEnd   - FPageSplitSize) and  // max extended start
       (ALineIdx <= RealStart + FPageSplitSize)      // max extended end
     ) or
     (AWrappCount = 1)
  then begin
    FCurrentValidatingNode.ValidateLine(ALineIdx, AWrappCount);
    exit;
  end;

  // need splitting
  FCurrentValidatingNode.Page.EndValidate;
  SiblingNode.ClearData;

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

  FCurrentValidatingNode.ValidateLine(ALineIdx, AWrappCount);
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
  Result.Init(n, n.GetPosition, n.GetSizesBeforeSum);
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
    //Result := 0;
result := ATextLine;
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
  if not pg.HasPage then begin
    Result := 0;
    AWrapOffset := 0;
    exit;
  end;

  if pg.StartLine + pg.ViewedCountDifferenceBefore > AWrapLine then begin
    // no wrapped line before AWrapLine;
    Result := AWrapLine;
    AWrapOffset := 0;
    exit;
  end;

  // TODO: this may be the next node.
  Result := pg.StartLine + pg.Page.GetOffsetForWrap(AWrapLine - pg.StartLine - pg.ViewedCountDifferenceBefore, AWrapOffset);
end;

function TSynLineMapAVLTree.TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint
  ): TPhysPoint;
var
  pg: TSynEditLineMapPageHolder;
begin
  pg := FindPageForLine(ATextXYIdx.y);
  Result := pg.TextXYIdxToViewXYIdx(ATextXYIdx);
end;

function TSynLineMapAVLTree.ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint
  ): TPhysPoint;
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
  AWrappedLine: TLineIdx; out ARealLine: TLineIdx; out AStartBytePos,
  ALineByteLen: Integer);
var
  RealIdx: IntIdx;
begin
  if (not FCurWrapPage.HasPage) or (FCurWrapPage.StartLine > AWrappedLine) or
     (AWrappedLine >= FCurWrapPage.RealEndLine)
  then begin
    FCurWrapPage := FLineMappingView.FLineMappingData.FindPageForWrap(AWrappedLine);
    FCurWrappedLine := AWrappedLine;
  end;

  RealIdx := FCurWrapPage.StartLine +
    FCurWrapPage.Page.GetOffsetForWrap(AWrappedLine - FCurWrapPage.StartLine - FCurWrapPage.ViewedCountDifferenceBefore, FCurrentWrapSubline);

  inherited SetHighlighterTokensLine(RealIdx, ARealLine, AStartBytePos, ALineByteLen);
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

function TSynEditLineMappingView.GetPageMapCreator: TLineMapPageCreatorProc;
begin
  Result := FLineMappingData.FPageCreatorProc;
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

procedure TSynEditLineMappingView.SetPageMapCreator(
  AValue: TLineMapPageCreatorProc);
begin
  FLineMappingData.FPageCreatorProc := AValue;
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

procedure TSynEditLineMappingView.InternalGetInfoForViewedXY(
  AViewedXY: TPhysPoint; AFlags: TViewedXYInfoFlags; out
  AViewedXYInfo: TViewedXYInfo; ALogPhysConvertor: TSynLogicalPhysicalConvertor
  );
var
  FirstViewedX: IntPos;
begin
  WrapInfoForViewedXYProc(AViewedXY, AFlags, FirstViewedX, ALogPhysConvertor);

  inherited InternalGetInfoForViewedXY(AViewedXY, AFlags, AViewedXYInfo,
    ALogPhysConvertor);

  AViewedXYInfo.CorrectedViewedXY :=
    YToPos(FLineMappingData.TextXYIdxToViewXYIdx(YToIdx(AViewedXYInfo.CorrectedViewedXY)));
  AViewedXYInfo.FirstViewedX := FirstViewedX;
end;

constructor TSynEditLineMappingView.Create;
begin
  inherited Create;
  FNotifyLinesHandlers := TMethodList.Create;
  FLineMappingData := TSynLineMapAVLTree.Create;
  FKnownLengthOfLongestLine := -1;
end;

destructor TSynEditLineMappingView.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FNotifyLinesHandlers);
  FLineMappingData.Free;
  FDisplayFiew.Free;
end;

procedure TSynEditLineMappingView.SetDisplayView(
  ADisplayView: TLazSynDisplayLineMapping);
begin
  FDisplayFiew.Free;
  FDisplayFiew := ADisplayView;
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
  ): TPhysPoint;
begin
  Result := inherited TextXYToViewXY(APhysTextXY);
  Result := YToPos(FLineMappingData.TextXYIdxToViewXYIdx(YToIdx(Result)));
end;

function TSynEditLineMappingView.ViewXYToTextXY(APhysViewXY: TPhysPoint
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


unit SynEditWrappedView experimental;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, LazSynEditText, SynEdit, SynEditViewedLineMap,
  SynEditTypes, SynEditMiscProcs, SynEditHighlighter, SynEditMiscClasses,
  Graphics, LazLogger, LazListClasses;

type
  TLazSynEditLineWrapPlugin = class;

  TSynWordWrapLineData = Integer;
  PSynWordWrapLineData = ^TSynWordWrapLineData;

  TSynWordWrapIndexPage = class;
  TSynWordWrapLineMap = class;

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
    FAvlNode: TSynWordWrapIndexPage;
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

    property AvlNode: TSynWordWrapIndexPage read FAvlNode;
    property Offset: Integer read FOffsetAtStart;
    property RealCount: Integer read FWrappedExtraSumsCount;
    property ViewedCount: Integer read GetViewedCount;
    property ViewedRealCountDifference: Integer read GetViewedRealCountDifference; // viewed - real


    procedure InvalidateLines(AFromOffset, AToOffset: Integer); //
    procedure ValidateLine(ALineOffset, AWrappCount: Integer);
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

  { TSynWordWrapIndexPage }

  TSynWordWrapIndexPage = class(TSynEditLineMapPage)
  private
    FSynWordWrapLineMap: TSynWordWrapLineMap;
    FSynEditWrappedPlugin :TLazSynEditLineWrapPlugin;

    procedure UpdateViewedSizeDifference;
  protected
    function GetFirstInvalidLine: Integer; override;
    function GetFirstInvalidEndLine: Integer; override;
    function GetLastInvalidLine: Integer; override;
    function GetViewedRealCountDifference: Integer; override;

    function GetWrappedOffsetFor(ARealOffset: IntIdx): IntIdx; override;
    function IsValid: boolean; override;
  public
    property SynWordWrapLineMapStore: TSynWordWrapLineMap read FSynWordWrapLineMap; experimental; // 'For test case only';
  public
    constructor Create(ATree: TSynLineMapAVLTree); override;
    destructor Destroy; override;
    procedure DumpNode(ALine: Integer = 0; AnIndent: Integer = 0); override;

    function CanExtendStartTo(ALineOffs: Integer; AIgnoreJoinDist: Boolean = False): boolean; override;
    function CanExtendEndTo(ALineOffs: Integer; AIgnoreJoinDist: Boolean = False): boolean; override;

    procedure InsertLinesAtOffset(ALineOffset, ALineCount: IntIdx); override;
    procedure DeleteLinesAtOffset(ALineOffset, ALineCount: IntIdx; ADoNotShrink: Boolean = False); override;

    procedure AdjustForLinesInserted(AStartLine, ALineCount: IntIdx; ABytePos: Integer); override;
    procedure AdjustForLinesDeleted(AStartLine, ALineCount: IntIdx; ABytePos: Integer); override;

    procedure MoveLinesAtStartTo(ADestPage: TSynEditLineMapPage; ASourceEndLine, ATargetStartLine: Integer); override;
    procedure MoveLinesAtEndTo(ADestPage: TSynEditLineMapPage; ASourceStartLine, ACount: Integer); override;

    // must be FirstInvalidLine (or Last) => so FirstInvalidLine can be set.
    procedure EndValidate; override;
    procedure ValidateLine(ALineOffset, AWrappCount: Integer); override;
    procedure InvalidateLines(AFromOffset, AToOffset: Integer); override; // TODO: adjust offset
    function ExtendAndInvalidateLines(AFromLineIdx, AToLineIdx: TLineIdx): Boolean; override;

    function RealCount: Integer; override; // count of real lines
    function RealStartLine: Integer; override; // Offset
    function RealEndLine: Integer; override; // Offset + RealCount - 1;

    function GetOffsetForWrap(AWrapOffset: IntIdx; out ASubOffset: IntIdx): IntIdx; override;

    function TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint; ANodeStartLine: IntIdx): TPhysPoint; override;
    function ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint; ANodeStartLine: IntIdx): TPhysPoint; override;
  end;


  TLazSynEditWrapCaretPos = (wcpEOL, wcpBOL);

  { TLazSynDisplayWordWrap }

  TLazSynDisplayWordWrap = class(TLazSynDisplayLineMapping)
  private
    FWrapPlugin: TLazSynEditLineWrapPlugin;

    FCurSubLineLogStartIdx, FCurSubLineNextLogStartIdx, FCurSubLinePhysStartIdx: Integer;
    FCurToken: TLazSynDisplayTokenInfo;
    FCurLineLogIdx: Integer;
  public
    constructor Create(AWrappedView: TSynEditLineMappingView; AWrapPlugin: TLazSynEditLineWrapPlugin);
    //destructor Destroy; override;
    procedure SetHighlighterTokensLine(AWrappedLine: TLineIdx; out
      ARealLine: TLineIdx; out AStartBytePos, ALineByteLen: Integer); override;
    function GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean; override;
  end;

  { TLazSynEditLineWrapPlugin }

  TLazSynEditLineWrapPlugin = class(TLazSynEditPlugin)
  private
    FCaretWrapPos: TLazSynEditWrapCaretPos;
    procedure DoLinesChanged(Sender: TObject);
    procedure DoWidthChanged(Sender: TObject; Changes: TSynStatusChanges);
    function GetWrapColumn: Integer;
public
    FLineMapView: TSynEditLineMappingView;
    function CreatePageMapNode(AMapTree: TSynLineMapAVLTree
      ): TSynEditLineMapPage;
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); override;

    function CalculateNextBreak(ALine: PChar; ALogStartFrom: IntIdx; AMaxWidth: Integer;
      const PhysCharWidths: TPhysicalCharWidths; out APhysWidth: Integer): IntIdx;
    function  GetSublineCount (ALine: String; AMaxWidth: Integer; const APhysCharWidths: TPhysicalCharWidths): Integer; inline;
    procedure GetSublineBounds(ALine: String; AMaxWidth: Integer;
      const APhysCharWidths: TPhysicalCharWidths; ASubLine: Integer; out ALogStartX,
      ANextLogStartX, APhysStart: IntIdx; out APhysWidth: integer);
    function  GetSubLineFromX (ALine: String; AMaxWidth: Integer; const APhysCharWidths: TPhysicalCharWidths; var APhysXPos: Integer): integer;

    procedure GetWrapInfoForViewedXY(var AViewedXY: TPhysPoint; AFlags: TViewedXYInfoFlags; out AFirstViewedX: IntPos; ALogPhysConvertor: TSynLogicalPhysicalConvertor);

    function TextXYToLineXY(ATextXY: TPhysPoint): TPhysPoint;
    function LineXYToTextX(ARealLine: IntPos; ALineXY: TPhysPoint): Integer;
    function CalculateWrapForLine(ALineIdx: IntIdx; AMaxWidth: integer): Integer; inline;
  public
    constructor Create(AOwner: TComponent); override;

    procedure WrapAll; experimental;
    procedure ValidateAll; experimental;

    property CaretWrapPos: TLazSynEditWrapCaretPos read FCaretWrapPos write FCaretWrapPos;
    property WrapColumn: Integer read GetWrapColumn;
  end;

implementation

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
    assert((i >= c -1) or (Item[i+1].First > AToOffset+1), 'TSynWordWrapInvalidLines.InvalidateLines: (i < Cnt-1) or (Item[i+1].First > AToOffset+1)');
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
    WrapInfoCopyFromTo(
      @FWrappedExtraSums[i],
      @FWrappedExtraSums[0],
      FWrappedExtraSumsCount);
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

procedure TSynWordWrapLineMap.ValidateLine(ALineOffset, AWrappCount: Integer);
var
  i, j: Integer;
begin
  assert(ALineOffset >= 0, 'TSynWordWrapLineMap.ValidateLine: ALineOffset >= 0');
  assert((FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0), 'TSynWordWrapLineMap.ValidateLine: (FOffsetAtStart = 0) or (FWrappedExtraSumsCount > 0)');
  FInvalidLines.ValidateLines(ALineOffset);

  if ALineOffset - FOffsetAtStart < FDeferredAdjustFromOffs then begin
    EndValidate;
  end
  else
  if FDeferredAdjustFromOffs > 0 then begin
    j := FDeferredAdjustFromVal;
    //AWrappCount := AWrappCount + j;
    for i := FDeferredAdjustFromOffs to Min(FWrappedExtraSumsCount - 1, ALineOffset - FOffsetAtStart) do
      FWrappedExtraSums[i] := FWrappedExtraSums[i] + j;
    FDeferredAdjustFromOffs := 0;
  end;


  if ALineOffset < FOffsetAtStart then begin
    if AWrappCount <> 1 then begin
      i := FirstInvalidLine;
      if (i < 0) or (ALineOffset < i) then
        i := ALineOffset;
      j := FOffsetAtStart - i;
      GrowCapacity(FWrappedExtraSumsCount + j);
      WrapInfoMoveUpAndAdjustFromTo(
        @FWrappedExtraSums[0],
        @FWrappedExtraSums[j],
        FWrappedExtraSumsCount,
        AWrappCount - 1);
      WrapInfoFillFrom(
        @FWrappedExtraSums[i - ALineOffset],
        j - (i - ALineOffset),
        AWrappCount - 1);

      assert(FOffsetAtStart >= j, 'TSynWordWrapLineMap.ValidateLine: FOffsetAtStart >= j');
      FOffsetAtStart := FOffsetAtStart - j;
      FWrappedExtraSumsCount := FWrappedExtraSumsCount + j;
    end;
  end

  else
  begin
    ALineOffset := ALineOffset - FOffsetAtStart;
    if ALineOffset >= FWrappedExtraSumsCount then begin
      if AWrappCount > 1 then begin
        i := FWrappedExtraSumsCount;
  // TODO: check LastInvalidLine and SYN_WORD_WRAP_SPLIT_SIZE;
        GrowCapacity(ALineOffset + 1);
        FWrappedExtraSumsCount := ALineOffset + 1;
        WrapInfoFillFrom(
          @FWrappedExtraSums[i],
          FWrappedExtraSumsCount - i,
          GetWrappedExtraSumBefore(i));
        FWrappedExtraSums[ALineOffset] := AWrappCount - 1; // last element, no AdjustFrom() needed
        if ALineOffset > 0 then
          FWrappedExtraSums[ALineOffset] := FWrappedExtraSums[ALineOffset] + FWrappedExtraSums[ALineOffset-1];
      end;
    end

    else
    if (AWrappCount = 1) and (ALineOffset = FWrappedExtraSumsCount - 1) then begin
      if ALineOffset = 0 then begin
        FWrappedExtraSumsCount := 0;
        FOffsetAtStart := 0;
      end
      else begin
// TODO: defer if there are further invalid lines after ALineOffset
        i := ALineOffset - 1; // i = FWrappedExtraSumsCount - 2
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
      j := AWrappCount - 1 + GetWrappedExtraSumBefore(ALineOffset);
      FDeferredAdjustFromOffs := ALineOffset + 1;
      FDeferredAdjustFromVal  := FDeferredAdjustFromVal + j - FWrappedExtraSums[ALineOffset];
      FWrappedExtraSums[ALineOffset] := j;
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
begin
  assert(ATargetStartLine >= ADestPage.FWrappedExtraSumsCount + ADestPage.FOffsetAtStart, 'TSynWordWrapLineMap.InsertLinesFromPage: ATargetStartLine > ADestPage.FWrappedExtraSumsCount + ADestPage.FOffsetAtStart');

  FInvalidLines.MoveRangeAtStartTo(ADestPage.FInvalidLines, ASourceEndLine, ATargetStartLine);

  if (FWrappedExtraSumsCount = 0) then
    exit;

  if ADestPage.FWrappedExtraSumsCount = 0 then begin
    // Target page is empty
    ADestPage.FOffsetAtStart := ATargetStartLine + Min(Offset, ASourceEndLine);
    assert(ADestPage.FOffsetAtStart >= 0, 'TSynWordWrapLineMap.MoveLinesAtStartTo: ADestPage.FOffsetAtStart >= 0');
    ASourceEndLine := ASourceEndLine - Offset;
    if ASourceEndLine < 0 then begin
      ADestPage.FOffsetAtStart := 0;
      exit;
    end;

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

  ASourceEndLine := ASourceEndLine - Offset;
  if ASourceEndLine < 0 then
    exit;

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

  SrcO1 := GetWrappedExtraSumBefore(Min(ASourceStartLine,              FWrappedExtraSumsCount));
  SrcO2 := GetWrappedExtraSumBefore(Min(ASourceStartLine + ALineCount, FWrappedExtraSumsCount));
  MinLineCount := Max(0, Min(ALineCount, FWrappedExtraSumsCount - ASourceStartLine));

  if ADestPage.FWrappedExtraSumsCount = 0 then begin
    // Moving to an empty page. Do NOT include any lines after FWrappedExtraSumsCount
    if MinLineCount > 0 then begin;
      ADestPage.GrowCapacity(MinLineCount);
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

  ADestPage.GrowCapacity(ADestPage.FWrappedExtraSumsCount + ALineCount + OldOffset);
  WrapInfoMoveUpAndAdjustFromTo(
    @ADestPage.FWrappedExtraSums[0],
    @ADestPage.FWrappedExtraSums[ALineCount + OldOffset],
    ADestPage.FWrappedExtraSumsCount,
    SrcO2 - SrcO1);
  if MinLineCount > 0 then
    WrapInfoCopyAndAdjustFromTo(
      @FWrappedExtraSums[ASourceStartLine],
      @ADestPage.FWrappedExtraSums[0],
      MinLineCount,
      -SrcO1);
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
  i, j: Integer;
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
    WrapInfoCopyAndAdjustFromTo(
      @FWrappedExtraSums[ALineOffset + ALineCount],
      @FWrappedExtraSums[ALineOffset],
      FWrappedExtraSumsCount - (ALineCount + ALineOffset),
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

{ TSynWordWrapIndexPage }

function TSynWordWrapIndexPage.GetFirstInvalidLine: Integer;
begin
  Result := FSynWordWrapLineMap.FirstInvalidLine;
end;

function TSynWordWrapIndexPage.GetFirstInvalidEndLine: Integer;
begin
  Result := FSynWordWrapLineMap.FirstInvalidEndLine;
end;

function TSynWordWrapIndexPage.GetLastInvalidLine: Integer;
begin
  Result := FSynWordWrapLineMap.LastInvalidLine;
end;

function TSynWordWrapIndexPage.GetViewedRealCountDifference: Integer;
begin
  Result := FSynWordWrapLineMap.ViewedRealCountDifference;
end;

procedure TSynWordWrapIndexPage.UpdateViewedSizeDifference;
begin
  UpdateNodeSize(FSynWordWrapLineMap.ViewedRealCountDifference);
end;

function TSynWordWrapIndexPage.GetWrappedOffsetFor(ARealOffset: IntIdx): IntIdx;
begin
  Result := FSynWordWrapLineMap.GetWrappedOffsetFor(ARealOffset);
end;

function TSynWordWrapIndexPage.IsValid: boolean;
begin
  Result := FSynWordWrapLineMap.FInvalidLines.Count = 0;
end;

function TSynWordWrapIndexPage.CanExtendStartTo(ALineOffs: Integer;
  AIgnoreJoinDist: Boolean): boolean;
begin
  Result := (RealEndLine - ALineOffs < Tree.PageSplitSize) and
            (AIgnoreJoinDist or (RealStartLine - ALineOffs < Tree.PageJoinDistance));
end;

function TSynWordWrapIndexPage.CanExtendEndTo(ALineOffs: Integer;
  AIgnoreJoinDist: Boolean): boolean;
begin
  Result := (ALineOffs - RealStartLine < Tree.PageSplitSize) and
            (AIgnoreJoinDist or (ALineOffs - RealEndLine < Tree.PageJoinDistance));
end;

function TSynWordWrapIndexPage.GetOffsetForWrap(AWrapOffset: IntIdx; out
  ASubOffset: IntIdx): IntIdx;
begin
  Result := FSynWordWrapLineMap.GetOffsetForWrap(AWrapOffset, ASubOffset);
end;

function TSynWordWrapIndexPage.TextXYIdxToViewXYIdx(ATextXYIdx: TPhysPoint;
  ANodeStartLine: IntIdx): TPhysPoint;
var
  p: TPoint;
begin
  Result := inherited TextXYIdxToViewXYIdx(ATextXYIdx, ANodeStartLine);

  p := FSynEditWrappedPlugin.TextXYToLineXY(Result);
  Result.y := ANodeStartLine + GetWrappedOffsetFor(Result.y - ANodeStartLine);

  Result.x := p.x;
  Result.y := Result.y + p.y;
end;

function TSynWordWrapIndexPage.ViewXYIdxToTextXYIdx(AViewXYIdx: TPhysPoint;
  ANodeStartLine: IntIdx): TPhysPoint;
var
  SubOffset: Integer;
begin
  Result := inherited ViewXYIdxToTextXYIdx(AViewXYIdx, ANodeStartLine);
  Result.y := ANodeStartLine + GetOffsetForWrap(Result.y - ANodeStartLine, SubOffset);

  Result.x := FSynEditWrappedPlugin.LineXYToTextX(Result.y, Point(Result.x, SubOffset) );
end;

procedure TSynWordWrapIndexPage.InvalidateLines(AFromOffset, AToOffset: Integer);
begin
  FSynWordWrapLineMap.InvalidateLines(AFromOffset, AToOffset);
end;

function TSynWordWrapIndexPage.ExtendAndInvalidateLines(AFromLineIdx,
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

constructor TSynWordWrapIndexPage.Create(ATree: TSynLineMapAVLTree);
begin
  FSynWordWrapLineMap := TSynWordWrapLineMap.Create;
  FSynWordWrapLineMap.FAvlNode := Self;
  inherited Create(ATree);
end;

destructor TSynWordWrapIndexPage.Destroy;
begin
  FSynWordWrapLineMap.Destroy;
  inherited Destroy;
end;

procedure TSynWordWrapIndexPage.DumpNode(ALine: Integer; AnIndent: Integer);
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

procedure TSynWordWrapIndexPage.AdjustForLinesInserted(AStartLine,
  ALineCount: IntIdx; ABytePos: Integer);
var
  rs, re, LineOffs, dummy, Cnt: Integer;
  NextPage, PrevPage: TSynEditLineMapPage;
begin
  assert(AStartLine >= 0, 'TSynWordWrapIndexPage.AdjustForLinesInserted: AStartLine >= 0');

  if (AStartLine <= RealStartLine) or (AStartLine > RealEndLine) then begin
    InsertLinesAtOffset(AStartLine, ALineCount);
    if AStartLine = 0 then
      AdjustPosition(-ALineCount);
    exit;
  end;

  (* This node was NOT moved by the callers AdjustForLinesInserted.
     This would only have happened if AStartLine = 0
  *)
  assert(RealCount > 0, 'TSynWordWrapIndexPage.InsertLines: RealCount > 0');

  rs := RealStartLine;
  re := RealEndLine;
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

//  inherited AdjustForLinesInserted(AStartLine, ALineCount, ABytePos);
end;

procedure TSynWordWrapIndexPage.AdjustForLinesDeleted(AStartLine,
  ALineCount: IntIdx; ABytePos: Integer);
begin
  DeleteLinesAtOffset(AStartLine, ALineCount);
end;

procedure TSynWordWrapIndexPage.InsertLinesAtOffset(ALineOffset,
  ALineCount: IntIdx);
begin
  FSynWordWrapLineMap.InsertLinesAtOffset(ALineOffset, ALineCount);
end;

procedure TSynWordWrapIndexPage.DeleteLinesAtOffset(ALineOffset,
  ALineCount: IntIdx; ADoNotShrink: Boolean);
begin
  FSynWordWrapLineMap.DeleteLinesAtOffset(ALineOffset, ALineCount, ADoNotShrink);
end;

procedure TSynWordWrapIndexPage.MoveLinesAtStartTo(
  ADestPage: TSynEditLineMapPage; ASourceEndLine, ATargetStartLine: Integer);
begin
// TODO: adestpage <> TSynWordWrapIndexPage
  assert(ADestPage is TSynWordWrapIndexPage, 'TSynWordWrapIndexPage.MoveLinesAtStartTo: ADestPage is TSynWordWrapIndexPage');
  FSynWordWrapLineMap.MoveLinesAtStartTo(TSynWordWrapIndexPage(ADestPage).FSynWordWrapLineMap, ASourceEndLine, ATargetStartLine);
  FSynWordWrapLineMap.DeleteLinesAtOffset(0, ASourceEndLine + 1);
end;

procedure TSynWordWrapIndexPage.MoveLinesAtEndTo(
  ADestPage: TSynEditLineMapPage; ASourceStartLine, ACount: Integer);
begin
// TODO: adestpage <> TSynWordWrapIndexPage
  assert(ADestPage is TSynWordWrapIndexPage, 'TSynWordWrapIndexPage.MoveLinesAtEndTo: ADestPage is TSynWordWrapIndexPage');
  FSynWordWrapLineMap.MoveLinesAtEndTo(TSynWordWrapIndexPage(ADestPage).FSynWordWrapLineMap, ASourceStartLine, ACount);
  FSynWordWrapLineMap.DeleteLinesAtOffset(ASourceStartLine, ACount);
end;

procedure TSynWordWrapIndexPage.EndValidate;
begin
  FSynWordWrapLineMap.EndValidate;
end;

procedure TSynWordWrapIndexPage.ValidateLine(ALineOffset, AWrappCount: Integer);
begin
  FSynWordWrapLineMap.ValidateLine(ALineOffset, AWrappCount);
end;

function TSynWordWrapIndexPage.RealCount: Integer;
begin
  Result := FSynWordWrapLineMap.RealCount;
end;

function TSynWordWrapIndexPage.RealStartLine: Integer;
begin
  Result := FSynWordWrapLineMap.Offset;
end;

function TSynWordWrapIndexPage.RealEndLine: Integer;
begin
  Result := FSynWordWrapLineMap.Offset + FSynWordWrapLineMap.RealCount - 1;
end;

{ TLazSynDisplayWordWrap }

constructor TLazSynDisplayWordWrap.Create(AWrappedView: TSynEditLineMappingView;
  AWrapPlugin: TLazSynEditLineWrapPlugin);
begin
  FWrapPlugin := AWrapPlugin;
  inherited Create(AWrappedView);
end;

procedure TLazSynDisplayWordWrap.SetHighlighterTokensLine(
  AWrappedLine: TLineIdx; out ARealLine: TLineIdx; out AStartBytePos,
  ALineByteLen: Integer);
var
  IsNext: Boolean;
  PrevSub: IntIdx;
  LineTxt: String;
  PWidth: TPhysicalCharWidths;
  PhysWidth, MaxW: Integer;
begin
  IsNext := (AWrappedLine = FCurWrappedLine + 1) and (FCurWrappedLine >= 0);
  PrevSub := FCurrentWrapSubline;

  inherited SetHighlighterTokensLine(AWrappedLine, ARealLine, AStartBytePos, ALineByteLen);

  LineTxt := FLineMappingView.NextLines.Strings[ARealLine];
  FLineMappingView.LogPhysConvertor.CurrentLine := ARealLine;
  PWidth := FLineMappingView.LogPhysConvertor.CurrentWidthsDirect;
  //PWidth  := FLineMappingView.GetPhysicalCharWidths(ARealLine);
  MaxW    := FWrapPlugin.WrapColumn;
  if IsNext and (FCurrentWrapSubline = PrevSub + 1) then begin
    FCurSubLineLogStartIdx := FCurSubLineNextLogStartIdx;
    FCurSubLineNextLogStartIdx := FWrapPlugin.CalculateNextBreak(PChar(LineTxt), FCurSubLineNextLogStartIdx,
      MaxW, PWidth, PhysWidth);
    FCurSubLinePhysStartIdx := FCurSubLinePhysStartIdx + PhysWidth;
  end
  else begin
    FWrapPlugin.GetSublineBounds(LineTxt, MaxW, PWidth, FCurrentWrapSubline,
      FCurSubLineLogStartIdx, FCurSubLineNextLogStartIdx, FCurSubLinePhysStartIdx, PhysWidth);
  end;
  AStartBytePos := AStartBytePos + FCurSubLineLogStartIdx;
  ALineByteLen := FCurSubLineNextLogStartIdx - FCurSubLineLogStartIdx;

  FCurLineLogIdx := 0;
end;

function TLazSynDisplayWordWrap.GetNextHighlighterToken(out
  ATokenInfo: TLazSynDisplayTokenInfo): Boolean;
var
  PreStart: Integer;
begin
  If FCurLineLogIdx >= FCurSubLineNextLogStartIdx then begin
    Result := False;
    exit;
  end;

  repeat
    PreStart := FCurSubLineLogStartIdx - FCurLineLogIdx;
    Result := inherited GetNextHighlighterToken(ATokenInfo);
    if (not Result) or (ATokenInfo.TokenLength <= 0) then begin
      exit;
    end;
    FCurToken := ATokenInfo;

    FCurLineLogIdx := FCurLineLogIdx + ATokenInfo.TokenLength;
  until FCurLineLogIdx > FCurSubLineLogStartIdx;

  if PreStart > 0 then begin
    ATokenInfo.TokenStart := ATokenInfo.TokenStart + PreStart;
    ATokenInfo.TokenLength := ATokenInfo.TokenLength - PreStart;
    Result := ATokenInfo.TokenLength > 0;
    if not Result then
      exit;
  end;


  If FCurLineLogIdx > FCurSubLineNextLogStartIdx then begin
    ATokenInfo.TokenLength := ATokenInfo.TokenLength - (FCurLineLogIdx - FCurSubLineNextLogStartIdx);
    Result := ATokenInfo.TokenLength > 0;
  end;
end;

{ TLazSynEditLineWrapPlugin }

procedure TLazSynEditLineWrapPlugin.DoLinesChanged(Sender: TObject);
begin
  ValidateAll;
end;

procedure TLazSynEditLineWrapPlugin.DoWidthChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  FLineMapView.KnownLengthOfLongestLine := WrapColumn;
  FLineMapView.InvalidateLines(0, FLineMapView.NextLines.Count);
end;

function TLazSynEditLineWrapPlugin.GetWrapColumn: Integer;
begin
  Result := TSynEdit(Editor).CharsInWindow;
end;

function TLazSynEditLineWrapPlugin.CreatePageMapNode(AMapTree: TSynLineMapAVLTree): TSynEditLineMapPage;
begin
  Result := TSynWordWrapIndexPage.Create(AMapTree);
  TSynWordWrapIndexPage(Result).FSynEditWrappedPlugin := Self;
end;

procedure TLazSynEditLineWrapPlugin.SetEditor(const AValue: TCustomSynEdit);
begin
  if (Editor <> nil) and (AValue <> nil) then
    raise Exception.Create('Not allowed to change editor');
  inherited SetEditor(AValue);
end;

function TLazSynEditLineWrapPlugin.CalculateNextBreak(ALine: PChar;
  ALogStartFrom: IntIdx; AMaxWidth: Integer;
  const PhysCharWidths: TPhysicalCharWidths; out APhysWidth: Integer): IntIdx;
const
  // todo, other break chars // utf8
  BREAKCHARS = [#9, #32, '.', ',', ':', ';', '=', '-', '+', '*', '/', '(', ')', '{', '}', '[', ']', '!', '<', '>'];
var
  PhysWidthPtr: PByte;
  CurCharPhysWidth: Cardinal;
  LastGoodPos: PChar;
begin
  if (ALine = nil) or (ALine^ = #0) then
    exit(0);

  PhysWidthPtr := @PhysCharWidths[ALogStartFrom];
  APhysWidth := AMaxWidth;
  Result := ALogStartFrom;
  ALine := ALine + ALogStartFrom;
  LastGoodPos := ALine;

  while ALine <> nil do begin
    if ALine^ in BREAKCHARS then
      while ALine^ in BREAKCHARS do begin
        CurCharPhysWidth := PhysWidthPtr^ and PCWMask;
        if CurCharPhysWidth <= AMaxWidth then begin
          inc(ALine);
          inc(PhysWidthPtr);
          inc(Result);
          dec(AMaxWidth, CurCharPhysWidth);
        end
        else begin
          ALine := nil; // break outer loop
          break;
        end;
      end

    else begin
      CurCharPhysWidth := 0;
      LastGoodPos := ALine;
      while (ALine^ <> #0) and not (ALine^ in BREAKCHARS) do begin
        CurCharPhysWidth := CurCharPhysWidth + PhysWidthPtr^ and PCWMask;
        inc(ALine);
        inc(PhysWidthPtr);
      end;

      if (CurCharPhysWidth > 0) and (CurCharPhysWidth <= AMaxWidth) then begin
        inc(Result, ALine-LastGoodPos);
        dec(AMaxWidth, CurCharPhysWidth);
      end
      else begin
        ALine := nil; // break outer loop
        break;
      end;
    end;
  end;

  if Result = ALogStartFrom then begin
    PhysWidthPtr := @PhysCharWidths[0];
    ALine := LastGoodPos;
    while ALine^ <> #0 do begin
      CurCharPhysWidth := PhysWidthPtr^ and PCWMask;
      if (CurCharPhysWidth <= AMaxWidth) or (Result = ALogStartFrom) then begin
        inc(ALine);
        inc(PhysWidthPtr);
        inc(Result);
        dec(AMaxWidth, CurCharPhysWidth);
      end
      else
        break;
    end;
  end;
  APhysWidth := APhysWidth - AMaxWidth;
end;

function TLazSynEditLineWrapPlugin.GetSublineCount(ALine: String;
  AMaxWidth: Integer; const APhysCharWidths: TPhysicalCharWidths): Integer;
var
  x, dummy: Integer;
begin
  Result := 1;
  if Length(ALine) = 0 then
    exit;
  x := CalculateNextBreak(PChar(ALine), 0, AMaxWidth, APhysCharWidths, dummy);
  while (x < Length(ALine)) do begin
    inc(Result);
    x := CalculateNextBreak(PChar(ALine), x, AMaxWidth, APhysCharWidths, dummy);
  end;
end;

procedure TLazSynEditLineWrapPlugin.GetSublineBounds(ALine: String;
  AMaxWidth: Integer; const APhysCharWidths: TPhysicalCharWidths; ASubLine: Integer;
  out ALogStartX, ANextLogStartX, APhysStart: IntIdx; out APhysWidth: integer);
begin
  ALogStartX := 0;
  ANextLogStartX := 0;
  APhysStart := 0;
  if Length(ALine) = 0 then
    exit;
  ANextLogStartX := CalculateNextBreak(PChar(ALine), ALogStartX, AMaxWidth, APhysCharWidths, APhysWidth);
  while ASubLine > 0 do begin
    ALogStartX := ANextLogStartX;
    APhysStart := APhysStart + APhysWidth;
    ANextLogStartX := CalculateNextBreak(PChar(ALine), ALogStartX, AMaxWidth, APhysCharWidths, APhysWidth);
    dec(ASubLine);
  end;
end;

function TLazSynEditLineWrapPlugin.GetSubLineFromX(ALine: String;
  AMaxWidth: Integer; const APhysCharWidths: TPhysicalCharWidths;
  var APhysXPos: Integer): integer;
var
  x, PhysWidth: Integer;
begin
  Result := 0;
  if Length(ALine) = 0 then
    exit;
  Result := -1;
  x := 0;
  APhysXPos := ToIdx(APhysXPos);
  while (x < Length(ALine)) do begin
    inc(Result);
    x := CalculateNextBreak(PChar(ALine), x, AMaxWidth, APhysCharWidths, PhysWidth);
    if x >= Length(ALine) then
      break;
    if (FCaretWrapPos = wcpBOL) and (PhysWidth = APhysXPos) and (x < Length(ALine))
    then begin
      inc(Result);
      APhysXPos := APhysXPos - PhysWidth;
      break;
    end;
    if PhysWidth >= APhysXPos then
      break;
    APhysXPos := APhysXPos - PhysWidth;
  end;
  APhysXPos := ToPos(APhysXPos);
end;

procedure TLazSynEditLineWrapPlugin.GetWrapInfoForViewedXY(
  var AViewedXY: TPhysPoint; AFlags: TViewedXYInfoFlags;
  out AFirstViewedX: IntPos; ALogPhysConvertor: TSynLogicalPhysicalConvertor);
var
  SubLineOffset, YIdx: TLineIdx;
  LineTxt: String;
  PWidth: TPhysicalCharWidths;
  LogX, NextLogX, PhysX: IntIdx;
  PhysWidth: Integer;
begin

  YIdx := FLineMapView.Tree.GetLineForForWrap(ToIdx(AViewedXY.y), SubLineOffset);
  YIdx := FLineMapView.NextLines.ViewToTextIndex(YIdx);

  LineTxt := FLineMapView.Strings[YIdx];
  ALogPhysConvertor.CurrentLine := YIdx;
  PWidth  := ALogPhysConvertor.CurrentWidthsDirect;

  GetSublineBounds(LineTxt, WrapColumn, PWidth, SubLineOffset, LogX, NextLogX, PhysX, PhysWidth);

  case CaretWrapPos of
    wcpEOL: begin
        if (SubLineOffset > 0) and (AViewedXY.x <= 1) then
          AViewedXY.x := 2
        else
        if (NextLogX < length(LineTxt)) and (AViewedXY.x > ToPos(PhysWidth)) then
          AViewedXY.x := ToPos(PhysWidth);
        AFirstViewedX := 2;
      end;
    wcpBOL: begin
        if (NextLogX < length(LineTxt)) and (AViewedXY.x >= ToPos(PhysWidth)) then
          AViewedXY.x := ToPos(PhysWidth) - 1;
        AFirstViewedX := 1;
      end;
  end;

  AViewedXY.y := ToPos(YIdx);
  AViewedXY.x := AViewedXY.x + PhysX;
end;

function TLazSynEditLineWrapPlugin.TextXYToLineXY(ATextXY: TPhysPoint
  ): TPhysPoint;
begin
  FLineMapView.LogPhysConvertor.CurrentLine := ATextXY.y;
  Result.x := ATextXY.x;
  Result.y :=
    GetSubLineFromX(FLineMapView.NextLines.Strings[ATextXY.y],
      WrapColumn,
      FLineMapView.LogPhysConvertor.CurrentWidthsDirect,
      Result.x
    );
end;

function TLazSynEditLineWrapPlugin.LineXYToTextX(ARealLine: IntPos;
  ALineXY: TPhysPoint): Integer;
var
  dummy, dummy2: IntIdx;
  dummy3: integer;
begin
  FLineMapView.LogPhysConvertor.CurrentLine := ARealLine;
  GetSublineBounds(FLineMapView.NextLines.Strings[ARealLine],
    WrapColumn,
    FLineMapView.LogPhysConvertor.CurrentWidthsDirect,
    ALineXY.y, dummy, dummy2, Result, dummy3
  );
  Result := Result + ALineXY.x;
end;

function TLazSynEditLineWrapPlugin.CalculateWrapForLine(ALineIdx: IntIdx;
  AMaxWidth: integer): Integer;
begin
  FLineMapView.LogPhysConvertor.CurrentLine := ALineIdx;
  Result := GetSublineCount(FLineMapView.NextLines.Strings[ALineIdx], AMaxWidth,
    FLineMapView.LogPhysConvertor.CurrentWidthsDirect);
end;

constructor TLazSynEditLineWrapPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineMapView := TSynEditLineMappingView(TSynEdit(Editor).TextViewsManager.SynTextViewByClass[TSynEditLineMappingView]);
  if FLineMapView = nil then begin
    FLineMapView := TSynEditLineMappingView.Create;
    FLineMapView.SetDisplayView(TLazSynDisplayWordWrap.Create(FLineMapView, Self));
    TSynEdit(Editor).TextViewsManager.AddTextView(FLineMapView);
  end;
  if FLineMapView.PageMapCreator <> nil then
    raise Exception.Create('Conflicting Plugin detected');

  FLineMapView.PageMapCreator := @CreatePageMapNode;
  FLineMapView.WrapInfoForViewedXYProc := @GetWrapInfoForViewedXY;
  FLineMapView.AddLinesChangedHandler(@DoLinesChanged);
  TSynEdit(Editor).RegisterStatusChangedHandler(@DoWidthChanged, [scCharsInWindow]);
  FLineMapView.KnownLengthOfLongestLine := WrapColumn;
  WrapAll;
end;

procedure TLazSynEditLineWrapPlugin.WrapAll;
var
  c: Integer;
begin
  FLineMapView.Tree.Clear;
  c := FLineMapView.NextLines.Count;
  if c > 0 then
    FLineMapView.Tree.AdjustForLinesInserted(0, c, 0);
  ValidateAll;
end;

procedure TLazSynEditLineWrapPlugin.ValidateAll;
var
  AMaxWidth, i, w: Integer;
  LowLine, HighLine: TLineIdx;
begin
if not FLineMapView.Tree.NeedsValidation then exit;
  AMaxWidth := WrapColumn;

  while FLineMapView.Tree.NextBlockForValidation(LowLine, HighLine) do begin
    for i := LowLine to HighLine do begin
      w := CalculateWrapForLine(i, AMaxWidth);
      FLineMapView.Tree.ValidateLine(i, w);
    end;
  end;
  FLineMapView.Tree.EndValidate;
  FLineMapView.SendNotification(senrLineMappingChanged, FLineMapView, 0, 0);
  TSynEdit(Editor).Invalidate;
end;

end.


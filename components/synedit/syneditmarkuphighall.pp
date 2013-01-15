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
  Classes, SysUtils, ExtCtrls, SynEditMarkup, SynEditTypes, SynEditSearch,
  SynEditMiscClasses, Controls, LCLProc, SynEditHighlighter, SynEditPointClasses,
  SynEditMiscProcs, SynEditFoldedView;

type

  { TSynMarkupHighAllMatch }
  TSynMarkupHighAllMatch = Record
    StartPoint, EndPoint : TPoint;
  end;

  { TSynMarkupHighAllMatchList }
  TSynMarkupHighAllMatchList = class(TObject)
  private
    FCount, FCapacity : Integer;
    FMatches : Array of TSynMarkupHighAllMatch;
    
    function GetEndPoint(const Index : Integer) : TPoint;
    function GetPoint(const Index : Integer) : TPoint;
    function GetPointCount : Integer;
    function GetStartPoint(const Index : Integer) : TPoint;
    procedure SetCount(const AValue : Integer);
    function  GetMatch(const Index : Integer) : TSynMarkupHighAllMatch;
    procedure SetEndPoint(const Index : Integer; const AValue : TPoint);
    procedure SetMatch(const Index : Integer; const AValue : TSynMarkupHighAllMatch);
    procedure SetStartPoint(const Index : Integer; const AValue : TPoint);
  public
    constructor Create;
    Function MaybeReduceCapacity : Boolean;
    function IndexOfFirstMatchForLine(ALine: Integer): Integer;
    function IndexOfLastMatchForLine(ALine: Integer): Integer;
    procedure Delete(AIndex: Integer; ACount: Integer = 1);
    procedure Insert(AIndex: Integer; ACount: Integer = 1);
    procedure Insert(AIndex: Integer; AStartPoint, AEndPoint: TPoint);
    property Count : Integer read FCount write SetCount;
    property Match [const Index : Integer] : TSynMarkupHighAllMatch read GetMatch write SetMatch; default;
    property StartPoint [const Index : Integer] : TPoint read GetStartPoint write SetStartPoint;
    property EndPoint   [const Index : Integer] : TPoint read GetEndPoint write SetEndPoint;
    property PointCount : Integer read GetPointCount;
    property Point      [const Index : Integer] : TPoint read GetPoint;
  end;
  
  
  { TSynEditMarkupHighlightAll }

  TSynEditMarkupHighlightAll = class(TSynEditMarkup)
  private
    FFoldView: TSynEditFoldedView;
    FSearchString : string;            // for highlighting all occurences of a word/term
    FSearchOptions: TSynSearchOptions;
    FSearchStringMaxLines: Integer;
    FSearch: TSynEditSearch;
    FNextPosIdx, FNextPosRow: Integer;
    FNeedValidate, FNeedValidatePaint: Boolean;
    FMarkupEnabled: Boolean;

    FStartPoint : TPoint;        // First found position, before TopLine of visible area
    FSearchedEnd: TPoint;
    FMatches : TSynMarkupHighAllMatchList;
    FFirstInvalidLine, FLastInvalidLine: Integer;
    FHideSingleMatch: Boolean;

    function GetMatchCount: Integer;
    procedure SetFoldView(AValue: TSynEditFoldedView);
    procedure SetHideSingleMatch(AValue: Boolean);
    procedure SetSearchOptions(const AValue : TSynSearchOptions);
    procedure DoFoldChanged(aLine: Integer);

    Procedure ValidateMatches(SkipPaint: Boolean = False);

    function  SearchStringMaxLines: Integer;
    procedure FindInitialize;
    function  FindMatches(AStartPoint, AEndPoint: TPoint;
                          var AIndex: Integer;
                          AStopAfterLine: Integer = -1; // AEndPoint may be set further down, for multi-line matches
                          ABackward : Boolean = False
                         ): TPoint; // returns searhed until point
  protected
    procedure SetSearchString(const AValue : String); virtual;

    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoTextChanged(StartLine, EndLine: Integer); override; // 1 based
    function  HasVisibleMatch: Boolean; // does not check, if in visible line range. Only Count and DideSingleMatch
    property  MatchCount: Integer read GetMatchCount;
    property  MarkupEnabled: Boolean read FMarkupEnabled;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure IncPaintLock; override;
    procedure DecPaintLock; override;

    procedure PrepareMarkupForRow(aRow: Integer); override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    // AFirst/ ALast are 1 based
    Procedure Invalidate(SkipPaint: Boolean = False);
    Procedure InvalidateLines(AFirstLine: Integer = 0; ALastLine: Integer = 0; SkipPaint: Boolean = False);
    Procedure SendLineInvalidation(AFirstIndex: Integer = -1;ALastIndex: Integer = -1);

    property FoldView: TSynEditFoldedView read FFoldView write SetFoldView;

    property SearchString : String read fSearchString write SetSearchString;
    property SearchOptions : TSynSearchOptions read fSearchOptions write SetSearchOptions;
    property HideSingleMatch: Boolean read FHideSingleMatch write SetHideSingleMatch;
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
    procedure SetSearchString(const AValue : String); override;
    procedure SelectionChanged(Sender: TObject);
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTextChanged(StartLine, EndLine : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
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

{ TSynMarkupHighAllLines }

(*
constructor TSynMarkupHighAllLines.Create;
begin
  FCount := 0;
  FCapacity := 128;
  SetLength(FLines, FCapacity);
end;

function TSynMarkupHighAllLines.MaybeReduceCapacity : Boolean;
begin
  if not( (FCapacity > 128) and (FCapacity > FCount*4) )
  then exit(False);

  FCapacity := FCapacity div 2;
  SetLength(FLines, FCapacity);
  result := true;
end;

procedure TSynMarkupHighAllLines.SetCount(const AValue : Integer);

begin
  if FCount=AValue then exit;
  FCount:=AValue;

  if MaybeReduceCapacity
  then exit;
  if (FCapacity <= FCount)
  then FCapacity := FCapacity * 2
  else exit;
  SetLength(FLines, FCapacity);
end;

function TSynMarkupHighAllLines.GetLine(const Index : Integer) : TSynMarkupHighAllLine;
begin
  Result := FLines[Index];
end;

procedure TSynMarkupHighAllLines.SetLine(const Index : Integer; const AValue : TSynMarkupHighAllLine);
begin
//  if Index = Count  then Count := Count + 1; // AutoIncrease
  FLines[Index] := AValue;
end;
*)

{ TSynMarkupHighAllPosList }

constructor TSynMarkupHighAllMatchList.Create;
begin
  FCount := 0;
  FCapacity := 256;
  SetLength(FMatches, FCapacity);
end;

function TSynMarkupHighAllMatchList.MaybeReduceCapacity : Boolean;
begin
  if not( (FCapacity > 512) and (FCapacity > FCount*4) )
  then exit(False);

  FCapacity := FCapacity div 2;
  SetLength(FMatches, FCapacity);
  result := true;
end;

function TSynMarkupHighAllMatchList.IndexOfFirstMatchForLine(ALine: Integer): Integer;
var
  l, h: Integer;
begin
  if FCount = 0 then
    exit(-1);
  l := 0;
  h := FCount -1;
  Result := (l+h) div 2;
  while (h > l) do begin
    if FMatches[Result].EndPoint.y >= ALine then
      h := Result
    else
      l := Result + 1;
    Result := (l+h) div 2;
  end;
  if (FMatches[Result].EndPoint.y < ALine) then
    Result := -1;
end;

function TSynMarkupHighAllMatchList.IndexOfLastMatchForLine(ALine: Integer): Integer;
var
  l, h: Integer;
begin
  if FCount = 0 then
    exit(-1);
  l := 0;
  h := FCount -1;
  Result := (l+h) div 2;
  while (h > l) do begin
    if FMatches[Result].StartPoint.y <= ALine then
      l := Result + 1
    else
      h := Result;
    Result := (l+h) div 2;
  end;
  if (FMatches[Result].StartPoint.y > ALine) then
    Result := -1;
end;

procedure TSynMarkupHighAllMatchList.Delete(AIndex: Integer; ACount: Integer);
begin
  if AIndex >= FCount then
    exit;
  if AIndex + ACount > FCount then
    ACount := FCount - AIndex
  else
  if AIndex + ACount < FCount then
    System.Move(FMatches[AIndex+ACount], FMatches[AIndex], (FCount-AIndex-ACount)*SizeOf(TSynMarkupHighAllMatch));
  Count := Count - ACount;
end;

procedure TSynMarkupHighAllMatchList.Insert(AIndex: Integer; ACount: Integer);
begin
  if AIndex > FCount then
    exit;
  Count := FCount + ACount;
  if AIndex < FCount then
    System.Move(FMatches[AIndex], FMatches[AIndex+ACount], (FCount-AIndex-ACount)*SizeOf(TSynMarkupHighAllMatch));
end;

procedure TSynMarkupHighAllMatchList.Insert(AIndex: Integer; AStartPoint, AEndPoint: TPoint);
begin
  Insert(AIndex);
  FMatches[AIndex].StartPoint := AStartPoint;
  FMatches[AIndex].EndPoint   := AEndPoint;
end;

procedure TSynMarkupHighAllMatchList.SetCount(const AValue : Integer);
begin
  if FCount=AValue then exit;
  FCount:=AValue;

  if MaybeReduceCapacity
  then exit;
  if (FCapacity <= FCount)
  then FCapacity := FCapacity * 2
  else exit;
  SetLength(FMatches, FCapacity);
end;

function TSynMarkupHighAllMatchList.GetPointCount : Integer;
begin
  result := Count * 2;
end;

function TSynMarkupHighAllMatchList.GetPoint(const Index : Integer) : TPoint;
begin
  if (Index and 1) = 0
  then Result := FMatches[Index>>1].StartPoint
  else Result := FMatches[Index>>1].EndPoint
end;

function TSynMarkupHighAllMatchList.GetStartPoint(const Index : Integer) : TPoint;
begin
  Result := FMatches[Index].StartPoint;
end;

procedure TSynMarkupHighAllMatchList.SetStartPoint(const Index : Integer; const AValue : TPoint);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  FMatches[Index].StartPoint := AValue;
end;

function TSynMarkupHighAllMatchList.GetEndPoint(const Index : Integer) : TPoint;
begin
  Result := FMatches[Index].EndPoint;
end;

procedure TSynMarkupHighAllMatchList.SetEndPoint(const Index : Integer; const AValue : TPoint);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  FMatches[Index].EndPoint := AValue;
end;

function TSynMarkupHighAllMatchList.GetMatch(const index : Integer) : TSynMarkupHighAllMatch;
begin
  Result := FMatches[Index];
end;

procedure TSynMarkupHighAllMatchList.SetMatch(const index : Integer; const AValue : TSynMarkupHighAllMatch);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  FMatches[Index] := AValue;
end;


{ TSynEditMarkupHighlightAll }

constructor TSynEditMarkupHighlightAll.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  fStartPoint.y := -1;
  FSearchedEnd.y := -1;
  fSearch := TSynEditSearch.Create;
  fMatches := TSynMarkupHighAllMatchList.Create;
  fSearchString:='';
  FFirstInvalidLine := 1;
  FLastInvalidLine := MaxInt;
  FHideSingleMatch := False;
  FMarkupEnabled := MarkupInfo.IsEnabled;
end;

destructor TSynEditMarkupHighlightAll.Destroy;
begin
  FoldView := nil;
  FreeAndNil(fSearch);
  FreeAndNil(fMatches);
  inherited Destroy;
end;

procedure TSynEditMarkupHighlightAll.IncPaintLock;
begin
  if FPaintLock = 0 then begin
    FNeedValidatePaint := False;
    FNeedValidate := False;
  end;
  inherited IncPaintLock;
end;

procedure TSynEditMarkupHighlightAll.DecPaintLock;
begin
  inherited DecPaintLock;
  if (FPaintLock = 0) and FNeedValidate then
    ValidateMatches(not FNeedValidatePaint);
end;

procedure TSynEditMarkupHighlightAll.SetSearchOptions(const AValue : TSynSearchOptions);
begin
  if fSearchOptions = AValue then exit;
  fSearchOptions := AValue;
  FSearchStringMaxLines := -1;
  Invalidate;
end;

procedure TSynEditMarkupHighlightAll.SetSearchString(const AValue : String);
begin
  if fSearchString = AValue then exit;
  fSearchString := AValue;
  FSearchStringMaxLines := -1;
  //DebugLnEnter(['TSynEditMarkupHighlightAll.SetSearchString ', fSearchString]);
  Invalidate; // bad if options and string search at the same time *and* string is <> ''
  //DebugLnExit(['TSynEditMarkupHighlightAll.SetSearchString ']);
end;

procedure TSynEditMarkupHighlightAll.DoTopLineChanged(OldTopLine : Integer);
begin
  // {TODO: Only do a partial search on the new area}
  //DebugLnEnter(['TSynEditMarkupHighlightAll.DoTopLineChanged ',FSearchString]);
  ValidateMatches(True);
  //DebugLnExit(['TSynEditMarkupHighlightAll.DoTopLineChanged ']);
end;

procedure TSynEditMarkupHighlightAll.DoLinesInWindoChanged(OldLinesInWindow : Integer);
begin
  // {TODO: Only do a partial search on the new area}
  //DebugLnEnter(['TSynEditMarkupHighlightAll.DoLinesInWindoChanged ',FSearchString]);
  ValidateMatches(True);
  //DebugLnExit(['TSynEditMarkupHighlightAll.DoLinesInWindoChanged ']);
end;

procedure TSynEditMarkupHighlightAll.DoMarkupChanged(AMarkup : TSynSelectedColor);
begin
  If (not FMarkupEnabled) and MarkupInfo.IsEnabled then
    Invalidate
  else
    SendLineInvalidation;
  FMarkupEnabled := MarkupInfo.IsEnabled;
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

function TSynEditMarkupHighlightAll.GetMatchCount: Integer;
begin
  Result := fMatches.Count;
end;

procedure TSynEditMarkupHighlightAll.SetFoldView(AValue: TSynEditFoldedView);
begin
  if FFoldView = AValue then Exit;

  if FFoldView <> nil then
    FFoldView.RemoveFoldChangedHandler(@DoFoldChanged);

  FFoldView := AValue;

  if FFoldView <> nil then
    FFoldView.AddFoldChangedHandler(@DoFoldChanged);
end;

function TSynEditMarkupHighlightAll.FindMatches(AStartPoint, AEndPoint: TPoint;
  var AIndex: Integer; AStopAfterLine: Integer; ABackward: Boolean): TPoint;
var
  ptFoundStart, ptFoundEnd: TPoint;
begin
  //debugln(['FindMatches IDX=', AIndex,' # ',dbgs(AStartPoint),' - ',dbgs(AEndPoint), ' AStopAfterLine=',AStopAfterLine, ' cnt=',FMatches.Count, ' Back=', dbgs(ABackward)]);
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
  //debugln(['FindMatches IDX=', AIndex, ' ## ',dbgs(Result)]);
end;

procedure TSynEditMarkupHighlightAll.SetHideSingleMatch(AValue: Boolean);
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

procedure TSynEditMarkupHighlightAll.DoFoldChanged(aLine: Integer);
begin
  InvalidateLines(aLine+1, MaxInt, True);
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

procedure TSynEditMarkupHighlightAll.ValidateMatches(SkipPaint: Boolean);

  function IsPosValid(APos: TPoint): Boolean; // Check if point is in invalid range
  begin
    Result := (APos.y > 0) and
       ( (FFirstInvalidLine < 1) or (APos.y < FFirstInvalidLine) or
         ( (FLastInvalidLine > 0) and (APos.y > FLastInvalidLine) )
       );
  end;

  function IsStartAtMatch0: Boolean; // Check if FStartPoint = FMatches[0]
  begin
    Result := (FMatches.Count > 0) and
              (FStartPoint.y = FMatches.EndPoint[0].y)and (FStartPoint.x = FMatches.EndPoint[0].x);
  end;

  function AdjustedSearchStrMaxLines: Integer;
  begin
    Result := SearchStringMaxLines - 1;
    if Result < 0 then Result := SEARCH_START_OFFS;
  end;

var
  LastLine : Integer;     // Last visible

  procedure MaybeDropOldMatches;
  var
    Idx: Integer;
  begin
    // remove matches, that are too far off the current visible area
    if FMatches.Count > MATCHES_CLEAN_CNT_THRESHOLD then begin
      if TopLine - FMatches.EndPoint[0].y > MATCHES_CLEAN_LINE_THRESHOLD then begin
        Idx := FMatches.IndexOfFirstMatchForLine(TopLine - MATCHES_CLEAN_LINE_KEEP) - 1;
        FMatches.Delete(0, Idx);
        FStartPoint.y := -1;
      end;
      if FMatches.StartPoint[FMatches.Count-1].y - LastLine > MATCHES_CLEAN_LINE_THRESHOLD then begin
        Idx := FMatches.IndexOfLastMatchForLine(LastLine  + MATCHES_CLEAN_LINE_KEEP) + 1;
        FMatches.Delete(Idx, FMatches.Count - Idx);
        FSearchedEnd.y := -1;
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
      if (FirstInvalIdx >= 0) xor (LastInvalIdx >= 0) then begin
        if FirstInvalIdx < 0 then
          FirstInvalIdx := 0;
        if LastInvalIdx < 0 then
          LastInvalIdx := FMatches.Count - 1;
      end;
      if FirstInvalIdx >= 0 then begin
        if (not SkipPaint) and HasVisibleMatch then
          SendLineInvalidation(FirstInvalIdx, LastInvalIdx);
        FMatches.Delete(FirstInvalIdx, LastInvalIdx-FirstInvalIdx+1);
        if FirstInvalIdx >= FMatches.Count then
          FirstInvalIdx := -1;
      end;
    end;
    Result := FirstInvalIdx;
  end;

  function FindStartPoint(var AFirstKeptValidIdx: Integer): Boolean;
  var
    Idx, Idx2 : Integer;
  begin
    Result := False; // No Gap at start to fill

    if (FMatches.Count > 0) and (FMatches.StartPoint[0].y < TopLine) then begin
      // New StartPoint from existing matches
      FStartPoint := FMatches.StartPoint[0];
      if AFirstKeptValidIdx = 0 then
        AFirstKeptValidIdx := -1;
    end

    else begin
      Result := True;
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
          FStartPoint := FMatches.EndPoint[0];
          if (AFirstKeptValidIdx >= 0) then
            inc(AFirstKeptValidIdx, Idx);
        end
        else
          FStartPoint := Point(1, TopLine)     // no previous match found
      end;
      //debugln(['ValidateMatches:  startpoint ', dbgs(FStartPoint)]);
    end;
  end;


var
  EndOffsLine : Integer;  // Stop search (LastLine + Offs)
  Idx, Idx2 : Integer;
  FirstKeptValidIdx: Integer; // The first index, kept after the removed invalidated range
  p: TPoint;
begin
  if FPaintLock > 0 then begin
    FNeedValidate := True;
    if not SkipPaint then
      FNeedValidatePaint := True;
    exit;
  end;
  FNeedValidate := False;

  if (fSearchString = '') or (not MarkupInfo.IsEnabled) then begin
    if (not SkipPaint) and (fMatches.Count > 0) then
      SendLineInvalidation;
    fMatches.Count := 0;
    exit;
  end;

  LastLine := ScreenRowToRow(LinesInWindow+1);

  MaybeDropOldMatches;
  FirstKeptValidIdx := DeleteInvalidMatches;
  //DebugLnEnter(['>>> ValidateMatches ', FFirstInvalidLine, ' - ',FLastInvalidLine, ' Cnt=',FMatches.Count, ' Idx: ', FirstKeptValidIdx, '  ',SynEdit.Name, ' # ',fSearchString]);

  FindInitialize;

  if not IsPosValid(FSearchedEnd) then
    FSearchedEnd.y := -1;

  if not ( IsPosValid(FStartPoint) and
           ( (IsStartAtMatch0 and (FStartPoint.y < TopLine)) or
             (FStartPoint.y < TopLine - AdjustedSearchStrMaxLines) or
             ((FStartPoint.y = TopLine - AdjustedSearchStrMaxLines) and (FStartPoint.x = 1))
           )
         )
  then begin
    if FindStartPoint(FirstKeptValidIdx)
    then begin
      // Maybe created a gap at start
      if IsStartAtMatch0
      then Idx := 1
      else Idx := 0;
      if (FMatches.Count > Idx) then begin
        if FMatches.StartPoint[Idx].y > LastLine+SEARCH_START_OFFS then begin
           // New search has smaller range than gap
          FMatches.Delete(Idx, FMatches.Count - Idx);
        end
        else begin
          // *** Fill gap at start
          Idx2 := Idx;
          FindMatches(FStartPoint, FMatches.StartPoint[Idx], Idx);
          // We already had at least one match, so now we must have 2 or more and can ignore HideSingleMatch
          if (not SkipPaint) and (Idx > Idx2) then
            SendLineInvalidation(Idx2, Idx-1);
          if FirstKeptValidIdx = Idx2 then
            FirstKeptValidIdx := -1;
        end;
      end;
      if (FirstKeptValidIdx >= 0) and (Idx > Idx2) then
        inc(FirstKeptValidIdx, Idx-Idx2);
    end;
  end;


  if FMatches.Count = 0 then begin
    // *** Complete search
    EndOffsLine := min(LastLine+AdjustedSearchStrMaxLines, Lines.Count);
    if (FSearchedEnd.y < 0) or (EndOffsLine > FSearchedEnd.y) then begin
      if HideSingleMatch then
        EndOffsLine := min(LastLine+SEARCH_START_OFFS, Lines.Count);
      Idx := 0;
      FSearchedEnd := FindMatches(FStartPoint, Point(Length(Lines[EndOffsLine - 1]), EndOffsLine),
                                  Idx, LastLine);
      if (not SkipPaint) and (Idx > 0) and HasVisibleMatch then
        SendLineInvalidation(0, Idx-1);
    end;
  end
  else begin

    if (FirstKeptValidIdx >= 0) and (FirstKeptValidIdx < FMatches.Count) then begin
      // Gap from invalidation
      if FMatches.StartPoint[Idx].y > LastLine+SEARCH_START_OFFS then begin
         // New search (extend end) has smaller range than gap
        FMatches.Delete(Idx, FMatches.Count - Idx);
      end
      else begin
        // *** Fill the gap created by invalidation
        Idx := FirstKeptValidIdx; // actually first valid now
        if Idx > 0 then
          FindMatches(FMatches.EndPoint[Idx-1], FMatches.StartPoint[Idx], Idx)
        else
          FindMatches(FStartPoint,              FMatches.StartPoint[Idx], Idx);
        // We already had at least one match, so now we must have 2 or more and can ignore HideSingleMatch
        if (not SkipPaint) and (Idx > FirstKeptValidIdx) then
          SendLineInvalidation(FirstKeptValidIdx, Idx-1);
      end;
    end;

    if FMatches.EndPoint[FMatches.Count - 1].y <= LastLine then begin
      // *** extend at end
      if HideSingleMatch and (FMatches.Count = 1) then
        EndOffsLine := min(LastLine+SEARCH_START_OFFS, Lines.Count)          // Search full range, might find a 2nd match
      else
        EndOffsLine := min(LastLine+AdjustedSearchStrMaxLines, Lines.Count); // Search only for visible new matches
      if (FSearchedEnd.y < 0) or (EndOffsLine > FSearchedEnd.y) then begin
        Idx  := FMatches.Count;
        Idx2 := Idx;
        p := FMatches.EndPoint[Idx-1];
        if (FSearchedEnd.y - AdjustedSearchStrMaxLines > p.y) then
          p := point(1, FSearchedEnd.y - AdjustedSearchStrMaxLines);
        FSearchedEnd := FindMatches(p, Point(Length(Lines[EndOffsLine - 1]), EndOffsLine),
                                    Idx, LastLine);
        if (not SkipPaint) and (Idx > Idx2) and HasVisibleMatch then
          SendLineInvalidation(Idx2, Idx-1);
      end;
    end;

  end;

  if HideSingleMatch and (FMatches.Count = 1) and
     (FMatches.StartPoint[0].y <= LastLine) and (FStartPoint.y > TopLine - SEARCH_START_OFFS)
  then begin
    // A single match in visible lines is hidden, check for 2nd match before FStartPoint
    Idx := 0;
    FindMatches(Point(1, Max(1, TopLine-SEARCH_START_OFFS)), FStartPoint,
                Idx, 0, True); // stopAfterline=0, do only ONE find // Search backwards
    if Idx > 0 then begin
      FStartPoint := FMatches.EndPoint[0];
      SendLineInvalidation;
    end
  end;


  //DebugLnExit(['<<< ValidateMatches Cnt=',FMatches.Count]) //;
  FFirstInvalidLine := 0;
  FLastInvalidLine := 0;
  FNextPosRow := -1;
end;

procedure TSynEditMarkupHighlightAll.DoTextChanged(StartLine, EndLine: Integer);
begin
  if (fSearchString = '') then exit;
  InvalidateLines(StartLine, MaxInt); // EndLine); // Might be LineCount changed
end;

function TSynEditMarkupHighlightAll.HasVisibleMatch: Boolean;
begin
  Result := ( HideSingleMatch and (FMatches.Count > 1) ) or
            ( (not HideSingleMatch) and (FMatches.Count > 0) );
end;

procedure TSynEditMarkupHighlightAll.InvalidateLines(AFirstLine: Integer; ALastLine: Integer;
  SkipPaint: Boolean);
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

procedure TSynEditMarkupHighlightAll.SendLineInvalidation(AFirstIndex: Integer;
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

procedure TSynEditMarkupHighlightAll.PrepareMarkupForRow(aRow: Integer);
begin
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
  if FNextPosIdx < 0 then
    exit;
  if (FMatches.Point[FNextPosIdx].y < aRow) then
    inc(FNextPosIdx);  // Use EndPoint
end;

function TSynEditMarkupHighlightAll.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  pos, s, e: Integer;
begin
  result := nil;
  if (fSearchString = '') then
  exit;

  if (HideSingleMatch and (fMatches.Count <= 1)) or (aRow <> FNextPosRow) or (FNextPosIdx < 0)
  then
    exit;

  while (FNextPosIdx < fMatches.PointCount) and
        (fMatches.Point[FNextPosIdx].y = aRow) and
        (fMatches.Point[FNextPosIdx].x <= aStartCol.Logical)
  do
    inc(FNextPosIdx);

  if FNextPosIdx = fMatches.PointCount // last point was EndPoint => no markup
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
  
  //debugLN('+++>MARUP *ON* ',dbgs(@result),' / ',dbgs(ARow), ' at index ', dbgs(FNextPosIdx));
  if fMatches.Point[pos].y < aRow then
    s := -1
  else
    s := fMatches.Point[pos].x;
  if (pos = FMatches.PointCount) or (fMatches.Point[pos+1].y > aRow) then
    e := -1
  else
    e := fMatches.Point[pos+1].x;
  MarkupInfo.SetFrameBoundsLog(s, e);
  Result := MarkupInfo;
end;

procedure TSynEditMarkupHighlightAll.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (fSearchString = '') then
  exit;
  if (HideSingleMatch and (fMatches.Count <= 1)) or
     (aRow <> FNextPosRow) or (FNextPosIdx < 0) or
     (FNextPosIdx >= fMatches.PointCount) or (FMatches.Point[FNextPosIdx].y > aRow)
  then
    exit;

  while (FNextPosIdx < fMatches.PointCount) and
        (fMatches.Point[FNextPosIdx].y = aRow) and
        (fMatches.Point[FNextPosIdx].x <= aStartCol.Logical)
  do
    inc(FNextPosIdx);

  if FNextPosIdx = fMatches.PointCount
  then exit;
  if fMatches.Point[FNextPosIdx].y <> aRow
  then exit;

  ANextLog := fMatches.Point[FNextPosIdx].x;
  //debugLN('--->NEXT POS ',dbgs(ANextLog),' / ',dbgs(ARow), ' at index ', dbgs(Pos));
end;

procedure TSynEditMarkupHighlightAll.Invalidate(SkipPaint: Boolean);
begin
  if not SkipPaint then
    SendLineInvalidation;
  FStartPoint.y := -1;
  FSearchedEnd.y := -1;
  FMatches.Count := 0;
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

procedure TSynEditMarkupHighlightAllCaret.SetSearchString(const AValue: String);
begin
  inherited SetSearchString(AValue);
  if AValue = '' then
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

procedure TSynEditMarkupHighlightAllCaret.DoTextChanged(StartLine, EndLine: Integer);
begin
  FStateChanged := True; // Something changed, paint will be called
  if ( (not HideSingleMatch) and (MatchCount > 0) ) or
     ( (HideSingleMatch) and (MatchCount > 1) )
  then
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


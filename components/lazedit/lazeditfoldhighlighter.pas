{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit LazEditFoldHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LazLoggerBase, LazClasses,
  // LazEdit
  LazEditHighlighter, LazEditHighlighterFoldNodeHighlighter, LazEditTypes;

type

  (* TSynFoldBlockFilter
     used to specify which folds to include for:
     - FoldOpenCount, FoldCloseCount, FoldNestCount
     - maybe in future TLazEditFoldNodeInfoList
       TLazEditFoldNodeInfoList has additional filters
       TLazEditFoldNodeInfoList always uses the full set (sfbIncludeDisabled)

     A Highlighter is not required to implement this, or can choose to implement
     a subset only. For any field/value a Highlighter may simple assume default.
     - Highlighter that have only one "FoldGroup" do not require this.
     - Highlighter that do not store foldblocks that are unavailable (e.g. off by
       config) always return the same set

     Using a record, as argument is the virtual methods, allows one to add further
     fields/values, without breaking inheritance.
     New fields values are expected to be ignored (handled as default) by existing
     highlighter.

     Callers of the method can:
     - use InitFoldBlockFilter to make sure all fields are set to default
     - use (none virtual) wrapper methods
  *)
  TSynFoldBlockFilterFlag = (
    sfbIncludeDisabled // Foldable by config = off
  );
  TSynFoldBlockFilterFlags = set of TSynFoldBlockFilterFlag;
  TSynFoldBlockFilter = record
    FoldGroup: integer;
    Flags: TSynFoldBlockFilterFlags;
  end;

  TSynFoldAction  = LazEditHighlighterFoldNodeHighlighter.TSynFoldAction;
  TSynFoldActions = LazEditHighlighterFoldNodeHighlighter.TSynFoldActions;

  TSynCustomFoldConfigMode = (fmFold, fmHide, fmMarkup, fmOutline);
  TSynCustomFoldConfigModes = set of TSynCustomFoldConfigMode;

const
  {$WriteableConst off}
  FOLD_MODE_TO_ACTION: array [TSynCustomFoldConfigMode] of TSynFoldActions = (
    { fmFold }    [sfaFold, sfaFoldFold],
    { fmHide }    [sfaFold, sfaFoldHide],
    { fmMarkup }  [sfaMarkup],
    { fmOutline } [sfaOutline]
  );

type

  { TLazEditCustomFoldConfig }

  TLazEditCustomFoldConfig = class(TPersistent)
  private
    FEnabled: Boolean;
    FModes: TSynCustomFoldConfigModes;

    function  GetFoldActions: TSynFoldActions; inline;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetModes(AValue: TSynCustomFoldConfigModes);
  protected
    FSupportedModes: TSynCustomFoldConfigModes; // TODO: private, as soon as the inherited class no longer needs the setter
    procedure SetSupportedModes(AValue: TSynCustomFoldConfigModes); deprecated 'use create';
    procedure DoOnChange; virtual;
  public
    constructor Create;
    constructor Create(ASupportedModes: TSynCustomFoldConfigModes);
    procedure Assign(Src: TLazEditCustomFoldConfig); reintroduce; virtual; // TODO: do not copy supported modes

    property SupportedModes: TSynCustomFoldConfigModes read FSupportedModes;
    property FoldActions: TSynFoldActions read GetFoldActions;     // Actions representing the modes
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Modes: TSynCustomFoldConfigModes read FModes write SetModes default [fmFold];
  end;

  TLazEditCustomFoldHighlighter = class(TLazEditCustomFoldNodeHighlighter)
  protected
    // Fold Config
    function GetFoldConfig(Index: Integer): TLazEditCustomFoldConfig; virtual;
    procedure SetFoldConfig(Index: Integer; const AValue: TLazEditCustomFoldConfig); virtual;
    function GetFoldConfigCount: Integer; virtual;
  public
    (* * FoldBlockOpeningCount:
         Count of folds opening on the given line;
         (not including "one line" blocks that also close on the same line)
       * FoldBlockClosingCount:
         Count of folds that close/end on this line
       * FoldBlockEndLevel:
         Count of folds (including nested from prior lines) that are open
         at the end of the line.
       * FoldBlockMinLevel:
         Count of folds (from prior lines) that "continue through" this line
    *)
    // Info about Folds
    function FoldBlockOpeningCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockClosingCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockEndLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;

    function FoldBlockOpeningCount(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                                   const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;
    function FoldBlockClosingCount(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                                   const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;
    function FoldBlockEndLevel(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                               const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;
    function FoldBlockMinLevel(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                               const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;

    function FindNextLineWithMinFoldLevel(ALineIndex: TLineIdx; ASearchLevel: Integer; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FindNextLineWithMinFoldLevel(const ALineIndex: TLineIdx; const ASearchLevel: Integer;
                               const AFoldGroup: integer = 0; const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;

    function FoldEndLine(ALineIndex, FoldIndex: Integer; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldEndLine(const ALineIndex, FoldIndex: Integer;
                         const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags=[]): integer; inline; overload;

    function FoldLineLength(ALineIndex, FoldIndex: Integer): integer; virtual;  // only for group 0 // may be one less than FoldEndLine, if end line is a mixed end-begin

    (* ***
       *** Scanning nodes in a line
       ***
     *)

    (* FoldBlockNestedTypes:
       All nested FoldType (cfbtBegin) if available. Similar to TopCodeFoldBlockType
       - Index=0 is most outer / Index=FoldBlockEndLevel is most inner (TopCodeFoldBlockType 0=inner)
       - False, if it can not be determined for the filter settings
    *)
    function FoldBlockNestedTypes(ALineIndex: TLineIdx; ANestIndex: Integer; out AType: Pointer;
                                  const AFilter: TSynFoldBlockFilter): boolean; virtual; overload;

    function FoldBlockNestedTypes(const ALineIndex: TLineIdx; const ANestIndex: Integer; out AType: Pointer;
                                  const AFoldGroup: integer = 0;
                                  const AFlags: TSynFoldBlockFilterFlags = []): boolean; inline; overload;


    function FoldGroupCount: integer; virtual;
    function FoldGroupAtNodeIndex(ALineIndex, FoldIndex: Integer;
             UseCloseNodes: boolean = false): integer; virtual; // TODO: could be deprecated ./ only child-classes
    function FoldTypeCount: integer; virtual; deprecated 'Misnomer: Use FoldGroupCount';
    function FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
             UseCloseNodes: boolean = false): integer; virtual; // TODO: could be deprecated ./ only child-classes
             deprecated 'Misnomer: Use FoldGroupAtNodeIndex';


    (* ***
       *** Config
       ***
     *)
    property FoldConfig[Index: Integer]: TLazEditCustomFoldConfig read GetFoldConfig write SetFoldConfig;
    property FoldConfigCount: Integer read GetFoldConfigCount;
  end;

  TLazEditCustomFoldRangesHighlighter = class(TLazEditCustomFoldHighlighter)
  end;

  TLazEditNestedFoldsListEntry = record
    FFLags: set of (nfeHasHNode, nfeMaxPrevReached);
    FGroupMinLevels: Array of Integer;
    //OpenCount: Integer;
    LineIdx: TLineIdx;
    EndLineIdx: TLineIdx;
    HNode: TLazEditFoldNodeInfo;    // Highlighter Node
    //FNode: TSynTextFoldAVLNode; // AvlFoldNode
    PrevNodeAtSameLevel: array of TLazEditNestedFoldsListEntry; // Only for same NodeGroup
  end;

  (* TLazEditNestedFoldsList
     Provides Info on all foldable-blocks containing a given line (0 based index).
     That are:
     - All foldable blocks opening on a previous line, that are still open
       at the start of the line. (May end on this line or later)
     - Foldable blocks opening on that line. (OpeningOnLineCount)

     The data is NOT automatically invalidated.
  *)

  TLazEditNestedFoldsList = class
  { $Define DebugTLazSynEditNestedFoldsList}
  // TODO: in all methods: get "FoldNodeInfo" from FoldProvider, instead of Highlighter
  private
    FLines : TLazEditStringsBase;
    FHighLighter: TLazEditCustomFoldHighlighter;
    FFoldGroup: Integer;
    FLine: TLineIdx;
    procedure SetFoldGroup(AValue: Integer);
    procedure SetLine(AValue: TLineIdx);
  private
    FFoldFlags: TSynFoldBlockFilterFlags;
    FGroupCount: Integer;
    FGroupEndLevelsAtEval: Array of integer;
    FCount, FOpeningOnLineCount: Integer;
    FOpeningLineEndIndex: Integer;
    FIncludeOpeningOnLine: Boolean;
    FNestInfo, FOnLineNestInfo: Array of TLazEditNestedFoldsListEntry;
    FEvaluationIndex: Integer;

    FPreviousNestInfo: Array of TLazEditNestedFoldsListEntry;
    FPreviousLine, FPreviousEvaluationIndex, FPreviousCount: Integer;
    FPreviousMergeLine: Integer;

    FFoldNodeInfoList: TLazEditFoldNodeInfoList;
    FFoldNodeInfoListHoldCnt: integer;

    function GetHLNode(Index: Integer): TLazEditFoldNodeInfo;
    function GetNodeEndLine(Index: Integer): Integer;
    function GetNodeFoldGroup(Index: Integer): Integer;
    function GetNodeLine(Index: Integer): Integer;
    function GetNodeFoldType(Index: Integer): Pointer;
    function GetNodeLineEx(Index, PrevCount: Integer): Integer;
    procedure InitSubGroupEndLevels;
    procedure InitNestInfoForIndex(AnIndex: Integer);
    procedure InitLineInfoForIndex(AnIndex: Integer);
    procedure InitCount;
    procedure InitOpeningOnLine;
    procedure SetFoldFlags(AValue: TSynFoldBlockFilterFlags);
    procedure SetHighLighter(AValue: TLazEditCustomFoldHighlighter);
    procedure SetIncludeOpeningOnLine(AValue: Boolean);
    procedure AquireFoldNodeInfoList(const ALine: Integer = -1);
    procedure ReleaseFoldNodeInfoList;
    procedure SetLines(AValue: TLazEditStringsBase);
    procedure SetOpeningLineEndIndex(AValue: Integer);
    function HasCount: Boolean;
    procedure ClearPreviousCache;
  public
    constructor Create(ALines : TLazEditStringsBase; AnHighLighter: TLazEditCustomFoldHighlighter = nil);
    procedure Clear;
    procedure ResetFilter;
    function Count: Integer;
    function OpeningOnLineCount: Integer;  // ignores FFoldFlags
    procedure Debug;
    property Line: TLineIdx read FLine write SetLine;
    property FoldGroup: Integer read FFoldGroup write SetFoldGroup;
    property FoldFlags: TSynFoldBlockFilterFlags read FFoldFlags write SetFoldFlags;
    property IncludeOpeningOnLine: Boolean read FIncludeOpeningOnLine write SetIncludeOpeningOnLine;
    // OpeningLineEnd... can only be used with sfbIncludeDisabled
    // Highest included index (unfiltered index)
    property OpeningLineEndIndex: Integer read FOpeningLineEndIndex write SetOpeningLineEndIndex;
    //property OpeningLineEndLogicalPos: Integer read FOpeningLineEndLogicalPos write SetOpeningLineEndLogicalPos;
    property Lines: TLazEditStringsBase read FLines write SetLines;
    property HighLighter: TLazEditCustomFoldHighlighter read FHighLighter write SetHighLighter;
  public
    property HLNode[Index: Integer]: TLazEditFoldNodeInfo read GetHLNode;
    property NodeFoldType[Index: Integer]: Pointer read GetNodeFoldType;        // e.g.cfbtBeginEnd, cfbtcfbtProcedure ...
    property NodeFoldGroup[Index: Integer]: Integer read GetNodeFoldGroup;      // independend/overlapping folds, e.g begin/end; ifdef, region
    property NodeLine[Index: Integer]: Integer read GetNodeLine;                // Index
    property NodeEndLine[Index: Integer]: Integer read GetNodeEndLine;          // Index
    property NodeLineEx[Index, PrevCount: Integer]: Integer read GetNodeLineEx; // Index
  end;

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter;
                              const AFoldGroup: Integer = 0;
                              const AFlag: TSynFoldBlockFilterFlags = []
                             ); inline;
function dbgs(ANestInfo: TLazEditNestedFoldsListEntry): String; overload;
function dbgs(AFoldFlag: TSynFoldBlockFilterFlag): String; overload;
function dbgs(AFoldFlags: TSynFoldBlockFilterFlags): String; overload;

implementation

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter; const AFoldGroup: Integer;
  const AFlag: TSynFoldBlockFilterFlags);
begin
  AFilter := Default(TSynFoldBlockFilter);
end;

function dbgs(ANestInfo: TLazEditNestedFoldsListEntry): String;
var
  i: Integer;
begin
  Result := Format('LineIdx:%4d', [ANestInfo.LineIdx ])
   +' HNode: '+dbgs(ANestInfo.HNode)
   +' | PrevCnt: '+DbgS(length(ANestInfo.PrevNodeAtSameLevel))
   +' MinLvl: [';
  for i := 0  to high(ANestInfo.FGroupMinLevels) do
    Result := Result+inttostr(ANestInfo.FGroupMinLevels[i])+',';
  Result := Result+']';
end;

function dbgs(AFoldFlag: TSynFoldBlockFilterFlag): String;
begin
  Result:='';
  WriteStr(Result, AFoldFlag);
end;

function dbgs(AFoldFlags: TSynFoldBlockFilterFlags): String;
var
  i: TSynFoldBlockFilterFlag;
begin
  Result := '';
  for i := low(TSynFoldBlockFilterFlag) to high(TSynFoldBlockFilterFlag) do
    if i in AFoldFlags then
      if Result = ''
      then Result := dbgs(i)
      else Result := Result + ',' + dbgs(i);
  if Result <> '' then
    Result := '[' + Result + ']';
end;

{ TLazEditCustomFoldConfig }

procedure TLazEditCustomFoldConfig.SetEnabled(const AValue: Boolean);
begin
  if FEnabled = AValue then
    exit;
  FEnabled := AValue;
  DoOnChange;
end;

function TLazEditCustomFoldConfig.GetFoldActions: TSynFoldActions;
begin
  Result := [];
  if fmFold    in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmFold];
  if fmHide    in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmHide];
  if fmMarkup  in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmMarkup];
  if fmOutline in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmOutline];
end;

procedure TLazEditCustomFoldConfig.SetModes(AValue: TSynCustomFoldConfigModes);
begin
   AValue := AValue * FSupportedModes;
  if FModes = AValue then
    exit;
  FModes := AValue;
  DoOnChange;
end;

procedure TLazEditCustomFoldConfig.SetSupportedModes(AValue: TSynCustomFoldConfigModes);
begin
  if FSupportedModes = AValue then
    exit;
  FSupportedModes := AValue;
  if Modes <> Modes * FSupportedModes then
    Modes := Modes * FSupportedModes
  else
    DoOnChange;
end;

procedure TLazEditCustomFoldConfig.DoOnChange;
begin
  //
end;

constructor TLazEditCustomFoldConfig.Create;
begin
  inherited Create;
  FSupportedModes := [fmFold];
  Modes := [fmFold];
end;

constructor TLazEditCustomFoldConfig.Create(ASupportedModes: TSynCustomFoldConfigModes);
begin
  Create;
  FSupportedModes := ASupportedModes;
  FModes := FModes * ASupportedModes;
end;

procedure TLazEditCustomFoldConfig.Assign(Src: TLazEditCustomFoldConfig);
begin
  FEnabled        := Src.Enabled;
  FModes          := Src.Modes;
  FSupportedModes := Src.SupportedModes;
  DoOnChange;
end;

{ TLazEditCustomFoldHighlighter }

function TLazEditCustomFoldHighlighter.GetFoldConfig(Index: Integer): TLazEditCustomFoldConfig;
begin
  Result := nil;
end;

procedure TLazEditCustomFoldHighlighter.SetFoldConfig(Index: Integer;
  const AValue: TLazEditCustomFoldConfig);
begin
  //
end;

function TLazEditCustomFoldHighlighter.GetFoldConfigCount: Integer;
begin
  Result := 0;
end;

function TLazEditCustomFoldHighlighter.FoldBlockOpeningCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  Result := FoldBlockEndLevel(ALineIndex, AFilter) - FoldBlockMinLevel(ALineIndex, AFilter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockClosingCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  Result := FoldBlockEndLevel(ALineIndex - 1, AFilter) - FoldBlockMinLevel(ALineIndex, AFilter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockEndLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TLazEditCustomFoldHighlighter.FoldBlockMinLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TLazEditCustomFoldHighlighter.FoldBlockOpeningCount(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockOpeningCount(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockClosingCount(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockClosingCount(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockEndLevel(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockEndLevel(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockMinLevel(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockMinLevel(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FindNextLineWithMinFoldLevel(ALineIndex: TLineIdx;
  ASearchLevel: Integer; const AFilter: TSynFoldBlockFilter): integer;
var
  cnt: Integer;
begin
  cnt := CurrentLines.Count - 1;
  Result := ALineIndex; // Can return the original line
  while (Result < cnt) and (FoldBlockMinLevel(Result, AFilter) > ASearchLevel) do inc(Result);
end;

function TLazEditCustomFoldHighlighter.FindNextLineWithMinFoldLevel(const ALineIndex: TLineIdx;
  const ASearchLevel: Integer; const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags
  ): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FindNextLineWithMinFoldLevel(ALineIndex, ASearchLevel, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldEndLine(ALineIndex, FoldIndex: Integer;
  const AFilter: TSynFoldBlockFilter): integer;
var
  lvl: Integer;
begin
  // max count;
  lvl := min(FoldBlockMinLevel(ALineIndex, AFilter) + 1 + FoldIndex,
             FoldBlockEndLevel(ALineIndex, AFilter)
            );
  Result := FindNextLineWithMinFoldLevel(ALineIndex+1, lvl-1, AFilter);
end;

function TLazEditCustomFoldHighlighter.FoldEndLine(const ALineIndex, FoldIndex: Integer;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldEndLine(ALineIndex, FoldIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldLineLength(ALineIndex, FoldIndex: Integer): integer;
begin
  Result := FoldEndLine(ALineIndex, FoldIndex, 0);
  // check if fold last line of block (not mixed "end begin")
  if (FoldBlockEndLevel(Result) > FoldBlockMinLevel(Result)) then
    dec(Result);
  // Amount of lines, that will become invisible (excludes the cfCollapsed line)
  Result := Result - ALineIndex;
end;

function TLazEditCustomFoldHighlighter.FoldBlockNestedTypes(ALineIndex: TLineIdx;
  ANestIndex: Integer; out AType: Pointer; const AFilter: TSynFoldBlockFilter): boolean;
begin
  AType := nil;
  Result := False;
end;

function TLazEditCustomFoldHighlighter.FoldBlockNestedTypes(const ALineIndex: TLineIdx;
  const ANestIndex: Integer; out AType: Pointer; const AFoldGroup: integer;
  const AFlags: TSynFoldBlockFilterFlags): boolean;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockNestedTypes(ALineIndex, ANestIndex, AType, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldGroupCount: integer;
begin
  Result := FoldTypeCount;
end;

function TLazEditCustomFoldHighlighter.FoldGroupAtNodeIndex(ALineIndex, FoldIndex: Integer;
  UseCloseNodes: boolean): integer;
begin
  Result := FoldTypeAtNodeIndex(ALineIndex, FoldIndex, UseCloseNodes);
end;

function TLazEditCustomFoldHighlighter.FoldTypeCount: integer;
begin
  Result := 1;
end;

function TLazEditCustomFoldHighlighter.FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
  UseCloseNodes: boolean): integer;
begin
  Result := 0;
end;


{ TLazEditNestedFoldsList }

procedure TLazEditNestedFoldsList.SetLine(AValue: TLineIdx);
begin
  if FLine = AValue then Exit;
  {$IfDef DebugTLazSynEditNestedFoldsList}
  debugln(['TLazEditNestedFoldsList.SetLine ', AValue, ' from previous ', FLine]);
  {$EndIf}

  // might be able to re-use old data
  FPreviousCount := FCount;
  FPreviousEvaluationIndex := FEvaluationIndex;
  FPreviousNestInfo := FNestInfo;
  FPreviousLine := FLine;
  FNestInfo := nil;
  FOnLineNestInfo := nil;

  FLine := AValue;
  FCount := -1;                          // will trigger InitCount
  //FEvaluationIndex := -1;
  FOpeningOnLineCount := -1;
  FGroupEndLevelsAtEval := nil;   // will trigger InitSubGroupEndLevels
  FGroupCount := -1;
end;

procedure TLazEditNestedFoldsList.Clear;
begin
  {$IfDef DebugTLazSynEditNestedFoldsList}
  debugln(['TLazEditNestedFoldsList.Clear ']);
  {$EndIf}

  FGroupCount := -1;
  SetLength(FGroupEndLevelsAtEval, 0);
  FCount := -1;
  FOpeningOnLineCount := -1;
  FEvaluationIndex := -1;
  SetLength(FNestInfo, 0);
  SetLength(FOnLineNestInfo, 0);

  ClearPreviousCache;
end;

procedure TLazEditNestedFoldsList.ResetFilter;
begin
  if FIncludeOpeningOnLine and (FFoldFlags = []) and (FFoldGroup = 0) and
     (FOpeningLineEndIndex = -1)
  then
    exit;
  FIncludeOpeningOnLine := True;
  FFoldFlags := [];
  FFoldGroup := 0;
  FOpeningLineEndIndex := -1;
  Clear;
end;

procedure TLazEditNestedFoldsList.InitSubGroupEndLevels;
var
  i: integer;
begin
  if Length(FGroupEndLevelsAtEval) > 0 then
    exit;
  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  if FFoldGroup = 0 then begin
    // special, join other groups
    FGroupCount := FHighlighter.FoldTypeCount;
    // start at 1, so FoldGroup can be used as index
    SetLength(FGroupEndLevelsAtEval, FGroupCount + 1);
    for i := 1 to FGroupCount do
      FGroupEndLevelsAtEval[i] := FHighlighter.FoldBlockEndLevel(FLine - 1, i, FFoldFlags);
  end
  else begin
    FGroupCount := 1;
    SetLength(FGroupEndLevelsAtEval, 1);
    FGroupEndLevelsAtEval[0] := Count - OpeningOnLineCount;
  end;
  // Warning: storing endlevels, not minlevels
  FNestInfo[FCount].FGroupMinLevels := copy(FGroupEndLevelsAtEval,0, length(FGroupEndLevelsAtEval));
  FNestInfo[FCount].LineIdx := Line - 1;
  FNestInfo[FCount].EndLineIdx := 0;
end;

function TLazEditNestedFoldsList.GetHLNode(Index: Integer): TLazEditFoldNodeInfo;
begin
  if (Index < 0) or (Index >= Count) then begin
    Result.FoldAction := [sfaInvalid];
    exit;
  end;
  if Index >= FCount then
    Result := FOnLineNestInfo[Index - FCount].HNode
  else begin
    InitNestInfoForIndex(Index);
    Result := FNestInfo[Index].HNode;
  end;
end;

function TLazEditNestedFoldsList.GetNodeEndLine(Index: Integer): Integer;
var
  nd: TLazEditFoldNodeInfo;
  lvl, i, CurIdx, minlvl, grp, cnt: Integer;
begin
  if (Index < 0) or (Index >= Count) then
    exit(-1);

  nd := HLNode[Index]; // make sure the list/array is initialzied
  grp := nd.FoldGroup;
  CurIdx := Index;
  cnt := Count;
  while CurIdx < cnt do begin
    nd := HLNode[CurIdx];
    if nd.FoldGroup = grp then begin
      if CurIdx >= FCount then
        Result := FOnLineNestInfo[CurIdx - FCount].EndLineIdx
      else
        Result := FNestInfo[CurIdx].EndLineIdx;
      if Result > 0 then
        break;
    end;
    inc(CurIdx);
  end;
  {$IfDef DebugTLazSynEditNestedFoldsList}
  debugln(['TLazEditNestedFoldsList.GetNodeEndLine  for ', Index, '    from curidx ',CurIdx, '   of cnt ', Count, '  / ', FCount ]);
  {$EndIf}

  if CurIdx = Index then
    exit;

  minlvl:= MaxInt;
  if CurIdx < cnt then begin
    i := Result;
    minlvl := HighLighter.FoldBlockMinLevel(Result, nd.FoldGroup, FoldFlags);
  end
  else if CurIdx - 1 >= FCount then
    i := FLine + 1
  else
    i := FLine;

  while CurIdx > Index do begin
    dec(CurIdx);
    nd := HLNode[CurIdx];
    if nd.FoldGroup <> grp then
      continue;

    if sfbIncludeDisabled in FoldFlags then
      lvl := nd.NestLvlStart
    else
      lvl := nd.FoldLvlStart;

    if minlvl > lvl then begin
      Result := HighLighter.FindNextLineWithMinFoldLevel(i, lvl, nd.FoldGroup, FoldFlags);
      minlvl := HighLighter.FoldBlockMinLevel(Result, nd.FoldGroup, FoldFlags);
    end;

    if CurIdx >= FCount then
      FOnLineNestInfo[CurIdx - FCount].EndLineIdx := Result
    else
      FNestInfo[CurIdx].EndLineIdx := Result;

    if minlvl >= lvl then begin
      i := Result + 1;
      minlvl:= MaxInt;
    end
    else if CurIdx = FCount then
      minlvl:= MaxInt;
  end;
end;

function TLazEditNestedFoldsList.GetNodeFoldGroup(Index: Integer): Integer;
begin
  if FoldGroup <> 0 then
    Result := FoldGroup
  else
    Result := HLNode[Index].FoldGroup;
end;

function TLazEditNestedFoldsList.GetNodeLine(Index: Integer): Integer;
begin
  InitLineInfoForIndex(Index);
  if Index >= FCount then
    Result := FLine
  else
    Result := FNestInfo[Index].LineIdx;
end;

function TLazEditNestedFoldsList.GetNodeFoldType(Index: Integer): Pointer;
begin
  Result := nil;

  if HasCount and
     ( (Index >= Count - OpeningOnLineCount) or // OpeningOnLine
       ( (Index >= FEvaluationIndex) and (nfeHasHNode in FNestInfo[Index].FFLags) )
     )
  then begin
    Result := HLNode[Index].FoldType;
    exit;
  end;

  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  // TODO: Cache
  if (FFoldGroup > 0) and (FHighLighter.FoldBlockNestedTypes(Line - 1, Index, Result, FFoldGroup, FFoldFlags)) then
    exit;

  Result := HLNode[Index].FoldType;
end;

function TLazEditNestedFoldsList.GetNodeLineEx(Index, PrevCount: Integer): Integer;
var
  Node: TLazEditNestedFoldsListEntry;
  MinLvl, SearchLvl, Grp, PCnt, PLineIdx: Integer;
begin
  InitLineInfoForIndex(Index);
  Result := -1;

  Node := FNestInfo[Index];
  PCnt := length(Node.PrevNodeAtSameLevel);

  if PrevCount > PCnt then begin
    if (nfeMaxPrevReached in Node.FFLags) then
      exit;
    if FHighLighter = nil then exit;
    FHighLighter.CurrentLines := FLines;

    if FoldGroup = 0 then begin
      InitNestInfoForIndex(Index);
      Grp    := Node.HNode.FoldGroup;
      if sfbIncludeDisabled in FFoldFlags then
        SearchLvl := Node.HNode.NestLvlStart
      else
        SearchLvl := Node.HNode.FoldLvlStart;
    end else begin
      Grp    := FoldGroup;
      SearchLvl := Index;
    end;
    if PCnt = 0 then
      PLineIdx := Node.LineIdx - 1
    else
      PLineIdx := Node.PrevNodeAtSameLevel[PCnt-1].LineIdx - 1;

    while true do begin

      MinLvl := FHighLighter.FoldBlockMinLevel(PLineIdx, Grp, FFoldFlags);
      while (PLineIdx >= 0) and (SearchLvl < MinLvl) do begin
        dec(PLineIdx);
        MinLvl := FHighLighter.FoldBlockMinLevel(PLineIdx, Grp, FFoldFlags);
      end;

      if PLineIdx >= 0 then begin
        if length(Node.PrevNodeAtSameLevel) = PCnt then
          SetLength(Node.PrevNodeAtSameLevel, Max(PrevCount, PCnt+1));
        Node.PrevNodeAtSameLevel[PCnt].LineIdx := PLineIdx;
        Node.PrevNodeAtSameLevel[PCnt].FFLags  := [];
        inc(PCnt);
        if PCnt = PrevCount then begin
          if length(Node.PrevNodeAtSameLevel) > PCnt then
            SetLength(Node.PrevNodeAtSameLevel, PCnt);
          Result := PLineIdx;
          exit;
        end;
      end;

      If (PLineIdx < 0) or (MinLvl < SearchLvl) then begin
        Include(Node.FFLags, nfeMaxPrevReached);
        if length(Node.PrevNodeAtSameLevel) > PCnt then
          SetLength(Node.PrevNodeAtSameLevel, PCnt);
        exit;
      end;

    end;
  end;

  Result := Node.PrevNodeAtSameLevel[PrevCount-1].LineIdx;
end;

procedure TLazEditNestedFoldsList.InitNestInfoForIndex(AnIndex: Integer);
var
  CurLine: TLineIdx;
  i, EvalIdx, c, t, l: Integer;
  NFilter: TSynFoldActions;
  nd: TLazEditFoldNodeInfo;
  GrpCnt: Array of integer;
begin
  if HasCount and
     ( (AnIndex >= Count - OpeningOnLineCount) or
       ( (AnIndex >= FEvaluationIndex) and (nfeHasHNode in FNestInfo[AnIndex].FFLags) )
     )
  then exit;

  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  AquireFoldNodeInfoList;
  try
    InitLineInfoForIndex(AnIndex);
    if (AnIndex >= Count - OpeningOnLineCount) or
       ( (AnIndex >= FEvaluationIndex) and (nfeHasHNode in FNestInfo[AnIndex].FFLags) )
    then exit;

    EvalIdx := AnIndex;
    CurLine := FNestInfo[EvalIdx].LineIdx;
    while (EvalIdx < FCount-1) and (FNestInfo[EvalIdx+1].LineIdx = CurLine) do inc(EvalIdx);
    assert(Length(FNestInfo[EvalIdx+1].FGroupMinLevels) > 0, 'Length(FNestInfo[EvalIdx].FGroupEndLevels)');

    //TODO keep groupcount allocated on the same mem / instance var
    GrpCnt := copy(FNestInfo[EvalIdx+1].FGroupMinLevels);

    NFilter := [sfaOpenFold];
    if not(sfbIncludeDisabled in FFoldFlags) then Include(NFilter, sfaFold);
    FFoldNodeInfoList.Line := CurLine;
    FFoldNodeInfoList.ActionFilter := NFilter;
    FFoldNodeInfoList.GroupFilter := FFoldGroup;
    c := FFoldNodeInfoList.Count - 1;
    {$IfDef DebugTLazSynEditNestedFoldsList}
    debugln(['TLazEditNestedFoldsList.InitNestInfoForIndex CurLine=',CurLine, '  c=',c, '  EvalIdx=',EvalIdx]);
    {$EndIf}
    (* if c < 0 then it is possible that a highlighter called
       CodeFoldRange.Pop(false); // avoid minlevel // << still triggers min level for sfbIncludeDisabled;
       without generating foldnode info // maybe the HL tries to silently change the fold type
    *)
    assert(c >= 0, 'InitNestInfoForIndex: FFoldNodeInfoList.Count');

    for i := c downto 0 do begin
      nd := FFoldNodeInfoList[i];

      if FFoldGroup = 0
      then t := nd.FoldGroup
      else t := 0;

      if (sfbIncludeDisabled in FFoldFlags)
      then l := nd.NestLvlStart
      else l := nd.FoldLvlStart;
      if l >= GrpCnt[t] then continue;

      dec(GrpCnt[t]);

      assert(GrpCnt[t] >= 0, 'TLazEditNestedFoldsList.InitNestInfoForIndex GroupEndLevel < 0');
      assert(EvalIdx >= 0, 'TLazEditNestedFoldsList.InitNestInfoForIndex FEvaluationIndex < 0');
      assert(FNestInfo[EvalIdx].LineIdx = CurLine, 'TLazEditNestedFoldsList.InitNestInfoForIndex FNestInfo[EvalIdx].LineIdx = CurLine');

      //FNestInfo[EvalIdx].LineIdx := CurLine;
      include(FNestInfo[EvalIdx].FFLags, nfeHasHNode);
      FNestInfo[EvalIdx].HNode := nd;

      dec(EvalIdx);
    end;

  finally
    ReleaseFoldNodeInfoList;
  end;
  //for i := FCount-1 downto 0 do  DbgOut([', ',dbgs(nfeHasHNode in FNestInfo[i].FFLags)]); DebugLn();
  assert(nfeHasHNode in FNestInfo[AnIndex].FFLags, 'nfeHasHNode in FNestInfo[AnIndex].FFLags');
  assert(AnIndex >= FEvaluationIndex, 'TLazEditNestedFoldsList.InitNestInfoForIndex Index not found');
end;

procedure TLazEditNestedFoldsList.InitLineInfoForIndex(AnIndex: Integer);
var
  CurLine: TLineIdx;
  i, c, c1, l: Integer;

  procedure DoMergePrevious;   // TODO: copy nodeinfo if avail
  var
    pcnt, c, l, c1: integer;
  begin
    pcnt := FPreviousCount - 1;
    {$IfDef DebugTLazSynEditNestedFoldsList}
    debugln(['TLazEditNestedFoldsList.InitLineInfoForIndex() DoMergePrev (',pcnt, ' ',FPreviousEvaluationIndex ,') ', FPreviousNestInfo[pcnt].LineIdx,' to ', FPreviousNestInfo[FPreviousEvaluationIndex].LineIdx, ' FEvaluationIndex:',FEvaluationIndex, ' CurLine=',CurLine ]);
    {$EndIf}
    assert(FPreviousNestInfo[pcnt].LineIdx = CurLine, 'TLazEditNestedFoldsList.InitLineInfoForIndex.DoMergePrevious LineIdx = CurLine');
    while  pcnt >= FPreviousEvaluationIndex do begin
      while (pcnt > 0) and (pcnt > FPreviousEvaluationIndex) and
            (FPreviousNestInfo[pcnt].LineIdx = FPreviousNestInfo[pcnt-1].LineIdx)
      do
        dec(pcnt);
      assert(length(FPreviousNestInfo[pcnt].FGroupMinLevels) > 0, 'TLazEditNestedFoldsList.InitLineInfoForIndex.DoMergePrevious FGroupEndLevels > 0');

      c := 0;
      if FFoldGroup = 0 then begin
        i := FGroupCount;
        while (i > 0) do begin
          l := FPreviousNestInfo[pcnt].FGroupMinLevels[i];
          if (l < FGroupEndLevelsAtEval[i]) then begin
            c1 := FGroupEndLevelsAtEval[i] - l;
            FGroupEndLevelsAtEval[i] := l;
            c := c + c1;
          end
          else begin
            FPreviousNestInfo[pcnt].FGroupMinLevels[i] := FGroupEndLevelsAtEval[i];
          end;
          dec(i);
        end;
      end
      else begin
        l := FPreviousNestInfo[pcnt].FGroupMinLevels[0];
        if (l < FGroupEndLevelsAtEval[0]) then begin
          c := FGroupEndLevelsAtEval[0] - l;
          FGroupEndLevelsAtEval[0] := l;
        end
        else begin
          FPreviousNestInfo[pcnt].FGroupMinLevels[0] := FGroupEndLevelsAtEval[0];
        end;
      end;

      while c > 0 do begin
        dec(FEvaluationIndex);
        FNestInfo[FEvaluationIndex].LineIdx := FPreviousNestInfo[pcnt].LineIdx;
        FNestInfo[FEvaluationIndex].EndLineIdx := FPreviousNestInfo[pcnt].EndLineIdx;
        FNestInfo[FEvaluationIndex].FFLags:= [];
        FNestInfo[FEvaluationIndex].FGroupMinLevels := FPreviousNestInfo[pcnt].FGroupMinLevels;
        dec(c);
      end;

      dec(pcnt);
    end;
    ClearPreviousCache;
  end;

begin
  if HasCount and ((AnIndex >= Count - OpeningOnLineCount) or (AnIndex >= FEvaluationIndex)) then exit;
  assert(FEvaluationIndex > 0, 'TLazEditNestedFoldsList.InitLineInfoForIndex already finilhed');

  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  // prepare previous info.
  // TODO FLine = FPreviousNestInfo[i].LineIdx or FLine = FPreviousLine;
  if (FEvaluationIndex = FCount) then begin
    FPreviousMergeLine := -1;
    i := FPreviousCount; // + 1 - 1
    if (i > 0) and (FPreviousEvaluationIndex < i) then begin
      if i >= Length(FPreviousNestInfo) then
        exit;
      if (i > FPreviousEvaluationIndex) and
         (FPreviousNestInfo[i].LineIdx = FPreviousNestInfo[i-1].LineIdx)
      then
        dec(i);
      while (i >= 0) and (i >= FPreviousEvaluationIndex) do
        if FPreviousNestInfo[i].LineIdx >= Line then
          dec(i)
        else
          break;
      FPreviousCount := i + 1;
      if (i >= 0) and (i >= FPreviousEvaluationIndex) then
        FPreviousMergeLine := FPreviousNestInfo[i].LineIdx;
    end;
  end;

  AquireFoldNodeInfoList;
  try
    if (AnIndex >= Count - OpeningOnLineCount) or (AnIndex >= FEvaluationIndex) then exit;

    InitSubGroupEndLevels;

//    FNestInfo[FEvaluationIndex].FGroupMinLevels := copy(FGroupEndLevelsAtEval,0, length(FGroupEndLevelsAtEval));

    if (FEvaluationIndex = FCount) then
      CurLine := Line - 1
    else
      CurLine := FNestInfo[FEvaluationIndex].LineIdx - 1;

    inc(CurLine);
    while CurLine > 0 do begin
      dec(CurLine);
      if CurLine = FPreviousMergeLine then begin
        FPreviousMergeLine := -1;
        DoMergePrevious;
        InitLineInfoForIndex(AnIndex);
        exit;
      end;

      c := 0;
      if FFoldGroup = 0 then begin
        i := FGroupCount;
        while (i > 0) do begin
          l := FHighLighter.FoldBlockMinLevel(CurLine, i, FFoldFlags);
          if (l < FGroupEndLevelsAtEval[i]) then begin
            c1 := FGroupEndLevelsAtEval[i] - l;
            FGroupEndLevelsAtEval[i] := FGroupEndLevelsAtEval[i] - c1;
            c := c + c1;
          end;
          dec(i);
        end;
      end
      else begin
        l := FHighLighter.FoldBlockMinLevel(CurLine, FFoldGroup, FFoldFlags);
        if l < FGroupEndLevelsAtEval[0] then begin
          c := FGroupEndLevelsAtEval[0] - l;
          FGroupEndLevelsAtEval[0] := FGroupEndLevelsAtEval[0] - c;
        end;
      end;
      if c = 0 then continue;

      while c > 0 do begin
        dec(FEvaluationIndex);
        FNestInfo[FEvaluationIndex].LineIdx := CurLine;
        FNestInfo[FEvaluationIndex].FFLags:= [];
        dec(c);
      end;
      FNestInfo[FEvaluationIndex].FGroupMinLevels := copy(FGroupEndLevelsAtEval,0, length(FGroupEndLevelsAtEval));

      if (AnIndex >= FEvaluationIndex) then Break;
    end;

  finally
    ReleaseFoldNodeInfoList;
  end;
  {$IfDef DebugTLazSynEditNestedFoldsList}
  debugln(['TLazEditNestedFoldsList.InitLineInfoForIndex FEvaluationIndex=', FEvaluationIndex, '  AnIndex=',AnIndex]);
  for i := FCount-1 downto 0 do begin DbgOut([', ',FNestInfo[i].LineIdx]); if length(FNestInfo[i].FGroupMinLevels) > 0 then begin DbgOut(' ('); for c := 0 to length(FNestInfo[i].FGroupMinLevels)-1 do DbgOut([',',FNestInfo[i].FGroupMinLevels[c]]);  DbgOut(') '); end; end; DebugLn();
  {$EndIf}
  assert(CurLine >= 0, 'TLazEditNestedFoldsList.InitLineInfoForIndex Curline < 0');
  assert(AnIndex >= FEvaluationIndex, 'TLazEditNestedFoldsList.InitLineInfoForIndex Index not found');
end;

procedure TLazEditNestedFoldsList.InitCount;
begin
  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  FCount := FHighlighter.FoldBlockEndLevel(FLine - 1, FFoldGroup, FFoldFlags);
  FEvaluationIndex := FCount;
  SetLength(FNestInfo, FCount+1);
end;

procedure TLazEditNestedFoldsList.InitOpeningOnLine;
var
  nd: TLazEditFoldNodeInfo;
  OpenIdx: Array of Array of Integer; // List of open-node-index, for each FoldCroup
  OpenCnt: Array of Integer; // List of open-node-index, for each FoldCroup
  Grp, c, i, j, GrpLow, GrpHigh, ListCnt: Integer;
  oc: LongInt;
begin
  Assert((FOpeningLineEndIndex < 0) or (sfbIncludeDisabled in FoldFlags), 'OpeningLineEndIndex only implemented for sfbIncludeDisabled');

  FOpeningOnLineCount := 0;
  if FCount < 0 then
    InitCount;

  if not FIncludeOpeningOnLine then
    exit;
  // FOnLineNestInfo

  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  AquireFoldNodeInfoList(FLine);
  try
    if (sfbIncludeDisabled in FFoldFlags) then
      FFoldNodeInfoList.ActionFilter := []
    else
      FFoldNodeInfoList.ActionFilter := [sfaFold];
    FFoldNodeInfoList.GroupFilter := 0;

    if FFoldGroup = 0 then begin
      FGroupCount := FHighlighter.FoldTypeCount;
      GrpLow := 1;
      GrpHigh := FGroupCount;
    end
    else begin
      FGroupCount := 1;
      GrpLow := FFoldGroup;
      GrpHigh := FFoldGroup;
    end;
    SetLength(OpenCnt{%H-}, FGroupCount);
    for Grp := 0 to FGroupCount - 1 do
      OpenCnt[Grp] := 0;
    ListCnt := FFoldNodeInfoList.Count;
    if ListCnt < 0 then
      exit;
    SetLength(OpenIdx{%H-}, FGroupCount, ListCnt);

    for Grp := GrpLow to GrpHigh do begin
      (* Filtering group in the loop instead of the list only works, if 0 is the only special group
         See use of NodeIndex below, if changing this *)
      //FFoldNodeInfoList.GroupFilter := Grp;
      for i := 0 to ListCnt - 1 do begin
        nd := FFoldNodeInfoList[i];
        if (sfaInvalid in nd.FoldAction) or (nd.FoldGroup <> Grp) then
          Continue;
        if (FOpeningLineEndIndex >= 0) and (nd.AllNodeIndex > FOpeningLineEndIndex) then
          break;

        if sfaOpen in nd.FoldAction then begin
          inc(FOpeningOnLineCount);
          OpenIdx[Grp - GrpLow, OpenCnt[Grp - GrpLow]] := nd.NodeIndex; // Using NodeIndex only works, because we do NOT change the filter
          inc(OpenCnt[Grp - GrpLow]);
        end
        else
        if (nd.FoldAction * [sfaClose, sfaFold, sfaSingleLine] = [sfaClose, sfaSingleLine]) then begin
          dec(FOpeningOnLineCount);
          dec(OpenCnt[Grp - GrpLow]);
        end;
      end;
    end;

    SetLength(FOnLineNestInfo, FOpeningOnLineCount);

    //FFoldNodeInfoList.ActionFilter := [];
    //FFoldNodeInfoList.GroupFilter := 0;
    c := ListCnt - 1;
    if (FOpeningLineEndIndex >= 0) and (c > FOpeningLineEndIndex) then
      c := FOpeningLineEndIndex;
    j := FOpeningOnLineCount;

    For i := c downto 0 do begin
      if j = 0 then break;
      nd := FFoldNodeInfoList[i];
      Grp := nd.FoldGroup;
      if (Grp < GrpLow) or (Grp > GrpHigh) then Continue;
      oc := OpenCnt[Grp - GrpLow];
      Assert(oc >= 0, 'TLazEditNestedFoldsList.InitOpeningOnLine bad count for '+IntToStr(Grp));
      Assert((oc=0) or (OpenIdx[Grp - GrpLow, oc-1] <= i), 'TLazEditNestedFoldsList.InitOpeningOnLine bad index for '+IntToStr(i)+' G='+IntToStr(Grp));
      if (oc > 0) and (OpenIdx[Grp - GrpLow, oc-1] = i) then begin
        dec(OpenCnt[Grp - GrpLow]);
        dec(j);
        FOnLineNestInfo[j].LineIdx := FLine;
        FOnLineNestInfo[j].HNode := nd;
        FOnLineNestInfo[j].HNode.NodeIndex := j;
        FOnLineNestInfo[j].EndLineIdx := 0;
      end;
    end;

    Assert(j=0, 'TLazEditNestedFoldsList.InitOpeningOnLine did not fill all nodes '+IntToStr(j));
  finally
    ReleaseFoldNodeInfoList;
  end;
end;

procedure TLazEditNestedFoldsList.SetFoldFlags(AValue: TSynFoldBlockFilterFlags);
begin
  if FFoldFlags = AValue then Exit;
  FFoldFlags := AValue;
  Clear;
end;

procedure TLazEditNestedFoldsList.SetHighLighter(
  AValue: TLazEditCustomFoldHighlighter);
begin
  if FHighLighter = AValue then Exit;
  FHighLighter := AValue;
  Clear;
end;

procedure TLazEditNestedFoldsList.SetIncludeOpeningOnLine(AValue: Boolean);
begin
  if FIncludeOpeningOnLine = AValue then Exit;
  FIncludeOpeningOnLine := AValue;
  //Clear; // Do not Clear, keep the data, can be re-enabled
end;

procedure TLazEditNestedFoldsList.AquireFoldNodeInfoList(const ALine: Integer
  );
begin
  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  if FFoldNodeInfoListHoldCnt = 0 then begin
    FFoldNodeInfoList := FHighlighter.FoldNodeInfo[ALine];
    FFoldNodeInfoList.AddReference;
  end else
    FFoldNodeInfoList.Line := ALine;
  inc(FFoldNodeInfoListHoldCnt);
end;

procedure TLazEditNestedFoldsList.ReleaseFoldNodeInfoList;
begin
  dec(FFoldNodeInfoListHoldCnt);
  if FFoldNodeInfoListHoldCnt = 0 then
    ReleaseRefAndNil(FFoldNodeInfoList);
end;

procedure TLazEditNestedFoldsList.SetLines(AValue: TLazEditStringsBase);
begin
  if FLines = AValue then Exit;
  FLines := AValue;
  Clear;
end;

procedure TLazEditNestedFoldsList.SetOpeningLineEndIndex(AValue: Integer);
begin
  if FOpeningLineEndIndex = AValue then Exit;
  FOpeningLineEndIndex := AValue;
  Clear; // TODO only clear current line, the rest will still be valid
end;

function TLazEditNestedFoldsList.HasCount: Boolean;
begin
  Result := (FCount >= 0) and ( (not FIncludeOpeningOnLine) or (FOpeningOnLineCount >= 0) );
end;

procedure TLazEditNestedFoldsList.ClearPreviousCache;
begin
  FPreviousCount := -1;
  FPreviousEvaluationIndex := -1;
  SetLength(FPreviousNestInfo, 0);
end;

procedure TLazEditNestedFoldsList.SetFoldGroup(AValue: Integer);
begin
  if FFoldGroup = AValue then Exit;
  FFoldGroup := AValue;
  Clear;
end;

constructor TLazEditNestedFoldsList.Create(ALines: TLazEditStringsBase;
  AnHighLighter: TLazEditCustomFoldHighlighter);
begin
  FLines := ALines;
  FHighLighter := AnHighLighter;
  FIncludeOpeningOnLine := True;
  FFoldFlags := [];
  FFoldGroup := 0;
  FFoldNodeInfoListHoldCnt := 0;
end;

function TLazEditNestedFoldsList.Count: Integer;
begin
  if (FCount < 0) then begin
    InitCount;
  end;
  if FIncludeOpeningOnLine and (FOpeningOnLineCount < 0) then begin
    InitOpeningOnLine;
  end;

  Result := FCount + OpeningOnLineCount;
end;

function TLazEditNestedFoldsList.OpeningOnLineCount: Integer;
begin
  if (not FIncludeOpeningOnLine) or (FLine < 0) then
    exit(0);

  if (FOpeningOnLineCount < 0) then begin
    InitOpeningOnLine;
  end;

  Result := FOpeningOnLineCount;
end;

procedure TLazEditNestedFoldsList.Debug;
var
  i: Integer;
begin
  Debugln(['TLazEditNestedFoldsList for FFoldGroup=', FFoldGroup, ' FLine=', FLine,
           ' FFoldFlags=', dbgs(FFoldFlags), ' FGroupCount=', FGroupCount,
           ' FIncludeOpeningOnLine=', dbgs(FIncludeOpeningOnLine), ' FEvaluationIndex=', FEvaluationIndex,
           ' FCount=', FCount, ' FOpeningOnLineCount=', FOpeningOnLineCount]);
  Debugln(['FGroupEndLevelsAtEval=', length(FGroupEndLevelsAtEval), ': ']); for i := 0 to length(FGroupEndLevelsAtEval)-1 do DbgOut([FGroupEndLevelsAtEval[i]]); Debugln;
  for i := 0 to length(FNestInfo)-1 do
    Debugln(['N-Info ', i,': ',dbgs(FNestInfo[i])]);
end;

end.


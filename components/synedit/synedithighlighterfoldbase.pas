{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditHighlighter.pas, released 2000-04-07.

The Original Code is based on mwHighlighter.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id: synedithighlighter.pp 19051 2009-03-21 00:47:33Z martin $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

(* Naming Conventions:
   -  FoldBlock:
     A continuous range of lines, that can (optional) be folded.
     Which Foldblocks can be folded is decided by the Highlighter. It may be
     configurable.
     A Foldblock can contain other Foldbloccks (nested), but two Foldblocks can
     not overlap.
   -  FoldBlockLevel (FoldBlockNestLevel):
     The amount of FoldBlocks in which a line (or a point of text) is.
   -  FoldGroup:
     An independent set of FoldBlocks. FoldBlocks in different Groups may overlap.
     (e.g. IFDEF/REGION in the SynPasSyn allow for overlaps, rather than strict nesting)
     Some older code use "FoldType" instead
   -  FoldNode
     Start or End of a FoldBlock
*)

(* TODO : Workaround for bug #20850
   Remove when FPC 2.6.2 is out
*)
{$IFDEF CPU64}
{$IF (FPC_FULLVERSION = 20600) or (FPC_FULLVERSION = 20501)}
  {$DEFINE ISSUE_20850 }
{$ENDIF}
{$ENDIF}

unit SynEditHighlighterFoldBase;

{$I synedit.inc}

interface

uses
  SysUtils, Classes, Math, AVL_Tree,
  // LazUtils
  LazClasses, LazLoggerBase, LazTracer,
  // LazEdit
  LazEditHighlighterFoldNodeHighlighter, LazEditHighlighterUtils, LazEditFoldHighlighter,
  // SynEdit
  SynEditHighlighter, SynEditTypes, LazSynEditText;

const
  NullRange = TSynEditRange(nil);

type

  TSynFoldAction  = LazEditFoldHighlighter.TSynFoldAction;
  TSynFoldActions = LazEditFoldHighlighter.TSynFoldActions;

  TSynFoldBlockFilterFlag  = LazEditFoldHighlighter.TSynFoldBlockFilterFlag;
  TSynFoldBlockFilterFlags = LazEditFoldHighlighter.TSynFoldBlockFilterFlags;
  TSynFoldBlockFilter      = LazEditFoldHighlighter.TSynFoldBlockFilter;

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter;
                              AFoldGroup: Integer = 0; AFlag: TSynFoldBlockFilterFlags = []);

type
  TSynFoldNodeInfo = LazEditHighlighterFoldNodeHighlighter.TLazEditFoldNodeInfo deprecated 'use TLazEditFoldNodeInfo // will be removed in 5.99';
  PSynFoldNodeInfo = LazEditHighlighterFoldNodeHighlighter.PLazEditFoldNodeInfo deprecated 'use PLazEditFoldNodeInfo // will be removed in 5.99';
  TLazSynFoldNodeInfoList = LazEditHighlighterFoldNodeHighlighter.TLazEditFoldNodeInfoList deprecated 'use TLazEditFoldNodeInfoList // will be removed in 5.99';

const
  sfaOpen               = LazEditFoldHighlighter.TSynFoldAction.sfaOpen               deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOpen // will be removed in 5.99';
  sfaClose              = LazEditFoldHighlighter.TSynFoldAction.sfaClose              deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaClose // will be removed in 5.99';
  sfaFold               = LazEditFoldHighlighter.TSynFoldAction.sfaFold               deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaFold // will be removed in 5.99';
  sfaFoldFold           = LazEditFoldHighlighter.TSynFoldAction.sfaFoldFold           deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaFoldFold // will be removed in 5.99';
  sfaFoldHide           = LazEditFoldHighlighter.TSynFoldAction.sfaFoldHide           deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaFoldHide // will be removed in 5.99';
  sfaMultiLine          = LazEditFoldHighlighter.TSynFoldAction.sfaMultiLine          deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaMultiLine // will be removed in 5.99';
  sfaSingleLine         = LazEditFoldHighlighter.TSynFoldAction.sfaSingleLine         deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaSingleLine // will be removed in 5.99';
  sfaCloseForNextLine   = LazEditFoldHighlighter.TSynFoldAction.sfaCloseForNextLine   deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaCloseForNextLine // will be removed in 5.99';
  sfaLastLineClose      = LazEditFoldHighlighter.TSynFoldAction.sfaLastLineClose      deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaLastLineClose // will be removed in 5.99';
  sfaCloseAndOpen       = LazEditFoldHighlighter.TSynFoldAction.sfaCloseAndOpen       deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaCloseAndOpen // will be removed in 5.99';
  sfaDefaultCollapsed   = LazEditFoldHighlighter.TSynFoldAction.sfaDefaultCollapsed   deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaDefaultCollapsed // will be removed in 5.99';
  sfaMarkup             = LazEditFoldHighlighter.TSynFoldAction.sfaMarkup             deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaMarkup // will be removed in 5.99';
  sfaOutline            = LazEditFoldHighlighter.TSynFoldAction.sfaOutline            deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutline // will be removed in 5.99';
  sfaOutlineKeepLevel   = LazEditFoldHighlighter.TSynFoldAction.sfaOutlineKeepLevel   deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutlineKeepLevel // will be removed in 5.99';
  sfaOutlineMergeParent = LazEditFoldHighlighter.TSynFoldAction.sfaOutlineMergeParent deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutlineMergeParent // will be removed in 5.99';
  sfaOutlineForceIndent = LazEditFoldHighlighter.TSynFoldAction.sfaOutlineForceIndent deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutlineForceIndent // will be removed in 5.99';
  sfaInvalid            = LazEditFoldHighlighter.TSynFoldAction.sfaInvalid            deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaInvalid // will be removed in 5.99';
  sfaOpenFold           = LazEditFoldHighlighter.TSynFoldAction.sfaOpenFold           deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOpenFold // will be removed in 5.99';
  sfaCloseFold          = LazEditFoldHighlighter.TSynFoldAction.sfaCloseFold          deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaCloseFold // will be removed in 5.99';
  sfaOneLineOpen        = LazEditFoldHighlighter.TSynFoldAction.sfaOneLineOpen        deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOneLineOpen // will be removed in 5.99';
  sfaOneLineClose       = LazEditFoldHighlighter.TSynFoldAction.sfaOneLineClose       deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOneLineClose // will be removed in 5.99';

type
  TSynCustomFoldHighlighter = class;

  (* TLazSynEditNestedFoldsList
     Provides Info on all foldable-blocks containing a given line (0 based index).
     That are:
     - All foldable blocks opening on a previous line, that are still open
       at the start of the line. (May end on this line or later)
     - Foldable blocks opening on that line. (OpeningOnLineCount)

     The data is NOT automatically invalidated.
  *)

  TLazSynEditNestedFoldsListEntry = record
    FFLags: set of (nfeHasHNode, nfeMaxPrevReached);
    FGroupMinLevels: Array of Integer;
    //OpenCount: Integer;
    LineIdx: TLineIdx;
    EndLineIdx: TLineIdx;
    HNode: TSynFoldNodeInfo;    // Highlighter Node
    //FNode: TSynTextFoldAVLNode; // AvlFoldNode
    PrevNodeAtSameLevel: array of TLazSynEditNestedFoldsListEntry; // Only for same NodeGroup
  end;

//  TSynGetHighLighter = function(): TSynCustomFoldHighlighter of object;

  TLazSynEditNestedFoldsList = class
  { $Define DebugTLazSynEditNestedFoldsList}
  // TODO: in all methods: get "FoldNodeInfo" from FoldProvider, instead of Highlighter
  private
    FLines : TSynEditStrings;
    FHighLighter: TSynCustomFoldHighlighter;
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
    FNestInfo, FOnLineNestInfo: Array of TLazSynEditNestedFoldsListEntry;
    FEvaluationIndex: Integer;

    FPreviousNestInfo: Array of TLazSynEditNestedFoldsListEntry;
    FPreviousLine, FPreviousEvaluationIndex, FPreviousCount: Integer;
    FPreviousMergeLine: Integer;

    FFoldNodeInfoList: TLazEditFoldNodeInfoList;
    FFoldNodeInfoListHoldCnt: integer;

    function GetHLNode(Index: Integer): TSynFoldNodeInfo;
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
    procedure SetHighLighter(AValue: TSynCustomFoldHighlighter);
    procedure SetIncludeOpeningOnLine(AValue: Boolean);
    procedure AquireFoldNodeInfoList(const ALine: Integer = -1);
    procedure ReleaseFoldNodeInfoList;
    procedure SetLines(AValue: TSynEditStrings);
    procedure SetOpeningLineEndIndex(AValue: Integer);
    function HasCount: Boolean;
    procedure ClearPreviousCache;
  public
    constructor Create(ALines : TSynEditStrings; AnHighLighter: TSynCustomFoldHighlighter = nil);
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
    property Lines: TSynEditStrings read FLines write SetLines;
    property HighLighter: TSynCustomFoldHighlighter read FHighLighter write SetHighLighter;
  public
    property HLNode[Index: Integer]: TSynFoldNodeInfo read GetHLNode;
    property NodeFoldType[Index: Integer]: Pointer read GetNodeFoldType;        // e.g.cfbtBeginEnd, cfbtcfbtProcedure ...
    property NodeFoldGroup[Index: Integer]: Integer read GetNodeFoldGroup;      // independend/overlapping folds, e.g begin/end; ifdef, region
    property NodeLine[Index: Integer]: Integer read GetNodeLine;                // Index
    property NodeEndLine[Index: Integer]: Integer read GetNodeEndLine;          // Index
    property NodeLineEx[Index, PrevCount: Integer]: Integer read GetNodeLineEx; // Index
  end;

  TSynCustomFoldConfigMode  = LazEditFoldHighlighter.TSynCustomFoldConfigMode;
  TSynCustomFoldConfigModes = LazEditFoldHighlighter.TSynCustomFoldConfigModes;

  { TSynCustomFoldConfig }

  TSynCustomFoldConfig = class(TLazEditCustomFoldConfig)
  private
    FIsEssential: boolean;
    FOnChange: TNotifyEvent;
  protected
    procedure DoOnChange; override;
  public
    constructor Create;
    constructor Create(ASupportedModes: TSynCustomFoldConfigModes; AnIsEssential: Boolean = False);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property IsEssential: boolean read FIsEssential;   // create node, even if disabled
    property SupportedModes: TSynCustomFoldConfigModes read FSupportedModes write SetSupportedModes;
  end;

  PSynCustomFoldConfig = ^TSynCustomFoldConfig;

  { TSynCustomCodeFoldBlock }

  TSynCustomCodeFoldBlock = class(TLazHighlighterRangeForDictionary)
  private
    FBlockType: Pointer;
    FParent: TSynCustomCodeFoldBlock;
  public
    constructor Create; overload;
    procedure Assign(Src: TLazHighlighterRange); override;
  public
    procedure InitRootBlockType(AType: Pointer);
    property BlockType: Pointer read FBlockType;
    property Parent: TSynCustomCodeFoldBlock read FParent;
  end;
  TSynCustomCodeFoldBlockClass = class of TSynCustomCodeFoldBlock;

  { TSynCustomHighlighterRange }

  TSynCustomHighlighterRange = class(TLazHighlighterRangeForDictionary)
  private
    // TODO: either reduce to one level, or create subclass for 2nd level
    FCodeFoldStackSize: integer; // EndLevel
    FNestFoldStackSize: integer; // EndLevel
    FMinimumCodeFoldBlockLevel: integer;
    FMinimumNestFoldBlockLevel: integer;
    FRangeType: Pointer;
    FTop: TSynCustomCodeFoldBlock;
  public
    constructor Create(Template: TLazHighlighterRange); override;
    destructor Destroy; override;
    function Compare(Range: TLazHighlighterRange): integer; override;
    function Push(ABlock: TSynCustomCodeFoldBlock; IncreaseLevel: Boolean = True):
        TSynCustomCodeFoldBlock; inline;
    procedure Pop(DecreaseLevel: Boolean = True); inline;
    function MaxFoldLevel: Integer; virtual;
    procedure Clear; virtual;
    procedure Assign(ASrc: TLazHighlighterRange); override;
    procedure WriteDebugReport;
    property FoldRoot: TSynCustomCodeFoldBlock read FTop write FTop;
  public
    property RangeType: Pointer read FRangeType write FRangeType;
    property CodeFoldStackSize: integer read FCodeFoldStackSize; // excl disabled, only IncreaseLevel
    property MinimumCodeFoldBlockLevel: integer
      read FMinimumCodeFoldBlockLevel write FMinimumCodeFoldBlockLevel;
    property NestFoldStackSize: integer read FNestFoldStackSize; // all, incl disabled (not IncreaseLevel)
    property MinimumNestFoldBlockLevel: integer
      read FMinimumNestFoldBlockLevel; // write FMinimumNestFoldBlockLevel;
    property Top: TSynCustomCodeFoldBlock read FTop;
  end;
  TSynCustomHighlighterRangeClass = class of TSynCustomHighlighterRange deprecated 'use TLazHighlighterRangeClass // will be removed in 5.99';

  { TSynCustomFoldHighlighter }

  TSynCustomFoldHighlighter = class(TSynCustomHighlighter)
  private const
    MAX_UFOLD_CAPACITY = 16;
  private type
    TUncommittedFold = record
      FBlockType: Pointer;
      FIncreaseLevel: Boolean;
    end;
  protected
    // Fold Config
    FFoldConfig: Array of TSynCustomFoldConfig;
    function GetFoldConfig(Index: Integer): TSynCustomFoldConfig; reintroduce; virtual;
    procedure SetFoldConfig(Index: Integer; const AValue: TSynCustomFoldConfig); reintroduce; virtual;
    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; virtual;
    function CreateFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; virtual;
    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; virtual;
    procedure InitFoldConfig;
    procedure DestroyFoldConfig;
    procedure DoFoldConfigChanged(Sender: TObject); virtual;
  private
    FCodeFoldRange: TSynCustomHighlighterRange;
    FUncommittedFolds: array of TUncommittedFold;
    FUncommittedFoldStackCount, FUncommittedFoldNestCount: integer;
    FIsCollectingNodeInfo: boolean;
    FRanges: TLazHighlighterRangesDictionary;
    FFoldBlockRanges: TLazHighlighterRangesDictionary;
    FRootCodeFoldBlock, FTepmCodeFoldBlock: TSynCustomCodeFoldBlock;
    FFoldNodeInfoList: TLazEditFoldNodeInfoList;
    FCollectingNodeInfoList: TLazEditFoldNodeInfoList;
    procedure ClearFoldNodeList;
    procedure CommitUncommittedFolds; inline;
  protected
    // "Range"
    function GetRangeClass: TLazHighlighterRangeClass; virtual;
    procedure CreateRootCodeFoldBlock; virtual; // set RootCodeFoldBlock
    property CodeFoldRange: TSynCustomHighlighterRange read FCodeFoldRange;
    function TopCodeFoldBlockType(DownIndex: Integer = 0): Pointer;
    property RootCodeFoldBlock: TSynCustomCodeFoldBlock read FRootCodeFoldBlock
      write FRootCodeFoldBlock;

    // Open/Close Folds
    function StartCodeFoldBlock(ABlockType: Pointer = nil;
              IncreaseLevel: Boolean = true; ForceDisabled: Boolean = False): Boolean; virtual;
    procedure EndCodeFoldBlock(DecreaseLevel: Boolean = True); virtual;
    procedure CollectNodeInfo(FinishingABlock : Boolean; ABlockType: Pointer;
              LevelChanged: Boolean); virtual;
    procedure DoInitNode(var Node: TSynFoldNodeInfo;
                       FinishingABlock: Boolean;
                       ABlockType: Pointer; aActions: TSynFoldActions;
                       AIsFold: Boolean); virtual;
    procedure RepairSingleLineNode(var Node: TSynFoldNodeInfo); virtual;
    procedure GetTokenBounds(out LogX1,LogX2: Integer); virtual;

    // Info about Folds
    function CreateFoldNodeInfoList: TLazEditFoldNodeInfoList; virtual;
    function GetFoldNodeInfo(Line: TLineIdx): TLazEditFoldNodeInfoList;
    procedure ScanFoldNodeInfo(); virtual;
    procedure InitFoldNodeInfo(AList: TLazEditFoldNodeInfoList; Line: TLineIdx); override;

    // Info about Folds, on currently set line/range (simply forwarding to range
    function MinimumCodeFoldBlockLevel: integer; inline;
    function CurrentCodeFoldBlockLevel: integer; inline;
    function CurrentCodeNestBlockLevel: integer; inline;

    property IsCollectingNodeInfo : boolean read FIsCollectingNodeInfo;
    property CollectingNodeInfoList : TLazEditFoldNodeInfoList read FCollectingNodeInfoList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    function GetRange: Pointer; override;

    // Info about Folds
    function FoldBlockEndLevel(ALineIndex: TLineIdx;
                               const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx;
                               const AFilter: TSynFoldBlockFilter): integer; override; overload;

    function FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer;  deprecated;
    function FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer; deprecated;
    function FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer; deprecated;

    function FoldEndLine(ALineIndex, FoldIndex: Integer): integer; virtual; overload; // deprecate // fix inherited classes

    // All fold-nodes
    // FoldNodeInfo: Returns a shared object
    // Adding RefCount, will prevent others from getting further copies, but not from using copies they already have.
    // If not adding refcount, the object should not be stored/re-used
    // Not adding ref-count, should only be done for CountEx, NodeInfoEx
    property FoldNodeInfo[Line: TLineIdx]: TLazEditFoldNodeInfoList read GetFoldNodeInfo;

    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure InitForScanningLine; override;
    procedure DoCurrentLinesChanged; override;
    function DoPrepareLines(AFirstLineIdx: IntIdx; AMinimumRequiredLineIdx: IntIdx = - 1;
      AMaxTime: integer = 0): integer; override;

  public
    property FoldConfig[Index: Integer]: TSynCustomFoldConfig read GetFoldConfig write SetFoldConfig;
  end;

  { TSynCustomHighlighterRangeTree }

  TSynCustomHighlighterRangeTree = class(TLazHighlighterRanges)
  private
    FItems: TAvlTree;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetEqual(Range: TLazHighlighterRange
                      ): TLazHighlighterRange; override;
  end experimental; // replaced by TLazHighlighterRangesDictionary

function CompareSynHighlighterRanges(Data1, Data2: Pointer): integer;
function AllocateHighlighterRanges(
     HighlighterClass: TSynCustomHighlighterClass): TLazHighlighterRangesDictionary;
     deprecated 'use GetHighlighterRangesForHighlighter // will be removed in 5.99';

function dbgs(AFoldActions: TSynFoldActions): String; overload;
function dbgs(ANode: TSynFoldNodeInfo):string; overload;
function dbgs(AMode: TSynCustomFoldConfigMode): String; overload;
function dbgs(AModes: TSynCustomFoldConfigModes): String; overload;
function dbgs(AFoldFlag: TSynFoldBlockFilterFlag): String; overload;
function dbgs(AFoldFlags: TSynFoldBlockFilterFlags): String; overload;
function dbgs(ANestInfo: TLazSynEditNestedFoldsListEntry): String; overload;

implementation

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter; AFoldGroup: Integer;
  AFlag: TSynFoldBlockFilterFlags = []);
begin
  AFilter.FoldGroup := AFoldGroup;
  AFilter.Flags     := AFlag;
end;

function CompareSynHighlighterRanges(Data1, Data2: Pointer): integer;
var
  Range1: TSynCustomHighlighterRange;
  Range2: TSynCustomHighlighterRange;
begin
  Range1:=TSynCustomHighlighterRange(Data1);
  Range2:=TSynCustomHighlighterRange(Data2);
  Result:=Range1.Compare(Range2);
end;

function AllocateHighlighterRanges(
  HighlighterClass: TSynCustomHighlighterClass): TLazHighlighterRangesDictionary;
begin
  Result := TLazHighlighterRangesDictionary(GetHighlighterRangesForHighlighter(HighlighterClass, TLazHighlighterRangesDictionary));
  Result.AddReference;
end;

function dbgs(AFoldActions: TSynFoldActions): String;
var
  i: TSynFoldAction;
  s: string;
begin
  Result:='';
  for i := low(TSynFoldAction) to high(TSynFoldAction) do
    if i in AFoldActions then begin
      WriteStr(s{%H-}, i);
      Result := Result + s + ',';
    end;
  if Result <> '' then Result := '[' + copy(Result, 1, Length(Result)-1) + ']';
end;

function dbgs(ANode: TSynFoldNodeInfo): string;
begin
  with ANode do
    if sfaInvalid in FoldAction then
      Result := Format('L=%3d I=%d  X=%2d-%2d  Fld=%d-%d Nst=%d-%d  FT=%2d FTC=%2d  Grp=%d  A=%s',
                       [LineIndex, NodeIndex, 0, 0, 0, 0, 0, 0, 0, 0, 0, dbgs(FoldAction)])
    else
      Result := Format('L=%3d I=%d  X=%2d-%2d  Fld=%d-%d Nst=%d-%d  FT=%2d FTC=%2d  Grp=%d  A=%s',
                       [LineIndex, NodeIndex, LogXStart, LogXEnd,
                        FoldLvlStart, FoldLvlEnd, NestLvlStart, NestLvlEnd,
                        PtrUInt(FoldType), PtrUInt(FoldTypeCompatible), FoldGroup,
                        dbgs(FoldAction)]);
end;

function dbgs(AMode: TSynCustomFoldConfigMode): String;
begin
  WriteStr(Result{%H-}, AMode);
end;

function dbgs(AModes: TSynCustomFoldConfigModes): String;
var
  i: TSynCustomFoldConfigMode;
  s: string;
begin
  Result:='';
  for i := low(TSynCustomFoldConfigMode) to high(TSynCustomFoldConfigMode) do
    if i in AModes then begin
      WriteStr(s{%H-}, i);
      Result := Result + s + ',';
    end;
  if Result <> '' then Result := '[' + copy(Result, 1, Length(Result)-1) + ']';
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

function dbgs(ANestInfo: TLazSynEditNestedFoldsListEntry): String;
var
  i: Integer;
begin
  Result := Format('LineIdx:%4d', [ANestInfo.LineIdx ])
   +' HNode: '+dbgs(ANestInfo.HNode)
   +' | PrevCnt: '+dbgs(length(ANestInfo.PrevNodeAtSameLevel))
   +' MinLvl: [';
  for i := 0  to high(ANestInfo.FGroupMinLevels) do
    Result := Result+inttostr(ANestInfo.FGroupMinLevels[i])+',';
  Result := Result+']';
end;

{ TLazSynEditNestedFoldsList }

procedure TLazSynEditNestedFoldsList.SetLine(AValue: TLineIdx);
begin
  if FLine = AValue then Exit;
  {$IfDef DebugTLazSynEditNestedFoldsList}
  debugln(['TLazSynEditNestedFoldsList.SetLine ', AValue, ' from previous ', FLine]);
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

procedure TLazSynEditNestedFoldsList.Clear;
begin
  {$IfDef DebugTLazSynEditNestedFoldsList}
  debugln(['TLazSynEditNestedFoldsList.Clear ']);
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

procedure TLazSynEditNestedFoldsList.ResetFilter;
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

procedure TLazSynEditNestedFoldsList.InitSubGroupEndLevels;
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

function TLazSynEditNestedFoldsList.GetHLNode(Index: Integer): TSynFoldNodeInfo;
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

function TLazSynEditNestedFoldsList.GetNodeEndLine(Index: Integer): Integer;
var
  nd: TSynFoldNodeInfo;
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
  debugln(['TLazSynEditNestedFoldsList.GetNodeEndLine  for ', Index, '    from curidx ',CurIdx, '   of cnt ', Count, '  / ', FCount ]);
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

function TLazSynEditNestedFoldsList.GetNodeFoldGroup(Index: Integer): Integer;
begin
  if FoldGroup <> 0 then
    Result := FoldGroup
  else
    Result := HLNode[Index].FoldGroup;
end;

function TLazSynEditNestedFoldsList.GetNodeLine(Index: Integer): Integer;
begin
  InitLineInfoForIndex(Index);
  if Index >= FCount then
    Result := FLine
  else
    Result := FNestInfo[Index].LineIdx;
end;

function TLazSynEditNestedFoldsList.GetNodeFoldType(Index: Integer): Pointer;
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

function TLazSynEditNestedFoldsList.GetNodeLineEx(Index, PrevCount: Integer): Integer;
var
  Node: TLazSynEditNestedFoldsListEntry;
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

procedure TLazSynEditNestedFoldsList.InitNestInfoForIndex(AnIndex: Integer);
var
  CurLine: TLineIdx;
  i, EvalIdx, c, t, l: Integer;
  NFilter: TSynFoldActions;
  nd: TSynFoldNodeInfo;
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
    debugln(['TLazSynEditNestedFoldsList.InitNestInfoForIndex CurLine=',CurLine, '  c=',c, '  EvalIdx=',EvalIdx]);
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

      assert(GrpCnt[t] >= 0, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex GroupEndLevel < 0');
      assert(EvalIdx >= 0, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex FEvaluationIndex < 0');
      assert(FNestInfo[EvalIdx].LineIdx = CurLine, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex FNestInfo[EvalIdx].LineIdx = CurLine');

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
  assert(AnIndex >= FEvaluationIndex, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex Index not found');
end;

procedure TLazSynEditNestedFoldsList.InitLineInfoForIndex(AnIndex: Integer);
var
  CurLine: TLineIdx;
  i, c, c1, l: Integer;

  procedure DoMergePrevious;   // TODO: copy nodeinfo if avail
  var
    pcnt, c, l, c1: integer;
  begin
    pcnt := FPreviousCount - 1;
    {$IfDef DebugTLazSynEditNestedFoldsList}
    debugln(['TLazSynEditNestedFoldsList.InitLineInfoForIndex() DoMergePrev (',pcnt, ' ',FPreviousEvaluationIndex ,') ', FPreviousNestInfo[pcnt].LineIdx,' to ', FPreviousNestInfo[FPreviousEvaluationIndex].LineIdx, ' FEvaluationIndex:',FEvaluationIndex, ' CurLine=',CurLine ]);
    {$EndIf}
    assert(FPreviousNestInfo[pcnt].LineIdx = CurLine, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex.DoMergePrevious LineIdx = CurLine');
    while  pcnt >= FPreviousEvaluationIndex do begin
      while (pcnt > 0) and (pcnt > FPreviousEvaluationIndex) and
            (FPreviousNestInfo[pcnt].LineIdx = FPreviousNestInfo[pcnt-1].LineIdx)
      do
        dec(pcnt);
      assert(length(FPreviousNestInfo[pcnt].FGroupMinLevels) > 0, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex.DoMergePrevious FGroupEndLevels > 0');

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
  assert(FEvaluationIndex > 0, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex already finilhed');

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
  debugln(['TLazSynEditNestedFoldsList.InitLineInfoForIndex FEvaluationIndex=', FEvaluationIndex, '  AnIndex=',AnIndex]);
  for i := FCount-1 downto 0 do begin DbgOut([', ',FNestInfo[i].LineIdx]); if length(FNestInfo[i].FGroupMinLevels) > 0 then begin DbgOut(' ('); for c := 0 to length(FNestInfo[i].FGroupMinLevels)-1 do DbgOut([',',FNestInfo[i].FGroupMinLevels[c]]);  DbgOut(') '); end; end; DebugLn();
  {$EndIf}
  assert(CurLine >= 0, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex Curline < 0');
  assert(AnIndex >= FEvaluationIndex, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex Index not found');
end;

procedure TLazSynEditNestedFoldsList.InitCount;
begin
  if FHighLighter = nil then exit;
  FHighLighter.CurrentLines := FLines;

  FCount := FHighlighter.FoldBlockEndLevel(FLine - 1, FFoldGroup, FFoldFlags);
  FEvaluationIndex := FCount;
  SetLength(FNestInfo, FCount+1);
end;

procedure TLazSynEditNestedFoldsList.InitOpeningOnLine;
var
  nd: TSynFoldNodeInfo;
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
      Assert(oc >= 0, 'TLazSynEditNestedFoldsList.InitOpeningOnLine bad count for '+IntToStr(Grp));
      Assert((oc=0) or (OpenIdx[Grp - GrpLow, oc-1] <= i), 'TLazSynEditNestedFoldsList.InitOpeningOnLine bad index for '+IntToStr(i)+' G='+IntToStr(Grp));
      if (oc > 0) and (OpenIdx[Grp - GrpLow, oc-1] = i) then begin
        dec(OpenCnt[Grp - GrpLow]);
        dec(j);
        FOnLineNestInfo[j].LineIdx := FLine;
        FOnLineNestInfo[j].HNode := nd;
        FOnLineNestInfo[j].HNode.NodeIndex := j;
        FOnLineNestInfo[j].EndLineIdx := 0;
      end;
    end;

    Assert(j=0, 'TLazSynEditNestedFoldsList.InitOpeningOnLine did not fill all nodes '+IntToStr(j));
  finally
    ReleaseFoldNodeInfoList;
  end;
end;

procedure TLazSynEditNestedFoldsList.SetFoldFlags(AValue: TSynFoldBlockFilterFlags);
begin
  if FFoldFlags = AValue then Exit;
  FFoldFlags := AValue;
  Clear;
end;

procedure TLazSynEditNestedFoldsList.SetHighLighter(
  AValue: TSynCustomFoldHighlighter);
begin
  if FHighLighter = AValue then Exit;
  FHighLighter := AValue;
  Clear;
end;

procedure TLazSynEditNestedFoldsList.SetIncludeOpeningOnLine(AValue: Boolean);
begin
  if FIncludeOpeningOnLine = AValue then Exit;
  FIncludeOpeningOnLine := AValue;
  //Clear; // Do not Clear, keep the data, can be re-enabled
end;

procedure TLazSynEditNestedFoldsList.AquireFoldNodeInfoList(const ALine: Integer
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

procedure TLazSynEditNestedFoldsList.ReleaseFoldNodeInfoList;
begin
  dec(FFoldNodeInfoListHoldCnt);
  if FFoldNodeInfoListHoldCnt = 0 then
    ReleaseRefAndNil(FFoldNodeInfoList);
end;

procedure TLazSynEditNestedFoldsList.SetLines(AValue: TSynEditStrings);
begin
  if FLines = AValue then Exit;
  FLines := AValue;
  Clear;
end;

procedure TLazSynEditNestedFoldsList.SetOpeningLineEndIndex(AValue: Integer);
begin
  if FOpeningLineEndIndex = AValue then Exit;
  FOpeningLineEndIndex := AValue;
  Clear; // TODO only clear current line, the rest will still be valid
end;

function TLazSynEditNestedFoldsList.HasCount: Boolean;
begin
  Result := (FCount >= 0) and ( (not FIncludeOpeningOnLine) or (FOpeningOnLineCount >= 0) );
end;

procedure TLazSynEditNestedFoldsList.ClearPreviousCache;
begin
  FPreviousCount := -1;
  FPreviousEvaluationIndex := -1;
  SetLength(FPreviousNestInfo, 0);
end;

procedure TLazSynEditNestedFoldsList.SetFoldGroup(AValue: Integer);
begin
  if FFoldGroup = AValue then Exit;
  FFoldGroup := AValue;
  Clear;
end;

constructor TLazSynEditNestedFoldsList.Create(ALines: TSynEditStrings;
  AnHighLighter: TSynCustomFoldHighlighter);
begin
  FLines := ALines;
  FHighLighter := AnHighLighter;
  FIncludeOpeningOnLine := True;
  FFoldFlags := [];
  FFoldGroup := 0;
  FFoldNodeInfoListHoldCnt := 0;
end;

function TLazSynEditNestedFoldsList.Count: Integer;
begin
  if (FCount < 0) then begin
    InitCount;
  end;
  if FIncludeOpeningOnLine and (FOpeningOnLineCount < 0) then begin
    InitOpeningOnLine;
  end;

  Result := FCount + OpeningOnLineCount;
end;

function TLazSynEditNestedFoldsList.OpeningOnLineCount: Integer;
begin
  if (not FIncludeOpeningOnLine) or (FLine < 0) then
    exit(0);

  if (FOpeningOnLineCount < 0) then begin
    InitOpeningOnLine;
  end;

  Result := FOpeningOnLineCount;
end;

procedure TLazSynEditNestedFoldsList.Debug;
var
  i: Integer;
begin
  Debugln(['TLazSynEditNestedFoldsList for FFoldGroup=', FFoldGroup, ' FLine=', FLine,
           ' FFoldFlags=', dbgs(FFoldFlags), ' FGroupCount=', FGroupCount,
           ' FIncludeOpeningOnLine=', dbgs(FIncludeOpeningOnLine), ' FEvaluationIndex=', FEvaluationIndex,
           ' FCount=', FCount, ' FOpeningOnLineCount=', FOpeningOnLineCount]);
  Debugln(['FGroupEndLevelsAtEval=', length(FGroupEndLevelsAtEval), ': ']); for i := 0 to length(FGroupEndLevelsAtEval)-1 do DbgOut([FGroupEndLevelsAtEval[i]]); Debugln;
  for i := 0 to length(FNestInfo)-1 do
    Debugln(['N-Info ', i,': ',dbgs(FNestInfo[i])]);
end;

{ TSynCustomFoldHighlighter }

constructor TSynCustomFoldHighlighter.Create(AOwner: TComponent);
begin
  SetLength(FFoldConfig, GetFoldConfigInternalCount);
  InitFoldConfig;
  fRanges := TLazHighlighterRangesDictionary(
    GetHighlighterRangesForHighlighter(TSynCustomHighlighterClass(ClassType), TLazHighlighterRangesDictionary)
  );
  fRanges.AddReference;

  FFoldBlockRanges := TLazHighlighterRangesDictionary(
    GetHighlighterRangesForHighlighter(TSynCustomHighlighterClass(pointer(ClassType)+1), TLazHighlighterRangesDictionary)
  );
  FFoldBlockRanges.AddReference;

  CreateRootCodeFoldBlock;
  FTepmCodeFoldBlock := FRootCodeFoldBlock;
  FRootCodeFoldBlock := TSynCustomCodeFoldBlock(FFoldBlockRanges.GetEqual(FRootCodeFoldBlock));
  inherited Create(AOwner);
  FCodeFoldRange:=TSynCustomHighlighterRange(GetRangeClass.Create(nil));
  FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
  FFoldNodeInfoList := nil;
end;

destructor TSynCustomFoldHighlighter.Destroy;
begin
  inherited Destroy;
  DestroyFoldConfig;
  FreeAndNil(FCodeFoldRange);
  FreeAndNil(FTepmCodeFoldBlock);
  ReleaseRefAndNil(FFoldNodeInfoList);
  fRanges.ReleaseReference;
  FFoldBlockRanges.ReleaseReference;
  FFoldConfig := nil;
end;

class function TSynCustomFoldHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcCodeFolding];
end;

function TSynCustomFoldHighlighter.GetRange: Pointer;
begin
  // FCodeFoldRange is the working range and changed steadily
  // => return a fixed copy of the current CodeFoldRange instance,
  //    that can be stored by other classes (e.g. TSynEdit)
  CommitUncommittedFolds;
  Result:=fRanges.GetEqual(FCodeFoldRange);
end;

function TSynCustomFoldHighlighter.FoldBlockEndLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
var
  r: Pointer;
begin
  Assert(CurrentRanges <> nil, 'TSynCustomFoldHighlighter.FoldBlockEndLevel requires CurrentRanges');
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count - 1) then
    exit(0);
  r := CurrentRanges[ALineIndex];
  if (r <> nil) and (r <> NullRange) then begin
    if sfbIncludeDisabled in AFilter.Flags then
      Result := TSynCustomHighlighterRange(r).NestFoldStackSize
    else
      Result := TSynCustomHighlighterRange(r).CodeFoldStackSize;
  end
  else
    Result:=0;
end;

function TSynCustomFoldHighlighter.FoldBlockMinLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
var
  r: Pointer;
begin
  Assert(CurrentRanges <> nil, 'TSynCustomFoldHighlighter.FoldBlockMinLevelrequires CurrentRanges');
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count - 1) then
    exit(0);
  r := CurrentRanges[ALineIndex];
  if (r <> nil) and (r <> NullRange) then begin
    if sfbIncludeDisabled in AFilter.Flags then
      Result := TSynCustomHighlighterRange(r).MinimumNestFoldBlockLevel
    else
      Result := TSynCustomHighlighterRange(r).MinimumCodeFoldBlockLevel
  end
  else
    Result:=0;
end;

procedure TSynCustomFoldHighlighter.ResetRange;
begin
  FCodeFoldRange.Clear;
  FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
end;

function TSynCustomFoldHighlighter.MinimumCodeFoldBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'MinimumCodeFoldBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.MinimumCodeFoldBlockLevel;
end;

procedure TSynCustomFoldHighlighter.SetRange(Value: Pointer);
begin
  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
  FCodeFoldRange.Assign(TSynCustomHighlighterRange(Value));
  // in case we asigned a null range
  if not assigned(FCodeFoldRange.FoldRoot) then
    FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
end;

procedure TSynCustomFoldHighlighter.InitForScanningLine;
begin
  inherited;
  FCodeFoldRange.MinimumCodeFoldBlockLevel := FCodeFoldRange.FCodeFoldStackSize;
  FCodeFoldRange.FMinimumNestFoldBlockLevel := FCodeFoldRange.NestFoldStackSize;
end;

procedure TSynCustomFoldHighlighter.DoCurrentLinesChanged;
begin
  inherited DoCurrentLinesChanged;
  ClearFoldNodeList;
end;

function TSynCustomFoldHighlighter.DoPrepareLines(AFirstLineIdx: IntIdx;
  AMinimumRequiredLineIdx: IntIdx; AMaxTime: integer): integer;
begin
  ClearFoldNodeList;
  Result := inherited DoPrepareLines(AFirstLineIdx, AMinimumRequiredLineIdx, AMaxTime);

  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
  if Length(FUncommittedFolds) > MAX_UFOLD_CAPACITY then
    SetLength(FUncommittedFolds, MAX_UFOLD_CAPACITY);
end;

function TSynCustomFoldHighlighter.CurrentCodeFoldBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'CurrentCodeFoldBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.CodeFoldStackSize + FUncommittedFoldStackCount;
end;

function TSynCustomFoldHighlighter.CurrentCodeNestBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'CurrentCodeNestBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.NestFoldStackSize + FUncommittedFoldNestCount;
end;

function TSynCustomFoldHighlighter.FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer;
begin
  result := FoldBlockOpeningCount(ALineIndex, AType);
end;

function TSynCustomFoldHighlighter.FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer;
begin
  result := FoldBlockClosingCount(ALineIndex, AType);
end;

function TSynCustomFoldHighlighter.FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer;
begin
  Result := FoldBlockEndLevel(ALineIndex, AType);
end;

function TSynCustomFoldHighlighter.FoldEndLine(ALineIndex, FoldIndex: Integer): integer;
begin
  Result := FoldEndLine(ALineIndex, FoldIndex, 0, []);
end;

function TSynCustomFoldHighlighter.GetFoldConfig(Index: Integer): TSynCustomFoldConfig;
begin
  Result := FFoldConfig[Index];
end;

procedure TSynCustomFoldHighlighter.SetFoldConfig(Index: Integer; const AValue: TSynCustomFoldConfig);
begin
  BeginUpdate;
  FFoldConfig[Index].Assign(AValue);
  EndUpdate;
end;

function TSynCustomFoldHighlighter.GetFoldConfigCount: Integer;
begin
  Result := 0;
end;

function TSynCustomFoldHighlighter.GetFoldConfigInternalCount: Integer;
begin
  Result := 0;
end;

function TSynCustomFoldHighlighter.CreateFoldConfigInstance(Index: Integer
  ): TSynCustomFoldConfig;
begin
  Result := TSynCustomFoldConfig.Create;
end;

function TSynCustomFoldHighlighter.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result := CreateFoldConfigInstance(Index);
  Result.Enabled := False;
end;

procedure TSynCustomFoldHighlighter.InitFoldConfig;
var
  i: Integer;
begin
  for i := 0 to high(FFoldConfig) do begin
    FFoldConfig[i] := GetFoldConfigInstance(i);
    if not assigned(FFoldConfig[i].OnChange) then
      FFoldConfig[i].OnChange := @DoFoldConfigChanged;
  end;
end;

procedure TSynCustomFoldHighlighter.DestroyFoldConfig;
var
  i: Integer;
begin
  for i := 0 to high(FFoldConfig) do
    FFoldConfig[i].Free;
end;

procedure TSynCustomFoldHighlighter.DoFoldConfigChanged(Sender: TObject);
begin
  RequestFullRescan;
end;

procedure TSynCustomFoldHighlighter.ClearFoldNodeList;
begin
  if FFoldNodeInfoList <> nil then begin
    if (FFoldNodeInfoList.RefCount > 1) then
      ReleaseRefAndNil(FFoldNodeInfoList)
    else
      FFoldNodeInfoList.Clear;
  end;
end;

procedure TSynCustomFoldHighlighter.CommitUncommittedFolds;
var
  i: Integer;
  b, p: TSynCustomCodeFoldBlock;
begin
  p := FCodeFoldRange.FTop;
  for i := 0 to FUncommittedFoldNestCount - 1 do begin
    FTepmCodeFoldBlock.FBlockType := FUncommittedFolds[i].FBlockType;
    FTepmCodeFoldBlock.FParent := p;
    p := TSynCustomCodeFoldBlock(FFoldBlockRanges.GetEqual(FTepmCodeFoldBlock));
    FCodeFoldRange.Push(p, FUncommittedFolds[i].FIncreaseLevel);  // TODO: may fail
  end;

  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
end;

function TSynCustomFoldHighlighter.GetFoldNodeInfo(Line: TLineIdx
  ): TLazEditFoldNodeInfoList;
begin
  if (FFoldNodeInfoList <> nil) and (FFoldNodeInfoList.RefCount > 1) then
    ReleaseRefAndNil(FFoldNodeInfoList);

  if FFoldNodeInfoList = nil then begin
    FFoldNodeInfoList := CreateFoldNodeInfoList;
    FFoldNodeInfoList.AddReference;
  end
  else
  if (CurrentRanges <> nil) and (CurrentRanges.FirstInvalidLine >= 0) then
    ClearFoldNodeList;


  Result := FFoldNodeInfoList;
  if (Line >= 0) and
     ((Result.Line <> Line) or (not Result.valid))
  then
    Result.ClearFilter;
  Result.Line := Line;
end;

procedure TSynCustomFoldHighlighter.ScanFoldNodeInfo;
begin
  NextToEol;
end;

procedure TSynCustomFoldHighlighter.InitFoldNodeInfo(AList: TLazEditFoldNodeInfoList; Line: TLineIdx);
begin
  FIsCollectingNodeInfo := True;
  try
    assert(FCollectingNodeInfoList = nil, 'TSynCustomFoldHighlighter.InitFoldNodeInfo: FCollectingNodeInfoList = nil');
    FCollectingNodeInfoList := AList;
    StartAtLineIndex(Line);
    ScanFoldNodeInfo();
  finally
    FIsCollectingNodeInfo := False;
    FCollectingNodeInfoList := nil;
  end;
end;

function TSynCustomFoldHighlighter.CreateFoldNodeInfoList: TLazEditFoldNodeInfoList;
begin
  Result := TLazEditFoldNodeInfoList.Create(Self);
end;

function TSynCustomFoldHighlighter.GetRangeClass: TLazHighlighterRangeClass;
begin
  Result:=TSynCustomHighlighterRange;
end;

function TSynCustomFoldHighlighter.TopCodeFoldBlockType(DownIndex: Integer = 0): Pointer;
var
  Fold: TSynCustomCodeFoldBlock;
begin
  Result:=nil;
  if DownIndex < FUncommittedFoldNestCount then begin
    Result := FUncommittedFolds[FUncommittedFoldNestCount-1 - DownIndex].FBlockType;
    exit;
  end;

  DownIndex := DownIndex - FUncommittedFoldNestCount;
  if (CodeFoldRange<>nil) then begin
    Fold := CodeFoldRange.Top;
    while (Fold <> nil) and (DownIndex > 0) do begin
      Fold := Fold.Parent;
      dec(DownIndex);
    end;
    if Fold <> nil then
      Result := Fold.BlockType
  end;
end;

procedure TSynCustomFoldHighlighter.GetTokenBounds(out LogX1, LogX2: Integer);
var p : pchar; L : integer;
begin
  GetTokenEx(p,L);
  LogX1 := GetTokenPos;
  LogX2 := LogX1 + L ;
end;

function TSynCustomFoldHighlighter.StartCodeFoldBlock(ABlockType: Pointer; IncreaseLevel: Boolean;
  ForceDisabled: Boolean): Boolean;
begin
  Result := False;
  if (PtrUInt(ABlockType) < FoldConfigCount) and (not ForceDisabled) and
     (not FoldConfig[PtrUInt(ABlockType)].Enabled) and
     (not FoldConfig[PtrUInt(ABlockType)].IsEssential)
  then
    exit;

  Result := True;

  if FIsCollectingNodeInfo then
    CollectNodeInfo(False, ABlockType, IncreaseLevel);

  if Length(FUncommittedFolds) <= FUncommittedFoldNestCount then
    SetLength(FUncommittedFolds, FUncommittedFoldNestCount + 4);
  FUncommittedFolds[FUncommittedFoldNestCount].FBlockType := ABlockType;
  FUncommittedFolds[FUncommittedFoldNestCount].FIncreaseLevel := IncreaseLevel;
  inc(FUncommittedFoldNestCount);
  if IncreaseLevel then
    inc(FUncommittedFoldStackCount);
//  CodeFoldRange.Add(ABlockType, IncreaseLevel);
end;

procedure TSynCustomFoldHighlighter.EndCodeFoldBlock(DecreaseLevel: Boolean = True);
var
  BlockType: Pointer;
begin
  //ABlockType required for detect whether singleline /multiline is being paired
  BlockType := TopCodeFoldBlockType;
  if FIsCollectingNodeInfo then
    CollectNodeInfo(True, BlockType, DecreaseLevel);

  if FUncommittedFoldNestCount > 0 then begin
    dec(FUncommittedFoldNestCount);
    if DecreaseLevel then
      dec(FUncommittedFoldStackCount);
  end
  else
    CodeFoldRange.Pop(DecreaseLevel);
end;

procedure TSynCustomFoldHighlighter.CollectNodeInfo(FinishingABlock: Boolean;
  ABlockType: Pointer; LevelChanged: Boolean);
var
  //DecreaseLevel,
  BlockTypeEnabled, BlockConfExists: Boolean;
  act: TSynFoldActions;
  nd: TSynFoldNodeInfo;
begin
  if not IsCollectingNodeInfo then exit;

  BlockConfExists := (PtrUInt(ABlockType) < FoldConfigCount);  // how about pascal that has blocktype > foldconfigcount?
  //BlockConfExists := HasFoldConfig(PtrUInt(ABlockType));
  BlockTypeEnabled := False;
  if BlockConfExists then
    BlockTypeEnabled := FoldConfig[PtrUInt(ABlockType)].Enabled;

  //Start
  if not FinishingABlock then
  begin
    act := [sfaOpen, sfaOpenFold]; // todo deprecate sfaOpenFold
    if BlockTypeEnabled then
      act := act + FoldConfig[PtrUInt(ABlockType)].FoldActions
    else
    if not BlockConfExists then
      act := act + [sfaFold,sfaFoldFold, sfaMarkup, sfaOutline];
  end
  else
  //Finish
  begin
    act := [sfaClose, sfaCloseFold]; // todo deprecate sfaCloseFold
    if BlockTypeEnabled then
      act := act + FoldConfig[PtrUInt(ABlockType)].FoldActions
    else
    if not BlockConfExists then
      act := act + [sfaFold, sfaFoldFold, sfaMarkup, sfaOutline];
    act := act - [sfaFoldFold, sfaFoldHide]; // it is closing tag
  end;

  DoInitNode(nd{%H-}, FinishingABlock, ABlockType, act, LevelChanged);
  FCollectingNodeInfoList.Add(nd);
end;

procedure TSynCustomFoldHighlighter.DoInitNode(var Node: TSynFoldNodeInfo;
  FinishingABlock: Boolean; ABlockType: Pointer;
  aActions: TSynFoldActions; AIsFold: Boolean);
var
  OneLine: Boolean;
  EndOffs: Integer;
  LogX1, LogX2: Integer;

begin
  GetTokenBounds(LogX1, LogX2);

  aActions := aActions + [sfaMultiLine];
  if FinishingABlock then
    EndOffs := -1
  else
    EndOffs := +1;
  Node.LineIndex := LineIndex;
  Node.LogXStart := LogX1;
  Node.LogXEnd := LogX2;
  Node.FoldType := ABlockType;
  Node.FoldTypeCompatible := ABlockType;
  Node.FoldAction := aActions;
  node.FoldGroup := 1;//FOLDGROUP_PASCAL;
  Node.FoldLvlStart := CodeFoldRange.CodeFoldStackSize + FUncommittedFoldStackCount; // If "not AIsFold" then the node has no foldlevel of its own
  Node.NestLvlStart := CodeFoldRange.NestFoldStackSize + FUncommittedFoldNestCount;
  OneLine := FinishingABlock and (Node.FoldLvlStart > CodeFoldRange.MinimumCodeFoldBlockLevel);
  Node.NestLvlEnd := Node.NestLvlStart + EndOffs;
  if not (sfaFold in aActions) then
    EndOffs := 0;
  Node.FoldLvlEnd := Node.FoldLvlStart + EndOffs;
  if OneLine then  // find opening node
    RepairSingleLineNode(Node);
end;

procedure TSynCustomFoldHighlighter.RepairSingleLineNode(var Node: TSynFoldNodeInfo);
var
  nd: PLazEditFoldNodeInfo;
  i : integer;
begin
    i := FCollectingNodeInfoList.CountAll - 1;
    nd := FCollectingNodeInfoList.ItemPointer[i];
    while (i >= 0) and
          ( (nd^.FoldType <> node.FoldType) or
            (nd^.FoldGroup <> node.FoldGroup) or
            (not (sfaOpenFold in nd^.FoldAction))
            or (nd^.FoldLvlEnd <> Node.FoldLvlStart)
          )
    do begin
      dec(i);
      nd := FCollectingNodeInfoList.ItemPointer[i];
    end;
    if i >= 0 then begin
      nd^.FoldAction  := nd^.FoldAction + [sfaOneLineOpen, sfaSingleLine] - [sfaMultiLine];
      Node.FoldAction := Node.FoldAction + [sfaOneLineClose, sfaSingleLine] - [sfaMultiLine];
      if (sfaFoldHide in nd^.FoldAction) then begin
        assert(sfaFold in nd^.FoldAction, 'sfaFoldHide without sfaFold');
        // one liner: hide-able / not fold-able
        nd^.FoldAction  := nd^.FoldAction - [sfaFoldFold];
        Node.FoldAction := Node.FoldAction - [sfaFoldFold];
      end else begin
        // one liner: nether hide-able nore fold-able
        nd^.FoldAction  := nd^.FoldAction - [sfaOpenFold, sfaFold, sfaFoldFold];
        Node.FoldAction := Node.FoldAction - [sfaCloseFold, sfaFold, sfaFoldFold];
      end;
    end;
end;

procedure TSynCustomFoldHighlighter.CreateRootCodeFoldBlock;
begin
  FRootCodeFoldBlock := TSynCustomCodeFoldBlock.Create;
end;

{ TSynCustomCodeFoldBlock }

constructor TSynCustomCodeFoldBlock.Create;
begin
  inherited Create(nil);
end;

procedure TSynCustomCodeFoldBlock.Assign(Src: TLazHighlighterRange);
var
  TheSrc: TSynCustomCodeFoldBlock absolute Src;
begin
  if Src is TSynCustomCodeFoldBlock then begin
    FBlockType := TheSrc.FBlockType;
    FParent := TheSrc.FParent;
  end;
end;

procedure TSynCustomCodeFoldBlock.InitRootBlockType(AType: Pointer);
begin
  if assigned(FParent) then
    raise Exception.Create('Attempt to modify a FoldBlock');
  FBlockType := AType;
end;

{ TSynCustomHighlighterRange }

constructor TSynCustomHighlighterRange.Create(
  Template: TLazHighlighterRange);
begin
  if (Template<>nil) and (ClassType<>Template.ClassType) then
    RaiseGDBException('');
  if Template<>nil then
    Assign(Template);
end;

destructor TSynCustomHighlighterRange.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSynCustomHighlighterRange.Compare(Range: TLazHighlighterRange): integer;
begin
  Result := DoCompare(Range, TSynCustomHighlighterRange.InstanceSize);
end;

function TSynCustomHighlighterRange.Push(ABlock: TSynCustomCodeFoldBlock; IncreaseLevel: Boolean
  ): TSynCustomCodeFoldBlock;
var
  i: LongInt;
begin
  i := MaxFoldLevel;
  if (i > 0) and (FCodeFoldStackSize >= i) then begin
    //debugln('Reached MaxFoldLevel, ignoring folds');
    exit(nil);
  end;
  inc(FNestFoldStackSize);
  if IncreaseLevel then
    inc(FCodeFoldStackSize);
  FTop:=ABlock;
  Result := ABlock;
end;

procedure TSynCustomHighlighterRange.Pop(DecreaseLevel: Boolean = True);
// can be called, even if there is no stack
// because it's normal that sources under development have unclosed blocks
begin
  //debugln('TSynCustomHighlighterRange.Pop');
  if assigned(FTop.Parent) then begin
    FTop := FTop.Parent;
    dec(FNestFoldStackSize);
    if FMinimumNestFoldBlockLevel > FNestFoldStackSize then
      FMinimumNestFoldBlockLevel := FNestFoldStackSize;
    if DecreaseLevel then begin
      dec(FCodeFoldStackSize);
      if FMinimumCodeFoldBlockLevel > FCodeFoldStackSize then
        FMinimumCodeFoldBlockLevel := FCodeFoldStackSize;
    end;
  end;
end;

function TSynCustomHighlighterRange.MaxFoldLevel: Integer;
begin
  Result := -1;
end;

procedure TSynCustomHighlighterRange.Clear;
begin
  FRangeType:=nil;
  FCodeFoldStackSize := 0;
  FNestFoldStackSize := 0;
  FMinimumCodeFoldBlockLevel := 0;
  FMinimumNestFoldBlockLevel:= 0;
  FTop:=nil;
end;

procedure TSynCustomHighlighterRange.Assign(ASrc: TLazHighlighterRange);
var Src: TSynCustomHighlighterRange absolute ASrc;
begin
  if (Src<>nil) and (Src<>TSynCustomHighlighterRange(NullRange)) then begin
    FTop := Src.FTop;
    FCodeFoldStackSize := Src.FCodeFoldStackSize;
    FMinimumCodeFoldBlockLevel := Src.FMinimumCodeFoldBlockLevel;
    FNestFoldStackSize := Src.FNestFoldStackSize;
    FMinimumNestFoldBlockLevel := Src.FMinimumNestFoldBlockLevel;
    FRangeType := Src.FRangeType;
  end
  else begin
    FTop := nil;
    FCodeFoldStackSize := 0;
    FNestFoldStackSize := 0;
    FMinimumCodeFoldBlockLevel := 0;
    FMinimumNestFoldBlockLevel := 0;
    FRangeType := nil;
  end;
end;

procedure TSynCustomHighlighterRange.WriteDebugReport;
begin
  debugln('TSynCustomHighlighterRange.WriteDebugReport ',DbgSName(Self),
    ' RangeType=',dbgs(RangeType),' StackSize=',dbgs(CodeFoldStackSize));
  debugln(' Block=',dbgs(PtrInt(FTop)));
end;

{ TSynCustomHighlighterRangeTree }

constructor TSynCustomHighlighterRangeTree.Create;
begin
  FItems:=TAvlTree.Create(@CompareSynHighlighterRanges);
  inherited Create;
end;

destructor TSynCustomHighlighterRangeTree.Destroy;
begin
  FItems.FreeAndClear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSynCustomHighlighterRangeTree.GetEqual(Range: TLazHighlighterRange): TLazHighlighterRange;
var
  Node: TAvlTreeNode;
begin
  if Range=nil then exit(nil);
  Node:=FItems.Find(Range);
  if Node<>nil then begin
    Result:=TSynCustomHighlighterRange(Node.Data);
  end else begin
    // add a copy
    Result:=TLazHighlighterRangeClass(Range.ClassType).Create(Range);
    FItems.Add(Result);
    //if FItems.Count mod 32 = 0 then debugln(['FOLDRANGE Count=', FItems.Count]);
  end;
  //debugln('TSynCustomHighlighterRangeTree.GetEqual A ',dbgs(Node),' ',dbgs(Result.Compare(Range)),' ',dbgs(Result.CodeFoldStackSize));
end;

{ TSynCustomFoldConfig }

procedure TSynCustomFoldConfig.DoOnChange;
begin
  if assigned(FOnChange) then
    FOnChange(self);
end;

constructor TSynCustomFoldConfig.Create;
begin
  Inherited;
  FIsEssential := True;
end;

constructor TSynCustomFoldConfig.Create(ASupportedModes: TSynCustomFoldConfigModes; AnIsEssential: Boolean);
begin
  inherited Create(ASupportedModes);
  FSupportedModes := ASupportedModes;
  FIsEssential := AnIsEssential;
end;


end.


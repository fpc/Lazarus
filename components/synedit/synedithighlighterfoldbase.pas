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
  SysUtils, Classes, math, Laz_AVL_Tree,
  // LCL
  LCLProc,
  // LazUtils
  LazClasses, LazLoggerBase,
  // SynEdit
  SynEditHighlighter, SynEditTypes, LazSynEditText;

const
  NullRange = TSynEditRange(nil);

type

  TSynFoldAction = ( sfaOpen,         // Any Opening node
                     sfaClose,        // Any Closing node

                     sfaFold,         // Part of a fold- or hide-able block (FoldConf.Enabled = True)           - excludes one=liners for FoldFold, as they can not fold
                     sfaFoldFold,     // Part of a fold-able block (FoldConf.Enabled = True / smFold in Modes)  - excludes one=liners / only opening node, except ifdef/region (todo: maybe both?)
                     sfaFoldHide,     // Part of a hide-able block (FoldConf.Enabled = True / smHide in Modes)  - includes one=liners / only opening node, except ifdef/region (todo: maybe both?)

                     sfaMultiLine,    // The closing node is on an other line
                     sfaSingleLine,   // The closing node is on the same line (though the keyword may be on the next)
                     // //sfaSingleLineClosedByNext
                     sfaCloseForNextLine,  // Fold closes this line, but keyword is on the next (e.g. "var" block)
                     sfaLastLineClose,     // Fold is incomplete, and closed at last line of file
                     sfaCloseAndOpen,    // This node has the same location/type as the neighbouring opposite node.
                                         // eg an open node, matche exactly the previous node, which has to be a closing node of the same type and location (and vice versa for a closing node matching the next...)

                     sfaDefaultCollapsed,
                     sfaMarkup,   // This node can be highlighted, by the matching Word-Pair Markup
                     sfaOutline,  // This node will be higlighted by nested color replacing the token color
                     sfaOutlineKeepLevel, // Direct children should not increase color dept. (But grandchild can.)  e.g. "if","then" any "procedure"
                     sfaOutlineMergeParent,// This node want to decrease current color depth. (But Previous sibling increased) e.g. "except", "finally"
                     sfaOutlineForceIndent, // Node will temporary ignore sfaOutlineKeep. (Next sibling can.) e.g in NESTED "procedure"
                     sfaOutlineNoColor,     // Node will not painted by nested-coloring, but may increase color (e.g. any "procedure")
                     sfaOutlineNoLine,      // Node doesn't want to have vertical line. (e.g. "then")
                     sfaInvalid,  // Wrong Index

                     // TODO: deprecate
                     sfaOpenFold,     // At this node a new Fold can start // Actually, includes all,any multiline node too.
                     sfaCloseFold,    // At this node a fold ends
                     sfaOneLineOpen,   // Open, but closes on same line; *only* if hide-able has [sfaOpenFold, sfaFold]; always has [sfaFoldFold, sfaFoldHide]
                     sfaOneLineClose  // Open, but closes on same line;
                   );
  TSynFoldActions = set of TSynFoldAction;

  (* TSynFoldBlockFilter
     used to specify which folds to include for:
     - FoldOpenCount, FoldCloseCount, FoldNestCount
     - maybe in future TLazSynFoldNodeInfoList
       TLazSynFoldNodeInfoList has additional filters
       TLazSynFoldNodeInfoList always uses the full set (sfbIncludeDisabled)

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

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter;
                              AFoldGroup: Integer = 0; AFlag: TSynFoldBlockFilterFlags = []);

type
  TSynCustomFoldHighlighter = class;

  TSynFoldNodeInfo = record
    LineIndex: Integer;
    NodeIndex: Integer;          // Indicates the position within the list of info nodes (depends on search-Filter)
    AllNodeIndex: Integer;       // Indicates the position within the unfiltered list of info nodes
    LogXStart, LogXEnd: Integer; // -1 previous line ( 0-based)
    FoldLvlStart, FoldLvlEnd: Integer; // FoldLvl within each FoldGroup
    NestLvlStart, NestLvlEnd: Integer; // include disabled nodes, e.g markup (within each FoldGroup)
    FoldAction: TSynFoldActions;
    FoldType: Pointer;           // e.g.cfbtBeginEnd, cfbtProcedure ...
    FoldTypeCompatible: Pointer; // map outer and inner begin, and other exchangeable types
    FoldGroup: Integer;          // independend/overlapping folds, e.g begin/end; ifdef, region
  end;
  PSynFoldNodeInfo = ^TSynFoldNodeInfo;

  { TLazSynFoldNodeInfoList }

  TLazSynFoldNodeInfoList = class(TRefCountedObject)
  private
    FHighLighter: TSynCustomFoldHighlighter;
    FValid: Boolean;
    FActionFilter: TSynFoldActions;
    FGroupFilter: Integer;
    FLine: TLineIdx;
    FNodeCount: Integer;
    FFilteredCount, FFilteredProgress: Integer;
    FNodeInfoList: Array of TSynFoldNodeInfo;
    FFilteredList: Array of TSynFoldNodeInfo;
    function  GetItem(Index: Integer): TSynFoldNodeInfo;
    procedure SetActionFilter(AValue: TSynFoldActions);
    procedure SetGroupFilter(AValue: Integer);
    function  GetItemPointer(AnIndex: Integer): PSynFoldNodeInfo;
    function  GetLastItemPointer: PSynFoldNodeInfo;
  protected
    procedure Invalidate;
    procedure Clear;
    procedure ClearData;
    procedure ClearFilteredList;
    procedure DoFilter(MinIndex: Integer = -1);
    procedure SetLine(ALine: TLineIdx); // Does not clear anything, if line has not changed.
    procedure SetLineClean(ALine: TLineIdx); // Does not clear anything, if line has not changed.
    property  HighLighter: TSynCustomFoldHighlighter read FHighLighter write FHighLighter;
  public
    // used by HighLighters to add data
    procedure Add(const AnInfo: TSynFoldNodeInfo);
    procedure Delete(AnIndex: Integer = -1);
    function  CountAll: Integer;
    property  ItemPointer[AnIndex: Integer]: PSynFoldNodeInfo read GetItemPointer;
    property  LastItemPointer: PSynFoldNodeInfo read GetLastItemPointer;
  protected
    function  DefaultGroup: Integer; virtual;
    function  MinCapacity: Integer; virtual;
    procedure InvalidateNode(out AnInfo: TSynFoldNodeInfo);
    function  Match(const AnInfo: TSynFoldNodeInfo;
                    AnActionFilter: TSynFoldActions; AGroupFilter: Integer = 0): Boolean; virtual;
  public
    // filtered items
    procedure ClearFilter;
    function Count: Integer;
    property Item[Index: Integer]: TSynFoldNodeInfo read GetItem; default;
    property ActionFilter: TSynFoldActions read FActionFilter write SetActionFilter;
    property GroupFilter: Integer read FGroupFilter write SetGroupFilter;
  public
    // all items / filtered on the fly
    function CountEx   (AnActionFilter: TSynFoldActions; AGroupFilter: Integer = 0): Integer;
    function NodeInfoEx(Index: Integer; AnActionFilter: TSynFoldActions; AGroupFilter: Integer = 0): TSynFoldNodeInfo; virtual;
  public
    // Only allowed to be set, if highlighter has CurrentLines (and is scanned)
    property Line: TLineIdx read FLine write SetLine;
  end;

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

    FFoldNodeInfoList: TLazSynFoldNodeInfoList;
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

  TSynCustomFoldConfigMode = (fmFold, fmHide, fmMarkup, fmOutline);
  TSynCustomFoldConfigModes = set of TSynCustomFoldConfigMode;

  { TSynCustomFoldConfig }

  TSynCustomFoldConfig = class(TPersistent)
  private
    FEnabled: Boolean;
    FFoldActions: TSynFoldActions;
    FIsEssential: boolean;
    FModes: TSynCustomFoldConfigModes;
    FOnChange: TNotifyEvent;
    FSupportedModes: TSynCustomFoldConfigModes;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetModes(AValue: TSynCustomFoldConfigModes);
    procedure SetSupportedModes(AValue: TSynCustomFoldConfigModes); deprecated 'use create';
  protected
    procedure DoOnChange;
  public
    constructor Create;
    constructor Create(ASupportedModes: TSynCustomFoldConfigModes; AnIsEssential: Boolean = False);
    procedure Assign(Src: TSynCustomFoldConfig); reintroduce; virtual; // TODO: do not copy supported modes
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property IsEssential: boolean read FIsEssential;   // create node, even if disabled
    property SupportedModes: TSynCustomFoldConfigModes
             read FSupportedModes write SetSupportedModes;
    // Actions representing the modes
    property FoldActions: TSynFoldActions read FFoldActions;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Modes: TSynCustomFoldConfigModes read FModes write SetModes default [fmFold];
  end;

  { TSynCustomCodeFoldBlock }

  TSynCustomCodeFoldBlock = class
  private
    FBlockType: Pointer;
    FParent, FChildren: TSynCustomCodeFoldBlock;
    FRight, FLeft: TSynCustomCodeFoldBlock;
    FBalance: Integer;
    function GetChild(ABlockType: Pointer): TSynCustomCodeFoldBlock;
  protected
    function GetOrCreateSibling(ABlockType: Pointer): TSynCustomCodeFoldBlock;
    property Right: TSynCustomCodeFoldBlock read FRight;
    property Left: TSynCustomCodeFoldBlock read FLeft;
    property Children: TSynCustomCodeFoldBlock read FChildren;
  public
    destructor Destroy; override;
    procedure WriteDebugReport;
  public
    procedure InitRootBlockType(AType: Pointer);
    property BlockType: Pointer read FBlockType;
    property Parent: TSynCustomCodeFoldBlock read FParent;
    property Child[ABlockType: Pointer]: TSynCustomCodeFoldBlock read GetChild;
  end;

  { TSynCustomHighlighterRange }

  TSynCustomHighlighterRange = class
  private
    // TODO: either reduce to one level, or create subclass for 2nd level
    FCodeFoldStackSize: integer; // EndLevel
    FNestFoldStackSize: integer; // EndLevel
    FMinimumCodeFoldBlockLevel: integer;
    FMinimumNestFoldBlockLevel: integer;
    FRangeType: Pointer;
    FTop: TSynCustomCodeFoldBlock;
  public
    constructor Create(Template: TSynCustomHighlighterRange); virtual;
    destructor Destroy; override;
    function Compare(Range: TSynCustomHighlighterRange): integer; virtual;
    function Add(ABlockType: Pointer = nil; IncreaseLevel: Boolean = True):
        TSynCustomCodeFoldBlock; virtual;
    procedure Pop(DecreaseLevel: Boolean = True); virtual;
    function MaxFoldLevel: Integer; virtual;
    procedure Clear; virtual;
    procedure Assign(Src: TSynCustomHighlighterRange); virtual;
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
  TSynCustomHighlighterRangeClass = class of TSynCustomHighlighterRange;

  TSynCustomHighlighterRanges = class;

  { TSynCustomFoldHighlighter }

  TSynCustomFoldHighlighter = class(TSynCustomHighlighter)
  protected
    // Fold Config
    FFoldConfig: Array of TSynCustomFoldConfig;
    function GetFoldConfig(Index: Integer): TSynCustomFoldConfig; virtual;
    procedure SetFoldConfig(Index: Integer; const AValue: TSynCustomFoldConfig); virtual;
    function GetFoldConfigCount: Integer; virtual;
    function GetFoldConfigInternalCount: Integer; virtual;
    function CreateFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; virtual;
    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; virtual;
    procedure InitFoldConfig;
    procedure DestroyFoldConfig;
    procedure DoFoldConfigChanged(Sender: TObject); virtual;
  private
    FCodeFoldRange: TSynCustomHighlighterRange;
    FIsCollectingNodeInfo: boolean;
    fRanges: TSynCustomHighlighterRanges;
    FRootCodeFoldBlock: TSynCustomCodeFoldBlock;
    FFoldNodeInfoList: TLazSynFoldNodeInfoList;
    FCollectingNodeInfoList: TLazSynFoldNodeInfoList;
    procedure ClearFoldNodeList;
  protected
    // "Range"
    function GetRangeClass: TSynCustomHighlighterRangeClass; virtual;
    procedure CreateRootCodeFoldBlock; virtual; // set RootCodeFoldBlock
    property CodeFoldRange: TSynCustomHighlighterRange read FCodeFoldRange;
    function TopCodeFoldBlockType(DownIndex: Integer = 0): Pointer;
    property RootCodeFoldBlock: TSynCustomCodeFoldBlock read FRootCodeFoldBlock
      write FRootCodeFoldBlock;

    // Open/Close Folds
    function StartCodeFoldBlock(ABlockType: Pointer = nil;
              IncreaseLevel: Boolean = true; ForceDisabled: Boolean = False): TSynCustomCodeFoldBlock; virtual;
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
    function CreateFoldNodeInfoList: TLazSynFoldNodeInfoList; virtual;
    function GetFoldNodeInfo(Line: TLineIdx): TLazSynFoldNodeInfoList;
    procedure ScanFoldNodeInfo(); virtual;
    procedure InitFoldNodeInfo(AList: TLazSynFoldNodeInfoList; Line: TLineIdx);

    // Info about Folds, on currently set line/range (simply forwarding to range
    function MinimumCodeFoldBlockLevel: integer; virtual;
    function CurrentCodeFoldBlockLevel: integer; virtual;

    property IsCollectingNodeInfo : boolean read FIsCollectingNodeInfo;
    property CollectingNodeInfoList : TLazSynFoldNodeInfoList read FCollectingNodeInfoList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    function GetRange: Pointer; override;

    // Info about Folds
    function FoldBlockOpeningCount(ALineIndex: TLineIdx;
                                   const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockClosingCount(ALineIndex: TLineIdx;
                                   const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockEndLevel(ALineIndex: TLineIdx;
                               const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx;
                               const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    (* All nested FoldType (cfbtBegin) if available. Similar to TopCodeFoldBlockType
       - Index=0 is most outer / Index=FoldBlockEndLevel is most inner (TopCodeFoldBlockType 0=inner)
       - False, if it can not be determined for the filter settings
    *)
    function FoldBlockNestedTypes(ALineIndex: TLineIdx; ANestIndex: Integer; out AType: Pointer;
                                  const AFilter: TSynFoldBlockFilter): boolean; virtual; overload;

    function FoldBlockOpeningCount(ALineIndex: TLineIdx; AFoldGroup: integer = 0;
                                   AFlags: TSynFoldBlockFilterFlags = []): integer; overload;
    function FoldBlockClosingCount(ALineIndex: TLineIdx; AFoldGroup: integer = 0;
                                   AFlags: TSynFoldBlockFilterFlags = []): integer; overload;
    function FoldBlockEndLevel(ALineIndex: TLineIdx; AFoldGroup: integer = 0;
                               AFlags: TSynFoldBlockFilterFlags = []): integer; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx; AFoldGroup: integer = 0;
                               AFlags: TSynFoldBlockFilterFlags = []): integer; overload;
    function FoldBlockNestedTypes(ALineIndex: TLineIdx; ANestIndex: Integer; out AType: Pointer;
                                  AFoldGroup: integer = 0;
                                  AFlags: TSynFoldBlockFilterFlags = []): boolean; virtual; overload;

    function FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer;  deprecated;
    function FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer; deprecated;
    function FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer; deprecated;

    function FoldTypeCount: integer; virtual;
    function FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
             UseCloseNodes: boolean = false): integer; virtual; // TODO: could be deprecated ./ only child-classes

    function FindNextLineWithMinFoldLevel(ALineIndex: TLineIdx; ASearchLevel: Integer;
                               const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FindNextLineWithMinFoldLevel(ALineIndex: TLineIdx; ASearchLevel: Integer;
                               AFoldGroup: integer = 0; AFlags: TSynFoldBlockFilterFlags = []): integer; overload;

    function FoldEndLine(ALineIndex, FoldIndex: Integer): integer; virtual; overload; // deprecate // fix inherited classes
    function FoldEndLine(ALineIndex, FoldIndex: Integer;
                         const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldEndLine(ALineIndex, FoldIndex: Integer;
                         AFoldGroup: integer; AFlags: TSynFoldBlockFilterFlags): integer; overload;
//                         AFoldGroup: integer = 0; AFlags: TSynFoldBlockFilterFlags = []): integer; overload;

    function FoldLineLength(ALineIndex, FoldIndex: Integer): integer; virtual;  // only for group 0 // may be one less than FoldEndLine, if end line is a mixed end-begin

    // All fold-nodes
    // FoldNodeInfo: Returns a shared object
    // Adding RefCount, will prevent others from getting further copies, but not from using copies they already have.
    // If not adding refcount, the object should not be stored/re-used
    // Not adding ref-count, should only be done for CountEx, NodeInfoEx
    property FoldNodeInfo[Line: TLineIdx]: TLazSynFoldNodeInfoList read GetFoldNodeInfo;

    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure SetLine(const NewValue: String;
                      LineNumber:Integer // 0 based
                      ); override;
    procedure DoCurrentLinesChanged; override;
    function PerformScan(StartIndex, EndIndex: Integer; ForceEndIndex: Boolean =
      False): Integer; override;
  public
    property FoldConfig[Index: Integer]: TSynCustomFoldConfig
      read GetFoldConfig write SetFoldConfig;
    property FoldConfigCount: Integer read GetFoldConfigCount;

  end;

  { TSynCustomHighlighterRanges }

  TSynCustomHighlighterRanges = class
  private
    FAllocatedCount: integer;
    FHighlighterClass: TSynCustomHighlighterClass;
    FItems: TAvlTree;
  public
    constructor Create(TheHighlighterClass: TSynCustomHighlighterClass);
    destructor Destroy; override;
    function GetEqual(Range: TSynCustomHighlighterRange
                      ): TSynCustomHighlighterRange;
    procedure Allocate;
    procedure Release;
    property HighlighterClass: TSynCustomHighlighterClass read FHighlighterClass;
    property AllocatedCount: integer read FAllocatedCount;
  end;

function CompareSynHighlighterRanges(Data1, Data2: Pointer): integer;
function AllocateHighlighterRanges(
     HighlighterClass: TSynCustomHighlighterClass): TSynCustomHighlighterRanges;

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

var
  HighlighterRanges: TFPList = nil;

function IndexOfHighlighterRanges(
  HighlighterClass: TSynCustomHighlighterClass): integer;
begin
  if HighlighterRanges=nil then
    Result:=-1
  else begin
    Result:=HighlighterRanges.Count-1;
    while (Result>=0)
    and (TSynCustomHighlighterRanges(HighlighterRanges[Result]).HighlighterClass
      <>HighlighterClass)
    do
      dec(Result);
  end;
end;

function AllocateHighlighterRanges(
  HighlighterClass: TSynCustomHighlighterClass): TSynCustomHighlighterRanges;
var
  i: LongInt;
begin
  if HighlighterRanges=nil then HighlighterRanges:=TFPList.Create;
  i:=IndexOfHighlighterRanges(HighlighterClass);
  if i>=0 then begin
    Result:=TSynCustomHighlighterRanges(HighlighterRanges[i]);
    Result.Allocate;
  end else begin
    Result:=TSynCustomHighlighterRanges.Create(HighlighterClass);
    HighlighterRanges.Add(Result);
  end;
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

{ TLazSynFoldNodeInfoList }

function TLazSynFoldNodeInfoList.GetItem(Index: Integer): TSynFoldNodeInfo;
begin
  DoFilter(Index);
  if (Index >= FFilteredCount) or (Index < 0) or (not FValid) then
    InvalidateNode(Result)
  else begin
    Result := FFilteredList[Index];
    Result.NodeIndex := Index; // only set copy on result
  end;
end;

procedure TLazSynFoldNodeInfoList.SetActionFilter(AValue: TSynFoldActions);
begin
  if FActionFilter=AValue then Exit;
  FActionFilter:=AValue;
  ClearFilteredList;
end;

procedure TLazSynFoldNodeInfoList.SetGroupFilter(AValue: Integer);
begin
  if FGroupFilter=AValue then Exit;
  FGroupFilter:=AValue;
  ClearFilteredList;
end;

procedure TLazSynFoldNodeInfoList.Clear;
begin
  ClearFilter;
  ClearData;
end;

procedure TLazSynFoldNodeInfoList.ClearData;
var
  c: Integer;
begin
  FValid := True;
  ClearFilteredList;
  FLine := -1;
  c := MinCapacity;
  FNodeCount := 0;
  if Length(FNodeInfoList) > c then
    SetLength(FNodeInfoList, c);
end;

procedure TLazSynFoldNodeInfoList.ClearFilteredList;
begin
  SetLength(FFilteredList, 0);
  FFilteredCount := 0;
  FFilteredProgress := 0; // next to be filtered
end;

procedure TLazSynFoldNodeInfoList.ClearFilter;
begin
  ClearFilteredList;
  FGroupFilter := 0;
  FActionFilter := [];
end;

procedure TLazSynFoldNodeInfoList.DoFilter(MinIndex: Integer = -1);
begin
  if FFilteredProgress = FNodeCount then exit;
  if (MinIndex >= 0) and (FFilteredCount > MinIndex) or (not FValid) then exit;

  if (FActionFilter = []) and (FGroupFilter = DefaultGroup) then begin
    FFilteredList := FNodeInfoList;
    FFilteredCount := FNodeCount;
    FFilteredProgress := FNodeCount;
    exit;
  end;

  if Length(FFilteredList) < Length(FNodeInfoList) then
    SetLength(FFilteredList, Length(FNodeInfoList));

  while FFilteredProgress < FNodeCount do begin
    if Match(FNodeInfoList[FFilteredProgress], FActionFilter, FGroupFilter)
    then begin
      FFilteredList[FFilteredCount] := FNodeInfoList[FFilteredProgress];
      inc(FFilteredCount);
    end;
    inc(FFilteredProgress);
    if (MinIndex >= 0) and (FFilteredCount > MinIndex) then break;
  end;
end;

procedure TLazSynFoldNodeInfoList.SetLine(ALine: TLineIdx);
begin
  if (FLine = ALine) or (ALine < 0) then exit;
  ClearData;
  FLine := ALine;
  FHighLighter.InitFoldNodeInfo(Self, FLine);
end;

procedure TLazSynFoldNodeInfoList.SetLineClean(ALine: TLineIdx);
begin
  if (FLine = ALine) or (ALine < 0) then exit;
  Clear;
  FLine := ALine;
  FHighLighter.InitFoldNodeInfo(Self, FLine);
end;

function TLazSynFoldNodeInfoList.MinCapacity: Integer;
begin
  Result := 8;
end;

procedure TLazSynFoldNodeInfoList.InvalidateNode(out AnInfo: TSynFoldNodeInfo);
begin
  AnInfo.FoldAction := [sfaInvalid];
  AnInfo.LineIndex := Line;
  AnInfo.NodeIndex := -1;
end;

procedure TLazSynFoldNodeInfoList.Add(const AnInfo: TSynFoldNodeInfo);
var
  c: Integer;
begin
  if FNodeCount >= Length(FNodeInfoList) - 1 then begin
    c := MinCapacity;
    if c <= 0 then c := 8;
    SetLength(FNodeInfoList, Max(Length(FNodeInfoList) * 2, c));
  end;
  FNodeInfoList[FNodeCount] := AnInfo;
  FNodeInfoList[FNodeCount].AllNodeIndex := FNodeCount;
  If (FNodeCount > 0) and (sfaOpen in AnInfo.FoldAction) then begin
    c := FNodeCount-1;
    if (sfaClose in FNodeInfoList[c].FoldAction) and
       //(AnInfo.FoldType = FNodeInfoList[c].FoldType) and  // cfbtIfDef <> cfbtIfElse
       (AnInfo.LogXStart = FNodeInfoList[c].LogXStart) and
       (AnInfo.LogXEnd = FNodeInfoList[c].LogXEnd)
    then begin
      include(FNodeInfoList[FNodeCount].FoldAction, sfaCloseAndOpen);
      include(FNodeInfoList[c].FoldAction, sfaCloseAndOpen);
    end;
  end;
  inc(FNodeCount);
end;

procedure TLazSynFoldNodeInfoList.Delete(AnIndex: Integer = -1);
begin
  if AnIndex > 0 then begin
    while (AnIndex < FNodeCount) do begin
      FNodeInfoList[AnIndex] := FNodeInfoList[AnIndex + 1];
      FNodeInfoList[AnIndex].AllNodeIndex := AnIndex;
    inc(AnIndex);
    end;
  end;
  if FNodeCount > 0 then
    dec(FNodeCount);
end;

function TLazSynFoldNodeInfoList.CountAll: Integer;
begin
  if FValid then
    Result := FNodeCount
  else
    Result := -1;
end;

function TLazSynFoldNodeInfoList.GetItemPointer(AnIndex: Integer
  ): PSynFoldNodeInfo;
begin
  if (AnIndex >= FNodeCount) or (AnIndex < 0) then
    Result := nil
  else
    Result := @FNodeInfoList[AnIndex];
end;

function TLazSynFoldNodeInfoList.GetLastItemPointer: PSynFoldNodeInfo;
begin
  if FNodeCount < 0 then
    Result := nil
  else
    Result := @FNodeInfoList[FNodeCount-1];
end;

procedure TLazSynFoldNodeInfoList.Invalidate;
begin
  Clear;
  FValid := False;
end;

function TLazSynFoldNodeInfoList.Match(const AnInfo: TSynFoldNodeInfo;
  AnActionFilter: TSynFoldActions; AGroupFilter: Integer): Boolean;
begin
  Result := (AnInfo.FoldAction * AnActionFilter = AnActionFilter) and
            ( (AGroupFilter = 0) or (AnInfo.FoldGroup = AGroupFilter) );
end;

function TLazSynFoldNodeInfoList.DefaultGroup: Integer;
begin
  Result := 0;
end;

function TLazSynFoldNodeInfoList.Count: Integer;
begin
  if not FValid then exit(-1);

  DoFilter(-1);
  Result := FFilteredCount;
end;

function TLazSynFoldNodeInfoList.CountEx(AnActionFilter: TSynFoldActions;
  AGroupFilter: Integer): Integer;
var
  i: Integer;
begin
  if not FValid then exit(-1);
  if (AnActionFilter = []) and (AGroupFilter = DefaultGroup) then begin
    Result := FNodeCount;
    exit;
  end;

  Result := 0;
  for i := 0 to FNodeCount - 1 do
    if Match(FNodeInfoList[i], AnActionFilter, AGroupFilter) then inc(Result);
end;

function TLazSynFoldNodeInfoList.NodeInfoEx(Index: Integer;
  AnActionFilter: TSynFoldActions; AGroupFilter: Integer): TSynFoldNodeInfo;
var
  i, j: Integer;
begin
  if (Index < 0) or (not FValid) then begin
    InvalidateNode(Result);
    exit;
  end;

  if (AnActionFilter = []) and (AGroupFilter = DefaultGroup) then begin
    if (Index >= FNodeCount) then
      InvalidateNode(Result)
    else
      Result := FNodeInfoList[Index];
    Result.NodeIndex := Index; // only set copy on result
    exit;
  end;

  i := 0;
  j := Index;
  while i < FNodeCount do begin
    if Match(FNodeInfoList[i], AnActionFilter, AGroupFilter) then dec(j);
    if j < 0 then begin;
      Result := FNodeInfoList[i];
      Result.NodeIndex := Index; // only set copy on result
      exit;
    end;
    inc(i);
  end;

  InvalidateNode(Result);
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
    SetLength(OpenCnt, FGroupCount);
    for Grp := 0 to FGroupCount - 1 do
      OpenCnt[Grp] := 0;
    ListCnt := FFoldNodeInfoList.Count;
    if ListCnt < 0 then
      exit;
    SetLength(OpenIdx, FGroupCount, ListCnt);

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
  fRanges:=AllocateHighlighterRanges(TSynCustomHighlighterClass(ClassType));
  CreateRootCodeFoldBlock;
  inherited Create(AOwner);
  FCodeFoldRange:=GetRangeClass.Create(nil);
  FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
  FFoldNodeInfoList := nil;;
end;

destructor TSynCustomFoldHighlighter.Destroy;
begin
  inherited Destroy;
  DestroyFoldConfig;
  FreeAndNil(FCodeFoldRange);
  FreeAndNil(FRootCodeFoldBlock);
  ReleaseRefAndNil(FFoldNodeInfoList);
  fRanges.Release;
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
  Result:=fRanges.GetEqual(FCodeFoldRange);
end;

function TSynCustomFoldHighlighter.FoldBlockOpeningCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
{$IFDEF ISSUE_20850}
var x : integer;
{$ENDIF}
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  {$IFDEF ISSUE_20850}
  x      := FoldBlockEndLevel(ALineIndex, AFilter);
  Result := FoldBlockMinLevel(ALineIndex, AFilter);
  Result := x - Result;
  {$ELSE}
  Result := FoldBlockEndLevel(ALineIndex, AFilter) - FoldBlockMinLevel(ALineIndex, AFilter);
  {$ENDIF}
end;

function TSynCustomFoldHighlighter.FoldBlockClosingCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
{$IFDEF ISSUE_20850}
var x : integer;
{$ENDIF}
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  {$IFDEF ISSUE_20850}
  x      := FoldBlockEndLevel(ALineIndex - 1, AFilter);
  Result := FoldBlockMinLevel(ALineIndex, AFilter);
  Result := x - Result;
  {$ELSE}
  Result := FoldBlockEndLevel(ALineIndex - 1, AFilter) - FoldBlockMinLevel(ALineIndex, AFilter);
  {$ENDIF}
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
  if (r <> nil) and (r <> NullRange) then
    Result := TSynCustomHighlighterRange(r).CodeFoldStackSize
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
  if (r <> nil) and (r <> NullRange) then
    Result := TSynCustomHighlighterRange(r).MinimumCodeFoldBlockLevel
  else
    Result:=0;
end;

function TSynCustomFoldHighlighter.FoldBlockNestedTypes(ALineIndex: TLineIdx;
  ANestIndex: Integer; out AType: Pointer; const AFilter: TSynFoldBlockFilter): boolean;
begin
  Result := False;
end;

function TSynCustomFoldHighlighter.FoldBlockOpeningCount(ALineIndex: TLineIdx;
  AFoldGroup: integer; AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockOpeningCount(ALineIndex, Filter);
end;

function TSynCustomFoldHighlighter.FoldBlockClosingCount(ALineIndex: TLineIdx;
  AFoldGroup: integer; AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockClosingCount(ALineIndex, Filter);
end;

function TSynCustomFoldHighlighter.FoldBlockEndLevel(ALineIndex: TLineIdx;
  AFoldGroup: integer; AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockEndLevel(ALineIndex, Filter);
end;

function TSynCustomFoldHighlighter.FoldBlockMinLevel(ALineIndex: TLineIdx;
  AFoldGroup: integer; AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockMinLevel(ALineIndex, Filter);
end;

function TSynCustomFoldHighlighter.FoldBlockNestedTypes(ALineIndex: TLineIdx;
  ANestIndex: Integer; out AType: Pointer; AFoldGroup: integer;
  AFlags: TSynFoldBlockFilterFlags): boolean;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockNestedTypes(ALineIndex, ANestIndex, AType, Filter);
end;

procedure TSynCustomFoldHighlighter.ResetRange;
begin
  FCodeFoldRange.Clear;
  FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
end;

function TSynCustomFoldHighlighter.MinimumCodeFoldBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'MinimumCodeFoldBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.MinimumCodeFoldBlockLevel;
end;

procedure TSynCustomFoldHighlighter.SetRange(Value: Pointer);
begin
  FCodeFoldRange.Assign(TSynCustomHighlighterRange(Value));
  // in case we asigned a null range
  if not assigned(FCodeFoldRange.FoldRoot) then
    FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
end;

procedure TSynCustomFoldHighlighter.SetLine(const NewValue: String;
  LineNumber: Integer);
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

function TSynCustomFoldHighlighter.PerformScan(StartIndex, EndIndex: Integer;
  ForceEndIndex: Boolean): Integer;
begin
  ClearFoldNodeList;
  Result := inherited PerformScan(StartIndex, EndIndex, ForceEndIndex);
end;

function TSynCustomFoldHighlighter.CurrentCodeFoldBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'MinimumCodeFoldBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.CodeFoldStackSize;
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

function TSynCustomFoldHighlighter.FoldTypeCount: integer;
begin
  Result := 1;
end;

function TSynCustomFoldHighlighter.FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
  UseCloseNodes: boolean): integer;
begin
  Result := 0;
end;

function TSynCustomFoldHighlighter.FindNextLineWithMinFoldLevel(
  ALineIndex: TLineIdx; ASearchLevel: Integer;
  const AFilter: TSynFoldBlockFilter): integer;
var
  cnt: Integer;
begin
  cnt := CurrentLines.Count;
  Result := ALineIndex; // Can return the original line
  while (Result < cnt) and (FoldBlockMinLevel(Result, AFilter) > ASearchLevel) do inc(Result);
  if (Result = cnt) then
    dec(Result);
end;

function TSynCustomFoldHighlighter.FindNextLineWithMinFoldLevel(
  ALineIndex: TLineIdx; ASearchLevel: Integer; AFoldGroup: integer;
  AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FindNextLineWithMinFoldLevel(ALineIndex, ASearchLevel, Filter);
end;

function TSynCustomFoldHighlighter.FoldLineLength(ALineIndex, FoldIndex: Integer): integer;
begin
  Result := FoldEndLine(ALineIndex, FoldIndex);
  // check if fold last line of block (not mixed "end begin")
  if (FoldBlockEndLevel(Result) > FoldBlockMinLevel(Result)) then
    dec(Result);
  // Amount of lines, that will become invisible (excludes the cfCollapsed line)
  Result := Result - ALineIndex;
end;

function TSynCustomFoldHighlighter.FoldEndLine(ALineIndex, FoldIndex: Integer): integer;
begin
  Result := FoldEndLine(ALineIndex, FoldIndex, 0, []);
end;

function TSynCustomFoldHighlighter.FoldEndLine(ALineIndex, FoldIndex: Integer;
  const AFilter: TSynFoldBlockFilter): integer;
var
  lvl: Integer;
  e, m: Integer;
begin
  e := FoldBlockEndLevel(ALineIndex, AFilter);
  m := FoldBlockMinLevel(ALineIndex, AFilter);
  lvl := Min(m+1+FoldIndex, e);
  Result := FindNextLineWithMinFoldLevel(ALineIndex+1, lvl-1, AFilter);
end;

function TSynCustomFoldHighlighter.FoldEndLine(ALineIndex, FoldIndex: Integer;
  AFoldGroup: integer; AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldEndLine(ALineIndex, FoldIndex, Filter);
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
  Result.OnChange := @DoFoldConfigChanged;
  Result.Enabled := False;
end;

procedure TSynCustomFoldHighlighter.InitFoldConfig;
var
  i: Integer;
begin
  for i := 0 to high(FFoldConfig) do
    FFoldConfig[i] := GetFoldConfigInstance(i);
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
  FAttributeChangeNeedScan := True;
  DefHighlightChange(self);
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

function TSynCustomFoldHighlighter.GetFoldNodeInfo(Line: TLineIdx
  ): TLazSynFoldNodeInfoList;
begin
  if (FFoldNodeInfoList <> nil) and (FFoldNodeInfoList.RefCount > 1) then
    ReleaseRefAndNil(FFoldNodeInfoList);

  if FFoldNodeInfoList = nil then begin
    FFoldNodeInfoList := CreateFoldNodeInfoList;
    FFoldNodeInfoList.AddReference;
    FFoldNodeInfoList.HighLighter := Self;
  end
  else
  if (CurrentRanges <> nil) and (CurrentRanges.NeedsReScanStartIndex >= 0) then
    ClearFoldNodeList;


  Result := FFoldNodeInfoList;
  Result.SetLineClean(Line);
end;

procedure TSynCustomFoldHighlighter.ScanFoldNodeInfo;
begin
  NextToEol;
end;

procedure TSynCustomFoldHighlighter.InitFoldNodeInfo(AList: TLazSynFoldNodeInfoList; Line: TLineIdx);
begin
  FIsCollectingNodeInfo := True;
  try
    FCollectingNodeInfoList := TLazSynFoldNodeInfoList(AList);
    StartAtLineIndex(Line);
    ScanFoldNodeInfo();
  finally
    FIsCollectingNodeInfo := False;
  end;
end;

function TSynCustomFoldHighlighter.CreateFoldNodeInfoList: TLazSynFoldNodeInfoList;
begin
  Result := TLazSynFoldNodeInfoList.Create;
end;

function TSynCustomFoldHighlighter.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
  Result:=TSynCustomHighlighterRange;
end;

function TSynCustomFoldHighlighter.TopCodeFoldBlockType(DownIndex: Integer = 0): Pointer;
var
  Fold: TSynCustomCodeFoldBlock;
begin
  Result:=nil;
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

function TSynCustomFoldHighlighter.StartCodeFoldBlock(ABlockType: Pointer;
  IncreaseLevel: Boolean; ForceDisabled: Boolean): TSynCustomCodeFoldBlock;
begin
  if (PtrUInt(ABlockType) < FoldConfigCount) and (not ForceDisabled) and
     (not FoldConfig[PtrUInt(ABlockType)].Enabled) and
     (not FoldConfig[PtrUInt(ABlockType)].IsEssential)
  then
    exit(nil);

  if FIsCollectingNodeInfo then
    CollectNodeInfo(False, ABlockType, IncreaseLevel);

  Result:=CodeFoldRange.Add(ABlockType, IncreaseLevel);
end;

procedure TSynCustomFoldHighlighter.EndCodeFoldBlock(DecreaseLevel: Boolean = True);
var
  BlockType: Pointer;
begin
  //ABlockType required for detect whether singleline /multiline is being paired
  BlockType := TopCodeFoldBlockType;
  if FIsCollectingNodeInfo then
    CollectNodeInfo(True, BlockType, DecreaseLevel);

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
  Node.FoldLvlStart := CodeFoldRange.CodeFoldStackSize; // If "not AIsFold" then the node has no foldlevel of its own
  Node.NestLvlStart := CodeFoldRange.NestFoldStackSize;
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
  nd: PSynFoldNodeInfo;
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

function TSynCustomCodeFoldBlock.GetChild(ABlockType: Pointer): TSynCustomCodeFoldBlock;
begin
  if assigned(FChildren) then
    Result := FChildren.GetOrCreateSibling(ABlockType)
  else begin
    Result := TSynCustomCodeFoldBlock(self.ClassType.Create);
    Result.FBlockType := ABlockType;
    Result.FParent := self;
    FChildren := Result;
  end;
end;

var
  CreateSiblingBalanceList: Array of TSynCustomCodeFoldBlock;

function TSynCustomCodeFoldBlock.GetOrCreateSibling(ABlockType: Pointer): TSynCustomCodeFoldBlock;
  procedure BalanceNode(TheNode: TSynCustomCodeFoldBlock);
  var
    i, l: Integer;
    t: Pointer;
    N, P, C: TSynCustomCodeFoldBlock;
  begin
    l := length(CreateSiblingBalanceList);
    i := 0;
    t := TheNode.FBlockType;
    N := self;
    while N.FBlockType <> t do begin
      if i >= l then begin
        inc(l, 20);
        SetLength(CreateSiblingBalanceList, l);
      end;
      CreateSiblingBalanceList[i] := N; // Record all parents
      inc(i);
      if t < N.FBlockType
      then N := N.FLeft
      else N := N.FRight;
    end;
    if i >= l then begin
      inc(l, 20);
      SetLength(CreateSiblingBalanceList, l);
    end;
    CreateSiblingBalanceList[i] := TheNode;
    while i >= 0 do begin
      if CreateSiblingBalanceList[i].FBalance = 0
        then exit;
      if (CreateSiblingBalanceList[i].FBalance = -1) or
         (CreateSiblingBalanceList[i].FBalance = 1) then begin
        if i = 0 then
          exit;
        dec(i);
        if CreateSiblingBalanceList[i+1] = CreateSiblingBalanceList[i].FLeft
        then dec(CreateSiblingBalanceList[i].FBalance)
        else inc(CreateSiblingBalanceList[i].FBalance);
        continue;
      end;
      // rotate
      P := CreateSiblingBalanceList[i];
      if P.FBalance = -2 then begin
        N := P.FLeft;
        if N.FBalance < 0 then begin
          (* ** single rotate ** *)
          (*  []\[]_     _C                []_      C_    _[]
                    N(-1)_     _[]    =>      []_    _P(0)
                          P(-2)                  N(0)           *)
          C := N.FRight;
          N.FRight := P;
          P.FLeft := C;
          N.FBalance := 0;
          P.FBalance := 0;
        end else begin
          (* ** double rotate ** *)
          (*          x1 x2
               []_     _C                  x1    x2
                  N(+1)_     _[]    =>    N _    _ P
                        P(-2)                 C           *)
          C := N.FRight;
          N.FRight := C.FLeft;
          P.FLeft  := C.FRight;
          C.FLeft  := N;
          C.FRight := P;
          // balance
          if (C.FBalance <= 0)
          then N.FBalance := 0
          else N.FBalance := -1;
          if (C.FBalance = -1)
          then P.FBalance := 1
          else P.FBalance := 0;
          C.FBalance := 0;
          N := C;
        end;
      end else begin // *******************
        N := P.FRight;
        if N.FBalance > 0 then begin
          (* ** single rotate ** *)
          C := N.FLeft;
          N.FLeft := P;
          P.FRight := C;
          N.FBalance := 0;
          P.FBalance := 0;
        end else begin
          (* ** double rotate ** *)
          C := N.FLeft;
          N.FLeft := C.FRight;
          P.FRight  := C.FLeft;
          C.FRight  := N;
          C.FLeft := P;
          // balance
          if (C.FBalance >= 0)
          then N.FBalance := 0
          else N.FBalance := +1;
          if (C.FBalance = +1)
          then P.FBalance := -1
          else P.FBalance := 0;
          C.FBalance := 0;
          N := C;
        end;
      end;
      // update parent
      dec(i);
      if i < 0 then begin
        if assigned(self.FParent) then
          self.FParent.FChildren := N
      end else
        if CreateSiblingBalanceList[i].FLeft = P
        then CreateSiblingBalanceList[i].FLeft := N
        else CreateSiblingBalanceList[i].FRight := N;
      break;
    end
  end;
var
  P: TSynCustomCodeFoldBlock;
begin
  Result := self;
  while (assigned(Result)) do begin
    if Result.FBlockType = ABlockType then
      exit;
    P := Result;
    if ABlockType < Result.FBlockType
    then Result := Result.FLeft
    else Result := Result.FRight;
  end;
  // Not Found
  Result := TSynCustomCodeFoldBlock(self.ClassType.Create);
  Result.FBlockType := ABlockType;
  Result.FParent := self.FParent;

  if ABlockType < P.FBlockType then begin
    P.FLeft := Result;
    dec(P.FBalance);
  end else begin
    P.FRight := Result;
    inc(P.FBalance);
  end;

  // Balance
  if P.FBalance <> 0 then
    BalanceNode(P);

end;

destructor TSynCustomCodeFoldBlock.Destroy;
begin
  FreeAndNil(FRight);
  FreeAndNil(FLeft);
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TSynCustomCodeFoldBlock.WriteDebugReport;
  procedure debugout(n: TSynCustomCodeFoldBlock; s1, s: String; p: TSynCustomCodeFoldBlock);
  begin
    if n = nil then exit;
    if n.FParent <> p then
      DebugLn([s1, 'Wrong Parent for', ' (', PtrInt(n), ')']);
    DebugLn([s1, PtrUInt(n.BlockType), ' (', PtrInt(n), ')']);
    debugout(n.FLeft, s+'L: ', s+'   ', p);
    debugout(n.FRight, s+'R: ', s+'   ', p);
    debugout(n.FChildren, s+'C: ', s+'   ', n);
  end;
begin
  debugout(self, '', '', nil);
end;

procedure TSynCustomCodeFoldBlock.InitRootBlockType(AType: Pointer);
begin
  if assigned(FParent) then
    raise Exception.Create('Attempt to modify a FoldBlock');
  FBlockType := AType;
end;

{ TSynCustomHighlighterRange }

constructor TSynCustomHighlighterRange.Create(
  Template: TSynCustomHighlighterRange);
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

function TSynCustomHighlighterRange.Compare(Range: TSynCustomHighlighterRange
  ): integer;
begin
  if RangeType < Range.RangeType then
    Result:=1
  else if RangeType > Range.RangeType then
    Result:=-1
  else if Pointer(FTop) < Pointer(Range.FTop) then
    Result:= -1
  else if Pointer(FTop) > Pointer(Range.FTop) then
    Result:= 1
  else
    Result := FMinimumNestFoldBlockLevel - Range.FMinimumNestFoldBlockLevel;
  if Result <> 0 then
    exit;
  Result := FNestFoldStackSize - Range.FNestFoldStackSize;
  if Result <> 0 then
    exit;

    Result := FMinimumCodeFoldBlockLevel - Range.FMinimumCodeFoldBlockLevel;
  if Result <> 0 then
    exit;
  Result := FCodeFoldStackSize - Range.FCodeFoldStackSize;
end;

function TSynCustomHighlighterRange.Add(ABlockType: Pointer;
  IncreaseLevel: Boolean = True): TSynCustomCodeFoldBlock;
var
  i: LongInt;
begin
  i := MaxFoldLevel;
  if (i > 0) and (FCodeFoldStackSize >= i) then begin
    //debugln('Reached MaxFoldLevel, ignoring folds');
    exit(nil);
  end;
  Result := FTop.Child[ABlockType];
  inc(FNestFoldStackSize);
  if IncreaseLevel then
    inc(FCodeFoldStackSize);
  FTop:=Result;
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

procedure TSynCustomHighlighterRange.Assign(Src: TSynCustomHighlighterRange);
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
  FTop.WriteDebugReport;
end;

{ TSynCustomHighlighterRanges }

constructor TSynCustomHighlighterRanges.Create(
  TheHighlighterClass: TSynCustomHighlighterClass);
begin
  Allocate;
  FItems:=TAvlTree.Create(@CompareSynHighlighterRanges);
end;

destructor TSynCustomHighlighterRanges.Destroy;
begin
  if HighlighterRanges<>nil then begin
    HighlighterRanges.Remove(Self);
    if HighlighterRanges.Count=0 then
      FreeAndNil(HighlighterRanges);
  end;
  FItems.FreeAndClear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSynCustomHighlighterRanges.GetEqual(Range: TSynCustomHighlighterRange
  ): TSynCustomHighlighterRange;
var
  Node: TAvlTreeNode;
begin
  if Range=nil then exit(nil);
  Node:=FItems.Find(Range);
  if Node<>nil then begin
    Result:=TSynCustomHighlighterRange(Node.Data);
  end else begin
    // add a copy
    Result:=TSynCustomHighlighterRangeClass(Range.ClassType).Create(Range);
    FItems.Add(Result);
    //if FItems.Count mod 32 = 0 then debugln(['FOLDRANGE Count=', FItems.Count]);
  end;
  //debugln('TSynCustomHighlighterRanges.GetEqual A ',dbgs(Node),' ',dbgs(Result.Compare(Range)),' ',dbgs(Result.CodeFoldStackSize));
end;

procedure TSynCustomHighlighterRanges.Allocate;
begin
  inc(FAllocatedCount);
end;

procedure TSynCustomHighlighterRanges.Release;
begin
  dec(FAllocatedCount);
  if FAllocatedCount=0 then Free;
end;

{ TSynCustomFoldConfig }

procedure TSynCustomFoldConfig.SetEnabled(const AValue: Boolean);
begin
  if FEnabled = AValue then exit;
  FEnabled := AValue;
  DoOnChange;
end;

procedure TSynCustomFoldConfig.SetModes(AValue: TSynCustomFoldConfigModes);
begin
  AValue := AValue * FSupportedModes;
  if FModes = AValue then exit;
  FModes := AValue;
  FFoldActions := [];
  if fmFold   in AValue then FFoldActions := FFoldActions + [sfaFold, sfaFoldFold];
  if fmHide   in AValue then FFoldActions := FFoldActions + [sfaFold, sfaFoldHide];
  if fmMarkup in AValue then FFoldActions := FFoldActions + [sfaMarkup];
  if fmOutline in AValue then FFoldActions := FFoldActions + [sfaOutline];
  DoOnChange;
end;

procedure TSynCustomFoldConfig.SetSupportedModes(AValue: TSynCustomFoldConfigModes);
begin
  if FSupportedModes = AValue then Exit;
  FSupportedModes := AValue;
  Modes := Modes * FSupportedModes;
end;

procedure TSynCustomFoldConfig.DoOnChange;
begin
  if assigned(FOnChange) then
    FOnChange(self);
end;

constructor TSynCustomFoldConfig.Create;
begin
  Inherited;
  FIsEssential := True;
  FSupportedModes := [fmFold];
  Modes := [fmFold];
end;

constructor TSynCustomFoldConfig.Create(
  ASupportedModes: TSynCustomFoldConfigModes; AnIsEssential: Boolean);
begin
  Create;
  FSupportedModes := ASupportedModes;
  FIsEssential := AnIsEssential;
end;

procedure TSynCustomFoldConfig.Assign(Src: TSynCustomFoldConfig);
begin
  Enabled := Src.Enabled;
  SupportedModes := Src.SupportedModes;
  Modes := Src.Modes;
end;

end.


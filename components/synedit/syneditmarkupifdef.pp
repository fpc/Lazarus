(* SynEditMarkupIfDef

 Provides a framework to high-(low-)light "{$IFDEF  }" blocks. This unit is
 directly bound to the pascal Highlighter.
 The evaluation of IFDEF expression must be done by user code.

 The state of the IFDEF directives (true/false/unknown) is stored in a
 differential AVL tree.
 Each Node represents a line in the text (and may also indicate the number of
 IFDEF free lines following). The nodes contain a list of all IFDEF on their line.

 This allows to quickly find and insert/delete nodes, as well as move nodes
 (change line), if text is edited.

 The tree can be accessed, validated, invalidated, adjusted by several "shared"
 SynEdit.

*)
unit SynEditMarkupIfDef;

{$mode objfpc}{$H+}
{$ASSERTIONS on}
interface

uses
  SysUtils, Classes,
  // LCL
  Graphics, LCLProc,
  // LazUtils
  LazClasses, LazLoggerBase, LazMethodList,
  // SynEdit
  SynEditMiscClasses, SynHighlighterPas, SynEditMarkupHighAll,
  SynEditHighlighterFoldBase, SynEditFoldedView, LazSynEditText, SynEditMiscProcs,
  SynEditMarkup, SynEditPointClasses, SynEditHighlighter;

type

  { TSynRefCountedDict }

  TSynRefCountedDict = class(TRefCountedObject)
  private
    FDict: TSynSearchDictionary; // used to check for single line nodes (avoid using highlighter)
    procedure CheckWordEnd(MatchEnd: PChar; MatchIdx: Integer; var IsMatch: Boolean;
      var StopSeach: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function GetMatchAtChar(AText: PChar; ATextLen: Integer): Integer;

    property Dict: TSynSearchDictionary read FDict;
  end;


  TSynMarkupHighIfDefLinesNode = class;
  TSynMarkupHighIfDefLinesTree = class;

  { TSynMarkupHighIfDefEntry - Information about a single $IfDef/$Else/$EndIf }

  TSynMarkupIfDefNodeFlag = (
    idnMultiLineTag,                // The "{$IFDEF ... }" wraps across lines.
                                    // Only allowed on last node AND if FLine.FEndLineOffs > 0
    idnStateByUser,
    idnCommented
  );
  SynMarkupIfDefNodeFlags = set of TSynMarkupIfDefNodeFlag;

  TSynMarkupIfdefNodeType = (
    idnIfdef, idnElseIf, idnElse, idnEndIf,
    idnCommentedNode // Keep Ifdef if commented
  );

  TSynMarkupIfdefNodeStateEx  = (
    idnEnabled, idnDisabled,
    idnTempEnabled, idnTempDisabled,
    idnNotInCode,  // in currently inactive outer IfDef
    idnInvalid,    // not known for other reasons. Will not ask again
    idnUnknown,    // needs requesting
    idnRequested   // not used
    );
  TSynMarkupIfdefNodeState    = idnEnabled..idnInvalid;

  TSynMarkupIfdefPeerType = (idpOpeningPeer, idpClosingPeer);
const
  ReversePeerType: array [TSynMarkupIfdefPeerType] of TSynMarkupIfdefPeerType =
    (idpClosingPeer, idpOpeningPeer);

type
  TSynMarkupIfdefStateRequest = function(Sender: TObject; // SynEdit
    LinePos, XStartPos: Integer; // pos of the "{"
    CurrentState: TSynMarkupIfdefNodeStateEx
    ): TSynMarkupIfdefNodeState of object;

  TSynMarkupHighIfDefEntry = class
  private
    FLine: TSynMarkupHighIfDefLinesNode;
    FNodeType: TSynMarkupIfdefNodeType;
    FNodeState, FOpeningPeerNodeState: TSynMarkupIfdefNodeStateEx;
    FNodeFlags: SynMarkupIfDefNodeFlags;
    FPeers: Array [TSynMarkupIfdefPeerType] of TSynMarkupHighIfDefEntry;
    //FRelativeNestDepth: Integer;
    FStartColumn, FEndColumn: Integer;

    function GetNeedsRequesting: Boolean;
    function GetNodeState: TSynMarkupIfdefNodeStateEx;
    function GetStateByUser: Boolean;
    procedure SetLine(AValue: TSynMarkupHighIfDefLinesNode);

    function  NodeStateForPeer(APeerType: TSynMarkupIfdefNodeType): TSynMarkupIfdefNodeStateEx;
    procedure SetOpeningPeerNodeState(AValueOfPeer, AValueForNode: TSynMarkupIfdefNodeStateEx);
    procedure SetNodeState(AValue: TSynMarkupIfdefNodeStateEx; ASkipLineState: Boolean);
    procedure SetNodeState(AValue: TSynMarkupIfdefNodeStateEx);

    function  GetPeer(APeerType: TSynMarkupIfdefPeerType): TSynMarkupHighIfDefEntry;
    procedure SetPeer(APeerType: TSynMarkupIfdefPeerType; ANewPeer: TSynMarkupHighIfDefEntry);
    procedure ClearPeerField(APeerType: TSynMarkupIfdefPeerType);

    procedure ApplyNodeStateToLine(ARemove: Boolean = False);
    procedure RemoveNodeStateFromLine;
    procedure SetStartColumn(AValue: Integer);
    procedure SetStateByUser(AValue: Boolean);
  protected
    procedure SetNodeType(ANodeType: TSynMarkupIfdefNodeType);
    function  DebugText(Short: Boolean = False): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearPeers;
    procedure ClearAll;
  public
    procedure MakeDisabled;
    procedure MakeEnabled;
    procedure MakeRequested;
    procedure MakeUnknown;

    function  IsRequested: Boolean;
    function  HasKnownState: Boolean; // Opposite of NeedsRequesting (except idnRequested) / BUT ignore NodeType
    property  NeedsRequesting: Boolean read GetNeedsRequesting;
    property  StateByUser: Boolean read GetStateByUser write SetStateByUser; // only else(if) keep state set by user

    function  IsDisabled: Boolean;
    function  HasDisabledOpening: Boolean;
    function  IsEnabled: Boolean;
    function  HasEnabledOpening: Boolean;
    function  IsTemp: Boolean;
    function  HasTempOpening: Boolean;

    function  IsOpening: Boolean;
    function  IsClosing: Boolean;
    function  IsTempOpening: Boolean;
    function  IsTempClosing: Boolean;
    function  IsDisabledOpening: Boolean;
    function  IsDisabledClosing: Boolean;

    function  NodeType: TSynMarkupIfdefNodeType;
    property  NodeState: TSynMarkupIfdefNodeStateEx read GetNodeState write SetNodeState;
    property  NodeFlags: SynMarkupIfDefNodeFlags read FNodeFlags write FNodeFlags;
    function  UncommentedNodeType: TSynMarkupIfdefNodeType;
    procedure MakeCommented;
    procedure MakeUnCommented;
    property  Line: TSynMarkupHighIfDefLinesNode read FLine write SetLine;
    property  StartColumn: Integer read FStartColumn write SetStartColumn;// FStartColumn;
    property  EndColumn:   Integer read FEndColumn write FEndColumn;
    // RelativeNestDepth (opening depth)) First node is always 0 // nodes in line can be negative
    ////property  RelativeNestDepth: Integer read FRelativeNestDepth;
// COMMENT  BEFORE  AUTO  COMPLETE !!!!!
    property OpeningPeer: TSynMarkupHighIfDefEntry index idpOpeningPeer read GetPeer write SetPeer;
    property ClosingPeer: TSynMarkupHighIfDefEntry index idpClosingPeer read GetPeer write SetPeer;
  end;

  { TSynMarkupHighIfDefLinesNode - List of all nodes on the same line }

  SynMarkupIfDefLineFlag = (
    idlValid,            // X start/stop positions are ok
    idlAllNodesCommented, // all nodes are comments
    idlHasUnknownNodes,  // need requesting
    idlHasNodesNotInCode,
    idlNotInCodeToUnknown,    // treat all idnNotInCode nodes as unknown
    idlNotInCodeToUnknownReq, // Request to set all nested lines to ... during next validate
    idlDisposed,          // Node is disposed, may be re-used
    idlInGlobalClear      // Skip unlinking Peers
  );
  SynMarkupIfDefLineFlags = set of SynMarkupIfDefLineFlag;

  TSynMarkupHighIfDefLinesNode = class(TSynSizedDifferentialAVLNode)
  private
    FDisabledEntryCloseCount: Integer;
    FDisabledEntryOpenCount: Integer;
    FEntries: Array of TSynMarkupHighIfDefEntry;
    FEntryCount: Integer;

    FLastEntryEndLineOffs: Integer;
    FLineFlags: SynMarkupIfDefLineFlags;
    FScanEndOffs: Integer;
    function GetEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
    function GetEntryCapacity: Integer;
    function GetId: PtrUInt;
    procedure SetEntry(AIndex: Integer; AValue: TSynMarkupHighIfDefEntry);
    procedure SetEntryCapacity(AValue: Integer);
    procedure SetEntryCount(AValue: Integer);
  protected
    procedure AdjustPositionOffset(AnAdjustment: integer); // Caller is responsible for staying between neighbours
    property NextDispose: TSynSizedDifferentialAVLNode read FParent write FParent;
    function DebugText: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeDisposed; // Also called to clear, on real destroy
    property LineFlags: SynMarkupIfDefLineFlags read FLineFlags;
    // LastEntryEndLineOffs: For last Entry only, if entry closing "}" is on a diff line. (can go one OVER ScanEndOffs)
    property LastEntryEndLineOffs: Integer read FLastEntryEndLineOffs write FLastEntryEndLineOffs;
    // ScanEndOffs: How many (empty) lines were scanned after this node
    property ScanEndOffs: Integer read FScanEndOffs write FScanEndOffs;
    property DisabledEntryOpenCount: Integer read FDisabledEntryOpenCount write FDisabledEntryOpenCount;
    property DisabledEntryCloseCount: Integer read FDisabledEntryCloseCount write FDisabledEntryCloseCount;
  public
    function AddEntry(AIndex: Integer = -1): TSynMarkupHighIfDefEntry;
    procedure DeletEntry(AIndex: Integer; AFree: Boolean = false);
    procedure ReduceCapacity;
    function IndexOf(AEntry: TSynMarkupHighIfDefEntry): Integer;
    property EntryCount: Integer read FEntryCount write SetEntryCount;
    property EntryCapacity: Integer read GetEntryCapacity write SetEntryCapacity;
    property Entry[AIndex: Integer]: TSynMarkupHighIfDefEntry read GetEntry write SetEntry; default;
  end;

  { TSynMarkupHighIfDefLinesNodeInfo }

  TSynMarkupHighIfDefLinesNodeInfo = object
  private
    FAtBOL: Boolean;
    FAtEOL: Boolean;
    FNode: TSynMarkupHighIfDefLinesNode;
    FTree: TSynMarkupHighIfDefLinesTree;
    FStartLine, Index: Integer; // Todo: Indek is not used
    FCacheNestMinimum, FCacheNestStart, FCacheNestEnd: Integer;
    function GetLastEntryEndLine: Integer;
    function GetLastEntryEndLineOffs: Integer;
    function GetEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
    function GetEntryCount: Integer;
    function GetLineFlags: SynMarkupIfDefLineFlags;
    function GetScanEndLine: Integer;
    function GetScanEndOffs: Integer;
    procedure SetEntry(AIndex: Integer; AValue: TSynMarkupHighIfDefEntry);
    procedure SetEntryCount(AValue: Integer);
    procedure SetLastEntryEndLineOffs(AValue: Integer);
    procedure SetScanEndLine(AValue: Integer);
    procedure SetScanEndOffs(AValue: Integer);
    procedure SetStartLine(AValue: Integer);  // Caller is responsible for staying between neighbours
    function DebugText: String;
  public
    procedure ClearInfo;
    procedure InitForNode(ANode: TSynMarkupHighIfDefLinesNode; ALine: Integer);
    function Precessor: TSynMarkupHighIfDefLinesNodeInfo;
    function Successor: TSynMarkupHighIfDefLinesNodeInfo;
  public
    procedure ClearNestCache;
    function NestMinimumDepthAtNode: Integer;
    function NestDepthAtNodeStart: Integer;
    function NestDepthAtNodeEnd: Integer;
  public
    function IsValid: Boolean;
    procedure Invalidate;
    function HasNode: Boolean;
    property StartLine: Integer read FStartLine write SetStartLine;
    property LineFlags: SynMarkupIfDefLineFlags read GetLineFlags;
    property LastEntryEndLineOffs: Integer read GetLastEntryEndLineOffs write SetLastEntryEndLineOffs;
    property LastEntryEndLine: Integer read GetLastEntryEndLine; // write SetLastEntryEndLineOffs;
    property ScanEndOffs: Integer read GetScanEndOffs write SetScanEndOffs;
    property ScanEndLine: Integer read GetScanEndLine write SetScanEndLine;
    function ValidToLine(const ANextNode: TSynMarkupHighIfDefLinesNodeInfo): Integer; // ScanEndLine or next node

    //function AddEntry: TSynMarkupHighIfDefEntry;
    //procedure DeletEntry(AIndex: Integer; AFree: Boolean = false);
    property EntryCount: Integer read GetEntryCount write SetEntryCount;
    property Entry[AIndex: Integer]: TSynMarkupHighIfDefEntry read GetEntry write SetEntry;
    property AtBOL: Boolean read FAtBOL;
    property AtEOL: Boolean read FAtEOL;
    property Node: TSynMarkupHighIfDefLinesNode read FNode;
  end;

  { TSynMarkupHighIfDefLinesNodeInfoList }

  TSynMarkupHighIfDefLinesNodeInfoList = object
  private
    FCount: Integer;
    FNestOpenNodes: Array of TSynMarkupHighIfDefLinesNodeInfo;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetNode(AIndex: Integer): TSynMarkupHighIfDefLinesNodeInfo;
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetNode(AIndex: Integer; AValue: TSynMarkupHighIfDefLinesNodeInfo);
    procedure SetNodes(ALow, AHigh: Integer; const AValue: TSynMarkupHighIfDefLinesNodeInfo);
  public
    property Count: Integer read GetCount write SetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Node[AIndex: Integer]: TSynMarkupHighIfDefLinesNodeInfo read GetNode write SetNode;
    property Nodes[ALow, AHigh: Integer]: TSynMarkupHighIfDefLinesNodeInfo write SetNodes;
    Procedure PushNodeLine(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
    procedure dbg;
  end;

  TSynMarkupHighIfDefTreeNotifications = (
    itnUnlocking,  // About to unlock, Markup should validate it's range
    itnUnlocked,   // Unlocked, markup should update it's matches
    itnChanged     // A node was changed, while NOT locked / SetNodeState
  );

  { TSynMarkupHighIfDefLinesTree }

  TSynMarkupHighIfDefLinesTree = class(TSynSizedDifferentialAVLTree)
  private
    FHighlighter: TSynPasSyn;
    FLines: TSynEditStringsLinked;
    FClearing: Boolean;
    FDisposedNodes: TSynSizedDifferentialAVLNode;
    FOnNodeStateRequest: TSynMarkupIfdefStateRequest;
    FRequestingNodeState: Boolean;
    FLockTreeCount: Integer;
    FNotifyLists: Array [TSynMarkupHighIfDefTreeNotifications] of TMethodList;
    FChangeStep: Integer;

    procedure IncChangeStep;
    procedure SetHighlighter(AValue: TSynPasSyn);
    procedure SetLines(AValue: TSynEditStringsLinked);
    function GetHighLighterWithLines: TSynCustomFoldHighlighter;
  private
    //copied from SynEdit
    //TODO, move to highlighter
    function GetHighlighterAttriAtRowColEx(XY: TPoint; out Token: string;
      out TokenType, Start: Integer;
      out Attri: TSynHighlighterAttributes): boolean;                           //L505


    procedure MaybeRequestNodeStates(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
    procedure MaybeValidateNode(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
    procedure MaybeExtendNodeBackward(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
      AStopAtLine: Integer = 0);
    procedure MaybeExtendNodeForward(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
      var ANextNode: TSynMarkupHighIfDefLinesNodeInfo;
      AStopBeforeLine: Integer = -1);

    function  GetOrInsertNodeAtLine(ALinePos: Integer): TSynMarkupHighIfDefLinesNodeInfo;
    procedure ConnectPeers(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
                           var ANestList: TSynMarkupHighIfDefLinesNodeInfoList;
                           AOuterLines: TLazSynEditNestedFoldsList = nil);
    function  CheckLineForNodes(ALine: Integer): Boolean;
    procedure ScanLine(ALine: Integer; var ANodeForLine: TSynMarkupHighIfDefLinesNode;
                       ACheckOverlapOnCreateLine: Boolean = False);

    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
    procedure DoHighlightChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
  protected
    function  CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode; override;
    procedure DisposeNode(var ANode: TSynSizedDifferentialAVLNode); override;
    procedure RemoveLine(var ANode: TSynMarkupHighIfDefLinesNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure DebugPrint(Flat: Boolean = False);

    procedure RegisterNotification(AReason: TSynMarkupHighIfDefTreeNotifications;
                                   AHandler: TNotifyEvent);
    procedure UnRegisterNotification(AReason: TSynMarkupHighIfDefTreeNotifications;
                                     AHandler: TNotifyEvent);
    procedure LockTree;
    procedure UnLockTree;

    function FindNodeAtPosition(ALine: Integer;
                                AMode: TSynSizedDiffAVLFindMode): TSynMarkupHighIfDefLinesNodeInfo;
                                overload;

    function  CreateOpeningList: TLazSynEditNestedFoldsList;
    procedure DiscardOpeningList(AList: TLazSynEditNestedFoldsList);

    procedure ValidateRange(AStartLine, AEndLine: Integer;
                            OuterLines: TLazSynEditNestedFoldsList);
    procedure SetNodeState(ALinePos, AstartPos: Integer; AState: TSynMarkupIfdefNodeState);

    property OnNodeStateRequest: TSynMarkupIfdefStateRequest read FOnNodeStateRequest write FOnNodeStateRequest;
    property Highlighter: TSynPasSyn read FHighlighter write SetHighlighter;
    property Lines : TSynEditStringsLinked read FLines write SetLines;
    property ChangeStep: Integer read FChangeStep;
  end;

  { TSynEditMarkupIfDefBase }

  TSynEditMarkupIfDefBase = class(TSynEditMarkupHighlightMatches)
  private
    FScanLastMatchIdx: Integer;
  protected
    function CreateMatchList: TSynMarkupHighAllMatchList; override;
    function HasEnabledMarkup: Boolean; virtual;

    procedure StartMatchScan;
    procedure EndMatchScan(ALastLine: Integer);
    procedure AddMatch(P1, P2: TPoint; P1AtTop, P2AtBottom: Boolean; AnID: Integer);
  public
    constructor Create(ASynEdit: TSynEditBase);
    function RealEnabled: Boolean; override;
  end;

  { TSynEditMarkupIfDefNodes }

  TSynEditMarkupIfDefNodes = class(TSynEditMarkupIfDefBase)
  private
    FMarkupInfoEnabled: TSynSelectedColor;
    FMarkupInfoTempDisabled: TSynSelectedColor;
    FMarkupInfoTempEnabled: TSynSelectedColor;
    function GetMarkupInfo: TSynSelectedColor;
  protected
    function MarkupIdForMatch(Idx: Integer): Integer; override;
    function HasEnabledMarkup: Boolean; override;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    procedure MergeMarkupAttributeAtRowCol(const aRow: Integer; const aStartCol,
      AEndCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo;
      AMarkup: TSynSelectedColorMergeResult); override;

    property MarkupInfoEnabled:      TSynSelectedColor read FMarkupInfoEnabled;
    property MarkupInfoDisabled:     TSynSelectedColor read GetMarkupInfo; // alias for MarkupInfo
    property MarkupInfoTempEnabled:  TSynSelectedColor read FMarkupInfoTempEnabled;
    property MarkupInfoTempDisabled: TSynSelectedColor read FMarkupInfoTempDisabled;
  end;

  { TSynEditMarkupIfDef }

  TSynEditMarkupIfDef = class(TSynEditMarkupIfDefBase)
  private
    FFoldView: TSynEditFoldedView;
    FHighlighter: TSynPasSyn;
    FIfDefTree: TSynMarkupHighIfDefLinesTree;
    FOnNodeStateRequest: TSynMarkupIfdefStateRequest;
    FOuterLines: TLazSynEditNestedFoldsList;
    FAdjustedTop: Integer;
    FLastValidTopLine, FLastValidLastLine: Integer;
    FLastValidTreeStep: Integer;
    FMarkOnlyOpeningNodes: Boolean;

    FMarkupNodes: TSynEditMarkupIfDefNodes;
    FMarkupEnabled: TSynEditMarkupIfDefBase;
    FMarkupTemp, FMarkupEnabledTemp: TSynEditMarkupIfDefBase;

    function GetMarkupInfoDisabled: TSynSelectedColor;
    function GetMarkupInfoEnabled: TSynSelectedColor;
    function GetMarkupInfoNodeDisabled: TSynSelectedColor;
    function GetMarkupInfoNodeEnabled: TSynSelectedColor;
    function GetMarkupInfoTempDisabled: TSynSelectedColor;
    function GetMarkupInfoTempEnabled: TSynSelectedColor;
    function GetMarkupInfoTempNodeDisabled: TSynSelectedColor;
    function GetMarkupInfoTempNodeEnabled: TSynSelectedColor;
    procedure SetFoldView(AValue: TSynEditFoldedView);
    procedure SetHighlighter(AValue: TSynPasSyn);
    procedure DoBufferChanging(Sender: TObject);
    procedure DoBufferChanged(Sender: TObject);
    function  DoNodeStateRequest(Sender: TObject; LinePos, XStartPos: Integer;
      CurrentState: TSynMarkupIfdefNodeStateEx): TSynMarkupIfdefNodeState;
    procedure SetMarkOnlyOpeningNodes(AValue: Boolean);

    Procedure ValidateMatches;
    procedure DoTreeUnlocking(Sender: TObject);
    procedure DoTreeUnlocked(Sender: TObject);
    procedure DoTreeChanged(Sender: TObject);
    procedure PrepareHighlighter;
  protected
    function  HasEnabledMarkup: Boolean; override;

    procedure DoFoldChanged(aLine: Integer);
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override; // 1 based
    procedure DoVisibleChanged(AVisible: Boolean); override;
    procedure SetLines(const AValue: TSynEditStringsLinked); override;
    procedure SetInvalidateLinesMethod(const AValue: TInvalidateLines); override;
    property  IfDefTree: TSynMarkupHighIfDefLinesTree read FIfDefTree;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure IncPaintLock; override;
    procedure DecPaintLock; override;

    procedure PrepareMarkupForRow(aRow: Integer); override;
    procedure FinishMarkupForRow(aRow: Integer); override;
    procedure EndMarkup; override;
    //GetMarkupAttributeAtRowCol
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out
      ANextPhys, ANextLog: Integer); override;
    procedure MergeMarkupAttributeAtRowCol(const aRow: Integer; const aStartCol,
      AEndCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo;
      AMarkup: TSynSelectedColorMergeResult); override;

    procedure InvalidateAll;
    procedure SetNodeState(ALinePos, AstartPos: Integer; AState: TSynMarkupIfdefNodeState);

    property FoldView: TSynEditFoldedView read FFoldView write SetFoldView;
    property Highlighter: TSynPasSyn read FHighlighter write SetHighlighter;
    property OnNodeStateRequest: TSynMarkupIfdefStateRequest read FOnNodeStateRequest write FOnNodeStateRequest;

    property MarkOnlyOpeningNodes: Boolean read FMarkOnlyOpeningNodes write SetMarkOnlyOpeningNodes;

    property MarkupInfoDisabled:  TSynSelectedColor read GetMarkupInfoDisabled; // alias for MarkupInfo
    property MarkupInfoEnabled:  TSynSelectedColor read GetMarkupInfoEnabled;
    property MarkupInfoNodeDisabled: TSynSelectedColor read GetMarkupInfoNodeDisabled;
    property MarkupInfoNodeEnabled:  TSynSelectedColor read GetMarkupInfoNodeEnabled;

    property MarkupInfoTempDisabled:     TSynSelectedColor read GetMarkupInfoTempDisabled;
    property MarkupInfoTempEnabled:      TSynSelectedColor read GetMarkupInfoTempEnabled;
    property MarkupInfoTempNodeDisabled: TSynSelectedColor read GetMarkupInfoTempNodeDisabled;
    property MarkupInfoTempNodeEnabled:  TSynSelectedColor read GetMarkupInfoTempNodeEnabled;
  end;

function dbgs(AFlag: SynMarkupIfDefLineFlag): String; overload;
function dbgs(AFlags: SynMarkupIfDefLineFlags): String; overload;
function dbgs(AFlag: TSynMarkupIfDefNodeFlag): String; overload;
function dbgs(AFlags: SynMarkupIfDefNodeFlags): String; overload;
function dbgs(AFlag: TSynMarkupIfdefNodeType): String; overload;
function dbgs(AFlag: TSynMarkupIfdefNodeStateEx): String; overload;
function dbgs(APeerType: TSynMarkupIfdefPeerType): String; overload;

implementation

uses
  SynEdit;
var
  TheDict: TSynRefCountedDict = nil;
  {$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
  DebugCurTreeForAssert: TSynMarkupHighIfDefLinesTree = nil;
  {$ENDIF}

type

  { TSynRefCountedDictIfDef }

  TSynRefCountedDictIfDef = class(TSynRefCountedDict)
  public
    destructor Destroy; override;
  end;

const
  MARKUP_DEFAULT = 0;
  // Nodes, bitmask
  MARKUP_DISABLED = 1;
  MARKUP_ENABLED  = 2;
  MARKUP_TEMP_DISABLED = 4;
  MARKUP_TEMP_ENABLED  = 8;

procedure MaybeCreateDict;
begin
  if TheDict = nil then
    TheDict := TSynRefCountedDictIfDef.Create;
end;

function dbgs(AFlag: SynMarkupIfDefLineFlag): String;
begin
  Result := '';
  WriteStr(Result, AFlag);
end;

function dbgs(AFlags: SynMarkupIfDefLineFlags): String;
var
  i: SynMarkupIfDefLineFlag;
begin
  Result := '';
  for i := low(AFlags) to high(AFlags) do
    if i in AFlags then
      if Result = '' then
        Result := Result + dbgs(i)
      else
        Result := Result + ', ' + dbgs(i);
  Result := '[' + Result + ']';
end;

function dbgs(AFlag: TSynMarkupIfDefNodeFlag): String;
begin
  Result := '';
  WriteStr(Result, AFlag);
end;

function dbgs(AFlags: SynMarkupIfDefNodeFlags): String;
var
  i: TSynMarkupIfDefNodeFlag;
begin
  Result := '';
  for i := low(AFlags) to high(AFlags) do
    if i in AFlags then
      if Result = '' then
        Result := Result + dbgs(i)
      else
        Result := Result + ', ' + dbgs(i);
  Result := '[' + Result + ']';
end;

function dbgs(AFlag: TSynMarkupIfdefNodeType): String;
begin
  Result := '';
  WriteStr(Result, AFlag);
end;

function dbgs(AFlag: TSynMarkupIfdefNodeStateEx): String;
begin
  Result := '';
  WriteStr(Result, AFlag);
end;

function dbgs(APeerType: TSynMarkupIfdefPeerType): String;
begin
  Result := '';
  WriteStr(Result, APeerType);
end;

{ TSynEditMarkupIfDefNodes }

function TSynEditMarkupIfDefNodes.GetMarkupInfo: TSynSelectedColor;
begin
  Result := MarkupInfo;
end;

function TSynEditMarkupIfDefNodes.MarkupIdForMatch(Idx: Integer): Integer;
begin
  Result := TSynMarkupHighAllMultiMatchList(Matches).MarkupId[Idx];
end;

function TSynEditMarkupIfDefNodes.HasEnabledMarkup: Boolean;
begin
  Result := (inherited HasEnabledMarkup) or
            FMarkupInfoEnabled.IsEnabled or
            FMarkupInfoTempDisabled.IsEnabled or
            FMarkupInfoTempEnabled.IsEnabled;
end;

constructor TSynEditMarkupIfDefNodes.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);

  FMarkupInfoEnabled      := TSynSelectedColor.Create;
  FMarkupInfoTempDisabled := TSynSelectedColor.Create;
  FMarkupInfoTempEnabled  := TSynSelectedColor.Create;

  MarkupInfoDisabled.Clear;
  MarkupInfoEnabled.Clear;
  MarkupInfoTempDisabled.Clear;
  MarkupInfoTempEnabled.Clear;

  MarkupInfoEnabled.ForePriority := 99999+1;
  MarkupInfoDisabled.ForePriority := 99999+1;
  MarkupInfoTempEnabled.ForePriority := 99999+1;
  MarkupInfoTempDisabled.ForePriority := 99999+1;

  MarkupInfoDisabled.OnChange := @MarkupChanged;
  MarkupInfoEnabled.OnChange := @MarkupChanged;
  MarkupInfoTempEnabled.OnChange := @MarkupChanged;
  MarkupInfoTempDisabled.OnChange := @MarkupChanged;
end;

destructor TSynEditMarkupIfDefNodes.Destroy;
begin
  inherited Destroy;
  FreeThenNil(FMarkupInfoEnabled);
  FreeThenNil(FMarkupInfoTempDisabled);
  FreeThenNil(FMarkupInfoTempEnabled);
end;

procedure TSynEditMarkupIfDefNodes.MergeMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol, AEndCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo;
  AMarkup: TSynSelectedColorMergeResult);
var
  i, s, e: Integer;
  c: TSynSelectedColor;
begin
  i := GetMarkupAttrIdAtRowCol(aRow, aStartCol, s, e);
  if i < 0 then
    exit;

  if (i and MARKUP_ENABLED) <> 0 then begin
    c := MarkupInfoEnabled;
    c.SetFrameBoundsLog(s, e);
    AMarkup.Merge(c, aStartCol, AEndCol);
  end;
  if (i and MARKUP_DISABLED) <> 0 then begin
    c := MarkupInfoDisabled;
    c.SetFrameBoundsLog(s, e);
    AMarkup.Merge(c, aStartCol, AEndCol);
  end;

  if (i and MARKUP_TEMP_ENABLED) <> 0 then begin
    c := MarkupInfoTempEnabled;
    c.SetFrameBoundsLog(s, e);
    AMarkup.Merge(c, aStartCol, AEndCol);
  end;
  if (i and MARKUP_TEMP_DISABLED) <> 0 then begin
    c := MarkupInfoTempDisabled;
    c.SetFrameBoundsLog(s, e);
    AMarkup.Merge(c, aStartCol, AEndCol);
  end;
end;

{ TSynEditMarkupIfDefBase }

function TSynEditMarkupIfDefBase.HasEnabledMarkup: Boolean;
begin
  Result := MarkupInfo.IsEnabled;
end;

function TSynEditMarkupIfDefBase.CreateMatchList: TSynMarkupHighAllMatchList;
begin
  Result := TSynMarkupHighAllMultiMatchList.Create;
end;

procedure TSynEditMarkupIfDefBase.StartMatchScan;
begin
  FScanLastMatchIdx := -1;
end;

procedure TSynEditMarkupIfDefBase.EndMatchScan(ALastLine: Integer);
var
  m: TSynMarkupHighAllMatch;
begin
  while Matches.Count - 1 > FScanLastMatchIdx do begin
    m := Matches.Match[Matches.Count - 1];
    if (m.EndPoint.y >= TopLine) and (m.StartPoint.y <= ALastLine) then
      InvalidateSynLines(m.StartPoint.y, m.EndPoint.y);
    Matches.Delete(Matches.Count - 1);
  end;
end;

procedure TSynEditMarkupIfDefBase.AddMatch(P1, P2: TPoint; P1AtTop, P2AtBottom: Boolean;
  AnID: Integer);
var
  Match, OldMatch: TSynMarkupHighAllMatch;
  MList: TSynMarkupHighAllMultiMatchList;
begin
  if ComparePoints(P1, P2) = 0 then // empty match does not highlight
    exit;
  //if not MarkupInfoForMatch(AnID).IsEnabled then exit;

  Match.StartPoint := P1;
  Match.EndPoint   := P2;

  MList := Matches as TSynMarkupHighAllMultiMatchList;
  inc(FScanLastMatchIdx);
  if FScanLastMatchIdx >= MList.Count then begin
    MList.Match[FScanLastMatchIdx] := Match;
    MList.MarkupId[FScanLastMatchIdx] := AnID;
    InvalidateSynLines(Match.StartPoint.y, Match.EndPoint.y);
    exit;
  end;

  OldMatch := MList.Match[FScanLastMatchIdx];
  if ( (ComparePoints(OldMatch.StartPoint, Match.StartPoint) = 0) or
       (P1AtTop and (ComparePoints(OldMatch.StartPoint, Match.StartPoint) <= 0))
     ) and
     ( (ComparePoints(OldMatch.EndPoint, Match.EndPoint) = 0) or
       (P2AtBottom and (ComparePoints(OldMatch.EndPoint, Match.EndPoint) >= 0))
     )
  then begin // existing match found
    if MList.MarkupId[FScanLastMatchIdx] <> AnID then begin
      InvalidateSynLines(OldMatch.StartPoint.y, OldMatch.EndPoint.y);;
      MList.MarkupId[FScanLastMatchIdx] := AnID;
    end;
    exit;
  end;

  if ComparePoints(OldMatch.StartPoint, Match.EndPoint) >= 0 then
    MList.Insert(FScanLastMatchIdx, 1)
  else
    InvalidateSynLines(OldMatch.StartPoint.y, OldMatch.EndPoint.y);;

  MList.Match[FScanLastMatchIdx] := Match;
  MList.MarkupId[FScanLastMatchIdx] := AnID;
  InvalidateSynLines(Match.StartPoint.y, Match.EndPoint.y);
end;

constructor TSynEditMarkupIfDefBase.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);

  MarkupInfo.Clear;
  MarkupInfo.ForePriority := 99999;
end;

function TSynEditMarkupIfDefBase.RealEnabled: Boolean;
begin
  Result := Enabled and (not IsTempDisabled) and HasEnabledMarkup;
end;

{ TSynRefCountedDictIfDef }

destructor TSynRefCountedDictIfDef.Destroy;
begin
  inherited Destroy;
  TheDict := nil;
end;

{ TSynMarkupHighIfDefLinesNodeInfoList }

function TSynMarkupHighIfDefLinesNodeInfoList.GetCapacity: Integer;
begin
  Result := Length(FNestOpenNodes);
end;

function TSynMarkupHighIfDefLinesNodeInfoList.GetCount: Integer;
begin
  if Capacity = 0 then
    FCount := 0;
  Result := FCount;
end;

function TSynMarkupHighIfDefLinesNodeInfoList.GetNode(AIndex: Integer): TSynMarkupHighIfDefLinesNodeInfo;
begin
  Assert((AIndex < Count) and (AIndex >= 0), 'TSynMarkupHighIfDefLinesNodeInfoList.GetNode Index='+IntToStr(AIndex)+' Cnt='+IntToStr(Count));
  Result := FNestOpenNodes[AIndex];
end;

procedure TSynMarkupHighIfDefLinesNodeInfoList.SetCapacity(AValue: Integer);
begin
  if Capacity = 0 then
    FCount := 0;
  SetLength(FNestOpenNodes, AValue);
  FCount := Min(FCount, AValue);
end;

procedure TSynMarkupHighIfDefLinesNodeInfoList.SetCount(AValue: Integer);
begin
  if Capacity = 0 then
    FCount := 0;
  if Count = AValue then Exit;
  Capacity := Max(FCount, Capacity);
  FCount := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfoList.SetNode(  AIndex: Integer;
  AValue: TSynMarkupHighIfDefLinesNodeInfo);
begin
  Assert((  AIndex < Count) and (  AIndex >= 0), 'TSynMarkupHighIfDefLinesNodeInfoList.SetNode Index='+IntToStr(AIndex)+' Cnt='+IntToStr(Count));
  FNestOpenNodes[  AIndex] := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfoList.SetNodes(ALow, AHigh: Integer;
  const AValue: TSynMarkupHighIfDefLinesNodeInfo);
var
  i: Integer;
begin
  if AHigh >= Count then begin
    Capacity := 1 + AHigh + Min(AHigh div 2, 100);
    Count := AHigh + 1;
  end;
  for i := ALow to AHigh do
    FNestOpenNodes[i] := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfoList.PushNodeLine(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
begin
  Nodes[ANode.NestMinimumDepthAtNode+1, ANode.NestDepthAtNodeEnd] := ANode;
end;

procedure TSynMarkupHighIfDefLinesNodeInfoList.dbg;
var
  i: Integer;
begin
  for i := 0 to Count-1 do begin
    DbgOut(['##  ',i, ': ', dbgs(Node[i].HasNode)]);
    if Node[i].HasNode then  DbgOut(['(', Node[i].StartLine, ')']);
  end;
  DebugLn();
end;

{ TSynRefCountedDict }

procedure TSynRefCountedDict.CheckWordEnd(MatchEnd: PChar; MatchIdx: Integer;
  var IsMatch: Boolean; var StopSeach: Boolean);
begin
  IsMatch := not ((MatchEnd)^ in ['a'..'z', 'A'..'Z', '0'..'9', '_']);
end;

constructor TSynRefCountedDict.Create;
begin
  inherited Create;
  FDict := TSynSearchDictionary.Create;
  FDict.Add('{$if',     1);
  FDict.Add('{$ifc',    1);
  FDict.Add('{$ifdef',  1);
  FDict.Add('{$ifndef', 1);
  FDict.Add('{$ifopt',  1);

  FDict.Add('{$else',   2);
  FDict.Add('{$elsec',  2);

  FDict.Add('{$elseif', 4);
  FDict.Add('{$elifc',  4);

  FDict.Add('{$endif',  3);
  FDict.Add('{$ifend',  3);
  FDict.Add('{$endc',   3);

end;

destructor TSynRefCountedDict.Destroy;
begin
  FDict.Free;
  inherited Destroy;
end;

function TSynRefCountedDict.GetMatchAtChar(AText: PChar; ATextLen: Integer): Integer;
begin
  Result := FDict.GetMatchAtChar(AText, ATextLen, @CheckWordEnd);
end;

{ TSynMarkupHighIfDefEntry }

function TSynMarkupHighIfDefEntry.NodeType: TSynMarkupIfdefNodeType;
begin
  Result := FNodeType;
  if idnCommented in NodeFlags then
    Result := idnCommentedNode;
end;

function TSynMarkupHighIfDefEntry.UncommentedNodeType: TSynMarkupIfdefNodeType;
begin
  Result := FNodeType; 
end;

procedure TSynMarkupHighIfDefEntry.MakeCommented;
begin
  RemoveNodeStateFromLine;
  Include(FNodeFlags, idnCommented);
  ApplyNodeStateToLine;
end;

procedure TSynMarkupHighIfDefEntry.MakeUnCommented;
begin
  RemoveNodeStateFromLine;
  Exclude(FNodeFlags, idnCommented);
  ApplyNodeStateToLine;
end;

function TSynMarkupHighIfDefEntry.IsDisabled: Boolean;
begin
  Result := FNodeState in [idnDisabled, idnTempDisabled];
end;

function TSynMarkupHighIfDefEntry.HasDisabledOpening: Boolean;
begin
  Result := IsClosing and (FOpeningPeerNodeState in [idnDisabled, idnTempDisabled]);
end;

function TSynMarkupHighIfDefEntry.IsEnabled: Boolean;
begin
  Result := FNodeState in [idnEnabled, idnTempEnabled];
end;

function TSynMarkupHighIfDefEntry.HasEnabledOpening: Boolean;
begin
  Result := IsClosing and (FOpeningPeerNodeState in [idnEnabled, idnTempEnabled]);
end;

function TSynMarkupHighIfDefEntry.IsRequested: Boolean;
begin
  Result := FNodeState = idnRequested;
end;

function TSynMarkupHighIfDefEntry.IsTempClosing: Boolean;
begin
  Result := IsClosing and HasTempOpening;
end;

function TSynMarkupHighIfDefEntry.IsTempOpening: Boolean;
begin
  Result := IsOpening and IsTemp;
end;

function TSynMarkupHighIfDefEntry.GetNeedsRequesting: Boolean;
begin
  Result := ( (NodeType = idnIfdef) or
              ( (NodeType = idnElseIf) and (FOpeningPeerNodeState in [idnEnabled, idnTempEnabled]) )
            ) and
            ( (FNodeState in [idnUnknown, idnRequested]) or
              ( (FNodeState = idnNotInCode) and (idlNotInCodeToUnknown in Line.LineFlags) )
            );
end;

function TSynMarkupHighIfDefEntry.GetNodeState: TSynMarkupIfdefNodeStateEx;
begin
  Result := FNodeState
end;

function TSynMarkupHighIfDefEntry.GetStateByUser: Boolean;
begin
  Result := idnStateByUser in FNodeFlags;
end;

procedure TSynMarkupHighIfDefEntry.SetLine(AValue: TSynMarkupHighIfDefLinesNode);
begin
  if FLine = AValue then Exit;

  RemoveNodeStateFromLine;

  if (FNodeState = idnNotInCode) and (FLine <> nil) and
     (idlNotInCodeToUnknown in FLine.LineFlags)
  then
    SetNodeState(idnUnknown, True);

  FLine := AValue;
  ApplyNodeStateToLine;
end;

procedure TSynMarkupHighIfDefEntry.SetOpeningPeerNodeState(AValueOfPeer,
  AValueForNode: TSynMarkupIfdefNodeStateEx);
begin
  RemoveNodeStateFromLine;
  FOpeningPeerNodeState := AValueOfPeer;

  // ignore invalidated by LineFlags If StateByUser. Keep old (invalid) state until set again
  if (not StateByUser) or (AValueForNode = idnUnknown)
  then begin

    if NodeType in [idnElse, idnEndIf] then
      SetNodeState(AValueForNode, True)
    else
    if NodeType = idnElseIf then
      case AValueForNode of
        idnEnabled, idnTempEnabled:   SetNodeState(idnUnknown, True); // Maybe keep?
        idnDisabled, idnTempDisabled: SetNodeState(AValueForNode, True);
        else         SetNodeState(idnUnknown, True);
      end;
  end;

  ApplyNodeStateToLine;
end;

procedure TSynMarkupHighIfDefEntry.SetNodeState(AValue: TSynMarkupIfdefNodeStateEx);
begin
  SetNodeState(AValue, False);
end;

procedure TSynMarkupHighIfDefEntry.SetNodeState(AValue: TSynMarkupIfdefNodeStateEx;
  ASkipLineState: Boolean);
begin
  if not ASkipLineState then
    RemoveNodeStateFromLine;
  FNodeState := AValue;
  if not ASkipLineState then
    ApplyNodeStateToLine;

  if FNodeState in [idnEnabled, idnTempEnabled] then
    Line.FLineFlags := Line.FLineFlags + [idlNotInCodeToUnknownReq, idlNotInCodeToUnknown];

  case NodeType of
    idnIfdef, idnElse, idnElseIf: begin
        if (ClosingPeer <> nil) then
          ClosingPeer.SetOpeningPeerNodeState(NodeState, NodeStateForPeer(ClosingPeer.NodeType))
      end;
    idnCommentedNode: Assert(AValue = idnUnknown, 'SetOpeningPeerNodeState for idnCommentedIfdef not possible. '+DebugText);
  end;
end;

function TSynMarkupHighIfDefEntry.GetPeer(APeerType: TSynMarkupIfdefPeerType): TSynMarkupHighIfDefEntry;
begin
  Result := FPeers[APeerType];
end;

procedure TSynMarkupHighIfDefEntry.SetPeer(APeerType: TSynMarkupIfdefPeerType;
  ANewPeer: TSynMarkupHighIfDefEntry);
begin
  assert( ((APeerType=idpOpeningPeer) and (NodeType <> idnIfdef)) OR ((APeerType=idpClosingPeer) and (NodeType <> idnEndIf)), 'Invalid peertype ('+dbgs(APeerType)+') for this node'+DebugText+' NEWNODE='+ANewPeer.DebugText(True));
  assert((ANewPeer=nil) OR
         ((APeerType=idpOpeningPeer) and (ANewPeer.NodeType <> idnEndIf)) OR ((APeerType=idpClosingPeer) and (ANewPeer.NodeType <> idnIfdef))
         , 'New peer not allowed for peertype ('+dbgs(APeerType)+')  Node:'+DebugText+' NEWNODE='+ANewPeer.DebugText(True));
  if FPeers[APeerType] = ANewPeer then begin
    assert((ANewPeer = nil) or (ANewPeer.GetPeer(ReversePeerType[APeerType]) = self), 'Peer does not point back to self. Node:'+DebugText+' NEWNODE='+ANewPeer.DebugText(True));
    assert((NodeType in [idnElse, idnElseIf]) or (FPeers[idpOpeningPeer] = nil) or (FPeers[idpClosingPeer] = nil), 'Only ELSE has 2 peers. Node:'+DebugText+' NEWNODE='+ANewPeer.DebugText(True));
    exit;
  end;

  ClearPeerField(APeerType);

  if ANewPeer = nil then begin
    FPeers[APeerType] := ANewPeer;

    if APeerType = idpOpeningPeer then
      SetOpeningPeerNodeState(idnUnknown, idnUnknown);
  end
  else begin
    // If new peer is part of another pair, disolve that pair. This may set FPeers[APeerType] = nil, if new pair points to this node
    assert(ANewPeer.GetPeer(ReversePeerType[APeerType]) <> self, 'New peer points to this, but was not known by this / link is not bidirectional. Node:'+DebugText+' NEWNODE='+ANewPeer.DebugText(True));
    ANewPeer.ClearPeerField(ReversePeerType[APeerType]);

    ANewPeer.FPeers[ReversePeerType[APeerType]] := Self;
    FPeers[APeerType] := ANewPeer;

    if APeerType = idpClosingPeer then
      ANewPeer.SetOpeningPeerNodeState(NodeState, NodeStateForPeer(ANewPeer.NodeType))
    else
      SetOpeningPeerNodeState(FPeers[APeerType].NodeState, FPeers[APeerType].NodeStateForPeer(NodeType));
  end;
  assert((NodeType in [idnElse, idnElseIf]) or (FPeers[idpOpeningPeer] = nil) or (FPeers[idpClosingPeer] = nil), 'Only ELSE has 2 peers. Node:'+DebugText+' NEWNODE='+ANewPeer.DebugText(True));
end;

procedure TSynMarkupHighIfDefEntry.MakeDisabled;
begin
  NodeState := idnDisabled;
end;

procedure TSynMarkupHighIfDefEntry.MakeEnabled;
begin
  NodeState := idnEnabled;
end;

procedure TSynMarkupHighIfDefEntry.MakeRequested;
begin
  NodeState := idnRequested;
end;

procedure TSynMarkupHighIfDefEntry.MakeUnknown;
begin
  NodeState := idnUnknown;
end;

function TSynMarkupHighIfDefEntry.IsTemp: Boolean;
begin
  Result := FNodeState in [idnTempEnabled, idnTempDisabled];
end;

function TSynMarkupHighIfDefEntry.HasTempOpening: Boolean;
begin
  Result := IsClosing and (FOpeningPeerNodeState in [idnTempEnabled, idnTempDisabled]);
end;

function TSynMarkupHighIfDefEntry.IsOpening: Boolean;
begin
  Result := (NodeType in [idnIfdef, idnElseIf, idnElse]);
end;

function TSynMarkupHighIfDefEntry.IsClosing: Boolean;
begin
  Result := (NodeType in [idnElse, idnElseIf, idnEndIf]);
end;

function TSynMarkupHighIfDefEntry.HasKnownState: Boolean;
begin
  Result := FNodeState in [idnEnabled, idnDisabled, idnTempEnabled, idnTempDisabled, idnInvalid];
end;

function TSynMarkupHighIfDefEntry.IsDisabledOpening: Boolean;
begin
  Result := IsOpening and IsDisabled;
end;

function TSynMarkupHighIfDefEntry.IsDisabledClosing: Boolean;
begin
  Result := IsClosing and HasDisabledOpening;
end;

procedure TSynMarkupHighIfDefEntry.ClearPeerField(APeerType: TSynMarkupIfdefPeerType);
begin
  if FPeers[APeerType] = nil then exit;
  assert(FPeers[APeerType].GetPeer(ReversePeerType[APeerType]) = self, 'ClearPeerField('+dbgs(APeerType)+'): Peer does not point back to self. '+DebugText);

  if APeerType = idpClosingPeer then
    FPeers[APeerType].SetOpeningPeerNodeState(idnUnknown, idnUnknown)
  else
    SetOpeningPeerNodeState(idnUnknown, idnUnknown);

  FPeers[APeerType].FPeers[ReversePeerType[APeerType]] := nil;
  FPeers[APeerType] := nil;
end;

procedure TSynMarkupHighIfDefEntry.ApplyNodeStateToLine(ARemove: Boolean = False);
var
  i: Integer;
begin
  if (FLine <> nil) then begin
    i := 1;
    if ARemove then i := -1;
    case NodeType of
      idnIfdef: begin
        if FNodeState in [idnDisabled, idnTempDisabled] then
          FLine.DisabledEntryOpenCount := FLine.DisabledEntryOpenCount + i;
        end;
      idnElse, idnElseIf: begin
          if FOpeningPeerNodeState in [idnDisabled, idnTempDisabled] then
            FLine.DisabledEntryCloseCount := FLine.DisabledEntryCloseCount + i;
          if FNodeState in [idnDisabled, idnTempDisabled] then
            FLine.DisabledEntryOpenCount := FLine.DisabledEntryOpenCount + i;
        end;
      idnEndIf: begin
          if FOpeningPeerNodeState in [idnDisabled, idnTempDisabled] then
            FLine.DisabledEntryCloseCount := FLine.DisabledEntryCloseCount + i;
        end;
    end;
  end;

  if not ARemove then begin
    if FNodeState = idnNotInCode then
      Include(FLine.FLineFlags, idlHasNodesNotInCode)
    else
    if NeedsRequesting then
      Include(FLine.FLineFlags, idlHasUnknownNodes);
  end;
end;

procedure TSynMarkupHighIfDefEntry.RemoveNodeStateFromLine;
begin
  ApplyNodeStateToLine(True);
end;

procedure TSynMarkupHighIfDefEntry.SetStartColumn(AValue: Integer);
begin
  if FStartColumn = AValue then Exit;
  FStartColumn := AValue;
  Assert(AValue>0, 'Startcol negative'+DebugText);
end;

procedure TSynMarkupHighIfDefEntry.SetStateByUser(AValue: Boolean);
begin
  if AValue then
    Include(FNodeFlags, idnStateByUser)
  else
    Exclude(FNodeFlags, idnStateByUser);
end;

procedure TSynMarkupHighIfDefEntry.SetNodeType(
  ANodeType: TSynMarkupIfdefNodeType);
begin
  RemoveNodeStateFromLine;
  FNodeType := ANodeType;
  ApplyNodeStateToLine;
end;

function TSynMarkupHighIfDefEntry.DebugText(Short: Boolean): String;
begin
  If Self = nil then
    exit('NODE IS NIL');
  Result := Format('Line=%s   NType=%s State=%s OpenState=%s Flags=%s ' +
                   ' StartCol=%d EndCol=%d',
                   [FLine.DebugText, dbgs(FNodeType), dbgs(FNodeState), dbgs(FOpeningPeerNodeState) ,
                    dbgs(FNodeFlags), FStartColumn, FEndColumn]
                  );
  if Short or (FPeers[idpOpeningPeer] = nil) then
    Result := Result + ' OpenPeer='+dbgs(FPeers[idpOpeningPeer])
  else
    Result := Result + ' OpenPeer='+FPeers[idpOpeningPeer].DebugText(True);

  if Short or (FPeers[idpClosingPeer] = nil) then
    Result := Result + ' ClosePeer='+dbgs(FPeers[idpClosingPeer])
  else
    Result := Result + ' OpenPeer='+FPeers[idpClosingPeer].DebugText(True);
end;

function TSynMarkupHighIfDefEntry.NodeStateForPeer(APeerType: TSynMarkupIfdefNodeType): TSynMarkupIfdefNodeStateEx;
const
  NodeStateMap: array [Boolean] of TSynMarkupIfdefNodeStateEx =
    (idnDisabled, idnEnabled); // False, True
  NodeStateTempMap: array [Boolean] of TSynMarkupIfdefNodeStateEx =
    (idnTempDisabled, idnTempEnabled); // False, True
begin
  Result := idnUnknown;
  Assert((NodeType <> APeerType) or (NodeType = idnElseIf), 'NodeStateForPeer: NodeType <> APeerType'+dbgs(APeerType)+' Node:'+DebugText);
  case NodeState of
    idnEnabled: begin
        case NodeType of
          idnIfdef:  Result := NodeStateMap[APeerType = idnEndIf]; // idnElse[if] will be idnDisabled;
          idnElseIf: Result := NodeStateMap[APeerType = idnEndIf]; // idnElse[if] will be idnDisabled;
          idnElse:   Result := NodeStateMap[APeerType = idnEndIf]; // idnIfdef will be idnDisabled;;
          idnEndIf:  Result := idnEnabled;
        end;
      end;
    idnDisabled: begin
        case NodeType of
          idnIfdef:  Result := NodeStateMap[APeerType <> idnEndIf];
          idnElseIf: Result := NodeStateMap[APeerType <> idnEndIf];
          idnElse:   Result := NodeStateMap[APeerType <> idnEndIf];
          idnEndIf:  Result := idnDisabled;
        end;
      end;
    idnTempEnabled: begin
        case NodeType of
          idnIfdef:  Result := NodeStateTempMap[APeerType = idnEndIf]; // idnElse[if] will be idnDisabled;
          idnElseIf: Result := NodeStateTempMap[APeerType = idnEndIf]; // idnElse[if] will be idnDisabled;
          idnElse:   Result := NodeStateTempMap[APeerType = idnEndIf]; // idnIfdef will be idnDisabled;;
          idnEndIf:  Result := idnTempEnabled;
        end;
      end;
    idnTempDisabled: begin
        case NodeType of
          idnIfdef:  Result := NodeStateTempMap[APeerType <> idnEndIf];
          idnElseIf: Result := NodeStateTempMap[APeerType <> idnEndIf];
          idnElse:   Result := NodeStateTempMap[APeerType <> idnEndIf];
          idnEndIf:  Result := idnTempDisabled;
        end;
      end;
  end;
end;

constructor TSynMarkupHighIfDefEntry.Create;
begin
  FNodeState := idnUnknown;
  FNodeFlags := [];
end;

destructor TSynMarkupHighIfDefEntry.Destroy;
begin
  if (FLine <> nil) and not(idlInGlobalClear in FLine.LineFlags) then begin
    NodeState := idnUnknown; //  RemoveNodeStateFromLine;
    ClearPeers;
  end;
  inherited Destroy;
end;

procedure TSynMarkupHighIfDefEntry.ClearPeers;
begin
  ClearPeerField(idpOpeningPeer);
  ClearPeerField(idpClosingPeer);
end;

procedure TSynMarkupHighIfDefEntry.ClearAll;
begin
  ClearPeers;
  RemoveNodeStateFromLine;
  FNodeFlags := [];
  ApplyNodeStateToLine;
end;

{ TSynMarkupHighIfDefLinesNode }

function TSynMarkupHighIfDefLinesNode.GetEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
begin
  Result := FEntries[AIndex];
end;

function TSynMarkupHighIfDefLinesNode.GetEntryCapacity: Integer;
begin
  Result := Length(FEntries);
end;

function TSynMarkupHighIfDefLinesNode.GetId: PtrUInt;
begin
  Result := PtrUInt(Self);
end;

procedure TSynMarkupHighIfDefLinesNode.SetEntry(AIndex: Integer;
  AValue: TSynMarkupHighIfDefEntry);
begin
  FEntries[AIndex] := AValue;
  if AValue <> nil then
    AValue.Line := Self;
end;

procedure TSynMarkupHighIfDefLinesNode.SetEntryCapacity(AValue: Integer);
var
  i, j: Integer;
begin
  j := Length(FEntries);
  if j = AValue then Exit;
  for i := j - 1 downto AValue do
    FreeAndNil(FEntries[i]);
  SetLength(FEntries, AValue);
  for i := j to AValue - 1 do
    FEntries[i] := nil;
end;

procedure TSynMarkupHighIfDefLinesNode.SetEntryCount(AValue: Integer);
begin
  if FEntryCount = AValue then Exit;
  FEntryCount := AValue;
  if EntryCapacity < FEntryCount then
    EntryCapacity := FEntryCount;
end;

procedure TSynMarkupHighIfDefLinesNode.AdjustPositionOffset(AnAdjustment: integer);
begin
  Assert((Successor = nil) or (GetPosition + AnAdjustment < Successor.GetPosition), 'GetPosition + AnAdjustment < Successor.GetPosition '+DebugText);
  Assert((Precessor = nil) or (GetPosition + AnAdjustment > Precessor.GetPosition), 'GetPosition + AnAdjustment > Precessor.GetPosition '+DebugText);
  FPositionOffset := FPositionOffset + AnAdjustment;
  if FLeft <> nil then
    TSynMarkupHighIfDefLinesNode(FLeft).FPositionOffset :=
      TSynMarkupHighIfDefLinesNode(FLeft).FPositionOffset - AnAdjustment;
  if FRight <> nil then
    TSynMarkupHighIfDefLinesNode(FRight).FPositionOffset :=
      TSynMarkupHighIfDefLinesNode(FRight).FPositionOffset - AnAdjustment;
end;

function TSynMarkupHighIfDefLinesNode.DebugText: String;
begin
  if self = nil then
    exit('NODE IN NIL');
  Result := Format('Pos=%d Flags=%s ECnt=%d LastEOffs=%d ScanEndOffs=%d ' +
                   ' DisEOpen=%d DisEClose=%d',
                   [GetPosition, dbgs(FLineFlags), FEntryCount, FLastEntryEndLineOffs,
                    FScanEndOffs, FDisabledEntryOpenCount, FDisabledEntryCloseCount
                   ]);
end;

constructor TSynMarkupHighIfDefLinesNode.Create;
begin
  FSize := 1; // used for index
  FScanEndOffs := 0;
end;

destructor TSynMarkupHighIfDefLinesNode.Destroy;
begin
  inherited Destroy;
  MakeDisposed;
end;

procedure TSynMarkupHighIfDefLinesNode.MakeDisposed; // Also called to clear, on real destroy
begin
  FLineFlags := [idlDisposed] + FLineFlags * [idlInGlobalClear];
  while EntryCount > 0 do
    DeletEntry(EntryCount-1, True);
  assert((idlInGlobalClear in LineFlags) or ((FDisabledEntryOpenCount =0) and (FDisabledEntryCloseCount = 0)), 'no close count left over'+DebugText);
  FDisabledEntryOpenCount := 0;
  FDisabledEntryCloseCount := 0;
end;

function TSynMarkupHighIfDefLinesNode.AddEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
var
  c: Integer;
begin
  c := EntryCount;
  EntryCount := c + 1;
  assert(FEntries[c]=nil, 'FEntries[c]=nil  Aindex='+IntToStr(AIndex)+' '+DebugText);
  Result := TSynMarkupHighIfDefEntry.Create;
  Result.Line := Self;
  if (AIndex >= 0) then begin
    Assert(AIndex <= c, 'Add node index ('+IntToStr(AIndex)+') <= count c='+IntToStr(c)+' '+DebugText);
    while c > AIndex do begin
      FEntries[c] := FEntries[c - 1];
      dec(c);
    end;
  end;
  FEntries[c] := Result;
end;

procedure TSynMarkupHighIfDefLinesNode.DeletEntry(AIndex: Integer; AFree: Boolean);
begin
  Assert((AIndex >= 0) and (AIndex < FEntryCount), 'DeletEntry Aindex='+IntToStr(AIndex)+' '+DebugText);
  if AFree then
    FEntries[AIndex].Free
  else
    FEntries[AIndex] := nil;
  while AIndex < FEntryCount - 1 do begin
    FEntries[AIndex] := FEntries[AIndex + 1];
    inc(AIndex);
  end;
  FEntries[AIndex] := nil;
  dec(FEntryCount);
end;

procedure TSynMarkupHighIfDefLinesNode.ReduceCapacity;
begin
  EntryCapacity := EntryCount;
end;

function TSynMarkupHighIfDefLinesNode.IndexOf(AEntry: TSynMarkupHighIfDefEntry): Integer;
begin
  Result := EntryCount - 1;
  while (Result >= 0) and (Entry[Result] <> AEntry) do
    dec(Result);
end;

{ TSynMarkupHighIfDefLinesNodeInfo }

procedure TSynMarkupHighIfDefLinesNodeInfo.SetStartLine(AValue: Integer);
begin
  Assert(FNode <> nil, 'TSynMarkupHighIfDefLinesNodeInfo.SetStartLine has node '+DebugText);
  if FStartLine = AValue then Exit;
  FNode.AdjustPositionOffset(AValue - FStartLine);
  FStartLine := AValue;
end;

function TSynMarkupHighIfDefLinesNodeInfo.DebugText: String;
begin
  Result := ' Startline='+IntToStr(FStartLine);
  if FNode <> nil then
    Result := Result + ' '+FNode.DebugText
  else
    Result := Result + ' Node is nil';
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetLineFlags: SynMarkupIfDefLineFlags;
begin
  if not HasNode then
    exit([]);
  Result := FNode.LineFlags;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetLastEntryEndLineOffs: Integer;
begin
  if not HasNode then
    exit(0);
  Result := FNode.LastEntryEndLineOffs;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetLastEntryEndLine: Integer;
begin
  if not HasNode then
    exit(0);
  Result := StartLine + FNode.LastEntryEndLineOffs;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.GetEntry'+DebugText);
  Result := FNode.Entry[AIndex];
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetEntryCount: Integer;
begin
  if not HasNode then
    exit(0);
  Result := FNode.EntryCount;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetScanEndLine: Integer;
begin
  if not IsValid then
    exit(StartLine);
  if ScanEndOffs >= 0 then
    Result := StartLine + ScanEndOffs
  else
    Result := StartLine;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetScanEndOffs: Integer;
begin
  if not HasNode then
    exit(0);
  Result := FNode.ScanEndOffs;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetEntry(AIndex: Integer;
  AValue: TSynMarkupHighIfDefEntry);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetEntry'+DebugText);
  FNode.Entry[AIndex] := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetEntryCount(AValue: Integer);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetEntryCount'+DebugText);
  FNode.EntryCount := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetLastEntryEndLineOffs(AValue: Integer);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetEndLineOffs'+DebugText);
  FNode.LastEntryEndLineOffs := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetScanEndLine(AValue: Integer);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetScanEndLine'+DebugText);
  ScanEndOffs := AValue - StartLine;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetScanEndOffs(AValue: Integer);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetScanEndOffs'+DebugText);
  FNode.ScanEndOffs := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.ClearInfo;
begin
  FStartLine := 0;
  Index      := 0;
  FNode := nil;
  FAtBOL := False;
  FAtEOL := False;
  ClearNestCache;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.InitForNode(ANode: TSynMarkupHighIfDefLinesNode;
  ALine: Integer);
begin
  ClearInfo;
  FNode := ANode;
  FStartLine := ALine;
end;

function TSynMarkupHighIfDefLinesNodeInfo.Precessor: TSynMarkupHighIfDefLinesNodeInfo;
begin
  ClearNestCache;
  Result.ClearNestCache;
  Result.FTree := FTree;
  If HasNode then begin
    Result.FStartLine := FStartLine;
    Result.Index      := Index;
    Result.FNode := TSynMarkupHighIfDefLinesNode(FNode.Precessor(Result.FStartLine, Result.Index));
    Result.FAtBOL := not Result.HasNode;
    Result.FAtEOL := False;
  end
  else
  if AtEOL then begin
    Result.FNode := TSynMarkupHighIfDefLinesNode(FTree.Last(Result.FStartLine, Result.Index));
    Result.FAtBOL := not Result.HasNode;
    Result.FAtEOL := False;
  end
  else begin
    Result.ClearInfo;
  end;
end;

function TSynMarkupHighIfDefLinesNodeInfo.Successor: TSynMarkupHighIfDefLinesNodeInfo;
begin
  ClearNestCache;
  Result.ClearNestCache;
  Result.FTree := FTree;
  If HasNode then begin
    Result.FStartLine := FStartLine;
    Result.Index      := Index;
    Result.FNode := TSynMarkupHighIfDefLinesNode(FNode.Successor(Result.FStartLine, Result.Index));
    Result.FAtBOL := False;
    Result.FAtEOL := not Result.HasNode;
  end
  else
  if FAtBOL then begin
    Result.FNode := TSynMarkupHighIfDefLinesNode(FTree.First(Result.FStartLine, Result.Index));
    Result.FAtBOL := False;
    Result.FAtEOL := not Result.HasNode;
  end
  else begin
    Result.ClearInfo;
  end;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.ClearNestCache;
begin
  FCacheNestMinimum := -1;
  FCacheNestStart   := -1;
  FCacheNestEnd     := -1;
end;

function TSynMarkupHighIfDefLinesNodeInfo.NestMinimumDepthAtNode: Integer;
begin
  assert(FTree <> nil, 'NestWinimumDepthAtNode has tree'+DebugText);
  if FCacheNestMinimum < 0 then
    FCacheNestMinimum :=
      FTree.GetHighLighterWithLines.FoldBlockMinLevel(ToIdx(StartLine), FOLDGROUP_IFDEF,
                                           [sfbIncludeDisabled]);
  Result := FCacheNestMinimum;
end;

function TSynMarkupHighIfDefLinesNodeInfo.NestDepthAtNodeStart: Integer;
begin
  assert(FTree <> nil, 'NestDepthAtNodeStart has tree'+DebugText);
  if FCacheNestStart < 0 then
    FCacheNestStart :=
      FTree.GetHighLighterWithLines.FoldBlockEndLevel(ToIdx(StartLine)-1, FOLDGROUP_IFDEF,
                                           [sfbIncludeDisabled]);
  Result := FCacheNestStart;
end;

function TSynMarkupHighIfDefLinesNodeInfo.NestDepthAtNodeEnd: Integer;
begin
  assert(FTree <> nil, 'NestDepthAtNodeEnd has tree'+DebugText);
  if FCacheNestEnd < 0 then
    FCacheNestEnd :=
      FTree.GetHighLighterWithLines.FoldBlockEndLevel(ToIdx(StartLine), FOLDGROUP_IFDEF,
                                           [sfbIncludeDisabled]);
  Result := FCacheNestEnd;
end;

function TSynMarkupHighIfDefLinesNodeInfo.ValidToLine(const ANextNode: TSynMarkupHighIfDefLinesNodeInfo): Integer;
begin
  if not HasNode then
    exit(-1);
  if not IsValid then
    exit(StartLine);

  if ScanEndOffs >= 0 then begin
    Result := StartLine + ScanEndOffs;
    assert((not ANextNode.HasNode) or (Result<ANextNode.StartLine), '(ANextNode=nil) or (Result<ANextNode.StartLine)'+DebugText);
  end
  else
  if ANextNode.HasNode then
    Result := ANextNode.StartLine - 1
  else
    Result := StartLine;
end;

function TSynMarkupHighIfDefLinesNodeInfo.IsValid: Boolean;
begin
  Result := HasNode and (idlValid in LineFlags);
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.Invalidate;
begin
  Exclude(Node.FLineFlags, idlValid);
end;

function TSynMarkupHighIfDefLinesNodeInfo.HasNode: Boolean;
begin
  Result := FNode <> nil;
end;

{ TSynMarkupHighIfDefLinesTree }

procedure TSynMarkupHighIfDefLinesTree.IncChangeStep;
begin
  if FChangeStep = high(FChangeStep) then
    FChangeStep := low(FChangeStep)
   else
    inc(FChangeStep);
end;

procedure TSynMarkupHighIfDefLinesTree.SetHighlighter(AValue: TSynPasSyn);
begin
  if FHighlighter = AValue then Exit;
  FHighlighter := AValue;
  Clear;
end;

procedure TSynMarkupHighIfDefLinesTree.SetLines(AValue: TSynEditStringsLinked);
begin
  if FLines = AValue then Exit;

  if FLines <> nil then begin
    FLines.RemoveChangeHandler(senrHighlightChanged, @DoHighlightChanged);
    FLines.RemoveEditHandler(@DoLinesEdited);
  end;

  FLines := AValue;
  Clear;

  if FLines <> nil then begin
    FLines.AddChangeHandler(senrHighlightChanged, @DoHighlightChanged);
    FLines.AddEditHandler(@DoLinesEdited);
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.MaybeRequestNodeStates(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
var
  e: TSynMarkupHighIfDefEntry;
  i: Integer;
  NewState: TSynMarkupIfdefNodeState;
begin
  if (not (idlHasUnknownNodes in ANode.LineFlags)) or
     (not Assigned(FOnNodeStateRequest)) or
     FRequestingNodeState
  then
    exit;
  Exclude(ANode.Node.FLineFlags, idlHasUnknownNodes);
  FRequestingNodeState := True;
  try
    i := 0;
    while i < ANode.EntryCount do begin
      // replace Sender in Markup object
      e := ANode.Entry[i];
      if e.NeedsRequesting then begin
        NewState := FOnNodeStateRequest(nil, ANode.StartLine, e.StartColumn, e.NodeState);
        if e.NodeState <> NewState then
          IncChangeStep;
        e.NodeState := NewState;
      end;
      inc(i);
    end;
  finally
    FRequestingNodeState := False;
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.MaybeValidateNode(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
begin
  // TODO: search first
  if not ANode.HasNode then
    exit;
  if (not ANode.IsValid) then begin
    //debugln(['Validating existing node ', ANode.StartLine, ' - ', ANode.ScanEndLine]);
    ScanLine(ANode.StartLine, ANode.FNode);
  end;
  MaybeRequestNodeStates(ANode);
end;

procedure TSynMarkupHighIfDefLinesTree.MaybeExtendNodeBackward(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
  AStopAtLine: Integer);
var
  Line: Integer;
begin
  Assert(ANode.HasNode, 'ANode.HasNode in MaybeExtendNodeDownwards'+ANode.DebugText+ ' Stopline='+IntToStr(AStopAtLine));
  MaybeValidateNode(ANode);
  if (ANode.EntryCount = 0) then begin
    // ANode is a Scan-Start-Marker and may be extended downto StartLine
    Line := ANode.StartLine;
    while Line > AStopAtLine do begin
      dec(Line);
      if CheckLineForNodes(Line) then begin
        ScanLine(Line, ANode.FNode);
        if ANode.EntryCount > 0 then begin
          MaybeRequestNodeStates(ANode);
          break;
        end;
      end;
    end;
    if ANode.StartLine <> Line then begin
      //debugln(['EXTEND BACK node ', ANode.StartLine, ' - ', ANode.ScanEndLine, ' TO ', Line]);
      ANode.StartLine := Line;
    end;
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.MaybeExtendNodeForward(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
  var ANextNode: TSynMarkupHighIfDefLinesNodeInfo; AStopBeforeLine: Integer);
var
  Line: Integer;
begin
  ANextNode.ClearInfo;
  Assert(ANode.IsValid, 'ANode.IsValid in MaybeExtendNodeForward');
  if AStopBeforeLine < 0 then AStopBeforeLine := Lines.Count + 1;
    // ANode is a Scan-Start-Marker and may be extended downto StartLine
    Line := ANode.StartLine + ANode.ScanEndOffs;
    while Line < AStopBeforeLine - 1 do begin
      inc(Line);
      if CheckLineForNodes(Line) then begin
        ScanLine(Line, ANextNode.FNode);
        if ANextNode.HasNode then begin
          ANextNode.FStartLine := Line;  // directly to field
          ANode.ScanEndLine := Line - 1;
          MaybeRequestNodeStates(ANextNode);
          exit;
        end;
      end;
    end;
    // Line is empty, include in offs
    if ANode.ScanEndLine <> Line then begin
      //debugln(['EXTEND FORWARD node ', ANode.StartLine, ' - ', ANode.ScanEndLine, ' TO ', Line]);
      ANode.ScanEndLine := Line;
    end;
end;

function TSynMarkupHighIfDefLinesTree.GetOrInsertNodeAtLine(ALinePos: Integer): TSynMarkupHighIfDefLinesNodeInfo;
begin
  Result := FindNodeAtPosition(ALinePos, afmPrev); // might be multiline
  if (not Result.HasNode) then begin
    Result := FindNodeAtPosition(ALinePos, afmCreate);
  end
  else
  if (Result.StartLine <> ALinePos) then begin
    if Result.ScanEndLine >= ALinePos then
      Result.ScanEndLine := ALinePos - 1;
    if Result.LastEntryEndLine > ALinePos then begin
      Result.LastEntryEndLineOffs := 0;
      Assert(Result.EntryCount > 0, 'Result.EntryCount > 0 : in Outer Nesting');
      Result.Entry[Result.EntryCount-1].MakeUnknown;
      //Result.Entry[Result.EntryCount].MakeUnknown;
      //Result.; xxxxxxxxxxxxxxxxxxxxxxxx TODO invalidate
    end;
    Result := FindNodeAtPosition(ALinePos, afmCreate);
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.ConnectPeers(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
  var ANestList: TSynMarkupHighIfDefLinesNodeInfoList; AOuterLines: TLazSynEditNestedFoldsList);
var
  PeerList: array of TSynMarkupHighIfDefEntry; // List of Else/Endif in the current line, that where opened in a previous line
  OpenList: array of TSynMarkupHighIfDefEntry; // List of IfDef/Else in the current line
  CurDepth, MaxListIdx, MinOpenDepth, MaxOpenDepth, MaxPeerDepth, MinPeerDepth: Integer;
  i, j, OtherDepth: Integer;
  OtherLine: TSynMarkupHighIfDefLinesNodeInfo;
  PeerChanged: Boolean;
  CurEntry: TSynMarkupHighIfDefEntry;

  function OpenIdx(AIdx: Integer): Integer; // correct negative idx
  begin
    Result := AIdx;
    if Result >= 0 then exit;
    Result := MaxListIdx + (-AIdx);
  end;

begin
  /// Scan for onel line blocks
  PeerChanged := False;
  CurDepth := ANode.NestDepthAtNodeStart;
  MinOpenDepth := MaxInt;
  MaxOpenDepth := -1;
  MinPeerDepth := MaxInt;
  MaxPeerDepth := -1;

  MaxListIdx := CurDepth + ANode.EntryCount + 1;
  SetLength(OpenList, MaxListIdx + ANode.EntryCount);
  SetLength(PeerList, MaxListIdx);

  for i := 0 to ANode.EntryCount - 1 do begin
    CurEntry := ANode.Entry[i];
    case CurEntry.NodeType of
      idnIfdef: begin
          inc(CurDepth);
          OpenList[OpenIdx(CurDepth)] := CurEntry; // Store IfDef, with Index at end of IfDef (inside block)
          if CurDepth < MinOpenDepth then MinOpenDepth := CurDepth;
          if CurDepth > MaxOpenDepth then MaxOpenDepth := CurDepth;
        end;
      idnElse, idnElseIf: begin
          If CurDepth <= 0 then begin
            //debugln(['Ignoring node with has no opening at all in line ', ANode.StartLine]);
          end;

          if (CurDepth >= MinOpenDepth) and (CurDepth <= MaxOpenDepth) then begin
            // Opening Node on this line
            assert(CurDepth = MaxOpenDepth, 'ConnectPeers: Same line peer skips opening node(s)');
            case OpenList[OpenIdx(CurDepth)].NodeType of
              idnIfdef, idnElseIf:
                if OpenList[OpenIdx(CurDepth)].ClosingPeer <> CurEntry then begin
                  //Debugln(['New Peer for ',dbgs(OpenList[OpenIdx(CurDepth)].NodeType), ' to else same line']);
                  OpenList[OpenIdx(CurDepth)].ClosingPeer := CurEntry;
                  PeerChanged := True;
                  //dec(MaxOpenDepth); // Will be set with the current entry
                end;
              idnElse: ;//DebugLn('Ignoring invalid double else (on same line)');
            end;
          end
          else
          If CurDepth >= 0 then begin
            // Opening Node in previous line
            PeerList[CurDepth] := CurEntry;
            assert((MaxPeerDepth=-1) or ((MinPeerDepth <= MaxPeerDepth) and (CurDepth = MinPeerDepth-1)), 'ConnectPeers: skipped noeds during line scan');
            if CurDepth < MinPeerDepth then MinPeerDepth := CurDepth;
            if CurDepth > MaxPeerDepth then MaxPeerDepth := CurDepth;
          end;

          OpenList[OpenIdx(CurDepth)] := CurEntry; // Store IfDef, with Index at end of IfDef (inside block)
          if CurDepth < MinOpenDepth then MinOpenDepth := CurDepth;
          if CurDepth > MaxOpenDepth then MaxOpenDepth := CurDepth;
        end;
      idnEndIf: begin
          If CurDepth <= 0 then begin
            //debugln(['Ignoring node with has no opening at all in line', ANode.StartLine]);
            dec(CurDepth);
            continue;  // This node has no opening node
          end;

          if (CurDepth >= MinOpenDepth) and (CurDepth <= MaxOpenDepth) then begin
            // Opening Node on this line
            assert(CurDepth = MaxOpenDepth, 'ConnectPeers: Same line peer skips opening node(s)');
            if OpenList[OpenIdx(CurDepth)].ClosingPeer <> CurEntry then begin
              //Debugln(['New Peer for ',dbgs(OpenList[OpenIdx(CurDepth)].NodeType), ' to endif same line']);
              OpenList[OpenIdx(CurDepth)].ClosingPeer := CurEntry;
              PeerChanged := True;
            end;
            dec(MaxOpenDepth);
          end
          else begin
            // Opening Node in previous line
            PeerList[CurDepth] := CurEntry;
            assert((MaxPeerDepth=-1) or ((MinPeerDepth <= MaxPeerDepth) and (CurDepth = MinPeerDepth-1)), 'ConnectPeers: skipped noeds during line scan');
            if CurDepth < MinPeerDepth then MinPeerDepth := CurDepth;
            if CurDepth > MaxPeerDepth then MaxPeerDepth := CurDepth;
          end;

          dec(CurDepth);
        end;
    end;
  end;



  // Find peers in previous lines. MinPeerDepth <= 0 have no opening
  // Opening (IfDef) nodes will be connected when there closing node is found.
  for i := MaxPeerDepth downto Max(MinPeerDepth, 1) do begin
    // Todo: MAybe optimize, if it can be known that an existing peer link is correct
    //case PeerList[i].NodeType of
    //  idnElse:  if PeerList[i].IfDefPeer <> nil then continue;
    //  idnEndIf: if PeerList[i].ElsePeer  <> nil then continue;
    //end;

    assert(not (PeerList[i].NodeType in [idnIfdef, idnCommentedNode]), 'multi-line peer valid');

    // AOuterLines is only set while scanning opening lines in front of the 1st visible screenline
    //   (at the start of ValidateRange)
    if (AOuterLines <> nil) then begin
      if PeerList[i].NodeType in [idnElse, idnElseIf] then begin
        // todo: find multiply elseif
        j := ToPos(AOuterLines.NodeLineEx[i-1, 1]);
        if j < 0 then begin
          //debugln(['Skipping peer for ELSE with NO IFDEF at depth ', i-1, ' before line ', ANode.StartLine]);
          continue;
        end;
        OtherLine := GetOrInsertNodeAtLine(j);
        MaybeValidateNode(OtherLine);
      end
      else
        continue; // while scanning outerlines, any EndIf can be ignored
    end
    else
      OtherLine := ANestList.Node[i]; // Todo: keep if same al last loop, and continue at OtherDepth / j

    OtherDepth := OtherLine.NestDepthAtNodeEnd;
    j := OtherLine.EntryCount;
    while j > 0 do begin
      dec(j);
      if OtherDepth = i then begin
        case OtherLine.Entry[j].NodeType of
          idnIfdef: begin
              assert(PeerList[i].NodeType in [idnElse, idnElseIf, idnEndIf], 'PeerList[i].NodeType in [idnElse, idnEndIf] for other ifdef');
              if PeerList[i].OpeningPeer <> OtherLine.Entry[j] then begin
                //Debugln(['New Peer for ',dbgs(PeerList[i].NodeType), ' to ifdef other line']);
                PeerList[i].OpeningPeer := OtherLine.Entry[j];
                PeerChanged := True;
              end;
              j := -1;
              break;
            end;
          idnElse, idnElseIf: begin
              assert(PeerList[i].NodeType in [idnElse, idnElseIf, idnEndIf], 'PeerList[i].NodeType in [idnElse, idnEndIf] for other else');
              if (PeerList[i].NodeType = idnEndIf) OR
                 ( (PeerList[i].NodeType in [idnElseIf, idnElse]) and
                   (OtherLine.Entry[j].NodeType = idnElseIf) )
              then begin
                if PeerList[i].OpeningPeer <> OtherLine.Entry[j] then begin
                  //Debugln(['New Peer for ',dbgs(PeerList[i].NodeType), ' to else other line']);
                  PeerList[i].OpeningPeer := OtherLine.Entry[j];
                  PeerChanged := True;
                end;
                j := -1;
              end
              else begin
                //DebugLn('Ignoring invalid double else');
              end;
              break;
            end;
        end;
      end;
      case OtherLine.Entry[j].NodeType of
        idnIfdef: dec(OtherDepth);
        idnElse, idnElseIf:  ; //
        idnEndIf: inc(OtherDepth);
      end;
    end;

    if j >= 0 then begin
      // no peer found
      case PeerList[i].NodeType of
        idnIfdef: ;
        idnElse, idnElseIf:  begin
            //Debugln(['CLEARING ifdef Peer for ',dbgs(PeerList[i].NodeType)]);
            PeerList[i].OpeningPeer := nil;
            PeerChanged := True;
            //DoModified;
          end;
        idnEndIf: begin
            //Debugln(['CLEARING BOTH Peer for ',dbgs(PeerList[i].NodeType)]);
            PeerList[i].ClearPeers;
            PeerChanged := True;
            //DoModified;
          end;
      end;
    end;

  end;

  if PeerChanged then
    IncChangeStep;
end;

procedure TSynMarkupHighIfDefLinesTree.DoLinesEdited(Sender: TSynEditStrings; aLinePos,
  aBytePos, aCount, aLineBrkCnt: Integer; aText: String);

  function IndexOfEntryAfter(ANode: TSynMarkupHighIfDefLinesNode; AXPos: Integer): Integer;
  var
    Cnt: Integer;
  begin
    Result := 0;
    Cnt := aNode.EntryCount;
    while (Result < Cnt) and (ANode.Entry[Result].StartColumn < aBytePos) do
      inc(Result);
    // if LastEntryEndLineOffs = 0, then keep at count. Migth check closing pos of (Result-1)
    if (Result = Cnt) and (ANode.LastEntryEndLineOffs > 0) then
      Result := -1;
  end;

  procedure AdjustEntryXPos(ANode: TSynMarkupHighIfDefLinesNode; ADiffX: Integer;
    AStartIdx: Integer = 0; ADestNode: TSynMarkupHighIfDefLinesNode = nil;
    AMinXPos : Integer = 1);
  var
    Cnt, SkipEndIdx, DestPos, j: Integer;
    CurEntry: TSynMarkupHighIfDefEntry;
  begin
    Cnt := aNode.EntryCount;
    if AStartIdx >= Cnt then
      exit;
    if (aNode.LastEntryEndLineOffs > 0) then
      SkipEndIdx := Cnt - 1
    else
      SkipEndIdx := -1;

    if ADestNode = nil then begin
      for j := AStartIdx to Cnt - 1 do begin
        CurEntry := aNode.Entry[j];
        assert(CurEntry.StartColumn >= AMinXPos, 'DoLinesEdited: CurEntry.StartColumn >= AMinXPos');
        CurEntry.StartColumn := Max(AMinXPos, CurEntry.StartColumn + ADiffX);
        if (j <> SkipEndIdx) then
          CurEntry.EndColumn := Max(CurEntry.StartColumn, CurEntry.EndColumn + ADiffX);
      end;
    end
    else begin
      DestPos := ADestNode.EntryCount;
      ADestNode.EntryCount := DestPos + (Cnt - AStartIdx);
      for j := AStartIdx to Cnt - 1 do begin
        CurEntry := aNode.Entry[j];
        aNode.Entry[j] := nil;
        assert(CurEntry.StartColumn >= AMinXPos, 'DoLinesEdited: CurEntry.StartColumn >= AMinXPos');
        CurEntry.StartColumn := Max(AMinXPos, CurEntry.StartColumn + ADiffX);
        if (j <> SkipEndIdx) then
          CurEntry.EndColumn := Max(CurEntry.StartColumn, CurEntry.EndColumn + ADiffX);
        ADestNode.Entry[DestPos] := CurEntry;
        inc(DestPos);
      end;
      ANode.EntryCount := AStartIdx;
      ADestNode.LastEntryEndLineOffs := ANode.LastEntryEndLineOffs;
      ANode.LastEntryEndLineOffs := 0;
    end;
  end;

var
  i, c: Integer;
  WorkNode, NextNode, LinePosNode: TSynMarkupHighIfDefLinesNodeInfo;
  WorkLine, LineAfterDelete: Integer;

begin
  // Line nodes vill be invalidated in DoHighlightChanged
  {$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
  DebugCurTreeForAssert := self; try
  {$ENDIF}

  IncChangeStep;

  if aLineBrkCnt > 0 then begin
    WorkNode := FindNodeAtPosition(aLinePos - 1, afmPrev);
    if WorkNode.HasNode then begin
      WorkLine := WorkNode.StartLine;
      if aLinePos <= WorkNode.LastEntryEndLine then begin
        c := WorkNode.EntryCount - 1;
        assert(c >= 0, 'must have node, if LastEntryOffs > 0 [insert lines]');
        if (aLinePos = WorkNode.LastEntryEndLine) then begin
          if (WorkNode.Entry[c].EndColumn > aBytePos) then begin
            WorkNode.LastEntryEndLineOffs := WorkNode.LastEntryEndLineOffs + aLineBrkCnt;
            WorkNode.Entry[c].EndColumn := WorkNode.Entry[c].EndColumn - aBytePos + 1;
          end;
        end
        else
          WorkNode.LastEntryEndLineOffs := WorkNode.LastEntryEndLineOffs + aLineBrkCnt;
        WorkNode.Entry[c].MakeUnknown;
      end;
      if aLinePos <= WorkLine + WorkNode.ScanEndOffs then begin
        assert(aLinePos > WorkLine);
        WorkNode.ScanEndOffs := aLinePos - WorkLine;
      end;
    end;

    WorkNode := WorkNode.Successor;
    if (not WorkNode.HasNode) then
      exit;
    WorkLine := WorkNode.StartLine;
    NextNode.ClearInfo;

    if (WorkLine > aLinePos) or
       ( (WorkLine = aLinePos) and
         ( (aBytePos <= 1) or (WorkNode.EntryCount = 0) or (WorkNode.Entry[0].StartColumn >= aBytePos) )
       )
    then begin
      // Move Entire Line
      AdjustForLinesInserted(aLinePos, aLineBrkCnt);
      // Adjust the FIELD directly (the real node already moved)
      WorkNode.FStartLine := WorkNode.FStartLine + aLineBrkCnt;
      if (aBytePos > 1) and (WorkLine = aLinePos) then
        AdjustEntryXPos(WorkNode.Node, (-aBytePos) + 1);
    end
    else begin
      // Move part of Line (or nothing)
      AdjustForLinesInserted(aLinePos + 1, aLineBrkCnt);
      if (WorkLine = aLinePos) then begin
        i := IndexOfEntryAfter(WorkNode.Node, aBytePos);
        if i >= 0 then begin
          NextNode := FindNodeAtPosition(aLinePos + aLineBrkCnt, afmCreate);
          AdjustEntryXPos(WorkNode.Node, (-aBytePos) + 1, i, NextNode.Node);
          if (i > 0) and (WorkNode.Entry[i-1].EndColumn > aBytePos) then begin
            WorkNode.LastEntryEndLineOffs := aLineBrkCnt;
            WorkNode.Entry[i-1].EndColumn := WorkNode.Entry[i-1].EndColumn - aBytePos + 1;
            WorkNode.Entry[i-1].MakeUnknown;
          end;
        end
        else begin
          assert(WorkNode.LastEntryEndLineOffs > 0, 'WorkNode.LastEntryEndLineOffs > 0');
          c := WorkNode.EntryCount - 1;
          if (c >= 0) and (WorkNode.Entry[c].StartColumn < aBytePos) then begin
            WorkNode.LastEntryEndLineOffs := WorkNode.LastEntryEndLineOffs + aLineBrkCnt;
            WorkNode.Entry[c].MakeUnknown;
          end;
        end;
      end
      else begin
        if aLinePos <= WorkNode.LastEntryEndLine then begin
          c := WorkNode.EntryCount - 1;
          assert(c >= 0, 'must have node, if LastEntryOffs > 0 [insert lines]');
          if (aLinePos = WorkNode.LastEntryEndLine) then begin
            if (WorkNode.Entry[c].EndColumn > aBytePos) then begin
              WorkNode.LastEntryEndLineOffs := WorkNode.LastEntryEndLineOffs + aLineBrkCnt;
              WorkNode.Entry[c].EndColumn := WorkNode.Entry[c].EndColumn - aBytePos + 1;
            end;
          end
          else
            WorkNode.LastEntryEndLineOffs := WorkNode.LastEntryEndLineOffs + aLineBrkCnt;
          WorkNode.Entry[c].MakeUnknown;
        end;
        if aLinePos <= WorkLine + WorkNode.ScanEndOffs then begin
          assert(aLinePos > WorkLine);
          WorkNode.ScanEndOffs := aLinePos - WorkLine;
        end;
      end;
    end;
  end

  else
  if aLineBrkCnt < 0 then begin
    aLineBrkCnt := - aLineBrkCnt;
    WorkNode := FindNodeAtPosition(aLinePos - 1, afmPrev);
    if WorkNode.HasNode then begin
      WorkLine := WorkNode.StartLine;
      if aLinePos <= WorkNode.LastEntryEndLine then begin
        c := WorkNode.EntryCount - 1;
        assert(c >= 0, 'must have node, if LastEntryOffs > 0 [insert lines]');
        if (aLinePos < WorkNode.LastEntryEndLine) or (WorkNode.Entry[c].EndColumn > aBytePos) then begin
          if aLinePos + aLineBrkCnt > WorkNode.LastEntryEndLine then
            WorkNode.Entry[c].EndColumn := 1
          else
          if (aLinePos + aLineBrkCnt = WorkNode.LastEntryEndLine) then
            WorkNode.Entry[c].EndColumn := WorkNode.Entry[c].EndColumn + aBytePos - 1;
          WorkNode.LastEntryEndLineOffs := WorkNode.LastEntryEndLineOffs - aLineBrkCnt;
          WorkNode.Entry[c].MakeUnknown;
        end;
      end;
      if aLinePos <= WorkLine + WorkNode.ScanEndOffs then begin
        assert(aLinePos > WorkLine);
        WorkNode.ScanEndOffs := aLinePos - WorkLine;
      end;
    end;

    WorkNode := WorkNode.Successor;
    if (not WorkNode.HasNode) then
      exit;
    WorkLine := WorkNode.StartLine;
    NextNode.ClearInfo;

    LinePosNode.ClearInfo;
    if (WorkNode.StartLine = aLinePos) then begin
      LinePosNode := WorkNode;
      WorkNode := WorkNode.Successor;
    end;
    LineAfterDelete := aLinePos + aLineBrkCnt;
    while (WorkNode.HasNode) and (WorkNode.StartLine < LineAfterDelete) do begin
      NextNode := WorkNode.Successor;
      RemoveLine(WorkNode.FNode);
      WorkNode := NextNode;
    end;

    if LinePosNode.HasNode and (LinePosNode.LastEntryEndLineOffs > 0) then begin
      c := LinePosNode.EntryCount - 1;
      if LinePosNode.LastEntryEndLineOffs < aLineBrkCnt then
        LinePosNode.Entry[c].EndColumn := aBytePos //  no known end
      else
      if LinePosNode.LastEntryEndLineOffs = aLineBrkCnt then
        LinePosNode.Entry[c].EndColumn := LinePosNode.Entry[c].EndColumn + aBytePos - 1;
      LinePosNode.LastEntryEndLineOffs := Max(0, LinePosNode.LastEntryEndLineOffs - aLineBrkCnt);
    end;
    if (WorkNode.StartLine = LineAfterDelete) then begin
      if LinePosNode.HasNode then begin
        AdjustEntryXPos(WorkNode.Node, aBytePos - 1, 0, LinePosNode.Node);
        RemoveLine(WorkNode.FNode);
        WorkNode.ClearInfo;
      end
      else begin
        AdjustEntryXPos(WorkNode.Node, aBytePos - 1);
        WorkNode.StartLine := aLinePos;
      end;
    end;

    AdjustForLinesDeleted(aLinePos + 1, aLineBrkCnt);
  end

  else begin
    WorkNode := FindNodeAtPosition(aLinePos, afmPrev);
    if (not WorkNode.HasNode) then
      exit;
    WorkLine := WorkNode.StartLine;

    if aLinePos = WorkLine then begin
      i := IndexOfEntryAfter(WorkNode.Node, aBytePos);
      if i >= 0 then begin
        AdjustEntryXPos(WorkNode.Node, aCount, i, nil, aBytePos);
        if (i > 0) and (WorkNode.Entry[i-1].EndColumn > aBytePos) then begin
          WorkNode.Entry[i-1].EndColumn := Max(aBytePos, WorkNode.Entry[i-1].EndColumn + aCount);
          WorkNode.Entry[i-1].MakeUnknown;
        end;
        if aCount < 0 then begin
          c := WorkNode.EntryCount - 1;
          while ( (i < c) or ((i = c) and (WorkNode.LastEntryEndLineOffs=0)) ) and
                (WorkNode.Entry[i].EndColumn <= WorkNode.Entry[i].StartColumn)
          do begin
            WorkNode.Node.DeletEntry(i, True);
            dec(c);
          end;
          if (i <= c) and (WorkNode.Entry[i].StartColumn < aBytePos) then begin
            WorkNode.Entry[i].StartColumn := aBytePos;
            WorkNode.Entry[i].MakeUnknown;
          end;
        end;
      end;

      WorkNode := WorkNode.Precessor;
      if (not WorkNode.HasNode) then
        exit;
    end;

    assert(WorkNode.StartLine < aLinePos);
    WorkLine := WorkNode.StartLine + WorkNode.ScanEndOffs;
    if (WorkLine >= aLinePos) then
      WorkNode.ScanEndOffs := aLinePos - 1 - WorkNode.StartLine;

    WorkLine := WorkNode.StartLine + WorkNode.LastEntryEndLineOffs;
    i := WorkNode.EntryCount - 1;
    if (WorkLine >= aLinePos) and (i >= 0) then begin
      if (WorkLine = aLinePos) and (WorkNode.Entry[i].EndColumn > aBytePos) then
        WorkNode.Entry[i].EndColumn := Max(aBytePos, WorkNode.Entry[i].EndColumn + aCount);
      WorkNode.Entry[i].MakeUnknown;
    end;

  end;

  {$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
  finally DebugCurTreeForAssert := nil; end;
  {$ENDIF}
end;

procedure TSynMarkupHighIfDefLinesTree.DoHighlightChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
var
  LinePos: Integer;
  WorkNode: TSynMarkupHighIfDefLinesNodeInfo;
begin
  IncChangeStep;
  // Invalidate. The highlighter should only run once, so no need to collect multply calls
  LinePos := ToPos(AIndex);
  WorkNode := FindNodeAtPosition(LinePos, afmPrev);
  if WorkNode.HasNode and (Max(WorkNode.ScanEndLine, WorkNode.LastEntryEndLine) >= LinePos) then
    WorkNode.Invalidate;
  WorkNode := WorkNode.Successor;
  LinePos := LinePos + ACount;
  while WorkNode.HasNode and (WorkNode.StartLine <= LinePos) do begin
    WorkNode.Invalidate;
    WorkNode := WorkNode.Successor;
  end;
end;

function TSynMarkupHighIfDefLinesTree.GetHighLighterWithLines: TSynCustomFoldHighlighter;
begin
  Result := FHighlighter;
  if (Result = nil) then
    exit;
  Result.CurrentLines := FLines;
end;

function TSynMarkupHighIfDefLinesTree.GetHighlighterAttriAtRowColEx(XY: TPoint; out
  Token: string; out TokenType, Start: Integer; out Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: integer;
  Line: string;
begin
  PosY := XY.Y -1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < FLines.Count) then
  begin
    Line := FLines[PosY];
    fHighlighter.CurrentLines := FLines;
    Highlighter.StartAtLineIndex(PosY);
    PosX := XY.X;
    if (PosX > 0) and (PosX <= Length(Line)) then begin
      while not Highlighter.GetEol do begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + Length(Token)) then begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := TRUE;
          exit;
        end;
        Highlighter.Next;
      end;
    end;
  end;
  Token := '';
  Attri := nil;
  TokenType := -1;
  Result := FALSE;
end;

function TSynMarkupHighIfDefLinesTree.CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode;
begin
  IncChangeStep;
  if FDisposedNodes <> nil then begin
    Result := FDisposedNodes;
    FDisposedNodes := TSynMarkupHighIfDefLinesNode(Result).NextDispose;
    TSynMarkupHighIfDefLinesNode(Result).FLineFlags := [];
  end
  else
    Result := TSynMarkupHighIfDefLinesNode.Create;
end;

procedure TSynMarkupHighIfDefLinesTree.DisposeNode(var ANode: TSynSizedDifferentialAVLNode);
begin
  IncChangeStep;
  if FClearing then begin
    Include(TSynMarkupHighIfDefLinesNode(ANode).FLineFlags, idlInGlobalClear);
    inherited DisposeNode(ANode);
  end
  else begin
    TSynMarkupHighIfDefLinesNode(ANode).MakeDisposed;
    TSynMarkupHighIfDefLinesNode(ANode).NextDispose := FDisposedNodes;
    FDisposedNodes := TSynMarkupHighIfDefLinesNode(ANode);
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.RemoveLine(var ANode: TSynMarkupHighIfDefLinesNode);
begin
  RemoveNode(TSynSizedDifferentialAVLNode(ANode));
  DisposeNode(TSynSizedDifferentialAVLNode(ANode));
end;

constructor TSynMarkupHighIfDefLinesTree.Create;
var
  i: TSynMarkupHighIfDefTreeNotifications;
begin
  inherited Create;
  for i := low(TSynMarkupHighIfDefTreeNotifications) to high(TSynMarkupHighIfDefTreeNotifications) do
    FNotifyLists[i] := TMethodList.Create;

  FRequestingNodeState := False;
  MaybeCreateDict;
  TheDict.AddReference;
  FChangeStep := 0;
end;

destructor TSynMarkupHighIfDefLinesTree.Destroy;
var
  i: TSynMarkupHighIfDefTreeNotifications;
begin
  Lines := nil;
  inherited Destroy;
  TheDict.ReleaseReference;
  for i := low(TSynMarkupHighIfDefTreeNotifications) to high(TSynMarkupHighIfDefTreeNotifications) do
    FreeAndNil(FNotifyLists[i]);
end;

function TSynMarkupHighIfDefLinesTree.CreateOpeningList: TLazSynEditNestedFoldsList;
begin
  Result := TLazSynEditNestedFoldsList.Create(FLines, FHighlighter);
  Result.ResetFilter;
  Result.Clear;
  //Result.Line :=
  Result.FoldGroup := FOLDGROUP_IFDEF;
  Result.FoldFlags := [sfbIncludeDisabled];
  Result.IncludeOpeningOnLine := False;
end;

procedure TSynMarkupHighIfDefLinesTree.DiscardOpeningList(AList: TLazSynEditNestedFoldsList);
begin
  AList.Free;
end;

function TSynMarkupHighIfDefLinesTree.CheckLineForNodes(ALine: Integer): Boolean;
var
  m, e: Integer;
  LineText, LineTextLower: String;
  h: TSynCustomFoldHighlighter;
begin
  h := GetHighLighterWithLines;
  m := h.FoldBlockMinLevel(ToIdx(ALine), FOLDGROUP_IFDEF, [sfbIncludeDisabled]);
  e := h.FoldBlockEndLevel(ToIdx(ALine), FOLDGROUP_IFDEF, [sfbIncludeDisabled]);
  Result := (m < e);

  if not Result then begin
    LineText := Lines[ToIdx(ALine)];
    if LineText = '' then
      exit;
    LineTextLower := LowerCase(LineText);
    Result := TheDict.Dict.Search(@LineTextLower[1], Length(LineTextLower), nil) <> nil;
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.ScanLine(ALine: Integer;
  var ANodeForLine: TSynMarkupHighIfDefLinesNode; ACheckOverlapOnCreateLine: Boolean);
var
  FoldNodeInfoList: TLazSynFoldNodeInfoList;
  LineTextLower: String;
  LineLen, NodesAddedCnt: Integer;
  LineNeedsReq, LineChanged, HasUncommentedNodes: Boolean;

  function FindCloseCurlyBracket(StartX: Integer; out ALineOffs: Integer;
    ASearchComment: Boolean = False; AMaxLine: Integer = -1): Integer;
  var
    len, i, c, tk, x: Integer;
    dummy: String;
    attr: TSynHighlighterAttributes;
    r: TSynCustomHighlighterRange;
    f: TRangeStates;
  begin
    Result := StartX;
    ALineOffs := 0;

    c := Lines.Count;
    i := ALine;

    while (i <= c) do begin
      len := Length(Lines[ToIdx(i)]);
      if len > 0 then begin
        GetHighlighterAttriAtRowColEx(Point(StartX, i), dummy, tk, x, attr);
        Result := x + Length(dummy) - 1;
        if Result < len - 1 then
          exit;
        r := TSynCustomHighlighterRange(Highlighter.GetRange);
        f := TRangeStates(Integer(PtrUInt(r.RangeType)));
        if ASearchComment then begin
          if (f * [rsAnsi, rsBor] = []) or ( (AMaxLine > 0) and (i > AMaxLine) ) then
            exit;
        end
        else
        if not (rsDirective in f) then
          exit;
      end;

      inc(i);
      inc(ALineOffs);
      StartX := 1;
    end;

    Result := -1;
    exit;
  end;

  function IsCommentedIfDef(AEntry: TSynMarkupHighIfDefEntry): Boolean;
  var
    i, j, k, FoundOffs, LastLineOffs: Integer;
    s: String;
  begin
    i := AEntry.StartColumn;
    Result :=
       (i <= length(LineTextLower)) and
       (LineTextLower[i] = '{') and
       ( (AEntry.NodeType in [idnIfdef, idnElseIf, idnCommentedNode]) or
         (AEntry.StateByUser)
       ) and
       (AEntry.HasKnownState);
    if not Result then
      exit;

    j := AEntry.EndColumn;
    LastLineOffs := AEntry.Line.LastEntryEndLineOffs;
    if (idnMultiLineTag in AEntry.NodeFlags) then
      s := Lines[ToIdx(ALine + LastLineOffs)]
    else
      s := LineTextLower;
    Result :=
       (j-1 <= length(s)) and (j > 1) and (s[j-1] = '}') and
       (TheDict.GetMatchAtChar(@LineTextLower[i], LineLen + 1 - i) in [1, 4]);
    // Old closing is inside the comment?

    if Result then begin
      k := FindCloseCurlyBracket(i+1, FoundOffs, True, ALine+LastLineOffs)+1;
      Result := (LastLineOffs < FoundOffs) or
                ( (LastLineOffs = FoundOffs) and (j < k) );

      //if Result then // FoundOffs is evaluated
      //  Result :=
      //   ((not (idnMultiLineTag in AEntry.NodeFlags)) and (FoundOffs = 0)) or
      //   ((idnMultiLineTag in AEntry.NodeFlags) and (o = AEntry.Line.LastEntryEndLineOffs));
    end;

  end;

  function GetEntry(ALogStart, ALogEnd, ALineOffs: Integer;
    AType: TSynMarkupIfdefNodeType): TSynMarkupHighIfDefEntry;
  var
    i: Integer;
    e: TSynMarkupHighIfDefEntry;
  begin
    if ANodeForLine = nil then begin
      if ACheckOverlapOnCreateLine then
        ANodeForLine := GetOrInsertNodeAtLine(ALine).Node
      else
        ANodeForLine := FindNodeAtPosition(ALine, afmCreate).FNode;
      ANodeForLine.EntryCapacity := FoldNodeInfoList.Count;
      LineChanged := True;
    end;
    if NodesAddedCnt >= ANodeForLine.EntryCount then begin
      Result := ANodeForLine.AddEntry;
      LineNeedsReq := True;
      LineChanged := True;
    end
    else begin
      Result := nil;

      // Check for comments
      i := NodesAddedCnt;
      while (i < ANodeForLine.EntryCount-1) do begin
        e := ANodeForLine.Entry[i];
        if e.StartColumn >= ALogStart then
          break;
        if IsCommentedIfDef(e) then begin      // commented Ifdef or ElseIf
          //debugln('Found commented node');
          LineChanged := LineChanged or (i-1 >= NodesAddedCnt);
          while i-1 >= NodesAddedCnt do begin
            ANodeForLine.DeletEntry(i-1, True);
            dec(i);
          end;
          inc(NodesAddedCnt);
          e.MakeCommented;
        end;
        inc(i);
      end;
      // Check if existing node matches
      if i < ANodeForLine.EntryCount then begin
        Result := ANodeForLine.Entry[i];
        if ( (Result.NodeType = AType) or
             ( (Result.NodeType = idnCommentedNode) and (Result.UncommentedNodeType = AType) )
           ) and
           (Result.StartColumn = ALogStart) and
           (Result.EndColumn = ALogEnd) and
           ((idnMultiLineTag in Result.NodeFlags) = (ALineOffs > 0)) and
           ( (ALineOffs = 0) or (ALineOffs = ANodeForLine.LastEntryEndLineOffs) )
        then begin
          // Does match exactly, keep as is
          //DebugLn(['++++ KEEPING NODE ++++ ', ALine, ' ', dbgs(AType), ': ', ALogStart, ' - ', ALogEnd]);
          Result.MakeUnCommented;
          if not LineNeedsReq then
            LineNeedsReq := Result.NeedsRequesting;
          if i > NodesAddedCnt then begin
            // Delete the skipped notes
            dec(i);
            LineChanged := LineChanged or (i >= NodesAddedCnt);
            while i >= NodesAddedCnt do begin
              ANodeForLine.DeletEntry(i, True);
              dec(i);
            end;
          end;
        end
        else
          Result := nil;
      end;

      If Result = nil then begin
        // No matching node found
        LineChanged := True;
        if ANodeForLine.Entry[NodesAddedCnt].StartColumn < ALogEnd then
          Result := ANodeForLine.Entry[NodesAddedCnt]
        else
          Result := ANodeForLine.AddEntry(NodesAddedCnt);
        Result.ClearAll;
        Result.SetNodeType(AType);
        LineNeedsReq := True;
      end;
    end;
    if not(idnCommented in Result.FNodeFlags) then
      HasUncommentedNodes := True;
    inc(NodesAddedCnt);
    Result.StartColumn := ALogStart;
    Result.EndColumn   := ALogEnd;
    Result.SetNodeType(AType);
  end;

var
  fn, fn2: TSynFoldNodeInfo;
  LogStartX, LogEndX, LineOffs: Integer;
  Entry: TSynMarkupHighIfDefEntry;
  i, c: Integer;
  //RelNestDepth, RelNestDepthNext: Integer;
  NType: TSynMarkupIfdefNodeType;
begin
  //NestComments := Highlighter.NestedComments;
  LineNeedsReq := False;
  LineChanged := False;
  HasUncommentedNodes := False;
  FoldNodeInfoList := GetHighLighterWithLines.FoldNodeInfo[ToIdx(ALine)];
  FoldNodeInfoList.AddReference;
  FoldNodeInfoList.ActionFilter := []; //[sfaOpen, sfaClose];
  FoldNodeInfoList.GroupFilter := FOLDGROUP_IFDEF;

  if (ANodeForLine <> nil) and (ANodeForLine.EntryCapacity < FoldNodeInfoList.Count) then
    ANodeForLine.EntryCapacity := FoldNodeInfoList.Count;
  NodesAddedCnt := 0;
  LineOffs := 0;
  //RelNestDepthNext := 0;

  LineTextLower := LowerCase(Lines[ToIdx(ALine)]);
  LineLen := Length(LineTextLower);

  Entry := nil;
  i := -1;
  LogEndX := 0;
  c := FoldNodeInfoList.Count - 1;
  while i < c do begin
    inc(i);
    fn := FoldNodeInfoList[i];
    if fn.FoldAction * [sfaInvalid, sfaLastLineClose] <> [] then
      continue;

    LogStartX := ToPos(fn.LogXStart)-1;  // LogXStart is at "$", we need "{"
    if (LogStartX < 1) or (LogStartX > LineLen) then begin
      assert(false, '(LogStartX < 1) or (LogStartX > LineLen)  LogX='+IntToStr(LogStartX)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
      continue;
    end;
//    assert(LogStartX >= LogEndX, 'ifdef xpos found before end of previous ifdef');

    LogEndX := FindCloseCurlyBracket(LogStartX+1, LineOffs) + 1;
    //RelNestDepth := RelNestDepthNext;
    case TheDict.GetMatchAtChar(@LineTextLower[LogStartX], LineLen + 1 - LogStartX) of
      1: // ifdef
        begin
          assert(sfaOpen in fn.FoldAction, 'sfaOpen in fn.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          NType := idnIfdef;
          //inc(RelNestDepthNext);
        end;
      2: // else
        begin
          assert(i < c, '$ELSE i < c  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          inc(i);
          fn2 := FoldNodeInfoList[i];
          assert(sfaClose in fn.FoldAction, 'sfaClose in fn.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          assert(sfaOpen in fn2.FoldAction, 'sfaOpen in fn2.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          assert(fn.LogXStart = fn2.LogXStart, 'sfaOpen in fn2.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          NType := idnElse;
        end;
      3: // endif
        begin
          assert(sfaClose in fn.FoldAction, 'sfaOpen in fn.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          NType := idnEndIf;
          //dec(RelNestDepthNext);
        end;
      4: // ElseIf
        begin
          assert(i < c, '$ELSE i < c  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          inc(i);
          fn2 := FoldNodeInfoList[i];
          assert(sfaClose in fn.FoldAction, 'sfaClose in fn.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          assert(sfaOpen in fn2.FoldAction, 'sfaOpen in fn2.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          assert(fn.LogXStart = fn2.LogXStart, 'sfaOpen in fn2.FoldAction  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          NType := idnElseIf;
        end;
      else
        begin
          assert(false, 'not found ifdef  LogX='+IntToStr(LogStartX)+ ' FldAct='+dbgs(fn.FoldAction)+ ' Line='+IntToStr(ALine)+' Txt='+LineTextLower);
          continue;
        end;
    end;

    Entry := GetEntry(LogStartX, LogEndX, LineOffs, NType);
    //Entry.FRelativeNestDepth := RelNestDepth;

    if LineOffs > 0 then
      break;
  end;

  // LineOffs now has the value of the last node / or zero, if there is no node
  if (ANodeForLine <> nil) and (ANodeForLine.LastEntryEndLineOffs <> LineOffs) then begin
    LineNeedsReq := True;
    LineChanged := True;
    ANodeForLine.LastEntryEndLineOffs := LineOffs;
    if Entry <> nil then begin
      if LineOffs > 0 then
        Include(Entry.FNodeFlags, idnMultiLineTag)
       else
        Exclude(Entry.FNodeFlags, idnMultiLineTag);
    end;
  end;

  FoldNodeInfoList.ReleaseReference;
  if ANodeForLine <> nil then begin
    Include(ANodeForLine.FLineFlags, idlValid);
    if (NodesAddedCnt > 0) and LineNeedsReq then
      Include(ANodeForLine.FLineFlags, idlHasUnknownNodes);
    if HasUncommentedNodes then
      exclude(ANodeForLine.FLineFlags, idlAllNodesCommented)
    else
      include(ANodeForLine.FLineFlags, idlAllNodesCommented);
    // Check for commented ifdef
    i := ANodeForLine.EntryCount - 1;
    while i >= NodesAddedCnt do begin
      if IsCommentedIfDef(ANodeForLine.Entry[i]) then begin
        ANodeForLine.Entry[i].MakeCommented;
        inc(NodesAddedCnt);
      end
      else begin
        ANodeForLine.DeletEntry(i, True);
        LineChanged := True;
       end;
      dec(i);
    end;
    ANodeForLine.EntryCount := NodesAddedCnt;
    ANodeForLine.ReduceCapacity;
    ANodeForLine.ScanEndOffs := Max(0, LineOffs-1);
  end;
  if LineChanged then
    IncChangeStep;
end;

procedure TSynMarkupHighIfDefLinesTree.ValidateRange(AStartLine, AEndLine: Integer;
  OuterLines: TLazSynEditNestedFoldsList);
var
  NestList: TSynMarkupHighIfDefLinesNodeInfoList;
  NotInCodeLowLevel: Integer;

  procedure FixNodePeers(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
  begin
    if ANode.EntryCount = 0 then
      exit;
    ConnectPeers(ANode, NestList);
    NestList.PushNodeLine(ANode);
  end;

  procedure ApplyNotInCodeFlagToNode(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
  begin
    if Anode.HasNode and (ANode.NestDepthAtNodeStart >= NotInCodeLowLevel) then
      Include(ANode.Node.FLineFlags, idlNotInCodeToUnknown);
  end;

  procedure CheckNodeForAppNotInCodeFlag(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
  begin
    if not ANode.HasNode then
      exit;
    if idlNotInCodeToUnknownReq in ANode.LineFlags then
      NotInCodeLowLevel := Min(NotInCodeLowLevel, ANode.NestMinimumDepthAtNode);
  end;

  procedure FinishNodeForAppNotInCodeFlag(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
  begin
    if not ANode.HasNode then
      exit;
    if idlNotInCodeToUnknownReq in ANode.LineFlags then
      NotInCodeLowLevel := Min(NotInCodeLowLevel, ANode.NestMinimumDepthAtNode);
    ANode.Node.FLineFlags := ANode.Node.FLineFlags - [idlNotInCodeToUnknown, idlNotInCodeToUnknownReq];
  end;

  procedure CheckNextNodeForEmpty(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
    ABeforeLineOnly: integer = -1);
  var
    n: TSynMarkupHighIfDefLinesNode;
  begin
    (* TODO: keep state info on commented nodes.*)
    if not ANode.HasNode then
      exit;
    if (ABeforeLineOnly > 0) and (ANode.StartLine >= ABeforeLineOnly) then
      exit;
    MaybeValidateNode(ANode); // maybe skip node states?
    while (ANode.EntryCount = 0) or (idlAllNodesCommented in ANode.LineFlags) do begin
      n := ANode.Node;
      ANode := ANode.Successor;
      while n.EntryCount > 0 do
        n.DeletEntry(0, True);
      RemoveLine(n);
      if not ANode.HasNode then
        exit;
      if (ABeforeLineOnly > 0) and (ANode.StartLine >= ABeforeLineOnly) then
        exit;
      MaybeValidateNode(ANode);
    end;
  end;

var
  NextNode, Node, TmpNode: TSynMarkupHighIfDefLinesNodeInfo;
  i, j, NodeValidTo: Integer;
  SkipPeers: Boolean;
begin
  {$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
  DebugCurTreeForAssert := self; try
  {$ENDIF}
  TmpNode.FTree := Self;

  NotInCodeLowLevel := MaxInt;
  Node.ClearInfo;
  (*** Outer Nesting ***)
  OuterLines.Line := ToIdx(AStartLine);
  For i := 0 to OuterLines.Count - 1 do begin
    j := ToPos(OuterLines.NodeLine[i]);
    Node := GetOrInsertNodeAtLine(j);
    ApplyNotInCodeFlagToNode(Node);
    Assert(CheckLineForNodes(j), 'CheckLineForNodes(j) : in Outer Nesting');
    MaybeValidateNode(Node);
//    FixNodePeers(Node);
    ConnectPeers(Node, NestList, OuterLines);
    FinishNodeForAppNotInCodeFlag(Node);
    NestList.PushNodeLine(Node);
  end;

  (*** Find or create a node for StartLine ***)
  // Todo use node from outer, if possible
  //if (not node.HasNode) or (Node.StartLine + Node.ScanEndOffs < AStartLine) then
  Node := FindNodeAtPosition(AStartLine, afmPrev); // might be multiline
  MaybeValidateNode(Node);

  //debugln(['Validate RANGE ', AStartLine, ' - ', AEndLine,' -- 1st node ', Node.StartLine, ' - ', Node.ScanEndLine]);
  NextNode := Node.Successor;
  CheckNextNodeForEmpty(NextNode, Node.LastEntryEndLine);
  assert((not NextNode.HasNode) or (AStartLine < NextNode.StartLine), 'AStartLine < NextNode.StartLine');

  if (not Node.HasNode) or (AStartLine > Node.ValidToLine(NextNode)) then begin
    // Node does not cover startline
    if NextNode.HasNode and (NextNode.EntryCount = 0) then begin
      // NextNode is/was a Start-Of-Scan marker
      if (not NextNode.IsValid) then begin
        Node := NextNode;
        Node.StartLine := AStartLine;
        NextNode := Node.Successor;
      end
      else
      if (NextNode.StartLine <= AEndLine) then begin
        //CheckNodeForAppNotInCodeFlag(Node);
        //ApplyNotInCodeFlagToNode(NextNode);
        MaybeExtendNodeBackward(NextNode, AStartLine);
        //FinishNodeForAppNotInCodeFlag(NextNode);
        if NextNode.StartLine = AStartLine then begin
          Node := NextNode;
          NextNode := Node.Successor;
        end;
      end;
    end;

    MaybeValidateNode(Node);
    CheckNextNodeForEmpty(NextNode, Node.LastEntryEndLine);
    if (not Node.HasNode) or (AStartLine > Node.ValidToLine(NextNode)) then begin
      Node := FindNodeAtPosition(AStartLine, afmCreate);
      NextNode := Node.Successor;
    end;
  end;
// If node starts before AStartLine, then it must be in the OuterNodes
  SkipPeers := Node.StartLine < AStartLine; // NO peer info available, if node starts before startline
  ApplyNotInCodeFlagToNode(Node);
  MaybeValidateNode(Node);
  assert((AStartLine >= Node.StartLine) and (AStartLine <= Node.StartLine + Node.ScanEndLine), 'AStartLine is in Node');

  (*** Node is at StartLine, NextNode is set --- Scan to Endline ***)

  CheckNextNodeForEmpty(NextNode, Node.LastEntryEndLine);
  while (Node.HasNode) and (NextNode.HasNode) and
        (Node.ValidToLine(NextNode) < AEndLine) and
        (NextNode.StartLine <= AEndLine)
  do begin
    Assert(Node.IsValid, 'Node.IsValid while "Scan to Endline"');
    if not SkipPeers then
      FixNodePeers(Node);
    SkipPeers := False;
    FinishNodeForAppNotInCodeFlag(Node);
    ApplyNotInCodeFlagToNode(NextNode);

    MaybeValidateNode(NextNode);
    NodeValidTo := Node.ValidToLine(NextNode);
    MaybeExtendNodeBackward(NextNode, NodeValidTo + 1);
    assert(NextNode.StartLine > NodeValidTo, 'NextNode.StartLine > NodeValidTo');

    TmpNode.ClearInfo;
    MaybeExtendNodeForward(Node, TmpNode, NextNode.StartLine);
    assert(NextNode.StartLine > Node.ScanEndLine, 'NextNode.StartLine > Node.ScanEndLine');

    if NextNode.StartLine = Node.ScanEndLine + 1 then begin
      Assert(not TmpNode.HasNode, 'not TmpNode.HasNode');
      if NextNode.EntryCount = 0 then begin
        // Merge nodes
        Node.ScanEndOffs := Node.ScanEndOffs  + NextNode.ScanEndOffs + 1;
        RemoveLine(NextNode.FNode);
        NextNode := Node.Successor;
        // Do NOT FixNodePeers again for Node
        SkipPeers := True;
      end
      else begin
        Node := NextNode;
        NextNode := Node.Successor;
      end;
      CheckNextNodeForEmpty(NextNode, Node.LastEntryEndLine);
      continue;
    end;

    // scan gap
    Assert(TmpNode.HasNode, 'TmpNode.HasNode');
    Assert(NextNode.EntryCount > 0, 'NextNode.EntryCount > 0');
    Node := TmpNode;
    CheckNextNodeForEmpty(NextNode, Node.LastEntryEndLine);
    if not NextNode.HasNode then
      break;
    TmpNode.ClearInfo;
    while Node.ScanEndLine + 1 < NextNode.StartLine do begin
      MaybeExtendNodeForward(Node, TmpNode, NextNode.StartLine);
      if not TmpNode.HasNode then
        break;
      FixNodePeers(Node);
      assert(Node.ScanEndLine + 1 < NextNode.StartLine, 'Scan gap still before next node');
      Node := TmpNode;
      TmpNode.ClearInfo;
    end;
    assert(Node.ScanEndLine + 1 = NextNode.StartLine, 'Scan gap has reached next node');
    assert(NextNode.Node = Node.Successor.Node, 'NextNode = Node.Successor');
    assert(NextNode.StartLine = Node.Successor.StartLine, 'NextNode = Node.Successor / start');
    NextNode := Node.Successor; // TODO: not needed
    CheckNextNodeForEmpty(NextNode, Node.LastEntryEndLine);

  end;

  assert(Node.HasNode);
  if not SkipPeers then
    FixNodePeers(Node);
  FinishNodeForAppNotInCodeFlag(Node);


  while Node.ScanEndLine < AEndLine do begin
    MaybeExtendNodeForward(Node, TmpNode, AEndLine + 1);
    if not TmpNode.HasNode then
      break;
    assert(Node.ScanEndLine < AEndLine, 'Scan gap still before AEndLine');
    Node := TmpNode;
    TmpNode.ClearInfo;

    FixNodePeers(Node);
  end;
  assert(Node.ScanEndLine >= AEndLine, 'Scan gap has reached AEndLine');

  // Check for existing nodes nested in NotInCodeLowLevel
  if NotInCodeLowLevel < MaxInt then begin
    Node := Node.Successor;
    while Node.HasNode do begin
      if Node.NestDepthAtNodeStart >= NotInCodeLowLevel then begin
        ApplyNotInCodeFlagToNode(Node);
        MaybeValidateNode(Node);
        FinishNodeForAppNotInCodeFlag(Node);
      end;
      Node := Node.Successor;
    end;
  end;

  {$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
  finally DebugCurTreeForAssert := nil; end;
  {$ENDIF}
end;

procedure TSynMarkupHighIfDefLinesTree.SetNodeState(ALinePos, AstartPos: Integer;
  AState: TSynMarkupIfdefNodeState);
var
  Node: TSynMarkupHighIfDefLinesNodeInfo;
  e, e2: TSynMarkupHighIfDefEntry;
  LineNeedReq: Boolean;
  i: Integer;
begin
  Node := FindNodeAtPosition(ALinePos, afmNil);
  if not (Node.HasNode and Node.IsValid) then begin
    ScanLine(ALinePos, Node.FNode, True);
    if Node.HasNode then begin
      Node.FStartLine := ALinePos;  // directly to field
    end
    else begin
      DebugLn([ 'SetNodeState did not find a node (ScanLine) ', ALinePos, '/', 'AstartPos', AstartPos, ' ', dbgs(AState), ' #',copy(Lines[ALinePos-1],1,20)]);
      //assert(false, 'SetNodeState did not find a node (ScanLine)');
      exit;
    end;
  end;

  i := Node.EntryCount;
  if i = 0 then begin DebugLn(['SetNodeState did not find a node (zero entries)', ALinePos, '/', 'AstartPos', AstartPos, ' ', dbgs(AState), ' #',copy(Lines[ALinePos-1],1,20)]); exit; end;
  //assert(i > 0, 'SetNodeState did not find a node (zero entries)');
  e := nil;
  LineNeedReq := False;
  repeat
    dec(i);
    e2 := Node.Entry[i];
    if e2.StartColumn = AstartPos then
      e := e2
    else
      LineNeedReq := LineNeedReq or e2.NeedsRequesting;
  until (i = 0) or (e <> nil);

  //assert(e <> nil, 'SetNodeState did not find a node (no matching entry)');
  if (e = nil) then DebugLn([ 'SetNodeState did not find a matching node  ', ALinePos, '/', 'AstartPos',AstartPos, ' ', dbgs(AState), ' #',copy(Lines[ALinePos-1],1,20)]);
  //assert(e.NodeType in [idnIfdef, idnElseIf], 'SetNodeState did not find a node (e.NodeType <> idnIfdef)');
  //if (e = nil) or not(e.NodeType in [idnIfdef, idnElseIf]) then
  if (e <> nil) and (e.NodeType in [idnCommentedNode]) then begin
    DebugLn([ 'SetNodeState did find a COMMENTED node  ', ALinePos, '/', 'AstartPos',AstartPos, ' ', dbgs(AState), ' #',copy(Lines[ALinePos-1],1,20)]);
    exit;
  end;
  if (e = nil) then
    exit;

  if e.NodeState <> AState then begin
    IncChangeStep;
    if FLockTreeCount = 0 then
      FNotifyLists[itnChanged].CallNotifyEvents(Self);
      //DebugLn([ 'SetNodeState ', ALinePos, '/', 'AstartPos', AstartPos, ' ', dbgs(AState)]);
  end;
  e.NodeState := AState;
  e.StateByUser := True; // ignored by ifdef

  dec(i); // Assume the node, just set does not need requesting
  while (i >= 0) and (not LineNeedReq) do begin
    e2 := Node.Entry[i];
    LineNeedReq := LineNeedReq or e2.NeedsRequesting;
    dec(i);
  end;

  if not LineNeedReq then
    Exclude(Node.Node.FLineFlags, idlHasUnknownNodes);
end;

procedure TSynMarkupHighIfDefLinesTree.DebugPrint(Flat: Boolean);
  function PeerLine(AEntry: TSynMarkupHighIfDefEntry): string;
  begin
    if AEntry = nil then exit('-                 ');
    Result := Format('%2d/%2d (%10s)', [AEntry.Line.GetPosition, AEntry.StartColumn, dbgs(PtrUInt(AEntry))]);
  end;

  function PeerLines(AEntry: TSynMarkupHighIfDefEntry): string;
  begin
    Result := '>>p1:  ' + PeerLine(AEntry.FPeers[idpOpeningPeer]) +
           '   >>p2: ' + PeerLine(AEntry.FPeers[idpClosingPeer]);
  end;

  procedure DebugPrintNode(ANode: TSynMarkupHighIfDefLinesNode; PreFix, PreFixOne: String);
    function DbgsLine(ANode: TSynMarkupHighIfDefLinesNode): String;
    begin
      Result := 'nil';
      if ANode <> nil then Result := IntToStr(ANode.GetPosition);
    end;
  var
    i: Integer;
  begin
    DebugLn([PreFixOne, 'Line=', ANode.GetPosition, ' ScannedTo=', ANode.ScanEndOffs,
      '  Cnt=', ANode.EntryCount,
      ' EndLine=', ANode.LastEntryEndLineOffs,
      ' Flags=', dbgs(ANode.LineFlags),
      ' D-Open=', ANode.DisabledEntryOpenCount,
      ' D-Close=', ANode.DisabledEntryCloseCount
       ]);
    for i := 0 to ANode.EntryCount - 1 do
      DebugLn(Format('%s%s%s  x1-x2=%2d - %2d   %6s  %8s  flg=%12s %s',
                [PreFix, '       # ', dbgs(PtrUInt(ANode.Entry[i])),
                 ANode.Entry[i].StartColumn, ANode.Entry[i].EndColumn,
                 dbgs(ANode.Entry[i].NodeType),
                 dbgs(ANode.Entry[i].NodeState),
                dbgs(ANode.Entry[i].NodeFlags),
                PeerLines(ANode.Entry[i])
                //'  p1=', PeerLine(ANode.Entry[i].FPeer1),
                //'  p2=', PeerLine(ANode.Entry[i].FPeer2)
      ]));
    if Flat then
      exit;
    if ANode.FLeft <> nil then
      DebugPrintNode(TSynMarkupHighIfDefLinesNode(ANode.FLeft), PreFix+'   ', PreFix + 'L: ');
    if ANode.FRight <> nil then
      DebugPrintNode(TSynMarkupHighIfDefLinesNode(ANode.FRight), PreFix+'   ', PreFix + 'R: ');
  end;
var
  i: Integer;
  n: TSynMarkupHighIfDefLinesNode;
begin
  if Flat then begin
    i := 1;
    n := TSynMarkupHighIfDefLinesNode(First);
    while n <> nil do begin
      DebugPrintNode(n, '    ', format('%3d ', [i]));
      n := TSynMarkupHighIfDefLinesNode(n.Successor);
      inc(i);
    end;
  end
  else
    DebugPrintNode(TSynMarkupHighIfDefLinesNode(FRoot), '', '');
end;

procedure TSynMarkupHighIfDefLinesTree.RegisterNotification(AReason: TSynMarkupHighIfDefTreeNotifications;
  AHandler: TNotifyEvent);
begin
  FNotifyLists[AReason].Add(TMethod(AHandler));
end;

procedure TSynMarkupHighIfDefLinesTree.UnRegisterNotification(AReason: TSynMarkupHighIfDefTreeNotifications;
  AHandler: TNotifyEvent);
begin
  FNotifyLists[AReason].Remove(TMethod(AHandler));
end;

procedure TSynMarkupHighIfDefLinesTree.LockTree;
begin
  inc(FLockTreeCount);
end;

procedure TSynMarkupHighIfDefLinesTree.UnLockTree;
begin
  assert(FLockTreeCount > 0, 'UnLockTree < 0');
  if FLockTreeCount = 1 then
    FNotifyLists[itnUnlocking].CallNotifyEvents(Self);
  dec(FLockTreeCount);
  if FLockTreeCount = 0 then
    FNotifyLists[itnUnlocked].CallNotifyEvents(Self);
end;

procedure TSynMarkupHighIfDefLinesTree.Clear;
var
  n: TSynSizedDifferentialAVLNode;
begin
  FClearing := True;
  inherited Clear;
  while FDisposedNodes <> nil do begin
    n := FDisposedNodes;
    FDisposedNodes := TSynMarkupHighIfDefLinesNode(n).NextDispose;
    n.Free;
  end;
  FClearing := False;
end;

function TSynMarkupHighIfDefLinesTree.FindNodeAtPosition(ALine: Integer;
  AMode: TSynSizedDiffAVLFindMode): TSynMarkupHighIfDefLinesNodeInfo;
begin
  Result{%H-}.ClearInfo;
  Result.FTree := Self;
  Result.FNode :=
    TSynMarkupHighIfDefLinesNode(FindNodeAtPosition(ALine, AMode,
      Result.FStartLine, Result.Index));
  if Result.FNode = nil then begin
    Result.FAtBOL := AMode = afmPrev;
    Result.FAtEOL := AMode = afmNext;
  end;
end;

{ TSynEditMarkupIfDef }

procedure TSynEditMarkupIfDef.DoTreeUnlocking(Sender: TObject);
var
  LastLine: Integer;
begin
  Assert(FPaintLock = 0, 'DoTreeUnlocked in paintlock');

  if (not SynEdit.IsVisible) or (not HasEnabledMarkup) or (Highlighter = nil) then
    exit;

  LastLine := ScreenRowToRow(LinesInWindow+1);
  FAdjustedTop := ToIdx(TopLine);
  PrepareHighlighter;

  if FMarkupNodes.HasEnabledMarkup then
    while (FAdjustedTop > 0) and Highlighter.IsLineStartingInDirective(FAdjustedTop) and
      (not (Highlighter.FoldBlockMinLevel(FAdjustedTop, FOLDGROUP_IFDEF, [sfbIncludeDisabled]) <
            Highlighter.FoldBlockEndLevel(FAdjustedTop, FOLDGROUP_IFDEF, [sfbIncludeDisabled])  ))
    do
      dec(FAdjustedTop);
  FAdjustedTop := ToPos(FAdjustedTop);
if FAdjustedTop <> TopLine then DebugLn(['FAdjustedTop=', FAdjustedTop, '  top=', TopLine]);

  if (FLastValidTopLine  <= TopLine) and (FLastValidLastLine >= LastLine) and
     (FLastValidTreeStep = FIfDefTree.ChangeStep)
  then
    exit;

// TODO: assert synedit does not change
//DebugLn(['TSynEditMarkupIfDef.DoTreeUnlocking', TopLine, ' - ', LastLine]);
  FOuterLines.Clear; // TODO: invalidate acording to actual lines edited
  FIfDefTree.ValidateRange(TopLine, LastLine, FOuterLines);
end;

procedure TSynEditMarkupIfDef.DoTreeUnlocked(Sender: TObject);
var
  LastLine: Integer;

  function IndexOfLastNodeAtLevel(ALevel: Integer; ANode: TSynMarkupHighIfDefLinesNode;
    ADownFromIndex: Integer; var ADownFromLevel: Integer): Integer;
  begin
    Result := ADownFromIndex;
    repeat
      case ANode.Entry[Result].NodeType of
        idnIfdef: dec(ADownFromLevel);
        idnEndIf: inc(ADownFromLevel);
        idnElse, idnElseIf:
          if ADownFromLevel <= ALevel + 1 then
            dec(ADownFromLevel);
      end;
      if (ADownFromLevel <= ALevel) then
        exit;
      dec(Result);
    until (Result < 0);
  end;

  function GetValidClosingPeer(AEntry: TSynMarkupHighIfDefEntry): TSynMarkupHighIfDefEntry;
  begin
    Result := AEntry.ClosingPeer;
    // if the Result line is not valid, then it ends after the visible area (otherwise it would have a valid line)
    If (Result <> nil) and not(idlValid in Result.Line.LineFlags) then
      Result := nil;
  end;

  function EntryToPointAtBegin(Entry: TSynMarkupHighIfDefEntry; Line: Integer): TPoint;
  begin
    Result.y := Line;
    Result.x := Entry.StartColumn;
  end;

  function EntryToPointAtEnd(Entry: TSynMarkupHighIfDefEntry; Line: Integer): TPoint;
  begin
    Result.y := Line;
    if idnMultiLineTag in Entry.NodeFlags then
      Result.y := Line + Entry.Line.LastEntryEndLineOffs;
    Result.x := Entry.EndColumn;
  end;

var
  ScanNodes, ScanDisNode, ScanEnaNode, ScanTmpDisNode, ScanTmpEnaNode: Boolean;

  procedure AddNodeMatch(Entry1: TSynMarkupHighIfDefEntry; Line1: Integer);
  var
    P1, P2: TPoint;
    i: Integer;
  begin
    P1 := EntryToPointAtBegin(Entry1, Line1);
    P2 := EntryToPointAtEnd(Entry1, Line1);
    i := 0;

    if Entry1.IsOpening then begin
      if Entry1.IsDisabled then begin
        if Entry1.IsTemp then begin
          if ScanTmpDisNode then i := i or MARKUP_TEMP_DISABLED;
        end else begin
          if ScanDisNode then i := i or MARKUP_DISABLED;
        end;
      end
      else if Entry1.IsEnabled then begin
        if Entry1.IsTemp then begin
          if ScanTmpEnaNode then i := i or MARKUP_TEMP_ENABLED;
        end else begin
          if ScanEnaNode then i := i or MARKUP_ENABLED;
        end;
      end;
    end;

    if Entry1.IsClosing and (not MarkOnlyOpeningNodes) then begin
      if Entry1.HasDisabledOpening then begin
        if Entry1.IsTempClosing then begin
          if ScanTmpDisNode then i := i or MARKUP_TEMP_DISABLED;
        end else begin
          if ScanDisNode then i := i or MARKUP_DISABLED;
        end;
      end
      else if Entry1.HasEnabledOpening then begin
        if Entry1.IsTempClosing then begin
          if ScanTmpEnaNode then i := i or MARKUP_TEMP_ENABLED;
        end else begin
          if ScanEnaNode then i := i or MARKUP_ENABLED;
        end;
      end;
    end;

    if i <> 0 then
      FMarkupNodes.AddMatch(P1, P2, False, False, i)
  end;

  procedure AddRangeMatch(Entry1: TSynMarkupHighIfDefEntry; Line1: Integer;
    Entry2: TSynMarkupHighIfDefEntry; Line2: Integer);
  var
    P1, P2: TPoint;
  begin
    P1 := EntryToPointAtEnd(Entry1, Line1);
    if Entry2 <> nil then
      P2 := EntryToPointAtBegin(Entry2, Line2)
    else
      P2 := point(1, LastLine+1);

    // Entry1 always is an opening node.
    if (Entry1.IsDisabled) then begin
      if (Entry1.IsTemp) then
        FMarkupTemp.AddMatch(P1, P2, FAlse, Entry2=nil, MARKUP_DEFAULT)
      else
        AddMatch(P1, P2, False, Entry2=nil, MARKUP_DEFAULT);
    end
    else if (Entry1.IsEnabled) then begin
      if (Entry1.IsTemp) then
        FMarkupEnabledTemp.AddMatch(P1, P2, FAlse, Entry2=nil, MARKUP_DEFAULT)
      else
        FMarkupEnabled.AddMatch(P1, P2, False, Entry2=nil, MARKUP_DEFAULT);
    end;
  end;

  procedure AddRangeIfNotNil(AnOpenEntry: TSynMarkupHighIfDefEntry; AnOpenLine: Integer;
    out AClosePeerEntry: TSynMarkupHighIfDefEntry; out AClosePeerLine: Integer);
  begin
    AClosePeerEntry := nil;
    AClosePeerLine := -1;
    if AnOpenEntry = nil then
      exit;
    AClosePeerEntry := GetValidClosingPeer(AnOpenEntry);
    If (AClosePeerEntry <> nil) then   // Disabled to end of display or beyond (end of validated)
      AClosePeerLine := AClosePeerEntry.Line.GetPosition;
    assert((AClosePeerEntry = nil) or ((AClosePeerLine >= FAdjustedTop)), 'outernode finish early'); // Should not reach here

    AddRangeMatch(AnOpenEntry, AnOpenLine, AClosePeerEntry, AClosePeerLine);
  end;

var
  ScanDisRange, ScanEnaRange, ScanTmpDisRange, ScanTmpEnaRange: Boolean;
  DisableOpenEntry, DisabledCloseEntry, EnableOpenEntry, EnableCloseEntry: TSynMarkupHighIfDefEntry;
  TmpDisableOpenEntry, TmpDisabledCloseEntry, TmpEnableOpenEntry, TmpEnableCloseEntry: TSynMarkupHighIfDefEntry;
  DisabledCloseLinePos, EnableCloseLinePos, TmpDisabledCloseLinePos, TmpEnableCloseLinePos: Integer;

  function NeedDisableOpen: Boolean;
  begin
    Result := ScanDisRange and (DisableOpenEntry = nil);
  end;
  function NeedEnableOpen: Boolean;
  begin
    Result := ScanEnaRange and (EnableOpenEntry = nil) and
              (DisableOpenEntry = nil) and (TmpDisableOpenEntry = nil); // Do not show enblade, intside a disabled
  end;
  function NeedTmpDisableOpen: Boolean;
  begin
    Result := ScanTmpDisRange and (TmpDisableOpenEntry = nil);
  end;
  function NeedTmpEnableOpen: Boolean;
  begin
    Result := ScanTmpEnaRange and (TmpEnableOpenEntry = nil) and
              (DisableOpenEntry = nil) and (TmpDisableOpenEntry = nil);
  end;

  procedure SetNil(var AOpenEntry, ACloseEntry: TSynMarkupHighIfDefEntry;
    var ALine: Integer);
  begin
    AOpenEntry  := nil;
    ACloseEntry := nil;
    ALine := 0;
  end;

  var
    CacheIdx: Array[0..3] of Integer;
    CacheEntry: Array[0..3] of TSynMarkupHighIfDefEntry;

  procedure ClearIdxCache;
  var
    i: Integer;
  begin
    for i := 0 to 3 do CacheEntry[i] := nil;
  end;
  function IndexOfEntry(AEntry: TSynMarkupHighIfDefEntry; ACache: Integer): Integer;
  begin
    if CacheEntry[ACache] = AEntry then exit(CacheIdx[ACache]);
    Result := AEntry.Line.IndexOf(AEntry);
    CacheEntry[ACache] := AEntry;
    CacheIdx[ACache] := Result;
  end;
  function IndexOfDisabledClose: Integer; // cache 0
  begin
    Result := IndexOfEntry(DisabledCloseEntry, 0);
  end;
  function IndexOfEnableClose: Integer; // cache 1
  begin
    Result := IndexOfEntry(EnableCloseEntry, 1);
  end;
  function IndexOfTmpDisabledClose: Integer; // cache 2
  begin
    Result := IndexOfEntry(TmpDisabledCloseEntry, 2);
  end;
  function IndexOfTmpEnableClose: Integer; // cache 3
  begin
    Result := IndexOfEntry(TmpEnableCloseEntry, 3);
  end;

  procedure FindNextScanPos(var ANodeInfo: TSynMarkupHighIfDefLinesNodeInfo;
    var AFirstIdx: Integer);
  var
    CurFoundLine: Integer;
    procedure SetResultFor(AEntry: TSynMarkupHighIfDefEntry; ALine: Integer);
    begin
      CurFoundLine := ALine;
      Assert((not ANodeInfo.HasNode) or (ANodeInfo.StartLine <= ALine), 'FindNextScanPos goes backward');
      ANodeInfo.InitForNode(AEntry.Line, ALine);
      AFirstIdx := ANodeInfo.Node.IndexOf(DisabledCloseEntry) + 1;
    end;
    procedure MaybeSetNil(var AOpenEntry, ACloseEntry: TSynMarkupHighIfDefEntry;
      var ALine: Integer; ACacheNo: Integer);
    begin
      if (ALine < CurFoundLine) or
         ((ALine = CurFoundLine) and (IndexOfEntry(ACloseEntry, ACacheNo) < AFirstIdx))
      then begin
        AOpenEntry  := nil;
        ACloseEntry := nil;
        ALine := 0;
      end;
    end;
  begin
    if NeedDisableOpen or NeedEnableOpen or NeedTmpDisableOpen or NeedTmpEnableOpen or
       ScanNodes
    then
      exit; // can not skip ahead

    // assert: all nodes must end after ANodeInfo/AFirstIdx
    CurFoundLine := -1;

    if ScanDisNode and (DisabledCloseEntry <> nil) then
      SetResultFor(DisabledCloseEntry, DisabledCloseLinePos);

    if ScanEnaNode and (EnableCloseEntry <> nil) then begin
      if (CurFoundLine < 0) or (EnableCloseLinePos < CurFoundLine) then begin
        SetResultFor(EnableCloseEntry, EnableCloseLinePos);
      end
      else
      if (EnableCloseLinePos = CurFoundLine) then begin
        assert(ANodeInfo.HasNode and (ANodeInfo.Node = EnableCloseEntry.Line), 'ANodeInfo.HasNode and (ANodeInfo.Node = EnableCloseEntry.Line)');
        if IndexOfEnableClose < AFirstIdx then AFirstIdx := IndexOfEnableClose;
      end;
    end;

    if ScanTmpDisNode and (TmpDisabledCloseEntry <> nil) then begin
      if (CurFoundLine < 0) or (TmpDisabledCloseLinePos < CurFoundLine) then begin
        SetResultFor(TmpDisabledCloseEntry, TmpDisabledCloseLinePos);
      end
      else
      if (TmpDisabledCloseLinePos = CurFoundLine) then begin
        assert(ANodeInfo.HasNode and (ANodeInfo.Node = TmpDisabledCloseEntry.Line), 'ANodeInfo.HasNode and (ANodeInfo.Node = TmpDisabledCloseEntry.Line)');
        if IndexOfTmpDisabledClose < AFirstIdx then AFirstIdx := IndexOfTmpDisabledClose;
      end;
    end;

    if ScanTmpEnaNode and (TmpEnableCloseEntry <> nil) then begin
      if (CurFoundLine < 0) or (TmpEnableCloseLinePos < CurFoundLine) then begin
        SetResultFor(TmpEnableCloseEntry, TmpEnableCloseLinePos);
      end
      else
      if (TmpEnableCloseLinePos = CurFoundLine) then begin
        assert(ANodeInfo.HasNode and (ANodeInfo.Node = TmpEnableCloseEntry.Line), 'ANodeInfo.HasNode and (ANodeInfo.Node = TmpEnableCloseEntry.Line)');
        if IndexOfTmpEnableClose < AFirstIdx then AFirstIdx := IndexOfTmpEnableClose;
      end;
    end;

    if CurFoundLine < 0 then begin
      ANodeInfo.ClearInfo;         // FStartLine = 0
      ANodeInfo.FStartLine := -1;  // all nodes, end behind visible area
    end
    else begin
      MaybeSetNil(DisableOpenEntry, DisabledCloseEntry, DisabledCloseLinePos, 0);
      MaybeSetNil(EnableOpenEntry,  EnableCloseEntry,   EnableCloseLinePos, 1);
      MaybeSetNil(TmpDisableOpenEntry, TmpDisabledCloseEntry, TmpDisabledCloseLinePos, 2);
      MaybeSetNil(TmpEnableOpenEntry,  TmpEnableCloseEntry,   TmpEnableCloseLinePos, 3);
    end;
  end;

var
  NestLvl, NestLvlLow, FoundLvl, CurLine, EndLvl, FirstEntryIdx: Integer;
  j: Integer;
  NodeInfo: TSynMarkupHighIfDefLinesNodeInfo;
  Node: TSynMarkupHighIfDefLinesNode;
  Entry: TSynMarkupHighIfDefEntry;
  EntryFound: Boolean;
begin
  Assert(FPaintLock = 0, 'DoTreeUnlocked in paintlock');

  if (not SynEdit.IsVisible) then
    exit;
  if (Highlighter = nil) or (not HasEnabledMarkup) then begin
    FIfDefTree.Clear;
    if Matches.Count > 0 then
      InvalidateSynLines(TopLine, ScreenRowToRow(LinesInWindow+1));
    Matches.Count := 0;
    exit;
  end;

  PrepareHighlighter;
  LastLine := ScreenRowToRow(LinesInWindow+1);
  if (FLastValidTopLine  <= TopLine) and (FLastValidLastLine >= LastLine) and
     (FLastValidTreeStep = FIfDefTree.ChangeStep)
  then
    exit;

  FLastValidTopLine  := TopLine;
  FLastValidLastLine := LastLine;
  FLastValidTreeStep := FIfDefTree.ChangeStep;

  {$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
  try
  DebugCurTreeForAssert := FIfDefTree; try
  {$ENDIF}
//debugln(['TSynEditMarkupIfDef.DoTreeUnlocked ', TopLine, ' - ', LastLine]);

  ScanDisRange    := MarkupInfoDisabled.IsEnabled;
  ScanEnaRange    := MarkupInfoEnabled.IsEnabled;
  ScanTmpDisRange := MarkupInfoTempDisabled.IsEnabled;
  ScanTmpEnaRange := MarkupInfoTempEnabled.IsEnabled;

  ScanDisNode    := MarkupInfoNodeDisabled.IsEnabled;
  ScanEnaNode    := MarkupInfoNodeEnabled.IsEnabled;
  ScanTmpDisNode := MarkupInfoTempNodeDisabled.IsEnabled;
  ScanTmpEnaNode := MarkupInfoTempNodeEnabled.IsEnabled;
  ScanNodes := ScanDisNode or ScanEnaNode or ScanTmpDisNode or ScanTmpEnaNode;

  StartMatchScan;
  FMarkupEnabled.StartMatchScan;
  FMarkupTemp.StartMatchScan;
  FMarkupEnabledTemp.StartMatchScan;
  FMarkupNodes.StartMatchScan;
  ClearIdxCache;

  // *** Check outerlines, for node that goes into visible area
  DisableOpenEntry := nil;
  EnableOpenEntry := nil;
  TmpDisableOpenEntry := nil;
  TmpEnableOpenEntry := nil;
  DisabledCloseEntry := nil;
  EnableCloseEntry := nil;
  TmpDisabledCloseEntry := nil;
  TmpEnableCloseEntry := nil;

  NestLvl := -1;
  while (NestLvl < FOuterLines.Count - 1) and
        (NeedDisableOpen or NeedEnableOpen or NeedTmpDisableOpen or NeedTmpDisableOpen)
  do begin
    inc(NestLvl);

    CurLine := ToPos(FOuterLines.NodeLine[NestLvl]);
    NestLvlLow := NestLvl;
    while (NestLvl < FOuterLines.Count - 1) and (ToPos(FOuterLines.NodeLine[NestLvl + 1]) = CurLine) do
      inc(NestLvl);

    NodeInfo := FIfDefTree.FindNodeAtPosition(CurLine, afmNil);
    Node := NodeInfo.Node;
    assert(NodeInfo.HasNode, 'ValidateMatches: No node for outerline');

    //if NodeInfo.Node.DisabledEntryOpenCount <= NodeInfo.Node.DisabledEntryCloseCount then
    //  continue;

    EndLvl := NodeInfo.NestDepthAtNodeEnd;
    FoundLvl := NestLvl;
    j := IndexOfLastNodeAtLevel(FoundLvl, Node, Node.EntryCount - 1, EndLvl);
    while (j >= 0) do begin
      Entry := Node.Entry[j];
      dec(FoundLvl);
      if Entry.IsTempOpening then begin
        if Entry.IsDisabledOpening and NeedTmpDisableOpen then TmpDisableOpenEntry := Entry;
        if Entry.IsEnabled         and NeedTmpEnableOpen  then TmpEnableOpenEntry := Entry;
      end else begin
        if Entry.IsDisabledOpening and NeedDisableOpen then DisableOpenEntry := Entry;
        if Entry.IsEnabled         and NeedEnableOpen  then EnableOpenEntry := Entry;
      end;
      if FoundLvl < NestLvlLow then
        break;
       j := IndexOfLastNodeAtLevel(FoundLvl, Node, j, EndLvl);
    end;
  end; // while

  AddRangeIfNotNil(DisableOpenEntry,    CurLine, DisabledCloseEntry,    DisabledCloseLinePos);
  AddRangeIfNotNil(EnableOpenEntry,     CurLine, EnableCloseEntry,      EnableCloseLinePos);
  AddRangeIfNotNil(TmpDisableOpenEntry, CurLine, TmpDisabledCloseEntry, TmpDisabledCloseLinePos);
  AddRangeIfNotNil(TmpEnableOpenEntry,  CurLine, TmpEnableCloseEntry,   TmpEnableCloseLinePos);

  // *** END Check outerlines, for node that goes into visible area
  // *** if found, then it is in DisableOpenEntry and DisabledCloseEntry

  if ScanNodes then begin
  // FAdjustedTop
    NodeInfo := FIfDefTree.FindNodeAtPosition(FAdjustedTop, afmNext);
    //while NodeInfo.HasNode and (NodeInfo.EntryCount = 0) do
    //  NodeInfo := NodeInfo.Successor;
    Node := NodeInfo.Node;
    FirstEntryIdx := 0;
    if (Node <> nil) and (NodeInfo.StartLine < TopLine) then
      FirstEntryIdx := Node.EntryCount - 1; // May be visible
  end
  else begin
    // Not scanning nodes
    NodeInfo.ClearInfo;
    FindNextScanPos(NodeInfo, FirstEntryIdx);
    if (not NodeInfo.HasNode) and (NodeInfo.StartLine = 0) then begin
      NodeInfo := FIfDefTree.FindNodeAtPosition(TopLine, afmNext);
      Node := NodeInfo.Node;
      FirstEntryIdx := 0;
    end;
    Node := NodeInfo.Node;
  end;
  CurLine := NodeInfo.StartLine;

  while (Node <> nil) and (CurLine <= LastLine) do begin
    while FirstEntryIdx < Node.EntryCount do begin
      EntryFound := False;
      Entry := Node.Entry[FirstEntryIdx];
      inc(FirstEntryIdx);

      if ScanNodes then AddNodeMatch(Entry, CurLine);

      if Entry = DisabledCloseEntry then SetNil(DisableOpenEntry, DisabledCloseEntry, DisabledCloseLinePos);
      if Entry = EnableCloseEntry   then SetNil(EnableOpenEntry,  EnableCloseEntry,   EnableCloseLinePos);
      if Entry = TmpDisabledCloseEntry then SetNil(TmpDisableOpenEntry, TmpDisabledCloseEntry, TmpDisabledCloseLinePos);
      if Entry = TmpEnableCloseEntry   then SetNil(TmpEnableOpenEntry,  TmpEnableCloseEntry,   TmpEnableCloseLinePos);

      if NeedDisableOpen and (not Entry.IsTemp) and Entry.IsDisabledOpening then begin
        DisableOpenEntry := Entry;
        DisabledCloseEntry := GetValidClosingPeer(DisableOpenEntry);
        if DisabledCloseEntry <> nil then begin
          if DisabledCloseEntry.Line = Node
          then DisabledCloseLinePos := CurLine
          else DisabledCloseLinePos := DisabledCloseEntry.Line.GetPosition;
        end;
        assert((DisabledCloseEntry=nil) or (DisabledCloseLinePos >= CurLine), 'Node goes backward');
        AddRangeMatch(DisableOpenEntry, CurLine, DisabledCloseEntry, DisabledCloseLinePos);
        EntryFound := True;
      end;

      if NeedEnableOpen and (not Entry.IsTemp) and Entry.IsEnabled and Entry.IsOpening then begin
        EnableOpenEntry := Entry;
        EnableCloseEntry := GetValidClosingPeer(EnableOpenEntry);
        if EnableCloseEntry <> nil then begin
          if EnableCloseEntry.Line = Node
          then EnableCloseLinePos := CurLine
          else EnableCloseLinePos := EnableCloseEntry.Line.GetPosition;
        end;
        assert((EnableCloseEntry=nil) or (EnableCloseLinePos >= CurLine), 'Node goes backward');
        AddRangeMatch(EnableOpenEntry, CurLine, EnableCloseEntry, EnableCloseLinePos);
        EntryFound := True;
      end;

      if NeedTmpDisableOpen and (Entry.IsTemp) and Entry.IsDisabledOpening then begin
        TmpDisableOpenEntry := Entry;
        TmpDisabledCloseEntry := GetValidClosingPeer(TmpDisableOpenEntry);
        if TmpDisabledCloseEntry <> nil then begin
          if TmpDisabledCloseEntry.Line = Node
          then TmpDisabledCloseLinePos := CurLine
          else TmpDisabledCloseLinePos := TmpDisabledCloseEntry.Line.GetPosition;
        end;
        assert((TmpDisabledCloseEntry=nil) or (TmpDisabledCloseLinePos >= CurLine), 'Node goes backward');
        AddRangeMatch(TmpDisableOpenEntry, CurLine, TmpDisabledCloseEntry, TmpDisabledCloseLinePos);
        EntryFound := True;
      end;

      if NeedTmpEnableOpen and (Entry.IsTemp) and Entry.IsEnabled and Entry.IsOpening then begin
        TmpEnableOpenEntry := Entry;
        TmpEnableCloseEntry := GetValidClosingPeer(TmpEnableOpenEntry);
        if TmpEnableCloseEntry <> nil then begin
          if TmpEnableCloseEntry.Line = Node
          then TmpEnableCloseLinePos := CurLine
          else TmpEnableCloseLinePos := TmpEnableCloseEntry.Line.GetPosition;
        end;
        assert((TmpEnableCloseEntry=nil) or (TmpEnableCloseLinePos >= CurLine), 'Node goes backward');
        AddRangeMatch(TmpEnableOpenEntry, CurLine, TmpEnableCloseEntry, TmpEnableCloseLinePos);
        EntryFound := True;
      end;

      if EntryFound then begin
        FindNextScanPos(NodeInfo, FirstEntryIdx);     Node := NodeInfo.Node; CurLine := NodeInfo.StartLine;
      end;
    end; // while FirstEntryIdx < Node.EntryCount do

    NodeInfo := NodeInfo.Successor;
    if not NodeInfo.HasNode then
      break;
    CurLine := NodeInfo.StartLine;
    Node := NodeInfo.Node;
    FirstEntryIdx := 0;
  end; // while

  // delete remaining matchdata
  FMarkupEnabled.EndMatchScan(LastLine);
  FMarkupTemp.EndMatchScan(LastLine);
  FMarkupEnabledTemp.EndMatchScan(LastLine);
  FMarkupNodes.EndMatchScan(LastLine);
  EndMatchScan(LastLine);

  {$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
  finally DebugCurTreeForAssert := nil; end;
  except FIfDefTree.DebugPrint(true); end;
  {$ENDIF}
end;

procedure TSynEditMarkupIfDef.DoTreeChanged(Sender: TObject);
begin
  DebugLn('TSynEditMarkupIfDef.DoTreeChanged');
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.PrepareHighlighter;
begin
  Highlighter.CurrentLines := Lines;
  if Highlighter.NeedScan then DebugLn('******** Highlighter.NeedScan ************');
  Highlighter.ScanRanges;
end;

function TSynEditMarkupIfDef.HasEnabledMarkup: Boolean;
begin
  Result := (inherited HasEnabledMarkup) or
            FMarkupNodes.HasEnabledMarkup or
            FMarkupEnabled.HasEnabledMarkup or
            FMarkupTemp.HasEnabledMarkup or
            FMarkupEnabledTemp.HasEnabledMarkup;
end;

procedure TSynEditMarkupIfDef.SetFoldView(AValue: TSynEditFoldedView);
begin
  if FFoldView = AValue then Exit;
  FFoldView := AValue;
end;

function TSynEditMarkupIfDef.GetMarkupInfoDisabled: TSynSelectedColor;
begin
  Result := MarkupInfo;
end;

function TSynEditMarkupIfDef.GetMarkupInfoEnabled: TSynSelectedColor;
begin
  Result := FMarkupEnabled.MarkupInfo;
end;

function TSynEditMarkupIfDef.GetMarkupInfoNodeDisabled: TSynSelectedColor;
begin
  Result := FMarkupNodes.MarkupInfoDisabled;
end;

function TSynEditMarkupIfDef.GetMarkupInfoNodeEnabled: TSynSelectedColor;
begin
  Result := FMarkupNodes.MarkupInfoEnabled;
end;

function TSynEditMarkupIfDef.GetMarkupInfoTempDisabled: TSynSelectedColor;
begin
  Result := FMarkupTemp.MarkupInfo;
end;

function TSynEditMarkupIfDef.GetMarkupInfoTempEnabled: TSynSelectedColor;
begin
  Result := FMarkupEnabledTemp.MarkupInfo;
end;

function TSynEditMarkupIfDef.GetMarkupInfoTempNodeDisabled: TSynSelectedColor;
begin
  Result := FMarkupNodes.MarkupInfoTempDisabled;
end;

function TSynEditMarkupIfDef.GetMarkupInfoTempNodeEnabled: TSynSelectedColor;
begin
  Result := FMarkupNodes.MarkupInfoTempEnabled;
end;

procedure TSynEditMarkupIfDef.SetHighlighter(AValue: TSynPasSyn);
begin
  if FHighlighter = AValue then Exit;
  FHighlighter := AValue;
  FIfDefTree.Highlighter := AValue;
  FOuterLines.HighLighter := AValue;
end;

procedure TSynEditMarkupIfDef.DoBufferChanging(Sender: TObject);
begin
  FIfDefTree.Clear;
  FIfDefTree.Lines := nil;
  FOuterLines.Lines := nil;
end;

procedure TSynEditMarkupIfDef.ValidateMatches;
begin
  if (FPaintLock > 0) or (not SynEdit.IsVisible) or (not HasEnabledMarkup) then
    exit;

  //debugln(['Validate without lock']);
  DoTreeUnlocking(FIfDefTree);
  DoTreeUnlocked(FIfDefTree);
end;

procedure TSynEditMarkupIfDef.DoFoldChanged(aLine: Integer);
begin
  DoTopLineChanged(-1);
end;

procedure TSynEditMarkupIfDef.DoTopLineChanged(OldTopLine: Integer);
begin
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoLinesInWindoChanged(OldLinesInWindow: Integer);
begin
  DoTopLineChanged(-1);
end;

procedure TSynEditMarkupIfDef.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  if FIfDefTree = nil then
    exit;

  FIfDefTree.IncChangeStep; // force all validation to run
  ValidateMatches;
  SynEdit.Invalidate;
end;

procedure TSynEditMarkupIfDef.DoTextChanged(StartLine, EndLine, ACountDiff: Integer);
begin
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoVisibleChanged(AVisible: Boolean);
begin
  FLastValidTopLine  := 0;
  FLastValidLastLine := 0;
  FLastValidTreeStep := 0;
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoBufferChanged(Sender: TObject);
begin
  //FIfDefTree.Lines  pointing to view => so still valid
  FIfDefTree.Clear;
  FIfDefTree.Lines := Lines;
  FOuterLines.Lines := Lines;

  FLastValidTopLine  := 0;
  FLastValidLastLine := 0;
  FLastValidTreeStep := 0;
end;

function TSynEditMarkupIfDef.DoNodeStateRequest(Sender: TObject; LinePos,
  XStartPos: Integer; CurrentState: TSynMarkupIfdefNodeStateEx): TSynMarkupIfdefNodeState;
begin
  if FOnNodeStateRequest <> nil then
    Result := FOnNodeStateRequest(Self, LinePos, XStartPos, CurrentState)
  else
  if CurrentState in [low(TSynMarkupIfdefNodeState)..high(TSynMarkupIfdefNodeState)]
  then
    Result := CurrentState
  else
    Result := idnInvalid;

  //DebugLn(['STATE REQUEST ', LinePos, ' ', XStartPos, ' : ', dbgs(Result)]);
end;

procedure TSynEditMarkupIfDef.SetMarkOnlyOpeningNodes(AValue: Boolean);
begin
  if FMarkOnlyOpeningNodes = AValue then Exit;
  FMarkOnlyOpeningNodes := AValue;
  if FMarkupNodes.HasEnabledMarkup then
    DoMarkupChanged(nil);
end;

procedure TSynEditMarkupIfDef.SetLines(const AValue: TSynEditStringsLinked);
begin
  if Lines <> nil then begin
    Lines.RemoveNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
    Lines.RemoveNotifyHandler(senrTextBufferChanging, @DoBufferChanging);
    //FLines.RemoveEditHandler(@DoLinesEdited);
//    FLines.RemoveChangeHandler(senrHighlightChanged, @DoHighlightChanged);
  end;

  inherited SetLines(AValue);
  FIfDefTree.Lines := AValue;
  FOuterLines.Lines := AValue;

  if Lines <> nil then begin
    Lines.AddNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
    Lines.AddNotifyHandler(senrTextBufferChanging, @DoBufferChanging);
    //FLines.AddChangeHandler(senrHighlightChanged, @DoHighlightChanged);
//    FLines.AddEditHandler(@DoLinesEdited);
  end;
end;

procedure TSynEditMarkupIfDef.SetInvalidateLinesMethod(const AValue: TInvalidateLines);
begin
  inherited SetInvalidateLinesMethod(AValue);
  FMarkupNodes.InvalidateLinesMethod := AValue;
  FMarkupEnabled.InvalidateLinesMethod := AValue;
  FMarkupTemp.InvalidateLinesMethod := AValue;
  FMarkupEnabledTemp.InvalidateLinesMethod := AValue;

end;

constructor TSynEditMarkupIfDef.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  FMarkupNodes       := TSynEditMarkupIfDefNodes.Create(ASynEdit);
  FMarkupEnabled     := TSynEditMarkupIfDefBase.Create(ASynEdit);
  FMarkupTemp        := TSynEditMarkupIfDefBase.Create(ASynEdit);
  FMarkupEnabledTemp := TSynEditMarkupIfDefBase.Create(ASynEdit);

  FIfDefTree := TSynMarkupHighIfDefLinesTree.Create;
  FIfDefTree.OnNodeStateRequest := @DoNodeStateRequest;
  FIfDefTree.RegisterNotification(itnUnlocking, @DoTreeUnlocking);
  FIfDefTree.RegisterNotification(itnUnlocked, @DoTreeUnlocked);
  FIfDefTree.RegisterNotification(itnChanged, @DoTreeChanged);

  FOuterLines := FIfDefTree.CreateOpeningList;

  FLastValidTopLine  := 0;
  FLastValidLastLine := 0;
  FLastValidTreeStep := 0;

  IncPaintLock;

  FMarkOnlyOpeningNodes := False;

  MarkupInfo.Clear;
  MarkupInfo.Foreground := clLtGray;
  MarkupInfo.ForeAlpha := 180;
  MarkupInfo.ForePriority := 99999;

  MarkupInfoTempDisabled.Clear;
  MarkupInfoTempDisabled.Foreground := clLtGray;
  MarkupInfoTempDisabled.ForeAlpha := 140;
  MarkupInfoTempDisabled.ForePriority := 99999;

  FMarkupEnabled.MarkupInfo.OnChange := @MarkupChanged;
  FMarkupTemp.MarkupInfo.OnChange := @MarkupChanged;
  FMarkupEnabledTemp.MarkupInfo.OnChange := @MarkupChanged;
  FMarkupNodes.MarkupInfoDisabled.OnChange := @MarkupChanged;
  FMarkupNodes.MarkupInfoEnabled.OnChange := @MarkupChanged;
  FMarkupNodes.MarkupInfoTempDisabled.OnChange := @MarkupChanged;
  FMarkupNodes.MarkupInfoTempEnabled.OnChange := @MarkupChanged;

  DecPaintLock;
end;

destructor TSynEditMarkupIfDef.Destroy;
begin
  inherited Destroy;
  FIfDefTree.DiscardOpeningList(FOuterLines);

  FIfDefTree.UnRegisterNotification(itnUnlocking, @DoTreeUnlocking);
  FIfDefTree.UnRegisterNotification(itnUnlocked, @DoTreeUnlocked);
  FIfDefTree.UnRegisterNotification(itnChanged, @DoTreeChanged);
  FreeAndNil(FMarkupNodes);
  FreeAndNil(FIfDefTree);
  FreeAndNil(FMarkupEnabled);
  FreeAndNil(FMarkupTemp);
  FreeAndNil(FMarkupEnabledTemp);
end;

procedure TSynEditMarkupIfDef.IncPaintLock;
begin
  inherited IncPaintLock;
  FIfDefTree.LockTree;
end;

procedure TSynEditMarkupIfDef.DecPaintLock;
begin
  inherited DecPaintLock;
  FIfDefTree.UnLockTree;
end;

procedure TSynEditMarkupIfDef.PrepareMarkupForRow(aRow: Integer);
begin
  FMarkupEnabled.PrepareMarkupForRow(aRow);
  FMarkupNodes.PrepareMarkupForRow(aRow);
  FMarkupTemp.PrepareMarkupForRow(aRow);
  FMarkupEnabledTemp.PrepareMarkupForRow(aRow);
  inherited PrepareMarkupForRow(aRow);
end;

procedure TSynEditMarkupIfDef.FinishMarkupForRow(aRow: Integer);
begin
  FMarkupEnabled.FinishMarkupForRow(aRow);
  FMarkupNodes.FinishMarkupForRow(aRow);
  FMarkupTemp.FinishMarkupForRow(aRow);
  FMarkupEnabledTemp.FinishMarkupForRow(aRow);
  inherited FinishMarkupForRow(aRow);
end;

procedure TSynEditMarkupIfDef.EndMarkup;
begin
  FMarkupEnabled.EndMarkup;
  FMarkupNodes.EndMarkup;
  FMarkupTemp.EndMarkup;
  FMarkupEnabledTemp.EndMarkup;
  inherited EndMarkup;
end;

procedure TSynEditMarkupIfDef.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out
  ANextPhys, ANextLog: Integer);
begin
  inherited GetNextMarkupColAfterRowCol(aRow, aStartCol, AnRtlInfo, ANextPhys, ANextLog);
  FMarkupEnabled.GetNextMarkupColAfterRowColEx(aRow, aStartCol, AnRtlInfo, ANextPhys, ANextLog);
  FMarkupNodes.GetNextMarkupColAfterRowColEx(aRow, aStartCol, AnRtlInfo, ANextPhys, ANextLog);
  FMarkupTemp.GetNextMarkupColAfterRowColEx(aRow, aStartCol, AnRtlInfo, ANextPhys, ANextLog);
  FMarkupEnabledTemp.GetNextMarkupColAfterRowColEx(aRow, aStartCol, AnRtlInfo, ANextPhys, ANextLog);
end;

procedure TSynEditMarkupIfDef.MergeMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol, AEndCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo;
  AMarkup: TSynSelectedColorMergeResult);
begin
  FMarkupEnabled.MergeMarkupAttributeAtRowCol(aRow, aStartCol, AEndCol, AnRtlInfo, AMarkup);
  FMarkupNodes.MergeMarkupAttributeAtRowCol(aRow, aStartCol, AEndCol, AnRtlInfo, AMarkup);
  FMarkupTemp.MergeMarkupAttributeAtRowCol(aRow, aStartCol, AEndCol, AnRtlInfo, AMarkup);
  FMarkupEnabledTemp.MergeMarkupAttributeAtRowCol(aRow, aStartCol, AEndCol, AnRtlInfo, AMarkup);
  inherited MergeMarkupAttributeAtRowCol(aRow, aStartCol, AEndCol, AnRtlInfo, AMarkup);
end;

procedure TSynEditMarkupIfDef.InvalidateAll;
begin
  FIfDefTree.Clear;
end;

procedure TSynEditMarkupIfDef.SetNodeState(ALinePos, AstartPos: Integer;
  AState: TSynMarkupIfdefNodeState);
begin
  if (Highlighter = nil) then
    exit;
  PrepareHighlighter;
  FIfDefTree.SetNodeState(ALinePos, AstartPos, AState);
end;


{$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
var OldAssert: TAssertErrorProc = @SysAssert;
Procedure MyAssert(const Msg,FName:ShortString;LineNo:Longint;ErrorAddr:Pointer);
var
  t: TSynMarkupHighIfDefLinesTree;
begin
debugln('################# '+Msg);
if DebugCurTreeForAssert <> nil then begin
  t := DebugCurTreeForAssert;
  DebugCurTreeForAssert:= nil;
  t.DebugPrint(true);
end;
  if OldAssert <> nil
  then OldAssert(Msg, FName, LineNo, ErrorAddr)
  else halt(0);
end;
{$ENDIF}

{$IFDEF WITH_SYN_DEBUG_MARKUP_IFDEF}
initialization
  OldAssert := AssertErrorProc;
  AssertErrorProc := @MyAssert;
{$ENDIF}

end.


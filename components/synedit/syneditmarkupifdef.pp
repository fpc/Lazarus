(* SynEditMarkupIfDef

 Provides a framework to high-(low-)light "{$IFDEF  }" blocks. This unit is directly
 bound to the pascal Highlighter.
 The evaluation of IFDEF expression must be done by user code.

 A differential AVL tree is build, with a Node for each line having "IFDEF" nodes.
 The nodes contain a list of all IFDEF on the line.

 This allows to quickly find and insert/delete nodes, as well as move nodes
 (change line), if text is edited.

 SynEdit may to multiple edits, between the requests to validate, and not all
 edited areas may need to be validated everytime.
 Therefore the goal is that edit can invalidate, with very little effort, and
 work is done during validation.


 Each line has a link to the next outer (nesting) line: "OuterNestingLine"
  - The outer NestingLine is for the first node in the line. This means that the
    OuterNestingLine of an OuterNestingLine does not always nest the originating
    node
       1:  {$IFDEF }
       2:    {$IFDEF }              // OuterNestingLine = 1
       3:    {$ENDIF}  {$IFDEF }    // OuterNestingLine = 2 (the start of the line is inside the IFDEF from line 2)
       4:      {$IFDEF}                    // OuterNestingLine = 3
   Line 4 is NOT nested by line 2
  - The OuterNestingLine can be Invalidated by setting HighestValidNestedLine.
    All nodes with a line after HighestValidNestedLine will recalculate their
    OuterNestingLine.
    If a node is no longer needed, it must be kept in a disposed nodes list,
    since other node's OuterNestingLine can still refer to them.
    It can be re-used, since all nodes up to HighestValidNestedLine will have
    valid OuterNestingLine.
    Before freeing any nodes, a GarbageCollection must run through all nodes and
    remove references.


  Ifdef/Endif pairs/tripples are double linked. They move with the lines.
  In case a node is inserted, it must go through it's outer lines and for all
  nodes with forward refernces disolve the linked pairs.
  For Speed it can flag the need to do so. (idlValidForPostPeers)


*)
unit SynEditMarkupIfDef;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SynEditMarkup, SynEditMiscClasses, SynHighlighterPas, SynEditMarkupHighAll,
  SynEditHighlighterFoldBase, SynEditFoldedView, LazSynEditText, SynEditMiscProcs, LazClasses,
  LazLoggerBase, Graphics;

type

  { TSynRefCountedDict }

  TSynRefCountedDict = class(TRefCountedObject)
  private
    FDict: TSynSearchDictionary; // used to check for single line nodes (avoid using highlighter)
  public
    constructor Create;
    destructor Destroy; override;
    property Dict: TSynSearchDictionary read FDict;
  end;


  TSynMarkupHighIfDefLinesNode = class;
  TSynMarkupHighIfDefLinesTree = class;

  { TSynMarkupHighIfDefEntry - Information about a single $IfDef/$Else/$EndIf }

  SynMarkupIfDefNodeFlag = (
    idnIfdef, idnElse, idnEndIf,
    //idnCommentedIfdef, // Keep Ifdef if commented
    idnMultiLineTag,                // The "{$IFDEF ... }" wraps across lines.
                                    // Only allowed on last node AND if FLine.FEndLineOffs > 0
    idnEnabled, idnDisabled
  );
  SynMarkupIfDefNodeFlags = set of SynMarkupIfDefNodeFlag;

  PSynMarkupHighIfDefEntry = ^TSynMarkupHighIfDefEntry;
  TSynMarkupHighIfDefEntry = class
  private
    FLine: TSynMarkupHighIfDefLinesNode;
    FNodeFlags: SynMarkupIfDefNodeFlags;
    FPeer1, FPeer2: TSynMarkupHighIfDefEntry;
    FStartColumn, FEndColumn: Integer;
    function GetIsDisabled: Boolean;
    function GetIsEnabled: Boolean;
    function GetNeedsEnabledCheck: Boolean;
    function  GetPeerField(AIndex: SynMarkupIfDefNodeFlag): PSynMarkupHighIfDefEntry;
    function  GetPeer(AIndex: SynMarkupIfDefNodeFlag): TSynMarkupHighIfDefEntry;
    procedure SetIsDisabled(AValue: Boolean);
    procedure SetIsEnabled(AValue: Boolean);
    procedure SetNeedsEnabledCheck(AValue: Boolean);
    procedure SetPeer(AIndex: SynMarkupIfDefNodeFlag; AValue: TSynMarkupHighIfDefEntry);
    procedure ClearPeerField(APeer: PSynMarkupHighIfDefEntry);
    procedure RemoveForwardPeers;
  public
    //constructor Create;
    destructor Destroy; override;
    procedure ClearPeers;
    procedure MaybeInvalidatePeers;

    function  NodeType: SynMarkupIfDefNodeFlag;
    property  IsEnabled: Boolean read GetIsEnabled write SetIsEnabled;
    property  IsDisabled: Boolean read GetIsDisabled write SetIsDisabled;
    property  NeedsEnabledCheck: Boolean read GetNeedsEnabledCheck write SetNeedsEnabledCheck; // Can only be set to True
    property NodeFlags: SynMarkupIfDefNodeFlags read FNodeFlags write FNodeFlags;

    property Line: TSynMarkupHighIfDefLinesNode read FLine;
    property StartColumn: Integer read FStartColumn write FStartColumn;
    property EndColumn:   Integer read FEndColumn write FEndColumn;
// COMMENT  BEFORE  AUTO  COMPLETE !!!!!
    property IfDefPeer: TSynMarkupHighIfDefEntry index idnIfdef read GetPeer write SetPeer;
    property ElsePeer: TSynMarkupHighIfDefEntry  index idnElse  read GetPeer write SetPeer;
    property EndIfPeer: TSynMarkupHighIfDefEntry index idnEndIf read GetPeer write SetPeer;
  end;

  { TSynMarkupHighIfDefLinesNode - List of all nodes on the same line }

  SynMarkupIfDefLineFlag = (
    //idlScannedFrom,  idlScannedTo,
    idlValid,
    idlValidForPostPeers, // peer referentials from following lines are valid

    //idlChanged, idlChangedAtStart, idlChangedEnd,  // Flag lines for which to send invaldate
    //idlHasUnknowNodes,

    idlEnabledAtStart, idlDisabledAtStart,
    idlEnabledAtEnd, idlDisabledAtend,
    idlContineousEnabled, idlContineousDisabled,

    idlDisposed,          // Node is disposed, may be re-used
    idlInGlobalClear      // Skip unlinking Peers
  );
  SynMarkupIfDefLineFlags = set of SynMarkupIfDefLineFlag;

  TSynMarkupHighIfDefLinesNode = class(TSynSizedDifferentialAVLNode)
  private
    FEntries: Array of TSynMarkupHighIfDefEntry;
    FEntryCount: Integer;

    FLastEntryEndLineOffs: Integer;
    FHighestValidNestedNode: TSynMarkupHighIfDefLinesNode;
    FLineFlags: SynMarkupIfDefLineFlags;
    FOuterNestingNode: TSynMarkupHighIfDefLinesNode;
    FScanEndOffs: Integer;
    function GetEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
    function GetEntryCapacity: Integer;
    function GetId: PtrUInt;
    procedure SetEntry(AIndex: Integer; AValue: TSynMarkupHighIfDefEntry);
    procedure SetEntryCapacity(AValue: Integer);
    procedure SetEntryCount(AValue: Integer);
  protected
    procedure AdjustPositionOffset(AnAdjustment: integer); // Caller is responsible for staying between neighbours
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeDisposed;
    procedure MaybeInvalidatePeers;
    property LineFlags: SynMarkupIfDefLineFlags read FLineFlags;
    // LastEntryEndLineOffs: For last Entry only, if entry closing "}" is on a diff line. (can go one OVER ScanEndOffs)
    property LastEntryEndLineOffs: Integer read FLastEntryEndLineOffs write FLastEntryEndLineOffs;
    // ScanEndOffs: How many (empty) lines were scanned after this node
    property ScanEndOffs: Integer read FScanEndOffs write FScanEndOffs;
    // HighestValidNestedNode: Tte last (highest line num) node that is valid to refer to this node as OuterNestingLine
    property HighestValidNestedNode: TSynMarkupHighIfDefLinesNode read FHighestValidNestedNode write FHighestValidNestedNode;
    // OuterNestingNode: Line that contains the next outer IFDEF, in which this line is nested
    property OuterNestingNode: TSynMarkupHighIfDefLinesNode read FOuterNestingNode write FOuterNestingNode;
    property Id: PtrUInt read GetId;
    //property OpenedCount: Integer read FOpenedCount;
    //property ClosedCount: Integer read FClosedCount;
  public
    function AddEntry: TSynMarkupHighIfDefEntry;
    procedure DeletEntry(AIndex: Integer; AFree: Boolean = false);
    procedure ReduceCapacity;
    property EntryCount: Integer read FEntryCount write SetEntryCount;
    property EntryCapacity: Integer read GetEntryCapacity write SetEntryCapacity;
    property Entry[AIndex: Integer]: TSynMarkupHighIfDefEntry read GetEntry write SetEntry; default;
  end;

  { TSynMarkupHighIfDefLinesNodeInfo }

  TSynMarkupHighIfDefLinesNodeInfo = object
  private
    FAtBOL: Boolean;
    FAtEOL: Boolean;
    FHighestValidNestedLine: Integer;
    FOuterNestingLine: Integer;
    FNode: TSynMarkupHighIfDefLinesNode;
    FTree: TSynMarkupHighIfDefLinesTree;
    FStartLine, Index: Integer; // Todo: Indek is not used
    FCacheNestMinimum, FCacheNestStart, FCacheNestEnd: Integer;
    function GetEndLineOffs: Integer;
    function GetEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
    function GetEntryCount: Integer;
    function GetHighestValidNestedLine: Integer;
    function GetHighestValidNestedNode: TSynMarkupHighIfDefLinesNodeInfo;
    function GetLineFlags: SynMarkupIfDefLineFlags;
    function GetNodeId: PtrUInt;
    function GetOuterNestingNode: TSynMarkupHighIfDefLinesNodeInfo;
    function GetOuterNestingLine: Integer;
    function GetScanEndLine: Integer;
    function GetScanEndOffs: Integer;
    procedure SetEndLineOffs(AValue: Integer);
    procedure SetHighestValidNestedNode(AValue: TSynMarkupHighIfDefLinesNodeInfo);
    procedure SetOuterNestingNode(AValue: TSynMarkupHighIfDefLinesNodeInfo);
    procedure SetScanEndLine(AValue: Integer);
    procedure SetScanEndOffs(AValue: Integer);
    procedure SetStartLine(AValue: Integer);  // Caller is responsible for staying between neighbours
  public
    procedure ClearInfo;
    function Precessor: TSynMarkupHighIfDefLinesNodeInfo;
    function Successor: TSynMarkupHighIfDefLinesNodeInfo;
  public
    procedure ClearNestCache;
    function NestMinimumDepthAtNode: Integer;
    function NestDepthAtNodeStart: Integer;
    function NestDepthAtNodeEnd: Integer;
  public
    function IsValid: Boolean;
    function HasNode: Boolean;
    property StartLine: Integer read FStartLine write SetStartLine;
    property LineFlags: SynMarkupIfDefLineFlags read GetLineFlags;
    property EndLineOffs: Integer read GetEndLineOffs write SetEndLineOffs;
    property ScanEndOffs: Integer read GetScanEndOffs write SetScanEndOffs;
    property ScanEndLine: Integer read GetScanEndLine write SetScanEndLine;
    function ValidToLine(ANextNode: TSynMarkupHighIfDefLinesNodeInfo): Integer; // ScanEndLine or next node

    function IsValidOuterNode(ANode: TSynMarkupHighIfDefLinesNodeInfo): Boolean;
    procedure ClearHighestValidNestedNode;
    property HighestValidNestedNode: TSynMarkupHighIfDefLinesNodeInfo read GetHighestValidNestedNode write SetHighestValidNestedNode;
    property HighestValidNestedLine: Integer read GetHighestValidNestedLine;
    property OuterNestingLine: Integer read GetOuterNestingLine;
    property OuterNestingNode: TSynMarkupHighIfDefLinesNodeInfo read GetOuterNestingNode write SetOuterNestingNode;

    //function AddEntry: TSynMarkupHighIfDefEntry;
    //procedure DeletEntry(AIndex: Integer; AFree: Boolean = false);
    property EntryCount: Integer read GetEntryCount;
    property Entry[AIndex: Integer]: TSynMarkupHighIfDefEntry read GetEntry; // write SetEntry;
    property AtBOL: Boolean read FAtBOL;
    property AtEOL: Boolean read FAtEOL;
    property Node: TSynMarkupHighIfDefLinesNode read FNode;
    property NodeId: PtrUInt read GetNodeId;
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
    procedure SetNodes(ALow, AHigh: Integer; AValue: TSynMarkupHighIfDefLinesNodeInfo);
  public
    property Count: Integer read GetCount write SetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Node[AIndex: Integer]: TSynMarkupHighIfDefLinesNodeInfo read GetNode write SetNode;
    property Nodes[ALow, AHigh: Integer]: TSynMarkupHighIfDefLinesNodeInfo write SetNodes;
    // Set OuterLine, and HighestValidNestedNode
    procedure FixOuterLineForNode(var ANode: TSynMarkupHighIfDefLinesNodeInfo); // "var" so carhe is updated
    Procedure PushNodeLine(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
    procedure dbg;
  end;

  { TSynMarkupHighIfDefLinesTree }

  TSynMarkupHighIfDefLinesTree = class(TSynSizedDifferentialAVLTree)
  private
    FHighlighter: TSynPasSyn;
    FLines: TSynEditStrings;
    FClearing: Boolean;
    FDisposedNodes: TSynMarkupHighIfDefLinesNode;
    procedure SetHighlighter(AValue: TSynPasSyn);
    procedure SetLines(AValue: TSynEditStrings);
  private
    procedure MaybeValidateNode(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
    procedure MaybeExtendNodeBackward(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
      AStopAtLine: Integer = 0);
    procedure MaybeExtendNodeForward(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
      var ANextNode: TSynMarkupHighIfDefLinesNodeInfo;
      AStopBeforeLine: Integer = -1);
    procedure ScanOuterNesting(ANode: TSynMarkupHighIfDefLinesNodeInfo;
                               out ANestList: TSynMarkupHighIfDefLinesNodeInfoList);
    procedure ConnectPeers(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
                           var ANestList: TSynMarkupHighIfDefLinesNodeInfoList);
  protected
    function  CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode; override;
    procedure DisposeNode(var ANode: TSynSizedDifferentialAVLNode); override;
  public
    constructor Create;
    destructor Destroy; override;
    function  CheckLineForNodes(ALine: Integer): Boolean;
    procedure ScanLine(ALine: Integer; var ANodeForLine: TSynMarkupHighIfDefLinesNode);
    procedure ValidateRange(AStartLine, AEndLine: Integer);
  public
    procedure DebugPrint(Flat: Boolean = False);
    procedure Clear; override;
    function FindNodeAtPosition(ALine: Integer;
                                AMode: TSynSizedDiffAVLFindMode): TSynMarkupHighIfDefLinesNodeInfo;
                                overload;
    property Highlighter: TSynPasSyn read FHighlighter write SetHighlighter;
    property Lines : TSynEditStrings read FLines write SetLines;
  end;

  { TSynEditMarkupIfDef }

  TSynEditMarkupIfDef = class(TSynEditMarkup)
  private
    FFoldView: TSynEditFoldedView;
    FHighlighter: TSynPasSyn;
    FIfDefTree: TSynMarkupHighIfDefLinesTree;
    FNeedValidate, FNeedValidatePaint: Boolean;

    procedure SetFoldView(AValue: TSynEditFoldedView);
    procedure SetHighlighter(AValue: TSynPasSyn);

    Procedure ValidateMatches(SkipPaint: Boolean = False);
  protected
    procedure DoFoldChanged(aLine: Integer);
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override; // 1 based
    procedure DoVisibleChanged(AVisible: Boolean); override;
    procedure SetLines(const AValue: TSynEditStrings); override;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure IncPaintLock; override;
    procedure DecPaintLock; override;

    //procedure PrepareMarkupForRow(aRow: Integer); override;
    //procedure EndMarkup; override;
    //function GetMarkupAttributeAtRowCol(const aRow: Integer;
    //                                    const aStartCol: TLazSynDisplayTokenBound;
    //                                    const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    //procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
    //                                     const aStartCol: TLazSynDisplayTokenBound;
    //                                     const AnRtlInfo: TLazSynDisplayRtlInfo;
    //                                     out   ANextPhys, ANextLog: Integer); override;

    // AFirst/ ALast are 1 based
    //Procedure Invalidate(SkipPaint: Boolean = False);
    //Procedure InvalidateLines(AFirstLine: Integer = 0; ALastLine: Integer = 0; SkipPaint: Boolean = False);
    //Procedure SendLineInvalidation(AFirstIndex: Integer = -1;ALastIndex: Integer = -1);

    property FoldView: TSynEditFoldedView read FFoldView write SetFoldView;
    property Highlighter: TSynPasSyn read FHighlighter write SetHighlighter;
  end;

function dbgs(AFlag: SynMarkupIfDefLineFlag): String; overload;
function dbgs(AFlags: SynMarkupIfDefLineFlags): String; overload;
function dbgs(AFlag: SynMarkupIfDefNodeFlag): String; overload;
function dbgs(AFlags: SynMarkupIfDefNodeFlags): String; overload;

implementation

var
  TheDict: TSynRefCountedDict = nil;

procedure MaybeCreateDict;
begin
  if TheDict = nil then
    TheDict := TSynRefCountedDict.Create;
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

function dbgs(AFlag: SynMarkupIfDefNodeFlag): String;
begin
  Result := '';
  WriteStr(Result, AFlag);
end;

function dbgs(AFlags: SynMarkupIfDefNodeFlags): String;
var
  i: SynMarkupIfDefNodeFlag;
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
  Assert((AIndex < Count) and (  AIndex >= 0), 'TSynMarkupHighIfDefLinesNodeInfoList.GetNode Index');
  Result := FNestOpenNodes[  AIndex];
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
  Assert((  AIndex < Count) and (  AIndex >= 0), 'TSynMarkupHighIfDefLinesNodeInfoList.SetNode Index');
  FNestOpenNodes[  AIndex] := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfoList.SetNodes(ALow, AHigh: Integer;
  AValue: TSynMarkupHighIfDefLinesNodeInfo);
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

procedure TSynMarkupHighIfDefLinesNodeInfoList.FixOuterLineForNode(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
var
  j: Integer;
begin
  j := ANode.NestDepthAtNodeStart;
  if j = 0 then
    exit;
  ANode.OuterNestingNode := Node[j];
  if (not Node[j].HighestValidNestedNode.HasNode) or
     (Node[j].HighestValidNestedNode.StartLine < ANode.StartLine)
  then
    Node[j].HighestValidNestedNode := ANode;
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
    if Node[i].HasNode then  DbgOut(['(', Node[i].StartLine, ', ', Node[i].OuterNestingLine, ',', Node[i].HighestValidNestedLine, ')']);
  end;
  DebugLn();
end;

{ TSynRefCountedDict }

constructor TSynRefCountedDict.Create;
begin
  FDict := TSynSearchDictionary.Create;
  FDict.Add('{$if',     1);
  FDict.Add('{$ifdef',  1);
  FDict.Add('{$ifndef', 1);
  FDict.Add('{$ifopt',  1);
  FDict.Add('{$endif',  3);
  FDict.Add('{$else',   2);
end;

destructor TSynRefCountedDict.Destroy;
begin
  FDict.Free;
  inherited Destroy;
end;

{ TSynMarkupHighIfDefEntry }

function TSynMarkupHighIfDefEntry.NodeType: SynMarkupIfDefNodeFlag;
begin
  if idnIfdef in FNodeFlags then
    Result := idnIfdef
  else
  if idnElse in FNodeFlags then
    Result := idnElse
  else
  if idnEndIf in FNodeFlags then
    Result := idnEndIf
  else
    assert(false, 'Bad node type');
end;

function TSynMarkupHighIfDefEntry.GetPeerField(  AIndex: SynMarkupIfDefNodeFlag): PSynMarkupHighIfDefEntry;
begin
  Result := nil;
  case NodeType of
    idnIfdef:
      case   AIndex of
        idnIfdef: assert(false, 'Invalid node state for getting peer field');
        idnElse:  Result := @FPeer1;
        idnEndIf: Result := @FPeer2;
      end;
    idnElse:
      case   AIndex of
        idnIfdef: Result := @FPeer1;
        idnElse:  assert(false, 'Invalid node state for getting peer field');
        idnEndIf: Result := @FPeer2;
      end;
    idnEndIf:
      case   AIndex of
        idnIfdef: Result := @FPeer1;
        idnElse:  Result := @FPeer2;
        idnEndIf: assert(false, 'Invalid node state for getting peer field');
      end;
  end;
end;

function TSynMarkupHighIfDefEntry.GetIsDisabled: Boolean;
begin
  Result := idnDisabled in FNodeFlags;
end;

function TSynMarkupHighIfDefEntry.GetIsEnabled: Boolean;
begin
  Result := idnEnabled in FNodeFlags;
end;

function TSynMarkupHighIfDefEntry.GetNeedsEnabledCheck: Boolean;
begin
  Result := FNodeFlags * [idnEnabled, idnDisabled] = [];
end;

function TSynMarkupHighIfDefEntry.GetPeer(  AIndex: SynMarkupIfDefNodeFlag): TSynMarkupHighIfDefEntry;
begin
  Result := GetPeerField(  AIndex)^;
end;

procedure TSynMarkupHighIfDefEntry.SetIsDisabled(AValue: Boolean);
begin
  if IsDisabled = AValue then exit;
  IsEnabled := not AValue;
end;

procedure TSynMarkupHighIfDefEntry.SetIsEnabled(AValue: Boolean);
begin
  if IsEnabled = AValue then exit;
  if AValue then begin
    Include(FNodeFlags, idnEnabled);
    Exclude(FNodeFlags, idnDisabled);
  end
  else begin
    Exclude(FNodeFlags, idnEnabled);
    Include(FNodeFlags, idnDisabled);
  end;
end;

procedure TSynMarkupHighIfDefEntry.SetNeedsEnabledCheck(AValue: Boolean);
begin
  if (NeedsEnabledCheck = AValue) or (not AValue) then exit;
  Exclude(FNodeFlags, idnEnabled);
  Exclude(FNodeFlags, idnDisabled);
end;

procedure TSynMarkupHighIfDefEntry.SetPeer(  AIndex: SynMarkupIfDefNodeFlag;
  AValue: TSynMarkupHighIfDefEntry);
var
  PeerField, OthersField: PSynMarkupHighIfDefEntry;
begin
  PeerField := GetPeerField(  AIndex);
  if PeerField^ = AValue then
    Exit;

  if PeerField^ <> nil then begin
    OthersField := PeerField^.GetPeerField(NodeType);
    assert(OthersField^ = self, 'Peer does not point back to self');
    OthersField^ := nil;
    if NodeType = idnIfdef then
      PeerField^.NodeFlags := PeerField^.NodeFlags - [idnEnabled, idnDisabled];
  end;

  PeerField^ := AValue;

  if PeerField^ <> nil then begin
    OthersField := PeerField^.GetPeerField(NodeType);
    assert(OthersField^ = nil, 'Peer is not empty');
    OthersField^ := self;
    if NodeType = idnIfdef then begin
      Assert(PeerField^.NodeFlags * [idnEnabled, idnDisabled] = [], 'PeerField^.NodeFlags * [idnEnabled, idnDisabled] = []');
      if (PeerField^.NodeType = idnElse) and (not NeedsEnabledCheck) then
        PeerField^.IsEnabled := not IsEnabled;
    end;
  end;
end;

procedure TSynMarkupHighIfDefEntry.ClearPeerField(APeer: PSynMarkupHighIfDefEntry);
begin
  if APeer^ = nil then exit;
  if APeer^.NodeType <> idnIfdef then
    APeer^.NodeFlags := APeer^.NodeFlags - [idnEnabled, idnDisabled];

  if APeer^.FPeer1 = self then
    APeer^.FPeer1  := nil
  else
  if APeer^.FPeer2 = self then
    APeer^.FPeer2  := nil
  else
    assert(false, 'ClearPeerField did not find back reference');
  APeer^ := nil;
end;

procedure TSynMarkupHighIfDefEntry.RemoveForwardPeers;
begin
  case NodeType of
    idnIfdef: begin
        SetPeer(idnElse, nil);
        SetPeer(idnEndIf, nil);
      end;
    idnElse: begin
        SetPeer(idnEndIf, nil);
      end;
    idnEndIf: begin
      end;
  end;
end;

destructor TSynMarkupHighIfDefEntry.Destroy;
begin
  if (FLine <> nil) and not(idlInGlobalClear in FLine.LineFlags) then
    ClearPeers;
  inherited Destroy;
end;

procedure TSynMarkupHighIfDefEntry.ClearPeers;
begin
  ClearPeerField(@FPeer1);
  ClearPeerField(@FPeer2);
  if NodeType <> idnIfdef then
    NodeFlags := NodeFlags - [idnEnabled, idnDisabled];
end;

procedure TSynMarkupHighIfDefEntry.MaybeInvalidatePeers;
begin
  FLine.MaybeInvalidatePeers;
  if FPeer1 <> nil then
    FPeer1.FLine.MaybeInvalidatePeers;
  if FPeer2 <> nil then
    FPeer2.FLine.MaybeInvalidatePeers;
end;

{ TSynMarkupHighIfDefLinesNode }

function TSynMarkupHighIfDefLinesNode.GetEntry(  AIndex: Integer): TSynMarkupHighIfDefEntry;
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

procedure TSynMarkupHighIfDefLinesNode.SetEntry(  AIndex: Integer;
  AValue: TSynMarkupHighIfDefEntry);
begin
  FEntries[  AIndex] := AValue;
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
  Assert(GetPosition + AnAdjustment < Successor.GetPosition, 'GetPosition + AnAdjustment < Successor.GetPosition');
  Assert(GetPosition + AnAdjustment > Precessor.GetPosition, 'GetPosition + AnAdjustment > Precessor.GetPosition');
  FPositionOffset := FPositionOffset + AnAdjustment;
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

procedure TSynMarkupHighIfDefLinesNode.MakeDisposed;
begin
  FLineFlags := [idlDisposed];
  while EntryCount > 0 do
    DeletEntry(EntryCount-1);
  if (FHighestValidNestedNode <> nil) and
     (FHighestValidNestedNode.OuterNestingNode = Self)
  then
    FHighestValidNestedNode.OuterNestingNode := nil;
end;

procedure TSynMarkupHighIfDefLinesNode.MaybeInvalidatePeers;
var
  i: Integer;
begin
  if idlValidForPostPeers in FLineFlags then
    exit;
  for i := 0 to EntryCount - 1 do
    Entry[i].RemoveForwardPeers;
  Include(FLineFlags, idlValidForPostPeers);
end;

function TSynMarkupHighIfDefLinesNode.AddEntry: TSynMarkupHighIfDefEntry;
var
  c: Integer;
begin
  c := EntryCount;
  EntryCount := c + 1;
  assert(FEntries[c]=nil, 'FEntries[c]=nil');
  Result := TSynMarkupHighIfDefEntry.Create;
  FEntries[c] := Result;
end;

procedure TSynMarkupHighIfDefLinesNode.DeletEntry(  AIndex: Integer; AFree: Boolean);
begin
  Assert((  AIndex >= 0) and (  AIndex < FEntryCount), 'DeletEntry');
  if AFree then
    FEntries[  AIndex].Free
  else
    FEntries[  AIndex] := nil;
  while   AIndex < FEntryCount - 1 do begin
    FEntries[  AIndex] := FEntries[  AIndex + 1];
    inc(  AIndex);
  end;
  dec(FEntryCount);
end;

procedure TSynMarkupHighIfDefLinesNode.ReduceCapacity;
begin
  EntryCapacity := EntryCount;
end;

{ TSynMarkupHighIfDefLinesNodeInfo }

procedure TSynMarkupHighIfDefLinesNodeInfo.SetStartLine(AValue: Integer);
begin
  Assert(FNode <> nil, 'TSynMarkupHighIfDefLinesNodeInfo.SetStartLine has node');
  if FStartLine = AValue then Exit;
  FNode.AdjustPositionOffset(AValue - FStartLine);
  FStartLine := AValue;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetLineFlags: SynMarkupIfDefLineFlags;
begin
  if not HasNode then
    exit([]);
  Result := FNode.LineFlags;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetNodeId: PtrUInt;
begin
  if not HasNode then
    exit(0);
  Result := FNode.Id;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetOuterNestingNode: TSynMarkupHighIfDefLinesNodeInfo;
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.GetOuterNestingNode');

  Result := Self;
  Result.ClearInfo;
  Result.FNode := Node.OuterNestingNode;
  Result.FStartLine := OuterNestingLine;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetEndLineOffs: Integer;
begin
  if not HasNode then
    exit(0);
  Result := FNode.LastEntryEndLineOffs;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetEntry(AIndex: Integer): TSynMarkupHighIfDefEntry;
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.GetEntry');
  Result := FNode.Entry[AIndex];
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetEntryCount: Integer;
begin
  if not HasNode then
    exit(0);
  Result := FNode.EntryCount;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetHighestValidNestedLine: Integer;
begin
  if not HasNode then
    exit(0);
  if FHighestValidNestedLine < 0 then begin
    if Node.HighestValidNestedNode = nil then
      FHighestValidNestedLine := 0
    else
      FHighestValidNestedLine := Node.HighestValidNestedNode.GetPosition;
  end;
  assert((FHighestValidNestedLine = 0) or (FHighestValidNestedLine = Node.HighestValidNestedNode.GetPosition), 'FHighestValidNestedLine correct');
  Result := FHighestValidNestedLine;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetHighestValidNestedNode: TSynMarkupHighIfDefLinesNodeInfo;
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.GetHighestValidNestedNode');

  Result := Self;
  Result.ClearInfo;
  Result.FNode := Node.HighestValidNestedNode;
  Result.FStartLine := HighestValidNestedLine;
end;

function TSynMarkupHighIfDefLinesNodeInfo.GetOuterNestingLine: Integer;
begin
  if not HasNode then
    exit(0);
  if FOuterNestingLine < 0 then begin
    if Node.OuterNestingNode = nil then
      FOuterNestingLine := 0
    else
      FOuterNestingLine := Node.OuterNestingNode.GetPosition;
  end;
  assert((FOuterNestingLine = 0) or (FOuterNestingLine = Node.OuterNestingNode.GetPosition), 'FOuterNestingLine correct');
  Result := FOuterNestingLine;
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

procedure TSynMarkupHighIfDefLinesNodeInfo.SetEndLineOffs(AValue: Integer);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetEndLineOffs');
  FNode.LastEntryEndLineOffs := AValue;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetHighestValidNestedNode(AValue: TSynMarkupHighIfDefLinesNodeInfo);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetHighestValidNestedNode');
  if Node.HighestValidNestedNode = AValue.Node then exit;
  Node.HighestValidNestedNode := AValue.Node;
  FHighestValidNestedLine := AValue.StartLine;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetOuterNestingNode(AValue: TSynMarkupHighIfDefLinesNodeInfo);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetOuterNestingNode');
  if Node.OuterNestingNode = AValue.Node then exit;
  Node.OuterNestingNode := AValue.Node;
  FOuterNestingLine := AValue.StartLine;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetScanEndLine(AValue: Integer);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetScanEndLine');
  ScanEndOffs := AValue - StartLine;
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.SetScanEndOffs(AValue: Integer);
begin
  Assert(HasNode, 'HasNode for TSynMarkupHighIfDefLinesNodeInfo.SetScanEndOffs');
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

function TSynMarkupHighIfDefLinesNodeInfo.Precessor: TSynMarkupHighIfDefLinesNodeInfo;
begin
  ClearNestCache;
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
    Result.FNode := TSynMarkupHighIfDefLinesNode(FTree.First(Result.FStartLine, Result.Index));
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
    Result.FNode := TSynMarkupHighIfDefLinesNode(FTree.Last(Result.FStartLine, Result.Index));
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
  FHighestValidNestedLine := -1;
  FOuterNestingLine := -1;
end;

function TSynMarkupHighIfDefLinesNodeInfo.NestMinimumDepthAtNode: Integer;
begin
  assert(FTree <> nil, 'NestWinimumDepthAtNode has tree');
  if FCacheNestMinimum < 0 then
    FCacheNestMinimum :=
      FTree.FHighlighter.FoldBlockMinLevel(ToIdx(StartLine), FOLDGROUP_IFDEF,
                                           [sfbIncludeDisabled]);
  Result := FCacheNestMinimum;
end;

function TSynMarkupHighIfDefLinesNodeInfo.NestDepthAtNodeStart: Integer;
begin
  assert(FTree <> nil, 'NestDepthAtNodeStart has tree');
  if FCacheNestStart < 0 then
    FCacheNestStart :=
      FTree.FHighlighter.FoldBlockEndLevel(ToIdx(StartLine)-1, FOLDGROUP_IFDEF,
                                           [sfbIncludeDisabled]);
  Result := FCacheNestStart;
end;

function TSynMarkupHighIfDefLinesNodeInfo.NestDepthAtNodeEnd: Integer;
begin
  assert(FTree <> nil, 'NestDepthAtNodeEnd has tree');
  if FCacheNestEnd < 0 then
    FCacheNestEnd :=
      FTree.FHighlighter.FoldBlockEndLevel(ToIdx(StartLine), FOLDGROUP_IFDEF,
                                           [sfbIncludeDisabled]);
  Result := FCacheNestEnd;
end;

function TSynMarkupHighIfDefLinesNodeInfo.ValidToLine(ANextNode: TSynMarkupHighIfDefLinesNodeInfo): Integer;
begin
  if not HasNode then
    exit(-1);
  if not IsValid then
    exit(StartLine);

  if ScanEndOffs >= 0 then begin
    Result := StartLine + ScanEndOffs;
    assert((not ANextNode.HasNode) or (Result<ANextNode.StartLine), '(ANextNode=nil) or (Result<ANextNode.StartLine)');
  end
  else
  if ANextNode.HasNode then
    Result := ANextNode.StartLine - 1
  else
    Result := StartLine;
end;

function TSynMarkupHighIfDefLinesNodeInfo.IsValidOuterNode(ANode: TSynMarkupHighIfDefLinesNodeInfo): Boolean;
begin
  Result := (ANode.IsValid) and
            (ANode.HighestValidNestedLine >= StartLine);
end;

procedure TSynMarkupHighIfDefLinesNodeInfo.ClearHighestValidNestedNode;
begin
  Node.HighestValidNestedNode := nil;
  FHighestValidNestedLine := 0;
end;

function TSynMarkupHighIfDefLinesNodeInfo.IsValid: Boolean;
begin
  Result := HasNode and (idlValid in LineFlags);
end;

function TSynMarkupHighIfDefLinesNodeInfo.HasNode: Boolean;
begin
  Result := FNode <> nil;
end;

{ TSynMarkupHighIfDefLinesTree }

procedure TSynMarkupHighIfDefLinesTree.SetHighlighter(AValue: TSynPasSyn);
begin
  if FHighlighter = AValue then Exit;
  FHighlighter := AValue;
  Clear;
end;

procedure TSynMarkupHighIfDefLinesTree.SetLines(AValue: TSynEditStrings);
begin
  if FLines = AValue then Exit;
  FLines := AValue;
  Clear;
end;

procedure TSynMarkupHighIfDefLinesTree.MaybeValidateNode(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
begin
  Assert(ANode.HasNode, 'ANode.HasNode in MaybeValidateNode');
// TODO: search first
  if (not ANode.IsValid) then
    ScanLine(ANode.StartLine, ANode.FNode);
end;

procedure TSynMarkupHighIfDefLinesTree.MaybeExtendNodeBackward(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
  AStopAtLine: Integer);
var
  Line: Integer;
begin
  Assert(ANode.HasNode, 'ANode.HasNode in MaybeExtendNodeDownwards');
  MaybeValidateNode(ANode);
  if (ANode.EntryCount = 0) then begin
    Assert(not ANode.HighestValidNestedNode.HasNode, 'ANode.HighestValidPeerLineOffs in MaybeExtendNodeDownwards');
    // ANode is a Scan-Start-Marker and may be extended downto StartLine
    Line := ANode.StartLine;
    while Line > AStopAtLine do begin
      dec(Line);
      if CheckLineForNodes(Line) then begin
        ScanLine(Line, ANode.FNode);
        if ANode.EntryCount > 0 then
          break;
      end;
    end;
    ANode.StartLine := Line;
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
          exit;
        end;
      end;
    end;
    // Line is empty, include in offs
    ANode.ScanEndLine := Line;
end;

procedure TSynMarkupHighIfDefLinesTree.ScanOuterNesting(ANode: TSynMarkupHighIfDefLinesNodeInfo;
  out ANestList: TSynMarkupHighIfDefLinesNodeInfoList);

  function NestDepthAtLineStart(ALine: Integer): Integer; inline;
  begin
    Result := FHighlighter.FoldBlockEndLevel(ToIdx(ALine)-1, FOLDGROUP_IFDEF, [sfbIncludeDisabled]);
  end;
  function MinimumNestDepthAtLine(ALine: Integer): Integer; inline;
  begin
    Result := FHighlighter.FoldBlockMinLevel(ToIdx(ALine), FOLDGROUP_IFDEF, [sfbIncludeDisabled]);
  end;

  var
    NestedMinDepth: Integer;
  procedure AddNodeToNestList(ANodeToAdd: TSynMarkupHighIfDefLinesNodeInfo);
  var
    j, k: Integer;
  begin
    j := ANodeToAdd.NestMinimumDepthAtNode;
    k := NestedMinDepth; // NestDepthAtNodeEnd
    if j >= k then
      exit;
    ANestList.Nodes[j+1, k] := ANodeToAdd;
    NestedMinDepth := j;
  end;

var
  Line, MaxNest, i: Integer;
  NextNode, TmpNode: TSynMarkupHighIfDefLinesNodeInfo;
  NestedDepth: Integer;
  NestOpenNodes: TSynMarkupHighIfDefLinesNodeInfoList;
begin
  TmpNode.FTree := Self;
  NestedDepth := ANode.NestDepthAtNodeStart;
  NestedMinDepth := NestedDepth; // ANode.NestDepthAtNodeEnd;
  MaxNest := NestedDepth;
  //AddNodeToNestList(ANode);
  assert((ANode.OuterNestingLine <= 0) or (NestedDepth > 0), 'No outer Anode, if nested=0');

  while NestedDepth > 0 do begin
    Assert(ANode.IsValid, 'ANode.IsValid while looking for OuterNestingLine');

    if ANode.OuterNestingLine > 0 then begin
      NextNode := FindNodeAtPosition(ANode.OuterNestingLine, afmNil);
      if ANode.IsValidOuterNode(NextNode) then begin
        assert(NestedDepth > NextNode.NestDepthAtNodeStart, 'dec NestedDepth');
        ANode := NextNode;
        AddNodeToNestList(NextNode);
        NestedDepth := ANode.NestDepthAtNodeStart;
        Continue;
      end;
    end;

    // Search
    Line := ANode.StartLine - 1;
    i := MinimumNestDepthAtLine(Line);
    while (NestedDepth <= i) do begin
      dec(Line);
      i := MinimumNestDepthAtLine(Line);
      MaxNest := Max(MaxNest, NestDepthAtLineStart(Line))
    end;
    NestedDepth := i;

    NextNode := FindNodeAtPosition(Line, afmCreate);
    AddNodeToNestList(NextNode);
    assert((NextNode.OuterNestingLine <= 0) or (NextNode.IsValid and (NestedDepth > 0)), 'No outer Nextnode, if not valid');
    MaybeValidateNode(NextNode);
    ANode.SetOuterNestingNode(NextNode);
    Assert(NextNode.EntryCount > 0, 'NextNode.EntryCount > 0');

    // Update nodes inbetweeen
    NestOpenNodes.Capacity := Max(NestOpenNodes.Capacity, MaxNest + 1);
    TmpNode := NextNode;
    while TmpNode.Node <> ANode.Node do begin
      Assert(TmpNode.HasNode, 'TmpNode.HasNode - Update nodes inbetweeen');
      Assert(TmpNode.NestDepthAtNodeEnd > TmpNode.NestMinimumDepthAtNode, 'TmpNode.NestDepthAtNodeEnd >= TmpNode.NestMinimumDepthAtNode');

      NestOpenNodes.PushNodeLine(TmpNode);
      //NestOpenNodes.Nodes[TmpNode.NestMinimumDepthAtNode+1, TmpNode.NestDepthAtNodeEnd] := TmpNode;
      TmpNode := TmpNode.Successor;
      NestOpenNodes.FixOuterLineForNode(TmpNode);
    end;

    NextNode.HighestValidNestedNode := ANode;
    ANode := NextNode;
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.ConnectPeers(var ANode: TSynMarkupHighIfDefLinesNodeInfo;
  var ANestList: TSynMarkupHighIfDefLinesNodeInfoList);
var
  PeerList: array of TSynMarkupHighIfDefEntry;
  NestDepth, LowerNestDepth: Integer;
  i, j, OtherDepth: Integer;
  OtherLine: TSynMarkupHighIfDefLinesNodeInfo;
begin
  ANode.Node.MaybeInvalidatePeers;
  NestDepth := ANode.NestDepthAtNodeEnd;
  LowerNestDepth := MaxInt;
  SetLength(PeerList, NestDepth + ANode.EntryCount + 1);
  for i := ANode.EntryCount - 1 downto 0 do begin
    case ANode.Entry[i].NodeType of
      idnIfdef: begin
          if LowerNestDepth <= NestDepth then
            PeerList[NestDepth].IfDefPeer := ANode.Entry[i];  // update closing node
          dec(NestDepth);
        end;
      idnElse: begin
          if LowerNestDepth <= NestDepth then
            PeerList[NestDepth].ElsePeer := ANode.Entry[i];  // update closing node
          PeerList[NestDepth] := ANode.Entry[i];
          if LowerNestDepth > NestDepth then
            LowerNestDepth := NestDepth;
        end;
      idnEndIf: begin
          inc(NestDepth);
          PeerList[NestDepth] := ANode.Entry[i];
          if LowerNestDepth > NestDepth then
            LowerNestDepth := NestDepth;
        end;
    end;
  end;

    // Find peers in previous lines.
    // Opening (IfDef) nodes will be connected when there closing node is found.

  for i := NestDepth downto LowerNestDepth do begin
    PeerList[i].MaybeInvalidatePeers;
    case PeerList[i].NodeType of
      idnElse:  if PeerList[i].IfDefPeer <> nil then continue;
      idnEndIf: if PeerList[i].ElsePeer  <> nil then continue;
    end;

    OtherLine := ANestList.Node[i];
    OtherDepth := OtherLine.NestDepthAtNodeEnd;
    j := OtherLine.EntryCount;
    while j > 0 do begin
      dec(j);
      if OtherDepth = i then begin
        case OtherLine.Entry[j].NodeType of
          idnIfdef: begin
              PeerList[i].IfDefPeer := OtherLine.Entry[j];
              break;
            end;
          idnElse: begin
              PeerList[i].ElsePeer := OtherLine.Entry[j];
              break;
            end;
        end;
      end;
      case OtherLine.Entry[j].NodeType of
        idnIfdef:           inc(OtherDepth);
        idnElse: ; //
        idnEndIf:  dec(OtherDepth);
      end;
      //if OtherLine.Entry[j].NodeType  = idnElse then
      //  inc(OtherDepth);
    end;

  end;

    //  5 end  if  if  end  end 4
    //    5            6    5

    //  5 end  if  if if if  end  end 6
    //    5                  8    7

    //  5 end  end  end 2
    //    5    4    3
end;

function TSynMarkupHighIfDefLinesTree.CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode;
begin
  if FDisposedNodes <> nil then begin
    Result := FDisposedNodes;
    FDisposedNodes := TSynMarkupHighIfDefLinesNode(Result).FOuterNestingNode;
    TSynMarkupHighIfDefLinesNode(Result).FLineFlags := [];
  end
  else
    Result := TSynMarkupHighIfDefLinesNode.Create;
end;

procedure TSynMarkupHighIfDefLinesTree.DisposeNode(var ANode: TSynSizedDifferentialAVLNode);
begin
  if FClearing then begin
    Include(TSynMarkupHighIfDefLinesNode(ANode).FLineFlags, idlInGlobalClear);
    inherited DisposeNode(ANode);
  end
  else begin
    TSynMarkupHighIfDefLinesNode(ANode).MakeDisposed;
    // Use FOuterNestingNode to link dispossed nodes
    TSynMarkupHighIfDefLinesNode(ANode).FOuterNestingNode := FDisposedNodes;
    FDisposedNodes := TSynMarkupHighIfDefLinesNode(ANode);
  end;
end;

constructor TSynMarkupHighIfDefLinesTree.Create;
begin
  inherited Create;
  MaybeCreateDict;
  TheDict.AddReference;
end;

destructor TSynMarkupHighIfDefLinesTree.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(TheDict);
end;

function TSynMarkupHighIfDefLinesTree.CheckLineForNodes(ALine: Integer): Boolean;
var
  m, e: Integer;
  LineText, LineTextLower: String;
begin
  m := FHighlighter.FoldBlockMinLevel(ALine-1, FOLDGROUP_IFDEF, [sfbIncludeDisabled]);
  e := FHighlighter.FoldBlockEndLevel(ALine-1, FOLDGROUP_IFDEF, [sfbIncludeDisabled]);
  Result := (m < e);
  if (not Result) and (ALine > 1) then
    Result := (e > FHighlighter.FoldBlockEndLevel(ALine-2, FOLDGROUP_IFDEF, [sfbIncludeDisabled]));

  if not Result then begin
    LineText := Lines[ToIdx(ALine)];
    if LineText = '' then
      exit;
    LineTextLower := LowerCase(LineText);
    Result := TheDict.Dict.Search(@LineTextLower[1], Length(LineTextLower), nil) <> nil;
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.ScanLine(ALine: Integer;
  var ANodeForLine: TSynMarkupHighIfDefLinesNode);
var
  FoldNodeInfoList: TLazSynFoldNodeInfoList;
  LineTextLower: String;
  NodesAddedCnt: Integer;

  function FindCloseCurlyBracket(StartX: Integer; out ALineOffs: Integer): Integer;
  var
    CurlyLvl: Integer;
    i, l, c: Integer;
    s: String;
  begin
    Result := StartX;
    ALineOffs := 0;
    CurlyLvl := 1;

    l := Length(LineTextLower);
    while Result <= l do begin
      case LineTextLower[Result] of
        '{': inc(CurlyLvl);
        '}': if CurlyLvl = 1 then exit
             else dec(CurlyLvl);
      end;
      inc(Result);
    end;

    c := Lines.Count;
    i := ToIdx(ALine) + 1;
    inc(ALineOffs);
    while (i < c) do begin
      // TODO: get range flag fron pas highlighter
      s := Lines[i];
      l := Length(s);
      Result := 1;
      while Result <= l do begin
        case s[Result] of
          '{': inc(CurlyLvl);
          '}': if CurlyLvl = 1 then exit
               else dec(CurlyLvl);
        end;
        inc(Result);
      end;
      inc(i);
      inc(ALineOffs);
    end;

    Result := -1;
  end;

  function GetEntry(ALogStart, ALogEnd, ALineOffs: Integer;
    AType: SynMarkupIfDefNodeFlag): TSynMarkupHighIfDefEntry;
  var
    i: Integer;
  begin
    if ANodeForLine = nil then begin
      ANodeForLine := FindNodeAtPosition(ALine, afmCreate).FNode;
      ANodeForLine.EntryCapacity := FoldNodeInfoList.Count;
    end;
    if NodesAddedCnt >= ANodeForLine.EntryCount then begin
      Result := ANodeForLine.AddEntry;
    end
    else begin
      i := NodesAddedCnt;
      Result := ANodeForLine.Entry[i];
      while (i < ANodeForLine.EntryCount-1) and (Result.EndColumn < ALogStart) do begin // Last node in line can not be compared
        // TODO: check for commented notes, maybe keep
        ANodeForLine.DeletEntry(i, True);
        Result := ANodeForLine.Entry[i];
      end;
      // Check if existing node matches
      if (AType = idnIfdef) and // only keep inf for opening nodes
         (Result.NodeFlags * [idnEnabled, AType] = [idnEnabled, AType]) and
         (Result.StartColumn = ALogStart) and
         (Result.EndColumn = ALogEnd) and
         ((idnMultiLineTag in Result.NodeFlags) = (ALineOffs > 0)) and
         ( (ALineOffs = 0) or (ALineOffs = ANodeForLine.LastEntryEndLineOffs) )
      then begin        // Does match, keep as is
      end
      else begin        // Does NOT match
        //Result.ClearPeers;
        Result.NodeFlags := [];
      end;
      Result.ClearPeers;
    end;
    inc(NodesAddedCnt);
    Result.FLine := ANodeForLine;
    Result.StartColumn := ALogStart;
    Result.EndColumn   := ALogEnd;
    include(Result.FNodeFlags, AType);
  end;

var
  fn, fn2: TSynFoldNodeInfo;
  LogStartX, LogEndX, LineLen, LineOffs: Integer;
  Entry: TSynMarkupHighIfDefEntry;
  NType: SynMarkupIfDefNodeFlag;
  i, c: Integer;
begin
  FoldNodeInfoList := Highlighter.FoldNodeInfo[ToIdx(ALine)];
  FoldNodeInfoList.AddReference;
  FoldNodeInfoList.ActionFilter := []; //[sfaOpen, sfaClose];
  FoldNodeInfoList.GroupFilter := FOLDGROUP_IFDEF;

  if (ANodeForLine <> nil) and (ANodeForLine.EntryCapacity < FoldNodeInfoList.Count) then
    ANodeForLine.EntryCapacity := FoldNodeInfoList.Count;
  NodesAddedCnt := 0;

  LineTextLower := LowerCase(Lines[ToIdx(ALine)]);
  LineLen := Length(LineTextLower);

  i := -1;
  LogEndX := 0;
  c := FoldNodeInfoList.Count - 1;
  while i < c do begin
    inc(i);
    fn := FoldNodeInfoList[i];
    if sfaInvalid in fn.FoldAction then
      continue;

    LogStartX := ToPos(fn.LogXStart)-1;  // LogXStart is at "$", we need "{"
    if (LogStartX < 1) or (LogStartX > LineLen) then begin
      assert(false, '(LogStartX < 1) or (LogStartX > LineLen) ');
      continue;
    end;
    assert(LogStartX > LogEndX, 'ifdef xpos found before end of previous ifdef');

    LogEndX := FindCloseCurlyBracket(LogStartX+1, LineOffs);
    case TheDict.Dict.GetMatchAtChar(@LineTextLower[LogStartX], LineLen + 1 - LogStartX) of
      1: // ifdef
        begin
          assert(sfaOpen in fn.FoldAction, 'sfaOpen in fn.FoldAction');
          NType := idnIfdef;
        end;
      2: // else
        begin
          assert(i < c, '$ELSE i < c');
          inc(i);
          fn2 := FoldNodeInfoList[i];
          assert(sfaClose in fn.FoldAction, 'sfaClose in fn.FoldAction');
          assert(sfaOpen in fn2.FoldAction, 'sfaOpen in fn2.FoldAction');
          assert(fn.LogXStart = fn2.LogXStart, 'sfaOpen in fn2.FoldAction');
          NType := idnElse;
        end;
      3: // endif
        begin
          assert(sfaClose in fn.FoldAction, 'sfaOpen in fn.FoldAction');
          NType := idnEndIf;
        end;
      else
        begin
          assert(false, 'not found ifdef');
          continue;
        end;
    end;

    Entry := GetEntry(LogStartX, LogEndX, LineOffs, NType);

    if LineOffs > 0 then begin
      ANodeForLine.LastEntryEndLineOffs := LineOffs;
      Include(Entry.FNodeFlags, idnMultiLineTag);
      break;
    end;

  end;

  FoldNodeInfoList.ReleaseReference;
  if ANodeForLine <> nil then begin
    Include(ANodeForLine.FLineFlags, idlValid);
    ANodeForLine.EntryCount := NodesAddedCnt;
    ANodeForLine.ReduceCapacity;
    ANodeForLine.ScanEndOffs := Max(0, LineOffs-1);
    ANodeForLine.OuterNestingNode := nil;
  end;
end;

procedure TSynMarkupHighIfDefLinesTree.ValidateRange(AStartLine, AEndLine: Integer);
var
  NestList: TSynMarkupHighIfDefLinesNodeInfoList;

  procedure FixNodeOuterAndPeers(var ANode: TSynMarkupHighIfDefLinesNodeInfo);
  begin
    NestList.FixOuterLineForNode(ANode);

    ConnectPeers(ANode, NestList);

    if ANode.EntryCount > 0 then
      NestList.PushNodeLine(ANode);
  end;

var
  NextNode, Node, TmpNode: TSynMarkupHighIfDefLinesNodeInfo;
  NodeValidTo: Integer;
begin
  TmpNode.FTree := Self;

  (*** Find or create a node for StartLine ***)

  Node := FindNodeAtPosition(AStartLine, afmPrev); // might be multiline
  NextNode := Node.Successor;
  assert((not NextNode.HasNode) or (AStartLine < NextNode.StartLine), 'AStartLine < NextNode.StartLine');

  if NextNode.HasNode and
     ( (not Node.HasNode) or (AStartLine > Node.ValidToLine(NextNode)) )
  then begin
    // AStartLine has not been scanned yet
    if (not NextNode.IsValid) and (NextNode.EntryCount = 0) then begin
      // NextNode is a no longer used start-of-scan-marker => move it
      Node := NextNode;
      Node.StartLine := AStartLine;
      Node.ClearHighestValidNestedNode;
      NextNode.ClearInfo;
    end
    else
    if (NextNode.StartLine <= AEndLine) then begin
      MaybeExtendNodeBackward(NextNode, AStartLine);
      if NextNode.StartLine = AStartLine then begin
        Node := NextNode;
        NextNode.ClearInfo;
      end;
    end;
  end;

  if (not Node.HasNode) or (AStartLine > Node.ValidToLine(NextNode)) then
    Node := FindNodeAtPosition(AStartLine, afmCreate);
  MaybeValidateNode(Node);
  assert((AStartLine >= Node.StartLine) and (AStartLine <= Node.StartLine + Node.EndLineOffs), 'AStartLine is in Node');

  (*** Check outer nodes (nesting) ***)
  ScanOuterNesting(Node, NestList);
DbgOut('--'); NestList.dbg;


  (*** Scan to Endline ***)
  NextNode := Node.Successor;

  while (Node.HasNode) and (NextNode.HasNode) and
        (Node.ValidToLine(NextNode) < AEndLine) and
        (NextNode.StartLine <= AEndLine)
  do begin
    Assert(Node.IsValid, 'Node.IsValid while "Scan to Endline"');

    FixNodeOuterAndPeers(Node);
    MaybeValidateNode(NextNode);
    NodeValidTo := Node.ValidToLine(NextNode);
    MaybeExtendNodeBackward(NextNode, NodeValidTo);

    TmpNode.ClearInfo;
    MaybeExtendNodeForward(Node, TmpNode, NextNode.StartLine);
    assert(NextNode.StartLine > Node.ScanEndLine, 'NextNode.StartLine > Node.ScanEndLine');

    if NextNode.StartLine = Node.ScanEndLine + 1 then begin
      Assert(not TmpNode.HasNode, 'not TmpNode.HasNode');
      if NextNode.EntryCount = 0 then begin
        // Merge nodes
        Node.EndLineOffs := Node.EndLineOffs  + NextNode.EndLineOffs + 1;
        RemoveNode(NextNode.FNode);
        NextNode := Node.Successor;
        continue;
      end
      else begin
        Node := NextNode;
        NextNode := Node.Successor;
        continue;
      end;
    end
    else
    begin
      // scan gap
      Assert(TmpNode.HasNode, 'TmpNode.HasNode');
      Assert(NextNode.EntryCount > 0, 'NextNode.EntryCount > 0');
      Node := TmpNode;
      TmpNode.ClearInfo;
      while Node.ScanEndLine + 1 < NextNode.StartLine do begin
        MaybeExtendNodeForward(Node, TmpNode, NextNode.StartLine);
        if not TmpNode.HasNode then
          break;
        FixNodeOuterAndPeers(Node);
        assert(Node.ScanEndLine + 1 < NextNode.StartLine, 'Scan gap still before next node');
        Node := TmpNode;
        TmpNode.ClearInfo;
      end;
      assert(Node.ScanEndLine + 1 = NextNode.StartLine, 'Scan gap has reached next node');
      NextNode := Node.Successor;
    end;

  end;

  assert(Node.HasNode);
  FixNodeOuterAndPeers(Node);


  while Node.ScanEndLine < AEndLine do begin
    MaybeExtendNodeForward(Node, TmpNode, AEndLine + 1);
    if not TmpNode.HasNode then
      break;
    assert(Node.ScanEndLine < AEndLine, 'Scan gap still before AEndLine');
    Node := TmpNode;
    TmpNode.ClearInfo;

    FixNodeOuterAndPeers(Node);
  end;
  assert(Node.ScanEndLine >= AEndLine, 'Scan gap has reached AEndLine');

end;

procedure TSynMarkupHighIfDefLinesTree.DebugPrint(Flat: Boolean);
  function PeerLine(AEntry: TSynMarkupHighIfDefEntry): string;
  begin
    if AEntry = nil then exit('');
    Result := IntToStr(AEntry.FLine.GetPosition);
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
      ' Outer=', DbgsLine(ANode.OuterNestingNode),
      ' HighN=', DbgsLine(ANode.HighestValidNestedNode)
       ]);
    for i := 0 to ANode.EntryCount - 1 do
      DebugLn([PreFix, '       # ', dbgs(PtrUInt(ANode.Entry[i])),
        ' x1=', ANode.Entry[i].StartColumn, ' x2=', ANode.Entry[i].EndColumn,
        ' flg=',dbgs(ANode.Entry[i].NodeFlags),
        '  p1=', PeerLine(ANode.Entry[i].FPeer1), ':', dbgs(PtrUInt(ANode.Entry[i].FPeer1)),
        '  p2=', PeerLine(ANode.Entry[i].FPeer2), ':', dbgs(PtrUInt(ANode.Entry[i].FPeer2))
      ]);
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

procedure TSynMarkupHighIfDefLinesTree.Clear;
var
  n: TSynMarkupHighIfDefLinesNode;
begin
  FClearing := True;
  inherited Clear;
  while FDisposedNodes <> nil do begin
    n := FDisposedNodes;
    FDisposedNodes := TSynMarkupHighIfDefLinesNode(n).FOuterNestingNode;
    n.Free;
  end;
  FClearing := False;
end;

function TSynMarkupHighIfDefLinesTree.FindNodeAtPosition(ALine: Integer;
  AMode: TSynSizedDiffAVLFindMode): TSynMarkupHighIfDefLinesNodeInfo;
begin
  Result.ClearInfo;
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

procedure TSynEditMarkupIfDef.SetFoldView(AValue: TSynEditFoldedView);
begin
  if FFoldView = AValue then Exit;
  FFoldView := AValue;
end;

procedure TSynEditMarkupIfDef.SetHighlighter(AValue: TSynPasSyn);
begin
  if FHighlighter = AValue then Exit;
  FHighlighter := AValue;
  FIfDefTree.Highlighter := AValue;
end;

procedure TSynEditMarkupIfDef.ValidateMatches(SkipPaint: Boolean);
var
  i, LastLine: Integer;
  n: TSynMarkupHighIfDefLinesNodeInfo;
begin
  if (FPaintLock > 0) or (not SynEdit.IsVisible) then begin
    FNeedValidate := True;
    if not SkipPaint then
      FNeedValidatePaint := True;
    exit;
  end;
  FNeedValidate := False;

  if Highlighter = nil then begin
    FIfDefTree.Clear;
    exit;
  end;

  LastLine := ScreenRowToRow(LinesInWindow+1);
  FIfDefTree.ValidateRange(TopLine, TopLine + LastLine);

  InvalidateSynLines(TopLine, TopLine + LastLine);

end;

procedure TSynEditMarkupIfDef.DoFoldChanged(aLine: Integer);
begin
  //InvalidateLines(aLine+1, MaxInt, True);
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoTopLineChanged(OldTopLine: Integer);
begin
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoLinesInWindoChanged(OldLinesInWindow: Integer);
begin
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoTextChanged(StartLine, EndLine, ACountDiff: Integer);
begin
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.DoVisibleChanged(AVisible: Boolean);
begin
  ValidateMatches;
end;

procedure TSynEditMarkupIfDef.SetLines(const AValue: TSynEditStrings);
begin
  inherited SetLines(AValue);
  FIfDefTree.Lines := AValue;
end;

constructor TSynEditMarkupIfDef.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  FIfDefTree := TSynMarkupHighIfDefLinesTree.Create;

  MarkupInfo.Clear;
  MarkupInfo.Background := clLight;
end;

destructor TSynEditMarkupIfDef.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FIfDefTree);
end;

procedure TSynEditMarkupIfDef.IncPaintLock;
begin
  if FPaintLock = 0 then begin
    FNeedValidatePaint := False;
    FNeedValidate := False;
  end;
  inherited IncPaintLock;
end;

procedure TSynEditMarkupIfDef.DecPaintLock;
begin
  inherited DecPaintLock;
  if (FPaintLock = 0) and FNeedValidate then
    ValidateMatches(not FNeedValidatePaint);
end;

end.


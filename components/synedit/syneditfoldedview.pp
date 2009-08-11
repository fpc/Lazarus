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
(* some parts (AdjustBalance...) of this unit are based on the AVLTree unit *)
(* TODO: Implement node.eof / node.bof *)
unit SynEditFoldedView;

{$mode objfpc}{$H+}
{$coperators on}
{$IFDEF CPUPOWERPC} {$INLINE OFF} {$ENDIF} (* Workaround for bug 12576 (fpc) see bugs.freepascal.org/view.php?id=12576 *)

interface

uses
  LCLProc, Graphics,
  Classes, SysUtils, SynEditTextBase, SynEditTypes,
  SynEditMiscClasses, SynEditMiscProcs, SynEditPointClasses,
  SynEditHighlighter, SynEditHighlighterFoldBase;

type

  TReplacedChildSite = (rplcLeft, rplcRight);

  { TSynTextFoldAVLNodeData }

  TSynTextFoldAVLNodeData = Class
  public
    Parent, Left, Right : TSynTextFoldAVLNodeData;    (* AVL Links *)
    Balance : shortint;                               (* AVL Balance *)
    Nested : TSynTextFoldAVLNodeData; (* Nested folds (folds within this fold) do not need to be part of the searchable tree
                             They will be restored, if the outer fold (this fold) is unfolded
                             Nested points to a standalone tree, the root node in the nested tree, does *not* point back to this node *)
    LineOffset : Integer; (* Line-Number Offset to parent node
                             All line numbers are stored as offsets, for faster updates if lines are inserted/deleted *)
    LeftCount : Integer;  (* Lines folded in left tree. Used to calculate how many lines are folded up to a specified line *)
    FullCount : Integer;  (* Amount of lines in source for this fold *)
    LineCount : Integer;  (* Amount of lines folded away by this fold,
                             FullCount + Lines covered by overlaps *)
    FoldIndex: Integer;   (* index of fold in line; if a line has more than one fold starting *)

    function TreeDepth: integer;           (* longest WAY down. Only one node => 1! *)
    function RecursiveFoldCount : Integer; (* Amount of lines covered by this and all child nodes *)

    procedure SetLeftChild(ANode : TSynTextFoldAVLNodeData); overload; inline;
    procedure SetLeftChild(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer); overload; inline;
    procedure SetLeftChild(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset, aLeftCount : Integer); overload; inline;

    procedure SetRightChild(ANode : TSynTextFoldAVLNodeData); overload; inline;
    procedure SetRightChild(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer); overload; inline;

    function ReplaceChild(OldNode, ANode : TSynTextFoldAVLNodeData) : TReplacedChildSite; overload; inline;
    function ReplaceChild(OldNode, ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer) : TReplacedChildSite; overload; inline;

    procedure AdjustLeftCount(AValue : Integer);
    procedure AdjustParentLeftCount(AValue : Integer);

    function Precessor : TSynTextFoldAVLNodeData;
    function Successor : TSynTextFoldAVLNodeData;
    function Precessor(var aStartLine, aLinesBefore : Integer) : TSynTextFoldAVLNodeData;
    function Successor(var aStartLine, aLinesBefore : Integer) : TSynTextFoldAVLNodeData;
  end;

  { TSynTextFoldAVLNode }

  TSynTextFoldAVLNode = object
  private
    function GetFoldIndex: Integer;
    function GetLineCount : Integer;
    function GetFullCount : Integer;
  protected
    fData : TSynTextFoldAVLNodeData; // nil if unfolded
    fStartLine : Integer;            // start of folded
    fFoldedBefore : Integer;
  public
    procedure Init(aData : TSynTextFoldAVLNodeData; aStartLine, aFoldedBefore: Integer);
    function IsInFold : Boolean;
    function Next : TSynTextFoldAVLNode;
    function Prev : TSynTextFoldAVLNode;
    property LineCount: Integer read GetLineCount; // Zero, if Not in a fold
    property FullCount: Integer read GetFullCount; // Zero, if Not in a fold
    property StartLine: Integer read fStartLine;   // 1st Line of Current Fold
    property FoldedBefore: Integer read fFoldedBefore;  // Count of Lines folded before Startline
    property FoldIndex: Integer read GetFoldIndex;
  end;

  { TSynTextFoldAVLNodeNestedIterator:
    Iterates included nested nodes
    FoldedBefore is not valid in nested nodes
  }

  TSynTextFoldAVLNodeNestedIterator = class
  private
    FCurrentNode: TSynTextFoldAVLNode;
    FOuterNodes: Array of TSynTextFoldAVLNode;
  public
    constructor Create(ANode: TSynTextFoldAVLNode);
    destructor Destroy; override;
    function Next: TSynTextFoldAVLNode;
    function Prev: TSynTextFoldAVLNode;
    function EOF: Boolean;
    function BOF: Boolean;
    function IsInFold: Boolean;
    property Node: TSynTextFoldAVLNode read FCurrentNode;
  end;

  { TSynFoldNodeInfoHelper }

  TSynFoldNodeInfoHelper = class
    FCurInfo: TSynFoldNodeInfo;
    FActions: TSynFoldActions;
    FHighlighter: TSynCustomFoldHighlighter;
  protected
    procedure Invalidate;
  public
    constructor Create(AHighlighter: TSynCustomFoldHighlighter);

    function FirstOpen: TSynFoldNodeInfo;
    function Next: TSynFoldNodeInfo;
    function Prev: TSynFoldNodeInfo;
    function FindClose(KnownLength: Integer = -1): TSynFoldNodeInfo;
    function GotoOpenPos(aLineIdx, aNodeIdx: integer): TSynFoldNodeInfo;
    function GotoOpenAtChar(aLineIdx, aXPos: integer): TSynFoldNodeInfo;
    function GotoNodeOpenPos(ANode : TSynTextFoldAVLNode): TSynFoldNodeInfo;
    function GotoNodeClosePos(ANode : TSynTextFoldAVLNode): TSynFoldNodeInfo;
    function IsAtNodeOpenPos(ANode : TSynTextFoldAVLNode): Boolean;
    function IsValid: Boolean;
    function Equals(AnInfo: TSynFoldNodeInfo): Boolean;
    function Equals(AHelper: TSynFoldNodeInfoHelper): Boolean;

    property Info: TSynFoldNodeInfo read FCurInfo write FCurInfo;
    property Actions: TSynFoldActions read FActions write FActions;
  end;

  { TSynTextFoldAVLTree
    Nodes in the tree cover the folded lines only
    the cfCollapsed line at the start of a fold, is *not* part of a node.
    RemoveFoldForline has special precaution for this.
  }

  TSynTextFoldAVLTree = class
  protected
    fRoot: TSynTextFoldAVLNodeData;
    fNestParent: TSynTextFoldAVLNodeData;
    fNestedNodesTree: TSynTextFoldAVLTree; // FlyWeight Tree used for any nested subtree.
    fRootOffset : Integer;

    function NewNode : TSynTextFoldAVLNodeData; inline;
    procedure DisposeNode(var ANode : TSynTextFoldAVLNodeData); inline;
    Function RemoveFoldForNodeAtLine(ANode: TSynTextFoldAVLNode; ALine : Integer;
      IgnoreFirst : Boolean = False) : Integer; overload; // Line is for Nested Nodes

    // SetRoot, does not obbey fRootOffset => use SetRoot(node, -fRootOffset)
    procedure SetRoot(ANode : TSynTextFoldAVLNodeData); overload; inline;
    procedure SetRoot(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer); overload; inline;

    Function  InsertNode(ANode : TSynTextFoldAVLNodeData) : Integer; // returns FoldedBefore // ANode may not have children
    procedure RemoveNode(ANode: TSynTextFoldAVLNodeData);
    procedure BalanceAfterInsert(ANode: TSynTextFoldAVLNodeData);
    procedure BalanceAfterDelete(ANode: TSynTextFoldAVLNodeData);
    function TreeForNestedNode(ANode: TSynTextFoldAVLNodeData; aOffset : Integer) : TSynTextFoldAVLTree;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    (* Find Fold by Line in Real Text *)
    Function FindFoldForLine(ALine : Integer; FindNextNode : Boolean = False) : TSynTextFoldAVLNode;
    (* Find Fold by Line in Folded Text // always returns unfolded, unless next=true *)
    Function FindFoldForFoldedLine(ALine : Integer; FindNextNode : Boolean = False) : TSynTextFoldAVLNode;
    Function InsertNewFold(ALine, AColumn, ACount : Integer) : TSynTextFoldAVLNode;
    (* This will unfold the block which either contains tALine, or has Aline as its cgColapsed line
       If IgnoreFirst, the cfCollapsed will *not* unfold => Hint: IgnoreFirst = Make folded visible
       Returns the pos(1-based) of the cfCollapsed Line that was expanded; or ALine, if nothing was done
    *)
    Function RemoveFoldForLine(ALine : Integer; IgnoreFirst : Boolean = False;
                               OnlyCol: Integer = -1) : Integer; overload;
    Procedure AdjustForLinesInserted(AStartLine, ALineCount : Integer);
    Procedure AdjustForLinesDeleted(AStartLine, ALineCount : Integer);
    Function FindLastFold : TSynTextFoldAVLNode;
    Function FindFirstFold : TSynTextFoldAVLNode;
    Function LastFoldedLine : integer; // The actual line; LastNode.StartLine + LastNode.LineCount - 1
    procedure debug;
  end;

  TSynEditCodeFoldType = (
    cfNone,       // line is not in a block
    cfCollapsed,  // line is start of collapsed block
    cfExpanded,   // line is start of expanded block
    cfContinue,   // line is middle part of block(s)
    cfEnd         // line is end of block(s)
    );
  TFoldChangedEvent = procedure(aLine: Integer) of object;
const
  SynEditCodeFoldTypeNames: array[TSynEditCodeFoldType] of string = (
    'cfNone',
    'cfCollapsed',
    'cfExpanded',
    'cfContinue',
    'cfEnd'
    );

type
  TFoldViewNodeInfo = record
    HNode: TSynFoldNodeInfo;
    Text, Keyword: String;
    LineNum, ColIndex: Integer;
    OpenCount: Integer;
    Folded: boolean;
  end;

  { TSynTextFoldedView
      *Line      = Line (0-based) on Screen (except TopLine which should be TopViewPos)
      *ViewPos   = Line (1-based) in the array of viewable/visible lines
      *TextIndex = Line (0-based) in the complete text(folded and unfolded)
  }

  { TSynEditFoldedView }

  TSynEditFoldedView = class // TODO: class(TSynEditStringsLinked)
  private
    fCaret: TSynEditCaret;
    FHighLighter: TSynCustomHighlighter;
    fLines : TSynEditStrings;
    fFoldTree : TSynTextFoldAVLTree;   // Folds are stored 1-based (the 1st line is 1)
    FMarkupInfoFoldedCode: TSynSelectedColor;
    fTopLine : Integer;
    fLinesInWindow : Integer;          // there may be an additional part visible line
    fTextIndexList : Array of integer;   (* Map each Screen line into a line in textbuffer *)
    fFoldTypeList : Array of TSynEditCodeFoldType;
    fOnFoldChanged : TFoldChangedEvent;
    fLockCount : Integer;
    fNeedFixFrom, fNeedFixMinEnd : Integer;
    fNeedCaretCheck : Boolean;
    FCurFoldDescription: String;

    function GetCount : integer;
    function GetHighLighter: TSynCustomHighlighter;
    function GetLines(index : Integer) : String;
    function GetDisplayNumber(index : Integer) : Integer;
    function GetTextIndex(index : Integer) : Integer;
    function GetFoldType(index : Integer) : TSynEditCodeFoldType;
    function IsFolded(index : integer) : Boolean;  // TextIndex
    procedure SetTopLine(const ALine : integer);
    function  GetTopTextIndex : integer;
    procedure SetTopTextIndex(const AIndex : integer);
    procedure SetLinesInWindow(const AValue : integer);
  protected
    Procedure CalculateMaps;
    function  LengthForFoldAtTextIndex(ALine, AFoldIndex : Integer) : Integer;
    function FixFolding(AStart : Integer; AMinEnd : Integer; aFoldTree : TSynTextFoldAVLTree) : Boolean;

    procedure DoCaretChanged(Sender : TObject);
    Procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    Procedure LinesInsertedAtTextIndex(AStartIndex, ALineCount : Integer;
                                       SkipFixFolding : Boolean = False);
    Procedure LinesInsertedAtViewPos(AStartPos, ALineCount : Integer;
                                     SkipFixFolding : Boolean = False);
    Procedure LinesDeletedAtTextIndex(AStartIndex, ALineCount : Integer;
                                      SkipFixFolding : Boolean = False);
    Procedure LinesDeletedAtViewPos(AStartPos, ALineCount : Integer;
                                    SkipFixFolding : Boolean = False);
  public
    constructor Create(aTextView : TSynEditStrings; ACaret: TSynEditCaret);
    destructor Destroy; override;
    
    // Converting between Folded and Unfolded Lines/Indexes
    function TextIndexToViewPos(aTextIndex : Integer) : Integer;    (* Convert TextIndex (0-based) to ViewPos (1-based) *)
    function TextIndexToScreenLine(aTextIndex : Integer) : Integer; (* Convert TextIndex (0-based) to Screen (0-based) *)
    function ViewPosToTextIndex(aViewPos : Integer) : Integer;      (* Convert ViewPos (1-based) to TextIndex (0-based) *)
    function ScreenLineToTextIndex(aLine : Integer) : Integer;      (* Convert Screen (0-based) to TextIndex (0-based) *)

    function TextIndexAddLines(aTextIndex, LineOffset : Integer) : Integer;     (* Add/Sub to/from TextIndex (0-based) skipping folded *)
    function TextPosAddLines(aTextpos, LineOffset : Integer) : Integer;     (* Add/Sub to/from TextPos (1-based) skipping folded *)

    // Attributes for Visible-Lines-On-screen
    property Lines[index : Integer] : String            (* Lines on screen / 0 = TopLine *)
      read GetLines; default;
    property DisplayNumber[index : Integer] : Integer   (* LineNumber for display in Gutter / result is 1-based *)
      read GetDisplayNumber;
    property FoldType[index : Integer] : TSynEditCodeFoldType (* FoldIcon / State *)
      read GetFoldType;
    property TextIndex[index : Integer] : Integer       (* Position in SynTextBuffer / result is 0-based *)
      read GetTextIndex; // maybe writable

    // Define Visible Area
    property TopLine : integer                          (* refers to visible (unfolded) lines / 1-based *)
      read fTopLine write SetTopLine;
    property TopTextIndex : integer                     (* refers to TextIndex (folded + unfolded lines) / 1-based *)
      read GetTopTextIndex write SetTopTextIndex;
    property LinesInWindow : integer                    (* Fully Visible lines in Window; There may be one half visible line *)
      read fLinesInWindow write SetLinesInWindow;

    property Count : integer read GetCount;             (* refers to visible (unfolded) lines *)

    property MarkupInfoFoldedCode: TSynSelectedColor read FMarkupInfoFoldedCode;
  public
    procedure Lock;
    procedure UnLock;
    procedure debug;
    // Action Fold/Unfold
    // ColumnIndex can be negative, to access the highest(-1) availabel, 2nd highest(-2) ...
    //   With Negative, count points downward
    // ColCount = 0 => all
    // Skip => Do not count nodes that are already in the desired state
    procedure FoldAtLine(AStartLine: Integer; ColIndex : Integer = -1;          (* Folds at ScreenLine / 0-based *)
                         ColCount : Integer = 1; Skip: Boolean = False);
    procedure FoldAtViewPos(AStartPos: Integer; ColIndex : Integer = -1;        (* Folds at nth visible/unfolded Line / 1-based *)
                            ColCount : Integer = 1; Skip: Boolean = False);
    procedure FoldAtTextIndex(AStartIndex: Integer; ColIndex : Integer = -1;    (* Folds at nth TextIndex (all lines in buffer) / 1-based *)
                              ColCount : Integer = 1; Skip: Boolean = False);
    procedure UnFoldAtLine(AStartLine: Integer; ColIndex : Integer = -1;        (* UnFolds at ScreenLine / 0-based *)
                         ColCount : Integer = 0; Skip: Boolean = False);
    procedure UnFoldAtViewPos(AStartPos: Integer; ColIndex : Integer = -1;      (* UnFolds at nth visible/unfolded Line / 1-based *)
                         ColCount : Integer = 0; Skip: Boolean = False);
    procedure UnFoldAtTextIndex(AStartIndex: Integer; ColIndex : Integer = -1;  (* UnFolds at nth TextIndex (all lines in buffer) / 1-based *)
                         ColCount : Integer = 0; Skip: Boolean = False);
    procedure UnFoldAtTextIndexCollapsed(AStartIndex: Integer);   (* UnFolds only if Index is in the fold, ignores cfcollapsed line, if unfolded / 1-based *)

    function IsFoldedAtTextIndex(AStartIndex, ColIndex: Integer): Boolean;      (* Checks xth Fold at nth TextIndex (all lines in buffer) / 1-based *)
    function LogicalPosToNodeIndex(AStartIndex: Integer; LogX: Integer;         (* Returns the index of the node, at the logical char pos *)
                                   Previous: Boolean = False): Integer;

    procedure CollapseDefaultFolds;
    // Load/Save folds to string
    // AStartIndex, AEndIndex: (0 based) First/last line (EndIndex = -1 = open end)
    // AStartCol, AEndCol: (1 based) Logical text pos in Line. (AEndCol = -1 = full line)
    function  GetFoldDescription(AStartIndex, AStartCol, AEndIndex,
                                 AEndCol: Integer; AsText: Boolean = False;
                                 Extended: Boolean = False) :String;
    procedure ApplyFoldDescription(AStartIndex, AStartCol, AEndIndex,
                                   AEndCol: Integer; FoldDesc: PChar;
                                   FoldDescLen: Integer; IsText: Boolean = False);

    procedure UnfoldAll;
    procedure FoldAll(StartLevel : Integer = 0; IgnoreNested : Boolean = False);
    procedure FixFoldingAtTextIndex(AStartIndex: Integer; AMinEndLine: Integer = 0); // Real/All lines
  public
    function OpenFoldCount(aStartIndex: Integer): Integer;
    function OpenFoldInfo(aStartIndex, ColIndex: Integer): TFoldViewNodeInfo;

  public
    // Find the visible first line of the fold at ALine. Returns -1 if Aline is not folded
    function CollapsedLineForFoldAtLine(ALine : Integer) : Integer;
    function ExpandedLineForBlockAtLine(ALine : Integer; HalfExpanded: Boolean = True) : Integer;

    function GetPhysicalCharWidths(Index: Integer): TPhysicalCharWidths;

    property FoldedAtTextIndex [index : integer] : Boolean read IsFolded;

    property OnFoldChanged: TFoldChangedEvent  (* reports 1-based line *) {TODO: synedit expects 0 based }
      read fOnFoldChanged write fOnFoldChanged;
    property HighLighter: TSynCustomHighlighter read GetHighLighter
                                                write FHighLighter;
  end;
  
implementation

type
  TFoldExportEntry = Record
    Line, LogX, LogX2, ELine, ELogX, ELogX2, FType: Integer;
  end;

  { TSynEditFoldExportStream }

  TSynEditFoldExportStream = class
  private
    FData: String;
    FLen, FPos: Integer;
    FMem: PChar;
    function  GetLen: Integer;
    procedure SetLen(const AValue: Integer);
    function  GetMem: PChar;
    procedure SetMem(const AValue: PChar);
    function  GetText: String;
    procedure SetText(const AValue: String);
  protected
    function GrowData(AppendSize: Integer): PChar;
    function EncodeIntEx(Anum: Integer): String;  // base 43, with leading continue bit
    function EncodeIntEx2(Anum: Integer): String; // for numbers expected below 467; specially 0..80
    function InternalReadNum(var APos: Integer): Integer;
    function InternalReadNumEx(var APos: Integer): Integer;
  public
    constructor Create;
    procedure Compress;
    procedure Decompress;

    procedure AddChecksum;
    function  VerifyChecksum: Boolean;

    // see notes for Compression
    Procedure AppendMem(AMem: Pointer; ALen: Integer);
    Procedure AppendString(ATxt: String);
    Procedure AppendNum(ANum: Integer);
    Procedure AppendNumEx(ANum: Integer);

    Procedure Reset;
    Procedure Clear;
    function ReadMem(AMem: Pointer; ALen: Integer): Boolean;
    function PeakString(ALen: Integer): String;
    function FindChar(AChar: Char): Integer; // 0 based
    function ReadString(ALen: Integer): String;
    function ReadNum: Integer;
    function ReadNumEx: Integer;
    function EOF: Boolean;

    property Text: String read GetText write SetText;
    property Mem: PChar read GetMem write SetMem;
    property Len: Integer read GetLen write SetLen;
    property Pos: Integer read FPos;
  end;

  { TSynEditFoldExportCoder }

  TSynEditFoldExportCoder = class
  private
    FExportStream: TSynEditFoldExportStream;
    FFoldType: Pointer;
    FCount, FSumCount, FSumFolded, FLastLine, FFirstLine: Integer;
    FLastY, FLastX: Integer;
    FFolded: Boolean;
    FReadIsValid: Boolean;
    FReadY, FReadLastY, FReadX, FReadSumLen, FReadCount: Integer;
    FReadAtPoint, FReadFolded, FReadBeforeFirst: Boolean;
  public
    constructor Create(AFoldType: Pointer);
    constructor Create(AStream: TSynEditFoldExportStream);
    destructor Destroy; override;

    procedure AddNode(aX, aY, aLen: Integer; aFolded:Boolean);
    procedure Finish;

    function ReadNode(aX, aY, aLen: Integer): Boolean;
    function EOF: Boolean;
    procedure Reset;
    property ReadIsValid: Boolean read FReadIsValid;

    property FoldType: Pointer read FFoldType;
    property Stream: TSynEditFoldExportStream read FExportStream;
  end;

const
  // use only xml encode-able ascii
  // do not use [ or ], they are reserved for compression
  // space can be used a special indicator
  NumEncode86Chars: string[86] = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-+;:,.@=*/\!?$%()''^{}~_#';

  NumEncodeAsOneMax = 80;                        // Maximum Value to encode as 1 char
  NumEncodeAsTwoMax = 81 + 4*86 + 43;            // =  467; Maximum Value to encode as 2 char
  NumEncodeAsThreeMax = 81 + 4*86 + 43 * 43 - 1; // = 2273  Maximum Value to encode as 3 char


  SEQMaxNodeCount     = 75;  // New Full entry at least every 75 folds
  SEQMaxLineDistEach  = 500;  //  New Full entry, if folds startlines are more than 500 appart
  SEQMaxLineDistTotal = 2500; // New Full entry at least every 2500; check position

var
  NumEncode86Values: Array [Char] of integer;

procedure InitNumEncodeValues;
var
  i: integer;
  c : Char;
begin
  for c := low(Char) to high(Char) do begin
    NumEncode86Values[c] := -1;
  end;
  for i := 1 to length(NumEncode86Chars) do
    NumEncode86Values[NumEncode86Chars[i]] := i - 1;
end;

{ TSynEditFoldExportStream }

constructor TSynEditFoldExportStream.Create;
begin
  inherited;
  FPos := 0;
  FLen := 0;
  FMem := nil;
end;


function TSynEditFoldExportStream.GetLen: Integer;
begin
  Result := FLen;
end;

procedure TSynEditFoldExportStream.SetLen(const AValue: Integer);
begin
  FPos := 0;
  FLen:= AValue;
end;

function TSynEditFoldExportStream.GetMem: PChar;
begin
  if FData <> '' then
    Result := @FData[1]
  else
    Result := FMem;
end;

procedure TSynEditFoldExportStream.SetMem(const AValue: PChar);
begin
  FData := '';
  FMem := AValue;
  FPos := 0;
end;

function TSynEditFoldExportStream.GetText: String;
begin
  // only valid for FData
  SetLength(FData, FLen);
  Result := FData;
end;

procedure TSynEditFoldExportStream.SetText(const AValue: String);
begin
  FData := AValue;
  FMem := nil;
  FPos := 0;
end;

function TSynEditFoldExportStream.GrowData(AppendSize: Integer): PChar;
var
  l: integer;
begin
  l := length(FData);
  if l < FLen + AppendSize then
    SetLength(FData, l + AppendSize + Max((l+AppendSize) div 4, 1024));
  Result := @FData[FLen + 1];
  inc(FLen, AppendSize);
end;

function TSynEditFoldExportStream.EncodeIntEx(Anum: Integer): String;
var
  n: integer;
begin
  Result := '';
  if ANum = 0 then Result := NumEncode86Chars[1];
  n := 0;
  while ANum > 0 do begin
    Result := NumEncode86Chars[1 + (Anum mod 43) + n] + Result;
    ANum := ANum div 43;
    n := 43;
  end;
end;

function TSynEditFoldExportStream.EncodeIntEx2(Anum: Integer): String;
var
  n: Integer;
begin
  Result := '';
  if Anum <= 80 then
    Result := NumEncode86Chars[1 + Anum]
  else
  begin
    n := (Anum-81) div 86;
    if n <= 3 then
      Result := NumEncode86Chars[1 + 81 + n] + NumEncode86Chars[1 + (Anum - 81) mod 86]
    else
      Result := NumEncode86Chars[1 + 85] + EncodeIntEx(Anum - 81 - 4*86);
  end;
end;

function TSynEditFoldExportStream.InternalReadNum(var APos: Integer): Integer;
var
  n: Integer;
begin
  Result := 0;
  while (APos < FLen) do begin
    n := NumEncode86Values[(FMem + APos)^];
    if n < 43 then break;
    dec(n, 43);
    Result := Result * 43 + n;
    inc(APos);
  end;
  Result := Result * 43 + n;
  inc(APos);
end;

function TSynEditFoldExportStream.InternalReadNumEx(var APos: Integer): Integer;
begin
  Result := NumEncode86Values[(FMem + APos)^];
  inc(APos);
  if Result <= 80 then
    exit;
  if Result < 85 then begin
    Result := 81 + (Result-81)*86 +  NumEncode86Values[(FMem + APos)^];
    inc(APos);
    exit;
  end;
  Result := 81 + 4*86 + InternalReadNum(APos);
end;

procedure TSynEditFoldExportStream.Compress;
(* Known Sequences: XX = Enc64Num (copy sequence from XX chars before)
                    NN = ENc22 Num / n = enc22digit (copy n bytes)
     [XXn     (up to 21 bytes, from up to 64*64 back)
     [NNXX[   (more then 21 bytes, from up to 64*64 back)
     ]X       (3 bytes from max 64 back)
     ]nx      ( reocurring space,x times, ever n pos)
*)
const
  max_single_len = 22 - 1;
var
  CurPos, EndPos, SearchPos: Integer;
  FndLen, FndPos, FndPos2: Integer;
  BestLen, BestPos, BestPos2: Integer;
  s: string;
begin
  AppendString(#0);
  dec(FLen);

  EndPos := FLen;
  CurPos := FLen - 3;
  while CurPos >= 4 do begin
    SearchPos := CurPos - 3;
    BestLen := 0;
    while (SearchPos >= 1) do begin
      if CompareMem(@FData[CurPos], @FData[SearchPos], 3) then begin
        FndLen := 3;
        FndPos := SearchPos;
        FndPos2 := CurPos;
        while (SearchPos + FndLen < FndPos2) and
              (FndPos2 + FndLen < EndPos - 1) and
              (FData[SearchPos + FndLen] = FData[CurPos + FndLen])
        do
          inc(FndLen);
        while (FndPos > 1) and (FndPos + FndLen < FndPos2) and
              (FData[FndPos - 1] = FData[FndPos2 - 1]) do
        begin
          dec(FndPos);
          dec(FndPos2);
          inc(FndLen);
        end;

        if (FndLen > BestLen) and
           ((FndPos2 - FndPos <= NumEncodeAsOneMax) or (FndLen >= 4)) and
           ((FndPos2 - FndPos <= NumEncodeAsTwoMax) or (FndLen >= 5)) and
           ((FndPos2 - FndPos <= NumEncodeAsThreeMax) or (FndLen >= 6))
        then begin
          BestLen := FndLen;
          BestPos := FndPos;
          BestPos2 := FndPos2;
        end;
      end;
      dec(SearchPos);
    end;

    s := '';
    if (BestLen >= 4) then
      s := '[' + EncodeIntEx2(BestPos2 - BestPos) + EncodeIntEx2(BestLen)
    else
    if (BestLen = 3) and (BestPos2 - BestPos <= NumEncodeAsOneMax) then
      s := ']' + EncodeIntEx2(BestPos2 - BestPos);
    if (s<>'') and (length(s) < BestLen) then begin
      System.Move(s[1], FData[BestPos2], length(s));
      System.Move(FData[BestPos2 + BestLen], FData[BestPos2 + length(s)], FLen + 1 - (BestPos2 + BestLen));
      dec(FLen, BestLen - length(s));
      EndPos := BestPos;
      CurPos := BestPos2 - 3;
    end
    else
      dec(CurPos);
  end;
end;

procedure TSynEditFoldExportStream.Decompress;
var
  i, j, n: Integer;
  p, p2: PChar;
  NewLen: Integer;
begin
  // curently assumes that FMem points NOT at FData
  if FLen = 0 then
    exit;
  NewLen := 0;
  i := 0;
  while i < Flen do begin
    case (FMem+i)^ of
      '[' :
        begin
          inc(i);
          j := InternalReadNumEx(i);
          n := InternalReadNumEx(i);
          if (j < n) or (j > NewLen) then raise ESynEditError.Create('fold format error');
          inc(NewLen, n);
        end;
      ']' :
        begin
          inc(i, 1);
          j := InternalReadNumEx(i);
          if (j < 3) or (j > NewLen) then raise ESynEditError.Create('fold format error');
          inc(NewLen, 3);
        end;
      else
        begin
          inc(NewLen);
          inc(i);
        end;
    end;
  end;
  SetLength(FData, NewLen);

  i := 0;
  p := @FData[1];
  while i < Flen do begin
    case (FMem+i)^ of
      '[' :
        begin
          inc(i);
          j := InternalReadNumEx(i);
          n := InternalReadNumEx(i);
          p2 := p;
          while n > 0 do begin
            p^ := (p2 - j)^;
            inc(p);
            dec(j);
            dec(n);
          end;
        end;
      ']' :
        begin
          inc(i);
          j := InternalReadNumEx(i);
          p2 := p;
          for n := 0 to 2 do begin
            p^ := (p2 - j)^;
            inc(p);
            dec(j);
          end;
        end;
      else
        begin
          p^ := (FMem + i)^;
          inc(p);
          inc(i);
        end;
    end;
  end;

  FLen := NewLen;
  FMem := @FData[1];
  FPos := 0;
end;

procedure TSynEditFoldExportStream.AddChecksum;
var
  i, c: Integer;
begin
  if FLen = 0 then
    exit;
  if FMem = nil then
    FMem := @FData[1];
  c := 0;
  for i := 0 to FLen - 1 do
    c := c xor (ord((FMem + i)^) * (i+1));
  c := (c mod 256) xor ((c div 256) mod 256) xor ((c div 65536) mod 256);
  AppendString(NumEncode86Chars[1 + (c mod 86)]);
end;

function TSynEditFoldExportStream.VerifyChecksum: Boolean;
var
  i, c: Integer;
begin
  if FLen = 0 then
    exit(True);
  if FMem = nil then
    FMem := @FData[1];
  dec(Flen);
  c := 0;
  for i := 0 to FLen - 1 do
    c := c xor (ord((FMem + i)^) * (i+1));
  c := (c mod 256) xor ((c div 256) mod 256) xor ((c div 65536) mod 256);
  Result := (FMem + FLen)^ = NumEncode86Chars[1 + (c mod 86)];
end;

procedure TSynEditFoldExportStream.AppendMem(AMem: Pointer; ALen: Integer);
begin
  FMem := nil;
  if ALen > 0 then
    System.Move(AMem^, GrowData(ALen)^, ALen);
end;

procedure TSynEditFoldExportStream.AppendString(ATxt: String);
var
  l: Integer;
begin
  FMem := nil;
  l := length(ATxt);
  if l > 0 then
    System.Move(ATxt[1], GrowData(l)^, l);
end;

procedure TSynEditFoldExportStream.AppendNum(ANum: Integer);
begin
  FMem := nil;
  AppendString(EncodeIntEx(ANum));
end;

procedure TSynEditFoldExportStream.AppendNumEx(ANum: Integer);
begin
  FMem := nil;
  AppendString(EncodeIntEx2(ANum));
end;

procedure TSynEditFoldExportStream.Reset;
begin
  FPos := 0;
  if (FMem = nil) and (FData <> '') then
    FMem := @FData[1];
end;

procedure TSynEditFoldExportStream.Clear;
begin
  FLen := 0;
  FMem := nil;
  FPos := 0;
  SetLength(FData, 0);
end;

function TSynEditFoldExportStream.ReadMem(AMem: Pointer; ALen: Integer): Boolean;
begin
  Result := FPos+ ALen <= FLen;
  If not Result then
    exit;
  System.Move((FMem + FPos)^, AMem^, ALen);
  inc(FPos, ALen);
end;

function TSynEditFoldExportStream.PeakString(ALen: Integer): String;
begin
  If not(FPos+ ALen <= FLen) then
    exit('');
  SetLength(Result, ALen);
  if ALen > 0 then
    System.Move((FMem + FPos)^, Result[1], ALen);
end;

function TSynEditFoldExportStream.FindChar(AChar: Char): Integer;
begin
  Result := 0;
  While (FPos + Result < FLen) and ((FMem + FPos + Result)^ <> AChar) do
    inc(Result);
  if FPos + Result = FLen then
    Result := -1;
end;

function TSynEditFoldExportStream.ReadString(ALen: Integer): String;
begin
  If not(FPos+ ALen <= FLen) then
    exit('');
  SetLength(Result, ALen);
  if ALen > 0 then
    System.Move((FMem + FPos)^, Result[1], ALen);
  inc(FPos, ALen);
end;

function TSynEditFoldExportStream.ReadNum: Integer;
begin
  Result := InternalReadNum(FPos);
end;

function TSynEditFoldExportStream.ReadNumEx: Integer;
begin
  Result := InternalReadNumEx(FPos);
end;

function TSynEditFoldExportStream.EOF: Boolean;
begin
  Result := FPos >= FLen;
end;

{ TSynEditFoldExportCoder }

constructor TSynEditFoldExportCoder.Create(AFoldType: Pointer);
begin
  inherited Create;
  FExportStream := TSynEditFoldExportStream.Create;
  FExportStream.AppendString(' T');                // Type Marker
  FExportStream.AppendNum(PtrUInt(AFoldType));
  FFolded := False;
  FCount := -1;  // Flag to avoid the first ' p' marker
  FSumCount := 0;
  FLastLine := 0;
  FFirstLine := 0;
  FFoldType := AFoldType;
end;

constructor TSynEditFoldExportCoder.Create(AStream: TSynEditFoldExportStream);
var i: Integer;
begin
  inherited Create;
  FExportStream := TSynEditFoldExportStream.Create;
  FReadIsValid := False;
  if AStream.PeakString(2) <> ' T' then exit;

  FReadIsValid := True;
  AStream.ReadString(2);

  FFoldType := Pointer(PtrUInt(AStream.ReadNum));
  while(true) do begin
    i := AStream.FindChar(' ');
    if i < 0 then i := AStream.Len - AStream.Pos;
    FExportStream.AppendString(AStream.ReadString(i));
    if AStream.EOF or
       ((AStream.PeakString(2) <> ' p') and (AStream.PeakString(2) <> ' P'))
    then
      break;
    FExportStream.AppendString(AStream.ReadString(2));
  end;
  Reset;
end;

destructor TSynEditFoldExportCoder.Destroy;
begin
  FreeAndNil(FExportStream);
  Inherited;
end;

procedure TSynEditFoldExportCoder.AddNode(aX, aY, aLen: Integer; aFolded: Boolean);
(* Format:  [Num] <NumEX>
  ' T' [type] [yo] <X> <len> ( <c>* ' p' [sum]  [yo] <X> <len> )* <c>* (' P' [sum] [yo] <X> <len>)?
*)
begin
  if (not aFolded) and (FCount < 0) then
    exit;
  if aFolded then begin
    FLastY := aY;;
    FLastX := aX;;
  end;

  if aFolded and ( (FCount < 0) or
                   (FCount          > SEQMaxNodeCount) or
                   (FSumCount       > SEQMaxNodeCount) or
                   (aY - FLastLine  > SEQMaxLineDistEach) or
                   (aY - FFirstLine > SEQMaxLineDistTotal) )
  then begin // Need Full x,Y checkpoint
    if (FCount > 0) and FFolded then
      FExportStream.AppendNumEx(FCount);     // Finish Sequence / since only folded nodes are fnished, there may be unfolded, before the next checkpoit
    if FCount >= 0 then begin
      FExportStream.AppendString(' p');      // point marker (no marker needed for first entry)
      FExportStream.AppendNum(FSumFolded);   // Start with sum from last sequence
    end;
    FExportStream.AppendNum(aY - FFirstLine);
    FExportStream.AppendNumEx(aX);
    FExportStream.AppendNumEx(aLen);
    FFolded := True;
    FFirstLine := aY;
    FLastLine := aY;
    FCount := 0;
    FSumCount := 0;
    FSumFolded := 0;
    exit;
  end;

  if (aFolded <> FFolded) then begin
    FExportStream.AppendNumEx(FCount);
    FFolded := aFolded;
    FCount := 1;
  end
  else
    inc(FCount);

  inc(FSumCount);
  inc(FSumFolded, aLen);
  FLastLine := aY;
end;

procedure TSynEditFoldExportCoder.Finish;
begin
  if FCount < 0 then
    FExportStream.Clear;                   // Never added a node
  if (FCount > 0) and FFolded then
    FExportStream.AppendNumEx(FCount);     // Finish Sequence
  if FSumCount > 10 then begin
    FExportStream.AppendString(' P');      // finish marker (optional)
    FExportStream.AppendNum(FSumFolded);   // Start with sum from last sequence
    FExportStream.AppendNum(FLastY - FFirstLine);  // Last folded Coords
    FExportStream.AppendNumEx(FLastX);
  end;
end;

function TSynEditFoldExportCoder.ReadNode(aX, aY, aLen: Integer): Boolean;
(* Format:  [Num] <NumEX>
  ' T' [type]
   [yo] <X> <len> ( <c>* ' p' [sum]  [yo] <X> <len> )* <c>* (' P' [sum] [yo] <X>)?
*)
begin
  Result := False;
  if not FReadIsValid then
    exit;

  if FReadBeforeFirst then begin
    if ((aY < FReadY) or ((aY = FReadY) and (aX < FReadX))) then
      exit;
    if not((aY = FReadY) and (aX = FReadX)) then
      FReadIsValid := False;
    if FExportStream.EOF then
      FReadIsValid := False;
    if aLen <> FExportStream.ReadNumEx then
      FReadIsValid := False;
    FReadLastY := FReadY;
    FReadCount := FExportStream.ReadNumEx;
    FReadFolded := True;
    FReadSumLen := 0;
    FReadBeforeFirst := False;
    Result := FReadIsValid;
    exit;
  end;

  if not FReadAtPoint then begin
    if FReadCount <= 0 then begin
      if FExportStream.PeakString(2) = ' P' then begin // Finish
        FExportStream.ReadString(2);
        if (not (FReadSumLen = FExportStream.ReadNum)) or
           (not (FReadY = FExportStream.ReadNum + FReadLastY)) or
           (not (FReadX = FExportStream.ReadNumEx))
        then
          FReadIsValid := False;
        exit(False);
      end
      else if FExportStream.PeakString(2) = ' p' then begin // point
        FExportStream.ReadString(2);
        if not(FReadSumLen = FExportStream.ReadNum) then
          FReadIsValid := False;
        FReadAtPoint := True;
      end
      else begin
        FReadFolded := not FReadFolded;
        FReadCount := FExportStream.ReadNumEx;
        //Result := FReadFolded;
      end;
    end;
    if not FReadAtPoint then begin
      Result := FReadFolded and FReadIsValid;
      dec(FReadCount);
      if Result then
        inc(FReadSumLen, aLen);
    end;
  end;

  if FReadAtPoint then begin
    FReadAtPoint := False;
    FReadY := FExportStream.ReadNum + FReadLastY;
    FReadX := FExportStream.ReadNumEx;
    if FExportStream.EOF then
      FReadIsValid := False;

    if ((aY < FReadY) or ((aY = FReadY) and (aX < FReadX))) then begin
      Result := False;
      FReadBeforeFirst := True; // actually, read before point / if FReadCount <
    end
    else
    begin
      if not((aY = FReadY) and (aX = FReadX)) then
        FReadIsValid := False;
      if aLen <> FExportStream.ReadNumEx then
        FReadIsValid := False;
      FReadLastY := FReadY;
      if FExportStream.PeakString(1) = ' ' then
        FReadCount := 0
      else
        FReadCount := FExportStream.ReadNumEx;
      FReadFolded := True;
      FReadSumLen := 0;
      Result := FReadIsValid;
    end;
  end;
  if Result then begin
    FReadY := aY;
    FReadX := aX;
  end;
end;

function TSynEditFoldExportCoder.EOF: Boolean;
begin
  Result := FExportStream.EOF;
end;

procedure TSynEditFoldExportCoder.Reset;
begin
  FExportStream.Reset;
  FReadIsValid := FExportStream.Len > 0;
  FReadY := -1;
  FReadX := -1;
  FReadLastY := 0;
  FReadCount := -1;
  FReadSumLen := 0;
  FReadAtPoint := True;
  FReadBeforeFirst := False;
end;

{ TSynTextFoldAVLNodeData }

function TSynTextFoldAVLNodeData.TreeDepth : integer;
var t: integer;
begin
  Result := 1;
  if Left<>nil  then Result := Left.TreeDepth+1;
  if Right<>nil then t := Right.TreeDepth+1 else t := 0;
  if t> Result then Result := t;
end;

function TSynTextFoldAVLNodeData.RecursiveFoldCount : Integer;
var ANode : TSynTextFoldAVLNodeData;
begin
  Result := 0;
  ANode := self;
  while ANode <> nil do begin
    Result := Result + ANode.LineCount + ANode.LeftCount;
    ANode := ANode.Right;
  end;
end;

procedure TSynTextFoldAVLNodeData.SetLeftChild(ANode : TSynTextFoldAVLNodeData); inline;
begin
  Left := ANode;
  if ANode <> nil then ANode.Parent := self;
end;

procedure TSynTextFoldAVLNodeData.SetLeftChild(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer); inline;
begin
  Left := ANode;
  if ANode <> nil then begin
    ANode.Parent := self;
    ANode.LineOffset := ANode.LineOffset + anAdjustChildLineOffset;
  end;
end;

procedure TSynTextFoldAVLNodeData.SetLeftChild(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset, aLeftCount : Integer); inline;
begin
  Left := ANode;
  LeftCount := aLeftCount;
  if ANode <> nil then begin
    ANode.Parent := self;
    ANode.LineOffset := ANode.LineOffset + anAdjustChildLineOffset;
  end
end;

procedure TSynTextFoldAVLNodeData.SetRightChild(ANode : TSynTextFoldAVLNodeData); inline;
begin
  Right := ANode;
  if ANode <> nil then ANode.Parent := self;
end;

procedure TSynTextFoldAVLNodeData.SetRightChild(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer); inline;
begin
  Right := ANode;
  if ANode <> nil then begin
    ANode.Parent := self;
    ANode.LineOffset := ANode.LineOffset + anAdjustChildLineOffset;
  end;
end;

function TSynTextFoldAVLNodeData.ReplaceChild(OldNode, ANode : TSynTextFoldAVLNodeData) : TReplacedChildSite; inline;
begin
  if Left = OldNode then begin
    SetLeftChild(ANode);
    exit(rplcLeft);
  end;
  SetRightChild(ANode);
  result := rplcRight;
end;

function TSynTextFoldAVLNodeData.ReplaceChild(OldNode, ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer) : TReplacedChildSite;
  inline;
begin
  if Left = OldNode then begin
    SetLeftChild(ANode, anAdjustChildLineOffset);
    exit(rplcLeft);
  end;
  SetRightChild(ANode, anAdjustChildLineOffset);
  result := rplcRight;
end;

procedure TSynTextFoldAVLNodeData.AdjustLeftCount(AValue : Integer);
begin
  LeftCount := LeftCount + AValue;
  AdjustParentLeftCount(AValue);
end;

procedure TSynTextFoldAVLNodeData.AdjustParentLeftCount(AValue : Integer);
var
  node, pnode : TSynTextFoldAVLNodeData;
begin
  node := self;
  pnode := node.Parent;
  while pnode <> nil do begin
    if node = pnode.Left
    then pnode.LeftCount := pnode.LeftCount + AValue;
    node := pnode;
    pnode := node.Parent;
  end;
end;

function TSynTextFoldAVLNodeData.Precessor : TSynTextFoldAVLNodeData;
begin
  Result := Left;
  if Result<>nil then begin
    while (Result.Right<>nil) do Result := Result.Right;
  end else begin
    Result := self;
    while (Result.Parent<>nil) and (Result.Parent.Left=Result) do
      Result := Result.Parent;
    Result := Result.Parent;
  end;
end;

function TSynTextFoldAVLNodeData.Successor : TSynTextFoldAVLNodeData;
begin
  Result := Right;
  if Result<>nil then begin
    while (Result.Left<>nil) do Result := Result.Left;
  end else begin
    Result := self;
    while (Result.Parent<>nil) and (Result.Parent.Right=Result) do
      Result := Result.Parent;
    Result := Result.Parent;
  end;
end;

function TSynTextFoldAVLNodeData.Precessor(var aStartLine, aLinesBefore : Integer) : TSynTextFoldAVLNodeData;
begin
  Result := Left;
  if Result<>nil then begin
    aStartLine := aStartLine + Result.LineOffset;
    while (Result.Right<>nil) do begin
      Result := Result.Right;
      aStartLine := aStartLine + Result.LineOffset;
    end;
  end else begin
    Result := self;
    while (Result.Parent<>nil) and (Result.Parent.Left=Result) do begin
      aStartLine := aStartLine - Result.LineOffset;
      Result := Result.Parent;
    end;
    // result is now a right son
    aStartLine := aStartLine - Result.LineOffset;
    Result := Result.Parent;
  end;
  if result <> nil then
    aLinesBefore := aLinesBefore - result.LineCount
  else
    aLinesBefore := 0;
end;

function TSynTextFoldAVLNodeData.Successor(var aStartLine, aLinesBefore : Integer) : TSynTextFoldAVLNodeData;
begin
  aLinesBefore := aLinesBefore + LineCount;
  Result := Right;
  if Result<>nil then begin
    aStartLine := aStartLine + Result.LineOffset;
    while (Result.Left<>nil) do begin
      Result := Result.Left;
      aStartLine := aStartLine + Result.LineOffset;
    end;
  end else begin
    Result := self;
    while (Result.Parent<>nil) and (Result.Parent.Right=Result) do begin
      aStartLine := aStartLine - Result.LineOffset;
      Result := Result.Parent;
    end;
    // Result is now a left son; result has a negative LineOffset
    aStartLine := aStartLine - Result.LineOffset;
    Result := Result.Parent;
  end;
end;

{ TSynTextFoldAVLNode }

function TSynTextFoldAVLNode.GetFoldIndex: Integer;
begin
  if fData = nil
  then Result := -1
  else Result := fData.FoldIndex;
end;

function TSynTextFoldAVLNode.GetLineCount : Integer;
begin
  if fData = nil
  then Result := 0
  else Result := fData.LineCount;
end;

function TSynTextFoldAVLNode.GetFullCount: Integer;
begin
  if fData = nil
  then Result := -1
  else Result := fData.FullCount;
end;

procedure TSynTextFoldAVLNode.Init(aData: TSynTextFoldAVLNodeData; aStartLine,
  aFoldedBefore: Integer);
begin
  fData := aData;
  fStartLine :=  aStartLine;
  fFoldedBefore := aFoldedBefore;
end;

function TSynTextFoldAVLNode.IsInFold : Boolean;
begin
  Result := fData <> nil;
end;

function TSynTextFoldAVLNode.Next : TSynTextFoldAVLNode;
var aStart, aBefore : Integer;
begin
  if fData <> nil then begin
    aStart := StartLine;
    aBefore := FoldedBefore;
    Result.fData := fData.Successor(aStart, aBefore);
    Result.fStartLine := aStart;
    Result.fFoldedBefore := aBefore;
  end
  else Result.fData := nil;
end;

function TSynTextFoldAVLNode.Prev : TSynTextFoldAVLNode;
var aStart, aBefore : Integer;
begin
  if fData <> nil then begin
    aStart := StartLine;
    aBefore := FoldedBefore;
    Result.fData := fData.Precessor(aStart, aBefore);
    Result.fStartLine := aStart;
    Result.fFoldedBefore := aBefore;
  end
  else Result.fData := nil;
end;

{ TSynTextFoldAVLNodeNestedIterator }

constructor TSynTextFoldAVLNodeNestedIterator.Create(ANode: TSynTextFoldAVLNode);
begin
  SetLength(FOuterNodes, 0);
  FCurrentNode := ANode;
end;

destructor TSynTextFoldAVLNodeNestedIterator.Destroy;
begin
  SetLength(FOuterNodes, 0);
  inherited Destroy;
end;

function TSynTextFoldAVLNodeNestedIterator.Next: TSynTextFoldAVLNode;
var
  NewData: TSynTextFoldAVLNodeData;
  i: Integer;
  PNode: TSynTextFoldAVLNode;
begin
  i := length(FOuterNodes);
  if FCurrentNode.fData.Nested = nil then begin
    FCurrentNode := FCurrentNode.Next;
    while (not FCurrentNode.IsInFold) and (i > 0) do begin
      dec(i);
      FCurrentNode := FOuterNodes[i];
      SetLength(FOuterNodes, i);
      FCurrentNode := FCurrentNode.Next;
    end;
  end else begin
    SetLength(FOuterNodes, i + 1);
    FOuterNodes[i] := FCurrentNode;
    NewData := FCurrentNode.fData.Nested;
    FCurrentNode.fData := NewData;
    FCurrentNode.FStartLine := FCurrentNode.FStartLine + NewData.LineOffset;

    PNode := FCurrentNode.Prev;
    while PNode.IsInFold do begin
      FCurrentNode := PNode;
      PNode := FCurrentNode.Prev;
    end;
  end;
  Result := FCurrentNode;
end;

function TSynTextFoldAVLNodeNestedIterator.Prev: TSynTextFoldAVLNode;
var
  i: Integer;
  NewData: TSynTextFoldAVLNodeData;
  PNode: TSynTextFoldAVLNode;
begin
  FCurrentNode := FCurrentNode.Prev;
  i := length(FOuterNodes);
  if FCurrentNode.IsInFold then begin
    while (FCurrentNode.fData.Nested <> nil) do begin
      SetLength(FOuterNodes, i + 1);
      FOuterNodes[i] := FCurrentNode;
      NewData := FCurrentNode.fData.Nested;
      FCurrentNode.fData := NewData;
      FCurrentNode.FStartLine := FCurrentNode.FStartLine + NewData.LineOffset;

      PNode := FCurrentNode.Next;
      while PNode.IsInFold do begin
        FCurrentNode := PNode;
        PNode := FCurrentNode.Next;
      end;
    end;
  end
  else // not IsInFold
  if (i > 0) then begin
    dec(i);
    FCurrentNode := FOuterNodes[i];
    SetLength(FOuterNodes, i);
  end;
  Result := FCurrentNode;
end;

function TSynTextFoldAVLNodeNestedIterator.EOF: Boolean;
begin
  Result := not FCurrentNode.Next.IsInFold;
end;

function TSynTextFoldAVLNodeNestedIterator.BOF: Boolean;
begin
  Result := not FCurrentNode.Prev.IsInFold;
end;

function TSynTextFoldAVLNodeNestedIterator.IsInFold: Boolean;
begin
  Result := FCurrentNode.IsInFold;
end;

{ TSynFoldNodeInfoHelper }

constructor TSynFoldNodeInfoHelper.Create(AHighlighter: TSynCustomFoldHighlighter);
begin
  inherited Create;
  FHighlighter := AHighlighter;
  Invalidate;
end;

function TSynFoldNodeInfoHelper.FirstOpen: TSynFoldNodeInfo;
begin
  FActions := [sfaOpen, sfaFold];
  FCurInfo.NodeIndex := -1;
  FCurInfo.LineIndex := 0;
  Result := Next;
end;

procedure TSynFoldNodeInfoHelper.Invalidate;
begin
  FCurInfo.FoldAction := [sfaInvalid];
end;

function TSynFoldNodeInfoHelper.Next: TSynFoldNodeInfo;
var
  Cnt, Line, Idx: LongInt;
begin
  Idx := FCurInfo.NodeIndex + 1;
  Line := FCurInfo.LineIndex;
  Cnt := FHighlighter.FoldNodeInfoCount[Line, FActions];
  if Idx >= Cnt then begin
    Idx := 0;
    inc(Line);
    while (Line < FHighlighter.CurrentLines.Count) and
          (FHighlighter.FoldNodeInfoCount[Line, FActions] = 0)
    do
      inc(Line);
  end;
  if (Line < FHighlighter.CurrentLines.Count) then
    FCurInfo := FHighlighter.FoldNodeInfo[Line, Idx, FActions]
  else
    Invalidate;
  Result := FCurInfo;
end;

function TSynFoldNodeInfoHelper.Prev: TSynFoldNodeInfo;
var
  Line, Idx: LongInt;
begin
  Idx := FCurInfo.NodeIndex - 1;
  Line := FCurInfo.LineIndex;
  if Idx < 0 then begin
    dec(Line);
    while (Line >= 0) and
          (FHighlighter.FoldNodeInfoCount[Line, FActions] = 0)
    do
      dec(Line);
    Idx := FHighlighter.FoldNodeInfoCount[Line, FActions] - 1;
  end;
  if (Line >= 0) then
    FCurInfo := FHighlighter.FoldNodeInfo[Line, Idx, FActions]
  else
    Invalidate;
  Result := FCurInfo;
end;

function TSynFoldNodeInfoHelper.FindClose(KnownLength: Integer): TSynFoldNodeInfo;
var
  Line, Cnt: Integer;
  NdInfo: TSynFoldNodeInfo;
begin
  Line := FCurInfo.LineIndex;
  if KnownLength < 0 then
    KnownLength := FHighlighter.FoldLineLength(Line, FCurInfo.NodeIndex);
  FActions := [sfaClose, sfaFold];
  Cnt := FHighlighter.FoldNodeInfoCount[Line + KnownLength, FActions] - 1;
  while Cnt >= 0 do begin
    NdInfo := FHighlighter.FoldNodeInfo[Line + KnownLength, Cnt, FActions];
    if (NdInfo.FoldLvlStart = FCurInfo.FoldLvlEnd) and
       (NdInfo.FoldType = FCurInfo.FoldType)
    then
      break;
    dec(Cnt);
  end;
  if Cnt < 0 then
    Invalidate
  else
    FCurInfo := NdInfo;
  Result := FCurInfo;
end;

function TSynFoldNodeInfoHelper.GotoOpenPos(aLineIdx, aNodeIdx: integer): TSynFoldNodeInfo;
begin
  FActions := [sfaOpen, sfaFold];
  FCurInfo := FHighlighter.FoldNodeInfo[aLineIdx, aNodeIdx, FActions];
  Result := FCurInfo;
end;

function TSynFoldNodeInfoHelper.GotoOpenAtChar(aLineIdx, aXPos: integer): TSynFoldNodeInfo;
var
  Cnt: Integer;
begin
  FActions := [sfaOpen, sfaFold];
  Cnt := FHighlighter.FoldNodeInfoCount[aLineIdx, FActions] - 1;
  while Cnt >= 0 do begin
    FCurInfo := FHighlighter.FoldNodeInfo[aLineIdx, Cnt, FActions];
    if FCurInfo.LogXStart = aXPos then break;
    dec(Cnt);
  end;
  if Cnt < 0 then
    Invalidate;
  Result := FCurInfo;
end;

function TSynFoldNodeInfoHelper.GotoNodeOpenPos(ANode: TSynTextFoldAVLNode): TSynFoldNodeInfo;
begin
  FActions := [sfaOpen, sfaFold];
  FCurInfo := FHighlighter.FoldNodeInfo[ANode.StartLine-2, ANode.FoldIndex, FActions];
  Result := FCurInfo;
end;

function TSynFoldNodeInfoHelper.GotoNodeClosePos(ANode: TSynTextFoldAVLNode): TSynFoldNodeInfo;
var
  NdInfo, NdInfo2: TSynFoldNodeInfo;
  Cnt, EndCol, EndLineIdx: Integer;
begin
  FActions := [sfaClose, sfaFold];
  NdInfo := FHighlighter.FoldNodeInfo[ANode.StartLine-2, ANode.FoldIndex, [sfaOpen, sfaFold]];

  EndLineIdx := ANode.StartLine - 2 + ANode.LineCount;
  Cnt := FHighlighter.FoldNodeInfoCount[EndLineIdx, [sfaClose, sfaFold]];
  EndCol := 0;
  while EndCol < Cnt do begin
    NdInfo2 := FHighlighter.FoldNodeInfo[EndLineIdx, EndCol, [sfaClose, sfaFold]];
    if (NdInfo2.FoldLvlStart = NdInfo.FoldLvlEnd) and
       (NdInfo2.FoldType = NdInfo.FoldType) then break;
    inc(EndCol);
  end;
  if (EndCol = Cnt) or (sfaInvalid in NdInfo2.FoldAction) then
    Invalidate
  else
    FCurInfo := NdInfo2;
  Result := FCurInfo;
end;

function TSynFoldNodeInfoHelper.IsAtNodeOpenPos(ANode: TSynTextFoldAVLNode): Boolean;
begin
  Result := (not (sfaInvalid in FCurInfo.FoldAction)) and
            (ANode.IsInFold) and
            (FCurInfo.LineIndex = ANode.StartLine - 2) and
            (FCurInfo.NodeIndex = ANode.FoldIndex);
end;

function TSynFoldNodeInfoHelper.IsValid: Boolean;
begin
  Result := (not (sfaInvalid in FCurInfo.FoldAction));
end;

function TSynFoldNodeInfoHelper.Equals(AnInfo: TSynFoldNodeInfo): Boolean;
begin
  Result := (FCurInfo.LineIndex = AnInfo.LineIndex) and
            (FCurInfo.NodeIndex = AnInfo.NodeIndex) and
            (FCurInfo.LogXStart = AnInfo.LogXStart) and
            (FCurInfo.LogXEnd   = AnInfo.LogXEnd) and
            (FCurInfo.FoldLvlStart = AnInfo.FoldLvlStart) and
            (FCurInfo.FoldLvlEnd   = AnInfo.FoldLvlEnd) and
            (FCurInfo.FoldAction = AnInfo.FoldAction) and
            (FCurInfo.FoldType   = AnInfo.FoldType) and
            (FCurInfo.FoldGroup  = AnInfo.FoldGroup);
end;

function TSynFoldNodeInfoHelper.Equals(AHelper: TSynFoldNodeInfoHelper): Boolean;
begin
  Result := Equals(AHelper.Info);
end;

{ TSynTextFoldAVLTree }

function TSynTextFoldAVLTree.NewNode : TSynTextFoldAVLNodeData;
begin
  Result := TSynTextFoldAVLNodeData.Create;
end;

procedure TSynTextFoldAVLTree.DisposeNode(var ANode : TSynTextFoldAVLNodeData);
begin
  FreeAndNil(ANode);
end;

destructor TSynTextFoldAVLTree.Destroy;
begin
  Clear;
  if fNestedNodesTree <> nil then begin
    fNestedNodesTree.fRoot := nil; //was freed in self.Clear
    fNestedNodesTree.fNestParent := nil; // Or Destroy will access invalid memory
    fNestedNodesTree.Free;
  end;
  inherited Destroy;
end;

procedure TSynTextFoldAVLTree.Clear;
  procedure DeleteNode(var ANode: TSynTextFoldAVLNodeData);
  begin
    if ANode.Left <>nil   then DeleteNode(ANode.Left);
    if ANode.Right <>nil  then DeleteNode(ANode.Right);
    if ANode.Nested <>nil then DeleteNode(ANode.Nested);
    DisposeNode(ANode);
  end;
begin
  if fRoot <> nil then DeleteNode(fRoot);
  SetRoot(nil);
end;

procedure TSynTextFoldAVLTree.SetRoot(ANode : TSynTextFoldAVLNodeData); inline;
begin
  fRoot := ANode;
  if fNestParent <> nil then fNestParent.Nested := ANode;
  if ANode <> nil then ANode.Parent := nil;
end;

procedure TSynTextFoldAVLTree.SetRoot(ANode : TSynTextFoldAVLNodeData; anAdjustChildLineOffset : Integer);
  inline;
begin
  fRoot := ANode;
  if fNestParent <> nil then fNestParent.Nested := ANode;
  if ANode <> nil then begin
    ANode.Parent := nil;
    ANode.LineOffset := ANode.LineOffset + anAdjustChildLineOffset;
  end;
end;

(* Find Fold by Line in Real Text *)
function TSynTextFoldAVLTree.FindFoldForLine(ALine : Integer;
  FindNextNode : Boolean = False) : TSynTextFoldAVLNode;
var
  r : TSynTextFoldAVLNodeData;
  rStartLine : Integer;
  rFoldedBefore : Integer;
begin
  r := fRoot;
  rStartLine := fRootOffset;
  rFoldedBefore := 0;
  while (r <> nil) do begin
    rStartLine := rStartLine + r.LineOffset;

    if ALine < rStartLine then begin
      if FindNextNode and (r.Left = nil) then break;
      r := r.Left; // rStartLine points to r, so if r.Left is nil then it is pointing to the next fold;
      continue;
    end;

    rFoldedBefore := rFoldedBefore + r.LeftCount;
    if ALine < rStartLine + r.LineCount
    then break;

    if FindNextNode and (r.Right = nil) then begin
      r := r.Successor(rStartLine, rFoldedBefore);
      break;
    end;

    rFoldedBefore := rFoldedBefore + r.LineCount;
    r := r.Right; // rStartLine points to r, which now is the start of the previous fold;
  end;

  Result.Init(r, rStartLine, rFoldedBefore);
end;

(* Find Fold by Line in Folded Text // always returns unfolded, unless next=true *)
function TSynTextFoldAVLTree.FindFoldForFoldedLine(ALine : Integer;
  FindNextNode : Boolean) : TSynTextFoldAVLNode;
var
  r : TSynTextFoldAVLNodeData;
  rStartLine : Integer;
  rFoldedBefore : Integer;
begin
  r := fRoot;
  rStartLine := fRootOffset;
  rFoldedBefore := 0;
  while (r <> nil) do begin
    rStartLine := rStartLine + r.LineOffset;

    // r.LeftCount => "FoldedBefore"
    if ALine + r.LeftCount < rStartLine then begin
      if FindNextNode and (r.Left = nil) then break;
      r := r.Left; // rStartLine points to r, so if r.Left is nil then it is pointing to the next fold;
      continue;
    end;

    ALine := ALine + r.LeftCount + r.LineCount;
    rFoldedBefore := rFoldedBefore + r.LeftCount;

    if FindNextNode and (r.Right = nil) then begin
      r := r.Successor(rStartLine, rFoldedBefore);
      break;
    end;

    rFoldedBefore := rFoldedBefore + r.LineCount;
    r := r.Right; // rStartLine points to r, which now is the start of the previous fold;
  end;

  Result.Init(r, rStartLine, rFoldedBefore);
end;

procedure TSynTextFoldAVLTree.AdjustForLinesInserted(AStartLine, ALineCount : Integer);
  Procedure DoAdjustForLinesInserted(Current : TSynTextFoldAVLNodeData;
    CurrentLine : Integer);
  var
    t: LongInt;
  begin
    while (Current <> nil) do begin
      CurrentLine := CurrentLine + Current.LineOffset;

      if AStartLine < CurrentLine then begin
        // move current node
        Current.LineOffset := Current.LineOffset + ALineCount;
        CurrentLine := CurrentLine + ALineCount;
        if Current.Left <> nil then
          Current.Left.LineOffset := Current.Left.LineOffset - ALineCount;
        Current := Current.Left;
      end
      else if AStartLine > CurrentLine + Current.LineCount - 1 then begin
        // The new lines are entirly behind the current node
        Current := Current.Right;
      end
      else begin
        // grow current node
        // CurrentLine <= AStartLine  <= CurrentLine + Current.FullCount - 1
        t := Current.FullCount;
        if AStartLine <= CurrentLine + t - 1 then
          Current.FullCount := t + ALineCount;
        Current.LineCount := Current.LineCount + ALineCount;
        Current.AdjustParentLeftCount(ALineCount);
        TreeForNestedNode(Current, CurrentLine).AdjustForLinesInserted(AStartLine, ALineCount);

        if Current.Right <> nil then // and move entire right
          Current.Right.LineOffset := Current.Right.LineOffset + ALineCount;
        break;
      end;
    end;
  end;

begin
  DoAdjustForLinesInserted(fRoot, fRootOffset);
end;

procedure TSynTextFoldAVLTree.AdjustForLinesDeleted(AStartLine,
  ALineCount : Integer);
  Procedure AdjustNodeForLinesDeleted(Current : TSynTextFoldAVLNodeData;
    CurrentLine, FirstLineToDelete, CountLinesToDelete : Integer);
  var
    LastLineToDelete, LinesBefore, LinesInside, LinesAfter, t : Integer;
  begin
    LastLineToDelete := FirstLineToDelete + CountLinesToDelete - 1; // only valid for delete; CountLinesToDelete < 0

    while (Current <> nil) do begin
      CurrentLine := CurrentLine + Current.LineOffset;

      if FirstLineToDelete < CurrentLine then begin
        // move current node
        if LastLineToDelete >= CurrentLine then begin
          // overlap => shrink
          LinesBefore := CurrentLine - FirstLineToDelete;
          LinesInside := CountLinesToDelete - LinesBefore;
          // shrink
          t := Current.LineCount;
          Current.FullCount := Max(Current.FullCount - LinesInside, -1);
          Current.LineCount := Max(Current.LineCount - LinesInside, 0);
          Current.AdjustParentLeftCount(Current.LineCount - t); // If LineCount = -1; LeftCount will be correctd on delete node
          TreeForNestedNode(Current, CurrentLine).AdjustForLinesDeleted(CurrentLine, LinesInside);

          if (Current.Right <> nil) then begin
            // move right // Calculate from the new curent.LineOffset, as below
            AdjustNodeForLinesDeleted(Current.Right, CurrentLine - LinesBefore,
                                      FirstLineToDelete, LinesInside);
          end;
        end
        else LinesBefore := CountLinesToDelete;

        // move current node (includes right subtree / left subtree needs eval)
        Current.LineOffset := Current.LineOffset - LinesBefore;
        CurrentLine := CurrentLine - LinesBefore;
        if Current.Left <> nil then
          Current.Left.LineOffset := Current.Left.LineOffset + LinesBefore;
        Current := Current.Left;
      end
      else if FirstLineToDelete > CurrentLine + Current.LineCount - 1 then begin
        // The deleted lines are entirly behind the current node
        Current := Current.Right;
      end
      else begin
        // (FirstLineToDelete >= CurrentLine) AND (FirstLineToDelete < CurrentLine + Current.LineCount);
        LinesAfter  := LastLineToDelete - (CurrentLine + Current.LineCount - 1);
        if LinesAfter < 0 then LinesAfter := 0;
        LinesInside := CountLinesToDelete - LinesAfter;

        // shrink current node
        t := Current.LineCount;
        Current.LineCount := Current.LineCount - LinesInside;
        if Current.FullCount > Current.LineCount then
          Current.FullCount := Current.LineCount;
        Current.AdjustParentLeftCount(Current.LineCount - t); // If LineCount = -1; LeftCount will be correctd on delete node

        TreeForNestedNode(Current, CurrentLine).AdjustForLinesDeleted(FirstLineToDelete, LinesInside);
        Current := Current.Right;
      end;

    end;
  end;

begin
  AdjustNodeForLinesDeleted(fRoot, fRootOffset, AStartLine, ALineCount);
end;

function TSynTextFoldAVLTree.FindLastFold : TSynTextFoldAVLNode;
var
  r : TSynTextFoldAVLNodeData;
  rStartLine : Integer;
  rFoldedBefore : Integer;
begin
  r := fRoot;
  rStartLine := fRootOffset;
  rFoldedBefore := 0;
  while (r <> nil) do begin
    rStartLine := rStartLine + r.LineOffset;
    rFoldedBefore := rFoldedBefore + r.LeftCount + r.LineCount;
    if r.Right = nil then break;
    r := r.Right; // rStartLine points to r, which now is the start of the previous fold;
  end;

  Result.Init(r, rStartLine, rFoldedBefore);
end;

function TSynTextFoldAVLTree.FindFirstFold : TSynTextFoldAVLNode;
var
  r : TSynTextFoldAVLNodeData;
  rStartLine : Integer;
begin
  r := fRoot;
  rStartLine := fRootOffset;
  while (r <> nil) do begin
    rStartLine := rStartLine + r.LineOffset;
    if r.Left = nil then break;
    r := r.Left;
  end;

  Result.Init(r, rStartLine, 0);
end;

function TSynTextFoldAVLTree.LastFoldedLine: integer;
var
  n: TSynTextFoldAVLNode;
begin
  n := FindFirstFold;
  if not n.IsInFold then exit(0);
  Result := n.StartLine + n.LineCount - 1;
end;

procedure TSynTextFoldAVLTree.debug;
  function debug2(ind, typ : String; ANode, AParent : TSynTextFoldAVLNodeData; offset : integer) :integer;
  begin
    result := 0;
    if ANode = nil then exit;
    with ANode do
      DebugLn([Format('L=%3d-%3d (e=%3d / x=%d):  %2d / %2d',
                      [offset + ANode.LineOffset, offset + ANode.LineOffset + ANode.FullCount -1,
                       offset + ANode.LineOffset + ANode.LineCount -1, ANode.FoldIndex,
                       LineCount, FullCount]),
               ind, typ, ' (',LineOffset, ')  LeftCount: ', LeftCount,
               '     Balance: ',Balance]);
    if ANode.Parent <> AParent then DebugLn([ind,'* Bad parent']);
    Result := debug2(ind+'   ', 'L', ANode.Left, ANode, offset+ANode.LineOffset);
    If Result <> ANode.LeftCount then  debugln([ind,'   ***** Leftcount was ',Result, ' but should be ', ANode.LeftCount]);
    Result := Result + debug2(ind+'   ', 'R', ANode.Right, ANode, offset+ANode.LineOffset);
    debug2(ind+'  #', 'N', ANode.Nested, nil, offset+ANode.LineOffset);
    Result := Result + ANode.LineCount;
  end;
begin
  debug2('', ' -', fRoot, nil, 0);
end;

function TSynTextFoldAVLTree.InsertNewFold(ALine, AColumn, ACount : Integer) : TSynTextFoldAVLNode;
var
  r : TSynTextFoldAVLNodeData;
begin
  {$IFDEF SYNFOLDDEBUG}debugln(['FOLD-- InsertNewFold ALine:=', ALine, '  AColumn=', AColumn]);{$ENDIF}
  r := NewNode;
  r.LineOffset := ALine;
  r.FoldIndex := AColumn;
  r.LineCount  := ACount;
  r.FullCount  := ACount;
  r.LeftCount  := 0;

  Result.Init(r, ALine, 0);
  Result.fFoldedBefore := InsertNode(r);
end;

function TSynTextFoldAVLTree.RemoveFoldForLine(ALine : Integer;
  IgnoreFirst : Boolean = False; OnlyCol: Integer = -1) : Integer;
var
  OldFold : TSynTextFoldAVLNode;
  lcount: Integer;
begin
  {$IFDEF SYNFOLDDEBUG}debugln(['FOLD-- RemoveFoldForLine ALine:=', ALine, '  IgnoreFirst=', IgnoreFirst,'  OnlyCol=',OnlyCol]);{$ENDIF}
  Result := ALine - 1; // Return index
  OldFold := FindFoldForLine(ALine, true);
  if OldFold.StartLine-2 < Result then
    Result := OldFold.StartLine-2;
  if (not OldFold.IsInFold) // behind last node
  or (IgnoreFirst and (OldFold.StartLine > ALine))
  or ((not IgnoreFirst) and (OldFold.StartLine > ALine+1))
  then exit;
  if OnlyCol < 0 then
    RemoveFoldForNodeAtLine(OldFold, ALine, IgnoreFirst)
  else
  if OldFold.FoldIndex = OnlyCol then
    RemoveFoldForNodeAtLine(OldFold, -1, IgnoreFirst)
  else
  if OldFold.fData.Nested <> nil then begin
    TreeForNestedNode(OldFold.fData, OldFold.StartLine).RemoveFoldForLine
      (ALine, IgnoreFirst, OnlyCol);
    lcount := max(OldFold.FullCount,
                  TreeForNestedNode(OldFold.fData, 0).LastFoldedLine + 1);
    if lcount <> OldFold.LineCount then begin
      OldFold.fData.LineCount := lcount;
      OldFold.fData.AdjustParentLeftCount(OldFold.LineCount - lcount);
    end;
  end;
end;

function TSynTextFoldAVLTree.RemoveFoldForNodeAtLine(ANode : TSynTextFoldAVLNode;
  ALine : Integer; IgnoreFirst : Boolean = False) : Integer;
var
  NestedNode, MergeNode : TSynTextFoldAVLNodeData;
  NestedLine, offs, lcount : Integer;
  OnlyNested: Boolean;
  Nested: TSynTextFoldAVLNode;
begin
  {$IFDEF SYNFOLDDEBUG}debugln(['FOLD-- RemoveFoldForNodeAtLine: ALine:=', ALine, ' ANode.StartLine=', ANode.StartLine]);{$ENDIF}
  OnlyNested := ALine >= ANode.StartLine + ANode.FullCount;
  // The cfCollapsed line is one line before the fold
  Result := ANode.StartLine-1;  // Return the cfcollapsed that was unfolded
  if not OnlyNested then
    RemoveNode(ANode.fData);

  If ANode.fData.Nested <> nil then
  begin
    (*Todo: should we mark the tree as NO balancing needed ???*)
    TreeForNestedNode(ANode.fData, ANode.StartLine).RemoveFoldForLine(ALine, IgnoreFirst);

    if OnlyNested then begin
      NestedLine := ANode.StartLine + ANode.FullCount;
      Nested := TreeForNestedNode(ANode.fData, ANode.StartLine).FindLastFold;
      while Nested.IsInFold and (Nested.StartLine >= NestedLine) do begin
        NestedNode := Nested.fData;
        offs := Nested.StartLine;
        Nested := Nested.Prev;

        lcount := ANode.fData.LineCount;
        ANode.fData.LineCount := max(ANode.FullCount,
                         Nested.StartLine + Nested.LineCount - ANode.StartLine);
        ANode.fData.AdjustParentLeftCount(ANode.LineCount - lcount);

        TreeForNestedNode(ANode.fData, ANode.StartLine).RemoveNode(NestedNode);
        NestedNode.LineOffset := offs;
        InsertNode(NestedNode);
      end;
      lcount := max(ANode.FullCount,
                TreeForNestedNode(ANode.fData, ANode.StartLine).LastFoldedLine
                - ANode.StartLine + 1);
      if lcount <> ANode.LineCount then begin
        ANode.fData.LineCount := lcount;
        ANode.fData.AdjustParentLeftCount(ANode.LineCount - lcount);
      end;
    end
    else begin
      // merge the remaining nested into current
      NestedNode := ANode.fData.Nested;
      if NestedNode <> nil
      then NestedLine := ANode.fStartLine + NestedNode.LineOffset;

      while NestedNode <> nil do begin
        while NestedNode.Left <> nil do begin
          NestedNode := NestedNode.Left;
          NestedLine := NestedLine + NestedNode.LineOffset;
        end;

        if NestedNode.Right <> nil then begin
          NestedNode := NestedNode.Right;
          NestedLine := NestedLine + NestedNode.LineOffset;
          continue;
        end;

        // leaf node
        // Anything that is still nested (MergeNode.Nested), will stay nested
        MergeNode := NestedNode;

        NestedLine := NestedLine - NestedNode.LineOffset;
        NestedNode := NestedNode.Parent;

        MergeNode.LineOffset := MergeNode.LineOffset + NestedLine;
        if NestedNode <> nil then begin
          NestedNode.ReplaceChild(MergeNode, nil);
          MergeNode.Parent := nil;
        end;
        MergeNode.LeftCount := 0;
        MergeNode.Balance   := 0;
        InsertNode(MergeNode);
      end;
    end;

  end;

  if not OnlyNested then
    DisposeNode(ANode.fData);
end;

function TSynTextFoldAVLTree.InsertNode(ANode : TSynTextFoldAVLNodeData) : Integer;
var
  rStartLine, NestStartLine : Integer;
  rFoldedBefore, NestFoldedBefore : Integer;

  current, Nest : TSynTextFoldAVLNodeData;
  ALine, AEnd, ACount : Integer;

  (* ANode.StartLine < Current.StartLine // ANode goes into tree  *)
  procedure NestCurrentIntoNewBlock; inline;
  var
    diff, start2, before2 : Integer;
    p : TSynTextFoldAVLNodeData;
  begin
    current.AdjustParentLeftCount(ACount-current.LineCount); // -RecursiveFoldCount(current));
    rStartLine := rStartLine - current.LineOffset;  // rStarteLine is now current.Parent
    p := current.Parent;
    if p <> nil
    then p.ReplaceChild(current, ANode, -rStartLine)
    else SetRoot(ANode, -rStartLine);

    diff := current.LineOffset - ANode.LineOffset;
    ANode.Nested  := current;
    ANode.Balance := current.Balance;
    current.LineOffset := diff; // offset to ANode (via Nested)
    current.Parent := nil;
    current.Balance := 0;
    
    ANode.SetLeftChild(current.Left, diff, current.LeftCount);
    current.Left := nil;
    current.LeftCount := 0;
    ANode.SetRightChild(current.Right, diff);
    current.Right := nil;

    start2 := ALine; before2 := rFoldedBefore;
    p := ANode.Successor(start2, before2);
    while (p <> nil) and (start2 <= AEnd) do begin
      RemoveNode(p);
      p.LineOffset := start2- ALine;
      TreeForNestedNode(Anode, 0).InsertNode(p);

      start2 := ALine; before2 := rFoldedBefore;
      p := ANode.Successor(start2, before2);
    end;
    // check only after loop, if we gre, we did so by existing nodes, so no new overlaps
    start2 := TreeForNestedNode(Anode, 0).LastFoldedLine;
    if start2 > ANode.FullCount - 1 then begin
      ANode.AdjustParentLeftCount(start2 + 1 - ANode.LineCount);
      ANode.LineCount := start2 + 1;
    end;
  end;

  (* ANode.StartLine > Current.StartLine // Current remains in tree  *)
  procedure NestNewBlockIntoCurrent; //inline;
  var
    end2, start2, before2: Integer;
    p: TSynTextFoldAVLNodeData;
  begin
    // Check if current.LineCount needs extension
    ANode.LineOffset := ALine - rStartLine;
    if current.Nested <> nil
    then TreeForNestedNode(current, 0).InsertNode(ANode)
    else current.Nested := ANode;

    end2 := TreeForNestedNode(current, 0).LastFoldedLine;
    if end2 > current.FullCount -1 then begin
      end2 := rStartLine + end2;

      start2 := rStartLine; before2 := rFoldedBefore;
      p := current.Successor(start2, before2);
      while (p <> nil) and (start2 <= end2) do begin
        RemoveNode(p);
        p.LineOffset := start2 - rStartLine;
        TreeForNestedNode(current, 0).InsertNode(p);

        start2 := rStartLine; before2 := rFoldedBefore;
        p := current.Successor(start2, before2);
      end;
      end2 := TreeForNestedNode(current, 0).LastFoldedLine;
      if end2 > current.FullCount -1 then begin
        current.AdjustParentLeftCount(end2 + 1 - current.LineCount);
        current.LineCount := end2 + 1;
      end;
    end;
  end;

begin
  if fRoot = nil then begin
    SetRoot(ANode, -fRootOffset);
    Result := 0;
    exit;
  end;
  ALine := ANode.LineOffset;
  ACount := ANode.LineCount;
  AEnd := ALine + ACount - 1;
  current := fRoot;
  rStartLine := fRootOffset;
  rFoldedBefore := 0;
  Nest := nil;
  NestFoldedBefore := 0;
  NestStartLine := 0;

  while (current <> nil) do begin
    rStartLine := rStartLine + current.LineOffset;
    
    if ALine < rStartLine then begin
      (* *** New block goes to the left *** *)
      // remember possible nesting, continue scan for nesting with precessor
      if (AEnd >= rStartLine) then begin
        Nest := current;
        NestFoldedBefore := rFoldedBefore;
        NestStartLine := rStartLine;
      end;

      if current.Left <> nil Then begin
        current := current.Left;
        continue;
      end
      else if Nest = nil then begin // insert as Left - no nesting
        current.AdjustParentLeftCount(ACount);
        current.SetLeftChild(ANode, -rStartLine, ANode.LineCount);
        BalanceAfterInsert(ANode);
      end
      else begin // nest
        current := Nest;
        rStartLine := NestStartLine;
        rFoldedBefore := NestFoldedBefore;
        NestCurrentIntoNewBlock;
      end;
      break;
    end;

    rFoldedBefore := rFoldedBefore + current.LeftCount;
    if ALine = rStartLine then begin
      if ANode.FoldIndex > current.FoldIndex then
        (* *** New Block will be nested in current *** *)
        NestNewBlockIntoCurrent
      else
      if ANode.FoldIndex < current.FoldIndex then
        (* *** current will be nested in New Block *** *)
        NestCurrentIntoNewBlock
      else begin
        debugln(['Droping Foldnode / Already exists. Startline=', rStartLine,' LineCount=',ACount]);
        FreeAndNil(ANode);
      end;
    end
    else begin
      If ALine <= rStartLine + current.LineCount - 1
      (* *** New Block will be nested in current *** *)
      then NestNewBlockIntoCurrent
      (* *** New block goes to the right *** *)
      else begin
        rFoldedBefore := rFoldedBefore + current.LineCount;
        if current.Right <> nil then begin
          current := current.Right;
          continue;
        end
        else  if Nest=nil then Begin  // insert to the right - no nesting
          current.AdjustParentLeftCount(ACount);
          current.SetRightChild(ANode, -rStartLine);
          BalanceAfterInsert(ANode);
        end
        else begin // nest
          current := Nest;
          rStartLine := NestStartLine;
          rFoldedBefore := NestFoldedBefore;
          NestCurrentIntoNewBlock;
        end;

      end;
    end;
    
    break;
  end; // while

  Result := rFoldedBefore;
end;

procedure TSynTextFoldAVLTree.RemoveNode(ANode: TSynTextFoldAVLNodeData);
var OldParent, Precessor, PrecOldParent, PrecOldLeft,
  OldSubTree: TSynTextFoldAVLNodeData;
  OldBalance, PrecOffset, PrecLeftCount: integer;

begin
  if ((ANode.Left<>nil) and (ANode.Right<>nil)) then begin
    PrecOffset := 0;
//    PrecOffset := ANode.LineOffset;
    Precessor := ANode.Left;
    while (Precessor.Right<>nil) do begin
      PrecOffset := PrecOffset + Precessor.LineOffset;
      Precessor := Precessor.Right;
    end;
(*                            *OR*
 PnL              PnL
   \               \
   Precessor       Anode
   /               /
  *               *                     PnL             PnL
 /               /                        \               \
AnL   AnR       AnL      AnR        Precessor   AnR       AnL      AnR
  \   /           \      /                  \   /           \      /
   Anode          Precessor()               Anode          Precessor()
*)
    OldBalance := ANode.Balance;
    ANode.Balance     := Precessor.Balance;
    Precessor.Balance := OldBalance;

    // Successor.Left = nil
    PrecOldLeft   := Precessor.Left;
    PrecOldParent := Precessor.Parent;

    if (ANode.Parent<>nil)
    then ANode.Parent.ReplaceChild(ANode, Precessor, PrecOffset + ANode.LineOffset)
    else SetRoot(Precessor, PrecOffset + ANode.LineOffset);

    Precessor.SetRightChild(ANode.Right,
                           +ANode.LineOffset-Precessor.LineOffset);

    PrecLeftCount := Precessor.LeftCount;
    // ANode.Right will be empty  // ANode.Left will be Succesor.Left
    if (PrecOldParent = ANode) then begin
      // Precessor is left son of ANode
      // set ANode.LineOffset=0 => LineOffset for the Prec-Children is already correct;
      Precessor.SetLeftChild(ANode, -ANode.LineOffset,
                             PrecLeftCount + ANode.LineCount);
      ANode.SetLeftChild(PrecOldLeft, 0, PrecLeftCount);
    end else begin
      // at least one node between ANode and Precessor ==> Precessor = PrecOldParent.Right
      Precessor.SetLeftChild(ANode.Left, +ANode.LineOffset - Precessor.LineOffset,
                             ANode.LeftCount + ANode.LineCount - Precessor.LineCount);
      PrecOffset:=PrecOffset + ANode.LineOffset - Precessor.LineOffset;
      // Set Anode.LineOffset, so ANode movesinto position of Precessor;
      PrecOldParent.SetRightChild(ANode, - ANode.LineOffset -  PrecOffset);
      ANode.SetLeftChild(PrecOldLeft, 0, PrecLeftCount);
    end;

    ANode.Right := nil;
  end;

  if (ANode.Right<>nil) then begin
    OldSubTree := ANode.Right;
    ANode.Right := nil;
  end
  else if (ANode.Left<>nil) then begin
    OldSubTree := ANode.Left;
    ANode.Left := nil;
  end
  else OldSubTree := nil;

  OldParent := ANode.Parent;
  ANode.Parent := nil;
  ANode.Left := nil;
  ANode.Right := nil;
  ANode.Balance := 0;
  ANode.LeftCount := 0;
  // nested???

  if (OldParent<>nil) then begin      // Node has parent
    if OldParent.ReplaceChild(ANode, OldSubTree, ANode.LineOffset) = rplcLeft
    then begin
      Inc(OldParent.Balance);
      OldParent.AdjustLeftCount(-ANode.LineCount);
    end
    else begin
      Dec(OldParent.Balance);
      OldParent.AdjustParentLeftCount(-ANode.LineCount);
    end;
    BalanceAfterDelete(OldParent);
  end
  else SetRoot(OldSubTree, ANode.LineOffset);
end;


procedure TSynTextFoldAVLTree.BalanceAfterInsert(ANode : TSynTextFoldAVLNodeData);
var OldParent, OldParentParent, OldRight, OldRightLeft, OldRightRight, OldLeft,
   OldLeftLeft, OldLeftRight: TSynTextFoldAVLNodeData;
   tmp : integer;
begin
  OldParent := ANode.Parent;
  if (OldParent=nil) then exit;

  if (OldParent.Left=ANode) then begin
    (* *** Node is left son *** *)
    dec(OldParent.Balance);
    if (OldParent.Balance=0) then exit;
    if (OldParent.Balance=-1) then begin
      BalanceAfterInsert(OldParent);
      exit;
    end;

    // OldParent.Balance=-2
    if (ANode.Balance=-1) then begin
      (* ** single rotate ** *)
      (*  []
           \
           []  ORight                     []    ORight    []
            \   /                          \      \       /
            ANode(-1)  []        =>        []     OldParent(0)
               \       /                    \     /
               OldParent(-2)                 ANode(0)
      *)
      OldRight := ANode.Right;
      OldParentParent := OldParent.Parent;
      (* ANode moves into position of OldParent *)
      if (OldParentParent<>nil)
      then OldParentParent.ReplaceChild(OldParent, ANode, OldParent.LineOffset)
      else SetRoot(ANode, OldParent.LineOffset);

      (* OldParent moves under ANode, replacing Anode.Right, which moves under OldParent *)
      ANode.SetRightChild(OldParent, -ANode.LineOffset );
      OldParent.SetLeftChild(OldRight, -OldParent.LineOffset, OldParent.LeftCount - ANode.LineCount - ANode.LeftCount);

      ANode.Balance := 0;
      OldParent.Balance := 0;
      (* ** END single rotate ** *)
    end
    else begin  // ANode.Balance = +1
      (* ** double rotate ** *)
      OldParentParent := OldParent.Parent;
      OldRight := ANode.Right;
      OldRightLeft := OldRight.Left;
      OldRightRight := OldRight.Right;

      (* OldRight moves into position of OldParent *)
      if (OldParentParent<>nil)
      then OldParentParent.ReplaceChild(OldParent, OldRight, OldParent.LineOffset + ANode.LineOffset)
      else SetRoot(OldRight, OldParent.LineOffset + ANode.LineOffset);        // OldParent was root node. new root node
      
      OldRight.SetRightChild(OldParent, -OldRight.LineOffset);
      OldRight.SetLeftChild(ANode, OldParent.LineOffset, OldRight.LeftCount + ANode.LeftCount + ANode.LineCount);
      ANode.SetRightChild(OldRightLeft, -ANode.LineOffset);
      OldParent.SetLeftChild(OldRightRight, -OldParent.LineOffset, OldParent.LeftCount - OldRight.LeftCount - OldRight.LineCount);

      // balance
      if (OldRight.Balance<=0)
      then ANode.Balance := 0
      else ANode.Balance := -1;
      if (OldRight.Balance=-1)
      then OldParent.Balance := 1
      else OldParent.Balance := 0;
      OldRight.Balance := 0;
      (* ** END double rotate ** *)
    end;
    (* *** END Node is left son *** *)
  end
  else begin
    (* *** Node is right son *** *)
    Inc(OldParent.Balance);
    if (OldParent.Balance=0) then exit;
    if (OldParent.Balance=+1) then begin
      BalanceAfterInsert(OldParent);
      exit;
    end;

    // OldParent.Balance = +2
    if(ANode.Balance=+1) then begin
      (* ** single rotate ** *)
      OldLeft := ANode.Left;
      OldParentParent := OldParent.Parent;
      
      if (OldParentParent<>nil)
      then  OldParentParent.ReplaceChild(OldParent, ANode, OldParent.LineOffset)
      else SetRoot(ANode, OldParent.LineOffset);

      (* OldParent moves under ANode, replacing Anode.Left, which moves under OldParent *)
      ANode.SetLeftChild(OldParent, -ANode.LineOffset, ANode.LeftCount + OldParent.LineCount + OldParent.LeftCount);
      OldParent.SetRightChild(OldLeft, -OldParent.LineOffset);
      
      ANode.Balance := 0;
      OldParent.Balance := 0;
      (* ** END single rotate ** *)
    end
    else begin  // Node.Balance = -1
      (* ** double rotate ** *)
      OldLeft := ANode.Left;
      OldParentParent := OldParent.Parent;
      OldLeftLeft := OldLeft.Left;
      OldLeftRight := OldLeft.Right;

      (* OldLeft moves into position of OldParent *)
      if (OldParentParent<>nil)
      then  OldParentParent.ReplaceChild(OldParent, OldLeft, OldParent.LineOffset + ANode.LineOffset)
      else SetRoot(OldLeft, OldParent.LineOffset + ANode.LineOffset);

      tmp := OldLeft.LeftCount;
      OldLeft.SetLeftChild (OldParent, -OldLeft.LineOffset, tmp + OldParent.LeftCount + OldParent.LineCount);
      OldLeft.SetRightChild(ANode, OldParent.LineOffset);

      OldParent.SetRightChild(OldLeftLeft, -OldParent.LineOffset);
      ANode.SetLeftChild(OldLeftRight, -ANode.LineOffset, ANode.LeftCount - tmp - OldLeft.LineCount);

      // Balance
      if (OldLeft.Balance>=0)
      then ANode.Balance := 0
      else ANode.Balance := +1;
      if (OldLeft.Balance=+1)
      then OldParent.Balance := -1
      else OldParent.Balance := 0;
      OldLeft.Balance := 0;
      (* ** END double rotate ** *)
    end;
  end;
end;

procedure TSynTextFoldAVLTree.BalanceAfterDelete(ANode : TSynTextFoldAVLNodeData);
var OldParent, OldRight, OldRightLeft, OldLeft, OldLeftRight,
  OldRightLeftLeft, OldRightLeftRight, OldLeftRightLeft, OldLeftRightRight
  : TSynTextFoldAVLNodeData;
  tmp : integer;
begin
  if (ANode=nil) then exit;
  if ((ANode.Balance=+1) or (ANode.Balance=-1)) then exit;
  OldParent := ANode.Parent;
  if (ANode.Balance=0) then begin
    // Treeheight has decreased by one
    if (OldParent<>nil) then begin
      if(OldParent.Left=ANode) then
        Inc(OldParent.Balance)
      else
        Dec(OldParent.Balance);
      BalanceAfterDelete(OldParent);
    end;
    exit;
  end;

  if (ANode.Balance=-2) then begin
    // Node.Balance=-2
    // Node is overweighted to the left
    (*
          OLftRight
           /
        OLeft(<=0)
           \
             ANode(-2)
    *)
    OldLeft := ANode.Left;
    if (OldLeft.Balance<=0) then begin
      // single rotate left
      OldLeftRight := OldLeft.Right;
      
      if (OldParent<>nil)
      then OldParent.ReplaceChild(ANode, OldLeft, ANode.LineOffset)
      else SetRoot(OldLeft, ANode.LineOffset);

      OldLeft.SetRightChild(ANode, -OldLeft.LineOffset);
      ANode.SetLeftChild(OldLeftRight, -ANode.LineOffset, ANode.LeftCount - OldLeft.LineCount - OldLeft.LeftCount);

      ANode.Balance := (-1-OldLeft.Balance);
      Inc(OldLeft.Balance);

      BalanceAfterDelete(OldLeft);
    end else begin
      // OldLeft.Balance = 1
      // double rotate left left
      OldLeftRight := OldLeft.Right;
      OldLeftRightLeft := OldLeftRight.Left;
      OldLeftRightRight := OldLeftRight.Right;
      
(*
 OLR-Left   OLR-Right
      \     /
      OldLeftRight          OLR-Left    OLR-Right
       /                       /            \
   OldLeft                 OldLeft         ANode
      \                         \           /
     ANode                       OldLeftRight
       |                            |
     OldParent                   OldParent  (or root)
*)
      if (OldParent<>nil)
      then OldParent.ReplaceChild(ANode, OldLeftRight, ANode.LineOffset + OldLeft.LineOffset)
      else SetRoot(OldLeftRight, ANode.LineOffset + OldLeft.LineOffset);

      OldLeftRight.SetRightChild(ANode, -OldLeftRight.LineOffset);
      OldLeftRight.SetLeftChild(OldLeft, ANode.LineOffset, OldLeftRight.LeftCount + OldLeft.LeftCount + OldLeft.LineCount);
      OldLeft.SetRightChild(OldLeftRightLeft, -OldLeft.LineOffset);
      ANode.SetLeftChild(OldLeftRightRight,  -ANode.LineOffset, ANode.LeftCount - OldLeftRight.LeftCount - OldLeftRight.LineCount);

      if (OldLeftRight.Balance<=0)
      then OldLeft.Balance := 0
      else OldLeft.Balance := -1;
      if (OldLeftRight.Balance>=0)
      then ANode.Balance := 0
      else ANode.Balance := +1;
      OldLeftRight.Balance := 0;
      
      BalanceAfterDelete(OldLeftRight);
    end;
  end else begin
    // Node is overweighted to the right
    OldRight := ANode.Right;
    if (OldRight.Balance>=0) then begin
      // OldRight.Balance=={0 or -1}
      // single rotate right
      OldRightLeft := OldRight.Left;
      
      if (OldParent<>nil)
      then OldParent.ReplaceChild(ANode, OldRight, ANode.LineOffset)
      else SetRoot(OldRight, ANode.LineOffset);
      
      OldRight.SetLeftChild(ANode, -OldRight.LineOffset, OldRight.LeftCount + ANode.LineCount + ANode.LeftCount);
      ANode.SetRightChild(OldRightLeft, -ANode.LineOffset);

      ANode.Balance := (1-OldRight.Balance);
      Dec(OldRight.Balance);

      BalanceAfterDelete(OldRight);
    end else begin
      // OldRight.Balance=-1
      // double rotate right left
      OldRightLeft := OldRight.Left;
      OldRightLeftLeft := OldRightLeft.Left;
      OldRightLeftRight := OldRightLeft.Right;
      if (OldParent<>nil)
      then OldParent.ReplaceChild(ANode, OldRightLeft, ANode.LineOffset + OldRight.LineOffset)
      else SetRoot(OldRightLeft, ANode.LineOffset + OldRight.LineOffset);
      
      tmp := OldRightLeft.LeftCount;
      OldRightLeft.SetLeftChild(ANode, -OldRightLeft.LineOffset, tmp + ANode.LeftCount + ANode.LineCount);
      OldRightLeft.SetRightChild(OldRight, ANode.LineOffset);

      ANode.SetRightChild(OldRightLeftLeft, -ANode.LineOffset);
      OldRight.SetLeftChild(OldRightLeftRight, -OldRight.LineOffset, OldRight.LeftCount - tmp - OldRightLeft.LineCount);
        
      if (OldRightLeft.Balance<=0)
      then ANode.Balance := 0
      else ANode.Balance := -1;
      if (OldRightLeft.Balance>=0)
      then OldRight.Balance := 0
      else OldRight.Balance := +1;
      OldRightLeft.Balance := 0;
      BalanceAfterDelete(OldRightLeft);
    end;
  end;

end;

function TSynTextFoldAVLTree.TreeForNestedNode(ANode: TSynTextFoldAVLNodeData; aOffset : Integer) : TSynTextFoldAVLTree;
begin
  if fNestedNodesTree = nil then fNestedNodesTree := TSynTextFoldAVLTree.Create;
  Result := fNestedNodesTree;
  Result.fRoot := ANode.Nested;
  Result.fNestParent := ANode; // TODO: this is dangerous, this is never cleaned up, even if ANode is Destroyed
  Result.fRootOffset := aOffset;
end;

constructor TSynTextFoldAVLTree.Create;
begin
  fRoot := nil;
  fRootOffset := 0;
  fNestParent := nil;
  fNestedNodesTree := nil;
end;

{ TSynEditFoldedView }

constructor TSynEditFoldedView.Create(aTextView : TSynEditStrings; ACaret: TSynEditCaret);
begin
  FCurFoldDescription := '';
  fLines := aTextView;
  fCaret := ACaret;
  fCaret.AddChangeHandler({$IFDEF FPC}@{$ENDIF}DoCaretChanged);
  fFoldTree := TSynTextFoldAVLTree.Create;
  fTopLine := 0;
  fLinesInWindow := -1;

  FMarkupInfoFoldedCode := TSynSelectedColor.Create;
  FMarkupInfoFoldedCode.Background := clNone;
  FMarkupInfoFoldedCode.Foreground := clDkGray;
  FMarkupInfoFoldedCode.FrameColor := clDkGray;

  fLines.AddChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
end;

destructor TSynEditFoldedView.Destroy;
begin
  fLines.RemoveChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  fCaret.RemoveChangeHandler({$IFDEF FPC}@{$ENDIF}DoCaretChanged);
  fFoldTree.Free;
  fTextIndexList := nil;
  fFoldTypeList := nil;
  FMarkupInfoFoldedCode.Free;
  inherited Destroy;
end;

procedure TSynEditFoldedView.LinesInsertedAtTextIndex(AStartIndex, ALineCount : Integer; SkipFixFolding : Boolean);
var top : Integer;
begin
  if ALineCount = 0 then exit;
  top := TopTextIndex;
  fFoldTree.AdjustForLinesInserted(AStartIndex+1, ALineCount);
  if AStartIndex < top then
    TopTextIndex := top + ALineCount;
  if not(SkipFixFolding) then FixFoldingAtTextIndex(AStartIndex, AStartIndex+ALineCount+1)
  else
  if AStartIndex < top + ALineCount then CalculateMaps;
end;

procedure TSynEditFoldedView.LinesInsertedAtViewPos(AStartPos, ALineCount : Integer; SkipFixFolding : Boolean);
begin
  LinesInsertedAtTextIndex(ViewPosToTextIndex(AStartPos), ALineCount, SkipFixFolding);
end;

procedure TSynEditFoldedView.LinesDeletedAtTextIndex(AStartIndex, ALineCount : Integer; SkipFixFolding : Boolean);
var top : Integer;
begin
  top := TopTextIndex;
  // topline may get out of sync => synedit is always going to chnage it back
  fFoldTree.AdjustForLinesDeleted(AStartIndex+1, ALineCount);
  if not(SkipFixFolding) then
    FixFoldingAtTextIndex(AStartIndex, AStartIndex+ALineCount+1)
  else
  if AStartIndex < top - ALineCount then CalculateMaps;
end;

procedure TSynEditFoldedView.LinesDeletedAtViewPos(AStartPos, ALineCount : Integer; SkipFixFolding : Boolean);
begin
  LinesDeletedAtTextIndex(ViewPosToTextIndex(AStartPos), ALineCount, SkipFixFolding);
end;

function TSynEditFoldedView.TextIndexToViewPos(aTextIndex : Integer) : Integer;
var
  n: TSynTextFoldAVLNode;
begin
  n := fFoldTree.FindFoldForLine(aTextIndex + 1);
  if n.IsInFold then
    Result := n.StartLine - 1 - n.FoldedBefore
  else
    Result := aTextIndex + 1 - n.FoldedBefore;
end;

function TSynEditFoldedView.TextIndexToScreenLine(aTextIndex : Integer) : Integer;
begin
  Result := TextIndexToViewPos(aTextIndex) - TopLine;
end;

function TSynEditFoldedView.ViewPosToTextIndex(aViewPos : Integer) : Integer;
begin
  if aViewPos > Count then
    aViewPos := Count;
  result := aViewPos - 1 + fFoldTree.FindFoldForFoldedLine(aViewPos).FoldedBefore;
end;

function TSynEditFoldedView.ScreenLineToTextIndex(aLine : Integer) : Integer;
begin
  Result := ViewPosToTextIndex(aLine + TopLine);
end;

function TSynEditFoldedView.TextIndexAddLines(aTextIndex, LineOffset : Integer) : Integer;
var
  node : TSynTextFoldAVLNode;
  cnt : integer;
begin
  node := fFoldTree.FindFoldForLine(aTextIndex+1, True);
  result := aTextIndex;
  if LineOffset < 0 then begin
    if node.IsInFold
    then node := node.Prev
    else node := fFoldTree.FindLastFold;
    while LineOffset < 0 do begin
      if Result <= 0 then exit(0);
      dec(Result);
      if node.IsInFold and (Result+1 < node.StartLine + node.LineCount) then begin
        Result := Result - node.LineCount;
        node := node.Prev;
      end;
      inc(LineOffset);
    end;
  end else begin
    cnt := fLines.Count;
    while LineOffset > 0 do begin
      if Result >= cnt then exit(cnt);
      inc(Result);
      if node.IsInFold and (Result+1 >= node.StartLine) then begin
        Result := Result + node.LineCount;
        if Result >= cnt then exit(cnt-node.LineCount-1);
        node := node.Next;
      end;
      dec(LineOffset);
    end;
  end;
end;

function TSynEditFoldedView.TextPosAddLines(aTextpos, LineOffset : Integer) : Integer;
begin
  Result := TextIndexAddLines(aTextpos-1, LineOffset)+1;
end;

procedure TSynEditFoldedView.Lock;
begin
  if fLockCount=0 then begin
    fNeedFixFrom := -1;
    fNeedFixMinEnd := -1;
    fNeedCaretCheck := false;
  end;;
  inc(fLockCount);
end;

procedure TSynEditFoldedView.UnLock;
begin
  dec(fLockCount);
  if (fLockCount=0) then begin
    if (fNeedFixFrom >= 0) then
      FixFolding(fNeedFixFrom, fNeedFixMinEnd, fFoldTree);
    if fNeedCaretCheck then
      DoCaretChanged(fCaret);
  end;
end;

(* Count *)
function TSynEditFoldedView.GetCount : integer;
begin
  Result := fLines.Count - fFoldTree.FindLastFold.FoldedBefore;
end;

function TSynEditFoldedView.GetHighLighter: TSynCustomHighlighter;
begin
  Result := FHighLighter;
  if not(assigned(Result) and (Result is TSynCustomFoldHighlighter)) then
    exit(nil);
  Result.CurrentLines := fLines;
end;

(* Topline *)
procedure TSynEditFoldedView.SetTopLine(const ALine : integer);
begin
  if fTopLine = ALine then exit;
  fTopLine := ALine;
  CalculateMaps;
end;

function TSynEditFoldedView.GetTopTextIndex : integer;
begin
  Result := fTopLine + fFoldTree.FindFoldForFoldedLine(fTopLine).FoldedBefore - 1;
end;

procedure TSynEditFoldedView.SetTopTextIndex(const AIndex : integer);
begin
  TopLine := AIndex + 1 - fFoldTree.FindFoldForLine(AIndex+1).FoldedBefore;
end;

(* LinesInWindow*)
procedure TSynEditFoldedView.SetLinesInWindow(const AValue : integer);
begin
  if fLinesInWindow = AValue then exit;
  fLinesInWindow := AValue;
  SetLength(fTextIndexList, AValue + 1);
  SetLength(fFoldTypeList, AValue + 1);
  CalculateMaps;
end;

procedure TSynEditFoldedView.CalculateMaps;
var
  i, tpos, cnt  : Integer;
  node : TSynTextFoldAVLNode;
  hl: TSynCustomFoldHighlighter;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then begin
    for i := 0 to fLinesInWindow do begin
      fTextIndexList[i] := fTopLine + i - 1;
      fFoldTypeList[i] := cfNone;
    end;
    exit;
  end;

  node := fFoldTree.FindFoldForFoldedLine(fTopLine, true);
  // ftopline is not a folded line
  // so node.FoldedBefore(next node after ftopl) does apply
  tpos  := fTopLine + node.FoldedBefore;
  {$IFDEF SYNFOLDDEBUG}debugln(['FOLD-- CalculateMaps fTopLine:=', fTopLine, '  tpos=',tpos]);{$ENDIF}
  cnt := fLines.Count;
  for i := 0 to fLinesInWindow do begin
    if tpos > cnt then begin
      fTextIndexList[i] := -1;
      fFoldTypeList[i] := cfNone;
    end else begin
      fTextIndexList[i] := tpos - 1; // TextIndex is 0-based

      if (node.IsInFold) and (tpos+1 = node.StartLine)
      then fFoldTypeList[i] := cfCollapsed
      else
      if (hl.FoldOpenCount(tpos - 1) > 0)
      then fFoldTypeList[i] := cfExpanded
      else
      if (tpos > 1) and (hl.FoldCloseCount(tpos - 1) > 0)
      then fFoldTypeList[i] := cfEnd
      else
      if hl.FoldNestCount(tpos - 1) > 0
      then fFoldTypeList[i] := cfContinue
      else fFoldTypeList[i] := cfNone;

      inc(tpos);
      if (node.IsInFold) and (tpos >= node.StartLine) then begin
        tpos := tpos + node.LineCount;
        node := node.Next;
      end;
    end;
  end;
end;

(* Lines *)
function TSynEditFoldedView.GetLines(index : Integer) : String;
begin
  if (index < 0) or (index > fLinesInWindow) then
    exit(fLines[ScreenLineToTextIndex(Index)]);
  Result := fLines[fTextIndexList[index]];
end;

function TSynEditFoldedView.GetDisplayNumber(index : Integer) : Integer;
begin
  if (index < 0) or (index > fLinesInWindow)
  or (fTextIndexList[index] < 0) then exit(-1);
  Result := fTextIndexList[index]+1;
end;

function TSynEditFoldedView.GetTextIndex(index : Integer) : Integer;
begin
  if (index < 0) or (index > fLinesInWindow) then
    exit(ScreenLineToTextIndex(Index));
  Result := fTextIndexList[index];
end;

function TSynEditFoldedView.GetFoldType(index : Integer) : TSynEditCodeFoldType;
begin
  if (index < 0) or (index > fLinesInWindow) then exit(cfNone);
  Result := fFoldTypeList[index];
end;

function TSynEditFoldedView.IsFolded(index : integer) : Boolean;
begin
  Result := fFoldTree.FindFoldForLine(index+1).IsInFold;
end;

(* Folding *)

procedure TSynEditFoldedView.FoldAtLine(AStartLine : Integer;
  ColIndex : Integer = -1; ColCount : Integer = 1; Skip: Boolean = False);
begin
  FoldAtViewPos(AStartLine + fTopLine, ColIndex, ColCount, Skip);
end;

procedure TSynEditFoldedView.FoldAtViewPos(AStartPos : Integer;
  ColIndex : Integer = -1; ColCount : Integer = 1; Skip: Boolean = False);
begin
  FoldAtTextIndex(AStartPos - 1 + fFoldTree.FindFoldForFoldedLine(AStartPos).FoldedBefore,
                  ColIndex, ColCount, Skip);
end;

function TSynEditFoldedView.LengthForFoldAtTextIndex(ALine, AFoldIndex: Integer) : Integer;
var
  hl: TSynCustomFoldHighlighter;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit(0);
  Result := hl.FoldLineLength(ALine, AFoldIndex);
end;

function TSynEditFoldedView.LogicalPosToNodeIndex(AStartIndex: Integer; LogX: Integer;
  Previous: Boolean): Integer;
var
  hl: TSynCustomFoldHighlighter;
  c, i: Integer;
  nd: TSynFoldNodeInfo;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit(0);
  // AStartIndex is 0-based
  // FoldTree is 1-based AND first line remains visble
  c := hl.FoldNodeInfoCount[AStartIndex, [sfaOpen, sfaFold]];
  if c = 0 then
    exit(-1);
  i := 0;
  while i < c do begin
    nd := hl.FoldNodeInfo[aStartIndex, i, [sfaOpen, sfaFold]];
    if (nd.LogXStart >= LogX) then begin
      dec(i);
      if not Previous then
        i := -1;
      break;
    end;
    if (nd.LogXEnd >= LogX) then
      break;
    inc(i);
  end;
  Result := i;
end;

procedure TSynEditFoldedView.CollapseDefaultFolds;
var
  i, j, c: Integer;
  hl: TSynCustomFoldHighlighter;
  nd: TSynFoldNodeInfo;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit;
  FCurFoldDescription := '';

  i := 0;
  while i < fLines.Count do begin
     // Todo: Highlighter should return a list of typpes that can return default folded
     // Currently PascalHl Type 2 = Region
    c := hl.FoldOpenCount(i, 2);
    if c > 0 then begin
      c := hl.FoldNodeInfoCount[i, [sfaOpen, sfaFold]];
      j := 0;
      while j < c do begin
        nd := hl.FoldNodeInfo[i, j, [sfaOpen, sfaFold]];
        if (sfaDefaultCollapsed in nd.FoldAction) and
           (not IsFoldedAtTextIndex(i, j))
        then begin
          fFoldTree.InsertNewFold(i+2, j, LengthForFoldAtTextIndex(i, j));
          if Assigned(fOnFoldChanged) then
            fOnFoldChanged(i);
        end;
      inc(j);
      end;
    end;
    inc(i);
  end;
  CalculateMaps;
end;

function TSynEditFoldedView.GetFoldDescription(AStartIndex, AStartCol, AEndIndex,
  AEndCol: Integer; AsText: Boolean = False; Extended: Boolean = False): String;
var
  FoldCoders: Array of TSynEditFoldExportCoder;

  function FoldCoderForType(AType: Pointer): TSynEditFoldExportCoder;
  var
    i, j: Integer;
  begin
    i := 0;
    j := length(FoldCoders);
    while (i < j) and (FoldCoders[i].FoldType <> AType) do
      inc(i);
    if (i = j) then begin
      SetLength(FoldCoders, i + 1);
      FoldCoders[i] := TSynEditFoldExportCoder.Create(AType);
    end;
    Result := FoldCoders[i];
  end;

var
  hl: TSynCustomFoldHighlighter;
  FoldHelper: TSynEditFoldExportStream;
  NodeIterator: TSynTextFoldAVLNodeNestedIterator;
  NdiHelper1: TSynFoldNodeInfoHelper;
  Node: TSynTextFoldAVLNode;
  NdInfo, NdInfo2: TSynFoldNodeInfo;
  entry: TFoldExportEntry;
  i: Integer;
begin
  Result := '';
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then exit;

  if AEndIndex < 0 then AEndIndex := MaxInt;
  if AEndCol   < 0 then AEndCol   := MaxInt;

  Node := fFoldTree.FindFoldForLine(AStartIndex + 1, True);
  NodeIterator := TSynTextFoldAVLNodeNestedIterator.Create(Node);
  FoldHelper := TSynEditFoldExportStream.Create;
  NdiHelper1 := TSynFoldNodeInfoHelper.Create(hl);
  try
    if (AStartCol > 1) then
      while Node.IsInFold and (Node.StartLine = AStartIndex + 2) do begin
        NdInfo := NdiHelper1.GotoNodeOpenPos(Node);
        if (sfaInvalid in NdInfo.FoldAction) or (ndinfo.LogXStart >= AStartCol) then
          break;
        Node := NodeIterator.Next;
      end;
    dec(AStartCol);
    if not node.IsInFold then
      exit;

    if AsText then
    begin           (* *** Encode as Text for XML *** *)
      if FCurFoldDescription <> '' then exit(FCurFoldDescription);
      NdInfo := NdiHelper1.GotoNodeOpenPos(Node);
      while Node.IsInFold and (Node.StartLine-2 <= AEndIndex) do
      begin
        if (node.StartLine > AStartIndex + 2) then AStartCol := 0;

        if not(sfaDefaultCollapsed in NdInfo.FoldAction) then // Currently skip default nodes
          FoldCoderForType(NdInfo.FoldType).AddNode
                     (NdInfo.LogXStart, NdInfo.LineIndex, Node.LineCount, True);
        Node := NodeIterator.Next;
        if not Node.IsInFold then
          break;

        NdInfo := NdiHelper1.Next;
        while NdiHelper1.IsValid and (not NdiHelper1.IsAtNodeOpenPos(Node)) do begin
          if not(sfaDefaultCollapsed in NdInfo.FoldAction) then // Currently skip default nodes
            FoldCoderForType(NdInfo.FoldType).AddNode
                                 (NdInfo.LogXStart, NdInfo.LineIndex, 0, False);
          NdInfo := NdiHelper1.Next;
        end;
      end;

      for i := 0 to length(FoldCoders) - 1 do begin
        FoldCoders[i].Finish;
        FoldHelper.AppendMem(FoldCoders[i].Stream.Mem, FoldCoders[i].Stream.Len);
      end;
      FoldHelper.AddChecksum;
      FoldHelper.Compress;
    end             (* *** END: Encode as Text for XML *** *)
    else
    begin           (* *** Encode as Binary *** *)
      while Node.IsInFold and (Node.StartLine-2 <= AEndIndex) do
      begin
        if (node.StartLine > AStartIndex + 2) then
          AStartCol := 0;

        NdInfo2 := NdiHelper1.GotoNodeClosePos(Node);
        if (sfaInvalid in NdInfo2.FoldAction) or
           (NdInfo2.LineIndex > AEndIndex) or
           ((NdInfo2.LineIndex = AEndIndex) and (ndinfo2.LogXEnd > AEndCol))
        then begin
          node := NodeIterator.Next;
          continue;
        end;

        NdInfo := NdiHelper1.GotoNodeOpenPos(Node);

        with entry do begin
          LogX  := NdInfo.LogXStart - AStartCol;
          LogX2 := NdInfo.LogXEnd - ndinfo.LogXStart + (ndinfo.LogXStart - AStartCol);
          Line  := NdInfo.LineIndex - AStartIndex;
          ELogX := NdInfo2.LogXStart;
          ELogX2 := NdInfo2.LogXEnd;
          ELine := NdInfo2.LineIndex - AStartIndex;
          FType := PtrUInt(NdInfo.FoldType);
        end;
        FoldHelper.AppendMem(@entry, SizeOf(TFoldExportEntry));

        Node := NodeIterator.Next;
      end;
    end;            (* *** END: Encode as Binary *** *)

    Result := FoldHelper.Text;
    FCurFoldDescription := Result;
  finally
    FoldHelper.Free;
    for i := 0 to length(FoldCoders) - 1 do
      FoldCoders[i].Free;
    NodeIterator.Free;
    NdiHelper1.Free;
  end;
end;

procedure TSynEditFoldedView.ApplyFoldDescription(AStartIndex, AStartCol, AEndIndex,
  AEndCol: Integer; FoldDesc: PChar; FoldDescLen: Integer; IsText: Boolean = False);
var
  FoldCoders: Array of TSynEditFoldExportCoder;

  function FoldCoderForType(AType: Pointer): TSynEditFoldExportCoder;
  var
    j: Integer;
  begin
    j := length(FoldCoders) - 1;
    while (j >= 0) and (FoldCoders[j] <> nil) and (FoldCoders[j].FoldType <> AType) do
      dec(j);
    if (j < 0) then
      Result := nil
    else
      Result := FoldCoders[j];
  end;

  procedure RemoveCoderForType(AType: Pointer);
  var
    j: Integer;
  begin
    j := length(FoldCoders) - 1;
    while (j >= 0) and (FoldCoders[j] <> nil) and (FoldCoders[j].FoldType <> AType) do
      dec(j);
    if (j >= 0) then begin
      debugln(['FoldState loading removed data for foldtype: ', PtrUInt(AType)]);
      FreeAndNil(FoldCoders[j]);
    end;
  end;


var
  hl: TSynCustomFoldHighlighter;
  FoldHelper: TSynEditFoldExportStream;
  NdiHelper1: TSynFoldNodeInfoHelper;
  NdInfo, ndinfo2: TSynFoldNodeInfo;
  i: Integer;
  Line, FL: Integer;
  entry: TFoldExportEntry;
  Coder: TSynEditFoldExportCoder;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit;
  if (FoldDesc = nil) or (FoldDescLen = 0) then exit;

  NdiHelper1 := TSynFoldNodeInfoHelper.Create(hl);
  FoldHelper := TSynEditFoldExportStream.Create;
  try
    FoldHelper.Mem := FoldDesc;
    FoldHelper.Len := FoldDescLen;

    if IsText then
    begin           (* *** Encode as Text for XML *** *)
      FoldHelper.Decompress;
      if not FoldHelper.VerifyChecksum then
        raise ESynEditError.Create('fold checksum error');

      i := 0;
      while not FoldHelper.EOF do begin
        SetLength(FoldCoders, i + 1);
        FoldCoders[i] := TSynEditFoldExportCoder.Create(FoldHelper);
        if not FoldCoders[i].ReadIsValid then
          break;
        inc(i);
      end;

      NdInfo := NdiHelper1.FirstOpen;

      while NdiHelper1.IsValid do begin
        if (sfaDefaultCollapsed in NdInfo.FoldAction) then begin // Currently skip default nodes
          NdInfo := NdiHelper1.Next;
          continue;
        end;
        Coder := FoldCoderForType(NdInfo.FoldType);
        if coder <> nil then begin
          if coder.ReadNode(NdInfo.LogXStart, NdInfo.LineIndex,
                  LengthForFoldAtTextIndex(NdInfo.LineIndex, NdInfo.NodeIndex))
          then
            FoldAtTextIndex(NdInfo.LineIndex, NdInfo.NodeIndex);
        if (coder<>nil) and (not coder.ReadIsValid) then
          RemoveCoderForType(NdInfo.FoldType);
        end;
        NdInfo := NdiHelper1.Next;
      end;
    end             (* *** END: Encode as Text for XML *** *)
    else
    begin           (* *** Encode as Binary *** *)
      entry.Line := 0;
      if AStartCol > 0 then
        dec(AStartCol);
      while not FoldHelper.EOF do begin
        if not FoldHelper.ReadMem(@entry, sizeof(TFoldExportEntry)) then
          break;
        if entry.Line > 0 then AStartCol := 0;

        Line := AStartIndex + entry.Line;
        if Line >= FLines.Count then
          continue;

        ndinfo :=NdiHelper1.GotoOpenAtChar(Line, entry.LogX);
        Fl := LengthForFoldAtTextIndex(Line, ndinfo.NodeIndex);
        if (sfaInvalid in ndinfo.FoldAction) or
           (ndinfo.LogXStart <> entry.LogX + AStartCol) or
           (ndinfo.LogXEnd <> entry.LogX2 + AStartCol)  or
           //(ndinfo.FoldType <> entry.FType) or
           (entry.ELine - entry.Line <> FL)
        then
          continue;

        ndinfo2 := NdiHelper1.FindClose(FL);
        if (sfaInvalid in ndinfo2.FoldAction) or
           (ndinfo2.LogXStart <> entry.ELogX) or
           (ndinfo2.LogXEnd <> entry.ELogX2)
        then
          continue;

        FoldAtTextIndex(Line, NdInfo.NodeIndex);
      end;
    end;            (* *** END: Encode as Binary *** *)
  finally
    for i := 0 to length(FoldCoders) - 1 do
      FoldCoders[i].Free;
    FreeAndNil(FoldHelper);
    FreeAndNil(NdiHelper1);
  end;
end;

procedure TSynEditFoldedView.FoldAtTextIndex(AStartIndex : Integer;
  ColIndex : Integer = -1; ColCount : Integer = 1; Skip: Boolean = False);
var
  c, top : Integer;
  hl: TSynCustomFoldHighlighter;
  down, NFolded: Boolean;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit;
  FCurFoldDescription := '';
  top := TopTextIndex;
  // AStartIndex is 0-based
  // FoldTree is 1-based AND first line remains visble
  c := hl.FoldOpenCount(AStartIndex);
  if ColCount = 0 then
    ColCount := c;
  down := ColIndex < 0;
  if down then
    ColIndex := c + ColIndex ;
  while ColCount > 0 do begin
    if (ColIndex < 0) or (ColIndex >= c) then break;
    NFolded := IsFoldedAtTextIndex(AStartIndex, ColIndex);
    if skip and NFolded then begin
      if down
      then dec(ColIndex)
      else inc(ColIndex);
      continue;
    end;
    if not NFolded then
      fFoldTree.InsertNewFold(AStartIndex+2, ColIndex,
                              LengthForFoldAtTextIndex(AStartIndex, ColIndex));
    if down
    then dec(ColIndex)
    else inc(ColIndex);
    dec(ColCount);
  end;
  fTopLine := -1;  // make sure seting TopLineTextIndex, will do CalculateMaps;
  TopTextIndex := top;
  if Assigned(fOnFoldChanged) then
    fOnFoldChanged(AStartIndex);
end;

procedure TSynEditFoldedView.UnFoldAtLine(AStartLine : Integer;
  ColIndex : Integer = -1; ColCount : Integer = 0; Skip: Boolean = False);
begin
  UnFoldAtViewPos(AStartLine + fTopLine, ColIndex, ColCount, Skip);
end;

procedure TSynEditFoldedView.UnFoldAtViewPos(AStartPos : Integer;
  ColIndex : Integer = -1; ColCount : Integer = 0; Skip: Boolean = False);
begin
  UnFoldAtTextIndex(AStartPos - 1 + fFoldTree.FindFoldForFoldedLine(AStartPos).FoldedBefore,
                    ColIndex, ColCount, Skip);
end;

procedure TSynEditFoldedView.UnFoldAtTextIndex(AStartIndex : Integer;
  ColIndex : Integer = -1; ColCount : Integer = 0; Skip: Boolean = False);
var
  top, c, r, r2 : Integer;
  hl: TSynCustomFoldHighlighter;
  down: Boolean;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit;
  FCurFoldDescription := '';
  top := TopTextIndex;
  c := hl.FoldOpenCount(AStartIndex);

  r := -1;
  if ColCount = 0 then begin
    r := fFoldTree.RemoveFoldForLine(AStartIndex+1, False);
  end
  else begin
    down := ColIndex < 0;
    if down then
      ColIndex := c + ColIndex ;
    while ColCount > 0 do begin
      if (ColIndex < 0) or (ColIndex >= c) then break;
      if skip and not IsFoldedAtTextIndex(AStartIndex, ColIndex) then begin
        if down
        then dec(ColIndex)
        else inc(ColIndex);
        continue;
      end;
      r2 := fFoldTree.RemoveFoldForLine(AStartIndex+1, False, ColIndex) - 1;
      if (r < 0) or (r2 < r) then r := r2;
      if down
      then dec(ColIndex)
      else inc(ColIndex);
      dec(ColCount);
    end;
  end;

  fTopLine := -1;  // make sure seting TopLineTextIndex, will do CalculateMaps;
  TopTextIndex := top;
  if Assigned(fOnFoldChanged) and (r >= 0) then
    fOnFoldChanged(r);
end;

procedure TSynEditFoldedView.UnFoldAtTextIndexCollapsed(AStartIndex: Integer);
var
  top, r: Integer;
begin
  FCurFoldDescription := '';
  top := TopTextIndex;
  r := fFoldTree.RemoveFoldForLine(AStartIndex+1, True) - 1;
  fTopLine := -1;  // make sure seting TopLineTextIndex, will do CalculateMaps;
  TopTextIndex := top;
  if Assigned(fOnFoldChanged) then
    fOnFoldChanged(r);
end;

function TSynEditFoldedView.IsFoldedAtTextIndex(AStartIndex, ColIndex: Integer): Boolean;
var
  node: TSynTextFoldAVLNode;
  tree: TSynTextFoldAVLTree;
begin
  Result := False;
  node := fFoldTree.FindFoldForLine(AStartIndex + 1, True);

  tree := fFoldTree;
  while (not node.IsInFold) or (node.StartLine <> AStartIndex + 2) do begin
    if (not node.IsInFold) then
      node := tree.FindLastFold;
    while node.IsInFold and (node.StartLine > AStartIndex + 2) do
      node := node.Prev;
    if not node.IsInFold then break;

    if node.IsInFold and (node.StartLine < AStartIndex + 2) then begin
      if node.fData.Nested = nil then break;
      tree := fFoldTree.TreeForNestedNode(node.fData, node.StartLine);
      node := tree.FindFirstFold;
      while node.IsInFold and (node.StartLine < AStartIndex + 2) do
        node := node.Next;
    end
    else
      break;
  end;

  while node.IsInFold and (node.StartLine = AStartIndex + 2) do begin
    if node.FoldIndex = ColIndex then exit(True);
    if node.fData.Nested = nil then break;
    node := fFoldTree.TreeForNestedNode(node.fData, node.StartLine).FindFirstFold;
  end;
end;

procedure TSynEditFoldedView.UnfoldAll;
var
  top : Integer;
begin
  FCurFoldDescription := '';
  top := TopTextIndex;
  fFoldTree.Clear;
  fTopLine := -1;  // make sure seting TopLineTextIndex, will do CalculateMaps;
  TopTextIndex := top;
  if Assigned(fOnFoldChanged) then
    fOnFoldChanged(0);
end;

procedure TSynEditFoldedView.FoldAll(StartLevel : Integer = 0; IgnoreNested : Boolean = False);
var
  c, i, l, top, t: Integer;
  hl: TSynCustomFoldHighlighter;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit;
  FCurFoldDescription := '';

  t := 1; // TODO: Highlighter default type; or iterate through all types
  top := TopTextIndex;
  fFoldTree.Clear;
  i := 0;
  while i < fLines.Count do begin
    if (hl.FoldOpenCount(i, t) > 0)
    and (hl.FoldNestCount(i, t) > StartLevel) then begin
      c := hl.FoldOpenCount(i) -1;
      l := LengthForFoldAtTextIndex(i, c);
      // i is 0-based
      // FoldTree is 1-based AND first line remains visble
      fFoldTree.InsertNewFold(i+2, c, l);
      if IgnoreNested then
        i := i + l;
    end;
    inc(i);
  end;
  fTopLine := -1;
  TopTextIndex := top;
  if Assigned(fOnFoldChanged) then
    fOnFoldChanged(0);
end;

function TSynEditFoldedView.FixFolding(AStart: Integer; AMinEnd: Integer;
  aFoldTree: TSynTextFoldAVLTree): Boolean;
var
  hl: TSynCustomFoldHighlighter;

  function DoFixFolding(doStart: Integer; doMinEnd, AtColumn: Integer;
    doFoldTree: TSynTextFoldAVLTree; node: TSynTextFoldAVLNode) : Boolean;

    Procedure DoRemoveNode(var theNode: TSynTextFoldAVLNode);
    var
      tmpnode: TSynTextFoldAVLNode;
      l: Integer;
    begin
      Result := True;
      tmpnode := theNode.Prev;
      l := theNode.StartLine;
      doFoldTree.RemoveFoldForNodeAtLine(theNode, -1); // Don't touch any nested node
      if tmpnode.IsInFold then theNode := tmpnode.Next
      else theNode := doFoldTree.FindFirstFold;
      if Assigned(fOnFoldChanged) then
        fOnFoldChanged(l);
    end;

  var
    FldLine, FldIndex, FldLen, FldCol: Integer;
    MaxCol, CurLen: Integer;
    PrevFldLine: Integer;
    SubTree: TSynTextFoldAVLTree;
  begin
    Result := False;
    FldLine := doStart;
    while node.IsInFold do begin
      PrevFldLine := FldLine;
      FldLine := node.StartLine - 1; // the 1-based cfCollapsed (last visible) Line
      FldIndex := FldLine - 1;
      FldLen := node.FullCount;
      if (FldLen < 0) then begin
        {$IFDEF SYNFOLDDEBUG}debugln(['>>FOLD-- FixFolding: Remove node with len<0 FldLine=', FldLine]);{$ENDIF}
        DoRemoveNode(node);
        continue;
      end;

      if (FldLine > PrevFldLine) then
        AtColumn := 0;
      // check the fold-length
      MaxCol := hl.FoldOpenCount(FldIndex) - 1;
      FldCol := node.FoldIndex;
      if (FldCol < AtColumn) then FldCol := AtColumn;
      if (FldCol <= MaxCol) and (FldCol >= AtColumn) and
         (FldLen = LengthForFoldAtTextIndex(FldIndex, FldCol)) then begin
        CurLen := FldLen;
      end
      else if (FldCol - 1 <= MaxCol) and (FldCol > AtColumn) and (FldCol > 0) and
         (FldLen = LengthForFoldAtTextIndex(FldIndex, FldCol - 1)) then begin
        CurLen := FldLen;
        dec(FldCol);
      end
      else if (FldCol + 1 <= MaxCol) and (FldCol + 1 >= AtColumn) and
         (FldLen = LengthForFoldAtTextIndex(FldIndex, FldCol + 1)) then begin
        CurLen := FldLen;
        inc(FldCol);
      end
      else
        CurLen := -1;

      if CurLen <> FldLen then begin
        {$IFDEF SYNFOLDDEBUG}debugln(['>>FOLD-- FixFolding: Remove node with len<>len FldLine=', FldLine, ' curlen=',CurLen, ' FldLen=',FldLen]);{$ENDIF}
        DoRemoveNode(node);
        continue;
      end;
      node.fData.FoldIndex := FldCol;
      AtColumn := FldCol + 1;

      if (node.fData.Nested <> nil) then begin
        SubTree := doFoldTree.TreeForNestedNode(node.fData, FldLine+1);
        CurLen := node.LineCount;
        if DoFixFolding(FldLine, FldLine + CurLen + 1, AtColumn, SubTree, SubTree.FindFirstFold)
        then begin
          if CurLen > FldLen then begin
            node.fData.LineCount := max(node.FullCount,
              doFoldTree.TreeForNestedNode(node.fData, 0).LastFoldedLine + 1);
            if CurLen <> node.LineCount then
              node.fData.AdjustParentLeftCount(node.LineCount - CurLen);
          end;
          continue;
        end;
      end;

      // the node was ok
      if node.StartLine >= doMinEnd then break;
      node := node.Next;
    end;
  end;

var
  node, tmpnode: TSynTextFoldAVLNode;
begin
  {$IFDEF SYNFOLDDEBUG}debugln(['>>FOLD-- FixFolding: Start=', AStart, '  AMinEnd=',AMinEnd]);{$ENDIF}
  FCurFoldDescription := '';
  Result := false;
  if fLockCount > 0 then begin
    fNeedCaretCheck := true; // We may be here as a result of lines deleted/inserted
    if fNeedFixFrom < 0 then fNeedFixFrom := AStart
    else fNeedFixFrom := Min(fNeedFixFrom, AStart);
    fNeedFixMinEnd := Max(fNeedFixMinEnd, AMinEnd);
    exit;
  end;

  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then begin
    CalculateMaps;
    exit;
  end;

  node := aFoldTree.FindFoldForLine(aStart, true);
  if not node.IsInFold then node:= aFoldTree.FindLastFold;
  if not node.IsInFold then begin
    CalculateMaps;
    exit;
  end;
  If aMinEnd < node.StartLine then aMinEnd := node.StartLine;

  // FullCount is allowed to be -1
  while node.IsInFold and (node.StartLine + node.FullCount + 1 >= aStart) do begin
    tmpnode := node.Prev;
    if tmpnode.IsInFold
    then node := tmpnode
    else break; // first node
  end;

  Result := DoFixFolding(-1, AMinEnd, 0, aFoldTree, node);
  CalculateMaps;
  {$IFDEF SYNFOLDDEBUG}debugln(['<<FOLD-- FixFolding: DONE=', Result]);{$ENDIF}
end;

procedure TSynEditFoldedView.DoCaretChanged(Sender : TObject);
var
  i: Integer;
begin
  if fLockCount > 0 then begin
    fNeedCaretCheck := true;
    exit;
  end;
  i := TSynEditCaret(Sender).LinePos-1;
  {$IFDEF SYNFOLDDEBUG}if FoldedAtTextIndex[i] then debugln(['FOLD-- DoCaretChanged  about to unfold at Index=', i]);{$ENDIF}
  if FoldedAtTextIndex[i] then
    UnFoldAtTextIndexCollapsed(i);
end;

procedure TSynEditFoldedView.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
begin
  FCurFoldDescription := '';
  {$IFDEF SYNFOLDDEBUG}debugln(['FOLD-- LineCountChanged AIndex=', AIndex, '  Acount=',ACount]);{$ENDIF}
  // no need for fix folding => synedit will be called, and scanlines will call fixfolding
  {TODO: a "need fix folding" flag => to ensure it will be called if synedit doesnt}
  if (fLockCount > 0) and (AIndex < max(fNeedFixFrom, fNeedFixMinEnd)) then begin
    // adapt the fixfold range. Could be done smarter, but it doesn't matter if the range gets bigger than needed.
    if (ACount < 0) and (AIndex < fNeedFixFrom) then inc(fNeedFixFrom, ACount);
    if (ACount > 0) and (AIndex < fNeedFixMinEnd) then inc(fNeedFixMinEnd, ACount);
  end;
  if ACount<0
  then LinesDeletedAtTextIndex(AIndex, -ACount, true)
  else LinesInsertedAtTextIndex(AIndex, ACount, true);
end;

procedure TSynEditFoldedView.FixFoldingAtTextIndex(AStartIndex: Integer; AMinEndLine : Integer);
begin
  FixFolding(AStartIndex + 1, AMinEndLine, fFoldTree);
end;

function TSynEditFoldedView.OpenFoldCount(aStartIndex: Integer): Integer;
var
  hl: TSynCustomFoldHighlighter;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit(-1);
  Result := hl.FoldNestCount(AStartIndex-1) + hl.FoldOpenCount(AStartIndex);
end;

function TSynEditFoldedView.OpenFoldInfo(aStartIndex, ColIndex: Integer): TFoldViewNodeInfo;
var
  hl: TSynCustomFoldHighlighter;
  TypeCnt, Lvl: Integer;
  EndLvl, CurLvl: Array of integer;
  i, t, n, o: Integer;
  nd: TSynFoldNodeInfo;
  procedure GetEndLvl(l: Integer);
  var i: integer;
  begin
    for i := 1 to TypeCnt do begin
      EndLvl[i] := hl.FoldNestCount(l-1, i);
      EndLvl[i] := EndLvl[i] + hl.FoldOpenCount(l, i);
      CurLvl[i] := EndLvl[i];
    end
  end;
begin
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit;

  TypeCnt := hl.FoldTypeCount;
  Lvl := hl.FoldNestCount(AStartIndex-1);
  i := 0;
  if ColIndex >= Lvl then begin
    // search current line
    Lvl := Lvl + hl.FoldOpenCount(aStartIndex);
    i := 1;
  end;
  SetLength(EndLvl, TypeCnt+1);
  SetLength(CurLvl, TypeCnt+1);
  GetEndLvl(aStartIndex);
  GetEndLvl(aStartIndex);
  aStartIndex := aStartIndex + i;
  while (ColIndex < Lvl) and (aStartIndex > 0) do begin
    dec(aStartIndex);
    if (hl.FoldOpenCount(aStartIndex) > 0) or
       (hl.FoldCloseCount(aStartIndex) > 0) then begin
      o := hl.FoldOpenCount(AStartIndex);
      n := o;
      for i := hl.FoldNodeInfoCount[aStartIndex, []] - 1 downto 0 do begin
        nd := hl.FoldNodeInfo[aStartIndex, i, []];
        if not(sfaFold in nd.FoldAction) then
          continue;
        t := nd.FoldGroup;
        if sfaOpen in nd.FoldAction then begin
          dec(n);
          dec(CurLvl[t]);
          if CurLvl[t] < EndLvl[t] then begin
            dec(EndLvl[t]);
            dec(Lvl);
            if ColIndex = Lvl then begin
              break;
            end;
          end;
        end else
        if sfaClose in nd.FoldAction then begin
          inc(CurLvl[t]);
        end;
      end;
    end
    else
    if hl.FoldNestCount(AStartIndex-1) = 0 then break;
  end;
  Result.HNode := nd;
  Result.OpenCount := o;
  Result.Text := fLines[aStartIndex];
  Result.Keyword := copy(Result.Text, 1 + nd.LogXStart, nd.LogXEnd-nd.LogXStart);
  Result.LineNum := aStartIndex + 1;
  Result.ColIndex := n;
  Result.Folded := IsFoldedAtTextIndex(aStartIndex, n);
end;

function TSynEditFoldedView.ExpandedLineForBlockAtLine(ALine : Integer;
  HalfExpanded: Boolean = True) : Integer;
var
  i, l : Integer;
  node: TSynTextFoldAVLNode;
  hl: TSynCustomFoldHighlighter;
begin
  Result := -1;
  hl := TSynCustomFoldHighlighter(HighLighter);
  if not assigned(hl) then
    exit;

  i := ALine;
  l := hl.FoldOpenCount(i - 1);
  if l > 0 then begin
    node := fFoldTree.FindFoldForLine(ALine, true);
    if node.IsInFold and (node.StartLine = ALine +1) then begin
      dec(l);
      if HalfExpanded then while (l >= 0) do begin
        if not IsFoldedAtTextIndex(ALine-1, l) then exit(ALine);
        dec(l);
      end;
      dec(i);
    end
    else
      exit(ALine);
  end
  else if hl.FoldCloseCount(i - 1) > 0 then
    dec(i);
  if (i < 0) or (hl.FoldNestCount(i-1) = 0) then
    exit;

  l := 0;
  while (i > 0) and (l >= 0) do begin // (FoldMinLevel[i] >= l) do
    dec(i);
    l := l - hl.FoldOpenCount(i);
    if l >= 0 then
      l := l + hl.FoldCloseCount(i);
  end;
  if (hl.FoldNestCount(i) > 0) then // TODO, check for collapsed at index = 0
    Result := i + 1;
end;

function TSynEditFoldedView.GetPhysicalCharWidths(Index: Integer): TPhysicalCharWidths;
begin
  Result := fLines.GetPhysicalCharWidths(ScreenLineToTextIndex(Index));
end;

function TSynEditFoldedView.CollapsedLineForFoldAtLine(ALine : Integer) : Integer;
var
  node: TSynTextFoldAVLNode;
begin
  Result := -1;
  node := fFoldTree.FindFoldForLine(ALine, false);
  if node.IsInFold then Result := node.StartLine-1;
end;

procedure TSynEditFoldedView.debug;
begin
  fFoldTree.debug;
end;

initialization
  InitNumEncodeValues;

end.


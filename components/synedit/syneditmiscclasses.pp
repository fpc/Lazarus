{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscClasses.pas, released 2000-04-07.
The Original Code is based on the mwSupportClasses.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditMiscClasses;

{$I synedit.inc}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazMethodList,
  // LCL
  LCLIntf, LCLType, LCLProc, Graphics, Controls, Clipbrd, ImgList,
  // SynEdit
  SynEditHighlighter, SynEditMiscProcs, SynEditTypes, LazSynEditText, SynEditPointClasses;

type

  { TSynWordBreaker }

  TSynWordBreaker = class
  private
    FIdentChars: TSynIdentChars;
    FWhiteChars: TSynIdentChars;
    FWordBreakChars: TSynIdentChars;
    FWordChars: TSynIdentChars;
    procedure SetIdentChars(const AValue: TSynIdentChars);
    procedure SetWhiteChars(const AValue: TSynIdentChars);
    procedure SetWordBreakChars(const AValue: TSynIdentChars);
  public
    constructor Create;
    procedure Reset;

    // aX is the position between the chars (as in CaretX)
    // 1 is in front of the first char
    function IsInWord     (aLine: String; aX: Integer
                           ): Boolean;  // Includes at word boundary
    function IsAtWordStart(aLine: String; aX: Integer): Boolean;
    function IsAtWordEnd  (aLine: String; aX: Integer): Boolean;
    function NextWordStart(aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;
    function NextWordEnd  (aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;
    function PrevWordStart(aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;
    function PrevWordEnd  (aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;

    function NextBoundary (aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;
    function PrevBoundary (aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;

    property IdentChars: TSynIdentChars read FIdentChars write SetIdentChars;
    property WordChars: TSynIdentChars read FWordChars;
    property WordBreakChars: TSynIdentChars read FWordBreakChars write SetWordBreakChars;
    property WhiteChars: TSynIdentChars read FWhiteChars write SetWhiteChars;
  end;

  TLazSynSurface = class;

  { TSynEditBase }

  TSynEditBase = class(TCustomControl)
  protected
    FWordBreaker: TSynWordBreaker;
    FBlockSelection: TSynEditSelection;
    FScreenCaret: TSynEditScreenCaret;
    function GetMarkupMgr: TObject; virtual; abstract;
    function GetLines: TStrings; virtual; abstract;
    function GetCaretObj: TSynEditCaret; virtual; abstract;
    procedure SetLines(Value: TStrings); virtual; abstract;
    function GetViewedTextBuffer: TSynEditStrings; virtual; abstract;
    function GetFoldedTextBuffer: TObject; virtual; abstract;
    function GetTextBuffer: TSynEditStrings; virtual; abstract;
    function GetPaintArea: TLazSynSurface; virtual; abstract; // TLazSynSurfaceManager

    property MarkupMgr: TObject read GetMarkupMgr;
    property FoldedTextBuffer: TObject read GetFoldedTextBuffer;                // TSynEditFoldedView
    property ViewedTextBuffer: TSynEditStrings read GetViewedTextBuffer;        // As viewed internally (with uncommited spaces / TODO: expanded tabs, folds). This may change, use with care
    property TextBuffer: TSynEditStrings read GetTextBuffer;                    // (TSynEditStringList) No uncommited (trailing/trimmable) spaces
    property WordBreaker: TSynWordBreaker read FWordBreaker;
  public
    property Lines: TStrings read GetLines write SetLines;
  end;

  { TSynEditFriend }
  // TODO: Redesign

  TSynEditFriend = class(TComponent)
  private
    FFriendEdit: TSynEditBase;
    function GetCaretObj: TSynEditCaret;
    function GetFoldedTextBuffer: TObject;
    function GetIsRedoing: Boolean;
    function GetIsUndoing: Boolean;
    function GetMarkupMgr: TObject;
    function GetPaintArea: TLazSynSurface; // TLazSynSurfaceManager
    function GetScreenCaret: TSynEditScreenCaret;
    function GetSelectionObj: TSynEditSelection;
    function GetTextBuffer: TSynEditStrings;
    function GetViewedTextBuffer: TSynEditStrings;
    function GetWordBreaker: TSynWordBreaker;
  protected
    property FriendEdit: TSynEditBase read FFriendEdit write FFriendEdit;
    property FoldedTextBuffer: TObject read GetFoldedTextBuffer;                // TSynEditFoldedView
    property ViewedTextBuffer: TSynEditStrings read GetViewedTextBuffer;        // As viewed internally (with uncommited spaces / TODO: expanded tabs, folds). This may change, use with care
    property TextBuffer: TSynEditStrings read GetTextBuffer;                    // (TSynEditStringList)
    property CaretObj: TSynEditCaret read GetCaretObj;
    property ScreenCaret: TSynEditScreenCaret read GetScreenCaret; // TODO: should not be exposed
    property SelectionObj: TSynEditSelection read GetSelectionObj;
    property PaintArea: TLazSynSurface read GetPaintArea; // TLazSynSurfaceManager
    property MarkupMgr: TObject read GetMarkupMgr;
    property IsUndoing: Boolean read GetIsUndoing;
    property IsRedoing: Boolean read GetIsRedoing;
    property WordBreaker: TSynWordBreaker read GetWordBreaker;
  end;


  TSynObjectListItem = class;

  { TSynObjectList }

  TSynObjectList = class(TComponent)
  private
    FList: TList;
    FOnChange: TNotifyEvent;
    FOwner: TComponent;
    FSorted: Boolean;
    function GetBasePart(Index: Integer): TSynObjectListItem;
    procedure PutBasePart(Index: Integer; const AValue: TSynObjectListItem);
    procedure SetSorted(const AValue: Boolean);
  protected
    function GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure RegisterItem(AnItem: TSynObjectListItem); virtual;
    procedure DoChange(Sender: TObject); virtual;
    property List: TList read FList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    Function  Add(AnItem: TSynObjectListItem): Integer;
    Procedure Delete(Index: Integer);
    Procedure Clear;
    Function  Count: Integer;
    Function  IndexOf(AnItem: TSynObjectListItem): Integer;
    Procedure Move(AOld, ANew: Integer);
    procedure Sort;
    property Sorted: Boolean read FSorted write SetSorted;
    property Owner: TComponent read FOwner;
    property BaseItems[Index: Integer]: TSynObjectListItem
      read GetBasePart write PutBasePart; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TSynObjectListItem }

  TSynObjectListItem = class(TSynEditFriend)
  private
    FOwner: TSynObjectList;
    function GetIndex: Integer;
    procedure SetIndex(const AValue: Integer);
  protected
    function Compare(Other: TSynObjectListItem): Integer; virtual;
    function GetDisplayName: String; virtual;
    property Owner: TSynObjectList read FOwner;
    // Use Init to setup things that are needed before Owner.RegisterItem (bur require Owner to be set)
    procedure Init; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Index: Integer read GetIndex write SetIndex;
    property DisplayName: String read GetDisplayName;
    function GetParentComponent: TComponent; override; // for child order in stream reading
  end;

  TSynObjectListItemClass = class of TSynObjectListItem;

  TLazSynDisplayTokenBound = record
    Physical: Integer;      // 1 based - May be in middle of char
    Logical: Integer;       // 1 based
    Offset: Integer;        // default 0. MultiWidth (e.g. Tab), if token starts in the middle of char
  end;

  { TSynSelectedColor }

  TSynSelectedColor = class(TSynHighlighterAttributesModifier)
  private
    // 0 or -1 start/end before/after line // 1 first char
    FStartX, FEndX: TLazSynDisplayTokenBound;
  protected
    procedure DoClear; override;
    procedure AssignFrom(Src: TLazSynCustomTextAttributes); override;
    procedure Init; override;
  public
    // boundaries of the frame
    procedure SetFrameBoundsPhys(AStart, AEnd: Integer);
    procedure SetFrameBoundsLog(AStart, AEnd: Integer; AStartOffs: Integer = 0; AEndOffs: Integer = 0);
    property StartX: TLazSynDisplayTokenBound read FStartX write FStartX;
    property EndX: TLazSynDisplayTokenBound read FEndX write FEndX;
  public
    function IsEnabled: boolean;
    function GetModifiedStyle(aStyle: TFontStyles): TFontStyles; // deprecated;
    procedure ModifyColors(var AForeground, ABackground, AFrameColor: TColor;
      var AStyle: TFontStyles; var AFrameStyle: TSynLineStyle); deprecated;
  end;

  TSynSelectedColorAlphaEntry = record
    Color: TColor;
    Alpha: Integer;
    Priority: Integer
  end;

  TSynSelectedColorMergeInfo = record
    BaseColor: TColor;
    BasePriority: Integer;
    AlphaCount: Integer;
    AlphaStack: Array of TSynSelectedColorAlphaEntry;
  end;

  TSynSelectedColorEnum = (
    sscBack, sscFore, sscFrameLeft, sscFrameRight, sscFrameTop, sscFrameBottom
  );

  { TSynSelectedColorMergeResult }

  TSynSelectedColorMergeResult = class(TSynSelectedColor)
  private
    // TSynSelectedColor.Style and StyleMask describe how to modify a style,
    // but PaintLines creates an instance that contains an actual style (without mask)
    MergeFinalStyle: Boolean; // always true
    FMergeInfoInitialized: Boolean;

    FCurrentEndX: TLazSynDisplayTokenBound;
    FCurrentStartX: TLazSynDisplayTokenBound;
    FFrameSidesInitialized: Boolean;
    FFrameSideColors: array[TLazSynBorderSide] of TColor;
    FFrameSideStyles: array[TLazSynBorderSide] of TSynLineStyle;
    FFrameSidePriority: array[TLazSynBorderSide] of Integer;
    FFrameSideOrigin: array[TLazSynBorderSide] of TSynFrameEdges;

    FMergeInfos: array [TSynSelectedColorEnum] of TSynSelectedColorMergeInfo;

    function IsMatching(ABound1, ABound2: TLazSynDisplayTokenBound): Boolean;
    function GetFrameSideColors(Side: TLazSynBorderSide): TColor;
    function GetFrameSideOrigin(Side: TLazSynBorderSide): TSynFrameEdges;
    function GetFrameSidePriority(Side: TLazSynBorderSide): integer;
    function GetFrameSideStyles(Side: TLazSynBorderSide): TSynLineStyle;
    procedure SetCurrentEndX(AValue: TLazSynDisplayTokenBound);
    procedure SetCurrentStartX(AValue: TLazSynDisplayTokenBound);
  protected
    procedure AssignFrom(Src: TLazSynCustomTextAttributes); override;
    procedure DoClear; override;
    procedure Init; override;

    procedure MaybeInitFrameSides;
    procedure MergeToInfo(var AnInfo: TSynSelectedColorMergeInfo;
      AColor: TColor; APriority, AnAlpha: Integer);
    function  CalculateInfo(var AnInfo: TSynSelectedColorMergeInfo;
              ANoneColor: TColor; IsFrame: Boolean = False): TColor;
    property FrameSidePriority[Side: TLazSynBorderSide]: integer read GetFrameSidePriority;
    property FrameSideOrigin[Side: TLazSynBorderSide]: TSynFrameEdges read GetFrameSideOrigin;
  public
    destructor Destroy; override;

    property FrameSideColors[Side: TLazSynBorderSide]: TColor read GetFrameSideColors;
    property FrameSideStyles[Side: TLazSynBorderSide]: TSynLineStyle read GetFrameSideStyles;
    // boundaries for current paint
    property CurrentStartX: TLazSynDisplayTokenBound read FCurrentStartX write SetCurrentStartX;
    property CurrentEndX: TLazSynDisplayTokenBound read FCurrentEndX write SetCurrentEndX;
  public
    procedure InitMergeInfo;    // (called automatically) Set all MergeInfo to the start values. After this was called, ay Changes to the color properties are ignored
    procedure ProcessMergeInfo; // copy the merge result, to the actual color properties
    procedure CleanupMergeInfo; // free the alpha arrays
    procedure Merge(Other: TSynHighlighterAttributesModifier);
    procedure Merge(Other: TSynHighlighterAttributesModifier; LeftCol, RightCol: TLazSynDisplayTokenBound);
    procedure MergeFrames(Other: TSynHighlighterAttributesModifier; LeftCol, RightCol: TLazSynDisplayTokenBound);
  end;

  { TLazSynSurface }

  TLazSynSurface = class
  private
    FBounds: TRect;
    FBoundsChangeList: TMethodList;
    FDisplayView: TLazSynDisplayView;
    FOwner: TWinControl;
    function GetHandle: HWND;
    procedure SetDisplayView(AValue: TLazSynDisplayView);
  protected
    procedure BoundsChanged; virtual;
    procedure DoPaint(ACanvas: TCanvas; AClip: TRect); virtual; abstract;
    procedure DoDisplayViewChanged; virtual;
    property  Handle: HWND read GetHandle;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Src: TLazSynSurface); virtual;
    procedure AddBoundsChangeHandler(AHandler: TNotifyEvent);
    procedure RemoveBoundsChangeHandler(AHandler: TNotifyEvent);

    procedure Paint(ACanvas: TCanvas; AClip: TRect);
    procedure InvalidateLines(FirstTextLine, LastTextLine: TLineIdx); virtual;
    procedure SetBounds(ATop, ALeft, ABottom, ARight: Integer);

    property Left: Integer   read FBounds.Left;
    property Top: Integer    read FBounds.Top;
    property Right:Integer   read FBounds.Right;
    property Bottom: integer read FBounds.Bottom;
    property Bounds: TRect read FBounds;

    property DisplayView:   TLazSynDisplayView    read FDisplayView   write SetDisplayView;
  end;

  { TSynBookMarkOpt }

  TSynBookMarkOpt = class(TPersistent)
  private
    fBookmarkImages: TCustomImageList;
    fDrawBookmarksFirst: boolean;                                               //mh 2000-10-12
    fEnableKeys: Boolean;
    fGlyphsVisible: Boolean;
    fLeftMargin: Integer;
    fOwner: TComponent;
    fXoffset: integer;
    fOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TCustomImageList);
    procedure SetDrawBookmarksFirst(Value: boolean);                            //mh 2000-10-12
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: integer);
  public
    constructor Create(AOwner: TComponent);
  published
    property BookmarkImages: TCustomImageList
      read fBookmarkImages write SetBookmarkImages;
    property DrawBookmarksFirst: boolean read fDrawBookmarksFirst               //mh 2000-10-12
      write SetDrawBookmarksFirst default True;
    property EnableKeys: Boolean
      read fEnableKeys write fEnableKeys default True;
    property GlyphsVisible: Boolean
      read fGlyphsVisible write SetGlyphsVisible default True;
    property LeftMargin: Integer read fLeftMargin write SetLeftMargin default 2;
    property Xoffset: integer read fXoffset write SetXOffset default 12;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TSynInternalImage }

  TSynInternalImage = class(TObject)
  public
    constructor Create(const AName: string; Count: integer);
    destructor Destroy; override;
    procedure DrawMark(ACanvas: TCanvas; Number, X, Y, LineHeight: integer);
  end;


  { TSynEditSearchCustom }

  TSynEditSearchCustom = class(TComponent)
  protected
    function GetPattern: string; virtual; abstract;
    procedure SetPattern(const Value: string); virtual; abstract;
    function GetLength(aIndex: integer): integer; virtual; abstract;
    function GetResult(aIndex: integer): integer; virtual; abstract;
    function GetResultCount: integer; virtual; abstract;
    procedure SetOptions(const Value: TSynSearchOptions); virtual; abstract;
  public
    function FindAll(const NewText: string): integer; virtual; abstract;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: integer read GetResultCount;
    property Results[aIndex: integer]: integer read GetResult;
    property Lengths[aIndex: integer]: integer read GetLength;
    property Options: TSynSearchOptions write SetOptions;
  end;

  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  TSynClipboardStreamTag = type integer;
  {$ELSE }
  TSynClipboardStreamTag = type word;
  {$ENDIF}

  { TSynClipboardStream }

  TSynClipboardStream = class
  private
    FMemStream: TMemoryStream;
    FText: String;
    FTextP: PChar;
    FIsPlainText: Boolean;

    function GetMemory: Pointer;
    function GetSize: LongInt;
    function GetSelectionMode: TSynSelectionMode;
    procedure SetSelectionMode(const AValue: TSynSelectionMode);
    procedure SetInternalText(const AValue: String);
    procedure SetText(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    class function ClipboardFormatId: TClipboardFormat;

    function CanReadFromClipboard(AClipboard: TClipboard): Boolean;
    function ReadFromClipboard(AClipboard: TClipboard): Boolean;
    function WriteToClipboard(AClipboard: TClipboard): Boolean;

    procedure Clear;

    function HasTag(ATag: TSynClipboardStreamTag): Boolean;
    function GetTagPointer(ATag: TSynClipboardStreamTag): Pointer;
    function GetTagLen(ATag: TSynClipboardStreamTag): Integer;
    // No check for duplicates
    Procedure AddTag(ATag: TSynClipboardStreamTag; Location: Pointer; Len: Integer);
    property IsPlainText: Boolean read FIsPlainText;

    // Currently Each method (or each method of a pair) must be assigned only ONCE
    property TextP: PChar read FTextP;
    property Text: String write SetText;
    property InternalText: String write SetInternalText;

    property SelectionMode: TSynSelectionMode read GetSelectionMode write SetSelectionMode;

    property Memory: Pointer read GetMemory;
    property Size: LongInt read GetSize;
  end;

  { TSynMethodList }

  TSynMethodList = Class(TMethodList)
  private
    function IndexToObjectIndex(const AnObject: TObject; AnIndex: Integer): integer;
    function GetObjectItems(AnObject: TObject; Index: integer): TMethod;
    procedure SetObjectItems(AnObject: TObject; Index: integer; const AValue: TMethod);
  public
    function CountByObject(const AnObject: TObject): integer;
    procedure DeleteByObject(const AnObject: TObject; Index: integer);
    procedure AddCopyFrom(AList: TSynMethodList; AOwner: TObject = nil);
  public
    property ItemsByObject[AnObject: TObject; Index: integer]: TMethod
      read GetObjectItems write SetObjectItems; default;
  end;

  TSynFilteredMethodListEntry = record
    FHandler: TMethod;
    FFilter: LongInt;
  end;

  { TSynFilteredMethodList }

  TSynFilteredMethodList = Class
  private
    FCount: Integer;
  protected
    FItems: Array of TSynFilteredMethodListEntry;
    function IndexOf(AHandler: TMethod): Integer;
    function IndexOf(AHandler: TMethod; AFilter: LongInt): Integer;
    function NextDownIndex(var Index: integer): boolean;
    function NextDownIndexNumFilter(var Index: integer; AFilter: LongInt): boolean;
    function NextDownIndexBitFilter(var Index: integer; AFilter: LongInt): boolean;
    procedure Delete(AIndex: Integer);
  public
    constructor Create;
    procedure AddNumFilter(AHandler: TMethod; AFilter: LongInt);                         // Separate entries for same method with diff filter
    procedure AddBitFilter(AHandler: TMethod; AFilter: LongInt);                    // Filter is bitmask
    procedure Remove(AHandler: TMethod);
    procedure Remove(AHandler: TMethod; AFilter: LongInt);
    procedure CallNotifyEventsNumFilter(Sender: TObject; AFilter: LongInt);
    procedure CallNotifyEventsBitFilter(Sender: TObject; AFilter: LongInt);         // filter is Bitmask
    property Count: Integer read FCount;
  end;

const
  synClipTagText = TSynClipboardStreamTag(1);
  synClipTagExtText = TSynClipboardStreamTag(2);
  synClipTagMode = TSynClipboardStreamTag(3);
  synClipTagFold = TSynClipboardStreamTag(4);


type

  TReplacedChildSite = (rplcLeft, rplcRight);

  { TSynSizedDifferentialAVLNode }

  TSynSizedDifferentialAVLNode = Class
  protected
    (* AVL Tree structure *)
    FParent, FLeft, FRight : TSynSizedDifferentialAVLNode;    (* AVL Links *)
    FBalance : shortint;                                    (* AVL Balance *)

    (* Position:  stores difference to parent value
    *)
    FPositionOffset: Integer;

    (* Size:  Each node can have a Size, or similar value.
              LeftSizeSum is the Sum of all sizes on the Left. This allows one to quickly
              calculate the sum of all preceding nodes together
    *)
    FSize: Integer;
    FLeftSizeSum: Integer;

    property LeftSizeSum: Integer read FLeftSizeSum;
    {$IFDEF SynDebug}
    function Debug: String; virtual;
    {$ENDIF}
  public
    function TreeDepth: integer;           (* longest WAY down. Only one node => 1! *)

    procedure SetLeftChild(ANode : TSynSizedDifferentialAVLNode); overload; inline;
    procedure SetLeftChild(ANode : TSynSizedDifferentialAVLNode;
                           anAdjustChildPosOffset : Integer); overload; inline;
    procedure SetLeftChild(ANode : TSynSizedDifferentialAVLNode;
                           anAdjustChildPosOffset,
                           aLeftSizeSum : Integer); overload; inline;

    procedure SetRightChild(ANode : TSynSizedDifferentialAVLNode); overload; inline;
    procedure SetRightChild(ANode : TSynSizedDifferentialAVLNode;
                            anAdjustChildPosOffset : Integer); overload; inline;

    function ReplaceChild(OldNode, ANode : TSynSizedDifferentialAVLNode) : TReplacedChildSite; overload; inline;
    function ReplaceChild(OldNode, ANode : TSynSizedDifferentialAVLNode;
                          anAdjustChildPosOffset : Integer) : TReplacedChildSite; overload; inline;

    procedure AdjustLeftCount(AValue : Integer);
    procedure AdjustParentLeftCount(AValue : Integer);

    function Precessor: TSynSizedDifferentialAVLNode;
    function Successor: TSynSizedDifferentialAVLNode;
    function Precessor(var aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;
    function Successor(var aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;

    function GetSizesBeforeSum: Integer;
    function GetPosition: Integer;
  end;

  TSynSizedDiffAVLFindMode = (afmNil, afmCreate, afmPrev, afmNext);

  { TSynSizedDifferentialAVLTree }

  TSynSizedDifferentialAVLTree = class
  protected
    FRoot: TSynSizedDifferentialAVLNode;
    FRootOffset : Integer; // Always 0, unless subclassed with nested trees

    // SetRoot, does not obbey fRootOffset => use SetRoot(node, -fRootOffset)
    procedure SetRoot(ANode : TSynSizedDifferentialAVLNode); virtual; overload;
    procedure SetRoot(ANode : TSynSizedDifferentialAVLNode; anAdjustChildPosOffset : Integer); virtual; overload;

    procedure DisposeNode(var ANode: TSynSizedDifferentialAVLNode); virtual;

    function  InsertNode(ANode : TSynSizedDifferentialAVLNode) : Integer; // returns FoldedBefore // ANode may not have children
    procedure RemoveNode(ANode: TSynSizedDifferentialAVLNode); // Does not Free
    procedure BalanceAfterInsert(ANode: TSynSizedDifferentialAVLNode);
    procedure BalanceAfterDelete(ANode: TSynSizedDifferentialAVLNode);

    function CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode; virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    {$IFDEF SynDebug}
    procedure   Debug;
    {$ENDIF}

    procedure Clear; virtual;
    function First: TSynSizedDifferentialAVLNode;
    function Last: TSynSizedDifferentialAVLNode;
    function First(out aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;
    function Last(out aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;

    function FindNodeAtLeftSize(ALeftSum: INteger;
                                out aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;
    function FindNodeAtPosition(APosition: INteger; AMode: TSynSizedDiffAVLFindMode;
                                out aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;
    procedure AdjustForLinesInserted(AStartLine, ALineCount : Integer);
    procedure AdjustForLinesDeleted(AStartLine, ALineCount : Integer);
  end;


implementation

{ TSynEditFriend }

function TSynEditFriend.GetViewedTextBuffer: TSynEditStrings;
begin
  Result := FFriendEdit.ViewedTextBuffer;
end;

function TSynEditFriend.GetWordBreaker: TSynWordBreaker;
begin
  Result := FFriendEdit.WordBreaker;
end;

function TSynEditFriend.GetMarkupMgr: TObject;
begin
  Result := FFriendEdit.MarkupMgr;
end;

function TSynEditFriend.GetPaintArea: TLazSynSurface;
begin
  Result := FFriendEdit.GetPaintArea;
end;

function TSynEditFriend.GetScreenCaret: TSynEditScreenCaret;
begin
  Result := FFriendEdit.FScreenCaret;
end;

function TSynEditFriend.GetSelectionObj: TSynEditSelection;
begin
  Result := FFriendEdit.FBlockSelection;
end;

function TSynEditFriend.GetTextBuffer: TSynEditStrings;
begin
  Result := FFriendEdit.TextBuffer;
end;

function TSynEditFriend.GetIsRedoing: Boolean;
begin
  Result := FFriendEdit.ViewedTextBuffer.IsRedoing;
end;

function TSynEditFriend.GetCaretObj: TSynEditCaret;
begin
  Result := FFriendEdit.GetCaretObj;
end;

function TSynEditFriend.GetFoldedTextBuffer: TObject;
begin
  Result := FFriendEdit.FoldedTextBuffer;
end;

function TSynEditFriend.GetIsUndoing: Boolean;
begin
  Result := FFriendEdit.ViewedTextBuffer.IsUndoing;
end;

{ TSynSelectedColorMergeResult }

function TSynSelectedColorMergeResult.IsMatching(ABound1,
  ABound2: TLazSynDisplayTokenBound): Boolean;
begin
  Result := ( (ABound1.Physical > 0) and
              (ABound1.Physical = ABound2.Physical)
            ) or
            ( (ABound1.Logical > 0) and
              (ABound1.Logical = ABound2.Logical) and (ABound1.Offset = ABound2.Offset)
            );
end;

function TSynSelectedColorMergeResult.GetFrameSideColors(Side: TLazSynBorderSide): TColor;
begin
  if FFrameSidesInitialized then begin
    Result := FFrameSideColors[Side];
    exit
  end;

  if (FCurrentStartX.Logical >= 0) or (FCurrentStartX.Physical >= 0) then
    case Side of
      bsLeft:  if not IsMatching(FCurrentStartX, FStartX) then exit(clNone);
      bsRight: if not IsMatching(FCurrentEndX,   FEndX)   then exit(clNone);
    end;

  if (Side in SynFrameEdgeToSides[FrameEdges])
  then Result := FrameColor
  else Result := clNone;
end;

function TSynSelectedColorMergeResult.GetFrameSideOrigin(Side: TLazSynBorderSide): TSynFrameEdges;
begin
  if FFrameSidesInitialized
  then Result := FFrameSideOrigin[Side]
  else if FrameColor = clNone
  then Result := sfeNone
  else Result := FrameEdges;
end;

function TSynSelectedColorMergeResult.GetFrameSidePriority(Side: TLazSynBorderSide): integer;
begin
  if FFrameSidesInitialized then begin
    Result := FFrameSidePriority[Side];
    exit
  end;

  if (FCurrentStartX.Logical >= 0) or (FCurrentStartX.Physical >= 0) then
    case Side of
      bsLeft:  if not IsMatching(FCurrentStartX, FStartX) then exit(0);
      bsRight: if not IsMatching(FCurrentEndX,   FEndX)   then exit(0);
    end;

  if (Side in SynFrameEdgeToSides[FrameEdges])
  then Result := FramePriority
  else Result := 0;
end;

function TSynSelectedColorMergeResult.GetFrameSideStyles(Side: TLazSynBorderSide): TSynLineStyle;
begin
  if FFrameSidesInitialized
  then Result := FFrameSideStyles[Side]
  else
  if Side in SynFrameEdgeToSides[FrameEdges]
  then Result := FrameStyle
  else Result := slsSolid;
end;

procedure TSynSelectedColorMergeResult.SetCurrentEndX(AValue: TLazSynDisplayTokenBound);
begin
  //if FCurrentEndX = AValue then Exit;
  FCurrentEndX := AValue;
  if not IsMatching(FCurrentEndX, FEndX) then begin
    FFrameSideColors[bsRight] := clNone;
    FMergeInfos[sscFrameRight].BaseColor := clNone;
    FMergeInfos[sscFrameRight].AlphaCount := 0;
  end;
end;

procedure TSynSelectedColorMergeResult.SetCurrentStartX(AValue: TLazSynDisplayTokenBound);
begin
  //if FCurrentStartX = AValue then Exit;
  FCurrentStartX := AValue;
  if not IsMatching(FCurrentStartX, FStartX) then begin
    FFrameSideColors[bsLeft] := clNone;
    FMergeInfos[sscFrameLeft].BaseColor := clNone;
    FMergeInfos[sscFrameLeft].AlphaCount := 0;
  end;
end;

procedure TSynSelectedColorMergeResult.AssignFrom(Src: TLazSynCustomTextAttributes);
var
  i: TLazSynBorderSide;
  j: TSynSelectedColorEnum;
  c: Integer;
begin
  //DoClear;
  FFrameSidesInitialized := False;
  FMergeInfoInitialized := False;
  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
    FFrameSideColors[i] := clNone;
    FFrameSideStyles[i] := slsSolid;
    FFrameSideOrigin[i] := sfeNone;
  end;
  FCurrentStartX.Physical := -1;
  FCurrentEndX.Physical   := -1;
  FCurrentStartX.Logical  := -1;
  FCurrentEndX.Logical    := -1;
  FCurrentStartX.Offset   := 0;
  FCurrentEndX.Offset     := 0;

  inherited AssignFrom(Src);

  if not (Src is TSynSelectedColorMergeResult) then
    exit;

  FCurrentStartX := TSynSelectedColorMergeResult(Src).FCurrentStartX;
  FCurrentEndX   := TSynSelectedColorMergeResult(Src).FCurrentEndX;
  FFrameSidesInitialized := TSynSelectedColorMergeResult(Src).FFrameSidesInitialized;

  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
    FFrameSideColors[i] := TSynSelectedColorMergeResult(Src).FFrameSideColors[i];
    FFrameSideStyles[i] := TSynSelectedColorMergeResult(Src).FFrameSideStyles[i];
    FFrameSideOrigin[i] := TSynSelectedColorMergeResult(Src).FFrameSideOrigin[i];
    FFrameSidePriority[i] := TSynSelectedColorMergeResult(Src).FFrameSidePriority[i];
  end;

  FMergeInfoInitialized := TSynSelectedColorMergeResult(Src).FMergeInfoInitialized;

  if FMergeInfoInitialized then begin
    for j := low(TSynSelectedColorEnum) to high(TSynSelectedColorEnum) do begin
      FMergeInfos[j].BaseColor    := TSynSelectedColorMergeResult(Src).FMergeInfos[j].BaseColor;
      FMergeInfos[j].BasePriority := TSynSelectedColorMergeResult(Src).FMergeInfos[j].BasePriority;
      c := TSynSelectedColorMergeResult(Src).FMergeInfos[j].AlphaCount;
      FMergeInfos[j].AlphaCount   := c;
      if Length(FMergeInfos[j].AlphaStack) < c then
        SetLength(FMergeInfos[j].AlphaStack, c + 3);
      if c > 0 then
        move(TSynSelectedColorMergeResult(Src).FMergeInfos[j].AlphaStack[0],
             FMergeInfos[j].AlphaStack[0],
             c * SizeOf(TSynSelectedColorAlphaEntry) );
    end;
  end;

  Changed; {TODO: only if really changed}
end;

procedure TSynSelectedColorMergeResult.DoClear;
var
  i: TLazSynBorderSide;
begin
  inherited;
  FFrameSidesInitialized := False;
  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
    FFrameSideColors[i] := clNone;
    FFrameSideStyles[i] := slsSolid;
    FFrameSideOrigin[i] := sfeNone;
  end;
  FCurrentStartX.Physical := -1;
  FCurrentEndX.Physical   := -1;
  FCurrentStartX.Logical  := -1;
  FCurrentEndX.Logical    := -1;
  FCurrentStartX.Offset   := 0;
  FCurrentEndX.Offset     := 0;
  CleanupMergeInfo;
end;

procedure TSynSelectedColorMergeResult.Init;
begin
  inherited Init;
  MergeFinalStyle := True;
  FMergeInfoInitialized := False;
end;

procedure TSynSelectedColorMergeResult.MaybeInitFrameSides;
var
  i: TLazSynBorderSide;
begin
  if FFrameSidesInitialized then
    exit;

  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
    FFrameSideColors[i]   := FrameSideColors[i];
    FFrameSideStyles[i]   := FrameSideStyles[i];
    FFrameSidePriority[i] := FrameSidePriority[i];
    FFrameSideOrigin[i]   := FrameSideOrigin[i];
  end;
  FFrameSidesInitialized := True;
end;

procedure TSynSelectedColorMergeResult.MergeToInfo(var AnInfo: TSynSelectedColorMergeInfo;
  AColor: TColor; APriority, AnAlpha: Integer);
begin
  if (APriority < AnInfo.BasePriority) or (AColor = clNone) then
    exit;

  if AnAlpha = 0 then begin // solid
    AnInfo.BaseColor := AColor;
    AnInfo.BasePriority := APriority;
  end
  else begin // remember alpha for later
    if Length(AnInfo.AlphaStack) <= AnInfo.AlphaCount then
      SetLength(AnInfo.AlphaStack, AnInfo.AlphaCount + 5);
    AnInfo.AlphaStack[AnInfo.AlphaCount].Color    := AColor;
    AnInfo.AlphaStack[AnInfo.AlphaCount].Alpha    := AnAlpha;
    AnInfo.AlphaStack[AnInfo.AlphaCount].Priority := APriority;
    inc(AnInfo.AlphaCount);
  end;
end;

function TSynSelectedColorMergeResult.CalculateInfo(var AnInfo: TSynSelectedColorMergeInfo;
  ANoneColor: TColor; IsFrame: Boolean): TColor;
var
  i, j, c, p: Integer;
  tmp: TSynSelectedColorAlphaEntry;
  C1, C2, C3, M1, M2, M3, Alpha: Integer;
  Col: TColor;
begin
  p := AnInfo.BasePriority;
  c := AnInfo.AlphaCount - 1;

  //if c >= 0 then begin
    while (c >= 0) and (AnInfo.AlphaStack[c].Priority < p) do
      dec(c);
    i := 1;
    while i <= c do begin
      if AnInfo.AlphaStack[i].Priority < p then begin
        AnInfo.AlphaStack[i] := AnInfo.AlphaStack[c];
        dec(c);
        while (c >= 0) and (AnInfo.AlphaStack[c].Priority < p) do
          dec(c);
        Continue;
      end;

      j := i - 1;
      if AnInfo.AlphaStack[j].Priority > AnInfo.AlphaStack[i].Priority then begin
        tmp := AnInfo.AlphaStack[i];
        AnInfo.AlphaStack[i] := AnInfo.AlphaStack[j];
        while (j > 0) and (AnInfo.AlphaStack[j-1].Priority > AnInfo.AlphaStack[j].Priority) do begin
          AnInfo.AlphaStack[j] := AnInfo.AlphaStack[j-1];
          dec(j);
        end;
        AnInfo.AlphaStack[j] := tmp;
      end;

      inc(i);
    end;
  //end;

  Result := AnInfo.BaseColor;

  // The highlighter may have merged, before defaults where set in
  // TLazSynPaintTokenBreaker.GetNextHighlighterTokenFromView / InitSynAttr
  if (Result = clNone) and (not IsFrame) then
    Result := ANoneColor;

  if (c >= 0) and (AnInfo.AlphaStack[0].Priority >= p) then begin
    if (Result = clNone) then
      Result := ANoneColor;
    Result := ColorToRGB(Result);  // no system color.
    C1 := Red(Result);
    C2 := Green(Result);
    C3 := Blue(Result);
    for i := 0 to c do begin
      Col := ColorToRGB(AnInfo.AlphaStack[i].Color);
      Alpha := AnInfo.AlphaStack[i].Alpha;
      M1 := Red(Col);
      M2 := Green(Col);
      M3 := Blue(Col);
      C1 := MinMax(C1 + (M1 - C1) * Alpha div 256, 0, 255);
      C2 := MinMax(C2 + (M2 - C2) * Alpha div 256, 0, 255);
      C3 := MinMax(C3 + (M3 - C3) * Alpha div 256, 0, 255);

    end;
    Result := RGBToColor(C1, C2, C3);
  end;
end;

destructor TSynSelectedColorMergeResult.Destroy;
begin
  CleanupMergeInfo;
  inherited Destroy;
end;

procedure TSynSelectedColorMergeResult.InitMergeInfo;
begin
  MaybeInitFrameSides;

  FMergeInfos[sscBack].AlphaCount   := 0;
  FMergeInfos[sscBack].BaseColor    := Background;
  FMergeInfos[sscBack].BasePriority := BackPriority;

  FMergeInfos[sscFore].AlphaCount   := 0;
  FMergeInfos[sscFore].BaseColor    := Foreground;
  FMergeInfos[sscFore].BasePriority := ForePriority;

  FMergeInfos[sscFrameLeft].AlphaCount   := 0;
  FMergeInfos[sscFrameLeft].BaseColor    := FrameSideColors[bsLeft];
  FMergeInfos[sscFrameLeft].BasePriority := FrameSidePriority[bsLeft];

  FMergeInfos[sscFrameRight].AlphaCount   := 0;
  FMergeInfos[sscFrameRight].BaseColor    := FrameSideColors[bsRight];
  FMergeInfos[sscFrameRight].BasePriority := FrameSidePriority[bsRight];

  FMergeInfos[sscFrameTop].AlphaCount   := 0;
  FMergeInfos[sscFrameTop].BaseColor    := FrameSideColors[bsTop];
  FMergeInfos[sscFrameTop].BasePriority := FrameSidePriority[bsTop];

  FMergeInfos[sscFrameBottom].AlphaCount   := 0;
  FMergeInfos[sscFrameBottom].BaseColor    := FrameSideColors[bsBottom];
  FMergeInfos[sscFrameBottom].BasePriority := FrameSidePriority[bsBottom];

  FMergeInfoInitialized := True;
end;

procedure TSynSelectedColorMergeResult.ProcessMergeInfo;
begin
  if not FMergeInfoInitialized then
    exit;
  BeginUpdate;
  Background := CalculateInfo(FMergeInfos[sscBack], Background);
  Foreground := CalculateInfo(FMergeInfos[sscFore], Foreground);
  // if the frame is clNone, and alpha is aplied, use the background as base
  FFrameSideColors[bsLeft]   := CalculateInfo(FMergeInfos[sscFrameLeft],   Background, True);
  FFrameSideColors[bsRight]  := CalculateInfo(FMergeInfos[sscFrameRight],  Background, True);
  FFrameSideColors[bsTop]    := CalculateInfo(FMergeInfos[sscFrameTop],    Background, True);
  FFrameSideColors[bsBottom] := CalculateInfo(FMergeInfos[sscFrameBottom], Background, True);
  EndUpdate;
  FMergeInfoInitialized := False;
end;

procedure TSynSelectedColorMergeResult.CleanupMergeInfo;
var
  i: TSynSelectedColorEnum;
begin
  for i := low(TSynSelectedColorEnum) to high(TSynSelectedColorEnum) do
    SetLength(FMergeInfos[i].AlphaStack, 0);
  FMergeInfoInitialized := False;
end;

procedure TSynSelectedColorMergeResult.Merge(Other: TSynHighlighterAttributesModifier);
begin
  Merge(Other, FStartX, FEndX); // always merge frame
end;

procedure TSynSelectedColorMergeResult.Merge(Other: TSynHighlighterAttributesModifier; LeftCol,
  RightCol: TLazSynDisplayTokenBound);
var
  sKeep, sSet, sClr, sInv, sInvInv: TFontStyles;
  j: TFontStyle;
begin
  BeginUpdate;
  if not FMergeInfoInitialized then
    InitMergeInfo;

  MergeToInfo(FMergeInfos[sscBack], Other.Background, Other.BackPriority, Other.BackAlpha);
  MergeToInfo(FMergeInfos[sscFore], Other.Foreground, Other.ForePriority, Other.ForeAlpha);

  MergeFrames(Other, LeftCol, RightCol);

  sKeep := [];
  for j := Low(TFontStyle) to High(TFontStyle) do
    if Other.StylePriority[j] < StylePriority[j]
     then sKeep := sKeep + [j];

  sSet := (Other.Style        * Other.StyleMask) - sKeep;
  sClr := (fsNot(Other.Style) * Other.StyleMask) - sKeep;
  sInv := (Other.Style        * fsNot(Other.StyleMask)) - sKeep;

  if MergeFinalStyle then begin
    Style := fsXor(Style, sInv) + sSet - sClr;
  end else begin
    sKeep := fsNot(Other.Style) * fsNot(Other.StyleMask);
    sInvInv := sInv * (Style * fsNot(StyleMask)); // invert * invert = not modified
    sInv    := sInv - sInvInv;
    sSet := sSet + sInv * (fsnot(Style) * StyleMask); // currently not set
    sClr := sClr + sInv * (Style        * StyleMask); // currently set
    sInv    := sInv - StyleMask; // now SInv only inverts currently "not modifying"

    Style     := (Style     * sKeep) + sSet - sClr - sInvInv + sInv;
    StyleMask := (StyleMask * sKeep) + sSet + sClr - sInvInv - sInv;
  end;


  //sMask := Other.StyleMask                            // Styles to be taken from Other
  //       + (fsNot(Other.StyleMask) * Other.Style);    // Styles to be inverted
  //Style     := (Style * fsNot(sMask))    // Styles that are neither taken, nor inverted
  //           + (Other.Style * sMask);    // Styles that are either inverted or set
  //StyleMask := (StyleMask * fsNot(sMask)) + (Other.StyleMask * sMask);

  EndUpdate;
end;

procedure TSynSelectedColorMergeResult.MergeFrames(Other: TSynHighlighterAttributesModifier; LeftCol,
  RightCol: TLazSynDisplayTokenBound);

  //procedure SetSide(ASide: TLazSynBorderSide; ASrc: TSynHighlighterAttributesModifier);
  //begin
  //(*
  //  if (FrameSideColors[ASide] <> clNone) and
  //     ( (ASrc.FramePriority < FrameSidePriority[ASide]) or
  //       ( (ASrc.FramePriority = FrameSidePriority[ASide]) and
  //         (SynFrameEdgePriorities[ASrc.FrameEdges] < SynFrameEdgePriorities[FrameSideOrigin[ASide]]) )
  //     )
  //
  //*)
  //  if (FrameSideColors[ASide] <> clNone) and
  //     ( (ASrc.FramePriority < FrameSidePriority[ASide]) or
  //       ( (ASrc.FramePriority = FrameSidePriority[ASide]) and
  //         (SynFrameEdgePriorities[ASrc.FrameEdges] < SynFrameEdgePriorities[FrameSideOrigin[ASide]]) )
  //     )
  //  then
  //    exit;
  //  FFrameSideColors[ASide] := ASrc.FrameColor;
  //  FFrameSideStyles[ASide] := ASrc.FrameStyle;
  //  FFrameSidePriority[ASide] := ASrc.FramePriority;
  //  FFrameSideOrigin[ASide]   := ASrc.FrameEdges;
  //  if ASide = bsLeft then
  //    FStartX := LeftCol; // LeftCol has Phys and log ; // ASrc.FStartX;
  //  if ASide = bsRight then
  //    FEndX := RightCol; // ASrc.FEndX;
  //end;

  procedure SetSide(AInfoSide: TSynSelectedColorEnum; ASide: TLazSynBorderSide;
    ASrc: TSynHighlighterAttributesModifier);
  begin
    if (FMergeInfos[AInfoSide].BaseColor <> clNone) and
       ( (ASrc.FramePriority < FMergeInfos[AInfoSide].BasePriority) or
         ( (ASrc.FramePriority = FMergeInfos[AInfoSide].BasePriority) and
           (SynFrameEdgePriorities[ASrc.FrameEdges] < SynFrameEdgePriorities[FrameSideOrigin[ASide]]) )
       )
    then
      exit;

    MergeToInfo(FMergeInfos[AInfoSide], ASrc.FrameColor, ASrc.FramePriority, ASrc.FrameAlpha);

    FFrameSidePriority[ASide] := ASrc.FramePriority; // used for style (style may be taken, from an alpha frame
    if ( (ASrc.FramePriority > FFrameSidePriority[ASide]) or
         ( (ASrc.FramePriority = FFrameSidePriority[ASide]) and
           (SynFrameEdgePriorities[ASrc.FrameEdges] >= SynFrameEdgePriorities[FrameSideOrigin[ASide]]) )
       )
    then
      FFrameSideStyles[ASide] := ASrc.FrameStyle;

    if ASrc.FrameAlpha = 0 then
      FFrameSideOrigin[ASide] := ASrc.FrameEdges;
  end;

begin
  if not FFrameSidesInitialized then
    MaybeInitFrameSides;

  If (Other = nil) or (Other.FrameColor = clNone) then
    exit;

  // Merge Values
  case Other.FrameEdges of
    sfeAround: begin
        // UpdateOnly, frame keeps behind individual sites
        if (not (Other is TSynSelectedColor)) or  // always merge, if it has no startx
           IsMatching(TSynSelectedColor(Other).StartX, LeftCol)
        then
          SetSide(sscFrameLeft, bsLeft, Other);
        if  (not (Other is TSynSelectedColor)) or
           IsMatching(TSynSelectedColor(Other).EndX, RightCol)
        then
          SetSide(sscFrameRight, bsRight, Other);
        SetSide(sscFrameBottom, bsBottom, Other);
        SetSide(sscFrameTop, bsTop, Other);
        //FrameColor := Other.FrameColor;
        //FrameStyle := Other.FrameStyle;
        //FrameEdges := Other.FrameEdges;
      end;
    sfeBottom: begin
        SetSide(sscFrameBottom, bsBottom, Other);
      end;
    sfeLeft: begin
       // startX ?
        SetSide(sscFrameLeft, bsLeft, Other);
      end;
  end;
end;

{ TSynSelectedColor }

function TSynSelectedColor.GetModifiedStyle(aStyle : TFontStyles) : TFontStyles;
begin
  Result := fsXor(aStyle, Style * fsNot(StyleMask)) // Invert Styles
            + (Style*StyleMask)                     // Set Styles
            - (fsNot(Style)*StyleMask);             // Remove Styles
end;

procedure TSynSelectedColor.ModifyColors(var AForeground, ABackground,
    AFrameColor: TColor; var AStyle: TFontStyles; var AFrameStyle: TSynLineStyle);
begin
  if Foreground <> clNone then AForeground := Foreground;
  if Background <> clNone then ABackground := Background;
  if FrameColor <> clNone then
  begin
    AFrameColor := FrameColor;
    AFrameStyle := FrameStyle;
  end;

  AStyle := GetModifiedStyle(AStyle);
end;

procedure TSynSelectedColor.AssignFrom(Src: TLazSynCustomTextAttributes);
begin
  inherited AssignFrom(Src);
  if not (Src is TSynSelectedColor) then exit;

  FStartX := TSynSelectedColor(Src).FStartX;
  FEndX   := TSynSelectedColor(Src).FEndX;

  Changed; {TODO: only if really changed}
end;

procedure TSynSelectedColor.Init;
begin
  inherited Init;
  Background := clHighLight;
  Foreground := clHighLightText;
  FrameColor := clNone;
  FrameStyle := slsSolid;
  FrameEdges := sfeAround;
  InternalSaveDefaultValues;
end;

procedure TSynSelectedColor.SetFrameBoundsPhys(AStart, AEnd: Integer);
begin
  FStartX.Physical := AStart;
  FEndX.Physical   := AEnd;
  FStartX.Logical  := -1;
  FEndX.Logical    := -1;
  FStartX.Offset   := 0;
  FEndX.Offset     := 0;
end;

procedure TSynSelectedColor.SetFrameBoundsLog(AStart, AEnd: Integer; AStartOffs: Integer;
  AEndOffs: Integer);
begin
  FStartX.Physical := -1;
  FEndX.Physical   := -1;
  FStartX.Logical  := AStart;
  FEndX.Logical    := AEnd;
  FStartX.Offset   := AStartOffs;
  FEndX.Offset     := AEndOffs;
end;

procedure TSynSelectedColor.DoClear;
begin
  inherited;
  FStartX.Physical := -1;
  FEndX.Physical   := -1;
  FStartX.Logical  := -1;
  FEndX.Logical    := -1;
  FStartX.Offset   := 0;
  FEndX.Offset     := 0;
end;

function TSynSelectedColor.IsEnabled: boolean;
begin
  Result := (Background <> clNone) or (Foreground <> clNone) or (FrameColor <> clNone) or
            (Style <> []) or (StyleMask <> []);
end;

{ TLazSynSurface }

function TLazSynSurface.GetHandle: HWND;
begin
  Result := FOwner.Handle;
end;

procedure TLazSynSurface.SetDisplayView(AValue: TLazSynDisplayView);
begin
  if FDisplayView = AValue then Exit;
  FDisplayView := AValue;
  DoDisplayViewChanged;
end;

procedure TLazSynSurface.BoundsChanged;
begin
  //
end;

procedure TLazSynSurface.DoDisplayViewChanged;
begin
  //
end;

constructor TLazSynSurface.Create(AOwner: TWinControl);
begin
  FOwner := AOwner;
  FBoundsChangeList := TMethodList.Create;
end;

destructor TLazSynSurface.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBoundsChangeList);
end;

procedure TLazSynSurface.Assign(Src: TLazSynSurface);
begin
  // do not assign the bounds
  DisplayView := Src.DisplayView;
end;

procedure TLazSynSurface.AddBoundsChangeHandler(AHandler: TNotifyEvent);
begin
  FBoundsChangeList.Add(TMethod(AHandler));
end;

procedure TLazSynSurface.RemoveBoundsChangeHandler(AHandler: TNotifyEvent);
begin
  FBoundsChangeList.Remove(TMethod(AHandler));
end;

procedure TLazSynSurface.Paint(ACanvas: TCanvas; AClip: TRect);
begin
  if (AClip.Left   >= Bounds.Right) or
     (AClip.Right  <= Bounds.Left) or
     (AClip.Top    >= Bounds.Bottom) or
     (AClip.Bottom <= Bounds.Top)
  then
    exit;

  if (AClip.Left   < Bounds.Left)   then AClip.Left   := Bounds.Left;
  if (AClip.Right  > Bounds.Right)  then AClip.Right  := Bounds.Right;
  if (AClip.Top    < Bounds.Top)    then AClip.Top    := Bounds.Top;
  if (AClip.Bottom > Bounds.Bottom) then AClip.Bottom := Bounds.Bottom;

  DoPaint(ACanvas, AClip);
end;

procedure TLazSynSurface.InvalidateLines(FirstTextLine, LastTextLine: TLineIdx);
begin
  //
end;

procedure TLazSynSurface.SetBounds(ATop, ALeft, ABottom, ARight: Integer);
begin
  if (FBounds.Left = ALeft) and (FBounds.Top = ATop) and
     (FBounds.Right = ARight) and (FBounds.Bottom = ABottom)
  then exit;

  FBounds.Left := ALeft;
  FBounds.Top := ATop;
  FBounds.Right := ARight;
  FBounds.Bottom := ABottom;
  BoundsChanged;
  FBoundsChangeList.CallNotifyEvents(Self);
end;

{ TSynBookMarkOpt }

constructor TSynBookMarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  fDrawBookmarksFirst := TRUE;                                                  //mh 2000-10-12
  fEnableKeys := True;
  fGlyphsVisible := True;
  fLeftMargin := 2;
  fOwner := AOwner;
  fXOffset := 12;
end;

procedure TSynBookMarkOpt.SetBookmarkImages(const Value: TCustomImageList);
begin
  if fBookmarkImages <> Value then begin
    if Assigned(fBookmarkImages) then fBookmarkImages.RemoveFreeNotification(fOwner);
    fBookmarkImages := Value;
    if Assigned(fBookmarkImages) then fBookmarkImages.FreeNotification(fOwner);
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{begin}                                                                         //mh 2000-10-12
procedure TSynBookMarkOpt.SetDrawBookmarksFirst(Value: boolean);
begin
  if Value <> fDrawBookmarksFirst then begin
    fDrawBookmarksFirst := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;
{end}                                                                           //mh 2000-10-12

procedure TSynBookMarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if fGlyphsVisible <> Value then begin
    fGlyphsVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetLeftMargin(Value: Integer);
begin
  if fLeftMargin <> Value then begin
    fLeftMargin := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetXOffset(Value: integer);
begin
  if fXOffset <> Value then begin
    fXOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

var
  InternalImages: TBitmap;
  InternalImagesUsers: integer;
  IIWidth, IIHeight: integer;
  IICount: integer;

constructor TSynInternalImage.Create(const AName: string; Count: integer);
begin
  inherited Create;
  Inc(InternalImagesUsers);
  if InternalImagesUsers = 1 then begin
    InternalImages := TBitmap.Create;
    InternalImages.LoadFromResourceName(HInstance, AName);
    IIWidth := (InternalImages.Width + Count shr 1) div Count;
    IIHeight := InternalImages.Height;
    IICount := Count;
  end;
end;

destructor TSynInternalImage.Destroy;
begin
  Dec(InternalImagesUsers);
  if InternalImagesUsers = 0 then begin
    InternalImages.Free;
    InternalImages := nil;
  end;
  inherited Destroy;
end;

procedure TSynInternalImage.DrawMark(ACanvas: TCanvas;
  Number, X, Y, LineHeight: integer);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < IICount) then
  begin
    if LineHeight >= IIHeight then begin
      rcSrc := Rect(Number * IIWidth, 0, (Number + 1) * IIWidth, IIHeight);
      Inc(Y, (LineHeight - IIHeight) div 2);
      rcDest := Rect(X, Y, X + IIWidth, Y + IIHeight);
    end else begin
      rcDest := Rect(X, Y, X + IIWidth, Y + LineHeight);
      Y := (IIHeight - LineHeight) div 2;
      rcSrc := Rect(Number * IIWidth, Y, (Number + 1) * IIWidth, Y + LineHeight);
    end;
    ACanvas.CopyRect(rcDest, InternalImages.Canvas, rcSrc);
  end;
end;

{ TSynObjectList }

constructor TSynObjectList.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  SetAncestor(True);
  SetInline(True);
  FList := TList.Create;
  FOwner := AOwner;
end;

destructor TSynObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TSynObjectList.Assign(Source: TPersistent);
begin
  FList.Assign(TSynObjectList(Source).FList);
  DoChange(self);
end;

function TSynObjectList.GetChildOwner: TComponent;
begin
  Result := self;
end;

procedure TSynObjectList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Root = self then
    for i:= 0 to Count -1 do
      Proc(BaseItems[i]);
end;

procedure TSynObjectList.SetChildOrder(Child: TComponent; Order: Integer);
begin
  (Child as TSynObjectListItem).Index := Order;
  DoChange(self);;
end;

procedure TSynObjectList.RegisterItem(AnItem: TSynObjectListItem);
begin
  Add(AnItem);
end;

function TSynObjectList.GetBasePart(Index: Integer): TSynObjectListItem;
begin
  Result := TSynObjectListItem(FList[Index]);
end;

procedure TSynObjectList.PutBasePart(Index: Integer; const AValue: TSynObjectListItem);
begin
  FList[Index] := Pointer(AValue);
  DoChange(self);
end;

procedure TSynObjectList.SetSorted(const AValue: Boolean);
begin
  if FSorted = AValue then exit;
  FSorted := AValue;
  Sort;
end;

procedure TSynObjectList.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function CompareSynObjectListItems(Item1, Item2: Pointer): Integer;
begin
  Result := TSynObjectListItem(Item1).Compare(TSynObjectListItem(Item2));
end;

procedure TSynObjectList.Sort;
begin
  FList.Sort(@CompareSynObjectListItems);
end;

function TSynObjectList.Add(AnItem: TSynObjectListItem): Integer;
begin
  Result := FList.Add(Pointer(AnItem));
  if FSorted then Sort;
  DoChange(self);
end;

procedure TSynObjectList.Delete(Index: Integer);
begin
  FList.Delete(Index);
  DoChange(self);
end;

procedure TSynObjectList.Clear;
begin
  while FList.Count > 0 do
    BaseItems[0].Free;
  FList.Clear;
  DoChange(self);
end;

function TSynObjectList.Count: Integer;
begin
  Result := FList.Count;
end;

function TSynObjectList.IndexOf(AnItem: TSynObjectListItem): Integer;
begin
  Result := Flist.IndexOf(Pointer(AnItem));
end;

procedure TSynObjectList.Move(AOld, ANew: Integer);
begin
  if FSorted then raise Exception.Create('not allowed');
  FList.Move(AOld, ANew);
  DoChange(self);;
end;

{ TSynObjectListItem }

function TSynObjectListItem.GetIndex: Integer;
begin
  Result := Owner.IndexOf(self);
end;

function TSynObjectListItem.GetDisplayName: String;
begin
  Result := Name + ' (' + ClassName + ')';
end;

procedure TSynObjectListItem.Init;
begin
  //
end;

procedure TSynObjectListItem.SetIndex(const AValue: Integer);
begin
  Owner.Move(GetIndex, AValue);
end;

function TSynObjectListItem.Compare(Other: TSynObjectListItem): Integer;
begin
  Result := PtrUInt(self) - PtrUInt(Other);
end;

constructor TSynObjectListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAncestor(True);
  FOwner := AOwner as TSynObjectList;
  Init;
  FOwner.RegisterItem(self);
end;

destructor TSynObjectListItem.Destroy;
begin
  inherited Destroy;
  FOwner.Delete(FOwner.IndexOf(self));
end;

function TSynObjectListItem.GetParentComponent: TComponent;
begin
  Result := FOwner;
end;

{ TSynClipboardStream }

function TSynClipboardStream.GetMemory: Pointer;
begin
  Result := FMemStream.Memory;
end;

function TSynClipboardStream.GetSize: LongInt;
begin
  Result := FMemStream.Size;
end;

procedure TSynClipboardStream.SetInternalText(const AValue: String);
begin
  FIsPlainText := False;
  // Text, if we don't need CF_TEXT // Must include a zero byte
  AddTag(synClipTagText, @AValue[1], length(AValue) + 1);
end;

function TSynClipboardStream.GetSelectionMode: TSynSelectionMode;
var
  PasteMode: ^TSynSelectionMode;
begin
  PasteMode := GetTagPointer(synClipTagMode);
  if PasteMode = nil then
    Result := smNormal
  else
    Result := PasteMode^;
end;

procedure TSynClipboardStream.SetSelectionMode(const AValue: TSynSelectionMode);
begin
  AddTag(synClipTagMode, @AValue, SizeOf(TSynSelectionMode));
end;

procedure TSynClipboardStream.SetText(const AValue: String);
var
  SLen: Integer;
begin
  FIsPlainText := True;
  FText := AValue;
  SLen := length(FText);
  AddTag(synClipTagExtText, @SLen, SizeOf(Integer));
end;

constructor TSynClipboardStream.Create;
begin
  FMemStream := TMemoryStream.Create;
end;

destructor TSynClipboardStream.Destroy;
begin
  FreeAndNil(FMemStream);
  inherited Destroy;
end;

class function TSynClipboardStream.ClipboardFormatId: TClipboardFormat;
const
  SYNEDIT_CLIPBOARD_FORMAT_TAGGED = 'Application/X-Laz-SynEdit-Tagged';
  Format: TClipboardFormat = 0;
begin
  if Format = 0 then
    Format := ClipboardRegisterFormat(SYNEDIT_CLIPBOARD_FORMAT_TAGGED);
  Result := Format;
end;

function TSynClipboardStream.CanReadFromClipboard(AClipboard: TClipboard): Boolean;
begin
  Result := AClipboard.HasFormat(ClipboardFormatId);
end;

function TSynClipboardStream.ReadFromClipboard(AClipboard: TClipboard): Boolean;
var
  ip: PInteger;
  len: LongInt;
begin
  Result := false;
  Clear;
  FTextP := nil;
  // Check for embedded text
  if AClipboard.HasFormat(ClipboardFormatId) then begin
    Result := AClipboard.GetFormat(ClipboardFormatId, FMemStream);
    FTextP := GetTagPointer(synClipTagText);
    if FTextP <> nil then begin
      len := GetTagLen(synClipTagText);
      if len > 0 then
        (FTextP + len - 1)^ := #0
      else
        FTextP := nil;
    end;
  end;
  // Normal text
  if (FTextP = nil) then begin
    Result := true;
    FText := AClipboard.AsText;
    if FText <> '' then begin
      FTextP := @FText[1];
      ip := GetTagPointer(synClipTagExtText);
      if (length(FText) = 0) or (ip = nil) or (length(FText) <> ip^) then
        FIsPlainText := True;
    end;
  end;
end;

function TSynClipboardStream.WriteToClipboard(AClipboard: TClipboard): Boolean;
begin
  AClipboard.Open;
  try
    if FIsPlainText and (FText <> '') then begin
      AClipboard.AsText:= FText;
    end;
    Result := AClipboard.AddFormat(ClipboardFormatId, FMemStream.Memory^, FMemStream.Size);
  finally
    AClipboard.Close;
  end;
  {$IFDEF SynClipboardExceptions}
  if not AClipboard.HasFormat(CF_TEXT) then
    raise ESynEditError.Create('Clipboard copy operation failed: HasFormat');
  {$ENDIF}
end;

procedure TSynClipboardStream.Clear;
begin
  FMemStream.Clear;
  FIsPlainText := False;
end;

function TSynClipboardStream.HasTag(ATag: TSynClipboardStreamTag): Boolean;
begin
  Result := GetTagPointer(ATag) <> nil;
end;

function TSynClipboardStream.GetTagPointer(ATag: TSynClipboardStreamTag): Pointer;
var
  ctag, mend: Pointer;
begin
  Result :=  nil;
  if FIsPlainText then
    exit;
  ctag := FMemStream.Memory;
  mend := ctag + FMemStream.Size;
  while (result = nil) and
        (ctag + SizeOf(TSynClipboardStreamTag) + SizeOf(Integer) <= mend) do
  begin
     if TSynClipboardStreamTag(ctag^) = ATag then begin
      Result := ctag + SizeOf(TSynClipboardStreamTag) + SizeOf(Integer)
    end else begin
      inc(ctag, SizeOf(TSynClipboardStreamTag));
      inc(ctag, PInteger(ctag)^);
      inc(ctag, SizeOf(Integer));
      {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      ctag := Align(ctag, SizeOf(integer));
      {$ENDIF}
    end;
  end;
  if (Result <> nil) and
     (ctag + Integer((ctag + SizeOf(TSynClipboardStreamTag))^) > mend) then
  begin
    Result := nil;
    raise ESynEditError.Create('Clipboard read operation failed, data corrupt');
  end;
end;

function TSynClipboardStream.GetTagLen(ATag: TSynClipboardStreamTag): Integer;
var
  p: PInteger;
begin
  Result := 0;
  p := GetTagPointer(ATag);
  if p = nil then
    exit;
  dec(p, 1);
  Result := p^;
end;

procedure TSynClipboardStream.AddTag(ATag: TSynClipboardStreamTag; Location: Pointer;
  Len: Integer);
var
  msize: Int64;
  mpos: Pointer;
  LenBlock:PtrUInt;
begin
  msize := FMemStream.Size;
  LenBlock:= Len + SizeOf(TSynClipboardStreamTag) + SizeOf(Integer);
  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  LenBlock := Align(LenBlock, SizeOf(integer));
  {$ENDIF}
  FMemStream.Size := msize +LenBlock;
  mpos := FMemStream.Memory + msize;
  TSynClipboardStreamTag(mpos^) := ATag;
  inc(mpos, SizeOf(TSynClipboardStreamTag));
  Integer(mpos^) := Len;
  inc(mpos, SizeOf(Integer));
  System.Move(Location^, mpos^, Len);
end;

{ TSynWordBreaker }

procedure TSynWordBreaker.SetIdentChars(const AValue: TSynIdentChars);
begin
  if FIdentChars = AValue then exit;
  FIdentChars := AValue;
end;

procedure TSynWordBreaker.SetWhiteChars(const AValue: TSynIdentChars);
begin
  if FWhiteChars = AValue then exit;
  FWhiteChars := AValue;
  FWordChars := [#1..#255] - (FWordBreakChars + FWhiteChars);
end;

procedure TSynWordBreaker.SetWordBreakChars(const AValue: TSynIdentChars);
begin
  if FWordBreakChars = AValue then exit;
  FWordBreakChars := AValue;
  FWordChars := [#1..#255] - (FWordBreakChars + FWhiteChars);
end;

constructor TSynWordBreaker.Create;
begin
  inherited;
  Reset;
end;

procedure TSynWordBreaker.Reset;
begin
  FWhiteChars     := TSynWhiteChars;
  FWordBreakChars := TSynWordBreakChars;
  FIdentChars     := TSynValidStringChars - TSynSpecialChars;
  FWordChars      := [#1..#255] - (FWordBreakChars + FWhiteChars);
end;

function TSynWordBreaker.IsInWord(aLine: String; aX: Integer): Boolean;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(False);
  Result := ((ax <= len) and (aLine[aX] in FWordChars)) or
            ((aX > 1) and (aLine[aX - 1] in FWordChars));
end;

function TSynWordBreaker.IsAtWordStart(aLine: String; aX: Integer): Boolean;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len) then exit(False);
  Result := (aLine[aX] in FWordChars) and
            ((aX = 1) or not (aLine[aX - 1] in FWordChars));
end;

function TSynWordBreaker.IsAtWordEnd(aLine: String; aX: Integer): Boolean;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX <= 1) or (aX > len + 1) or (len = 0) then exit(False);
  Result := ((ax = len + 1) or not(aLine[aX] in FWordChars)) and
            (aLine[aX - 1] in FWordChars);
end;

function TSynWordBreaker.NextWordStart(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) then exit(-1);
  if not aIncludeCurrent then
    inc(aX);
  if (aX > len + 1) then exit(-1);
  if (aX > 1) and (aLine[aX - 1] in FWordChars) then
    while (aX <= len) and (aLine[aX] in FWordChars) do Inc(ax);
  while (aX <= len) and not(aLine[aX] in FWordChars) do Inc(ax);
  if aX > len then
    exit(-1);
  Result := aX;
end;

function TSynWordBreaker.NextWordEnd(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) then exit(-1);
  if not aIncludeCurrent then
    inc(aX);
  if (aX > len + 1) then exit(-1);
  if (aX = 1) or not(aLine[aX - 1] in FWordChars) then begin
    while (aX <= len) and not(aLine[aX] in FWordChars) do Inc(ax);
    if (aX >= len + 1) then exit(-1);
  end;
  while (aX <= len) and (aLine[aX] in FWordChars) do Inc(ax);
  Result := aX;
end;

function TSynWordBreaker.PrevWordStart(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if not aIncludeCurrent then
    dec(aX);
  while (aX >= 1) and ( (ax > len) or not(aLine[aX] in FWordChars) ) do Dec(ax);
  if aX = 0 then
    exit(-1);
  while (aX >= 1) and ( (ax > len) or (aLine[aX] in FWordChars) ) do Dec(ax);
  Result := aX  + 1;
end;

function TSynWordBreaker.PrevWordEnd(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if not aIncludeCurrent then
    dec(aX);
  if aX <= len then
    while (aX >= 1) and (aLine[aX] in FWordChars) do Dec(ax);
  while (aX >= 1) and ( (ax > len) or not(aLine[aX] in FWordChars) ) do Dec(ax);
  if aX = 0 then
    exit(-1);
  Result := aX + 1;
end;

function TSynWordBreaker.NextBoundary(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) then exit(-1);
  if aIncludeCurrent then dec(ax);
  if (ax > len) then exit(-1);

  if (aX > 0) and (aLine[aX] in FWordChars) then
    while (aX <= len) and (aLine[aX] in FWordChars) do Inc(ax)
  else
  if (aX > 0) and (aLine[aX] in FWordBreakChars) then
    while (aX <= len) and (aLine[aX] in FWordBreakChars) do Inc(ax)
  else
  begin
    while (aX <= len) and ((aX = 0) or (aLine[aX] in FWhiteChars)) do Inc(ax);
    if (ax > len) then exit(-1);
  end;
  Result := aX;
end;

function TSynWordBreaker.PrevBoundary(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX > len + 1) then exit(-1);
  if not aIncludeCurrent then dec(ax);
  if (aX < 1) then exit(-1);

  if (aX <= len) and (aLine[aX] in FWordChars) then
    while (aX >= 1) and (aLine[aX] in FWordChars) do dec(ax)
  else
  if (aX <= len) and (aLine[aX] in FWordBreakChars) then
    while (aX >= 1) and (aLine[aX] in FWordBreakChars) do dec(ax)
  else
  begin
    while (aX >= 1) and ((aX > len) or (aLine[aX] in FWhiteChars)) do dec(ax);
    if aX = 0 then exit(-1);
  end;
  Result := aX + 1;
end;

{ TSynMethodList }

function TSynMethodList.IndexToObjectIndex(const AnObject: TObject; AnIndex: Integer): integer;
var
  i, c: Integer;
begin
  Result := -1;
  if Self = nil then exit;
  i := 0;
  c := Count;
  while i < c do begin
    if TObject(Items[i].Data)=AnObject then begin
      if AnIndex = 0 then exit(i);
      dec(AnIndex);
    end;
    inc(i);
  end;
end;

function TSynMethodList.GetObjectItems(AnObject: TObject; Index: integer): TMethod;
begin
  Result := Items[IndexToObjectIndex(AnObject, Index)];
end;

procedure TSynMethodList.SetObjectItems(AnObject: TObject; Index: integer;
  const AValue: TMethod);
begin
  Items[IndexToObjectIndex(AnObject, Index)] := AValue;
end;

function TSynMethodList.CountByObject(const AnObject: TObject): integer;
var
  i: Integer;
begin
  Result := 0;
  if Self=nil then exit;
  i := Count-1;
  while i>=0 do begin
    if TObject(Items[i].Data)=AnObject then inc(Result);
    dec(i);
  end;
end;

procedure TSynMethodList.DeleteByObject(const AnObject: TObject; Index: integer);
begin
  Delete(IndexToObjectIndex(AnObject, Index));
end;

procedure TSynMethodList.AddCopyFrom(AList: TSynMethodList; AOwner: TObject = nil);
var
  i: Integer;
begin
  if AOwner = nil then begin
    for i := 0 to AList.Count - 1 do
      Add(AList.Items[i], True);
  end else begin
    for i := 0 to AList.CountByObject(AOwner) - 1 do
      Add(AList.ItemsByObject[AOwner, i], True);
  end;
end;

{ TSynFilteredMethodList }

function TSynFilteredMethodList.IndexOf(AHandler: TMethod): Integer;
begin
  Result := FCount - 1;
  while (Result >= 0) and
        ( (FItems[Result].FHandler.Code <> AHandler.Code) or
          (FItems[Result].FHandler.Data <> AHandler.Data) )
  do
    dec(Result);
end;

function TSynFilteredMethodList.IndexOf(AHandler: TMethod; AFilter: LongInt): Integer;
begin
  Result := FCount - 1;
  while (Result >= 0) and (
        (FItems[Result].FHandler.Code <> AHandler.Code) or
        (FItems[Result].FHandler.Data <> AHandler.Data) or
        (FItems[Result].FFilter <> AFilter) )
  do
    dec(Result);
end;

function TSynFilteredMethodList.NextDownIndex(var Index: integer): boolean;
begin
  if Self<>nil then begin
    dec(Index);
    if (Index>=FCount) then
      Index:=FCount-1;
  end else
    Index:=-1;
  Result:=(Index>=0);
end;

function TSynFilteredMethodList.NextDownIndexNumFilter(var Index: integer;
  AFilter: LongInt): boolean;
begin
  Repeat
    Result := NextDownIndex(Index);
  until (not Result) or (FItems[Index].FFilter = AFilter);
end;

function TSynFilteredMethodList.NextDownIndexBitFilter(var Index: integer;
  AFilter: LongInt): boolean;
begin
  Repeat
    Result := NextDownIndex(Index);
  until (not Result) or ((FItems[Index].FFilter and AFilter) <> 0);
end;

procedure TSynFilteredMethodList.Delete(AIndex: Integer);
begin
  if AIndex < 0 then exit;
  while AIndex < FCount - 1 do begin
    FItems[AIndex] := FItems[AIndex + 1];
    inc(AIndex);
  end;
  dec(FCount);
  if length(FItems) > FCount * 4 then
    SetLength(FItems, FCount * 2);
end;

constructor TSynFilteredMethodList.Create;
begin
  FCount := 0;
end;

procedure TSynFilteredMethodList.AddNumFilter(AHandler: TMethod; AFilter: LongInt);
var
  i: Integer;
begin
  i := IndexOf(AHandler, AFilter);
  if i >= 0 then
    raise Exception.Create('Duplicate');

  if FCount >= high(FItems) then
    SetLength(FItems, Max(8, FCount * 2));
  FItems[FCount].FHandler := AHandler;
  FItems[FCount].FFilter := AFilter;
  inc(FCount);
end;

procedure TSynFilteredMethodList.AddBitFilter(AHandler: TMethod; AFilter: LongInt);
var
  i: Integer;
begin
  i := IndexOf(AHandler);
  if i >= 0 then
    FItems[i].FFilter := FItems[i].FFilter or AFilter
  else begin
    if FCount >= high(FItems) then
      SetLength(FItems, Max(8, FCount * 2));
    FItems[FCount].FHandler := AHandler;
    FItems[FCount].FFilter := AFilter;
    inc(FCount);
  end;
end;

procedure TSynFilteredMethodList.Remove(AHandler: TMethod);
begin
  Delete(IndexOf(AHandler));
end;

procedure TSynFilteredMethodList.Remove(AHandler: TMethod; AFilter: LongInt);
begin
  Delete(IndexOf(AHandler, AFilter));
end;

procedure TSynFilteredMethodList.CallNotifyEventsNumFilter(Sender: TObject; AFilter: LongInt);
var
  i: Integer;
begin
  i:=Count;
  while NextDownIndexNumFilter(i, AFilter) do
    TNotifyEvent(FItems[i].FHandler)(Sender);
end;

procedure TSynFilteredMethodList.CallNotifyEventsBitFilter(Sender: TObject; AFilter: LongInt);
var
  i: Integer;
begin
  i:=Count;
  while NextDownIndexBitFilter(i, AFilter) do
    TNotifyEvent(FItems[i].FHandler)(Sender);
end;

{ TSynSizedDifferentialAVLNode }

{$IFDEF SynDebug}
function TSynSizedDifferentialAVLNode.Debug: String;
begin
  Result := Format('Size=%3d (LeftSum=%3d)  Balance=%3d ',
                      [FSize,   FLeftSizeSum, FBalance]);
end;
{$ENDIF}

function TSynSizedDifferentialAVLNode.TreeDepth: integer;
var t: integer;
begin
  Result := 1;
  if FLeft <> nil  then Result := FLeft.TreeDepth+1;
  if FRight <> nil then t := FRight.TreeDepth+1 else t := 0;
  if t > Result then Result := t;
end;

procedure TSynSizedDifferentialAVLNode.SetLeftChild(ANode: TSynSizedDifferentialAVLNode);
begin
  FLeft := ANode;
  if ANode <> nil then ANode.FParent := self;
end;

procedure TSynSizedDifferentialAVLNode.SetLeftChild(ANode: TSynSizedDifferentialAVLNode;
  anAdjustChildPosOffset: Integer);
begin
  FLeft := ANode;
  if ANode <> nil then begin
    ANode.FParent := self;
    ANode.FPositionOffset := ANode.FPositionOffset + anAdjustChildPosOffset;
  end;
end;

procedure TSynSizedDifferentialAVLNode.SetLeftChild(ANode: TSynSizedDifferentialAVLNode;
  anAdjustChildPosOffset, aLeftSizeSum: Integer);
begin
  FLeft := ANode;
  FLeftSizeSum := aLeftSizeSum;
  if ANode <> nil then begin
    ANode.FParent := self;
    ANode.FPositionOffset := ANode.FPositionOffset + anAdjustChildPosOffset;
  end
end;

procedure TSynSizedDifferentialAVLNode.SetRightChild(ANode: TSynSizedDifferentialAVLNode);
begin
  FRight := ANode;
  if ANode <> nil then ANode.FParent := self;
end;

procedure TSynSizedDifferentialAVLNode.SetRightChild(ANode: TSynSizedDifferentialAVLNode;
  anAdjustChildPosOffset: Integer);
begin
  FRight := ANode;
  if ANode <> nil then begin
    ANode.FParent := self;
    ANode.FPositionOffset := ANode.FPositionOffset + anAdjustChildPosOffset;
  end;
end;

function TSynSizedDifferentialAVLNode.ReplaceChild(OldNode,
  ANode: TSynSizedDifferentialAVLNode): TReplacedChildSite;
begin
  if FLeft = OldNode then begin
    SetLeftChild(ANode);
    exit(rplcLeft);
  end;
  SetRightChild(ANode);
  result := rplcRight;
end;

function TSynSizedDifferentialAVLNode.ReplaceChild(OldNode,
  ANode: TSynSizedDifferentialAVLNode; anAdjustChildPosOffset: Integer): TReplacedChildSite;
begin
  if FLeft = OldNode then begin
    SetLeftChild(ANode, anAdjustChildPosOffset);
    exit(rplcLeft);
  end;
  SetRightChild(ANode, anAdjustChildPosOffset);
  result := rplcRight;
end;

procedure TSynSizedDifferentialAVLNode.AdjustLeftCount(AValue: Integer);
begin
  FLeftSizeSum := FLeftSizeSum + AValue;
  AdjustParentLeftCount(AValue);
end;

procedure TSynSizedDifferentialAVLNode.AdjustParentLeftCount(AValue: Integer);
var
  node, pnode : TSynSizedDifferentialAVLNode;
begin
  node := self;
  pnode := node.FParent;
  while pnode <> nil do begin
    if node = pnode.FLeft
    then pnode.FLeftSizeSum := pnode.FLeftSizeSum + AValue;
    node := pnode;
    pnode := node.FParent;
  end;
end;

function TSynSizedDifferentialAVLNode.GetSizesBeforeSum: Integer;
var
  n1, n2: TSynSizedDifferentialAVLNode;
begin
  Result := FLeftSizeSum;
  n1 := FParent;
  n2 := Self;
  while n1 <> nil do begin
    if n2 = n1.FRight then
      Result := Result + n1.FLeftSizeSum + n1.FSize;
    n2 := n1;
    n1 := n1.FParent;
  end;
end;

function TSynSizedDifferentialAVLNode.GetPosition: Integer;
var
  N: TSynSizedDifferentialAVLNode;
begin
  Result := FPositionOffset;
  N := FParent;
  while N <> nil do begin
    Result := Result + N.FPositionOffset;
    N := N.FParent;
  end;
end;

function TSynSizedDifferentialAVLNode.Precessor: TSynSizedDifferentialAVLNode;
begin
  Result := FLeft;
  if Result<>nil then begin
    while (Result.FRight<>nil) do Result := Result.FRight;
  end else begin
    Result := self;
    while (Result.FParent<>nil) and (Result.FParent.FLeft=Result) do
      Result := Result.FParent;
    Result := Result.FParent;
  end;
end;

function TSynSizedDifferentialAVLNode.Successor: TSynSizedDifferentialAVLNode;
begin
  Result := FRight;
  if Result<>nil then begin
    while (Result.FLeft<>nil) do Result := Result.FLeft;
  end else begin
    Result := self;
    while (Result.FParent<>nil) and (Result.FParent.FRight=Result) do
      Result := Result.FParent;
    Result := Result.FParent;
  end;
end;

function TSynSizedDifferentialAVLNode.Precessor(var aStartPosition,
  aSizesBeforeSum: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := FLeft;
  if Result<>nil then begin
    aStartPosition := aStartPosition + Result.FPositionOffset;
    while (Result.FRight<>nil) do begin
      Result := Result.FRight;
      aStartPosition := aStartPosition + Result.FPositionOffset;
    end;
  end else begin
    Result := self;
    while (Result.FParent<>nil) and (Result.FParent.FLeft=Result) do begin
      aStartPosition := aStartPosition - Result.FPositionOffset;
      Result := Result.FParent;
    end;
    // result is now a FRight son
    aStartPosition := aStartPosition - Result.FPositionOffset;
    Result := Result.FParent;
  end;
  if result <> nil then
    aSizesBeforeSum := aSizesBeforeSum - Result.FSize
  else
    aSizesBeforeSum := 0;
end;

function TSynSizedDifferentialAVLNode.Successor(var aStartPosition,
  aSizesBeforeSum: Integer): TSynSizedDifferentialAVLNode;
begin
  aSizesBeforeSum := aSizesBeforeSum + FSize;
  Result := FRight;
  if Result<>nil then begin
    aStartPosition := aStartPosition + Result.FPositionOffset;
    while (Result.FLeft<>nil) do begin
      Result := Result.FLeft;
      aStartPosition := aStartPosition + Result.FPositionOffset;
    end;
  end else begin
    Result := self;
    while (Result.FParent<>nil) and (Result.FParent.FRight=Result) do begin
      aStartPosition := aStartPosition - Result.FPositionOffset;
      Result := Result.FParent;
    end;
    // Result is now a FLeft son; result has a negative FPositionOffset
    aStartPosition := aStartPosition - Result.FPositionOffset;
    Result := Result.FParent;
  end;
end;

{ TSynSizedDifferentialAVLTree }

procedure TSynSizedDifferentialAVLTree.SetRoot(ANode: TSynSizedDifferentialAVLNode);
begin
  fRoot := ANode;
  if ANode <> nil then ANode.FParent := nil;
end;

procedure TSynSizedDifferentialAVLTree.SetRoot(ANode: TSynSizedDifferentialAVLNode;
  anAdjustChildPosOffset: Integer);
begin
  fRoot := ANode;
  if ANode <> nil then begin
    ANode.FParent := nil;
    ANode.FPositionOffset := ANode.FPositionOffset + anAdjustChildPosOffset;
  end;
end;

procedure TSynSizedDifferentialAVLTree.DisposeNode(var ANode: TSynSizedDifferentialAVLNode);
begin
  FreeAndNil(ANode);
end;

function TSynSizedDifferentialAVLTree.InsertNode(ANode: TSynSizedDifferentialAVLNode): Integer;
var
  current: TSynSizedDifferentialAVLNode;
  rStartPosition, rSizesBeforeSum: Integer;
  ALine, ACount: Integer;
begin
  if fRoot = nil then begin
    SetRoot(ANode, -fRootOffset);
    Result := 0;
    exit;
  end;

  ALine := ANode.FPositionOffset;
  ACount := ANode.FSize;

  current := fRoot;
  rStartPosition := fRootOffset;
  rSizesBeforeSum := 0;

  while (current <> nil) do begin
    rStartPosition := rStartPosition + current.FPositionOffset;

    if ALine < rStartPosition then begin
      (* *** New block goes to the Fleft *** *)
      if current.FLeft <> nil Then begin
        current := current.FLeft;
        continue;
      end
      else begin // insert as FLeft
        current.AdjustParentLeftCount(ACount);
        current.SetLeftChild(ANode, -rStartPosition, ANode.FSize);
        BalanceAfterInsert(ANode);
        break;
      end;
    end;

    rSizesBeforeSum := rSizesBeforeSum + current.FLeftSizeSum;

    if ALine = rStartPosition then begin
      // Should not happen // did happen when nodes with 0 lines where re-inserrted, after editor-delete-lines
      debugln(['Droping Foldnode / Already exists. Startline=', rStartPosition,' LineCount=',ACount]);
      FreeAndNil(ANode);
      break;
    end

    else begin
      rSizesBeforeSum := rSizesBeforeSum + current.FSize;
      if current.FRight <> nil then begin
        current := current.FRight;
        continue;
      end
      else begin  // insert to the Fright - no nesting
        current.AdjustParentLeftCount(ACount);
        current.SetRightChild(ANode, -rStartPosition);
        BalanceAfterInsert(ANode);
        break;
      end;
    end;
  end; // while

  Result := rSizesBeforeSum;
end;

procedure TSynSizedDifferentialAVLTree.RemoveNode(ANode: TSynSizedDifferentialAVLNode);
var OldParent, Precessor, PrecOldParent, PrecOldLeft,
  OldSubTree: TSynSizedDifferentialAVLNode;
  OldBalance, PrecOffset, PrecLeftCount: integer;

begin
  if ((ANode.FLeft<>nil) and (ANode.FRight<>nil)) then begin
    PrecOffset := 0;
//    PrecOffset := ANode.FPositionOffset;
    Precessor := ANode.FLeft;
    while (Precessor.FRight<>nil) do begin
      PrecOffset := PrecOffset + Precessor.FPositionOffset;
      Precessor := Precessor.FRight;
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
    OldBalance := ANode.FBalance;
    ANode.FBalance     := Precessor.FBalance;
    Precessor.FBalance := OldBalance;

    // Successor.FLeft = nil
    PrecOldLeft   := Precessor.FLeft;
    PrecOldParent := Precessor.FParent;

    if (ANode.FParent<>nil)
    then ANode.FParent.ReplaceChild(ANode, Precessor, PrecOffset + ANode.FPositionOffset)
    else SetRoot(Precessor, PrecOffset + ANode.FPositionOffset);

    Precessor.SetRightChild(ANode.FRight,
                           +ANode.FPositionOffset-Precessor.FPositionOffset);

    PrecLeftCount := Precessor.FLeftSizeSum;
    // ANode.FRight will be empty  // ANode.FLeft will be Succesor.FLeft
    if (PrecOldParent = ANode) then begin
      // Precessor is Fleft son of ANode
      // set ANode.FPositionOffset=0 => FPositionOffset for the Prec-Children is already correct;
      Precessor.SetLeftChild(ANode, -ANode.FPositionOffset,
                             PrecLeftCount + ANode.FSize);
      ANode.SetLeftChild(PrecOldLeft, 0, PrecLeftCount);
    end else begin
      // at least one node between ANode and Precessor ==> Precessor = PrecOldParent.FRight
      Precessor.SetLeftChild(ANode.FLeft, +ANode.FPositionOffset - Precessor.FPositionOffset,
                             ANode.FLeftSizeSum + ANode.FSize - Precessor.FSize);
      PrecOffset:=PrecOffset + ANode.FPositionOffset - Precessor.FPositionOffset;
      // Set Anode.FPositionOffset, so ANode movesinto position of Precessor;
      PrecOldParent.SetRightChild(ANode, - ANode.FPositionOffset -  PrecOffset);
      ANode.SetLeftChild(PrecOldLeft, 0, PrecLeftCount);
    end;

    ANode.FRight := nil;
  end;

  if (ANode.FRight<>nil) then begin
    OldSubTree := ANode.FRight;
    ANode.FRight := nil;
  end
  else if (ANode.FLeft<>nil) then begin
    OldSubTree := ANode.FLeft;
    ANode.FLeft := nil;
  end
  else OldSubTree := nil;

  OldParent := ANode.FParent;
  ANode.FParent := nil;
  ANode.FLeft := nil;
  ANode.FRight := nil;
  ANode.FBalance := 0;
  ANode.FLeftSizeSum := 0;
  // nested???

  if (OldParent<>nil) then begin      // Node has Fparent
    if OldParent.ReplaceChild(ANode, OldSubTree, ANode.FPositionOffset) = rplcLeft
    then begin
      Inc(OldParent.FBalance);
      OldParent.AdjustLeftCount(-ANode.FSize);
    end
    else begin
      Dec(OldParent.FBalance);
      OldParent.AdjustParentLeftCount(-ANode.FSize);
    end;
    BalanceAfterDelete(OldParent);
  end
  else SetRoot(OldSubTree, ANode.FPositionOffset);
end;

procedure TSynSizedDifferentialAVLTree.BalanceAfterInsert(ANode: TSynSizedDifferentialAVLNode);
var
  OldParent, OldParentParent, OldRight, OldRightLeft, OldRightRight, OldLeft,
  OldLeftLeft, OldLeftRight: TSynSizedDifferentialAVLNode;
  tmp : integer;
begin
  OldParent := ANode.FParent;
  if (OldParent=nil) then exit;

  if (OldParent.FLeft=ANode) then begin
    (* *** Node is left son *** *)
    dec(OldParent.FBalance);
    if (OldParent.FBalance=0) then exit;
    if (OldParent.FBalance=-1) then begin
      BalanceAfterInsert(OldParent);
      exit;
    end;

    // OldParent.FBalance=-2
    if (ANode.FBalance=-1) then begin
      (* ** single rotate ** *)
      (*  []
           \
           []  ORight                     []    ORight    []
            \   /                          \      \       /
            ANode(-1)  []        =>        []     OldParent(0)
               \       /                    \     /
               OldParent(-2)                 ANode(0)
      *)
      OldRight := ANode.FRight;
      OldParentParent := OldParent.FParent;
      (* ANode moves into position of OldParent *)
      if (OldParentParent<>nil)
      then OldParentParent.ReplaceChild(OldParent, ANode, OldParent.FPositionOffset)
      else SetRoot(ANode, OldParent.FPositionOffset);

      (* OldParent moves under ANode, replacing Anode.FRight, which moves under OldParent *)
      ANode.SetRightChild(OldParent, -ANode.FPositionOffset );
      OldParent.SetLeftChild(OldRight, -OldParent.FPositionOffset, OldParent.FLeftSizeSum - ANode.FSize - ANode.FLeftSizeSum);

      ANode.FBalance := 0;
      OldParent.FBalance := 0;
      (* ** END single rotate ** *)
    end
    else begin  // ANode.FBalance = +1
      (* ** double rotate ** *)
      OldParentParent := OldParent.FParent;
      OldRight := ANode.FRight;
      OldRightLeft := OldRight.FLeft;
      OldRightRight := OldRight.FRight;

      (* OldRight moves into position of OldParent *)
      if (OldParentParent<>nil)
      then OldParentParent.ReplaceChild(OldParent, OldRight, OldParent.FPositionOffset + ANode.FPositionOffset)
      else SetRoot(OldRight, OldParent.FPositionOffset + ANode.FPositionOffset);        // OldParent was root node. new root node

      OldRight.SetRightChild(OldParent, -OldRight.FPositionOffset);
      OldRight.SetLeftChild(ANode, OldParent.FPositionOffset, OldRight.FLeftSizeSum + ANode.FLeftSizeSum + ANode.FSize);
      ANode.SetRightChild(OldRightLeft, -ANode.FPositionOffset);
      OldParent.SetLeftChild(OldRightRight, -OldParent.FPositionOffset, OldParent.FLeftSizeSum - OldRight.FLeftSizeSum - OldRight.FSize);

      // balance
      if (OldRight.FBalance<=0)
      then ANode.FBalance := 0
      else ANode.FBalance := -1;
      if (OldRight.FBalance=-1)
      then OldParent.FBalance := 1
      else OldParent.FBalance := 0;
      OldRight.FBalance := 0;
      (* ** END double rotate ** *)
    end;
    (* *** END Node is left son *** *)
  end
  else begin
    (* *** Node is right son *** *)
    Inc(OldParent.FBalance);
    if (OldParent.FBalance=0) then exit;
    if (OldParent.FBalance=+1) then begin
      BalanceAfterInsert(OldParent);
      exit;
    end;

    // OldParent.FBalance = +2
    if(ANode.FBalance=+1) then begin
      (* ** single rotate ** *)
      OldLeft := ANode.FLeft;
      OldParentParent := OldParent.FParent;

      if (OldParentParent<>nil)
      then  OldParentParent.ReplaceChild(OldParent, ANode, OldParent.FPositionOffset)
      else SetRoot(ANode, OldParent.FPositionOffset);

      (* OldParent moves under ANode, replacing Anode.FLeft, which moves under OldParent *)
      ANode.SetLeftChild(OldParent, -ANode.FPositionOffset, ANode.FLeftSizeSum + OldParent.FSize + OldParent.FLeftSizeSum);
      OldParent.SetRightChild(OldLeft, -OldParent.FPositionOffset);

      ANode.FBalance := 0;
      OldParent.FBalance := 0;
      (* ** END single rotate ** *)
    end
    else begin  // Node.Balance = -1
      (* ** double rotate ** *)
      OldLeft := ANode.FLeft;
      OldParentParent := OldParent.FParent;
      OldLeftLeft := OldLeft.FLeft;
      OldLeftRight := OldLeft.FRight;

      (* OldLeft moves into position of OldParent *)
      if (OldParentParent<>nil)
      then  OldParentParent.ReplaceChild(OldParent, OldLeft, OldParent.FPositionOffset + ANode.FPositionOffset)
      else SetRoot(OldLeft, OldParent.FPositionOffset + ANode.FPositionOffset);

      tmp := OldLeft.FLeftSizeSum;
      OldLeft.SetLeftChild (OldParent, -OldLeft.FPositionOffset, tmp + OldParent.FLeftSizeSum + OldParent.FSize);
      OldLeft.SetRightChild(ANode, OldParent.FPositionOffset);

      OldParent.SetRightChild(OldLeftLeft, -OldParent.FPositionOffset);
      ANode.SetLeftChild(OldLeftRight, -ANode.FPositionOffset, ANode.FLeftSizeSum - tmp - OldLeft.FSize);

      // Balance
      if (OldLeft.FBalance>=0)
      then ANode.FBalance := 0
      else ANode.FBalance := +1;
      if (OldLeft.FBalance=+1)
      then OldParent.FBalance := -1
      else OldParent.FBalance := 0;
      OldLeft.FBalance := 0;
      (* ** END double rotate ** *)
    end;
  end;
end;

procedure TSynSizedDifferentialAVLTree.BalanceAfterDelete(ANode: TSynSizedDifferentialAVLNode);
var
  OldParent, OldRight, OldRightLeft, OldLeft, OldLeftRight,
  OldRightLeftLeft, OldRightLeftRight, OldLeftRightLeft, OldLeftRightRight: TSynSizedDifferentialAVLNode;
  tmp: integer;
begin
  if (ANode=nil) then exit;
  if ((ANode.FBalance=+1) or (ANode.FBalance=-1)) then exit;
  OldParent := ANode.FParent;
  if (ANode.FBalance=0) then begin
    // Treeheight has decreased by one
    if (OldParent<>nil) then begin
      if(OldParent.FLeft=ANode) then
        Inc(OldParent.FBalance)
      else
        Dec(OldParent.FBalance);
      BalanceAfterDelete(OldParent);
    end;
    exit;
  end;

  if (ANode.FBalance=-2) then begin
    // Node.Balance=-2
    // Node is overweighted to the left
    (*
          OLftRight
           /
        OLeft(<=0)
           \
             ANode(-2)
    *)
    OldLeft := ANode.FLeft;
    if (OldLeft.FBalance<=0) then begin
      // single rotate left
      OldLeftRight := OldLeft.FRight;

      if (OldParent<>nil)
      then OldParent.ReplaceChild(ANode, OldLeft, ANode.FPositionOffset)
      else SetRoot(OldLeft, ANode.FPositionOffset);

      OldLeft.SetRightChild(ANode, -OldLeft.FPositionOffset);
      ANode.SetLeftChild(OldLeftRight, -ANode.FPositionOffset, ANode.FLeftSizeSum - OldLeft.FSize - OldLeft.FLeftSizeSum);

      ANode.FBalance := (-1-OldLeft.FBalance);
      Inc(OldLeft.FBalance);

      BalanceAfterDelete(OldLeft);
    end else begin
      // OldLeft.FBalance = 1
      // double rotate left left
      OldLeftRight := OldLeft.FRight;
      OldLeftRightLeft := OldLeftRight.FLeft;
      OldLeftRightRight := OldLeftRight.FRight;

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
      then OldParent.ReplaceChild(ANode, OldLeftRight, ANode.FPositionOffset + OldLeft.FPositionOffset)
      else SetRoot(OldLeftRight, ANode.FPositionOffset + OldLeft.FPositionOffset);

      OldLeftRight.SetRightChild(ANode, -OldLeftRight.FPositionOffset);
      OldLeftRight.SetLeftChild(OldLeft, ANode.FPositionOffset, OldLeftRight.FLeftSizeSum + OldLeft.FLeftSizeSum + OldLeft.FSize);
      OldLeft.SetRightChild(OldLeftRightLeft, -OldLeft.FPositionOffset);
      ANode.SetLeftChild(OldLeftRightRight,  -ANode.FPositionOffset, ANode.FLeftSizeSum - OldLeftRight.FLeftSizeSum - OldLeftRight.FSize);

      if (OldLeftRight.FBalance<=0)
      then OldLeft.FBalance := 0
      else OldLeft.FBalance := -1;
      if (OldLeftRight.FBalance>=0)
      then ANode.FBalance := 0
      else ANode.FBalance := +1;
      OldLeftRight.FBalance := 0;

      BalanceAfterDelete(OldLeftRight);
    end;
  end else begin
    // Node is overweighted to the right
    OldRight := ANode.FRight;
    if (OldRight.FBalance>=0) then begin
      // OldRight.FBalance=={0 or -1}
      // single rotate right
      OldRightLeft := OldRight.FLeft;

      if (OldParent<>nil)
      then OldParent.ReplaceChild(ANode, OldRight, ANode.FPositionOffset)
      else SetRoot(OldRight, ANode.FPositionOffset);

      OldRight.SetLeftChild(ANode, -OldRight.FPositionOffset, OldRight.FLeftSizeSum + ANode.FSize + ANode.FLeftSizeSum);
      ANode.SetRightChild(OldRightLeft, -ANode.FPositionOffset);

      ANode.FBalance := (1-OldRight.FBalance);
      Dec(OldRight.FBalance);

      BalanceAfterDelete(OldRight);
    end else begin
      // OldRight.FBalance=-1
      // double rotate right left
      OldRightLeft := OldRight.FLeft;
      OldRightLeftLeft := OldRightLeft.FLeft;
      OldRightLeftRight := OldRightLeft.FRight;
      if (OldParent<>nil)
      then OldParent.ReplaceChild(ANode, OldRightLeft, ANode.FPositionOffset + OldRight.FPositionOffset)
      else SetRoot(OldRightLeft, ANode.FPositionOffset + OldRight.FPositionOffset);

      tmp := OldRightLeft.FLeftSizeSum;
      OldRightLeft.SetLeftChild(ANode, -OldRightLeft.FPositionOffset, tmp + ANode.FLeftSizeSum + ANode.FSize);
      OldRightLeft.SetRightChild(OldRight, ANode.FPositionOffset);

      ANode.SetRightChild(OldRightLeftLeft, -ANode.FPositionOffset);
      OldRight.SetLeftChild(OldRightLeftRight, -OldRight.FPositionOffset, OldRight.FLeftSizeSum - tmp - OldRightLeft.FSize);

      if (OldRightLeft.FBalance<=0)
      then ANode.FBalance := 0
      else ANode.FBalance := -1;
      if (OldRightLeft.FBalance>=0)
      then OldRight.FBalance := 0
      else OldRight.FBalance := +1;
      OldRightLeft.FBalance := 0;
      BalanceAfterDelete(OldRightLeft);
    end;
  end;
end;

function TSynSizedDifferentialAVLTree.CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := TSynSizedDifferentialAVLNode.Create;
end;

constructor TSynSizedDifferentialAVLTree.Create;
begin
  inherited;
  fRoot := nil;
  fRootOffset := 0;
end;

destructor TSynSizedDifferentialAVLTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{$IFDEF SynDebug}
procedure TSynSizedDifferentialAVLTree.Debug;
  function debug2(ind, typ : String; ANode, AParent : TSynSizedDifferentialAVLNode; offset : integer) :integer;
  begin
    result := 0;
    if ANode = nil then exit;
    with ANode do
      DebugLn([Format('%-14s - Pos=%3d (offs=%3d)  %s',
                      [ind + typ,
                       offset + ANode.FPositionOffset,   ANode.FPositionOffset,
                       ANode.Debug])
              ]);
    if ANode.FParent <> AParent then DebugLn([ind,'* Bad parent']);

    Result := debug2(ind+'  ', 'L', ANode.FLeft, ANode, offset+ANode.FPositionOffset);
    If Result <> ANode.FLeftSizeSum then  debugln([ind,'   ***** Leftcount was ',Result, ' but should be ', ANode.FLeftSizeSum]);
    Result := Result + debug2(ind+'  ', 'R', ANode.FRight, ANode, offset+ANode.FPositionOffset);
    Result := Result + ANode.FSize;
  end;
begin
  debug2('', '**', fRoot, nil, 0);
end;
{$ENDIF}

procedure TSynSizedDifferentialAVLTree.Clear;
  procedure DeleteNode(var ANode: TSynSizedDifferentialAVLNode);
  begin
    if ANode.FLeft  <> nil then DeleteNode(ANode.FLeft);
    if ANode.FRight <> nil then DeleteNode(ANode.FRight);
    DisposeNode(ANode);
  end;
begin
  if FRoot <> nil then DeleteNode(FRoot);
  SetRoot(nil);
end;

function TSynSizedDifferentialAVLTree.First: TSynSizedDifferentialAVLNode;
begin
  Result := FRoot;
  if Result = nil then
    exit;
  while Result.FLeft <> nil do
    Result := Result.FLeft;
end;

function TSynSizedDifferentialAVLTree.Last: TSynSizedDifferentialAVLNode;
begin
  Result := FRoot;
  if Result = nil then
    exit;
  while Result.FRight <> nil do
    Result := Result.FRight;
end;

function TSynSizedDifferentialAVLTree.First(out aStartPosition,
  aSizesBeforeSum: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := FRoot;
  aStartPosition := 0;
  aSizesBeforeSum := 0;
  if Result = nil then
    exit;

  aStartPosition := Result.FPositionOffset;
  while Result.FLeft <> nil do begin
    Result := Result.FLeft;
    aStartPosition := aStartPosition + Result.FPositionOffset;
  end;
end;

function TSynSizedDifferentialAVLTree.Last(out aStartPosition,
  aSizesBeforeSum: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := FRoot;
  aStartPosition := 0;
  aSizesBeforeSum := 0;
  if Result = nil then
    exit;

  aStartPosition := Result.FPositionOffset;
  while Result.FRight <> nil do begin
    aSizesBeforeSum := aSizesBeforeSum + Result.FLeftSizeSum + Result.FSize;
    Result := Result.FRight;
    aStartPosition := aStartPosition + Result.FPositionOffset;
  end;
end;

function TSynSizedDifferentialAVLTree.FindNodeAtLeftSize(ALeftSum: INteger; out
  aStartPosition, aSizesBeforeSum: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := FRoot;
  aStartPosition := 0;
  aSizesBeforeSum := 0;
  if Result = nil then
    exit;

  aStartPosition := Result.FPositionOffset;
  while Result <> nil do begin
    if ALeftSum < Result.FLeftSizeSum then begin
      Result := Result.FLeft;
      aStartPosition := aStartPosition + Result.FPositionOffset;
      continue;
    end;

    ALeftSum := ALeftSum - Result.FLeftSizeSum;
    aSizesBeforeSum := aSizesBeforeSum + Result.FLeftSizeSum;
    if ALeftSum < Result.FSize then begin
      break;
    end
    else begin
      ALeftSum := ALeftSum - Result.FSize;
      aSizesBeforeSum := aSizesBeforeSum + Result.FSize;
      Result := Result.FRight;
      aStartPosition := aStartPosition + Result.FPositionOffset;
      continue;
    end;
  end;
end;

function TSynSizedDifferentialAVLTree.FindNodeAtPosition(APosition: INteger;
  AMode: TSynSizedDiffAVLFindMode; out aStartPosition,
  aSizesBeforeSum: Integer): TSynSizedDifferentialAVLNode;
var
  NxtPrv: Array [0..1] of TSynSizedDifferentialAVLNode;
  NxtPrvBefore, NxtPrvPos: Array [0..1] of Integer;

  procedure Store(i: integer; N: TSynSizedDifferentialAVLNode); inline;
  begin
    NxtPrv[i] := N;
    NxtPrvBefore[i] := aSizesBeforeSum;
    NxtPrvPos[i]    := aStartPosition;
  end;

  function Restore(i: integer): TSynSizedDifferentialAVLNode; inline;
  begin
    Result := NxtPrv[i];
    aSizesBeforeSum := NxtPrvBefore[i];
    aStartPosition  := NxtPrvPos[i];
  end;

  function CreateRoot: TSynSizedDifferentialAVLNode; inline;
  begin
    Result := CreateNode(APosition);
    Result.FPositionOffset := APosition;
    SetRoot(Result);
  end;

  function CreateLeft(N: TSynSizedDifferentialAVLNode; ACurOffs: Integer): TSynSizedDifferentialAVLNode; inline;
  begin
    Result := CreateNode(APosition);
    Result.FPositionOffset := APosition;
    N.SetLeftChild(Result, -ACurOffs);
    BalanceAfterInsert(Result);
    aStartPosition := APosition;
    aSizesBeforeSum := Result.GetSizesBeforeSum;
  end;

  function CreateRight(N: TSynSizedDifferentialAVLNode; ACurOffs: Integer): TSynSizedDifferentialAVLNode; inline;
  begin
    Result := CreateNode(APosition);
    Result.FPositionOffset := APosition;
    N.SetRightChild(Result, -ACurOffs);
    BalanceAfterInsert(Result);
    aStartPosition := APosition;
    aSizesBeforeSum := Result.GetSizesBeforeSum;
  end;

begin
  aSizesBeforeSum := 0;
  aStartPosition := 0;
  Store(0, nil);
  Store(1, nil);
  aStartPosition := fRootOffset;
  Result := FRoot;
  if (Result = nil) then begin
    if (AMode = afmCreate) then begin
      Result := CreateRoot;
      aStartPosition := aStartPosition + Result.FPositionOffset;
    end;
    exit;
  end;

  while (Result <> nil) do begin
    aStartPosition := aStartPosition + Result.FPositionOffset;

    if aStartPosition > APosition then begin
      if (Result.FLeft = nil) then begin
        case AMode of
          afmCreate: Result := CreateLeft(Result, aStartPosition);
          afmNil:    Result := nil;
          afmPrev:   Result := Restore(0); // Precessor
          //afmNext:   Result := ; //already contains next node
        end;
        break;
      end;
      Store(1, Result); // Successor
      Result := Result.FLeft;
    end

    else
    if APosition = aStartPosition then begin
      break;
    end

    else
    if aStartPosition < APosition then begin
      if (Result.FRight = nil) then begin
        case AMode of
          afmCreate: Result := CreateRight(Result, aStartPosition);
          afmNil:    Result := nil;
          afmNext:   Result := Restore(1); // Successor
          //afmPrev :  Result := ; //already contains prev node
        end;
        break;
      end;
      Store(0, Result); // Precessor
      aSizesBeforeSum := aSizesBeforeSum + Result.FLeftSizeSum + Result.FSize;
      Result := Result.FRight;
    end;
  end; // while
end;

procedure TSynSizedDifferentialAVLTree.AdjustForLinesInserted(AStartLine, ALineCount: Integer);
var
  Current: TSynSizedDifferentialAVLNode;
  CurrentLine: Integer;
begin
  Current := TSynSizedDifferentialAVLNode(fRoot);
  CurrentLine := FRootOffset;
  while (Current <> nil) do begin
    CurrentLine := CurrentLine + Current.FPositionOffset;

    if AStartLine <= CurrentLine then begin
      // move current node
      Current.FPositionOffset := Current.FPositionOffset + ALineCount;
      CurrentLine := CurrentLine + ALineCount;
      if Current.FLeft <> nil then
        Current.FLeft.FPositionOffset := Current.FLeft.FPositionOffset - ALineCount;
      Current := Current.FLeft;
    end
    else if AStartLine > CurrentLine then begin
      // The new lines are entirly behind the current node
      Current := Current.FRight;
    end
  end;
end;

procedure TSynSizedDifferentialAVLTree.AdjustForLinesDeleted(AStartLine, ALineCount: Integer);
var
  Current : TSynSizedDifferentialAVLNode;
  CurrentLine, LastLineToDelete: Integer;
begin
  Current := TSynSizedDifferentialAVLNode(fRoot);
  CurrentLine := FRootOffset;;
  LastLineToDelete := AStartLine + ALineCount - 1; // only valid for delete; ALineCount < 0

  while (Current <> nil) do begin
    CurrentLine := CurrentLine + Current.FPositionOffset;

    if (AStartLine = CurrentLine) or
       ((AStartLine < CurrentLine) and (LastLineToDelete >= CurrentLine)) then begin
      { $IFDEF AssertSynMemIndex}
      raise Exception.Create('TSynEditMarkLineList.AdjustForLinesDeleted node to remove');
      { $ENDIF}
    end

    else if AStartLine < CurrentLine then begin
      // move current node (includes Fright subtree / Fleft subtree needs eval)
      Current.FPositionOffset := Current.FPositionOffset - ALineCount;
      CurrentLine := CurrentLine - ALineCount;

      Current := Current.FLeft;
      if Current <> nil then
        Current.FPositionOffset := Current.FPositionOffset + ALineCount;
    end

    else if AStartLine > CurrentLine then begin
      // The deleted lines are entirly behind the current node
      Current := Current.FRight;
    end;
  end;
end;

end.


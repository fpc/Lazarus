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
  LCLIntf, LCLType, LCLProc,
  Classes, Graphics, Controls, SysUtils, Clipbrd, SynEditHighlighter,
  SynEditMiscProcs, SynEditTypes, LazSynEditText, SynEditPointClasses;

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
                           ): Boolean;
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

    function NextBoundary (aLine: String; aX: Integer): Integer;
    function PrevBoundary (aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;

    property IdentChars: TSynIdentChars read FIdentChars write SetIdentChars;
    property WordChars: TSynIdentChars read FWordChars;
    property WordBreakChars: TSynIdentChars read FWordBreakChars write SetWordBreakChars;
    property WhiteChars: TSynIdentChars read FWhiteChars write SetWhiteChars;
  end;

  { TSynEditBase }

  TSynEditBase = class(TCustomControl)
  protected
    FWordBreaker: TSynWordBreaker;
    FBlockSelection: TSynEditSelection;
    function GetMarkupMgr: TObject; virtual; abstract;
    function GetLines: TStrings; virtual; abstract;
    function GetCaretObj: TSynEditCaret; virtual; abstract;
    procedure SetLines(Value: TStrings); virtual; abstract;
    function GetViewedTextBuffer: TSynEditStrings; virtual; abstract;
    function GetFoldedTextBuffer: TObject; virtual; abstract;
    function GetTextBuffer: TSynEditStrings; virtual; abstract;

    property MarkupMgr: TObject read GetMarkupMgr;
    property FoldedTextBuffer: TObject read GetFoldedTextBuffer;                // TSynEditFoldedView
    property ViewedTextBuffer: TSynEditStrings read GetViewedTextBuffer;        // As viewed internally (with uncommited spaces / TODO: expanded tabs, folds). This may change, use with care
    property TextBuffer: TSynEditStrings read GetTextBuffer;                    // (TSynEditStringList) No uncommited (trailing/trimmable) spaces
    property WordBreaker: TSynWordBreaker read FWordBreaker;
  public
    property Lines: TStrings read GetLines write SetLines;
  end;

  { TSynEditFriend }

  TSynEditFriend = class(TComponent)
  private
    FFriendEdit: TSynEditBase;
    function GetCaretObj: TSynEditCaret;
    function GetFoldedTextBuffer: TObject;
    function GetIsRedoing: Boolean;
    function GetIsUndoing: Boolean;
    function GetMarkupMgr: TObject;
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
    property SelectionObj: TSynEditSelection read GetSelectionObj;
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

  { TSynSelectedColor }

  TSynSelectedColor = class(TLazSynCustomTextAttributes)
  private
    FCurrentEndX: Integer;
    FCurrentStartX: Integer;
    FOnChange: TNotifyEvent;
    // 0 or -1 start/end before/after line // 1 first char
    FStartX, FEndX: Integer;
    FFrameSidesInitialized: Boolean;
    FFrameSideColors: array[TLazSynBorderSide] of TColor;
    FFrameSideStyles: array[TLazSynBorderSide] of TSynLineStyle;
    FFrameSidePriority: array[TLazSynBorderSide] of Integer;
    FFrameSideOrigin: array[TLazSynBorderSide] of TSynFrameEdges;
    function GetFrameSideColors(Side: TLazSynBorderSide): TColor;
    function GetFrameSideOrigin(Side: TLazSynBorderSide): TSynFrameEdges;
    function GetFrameSidePriority(Side: TLazSynBorderSide): integer;
    function GetFrameSideStyles(Side: TLazSynBorderSide): TSynLineStyle;
  protected
    procedure DoChange; override;
    procedure AssignFrom(Src: TLazSynCustomTextAttributes); override;
    property FrameSidePriority[Side: TLazSynBorderSide]: integer read GetFrameSidePriority;
    property FrameSideOrigin[Side: TLazSynBorderSide]: TSynFrameEdges read GetFrameSideOrigin;
  public
    // TSynSelectedColor.Style and StyleMask describe how to modify a style,
    // but PaintLines creates an instance that contains an actual style (without mask)
    // Todo: always start with actual style
    MergeFinalStyle: Boolean;
    procedure Merge(Other: TSynSelectedColor; LeftCol, RightCol: Integer); deprecated;
    procedure MergeFrames(Other: TSynSelectedColor; LeftCol, RightCol: Integer); deprecated;
    property FrameSideColors[Side: TLazSynBorderSide]: TColor read GetFrameSideColors;
    property FrameSideStyles[Side: TLazSynBorderSide]: TSynLineStyle read GetFrameSideStyles;
    property StartX: Integer read FStartX write FStartX;
    property EndX: Integer read FEndX write FEndX;
    property CurrentStartX: Integer read FCurrentStartX write FCurrentStartX;
    property CurrentEndX: Integer read FCurrentEndX write FCurrentEndX;
  public
    constructor Create;
    procedure Clear; override;
    function IsEnabled: boolean;
    function GetModifiedStyle(aStyle: TFontStyles): TFontStyles;
    procedure ModifyColors(var AForeground, ABackground, AFrameColor: TColor;
      var AStyle: TFontStyles; var AFrameStyle: TSynLineStyle);
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Background default clHighLight;
    property Foreground default clHighLightText;
    property FrameColor default clNone;
    property FrameStyle default slsSolid;
    property FrameEdges default sfeAround;
    // FStyle = [],       FStyleMask = []        ==> no modification
    // FStyle = [fsBold], FStyleMask = []        ==> invert fsBold
    // FStyle = [],       FStyleMask = [fsBold]  ==> clear  fsBold
    // FStyle = [fsBold], FStyleMask = [fsBold]  ==> set    fsBold
    property Style default [];
    property StyleMask default [];
    property BackPriority default 0;
    property ForePriority default 0;
    property FramePriority default 0;
    property BoldPriority default 0;
    property ItalicPriority default 0;
    property UnderlinePriority default 0;
  end;

  { TSynBookMarkOpt }

  TSynBookMarkOpt = class(TPersistent)
  private
    fBookmarkImages: TImageList;
    fDrawBookmarksFirst: boolean;                                               //mh 2000-10-12
    fEnableKeys: Boolean;
    fGlyphsVisible: Boolean;
    fLeftMargin: Integer;
    fOwner: TComponent;
    fXoffset: integer;
    fOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TImageList);
    procedure SetDrawBookmarksFirst(Value: boolean);                            //mh 2000-10-12
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: integer);
  public
    constructor Create(AOwner: TComponent);
  published
    property BookmarkImages: TImageList
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

    (* Position:  sores difference to parent value
    *)
    FPositionOffset: Integer;

    (* Size:  Each node can have a Size, or similar value.
              LeftSizeSum is the Sum of all sizes on the Left. This allows to quickly
              calculate the sum of all preceding nodes together
    *)
    FSize: Integer;
    FLeftSizeSum: Integer;

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
    function GetSizesBeforeSum: Integer;

    function Precessor: TSynSizedDifferentialAVLNode;
    function Successor: TSynSizedDifferentialAVLNode;
    function Precessor(var aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;
    function Successor(var aStartPosition, aSizesBeforeSum : Integer): TSynSizedDifferentialAVLNode;
  end;

  { TSynSizedDifferentialAVLTree }

  TSynSizedDifferentialAVLTree = class
  protected
    FRoot: TSynSizedDifferentialAVLNode;
    FRootOffset : Integer; // Always 0, unless subclassed with nested trees

    // SetRoot, does not obbey fRootOffset => use SetRoot(node, -fRootOffset)
    procedure SetRoot(ANode : TSynSizedDifferentialAVLNode); virtual; overload;
    procedure SetRoot(ANode : TSynSizedDifferentialAVLNode; anAdjustChildPosOffset : Integer); virtual; overload;

    procedure DisposeNode(var ANode: TSynSizedDifferentialAVLNode); inline;

    function  InsertNode(ANode : TSynSizedDifferentialAVLNode) : Integer; // returns FoldedBefore // ANode may not have children
    procedure RemoveNode(ANode: TSynSizedDifferentialAVLNode);
    procedure BalanceAfterInsert(ANode: TSynSizedDifferentialAVLNode);
    procedure BalanceAfterDelete(ANode: TSynSizedDifferentialAVLNode);
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

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited Create;
  MergeFinalStyle := False;
  Background := clHighLight;
  Foreground := clHighLightText;
end;

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

function TSynSelectedColor.GetFrameSideColors(Side: TLazSynBorderSide): TColor;
begin
  if FFrameSidesInitialized
  then Result := FFrameSideColors[Side]
  else
  if (Side in SynFrameEdgeToSides[FrameEdges]) and (
      (Side in [bsTop, bsBottom]) or
      ( (Side = bsLeft)  and (FCurrentStartX = FStartX) ) or
      ( (Side = bsRight) and (FCurrentEndX = FEndX) )
     )
  then Result := FrameColor
  else Result := clNone;
end;

function TSynSelectedColor.GetFrameSideOrigin(Side: TLazSynBorderSide): TSynFrameEdges;
begin
  if FFrameSidesInitialized
  then Result := FFrameSideOrigin[Side]
  else if FrameColor = clNone
  then Result := sfeNone
  else Result := FrameEdges;
end;

function TSynSelectedColor.GetFrameSidePriority(Side: TLazSynBorderSide): integer;
begin
  if FFrameSidesInitialized
  then Result := FFrameSidePriority[Side]
  else
  if (Side in SynFrameEdgeToSides[FrameEdges]) and (
      (Side in [bsTop, bsBottom]) or
      ( (Side = bsLeft)  and (FCurrentStartX = FStartX) ) or
      ( (Side = bsRight) and (FCurrentEndX = FEndX) )
     )
  then Result := FramePriority
  else Result := 0;
end;

function TSynSelectedColor.GetFrameSideStyles(Side: TLazSynBorderSide): TSynLineStyle;
begin
  if FFrameSidesInitialized
  then Result := FFrameSideStyles[Side]
  else
  if Side in SynFrameEdgeToSides[FrameEdges]
  then Result := FrameStyle
  else Result := slsSolid;
end;

procedure TSynSelectedColor.DoChange;
begin
  if Assigned(FOnChange) then
    OnChange(Self);
end;

procedure TSynSelectedColor.AssignFrom(Src: TLazSynCustomTextAttributes);
var
  i: TLazSynBorderSide;
begin
  FFrameSidesInitialized := False;

  inherited AssignFrom(Src);
  if not (Src is TSynSelectedColor) then exit;

  FStartX := TSynSelectedColor(Src).FStartX;
  FEndX   := TSynSelectedColor(Src).FEndX;
  FCurrentStartX := TSynSelectedColor(Src).FCurrentStartX;
  FCurrentEndX   := TSynSelectedColor(Src).FCurrentEndX;
  FFrameSidesInitialized := TSynSelectedColor(Src).FFrameSidesInitialized;

  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
    FFrameSideColors[i] := TSynSelectedColor(Src).FFrameSideColors[i];
    FFrameSideStyles[i] := TSynSelectedColor(Src).FFrameSideStyles[i];
  end;
  Changed; {TODO: only if really changed}
end;

procedure TSynSelectedColor.Merge(Other: TSynSelectedColor; LeftCol, RightCol: Integer);
var
  sKeep, sSet, sClr, sInv, sInvInv: TFontStyles;
  j: TFontStyle;
begin
  BeginUpdate;

  if (Other.Background <> clNone) and (Other.BackPriority >= BackPriority) then begin
    Background := Other.Background;
    BackPriority := Other.BackPriority;
  end;
  if (Other.Foreground <> clNone) and (Other.ForePriority >= ForePriority) then begin
    Foreground := Other.Foreground;
    ForePriority := Other.ForePriority;
  end;

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

procedure TSynSelectedColor.MergeFrames(Other: TSynSelectedColor; LeftCol, RightCol: Integer);

  procedure SetSide(ASide: TLazSynBorderSide; ASrc: TSynSelectedColor);
  begin
    if (FrameSideColors[ASide] <> clNone) and
       ( (ASrc.FrameSidePriority[ASide] < FrameSidePriority[ASide]) or
         ( (ASrc.FrameSidePriority[ASide] = FrameSidePriority[ASide]) and
           (SynFrameEdgePriorities[ASrc.FrameSideOrigin[ASide]] < SynFrameEdgePriorities[FrameSideOrigin[ASide]]) )
       )
    then
      exit;
    FFrameSideColors[ASide] := ASrc.FrameColor;
    FFrameSideStyles[ASide] := ASrc.FrameStyle;
    FFrameSidePriority[ASide] := ASrc.FramePriority;
    FFrameSideOrigin[ASide]   := ASrc.FrameEdges;
    if ASide = bsLeft then
      FStartX := ASrc.FStartX;
    if ASide = bsRight then
      FEndX := ASrc.FEndX;
  end;

var
  i: TLazSynBorderSide;
begin
  if not FFrameSidesInitialized then begin
    for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
      FFrameSideColors[i]   := FrameSideColors[i];
      FFrameSideStyles[i]   := FrameSideStyles[i];
      FFrameSidePriority[i] := FrameSidePriority[i];
      FFrameSideOrigin[i]   := FrameSideOrigin[i];
    end;
    FFrameSidesInitialized := True;
  end;

  If (Other = nil) or (Other.FrameColor = clNone) then
    exit;

  // Merge Values
  case Other.FrameEdges of
    sfeAround: begin
        // UpdateOnly, frame keeps behind individual sites
        if (Other.StartX = LeftCol) then SetSide(bsLeft, Other);
        if (Other.EndX = RightCol)  then SetSide(bsRight, Other);
        SetSide(bsBottom, Other);
        SetSide(bsTop, Other);
        //FrameColor := Other.FrameColor;
        //FrameStyle := Other.FrameStyle;
        //FrameEdges := Other.FrameEdges;
      end;
    sfeBottom: begin
        SetSide(bsBottom, Other);
      end;
    sfeLeft: begin
       // startX ?
        SetSide(bsLeft, Other);
      end;
  end;
end;

procedure TSynSelectedColor.Clear;
var
  i: TLazSynBorderSide;
begin
  BeginUpdate;
  inherited Clear;
  FFrameSidesInitialized := False;
  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
    FFrameSideColors[i] := clNone;
    FFrameSideStyles[i] := slsSolid;
    FFrameSideOrigin[i] := sfeNone;
  end;
  FStartX := -1;
  FEndX := -1;
  FCurrentStartX := -1;
  FCurrentEndX := -1;
  EndUpdate;
end;

function TSynSelectedColor.IsEnabled: boolean;
begin
  Result := (Background <> clNone) or (Foreground <> clNone) or (FrameColor <> clNone) or
            (Style <> []) or (StyleMask <> []);
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

procedure TSynBookMarkOpt.SetBookmarkImages(const Value: TImageList);
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
  FList.Sort({$IFDEF FPC}@{$ENDIF}CompareSynObjectListItems);
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
  Format: UINT = 0;
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
  if (FTextP = nil) and AClipboard.HasFormat(CF_TEXT) then begin
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
  if FIsPlainText and (FText <> '') then begin
    AClipboard.AsText:= FText;
    if not AClipboard.HasFormat(CF_TEXT) then
      raise ESynEditError.Create('Clipboard copy operation failed: HasFormat');
  end;
  Result := AClipboard.AddFormat(ClipboardFormatId, FMemStream.Memory^, FMemStream.Size);
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
  if (aX < 1) or (aX > len + 1) then exit(False);
  Result := ((ax = len + 1) or not(aLine[aX] in FWordChars)) and
            (aLine[aX - 1] in FWordChars);
end;

function TSynWordBreaker.NextWordStart(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if not aIncludeCurrent then
    inc(aX);
  if (aX < 1) or (aX > len + 1) then exit(-1);
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
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if not aIncludeCurrent then
    inc(aX);
  if (aX = 1) or not(aLine[aX - 1] in FWordChars) then
    while (aX <= len) and not(aLine[aX] in FWordChars) do Inc(ax);
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
  if ax > len then aX := len;
  if not aIncludeCurrent then
    dec(aX);
  while (aX >= 1) and not(aLine[aX] in FWordChars) do Dec(ax);
  if aX = 0 then
    exit(-1);
  while (aX >= 1) and (aLine[aX] in FWordChars) do Dec(ax);
  Result := aX  + 1;
end;

function TSynWordBreaker.PrevWordEnd(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if ax > len then aX := len;
  if not aIncludeCurrent then
    dec(aX);
  while (aX >= 1) and (aLine[aX] in FWordChars) do Dec(ax);
  while (aX >= 1) and not(aLine[aX] in FWordChars) do Dec(ax);
  if aX = 0 then
    exit(-1);
  Result := aX + 1;
end;

function TSynWordBreaker.NextBoundary(aLine: String; aX: Integer): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (ax > len) then exit(-1);

  if (aLine[aX] in FWordChars) then
    while (aX <= len) and (aLine[aX] in FWordChars) do Inc(ax)
  else
  if (aLine[aX] in FWordBreakChars) then
    while (aX <= len) and (aLine[aX] in FWordBreakChars) do Inc(ax)
  else
    while (aX <= len) and (aLine[aX] in FWhiteChars) do Inc(ax);
  Result := aX;
end;

function TSynWordBreaker.PrevBoundary(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if not aIncludeCurrent then dec(ax);
  if (aX < 1) or (aX > len) then exit(-1);

  if (aLine[aX] in FWordChars) then
    while (aX > 1) and (aLine[aX] in FWordChars) do dec(ax)
  else
  if (aLine[aX] in FWordBreakChars) then
    while (aX > 1) and (aLine[aX] in FWordBreakChars) do dec(ax)
  else
    while (aX > 1) and (aLine[aX] in FWhiteChars) do dec(ax);
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

end.


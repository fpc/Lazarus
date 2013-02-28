{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    LCL controls for Cody.
}
unit CodyCtrls;

{$mode objfpc}{$H+}

interface

uses
  types, math, typinfo, contnrs, Classes, SysUtils, FPCanvas, FPimage,
  LazLogger, AvgLvlTree, ComCtrls, Controls, Graphics, LCLType, Forms, LCLIntf,
  LMessages;

type
  TCodyCtrlPalette = array of TFPColor;

type

  { TCodyTreeView }

  TCodyTreeView = class(TTreeView)
  public
    procedure FreeNodeData;
  end;

const
  FullCircle16 = 360*16;
  DefaultCategoryGapDegree16 = 0.02*FullCircle16;
  DefaultFirstCategoryDegree16 = 0;
  DefaultCategoryMinSize = 1.0;
  DefaultItemSize = 1.0;
type
  TCustomCircleDiagramControl = class;
  TCircleDiagramCategory = class;

  { TCircleDiagramItem }

  TCircleDiagramItem = class(TPersistent)
  private
    FCaption: TCaption;
    FCategory: TCircleDiagramCategory;
    FEndDegree16: single;
    FSize: single;
    FStartDegree16: single;
    procedure SetCaption(AValue: TCaption);
    procedure SetSize(AValue: single);
    procedure UpdateLayout;
  public
    Data: Pointer; // free to use by user
    constructor Create(TheCategory: TCircleDiagramCategory);
    destructor Destroy; override;
    property Category: TCircleDiagramCategory read FCategory;
    property Caption: TCaption read FCaption write SetCaption;
    property Size: single read FSize write SetSize default DefaultItemSize; // scaled to fit
    property StartDegree16: single read FStartDegree16; // 360*16 = one full circle, 0 at 3o'clock
    property EndDegree16: single read FEndDegree16;     // 360*16 = one full circle, 0 at 3o'clock
  end;

  { TCircleDiagramCategory }

  TCircleDiagramCategory = class(TPersistent)
  private
    FCaption: TCaption;
    FColor: TColor;
    FDiagram: TCustomCircleDiagramControl;
    FEndDegree16: single;
    FMinSize: single;
    fItems: TFPList; // list of TCircleDiagramItem
    FSize: single;
    FStartDegree16: single;
    function GetItems(Index: integer): TCircleDiagramItem;
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
    procedure SetMinSize(AValue: single);
    procedure UpdateLayout;
    procedure Invalidate;
    procedure InternalRemoveItem(Item: TCircleDiagramItem);
  public
    Data: Pointer; // free to use by user
    constructor Create(TheDiagram: TCustomCircleDiagramControl);
    destructor Destroy; override;
    procedure Clear;
    function InsertItem(Index: integer; aCaption: string): TCircleDiagramItem;
    function AddItem(aCaption: string): TCircleDiagramItem;
    property Diagram: TCustomCircleDiagramControl read FDiagram;
    property Caption: TCaption read FCaption write SetCaption;
    property MinSize: single read FMinSize write SetMinSize default DefaultCategoryMinSize; // scaled to fit
    function Count: integer;
    property Items[Index: integer]: TCircleDiagramItem read GetItems; default;
    property Color: TColor read FColor write SetColor;
    property Size: single read FSize;
    property StartDegree16: single read FStartDegree16; // 360*16 = one full circle, 0 at 3o'clock
    property EndDegree16: single read FEndDegree16;     // 360*16 = one full circle, 0 at 3o'clock
  end;

  TCircleDiagramCtrlFlag = (
    cdcNeedUpdateLayout
    );
  TCircleDiagramCtrlFlags = set of TCircleDiagramCtrlFlag;

  { TCustomCircleDiagramControl }

  TCustomCircleDiagramControl = class(TCustomControl)
  private
    FCategoryGapDegree16: single;
    FCenter: TPoint;
    FCenterCaption: TCaption;
    FCenterCaptionRect: TRect;
    FFirstCategoryDegree16: single;
    fCategories: TObjectList; // list of TCircleDiagramCategory
    FInnerRadius: single;
    FOuterRadius: single;
    fUpdateLock: integer;
    fFlags: TCircleDiagramCtrlFlags;
    function GetCategories(Index: integer): TCircleDiagramCategory;
    procedure SetCategoryGapDegree16(AValue: single);
    procedure SetCenterCaption(AValue: TCaption);
    procedure SetFirstCategoryDegree16(AValue: single);
    procedure InternalRemoveCategory(Category: TCircleDiagramCategory);
  protected
    //procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    //procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure CreateWnd; override;
    procedure UpdateScrollBar;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;

    //procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    //procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    //procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;

    //procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); virtual;
    //procedure HandleKeyUp(var Key: Word; Shift: TShiftState); virtual;

    procedure Paint; override;
    procedure DrawCategory(i: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CenterCaption: TCaption read FCenterCaption write SetCenterCaption;
    procedure Clear;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure UpdateLayout;
    procedure EraseBackground({%H-}DC: HDC); override;
    function InsertCategory(Index: integer; aCaption: TCaption): TCircleDiagramCategory;
    function AddCategory(aCaption: TCaption): TCircleDiagramCategory;
    function IndexOfCategory(aCaption: TCaption): integer;
    function FindCategory(aCaption: TCaption): TCircleDiagramCategory;
    property CategoryGapDegree16: single read FCategoryGapDegree16 write SetCategoryGapDegree16 default DefaultCategoryGapDegree16; // 360*16 = one full circle, 0 at 3o'clock
    property FirstCategoryDegree16: single read FFirstCategoryDegree16 write SetFirstCategoryDegree16 default DefaultFirstCategoryDegree16; // 360*16 = one full circle, 0 at 3o'clock
    function CategoryCount: integer;
    property Categories[Index: integer]: TCircleDiagramCategory read GetCategories; default;
    property Color default clWhite;
    // computed values
    property CenterCaptionRect: TRect read FCenterCaptionRect;
    property Center: TPoint read FCenter;
    property InnerRadius: single read FInnerRadius;
    property OuterRadius: single read FOuterRadius;

    // debugging
    procedure WriteDebugReport(Msg: string);
  end;

  { TCircleDiagramControl }

  TCircleDiagramControl = class(TCustomCircleDiagramControl)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;


{off $DEFINE CheckMinXGraph}
type
  TLvlGraph = class;
  TLvlGraphEdge = class;
  TLvlGraphLevel = class;

  { TLvlGraphNode }

  TLvlGraphNode = class(TPersistent)
  private
    FCaption: string;
    FColor: TFPColor;
    FGraph: TLvlGraph;
    FInEdges: TFPList; // list of TLvlGraphEdge
    FDrawSize: integer;
    FInWeight: single;
    FLevel: TLvlGraphLevel;
    FNextSelected: TLvlGraphNode;
    FOutEdges: TFPList; // list of TLvlGraphEdge
    FDrawPosition: integer;
    FOutWeight: single;
    FPrevSelected: TLvlGraphNode;
    FSelected: boolean;
    FVisible: boolean;
    function GetIndexInLevel: integer;
    function GetInEdges(Index: integer): TLvlGraphEdge;
    function GetOutEdges(Index: integer): TLvlGraphEdge;
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TFPColor);
    procedure OnLevelDestroy;
    procedure SetDrawSize(AValue: integer);
    procedure SetIndexInLevel(AValue: integer);
    procedure SetLevel(AValue: TLvlGraphLevel);
    procedure SetSelected(AValue: boolean);
    procedure SetVisible(AValue: boolean);
    procedure UnbindLevel;
    procedure SelectionChanged;
  public
    Data: Pointer; // free for user data
    constructor Create(TheGraph: TLvlGraph; TheCaption: string; TheLevel: TLvlGraphLevel);
    destructor Destroy; override;
    procedure Clear;
    procedure Invalidate;
    property Color: TFPColor read FColor write SetColor;
    property Caption: string read FCaption write SetCaption;
    property Visible: boolean read FVisible write SetVisible;
    property Graph: TLvlGraph read FGraph;
    function IndexOfInEdge(Source: TLvlGraphNode): integer;
    function FindInEdge(Source: TLvlGraphNode): TLvlGraphEdge;
    function InEdgeCount: integer;
    property InEdges[Index: integer]: TLvlGraphEdge read GetInEdges;
    function IndexOfOutEdge(Target: TLvlGraphNode): integer;
    function FindOutEdge(Target: TLvlGraphNode): TLvlGraphEdge;
    function OutEdgeCount: integer;
    property OutEdges[Index: integer]: TLvlGraphEdge read GetOutEdges;
    property IndexInLevel: integer read GetIndexInLevel write SetIndexInLevel;
    property Level: TLvlGraphLevel read FLevel write SetLevel;
    property Selected: boolean read FSelected write SetSelected;
    property NextSelected: TLvlGraphNode read FNextSelected;
    property PrevSelected: TLvlGraphNode read FPrevSelected;
    property DrawPosition: integer read FDrawPosition write FDrawPosition; // position in a level
    property DrawSize: integer read FDrawSize write SetDrawSize default 1;
    function DrawCenter: integer;
    function DrawPositionEnd: integer;// = DrawPosition+Max(InSize,OutSize)
    property InWeight: single read FInWeight; // total weight of InEdges
    property OutWeight: single read FOutWeight; // total weight of OutEdges
  end;
  TLvlGraphNodeClass = class of TLvlGraphNode;

  { TLvlGraphEdge }

  TLvlGraphEdge = class(TPersistent)
  private
    FBackEdge: boolean;
    FSource: TLvlGraphNode;
    FTarget: TLvlGraphNode;
    FWeight: single;
    procedure SetWeight(AValue: single);
  public
    Data: Pointer; // free for user data
    constructor Create(TheSource: TLvlGraphNode; TheTarget: TLvlGraphNode);
    destructor Destroy; override;
    property Source: TLvlGraphNode read FSource;
    property Target: TLvlGraphNode read FTarget;
    property Weight: single read FWeight write SetWeight; // >=0
    function IsBackEdge: boolean;
    property BackEdge: boolean read FBackEdge; // edge was disabled to break a cycle
    function AsString: string;
  end;
  TLvlGraphEdgeClass = class of TLvlGraphEdge;

  { TLvlGraphLevel }

  TLvlGraphLevel = class(TPersistent)
  private
    FGraph: TLvlGraph;
    FIndex: integer;
    fNodes: TFPList;
    FDrawPosition: integer;
    function GetNodes(Index: integer): TLvlGraphNode;
    procedure SetDrawPosition(AValue: integer);
    procedure MoveNode(Node: TLvlGraphNode; NewIndexInLevel: integer);
  public
    Data: Pointer; // free for user data
    constructor Create(TheGraph: TLvlGraph; TheIndex: integer);
    destructor Destroy; override;
    procedure Invalidate;
    property Nodes[Index: integer]: TLvlGraphNode read GetNodes; default;
    function IndexOf(Node: TLvlGraphNode): integer;
    function Count: integer;
    function GetTotalInOutWeights: single; // sum of all nodes Max(InWeight,OutWeight)
    property Index: integer read FIndex;
    property Graph: TLvlGraph read FGraph;
    property DrawPosition: integer read FDrawPosition write SetDrawPosition;
  end;
  TLvlGraphLevelClass = class of TLvlGraphLevel;

  TOnLvlGraphStructureChanged = procedure(Sender, Element: TObject;
                                               Operation: TOperation) of object;

  TLvlGraphEdgeSplitMode = (
    lgesNone,
    lgesSeparate, // create for each edge separate hidden nodes, this creates a lot of hidden nodes
    lgesMergeSource, // combine hidden nodes at source
    lgesMergeTarget // combine hidden nodes at target
    );

  { TLvlGraph }

  TLvlGraph = class(TPersistent)
  private
    FEdgeClass: TLvlGraphEdgeClass;
    FFirstSelected: TLvlGraphNode;
    FLastSelected: TLvlGraphNode;
    FLevelClass: TLvlGraphLevelClass;
    FNodeClass: TLvlGraphNodeClass;
    FOnInvalidate: TNotifyEvent;
    FNodes: TFPList; // list of TLvlGraphNode
    fLevels: TFPList;
    FOnSelectionChanged: TNotifyEvent;
    FOnStructureChanged: TOnLvlGraphStructureChanged;
    function GetLevelCount: integer;
    function GetLevels(Index: integer): TLvlGraphLevel;
    function GetNodes(Index: integer): TLvlGraphNode;
    procedure SetLevelCount(AValue: integer);
    procedure InternalRemoveNode(Node: TLvlGraphNode);
    procedure InternalRemoveLevel(Lvl: TLvlGraphLevel);
  protected
    procedure SelectionChanged;
  public
    Data: Pointer; // free for user data
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Invalidate;
    procedure StructureChanged(Element: TObject; Operation: TOperation);
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnStructureChanged: TOnLvlGraphStructureChanged read FOnStructureChanged write FOnStructureChanged;// node, edge, level was added/deleted

    // nodes
    function NodeCount: integer;
    property Nodes[Index: integer]: TLvlGraphNode read GetNodes;
    function GetNode(aCaption: string; CreateIfNotExists: boolean): TLvlGraphNode;
    function CreateHiddenNode(Level: integer = 0): TLvlGraphNode;
    property NodeClass: TLvlGraphNodeClass read FNodeClass;
    property FirstSelected: TLvlGraphNode read FFirstSelected;
    property LastSelected: TLvlGraphNode read FLastSelected;
    procedure ClearSelection;
    procedure SingleSelect(Node: TLvlGraphNode);
    function IsMultiSelection: boolean;

    // edges
    function GetEdge(SourceCaption, TargetCaption: string;
      CreateIfNotExists: boolean): TLvlGraphEdge;
    function GetEdge(Source, Target: TLvlGraphNode;
      CreateIfNotExists: boolean): TLvlGraphEdge;
    property EdgeClass: TLvlGraphEdgeClass read FEdgeClass;

    // levels
    property Levels[Index: integer]: TLvlGraphLevel read GetLevels;
    property LevelCount: integer read GetLevelCount write SetLevelCount;
    property LevelClass: TLvlGraphLevelClass read FLevelClass;

    procedure CreateTopologicalLevels(HighLevels: boolean); // create levels from edges
    procedure SplitLongEdges(SplitMode: TLvlGraphEdgeSplitMode); // split long edges by adding hidden nodes
    procedure ScaleNodeDrawSizes(NodeGapAbove, NodeGapBelow,
      HardMaxTotal, HardMinOneNode, SoftMaxTotal, SoftMinOneNode: integer);
    procedure SetAllNodeDrawSizes(PixelPerWeight: single = 1.0; MinWeight: single = 0.0);
    procedure MarkBackEdges;
    procedure MinimizeCrossings; // permutate nodes to minimize crossings
    procedure MinimizeOverlappings(MinPos: integer = 0;
      NodeGapAbove: integer = 1; NodeGapBelow: integer = 1;
      aLevel: integer = -1); // set all Node.Position to minimize overlappings
    procedure SetColors(Palette: TCodyCtrlPalette);

    // debugging
    procedure WriteDebugReport(Msg: string);
    procedure ConsistencyCheck(WithBackEdge: boolean);
  end;

type
  TLvlGraphCtrlOption = (
    lgoAutoLayout, // automatic graph layout after graph was changed
    lgoHighlightNodeUnderMouse, // when mouse over node highlight node and its edges
    lgoMouseSelects,
    lgoHighLevels // put nodes topologically at higher levels
    );
  TLvlGraphCtrlOptions = set of TLvlGraphCtrlOption;

  TLvlGraphNodeCaptionPosition = (
    lgncLeft,
    lgncTop,
    lgncRight,
    lgncBottom);

const
  DefaultLvlGraphCtrlOptions = [lgoAutoLayout,
                                lgoHighlightNodeUnderMouse,lgoMouseSelects];
  DefaultLvlGraphEdgeSplitMode = lgesMergeSource;
  DefaultLvlGraphNodeWith = 10;
  DefaultLvlGraphNodeCaptionScale = 0.7;
  DefaultLvlGraphNodeCaptionPosition = lgncTop;
  DefaultLvlGraphNodeGapLeft   = 2;
  DefaultLvlGraphNodeGapRight  = 2;
  DefaultLvlGraphNodeGapTop    = 1;
  DefaultLvlGraphNodeGapBottom = 1;

type
  TLvlGraphControlFlag =  (
    lgcNeedInvalidate,
    lgcNeedAutoLayout,
    lgcIgnoreGraphInvalidate,
    lgcUpdatingScrollBars
    );
  TLvlGraphControlFlags = set of TLvlGraphControlFlag;

  TCustomLvlGraphControl = class;

  { TLvlGraphNodeStyle }

  TLvlGraphNodeStyle = class(TPersistent)
  private
    FCaptionPosition: TLvlGraphNodeCaptionPosition;
    FCaptionScale: single;
    FControl: TCustomLvlGraphControl;
    FGapBottom: integer;
    FGapLeft: integer;
    FGapRight: integer;
    FGapTop: integer;
    FWidth: integer;
    procedure SetCaptionPosition(AValue: TLvlGraphNodeCaptionPosition);
    procedure SetCaptionScale(AValue: single);
    procedure SetGapBottom(AValue: integer);
    procedure SetGapLeft(AValue: integer);
    procedure SetGapRight(AValue: integer);
    procedure SetGapTop(AValue: integer);
    procedure SetWidth(AValue: integer);
  public
    constructor Create(AControl: TCustomLvlGraphControl);
    destructor Destroy; override;
  published
    procedure Assign(Source: TPersistent); override;
    function Equals(Obj: TObject): boolean; override;
    property Control: TCustomLvlGraphControl read FControl;
    property CaptionPosition: TLvlGraphNodeCaptionPosition
      read FCaptionPosition write SetCaptionPosition default DefaultLvlGraphNodeCaptionPosition;
    property CaptionScale: single read FCaptionScale write SetCaptionScale default DefaultLvlGraphNodeCaptionScale;
    property GapLeft: integer read FGapLeft write SetGapLeft default DefaultLvlGraphNodeGapLeft; // used by AutoLayout
    property GapTop: integer read FGapTop write SetGapTop default DefaultLvlGraphNodeGapTop; // used by AutoLayout
    property GapRight: integer read FGapRight write SetGapRight default DefaultLvlGraphNodeGapRight; // used by AutoLayout
    property GapBottom: integer read FGapBottom write SetGapBottom default DefaultLvlGraphNodeGapBottom; // used by AutoLayout
    property Width: integer read FWidth write SetWidth default DefaultLvlGraphNodeWith;
  end;

  { TCustomLvlGraphControl }

  TCustomLvlGraphControl = class(TCustomControl)
  private
    FEdgeSplitMode: TLvlGraphEdgeSplitMode;
    FGraph: TLvlGraph;
    FNodeStyle: TLvlGraphNodeStyle;
    FNodeUnderMouse: TLvlGraphNode;
    FOnMinimizeCrossings: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOptions: TLvlGraphCtrlOptions;
    FScrollLeft: integer;
    FScrollLeftMax: integer;
    FScrollTopMax: integer;
    FScrollTop: integer;
    fUpdateLock: integer;
    FFlags: TLvlGraphControlFlags;
    procedure DrawCaptions(const TxtH: integer);
    procedure DrawEdges(Highlighted: boolean);
    procedure DrawNodes;
    procedure SetNodeStyle(AValue: TLvlGraphNodeStyle);
    procedure SetNodeUnderMouse(AValue: TLvlGraphNode);
    procedure SetOptions(AValue: TLvlGraphCtrlOptions);
    procedure SetScrollLeft(AValue: integer);
    procedure SetScrollTop(AValue: integer);
    procedure UpdateScrollBars;
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
  protected
    procedure AutoLayoutLevels(TxtH: LongInt); virtual;
    procedure GraphInvalidate(Sender: TObject); virtual;
    procedure GraphSelectionChanged(Sender: TObject); virtual;
    procedure GraphStructureChanged(Sender, Element: TObject; Operation: TOperation); virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;
    property Graph: TLvlGraph read FGraph;
    procedure Clear;
    procedure AutoLayout(RndColors: boolean = true); virtual;
    procedure Invalidate; override;
    procedure InvalidateAutoLayout;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetNodeAt(X,Y: integer): TLvlGraphNode;
    class function GetControlClassDefaultSize: TSize; override;
    function GetDrawSize: TPoint;
  public
    property EdgeSplitMode: TLvlGraphEdgeSplitMode read FEdgeSplitMode write FEdgeSplitMode default DefaultLvlGraphEdgeSplitMode;
    property NodeStyle: TLvlGraphNodeStyle read FNodeStyle write SetNodeStyle;
    property NodeUnderMouse: TLvlGraphNode read FNodeUnderMouse write SetNodeUnderMouse;
    property Options: TLvlGraphCtrlOptions read FOptions write SetOptions default DefaultLvlGraphCtrlOptions;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property ScrollTop: integer read FScrollTop write SetScrollTop;
    property ScrollTopMax: integer read FScrollTopMax;
    property ScrollLeft: integer read FScrollLeft write SetScrollLeft;
    property ScrollLeftMax: integer read FScrollLeftMax;
    property OnMinimizeCrossings: TNotifyEvent read FOnMinimizeCrossings write FOnMinimizeCrossings;// provide an alternative minimize crossing algorithm
  end;

  { TLvlGraphControl }

  TLvlGraphControl = class(TCustomLvlGraphControl)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EdgeSplitMode;
    property Enabled;
    property Font;
    property NodeStyle;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMinimizeCrossings;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectionChanged;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Options;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
  end;

// misc
procedure FreeTVNodeData(TV: TCustomTreeView);

function GetCCPaletteRGB(Cnt: integer; Shuffled: boolean): TCodyCtrlPalette;
procedure ShuffleCCPalette(Palette: TCodyCtrlPalette);
function Darker(const c: TColor): TColor; overload;

// diagram
procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2: integer;
  InnerSize: single; StartAngle16, EndAngle16: integer); overload;
procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2,
  InnerSize, StartAngle, EndAngle: single); overload;

// level graph
function CompareLGNodesByCenterPos(Node1, Node2: Pointer): integer;

function dbgs(p: TLvlGraphNodeCaptionPosition): string; overload;
function dbgs(o: TLvlGraphCtrlOption): string; overload;
function dbgs(Options: TLvlGraphCtrlOptions): string; overload;

procedure LvlGraphMinimizeCrossings(Graph: TLvlGraph); overload;

implementation

type
  TMinXGraph = class;
  TMinXLevel = class;
  TMinXPair = class;

  { TMinXNode }

  TMinXNode = class
  public
    GraphNode: TLvlGraphNode;
    InEdges, OutEdges: array of TMinXNode;
    Level: TMinXLevel;
    IndexInLevel: integer;
    constructor Create(aNode: TLvlGraphNode);
    destructor Destroy; override;
  end;

  { TMinXLevel }

  TMinXLevel = class
  public
    Index: integer;
    Graph: TMinXGraph;
    GraphLevel: TLvlGraphLevel;
    Nodes: array of TMinXNode;
    Pairs: array of TMinXPair;
    BestNodes: array of TLvlGraphNode;
    constructor Create(aGraph: TMinXGraph; aIndex: integer);
    destructor Destroy; override;
    procedure GetCrossingCount(Node1, Node2: TMinXNode; out Crossing, SwitchCrossing: integer);
  end;

  { TMinXPair }

  TMinXPair = class
  private
    FSwitchDiff: integer; // change of crossings when the two nodes would switch
    procedure SetSwitchDiff(AValue: integer);
  public
    Level: TMinXLevel;
    Graph: TMinXGraph;
    Index: integer;
    PrevSameSwitchPair, NextSameSwitchPair: TMinXPair;
    constructor Create(aLevel: TMinXLevel; aIndex: integer);
    destructor Destroy; override;
    procedure UnbindFromSwitchList;
    procedure BindToSwitchList;
    procedure ComputeCrossingCount(out Crossing, SwitchCrossing: integer);
    function ComputeSwitchDiff: integer;
    property SwitchDiff: integer read FSwitchDiff write SetSwitchDiff;
    function AsString: string;
  end;

  { TMinXGraph }

  TMinXGraph = class
  private
    FGraphNodeToNode: TPointerToPointerTree; // TLvlGraphNode to TMinXNode
    procedure UnbindPairs;
    procedure BindPairs;
    function ComputeCrossCount: integer;
    procedure StoreAsBest(CheckIfBetter: boolean);
    function ComputeLowestSwitchDiff(StartAtOld: boolean; IgnorePair: TMinXPair): integer;
  public
    Graph: TLvlGraph;
    Levels: array of TMinXLevel;
    Pairs: array of TMinXPair;
    SameSwitchDiffPairs: array of TMinXPair; //
    SameSwitchDiffPair0: integer;
    LowestSwitchDiff: integer;
    CrossCount: integer;
    BestCrossCount: integer;
    constructor Create(aGraph: TLvlGraph);
    destructor Destroy; override;
    procedure InitSearch;
    function FindBestPair: TMinXPair;
    procedure SwitchCrossingPairs(MaxRun: int64; var Run: int64);
    procedure Shuffle;
    procedure SwitchAndShuffle(MaxSingleRun, MaxTotalRun: int64);
    procedure SwitchPair(Pair: TMinXPair);
    procedure Apply; // reorder Graph nodes
    function GraphNodeToNode(GraphNode: TLvlGraphNode): TMinXNode; inline;
    procedure ConsistencyCheck;
  end;

procedure LvlGraphMinimizeCrossings(Graph: TLvlGraph);
var
  g: TMinXGraph;
  {%H-}Run: int64;
begin
  if (Graph.LevelCount<2) or (Graph.NodeCount<3) then exit;
  g:=TMinXGraph.Create(Graph);
  try
    if length(g.Pairs)=0 then exit;
    g.InitSearch;
    Run:=0;
    debugln(['LvlGraphMinimizeCrossings Graph.NodeCount=',Graph.NodeCount]);
    g.SwitchAndShuffle(100*Graph.NodeCount,
                       Min(10000,Graph.NodeCount*Graph.NodeCount));
    g.Apply;
  finally
    g.Free;
  end;
end;

procedure FreeTVNodeData(TV: TCustomTreeView);
var
  Node: TTreeNode;
begin
  TV.BeginUpdate;
  Node:=TV.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Data<>nil then begin
      TObject(Node.Data).Free;
      Node.Data:=nil;
    end;
    Node:=Node.GetNext;
  end;
  TV.EndUpdate;
end;

procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2: integer;
  InnerSize: single; StartAngle16, EndAngle16: integer);
begin
  RingSector(Canvas,single(x1),single(y1),single(x2),single(y2),InnerSize,
    single(StartAngle16)/16,single(EndAngle16)/16);
end;

procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2, InnerSize, StartAngle,
  EndAngle: single);
var
  OuterCnt: integer;
  centerx, centery: single;
  i: Integer;
  Ang: single;
  OuterRadiusX, OuterRadiusY, InnerRadiusX, InnerRadiusY: single;
  Points: array of TPoint;
  j: Integer;
begin
  OuterCnt:=Round(SQRT((Abs(x2-x1)+Abs(y2-y1))*Abs(EndAngle-StartAngle)/FullCircle16)+0.5);
  centerx:=(x1+x2)/2;
  centery:=(y1+y2)/2;
  OuterRadiusX:=(x2-x1)/2;
  OuterRadiusY:=(y2-y1)/2;
  InnerRadiusX:=OuterRadiusX*InnerSize;
  InnerRadiusY:=OuterRadiusY*InnerSize;
  SetLength(Points,OuterCnt*2+2);
  j:=0;
  // outer arc
  for i:=0 to OuterCnt do begin
    Ang:=StartAngle+((EndAngle-StartAngle)/OuterCnt)*single(i);
    Ang:=(Ang/FullCircle16)*2*pi;
    Points[j].x:=round(centerx+cos(Ang)*OuterRadiusX);
    Points[j].y:=round(centery-sin(Ang)*OuterRadiusY);
    inc(j);
  end;
  // inner arc
  for i:=OuterCnt downto 0 do begin
    Ang:=StartAngle+((EndAngle-StartAngle)/OuterCnt)*single(i);
    Ang:=(Ang/FullCircle16)*2*pi;
    Points[j].x:=round(centerx+cos(Ang)*InnerRadiusX);
    Points[j].y:=round(centery-sin(Ang)*InnerRadiusY);
    inc(j);
  end;
  Canvas.Polygon(Points);
  SetLength(Points,0);
end;

function GetCCPaletteRGB(Cnt: integer; Shuffled: boolean): TCodyCtrlPalette;
type
  TChannel = (cRed, cGreen, cBlue);
const
  ChannelMax = alphaOpaque;
var
  Steps, Step, Start, Value: array[TChannel] of integer;

  function EnoughColors: boolean;
  var
    PotCnt: Integer;
    ch: TChannel;
  begin
    PotCnt:=1;
    for ch:=Low(TChannel) to High(TChannel) do
      PotCnt*=Steps[ch];
    Result:=PotCnt>=Cnt;
  end;

var
  ch: TChannel;
  i: Integer;
begin
  SetLength(Result,Cnt);
  if Cnt=0 then exit;
  for ch:=Low(TChannel) to High(TChannel) do
    Steps[ch]:=1;
  while not EnoughColors do
    for ch:=Low(TChannel) to High(TChannel) do begin
      if EnoughColors then break;
      inc(Steps[ch]);
    end;
  for ch:=Low(TChannel) to High(TChannel) do begin
    Step[ch]:=ChannelMax div Steps[ch];
    Start[ch]:=ChannelMax-1-Step[ch]*(Steps[ch]-1);
    Value[ch]:=Start[ch];
  end;
  for i:=0 to Cnt-1 do begin
    Result[i].red:=Value[cRed];
    Result[i].green:=Value[cGreen];
    Result[i].blue:=Value[cBlue];
    ch:=Low(TChannel);
    repeat
      Value[ch]+=Step[ch];
      if (Value[ch]<ChannelMax) or (ch=High(TChannel)) then break;
      Value[ch]:=Start[ch];
      inc(ch);
    until false;
  end;
  if Shuffled then
    ShuffleCCPalette(Result);
end;

procedure ShuffleCCPalette(Palette: TCodyCtrlPalette);
begin

end;

function Darker(const c: TColor): TColor;
var
  r: Byte;
  g: Byte;
  b: Byte;
begin
  RedGreenBlue(c,r,g,b);
  r:=r div 2;
  g:=g div 2;
  b:=b div 2;
  Result:=RGBToColor(r,g,b);
end;

function CompareLGNodesByCenterPos(Node1, Node2: Pointer): integer;
var
  LNode1: TLvlGraphNode absolute Node1;
  LNode2: TLvlGraphNode absolute Node2;
  p1: Integer;
  p2: Integer;
begin
  p1:=LNode1.DrawCenter;
  p2:=LNode2.DrawCenter;
  if p1<p2 then
    exit(-1)
  else if p1>p2 then
    exit(1);
  // default compare by position in level
  Result:=LNode1.IndexInLevel-LNode2.IndexInLevel;
end;

function dbgs(p: TLvlGraphNodeCaptionPosition): string;
begin
  Result:=GetEnumName(typeinfo(p),ord(p));
end;

function dbgs(o: TLvlGraphCtrlOption): string;
begin
  Result:=GetEnumName(typeinfo(o),ord(o));
end;

function dbgs(Options: TLvlGraphCtrlOptions): string;
var
  o: TLvlGraphCtrlOption;
begin
  Result:='';
  for o:=Low(TLvlGraphCtrlOption) to high(TLvlGraphCtrlOption) do
    if o in Options then begin
      if Result<>'' then Result+=',';
      Result+=dbgs(o);
    end;
  Result:='['+Result+']';
end;

{ TMinXPair }

procedure TMinXPair.SetSwitchDiff(AValue: integer);
begin
  if FSwitchDiff=AValue then Exit;
  UnbindFromSwitchList;
  FSwitchDiff:=AValue;
  BindToSwitchList;
end;

constructor TMinXPair.Create(aLevel: TMinXLevel; aIndex: integer);
begin
  Level:=aLevel;
  Graph:=Level.Graph;
  Index:=aIndex;
end;

destructor TMinXPair.Destroy;
begin
  inherited Destroy;
end;

procedure TMinXPair.UnbindFromSwitchList;
begin
  if PrevSameSwitchPair<>nil then
    PrevSameSwitchPair.NextSameSwitchPair:=NextSameSwitchPair
  else if Graph.SameSwitchDiffPairs[Graph.SameSwitchDiffPair0+SwitchDiff]=Self
  then begin
    Graph.SameSwitchDiffPairs[Graph.SameSwitchDiffPair0+SwitchDiff]:=NextSameSwitchPair;
    if (NextSameSwitchPair=nil) and (Graph.LowestSwitchDiff=SwitchDiff) then
      Graph.LowestSwitchDiff:=Graph.ComputeLowestSwitchDiff(true,Self);
  end;
  if NextSameSwitchPair<>nil then
    NextSameSwitchPair.PrevSameSwitchPair:=PrevSameSwitchPair;
  PrevSameSwitchPair:=nil;
  NextSameSwitchPair:=nil;
end;

procedure TMinXPair.BindToSwitchList;
begin
  NextSameSwitchPair:=Graph.SameSwitchDiffPairs[Graph.SameSwitchDiffPair0+SwitchDiff];
  Graph.SameSwitchDiffPairs[Graph.SameSwitchDiffPair0+SwitchDiff]:=Self;
  if NextSameSwitchPair<>nil then
    NextSameSwitchPair.PrevSameSwitchPair:=Self;
  if (Graph.LowestSwitchDiff+Graph.SameSwitchDiffPair0<0)
  or (Graph.LowestSwitchDiff>SwitchDiff) then
    Graph.LowestSwitchDiff:=SwitchDiff;
end;

procedure TMinXPair.ComputeCrossingCount(out Crossing,
  SwitchCrossing: integer);
begin
  Level.GetCrossingCount(Level.Nodes[Index],Level.Nodes[Index+1],
    Crossing,SwitchCrossing);
end;

function TMinXPair.ComputeSwitchDiff: integer;
var
  Crossing, SwitchCrossing: integer;
begin
  Level.GetCrossingCount(Level.Nodes[Index],Level.Nodes[Index+1],
    Crossing,SwitchCrossing);
  Result:=SwitchCrossing-Crossing;
end;

function TMinXPair.AsString: string;
begin
  Result:='[lvl='+dbgs(Level.Index)
    +',A='+dbgs(Index)+':'+Level.Nodes[Index].GraphNode.Caption
    +',B='+dbgs(Index+1)+':'+Level.Nodes[Index+1].GraphNode.Caption
    +',Switch='+dbgs(SwitchDiff)
    +']';
end;

{ TMinXGraph }

constructor TMinXGraph.Create(aGraph: TLvlGraph);
var
  GraphNode: TLvlGraphNode;
  i: Integer;
  Level: TMinXLevel;
  n: Integer;
  e: Integer;
  Node: TMinXNode;
  Cnt: Integer;
  OtherNode: TMinXNode;
begin
  Graph:=aGraph;

  // create nodes
  FGraphNodeToNode:=TPointerToPointerTree.Create;
  for i:=0 to Graph.NodeCount-1 do begin
    GraphNode:=Graph.Nodes[i];
    Node:=TMinXNode.Create(GraphNode);
    FGraphNodeToNode[GraphNode]:=Node;
  end;

  // create levels
  SetLength(Levels,aGraph.LevelCount);
  for i:=0 to length(Levels)-1 do
    Levels[i]:=TMinXLevel.Create(Self,i);

  // create OutEdges arrays
  for i:=0 to length(Levels)-2 do begin
    Level:=Levels[i];
    for n:=0 to length(Level.Nodes)-1 do begin
      Node:=Level.Nodes[n];
      GraphNode:=Node.GraphNode;
      SetLength(Node.OutEdges,GraphNode.OutEdgeCount);
      Cnt:=0;
      for e:=0 to GraphNode.OutEdgeCount-1 do begin
        OtherNode:=GraphNodeToNode(GraphNode.OutEdges[e].Target);
        if Node.Level.Index+1<>OtherNode.Level.Index then continue;
        Node.OutEdges[Cnt]:=OtherNode;
        Cnt+=1;
      end;
      SetLength(Node.OutEdges,Cnt);
    end;
  end;

  // create InEdges arrays
  for i:=1 to length(Levels)-1 do begin
    Level:=Levels[i];
    for n:=0 to length(Level.Nodes)-1 do begin
      Node:=Level.Nodes[n];
      GraphNode:=Node.GraphNode;
      SetLength(Node.InEdges,GraphNode.InEdgeCount);
      Cnt:=0;
      for e:=0 to GraphNode.InEdgeCount-1 do begin
        OtherNode:=GraphNodeToNode(GraphNode.InEdges[e].Source);
        if Node.Level.Index-1<>OtherNode.Level.Index then continue;
        Node.InEdges[Cnt]:=OtherNode;
        Cnt+=1;
      end;
      SetLength(Node.InEdges,Cnt);
    end;
  end;

  BindPairs;

  {$IFDEF CheckMinXGraph}
  ConsistencyCheck;
  {$ENDIF}
end;

destructor TMinXGraph.Destroy;
var
  i: Integer;
begin
  for i:=0 to length(Levels)-1 do
    Levels[i].Free;
  SetLength(Levels,0);
  for i:=0 to length(Pairs)-1 do
    Pairs[i].Free;
  SetLength(Pairs,0);
  SetLength(SameSwitchDiffPairs,0);
  FreeAndNil(FGraphNodeToNode);
  inherited Destroy;
end;

procedure TMinXGraph.UnbindPairs;
var
  i: Integer;
begin
  for i:=0 to length(Pairs)-1 do
    Pairs[i].UnbindFromSwitchList;
end;

procedure TMinXGraph.BindPairs;
var
  Cnt: Integer;
  i: Integer;
  Level: TMinXLevel;
  n: Integer;
  Pair: TMinXPair;
  First: Boolean;
begin
  First:=length(Pairs)=0;
  if First then begin
    Cnt:=0;
    for i:=0 to length(Levels)-1 do
      Cnt+=Max(0,length(Levels[i].Nodes)-1);
    SetLength(Pairs,Cnt);
  end;
  Cnt:=0;
  for i:=0 to length(Levels)-1 do begin
    Level:=Levels[i];
    SetLength(Level.Pairs,length(Level.Nodes)-1);
    for n:=0 to length(Level.Pairs)-1 do begin
      if First then begin
        Pair:=TMinXPair.Create(Level,n);
        Pairs[Cnt]:=Pair;
        Level.Pairs[n]:=Pair;
      end else
        Pair:=Pairs[Cnt];
      Pair.FSwitchDiff:=Pair.ComputeSwitchDiff;
      Cnt+=1;
    end;
  end;
  if First then begin
    SameSwitchDiffPair0:=Graph.NodeCount*Graph.NodeCount;
    LowestSwitchDiff:=-SameSwitchDiffPair0-1;
    SetLength(SameSwitchDiffPairs,2*SameSwitchDiffPair0+1);
  end;
  for i:=0 to length(Pairs)-1 do
    Pairs[i].BindToSwitchList;
  CrossCount:=ComputeCrossCount;
end;

function TMinXGraph.ComputeCrossCount: integer;
var
  l: Integer;
  Level: TMinXLevel;
  i: Integer;
  Node1: TMinXNode;
  j: Integer;
  Node2: TMinXNode;
  e1: Integer;
  Target1: TMinXNode;
  e2: Integer;
  Target2: TMinXNode;
begin
  Result:=0;
  for l:=0 to length(Levels)-2 do begin
    Level:=Levels[l];
    for i:=0 to length(Level.Nodes)-2 do begin
      Node1:=Level.Nodes[i];
      for j:=i+1 to length(Level.Nodes)-1 do begin
        Node2:=Level.Nodes[j];
        for e1:=0 to length(Node1.OutEdges)-1 do begin
          Target1:=Node1.OutEdges[e1];
          for e2:=0 to length(Node2.OutEdges)-1 do begin
            Target2:=Node2.OutEdges[e2];
            if Target1.IndexInLevel>Target2.IndexInLevel then
              Result+=1;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMinXGraph.InitSearch;
begin
  StoreAsBest(false);
end;

procedure TMinXGraph.StoreAsBest(CheckIfBetter: boolean);
var
  l: Integer;
  Level: TMinXLevel;
  n: Integer;
begin
  if CheckIfBetter and (BestCrossCount>=0) and (BestCrossCount<CrossCount) then
    exit;
  BestCrossCount:=CrossCount;
  for l:=0 to length(Levels)-1 do begin
    Level:=Levels[l];
    for n:=0 to length(Level.Nodes)-1 do
      Level.BestNodes[n]:=Level.Nodes[n].GraphNode;
  end;
end;

function TMinXGraph.ComputeLowestSwitchDiff(StartAtOld: boolean;
  IgnorePair: TMinXPair): integer;
var
  i: Integer;
  Pair: TMinXPair;
begin
  if StartAtOld then begin
    for i:=LowestSwitchDiff to Graph.NodeCount-1 do begin
      if SameSwitchDiffPairs[i+SameSwitchDiffPair0]<>nil then
        exit(i);
    end;
  end;
  Result:=SameSwitchDiffPair0+1;
  for i:=0 to length(Pairs)-1 do begin
    Pair:=Pairs[i];
    if IgnorePair=Pair then continue;
    Result:=Min(Result,Pairs[i].SwitchDiff);
  end;
  if Result>SameSwitchDiffPair0 then
    Result:=-1-SameSwitchDiffPair0;
end;

function TMinXGraph.FindBestPair: TMinXPair;
var
  i: Integer;
begin
  i:=LowestSwitchDiff+SameSwitchDiffPair0;
  if i>=0 then
    Result:=SameSwitchDiffPairs[i]
  else
    Result:=nil;
end;

procedure TMinXGraph.SwitchCrossingPairs(MaxRun: int64; var Run: int64);
var
  Pair: TMinXPair;
begin
  while (MaxRun>0) and (BestCrossCount<>0) do begin
    //debugln(['TMinXGraph.SwitchCrossingPairs ',MaxRun,' ',Run]);
    Pair:=FindBestPair;
    Run+=1;
    if (Pair=nil) or (Pair.SwitchDiff=0) then exit;
    SwitchPair(Pair);
    MaxRun-=1;
  end;
end;

procedure TMinXGraph.Shuffle;
var
  l: Integer;
  Level: TMinXLevel;
  n1: Integer;
  n2: Integer;
  Node: TMinXNode;
begin
  {$IFDEF CheckMinXGraph}
  ConsistencyCheck;
  {$ENDIF}
  UnbindPairs;
  for l:=0 to length(Levels)-1 do begin
    Level:=Levels[l];
    for n1:=0 to length(Level.Nodes)-1 do begin
      n2:=Random(length(Level.Nodes));
      if n1=n2 then continue;
      Node:=Level.Nodes[n1];
      Level.Nodes[n1]:=Level.Nodes[n2];
      Level.Nodes[n2]:=Node;
      Level.Nodes[n1].IndexInLevel:=n1;
      Level.Nodes[n2].IndexInLevel:=n2;
    end;
  end;
  BindPairs;
  StoreAsBest(true);
  {$IFDEF CheckMinXGraph}
  ConsistencyCheck;
  {$ENDIF}
end;

procedure TMinXGraph.SwitchAndShuffle(MaxSingleRun, MaxTotalRun: int64);
var
  Run: int64;
begin
  Run:=1;
  while BestCrossCount<>0 do begin
    SwitchCrossingPairs(MaxSingleRun,Run);
    if Run>MaxTotalRun then exit;
    Shuffle;
  end;
end;

procedure TMinXGraph.SwitchPair(Pair: TMinXPair);

  procedure UpdateSwitchDiff(TargetOfNode1, TargetOfNode2: TMinXNode);
  var
    TargetPair: TMinXPair;
  begin
    if TargetOfNode1.IndexInLevel+1=TargetOfNode2.IndexInLevel then begin
      TargetPair:=TargetOfNode1.Level.Pairs[TargetOfNode1.IndexInLevel];
      // no longer crossing, switching TargetPair would create the cross again, from -1 to +1 = +2
      TargetPair.SwitchDiff:=TargetPair.SwitchDiff+2;
    end else if TargetOfNode1.IndexInLevel-1=TargetOfNode2.IndexInLevel then begin
      TargetPair:=TargetOfNode2.Level.Pairs[TargetOfNode2.IndexInLevel];
      // now crossing, switching TargetPair would solve the cross again, from +1 to -1 = -2
      TargetPair.SwitchDiff:=TargetPair.SwitchDiff-2;
    end;
  end;

var
  Node1, Node2: TMinXNode;
  i: Integer;
  j: Integer;
  NeighbourPair: TMinXPair;
  Level: TMinXLevel;
begin
  //debugln(['TMinXGraph.SwitchPair ',Pair.AsString]);
  {$IFDEF CheckMinXGraph}
  ConsistencyCheck;
  {$ENDIF}

  Level:=Pair.Level;

  // switch nodes
  Node1:=Level.Nodes[Pair.Index];
  Node2:=Level.Nodes[Pair.Index+1];
  Level.Nodes[Pair.Index]:=Node2;
  Level.Nodes[Pair.Index+1]:=Node1;
  Node1:=Level.Nodes[Pair.Index];
  Node2:=Level.Nodes[Pair.Index+1];
  Node1.IndexInLevel:=Pair.Index;
  Node2.IndexInLevel:=Pair.Index+1;

  // reverse Pair.SwitchDiff
  CrossCount+=Pair.SwitchDiff;
  Pair.SwitchDiff:=-Pair.SwitchDiff;
  //debugln(['TMinXGraph.SwitchPair Pair.SwitchDiff should be equal: ',Pair.SwitchDiff,' = ',Pair.ComputeSwitchDiff]);

  // compute SwitchDiff of new neighbour pairs
  if Pair.Index>0 then begin
    NeighbourPair:=Level.Pairs[Pair.Index-1];
    NeighbourPair.SwitchDiff:=NeighbourPair.ComputeSwitchDiff;
  end;
  if Pair.Index+1<length(Level.Pairs) then begin
    NeighbourPair:=Level.Pairs[Pair.Index+1];
    NeighbourPair.SwitchDiff:=NeighbourPair.ComputeSwitchDiff;
  end;

  // update SwitchDiff of all connected nodes
  for i:=0 to length(Node1.OutEdges)-1 do
    for j:=0 to length(Node2.OutEdges)-1 do
      UpdateSwitchDiff(Node1.OutEdges[i],Node2.OutEdges[j]);
  for i:=0 to length(Node1.InEdges)-1 do
    for j:=0 to length(Node2.InEdges)-1 do
      UpdateSwitchDiff(Node1.InEdges[i],Node2.InEdges[j]);

  StoreAsBest(true);

  {$IFDEF CheckMinXGraph}
  ConsistencyCheck;
  {$ENDIF}
end;

procedure TMinXGraph.Apply;
var
  i: Integer;
  Level: TMinXLevel;
  j: Integer;
begin
  for i:=0 to length(Levels)-1 do begin
    Level:=Levels[i];
    for j:=0 to length(Level.BestNodes)-1 do
      Level.BestNodes[j].IndexInLevel:=j;
  end;
end;

function TMinXGraph.GraphNodeToNode(GraphNode: TLvlGraphNode): TMinXNode;
begin
  Result:=TMinXNode(FGraphNodeToNode[GraphNode]);
end;

procedure TMinXGraph.ConsistencyCheck;

  procedure Err(Msg: string = '');
  begin
    raise Exception.Create('TMinXGraph.ConsistencyCheck: '+Msg);
  end;

var
  i: Integer;
  Pair: TMinXPair;
  Level: TMinXLevel;
  j: Integer;
  Node: TMinXNode;
  e: Integer;
  OtherNode: TMinXNode;
  k: Integer;
  AVLNode: TAvgLvlTreeNode;
  P2PItem: PPointerToPointerItem;
begin
  AVLNode:=FGraphNodeToNode.Tree.FindLowest;
  while AVLNode<>nil do begin
    P2PItem:=PPointerToPointerItem(AVLNode.Data);
    if not (TObject(P2PItem^.Key) is TLvlGraphNode) then
      Err(DbgSName(TObject(P2PItem^.Key)));
    if not (TObject(P2PItem^.Value) is TMinXNode) then
      Err(DbgSName(TObject(P2PItem^.Value)));
    if TMinXNode(P2PItem^.Value).GraphNode=nil then
      Err(dbgs(TMinXNode(P2PItem^.Value).IndexInLevel));
    if TLvlGraphNode(P2PItem^.Key)<>TMinXNode(P2PItem^.Value).GraphNode then
      Err;
    AVLNode:=FGraphNodeToNode.Tree.FindSuccessor(AVLNode);
  end;

  if length(Levels)<>Graph.LevelCount then
    Err;
  for i:=0 to length(Levels)-1 do begin
    Level:=Levels[i];
    for j:=0 to Length(Level.Pairs)-1 do begin
      Pair:=Level.Pairs[j];
      if Pair.Level<>Level then
        Err(Pair.AsString);
    end;
    for j:=0 to length(Level.Nodes)-1 do begin
      Node:=Level.Nodes[j];
      if Node.Level<>Level then
        Err;
      if Node.IndexInLevel<>j then
        Err;
      if Node.GraphNode=nil then
        Err;
      for e:=0 to length(Node.InEdges)-1 do begin
        OtherNode:=Node.InEdges[e];
        if OtherNode=nil then
          Err('node="'+Node.GraphNode.Caption+'" e='+dbgs(e));
        if Node.Level.Index-1<>OtherNode.Level.Index then
          Err('node="'+Node.GraphNode.Caption+'" othernode="'+OtherNode.GraphNode.Caption+'"');
        k:=length(OtherNode.OutEdges)-1;
        while (k>=0) and (OtherNode.OutEdges[k]<>Node) do dec(k);
        if k<0 then
          Err('node="'+Node.GraphNode.Caption+'" othernode="'+OtherNode.GraphNode.Caption+'"');
      end;
      for e:=0 to length(Node.OutEdges)-1 do begin
        OtherNode:=Node.OutEdges[e];
        if OtherNode=nil then
          Err('node="'+Node.GraphNode.Caption+'" e='+dbgs(e));
        if Node.Level.Index+1<>OtherNode.Level.Index then
          Err('node="'+Node.GraphNode.Caption+'" othernode="'+OtherNode.GraphNode.Caption+'"');
        k:=length(OtherNode.InEdges)-1;
        while (k>=0) and (OtherNode.InEdges[k]<>Node) do dec(k);
        if k<0 then
          Err('node="'+Node.GraphNode.Caption+'" othernode="'+OtherNode.GraphNode.Caption+'"');
      end;
    end;
  end;
  for i:=0 to length(Pairs)-1 do begin
    Pair:=Pairs[i];
    if Pair.Graph<>Self then
      Err(Pair.AsString);
    if Pair.Level.Pairs[Pair.Index]<>Pair then
      Err(Pair.AsString);
    if Pair.SwitchDiff<>Pair.ComputeSwitchDiff then
      Err(Pair.AsString);
  end;
  for i:=0 to length(SameSwitchDiffPairs)-1 do begin
    Pair:=SameSwitchDiffPairs[i];
    while Pair<>nil do begin
      if Pair.SwitchDiff<>i-SameSwitchDiffPair0 then
        Err(Pair.AsString);
      if Pair.PrevSameSwitchPair<>nil then begin
        if Pair.PrevSameSwitchPair.NextSameSwitchPair<>Pair then
          Err(Pair.AsString);
      end else begin
        if Pair<>SameSwitchDiffPairs[i] then
          Err(Pair.AsString);
      end;
      if Pair.NextSameSwitchPair<>nil then begin
        if Pair.NextSameSwitchPair.PrevSameSwitchPair<>Pair then
          Err(Pair.AsString);
      end;
      Pair:=Pair.NextSameSwitchPair;
    end;
  end;

  if CrossCount<>ComputeCrossCount then
    Err;
  if LowestSwitchDiff<>ComputeLowestSwitchDiff(false,nil) then
    Err;
end;

{ TMinXLevel }

constructor TMinXLevel.Create(aGraph: TMinXGraph; aIndex: integer);
var
  i: Integer;
  GraphNode: TLvlGraphNode;
  Node: TMinXNode;
begin
  Index:=aIndex;
  Graph:=aGraph;
  GraphLevel:=Graph.Graph.Levels[Index];
  SetLength(Nodes,GraphLevel.Count);
  SetLength(BestNodes,length(Nodes));
  for i:=0 to length(Nodes)-1 do begin
    GraphNode:=GraphLevel[i];
    Node:=Graph.GraphNodeToNode(GraphNode);
    Node.Level:=Self;
    Node.IndexInLevel:=i;
    Nodes[i]:=Node;
    BestNodes[i]:=GraphNode;
  end;
end;

destructor TMinXLevel.Destroy;
var
  i: Integer;
begin
  SetLength(Pairs,0);
  for i:=0 to length(Nodes)-1 do
    Nodes[i].Free;
  SetLength(Nodes,0);
  SetLength(BestNodes,0);
  inherited Destroy;
end;

procedure TMinXLevel.GetCrossingCount(Node1, Node2: TMinXNode; out
  Crossing, SwitchCrossing: integer);
var
  i: Integer;
  j: Integer;
begin
  Crossing:=0;
  SwitchCrossing:=0;
  for i:=0 to length(Node1.OutEdges)-1 do begin
    for j:=0 to length(Node2.OutEdges)-1 do begin
      if Node1.OutEdges[i]=Node2.OutEdges[j] then continue;
      // these two edges can cross
      if (Node1.IndexInLevel<Node2.IndexInLevel)
        <>(Node1.OutEdges[i].IndexInLevel<Node2.OutEdges[j].IndexInLevel)
      then
        Crossing+=1
      else
        SwitchCrossing+=1;
    end;
  end;
  for i:=0 to length(Node1.InEdges)-1 do begin
    for j:=0 to length(Node2.InEdges)-1 do begin
      if Node1.InEdges[i]=Node2.InEdges[j] then continue;
      // these two edges can cross
      if (Node1.IndexInLevel<Node2.IndexInLevel)
        <>(Node1.InEdges[i].IndexInLevel<Node2.InEdges[j].IndexInLevel)
      then
        Crossing+=1
      else
        SwitchCrossing+=1;
    end;
  end;
end;

{ TMinXNode }

constructor TMinXNode.Create(aNode: TLvlGraphNode);
begin
  GraphNode:=aNode;
end;

destructor TMinXNode.Destroy;
begin
  SetLength(InEdges,0);
  SetLength(OutEdges,0);
  inherited Destroy;
end;

{ TLvlGraphNodeStyle }

procedure TLvlGraphNodeStyle.SetCaptionPosition(
  AValue: TLvlGraphNodeCaptionPosition);
begin
  if FCaptionPosition=AValue then Exit;
  FCaptionPosition:=AValue;
  Control.InvalidateAutoLayout;
end;

procedure TLvlGraphNodeStyle.SetCaptionScale(AValue: single);
begin
  if FCaptionScale=AValue then Exit;
  FCaptionScale:=AValue;
  Control.InvalidateAutoLayout;
end;

procedure TLvlGraphNodeStyle.SetGapBottom(AValue: integer);
begin
  if FGapBottom=AValue then Exit;
  FGapBottom:=AValue;
  Control.InvalidateAutoLayout;
end;

procedure TLvlGraphNodeStyle.SetGapLeft(AValue: integer);
begin
  if FGapLeft=AValue then Exit;
  FGapLeft:=AValue;
  Control.InvalidateAutoLayout;
end;

procedure TLvlGraphNodeStyle.SetGapRight(AValue: integer);
begin
  if FGapRight=AValue then Exit;
  FGapRight:=AValue;
  Control.InvalidateAutoLayout;
end;

procedure TLvlGraphNodeStyle.SetGapTop(AValue: integer);
begin
  if FGapTop=AValue then Exit;
  FGapTop:=AValue;
  Control.InvalidateAutoLayout;
end;

procedure TLvlGraphNodeStyle.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  Control.InvalidateAutoLayout;
end;

constructor TLvlGraphNodeStyle.Create(AControl: TCustomLvlGraphControl);
begin
  FControl:=AControl;
  FWidth:=DefaultLvlGraphNodeWith;
  FGapLeft:=DefaultLvlGraphNodeGapLeft;
  FGapTop:=DefaultLvlGraphNodeGapTop;
  FGapRight:=DefaultLvlGraphNodeGapRight;
  FGapBottom:=DefaultLvlGraphNodeGapBottom;
  FCaptionScale:=DefaultLvlGraphNodeCaptionScale;
  FCaptionPosition:=DefaultLvlGraphNodeCaptionPosition;
end;

destructor TLvlGraphNodeStyle.Destroy;
begin
  FControl.FNodeStyle:=nil;
  inherited Destroy;
end;

procedure TLvlGraphNodeStyle.Assign(Source: TPersistent);
var
  Src: TLvlGraphNodeStyle;
begin
  if Source is TLvlGraphNodeStyle then begin
    Src:=TLvlGraphNodeStyle(Source);
    Width:=Src.Width;
    GapLeft:=Src.GapLeft;
    GapRight:=Src.GapRight;
    GapTop:=Src.GapTop;
    GapBottom:=Src.GapBottom;
    CaptionScale:=Src.CaptionScale;
    CaptionPosition:=Src.CaptionPosition;
  end else
    inherited Assign(Source);
end;

function TLvlGraphNodeStyle.Equals(Obj: TObject): boolean;
var
  Src: TLvlGraphNodeStyle;
begin
  Result:=inherited Equals(Obj);
  if not Result then exit;
  if Obj is TLvlGraphNodeStyle then begin
    Src:=TLvlGraphNodeStyle(Obj);
    Result:=(Width=Src.Width)
        and (GapLeft=Src.GapLeft)
        and (GapRight=Src.GapRight)
        and (GapTop=Src.GapTop)
        and (GapBottom=Src.GapBottom)
        and (CaptionScale=Src.CaptionScale)
        and (CaptionPosition=Src.CaptionPosition);
  end;
end;

{ TLvlGraphLevel }

function TLvlGraphLevel.GetNodes(Index: integer): TLvlGraphNode;
begin
  Result:=TLvlGraphNode(fNodes[Index]);
end;

procedure TLvlGraphLevel.SetDrawPosition(AValue: integer);
begin
  if FDrawPosition=AValue then Exit;
  FDrawPosition:=AValue;
  Invalidate;
end;

procedure TLvlGraphLevel.MoveNode(Node: TLvlGraphNode; NewIndexInLevel: integer
  );
var
  OldIndexInLevel: Integer;
begin
  OldIndexInLevel:=fNodes.IndexOf(Node);
  if OldIndexInLevel=NewIndexInLevel then exit;
  fNodes.Move(OldIndexInLevel,NewIndexInLevel);
end;

constructor TLvlGraphLevel.Create(TheGraph: TLvlGraph; TheIndex: integer);
begin
  FGraph:=TheGraph;
  FGraph.fLevels.Add(Self);
  FIndex:=TheIndex;
  fNodes:=TFPList.Create;
  if Graph<>nil then
    Graph.StructureChanged(Self,opInsert);
end;

destructor TLvlGraphLevel.Destroy;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Nodes[i].OnLevelDestroy;
  if Count>0 then
    raise Exception.Create('');
  FreeAndNil(fNodes);
  Graph.InternalRemoveLevel(Self);
  inherited Destroy;
end;

procedure TLvlGraphLevel.Invalidate;
begin
  if Graph<>nil then
    Graph.Invalidate;
end;

function TLvlGraphLevel.IndexOf(Node: TLvlGraphNode): integer;
begin
  for Result:=0 to Count-1 do
    if Nodes[Result]=Node then exit;
  Result:=-1;
end;

function TLvlGraphLevel.Count: integer;
begin
  Result:=fNodes.Count;
end;

function TLvlGraphLevel.GetTotalInOutWeights: single;
var
  i: Integer;
  Node: TLvlGraphNode;
begin
  Result:=0;
  for i:=0 to Count-1 do begin
    Node:=Nodes[i];
    Result+=Max(Node.InWeight,Node.OutWeight);
  end;
end;

{ TCustomLvlGraphControl }

procedure TCustomLvlGraphControl.GraphInvalidate(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomLvlGraphControl.GraphStructureChanged(Sender,
  Element: TObject; Operation: TOperation);
begin
  if ((Element is TLvlGraphNode)
  or (Element is TLvlGraphEdge)) then begin
    if Operation=opRemove then begin
      if FNodeUnderMouse=Element then
        FNodeUnderMouse:=nil;
    end;
    //debugln(['TCustomLvlGraphControl.GraphStructureChanged ']);
    if lgoAutoLayout in FOptions then
      InvalidateAutoLayout;
  end;
end;

procedure TCustomLvlGraphControl.SetNodeUnderMouse(AValue: TLvlGraphNode);
begin
  if FNodeUnderMouse=AValue then Exit;
  FNodeUnderMouse:=AValue;
  Invalidate;
end;

procedure TCustomLvlGraphControl.DrawEdges(Highlighted: boolean);

  procedure HighlightNode(Node: TLvlGraphNode; HighlightedElements: TAvgLvlTree;
    FollowIn, FollowOut: boolean);
  var
    i: Integer;
    Edge: TLvlGraphEdge;
  begin
    if HighlightedElements.Find(Node)<>nil then exit;
    HighlightedElements.Add(Node);
    if FollowIn then
      for i:=0 to Node.InEdgeCount-1 do begin
        Edge:=Node.InEdges[i];
        HighlightedElements.Add(Edge);
        if not Edge.Source.Visible then
          HighlightNode(Edge.Source,HighlightedElements,true,false);
      end;
    if FollowOut then
      for i:=0 to Node.OutEdgeCount-1 do begin
        Edge:=Node.OutEdges[i];
        HighlightedElements.Add(Edge);
        if not Edge.Target.Visible then
          HighlightNode(Edge.Target,HighlightedElements,false,true);
      end;
  end;

var
  i: Integer;
  Level: TLvlGraphLevel;
  j: Integer;
  Node: TLvlGraphNode;
  k: Integer;
  Edge: TLvlGraphEdge;
  TargetNode: TLvlGraphNode;
  x1, y1, x2, y2: Integer;
  HighlightedElements: TAvgLvlTree;
  EdgeHighlighted: Boolean;
begin
  HighlightedElements:=TAvgLvlTree.Create;
  try
    if NodeUnderMouse<>nil then
      HighlightNode(NodeUnderMouse,HighlightedElements,true,true);
    for i:=0 to Graph.LevelCount-1 do begin
      Level:=Graph.Levels[i];
      for j:=0 to Level.Count-1 do begin
        Node:=Level.Nodes[j];
        for k:=0 to Node.OutEdgeCount-1 do begin
          Edge:=Node.OutEdges[k];
          TargetNode:=Edge.Target;
          EdgeHighlighted:=HighlightedElements.Find(Edge)<>nil;
          if EdgeHighlighted<>Highlighted then continue;
          x1:=Level.DrawPosition-ScrollLeft;
          y1:=Node.DrawCenter-ScrollTop;
          x2:=TargetNode.Level.DrawPosition-ScrollLeft;
          y2:=TargetNode.DrawCenter-ScrollTop;
          if TargetNode.Level.Index>Level.Index then begin
            // normal dependency
            // => draw line from right of Node to left of TargetNode
            if Node.Visible then
              x1+=NodeStyle.Width
            else
              x1+=NodeStyle.Width div 2;
            if not TargetNode.Visible then
              x2+=NodeStyle.Width div 2;
            if EdgeHighlighted then
              Canvas.Pen.Color:=clGray
            else
              Canvas.Pen.Color:=clSilver;
            Canvas.Line(x1,y1,x2,y2);
          end else begin
            // cycle dependency
            // => draw line from left of Node to right of TargetNode
            if not Node.Visible then
              x1+=NodeStyle.Width div 2;
            if TargetNode.Visible then
              x2+=NodeStyle.Width div 2
            else
              x2+=NodeStyle.Width div 2;
            Canvas.Pen.Color:=clRed;
            Canvas.Line(x1,y1,x2+NodeStyle.Width,y2);
          end;
        end;
      end;
    end;
  finally
    HighlightedElements.Free;
  end;
end;

procedure TCustomLvlGraphControl.GraphSelectionChanged(Sender: TObject);
begin
  if OnSelectionChanged<>nil then
    OnSelectionChanged(Self);
end;

procedure TCustomLvlGraphControl.DrawCaptions(const TxtH: integer);
var
  Node: TLvlGraphNode;
  j: Integer;
  Level: TLvlGraphLevel;
  i: Integer;
  TxtW: Integer;
  p: TPoint;
begin
  Canvas.Font.Height:=round(single(TxtH)*NodeStyle.CaptionScale+0.5);
  for i:=0 to Graph.LevelCount-1 do begin
    Level:=Graph.Levels[i];
    for j:=0 to Level.Count-1 do begin
      Node:=Level.Nodes[j];
      if (Node.Caption='') or (not Node.Visible) then continue;
      TxtW:=Canvas.TextWidth(Node.Caption);
      case NodeStyle.CaptionPosition of
      lgncLeft,lgncRight: p.y:=Node.DrawCenter-(TxtH div 2);
      lgncTop: p.y:=Node.DrawPosition-NodeStyle.GapTop-TxtH;
      lgncBottom: p.y:=Node.DrawPositionEnd+NodeStyle.GapBottom;
      end;
      case NodeStyle.CaptionPosition of
      lgncLeft: p.x:=Level.DrawPosition-NodeStyle.GapLeft-TxtW;
      lgncRight: p.x:=Level.DrawPosition+NodeStyle.Width+NodeStyle.GapRight;
      lgncTop,lgncBottom: p.x:=Level.DrawPosition+((NodeStyle.Width-TxtW) div 2);
      end;
      //debugln(['TCustomLvlGraphControl.Paint ',Node.Caption,' DrawPosition=',Node.DrawPosition,' DrawSize=',Node.DrawSize,' TxtH=',TxtH,' TxtW=',TxtW,' p=',dbgs(p),' Selected=',Node.Selected]);
      if Node.Selected then begin
        Canvas.Brush.Style:=bsSolid;
        Canvas.Brush.Color:=clHighlight;
      end else begin
        Canvas.Brush.Style:=bsClear;
        Canvas.Brush.Color:=clNone;
      end;
      Canvas.TextOut(p.x-ScrollLeft,p.y-ScrollTop,Node.Caption);
    end;
  end;
end;

procedure TCustomLvlGraphControl.DrawNodes;
var
  i: Integer;
  Level: TLvlGraphLevel;
  j: Integer;
  Node: TLvlGraphNode;
begin
  Canvas.Brush.Style:=bsSolid;
  for i:=0 to Graph.LevelCount-1 do begin
    Level:=Graph.Levels[i];
    for j:=0 to Level.Count-1 do begin
      Node:=Level.Nodes[j];
      if not Node.Visible then continue;
      //debugln(['TCustomLvlGraphControl.Paint ',Node.Caption,' ',dbgs(FPColorToTColor(Node.Color)),' Level.DrawPosition=',Level.DrawPosition,' Node.DrawPosition=',Node.DrawPosition,' ',Node.DrawPositionEnd]);
      Canvas.Brush.Color:=FPColorToTColor(Node.Color);
      Canvas.Pen.Color:=Darker(Canvas.Brush.Color);
      Canvas.Rectangle(Level.DrawPosition-ScrollLeft, Node.DrawPosition-ScrollTop,
        Level.DrawPosition+NodeStyle.Width-ScrollLeft, Node.DrawPositionEnd-ScrollTop);
    end;
  end;
end;

procedure TCustomLvlGraphControl.SetNodeStyle(AValue: TLvlGraphNodeStyle);
begin
  if FNodeStyle=AValue then Exit;
  FNodeStyle.Assign(AValue);
end;

procedure TCustomLvlGraphControl.SetOptions(AValue: TLvlGraphCtrlOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  InvalidateAutoLayout;
end;

procedure TCustomLvlGraphControl.SetScrollLeft(AValue: integer);
begin
  AValue:=Max(0,Min(AValue,ScrollLeftMax));
  if FScrollLeft=AValue then Exit;
  FScrollLeft:=AValue;
  UpdateScrollBars;
  Invalidate;
end;

procedure TCustomLvlGraphControl.SetScrollTop(AValue: integer);
begin
  AValue:=Max(0,Min(AValue,ScrollTopMax));
  if FScrollTop=AValue then Exit;
  FScrollTop:=AValue;
  UpdateScrollBars;
  Invalidate;
end;

procedure TCustomLvlGraphControl.UpdateScrollBars;
var
  ScrollInfo: TScrollInfo;
  DrawSize: TPoint;
begin
  if HandleAllocated and (not (lgcUpdatingScrollBars in FFlags)) then begin
    Include(FFlags,lgcUpdatingScrollBars);
    DrawSize:=GetDrawSize;
    FScrollTopMax:=DrawSize.Y-ClientHeight+2*BorderWidth;
    FScrollTop:=Max(0,Min(FScrollTop,ScrollTopMax));
    FScrollLeftMax:=DrawSize.X-ClientWidth+2*BorderWidth;
    FScrollLeft:=Max(0,Min(FScrollLeft,ScrollLeftMax));
    //debugln(['TCustomLvlGraphControl.UpdateScrollBars ',dbgs(DrawSize),' ClientRect=',dbgs(ClientRect),' ScrollLeft=',ScrollLeft,'/',ScrollLeftMax,' ScrollTop=',ScrollTop,'/',ScrollTopMax,' ']);

    // vertical scrollbar
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMax := DrawSize.Y;
    ScrollInfo.nPage := Max(1,ClientHeight-1);
    ScrollInfo.nPos := ScrollTop;
    ShowScrollBar(Handle, SB_VERT, True);
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

    // horizontal scrollbar
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMax := DrawSize.X;
    ScrollInfo.nPage := Max(1,ClientWidth-1);
    ScrollInfo.nPos := ScrollLeft;
    ShowScrollBar(Handle, SB_Horz, True);
    SetScrollInfo(Handle, SB_Horz, ScrollInfo, True);

    Exclude(FFlags,lgcUpdatingScrollBars);
  end;
end;

procedure TCustomLvlGraphControl.WMHScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
    SB_TOP:        ScrollLeft := 0;
    SB_BOTTOM:     ScrollLeft := ScrollLeftMax;
    SB_LINEDOWN:   ScrollLeft := ScrollLeft + NodeStyle.Width div 2;
    SB_LINEUP:     ScrollLeft := ScrollLeft - NodeStyle.Width div 2;
    SB_PAGEDOWN:   ScrollLeft := ScrollLeft + ClientWidth - NodeStyle.Width;
    SB_PAGEUP:     ScrollLeft := ScrollLeft - ClientWidth + NodeStyle.Width;
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrollLeft := Msg.Pos;
    SB_ENDSCROLL:  SetCaptureControl(nil); // release scrollbar capture
  end;
end;

procedure TCustomLvlGraphControl.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
    SB_TOP:        ScrollTop := 0;
    SB_BOTTOM:     ScrollTop := ScrollTopMax;
    SB_LINEDOWN:   ScrollTop := ScrollTop + NodeStyle.Width div 2;
    SB_LINEUP:     ScrollTop := ScrollTop - NodeStyle.Width div 2;
    SB_PAGEDOWN:   ScrollTop := ScrollTop + ClientHeight - NodeStyle.Width;
    SB_PAGEUP:     ScrollTop := ScrollTop - ClientHeight + NodeStyle.Width;
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrollTop := Msg.Pos;
    SB_ENDSCROLL:  SetCaptureControl(nil); // release scrollbar capture
  end;
end;

procedure TCustomLvlGraphControl.WMMouseWheel(var Message: TLMMouseEvent);
begin
  if Mouse.WheelScrollLines=-1 then
  begin
    // -1 : scroll by page
    ScrollTop := ScrollTop -
              (Message.WheelDelta * (ClientHeight - NodeStyle.Width)) div 120;
  end else begin
    // scrolling one line -> scroll half an item, see SB_LINEDOWN and SB_LINEUP
    // handler in WMVScroll
    ScrollTop := ScrollTop -
        (Message.WheelDelta * Mouse.WheelScrollLines*NodeStyle.Width) div 240;
  end;
  Message.Result := 1;
end;

procedure TCustomLvlGraphControl.AutoLayoutLevels(TxtH: LongInt);
// compute all Levels.DrawPosition
var
  j: Integer;
  p: Integer;
  i: Integer;
  LevelTxtWidths: array of integer;
  Level: TLvlGraphLevel;
begin
  Canvas.Font.Height:=round(single(TxtH)*NodeStyle.CaptionScale+0.5);
  if Graph.LevelCount=0 then exit;
  SetLength(LevelTxtWidths,Graph.LevelCount);
  for i:=0 to Graph.LevelCount-1 do begin
    // compute needed width of the level
    LevelTxtWidths[i]:=Max(NodeStyle.Width,Canvas.TextWidth('NodeX'));
    Level:=Graph.Levels[i];
    for j:=0 to Level.Count-1 do
      if Level[j].Visible then
        LevelTxtWidths[i]:=Max(LevelTxtWidths[i], Canvas.TextWidth(Level[j].Caption));

    if i=0 then begin
      // first level
      case NodeStyle.CaptionPosition of
      lgncLeft: p:=NodeStyle.GapRight+LevelTxtWidths[0]+NodeStyle.GapLeft;
      lgncRight: p:=NodeStyle.GapLeft;
      lgncTop,lgncBottom: p:=NodeStyle.GapLeft+((LevelTxtWidths[0]-NodeStyle.Width) div 2);
      end;
    end else begin
      // following level
      p:=Graph.Levels[i-1].DrawPosition;
      case NodeStyle.CaptionPosition of
      lgncLeft: p+=NodeStyle.Width+NodeStyle.GapRight+LevelTxtWidths[i]+NodeStyle.GapLeft;
      lgncRight: p+=NodeStyle.Width+NodeStyle.GapRight+LevelTxtWidths[i-1]+NodeStyle.GapLeft;
      lgncTop,lgncBottom:
        p+=((LevelTxtWidths[i-1]+LevelTxtWidths[i]) div 2)+NodeStyle.GapRight+NodeStyle.GapLeft;
      end;
    end;
    Graph.Levels[i].DrawPosition:=p;
  end;
  SetLength(LevelTxtWidths,0);
end;

procedure TCustomLvlGraphControl.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateScrollBars;
end;

procedure TCustomLvlGraphControl.Paint;
var
  w: Integer;
  TxtH: integer;
begin
  inherited Paint;

  Canvas.Font.Assign(Font);

  if (lgoAutoLayout in FOptions)
  and (lgcNeedAutoLayout in FFlags) then begin
    Include(FFlags,lgcIgnoreGraphInvalidate);
    try
      AutoLayout;
    finally
      Exclude(FFlags,lgcIgnoreGraphInvalidate);
    end;
  end;

  // background
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clWhite;
  Canvas.FillRect(ClientRect);

  TxtH:=Canvas.TextHeight('ABCTM');

  // header
  if Caption<>'' then begin
    w:=Canvas.TextWidth(Caption);
    Canvas.TextOut((ClientWidth-w) div 2-ScrollLeft,round(0.25*TxtH)-ScrollTop,Caption);
  end;

  // draw
  DrawEdges(false); // draw normal edges
  DrawCaptions(TxtH);
  DrawEdges(true); // draw highlighted edges
  DrawNodes;
end;

procedure TCustomLvlGraphControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  NodeUnderMouse:=GetNodeAt(X,Y);
end;

procedure TCustomLvlGraphControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node: TLvlGraphNode;
begin
  BeginUpdate;
  try
    inherited MouseDown(Button, Shift, X, Y);
    Node:=GetNodeAt(X,Y);
    if Node<>nil then begin
      if Button=mbLeft then begin
        if lgoMouseSelects in Options then begin
          if ssCtrl in Shift then begin
            // toggle selection
            Node.Selected:=not Node.Selected;
          end else begin
            // single selection
            Graph.ClearSelection;
            Node.Selected:=true;
          end;
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomLvlGraphControl.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBars;
end;

constructor TCustomLvlGraphControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEdgeSplitMode:=DefaultLvlGraphEdgeSplitMode;
  FOptions:=DefaultLvlGraphCtrlOptions;
  FGraph:=TLvlGraph.Create;
  FGraph.OnInvalidate:=@GraphInvalidate;
  FGraph.OnSelectionChanged:=@GraphSelectionChanged;
  FGraph.OnStructureChanged:=@GraphStructureChanged;
  FNodeStyle:=TLvlGraphNodeStyle.Create(Self);
end;

destructor TCustomLvlGraphControl.Destroy;
begin
  FreeAndNil(FGraph);
  FreeAndNil(FNodeStyle);
  inherited Destroy;
end;

procedure TCustomLvlGraphControl.EraseBackground(DC: HDC);
begin
  // Paint paints all, no need to erase background
end;

procedure TCustomLvlGraphControl.Clear;
begin
  BeginUpdate;
  try
    Graph.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TCustomLvlGraphControl.AutoLayout(RndColors: boolean);
{ Min/MaxPixelPerWeight: used to scale Node.DrawSize depending on weight of
                         incoming and outgoing edges
  NodeGap: space between nodes
}
var
  HeaderHeight: integer;
  Palette: TCodyCtrlPalette;
  TxtH: LongInt;
  GapTop: Integer;
  GapBottom: Integer;
begin
  debugln(['TCustomLvlGraphControl.AutoLayout ',DbgSName(Self),' ClientRect=',dbgs(ClientRect)]);
  BeginUpdate;
  try
    Canvas.Font.Assign(Font);

    if HandleAllocated then
      TxtH:=Canvas.TextHeight('M')
    else
      TxtH:=Max(10,abs(Font.Height));
    if Caption<>'' then begin
      HeaderHeight:=round(1.5*TxtH);
    end else
      HeaderHeight:=0;

    // distribute the nodes on levels and mark back edges
    Graph.CreateTopologicalLevels(lgoHighLevels in Options);

    Graph.SplitLongEdges(EdgeSplitMode);

    // Level DrawPosition
    AutoLayoutLevels(TxtH);

    GapTop:=NodeStyle.GapTop;
    GapBottom:=NodeStyle.GapBottom;
    case NodeStyle.CaptionPosition of
    lgncTop: GapTop+=TxtH;
    lgncBottom: GapBottom+=TxtH;
    end;

    // scale Nodes.DrawSize
    // Preferably the smallest node should be the size of the text
    // Preferably the largest level should fit without needing a scrollbar
    Graph.ScaleNodeDrawSizes(GapTop,GapBottom,Screen.Height*2,1,
      ClientHeight-HeaderHeight,round(single(TxtH)*NodeStyle.CaptionScale+0.5));

    // sort nodes within levels to avoid crossings
    if OnMinimizeCrossings<>nil then
      OnMinimizeCrossings(Self)
    else
      Graph.MinimizeCrossings;

    // position nodes without overlapping
    Graph.MinimizeOverlappings(HeaderHeight,GapTop,GapBottom);

    UpdateScrollBars;

    // node colors
    if RndColors then begin
      Palette:=GetCCPaletteRGB(Graph.NodeCount,true);
      Graph.SetColors(Palette);
      SetLength(Palette,0);
    end;

    Exclude(FFlags,lgcNeedAutoLayout);
  finally
    EndUpdate;
  end;
end;

procedure TCustomLvlGraphControl.Invalidate;
begin
  if lgcIgnoreGraphInvalidate in FFlags then
    exit;
  if fUpdateLock>0 then begin
    Include(FFlags,lgcNeedInvalidate);
    exit;
  end;
  Exclude(FFlags,lgcNeedInvalidate);
  inherited Invalidate;
end;

procedure TCustomLvlGraphControl.InvalidateAutoLayout;
begin
  if lgoAutoLayout in Options then
    Include(FFlags,lgcNeedAutoLayout);
  Invalidate;
end;

procedure TCustomLvlGraphControl.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TCustomLvlGraphControl.EndUpdate;
begin
  if fUpdateLock=0 then
    raise Exception.Create('');
  dec(fUpdateLock);
  if fUpdateLock=0 then begin
    if [lgcNeedAutoLayout,lgcNeedInvalidate]*FFlags<>[] then
      Invalidate;
  end;
end;

function TCustomLvlGraphControl.GetNodeAt(X, Y: integer): TLvlGraphNode;
var
  l: Integer;
  Level: TLvlGraphLevel;
  n: Integer;
  Node: TLvlGraphNode;
begin
  Result:=nil;
  X+=ScrollLeft;
  Y+=ScrollTop;
  // check in reverse painting order
  for l:=Graph.LevelCount-1 downto 0 do begin
    Level:=Graph.Levels[l];
    if (X<Level.DrawPosition) or (X>=Level.DrawPosition+NodeStyle.Width) then continue;
    for n:=Level.Count-1 downto 0 do begin
      Node:=Level.Nodes[n];
      if not Node.Visible then continue;
      if (Y<Node.DrawPosition) or (Y>=Node.DrawPositionEnd) then continue;
      exit(Node);
    end;
  end;
end;

class function TCustomLvlGraphControl.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=200;
  Result.cy:=200;
end;

function TCustomLvlGraphControl.GetDrawSize: TPoint;
var
  l: Integer;
  Level: TLvlGraphLevel;
  n: Integer;
  Node: TLvlGraphNode;
  x: LongInt;
begin
  Result:=Point(0,0);
  for l:=0 to Graph.LevelCount-1 do begin
    Level:=Graph.Levels[l];
    for n:=0 to Level.Count-1 do begin
      Node:=Level[n];
      Result.Y:=Max(Result.Y,Node.DrawPositionEnd+NodeStyle.GapBottom);
      x:=NodeStyle.GapRight;
      if Node.OutEdgeCount>0 then
        x:=Max(x,NodeStyle.Width);
      x+=Level.DrawPosition+NodeStyle.Width;
      Result.X:=Max(Result.X,x);
    end;
  end;
end;

type

  { TGraphLevelerNode - used by TLvlGraph.UpdateLevels }

  TGraphLevelerNode = class
  public
    Node: TLvlGraphNode;
    Level: integer;
    Visited: boolean;
    InPath: boolean; // = node on stack
  end;

function CompareGraphLevelerNodes(Node1, Node2: Pointer): integer;
var
  LNode1: TGraphLevelerNode absolute Node1;
  LNode2: TGraphLevelerNode absolute Node2;
begin
  Result:=ComparePointer(LNode1.Node,LNode2.Node);
end;

function CompareLGNodeWithLevelerNode(GNode, LNode: Pointer): integer;
var
  LevelerNode: TGraphLevelerNode absolute LNode;
begin
  Result:=ComparePointer(GNode,LevelerNode.Node);
end;

{ TLvlGraph }

function TLvlGraph.GetNodes(Index: integer): TLvlGraphNode;
begin
  Result:=TLvlGraphNode(FNodes[Index]);
end;

procedure TLvlGraph.SetLevelCount(AValue: integer);
begin
  if AValue<1 then
    raise Exception.Create('at least one level');
  if LevelCount=AValue then Exit;
  while LevelCount<AValue do
    FLevelClass.Create(Self,LevelCount);
  while LevelCount>AValue do
    Levels[LevelCount-1].Free;
end;

procedure TLvlGraph.InternalRemoveNode(Node: TLvlGraphNode);
begin
  FNodes.Remove(Node);
  Node.FGraph:=nil;
  StructureChanged(Node,opRemove);
end;

function TLvlGraph.GetLevels(Index: integer): TLvlGraphLevel;
begin
  Result:=TLvlGraphLevel(fLevels[Index]);
end;

function TLvlGraph.GetLevelCount: integer;
begin
  Result:=fLevels.Count;
end;

constructor TLvlGraph.Create;
begin
  FNodeClass:=TLvlGraphNode;
  FEdgeClass:=TLvlGraphEdge;
  FLevelClass:=TLvlGraphLevel;
  FNodes:=TFPList.Create;
  fLevels:=TFPList.Create;
end;

destructor TLvlGraph.Destroy;
begin
  Clear;
  FreeAndNil(fLevels);
  FreeAndNil(FNodes);
  inherited Destroy;
end;

procedure TLvlGraph.Clear;
var
  i: Integer;
begin
  while NodeCount>0 do
    Nodes[NodeCount-1].Free;
  for i:=LevelCount-1 downto 0 do
    Levels[i].Free;
end;

procedure TLvlGraph.Invalidate;
begin
  if OnInvalidate<>nil then
    OnInvalidate(Self);
end;

procedure TLvlGraph.StructureChanged(Element: TObject; Operation: TOperation);
begin
  if Assigned(OnStructureChanged) then
    OnStructureChanged(Self,Element,Operation);
end;

function TLvlGraph.NodeCount: integer;
begin
  Result:=FNodes.Count;
end;

function TLvlGraph.GetNode(aCaption: string; CreateIfNotExists: boolean
  ): TLvlGraphNode;
var
  i: Integer;
begin
  i:=NodeCount-1;
  while (i>=0) and (aCaption<>Nodes[i].Caption) do dec(i);
  if i>=0 then begin
    Result:=Nodes[i];
  end else if CreateIfNotExists then begin
    if LevelCount=0 then
      LevelCount:=1;
    Result:=FNodeClass.Create(Self,aCaption,Levels[0]);
    FNodes.Add(Result);
    StructureChanged(Result,opInsert);
  end else
    Result:=nil;
end;

function TLvlGraph.CreateHiddenNode(Level: integer): TLvlGraphNode;
begin
  Result:=FNodeClass.Create(Self,'',Levels[Level]);
  Result.Visible:=false;
  FNodes.Add(Result);
  StructureChanged(Result,opInsert);
end;

procedure TLvlGraph.ClearSelection;
begin
  while FirstSelected<>nil do
    FirstSelected.Selected:=false;
end;

procedure TLvlGraph.SingleSelect(Node: TLvlGraphNode);
begin
  if (Node=FirstSelected) and (Node.NextSelected=nil) then exit;
  Node.Selected:=true;
  while FirstSelected<>Node do
    FirstSelected.Selected:=false;
end;

function TLvlGraph.IsMultiSelection: boolean;
begin
  Result:=(FirstSelected<>nil) and (FirstSelected.NextSelected<>nil);
end;

function TLvlGraph.GetEdge(SourceCaption, TargetCaption: string;
  CreateIfNotExists: boolean): TLvlGraphEdge;
var
  Source: TLvlGraphNode;
  Target: TLvlGraphNode;
begin
  Source:=GetNode(SourceCaption,CreateIfNotExists);
  if Source=nil then exit(nil);
  Target:=GetNode(TargetCaption,CreateIfNotExists);
  if Target=nil then exit(nil);
  Result:=GetEdge(Source,Target,CreateIfNotExists);
end;

function TLvlGraph.GetEdge(Source, Target: TLvlGraphNode;
  CreateIfNotExists: boolean): TLvlGraphEdge;
begin
  Result:=Source.FindOutEdge(Target);
  if Result<>nil then exit;
  if CreateIfNotExists then begin
    Result:=FEdgeClass.Create(Source,Target);
    StructureChanged(Result,opInsert);
  end;
end;

procedure TLvlGraph.InternalRemoveLevel(Lvl: TLvlGraphLevel);
var
  i: Integer;
begin
  if Levels[Lvl.Index]<>Lvl then
    raise Exception.Create('inconsistency');
  fLevels.Delete(Lvl.Index);
  // update level Index
  for i:=Lvl.Index to LevelCount-1 do
    Levels[i].FIndex:=i;
  StructureChanged(Lvl,opRemove);
end;

procedure TLvlGraph.SelectionChanged;
begin
  Invalidate;
  if OnSelectionChanged<>nil then
    OnSelectionChanged(Self);
end;

procedure TLvlGraph.CreateTopologicalLevels(HighLevels: boolean);
{$DEFINE LvlGraphConsistencyCheck}
var
  ExtNodes: TAvgLvlTree; // tree of TGraphLevelerNode sorted by Node
  MaxLevel: Integer;

  function GetExtNode(Node: TLvlGraphNode): TGraphLevelerNode;
  begin
    Result:=TGraphLevelerNode(ExtNodes.FindKey(Pointer(Node),@CompareLGNodeWithLevelerNode).Data);
  end;

  procedure Traverse(ExtNode: TGraphLevelerNode);
  var
    Node: TLvlGraphNode;
    e: Integer;
    Edge: TLvlGraphEdge;
    ExtNextNode: TGraphLevelerNode;
  begin
    if ExtNode.Visited then exit;
    ExtNode.InPath:=true;
    ExtNode.Visited:=true;
    Node:=ExtNode.Node;
    if HighLevels then begin
      for e:=0 to Node.OutEdgeCount-1 do begin
        Edge:=Node.OutEdges[e];
        ExtNextNode:=GetExtNode(Edge.Target);
        if ExtNextNode.InPath then
          Edge.FBackEdge:=true; // edge is part of a cycle
        Traverse(ExtNextNode);
        ExtNode.Level:=Max(ExtNode.Level,ExtNextNode.Level+1);
      end;
    end else begin
      for e:=0 to Node.InEdgeCount-1 do begin
        Edge:=Node.InEdges[e];
        ExtNextNode:=GetExtNode(Edge.Source);
        if ExtNextNode.InPath then
          Edge.FBackEdge:=true; // edge is part of a cycle
        Traverse(ExtNextNode);
        ExtNode.Level:=Max(ExtNode.Level,ExtNextNode.Level+1);
      end;
    end;
    MaxLevel:=Max(MaxLevel,ExtNode.Level);
    // backtrack
    ExtNode.InPath:=false;
  end;

var
  i: Integer;
  Node: TLvlGraphNode;
  ExtNode: TGraphLevelerNode;
  j: Integer;
  Edge: TLvlGraphEdge;
begin
  //WriteDebugReport('TLvlGraph.CreateTopologicalLevels START');
  {$IFDEF LvlGraphConsistencyCheck}
  ConsistencyCheck(false);
  {$ENDIF}
  ExtNodes:=TAvgLvlTree.Create(@CompareGraphLevelerNodes);
  try
    // init ExtNodes
    // clear BackEdge flags
    for i:=0 to NodeCount-1 do begin
      Node:=Nodes[i];
      ExtNode:=TGraphLevelerNode.Create;
      ExtNode.Node:=Node;
      ExtNodes.Add(ExtNode);
      for j:=0 to Node.InEdgeCount-1 do begin
        Edge:=Node.InEdges[j];
        Edge.fBackEdge:=false;
      end;
    end;
    // traverse all nodes
    MaxLevel:=0;
    for i:=0 to NodeCount-1 do begin
      Node:=Nodes[i];
      Traverse(GetExtNode(Node));
    end;
    // set levels
    LevelCount:=Max(LevelCount,MaxLevel+1);
    for i:=0 to NodeCount-1 do begin
      Node:=Nodes[i];
      ExtNode:=GetExtNode(Node);
      if HighLevels then
        Node.Level:=Levels[MaxLevel-ExtNode.Level]
      else
        Node.Level:=Levels[ExtNode.Level];
    end;
    // delete unneeded levels
    LevelCount:=MaxLevel+1;
  finally
    ExtNodes.FreeAndClear;
    ExtNodes.Free;
  end;
  //WriteDebugReport('TLvlGraph.CreateTopologicalLevels END');
  {$IFDEF LvlGraphConsistencyCheck}
  ConsistencyCheck(true);
  {$ENDIF}
end;

procedure TLvlGraph.SplitLongEdges(SplitMode: TLvlGraphEdgeSplitMode);
// replace edges over several levels into several short edges by adding
// hidden nodes
var
  n: Integer;
  SourceNode: TLvlGraphNode;
  e: Integer;
  Edge: TLvlGraphEdge;
  TargetNode: TLvlGraphNode;
  l: Integer;
  LastNode: TLvlGraphNode;
  NextNode: TLvlGraphNode;
  HiddenNodes: array of TLvlGraphNode;
  Weight: Single;
begin
  if SplitMode=lgesNone then exit;
  case SplitMode of
  lgesNone: ;

  lgesSeparate,lgesMergeSource:
    begin
      // lgesSeparate: each long edge gets its own hidden nodes, this creates a lot of hidden nodes
      // lgesMergeSource: each node has a list of hidden nodes, long edges share this list from source to target
      // lgesMergeTarget: ToDo: each node has a list of hidden nodes, long edges share this list from target to source
      SetLength(HiddenNodes,LevelCount-1);
      for n:=0 to NodeCount-1 do begin
        SourceNode:=Nodes[n];
        for e:=SourceNode.Level.Index+1 to LevelCount-2 do
          HiddenNodes[e]:=nil;
        for e:=SourceNode.OutEdgeCount-1 downto 0 do begin
          Edge:=SourceNode.OutEdges[e];
          TargetNode:=Edge.Target;
          if TargetNode.Level.Index-SourceNode.Level.Index<=1 then continue;
          debugln(['TLvlGraph.SplitLongEdges long edge: ',SourceNode.Caption,' ',TargetNode.Caption]);
          Weight:=Edge.Weight;
          // remove long edge
          Edge.Free;
          LastNode:=SourceNode;
          for l:=SourceNode.Level.Index+1 to TargetNode.Level.Index do begin
            if l<TargetNode.Level.Index then begin
              NextNode:=HiddenNodes[l];
              if NextNode=nil then begin
                NextNode:=CreateHiddenNode(l);
                if SplitMode=lgesMergeSource then
                  HiddenNodes[l]:=NextNode;
              end;
            end else
              NextNode:=TargetNode;
            Edge:=GetEdge(LastNode,NextNode,true);
            Edge.Weight:=Edge.Weight+Weight;
            LastNode:=NextNode;
          end;
        end;
      end;
    end;

  lgesMergeTarget:
    begin
      // lgesMergeTarget: each node has a list of hidden nodes, long edges share this list from target to source
      SetLength(HiddenNodes,LevelCount-1);
      for n:=0 to NodeCount-1 do begin
        TargetNode:=Nodes[n];
        for e:=1 to TargetNode.Level.Index-1 do
          HiddenNodes[e]:=nil;
        for e:=TargetNode.InEdgeCount-1 downto 0 do begin
          Edge:=TargetNode.InEdges[e];
          SourceNode:=Edge.Source;
          if TargetNode.Level.Index-SourceNode.Level.Index<=1 then continue;
          debugln(['TLvlGraph.SplitLongEdges long edge: ',SourceNode.Caption,' ',TargetNode.Caption]);
          Weight:=Edge.Weight;
          // remove long edge
          Edge.Free;
          LastNode:=TargetNode;
          for l:=TargetNode.Level.Index-1 downto SourceNode.Level.Index do begin
            if l>SourceNode.Level.Index then begin
              NextNode:=HiddenNodes[l];
              if NextNode=nil then begin
                NextNode:=CreateHiddenNode(l);
                HiddenNodes[l]:=NextNode;
              end;
            end else
              NextNode:=SourceNode;
            Edge:=GetEdge(NextNode,LastNode,true);
            Edge.Weight:=Edge.Weight+Weight;
            LastNode:=NextNode;
          end;
        end;
      end;
    end;

  end;
end;

procedure TLvlGraph.ScaleNodeDrawSizes(NodeGapAbove, NodeGapBelow,
  HardMaxTotal, HardMinOneNode, SoftMaxTotal, SoftMinOneNode: integer);
{ NodeGapAbove: minimum space above each node
  NodeGapBelow: minimum space below each node
  HardMaxTotal: maximum size of largest level
  HardMinOneNode: minimum size of a node
  SoftMaxTotal: preferred maximum size of the largest level, total can be bigger
                to achieve HardMinOneNode
  SoftMinOneNode: preferred minimum size of a node, can be smaller to achieve
                  SoftMaxTotal
  Order of precedence: HardMinOneNode, SoftMaxTotal, SoftMinOneNode
}
var
  SmallestWeight: Single;
  i: Integer;
  Node: TLvlGraphNode;
  j: Integer;
  Edge: TLvlGraphEdge;
  Level: TLvlGraphLevel;
  LvlWeight: Single;
  MinPixelPerWeight, PrefMinPixelPerWeight: single;
  DrawHeight: integer;
  PixelPerWeight, MaxPixelPerWeight, PrefMaxPixelPerWeight: single;
  Gap: Integer;
begin
  //debugln(['TLvlGraph.ScaleNodeDrawSizes',
  //  ' NodeGapAbove=',NodeGapAbove,' NodeGapBelow=',NodeGapBelow,
  //  ' HardMaxTotal=',HardMaxTotal,' HardMinOneNode=',HardMinOneNode,
  //  ' SoftMaxTotal=',SoftMaxTotal,' SoftMinOneNode=',SoftMinOneNode]);

  // sanitize input
  HardMinOneNode:=Max(0,HardMinOneNode);
  SoftMinOneNode:=Max(SoftMinOneNode,HardMinOneNode);
  HardMaxTotal:=Max(1,HardMaxTotal);
  SoftMaxTotal:=Min(Max(1,SoftMaxTotal),HardMaxTotal);

  SmallestWeight:=-1.0;
  for i:=0 to NodeCount-1 do begin
    Node:=Nodes[i];
    for j:=0 to Node.OutEdgeCount-1 do begin
      Edge:=Node.OutEdges[j];
      if Edge.Weight<=0.0 then continue;
      if (SmallestWeight<0) or (SmallestWeight>Edge.Weight) then
        SmallestWeight:=Edge.Weight;
    end;
  end;
  if SmallestWeight<0 then SmallestWeight:=1.0;
  if SmallestWeight>0 then begin
    MinPixelPerWeight:=single(HardMinOneNode)/SmallestWeight;
    PrefMinPixelPerWeight:=single(SoftMinOneNode)/SmallestWeight;
  end else begin
    MinPixelPerWeight:=single(HardMinOneNode);
    PrefMinPixelPerWeight:=single(SoftMinOneNode);
  end;
  //debugln(['TLvlGraph.ScaleNodeDrawSizes SmallestWeight=',SmallestWeight,
  //  ' MinPixelPerWeight=',MinPixelPerWeight,
  //  ' PrefMinPixelPerWeight=',PrefMinPixelPerWeight]);

  MaxPixelPerWeight:=0.0;
  PrefMaxPixelPerWeight:=0.0;
  for i:=0 to LevelCount-1 do begin
    Level:=Levels[i];
    // LvlWeight = how much weight to draw
    // DrawHeight - how much pixel left to draw the weight
    LvlWeight:=0.0;
    Gap:=0;
    DrawHeight:=HardMaxTotal;
    for j:=0 to Level.Count-1 do begin
      LvlWeight+=Max(Node.InWeight,Node.OutWeight);
      if Node.Visible then
        Gap+=NodeGapAbove+NodeGapBelow
      else
        Gap+=1;
    end;
    if LvlWeight=0.0 then continue;
    DrawHeight:=Max(1,HardMaxTotal-Gap);
    PixelPerWeight:=single(DrawHeight)/LvlWeight;
    if (MaxPixelPerWeight=0.0) or (MaxPixelPerWeight>PixelPerWeight) then
      MaxPixelPerWeight:=PixelPerWeight;
    DrawHeight:=Max(1,SoftMaxTotal-Gap);
    PixelPerWeight:=single(DrawHeight)/LvlWeight;
    if (PrefMaxPixelPerWeight=0.0) or (PrefMaxPixelPerWeight>PixelPerWeight) then
      PrefMaxPixelPerWeight:=PixelPerWeight;
  end;
  //debugln(['TLvlGraph.ScaleNodeDrawSizes MaxPixelPerWeight=',MaxPixelPerWeight,' PrefMaxPixelPerWeight=',PrefMaxPixelPerWeight]);

  PixelPerWeight:=PrefMinPixelPerWeight;
  if PrefMaxPixelPerWeight>0.0 then
    PixelPerWeight:=Min(PixelPerWeight,PrefMaxPixelPerWeight);
  PixelPerWeight:=Max(PixelPerWeight,MinPixelPerWeight);
  if MaxPixelPerWeight>0.0 then
    PixelPerWeight:=Min(PixelPerWeight,MaxPixelPerWeight);

  //debugln(['TLvlGraph.ScaleNodeDrawSizes PixelPerWeight=',PixelPerWeight]);
  SetAllNodeDrawSizes(PixelPerWeight,SmallestWeight);
end;

procedure TLvlGraph.SetAllNodeDrawSizes(PixelPerWeight: single;
  MinWeight: single);
var
  i: Integer;
  Node: TLvlGraphNode;
begin
  for i:=0 to NodeCount-1 do begin
    Node:=Nodes[i];
    Node.DrawSize:=round(Max(MinWeight,Max(Node.InWeight,Node.OutWeight))*PixelPerWeight+0.5);
  end;
end;

procedure TLvlGraph.MarkBackEdges;
var
  i: Integer;
  Node: TLvlGraphNode;
  j: Integer;
  Edge: TLvlGraphEdge;
begin
  for i:=0 to NodeCount-1 do begin
    Node:=Nodes[i];
    for j:=0 to Node.OutEdgeCount-1 do begin
      Edge:=Node.OutEdges[j];
      Edge.fBackEdge:=Edge.IsBackEdge;
    end;
  end;
end;

procedure TLvlGraph.MinimizeCrossings;
begin
  LvlGraphMinimizeCrossings(Self);
end;

procedure TLvlGraph.MinimizeOverlappings(MinPos: integer;
  NodeGapAbove: integer; NodeGapBelow: integer; aLevel: integer);
var
  i: Integer;
  Level: TLvlGraphLevel;
  Node: TLvlGraphNode;
  Last: TLvlGraphNode;
begin
  if aLevel<0 then begin
    for i:=0 to LevelCount-1 do
      MinimizeOverlappings(MinPos,NodeGapAbove,NodeGapBelow,i);
  end else begin
    Level:=Levels[aLevel];
    Last:=nil;
    for i:=0 to Level.Count-1 do begin
      Node:=Level[i];
      if Last=nil then
        Node.DrawPosition:=MinPos+NodeGapAbove
      else if Node.Visible then
        Node.DrawPosition:=Max(Node.DrawPosition,Last.DrawPositionEnd+NodeGapBelow+NodeGapAbove)
      else
        Node.DrawPosition:=Max(Node.DrawPosition,Last.DrawPositionEnd+1);
      //debugln(['TLvlGraph.MinimizeOverlappings Level=',aLevel,' Node=',Node.Caption,' Size=',Node.DrawSize,' Position=',Node.DrawPosition]);
      Last:=Node;
    end;
  end;
end;

procedure TLvlGraph.SetColors(Palette: TCodyCtrlPalette);
var
  i: Integer;
begin
  for i:=0 to NodeCount-1 do
    Nodes[i].Color:=Palette[i];
end;

procedure TLvlGraph.WriteDebugReport(Msg: string);
var
  l: Integer;
  Level: TLvlGraphLevel;
  i: Integer;
  Node: TLvlGraphNode;
  Edge: TLvlGraphEdge;
  j: Integer;
begin
  debugln([Msg,' NodeCount=',NodeCount,' LevelCount=',LevelCount]);
  debugln(['  Nodes:']);
  for i:=0 to NodeCount-1 do begin
    Node:=Nodes[i];
    dbgout(['   ',i,'/',NodeCount,': "',Node.Caption,'" OutEdges:']);
    for j:=0 to Node.OutEdgeCount-1 do begin
      Edge:=Node.OutEdges[j];
      dbgout('"',Edge.Target.Caption,'",');
    end;
    debugln;
  end;
  debugln(['  Levels:']);
  for l:=0 to LevelCount-1 do begin
    dbgout(['   Level: ',l,'/',LevelCount]);
    Level:=Levels[l];
    if l<>Level.Index then
      debugln(['ERROR: l<>Level.Index=',Level.Index]);
    dbgout('  ');
    for i:=0 to Level.Count-1 do begin
      dbgout('"',Level.Nodes[i].Caption,'",');
    end;
    debugln;
  end;
end;

procedure TLvlGraph.ConsistencyCheck(WithBackEdge: boolean);
var
  i: Integer;
  Node: TLvlGraphNode;
  j: Integer;
  Edge: TLvlGraphEdge;
  Level: TLvlGraphLevel;
begin
  for i:=0 to LevelCount-1 do begin
    Level:=Levels[i];
    if Level.Index<>i then
      raise Exception.Create('');
    for j:=0 to Level.Count-1 do begin
      Node:=Level.Nodes[j];
      if Node.Level<>Level then
        raise Exception.Create('');
      if Level.IndexOf(Node)<j then
        raise Exception.Create('');
    end;
  end;
  for i:=0 to NodeCount-1 do begin
    Node:=Nodes[i];
    for j:=0 to Node.OutEdgeCount-1 do begin
      Edge:=Node.OutEdges[j];
      if Edge.Source<>Node then
        raise Exception.Create('');
      if Edge.Target.FInEdges.IndexOf(Edge)<0 then
        raise Exception.Create('');
      if WithBackEdge and (Edge.BackEdge<>Edge.IsBackEdge) then
        raise Exception.Create('Edge.BackEdge '+Edge.AsString+' Edge.BackEdge='+dbgs(Edge.BackEdge)+' Edge.IsBackEdge='+dbgs(Edge.IsBackEdge)+' Source.Index='+dbgs(Edge.Source.Level.Index)+' Target.Index='+dbgs(Edge.Target.Level.Index));
    end;
    for j:=0 to Node.InEdgeCount-1 do begin
      Edge:=Node.InEdges[j];
      if Edge.Target<>Node then
        raise Exception.Create('');
      if Edge.Source.FOutEdges.IndexOf(Edge)<0 then
        raise Exception.Create('');
    end;
    if Node.Level.fNodes.IndexOf(Node)<0 then
      raise Exception.Create('');
  end;
end;

{ TLvlGraphEdge }

procedure TLvlGraphEdge.SetWeight(AValue: single);
var
  Diff: single;
begin
  if AValue<0.0 then AValue:=0.0;
  if FWeight=AValue then Exit;
  Diff:=AValue-FWeight;
  Source.FOutWeight+=Diff;
  Target.FInWeight+=Diff;
  FWeight:=AValue;
  Source.Invalidate;
end;

constructor TLvlGraphEdge.Create(TheSource: TLvlGraphNode;
  TheTarget: TLvlGraphNode);
begin
  FSource:=TheSource;
  FTarget:=TheTarget;
  Source.FOutEdges.Add(Self);
  Target.FInEdges.Add(Self);
end;

destructor TLvlGraphEdge.Destroy;
var
  OldGraph: TLvlGraph;
begin
  OldGraph:=Source.Graph;
  Source.FOutEdges.Remove(Self);
  Target.FInEdges.Remove(Self);
  FSource:=nil;
  FTarget:=nil;
  if OldGraph<>nil then
    OldGraph.StructureChanged(Self,opRemove);
  inherited Destroy;
end;

function TLvlGraphEdge.IsBackEdge: boolean;
begin
  Result:=Source.Level.Index>=Target.Level.Index;
end;

function TLvlGraphEdge.AsString: string;
begin
  Result:='('+Source.Caption+'->'+Target.Caption+')';
end;

{ TLvlGraphNode }

function TLvlGraphNode.GetInEdges(Index: integer): TLvlGraphEdge;
begin
  Result:=TLvlGraphEdge(FInEdges[Index]);
end;

function TLvlGraphNode.GetIndexInLevel: integer;
begin
  if Level=nil then exit(-1);
  Result:=Level.IndexOf(Self);
end;

function TLvlGraphNode.GetOutEdges(Index: integer): TLvlGraphEdge;
begin
  Result:=TLvlGraphEdge(FOutEdges[Index]);
end;

procedure TLvlGraphNode.SetCaption(AValue: string);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  Invalidate;
end;

procedure TLvlGraphNode.SetColor(AValue: TFPColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  Invalidate;
end;

procedure TLvlGraphNode.OnLevelDestroy;
begin
  if Level.Index>0 then
    Level:=Graph.Levels[0]
  else if Graph.LevelCount>1 then
    Level:=Graph.Levels[1]
  else
    fLevel:=nil;
end;

procedure TLvlGraphNode.SetDrawSize(AValue: integer);
begin
  if FDrawSize=AValue then Exit;
  FDrawSize:=AValue;
  Invalidate;
end;

procedure TLvlGraphNode.SetIndexInLevel(AValue: integer);
begin
  Level.MoveNode(Self,AValue);
end;

procedure TLvlGraphNode.SetLevel(AValue: TLvlGraphLevel);
begin
  if AValue=nil then
    raise Exception.Create('node needs a level');
  if AValue.Graph<>Graph then
    raise Exception.Create('wrong graph');
  if FLevel=AValue then Exit;
  if FLevel<>nil then
    UnbindLevel;
  FLevel:=AValue;
  FLevel.fNodes.Add(Self);
end;

procedure TLvlGraphNode.SetSelected(AValue: boolean);

  procedure Unselect;
  begin
    if FPrevSelected<>nil then
      FPrevSelected.FNextSelected:=FNextSelected
    else
      Graph.FFirstSelected:=FNextSelected;
    if FNextSelected<>nil then
      FNextSelected.FPrevSelected:=FPrevSelected
    else
      Graph.FLastSelected:=FPrevSelected;
    FNextSelected:=nil;
    FPrevSelected:=nil;
  end;

  procedure Select;
  begin
    FPrevSelected:=Graph.LastSelected;
    if FPrevSelected<>nil then
      FPrevSelected.FNextSelected:=Self
    else
      Graph.FFirstSelected:=Self;
    Graph.FLastSelected:=Self;
  end;

begin
  if FSelected=AValue then begin
    if Graph=nil then exit;
    if Graph.LastSelected=Self then exit;
    // make this node the last selected
    Unselect;
    Select;
    SelectionChanged;
    exit;
  end;
  // change Selected
  FSelected:=AValue;
  if Graph<>nil then begin
    if Selected then begin
      Select;
    end else begin
      Unselect;
    end;
  end;
  SelectionChanged;
end;

procedure TLvlGraphNode.SetVisible(AValue: boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  Invalidate;
end;

procedure TLvlGraphNode.UnbindLevel;
begin
  if FLevel<>nil then
    FLevel.fNodes.Remove(Self);
end;

procedure TLvlGraphNode.SelectionChanged;
begin
  if Graph<>nil then
    Graph.SelectionChanged;
end;

procedure TLvlGraphNode.Invalidate;
begin
  if Graph<>nil then
    Graph.Invalidate;
end;

constructor TLvlGraphNode.Create(TheGraph: TLvlGraph; TheCaption: string;
  TheLevel: TLvlGraphLevel);
begin
  FGraph:=TheGraph;
  FCaption:=TheCaption;
  FInEdges:=TFPList.Create;
  FOutEdges:=TFPList.Create;
  FDrawSize:=1;
  FVisible:=true;
  Level:=TheLevel;
end;

destructor TLvlGraphNode.Destroy;
begin
  Selected:=false;
  Clear;
  UnbindLevel;
  if Graph<>nil then
    Graph.InternalRemoveNode(Self);
  FreeAndNil(FInEdges);
  FreeAndNil(FOutEdges);
  inherited Destroy;
end;

procedure TLvlGraphNode.Clear;
begin
  while InEdgeCount>0 do
    InEdges[InEdgeCount-1].Free;
  while OutEdgeCount>0 do
    OutEdges[OutEdgeCount-1].Free;
end;

function TLvlGraphNode.IndexOfInEdge(Source: TLvlGraphNode): integer;
begin
  for Result:=0 to InEdgeCount-1 do
    if InEdges[Result].Source=Source then exit;
  Result:=-1;
end;

function TLvlGraphNode.FindInEdge(Source: TLvlGraphNode): TLvlGraphEdge;
var
  i: Integer;
begin
  i:=IndexOfInEdge(Source);
  if i>=0 then
    Result:=InEdges[i]
  else
    Result:=nil;
end;

function TLvlGraphNode.InEdgeCount: integer;
begin
  Result:=FInEdges.Count;
end;

function TLvlGraphNode.IndexOfOutEdge(Target: TLvlGraphNode): integer;
begin
  for Result:=0 to OutEdgeCount-1 do
    if OutEdges[Result].Target=Target then exit;
  Result:=-1;
end;

function TLvlGraphNode.FindOutEdge(Target: TLvlGraphNode): TLvlGraphEdge;
var
  i: Integer;
begin
  i:=IndexOfOutEdge(Target);
  if i>=0 then
    Result:=OutEdges[i]
  else
    Result:=nil;
end;

function TLvlGraphNode.OutEdgeCount: integer;
begin
  Result:=FOutEdges.Count;
end;

function TLvlGraphNode.DrawCenter: integer;
begin
  Result:=DrawPosition+(DrawSize div 2);
end;

function TLvlGraphNode.DrawPositionEnd: integer;
begin
  Result:=DrawPosition+DrawSize;
end;

{ TCodyTreeView }

procedure TCodyTreeView.FreeNodeData;
begin
  FreeTVNodeData(Self);
end;

{ TCustomCircleDiagramControl }

procedure TCustomCircleDiagramControl.SetCategoryGapDegree16(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if AValue>0.3 then AValue:=0.3;
  if FCategoryGapDegree16=AValue then Exit;
  FCategoryGapDegree16:=AValue;
  UpdateLayout;
end;

function TCustomCircleDiagramControl.GetCategories(Index: integer
  ): TCircleDiagramCategory;
begin
  Result:=TCircleDiagramCategory(fCategories[Index]);
end;

procedure TCustomCircleDiagramControl.SetCenterCaption(AValue: TCaption);
begin
  if FCenterCaption=AValue then Exit;
  FCenterCaption:=AValue;
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.SetFirstCategoryDegree16(AValue: single);
begin
  if FFirstCategoryDegree16=AValue then Exit;
  FFirstCategoryDegree16:=AValue;
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.InternalRemoveCategory(
  Category: TCircleDiagramCategory);
begin
  fCategories.Remove(Category);
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;

procedure TCustomCircleDiagramControl.UpdateScrollBar;
begin

end;

procedure TCustomCircleDiagramControl.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateLayout;
  UpdateScrollBar;
end;

procedure TCustomCircleDiagramControl.Paint;
var
  i: Integer;
begin
  inherited Paint;
  if cdcNeedUpdateLayout in fFlags then
    UpdateLayout;

  // background
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=Color;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color:=clRed;

  // draw categories
  for i:=0 to CategoryCount-1 do
    DrawCategory(i);

  // center caption
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clNone;
  Canvas.TextOut(FCenterCaptionRect.Left,FCenterCaptionRect.Top,CenterCaption);
end;

procedure TCustomCircleDiagramControl.DrawCategory(i: integer);
var
  Cat: TCircleDiagramCategory;
begin
  Cat:=Categories[i];
  Canvas.Brush.Color:=Cat.Color;
  RingSector(Canvas,Center.X-OuterRadius,Center.Y-OuterRadius,
    Center.X+OuterRadius,Center.Y+OuterRadius,
    single(InnerRadius)/single(OuterRadius),
    Cat.StartDegree16,Cat.EndDegree16);
end;

constructor TCustomCircleDiagramControl.Create(AOwner: TComponent);
begin
  BeginUpdate;
  try
    inherited Create(AOwner);
    fCategories:=TObjectList.create(true);
    FFirstCategoryDegree16:=DefaultFirstCategoryDegree16;
    FCategoryGapDegree16:=DefaultCategoryGapDegree16;
    Color:=clWhite;
  finally
    EndUpdate;
  end;
end;

destructor TCustomCircleDiagramControl.Destroy;
begin
  BeginUpdate; // disable updates
  Clear;
  FreeAndNil(fCategories);
  inherited Destroy;
end;

procedure TCustomCircleDiagramControl.Clear;
begin
  if CategoryCount=0 then exit;
  BeginUpdate;
  try
    while CategoryCount>0 do
      fCategories.Delete(CategoryCount-1);
  finally
    EndUpdate;
  end;
end;

procedure TCustomCircleDiagramControl.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TCustomCircleDiagramControl.EndUpdate;
begin
  if fUpdateLock=0 then
    raise Exception.Create('TCustomCircleDiagramControl.EndUpdate');
  dec(fUpdateLock);
  if fUpdateLock=0 then begin
    if cdcNeedUpdateLayout in fFlags then
      UpdateLayout;
  end;
end;

procedure TCustomCircleDiagramControl.UpdateLayout;
var
  aSize: TSize;
  aCategory: TCircleDiagramCategory;
  i: Integer;
  j: Integer;
  TotalSize: Single;
  CurCategoryDegree: Single;
  GapDegree: Single;
  TotalItemDegree: Single;
  Item: TCircleDiagramItem;
  CurItemDegree: Single;
begin
  if (fUpdateLock>0) or (not IsVisible) or (not HandleAllocated) then begin
    Include(fFlags,cdcNeedUpdateLayout);
    exit;
  end;
  Exclude(fFlags,cdcNeedUpdateLayout);

  // center caption
  FCenter:=Point(ClientWidth div 2,ClientHeight div 2);
  aSize:=Canvas.TextExtent(CenterCaption);
  FCenterCaptionRect:=Bounds(FCenter.X-(aSize.cx div 2),FCenter.Y-(aSize.cy div 2)
    ,aSize.cx,aSize.cy);

  // radius
  fInnerRadius:=0.24*Min(ClientWidth,ClientHeight);
  fOuterRadius:=1.2*InnerRadius;

  // degrees
  TotalSize:=0.0;
  CurCategoryDegree:=FirstCategoryDegree16;
  if CategoryCount>0 then begin
    // calculate TotalSize
    for i:=0 to CategoryCount-1 do begin
      aCategory:=Categories[i];
      aCategory.FSize:=0;
      for j:=0 to aCategory.Count-1 do
        aCategory.FSize+=aCategory[j].Size;
      aCategory.FSize:=Max(aCategory.FSize,aCategory.MinSize);
      TotalSize+=aCategory.FSize;
    end;

    // calculate degrees
    GapDegree:=Min(CategoryGapDegree16,(0.8/CategoryCount)*FullCircle16);
    TotalItemDegree:=FullCircle16-(GapDegree*CategoryCount);
    for i:=0 to CategoryCount-1 do begin
      aCategory:=Categories[i];
      aCategory.FStartDegree16:=CurCategoryDegree;
      if TotalSize>0 then
        CurCategoryDegree+=TotalItemDegree*aCategory.Size/TotalSize;
      aCategory.FEndDegree16:=CurCategoryDegree;

      // item degrees
      CurItemDegree:=aCategory.StartDegree16;
      for j:=0 to aCategory.Count-1 do begin
        Item:=aCategory[j];

        Item.FStartDegree16:=CurItemDegree;
        if aCategory.Size>0 then
          CurItemDegree+=(aCategory.EndDegree16-aCategory.StartDegree16)*Item.Size/aCategory.Size;
        Item.FEndDegree16:=CurItemDegree;
      end;

      CurCategoryDegree+=GapDegree;
    end;
  end;

  Invalidate;
  WriteDebugReport('TCustomCircleDiagramControl.UpdateLayout');
end;

procedure TCustomCircleDiagramControl.EraseBackground(DC: HDC);
begin
  // do not erase background, Paint will paint the whole area
end;

function TCustomCircleDiagramControl.InsertCategory(Index: integer;
  aCaption: TCaption): TCircleDiagramCategory;
begin
  Result:=TCircleDiagramCategory.Create(Self);
  Result.Caption:=aCaption;
  fCategories.Insert(Index,Result);
end;

function TCustomCircleDiagramControl.AddCategory(aCaption: TCaption
  ): TCircleDiagramCategory;
begin
  Result:=InsertCategory(CategoryCount,aCaption);
end;

function TCustomCircleDiagramControl.IndexOfCategory(aCaption: TCaption
  ): integer;
begin
  Result:=CategoryCount-1;
  while Result>=0 do begin
    if Categories[Result].Caption=aCaption then exit;
    dec(Result);
  end;
end;

function TCustomCircleDiagramControl.FindCategory(aCaption: TCaption
  ): TCircleDiagramCategory;
var
  i: Integer;
begin
  i:=IndexOfCategory(aCaption);
  if i>=0 then
    Result:=Categories[i]
  else
    Result:=nil;
end;

function TCustomCircleDiagramControl.CategoryCount: integer;
begin
  Result:=fCategories.Count;
end;

procedure TCustomCircleDiagramControl.WriteDebugReport(Msg: string);
var
  aCat: TCircleDiagramCategory;
  i: Integer;
  j: Integer;
  Item: TCircleDiagramItem;
begin
  DebugLn([Msg,' CategoryCount=',CategoryCount]);
  for i:=0 to CategoryCount-1 do begin
    aCat:=Categories[i];
    debugln(['  Category: ',i,'/',CategoryCount,' ',aCat.Caption,
      ' MinSize=',aCat.MinSize,
      ' Size=',aCat.Size,
      ' Start=',round(aCat.StartDegree16),' End=',round(aCat.EndDegree16)]);
    for j:=0 to aCat.Count-1 do begin
      Item:=aCat.Items[j];
      debugln(['    Item: ',j,'/',aCat.Count,' ',Item.Caption,
        ' Size=',Item.Size,
        ' Start=',round(Item.StartDegree16),
        ' End=',round(Item.EndDegree16)]);
    end;
  end;
end;

{ TCircleDiagramCategory }

procedure TCircleDiagramCategory.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TCircleDiagramCategory.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  Invalidate;
end;

function TCircleDiagramCategory.GetItems(Index: integer): TCircleDiagramItem;
begin
  Result:=TCircleDiagramItem(fItems[Index]);
end;

procedure TCircleDiagramCategory.SetMinSize(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if FMinSize=AValue then Exit;
  FMinSize:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramCategory.UpdateLayout;
begin
  if Diagram<>nil then
    Diagram.UpdateLayout;
end;

procedure TCircleDiagramCategory.Invalidate;
begin
  if Diagram<>nil then
    Diagram.Invalidate;
end;

procedure TCircleDiagramCategory.InternalRemoveItem(Item: TCircleDiagramItem);
begin
  Item.FCategory:=nil;
  fItems.Remove(Item);
  UpdateLayout;
end;

constructor TCircleDiagramCategory.Create(
  TheDiagram: TCustomCircleDiagramControl);
begin
  FDiagram:=TheDiagram;
  fItems:=TFPList.Create;
  FMinSize:=DefaultCategoryMinSize;
end;

destructor TCircleDiagramCategory.Destroy;
begin
  if Diagram<>nil then
    Diagram.InternalRemoveCategory(Self);
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TCircleDiagramCategory.Clear;
begin
  if Count=0 then exit;
  if Diagram<>nil then
    Diagram.BeginUpdate;
  try
    while Count>0 do
      Items[Count-1].Free;
  finally
    if Diagram<>nil then
      Diagram.EndUpdate;
  end;
end;

function TCircleDiagramCategory.InsertItem(Index: integer; aCaption: string
  ): TCircleDiagramItem;
begin
  Result:=TCircleDiagramItem.Create(Self);
  Result.Caption:=aCaption;
  fItems.Insert(Index,Result);
end;

function TCircleDiagramCategory.AddItem(aCaption: string): TCircleDiagramItem;
begin
  Result:=InsertItem(Count,aCaption);
end;

function TCircleDiagramCategory.Count: integer;
begin
  Result:=fItems.Count;
end;

{ TCircleDiagramItem }

procedure TCircleDiagramItem.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramItem.SetSize(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if FSize=AValue then Exit;
  FSize:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramItem.UpdateLayout;
begin
  if Category<>nil then
    Category.UpdateLayout;
end;

constructor TCircleDiagramItem.Create(TheCategory: TCircleDiagramCategory);
begin
  FCategory:=TheCategory;
  FSize:=DefaultItemSize;
end;

destructor TCircleDiagramItem.Destroy;
begin
  if Category<>nil then
    Category.InternalRemoveItem(Self);
  inherited Destroy;
end;

end.


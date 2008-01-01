{  $Id$  }
{
 /***************************************************************************
                               Controls.pp
                             -------------------
                             Component Library Controls
                   Initial Revision : Sat Apr 10 22:49:32 CST 1999


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Controls;

{$mode objfpc}{$H+}
{off $DEFINE BUFFERED_WMPAINT}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

uses
  Classes, SysUtils, TypInfo, DynHashArray, LCLStrConsts, LCLType, AvgLvlTree,
  LCLProc, GraphType, Graphics, LMessages, LCLIntf, InterfaceBase, ImgList,
  UTrace, PropertyStorage, Menus, ActnList, LCLClasses;


const
  mrNone = 0;
  mrOK = mrNone + 1;
  mrCancel = mrNone + 2;
  mrAbort = mrNone + 3;
  mrRetry = mrNone + 4;
  mrIgnore = mrNone + 5;
  mrYes = mrNone + 6;
  mrNo = mrNone + 7;
  mrAll = mrNone + 8;
  mrNoToAll = mrNone + 9;
  mrYesToAll = mrNone + 10;
  mrLast = mrYesToAll;

  // define aliases for Delphi compatibility
  fsSurface = GraphType.fsSurface;
  fsBorder = GraphType.fsBorder;

  bvNone = GraphType.bvNone;
  bvLowered = GraphType.bvLowered;
  bvRaised = GraphType.bvRaised;
  bvSpace = GraphType.bvSpace;

type
  TWinControl = class;
  TControl = class;
  TWinControlClass = class of TWinControl;
  TControlClass = class of TControl;

  TDate = type TDateTime;
  TTime = type TDateTime;

  // ToDo: move this to a message definition unit
  TCMMouseWheel = record
    MSg: Cardinal;
    ShiftState: TShiftState;
    Unused: Byte;
    WheelData: SmallInt;
    case Integer of
    0: (
      XPos: SmallInt;
      YPos: SmallInt);
    1: (
      Pos: TSmallPoint;
      Result: LRESULT);
  end;

  TCMHitTest = TLMNCHitTest;

  TCMControlChange = record
    Msg: Cardinal;
    Control: TControl;
    Inserting: Boolean;
    Result: LRESULT;
  End;

  TCMDialogChar = TLMKEY;
  TCMDialogKey = TLMKEY;

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);
  TAlignSet = set of TAlign;
  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;
  TAnchorSideReference = (asrTop, asrBottom, asrCenter);
  
const
  asrLeft = asrTop;
  asrRight = asrBottom;

type
  TCaption = TTranslateString;
  TCursor = -32768..32767;

  TFormStyle = (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop, fsSplash);
  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow,
                      bsSizeToolWin);
  TBorderStyle = bsNone..bsSingle;
  TControlBorderStyle = TBorderStyle;

  TControlRoleForForm = (
    crffDefault,// this control is notified when user presses Return
    crffCancel  // this control is notified when user presses Escape
    );
  TControlRolesForForm = set of TControlRoleForForm;

  TBevelCut = TGraphicsBevelCut;

  TMouseButton = (mbLeft, mbRight, mbMiddle);

const
  fsAllStayOnTop = [fsStayOnTop, fsSplash];

  // Cursor constants
  crHigh        = TCursor(0);

  crDefault     = TCursor(0);
  crNone        = TCursor(-1);
  crArrow       = TCursor(-2);
  crCross       = TCursor(-3);
  crIBeam       = TCursor(-4);
  crSize        = TCursor(-22);
  crSizeNESW    = TCursor(-6); // diagonal north east - south west
  crSizeNS      = TCursor(-7);
  crSizeNWSE    = TCursor(-8);
  crSizeWE      = TCursor(-9);
  crSizeNW      = TCursor(-23);
  crSizeN       = TCursor(-24);
  crSizeNE      = TCursor(-25);
  crSizeW       = TCursor(-26);
  crSizeE       = TCursor(-27);
  crSizeSW      = TCursor(-28);
  crSizeS       = TCursor(-29);
  crSizeSE      = TCursor(-30);
  crUpArrow     = TCursor(-10);
  crHourGlass   = TCursor(-11);
  crDrag        = TCursor(-12);
  crNoDrop      = TCursor(-13);
  crHSplit      = TCursor(-14);
  crVSplit      = TCursor(-15);
  crMultiDrag   = TCursor(-16);
  crSQLWait     = TCursor(-17);
  crNo          = TCursor(-18);
  crAppStart    = TCursor(-19);
  crHelp        = TCursor(-20);
  crHandPoint   = TCursor(-21);
  crSizeAll     = TCursor(-22);

  crLow         = TCursor(-30);

type
  TCaptureMouseButtons = set of TMouseButton;

  TWndMethod = procedure(var TheMessage: TLMessage) of Object;

  TControlStyleType = (
    csAcceptsControls, // can have childs in the designer
    csCaptureMouse,
    csDesignInteractive, // wants mouse events in design mode
    csClickEvents,
    csFramed,
    csSetCaption,
    csOpaque,
    csDoubleClicks,// control understands mouse double clicks
    csTripleClicks,// control understands mouse triple clicks
    csQuadClicks,  // control understands mouse quad clicks
    csFixedWidth,
    csFixedHeight, // control cannot change it height (for example combobox)
    csNoDesignVisible,
    csReplicatable,
    csNoStdEvents,
    csDisplayDragImage,
    csReflector,
    csActionClient,
    csMenuEvents,
    csNoFocus,
    csNeedsBorderPaint, // not implemented
    csParentBackground, // not implemented
    csDesignNoSmoothResize, // no WYSIWYG resizing in designer
    csDesignFixedBounds, // control can not be moved nor resized in designer
    csHasDefaultAction, // control implements useful ExecuteDefaultAction
    csHasCancelAction,   // control implements useful ExecuteCancelAction
    csNoDesignSelectable, // control can not be selected at design time
    csOwnedChildsSelectable // child controls owned by this control are selectable in the designer
    );
  TControlStyle = set of TControlStyleType;

const
  csMultiClicks = [csDoubleClicks,csTripleClicks,csQuadClicks];


type
  TControlStateType = (
    csLButtonDown,
    csClicked,
    csPalette,
    csReadingState,
    csAlignmentNeeded,
    csFocusing,
    csCreating,
    csPaintCopy,
    csCustomPaint,
    csDestroyingHandle,
    csDocking,
    csVisibleSetInLoading
    );
  TControlState = set of TControlStateType;


  { TControlCanvas }

  TControlCanvas = class(TCanvas)
  private
    FControl: TControl;
    FDeviceContext: HDC;
    FWindowHandle: HWND;
    procedure SetControl(AControl: TControl);
    procedure CreateFont; override;
  protected
    procedure CreateHandle; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeHandle;override;
    property Control: TControl read FControl write SetControl;
  end;
  
  { Hint stuff }
  
  PHintInfo = ^THintInfo;
  THintInfo = record
    HintControl: TControl;
    HintWindowClass: TWinControlClass;
    HintPos: TPoint;
    HintMaxWidth: Integer;
    HintColor: TColor;
    CursorRect: TRect;
    CursorPos: TPoint;
    ReshowTimeout: Integer;
    HideTimeout: Integer;
    HintStr: string;
    HintData: Pointer;
  end;


  { TDragImageList }

  TDragImageList = class(TCustomImageList)
  private
    FDragCursor: TCursor;
    FDragging: Boolean;
    FDragHotspot: TPoint;
    FOldCursor: TCursor;
    FImageIndex: Integer;
    FLastDragPos: TPoint;
    FLockedWindow: HWND;
    procedure SetDragCursor(const AValue: TCursor);
  protected
    procedure Initialize; override;
  public
    function BeginDrag(Window: HWND; X, Y: Integer): Boolean;
    function DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
    function DragMove(X, Y: Integer): Boolean;
    procedure DragUnlock;
    function EndDrag: Boolean;
    function GetHotSpot: TPoint; override;
    procedure HideDragImage;
    function SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean;
    procedure ShowDragImage;
    property DragCursor: TCursor read FDragCursor write SetDragCursor;
    property DragHotspot: TPoint read FDragHotspot write FDragHotspot;
    property Dragging: Boolean read FDragging;
  end;

  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of Object;
  TKeyPressEvent = procedure(Sender: TObject; var Key: char) of Object;
  TUTF8KeyPressEvent = procedure(Sender: TObject; var UTF8Key: TUTF8Char) of Object;

  TMouseEvent = Procedure(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer) of object;
  TMouseMoveEvent = Procedure(Sender: TObject; Shift: TShiftState;
                              X, Y: Integer) of object;
  TMouseWheelEvent = Procedure(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object;
  TMouseWheelUpDownEvent = Procedure(Sender: TObject;
          Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of object;


  { TDragObject }

  TDragObject = class;

  TDragKind = (dkDrag, dkDock);
  TDragMode = (dmManual , dmAutomatic);
  TDragState = (dsDragEnter, dsDragLeave, dsDragMove);
  TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop,
                  dmDragCancel,dmFindTarget);

  TDragOverEvent = procedure(Sender, Source: TObject;
               X,Y: Integer; State: TDragState; var Accept: Boolean) of object;
  TDragDropEvent = procedure(Sender, Source: TObject; X,Y: Integer) of object;
  TStartDragEvent = procedure(Sender: TObject; var DragObject: TDragObject) of object;
  TEndDragEvent = procedure(Sender, Target: TObject; X,Y: Integer) of object;

  TDragObject = class
  private
    FDragPos: TPoint;
    FControl: TControl;
    FDragTarget: TControl;
    FDragTargetPos: TPoint;
  protected
    function GetDragImages: TDragImageList; virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
  public
    constructor Create(AControl: TControl); virtual;

    procedure HideDragImage; virtual;
    procedure ShowDragImage; virtual;
    
    property Control: TControl read FControl write FControl;
    property DragPos: TPoint read FDragPos write FDragPos;
    property DragTarget: TControl read FDragTarget write FDragTarget;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
  end;

  TDragObjectClass = class of TDragObject;


  { TDragControlObject }

  TDragControlObject = class(TDragObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages: TDragImageList; override;
  end;


  { TDragDockObject }

  TDragDockObject = class;

  TDockOrientation = (
    doNoOrient,   // zone contains a TControl and no child zones.
    doHorizontal, // zone's children are stacked top-to-bottom.
    doVertical,   // zone's children are arranged left-to-right.
    doPages       // zone's children are pages arranged left-to-right.
    );
  TDockDropEvent = procedure(Sender: TObject; Source: TDragDockObject;
                             X, Y: Integer) of object;
  TDockOverEvent = procedure(Sender: TObject; Source: TDragDockObject;
                             X, Y: Integer; State: TDragState;
                             var Accept: Boolean) of object;
  TUnDockEvent = procedure(Sender: TObject; Client: TControl;
                          NewTarget: TWinControl; var Allow: Boolean) of object;
  TStartDockEvent = procedure(Sender: TObject;
                              var DragObject: TDragDockObject) of object;
  TGetSiteInfoEvent = procedure(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean) of object;

  TDragDockObject = class(TDragObject)
  private
    FDockRect: TRect;
    FDropAlign: TAlign;
    FDropOnControl: TControl;
    FEraseDockRect: TRect;
    FFloating: Boolean;
    FIncreaseDockArea: Boolean;
  protected
    procedure AdjustDockRect(ARect: TRect); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  public
    property DockRect: TRect read FDockRect write FDockRect;
    property DropAlign: TAlign read FDropAlign write FDropAlign;
    property DropOnControl: TControl read FDropOnControl write FDropOnControl;
    property Floating: Boolean read FFloating write FFloating;
    property IncreaseDockArea: Boolean read FIncreaseDockArea;
    property EraseDockRect: TRect read FEraseDockRect write FEraseDockRect;
  end;


  { TDragManager }

  TDragManager = class(TPersistent)
  private
    FDragImmediate: Boolean;
    FDragThreshold: Integer;
  protected
    //input capture
    procedure KeyUp(var Key: Word; Shift : TShiftState); virtual;abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;abstract;
    procedure CaptureChanged(OldCaptureControl: TControl); virtual;abstract;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual;abstract;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;abstract;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;abstract;
  public
    constructor Create; virtual;
    
    function IsDragging: boolean; virtual;abstract;
    function Dragging(AControl: TControl): boolean; virtual;abstract;
    procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean); virtual;abstract;

    procedure DragStart(AControl: TControl; AImmediate: Boolean; AThreshold: Integer);virtual;abstract;
    procedure DragMove(APosition: TPoint); virtual;abstract;
    procedure DragStop(ADrop: Boolean); virtual;abstract;
    
    property DragImmediate: Boolean read FDragImmediate write FDragImmediate default True;
    property DragThreshold: Integer read FDragThreshold write FDragThreshold default 5;
  end;
  
var
  DragManager: TDragManager;

type
  { TDockManager is an abstract class for managing a dock site's docked
    controls. See TDockTree below for the more info.
    }
  TDockManager = class(TPersistent)
  public
    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    procedure GetControlBounds(Control: TControl;
                               out AControlBounds: TRect); virtual; abstract;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
                            DropCtl: TControl); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure PaintSite(DC: HDC); virtual; abstract;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
                               var DockRect: TRect); virtual; abstract;
    procedure RemoveControl(Control: TControl); virtual; abstract;
    procedure ResetBounds(Force: Boolean); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SetReplacingControl(Control: TControl); virtual; abstract;
  end;


  { TSizeConstraints }

  TConstraintSize = 0..MaxInt;

  TSizeConstraintsOption = (
    // not yet used
    scoAdviceWidthAsMin,
    scoAdviceWidthAsMax,
    scoAdviceHeightAsMin,
    scoAdviceHeightAsMax
    );
  TSizeConstraintsOptions = set of TSizeConstraintsOption;

  TSizeConstraints = class(TPersistent)
  private
    FControl: TControl;
    FMaxHeight: TConstraintSize;
    FMaxInterfaceHeight: integer;
    FMaxInterfaceWidth: integer;
    FMaxWidth: TConstraintSize;
    FMinHeight: TConstraintSize;
    FMinInterfaceHeight: integer;
    FMinInterfaceWidth: integer;
    FMinWidth: TConstraintSize;
    FOnChange: TNotifyEvent;
    FOptions: TSizeConstraintsOptions;
    procedure SetOptions(const AValue: TSizeConstraintsOptions);
  protected
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetMaxHeight(Value: TConstraintSize); virtual;
    procedure SetMaxWidth(Value: TConstraintSize); virtual;
    procedure SetMinHeight(Value: TConstraintSize); virtual;
    procedure SetMinWidth(Value: TConstraintSize); virtual;
  public
    constructor Create(AControl: TControl); virtual;
    procedure UpdateInterfaceConstraints; virtual;
    procedure SetInterfaceConstraints(MinW, MinH, MaxW, MaxH: integer); virtual;
    function EffectiveMinWidth: integer; virtual;
    function EffectiveMinHeight: integer; virtual;
    function EffectiveMaxWidth: integer; virtual;
    function EffectiveMaxHeight: integer; virtual;
    function MinMaxWidth(Width: integer): integer;
    function MinMaxHeight(Height: integer): integer;
  public
    property MaxInterfaceHeight: integer read FMaxInterfaceHeight;
    property MaxInterfaceWidth: integer read FMaxInterfaceWidth;
    property MinInterfaceHeight: integer read FMinInterfaceHeight;
    property MinInterfaceWidth: integer read FMinInterfaceWidth;
    property Control: TControl read FControl;
    property Options: TSizeConstraintsOptions read FOptions write SetOptions default [];
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property MaxHeight: TConstraintSize read FMaxHeight write SetMaxHeight default 0;
    property MaxWidth: TConstraintSize read FMaxWidth write SetMaxWidth default 0;
    property MinHeight: TConstraintSize read FMinHeight write SetMinHeight default 0;
    property MinWidth: TConstraintSize read FMinWidth write SetMinWidth default 0;
  end;

  TConstrainedResizeEvent = procedure(Sender: TObject;
      var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize) of object;


  { TControlBorderSpacing }
  
  { TControlBorderSpacing defines the spacing around a control.
    The spacing around its childs and between its childs is defined in
    TWinControl.ChildSizing.

    Left, Top, Right, Bottom: integer;
        minimum space left to the autosized control.
        For example: Control A lies left of control B.
        A has borderspacing Right=10 and B has borderspacing Left=5.
        Then A and B will have a minimum space of 10 between.

    Around: integer;
        same as Left, Top, Right and Bottom all at once. This will be added to
        the effective Left, Top, Right and Bottom.
        Example: Left=3 and Around=5 results in a minimum spacing to the left
        of 8.

    InnerBorder: integer;
        This is added to the preferred size.
        For example: A buttons widget returns 75x25 on GetPreferredSize.
        CalculatePreferredSize adds 2 times the InnerBorder to the width and
        height.
        
    CellAlignHorizontal, CellAlignVertical: TControlCellAlign;
        Used for example when the Parents.ChildSizing.Layout defines a table
        layout.
  }
  
  TSpacingSize = 0..MaxInt;
  TControlCellAlign = (
    ccaFill,
    ccaLeftTop,
    ccaRightBottom,
    ccaCenter
    );
  TControlCellAligns = set of TControlCellAlign;

  { TControlBorderSpacing }

  TControlBorderSpacing = class(TPersistent)
  private
    FAround: TSpacingSize;
    FBottom: TSpacingSize;
    FCellAlignHorizontal: TControlCellAlign;
    FCellAlignVertical: TControlCellAlign;
    FControl: TControl;
    FInnerBorder: Integer;
    FLeft: TSpacingSize;
    FOnChange: TNotifyEvent;
    FRight: TSpacingSize;
    FTop: TSpacingSize;
    function IsInnerBorderStored: boolean;
    procedure SetAround(const AValue: TSpacingSize);
    procedure SetBottom(const AValue: TSpacingSize);
    procedure SetCellAlignHorizontal(const AValue: TControlCellAlign);
    procedure SetCellAlignVertical(const AValue: TControlCellAlign);
    procedure SetInnerBorder(const AValue: Integer);
    procedure SetLeft(const AValue: TSpacingSize);
    procedure SetRight(const AValue: TSpacingSize);
    procedure SetSpace(Kind: TAnchorKind; const AValue: integer);
    procedure SetTop(const AValue: TSpacingSize);
  protected
    procedure Change(InnerSpaceChanged: Boolean); dynamic;
  public
    constructor Create(OwnerControl: TControl);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Spacing: TControlBorderSpacing): boolean;
    procedure GetSpaceAround(var SpaceAround: TRect);
    function GetSpace(Kind: TAnchorKind): Integer;
    function GetSideSpace(Kind: TAnchorKind): Integer;
  public
    property Control: TControl read FControl;
    property Space[Kind: TAnchorKind]: integer read GetSpace write SetSpace;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Left: TSpacingSize read FLeft write SetLeft;
    property Top: TSpacingSize read FTop write SetTop;
    property Right: TSpacingSize read FRight write SetRight;
    property Bottom: TSpacingSize read FBottom write SetBottom;
    property Around: TSpacingSize read FAround write SetAround;
    property InnerBorder: Integer read FInnerBorder write SetInnerBorder stored IsInnerBorderStored default -1;
    property CellAlignHorizontal: TControlCellAlign read FCellAlignHorizontal write SetCellAlignHorizontal;
    property CellAlignVertical: TControlCellAlign read FCellAlignVertical write SetCellAlignVertical;
  end;
  
  
  { TAnchorSide
    Class holding the reference sides of the anchors of a TControl.
    Every TControl has four AnchorSides:
    AnchorSide[akLeft], AnchorSide[akRight], AnchorSide[akTop] and
    AnchorSide[akBottom].
    Normally if Anchors contain akLeft, and the Parent is resized, the LCL
    tries to keep the distance between the left side of the control and the
    right side of its parent client area.
    With AnchorSide[akLeft] you can define a different reference side. The
    kept distance is defined by the BorderSpacing and Parent.ChildSizing.
    
    Example1:
       +-----+  +-----+
       |  B  |  |  C  |
       |     |  +-----+
       +-----+

      If you want to have the top of B the same as the top of C use
        B.AnchorSide[akTop].Side:=asrTop;
        B.AnchorSide[akTop].Control:=C;
      If you want to keep a distance of 10 pixels between B and C use
        B.BorderSpacing.Right:=10;
        B.AnchorSide[akRight].Side:=asrLeft;
        B.AnchorSide[akRight].Control:=C;

      Do not setup in both directions, because this will create a circle, and
      circles are not allowed.
      
    Example2:
            +-------+
      +---+ |       |
      | A | |   B   |
      +---+ |       |
            +-------+
            
      Centering A relative to B:
        A.AnchorSide[akTop].Side:=arsCenter;
        A.AnchorSide[akTop].Control:=B;
      Or use this. It's equivalent:
        A.AnchorSide[akBottom].Side:=arsCenter;
        A.AnchorSide[akBottom].Control:=B;
    }
  TAnchorSideChangeOperation = (ascoAdd, ascoRemove, ascoChangeSide);

  { TAnchorSide }

  TAnchorSide = class(TPersistent)
  private
    FControl: TControl;
    FKind: TAnchorKind;
    FOwner: TControl;
    FSide: TAnchorSideReference;
    function IsSideStored: boolean;
    procedure SetControl(const AValue: TControl);
    procedure SetSide(const AValue: TAnchorSideReference);
  public
    constructor Create(TheOwner: TControl; TheKind: TAnchorKind);
    destructor Destroy; override;
    procedure GetSidePosition(out ReferenceControl: TControl;
                out ReferenceSide: TAnchorSideReference; out Position: Integer);
    procedure Assign(Source: TPersistent); override;
  public
    property Owner: TControl read FOwner;
    property Kind: TAnchorKind read FKind;
  published
    property Control: TControl read FControl write SetControl;
    property Side: TAnchorSideReference read FSide write SetSide default asrTop;
  end;


  { TControlActionLink }

  TControlActionLink = class(TActionLink)
  protected
    FClient: TControl;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpLinked: Boolean;  override;
    function IsHintLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    function DoShowHint(var HintStr: string): Boolean; virtual;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHelpType(Value: THelpType); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

  TControlActionLinkClass = class of TControlActionLink;


  { TControl }

  TTabOrder = -1..32767;

  TControlShowHintEvent = procedure(Sender: TObject; HintInfo: PHintInfo) of object;
  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint;
                                 var Handled: Boolean) of object;

  TControlFlag = (
    cfRequestAlignNeeded,
    cfAutoSizeNeeded,
    cfLeftLoaded,  // cfLeftLoaded is set, when 'Left' is set during loading.
    cfTopLoaded,
    cfWidthLoaded,
    cfHeightLoaded,
    cfClientWidthLoaded,
    cfClientHeightLoaded,
    cfBoundsRectForNewParentValid,
    cfBaseBoundsValid,
    cfPreferredSizeValid,
    cfPreferredMinSizeValid,
    cfOnResizeNeeded,
    cfOnChangeBoundsNeeded
    );
  TControlFlags = set of TControlFlag;

  TControlHandlerType = (
    chtOnResize,
    chtOnChangeBounds,
    chtOnVisibleChanging,
    chtOnVisibleChanged
    );

{* Note on TControl.Caption
 * The VCL implementation relies on the virtual Get/SetTextBuf to 
 * exchange text between widgets and VCL. This means a lot of 
 * (unnecesary) text copies.
 * The LCL uses strings for exchanging text (more efficient).
 * To maintain VCL compatibility, the virtual RealGet/SetText is
 * introduced. These functions interface with the LCLInterface. The
 * default Get/SetTextbuf implementation calls the RealGet/SetText.
 * As long as the Get/SetTextBuf isn't overridden Get/SetText 
 * calls RealGet/SetText to avoid PChar copying.
 * To keep things optimal, LCL implementations should always 
 * override RealGet/SetText. Get/SetTextBuf is only kept for
 * compatibility.
 }

  TControl = class(TLCLComponent)
  private
    FActionLink: TControlActionLink;
    FAlign: TAlign;
    FAnchors: TAnchors;
    FAnchorSides: array[TAnchorKind] of TAnchorSide;
    fAnchoredControls: TFPList; // list of TControl anchored to this control
    FAutoSizing: Boolean;
    FAutoSizingLockCount: Integer;
    FAutoSize: Boolean;
    FBaseBounds: TRect;
    FBaseBoundsLock: integer;
    FBaseParentClientSize: TPoint;
    FBorderSpacing: TControlBorderSpacing;
    FBoundsRectForNewParent: TRect;
    FCaption: TCaption;
    FCaptureMouseButtons: TCaptureMouseButtons;
    FColor: TColor;
    FConstraints: TSizeConstraints;
    FControlFlags: TControlFlags;
    FControlHandlers: array[TControlHandlerType] of TMethodList;
    FControlStyle: TControlStyle;
    FCtl3D: Boolean;
    FDockOrientation: TDockOrientation;
    FDragCursor: TCursor;
    FDragKind: TDragKind;
    FDragMode: TDragMode;
    FEnabled: Boolean;
    FFloatingDockSiteClass: TWinControlClass;
    FFont: TFont;
    FHeight: Integer;
    FHelpContext: THelpContext;
    FHelpKeyword: String;
    FHelpType: THelpType;
    FHint: TTranslateString;
    FHostDockSite: TWinControl;
    FIsControl: Boolean;
    fLastAlignedBounds: TRect;
    fLastAlignedBoundsTried: integer;
    FLastChangebounds: TRect;
    FLastDoChangeBounds: TRect;
    FLastDoChangeClientSize: TPoint;
    FLastResizeClientHeight: integer;
    FLastResizeClientWidth: integer;
    FLastResizeHeight: integer;
    FLastResizeWidth: integer;
    FLeft: Integer;
    FLoadedClientSize: TPoint;
    FLRDockWidth: Integer;
    FMouseEntered: boolean;
    FOnChangeBounds: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnConstrainedResize: TConstrainedResizeEvent;
    FOnContextPopup: TContextPopupEvent;
    FOnDblClick: TNotifyEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnEditingDone: TNotifyEvent;
    FOnEndDock: TEndDragEvent;
    FOnEndDrag: TEndDragEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnQuadClick: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnShowHint: TControlShowHintEvent;
    FOnStartDock: TStartDockEvent;
    FOnStartDrag: TStartDragEvent;
    FOnTripleClick: TNotifyEvent;
    FParent: TWinControl;
    FParentColor: Boolean;
    FParentFont: Boolean;
    FParentShowHint: Boolean;
    FPopupMenu: TPopupMenu;
    FPreferredMinWidth: integer;// without theme space
    FPreferredMinHeight: integer;// without theme space
    FPreferredWidth: integer;// with theme space
    FPreferredHeight: integer;// with theme space
    FReadBounds: TRect;
    FSessionProperties: string;
    FShowHint: Boolean;
    FSizeLock: integer;
    FTBDockHeight: Integer;
    FTop: Integer;
    FUndockHeight: Integer;
    FUndockWidth: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FWindowProc: TWndMethod;
    function CaptureMouseButtonsIsStored: boolean;
    procedure DoActionChange(Sender: TObject);
    function GetAnchorSide(Kind: TAnchorKind): TAnchorSide;
    function GetAnchorSideIndex(Index: integer): TAnchorSide;
    function GetAnchoredControls(Index: integer): TControl;
    function GetBoundsRect: TRect;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetLRDockWidth: Integer;
    function GetMouseCapture: Boolean;
    function GetTBDockHeight: Integer;
    function GetText: TCaption; 
    function GetUndockHeight: Integer;
    function GetUndockWidth: Integer;
    function IsAnchorsStored: boolean;
    function IsCaptionStored: Boolean;
    function IsColorStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsFontStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsHelpKeyWordStored: boolean;
    function IsOnClickStored: Boolean;
    function IsShowHintStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure DoBeforeMouseMessage;
    procedure DoConstrainedResize(var NewLeft, NewTop, NewWidth, NewHeight: integer);
    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
                          Shift: TShiftState);
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
    procedure SetAnchorSideIndex(Index: integer; const AValue: TAnchorSide);
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure SetBoundsRect(const ARect: TRect);
    procedure SetBoundsRectForNewParent(const AValue: TRect);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientSize(const Value: TPoint);
    procedure SetClientWidth(Value: Integer);
    procedure SetConstraints(const Value: TSizeConstraints);
    procedure SetDragCursor(const AValue: TCursor);
    procedure SetFont(Value: TFont);
    procedure SetHeight(Value: Integer);
    procedure SetHelpContext(const AValue: THelpContext);
    procedure SetHelpKeyword(const AValue: String);
    procedure SetHostDockSite(const AValue: TWinControl);
    procedure SetLeft(Value: Integer);
    procedure SetMouseCapture(Value: Boolean);
    procedure SetParentShowHint(Value: Boolean);
    procedure SetParentColor(Value: Boolean);
    procedure SetParentFont(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetShowHint(Value: Boolean);
    procedure SetText(const Value: TCaption); 
    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  protected
    FControlState: TControlState;
    FCursor: TCursor;
    function GetCursor: TCursor; virtual;
    procedure SetCursor(Value: TCursor); virtual;
  protected
    // sizing/aligning
    procedure DoAutoSize; virtual;
    procedure BeginAutoSizing;
    procedure EndAutoSizing;
    function AutoSizeCanStart: boolean; virtual;
    procedure AnchorSideChanged(TheAnchorSide: TAnchorSide); virtual;
    procedure ForeignAnchorSideChanged(TheAnchorSide: TAnchorSide;
                                       Operation: TAnchorSideChangeOperation); virtual;
    procedure SetAlign(Value: TAlign); virtual;
    procedure SetAnchors(const AValue: TAnchors); virtual;
    procedure SetAutoSize(const Value: Boolean); virtual;
    procedure BoundsChanged; dynamic;
    procedure DoConstraintsChange(Sender: TObject); virtual;
    procedure DoBorderSpacingChange(Sender: TObject;
                                    InnerSpaceChanged: Boolean); virtual;
    function IsBorderSpacingInnerBorderStored: Boolean; virtual;
    procedure SendMoveSizeMessages(SizeChanged, PosChanged: boolean); virtual;
    procedure ConstrainedResize(var MinWidth, MinHeight,
                                MaxWidth, MaxHeight: TConstraintSize); virtual;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         WithThemeSpace: Boolean); virtual;
    procedure DoOnResize; virtual;// call OnReresize
    procedure DoOnChangeBounds; virtual;// call OnChangeBounds
    procedure CheckOnChangeBounds;// checks for changes and calls DoOnChangeBounds
    procedure Resize; virtual;// checks for changes and calls DoOnResize
    procedure RequestAlign; dynamic;// smart calling Parent.AlignControls
    procedure UpdateAnchorRules;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    procedure ChangeScale(Multiplier, Divider: Integer); dynamic;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; virtual;
    procedure SetAlignedBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    function IsAParentAligning: boolean;
    function GetClientOrigin: TPoint; virtual;
    function GetClientRect: TRect; virtual;// visual size of client area
    function GetLogicalClientRect: TRect; virtual;// logical size of client area (e.g. in a TScrollBox the logical client area can be bigger than the visual)
    function GetScrolledClientRect: TRect; virtual;// visual client area scrolled
    function GetClientScrollOffset: TPoint; virtual;
    function GetControlOrigin: TPoint; virtual;
    function IsClientHeightStored: boolean; virtual;
    function IsClientWidthStored: boolean; virtual;
    
    property AutoSizing: Boolean Read FAutoSizing;
  protected
    // protected messages
    procedure WMContextMenu(Var Message: TLMMouse); message LM_CONTEXTMENU;
    procedure WMLButtonDown(Var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMRButtonDown(Var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMMButtonDown(Var Message: TLMMButtonDown); message LM_MBUTTONDOWN;
    procedure WMLButtonDBLCLK(Var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDBLCLK(Var Message: TLMRButtonDblClk); message LM_RBUTTONDBLCLK;
    procedure WMMButtonDBLCLK(Var Message: TLMMButtonDblClk); message LM_MBUTTONDBLCLK;
    procedure WMLButtonTripleCLK(Var Message: TLMLButtonTripleClk); message LM_LBUTTONTRIPLECLK;
    procedure WMRButtonTripleCLK(Var Message: TLMRButtonTripleClk); message LM_RBUTTONTRIPLECLK;
    procedure WMMButtonTripleCLK(Var Message: TLMMButtonTripleClk); message LM_MBUTTONTRIPLECLK;
    procedure WMLButtonQuadCLK(Var Message: TLMLButtonQuadClk); message LM_LBUTTONQUADCLK;
    procedure WMRButtonQuadCLK(Var Message: TLMRButtonQuadClk); message LM_RBUTTONQUADCLK;
    procedure WMMButtonQuadCLK(Var Message: TLMMButtonQuadClk); message LM_MBUTTONQUADCLK;
    procedure WMMouseMove(Var Message: TLMMouseMove); message LM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TLMRButtonUp); message LM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TLMMButtonUp); message LM_MBUTTONUP;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
    procedure LMCaptureChanged(Var Message: TLMessage); message LM_CaptureChanged;
    procedure CMEnabledChanged(var Message: TLMEssage); message CM_ENABLEDCHANGED;
    procedure CMHitTest(Var Message: TCMHittest) ; Message CM_HITTEST;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MouseLeave;
    procedure CMHintShow(var Message: TLMessage); message CM_HINTSHOW;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentShowHintChanged(var Message: TLMessage); message CM_PARENTSHOWHINTCHANGED;
    procedure CMVisibleChanged(var Message: TLMessage); message CM_VISIBLECHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
  protected
    // drag and drop
    procedure CalculateDockSizes;
    function CreateFloatingDockSite(const Bounds: TRect): TWinControl;
    function GetDockEdge(const MousePos: TPoint): TAlign; dynamic;
    function GetDragImages: TDragImageList; virtual;
    function GetFloating: Boolean; virtual;
    function GetFloatingDockSiteClass: TWinControlClass; virtual;
    procedure BeginAutoDrag; dynamic;
    procedure DoFloatMsg(ADockSource: TDragDockObject);dynamic;//CM_FLOAT
    procedure DockTrackNoTarget(Source: TDragDockObject; X, Y: Integer); dynamic;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); dynamic;
    function DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean):LRESULT; virtual;//Cm_Drag
    procedure DoEndDock(Target: TObject; X, Y: Integer); dynamic;
    procedure DoEndDrag(Target: TObject; X,Y: Integer); dynamic;
    procedure DoStartDock(var DragObject: TDragObject); dynamic;
    procedure DoStartDrag(var DragObject: TDragObject); dynamic;
    procedure DragCanceled; dynamic;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState;
                       var Accept: Boolean); dynamic;
    procedure PositionDockRect(DragDockObject: TDragDockObject); dynamic;
    procedure SetDragMode(Value: TDragMode); virtual;
    //procedure SendDockNotification; virtual; MG: probably not needed
  protected
    // key and mouse
    procedure Click; dynamic;
    procedure DblClick; dynamic;
    procedure TripleClick; dynamic;
    procedure QuadClick; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); Dynamic;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); dynamic;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    function  DialogChar(var Message: TLMKey): boolean; virtual;
    procedure UpdateMouseCursor(X, Y: integer);
  protected
    procedure Changed;
    function  GetPalette: HPalette; virtual;
    function ChildClassAllowed(ChildClass: TClass): boolean; virtual;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure FormEndUpdated; virtual;
    procedure InvalidateControl(CtrlIsVisible, CtrlIsOpaque: Boolean);
    procedure InvalidateControl(CtrlIsVisible, CtrlIsOpaque, IgnoreWinControls: Boolean);
    procedure FontChanged(Sender: TObject); virtual;
    procedure ParentFontChanged; virtual;
    function GetAction: TBasicAction; virtual;
    function RealGetText: TCaption; virtual;
    procedure RealSetText(const Value: TCaption); virtual;
    procedure TextChanged; virtual;
    function GetCachedText(var CachedText: TCaption): boolean; virtual;
    procedure SetAction(Value: TBasicAction); virtual;
    procedure SetColor(Value: TColor); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetHint(const Value: TTranslateString); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(NewParent: TWinControl); virtual;
    Procedure SetParentComponent(NewParentComponent: TComponent); override;
    procedure WndProc(var TheMessage: TLMessage); virtual;
    procedure ParentFormHandleInitialized; virtual; // called by ChildHandlesCreated of parent form
    procedure CaptureChanged; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function CanTab: Boolean; virtual;
    function GetDeviceContext(var WindowHandle: HWnd): HDC; virtual;
    Function GetEnabled: Boolean; virtual;
    Function GetPopupMenu: TPopupMenu; dynamic;
    procedure DoOnShowHint(HintInfo: Pointer);
    procedure VisibleChanging; dynamic;
    procedure VisibleChanged; dynamic;
    procedure AddHandler(HandlerType: TControlHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TControlHandlerType;
                            const AMethod: TMethod);
    procedure DoCallNotifyHandler(HandlerType: TControlHandlerType);
    procedure DoContextPopup(const MousePos: TPoint; var Handled: Boolean); virtual;
    procedure SetZOrder(TopMost: Boolean); virtual;
  protected
    // actions
    function GetActionLinkClass: TControlActionLinkClass; dynamic;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
  protected
    // optional properties (not every descendent supports them)
    property ActionLink: TControlActionLink read FActionLink write FActionLink;
    property Ctl3D: Boolean read FCtl3D write FCtl3D;//Is this needed for anything other than compatability?
                                                     // MWE: no and even on delphi it is depreciated. IMO we remove this
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crDrag;
    property DragKind: TDragKind read FDragKind write FDragKind default dkDrag;
    property DragMode: TDragMode read fDragMode write SetDragMode default dmManual;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property ParentColor: Boolean read FParentColor write SetParentColor default true;
    property ParentFont: Boolean  read FParentFont write SetParentFont;
    property ParentShowHint: Boolean read FParentShowHint write SetParentShowHint default True;
    property SessionProperties: string read FSessionProperties write FSessionProperties;
    property Text: TCaption read GetText write SetText;
    property OnConstrainedResize: TConstrainedResizeEvent read FOnConstrainedResize write FOnConstrainedResize;
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnTripleClick: TNotifyEvent read FOnTripleClick write FOnTripleClick;
    property OnQuadClick: TNotifyEvent read FOnQuadClick write FOnQuadClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEndDock: TEndDragEvent read FOnEndDock write FOnEndDock;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnStartDock: TStartDockEvent read FOnStartDock write FOnStartDock;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
    property OnEditingDone: TNotifyEvent read FOnEditingDone write FOnEditingDone;
  public
    FCompStyle: Byte; // DEPRECATED. Enables (valid) use of 'IN' operator (this
      // is a hack for speed. It will be replaced by the use of the widgetset
      // classes.
      // So, don't use it anymore.
  public
    // drag and dock
    procedure DragDrop(Source: TObject; X,Y: Integer); Dynamic;
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); dynamic;
    function ManualDock(NewDockSite: TWinControl;
                        DropControl: TControl = nil;
                        ControlSide: TAlign = alNone;
                        KeepDockSiteSize: Boolean = true): Boolean;
    function ManualFloat(TheScreenRect: TRect;
                         KeepDockSiteSize: Boolean = true): Boolean;
    function ReplaceDockedControl(Control: TControl; NewDockSite: TWinControl;
                           DropControl: TControl; ControlSide: TAlign): Boolean;
    function Dragging: Boolean;
  public
    // size
    procedure AdjustSize; virtual;// smart calling DoAutoSize
    function AutoSizeDelayed: boolean; virtual;
    function NeedParentForAutoSize: Boolean; virtual;
    procedure AnchorToNeighbour(Side: TAnchorKind; Space: integer;
                                Sibling: TControl);
    procedure AnchorParallel(Side: TAnchorKind; Space: integer;
                             Sibling: TControl);
    procedure AnchorHorizontalCenterTo(Sibling: TControl);
    procedure AnchorVerticalCenterTo(Sibling: TControl);
    procedure AnchorToCompanion(Side: TAnchorKind; Space: integer;
                                Sibling: TControl;
                                FreeCompositeSide: boolean = true);
    procedure AnchorSame(Side: TAnchorKind; Sibling: TControl);
    procedure AnchorAsAlign(TheAlign: TAlign; Space: Integer);
    procedure AnchorClient(Space: Integer);
    function AnchoredControlCount: integer;
    property AnchoredControls[Index: integer]: TControl read GetAnchoredControls;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer;
                                Lock: boolean = true); virtual;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               Raw: boolean = false;
                               WithThemeSpace: boolean = true); virtual;
    function GetDefaultWidth: integer;
    function GetDefaultHeight: integer;
    class function GetControlClassDefaultSize: TPoint; virtual;
    function GetSidePosition(Side: TAnchorKind): integer;
    procedure CNPreferredSizeChanged;
    procedure InvalidatePreferredSize; virtual;
    function GetAnchorsDependingOnParent(WithNormalAnchors: Boolean): TAnchors;
    procedure DisableAutoSizing;
    procedure EnableAutoSizing;
    procedure UpdateBaseBounds(StoreBounds, StoreParentClientSize,
                               UseLoadedValues: boolean); virtual;
    procedure LockBaseBounds;
    procedure UnlockBaseBounds;
    property BaseBounds: TRect read FBaseBounds;
    property ReadBounds: TRect read FReadBounds;
    procedure WriteLayoutDebugReport(const Prefix: string); virtual;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy; override;
    procedure EditingDone; virtual;
    procedure ExecuteDefaultAction; virtual;
    procedure ExecuteCancelAction; virtual;
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer = -1);
    procedure BringToFront;
    function ColorIsStored: boolean; virtual;
    function HasParent: Boolean; override;
    function IsParentOf(AControl: TControl): boolean; virtual;
    function GetTopParent: TControl;
    function IsVisible: Boolean; virtual;// checks parents too
    function IsControlVisible: Boolean; virtual;// does not check parents
    function FormIsUpdating: boolean; virtual;
    procedure Hide;
    procedure Refresh;
    procedure Repaint; virtual;
    procedure Invalidate; virtual;
    function CheckChildClassAllowed(ChildClass: TClass;
                                    ExceptionOnInvalid: boolean): boolean;
    procedure CheckNewParent(AParent: TWinControl); virtual;
    procedure SendToBack;
    procedure SetTempCursor(Value: TCursor); virtual;
    procedure UpdateRolesForForm; virtual;
    procedure ActiveDefaultControlChanged(NewControl: TControl); virtual;
    function  GetTextBuf(Buffer: PChar; BufSize: Integer): Integer; virtual;
    function  GetTextLen: Integer; virtual;
    procedure SetTextBuf(Buffer: PChar); virtual;
    function  Perform(Msg:Cardinal; WParam: WParam; LParam: LParam): LRESULT;
    function  ScreenToClient(const APoint: TPoint): TPoint;
    function  ClientToScreen(const APoint: TPoint): TPoint;
    function  ScreenToControl(const APoint: TPoint): TPoint;
    function  ControlToScreen(const APoint: TPoint): TPoint;
    function GetChildsRect(Scrolled: boolean): TRect; virtual;
    procedure Show;
    procedure Update; virtual;
    function HandleObjectShouldBeVisible: boolean; virtual;
    function ParentDestroyingHandle: boolean;
    function ParentHandlesAllocated: boolean; virtual;
    procedure InitiateAction; virtual;
    procedure ShowHelp; virtual;
    function HasHelp: Boolean;
  public
    // Event lists
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddHandlerOnResize(const OnResizeEvent: TNotifyEvent;
                                 AsLast: boolean = false);
    procedure RemoveHandlerOnResize(const OnResizeEvent: TNotifyEvent);
    procedure AddHandlerOnChangeBounds(const OnChangeBoundsEvent: TNotifyEvent;
                                       AsLast: boolean = false);
    procedure RemoveHandlerOnChangeBounds(const OnChangeBoundsEvent: TNotifyEvent);
    procedure AddHandlerOnVisibleChanging(const OnVisibleChangingEvent: TNotifyEvent;
                                          AsLast: boolean = false);
    procedure RemoveHandlerOnVisibleChanging(const OnVisibleChangingEvent: TNotifyEvent);
    procedure AddHandlerOnVisibleChanged(const OnVisibleChangedEvent: TNotifyEvent;
                                         AsLast: boolean = false);
    procedure RemoveHandlerOnVisibleChanged(const OnVisibleChangedEvent: TNotifyEvent);
  public
    // standard properties, which should be supported by all descendants
    property Action: TBasicAction read GetAction write SetAction;
    property Align: TAlign read FAlign write SetAlign;
    property Anchors: TAnchors read FAnchors write SetAnchors stored IsAnchorsStored;
    property AnchorSide[Kind: TAnchorKind]: TAnchorSide read GetAnchorSide;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property BoundsRectForNewParent: TRect read FBoundsRectForNewParent write SetBoundsRectForNewParent;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property CaptureMouseButtons: TCaptureMouseButtons read FCaptureMouseButtons
      write FCaptureMouseButtons stored CaptureMouseButtonsIsStored default [mbLeft];
    property ClientHeight: Integer read GetClientHeight write SetClientHeight stored  IsClientHeightStored;
    property ClientOrigin: TPoint read GetClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth stored IsClientWidthStored;
    property Color: TColor read FColor write SetColor stored ColorIsStored default clWindow;
    property Constraints: TSizeConstraints read FConstraints write SetConstraints;
    property ControlOrigin: TPoint read GetControlOrigin;
    property ControlState: TControlState read FControlState write FControlState;
    property ControlStyle: TControlStyle read FControlStyle write FControlStyle;
    property Enabled: Boolean read GetEnabled write SetEnabled stored IsEnabledStored default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property IsControl: Boolean read FIsControl write FIsControl;
    property MouseEntered: Boolean read FMouseEntered;
    property OnChangeBounds: TNotifyEvent read FOnChangeBounds write FOnChangeBounds;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnShowHint: TControlShowHintEvent read FOnShowHint write FOnShowHint;
    property Parent: TWinControl read FParent write SetParent;
    property PopupMenu: TPopupmenu read GetPopupmenu write SetPopupMenu;
    property ShowHint: Boolean read FShowHint write SetShowHint stored IsShowHintStored default False;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored default True;
    property WindowProc: TWndMethod read FWindowProc write FWindowProc;
  public
    // docking properties
    property DockOrientation: TDockOrientation read FDockOrientation write FDockOrientation;
    property Floating: Boolean read GetFloating;
    property FloatingDockSiteClass: TWinControlClass read GetFloatingDockSiteClass write FFloatingDockSiteClass;
    property HostDockSite: TWinControl read FHostDockSite write SetHostDockSite;
    property LRDockWidth: Integer read GetLRDockWidth write FLRDockWidth;
    property TBDockHeight: Integer read GetTBDockHeight write FTBDockHeight;
    property UndockHeight: Integer read GetUndockHeight write FUndockHeight;// Height used when undocked
    property UndockWidth: Integer read GetUndockWidth write FUndockWidth;// Width used when undocked
  private
    //BidiMode property, RightToLeft
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    function IsBiDiModeStored: boolean;
    procedure SetBiDiMode(const AValue: TBiDiMode);
    procedure SetParentBiDiMode(const AValue: Boolean);
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMParentBiDiModeChanged(var Message: TLMessage); message CM_PARENTBIDIMODECHANGED;
  public
    function UseRightToLeftAlignment: Boolean; virtual;
    function UseRightToLeftReading: Boolean; virtual;
    function UseRightToLeftScrollBar: Boolean;
    function IsRightToLeft: Boolean;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored default bdLeftToRight;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
  published
    property AnchorSideLeft: TAnchorSide index 0 read GetAnchorSideIndex write SetAnchorSideIndex;
    property AnchorSideTop: TAnchorSide index 1 read GetAnchorSideIndex write SetAnchorSideIndex;
    property AnchorSideRight: TAnchorSide index 2 read GetAnchorSideIndex write SetAnchorSideIndex;
    property AnchorSideBottom: TAnchorSide index 3 read GetAnchorSideIndex write SetAnchorSideIndex;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property Left: Integer read FLeft write SetLeft;
    property Height: Integer read FHeight write SetHeight;
    property Hint: TTranslateString read FHint write SetHint;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property HelpType: THelpType read FHelpType write FHelpType default htContext;
    property HelpKeyword: String read FHelpKeyword write SetHelpKeyword stored IsHelpKeyWordStored;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext stored IsHelpContextStored;
  end;


  TBorderWidth = 0..MaxInt;

  TGetChildProc = procedure(Child: TComponent) of Object;

  { TControlChildSizing }

  { LeftRightSpacing, TopBottomSpacing: integer;
        minimum space between left client border and left most childs.
        For example: ClientLeftRight=5 means childs Left position is at least 5.

    HorizontalSpacing, VerticalSpacing: integer;
        minimum space between each child horizontally
  }

  {   Defines how child controls are resized/aligned.

      cesAnchorAligning, cssAnchorAligning
        Anchors and Align work like Delphi. For example if Anchors property of
        the control is [akLeft], it means fixed distance between left border of
        parent's client area. [akRight] means fixed distance between right
        border of the control and the right border of the parent's client area.
        When the parent is resized the child is moved to keep the distance.
        [akLeft,akRight] means fixed distance to left border and fixed distance
        to right border. When the parent is resized, the controls width is
        changed (resized) to keep the left and right distance.
        Same for akTop,akBottom.

        Align=alLeft for a control means set Left leftmost, Top topmost and
        maximize Height. The width is kept, if akRight is not set. If akRight
        is set in the Anchors property, then the right distance is kept and
        the control's width is resized.
        If there several controls with Align=alLeft, they will not overlapp and
        be put side by side.
        Same for alRight, alTop, alBottom. (Always expand 3 sides).

        Align=alClient. The control will fill the whole remaining space.
        Setting two childs to Align=alClient does only make sense, if you set
        maximum Constraints.

        Order: First all alTop childs are resized, then alBottom, then alLeft,
        then alRight and finally alClient.

      cesScaleChilds, cssScaleChilds
        Scale childs, keep space between them fixed.
        Childs are resized to their normal/adviced size. If there is some space
        left in the client area of the parent, then the childs are scaled to
        fill the space. You can set maximum Constraints. Then the other childs
        are scaled more.
        For example: 3 child controls A, B, C with A.Width=10, B.Width=20 and
        C.Width=30 (total=60). If the Parent's client area has a ClientWidth of
        120, then the childs are scaled with Factor 2.
        If B has a maximum constraint width of 30, then first the childs will be
        scaled with 1.5 (A.Width=15, B.Width=30, C.Width=45). Then A and C
        (15+45=60 and 30 pixel space left) will be scaled by 1.5 again, to a
        final result of: A.Width=23, B.Width=30, C.Width=67 (23+30+67=120).

      cesHomogenousChildGrowth, cssHomogenousChildDecrease
        Enlarge childs equally.
        Childs are resized to their normal/adviced size. If there is some space
        left in the client area of the parent, then the remaining space is
        distributed equally to each child.
        For example: 3 child controls A, B, C with A.Width=10, B.Width=20 and
        C.Width=30 (total=60). If the Parent's client area has a ClientWidth of
        120, then 60/3=20 is added to each Child.
        If B has a maximum constraint width of 30, then first 10 is added to
        all childs (A.Width=20, B.Width=30, C.Width=40). Then A and C
        (20+40=60 and 30 pixel space left) will get 30/2=15 additional,
        resulting in: A.Width=35, B.Width=30, C.Width=55 (35+30+55=120).

      cesHomogenousSpaceGrowth
        Enlarge space between childs equally.
        Childs are resized to their normal/adviced size. If there is some space
        left in the client area of the parent, then the space between the childs
        is expanded.
        For example: 3 child controls A, B, C with A.Width=10, B.Width=20 and
        C.Width=30 (total=60). If the Parent's client area has a ClientWidth of
        120, then there will be 60/2=30 space between A and B and between
        B and C.
  }

  TChildControlResizeStyle = (
      crsAnchorAligning, // (like Delphi)
      crsScaleChilds, // scale childs, keep space between childs fixed
      crsHomogenousChildResize, // enlarge childs equally (i.e. by the same amount of pixel)
      crsHomogenousSpaceResize  // enlarge space between childs equally
    );

  TControlChildrenLayout = (
      cclNone,
      cclLeftToRightThenTopToBottom,
      cclTopToBottomThenLeftToRight
    );

  TControlChildSizing = class(TPersistent)
  private
    FControl: TWinControl;
    FControlsPerLine: integer;
    FEnlargeHorizontal: TChildControlResizeStyle;
    FEnlargeVertical: TChildControlResizeStyle;
    FHorizontalSpacing: integer;
    FLayout: TControlChildrenLayout;
    FLeftRightSpacing: integer;
    FOnChange: TNotifyEvent;
    FShrinkHorizontal: TChildControlResizeStyle;
    FShrinkVertical: TChildControlResizeStyle;
    FTopBottomSpacing: integer;
    FVerticalSpacing: integer;
    procedure SetControlsPerLine(const AValue: integer);
    procedure SetEnlargeHorizontal(const AValue: TChildControlResizeStyle);
    procedure SetEnlargeVertical(const AValue: TChildControlResizeStyle);
    procedure SetHorizontalSpacing(const AValue: integer);
    procedure SetLayout(const AValue: TControlChildrenLayout);
    procedure SetLeftRightSpacing(const AValue: integer);
    procedure SetShrinkHorizontal(const AValue: TChildControlResizeStyle);
    procedure SetShrinkVertical(const AValue: TChildControlResizeStyle);
    procedure SetTopBottomSpacing(const AValue: integer);
    procedure SetVerticalSpacing(const AValue: integer);
  protected
    procedure Change; dynamic;
  public
    constructor Create(OwnerControl: TWinControl);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Sizing: TControlChildSizing): boolean;
    procedure SetGridSpacing(Spacing: integer);
  public
    property Control: TWinControl read FControl;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LeftRightSpacing: integer read FLeftRightSpacing write SetLeftRightSpacing;
    property TopBottomSpacing: integer read FTopBottomSpacing write SetTopBottomSpacing;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing;
    property EnlargeHorizontal: TChildControlResizeStyle read FEnlargeHorizontal
                           write SetEnlargeHorizontal default crsAnchorAligning;
    property EnlargeVertical: TChildControlResizeStyle read FEnlargeVertical
                             write SetEnlargeVertical default crsAnchorAligning;
    property ShrinkHorizontal: TChildControlResizeStyle read FShrinkHorizontal
                            write SetShrinkHorizontal default crsAnchorAligning;
    property ShrinkVertical: TChildControlResizeStyle read FShrinkVertical
                              write SetShrinkVertical default crsAnchorAligning;
    property Layout: TControlChildrenLayout read FLayout write SetLayout default cclNone;
    property ControlsPerLine: integer read FControlsPerLine write SetControlsPerLine;
  end;


  { TWinControlActionLink }

  // Since HelpContext and HelpKeyword are properties of TControl,
  // this class is obsolete. In order not to break existing code,
  // its declaration is aliased to TControlActionLink.
  TWinControlActionLink = TControlActionLink;
  TWinControlActionLinkClass = class of TWinControlActionLink;


  { TWinControl }
  
  TWinControlFlag = (
    wcfClientRectNeedsUpdate,
    wcfColorChanged,
    wcfFontChanged,
    wcfReAlignNeeded,
    wcfAligningControls,
    wcfEraseBackground,
    wcfCreatingHandle,      // Set while constructing the handle of this control
    wcfInitializing,        // Set while initializing during handle creation
    wcfCreatingChildHandles, // Set while constructing the handles of the childs
    wcfHandleVisible
    );
  TWinControlFlags = set of TWinControlFlag;
  
  TControlAtPosFlag = (
    capfAllowDisabled,   // include controls with Enabled=false
    capfAllowWinControls,// include TWinControls
    capfOnlyClientAreas, // use the client areas, not the whole child area
    capfRecursive,       // search recursively in grand childrens
    capfHasScrollOffset  // do not add the scroll offset to Pos (already included)
    );
  TControlAtPosFlags = set of TControlAtPosFlag;

  TWinControl = class(TControl)
  private
    FAlignLevel: Word;
    FBorderWidth: TBorderWidth;
    FBoundsLockCount: integer;
    FBoundsRealized: TRect;
    FBorderStyle: TBorderStyle;
    FBrush: TBrush;
    FAdjustClientRectRealized: TRect;
    FChildSizing: TControlChildSizing;
    FControls: TFPList;    // the child controls (only TControl, no TWinControl)
    FWinControls: TFPList; // the child controls (only TWinControl, no TControl)
    FDefWndProc: Pointer;
    FDockClients: TFPList;
    FDoubleBuffered: Boolean;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FDockManager: TDockManager;
    FDockSite: Boolean;
    FOnDockDrop: TDockDropEvent;
    FOnDockOver: TDockOverEvent;
    FOnGetSiteInfo: TGetSiteInfoEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnUnDock: TUnDockEvent;
    FOnUTF8KeyPress: TUTF8KeyPressEvent;
    FParentWindow: hwnd;
    FParentCtl3D: Boolean;
    FRealizeBoundsLockCount: integer;
    FHandle: Hwnd;
    FShowing: Boolean;
    FTabOrder: integer;
    FTabStop: Boolean;
    FTabList: TFPList;
    FUseDockManager: Boolean;
    procedure AlignControl(AControl: TControl);
    function GetBrush: TBrush;
    function GetControl(const Index: Integer): TControl;
    function GetControlCount: Integer;
    function GetDockClientCount: Integer;
    function GetDockClients(Index: Integer): TControl;
    function GetHandle: HWND;
    function GetIsResizing: boolean;
    function GetTabOrder: TTabOrder;
    function GetVisibleDockClientCount: Integer;
    procedure SetChildSizing(const AValue: TControlChildSizing);
    procedure SetDockSite(const NewDockSite: Boolean);
    procedure SetHandle(NewHandle: HWND);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetParentCtl3D(Value: Boolean);
    Procedure SetTabOrder(NewTabOrder: TTabOrder);
    procedure SetTabStop(NewTabStop: Boolean);
    procedure SetUseDockManager(const AValue: Boolean);
    procedure UpdateTabOrder(NewTabOrder: TTabOrder);
    function  WantsKeyBeforeInterface(Key: word; Shift: TShiftState): boolean;
    procedure Insert(AControl: TControl);
    procedure Insert(AControl: TControl; Index: integer);
    procedure Remove(AControl: TControl);
    procedure AlignNonAlignedControls(ListOfControls: TFPList;
                                      var BoundsModified: Boolean);
    function IsClientHeightStored: boolean; override;
    function IsClientWidthStored: boolean; override;
  protected
    FWinControlFlags: TWinControlFlags;
    procedure AdjustClientRect(var ARect: TRect); virtual;
    procedure CreateControlAlignList(TheAlign: TAlign;
                                    AlignList: TFPList; StartControl: TControl);
    procedure AlignControls(AControl: TControl;
                            var RemainingClientRect: TRect); virtual;
    function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean; virtual;
    procedure DoChildSizingChange(Sender: TObject); virtual;
    procedure ResizeDelayedAutoSizeChildren; virtual;
    function CanTab: Boolean; override;
    function DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean):LRESULT; override;
    function DoDockClientMsg(DragDockObject: TDragDockObject; Position: TPoint): boolean; virtual;
    function DoUndockClientMsg(NewTarget, Client: TControl):boolean; virtual;
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED;
    procedure CMVisibleChanged(var TheMessage: TLMessage); message CM_VISIBLECHANGED;
    procedure DoSendShowHideToInterface; virtual;
    procedure ControlsAligned; virtual;// called by AlignControls after aligning controls
    procedure DoSendBoundsToInterface; virtual;
    procedure RealizeBounds; virtual;// checks for changes and calls DoSendBoundsToInterface
    procedure RealizeBoundsRecursive;
    procedure CreateSubClass(var Params: TCreateParams;ControlClassName: PChar);
    procedure DoConstraintsChange(Sender: TObject); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure DoAutoSize; override;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure PaintControls(DC: HDC; First: TControl);
    procedure PaintHandler(var TheMessage: TLMPaint);
    procedure PaintWindow(DC: HDC); virtual;
    procedure CreateBrush; virtual;
  protected
    // messages
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMShowHintChanged(var Message: TLMessage); message CM_SHOWHINTCHANGED;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMNotify(var Message: TLMNotify); message LM_NOTIFY;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMEnter(var Message: TLMEnter); message LM_ENTER;
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TLMKeyDown); message LM_SYSKEYDOWN;
    procedure WMKeyUp(var Message: TLMKeyUp); message LM_KEYUP;
    procedure WMSysKeyUp(var Message: TLMKeyUp); message LM_SYSKEYUP;
    procedure WMChar(var Message: TLMChar); message LM_CHAR;
    procedure WMSysChar(var Message: TLMKeyUp); message LM_SYSCHAR;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure WMDestroy(var Message: TLMDestroy); message LM_DESTROY;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
    procedure CNSysKeyDown(var Message: TLMKeyDown); message CN_SYSKEYDOWN;
    procedure CNKeyUp(var Message: TLMKeyUp); message CN_KEYUP;
    procedure CNSysKeyUp(var Message: TLMKeyUp); message CN_SYSKEYUP;
    procedure CNChar(var Message: TLMKeyUp); message CN_CHAR;
  protected
    // drag and drop
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); dynamic;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
                       State: TDragState; var Accept: Boolean); dynamic;
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer;
                         State: TDragState; var Accept: Boolean); dynamic;
    procedure DoRemoveDockClient(Client: TControl); dynamic;
    function  DoUnDock(NewTarget: TWinControl; Client: TControl;
                       KeepDockSiteSize: Boolean = true): Boolean; dynamic;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
                          MousePos: TPoint; var CanDock: Boolean); dynamic;
    procedure ReloadDockedControl(const AControlName: string;
                                  var AControl: TControl); dynamic;
    function CreateDockManager: TDockManager; dynamic;
    procedure DoFloatMsg(ADockSource: TDragDockObject); override;//CM_FLOAT
  protected
    // mouse and keyboard
    procedure DoEnter; dynamic;
    procedure DoExit; dynamic;
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                           MousePos: TPoint): Boolean; dynamic;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    function  DoKeyDownBeforeInterface(var Message: TLMKey): Boolean;
    function  DoRemainingKeyDown(var Message: TLMKeyDown): Boolean;
    function  DoRemainingKeyUp(var Message: TLMKeyDown): Boolean;
    function  DoKeyPress(var Message: TLMKey): Boolean;
    function  DoUTF8KeyPress(var UTF8Key: TUTF8Char): boolean; dynamic;
    function  DoKeyUpBeforeInterface(var Message: TLMKey): Boolean;
    function  ChildKey(var Message: TLMKey): boolean; dynamic;
    function  SendDialogChar(var Message: TLMKey): Boolean;
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure ControlKeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure ControlKeyUp(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyDownBeforeInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyDownAfterInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyPress(var Key: char); dynamic;
    procedure KeyUpBeforeInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUpAfterInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); dynamic;
  protected
    function  FindNextControl(CurrentControl: TWinControl; GoForward,
                              CheckTabStop, CheckParent: Boolean): TWinControl;
    procedure SelectFirst;
    function  RealGetText: TCaption; override;
    function  GetBorderStyle: TBorderStyle;
    function  GetClientOrigin: TPoint; override;
    function  GetClientRect: TRect; override;
    function  GetControlOrigin: TPoint; override;
    function  GetDeviceContext(var WindowHandle: HWnd): HDC; override;
    function  IsControlMouseMsg(var TheMessage: TLMMouse): Boolean;
    function  ParentHandlesAllocated: boolean; override;
    procedure CreateHandle; virtual;
    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure CreateWnd; virtual; //creates the window
    procedure DestroyHandle; virtual;
    procedure DestroyWnd; virtual;
    procedure DoFlipChildren; dynamic;
    procedure FinalizeWnd; virtual; // gets called before the Handle is destroyed.
    procedure FixupTabList;
    procedure FontChanged(Sender: TObject); override;
    procedure InitializeWnd; virtual; // gets called after the Handle is created and before the child handles are created
    procedure Loaded; override;
    procedure FormEndUpdated; override;
    procedure MainWndProc(var Msg: TLMessage);
    procedure ParentFormHandleInitialized; override;
    procedure ChildHandlesCreated; virtual;// called after childs handles are created
    procedure RealSetText(const AValue: TCaption); override;
    procedure RemoveFocus(Removing: Boolean);
    procedure SendMoveSizeMessages(SizeChanged, PosChanged: boolean); override;
    procedure SetBorderStyle(NewStyle: TBorderStyle); virtual;
    procedure SetColor(Value: TColor); override;
    procedure SetChildZPosition(const AChild: TControl; const APosition: Integer);
    procedure ShowControl(AControl: TControl); virtual;
    procedure Update; override;
    procedure UpdateControlState;
    procedure UpdateShowing; virtual;
    procedure WndProc(var Message: TLMessage); override;
    procedure WSSetText(const AText: String); virtual;
  protected
    // properties which are not supported by all descendents
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsNone;
    property OnGetSiteInfo: TGetSiteInfoEvent read FOnGetSiteInfo write FOnGetSiteInfo;
  public
    // properties which are supported by all descendents
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BoundsLockCount: integer read FBoundsLockCount;
    property Brush: TBrush read GetBrush;
    property CachedClientHeight: integer read FClientHeight;
    property CachedClientWidth: integer read FClientWidth;
    property ChildSizing: TControlChildSizing read FChildSizing write SetChildSizing;
    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TControl read GetControl;
    property DefWndProc: Pointer read FDefWndProc write FDefWndPRoc;
    property DockClientCount: Integer read GetDockClientCount;
    property DockClients[Index: Integer]: TControl read GetDockClients;
    property DockManager: TDockManager read FDockManager write FDockManager;
    property DockSite: Boolean read FDockSite write SetDockSite default False;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property Handle: HWND read GetHandle write SetHandle;
    property IsResizing: Boolean read GetIsResizing;
    property TabOrder: TTabOrder read GetTabOrder write SetTaborder default -1;
    property TabStop: Boolean read FTabStop write SetTabStop default false;
    property OnDockDrop: TDockDropEvent read FOnDockDrop write FOnDockDrop;
    property OnDockOver: TDockOverEvent read FOnDockOver write FOnDockOver;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
    property OnUnDock: TUnDockEvent read FOnUnDock write FOnUnDock;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read FOnUTF8KeyPress write FOnUTF8KeyPress;
    property ParentCtl3D: Boolean read FParentCtl3D write SetParentCtl3d default True;
    property Showing: Boolean read FShowing;
    property UseDockManager: Boolean read FUseDockManager
                                     write SetUseDockManager default False;
    property VisibleDockClientCount: Integer read GetVisibleDockClientCount;
  public
    // size, position, bounds
    function AutoSizeDelayed: boolean; override;
    procedure BeginUpdateBounds;
    procedure EndUpdateBounds;
    procedure LockRealizeBounds;
    procedure UnlockRealizeBounds;
    function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl;
    function ControlAtPos(const Pos: TPoint;
                          AllowDisabled, AllowWinControls: Boolean): TControl;
    function ControlAtPos(const Pos: TPoint; Flags: TControlAtPosFlags): TControl;
    function  ContainsControl(Control: TControl): Boolean;
    procedure DoAdjustClientRectChange;
    procedure InvalidateClientRectCache(WithChildControls: boolean);
    function ClientRectNeedsInterfaceUpdate: boolean;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    function  GetChildsRect(Scrolled: boolean): TRect; override;
    procedure DisableAlign;
    procedure EnableAlign;
    procedure ReAlign; // realign all childs
    procedure WriteLayoutDebugReport(const Prefix: string); override;
  public
    constructor Create(TheOwner: TComponent);override;
    constructor CreateParented(ParentWindow: HWnd);
    class function CreateParentedControl(ParentWindow: HWnd): TWinControl;
    destructor Destroy; override;
    procedure DockDrop(DragDockObject: TDragDockObject; X, Y: Integer); dynamic;
    function CanFocus: Boolean; virtual;
    function GetControlIndex(AControl: TControl): integer;
    procedure SetControlIndex(AControl: TControl; NewIndex: integer);
    function Focused: Boolean; virtual;
    function PerformTab(ForwardTab: boolean): boolean; virtual;
    function ControlByName(const ControlName: string): TControl;
    procedure SelectNext(CurControl: TWinControl;
                         GoForward, CheckTabStop: Boolean);
    procedure SetTempCursor(Value: TCursor); override;
    procedure BroadCast(var ToAllMessage);
    procedure NotifyControls(Msg: Word);
    procedure DefaultHandler(var AMessage); override;
    function  GetTextLen: Integer; override;
    procedure Invalidate; override;
    procedure AddControl; virtual;
    procedure InsertControl(AControl: TControl);
    procedure InsertControl(AControl: TControl; Index: integer); virtual;
    procedure RemoveControl(AControl: TControl);
    procedure Repaint; override;
    procedure SetFocus; virtual;
    function FindChildControl(const ControlName: String): TControl;
    procedure FlipChildren(AllLevels: Boolean); dynamic;
    procedure GetTabOrderList(List: TFPList);
    function HandleAllocated: Boolean;
    procedure HandleNeeded;
    function BrushCreated: Boolean;
    procedure EraseBackground(DC: HDC); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUp(var Key: Word; Shift: TShiftState); dynamic;
    function IntfUTF8KeyPress(var UTF8Key: TUTF8Char;
                              RepeatCount: integer; SystemKey: boolean): boolean; dynamic;
    procedure PaintTo(DC: HDC; X, Y: Integer); virtual; overload;
    procedure PaintTo(ACanvas: TCanvas; X, Y: Integer); overload;
  end;


  { TGraphicControl }

  TGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    FOnPaint: TNotifyEvent;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; virtual;
    procedure DoOnChangeBounds; override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  end;


  { TCustomControl }

  TCustomControl = class(TWinControl)
  private
    FCanvas: TCanvas;
    FOnPaint: TNotifyEvent;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure FontChanged(Sender: TObject); override;
    procedure SetColor(Value: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyWnd; override;
    procedure Paint; virtual;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property BorderStyle;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;


  { TImageList }

  TImageList = class(TDragImageList)
  published
    property AllocBy;
    property BlendColor;
    property BkColor;
    property DrawingStyle;
    property Height;
    property ImageType;
    property Masked;
    property ShareImages;
    property Width;
    property OnChange;
  end;


  { TControlPropertyStorage - abstract base class }

  TControlPropertyStorage = class(TCustomPropertyStorage)
  protected
    procedure GetPropertyList(List: TStrings); override;
  end;
  
  
  { TDockZone }

  TDockTree = class;

  { TDockZone is a node in the TDockTree and encapsulates a region into which
    other zones or a single control are contained. }

  TDockZone = class
  private
    FChildControl: TControl;
    FChildCount: integer;
    FFirstChildZone: TDockZone;
    FTree: TDockTree;
    FParentZone: TDockZone;
    FOrientation: TDockOrientation;
    FNextSibling: TDockZone;
    FPrevSibling: TDockZone;
  protected
    function GetHeight: Integer; virtual;
    function GetLeft: Integer; virtual;
    function GetLimitBegin: Integer; virtual;
    function GetLimitSize: Integer; virtual;
    function GetTop: Integer; virtual;
    function GetVisible: Boolean; virtual;
    function GetVisibleChildCount: Integer; virtual;
    function GetWidth: Integer; virtual;
  public
    constructor Create(TheTree: TDockTree; TheChildControl: TControl);
    function FindZone(AControl: TControl): TDockZone;
    function FirstVisibleChild: TDockZone;
    function GetNextVisibleZone: TDockZone;
    function NextVisible: TDockZone;
    function PrevVisible: TDockZone;
    procedure AddAsFirstChild(NewChildZone: TDockZone);
    procedure AddAsLastChild(NewChildZone: TDockZone);
    procedure ReplaceChild(OldChild, NewChild: TDockZone);
    function GetLastChild: TDockZone;
    function GetIndex: Integer;
  public
    property ChildControl: TControl read FChildControl;
    property ChildCount: Integer read FChildCount;
    property FirstChild: TDockZone read FFirstChildZone;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft;
    property LimitBegin: Integer read GetLimitBegin; // returns Left or Top
    property LimitSize: Integer read GetLimitSize;   // returns Width or Height
    property Orientation: TDockOrientation read FOrientation write FOrientation;
    property Parent: TDockZone read FParentZone;
    property Top: Integer read GetTop;
    property Tree: TDockTree read FTree;
    property Visible: Boolean read GetVisible;
    property VisibleChildCount: Integer read GetVisibleChildCount;
    property Width: Integer read GetWidth;
    property NextSibling: TDockZone read FNextSibling;
    property PrevSibling: TDockZone read FPrevSibling;
  end;
  TDockZoneClass = class of TDockZone;


  { TDockTree - a tree of TDockZones - Every docked window has one tree
  
    This is an abstract class. The real implementation is in ldocktree.pas.

    Docking means here: Combining several windows to one. A window can here be
    a TCustomForm or a floating control (undocked) or a TDockForm.
    A window can be docked to another to the left, right, top, bottom or "into".
    The docking source window will be resized, to fit to the docking target
    window.

    Example1: Docking "A" (source window) left to "B" (target window)
    
       +---+    +----+
       | A | -> | B  |
       +---+    |    |
                +----+
      Result: A new docktree will be created. Height of "A" will be resized to
              the height of "B".
              A splitter will be inserted between "A" and "B".
              And all three are childs of the newly created TLazDockForm of the
              newly created TDockTree.
      
       +------------+
       |+---+|+----+|
       || A ||| B  ||
       ||   |||    ||
       |+---+|+----+|
       +------------+

      If "A" or "B" were floating controls, the floating dock sites are freed.
      If "A" or "B" were forms, their decorations (title bars and borders) are
      replaced by docked decorations.
      If "A" had a TDockTree, it is freed and its child dockzones are merged to
      the docktree of "B". Analog for docking "C" left to "A":
      
       +------------------+
       |+---+|+---+|+----+|
       || C ||| A ||| B  ||
       ||   |||   |||    ||
       |+---+|+---+|+----+|
       +------------------+
       

      
    Example2: Docking A into B
                +-----+
       +---+    |     |
       | A | ---+-> B |
       +---+    |     |
                +-----+

      Result: A new docktree will be created. "A" will be resized to the size
              of "B". Both will be put into a TLazDockPages control which is the
              child of the newly created TDockTree.
              
       +-------+
       |[B][A] |
       |+-----+|
       ||     ||
       || A   ||
       ||     ||
       |+-----+|
       +-------+

    Every DockZone has siblings and childs. Siblings can either be
    - horizontally (left to right, splitter),
    - vertically (top to bottom, splitter)
    - or upon each other (as pages, left to right).


    InsertControl - undock control and dock it into the manager. For example
                    dock Form1 left to a Form2:
                    InsertControl(Form1,alLeft,Form2);
                    To dock "into", into a TDockPage, use Align=alNone.
    PositionDockRect - calculates where a control would be placed, if it would
                       be docked via InsertControl.
    RemoveControl - removes a control from the dock manager.

    GetControlBounds - TODO for Delphi compatibility
    ResetBounds - TODO for Delphi compatibility
    SetReplacingControl - TODO for Delphi compatibility
    PaintSite - TODO for Delphi compatibility
  }

  TForEachZoneProc = procedure(Zone: TDockZone) of object;

  TDockTreeClass = class of TDockTree;

  TDockTreeFlag = (
    dtfUpdateAllNeeded
    );
  TDockTreeFlags = set of TDockTreeFlag;

  { TDockTree - see comment above }

  TDockTree = class(TDockManager)
  private
    FBorderWidth: Integer; // width of the border of the preview rectangle
    FDockSite: TWinControl;
    FDockZoneClass: TDockZoneClass;
    //FGrabberSize: Integer;
    //FGrabbersOnTop: Boolean;
    FFlags: TDockTreeFlags;
    FRootZone: TDockZone;
    //FTopXYLimit: Integer;
    FUpdateCount: Integer;
    procedure DeleteZone(Zone: TDockZone);
  protected
    procedure AdjustDockRect(AControl: TControl; var ARect: TRect); virtual;
    function HitTest(const MousePos: TPoint; var HTFlag: Integer): TControl; virtual;
    procedure PaintDockFrame(ACanvas: TCanvas; AControl: TControl;
                             const ARect: TRect); virtual;
    procedure UpdateAll;
    procedure SetDockZoneClass(const AValue: TDockZoneClass);
  public
    constructor Create(TheDockSite: TWinControl); virtual;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure GetControlBounds(AControl: TControl;
                               out ControlBounds: TRect); override;
    procedure InsertControl(AControl: TControl; InsertAt: TAlign;
                            DropControl: TControl); override;
    procedure LoadFromStream(SrcStream: TStream); override;
    procedure PositionDockRect(AClient, DropCtl: TControl; DropAlign: TAlign;
                               var DockRect: TRect); override;
    procedure RemoveControl(AControl: TControl); override;
    procedure SaveToStream(DestStream: TStream); override;
    procedure SetReplacingControl(AControl: TControl); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure PaintSite(DC: HDC); override;
  public
    property DockZoneClass: TDockZoneClass read FDockZoneClass;
    property DockSite: TWinControl read FDockSite write FDockSite;
    property RootZone: TDockZone read FRootZone;
  end;
  
var
  DockSplitterClass: TControlClass = nil;

type
  { TMouse }

  TMouse = class
  private
    FCapture: HWND;
    FWheelScrollLines: Integer;
    Procedure SetCapture(const Value: HWND);
    Function GetCapture: HWND;
    Function GetCursorPos: TPoint;
    procedure SetCursorPos(AValue : TPoint);
    function GetWheelScrollLines: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Capture: HWND read GetCapture write SetCapture;
    property CursorPos: TPoint read GetCursorPos write SetCursorPos;
    property WheelScrollLines: Integer read GetWheelScrollLines;
  end;


const
  AnchorAlign: array[TAlign] of TAnchors = (
    [akLeft, akTop],                   // alNone
    [akLeft, akTop, akRight],          // alTop
    [akLeft, akRight, akBottom],       // alBottom
    [akLeft, akTop, akBottom],         // alLeft
    [akRight, akTop, akBottom],        // alRight
    [akLeft, akTop, akRight, akBottom],// alClient
    [akLeft, akTop]                    // alCustom
    );
  MainAlignAnchor: array[TAlign] of TAnchorKind = (
    akLeft,   // alNone
    akTop,    // alTop
    akBottom, // alBottom
    akLeft,   // alLeft
    akRight,  // alRight
    akLeft,   // alClient
    akLeft    // alCustom
    );
  OppositeAnchor: array[TAnchorKind] of TAnchorKind = (
    akBottom, // akTop,
    akRight,  // akLeft,
    akLeft,   // akRight,
    akTop     // akBottom
    );
  ClockwiseAnchor: array[TAnchorKind] of TAnchorKind = (
    akRight,  // akTop,
    akTop,    // akLeft,
    akBottom, // akRight,
    akLeft    // akBottom
    );
  DefaultSideForAnchorKind: array[TAnchorKind] of TAnchorSideReference = (
    asrBottom, // akTop
    asrBottom, // akLeft
    asrTop,    // akRight
    asrTop     // akBottom
    );
  AnchorReferenceSide: array[TAnchorKind,TAnchorSideReference] of TAnchorKind =(
    // akTop -> asrTop, asrBottom, asrCenter
    (akTop,akBottom,akTop),
    // akLeft -> asrTop, asrBottom, asrCenter
    (akLeft,akRight,akLeft),
    // akRight -> asrTop, asrBottom, asrCenter
    (akTop,akBottom,akTop),
    // akBottom -> asrTop, asrBottom, asrCenter
    (akLeft,akRight,akLeft)
    );
  AlignNames: array[TAlign] of string = (
    'alNone', 'alTop', 'alBottom', 'alLeft', 'alRight', 'alClient', 'alCustom');
  AnchorNames: array[TAnchorKind] of string = (
    'akTop', 'akLeft', 'akRight', 'akBottom');


function FindDragTarget(const Position: TPoint; AllowDisabled: Boolean): TControl;
function FindControlAtPosition(const Position: TPoint; AllowDisabled: Boolean): TControl;
function FindLCLWindow(const ScreenPos: TPoint): TWinControl;
function FindControl(Handle: hwnd): TWinControl;
function FindOwnerControl(Handle: hwnd): TWinControl;
function FindLCLControl(const ScreenPos: TPoint): TControl;

function SendAppMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): Longint;
procedure MoveWindowOrg(dc: hdc; X,Y: Integer);

// Interface support.
procedure RecreateWnd(const AWinControl:TWinControl);


// drag and drop
var
  DefaultDockTreeClass: TDockTreeClass;

procedure SetCaptureControl(AWinControl: TWinControl; const Position: TPoint);
procedure SetCaptureControl(Control: TControl);
function GetCaptureControl: TControl;

var
  NewStyleControls: Boolean;
  Mouse: TMouse;

// mouse cursor
function CursorToString(Cursor: TCursor): string;
function StringToCursor(const S: string): TCursor;
procedure GetCursorValues(Proc: TGetStrProc);
function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;

// shiftstate
function GetKeyShiftState: TShiftState;

procedure AdjustBorderSpace(var RemainingClientRect, CurBorderSpace: TRect;
  Left, Top, Right, Bottom: integer);
procedure AdjustBorderSpace(var RemainingClientRect, CurBorderSpace: TRect;
  const Space: TRect);
  
function DbgS(a: TAnchorKind): string; overload;
function DbgS(Anchors: TAnchors): string; overload;
function DbgS(a: TAlign): string; overload;
function DbgS(a: TAnchorKind; Side: TAnchorSideReference): string;

// register (called by the package initialization in design mode)
procedure Register;


implementation

uses
  WSControls, // circle with base widgetset is allowed
  Forms, // the circle can't be broken without breaking Delphi compatibility
  Math;  // Math is in RTL and only a few functions are used.

var
  // The interface knows, which TWinControl has the capture. This stores
  // what child control of this TWinControl has actually the capture.
  CaptureControl: TControl=nil;

procedure AdjustBorderSpace(var RemainingClientRect, CurBorderSpace: TRect;
  Left, Top, Right, Bottom: integer);
// RemainingClientRect: remaining clientrect without CurBorderSpace
// CurBorderSpace: current borderspace around RemainingClientRect
// Left, Top, Right, Bottom: apply these borderspaces to CurBorderSpace
//
// CurBorderSpace will be set to the maximum of CurBorderSpace and Left, Top,
// Right, Bottom.
// RemainingClientRect will shrink.
// RemainingClientRect will not shrink to negative size.
var
  NewWidth: Integer;
  NewHeight: Integer;
  NewLeft: Integer;
  NewTop: Integer;
begin
  // set CurBorderSpace to maximum border spacing and adjust RemainingClientRect
  if CurBorderSpace.Left<Left then begin
    inc(RemainingClientRect.Left,Left-CurBorderSpace.Left);
    CurBorderSpace.Left:=Left;
  end;
  if CurBorderSpace.Right<Right then begin
    dec(RemainingClientRect.Right,Right-CurBorderSpace.Right);
    CurBorderSpace.Right:=Right;
  end;
  if CurBorderSpace.Top<Top then begin
    inc(RemainingClientRect.Top,Top-CurBorderSpace.Top);
    CurBorderSpace.Top:=Top;
  end;
  if CurBorderSpace.Bottom<Bottom then begin
    dec(RemainingClientRect.Bottom,Bottom-CurBorderSpace.Bottom);
    CurBorderSpace.Bottom:=Bottom;
  end;

  // make sure RemainingClientRect has no negative Size
  NewWidth:=RemainingClientRect.Right-RemainingClientRect.Left;
  if NewWidth<0 then begin
    // Width is negative
    // set Width to 0 and adjust borderspace. Set Left/Right to center.
    // Example: RemainingClientRect.Left=20, RemainingClientRect.Right=10,
    //          CurBorderSpace.Left:=17, CurBorderSpace.Right:=18
    // Result: RemainingClientRect.Left=RemainingClientRect.Right=15;
    //         CurBorderSpace.Left:=17, CurBorderSpace.Right:=18
    NewLeft:=(RemainingClientRect.Left+RemainingClientRect.Right) div 2;
    dec(CurBorderSpace.Left,RemainingClientRect.Left-NewLeft);
    dec(CurBorderSpace.Right,NewLeft-RemainingClientRect.Right);
    RemainingClientRect.Left:=NewLeft;
    RemainingClientRect.Right:=RemainingClientRect.Left;
  end;
  NewHeight:=RemainingClientRect.Bottom-RemainingClientRect.Top;
  if NewHeight<0 then begin
    // Height is negative
    NewTop:=(RemainingClientRect.Top+RemainingClientRect.Bottom) div 2;
    dec(CurBorderSpace.Top,RemainingClientRect.Top-NewTop);
    dec(CurBorderSpace.Bottom,NewTop-RemainingClientRect.Bottom);
    RemainingClientRect.Top:=NewTop;
    RemainingClientRect.Bottom:=RemainingClientRect.Top;
  end;
end;

procedure AdjustBorderSpace(var RemainingClientRect, CurBorderSpace: TRect;
  const Space: TRect);
begin
  AdjustBorderSpace(RemainingClientRect,CurBorderSpace,Space.Left,Space.Top,
                    Space.Right,Space.Bottom);
end;

function DbgS(a: TAnchorKind): string;
begin
  Result:=AnchorNames[a];
end;

function DbgS(Anchors: TAnchors): string;
var
  a: TAnchorKind;
begin
  Result:='';
  for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
    if a in Anchors then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+AnchorNames[a];
    end;
  end;
  Result:='['+Result+']';
end;

function DbgS(a: TAlign): string;
begin
  Result:=AlignNames[a];
end;

function DbgS(a: TAnchorKind; Side: TAnchorSideReference): string;
begin
  case Side of
  asrTop: if a in [akLeft,akRight] then Result:='asrLeft' else Result:='asrTop';
  asrBottom: if a in [akLeft,akRight] then Result:='asrRight' else Result:='asrBottom';
  asrCenter: Result:='asrCenter';
  else Result:='asr???';
  end;
end;

{------------------------------------------------------------------------------
 RecreateWnd
 This function was originally member of TWincontrol. From a VCL point of view
 that made perfectly sense since the VCL knows when a win32 widget has to be
 recreated when properties have changed.
 The LCL however doesn't know, the widgetset does. To avoid old VCL behaviour
 and to provide a central function to the widgetset, it is moved here.
 MWE.
------------------------------------------------------------------------------}
procedure RecreateWnd(const AWinControl:TWinControl);
var
  IsFocused: Boolean;
begin
  if csDestroying in AWinControl.ComponentState then Exit;
  if wcfCreatingHandle in AWinControl.FWinControlFlags then exit;

  if not AWinControl.HandleAllocated
  then begin
    // since only the interface (or custom interface dependent controls) should
    // call us, the handle is always created
    DebugLN('WARNING: obsolete call to RecreateWnd for %s', [AWinControl.ClassName]);
  end;

  IsFocused := AWinControl.Focused;
  AWinControl.DestroyHandle;
  AWinControl.UpdateControlState;
  if IsFocused and AWinControl.HandleAllocated
  then SetFocus(AWinControl.FHandle);
end;

procedure Register;
begin
  RegisterComponents('Common Controls',[TImageList]);
  RegisterNoIcon([TCustomControl,TGraphicControl]);
end;

function SendAppMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): Longint;
begin
  Result:=LCLProc.SendApplicationMessage(Msg,WParam,LParam);
end;

procedure MoveWindowOrg(dc: hdc; X, Y: Integer);
begin
  MoveWindowOrgEx(DC,X,Y);
end;

function GetKeyShiftState: TShiftState;
begin
  Result:=[];
  if (GetKeyState(VK_CONTROL) and $8000)<>0 then
    Include(Result,ssCtrl);
  if (GetKeyState(VK_SHIFT) and $8000)<>0 then
    Include(Result,ssShift);
  if (GetKeyState(VK_MENU) and $8000)<>0 then
    Include(Result,ssAlt);
end;

{------------------------------------------------------------------------------
  FindControl

  Returns the TWinControl associated with the Handle.
  This is very interface specific. Better use FindOwnerControl.

  Handle can also be a child handle, and does not need to be the Handle
  property of the Result.
  IMPORTANT: So, in most cases: Result.Handle <> Handle in the params.

------------------------------------------------------------------------------}
function FindControl(Handle: hwnd): TWinControl;
begin
  if Handle <> 0
  then Result := TWinControl(GetProp(Handle,'WinControl'))
  else Result := nil;
end;

{------------------------------------------------------------------------------
  FindOwnerControl

  Returns the TWinControl owning the Handle. Handle can also be a child handle,
  and does not need to be the Handle property of the Result.
  IMPORTANT: Therefore, in most cases: parameter Handle <> Result.Handle
------------------------------------------------------------------------------}
function FindOwnerControl(Handle: hwnd): TWinControl;
begin
  While Handle<>0 do begin
    Result:=FindControl(Handle);
    if Result<>nil then exit;
    Handle:=GetParent(Handle);
  end;
  Result:=nil;
end;

{------------------------------------------------------------------------------
  FindLCLControl

  Returns the TControl that it at the moment at the visible screen position.
  This is not reliable during resizing.
------------------------------------------------------------------------------}
function FindLCLControl(const ScreenPos: TPoint): TControl;
var
  AWinControl: TWinControl;
  ClientPos: TPoint;
begin
  Result:=nil;
  // find wincontrol at mouse cursor
  AWinControl:=FindLCLWindow(ScreenPos);
  if AWinControl=nil then exit;
  // find control at mouse cursor
  ClientPos:=AWinControl.ScreenToClient(ScreenPos);
  Result:=AWinControl.ControlAtPos(ClientPos,
                        [capfAllowDisabled,capfAllowWinControls,capfRecursive]);
  if Result=nil then Result:=AWinControl;
end;

{-------------------------------------------------------------------------------
  function DoControlMsg(Handle: hwnd; var Message): Boolean;
  
  Find the owner wincontrol and Perform the Message.
-------------------------------------------------------------------------------}
function DoControlMsg(Handle: hwnd; var Message): Boolean;
var
  AWinControl: TWinControl;
begin
  Result := false;
  AWinControl := FindOwnerControl(Handle);
  if AWinControl <> nil then
  begin
    { do not use Perform, use WndProc so we can save the Result }
    Inc(TLMessage(Message).Msg, CN_BASE);
    AWinControl.WindowProc(TLMessage(Message));
    Dec(TLMessage(Message).Msg, CN_BASE);
    Result := true;
  end;
end;

{------------------------------------------------------------------------------
  Function: FindLCLWindow
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function FindLCLWindow(const ScreenPos: TPoint): TWinControl;
var
  Handle: HWND;
begin
  Handle := WindowFromPoint(ScreenPos);
  Result := FindOwnerControl(Handle);
end;

function FindDragTarget(const Position: TPoint;
  AllowDisabled: Boolean): TControl;
begin
  Result:=FindControlAtPosition(Position,AllowDisabled);
end;

{------------------------------------------------------------------------------
  Function: FindControlAtPosition
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function FindControlAtPosition(const Position: TPoint;
  AllowDisabled: Boolean): TControl;
var
  WinControl: TWinControl;
  Control: TControl;
begin
  Result := nil;
  WinControl := FindLCLWindow(Position);
  if WinControl <> nil
  then begin
    Result := WinControl;
    Control := WinControl.ControlAtPos(WinControl.ScreenToClient(Position),
                        [capfAllowDisabled,capfAllowWinControls,capfRecursive]);
    //debugln(['FindControlAtPosition ',dbgs(Position),' ',DbgSName(WinControl),' ',dbgs(WinControl.ScreenToClient(Position)),' ',DbgSName(Control)]);
    if Control <> nil then Result := Control;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetCaptureControl
  Params:

  Returns the current capturing TControl.
  Note: For the interface only a Handle = TWinControl can capture. The LCL
  extends this to allow TControl capture the mouse.
 ------------------------------------------------------------------------------}
function GetCaptureControl: TControl;
begin
  Result := FindOwnerControl(GetCapture);
  if (Result <> nil)
  and (CaptureControl <> nil)
  and (CaptureControl.Parent = Result)
  then Result := CaptureControl;
end;

procedure SetCaptureControl(AWinControl: TWinControl; const Position: TPoint);
var
  Control: TControl;
begin
  Control:=AWinControl;
  if (AWinControl<>nil) then begin
    Control:=AWinControl.ControlAtPos(Position,
                                      [capfAllowWinControls,capfRecursive]);
    if Control=nil then
      Control:=AWinControl;
  end;
  SetCaptureControl(Control);
end;

procedure SetCaptureControl(Control: TControl);
var
  OldCaptureWinControl: TWinControl;
  NewCaptureWinControl: TWinControl;
begin
  //DebugLn('SetCaptureControl Old=',DbgSName(CaptureControl),' New=',DbgSName(Control));
  if (CaptureControl=Control) then exit;
  if Control=nil then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('SetCaptureControl Only ReleaseCapture');
    {$ENDIF}
    // just unset the capturing, intf call not needed
    CaptureControl:=nil;
    ReleaseCapture;
    exit;
  end;
  OldCaptureWinControl:=FindOwnerControl(GetCapture);
  if Control is TWinControl then
    NewCaptureWinControl:=TWinControl(Control)
  else
    NewCaptureWinControl:=Control.Parent;
  if NewCaptureWinControl=nil then begin
    {$IFDEF VerboseMouseCapture}
    DebugLN('SetCaptureControl Only ReleaseCapture');
    {$ENDIF}
    // just unset the capturing, intf call not needed
    CaptureControl:=nil;
    ReleaseCapture;
    exit;
  end;
  if NewCaptureWinControl=OldCaptureWinControl then begin
    {$IFDEF VerboseMouseCapture}
    DebugLN('SetCaptureControl Keep WinControl ',DbgSName(NewCaptureWinControl),
      ' switch Control ',DbgSName(Control));
    {$ENDIF}
    // just change the CaptureControl, intf call not needed
    CaptureControl:=Control;
    exit;
  end;
  // switch capture control
  {$IFDEF VerboseMouseCapture}
  DebugLN('SetCaptureControl Switch to WinControl=',DbgSName(NewCaptureWinControl),
    ' and Control=',DbgSName(Control));
  {$ENDIF}
  CaptureControl:=Control;
  ReleaseCapture;
  SetCapture(TWinControl(NewCaptureWinControl).Handle);
end;

{ Cursor translation function }

const
  DeadCursors = 1;

const
  Cursors: array[0..21] of TIdentMapEntry = (
    (Value: crDefault;      Name: 'crDefault'),
    (Value: crArrow;        Name: 'crArrow'),
    (Value: crCross;        Name: 'crCross'),
    (Value: crIBeam;        Name: 'crIBeam'),
    (Value: crSizeNESW;     Name: 'crSizeNESW'),
    (Value: crSizeNS;       Name: 'crSizeNS'),
    (Value: crSizeNWSE;     Name: 'crSizeNWSE'),
    (Value: crSizeWE;       Name: 'crSizeWE'),
    (Value: crUpArrow;      Name: 'crUpArrow'),
    (Value: crHourGlass;    Name: 'crHourGlass'),
    (Value: crDrag;         Name: 'crDrag'),
    (Value: crNoDrop;       Name: 'crNoDrop'),
    (Value: crHSplit;       Name: 'crHSplit'),
    (Value: crVSplit;       Name: 'crVSplit'),
    (Value: crMultiDrag;    Name: 'crMultiDrag'),
    (Value: crSQLWait;      Name: 'crSQLWait'),
    (Value: crNo;           Name: 'crNo'),
    (Value: crAppStart;     Name: 'crAppStart'),
    (Value: crHelp;         Name: 'crHelp'),
    (Value: crHandPoint;    Name: 'crHandPoint'),
    (Value: crSizeAll;      Name: 'crSizeAll'),

    { Dead cursors }
    (Value: crSize;         Name: 'crSize'));

function CursorToString(Cursor: TCursor): string;
begin
  Result := '';
  if not CursorToIdent(Cursor, Result) then FmtStr(Result, '%d', [Cursor]);
end;

function StringToCursor(const S: string): TCursor;
var
  L: Longint;
begin
  if not IdentToCursor(S, L) then L := StrToInt(S);
  Result := TCursor(L);
end;

procedure GetCursorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(Cursors) to High(Cursors) - DeadCursors do Proc(Cursors[I].Name);
end;

function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Cursor, Ident, Cursors);
end;

function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Cursor, Cursors);
end;

// turn off before includes !!
{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

// helper types and functions
{$I dragdock.inc}
{$I controlsproc.inc}

// components
{$I sizeconstraints.inc}
{$I dragmanager.inc}
{$I controlcanvas.inc}
{$I wincontrol.inc}
{$I controlactionlink.inc}
{$I control.inc}
{$I graphiccontrol.inc}
{$I customcontrol.inc}
{$I dockzone.inc}
{$I docktree.inc}
{$I mouse.inc}
{$I dragobject.inc}
{$I dragimagelist.inc}

{ TControlBorderSpacing }

procedure TControlBorderSpacing.SetAround(const AValue: TSpacingSize);
begin
  if FAround=AValue then exit;
  FAround:=AValue;
  Change(false);
end;

function TControlBorderSpacing.IsInnerBorderStored: boolean;
begin
  Result:=Control.IsBorderSpacingInnerBorderStored;
end;

procedure TControlBorderSpacing.SetBottom(const AValue: TSpacingSize);
begin
  if FBottom=AValue then exit;
  FBottom:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetCellAlignHorizontal(
  const AValue: TControlCellAlign);
begin
  if FCellAlignHorizontal=AValue then exit;
  FCellAlignHorizontal:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetCellAlignVertical(
  const AValue: TControlCellAlign);
begin
  if FCellAlignVertical=AValue then exit;
  FCellAlignVertical:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetInnerBorder(const AValue: Integer);
begin
  if FInnerBorder=AValue then exit;
  FInnerBorder:=AValue;
  if Control<>nil then Control.InvalidatePreferredSize;
  Change(true);
end;

procedure TControlBorderSpacing.SetLeft(const AValue: TSpacingSize);
begin
  if FLeft=AValue then exit;
  FLeft:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetRight(const AValue: TSpacingSize);
begin
  if FRight=AValue then exit;
  FRight:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetSpace(Kind: TAnchorKind;
  const AValue: integer);
begin
  case Kind of
  akLeft: Left:=AValue;
  akTop: Top:=AValue;
  akBottom: Bottom:=AValue;
  akRight: Right:=AValue;
  end;
end;

procedure TControlBorderSpacing.SetTop(const AValue: TSpacingSize);
begin
  if FTop=AValue then exit;
  FTop:=AValue;
  Change(false);
end;

constructor TControlBorderSpacing.Create(OwnerControl: TControl);
begin
  FControl:=OwnerControl;
  inherited Create;
end;

procedure TControlBorderSpacing.Assign(Source: TPersistent);
var
  SrcSpacing: TControlBorderSpacing;
begin
  if Source is TControlBorderSpacing then begin
    SrcSpacing:=TControlBorderSpacing(Source);
    if IsEqual(SrcSpacing) then exit;
    
    FAround:=SrcSpacing.Around;
    FBottom:=SrcSpacing.Bottom;
    FLeft:=SrcSpacing.Left;
    FRight:=SrcSpacing.Right;
    FTop:=SrcSpacing.Top;
    FInnerBorder:=SrcSpacing.InnerBorder;
    FCellAlignHorizontal:=SrcSpacing.CellAlignHorizontal;
    FCellAlignVertical:=SrcSpacing.CellAlignVertical;

    Change(false);
  end else
    inherited Assign(Source);
end;

procedure TControlBorderSpacing.AssignTo(Dest: TPersistent);
begin
  Dest.Assign(Self);
end;

function TControlBorderSpacing.IsEqual(Spacing: TControlBorderSpacing
  ): boolean;
begin
  Result:=(FAround=Spacing.Around)
      and (FBottom=Spacing.Bottom)
      and (FLeft=Spacing.Left)
      and (FRight=Spacing.Right)
      and (FTop=Spacing.Top);
end;

procedure TControlBorderSpacing.GetSpaceAround(var SpaceAround: TRect);
begin
  SpaceAround.Left:=Left+Around;
  SpaceAround.Top:=Top+Around;
  SpaceAround.Right:=Right+Around;
  SpaceAround.Bottom:=Bottom+Around;
end;

function TControlBorderSpacing.GetSpace(Kind: TAnchorKind): Integer;
begin
  Result:=Around+GetSideSpace(Kind);
end;

function TControlBorderSpacing.GetSideSpace(Kind: TAnchorKind): Integer;
begin
  case Kind of
  akLeft: Result:=Left;
  akTop: Result:=Top;
  akRight: Result:=Right;
  akBottom: Result:=Bottom;
  end;
end;

procedure TControlBorderSpacing.Change(InnerSpaceChanged: Boolean);
begin
  FControl.DoBorderSpacingChange(Self,InnerSpaceChanged);
  if Assigned(OnChange) then OnChange(Self);
end;

{ TControlChildSizing }

procedure TControlChildSizing.SetEnlargeHorizontal(
  const AValue: TChildControlResizeStyle);
begin
  if FEnlargeHorizontal=AValue then exit;
  FEnlargeHorizontal:=AValue;
  Change;
end;

procedure TControlChildSizing.SetControlsPerLine(const AValue: integer);
begin
  if FControlsPerLine=AValue then exit;
  FControlsPerLine:=AValue;
  Change;
end;

procedure TControlChildSizing.SetEnlargeVertical(
  const AValue: TChildControlResizeStyle);
begin
  if FEnlargeVertical=AValue then exit;
  FEnlargeVertical:=AValue;
  Change;
end;

procedure TControlChildSizing.SetHorizontalSpacing(const AValue: integer);
begin
  if FHorizontalSpacing=AValue then exit;
  FHorizontalSpacing:=AValue;
  Change;
end;

procedure TControlChildSizing.SetLayout(const AValue: TControlChildrenLayout);
begin
  if FLayout=AValue then exit;
  FLayout:=AValue;
  //debugln('TControlChildSizing.SetLayout ',DbgSName(Control));
  Change;
end;

procedure TControlChildSizing.SetLeftRightSpacing(const AValue: integer);
begin
  if FLeftRightSpacing=AValue then exit;
  FLeftRightSpacing:=AValue;
  Change;
end;

procedure TControlChildSizing.SetShrinkHorizontal(
  const AValue: TChildControlResizeStyle);
begin
  if FShrinkHorizontal=AValue then exit;
  FShrinkHorizontal:=AValue;
  Change;
end;

procedure TControlChildSizing.SetShrinkVertical(
  const AValue: TChildControlResizeStyle);
begin
  if FShrinkVertical=AValue then exit;
  FShrinkVertical:=AValue;
  Change;
end;

procedure TControlChildSizing.SetTopBottomSpacing(const AValue: integer);
begin
  if FTopBottomSpacing=AValue then exit;
  FTopBottomSpacing:=AValue;
  Change;
end;

procedure TControlChildSizing.SetVerticalSpacing(const AValue: integer);
begin
  if FVerticalSpacing=AValue then exit;
  FVerticalSpacing:=AValue;
  Change;
end;

constructor TControlChildSizing.Create(OwnerControl: TWinControl);
begin
  FControl:=OwnerControl;
  inherited Create;
  FLayout:=cclNone;
  FEnlargeHorizontal:=crsAnchorAligning;
  FEnlargeVertical:=crsAnchorAligning;
  FShrinkHorizontal:=crsAnchorAligning;
  FShrinkVertical:=crsAnchorAligning;
end;

procedure TControlChildSizing.Assign(Source: TPersistent);
var
  SrcSizing: TControlChildSizing;
begin
  if Source is TControlChildSizing then begin
    SrcSizing:=TControlChildSizing(Source);
    if IsEqual(SrcSizing) then exit;

    FEnlargeHorizontal:=SrcSizing.EnlargeHorizontal;
    FEnlargeVertical:=SrcSizing.EnlargeVertical;
    FShrinkHorizontal:=SrcSizing.ShrinkHorizontal;
    FShrinkVertical:=SrcSizing.ShrinkVertical;
    FEnlargeHorizontal:=SrcSizing.EnlargeHorizontal;
    FEnlargeVertical:=SrcSizing.EnlargeVertical;
    FShrinkHorizontal:=SrcSizing.ShrinkHorizontal;
    FShrinkVertical:=SrcSizing.ShrinkVertical;
    FControlsPerLine:=SrcSizing.ControlsPerLine;
    FLayout:=SrcSizing.Layout;
    FLeftRightSpacing:=SrcSizing.LeftRightSpacing;
    FTopBottomSpacing:=SrcSizing.TopBottomSpacing;
    FHorizontalSpacing:=SrcSizing.HorizontalSpacing;
    FVerticalSpacing:=SrcSizing.VerticalSpacing;

    Change;
  end else
    inherited Assign(Source);
end;

procedure TControlChildSizing.AssignTo(Dest: TPersistent);
begin
  Dest.Assign(Self);
end;

function TControlChildSizing.IsEqual(Sizing: TControlChildSizing): boolean;
begin
  Result:=(FEnlargeHorizontal=Sizing.EnlargeHorizontal)
      and (FEnlargeVertical=Sizing.EnlargeVertical)
      and (FShrinkHorizontal=Sizing.ShrinkHorizontal)
      and (FShrinkVertical=Sizing.ShrinkVertical)
      and (FEnlargeHorizontal=Sizing.EnlargeHorizontal)
      and (FEnlargeVertical=Sizing.EnlargeVertical)
      and (FShrinkHorizontal=Sizing.ShrinkHorizontal)
      and (FShrinkVertical=Sizing.ShrinkVertical)
      and (FControlsPerLine=Sizing.ControlsPerLine)
      and (FLayout=Sizing.Layout)
      and (FLeftRightSpacing=Sizing.LeftRightSpacing)
      and (FTopBottomSpacing=Sizing.TopBottomSpacing)
      and (FHorizontalSpacing=Sizing.HorizontalSpacing)
      and (FVerticalSpacing=Sizing.VerticalSpacing);
end;

procedure TControlChildSizing.SetGridSpacing(Spacing: integer);
begin
  if (LeftRightSpacing=Spacing)
  and (TopBottomSpacing=Spacing)
  and (HorizontalSpacing=Spacing)
  and (VerticalSpacing=Spacing) then exit;
  fLeftRightSpacing:=Spacing;
  fTopBottomSpacing:=Spacing;
  fHorizontalSpacing:=Spacing;
  fVerticalSpacing:=Spacing;
  Change;
end;

procedure TControlChildSizing.Change;
begin
  if Control<>nil then
    Control.DoChildSizingChange(Self);
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TAnchorSide }

procedure TAnchorSide.SetControl(const AValue: TControl);

  procedure RaiseOwnerCircle;
  begin
    DebugLN('RaiseOwnerCircle AValue=',DbgSName(AValue),' FOwner=',DbgSName(FOwner));
    raise Exception.Create('TAnchorSide.SetControl AValue=FOwner');
  end;

var
  OldControl: TControl;
begin
  if (AValue=FOwner) then RaiseOwnerCircle;
  if FControl=AValue then exit;
  OldControl:=FControl;
  FControl:=nil;
  if OldControl<>nil then
    OldControl.ForeignAnchorSideChanged(Self,ascoRemove);
  FControl:=AValue;
  //debugln('TAnchorSide.SetControl A ',DbgSName(FOwner),' FControl=',DbgSName(FControl));
  if FControl<>nil then
    FControl.ForeignAnchorSideChanged(Self,ascoAdd);
  FOwner.AnchorSideChanged(Self);
end;

function TAnchorSide.IsSideStored: boolean;
begin
  Result:=(Control<>nil) and (Side<>DefaultSideForAnchorKind[Kind]);
end;

procedure TAnchorSide.SetSide(const AValue: TAnchorSideReference);
var
  OldSide: TAnchorSideReference;
begin
  if FSide=AValue then exit;
  if AValue=asrCenter then begin
    // in case asrCenter, both sides are controlled by one anchor
    // -> disable opposite anchor and aligning
    OldSide:=FSide;
    if FOwner.Align in [alLeft,alTop,alRight,alBottom,alClient] then begin
      FOwner.Align:=alNone;
    end;
    case Kind of
    akLeft: FOwner.Anchors:=FOwner.Anchors-[akRight];
    akTop: FOwner.Anchors:=FOwner.Anchors-[akBottom];
    akRight: FOwner.Anchors:=FOwner.Anchors-[akLeft];
    akBottom: FOwner.Anchors:=FOwner.Anchors-[akTop];
    end;
    if OldSide<>FSide then exit;
  end;
  FSide:=AValue;
  FOwner.AnchorSideChanged(Self);
  if FControl<>nil then
    FControl.ForeignAnchorSideChanged(Self,ascoChangeSide);
end;

constructor TAnchorSide.Create(TheOwner: TControl; TheKind: TAnchorKind);
begin
  inherited Create;
  FOwner:=TheOwner;
  FKind:=TheKind;
  FSide:=asrTop;
end;

destructor TAnchorSide.Destroy;
var
  OldControl: TControl;
begin
  OldControl:=Control;
  FControl:=nil;
  //DebugLN('TAnchorSide.Destroy A ',DbgSName(Owner));
  if OldControl<>nil then
    OldControl.ForeignAnchorSideChanged(Self,ascoRemove);
  inherited Destroy;
end;

procedure TAnchorSide.GetSidePosition(out ReferenceControl: TControl;
  out ReferenceSide: TAnchorSideReference; out Position: Integer);
  
  procedure RaiseInvalidSide;
  begin
    raise Exception.Create('TAnchorSide.GetSidePosition invalid Side');
  end;
  
  function GetNextCentered(ReferenceControl: TControl; Side: TAnchorKind;
    var NextReferenceSide: TAnchorSide): boolean;
  begin
    if (Side in ReferenceControl.Anchors)
    and (ReferenceControl.AnchorSide[Side].Control<>nil)
    and (ReferenceControl.AnchorSide[Side].Side=asrCenter) then begin
      Result:=true;
      NextReferenceSide:=ReferenceControl.AnchorSide[Side];
    end else
      Result:=false;
  end;
  
var
  NextReferenceSide: TAnchorSide;
  ChainLength: Integer;
  MaxChainLength: LongInt;
  OwnerBorderSpacing: LongInt;
  OwnerParent: TWinControl;
begin
  ReferenceControl:=Control;
  ReferenceSide:=Side;
  Position:=0;
  OwnerParent:=FOwner.Parent;
  if OwnerParent=nil then begin
    // AnchorSide is only between siblings or its direct parent allowed
    //if CheckPosition(Owner) then DebugLn(['TAnchorSide.GetSidePosition OwnerParent=nil']);
    ReferenceControl:=nil;
    exit;
  end;
  ChainLength:=0;
  MaxChainLength:=OwnerParent.ControlCount;
  while ReferenceControl<>nil do begin
  
    // check for circles
    inc(ChainLength);
    if ChainLength>MaxChainLength then begin
      // the chain has more elements than there are siblings -> circle
      //if CheckPosition(Owner) then DebugLn(['TAnchorSide.GetSidePosition Circle']);
      ReferenceControl:=nil;
      exit;
    end;
    
    // check if ReferenceControl is valid
    if (ReferenceControl.Parent<>OwnerParent)
    and (ReferenceControl<>OwnerParent) then begin
      // not a sibling and not the parent -> invalid AnchorSide
      //if CheckPosition(Owner) then DebugLn(['TAnchorSide.GetSidePosition invalid AnchorSide ',dbgsName(ReferenceControl)]);
      ReferenceControl:=nil;
      exit;
    end;
    
    if ReferenceControl.IsControlVisible then begin
      // ReferenceControl is visible
      // -> calculate Position
      OwnerBorderSpacing:=FOwner.BorderSpacing.GetSpace(Kind);
      //if CheckPosition(Owner) then DebugLn(['TAnchorSide.GetSidePosition ',dbgsName(Owner),' ReferenceControl=',dbgsName(ReferenceControl),' ',dbgs(ReferenceControl.BoundsRect),' OwnerBorderSpacing=',OwnerBorderSpacing,' Kind=',dbgs(Kind),' ReferenceSide=',dbgs(Kind,ReferenceSide)]);
      case ReferenceSide of
      
      asrTop: // asrTop = asrLeft
        if Kind in [akLeft,akRight] then begin
          // anchor to left side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=0
          else
            Position:=ReferenceControl.Left;
          if ReferenceControl=OwnerParent then
            OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                    OwnerParent.ChildSizing.LeftRightSpacing)
          else if Kind=akRight then
            OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                 ReferenceControl.BorderSpacing.GetSpace(OppositeAnchor[Kind])),
                 OwnerParent.ChildSizing.HorizontalSpacing);
          if Kind=akLeft then begin
            // anchor left of ReferenceControl and left of Owner
            inc(Position,OwnerBorderSpacing);
          end else begin
            // anchor left of ReferenceControl and right of Owner
            dec(Position,OwnerBorderSpacing);
          end;
        end else begin
          // anchor to top side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=0
          else
            Position:=ReferenceControl.Top;
          if ReferenceControl=OwnerParent then
            OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                    OwnerParent.ChildSizing.TopBottomSpacing)
          else if Kind=akBottom then
            OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                 ReferenceControl.BorderSpacing.GetSpace(OppositeAnchor[Kind])),
                 OwnerParent.ChildSizing.VerticalSpacing);
          if Kind=akTop then begin
            // anchor top of ReferenceControl and top of Owner
            inc(Position,OwnerBorderSpacing);
          end else begin
            // anchor top of ReferenceControl and bottom of Owner
            dec(Position,OwnerBorderSpacing);
          end;
        end;

      asrBottom: // asrBottom = asrRight
        if Kind in [akLeft,akRight] then begin
          // anchor to right side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=OwnerParent.GetLogicalClientRect.Right
          else
            Position:=ReferenceControl.Left+ReferenceControl.Width;
          if ReferenceControl=OwnerParent then
            OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                    OwnerParent.ChildSizing.LeftRightSpacing)
          else if Kind=akLeft then
            OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                 ReferenceControl.BorderSpacing.GetSpace(OppositeAnchor[Kind])),
                 OwnerParent.ChildSizing.HorizontalSpacing);
          if Kind=akLeft then begin
            // anchor right of ReferenceControl and left of Owner
            inc(Position,OwnerBorderSpacing);
          end else begin
            // anchor right of ReferenceControl and right of Owner
            dec(Position,OwnerBorderSpacing);
          end;
        end else begin
          // anchor to bottom side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=OwnerParent.GetLogicalClientRect.Bottom
          else
            Position:=ReferenceControl.Top+ReferenceControl.Height;
          if ReferenceControl=OwnerParent then
            OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                    OwnerParent.ChildSizing.TopBottomSpacing)
          else if Kind=akTop then
            OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                 ReferenceControl.BorderSpacing.GetSpace(OppositeAnchor[Kind])),
                 OwnerParent.ChildSizing.VerticalSpacing);
          if Kind=akTop then begin
            // anchor bottom of ReferenceControl and top of Owner
            inc(Position,OwnerBorderSpacing);
          end else begin
            // anchor bottom of ReferenceControl and bottom of Owner
            dec(Position,OwnerBorderSpacing);
          end;
        end;

      asrCenter:
        if Kind in [akLeft,akRight] then begin
          // center horizontally
          if ReferenceControl=OwnerParent then
            Position:=OwnerParent.ClientWidth div 2
          else
            Position:=ReferenceControl.Left+(ReferenceControl.Width div 2);
          if Kind=akLeft then
            dec(Position,FOwner.Width div 2)
          else
            inc(Position,FOwner.Width div 2);
        end else begin
          // center vertically
          if ReferenceControl=OwnerParent then
            Position:=OwnerParent.ClientHeight div 2
          else
            Position:=ReferenceControl.Top+(ReferenceControl.Height div 2);
          if Kind=akTop then
            dec(Position,FOwner.Height div 2)
          else
            inc(Position,FOwner.Height div 2);
        end;

      else
        RaiseInvalidSide;
      end;
      // side found
      exit;
    end;
    
    // ReferenceControl is not visible -> try next
    NextReferenceSide:=nil;
    if ReferenceControl<>OwnerParent then begin
      if ReferenceSide=asrCenter then begin
        // center can only be anchored to another centered anchor
        if Kind in [akLeft,akRight] then begin
          if not GetNextCentered(ReferenceControl,akLeft,NextReferenceSide)
          then   GetNextCentered(ReferenceControl,akRight,NextReferenceSide);
        end else begin
          if not GetNextCentered(ReferenceControl,akTop,NextReferenceSide)
          then   GetNextCentered(ReferenceControl,akBottom,NextReferenceSide);
        end;
      end else if (ReferenceSide=asrLeft) = (Kind in [akLeft,akTop]) then begin
        // anchor parallel (e.g. a left side to a left side)
        NextReferenceSide:=ReferenceControl.AnchorSide[Kind];
      end else begin
        // anchor opposite (e.g. a left side to a right side)
        NextReferenceSide:=ReferenceControl.AnchorSide[Kind];
      end;
    end;
    if (NextReferenceSide=nil) then begin
      //if CheckPosition(Owner) and (Kind=akRight) then
      //  DebugLn(['TAnchorSide.GetSidePosition not IsControlVisible ReferenceControl=',dbgsName(ReferenceControl)]);
      ReferenceControl:=nil;
      exit;
    end;
    ReferenceControl:=NextReferenceSide.Control;
    ReferenceSide:=NextReferenceSide.Side;
    //DebugLn(['TAnchorSide.GetSidePosition ',DbgSName(FOwner),' ReferenceControl=',DbgSName(ReferenceControl),' Kind=',dbgs(Kind),' ReferenceSide=',dbgs(Kind,ReferenceSide)]);
  end;
end;

procedure TAnchorSide.Assign(Source: TPersistent);
var
  Src: TAnchorSide;
begin
  if Source is TAnchorSide then begin
    Src:=TAnchorSide(Source);
    Side:=Src.Side;
    Control:=Src.Control;
  end else
    inherited Assign(Source);
end;

{ TControlPropertyStorage }

procedure TControlPropertyStorage.GetPropertyList(List: TStrings);
var
  ARoot: TPersistent;
  PropsAsStr: String;
  StartPos: Integer;
  EndPos: LongInt;
  PropertyStr: String;
  AControl: TControl;
  PointPos: LongInt;
begin
  ARoot:=Root;
  if ARoot is TControl then begin
    AControl:=TControl(ARoot);
    PropsAsStr:=AControl.SessionProperties;
    //debugln('PropsAsStr=',PropsAsStr);
    StartPos:=1;
    while (StartPos<=length(PropsAsStr)) do begin
      EndPos:=StartPos;
      while (EndPos<=length(PropsAsStr)) and (PropsAsStr[EndPos]<>';') do
        inc(EndPos);
      if (EndPos>StartPos) then begin
        PropertyStr:=copy(PropsAsStr,StartPos,EndPos-StartPos);
        //debugln('A PropertyStr=',PropertyStr);
        // if no point char, then prepend the owner name as default
        PointPos:=StartPos;
        while (PointPos<EndPos) and (PropsAsStr[PointPos]<>'.') do
          inc(PointPos);
        if PointPos=EndPos then
          PropertyStr:=AControl.Name+'.'+PropertyStr;
        // add to list
        //debugln('B PropertyStr=',PropertyStr);
        List.Add(PropertyStr);
      end;
      StartPos:=EndPos+1;
    end;
  end;
end;

{ TDragManager }

constructor TDragManager.Create;
begin
  FDragImmediate := true;
  FDragThreshold := 5;
end;

initialization
  //DebugLn('controls.pp - initialization');
  Mouse := TMouse.Create;
  DefaultDockTreeClass := TDockTree;
  DragManager := TDragManagerDefault.Create;
  RegisterIntegerConsts(TypeInfo(TCursor), @IdentToCursor, @CursorToIdent);

finalization
  FreeThenNil(DragManager);
  FreeThenNil(Mouse);

end.





